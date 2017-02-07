unit jsDbgServer;

interface

uses windows, classes, sysUtils, js15decl, SyncObjs, Vcl.forms, Vcl.dialogs, Generics.Collections, NamedPipesImpl;

Type
    TJSDebuggerMode = (dmRun, dmStepInto, dmStepover, dmStepout, dmBreak);
    TJSbreakPoints = TList<Integer>;

    TJSDebugScript = class
      fileName: string;
      extent: integer;
      start: integer;
      end_: integer;
      script: PJSObject;
      breakPoints: TJSbreakPoints;
    public
      constructor Create;
      destructor destroy; override;
    end;

    TJSBreakPoint = class
      fileName: string;
      line: integer;
    end;

    TJSDebugScripts = TObjectDictionary<PJSObject, TJSDebugScript>;
    TJSDebugBreakpoints = TObjectList<TJSBreakPoint>;

  TJSDebugServer = class
  type
  private
    FPipeHandle: THandle;
    FMode: TJSDebuggerMode;
    FFrameStop: PJSStackFrame;
    FlastStop: PJSObject;
    FlastLine: Integer;
    Fdepth: integer;
    FCurDepth: integer;

    FRuntime: PJSRuntime;
    FContext: PJSContext;
    FNamedPipe: TNamedPipeServer;
    FScripts: TJSDebugScripts;
    FBreakPoints: TJSDebugBreakpoints;
    FTrapBreakpoint: Integer;
    FCurrentScriptName: string;

  protected
    // JS callbacks
    //called at every execution point
    class function _JSTrapHandler(cx: PJSContext; Script: PJSObject; pc: pjsbytecode; rval: pjsval;
      closure: pointer): JSTrapStatus; cdecl; static;

    class function _JSBreakHandler(cx: PJSContext; Script: PJSObject; pc: pjsbytecode; rval: pjsval;
      closure: pointer): JSTrapStatus; cdecl; static;

    class procedure _JSNewScriptHook(cx: PJSContext; filename: PAnsiChar; // * URL of script
      lineno: uintN; // * first line */
      Script: PJSObject; fun: PJSFunction; callerdata: pointer); cdecl; static;
    // called just before script destruction
    class procedure _JSDestroyScriptHook(cx: PJSContext; Script: PJSObject; callerdata: pointer); cdecl; static;
    class function _JSDebugErrorHook(cx: PJSContext; const _message: PAnsiChar; report: PJSErrorReport;
      closure: pointer): JSBool; cdecl; static;

    //gets called when a script starts to be executed
    class function _JSInterpreterHook(cx: PJSContext; fp: PJSStackFrame; before: JSBool; ok: PJSBool;
      closure: pointer): pointer; cdecl; static;

    function Debug(cx: PJSContext; rval: pjsval; fpa: PJSStackFrame): JSTrapStatus;
    function SetBreakPoint(script: PJSObject; lineno: uintN): integer; overload;
    function ClearBreakPoint(script: PJSObject; lineno: uintN): boolean;
    procedure ClearBreakPoints(script: PJSObject);
  public
    constructor Create(AContext: PJSContext);
    destructor Destroy; override;

    property Scripts: TJSDebugScripts read FScripts;
    property BreakPoints: TJSDebugBreakpoints read FBreakPoints;
    property TrapBreakpoint: Integer read FTrapBreakpoint write FTrapBreakpoint;
  end;

implementation
uses jsintf;

function StrRestOf(const S: string; N: Integer): string;
begin
  Result := Copy(S, N, (Length(S) - N + 1));
end;

function StrLeft(const S: string; Count: Integer): string;
begin
  Result := Copy(S, 1, Count);
end;

function StrAfter(const SubStr, S: string): string;
var
  P: Integer;
begin
  P := Pos(SubStr, S); // StrFind is case-insensitive pos
  if P <= 0 then
    Result := ''           // substr not found -> nothing after it
  else
    Result := StrRestOf(S, P + Length(SubStr));
end;

function StrBefore(const SubStr, S: string): string;
var
  P: Integer;
begin
  P := Pos(SubStr, S);
  if P <= 0 then
    Result := S
  else
    Result := StrLeft(S, P - 1);
end;


{ TJSDebugServer }

function TJSDebugServer.ClearBreakPoint(script: PJSObject; lineno: uintN): boolean;
var
  pc: pjsbytecode;
  t: JSTrapHandler;
  p: pointer;
  value: TJSDebugScript;
  b: TJSBreakPoint;
begin

  if lineNo = 0 then exit(false);
  pc := JS_LineNumberToPC(FContext, Script, lineNo);
  if pc = nil then
    exit(false);

   if FScripts.TryGetValue(script, value) then
   begin
      for b in FBreakPoints do
      begin
         if b.fileName = value.fileName then
         begin
            FBreakPoints.Remove(b);
            break;
         end;
      end;
      value.breakPoints.Remove(lineNo);
   end;
  JS_ClearTrap(FContext, Script, pc, t, p);

end;

procedure TJSDebugServer.ClearBreakPoints(script: PJSObject);
begin
  JS_ClearScriptTraps(FContext, script);

end;

constructor TJSDebugServer.Create(AContext: PJSContext);
begin
  //  Result := TJSEngine(JS_GetRuntimePrivate(JS_GetRuntime(cx)));

  FContext := AContext;
  FRuntime := JS_GetRuntime(AContext);
  JS_SetNewScriptHook(FRuntime, _JSNewScriptHook, self);
  JS_SetDestroyScriptHook(FRuntime, _JSDestroyScriptHook, self);
  JS_SetDebugErrorHook(FRuntime, _JSDebugErrorHook, self);
  JS_SetExecuteHook(FRuntime, _JSInterpreterHook, self);
  FMode := dmStepInto;
  FFrameStop := nil;
  Fdepth := 0;

  FNamedPipe := TNamedPipeServer.Create('js_debug_pipe');
  FNamedPipe.Open;

  FScripts:= TJSDebugScripts.Create;
  FBreakPoints:= TJSDebugBreakpoints.Create;
  FTrapBreakpoint := -1;
end;

function TJSDebugServer.Debug(cx: PJSContext; rval: pjsval; fpa: PJSStackFrame): JSTrapStatus;
var
//  Action: TJSDebuggerAction;
  line, cmd: string;
  script: PJSObject;
  newLineNo, lineNo: integer;
  pc: pjsbytecode;
  scriptName, extra: string;
  value: TJSDebugScript;
  breakPoint: TJSBreakpoint;
  m: TJSDebuggerMode;
  eval: jsval;
  fp, fi: PJSStackFrame;

  function readLine: string;
  begin

    if FNamedPipe.Connected then
       Result := FNamedPipe.Read(10);
    while Result = '' do
    begin
       Application.ProcessMessages;
       if FNamedPipe.Connected then
          Result := FNamedPipe.Read(10)
       else
          sleep(10);
    end;

  end;

  function getScript(fileName: string; lineNo: integer = 0): PJSObject;
  var
    value: TJSDebugScript;
    top_script: PJSObject;
  begin
     top_script := nil;
     fileName := stringReplace(fileName, '"', '', [rfReplaceAll]);
     for value in FScripts.Values do
     begin
       // excluide top-level scripts

       if sameText(value.fileName, fileName) then
       begin
          if lineNo >=0 then
          begin
             if (lineNo >= value.start) and (lineNo <= value.start + value.extent) then
             begin
                if value.start = 0 then top_script := value.script
                else exit(value.script);
             end;
          end
          else exit(value.script);
       end;
     end;
     result := top_script;
  end;

begin
  // JS_GetFrameScript(cx, fp)
  try

    while True do
    begin
      { TODO : Not sure about this code }
      if Self.FTrapBreakpoint <> -1 then
      begin
         script := JS_GetFrameScript(cx, fpa);
         scriptName := JS_GetScriptFilename(cx, script);
         script := getScript(scriptName, Self.FTrapBreakpoint);
         newLineNo := setBreakPoint(script, Self.FTrapBreakpoint);
         line := 'run';
         Self.FTrapBreakpoint := -1;
      end
      else  line := readLine;
      extra := strAfter(' ', line);
      cmd := strBefore(' ', line);

      fp := fpa;
      if (fp = nil) then
         fp := JS_FrameIterator(cx, fp);

      if sameText(cmd, 'run') then
      begin
          JS_SetInterrupt(FRuntime, nil, nil);
          FMode := dmRun;
          exit(JSTRAP_CONTINUE);
      end
      else if sameText(cmd, 'stepover') then
      begin
          FFrameStop := fp;
          FMode := dmStepover;
          FDepth := FCurDepth;
          exit(JSTRAP_CONTINUE);
      end
      else if sameText(cmd, 'stepinto') then
      begin
          JS_SetInterrupt(FRuntime, _JSTrapHandler, self);
          FMode := dmStepInto;
          exit(JSTRAP_CONTINUE);
      end
      else if sameText(cmd, 'setbreak') then
      begin
          lineNo:= strToIntDef(strAfter('@', extra), 0);
          script := getScript(strBefore('@', extra), lineNo);
          //script := JS_GetFrameScript(cx, fp);
          if script <> nil then
          begin
             newLineNo := setBreakPoint(script, lineNo);
             FNamedPipe.Write(format('OK,SETBREAK %s@%d@%d', [strBefore('@', extra), lineNo, newLineNo]));
          end;
      end
      else if sameText(cmd, 'clearbreak') then
      begin
          lineNo:= strToIntDef(strAfter('@', extra), 0);
          script := getScript(strBefore('@', extra), lineNo);
          script := JS_GetFrameScript(cx, fp);
          clearBreakPoint(script, strToIntDef(strAfter('@', extra), 0));
          //FNamedPipe.Write('OK');

      end
      else if sameText(cmd, 'listbreak') then
      begin
          line := '';
          extra := '';
          for breakPoint in FBreakpoints do
          begin
             line := line + inttostr(breakpoint.line) + ',';
          end;

          FNamedPipe.Write('OK,LISTBREAK "'+extra+'"' + ' ' + line);

      end
      else if sameText(cmd, 'evaluate') then
      begin
          fi := nil;
          fp := JS_FrameIterator(cx, fi);
          (*if fp <> nil then
          begin
             JS_EvaluateInStackFrame(cx, fp,PAnsiChar(AnsiString(extra)), length(extra),0,0, @eval);
             extra := JSStringToString(JS_ValueToString(cx, eval));
          end;
          *)
          m := fmode;
          FMode := dmRun;
          if extra <> '' then
          begin
               if JS_EvaluateUCScript(cx, JS_GetGlobalObject(cx), PWideChar(extra), length(extra), '<debug eval>', 1, @eval) <> js_true then
                  FNamedPipe.Write(format('ERROR,EVALUATE "%s"', [extra] ))
               else
                  FNamedPipe.Write(format('OK,EVALUATE "%s"="%s"', [extra,JSStringToString(cx, JS_ValueToString(cx, eval))] ));
{            if(fp <> nil) then
            begin
               if JS_EvaluateUCInStackFrame(cx, fp, PWideChar(extra), length(extra), '<debug eval>', 1, @eval) <> js_true then
                 FNamedPipe.Write(format('ERROR,EVALUATE "%s"', [extra] ))
               else
                 FNamedPipe.Write(format('OK,EVALUATE "%s"="%s"', [extra,JSStringToString(JS_ValueToString(cx, eval))] ));
            end
            else begin
               JS_EvaluateUCScript(cx, JS_GetGlobalObject(cx), PWideChar(extra), length(extra), '<debug eval>', 1, @eval);
               FNamedPipe.Write(format('OK,EVALUATE "%s"="%s"', [extra,JSStringToString(JS_ValueToString(cx, eval))] ));
            end;
 }         end;

          fmode := m;

      end
      else if sameText(cmd, 'stop') then
      begin
        exit(JSTRAP_ERROR);
      end;
    end;

  except
    JS_SetInterrupt(FRuntime, nil, nil);
    FMode := dmRun;
    exit(JSTRAP_CONTINUE);
  end;

end;

destructor TJSDebugServer.Destroy;
begin
  FNamedPipe.Free;
  FScripts.Free;
  FBreakPoints.Free;
  inherited;
end;

function TJSDebugServer.SetBreakPoint(script: PJSObject; lineno: uintN): integer;
var
  pc: pjsbytecode;
  value: TJSDebugScript;
  b: TJSBreakPoint;
  op: JSOp;
begin
   Result := -1;

   pc := JS_LineNumberToPC(FContext, Script, lineNo);
   if pc = nil then
     exit;


   if JS_SetTrap(FContext, Script, pc, _JSBreakHandler, self) = JS_TRUE then
   begin
      Result := JS_PCToLineNumber(FContext, script, pc);

      if FScripts.TryGetValue(script, value) then
      begin
          value.breakPoints.Add(Result);
          b:= TJSBreakPoint.Create();
          b.fileName := value.fileName;
          b.line := Result;
          FBreakPoints.Add(b);
      end;
   end;

end;

class function TJSDebugServer._JSBreakHandler(cx: PJSContext; Script: PJSObject; pc: pjsbytecode; rval: pjsval;
  closure: pointer): JSTrapStatus;
var
  scr: TJSDebugServer;
  line: uintN;
begin
  scr := closure;
  line := JS_PCToLineNumber(cx, Script, pc);

  scr.FMode := dmBreak;
  // traphandler will be called at the beginning of the js_Execute loop
  JS_SetInterrupt(scr.FRuntime, _JSTrapHandler, closure);
  JS_SetExecuteHook(scr.FRuntime, _JSInterpreterHook, closure);

  Result := JSTRAP_CONTINUE;

end;

class function TJSDebugServer._JSDebugErrorHook(cx: PJSContext; const _message: PAnsiChar; report: PJSErrorReport;
  closure: pointer): JSBool;
var
  scr: TJSDebugServer;
begin
  scr := TJSDebugServer(closure);
  if scr = NIL then
    exit(JS_TRUE);

end;

class procedure TJSDebugServer._JSDestroyScriptHook(cx: PJSContext; Script: PJSObject; callerdata: pointer);
var
  scr: TJSDebugServer;
begin
  scr := TJSDebugServer(callerdata);
  if scr = nil then
    exit;

  JS_ClearScriptTraps(cx, Script);

end;

class function TJSDebugServer._JSInterpreterHook(cx: PJSContext; fp: PJSStackFrame; before: JSBool; ok: PJSBool;
  closure: pointer): pointer;
var
  scr: TJSDebugServer;
  s: PJSObject;
  pc: pjsbytecode;
  line: Integer;
  scriptName: PAnsiChar;
begin

  scr := TJSDebugServer(closure);
  if scr.FMode = dmRun then
    exit(nil);

  if (scr.FFrameStop <> fp) and ((scr.FMode = dmStepout) or (scr.FMode = dmStepover)) then
    exit(nil);

  s := JS_GetFrameScript(cx, fp);
  pc := JS_GetFramePC(cx, fp);
  line := 0;
  if (pc <> nil) then
     line := JS_PCToLineNumber(cx, s, pc);

//  opcode := JS_GetTrapOpcode(cx, s, pc);
  scriptName := JS_GetScriptFilename(cx, s);

  scr.FCurrentScriptName := scriptName;
  scr.Fdepth := 0;
  scr.FNamedPipe.Write(Format('STOP SCRIPT "%s"@%d@%d', [scriptName, line, nativeInt(cx)]));

  scr.FlastStop := s;
  scr.FlastLine := line;
  if (scr.Debug(cx, nil, fp) = JSTRAP_ERROR) then
  begin
    // exit if possible
    if (ok <> nil) then
      ok^ := js_false;
    exit(nil);
  end;

  if (before = JS_TRUE) then
  begin
    if (scr.FMode = dmStepover) or (scr.FMode = dmStepout) then
    begin
      JS_SetExecuteHook(scr.FRuntime, nil, nil);
      JS_SetInterrupt(scr.FRuntime, nil, nil);
      exit(closure); // call when done to restore debugger state.
    end;
    // running or stepping? no need to call again for this script.
    exit(nil);
  end
  else
  begin
    scr.FMode := dmStepInto;
    JS_SetExecuteHook(scr.FRuntime, _JSInterpreterHook, closure);
    JS_SetInterrupt(scr.FRuntime, _JSTrapHandler, closure);
    exit(nil);
  end;

end;

class procedure TJSDebugServer._JSNewScriptHook(cx: PJSContext; filename: PAnsiChar; lineno: uintN; Script: PJSObject;
  fun: PJSFunction; callerdata: pointer);
var
  scr: TJSDebugServer;
  extent: Cardinal;
  s: TJSDebugScript;

begin

  // showmessage('aa');
  scr := TJSDebugServer(callerdata);
  extent := JS_GetScriptLineExtent(cx, script);
  if fileName <> '' then
  begin
     s:= TJSDebugScript.Create;
     s.fileName := fileName;
     s.extent := extent;
     s.start := lineNo;
     s.end_ := lineNo + extent;
     s.script := script;
     //s.start := JS_GetScriptBaseLineNumber(cx, script);
     scr.FScripts.AddOrSetValue(script, s);
  end;

end;

class function TJSDebugServer._JSTrapHandler(cx: PJSContext; Script: PJSObject; pc: pjsbytecode; rval: pjsval;
  closure: pointer): JSTrapStatus;
var
  str: PJSString;
  //caller: PJSStackFrame;
  scr: TJSDebugServer;
  line: uintN;
  scriptName: PAnsiChar;
  fi, fp: PJSStackFrame;

begin
  scr := closure;

  if (script = nil) then exit(JSTRAP_CONTINUE);

  if (scr.FMode = dmRun) then
    exit(JSTRAP_CONTINUE);


  scr.FCurDepth := 0;
  fi := nil;
  fp := JS_FrameIterator(cx, fi);

  if (fp = nil) then
     fp := JS_GetScriptedCaller(cx, nil);

  while (fp <> nil) do
  begin
      if (JS_IsNativeFrame(cx, fp) = js_true) then
              break;
      inc(scr.FCurDepth);
      fp := JS_GetScriptedCaller(cx, fp);
  end;


  line := JS_PCToLineNumber(cx, Script, pc);

  if (((scr.FMode = dmStepInto) or (scr.FMode = dmStepover) or (scr.FMode = dmStepout)) and (Script = scr.FlastStop)
      and (line = scr.FlastLine)) then
    exit(JSTRAP_CONTINUE);

  if (scr.FMode = dmStepOver) and (scr.FCurDepth > scr.Fdepth) then
  begin
      exit(JSTRAP_CONTINUE);
  end;


  scr.FlastStop := Script;
  scr.FlastLine := line;

  if (scr.FMode = dmBreak) or (scr.FMode = dmStepInto) then
  begin
    // scr.FlastLine := line;
  end;

  if scr.FNamedPipe.Connected  then
  begin
    scriptName := JS_GetScriptFilename(cx, script);
    if scr.FMode = dmBreak then
       scr.FNamedPipe.Write(Format('STOP BREAK "%s"@%d@%d', [scriptName, line, nativeInt(cx)]))
    else
       scr.FNamedPipe.Write(Format('STOP STEP "%s"@%d@%d', [scriptName, line, nativeInt(cx)]));
  end;

  Result := scr.Debug(cx, rval, nil);

end;

{ TJSDebugScript }

constructor TJSDebugScript.Create;
begin
  breakPoints:= TList<Integer>.Create;
end;

destructor TJSDebugScript.destroy;
begin

  breakPoints.Free;
  inherited;
end;

end.

