unit jsintf;
interface

uses Classes, {ptrarray, namedarray,} TypInfo, js15decl, RTTI, types,
  Generics.Collections, SysUtils, Windows, Vcl.Controls, syncObjs, JSDbgServer, Vcl.forms, Vcl.dialogs;

var
//293632
  global_class: JSClass = (name: 'global'; flags: 293632 or JSCLASS_HAS_PRIVATE;

    addProperty: JS_PropertyStub;
    delProperty: JS_PropertyStub; getProperty: JS_PropertyStub; setProperty: JS_StrictPropertyStub;
    enumerate: JS_EnumerateStub; resolve: JS_ResolveStub; convert: JS_ConvertStub; finalize: JS_FinalizeStub);

type
  TJSFunctionsDynArray =  array of JSFunctionSpec;
  TJSPropertiesDynArray =  array of JSPropertySpec;
  PWord = ^Word;
  PInteger = ^Integer;

  JSClassType = (ctDate, ctArray, ctString, ctNumber, ctBoolean, ctUnknownClass, ctUnknownType);

  PNativeMethod = ^TNativeMethod;

  TNativeMethod = record
    Name: String; // Method name
    Obj: TObject; // Object containing method
    JS: PJSObject; // Corresponding JS object
  end;

  PBridgeChar = PWideChar;
  PBridgeData = ^TBridgeData;

  TBridgeData = record
    container: Pointer;
    data: Pointer;
  end;

  TJSBase = class;
  TJSObject = class;
  TJSEngine = class;

  // Use from parameters for getting native jsparameters
  TJSNativeCallParams = record
    cx: PJSContext;
    jsobj: PJSObject;
    argc: uintN;
    argv: pjsval;
    // rval: pjsval;
  end;

  // JS RTTI Attributes
  JSClassNameAttribute = class(TCustomAttribute)
  private
    FClassName: string;
  public
    constructor Create(ClassName: string);
  end;

  // JS RTTI Attributes
  TJSClassFlagAttributes = set of (cfaInheritedMethods,     // Publish inherited methods
                                   cfaProtectedMethods,     // publish protected methods
                                   cfaProtectedFields,
                                   cfaInheritedProperties,  // Publish inherited properties
                                   cfaOwnObject,            // Free object on javascript destructor
                                   cfaGlobalFields,         // Register Private fields as properties to global object
                                   cfaGlobalProperties);    // global properties

  JSClassFlagsAttribute = class(TCustomAttribute)
  private
    FClassFlags: TJSClassFlagAttributes;
  public
    constructor Create(flags: TJSClassFlagAttributes);
  end;

  // JS RTTI Attributes for method,properties names
  JSNameAttribute = class(TCustomAttribute)
  private
    FName: string;
  public
    constructor Create(Name: string);
  end;

  JSCtorAttribute = class(TCustomAttribute)
  end;

  JSExcludeAttribute = class(TCustomAttribute)
  end;

  JSGlobalScopeAttribute = class(TCustomAttribute)
  end;

  TJSEventData = class
  protected
    fjsfuncobj: PJSObject;
    feventName: string;
    fmethodName: string;
    fobj: TObject;
    fcx: PJSContext;
  protected
  public
    constructor Create(ajsfuncobj: PJSObject; aeventname,amethodname: string; aobj: TObject; acx: PJSContext);
  end;

  TJSPropRead = reference to function (cx: PJSContext; jsobj: PJSObject; id: jsval; vp: pjsval): JSBool;

  TJSClassProto = class(TInterfacedObject)
  //private class constructor Create;
  private
    function getJSClassName: string;
  protected
    FJSCtor: TRttiMethod;
    FJSClass: JSClass;
    FClass: TClass;
    Fclass_methods: TJSFunctionsDynArray;

    Fclass_props: TJSPropertiesDynArray;
    Fclass_indexedProps: TJSPropertiesDynArray;
    Fclass_fields: TJSPropertiesDynArray;

    FConsts: array of JSConstDoubleSpec;
    //Fctx: TRttiContext;
    FRttiType: TRttiType;
    FJSClassProto: PJSObject;
    FMethodNamesMap: TStringList;
    FClassFlags: TJSClassFlagAttributes;

    procedure DefineJSClass(AClass: TClass; AClassFlags: TJSClassFlagAttributes = []);
    // Used from Engine.registerClass;
    procedure JSInitClass(AEngine: TJSEngine);
    function CreateNativeObject(cx: PJSContext; AClass: TClass): TObject;
  public
    constructor Create(AClass: TClass; AClassFlags: TJSClassFlagAttributes = []);
    destructor Destroy; override;
    class procedure DefineProperties(cx: PJSContext; obj: PJSObject; props: TJSPropertiesDynArray);
    class procedure DefineFunctions(cx: PJSContext; obj: PJSObject; funcs: TJSFunctionsDynArray);
    class procedure DeleteProperties(cx: PJSContext; obj: PJSObject; props: TJSPropertiesDynArray);
    class procedure DefineEnum(cx: PJSContext; Obj: TJSObject; pt: TRttiType);

    property JSClassName: string read getJSClassName;
    property JSClassProto: PJSObject read FJSClassProto;
  end;

  TJSClass = class
  private

    FEventsCode: TObjectDictionary<string, TJSEventData>;
    Fjsobj: PJSObject;
    FNativeObj: TObject;
    FJSObject: TJSObject;

  protected
    FPointerProps: TDictionary<string, TJSClass>;
    FClassFlags: TJSClassFlagAttributes;
    FClassProto: TJSClassProto;
    FClassProtoIntf: IInterface;
    FJSEngine: TJSEngine;
    FFreeNotifies: TList<TJSClass>;

    class function JSMethodCall(cx: PJSContext; argc: uintN; vp: pjsval): JSBool; cdecl; static;
    class function JSPropWrite(cx: PJSContext; jsobj: PJSObject; id: jsid; _strict:jsbool; vp: pjsval): JSBool; cdecl; static;
    class function JSPropRead(cx: PJSContext; jsobj: PJSObject; id: jsid; vp: pjsval): JSBool; cdecl; static;
    class function JSFieldRead(cx: PJSContext; jsobj: PJSObject; id: jsid; vp: pjsval): JSBool; cdecl; static;
    class function JSPropReadClass(cx: PJSContext; jsobj: PJSObject; id: jsid; vp: pjsval): JSBool; cdecl; static;

{$IF CompilerVersion >= 23}
    class function JSIndexedPropRead(cx: PJSContext; jsobj: PJSObject; id: jsid; vp: pjsval): JSBool; cdecl; static;
{$IFEND}
    class function GetParamName(cx: PJSContext; id: jsval): string;

    class function JSObjectCtor(cx: PJSContext; argc: uintN; vp: pjsval): JSBool; cdecl; static;
    class procedure JSObjectDestroy(cx: PJSContext; Obj: PJSObject); cdecl; static;

    // Events
    procedure JSGetStrProc(const s: string);
    procedure JSNotifyEvent(Sender: TObject);
    procedure JSKeyEvent(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure JSKeyPressEvent(Sender: TObject; var Key: Char);
    procedure JSMouseDownUpEvent(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure JSMouseMoveEvent(Sender: TObject; Shift: TShiftState; X, Y: Integer);

    // procedure TForm12.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure RemoveNotification(AClass: TJSClass);
    procedure Notification(AClass: TJSClass; Operation: TOperation); virtual;
    procedure RemoveFreeNotifications;
    procedure FreeNotification(AClass: TJSClass);
    procedure RemoveFreeNotification(AClass: TJSClass);

//    procedure RemoveFreeNotification(AClass: TJSClass);
  public
    constructor Create; virtual;
    constructor CreateJSObject(Instance: TObject; AEngine: TJSEngine; JSObjectName: string = '';
      AClassFlags: TJSClassFlagAttributes = []); overload; virtual;
    constructor CreateJSObject(AEngine: TJSEngine; JSObjectName: string = ''); overload; virtual;
    destructor Destroy; override;

    class function TValueToJSVal(cx: PJSContext; Value: TValue; isDateTime: boolean = false; classObj: TJSClass = NIL; propName: string = ''): jsval;
    class function JSValToTValue(cx: PJSContext; t: PTypeInfo; vp: jsval; RttiType: TRttiType): TValue; overload;
    class function JSArgsToTValues(params: TArray<TRttiParameter>; cx: PJSContext; jsobj: PJSObject; argc: uintN;
      argv: pjsval): TArray<TValue>; overload;

    class function JSDateToDateTime(JSEngine: TJSEngine; oDate: PJSObject; var dDate: TDateTime
    ): boolean;
    class function JSEngine(cx: PJSContext): TJSEngine; overload;
    class function JSPrintf(JSEngine: TJSEngine; const fmt: String; argc: Integer; args: pjsval): String;

    procedure NewJSObject(Engine: TJSEngine; JSObjectName: string = ''; AInstance: TObject = nil;
      AClassFlags: TJSClassFlagAttributes = []); overload; virtual;
    function JSEngine: TJSEngine; overload;
    property JSObj: PJSObject read Fjsobj;
    property NativeObj: TObject read FNativeObj;
    property JSObject: TJSObject read FJSObject;
    property ClassProto: TJSClassProto read FClassProto;

  end;

  TJSDebuggerScripts = TDictionary<string, string>;

  TJSEngine = class
  public class var DebugClientClass: TClass;
  private type
    TJSMethod = record
      method_class: TClass;
      RttiType: TRttiType;
      delphiName: string;
      method: TRttiMethod;
      params: TArray<TRttiParameter>;
      CodeAddress: Pointer;
    end;

  var
    fslowArrayClass: PJSClass;
    fArrayClass: PJSClass;
    fXMLClass: PJSClass;
    fbooleanclass: PJSClass;
    fcx: PJSContext;
    fdateclass: PJSClass;
    ffunctionclass: PJSClass;
    fglobal: PJSObject;
    fnativeglobal: TJSObject;
    fnumberclass: PJSClass;
    frt: PJSRuntime;
    fstackSize: Cardinal;
    fstringclass: PJSClass;

    FMethodNamesMap: TDictionary<string, TJSMethod>;

    FDebugging: boolean;
    FDebugger: TJSDebugServer;
    FDebuggerScripts: TJSDebuggerScripts;

    function getStrictMode: boolean;
    procedure SetStrictMode(const Value: boolean);
    function Compile(const Code: String; const FileName: String = ''): PJSObject;
    function Execute(Script: PJSObject; Scope: TJSObject = nil): boolean; overload;

    procedure GetStandardClasses;
    function InternalCall(const Func: PJSFunction; Obj: PJSObject; var args: Array of TJSBase; rval: pjsval): boolean;
    function InternalCallName(const Func: String; Obj: PJSObject; var args: Array of TJSBase; rval: pjsval): boolean;
    function InternalGet(const Name: String; Obj: PJSObject; var rval: jsval): boolean;
    function GetVersion: String;
    procedure SetDebugging(const Value: boolean);
  protected
    FDestroying: boolean;
    FNeverFree: TStringList;
    FDelphiClasses: TDictionary<string, TJSClassProto>;
    class function JSGlobalMethodCall(cx: PJSContext; argc: uintN; vp: pjsval): JSBool; cdecl; static;
  public

    constructor Create(MaxMemory: Cardinal = 64 * 1024 * 1024); virtual;
    destructor Destroy; override;

    procedure registerClass(AClass: TClass; AClassFlags: TJSClassFlagAttributes = []);
    procedure registerClasses(AClass: array of TClass; AClassFlags: TJSClassFlagAttributes = [cfaInheritedMethods, cfaInheritedProperties]);
    procedure registerGlobalFunctions(AClass: TClass);

    // For TRttiSetType, TRttiOrdinalType
    function Define(val: TRttiType): boolean; overload;
    function Define(ATypeInfo: Pointer): boolean;overload;

    function Declare(val: Integer; const Name: String): boolean; overload;
    function Declare(val: Double; const Name: String): boolean; overload;
    function Declare(const val: String; const Name: String): boolean; overload;
    function Declare(const val: PWideChar; const Name: String): boolean; overload;
    function Declare(val: boolean; const Name: String): boolean; overload;

    function EvaluateFile(const FileName: String; Scope: TJSObject = NIL): boolean; overload;

    function Evaluate(const Code: String; Scope: TJSObject; scriptFileName: String = ''): boolean; overload;
    function Evaluate(const Code: String; Scope: TJSObject; var rval: jsval; scriptFileName: String = '')
      : boolean; overload;
    function Evaluate(const Code: String; scriptFileName: String = ''): boolean; overload;
    function Evaluate(const Code: String; var rval: jsval; scriptFileName: String = ''): boolean; overload;

    function callFunction(functionName: AnsiString; var rval: jsval): boolean; overload;
    function callFunction(functionName: AnsiString): boolean; overload;

    procedure GarbageCollect;
    procedure lockObject(Obj: PJSObject; Name: AnsiString = ''); overload;
    procedure lockObject(Obj: TJSObject; Name: AnsiString = ''); overload;
    procedure unlockObject(Obj: TJSObject); overload;
    procedure unlockObject(Obj: PJSObject); overload;
    function IsExceptionRaised: boolean;
    function IsValidCode(const Code: String): boolean;

    function NewJSObject: TJSObject; overload;
    function NewJSObject(const Name: String): TJSObject; overload;
    function NewJSObject(const Name: String; parent: TJSObject): TJSObject; overload;

    procedure SetErrorReporter(proc: JSErrorReporter);

    property slowArrayClass: PJSClass read fslowArrayClass;
    property ArrayClass: PJSClass read FArrayClass;
    property XMLClass: PJSClass read FXMLClass;
    property BooleanClass: PJSClass read fbooleanclass;
    property Context: PJSContext read fcx;
    property Runtime: PJSRuntime read frt;
    property DateClass: PJSClass read fdateclass;
    property Global: TJSObject read fnativeglobal;
    property NumberClass: PJSClass read fnumberclass;
    property StringClass: PJSClass read fstringclass;
    property Version: String read GetVersion;

    property strictMode: boolean read getStrictMode write SetStrictMode;
    property Debugging: boolean read FDebugging write SetDebugging;
    property DebuggerScripts: TJSDebuggerScripts read FDebuggerScripts;
    property Debugger: TJSDebugServer read FDebugger;

  end;

  (*
    * These were initially set up to be ref-counted, but that may be more effort than its worth.
    * On the other hand, it may open up thread safety for a single TJSBase to be used within
    * multiple threads.  Need to do more reading to see if this is correct or not :)
  *)
  TJSBase = class
  protected
    FConnected: boolean;
    FDestroying: boolean;
    FEngine: TJSEngine;
    FJSVal: jsval;
    FName: String;
    FRefCnt: Integer;
    FScope: TJSObject;

    procedure AddRef;
    function CanGoLive: boolean; virtual;
    procedure InternalConnect; virtual;
    function IsLive: boolean; virtual;
    procedure SetConnected;
    procedure SetEngine(const Value: TJSEngine);
    procedure SetName(const Value: String);
    procedure SetScope(const Value: TJSObject);
  public
    constructor Create(AEngine: TJSEngine; AName: String); overload;
    constructor Create; overload;
    destructor Destroy; override;

    procedure Connect(AEngine: TJSEngine; AName: String; AParent: TJSObject); overload;
    procedure Connect(AEngine: TJSEngine; AName: String); overload;
    function ToString: String;

    property Connected: boolean read FConnected;
    property Destroying: boolean read FDestroying write FDestroying;
    property Engine: TJSEngine read FEngine write SetEngine;
    property JScriptVal: jsval read FJSVal;
    property JSName: String read FName write SetName;
    property parent: TJSObject read FScope write SetScope;

  end;

  TJSObject = class(TJSBase)
  protected
    Fjsobj: PJSObject;

    procedure CheckConnection;
    procedure InternalConnect; override;
  public
    constructor Create(AValue: PJSObject; AEngine: TJSEngine; const AName: String); overload;
    constructor Create(AValue: PJSObject; AEngine: TJSEngine; const AName: String; AParent: TJSObject); overload;
    destructor Destroy; override;

    function AddMethods(var methods: TJSFunctionsDynArray): boolean;

    function ClassType(const Name: String): JSClassType;
    function Declare(val: Double; const Name: String): boolean; overload;
    function Declare(val: Integer; const Name: String): boolean; overload;
    function Declare(const val: String; const Name: String): boolean; overload;
    function Declare(val: boolean; const Name: String): boolean; overload;
    class function enumerate(cx: PJSContext; obj: PJSObject): TArray<string>;
    function Evaluate(const Code: String; scriptFileName: String = ''): boolean; overload;
    function Evaluate(Code: String; var rval: jsval; scriptFileName: String = ''): boolean; overload;
    function Compile(const Code: String; scriptFileName: AnsiString = ''): PJSObject;
    function Execute(Script: PJSObject; rval: pjsval = NIL): boolean;

    function getProperty(const Name: String; var dbl: Double): boolean; overload;
    function getProperty(const Name: String; var int: Integer): boolean; overload;
    function getProperty(const Name: String; var ret: TJSObject): boolean; overload;
    function getProperty(const Name: String; var str: String): boolean; overload;
    function getProperty(const Name: String; var bool: boolean): boolean; overload;
    function HasProperty(const Name: String): boolean;
    function IsFunction(const Name: String): boolean;
    function IsInteger(const Name: String): boolean;
    procedure RemoveObject(Obj: TJSBase);
    function setProperty(const Name: String; val: TJSBase; flags : integer = 0): boolean; overload;
    function setProperty(const Name: String; val: TValue; flags : integer = 0): boolean; overload;
    function TypeOf(const Name: String): JSType;

    property JSObject: PJSObject read Fjsobj write Fjsobj;

  end;

  TJSScript = class
  private
    FCompiled: boolean;
    FScript: PJSObject;
    FEngine: TJSEngine;
    FCode: String;
    FFileName: string;
  protected
    procedure Compile(const AFileName: string = ''); virtual;
  public
    constructor Create; overload; virtual;
    constructor Create(AEngine: TJSEngine; const ACode: String; const AFileName: string = ''); overload; virtual;
    destructor Destroy; override;

    class function LoadScript(FileName: string): string; overload;
    class function LoadScript(Stream: TStream): string; overload;

    procedure Execute(AScope: TJSObject = nil); overload;
    // Streaming
    procedure LoadCompiled(const AFile: String);
    procedure LoadCompiledFromStream(AStream: TStream);
    procedure LoadRaw(const AFile: String);
    procedure SaveCompiled(const AFile: String);
    procedure SaveCompiledToStream(AStream: TStream);
    procedure SaveRaw(const AFile: String);

    property Code: String read FCode write FCode;
    property Compiled: boolean read FCompiled;
    property Script: PJSObject read FScript;
  end;

implementation

uses PSApi, Math, ActiveX, DateUtils, RegularExpressions;

const
  NilMethod: TMethod = (Code: nil; data: nil);

Type
  pjsval_ptr = ^jsval_array;
  jsval_array = array [0 .. 256] of jsval;

  TJSInternalGlobalFunctions = class
  public
    class procedure DebugBreak(Params: TJSNativeCallParams);
  end;

  TJSIndexedProperty = class(TJSClass)
  public
    parentObj: TObject;
    propName: string;
  end;

var
//  NumObjsFree: integer = 0;
//  TJSClassProtoCount: integer = 0;
  RttiContext: TRttiContext;

function MBytes(Bytes: int64): double;
begin
  Result := Bytes / 1024 / 1024;
end;

function CurrentMemoryUsage: Cardinal;
var
  pmc: TProcessMemoryCounters;
begin
  pmc.cb := SizeOf(pmc);
  if GetProcessMemoryInfo(GetCurrentProcess, @pmc, SizeOf(pmc)) then
    Result := pmc.WorkingSetSize
  else
  begin
    Result := 0; // RaiseLastOSError;
  end;
end;

procedure CheckDebugBreak(Engine: TJSEngine; var code: string);
var
  Lines: TStringList;
  lineBreak, i: integer;
  //ctx: TRttiContext;
  RttiType: TRttiType;
  m: TRttiMethod;
begin
  if TJSEngine.DebugClientClass = NIL then exit;

  lineBreak := -1;
  if pos('DebugBreak()', Code) > 1  then
  begin
     Lines:= TStringList.Create;
     Lines.Text := Code;
     for i:=0 to lines.count-1 do
       if (pos('DebugBreak()', lines[i]) > 1) and (trim(lines[i]) = 'DebugBreak();') then
       begin
          lineBreak := i+1;
          if (not Engine.Debugging) and (TJSEngine.DebugClientClass <> NIL)  then
          begin
            Engine.Debugging := true;
            RttiType := RttiContext.GetType(TJSEngine.DebugClientClass);
            m := RttiType.GetMethod('Create');
            m.Invoke(RttiType.AsInstance.MetaclassType,[]);
          end;

          Engine.Debugger.TrapBreakpoint := lineBreak;
          break;
       end;

     Lines.Free;
  end;
end;

function strdup(s: AnsiString): PAnsiChar;
begin
  GetMem(Result, Length(s) + 1);
  strCopy(Result, PAnsiChar(s));
  // move(PAnsiChar(s)^, Result^, Length(s)+1);
end;

function GetGUID: AnsiString;
Var
  GUID: TGUID;
  psz: array [0 .. 256] of widechar;
begin
  CoCreateGuid(GUID);
  StringFromGUID2(GUID, @psz, 256);
  Result := POleStr(@psz);
end;

function generateScriptName: string;
begin
  Result := '(inline)' + GetGUID + '.js'
end;

procedure Debug(s: string); overload;
begin
  OutputDebugString(PChar(s));
end;

procedure Debug(f: string;const a:array of const ); overload;
begin
  OutputDebugString(PChar(format(f, a)));
end;

function JSBranchCallback(cx: PJSContext; script: PJSObject): JSBool; cdecl;
begin
  JS_MaybeGC(cx);
end;

function JSMarkOp(cx: PJSContext; obj: PJSObject; arg: Pointer): uint32; cdecl;
begin

end;

procedure ErrorReporter(cx: PJSContext; message: PAnsiChar; report: PJSErrorReport); cdecl;
var
  FileName, msg: String;
  o: Integer;
begin
  // if (report^.flags and JSREPORT_EXCEPTION <> 0) then  // ignore js-catchable exceptions
  // exit;

  if report.FileName <> nil then
    FileName := extractFileName(report.FileName)
  else
    FileName := 'typein:';

  o := pos(report.uctokenptr, report.uclinebuf);

  msg := format('%s'#13#10'%s'#13#10'%s^'#13#10'Filename: %s'#13#10'Line: %d',
    [message, trim(report.uclinebuf), stringofchar(' ', o), FileName, report.lineno + 1]);
  MessageBox(0, PChar(msg), PChar('Javascript error'), MB_ICONERROR or MB_OK);
  { msg := 'Notice type: ';
    if (report^.flags and JSREPORT_WARNING <> 0) then
    msg := msg + 'Warning'
    else
    msg := msg + 'Error';

    msg := msg + #10 + 'Message: ' + message + #10'Line: ' + IntToStr(report^.lineNo);
  }
end;

function GetParamName(cx: PJSContext; id: jsval): String;
begin
  Result := JSValToString(cx, id);
end;

{ TJSEngine }

function TJSEngine.callFunction(functionName: AnsiString): boolean;
var
  rval: jsval;
begin
  Result := callFunction(functionName, rval);

end;

function TJSEngine.callFunction(functionName: AnsiString; var rval: jsval): boolean;
var
  r: Integer;
  vp, argv: jsval;
begin

  Result := false;
  if JS_LookupProperty(Context, Global.JSObject, PAnsiChar(functionName), @vp) = js_true then
  begin
    if (not JSValIsVoid(vp)) and (JSValIsObject(vp)) then
    begin
      r := JS_CallFunctionValue(Context, Global.JSObject, vp, 0,nil, @rval);
      Result := true;

    end;
  end;

end;

function TJSEngine.Compile(const Code: String; const FileName: String): PJSObject;
var
  Name: UTF8String;
begin
  if FileName = '' then
    name := generateScriptName
  else
    name := FileName;

  // Register script source
  if FDebugging then
     FDebuggerScripts.AddOrSetValue(name, Code);

  Result := JS_CompileUCScript(fcx, fglobal, PWideChar(Code), Length(Code), PAnsiChar(Name), 0);
end;

constructor TJSEngine.Create(MaxMemory: Cardinal);
var
  d: word;
  em: TArithmeticExceptionMask;
  bool: JSBool;
  v: jsval;
  ver: JSVersion;
begin
{$ifdef CPUX64}
  ClearExceptions(false);
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
{$ENDIF}
//  NumObjsFree := 0;
//  TJSClassProtoCount := 0;
  fstackSize := 8192;
  Frt := JS_NewRuntime(MaxMemory);

  fcx := JS_NewContext(frt, fstackSize);
  JS_SetRuntimePrivate(frt, self);

  JS_SetOptions(fcx, JS_GetOptions(fcx) or {JSOPTION_VAROBJFIX or} JSOPTION_JIT or JSOPTION_METHODJIT);

  //FRttiContext := TRttiContext.Create;
  //JS_SetContextPrivate(fcx, @FRttiContext);

  //SetReservedSlots(global_class, 255);
  fglobal := JS_NewCompartmentAndGlobalObject(fcx, @global_class, nil);
  JS_SetErrorReporter(fcx, ErrorReporter);

  fnativeglobal := TJSObject.Create(fglobal, self, '');
  bool := JS_InitStandardClasses(fcx, fglobal);

  GetStandardClasses;

  FDelphiClasses := TDictionary<string, TJSClassProto>.Create;
  FDebuggerScripts := TJSDebuggerScripts.Create;
  FMethodNamesMap := TDictionary<string, TJSMethod>.Create;
  FNeverFree := TStringList.Create;
  registerGlobalFunctions(TJSInternalGlobalFunctions);
  //strictMode := true;
end;

function TJSEngine.Declare(val: Integer; const Name: String): boolean;
begin
  Result := Global.setProperty(name, val);
end;

function TJSEngine.Declare(val: Double; const Name: String): boolean;
begin
  Result := Global.setProperty(name, val);
end;

function TJSEngine.Declare(const val: String; const Name: String): boolean;
begin
  Result := Global.setProperty(name, val);
end;

function TJSEngine.Declare(val: boolean; const Name: String): boolean;
begin
  Result := Global.setProperty(name, val);
end;

function TJSEngine.Declare(const val: PWideChar; const Name: String): boolean;
var
  v: TValue;
begin
  v := TVAlue.From<PWideChar>(val);
  Result := Global.setProperty(name, v);

end;

function TJSEngine.Define(ATypeInfo: Pointer): boolean;
begin
  Result := Define(RttiContext.GetType(ATypeInfo));
end;

destructor TJSEngine.Destroy;
var
  p: TJSClassProto;
begin

  FDestroying := true;

  fnativeglobal.Free;
  try
    for p in FDelphiClasses.Values do
    begin
      p._Release;
    end;
  except

  end;


  JS_DestroyContext(fcx);
  JS_DestroyRuntime(frt);
{
}
  FDelphiClasses.Free;
  FDebuggerScripts.Free;
  FMethodNamesMap.Free;
  FNeverFree.Free;
//  Debug('NumObjsFree=%d', [NumObjsFree]);
//  Debug('TJSClassProtoCount=%d', [TJSClassProtoCount]);

  inherited;
end;

function TJSEngine.Evaluate(const Code: String; Scope: TJSObject; scriptFileName: String): boolean;
begin
  Result := Scope.Evaluate(Code, scriptFileName);
end;

function TJSEngine.Evaluate(const Code: String; scriptFileName: String): boolean;
begin
  Result := Evaluate(Code, fnativeglobal, scriptFileName);
end;

function TJSEngine.Evaluate(const Code: String; var rval: jsval; scriptFileName: String): boolean;
begin
  Result := Evaluate(Code, fnativeglobal, rval, scriptFileName);
end;

function TJSEngine.EvaluateFile(const FileName: String; Scope: TJSObject): boolean;
var
  filenameutf8: UTF8String;
  s, Code: string;
  rval: jsval;
begin
  if Scope = nil then
    Scope := fnativeglobal;

  filenameutf8 := FileName;
  Code := TJSScript.LoadScript(FileName);
  CheckDebugBreak(self, code);

  if FDebugging then
    FDebuggerScripts.AddOrSetValue(FileName, Code);

  Result := JS_EvaluateUCScript(Context, fnativeglobal.Fjsobj, PWideChar(Code), Length(Code), PAnsiChar(filenameutf8),
    0, @rval) = 1;

  // JS_AddNamedObjectRoot(cx, &scriptObj, "compileAndRepeat script object")
end;

function TJSEngine.Evaluate(const Code: String; Scope: TJSObject; var rval: jsval; scriptFileName: String): boolean;
begin
  Result := Scope.Evaluate(Code, rval, scriptFileName);

end;

function TJSEngine.Execute(Script: PJSObject; Scope: TJSObject): boolean;
var
  rval: jsval;
begin
  if (Scope = nil) then
    Scope := fnativeglobal;
  Result := (JS_ExecuteScript(fcx, Scope.JSObject, Script, @rval) = js_true);
end;

procedure TJSEngine.GarbageCollect;
begin
  //JS_GC(fcx);
  JS_MaybeGC(fcx);
end;

procedure TJSEngine.GetStandardClasses;
var
  Obj: PJSObject;
  v: jsval;
  jsarr: PJSObject;

  function Eval(const str: String): jsval;
  var
    v: jsval;
  begin
    JS_EvaluateUCScript(fcx, fglobal, PWideChar(str), Length(str), nil, 0, @v);
    Result := v;
  end;

begin

  //jsarr := JS_NewArrayObject(fcx, 0, nil);

  //JS_EvaluateUCScript(fcx, fglobal, PWideChar('Date.prototype'), Length('Date.prototype'), nil, 0, @v);
  v := Eval('Date.prototype');
  Obj := JSValToObject(v);
  fdateclass := JS_GetClass(Obj);

  Obj := JSValToObject(Eval('new Array()'));
  fArrayClass := JS_GetClass(Obj);

  Obj := JSValToObject(Eval('new XML()'));
  fXMLClass := JS_GetClass(Obj);

  Obj := JSValToObject(Eval('Array.prototype'));
  fslowArrayClass := JS_GetClass(Obj);

  Obj := JSValToObject(Eval('Boolean.prototype'));
  fbooleanclass := JS_GetClass(Obj);

  Obj := JSValToObject(Eval('String.prototype'));
  fstringclass := JS_GetClass(Obj);

  Obj := JSValToObject(Eval('Number.prototype'));
  fnumberclass := JS_GetClass(Obj);
end;

function TJSEngine.getStrictMode: boolean;
begin
  Result := JS_GetOptions(fcx) and JSOPTION_STRICT <> 0;
end;

function TJSEngine.GetVersion: String;
begin
  Result := JS_GetImplementationVersion;
end;

function TJSEngine.InternalCall(const Func: PJSFunction; Obj: PJSObject; var args: Array of TJSBase;
  rval: pjsval): boolean;
var
  myargs: TArray<jsval>;
  i: Integer;
begin
  if (Obj = nil) then
    Obj := fglobal;

  if (Length(args) = 0) then
    Result := (JS_CallFunction(fcx, Obj, Func, 0, nil, rval) = js_true)
  else
  begin
    SetLength(myargs, Length(args));
    for i := 0 to Length(args) - 1 do
      myargs[i] := args[i].JScriptVal;

    Result := (JS_CallFunction(fcx, Obj, Func, Length(myargs), @myargs[0], rval) = js_true);
    SetLength(myargs, 0);
  end;
end;

function TJSEngine.InternalCallName(const Func: String; Obj: PJSObject; var args: array of TJSBase;
  rval: pjsval): boolean;
var
  fval: jsval;
begin
  JS_GetUCProperty(fcx, Obj, PWideChar(Func), Length(Func), @fval);

  Result := InternalCall(JS_ValueToFunction(fcx, fval), Obj, args, rval);
end;

function TJSEngine.InternalGet(const Name: String; Obj: PJSObject; var rval: jsval): boolean;
begin
  if (Obj = nil) then
    Obj := fglobal;
  Result := (JS_GetUCProperty(fcx, Obj, PWideChar(name), Length(name), @rval) = js_true);
end;

function TJSEngine.IsExceptionRaised: boolean;
begin
  Result := (JS_IsExceptionPending(fcx) = js_true);
end;

function TJSEngine.IsValidCode(const Code: String): boolean;
begin
  Result := (JS_BufferIsCompilableUnit(fcx, fglobal, PAnsiChar(AnsiString(Code)), Length(Code)) = js_true);
end;

class function TJSEngine.JSGlobalMethodCall(cx: PJSContext; argc: uintN; vp: pjsval): JSBool;
var
  methodName: string;
  eng: TJSEngine;
  method: TJSMethod;
  RttiType: TRttiType;
  m: TRttiMethod;
  params: TArray<TRttiParameter>;
  args: TArray<TValue>;
  v, methodResult: TValue;
  ppp: pointer;
  argv: pjsval;
  jsobj: pjsobject;
begin
{$POINTERMATH ON}
  jsObj := JSValToObject(JS_CALLEE(cx, vp));
  argv := JS_ARGV_PTR(cx, vp);
  Result := JS_FALSE;
  methodName := GetParamName(cx, argv[-2]);
  Delete(methodName, 1, Length('function '));
  Delete(methodName, pos('(', methodName), Length(methodName));

  eng := TJSClass.JSEngine(cx);
  if eng.FMethodNamesMap.TryGetValue(methodName, method) then
  begin
    RttiType := method.RttiType;

    m := method.method;
    params := m.GetParameters;

    try
      args := TJSClass.JSArgsToTValues(params, cx, jsobj, argc, argv);
      methodResult := m.Invoke(method.method_class, args);
      if methodResult.Kind <> tkUnknown then
         vp^ := TJSClass.TValueToJSVal(cx, methodResult, methodResult.typeinfo.name = 'TDateTime');

      Result := js_true;
    except
      on e: Exception do
      begin
        Result := JS_FALSE;
        //JS_SetPendingException(cx, StringToJSVal(cx, e.Message));
        JS_ReportError(cx, PAnsiChar(AnsiString(e.message)), nil);
      end
    end;
    // Break since inherited/virtual methods will be called
  end;
{$POINTERMATH OFF}
end;

procedure TJSEngine.lockObject(Obj: PJSObject; Name: AnsiString);
var
  r: JSBool;
begin
  {
    Do not pass a pointer to a JS double, string, or objectrp must be either a pointer to a pointer variable
    or a pointer to a jsval variable.
  }

  if name <> '' then
    r := JS_AddNamedObjectRoot(Context, @Obj, PAnsiChar(name))
  else
    r := JS_AddObjectRoot(Context, @Obj)
end;

procedure TJSEngine.lockObject(Obj: TJSObject; Name: AnsiString);
begin
  lockObject(Obj.JSObject, name);
end;

procedure TJSEngine.unlockObject(Obj: PJSObject);
var
  r: JSBool;
begin
  r := JS_RemoveObjectRoot(Context, @Obj);

end;

procedure TJSEngine.unlockObject(Obj: TJSObject);
begin
  unlockObject(Obj.JSObject);
end;

function TJSEngine.NewJSObject: TJSObject;
begin
  Result := TJSObject.Create(nil, self, '');
end;

function TJSEngine.NewJSObject(const Name: String): TJSObject;
begin
  Result := TJSObject.Create(nil, self, name);
end;

function TJSEngine.NewJSObject(const Name: String; parent: TJSObject): TJSObject;
begin
  Result := TJSObject.Create(nil, self, name, parent);
end;

procedure TJSEngine.registerClass(AClass: TClass; AClassFlags: TJSClassFlagAttributes);
var
  p: TJSClassProto;
  i: IInterface;
begin

  for p in FDelphiClasses.Values do
  begin
    if p.FClass.ClassName = aclass.Classname then
       exit;

  end;

  p := TJSClassProto.Create(AClass, AClassFlags);
  p._AddRef;
  p.JSInitClass(self);
  FDelphiClasses.Add(p.JSClassName, p);

end;

procedure TJSEngine.registerClasses(AClass: array of TClass; AClassFlags: TJSClassFlagAttributes);
var
  i: Integer;
begin
  for i := 0 to high(AClass) do
    registerClass(AClass[i], AClassFlags);

end;

procedure TJSEngine.registerGlobalFunctions(AClass: TClass);
var
  //ctx: TRttiContext;
  RttiType: TRttiType;
  m: TRttiMethod;
  exclude: boolean;
  methodName: string;
  a: TCustomAttribute;
  methods: TJSFunctionsDynArray;
  method: TJSMethod;
  f: TRttiField;
  i: integer;
  p: TRttiParameter;
begin
  // setter.Invoke(TClass(Instance), argsV)
  methods := nil;
  RttiType := RttiContext.GetType(AClass);
  for m in RttiType.GetMethods do
  begin
    methodName := m.Name;
    method.method_class := AClass;
    method.delphiName := methodName;
    method.method := m;
    method.params := m.GetParameters;

    // cfaInheritedMethods, cfaInheritedProperties
    if (m.Visibility < mvPublic) or (m.parent <> RttiType) or (not m.IsClassMethod) then
      continue;

    exclude := false;
    for a in m.GetAttributes do
    begin
      if (a is JSExcludeAttribute) then
        exclude := true;

      if (a is JSNameAttribute) then
        methodName := JSNameAttribute(a).FName;

    end;

    if exclude then
      continue;
 //defineEnums(f.FieldType);
    // Define enums
    for p in method.params do
      TJSClassProto.DefineEnum(Fcx, fnativeglobal, p.ParamType); // TRttiType
    FMethodNamesMap.Add(methodName, method);
    SetLength(methods, Length(methods) + 1);
    methods[high(methods)].Name := strdup(methodName);
    methods[high(methods)].Call := @TJSEngine.JSGlobalMethodCall;
    methods[high(methods)].nargs := Length(m.GetParameters);
    methods[high(methods)].flags := 0;
    methods[high(methods)].extra := 0;

  end;

  if methods <> nil then
  begin
    SetLength(methods, Length(methods) + 1);
    TJSClassProto.DefineFunctions(Context, fglobal, methods);
    for i:=0 to high(methods) do
       freeMem(methods[i].name);
  end;
end;

procedure TJSEngine.SetDebugging(const Value: boolean);
begin
  if FDebugging = Value then
    exit;

  if (not FDebugging) and Value then
  begin
    FDebugger := TJSDebugServer.Create(fcx);
  end
  else if not Value then
  begin
    if Assigned(FDebugger) then
    begin
      FDebugger.Free;
      FDebugger := NIL;
      FDebuggerScripts.Clear;
    end;

  end;

  FDebugging := Value;
end;

procedure TJSEngine.SetErrorReporter(proc: JSErrorReporter);
begin
  JS_SetErrorReporter(fcx, proc);
end;

procedure TJSEngine.SetStrictMode(const Value: boolean);
begin
  if Value then
    JS_SetOptions(fcx, JS_GetOptions(fcx) or JSOPTION_STRICT)
  else
    JS_SetOptions(fcx, JS_GetOptions(fcx) and not JSOPTION_STRICT);

end;

function TJSEngine.Define(val: TRttiType): boolean;
var
  i: Integer;
  st: TRttiSetType;
  ot: TRttiOrdinalType;
  ename: string;
  Consts: array of JSConstDoubleSpec;
  //proc: TIntToIdent;

  procedure defineEnum(ename: string; v: integer);
  begin
    Global.setProperty(ename, v, JSPROP_READONLY or JSPROP_PERMANENT);

    {SetLength(Consts, Length(Consts) + 1);
    Consts[high(Consts)].dval := v;
    Consts[high(Consts)].Name := strdup(ename);
    Consts[high(Consts)].flags := JSPROP_ENUMERATE or JSPROP_READONLY or JSPROP_PERMANENT;
    }
  end;

begin

  if val = nil then exit(false);

  Result := False;
  Consts := nil;

  if val.Handle^.Kind = tkEnumeration then
  begin
    Result := True;
    ot := val.AsOrdinal;
    for i := ot.MinValue to ot.MaxValue do
    begin
      defineEnum( GetEnumName(val.Handle, i), i);
    end;
  end
  else if val.IsSet then
  begin
    Result := True;
    st := val.AsSet;
    val := st.ElementType;
    if val.IsOrdinal then
    begin
      ot := val.AsOrdinal;
      for i := ot.MinValue to ot.MaxValue do
      begin
        defineEnum(GetEnumName(val.Handle, i), 1 shl i);
      end;
    end;

  end
  else if val.Handle^.Kind = tkInteger then
  begin
    // proc := FindIntToIdent(val.handle) ;
  end;

end;

{ TJSBase }

procedure TJSBase.AddRef;
begin
  Inc(FRefCnt);
end;

function TJSBase.CanGoLive: boolean;
begin
  Result := (FName <> '') and (FScope <> nil) and (FScope.Connected);
end;

procedure TJSBase.Connect(AEngine: TJSEngine; AName: String; AParent: TJSObject);
begin
  Engine := AEngine;
  parent := AParent;
  JSName := AName;
end;

procedure TJSBase.Connect(AEngine: TJSEngine; AName: String);
begin
  Engine := AEngine;
  parent := AEngine.Global;
  JSName := AName;
end;

constructor TJSBase.Create(AEngine: TJSEngine; AName: String);
begin
  Engine := AEngine;
  JSName := AName;
  parent := FEngine.Global;
end;

constructor TJSBase.Create;
begin
  FEngine := nil;
  FScope := nil;
end;

destructor TJSBase.Destroy;
var
  rval: jsval;
begin
  if (FEngine <> nil) and (not FDestroying) then
    try
      if (FName <> '') and (FScope <> nil) then
          if not FEngine.FDestroying then
             JS_DeleteUCProperty2(FEngine.Context, FScope.JSObject, PWideChar(FName), Length(FName), @rval);
    except
    end;
end;

procedure TJSBase.InternalConnect;
begin
end;

function TJSBase.IsLive: boolean;
begin
  (*
    * This may not be the fastest way to determine whether the property already exists in FScope.
  *)
  // Result := (FScope <> nil) and FScope.HasProperty(FName);
  Result := false;
end;

procedure TJSBase.SetConnected;
begin
  FConnected := (FEngine <> nil);
  if (FConnected) then
    InternalConnect;
end;

procedure TJSBase.SetEngine(const Value: TJSEngine);
begin
  FEngine := Value;
  SetConnected;
end;

procedure TJSBase.SetName(const Value: String);
begin
  FName := Value;
  SetConnected;
end;

procedure TJSBase.SetScope(const Value: TJSObject);
begin
  if (FEngine <> nil) and (Value = nil) then
    FScope := FEngine.Global
  else
    FScope := Value;
  SetConnected;
end;

function TJSBase.ToString: String;
begin
  Result := GetParamName(FEngine.Context, FJSVal);
end;

{ TJSObject }

function TJSObject.AddMethods(var methods: TJSFunctionsDynArray): boolean;
var
  len: Integer;
begin
  CheckConnection;
  (* The last element of |methods| must be blank *)
  len := Length(methods);
  SetLength(methods, len + 1);
  FillChar(methods[len], SizeOf(JSFunctionSpec), #0);

  Result := true;
  TJSClassProto.DefineFunctions(FEngine.Context, Fjsobj,methods);
end;

procedure TJSObject.CheckConnection;
begin
  if (not FConnected) then
    raise Exception.Create('Connection to TJSEngine instance expected.  Assign Engine property of TJSObject instance.');
end;

function TJSObject.ClassType(const Name: String): JSClassType;
var
  rval: jsval;
  cls: PJSClass;
begin
  JS_LookupUCProperty(FEngine.Context, Fjsobj, PWideChar(Name), Length(Name), @rval);
  if (JS_TypeOfValue(FEngine.Context, rval) = JSTYPE_OBJECT) then
  begin
    cls := JS_GetClass(JSValToObject(rval));
    Result := ctUnknownClass;
  end
  else
  begin
    cls := nil;
    Result := ctUnknownType;
  end;



  if (JS_IsArrayObject(FEngine.Context, fjsobj) = js_true) then
    Result := ctArray
  else if (cls = FEngine.fdateclass) then
    Result := ctDate
  else if (cls = FEngine.fbooleanclass) then
    Result := ctBoolean
  else if (cls = FEngine.fnumberclass) then
    Result := ctNumber
  else if (cls = FEngine.fstringclass) then
    Result := ctString
  else
    case JS_TypeOfValue(FEngine.Context, rval) of
      JSTYPE_STRING:
        Result := ctString;
      JSTYPE_BOOLEAN:
        Result := ctBoolean;
      JSTYPE_NUMBER:
        Result := ctNumber;
    end;
end;

function TJSObject.Compile(const Code: String; scriptFileName: AnsiString): PJSObject;
begin
  Result := JS_CompileUCScript(FEngine.Context, Fjsobj, PWideChar(Code), Length(Code), PAnsiChar(scriptFileName), 0);
end;

constructor TJSObject.Create(AValue: PJSObject; AEngine: TJSEngine; const AName: String);
begin
  Fjsobj := AValue;
  FJSVal := JSObjectToJSVal(Fjsobj);

  Engine := AEngine;
  if (AEngine <> nil) then
    parent := FEngine.Global; // Set this before we
  JSName := AName;
end;

constructor TJSObject.Create(AValue: PJSObject; AEngine: TJSEngine; const AName: String; AParent: TJSObject);
begin
  Fjsobj := AValue;
  FJSVal := JSObjectToJSVal(Fjsobj);

  Engine := AEngine;
  JSName := AName;
  parent := AParent;
end;

function TJSObject.Declare(const val: String; const Name: String): boolean;
begin
  Result := setProperty(name, val);
end;

function TJSObject.Declare(val: Integer; const Name: String): boolean;
begin
  Result := setProperty(name, val);
end;

function TJSObject.Declare(val: Double; const Name: String): boolean;
begin
  Result := setProperty(name, val);
end;

function TJSObject.Declare(val: boolean; const Name: String): boolean;
begin
  Result := setProperty(name, val);
end;

destructor TJSObject.Destroy;
var
  rval: jsval;
  p: Pointer;
begin
  // fnatives.Free;
{  Destroying := True;

  if (FEngine <> nil) then
    try
      if (FName <> '') and (Fjsobj <> nil) then
        JS_DeleteUCProperty2(FEngine.Context, Fjsobj, PWideChar(FName), Length(FName), @rval);
    except
    end;
}
(*
  if Fjsobj <> nil then
  begin
     Debug('tjsobject.destroy=%s', [JS_GetClass(fjsobj).name]);
     p := JS_GetPrivate(FEngine.Context, fjsobj);
     if (p <> nil) then
        if (TObject(p) is TJSClass) and (TJSClass(p).FNativeObjOwner) then
           begin
              TJSClass(p).free;
           end;

  end;
*)
  inherited;
end;

class function TJSObject.enumerate(cx: PJSContext; obj: PJSObject): TArray<string>;
var
  list: PJSIdArray;
  curid: pjsid;
  val: jsval;
  i: Integer;
begin
//  CheckConnection;
  list := JS_Enumerate(cx, obj);
  curid := @list^.vector;

  SetLength(Result, list^.Length);
  for i := 0 to list^.Length - 1 do
  begin
    JS_IdToValue(cx, curid^, @val);
    Result[i] := JSValToString(cx, val);
    Inc(curid);
  end;
end;

function TJSObject.Evaluate( Code: String; var rval: jsval; scriptFileName: String): boolean;
begin
  CheckConnection;

  if scriptFileName = '' then
    scriptFileName := generateScriptName;

  CheckDebugBreak(FEngine, code);

  if FEngine.FDebugging then
     FEngine.FDebuggerScripts.AddOrSetValue(scriptFileName, Code);

  Result := JS_EvaluateUCScript(FEngine.Context, Fjsobj, PWideChar(Code), Length(Code), PAnsiChar(AnsiString(scriptFileName)), 0,
    @rval) = 1;

end;

function TJSObject.Execute(Script: PJSObject; rval: pjsval): boolean;
var
  val: jsval;
begin
  if rval = nil then
    rval := @val;

  Result := JS_ExecuteScript(FEngine.Context, Fjsobj, Script, rval) = js_true;
end;

function TJSObject.Evaluate(const Code: String; scriptFileName: String): boolean;
var
  rval: jsval;
begin
  Result := Evaluate(Code, rval, scriptFileName);
end;

function TJSObject.getProperty(const Name: String; var int: Integer): boolean;
var
  rval: jsval;
begin
  CheckConnection;
  Result := FEngine.InternalGet(name, Fjsobj, rval);

  if (Result) and (not JSValIsNull(rval)) then
    JS_ValueToInt32(FEngine.Context, rval, @int)
  else
    int := 0;
end;

function TJSObject.getProperty(const Name: String; var dbl: Double): boolean;
var
  rval: jsval;
begin
  CheckConnection;
  Result := FEngine.InternalGet(name, Fjsobj, rval);

  if (Result) and (not JSValIsNull(rval)) then
    JS_ValueToNumber(FEngine.Context, rval, @dbl)
  else
    dbl := 0;
end;

function TJSObject.getProperty(const Name: String; var ret: TJSObject): boolean;
var
  rval: jsval;
  p: PJSObject;
begin
  CheckConnection;
  Result := FEngine.InternalGet(name, Fjsobj, rval);

  if (Result) and (not JSValIsNull(rval)) then
  begin
    JS_ValueToObject(FEngine.Context, rval, p);
    (* !!!
      * This is wasteful.  We need to figure out how to find existing wrappers
      * for instance |p|.
    *)
    ret := TJSObject.Create(p, FEngine, name, self);
  end
  else
    ret := nil;
end;

function TJSObject.getProperty(const Name: String; var bool: boolean): boolean;
var
  rval: jsval;
begin
  CheckConnection;
  Result := FEngine.InternalGet(name, Fjsobj, rval);

  if (Result) and (not JSValIsNull(rval)) then
    bool := JSValToBoolean(rval)
  else
    bool := false;
end;

function TJSObject.getProperty(const Name: String; var str: String): boolean;
var
  rval: jsval;
begin
  CheckConnection;
  Result := FEngine.InternalGet(name, Fjsobj, rval);

  if (Result) and (not JSValIsNull(rval)) then
  begin
    str := JSValToString(FEngine.Context, rval);
    UniqueString(str);
  end
  else
    str := '';
end;

function TJSObject.HasProperty(const Name: String): boolean;
var
  rval: jsval;
begin
  CheckConnection;
  JS_LookupUCProperty(FEngine.Context, Fjsobj, PWideChar(name), Length(name), @rval);
  Result := (not JSValIsVoid(rval));
end;

procedure TJSObject.InternalConnect;
begin
  if (not IsLive) and (CanGoLive) then
  begin
//    JS_RemoveValueRoot(FEngine.Context, @FJSVal);

    FScope.setProperty(FName, self);
  end;
end;

function TJSObject.IsFunction(const Name: String): boolean;
var
  rval: jsval;
begin
  CheckConnection;
  JS_LookupUCProperty(FEngine.Context, Fjsobj, PWideChar(name), Length(name), @rval);
  if (not JSValIsVoid(rval)) then
    Result := (JS_TypeOfValue(FEngine.Context, rval) = JSTYPE_FUNCTION)
  else
    Result := false;
end;

function TJSObject.IsInteger(const Name: String): boolean;
var
  rval: jsval;
begin
  CheckConnection;
  JS_LookupUCProperty(FEngine.Context, Fjsobj, PWideChar(name), Length(name), @rval);
  if (not JSValIsVoid(rval)) then
    Result := JSValIsInt(rval)
  else
    Result := false;
end;

procedure TJSObject.RemoveObject(Obj: TJSBase);
var
  parent: PJSObject;
  rval: jsval;
begin
  CheckConnection;
  parent := Obj.parent.JSObject;
  JS_DeleteUCProperty2(FEngine.Context, parent, PWideChar(Obj.JSName), Length(Obj.JSName), @rval);
  Obj.Free;
end;

function TJSObject.setProperty(const Name: String; val: TValue; flags : integer ): boolean;
var
  v: jsval;
begin
  v := TJSClass.TValueToJSVal(FEngine.Context, val);
  if (HasProperty(name)) then
    Result := (JS_SetUCProperty(FEngine.Context, Fjsobj, PWideChar(name), Length(name), @v) = js_true)
  else
    Result := (JS_DefineUCProperty(FEngine.Context, Fjsobj, PWideChar(name), Length(name), v, nil, nil,
      { JSPROP_READONLY or } JSPROP_ENUMERATE or flags) = js_true);

end;

function TJSObject.setProperty(const Name: String; val: TJSBase; flags : integer ): boolean;
begin
  CheckConnection;
  if (HasProperty(name)) then
    Result := (JS_SetUCProperty(FEngine.Context, Fjsobj, PWideChar(name), Length(name), @val.JScriptVal)
      = js_true)
  else
    Result := (JS_DefineUCProperty(FEngine.Context, Fjsobj, PWideChar(name), Length(name), val.JScriptVal, nil,
      nil, JSPROP_ENUMERATE or flags) = js_true);
end;

function TJSObject.TypeOf(const Name: String): JSType;
var
  rval: jsval;
begin
  CheckConnection;
  if (FEngine.InternalGet(name, Fjsobj, rval)) then
    Result := JS_TypeOfValue(FEngine.Context, rval)
  else
    Result := JSTYPE_VOID;
end;

{ TJSScript }

procedure TJSScript.Compile(const AFileName: string);
var
  r: JSBool;
begin
  FScript := FEngine.Compile(FCode, AFileName);
  if Fscript <> NIL then
  begin
    r := JS_AddObjectRoot(FEngine.Context, @FScript);
  end;
  FCompiled := FScript <> nil;
end;

constructor TJSScript.Create;
begin
  FCode := '';
  FScript := nil;
end;

constructor TJSScript.Create(AEngine: TJSEngine; const ACode: String; const AFileName: string);
begin
  FEngine := AEngine;
  FCode := ACode;
  FFileName := AFileName;
  Compile(FFileName);
end;

destructor TJSScript.Destroy;
var
  r: JSBool;
begin
  if Assigned(FEngine) and Assigned(FScript) then
  begin
    //JS_DestroyScript(FEngine.Context, FScript);
    r := JS_RemoveObjectRoot(FEngine.Context, @FScript);
    JS_MaybeGC(FEngine.Context);
  end;
  inherited;
end;

procedure TJSScript.Execute(AScope: TJSObject);
var
  scriptObj: PJSObject;
begin
  if AScope = NIL then
  begin
     AScope := FEngine.Global;
  end;

  if (not FCompiled) then
    Compile();

  if FScript <> nil then
  begin
    // // Create object based on it so we can root it
    // scriptObj := JS_NewScriptObject(FEngine.Context, fscript);
    // if JS_AddRoot(FEngine.Context, @scriptObj) = js_true then
    // begin
    FEngine.Execute(FScript, AScope);
    // Remove rooting, allowing GC to take place
    // JS_RemoveRoot(FEngine.Context, scriptObj);
    // end;
  end;
end;

procedure TJSScript.LoadCompiled(const AFile: String);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(AFile, fmOpenRead);
  try
    LoadCompiledFromStream(fs);
  finally
    fs.Free;
  end;
end;

procedure TJSScript.LoadCompiledFromStream(AStream: TStream);
var
  ms: TMemoryStream;
  xdr: PJSXDRState;
  data: PWideChar;
  len: size_t;
begin
  ms := TMemoryStream.Create;
  try
    ms.LoadFromStream(AStream);

    ms.Position := 0;
    data := ms.Memory;
    len := ms.Size;

    xdr := JS_XDRNewMem(FEngine.Context, JSXDR_DECODE);
    if (xdr <> nil) then
    begin
      JS_XDRMemSetData(xdr, data, len);
      JS_XDRScript(xdr, FScript);
    end;

    FCompiled := true;
    FCode := '';
  finally
    ms.Free;
  end;
end;

procedure TJSScript.LoadRaw(const AFile: String);
var
  s: TStringList;
begin
  s := TStringList.Create;
  try
    s.LoadFromFile(AFile);
    FCode := s.Text;

    FCompiled := false;
    FScript := nil;
  finally
    s.Free;
  end;
end;

class function TJSScript.LoadScript(Stream: TStream): string;
begin
  with TStreamReader.Create(Stream, TEncoding.UTF8, true) do
    try
      Result := ReadToEnd;
    finally
      Free;
    end;
end;

class function TJSScript.LoadScript(FileName: string): string;
begin
  with TStreamReader.Create(fileName, TEncoding.UTF8, true) do
  try
    Result := ReadToEnd;
  finally
    Free;
  end;

end;

procedure TJSScript.SaveCompiled(const AFile: String);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(AFile, fmCreate);
  try
    SaveCompiledToStream(fs);
  finally
    fs.Free;
  end;
end;

procedure TJSScript.SaveCompiledToStream(AStream: TStream);
var
  xdr: PJSXDRState;
  data: Pointer;
  len: size_t;
begin
  if (not FCompiled) then
    Compile();

  xdr := JS_XDRNewMem(FEngine.Context, JSXDR_ENCODE);
  if (xdr <> nil) and (JS_XDRScript(xdr, FScript) = js_true) then
  begin
    data := JS_XDRMemGetData(xdr, @len);
    AStream.Write(data^, len);
  end
  else
    raise Exception.Create('The compiled script code may be corrupted; unable to save it to disk.');
end;

procedure TJSScript.SaveRaw(const AFile: String);
var
  s: TStringList;
begin
  s := TStringList.Create;
  try
    s.Text := FCode;
    s.SaveToFile(AFile);
  finally
    s.Free;
  end;
end;

{ JSClassNameAttribute }

constructor JSClassNameAttribute.Create(ClassName: string);
begin
  FClassName := ClassName;
end;

{ TJSClassProto }

function TJSClassProto.CreateNativeObject(cx: PJSContext; AClass: TClass): TObject;
var
  t: TRttiType;
  m: TRttiMethod;
  methodResult: TValue;
  args: TArray<TValue>;
begin
  // constructor Create(AOwner: TComponent); override;
  t := RttiContext.GetType(AClass);
  for m in t.GetMethods do
  begin
    if m.IsConstructor and (Length(m.GetParameters) = 0) then
    begin
      args := nil;
      methodResult := m.Invoke(AClass, args);
      exit(methodResult.AsObject);
    end;

  end;

  Result := AClass.Create;
end;

constructor TJSClassProto.Create(AClass: TClass; AClassFlags: TJSClassFlagAttributes);
begin
  inherited Create;
//  inc(TJSClassProtoCount);
  FClassFlags := AClassFlags;
  Fclass_methods := nil;
  Fclass_props := nil;
  FConsts := nil;
  FMethodNamesMap := TStringList.Create;

  DefineJSClass(AClass, AClassFlags);

end;

class procedure TJSClassProto.DefineEnum(cx: PJSContext; Obj: TJSObject; pt: TRttiType);
  var
    i: Integer;
    st: TRttiSetType;
    ot: TRttiOrdinalType;
    ename: string;
    proc: TIntToIdent;

  begin
    if pt = nil then exit;

    if pt.Handle^.Kind = tkEnumeration then
    begin
      ot := pt.AsOrdinal;
      for i := ot.MinValue to ot.MaxValue do
      begin
        Obj.setProperty(GetEnumName(pt.Handle, i), i, JSPROP_READONLY or JSPROP_PERMANENT);
      end;
    end
    else if pt.IsSet then
    begin
      st := pt.AsSet;
      pt := st.ElementType;
      if pt.IsOrdinal then
      begin
        ot := pt.AsOrdinal;
        for i := ot.MinValue to ot.MaxValue do
        begin
          Obj.setProperty(GetEnumName(pt.Handle, i), 1 shl i, JSPROP_READONLY or JSPROP_PERMANENT);
        end;
      end;

    end
    else if pt.Handle^.Kind = tkInteger then
    begin
      // proc := FindIntToIdent(pt.handle) ;
    end;

end;

class procedure TJSClassProto.DefineFunctions(cx: PJSContext; obj: PJSObject; funcs: TJSFunctionsDynArray);
var
  i: Integer;
  b: PJSFunction;
begin
  for i := 0 to high(funcs) do
  begin
     if funcs[i].name <> nil then
       b := JS_DefineFunction(cx, obj,
          funcs[i].name, funcs[i].call, funcs[i].nargs, funcs[i].flags);
  end;
end;

procedure TJSClassProto.DefineJSClass(AClass: TClass; AClassFlags: TJSClassFlagAttributes);
var

  clName: AnsiString;
  methodName: string;
  // pt: TRttiType;
  p: TRttiProperty;
{$IF CompilerVersion >= 23}
  ip: TRttiIndexedProperty;
{$IFEND}
  m: TRttiMethod;
  f: TRttiField;
  // a: TRttiParameter;

  tinyid: shortInt;
  jsobj: PJSObject;
  jsp: PJSObject;
  j,i: Integer;
  Enums: TDictionary<string, Integer>;
  a: TCustomAttribute;
  exclude: boolean;
  clFlags: TJSClassFlagAttributes;
  defaultCtorSimple, defaultCtor: TRttiMethod;

  clctx: TRttiContext;
  clt: TRttiType;
  param: TRttiParameter;
  params: TArray<TRttiParameter>;

  DuplicateStrings: TStringList;
  freeExists: boolean;
  Visibility: TMemberVisibility;

  procedure defineEnums(pt: TRttiType);
  var
    i: Integer;
    st: TRttiSetType;
    ot: TRttiOrdinalType;
    ename: string;
    proc: TIntToIdent;
    procedure defineEnum(ename: string; v: Integer);
    begin

      if Enums.ContainsKey(ename) then
        exit;

      Enums.Add(ename, v);
      SetLength(FConsts, Length(FConsts) + 1);
      FConsts[high(FConsts)].dval := v;
      FConsts[high(FConsts)].Name := strdup(ename);
      FConsts[high(FConsts)].flags := JSPROP_ENUMERATE or JSPROP_READONLY or JSPROP_PERMANENT;
    end;

  begin
    // FindIntToIdent(pt.handle);
    if pt = nil then exit;

    if pt.Handle^.Kind = tkEnumeration then
    begin
      ot := pt.AsOrdinal;
      for i := ot.MinValue to ot.MaxValue do
      begin
        defineEnum( { pt.Name + '_' + } GetEnumName(pt.Handle, i), i);
      end;
    end
    else if pt.IsSet then
    begin

      st := pt.AsSet;
      pt := st.ElementType;
      if pt.Handle.Kind = tkChar then  exit;
      if pt.IsOrdinal then
      begin
        ot := pt.AsOrdinal;
        for i := ot.MinValue to ot.MaxValue do
        begin
          defineEnum(
            { pt.Name + '_' + } GetEnumName(pt.Handle, i), 1 shl i);
        end;
      end;

    end
    else if pt.Handle^.Kind = tkInteger then
    begin
      // proc := FindIntToIdent(pt.handle) ;
    end;
  end;


  function PropExist(const PropName:string; List:TArray<TRttiProperty>) : Boolean;
  var
   Prop: JSPropertySpec;
  begin
    result:=False;
    for Prop in Fclass_props  do
     if SameText(PropName, Prop.Name) then
     begin
       Result:=True;
       break;
     end;
  end;

begin

  Enums := TDictionary<string, Integer>.Create;
  FClass := AClass;
  Fclass_methods := NIL;
  Fclass_props := NIL;
  Fclass_indexedProps := NIL;
  FClass_fields := nil;
  FConsts := nil;
  defaultCtor := nil;
  defaultCtorSimple := nil;
  FJSCtor := nil;
  freeExists := false;

  FRttiType := RttiContext.GetType(AClass);
  // FRttiType := FRttiType;
  if FRttiType = NIL then
    raise Exception.Create('Fatal: RttiContext.getClass failed');

  clName := FRttiType.Name;
  clFlags := AClassFlags;
  DuplicateStrings:= TStringList.Create;
  //DuplicateStrings.Sorted := true;

  if cfaProtectedMethods in AClassFlags then
     Visibility :=  mvProtected
  else
     Visibility :=  mvPublic;

  for a in FRttiType.GetAttributes do
  begin
    if a is JSClassNameAttribute then
      clName := JSClassNameAttribute(a).FClassName
    else if a is JSClassFlagsAttribute then
      clFlags := JSClassFlagsAttribute(a).FClassFlags;
  end;

  for m in FRttiType.GetMethods do
  begin

    // m.GetAttributes
    exclude := false;
    methodName := m.Name;
    // cfaInheritedMethods, cfaInheritedProperties
    for a in m.GetAttributes do
    begin
      if (a is JSCtorAttribute) and m.IsConstructor then
      begin
        FJSCtor := m;
      end;

      if (a is JSExcludeAttribute) then
        exclude := true;

      if (a is JSNameAttribute) then
      begin
        methodName := JSNameAttribute(a).FName;
        FMethodNamesMap.Values[JSNameAttribute(a).FName] := m.Name;
      end;
    end;

    Params := m.GetParameters;
    // Default js ctor for tcomponent inherited objects
    if m.IsConstructor and (Length(Params) = 1) and (Params[0].ParamType.Handle = TypeInfo(TComponent))
    then
    begin
      defaultCtor := m;
    end
    else if m.IsConstructor and (Length(Params) = 0)
    then
    begin
      defaultCtorSimple := m;
    end;

    if (m.parent <> FRttiType) and (not(cfaInheritedMethods in clFlags)) then
      exclude := true;

    //outputdebugstring(pchar('methodname: ' + methodName));
    if methodName = 'Read' then
       methodName := methodName;

    //try
{$IF CompilerVersion >= 23}
    if not m.HasExtendedInfo then continue;
{$ifend}

    if exclude or m.IsConstructor or m.IsDestructor or m.IsStatic or m.IsClassMethod or
      (not(m.MethodKind in [mkProcedure, mkFunction])) or (m.Visibility < Visibility) then
      continue;

    for param in Params do
      defineEnums(param.ParamType);

    if not freeExists then
       freeExists := methodName = 'Free';

    SetLength(Fclass_methods, Length(Fclass_methods) + 1);
    Fclass_methods[high(Fclass_methods)].Name := strdup(methodName);
    Fclass_methods[high(Fclass_methods)].Call := @TJSClass.JSMethodCall;
    Fclass_methods[high(Fclass_methods)].nargs := Length(Params);
    Fclass_methods[high(Fclass_methods)].flags := JSPROP_ENUMERATE;//JSPROP_SHARED or JSPROP_READONLY or JSPROP_ENUMERATE or JSPROP_PERMANENT;
    Fclass_methods[high(Fclass_methods)].extra := 0;

  end;

  // Set default CTOR for TComponent parents
  if FJSCtor = nil then
    FJSCtor := defaultCtor;

  //if FJSCtor = nil then
  //  FJSCtor := defaultCtorSimple; // fallback to TObject.Create


  // Support only integer indexed properties
{$IF CompilerVersion >= 23}
   DuplicateStrings.Clear;
  for ip in FRttiType.GetIndexedProperties do
  begin
    if (ip.parent <> FRttiType) and (not(cfaInheritedProperties in clFlags)) then
      continue;

    exclude := false;
    for a in ip.GetAttributes do
    begin
      if (a is JSExcludeAttribute) then
      begin
        exclude := true;
        break;
      end;
    end;

    if exclude or (ip.ReadMethod = nil) or (Length(ip.ReadMethod.GetParameters) = 0) or (ip.Visibility < mvPublic) then
      continue;

    if (ip.ReadMethod.GetParameters[0].ParamType.Handle <> TypeInfo(Integer)) then
      continue;

    if DuplicateStrings.IndexOf(ip.name) <> -1 then
       continue;
    DuplicateStrings.Add(ip.name);

    SetLength(Fclass_indexedProps, Length(Fclass_indexedProps) + 1);
    Fclass_indexedProps[high(Fclass_indexedProps)].flags := JSPROP_SHARED or JSPROP_READONLY or JSPROP_ENUMERATE or JSPROP_PERMANENT;
    Fclass_indexedProps[high(Fclass_indexedProps)].Name := strdup(ip.Name);
    Fclass_indexedProps[high(Fclass_indexedProps)].getter := @TJSClass.JSIndexedPropRead;
    Fclass_indexedProps[high(Fclass_indexedProps)].tinyid := 0;//High(Fclass_indexedProps) - 127;

  end;

{$IFEND}

  if cfaProtectedFields in AClassFlags then
     Visibility :=  mvProtected
  else
     Visibility :=  mvPublic;

  DuplicateStrings.Clear;
  for f in FRttiType.GetFields do
  begin

    if (f.parent <> FRttiType) and (not(cfaInheritedProperties in clFlags)) then
      continue;

    exclude := false;
    for a in f.GetAttributes do
    begin
      if (a is JSExcludeAttribute) then
      begin
        exclude := true;
        break;
      end;
    end;

    if exclude or (f.Visibility < Visibility) then
      continue;

    if DuplicateStrings.IndexOf(f.name) <> -1 then
       continue;

    DuplicateStrings.Add(f.name);
    defineEnums(f.FieldType);

    SetLength(Fclass_fields, Length(Fclass_fields) + 1);
    Fclass_fields[high(Fclass_fields)].flags := JSPROP_SHARED or JSPROP_ENUMERATE or JSPROP_PERMANENT or JSPROP_READONLY;
    Fclass_fields[high(Fclass_fields)].Name := strdup(f.Name);
    Fclass_fields[high(Fclass_fields)].getter := @TJSClass.JSFieldRead;
    Fclass_fields[high(Fclass_fields)].tinyid := 0;//High(Fclass_fields) - 127;

  end;

  DuplicateStrings.Clear;
  //  TArray<TRttiProperty>
  for p in FRttiType.GetProperties do
  begin

    if (p.parent <> FRttiType) and (not(cfaInheritedProperties in clFlags)) then
      continue;

//    if p.name = 'Position' then
//    exclude := false;

    exclude := false;
    for a in p.GetAttributes do
    begin
      if (a is JSExcludeAttribute) then
      begin
        exclude := true;
        break;
      end;
    end;

    if exclude or (p.Visibility < mvPublic) then
      continue;

    if p.Name = 'WindowState' then
       exclude := false;

    //if PropExist(p.name, Properties) then
    //   continue;

    if DuplicateStrings.IndexOf(p.name) <> -1 then
       continue;
    DuplicateStrings.Add(p.name);

    defineEnums(p.PropertyType);

    SetLength(Fclass_props, Length(Fclass_props) + 1);
    Fclass_props[high(Fclass_props)].flags := JSPROP_ENUMERATE or JSPROP_PERMANENT or JSPROP_SHARED;
    Fclass_props[high(Fclass_props)].Name := strdup(p.Name);//PAnsiChar(High(Fclass_props));//strdup(p.Name);
    if p.IsReadable then
      Fclass_props[high(Fclass_props)].getter := @TJSClass.JSPropRead;

    if p.IsWritable then
      Fclass_props[high(Fclass_props)].setter := @TJSClass.JSPropWrite
    else
      Fclass_props[high(Fclass_props)].flags := Fclass_props[high(Fclass_props)].flags or JSPROP_READONLY;

    Fclass_props[high(Fclass_props)].tinyid := 0;//High(Fclass_props) - 127;

  end;

  (*for fs in FClass_Methods do
  begin
    if fs.name = 'Free' then

  end;
  *)

  if not freeExists then
  begin
    SetLength(Fclass_methods, Length(Fclass_methods) + 1);
    Fclass_methods[high(Fclass_methods)].Name := strdup('Free');
    Fclass_methods[high(Fclass_methods)].Call := @TJSClass.JSMethodCall;
    Fclass_methods[high(Fclass_methods)].nargs := Length(Params);
    Fclass_methods[high(Fclass_methods)].flags := JSPROP_ENUMERATE;//JSPROP_SHARED or JSPROP_READONLY or JSPROP_ENUMERATE or JSPROP_PERMANENT;
    Fclass_methods[high(Fclass_methods)].extra := 0;
  end;

  // NULL terminate array
  SetLength(Fclass_props, Length(Fclass_props) + 1);
  SetLength(Fclass_indexedProps, Length(Fclass_indexedProps) + 1);
  SetLength(Fclass_fields, Length(Fclass_fields) + 1);
  SetLength(Fclass_methods, Length(Fclass_methods) + 1);
  SetLength(FConsts, Length(FConsts) + 1);

  FillChar(Fclass_props[Length(Fclass_props) - 1], SizeOf(JSPropertySpec), 0);
  FillChar(Fclass_indexedProps[Length(Fclass_indexedProps) - 1], SizeOf(JSPropertySpec), 0);
  FillChar(Fclass_fields[Length(Fclass_fields) - 1], SizeOf(JSPropertySpec), 0);
  FillChar(Fclass_methods[Length(Fclass_methods) - 1], SizeOf(JSFunctionSpec), 0);
  FillChar(FConsts[Length(FConsts) - 1], SizeOf(JSConstDoubleSpec), 0);

  FJSClass.Name := strdup(clName);
//  SetReservedSlots(FJSClass, 255);
  FJSClass.flags := JSCLASS_HAS_PRIVATE;
  FJSClass.addProperty := JS_PropertyStub;
  FJSClass.delProperty := JS_PropertyStub;
  FJSClass.getProperty := TJSClass.JSPropReadClass;
  FJSClass.setProperty := JS_StrictPropertyStub;
  FJSClass.enumerate := JS_EnumerateStub;
  FJSClass.resolve := JS_ResolveStub;
  FJSClass.convert := JS_ConvertStub;
  FJSClass.finalize := TJSClass.JSObjectDestroy;
  FreeAndNil(Enums);
  DuplicateStrings.Free;

end;

class procedure TJSClassProto.DefineProperties(cx: PJSContext; obj: PJSObject; props: TJSPropertiesDynArray);
var
  i: Integer;
  ok: JSBool;
begin
  for i := 0 to High(Props) do
  begin
    if Props[i].name = NIL then continue;

    ok := JS_DefineProperty( cx,
                      obj,
                      Props[i].name,
                      JSVAL_NULL,
                      Props[i].getter,
                      Props[i].setter,
                      Props[i].flags);

  end;


end;

class procedure TJSClassProto.DeleteProperties(cx: PJSContext; obj: PJSObject; props: TJSPropertiesDynArray);
var
  i: Integer;
  ok: JSBool;
begin
  for i := 0 to High(Props) do
  begin
    if Props[i].name = NIL then continue;

    ok := JS_DeleteProperty( cx, obj, Props[i].name);
  end;
end;

destructor TJSClassProto.Destroy;
var
  i: Integer;
begin

  for i := 0 to High(Fclass_fields) - 1 do
    freeMem(Fclass_fields[i].Name);

  for i := 0 to High(Fclass_indexedProps) - 1 do
    freeMem(Fclass_indexedProps[i].Name);

  for i := 0 to High(Fclass_methods) - 1 do
    freeMem(Fclass_methods[i].Name);

  for i := 0 to High(Fclass_props) - 1 do
    freeMem(Fclass_props[i].Name);

  for i := 0 to High(FConsts) - 1 do
    freeMem(FConsts[i].Name);

  if Assigned(FJSClass.Name) then
    freeMem(FJSClass.Name);

  FMethodNamesMap.Free;

//  dec(TJSClassProtoCount);
  //Fctx.Free;
  inherited;
end;

function TJSClassProto.getJSClassName: string;
begin
  Result := FJSClass.Name;
end;

procedure TJSClassProto.JSInitClass(AEngine: TJSEngine);
var
  B: JSBool;
  ctorObj: PJSObject;
  i: Integer;
begin
  if FJSClassProto = nil then
  begin
  //FJSClass.finalize := TJSClass.JSObjectDestroy;
    FJSClassProto := JS_InitClass(AEngine.Context, AEngine.Global.JSObject, nil, @FJSClass, @TJSClass.JSObjectCtor, 0,
      nil, nil, nil, nil);
    if FJSClassProto <> nil then
    begin
      TJSClassProto.DefineFunctions(AEngine.Context, FJSClassProto, Fclass_methods);
      JS_DefineConstDoubles(AEngine.Context, AEngine.Global.JSObject, @FConsts[0]);

      TJSClassProto.DefineProperties(AEngine.Context, FJSClassProto, Fclass_fields);
      TJSClassProto.DefineProperties(AEngine.Context, FJSClassProto, Fclass_indexedProps);
      TJSClassProto.DefineProperties(AEngine.Context, FJSClassProto, Fclass_props);

      //JS_DefineProperties(AEngine.Context, FJSClassProto, @Fclass_props[0]);
    end;
  end;

end;

{ TJSClass }

class function TJSClass.JSMethodCall(cx: PJSContext; argc: uintN; vp: pjsval): JSBool;
{$POINTERMATH ON}
var
  Obj: TJSClass;
  ptr, p: Pointer;
  m: TRttiMethod;
  methodResult: TValue;
  methodName: string;
  t: TRttiType;
  args: TArray<TValue>;
  eng: TJSEngine;
  JSClassName: PAnsiChar;
  params: TArray<TRttiParameter>;
  func: PJSFunction;

  argv: pjsval;
  objval: jsval;
  jsobj: pjsobject;
  methods: TArray<TRttiMethod>;
  found: Boolean;

  function RttiMethodFindOverload(const LMethod : TRttiMethod; t : TRttiType; Instance: TValue): boolean;
  var
   Found   : Boolean;

   LIndex  : Integer;
   //argsKind: System.TypInfo.TTypeKind;
   vp: jsval;
  begin
    Result := false;
    Found:=False;
   if Length(Args)=Length(Params) then
   begin
     Found:=True;
     for LIndex:=0 to Length(Params)-1 do
     begin

       vp := argv[LIndex];
       if Params[LIndex].ParamType = nil then // untyped var
       begin
         found := JSValIsObject(vp)  and (JS_IsArrayObject(cx, JSValToObject(vp)) = js_true); // eg new Array (10);
       end
       else begin
         case Params[LIndex].ParamType.typeKind of
            tkEnumeration:
              if sameText(Params[LIndex].ParamType.name, 'boolean') then
                 found :=  JSValIsBoolean(vp)
              else
                 found := JSValIsInt(vp);
            tkSet,
            tkInteger: found := JSValIsInt(vp);
            tkFloat:  found := JSValIsNumber(vp);
            tkInt64:   found := JSValIsDouble(vp);
            tkRecord: found := JSValIsObject(vp);
            tkDynArray: found := JSValIsObject(vp) and (JS_IsArrayObject(cx, JSValToObject(vp)) = js_true);
            tkString,
            tkLString,
            tkWString,
            tkUString: found := JSValIsString(vp);
            tkWChar,
            tkChar: found := JSValIsString(vp) and (length(JSValToString(cx, vp)) = 1);
            tkPointer: found := JSValIsNull(vp) or JSValIsObject(vp) or
                       (JSValIsString(vp) and ((t.Name = 'PWideChar') or (t.Name = 'PAnsiChar')));

            tkClass: found := JSValIsNull(vp) or
                     JSValIsObjectClass(cx, vp, TJSClass) or
                     ((JSValIsObject(vp) and ((JS_TypeOfValue(cx, vp) = JSTYPE_OBJECT)) and (JS_GetClass(JSValToObject(vp)) <> nil) and (JS_GetClass(JSValToObject(vp)).Name = 'Object')) ) ;


         end;
       end;

       if not found then  break;

     end;
   end;

   Result := found;

   //if (LMethod<>nil) and Found then
  //    Result:=LMethod.Invoke(Instance, Args);
  end;

begin

  objval := JS_THIS(cx, vp);
  jsObj := JSValToObject(objval);
  // The function object is in argv[-2].
  // Use JS_ValueToFunction on it to get the JSFunction* and then JS_GetFunctionName to get its name.
  argv := JS_ARGV_PTR(cx, vp);
  func := JS_ValueToFunction(cx, argv[-2]);
  if func <> nil then
  begin
     methodName := JSStringToString(cx, JS_GetFunctionId(func));
  end
  else begin
    methodName := GetParamName(cx, argv[-2]);
    Delete(methodName, 1, Length('function '));
    Delete(methodName, pos('(', methodName), Length(methodName));
  end;

  Result := js_true;

  p := JS_GetPrivate(cx, jsobj);
  if p = nil then
    exit;

  eng := TJSClass.JSEngine(cx);

  Obj := TJSClass(p);
  t := Obj.FClassProto.FRttiType;

  if (Obj.FClassProto <> nil) and (Obj.FClassProto.FMethodNamesMap.Values[methodName] <> '') then
    methodName := Obj.FClassProto.FMethodNamesMap.Values[methodName];

  if methodName = 'Free' then
  begin
    found := true;
    if Assigned(Obj.FNativeObj) and (Obj.FNativeObj <> Obj) then
       FreeAndNil(Obj.FNativeObj);
  end
  else begin
  // Handle overload methods
    methods := t.GetMethods(methodName);
    found := false;
    for m in methods do
    begin
      if found then break;

      params := m.GetParameters;

      try
        args := TJSClass.JSArgsToTValues(params, cx, jsobj, argc, argv);
        // Try a much on parameters for overloaded methods

        if Length(methods) > 1 then
        begin
           if argc <> length(params) then
              continue;
           if not RttiMethodFindOverload(m, t, Obj.FNativeObj) then
              continue;
        end;

        found := true;
        methodResult := m.Invoke(Obj.FNativeObj, args);
        if methodResult.Kind <> tkUnknown then
        begin
           vp^ := TValueToJSVal(cx, methodResult, methodResult.typeinfo.name = 'TDateTime');
        end;

      except
        on e: Exception do
        begin
          Result := JS_FALSE;
          JS_ReportError(cx, PAnsiChar(AnsiString(e.message)), nil);
        end
      end;
    end;
  end;

  if not found then
     raise Exception.Create('Method call for "'+methodName+'" failed.');
{$POINTERMATH OFF}
end;

procedure TJSClass.JSMouseDownUpEvent(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  eventData: TJSEventData;
  f_rval: jsval;
  f_argv: array [0 .. 4] of jsval;

begin

  eventData := TJSEventData(self);
  f_argv[0] := JSObjectToJSVal(TJSClass(eventData.fobj).Fjsobj);
  f_argv[1] := TValueToJSVal(eventData.fcx, TValue.From<TMouseButton>(Button));
  f_argv[2] := TValueToJSVal(eventData.fcx, TValue.From<TShiftState>(Shift));
  f_argv[3] := TValueToJSVal(eventData.fcx, X);
  f_argv[4] := TValueToJSVal(eventData.fcx, Y);

  if JS_CallFunctionValue(eventData.fcx, eventData.fjsfuncobj,
                          JSObjectToJSVal(eventData.fjsfuncobj), 5, @f_argv, @f_rval) = js_true then
  begin
    //f_rval := 0;
  end;

end;

procedure TJSClass.JSMouseMoveEvent(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  eventData: TJSEventData;
  f_rval: jsval;
  f_argv: array [0 .. 3] of jsval;

begin

  eventData := TJSEventData(self);
  f_argv[0] := JSObjectToJSVal(TJSClass(eventData.fobj).Fjsobj);
  f_argv[1] := TValueToJSVal(eventData.fcx, TValue.From<TShiftState>(Shift));
  f_argv[2] := TValueToJSVal(eventData.fcx, X);
  f_argv[3] := TValueToJSVal(eventData.fcx, Y);

  if JS_CallFunctionValue(eventData.fcx, eventData.fjsfuncobj,
                          JSObjectToJSVal(eventData.fjsfuncobj), 4, @f_argv, @f_rval) = js_true then
  begin
    //f_rval := 0;
  end;

end;

procedure TJSClass.JSNotifyEvent(Sender: TObject);
var
  eventData: TJSEventData;
  f_rval: jsval;
  f_argv: jsval;
begin
  eventData := TJSEventData(self);
  f_argv := JSObjectToJSVal(TJSClass(eventData.fobj).Fjsobj);


//  if (JS_EnterLocalRootScope(eventData.fcx) = js_false) then exit;

  if JS_CallFunctionValue(eventData.fcx, eventData.fjsfuncobj, JSObjectToJSVal(eventData.fjsfuncobj), 1, @f_argv,
    @f_rval) = js_true then
  begin
    //f_rval := 0;
  end;

//  JS_LeaveLocalRootScope(eventData.fcx);

end;

procedure TJSClass.JSGetStrProc(const s: string);
var
  eventData: TJSEventData;
  f_rval: jsval;
  f_argv: jsval;
begin
  eventData := TJSEventData(self);
  f_argv := StringToJSVal(eventData.fcx, s);

  if JS_CallFunctionValue(eventData.fcx, eventData.fjsfuncobj, JSObjectToJSVal(eventData.fjsfuncobj), 1, @f_argv,
    @f_rval) = js_true then
  begin
    //f_rval := 0;
  end;


end;


class function TJSClass.JSPrintf(JSEngine: TJSEngine; const fmt: String; argc: Integer; args: pjsval): String;
var
  jsArgs: pjsval_ptr;
  pFmt, prev, fp, sp, p: PWideChar;
  specLen, len: Integer;
  wfmt, literal: String;
  nArg: Integer;
  nDecSep: char;
  cFlags: char;
  nPrecision, nWidth: Integer;
  padChar: Char;
  sepChar: String;
  FormatSettings: TFormatSettings;

  vDouble: Double;
  vInteger: Integer;
  JS: PJSString;

  function nextSpec(p: PWideChar): PWideChar;
  begin
    literal := '';

    while (p^ <> #0) and (p^ <> '%') do
    begin
      
      if (p^ = '\') and CharInSet((p + 1)^,  ['n', 'r', 't', 'b', '\', '%']) then
      begin
        Inc(p);
        case p^ of
          'n':
            p^ := #10;
          'r':
            p^ := #13;
          't':
            p^ := #8;
        end;
      end;

      literal := literal + p^;
      Inc(p);
    end;

    if p^ = #0 then
      Result := nil
    else
      Result := p;
  end;

begin

  jsArgs := pjsval_ptr(args);
  nArg := 0;
  Result := '';
  pFmt := PWideChar(fmt);
  prev := pFmt;
  p := nextSpec(prev);

  while (nArg < argc) and (p <> nil) do
  begin
    len := p - prev;
    { if len > 0 then
      setString(literal, prev, len)
      else
      literal := '';
    }
    // get format specifier
    Inc(p);
    nDecSep := #0;
    if p^ = ',' then
    begin
      Inc(p);
      nDecSep := p[0];
      Inc(p);
    end;
    sp := p;
    // locate first % or space character
    { repeat

      inc(p);
      until (p^ in [#0, '%', ' ', 'f', 'd', 'x', 's']) ; }
    while not(p^ in [#0, { '%', ' ', } 'f', 'd', 'x', 's']) do
    begin
      Inc(p);
    end;
    Inc(p);
    len := p - sp;
    setString(wfmt, sp, len);
    if len >= 0 then
    begin
      cFlags := #0;
      fp := sp;
      if fp[len - 1] in ['f', 'd'] then
      begin
        if fp^ in ['+', ' ', '#', '0'] then
        begin
          cFlags := fp^;
          Inc(fp);
        end;
      end;

      nWidth := 0;
      nPrecision := 0;

      // width
      sp := fp;
      while (fp^ in ['0' .. '9']) do
        Inc(fp);
      if fp <> sp then
      begin
        len := fp - sp;
        setString(wfmt, sp, len);
        nWidth := strtoIntDef(wfmt, 0);
      end;

      // precision
      if fp^ = '.' then
      begin
        Inc(fp);
        sp := fp;
        while (fp^ in ['0' .. '9']) do
          Inc(fp);
        if fp <> sp then
        begin
          len := fp - sp;
          setString(wfmt, sp, len);
          nPrecision := strtoIntDef(wfmt, 0);
        end;
      end;

      if fp^ in ['f', 'd'] then
      begin
        if cFlags = '0' then
          padChar := '0'
        else
          padChar := '#';
        sepChar := '';
        if nWidth = 0 then
          nWidth := 1;

        sepChar := ',';
        FormatSettings.ThousandSeparator := ',';
        FormatSettings.DecimalSeparator := '.';
        case nDecSep of
          '0':
            begin
              sepChar := ',';
              FormatSettings.ThousandSeparator := ',';
              FormatSettings.DecimalSeparator := '.';
            end;
          '1':
            begin
              sepChar := '';
              FormatSettings.ThousandSeparator := ',';
              FormatSettings.DecimalSeparator := '.';
            end;
          '2':
            begin
              sepChar := ',';
              FormatSettings.ThousandSeparator := '.';
              FormatSettings.DecimalSeparator := ',';
            end;
          '3':
            begin
              sepChar := '';
              FormatSettings.ThousandSeparator := '.';
              FormatSettings.DecimalSeparator := ',';
            end;
        end;

      end;

      case fp^ of
        'f', 'd':
          begin
            vDouble := JSValToDouble(JSEngine.Context, jsArgs[nArg]);

            if nPrecision > 0 then
              wfmt := sepChar + stringofchar(padChar, nWidth) + '.' + stringofchar('0', nPrecision)
            else
              wfmt := sepChar + stringofchar(padChar, nWidth);

            wfmt := formatFloat(wfmt, vDouble, FormatSettings);
            if cFlags = '+' then
            begin
              if vDouble > 0 then
                wfmt := '+' + wfmt
              else
                wfmt := '-' + wfmt;
            end
            else if (cFlags = ' ') then
            begin
              if vDouble > 0 then
                wfmt := '+' + wfmt
              else
                wfmt := ' ' + wfmt;
            end;

            Result := Result + literal + wfmt;
            // Result := Result + literal + floatToStr(JSValToDouble(JSEngine.context, jsArgs[nArg]));
          end;
        { 'd':
          begin
          vInteger := JSValToInt(jsArgs[nArg]);
          Result := Result + literal + inttoStr(vInteger);
          end; }
        'x':
          begin
            Result := Result + literal + inttoHex(JSValToInt(jsArgs[nArg]), nWidth);
          end;
        's':
          begin
            Result := Result + literal + JSValToString(JSEngine.Context, jsArgs[nArg])
            // end;
          end;
      end;
    end;

    prev := p;
    p := nextSpec(prev);
    Inc(nArg);
  end;

  if literal <> '' then
    Result := Result + literal;

end;

class function TJSClass.JSPropRead(cx: PJSContext; jsobj: PJSObject; id: jsid; vp: pjsval): JSBool;
var
  str, propName: String;
  p: Pointer;
  Obj: TJSClass;
  t: TRttiType;
  prop: TRttiProperty;
  propIndex: Integer;
  v: jsval;
  propValue: TValue;
  propClass: TJSClass;
  classObj: PJSObject;
  vid: jsval;
begin

  //OutputDebugString(PChar(Format('Before.PropRead: %.2f', [MBytes(CurrentMemoryUsage)])));
  p := JS_GetPrivate(cx, jsobj);
  if p = nil then
  begin
    // Special case for fields defined in global scope
    if TJSClass.JSEngine(cx).Global.JSObject = jsobj then
    begin
      if (JS_GetReservedSlot(cx, jsobj, 0,@v) = js_true) and JSValIsObject(v) then
      begin
         p := JS_GetPrivate(cx, JSValToObject(v));
      end;
    end;
  end;

  if p = nil then exit;

  JS_IdToValue(cx, id, @vid);
  Obj := TJSClass(p);
  t := Obj.FClassProto.FRttiType;

//  propName := JSValToString(cx, id);
  if JSValIsString(vid) then
     propName := JSValToString(cx, vid)
  else
     propName := Obj.FClassProto.Fclass_props[JSValToInt(vid) + 127].Name;


  prop := t.getProperty(propName);
  if prop <> nil then
  begin
    propClass := nil;
    propValue := prop.GetValue(Obj.FNativeObj);
    vp^ := TValueToJSVal(cx, propValue, prop.propertytype.name = 'TDateTime', obj, propName);
  end
  else
    vp^ := JSVAL_NULL;
  //OutputDebugString(PChar(Format('After.PropRead: %.2f', [MBytes(CurrentMemoryUsage)])));

  Result := js_true;
end;

class function TJSClass.JSPropReadClass(cx: PJSContext; jsobj: PJSObject; id: jsid; vp: pjsval): JSBool;
var
  propName: String;
  p: Pointer;
  Obj: TJSClass;
  t: TRttiType;
{$IF CompilerVersion >= 23}
  prop: TRttiIndexedProperty;
{$ifend}
  propIndex: Integer;
  iObj: TJSIndexedProperty;
  v: TValue;
  idx: integer;
  vid: jsval;
begin
  p := JS_GetPrivate(cx, jsobj);
  if p = nil then
    exit;
  Obj := TJSClass(p);

{$IF CompilerVersion >= 23}
  //OutputDebugString(PChar(Format('Before.PropRead: %.2f', [MBytes(CurrentMemoryUsage)])));
  JS_IdToValue(cx, id, @vid);
  if JSValIsInt(vid) then
  begin
    if (Obj.FNativeObj is TJSIndexedProperty)  then
    begin
        iObj := TJSIndexedProperty(Obj.FNativeObj);
        Obj := TJSClass(iObj.parentObj);
        t := Obj.FClassProto.FRttiType;
        prop := t.getIndexedProperty(iObj.propName);
        if prop <> nil then
        begin
          idx := JSValToInt(vid);
        //OutputDebugString(pchar(format('%s[%d]', [iObj.propName, idx])));
          v := JSValToInt(vid);
          v := prop.ReadMethod.Invoke(Obj.FNativeObj, [v]);
          vp^ := TValueToJSVal(cx, v, prop.propertytype.name = 'TDateTime'
                       , obj, format('%s[%d]', [iObj.propName, JSValToInt(vid)]));
        end;
    end
    else begin
      // Try default integer indexed property on fields
      p := nil;
      t := Obj.FClassProto.FRttiType;
      for prop in t.GetIndexedProperties do
      begin
         if prop.isDefault and
            (prop.ReadMethod <> nil) and
            (Length(prop.ReadMethod.GetParameters) = 1) and
            (prop.ReadMethod.GetParameters[0].ParamType.Handle = TypeInfo(Integer)) then
         begin
            v := JSValToInt(vid);
            v := prop.ReadMethod.Invoke(Obj.FNativeObj, [v]);
            vp^ := TValueToJSVal(cx, v, prop.propertytype.name = 'TDateTime'
                ,obj, format('%s[%d]', [prop.name, JSValToInt(vid)]));
            break;
         end;
      end;
    end;
  end;
  //OutputDebugString(PChar(Format('After.PropRead: %.2f', [MBytes(CurrentMemoryUsage)])));
{$ifend}

  Result := js_true;

end;

class function TJSClass.JSPropWrite(cx: PJSContext; jsobj: PJSObject; id: jsid; _strict:jsbool; vp: pjsval): JSBool;
var
  methodName, propName: String;
  p: Pointer;
  Obj: TJSClass;
  t: TRttiType;
  prop: TRttiProperty;
  v: TValue;
  jsfuncobj: PJSObject;
  Method: TMethod;
  eventData: TJSEventData;
  func: PJSFunction;
  vid, jv: jsval;
begin
  //debugbreak;
  p := JS_GetPrivate(cx, jsobj);
  if p = nil then
  begin
    // Special case for fields defined in global scope
    if TJSClass.JSEngine(cx).Global.JSObject = jsobj then
    begin
      if (JS_GetReservedSlot(cx, jsobj, 0,@jv) = js_true) and JSValIsObject(jv) then
      begin
         p := JS_GetPrivate(cx, JSValToObject(jv));
      end;
    end;
  end;

  if p = nil then exit;

  JS_IdToValue(cx, id, @vid);
  //if JSValIsInt(vid) then
  Obj := TJSClass(p);

  t := Obj.FClassProto.FRttiType;
  if JSValIsString(vid) then
     propName := JSValToString(cx, vid)
  else
     propName := Obj.FClassProto.Fclass_props[JSValToInt(vid) + 127].Name;

//  if propName = 'Position' then
//  begin
//     prop := t.getProperty(propName);
//  end;

  prop := t.getProperty(propName);
  if prop <> nil then
  begin
    if ((prop.PropertyType.Handle.Kind = tkMethod) or (prop.PropertyType.Handle.Kind = tkInterface)) then
    begin
      SetMethodProp(Obj.FNativeObj, propName, NilMethod);

      if (not JSValIsNull(vp^)) and JSValIsObject(vp^) then
      begin
        jsfuncobj := JSValToObject(vp^);

        if JS_ObjectIsFunction(cx, jsfuncobj) = js_true then
        begin
          methodName := '';
          func := JS_ValueToFunction(cx, vp^);
          if func <> nil then
             methodName := JSStringToString(cx, JS_GetFunctionId(func));

          //new(Method);
          if prop.PropertyType.Handle = TypeInfo(TNotifyEvent) then
          begin
            eventData := TJSEventData.Create(jsfuncobj, propName, methodName, obj , cx);
            Method.Code := @TJSClass.JSNotifyEvent;
            Method.data := Pointer(eventData); // Tricky  set method's self variable to eventdata
            SetMethodProp(Obj.FNativeObj, propName, Method);
            Obj.FEventsCode.AddOrSetValue(PropName, eventData);
          end
          else if prop.PropertyType.Handle = TypeInfo(TKeyEvent) then
          begin
            eventData := TJSEventData.Create(jsfuncobj, propName, methodName, Obj, cx);
            Method.Code := @TJSClass.JSKeyEvent;
            Method.data := eventData; // Tricky  set method's self variable to eventdata
            SetMethodProp(Obj.FNativeObj, propName, Method);
            Obj.FEventsCode.AddOrSetValue(PropName, eventData);
          end
          else if prop.PropertyType.Handle = TypeInfo(TKeyPressEvent) then
          begin
            eventData := TJSEventData.Create(jsfuncobj, propName, methodName, Obj, cx);
            Method.Code := @TJSClass.JSKeyPressEvent;
            Method.data := eventData; // Tricky  set method's self variable to eventdata
            SetMethodProp(Obj.FNativeObj, propName, Method);
            Obj.FEventsCode.AddOrSetValue(PropName, eventData);
          end
          else if prop.PropertyType.Handle = TypeInfo(TMouseEvent) then
          begin
            eventData := TJSEventData.Create(jsfuncobj, propName, methodName, Obj, cx);
            Method.Code := @TJSClass.JSMouseDownUpEvent;
            Method.data := eventData; // Tricky  set method's self variable to eventdata
            SetMethodProp(Obj.FNativeObj, propName, Method);
            Obj.FEventsCode.AddOrSetValue(PropName, eventData);
          end
          else if prop.PropertyType.Handle = TypeInfo(TMouseMoveEvent) then
          begin
            eventData := TJSEventData.Create(jsfuncobj, propName, methodName, Obj, cx);
            Method.Code := @TJSClass.JSMouseMoveEvent;
            Method.data := eventData; // Tricky  set method's self variable to eventdata
            SetMethodProp(Obj.FNativeObj, propName, Method);
            Obj.FEventsCode.AddOrSetValue(PropName, eventData);
          end
          else if prop.PropertyType.Handle = TypeInfo(TGetStrProc) then
          begin
            eventData := TJSEventData.Create(jsfuncobj, propName, methodName, Obj, cx);
            Method.Code := @TJSClass.JSGetStrProc;
            Method.data := eventData; // Tricky  set method's self variable to eventdata
            SetMethodProp(Obj.FNativeObj, propName, Method);
            Obj.FEventsCode.AddOrSetValue(PropName, eventData);
          end;
          //
        end;
      end;

    end
    else
    begin
      v := JSValToTValue(cx, prop.PropertyType.Handle, vp^, prop.PropertyType);
      prop.SetValue(Obj.FNativeObj, v);
    end;
  end;

  Result := js_true;
end;

class function TJSClass.JSValToTValue(cx: PJSContext; t: PTypeInfo; vp: jsval; RttiType: TRttiType): TValue;

var
  jsobj, jsarr: PJSObject;
  len: jsuint;
  i: Integer;
  eng: TJSEngine;
  elType: PTypeInfo;
  Values: array of TValue;
  v: TValue;
  typeData: PTypeData;
  dDate: TDateTime;
  st: TRttiSetType;
  pt: TRttiType;
  ot: TRttiOrdinalType;

  L: LongWord;
  W: Word;
  B: Byte;
  p: Pointer;
  Obj: TJSClass;
  field: TRttiField;
  pObj: PJSObject;
  vvp: jsval;
  ClassInstance: TRttiInstanceType;
  constructorMethod: TRttiMethod;
  prop: TRttiProperty;
  clasp: PJSClass;
  str, boolStr: string;

begin
  eng := TJSClass.JSEngine(cx);
  Result := TValue.Empty;
  case t^.Kind of
    tkEnumeration:
      if t = System.TypeInfo(boolean) then
      begin
        //boolStr := JSValToString(cx, vp);
        result := false;
        if JSValIsBoolean(vp) then
           Result := JSValToBoolean(vp)
        else if JSValIsNumber(vp) then
        begin
           if JSValToDouble(cx, vp) = 1 then
              result := true
           else
              result := false;
        end;
      end
      else
      begin
        if JSValIsInt(vp) then
           Result := TValue.FromOrdinal(t, JSValToInt(vp))
        else if JSValIsDouble(vp) then
           Result := TValue.FromOrdinal(t, trunc(JSValToDouble(cx, vp)));
        //Result := TValue.FromOrdinal(t, JSValToInt(vp))
       // Result := TValue.FromOrdinal(t, JSValToInt(vp));
      end;
    tkSet:
      begin

        case GetTypeData(t)^.OrdType of
          otSByte, otUByte:
            begin
              B := JSValToInt(vp);
              TValue.Make(@B, t, Result);
            end;
          otSWord, otUWord:
            begin
              W := JSValToInt(vp);
              TValue.Make(@W, t, Result);
            end;
          otSLong, otULong:
            begin
              L := JSValToInt(vp);
              TValue.Make(@L, t, Result);
            end;
        end;

      end;
    tkInt64, tkInteger:
    begin
      if JSValIsInt(vp) then
         Result := JSValToInt(vp)
      else if JSValIsDouble(vp) then
         Result := trunc(JSValToDouble(cx, vp))

    end;

    tkPointer:
    begin
       if JSValIsNull(vp) then
       begin
          Result := TValue.From<pointer>( nil);
       end
       else if JSValIsObject(vp) then
       begin
          jsobj := JSValToObject(vp);
          p := JS_GetPrivate(cx, jsobj);
          if TObject(p) is TJSClass then
          begin
            Obj := TJSClass(p);
            //Result := Pointer(Obj.FNativeObj);
            Result := TValue.From<pointer>(Pointer(Obj.FNativeObj));
          end;
          //Result := TValue.From<pointer>( nil);
       end
       else if JSValIsDouble(vp) then
       begin
          Result := TValue.From<pointer>( Pointer(NativeUINT(trunc(JSValToDouble(cx, vp)))));
       end
       else if JSValIsString(vp) then
       begin
          if (t.Name = 'PWideChar') then
             Result := TValue.From<PWideChar>( PWideChar(JSValToString(cx, vp)))
          else if (t.Name = 'PAnsiChar') then
             Result := TValue.From<PAnsiChar>( PAnsiChar(AnsiString(JSValToString(cx, vp))))

       end;
    end;

    tkFloat:
      if JSValIsNumber(vp) then
        Result := JSValToDouble(cx, vp)
      else if (JSValIsObject(vp)) and (JS_InstanceOf(cx, JSValToObject(vp), eng.DateClass, nil) = js_true) then
      begin
        if not TJSClass.JSDateToDateTime(eng, JSValToObject(vp), dDate) then
          dDate := 0;
        Result := dDate;
      end;

    tkLString:
      if not JSValIsVoid(vp) then
         Result := TValue.From<AnsiString>(AnsiString(JSValToString(cx, vp)));

    tkWString, tkString, tkUString:
      if not (JSValIsVoid(vp) or JSValIsNull(vp)) then
         Result := JSValToString(cx, vp);

    tkWChar, tkChar:
      if (not (JSValIsVoid(vp) or JSValIsNull(vp))) and (JSValIsString(vp)) then
      begin
         str := JSValToString(cx, vp);
         if Length(str) = 1 then
            Result := TValue.From<Char>(str[1])
         else
            Result := TValue.From<Char>(#0)

      end;

    tkClass:
      begin
        if JSValIsNull(vp) then
        begin
           Result := TValue.From<TObject>( nil)
        end
        else if (JSValIsObject(vp) and ((JS_TypeOfValue(cx, vp) = JSTYPE_OBJECT)) and (JS_GetClass(JSValToObject(vp)) <> nil) and (JS_GetClass(JSValToObject(vp)).Name = 'Object')) then
        begin
            pObj := JSValToObject(vp);
            ClassInstance := RttiType.AsInstance;
            constructorMethod := RttiType.GetMethod('Create'); // Default constructors only
            Result := constructorMethod.Invoke(ClassInstance.MetaclassType, []);
            for prop in RttiType.GetProperties do
            begin
               if (JS_GetProperty(cx, pObj, PAnsiChar(AnsiString(prop.Name)), @vvp) = 1) and (not JSValIsVoid(vvp)) then
               begin
                   prop.SetValue(Result.asObject, JSValToTValue(cx, prop.PropertyType.Handle, vvp, prop.PropertyType));
               end;
            end;
        end
        else if (JSValIsObject(vp)) then
        begin
          jsobj := JSValToObject(vp);
          p := JS_GetPrivate(cx, jsobj);
          if TObject(p) is TJSClass then
          begin
            Obj := TJSClass(p);
            Result := Obj.FNativeObj;
          end;

        end;
      end;
    tkDynArray:
      begin
        if (JSValIsObject(vp)) and (JS_IsArrayObject(cx, JSValToObject(vp)) = js_true) then
        begin
          typeData := GetTypeData(t);
          len := 0;
          jsarr := JSValToObject(vp);
          if JS_GetArrayLength(cx, jsarr, len) = js_true then
          begin
            SetLength(Values, len);
            for i := 0 to len - 1 do
            begin
              if JS_GetElement(cx, jsarr, i, @vp) = js_true then
              begin
                if not(JSValIsNull(vp) or JSValIsVoid(vp)) then
                begin
                  Values[i] := JSValToTValue(cx, typeData.eltype2^, vp, RttiType);
                end;
              end;
            end;
            Result := TValue.FromArray(t, Values);
          end;

        end;
      end;
    tkRecord:
      begin
        if t = TypeInfo(TValue) then
        begin
           if JSValIsString(vp) then
              result := JSValToTValue(cx, TypeInfo(string), vp, nil)
           else if JSValIsDouble(vp) then
              result := JSValToTValue(cx, TypeInfo(double), vp, nil)
           else if JSValIsInt(vp) then
              result := JSValToTValue(cx, TypeInfo(integer), vp, nil)
           else if JSValIsBoolean(vp) then
              result := JSValToTValue(cx, TypeInfo(boolean), vp, nil)
           //else if JSValIsObject(vp) then
           //   result := JSValToTValue(cx, TypeInfo(TObject), vp, nil)
           else if JSValIsNull(vp) then
              result := TValue.Empty
        end
        else begin

  (*
    // Check if this is a registered object or javascript parameters  object
    var annot = this.addAnnot({
    page: 0,
    type: "Stamp",
    }
  *)
         if (JSValIsObject(vp) and ((JS_TypeOfValue(cx, vp) = JSTYPE_OBJECT)) and (JS_GetClass(JSValToObject(vp)) <> nil) and (JS_GetClass(JSValToObject(vp)).Name = 'Object')) then
         begin

            pObj := JSValToObject(vp);
            TValue.Make(nil, RttiType.Handle, Result);
            for field in RttiType.GetFields do
            begin
               if (JS_GetProperty(cx, pObj, PAnsiChar(AnsiString(field.Name)), @vvp) = 1) and (not JSValIsVoid(vvp)) then
               begin
                   Field.SetValue(Result.GetReferenceToRawData, JSValToTValue(cx, field.FieldType.Handle, vvp, field.FieldType));
               end;
            end;
         end;

        end;

      end;
    tkMethod: // Events
      begin
        // const
        // NilMethod: TMethod = (Code: nil; Data: nil);
        TValue.Make(@NilMethod, t, Result);
        // TValue.From<TNotifyEvent>(notifyEvent);

      end;
  end;

end;

procedure TJSClass.NewJSObject(Engine: TJSEngine; JSObjectName: string; AInstance: TObject;
  AClassFlags: TJSClassFlagAttributes);
var
  B: JSBool;
  iter: PJSObject;
  id: jsid;
  nextobj: PJSObject;
  vp: jsval;
  n: string;
  i: integer;
  p: TJSClassProto;


  function getClassProto(AClassName: String): TJSClassProto;
  var
    p: TJSClassProto;
  begin
    for p in TJSEngine(Engine).FDelphiClasses.Values do
       if p.FClass.ClassName = AInstance.ClassName then exit(p);

    Result := nil;
  end;
begin

  if FClassProto <> NIL then
    raise Exception.Create('TJSClass.NewJSObject multiple calls');

  if AInstance = nil then
    AInstance := self;

  FJSEngine := Engine;
  FClassFlags := AClassFlags;

  FClassProto := GetClassProto(Ainstance.ClassName);
  if FClassProto =  nil then
  begin
     FClassProto := TJSClassProto.Create(AInstance.ClassType, AClassFlags);
  end;

  FClassProto._AddRef;

  if not TJSEngine(Engine).FDelphiClasses.ContainsKey(FClassProto.JSClassName) then
    TJSEngine(Engine).FDelphiClasses.Add(FClassProto.JSClassName, FClassProto);

  FNativeObj := AInstance;
  Fjsobj := JS_NewObject(Engine.Context, @FClassProto.FJSClass, nil, nil{Engine.Global.JSObject});
  FJSObject := TJSObject.Create(Fjsobj, Engine, JSObjectName);
  JS_SetPrivate(Engine.Context, Fjsobj, Pointer(self));
  //if (not FNativeObj.InheritsFrom(TJSClass)) and (cfaNativeObjOwner in AClassFlags) then
  //   FNativeObjOwner := True;

  if length(FClassProto.Fclass_props) > 0 then
     TJSClassProto.DefineProperties(Engine.Context, Fjsobj, FClassProto.Fclass_props);
  if length(FClassProto.Fclass_indexedProps) > 0 then
     TJSClassProto.DefineProperties(Engine.Context, Fjsobj, FClassProto.Fclass_indexedProps);
  if length(FClassProto.Fclass_fields) > 0 then
     TJSClassProto.DefineProperties(Engine.Context, Fjsobj, FClassProto.Fclass_fields);

  if cfaGlobalFields in FClassFlags then
     TJSClassProto.DefineProperties(Engine.Context, Engine.Global.JSObject, FClassProto.Fclass_fields);

  if cfaGlobalProperties in FClassFlags then
     TJSClassProto.DefineProperties(Engine.Context, Engine.Global.JSObject, FClassProto.Fclass_props);

  if (cfaGlobalProperties in FClassFlags) or (cfaGlobalFields in FClassFlags) then
  begin
     // Only one object is allowed to publish fields
     if (JS_GetReservedSlot(Engine.Context, Engine.Global.JSObject,  0,@vp) = js_true) and JSValIsVoid(vp) then
        JS_SetReservedSlot(Engine.Context, Engine.Global.JSObject, 0, JSObjectToJSVal(FJsobj));
  end;

  TJSClassProto.DefineFunctions(Engine.Context, Fjsobj, FClassProto.Fclass_methods);
  B := JS_DefineConstDoubles(Engine.Context, Engine.Global.JSObject, @FClassProto.FConsts[0]);


end;

procedure TJSClass.Notification(AClass: TJSClass; Operation: TOperation);
var
  I: Integer;
  v: TJSClass;
  p: TPair<string, TJSClass>;
begin
  if (Operation = opRemove) and (AClass <> nil) then
  begin
    for p in FPointerProps do
    begin
        if p.Value = AClass then
        begin
           FPointerProps.Remove(p.Key);
           break;
        end;
    end;
    RemoveFreeNotification(AClass);
  end;
end;

procedure TJSClass.RemoveFreeNotification(AClass: TJSClass);
begin
  RemoveNotification(AClass);
  AClass.RemoveNotification(Self);

end;

procedure TJSClass.RemoveFreeNotifications;
begin
  if FFreeNotifies <> nil then
  begin
    while Assigned(FFreeNotifies) and (FFreeNotifies.Count > 0) do
      FFreeNotifies[FFreeNotifies.Count - 1].Notification(Self, opRemove);
    FreeAndNil(FFreeNotifies);
  end;

end;

procedure TJSClass.RemoveNotification(AClass: TJSClass);
var
  Count: Integer;
begin
  if FFreeNotifies <> nil then
  begin
    Count := FFreeNotifies.Count;
    if Count > 0 then
    begin
      { On destruction usually the last item is deleted first }
      if FFreeNotifies[Count - 1] = AClass  then
        FFreeNotifies.Delete(Count - 1)
      else
        FFreeNotifies.Remove(AClass);
    end;
    if FFreeNotifies.Count = 0 then
    begin
      FFreeNotifies.Free;
      FFreeNotifies := nil;
    end;
  end;

end;

class function TJSClass.TValueToJSVal(cx: PJSContext; Value: TValue; isDateTime: boolean; classObj: TJSClass; propName: string): jsval;
var
  L: LongWord;
  B: Byte;
  W: Word;
  Obj: TObject;
  eng: TJSEngine;
  classProto: TJSClassProto;
  v: TValue;
  jsarr, jsobj: PJSObject;
  vr, val: jsval;
  argv: array[0..10] of jsval;
  oDate: PJSObject;
  propClass, retClass: TJSClass;

  RttiType: TRttiType;
  field: TRttiField;
  p: pointer;
  bb: jsbool;
  clasp: PJSClass;

begin
  Result := JSVAL_NULL;

  if Value.IsEmpty then
    exit;

  if Value.IsType<jsval> then
     exit(Value.AsType<jsval>);

  case Value.Kind of
    tkSet:
      begin
        case Value.DataSize of
          1:
            begin
              Value.ExtractRawData(@B);
              L := B;
            end;
          2:
            begin
              Value.ExtractRawData(@W);
              L := W;
            end;
          4:
            begin
              Value.ExtractRawData(@L);
            end;
        end;
        Result := IntToJSVal(L);
      end;
    tkEnumeration:
      if Value.TypeInfo = System.TypeInfo(boolean) then
        Result := BoolToJSVal(Value.AsBoolean)
      else
      begin
        Result := IntToJSVal(Value.AsOrdinal);
      end;

    tkPointer:
    begin
      if Value.IsType<pointer> then
         Result := DoubleToJSVal( NativeUINT(Value.AsType<pointer>))
      else if Value.IsType<PWideChar> then
         Result := DoubleToJSVal(NativeUINT(Value.AsType<PWideChar>))
      else if Value.IsType<PAnsiChar> then
         Result := DoubleToJSVal( NativeUINT(Value.AsType<PAnsiChar>))
//      Result := DoubleToJSVal(cx, Value.asInt64);
    end;
    tkInt64:
    begin
      if UInt64(Value.asInt64) < $ffffffff then
         Result := IntToJSVal(Value.asInt64)
      else
         Result := DoubleToJSVal( Value.asInt64);
    end;

    tkInteger:
    begin
      //if IntFitsInJSVal(Value.AsInteger) then
         Result := IntToJSVal(Value.AsInteger)
      //else
      //   Result := DoubleToJSVal(Value.AsInteger);
    end;
    tkFloat:
      begin
         //Result := DoubleToJSVal(cx, DateTimeToUnix(Value.AsExtended)*1000); // default fallback to float
         Result := DoubleToJSVal( Value.AsExtended); // default fallback to float
         if isDateTime then
         begin
           argv[0] := IntToJsVal(YearOf(Value.AsExtended));
           argv[1] := IntToJsVal(MonthOf(Value.AsExtended)-1);
           argv[2] := IntToJsVal(DayOf(Value.AsExtended));
           argv[3] := IntToJsVal(HourOf(Value.AsExtended));
           argv[4] := IntToJsVal(MinuteOf(Value.AsExtended));
           argv[5] := IntToJsVal(SecondOf(Value.AsExtended));
           argv[6] := IntToJsVal(MillisecondOf(Value.AsExtended));

           eng := TJSClass.JSEngine(cx);
           try
             // year, month, day, hours, minutes, seconds, milliseconds
             oDate := JS_ConstructObjectWithArguments(cx, eng.DateClass,nil, nil, 7, @argv);
             if oDate <> NIL then
                Result := JSObjectToJSVal(oDate);
           except
           end;
         end;


      end;
    tkLString, tkWString, tkUString:
      Result := StringToJSVal(cx, Value.AsString);
    tkClass:
      begin
        propClass := nil;
        if  assigned(classObj) and
            classObj.FPointerProps.TryGetValue(propName, propClass) and
            (Value.AsObject = propClass.FNativeObj) then
        begin
           JS_MaybeGC(cx);
           //propClass := nil; classObj.FPointerProps.Remove(propName);
           exit( JSObjectToJSVal(propClass.fjsobj) );
        end
        else begin
           if propClass <> nil then
           begin
              classObj.FPointerProps.Remove(propName);
              FreeAndNil(propClass);
           end;

        end;

        retClass := nil;
        Obj := Value.AsObject;
        if obj = NIL then
        begin
             Result := JSVAL_NULL;
        end
        else begin
          eng := TJSClass.JSEngine(cx);
          Result := JSVAL_NULL;
          for classProto in eng.FDelphiClasses.Values do
          begin
            if classProto.FRttiType.Name = Obj.ClassName then
            begin
              retClass := TJSClass.CreateJSObject(Obj, eng, '', classProto.FClassFlags);
              Result := JSObjectToJSVal(retClass.Fjsobj);
              break;
            end;

          end;

          if JSValIsNull(Result ) then
          begin
            // Create JS Object
            retClass := TJSClass.CreateJSObject(Obj, eng, '', [cfaInheritedMethods, cfaInheritedProperties]);
            Result := JSObjectToJSVal(retClass.Fjsobj);
          end;
        end;

        if assigned(classObj) and JSValIsObject(result) and (propClass = nil ) and (retClass <> nil) then
        begin
           retClass.FreeNotification(classObj);
           classObj.FPointerProps.Add(propName, retClass);
        end;


      end;
    tkDynArray:
      begin
        if Value.GetArrayLength = 0 then
        begin
          Result := JSVAL_FALSE;
        end
        else
        begin
          eng := TJSClass.JSEngine(cx);
          jsarr := JS_NewArrayObject(eng.Context, 0, nil);
          for L := 0 to Value.GetArrayLength - 1 do
          begin
            v := Value.GetArrayElement(L);
            val := TValueToJSVal(cx, v);
            JS_SetElement(eng.Context, jsarr, L, @val);
          end;
          Result := JSObjectToJSVal(jsarr);
          //clasp := JS_GetClass(jsarr);
          //bb := JS_InstanceOf(cx, jsarr{JSValToObject(vp)}, eng.slowArrayClass, nil);
          //bb := bb;
        end;
      end;
    tkRecord:
      begin
        if Value.TypeInfo = TypeInfo(TValue) then
        begin
           Result := TValueToJSVal(cx, Value.AsType<TValue>, isDateTime);


        end
        else begin
          RttiType := RttiContext.GetType(Value.TypeInfo);
          jsobj := JS_NewObject(cx, NIL, nil, nil);

          for field in RttiType.GetFields do
          begin
             v := Field.GetValue(Value.GetReferenceToRawData);
             val := TValueToJSVal(cx, v, false);
             JS_SetProperty(cx, jsObj, PAnsiChar(AnsiString(field.Name)), @val);
          end;

          Result := JSOBjectToJSVal(jsobj);
        end;
      end;
  end;

end;

constructor TJSClass.Create;
begin
  FEventsCode := TObjectDictionary<string, TJSEventData>.Create([doOwnsValues]);
  FPointerProps:= TDictionary<string, TJSClass>.Create;
  FFreeNotifies:= TList<TJSClass>.Create;
//  FNativeObjOwner := false;

end;

destructor TJSClass.Destroy;
var
  i: Integer;
begin
//  inc(NumObjsFree);

  RemoveFreeNotifications;

  if ((cfaGlobalFields in FClassFlags) or (cfaGlobalProperties in FClassFlags))  then
  begin
     if assigned(FJSEngine) then
     begin
        if FJSEngine.FDestroying = false then
            JS_SetReservedSlot(FJSEngine.Context, FJSEngine.Global.JSObject, 0, JSVAL_VOID);
     end
     else begin
        JS_SetReservedSlot(FJSEngine.Context, FJSEngine.Global.JSObject, 0, JSVAL_VOID);
     end;
  end;

  if (cfaOwnObject in FClassProto.FClassFlags) and Assigned(FNativeObj) and (FNativeObj <> self) then
     FNativeObj.Free;

  if Fjsobj <> nil then
     JS_SetPrivate(FJSEngine.Context, fjsObj, nil);   
     
  if assigned(FJSObject) then
     FJSObject.Free;

  FEventsCode.free;
  FPointerProps.free;
  if FClassProto.RefCount = 1 then
     FJSEngine.FDelphiClasses.Remove(FClassProto.JSClassName);
  FClassProto._Release;
  inherited;
end;

procedure TJSClass.FreeNotification(AClass: TJSClass);
begin
  if FFreeNotifies.IndexOf(AClass) = -1 then
     FFreeNotifies.Add(AClass);
end;

constructor TJSClass.CreateJSObject(Instance: TObject; AEngine: TJSEngine; JSObjectName: string;
  AClassFlags: TJSClassFlagAttributes);
begin
  Create;

  NewJSObject(AEngine, JSObjectName, Instance, AClassFlags);

end;

class function TJSClass.GetParamName(cx: PJSContext; id: jsval): string;
begin
  Result := JSValToString(cx, id);

end;

class procedure TJSClass.JSObjectDestroy(cx: PJSContext; Obj: PJSObject);
var
  p: Pointer;
begin
  p := JS_GetPrivate(cx, Obj);
  if p <> nil then
  begin
    JS_SetPrivate(cx, Obj, nil);
    try
      TObject(p).Free;
    except

    end;
  end;

end;

class function TJSClass.JSObjectCtor(cx: PJSContext; argc: uintN; vp: pjsval): JSBool;
var
  eng: TJSEngine;
  defClass: TJSClassProto;
  clasp: PJSClass;
  Obj: TJSClass;
  JSClassName: string;
  args: TArray<TValue>;
  methodResult: TValue;
  t: TRttiType;
  ctor, m: TRttiMethod;
  i: Integer;
  params: TArray<TRttiParameter>;
  callee, jsObj: PJSObject;
  argv: pjsval;
  func: PJSFunction;
begin
  Result := js_true;
  eng := TJSClass.JSEngine(cx);

  argv := JS_ARGV_PTR(cx, vp);
  callee := JSValToObject(JS_CALLEE(cx, vp));

  if (JS_IsConstructing(cx, vp) = false) then
  begin
    JS_ReportError(cx, 'JSCtor: Class Not yet implemented', nil);
  end;

  jsObj := JS_NewObjectForConstructor(cx, vp);

  clasp := JS_GetClass(jsobj);
  JSClassName := clasp.Name;
  if eng.FDelphiClasses.TryGetValue(JSClassName, defClass) then
  begin
    // Call objects javascript ctor methods
    if defClass.FJSCtor <> nil then
    begin
      params := defClass.FJSCtor.GetParameters;
      args := TJSClass.JSArgsToTValues(Params, cx, jsobj, argc, argv);
      try
        methodResult := defClass.FJSCtor.Invoke(defClass.FClass, args);
        // Construct TJSClass if needed
        if defClass.FClass.InheritsFrom(TJSClass) then
        begin
          Obj := methodResult.AsObject as TJSClass;
          Obj.FNativeObj := Obj;
          Obj.FJSEngine := eng;
        end
        else
        begin
          Obj := TJSClass.Create;
          Obj.FNativeObj := methodResult.AsObject;
          Obj.FJSEngine := eng;
        end;
        Obj.FClassProto := defClass;

      except
        on e: Exception do
        begin
          Result := JS_FALSE;
          JS_ReportError(cx, PAnsiChar(AnsiString('Exception: ' + e.message)), nil);
          exit;
        end
      end;
    end
    else
    begin
      if defClass.FClass.InheritsFrom(TJSClass) then
      begin
        Obj := defClass.CreateNativeObject(cx, defClass.FClass) as TJSClass;
        Obj.FNativeObj := Obj;
        TJSClass(Obj).FJSEngine := eng;
      end
      else
      begin
        // Try to find a valid constructor
        t := RttiContext.GetType(defClass.FClass);
        ctor := NIL;
        for m in t.GetMethods do
        begin
          if m.IsConstructor and (m.Visibility >= mvPublic) and (Length(m.GetParameters) = argc) then
          begin
            ctor := m;
            break;
          end;
        end;

        Obj := TJSClass.Create;
        if ctor <> nil then
        begin
          args := nil;
          if Length(m.GetParameters) > 0 then
          begin

            args := TJSClass.JSArgsToTValues(ctor.GetParameters, cx, jsobj, argc, argv);
            for i := 0 to high(args) do
            begin
              if args[i].IsEmpty then
              begin
                ctor := nil; // Unable to find a valid constructor
                break;
              end;
            end;
          end;

          try
            if ctor <> nil then
              methodResult := ctor.Invoke(defClass.FClass, args);
          except
            methodResult := TValue.Empty;
          end;
          if not methodResult.IsEmpty then
             Obj.FNativeObj := methodResult.AsObject;

        end;

        // Fallback to default constructor
        if Obj.FNativeObj = nil then
          Obj.FNativeObj := defClass.CreateNativeObject(cx, defClass.FClass);

      end;
      Obj.FClassProto := defClass;
    end;


    Obj.FClassProto._AddRef;
    Obj.FJSEngine := eng;
    Obj.Fjsobj := jsobj;
    Obj.FJSObject := TJSObject.Create(jsobj, eng, '');

    vp^ := JSObjectToJSVal(jsObj);
    Result := JS_SetPrivate(cx, jsobj, Pointer(Obj));

  end;

end;

class function TJSClass.JSArgsToTValues(params: TArray<TRttiParameter>; cx: PJSContext; jsobj: PJSObject; argc: uintN;
  argv: pjsval): TArray<TValue>;
var
  i: Integer;
  pObj: PJSObject;
  param: TRttiParameter;
  vp: jsval;
  nativeParams: TJSNativeCallParams;
  field: TRttiField;
  P: Pointer;
  rec: TRttiRecordType ;

  function getDefaultValue(t: PTypeInfo): TValue;
  var
    typeData: PTypeData;

  begin
    case t^.Kind of
      tkEnumeration:
        TValue.Make(nil, t, Result);
      //  Result := 0;
      tkFloat:
        Result := 0.0;
      tkInt64, tkInteger:
        Result := 0;
      tkLString, tkWString, tkUString:
        Result := '';
      tkDynArray:
        Result := TValue.FromArray(t, []);
      tkPointer,
      tkClass:
        Result := nil;
    end;

  end;

begin
{$POINTERMATH ON}
  SetLength(Result, Length(params));
  if Length(params) = 0 then
    exit;

  (*
    // Check if this is a registered object or javascript literal  object
    var annot = this.addAnnot({
    page: 0,
    type: "Stamp",
    author: "A. C. Robat",
    name: "myStamp",
    rect: [400, 400, 550, 500],
    contents: "Try it again, this time with order and method!",
    AP: "NotApproved"
    }

    //cx: PJSContext; argc: cardinal; vp: pjsval
  *)

  for i := 0 to High(params) do
  begin

    param := params[i];
    // ParamType may be nil if it's an untyped var or const parameter.

    if (param.ParamType <> nil) and (param.ParamType.Handle = TypeInfo(TJSNativeCallParams)) then
    begin
      nativeParams.cx := cx;
      nativeParams.jsobj := jsobj;
      nativeParams.argc := argc;
      nativeParams.argv := argv;
      TValue.Make(@nativeParams, TypeInfo(TJSNativeCallParams), Result[i]);
    end
    else
    begin
      if (argc = 0) or (i > argc - 1) then
      begin
        Result[i] := getDefaultValue(param.ParamType.Handle);
      end
      else
      begin
        if param.ParamType = NIL then
        begin
           Result[i] := TValue.From<pointer>( nil);
        end
        else
           Result[i] := JSValToTValue(cx, param.ParamType.Handle, argv[i], param.ParamType);
      end;
    end;

  end;

{$POINTERMATH OFF}
end;

class function TJSClass.JSDateToDateTime(JSEngine: TJSEngine; oDate: PJSObject; var dDate: TDateTime): boolean;
var
  vp: jsval;
  d, m, Y, h, mn, s, ml: Integer;
  cx: PJSContext;
begin

  Result := false;
  cx := JSEngine.Context;
  if (JS_InstanceOf(cx, oDate, JSEngine.DateClass, nil) = JS_FALSE) then
    exit;

  if JS_CallFunctionName(cx, oDate, PAnsiChar(AnsiString('getDate')), 0, nil, @vp) = JS_FALSE then
     exit
  else if JSValIsInt(vp) then
     d := JSValToInt(vp);

  if JS_CallFunctionName(cx, oDate, PAnsiChar(AnsiString('getMonth')), 0, nil, @vp) = JS_FALSE then
     exit;
  if JSValIsInt(vp) then
      m := JSValToInt(vp) + 1;

  if JS_CallFunctionName(cx, oDate, PAnsiChar(AnsiString('getFullYear')), 0, nil, @vp) = JS_FALSE then
     exit;

  if JSValIsInt(vp) then
      Y := JSValToInt(vp);

  if JS_CallFunctionName(cx, oDate, PAnsiChar(AnsiString('getHours')), 0, nil, @vp) = JS_FALSE then
     exit;
  if JSValIsInt(vp) then
      h := JSValToInt(vp);

  if JS_CallFunctionName(cx, oDate, PAnsiChar(AnsiString('getMinutes')), 0, nil, @vp) = JS_FALSE then
     exit;
  if JSValIsInt(vp) then
      mn := JSValToInt(vp);

  if JS_CallFunctionName(cx, oDate, PAnsiChar(AnsiString('getSeconds')), 0, nil, @vp) = JS_FALSE then
     exit;
  if JSValIsInt(vp) then
      s := JSValToInt(vp);

  if JS_CallFunctionName(cx, oDate, PAnsiChar(AnsiString('getMilliseconds')), 0, nil, @vp) = JS_FALSE then
     exit;
  if JSValIsInt(vp) then
      ml := JSValToInt(vp);

  dDate := encodeDateTime(Y, m, d, h, mn, s, ml);
  Result := true;

end;

function TJSClass.JSEngine: TJSEngine;
begin
  Result := FJSEngine;
end;

class function TJSClass.JSFieldRead(cx: PJSContext; jsobj: PJSObject; id: jsid; vp: pjsval): JSBool;
var
  fieldName: String;
  p: Pointer;
  Obj: TJSClass;
  t: TRttiType;
  propIndex: Integer;
  f: TRttiField;
  vid, v: jsval;
begin


  p := JS_GetPrivate(cx, jsobj);
  if p = nil then
  begin
    // Special case for fields defined in global scope
    if TJSClass.JSEngine(cx).Global.JSObject = jsobj then
    begin
      if (JS_GetReservedSlot(cx, jsobj, 0,@v) = js_true) and JSValIsObject(v) then
      begin
         p := JS_GetPrivate(cx, JSValToObject(v));
      end;
    end;
  end;

  if p = nil then exit;

  JS_IdToValue(cx, id, @vid);
  Obj := TJSClass(p);
  t := Obj.FClassProto.FRttiType;

  fieldName := JSValToString(cx, vid);
//  fieldName := Obj.FClassProto.Fclass_fields[JSValToInt(id) + 127].Name;
  f := t.getField(fieldName);
  if f <> nil then
    vp^ := TValueToJSVal(cx, f.GetValue(Obj.FNativeObj), f.FieldType.name = 'TDateTime', obj, f.Name)
  else
    vp^ := JSVAL_NULL;
  Result := js_true;

end;

{$IF CompilerVersion >= 23}

class function TJSClass.JSIndexedPropRead(cx: PJSContext; jsobj: PJSObject; id: jsid; vp: pjsval): JSBool;
var
  propName: String;
  p: Pointer;
  propClass, Obj: TJSClass;
  t: TRttiType;
  prop: TRttiIndexedProperty;
  propIndex: Integer;
  jsarr: PJSObject;
  v: TValue;
  iObj: TJSIndexedProperty;
  vid: jsval;
  propValue: TValue;
begin
  p := JS_GetPrivate(cx, jsobj);
  if p = nil then
    exit;

  JS_IdToValue(cx, id, @vid);

  Obj := TJSClass(p);
  t := Obj.FClassProto.FRttiType;

  propName := JSValToString(cx, vid);
//  propName := Obj.FClassProto.Fclass_indexedProps[JSValToInt(id) + 127].Name;
  prop := t.getIndexedProperty(propName);
  if prop <> nil then
  begin

    iObj := TJSIndexedProperty.Create;
    iObj.parentObj := Obj;
    iObj.propName := propName;
    vp^ := TValueToJSVal(cx, iObj);
    //propValue := prop.GetValue(Obj.FNativeObj);
    //vp^ := TValueToJSVal(cx, propValue, prop.propertytype.name = 'TDateTime', obj, propName);
  end
  else
    vp^ := JSVAL_NULL;

  Result := js_true;

end;
{$IFEND}

procedure TJSClass.JSKeyEvent(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  eventData: TJSEventData;
  f_rval: jsval;
  f_argv: array [0 .. 2] of jsval;
  shiftI: Integer;
begin
  // i := Byte(Shift);
  eventData := TJSEventData(self);

  shiftI := 0;
  if ssShift in Shift then
    shiftI := shiftI and Integer(ssShift);
  if ssAlt in Shift then
    shiftI := shiftI and Integer(ssAlt);
  if ssCtrl in Shift then
    shiftI := shiftI and Integer(ssCtrl);
  if ssLeft in Shift then
    shiftI := shiftI and Integer(ssLeft);
  if ssRight in Shift then
    shiftI := shiftI and Integer(ssRight);
  if ssMiddle in Shift then
    shiftI := shiftI and Integer(ssMiddle);
  if ssDouble in Shift then
    shiftI := shiftI and Integer(ssDouble);

  f_argv[0] := JSObjectToJSVal(TJSClass(eventData.fobj).{ FJSObject. } Fjsobj);
  f_argv[1] := IntToJSVal(Key);
  f_argv[2] := IntToJSVal(shiftI);

  if JS_CallFunctionValue(eventData.fcx, eventData.fjsfuncobj, JSObjectToJSVal(eventData.fjsfuncobj), 3, @f_argv,
    @f_rval) = js_true then
  begin
    if JSValIsInt(f_rval) then
    begin
      Key := JSValToInt(f_rval);
    end;
  end;

end;

procedure TJSClass.JSKeyPressEvent(Sender: TObject; var Key: Char);
var
  eventData: TJSEventData;
  f_rval: jsval;
  f_argv: array [0 .. 1] of jsval;
begin
  eventData := TJSEventData(self);
  f_argv[0] := JSObjectToJSVal(TJSClass(eventData.fobj).{ FJSObject. } Fjsobj);
  f_argv[1] := StringToJSVal(eventData.fcx, Key);

  if JS_CallFunctionValue(eventData.fcx, eventData.fjsfuncobj,
                  JSObjectToJSVal(eventData.fjsfuncobj), 2, @f_argv,
    @f_rval) = js_true then
  begin
    //f_rval := 0;
  end;

end;

class function TJSClass.JSEngine(cx: PJSContext): TJSEngine;
begin
  Result := TJSEngine(JS_GetRuntimePrivate(JS_GetRuntime(cx)));

end;

constructor TJSClass.CreateJSObject(AEngine: TJSEngine; JSObjectName: string);
begin
  Create;
  NewJSObject(AEngine, JSObjectName);
end;

{ JSNameAttribute }

constructor JSNameAttribute.Create(Name: string);
begin
  FName := Name;
end;

{ JSClassFlagsAttribute }

constructor JSClassFlagsAttribute.Create(flags: TJSClassFlagAttributes);
begin
  FClassFlags := flags;
end;

{ TJSEvent }

constructor TJSEventData.Create(ajsfuncobj: PJSObject; aeventname, amethodname: string; aobj: TObject; acx: PJSContext);
begin
  fjsfuncobj := ajsfuncobj;
  feventName := aeventname;
  fmethodname := amethodname;
  fobj := aobj;
  fcx := acx;

end;

{ TJSInternalGlobalFunctions }

class procedure TJSInternalGlobalFunctions.DebugBreak(Params: TJSNativeCallParams);
var
  eng: TJSEngine;
begin
  eng := TJSClass.JSEngine(Params.cx);
{  eng.Debugging := true;
  TJSDebugClient.Create();}

end;

Initialization
  RttiContext:= TRttiContext.Create;
Finalization
  RttiContext.Free;
end.

