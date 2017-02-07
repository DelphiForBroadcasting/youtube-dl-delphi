{ *********************************************************************** }
{                                                                         }
{ Named Pipes Client/Server demo                                          }
{                                                                         }
{ Named Pipes class                                                       }
{                                                                         }
{ Chua Chee Wee, Singapore                                                }
{                                                                         }
{ *********************************************************************** }
unit NamedPipesImpl;

interface
uses SyncObjs, Windows, Classes, SysUtils;

const
  NamedPipeLocalHost = '.';
  NamedPipeIOBufferSize = 16384;
  NamedPipeOutputBufferSize = NamedPipeIOBufferSize;
  NamedPipeInputBufferSize = NamedPipeIOBufferSize;
type

  TError = procedure (const Msg: string) of object;

  TNamedPipe = class
  private
    FError: Cardinal;
    FOnError: TError;
    function GetPipeName: UnicodeString;
    function GetConnected: Boolean;
    procedure SetError(const Value: Cardinal);
  protected
    FSD: TSecurityDescriptor;
    FSA: TSecurityAttributes;
    FReadBuffer: UnicodeString;
    FConnectEvent, FReadEvent, FWriteEvent: TEvent;
    FConnectOverlapped, FReadOverlapped, FWriteOverlapped: TOverlapped;
    FName, FMessage, FServer, FPipeName,
    FUserName, FPassword: UnicodeString;
    FHandle: THandle;
    FTimeOut: Cardinal;
    FPending: Boolean;
    FReadTimeout: boolean;

    function GetHandle: THandle;
    procedure CreateHandle; virtual; abstract;
    procedure DoError(const Msg: string); virtual;
    procedure CreateEvents;
    property PipeName: UnicodeString read GetPipeName;
    property Error: Cardinal read FError write SetError;

  public
    constructor Create(const PipeName: UnicodeString;
      const Server: UnicodeString = NamedPipeLocalHost);
    destructor Destroy; override;

    procedure CheckConnected; virtual; abstract;
    function Open(): Boolean; virtual;
    procedure Close;

    function Read(timeout: cardinal = INFINITE): UnicodeString; overload;
    procedure Read(var Buffer: UnicodeString; timeout: cardinal = INFINITE); overload;
    procedure Write(const Message: UnicodeString); virtual;

    procedure Connect; virtual; abstract;
    procedure Disconnect; virtual;

    property Connected: Boolean read GetConnected;
    property Handle: THandle read GetHandle;
    property TimeOut: Cardinal read FTimeOut write FTimeOut;

    property OnError: TError read FOnError write FOnError;
  end;

  TNamedPipeServer = class(TNamedPipe)
  protected
    procedure CreateHandle; override;
  public
    procedure CheckConnected; override;
    procedure Connect; override;
    function Open(): Boolean; override;
  end;

  TNamedPipeClient = class(TNamedPipe)
  protected
    procedure CreateHandle; override;
  public
    procedure CheckConnected; override;
    procedure Connect; override;
    function Open(): Boolean; override;
  end;

  ENamedPipe = class(Exception)
  end;

  TNamedPipeThread = class(TThread)
  protected
   FNamedPipe: TNamedPipe;
   FMessage: UnicodeString;
   procedure WriteMessage;
  public
   constructor Create(NamedPipe: TNamedPipe);
   procedure Execute; override;
  end;

  TNamedPipeClass = class of TNamedPipe;

var NamedPipeClass: TNamedPipeClass;

implementation

procedure ErrorNamedPipe(const Message: string);
begin
  raise ENamedPipe.Create(Message);
end;

{ TNamedPipe }

procedure TNamedPipe.Close;
begin
  FReadEvent.Free;
  FWriteEvent.Free;
  FConnectEvent.Free;
  Disconnect;
  CloseHandle(FHandle);
  FHandle := 0;
  FPending := False;
end;

constructor TNamedPipe.Create(const PipeName, Server: UnicodeString);
begin
  FName := ClassName;
  FPipeName := PipeName;
  FServer := Server;
  FTimeOut := 10000; // 1000 = 1 sec
  FReadTimeout := false;
end;

procedure TNamedPipe.CreateEvents;
begin
  FReadEvent := TSimpleEvent.Create;
  FReadOverlapped.hEvent := FReadEvent.Handle;

  FWriteEvent := TSimpleEvent.Create;
  FWriteEvent.SetEvent;
  FWriteOverlapped.hEvent := FWriteEvent.Handle;

  FConnectEvent := TSimpleEvent.Create;
  FConnectOverlapped.hEvent := FConnectEvent.Handle;
end;

destructor TNamedPipe.Destroy;
begin
  Close;
  inherited;
end;

procedure TNamedPipe.Disconnect;
begin
  DisconnectNamedPipe(FHandle);
end;

function TNamedPipe.GetConnected: Boolean;
var
 Dummy: Cardinal;
begin
  Result := False;
  if not GetOverlappedResult(Handle, FConnectOverlapped, Dummy, False) then
    Error := GetLastError else
  case FConnectEvent.WaitFor(0)of
    wrTimeout:
      FPending := False;
    wrSignaled:
      Result := True;
  end;
end;

function TNamedPipe.GetHandle: THandle;
begin
  if FHandle = 0 then
    CreateHandle;
  Result := FHandle;
end;

function TNamedPipe.GetPipeName: UnicodeString;
begin
  Result := Format('\\%s\pipe\%s', [FServer, FPipeName]);
end;

function TNamedPipe.Read(timeout: cardinal): UnicodeString;
begin
  Read(Result, timeout);
end;

function TNamedPipe.Open(): Boolean;
begin
  Result := true;
end;

procedure TNamedPipe.Read(var Buffer: UnicodeString; timeout: cardinal);
var
  ToReadSize, ReadSize: Cardinal;
  e: dword;
begin


  ReadSize := 0;

  if not FReadTimeout then
  begin
     SetLength(FReadBuffer, NamedPipeOutputBufferSize);
     ToReadSize := Length(FReadBuffer) * SizeOf(FReadBuffer[1]);
     FReadEvent.ResetEvent;

     if not ReadFile(Handle, FReadBuffer[1], ToReadSize, ReadSize, @FReadOverlapped) then
     begin
        e := GetLastError;
        if ERROR_IO_PENDING <> e then
           OutputDebugString(PChar(SysErrorMessage(e)));
     end;
  end;

  if FReadEvent.WaitFor(timeout) <> wrTimeout then
  begin
    FReadTimeout := False;
    if ReadSize = 0 then
      begin
        if (not GetOverlappedResult(Handle, FReadOverlapped, ReadSize, True)) or
           (ReadSize = 0) then
         Error := GetLastError;
      end;
    SetLength(FReadBuffer, ReadSize div SizeOf(FMessage[1]));
    Buffer := FReadBuffer;
    FReadEvent.ResetEvent;
  end
  else begin
    Buffer := '';
    FReadTimeout := true;
  end;

end;

procedure TNamedPipe.Write(const Message: UnicodeString);
var
  Temp: string;
  Error, WriteSize, WrittenSize: Cardinal;
begin
  CheckConnected;
  FWriteEvent.WaitFor(INFINITE);
  FWriteEvent.ResetEvent;
  FMessage := Message;
  WriteSize := Length(FMessage) * SizeOf(FMessage[1]);
  WrittenSize := 0;
  if not WriteFile(Handle, FMessage[1], WriteSize, WrittenSize, @FWriteOverlapped) then
    begin
      if not GetOverlappedResult(Handle, FWriteOverlapped, WrittenSize, False) then
        begin
          Error := GetLastError;
          Temp := SysErrorMessage(Error);
          FMessage := Temp;
        end;
    end;
end;

procedure TNamedPipe.SetError(const Value: Cardinal);
begin
  FError := Value;
  case Value of
     ERROR_PIPE_CONNECTED: FConnectEvent.SetEvent;
     ERROR_IO_PENDING: FPending := True;
     ERROR_IO_INCOMPLETE:  ;
     ERROR_BROKEN_PIPE:
      begin
       Close;
       Open;
      end;
  end;
end;

procedure TNamedPipe.DoError(const Msg: string);
begin
  if Assigned(FOnError) then
    try
      FOnError(Msg); // Protect against a silly user error
    except
    end;
end;

{ TNamedPipeClient }

procedure TNamedPipeClient.CheckConnected;
begin
end;

procedure TNamedPipeClient.Connect;
begin
end;

procedure TNamedPipeClient.CreateHandle;
const
  ReadWrite = GENERIC_READ or GENERIC_WRITE;
  FileShare = FILE_SHARE_READ or FILE_SHARE_WRITE;
  FileFlag = FILE_ATTRIBUTE_NORMAL or FILE_FLAG_OVERLAPPED;
var
  Mode: Cardinal;
begin
  FHandle := CreateFileW(PWideChar(PipeName), ReadWrite, FileShare, nil,
    OPEN_EXISTING, FileFlag, 0);
  Mode := GetLastError;
  FMessage := SysErrorMessage(Mode);
  if FHandle = INVALID_HANDLE_VALUE then
    FHandle := 0 else
  begin
    Mode := PIPE_READMODE_MESSAGE or PIPE_WAIT;
    SetNamedPipeHandleState(FHandle, Mode, nil, nil);
    CreateEvents;
    FConnectEvent.SetEvent;
  end;
end;

function TNamedPipeClient.Open(): Boolean;
begin
  inherited Open();
  if WaitNamedPipeW(PWideChar(PipeName), NMPWAIT_USE_DEFAULT_WAIT) then
    begin
      CreateHandle;
      Result := True;
    end else
    begin
      DoError('Timed out waiting for server');
      Result := False;
    end;
end;

{ TNamedPipeServer }

procedure TNamedPipeServer.CheckConnected;
begin
  Connected;
end;

procedure TNamedPipeServer.Connect;
var
 TempError: Cardinal;
begin
  if not FPending then
    begin
      FConnectEvent.ResetEvent;
      DisconnectNamedPipe(Handle);
      if not ConnectNamedPipe(Handle, @FConnectOverlapped) then
         Error := GetLastError;
    end else
    begin
      if not GetOverlappedResult(Handle, FConnectOverlapped,
                                 TempError, True) then
        FPending := False;
    end;
end;

procedure TNamedPipeServer.CreateHandle;
begin
  InitializeSecurityDescriptor(@FSD, SECURITY_DESCRIPTOR_REVISION);
  SetSecurityDescriptorDacl(@FSD, True, PACL(0), False);
  FSA.nLength := SizeOf(FSA);
  FSA.lpSecurityDescriptor := @FSD;
  FSA.bInheritHandle := True;

  FHandle := CreateNamedPipeW(PWideChar(PipeName),
    PIPE_ACCESS_DUPLEX or FILE_FLAG_OVERLAPPED,
    PIPE_TYPE_MESSAGE or PIPE_READMODE_MESSAGE or
    PIPE_WAIT, PIPE_UNLIMITED_INSTANCES,
    NamedPipeOutputBufferSize,
    NamedPipeInputBufferSize, FTimeOut,
    @FSA);
    
  if FHandle <> INVALID_HANDLE_VALUE then
    CreateEvents else
    begin
      FHandle := 0;
      DoError('Unable to create handle');
    end;  
end;

function TNamedPipeServer.Open(): Boolean;
begin
  inherited Open(); // can't seem to use just 'inherited;'
  CreateHandle;
  Result := FHandle <> 0;
  Connect;
end;

{ TNamedPipeThread }

constructor TNamedPipeThread.Create(NamedPipe: TNamedPipe);
begin
  inherited Create(True);
  FNamedPipe := NamedPipe;
  FreeOnTerminate := True;
  Resume;
end;

procedure TNamedPipeThread.Execute;
begin
  NameThreadForDebugging(ClassName);
  while not Terminated do
    begin
      if FNamedPipe.Connected then
        begin
          FNamedPipe.Read(FMessage);
          Synchronize(WriteMessage);
        end;
    end;
end;

procedure TNamedPipeThread.WriteMessage;
begin
  //FMemo.Lines.Add(FMessage);
end;


end.

