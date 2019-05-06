unit IPC;

interface

uses
  Winapi.Windows,
  System.Classes;

type
  TServerRecieveIpcDataEvent = procedure(Sender: TObject; var ClientName:
    WideString; var ClientWaitingForResponse: Boolean; var Data: Pointer) of object;

  TClientRecieveResponseIpcDataEvent = procedure(Sender: TObject; var Data: Pointer) of object;

  TIPCServer = class;

  TIPCClient = class;

  TServerThread = class;

  TIPCServer = class(TComponent)
  private
    FServerHandle: THandle;
    FServerThread: TServerThread;
    FOnRecieveIpcData: TServerRecieveIpcDataEvent;
    FServerThreadEnabled: Boolean;
    function ReadData(var ClientName: WideString; var ClientWaitingForResponse:
      Boolean; var Data: Pointer): Boolean;
  public
    LastError: Integer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CreateServer(ServerName: WideString): Boolean;
    function FreeServer: Boolean;
    function SendIpcData(ClientName: WideString; Data: Pointer; DataSize: DWORD): Boolean;
    property OnRecieveIpcData: TServerRecieveIpcDataEvent read FOnRecieveIpcData
      write FOnRecieveIpcData;
  end;

  TServerThread = class(TThread)
  private
    procedure DoReadData;
  protected
    ThreadOwner: TIPCServer;
    procedure Execute; override;
  public
    constructor Create(AThreadOwner: TIPCServer);
    destructor Destroy; override;
  end;

  TIPCClient = class(TComponent)
  private
    FClientName: WideString;
    FResponseServerHandle: THandle;
    FOnRecieveIpcData: TClientRecieveResponseIpcDataEvent;
    function ReadData(ServerHandle: THandle; var ClientName: WideString; var
      Data: Pointer): Boolean;
  public
    LastError: Integer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CreateClient(ClientName: WideString): Boolean;
    function FreeClient: Boolean;
    function Send<T>(ServerName: WideString; Data: T; WaitForResponse: Boolean;
      ResponseTimeout: ULONG; var ResponseData: Pointer): Boolean;
    property OnRecieveResponseIpcData: TClientRecieveResponseIpcDataEvent read
      FOnRecieveIpcData write FOnRecieveIpcData;
  end;

type
  TClientNameData = packed record
    ClientName: array[0..255] of WideChar;
    ClientWaitingForResponse: Boolean;
  end;

implementation

type
  TConvertStringSecurityDescriptorToSecurityDescriptorW = function(StringSecurityDescriptor:
    LPCWSTR; StringSDRevision: DWORD; var SecurityDescriptor: Pointer;
    SecurityDescriptorSize: PULONG): BOOL; stdcall;

var
  OSVersion: DWORD;
  ConvertStringSecurityDescriptorToSecurityDescriptorW:
    TConvertStringSecurityDescriptorToSecurityDescriptorW;

function GetOSVersion: DWORD;
var
  OSVersionInfo: TOSVersionInfo;
begin
  Result := 0;
  FillChar(OSVersionInfo, SizeOf(OSVersionInfo), 0);
  OSVersionInfo.DwOSVersionInfoSize := SizeOf(OSVersionInfo);
  if GetVersionEx(OSVersionInfo) then
  begin
    if OSVersionInfo.DwMajorVersion = 5 then
    begin
      if OSVersionInfo.DwMinorVersion = 0 then
        Result := 50 // Windows 2000
      else if OSVersionInfo.DwMinorVersion = 2 then
        Result := 52 // Windows 2003
      else if OSVersionInfo.DwMinorVersion = 1 then
        Result := 51 // Windows XP
    end
    else if OSVersionInfo.DwMajorVersion = 6 then
    begin
      if OSVersionInfo.DwMinorVersion = 0 then
        Result := 60 // Windows Vista
      else if OSVersionInfo.DwMinorVersion = 1 then
        Result := 61 // Windows 7
      else if OSVersionInfo.DwMinorVersion = 2 then
        Result := 62 // Windows 8
      else if OSVersionInfo.DwMinorVersion = 2 then
        Result := 63; // Windows 8.1  ?
    end
    else if OSVersionInfo.DwMajorVersion = 10 then
    begin
      if OSVersionInfo.DwMinorVersion = 0 then
        Result := 100; // Windows 10
    end;
  end;
end;

constructor TIPCServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FServerHandle := 0;
end;

destructor TIPCServer.Destroy;
begin
  FreeServer;
  inherited;
end;

const
  LOW_INTEGRITY_SDDL_SACL = 'D:' + '(A;OICI;GRGW;;;AU)' + '(A;OICI;GRGW;;;BA)' +
    '(A;OICI;GRGW;;;AN)' + '(A;OICI;GRGW;;;BG)' + 'S:(ML;;NW;;;LW)';
  SDDL_REVISION_1 = 1;

function TIPCServer.CreateServer(ServerName: WideString): Boolean;
var
  SecurityAttributes: TSecurityAttributes;
  SecurityDescriptor: TSecurityDescriptor;
  SecurityDescriptor_V: PSECURITY_DESCRIPTOR;
  BufferSize: DWORD;
begin
  Result := False;
  try
    if GetMailslotInfo(FServerHandle, nil, BufferSize, nil, nil) then
    begin
      LastError := ERROR_ALREADY_EXISTS;
      Exit;
    end;

    if OSVersion >= 60 then
    begin
      FillChar(SecurityDescriptor_V, SizeOf(PSECURITY_DESCRIPTOR), 0);

      if not ConvertStringSecurityDescriptorToSecurityDescriptorW(LOW_INTEGRITY_SDDL_SACL,
        SDDL_REVISION_1, SecurityDescriptor_V, nil) then
      begin
        LastError := GetLastError;
        Exit;
      end;

      SecurityAttributes.nLength := SizeOf(TSecurityAttributes);
      SecurityAttributes.lpSecurityDescriptor := SecurityDescriptor_V;
      SecurityAttributes.bInheritHandle := True;

    end
    else if (OSVersion >= 50) and (OSVersion < 60) then
    begin
      FillChar(SecurityDescriptor, SECURITY_DESCRIPTOR_MIN_LENGTH, 0);
      if InitializeSecurityDescriptor(@SecurityDescriptor, SECURITY_DESCRIPTOR_REVISION) then
      begin
        if SetSecurityDescriptorDacl(@SecurityDescriptor, True, nil, False) then
        begin
          SecurityAttributes.nLength := SizeOf(TSecurityAttributes);
          SecurityAttributes.lpSecurityDescriptor := @SecurityDescriptor;
          SecurityAttributes.bInheritHandle := True;
        end;
      end;
    end;

    FServerHandle := CreateMailslotW(PWideChar('\\.\mailslot\' + ServerName), 0,
      0, @SecurityAttributes);

    if FServerHandle = INVALID_HANDLE_VALUE then
    begin
      LastError := GetLastError;
      Exit;
    end;

    if (FServerThreadEnabled = False) then
    begin
      FServerThread := TServerThread.Create(Self);
      FServerThreadEnabled := True;
    end;

    Result := True;
    LastError := ERROR_SUCCESS;
  except
  end;
end;

function TIPCServer.FreeServer: Boolean;
begin
  Result := False;
  try
    if FServerThreadEnabled = True then
    begin
      FServerThread.Terminate;
    end;

    if FServerHandle <> 0 then
    begin
      CloseHandle(FServerHandle);
      FServerHandle := 0;
    end;

    LastError := ERROR_SUCCESS;
    Result := True;
  except
  end;
end;

function TIPCServer.ReadData(var ClientName: WideString; var
  ClientWaitingForResponse: Boolean; var Data: Pointer): Boolean;
var
  BufferSize, NumberOfBytesWritten: DWORD;
  MemoryStream: TMemoryStream;
  ClientNameData: TClientNameData;
  DataBuffer, ClientDataBuffer: PVOID;
  ReadStatus: Boolean;
begin
  Result := False;
  try
    if GetMailslotInfo(FServerHandle, nil, BufferSize, nil, nil) then
    begin
      if BufferSize <> MAILSLOT_NO_MESSAGE then
      begin
        GetMem(DataBuffer, BufferSize);
        GetMem(ClientDataBuffer, BufferSize);
        GetMem(Data, BufferSize);
        try
          if (DataBuffer <> nil) and (ClientDataBuffer <> nil) and (Data <> nil) then
          begin
            ReadStatus := ReadFile(FServerHandle, DataBuffer^, BufferSize,
              NumberOfBytesWritten, nil);
            if ReadStatus then
            begin
              MemoryStream := TMemoryStream.Create;
              try
                MemoryStream.Write(DataBuffer^, BufferSize);
                MemoryStream.Position := 0;
                MemoryStream.Read(ClientDataBuffer^, BufferSize - SizeOf(TClientNameData));
                CopyMemory(Data, ClientDataBuffer, BufferSize - SizeOf(TClientNameData));
                MemoryStream.Position := BufferSize - SizeOf(TClientNameData);
                MemoryStream.Read(ClientNameData, SizeOf(TClientNameData));
                ClientName := WideString(ClientNameData.ClientName);
                ClientWaitingForResponse := ClientNameData.ClientWaitingForResponse;
              finally
                MemoryStream.Free;
              end;
              Result := True;
            end;
          end;
        finally
          FreeMem(Data);
          FreeMem(ClientDataBuffer);
          FreeMem(DataBuffer);
        end;
      end;
    end;
  except
  end;
end;

function TIPCServer.SendIpcData(ClientName: WideString; Data: Pointer; DataSize: DWORD): Boolean;
var
  MemoryStream: TMemoryStream;
  NumberOfBytesWritten: DWORD;
  ClientData: TClientNameData;
  Buffer: Pointer;
  ServerHandle: THandle;
begin
  Result := False;
  try
    ServerHandle := CreateFileW(PWideChar('\\.\mailslot\' + ClientName),
      GENERIC_WRITE, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
    if ServerHandle = INVALID_HANDLE_VALUE then
    begin
      LastError := GetLastError;
      Exit;
    end;
    try
      lstrcpynW(ClientData.ClientName, PWideChar(ClientName), 255);
      MemoryStream := TMemoryStream.Create;
      try
        MemoryStream.Write(Data^, DataSize);
        MemoryStream.Write(ClientData, SizeOf(TClientNameData));
        MemoryStream.Position := 0;
        GetMem(Buffer, MemoryStream.Size);
        try
          MemoryStream.Read(Buffer^, MemoryStream.Size);
          if WriteFile(ServerHandle, Buffer^, MemoryStream.Size, NumberOfBytesWritten, nil) then
          begin
            LastError := ERROR_SUCCESS;
            Result := True;
          end
          else
            LastError := GetLastError;
        finally
          FreeMem(Buffer);
        end;
      finally
        MemoryStream.Free;
      end;
    finally
      CloseHandle(ServerHandle);
    end;
  except
  end;
end;

procedure TServerThread.DoReadData;
var
  Data: Pointer;
  ClientName: WideString;
  ClientWaitResponse: Boolean;
begin
  try
    Data := nil;
    if ThreadOwner.ReadData(ClientName, ClientWaitResponse, Data) then
    begin
      if Data <> nil then
      begin
        if Assigned(ThreadOwner.FOnRecieveIpcData) then
          ThreadOwner.FOnRecieveIpcData(Self, ClientName, ClientWaitResponse, Data);
      end;
    end;
  except
  end;
end;

procedure TServerThread.Execute;
begin
  while not Terminated do
  begin
    Sleep(10);
    SYNCHRONIZE(DoReadData);
  end;
  ThreadOwner.FServerThreadEnabled := False;
end;

constructor TServerThread.Create(AThreadOwner: TIPCServer);
begin
  inherited Create(False);
  ThreadOwner := AThreadOwner;
  FreeOnTerminate := True;
  case GetThreadPriority(GetCurrentThread) of
    THREAD_PRIORITY_ABOVE_NORMAL:
      Priority := tpHigher;
    THREAD_PRIORITY_BELOW_NORMAL:
      Priority := tpLower;
    THREAD_PRIORITY_HIGHEST:
      Priority := tpHighest;
    THREAD_PRIORITY_IDLE:
      Priority := tpIdle;
    THREAD_PRIORITY_LOWEST:
      Priority := tpLowest;
    THREAD_PRIORITY_NORMAL:
      Priority := tpNormal;
    THREAD_PRIORITY_TIME_CRITICAL:
      Priority := tpTimeCritical;
  end;
end;

destructor TServerThread.Destroy;
begin
  inherited;
end;

// Client
constructor TIPCClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FResponseServerHandle := 0;
end;

destructor TIPCClient.Destroy;
begin
  FreeClient;
  inherited;
end;

function TIPCClient.CreateClient(ClientName: WideString): Boolean;
var
  SecurityAttributes: TSecurityAttributes;
  SecurityDescriptor: TSecurityDescriptor;
  SecurityDescriptor_V: PSECURITY_DESCRIPTOR;
  BufferSize: DWORD;
begin
  Result := False;
  try
    if GetMailslotInfo(FResponseServerHandle, nil, BufferSize, nil, nil) then
    begin
      LastError := ERROR_ALREADY_EXISTS;
      Exit;
    end;

    if OSVersion >= 60 then
    begin
      FillChar(SecurityDescriptor_V, SizeOf(PSECURITY_DESCRIPTOR), 0);

      if not ConvertStringSecurityDescriptorToSecurityDescriptorW(LOW_INTEGRITY_SDDL_SACL,
        SDDL_REVISION_1, SecurityDescriptor_V, nil) then
      begin
        LastError := GetLastError;
        Exit;
      end;

      SecurityAttributes.nLength := SizeOf(TSecurityAttributes);
      SecurityAttributes.lpSecurityDescriptor := SecurityDescriptor_V;
      SecurityAttributes.bInheritHandle := True;
    end
    else if (OSVersion >= 50) and (OSVersion < 60) then
    begin
      FillChar(SecurityDescriptor, SECURITY_DESCRIPTOR_MIN_LENGTH, 0);
      if InitializeSecurityDescriptor(@SecurityDescriptor, SECURITY_DESCRIPTOR_REVISION) then
      begin
        if SetSecurityDescriptorDacl(@SecurityDescriptor, True, nil, False) then
        begin
          SecurityAttributes.nLength := SizeOf(TSecurityAttributes);
          SecurityAttributes.lpSecurityDescriptor := @SecurityDescriptor;
          SecurityAttributes.bInheritHandle := True;
        end;
      end;
    end;

    FResponseServerHandle := CreateMailslotW(PWideChar('\\.\mailslot\' +
      ClientName), 0, 0, @SecurityAttributes);

    if FResponseServerHandle = INVALID_HANDLE_VALUE then
    begin
      LastError := GetLastError;
      Exit;
    end;

    Result := True;
    FClientName := ClientName;
    LastError := ERROR_SUCCESS;
  except
  end;
end;

function TIPCClient.FreeClient: Boolean;
begin
  Result := False;
  try
    if FResponseServerHandle <> 0 then
    begin
      CloseHandle(FResponseServerHandle);
      FResponseServerHandle := 0;
    end;
    LastError := ERROR_SUCCESS;
    Result := True;
  except
  end;
end;

function TIPCClient.ReadData(ServerHandle: THandle; var ClientName: WideString;
  var Data: Pointer): Boolean;
var
  BufferSize, NumberOfBytesWritten: DWORD;
  MemoryStream: TMemoryStream;
  ClientNameData: TClientNameData;
  DataBuffer, ClientDataBuffer: Pointer;
begin
  Result := False;
  try
    if GetMailslotInfo(ServerHandle, nil, BufferSize, nil, nil) then
    begin
      if BufferSize <> MAILSLOT_NO_MESSAGE then
      begin
        GetMem(DataBuffer, BufferSize);
        GetMem(ClientDataBuffer, BufferSize);
        GetMem(Data, BufferSize);

        try
          if (DataBuffer <> nil) and (ClientDataBuffer <> nil) and (Data <> nil) then
          begin
            if ReadFile(ServerHandle, DataBuffer^, BufferSize, NumberOfBytesWritten, nil) then
            begin
              MemoryStream := TMemoryStream.Create;
              try
                MemoryStream.Write(DataBuffer^, BufferSize);

                MemoryStream.Position := 0;

                MemoryStream.Read(ClientDataBuffer^, BufferSize - SizeOf(TClientNameData));

                CopyMemory(Data, ClientDataBuffer, BufferSize - SizeOf(TClientNameData));

                MemoryStream.Position := BufferSize - SizeOf(TClientNameData);

                MemoryStream.Read(ClientNameData, SizeOf(TClientNameData));

                ClientName := WideString(ClientNameData.ClientName);
              finally
                MemoryStream.Free;
              end;
              Result := True;
            end;
          end;
        finally
          FreeMem(Data);
          FreeMem(ClientDataBuffer);
          FreeMem(DataBuffer);
        end;
      end;
    end;
  except
  end;
end;

function TIPCClient.Send<T>(ServerName: WideString; Data: T; WaitForResponse:
  Boolean; ResponseTimeout: ULONG; var ResponseData: Pointer): Boolean;
var
  MemoryStream: TMemoryStream;
  i, NumberOfBytesWritten: DWORD;
  ClientNameData: TClientNameData;
  Buffer: Pointer;
  ServerHandle: THandle;
  ClientName: WideString;
  BufferSize: DWORD;
begin
  Result := False;
  try
    ServerHandle := CreateFileW(PWideChar('\\.\mailslot\' + ServerName),
      GENERIC_WRITE, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
    if ServerHandle = INVALID_HANDLE_VALUE then
    begin
      LastError := GetLastError;
      Exit;
    end;

    try
      lstrcpynW(ClientNameData.ClientName, PWideChar(FClientName), 255);
      ClientNameData.ClientWaitingForResponse := WaitForResponse;

      MemoryStream := TMemoryStream.Create;
      try
        MemoryStream.Write(Data, SizeOf(Data) {DataSize});
        MemoryStream.Write(ClientNameData, SizeOf(TClientNameData));
        MemoryStream.Position := 0;

        GetMem(Buffer, MemoryStream.Size);
        try
          MemoryStream.Read(Buffer^, MemoryStream.Size);
          if not WriteFile(ServerHandle, Buffer^, MemoryStream.Size, NumberOfBytesWritten, nil) then
          begin
            LastError := GetLastError;
            Exit;
          end;
        finally
          FreeMem(Buffer);
        end;
      finally
        MemoryStream.Free;
      end;
    finally
      CloseHandle(ServerHandle);
    end;

    LastError := ERROR_SUCCESS;
    Result := True;

    if (WaitForResponse = True) and (ResponseTimeout > 0) then
    begin

      try
        i := 0;
        while True do
        begin

          if ReadData(FResponseServerHandle, ClientName, ResponseData) then
          begin
            if ResponseData <> nil then
            begin
              if ClientName = FClientName then
              begin
                if Assigned(FOnRecieveIpcData) then
                  FOnRecieveIpcData(Self, ResponseData);
                Exit;
              end;
            end;
          end;

          Inc(i);
          Sleep(1);
          if i >= ResponseTimeout then
          begin
            Exit;
          end;
        end;
      except
        Exit;
      end;
    end;
  except
  end;
end;

procedure _Initialize;
var
  hLibrary: HMODULE;
begin
  OSVersion := GetOSVersion;
  hLibrary := LoadLibrary('advapi32.dll');
  if (hLibrary <> 0) and (OSVersion >= 60) then
  begin
    @ConvertStringSecurityDescriptorToSecurityDescriptorW := GetProcAddress(hLibrary,
      'ConvertStringSecurityDescriptorToSecurityDescriptorW');
  end;
end;

initialization
  _Initialize;

end.

