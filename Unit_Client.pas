unit Unit_Client;

interface

uses
{$IF CompilerVersion < 18}
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, ComCtrls, XPMan,
{$ELSE}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.XPMan,
{$IFEND}IPC;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    CheckBox1: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure ClientRecieveResponseIpcData(Sender: TObject; var ResponseData: Pointer);
  end;

var
  Form1: TForm1;
  IPCClient: TIPCClient;

implementation

{$R *.dfm}


const
  MAX_LENGTH = 1024;

type
  TData = packed record
    ProcessId: DWORD;
    Text: array [0 .. MAX_LENGTH] of WideChar;
  end;

type
  TResponseData = packed record
    Text: array [0 .. MAX_LENGTH] of WideChar;
  end;

  PResponseData = ^TResponseData;

procedure TForm1.ClientRecieveResponseIpcData(Sender: TObject; var ResponseData: Pointer);
begin
  // MessageBoxW(0, TResponseData(ResponseData^).Text, 'IPC Client', 0);
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  Data: TData;
  ResponseData: Pointer;
begin
  ResponseData := nil;
  Data.ProcessId := GetCurrentProcessId;
  lstrcpynW(Data.Text, PWideChar(Memo1.Text), MAX_LENGTH);
  if not IPCClient.SendIpcData('IPC Server', @Data, SizeOf(TData), CheckBox1.Checked, 1000, ResponseData) then
    Memo1.Lines.Add('Error send IPC data - ' + SysErrorMessage(IPCClient.LastError));
  if ResponseData <> nil then
  begin
    MessageBeep(0);
    MessageBoxW(0, TResponseData(ResponseData^).Text, 'IPC Client', 0);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  IPCClient := TIPCClient.Create(nil);
  if not IPCClient.CreateClient('IPCClient_' + IntToStr(GetCurrentProcessId)) then
    Memo1.Lines.Add('Error create client "' + 'IPC Client' + '" - ' + SysErrorMessage(IPCClient.LastError));
  IPCClient.OnRecieveResponseIpcData := ClientRecieveResponseIpcData;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  IPCClient.Free;
end;

end.
