unit Unit_Client;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  IPC;

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
    FIPCClient: TIPCClient;
  public
    { Public declarations }
    procedure ClientRecieveResponseIpcData(Sender: TObject; var ResponseData: Pointer);
  end;

var
  Form1: TForm1;

implementation

uses
  IPC.Demo.Types;
{$R *.dfm}

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
  Data.SetText(Memo1.Text);
  if not FIPCClient.Send<TData>('IPC Server', Data, CheckBox1.Checked, 1000, ResponseData) then
    Memo1.Lines.Add('Error send IPC data - ' + SysErrorMessage(FIPCClient.LastError));
  if ResponseData <> nil then
  begin
    MessageBeep(0);
    MessageBoxW(0, TResponseData(ResponseData^).Text, 'IPC Client', 0);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FIPCClient := TIPCClient.Create(nil);
  if not FIPCClient.CreateClient('IPCClient_' + IntToStr(GetCurrentProcessId)) then
    Memo1.Lines.Add('Error create client "' + 'IPC Client' + '" - ' + SysErrorMessage(FIPCClient.LastError));
  FIPCClient.OnRecieveResponseIpcData := ClientRecieveResponseIpcData;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FIPCClient.Free;
end;

end.

