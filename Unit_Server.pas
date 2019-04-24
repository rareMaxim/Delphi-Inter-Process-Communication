unit Unit_Server;

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
    Button2: TButton;
    Memo1: TMemo;
    Edit1: TEdit;
    Button3: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure ServerRecieveIpcData(Sender: TObject; var ClientName: WideString; var ClientWaitingForResponse: Boolean; var Data: Pointer);
  end;

var
  Form1: TForm1;
  IPCServer: TIPCServer;

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

procedure TForm1.ServerRecieveIpcData(Sender: TObject; var ClientName: WideString; var ClientWaitingForResponse: Boolean; var Data: Pointer);
var
  ResponseData: TResponseData;
begin
  Form1.Memo1.Lines.Add(ClientName + ': ' + TData(Data^).Text);
  if ClientWaitingForResponse then
  begin
    lstrcpynW(ResponseData.Text, PWideChar(Edit1.Text), MAX_LENGTH);
    if not IPCServer.SendIpcData(ClientName, @ResponseData, SizeOf(TResponseData)) then
      Form1.Memo1.Lines.Add('Error send response - ' + SysErrorMessage(IPCServer.LastError));
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if not IPCServer.CreateServer('IPC Server') then
    Form1.Memo1.Lines.Add('Error create server "' + 'IPC Server' + '" - ' + SysErrorMessage(IPCServer.LastError))
  else
  begin
    Button1.Enabled := False;
    Button2.Enabled := True;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if IPCServer.FreeServer then
  begin
    Button1.Enabled := True;
    Button2.Enabled := False;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Memo1.Clear;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  IPCServer.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  IPCServer := TIPCServer.Create(nil);
  IPCServer.OnRecieveIpcData := ServerRecieveIpcData;
  Button1.Click;
end;

end.
