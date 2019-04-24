unit IPC.Demo.Types;

interface

uses
  Winapi.Windows;

const
  MAX_LENGTH = 1024;

type
  TData = packed record
    ProcessId: DWORD;
    Text: array[0..MAX_LENGTH] of WideChar;
    procedure SetText(const AValue: string);
  end;

type
  TResponseData = packed record
    Text: array[0..MAX_LENGTH] of WideChar;
  end;

type
  PResponseData = ^TResponseData;

implementation

{ TData }

procedure TData.SetText(const AValue: string);
begin
  lstrcpynW(Text, PWideChar(AValue), MAX_LENGTH);
end;

end.

