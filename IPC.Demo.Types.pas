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
  end;

type
  TResponseData = packed record
    Text: array[0..MAX_LENGTH] of WideChar;
  end;

type
  PResponseData = ^TResponseData;

implementation

end.

