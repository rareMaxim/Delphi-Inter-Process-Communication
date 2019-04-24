object Form1: TForm1
  Left = 260
  Top = 127
  Caption = 'IPC Server'
  ClientHeight = 343
  ClientWidth = 877
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    877
    343)
  PixelsPerInch = 120
  TextHeight = 17
  object Button1: TButton
    Left = 10
    Top = 311
    Width = 144
    Height = 33
    Anchors = [akLeft, akBottom]
    Caption = 'Create Server'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 162
    Top = 311
    Width = 144
    Height = 33
    Anchors = [akLeft, akBottom]
    Caption = 'Free Server'
    Enabled = False
    TabOrder = 1
    OnClick = Button2Click
  end
  object Memo1: TMemo
    Left = 10
    Top = 10
    Width = 865
    Height = 253
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object Edit1: TEdit
    Left = 10
    Top = 275
    Width = 865
    Height = 25
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 3
    Text = 'This is response text from server '
  end
  object Button3: TButton
    Left = 777
    Top = 311
    Width = 98
    Height = 33
    Anchors = [akRight, akBottom]
    Caption = 'Clear'
    TabOrder = 4
    OnClick = Button3Click
  end
end
