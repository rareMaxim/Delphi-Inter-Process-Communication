object Form1: TForm1
  Left = 258
  Top = 128
  Width = 577
  Height = 273
  Caption = 'IPC Client'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    569
    245)
  PixelsPerInch = 120
  TextHeight = 17
  object Button1: TButton
    Left = 10
    Top = 201
    Width = 146
    Height = 33
    Anchors = [akLeft, akBottom]
    Caption = 'Send IPC Data'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 10
    Top = 10
    Width = 548
    Height = 178
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object CheckBox1: TCheckBox
    Left = 174
    Top = 207
    Width = 152
    Height = 22
    Anchors = [akLeft, akBottom]
    Caption = 'Wait For Response'
    TabOrder = 2
  end
end
