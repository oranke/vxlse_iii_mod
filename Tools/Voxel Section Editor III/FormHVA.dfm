object FrmTestHVA: TFrmTestHVA
  Left = 374
  Top = 286
  BorderStyle = bsToolWindow
  Caption = 'HVA Test Area!'
  ClientHeight = 174
  ClientWidth = 200
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 39
    Height = 13
    Caption = 'Section:'
  end
  object Label2: TLabel
    Left = 8
    Top = 32
    Width = 29
    Height = 13
    Caption = 'Frame'
  end
  object SpinEdit1: TSpinEdit
    Left = 8
    Top = 48
    Width = 121
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 0
    Value = 0
    OnChange = SpinEdit1Change
  end
  object RichEdit1: TRichEdit
    Left = 8
    Top = 80
    Width = 185
    Height = 89
    BorderStyle = bsNone
    Color = clBtnFace
    Lines.Strings = (
      'When the test is on the 3d view on '
      'the main form should b changed by '
      'this test when the frame changes...... '
      'this is the hope anyway')
    TabOrder = 1
  end
  object Button1: TButton
    Left = 96
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 100
    OnTimer = Timer1Timer
    Left = 136
    Top = 128
  end
end
