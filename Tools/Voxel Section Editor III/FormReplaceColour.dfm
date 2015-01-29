object FrmReplaceColour: TFrmReplaceColour
  Left = 348
  Top = 127
  BorderStyle = bsToolWindow
  Caption = ' '
  ClientHeight = 422
  ClientWidth = 490
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel3: TBevel
    Left = 0
    Top = 58
    Width = 490
    Height = 7
    Align = alTop
    Shape = bsTopLine
  end
  object Bevel4: TBevel
    Left = 0
    Top = 368
    Width = 490
    Height = 8
    Align = alBottom
    Shape = bsBottomLine
  end
  object PanelTitle: TPanel
    Left = 0
    Top = 0
    Width = 490
    Height = 58
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    TabOrder = 0
    object Image1: TImage
      Left = 0
      Top = 0
      Width = 490
      Height = 58
      Align = alClient
    end
    object Label5: TLabel
      Left = 24
      Top = 24
      Width = 247
      Height = 13
      Caption = 'Here you can change one colour into another colour'
      Transparent = True
    end
    object Label6: TLabel
      Left = 8
      Top = 8
      Width = 88
      Height = 13
      Caption = 'Replace Colour'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      Transparent = True
    end
  end
  object Panel4: TPanel
    Left = 0
    Top = 376
    Width = 490
    Height = 46
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object BtOK: TButton
      Left = 327
      Top = 11
      Width = 75
      Height = 23
      Caption = 'Ok'
      TabOrder = 0
      OnClick = BtOKClick
    end
    object BtCancel: TButton
      Left = 408
      Top = 11
      Width = 75
      Height = 23
      Cancel = True
      Caption = 'Cancel'
      TabOrder = 1
      OnClick = BtCancelClick
    end
  end
  object Panel5: TPanel
    Left = 56
    Top = 72
    Width = 377
    Height = 289
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 2
    object Label1: TLabel
      Left = 192
      Top = 16
      Width = 81
      Height = 13
      Alignment = taCenter
      AutoSize = False
      Caption = 'Replace Colour'
    end
    object Label2: TLabel
      Left = 288
      Top = 16
      Width = 65
      Height = 13
      Alignment = taCenter
      AutoSize = False
      Caption = 'With Colour'
    end
    object LabelReplace: TLabel
      Left = 200
      Top = 49
      Width = 65
      Height = 13
      Alignment = taCenter
      AutoSize = False
    end
    object LabelWith: TLabel
      Left = 288
      Top = 49
      Width = 65
      Height = 13
      Alignment = taCenter
      AutoSize = False
    end
    object Bevel1: TBevel
      Left = 24
      Top = 16
      Width = 152
      Height = 238
    end
    object cnvPalette: TPaintBox
      Left = 25
      Top = 17
      Width = 150
      Height = 236
      OnMouseMove = cnvPaletteMouseMove
      OnMouseUp = cnvPaletteMouseUp
      OnPaint = cnvPalettePaint
    end
    object PanelReplace: TPanel
      Left = 200
      Top = 32
      Width = 65
      Height = 17
      TabOrder = 0
      OnClick = PanelReplaceClick
    end
    object PanelWith: TPanel
      Left = 288
      Top = 32
      Width = 65
      Height = 17
      BevelOuter = bvLowered
      TabOrder = 1
      OnClick = PanelWithClick
    end
    object ListBox1: TListBox
      Left = 248
      Top = 72
      Width = 105
      Height = 153
      ItemHeight = 13
      TabOrder = 2
      OnClick = ListBox1Click
    end
    object BtAdd: TButton
      Left = 192
      Top = 104
      Width = 49
      Height = 25
      Caption = 'Add'
      TabOrder = 3
      OnClick = BtAddClick
    end
    object BtEdit: TButton
      Left = 192
      Top = 128
      Width = 49
      Height = 25
      Caption = 'Edit'
      TabOrder = 4
      OnClick = BtEditClick
    end
    object BtDelete: TButton
      Left = 192
      Top = 152
      Width = 49
      Height = 25
      Caption = 'Delete'
      TabOrder = 5
      OnClick = BtDeleteClick
    end
    object pnlPalette: TPanel
      Left = 24
      Top = 256
      Width = 145
      Height = 21
      BevelOuter = bvNone
      TabOrder = 6
      object lblActiveColour: TLabel
        Left = 40
        Top = 2
        Width = 3
        Height = 13
        Caption = '-'
      end
      object pnlActiveColour: TPanel
        Left = 2
        Top = 2
        Width = 33
        Height = 17
        BevelOuter = bvLowered
        TabOrder = 0
      end
    end
  end
end
