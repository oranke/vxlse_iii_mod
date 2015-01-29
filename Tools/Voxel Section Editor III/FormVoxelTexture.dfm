object FrmVoxelTexture: TFrmVoxelTexture
  Left = 301
  Top = 196
  BorderIcons = []
  BorderStyle = bsToolWindow
  Caption = 'Voxel Texture'
  ClientHeight = 398
  ClientWidth = 490
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Image2: TImage
    Left = 280
    Top = 232
    Width = 65
    Height = 65
    Visible = False
  end
  object Bevel2: TBevel
    Left = 0
    Top = 58
    Width = 490
    Height = 7
    Align = alTop
    Shape = bsTopLine
  end
  object Bevel3: TBevel
    Left = 0
    Top = 344
    Width = 490
    Height = 8
    Align = alBottom
    Shape = bsBottomLine
  end
  object Panel1: TPanel
    Left = 112
    Top = 72
    Width = 153
    Height = 265
    BevelOuter = bvLowered
    TabOrder = 0
    object Image1: TImage
      Left = 1
      Top = 1
      Width = 151
      Height = 263
      Align = alClient
      Stretch = True
    end
  end
  object Button1: TButton
    Left = 272
    Top = 72
    Width = 105
    Height = 25
    Caption = 'Get Voxel Texture'
    TabOrder = 1
    Visible = False
    OnClick = Button1Click
  end
  object Button6: TButton
    Left = 272
    Top = 96
    Width = 105
    Height = 25
    Caption = 'Apply Texture'
    TabOrder = 2
    Visible = False
    OnClick = Button6Click
  end
  object Button2: TButton
    Left = 272
    Top = 120
    Width = 105
    Height = 25
    Caption = 'Load Texture'
    TabOrder = 3
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 272
    Top = 144
    Width = 105
    Height = 25
    Caption = 'Save Texture'
    TabOrder = 4
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 272
    Top = 176
    Width = 105
    Height = 25
    Caption = 'Save Palette'
    TabOrder = 5
    OnClick = Button4Click
  end
  object CheckBox1: TCheckBox
    Left = 272
    Top = 208
    Width = 105
    Height = 17
    Caption = 'Apply To Layers'
    TabOrder = 6
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 490
    Height = 58
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    TabOrder = 7
    object Image3: TImage
      Left = 0
      Top = 0
      Width = 490
      Height = 58
      Align = alClient
    end
    object Label2: TLabel
      Left = 24
      Top = 24
      Width = 206
      Height = 13
      Caption = 'Here you can apply a texture over the voxel'
      Transparent = True
    end
    object Label3: TLabel
      Left = 8
      Top = 8
      Width = 79
      Height = 13
      Caption = 'Voxel Texture'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      Transparent = True
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 352
    Width = 490
    Height = 46
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 8
    object Label1: TLabel
      Left = 32
      Top = 4
      Width = 265
      Height = 13
      Alignment = taCenter
      AutoSize = False
      Caption = 'Done'
      Visible = False
    end
    object Button8: TButton
      Left = 327
      Top = 11
      Width = 75
      Height = 23
      Caption = 'Ok'
      TabOrder = 0
      OnClick = Button8Click
    end
    object Button9: TButton
      Left = 408
      Top = 11
      Width = 75
      Height = 23
      Cancel = True
      Caption = 'Cancel'
      TabOrder = 1
      OnClick = Button9Click
    end
    object ProgressBar1: TProgressBar
      Left = 32
      Top = 17
      Width = 265
      Height = 16
      Min = 0
      Max = 100
      TabOrder = 2
      Visible = False
    end
  end
  object OpenPictureDialog1: TOpenPictureDialog
    Filter = 'Bitmaps (*.bmp)|*.bmp'
    Left = 272
    Top = 176
  end
  object SavePictureDialog1: TSavePictureDialog
    DefaultExt = 'bmp'
    Filter = 'Bitmaps (*.bmp)|*.bmp'
    Left = 304
    Top = 176
  end
end
