object FrmPreferences: TFrmPreferences
  Left = 192
  Top = 107
  BorderIcons = []
  BorderStyle = bsToolWindow
  Caption = ' '
  ClientHeight = 431
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
    Top = 377
    Width = 490
    Height = 8
    Align = alBottom
    Shape = bsBottomLine
  end
  object GroupBox1: TGroupBox
    Left = 176
    Top = 88
    Width = 297
    Height = 265
    Caption = 'File Assosiation'
    TabOrder = 0
    object PageControl1: TPageControl
      Left = 8
      Top = 16
      Width = 273
      Height = 241
      ActivePage = TabSheet1
      Style = tsFlatButtons
      TabOrder = 0
      object TabSheet1: TTabSheet
        Caption = 'File_assosiation_tab'
        TabVisible = False
        object AssociateCheck: TCheckBox
          Left = 0
          Top = 8
          Width = 209
          Height = 17
          Caption = 'Associate *.vxl files with Voxel Editor III'
          TabOrder = 0
        end
        object GroupBox3: TGroupBox
          Left = 0
          Top = 32
          Width = 113
          Height = 89
          Caption = 'Icon'
          TabOrder = 1
          object IconPrev: TImage
            Left = 40
            Top = 16
            Width = 57
            Height = 57
            Center = True
          end
          object IconID: TTrackBar
            Left = 8
            Top = 16
            Width = 25
            Height = 65
            Max = 3
            Orientation = trVertical
            PageSize = 1
            TabOrder = 0
            OnChange = IconIDChange
          end
        end
        object BtnApply: TButton
          Left = 184
          Top = 208
          Width = 81
          Height = 22
          Caption = '&Apply'
          TabOrder = 2
          Visible = False
          OnClick = BtnApplyClick
        end
      end
      object TabSheet2: TTabSheet
        Caption = 'Palette_tab'
        ImageIndex = 1
        TabVisible = False
        object Label1: TLabel
          Left = 24
          Top = 24
          Width = 184
          Height = 13
          Caption = 'Select a palette to use for Tiberian Sun'
          Enabled = False
        end
        object Label2: TLabel
          Left = 24
          Top = 72
          Width = 177
          Height = 13
          Caption = 'Select a palette to use for Red Alert 2'
          Enabled = False
        end
        object CheckBox1: TCheckBox
          Left = 0
          Top = 0
          Width = 177
          Height = 17
          Caption = 'Use game specific palettes'
          TabOrder = 0
          OnClick = CheckBox1Click
        end
        object ComboBox2: TComboBoxEx
          Left = 32
          Top = 88
          Width = 177
          Height = 22
          ItemsEx = <>
          Enabled = False
          ItemHeight = 16
          TabOrder = 1
          Images = FrmMain.ImageList1
          DropDownCount = 8
        end
        object ComboBox1: TComboBoxEx
          Left = 32
          Top = 40
          Width = 177
          Height = 22
          ItemsEx = <>
          Enabled = False
          ItemHeight = 16
          TabOrder = 2
          Images = FrmMain.ImageList1
          DropDownCount = 8
        end
      end
    end
  end
  object Pref_List: TTreeView
    Left = 16
    Top = 80
    Width = 145
    Height = 281
    Indent = 19
    TabOrder = 1
    OnClick = Pref_ListClick
    OnKeyDown = Pref_ListKeyDown
    OnKeyPress = Pref_ListKeyPress
    OnKeyUp = Pref_ListKeyUp
    Items.NodeData = {
      01020000003B0000000000000001000000FFFFFFFFFFFFFFFF00000000000000
      0011460069006C00650020004100730073006F00630069006100740069006F00
      6E007300270000000000000000000000FFFFFFFFFFFFFFFF0000000000000000
      07500061006C006500740074006500}
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 490
    Height = 58
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    TabOrder = 2
    object Image1: TImage
      Left = 0
      Top = 0
      Width = 490
      Height = 58
      Align = alClient
    end
    object Label9: TLabel
      Left = 24
      Top = 24
      Width = 209
      Height = 13
      Caption = 'Here you can set the settings of the program'
      Transparent = True
    end
    object Label10: TLabel
      Left = 8
      Top = 8
      Width = 69
      Height = 13
      Caption = 'Preferences'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      Transparent = True
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 385
    Width = 490
    Height = 46
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 3
    object Button4: TButton
      Left = 327
      Top = 11
      Width = 75
      Height = 23
      Caption = 'Ok'
      TabOrder = 0
      OnClick = Button4Click
    end
    object Button1: TButton
      Left = 408
      Top = 11
      Width = 75
      Height = 23
      Cancel = True
      Caption = 'Cancel'
      TabOrder = 1
      OnClick = Button2Click
    end
  end
end
