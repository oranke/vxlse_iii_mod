object Frm3DPReview: TFrm3DPReview
  Left = 723
  Top = 264
  BorderStyle = bsSizeToolWin
  Caption = 'OpenGL 3D Preview'
  ClientHeight = 232
  ClientWidth = 363
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnDeactivate = FormDeactivate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Panel2: TPanel
    Left = 0
    Top = 26
    Width = 363
    Height = 206
    Cursor = crCross
    Align = alClient
    BevelOuter = bvLowered
    Color = clBlack
    TabOrder = 0
    OnMouseDown = Panel2MouseDown
    OnMouseMove = Panel2MouseMove
    OnMouseUp = Panel2MouseUp
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 363
    Height = 26
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      363
      26)
    object SpeedButton2: TSpeedButton
      Left = 2
      Top = 3
      Width = 23
      Height = 22
      Hint = 'Reset Depth'
      Flat = True
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        1800000000000003000000000000000000000000000000000000FF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF808080FF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF80808000000000000000000000
        0000808080FF00FFFF00FF000000808080FF00FFFF00FFFF00FFFF00FFFF00FF
        0000000000000000000000000000000000000000000000008080800000000000
        00FF00FFFF00FFFF00FF808080000000000000808080FF00FFFF00FFFF00FFFF
        00FF808080000000000000000000000000808080FF00FFFF00FF808080000000
        808080FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF0000000000000000000000
        00000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FF808080000000000000000000000000000000808080FF00FFFF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF0000000000000000000000000000
        00000000000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF000000000000
        000000000000000000000000000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FF808080000000000000000000000000000000808080FF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF000000
        000000000000000000000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF8080
        80000000808080FF00FFFF00FF808080000000000000000000000000808080FF
        00FFFF00FFFF00FFFF00FF808080000000000000808080FF00FFFF00FFFF00FF
        0000000000008080800000000000000000000000000000000000000000000000
        00FF00FFFF00FFFF00FFFF00FFFF00FF808080000000FF00FFFF00FF80808000
        0000000000000000000000808080FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FF808080FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
      ParentShowHint = False
      ShowHint = True
      OnClick = SpeedButton2Click
    end
    object btn3DRotateX2: TSpeedButton
      Left = 48
      Top = 3
      Width = 24
      Height = 22
      Hint = 'Rotate Up'
      AllowAllUp = True
      GroupIndex = 20
      Flat = True
      Glyph.Data = {
        EE000000424DEE0000000000000076000000280000000D0000000F0000000100
        04000000000078000000C40E0000C40E00001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777778877777
        7000777770077777700077777800777770007777778087777000777777700777
        7000777777700777700077777770077770007777777007777000777087808777
        7000777000007777700077700008777770007770000008777000777000008777
        700077700087777770007770877777777000}
      ParentShowHint = False
      ShowHint = True
      OnClick = btn3DRotateX2Click
    end
    object btn3DRotateY2: TSpeedButton
      Left = 72
      Top = 3
      Width = 23
      Height = 22
      Hint = 'Rotate Left'
      AllowAllUp = True
      GroupIndex = 21
      Flat = True
      Glyph.Data = {
        EE000000424DEE0000000000000076000000280000000F0000000F0000000100
        04000000000078000000C40E0000C40E00001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
        7770777777777777777077777777777777707777777777777770777777777777
        7770000000077777777080000087777777707000007777778080780000877778
        0080770080000000077077807780000877707778777777777770777777777777
        777077777777777777707777777777777770}
      ParentShowHint = False
      ShowHint = True
      OnClick = btn3DRotateY2Click
    end
    object btn3DRotateY: TSpeedButton
      Left = 95
      Top = 3
      Width = 23
      Height = 22
      Hint = 'Rotate Right'
      AllowAllUp = True
      GroupIndex = 21
      Flat = True
      Glyph.Data = {
        EE000000424DEE0000000000000076000000280000000F0000000F0000000100
        04000000000078000000C40E0000C40E00001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
        7770777777777777777077777777777777707777777777787770777800008770
        8770770000000080077080087777800008708087777770000070777777778000
        0080777777770000000077777777777777707777777777777770777777777777
        777077777777777777707777777777777770}
      ParentShowHint = False
      ShowHint = True
      Transparent = False
      OnClick = btn3DRotateYClick
    end
    object Bevel1: TBevel
      Left = 0
      Top = 0
      Width = 363
      Height = 2
      Align = alTop
      Shape = bsTopLine
      ExplicitWidth = 240
    end
    object SpeedButton1: TSpeedButton
      Left = 118
      Top = 3
      Width = 23
      Height = 22
      Hint = 'Views'
      Flat = True
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        18000000000000030000130B0000130B000000000000000000006C63634C4747
        413C3C403B3B4C47473B3636524D4D4540402924242924242924242924242924
        24292424292424453C3C5854542727271919190808081A1A1A00000000000000
        00000000000000000000000000000000000000000000002721213B3636000000
        0000000000000000000202031F1F1F0808080000000000000000000000000000
        000000000000002721213B363600000000000005050503033E1F1F5A30303011
        11110202020000000000000000000000000000000000002721213B3636000000
        00000030303049495E3C3C3C3434342929490202400A0A0A0101010000000000
        000000000000002721213B36360000000000001C1C1C31313106060D00000018
        181E3D3D652A2A2A0F0F0F0000000000000000000000002721213B3636000000
        0000000000000000000000002424245959632E2E5D2323270B0B0B0000000000
        000000000000002721213B36360000000000000000000000000000000C0C0C12
        123D0F0F4800000D0000000000000000000000000000002721213B3636000000
        0000000000000000000000000000000000001111110000000000000000000000
        000000000000002721213B363600000000000000000000000000000000000000
        00000404040000000000000000000000000000000000002721213B3636000000
        0000000000000000000000000000000000000000000000000000000000000000
        000000000000002721218D83836B64645D57575F58584A43434A43434A434349
        43434741414540404A43434A43434A43434A43434A4343625757CBB9B9A29494
        B1A1A1B9A8A8A29393C0AFAFA69797946B5FB59E9AAA9A9ACAB7B7CAB7B7CAB7
        B7CAB7B7CAB7B7C6B1B1C7B4B4BAA8A8C3B0B0B6A4A4B7A5A5C1AEAEC8B4B4BC
        A8A8BBA8A8BFACACC8B4B4C8B4B4C8B4B4C8B4B4C8B4B4C4AFAFB19792A58A84
        B49790AC938EAD928CBBA19ABFA39BC3A7A0C5ABA3C6ABA3C9B0A8CAAFA7CBB0
        A7CDB3AACAB2AABFA8A3B08D83A88072A87E6FB59183B38D7DB89383BB9786C0
        9C8BC6A594C9A897CDAC99CEAB96CFA991D5B198D4B5A2B49E97}
      ParentShowHint = False
      PopupMenu = Popup3d
      ShowHint = True
      OnMouseUp = SpeedButton1MouseUp
    end
    object btn3DRotateX: TSpeedButton
      Left = 25
      Top = 3
      Width = 23
      Height = 22
      Hint = 'Rotate Down'
      AllowAllUp = True
      GroupIndex = 20
      Flat = True
      Glyph.Data = {
        EE000000424DEE0000000000000076000000280000000D0000000F0000000100
        04000000000078000000C40E0000C40E00001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777778077
        7000777777800077700077778000007770007778000000777000777778000077
        7000777770000077700077778087807770007777007777777000777700777777
        7000777700777777700077770077777770007777808777777000777770087777
        700077777700777770007777778877777000}
      ParentShowHint = False
      ShowHint = True
      Transparent = False
      OnClick = btn3DRotateXClick
    end
    object SpPlay: TSpeedButton
      Left = 225
      Top = 3
      Width = 23
      Height = 22
      Anchors = [akTop, akRight]
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000130B0000130B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        33333333333333333333333333333333333333FFFFFFFFFFFFF3300000000000
        003337777777777777F330F777777777703337F33333333337F330F333333333
        703337F3333F333337F330F333033333703337F3337FF33337F330F333003333
        703337F33377FF3337F330F333000333703337F333777FF337F330F333000033
        703337F33377773337F330F333000333703337F33377733337F330F333003333
        703337F33377333337F330F333033333703337F33373333337F330F333333333
        703337F33333333337F330FFFFFFFFFFF03337FFFFFFFFFFF7F3300000000000
        0033377777777777773333333333333333333333333333333333}
      NumGlyphs = 2
      OnClick = SpPlayClick
    end
    object SpStop: TSpeedButton
      Left = 248
      Top = 3
      Width = 23
      Height = 22
      Anchors = [akTop, akRight]
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000130B0000130B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        33333333333333333333333333333333333333FFFFFFFFFFFFF3300000000000
        003337777777777777F330F777777777703337F33333333337F330F333333333
        703337F33333333337F330F333333333703337F333FFFFF337F330F330000033
        703337F3377777F337F330F330000033703337F3377777F337F330F330000033
        703337F3377777F337F330F330000033703337F3377777F337F330F330000033
        703337F33777773337F330F333333333703337F33333333337F330F333333333
        703337F33333333337F330FFFFFFFFFFF03337FFFFFFFFFFF7F3300000000000
        0033377777777777773333333333333333333333333333333333}
      NumGlyphs = 2
      OnClick = SpStopClick
    end
    object Label1: TLabel
      Left = 277
      Top = 8
      Width = 32
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Frame:'
    end
    object spin3Djmp: TSpinEdit
      Left = 144
      Top = 3
      Width = 37
      Height = 22
      Hint = 'Rotate Step Size'
      Color = clBtnFace
      MaxLength = 2
      MaxValue = 99
      MinValue = 1
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      Value = 10
      OnChange = spin3DjmpChange
    end
    object SpFrame: TSpinEdit
      Left = 315
      Top = 3
      Width = 50
      Height = 22
      Anchors = [akTop, akRight]
      MaxLength = 1
      MaxValue = 1
      MinValue = 1
      TabOrder = 1
      Value = 1
      OnChange = SpFrameChange
    end
  end
  object MainMenu1: TMainMenu
    Left = 144
    Top = 40
    object File1: TMenuItem
      Caption = 'File'
      object Exit1: TMenuItem
        Caption = 'Exit'
        OnClick = Exit1Click
      end
    end
    object Options1: TMenuItem
      Caption = 'Options'
      object BackgroundColour1: TMenuItem
        Caption = 'Background Colour'
        OnClick = BackgroundColour1Click
      end
      object FontColor1: TMenuItem
        Caption = 'Font Color'
        OnClick = FontColor1Click
      end
    end
    object ools1: TMenuItem
      Caption = 'Tools'
      object akeScreenshotBMP1: TMenuItem
        Caption = 'Take Screenshot (BMP)'
        OnClick = akeScreenshotBMP1Click
      end
      object akeScreenshotJPG1: TMenuItem
        Caption = 'Take Screenshot (JPG)'
        OnClick = akeScreenshotJPG1Click
      end
      object akeScreenshotPNG1: TMenuItem
        Caption = 'Take Screenshot (PNG)'
        OnClick = akeScreenshotPNG1Click
      end
      object akeScreenshot1: TMenuItem
        Caption = 'Take Screenshot (TGA)'
        OnClick = akeScreenshot1Click
      end
      object ake360DegScreenshots1: TMenuItem
        Caption = 'Make 360 Deg Animation (GIF)'
        OnClick = ake360DegScreenshots1Click
      end
    end
  end
  object ColorDialog1: TColorDialog
    Left = 176
    Top = 40
  end
  object Popup3d: TPopupMenu
    AutoPopup = False
    TrackButton = tbLeftButton
    Left = 144
    Top = 72
    object Views1: TMenuItem
      Caption = 'Views'
      object Front1: TMenuItem
        Caption = 'Front'
        OnClick = Front1Click
      end
      object Back1: TMenuItem
        Caption = 'Back'
        OnClick = Back1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object LEft1: TMenuItem
        Caption = 'Left'
        OnClick = LEft1Click
      end
      object Right1: TMenuItem
        Caption = 'Right'
        OnClick = Right1Click
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Bottom1: TMenuItem
        Caption = 'Bottom'
        OnClick = Bottom1Click
      end
      object op1: TMenuItem
        Caption = 'Top'
        OnClick = op1Click
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object Cameo1: TMenuItem
        Caption = 'Cameo'
        OnClick = Cameo1Click
      end
      object Cameo21: TMenuItem
        Caption = 'Cameo2'
        OnClick = Cameo21Click
      end
      object Cameo31: TMenuItem
        Caption = 'Cameo3'
        OnClick = Cameo31Click
      end
      object Cameo41: TMenuItem
        Caption = 'Cameo4'
        OnClick = Cameo41Click
      end
    end
    object Display1: TMenuItem
      Caption = 'Display'
      object CurrentSectionOnly1: TMenuItem
        Caption = 'Current Section Only'
        Checked = True
        OnClick = CurrentSectionOnly1Click
      end
      object WholeVoxel1: TMenuItem
        Caption = 'Whole Voxel'
        OnClick = CurrentSectionOnly1Click
      end
    end
    object RemapColour1: TMenuItem
      Caption = 'Remap Colour'
      object Gold1: TMenuItem
        Caption = 'Gold'
        OnClick = Gold1Click
      end
      object Red1: TMenuItem
        Caption = 'DarkRed'
        Checked = True
        OnClick = Red1Click
      end
      object Orange1: TMenuItem
        Caption = 'Orange'
        OnClick = Orange1Click
      end
      object Magenta1: TMenuItem
        Caption = 'Magenta'
        OnClick = Magenta1Click
      end
      object Purple1: TMenuItem
        Caption = 'Purple'
        OnClick = Purple1Click
      end
      object Blue1: TMenuItem
        Caption = 'DarkBlue'
        OnClick = Blue1Click
      end
      object Green1: TMenuItem
        Caption = 'DarkGreen'
        OnClick = Green1Click
      end
      object DarkSky1: TMenuItem
        Caption = 'DarkSky'
        OnClick = DarkSky1Click
      end
      object White1: TMenuItem
        Caption = 'White'
        Visible = False
        OnClick = White1Click
      end
    end
  end
  object AnimationTimer: TTimer
    Enabled = False
    Interval = 100
    OnTimer = AnimationTimerTimer
    Left = 176
    Top = 72
  end
end
