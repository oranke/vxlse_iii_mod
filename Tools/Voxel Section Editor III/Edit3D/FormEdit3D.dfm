object FrmEdit3D: TFrmEdit3D
  Left = 0
  Top = 0
  Caption = 'Edit 3D (by oranke)'
  ClientHeight = 434
  ClientWidth = 493
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnActivate = FormActivate
  OnClose = FormClose
  OnKeyDown = FormKeyDown
  OnMouseWheel = FormMouseWheel
  PixelsPerInch = 96
  TextHeight = 13
  object CtrlPanel: TPanel
    Left = 0
    Top = 0
    Width = 493
    Height = 27
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Bevel1: TBevel
      Left = 0
      Top = 0
      Width = 493
      Height = 2
      Align = alTop
      Shape = bsTopLine
      ExplicitWidth = 240
    end
    object UpsideMenuBtn: TSpeedButton
      Tag = 2
      Left = 27
      Top = 3
      Width = 23
      Height = 22
      Hint = 'change Upper side'
      Flat = True
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        1800000000000003000000000000000000000000000000000000FF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FF000000FF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FF000000000000FF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FFFF00FF000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF000000FF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FF0000000000000000000000000000000000000000
        00000000000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF000000FF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FF000000FF00FFFF00FFFF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FF000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF000000FF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FF000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF000000FF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FF000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF000000000000FF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        FF00FFFF00FFFF00FFFF00FF000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
        00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
      ParentShowHint = False
      ShowHint = True
      OnClick = UpsideMenuBtnClick
    end
    object ViewButton: TSpeedButton
      Tag = 1
      Left = 55
      Top = 3
      Width = 23
      Height = 22
      Hint = '1: View'
      AllowAllUp = True
      GroupIndex = 1
      Down = True
      Caption = 'V'
      Flat = True
      ParentShowHint = False
      ShowHint = True
    end
    object AddButton: TSpeedButton
      Tag = 1
      Left = 78
      Top = 3
      Width = 23
      Height = 22
      Hint = '2: Add voxel'
      AllowAllUp = True
      GroupIndex = 1
      Caption = 'A'
      Flat = True
      ParentShowHint = False
      ShowHint = True
    end
    object DelButton: TSpeedButton
      Tag = 1
      Left = 101
      Top = 3
      Width = 23
      Height = 22
      Hint = '3: Delete voxel'
      AllowAllUp = True
      GroupIndex = 1
      Caption = 'D'
      Flat = True
      ParentShowHint = False
      ShowHint = True
    end
    object RepaintButton: TSpeedButton
      Tag = 1
      Left = 124
      Top = 3
      Width = 23
      Height = 22
      Hint = '4: Repaint voxel'
      AllowAllUp = True
      GroupIndex = 1
      Caption = 'R'
      Flat = True
      ParentShowHint = False
      ShowHint = True
    end
    object ResetViewButton: TSpeedButton
      Left = 2
      Top = 3
      Width = 23
      Height = 22
      Hint = 'Reset View'
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
      OnClick = ResetViewButtonClick
    end
    object LinkXBtn: TSpeedButton
      Tag = 1
      Left = 173
      Top = 3
      Width = 23
      Height = 22
      Hint = 'X Axis Linkage'
      AllowAllUp = True
      GroupIndex = 2
      Caption = 'Lx'
      Flat = True
      ParentShowHint = False
      ShowHint = True
    end
    object LinkYBtn: TSpeedButton
      Tag = 1
      Left = 196
      Top = 3
      Width = 23
      Height = 22
      Hint = 'Y Axis Linkage'
      AllowAllUp = True
      GroupIndex = 3
      Caption = 'Ly'
      Flat = True
      ParentShowHint = False
      ShowHint = True
    end
    object LinkZBtn: TSpeedButton
      Tag = 1
      Left = 219
      Top = 3
      Width = 23
      Height = 22
      Hint = 'Z Axis Linkage'
      AllowAllUp = True
      GroupIndex = 4
      Caption = 'Lz'
      Flat = True
      ParentShowHint = False
      ShowHint = True
    end
  end
  object RenderPanel: TPanel
    Left = 0
    Top = 27
    Width = 493
    Height = 407
    Align = alClient
    BevelOuter = bvNone
    Color = clBlack
    TabOrder = 1
    OnMouseDown = RenderPaintMouseDown
    OnMouseMove = RenderPaintMouseMove
    OnMouseUp = RenderPaintMouseUp
    OnResize = RenderPanelResize
  end
  object UpsidePopup: TPopupMenu
    Left = 8
    Top = 32
    object UpsideMenuX: TMenuItem
      Caption = 'X'
      GroupIndex = 1
      RadioItem = True
      OnClick = UpsideMenuClick
    end
    object UpsideMenuY: TMenuItem
      Tag = 1
      Caption = 'Y'
      GroupIndex = 1
      RadioItem = True
      OnClick = UpsideMenuClick
    end
    object UpsideMenuZ: TMenuItem
      Tag = 2
      Caption = 'Z'
      Checked = True
      GroupIndex = 1
      RadioItem = True
      OnClick = UpsideMenuClick
    end
  end
end
