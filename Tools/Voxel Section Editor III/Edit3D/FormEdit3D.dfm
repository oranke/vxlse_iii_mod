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
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object CtrlPanel: TPanel
    Left = 0
    Top = 0
    Width = 493
    Height = 26
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 363
    object Bevel1: TBevel
      Left = 0
      Top = 0
      Width = 493
      Height = 2
      Align = alTop
      Shape = bsTopLine
      ExplicitWidth = 240
    end
  end
  object RenderPanel: TPanel
    Left = 0
    Top = 26
    Width = 493
    Height = 408
    Cursor = crCross
    Align = alClient
    BevelOuter = bvLowered
    Color = clBlack
    TabOrder = 1
    ExplicitWidth = 410
    ExplicitHeight = 301
    object RenderPaint: TPaintBox
      Left = 1
      Top = 1
      Width = 491
      Height = 406
      Align = alClient
      OnMouseDown = RenderPaintMouseDown
      OnMouseMove = RenderPaintMouseMove
      OnMouseUp = RenderPaintMouseUp
      ExplicitLeft = 120
      ExplicitTop = 136
      ExplicitWidth = 161
      ExplicitHeight = 137
    end
  end
end
