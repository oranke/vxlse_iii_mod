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
  OnCreate = FormCreate
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
    Align = alClient
    BevelOuter = bvNone
    Color = clBlack
    TabOrder = 1
    OnMouseDown = RenderPaintMouseDown
    OnMouseMove = RenderPaintMouseMove
    OnMouseUp = RenderPaintMouseUp
    OnResize = RenderPanelResize
  end
end
