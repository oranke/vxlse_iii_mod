object FrmAutoNormals: TFrmAutoNormals
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Auto Normals 7.0'
  ClientHeight = 557
  ClientWidth = 378
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  DesignSize = (
    378
    557)
  PixelsPerInch = 96
  TextHeight = 13
  object Label4: TLabel
    Left = 8
    Top = 8
    Width = 361
    Height = 41
    AutoSize = False
    Caption = 
      'Welcome to the Voxel Section Editor III Auto Normalizer. This pr' +
      'ocess will modify the normals from your voxel. If you choose to ' +
      'do this, we recommend you to save your model under a different n' +
      'ame as a backup.'
    WordWrap = True
  end
  object Label5: TLabel
    Left = 8
    Top = 64
    Width = 361
    Height = 57
    AutoSize = False
    Caption = 
      'Normal is an unitary vector that express the direction where the' +
      ' light relfects when it hits the pixel. If you don'#39't understand ' +
      'physics, just understand that good normals will make your model ' +
      'look more curved and less plain, making it look a lot cooler.'
    WordWrap = True
  end
  object Label6: TLabel
    Left = 8
    Top = 136
    Width = 361
    Height = 65
    AutoSize = False
    Caption = 
      'The Auto Normalizer is still on experimental stages, although it' +
      ' already provides excelent results. For this reason, we allow yo' +
      'u to choose one of the normalization methods used during the dev' +
      'elopment of the program. At this moment, we recommend everyone t' +
      'o use Tangent Plane Normalizer, with RA2 Range.'
    WordWrap = True
  end
  object GbNormalizationMethod: TGroupBox
    Left = 8
    Top = 216
    Width = 363
    Height = 121
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Normalization Method:'
    TabOrder = 0
    object RbInfluence: TRadioButton
      Left = 16
      Top = 48
      Width = 321
      Height = 17
      Hint = 
        'This is the latest Normalizer. Influence Normalizer is builds a ' +
        'reference map based on the influence of a region (how far it is ' +
        'inside the model). Then, it normalizes it and in the end, it smo' +
        'oths it. The range for the normalization and smooth can be defin' +
        'ed by the user. The higher the range, the smoother the result...' +
        ' however, it will kill your processor, memory and take longer. V' +
        'alues higher than 3 will use too much processor and will hardly ' +
        'do any difference at all.'
      Caption = 'Influence AutoNormals (Auto Normals v7.0, recommended)'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnClick = RbInfluenceClick
    end
    object RbCubed: TRadioButton
      Left = 16
      Top = 72
      Width = 289
      Height = 17
      Hint = 
        'Smoothed Cubed Normalizer works in the following way: first it r' +
        'ecognizes which pixels are inside the model. Then, it builds a r' +
        'eference map based on influences (how much is inside the structu' +
        're) for those pixels that are not inside the model. Then, it nor' +
        'malizes it and smooth it in the end. The results are often good ' +
        'and it distuingish well the borders of the model. The range for ' +
        'the normalization and smooth can be defined by the user. The hig' +
        'her the range, the smoother the result... however, it will kill ' +
        'your processor, memory and take longer. Values higher than 3 wil' +
        'l use too much processor and will hardly do any difference at al' +
        'l.'
      Caption = 'Cubed AutoNormals (Auto Normals v5.5)'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = RbCubedClick
    end
    object Rb6Faced: TRadioButton
      Left = 16
      Top = 96
      Width = 289
      Height = 17
      Hint = 
        'This is the very old normalizer available prior to the 1.3x vers' +
        'ions. It normalizes each pixel based on the 6 neighboor faces. T' +
        'he results are often poor, but good for those who want rigid mod' +
        'els or wants almost no curves at all.'
      Caption = '6-Faced AutoNormals (Auto Normals v1.1)'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = Rb6FacedClick
    end
    object RbTangent: TRadioButton
      Left = 16
      Top = 24
      Width = 313
      Height = 17
      Caption = 'Tangent Plane AutoNormals (Auto Normals 8.0 Beta)'
      Checked = True
      TabOrder = 3
      TabStop = True
      OnClick = RbTangentClick
    end
  end
  object GbInfluenceOptions: TGroupBox
    Left = 8
    Top = 350
    Width = 363
    Height = 170
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Influence Normalizer Options...'
    TabOrder = 1
    object Label1: TLabel
      Left = 8
      Top = 24
      Width = 102
      Height = 13
      Caption = 'Normalization Range:'
    end
    object Label2: TLabel
      Left = 8
      Top = 139
      Width = 64
      Height = 13
      Caption = 'Range Value:'
    end
    object Label3: TLabel
      Left = 200
      Top = 24
      Width = 72
      Height = 13
      Caption = 'Other Options:'
    end
    object Label7: TLabel
      Left = 200
      Top = 140
      Width = 77
      Height = 13
      Caption = 'Contrast Level: '
    end
    object Label8: TLabel
      Left = 200
      Top = 116
      Width = 71
      Height = 13
      Caption = 'Smooth Level: '
    end
    object LbRange: TListBox
      Left = 8
      Top = 40
      Width = 169
      Height = 89
      ItemHeight = 13
      Items.Strings = (
        '1.7 (TS Range)'
        '3.54 (RA2 Range)'
        'Other (Smoother, But CPU Killer)')
      TabOrder = 0
      OnClick = LbRangeClick
    end
    object EdRange: TEdit
      Left = 80
      Top = 136
      Width = 97
      Height = 21
      Hint = 
        'This determines the range for normalization and smooth procedure' +
        's from the Influence Normalizer. The higher the value, the smoot' +
        'her the result, however, it uses more cpu and memory. If you set' +
        ' a value like 100, your normalization will take weeks. So, we re' +
        'commend a value smaller than 6. Actually, 6 is already too much.' +
        ' Remember, the value must be integer and positive. Zero is not a' +
        'ccepted.'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      Text = '3'
      OnChange = EdRangeChange
    end
    object CbSmoothMe: TCheckBox
      Left = 200
      Top = 40
      Width = 137
      Height = 17
      Hint = 
        'Check this option if you want the normals of your model to be sm' +
        'oothed during the normalization. Note that the smooth operation ' +
        'during the normalization is far more efficient than the smooth d' +
        'one after it. (Recommended)'
      Caption = 'Smooth My Normals'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
    end
    object CbInfluenceMap: TCheckBox
      Left = 200
      Top = 56
      Width = 137
      Height = 17
      Hint = 
        'This option lets the Smooth Cubed Normalizer build an influence ' +
        'map that will avoid painted pixels that are isolated from the ma' +
        'in shape from picking wrong normal values. However, this has a l' +
        'ittle processor cost, not worth only if you don'#39't have any one p' +
        'ixel wide walls. (Recommended)'
      Caption = 'Build Influence Map'
      Checked = True
      ParentShowHint = False
      ShowHint = True
      State = cbChecked
      TabOrder = 3
    end
    object CbNewPixelsOnly: TCheckBox
      Left = 200
      Top = 72
      Width = 153
      Height = 17
      Caption = 'Normalize only new pixels'
      TabOrder = 4
    end
    object CbIncreaseContrast: TCheckBox
      Left = 200
      Top = 88
      Width = 137
      Height = 17
      Caption = 'Stretch Influence Map'
      Checked = True
      State = cbChecked
      TabOrder = 5
    end
    object EdContrast: TEdit
      Left = 288
      Top = 136
      Width = 65
      Height = 21
      TabOrder = 6
      Text = '1'
    end
    object EdSmooth: TEdit
      Left = 288
      Top = 112
      Width = 65
      Height = 21
      TabOrder = 7
      Text = '1'
    end
  end
  object BtOK: TButton
    Left = 216
    Top = 528
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    TabOrder = 2
    OnClick = BtOKClick
  end
  object BtCancel: TButton
    Left = 298
    Top = 528
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    TabOrder = 3
    OnClick = BtCancelClick
  end
  object BtTips: TButton
    Left = 8
    Top = 528
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Tips'
    TabOrder = 4
    OnClick = BtTipsClick
  end
end
