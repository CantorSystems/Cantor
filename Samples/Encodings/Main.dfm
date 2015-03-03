object MainForm: TMainForm
  Left = 202
  Top = 107
  AutoScroll = False
  Caption = 'CoreLite Encodings'
  ClientHeight = 453
  ClientWidth = 632
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel: TPanel
    Left = 0
    Top = 0
    Width = 632
    Height = 77
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object btLoad: TButton
      Left = 8
      Top = 40
      Width = 89
      Height = 24
      Caption = 'Load file...'
      TabOrder = 0
      OnClick = LoadFile
    end
    object btANSI: TButton
      Left = 148
      Top = 8
      Width = 89
      Height = 24
      Caption = 'Save ANSI...'
      TabOrder = 1
      OnClick = SaveLegacyChar
    end
    object btOEM: TButton
      Tag = 1
      Left = 148
      Top = 40
      Width = 89
      Height = 24
      Caption = 'Save OEM...'
      TabOrder = 2
      OnClick = SaveLegacyChar
    end
    object btUTF7: TButton
      Tag = 65000
      Left = 260
      Top = 8
      Width = 89
      Height = 24
      Caption = 'Save UTF-7...'
      TabOrder = 3
      OnClick = SaveLegacyChar
    end
    object btUTF16LE: TButton
      Left = 372
      Top = 8
      Width = 169
      Height = 24
      Caption = 'Save UTF-16 Little Endian...'
      TabOrder = 5
      OnClick = SaveUTF16
    end
    object btUTF8: TButton
      Tag = 65001
      Left = 260
      Top = 40
      Width = 89
      Height = 24
      Caption = 'Save UTF-8...'
      TabOrder = 4
      OnClick = SaveLegacyChar
    end
    object btUTF16BE: TButton
      Tag = 1
      Left = 372
      Top = 40
      Width = 169
      Height = 24
      Caption = 'Save UTF-16 Big Endian...'
      TabOrder = 6
      OnClick = SaveUTF16
    end
    object cbOEM: TCheckBox
      Left = 8
      Top = 8
      Width = 105
      Height = 24
      Caption = 'OEM code page'
      TabOrder = 7
      OnClick = cbOEMClick
    end
  end
  object Memo: TMemo
    Left = 0
    Top = 77
    Width = 632
    Height = 357
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 434
    Width = 632
    Height = 19
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBtnText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Panels = <
      item
        Width = 100
      end
      item
        Width = 100
      end
      item
        Width = 50
      end>
    SimplePanel = False
    UseSystemFont = False
  end
  object OpenDialog: TOpenDialog
    Filter = 'Text files|*.txt|All files|*.*'
    InitialDir = '.'
    Left = 20
    Top = 104
  end
  object SaveDialog: TSaveDialog
    DefaultExt = '.txt'
    Filter = 'Text files|*.txt|All files|*.*'
    InitialDir = '.'
    Left = 56
    Top = 104
  end
end
