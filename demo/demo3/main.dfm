object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 303
  ClientWidth = 570
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 120
    Top = 44
    Width = 24
    Height = 13
    Caption = 'Style'
  end
  object PraButtonStyle: TPraButtonStyle
    Left = 248
    Top = 188
    Width = 97
    Height = 37
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    BrushDown.Color = 15461355
    BrushFocused.Color = 15461355
    BrushDisabled.Color = 15461355
    FontDown.Charset = DEFAULT_CHARSET
    FontDown.Color = clWindowText
    FontDown.Height = -11
    FontDown.Name = 'Tahoma'
    FontDown.Style = []
    FontFocused.Charset = DEFAULT_CHARSET
    FontFocused.Color = clWindowText
    FontFocused.Height = -11
    FontFocused.Name = 'Tahoma'
    FontFocused.Style = []
    FontDisabled.Charset = DEFAULT_CHARSET
    FontDisabled.Color = clWindowText
    FontDisabled.Height = -11
    FontDisabled.Name = 'Tahoma'
    FontDisabled.Style = []
    Caption = ''
    TabOrder = 2
    Shape = stRectangle
  end
  object Label2: TLabel
    Left = 120
    Top = 87
    Width = 30
    Height = 13
    Caption = 'Shape'
  end
  object CheckBox1: TCheckBox
    Left = 392
    Top = 112
    Width = 89
    Height = 17
    Caption = 'Outlinebutton'
    TabOrder = 0
    OnClick = CheckBox1Click
  end
  object ComboBox1: TComboBox
    Left = 120
    Top = 60
    Width = 145
    Height = 22
    Style = csOwnerDrawFixed
    TabOrder = 1
    OnSelect = ComboBox1Select
    Items.Strings = (
      ''
      'Primary'
      'Secondary'
      'Success'
      'Danger'
      'Warning'
      'Info'
      'Light'
      'Dark')
  end
  object CheckBox2: TCheckBox
    Left = 392
    Top = 89
    Width = 97
    Height = 17
    Caption = 'Enabled'
    Checked = True
    State = cbChecked
    TabOrder = 3
    OnClick = CheckBox2Click
  end
  object ComboBox2: TComboBox
    Left = 120
    Top = 103
    Width = 145
    Height = 22
    Style = csOwnerDrawFixed
    ItemIndex = 1
    TabOrder = 4
    Text = 'stRectangle'
    OnSelect = ComboBox2Select
    Items.Strings = (
      'stRoundRect'
      'stRectangle')
  end
end
