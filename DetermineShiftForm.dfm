object frmDetermineShift: TfrmDetermineShift
  Left = 246
  Top = 92
  BorderStyle = bsDialog
  Caption = 'Determine timestamp shift'
  ClientHeight = 307
  ClientWidth = 432
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -14
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 17
  object lblDescr: TLabel
    Left = 8
    Top = 0
    Width = 416
    Height = 73
    AutoSize = False
    WordWrap = True
  end
  object Label2: TLabel
    Left = 8
    Top = 182
    Width = 183
    Height = 17
    Caption = 'Date/time that it should have:'
  end
  object Label3: TLabel
    Left = 8
    Top = 232
    Width = 68
    Height = 17
    Caption = 'Shift value:'
  end
  object Label4: TLabel
    Left = 218
    Top = 232
    Width = 48
    Height = 17
    Caption = 'minutes'
  end
  object Label5: TLabel
    Left = 8
    Top = 151
    Width = 140
    Height = 17
    Caption = 'EXIF date/time original:'
  end
  object lblDT: TLabel
    Left = 203
    Top = 151
    Width = 4
    Height = 17
  end
  object Button1: TButton
    Left = 204
    Top = 263
    Width = 98
    Height = 33
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Caption = 'OK'
    Default = True
    Enabled = False
    ModalResult = 1
    TabOrder = 0
  end
  object Button2: TButton
    Left = 310
    Top = 263
    Width = 98
    Height = 33
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object panJPEGFile: TPanel
    Left = 8
    Top = 79
    Width = 105
    Height = 57
    BevelInner = bvLowered
    Caption = 'Drop JPG here'
    TabOrder = 2
  end
  object dtpDate: TDateTimePicker
    Left = 203
    Top = 174
    Width = 97
    Height = 25
    Date = 0.440614537037618000
    Time = 0.440614537037618000
    TabOrder = 3
    OnChange = dtpDateChange
  end
  object dtpTime: TDateTimePicker
    Left = 310
    Top = 174
    Width = 90
    Height = 25
    Date = 41555.440819398150000000
    Time = 41555.440819398150000000
    Kind = dtkTime
    TabOrder = 4
    OnChange = dtpDateChange
  end
  object eShift: TEdit
    Left = 82
    Top = 229
    Width = 130
    Height = 25
    ReadOnly = True
    TabOrder = 5
  end
end
