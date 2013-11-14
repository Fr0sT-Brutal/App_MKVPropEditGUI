{**************************************************************************************}
{                                                                                      }
{ CCR Exif - Delphi class library for reading and writing Exif metadata in JPEG files  }
{ Version 1.5.1                                                                        }
{                                                                                      }
{ The contents of this file are subject to the Mozilla Public License Version 1.1      }
{ (the "License"); you may not use this file except in compliance with the License.    }
{ You may obtain a copy of the License at http://www.mozilla.org/MPL/                  }
{                                                                                      }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT   }
{ WARRANTY OF ANY KIND, either express or implied. See the License for the specific    }
{ language governing rights and limitations under the License.                         }
{                                                                                      }
{ The Original Code is FileTimeOptsForm.pas.                                           }
{                                                                                      }
{ The Initial Developer of the Original Code is Chris Rolliston. Portions created by   }
{ Chris Rolliston are Copyright (C) 2009-2012 Chris Rolliston. All Rights Reserved.    }
{                                                                                      }
{**************************************************************************************}

unit DetermineShiftForm;

interface

uses
  {$IF RTLVersion >= 23}System.UITypes,{$IFEND} Messages, Windows, ShellApi, DateUtils,
  SysUtils, Classes, Controls, Forms, StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls, Dialogs,
  CCR.Exif.BaseUtils, CCR.Exif;

type

  TPanel = class(Vcl.ExtCtrls.TPanel) //OK, using an interposer class is a bit lazy,
  protected                           //but this isn't a big app...
    procedure WMDropFiles(var Message: TWMDropFiles); message WM_DROPFILES;
  end;

  TfrmDetermineShift = class(TForm)
    Button1: TButton;
    Button2: TButton;
    lblDescr: TLabel;
    panJPEGFile: TPanel;
    Label2: TLabel;
    dtpDate: TDateTimePicker;
    dtpTime: TDateTimePicker;
    Label3: TLabel;
    eShift: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    lblDT: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure dtpDateChange(Sender: TObject);
  strict private // internal stuff
    FExifDT: TDateTime;
    FShiftMins: Integer;
    procedure CalcShift;
  private // access from TPanel
    procedure SetExifDT(dt: TDateTime);
  public  // access from other unit
    property ShiftMins: Integer read FShiftMins;
  end;

implementation

{$R *.dfm}

uses ExifTimeEditForm;

const
  SDescr = 'This tool helps you to calculate time shift value between EXIF timestamp of a file '+
           'and a timestamp you input. Useful when these timestamps differ significantly, for example, '+
           'if you have your camera cleared its date/time settings to factory default.';

{ TPanel }

procedure TPanel.WMDropFiles(var Message: TWMDropFiles);
var
  I: Integer;
  Buffer: array[0..MAX_PATH] of Char;
  FileName: string;
  DataPatcher : TExifDataPatcher;
begin
  try
    SetString(FileName, Buffer, DragQueryFile(HDROP(Message.Drop), 0, Buffer, Length(Buffer)));
    try
      DataPatcher := TExifDataPatcher.Create(FileName);
      TfrmDetermineShift(Parent).SetExifDT(DataPatcher.DateTimeOriginal);
    except
      on E: EInvalidJPEGHeader do     //If it's just that a certain file isn't a JPEG or
      begin                           //is corrupt, we still want to continue with any
        Application.ShowException(E); //other filenames we have been requested to process.
        Exit;
      end
      else
        raise;
    end;
  finally
    DragFinish(HDROP(Message.Drop));
    FreeAndNil(DataPatcher);
  end;
end;

{ TfrmDetermineShift }

procedure TfrmDetermineShift.FormCreate(Sender: TObject);
begin
  DragAcceptFiles(panJPEGFile.Handle, True);

  lblDescr.Caption := SDescr;
end;

procedure TfrmDetermineShift.SetExifDT(dt: TDateTime);
begin
  FExifDT := dt;
  lblDT.Caption := DateTimeToStr(dt);
  CalcShift;
  Button1.Enabled := True;
end;

procedure TfrmDetermineShift.CalcShift;
var
  NeedDT: TDateTime;
begin
  if FExifDT = 0 then Exit; // not assigned yet
  NeedDT := DateOf(dtpDate.Date) + TimeOf(dtpTime.Time);
  FShiftMins := MinutesBetween(NeedDT, FExifDT);
  if NeedDT < FExifDT then
    FShiftMins := -FShiftMins;
  eShift.Text := IntToStr(FShiftMins);
end;

procedure TfrmDetermineShift.dtpDateChange(Sender: TObject);
begin
  CalcShift;
end;

end.
