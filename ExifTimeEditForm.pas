{**************************************************************************************}
{                                                                                      }
{ CCR Exif - Delphi class library for reading and writing Exif metadata in JPEG files  }
{ Version 1.5.2 beta                                                                   }
{                                                                                      }
{ The contents of this file are subject to the Mozilla Public License Version 1.1      }
{ (the "License"); you may not use this file except in compliance with the License.    }
{ You may obtain a copy of the License at http://www.mozilla.org/MPL/                  }
{                                                                                      }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT   }
{ WARRANTY OF ANY KIND, either express or implied. See the License for the specific    }
{ language governing rights and limitations under the License.                         }
{                                                                                      }
{ The Original Code is TimeShiftForm.pas.                                              }
{                                                                                      }
{ The Initial Developer of the Original Code is Chris Rolliston. Portions created by   }
{ Chris Rolliston are Copyright (C) 2009-2012 Chris Rolliston. All Rights Reserved.    }
{                                                                                      }
{**************************************************************************************}

unit ExifTimeEditForm;

{
  Based on the TimeShift Demo, enhanced by Fr0sT.
    * Now you can adjust dates much more flexibly, i.e. shift the necessary date part
      (year/month/day/hour/minute/second) by a value or just set the date part
      to a value you input.
    * Process large numbers of files without application hang.
    * Processed files remain in the list with changed props, so you can continue
      adjusting
    * Press Ctrl-A to select all items, Del to remove selected items
    * Determine time shift tool: calculate time shift between selected file's EXIF
      timestamp and the timestamp you input.


  Possible use cases:
    * Correct timestamps if your camera's clocks are late by several minutes
    * Correct timestamps to the local time zone if you forgot to adjust your camera clocks in a trip
    * Correct timestamps if you had your camera's clocks cleared to factory defaults
      (usually this means complete difference with current timestamp)
    * Set last-modified property of a file to EXIF timestamp


  Please keep in mind leap years and the day of 29/02 when changing year/month date parts!
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Vcl.Dialogs,
  StdCtrls, ExtCtrls, ToolWin, ComCtrls, Menus, ActnList, ImgList, Vcl.ExtDlgs,
  {$IF CompilerVersion >= 23}System.UITypes,{$IFEND} ShellApi, DateUtils,
  Registry, Vcl.Buttons,
  CCR.Exif.BaseUtils, CCR.Exif,
  DetermineShiftForm, DateParts;

type
  TListView = class(ComCtrls.TListView) //OK, using an interposer class is a bit lazy,
  protected                             //but this isn't a big app...
    procedure WMDropFiles(var Message: TWMDropFiles); message WM_DROPFILES;
  end;

  TfrmExifTimeEdit = class(TForm)
    lvFiles: TListView;
    imlColor: TImageList;
    ActionList: TActionList;
    actAdd: TAction;
    actRemove: TAction;
    actProcess: TAction;
    dlgOpen: TOpenPictureDialog;
    actFileTimeOpts: TAction;
    imlDisabled: TImageList;
    panOptions: TPanel;
    ToolBar: TToolBar;
    btnAdd: TToolButton;
    btnRemove: TToolButton;
    sep1: TToolButton;
    btnProcess: TToolButton;
    GroupBox1: TGroupBox;
    rgFileDateTime: TRadioGroup;
    rgEXIFDateTime: TRadioGroup;
    GroupBox2: TGroupBox;
    cbDatePart: TComboBox;
    cbAction: TComboBox;
    eNumber: TEdit;
    updNumber: TUpDown;
    btnDetermineShift: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure actRemoveUpdate(Sender: TObject);
    procedure actRemoveExecute(Sender: TObject);
    procedure actAddExecute(Sender: TObject);
    procedure actProcessUpdate(Sender: TObject);
    procedure actProcessExecute(Sender: TObject);
    procedure edtNumberKeyPress(Sender: TObject; var Key: Char);
    procedure lvFilesDeletion(Sender: TObject; Item: TListItem);
    procedure lvFilesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnDetermineShiftClick(Sender: TObject);
  private
    procedure OpenFile(FilePath: string; var ForceReadOnly: TModalResult; var ForceReadOnlyOptions: TMsgDlgButtons);
    procedure OpenFiles(Files: TStrings);
    procedure ProcessFiles;
    procedure UpdListItem(DataPatcher: TExifDataPatcher; Item: TListItem);
    procedure EnableControls(Enable: Boolean);
  end;

var
  frmExifTimeEdit: TfrmExifTimeEdit;

implementation

{$R *.dfm}

resourcestring
  SReadOnlyFileMsg = '%s is marked read only. Attempt to open it for read/write access anyhow?';

  SFormLabel = 'EXIF Date/Time Editor';
  SLblProgressOpen = 'Opening: (%d / %d)';
  SLblProgressProcessing = 'Processing: (%d / %d)';
  SHint_rgFileDateTime = 'File date/time is a file property, not metadata. Useful for sorting with file managers';
  SHint_rgEXIFDateTime = 'EXIF date/time field holds a timestamp of the last image modification';
  SHint_btnDetermineShift = 'Determine shift value.'#13#10+
                            'Use this tool if you have a file with wrong timestamps but'#13#10+
                            'you know what they should be';

type
  TTimestampAction = (ftKeep, ftMatchExif, ftSetToNow);

const
  RegPath = 'Software\CCR';
  ActionLabels: array[0..2] of Char =
    ('+', '-', '=');

{ TListView }

procedure TListView.WMDropFiles(var Message: TWMDropFiles);
var
  I: Integer;
  Buffer: array[0..MAX_PATH] of Char;
  FileName: string;
  Files: TStringList;
begin
  Files := TStringList.Create;
  try
    for I := 0 to DragQueryFile(HDROP(Message.Drop), $FFFFFFFF, nil, 0) - 1 do
    begin
      SetString(FileName, Buffer, DragQueryFile(HDROP(Message.Drop), I, Buffer,
        Length(Buffer)));
      Files.Add(FileName);
    end;
    frmExifTimeEdit.OpenFiles(Files);
  finally
    DragFinish(HDROP(Message.Drop));
    FreeAndNil(Files);
  end;
end;

{ TfrmTimeShiftDemo }

procedure TfrmExifTimeEdit.FormCreate(Sender: TObject);
var
  IntVal: Integer;
  s: string;
  Files: TStringList;
begin
  // visual init
  Caption := SFormLabel;
  Application.Title := SFormLabel;
  rgFileDateTime.Hint := SHint_rgFileDateTime;
  rgEXIFDateTime.Hint := SHint_rgEXIFDateTime;
  btnDetermineShift.Hint := SHint_btnDetermineShift;

  with TRegIniFile.Create(RegPath) do
  try
    IntVal := ReadInteger(Application.Title, 'FileTimeAction', 0);
    if TTimestampAction(IntVal) in [Low(TTimestampAction)..High(TTimestampAction)] then
      rgFileDateTime.ItemIndex := IntVal;
    IntVal := ReadInteger(Application.Title, 'EXIFTimeAction', 0);
    if TTimestampAction(IntVal) in [Low(TTimestampAction)..High(TTimestampAction)] then
      rgEXIFDateTime.ItemIndex := IntVal;
  finally
    Free;
  end;

  DragAcceptFiles(lvFiles.Handle, True);
  //prevent list view flicker if running on Vista or above
  if Win32MajorVersion >= 6 then lvFiles.DoubleBuffered := True;
  // fixup the toolbar
  updNumber.HandleNeeded;
  // fill combobox items
  for s in ActionLabels do
    cbAction.Items.Add(s);
  for s in DatePartLabels do
    cbDatePart.Items.Add(s);

  // load files from command line
  Files := TStringList.Create;
  try
    for IntVal := 1 to ParamCount do
      Files.Add(ParamStr(IntVal));
    OpenFiles(Files);
  finally
    FreeAndNil(Files);
  end;
end;

procedure TfrmExifTimeEdit.FormClose(Sender: TObject; var Action: TCloseAction);
var
  RegIniFile: TRegIniFile;
begin
  RegIniFile := nil;
  try
    RegIniFile := TRegIniFile.Create(RegPath);
    RegIniFile.WriteInteger(Application.Title, 'FileTimeAction', rgFileDateTime.ItemIndex);
    RegIniFile.WriteInteger(Application.Title, 'EXIFTimeAction', rgEXIFDateTime.ItemIndex);
  finally
    RegIniFile.Free;
  end;
end;

// Open one file and add it to the listview
procedure TfrmExifTimeEdit.OpenFile(FilePath: string; var ForceReadOnly: TModalResult; var ForceReadOnlyOptions: TMsgDlgButtons);
var
  DataPatcher: TExifDataPatcher;
  Item: TListItem;
  ReadOnly: Boolean;
begin
  ReadOnly := FileIsReadOnly(FilePath);
  if ReadOnly then //we can't open a read-only file with read/write access obviously
  begin
    // ask a user if he wants us to modify RO files
    if ForceReadOnly = mrNone then
    begin
      ForceReadOnly := MessageDlg(Format(SReadOnlyFileMsg, [ExtractFileName(FilePath)]),
        mtConfirmation, ForceReadOnlyOptions, 0, mbNo);
      if ForceReadOnly = mrNone then ForceReadOnly := mrNo;
    end;
    if IsNegativeResult(ForceReadOnly) then
      Exit;
    // try to make file RW
    if not FileSetReadOnly(FilePath, False) then
    begin
      MessageDlg(SysErrorMessage(GetLastError), mtError, [mbOK], 0);
      Exit;
    end;
    // return RO state for now (we'll change it later in process)
    if ReadOnly then
      FileSetReadOnly(FilePath, True);
  end;
  try
    DataPatcher := TExifDataPatcher.Create(FilePath);
  except
    on E: EInvalidJPEGHeader do     //If it's just that a certain file isn't a JPEG or
    begin                           //is corrupt, we still want to continue with any
      Application.ShowException(E); //other filenames we have been requested to process.
      Exit;
    end
    else
      raise;
  end;
  Item := lvFiles.Items.Add;
  Item.Data := DataPatcher;
  Item.Caption := ExtractFileName(FilePath);
  Item.SubItems.AddObject('', TObject(ReadOnly));
  Item.SubItems.Add('');
  Item.SubItems.Add('');
  Item.SubItems.Add('');
  UpdListItem(DataPatcher, Item);
end;

// Open & add all the files from given list.
// Shows progress label and refreshes form view during the cycle
procedure TfrmExifTimeEdit.OpenFiles(Files: TStrings);
var
  S: string;
  ForceReadOnly: TModalResult;
  ForceReadOnlyOptions: TMsgDlgButtons;
  Counter: Integer;
begin
  if Files.Count = 0 then Exit;
  ForceReadOnly := mrNone;
  ForceReadOnlyOptions := [mbYes, mbNo];
  if Files.Count > 1 then
    ForceReadOnlyOptions := ForceReadOnlyOptions + [mbNoToAll, mbYesToAll];
  Counter := 0;
  EnableControls(False);  // ! disable the panel to avoid unneeded user input while doing the job
  try
    for S in Files do
    begin
      OpenFile(S, ForceReadOnly, ForceReadOnlyOptions);
      Inc(Counter);
      // visual refresh each 10 iterations
      if Counter mod 10 <> 0 then Continue;
      Caption := Format(SLblProgressOpen, [Counter, Files.Count]);
      Application.ProcessMessages;
    end;
  finally
    Caption := SFormLabel;
    EnableControls(True);
  end;
end;

// Do main job
procedure TfrmExifTimeEdit.ProcessFiles;
var
  FileDtAction, ExifDtAction: TTimestampAction;
  ChangeAction: TDateChangeAction;
  DatePart: TDatePart;
  Number: Integer;
  DTModified: TDateTime;

// If a given DateTime is valid, change it according to settings and update
// Uses external variables:
//   ChangeAction, DatePart, Number (read)
//   DTModified (write)
function DoChangeTime(const DateTime: TDateTimeTagValue): TDateTimeTagValue;
begin
  if DateTime.MissingOrInvalid then Exit;
  Result := ChangeDate(ChangeAction, DatePart, Number, DateTime);
  DTModified := Result;
end;

var
  DataPatcher: TExifDataPatcher;
  I: Integer;
  ReadOnly: Boolean;
begin
  // Get current options
  FileDtAction := TTimestampAction(rgFileDateTime.ItemIndex);
  if not (FileDtAction in [Low(TTimestampAction)..High(TTimestampAction)]) then
    FileDtAction := ftKeep;
  ExifDtAction := TTimestampAction(rgEXIFDateTime.ItemIndex);
  if not (ExifDtAction in [Low(TTimestampAction)..High(TTimestampAction)]) then
    ExifDtAction := ftKeep;
  Number := updNumber.Position;
  case cbAction.ItemIndex of // '+', '-', '='
    0: begin ChangeAction := dcaShift; end;
    1: begin ChangeAction := dcaShift; Number := -Number; end;
    2: begin ChangeAction := dcaSet; end;
    else Exit;
  end;
  if cbDatePart.ItemIndex = -1 then Exit;
  DatePart := TDatePart(cbDatePart.ItemIndex);

  // Action!
  EnableControls(False);  // ! disable the panel to avoid unneeded user input while doing the job
  try
    for I := 0 to lvFiles.Items.Count - 1 do
    begin
      DataPatcher := lvFiles.Items[I].Data;
      ReadOnly := Boolean(lvFiles.Items[I].SubItems.Objects[0]);
      // change EXIF dates
      DataPatcher.DateTimeOriginal  := DoChangeTime(DataPatcher.DateTimeOriginal);
      DataPatcher.DateTimeDigitized := DoChangeTime(DataPatcher.DateTimeDigitized);
      DataPatcher.DateTime          := DoChangeTime(DataPatcher.DateTime);    {}
      DataPatcher.PreserveFileDate := (FileDtAction = ftKeep);
      // make RO files RW and save changes
      if ReadOnly then
        FileSetReadOnly(DataPatcher.FileName, False);
      DataPatcher.UpdateFile;
      if FileDtAction = ftMatchExif then
      begin
        if DTModified <> 0 then
          DataPatcher.FileDateTime := DTModified;
      end;
      // make files RO again
      if ReadOnly then
        FileSetReadOnly(DataPatcher.FileName, True);
      // dislay changes
      UpdListItem(DataPatcher, lvFiles.Items[I]);
      // visual refresh each 10 iterations
      if I mod 10 <> 0 then Continue;
      Caption := Format(SLblProgressProcessing, [I, lvFiles.Items.Count]);
      Application.ProcessMessages;
    end; // for
  finally
    Caption := SFormLabel;
    EnableControls(True);
  end;
end;

procedure TfrmExifTimeEdit.EnableControls(Enable: Boolean);
begin
  panOptions.Enabled := Enable;
end;

// Updates visual look of a listview item according to the values of associated DataPatcher
procedure TfrmExifTimeEdit.UpdListItem(DataPatcher: TExifDataPatcher; Item: TListItem);

function DateTimeToText(const DateTime: TDateTimeTagValue): string;
begin
  if DateTime.MissingOrInvalid then
    Result := '<missing>'
  else
    Result := SysUtils.DateTimeToStr(DateTime);
end;

begin
  Item.SubItems[0] := DateTimeToText(DataPatcher.FileDateTime);
  Item.SubItems[1] := DateTimeToText(DataPatcher.DateTime);
  Item.SubItems[2] := DateTimeToText(DataPatcher.DateTimeOriginal);
  Item.SubItems[3] := DateTimeToText(DataPatcher.DateTimeDigitized);
end;

// Free associated object (+ close the file) when deleting an item from the list
procedure TfrmExifTimeEdit.lvFilesDeletion(Sender: TObject; Item: TListItem);
begin
  TExifDataPatcher(Item.Data).Free;
end;

// Process Ctrl-A and Del
procedure TfrmExifTimeEdit.lvFilesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_DELETE:
      actRemove.Execute;
    Word('A'):
      if Shift = [ssCtrl] then
        (Sender as TListView).SelectAll;
  end;
end;

// filter all non-digit keystrokes
procedure TfrmExifTimeEdit.edtNumberKeyPress(Sender: TObject; var Key: Char);
begin
  if not CharInSet(Key, ['0'..'9']) then
  begin
    Key := #0;
    Beep;
  end;
end;

// Open dialog and add files
procedure TfrmExifTimeEdit.actAddExecute(Sender: TObject);
begin
  if dlgOpen.Execute then OpenFiles(dlgOpen.Files);
end;

procedure TfrmExifTimeEdit.actRemoveUpdate(Sender: TObject);
begin
  actRemove.Enabled := (lvFiles.SelCount > 0);
end;

procedure TfrmExifTimeEdit.btnDetermineShiftClick(Sender: TObject);
begin
  with TfrmDetermineShift.Create(nil) do
  begin
    if ShowModal = mrOk then
    begin
      cbDatePart.ItemIndex := cbDatePart.Items.IndexOf(DatePartLabels[dpMinute]);
      if ShiftMins > 0 then
        cbAction.ItemIndex := 0
      else if ShiftMins < 0 then
        cbAction.ItemIndex := 1
      else if ShiftMins = 0 then
        cbAction.ItemIndex := 2;
      updNumber.Position := Abs(ShiftMins);
    end;
    Free;
  end;
end;

procedure TfrmExifTimeEdit.actProcessUpdate(Sender: TObject);
begin
  actProcess.Enabled :=
    (lvFiles.Items.Count > 0) and
    (cbDatePart.ItemIndex >= 0) and
    (cbAction.ItemIndex >= 0) and
    // no modifications required
    not ( (cbAction.ItemIndex in [0,1]) and (updNumber.Position = 0)
          and (rgFileDateTime.ItemIndex = 0) and (rgEXIFDateTime.ItemIndex = 0) );
end;

procedure TfrmExifTimeEdit.actProcessExecute(Sender: TObject);
begin
  ProcessFiles;
end;

// remove selected items from the list
procedure TfrmExifTimeEdit.actRemoveExecute(Sender: TObject);
var
  i, FocusIdx: Integer;
begin
  FocusIdx := lvFiles.ItemFocused.Index;
  // remove
  i := 0;
  while i < lvFiles.Items.Count do
    if lvFiles.Items[i].Selected then
      lvFiles.Items[i].Delete
    else
      Inc(i);
  // focus & select next item, or the last one
  if lvFiles.Items.Count = 0 then Exit; // do nothing if list is empty
  if FocusIdx >= lvFiles.Items.Count then
    FocusIdx := lvFiles.Items.Count - 1;
  lvFiles.Items[FocusIdx].Selected := True;
  lvFiles.Items[FocusIdx].Focused := True;
end;

end.
