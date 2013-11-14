program EXIFTimeEdit;

uses
  Forms,
  ExifTimeEditForm in 'ExifTimeEditForm.pas' {frmExifTimeEdit},
  DetermineShiftForm in 'DetermineShiftForm.pas' {frmDetermineShift};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmExifTimeEdit, frmExifTimeEdit);
  Application.Run;
end.
