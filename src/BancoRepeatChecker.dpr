program BancoRepeatChecker;

uses
  Vcl.Forms,
  fBancoRepeatChecker in 'fBancoRepeatChecker.pas' {frmBancoRepeatChecker};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'BANCO REPEAT CHECKER';
  Application.CreateForm(TfrmBancoRepeatChecker, frmBancoRepeatChecker);
  Application.Run;
end.
