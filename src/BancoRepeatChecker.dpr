//********************************************************************************
//* BANCO REPEAT CHECKER                                                         *
//* -----------------------------------------------------------------------------*
//* Application to make some fun with past draw results of Loto-Québec BANCO     *
//* lottery draw results.                                                        *
//* May be executed prior build, commmit, backup, etc. to remove unused files.   *
//* Written by Denis Bisson, Drummondville, Québec, 2021-09-09.                  *
//* -----------------------------------------------------------------------------*
//* Originally written by Denis Bisson, Drummondville, Québec, Canada            *
//*   https://github.com/denis-bisson/                                           *
//*   2021-09-09                                                                 *
//* -----------------------------------------------------------------------------*
//* You should not remove these comments.                                        *
//********************************************************************************
//*

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
