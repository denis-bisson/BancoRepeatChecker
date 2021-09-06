unit fBancoRepeatChecker;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.ImageList, Vcl.ImgList,
  System.Actions, Vcl.ActnList, Vcl.ComCtrls, Vcl.ToolWin, Vcl.StdCtrls,
  Vcl.Samples.Gauges, Vcl.AppEvnts;

const
  NB_BALLS_PER_DRAW = 20;
  MAX_BALL_NUMBER = 70;

type
  TBancoDraw = class(TObject)
  private
    FDate: TDateTime;
    FNumbers: array[0..pred(NB_BALLS_PER_DRAW)] of byte;
  public
    property Date: TDateTime read FDate;
    constructor MyCreate(const paramLine: string; const iLineNumber: integer; const dtPreviousDraw: TDateTime; paramSlLinesWithError: TStringList);
    function CompareToThisDraw(APreviousDraw: TBancoDraw; var paramNbMatch: integer): string;
    function GetBasicReportLine: string;
  end;

  TBancoDrawList = class(TList)
  private
    function GetBancoDraw(iIndex: integer): TBancoDraw;
  public
    property BancoDraw[iIndex: integer]: TBancoDraw read GetBancoDraw;
  end;

  TfrmBancoRepeatChecker = class(TForm)
    ilMainImageList: TImageList;
    alMainActionList: TActionList;
    actChercheCetteCombinaison: TAction;
    actValideLeFichierDesNumeros: TAction;
    actExit: TAction;
    actEdit: TAction;
    tbMainToolBar: TToolBar;
    tbOne: TToolButton;
    tbTwo: TToolButton;
    tbFour: TToolButton;
    tbFive: TToolButton;
    tbSix: TToolButton;
    pgMainPageControl: TPageControl;
    tsLog: TTabSheet;
    MasterGage: TGauge;
    StatusWindow: TRichEdit;
    tsResults: TTabSheet;
    ResultPageControl: TPageControl;
    tsSummary: TTabSheet;
    memoSommaire: TMemo;
    ts0: TTabSheet;
    mm0: TMemo;
    ts1: TTabSheet;
    mm1: TMemo;
    ts2: TTabSheet;
    mm2: TMemo;
    ts3: TTabSheet;
    mm3: TMemo;
    ts4: TTabSheet;
    mm4: TMemo;
    ts5: TTabSheet;
    mm5: TMemo;
    ts6: TTabSheet;
    mm6: TMemo;
    ts7: TTabSheet;
    mm7: TMemo;
    ts8: TTabSheet;
    mm8: TMemo;
    ts9: TTabSheet;
    mm9: TMemo;
    ts10: TTabSheet;
    mm10: TMemo;
    ts11: TTabSheet;
    mm11: TMemo;
    ts12: TTabSheet;
    mm12: TMemo;
    ts13: TTabSheet;
    mm13: TMemo;
    ts14: TTabSheet;
    mm14: TMemo;
    ts15: TTabSheet;
    mm15: TMemo;
    ts16: TTabSheet;
    mm16: TMemo;
    ts17: TTabSheet;
    mm17: TMemo;
    ts18: TTabSheet;
    mm18: TMemo;
    ts19: TTabSheet;
    mm19: TMemo;
    ts20: TTabSheet;
    mm20: TMemo;
    aeMainApplicationEvent: TApplicationEvents;
    procedure actEditExecute(Sender: TObject);
    procedure aeMainApplicationEventIdle(Sender: TObject; var Done: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure actValideLeFichierDesNumerosExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actChercheCetteCombinaisonExecute(Sender: TObject);
  private
    { Private declarations }
    ABancoDrawList: TBancoDrawList;
    bFirstIdle, bFinalActionResult: boolean;
    WantedFinalSheet: TTabSheet;
    ArrayMemo: array[0..NB_BALLS_PER_DRAW] of TMemo;
    ArrayStats: array[0..NB_BALLS_PER_DRAW] of integer;
    function LoadDatabaseInMemory: boolean;
    procedure WriteStatus(const sMessageToShow: string; const iColorToUse: dword);
    procedure DisableToute;
    procedure EnableToute;
    procedure LoadConfiguration;
    procedure SaveConfiguration;
    procedure MySetTitle;
  public
    { Public declarations }
  end;

var
  frmBancoRepeatChecker: TfrmBancoRepeatChecker;

implementation

uses
  Winapi.ShellAPI, System.UITypes, System.Win.Registry, System.StrUtils;

{$R *.dfm}

const
  BASELOCATIONOFREGISTRYINI = 'SOFTWARE\DENISBISSON\BANCOREPEATCHECKER';
  MAINCONFIGSECTION = 'MainSection';

  COLORDANGER = clOlive;
  COLORSUCCESS = clGreen;
  COLORERROR = clRed;
  COLORSTATUS = clBlack;
  COLORWINDOW_WORKING = $D0FFFF;
  COLORWINDOW_SUCCESS = $E0FFE0;
  COLORWINDOW_ERROR = $E0E0FF;

  { TBancoDraw.MyCreate }
constructor TBancoDraw.MyCreate(const paramLine: string; const iLineNumber: integer; const dtPreviousDraw: TDateTime; paramSlLinesWithError: TStringList);
var
  iBallIndex, iPreviousBall, iInitialNumberOfError: integer;
  sBallNumber: string;
  Year, Month, Day: WORD;
begin
  inherited Create;
  iInitialNumberOfError := paramSlLinesWithError.Count;

  Year := StrToIntDef(copy(paramLine, 1, 4), 0);
  Month := StrToIntDef(copy(paramLine, 6, 2), 0);
  Day := StrToIntDef(copy(paramLine, 9, 2), 0);

  if (Year <= 1988) or (Month < 1) or (Month > 12) or (Day < 1) or (Day > 31) then
  begin
    paramSlLinesWithError.Add('Je ne détecte pas une date valide...');
  end
  else
  begin
    FDate := EncodeDate(Year, Month, Day);
    if FDate > now then
    begin
      paramSlLinesWithError.Add('La date détectée me semble être dans le futur...');
    end
    else
    begin
      if dtPreviousDraw <= FDate then
      begin
        paramSlLinesWithError.Add('La date détectée est plus récente ou la même que la date de la ligne précédente...');
      end;
    end;
  end;

  if paramSlLinesWithError.Count = iInitialNumberOfError then
  begin
    iPreviousBall := 0;
    iBallIndex := 0;
    while (iBallIndex < NB_BALLS_PER_DRAW) and (iInitialNumberOfError = paramSlLinesWithError.Count) do
    begin
      sBallNumber := copy(paramLine, 13 + (iBallIndex * 3), 2);
      FNumbers[iBallIndex] := StrToIntDef(sBallNumber, 0);
      if (FNumbers[iBallIndex] = 0) then
        paramSlLinesWithError.Add(Format('Je ne détecte pas un numéro adéquat pour la boule #%d: %s.', [succ(iBallIndex), sBallNumber]))
      else if (FNumbers[iBallIndex] > MAX_BALL_NUMBER) then
        paramSlLinesWithError.Add(Format('Je détecte un numéro supérieur à 70 pour la boule #%d: %s.', [succ(iBallIndex), sBallNumber]))
      else if (FNumbers[iBallIndex] <= iPreviousBall) then
        paramSlLinesWithError.Add(Format('Je détecte un numéro inférieur à la boule précédente pour boule #%d: %d versus %d.', [succ(iBallIndex), FNumbers[iBallIndex], iPreviousBall]));
      iPreviousBall := FNumbers[iBallIndex];
      inc(iBallIndex);
    end;
  end;

  if paramSlLinesWithError.Count > iInitialNumberOfError then
  begin
    paramSlLinesWithError.Insert(pred(paramSlLinesWithError.Count), '');
    paramSlLinesWithError.Insert(pred(paramSlLinesWithError.Count), Format('Problème avec ligne #%d: %s', [iLineNumber, paramLine]));
  end;
end;

{ TBancoDraw.CompareToThisDraw }
function TBancoDraw.CompareToThisDraw(APreviousDraw: TBancoDraw; var paramNbMatch: integer): string;
var
  iCurrentIndex, iPreviousIndex, iStartPreviousIndex: integer;
  bGetOutLittleLoop: boolean;
begin
  result := '';
  paramNbMatch := 0;
  iCurrentIndex := 0;
  iStartPreviousIndex := 0;

  while (iCurrentIndex < NB_BALLS_PER_DRAW) and (iStartPreviousIndex < NB_BALLS_PER_DRAW) do
  begin
    bGetOutLittleLoop := False;
    iPreviousIndex := iStartPreviousIndex;
    while (iPreviousIndex < NB_BALLS_PER_DRAW) and (not bGetOutLittleLoop) do
    begin
      if FNumbers[iCurrentIndex] = APreviousDraw.FNumbers[iPreviousIndex] then
      begin
        inc(paramNbMatch);
        result := result + IfThen(result <> '', ',', '') + Format('%2.2d', [FNumbers[iCurrentIndex]]);
        iStartPreviousIndex := succ(iPreviousIndex);
        bGetOutLittleLoop := True;
      end
      else
      begin
        if FNumbers[iCurrentIndex] < APreviousDraw.FNumbers[iPreviousIndex] then
        begin
          bGetOutLittleLoop := True;
        end;
      end;
      inc(iPreviousIndex);
    end;
    inc(iCurrentIndex);
  end;

  result := Format('%2.2d : %s', [paramNbMatch, Result]);
end;

{ TBancoDraw.GetBasicReportLine }
function TBancoDraw.GetBasicReportLine: string;
var
  iBallIndex: integer;
  Year, Month, Day: Word;
begin
  result := '';
  DecodeDate(FDate, Year, Month, Day);
  result := Format('%4.4d-%2.2d-%2.2d ', [Year, Month, Day]);
  for iBallIndex := 0 to pred(NB_BALLS_PER_DRAW) do
    result := result + Format(' %2.2d', [FNumbers[iBallIndex]]);
end;

{ TBancoDrawList.GetBancoDraw }
function TBancoDrawList.GetBancoDraw(iIndex: integer): TBancoDraw;
begin
  result := TBancoDraw(Items[iIndex]);
end;

{ TfrmBancoRepeatChecker.LoadDatabaseInMemory }
function TfrmBancoRepeatChecker.LoadDatabaseInMemory: boolean;
var
  slAllDraws, slErrorLines: TStringList;
  sFilename, sMaybeDraw: string;
  dtPreviousDraw: TDateTime;
  iLineIndex: integer;
  ABancoDraw: TBancoDraw;
begin
  result := False;
  slAllDraws := TStringList.Create;
  slErrorLines := TStringList.Create;
  try
    ABancoDrawList.Clear;

    sFilename := IncludeTrailingPathDelimiter(ExtractFilePath(GetModuleName(HInstance))) + 'Banco.txt';
    if FileExists(sFilename) then
    begin
      slAllDraws.LoadFromFile(sFilename);
      dtPreviousDraw := EncodeDate(2100, 08, 05);

      for iLineIndex := 0 to pred(slAllDraws.Count) do
      begin
        sMaybeDraw := Trim(slAllDraws.Strings[iLineIndex]);
        if sMaybeDraw <> '' then
        begin
          ABancoDraw := TBancoDraw.MyCreate(sMaybeDraw, succ(iLineIndex), dtPreviousDraw, slErrorLines);
          ABancoDrawList.Add(ABancoDraw);
          dtPreviousDraw := ABancoDraw.FDate;
        end;
      end;

      WriteStatus('Nombre d''erreurs détectées: ' + (slErrorLines.Count div 3).ToString, COLORSTATUS);
      if slErrorLines.Count > 0 then
      begin
        for iLineIndex := 0 to pred(slErrorLines.Count) do WriteStatus(slErrorLines.Strings[iLineIndex], COLORERROR);
        WriteStatus('ERREUR: Il doit n''y avoir aucune erreur pour continuer...', COLORERROR);
        WriteStatus('        Révisez votre fichier d''origine!', COLORERROR);
      end
      else
      begin
        WriteStatus('Aucune erreur détectée! Bravo!', COLORSUCCESS);
        result := True;
      end;

    end
    else
    begin
      WriteStatus(Format('ERREUR: Je ne trouve pas ce fichier "%s"...', [sFilename]), COLORERROR);
    end;

  finally
    FreeAndNil(slAllDraws);
    FreeAndNil(slErrorLines);
  end;
end;

{ TfrmBancoRepeatChecker.aeMainApplicationEventIdle }
procedure TfrmBancoRepeatChecker.actValideLeFichierDesNumerosExecute(Sender: TObject);
begin
  DisableToute;
  try
    bFinalActionResult := LoadDatabaseInMemory;
  finally
    EnableToute;
  end;
end;

{ TfrmBancoRepeatChecker.actExitExecute }
procedure TfrmBancoRepeatChecker.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfrmBancoRepeatChecker.aeMainApplicationEventIdle(Sender: TObject; var Done: Boolean);
begin
  if bFirstIdle then
  begin
    bFirstIdle := FALSE;
    LoadConfiguration;
  end;
end;

{ TfrmBancoRepeatChecker.FormClose }
procedure TfrmBancoRepeatChecker.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveConfiguration;
end;

{ TfrmBancoRepeatChecker.FormCreate }
procedure TfrmBancoRepeatChecker.FormCreate(Sender: TObject);
begin
  bFirstIdle := True;
  ABancoDrawList := TBancoDrawList.Create;
  ArrayMemo[0] := mm0;
  ArrayMemo[1] := mm1;
  ArrayMemo[2] := mm2;
  ArrayMemo[3] := mm3;
  ArrayMemo[4] := mm4;
  ArrayMemo[5] := mm5;
  ArrayMemo[6] := mm6;
  ArrayMemo[7] := mm7;
  ArrayMemo[8] := mm8;
  ArrayMemo[9] := mm9;
  ArrayMemo[10] := mm10;
  ArrayMemo[11] := mm11;
  ArrayMemo[12] := mm12;
  ArrayMemo[13] := mm13;
  ArrayMemo[14] := mm14;
  ArrayMemo[15] := mm15;
  ArrayMemo[16] := mm16;
  ArrayMemo[17] := mm17;
  ArrayMemo[18] := mm18;
  ArrayMemo[19] := mm19;
  ArrayMemo[20] := mm20;
  MySetTitle;
end;

procedure TfrmBancoRepeatChecker.WriteStatus(const sMessageToShow: string; const iColorToUse: dword);
var
  iHour, iMinute, iSec, iMilliSec: word;
begin
  DecodeTime(now, iHour, iMinute, iSec, iMilliSec);
  StatusWindow.SelAttributes.Color := iColorToUse;
  StatusWindow.Lines.Add(Format('%2.2d:%2.2d:%2.2d:%3.3d:%s', [iHour, iMinute, iSec, iMilliSec, sMessageToShow]));
  Application.ProcessMessages;
end;

{ TfrmBancoRepeatChecker.actEditExecute }
procedure TfrmBancoRepeatChecker.actEditExecute(Sender: TObject);
var
  sNomFichierIn: string;
begin
  sNomFichierIn := paramstr(0);
  sNomFichierIn := IncludeTrailingPathDelimiter(ExtractFilePath(sNomFichierIn)) + 'Banco.txt';
  WriteStatus('On vérifie la présence du fichier ' + sNomFichierIn + ' ...', COLORDANGER);
  if FileExists(sNomFichierIn) then
  begin
    ShellExecute(Application.Handle, 'open', PChar(sNomFichierIn), nil, PChar(ExtractFilePath(sNomFichierIn)), SW_SHOWNORMAL); // SW_NORMAL
  end
  else
  begin
    MessageDlg('ERREUR: Je ne trouve pas le fichier des numéro...' + #$0A + #$0A + 'Normalement: ' + sNomFichierIn, mtError, [mbOk], 0);
  end;
end;

{ LoadWindowRegistryConfig }
procedure LoadWindowRegistryConfig(RegistryConfigFile: TRegistryIniFile; WorkingForm: TForm; SectionName: string);
begin
  WorkingForm.WindowState := TWindowState(RegistryConfigFile.ReadInteger(SectionName, 'WindowState', ord(wsNormal)));

  if WorkingForm.WindowState <> wsMaximized then
  begin
    if WorkingForm.WindowState = wsMinimized then
      WorkingForm.WindowState := wsNormal;
    WorkingForm.Width := RegistryConfigFile.ReadInteger(SectionName, 'width', WorkingForm.Constraints.MinWidth);
    WorkingForm.Height := RegistryConfigFile.ReadInteger(SectionName, 'height', WorkingForm.Constraints.MinHeight);
  end;

  WorkingForm.Left := RegistryConfigFile.ReadInteger(SectionName, 'left', (Screen.Width - WorkingForm.Width) div 2);
  WorkingForm.Top := RegistryConfigFile.ReadInteger(SectionName, 'top', (Screen.Height - WorkingForm.Height) div 2);
end;

{ SaveWindowRegistryConfig }
procedure SaveWindowRegistryConfig(RegistryConfigFile: TRegistryIniFile; WorkingForm: TForm; SectionName: string);
begin
  RegistryConfigFile.WriteInteger(SectionName, 'WindowState', ord(WorkingForm.WindowState));
  if WorkingForm.WindowState <> wsMaximized then
  begin
    RegistryConfigFile.WriteInteger(SectionName, 'width', WorkingForm.Width);
    RegistryConfigFile.WriteInteger(SectionName, 'height', WorkingForm.Height);
  end;

  RegistryConfigFile.WriteInteger(SectionName, 'left', WorkingForm.Left);
  RegistryConfigFile.WriteInteger(SectionName, 'top', WorkingForm.Top);
end;

{ TfrmBancoRepeatChecker.LoadConfiguration }
procedure TfrmBancoRepeatChecker.LoadConfiguration;
var
  BancoRepeatCheckerIniRegistry: TRegistryIniFile;
begin
  BancoRepeatCheckerIniRegistry := TRegistryIniFile.Create(BASELOCATIONOFREGISTRYINI);
  try
    with BancoRepeatCheckerIniRegistry do
    begin
      LoadWindowRegistryConfig(BancoRepeatCheckerIniRegistry, Self, MAINCONFIGSECTION);
      pgMainPageControl.ActivePageIndex := ReadInteger(MAINCONFIGSECTION, 'pgMainPageControl', 1);
      ResultPageControl.ActivePageIndex := ReadInteger(MAINCONFIGSECTION, 'ResultPageControl', 0);
      //..LoadConfiguration
    end;
  finally
    BancoRepeatCheckerIniRegistry.Free;
  end;
end;

{ TfrmBancoRepeatChecker.SaveConfiguration }
procedure TfrmBancoRepeatChecker.SaveConfiguration;
var
  BancoRepeatCheckerIniRegistry: TRegistryIniFile;
begin
  BancoRepeatCheckerIniRegistry := TRegistryIniFile.Create(BASELOCATIONOFREGISTRYINI);
  try
    with BancoRepeatCheckerIniRegistry do
    begin
      SaveWindowRegistryConfig(BancoRepeatCheckerIniRegistry, Self, MAINCONFIGSECTION);
      WriteInteger(MAINCONFIGSECTION, 'pgMainPageControl', pgMainPageControl.ActivePageIndex);
      WriteInteger(MAINCONFIGSECTION, 'ResultPageControl', ResultPageControl.ActivePageIndex);
      //..SaveC
    end;
  finally
    BancoRepeatCheckerIniRegistry.Free;
  end;
end;

{ TfrmBancoRepeatChecker.DisableToute }
procedure TfrmBancoRepeatChecker.DisableToute;
var
  iAction, iMemoIndex: integer;
begin
  for iAction := 0 to pred(alMainActionList.ActionCount) do
    alMainActionList.Actions[iAction].Enabled := FALSE;

  for iMemoIndex := 0 to NB_BALLS_PER_DRAW do
  begin
    ArrayMemo[iMemoIndex].Clear;
    ArrayStats[iMemoIndex] := 0;
  end;
  memoSommaire.Clear;

  bFinalActionResult := FALSE;
  WantedFinalSheet := nil;
  StatusWindow.Clear;
  StatusWindow.Color := COLORWINDOW_WORKING;
  pgMainPageControl.ActivePage := tsLog;
end;

{ TfrmBancoRepeatChecker.EnableToute }
procedure TfrmBancoRepeatChecker.EnableToute;
var
  iAction: integer;
begin
  for iAction := 0 to pred(alMainActionList.ActionCount) do
    alMainActionList.Actions[iAction].Enabled := TRUE;

  if bFinalActionResult then
  begin
    WriteStatus('L''opération a été un succès!', COLORSUCCESS);
    StatusWindow.Color := COLORWINDOW_SUCCESS;
    if WantedFinalSheet <> nil then
      pgMainPageControl.ActivePage := WantedFinalSheet;
  end
  else
  begin
    WriteStatus('L''opération a échouée...', COLORERROR);
    StatusWindow.Color := COLORWINDOW_ERROR;
  end;
end;

{ TfrmBancoRepeatChecker.actChercheCetteCombinaisonExecute }
procedure TfrmBancoRepeatChecker.actChercheCetteCombinaisonExecute(Sender: TObject);
var
  iDrawIndex, iNbMatch, iMatchIndex: integer;
  RememberCursor: TCursor;
  sCurrentLine: string;
begin
  DisableToute;
  RememberCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;
    if LoadDatabaseInMemory then
    begin
      MasterGage.Progress := 0;
      MasterGage.MaxValue := ABancoDrawList.Count;
      MasterGage.Visible := True;
      Application.ProcessMessages;
      try

        StatusWindow.Lines.BeginUpdate;
        try
          for iDrawIndex := 0 to pred(pred(ABancoDrawList.Count)) do
          begin
            sCurrentLine := ABancoDrawList.BancoDraw[iDrawIndex].GetBasicReportLine;
            StatusWindow.Lines.Add(sCurrentLine + ' - ' + ABancoDrawList.BancoDraw[iDrawIndex].CompareToThisDraw(ABancoDrawList.BancoDraw[succ(iDrawIndex)], iNbMatch));
            ArrayMemo[iNbMatch].Lines.Add(sCurrentLine);
            inc(ArrayStats[iNbMatch]);
            MasterGage.Progress := MasterGage.Progress + 1;
          end;

          StatusWindow.Lines.Add(ABancoDrawList.BancoDraw[pred(ABancoDrawList.Count)].GetBasicReportLine);
          MasterGage.Progress := MasterGage.Progress + 1;

          for iMatchIndex := 0 to NB_BALLS_PER_DRAW do
          begin
            MemoSommaire.Lines.Add(Format('Nombre de retour de %2.2d numéro%s: %d', [iMatchIndex, IfThen(iMatchIndex > 1, 's', ' '), ArrayStats[iMatchIndex]]));
            ArrayMemo[iMatchIndex].ScrollBars := ssBoth;
            ArrayMemo[iMatchIndex].WordWrap := False;
          end;

          pgMainPageControl.ActivePage := tsResults;
          ResultPageControl.ActivePage := tsSummary;
        finally
          StatusWindow.Lines.EndUpdate;
        end;
        Application.ProcessMessages;
      finally
        MasterGage.Visible := False;
      end;

      StatusWindow.SelStart := Length(StatusWindow.Text);
      StatusWindow.SelLength := 0;
      SendMessage(StatusWindow.Handle, EM_SCROLLCARET, 0, 0);

      bFinalActionResult := True;
    end;
  finally
    EnableToute;
    Screen.Cursor := RememberCursor;
  end;
end;

{ TfrmBancoRepeatChecker.MySetTitle }
procedure TfrmBancoRepeatChecker.MySetTitle;
var
  dwInfoSize, // Size of VERSIONINFO structure
  dwVerSize, // Size of Version Info Data
  dwWnd: DWORD; // Handle for the size call.
  FI: PVSFixedFileInfo; // Delphi structure; see WINDOWS.PAS
  ptrVerBuf: Pointer; // pointer to a version buffer
  V1, V2, V3, V4: string;
begin
  V1 := '';
  V2 := '';
  V3 := '';
  V4 := '';

  dwInfoSize := getFileVersionInfoSize(PChar(GetModuleName(HInstance)), dwWnd);

  if (dwInfoSize <> 0) then
  begin
    getMem(ptrVerBuf, dwInfoSize);
    try
      if getFileVersionInfo(PChar(GetModuleName(HInstance)), dwWnd, dwInfoSize, ptrVerBuf) then
      begin
        if verQueryValue(ptrVerBuf, '\', Pointer(FI), dwVerSize) then
        begin
          V1 := Format('%d', [hiWord(FI.dwFileVersionMS)]);
          V2 := Format('%d', [loWord(FI.dwFileVersionMS)]);
          V3 := Format('%d', [hiWord(FI.dwFileVersionLS)]);
          V4 := Format('%d', [loWord(FI.dwFileVersionLS)]);
          Caption := Application.Title + ' - ' + V1 + '.' + V2;
        end;
      end;
    finally
      freeMem(ptrVerBuf);
    end;
  end;
end;

end.

