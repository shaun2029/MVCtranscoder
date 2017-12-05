unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, Process, LCLType, Menus, Spin, Buttons;

type
  TEncSettings = (esAuto, esQVBR, esCQP, esVBR);

  TTranscode = record
    AProcess : TProcess;
    StdOutput: TStringList;
    StdErr: TStringList;
    Frames : longint;
    SrcFile, DstFile: String;
    MemoText: string;
    Running: boolean;
  end;

  { TfrmMain }

  TfrmMain = class(TForm)
    btnProcessFile: TButton;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    lblBitrate: TLabel;
    lblMaxBitrate: TLabel;
    lblQuality: TLabel;
    mnuAddTab: TMenuItem;
    mmEncode: TMemo;
    Panel1: TPanel;
    pbarEncoding: TProgressBar;
    mnuTabs: TPopupMenu;
    SpinEdit1: TSpinEdit;
    seBitrate: TSpinEdit;
    seMaxBitrate: TSpinEdit;
    seQuality: TSpinEdit;
    stxtFrames: TStaticText;
    tabSettings: TTabControl;
    tabTitles: TTabControl;
    tmrUpdate: TTimer;
    procedure btnAddTabClick(Sender: TObject);
    procedure btnProcessFileClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure mnuAddTabClick(Sender: TObject);
    procedure tabSettingsChange(Sender: TObject);
    procedure tabTitlesChange(Sender: TObject);
    procedure tabTitlesChanging(Sender: TObject; var AllowChange: Boolean);
    procedure tmrUpdateTimer(Sender: TObject);
  private
    Transcodes: array of TTranscode;
    function AddTranscode: integer;
    procedure EncodeFile(Id: integer);
    procedure UpdateProgress(Id: integer);
    { private declarations }
  public
    { public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.EncodeFile(Id: integer);
const
  BUF_SIZE = 2048; // Buffer size for reading the output in chunks
var
  BytesRead: longint;
  Buffer       : array[1..BUF_SIZE] of byte;
  OutputString: String;
begin
  // Set up the process; as an example a recursive directory search is used
  // because that will usually result in a lot of data.
  if Assigned(Transcodes[Id].AProcess) then
    FreeAndNil(Transcodes[Id].AProcess);

  Transcodes[Id].AProcess := TProcess.Create(nil);

  mmEncode.Clear;
  Transcodes[Id].StdOutput.Clear;
  Transcodes[Id].StdErr.Clear;
  Transcodes[Id].Running := True;
  Transcodes[Id].Frames := 0;

  // The commands for Windows and *nix are different hence the $IFDEFs
  // In Windows the dir command cannot be used directly because it's a build-in
  // shell command. Therefore cmd.exe and the extra parameters are needed.
  Transcodes[Id].AProcess.Executable := 'MVCtranscode.exe';

  if (tabSettings.TabIndex = Integer(esQVBR)) then
  begin
    Transcodes[Id].AProcess.Parameters.Text := ' mvc -dots -i "' + Transcodes[Id].SrcFile + '" mvc -o "'
      + Transcodes[Id].DstFile + '" -qvbr ' + IntToStr(seQuality.Value)
      + ' -b ' + IntToStr(seBitrate.Value) + ' -MaxKbps ' + IntToStr(seMaxBitrate.Value);
  end
  else if (tabSettings.TabIndex = Integer(esCQP)) then
  begin
    Transcodes[Id].AProcess.Parameters.Text := ' mvc -dots -i "' + Transcodes[Id].SrcFile + '" mvc -o "'
      + Transcodes[Id].DstFile + '" -cqp ' + ' -qpi ' + IntToStr(seQuality.Value)
      + ' -qpp ' + IntToStr(seQuality.Value) + ' -qpb ' + IntToStr(seQuality.Value);
  end
  else if (tabSettings.TabIndex = Integer(esVBR)) then
  begin
    Transcodes[Id].AProcess.Parameters.Text := ' mvc -dots -i "' + Transcodes[Id].SrcFile + '" mvc -o "'
      + Transcodes[Id].DstFile + '" -vbr -b ' + IntToStr(seBitrate.Value) + ' -MaxKbps ' + IntToStr(seMaxBitrate.Value);
  end
  else
  begin
    Transcodes[Id].AProcess.Parameters.Text :=' mvc -dots -i "' + Transcodes[Id].SrcFile
      + '" mvc -o "' + Transcodes[Id].DstFile + '"';
  end;

  mmEncode.Lines.Add(Transcodes[Id].AProcess.Parameters.Text);

  // Process option poUsePipes has to be used so the output can be captured.
  // Process option poWaitOnExit can not be used because that would block
  // this program, preventing it from reading the output data of the process.
  Transcodes[Id].AProcess.Options := [poUsePipes, poNoConsole];

  // Start the process (run the dir/ls command)
  Transcodes[Id].AProcess.Execute;
end;

procedure TfrmMain.UpdateProgress(Id: integer);
const
  BUF_SIZE = 2048; // Buffer size for reading the output in chunks
var
  BytesRead: longint;
  Buffer : array[1..BUF_SIZE] of byte;
  OutputString: String;
begin
  if Transcodes[Id].Running then
  begin
    Transcodes[Id].Running := Transcodes[Id].AProcess.Running;

    // Create a stream object to store the generated output in. This could
    // also be a file stream to directly save the output to disk.
    while Transcodes[Id].AProcess.Output.NumBytesAvailable > 0 do
    begin
      BytesRead := Transcodes[Id].AProcess.Output.Read(Buffer, BUF_SIZE);
      SetString(OutputString, PAnsiChar(@Buffer[1]), BytesRead);
      mmEncode.Lines.Add(OutputString);
      Transcodes[Id].StdOutput.Add(OutputString);
    end;

    while Transcodes[Id].AProcess.Stderr.NumBytesAvailable > 0 do
    begin
      BytesRead := Transcodes[Id].AProcess.Stderr.Read(Buffer, 1);
      if (Buffer[1] = 46) then
      begin
        { There is a '.' add 20 frames to total. }
        Transcodes[Id].Frames := Transcodes[Id].Frames + 20;
      end
      else
      begin
        { There is a genuine error message. }
        SetString(OutputString, PAnsiChar(@Buffer[1]), BytesRead);
        mmEncode.Lines.Add(OutputString);
        Transcodes[Id].StdErr.Add(OutputString);

        { Read entire error message. }
        if (Transcodes[Id].AProcess.Stderr.NumBytesAvailable > 0) then
        begin
          BytesRead := Transcodes[Id].AProcess.Stderr.Read(Buffer, BUF_SIZE);
          SetString(OutputString, PAnsiChar(@Buffer[1]), BytesRead);
          mmEncode.Lines.Add(OutputString);
          Transcodes[Id].StdErr.Add(OutputString);
        end;
      end;
    end;

    Transcodes[Id].Running := Transcodes[Id].AProcess.Running;
  end;
end;

procedure TfrmMain.btnProcessFileClick(Sender: TObject);
var
  t: integer;
begin
  if (btnProcessFile.Tag = 0) then
  begin
    if dlgOpen.Execute and dlgSave.Execute then
    begin
      t := tabTitles.TabIndex;
      Transcodes[t].SrcFile := dlgOpen.FileName;
      Transcodes[t].DstFile := dlgSave.FileName;

      btnProcessFile.Caption := 'Abort Processing';
      btnProcessFile.Tag := 1;
      pbarEncoding.Visible := True;
      stxtFrames.Caption := '';
      EncodeFile(t);
    end;
  end
  else btnProcessFile.Tag := 2;
end;

procedure TfrmMain.btnAddTabClick(Sender: TObject);
begin

end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  t: integer;
begin
  { Terminate all running processes. }
  for t := 0 to High(Transcodes) do
  begin
    if Transcodes[t].Running then
        Transcodes[t].AProcess.Terminate(1);
  end;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  Running, t, Reply: integer;
begin
  Running := 0;

  for t := 0 to High(Transcodes) do
  begin
    if Transcodes[t].Running then
      Running := Running + 1;
  end;

  if (Running > 0) then
  begin
    if (Running > 1) then
      Reply := Application.MessageBox(PChar('There are ' + IntToStr(Running) + ' trasncodes active.'
        + LineEnding + 'Close Transcoder?'), 'Abort Transcodes?', MB_ICONQUESTION + MB_YESNO)
    else
      Reply := Application.MessageBox(PChar('There is 1 trasncode active.'
        + LineEnding + 'Close Transcoder?'), 'Abort Transcodes?', MB_ICONQUESTION + MB_YESNO);

    CanClose := (Reply = IDYES);
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  SetLength(Transcodes, 0);
  AddTranscode();
  AddTranscode();
end;

procedure TfrmMain.mnuAddTabClick(Sender: TObject);
begin
  AddTranscode();
end;

procedure TfrmMain.tabSettingsChange(Sender: TObject);
begin
  if (tabSettings.TabIndex = Integer(esQVBR)) then
  begin
    lblQuality.Enabled := true;
    seQuality.Enabled := true;
    lblBitrate.Enabled := true;
    seBitrate.Enabled := true;
    lblMaxBitrate.Enabled := true;
    seMaxBitrate.Enabled := true;
  end
  else if (tabSettings.TabIndex = Integer(esCQP)) then
  begin
    lblQuality.Enabled := true;
    seQuality.Enabled := true;
    lblBitrate.Enabled := false;
    seBitrate.Enabled := false;
    lblMaxBitrate.Enabled := false;
    seMaxBitrate.Enabled := false;
  end
  else if (tabSettings.TabIndex = Integer(esVBR)) then
  begin
    lblQuality.Enabled := false;
    seQuality.Enabled := false;
    lblBitrate.Enabled := true;
    seBitrate.Enabled := true;
    lblMaxBitrate.Enabled := true;
    seMaxBitrate.Enabled := true;
  end
  else
  begin
    lblQuality.Enabled := false;
    seQuality.Enabled := false;
    lblBitrate.Enabled := false;
    seBitrate.Enabled := false;
    lblMaxBitrate.Enabled := false;
    seMaxBitrate.Enabled := false;
  end
end;

procedure TfrmMain.tabTitlesChange(Sender: TObject);
var
  t: integer;
begin
  t := tabTitles.TabIndex;

  if (mmEncode.Tag >= 0) then
  begin
    mmEncode.Text := Transcodes[t].MemoText;
  end;

  Updateprogress(t);
end;

procedure TfrmMain.tabTitlesChanging(Sender: TObject; var AllowChange: Boolean);
var
  t: integer;
begin
  t := tabTitles.TabIndex;

  if (t >= 0) then
  begin
    Transcodes[t].MemoText := mmEncode.Text;
  end;
end;

procedure TfrmMain.tmrUpdateTimer(Sender: TObject);
var
  t: integer;
  OutputString: string;
begin
  for t := 0 to High(Transcodes) do
  begin
    UpdateProgress(t);

    if t = tabTitles.TabIndex then
    begin
      if btnProcessFile.Tag = 2 then
      begin
        Transcodes[t].AProcess.Terminate(1);
        OutputString := 'Processing ABORTED!';
        mmEncode.Lines.Add(OutputString);
        Transcodes[t].StdOutput.Add(OutputString);
      end;

      if Transcodes[t].Running then
      begin
        if (btnProcessFile.Tag <> 1) then
        begin
          btnProcessFile.Caption := 'Abort Processing';
          btnProcessFile.Tag := 1;
          pbarEncoding.Visible := True;
        end;
      end
      else
      begin
        if (btnProcessFile.Tag <> 0) then
        begin
          pbarEncoding.Visible := False;
          btnProcessFile.Tag := 0;
          btnProcessFile.Caption := 'Process File';
        end;
      end;

      stxtFrames.Caption := IntToStr(Transcodes[t].Frames);
    end;

    if Transcodes[t].Running then
       TabTitles.Tabs[t] := IntToStr(t+1) + ' ' + IntToStr(Transcodes[t].Frames)
    else
        TabTitles.Tabs[t] := IntToStr(t+1);
  end;
end;

function TfrmMain.AddTranscode: integer;
var
  t: integer;
begin
  t := Length(Transcodes);
  SetLength(Transcodes, t+1);

  tabTitles.Tabs.Add(IntToStr(t+1));

  Transcodes[t].AProcess := TProcess.Create(nil);
  Transcodes[t].Frames := 0;
  Transcodes[t].StdOutput := TStringList.Create;
  Transcodes[t].StdErr := TStringList.Create;
  Transcodes[t].SrcFile:='';
  Transcodes[t].DstFile:='';
  Transcodes[t].Running := False;
end;

end.

