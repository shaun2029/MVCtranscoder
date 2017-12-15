unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, Process, LCLType, Menus, Spin, Buttons, ActnList,
  fileinfo, winpeimagereader, elfreader, machoreader, about;

ResourceString
  rsQualityContr = 'Quality controlled variable bitrate, quality in range [11,'
    +'51] where 11 is the highest quality (17 to 20 recommended). This '
    +'algorithm tries to achieve the subjective quality with minimum no. of '
    +'bits. Bitrate and max bitrate settings are used by QVBR to limit the '
    +'maximum output bitrate. This limits the effect of high motion scenes on '
    +'the output file size. Bitrates are doubled for MVC encoding. '
    +'%sNOTE: QVBR is supported from 4th generation Intel'
    +' Core processor(codename Haswell) onward.';
  rsConstantQuan = 'Constant quantization parameter (CQP BRC) bitrate control '
    +'method, quality in range [11,51] where 11 is the highest quality, 19 to '
    +'22 recommended. NOTE: CQP can produce large files with quality values of'
    +' 18 and below. This depends on the quantity of high motion content.';
  rsVariableBitr = 'Variable bitrate control. This method allows more bitrate '
    +'variations to match complexity of the inputs i.e. assign higher bit rate'
    +' to complex scenarios like scene change and low bitrate to less complex '
    +'scenarios. It tries to achieve a smaller overall file size, but also '
    +'gives unpredictable spikes. This method provides an overall better '
    +'bitrate quality by allowing bitrate to fluctuate more and also by '
    +'disable padding. Bitrates are doubled for MVC encoding.';
  rsIfEncoderHar = 'If encoder hardware acceleration is enabled QVBR is used '
    +'else VBR is used. QVBR is used with a quality of 17. A bitrate of 10000'
    +'Kbps and max bitrate of 20000Kbps. Bitrates are doubled for MVC encoding.';

type
  TEncSettings = (esAuto, esCQP, esVBR, esQVBR);

  TTranscode = record
    AProcess : TProcess;
    StdOutput: TStringList;
    Frames : longint;
    SrcFile, DstFile: array [0..1] of String;
    MemoText: string;
    Running: boolean;
    StartTime: qword;
    Fps: integer;
  end;

  { TfrmMain }

  TfrmMain = class(TForm)
    btnInputFile: TBitBtn;
    btnInputFileRemove: TBitBtn;
    btnOutputFile: TBitBtn;
    btnOutputFileRemove: TBitBtn;
    btnProcessFile: TButton;
    cbxInputHw: TCheckBox;
    cbxOutputHw: TCheckBox;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    edtDecoderCmd: TEdit;
    edtEncoderCMD: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    GroupBox6: TGroupBox;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    lblBitrate: TLabel;
    lblMaxBitrate: TLabel;
    lblQuality: TLabel;
    lbxInputFiles: TListBox;
    lbxOutputFiles: TListBox;
    MenuItem1: TMenuItem;
    mnuAbout: TMenuItem;
    mnuMain: TMainMenu;
    mnuAddFile1: TMenuItem;
    mnuOutputFiles: TPopupMenu;
    mnuRemoveFile1: TMenuItem;
    mnuSwitchFiles: TMenuItem;
    mnuRemoveFile: TMenuItem;
    mnuAddFile: TMenuItem;
    mnuAddTab: TMenuItem;
    mmEncode: TMemo;
    mnuSwitchFiles1: TMenuItem;
    Panel1: TPanel;
    mnuTabs: TPopupMenu;
    mnuInputFiles: TPopupMenu;
    rgrpOutputCodec: TRadioGroup;
    rgrpInputCodec: TRadioGroup;
    seBitrate: TSpinEdit;
    seMaxBitrate: TSpinEdit;
    seQuality: TSpinEdit;
    stxtBrcInfo: TStaticText;
    stxtFrames: TStaticText;
    stxtFps: TStaticText;
    tabSettings: TTabControl;
    tabTitles: TTabControl;
    tmrUpdate: TTimer;
    tbarSpeed: TTrackBar;
    procedure btnAddTabClick(Sender: TObject);
    procedure btnInputFileClick(Sender: TObject);
    procedure btnInputFileRemoveClick(Sender: TObject);
    procedure btnOutputFileClick(Sender: TObject);
    procedure btnOutputFileRemoveClick(Sender: TObject);
    procedure btnProcessFileClick(Sender: TObject);
    procedure cbxOutputHwChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure lbxInputFilesDblClick(Sender: TObject);
    procedure lbxOutputFilesDblClick(Sender: TObject);
    procedure mnuAboutClick(Sender: TObject);
    procedure mnuAddFileClick(Sender: TObject);
    procedure mnuAddTabClick(Sender: TObject);
    procedure mnuRemoveFileClick(Sender: TObject);
    procedure mnuSwitchFilesClick(Sender: TObject);
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
  InputCodec: String;
  OutputCodec, SrcFiles, DstFiles: String;
  Multiplier: double;
begin
  // Set up the process; as an example a recursive directory search is used
  // because that will usually result in a lot of data.
  if Assigned(Transcodes[Id].AProcess) then
    FreeAndNil(Transcodes[Id].AProcess);

  Transcodes[Id].AProcess := TProcess.Create(nil);

  mmEncode.Clear;
  Transcodes[Id].Running := True;
  Transcodes[Id].Frames := 0;

  // The commands for Windows and *nix are different hence the $IFDEFs
  // In Windows the dir command cannot be used directly because it's a build-in
  // shell command. Therefore cmd.exe and the extra parameters are needed.
  Transcodes[Id].AProcess.Executable := 'MVCtranscode.exe';

  if rgrpInputCodec.ItemIndex = 0 then
  begin
    InputCodec := 'mvc';
  end
  else if rgrpInputCodec.ItemIndex = 1 then
  begin
    InputCodec := 'h264';
  end
  else if rgrpInputCodec.ItemIndex = 2 then
  begin
    InputCodec := 'mpeg2';
  end
  else if rgrpInputCodec.ItemIndex = 2 then
  begin
    InputCodec := 'vc1';
  end;

  if rgrpOutputCodec.ItemIndex = 0 then
  begin
    OutputCodec := 'mvc';
    Multiplier := 2.0;
  end
  else if rgrpInputCodec.ItemIndex = 1 then
  begin
    OutputCodec := 'h264';
    Multiplier := 1.0;
  end
  else
  begin
    OutputCodec := 'mpeg2';
    Multiplier := 1.0;
  end;

  if cbxInputHw.Checked then
  begin
    InputCodec := InputCodec + ' -hw';
  end
  else
  begin
    InputCodec := InputCodec + ' -sw';
  end;

  if cbxOutputHw.Checked then
  begin
    OutputCodec := OutputCodec + ' -hw';
  end
  else
  begin
    OutputCodec := OutputCodec + ' -sw';
  end;

  OutputCodec := OutputCodec + ' -u '
    + IntToStr(tbarSpeed.Position) + ' ';

  if Length(Trim(edtDecoderCMD.Text)) > 0 then
     InputCodec := InputCodec + ' ' + Trim(edtDecoderCMD.Text) + ' ';

  if Length(Trim(edtEncoderCMD.Text)) > 0 then
    OutputCodec := OutputCodec + ' ' + Trim(edtEncoderCMD.Text) + ' ';

  SrcFiles := ' -dots -i "' + Transcodes[Id].SrcFile[0] + '"';
  if (Transcodes[Id].SrcFile[1] <> '') then
    SrcFiles := SrcFiles + ' -i "' + Transcodes[Id].SrcFile[1] + '"';

  DstFiles := ' -o "' + Transcodes[Id].DstFile[0] + '"';
  if (Transcodes[Id].DstFile[1] <> '') then
    DstFiles := ' -viewoutput ' + DstFiles + ' -o "' + Transcodes[Id].DstFile[1] + '"';

  if (tabSettings.TabIndex = Integer(esQVBR)) then
  begin
    Transcodes[Id].AProcess.Parameters.Text := ' ' + InputCodec + SrcFiles + ' ' + OutputCodec
      + DstFiles + ' -qvbr ' + IntToStr(seQuality.Value)
      + ' -b ' + IntToStr(Round(seBitrate.Value * Multiplier))
      + ' -MaxKbps ' + IntToStr(Round(seMaxBitrate.Value * Multiplier));
  end
  else if (tabSettings.TabIndex = Integer(esCQP)) then
  begin
    Transcodes[Id].AProcess.Parameters.Text := ' ' + InputCodec + SrcFiles + ' ' + OutputCodec
      + DstFiles + ' -cqp ' + ' -qpi ' + IntToStr(seQuality.Value)
      + ' -qpp ' + IntToStr(seQuality.Value) + ' -qpb ' + IntToStr(seQuality.Value);
  end
  else if (tabSettings.TabIndex = Integer(esVBR)) then
  begin
    Transcodes[Id].AProcess.Parameters.Text := ' ' + InputCodec + SrcFiles + ' ' + OutputCodec
      + DstFiles + ' -vbr '
      + ' -b ' + IntToStr(Round(seBitrate.Value * Multiplier))
      + ' -MaxKbps ' + IntToStr(Round(seMaxBitrate.Value * Multiplier));
  end
  else
  begin
    Transcodes[Id].AProcess.Parameters.Text := ' ' + InputCodec + SrcFiles + ' ' + OutputCodec + DstFiles;
  end;

  // Process option poUsePipes has to be used so the output can be captured.
  // Process option poWaitOnExit can not be used because that would block
  // this program, preventing it from reading the output data of the process.
  Transcodes[Id].AProcess.Options := [poUsePipes, poNoConsole];

  // Start the process (run the dir/ls command)
  Transcodes[Id].StdOutput.Add(Transcodes[Id].AProcess.Parameters.Text);
  Transcodes[Id].AProcess.Execute;
  Transcodes[Id].StartTime := GetTickCount64();
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
        Transcodes[Id].StdOutput.Add(OutputString);

        { Read entire error message. }
        if (Transcodes[Id].AProcess.Stderr.NumBytesAvailable > 0) then
        begin
          BytesRead := Transcodes[Id].AProcess.Stderr.Read(Buffer, BUF_SIZE);
          SetString(OutputString, PAnsiChar(@Buffer[1]), BytesRead);
          Transcodes[Id].StdOutput.Add(OutputString);
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
    t := tabTitles.TabIndex;

    if (lbxInputFiles.Count = 0) then
    begin
      ShowMessage('Please select an input file.');
    end
    else if (lbxOutputFiles.Count = 0) then
    begin
      ShowMessage('Please select an output file.');
    end
    else if not FileExists(lbxInputFiles.Items[0]) then
    begin
      ShowMessage('The input file could not be found:' + LineEnding
      + lbxInputFiles.Items[0] + LineEnding + 'Please select a valid input file!');
    end
    else if (lbxInputFiles.Count > 1) and not FileExists(lbxInputFiles.Items[1]) then
    begin
      ShowMessage('The input file could not be found:' + LineEnding
      + lbxInputFiles.Items[1] + LineEnding + 'Please select a valid input file!');
    end
    else if (Pos(lbxOutputFiles.Items[0], lbxInputFiles.Items.Text) > 0)
      or ((lbxOutputFiles.Count > 1) and (Pos(lbxOutputFiles.Items[1], lbxInputFiles.Items.Text) > 0)) then
    begin
      ShowMessage('The input and output files can''t be the same file!');
    end
    else
    begin
      t := tabTitles.TabIndex;
      Transcodes[t].StdOutput.Clear;

      { Configure the input and output files.
        There acn be upto 2 input and upto 2 output files. }
      Transcodes[t].SrcFile[0] := lbxInputFiles.Items[0];
      if (lbxInputFiles.Count > 1) then
      begin
        Transcodes[t].SrcFile[1] := lbxInputFiles.Items[1];
      end
      else Transcodes[t].SrcFile[1] := '';

      Transcodes[t].DstFile[0] := lbxOutputFiles.Items[0];
      if (lbxOutputFiles.Count > 1) then
      begin
        Transcodes[t].DstFile[1] := lbxOutputFiles.Items[1];
      end
      else Transcodes[t].DstFile[1] := '';


      btnProcessFile.Caption := 'Abort Processing';
      btnProcessFile.Tag := 1;
      stxtFrames.Caption := '';
      EncodeFile(t);
    end;
  end
  else btnProcessFile.Tag := 2;
end;

procedure TfrmMain.cbxOutputHwChange(Sender: TObject);
begin
  if cbxOutputHw.Checked then
  begin
    if tabSettings.Tabs.Count < 4 then
    begin
      tabSettings.Tabs.Add('QVBR');
    end;
  end
  else
  begin
    if tabSettings.Tabs.Count > 3 then
    begin
      tabSettings.Tabs.Delete(3);
    end;
  end;
end;

procedure TfrmMain.btnAddTabClick(Sender: TObject);
begin

end;

procedure TfrmMain.btnInputFileClick(Sender: TObject);
begin
  if dlgOpen.Execute then
  begin
    if dlgOpen.InitialDir = '' then
       dlgOpen.InitialDir := dlgSave.InitialDir;

    if (lbxInputFiles.Count > 1) then
       lbxInputFiles.Items[1] := Trim(dlgOpen.FileName)
    else
      lbxInputFiles.AddItem(Trim(dlgOpen.FileName), Nil);
  end;
end;

procedure TfrmMain.btnInputFileRemoveClick(Sender: TObject);
begin
    mnuRemoveFileClick(Sender);
end;

procedure TfrmMain.btnOutputFileClick(Sender: TObject);
begin
  if dlgSave.Execute then
  begin
    if dlgSave.InitialDir = '' then
       dlgSave.InitialDir := dlgOpen.InitialDir;

    if (lbxOutputFiles.Count > 1) then
       lbxOutputFiles.Items[1] := Trim(dlgSave.FileName)
    else
      lbxOutputFiles.AddItem(Trim(dlgSave.FileName), Nil);
  end;
end;

procedure TfrmMain.btnOutputFileRemoveClick(Sender: TObject);
begin
  mnuRemoveFileClick(Sender);
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
var
  FileVerInfo: TFileVersionInfo;
begin
  FileVerInfo:=TFileVersionInfo.Create(nil);
  try
    FileVerInfo.ReadFileInfo;
    Self.Caption := 'MVCtranscode ' + FileVerInfo.VersionStrings.Values['ProductVersion'];
  finally
  end;

  SetLength(Transcodes, 0);
  AddTranscode();
  AddTranscode();
  stxtBrcInfo.Caption := rsIfEncoderHar;
end;

procedure TfrmMain.lbxInputFilesDblClick(Sender: TObject);
var
  Index: integer;
begin
  Index := lbxInputFiles.ItemIndex;

  if dlgOpen.Execute then
  begin
    if dlgOpen.InitialDir = '' then
       dlgOpen.InitialDir := dlgSave.InitialDir;

    if (Index < 0) and (lbxInputFiles.Items.Count = 0) then
      lbxInputFiles.AddItem(Trim(dlgOpen.FileName), Nil)
    else if (Index < 0) then
      lbxInputFiles.Items[0] := Trim(dlgOpen.FileName)
    else
      lbxInputFiles.Items[Index] := Trim(dlgOpen.FileName);
  end;
end;

procedure TfrmMain.lbxOutputFilesDblClick(Sender: TObject);
var
  Index: integer;
begin
  Index := lbxOutputFiles.ItemIndex;

  if dlgSave.Execute then
  begin
    if dlgSave.InitialDir = '' then
       dlgSave.InitialDir := dlgOpen.InitialDir;

    if (Index < 0) and (lbxOutputFiles.Items.Count = 0) then
      lbxOutputFiles.AddItem(Trim(dlgSave.FileName), Nil)
    else if (Index < 0) then
      lbxOutputFiles.Items[0] := Trim(dlgOpen.FileName)
    else
      lbxOutputFiles.Items[Index] := Trim(dlgSave.FileName);
  end;
end;

procedure TfrmMain.mnuAboutClick(Sender: TObject);
begin
  frmAbout.ShowModal;
end;

procedure TfrmMain.mnuAddFileClick(Sender: TObject);
begin
  if Sender is TComponent then
  begin
    if (TComponent(Sender).Tag = 0) then
      btnInputFileClick(nil)
    else
      btnOutputFileClick(nil);
  end;
end;

procedure TfrmMain.mnuAddTabClick(Sender: TObject);
begin
  AddTranscode();
end;

procedure TfrmMain.mnuRemoveFileClick(Sender: TObject);
var
  List: TListBox;
begin
  if (TComponent(Sender).Tag = 0) then
    List := lbxInputFiles
  else
    List := lbxOutputFiles;

  if (List.Count > 0) then
  begin
    if (List.ItemIndex >= 0) then
      List.Items.Delete(List.ItemIndex)
    else
      List.Items.Delete(List.Count-1);
  end
end;

procedure TfrmMain.mnuSwitchFilesClick(Sender: TObject);
var
  List: TListBox;
  Filename: string;
begin
  if (TComponent(Sender).Tag = 0) then
    List := lbxInputFiles
  else
    List := lbxOutputFiles;

  if (List.Count > 1) then
  begin
    Filename := List.Items.Strings[0];
    List.Items.Strings[0] := List.Items.Strings[1];
    List.Items.Strings[1] := Filename;
  end
end;

procedure TfrmMain.tabSettingsChange(Sender: TObject);
begin
  if (tabSettings.TabIndex = Integer(esQVBR)) then
  begin
    if cbxOutputHw.Checked then
    begin
      lblQuality.Enabled := true;
      seQuality.Enabled := true;
      lblBitrate.Enabled := true;
      seBitrate.Enabled := true;
      lblMaxBitrate.Enabled := true;
      seMaxBitrate.Enabled := true;
    end
    else
    begin
      lblQuality.Enabled := true;
      seQuality.Enabled := true;
      lblBitrate.Enabled := false;
      seBitrate.Enabled := false;
      lblMaxBitrate.Enabled := false;
      seMaxBitrate.Enabled := false;
    end;

    stxtBrcInfo.Caption := Format(rsQualityContr, [LineEnding])
  end
  else if (tabSettings.TabIndex = Integer(esCQP)) then
  begin
    lblQuality.Enabled := true;
    seQuality.Enabled := true;
    lblBitrate.Enabled := false;
    seBitrate.Enabled := false;
    lblMaxBitrate.Enabled := false;
    seMaxBitrate.Enabled := false;

    stxtBrcInfo.Caption := rsConstantQuan
  end
  else if (tabSettings.TabIndex = Integer(esVBR)) then
  begin
    lblQuality.Enabled := false;
    seQuality.Enabled := false;
    lblBitrate.Enabled := true;
    seBitrate.Enabled := true;
    lblMaxBitrate.Enabled := true;
    seMaxBitrate.Enabled := true;

    stxtBrcInfo.Caption := rsVariableBitr
  end
  else
  begin
    lblQuality.Enabled := false;
    seQuality.Enabled := false;
    lblBitrate.Enabled := false;
    seBitrate.Enabled := false;
    lblMaxBitrate.Enabled := false;
    seMaxBitrate.Enabled := false;

    stxtBrcInfo.Caption := rsIfEncoderHar
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
  Ticks: qword;
  Fps: integer;
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
        Transcodes[t].StdOutput.Add(OutputString);
      end;

      if Transcodes[t].StdOutput.Text <> mmEncode.Lines.Text then
         mmEncode.Text := Transcodes[t].StdOutput.Text;

      if Transcodes[t].Running then
      begin
        if (btnProcessFile.Tag <> 1) then
        begin
          btnProcessFile.Caption := 'Abort Processing';
          btnProcessFile.Tag := 1;
        end;

        Ticks := GetTickCount64;
        if (Ticks - Transcodes[t].StartTime > 1000) then
          Fps := (Transcodes[t].Frames * 1000) div (Ticks - Transcodes[t].StartTime)
        else
          Fps := 0;

        Transcodes[t].Fps := Fps;
      end
      else
      begin
        if (btnProcessFile.Tag <> 0) then
        begin
          btnProcessFile.Tag := 0;
          btnProcessFile.Caption := 'Process File';
        end;
      end;

      stxtFrames.Caption := IntToStr(Transcodes[t].Frames);
      stxtFps.Caption := IntToStr(Transcodes[t].Fps);
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
  Transcodes[t].Fps := 0;
  Transcodes[t].StdOutput := TStringList.Create;
  Transcodes[t].SrcFile[0]:='';
  Transcodes[t].SrcFile[1]:='';
  Transcodes[t].DstFile[0]:='';
  Transcodes[t].DstFile[1]:='';
  Transcodes[t].Running := False;

  Result := 0;
end;

end.

