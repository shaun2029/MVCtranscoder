unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, Process;

type
  TTranscode = record
    AProcess : TProcess;
    StdOutput: TStringList;
    StdErr: TStringList;
    Frames : longint;
    SrcFile, DstFile: String;
    Running: boolean;
  end;

  { TfrmMain }

  TfrmMain = class(TForm)
    btnProcessFile: TButton;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    mmEncode: TMemo;
    pbarEncoding: TProgressBar;
    stxtFrames: TStaticText;
    tabTitles: TTabControl;
    tmrUpdate: TTimer;
    procedure btnProcessFileClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
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
  Transcodes[Id].AProcess.Parameters.Text:=' mvc -dots -i "' + Transcodes[Id].SrcFile + '" mvc -o "' + Transcodes[Id].DstFile + '"';

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
  Buffer       : array[1..BUF_SIZE] of byte;
  OutputString: String;
begin
  if Transcodes[Id].Running then
  begin
     Transcodes[Id].Running := Transcodes[Id].AProcess.Running;

    if btnProcessFile.Tag = 2 then
    begin
      Transcodes[Id].AProcess.Terminate(1);
      OutputString := 'Processing ABORTED!';
      mmEncode.Lines.Add(OutputString);
      Transcodes[Id].StdOutput.Add(OutputString);
    end;

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
      t := High(Transcodes);
      Transcodes[t].SrcFile := dlgOpen.FileName;
      Transcodes[t].DstFile := dlgSave.FileName;

      btnProcessFile.Caption := 'Abort Processing';
      btnProcessFile.Tag := 1;
      pbarEncoding.Visible := True;
      stxtFrames.Caption := '';
      EncodeFile(t);
{
      pbarEncoding.Visible := False;
      btnProcessFile.Tag := 0;
      btnProcessFile.Caption := 'Process File';
}
    end;
  end
  else btnProcessFile.Tag := 2;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  SetLength(Transcodes, 0);
  AddTranscode();
end;

procedure TfrmMain.tmrUpdateTimer(Sender: TObject);
var
  t: integer;
begin
  t := tabTitles.TabIndex;

  if (t >= 0) then
  begin
    UpdateProgress(t);
    stxtFrames.Caption := IntToStr(Transcodes[t].Frames);
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

