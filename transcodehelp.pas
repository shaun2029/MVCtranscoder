unit TranscodeHelp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Process, LCLType;

type

  { TfrmMVCtranscode }

  TfrmMVCtranscode = class(TForm)
    mmTranscode: TMemo;
    procedure FormActivate(Sender: TObject);
  private
    procedure TranscodeHelp;

  public

  end;

var
  frmMVCtranscode: TfrmMVCtranscode;

implementation

{$R *.lfm}

{ TfrmMVCtranscode }

procedure TfrmMVCtranscode.TranscodeHelp;
const
  BUF_SIZE = 128; // Buffer size for reading the output in chunks
var
  AProcess : TProcess;
  OutputString: String;
  BytesRead, TimeOut: integer;
  Buffer : array[1..BUF_SIZE] of byte;
begin
  mmTranscode.Clear;
  AProcess := TProcess.Create(nil);

  try
    AProcess.Executable := 'MVCtranscode.exe';
    AProcess.Parameters.Text := '-help';
    AProcess.Options := [poUsePipes, poNoConsole];
    AProcess.Execute;

    BytesRead := 0;
    TimeOut := 50;

    while (TimeOut > 0) and ((AProcess.Output.NumBytesAvailable < 1)
      or (AProcess.Output.NumBytesAvailable <> BytesRead)) do
    begin
       BytesRead := AProcess.Output.NumBytesAvailable;
       Sleep(100);
       Dec(Timeout);
    end;

    while AProcess.Output.NumBytesAvailable > 0 do
    begin
      BytesRead := AProcess.Output.Read(Buffer, BUF_SIZE);
      SetString(OutputString, PAnsiChar(@Buffer[1]), BytesRead);
      mmTranscode.Text := mmTranscode.Text + OutputString;
    end;
  except
    on E: Exception do
    begin
       Application.MessageBox(PChar('Failed to execute "MVCtranscode.exe" please re-install MVCtranscoder.'
         + LineEnding + 'Error: ' + E.Message), 'Fatal Error', MB_ICONERROR);
       Application.Terminate;
    end;
  end;

  FreeAndNil(AProcess);
end;

procedure TfrmMVCtranscode.FormActivate(Sender: TObject);
begin
  TranscodeHelp;
end;

end.

