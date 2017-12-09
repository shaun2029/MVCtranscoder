unit about;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, lclintf;

type

  { TfrmAbout }

  TfrmAbout = class(TForm)
    GroupBox1: TGroupBox;
    Image1: TImage;
    Label1: TLabel;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    procedure Label1Click(Sender: TObject);
    procedure Label1MouseEnter(Sender: TObject);
    procedure Label1MouseLeave(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmAbout: TfrmAbout;

implementation

{$R *.lfm}

{ TfrmAbout }

procedure TfrmAbout.Label1Click(Sender: TObject);
begin
    OpenDocument(Label1.Caption);
end;

procedure TfrmAbout.Label1MouseEnter(Sender: TObject);
begin
  Label1.Font.Color := clHotLight;
end;

procedure TfrmAbout.Label1MouseLeave(Sender: TObject);
begin
  Label1.Font.Color := clDefault;
end;


end.

