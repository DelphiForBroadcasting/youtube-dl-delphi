unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Generics.Collections,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Memo, FMX.Edit, FH.YouTube.Decipher;

type
  TForm1 = class(TForm)
    StatusBar1: TStatusBar;
    GroupBox1: TGroupBox;
    Edit2: TEdit;
    Edit3: TEdit;
    Button4: TButton;
    procedure Button4Click(Sender: TObject);

  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.Button4Click(Sender: TObject);
var
  LScript : TStringStream;
begin
  LScript := TStringStream.Create;
  try
    LScript.LoadFromFile('player.js');
    edit3.Text := TYouTubeSig.Decipher(LScript.DataString, Edit2.Text);
  finally
    FreeAndNil(LScript);
  end;
end;


end.
