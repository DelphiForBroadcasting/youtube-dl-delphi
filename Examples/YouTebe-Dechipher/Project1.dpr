program Project1;

uses
  System.StartUpCopy,
  FMX.Forms,

  jsDbgServer  in '..\..\Include\delphi-javascript\jsDbgServer.pas',
  jsintf in '..\..\Include\delphi-javascript\jsintf.pas',
  js15decl in '..\..\Include\delphi-javascript\js15decl.pas',
  NamedPipesImpl in '..\..\Include\delphi-javascript\NamedPipesImpl.pas',

  FH.YouTube.Decipher in '..\..\Source\FH.YouTube.Decipher.pas',

  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
