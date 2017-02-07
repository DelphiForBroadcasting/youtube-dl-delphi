program js_tests;

uses
  Vcl.Forms,
  Unit12 in 'Unit12.pas' {Form12},
  js15decl in '..\js15decl.pas',
  jsintf in '..\jsintf.pas',
  NamedPipesImpl in '..\NamedPipesImpl.pas',
  jsDbgServer in '..\jsDbgServer.pas';

{$R *.res}

begin
 // ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm12, Form12);
  Application.Run;
end.
