program KrakenThreadProject;

uses
  Vcl.Forms,
  Thread.View in '..\src\Thread.View.pas' {Form1},
  KrakenThread in '..\..\..\src\Thread\KrakenThread.pas',
  KrakenThreadAnimation in '..\..\..\src\Thread\KrakenThreadAnimation.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
