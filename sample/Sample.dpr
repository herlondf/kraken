program Sample;

uses
  Vcl.Forms,
  Main in 'Main.pas' {Form1},
  Kraken.Consts in '..\src\Kraken.Consts.pas',
  Kraken.Core in '..\src\Kraken.Core.pas',
  Kraken.Provider.Firedac.Metadata.Entity in '..\src\Kraken.Provider.Firedac.Metadata.Entity.pas',
  Kraken.Provider.Firedac.Metadata in '..\src\Kraken.Provider.Firedac.Metadata.pas',
  Kraken.Provider.Firedac in '..\src\Kraken.Provider.Firedac.pas',
  Kraken.Provider.Firedac.Query in '..\src\Kraken.Provider.Firedac.Query.pas',
  Kraken.Provider.Firedac.Settings in '..\src\Kraken.Provider.Firedac.Settings.pas',
  Kraken.Provider in '..\src\Kraken.Provider.pas',
  Kraken.Provider.Query in '..\src\Kraken.Provider.Query.pas',
  Kraken.Provider.Types in '..\src\Kraken.Provider.Types.pas',
  Kraken.Provider.Zeos in '..\src\Kraken.Provider.Zeos.pas',
  Kraken.Provider.Zeos.Query in '..\src\Kraken.Provider.Zeos.Query.pas',
  Kraken.Provider.Zeos.Settings in '..\src\Kraken.Provider.Zeos.Settings.pas',
  Kraken.Types in '..\src\Kraken.Types.pas',
  Kraken.Provider.RequestHTTP in '..\src\Kraken.Provider.RequestHTTP.pas',
  Kraken.Provider.RequestHTTP.Settings in '..\src\Kraken.Provider.RequestHTTP.Settings.pas',
  Kraken.Provider.RequestHTTP.Query in '..\src\Kraken.Provider.RequestHTTP.Query.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
