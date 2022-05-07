program CRUD;

uses
  Vcl.Forms,
  Main in '..\src\Main.pas' {frmSampleTimeOut},
  Kraken.Consts in '..\..\..\..\src\Kraken.Consts.pas',
  Kraken.Core in '..\..\..\..\src\Kraken.Core.pas',
  Kraken.JSON in '..\..\..\..\src\Kraken.JSON.pas',
  Kraken.Log in '..\..\..\..\src\Kraken.Log.pas',
  Kraken.Provider.Fields in '..\..\..\..\src\Kraken.Provider.Fields.pas',
  Kraken.Provider.Firedac.Metadata.Entity in '..\..\..\..\src\Kraken.Provider.Firedac.Metadata.Entity.pas',
  Kraken.Provider.Firedac.Metadata in '..\..\..\..\src\Kraken.Provider.Firedac.Metadata.pas',
  Kraken.Provider.Firedac in '..\..\..\..\src\Kraken.Provider.Firedac.pas',
  Kraken.Provider.Firedac.Query in '..\..\..\..\src\Kraken.Provider.Firedac.Query.pas',
  Kraken.Provider.Params in '..\..\..\..\src\Kraken.Provider.Params.pas',
  Kraken.Provider in '..\..\..\..\src\Kraken.Provider.pas',
  Kraken.Provider.Query in '..\..\..\..\src\Kraken.Provider.Query.pas',
  Kraken.Provider.RequestHTTP in '..\..\..\..\src\Kraken.Provider.RequestHTTP.pas',
  Kraken.Provider.RequestHTTP.Query in '..\..\..\..\src\Kraken.Provider.RequestHTTP.Query.pas',
  Kraken.Provider.Settings in '..\..\..\..\src\Kraken.Provider.Settings.pas',
  Kraken.Provider.Types in '..\..\..\..\src\Kraken.Provider.Types.pas',
  Kraken.Provider.Zeos in '..\..\..\..\src\Kraken.Provider.Zeos.pas',
  Kraken.Provider.Zeos.Query in '..\..\..\..\src\Kraken.Provider.Zeos.Query.pas',
  Kraken.Types in '..\..\..\..\src\Kraken.Types.pas',
  Kraken.Migrate in '..\..\..\..\src\Kraken.Migrate.pas',
  Kraken.Migrate.PG in '..\..\..\..\src\Kraken.Migrate.PG.pas',
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmSampleTimeOut, frmSampleTimeOut);
  Application.Run;
end.
