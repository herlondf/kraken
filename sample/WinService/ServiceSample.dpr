program ServiceSample;

uses
  Vcl.Forms,
  Vcl.SvcMgr,
  ServiceGUI in 'ServiceGUI.pas' {frmServiceGUI},
  Kraken.Setup.Contract in '..\Kraken.Setup.Contract.pas',
  Kraken.Setup in '..\Kraken.Setup.pas',
  Kraken.Service.Manager in '..\Kraken.Service.Manager.pas',
  Kraken.Service in '..\Kraken.Service.pas' {KrakenInstance: TService};

{$R *.res}

begin
  KrakenSetup
    .StartType(stAuto)
    .ServiceName('eSync')
    .DisplayName('eSync')
    .ServiceDetail('Servico de replicacao de banco de dados PostgreSQL')
    .ExecuteInterval(10000)
    .OnStart(
      procedure
      begin
        WriteLog('OnStart funcionando');
      end
    )
    .OnExecute(
      procedure
      begin
        WriteLog('OnExecute funcionando');
      end
    )
    .OnPause(
      procedure
      begin
        WriteLog('OnPause funcionando');
      end
    )
    .OnContinue(
      procedure
      begin
        WriteLog('OnContinue funcionando');
      end
    )
    .OnStop(
      procedure
      begin
        WriteLog('OnStop funcionando');
      end
    )
    .OnDestroy(
      procedure
      begin
        WriteLog('OnDestroy funcionando');
      end
    )
    .GUI(TfrmServiceGUI, frmServiceGUI)
    .TryRunAsService;
end.
