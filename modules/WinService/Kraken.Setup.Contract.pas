unit Kraken.Setup.Contract;

interface

uses
  Vcl.SvcMgr,
  System.Classes,
  System.Generics.Collections,
  Kraken.Service.Manager;

type
  TService             = Vcl.SvcMgr.TService;
  TServiceDependencies = TDictionary<string, string>;
  TStartType           = Vcl.SvcMgr.TStartType;

  TServiceEvent = reference to procedure;

  iSetupParam = interface
    ['{99A0BFAF-2B2D-4132-82E3-7CC2447DD316}']
    function StartType( AStartType: TStartType ): iSetupParam; overload;
    function StartType: TStartType; overload;

    function Name( AName: PWideChar ): iSetupParam; overload;
    function Name: PWideChar; overload;

    function DisplayName( ADisplayName: PWideChar ): iSetupParam; overload;
    function DisplayName: PWideChar; overload;

    function Description( ADescription: PWideChar ): iSetupParam; overload;
    function Description: PWideChar; overload;

    function ExecuteInterval( AExecuteInterval: Integer ): iSetupParam; overload;
    function ExecuteInterval: Integer; overload;

    function GUIServiceStopped( AForm: TComponentClass; var AReference ): iSetupParam;
    function GUIServiceRunning( AForm: TComponentClass; var AReference ): iSetupParam;

    function ServiceStartName( const AServiceStartName: PWideChar ): iSetupParam; overload;
    function ServiceStartName: PWideChar; overload;

    function ServicePassword ( const AServicePassword: PWideChar ): iSetupParam; overload;
    function ServicePassword: PWideChar; overload;

    function AfterInstall( AServiceEvent: TServiceEvent ): iSetupParam; overload;
    function AfterInstall: iSetupParam; overload;

    function BeforeInstall( AServiceEvent: TServiceEvent ): iSetupParam; overload;
    function BeforeInstall: iSetupParam; overload;

    function AfterUninstall( AServiceEvent: TServiceEvent ): iSetupParam; overload;
    function AfterUninstall: iSetupParam; overload;

    function BeforeUninstall( AServiceEvent: TServiceEvent ): iSetupParam; overload;
    function BeforeUninstall: iSetupParam; overload;

    function OnStart( AServiceEvent: TServiceEvent ): iSetupParam; overload;
    function OnStart: iSetupParam; overload;

    function OnStop( AServiceEvent: TServiceEvent ): iSetupParam; overload;
    function OnStop: iSetupParam; overload;

    function OnPause( AServiceEvent: TServiceEvent ): iSetupParam; overload;
    function OnPause: iSetupParam; overload;

    function OnContinue( AServiceEvent : TServiceEvent ): iSetupParam; overload;
    function OnContinue: iSetupParam; overload;

    function OnExecute( AServiceEvent : TServiceEvent ): iSetupParam; overload;
    function OnExecute: iSetupParam; overload;

    function OnDestroy( AServiceEvent : TServiceEvent ): iSetupParam; overload;
    function OnDestroy: iSetupParam; overload;

    function Dependencies( AServiceDependencies: TServiceDependencies ): iSetupParam;

    function Manager: TServiceManager;

    procedure TryRunAsService;
  end;

implementation

end.
