unit Kraken.Service.Setup.Contract;

interface

uses
  Vcl.SvcMgr,
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  Kraken.Service.Manager;

type
  TService             = Vcl.SvcMgr.TService;
  TServiceDependencies = TDictionary<string, string>;
  TStartType           = Vcl.SvcMgr.TStartType;

  TServiceEvent  = reference to procedure ;
  TContinueEvent = reference to procedure ;
  TPauseEvent    = reference to procedure ;
  TStartEvent    = reference to procedure ;
  TStopEvent     = reference to procedure ;  

  iKrakenServiceSetup = interface
    ['{99A0BFAF-2B2D-4132-82E3-7CC2447DD316}']
    function StartType( AStartType: TStartType ): iKrakenServiceSetup; overload;
    function StartType: TStartType; overload;

    function Name( AName: PWideChar ): iKrakenServiceSetup; overload;
    function Name: PWideChar; overload;

    function DisplayName( ADisplayName: PWideChar ): iKrakenServiceSetup; overload;
    function DisplayName: PWideChar; overload;

    function Description( ADescription: PWideChar ): iKrakenServiceSetup; overload;
    function Description: PWideChar; overload;

    function ExecuteInterval( AExecuteInterval: Integer ): iKrakenServiceSetup; overload;
    function ExecuteInterval: Integer; overload;

    function GUIServiceStopped( AForm: TComponentClass; var AReference ): iKrakenServiceSetup;
    function GUIServiceRunning( AForm: TComponentClass; var AReference ): iKrakenServiceSetup;

    function ServiceStartName( const AServiceStartName: PWideChar ): iKrakenServiceSetup; overload;
    function ServiceStartName: PWideChar; overload;

    function ServicePassword ( const AServicePassword: PWideChar ): iKrakenServiceSetup; overload;
    function ServicePassword: PWideChar; overload;

    function AfterInstall( AServiceEvent: TServiceEvent ): iKrakenServiceSetup; overload;
    function AfterInstall: TServiceEvent; overload;

    function BeforeInstall( AServiceEvent: TServiceEvent ): iKrakenServiceSetup; overload;
    function BeforeInstall: TServiceEvent; overload;

    function AfterUninstall( AServiceEvent: TServiceEvent ): iKrakenServiceSetup; overload;
    function AfterUninstall: TServiceEvent; overload;

    function BeforeUninstall( AServiceEvent: TServiceEvent ): iKrakenServiceSetup; overload;
    function BeforeUninstall: TServiceEvent; overload;

    function OnStart( AServiceEvent: TStartEvent ): iKrakenServiceSetup; overload;
    function OnStart: TStartEvent; overload;

    function OnStop( AServiceEvent: TStopEvent ): iKrakenServiceSetup; overload;
    function OnStop: TStopEvent; overload;

    function OnPause( AServiceEvent: TPauseEvent ): iKrakenServiceSetup; overload;
    function OnPause: TPauseEvent; overload;

    function OnContinue( AServiceEvent : TContinueEvent ): iKrakenServiceSetup; overload;
    function OnContinue: TContinueEvent; overload;

    function OnExecute( AServiceEvent : TServiceEvent ): iKrakenServiceSetup; overload;
    function OnExecute: TServiceEvent; overload;

    function OnDestroy( AServiceEvent : TServiceEvent ): iKrakenServiceSetup; overload;
    function OnDestroy: TServiceEvent; overload;

    function Dependencies( AServiceDependencies: TServiceDependencies ): iKrakenServiceSetup;

    function Manager: TServiceManager;

    procedure TryRunAsService;
  end;
  
implementation

end.
