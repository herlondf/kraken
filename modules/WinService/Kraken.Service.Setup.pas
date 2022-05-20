unit Kraken.Service.Setup;

interface

uses
  System.SysUtils,
  System.Classes,
  Winapi.Windows,
  Winapi.WinSvc,
  Vcl.Forms,
  Vcl.SvcMgr,
  Kraken.Service.Manager,
  Kraken.Service.Setup.Contract;

type
  iKrakenServiceSetup = Kraken.Service.Setup.Contract.iKrakenServiceSetup;

  TServiceEvent  = Kraken.Service.Setup.Contract.TServiceEvent;
  TContinueEvent = Kraken.Service.Setup.Contract.TContinueEvent;
  TPauseEvent    = Kraken.Service.Setup.Contract.TPauseEvent;
  TStartEvent    = Kraken.Service.Setup.Contract.TStartEvent;
  TStopEvent     = Kraken.Service.Setup.Contract.TStopEvent;

  TKrakenServiceSetup = class(TInterfacedObject, iKrakenServiceSetup)
    constructor Create;
    destructor Destroy; override;
    class function New: iKrakenServiceSetup;
  private
    FEventAfterInstall      : TServiceEvent;
    FEventBeforeInstall     : TServiceEvent;
    FEventAfterUninstall    : TServiceEvent;
    FEventBeforeUninstall   : TServiceEvent;
    FEventServiceOnStart    : TStartEvent;
    FEventServiceOnStop     : TStopEvent;
    FEventServiceOnPause    : TPauseEvent;
    FEventServiceOnContinue : TContinueEvent;
    FEventServiceOnExecute  : TServiceEvent;
    FEventServiceOnDestroy  : TServiceEvent;

    FStartType               : TStartType;
    FName                    : PWideChar;
    FDisplayName             : PWideChar;
    FDescription             : PWideChar;
    FExecuteInterval         : Integer;
    FServiceStartName        : PWideChar;
    FServicePassword         : PWideChar;

    FGUIServiceStopped       : TComponentClass;
    FGUIServiceStoppedRef    : TComponent;

    FGUIServiceRunning       : TComponentClass;
    FGUIServiceRunningRef    : TComponent;
  public
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

  function KrakenServiceSetup: iKrakenServiceSetup;

  procedure WriteLog(ALog: String);

implementation

var
  FKrakenServiceSetup: iKrakenServiceSetup;

function KrakenServiceSetup: iKrakenServiceSetup;
begin
  if not Assigned(FKrakenServiceSetup) then
    FKrakenServiceSetup := TKrakenServiceSetup.New;
  Result := FKrakenServiceSetup;
end;

procedure WriteLog(ALog: String);
var
  fileName: string;
  FileLogName: string;
begin
  FileLogName := ExtractFilePath(GetModuleName(HInstance)) + 'sample.log';

  fileName := FileLogName;
  with TStringList.Create do
  try
    if FileExists(FileName) then
      LoadFromFile(fileName);

    Add(FormatDateTime('yyyy-MM-dd hh:mm:ss', now) + ' ' + ALog);
    SaveToFile(fileName);
  finally
    Free;
  end;
end;

{ TKrakenServiceSetup }

constructor TKrakenServiceSetup.Create;
begin
  FName             := 'KrakenServ';
  FDisplayName      := 'KrakenService';
  FDescription      := 'Windows service creation framework';
  FExecuteInterval  := 3000;
  FServiceStartName := '';
  FServicePassword  := '';
end;

destructor TKrakenServiceSetup.Destroy;
begin

  inherited;
end;

class function TKrakenServiceSetup.New: iKrakenServiceSetup;
begin
  Result := TKrakenServiceSetup.Create;
end;

function TKrakenServiceSetup.StartType(AStartType: TStartType): iKrakenServiceSetup;
begin
  Result := Self;
  FStartType := AStartType;
end;

function TKrakenServiceSetup.StartType: TStartType;
begin
  Result := FStartType;
end;

function TKrakenServiceSetup.Name(AName: PWideChar): iKrakenServiceSetup;
begin
  Result := Self;
  FName := AName;
end;

function TKrakenServiceSetup.Name: PWideChar;
begin
  Result := FName;
end;

function TKrakenServiceSetup.DisplayName(ADisplayName: PWideChar): iKrakenServiceSetup;
begin
  Result := Self;
  FDisplayName := ADisplayName;
end;

function TKrakenServiceSetup.DisplayName: PWideChar;
begin
  Result := FDisplayName;
end;

function TKrakenServiceSetup.Description(ADescription: PWideChar): iKrakenServiceSetup;
begin
  Result := Self;
  FDescription := ADescription;
end;

function TKrakenServiceSetup.Description: PWideChar;
begin
  Result := FDescription;
end;

function TKrakenServiceSetup.ServiceStartName(const AServiceStartName: PWideChar): iKrakenServiceSetup;
begin
  Result := Self;
  FServiceStartName := AServiceStartName;
end;

function TKrakenServiceSetup.ServiceStartName: PWideChar;
begin
  Result := FServiceStartName;
end;

function TKrakenServiceSetup.ServicePassword(const AServicePassword: PWideChar): iKrakenServiceSetup;
begin
  Result := Self;
  FServicePassword := AServicePassword;
end;

function TKrakenServiceSetup.ServicePassword: PWideChar;
begin
  Result := FServicePassword;
end;

function TKrakenServiceSetup.ExecuteInterval( AExecuteInterval: Integer ): iKrakenServiceSetup;
begin
  Result := Self;
  FExecuteInterval := AExecuteInterval;
end;

function TKrakenServiceSetup.ExecuteInterval: Integer;
begin
  Result := FExecuteInterval;
end;

function TKrakenServiceSetup.GUIServiceStopped(AForm: TComponentClass; var AReference): iKrakenServiceSetup;
begin
  Result := Self;
  FGUIServiceStopped := AForm;
  FGUIServiceStoppedRef := TComponent( AReference );
end;

function TKrakenServiceSetup.GUIServiceRunning(AForm: TComponentClass; var AReference): iKrakenServiceSetup;
begin
  Result := Self;
  FGUIServiceRunning := AForm;
  FGUIServiceRunningRef := TComponent( AReference );
end;

function TKrakenServiceSetup.AfterInstall(AServiceEvent: TServiceEvent): iKrakenServiceSetup;
begin
  Result := Self;
  FEventAfterInstall := AServiceEvent;
end;

function TKrakenServiceSetup.AfterInstall: TServiceEvent;
begin
  Result := nil;
  
  if Assigned( FEventAfterInstall ) then 
    Result := FEventAfterInstall;
end;

function TKrakenServiceSetup.BeforeInstall(AServiceEvent: TServiceEvent): iKrakenServiceSetup;
begin
  Result := Self;
  FEventBeforeInstall := AServiceEvent;
end;

function TKrakenServiceSetup.BeforeInstall: TServiceEvent;
begin
  Result := nil;
  
  if Assigned( FEventBeforeInstall ) then 
    Result := FEventBeforeInstall;
end;

function TKrakenServiceSetup.AfterUninstall(AServiceEvent: TServiceEvent): iKrakenServiceSetup;
begin
  Result := Self;
  FEventAfterUninstall := AServiceEvent;
end;

function TKrakenServiceSetup.AfterUninstall: TServiceEvent;
begin
  Result := nil;
  
  if Assigned( FEventAfterUninstall ) then 
    Result := FEventAfterUninstall;
end;

function TKrakenServiceSetup.BeforeUninstall(AServiceEvent: TServiceEvent): iKrakenServiceSetup;
begin
  Result := Self;
  FEventBeforeUninstall := AServiceEvent;
end;

function TKrakenServiceSetup.BeforeUninstall: TServiceEvent;
begin
  Result := nil;
  
  if Assigned( FEventBeforeUninstall ) then 
    Result := FEventBeforeUninstall;
end;

function TKrakenServiceSetup.OnStart(AServiceEvent: TStartEvent): iKrakenServiceSetup;
begin
  Result := Self;
  FEventServiceOnStart := AServiceEvent;
end;

function TKrakenServiceSetup.OnStart: TStartEvent;
begin
  Result := nil;
  
  if Assigned( FEventServiceOnStart ) then 
    Result := FEventServiceOnStart;
end;

function TKrakenServiceSetup.OnExecute(AServiceEvent : TServiceEvent): iKrakenServiceSetup;
begin
  Result := Self;
  FEventServiceOnExecute := AServiceEvent;
end;

function TKrakenServiceSetup.OnExecute: TServiceEvent;
begin
  Result := nil;
  
  if Assigned( FEventServiceOnExecute ) then 
    Result := FEventServiceOnExecute;
end;

function TKrakenServiceSetup.OnStop(AServiceEvent: TStopEvent): iKrakenServiceSetup;
begin
  Result := Self;
  FEventServiceOnStop := AServiceEvent;
end;

function TKrakenServiceSetup.OnStop: TStopEvent;
begin
  Result := nil;
  
  if Assigned( FEventServiceOnStop ) then 
    Result := FEventServiceOnStop;
end;

function TKrakenServiceSetup.OnPause(AServiceEvent: TPauseEvent): iKrakenServiceSetup;
begin
  Result := Self;
  FEventServiceOnPause := AServiceEvent;
end;

function TKrakenServiceSetup.OnPause: TPauseEvent;
begin
  Result := nil;
  
  if Assigned( FEventServiceOnPause ) then 
    Result := FEventServiceOnPause;
end;

function TKrakenServiceSetup.OnContinue(AServiceEvent : TContinueEvent): iKrakenServiceSetup;
begin
  Result := Self;
  FEventServiceOnContinue := AServiceEvent;
end;

function TKrakenServiceSetup.OnContinue: TContinueEvent;
begin
  Result := nil;
  
  if Assigned( FEventServiceOnContinue ) then 
    Result := FEventServiceOnContinue;
end;

function TKrakenServiceSetup.OnDestroy(AServiceEvent : TServiceEvent): iKrakenServiceSetup;
begin
  Result := Self;
  FEventServiceOnDestroy := AServiceEvent;
end;

function TKrakenServiceSetup.OnDestroy: TServiceEvent;
begin
  Result := nil;
  
  if Assigned( FEventServiceOnDestroy ) then 
    Result := FEventServiceOnDestroy;
end;

function TKrakenServiceSetup.Dependencies(AServiceDependencies: TServiceDependencies): iKrakenServiceSetup;
begin
  Result := Self;
end;

function TKrakenServiceSetup.Manager: TServiceManager;
begin
  Result := ServiceManager;
end;

procedure TKrakenServiceSetup.TryRunAsService;
begin
  if not ServiceManager.TryRunAsService(FName, FDisplayName) then
  begin
    if ServiceManager.ServiceRunning then
    begin
      Vcl.Forms.Application.Initialize;
      Vcl.Forms.Application.MainFormOnTaskbar := True;
      Vcl.Forms.Application.Title := FName;
      Vcl.Forms.Application.CreateForm(FGUIServiceRunning, FGUIServiceRunningRef);
      Vcl.Forms.Application.Run;
    end
    else
    begin      
      Vcl.Forms.Application.Initialize;
      Vcl.Forms.Application.MainFormOnTaskbar := True;
      Vcl.Forms.Application.Title := FName;
      Vcl.Forms.Application.CreateForm(FGUIServiceStopped, FGUIServiceStoppedRef);
      Vcl.Forms.Application.Run;
    end;
  end;
end;

end.
