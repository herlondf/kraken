unit Kraken.Setup;

interface

uses
  System.SysUtils,
  System.Classes,
  Winapi.Windows,
  Winapi.WinSvc,
  Vcl.Forms,
  Vcl.SvcMgr,
  Kraken.Service.Manager,
  Kraken.Setup.Contract;

type
  iSetupParam = Kraken.Setup.Contract.iSetupParam;

  TSetupParam = class(TInterfacedObject, iSetupParam)
    constructor Create;
    destructor Destroy; override;
    class function New: iSetupParam;
  private
    FEventAfterInstall      : TServiceEvent;
    FEventBeforeInstall     : TServiceEvent;
    FEventAfterUninstall    : TServiceEvent;
    FEventBeforeUninstall   : TServiceEvent;
    FEventServiceOnStart    : TServiceEvent;
    FEventServiceOnStop     : TServiceEvent;
    FEventServiceOnPause    : TServiceEvent;
    FEventServiceOnContinue : TServiceEvent;
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

  function KrakenSetup: iSetupParam;

  procedure WriteLog(ALog: String);

implementation

uses
  Kraken.Service;

var
  FSetupParam: iSetupParam;

function KrakenSetup: iSetupParam;
begin
  if not Assigned(FSetupParam) then
    FSetupParam := TSetupParam.New;
  Result := FSetupParam;
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

{ TSetupParam }

constructor TSetupParam.Create;
begin
  FName             := 'KrakenServ';
  FDisplayName      := 'KrakenService';
  FDescription      := 'Windows service creation framework';
  FExecuteInterval  := 3000;
  FServiceStartName := '';
  FServicePassword  := '';
end;

destructor TSetupParam.Destroy;
begin

  inherited;
end;

class function TSetupParam.New: iSetupParam;
begin
  Result := TSetupParam.Create;
end;

function TSetupParam.StartType(AStartType: TStartType): iSetupParam;
begin
  Result := Self;
  FStartType := AStartType;
end;

function TSetupParam.StartType: TStartType;
begin
  Result := FStartType;
end;

function TSetupParam.Name(AName: PWideChar): iSetupParam;
begin
  Result := Self;
  FName := AName;
end;

function TSetupParam.Name: PWideChar;
begin
  Result := FName;
end;

function TSetupParam.DisplayName(ADisplayName: PWideChar): iSetupParam;
begin
  Result := Self;
  FDisplayName := ADisplayName;
end;

function TSetupParam.DisplayName: PWideChar;
begin
  Result := FDisplayName;
end;

function TSetupParam.Description(ADescription: PWideChar): iSetupParam;
begin
  Result := Self;
  FDescription := ADescription;
end;

function TSetupParam.Description: PWideChar;
begin
  Result := FDescription;
end;

function TSetupParam.ServiceStartName(const AServiceStartName: PWideChar): iSetupParam;
begin
  Result := Self;
  FServiceStartName := AServiceStartName;
end;

function TSetupParam.ServiceStartName: PWideChar;
begin
  Result := FServiceStartName;
end;

function TSetupParam.ServicePassword(const AServicePassword: PWideChar): iSetupParam;
begin
  Result := Self;
  FServicePassword := AServicePassword;
end;

function TSetupParam.ServicePassword: PWideChar;
begin
  Result := FServicePassword;
end;

function TSetupParam.ExecuteInterval( AExecuteInterval: Integer ): iSetupParam;
begin
  Result := Self;
  FExecuteInterval := AExecuteInterval;
end;

function TSetupParam.ExecuteInterval: Integer;
begin
  Result := FExecuteInterval;
end;

function TSetupParam.GUIServiceStopped(AForm: TComponentClass; var AReference): iSetupParam;
begin
  Result := Self;
  FGUIServiceStopped := AForm;
  FGUIServiceStoppedRef := TComponent( AReference );
end;

function TSetupParam.GUIServiceRunning(AForm: TComponentClass; var AReference): iSetupParam;
begin
  Result := Self;
  FGUIServiceRunning := AForm;
  FGUIServiceRunningRef := TComponent( AReference );
end;

function TSetupParam.AfterInstall(AServiceEvent: TServiceEvent): iSetupParam;
begin
  Result := Self;
  FEventAfterInstall := AServiceEvent;
end;

function TSetupParam.AfterInstall: iSetupParam;
begin
  Result := Self;
  if Assigned( FEventAfterInstall ) then FEventAfterInstall;
end;

function TSetupParam.BeforeInstall(AServiceEvent: TServiceEvent): iSetupParam;
begin
  Result := Self;
  FEventBeforeInstall := AServiceEvent;
end;

function TSetupParam.BeforeInstall: iSetupParam;
begin
  Result := Self;
  if Assigned( FEventBeforeInstall ) then FEventBeforeInstall;
end;

function TSetupParam.AfterUninstall(AServiceEvent: TServiceEvent): iSetupParam;
begin
  Result := Self;
  FEventAfterUninstall := AServiceEvent;
end;

function TSetupParam.AfterUninstall: iSetupParam;
begin
  Result := Self;
  if Assigned( FEventAfterUninstall ) then FEventAfterUninstall;
end;

function TSetupParam.BeforeUninstall(AServiceEvent: TServiceEvent): iSetupParam;
begin
  Result := Self;
  FEventBeforeUninstall := AServiceEvent;
end;

function TSetupParam.BeforeUninstall: iSetupParam;
begin
  Result := Self;
  if Assigned( FEventBeforeUninstall ) then FEventBeforeUninstall;
end;

function TSetupParam.OnStart(AServiceEvent: TServiceEvent): iSetupParam;
begin
  Result := Self;
  FEventServiceOnStart := AServiceEvent;
end;

function TSetupParam.OnStart: iSetupParam;
begin
  Result := Self;
  if Assigned( FEventServiceOnStart ) then FEventServiceOnStart;
end;

function TSetupParam.OnExecute(AServiceEvent : TServiceEvent): iSetupParam;
begin
  Result := Self;
  FEventServiceOnExecute := AServiceEvent;
end;

function TSetupParam.OnExecute: iSetupParam;
begin
  Result := Self;
  if Assigned( FEventServiceOnExecute ) then FEventServiceOnExecute;
end;

function TSetupParam.OnStop(AServiceEvent: TServiceEvent): iSetupParam;
begin
  Result := Self;
  FEventServiceOnStop := AServiceEvent;
end;

function TSetupParam.OnStop: iSetupParam;
begin
  Result := Self;
  if Assigned( FEventServiceOnStop ) then FEventServiceOnStop;
end;

function TSetupParam.OnPause(AServiceEvent: TServiceEvent): iSetupParam;
begin
  Result := Self;
  FEventServiceOnPause := AServiceEvent;
end;

function TSetupParam.OnPause: iSetupParam;
begin
  Result := Self;
  if Assigned( FEventServiceOnPause ) then FEventServiceOnPause;
end;

function TSetupParam.OnContinue(AServiceEvent : TServiceEvent): iSetupParam;
begin
  Result := Self;
  FEventServiceOnContinue := AServiceEvent;
end;

function TSetupParam.OnContinue: iSetupParam;
begin
  Result := Self;
  if Assigned( FEventServiceOnContinue ) then FEventServiceOnContinue;
end;

function TSetupParam.OnDestroy(AServiceEvent : TServiceEvent): iSetupParam;
begin
  Result := Self;
  FEventServiceOnDestroy := AServiceEvent;
end;

function TSetupParam.OnDestroy: iSetupParam;
begin
  Result := Self;
  if Assigned( FEventServiceOnDestroy ) then FEventServiceOnDestroy;
end;

function TSetupParam.Dependencies(AServiceDependencies: TServiceDependencies): iSetupParam;
begin
  Result := Self;
end;

function TSetupParam.Manager: TServiceManager;
begin
  Result := ServiceManager;
end;

procedure TSetupParam.TryRunAsService;
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
