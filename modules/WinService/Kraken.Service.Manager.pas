unit Kraken.Service.Manager;

interface

uses
  System.SysUtils,
  System.Classes,
  Winapi.Windows,
  Winapi.WinSvc,
  Vcl.Forms,
  Vcl.SvcMgr;

type
  TInstallType = ( itInstall, itUninstall );

  TServiceManager = class
    constructor Create;
    destructor Destroy; override;
  private
    FServiceHandle: SC_Handle;
    FServiceControlManager: SC_Handle;

    function IsDesktopMode(AServiceName: PWideChar): Boolean;

    procedure ExecProcess(AExeName: string; AType: TInstallType);

    function Connect: Boolean;
    procedure Start(NumberOfArgument: DWORD; ServiceArgVectors: PChar);
  public
    function TryRunAsService(AServiceName, ADisplayName: PWideChar): Boolean;

    function StartService    : TServiceManager;
    function ContinueService : TServiceManager;
    function RestartService  : TServiceManager;
    function StopService     : TServiceManager;
    function ShutdownService : TServiceManager;
    function DisableService  : TServiceManager;

    function ServiceRunning : Boolean;
    function ServiceStopped : Boolean;    

    function Install: TServiceManager;
    function Uninstall: TServiceManager;
  end;

  function ServiceManager: TServiceManager;

var
  FServiceManager: TServiceManager;

implementation

uses
  Kraken.Setup,
  Kraken.Service;

function ServiceManager: TServiceManager;
begin
  if FServiceManager = nil then
    FServiceManager := TServiceManager.Create;
  Result := FServiceManager;
end;

{ TServiceManager }

constructor TServiceManager.Create;
begin

end;

destructor TServiceManager.Destroy;
begin
  CloseServiceHandle(FServiceHandle);
  CloseServiceHandle(FServiceControlManager);
  
  inherited;
end;

function TServiceManager.IsDesktopMode(AServiceName: PWideChar): Boolean;
begin
  Result := False;

  if
    (Win32Platform <> VER_PLATFORM_WIN32_NT) or
    FindCmdLineSwitch( 'P', ['-', '/'], True ) or
    (
      ( not FindCmdLineSwitch( 'INSTALL'    , ['-', '/'], True ) ) and
      ( not FindCmdLineSwitch( 'UNINSTALL'  , ['-', '/'], True ) ) and
      ( not FindCmdLineSwitch( 'RUNSERVICE' , ['-', '/'], True ) ) and
      ( not FindCmdLineSwitch( 'RESTART'    , ['-', '/'], True ) )
    )
  then
    Result := True
  else
  begin
    Result := not FindCmdLineSwitch( 'INSTALL'    , ['-', '/'], True ) and
              not FindCmdLineSwitch( 'UNINSTALL'  , ['-', '/'], True ) and
              not FindCmdLineSwitch( 'RUNSERVICE' , ['-', '/'], True ) and
              not FindCmdLineSwitch( 'RESTART'    , ['-', '/'], True ) ;
  end;
end;


procedure TServiceManager.ExecProcess(AExeName: string; AType: TInstallType);
var
  sParams            : String;
  StartupInfo        : TStartupInfo;
  ProcessInformation : TProcessInformation;
begin
  case AType of
    TInstallType.itInstall  : sParams := ' /install';
    TInstallType.itUnInstall: sParams := ' /unInstall';
  end;

  FillChar(StartupInfo, SizeOf(StartupInfo), #0);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := SW_HIDE;

  if CreateProcess( Nil, PChar( AExeName + sParams ), Nil, Nil, False,
                           NORMAL_PRIORITY_CLASS, Nil,
                           PChar( ExtractFileDir( AExeName ) ),
                           StartupInfo, ProcessInformation ) then
  begin
    try
      WaitForSingleObject( ProcessInformation.hProcess, INFINITE);
    finally
      CloseHandle( ProcessInformation.hProcess);
      CloseHandle( ProcessInformation.hThread);
    end;
  end
  else
    Raise Exception.Create(Format('The operation %s could not executed.', [sParams]));
end;

function TServiceManager.TryRunAsService(AServiceName, ADisplayName: PWideChar): Boolean;
begin
  Result := False;

  if not ServiceManager.IsDesktopMode( AServiceName ) then
  begin
    if
      ( not Vcl.SvcMgr.Application.DelayInitialize ) or
      ( Vcl.SvcMgr.Application.Installing )
    then
      Vcl.SvcMgr.Application.Initialize;

    Vcl.SvcMgr.Application.Title := ADisplayName;
    Vcl.SvcMgr.Application.CreateForm( TKrakenService, FKrakenService );
    Vcl.SvcMgr.Application.Run;

    Result := True;
  end;
end;

function TServiceManager.Connect: Boolean;
begin
  if FServiceControlManager = 0 then
    FServiceControlManager := OpenSCManager('', nil, SC_MANAGER_CONNECT);

  if FServiceHandle = 0 then
    FServiceHandle := OpenService(FServiceControlManager, KrakenSetup.Name, SERVICE_ALL_ACCESS);

  Result := ( FServiceControlManager <> 0 ) and ( FServiceHandle <> 0 );
end;


procedure TServiceManager.Start(NumberOfArgument: DWORD; ServiceArgVectors: PChar);
begin
  if Connect then
    Winapi.WinSvc.StartService(FServiceHandle, NumberOfArgument, ServiceArgVectors);
end;

function TServiceManager.StartService: TServiceManager;
begin
  Result := Self;
  if Connect and ServiceStopped then
    Start(0, '');
end;

function TServiceManager.ContinueService: TServiceManager;
var
  ServiceStatus: TServiceStatus;
begin
  Result := Self;
  if Connect then
    ControlService(FServiceHandle, SERVICE_CONTROL_CONTINUE, ServiceStatus);
end;

function TServiceManager.RestartService: TServiceManager;
begin
  Result := Self;
  StopService;
  StartService;
end;

function TServiceManager.StopService: TServiceManager;
var
  ServiceStatus: TServiceStatus;
begin
  Result := Self;
  if ServiceRunning then
  begin
    if Connect then
      ControlService(FServiceHandle, SERVICE_CONTROL_STOP, ServiceStatus);
  end;
end;

function TServiceManager.ShutdownService: TServiceManager;
var
  ServiceStatus: TServiceStatus;
begin
  Result := Self;
  if Connect then
    ControlService(FServiceHandle, SERVICE_CONTROL_SHUTDOWN, ServiceStatus);
end;

function TServiceManager.DisableService: TServiceManager;
begin
  Result := Self;
end;

function TServiceManager.ServiceRunning: Boolean;
var
  LStatusService   : TServiceStatus;
begin
  Result := False;
  if Connect and ( QueryServiceStatus( FServiceHandle, LStatusService ) ) then         
    Result :=
      ( SERVICE_RUNNING          = LStatusService.dwCurrentState ) or
      ( SERVICE_CONTINUE_PENDING = LStatusService.dwCurrentState );
end;

function TServiceManager.ServiceStopped: Boolean;
var
  LStatusService   : TServiceStatus;
begin
  Result := False;
  if Connect and ( QueryServiceStatus( FServiceHandle, LStatusService ) ) then             
    Result :=
      ( SERVICE_STOPPED      = LStatusService.dwCurrentState ) or
      ( SERVICE_STOP_PENDING = LStatusService.dwCurrentState );
end;

function TServiceManager.Install: TServiceManager;
begin
  Result := Self;
  ExecProcess( ParamStr(0), TInstallType.itInstall );
  Sleep( 2000 );
end;

function TServiceManager.Uninstall: TServiceManager;
begin
  Result := Self;
  ExecProcess( ParamStr(0), TInstallType.itUninstall );
  Sleep( 2000 );
end;

initialization

finalization
  if FServiceManager <> nil then
    FServiceManager.Free;

end.
