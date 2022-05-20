unit Kraken.Service;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.WinSvc,
  System.SysUtils,
  System.Classes,
  System.SyncObjs,
  System.Generics.Collections,
  System.Win.Registry,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.SvcMgr,
  Kraken.Log;

  type
  TKrakenThreadService = class(TThread)
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  class var
    FEvent: TEvent;
    FDefaultJob: TKrakenThreadService;
  protected
    procedure Execute; override;
  public
    class function DefaultJob: TKrakenThreadService;
    class destructor UnInitialize;
  end;

  TKrakenService = class(TService)
    procedure ServiceAfterInstall(Sender: TService);
    procedure ServiceExecute(Sender: TService);
  private
    procedure SaveRegistry;
  protected
    constructor Create(AOwner: TComponent); override;
    function GetServiceController: TServiceController; override;
    procedure DoStart; override;
    function DoStop: Boolean; override;
    function DoPause: Boolean; override;
    procedure DoShutdown; override;
  end;
  
var
  FKrakenService : TKrakenService;
  
implementation

uses
  Kraken.Service.Setup;

{$R *.dfm}

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  FKrakenService.Controller(CtrlCode);
end;

{ TKrakenThreadService }
procedure TKrakenThreadService.AfterConstruction;
begin
  inherited;
  FEvent := TEvent.Create;
end;

procedure TKrakenThreadService.BeforeDestruction;
begin
  inherited;
  FEvent.Free;
end;

procedure TKrakenThreadService.Execute;
var
  LWaitResult: TWaitResult;
  LCriticalSection: TCriticalSection;
  LOnExecute: TServiceEvent;
begin
  inherited;
  
  LCriticalSection := TCriticalSection.Create;
  LCriticalSection.Enter;
  
  LOnExecute := KrakenServiceSetup.OnExecute;
  
  if not Self.Terminated  then
  begin
    try     
      if Assigned(LOnExecute) then
        LOnExecute;
    except
    
    end;
  end;

  while not Self.Terminated do
  begin
    LWaitResult := FEvent.WaitFor( KrakenServiceSetup.ExecuteInterval );

    if LWaitResult <> TWaitResult.wrTimeout then
      Break;

    try
      if Assigned(LOnExecute) then
        LOnExecute;
    except
      Continue;
    end;
  end;

  LCriticalSection.Leave;
  LCriticalSection.Free;
end;

class function TKrakenThreadService.DefaultJob: TKrakenThreadService;
begin
  if FDefaultJob = nil then
  begin
    FDefaultJob := TKrakenThreadService.Create(True);
    FDefaultJob.FreeOnTerminate := False;
  end;
  Result := FDefaultJob;
end;

class destructor TKrakenThreadService.UnInitialize;
begin
  if Assigned(FDefaultJob) then
  begin
    if not FDefaultJob.Terminated then
    begin
      FDefaultJob.Terminate;
      FEvent.SetEvent;
      FDefaultJob.WaitFor;
    end;
    FreeAndNil(FDefaultJob);
  end;
end;


{ TKrakenService }
constructor TKrakenService.Create(AOwner: TComponent);
begin
  inherited;
  Self.Name        := KrakenServiceSetup.Name;
  Self.DisplayName := KrakenServiceSetup.DisplayName;
  Self.StartType   := KrakenServiceSetup.StartType;
end;

function TKrakenService.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TKrakenService.SaveRegistry;
var
  reg : TRegIniFile;
  regEdit: TRegistry;
begin
  if KrakenServiceSetup.Description = '' then
    Exit;

  regEdit := TRegistry.Create(KEY_READ or KEY_WRITE);

  try
    regEdit.RootKey := HKEY_LOCAL_MACHINE;

    if regEdit.OpenKey('\SYSTEM\CurrentControlSet\Services\' + Name, False) then
    begin
      regEdit.WriteString('Description', KrakenServiceSetup.Description);
      regEdit.WriteInteger('ErrorControl', 2);
      regEdit.CloseKey;
    end;
  finally
    regEdit.Free;
  end;

  Reg := TRegIniFile.Create(KEY_ALL_ACCESS);

  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;

    if Reg.OpenKey('\SYSTEM\CurrentControlSet\Services\Eventlog\Application\' + Name, True) then
    begin
      TRegistry(Reg).WriteString('EventMessageFile', ParamStr(0));
      TRegistry(Reg).WriteInteger('TypesSupported', 7);
      TRegistry(Reg).WriteString('Description', KrakenServiceSetup.Description);
    end;
    if Reg.OpenKey('\SYSTEM\CurrentControlSet\Services\' + Name, True) then
    begin
      if FindCmdLineSwitch('NOVALIDATE', ['-', '/'], True) then
        TRegistry(Reg).WriteString('ImagePath', GetModuleName(HInstance) + ' -NOVALIDATE')
      Else
        TRegistry(Reg).WriteString('ImagePath', GetModuleName(HInstance) +  ' -RunService' );
    end;
  finally
    Reg.Free;
  end;
end;

procedure TKrakenService.ServiceAfterInstall(Sender: TService);
begin
  SaveRegistry;
  KrakenServiceSetup.AfterInstall;
end;

procedure TKrakenService.ServiceExecute(Sender: TService);
begin
  if not TKrakenThreadService.DefaultJob.Started then
    TKrakenThreadService.DefaultJob.Start;

  while not Self.Terminated do  
    ServiceThread.ProcessRequests(true);    

  if not TKrakenThreadService.DefaultJob.Terminated then
    TKrakenThreadService.DefaultJob.Terminate;
end;

function TKrakenService.DoPause: Boolean;
begin
  KrakenServiceSetup.OnPause;
  inherited;
end;

procedure TKrakenService.DoShutdown;
begin
  KrakenServiceSetup.OnDestroy;
  inherited;
end;

procedure TKrakenService.DoStart;
var
  LStartEvent: Kraken.Service.Setup.TStartEvent;
begin
  LStartEvent := KrakenServiceSetup.OnStart;
  if Assigned(LStartEvent) then
    LStartEvent;
  inherited;
end;

function TKrakenService.DoStop: Boolean;
begin
  KrakenServiceSetup.OnStop;
  inherited;
end;

end.
