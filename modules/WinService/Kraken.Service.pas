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
  Vcl.SvcMgr;

type
  TKrakenThreadExecute = class(TThread)
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  class var
    FEvent: TEvent;
    FDefaultJob: TKrakenThreadExecute;
  protected
    procedure Execute; override;
  public
    class function DefaultJob: TKrakenThreadExecute;
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
  Kraken.Setup;

{$R *.dfm}

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  FKrakenService.Controller(CtrlCode);
end;

{ TBinaryUpdaterJob }

procedure TKrakenThreadExecute.AfterConstruction;
begin
  inherited;
  FEvent := TEvent.Create;
end;

procedure TKrakenThreadExecute.BeforeDestruction;
begin
  inherited;
  FEvent.Free;
end;

procedure TKrakenThreadExecute.Execute;
var
  LWaitResult: TWaitResult;
  LCriticalSection: TCriticalSection;
begin
  inherited;

  LCriticalSection := TCriticalSection.Create;
  LCriticalSection.Enter;

  if not Self.Terminated  then
  begin
    try
      KrakenSetup.OnExecute;
    except

    end;
  end;

  while not Self.Terminated do
  begin
    LWaitResult := FEvent.WaitFor( KrakenSetup.ExecuteInterval );

    if LWaitResult <> TWaitResult.wrTimeout then
      Break;
    try
      KrakenSetup.OnExecute;
    except
      Continue;
    end;
  end;

  LCriticalSection.Leave;
  LCriticalSection.Free;
end;

class function TKrakenThreadExecute.DefaultJob: TKrakenThreadExecute;
begin
  if FDefaultJob = nil then
  begin
    FDefaultJob := TKrakenThreadExecute.Create(True);
    FDefaultJob.FreeOnTerminate := False;
  end;

  Result := FDefaultJob;
end;

class destructor TKrakenThreadExecute.UnInitialize;
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

  Self.Name        := KrakenSetup.Name;
  Self.DisplayName := KrakenSetup.DisplayName;
  Self.StartType   := KrakenSetup.StartType;
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
  if KrakenSetup.Description = '' then
    Exit;

  regEdit := TRegistry.Create(KEY_READ or KEY_WRITE);
  try
    regEdit.RootKey := HKEY_LOCAL_MACHINE;
    if regEdit.OpenKey('\SYSTEM\CurrentControlSet\Services\' + Name, False) then
    begin
      regEdit.WriteString('Description', KrakenSetup.Description);
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
      TRegistry(Reg).WriteString('Description', KrakenSetup.Description);
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
  KrakenSetup.AfterInstall;
end;

procedure TKrakenService.ServiceExecute(Sender: TService);
begin
  if not TKrakenThreadExecute.DefaultJob.Started then
    TKrakenThreadExecute.DefaultJob.Start;

  while not Self.Terminated do
    ServiceThread.ProcessRequests(true);

  if not TKrakenThreadExecute.DefaultJob.Terminated then
    TKrakenThreadExecute.DefaultJob.Terminate;
end;

function TKrakenService.DoPause: Boolean;
begin
  KrakenSetup.OnPause;
  inherited;
end;

procedure TKrakenService.DoShutdown;
begin
  KrakenSetup.OnDestroy;
  inherited;
end;

procedure TKrakenService.DoStart;
begin
  KrakenSetup.OnStart;
  inherited;
end;

function TKrakenService.DoStop: Boolean;
begin
  KrakenSetup.OnStop;
  inherited;
end;

end.
