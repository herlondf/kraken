unit KrakenThread;

interface

uses
  System.Classes,
  System.SyncObjs,
  System.SysUtils,
  Kraken.Log;

type
  //TKrakenEvent = reference to procedure;
  
  TKrakenThread = class(TThread)
    class destructor UnInitialize;
  class var
    FEvent: TEvent;
    FDefaultJob: TKrakenThread;
  private
    fProcEvent: TProc;
    fInterval: Integer;
    fPaused: Boolean;
  protected
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure Execute; override;
  public
    class function DefaultJob(AFreeOnTerminate: Boolean = False): TKrakenThread;
    class function NewJob(AFreeOnTerminate: Boolean = False): TKrakenThread;
    
    function Interval(const Value: Integer): TKrakenThread;
    function ProcEvent(const AProcEvent: TProc): TKrakenThread;
    procedure Pause;
    procedure Run;
    procedure Finish;
  end;

implementation

procedure TKrakenThread.AfterConstruction;
begin
  inherited;
  FEvent := TEvent.Create;
end;

procedure TKrakenThread.BeforeDestruction;
begin
  inherited;
  FEvent.Free;
end;

procedure TKrakenThread.Execute;
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
      if Assigned(fProcEvent) then
        fProcEvent;
    except
    end;
  end;

  while not Self.Terminated do
  begin
    if fPaused then
      Continue;
  
    LWaitResult := FEvent.WaitFor( fInterval );
    if
     LWaitResult <> TWaitResult.wrTimeout then
      Break;
      
    try
      if Assigned(fProcEvent) then        
        fProcEvent;
    except
      Continue;
    end;
  end;

  LCriticalSection.Leave;
  LCriticalSection.Free;
end;

procedure TKrakenThread.Finish;
begin
  if Assigned(FDefaultJob) then
  begin
    if not FDefaultJob.Terminated then
    begin
      FDefaultJob.Terminate;
      FEvent.SetEvent;
      if not FDefaultJob.Suspended then
      begin
        try
          FDefaultJob.WaitFor;
        except
          on e: exception do KrakenLog.Error(e.Message);
        end;
      end;
    end;

    FreeAndNil(FDefaultJob);
  end;
end;

function TKrakenThread.Interval(const Value: Integer): TKrakenThread;
begin
  Result := Self;
  fInterval := Value;
end;

procedure TKrakenThread.Pause;
begin
  fPaused := True;
end;

function TKrakenThread.ProcEvent(const AProcEvent: TProc): TKrakenThread;
begin
  Result := Self;
  fProcEvent := AProcEvent;
end;

procedure TKrakenThread.Run;
begin
  fPaused := False;
end;

class function TKrakenThread.DefaultJob(AFreeOnTerminate: Boolean = False): TKrakenThread;
begin
  if FDefaultJob = nil then
  begin
    FDefaultJob := TKrakenThread.Create(True);
    FDefaultJob.FreeOnTerminate := AFreeOnTerminate;
  end;

  Result := FDefaultJob;
end;

class function TKrakenThread.NewJob(AFreeOnTerminate: Boolean = False): TKrakenThread;
begin
  FDefaultJob := TKrakenThread.Create(True);
  FDefaultJob.FreeOnTerminate := AFreeOnTerminate;
  Result := FDefaultJob;
end;

class destructor TKrakenThread.UnInitialize;
begin
  if Assigned(FDefaultJob) then
  begin
    if not FDefaultJob.Terminated then
    begin
      FDefaultJob.Terminate;
      FEvent.SetEvent;
      if not FDefaultJob.Suspended then
      begin
        try
          FDefaultJob.WaitFor;
        except
          on e: exception do KrakenLog.Error(e.Message);
        end;
      end;
        
    end;

    FreeAndNil(FDefaultJob);
  end;
end;

end.
