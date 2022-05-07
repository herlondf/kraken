unit KrakenThread;

interface

uses
  System.Classes,
  System.SyncObjs,
  System.SysUtils;

type
  TKrakenEvent = reference to procedure;

  TKrakenThread = class(TThread)
    class destructor UnInitialize;
  class var
    FEvent: TEvent;
    FDefaultJob: TKrakenThread;
  private
    fProcEvent: TKrakenEvent;
    fInterval: Integer;
  protected
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure Execute; override;
  public
    class function DefaultJob: TKrakenThread;

    procedure Interval(const Value: Integer);
    procedure ProcEvent(const AProcEvent: TKrakenEvent);

    procedure Pause;
    procedure Run;
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
    LWaitResult := FEvent.WaitFor( fInterval );

    if LWaitResult <> TWaitResult.wrTimeout then
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

procedure TKrakenThread.Interval(const Value: Integer);
begin
  fInterval := Value;
end;

procedure TKrakenThread.Pause;
begin
  if not FDefaultJob.Suspended then
    FDefaultJob.Suspended := True;
end;

procedure TKrakenThread.ProcEvent(const AProcEvent: TKrakenEvent);
begin
  fProcEvent := AProcEvent;
end;

procedure TKrakenThread.Run;
begin
  if FDefaultJob.Suspended then
    FDefaultJob.Suspended := False;
end;

class function TKrakenThread.DefaultJob: TKrakenThread;
begin
  if FDefaultJob = nil then
  begin
    FDefaultJob := TKrakenThread.Create(True);
    FDefaultJob.FreeOnTerminate := False;
  end;

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
        FDefaultJob.WaitFor;
    end;

    FreeAndNil(FDefaultJob);
  end;
end;

end.
