unit KrakenThreadAnimation;

interface

uses
  System.Classes,
  System.SyncObjs,
  System.SysUtils,
  KrakenThread;

type
  TAniSyncProc = reference to procedure (Value: Integer);
  TAniFunction = reference to function (Value: Integer): Int64;

  TAniKind = (akIn, akOut);
  TAniFunctionKind = (afkCustom, afkLinear, afkQuadratic, afkCubic, afkQuartic, afkQuintic, afkBack);

  TKrakenThreadAnimation = class(TThread)
    class destructor UnInitialize;
  class var
    FEvent: TEvent;
    FDefaultJob: TKrakenThreadAnimation;
  private


    {Animation}
    FAniFunction: TAniFunction;
    FOnSync: TAniSyncProc;
    FOnDone: TProc;

    FAniKind: TAniKind;
    FAniFunctionKind: TAniFunctionKind;

    FStep         : Integer;
    FDelayStart   : Integer;
    FDuration     : Integer;
    FStartValue   : Integer;
    FStopValue    : Integer;
    FCurrent      : Integer; {temp - remover}
  protected
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure Execute; override;
  public
    class function DefaultJob: TKrakenThreadAnimation;
    class function NewJob: TKrakenThreadAnimation;

    function Animation(
      aAniKind      : TAniKind;
      aFunctionKind : TAniFunctionKind;
      aStartValue   : Integer;
      aStopValue    : Integer;
      aSyncProc     : TAniSyncProc
    ): TKrakenThreadAnimation;

    procedure UpdateControl;
    procedure DoneControl;

    class function afLinear   ( Value: Integer ): Int64;
    class function afQuadratic( Value: Integer ): Int64;
    class function afCubic    ( Value: Integer ): Int64;
    class function afQuartic  ( Value: Integer ): Int64;
    class function afQuintic  ( Value: Integer ): Int64;
    class function afBack     ( Value: Integer ): Int64;
  end;

implementation

procedure TKrakenThreadAnimation.AfterConstruction;
begin
  inherited;
  FEvent := TEvent.Create;
end;

function TKrakenThreadAnimation.Animation(
  aAniKind      : TAniKind;
  aFunctionKind : TAniFunctionKind;
  aStartValue   : Integer;
  aStopValue    : Integer;
  aSyncProc     : TAniSyncProc
): TKrakenThreadAnimation;
begin
  Result := Self;

  //  Default properties
  FStep         := 25;
  FDelayStart   := 0;
  FDuration     := 250;
  FStartValue   := aStartValue;
  FStopValue    := aStopValue;

  case aFunctionKind of
   afkCustom:
     FAniFunction := nil;
   afkLinear:
     FAniFunction := afLinear;
   afkQuadratic:
     FAniFunction := afQuadratic;
   afkCubic:
     FAniFunction := afCubic;
   afkQuartic:
     FAniFunction := afQuartic;
   afkQuintic:
     FAniFunction := afQuintic;
   afkBack:
     FAniFunction := afBack;
  end;


  //  Params properties
  FAniKind         := aAniKind;
  FAniFunctionKind := aFunctionKind;
  FOnSync          := aSyncProc;
end;

procedure TKrakenThreadAnimation.BeforeDestruction;
begin
  inherited;
  FEvent.Free;
end;

procedure TKrakenThreadAnimation.Execute;
var
  I               : Integer;
  AniLength       : Integer;
  TimePerStep     : Integer;
  Max             : Int64;
  Part            : Single;
  BaseInverse     : ShortInt;
  LCriticalSection: TCriticalSection;
begin
  inherited;

  LCriticalSection := TCriticalSection.Create;
  LCriticalSection.Enter;

  AniLength := Abs(FStopValue - FStartValue);
  TimePerStep := Round(FDuration / FStep);
  Max := FAniFunction(FStep);
  Part := AniLength / Max;

  if FStartValue < FStopValue then
    BaseInverse := 1
  else
    BaseInverse := -1;

  //  Delay start
  Sleep(FDelayStart);

  //  Loop
  if FAniKind = akIn then
    for i := 1 to FStep do
      begin
        FCurrent := FStartValue + BaseInverse * Round(FAniFunction(i) * Part);
        Synchronize(UpdateControl);
        Sleep(TimePerStep);
      end
  else
    for i := 1 to FStep do
      begin
        FCurrent := FStartValue + BaseInverse * (AniLength - Round(FAniFunction(FStep + 1 - i) * Part));
        Synchronize(UpdateControl);
        Sleep(TimePerStep);
      end;

  FCurrent := FStopValue;
  Synchronize(UpdateControl);
  Synchronize(DoneControl);

  LCriticalSection.Leave;
  LCriticalSection.Free;
end;


class function TKrakenThreadAnimation.NewJob: TKrakenThreadAnimation;
begin
  FDefaultJob := TKrakenThreadAnimation.Create(True);
  FDefaultJob.FreeOnTerminate := True;

  Result := FDefaultJob;
end;

class function TKrakenThreadAnimation.DefaultJob: TKrakenThreadAnimation;
begin
  if FDefaultJob = nil then
  begin
    FDefaultJob := TKrakenThreadAnimation.Create(True);
    FDefaultJob.FreeOnTerminate := False;
  end;

  Result := FDefaultJob;
end;

procedure TKrakenThreadAnimation.DoneControl;
begin
  if Assigned(FOnDone) then
    FOnDone();
end;

procedure TKrakenThreadAnimation.UpdateControl;
begin
  if Assigned(FOnSync) then
    FOnSync(FCurrent);
end;

class function TKrakenThreadAnimation.afBack(Value: Integer): Int64;
begin
  Result := ( Value - 2 ) * ( Value - 8 )// * ( Value - 8 ) * ( Value - 16 )
end;

class function TKrakenThreadAnimation.afCubic(Value: Integer): Int64;
begin
  Result := Value;
end;

class function TKrakenThreadAnimation.afLinear(Value: Integer): Int64;
begin
  Result := Value * Value;
end;

class function TKrakenThreadAnimation.afQuadratic(Value: Integer): Int64;
begin
  Result := Value * Value * Value;
end;

class function TKrakenThreadAnimation.afQuartic(Value: Integer): Int64;
begin
  Result := Value * Value * Value * Value;
end;

class function TKrakenThreadAnimation.afQuintic(Value: Integer): Int64;
begin
  Result := Value * Value * Value * Value * Value;
end;


class destructor TKrakenThreadAnimation.UnInitialize;
begin
//  if Assigned(FDefaultJob) then
//  begin
//    if not FDefaultJob.Terminated then
//    begin
//      FDefaultJob.Terminate;
//
//      FEvent.SetEvent;
//
//      if not FDefaultJob.Suspended then
//        FDefaultJob.WaitFor;
//    end;
//
//    FreeAndNil(FDefaultJob);
//  end;
end;

end.
