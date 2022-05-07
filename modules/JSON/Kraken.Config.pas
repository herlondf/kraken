unit Kraken.Config;

interface

uses
  System.SysUtils;

type
  TCaseDefinition = (cdNone, cdLower, cdUpper, cdLowerCamelCase);

  TKrakenConfig = class
  private
    class var FInstance: TKrakenConfig;

    FCaseDefinition: TCaseDefinition;

    constructor createPrivate;
  public
    constructor Create;
    destructor Destroy; override;

    function CaseDefinition(Value: TCaseDefinition): TKrakenConfig; overload;
    function CaseDefinition: TCaseDefinition; overload;

    class function GetInstance: TKrakenConfig;
    class destructor UnInitialize;
  end;

implementation

{ TKrakenConfig }

function TKrakenConfig.CaseDefinition(Value: TCaseDefinition): TKrakenConfig;
begin
  result := Self;
  FCaseDefinition := Value;
end;

function TKrakenConfig.CaseDefinition: TCaseDefinition;
begin
  result := FCaseDefinition;
end;

constructor TKrakenConfig.Create;
begin
  raise Exception.Create('Invoke the GetInstance Method.');
end;

constructor TKrakenConfig.createPrivate;
begin

end;

destructor TKrakenConfig.Destroy;
begin

  inherited;
end;

class function TKrakenConfig.GetInstance: TKrakenConfig;
begin
  if not Assigned(FInstance) then
  begin
    FInstance := TKrakenConfig.createPrivate;
    FInstance.CaseDefinition(cdNone);
  end;
  Result := FInstance;
end;

class destructor TKrakenConfig.UnInitialize;
begin
  if Assigned(FInstance) then
    FreeAndNil(FInstance);
end;

end.
