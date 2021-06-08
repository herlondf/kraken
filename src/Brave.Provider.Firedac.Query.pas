unit Brave.Provider.Firedac.Query;

interface

uses
  System.SysUtils,

  FireDAC.Comp.Client,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Def,
  FireDAC.Stan.Async,
  FireDAC.Stan.Param,
  FireDAC.Stan.Error,
  FireDAC.Stan.Option;

type
  TBraveProviderFiredacQuery = class(TFDQuery)
  private
    FConnection: TFDConnection;
    FQuery: TFDQuery;
  public
    constructor Create(AConnection: TFDConnection);
    destructor Destroy; override;

    procedure Open(ASQL: String = '');
    procedure StartTransaction;
    procedure Commit;
    function SQL: TBraveProviderFiredacQuery;
    function Add(ASQL: String): TBraveProviderFiredacQuery;
  end;

implementation

{ TBraveProviderFiredacQuery }

constructor TBraveProviderFiredacQuery.Create(AConnection: TFDConnection);
begin
  FConnection := AConnection;

  FQuery := TFDQuery.Create(nil);
  FQuery.Connection := FConnection;
end;

destructor TBraveProviderFiredacQuery.Destroy;
begin
  FQuery.Free;

  inherited;
end;

procedure TBraveProviderFiredacQuery.Open(ASQL: String);
begin
  if ASQL <> '' then
  begin
    FQuery.SQL.Clear;
    FQuery.SQL.Add(ASQL);
  end;

  FQuery.Open;
end;

procedure TBraveProviderFiredacQuery.StartTransaction;
begin
  FConnection.StartTransaction;
end;

procedure TBraveProviderFiredacQuery.Commit;
begin
  FConnection.Commit;
end;

function TBraveProviderFiredacQuery.SQL: TBraveProviderFiredacQuery;
begin
  Result := Self;
end;

function TBraveProviderFiredacQuery.Add(ASQL: String): TBraveProviderFiredacQuery;
begin
  Result := Self;
  FQuery.SQL.Add(ASQL);
end;

end.
