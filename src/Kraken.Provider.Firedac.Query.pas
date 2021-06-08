unit Kraken.Provider.Firedac.Query;

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
  TKrakenProviderFiredacQuery = class(TFDQuery)
  private
    FConnection: TFDConnection;
    FQuery: TFDQuery;
  public
    constructor Create(AConnection: TFDConnection);
    destructor Destroy; override;

    procedure Open(ASQL: String = '');
    procedure StartTransaction;
    procedure Commit;
    function SQL: TKrakenProviderFiredacQuery;
    function Add(ASQL: String): TKrakenProviderFiredacQuery;
  end;

implementation

{ TKrakenProviderFiredacQuery }

constructor TKrakenProviderFiredacQuery.Create(AConnection: TFDConnection);
begin
  FConnection := AConnection;

  FQuery := TFDQuery.Create(nil);
  FQuery.Connection := FConnection;
end;

destructor TKrakenProviderFiredacQuery.Destroy;
begin
  FQuery.Free;

  inherited;
end;

procedure TKrakenProviderFiredacQuery.Open(ASQL: String);
begin
  if ASQL <> '' then
  begin
    FQuery.SQL.Clear;
    FQuery.SQL.Add(ASQL);
  end;

  FQuery.Open;
end;

procedure TKrakenProviderFiredacQuery.StartTransaction;
begin
  FConnection.StartTransaction;
end;

procedure TKrakenProviderFiredacQuery.Commit;
begin
  FConnection.Commit;
end;

function TKrakenProviderFiredacQuery.SQL: TKrakenProviderFiredacQuery;
begin
  Result := Self;
end;

function TKrakenProviderFiredacQuery.Add(ASQL: String): TKrakenProviderFiredacQuery;
begin
  Result := Self;
  FQuery.SQL.Add(ASQL);
end;

end.
