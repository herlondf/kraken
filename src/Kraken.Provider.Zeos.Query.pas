unit Kraken.Provider.Zeos.Query;

interface

uses
  Data.DB,
  System.SysUtils,

  ZDataset,
  ZConnection;

type
  TKrakenProviderZeosQuery = class(TZQuery)
    constructor Create(AConnection: TZConnection);
    destructor Destroy; override;
  private
    FConnection: TZConnection;
    FQuery: TZQuery;
  public
    procedure Open(ASQL: String = '');
    procedure StartTransaction;
    procedure Commit;
    function SQL: TKrakenProviderZeosQuery;
    function Add(ASQL: String): TKrakenProviderZeosQuery;
  end;

implementation

{ TKrakenProviderZeosQuery }

constructor TKrakenProviderZeosQuery.Create(AConnection: TZConnection);
begin
  FConnection := AConnection;

  FQuery := TZQuery.Create(nil);
  FQuery.Connection := FConnection;
end;

destructor TKrakenProviderZeosQuery.Destroy;
begin
  FQuery.Free;

  inherited;
end;

procedure TKrakenProviderZeosQuery.Open(ASQL: String);
begin
  if ASQL <> '' then
  begin
    FQuery.SQL.Clear;
    FQuery.SQL.Add(ASQL);
  end;

  FQuery.Open;
end;

procedure TKrakenProviderZeosQuery.StartTransaction;
begin
  FConnection.StartTransaction;
end;

procedure TKrakenProviderZeosQuery.Commit;
begin
  FConnection.Commit;
end;

function TKrakenProviderZeosQuery.SQL: TKrakenProviderZeosQuery;
begin
  Result := Self;
end;

function TKrakenProviderZeosQuery.Add(ASQL: String): TKrakenProviderZeosQuery;
begin
  Result := Self;
  FQuery.SQL.Add(ASQL);
end;

end.
