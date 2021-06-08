unit Brave.Provider.Zeos.Query;

interface

uses
  Data.DB,
  System.SysUtils,

  ZDataset,
  ZConnection;

type
  TBraveProviderZeosQuery = class(TZQuery)
    constructor Create(AConnection: TZConnection);
    destructor Destroy; override;
  private
    FConnection: TZConnection;
    FQuery: TZQuery;
  public
    procedure Open(ASQL: String = '');
    procedure StartTransaction;
    procedure Commit;
    function SQL: TBraveProviderZeosQuery;
    function Add(ASQL: String): TBraveProviderZeosQuery;
  end;

implementation

{ TBraveProviderZeosQuery }

constructor TBraveProviderZeosQuery.Create(AConnection: TZConnection);
begin
  FConnection := AConnection;

  FQuery := TZQuery.Create(nil);
  FQuery.Connection := FConnection;
end;

destructor TBraveProviderZeosQuery.Destroy;
begin
  FQuery.Free;

  inherited;
end;

procedure TBraveProviderZeosQuery.Open(ASQL: String);
begin
  if ASQL <> '' then
  begin
    FQuery.SQL.Clear;
    FQuery.SQL.Add(ASQL);
  end;

  FQuery.Open;
end;

procedure TBraveProviderZeosQuery.StartTransaction;
begin
  FConnection.StartTransaction;
end;

procedure TBraveProviderZeosQuery.Commit;
begin
  FConnection.Commit;
end;

function TBraveProviderZeosQuery.SQL: TBraveProviderZeosQuery;
begin
  Result := Self;
end;

function TBraveProviderZeosQuery.Add(ASQL: String): TBraveProviderZeosQuery;
begin
  Result := Self;
  FQuery.SQL.Add(ASQL);
end;

end.
