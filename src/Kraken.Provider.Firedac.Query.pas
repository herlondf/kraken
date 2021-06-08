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

    procedure Open(ASQL: String); overload;
    procedure Open; overload;
    procedure StartTransaction;
    procedure Commit;
    function SQL: TKrakenProviderFiredacQuery;
    function Add(ASQL: String): TKrakenProviderFiredacQuery;
    function Clear: TKrakenProviderFiredacQuery;
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

procedure TKrakenProviderFiredacQuery.Open;
begin
  FQuery.Prepare;
  FQuery.Open;
end;

procedure TKrakenProviderFiredacQuery.Open(ASQL: String);
begin
  if ASQL <> '' then
  begin
    FQuery.SQL.Clear;
    FQuery.SQL.Add(ASQL);
  end;

  FQuery.Prepare;
  FQuery.Open;
end;

function TKrakenProviderFiredacQuery.SQL: TKrakenProviderFiredacQuery;
begin
  Result := Self;
  FQuery.SQL.Clear;
end;

procedure TKrakenProviderFiredacQuery.StartTransaction;
begin
  FConnection.StartTransaction;
end;

function TKrakenProviderFiredacQuery.Add(
  ASQL: String): TKrakenProviderFiredacQuery;
begin

end;

function TKrakenProviderFiredacQuery.Clear: TKrakenProviderFiredacQuery;
begin

end;

procedure TKrakenProviderFiredacQuery.Commit;
begin
  FConnection.Commit;
end;

end.
