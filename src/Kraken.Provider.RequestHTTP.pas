unit Kraken.Provider.RequestHTTP;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,

  ZConnection,

  Kraken.Consts,
  Kraken.Provider.RequestHTTP.Settings,
  Kraken.Provider.RequestHTTP.Query,
  Kraken.Provider.Types;

type
  TKrakenQuerys = TObjectList<TKrakenProviderRequestHTTPQuery>;

  TKrakenProviderRequestHTTP = class(TZConnection)
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
    private
      FId                    : String;
      FKrakenQuerys          : TKrakenQuerys;
      FKrakenProviderSettings: TKrakenProviderRequestHTTPSettings;
      FStartTransaction      : Boolean;

      procedure _SetDefaultConfig;
    public
      function GetInstance: TZConnection;
      function ProviderType(AProviderType: TKrakenProviderType): TKrakenProviderRequestHTTP;
      function Settings: TKrakenProviderRequestHTTPSettings;

      function Id(const Value: String): TKrakenProviderRequestHTTP; overload;
      function Id: String; overload;

      procedure Connect;
      procedure Disconnect;
      procedure StartTransaction;
      procedure Commit;
      procedure Rollback;

      function Querys: TKrakenQuerys;
      function Query: TKrakenProviderRequestHTTPQuery; overload;
      function Query(const AId: String): TKrakenProviderRequestHTTPQuery; overload;
      function Query(const AId: Integer): TKrakenProviderRequestHTTPQuery; overload;

  end;

implementation

{ TKrakenProviderRequestHTTP }

procedure TKrakenProviderRequestHTTP.Commit;
begin
  if FKrakenQuerys.Count > 0 then
    FKrakenQuerys.First.StartTransaction := False;
end;

procedure TKrakenProviderRequestHTTP.Connect;
begin

end;

constructor TKrakenProviderRequestHTTP.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  _SetDefaultConfig;

  FKrakenQuerys := TKrakenQuerys.Create();
end;

destructor TKrakenProviderRequestHTTP.Destroy;
begin
  if FKrakenProviderSettings <> nil then
    FreeAndNil(FKrakenProviderSettings);

  if FKrakenQuerys <> nil then
    FreeAndNil(FKrakenQuerys);

  inherited;
end;

procedure TKrakenProviderRequestHTTP.Disconnect;
begin

end;

function TKrakenProviderRequestHTTP.GetInstance: TZConnection;
begin
  Result := TZConnection(Self);
end;

function TKrakenProviderRequestHTTP.Id: String;
begin
  Result := FId;
end;

function TKrakenProviderRequestHTTP.Id(const Value: String): TKrakenProviderRequestHTTP;
begin
  Result := Self;
  FId    := Value;
  Self.Name := 'ZConn' + FId;
end;

function TKrakenProviderRequestHTTP.ProviderType(AProviderType: TKrakenProviderType): TKrakenProviderRequestHTTP;
begin
  Result := Self;
end;

function TKrakenProviderRequestHTTP.Query(const AId: Integer): TKrakenProviderRequestHTTPQuery;
begin
  Result := Query( IntToStr( AId ) );
end;

function TKrakenProviderRequestHTTP.Query(const AId: String): TKrakenProviderRequestHTTPQuery;
var
  LKrakenQuery: TKrakenProviderRequestHTTPQuery;
begin
  Result := nil;

  for LKrakenQuery in FKrakenQuerys do
  begin
    if AnsiUpperCase(LKrakenQuery.Id) = AnsiUpperCase(AId) then
    begin
      Result := LKrakenQuery;
      Break;
    end;
  end;

  if Result = nil then
  begin
    LKrakenQuery := FKrakenQuerys.Items[ FKrakenQuerys.Add( TKrakenProviderRequestHTTPQuery.Create(Self) ) ];
    LKrakenQuery.Id(AId);
    Result := LKrakenQuery;
  end;
end;

function TKrakenProviderRequestHTTP.Query: TKrakenProviderRequestHTTPQuery;
begin
  if FKrakenQuerys.Count > 0 then
    Result := FKrakenQuerys.First
  else
    Result := FKrakenQuerys.Items[ FKrakenQuerys.Add( TKrakenProviderRequestHTTPQuery.Create(Self) ) ];
end;

function TKrakenProviderRequestHTTP.Querys: TKrakenQuerys;
begin
  Result := FKrakenQuerys;
end;

procedure TKrakenProviderRequestHTTP.Rollback;
begin
  if FKrakenQuerys.Count > 0 then
    FKrakenQuerys.First.StartTransaction := False;
end;

function TKrakenProviderRequestHTTP.Settings: TKrakenProviderRequestHTTPSettings;
begin
  if FKrakenProviderSettings = nil then
    FKrakenProviderSettings := TKrakenProviderRequestHTTPSettings.Create(Self);

  Result := FKrakenProviderSettings;
end;

procedure TKrakenProviderRequestHTTP.StartTransaction;
begin
  if FKrakenQuerys.Count > 0 then
    FKrakenQuerys.First.StartTransaction := true;
end;

procedure TKrakenProviderRequestHTTP._SetDefaultConfig;
begin

end;

end.
