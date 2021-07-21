unit Kraken.Provider.RequestHTTP;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,

  Kraken.Consts,
  Kraken.Provider.RequestHTTP.Settings,
  Kraken.Provider.RequestHTTP.Query,
  Kraken.Provider.Types;

type
  TKrakenQuerys = TObjectList<TKrakenProviderRequestHTTPQuery>;

  TKrakenProviderRequestHTTP = class
      constructor Create(AOwner: TComponent);
      destructor Destroy; override;
    private
      FId                    : String;
      FKrakenQuerys          : TKrakenQuerys;
      FKrakenProviderSettings: TKrakenProviderRequestHTTPSettings;
      FStartTransaction      : Boolean;

      procedure _SetDefaultConfig;
    public
      function GetInstance: TKrakenProviderRequestHTTP;
      function ProviderType(AProviderType: TKrakenProviderType): TKrakenProviderRequestHTTP;
      function Settings: TKrakenProviderRequestHTTPSettings;

      function Id(const Value: String): TKrakenProviderRequestHTTP; overload;
      function Id: String; overload;

      procedure Connect;
      procedure Disconnect;
      procedure StartTransaction;
      procedure Commit;
      procedure Rollback;
      function  Connected: Boolean;

      function Querys: TKrakenQuerys;
      function Query: TKrakenProviderRequestHTTPQuery; overload;
      function Query(const AId: String): TKrakenProviderRequestHTTPQuery; overload;
      function Query(const AId: Integer): TKrakenProviderRequestHTTPQuery; overload;
  end;

implementation

{ TKrakenProviderRequestHTTP }

constructor TKrakenProviderRequestHTTP.Create(AOwner: TComponent);
begin
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

procedure TKrakenProviderRequestHTTP.Commit;
begin
  if FKrakenQuerys.Count > 0 then
    FKrakenQuerys.First.StartTransaction(False);

  FKrakenQuerys.First.Params.Clear;
end;

procedure TKrakenProviderRequestHTTP.Connect;
begin

end;

function TKrakenProviderRequestHTTP.Connected: Boolean;
begin

end;

procedure TKrakenProviderRequestHTTP.Disconnect;
begin

end;

function TKrakenProviderRequestHTTP.GetInstance: TKrakenProviderRequestHTTP;
begin
  Result := Self;
end;

function TKrakenProviderRequestHTTP.Id: String;
begin
  Result := FId;
end;

function TKrakenProviderRequestHTTP.Id(const Value: String): TKrakenProviderRequestHTTP;
begin
  Result := Self;
  FId    := Value;
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
    LKrakenQuery := FKrakenQuerys.Items[ FKrakenQuerys.Add( TKrakenProviderRequestHTTPQuery.Create ) ];
    LKrakenQuery.Id(AId);
    LKrakenQuery.Endpoint( Settings.URLRemoto );

    Result := LKrakenQuery;
  end;
end;

function TKrakenProviderRequestHTTP.Query: TKrakenProviderRequestHTTPQuery;
begin
  if FKrakenQuerys.Count > 0 then
    Result := FKrakenQuerys.First
  else
  begin
    Result := Query('Default');
  end;
end;

function TKrakenProviderRequestHTTP.Querys: TKrakenQuerys;
begin
  Result := FKrakenQuerys;
end;

procedure TKrakenProviderRequestHTTP.Rollback;
begin
  if FKrakenQuerys.Count > 0 then
    FKrakenQuerys.First.StartTransaction(False);
end;

function TKrakenProviderRequestHTTP.Settings: TKrakenProviderRequestHTTPSettings;
begin
  if FKrakenProviderSettings = nil then
    FKrakenProviderSettings := TKrakenProviderRequestHTTPSettings.Create;

  Result := FKrakenProviderSettings;
end;

procedure TKrakenProviderRequestHTTP.StartTransaction;
begin
  if FKrakenQuerys.Count > 0 then
    FKrakenQuerys.First.StartTransaction(True);
end;

procedure TKrakenProviderRequestHTTP._SetDefaultConfig;
begin

end;

end.
