unit Kraken.Provider.Zeos;

interface

uses
  System.SysUtils,
  System.Generics.Collections,

  ZConnection,

  Kraken.Consts,
  Kraken.Provider.Zeos.Settings,
  Kraken.Provider.Zeos.Query,
  Kraken.Provider.Types;

type
  TKrakenQuerys = TObjectList<TKrakenProviderZeosQuery>;

  TKrakenProviderZeos = class(TZConnection)
  strict private
    FConnection: TZConnection;
  private
    FKrakenQuerys: TKrakenQuerys;
    FKrakenProviderSettings: TKrakenProviderZeosSettings;

    procedure _SetProviderType(AProviderType: TKrakenProviderType);
  public
    constructor Create(AProviderType: TKrakenProviderType);
    destructor Destroy; override;

    function Connect    : TKrakenProviderZeos;
    function Disconnect : TKrakenProviderZeos;
    function Connected  : Boolean;

    function Settings: TKrakenProviderZeosSettings;
    function Query(AIndex: Integer = 0): TKrakenProviderZeosQuery;
  end;

implementation

{ TKrakenProviderZeos }

constructor TKrakenProviderZeos.Create(AProviderType: TKrakenProviderType);
begin
  FConnection := TZConnection.Create(nil);

  _SetProviderType( AProviderType );

  FKrakenQuerys := TKrakenQuerys.Create();
end;

destructor TKrakenProviderZeos.Destroy;
begin
  if FConnection <> nil then
    FreeAndNil(FConnection);

  if FKrakenProviderSettings <> nil then
    FreeAndNil(FKrakenProviderSettings);

  if FKrakenQuerys <> nil then
    FreeAndNil(FKrakenQuerys);
end;

function TKrakenProviderZeos.Connect: TKrakenProviderZeos;
begin
  Result := Self;

  try
    if not FConnection.Connected then
      FConnection.Connect;
  except

  end;
end;

function TKrakenProviderZeos.Connected: Boolean;
begin
  try
    Result := FConnection.Connected;
  except
    Result := False;
  end;
end;

function TKrakenProviderZeos.Disconnect: TKrakenProviderZeos;
begin
  Result := Self;
  try
    FConnection.Disconnect;
  except

  end;
end;

function TKrakenProviderZeos.Query(AIndex: Integer = 0): TKrakenProviderZeosQuery;
begin
  try
    Result := FKrakenQuerys.Items[AIndex];
  except
    Result := FKrakenQuerys.Items[ FKrakenQuerys.Add( TKrakenProviderZeosQuery.Create(FConnection) ) ]
  end;
end;

function TKrakenProviderZeos.Settings: TKrakenProviderZeosSettings;
begin
  if FKrakenProviderSettings = nil then
    FKrakenProviderSettings := TKrakenProviderZeosSettings.Create(FConnection);

  Result := FKrakenProviderSettings;
end;

procedure TKrakenProviderZeos._SetProviderType(AProviderType: TKrakenProviderType);
begin
  {$IFNDEF KRAKEN_FIREDAC}
  case AProviderType of
    ptPostgres: TKrakenProviderTypes.Postgres(FConnection);
    ptFirebird: TKrakenProviderTypes.Firebird(FConnection);
  end;
  {$ENDIF}
end;

end.
