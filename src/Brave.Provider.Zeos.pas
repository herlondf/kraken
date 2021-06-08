unit Brave.Provider.Zeos;

interface

uses
  System.SysUtils,
  System.Generics.Collections,

  ZConnection,

  Brave.Consts,
  Brave.Provider.Zeos.Settings,
  Brave.Provider.Zeos.Query,
  Brave.Provider.Types;

type
  TBraveQuerys = TObjectList<TBraveProviderZeosQuery>;

  TBraveProviderZeos = class(TZConnection)
  strict private
    FConnection: TZConnection;
  private
    FBraveQuerys: TBraveQuerys;
    FBraveProviderSettings: TBraveProviderZeosSettings;

    procedure _SetProviderType(AProviderType: TBraveProviderType);
  public
    constructor Create(AProviderType: TBraveProviderType);
    destructor Destroy; override;

    function Connect    : TBraveProviderZeos;
    function Disconnect : TBraveProviderZeos;
    function Connected  : Boolean;

    function Settings: TBraveProviderZeosSettings;
    function Query(AIndex: Integer = 0): TBraveProviderZeosQuery;
  end;

implementation

{ TBraveProviderZeos }

constructor TBraveProviderZeos.Create(AProviderType: TBraveProviderType);
begin
  FConnection := TZConnection.Create(nil);

  _SetProviderType( AProviderType );

  FBraveQuerys := TBraveQuerys.Create();
end;

destructor TBraveProviderZeos.Destroy;
begin
  if FConnection <> nil then
    FreeAndNil(FConnection);

  if FBraveProviderSettings <> nil then
    FreeAndNil(FBraveProviderSettings);

  if FBraveQuerys <> nil then
    FreeAndNil(FBraveQuerys);
end;

function TBraveProviderZeos.Connect: TBraveProviderZeos;
begin
  Result := Self;

  try
    if not FConnection.Connected then
      FConnection.Connect;
  except

  end;
end;

function TBraveProviderZeos.Connected: Boolean;
begin
  try
    Result := FConnection.Connected;
  except
    Result := False;
  end;
end;

function TBraveProviderZeos.Disconnect: TBraveProviderZeos;
begin
  Result := Self;
  try
    FConnection.Disconnect;
  except

  end;
end;

function TBraveProviderZeos.Query(AIndex: Integer = 0): TBraveProviderZeosQuery;
begin
  try
    Result := FBraveQuerys.Items[AIndex];
  except
    Result := FBraveQuerys.Items[ FBraveQuerys.Add( TBraveProviderZeosQuery.Create(FConnection) ) ]
  end;
end;

function TBraveProviderZeos.Settings: TBraveProviderZeosSettings;
begin
  if FBraveProviderSettings = nil then
    FBraveProviderSettings := TBraveProviderZeosSettings.Create(FConnection);

  Result := FBraveProviderSettings;
end;

procedure TBraveProviderZeos._SetProviderType(AProviderType: TBraveProviderType);
begin
  {$IFNDEF Brave_FIREDAC}
  case AProviderType of
    ptPostgres: TBraveProviderTypes.Postgres(FConnection);
    ptFirebird: TBraveProviderTypes.Firebird(FConnection);
  end;
  {$ENDIF}
end;

end.
