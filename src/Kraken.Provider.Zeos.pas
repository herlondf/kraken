unit Kraken.Provider.Zeos;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,

  ZConnection,
  ZAbstractConnection,
  ZDbcIntfs,

  Kraken.Consts,
  Kraken.Provider.Zeos.Settings,
  Kraken.Provider.Zeos.Query,
  Kraken.Provider.Types;

type
  TKrakenQuerys = TObjectList<TKrakenProviderZeosQuery>;

  TKrakenProviderZeos = class(TZConnection)
  private
    FIdentification: String;
    FKrakenQuerys: TKrakenQuerys;
    FKrakenProviderSettings: TKrakenProviderZeosSettings;

    procedure _SetDefaultConfig;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetInstance: TZConnection;

    function ProviderType(AProviderType: TKrakenProviderType): TKrakenProviderZeos;

    function Identification(const Value: String): TKrakenProviderZeos; overload;
    function Identification: String; overload;

    procedure Connect;
    procedure Disconnect;
    procedure StartTransaction;
    procedure Commit;
    procedure Rollback;

    function Settings: TKrakenProviderZeosSettings;
    function Query(AIndex: Integer): TKrakenProviderZeosQuery; overload;
    function Query: TKrakenProviderZeosQuery; overload;
  end;

implementation

{ TKrakenProviderZeos }

constructor TKrakenProviderZeos.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  _SetDefaultConfig;

  FKrakenQuerys := TKrakenQuerys.Create();
end;

destructor TKrakenProviderZeos.Destroy;
begin
  if FKrakenProviderSettings <> nil then
    FreeAndNil(FKrakenProviderSettings);

  if FKrakenQuerys <> nil then
    FreeAndNil(FKrakenQuerys);

  inherited;
end;

procedure TKrakenProviderZeos._SetDefaultConfig;
begin
  GetInstance.AutoCommit := False;
  GetInstance.Properties.Values['codepage'] := 'utf-8';
  GetInstance.TransactIsolationLevel := tiReadUncommitted;
end;

function TKrakenProviderZeos.GetInstance: TZConnection;
begin
  Result := TZConnection(Self);
end;

function TKrakenProviderZeos.Identification: String;
begin
  Result := FIdentification;
end;

function TKrakenProviderZeos.ProviderType(AProviderType: TKrakenProviderType): TKrakenProviderZeos;
begin
  Result := Self;
  {$IFNDEF KRAKEN_FIREDAC}
  case AProviderType of
    ptPostgres: TKrakenProviderTypes.Postgres(Self);
    ptFirebird: TKrakenProviderTypes.Firebird(Self);
  end;
  {$ENDIF}
end;

function TKrakenProviderZeos.Query: TKrakenProviderZeosQuery;
begin
  try
    Result := FKrakenQuerys.First;
  except
    Result := FKrakenQuerys.Items[ FKrakenQuerys.Add( TKrakenProviderZeosQuery.Create(Self) ) ]
  end;
end;

function TKrakenProviderZeos.Identification(const Value: String): TKrakenProviderZeos;
begin
  Result := Self;
  FIdentification := Value;
end;

function TKrakenProviderZeos.Query(AIndex: Integer ): TKrakenProviderZeosQuery;
begin
  try
    Result := FKrakenQuerys.Items[AIndex];
  except
    Result := FKrakenQuerys.Items[ FKrakenQuerys.Add( TKrakenProviderZeosQuery.Create(Self) ) ]
  end;
end;

function TKrakenProviderZeos.Settings: TKrakenProviderZeosSettings;
begin
  if FKrakenProviderSettings = nil then
    FKrakenProviderSettings := TKrakenProviderZeosSettings.Create(Self);

  Result := FKrakenProviderSettings;
end;

procedure TKrakenProviderZeos.Connect;
begin
  try
    if not Connected then
      GetInstance.Connect;
  except

  end;
end;

procedure TKrakenProviderZeos.Disconnect;
begin
  try
    GetInstance.Disconnect;
  except

  end;
end;

procedure TKrakenProviderZeos.StartTransaction;
begin
  try
    GetInstance.StartTransaction;
  except

  end;
end;

procedure TKrakenProviderZeos.Commit;
begin
  try
    if GetInstance.InTransaction then
      GetInstance.Commit;
  except

  end;
end;

procedure TKrakenProviderZeos.Rollback;
begin
  try
    GetInstance.Rollback;
  except

  end;

end;

end.
