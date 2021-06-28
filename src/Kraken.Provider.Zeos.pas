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
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  private
    FId                    : String;
    FKrakenQuerys          : TKrakenQuerys;
    FKrakenProviderSettings: TKrakenProviderZeosSettings;

    procedure _SetDefaultConfig;
  public
    function GetInstance: TZConnection;
    function ProviderType(AProviderType: TKrakenProviderType): TKrakenProviderZeos;
    function Settings: TKrakenProviderZeosSettings;

    function Id(const Value: String): TKrakenProviderZeos; overload;
    function Id: String; overload;

    procedure Connect;
    procedure Disconnect;
    procedure StartTransaction;
    procedure Commit;
    procedure Rollback;

    function Querys: TKrakenQuerys;
    function Query: TKrakenProviderZeosQuery; overload;
    function Query(const AId: String): TKrakenProviderZeosQuery; overload;
    function Query(const AId: Integer): TKrakenProviderZeosQuery; overload;
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

function TKrakenProviderZeos.Settings: TKrakenProviderZeosSettings;
begin
  if FKrakenProviderSettings = nil then
    FKrakenProviderSettings := TKrakenProviderZeosSettings.Create(Self);

  Result := FKrakenProviderSettings;
end;

function TKrakenProviderZeos.Id: String;
begin
  Result := FId;
end;

function TKrakenProviderZeos.Id(const Value: String): TKrakenProviderZeos;
begin
  Result := Self;
  FId    := Value;
  Self.Name := 'ZConn' + FId;
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
    if GetInstance.InTransaction then
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

function TKrakenProviderZeos.Querys: TKrakenQuerys;
begin
  Result := FKrakenQuerys;
end;

function TKrakenProviderZeos.Query: TKrakenProviderZeosQuery;
begin
  if FKrakenQuerys.Count > 0 then
    Result := FKrakenQuerys.First
  else
    Result := FKrakenQuerys.Items[ FKrakenQuerys.Add( TKrakenProviderZeosQuery.Create(Self) ) ];
end;

function TKrakenProviderZeos.Query(const AId: String): TKrakenProviderZeosQuery;
var
  LKrakenQuery: TKrakenProviderZeosQuery;
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
    LKrakenQuery := FKrakenQuerys.Items[ FKrakenQuerys.Add( TKrakenProviderZeosQuery.Create(Self) ) ];
    LKrakenQuery.Id(AId);
    Result := LKrakenQuery;
  end;
end;

function TKrakenProviderZeos.Query(const AId: Integer): TKrakenProviderZeosQuery;
begin
  Result := Query( IntToStr( AId ) );
end;


end.
