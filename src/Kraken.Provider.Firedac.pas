unit Kraken.Provider.Firedac;

interface

uses
  System.SysUtils,
  System.Generics.Collections,

  FireDAC.Comp.Client,
  FireDAC.Comp.UI,
  FireDAC.UI.Intf,
  FireDAC.VCLUI.Async,
  FireDAC.VCLUI.Wait,
  FireDAC.Phys,
  FireDAC.Dapt,

  FireDAC.Stan.Intf,

  FireDAC.Moni.Base,
  FireDAC.Moni.Custom,
  FireDAC.Moni.FlatFile,
  FireDAC.Comp.BatchMove,
  FireDAC.Comp.DataSet,
  FireDAC.Comp.BatchMove.DataSet,
  FireDAC.Comp.BatchMove.SQL,
  FireDAC.DatS,

  Kraken.Consts,
  Kraken.Provider.Firedac.Settings,
  Kraken.Provider.Firedac.Query,
  Kraken.Provider.Types,
  Kraken.Provider.Firedac.Metadata;

type
  TKrakenQuerys = TObjectList<TKrakenProviderFiredacQuery>;

  TKrakenProviderFiredac = class(TFDConnection)
  strict private
    FConnection: TFDConnection;
    FDriver: TFDPhysDriverLink;
  private
    FKrakenQuerys: TKrakenQuerys;
    FKrakenProviderSettings: TKrakenProviderFiredacSettings;
    FKrakenMetadata: TKrakenProviderFiredacMetadata;

    procedure _SetProviderType(AProviderType: TKrakenProviderType);
    procedure _SetDefaultConfig;
  public
    constructor Create(AProviderType: TKrakenProviderType);
    destructor Destroy; override;

    function Connect    : TKrakenProviderFiredac;
    function Disconnect : TKrakenProviderFiredac;
    function Connected  : Boolean;

    function Settings: TKrakenProviderFiredacSettings;
    function Query(AIndex: Integer = 0): TKrakenProviderFiredacQuery;
    function Metadata: TKrakenProviderFiredacMetadata;

  end;

implementation

{ TKrakenProviderFiredac }

constructor TKrakenProviderFiredac.Create(AProviderType: TKrakenProviderType);
begin
  FConnection := TFDConnection.Create(nil);

  _SetProviderType( AProviderType );

  FKrakenQuerys := TKrakenQuerys.Create();
end;

destructor TKrakenProviderFiredac.Destroy;
begin
  if FConnection <> nil then
    FreeAndNil(FConnection);

  if FKrakenProviderSettings <> nil then
    FreeAndNil(FKrakenProviderSettings);

  if FKrakenQuerys <> nil then
    FreeAndNil(FKrakenQuerys);

  if FDriver <> nil then
    FreeAndNil(FDriver);

  if FKrakenMetadata <> nil then
    FreeAndNil(FKrakenMetadata);
end;

function TKrakenProviderFiredac.Connect: TKrakenProviderFiredac;
begin
  Result := Self;

  try
    if not FConnection.Connected then
      FConnection.Connected := True;
  except

  end;
end;

function TKrakenProviderFiredac.Connected: Boolean;
begin
  try
    Result := FConnection.Connected;
  except
    Result := False;
  end;
end;

function TKrakenProviderFiredac.Disconnect: TKrakenProviderFiredac;
begin
  Result := Self;
  try
    FConnection.Connected := False;
  except

  end;
end;

function TKrakenProviderFiredac.Query(AIndex: Integer = 0): TKrakenProviderFiredacQuery;
begin
  try
    Result := FKrakenQuerys.Items[AIndex];
  except
    Result := FKrakenQuerys.Items[ FKrakenQuerys.Add( TKrakenProviderFiredacQuery.Create(FConnection) ) ]
  end;
end;

function TKrakenProviderFiredac.Settings: TKrakenProviderFiredacSettings;
begin
  if FKrakenProviderSettings = nil then
    FKrakenProviderSettings := TKrakenProviderFiredacSettings.Create(FConnection);

  Result := FKrakenProviderSettings;
end;

procedure TKrakenProviderFiredac._SetDefaultConfig;
begin
  FConnection.LoginPrompt := False;

  {Configuracao obrigatoria do autorecover de conexao}
  FConnection.ResourceOptions.AutoReconnect  := True;
  FConnection.ResourceOptions.KeepConnection := True;

  { No caso do PostgreSQL, foi usado para capturar nome da tabela em querys        }
  { http://docwiki.embarcadero.com/RADStudio/Sydney/en/Extended_Metadata_(FireDAC) }
  FConnection.Params.Add('ExtendedMetadata=True');

  {Configuracao de rodar a query em thread separada - amNonBlocking}
  FConnection.ResourceOptions.CmdExecMode := amBlocking;

  TxOptions.AutoCommit := False;
end;

procedure TKrakenProviderFiredac._SetProviderType(AProviderType: TKrakenProviderType);
begin
  {$IF DEFINED (KRAKEN_FIREDAC)}
  case AProviderType of
    ptPostgres: TKrakenProviderTypes.Postgres(FConnection, FDriver);
    ptFirebird: TKrakenProviderTypes.Firebird(FConnection, FDriver);
  end;
  {$ENDIF}
end;

function TKrakenProviderFiredac.Metadata: TKrakenProviderFiredacMetadata;
begin
  if FKrakenMetadata = nil then
    FKrakenMetadata := TKrakenProviderFiredacMetadata.Create(Self);

  Result := FKrakenMetadata;
end;

end.
