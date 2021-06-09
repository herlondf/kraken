unit Kraken.Provider.Firedac;

interface

uses
  System.SysUtils,
  System.Classes,
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
    FDriver: TFDPhysDriverLink;
  private
    FIdentification: String;
    FKrakenQuerys: TKrakenQuerys;
    FKrakenMetadata: TKrakenProviderFiredacMetadata;
    FKrakenProviderSettings: TKrakenProviderFiredacSettings;

    procedure _SetDefaultConfig;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetInstance: TFDConnection;

    function ProviderType(AProviderType: TKrakenProviderType): TKrakenProviderFiredac;

    function Identification(const Value: String): TKrakenProviderFiredac; overload;
    function Identification: String; overload;

    procedure Connect;
    procedure Disconnect;
    procedure StartTransaction;
    procedure Commit;
    procedure Rollback;

    function Settings: TKrakenProviderFiredacSettings;
    function Query(AIndex: Integer): TKrakenProviderFiredacQuery; overload;
    function Query: TKrakenProviderFiredacQuery; overload;
    function Metadata: TKrakenProviderFiredacMetadata;
  end;

implementation

{ TKrakenProviderFiredac }

constructor TKrakenProviderFiredac.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  _SetDefaultConfig;

  FKrakenQuerys := TKrakenQuerys.Create();
end;

destructor TKrakenProviderFiredac.Destroy;
begin
  if FKrakenProviderSettings <> nil then
    FreeAndNil(FKrakenProviderSettings);

  if FKrakenQuerys <> nil then
    FreeAndNil(FKrakenQuerys);

  if FDriver <> nil then
    FreeAndNil(FDriver);

  if FKrakenMetadata <> nil then
    FreeAndNil(FKrakenMetadata);

  inherited;
end;

procedure TKrakenProviderFiredac.Connect;
begin
  try
    if not Connected then
      GetInstance.Connected := True;
  except

  end;
end;

procedure TKrakenProviderFiredac.Disconnect;
begin
  try
    GetInstance.Close;
  except

  end;
end;

procedure TKrakenProviderFiredac.StartTransaction;
begin
  try
    GetInstance.StartTransaction;
  except

  end;
end;

procedure TKrakenProviderFiredac.Commit;
begin
  try
    GetInstance.Commit;
  except

  end;
end;

procedure TKrakenProviderFiredac.Rollback;
begin
  try
    GetInstance.Rollback;
  except

  end;
end;


function TKrakenProviderFiredac.GetInstance: TFDConnection;
begin
  Result := TFDConnection(Self);
end;

function TKrakenProviderFiredac.Identification: String;
begin
  Result := FIdentification;
end;

function TKrakenProviderFiredac.Metadata: TKrakenProviderFiredacMetadata;
begin
  if FKrakenMetadata = nil then
    FKrakenMetadata := TKrakenProviderFiredacMetadata.Create(Self);

  Result := FKrakenMetadata;
end;

function TKrakenProviderFiredac.ProviderType(AProviderType: TKrakenProviderType): TKrakenProviderFiredac;
begin
  Result := Self;
  {$IF DEFINED (KRAKEN_FIREDAC)}
  case AProviderType of
    ptPostgres: TKrakenProviderTypes.Postgres(Self, FDriver);
    ptFirebird: TKrakenProviderTypes.Firebird(Self, FDriver);
  end;
  {$ENDIF}
end;

function TKrakenProviderFiredac.Query: TKrakenProviderFiredacQuery;
begin
  try
    Result := FKrakenQuerys.First;
  except
    Result := FKrakenQuerys.Items[ FKrakenQuerys.Add( TKrakenProviderFiredacQuery.Create(Self) ) ]
  end;
end;

function TKrakenProviderFiredac.Identification(const Value: String): TKrakenProviderFiredac;
begin
  Result := Self;
  FIdentification := Value;
end;

function TKrakenProviderFiredac.Query(AIndex: Integer): TKrakenProviderFiredacQuery;
begin
  try
    Result := FKrakenQuerys.Items[AIndex];
  except
    Result := FKrakenQuerys.Items[ FKrakenQuerys.Add( TKrakenProviderFiredacQuery.Create(Self) ) ]
  end;
end;

function TKrakenProviderFiredac.Settings: TKrakenProviderFiredacSettings;
begin
  if FKrakenProviderSettings = nil then
    FKrakenProviderSettings := TKrakenProviderFiredacSettings.Create(Self);

  Result := FKrakenProviderSettings;
end;

procedure TKrakenProviderFiredac._SetDefaultConfig;
begin
  GetInstance.LoginPrompt := False;

  {Configuracao obrigatoria do autorecover de conexao}
  GetInstance.ResourceOptions.AutoReconnect  := True;
  GetInstance.ResourceOptions.KeepConnection := True;

  { No caso do PostgreSQL, foi usado para capturar nome da tabela em querys        }
  { http://docwiki.embarcadero.com/RADStudio/Sydney/en/Extended_Metadata_(FireDAC) }
  GetInstance.Params.Add('ExtendedMetadata=True');

  {Configuracao de rodar a query em thread separada - amNonBlocking}
  GetInstance.ResourceOptions.CmdExecMode := amBlocking;

  GetInstance.TxOptions.AutoCommit := False;
end;

end.
