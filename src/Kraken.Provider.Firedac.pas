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
  TKrakenMetas  = TObjectList<TKrakenProviderFiredacMetadata>;

  TKrakenProviderFiredac = class(TFDConnection)
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  strict private
    FDriver: TFDPhysDriverLink;
  private
    FId                    : String;
    FKrakenQuerys          : TKrakenQuerys;
    FKrakenMetadatas       : TKrakenMetas;
    FKrakenProviderSettings: TKrakenProviderFiredacSettings;

    procedure _SetDefaultConfig;
  public
    function GetInstance: TFDConnection;
    function ProviderType(AProviderType: TKrakenProviderType): TKrakenProviderFiredac;
    function Settings: TKrakenProviderFiredacSettings;

    function Id(const Value: String): TKrakenProviderFiredac; overload;
    function Id: String; overload;

    procedure Connect;
    procedure Disconnect;
    procedure StartTransaction;
    procedure Commit;
    procedure Rollback;

    function Querys: TKrakenQuerys;
    function Query: TKrakenProviderFiredacQuery; overload;
    function Query(const AId: String): TKrakenProviderFiredacQuery; overload;
    function Query(const AId: Integer ): TKrakenProviderFiredacQuery; overload;

    function Metadata: TKrakenProviderFiredacMetadata; overload;
    function Metadata(const AId: String): TKrakenProviderFiredacMetadata; overload;
    function Metadata(const AId: Integer): TKrakenProviderFiredacMetadata; overload;
  end;

implementation

{ TKrakenProviderFiredac }

constructor TKrakenProviderFiredac.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  _SetDefaultConfig;

  FKrakenQuerys := TKrakenQuerys.Create();
  FKrakenMetadatas := TKrakenMetas.Create();
end;

destructor TKrakenProviderFiredac.Destroy;
begin
  if FKrakenProviderSettings <> nil then
    FreeAndNil(FKrakenProviderSettings);

  if FKrakenQuerys <> nil then
    FreeAndNil(FKrakenQuerys);

  if FDriver <> nil then
    FreeAndNil(FDriver);

  if FKrakenMetadatas <> nil then
    FreeAndNil(FKrakenMetadatas);

  inherited;
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

function TKrakenProviderFiredac.GetInstance: TFDConnection;
begin
  Result := TFDConnection(Self);
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

function TKrakenProviderFiredac.Settings: TKrakenProviderFiredacSettings;
begin
  if FKrakenProviderSettings = nil then
    FKrakenProviderSettings := TKrakenProviderFiredacSettings.Create(Self);

  Result := FKrakenProviderSettings;
end;

function TKrakenProviderFiredac.Id: String;
begin
  Result := FId;
end;

function TKrakenProviderFiredac.Id(const Value: String): TKrakenProviderFiredac;
begin
  Result := Self;
  FId    := Value;
  Self.Name := 'FDConn' + FId;
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

function TKrakenProviderFiredac.Querys: TKrakenQuerys;
begin
  Result := FKrakenQuerys;
end;

function TKrakenProviderFiredac.Query: TKrakenProviderFiredacQuery;
begin
  if FKrakenQuerys.Count > 0 then
    Result := FKrakenQuerys.First
  else
    Result := FKrakenQuerys.Items[ FKrakenQuerys.Add( TKrakenProviderFiredacQuery.Create(Self) ) ];
end;

function TKrakenProviderFiredac.Query(const AId: String): TKrakenProviderFiredacQuery;
var
  LKrakenQuery: TKrakenProviderFiredacQuery;
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
    LKrakenQuery := FKrakenQuerys.Items[ FKrakenQuerys.Add( TKrakenProviderFiredacQuery.Create(Self) ) ];
    LKrakenQuery.Id(AId);
    Result := LKrakenQuery;
  end;
end;

function TKrakenProviderFiredac.Query(const AId: Integer): TKrakenProviderFiredacQuery;
begin
  Result := Query( IntToStr( AId ) );
end;


function TKrakenProviderFiredac.Metadata: TKrakenProviderFiredacMetadata;
begin
  if FKrakenMetadatas.Count > 0 then
    Result := FKrakenMetadatas.First
  else
    Result := FKrakenMetadatas.Items[ FKrakenMetadatas.Add( TKrakenProviderFiredacMetadata.Create(Self) ) ];
end;

function TKrakenProviderFiredac.Metadata(const AId: String): TKrakenProviderFiredacMetadata;
var
  LKrakenMetadata: TKrakenProviderFiredacMetadata;
begin
  Result := nil;

  for LKrakenMetadata in FKrakenMetadatas do
  begin
    if AnsiUppercase(LKrakenMetadata.Id) = AnsiUpperCase(AId) then
    begin
      Result := LKrakenMetadata;
      Break;
    end;
  end;

  if Result = nil then
  begin
    LKrakenMetadata := FKrakenMetadatas.Items[ FKrakenMetadatas.Add( TKrakenProviderFiredacMetadata.Create(Self) ) ];
    LKrakenMetadata.Id(AId);
    Result := LKrakenMetadata;
  end;
end;

function TKrakenProviderFiredac.Metadata(const AId: Integer): TKrakenProviderFiredacMetadata;
begin
  Result := Metadata(IntToStr(AId));
end;

end.
