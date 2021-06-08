unit Brave.Provider.Firedac;

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

  Brave.Consts,
  Brave.Provider.Firedac.Settings,
  Brave.Provider.Firedac.Query,
  Brave.Provider.Types,
  Brave.Provider.Firedac.Metadata;

type
  TBraveQuerys = TObjectList<TBraveProviderFiredacQuery>;

  TBraveProviderFiredac = class(TFDConnection)
  strict private
    FConnection: TFDConnection;
    FDriver: TFDPhysDriverLink;
  private
    FBraveQuerys: TBraveQuerys;
    FBraveProviderSettings: TBraveProviderFiredacSettings;
    FBraveMetadata: TBraveProviderFiredacMetadata;

    procedure _SetProviderType(AProviderType: TBraveProviderType);
    procedure _SetDefaultConfig;
  public
    constructor Create(AProviderType: TBraveProviderType);
    destructor Destroy; override;

    function Connect    : TBraveProviderFiredac;
    function Disconnect : TBraveProviderFiredac;
    function Connected  : Boolean;

    function Settings: TBraveProviderFiredacSettings;
    function Query(AIndex: Integer = 0): TBraveProviderFiredacQuery;
    function Metadata: TBraveProviderFiredacMetadata;

  end;

implementation

{ TBraveProviderFiredac }

constructor TBraveProviderFiredac.Create(AProviderType: TBraveProviderType);
begin
  FConnection := TFDConnection.Create(nil);

  _SetProviderType( AProviderType );

  FBraveQuerys := TBraveQuerys.Create();
end;

destructor TBraveProviderFiredac.Destroy;
begin
  if FConnection <> nil then
    FreeAndNil(FConnection);

  if FBraveProviderSettings <> nil then
    FreeAndNil(FBraveProviderSettings);

  if FBraveQuerys <> nil then
    FreeAndNil(FBraveQuerys);

  if FDriver <> nil then
    FreeAndNil(FDriver);

  if FBraveMetadata <> nil then
    FreeAndNil(FBraveMetadata);
end;

function TBraveProviderFiredac.Connect: TBraveProviderFiredac;
begin
  Result := Self;

  try
    if not FConnection.Connected then
      FConnection.Connected := True;
  except

  end;
end;

function TBraveProviderFiredac.Connected: Boolean;
begin
  try
    Result := FConnection.Connected;
  except
    Result := False;
  end;
end;

function TBraveProviderFiredac.Disconnect: TBraveProviderFiredac;
begin
  Result := Self;
  try
    FConnection.Connected := False;
  except

  end;
end;

function TBraveProviderFiredac.Query(AIndex: Integer = 0): TBraveProviderFiredacQuery;
begin
  try
    Result := FBraveQuerys.Items[AIndex];
  except
    Result := FBraveQuerys.Items[ FBraveQuerys.Add( TBraveProviderFiredacQuery.Create(FConnection) ) ]
  end;
end;

function TBraveProviderFiredac.Settings: TBraveProviderFiredacSettings;
begin
  if FBraveProviderSettings = nil then
    FBraveProviderSettings := TBraveProviderFiredacSettings.Create(FConnection);

  Result := FBraveProviderSettings;
end;

procedure TBraveProviderFiredac._SetDefaultConfig;
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

procedure TBraveProviderFiredac._SetProviderType(AProviderType: TBraveProviderType);
begin
  {$IF DEFINED (BRAVE_FIREDAC)}
  case AProviderType of
    ptPostgres: TBraveProviderTypes.Postgres(FConnection, FDriver);
    ptFirebird: TBraveProviderTypes.Firebird(FConnection, FDriver);
  end;
  {$ENDIF}
end;

function TBraveProviderFiredac.Metadata: TBraveProviderFiredacMetadata;
begin
  if FBraveMetadata = nil then
    FBraveMetadata := TBraveProviderFiredacMetadata.Create(Self);

  Result := FBraveMetadata;
end;

end.
