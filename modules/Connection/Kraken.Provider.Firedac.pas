unit Kraken.Provider.Firedac;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  ShellApi,
  Winapi.Windows,

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

  IdTCPClient,

  Kraken.Log,
  Kraken.Consts,
  Kraken.Provider.Settings,
  Kraken.Provider.Firedac.Query,
  Kraken.Provider.Types,
  Kraken.Provider.Firedac.Metadata,
  Kraken.Migrate;

type
  TKrakenQuerys = TObjectList<TKrakenProviderFiredacQuery>;
  TKrakenMetas  = TObjectList<TKrakenProviderFiredacMetadata>;

  TKrakenProviderFiredac = class(TFDConnection)
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  private
    FId                    : String;
    FKrakenQuerys          : TKrakenQuerys;
    FKrakenMetadatas       : TKrakenMetas;
    FKrakenProviderSettings: TKrakenProviderSettings<TKrakenProviderFiredac>;
    FKrakenProviderTypes   : TKrakenProviderTypes<TKrakenProviderFiredac>;
    FKrakenMigrate         : TKrakenMigrate;

    LIdTCPClient: TIdTCPClient;

    procedure _SetDefaultConfig;
    function GetDeviceName : String;

    ///<summary>
    ///  Teste de conectividade via IP
    ///</summary>
    function ConnectionInternalTest: Boolean;
  public
    /// <summary>
    ///   Retorna instancia do FDConnection;
    /// </summary>
    function GetInstance: TFDConnection;

    /// <summary>
    ///   Define provedor de banco de dados (PostgreSQL, Firebird ou SQLite)
    /// </summary>
    function ProviderType: TKrakenProviderTypes<TKrakenProviderFiredac>;

    /// <summary>
    ///   Define os parametros de conex�o
    /// </summary>
    function Settings: TKrakenProviderSettings<TKrakenProviderFiredac>;

    function Id(const Value: String): TKrakenProviderFiredac; overload;
    function Id: String; overload;

    function  Connect: Boolean;
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

    function Migrate: TKrakenMigrate; overload;
  end;

implementation

{ TKrakenProviderFiredac }

constructor TKrakenProviderFiredac.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  _SetDefaultConfig;

  LIdTCPClient := TIdTCPClient.Create(Self);

  FKrakenQuerys := TKrakenQuerys.Create();
  FKrakenMetadatas := TKrakenMetas.Create();
end;

destructor TKrakenProviderFiredac.Destroy;
begin
  if FKrakenProviderSettings <> nil then
    FreeAndNil(FKrakenProviderSettings);

  if FKrakenProviderTypes <> nil then
    FreeAndNil(FKrakenProviderTypes);

  if FKrakenQuerys <> nil then
    FreeAndNil(FKrakenQuerys);

  if FKrakenMetadatas <> nil then
    FreeAndNil(FKrakenMetadatas);

  if Assigned(LIdTCPClient) then
    FreeAndNil(LIdTCPClient);

  if Assigned(FKrakenMigrate) then
    FreeAndNil(FKrakenMigrate);

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

function TKrakenProviderFiredac.ProviderType: TKrakenProviderTypes<TKrakenProviderFiredac>;
begin
  if not Assigned(FKrakenProviderTypes) then
    FKrakenProviderTypes := TKrakenProviderTypes<TKrakenProviderFiredac>.Create(Self);
  Result := FKrakenProviderTypes;
end;

function TKrakenProviderFiredac.Settings: TKrakenProviderSettings<TKrakenProviderFiredac>;
begin
  if FKrakenProviderSettings = nil then
    FKrakenProviderSettings := TKrakenProviderSettings<TKrakenProviderFiredac>.Create(Self);
  Result := FKrakenProviderSettings;

  //Params.Add('application_name=' + Copy( ExtractFileName( ParamStr(0) ),  1, Pos('.', ExtractFileName(ParamStr(0)))-1) + '-' + id + '-' + GetDeviceName );
end;

function TKrakenProviderFiredac.GetDeviceName : String;
var ipbuffer : string;
      nsize : dword;
begin
   nsize := 255;
   SetLength(ipbuffer,nsize);
   if GetComputerName(pchar(ipbuffer),nsize) then
      result := ipbuffer;
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

function TKrakenProviderFiredac.Connect: Boolean;
begin
  Result := False;
  try
    try
      if ( not Connected ) and ( ConnectionInternalTest ) then
        GetInstance.Connected := True;
    finally
      Result := True;
    end;
  except
    on e: exception do
    begin
      KrakenLOG.Error(E.Message);
      raise;
    end;
  end;
end;

function TKrakenProviderFiredac.ConnectionInternalTest: Boolean;
begin
  Result := LIdTCPClient.Connected;

  if LIdTCPClient.Connected then Exit;

  try
    try
      LIdTCPClient.Host           := Settings.Host;
      LIdTCPClient.Port           := Settings.Port;
      LIdTCPClient.ConnectTimeout := Settings.TimeOut;
      LIdTCPClient.Connect;
    finally
      Result := LIdTCPClient.Connected;
    end;
  except
    on e: exception do
    begin
      KrakenLOG.Error(E.Message);
      raise;
    end;
  end;
end;

procedure TKrakenProviderFiredac.Disconnect;
begin
  try
    GetInstance.Close;
  except
    on e: exception do
    begin
      KrakenLOG.Error(E.Message);
      raise;
    end;
  end;
end;

procedure TKrakenProviderFiredac.StartTransaction;
var
  LSQL: string;
begin
  try
    if GetInstance.TxOptions.AutoCommit then
    begin
      if not GetInstance.InTransaction then
        GetInstance.StartTransaction
    end
    else
    begin
      LSQL := Query.SQL.Text;
      Query.SQL.Text := 'BEGIN';
      Query.ExecSQL;
      Query.SQL.Text := LSQL;
    end;
  except
    on e: exception do
    begin
      Rollback;
      if LSQL <> '' then Query.SQL.Text := LSQL;
      KrakenLOG.Error(E.Message);
      raise;
    end;
  end;
end;

procedure TKrakenProviderFiredac.Commit;
var
  LSQL: String;
begin
  try
    if GetInstance.TxOptions.AutoCommit then
    begin
      if GetInstance.InTransaction then
        GetInstance.Commit
    end
    else
    begin
      LSQL := Query.SQL.Text;
      Query.SQL.Text := 'COMMIT';
      Query.ExecSQL;
      Query.SQL.Text := LSQL;
    end;
  except
    on e: exception do
    begin
      Rollback;
      if LSQL <> '' then Query.SQL.Text := LSQL;
      KrakenLOG.Error(E.Message);
      raise;
    end;
  end;
end;

procedure TKrakenProviderFiredac.Rollback;
var
  LSQL: String;
begin
  try
    if GetInstance.TxOptions.AutoCommit then
    begin
      if GetInstance.InTransaction then
        GetInstance.Rollback
    end
    else
    begin
      LSQL := Query.SQL.Text;
      Query.SQL.Text := 'ROLLBACK';
      Query.ExecSQL;
      Query.SQL.Text := LSQL;
    end;

  except
    on e: exception do
    begin
      if LSQL <> '' then Query.SQL.Text := LSQL;
      KrakenLOG.Error(E.Message);
      raise;
    end;
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

function TKrakenProviderFiredac.Migrate: TKrakenMigrate;
begin
  if not Assigned(FKrakenMigrate) then
    FKrakenMigrate := TKrakenMigrate.Create(Self);
  Result := FKrakenMigrate;
end;

end.
