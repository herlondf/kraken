unit Kraken.Provider.Zeos;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  ShellApi,
  Winapi.Windows,

  ZConnection,
  ZAbstractConnection,
  ZDbcIntfs,

  IdTCPClient,

  Kraken.Log,
  Kraken.Consts,
  Kraken.Provider.Settings,
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
    FKrakenProviderSettings: TKrakenProviderSettings<TKrakenProviderZeos>;
    FKrakenProviderTypes   : TKrakenProviderTypes<TKrakenProviderZeos>;

    LIdTCPClient: TIdTCPClient;

    procedure IdTCPClientDisconnected(Sender: TObject);

    procedure _SetDefaultConfig;
    function GetDeviceName : String;

    function ConnectionInternalTest: Boolean;
  public
    function GetInstance: TZConnection;

    function ProviderType: TKrakenProviderTypes<TKrakenProviderZeos>;
    function Settings: TKrakenProviderSettings<TKrakenProviderZeos>;

    function Id(const Value: String): TKrakenProviderZeos; overload;
    function Id: String; overload;

    function  Connect: Boolean;
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

  LIdTCPClient := TIdTCPClient.Create(Self);

  _SetDefaultConfig;

  FKrakenQuerys := TKrakenQuerys.Create();
end;

destructor TKrakenProviderZeos.Destroy;
begin
  if FKrakenProviderSettings <> nil then
    FreeAndNil(FKrakenProviderSettings);

  if FKrakenProviderTypes <> nil then
    FreeAndNil(FKrakenProviderTypes);

  if FKrakenQuerys <> nil then
    FreeAndNil(FKrakenQuerys);

  if Assigned(LIdTCPClient) then
    FreeAndNil(LIdTCPClient);

  inherited;
end;

//Falta testar
procedure TKrakenProviderZeos.IdTCPClientDisconnected(Sender: TObject);
begin
  try
    try
      GetInstance.Disconnect;

      while not GetInstance.Connected do
      begin
        GetInstance.Connected := True;
        Sleep(10000);
      end;
    finally

    end;
  except

  end;
end;

procedure TKrakenProviderZeos._SetDefaultConfig;
begin
  GetInstance.AutoCommit := False;
  GetInstance.Properties.Values['codepage'] := 'utf-8';
  GetInstance.Properties.Values['application_name'] := Copy( ExtractFileName( ParamStr(0) ),  1, Pos('.', ExtractFileName( ParamStr( 0 ) ) ) -1 ); //+ '-' + id + '-' + GetDeviceName;
  GetInstance.TransactIsolationLevel := tiReadUncommitted;
end;

function TKrakenProviderZeos.GetInstance: TZConnection;
begin
  Result := TZConnection(Self);
end;

function TKrakenProviderZeos.ProviderType: TKrakenProviderTypes<TKrakenProviderZeos>;
begin
  if not Assigned(FKrakenProviderTypes) then
    FKrakenProviderTypes := TKrakenProviderTypes<TKrakenProviderZeos>.Create(Self);
  Result := FKrakenProviderTypes;
end;

function TKrakenProviderZeos.Settings: TKrakenProviderSettings<TKrakenProviderZeos>;
begin
  if FKrakenProviderSettings = nil then
    FKrakenProviderSettings := TKrakenProviderSettings<TKrakenProviderZeos>.Create(Self);
  Result := FKrakenProviderSettings;
end;

function TKrakenProviderZeos.GetDeviceName : String; var ipbuffer : string; nsize : dword;
begin
   nsize := 255;
   SetLength(ipbuffer,nsize);
   if GetComputerName(pchar(ipbuffer),nsize) then
      result := ipbuffer;
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

function TKrakenProviderZeos.Connect: Boolean;
begin
  Result := False;
  try
    try
      if ( not Connected ) and ( ConnectionInternalTest ) then
        GetInstance.Connect;
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

function TKrakenProviderZeos.ConnectionInternalTest: Boolean;
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

procedure TKrakenProviderZeos.Disconnect;
begin
  try
    GetInstance.Disconnect;
  except
    on e: exception do
    begin
      KrakenLOG.Error(E.Message);
      raise;
    end;
  end;
end;

procedure TKrakenProviderZeos.StartTransaction;
var
  LSQL: string;
begin
  try
    if GetInstance.AutoCommit then
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

procedure TKrakenProviderZeos.Commit;
var
  LSQL: String;
begin
  try
    if GetInstance.AutoCommit then
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

procedure TKrakenProviderZeos.Rollback;
var
  LSQL: String;
begin
  try
    if GetInstance.AutoCommit then
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
    end;
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
