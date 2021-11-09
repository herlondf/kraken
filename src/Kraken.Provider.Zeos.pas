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

    LIdTCPClient: TIdTCPClient;

    procedure IdTCPClientDisconnected(Sender: TObject);

    procedure _SetDefaultConfig;
    function GetDeviceName : String;

    ///<summary>Connectivity test with TIdTCPClient</summary>
    function ConnectionInternalTest: Boolean;
  public
    function GetInstance: TZConnection;
    function ProviderType(AProviderType: TKrakenProviderType): TKrakenProviderZeos;
    function Settings: TKrakenProviderZeosSettings;

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
  GetInstance.TransactIsolationLevel := tiReadCommitted;
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

  Properties.Add('application_name=' + Copy( ExtractFileName( ParamStr(0) ),  1, Pos('.', ExtractFileName(ParamStr(0)))-1) + '-' + id + '-' + GetDeviceName );

  Result := FKrakenProviderSettings;
end;

function TKrakenProviderZeos.GetDeviceName : String;
var ipbuffer : string;
      nsize : dword;
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
  try
    try
      if ( not Connected ) and ( ConnectionInternalTest ) then
        GetInstance.Connected := True;
    finally
      Result := True;
    end;
  except
    Result := False;
  end;
end;

function TKrakenProviderZeos.ConnectionInternalTest: Boolean;
begin
  Result := False;
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

  end;
end;

procedure TKrakenProviderZeos.StartTransaction;
var
  LSQL: string;
begin
  if GetInstance.InTransaction then Exit;

  try
    if GetInstance.AutoCommit then
      GetInstance.StartTransaction
    else
    begin
      LSQL := Query.SQL.Text;
      try
        Query.SQL.Clear;
        Query.SQL.Add('BEGIN');
        Query.ExecSQL;
      finally
        Query.SQL.Text := LSQL;
      end;
    end;
  except
    on e: exception do
    begin
      if LSQL <> '' then Query.SQL.Text := LSQL;
      KrakenLOG.Error(E.Message);
    end;
  end;
end;

procedure TKrakenProviderZeos.Commit;
var
  LSQL: string;
begin
  if GetInstance.InTransaction then Exit;

  try
    if GetInstance.AutoCommit then
      GetInstance.StartTransaction
    else
    begin
      LSQL := Query.SQL.Text;
      try
        Query.SQL.Clear;
        Query.SQL.Add('COMMIT');
        Query.ExecSQL;
      finally
        Query.SQL.Text := LSQL;
      end;
    end;
  except
    on e: exception do
    begin
      if LSQL <> '' then Query.SQL.Text := LSQL;
      KrakenLOG.Error(E.Message);
    end;
  end;
end;

procedure TKrakenProviderZeos.Rollback;
var
  LSQL: string;
begin
  if GetInstance.InTransaction then Exit;

  try
    if GetInstance.AutoCommit then
      GetInstance.StartTransaction
    else
    begin
      LSQL := Query.SQL.Text;
      try
        Query.SQL.Clear;
        Query.SQL.Add('ROLLBACK');
        Query.ExecSQL;
      finally
        Query.SQL.Text := LSQL;
      end;
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
