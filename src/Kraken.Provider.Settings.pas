unit Kraken.Provider.Settings;

interface

uses
  System.IniFiles,
  System.SysUtils,
  {$IF DEFINED(KRAKEN_FIREDAC)}
  FireDAC.Stan.Intf,
  FireDAC.Comp.Client;
  {$ELSE}
  ZConnection;
  {$ENDIF}


type
  TKrakenProviderSettings<T: class, constructor> = class
    constructor Create(AOwner: T);
    destructor Destroy; override;
  private
    FReturn       : T;
    FConnection   : {$IF DEFINED(KRAKEN_FIREDAC)} TFDConnection; {$ELSE} TZConnection; {$ENDIF}

    FIniFile      : TIniFile;
    FHost         : String;
    FPort         : Integer;
    FUsername     : String;
    FPassword     : String;
    FDatabase     : String;
    FURLRemoto    : string;
    FTimeout      : String;
    FAutoCommit   : Boolean;
    FSSLMode      : string;
    FSSLCert      : string;
    FSSLKey       : string;
    FSSLRootcert  : string;

    function HasAssignedIniFile: Boolean;
  public
    function IniPath(const APath: String): TKrakenProviderSettings<T>;

    function Host(const AHost: String): TKrakenProviderSettings<T>; overload;
    function Host(const ASection, AIdent, ADefault: String): TKrakenProviderSettings<T>; overload;
    function Host: String; overload;

    function Port(const APort: Integer): TKrakenProviderSettings<T>; overload;
    function Port(const APort: String): TKrakenProviderSettings<T>; overload;
    function Port(const ASection, AIdent, ADefault: String): TKrakenProviderSettings<T>; overload;
    function Port: Integer; overload;

    function Username(const AUsername: String): TKrakenProviderSettings<T>; overload;
    function Username(const ASection, AIdent, ADefault: String): TKrakenProviderSettings<T>; overload;
    function Username: String; overload;

    function Password(const APassword: String): TKrakenProviderSettings<T>; overload;
    function Password(const ASection, AIdent, ADefault: String): TKrakenProviderSettings<T>; overload;
    function Password: String; overload;

    function Database(const ADatabase: String): TKrakenProviderSettings<T>; overload;
    function Database(const ASection, AIdent, ADefault: String): TKrakenProviderSettings<T>; overload;
    function Database: String; overload;

    function URLRemoto(const AURLRemoto: String): TKrakenProviderSettings<T>; overload;
    function URLRemoto(const ASection, AIdent, ADefault: String): TKrakenProviderSettings<T>; overload;
    function URLRemoto: String; overload;

    function TimeOut(const ATimeout: String): TKrakenProviderSettings<T>; overload;
    function TimeOut(const ASection, AIdent, ADefault: String): TKrakenProviderSettings<T>; overload;
    function Timeout: Integer; overload;

    function SSLMode(const ASSLMode: String): TKrakenProviderSettings<T>; overload;
    function SSLMode(const ASection, AIdent, ADefault: String): TKrakenProviderSettings<T>; overload;
    function SSLMode: string; overload;

    function SSLCert(const ASSLCert: String): TKrakenProviderSettings<T>; overload;
    function SSLCert(const ASection, AIdent, ADefault: String): TKrakenProviderSettings<T>; overload;
    function SSLCert: string; overload;

    function SSLKey(const ASSLKey: string): TKrakenProviderSettings<T>; overload;
    function SSLKey(const ASection, AIdent, ADefault: String): TKrakenProviderSettings<T>; overload;
    function SSLKey: string; overload;

    function SSLRootcert(const ASSLRootcert: string): TKrakenProviderSettings<T>; overload;
    function SSLRootcert(const ASection, AIdent, ADefault: String): TKrakenProviderSettings<T>; overload;
    function SSLRootcert: string; overload;

    ///<summary>            Define o autocommit de transacoes (insert, update, delete)            </summary>
    ///<param name="AMode"> Se falso, deve usar o StartTransaction e Commit                       </param>
    ///<remarks>            Default: False                                                        </remarks>
    function AutoCommit(const AMode: Boolean): TKrakenProviderSettings<T>;

    ///<summary>            Define o modo de bloqueio da execucao da query                        </summary>
    ///<param name="AMode"> amBlocking, amNonBlocking, amCancelDialog, amAsync                    </param>
    ///<remarks>            Default: amBlocking                                                   </remarks>
    function AsyncMode(const AMode: {$IF DEFINED(KRAKEN_FIREDAC)}TFDStanAsyncMode{$ELSE}Boolean{$ENDIF}): TKrakenProviderSettings<T>;

    ///<summary>            Define a capacidade de auto-reconectar ao banco apos perda de conexao </summary>
    ///<param name="AMode"> Se falso, deve implementar rotina de auto-reconectar                  </param>
    ///<remarks>            Default: True                                                         </remarks>
    function AutoRecoverConnection(const AMode: Boolean): TKrakenProviderSettings<T>;

    function &End: T;
  end;

implementation

{ TKrakenProviderSettings<T> }

constructor TKrakenProviderSettings<T>.Create(AOwner: T);
begin
  FReturn     := AOwner;
  FConnection := {$IF DEFINED(KRAKEN_FIREDAC)} TFDConnection(AOwner); {$ELSE} TZConnection(AOwner); {$ENDIF}
end;

destructor TKrakenProviderSettings<T>.Destroy;
begin

  inherited;
end;

function TKrakenProviderSettings<T>.IniPath(const APath: String): TKrakenProviderSettings<T>;
begin
  Result := Self;

  if APath <> '' then
    FIniFile := TIniFile.Create(APath);
end;

function TKrakenProviderSettings<T>.HasAssignedIniFile: Boolean;
begin
  Result := FIniFile <> nil;

  if not Result then
    raise Exception.Create('Has not inifile assigned. Try pass parameter on create method.');
end;

function TKrakenProviderSettings<T>.Host(const AHost: String): TKrakenProviderSettings<T>;
begin
  Result := Self;
  FHost  := AHost;

  {$IF DEFINED(KRAKEN_FIREDAC)}
    FConnection.Params.Add('Server='+FHost);
  {$ELSE}
    FConnection.HostName := FHost;
  {$ENDIF}
end;

function TKrakenProviderSettings<T>.Host(const ASection, AIdent, ADefault: String): TKrakenProviderSettings<T>;
begin
  Result := Self;
  HasAssignedIniFile;

  FHost := FIniFile.ReadString(ASection, AIdent, ADefault);

  {$IF DEFINED(KRAKEN_FIREDAC)}
    FConnection.Params.Add('Server='+FHost);
  {$ELSE}
    FConnection.HostName := FHost;
  {$ENDIF}
end;

function TKrakenProviderSettings<T>.Host: String;
begin
  Result := FHost;
end;

function TKrakenProviderSettings<T>.Port(const APort: Integer): TKrakenProviderSettings<T>;
begin
  Result := Self;
  FPort  := APort;

  {$IF DEFINED(KRAKEN_FIREDAC)}
    FConnection.Params.Add('Port='+IntToStr(FPort));
  {$ELSE}
    FConnection.Port := Port;
  {$ENDIF}
end;

function TKrakenProviderSettings<T>.Port(const APort: String): TKrakenProviderSettings<T>;
begin
  Result := Self;
  FPort  := StrToIntDef(APort, 5432);

  {$IF DEFINED(KRAKEN_FIREDAC)}
    FConnection.Params.Add('Port='+IntToStr(FPort));
  {$ELSE}
    FConnection.Port := Port;
  {$ENDIF}
end;

function TKrakenProviderSettings<T>.Port(const ASection, AIdent, ADefault: String): TKrakenProviderSettings<T>;
begin
  Result := Self;
  HasAssignedIniFile;

  FPort := StrToInt(FIniFile.ReadString(ASection, AIdent, ADefault));

  {$IF DEFINED(KRAKEN_FIREDAC)}
    FConnection.Params.Add('Port='+IntToStr(FPort));
  {$ELSE}
    FConnection.Port := Port;
  {$ENDIF}
end;

function TKrakenProviderSettings<T>.Port: Integer;
begin
  Result := FPort;
end;

function TKrakenProviderSettings<T>.Username(const AUsername: String): TKrakenProviderSettings<T>;
begin
  Result := Self;
  FUsername := AUsername;

  {$IF DEFINED(KRAKEN_FIREDAC)}
    FConnection.Params.UserName := FUsername;
  {$ELSE}
    FConnection.User := FUsername;
  {$ENDIF}
end;

function TKrakenProviderSettings<T>.Username(const ASection, AIdent, ADefault: String): TKrakenProviderSettings<T>;
begin
  Result := Self;
  HasAssignedIniFile;

  FUsername := FIniFile.ReadString(ASection, AIdent, ADefault);

  {$IF DEFINED(KRAKEN_FIREDAC)}
    FConnection.Params.UserName := FUsername;
  {$ELSE}
    FConnection.User := FUsername;
  {$ENDIF}
end;

function TKrakenProviderSettings<T>.Username: String;
begin
  Result := FUsername;
end;

function TKrakenProviderSettings<T>.Password(const APassword: String): TKrakenProviderSettings<T>;
begin
  Result := Self;
  FPassword := APassword;

  {$IF DEFINED(KRAKEN_FIREDAC)}
    FConnection.Params.Password := FPassword;
  {$ELSE}
    FConnection.Password := FPassword;
  {$ENDIF}
end;

function TKrakenProviderSettings<T>.Password(const ASection, AIdent, ADefault: String): TKrakenProviderSettings<T>;
begin
  Result := Self;
  HasAssignedIniFile;

  FPassword := FIniFile.ReadString(ASection, AIdent, ADefault);

  {$IF DEFINED(KRAKEN_FIREDAC)}
    FConnection.Params.Password := FPassword;
  {$ELSE}
    FConnection.Password := FPassword;
  {$ENDIF}
end;

function TKrakenProviderSettings<T>.Password: String;
begin
  Result := FPassword;
end;

function TKrakenProviderSettings<T>.Database(const ADatabase: String): TKrakenProviderSettings<T>;
begin
  Result := Self;
  FDatabase := ADatabase;

  {$IF DEFINED(KRAKEN_FIREDAC)}
    FConnection.Params.Database := FDatabase;
  {$ELSE}
    FConnection.Database := FDatabase;
  {$ENDIF}
end;

function TKrakenProviderSettings<T>.Database(const ASection, AIdent, ADefault: String): TKrakenProviderSettings<T>;
begin
  Result := Self;
  HasAssignedIniFile;

  FDatabase := FIniFile.ReadString(ASection, AIdent, ADefault);

  {$IF DEFINED(KRAKEN_FIREDAC)}
    FConnection.Params.Database := FDatabase;
  {$ELSE}
    FConnection.Database := FDatabase;
  {$ENDIF}
end;

function TKrakenProviderSettings<T>.Database: String;
begin
  Result := FDatabase;
end;

function TKrakenProviderSettings<T>.URLRemoto(const AURLRemoto: String): TKrakenProviderSettings<T>;
begin
  Result := Self;
  FURLRemoto  := AURLRemoto;
end;

function TKrakenProviderSettings<T>.URLRemoto(const ASection, AIdent,ADefault: String): TKrakenProviderSettings<T>;
begin
  Result := Self;
  HasAssignedIniFile;

  FURLRemoto := FIniFile.ReadString(ASection, AIdent, ADefault);
end;

function TKrakenProviderSettings<T>.URLRemoto: String;
begin
  Result := FURLRemoto;
end;

function TKrakenProviderSettings<T>.TimeOut(const ATimeout: String): TKrakenProviderSettings<T>;
begin
  Result   := Self;
  FTimeout := ATimeOut;

  {$IF DEFINED(KRAKEN_FIREDAC)}
    FConnection.Params.Add('LoginTimeout='+FTimeout);
  {$ELSE}

  {$ENDIF}
end;

function TKrakenProviderSettings<T>.TimeOut(const ASection, AIdent, ADefault: String): TKrakenProviderSettings<T>;
begin
  Result := Self;
  HasAssignedIniFile;

  FTimeout := FIniFile.ReadString(ASection, AIdent, ADefault);

  {$IF DEFINED(KRAKEN_FIREDAC)}
    FConnection.Params.Add('LoginTimeout='+FTimeout);
  {$ELSE}

  {$ENDIF}
end;

function TKrakenProviderSettings<T>.Timeout: Integer;
begin
  Result := StrToIntDef(FTimeout, 0);
end;

function TKrakenProviderSettings<T>.SSLMode(const ASSLMode: String): TKrakenProviderSettings<T>;
begin
  Result := Self;
  FSSLMode := ASSLMode;

  {$IF DEFINED(KRAKEN_FIREDAC)}

  {$ELSE}
    if FConnection.Protocol = 'postgresql-9' then
      FConnection.Properties.Values['sslmode'] := FSSLMode;
  {$ENDIF}
end;

function TKrakenProviderSettings<T>.SSLMode(const ASection, AIdent, ADefault: String): TKrakenProviderSettings<T>;
begin
  Result := Self;
  HasAssignedIniFile;

  FSSLMode := FIniFile.ReadString(ASection, AIdent, ADefault);

  {$IF DEFINED(KRAKEN_FIREDAC)}

  {$ELSE}
    if FConnection.Protocol = 'postgresql-9' then
      FConnection.Properties.Values['sslmode'] := FSSLMode;
  {$ENDIF}
end;

function TKrakenProviderSettings<T>.SSLMode: string;
begin
  Result := FSSLMode;
end;

function TKrakenProviderSettings<T>.SSLCert(const ASSLCert: String): TKrakenProviderSettings<T>;
begin
  Result := Self;
  FSSLCert := ASSLCert;

  {$IF DEFINED(KRAKEN_FIREDAC)}

  {$ELSE}
    if FConnection.Protocol = 'postgresql-9' then
      FConnection.Properties.Values['sslcert'] := FSSLCert;
  {$ENDIF}
end;

function TKrakenProviderSettings<T>.SSLCert(const ASection, AIdent, ADefault: String): TKrakenProviderSettings<T>;
begin
  Result := Self;
  HasAssignedIniFile;

  FSSLCert := FIniFile.ReadString(ASection, AIdent, ADefault);

  {$IF DEFINED(KRAKEN_FIREDAC)}

  {$ELSE}
    if FConnection.Protocol = 'postgresql-9' then
      FConnection.Properties.Values['sslcert'] := FSSLCert;
  {$ENDIF}
end;

function TKrakenProviderSettings<T>.SSLCert: string;
begin
  Result := FSSLCert;
end;

function TKrakenProviderSettings<T>.SSLKey(const ASSLKey: string): TKrakenProviderSettings<T>;
begin
  Result := Self;
  FSSLKey := ASSLKey;

  {$IF DEFINED(KRAKEN_FIREDAC)}

  {$ELSE}
    if FConnection.Protocol = 'postgresql-9' then
      FConnection.Properties.Values['sslkey'] := FSSLKey;
  {$ENDIF}
end;

function TKrakenProviderSettings<T>.SSLKey(const ASection, AIdent, ADefault: String): TKrakenProviderSettings<T>;
begin
  Result := Self;
  HasAssignedIniFile;

  FSSLKey := FIniFile.ReadString(ASection, AIdent, ADefault);

  {$IF DEFINED(KRAKEN_FIREDAC)}

  {$ELSE}
    if FConnection.Protocol = 'postgresql-9' then
      FConnection.Properties.Values['sslkey'] := FSSLKey;
  {$ENDIF}
end;

function TKrakenProviderSettings<T>.SSLKey: string;
begin
  Result := FSSLKey;
end;

function TKrakenProviderSettings<T>.SSLRootcert(const ASSLRootcert: string): TKrakenProviderSettings<T>;
begin
  Result := Self;
  FSSLRootcert := ASSLRootcert;

  {$IF DEFINED(KRAKEN_FIREDAC)}

  {$ELSE}
    if FConnection.Protocol = 'postgresql-9' then
      FConnection.Properties.Values['sslrootcert'] := FSSLRootcert;
  {$ENDIF}
end;

function TKrakenProviderSettings<T>.SSLRootcert(const ASection, AIdent, ADefault: String): TKrakenProviderSettings<T>;
begin
  Result := Self;
  HasAssignedIniFile;

  FSSLRootcert := FIniFile.ReadString(ASection, AIdent, ADefault);

  {$IF DEFINED(KRAKEN_FIREDAC)}

  {$ELSE}
    if FConnection.Protocol = 'postgresql-9' then
      FConnection.Properties.Values['sslrootcert'] := FSSLRootcert;
  {$ENDIF}
end;

function TKrakenProviderSettings<T>.SSLRootcert: string;
begin
  Result := FSSLRootcert;
end;

function TKrakenProviderSettings<T>.AutoCommit(const AMode: Boolean): TKrakenProviderSettings<T>;
begin
  Result := Self;
  FAutoCommit := AMode;

  {$IF DEFINED(KRAKEN_FIREDAC)}
    FConnection.TxOptions.AutoCommit := FAutoCommit;
  {$ELSE}
    FConnection.AutoCommit := FAutoCommit;
  {$ENDIF}
end;

function TKrakenProviderSettings<T>.AsyncMode(const AMode: {$IF DEFINED(KRAKEN_FIREDAC)}TFDStanAsyncMode{$ELSE}Boolean{$ENDIF}): TKrakenProviderSettings<T>;
begin
  Result := Self;

  {$IF DEFINED(KRAKEN_FIREDAC)}
    FConnection.ResourceOptions.CmdExecMode := AMode
  {$ELSE}

  {$ENDIF}
end;

function TKrakenProviderSettings<T>.AutoRecoverConnection(const AMode: Boolean): TKrakenProviderSettings<T>;
begin
  Result := Self;

  {$IF DEFINED(KRAKEN_FIREDAC)}
    FConnection.ResourceOptions.AutoReconnect  := AMode;
    FConnection.ResourceOptions.KeepConnection := AMode;
  {$ELSE}

  {$ENDIF}
end;

function TKrakenProviderSettings<T>.&End: T;
begin
  Result := FReturn;
end;


end.
