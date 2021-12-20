unit Kraken.Provider.Zeos.Settings;

interface

uses
  System.IniFiles,
  System.SysUtils,
  ZConnection;

type
  TKrakenProviderZeosSettings<T: class, constructor> = class
    constructor Create(AOwner: T);
    destructor Destroy; override;
  private
    FReturn       : T;
    FConnection   : TZConnection;

    FIniFile      : TIniFile;
    FHost         : String;
    FPort         : Integer;
    FUsername     : String;
    FPassword     : String;
    FDatabase     : String;
    FURLRemoto    : string;
    FTimeout      : String;
    FSSLMode      : string;
    FSSLCert      : string;
    FSSLKey       : string;
    FSSLRootcert  : string;

    function HasAssignedIniFile: Boolean;
  public
    function Host(const AHost: String): TKrakenProviderZeosSettings<T>; overload;
    function Host(const ASection, AIdent, ADefault: String): TKrakenProviderZeosSettings<T>; overload;
    function Host: String; overload;

    function Port(const APort: Integer): TKrakenProviderZeosSettings<T>; overload;
    function Port(const APort: String): TKrakenProviderZeosSettings<T>; overload;
    function Port(const ASection, AIdent, ADefault: String): TKrakenProviderZeosSettings<T>; overload;
    function Port: Integer; overload;

    function Username(const AUsername: String): TKrakenProviderZeosSettings<T>; overload;
    function Username(const ASection, AIdent, ADefault: String): TKrakenProviderZeosSettings<T>; overload;
    function Username: String; overload;

    function Password(const APassword: String): TKrakenProviderZeosSettings<T>; overload;
    function Password(const ASection, AIdent, ADefault: String): TKrakenProviderZeosSettings<T>; overload;
    function Password: String; overload;

    function Database(const ADatabase: String): TKrakenProviderZeosSettings<T>; overload;
    function Database(const ASection, AIdent, ADefault: String): TKrakenProviderZeosSettings<T>; overload;
    function Database: String; overload;

    function URLRemoto(const AURLRemoto: String): TKrakenProviderZeosSettings<T>; overload;
    function URLRemoto(const ASection, AIdent, ADefault: String): TKrakenProviderZeosSettings<T>; overload;
    function URLRemoto: String; overload;

    function TimeOut(const ATimeout: String): TKrakenProviderZeosSettings<T>; overload;
    function TimeOut(const ASection, AIdent, ADefault: String): TKrakenProviderZeosSettings<T>; overload;
    function Timeout: Integer; overload;

    function SSLMode(const ASSLMode: String): TKrakenProviderZeosSettings<T>; overload;
    function SSLMode(const ASection, AIdent, ADefault: String): TKrakenProviderZeosSettings<T>; overload;
    function SSLMode: string; overload;

    function SSLCert(const ASSLCert: String): TKrakenProviderZeosSettings<T>; overload;
    function SSLCert(const ASection, AIdent, ADefault: String): TKrakenProviderZeosSettings<T>; overload;
    function SSLCert: string; overload;

    function SSLKey(const ASSLKey: string): TKrakenProviderZeosSettings<T>; overload;
    function SSLKey(const ASection, AIdent, ADefault: String): TKrakenProviderZeosSettings<T>; overload;
    function SSLKey: string; overload;

    function SSLRootcert(const ASSLRootcert: string): TKrakenProviderZeosSettings<T>; overload;
    function SSLRootcert(const ASection, AIdent, ADefault: String): TKrakenProviderZeosSettings<T>; overload;
    function SSLRootcert: string; overload;

    function IniPath(const APath: String): TKrakenProviderZeosSettings<T>;

    ///<summary>            Define o autocommit de transacoes (insert, update, delete)            </summary>
    ///<param name="AMode"> Se falso, deve usar o StartTransaction e Commit                       </param>
    ///<remarks>            Default: False                                                        </remarks>
    function AutoCommit(const AMode: Boolean): TKrakenProviderZeosSettings<T>;
  end;

implementation

{ TKrakenProviderZeosSettings<T> }

function TKrakenProviderZeosSettings<T>.AutoCommit(const AMode: Boolean): TKrakenProviderZeosSettings<T>;
begin
  FConnection.AutoCommit := AMode;
end;

constructor TKrakenProviderZeosSettings<T>.Create(AOwner: T);
begin
  FReturn     := AOwner;
  FConnection := {$IF DEFINED(KRAKEN_FIREDAC)} TFDConnection(AOwner); {$ELSE} TZConnection(AOwner); {$ENDIF}
end;

destructor TKrakenProviderZeosSettings<T>.Destroy;
begin

  inherited;
end;

function TKrakenProviderZeosSettings<T>.HasAssignedIniFile: Boolean;
begin
  Result := FIniFile <> nil;

  if not Result then
    raise Exception.Create('Has not inifile assigned. Try pass parameter on create method.');
end;

function TKrakenProviderZeosSettings<T>.Host(const AHost: String): TKrakenProviderZeosSettings<T>;
begin
  Result := Self;
  FHost  := AHost;

  FConnection.HostName := FHost;
end;

function TKrakenProviderZeosSettings<T>.Host(const ASection, AIdent, ADefault: String): TKrakenProviderZeosSettings<T>;
begin
  Result := Self;
  HasAssignedIniFile;

  FHost := FIniFile.ReadString(ASection, AIdent, ADefault);
  FConnection.HostName := FHost;
end;

function TKrakenProviderZeosSettings<T>.Host: String;
begin
  Result := FHost;
end;

function TKrakenProviderZeosSettings<T>.IniPath(const APath: String): TKrakenProviderZeosSettings<T>;
begin
  Result := Self;

  if APath <> '' then
    FIniFile := TIniFile.Create(APath);
end;

function TKrakenProviderZeosSettings<T>.Port(const APort: Integer): TKrakenProviderZeosSettings<T>;
begin
  Result := Self;
  FPort  := APort;
  FConnection.Port := Port;
end;

function TKrakenProviderZeosSettings<T>.Port(const APort: String): TKrakenProviderZeosSettings<T>;
begin
  Result := Self;
  FPort  := StrToIntDef(APort, 5432);
  FConnection.Port := Port;
end;

function TKrakenProviderZeosSettings<T>.Port(const ASection, AIdent, ADefault: String): TKrakenProviderZeosSettings<T>;
begin
  Result := Self;
  HasAssignedIniFile;

  FPort := StrToInt(FIniFile.ReadString(ASection, AIdent, ADefault));
  FConnection.Port := Port;
end;

function TKrakenProviderZeosSettings<T>.Port: Integer;
begin
  Result := FPort;
end;

function TKrakenProviderZeosSettings<T>.Username(const AUsername: String): TKrakenProviderZeosSettings<T>;
begin
  Result := Self;
  FUsername := AUsername;

  FConnection.User := FUsername;
end;

function TKrakenProviderZeosSettings<T>.Username(const ASection, AIdent, ADefault: String): TKrakenProviderZeosSettings<T>;
begin
  Result := Self;
  HasAssignedIniFile;

  FUsername := FIniFile.ReadString(ASection, AIdent, ADefault);
  FConnection.User := FUsername;
end;

function TKrakenProviderZeosSettings<T>.URLRemoto(const AURLRemoto: String): TKrakenProviderZeosSettings<T>;
begin
  Result := Self;
  FURLRemoto  := AURLRemoto;
end;

function TKrakenProviderZeosSettings<T>.URLRemoto(const ASection, AIdent,ADefault: String): TKrakenProviderZeosSettings<T>;
begin
  Result := Self;
  HasAssignedIniFile;

  FURLRemoto := FIniFile.ReadString(ASection, AIdent, ADefault);
end;

function TKrakenProviderZeosSettings<T>.URLRemoto: String;
begin
  Result := FURLRemoto;
end;

function TKrakenProviderZeosSettings<T>.TimeOut(const ATimeout: String): TKrakenProviderZeosSettings<T>;
begin
  Result   := Self;
  FTimeout := ATimeOut;
end;

function TKrakenProviderZeosSettings<T>.TimeOut(const ASection, AIdent, ADefault: String): TKrakenProviderZeosSettings<T>;
begin
  Result := Self;
  HasAssignedIniFile;

  FTimeout := FIniFile.ReadString(ASection, AIdent, ADefault);
end;

function TKrakenProviderZeosSettings<T>.Timeout: Integer;
begin
  Result := StrToIntDef(FTimeout, 0);
end;

function TKrakenProviderZeosSettings<T>.Username: String;
begin
  Result := FUsername;
end;

function TKrakenProviderZeosSettings<T>.Password(const APassword: String): TKrakenProviderZeosSettings<T>;
begin
  Result := Self;
  FPassword := APassword;

  FConnection.Password := FPassword;
end;

function TKrakenProviderZeosSettings<T>.Password(const ASection, AIdent, ADefault: String): TKrakenProviderZeosSettings<T>;
begin
  Result := Self;
  HasAssignedIniFile;

  FPassword := FIniFile.ReadString(ASection, AIdent, ADefault);
  FConnection.Password := FPassword;
end;

function TKrakenProviderZeosSettings<T>.Password: String;
begin
  Result := FPassword;
end;

function TKrakenProviderZeosSettings<T>.Database(const ADatabase: String): TKrakenProviderZeosSettings<T>;
begin
  Result := Self;
  FDatabase := ADatabase;

  FConnection.Database := FDatabase;
end;

function TKrakenProviderZeosSettings<T>.Database(const ASection, AIdent, ADefault: String): TKrakenProviderZeosSettings<T>;
begin
  Result := Self;
  HasAssignedIniFile;

  FDatabase := FIniFile.ReadString(ASection, AIdent, ADefault);
  FConnection.Database := FDatabase;
end;

function TKrakenProviderZeosSettings<T>.Database: String;
begin
  Result := FDatabase;
end;

function TKrakenProviderZeosSettings<T>.SSLMode(const ASSLMode: String): TKrakenProviderZeosSettings<T>;
begin
  Result := Self;
  FSSLMode := ASSLMode;
  FConnection.Properties.Values['sslmode'] := FSSLMode;
end;

function TKrakenProviderZeosSettings<T>.SSLMode(const ASection, AIdent, ADefault: String): TKrakenProviderZeosSettings<T>;
begin
  Result := Self;
  HasAssignedIniFile;

  FSSLMode := FIniFile.ReadString(ASection, AIdent, ADefault);
  FConnection.Properties.Values['sslmode'] := FSSLMode;
end;

function TKrakenProviderZeosSettings<T>.SSLMode: string;
begin
  Result := FSSLMode;
end;

function TKrakenProviderZeosSettings<T>.SSLCert(const ASSLCert: String): TKrakenProviderZeosSettings<T>;
begin
  Result := Self;
  FSSLCert := ASSLCert;
  FConnection.Properties.Values['sslcert'] := FSSLCert;
end;

function TKrakenProviderZeosSettings<T>.SSLCert(const ASection, AIdent, ADefault: String): TKrakenProviderZeosSettings<T>;
begin
  Result := Self;
  HasAssignedIniFile;

  FSSLCert := FIniFile.ReadString(ASection, AIdent, ADefault);
  FConnection.Properties.Values['sslcert'] := FSSLCert;
end;

function TKrakenProviderZeosSettings<T>.SSLCert: string;
begin
  Result := FSSLCert;
end;

function TKrakenProviderZeosSettings<T>.SSLKey(const ASSLKey: string): TKrakenProviderZeosSettings<T>;
begin
  Result := Self;
  FSSLKey := ASSLKey;
  FConnection.Properties.Values['sslkey'] := FSSLKey;
end;

function TKrakenProviderZeosSettings<T>.SSLKey(const ASection, AIdent, ADefault: String): TKrakenProviderZeosSettings<T>;
begin
  Result := Self;
  HasAssignedIniFile;

  FSSLKey := FIniFile.ReadString(ASection, AIdent, ADefault);
  FConnection.Properties.Values['sslkey'] := FSSLKey;
end;

function TKrakenProviderZeosSettings<T>.SSLKey: string;
begin
  Result := FSSLKey;
end;

function TKrakenProviderZeosSettings<T>.SSLRootcert(const ASSLRootcert: string): TKrakenProviderZeosSettings<T>;
begin
  Result := Self;
  FSSLRootcert := ASSLRootcert;
  FConnection.Properties.Values['sslrootcert'] := FSSLRootcert;
end;

function TKrakenProviderZeosSettings<T>.SSLRootcert(const ASection, AIdent, ADefault: String): TKrakenProviderZeosSettings<T>;
begin
  Result := Self;
  HasAssignedIniFile;

  FSSLRootcert := FIniFile.ReadString(ASection, AIdent, ADefault);
  FConnection.Properties.Values['sslrootcert'] := FSSLRootcert;
end;

function TKrakenProviderZeosSettings<T>.SSLRootcert: string;
begin
  Result := FSSLRootcert;
end;

end.
