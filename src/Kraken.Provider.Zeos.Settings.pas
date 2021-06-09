unit Kraken.Provider.Zeos.Settings;

interface

uses
  System.IniFiles,
  System.SysUtils,
  ZConnection;

type
  TKrakenProviderZeosSettings = class
    constructor Create(AConnetion: TZConnection);
    destructor Destroy; override;
  private
    FIniFile    : TIniFile;
    FConnection : TZConnection;
    FHost       : String;
    FPort       : Integer;
    FUsername   : String;
    FPassword   : String;
    FDatabase   : String;

    function HasAssignedIniFile: Boolean;
  public
    function Host(const AHost: String): TKrakenProviderZeosSettings; overload;
    function Host(const ASection, AIdent, ADefault: String): TKrakenProviderZeosSettings; overload;
    function Host: String; overload;

    function Port(const APort: Integer): TKrakenProviderZeosSettings; overload;
    function Port(const APort: String): TKrakenProviderZeosSettings; overload;
    function Port(const ASection, AIdent, ADefault: String): TKrakenProviderZeosSettings; overload;
    function Port: Integer; overload;

    function Username(const AUsername: String): TKrakenProviderZeosSettings; overload;
    function Username(const ASection, AIdent, ADefault: String): TKrakenProviderZeosSettings; overload;
    function Username: String; overload;

    function Password(const APassword: String): TKrakenProviderZeosSettings; overload;
    function Password(const ASection, AIdent, ADefault: String): TKrakenProviderZeosSettings; overload;
    function Password: String; overload;

    function Database(const ADatabase: String): TKrakenProviderZeosSettings; overload;
    function Database(const ASection, AIdent, ADefault: String): TKrakenProviderZeosSettings; overload;
    function Database: String; overload;

    function IniPath(const APath: String): TKrakenProviderZeosSettings;

    ///<summary>            Define o autocommit de transacoes (insert, update, delete)            </summary>
    ///<param name="AMode"> Se falso, deve usar o StartTransaction e Commit                       </param>
    ///<remarks>            Default: False                                                        </remarks>
    function AutoCommit(const AMode: Boolean): TKrakenProviderZeosSettings;
  end;

implementation

{ TKrakenProviderZeosSettings }

function TKrakenProviderZeosSettings.AutoCommit(const AMode: Boolean): TKrakenProviderZeosSettings;
begin
  FConnection.AutoCommit := AMode;
end;

constructor TKrakenProviderZeosSettings.Create(AConnetion: TZConnection);
begin
  FConnection := AConnetion;
end;

destructor TKrakenProviderZeosSettings.Destroy;
begin

  inherited;
end;

function TKrakenProviderZeosSettings.HasAssignedIniFile: Boolean;
begin
  Result := FIniFile <> nil;

  if not Result then
    raise Exception.Create('Has not inifile assigned. Try pass parameter on create method.');
end;

function TKrakenProviderZeosSettings.Host(const AHost: String): TKrakenProviderZeosSettings;
begin
  Result := Self;
  FHost  := AHost;

  FConnection.HostName := FHost;
end;

function TKrakenProviderZeosSettings.Host(const ASection, AIdent, ADefault: String): TKrakenProviderZeosSettings;
begin
  Result := Self;
  HasAssignedIniFile;

  FHost := FIniFile.ReadString(ASection, AIdent, ADefault);
  FConnection.HostName := FHost;
end;

function TKrakenProviderZeosSettings.Host: String;
begin
  Result := FHost;
end;

function TKrakenProviderZeosSettings.IniPath(const APath: String): TKrakenProviderZeosSettings;
begin
  Result := Self;

  if APath <> '' then
    FIniFile := TIniFile.Create(APath);
end;

function TKrakenProviderZeosSettings.Port(const APort: Integer): TKrakenProviderZeosSettings;
begin
  Result := Self;
  FPort  := APort;
  FConnection.Port := Port;
end;

function TKrakenProviderZeosSettings.Port(const APort: String): TKrakenProviderZeosSettings;
begin
  Result := Self;
  FPort  := StrToIntDef(APort, 5432);
  FConnection.Port := Port;
end;

function TKrakenProviderZeosSettings.Port(const ASection, AIdent, ADefault: String): TKrakenProviderZeosSettings;
begin
  Result := Self;
  HasAssignedIniFile;

  FPort := StrToInt(FIniFile.ReadString(ASection, AIdent, ADefault));
  FConnection.Port := Port;
end;

function TKrakenProviderZeosSettings.Port: Integer;
begin
  Result := FPort;
end;

function TKrakenProviderZeosSettings.Username(const AUsername: String): TKrakenProviderZeosSettings;
begin
  Result := Self;
  FUsername := AUsername;

  FConnection.User := FUsername;
end;

function TKrakenProviderZeosSettings.Username(const ASection, AIdent, ADefault: String): TKrakenProviderZeosSettings;
begin
  Result := Self;
  HasAssignedIniFile;

  FUsername := FIniFile.ReadString(ASection, AIdent, ADefault);
  FConnection.User := FUsername;
end;

function TKrakenProviderZeosSettings.Username: String;
begin
  Result := FUsername;
end;

function TKrakenProviderZeosSettings.Password(const APassword: String): TKrakenProviderZeosSettings;
begin
  Result := Self;
  FPassword := APassword;

  FConnection.Password := FPassword;
end;

function TKrakenProviderZeosSettings.Password(const ASection, AIdent, ADefault: String): TKrakenProviderZeosSettings;
begin
  Result := Self;
  HasAssignedIniFile;

  FPassword := FIniFile.ReadString(ASection, AIdent, ADefault);
  FConnection.Password := FPassword;
end;

function TKrakenProviderZeosSettings.Password: String;
begin
  Result := FPassword;
end;

function TKrakenProviderZeosSettings.Database(const ADatabase: String): TKrakenProviderZeosSettings;
begin
  Result := Self;
  FDatabase := ADatabase;

  FConnection.Database := FDatabase;
end;

function TKrakenProviderZeosSettings.Database(const ASection, AIdent, ADefault: String): TKrakenProviderZeosSettings;
begin
  Result := Self;
  HasAssignedIniFile;

  FDatabase := FIniFile.ReadString(ASection, AIdent, ADefault);
  FConnection.Database := FDatabase;
end;

function TKrakenProviderZeosSettings.Database: String;
begin
  Result := FDatabase;
end;

end.
