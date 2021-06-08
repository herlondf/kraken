unit Brave.Provider.Zeos.Settings;

interface

uses
  System.IniFiles,
  System.SysUtils,
  ZConnection;

type
  TBraveProviderZeosSettings = class
    constructor Create(AConnetion: TZConnection); overload;
    constructor Create(AConnetion: TZConnection; AIniPath: String); overload;
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
    function Host(const AHost: String): TBraveProviderZeosSettings; overload;
    function Host(const ASection, AIdent, ADefault: String): TBraveProviderZeosSettings; overload;
    function Host: String; overload;

    function Port(const APort: Integer): TBraveProviderZeosSettings; overload;
    function Port(const APort: String): TBraveProviderZeosSettings; overload;
    function Port(const ASection, AIdent, ADefault: String): TBraveProviderZeosSettings; overload;
    function Port: Integer; overload;

    function Username(const AUsername: String): TBraveProviderZeosSettings; overload;
    function Username(const ASection, AIdent, ADefault: String): TBraveProviderZeosSettings; overload;
    function Username: String; overload;

    function Password(const APassword: String): TBraveProviderZeosSettings; overload;
    function Password(const ASection, AIdent, ADefault: String): TBraveProviderZeosSettings; overload;
    function Password: String; overload;

    function Database(const ADatabase: String): TBraveProviderZeosSettings; overload;
    function Database(const ASection, AIdent, ADefault: String): TBraveProviderZeosSettings; overload;
    function Database: String; overload;
  end;

implementation

{ TBraveProviderZeosSettings }

constructor TBraveProviderZeosSettings.Create(AConnetion: TZConnection);
begin
  FConnection := AConnetion;
end;

constructor TBraveProviderZeosSettings.Create(AConnetion: TZConnection; AIniPath: String);
begin
  FConnection := AConnetion;
  FIniFile.Create(AIniPath);
end;

destructor TBraveProviderZeosSettings.Destroy;
begin

  inherited;
end;

function TBraveProviderZeosSettings.HasAssignedIniFile: Boolean;
begin
  Result := FIniFile <> nil;

  if not Result then
    raise Exception.Create('Has not inifile assigned. Try another create method.');
end;

function TBraveProviderZeosSettings.Host(const AHost: String): TBraveProviderZeosSettings;
begin
  Result := Self;
  FHost  := AHost;

  FConnection.HostName := FHost;
end;

function TBraveProviderZeosSettings.Host(const ASection, AIdent, ADefault: String): TBraveProviderZeosSettings;
begin
  Result := Self;
  HasAssignedIniFile;

  FHost := FIniFile.ReadString(ASection, AIdent, ADefault);
  FConnection.HostName := FHost;
end;

function TBraveProviderZeosSettings.Host: String;
begin
  Result := FHost;
end;

function TBraveProviderZeosSettings.Port(const APort: Integer): TBraveProviderZeosSettings;
begin
  Result := Self;
  FPort  := APort;
  FConnection.Port := Port;
end;

function TBraveProviderZeosSettings.Port(const APort: String): TBraveProviderZeosSettings;
begin
  Result := Self;
  FPort  := StrToIntDef(APort, 5432);
  FConnection.Port := Port;
end;

function TBraveProviderZeosSettings.Port(const ASection, AIdent, ADefault: String): TBraveProviderZeosSettings;
begin
  Result := Self;
  HasAssignedIniFile;

  FPort := StrToInt(FIniFile.ReadString(ASection, AIdent, ADefault));
  FConnection.Port := Port;
end;

function TBraveProviderZeosSettings.Port: Integer;
begin
  Result := FPort;
end;

function TBraveProviderZeosSettings.Username(const AUsername: String): TBraveProviderZeosSettings;
begin
  Result := Self;
  FUsername := AUsername;

  FConnection.User := FUsername;
end;

function TBraveProviderZeosSettings.Username(const ASection, AIdent, ADefault: String): TBraveProviderZeosSettings;
begin
  Result := Self;
  HasAssignedIniFile;

  FUsername := FIniFile.ReadString(ASection, AIdent, ADefault);
  FConnection.User := FUsername;
end;

function TBraveProviderZeosSettings.Username: String;
begin
  Result := FUsername;
end;

function TBraveProviderZeosSettings.Password(const APassword: String): TBraveProviderZeosSettings;
begin
  Result := Self;
  FPassword := APassword;

  FConnection.Password := FPassword;
end;

function TBraveProviderZeosSettings.Password(const ASection, AIdent, ADefault: String): TBraveProviderZeosSettings;
begin
  Result := Self;
  HasAssignedIniFile;

  FPassword := FIniFile.ReadString(ASection, AIdent, ADefault);
  FConnection.Password := FPassword;
end;

function TBraveProviderZeosSettings.Password: String;
begin
  Result := FPassword;
end;

function TBraveProviderZeosSettings.Database(const ADatabase: String): TBraveProviderZeosSettings;
begin
  Result := Self;
  FDatabase := ADatabase;

  FConnection.Database := FDatabase;
end;

function TBraveProviderZeosSettings.Database(const ASection, AIdent, ADefault: String): TBraveProviderZeosSettings;
begin
  Result := Self;
  HasAssignedIniFile;

  FDatabase := FIniFile.ReadString(ASection, AIdent, ADefault);
  FConnection.Database := FDatabase;
end;

function TBraveProviderZeosSettings.Database: String;
begin
  Result := FDatabase;
end;

end.
