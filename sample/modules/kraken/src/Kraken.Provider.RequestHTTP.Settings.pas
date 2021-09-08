unit Kraken.Provider.RequestHTTP.Settings;

interface

uses
  System.IniFiles,
  System.SysUtils;

type
  TKrakenProviderRequestHTTPSettings = class
    constructor Create;
    destructor Destroy; override;
  private
    FIniFile    : TIniFile;
    FHost       : String;
    FPort       : Integer;
    FUsername   : String;
    FPassword   : String;
    FDatabase   : String;
    FURLRemoto  : string;


    function HasAssignedIniFile: Boolean;
  public
    function Host(const AHost: String): TKrakenProviderRequestHTTPSettings; overload;
    function Host(const ASection, AIdent, ADefault: String): TKrakenProviderRequestHTTPSettings; overload;
    function Host: String; overload;

    function Port(const APort: Integer): TKrakenProviderRequestHTTPSettings; overload;
    function Port(const APort: String): TKrakenProviderRequestHTTPSettings; overload;
    function Port(const ASection, AIdent, ADefault: String): TKrakenProviderRequestHTTPSettings; overload;
    function Port: Integer; overload;

    function Username(const AUsername: String): TKrakenProviderRequestHTTPSettings; overload;
    function Username(const ASection, AIdent, ADefault: String): TKrakenProviderRequestHTTPSettings; overload;
    function Username: String; overload;

    function Password(const APassword: String): TKrakenProviderRequestHTTPSettings; overload;
    function Password(const ASection, AIdent, ADefault: String): TKrakenProviderRequestHTTPSettings; overload;
    function Password: String; overload;

    function Database(const ADatabase: String): TKrakenProviderRequestHTTPSettings; overload;
    function Database(const ASection, AIdent, ADefault: String): TKrakenProviderRequestHTTPSettings; overload;
    function Database: String; overload;

    function URLRemoto(const AURLRemoto: String): TKrakenProviderRequestHTTPSettings; overload;
    function URLRemoto(const ASection, AIdent, ADefault: String): TKrakenProviderRequestHTTPSettings; overload;
    function URLRemoto: String; overload;

    function IniPath(const APath: String): TKrakenProviderRequestHTTPSettings;
  end;

implementation

{ TKrakenProviderRequestHTTPSettings }

constructor TKrakenProviderRequestHTTPSettings.Create;
begin
  //
end;

function TKrakenProviderRequestHTTPSettings.Database(const ADatabase: String): TKrakenProviderRequestHTTPSettings;
begin
  Result := Self;
  FDatabase := ADatabase;
end;

function TKrakenProviderRequestHTTPSettings.Database: String;
begin
  Result := FDatabase;
end;

function TKrakenProviderRequestHTTPSettings.Database(const ASection, AIdent, ADefault: String): TKrakenProviderRequestHTTPSettings;
begin
  Result := Self;
  HasAssignedIniFile;

  FDatabase := FIniFile.ReadString(ASection, AIdent, ADefault);
end;

destructor TKrakenProviderRequestHTTPSettings.Destroy;
begin

  inherited;
end;

function TKrakenProviderRequestHTTPSettings.HasAssignedIniFile: Boolean;
begin
  Result := FIniFile <> nil;

  if not Result then
    raise Exception.Create('Has not inifile assigned. Try pass parameter on create method.');
end;

function TKrakenProviderRequestHTTPSettings.Host(const AHost: String): TKrakenProviderRequestHTTPSettings;
begin
  Result := Self;
  FHost  := AHost;
end;

function TKrakenProviderRequestHTTPSettings.Host(const ASection, AIdent, ADefault: String): TKrakenProviderRequestHTTPSettings;
begin
  Result := Self;
  HasAssignedIniFile;

  FHost := FIniFile.ReadString(ASection, AIdent, ADefault);
end;

function TKrakenProviderRequestHTTPSettings.Host: String;
begin
  Result := FHost;
end;

function TKrakenProviderRequestHTTPSettings.IniPath(const APath: String): TKrakenProviderRequestHTTPSettings;
begin
  Result := Self;

  if APath <> '' then
    FIniFile := TIniFile.Create(APath);
end;

function TKrakenProviderRequestHTTPSettings.Password(const ASection, AIdent, ADefault: String): TKrakenProviderRequestHTTPSettings;
begin
  Result := Self;
  HasAssignedIniFile;

  FPassword := FIniFile.ReadString(ASection, AIdent, ADefault);
end;

function TKrakenProviderRequestHTTPSettings.Password(const APassword: String): TKrakenProviderRequestHTTPSettings;
begin
  Result := Self;
  FPassword := APassword;
end;

function TKrakenProviderRequestHTTPSettings.Password: String;
begin
  Result := FPassword;
end;

function TKrakenProviderRequestHTTPSettings.Port(const ASection, AIdent, ADefault: String): TKrakenProviderRequestHTTPSettings;
begin
  Result := Self;
  HasAssignedIniFile;

  FPort := StrToInt(FIniFile.ReadString(ASection, AIdent, ADefault));
end;

function TKrakenProviderRequestHTTPSettings.Port: Integer;
begin
  Result := FPort;
end;

function TKrakenProviderRequestHTTPSettings.Port(const APort: Integer): TKrakenProviderRequestHTTPSettings;
begin
  Result := Self;
  FPort  := APort;
end;

function TKrakenProviderRequestHTTPSettings.Port(const APort: String): TKrakenProviderRequestHTTPSettings;
begin
  Result := Self;
  FPort  := StrToIntDef(APort, 5432);
end;

function TKrakenProviderRequestHTTPSettings.URLRemoto(const AURLRemoto: String): TKrakenProviderRequestHTTPSettings;
begin
  Result := Self;
  FURLRemoto  := AURLRemoto;
end;

function TKrakenProviderRequestHTTPSettings.URLRemoto: String;
begin
  Result := FURLRemoto;
end;

function TKrakenProviderRequestHTTPSettings.Username(const AUsername: String): TKrakenProviderRequestHTTPSettings;
begin
  Result := Self;
  FUsername := AUsername;
end;

function TKrakenProviderRequestHTTPSettings.Username(const ASection, AIdent, ADefault: String): TKrakenProviderRequestHTTPSettings;
begin
  Result := Self;
  HasAssignedIniFile;

  FUsername := FIniFile.ReadString(ASection, AIdent, ADefault);
end;

function TKrakenProviderRequestHTTPSettings.Username: String;
begin
  Result := FUsername;
end;

function TKrakenProviderRequestHTTPSettings.URLRemoto(const ASection, AIdent, ADefault: String): TKrakenProviderRequestHTTPSettings;
begin
  Result := Self;
  HasAssignedIniFile;

  FURLRemoto := FIniFile.ReadString(ASection, AIdent, ADefault);
end;

end.
