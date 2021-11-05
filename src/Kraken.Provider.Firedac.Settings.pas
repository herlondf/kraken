unit Kraken.Provider.Firedac.Settings;

interface

uses
  System.IniFiles,
  System.SysUtils,
  FireDAC.Stan.Intf,
  FireDAC.Comp.Client;

type
  TFDStanAsyncMode = FireDAC.Stan.Intf.TFDStanAsyncMode;

  TKrakenProviderFiredacSettings = class
    constructor Create(AConnetion: TFDCustomConnection);
    destructor Destroy; override;
  private
    FIniFile    : TIniFile;
    FConnection : TFDCustomConnection;
    FHost       : String;
    FPort       : Integer;
    FUsername   : String;
    FPassword   : String;
    FDatabase   : String;
    FURLRemoto  : string;
    FTimeout    : String;

    function HasAssignedIniFile: Boolean;
  public
    function Host(const AHost: String): TKrakenProviderFiredacSettings; overload;
    function Host(const ASection, AIdent, ADefault: String): TKrakenProviderFiredacSettings; overload;
    function Host: String; overload;

    function Port(const APort: Integer): TKrakenProviderFiredacSettings; overload;
    function Port(const APort: String): TKrakenProviderFiredacSettings; overload;
    function Port(const ASection, AIdent, ADefault: String): TKrakenProviderFiredacSettings; overload;
    function Port: Integer; overload;

    function Username(const AUsername: String): TKrakenProviderFiredacSettings; overload;
    function Username(const ASection, AIdent, ADefault: String): TKrakenProviderFiredacSettings; overload;
    function Username: String; overload;

    function Password(const APassword: String): TKrakenProviderFiredacSettings; overload;
    function Password(const ASection, AIdent, ADefault: String): TKrakenProviderFiredacSettings; overload;
    function Password: String; overload;

    function Database(const ADatabase: String): TKrakenProviderFiredacSettings; overload;
    function Database(const ASection, AIdent, ADefault: String): TKrakenProviderFiredacSettings; overload;
    function Database: String; overload;

    function URLRemoto(const AURLRemoto: String): TKrakenProviderFiredacSettings; overload;
    function URLRemoto(const ASection, AIdent, ADefault: String): TKrakenProviderFiredacSettings; overload;
    function URLRemoto: String; overload;

    function TimeOut(const ATimeout: String): TKrakenProviderFiredacSettings; overload;
    function TimeOut(const ASection, AIdent, ADefault: String): TKrakenProviderFiredacSettings; overload;
    function Timeout: Integer; overload;

    function IniPath(const APath: String): TKrakenProviderFiredacSettings;

    ///<summary>            Define o modo de bloqueio da execucao da query                        </summary>
    ///<param name="AMode"> amBlocking, amNonBlocking, amCancelDialog, amAsync                    </param>
    ///<remarks>            Default: amBlocking                                                   </remarks>
    function AsyncMode(const AMode: TFDStanAsyncMode): TKrakenProviderFiredacSettings;

    ///<summary>            Define o autocommit de transacoes (insert, update, delete)            </summary>
    ///<param name="AMode"> Se verdadeiro, deve usar o StartTransaction e Commit                  </param>
    ///<remarks>            Default: False                                                        </remarks>
    function AutoCommit(const AMode: Boolean): TKrakenProviderFiredacSettings;

    ///<summary>            Define a capacidade de auto-reconectar ao banco apos perda de conexao </summary>
    ///<param name="AMode"> Se falso, deve implementar rotina de auto-reconectar                  </param>
    ///<remarks>            Default: True                                                         </remarks>
    function AutoRecoverConnection(const AMode: Boolean): TKrakenProviderFiredacSettings;

  end;

implementation

{ TKrakenProviderFiredacSettings }

constructor TKrakenProviderFiredacSettings.Create(AConnetion: TFDCustomConnection);
begin
  FConnection := AConnetion;
end;

destructor TKrakenProviderFiredacSettings.Destroy;
begin

  inherited;
end;

function TKrakenProviderFiredacSettings.HasAssignedIniFile: Boolean;
begin
  Result := FIniFile <> nil;

  if not Result then
    raise Exception.Create('Has not inifile assigned. Try pass parameter on create method.');
end;

function TKrakenProviderFiredacSettings.Host(const AHost: String): TKrakenProviderFiredacSettings;
begin
  Result := Self;
  FHost  := AHost;

  FConnection.Params.Add('Server='+FHost);
end;

function TKrakenProviderFiredacSettings.Host(const ASection, AIdent, ADefault: String): TKrakenProviderFiredacSettings;
begin
  Result := Self;
  HasAssignedIniFile;

  FHost := FIniFile.ReadString(ASection, AIdent, ADefault);
  FConnection.Params.Add('Server='+FHost);
end;

function TKrakenProviderFiredacSettings.Host: String;
begin
  Result := FHost;
end;

function TKrakenProviderFiredacSettings.IniPath(const APath: String): TKrakenProviderFiredacSettings;
begin
  Result := Self;

  if APath <> '' then
    FIniFile := TIniFile.Create(APath);
end;

function TKrakenProviderFiredacSettings.Port(const APort: Integer): TKrakenProviderFiredacSettings;
begin
  Result := Self;
  FPort  := APort;
  FConnection.Params.Add('Port='+IntToStr(FPort));
end;

function TKrakenProviderFiredacSettings.Port(const APort: String): TKrakenProviderFiredacSettings;
begin
  Result := Self;
  FPort  := StrToIntDef(APort, 5432);
  FConnection.Params.Add('Port='+IntToStr(FPort));
end;

function TKrakenProviderFiredacSettings.Port(const ASection, AIdent, ADefault: String): TKrakenProviderFiredacSettings;
begin
  Result := Self;
  HasAssignedIniFile;

  FPort := StrToInt(FIniFile.ReadString(ASection, AIdent, ADefault));
  FConnection.Params.Add('Port='+IntToStr(FPort));
end;

function TKrakenProviderFiredacSettings.Port: Integer;
begin
  Result := FPort;
end;

function TKrakenProviderFiredacSettings.Username(const AUsername: String): TKrakenProviderFiredacSettings;
begin
  Result := Self;
  FUsername := AUsername;

  FConnection.Params.UserName := FUsername;
end;

function TKrakenProviderFiredacSettings.Username(const ASection, AIdent, ADefault: String): TKrakenProviderFiredacSettings;
begin
  Result := Self;
  HasAssignedIniFile;

  FUsername := FIniFile.ReadString(ASection, AIdent, ADefault);
  FConnection.Params.UserName := FUsername;
end;

function TKrakenProviderFiredacSettings.Username: String;
begin
  Result := FUsername;
end;

function TKrakenProviderFiredacSettings.Password(const APassword: String): TKrakenProviderFiredacSettings;
begin
  Result := Self;
  FPassword := APassword;

  FConnection.Params.Password := FPassword;
end;

function TKrakenProviderFiredacSettings.Password(const ASection, AIdent, ADefault: String): TKrakenProviderFiredacSettings;
begin
  Result := Self;
  HasAssignedIniFile;

  FPassword := FIniFile.ReadString(ASection, AIdent, ADefault);
  FConnection.Params.Password := FPassword;
end;

function TKrakenProviderFiredacSettings.Password: String;
begin
  Result := FPassword;
end;

function TKrakenProviderFiredacSettings.Database(const ADatabase: String): TKrakenProviderFiredacSettings;
begin
  Result := Self;
  FDatabase := ADatabase;

  FConnection.Params.Database := FDatabase;
end;

function TKrakenProviderFiredacSettings.Database(const ASection, AIdent, ADefault: String): TKrakenProviderFiredacSettings;
begin
  Result := Self;
  HasAssignedIniFile;

  FDatabase := FIniFile.ReadString(ASection, AIdent, ADefault);
  FConnection.Params.Database := FDatabase;
end;

function TKrakenProviderFiredacSettings.Database: String;
begin
  Result := FDatabase;
end;

function TKrakenProviderFiredacSettings.URLRemoto(const ASection, AIdent, ADefault: String): TKrakenProviderFiredacSettings;
begin
  Result := Self;
  HasAssignedIniFile;

  FURLRemoto := FIniFile.ReadString(ASection, AIdent, ADefault);
end;

function TKrakenProviderFiredacSettings.URLRemoto(const AURLRemoto: String): TKrakenProviderFiredacSettings;
begin
  Result := Self;
  FURLRemoto  := AURLRemoto;
end;

function TKrakenProviderFiredacSettings.URLRemoto: String;
begin
  Result := FURLRemoto;
end;

function TKrakenProviderFiredacSettings.TimeOut(const ATimeout: String): TKrakenProviderFiredacSettings;
begin
  Result   := Self;
  FTimeout := ATimeOut;

  FConnection.Params.Add('LoginTimeout='+FTimeout);
end;

function TKrakenProviderFiredacSettings.TimeOut(const ASection, AIdent, ADefault: String): TKrakenProviderFiredacSettings;
begin
  Result := Self;
  HasAssignedIniFile;

  FTimeout := FIniFile.ReadString(ASection, AIdent, ADefault);
end;

function TKrakenProviderFiredacSettings.Timeout: Integer;
begin
  Result := StrToIntDef(FTimeout, 0);
end;

function TKrakenProviderFiredacSettings.AsyncMode(const AMode: TFDStanAsyncMode): TKrakenProviderFiredacSettings;
begin
  Result := Self;
  FConnection.ResourceOptions.CmdExecMode := AMode
end;

function TKrakenProviderFiredacSettings.AutoCommit(const AMode: Boolean): TKrakenProviderFiredacSettings;
begin
  Result := Self;
  FConnection.TxOptions.AutoCommit := AMode;
end;

function TKrakenProviderFiredacSettings.AutoRecoverConnection(const AMode: Boolean): TKrakenProviderFiredacSettings;
begin
  Result := Self;

  FConnection.ResourceOptions.AutoReconnect  := AMode;
  FConnection.ResourceOptions.KeepConnection := AMode;
end;

end.
