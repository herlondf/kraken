unit Brave.Provider.Firedac.Settings;

interface

uses
  System.IniFiles,
  System.SysUtils,
  FireDAC.Stan.Intf,
  FireDAC.Comp.Client;

type
  TFDStanAsyncMode = FireDAC.Stan.Intf.TFDStanAsyncMode;

  TBraveProviderFiredacSettings = class
    constructor Create(AConnetion: TFDCustomConnection; AIniPath: String = '');
    destructor Destroy; override;
  private
    FIniFile    : TIniFile;
    FConnection : TFDCustomConnection;
    FHost       : String;
    FPort       : Integer;
    FUsername   : String;
    FPassword   : String;
    FDatabase   : String;

    function HasAssignedIniFile: Boolean;
  public
    function Host(const AHost: String): TBraveProviderFiredacSettings; overload;
    function Host(const ASection, AIdent, ADefault: String): TBraveProviderFiredacSettings; overload;
    function Host: String; overload;

    function Port(const APort: Integer): TBraveProviderFiredacSettings; overload;
    function Port(const APort: String): TBraveProviderFiredacSettings; overload;
    function Port(const ASection, AIdent, ADefault: String): TBraveProviderFiredacSettings; overload;
    function Port: Integer; overload;

    function Username(const AUsername: String): TBraveProviderFiredacSettings; overload;
    function Username(const ASection, AIdent, ADefault: String): TBraveProviderFiredacSettings; overload;
    function Username: String; overload;

    function Password(const APassword: String): TBraveProviderFiredacSettings; overload;
    function Password(const ASection, AIdent, ADefault: String): TBraveProviderFiredacSettings; overload;
    function Password: String; overload;

    function Database(const ADatabase: String): TBraveProviderFiredacSettings; overload;
    function Database(const ASection, AIdent, ADefault: String): TBraveProviderFiredacSettings; overload;
    function Database: String; overload;

    ///<summary>            Define o modo de bloqueio da execucao da query                        </summary>
    ///<param name="AMode"> amBlocking, amNonBlocking, amCancelDialog, amAsync                    </param>
    ///<remarks>            Default: amBlocking                                                   </remarks>
    function AsyncMode(const AMode: TFDStanAsyncMode): TBraveProviderFiredacSettings;

    ///<summary>            Define o autocommit de transacoes (insert, update, delete)            </summary>
    ///<param name="AMode"> Se falso, deve usar o StartTransaction e Commit                       </param>
    ///<remarks>            Default: False                                                        </remarks>
    function AutoCommit(const AMode: Boolean): TBraveProviderFiredacSettings;

    ///<summary>            Define a capacidade de auto-reconectar ao banco apos perda de conexao </summary>
    ///<param name="AMode"> Se falso, deve implementar rotina de auto-reconectar                  </param>
    ///<remarks>            Default: True                                                         </remarks>
    function AutoRecoverConnection(const AMode: Boolean): TBraveProviderFiredacSettings;

  end;

implementation

{ TBraveProviderFiredacSettings }

constructor TBraveProviderFiredacSettings.Create(AConnetion: TFDCustomConnection; AIniPath: String);
begin
  FConnection := AConnetion;

  if AIniPath <> '' then
    FIniFile := TIniFile.Create(AIniPath);
end;

destructor TBraveProviderFiredacSettings.Destroy;
begin

  inherited;
end;

function TBraveProviderFiredacSettings.HasAssignedIniFile: Boolean;
begin
  Result := FIniFile <> nil;

  if not Result then
    raise Exception.Create('Has not inifile assigned. Try pass parameter on create method.');
end;

function TBraveProviderFiredacSettings.Host(const AHost: String): TBraveProviderFiredacSettings;
begin
  Result := Self;
  FHost  := AHost;

  FConnection.Params.Add('Server='+FHost);
end;

function TBraveProviderFiredacSettings.Host(const ASection, AIdent, ADefault: String): TBraveProviderFiredacSettings;
begin
  Result := Self;
  HasAssignedIniFile;

  FHost := FIniFile.ReadString(ASection, AIdent, ADefault);
  FConnection.Params.Add('Server='+FHost);
end;

function TBraveProviderFiredacSettings.Host: String;
begin
  Result := FHost;
end;

function TBraveProviderFiredacSettings.Port(const APort: Integer): TBraveProviderFiredacSettings;
begin
  Result := Self;
  FPort  := APort;
  FConnection.Params.Add('Port='+IntToStr(FPort));
end;

function TBraveProviderFiredacSettings.Port(const APort: String): TBraveProviderFiredacSettings;
begin
  Result := Self;
  FPort  := StrToIntDef(APort, 5432);
  FConnection.Params.Add('Port='+IntToStr(FPort));
end;

function TBraveProviderFiredacSettings.Port(const ASection, AIdent, ADefault: String): TBraveProviderFiredacSettings;
begin
  Result := Self;
  HasAssignedIniFile;

  FPort := StrToInt(FIniFile.ReadString(ASection, AIdent, ADefault));
  FConnection.Params.Add('Port='+IntToStr(FPort));
end;

function TBraveProviderFiredacSettings.Port: Integer;
begin
  Result := FPort;
end;

function TBraveProviderFiredacSettings.Username(const AUsername: String): TBraveProviderFiredacSettings;
begin
  Result := Self;
  FUsername := AUsername;

  FConnection.Params.UserName := FUsername;
end;

function TBraveProviderFiredacSettings.Username(const ASection, AIdent, ADefault: String): TBraveProviderFiredacSettings;
begin
  Result := Self;
  HasAssignedIniFile;

  FUsername := FIniFile.ReadString(ASection, AIdent, ADefault);
  FConnection.Params.UserName := FUsername;
end;

function TBraveProviderFiredacSettings.Username: String;
begin
  Result := FUsername;
end;

function TBraveProviderFiredacSettings.Password(const APassword: String): TBraveProviderFiredacSettings;
begin
  Result := Self;
  FPassword := APassword;

  FConnection.Params.Password := FPassword;
end;

function TBraveProviderFiredacSettings.Password(const ASection, AIdent, ADefault: String): TBraveProviderFiredacSettings;
begin
  Result := Self;
  HasAssignedIniFile;

  FPassword := FIniFile.ReadString(ASection, AIdent, ADefault);
  FConnection.Params.Password := FPassword;
end;

function TBraveProviderFiredacSettings.Password: String;
begin
  Result := FPassword;
end;

function TBraveProviderFiredacSettings.Database(const ADatabase: String): TBraveProviderFiredacSettings;
begin
  Result := Self;
  FDatabase := ADatabase;

  FConnection.Params.Database := FDatabase;
end;

function TBraveProviderFiredacSettings.Database(const ASection, AIdent, ADefault: String): TBraveProviderFiredacSettings;
begin
  Result := Self;
  HasAssignedIniFile;

  FDatabase := FIniFile.ReadString(ASection, AIdent, ADefault);
  FConnection.Params.Database := FDatabase;
end;

function TBraveProviderFiredacSettings.Database: String;
begin
  Result := FDatabase;
end;

function TBraveProviderFiredacSettings.AsyncMode(const AMode: TFDStanAsyncMode): TBraveProviderFiredacSettings;
begin
  Result := Self;
  FConnection.ResourceOptions.CmdExecMode := AMode
end;

function TBraveProviderFiredacSettings.AutoCommit(const AMode: Boolean): TBraveProviderFiredacSettings;
begin
  Result := Self;
  FConnection.TxOptions.AutoCommit := AMode;
end;

function TBraveProviderFiredacSettings.AutoRecoverConnection(const AMode: Boolean): TBraveProviderFiredacSettings;
begin
  Result := Self;

  FConnection.ResourceOptions.AutoReconnect  := AMode;
  FConnection.ResourceOptions.KeepConnection := AMode;
end;

end.
