unit KrakenFTP;

interface

uses
  {Embarcadero}
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  System.Classes,
  System.SysUtils,
  System.Variants,
  Winapi.Windows,
  System.Generics.Collections,

  {Indy}
  IdText,
  IdContext,
  IdFTPServer,
  IdAttachmentFile,
  IdBaseComponent,
  IdTCPConnection,
  IdExplicitTLSClientServerBase,
  IdComponent,
  IdTCPClient,
  IdIOHandler,
  IdCustomTCPServer,
  IdFTP,
  IdIOHandlerSocket,
  IdTCPServer,
  IdGlobal,
  IdIOHandlerStack,
  IdCmdTCPServer,
  IdFTPCommon,
  IdSSL,
  IdAntiFreezeBase,
  IdSMTP,
  IdIntercept,
  IdAntiFreeze,
  IdSSLOpenSSL,
  IdFTPList,
  IdMessage,
  IdAllFTPListParsers,

  {Kraken}
  KrakenFTP.Interfaces;

type
  TKrakenFTP = class(TInterfacedObject, iKrakenFTP)
    constructor Create;
    destructor Destroy; override;
    class function New: iKrakenFTP;
  strict private
    FThread        : TThread;
  private
    FIdFTP : TIdFTP;
    FProxy : TIdFtpProxySettings;
    FIdSSL : TIdSSLIOHandlerSocketOpenSSL;

    { Integration }
    FKrakenFTPLog        : TKrakenFTPLog;
    FProgressBar        : TProgressBar;
    FProgressBarLabel   : TLabel;

    function Connect    : iKrakenFTP;

    procedure OnConnected( Sender: TObject                                              );
    procedure OnWork     ( ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64    );
    procedure OnWorkBegin( ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64 );
    procedure OnWorkEnd  ( ASender: TObject; AWorkMode: TWorkMode                       );
    procedure OnStatus   ( ASender: TObject; const AStatus: TIdStatus; const AStatusText: string );

    procedure Log( const Value: String );
    { Private declarations }
  public
    { Integration }
    function OnLog          ( const Value: TKrakenFTPLog ): iKrakenFTP;
    function OnProgress     ( const Value: TComponent   ): iKrakenFTP;
    function OnProgressLabel( const Value: TComponent   ): iKrakenFTP;

    { Attributes of connection }
    function Hostname      ( const Value: String  ): iKrakenFTP;
    function Username      ( const Value: String  ): iKrakenFTP;
    function Password      ( const Value: String  ): iKrakenFTP;
    function Port          ( const Value: Integer ): iKrakenFTP; overload;
    function Port          ( const Value: String  ): iKrakenFTP; overload;
    function ProxyHostname ( const Value: String  ): iKrakenFTP;
    function ProxyUsername ( const Value: String  ): iKrakenFTP;
    function ProxyPassword ( const Value: String  ): iKrakenFTP;
    function ProxyPort     ( const Value: Integer ): iKrakenFTP; overload;
    function ProxyPort     ( const Value: String  ): iKrakenFTP; overload;
    function Passive       ( const Value: Boolean ): iKrakenFTP;
    function UseTLS        ( const Value: Boolean ): iKrakenFTP;

    { Tranfers files }
    function Get( const ASource, aDest: String ): iKrakenFTP;
    function Put( const ASource, aDest: String ): iKrakenFTP;

    { Manipulating of directory and files }
    function RootDir   : iKrakenFTP;
    function CurrentDir: iKrakenFTP;
    function ListDir   : iKrakenFTP;

    function CreateDir( const ADirectory: String ): iKrakenFTP;
    function ChangeDir( const ADirectory: String ): iKrakenFTP;

    function ExistsFile( AFilename: String = '' ): Boolean;

    { Thread Tratament }
    function Start: iKrakenFTP;
    { Public declarations }
  end;

implementation

{ TKrakenFTP }

constructor TKrakenFTP.Create;
begin
  FIdFTP := TIdFTP.Create(nil);

  FIdFTP.UseMLIS               := True;
  FIdFTP.UseHOST               := True;
  FIdFTP.ListenTimeout         := 10000;
  FIdFTP.ReadTimeout           := 30000;
  FIdFTP.TransferTimeout       := 10000;
  FIdFTP.TransferType          := ftBinary;
  FIdFTP.Passive               := True;
  FIdFTP.PassiveUseControlHost := True;
  FIdFTP.Port                  := 21;

  FIdFTP.OnConnected           := OnConnected;
  FIdFTP.OnStatus              := OnStatus;
  FIdFTP.OnWork                := OnWork;
  FIdFTP.OnWorkBegin           := OnWorkBegin;
  FIdFTP.OnWorkEnd             := OnWorkEnd;

  FProxy                       := TIdFtpProxySettings.Create;

  FIdSSL                       := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
end;

destructor TKrakenFTP.Destroy;
begin
  FreeAndNil( FIdFTP   );
  FreeAndNil( FProxy   );
  FreeAndNil( FIdSSL   );

  inherited;
end;

class function TKrakenFTP.New: iKrakenFTP;
begin
  Result := Self.Create;
end;

procedure TKrakenFTP.OnConnected( Sender: TObject );
begin
  //
end;

procedure TKrakenFTP.OnStatus( ASender: TObject; const AStatus: TIdStatus; const AStatusText: string );
begin
  case AStatus of
    hsResolving     : Log(AStatusText);
    hsConnecting    : Log(AStatusText);
    hsConnected     : Log(AStatusText);
    hsDisconnecting : Log(AStatusText);
    hsDisconnected  : Log(AStatusText);
    hsStatusText    : Log(AStatusText);
    ftpTransfer     : Log(AStatusText);
    ftpReady        : Log(AStatusText);
    ftpAborted      : Log(AStatusText);
  end;
end;

procedure TKrakenFTP.OnWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
begin
  TThread.CreateAnonymousThread(
  procedure
  begin
    TThread.Synchronize(TThread.Current,
    procedure
    var
      LSizeTransfered: Integer;
    begin
      LSizeTransfered := AWorkCount div 1024;

      if Assigned( FProgressBarLabel ) then
        FProgressBarLabel.Caption := 'Transferido: ' + IntToStr( LSizeTransfered ) + '/kb.';


      if Assigned( FProgressBar ) then
      begin
        if FProgressBar.Tag = 0 then
          FProgressBar.Position := AWorkCount
        else
          FProgressBar.Position := LSizeTransfered;
      end;
    end);
  end).Start;
end;

procedure TKrakenFTP.OnWorkBegin(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
begin
  if Assigned( FProgressBar ) then
  begin
    if AWorkCountMax > 0 then
    begin
      FProgressBar.Max := AWorkCountMax;
      FProgressBar.Tag := 0;
    end
    else
    begin
      FProgressBar.Max := 100;
      FProgressBar.Tag := 1;
    end;

    FProgressBar.Min      := 0;
    FProgressBar.Position := 0;
  end;

  if Assigned(FProgressBarLabel) then
    FProgressBarLabel.Visible := True;
end;

procedure TKrakenFTP.OnWorkEnd(ASender: TObject; AWorkMode: TWorkMode);
begin
  if Assigned( FProgressBar ) then
    FProgressBar.Position := 0;

  if Assigned(FProgressBarLabel) then
    FProgressBarLabel.Visible := False;
end;

procedure TKrakenFTP.Log( const Value: String );
begin
  if Assigned( FKrakenFTPLog ) then
    FKrakenFTPLog( Value );
end;

function TKrakenFTP.Connect: iKrakenFTP;
begin
  Result := Self;

  try
    if FidFTP.Connected then
      FIdFTP.Disconnect;

    FIdFTP.Connect();
  except
    on E: Exception do
    begin
      Log( 'Houston, we have problem...' );
      Log( E.Message );
      raise;
    end;
  end;
end;

function TKrakenFTP.Hostname( const Value: String  ): iKrakenFTP;
begin
  Result      := Self;
  FIdFTP.Host := Value;
end;

function TKrakenFTP.Username( const Value: String  ): iKrakenFTP;
begin
  Result          := Self;
  FIdFTP.Username := Value;
end;

function TKrakenFTP.Password( const Value: String  ): iKrakenFTP;
begin
  Result          := Self;
  FIdFTP.Password := Value;
end;

function TKrakenFTP.Port( const Value: Integer ): iKrakenFTP;
begin
  Result      := Self;
  FIdFTP.Port := Value;
end;

function TKrakenFTP.Port( const Value: String ): iKrakenFTP;
begin
  Result      := Self;
  FIdFTP.Port := StrToIntDef( Value, 21 );
end;

function TKrakenFTP.ProxyHostname( const Value: String  ): iKrakenFTP;
begin
  Result := Self;

  if not Assigned( FIdFTP.ProxySettings ) then
    FIdFTP.ProxySettings := FProxy;

  FProxy.Host := Value;
end;

function TKrakenFTP.ProxyUsername( const Value: String  ): iKrakenFTP;
begin
  Result := Self;

  if not Assigned( FIdFTP.ProxySettings ) then
    FIdFTP.ProxySettings := FProxy;

  FProxy.UserName := Value;
end;

function TKrakenFTP.ProxyPassword( const Value: String  ): iKrakenFTP;
begin
  Result := Self;

  if not Assigned( FIdFTP.ProxySettings ) then
    FIdFTP.ProxySettings := FProxy;

  FProxy.Password := Value;
end;

function TKrakenFTP.ProxyPort( const Value: Integer ): iKrakenFTP;
begin
  Result := Self;

  if not Assigned( FIdFTP.ProxySettings ) then
    FIdFTP.ProxySettings := FProxy;

  FProxy.Port := Value;
end;

function TKrakenFTP.ProxyPort( const Value: String ): iKrakenFTP;
begin
  Result := Self;

  if not Assigned( FIdFTP.ProxySettings ) then
    FIdFTP.ProxySettings := FProxy;

  FProxy.Port := StrToIntDef( Value, 21 );
end;

function TKrakenFTP.Passive( const Value: Boolean ): iKrakenFTP;
begin
  Result                       := Self;
  FIdFTP.Passive               := Value;
  FIdFTP.PassiveUseControlHost := Value;
end;

function TKrakenFTP.UseTLS( const Value: Boolean ): iKrakenFTP;
begin
  Result := Self;

  if Value then
  begin
    FIdFTP.IOHandler           := FIdSSL;
    FIdFTP.UseTLS              := utUseExplicitTLS;
    FIdFTP.AUTHCmd             := tAuthTLS;
    FIdFTP.DataPortProtection  := ftpdpsPrivate;
  end;
end;

function TKrakenFTP.OnLog( const Value: TKrakenFTPLog ): iKrakenFTP;
begin
  Result       := Self;
  FKrakenFTPLog := Value;
end;

function TKrakenFTP.OnProgress( const Value: TComponent   ): iKrakenFTP;
begin
  Result       := Self;
  FProgressBar := TProgressBar( Value );
end;

function TKrakenFTP.OnProgressLabel( const Value: TComponent   ): iKrakenFTP;
begin
  Result := Self;
  FProgressBarLabel := TLabel( Value );
end;

function TKrakenFTP.Get(const ASource, aDest: String): iKrakenFTP;
begin
  Result := Self;

  if FileExists( ADest ) then
    DeleteFile( PWideChar( ADest ) );

  while Assigned(FThread) do
    TThread.Sleep(50);

  FThread :=
    TThread.CreateAnonymousThread(
    procedure
    begin
      try
        try
          Connect;

          if FIdFTP.Connected then
            FIdFTP.Get(ASource, aDest, False);
        except
          on E: Exception do
          begin
            Log(E.Message);
            FIdFTP.KillDataChannel;
            FIdFTP.Disconnect;
          end;
        end;
      finally
        FIdFTP.KillDataChannel;
        FIdFTP.Disconnect;
      end;
    end);
end;

function TKrakenFTP.Put(const ASource, aDest: String): iKrakenFTP;
var
  LSourceStream: TStream;
  LDestFileName: String;
begin
  Result := Self;

//  while Assigned(FThread) do
//    TThread.Sleep(50);
//
//  FThread :=
//    TThread.CreateAnonymousThread(
//    procedure
//    begin
      LDestFileName := ADest;

      if LDestFileName = '' then begin
        LDestFileName := ExtractFileName( ASource );
      end;

      LSourceStream := TIdReadFileNonExclusiveStream.Create( ASource );

      try
        try
          Connect;

          if FIdFTP.Connected then
            FIdFTP.Put( LSourceStream, LDestFileName, True, 0 );
        finally
          FreeAndNil(LSourceStream);
          FIdFTP.KillDataChannel;
          FIdFTP.Disconnect;
        end;
      except
        on E: Exception do
        begin
          Log(E.Message);
          FreeAndNil(LSourceStream);
          FIdFTP.KillDataChannel;
          FIdFTP.Disconnect;
        end;
      end;
//    end);
end;

function TKrakenFTP.RootDir: iKrakenFTP;
begin
  Result := Self;

  if FIdFTP.Connected then
  begin
    while FIdFTP.RetrieveCurrentDir <> '/' do
    begin
      Sleep(5000);
      FIdFTP.ChangeDirUp;
    end;
  end;
end;

function TKrakenFTP.CurrentDir: iKrakenFTP;
begin
  Result := Self;

  if FIdFTP.Connected then
    FIdFTP.RetrieveCurrentDir;
end;

function TKrakenFTP.ListDir: iKrakenFTP;
var
  I: Integer;
  LDirectorys: TStringList;
begin
  LDirectorys := TStringList.Create;
  FIdFTP.List( LDirectorys, '', false );

  for I := 0 to Pred( LDirectorys.Count ) do
    Log( LDirectorys.Strings[I] );

  LDirectorys.Free;
end;

function TKrakenFTP.CreateDir(const ADirectory: String): iKrakenFTP;
var
  LDirectorys: TStringList;
begin
  if FIdFTP.Connected then
  begin
    try
      LDirectorys := TStringList.Create;

      FIdFTP.List( LDirectorys, '', false );

      if LDirectorys.IndexOf( aDirectory ) <> - 1 then
        FIdFTP.ChangeDir( aDirectory )
      else
      begin
        try
          FIdFTP.MakeDir( aDirectory );
        finally
          FIdFTP.ChangeDir( aDirectory );
          FreeAndNil( LDirectorys );
        end;
      end;
    except
      FreeAndNil( LDirectorys );
    end;
  end;
end;

function TKrakenFTP.ChangeDir(const ADirectory: String): iKrakenFTP;
begin
  try
    FIdFTP.ChangeDir( ADirectory );
  except
    try
      Log('Ops, this directory dont exists but lets create...');
      CreateDir( ADirectory );

      Sleep(1000);

      FIdFTP.ChangeDir( ADirectory );
      Log('Directory actual is ' + ADirectory);
    except
      On E: Exception do
      begin
        Log('Ops, we have a problem...' + E.Message);
        raise;
      end;
    end;

  end;
end;

function TKrakenFTP.ExistsFile( AFilename: String = '' ): Boolean;
var
  I           : Integer;
  LDirectorys : TStringList;
begin
  LDirectorys := TStringList.Create;

  try
    FIdFTP.List( LDirectorys, '', False );
  except
    //
  end;

  for I := 0 to Pred( LDirectorys.Count ) do
  begin
    Result := AnsiUpperCase( AFilename ) = AnsiUpperCase( LDirectorys.Strings[I] );

    if Result then Break
  end;

  LDirectorys.Free;
end;

function TKrakenFTP.Start: iKrakenFTP;
begin
  Result := Self;

  FThread.FreeOnTerminate := True;
  FThread.Start;

  while not FThread.Finished do
    TThread.Sleep(50);

  FThread := nil;
end;

end.
