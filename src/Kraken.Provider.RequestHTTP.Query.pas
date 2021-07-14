unit Kraken.Provider.RequestHTTP.Query;

interface

uses
  System.Classes,
  System.SysUtils,
  ZDataset,
  ZConnection,
  System.hash,

  RESTRequest4D,
  REST.Types,
  JsonParser.Helper,
  System.JSON,
  Data.DB;

type
  TKrakenProviderRequestHTTPQuery = class(TZQuery)
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  private
    FId               : String;
    FJSONTmo          : string;
    FStartTransaction : Boolean;
  public
    function  GetInstance: TZQuery;

    function  Id(const Value: String): TKrakenProviderRequestHTTPQuery; overload;
    function  Id: String; overload;

    function  SaveQuery(aPath: String; AFilename: String = ''): TKrakenProviderRequestHTTPQuery; overload;
    function  SaveQuery: String; overload;

    procedure Open; overload;
    procedure ExecSQL;

    procedure Clear;

    property StartTransaction: Boolean read FStartTransaction write FStartTransaction;

    //property temporaria
    property  JSONTmp: string read FJSONTmo write FJSONTmo;

  end;


implementation

uses
  Kraken.Core;

{ TKrakenProviderRequestHTTPQuery }

procedure TKrakenProviderRequestHTTPQuery.Clear;
begin
  SQL.Clear;
  FJSONTmo := '';
  ClearBuffers;
end;

constructor TKrakenProviderRequestHTTPQuery.Create(AOwner: TComponent);
begin
  Inherited Create(AOwner);

  GetInstance.Connection := TZConnection(AOwner);
end;

destructor TKrakenProviderRequestHTTPQuery.Destroy;
begin

  inherited;
end;

function TKrakenProviderRequestHTTPQuery.GetInstance: TZQuery;
begin
  Result := TZQuery(Self);
end;

function TKrakenProviderRequestHTTPQuery.Id(const Value: String): TKrakenProviderRequestHTTPQuery;
begin
  Result    := Self;
  FId       := Value;
  Self.Name := 'ZQuery' + FId;
end;

function TKrakenProviderRequestHTTPQuery.Id: String;
begin
  Result    := FId;
end;

function TKrakenProviderRequestHTTPQuery.SaveQuery(aPath, AFilename: String): TKrakenProviderRequestHTTPQuery;
var
  I               : Integer;
  LArqFile        : TextFile;
  LRecordCount    : String;
  LParams         : TStringList;
  LFilename       : String;
begin
  Result := Self;

  if not DirectoryExists( aPath ) then
    ForceDirectories( aPath );

  if AFilename <> '' then
    LFilename := Format( '%s-%s.log' , [ AFilename, FormatDateTime('yyyy-mm-dd_hh-nn-ss', Now) ] )
  else
    LFilename := Format( 'Log-%s.log', [ FormatDateTime('yyyy-mm-dd_hh-nn-ss', Now) ] );


  AssignFile( LArqFile, aPath + '\' + LFilename );

  LParams := TStringLIst.Create;

  for I := 0 to Pred( Params.Count ) do
    LParams.AddPair( '- ' + Params[I].Name, Params[I].Value );

  try
    LRecordCount := IntToStr( RecordCount );
  except
    LRecordCount := '0';
  end;

  try
    Rewrite( LArqFile                                                            );
    WriteLN( LArqFile, '/*-----------------------------------------------'       );
    WriteLN( LArqFile, ''                                                        );
    WriteLN( LArqFile, 'Data..: ' + FormatDateTime( 'dd/mm/yyyy', Now ) + ' '
                                     + FormatDateTime( 'hh:mm:ss  ', Now )       );
    WriteLN( LArqFile, ''                                                        );
    WriteLN( LArqFile, 'Registros.: ' + LRecordCount                             );
    WriteLN( LArqFile, 'Parametros:'                                             );
    WriteLN( LArqFile, LParams.Text                                              );
    WriteLN( LArqFile, '-----------------------------------------------*/'       );
    WriteLN( LArqFile, ''                                                        );
    WriteLN( LArqFile, SQL.GetText                                               );
  finally
    CloseFile( LArqFile );
    LParams.Free;
  end;
end;

function TKrakenProviderRequestHTTPQuery.SaveQuery: String;
var
  I       : Integer;
  LParams : TStringList;
begin
  Result := '';
  LParams := TStringLIst.Create;

  for I := 0 to Pred( Params.Count ) do
    LParams.AddPair( '- ' + Params[I].Name, Params[I].Value );

  Result := Result + 'Params: ' + LParams.Text + sLineBreak;
  Result := Result + SQL.GetText;

  LParams.Free;
end;

procedure TKrakenProviderRequestHTTPQuery.Open;
var
  LResponse   : IResponse;
  LJsonObj    : TJSONObject;
  Lhash       : string;
  LSQLTmp     : String;
  LURLTmp     : string;
begin
  try
    try
      LSQLTmp := StringReplace(Trim(SQL.GetText) , #$D#$A, '', [rfReplaceAll]);
      LSQLTmp := StringReplace(LSqlTmp, '''', '', [rfReplaceAll]);

      Lhash := THashMD5.GetHashString(LSQLTmp);

      LURLTmp := TKraken.Provider('base').Settings.URLRemoto;

      LJsonObj := TJsonObject.Create();
      LJsonObj.AddPair(TJsonPair.Create('sql', StringReplace(SQL.GetText, '\r\n', '', [rfReplaceAll])));
      LJsonObj.AddPair(TJsonPair.Create('comando', 'open'));
      LJsonObj.AddPair(TJsonPair.Create('starttransaction', BoolToStr(FStartTransaction)));

      LResponse :=
      TRequest.New
        .BaseURL            ( LURLTmp                                                         )
        .Accept             ( 'application/json'                                              )
        .AcceptEncoding     ( 'UTF-8'                                                         )
        .AddHeader          ( 'Content-Type', 'application/json'                              )
        .ContentType        ( 'application/json'                                              )
        .AddHeader          ( 'hash', Lhash                                                   )
        .AddBody            ( LJsonObj, true                                                  )
        .Timeout            ( -1                                                              )
        .Post;

    finally
      if LResponse.StatusCode in [200,201] then
      begin
        FJSONTmo := (TJSONObject.ParseJSONValue(LResponse.Content) as TJSONArray).ToString;
      end
      else
      begin
        FJSONTmo := (TJSONObject.ParseJSONValue(LResponse.Content) as TJSONArray).ToString;
        DatabaseError((TJSONObject.ParseJSONValue(LResponse.Content) as TJSONArray).Items[0].GetValue<string>('erro'));
      end;
    end;
  except
    raise;
  end;
end;

procedure TKrakenProviderRequestHTTPQuery.ExecSQL;
var
  LResponse   : IResponse;
  LJsonObj    : TJSONObject;
  Lhash       : string;
  LSQLTmp     : String;
  LURLTmp     : string;
begin
  try
    try
      LSQLTmp := StringReplace(Trim(SQL.GetText) , #$D#$A, '', [rfReplaceAll]);
      LSQLTmp := StringReplace(LSqlTmp, '''', '', [rfReplaceAll]);

      Lhash := THashMD5.GetHashString(LSQLTmp);

      LURLTmp := TKraken.Provider('base').Settings.URLRemoto;

      LJsonObj := TJsonObject.Create();
      LJsonObj.AddPair(TJsonPair.Create('sql', StringReplace(SQL.GetText, '\r\n', '', [rfReplaceAll])));
      LJsonObj.AddPair(TJsonPair.Create('comando', 'execsql'));
      LJsonObj.AddPair(TJsonPair.Create('starttransaction', BoolToStr(FStartTransaction)));

      LResponse :=
      TRequest.New
        .BaseURL            ( LURLTmp                                                         )
        .Accept             ( 'application/json'                                              )
        .AcceptEncoding     ( 'UTF-8'                                                         )
        .AddHeader          ( 'Content-Type', 'application/json'                              )
        .ContentType        ( 'application/json'                                              )
        .AddHeader          ( 'hash', Lhash                                                   )
        .AddBody            ( LJsonObj, true                                                  )
        .Timeout            ( -1                                                              )
        .Post;

    finally
      if LResponse.StatusCode in [200,201] then
      begin
        FJSONTmo := (TJSONObject.ParseJSONValue(LResponse.Content) as TJSONArray).ToString;
      end
      else
      begin
        FJSONTmo := (TJSONObject.ParseJSONValue(LResponse.Content) as TJSONArray).ToString;
        DatabaseError((TJSONObject.ParseJSONValue(LResponse.Content) as TJSONArray).Items[0].GetValue<string>('erro'));
      end;
    end;
  except
    raise;
  end;
end;

end.
