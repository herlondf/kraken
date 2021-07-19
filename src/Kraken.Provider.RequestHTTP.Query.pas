unit Kraken.Provider.RequestHTTP.Query;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Hash,

  RESTRequest4D,
  REST.Types,
  JsonParser.Helper,
  System.JSON,
  Data.DB,

  Kraken.Provider.Fields,
  Kraken.Provider.Param;

type
  TRequestCommand = (rcExecSQL, rcOpen);

  TKrakenProviderRequestHTTPQuery = class(TDataSet)
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  private
    FId               : String;
    FSQL              : TStringList;
    FEndpoint         : String;
    FJSONResponse     : TJSONArray;
    FStartTransaction : Boolean;
    FFieldByName      : TKrakenProviderFields;
    FParamByName      : TKrakenProviderParam;
    FPosItem          : Integer;

    function  RequestCommandToString(ARequestCommand: TRequestCommand): String;
    function  HashGenerate: String;
    function  RequestPrepared: String;

    procedure Request(ARequestCommand: TRequestCommand; AStartTransaction: Boolean);
  public
    function  GetInstance: TDataSet;

    function  FieldByName(const AField: String): TKrakenProviderFields;
    function  ParamByName(const AParam: String): TKrakenProviderParam;

    function  Id: String; overload;
    function  Id(const Value: String): TKrakenProviderRequestHTTPQuery; overload;

    function  StartTransaction: Boolean; overload;
    function  StartTransaction(const Value: Boolean): TKrakenProviderRequestHTTPQuery; overload;

    function  Endpoint(const Value: String): TKrakenProviderRequestHTTPQuery;

    function  Response: String;

    function  SQL: TStringList;
    procedure Clear;

    function  SaveQuery(aPath: String; AFilename: String = ''): TKrakenProviderRequestHTTPQuery; overload;
    function  SaveQuery: String; overload;

    procedure Open; overload;
    procedure Open(ASQL: String); overload;
    procedure ExecSQL;

    function  Eof: Boolean;
    procedure Next;
  end;

implementation

{ TKrakenProviderRequestHTTPQuery }

constructor TKrakenProviderRequestHTTPQuery.Create(AOwner: TComponent);
begin
  Inherited Create(AOwner);

end;

destructor TKrakenProviderRequestHTTPQuery.Destroy;
begin

  inherited;
end;

function TKrakenProviderRequestHTTPQuery.HashGenerate: String;
begin
  Result := THashMD5.GetHashString( RequestPrepared );
end;

function TKrakenProviderRequestHTTPQuery.RequestCommandToString(ARequestCommand: TRequestCommand): String;
begin
  case ARequestCommand of
    rcExecSQL: Result := 'execsql';
    rcOpen   : Result := 'open';
  end;
end;

function TKrakenProviderRequestHTTPQuery.RequestPrepared: String;
var
  LResult : string;
begin
  LResult := '';
  LResult := Trim( SQL.GetText );
  LResult := StringReplace( LResult , #$D#$A , '' , [rfReplaceAll] );
  LResult := StringReplace( LResult , ''''   , '' , [rfReplaceAll] );
  LResult := StringReplace( LResult , '\r\n' , '' , [rfReplaceAll] );
  LResult := FParamByName.PrepareSQL(LResult);

  Result := LResult;
end;

procedure TKrakenProviderRequestHTTPQuery.Request(ARequestCommand: TRequestCommand; AStartTransaction: Boolean);
var
  LJSONObject : TJSONObject;
  LResponse   : IResponse;
  Teste: TJSONObject;
  Cu, Cu2, carai: String;
  LJsonArray: TJSONArray;
  I: INteger;
begin
  try
    try
      LJSONObject := TJsonObject.Create();
      LJSONObject.AddPair( TJsonPair.Create( 'sql'              , RequestPrepared                           ) );
      LJSONObject.AddPair( TJsonPair.Create( 'comando'          , RequestCommandToString( ARequestCommand ) ) );
      LJSONObject.AddPair( TJsonPair.Create( 'starttransaction' , BoolToStr( AStartTransaction )            ) );

      LResponse :=
      TRequest.New
        .BaseURL            ( FEndpoint                           )
        .Accept             ( 'application/json'                  )
        .AcceptEncoding     ( 'UTF-8'                             )
        .AddHeader          ( 'Content-Type', 'application/json'  )
        .ContentType        ( 'application/json'                  )
        .AddHeader          ( 'hash', HashGenerate                )
        .AddBody            ( LJSONObject, true                   )
        .Post;
    finally
      if not ( LResponse.StatusCode in [200,201] ) then
        DatabaseError((TJSONObject.ParseJSONValue(LResponse.Content) as TJSONArray).Items[0].GetValue<string>('erro'))
      else
      begin
        FPosItem      := 0;
        FJSONResponse := TJSONObject.ParseJSONValue(LResponse.Content) as TJSONArray;
      end;
    end;
  except
    raise;
  end;
end;

function TKrakenProviderRequestHTTPQuery.GetInstance: TDataSet;
begin
  Result := TDataSet(Self);
end;

function TKrakenProviderRequestHTTPQuery.Id(const Value: String): TKrakenProviderRequestHTTPQuery;
begin
  Result    := Self;
  FId       := Value;
end;

procedure TKrakenProviderRequestHTTPQuery.Next;
begin
  FPosItem := FPosItem + 1;
end;

function TKrakenProviderRequestHTTPQuery.Id: String;
begin
  Result    := FId;
end;

function TKrakenProviderRequestHTTPQuery.StartTransaction(const Value: Boolean): TKrakenProviderRequestHTTPQuery;
begin
  Result := Self;
  FStartTransaction := Value;
end;

function TKrakenProviderRequestHTTPQuery.StartTransaction: Boolean;
begin
  Result := FStartTransaction;
end;

function TKrakenProviderRequestHTTPQuery.Response: String;
begin
  //Result := FResponse;
end;

function TKrakenProviderRequestHTTPQuery.SQL: TStringList;
begin
  if not Assigned(FSQL) then
    FSQL := TStringList.Create;
  Result := FSQL;
end;

procedure TKrakenProviderRequestHTTPQuery.Clear;
begin
  SQL.Clear;
end;

function TKrakenProviderRequestHTTPQuery.SaveQuery(aPath, AFilename: String): TKrakenProviderRequestHTTPQuery;
var
  I               : Integer;
  LArqFile        : TextFile;
  LRecordCount    : String;
  LParams         : TStringList;
  LFilename       : String;
begin
{
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
}
end;

function TKrakenProviderRequestHTTPQuery.SaveQuery: String;
var
  I       : Integer;
  LParams : TStringList;
begin
{
  Result := '';
  LParams := TStringLIst.Create;

  for I := 0 to Pred( Params.Count ) do
    LParams.AddPair( '- ' + Params[I].Name, Params[I].Value );

  Result := Result + 'Params: ' + LParams.Text + sLineBreak;
  Result := Result + SQL.GetText;

  LParams.Free;
}
end;

procedure TKrakenProviderRequestHTTPQuery.Open;
begin
  Request(rcOpen, False);
end;

procedure TKrakenProviderRequestHTTPQuery.Open(ASQL: String);
begin
  SQL.Text := ASQL;
  Request(rcOpen, False);
end;

function TKrakenProviderRequestHTTPQuery.ParamByName(const AParam: String): TKrakenProviderParam;
begin
  if not Assigned(FParamByName) then
    FParamByName := TKrakenProviderParam.Create;
  FParamByName.Param(AParam);
  Result := FParamByName;
end;

function TKrakenProviderRequestHTTPQuery.Endpoint(const Value: String): TKrakenProviderRequestHTTPQuery;
begin
  Result := Self;
  FEndpoint := Value;
end;

function TKrakenProviderRequestHTTPQuery.Eof: Boolean;
begin
  Result := FPosItem = FJSONResponse.Count;
end;

procedure TKrakenProviderRequestHTTPQuery.ExecSQL;
begin
  Request(rcExecSQL, FStartTransaction);
end;

function TKrakenProviderRequestHTTPQuery.FieldByName(const AField: String): TKrakenProviderFields;
begin
  if not Assigned(FFieldByName) then
    FFieldByName := TKrakenProviderFields.Create( FJSONResponse );

  FFieldByName.Field(AField);
  FFieldByName.Pos(FPosItem);
  Result := FFieldByName;
end;

end.
