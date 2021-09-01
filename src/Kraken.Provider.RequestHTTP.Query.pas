unit Kraken.Provider.RequestHTTP.Query;

interface

uses
  FireDAC.Comp.Client,

  System.Classes,
  System.SysUtils,
  System.Hash,

  RESTRequest4D,
  REST.Types,
  JsonParser.Helper,
  System.JSON,

  Kraken.Types,
  Kraken.Provider.Fields,
  Kraken.Provider.Params,

  Dataset.Serialize,
  DataSet.Serialize.Config, Biblioteca,
  System.NetEncoding;

type
  TRequestCommand = (rcExecSQL, rcOpen);

  TKrakenProviderRequestHTTPQuery = class
    constructor Create;
    destructor Destroy; override;
  private
    FId               : String;
    FSQL              : TStringList;
    FEndpoint         : String;
    FStartTransaction : Boolean;
    FPosItem          : Integer;
    FDataset          : TFDMemTable;
    FSQLAUX           : string;

    FFields           : TKrakenProviderFields;
    FParams           : TKrakenProviderParams;

    ///<summary> Retorna o tipo de operacao (open, execsql) </summary>
    function  RequestCommandToString(ARequestCommand: TRequestCommand): String;

    ///<summary> Funcao de geracao do Hash do sql </summary>
    function  HashGenerate: String;

    ///<summary> Retira caracter especiais e quebras de linha do sql </summary>
    function  RequestPrepared: String; overload;
    function  RequestPrepared(aSQL: String): String; overload;

    ///<summary> Realiza o request no endpoint informado </summary>
    procedure Request(ARequestCommand: TRequestCommand; AStartTransaction: Boolean);

    ///<summary> Faz o destroy do JSON atual e recebe um novo, para evitar memory leak </summary>
    //function JSONResponse(const AJSONArray: TJSONArray): TKrakenProviderRequestHTTPQuery;

    function GetSQLText: String;
    procedure SetSQLText(const Value: String);
  public
    ///<summary> Recebe a instancia final da classe </summary>
    function  GetInstance: TKrakenProviderRequestHTTPQuery;

    ///<summary> Implementa a sobrescrita da rotina fieldbyname </summary>
    function  FieldByName(const AField: String): TKrakenProviderFields;

    ///<summary> Implementa a sobrescrita da rotina parambyname </summary>
    function  ParamByName(const AParam: String): TKrakenProviderParams;

    ///<summary> Id unica da instancia, usada para localizacao da mesma na lista </summary>
    function  Id: String; overload;
    function  Id(const Value: String): TKrakenProviderRequestHTTPQuery; overload;

    ///<summary> Define uma transaction para ser enviada a API </summary>
    function  StartTransaction: Boolean; overload;
    function  StartTransaction(const Value: Boolean): TKrakenProviderRequestHTTPQuery; overload;

    ///<summary> Define o endpoint de requisicao </summary>
    function  Endpoint(const Value: String): TKrakenProviderRequestHTTPQuery;

    ///<summary> Retorna a lista de string da SQL </summary>
    function  SQL: TKrakenProviderRequestHTTPQuery;

    ///<summary> Metodos de sobrescrita do SQL </summary>
    function  Add(const Value: string): TKrakenProviderRequestHTTPQuery;
    function  Append(const Value: string): TKrakenProviderRequestHTTPQuery;
    function  GetText: String;
    property  Text : String read GetSQLText write SetSQLText;

    ///<summary> Limpa a lista string de SQL </summary>
    procedure Clear;

    ///<summary> Salva a query em um local informado </summary>
    function  SaveQuery(aPath: String; AFilename: String = ''): TKrakenProviderRequestHTTPQuery; overload;

    ///<summary> Retorna a query em string </summary>
    function  SaveQuery: String; overload;

    ///<summary> Efetua a requisicao de consulta no endpoint informado </summary>
    procedure Open; overload;

    ///<summary> Efetura a requisicao de consulta por uma query por parametro no endpoint informado </summary>
    procedure Open(ASQL: String); overload;

    ///<summary> Efetura a requisicao de insercao/delete no endpoint informado </summary>
    procedure ExecSQL;

    ///<summary> Retorna a lista de parametros informados </summary>
    function  Params: TKrakenParams;

    ///<summary> Compara a posicao atual na lista com o totalizador de objetos do JSON </summary>
    function  Eof: Boolean;

    ///<summary> Avanca a posicao da lista de objetos do JSON </summary>
    procedure Next;

    ///<summary> Limpa todos os registros </summary>
    procedure Close;

    ///<summary> Quantidade de registros do JSON </summary>
    function RecordCount: Integer;

    ///<summary> Retorna para a primeira posicao da lista  </summary>
    procedure First;

    ///<summary> Vai para a ultima posicao da lista </summary>
    procedure Last;

    ///<summary> Linhas afetadas </summary>
    function RowsAffected: Integer;

    ///<summary> Retorna a existencia de um registro </summary>
    function Locate(const AKeyField: String; const AKeyValue: Variant; AOptions: TLocateOptions): Boolean;

    ///<summary> Retorna a existencia de registros </summary>
    function IsEmpty: Boolean;

    ///<summary> Finalizar uma transação, e armazenar as informações no DB </summary>
    procedure commit;
  end;

implementation

{ TKrakenProviderRequestHTTPQuery }

constructor TKrakenProviderRequestHTTPQuery.Create;
begin
  FFields  := TKrakenProviderFields.Create;
  FParams  := TKrakenProviderParams.Create;
  FSQL     := TStringList.Create;
  //FDataset := TFDMemTable.Create(nil);

  TDataSetSerializeConfig.GetInstance.CaseNameDefinition := cndUpper;
end;

destructor TKrakenProviderRequestHTTPQuery.Destroy;
begin
  FreeAndNil(FFields);
  FreeAndNil(FParams);

  if Assigned(FSQL) then
  FreeAndNil(FSQL);

  FreeAndNil(FDataset);

  inherited;
end;

function TKrakenProviderRequestHTTPQuery.HashGenerate: String;
begin
  Result := THashMD5.GetHashString( Trim( RequestPrepared ) );
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
  LResult := Trim( FSQL.Text );
  //LResult := StringReplace( LResult , #$D#$A , ' ' , [rfReplaceAll] );
  //LResult := StringReplace( LResult , '\r\n' , ''  , [rfReplaceAll] );

  LResult := FParams.ParseSQL(LResult);


  //LResult := StringReplace( LResult , #$D#$A , ' ' , [rfReplaceAll] );
  //LResult := StringReplace( LResult , '\r\n' , ''  , [rfReplaceAll] );

  Result := TNetEncoding.Base64.Encode( LResult );
end;

function TKrakenProviderRequestHTTPQuery.RequestPrepared(aSQL: String): String;
var
  LResult : string;
begin
  LResult := '';
  LResult := Trim( aSQL );
  //LResult := StringReplace( LResult , #$D#$A , ' ' , [rfReplaceAll] );
  //LResult := StringReplace( LResult , '\r\n' , ''  , [rfReplaceAll] );

  LResult := FParams.ParseSQL(LResult);


  //LResult := StringReplace( LResult , #$D#$A , ' ' , [rfReplaceAll] );
  //LResult := StringReplace( LResult , '\r\n' , ''  , [rfReplaceAll] );

  LResult := LResult + '; ';

  Result := LResult;
end;

function TKrakenProviderRequestHTTPQuery.RowsAffected: Integer;
begin
  Result := 0;
end;

procedure TKrakenProviderRequestHTTPQuery.Request(ARequestCommand: TRequestCommand; AStartTransaction: Boolean);
var
  LJSONObject : TJSONObject;
  LResponse   : IResponse;
  Teste: String;
begin
  try
    try
      LJSONObject := TJSONObject.Create;
      LJSONObject.AddPair( TJsonPair.Create( 'sql'              , RequestPrepared                           ) );
      LJSONObject.AddPair( TJsonPair.Create( 'comando'          , RequestCommandToString( ARequestCommand ) ) );
      LJSONObject.AddPair( TJsonPair.Create( 'starttransaction' , BoolToStr( AStartTransaction )            ) );

      {$IFDEF DEBUG}
      SaveQuery( ExtractFilePath( ParamStr(0) ) + 'LOG\eVendas\', '');
      {$ENDIF}

      LResponse :=
      TRequest.New
        .BaseURL            ( FEndpoint                           )
        .Accept             ( 'application/json'                  )
        .AcceptEncoding     ( 'UTF-8'                             )
        .AddHeader          ( 'Content-Type', 'application/json'  )
        .ContentType        ( 'application/json'                  )
        .AddHeader          ( 'hash', HashGenerate                )
        .AddBody            ( LJSONObject, False                  )
        .Post;
    finally
      FPosItem := 0;
      FreeAndNil(LJSONObject);

      if not ( LResponse.StatusCode in [200,201] ) then
      begin
        try


        except
          raise Exception.Create( ( TJSONObject.ParseJSONValue(LResponse.Content) as TJSONArray).Items[0].GetValue<string>('erro') );
        end;
      end
      else
      begin
        TEncoding.ASCII.GetBytes( LResponse.Content );

        FDataset := TFDMemTable.Create(nil);

        FDataset.LoadFromJSON(TJSONObject.ParseJSONValue(LResponse.Content) as TJSONArray);

       // SalvarArquivo('', 'json.json', LResponse.Content);

      end;
    end;
  except
    raise;
  end;
end;

function TKrakenProviderRequestHTTPQuery.FieldByName(const AField: String): TKrakenProviderFields;
begin
  FFields.ResponseJSON( FDataset      );
  FFields.Field       ( AField        );
  FFields.Pos         ( FPosItem      );

  Result := FFields;
end;

function TKrakenProviderRequestHTTPQuery.ParamByName(const AParam: String): TKrakenProviderParams;
begin
  FParams.Param( AParam );

  Result := FParams;
end;

function TKrakenProviderRequestHTTPQuery.GetInstance: TKrakenProviderRequestHTTPQuery;
begin
  Result := Self;
end;

function TKrakenProviderRequestHTTPQuery.GetSQLText: String;
begin
  Result := FSQL.Text;
end;

function TKrakenProviderRequestHTTPQuery.Id(const Value: String): TKrakenProviderRequestHTTPQuery;
begin
  Result    := Self;
  FId       := Value;
end;
  {
function TKrakenProviderRequestHTTPQuery.JSONResponse(const AJSONArray: TJSONArray): TKrakenProviderRequestHTTPQuery;
begin
  Result := Self;

  if Assigned(FJSONResponse) then
  FreeAndNil(FJSONResponse);

  FJSONResponse := AJSONArray;
end;  }

function TKrakenProviderRequestHTTPQuery.Id: String;
begin
  Result := FId;
end;

function TKrakenProviderRequestHTTPQuery.StartTransaction(const Value: Boolean): TKrakenProviderRequestHTTPQuery;
begin
  Result := Self;
  FStartTransaction := Value;

  FSQLAUX := FSQL.GetText;
  FSQL.Clear;
end;

function TKrakenProviderRequestHTTPQuery.StartTransaction: Boolean;
begin
  Result := FStartTransaction;
end;

function TKrakenProviderRequestHTTPQuery.Endpoint(const Value: String): TKrakenProviderRequestHTTPQuery;
begin
  Result    := Self;
  FEndpoint := Value;
end;

function TKrakenProviderRequestHTTPQuery.SQL: TKrakenProviderRequestHTTPQuery;
begin
  Result := Self;
end;

function TKrakenProviderRequestHTTPQuery.Add(const Value: string): TKrakenProviderRequestHTTPQuery;
begin
  Result := Self;
  FSQL.Add(Value);
end;

function TKrakenProviderRequestHTTPQuery.Append(const Value: string): TKrakenProviderRequestHTTPQuery;
begin
  Result := Self;
  FSQL.Append(Value);
end;

function TKrakenProviderRequestHTTPQuery.GetText: String;
begin
  Result := FSQL.Text;
end;

procedure TKrakenProviderRequestHTTPQuery.Clear;
begin
  FSQL.Clear;
  if Assigned(FDataset) then
    FreeAndNil(FDataset);

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
  {$IFDEF DEBUG}
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
    LRecordCount := IntToStr( FDataset.RecordCount );
  except
    LRecordCount := '0';
  end;

  try
    Rewrite( LArqFile                                                            );
    WriteLN( LArqFile, '/*-----------------------------------------------'       );
    WriteLN( LArqFile, ''                                                        );
    WriteLN( LArqFile, 'Data..: ' + FormatDateTime( 'dd/mm/yyyy', Now ) + ' '
                                  + FormatDateTime( 'hh:mm:ss  ', Now )          );
    WriteLN( LArqFile, ''                                                        );
    WriteLN( LArqFile, 'Registros.: ' + LRecordCount                             );
    WriteLN( LArqFile, 'Parametros:'                                             );
    WriteLN( LArqFile, LParams.Text                                              );
    WriteLN( LArqFile, '-----------------------------------------------*/'       );
    WriteLN( LArqFile, ''                                                        );
    WriteLN( LArqFile, RequestPrepared                                                 );
  finally
    CloseFile( LArqFile );
    LParams.Free;
  end;
  {$ENDIF}
end;

function TKrakenProviderRequestHTTPQuery.SaveQuery: String;
begin
  result := '';
  {$IFDEF DEBUG}
  //Result := FParams.ParseSQL( FSQL.Text );
  {$ENDIF}
end;

procedure TKrakenProviderRequestHTTPQuery.SetSQLText(const Value: String);
begin
  FSQL.Text := Value;
end;

procedure TKrakenProviderRequestHTTPQuery.Open;
begin
  if FStartTransaction then
    FSQL.Add(FSQLAUX);

  Request(rcOpen, FStartTransaction);

  {Quando for dado um open, o comando precisa
   ser enviado, porém se o starttransaction
   estiver true, no comit vai ser enviado novamento}
  FStartTransaction := false;
end;

procedure TKrakenProviderRequestHTTPQuery.Open(ASQL: String);
begin
  FSQL.Text := ASQL;
  Request(rcOpen, FStartTransaction);

  {Quando for dado um open, o comando precisa
   ser enviado, porém se o starttransaction
   estiver true, no comit vai ser enviado novamento}
  FStartTransaction := false;
end;

function TKrakenProviderRequestHTTPQuery.Eof: Boolean;
begin
  Result := FPosItem = FDataset.RecordCount;
end;

procedure TKrakenProviderRequestHTTPQuery.ExecSQL;
begin
  if not StartTransaction then
  begin
    FStartTransaction := true;
    Request(rcExecSQL, FStartTransaction);
    FStartTransaction := false;
  end
  else
  begin
    FSQL.Add( RequestPrepared(FSQLAUX) );
    //FParams.Clear;
  end;
end;

function TKrakenProviderRequestHTTPQuery.Params: TKrakenParams;
begin
  Result := FParams.List;
end;

procedure TKrakenProviderRequestHTTPQuery.Next;
begin
  FPosItem := FPosItem + 1;
  FDataset.Next;
end;

procedure TKrakenProviderRequestHTTPQuery.Close;
begin

end;

procedure TKrakenProviderRequestHTTPQuery.commit;
begin
  if StartTransaction then
    Request(rcExecSQL, FStartTransaction);
  FSQLAUX := '';
end;

function TKrakenProviderRequestHTTPQuery.RecordCount: Integer;
begin
  Result := FDataset.RecordCount;
end;

procedure TKrakenProviderRequestHTTPQuery.First;
begin
  FPosItem := 0;
end;

procedure TKrakenProviderRequestHTTPQuery.Last;
begin
  FPosItem := FDataset.RecordCount;
end;


function TKrakenProviderRequestHTTPQuery.Locate(const AKeyField: String; const AKeyValue: Variant; AOptions: TLocateOptions): Boolean;
var
  I: Integer;
  LJSONObject: TJSONObject;
  LPartialKey: Boolean;
  LCaseInsensitive: Boolean;
begin
  LPartialKey      := loPartialKey in AOptions;
  LCaseInsensitive := loCaseInsensitive in AOptions;

  try
    FDataset.First;
    for I := 0 to Pred( FDataset.RecordCount ) do
    begin
      if LCaseInsensitive and (not LPartialKey) then
        Result := AnsiUpperCase( FDataset.Fields.FieldByName(AKeyField).AsString ) = AnsiUpperCase( AKeyValue )
      else
      if LPartialKey and (not LCaseInsensitive) then
        Result := Pos(AKeyValue, FDataset.Fields.FieldByName(AKeyField).AsString ) > 0
      else
      if LPartialKey and LCaseInsensitive then
        Result := Pos(AnsiUpperCase( AKeyValue ), AnsiUpperCase( FDataset.Fields.FieldByName(AKeyField).AsString ) ) > 0;

      if Result then
        Break;

      FDataset.Next
    end;
  except
    raise;
  end;

end;

function TKrakenProviderRequestHTTPQuery.IsEmpty: Boolean;
begin
  Result := FDataset.RecordCount = 0;
end;

end.
