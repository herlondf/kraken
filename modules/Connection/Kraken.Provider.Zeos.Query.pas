unit Kraken.Provider.Zeos.Query;

interface

uses
  System.Classes,
  System.SysUtils,

  Kraken.Log,
  Kraken.Consts,

  Data.DB,
  ZDataset,
  ZClasses,
  ZConnection;

type
  TKrakenQueryNotify = reference to procedure(const Value: String);

  TKrakenProviderZeosQuery = class(TZQuery)
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  private
    fOwner        : TComponent;
    fId           : String;

    fProcessStart : TKrakenQueryNotify;
    fProcessFinish: TKrakenQueryNotify;
    fProcessError : TKrakenQueryNotify;

    procedure HandleException(pException: Exception; pOperation: TKrakenQueryOperation);
  public
    function  GetInstance: TZQuery;

    function  Id(const Value: String): TKrakenProviderZeosQuery; overload;
    function  Id: String; overload;

    function OnProcessStart (pProc: TKrakenQueryNotify): TKrakenProviderZeosQuery;
    function OnProcessFinish(pProc: TKrakenQueryNotify): TKrakenProviderZeosQuery;
    function OnProcessError (pProc: TKrakenQueryNotify): TKrakenProviderZeosQuery;

    function  SaveQuery: String;

    procedure Open(ASQL: String; ALog: Boolean = false); overload;
    procedure Open(ALog: Boolean = false); overload;

    procedure ExecSQL(ALog: Boolean = false);

    procedure Clear;
  end;

implementation

uses
  Kraken.Provider.Zeos;

{ TKrakenProviderZeosQuery }

constructor TKrakenProviderZeosQuery.Create(AOwner: TComponent);
begin
  Inherited Create(AOwner);
  FOwner := AOwner;
  GetInstance.Connection := TZConnection(AOwner);
end;

destructor TKrakenProviderZeosQuery.Destroy;
begin

  inherited;
end;

function TKrakenProviderZeosQuery.GetInstance: TZQuery;
begin
  Result := TZQuery(Self);
end;
function TKrakenProviderZeosQuery.Id: String;
begin
  Result    := FId;
end;

function TKrakenProviderZeosQuery.Id(const Value: String): TKrakenProviderZeosQuery;
begin
  Result    := Self;
  FId       := Value;
  Self.Name := 'ZQuery' + FId;
end;

function TKrakenProviderZeosQuery.OnProcessStart(pProc: TKrakenQueryNotify): TKrakenProviderZeosQuery;
begin
  Result := Self;
  fProcessStart := pProc;
end;

function TKrakenProviderZeosQuery.OnProcessFinish(pProc: TKrakenQueryNotify): TKrakenProviderZeosQuery;
begin
  Result := Self;
  fProcessFinish := pProc;
end;

function TKrakenProviderZeosQuery.OnProcessError(pProc: TKrakenQueryNotify): TKrakenProviderZeosQuery;
begin
  Result := Self;
  fProcessError := pProc;
end;


procedure TKrakenProviderZeosQuery.HandleException(pException: Exception; pOperation: TKrakenQueryOperation);

  procedure RunOperation(pOperation: TKrakenQueryOperation);
  begin
    case pOperation of
      qoExecSQL    : ExecSQL;
      qoOpen       : Open;
    end;
  end;

begin
  {https://hackage.haskell.org/package/postgresql-error-codes-1.0.1/docs/PostgreSQL-ErrorCodes.html}

  if Assigned(fProcessError) then
    fProcessError( pException.Message );

  KrakenLOG.Fatal( pException.Message  );
  KrakenLOG.Fatal( SaveQuery );

  if pException is EZSQLWarning then
  begin
    fProcessError( 'EZSQLWarning' );
    TKrakenProviderZeos(FOwner).Rollback;
    Exit;
  end;

  if pException is EZSQLThrowable then
  begin
    KrakenLOG.Fatal( EZSQLThrowable(pException).StatusCode );

    {Deadlock-PostgreSQL}
    if EZSQLThrowable(pException).StatusCode = '40P01' then
    begin
      fProcessError( 'Deadlock detectado, tentando novamente.' );
      TKrakenProviderZeos(FOwner).Rollback;
      TKrakenProviderZeos(FOwner).StartTransaction;
      RunOperation(pOperation);
      Exit;
    end;
  end;

  if pException is EZSQLException then
  begin
    fProcessError( 'EZSQLException' );
    KrakenLOG.Fatal( EZSQLException(pException).StatusCode );
    TKrakenProviderZeos(FOwner).Rollback;
    Exit;
  end;

  if pException is EZSQLConnectionLost then
  begin
    fProcessError( 'EZSQLConnectionLost' );
    KrakenLOG.Fatal( EZSQLConnectionLost(pException).StatusCode );
    TKrakenProviderZeos(FOwner).Rollback;
  end;
end;


function TKrakenProviderZeosQuery.SaveQuery: String;
var
  I            : Integer;
  LParams      : TStringList;
  LRecordCount : String;
  LQuery       : string;
begin
  Result := '';

  LParams := TStringLIst.Create;
  LQuery  := SQL.GetText;

  for I := 0 to Pred( Params.Count ) do
  begin
    if Params[I].IsNull then
      Params[I].Clear
    else
    if Params[I].DataType in [ftString, ftDate, ftTime, ftDateTime, ftWideString] then
      LQuery := StringReplace( LQuery, ':'+Params[I].Name, '''' + Params[I].AsString, [rfIgnoreCase] )
    else
      LQuery := StringReplace( LQuery, ':'+Params[I].Name, Params[I].AsString, [rfIgnoreCase] );

    LParams.AddPair( '- ' + Params[I].Name, Params[I].AsString );
  end;

  try
    if Self.Active then
      LRecordCount := IntToStr( RecordCount )
    else
      LRecordCount := '-';
  except
    LRecordCount := '0';
  end;

  Result := Format(
      ''                                                   + sLineBreak +
      '/*-----------------------------------------------'  + sLineBreak +
      'Data......: %s as %s '                              + sLineBreak +
      'Registros.: %s '                                    + sLineBreak +
      'Parametros: %s '                                    + sLineBreak +
      '/*-----------------------------------------------'  + sLineBreak +
      ''                                                   + sLineBreak +
      '%s'
    ,
    [
      FormatDateTime( 'dd/mm/yyyy', Now ),
      FormatDateTime( 'hh:mm:ss  ', Now ),
      LRecordCount,
      LParams.Text,
      LQuery
    ]
  );

  LParams.Free;
end;

procedure TKrakenProviderZeosQuery.Open(ASQL: String; ALog: Boolean = false);
begin
  if ALog then KrakenLOG.Trace( SaveQuery );

  try
    if Assigned(fProcessStart) then
      fProcessStart('Executando query...');

    try
      if not GetInstance.Prepared then
      GetInstance.Prepare;

      GetInstance.SQL.Clear;
      GetInstance.SQL.Add(ASQL);
      GetInstance.Active := True;
    finally
      if Assigned(fProcessFinish) then
        fProcessFinish('Query executada com sucesso!');
    end;
  except
    on E: Exception do HandleException(E, qoExecSQL);
  end;
end;

procedure TKrakenProviderZeosQuery.Open(ALog: Boolean = false);
begin
  if ALog then KrakenLOG.Trace( SaveQuery );

  try
    if Assigned(fProcessStart) then
      fProcessStart('Executando query...');

    try
      if not GetInstance.Prepared then
        GetInstance.Prepare;

      GetInstance.Active := True;
    finally
      if Assigned(fProcessFinish) then
        fProcessFinish('Query executada com sucesso!');
    end;
  except
    on E: Exception do HandleException(E, qoExecSQL);
  end;
end;

procedure TKrakenProviderZeosQuery.ExecSQL(ALog: Boolean = false);
begin
  if ALog then KrakenLOG.Trace( SaveQuery );

  try
    if Assigned(fProcessStart) then
      fProcessStart('Executando query...');

    try
      GetInstance.ExecSQL;
    finally
      if Assigned(fProcessFinish) then
        fProcessFinish('Query executada com sucesso!');
    end;
  except
    on E: Exception do HandleException(E, qoExecSQL);
  end;
end;

procedure TKrakenProviderZeosQuery.Clear;
begin
  SQL.Clear;
  ClearBuffers;
end;

end.
