unit Kraken.Provider.Firedac.Query;

interface

uses
  System.SysUtils,
  System.Classes,

  Kraken.Consts,

  FireDAC.Comp.Client,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Def,
  FireDAC.Stan.Async,
  FireDAC.Stan.Param,
  FireDAC.Stan.Error,
  FireDAC.Stan.Option;

type
  TKrakenQueryNotify = reference to procedure(const Value: String);

  TKrakenProviderFiredacQuery = class(TFDQuery)
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  private
    FOwner     : TComponent;
    FId        : String;

    fProcessStart : TKrakenQueryNotify;
    fProcessFinish: TKrakenQueryNotify;
    fProcessError : TKrakenQueryNotify;

    procedure HandleException(pException: Exception; pOperation: TKrakenQueryOperation);
  public
    function  GetInstance: TFDQuery;

    function  Id(const Value: String): TKrakenProviderFiredacQuery; overload;
    function  Id: String; overload;

    function OnProcessStart (pProc: TKrakenQueryNotify): TKrakenProviderFiredacQuery;
    function OnProcessFinish(pProc: TKrakenQueryNotify): TKrakenProviderFiredacQuery;
    function OnProcessError (pProc: TKrakenQueryNotify): TKrakenProviderFiredacQuery;

    function  SaveQuery: String;

    procedure Open(ASQL: String; ALog: Boolean = false); overload;
    procedure Open(ALog: Boolean = false); overload;
		
		procedure ExecSQL(ALog: Boolean = false);

    procedure Clear;
  end;

implementation

uses
  Kraken.Provider.Firedac;

{ TKrakenProviderFiredacQuery }

constructor TKrakenProviderFiredacQuery.Create(AOwner: TComponent);
begin
  Inherited Create(AOwner);
  FOwner := AOwner;
  GetInstance.Connection := TFDConnection(AOwner);
end;

destructor TKrakenProviderFiredacQuery.Destroy;
begin

  inherited;
end;

function TKrakenProviderFiredacQuery.GetInstance: TFDQuery;
begin
  Result := TFDQuery(Self);
end;

function TKrakenProviderFiredacQuery.Id: String;
begin
  Result    := FId;
end;

function TKrakenProviderFiredacQuery.Id(const Value: String): TKrakenProviderFiredacQuery;
begin
  Result    := Self;
  FId       := Value;
  Self.Name := 'FDQuery' + FId;
end;


function TKrakenProviderFiredacQuery.OnProcessStart (pProc: TKrakenQueryNotify): TKrakenProviderFiredacQuery;
begin
  Result := Self;
  fProcessStart := pProc;
end;

function TKrakenProviderFiredacQuery.OnProcessFinish(pProc: TKrakenQueryNotify): TKrakenProviderFiredacQuery;
begin
  Result := Self;
  fProcessFinish := pProc;
end;

function TKrakenProviderFiredacQuery.OnProcessError (pProc: TKrakenQueryNotify): TKrakenProviderFiredacQuery;
begin
  Result := Self;
  fProcessError := pProc;
end;

procedure TKrakenProviderFiredacQuery.HandleException(pException: Exception; pOperation: TKrakenQueryOperation);

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

//  TKrakenLOG.Fatal( SaveQuery );

  if pException is EFDDBEngineException then
  begin
//    TKrakenLOG.Fatal( IntToStr( EFDDBEngineException(pException).ErrorCode ) );

    if EFDDBEngineException(pException).ErrorCode = 7 then
      {Duplicidade registro}
    else        
    {Deadlock-PostgreSQL}
    if EFDDBEngineException(pException).FDCode = 1500 then
    begin
      fProcessError( 'Deadlock detectado, tentando novamente.' );
      TKrakenProviderFiredac(FOwner).Rollback;
      TKrakenProviderFiredac(FOwner).StartTransaction;
      RunOperation(pOperation);
      Exit;
    end;
  end
  else
  if pException is EFDDBArrayExecuteError then
  begin
//    TKrakenLOG.Fatal( IntToStr( EFDDBArrayExecuteError(pException).FDCode ) );
    TKrakenProviderFiredac(FOwner).Rollback;
  end
  else
    TKrakenProviderFiredac(FOwner).Rollback;
end;

function TKrakenProviderFiredacQuery.SaveQuery: String;
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
    LParams.AddPair( '- ' + Params[I].Name, Params[I].AsString );

    StringReplace( LQuery, ':'+Params[I].Name, Params[I].AsString + '{' + Params[I].Name + '}', [rfReplaceAll, rfIgnoreCase] );
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

procedure TKrakenProviderFiredacQuery.Open(ASQL: String; ALog: Boolean = false);
begin
//  if ALog then TKrakenLOG.Trace( SaveQuery );

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

procedure TKrakenProviderFiredacQuery.Open(ALog: Boolean = false);
begin
//  if ALog then TKrakenLOG.Trace( SaveQuery );

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

procedure TKrakenProviderFiredacQuery.ExecSQL(ALog: Boolean = false);
begin
//  if ALog then TKrakenLOG.Trace( SaveQuery );

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

procedure TKrakenProviderFiredacQuery.Clear;
begin
  SQL.Clear;
  ClearColumnMap;
  ClearBlobs;
  ClearDetails;
  ClearBuffers;
end;

end.
