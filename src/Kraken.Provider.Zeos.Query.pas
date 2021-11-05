unit Kraken.Provider.Zeos.Query;

interface

uses
  System.Classes,
  System.SysUtils,

  Kraken.Log,

  ZDataset,
  ZConnection;

type
  TKrakenProviderZeosQuery = class(TZQuery)
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  private
    FOwner     : TComponent;
    FId        : String;
  public
    function  GetInstance: TZQuery;

    function  Id(const Value: String): TKrakenProviderZeosQuery; overload;
    function  Id: String; overload;

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

function TKrakenProviderZeosQuery.SaveQuery: String;
var
  I            : Integer;
  LParams      : TStringList;
  LRecordCount : String;
begin
  Result := '';

  LParams := TStringLIst.Create;

  for I := 0 to Pred( Params.Count ) do
    LParams.AddPair( '- ' + Params[I].Name, Params[I].AsString );

  try
    if Self.Active then
      LRecordCount := IntToStr( RecordCount )
    else
      LRecordCount := '-';
  except
    LRecordCount := '0';
  end;

  Result := Format(
                                                           + sLineBreak +
      '/*-----------------------------------------------'  + sLineBreak +
      'Data......: %s as %s '                              + sLineBreak +
      'Registros.: %s '                                    + sLineBreak +
      'Parametros: %s '                                    + sLineBreak +
      '/*-----------------------------------------------'  + sLineBreak +
                                                           + sLineBreak +
      '%s'
    ,
    [
      FormatDateTime( 'dd/mm/yyyy', Now ),
      FormatDateTime( 'hh:mm:ss  ', Now ),
      LRecordCount,
      LParams.Text,
      SQL.GetText
    ]
  );

  LParams.Free;
end;

procedure TKrakenProviderZeosQuery.Open(ASQL: String; ALog: Boolean = false);
begin
  if ALog then KrakenLOG.Trace( SaveQuery );

  try
    if not GetInstance.Prepared then
    GetInstance.Prepare;

    GetInstance.SQL.Clear;
    GetInstance.SQL.Add(ASQL);
    GetInstance.Active := True;
  except
    on e: Exception do
    begin
      KrakenLOG.Fatal( SaveQuery );

      TKrakenProviderZeos(FOwner).Rollback;

      raise;
    end;
  end;
end;

procedure TKrakenProviderZeosQuery.Open(ALog: Boolean = false);
begin
  if ALog then KrakenLOG.Trace( SaveQuery );

  try
    if not GetInstance.Prepared then
    GetInstance.Prepare;

    GetInstance.Active := True;
  except
    on e: Exception do
    begin
      KrakenLOG.Fatal( SaveQuery );

      TKrakenProviderZeos(FOwner).Rollback;

      raise;
    end;
  end;
end;

procedure TKrakenProviderZeosQuery.ExecSQL(ALog: Boolean = false);
begin
  if ALog then KrakenLOG.Trace( SaveQuery );

  try
    GetInstance.ExecSQL;
  except
    on e: Exception do
    begin
      KrakenLOG.Fatal( SaveQuery );

		  TKrakenProviderZeos(FOwner).Rollback;

      raise;
    end;
  end;
end;

procedure TKrakenProviderZeosQuery.Clear;
begin
  SQL.Clear;
  ClearBuffers;
end;

end.
