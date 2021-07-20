unit Kraken.Provider.Zeos.Query;

interface

uses
  System.Classes,
  System.SysUtils,

  ZDataset,
  ZConnection;

type
  TKrakenProviderZeosQuery = class(TZQuery)
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  private
    FId        : String;
  public
    function  GetInstance: TZQuery;

    function  Id(const Value: String): TKrakenProviderZeosQuery; overload;
    function  Id: String; overload;

    function  StartTransaction: Boolean; overload;
    function  StartTransaction(const Value: Boolean): TKrakenProviderZeosQuery; overload;

    function  SaveQuery(aPath: String; AFilename: String = ''): TKrakenProviderZeosQuery; overload;
    function  SaveQuery: String; overload;

    procedure Open(ASQL: String); overload;
    procedure Open; overload;

    procedure ExecSQL;

    procedure Clear;
  end;

implementation

{ TKrakenProviderZeosQuery }

constructor TKrakenProviderZeosQuery.Create(AOwner: TComponent);
begin
  Inherited Create(AOwner);

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

function TKrakenProviderZeosQuery.StartTransaction(const Value: Boolean): TKrakenProviderZeosQuery;
begin
  Result := Self;
end;

function TKrakenProviderZeosQuery.StartTransaction: Boolean;
begin

end;

function TKrakenProviderZeosQuery.SaveQuery(aPath: String; AFilename: String = ''): TKrakenProviderZeosQuery;
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

procedure TKrakenProviderZeosQuery.Open(ASQL: String);
begin
  try
    if not GetInstance.Prepared then
    GetInstance.Prepare;

    GetInstance.SQL.Clear;
    GetInstance.SQL.Add(ASQL);
    GetInstance.Active := True;
  except
    raise;
  end;
end;

procedure TKrakenProviderZeosQuery.Open;
begin
  try
    if not GetInstance.Prepared then
    GetInstance.Prepare;

    GetInstance.Active := True;
  except
    raise;
  end;
end;

procedure TKrakenProviderZeosQuery.ExecSQL;
begin
  try
    GetInstance.ExecSQL;
  except
    raise;
  end;
end;

procedure TKrakenProviderZeosQuery.Clear;
begin
  SQL.Clear;
  ClearBuffers;
end;

end.
