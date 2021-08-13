unit Kraken.Provider.Firedac.Query;

interface

uses
  System.SysUtils,
  System.Classes,

  FireDAC.Comp.Client,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Def,
  FireDAC.Stan.Async,
  FireDAC.Stan.Param,
  FireDAC.Stan.Error,
  FireDAC.Stan.Option;

type
  TKrakenProviderFiredacQuery = class(TFDQuery)
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  private
    FId        : String;
    FConnection: TFDConnection;
  public
    function  GetInstance: TFDQuery;

    function  Id(const Value: String): TKrakenProviderFiredacQuery; overload;
    function  Id: String; overload;

    function  SaveQuery(aPath: String; AFilename: String = ''): TKrakenProviderFiredacQuery; overload;
    function  SaveQuery: String; overload;

    procedure Open(ASQL: String); overload;
    procedure Open; overload;

    procedure Clear;
  end;

implementation

{ TKrakenProviderFiredacQuery }

constructor TKrakenProviderFiredacQuery.Create(AOwner: TComponent);
begin
  Inherited Create(AOwner);

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

function TKrakenProviderFiredacQuery.SaveQuery: String;
var
  I       : Integer;
  LParams : TStringList;
begin
  Result := '';
  {$IFDEF DEBUG}
  LParams := TStringLIst.Create;

  for I := 0 to Pred( Params.Count ) do
    LParams.AddPair( '- ' + Params[I].Name, Params[I].AsString );

  Result := Result + 'Params: ' + LParams.Text + sLineBreak;
  Result := Result + SQL.GetText;

  LParams.Free;
  {$ENDIF}
end;

function TKrakenProviderFiredacQuery.SaveQuery(aPath: String; AFilename: String = ''): TKrakenProviderFiredacQuery;
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
  {$ENDIF}
end;

procedure TKrakenProviderFiredacQuery.Open(ASQL: String);
begin
  GetInstance.SQL.Clear;
  GetInstance.SQL.Add(ASQL);
  GetInstance.Prepare;
  GetInstance.Active := True;
end;

procedure TKrakenProviderFiredacQuery.Open;
begin
  GetInstance.Prepare;
  GetInstance.Active := True;
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
