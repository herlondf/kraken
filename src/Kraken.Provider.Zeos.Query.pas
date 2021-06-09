unit Kraken.Provider.Zeos.Query;

interface

uses
  //Data.DB,
  System.Classes,
  System.SysUtils,

  ZDataset,
  ZConnection;

type
  TKrakenProviderZeosQuery = class(TZQuery)
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  private
    FConnection: TZConnection;
  public
    function  GetInstance: TZQuery;
    procedure Open(ASQL: String); overload;
    procedure Open; overload;

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

procedure TKrakenProviderZeosQuery.Open(ASQL: String);
begin
  GetInstance.SQL.Clear;
  GetInstance.SQL.Add(ASQL);
  GetInstance.Active := True;
end;

procedure TKrakenProviderZeosQuery.Open;
begin
  GetInstance.Prepare;
  GetInstance.Active := True;
end;

end.
