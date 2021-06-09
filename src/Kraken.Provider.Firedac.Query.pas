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
    FConnection: TFDConnection;
  public
    function  GetInstance: TFDQuery;
    procedure Open(ASQL: String); overload;
    procedure Open; overload;
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

procedure TKrakenProviderFiredacQuery.Open(ASQL: String);
begin
  GetInstance.SQL.Clear;
  GetInstance.SQL.Add(ASQL);
  GetInstance.Active := True;
end;

procedure TKrakenProviderFiredacQuery.Open;
begin
  GetInstance.Prepare;
  GetInstance.Active := True;
end;

end.
