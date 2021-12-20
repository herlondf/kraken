unit Kraken.Provider.Types;

interface

uses
  System.SysUtils,
  System.Classes,
  {$IF DEFINED(KRAKEN_FIREDAC)}
  FireDAC.Comp.Client,
  FireDAC.Phys.PGDef,
  FireDAC.Phys,
  FireDAC.Phys.PG,
  FireDAC.Phys.FBDef,
  FireDAC.Phys.IBBase,
  FireDAC.Phys.FB,
  FireDAC.Phys.Intf,
  FireDAC.Stan.ExprFuncs,
  FireDAC.Phys.SQLiteDef,
  FireDAC.Stan.Intf,
  FireDAC.Phys.SQLite;
  {$ELSE}
  ZDbcIntfs,
  ZConnection;
  {$ENDIF}

type
  TKrakenProviderTypes<T: class, constructor> = class
  private
    FReturn: T;
    FDriver: TComponent;

    FConnection: {$IF DEFINED(KRAKEN_FIREDAC)} TFDConnection; {$ELSE} TZConnection; {$ENDIF}

    destructor Destroy; override;
  public
    constructor Create(AOwner: T);

    function Postgres: T;
    function Firebird: T;
    function SQLite  : T;
  end;

implementation

{ TKrakenProviderTypes }

constructor TKrakenProviderTypes<T>.Create(AOwner: T);
begin
  FReturn     := AOwner;
  FConnection := {$IF DEFINED(KRAKEN_FIREDAC)} TFDConnection(AOwner); {$ELSE} TZConnection(AOwner); {$ENDIF}
end;

destructor TKrakenProviderTypes<T>.Destroy;
begin
  if Assigned(FDriver) then
    FreeAndNil(FDriver);

  inherited;
end;

function TKrakenProviderTypes<T>.Postgres: T;
begin
  {$IF DEFINED(KRAKEN_FIREDAC)}
  FDriver := TFDPhysPgDriverLink.Create(FConnection);
  TFDPhysPgDriverLink(FDriver).Name := 'PGDriver';
  TFDPhysPgDriverLink(FDriver).VendorLib := 'libpq.dll';
  FConnection.DriverName := 'PG';
  {$ELSE}
  FConnection.Protocol := 'postgresql-9';
  FConnection.Properties.Values['codepage'] := 'utf-8';
  FConnection.TransactIsolationLevel := tiReadUncommitted;
  FConnection.AutoCommit := False;
  {$ENDIF}

  Result := FReturn;
end;

function TKrakenProviderTypes<T>.Firebird: T;
begin
  {$IF DEFINED(KRAKEN_FIREDAC)}
  FDriver := TFDPhysFBDriverLink.Create(FConnection);
  TFDPhysFBDriverLink(FDriver).Name := 'FBDriver';
  FConnection.DriverName := 'FB';
  {$ELSE}
  FConnection.Protocol := 'firebird-2.5';
  FConnection.Properties.Values['codepage'] := 'utf-8';
  FConnection.TransactIsolationLevel := tiReadUncommitted;
  FConnection.AutoCommit := False;
  {$ENDIF}

  Result := FReturn;
end;

function TKrakenProviderTypes<T>.SQLite: T;
begin
  {$IF DEFINED(KRAKEN_FIREDAC)}
  FDriver := TFDPhysSQLiteDriverLink.Create(FConnection);
  TFDPhysSQLiteDriverLink(FDriver).Name := 'SQLite';
  FConnection.DriverName := 'SQLite';
  {$ELSE}
  raise Exception.Create('Driver not implemented.');
  {$ENDIF}

  Result := FReturn;
end;

end.
