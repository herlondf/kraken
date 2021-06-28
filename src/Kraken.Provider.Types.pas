unit Kraken.Provider.Types;

interface

uses
  {$IF DEFINED(KRAKEN_FIREDAC)}
  FireDAC.Comp.Client,
  FireDAC.Phys.PGDef,
  FireDAC.Phys,
  FireDAC.Phys.PG,
  FireDAC.Phys.FBDef,
  FireDAC.Phys.IBBase,
  FireDAC.Phys.FB,
  FireDAC.Phys.Intf;
  {$ELSE}
  ZDbcIntfs,
  ZConnection;
  {$ENDIF}

type
  TKrakenProviderTypes = class
    {$IF DEFINED(KRAKEN_FIREDAC)}
    class procedure Postgres(AConnection: TFDConnection; out ADriver: TFDPhysDriverLink);
    class procedure Firebird(AConnection: TFDConnection; out ADriver: TFDPhysDriverLink);
    {$ELSE}
    class procedure Postgres(AConnection: TZConnection);
    class procedure Firebird(AConnection: TZConnection);
    {$ENDIF}
  end;

implementation

{ TKrakenProviderTypes }

{$IF DEFINED(KRAKEN_FIREDAC)}

class procedure TKrakenProviderTypes.Postgres(AConnection: TFDConnection; out ADriver: TFDPhysDriverLink);
begin
  ADriver                := TFDPhysPgDriverLink.Create(AConnection);
  ADriver.Name           := 'PGDriver';
  ADriver.VendorLib      := 'libpq.dll';
  AConnection.DriverName := 'PG';
end;

class procedure TKrakenProviderTypes.Firebird(AConnection: TFDConnection; out ADriver: TFDPhysDriverLink);
begin
  ADriver      := TFDPhysFBDriverLink.Create(AConnection);
  ADriver.Name := 'FBDriver';

  AConnection.DriverName := 'FB';
end;

{$ELSE}

class procedure TKrakenProviderTypes.Postgres(AConnection: TZConnection);
begin
  AConnection.Protocol := 'postgresql-9';
  AConnection.Properties.Values['codepage'] := 'utf-8';
  AConnection.TransactIsolationLevel := tiReadUncommitted;
  AConnection.AutoCommit := False;
end;

class procedure TKrakenProviderTypes.Firebird(AConnection: TZConnection);
begin
  AConnection.Protocol := 'firebird-2.5';
  AConnection.Properties.Values['codepage'] := 'utf-8';
  AConnection.TransactIsolationLevel := tiReadUncommitted;
  AConnection.AutoCommit := False;
end;

{$ENDIF}

end.
