unit Brave.Provider.Types;

interface

uses
  {$IF DEFINED(BRAVE_FIREDAC)}
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
  TBraveProviderTypes = class
    {$IF DEFINED(BRAVE_FIREDAC)}
    class procedure Postgres(AConnection: TFDConnection; out ADriver: TFDPhysDriverLink);
    class procedure Firebird(AConnection: TFDConnection; out ADriver: TFDPhysDriverLink);
    {$ELSE}
    class procedure Postgres(AConnection: TZConnection);
    class procedure Firebird(AConnection: TZConnection);
    {$ENDIF}
  end;

implementation

{ TBraveProviderTypes }

{$IF DEFINED(BRAVE_FIREDAC)}

class procedure TBraveProviderTypes.Postgres(AConnection: TFDConnection; out ADriver: TFDPhysDriverLink);
begin
  ADriver                := TFDPhysPgDriverLink.Create(nil);
  ADriver.Name           := 'PGDriver';
  ADriver.VendorLib      := 'libpq.dll';
  AConnection.DriverName := 'PG';
end;

class procedure TBraveProviderTypes.Firebird(AConnection: TFDConnection; out ADriver: TFDPhysDriverLink);
begin
  ADriver      := TFDPhysFBDriverLink.Create(nil);
  ADriver.Name := 'FBDriver';

  AConnection.DriverName := 'FB';
end;

{$ELSE}

class procedure TBraveProviderTypes.Postgres(AConnection: TZConnection);
begin
  AConnection.Protocol := 'postgresql-9';
  AConnection.Properties.Values['codepage'] := 'utf-8';
  AConnection.TransactIsolationLevel := tiReadUncommitted;
  AConnection.AutoCommit := False;
end;

class procedure TBraveProviderTypes.Firebird(AConnection: TZConnection);
begin
  AConnection.Protocol := 'firebird-2.5';
  AConnection.Properties.Values['codepage'] := 'utf-8';
  AConnection.TransactIsolationLevel := tiReadUncommitted;
  AConnection.AutoCommit := False;
end;

{$ENDIF}

end.
