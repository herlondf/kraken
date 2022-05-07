unit Kraken.Migrate;

interface

uses
  System.Classes,
  System.SysUtils,
  Kraken.Migrate.PG;

type
  TKrakenMigrate = class
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
  private
    FMigratePG: TKrakenMigratePG;
  public
    function PG: TKrakenMigratePG;
  end;


implementation

{ TKrakenMigrate }

constructor TKrakenMigrate.Create(AOwner: TComponent);
begin

end;

destructor TKrakenMigrate.Destroy;
begin
  if Assigned(FMigratePG) then
    FreeAndNil(FMigratePG);

  inherited;
end;

function TKrakenMigrate.PG: TKrakenMigratePG;
begin
  if not assigned(FMigratePG) then
    FMigratePG := TKrakenMigratePG.Create;
  Result := FMigratePG;
end;

end.
