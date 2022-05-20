unit KrakenJSON.Base;

interface

uses
  System.SysUtils;

type TKrakenBase = class(TInterfacedObject)

  protected
    FDateTimeFormat: String;

  public
    procedure DateTimeFormat(Value: String);

    constructor create; virtual;
    destructor  Destroy; override;
end;

implementation

{ TKrakenBase }

constructor TKrakenBase.create;
begin
  FDateTimeFormat := EmptyStr;
end;

procedure TKrakenBase.DateTimeFormat(Value: String);
begin
  FDateTimeFormat := Value;
end;

destructor TKrakenBase.Destroy;
begin

  inherited;
end;

end.
