unit JSONParser.Base;

interface

uses
  System.SysUtils;

type TJSONParserBase = class(TInterfacedObject)

  protected
    FDateTimeFormat: String;

  public
    procedure DateTimeFormat(Value: String);

    constructor create; virtual;
    destructor  Destroy; override;
end;

implementation

{ TJSONParserBase }

constructor TJSONParserBase.create;
begin
  FDateTimeFormat := EmptyStr;
end;

procedure TJSONParserBase.DateTimeFormat(Value: String);
begin
  FDateTimeFormat := Value;
end;

destructor TJSONParserBase.Destroy;
begin

  inherited;
end;

end.
