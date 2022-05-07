unit Kraken.Test.Register;

interface

uses
  DUnitX.TestFramework,
  Kraken.Test.Deserialize.Person,
  Kraken.Test.Serialize.Person;

procedure RegisterTestes;

implementation

procedure RegisterTestes;
begin
  TDUnitX.RegisterTestFixture(TKrakenTestDeserializePerson);
  TDUnitX.RegisterTestFixture(TKrakenTestSerializePerson);
end;

initialization
  RegisterTestes;

end.
