program KrakenTest;

{$IFNDEF TESTINSIGHT}
{$APPTYPE CONSOLE}
{$ENDIF}{$STRONGLINKTYPES ON}
uses
  System.SysUtils,
  TestInsight.DUnitX,
  Kraken.Test.Register in 'Kraken.Test.Register.pas',
  Kraken.Test.Models in 'Kraken.Test.Models.pas',
  Kraken.Test.Deserialize.Person in 'Kraken.Test.Deserialize.Person.pas',
  Kraken.Test.Serialize.Person in 'Kraken.Test.Serialize.Person.pas',
  Kraken.Attributes in '..\Source\Kraken.Attributes.pas',
  Kraken.Base in '..\Source\Kraken.Base.pas',
  Kraken.Config in '..\Source\Kraken.Config.pas',
  Kraken.DateTime.Helper in '..\Source\Kraken.DateTime.Helper.pas',
  Kraken.Deserializer in '..\Source\Kraken.Deserializer.pas',
  Kraken.Helper in '..\Source\Kraken.Helper.pas',
  Kraken.Interfaces in '..\Source\Kraken.Interfaces.pas',
  Kraken.RTTI in '..\Source\Kraken.RTTI.pas',
  Kraken.Serializer in '..\Source\Kraken.Serializer.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  TestInsight.DUnitX.RunRegisteredTests;

end.
