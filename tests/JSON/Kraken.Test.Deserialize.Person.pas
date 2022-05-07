unit Kraken.Test.Deserialize.Person;

interface

uses
  DUnitX.TestFramework,
  Kraken.Test.Models,
  Kraken.Interfaces,
  Kraken.Deserializer,
  Kraken.Serializer,
  Kraken.Helper,
  System.JSON,
  System.SysUtils;

type TKrakenTestDeserializePerson = class

  private
    FPerson      : TPerson;
    FAuxPerson   : TPerson;
    FUpperPerson : TUpperPerson;
    FDeserialize : IKrakenDeserializer<TPerson>;
    FSerialize   : IKrakenSerializer<TPerson>;
    FJSONObject  : TJSONObject;

    function GetJsonObject(APerson: TPerson): TJSONObject;
  public
    [Setup]    procedure Setup;
    [TearDown] procedure TearDown;

    [Test] procedure TestStringName;
    [Test] procedure TestStringEmpty;
    [Test] procedure TestStringWithAccent;
    [Test] procedure TestStringWithBar;
    [Test] procedure TestStringWithBackslash;
    [Test] procedure TestStringWithDoubleQuotes;

    [Test] procedure TestIntegerPositive;
    [Test] procedure TestIntegerEmpty;
    [Test] procedure TestIntegerNegative;

    [Test] procedure TestFloatPositive;
    [Test] procedure TestFloatNegative;
    [Test] procedure TestFloatZero;
    [Test] procedure TestFloatPositiveWithDecimal;
    [Test] procedure TestFloatNegativeWithDecimal;

    [Test] procedure TestDateEmpty;
    [Test] procedure TestDateFill;

    [Test] procedure TestBooleanFalse;
    [Test] procedure TestBooleanTrue;
    [Test] procedure TestBoolEmpty;

    [Test] procedure TestEnumString;

    [Test] procedure TestObjectValue;
    [Test] procedure TestObjectNull;

    [Test] procedure TestObjectLowerCase;
    [Test] procedure TestObjectUpperCase;
    [Test] procedure TestObjectUnderlineProperty;

    [Test] procedure TestObjectListFill;
    [Test] procedure TestObjectListEmpty;
    [Test] procedure TestObjectListOneElement;
    [Test] procedure TestObjectListNull;

    [Test] procedure TestListFill;
    [Test] procedure TestListEmpty;
    [Test] procedure TestListOneElement;
    [Test] procedure TestListNull;

    [Test] procedure TestArrayStringFill;
    [Test] procedure TestArrayStringEmpty;
    [Test] procedure TestArrayStringOneElement;

    constructor create;
    destructor  Destroy; override;
end;

implementation

{ TKrakenTestDeserializePerson }

constructor TKrakenTestDeserializePerson.create;
begin
  FDeserialize := TKrakenDeserializer<TPerson>.New(False);
  FSerialize   := TKrakenSerializer<TPerson>.New(False);
end;

destructor TKrakenTestDeserializePerson.Destroy;
begin
  inherited;
end;

function TKrakenTestDeserializePerson.GetJsonObject(APerson: TPerson): TJSONObject;
begin
  FreeAndNil(FJSONObject);
  FJSONObject := FDeserialize.ObjectToJsonObject(APerson);

  result := FJSONObject;
end;

procedure TKrakenTestDeserializePerson.Setup;
begin
  FPerson     := TPerson.CreatePerson;
  FUpperPerson:= TUpperPerson.CreatePerson;
  FJSONObject := GetJsonObject(FPerson);
  TKrakenConfig.GetInstance.CaseDefinition(TCaseDefinition.cdNone);
end;

procedure TKrakenTestDeserializePerson.TearDown;
begin
  FreeAndNil(FJSONObject);
  FreeAndNil(FPerson);
  FreeAndNil(FUpperPerson);
  FreeAndNil(FAuxPerson);
end;

procedure TKrakenTestDeserializePerson.TestArrayStringEmpty;
begin
  FPerson.qualities := [];
  FJSONObject := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.IsTrue (Length(FAuxPerson.qualities) = 0);
  Assert.AreEqual(Length(FPerson.qualities), Length(FAuxPerson.qualities));
end;

procedure TKrakenTestDeserializePerson.TestArrayStringFill;
begin
  FJSONObject := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.IsTrue (Length(FAuxPerson.qualities) > 0);
  Assert.AreEqual('q1', FAuxPerson.qualities[0]);
  Assert.AreEqual('q2', FAuxPerson.qualities[1]);

  Assert.AreEqual(Length(FPerson.qualities), Length(FAuxPerson.qualities));
end;

procedure TKrakenTestDeserializePerson.TestArrayStringOneElement;
begin
  FPerson.qualities := ['q1'];
  FJSONObject := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.IsTrue (Length(FAuxPerson.qualities) > 0);
  Assert.AreEqual('q1', FAuxPerson.qualities[0]);

  Assert.AreEqual(Length(FPerson.qualities), Length(FAuxPerson.qualities));
end;

procedure TKrakenTestDeserializePerson.TestBooleanFalse;
begin
  FPerson.active := False;
  FJSONObject   := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.IsFalse(FAuxPerson.active);
end;

procedure TKrakenTestDeserializePerson.TestBooleanTrue;
begin
  FPerson.active := True;
  FJSONObject   := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.IsTrue(FAuxPerson.active);
end;

procedure TKrakenTestDeserializePerson.TestBoolEmpty;
begin
  FPerson.active := True;
  FJSONObject   := GetJsonObject(FPerson);
  FJSONObject.RemovePair('active').Free;

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.IsFalse(FAuxPerson.active);
end;

procedure TKrakenTestDeserializePerson.TestDateFill;
begin
  FPerson.creationDate := Now;
  FJSONObject := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.AreEqual(FormatDateTime('yyyy-MM-dd hh:mm:ss', FPerson.creationDate),
                  FormatDateTime('yyyy-MM-dd hh:mm:ss', FAuxPerson.creationDate));
end;

procedure TKrakenTestDeserializePerson.TestDateEmpty;
begin
  FPerson.creationDate := 0;
  FJSONObject := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.AreEqual(FPerson.creationDate, FAuxPerson.creationDate);
end;

procedure TKrakenTestDeserializePerson.TestEnumString;
begin
  FPerson.personType := tpJuridica;
  FJSONObject := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.AreEqual(FPerson.personType, FAuxPerson.personType);
end;

procedure TKrakenTestDeserializePerson.TestFloatNegative;
begin
  FPerson.average := -5;
  FJSONObject := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.AreEqual(FPerson.average, FAuxPerson.average);
end;

procedure TKrakenTestDeserializePerson.TestFloatNegativeWithDecimal;
begin
  FPerson.average := -5.25;
  FJSONObject := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.AreEqual(FPerson.average, FAuxPerson.average);
end;

procedure TKrakenTestDeserializePerson.TestFloatPositive;
begin
  FPerson.average := 15;
  FJSONObject := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.AreEqual(FPerson.average, FAuxPerson.average);
end;

procedure TKrakenTestDeserializePerson.TestFloatPositiveWithDecimal;
begin
  FPerson.average := 15.351;
  FJSONObject := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.AreEqual(FPerson.average, FAuxPerson.average);
end;

procedure TKrakenTestDeserializePerson.TestFloatZero;
begin
  FPerson.average := 0;
  FJSONObject := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.IsTrue(FAuxPerson.average = 0);
end;

procedure TKrakenTestDeserializePerson.TestIntegerNegative;
begin
  FPerson.age := -5;
  FJSONObject := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.AreEqual(FPerson.age, FAuxPerson.age);
end;

procedure TKrakenTestDeserializePerson.TestIntegerPositive;
begin
  FPerson.age := 50;
  FJSONObject := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.AreEqual(FPerson.age, FAuxPerson.age);
end;

procedure TKrakenTestDeserializePerson.TestListEmpty;
begin
  FPerson.notes.Clear;

  FJSONObject := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.IsTrue (FAuxPerson.notes.Count = 0);
end;

procedure TKrakenTestDeserializePerson.TestListFill;
begin
  FJSONObject := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.IsTrue (FAuxPerson.notes.Count > 0);
  Assert.IsTrue (FAuxPerson.notes[0] > 0);

  Assert.AreEqual(FPerson.notes.Count, FAuxPerson.notes.Count);
  Assert.AreEqual(FPerson.notes[0], FAuxPerson.notes[0]);
end;

procedure TKrakenTestDeserializePerson.TestListNull;
begin
  FPerson.notes.Free;
  FPerson.notes := nil;

  FJSONObject := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.IsNotNull(FAuxPerson.notes);
  Assert.IsTrue (FAuxPerson.notes.Count = 0);
end;

procedure TKrakenTestDeserializePerson.TestListOneElement;
begin
  FPerson.notes.Clear;
  FPerson.notes.Add(2);
  FJSONObject := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.IsTrue (FAuxPerson.notes.Count = 1);
  Assert.AreEqual('2', FAuxPerson.notes[0].ToString);

  Assert.AreEqual(FPerson.notes.Count, FAuxPerson.notes.Count);
  Assert.AreEqual(FPerson.notes[0], FAuxPerson.notes[0]);
end;

procedure TKrakenTestDeserializePerson.TestIntegerEmpty;
begin
  FPerson.age := 0;
  FJSONObject := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.AreEqual(FPerson.age, FAuxPerson.age);
end;

procedure TKrakenTestDeserializePerson.TestObjectListFill;
begin
  FJSONObject := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.IsTrue (FAuxPerson.phones.Count > 0);
  Assert.IsFalse(FAuxPerson.phones[0].number.IsEmpty);

  Assert.AreEqual(FPerson.phones.Count, FAuxPerson.phones.Count);
  Assert.AreEqual(FPerson.phones[0].number, FAuxPerson.phones[0].number);
end;

procedure TKrakenTestDeserializePerson.TestObjectListNull;
begin
  FPerson.phones.Free;
  FPerson.phones := nil;

  FJSONObject := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.IsNotNull(FAuxPerson.phones);
  Assert.IsTrue (FAuxPerson.phones.Count = 0);
end;

procedure TKrakenTestDeserializePerson.TestObjectListOneElement;
begin
  FPerson.phones.Remove(FPerson.phones[1]);
  FJSONObject := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.IsTrue (FAuxPerson.phones.Count = 1);
  Assert.IsFalse(FAuxPerson.phones[0].number.IsEmpty);

  Assert.AreEqual(FPerson.phones.Count, FAuxPerson.phones.Count);
  Assert.AreEqual(FPerson.phones[0].number, FAuxPerson.phones[0].number);
end;

procedure TKrakenTestDeserializePerson.TestObjectLowerCase;
begin
  FreeAndNil(FJSONObject);
  TKrakenConfig.GetInstance
    .CaseDefinition(TCaseDefinition.cdLower);

  FUpperPerson.PERSON_ID := 1;
  FUpperPerson.PERSON_NAME := 'Test Person';

  FJSONObject := TKrakenDefault.Deserializer<TUpperPerson>
                    .ObjectToJsonObject(FUpperPerson);

  Assert.IsNotNull(FJSONObject.GetValue('person_id'));
  Assert.IsNotNull(FJSONObject.GetValue('person_name'));
end;

procedure TKrakenTestDeserializePerson.TestObjectListEmpty;
begin
  FPerson.phones.Remove(FPerson.phones[0]);
  FPerson.phones.Remove(FPerson.phones[0]);

  FJSONObject := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.IsTrue (FAuxPerson.phones.Count = 0);
end;

procedure TKrakenTestDeserializePerson.TestObjectNull;
begin
  FPerson.address.Free;
  FPerson.address := nil;

  FJSONObject := GetJsonObject(FPerson);
  FAuxPerson  := FSerialize.JsonObjectToObject(FJSONObject);

  Assert.IsEmpty(FAuxPerson.address.street);
end;

procedure TKrakenTestDeserializePerson.TestObjectUnderlineProperty;
begin
  FreeAndNil(FJSONObject);
  TKrakenConfig.GetInstance
    .CaseDefinition(TCaseDefinition.cdNone);

  FPerson.document_number := '123456';

  FJSONObject := TKrakenDefault.Deserializer<TUpperPerson>
                    .ObjectToJsonObject(FPerson);

  Assert.IsNotNull(FJSONObject.GetValue('document_number'));
  Assert.AreEqual('123456', FJSONObject.ValueAsString('document_number'));
end;

procedure TKrakenTestDeserializePerson.TestObjectUpperCase;
begin
  FreeAndNil(FJSONObject);
  TKrakenConfig.GetInstance
    .CaseDefinition(TCaseDefinition.cdUpper);

  FUpperPerson.PERSON_ID := 1;
  FUpperPerson.PERSON_NAME := 'Test Person';

  FJSONObject := TKrakenDefault.Deserializer<TUpperPerson>
                    .ObjectToJsonObject(FUpperPerson);

  Assert.IsNotNull(FJSONObject.GetValue('PERSON_ID'));
  Assert.IsNotNull(FJSONObject.GetValue('PERSON_NAME'));
end;

procedure TKrakenTestDeserializePerson.TestObjectValue;
begin
  FPerson.address.street := 'Rua Tal';
  FJSONObject := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.AreEqual(FPerson.address.street, FAuxPerson.address.street);
end;

procedure TKrakenTestDeserializePerson.TestStringWithAccent;
begin
  FPerson.name := 'Tomé';
  FJSONObject := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.IsNotNull(FAuxPerson);
  Assert.AreEqual(FPerson.name, FAuxPerson.name);
end;

procedure TKrakenTestDeserializePerson.TestStringWithBar;
begin
  FPerson.name := 'Value 1 / Value 2';
  FJSONObject := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.IsNotNull(FAuxPerson);
  Assert.AreEqual(FPerson.name, FAuxPerson.name);
end;

procedure TKrakenTestDeserializePerson.TestStringWithDoubleQuotes;
begin
  FPerson.name := 'Name With "Quotes"';
  FJSONObject := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.IsNotNull(FAuxPerson);
  Assert.AreEqual(FPerson.name, FAuxPerson.name);
  Assert.AreEqual('Name With "Quotes"', FAuxPerson.name);
end;

procedure TKrakenTestDeserializePerson.TestStringWithBackslash;
begin
  FPerson.name := 'Value 1 \ Value 2';
  FJSONObject := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.IsNotNull(FAuxPerson);
  Assert.AreEqual(FPerson.name, FAuxPerson.name);
end;

procedure TKrakenTestDeserializePerson.TestStringEmpty;
begin
  FPerson.name := EmptyStr;
  FJSONObject := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.IsNotNull(FAuxPerson);
  Assert.AreEqual(FPerson.name, FAuxPerson.name);
end;

procedure TKrakenTestDeserializePerson.TestStringName;
begin
  FPerson.name := 'Value 1';
  FJSONObject := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.IsNotNull(FAuxPerson);
  Assert.AreEqual(FPerson.name, FAuxPerson.name);
end;

end.
