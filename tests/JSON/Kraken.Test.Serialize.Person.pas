unit Kraken.Test.Serialize.Person;

interface

uses
  DUnitX.TestFramework,
  Kraken.Test.Models,
  Kraken.Interfaces,
  Kraken.Helper,
  Kraken.Deserializer,
  Kraken.Serializer,
  System.JSON,
  System.SysUtils;

type TKrakenTestSerializePerson = class

  private
    FPerson      : TPerson;
    FAuxPerson   : TPerson;
    FUpperPerson : TUpperPerson;
    FDeserialize : IKrakenDeserializer<TPerson>;
    FSerialize   : IKrakenSerializer<TPerson>;
    FJSONObject  : TJSONObject;

    function GetJsonObject(APerson: TPerson): TJSONObject;
  public
    constructor create;
    destructor  Destroy; override;

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

    [Test] procedure TestListFloatFill;
    [Test] procedure TestListFloatEmpty;
    [Test] procedure TestListFloatOneElement;
    [Test] procedure TestListFloatNull;

    [Test] procedure TestArrayStringFill;
    [Test] procedure TestArrayStringEmpty;
    [Test] procedure TestArrayStringOneElement;

end;

implementation

{ TKrakenTestSerializePerson }

constructor TKrakenTestSerializePerson.create;
begin
  FDeserialize := TKrakenDeserializer<TPerson>.New(False);
  FSerialize   := TKrakenSerializer<TPerson>.New(False);
end;

destructor TKrakenTestSerializePerson.Destroy;
begin
  inherited;
end;

function TKrakenTestSerializePerson.GetJsonObject(APerson: TPerson): TJSONObject;
begin
  FreeAndNil(FJSONObject);
  FJSONObject := FDeserialize.ObjectToJsonObject(APerson);

  result := FJSONObject;
end;

procedure TKrakenTestSerializePerson.Setup;
begin
  FPerson     := TPerson.CreatePerson;
  FJSONObject := GetJsonObject(FPerson);
end;

procedure TKrakenTestSerializePerson.TearDown;
begin
  FreeAndNil(FJSONObject);
  FreeAndNil(FUpperPerson);
  FreeAndNil(FPerson);
  FreeAndNil(FAuxPerson);
end;

procedure TKrakenTestSerializePerson.TestArrayStringEmpty;
begin
  FPerson.qualities := [];
  FJSONObject := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.IsTrue (Length( FAuxPerson.qualities) = 0);
end;

procedure TKrakenTestSerializePerson.TestArrayStringFill;
begin
  FJSONObject := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.AreEqual(2, Length( FAuxPerson.qualities));
  Assert.AreEqual('q1', FAuxPerson.qualities[0]);
  Assert.AreEqual('q2', FAuxPerson.qualities[1]);
end;

procedure TKrakenTestSerializePerson.TestArrayStringOneElement;
begin
  FPerson.qualities := ['q1'];
  FJSONObject := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.AreEqual(1, Length( FAuxPerson.qualities));
  Assert.AreEqual('q1', FAuxPerson.qualities[0]);
end;

procedure TKrakenTestSerializePerson.TestBooleanFalse;
begin
  FPerson.active := False;
  FJSONObject   := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.IsFalse(FAuxPerson.active);
end;

procedure TKrakenTestSerializePerson.TestBooleanTrue;
begin
  FPerson.active := True;
  FJSONObject   := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.IsTrue(FAuxPerson.active);
end;

procedure TKrakenTestSerializePerson.TestBoolEmpty;
begin
  FPerson.active := True;
  FJSONObject   := GetJsonObject(FPerson);
  FJSONObject.RemovePair('active').Free;

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.IsFalse(FAuxPerson.active);
end;

procedure TKrakenTestSerializePerson.TestDateFill;
begin
  FPerson.creationDate := Now;
  FJSONObject := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.AreEqual(FormatDateTime('yyyy-MM-dd hh:mm:ss', FPerson.creationDate),
                  FormatDateTime('yyyy-MM-dd hh:mm:ss', FAuxPerson.creationDate));
end;

procedure TKrakenTestSerializePerson.TestDateEmpty;
begin
  FPerson.creationDate := 0;
  FJSONObject := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.AreEqual(FPerson.creationDate, FAuxPerson.creationDate);
end;

procedure TKrakenTestSerializePerson.TestEnumString;
begin
  FPerson.personType := tpJuridica;
  FJSONObject := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.AreEqual(FPerson.personType, FAuxPerson.personType);
end;

procedure TKrakenTestSerializePerson.TestFloatNegative;
begin
  FPerson.average := -5;
  FJSONObject := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.AreEqual(FPerson.average, FAuxPerson.average);
end;

procedure TKrakenTestSerializePerson.TestFloatNegativeWithDecimal;
begin
  FPerson.average := -5.25;
  FJSONObject := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.AreEqual(FPerson.average, FAuxPerson.average);
end;

procedure TKrakenTestSerializePerson.TestFloatPositive;
begin
  FPerson.average := 15;
  FJSONObject := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.AreEqual(FPerson.average, FAuxPerson.average);
end;

procedure TKrakenTestSerializePerson.TestFloatPositiveWithDecimal;
begin
  FPerson.average := 15.351;
  FJSONObject := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.AreEqual(FPerson.average, FAuxPerson.average);
end;

procedure TKrakenTestSerializePerson.TestFloatZero;
begin
  FPerson.average := 0;
  FJSONObject := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.IsTrue(FAuxPerson.average = 0);
end;

procedure TKrakenTestSerializePerson.TestIntegerNegative;
begin
  FPerson.age := -5;
  FJSONObject := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.AreEqual(FPerson.age, FAuxPerson.age);
end;

procedure TKrakenTestSerializePerson.TestIntegerPositive;
begin
  FPerson.age := 50;
  FJSONObject := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.AreEqual(FPerson.age, FAuxPerson.age);
end;

procedure TKrakenTestSerializePerson.TestListFloatEmpty;
begin
  FPerson.notes.Clear;
  FJSONObject := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.IsTrue (FAuxPerson.notes.Count = 0);
end;

procedure TKrakenTestSerializePerson.TestListFloatFill;
begin
  FJSONObject := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.AreEqual(2, FAuxPerson.notes.Count);
  Assert.AreEqual('5', FAuxPerson.notes[0].ToString);
  Assert.AreEqual('6', FAuxPerson.notes[1].ToString);
end;

procedure TKrakenTestSerializePerson.TestListFloatNull;
begin
  FPerson.notes.Free;
  FPerson.notes := nil;

  FJSONObject := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.AreEqual(0, FAuxPerson.notes.Count);
end;

procedure TKrakenTestSerializePerson.TestListFloatOneElement;
begin
  FPerson.notes.Clear;
  FPerson.notes.Add(1);

  FJSONObject := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.AreEqual(1, FAuxPerson.notes.Count);
  Assert.AreEqual('1', FAuxPerson.notes[0].ToString);
end;

procedure TKrakenTestSerializePerson.TestIntegerEmpty;
begin
  FPerson.age := 0;
  FJSONObject := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.AreEqual(FPerson.age, FAuxPerson.age);
end;

procedure TKrakenTestSerializePerson.TestObjectListFill;
begin
  FJSONObject := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.IsTrue (FAuxPerson.phones.Count > 0);
  Assert.IsFalse(FAuxPerson.phones[0].number.IsEmpty);

  Assert.AreEqual(FPerson.phones.Count, FAuxPerson.phones.Count);
  Assert.AreEqual(FPerson.phones[0].number, FAuxPerson.phones[0].number);
end;

procedure TKrakenTestSerializePerson.TestObjectListNull;
begin
  FPerson.phones.Free;
  FPerson.phones := nil;

  FJSONObject := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.IsNotNull(FAuxPerson.phones);
  Assert.IsTrue (FAuxPerson.phones.Count = 0);
end;

procedure TKrakenTestSerializePerson.TestObjectListOneElement;
begin
  FPerson.phones.Remove(FPerson.phones[1]);
  FJSONObject := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.IsTrue (FAuxPerson.phones.Count = 1);
  Assert.IsFalse(FAuxPerson.phones[0].number.IsEmpty);

  Assert.AreEqual(FPerson.phones.Count, FAuxPerson.phones.Count);
  Assert.AreEqual(FPerson.phones[0].number, FAuxPerson.phones[0].number);
end;

procedure TKrakenTestSerializePerson.TestObjectLowerCase;
begin
  FreeAndNil(FJSONObject);
  FJSONObject := TJSONObject.Create;
  FJSONObject
    .AddPair('person_id', TJSONNumber.Create(1))
    .AddPair('person_name', 'Person Test');

  TKrakenConfig.GetInstance
    .CaseDefinition(TCaseDefinition.cdLower);

  FUpperPerson := TUpperPerson.create;
  FUpperPerson.fromJSONObject(FJSONObject);

  Assert.AreEqual('1', FUpperPerson.PERSON_ID.ToString);
  Assert.AreEqual('Person Test', FUpperPerson.PERSON_NAME);
end;

procedure TKrakenTestSerializePerson.TestObjectListEmpty;
begin
  FPerson.phones.Remove(FPerson.phones[0]);
  FPerson.phones.Remove(FPerson.phones[0]);

  FJSONObject := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.IsTrue (FAuxPerson.phones.Count = 0);
end;

procedure TKrakenTestSerializePerson.TestObjectNull;
begin
  FPerson.address.Free;
  FPerson.address := nil;

  FJSONObject := GetJsonObject(FPerson);
  FAuxPerson  := FSerialize.JsonObjectToObject(FJSONObject);

  Assert.IsEmpty(FAuxPerson.address.street);
end;

procedure TKrakenTestSerializePerson.TestObjectUnderlineProperty;
begin
  FreeAndNil(FPerson);
  FreeAndNil(FJSONObject);
  FJSONObject := TJSONObject.Create;
  FJSONObject
    .AddPair('document_number', '123456');

  TKrakenConfig.GetInstance
    .CaseDefinition(TCaseDefinition.cdNone);

  FPerson := TPerson.Create;
  FPerson.fromJSONObject(FJSONObject);

  Assert.AreEqual('123456', FPerson.document_number);
end;

procedure TKrakenTestSerializePerson.TestObjectUpperCase;
begin
  FreeAndNil(FPerson);
  FreeAndNil(FJSONObject);
  FJSONObject := TJSONObject.Create;
  FJSONObject
    .AddPair('IDPERSON', TJSONNumber.Create(1))
    .AddPair('NAME', 'Person Test');

  TKrakenConfig.GetInstance
    .CaseDefinition(TCaseDefinition.cdUpper);

  FPerson := TPerson.Create;
  FPerson.fromJSONObject(FJSONObject);

  Assert.AreEqual('1', FPerson.idPerson.ToString);
  Assert.AreEqual('Person Test', FPerson.name);
end;

procedure TKrakenTestSerializePerson.TestObjectValue;
begin
  FPerson.address.street := 'Rua Tal';
  FJSONObject := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.AreEqual(FPerson.address.street, FAuxPerson.address.street);
end;

procedure TKrakenTestSerializePerson.TestStringWithAccent;
begin
  FPerson.name := 'Tomé';
  FJSONObject := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.IsNotNull(FAuxPerson);
  Assert.AreEqual(FPerson.name, FAuxPerson.name);
end;

procedure TKrakenTestSerializePerson.TestStringWithBar;
begin
  FPerson.name := 'Value 1 / Value 2';
  FJSONObject := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.IsNotNull(FAuxPerson);
  Assert.AreEqual(FPerson.name, FAuxPerson.name);
end;

procedure TKrakenTestSerializePerson.TestStringWithDoubleQuotes;
begin
  FPerson.name := 'Name With "Quotes"';
  FJSONObject := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.IsNotNull(FAuxPerson);
  Assert.AreEqual(FPerson.name, FAuxPerson.name);
  Assert.AreEqual('Name With "Quotes"', FAuxPerson.name);
end;

procedure TKrakenTestSerializePerson.TestStringWithBackslash;
begin
  FPerson.name := 'Value 1 \ Value 2';
  FJSONObject := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.IsNotNull(FAuxPerson);
  Assert.AreEqual(FPerson.name, FAuxPerson.name);
  Assert.AreEqual('Value 1 \ Value 2', FAuxPerson.name);
end;

procedure TKrakenTestSerializePerson.TestStringEmpty;
begin
  FPerson.name := EmptyStr;
  FJSONObject := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.IsNotNull(FAuxPerson);
  Assert.AreEqual(FPerson.name, FAuxPerson.name);
end;

procedure TKrakenTestSerializePerson.TestStringName;
begin
  FPerson.name := 'Value 1';
  FJSONObject := GetJsonObject(FPerson);

  FAuxPerson := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.IsNotNull(FAuxPerson);
  Assert.AreEqual(FPerson.name, FAuxPerson.name);
end;

end.
