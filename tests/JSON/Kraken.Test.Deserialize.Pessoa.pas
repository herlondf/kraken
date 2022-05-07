unit Kraken.Test.Deserialize.Pessoa;

interface

uses
  DUnitX.TestFramework,
  Kraken.Test.Models,
  Kraken.Interfaces,
  System.JSON,
  System.SysUtils;

type TKrakenTestDeserializePessoa = class

  private
    FPessoa      : TPessoa;
    FAuxPessoa   : TPessoa;
    FDeserialize : IKrakenDeserializer<TPessoa>;
    FSerialize   : IKrakenSerializer<TPessoa>;
    FJSONObject  : TJSONObject;

    function GetJsonObject(APessoa: TPessoa): TJSONObject;
  public
    [Setup]    procedure Setup;
    [TearDown] procedure TearDown;

    [Test] procedure TestStringNome;
    [Test] procedure TestStringEmpty;
    [Test] procedure TestStringComAcento;
    [Test] procedure TestStringComBarra;
    [Test] procedure TestStringComBarraInvertida;

    [Test] procedure TestIntegerPreenchido;
    [Test] procedure TestIntegerVazio;
    [Test] procedure TestIntegerNegativo;

    [Test] procedure TestFloatPositivo;
    [Test] procedure TestFloatNegativo;
    [Test] procedure TestFloatZero;
    [Test] procedure TestFloatPositivoComDecimal;
    [Test] procedure TestFloatNegativoComDecimal;

    [Test] procedure TestDataVazia;
    [Test] procedure TestDataPreenchida;

    [Test] procedure TestBooleanFalse;
    [Test] procedure TestBooleanTrue;
    [Test] procedure TestBoolVazio;

    [Test] procedure TestEnumString;

    [Test] procedure TestObjectValue;
    [Test] procedure TestObjectNull;

    [Test] procedure TestObjectListCheio;
    [Test] procedure TestObjectListVazio;
    [Test] procedure TestObjectListUmElemento;
    [Test] procedure TestObjectListNull;

    constructor create;
    destructor  Destroy; override;
end;

implementation

{ TKrakenTestDeserializePessoa }

constructor TKrakenTestDeserializePessoa.create;
begin
  FDeserialize := TKrakenDefault.Deserializer<TPessoa>;
  FSerialize   := TKrakenDefault.Serializer<TPessoa>;
end;

destructor TKrakenTestDeserializePessoa.Destroy;
begin
  inherited;
end;

function TKrakenTestDeserializePessoa.GetJsonObject(APessoa: TPessoa): TJSONObject;
begin
  FreeAndNil(FJSONObject);
  FJSONObject := FDeserialize.ObjectToJsonObject(APessoa);

  result := FJSONObject;
end;

procedure TKrakenTestDeserializePessoa.Setup;
begin
  FPessoa     := TPessoa.CreatePessoa;
  FJSONObject := GetJsonObject(FPessoa);
end;

procedure TKrakenTestDeserializePessoa.TearDown;
begin
  FreeAndNil(FJSONObject);
  FreeAndNil(FPessoa);
  FreeAndNil(FAuxPessoa);
end;

procedure TKrakenTestDeserializePessoa.TestBooleanFalse;
begin
  FPessoa.ativo := False;
  FJSONObject   := GetJsonObject(FPessoa);

  FAuxPessoa := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.IsFalse(FAuxPessoa.ativo);
end;

procedure TKrakenTestDeserializePessoa.TestBooleanTrue;
begin
  FPessoa.ativo := True;
  FJSONObject   := GetJsonObject(FPessoa);

  FAuxPessoa := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.IsTrue(FAuxPessoa.ativo);
end;

procedure TKrakenTestDeserializePessoa.TestBoolVazio;
begin
  FPessoa.ativo := True;
  FJSONObject   := GetJsonObject(FPessoa);
  FJSONObject.RemovePair('ativo');

  FAuxPessoa := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.IsFalse(FAuxPessoa.ativo);
end;

procedure TKrakenTestDeserializePessoa.TestDataPreenchida;
begin
  FPessoa.dataCadastro := Now;
  FJSONObject := GetJsonObject(FPessoa);

  FAuxPessoa := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.AreEqual(FormatDateTime('yyyy-MM-dd hh:mm:ss', FPessoa.dataCadastro),
                  FormatDateTime('yyyy-MM-dd hh:mm:ss', FAuxPessoa.dataCadastro));
end;

procedure TKrakenTestDeserializePessoa.TestDataVazia;
begin
  FPessoa.dataCadastro := 0;
  FJSONObject := GetJsonObject(FPessoa);

  FAuxPessoa := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.AreEqual(FPessoa.dataCadastro, FAuxPessoa.dataCadastro);
end;

procedure TKrakenTestDeserializePessoa.TestEnumString;
begin
  FPessoa.tipoPessoa := tpJuridica;
  FJSONObject := GetJsonObject(FPessoa);

  FAuxPessoa := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.AreEqual(FPessoa.tipoPessoa, FAuxPessoa.tipoPessoa);
end;

procedure TKrakenTestDeserializePessoa.TestFloatNegativo;
begin
  FPessoa.media := -5;
  FJSONObject := GetJsonObject(FPessoa);

  FAuxPessoa := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.AreEqual(FPessoa.media, FAuxPessoa.media);
end;

procedure TKrakenTestDeserializePessoa.TestFloatNegativoComDecimal;
begin
  FPessoa.media := -5.25;
  FJSONObject := GetJsonObject(FPessoa);

  FAuxPessoa := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.AreEqual(FPessoa.media, FAuxPessoa.media);
end;

procedure TKrakenTestDeserializePessoa.TestFloatPositivo;
begin
  FPessoa.media := 15;
  FJSONObject := GetJsonObject(FPessoa);

  FAuxPessoa := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.AreEqual(FPessoa.media, FAuxPessoa.media);
end;

procedure TKrakenTestDeserializePessoa.TestFloatPositivoComDecimal;
begin
  FPessoa.media := 15.351;
  FJSONObject := GetJsonObject(FPessoa);

  FAuxPessoa := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.AreEqual(FPessoa.media, FAuxPessoa.media);
end;

procedure TKrakenTestDeserializePessoa.TestFloatZero;
begin
  FPessoa.media := 0;
  FJSONObject := GetJsonObject(FPessoa);

  FAuxPessoa := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.IsTrue(FAuxPessoa.media = 0);
end;

procedure TKrakenTestDeserializePessoa.TestIntegerNegativo;
begin
  FPessoa.idade := -5;
  FJSONObject := GetJsonObject(FPessoa);

  FAuxPessoa := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.AreEqual(FPessoa.idade, FAuxPessoa.idade);
end;

procedure TKrakenTestDeserializePessoa.TestIntegerPreenchido;
begin
  FPessoa.idade := 50;
  FJSONObject := GetJsonObject(FPessoa);

  FAuxPessoa := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.AreEqual(FPessoa.idade, FAuxPessoa.idade);
end;

procedure TKrakenTestDeserializePessoa.TestIntegerVazio;
begin
  FPessoa.idade := 0;
  FJSONObject := GetJsonObject(FPessoa);

  FAuxPessoa := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.AreEqual(FPessoa.idade, FAuxPessoa.idade);
end;

procedure TKrakenTestDeserializePessoa.TestObjectListCheio;
begin
  FJSONObject := GetJsonObject(FPessoa);

  FAuxPessoa := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.IsTrue (FAuxPessoa.telefones.Count > 0);
  Assert.IsFalse(FAuxPessoa.telefones[0].numero.IsEmpty);

  Assert.AreEqual(FPessoa.telefones.Count, FAuxPessoa.telefones.Count);
  Assert.AreEqual(FPessoa.telefones[0].numero, FAuxPessoa.telefones[0].numero);
end;

procedure TKrakenTestDeserializePessoa.TestObjectListNull;
begin
  FPessoa.telefones.Free;
  FPessoa.telefones := nil;

  FJSONObject := GetJsonObject(FPessoa);

  FAuxPessoa := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.IsNotNull(FAuxPessoa.telefones);
  Assert.IsTrue (FAuxPessoa.telefones.Count = 0);
end;

procedure TKrakenTestDeserializePessoa.TestObjectListUmElemento;
begin
  FPessoa.telefones.Remove(FPessoa.telefones[1]);
  FJSONObject := GetJsonObject(FPessoa);

  FAuxPessoa := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.IsTrue (FAuxPessoa.telefones.Count = 1);
  Assert.IsFalse(FAuxPessoa.telefones[0].numero.IsEmpty);

  Assert.AreEqual(FPessoa.telefones.Count, FAuxPessoa.telefones.Count);
  Assert.AreEqual(FPessoa.telefones[0].numero, FAuxPessoa.telefones[0].numero);
end;

procedure TKrakenTestDeserializePessoa.TestObjectListVazio;
begin
  FPessoa.telefones.Remove(FPessoa.telefones[0]);
  FPessoa.telefones.Remove(FPessoa.telefones[0]);

  FJSONObject := GetJsonObject(FPessoa);

  FAuxPessoa := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.IsTrue (FAuxPessoa.telefones.Count = 0);
end;

procedure TKrakenTestDeserializePessoa.TestObjectNull;
begin
  FPessoa.endereco.Free;
  FPessoa.endereco := nil;

  FJSONObject := GetJsonObject(FPessoa);
  FAuxPessoa  := FSerialize.JsonObjectToObject(FJSONObject);

  Assert.IsEmpty(FAuxPessoa.endereco.logradouro);
end;

procedure TKrakenTestDeserializePessoa.TestObjectValue;
begin
  FPessoa.endereco.logradouro := 'Rua Tal';
  FJSONObject := GetJsonObject(FPessoa);

  FAuxPessoa := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.AreEqual(FPessoa.endereco.logradouro, FAuxPessoa.endereco.logradouro);
end;

procedure TKrakenTestDeserializePessoa.TestStringComAcento;
begin
  FPessoa.nome := 'Tomé';
  FJSONObject := GetJsonObject(FPessoa);

  FAuxPessoa := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.IsNotNull(FAuxPessoa);
  Assert.AreEqual(FPessoa.nome, FAuxPessoa.nome);
end;

procedure TKrakenTestDeserializePessoa.TestStringComBarra;
begin
  FPessoa.nome := 'Value 1 / Value 2';
  FJSONObject := GetJsonObject(FPessoa);

  FAuxPessoa := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.IsNotNull(FAuxPessoa);
  Assert.AreEqual(FPessoa.nome, FAuxPessoa.nome);
end;

procedure TKrakenTestDeserializePessoa.TestStringComBarraInvertida;
begin
  FPessoa.nome := 'Value 1 \ Value 2';
  FJSONObject := GetJsonObject(FPessoa);

  FAuxPessoa := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.IsNotNull(FAuxPessoa);
  Assert.AreEqual(FPessoa.nome, FAuxPessoa.nome);
end;

procedure TKrakenTestDeserializePessoa.TestStringEmpty;
begin
  FPessoa.nome := EmptyStr;
  FJSONObject := GetJsonObject(FPessoa);

  FAuxPessoa := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.IsNotNull(FAuxPessoa);
  Assert.AreEqual(FPessoa.nome, FAuxPessoa.nome);
end;

procedure TKrakenTestDeserializePessoa.TestStringNome;
begin
  FPessoa.nome := 'Value 1';
  FJSONObject := GetJsonObject(FPessoa);

  FAuxPessoa := FSerialize.JsonObjectToObject(FJSONObject);
  Assert.IsNotNull(FAuxPessoa);
  Assert.AreEqual(FPessoa.nome, FAuxPessoa.nome);
end;

end.
