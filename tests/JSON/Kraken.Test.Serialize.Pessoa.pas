unit Kraken.Test.Serialize.Pessoa;

interface

uses
  DUnitX.TestFramework,
  Kraken.Test.Models,
  Kraken.DateTime.Helper,
  Kraken.Interfaces,
  System.JSON,
  System.Generics.Collections,
  System.SysUtils;

type TKrakenTestSerializePessoa = class

  private
    FPessoa     : TPessoa;
    FDeserialize: IKrakenDeserializer<TPessoa>;
    FJSONObject : TJSONObject;

    function GetJsonObject(APessoa: TPessoa): TJSONObject;
  public
    [Setup]    procedure Setup;
    [TearDown] procedure TearDown;

    [Test] procedure TestStringPreenchida;
    [Test] procedure TestStringVazia;
    [Test] procedure TestStringComAcento;
    [Test] procedure TestStringComBarra;
    [Test] procedure TestStringComBarraInvertida;
    [Test] procedure TestStringValorZero;

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

{ TKrakenTestSerializePessoa }

constructor TKrakenTestSerializePessoa.create;
begin
  FDeserialize := TKrakenDefault.Deserializer<TPessoa>;
end;

destructor TKrakenTestSerializePessoa.Destroy;
begin
  inherited;
end;

function TKrakenTestSerializePessoa.GetJsonObject(APessoa: TPessoa): TJSONObject;
begin
  FreeAndNil(FJSONObject);
  FJSONObject := FDeserialize.ObjectToJsonObject(APessoa);

  result := FJSONObject;
end;

procedure TKrakenTestSerializePessoa.Setup;
begin
  FPessoa     := TPessoa.CreatePessoa;
  FJSONObject := GetJsonObject(FPessoa);
end;

procedure TKrakenTestSerializePessoa.TearDown;
begin
  FreeAndNil(FJSONObject);
  FreeAndNil(FPessoa);
end;

procedure TKrakenTestSerializePessoa.TestBooleanFalse;
begin
  FPessoa.ativo := False;
  FJSONObject := GetJsonObject(FPessoa);

  Assert.IsNotNull(FJSONObject.Values['ativo']);
  Assert.AreEqual('false', FJSONObject.Values['ativo'].Value);
end;

procedure TKrakenTestSerializePessoa.TestBooleanTrue;
begin
  FPessoa.ativo := True;
  FJSONObject := GetJsonObject(FPessoa);

  Assert.IsNotNull(FJSONObject.Values['ativo']);
  Assert.AreEqual('true', FJSONObject.Values['ativo'].Value);
end;

procedure TKrakenTestSerializePessoa.TestDataPreenchida;
var
  data : TDateTime;
begin
  FPessoa.dataCadastro := Now;
  FJSONObject := GetJsonObject(FPessoa);

  Assert.IsNotNull(FJSONObject.Values['dataCadastro']);

  data.fromIso8601ToDateTime( FJSONObject.Values['dataCadastro'].Value);

  Assert.AreEqual(FormatDateTime('yyyy-MM-dd hh:mm:ss', FPessoa.dataCadastro),
                  FormatDateTime('yyyy-MM-dd hh:mm:ss', data));
end;

procedure TKrakenTestSerializePessoa.TestDataVazia;
begin
  FPessoa.dataCadastro := 0;
  FJSONObject := GetJsonObject(FPessoa);

  Assert.IsNull(FJSONObject.Values['dataCadastro']);
end;

procedure TKrakenTestSerializePessoa.TestEnumString;
begin
  FPessoa.tipoPessoa := TTipoPessoa.tpJuridica;
  FJSONObject        := FDeserialize.ObjectToJsonObject(FPessoa);

  Assert.AreEqual('tpJuridica', FJSONObject.Values['tipoPessoa'].Value);
end;

procedure TKrakenTestSerializePessoa.TestFloatNegativo;
begin
  FPessoa.media := -5;
  FJSONObject := GetJsonObject(FPessoa);

  Assert.IsNotNull(FJSONObject.Values['media']);
  Assert.AreEqual('-5', FJSONObject.Values['media'].Value);
end;

procedure TKrakenTestSerializePessoa.TestFloatNegativoComDecimal;
begin
  FPessoa.media := -5.15;
  FJSONObject := GetJsonObject(FPessoa);

  Assert.IsNotNull(FJSONObject.Values['media']);
  Assert.AreEqual('-5.15', FJSONObject.Values['media'].ToString);
end;

procedure TKrakenTestSerializePessoa.TestFloatPositivo;
begin
  FPessoa.media := 5;
  FJSONObject := GetJsonObject(FPessoa);

  Assert.IsNotNull(FJSONObject.Values['media']);
  Assert.AreEqual('5', FJSONObject.Values['media'].Value);
end;

procedure TKrakenTestSerializePessoa.TestFloatPositivoComDecimal;
begin
  FPessoa.media := 5.25;
  FJSONObject := GetJsonObject(FPessoa);

  Assert.IsNotNull(FJSONObject.Values['media']);
  Assert.AreEqual('5.25', FJSONObject.Values['media'].ToString);
end;

procedure TKrakenTestSerializePessoa.TestFloatZero;
begin
  FPessoa.media := 0;
  FJSONObject := GetJsonObject(FPessoa);

  Assert.IsNull(FJSONObject.Values['media']);
end;

procedure TKrakenTestSerializePessoa.TestIntegerNegativo;
begin
  FPessoa.idade := -5;
  FJSONObject   := GetJsonObject(FPessoa);

  Assert.AreEqual(FPessoa.idade, FJSONObject.Values['idade'].Value.ToInteger);
end;

procedure TKrakenTestSerializePessoa.TestIntegerPreenchido;
begin
  FPessoa.idade := 18;
  FJSONObject   := GetJsonObject(FPessoa);

  Assert.AreEqual(FPessoa.idade, FJSONObject.Values['idade'].Value.ToInteger);
end;

procedure TKrakenTestSerializePessoa.TestIntegerVazio;
begin
  FPessoa.idade := 0;
  FJSONObject := GetJsonObject(FPessoa);

  Assert.IsNull(FJSONObject.Values['idade']);
end;

procedure TKrakenTestSerializePessoa.TestObjectListCheio;
var
  jsonArray: TJSONArray;
begin
  jsonArray := FJSONObject.Values['telefones'] as TJSONArray;
  Assert.AreEqual(2, jsonArray.Count);
  Assert.IsNotNull(TJSONObject( jsonArray.Items[0]).Values['numero']);
end;

procedure TKrakenTestSerializePessoa.TestObjectListNull;
begin
  FPessoa.telefones.Free;
  FPessoa.telefones := nil;

  FJSONObject := GetJsonObject(FPessoa);

  Assert.IsNull(FJSONObject.Values['telefones']);
end;

procedure TKrakenTestSerializePessoa.TestObjectListUmElemento;
var
  jsonArray: TJSONArray;
begin
  FPessoa.telefones.Remove(FPessoa.telefones[1]);
  FJSONObject := GetJsonObject(FPessoa);
  jsonArray := FJSONObject.Values['telefones'] as TJSONArray;
  Assert.AreEqual(1, jsonArray.Count);
end;

procedure TKrakenTestSerializePessoa.TestObjectListVazio;
begin
  FPessoa.telefones.Remove(FPessoa.telefones[0]);
  FPessoa.telefones.Remove(FPessoa.telefones[0]);

  FJSONObject := GetJsonObject(FPessoa);
  Assert.IsNull(FJSONObject.Values['telefones']);
end;

procedure TKrakenTestSerializePessoa.TestObjectNull;
begin
  FPessoa.endereco.Free;
  FPessoa.endereco := nil;

  FJSONObject := GetJsonObject(FPessoa);
  Assert.IsNull(FJSONObject.Values['endereco']);
end;

procedure TKrakenTestSerializePessoa.TestObjectValue;
begin
  FPessoa.endereco.logradouro := 'Rua Tal';
  FJSONObject := GetJsonObject(FPessoa);

  Assert.IsNotNull(TJSONObject( FJSONObject.Values['endereco'] ));
  Assert.AreEqual (FPessoa.endereco.logradouro, TJSONObject(FJSONObject.Values['endereco']).Values['logradouro'].Value);
end;

procedure TKrakenTestSerializePessoa.TestStringComAcento;
begin
  FPessoa.nome := 'João';
  FJSONObject  := GetJsonObject(FPessoa);

  Assert.AreEqual(FPessoa.nome, FJSONObject.Values['nome'].Value );
end;

procedure TKrakenTestSerializePessoa.TestStringComBarra;
var
  nome: string;
begin
  FPessoa.nome := 'Nome 1 / Nome 2';
  FJSONObject  := GetJsonObject(FPessoa);

  nome := FJSONObject.Values['nome'].Value;

  Assert.AreEqual(FPessoa.nome,  nome);
end;

procedure TKrakenTestSerializePessoa.TestStringComBarraInvertida;
var
  nome: string;
begin
  FPessoa.nome := 'Nome 1 \ Nome 2';
  FJSONObject  := GetJsonObject(FPessoa);

  nome := FJSONObject.Values['nome'].Value;

  Assert.AreEqual(FPessoa.nome,  nome);
end;

procedure TKrakenTestSerializePessoa.TestStringPreenchida;
var
  nome : String;
begin
  nome := FJSONObject.GetValue<String>('nome', EmptyStr);
  Assert.AreEqual(FPessoa.nome, nome);
end;

procedure TKrakenTestSerializePessoa.TestStringValorZero;
begin
  FPessoa.nome := '0';
  FJSONObject  := GetJsonObject(FPessoa);

  Assert.IsNotNull( FJSONObject.Values['nome'] );
  Assert.AreEqual('0', FJSONObject.Values['nome'].Value);
end;

procedure TKrakenTestSerializePessoa.TestStringVazia;
begin
  FPessoa.nome := EmptyStr;
  FJSONObject  := GetJsonObject(FPessoa);

  Assert.IsNull( FJSONObject.Values['nome'] );
end;

end.
