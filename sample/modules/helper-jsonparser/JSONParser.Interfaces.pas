unit JSONParser.Interfaces;

interface

uses
  System.JSON,
  System.Generics.Collections;

type
  IJSONParserSerializer<T: class, constructor> = interface
    ['{F808BE4D-AF1A-4BDF-BF3B-945C39762853}']
    procedure JsonObjectToObject(AObject: TObject; Value: TJSONObject); overload;
    function  JsonObjectToObject(Value: TJSONObject): T; overload;
    function  JsonStringToObject(Value: String): T;

    function JsonArrayToList (Value: TJSONArray): TObjectList<T>;
    function JsonStringToList(Value: String): TObjectList<T>;
  end;

  IJSONParserDeserializer<T: class, constructor> = interface
    ['{C61D8875-A70B-4E65-911E-776FECC610F4}']
    function ObjectToJsonString(Value: TObject): string;
    function ObjectToJsonObject(Value: TObject): TJSONObject;
    function StringToJsonObject(Value: string) : TJSONObject;

    function ListToJSONArray(AList: TObjectList<T>): TJSONArray;
  end;

  TJSONParserDefault = class
    public
      class function Serializer(bUseIgnore: boolean = True): IJSONParserSerializer<TObject>; overload;
      class function Serializer<T: class, constructor>(bUseIgnore: boolean = True): IJSONParserSerializer<T>; overload;

      class function Deserializer(bUseIgnore: boolean = True; bUseBackslash: boolean = False): IJSONParserDeserializer<TObject>; overload;
      class function Deserializer<T: class, constructor>(bUseIgnore: boolean = True): IJSONParserDeserializer<T>; overload;
  end;

implementation

uses
  JSONParser.Serializer,
  JSONParser.Deserializer;

class function TJSONParserDefault.Deserializer(bUseIgnore: boolean = True; bUseBackslash: boolean = False): IJSONParserDeserializer<TObject>;
begin
  result := TJSONParserDeserializer<TObject>.New(bUseIgnore, bUseBackslash);
end;

class function TJSONParserDefault.Deserializer<T>(bUseIgnore: boolean = True): IJSONParserDeserializer<T>;
begin
  result := TJSONParserDeserializer<T>.New(bUseIgnore);
end;

class function TJSONParserDefault.Serializer(bUseIgnore: boolean): IJSONParserSerializer<TObject>;
begin
  Result := TJSONParserSerializer<TObject>.New(bUseIgnore);
end;

class function TJSONParserDefault.Serializer<T>(bUseIgnore: boolean): IJSONParserSerializer<T>;
begin
  Result := TJSONParserSerializer<T>.New(bUseIgnore);
end;

end.
