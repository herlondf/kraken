unit Kraken.Interfaces;

interface

uses
  Kraken.Config,
  System.JSON,
  System.Generics.Collections;

type
  TKrakenConfig = Kraken.Config.TKrakenConfig;
  TCaseDefinition = Kraken.Config.TCaseDefinition;

  IKrakenSerializer<T: class, constructor> = interface
    ['{F808BE4D-AF1A-4BDF-BF3B-945C39762853}']
    procedure JsonObjectToObject(AObject: TObject; Value: TJSONObject); overload;
    function  JsonObjectToObject(Value: TJSONObject): T; overload;
    function  JsonStringToObject(Value: String): T;

    function JsonArrayToList (Value: TJSONArray): TObjectList<T>;
    function JsonStringToList(Value: String): TObjectList<T>;
  end;

  IKrakenDeserializer<T: class, constructor> = interface
    ['{C61D8875-A70B-4E65-911E-776FECC610F4}']
    function ObjectToJsonString(Value: TObject): string;
    function ObjectToJsonObject(Value: TObject): TJSONObject;
    function StringToJsonObject(Value: string) : TJSONObject;

    function ListToJSONArray(AList: TObjectList<T>): TJSONArray;
  end;

  TKrakenDefault = class
    public
      class function Serializer(bUseIgnore: boolean = True): IKrakenSerializer<TObject>; overload;
      class function Serializer<T: class, constructor>(bUseIgnore: boolean = True): IKrakenSerializer<T>; overload;

      class function Deserializer(bUseIgnore: boolean = True): IKrakenDeserializer<TObject>; overload;
      class function Deserializer<T: class, constructor>(bUseIgnore: boolean = True): IKrakenDeserializer<T>; overload;
  end;

implementation

uses
  Kraken.Serializer,
  Kraken.Deserializer;

class function TKrakenDefault.Deserializer(bUseIgnore: boolean = True): IKrakenDeserializer<TObject>;
begin
  result := TKrakenDeserializer<TObject>.New(bUseIgnore);
end;

class function TKrakenDefault.Deserializer<T>(bUseIgnore: boolean = True): IKrakenDeserializer<T>;
begin
  result := TKrakenDeserializer<T>.New(bUseIgnore);
end;

class function TKrakenDefault.Serializer(bUseIgnore: boolean): IKrakenSerializer<TObject>;
begin
  Result := TKrakenSerializer<TObject>.New(bUseIgnore);
end;

class function TKrakenDefault.Serializer<T>(bUseIgnore: boolean): IKrakenSerializer<T>;
begin
  Result := TKrakenSerializer<T>.New(bUseIgnore);
end;

end.
