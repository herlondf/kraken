unit JSONParser.Helper;

interface

uses
  System.SysUtils,
  System.Classes,
  System.JSON,
  {$IF CompilerVersion <= 32.0}
  REST.Json,
  {$ENDIF}
  JSONParser.Interfaces;

type
  TJSONParserObjectHelper = class helper for TJSONObject

  public
    {$IF CompilerVersion <= 32.0}
    function Format: string; overload;
    {$ENDIF}

    class function ObjectToJSONString(Value: TObject): string;
    class function ObjectToJSONStringWithBackslash(Value: TObject): string;

    class function fromObject(Value: TObject): TJSONObject;
    class function fromFile  (Value: String) : TJSONObject;
    class function fromString(Value: String) : TJSONObject;

    class function format(Value: string): string; overload;

    procedure SaveToFile(AFileName: String);
    procedure toObject(Value: TObject; bUseIgnore: boolean = True);
  end;

  TObjectHelper = class helper for TObject
    public
      function ToJSONObject: TJSONObject;
      function ToJSONString(bFormat: Boolean = False; aBackslash: Boolean = False): string;
      procedure SaveToJSONFile(AFileName: String);

      procedure fromJSONObject(Value: TJSONObject);
      procedure fromJSONString(Value: String);
      procedure fromJSONFile(Value: String);
  end;

implementation

{ TJSONParserObjectHelper }

class function TJSONParserObjectHelper.format(Value: string): string;
var
  jsonObject: TJSONObject;
begin
  result     := EmptyStr;
  jsonObject := fromString(Value);
  try
    result := jsonObject.Format;
  finally
    jsonObject.Free;
  end;
end;

{$IF CompilerVersion <= 32.0}
function TJSONParserObjectHelper.Format: string;
begin
  Result := TJson.Format(Self);
end;
{$ENDIF}

class function TJSONParserObjectHelper.fromFile(Value: String): TJSONObject;
var
  fileJSON: TStringList;
begin
  if not FileExists(Value) then
    raise EFileNotFoundException.CreateFmt('Arquivo %s não encontrado', [Value]);

  fileJSON := TStringList.Create;
  try
    fileJSON.LoadFromFile(Value);
    result := fromString(fileJSON.Text);
  finally
    fileJSON.Free;
  end;
end;

class function TJSONParserObjectHelper.fromObject(Value: TObject): TJSONObject;
begin
  result := TJSONParserDefault.Deserializer.ObjectToJsonObject(Value);
end;

class function TJSONParserObjectHelper.fromString(Value: String): TJSONObject;
begin
  result := TJSONParserDefault.Deserializer.StringToJsonObject(Value);
end;

class function TJSONParserObjectHelper.ObjectToJSONString(Value: TObject): string;
begin
  result := TJSONParserDefault.Deserializer.ObjectToJsonString(Value);
end;

class function TJSONParserObjectHelper.ObjectToJSONStringWithBackslash(Value: TObject): string;
begin
  result := TJSONParserDefault.Deserializer(True, True).ObjectToJsonString(Value);
end;

procedure TJSONParserObjectHelper.SaveToFile(AFileName: String);
var
  fileJSON: TStringList;
begin
  fileJSON := TStringList.Create;
  try
    fileJSON.Text := Self.Format;
    fileJSON.SaveToFile(AFileName);
  finally
    fileJSON.Free;
  end;
end;

procedure TJSONParserObjectHelper.toObject(Value: TObject; bUseIgnore: boolean = True);
begin
  TJSONParserDefault.Serializer(bUseIgnore).JsonObjectToObject(Value, Self);
end;

{ TObjectHelper }

procedure TObjectHelper.fromJSONFile(Value: String);
var
  fileJSON: TStringList;
begin
  if not FileExists(Value) then
    raise EFileNotFoundException.CreateFmt('Arquivo %s não encontrado', [Value]);

  fileJSON := TStringList.Create;
  try
    fileJSON.LoadFromFile(Value);

    fromJSONString( fileJSON.Text );
  finally
    fileJSON.Free;
  end;
end;

procedure TObjectHelper.fromJSONObject(Value: TJSONObject);
begin
  if Assigned(Value) then
    Value.toObject(Self);
end;

procedure TObjectHelper.fromJSONString(Value: String);
var
  json : TJSONObject;
begin
  json := TJSONObject.fromString(Value);
  try
    if Assigned(json) then
      fromJSONObject(json);
  finally
    json.Free;
  end;
end;

procedure TObjectHelper.SaveToJSONFile(AFileName: String);
var
  json: TJSONObject;
begin
  json := Self.ToJSONObject;
  try
    json.SaveToFile(AFileName);
  finally
    json.Free;
  end;
end;

function TObjectHelper.ToJSONObject: TJSONObject;
begin
  result := TJSONObject.fromObject(Self);
end;

function TObjectHelper.ToJSONString(bFormat: Boolean = False; aBackslash: Boolean = False): string;
begin
  if aBackslash then
    result := TJSONObject.ObjectToJSONStringWithBackslash(Self)
  else
    result := TJSONObject.ObjectToJSONString(Self);

  if bFormat then
    result := TJSONObject.format(result);
end;

end.
