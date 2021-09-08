unit JSONParser.RTTI;

interface

uses
  System.Rtti,
  System.SysUtils,
  JSONParser.Attributes;

type
  IJsonParserRTTI = interface
    ['{B432A34C-5601-4254-A951-0DE059E73CCE}']
    function GetType(AClass: TClass): TRttiType;
    function FindType(ATypeName: string): TRttiType;
  end;

  TJsonParserRTTI = class(TInterfacedObject, IJsonParserRTTI)
    private
      class var FInstance: IJsonParserRTTI;

    private
      FContext: TRttiContext;

      constructor createPrivate;
    public
      function GetType (AClass: TClass): TRttiType;
      function FindType(ATypeName: string): TRttiType;

      class function GetInstance: IJsonParserRTTI;
      constructor create;
      destructor  Destroy; override;
  end;

  TJsonParserRTTITypeHelper = class helper for TRttiType
    public
      function IsList: Boolean;
  end;

  TJsonParserRTTIPropertyHelper = class helper for TRttiProperty
    public
      function IsList     : Boolean;
      function IsString   : Boolean;
      function IsInteger  : Boolean;
      function IsEnum     : Boolean;
      function IsArray    : Boolean;
      function IsObject   : Boolean;
      function IsFloat    : Boolean;
      function IsDateTime : Boolean;
      function IsBoolean  : Boolean;
      function IsVariant  : Boolean;

      function GetAttribute<T: TCustomAttribute>: T;

      function IsEmpty(AObject: TObject): Boolean;

      function IsIgnore(AClass: TClass): Boolean;

      function GetListType(AObject: TObject): TRttiType;
  end;

  TJsonParserObjectHelper = class helper for TObject
    public
      function invokeMethod(const MethodName: string; const Parameters: array of TValue): TValue;
      function GetPropertyValue(Name: String): TValue;

      class function GetAttribute<T: TCustomAttribute>: T;

      class function JsonIgnoreFields: TArray<String>;
  end;

implementation

{ TJsonParserRTTI }

constructor TJsonParserRTTI.create;
begin
  raise Exception.Create('Utilize o Construtor GetInstance.');
end;

constructor TJsonParserRTTI.createPrivate;
begin
  FContext := TRttiContext.Create;
end;

destructor TJsonParserRTTI.Destroy;
begin
  FContext.Free;
  inherited;
end;

function TJsonParserRTTI.FindType(ATypeName: string): TRttiType;
begin
  Result := FContext.FindType(ATypeName);
end;

class function TJsonParserRTTI.GetInstance: IJsonParserRTTI;
begin
  if not Assigned(FInstance) then
    FInstance := TJsonParserRTTI.createPrivate;
  result := FInstance;
end;

function TJsonParserRTTI.GetType(AClass: TClass): TRttiType;
begin
  result := FContext.GetType(AClass);
end;

{ TJsonParserRTTITypeHelper }

function TJsonParserRTTITypeHelper.IsList: Boolean;
begin
  result := False;

  if Self.AsInstance.Name.ToLower.StartsWith('tobjectlist<') then
    Exit(True);

  if Self.AsInstance.Name.ToLower.StartsWith('tlist<') then
    Exit(True);
end;

{ TJsonParserRTTIPropertyHelper }

function TJsonParserRTTIPropertyHelper.GetAttribute<T>: T;
var
  i: Integer;
begin
  result := nil;
  for i := 0 to Pred(Length(Self.GetAttributes)) do
    if Self.GetAttributes[i].ClassNameIs(T.className) then
      Exit(T( Self.GetAttributes[i]));
end;

function TJsonParserRTTIPropertyHelper.GetListType(AObject: TObject): TRttiType;
var
  ListType     : TRttiType;
  ListTypeName : string;
begin
  ListType := TJsonParserRTTI.GetInstance.GetType(Self.GetValue(AObject).AsObject.ClassType);
  ListTypeName := ListType.ToString;

  ListTypeName := ListTypeName.Replace('TObjectList<', EmptyStr);
  ListTypeName := ListTypeName.Replace('TList<', EmptyStr);
  ListTypeName := ListTypeName.Replace('>', EmptyStr);

  result := TJsonParserRTTI.GetInstance.FindType(ListTypeName);
end;

function TJsonParserRTTIPropertyHelper.IsArray: Boolean;
begin
  Result := Self.PropertyType.TypeKind in
    [tkSet, tkArray, tkDynArray]
end;

function TJsonParserRTTIPropertyHelper.IsBoolean: Boolean;
begin
  result := Self.PropertyType.ToString.ToLower.Equals('boolean');
end;

function TJsonParserRTTIPropertyHelper.IsDateTime: Boolean;
begin
  result := (Self.PropertyType.ToString.ToLower.Equals('tdatetime')) or
             (Self.PropertyType.ToString.ToLower.Equals('tdate')) or
             (Self.PropertyType.ToString.ToLower.Equals('ttime'));
end;

function TJsonParserRTTIPropertyHelper.IsEmpty(AObject: TObject): Boolean;
var
  objectList : TObject;
begin
  result := False;

  if (Self.IsString) and (Self.GetValue(AObject).AsString.IsEmpty) then
    Exit(True);

  if (Self.IsInteger) and (Self.GetValue(AObject).AsInteger = 0) then
    Exit(True);

  if (Self.IsObject) and (Self.GetValue(AObject).AsObject = nil) then
    Exit(True);

  if (Self.IsArray) and (Self.GetValue(AObject).GetArrayLength = 0) then
    Exit(True);

  if (Self.IsList) then
  begin
    objectList := Self.GetValue(AObject).AsObject;
    if objectList.GetPropertyValue('Count').AsInteger = 0 then
      Exit(True);
  end;

  if (Self.IsFloat) and (Self.GetValue(AObject).AsExtended = 0) then
    Exit(True);

  if (Self.IsDateTime) and (Self.GetValue(AObject).AsExtended = 0) then
    Exit(True);
end;

function TJsonParserRTTIPropertyHelper.IsEnum: Boolean;
begin
  result := (not IsBoolean) and (Self.PropertyType.TypeKind = tkEnumeration);
end;

function TJsonParserRTTIPropertyHelper.IsFloat: Boolean;
begin
  result := (Self.PropertyType.TypeKind = tkFloat) and (not IsDateTime);
end;

function TJsonParserRTTIPropertyHelper.IsIgnore(AClass: TClass): Boolean;
var
  ignoreProperties: TArray<String>;
  i: Integer;
begin
  ignoreProperties := AClass.JsonIgnoreFields;
  for i := 0 to Pred(Length(ignoreProperties)) do
  begin
    if Name.ToLower.Equals(ignoreProperties[i].ToLower) then
      Exit(True);
  end;

  result := Self.GetAttribute<JSONIgnore> <> nil;
  if not Result then
  begin
    if AClass.InheritsFrom(TInterfacedObject) then
      result := Self.Name.ToLower.Equals('refcount');
  end;

  if not Result then
  begin
    for i := 0 to Pred(Length(Self.GetAttributes)) do
    begin
      if GetAttributes[i].ClassNameIs('SwagIgnore') then
        Exit(True);
    end;
  end;
end;

function TJsonParserRTTIPropertyHelper.IsInteger: Boolean;
begin
  result := Self.PropertyType.TypeKind in [tkInt64, tkInteger];
end;

function TJsonParserRTTIPropertyHelper.IsList: Boolean;
begin
  Result := False;

  if Self.PropertyType.ToString.ToLower.StartsWith('tobjectlist<') then
    Exit(True);

  if Self.PropertyType.ToString.ToLower.StartsWith('tlist<') then
    Exit(True);
end;

function TJsonParserRTTIPropertyHelper.IsObject: Boolean;
begin
  result := (not IsList) and (Self.PropertyType.TypeKind = tkClass);
end;

function TJsonParserRTTIPropertyHelper.IsString: Boolean;
begin
  result := Self.PropertyType.TypeKind in
    [tkChar,
     tkString,
     tkWChar,
     tkLString,
     tkWString,
     tkUString];
end;

function TJsonParserRTTIPropertyHelper.IsVariant: Boolean;
begin
  result := Self.PropertyType.TypeKind = tkVariant;
end;

{ TJsonParserObjectHelper }

class function TJsonParserObjectHelper.GetAttribute<T>: T;
var
  i: Integer;
  rType: TRttiType;
begin
  result := nil;
  rType  := TJsonParserRTTI.GetInstance.GetType(Self);

  for i := 0 to Pred(Length(rType.GetAttributes)) do
    if rType.GetAttributes[i].ClassNameIs(T.className) then
      Exit(T( rType.GetAttributes[i]));
end;

function TJsonParserObjectHelper.GetPropertyValue(Name: String): TValue;
var
  rttiProp: TRttiProperty;
begin
  rttiProp := TJsonParserRTTI.GetInstance.GetType(Self.ClassType)
                .GetProperty(Name);

  if Assigned(rttiProp) then
    result := rttiProp.GetValue(Self);
end;

function TJsonParserObjectHelper.invokeMethod(const MethodName: string; const Parameters: array of TValue): TValue;
var
  rttiType: TRttiType;
  method  : TRttiMethod;
begin
  rttiType := TJsonParserRTTI.GetInstance.GetType(Self.ClassType);
  method   := rttiType.GetMethod(MethodName);

  if not Assigned(method) then
    raise ENotImplemented.CreateFmt('Cannot find method %s in %s', [MethodName, Self.ClassName]);

  result := method.Invoke(Self, Parameters);
end;

class function TJsonParserObjectHelper.JsonIgnoreFields: TArray<String>;
var
  ignore: JSONIgnore;
begin
  result := [];
  ignore := GetAttribute<JSONIgnore>;

  if Assigned(ignore) then
    result := ignore.IgnoreProperties;
end;

end.
