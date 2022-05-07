unit Pattern.RTTI;

interface

uses
  {Embarcadero}
  System.Classes,
  System.SysUtils,
  Vcl.Forms,
  Vcl.Controls,

  {RTTI}
  RTTI,
  typinfo,

  {Brave}
  Logger,

  {Pattern}
  Pattern.CustomAttributes;

type
  TPatternRTTI = class
  strict private

  private


  public
    class function ListMethods      (const AObject : TObject                 ): TArray<TRttiMethod>;
    class function ListMethodsParams(const AObject: TObject; AMethod: String ): TArray<TRttiParameter>;
    class function FindMethods      (const AObject: TObject; AMethod: String ): TRTTIMethod;
    class function ExecuteMethods   (const AObject: TObject; const AMethod: string; AParams: TArrayOfValue): TValue;

    class function ListObjectFields      ( const AObject: TObject                       ): TArray<TRttiField>;
    class function FindObjectFields      ( const AObject: TObject; const AField: String ): TRttiField;
    class function FindObjectFieldsValue ( const AObject: TObject; const AField: String ): Variant;
    class function FindObject            ( const AObject: TObject; const AField: String ): TObject;
    class function FindAttributeSearch   ( const AObject: TObject; const AField: String = '' ): String;

    class function FindCustomAttriburesByField(
        const AObject         : TObject;
        const AField          : String;
        const ACustomAttribute: TCustomAttribute
      ): TCustomAttribute;
  end;

implementation

{ TPatternRTTI }

class function TPatternRTTI.FindAttributeSearch( const AObject: TObject; const AField: String = '' ): String;
var
  LResult        : String;
  LRTTIType      : TRttiType;
  LRTTIContext   : TRttiContext;
  LRTTIField     : TRttiField;
  LRTTIMethod    : TRttiMethod;
  LRTTIProperty  : TRttiProperty;
  LRTTIAttributes: TCustomAttribute;
begin
  LResult    := '';
  LRTTIType  := LRTTIContext.GetType( TObject( AObject ).ClassType );

  if AField <> '' then
  begin

    LRTTIField := LRTTIType.GetField( AField );

    if LRTTIField <> nil then
    begin
      for LRTTIAttributes in  LRTTIField.GetAttributes do
      begin
        if LRTTIAttributes is Search  then
          LResult := Search(LRTTIAttributes).Value;

        if LRTTIAttributes is CastToVarchar  then
          LResult := LResult  Search(LRTTIAttributes).Value  '::::varchar';

        if LRTTIAttributes is CastToDate then
          LResult := 'to_char('  LResult  ', ''DD/MM/YYYY'')';
      end;
    end
    else
      LResult := '';

  end
  else
  begin
    for LRTTIField in LRTTIType.GetFields do
    begin
      for LRTTIAttributes in  LRTTIField.GetAttributes do
      begin
        if LRTTIAttributes is Search  then
          LResult := LResult  Search(LRTTIAttributes).KeyWord  ';';
      end;
    end;
  end;

  Result := LResult;
end;

class function TPatternRTTI.FindCustomAttriburesByField(
    const AObject         : TObject;
    const AField          : String;
    const ACustomAttribute: TCustomAttribute
  ): TCustomAttribute;
var
  LResult        : String;
  LRTTIType      : TRttiType;
  LRTTIContext   : TRttiContext;
  LRTTIField     : TRttiField;
  LRTTIAttributes: TCustomAttribute;
begin
  LResult    := '';
  LRTTIType  := LRTTIContext.GetType( AObject.ClassType );

  LRTTIField := LRTTIType.GetField( AField );

//  if LRTTIField <> nil then
//  begin
//    for LRTTIAttributes in  LRTTIField.GetAttributes do
//    begin
//      if LRTTIAttributes is Search  then
//      begin
//        LResult := LRTTIAttributes;
//        Break;
//      end;
//    end;
//  end
//  else
//    LResult := nil;
//
//  Result := LResult;
end;

class function TPatternRTTI.FindMethods(const AObject: TObject; AMethod: String): TRTTIMethod;
var
  LRTTIType      : TRttiType;
  LRTTIContext   : TRttiContext;
begin
  LRTTIType := LRTTIContext.GetType( TObject( AObject ).ClassType );

  Result := LRTTIType.GetMethod(AMethod);
end;

class function TPatternRTTI.ListMethods(const AObject : TObject): TArray<TRttiMethod>;
var
  LRTTIContext: TRttiContext;
  LRTTIType   : TRttiType;
  LRTTIMethod : TRttiMethod;
begin
  try
    LRTTIType := LRTTIContext.GetType( AObject.ClassType );

    for LRTTIMethod in LRTTIType.GetMethods do
    begin
      //para retornar somente os métodos da classe em questão
      if LRTTIMethod.Parent.Name <> AObject.ClassName then
        Continue;

    end;
  finally
    Result := LRTTIType.GetMethods;
  end;
end;

class function TPatternRTTI.ListMethodsParams(const AObject: TObject; AMethod: String): TArray<TRttiParameter>;
var
  LRTTIType   : TRttiType;
  LRTTIContext: TRttiContext;
  LRTTIMethod : TRttiMethod;
  LRRTIParam  : TRttiParameter;
begin
  try
    LRTTIType := LRTTIContext.GetType( AObject.ClassType );

    {Busca o metodo do objeto}
    LRTTIMethod := FindMethods( AObject, AMethod );

    if Assigned(LRTTIMethod) then
    begin
      for LRRTIParam in LRTTIMethod.GetParameters do
      begin


      end;
    end;
  finally
    if Assigned( LRTTIMethod ) then
      Result := LRTTIMethod.GetParameters;
  end;
end;

class function TPatternRTTI.ExecuteMethods(const AObject: TObject; const AMethod: string; AParams: TArrayOfValue): TValue;
var
  LRTTIType   : TRttiType;
  LRTTIContext: TRttiContext;
  LRTTIMethod : TRttiMethod;
  LRRTIParam  : TRttiParameter;
  LValue      : TValue;
begin
  LRTTIType := LRTTIContext.GetType( AObject.ClassType );

  try
    {Busca o metodo do objeto}
    LRTTIMethod := FindMethods( AObject, AMethod );

    if Assigned(LRTTIMethod) then
      LValue := LRTTIMethod.Invoke(AObject, AParams);
  finally

  end;
end;

class function TPatternRTTI.ListObjectFields(const AObject: TObject): TArray<TRttiField>;
var
  LRTTIType   : TRttiType;
  LRTTIContext: TRttiContext;
  LRTTIField  : TRttiField;
begin
  try
    LRTTIType := LRTTIContext.GetType( AObject.ClassType );

    try
      for LRTTIField in LRTTIType.GetFields do
      begin
        if LRTTIField.GetValue(AObject).IsArray then

        else
        if LRTTIField.GetValue(AObject).IsObject then
             ListObjectFields( LRTTIField.GetValue(AObject).AsObject )
        else

      end;
    except
    end;
  finally
    Result := LRTTIType.GetFields;
  end;
end;

class function TPatternRTTI.FindObjectFields( const AObject: TObject; const AField: String       ): TRttiField;
var
  LRTTIType      : TRttiType;
  LRTTIContext   : TRttiContext;
  LRTTIField     : TRttiField;
  LRTTIMethod    : TRttiMethod;
  LRTTIProperty  : TRttiProperty;
  LRTTIAttributes: TCustomAttribute;
begin
  LRTTIType := LRTTIContext.GetType( TObject( AObject ).ClassType );

  Result := LRTTIType.GetField(AField);

  if Assigned(Result) then
  begin

  end;
end;

class function TPatternRTTI.FindObjectFieldsValue( const AObject : TObject; const AField : String ): Variant;
var
  LRTTIField: TRttiField;
begin
  try
    try
      LRTTIField := FindObjectFields(AObject, AField);

      if Assigned(LRTTIField) then
      begin

      end;
    except
    end;
  finally
    if Assigned(LRTTIField) then
      Result     := LRTTIField.GetValue(AObject).ToString;

  end;
end;

class function TPatternRTTI.FindObject(const AObject: TObject; const AField: String): TObject;
var
  LRTTIField: TRttiField;
begin
  try
    try
      LRTTIField := FindObjectFields(AObject, AField);

      if Assigned(LRTTIField) then
      begin
      end;
    except
    end;
  finally
    if Assigned(LRTTIField) then
      Result := LRTTIField.GetValue(AObject).AsObject;

  end;
end;

end.