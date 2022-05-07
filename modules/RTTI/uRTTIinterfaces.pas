unit uRTTIinterfaces;

interface

uses
  {Basics}
  System.Classes,
  System.Generics.Collections,

  {RTTI}
  RTTI,
  typinfo;

type
  TArrayOfValue = Array of TValue;

  iKRTTI = interface
    ['{46B061ED-A2DA-4CC9-80EB-E2DB4D870A9C}']
    function ListMethods(const AObject : TObject): TArray<TRttiMethod>;
    function ListMethodsParams(const AObject: TObject; AMethod: String): TArray<TRttiParameter>;
    function FindMethods(const AObject: TObject; AMethod: String): TRTTIMethod;
    function ExecuteMethods(const AObject: TObject; const AMethod: string; AParams: TArrayOfValue): TValue;

    function ListObjectFields      ( const AObject: TObject                       ): TArray<TRttiField>;
    function FindObjectFields      ( const AObject: TObject; const AField: String ): TRttiField;
    function FindObjectFieldsValue ( const AObject: TObject; const AField: String ): Variant;
    function FindObject            ( const AObject: TObject; const AField: String ): TObject;
    function FindAttributeSearch   ( const AObject: TObject ): TDictionary<String, String>; overload;
    function FindAttributeSearch   ( const AObject: TObject; const AField: String ): String; overload;

    //function CloneFrom<T: class>(Source: T): T; static;
    //function Clone(const Parameters:Array of TValue): TObject;
  end;

implementation

end.
