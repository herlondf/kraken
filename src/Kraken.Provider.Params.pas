unit Kraken.Provider.Params;

interface

uses
  System.Math,
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  DateUtils,
  Kraken.Types,
  FireDAC.Comp.Client;

type
  TKrakenParams = TObjectList<TKrakenParamsClass>;

  TKrakenProviderParams = class
    constructor Create;
    destructor Destroy; override;
  private
    FKrakenParams : TKrakenParams;
    FParamname    : String;

    function  ParamExist(AParam: String; AValue: Variant; AFieldtype: TFieldType): Boolean;
    procedure ParamAdd(AParam: String; AValue: Variant; AFieldtype: TFieldType);
    function  ParamGet(AParamname: String): Variant;
    function  ReplaceWords(AValue: string; AFindWord: string; AReplaceWord: String): string;

    procedure SetAsString    ( const Value: String     );
    procedure SetAsInteger   ( const Value: Integer    );
    procedure SetAsFloat     ( const Value: Extended   );
    procedure SetAsXML       ( const Value: WideString );
    procedure SetAsCurrency  ( const Value: Currency   );
    procedure SetAsBoolean   ( const Value: Boolean    );
    procedure SetAsDate      ( const Value: TDate      );
    procedure SetAsDateTime  ( const Value: TDateTime  );
    procedure SetAsWideString( const Value: WideString );
    procedure SetIsNull      ( const Value: Boolean    );
    procedure SetValue       ( const Value: Variant    );

    function GetAsString     : String;
    function GetAsInteger    : Integer;
    function GetAsFloat      : Extended;
    function GetAsXML        : WideString;
    function GetAsCurrency   : Currency;
    function GetAsBoolean    : Boolean;
    function GetAsDate       : TDate;
    function GetAsDateTime   : TDateTime;
    function GetAsWideString : WideString;
    function GetIsNull       : Boolean;
    function GetValue        : Variant;
  public
    function Count: Integer;
    function Param(const AParam: String): TKrakenProviderParams;
    function ParseSQL(const ASQL: string): String;
    function List: TKrakenParams;

    property AsString     : String     read GetAsString     write SetAsString;
    property AsInteger    : Integer    read GetAsInteger    write SetAsInteger;
    property AsFloat      : Extended   read GetAsFloat      write SetAsFloat;
    property AsXML        : WideString read GetAsXML        write SetAsXML;
    property AsCurrency   : Currency   read GetAsCurrency   write SetAsCurrency;
    property AsBoolean    : Boolean    read GetAsBoolean    write SetAsBoolean;
    property AsDate       : TDate      read GetAsDate       write SetAsDate;
    property AsDateTime   : TDateTime  read GetAsDateTime   write SetAsDateTime;
    property AsWideString : WideString read GetAsWideString write SetAsWideString;
    property IsNull       : Boolean    read GetIsNull       write SetIsNull;
    property Value        : Variant    read GetValue        write SetValue;

    procedure Clear;
  end;

implementation

{ TKrakenProviderParams }

constructor TKrakenProviderParams.Create;
begin
  FKrakenParams := TKrakenParams.Create();
  SetRoundMode(rmNearest);
end;

destructor TKrakenProviderParams.Destroy;
begin
  FreeAndNil(FKrakenParams);
  inherited;
end;

function TKrakenProviderParams.List: TKrakenParams;
begin
  Result := FKrakenParams;
end;

function TKrakenProviderParams.ParamExist(AParam: String; AValue: Variant; AFieldtype: TFieldType): Boolean;
var
  LKrakenParamClass: TKrakenParamsClass;
begin
  Result := False;

  if FParamname = '' then
    raise Exception.Create('Param name not defined.');

  for LKrakenParamClass in FKrakenParams  do
  begin
    Result := AnsiUpperCase( LKrakenParamClass.Name ) = AnsiUpperCase(':'+AParam);

    if Result then
    begin
      LKrakenParamClass.Value    := AValue;
      LKrakenParamClass.Datatype := AFieldtype;

      Break;
    end;

  end;
end;

function TKrakenProviderParams.ParamGet(AParamname: String): Variant;
var
  LKrakenParam: TKrakenParamsClass;
begin
  Result := '';

  for LKrakenParam in FKrakenParams do
  begin
    if AnsiUpperCase( LKrakenParam.Name ) = AnsiUpperCase( ':'+AParamname ) then
    begin
      Result := LKrakenParam.Value;
      Break;
    end;
  end;
end;

procedure TKrakenProviderParams.ParamAdd(AParam: String; AValue: Variant; AFieldtype: TFieldType);
begin
  if not ParamExist(AParam, AValue, AFieldtype) then
  begin
    with FKrakenParams.Items[ FKrakenParams.Add( TKrakenParamsClass.Create ) ] do
    begin
      Name     := ':'+AParam;
      Value    := AValue;
      Datatype := AFieldtype;
    end;

    FParamname := '';
  end;
end;

procedure TKrakenProviderParams.Clear;
begin
  ParamExist(FParamname, '', ftUnknown);
end;

function TKrakenProviderParams.Count: Integer;
begin
  Result := FKrakenParams.Count;
end;

procedure TKrakenProviderParams.SetAsBoolean(const Value: Boolean);
begin
  ParamAdd(FParamname, Value, ftBoolean);
end;

procedure TKrakenProviderParams.SetAsCurrency(const Value: Currency);
begin
  ParamAdd(FParamname, Value, ftCurrency);
end;

procedure TKrakenProviderParams.SetAsDate(const Value: TDate);
begin
  ParamAdd(FParamname, Value, ftDate);
end;

procedure TKrakenProviderParams.SetAsDateTime(const Value: TDateTime);
begin
  ParamAdd(FParamname, Value, ftDateTime);
end;

procedure TKrakenProviderParams.SetAsFloat(const Value: Extended);
begin
  ParamAdd(FParamname, Value, ftFloat);
end;

procedure TKrakenProviderParams.SetAsInteger(const Value: Integer);
begin
  ParamAdd(FParamname, Value, ftInteger);
end;

procedure TKrakenProviderParams.SetAsString(const Value: String);
begin
  ParamAdd(FParamname, Value, ftString);
end;

procedure TKrakenProviderParams.SetAsWideString(const Value: WideString);
begin
  ParamAdd(FParamname, Value, ftWideString);
end;

procedure TKrakenProviderParams.SetAsXML(const Value: WideString);
begin
  ParamAdd(FParamname, Value, ftWideString);
end;

procedure TKrakenProviderParams.SetIsNull(const Value: Boolean);
begin
  //
end;

procedure TKrakenProviderParams.SetValue(const Value: Variant);
begin
  ParamAdd(FParamname, Value, ftVariant);
end;

function TKrakenProviderParams.GetAsBoolean: Boolean;
begin
  Result := ParamGet(FParamname);
end;

function TKrakenProviderParams.GetAsCurrency: Currency;
begin
  Result := ParamGet(FParamname);
end;

function TKrakenProviderParams.GetAsDate: TDate;
begin
  Result := ParamGet(FParamname);
end;

function TKrakenProviderParams.GetAsDateTime: TDateTime;
begin
  Result :=  ParamGet(FParamname);
end;

function TKrakenProviderParams.GetAsFloat: Extended;
begin
  Result := ParamGet(FParamname);
end;

function TKrakenProviderParams.GetAsInteger: Integer;
begin
  Result := ParamGet(FParamname);
end;

function TKrakenProviderParams.GetAsString: String;
begin
  Result := ParamGet(FParamname);
end;

function TKrakenProviderParams.GetAsWideString: WideString;
begin
  Result := ParamGet(FParamname);
end;

function TKrakenProviderParams.GetAsXML: WideString;
begin
  Result := ParamGet(FParamname);
end;

function TKrakenProviderParams.GetIsNull: Boolean;
begin
  Result := True;
end;

function TKrakenProviderParams.GetValue: Variant;
begin
  Result := ParamGet(FParamname);
end;

function TKrakenProviderParams.Param(const AParam: String): TKrakenProviderParams;
begin
  Result     := Self;
  FParamname := AParam;
end;

function TKrakenProviderParams.ParseSQL(const ASQL: string): String;
var
  LKrakenParam : TKrakenParamsClass;
  LSQL         : String;
  LField       : string;
  tmp: TStringList;
begin
  LSQL := ASQL;
  for LKrakenParam in FKrakenParams do
  begin
    case LKrakenParam.Datatype of
      ftFloat, ftInteger, ftCurrency : LField := StringReplace(LKrakenParam.Value, ',', '.', [rfReplaceAll, rfIgnoreCase]);
      ftString, ftWideString         : LField := '''' + LKrakenParam.Value + '''';
      ftDate                         : LField := '''' + DateToStr(LKrakenParam.Value) + '''';
      ftDateTime                     : LField := '''' + DateTimeToStr(LKrakenParam.Value) + '''';
    end;

    if LField <> '' then
      LSQL := ReplaceWords(LSQL, LKrakenParam.Name, LField)
    else
      LSQL := ReplaceWords(LSQL, LKrakenParam.Name, LKrakenParam.Value);

    LField := '';
  end;

  Result := LSQL;
end;

function TKrakenProviderParams.ReplaceWords(AValue: string; AFindWord: string; AReplaceWord: String): string;
var
  LStrings: TStringlist;
  I: Integer;
  LResult: string;
  LWordCompare: string;
  virgula: string;
  parentAntes: string;
  parentDepois:  string;
begin
  LStrings := TStringlist.Create;
  LStrings.Delimiter := ' ';
  LStrings.DelimitedText := AValue;

  for I := 0 to Pred(LStrings.Count) do
  begin
    LWordCompare := LStrings.Strings[I];

    if Pos(',', LWordCompare) > 0 then
    begin
      LWordCompare := Copy(LWordCompare, 0, Pos(',', LWordCompare) -1);
      virgula := ', ';
    end
    else
      virgula := '';

    if Pos('(', LWordCompare) > 0 then
    begin
      LWordCompare := Copy( LWordCompare, Pos('(', LWordCompare) + 1, length(LWordCompare) + 1 );
      parentAntes := '(';
    end
    else
      parentAntes := '';

    if Pos(')', LWordCompare) > 0 then
    begin
      LWordCompare := Copy(LWordCompare, 0, Pos(')', LWordCompare) -1);
      parentDepois := ')';
    end
    else
      parentDepois := '';

    if ( AnsiUpperCase( AFindWord ) = AnsiUpperCase( LWordCompare ) ) then
      LStrings.Strings[I] := parentAntes + AReplaceWord + parentDepois + virgula;

  end;

  LResult := LStrings.Text;

  LStrings.Free;

  result := LResult;
end;

end.
