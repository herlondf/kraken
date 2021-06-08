unit Brave.Provider.Firedac.Metadata;

interface

uses
  Data.DB,
  System.Classes,
  System.SysUtils,
  System.Contnrs,
  System.Diagnostics,
  System.Timespan,
  System.StrUtils,

  FireDAC.Comp.Client,
  FireDAC.Phys.Intf,

  {BraveDAC}
  Brave.Consts,
  Brave.Provider.Firedac.Metadata.Entity;

type
  TBraveProviderFiredacMetadata = class(TFDMetaInfoQuery)
    constructor Create(AConnection: TFDConnection);
    destructor Destroy; override;
  strict private
    FConnection: TFDConnection;

    FOnLog     : TNotifyEvent;
    procedure SetOnLog(const Value: TNotifyEvent);
  private
    FEntitySchemas    : TBraveEntitySchemas;
    FEntityTables     : TBraveEntityTables;
    FEntityColumns    : TBraveEntityColumns;
    FEntityPKs        : TBraveEntityPKs;
    FPKFields   : TBraveEntityPKFields;
    FEntityFKs        : TBraveEntityFKs;
    FEntityFKFields   : TBraveEntityFKFields;
    FEntityGenerators : TBraveEntityGenerators;
  public
    function EntitySchemas    : TBraveEntitySchemas;
    function EntityTables     : TBraveEntityTables;
    function EntityColumns    : TBraveEntityColumns;
    function EntityPKs        : TBraveEntityPKs;
    function EntityPKFields   : TBraveEntityPKFields;
    function EntityFKs        : TBraveEntityFKs;
    function EntityFKFields   : TBraveEntityFKFields;
    function EntityGenerators : TBraveEntityGenerators;

    procedure Schemas(const ASchema: String = '');

    procedure Tables ( const ASchema: String; const ATable: String = '' );
    procedure Columns( const ASchema: String; const ATable: String; const AColumn: String = '' );

    procedure PK(const ASchema: String = 'public'; const ATable: String = '');
    procedure PKFields(const ASchema: String = 'public'; ATable: String = '' );

    function FK(const ATable: String; ASchema: String = 'public'): String;
    function FKFields(ATable: String; ASchema: String = 'public'; AConstraint: String = ''): String;

    procedure Generators(const ASchema: String);
  end;

implementation

{ TBraveProviderFiredacMetadata }

constructor TBraveProviderFiredacMetadata.Create(AConnection: TFDConnection);
begin
  FConnection := AConnection;
  Self.Connection := FConnection;
end;

destructor TBraveProviderFiredacMetadata.Destroy;
begin
  if Assigned( FEntitySchemas ) then
    FEntitySchemas.Free;

  if Assigned( FEntityTables ) then
    FEntityTables.Free;

  if Assigned( FEntityColumns ) then
    FEntityColumns.Free;

  if Assigned( FEntityPKs ) then
    FEntityPKs.Free;

  if Assigned( FPKFields ) then
    FPKFields.Free;

  if Assigned( FEntityFKs ) then
    FEntityFKs.Free;

  if Assigned( FEntityFKFields ) then
    FEntityFKFields.Free;

  if Assigned( FEntityGenerators ) then
    FEntityGenerators.Free;


  inherited;
end;

function TBraveProviderFiredacMetadata.EntityTables: TBraveEntityTables;
begin
  if not Assigned( FEntityTables ) then
    FEntityTables := TBraveEntityTables.Create();

  Result := FEntityTables;
end;

function TBraveProviderFiredacMetadata.EntityColumns: TBraveEntityColumns;
begin
  if not Assigned( FEntityColumns ) then
    FEntityColumns := TBraveEntityColumns.Create();

  Result := FEntityColumns;
end;

function TBraveProviderFiredacMetadata.EntityPKs: TBraveEntityPKs;
begin
  if not Assigned( FEntityPKs ) then
    FEntityPKs := TBraveEntityPKs.Create();

  Result := FEntityPKs;
end;

function TBraveProviderFiredacMetadata.EntitySchemas: TBraveEntitySchemas;
begin
  if not Assigned( FEntitySchemas ) then
    FEntitySchemas := TBraveEntitySchemas.Create();

  Result := FEntitySchemas;
end;

function TBraveProviderFiredacMetadata.EntityPKFields: TBraveEntityPKFields;
begin
  if not Assigned( FPKFields ) then
    FPKFields := TBraveEntityPKFields.Create();

  Result := FPKFields;
end;

function TBraveProviderFiredacMetadata.EntityFKs: TBraveEntityFKs;
begin
  if not Assigned( FEntityFKs ) then
    FEntityFKs := TBraveEntityFKs.Create();

  Result := FEntityFKs;
end;

function TBraveProviderFiredacMetadata.EntityGenerators: TBraveEntityGenerators;
begin
  if not Assigned( FEntityGenerators ) then
    FEntityGenerators := TBraveEntityGenerators.Create();

  Result := FEntityGenerators;
end;

function TBraveProviderFiredacMetadata.EntityFKFields: TBraveEntityFKFields;
begin
  if not Assigned( FEntityFKFields ) then
    FEntityFKFields := TBraveEntityFKFields.Create();

  Result := FEntityFKFields;
end;

procedure TBraveProviderFiredacMetadata.SetOnLog(const Value: TNotifyEvent);
begin
  FOnLog := Value;
end;

procedure TBraveProviderFiredacMetadata.Schemas(const ASchema: String = '');
begin
  Close;
  MetaInfoKind := mkSchemas;
  TableKinds   := [tkSynonym,tkTable,tkView];
  ObjectName   := ASchema;

  EntitySchemas.Clear;

  try
    try
      Active := False;
      Open();

      while not Eof do
      begin
        if ASchema <> '' then
        begin
          if AnsiUpperCase( FieldByName( SCHEMA_NAME ).AsString ) = AnsiUpperCase( ASchema ) then
          begin
            with EntitySchemas.Items[ EntitySchemas.Add( TBraveEntitySchema.Create ) ] do
            begin
              Name := FieldByName( SCHEMA_NAME ).AsString;

              Break;
            end;
          end;
        end
        else
        begin
          with EntitySchemas.Items[ EntitySchemas.Add( TBraveEntitySchema.Create ) ] do
          begin
            Name := FieldByName( SCHEMA_NAME ).AsString;
          end;
        end;

        Next;
      end;
    finally

    end;
  except

  end;
end;


procedure TBraveProviderFiredacMetadata.Tables( const ASchema: String; const ATable: String = '' );
var
  LField : TField;
  LDebug: String;
  I: Integer;
begin
  Close;
  MetaInfoKind := mkTables;
  TableKinds   := [tkTable];
  SchemaName   := ASchema;

  EntityTables.Clear;

  try
    try
      if ATable <> '' then
        ObjectName := ATable;

      Active := False;
      Open();

      while not Eof do
      begin
        if ATable <> '' then
        begin
          if AnsiUpperCase( FieldByName( TABLE_NAME ).AsString ) = AnsiUpperCase( ATable ) then
          begin
            with EntityTables.Items[ EntityTables.Add( TBraveEntityTable.Create ) ] do
            begin
              Name   := FieldByName( TABLE_NAME ).AsString;

              Break;
            end;
          end;
        end
        else
        begin
          with EntityTables.Items[ EntityTables.Add( TBraveEntityTable.Create ) ] do
          begin
            Name := FieldByName( TABLE_NAME ).AsString;
          end;
        end;

        Next;
      end;
    finally

    end;
  except

  end;
end;

procedure TBraveProviderFiredacMetadata.Columns( const ASchema: String; const ATable: String; const AColumn: String = '' );
var
  L: String;
begin
  Close;
  MetaInfoKind   := mkTableFields;
  TableKinds     := [tkSynonym,tkTable,tkView];
  SchemaName     := ASchema;
  BaseObjectName := ATable;
  ObjectName     := ATable;

  EntityColumns.Clear;

  try
    try
      Active := False;
      Open();

      while not Eof do
      begin
        if AColumn <> '' then
        begin
          if AnsiUpperCase( FieldByName( COLUMN_NAME ).AsString ) = AnsiUpperCase( AColumn ) then
          begin
            with EntityColumns.Items[ EntityColumns.Add( TBraveEntityColumn.Create ) ] do
            begin
              Name      := FieldByName( COLUMN_NAME      ).AsString;
              Typename  := FieldByName( COLUMN_TYPENAME  ).AsString;
              Precision := FieldByName( COLUMN_PRECISION ).AsString;
              Scale     := FieldByName( COLUMN_SCALE     ).AsString;
              Length    := FieldByName( COLUMN_LENGTH    ).AsString;

              Break;
            end;
          end;
        end
        else
        begin
          with EntityColumns.Items[ EntityColumns.Add( TBraveEntityColumn.Create ) ] do
          begin
            Name      := FieldByName( COLUMN_NAME      ).AsString;
            Typename  := FieldByName( COLUMN_TYPENAME  ).AsString;
            Precision := FieldByName( COLUMN_PRECISION ).AsString;
            Scale     := FieldByName( COLUMN_SCALE     ).AsString;
            Length    := FieldByName( COLUMN_LENGTH    ).AsString;
          end;
        end;

        Next;
      end;
    finally

    end;
  except

  end;
end;

procedure TBraveProviderFiredacMetadata.PK(const ASchema: String = 'public'; const ATable: String = '');
var
  LTEste: String;
begin
  Close;
  MetaInfoKind   := mkPrimaryKey;
  TableKinds     := [tkTable];
  SchemaName     := ASchema;
  ObjectName     := ATable;

  EntityPKs.Clear;

  try
    try
      Active := False;
      Active := True;

      while not eof do
      begin
        with EntityPKs.Items[ EntityPKs.Add( TBraveEntityPK.Create ) ] do
        begin
          Name    := FieldByName( CONSTRAINT_NAME  ).AsString;
          Table   := FieldByName( TABLE_NAME       ).AsString;
        end;

        Next;
      end;

    finally

    end;
  except

  end;

end;

procedure TBraveProviderFiredacMetadata.PKFields( const ASchema: String = 'public'; ATable: String = '' );
begin
  Close;
  MetaInfoKind   := mkPrimaryKeyFields;
  TableKinds     := [tkSynonym,tkTable,tkView];
  SchemaName     := ASchema;
  BaseObjectName := ATable;

  EntityPKFields.Clear;

  try
    Active := False;
    Open();

    try
      while not Eof do
      begin
        with EntityPKFields.Items[ EntityPKFields.Add( TBraveEntityPKField.Create ) ] do
        begin
          Name    := FieldByName( INDEX_NAME  ).AsString;
          Column  := FieldByName( COLUMN_NAME ).AsString;
        end;

        Next;
      end;
    finally

    end;
  except

  end;
end;

function TBraveProviderFiredacMetadata.FK(const ATable: String; ASchema: String = 'public'): String;
var
  LFKField: TBraveEntityFK;
 LTeste : String;
  I: Integer;
begin
  Close;
  MetaInfoKind   := mkForeignKeys;
  TableKinds     := [tkTable];

  if ASchema = '' then
    ASchema := 'public';

  SchemaName     := ASchema;
  BaseObjectName := ATable;
  ObjectName     := ATable;

  EntityFKs.Clear;

  try
    Active := False;
    Open();

    try
      while not Eof do
      begin
        with EntityFKs.Items[ EntityFKs.Add( TBraveEntityFK.Create ) ] do
        begin
          for I := 1 to FieldList.Count do
          begin
            LTeste := FieldByNumber(I).DisplayName;
            LTeste := FieldByNumber(I).AsString;

          end;

          NameFK  := FieldByName( FKEY_NAME       ).AsString;
          TableFK := FieldByName( PKEY_TABLE_NAME ).AsString;
        end;

        Next;
      end;
    finally

    end;
  except

  end;
end;

function TBraveProviderFiredacMetadata.FKFields(ATable: String; ASchema: String = 'public'; AConstraint: String = ''): String;
var
  LField : TField;
  LFKField: TBraveEntityFKField;
  LContinue: Boolean;
  LTeste : String;
  I: Integer;
begin
  Close;
  MetaInfoKind   := mkForeignKeyFields;
  TableKinds     := [tkTable];
  SchemaName     := ASchema;
  BaseObjectName := ATable;
  ObjectName     := AConstraint;

  EntityFKFields.Clear;

  try
    Active := False;
    Open();

    try
      while not Eof do
      begin
        LContinue := False;

//        for LFKField in EntityFKFields do
//          if LFKField.ColumnFK = FieldByName( PKEY_COLUMN_NAME ).AsString then
//          begin
//            LContinue := True;
//            Next;
//            Break;
//          end;


        if LContinue then
          Continue;

        with EntityFKFields.Items[ EntityFKFields.Add( TBraveEntityFKField.Create ) ] do
        begin
          for I := 1 to FieldList.Count do
          begin
            LTeste := FieldByNumber(I).DisplayName;
            LTeste := FieldByNumber(I).AsString;

          end;


          Name     := FieldByName( FKEY_NAME        ).AsString;
          TableFK  := FieldByName( TABLE_NAME       ).AsString;
          Column   := FieldByName( COLUMN_NAME      ).AsString;
          ColumnFK := FieldByName( PKEY_COLUMN_NAME ).AsString;
        end;

        Next;
      end;
    finally

    end;
  except

  end;
end;

procedure TBraveProviderFiredacMetadata.Generators(const ASchema: String);
begin
  Close;
  MetaInfoKind   := mkGenerators;
  TableKinds     := [tkTable];
  SchemaName     := ASchema;

  EntityGenerators.Clear;

  try
    Active := False;
    Open();

    try
      while not Eof do
      begin
        with EntityGenerators.Items[ EntityGenerators.Add( TBraveEntityGenerator.Create ) ] do
        begin

          Name := FieldByName( GENERATOR_NAME ).AsString;
        end;

        Next;
      end;
    finally

    end;
  except

  end;
end;

end.

