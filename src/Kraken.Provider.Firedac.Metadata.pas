unit Kraken.Provider.Firedac.Metadata;

interface

uses
  Data.DB,
  System.Classes,
  System.SysUtils,
  System.Contnrs,
  System.Diagnostics,
  System.Timespan,
  System.StrUtils,

  {FireDAC}
  FireDAC.Comp.Client,
  FireDAC.Phys.Intf,

  {Kraken}
  Kraken.Consts,
  Kraken.Provider.Firedac.Metadata.Entity;

type
  TKrakenProviderFiredacMetadata = class(TFDMetaInfoQuery)
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  private
    FId               : String;
    FEntitySchemas    : TKrakenEntitySchemas;
    FEntityTables     : TKrakenEntityTables;
    FEntityColumns    : TKrakenEntityColumns;
    FEntityPKs        : TKrakenEntityPKs;
    FPKFields         : TKrakenEntityPKFields;
    FEntityFKs        : TKrakenEntityFKs;
    FEntityFKFields   : TKrakenEntityFKFields;
    FEntityGenerators : TKrakenEntityGenerators;
  public
    function Id(const Value: String): TKrakenProviderFiredacMetadata; overload;
    function Id: String; overload;

    function EntitySchemas    : TKrakenEntitySchemas;
    function EntityTables     : TKrakenEntityTables;
    function EntityColumns    : TKrakenEntityColumns;
    function EntityPKs        : TKrakenEntityPKs;
    function EntityPKFields   : TKrakenEntityPKFields;
    function EntityFKs        : TKrakenEntityFKs;
    function EntityFKFields   : TKrakenEntityFKFields;
    function EntityGenerators : TKrakenEntityGenerators;

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

{ TKrakenProviderFiredacMetadata }

constructor TKrakenProviderFiredacMetadata.Create(AOwner: TComponent);
begin
  Inherited Create(AOwner);

  Connection  := TFDConnection( AOwner );
end;

destructor TKrakenProviderFiredacMetadata.Destroy;
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

function TKrakenProviderFiredacMetadata.EntityTables: TKrakenEntityTables;
begin
  if not Assigned( FEntityTables ) then
    FEntityTables := TKrakenEntityTables.Create();

  Result := FEntityTables;
end;

function TKrakenProviderFiredacMetadata.EntityColumns: TKrakenEntityColumns;
begin
  if not Assigned( FEntityColumns ) then
    FEntityColumns := TKrakenEntityColumns.Create();

  Result := FEntityColumns;
end;

function TKrakenProviderFiredacMetadata.EntityPKs: TKrakenEntityPKs;
begin
  if not Assigned( FEntityPKs ) then
    FEntityPKs := TKrakenEntityPKs.Create();

  Result := FEntityPKs;
end;

function TKrakenProviderFiredacMetadata.EntitySchemas: TKrakenEntitySchemas;
begin
  if not Assigned( FEntitySchemas ) then
    FEntitySchemas := TKrakenEntitySchemas.Create();

  Result := FEntitySchemas;
end;

function TKrakenProviderFiredacMetadata.EntityPKFields: TKrakenEntityPKFields;
begin
  if not Assigned( FPKFields ) then
    FPKFields := TKrakenEntityPKFields.Create();

  Result := FPKFields;
end;

function TKrakenProviderFiredacMetadata.EntityFKs: TKrakenEntityFKs;
begin
  if not Assigned( FEntityFKs ) then
    FEntityFKs := TKrakenEntityFKs.Create();

  Result := FEntityFKs;
end;

function TKrakenProviderFiredacMetadata.EntityGenerators: TKrakenEntityGenerators;
begin
  if not Assigned( FEntityGenerators ) then
    FEntityGenerators := TKrakenEntityGenerators.Create();

  Result := FEntityGenerators;
end;

function TKrakenProviderFiredacMetadata.EntityFKFields: TKrakenEntityFKFields;
begin
  if not Assigned( FEntityFKFields ) then
    FEntityFKFields := TKrakenEntityFKFields.Create();

  Result := FEntityFKFields;
end;

procedure TKrakenProviderFiredacMetadata.Schemas(const ASchema: String = '');
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
            with EntitySchemas.Items[ EntitySchemas.Add( TKrakenEntitySchema.Create ) ] do
            begin
              Name := FieldByName( SCHEMA_NAME ).AsString;

              Break;
            end;
          end;
        end
        else
        begin
          with EntitySchemas.Items[ EntitySchemas.Add( TKrakenEntitySchema.Create ) ] do
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


procedure TKrakenProviderFiredacMetadata.Tables( const ASchema: String; const ATable: String = '' );
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
            with EntityTables.Items[ EntityTables.Add( TKrakenEntityTable.Create ) ] do
            begin
              Name   := FieldByName( TABLE_NAME ).AsString;

              Break;
            end;
          end;
        end
        else
        begin
          with EntityTables.Items[ EntityTables.Add( TKrakenEntityTable.Create ) ] do
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

procedure TKrakenProviderFiredacMetadata.Columns( const ASchema: String; const ATable: String; const AColumn: String = '' );
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
            with EntityColumns.Items[ EntityColumns.Add( TKrakenEntityColumn.Create ) ] do
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
          with EntityColumns.Items[ EntityColumns.Add( TKrakenEntityColumn.Create ) ] do
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

procedure TKrakenProviderFiredacMetadata.PK(const ASchema: String = 'public'; const ATable: String = '');
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
        with EntityPKs.Items[ EntityPKs.Add( TKrakenEntityPK.Create ) ] do
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

procedure TKrakenProviderFiredacMetadata.PKFields( const ASchema: String = 'public'; ATable: String = '' );
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
        with EntityPKFields.Items[ EntityPKFields.Add( TKrakenEntityPKField.Create ) ] do
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

function TKrakenProviderFiredacMetadata.FK(const ATable: String; ASchema: String = 'public'): String;
var
  LFKField: TKrakenEntityFK;
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
        with EntityFKs.Items[ EntityFKs.Add( TKrakenEntityFK.Create ) ] do
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

function TKrakenProviderFiredacMetadata.FKFields(ATable: String; ASchema: String = 'public'; AConstraint: String = ''): String;
var
  LField : TField;
  LFKField: TKrakenEntityFKField;
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

        with EntityFKFields.Items[ EntityFKFields.Add( TKrakenEntityFKField.Create ) ] do
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

procedure TKrakenProviderFiredacMetadata.Generators(const ASchema: String);
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
        with EntityGenerators.Items[ EntityGenerators.Add( TKrakenEntityGenerator.Create ) ] do
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

function TKrakenProviderFiredacMetadata.Id: String;
begin
  Result := FId;
end;

function TKrakenProviderFiredacMetadata.Id(const Value: String): TKrakenProviderFiredacMetadata;
begin
  Result := Self;
  FId    := Value;
end;

end.

