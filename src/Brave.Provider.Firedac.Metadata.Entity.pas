unit Brave.Provider.Firedac.Metadata.Entity;

interface

uses
  System.Classes,
  System.Generics.Collections;

type
  TBraveEntitySchema = class
  private
    FRecNo : Integer;
    FName : String;
    FCatalogName: String;
  public
    property RecNO: Integer read FRecNo write FRecNo;
    property Name: String read FName write FName;
    property CatalogName: String read FCatalogName write FCatalogName;
  end;

  TBraveEntitySchemas = TObjectList<TBraveEntitySchema>;

  TBraveEntityTable = class
  private
    FName : String;
  public
    property Name: String read FName write FName;
  end;

  TBraveEntityTables = TObjectList<TBraveEntityTable>;

  TBraveEntityColumn = class
  private
    FName      : String;
    FTypename  : String;
    FPrecision : String;
    FScale     : String;
    FLength    : String;
  public
    property Name      : String read FName      write FName;
    property Typename  : String read FTypename  write FTypename;
    property Precision : String read FPrecision write FPrecision;
    property Scale     : String read FScale     write FScale;
    property Length    : String read FLength    write FLength;
  end;

  TBraveEntityColumns = TObjectList<TBraveEntityColumn>;

  { Primary Key }
  TBraveEntityPK = class
    private
      FName  : String;
      FTable : String;
    public
      property Name   : String read FName   write FName;
      property Table  : String read FTable  write FTable;
  end;

  TBraveEntityPKs = TObjectList<TBraveEntityPK>;

  TBraveEntityPKField = class
    private
      FName   : String;
      FColumn : String;
    public
      property Name   : String read FName   write FName;
      property Column : String read FColumn write FColumn;
  end;

  TBraveEntityPKFields = TObjectList<TBraveEntityPKField>;

  { Foreighn Key }
  TBraveEntityFK = class
    private
      FNameFK  : String;
      FTableFK : String;
    public
      property NameFK  : String read FNameFK   write FNameFK;
      property TableFK : String read FTableFK  write FTableFK;
  end;

  TBraveEntityFKs = TObjectList<TBraveEntityFK>;

  TBraveEntityFKField = class
    private
      FName     : String;
      FTableFK : String;
      FColumn   : String;
      FColumnFK : String;
    public
      property Name     : String read FName     write FName;
      property TableFK  : String read FTableFK  write FTableFK;
      property Column   : String read FColumn   write FColumn;
      property ColumnFK : String read FColumnFK write FColumnFK;

  end;

  TBraveEntityFKFields = TObjectList<TBraveEntityFKField>;

  TBraveEntityGenerator = class
    private
      FName: String;
    public
      property Name: String read FName write FName;
  end;

  TBraveEntityGenerators = TObjectList<TBraveEntityGenerator>;

implementation

end.

