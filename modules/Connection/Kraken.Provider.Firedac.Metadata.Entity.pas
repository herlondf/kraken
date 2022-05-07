unit Kraken.Provider.Firedac.Metadata.Entity;

interface

uses
  System.Classes,
  System.Generics.Collections;

type
  TKrakenEntitySchema = class
  private
    FRecNo : Integer;
    FName : String;
    FCatalogName: String;
  public
    property RecNO: Integer read FRecNo write FRecNo;
    property Name: String read FName write FName;
    property CatalogName: String read FCatalogName write FCatalogName;
  end;

  TKrakenEntitySchemas = TObjectList<TKrakenEntitySchema>;

  TKrakenEntityTable = class
  private
    FName : String;
  public
    property Name: String read FName write FName;
  end;

  TKrakenEntityTables = TObjectList<TKrakenEntityTable>;

  TKrakenEntityColumn = class
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

  TKrakenEntityColumns = TObjectList<TKrakenEntityColumn>;

  { Primary Key }
  TKrakenEntityPK = class
    private
      FName  : String;
      FTable : String;
    public
      property Name   : String read FName   write FName;
      property Table  : String read FTable  write FTable;
  end;

  TKrakenEntityPKs = TObjectList<TKrakenEntityPK>;

  TKrakenEntityPKField = class
    private
      FName   : String;
      FColumn : String;
    public
      property Name   : String read FName   write FName;
      property Column : String read FColumn write FColumn;
  end;

  TKrakenEntityPKFields = TObjectList<TKrakenEntityPKField>;

  { Foreighn Key }
  TKrakenEntityFK = class
    private
      FNameFK  : String;
      FTableFK : String;
    public
      property NameFK  : String read FNameFK   write FNameFK;
      property TableFK : String read FTableFK  write FTableFK;
  end;

  TKrakenEntityFKs = TObjectList<TKrakenEntityFK>;

  TKrakenEntityFKField = class
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

  TKrakenEntityFKFields = TObjectList<TKrakenEntityFKField>;

  TKrakenEntityGenerator = class
    private
      FName: String;
    public
      property Name: String read FName write FName;
  end;

  TKrakenEntityGenerators = TObjectList<TKrakenEntityGenerator>;

implementation

end.

