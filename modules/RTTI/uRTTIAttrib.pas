unit uRTTIAttrib;

interface

uses
  System.RTTI,
  System.Variants,
  System.Classes;

type
  Table = class(TCustomAttribute)
  private
    FName: string;
  public
    constructor Create(aName: string);
    property Name: string read FName;
  end;

  Column = class(TCustomAttribute)
  private
    FName: string;
  public
    Constructor Create(aName: string);
    property Name: string read FName;
  end;

  PK = class(TCustomAttribute)
  end;

  FK = class(TCustomAttribute)
  end;

  NotNull = class(TCustomAttribute)
  end;

  Ignore = class(TCustomAttribute)
  end;

  AutoInc = class(TCustomAttribute)
  end;

  NumberOnly = class(TCustomAttribute)
  end;

  DateOnly = class(TCustomAttribute)

  end;

  Search = class(TCustomAttribute)
      constructor Create(ATitle, AKeyWord, AValue: String);
    private
      FTitle    : string;
      FKeyWord  : String;
      FValue    : String;
    public
      property Title   : String read FTitle write FTitle;
      property KeyWord : String read FKeyWord write FKeyWord;
      property Value   : String read FValue   write FValue;
  end;


implementation

{ Table }

constructor Table.Create(aName: string);
begin
  FName := AName;
end;

{ Field }

constructor Column.Create(aName: string);
begin
  FName := AName;
end;

{ Search }

constructor Search.Create(ATitle, AKeyWord, AValue: String);
begin
  FTitle    := ATitle;
  FKeyWord  := AKeyWord;
  FValue    := AValue;
end;

end.
