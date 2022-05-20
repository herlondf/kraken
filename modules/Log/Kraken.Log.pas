unit Kraken.Log;

interface

type
  TLogTypes = (ltTrace, ltDebug, ltInfo, ltWarning, ltError, ltFatal);

  TKrakenLog = class
  private
    FFileName: string;
    FIsInit: Boolean;
    FOutFile: TextFile;
    FQuietMode: Boolean;
    FQuietTypes: set of TLogTypes;
    procedure Initialize;
    procedure CreateFoldersIfNecessary;
    procedure Finalize;
    procedure Write(const Msg: string);
  public
    constructor Create(const FileName: string);
    destructor Destroy; override;

    property FileName: string read FFileName;

    procedure SetQuietMode;
    procedure DisableTraceLog;
    procedure DisableDebugLog;
    procedure DisableInfoLog;
    procedure DisableWarningLog;
    procedure DisableErrorLog;
    procedure DisableFatalLog;

    procedure SetNoisyMode;
    procedure EnableTraceLog;
    procedure EnableDebugLog;
    procedure EnableInfoLog;
    procedure EnableWarningLog;
    procedure EnableErrorLog;
    procedure EnableFatalLog;

    procedure Clear;

    procedure Trace(const Msg: string);
    procedure Debug(const Msg: string);
    procedure Info(const Msg: string);
    procedure Warning(const Msg: string);
    procedure Error(const Msg: string);
    procedure Fatal(const Msg: string);
  end;

  function  ProductName: String;
  function  KrakenLog: TKrakenLog;

var
  FInstance: TKrakenLog;

implementation

uses
  SysUtils,
  Windows;

const
  FORMAT_LOG   = '%s - %s';
  PREFIX_TRACE = 'TRACE';
  PREFIX_DEBUG = 'DEBUG';
  PREFIX_INFO  = 'INFO ';
  PREFIX_WARN  = 'WARN ';
  PREFIX_ERROR = 'ERROR';
  PREFIX_FATAL = 'FATAL';

function  KrakenLog: TKrakenLog;
begin
  Result := FInstance;
end;

function ProductName: String;
var
  Len               : Cardinal;
  n                 : Cardinal;
  Buf               : PChar;
  Value             : PChar;
begin
  n := GetFileVersionInfoSize(PChar( ExtractFileName( ParamStr(0) ) ), n);
  if n > 0 then
  begin
     Buf := AllocMem(n);
     try
       GetFileVersionInfo(PChar( ExtractFileName( ParamStr(0) ) ), 0, n, Buf);

       if VerQueryValue(Buf, PChar('StringFileInfo\041604E4\ProductName'), Pointer(Value), Len) then
         Result := Value;
     finally
       FreeMem(Buf, n);
     end;
  end;

  if Result = '' then
  begin
    Result := ExtractFileName( ParamStr(0) );

    if Pos('.', Result) > 0 then
      Result := Copy(Result, 0, Pos('.', Result)-1);
  end;
end;

{ TKrakenLog }

procedure TKrakenLog.Clear;
begin
  if not FileExists(FFileName) then
    Exit;

  if FIsInit then
    CloseFile(FOutFile);

  SysUtils.DeleteFile(FFileName);

  FIsInit := False;
end;

constructor TKrakenLog.Create(const FileName: string);
begin
  FFileName := FileName;
  FIsInit := False;
  Self.SetNoisyMode;
  FQuietTypes := [];
end;

procedure TKrakenLog.CreateFoldersIfNecessary;
var
  FilePath: string;
  FullApplicationPath: string;
begin
  FilePath := ExtractFilePath(FFileName);

  if Pos(':', FilePath) > 0 then
    ForceDirectories(FilePath)
  else begin
    FullApplicationPath := ExtractFilePath( ExtractFileName( ParamStr(0) ) );
    ForceDirectories(IncludeTrailingPathDelimiter(FullApplicationPath) + FilePath);
  end;
end;

procedure TKrakenLog.Debug(const Msg: string);
begin
  {$WARN SYMBOL_PLATFORM OFF}
  if DebugHook = 0 then
    Exit;
  {$WARN SYMBOL_PLATFORM ON}

  if not (ltDebug in FQuietTypes) then
    Self.Write(Format(FORMAT_LOG, [PREFIX_DEBUG, Msg]));
end;

destructor TKrakenLog.Destroy;
begin
  Self.Finalize;
  inherited;
end;

procedure TKrakenLog.DisableDebugLog;
begin
  Include(FQuietTypes, ltDebug);
end;

procedure TKrakenLog.DisableErrorLog;
begin
  Include(FQuietTypes, ltError);
end;

procedure TKrakenLog.DisableFatalLog;
begin
  Include(FQuietTypes, ltFatal);
end;

procedure TKrakenLog.DisableInfoLog;
begin
  Include(FQuietTypes, ltInfo);
end;

procedure TKrakenLog.DisableTraceLog;
begin
  Include(FQuietTypes, ltTrace);
end;

procedure TKrakenLog.DisableWarningLog;
begin
  Include(FQuietTypes, ltWarning);
end;

procedure TKrakenLog.EnableDebugLog;
begin
  Exclude(FQuietTypes, ltDebug);
end;

procedure TKrakenLog.EnableErrorLog;
begin
  Exclude(FQuietTypes, ltError);
end;

procedure TKrakenLog.EnableFatalLog;
begin
  Exclude(FQuietTypes, ltFatal);
end;

procedure TKrakenLog.EnableInfoLog;
begin
  Exclude(FQuietTypes, ltInfo);
end;

procedure TKrakenLog.EnableTraceLog;
begin
  Exclude(FQuietTypes, ltTrace);
end;

procedure TKrakenLog.EnableWarningLog;
begin
  Exclude(FQuietTypes, ltWarning);
end;

procedure TKrakenLog.Error(const Msg: string);
begin
  if not (ltError in FQuietTypes) then
    Self.Write(Format(FORMAT_LOG, [PREFIX_ERROR, Msg]));
end;

procedure TKrakenLog.Fatal(const Msg: string);
begin
  if not (ltFatal in FQuietTypes) then
    Self.Write(Format(FORMAT_LOG, [PREFIX_FATAL, Msg]));
end;

procedure TKrakenLog.Finalize;
begin
  if (FIsInit and (not FQuietMode)) then
    CloseFile(FOutFile);

  FIsInit := False;
end;

procedure TKrakenLog.Initialize;
begin
  if FIsInit then
    CloseFile(FOutFile);

  if not FQuietMode then
  begin
    Self.CreateFoldersIfNecessary;

    AssignFile(FOutFile, FFileName);
    if not FileExists(FFileName) then
      Rewrite(FOutFile)
    else
      Append(FOutFile);
  end;

  FIsInit := True;
end;

procedure TKrakenLog.Info(const Msg: string);
begin
  if not (ltInfo in FQuietTypes) then
    Self.Write(Format(FORMAT_LOG, [PREFIX_INFO, Msg]));
end;

procedure TKrakenLog.SetNoisyMode;
begin
  FQuietMode := False;
end;

procedure TKrakenLog.SetQuietMode;
begin
  FQuietMode := True;
end;

procedure TKrakenLog.Trace(const Msg: string);
begin
  if not (ltTrace in FQuietTypes) then
    Self.Write(Format(FORMAT_LOG, [PREFIX_TRACE, Msg]));
end;

procedure TKrakenLog.Warning(const Msg: string);
begin
  if not (ltWarning in FQuietTypes) then
    Self.Write(Format(FORMAT_LOG, [PREFIX_WARN, Msg]));
end;

procedure TKrakenLog.Write(const Msg: string);
const
  FORMAT_DATETIME_DEFAULT = 'yyyy-mm-dd hh:nn:ss';
begin
  if FQuietMode then
    Exit;

  Self.Initialize;
  try
    if FIsInit then
      Writeln(FOutFile, Format('[%s] %s', [FormatDateTime(FORMAT_DATETIME_DEFAULT, Now), Msg]));
  finally
    Self.Finalize;
  end;
end;

initialization
  FInstance := TKrakenLog.Create(
    Format(
      '%s\LOG\%s\%s.log',
      [
        ExtractFilePath( ParamStr( 0 ) ),
        ProductName,
        FormatDateTime('yyyy-mm-dd', Now)
      ]
    )
  );

finalization
  FInstance.Free;

end.
