unit Kraken.DateTime.Helper;

interface

uses System.sysUtils, System.DateUtils;

type TKrakenDatetimeHelper = record helper for TDateTime

  private
    function Iso8601ToDateTime(AValue: String): TDateTime;

  public
    function DateTimeToIso8601: string;
    function Format(ADateFormat: String): String;

    function FormatYYYY_MM_DD: string;

    procedure fromIso8601ToDateTime(AValue: String);
end;

implementation

{ TKrakenDatetimeHelper }

function TKrakenDatetimeHelper.DateTimeToIso8601: string;
begin
  if Self = 0 then
    Result := ''
  else
  if Frac(Self) = 0 then
    Result := FormatDateTime('yyyy"-"mm"-"dd', Self)
  else
  if Trunc(Self) = 0 then
    Result := FormatDateTime('"T"hh":"nn":"ss', Self)
  else
    Result := FormatDateTime('yyyy"-"mm"-"dd"T"hh":"nn":"ss', Self);
end;

function TKrakenDatetimeHelper.Format(ADateFormat: String): String;
begin
  result := FormatDateTime(ADateFormat, Self);
end;

function TKrakenDatetimeHelper.FormatYYYY_MM_DD: string;
begin
  result := Format('yyyy-MM-dd');
end;

procedure TKrakenDatetimeHelper.fromIso8601ToDateTime(AValue: String);
begin
  Self := Iso8601ToDateTime(AValue);
end;

function TKrakenDatetimeHelper.Iso8601ToDateTime(AValue: String): TDateTime;
var
  Y, M, D, HH, MI, SS: Cardinal;
begin
  // YYYY-MM-DD   Thh:mm:ss  or  YYYY-MM-DDThh:mm:ss
  // 1234567890   123456789      1234567890123456789

  //{"$date":"2019-08-24T11:08:13.000-03:00"} // Data no Mongo
  AValue := AValue.Replace('{"$date":"', '').Replace('"}', '');

  Result := 0;
  case Length(AValue) of
    9:
      if (AValue[1] = 'T') and (AValue[4] = ':') and (AValue[7] = ':') then
      begin
        HH := Ord(AValue[2]) * 10 + Ord(AValue[3]) - (48 + 480);
        MI := Ord(AValue[5]) * 10 + Ord(AValue[6]) - (48 + 480);
        SS := Ord(AValue[8]) * 10 + Ord(AValue[9]) - (48 + 480);
        if (HH < 24) and (MI < 60) and (SS < 60) then
          Result := EncodeTime(HH, MI, SS, 0);
      end;
    10:
      if (AValue[5] = AValue[8]) and (Ord(AValue[8]) in [Ord('-'), Ord('/')]) then
      begin
        Y := Ord(AValue[1]) * 1000 + Ord(AValue[2]) * 100 + Ord(AValue[3]) * 10 + Ord(AValue[4]) - (48 + 480 + 4800 + 48000);
        M := Ord(AValue[6]) * 10 + Ord(AValue[7]) - (48 + 480);
        D := Ord(AValue[9]) * 10 + Ord(AValue[10]) - (48 + 480);
        if (Y <= 9999) and ((M - 1) < 12) and ((D - 1) < 31) then
          Result := EncodeDate(Y, M, D);
      end;
    19,20,21,22,23,24,25,26,27,28,29:
      if (AValue[5] = AValue[8]) and
         (Ord(AValue[8]) in [Ord('-'), Ord('/')]) and
         (Ord(AValue[11]) in [Ord(' '), Ord('T')]) and
         (AValue[14] = ':') and
         (AValue[17] = ':') then
      begin
        Y := Ord(AValue[1]) * 1000 + Ord(AValue[2]) * 100 + Ord(AValue[3]) * 10 + Ord(AValue[4]) - (48 + 480 + 4800 + 48000);
        M := Ord(AValue[6]) * 10 + Ord(AValue[7]) - (48 + 480);
        D := Ord(AValue[9]) * 10 + Ord(AValue[10]) - (48 + 480);
        HH := Ord(AValue[12]) * 10 + Ord(AValue[13]) - (48 + 480);
        MI := Ord(AValue[15]) * 10 + Ord(AValue[16]) - (48 + 480);
        SS := Ord(AValue[18]) * 10 + Ord(AValue[19]) - (48 + 480);
        if (Y <= 9999) and ((M - 1) < 12) and ((D - 1) < 31) and (HH < 24) and (MI < 60) and (SS < 60) then
          Result := EncodeDate(Y, M, D) + EncodeTime(HH, MI, SS, 0);
      end;
  end;
end;

end.

