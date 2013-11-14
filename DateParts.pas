unit DateParts;

interface

uses SysUtils, DateUtils;

type
  TDatePart = (dpYear, dpMonth, dpDay, dpHour, dpMinute, dpSecond);
  TDateChangeAction = (dcaShift, dcaSet);

const
  DatePartLabels : array[TDatePart] of string =
    ('Year', 'Month', 'Day', 'Hour', 'Minute', 'Second');

function ChangeDate(ChangeAction: TDateChangeAction; DatePart: TDatePart; Number: Integer; SrcDate: TDateTime): TDateTime;

implementation

{
  Change given datetime
    ChangeAction: shift/set
    DatePart:     part of a date to change
    Number:       value to shift by/new value of a date part
}
function ChangeDate(ChangeAction: TDateChangeAction; DatePart: TDatePart; Number: Integer; SrcDate: TDateTime): TDateTime;
begin
  case ChangeAction of
    dcaShift:
      case DatePart of
        dpYear:   Result := IncYear(SrcDate, Number);
        dpMonth:  Result := IncMonth(SrcDate, Number);
        dpDay:    Result := IncDay(SrcDate, Number);
        dpHour:   Result := IncHour(SrcDate, Number);
        dpMinute: Result := IncMinute(SrcDate, Number);
        dpSecond: Result := IncSecond(SrcDate, Number);
      end;

    dcaSet:
      case DatePart of
        dpYear:   Result := RecodeYear(SrcDate, Number);
        dpMonth:  Result := RecodeMonth(SrcDate, Number);
        dpDay:    Result := RecodeDay(SrcDate, Number);
        dpHour:   Result := RecodeHour(SrcDate, Number);
        dpMinute: Result := RecodeMinute(SrcDate, Number);
        dpSecond: Result := RecodeSecond(SrcDate, Number);
      end;
  end;
end;

end.
