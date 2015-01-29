unit pause;

interface
 uses
  MMSystem,Forms;

 procedure Delay(czas: Cardinal); { czas w milisekundach }

implementation

procedure Delay(czas: Cardinal);
var
  i: Cardinal;
begin
  i:=TimeGetTime;
  repeat
    Application.ProcessMessages;
  until TimeGetTime - i >= czas;
end;

end.
