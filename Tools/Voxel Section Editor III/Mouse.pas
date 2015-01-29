unit Mouse;

{$INCLUDE Global_Conditionals.inc}

interface
uses
  Windows, SysUtils, Forms,Controls;

Const
   MouseBrush = 1201;
   MouseLine = 1203;
   MouseDropper = 1204;
   MouseFill = 1205;
   MouseDraw = 1206;
   MouseMagnify = 1207;
   MouseSpray = 8029;
   MouseMoveC = 1033; // Named MouseMoveC cos of other things named MouseMove

Var
   Mouse_Current : integer = crDefault;

   function LoadMouseCursors : boolean;
   function LoadMouseCursor(Number:integer) : integer;

implementation

function LoadMouseCursors : boolean;
var
   temp : integer;
begin
   result := true;
   temp := 0;
   temp := temp + LoadMouseCursor(MouseBrush);
   temp := temp + LoadMouseCursor(MouseLine);
   temp := temp + LoadMouseCursor(MouseDropper);
   temp := temp + LoadMouseCursor(MouseFill);
   temp := temp + LoadMouseCursor(MouseDraw);
   temp := temp + LoadMouseCursor(MouseMagnify);
   temp := temp + LoadMouseCursor(MouseSpray);
   temp := temp + LoadMouseCursor(MouseMoveC);

   if temp < 0 then
      Result := false;
end;

function LoadMouseCursor(Number:integer) : integer;
var
   filename : pchar;
begin
   Result := 0;
   filename := pchar(ExtractFileDir(ParamStr(0)) + '\cursors\'+inttostr(Number)+'.cur');
   if Not fileexists(filename) then
   begin
      Result := -1;
      MessageBox(0,pchar('Error Cursor Missing < ' + extractfilename(filename) + ' >'),'Cursor Error',0);
   end
   else
      Screen.Cursors[Number] := LoadCursorFromFile(filename);
end;

end.
