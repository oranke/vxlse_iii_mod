unit Class3DPointList;

interface

type
   P3DPosition = ^T3DPosition;
   T3DPosition = record
      x,y,z : integer;
      Next : P3DPosition;
   end;

   C3DPointList = class
      private
         Start,Last,Active : P3DPosition;
         procedure Reset;
      public
         // Constructors and Destructors
         constructor Create;
         destructor Destroy; override;
         // Add
         procedure Add (x,y,z : integer);
         procedure Delete;
         // Delete
         procedure Clear;
         // Gets
         function GetPosition (var x,y,z : integer): boolean;
         function GetX: integer;
         function GetY: integer;
         function GetZ: integer;
         // Misc
         procedure GoToNextElement;
   end;

implementation

constructor C3DPointList.Create;
begin
   Reset;
end;

destructor C3DPointList.Destroy;
begin
   Clear;
   Reset;
end;

procedure C3DPointList.Reset;
begin
   Start := nil;
   Last := nil;
   Active := nil;
end;

// Add
procedure C3DPointList.Add (x,y,z : integer);
var
   NewPosition : P3DPosition;
begin
   New(NewPosition);
   NewPosition^.x := x;
   NewPosition^.y := y;
   NewPosition^.z := z;
   NewPosition^.Next := nil;
   if Start <> nil then
   begin
      Last^.Next := NewPosition;
   end
   else
   begin
      Start := NewPosition;
      Active := Start;
   end;
   Last := NewPosition;
end;

// Delete
procedure C3DPointList.Delete;
var
   Previous : P3DPosition;
begin
   if Active <> nil then
   begin
      Previous := Start;
      if Active = Start then
      begin
         Start := Start^.Next;
      end
      else
      begin
         while Previous^.Next <> Active do
         begin
            Previous := Previous^.Next;
         end;
         Previous^.Next := Active^.Next;
         if Active = Last then
         begin
            Last := Previous;
         end;
      end;
      Dispose(Active);
   end;
end;

procedure C3DPointList.Clear;
var
   Garbage : P3DPosition;
begin
   Active := Start;
   while Active <> nil do
   begin
      Garbage := Active;
      Active := Active^.Next;
      dispose(Garbage);
   end;
end;

// Gets
function C3DPointList.GetPosition (var x,y,z : integer): boolean;
begin
   if Active <> nil then
   begin
      x := Active^.x;
      y := Active^.y;
      z := Active^.z;
      Result := true;
   end
   else
   begin
      Result := false;
   end;
end;

function C3DPointList.GetX: integer;
begin
   if Active <> nil then
   begin
      Result := Active^.x;
   end;
end;

function C3DPointList.GetY: integer;
begin
   if Active <> nil then
   begin
      Result := Active^.y;
   end;
end;

function C3DPointList.GetZ: integer;
begin
   if Active <> nil then
   begin
      Result := Active^.z;
   end;
end;

// Misc
procedure C3DPointList.GoToNextElement;
begin
   if Active <> nil then
   begin
      Active := Active^.Next;
   end
end;

end.
