unit BZK2_Camera;

interface

type
   TBZK2Camera = class
      private
         Actor : integer;
      public
         // Constructors & Destructors
         constructor Create;
         destructor Destroy; override;
         // I/O
         procedure WriteToFile (var MyFile : System.Text);
         // Gets
         function GetActor : integer;
         // Sets
         procedure SetActor (Value : integer);
         // Assign
         procedure Assign(const Camera: TBZK2Camera);
   end;

implementation

// Constructors & Destructors
constructor TBZK2Camera.Create;
begin
   Actor := 0;
end;

destructor TBZK2Camera.Destroy;
begin
   inherited Destroy;
end;

// I/O
procedure TBZK2Camera.WriteToFile (var MyFile : System.Text);
begin
   WriteLn(MyFile,'<Camera>');
   WriteLn(MyFile,Actor);
   WriteLn(MyFile,'</Camera>');
end;

// Gets
function TBZK2Camera.GetActor : integer;
begin
   Result := Actor;
end;

// Sets
procedure TBZK2Camera.SetActor (Value : integer);
begin
   Actor := Value;
end;

procedure TBZK2Camera.Assign(const Camera: TBZK2Camera);
begin
   SetActor(Camera.GetActor);
end;

end.
