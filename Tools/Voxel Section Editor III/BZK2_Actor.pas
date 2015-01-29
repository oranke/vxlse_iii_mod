unit BZK2_Actor;

interface

type
   TBZK2Actor = class
      private
         Name : string;
         Location : integer;
         MyClass : string;
         // I/O
         procedure WriteName (var MyFile : System.Text);
         procedure WriteLocation (var MyFile : System.Text);
         procedure WriteClass (var MyFile : System.Text);
      public
         // Constructors and Destructors
         constructor Create;
         destructor Destroy; override;
         // I/O
         procedure WriteToFile (var MyFile : System.Text);
         // Gets
         function GetName : string;
         function GetLocation : integer;
         function GetClass : string;
         // Sets
         procedure SetName ( value : string);
         procedure SetLocation ( value : integer);
         procedure SetClass ( value : string);
         // Assign
         procedure Assign(const Actor : TBZK2Actor);
   end;


implementation

// Constructors and Destructors
constructor TBZK2Actor.Create;
begin
   Name := '';
   Location := 1;
   MyClass := 'DefaultClass';
end;

destructor TBZK2Actor.Destroy;
begin
   Name := '';
   MyClass := '';
   inherited Destroy;
end;

// Gets
function TBZK2Actor.GetName : string;
begin
   Result := Name;
end;

function TBZK2Actor.GetLocation : integer;
begin
   Result := Location;
end;

function TBZK2Actor.GetClass : string;
begin
   Result := MyClass;
end;

// Sets
procedure TBZK2Actor.SetName ( Value : string);
begin
   Name := Value;
end;

procedure TBZK2Actor.SetLocation ( Value : integer);
begin
   Location := Value;
end;

procedure TBZK2Actor.SetClass ( Value : string);
begin
   MyClass := value;
end;

// I/O
procedure TBZK2Actor.WriteToFile (var MyFile : System.Text);
begin
   WriteLn(MyFile,'<Actor>');
   WriteName(MyFile);
   WriteLocation(MyFile);
   WriteClass(MyFile);
   WriteLn(MyFile,'</Actor>');
end;

procedure TBZK2Actor.WriteName (var MyFile : System.Text);
begin
   WriteLn(MyFile,'<Name>');
   WriteLn(MyFile,Name);
   WriteLn(MyFile,'</Name>');
end;

procedure TBZK2Actor.WriteLocation (var MyFile : System.Text);
begin
   WriteLn(MyFile,'<Location>');
   WriteLn(MyFile,Location);
   WriteLn(MyFile,'</Location>');
end;

procedure TBZK2Actor.WriteClass (var MyFile : System.Text);
begin
   WriteLn(MyFile,'<Class>');
   WriteLn(MyFile,MyClass);
   WriteLn(MyFile,'</Class>');
end;

procedure TBZK2Actor.Assign(const Actor : TBZK2Actor);
begin
   SetName(Actor.GetName);
   SetLocation(Actor.GetLocation);
   SetClass(Actor.GetClass);
end;


end.
