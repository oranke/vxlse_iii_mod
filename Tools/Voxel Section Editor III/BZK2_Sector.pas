unit BZK2_Sector;

interface

uses SysUtils,Voxel_Engine;

type
   TBZKFacesDirection = (bfdNorth,bfdEast,bfdSouth,bfdWest,bfdFloor,bfdCeiling);
   TVector4i = record
      X,
      Y,
      Z,
      W : integer;
   end;

   TBZK2Sector = class
      public
         // Constructors
         constructor Create;
         destructor Destroy; override;
         // I/O
         procedure WriteToFile (var MyFile : System.Text);
         // Adds
         procedure AddTrigger(value : integer);
         // Removes
         function RemoveTrigger(ID : integer): Boolean;
         // Gets
         function GetPosition : TVector3i;
         function GetVoxelPosition : TVector3i;
         function GetVolume : TVector3i;
         function GetName : string;
         function GetConnection(Direction : TBZKFacesDirection) : integer;
         function GetConnectionMap(Direction : TBZKFacesDirection) : string;
         function GetConnectionColours(Direction : TBZKFacesDirection) : TVector3i;
         function GetTrigger(ID : integer): integer;
         function GetNumTriggers: integer;
         function GetColour : TVector4i;
         function GetRenderFace(ID : integer): boolean;
         // Sets
         procedure SetPosition( value : TVector3i);
         procedure SetVoxelPosition( value : TVector3i);
         procedure SetVolume( value : TVector3i); overload;
         procedure SetVolume( value : integer); overload;
         procedure SetName( const value : string);
         procedure SetConnection( Direction : TBZKFacesDirection; value : integer);
         procedure SetConnections( north, east, south, west, floor, ceil : integer);
         procedure SetConnectionMap( Direction : TBZKFacesDirection; value : string);
         procedure SetConnectionColours( Direction : TBZKFacesDirection; value : TVector3i);
         function SetTrigger( ID, Value : integer): Boolean;
         procedure SetColour( value : TVector4i);
         function SetRenderFace( ID : integer; Value : boolean): Boolean;
         // Assign
         procedure Assign(const Sector : TBZK2Sector);
         // AutoSet
         procedure AutoSetForceRenderFaces;
      private
         Position : TVector3i;
         VoxelPosition : TVector3i;
         Volume : TVector3i;
         Name : string;
         Connections : array [TBzkFacesDirection] of integer;
         ConnectionMaps : array [TBzkFacesDirection] of string;
         ConnectionColours : array [TBzkFacesDirection] of TVector3i;
         Triggers : array of integer;
         Colour : TVector4i;
         ForceRenderFace : array[0..5] of boolean;
         // I/O
         procedure WriteTriggers (var MyFile : System.Text);
         procedure WritePosition (var MyFile : System.Text);
         procedure WriteVolume (var MyFile : System.Text);
         procedure WriteName (var MyFile : System.Text);
         procedure WriteConnection (var MyFile : System.Text; Direction : TBZKFacesDirection);
         procedure WriteConnections (var MyFile : System.Text);
         procedure WriteConnectionColour (var MyFile : System.Text; Direction : TBZKFacesDirection);
         procedure WriteConnectionColours (var MyFile : System.Text);
         procedure WriteColour (var MyFile : System.Text);
         procedure WriteForceRenderFace (var MyFile : System.Text);
   end;


implementation

// Constructors & Destructors
constructor TBZK2Sector.Create;
var
   i : integer;
begin
   Position := SetVectorI(0,0,0);
   VoxelPosition := SetVectorI(0,0,0);
   Volume := SetVectorI(0,0,0);
   Name := '';
   Connections[bfdNorth] := 0;
   Connections[bfdEast] := 0;
   Connections[bfdSouth] := 0;
   Connections[bfdWest] := 0;
   Connections[bfdFloor] := 0;
   Connections[bfdCeiling] := 0;
   ConnectionMaps[bfdNorth] := '';
   ConnectionMaps[bfdEast] := '';
   ConnectionMaps[bfdSouth] := '';
   ConnectionMaps[bfdWest] := '';
   ConnectionMaps[bfdFloor] := '';
   ConnectionMaps[bfdCeiling] := '';
   ConnectionColours[bfdNorth] := SetVectorI(0,0,0);
   ConnectionColours[bfdEast] := SetVectorI(0,0,0);
   ConnectionColours[bfdSouth] := SetVectorI(0,0,0);
   ConnectionColours[bfdWest] := SetVectorI(0,0,0);
   ConnectionColours[bfdFloor] := SetVectorI(0,0,0);
   ConnectionColours[bfdCeiling] := SetVectorI(0,0,0);
   SetLength(Triggers,0);
   Colour.X := 0;
   Colour.Y := 0;
   Colour.Z := 0;
   Colour.W := 0;
   for i := 0 to 5 do
      ForceRenderFace[i] := false;
end;

destructor TBZK2Sector.Destroy;
begin
   Name := '';
   SetLength(Triggers,0);
   inherited Destroy;
end;

// Gets
function TBZK2Sector.GetPosition : TVector3i;
begin
   Result.X := Position.X;
   Result.Y := Position.Y;
   Result.Z := Position.Z;
end;

function TBZK2Sector.GetVoxelPosition : TVector3i;
begin
   Result.X := VoxelPosition.X;
   Result.Y := VoxelPosition.Y;
   Result.Z := VoxelPosition.Z;
end;

function TBZK2Sector.GetVolume : TVector3i;
begin
   Result.X := Volume.X;
   Result.Y := Volume.Y;
   Result.Z := Volume.Z;
end;

function TBZK2Sector.GetName : string;
begin
   Result := Name;
end;

function TBZK2Sector.GetConnection(Direction : TBZKFacesDirection) : integer;
begin
   Result := Connections[Direction];
end;

function TBZK2Sector.GetConnectionMap(Direction : TBZKFacesDirection) : string;
begin
   Result := ConnectionMaps[Direction];
end;

function TBZK2Sector.GetConnectionColours(Direction : TBZKFacesDirection) : TVector3i;
begin
   Result.X := ConnectionColours[Direction].X;
   Result.Y := ConnectionColours[Direction].Y;
   Result.Z := ConnectionColours[Direction].Z;
end;

function TBZK2Sector.GetTrigger(ID : integer): integer;
begin
   if ID > High(Triggers) then
      Result := -1
   else
      Result := Triggers[ID];
end;

function TBZK2Sector.GetNumTriggers: integer;
begin
   Result := High(Triggers)+1;
end;

function TBZK2Sector.GetColour : TVector4i;
begin
   Result.X := Colour.X;
   Result.Y := Colour.Y;
   Result.Z := Colour.Z;
   Result.W := Colour.W;
end;

function TBZK2Sector.GetRenderFace(ID : integer): boolean;
begin
   if ID > High(ForceRenderFace) then
      Result := false
   else
      Result := ForceRenderFace[ID];
end;


// Sets
procedure TBZK2Sector.SetPosition( value : TVector3i);
begin
   Position.X := Value.X;
   Position.Y := Value.Y;
   Position.Z := Value.Z;
end;

procedure TBZK2Sector.SetVoxelPosition( value : TVector3i);
begin
   VoxelPosition.X := Value.X;
   VoxelPosition.Y := Value.Y;
   VoxelPosition.Z := Value.Z;
end;

procedure TBZK2Sector.SetVolume( value : TVector3i);
begin
   Volume.X := Value.X;
   Volume.Y := Value.Y;
   Volume.Z := Value.Z;
end;

procedure TBZK2Sector.SetVolume( value : integer);
begin
   Volume.X := Value;
   Volume.Y := Value;
   Volume.Z := Value;
end;

procedure TBZK2Sector.SetName( const value : string);
begin
   Name := Value;
end;

procedure TBZK2Sector.SetConnection( Direction : TBZKFacesDirection; value : integer);
begin
   Connections[Direction] := Value;
end;

procedure TBZK2Sector.SetConnectionMap( Direction : TBZKFacesDirection; value : string);
begin
   ConnectionMaps[Direction] := Value;
end;

procedure TBZK2Sector.SetConnections( north, east, south, west, floor, ceil : integer);
begin
   Connections[bfdNorth] := North;
   Connections[bfdEast] := East;
   Connections[bfdSouth] := South;
   Connections[bfdWest] := West;
   Connections[bfdFloor] := Floor;
   Connections[bfdCeiling] := Ceil;
end;

procedure TBZK2Sector.SetConnectionColours( Direction : TBZKFacesDirection; value : TVector3i);
begin
   ConnectionColours[Direction].X := Value.X;
   ConnectionColours[Direction].Y := Value.Y;
   ConnectionColours[Direction].Z := Value.Z;
end;

function TBZK2Sector.SetTrigger( ID, Value : integer): Boolean;
begin
   Result := false;
   if ID <= High(Triggers) then
   begin
      Triggers[ID] := Value;
      Result := true;
   end;
end;

procedure TBZK2Sector.SetColour( value : TVector4i);
begin
   Colour.X := Value.X;
   Colour.Y := Value.Y;
   Colour.Z := Value.Z;
   Colour.W := Value.W;
end;

function TBZK2Sector.SetRenderFace( ID: Integer; Value : Boolean): Boolean;
begin
   Result := false;
   if ID <= High(ForceRenderFace) then
   begin
      ForceRenderFace[ID] := Value;
      Result := true;
   end;
end;


// Adds
procedure TBZK2Sector.AddTrigger(value : integer);
begin
   SetLength(Triggers,High(Triggers)+2);
   Triggers[High(Triggers)] := Value;
end;

// Removes
function TBZK2Sector.RemoveTrigger(ID : integer): Boolean;
var
   i : integer;
begin
   Result := false;
   if ID <= High(Triggers) then
   begin
       i := ID;
       while i < High(Triggers) do
       begin
          Triggers[i] := Triggers[i+1];
          inc(i);
       end;
       SetLength(Triggers,High(Triggers));
       Result := true;
   end;
end;

// I/O
procedure TBZK2Sector.WriteToFile (var MyFile : System.Text);
begin
   WriteLn(MyFile,'<Sector>');
   WriteTriggers(MyFile);
   WriteVolume(MyFile);
   WriteName(MyFile);
   WriteConnections(MyFile);
   WriteConnectionColours(MyFile);
   WriteColour(MyFile);
   WriteForceRenderFace(MyFile);
   WriteLn(MyFile,'</Sector>');
end;

procedure TBZK2Sector.WriteTriggers (var MyFile : System.Text);
var
   i : integer;
begin
   if High(Triggers) >= 0 then
   begin
      WriteLn(MyFile,'<Triggers>');
      for i := Low(Triggers) to High(Triggers) do
         WriteLn(MyFile,Triggers[i]);
      WriteLn(MyFile,'</Triggers>');
   end;
end;

procedure TBZK2Sector.WritePosition (var MyFile : System.Text);
begin
   WriteLn(MyFile,'<Vec3f>');
   WriteLn(MyFile,Position.X);
   WriteLn(MyFile,Position.Y);
   WriteLn(MyFile,Position.Z);
   WriteLn(MyFile,'</Vec3f>');
end;

procedure TBZK2Sector.WriteVolume (var MyFile : System.Text);
begin
   WriteLn(MyFile,'<Volume>');
   WritePosition(MyFile);
   WriteLn(MyFile,Volume.X);
   WriteLn(MyFile,Volume.Y);
   WriteLn(MyFile,Volume.Z);
   WriteLn(MyFile,'</Volume>');
end;

procedure TBZK2Sector.WriteName (var MyFile : System.Text);
begin
   WriteLn(MyFile,'<Name>');
   WriteLn(MyFile,Name);
   WriteLn(MyFile,'</Name>');
end;

procedure TBZK2Sector.WriteConnection (var MyFile : System.Text; Direction : TBZKFacesDirection);
begin
   if CompareStr(ConnectionMaps[Direction],'') = 0 then
      WriteLn(MyFile,Connections[Direction])
   else
      WriteLn(MyFile,ConnectionMaps[Direction] + ' / ' + IntToStr(Connections[Direction]));
end;

procedure TBZK2Sector.WriteConnections (var MyFile : System.Text);
begin
   WriteLn(MyFile,'<Connections>');
   WriteConnection(MyFile,bfdNorth);
   WriteConnection(MyFile,bfdEast);
   WriteConnection(MyFile,bfdSouth);
   WriteConnection(MyFile,bfdWest);
   WriteConnection(MyFile,bfdFloor);
   WriteConnection(MyFile,bfdCeiling);
   WriteLn(MyFile,'</Connections>');
end;

procedure TBZK2Sector.WriteConnectionColour (var MyFile : System.Text; Direction : TBZKFacesDirection);
begin
   WriteLn(MyFile,'<RGB>');
   WriteLn(MyFile,ConnectionColours[Direction].X);
   WriteLn(MyFile,ConnectionColours[Direction].Y);
   WriteLn(MyFile,ConnectionColours[Direction].Z);
   WriteLn(MyFile,'</RGB>');
end;

procedure TBZK2Sector.WriteConnectionColours (var MyFile : System.Text);
begin
   WriteLn(MyFile,'<Color>');
   WriteConnectionColour(MyFile,bfdNorth);
   WriteConnectionColour(MyFile,bfdEast);
   WriteConnectionColour(MyFile,bfdSouth);
   WriteConnectionColour(MyFile,bfdWest);
   WriteConnectionColour(MyFile,bfdFloor);
   WriteConnectionColour(MyFile,bfdCeiling);
   WriteLn(MyFile,'</Color>');
end;

procedure TBZK2Sector.WriteColour (var MyFile : System.Text);
begin
   if (Colour.X > 0) or (Colour.Y > 0) or (Colour.Z > 0) or (Colour.W > 0) then
   begin
      WriteLn(MyFile,'<RGBA>');
      WriteLn(MyFile,Colour.X);
      WriteLn(MyFile,Colour.Y);
      WriteLn(MyFile,Colour.Z);
      WriteLn(MyFile,Colour.W);
      WriteLn(MyFile,'</RGBA>');
   end;
end;

procedure TBZK2Sector.WriteForceRenderFace (var MyFile : System.Text);
var
   i : integer;
begin
   for i := 0 to 5 do
   begin
      if ForceRenderFace[i] then
      begin
         WriteLn(MyFile,'<Force_Render_Face>');
         WriteLn(MyFile,i);
         WriteLn(MyFile,'</Force_Render_Face>');
      end;
   end;
end;

procedure TBZK2Sector.Assign(const Sector : TBZK2Sector);
var
   i: integer;
begin
   SetPosition(Sector.GetPosition);
   SetVoxelPosition(Sector.GetVoxelPosition);
   SetVolume(Sector.GetVolume);
   SetName(Sector.GetName);
   SetConnections(Sector.GetConnection(bfdNorth),Sector.GetConnection(bfdEast),Sector.GetConnection(bfdSouth),Sector.GetConnection(bfdWest),Sector.GetConnection(bfdFloor),Sector.GetConnection(bfdCeiling));
   SetConnectionMap(bfdNorth,Sector.GetConnectionMap(bfdNorth));
   SetConnectionMap(bfdEast,Sector.GetConnectionMap(bfdEast));
   SetConnectionMap(bfdSouth,Sector.GetConnectionMap(bfdSouth));
   SetConnectionMap(bfdWest,Sector.GetConnectionMap(bfdWest));
   SetConnectionMap(bfdFloor,Sector.GetConnectionMap(bfdFloor));
   SetConnectionMap(bfdCeiling,Sector.GetConnectionMap(bfdCeiling));
   SetConnectionColours(bfdNorth,Sector.GetConnectionColours(bfdNorth));
   SetConnectionColours(bfdEast,Sector.GetConnectionColours(bfdEast));
   SetConnectionColours(bfdSouth,Sector.GetConnectionColours(bfdSouth));
   SetConnectionColours(bfdWest,Sector.GetConnectionColours(bfdWest));
   SetConnectionColours(bfdFloor,Sector.GetConnectionColours(bfdFloor));
   SetConnectionColours(bfdCeiling,Sector.GetConnectionColours(bfdCeiling));
   SetLength(Triggers,0);
   if Sector.GetNumTriggers > 0 then
      for i := 0 to Sector.GetNumTriggers do
         AddTrigger(Sector.GetTrigger(i));
   SetColour(Sector.GetColour);
   for i := 0 to 5 do
      SetRenderFace(i,Sector.GetRenderFace(i));
end;

// AutoSets
procedure TBZK2Sector.AutoSetForceRenderFaces;
var
   i : integer;
begin
   if (Colour.X > 0) or (Colour.Y > 0) or (Colour.Z > 0) or (Colour.W > 0) then
   begin
      for i := 0 to 5 do
         ForceRenderFace[0] := false;
      if Connections[bfdNorth] = 0 then
         ForceRenderFace[0] := true;
      if Connections[bfdEast] = 0 then
         ForceRenderFace[1] := true;
      if Connections[bfdSouth] = 0 then
         ForceRenderFace[2] := true;
      if Connections[bfdWest] = 0 then
         ForceRenderFace[3] := true;
      if Connections[bfdFloor] = 0 then
         ForceRenderFace[4] := true;
      if Connections[bfdCeiling] = 0 then
         ForceRenderFace[5] := true;
   end;
end;



end.
