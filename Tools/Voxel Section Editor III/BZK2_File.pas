unit BZK2_File;

interface

uses  Windows, SysUtils, Voxel, Voxel_Engine, BZK2_Sector, BZK2_Camera, BZK2_Actor,
      Palette, Normals;

type
   TSectorMap = array of array of array of integer;

   TBZK2Settings = record
      UseBZKMapXSL : boolean;
      LocalLighting : TVector3f;
      UseTrueMasterSectorSize : Boolean;
      MasterSectorColours : array [TBzkFacesDirection] of TVector3i;
      InvertX, InvertY, InvertZ : Boolean;
      ColoursWithNormals: Boolean;
   end;

   TBZK2PrivateMembers = record
      xInc, yInc, zInc: Integer;
   end;

   TBZK2File = class
      private
         // Traditional File Atributes
         Sectors : array of TBZK2Sector;
         Actors : array of TBZK2Actor;
         Cameras : array of TBZK2Camera;
         // Burocratic Outerfile Additions
         Filename : string;
         SectorMap : TSectorMap;
         VoxelSection : PVoxelSection;
         Settings : TBZK2Settings;
         PrivateMembers : TBZK2PrivateMembers;
         Valid : Boolean;
         BZKMap : Boolean;
         // I/O
         procedure WriteToFile (var MyFile : System.Text);
         procedure WriteHeader (var MyFile : System.Text);
         procedure WriteFooter (var MyFile : System.Text);
         // Random
         function CalculateColour(Value : TVector3i): TVector3i;
         function MultiplyVectorI(Value,Scale : TVector3i): TVector3i;
         procedure SetMeAConnection(SectorNum,x,y,z : integer; Direction : TBZKFacesDirection);
         procedure SetFaceColour(SectorNum,x,y,z : integer; Direction : TBZKFacesDirection);
         procedure SetFaceColourWithoutNormals(SectorNum,x,y,z : integer; Direction : TBZKFacesDirection);
         function GetNormalX(V : TVoxelUnpacked) : single;
         function GetNormalY(V : TVoxelUnpacked) : single;
         function GetNormalZ(V : TVoxelUnpacked) : single;
      public
         // Constructors and Destructors
         constructor Create( p_VoxelSection : PVoxelSection);
         destructor Destroy; override;
         // I/O
         procedure SaveToFile(const _Filename : string);
         // Gets
         function GetNumSectors : integer;
         function GetNumActors : integer;
         function GetNumCameras : integer;
         function GetFilename : string;
         function GetVoxelSection : PVoxelSection;
         function GetUseBZKMapXSL : Boolean;
         function GetLocalLighting : TVector3f; overload;
         procedure GetLocalLighting(var r,g,b : integer); overload;
         function GetUseTrueMasterSectorSize: Boolean;
         function GetInvertX : Boolean;
         function GetInvertY : Boolean;
         function GetInvertZ : Boolean;
         function GetSector(ID : integer): TBZK2Sector;
         function GetActor(ID : integer): TBZK2Actor;
         function GetCamera(ID : integer): TBZK2Camera;
         function GetColoursWithNormals: Boolean;
         function GetMinBounds : TVector3i;
         function GetMaxBounds : TVector3i;
         function GetCenterPosition : TVector3i;
         function IsValid : Boolean;
         function IsBZKMap : Boolean;
         // Sets
         procedure SetFilename (Value : string);
         procedure SetUseBZKMapXSL(Value : Boolean);
         procedure SetLocalLighting( Value : TVector3f); overload;
         procedure SetLocalLighting( r,g,b : integer); overload;
         procedure SetUseTrueMasterSectorSize(Value : Boolean);
         procedure SetInvertX(Value : Boolean);
         procedure SetInvertY(Value : Boolean);
         procedure SetInvertZ(Value : Boolean);
         procedure SetColoursWithNormals(Value : Boolean);
         procedure SetMinBounds( Value : TVector3i);
         procedure SetMaxBounds( Value : TVector3i);
         procedure SetIsBZKMap(Value : Boolean);
         // Moves
         procedure MoveCenterToPosition(Value : TVector3i);
         procedure MoveSideToPosition(Direction : TBZKFacesDirection; Value : Integer);
         // Adds
         procedure AddSector;
         procedure AddActor;
         procedure AddCamera;
         // Removes
         function RemoveSector( ID: integer): Boolean;
         function RemoveActor( ID: integer): Boolean;
         function RemoveCamera( ID: integer): Boolean;
         // Clears
         procedure ClearSectors;
         procedure ClearActors;
         procedure ClearCameras;
         procedure ClearSectorMap;
         // Builds
         procedure BuildSectorMap;
         procedure BuildConnections;
         procedure BuildColours;
         // Attach
         procedure Attach(var BZK2File : TBZK2File; Direction : TBZKFacesDirection; Offset : TVector3i);
   end;

implementation

// Constructors and Destructors
constructor TBZK2File.Create( p_VoxelSection : PVoxelSection);
begin
   SetLength(Sectors,0);
   SetLength(Actors,0);
   SetLength(Cameras,0);
   SetLength(SectorMap,0,0,0);
   VoxelSection := p_VoxelSection;
   if VoxelSection <> nil then
   begin
      Filename := VoxelSection^.Header.Name;
      Valid := true;
    end
   else
      Valid := false;
   BZKMap := Valid;
   // Setup Basic Settings
   Settings.UseBZKMapXSL := true;
   Settings.LocalLighting.X := 1; // default, full red lighting
   Settings.LocalLighting.Y := 1; // default, full green lighting
   Settings.LocalLighting.Z := 1; // default, full blue lighting
   Settings.UseTrueMasterSectorSize := true;
   Settings.MasterSectorColours[bfdNorth] := SetVectorI(255,0,0);
   Settings.MasterSectorColours[bfdEast] := SetVectorI(255,255,255);
   Settings.MasterSectorColours[bfdSouth] := SetVectorI(0,255,0);
   Settings.MasterSectorColours[bfdWest] := SetVectorI(0,0,255);
   Settings.MasterSectorColours[bfdFloor] := SetVectorI(64,64,64);
   Settings.MasterSectorColours[bfdCeiling] := SetVectorI(128,128,128);
   Settings.InvertX := false;
   Settings.InvertY := false;
   Settings.InvertZ := false;
   Settings.ColoursWithNormals := true;
end;

destructor TBZK2File.Destroy;
begin
   ClearSectors;
   ClearActors;
   ClearCameras;
   ClearSectorMap;
   Finalize(Sectors);
   Finalize(Actors);
   Finalize(Cameras);
   Finalize(SectorMap);
   inherited Destroy;
end;


// I/O
procedure TBZK2File.SaveToFile(const _Filename : string);
var
   MyFile : System.Text;
begin
   if Valid and BZKMap then
   begin
      AssignFile(MyFile,_Filename);
      Rewrite(MyFile);
      WriteToFile(MyFile);
      CloseFile(MyFile);
   end;
end;

procedure TBZK2File.WriteToFile (var MyFile : System.Text);
var
   i : integer;
begin
   WriteHeader(MyFile);
   if GetNumSectors > 0 then
   begin
      for i := Low(Sectors) to High(Sectors) do
         Sectors[i].WriteToFile(MyFile);
   end;
   if GetNumActors > 0 then
   begin
      WriteLn(MyFile,'<Actors>');
      for i := Low(Actors) to High(Actors) do
         Actors[i].WriteToFile(MyFile);
      WriteLn(MyFile,'</Actors>');
   end;
   if GetNumCameras > 0 then
   begin
      for i := Low(Cameras) to High(Cameras) do
         Cameras[i].WriteToFile(MyFile);
   end;
   WriteFooter(MyFile);
end;

procedure TBZK2File.WriteHeader (var MyFile : System.Text);
begin
   WriteLn(MyFile,'<?xml version="1.0" encoding="ISO-8859-1"?>');
   if Settings.UseBZKMapXSL then
      WriteLn(MyFile,'<?xml-stylesheet type="text/xsl" href="./bzkmap.xsl" ?>');
   WriteLn(MyFile,'<BZK2>');
   WriteLn(MyFile,'<Version>');
   WriteLn(MyFile,'0.6');
   WriteLn(MyFile,'</Version>');
   WriteLn(MyFile,'<Number.of.Sectors>');
   WriteLn(MyFile,GetNumSectors);
   WriteLn(MyFile,'</Number.of.Sectors>');
end;

procedure TBZK2File.WriteFooter (var MyFile : System.Text);
begin
   WriteLn(MyFile,'</BZK2>');
end;

// Gets
function TBZK2File.GetNumSectors: integer;
begin
   Result := High(Sectors) + 1;
end;

function TBZK2File.GetNumActors: integer;
begin
   Result := High(Actors) + 1;
end;

function TBZK2File.GetNumCameras: integer;
begin
   Result := High(Cameras) + 1;
end;

function TBZK2File.GetFilename : string;
begin
   Result := Filename;
end;

function TBZK2File.GetVoxelSection : PVoxelSection;
begin
   Result := VoxelSection;
end;

function TBZK2File.GetUseBZKMapXSL : Boolean;
begin
   Result := Settings.UseBZKMapXSL;
end;

function TBZK2File.GetLocalLighting : TVector3f;
begin
   Result.X := Settings.LocalLighting.X;
   Result.Y := Settings.LocalLighting.Y;
   Result.Z := Settings.LocalLighting.Z;
end;

procedure TBZK2File.GetLocalLighting(var r,g,b : integer);
begin
   r := Round(Settings.LocalLighting.X * 255);
   g := Round(Settings.LocalLighting.Y * 255);
   b := Round(Settings.LocalLighting.Z * 255);
end;

function TBZK2File.GetUseTrueMasterSectorSize: Boolean;
begin
   Result := Settings.UseTrueMasterSectorSize;
end;

function TBZK2File.GetInvertX : Boolean;
begin
   Result := Settings.InvertX;
end;

function TBZK2File.GetInvertY : Boolean;
begin
   Result := Settings.InvertY;
end;

function TBZK2File.GetInvertZ : Boolean;
begin
   Result := Settings.InvertZ;
end;


function TBZK2File.GetSector(ID : integer): TBZK2Sector;
begin
   if ID <= High(Sectors) then
      Result := Sectors[ID]
   else
      Result := nil;
end;

function TBZK2File.GetActor(ID : integer): TBZK2Actor;
begin
   if ID <= High(Actors) then
      Result := Actors[ID]
   else
      Result := nil;
end;

function TBZK2File.GetCamera(ID : integer): TBZK2Camera;
begin
   if ID <= High(Cameras) then
      Result := Cameras[ID]
   else
      Result := nil;
end;

function TBZK2File.GetColoursWithNormals : Boolean;
begin
   Result := Settings.ColoursWithNormals;
end;

function TBZK2File.GetMinBounds : TVector3i;
begin
   Result.X := Round(VoxelSection^.Tailer.MinBounds[1]);
   Result.Y := Round(VoxelSection^.Tailer.MinBounds[2]);
   Result.Z := Round(VoxelSection^.Tailer.MinBounds[3]);
end;

function TBZK2File.GetMaxBounds : TVector3i;
begin
   Result.X := Round(VoxelSection^.Tailer.MaxBounds[1]);
   Result.Y := Round(VoxelSection^.Tailer.MaxBounds[2]);
   Result.Z := Round(VoxelSection^.Tailer.MaxBounds[3]);
end;

function TBZK2File.GetCenterPosition: TVector3i;
Var
   MinBound,MaxBound : TVector3i;
begin
   MinBound := GetMinBounds;
   MaxBound := GetMaxBounds;
   Result.X := (MinBound.X + MaxBound.X) div 2;
   Result.Y := (MinBound.Y + MaxBound.Y) div 2;
   Result.Z := (MinBound.Z + MaxBound.Z) div 2;
end;

function TBZK2File.IsValid : Boolean;
begin
   Result := Valid;
end;

function TBZK2File.IsBZKMap : Boolean;
begin
   Result := BZKMap;
end;


// Sets
procedure TBZK2File.SetFilename (Value : string);
begin
   Filename := Value;
end;

procedure TBZK2File.SetUseBZKMapXSL(Value : Boolean);
begin
   Settings.UseBZKMapXSL := Value;
end;

procedure TBZK2File.SetLocalLighting( Value : TVector3f);
begin
   Settings.LocalLighting.X := Value.X;
   Settings.LocalLighting.Y := Value.Y;
   Settings.LocalLighting.Z := Value.Z;
end;

procedure TBZK2File.SetLocalLighting( r,g,b : integer);
begin
   Settings.LocalLighting.X := r / 255;
   Settings.LocalLighting.Y := g / 255;
   Settings.LocalLighting.Z := b / 255;
end;

procedure TBZK2File.SetUseTrueMasterSectorSize(Value : Boolean);
begin
   Settings.UseBZKMapXSL := Value;
end;

procedure TBZK2File.SetInvertX(Value : Boolean);
begin
   Settings.InvertX := Value;
end;

procedure TBZK2File.SetInvertY(Value : Boolean);
begin
   Settings.InvertY := Value;
end;

procedure TBZK2File.SetInvertZ(Value : Boolean);
begin
   Settings.InvertZ := Value;
end;

procedure TBZK2File.SetColoursWithNormals(Value : Boolean);
begin
   Settings.ColoursWithNormals := Value;
end;

procedure TBZK2File.SetMinBounds( Value : TVector3i);
begin
   VoxelSection^.Tailer.MinBounds[1] := Value.X;
   VoxelSection^.Tailer.MinBounds[2] := Value.Y;
   VoxelSection^.Tailer.MinBounds[3] := Value.Z;
end;

procedure TBZK2File.SetMaxBounds( Value : TVector3i);
begin
   VoxelSection^.Tailer.MaxBounds[1] := Value.X;
   VoxelSection^.Tailer.MaxBounds[2] := Value.Y;
   VoxelSection^.Tailer.MaxBounds[3] := Value.Z;
end;

procedure TBZK2File.SetIsBZKMap(Value : Boolean);
begin
   BZKMap := Value;
end;


// Moves
procedure TBZK2File.MoveCenterToPosition(Value : TVector3i);
var
   MinBound,MaxBound,Center,Offset : TVector3i;
begin
   MinBound := GetMinBounds;
   MaxBound := GetMaxBounds;
   Center := GetCenterPosition;
   Offset.X := Value.X - Center.X;
   Offset.Y := Value.Y - Center.Y;
   Offset.Z := Value.Z - Center.Z;
   SetMinBounds(SetVectorI(MinBound.X + Offset.X,MinBound.Y + Offset.Y,MinBound.Z + Offset.Z));
   SetMaxBounds(SetVectorI(MaxBound.X + Offset.X,MaxBound.Y + Offset.Y,MaxBound.Z + Offset.Z));
end;



// Adds
procedure TBZK2File.AddSector;
begin
   SetLength(Sectors,High(Sectors)+2);
   Sectors[High(Sectors)] := TBZK2Sector.Create;
end;

procedure TBZK2File.AddActor;
begin
   SetLength(Actors,High(Actors)+2);
   Actors[High(Actors)] := TBZK2Actor.Create;
end;

procedure TBZK2File.AddCamera;
begin
   SetLength(Cameras,High(Cameras)+2);
   Cameras[High(Cameras)] := TBZK2Camera.Create;
end;


// Removes
function TBZK2File.RemoveSector(ID : integer): Boolean;
var
   i : integer;
   Position : TVector3i;
begin
   Result := false;
   if ID <= High(Sectors) then
   begin
      i := ID;
      Position := Sectors[i].GetVoxelPosition;
      SectorMap[Position.X,Position.Y,Position.Z] := 0;
      while i < High(Sectors) do
      begin
         Sectors[i].Assign(Sectors[i+1]);
         Position := Sectors[i].GetVoxelPosition;
         SectorMap[Position.X,Position.Y,Position.Z] := i;
         inc(i);
      end;
      SetLength(Sectors,High(Sectors));
      // Here we scan all authors to make sure they aren't located at an
      // invalid place
      for i := Low(Actors) to High(Actors) do
      begin
         if Actors[i].GetLocation > High(Sectors) then
            Actors[i].SetLocation(High(Sectors));
      end;
      BuildConnections;
      Result := true;
   end;
end;

function TBZK2File.RemoveActor(ID : integer): Boolean;
var
   i : integer;
   Name : string;
begin
   Result := false;
   if ID <= High(Actors) then
   begin
      i := ID;
      Name := Actors[i].GetName;
      while i < High(Actors) do
      begin
         Actors[i].Assign(Actors[i+1]);
         inc(i);
      end;
      SetLength(Actors,High(Actors));
      // Here we scan all cameras to remove the ones that link this actor
      if High(Cameras) > -1 then
         if High(Actors) > -1 then
         begin
            for i := High(Cameras) downto Low(Cameras) do
            begin
               if Cameras[i].GetActor > High(Actors) then
                  Cameras[i].SetActor(High(Actors));
            end;
         end
         else
         begin
            for i := High(Cameras) downto Low(Cameras) do
            begin
               RemoveCamera(i);
            end;
         end;
      Result := true;
   end;
end;

function TBZK2File.RemoveCamera(ID : integer): Boolean;
var
   i : integer;
begin
   Result := false;
   if ID <= High(Cameras) then
   begin
       i := ID;
       while i < High(Cameras) do
       begin
          Cameras[i].Assign(Cameras[i+1]);
          inc(i);
       end;
       SetLength(Cameras,High(Cameras));
       Result := true;
   end;
end;


// Clears
procedure TBZK2File.ClearSectors;
var
   i : integer;
begin
   if High(Sectors) > -1 then
      for i := High(Sectors) downto Low(Sectors) do
      begin
         RemoveSector(i);
      end;
   SetLength(Sectors,0);
end;

procedure TBZK2File.ClearActors;
var
   i : integer;
begin
   if High(Actors) > -1 then
      for i := High(Actors) downto Low(Actors) do
      begin
         RemoveActor(i);
      end;
   SetLength(Actors,0);
end;

procedure TBZK2File.ClearCameras;
var
   i : integer;
begin
   if High(Cameras) > -1 then
      for i := High(Cameras) downto Low(Cameras) do
      begin
         RemoveCamera(i);
      end;
   SetLength(Cameras,0);
end;

procedure TBZK2File.ClearSectorMap;
var
   x,y : integer;
begin
   if High(SectorMap) > -1 then
      for x := Low(SectorMap) to High(SectorMap) do
      begin
         for y := Low(SectorMap[x]) to High(SectorMap[x]) do
            SetLength(SectorMap[x,y],0);
         SetLength(SectorMap[x],0);
      end;
   SetLength(SectorMap,0);
end;

// Random
function TBZK2File.CalculateColour(Value : TVector3i): TVector3i;
begin
   Result.X := Round(Value.X * Settings.LocalLighting.X);
   Result.Y := Round(Value.Y * Settings.LocalLighting.Y);
   Result.Z := Round(Value.Z * Settings.LocalLighting.Z);
end;

function TBZK2File.MultiplyVectorI(Value,Scale : TVector3i): TVector3i;
begin
   Result.X := Value.X * Scale.X;
   Result.Y := Value.Y * Scale.Y;
   Result.Z := Value.Z * Scale.Z;
end;


// Builds
procedure TBZK2File.BuildSectorMap;
var
   Sector : TBZK2Sector;
   Scale : TVector3i;
   xcount, ycount, zcount, x, y, z, xmax, ymax, zmax: Integer;
   v : TVoxelUnpacked;
begin
   if VoxelSection <> nil then
   begin
      // Pre-calculate some basic values.
      Scale.X := Round((VoxelSection^.Tailer.MaxBounds[1]-VoxelSection^.Tailer.MinBounds[1])/VoxelSection^.Tailer.xSize);
      Scale.Y := Round((VoxelSection^.Tailer.MaxBounds[2]-VoxelSection^.Tailer.MinBounds[2])/VoxelSection^.Tailer.ySize);
      Scale.Z := Round((VoxelSection^.Tailer.MaxBounds[3]-VoxelSection^.Tailer.MinBounds[3])/VoxelSection^.Tailer.zSize);

      ClearSectors;
      AddSector;
      // Let's setup the MasterSector.
      if not Settings.UseTrueMasterSectorSize then
         Sectors[0].SetVolume(255)
      else
         Sectors[0].SetVolume(SetVectorI(VoxelSection^.Tailer.XSize * Scale.X,VoxelSection^.Tailer.YSize * Scale.Y,VoxelSection^.Tailer.ZSize * Scale.Z));
      Sectors[0].SetName('location');

      Sectors[0].SetConnectionColours(bfdNorth,CalculateColour(Settings.MasterSectorColours[bfdNorth]));
      Sectors[0].SetConnectionColours(bfdEast,CalculateColour(Settings.MasterSectorColours[bfdEast]));
      Sectors[0].SetConnectionColours(bfdSouth,CalculateColour(Settings.MasterSectorColours[bfdSouth]));
      Sectors[0].SetConnectionColours(bfdWest,CalculateColour(Settings.MasterSectorColours[bfdWest]));
      Sectors[0].SetConnectionColours(bfdFloor,CalculateColour(Settings.MasterSectorColours[bfdFloor]));
      Sectors[0].SetConnectionColours(bfdCeiling,CalculateColour(Settings.MasterSectorColours[bfdCeiling]));

      // Now that the MasterSector is over, let's start to build the map
      // First, bringing back a feature from 1.2i
      if Settings.InvertX then
      begin
         xmax := VoxelSection^.Tailer.xSize-1;
         PrivateMembers.xInc := -1;
      end
      else
      begin
         xmax := 0;
         PrivateMembers.xInc := 1;
      end;

      if Settings.InvertY then
      begin
         ymax := VoxelSection^.Tailer.ySize-1;
         PrivateMembers.yInc := -1;
      end
      else
      begin
         ymax := 0;
         PrivateMembers.yInc := 1;
      end;

      if Settings.InvertZ then
      begin
         zmax := VoxelSection^.Tailer.zSize-1;
         PrivateMembers.zInc := -1;
      end
      else
      begin
         zmax := 0;
         PrivateMembers.zInc := 1;
      end;

      SetLength(SectorMap,VoxelSection^.Tailer.xSize,VoxelSection^.Tailer.ySize,VoxelSection^.Tailer.zSize);
      x := xmax;
      for xcount := 0 to VoxelSection^.Tailer.xSize-1 do
      begin
         y := ymax;
         for ycount := 0 to VoxelSection^.Tailer.ySize-1 do
         begin
            z := zmax;
            for zcount := 0 to VoxelSection^.Tailer.zSize-1 do
            begin
               VoxelSection^.GetVoxel(x,y,z,v);
               // If it's a blank space, then it's a sector.
               if not v.Used then
               begin
                  AddSector;
                  Sectors[High(Sectors)].SetVoxelPosition(SetVectorI(x,y,z));
                  Sectors[High(Sectors)].SetPosition(MultiplyVectorI(SetVectorI(x,y,z),Scale));
                  Sectors[High(Sectors)].SetVolume(Scale);
                  Sectors[High(Sectors)].SetName('location');
                  SectorMap[x,y,z] := High(Sectors);
               end
               else
                  SectorMap[x,y,z] := 0;
               z := z + PrivateMembers.zInc;
            end;
            y := y + PrivateMembers.yInc;
         end;
         x := x + PrivateMembers.xInc;
      end;
   end;
end;

procedure TBZK2File.SetMeAConnection(SectorNum,x,y,z : integer; Direction : TBZKFacesDirection);
var
   V : TVoxelUnpacked;
begin
   // Check borders... North.
   if VoxelSection^.GetVoxelSafe(X,Y,Z,V) then
   begin
      Sectors[SectorNum].SetConnection(Direction,SectorMap[X,Y,Z]);
   end
   else  // Border.
   begin
      Sectors[SectorNum].SetConnection(Direction,0);
   end;
end;

procedure TBZK2File.BuildConnections;
var
   i : integer;
   Position : TVector3i;
begin
   // If we have sectors, let's connect the dots.
   if High(Sectors) > -1 then
   begin
      for i := Low(Sectors) to High(Sectors) do
      begin
         Position := Sectors[i].GetVoxelPosition;
         // Check borders...
         SetMeAConnection(i,Position.X,Position.Y - PrivateMembers.yInc, Position.Z, bfdNorth);
         SetMeAConnection(i,Position.X + PrivateMembers.xInc,Position.Y, Position.Z, bfdEast);
         SetMeAConnection(i,Position.X,Position.Y + PrivateMembers.yInc, Position.Z, bfdSouth);
         SetMeAConnection(i,Position.X - PrivateMembers.xInc,Position.Y, Position.Z, bfdWest);
         SetMeAConnection(i,Position.X,Position.Y, Position.Z - PrivateMembers.zInc, bfdFloor);
         SetMeAConnection(i,Position.X,Position.Y, Position.Z + PrivateMembers.zInc, bfdCeiling);
      end;
   end;
end;

function TBZK2File.GetNormalX(V : TVoxelUnpacked) : single;
begin
   if VoxelSection^.Tailer.Unknown = 4 then
   begin
      Result := abs(RA2Normals[v.Normal].X);
   end
   else
   begin
      Result := abs(TSNormals[v.Normal].X);
   end;
end;

function TBZK2File.GetNormalY(V : TVoxelUnpacked) : single;
begin
   if VoxelSection^.Tailer.Unknown = 4 then
   begin
      Result := abs(RA2Normals[v.Normal].Y);
   end
   else
   begin
      Result := abs(TSNormals[v.Normal].Y);
   end;
end;

function TBZK2File.GetNormalZ(V : TVoxelUnpacked) : single;
begin
   if VoxelSection^.Tailer.Unknown = 4 then
   begin
      Result := abs(RA2Normals[v.Normal].Z);
   end
   else
   begin
      Result := abs(TSNormals[v.Normal].Z);
   end;
end;

procedure TBZK2File.SetFaceColour(SectorNum,x,y,z : integer; Direction : TBZKFacesDirection);
var
   V : TVoxelUnpacked;
   Normal : single;
   Colour : TVector3i;
begin
   // Painting each face works in the following way. First, we check
   // the neighboor.
   if VoxelSection^.GetVoxelSafe(X,Y,Z,V) then
   begin //Two options here: wall or not wall.
      if v.Used then
      begin // Wall
         // First, we find the normal influence
         Case (Direction) of
            bfdNorth: Normal := GetNormalY(v);
            bfdEast: Normal := GetNormalX(v);
            bfdSouth: Normal := GetNormalY(v);
            bfdWest: Normal := GetNormalX(v);
            bfdFloor: Normal := GetNormalZ(v);
            bfdCeiling: Normal := GetNormalZ(v);
         end;
         // We get the colour
         Colour.X := Round(GetRValue(VXLPalette[V.Colour]) * Normal * Settings.LocalLighting.X);
         Colour.Y := Round(GetGValue(VXLPalette[V.Colour]) * Normal * Settings.LocalLighting.Y);
         Colour.Z := Round(GetBValue(VXLPalette[V.Colour]) * Normal * Settings.LocalLighting.Z);

         Sectors[SectorNum].SetConnectionColours(Direction,Colour);
      end
      else  // Transparent
      begin
         Sectors[SectorNum].SetConnectionColours(Direction,SetVectorI(0,0,0));
      end;
   end // Here we go with Master Sector colours...
   else
   begin
      Sectors[SectorNum].SetConnectionColours(Direction,Sectors[0].GetConnectionColours(Direction));
   end;
end;

procedure TBZK2File.SetFaceColourWithoutNormals(SectorNum,x,y,z : integer; Direction : TBZKFacesDirection);
var
   V : TVoxelUnpacked;
   Colour : TVector3i;
begin
   // Painting each face works in the following way. First, we check
   // the neighboor.
   if VoxelSection^.GetVoxelSafe(X,Y,Z,V) then
   begin //Two options here: wall or not wall.
      if v.Used then
      begin // Wall
         // We get the colour
         Colour.X := Round(GetRValue(VXLPalette[V.Colour]) * Settings.LocalLighting.X);
         Colour.Y := Round(GetGValue(VXLPalette[V.Colour]) * Settings.LocalLighting.Y);
         Colour.Z := Round(GetBValue(VXLPalette[V.Colour]) * Settings.LocalLighting.Z);

         Sectors[SectorNum].SetConnectionColours(Direction,Colour);
      end
      else  // Transparent
      begin
         Sectors[SectorNum].SetConnectionColours(Direction,SetVectorI(0,0,0));
      end;
   end // Here we go with Master Sector colours...
   else
   begin
      Sectors[SectorNum].SetConnectionColours(Direction,Sectors[0].GetConnectionColours(Direction));
   end;
end;

procedure TBZK2File.BuildColours;
var
   i : integer;
   Position : TVector3i;
begin
   // If we have sectors, let's paint everyone, except the Master Sector.
   if High(Sectors) > -1 then
   begin
      if Settings.ColoursWithNormals then
      begin
         for i := 1 to High(Sectors) do
         begin
            Position := Sectors[i].GetVoxelPosition;
            // We have to paint 6 faces.
            SetFaceColour(i,Position.X,Position.Y - PrivateMembers.YInc,Position.Z,bfdNorth);
            SetFaceColour(i,Position.X + PrivateMembers.xInc,Position.Y,Position.Z,bfdEast);
            SetFaceColour(i,Position.X,Position.Y + PrivateMembers.YInc,Position.Z,bfdSouth);
            SetFaceColour(i,Position.X - PrivateMembers.xInc,Position.Y,Position.Z,bfdWest);
            SetFaceColour(i,Position.X,Position.Y,Position.Z - PrivateMembers.zInc,bfdFloor);
            SetFaceColour(i,Position.X,Position.Y,Position.Z + PrivateMembers.zInc,bfdCeiling);
         end;
      end
      else
      begin
         for i := 1 to High(Sectors) do
         begin
            Position := Sectors[i].GetVoxelPosition;
            // We have to paint 6 faces.
            SetFaceColourWithoutNormals(i,Position.X,Position.Y - PrivateMembers.YInc,Position.Z,bfdNorth);
            SetFaceColourWithoutNormals(i,Position.X + PrivateMembers.xInc,Position.Y,Position.Z,bfdEast);
            SetFaceColourWithoutNormals(i,Position.X,Position.Y + PrivateMembers.YInc,Position.Z,bfdSouth);
            SetFaceColourWithoutNormals(i,Position.X - PrivateMembers.xInc,Position.Y,Position.Z,bfdWest);
            SetFaceColourWithoutNormals(i,Position.X,Position.Y,Position.Z - PrivateMembers.zInc,bfdFloor);
            SetFaceColourWithoutNormals(i,Position.X,Position.Y,Position.Z + PrivateMembers.zInc,bfdCeiling);
         end;
      end;
   end;
end;

procedure TBZK2File.MoveSideToPosition(Direction : TBZKFacesDirection; Value : Integer);
var
   MinBound,MaxBound : TVector3i;
   Offset : Integer;
begin
   MinBound := GetMinBounds;
   MaxBound := GetMaxBounds;
   case (Direction) of
      bfdNorth:
      begin
         Offset := Value - MinBound.Y;
         MinBound.Y := Value;
         MaxBound.Y := MaxBound.Y + Offset;
      end;
      bfdSouth:
      begin
         Offset := Value - MaxBound.Y;
         MaxBound.Y := Value;
         MinBound.Y := MinBound.Y + Offset;
      end;
      bfdEast:
      begin
         Offset := Value - MaxBound.X;
         MaxBound.X := Value;
         MinBound.X := MinBound.X + Offset;
      end;
      bfdWest:
      begin
         Offset := Value - MinBound.X;
         MinBound.X := Value;
         MaxBound.X := MaxBound.X + Offset;
      end;
      bfdFloor:
      begin
         Offset := Value - MinBound.Z;
         MinBound.Z := Value;
         MaxBound.Z := MaxBound.Z + Offset;
      end;
      bfdCeiling:
      begin
         Offset := Value - MaxBound.Z;
         MaxBound.Z := Value;
         MinBound.Z := MinBound.Z + Offset;
      end;
   end;
   SetMinBounds(MinBound);
   SetMaxBounds(MaxBound);
end;

// Attach
procedure TBZK2File.Attach(var BZK2File : TBZK2File; Direction : TBZKFacesDirection; Offset : TVector3i);
begin
   if Valid and BZK2File.IsValid then
   begin
      case (Direction) of
         bfdNorth:
         begin
            MoveSideToPosition(Direction,Offset.Y);
         end;
         bfdSouth:
         begin
            MoveSideToPosition(Direction,Offset.Y);
         end;
         bfdEast:
         begin
            MoveSideToPosition(Direction,Offset.X);
         end;
         bfdWest:
         begin
            MoveSideToPosition(Direction,Offset.X);
         end;
         bfdFloor:
         begin
            MoveSideToPosition(Direction,Offset.Z);
         end;
         bfdCeiling:
         begin
            MoveSideToPosition(Direction,Offset.Z);
         end;
      end;
   end;
end;


end.
