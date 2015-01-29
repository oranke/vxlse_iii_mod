unit Voxel_Tools;

{ REVISION HISTORY
  2005/11/23 Fixed ApplyCubedNormals, by adding the Binary3DFloodFill stuff.
  2005/11/10 Added ApplyCubedNormals.
  2004/06/14 Added GetSmoothNormal, used for smoothing single voxel points.
  2004/05/02 Normal Scheme's removed, Apply Normals remade. RemoveRedundantVoxels Remade.
  20020224 Fixed non-current folder bugs in loadnormalschemes,
           added flag NO_FORMS to switch back to ParamStr(0) instead
           of Application.ExeName (for console apps, the extra certainty isn't
           worth 300 KB :-) (since ParamStr(0) works fine on my/most PC's)
  20011218 removed all WriteLns
  20011213 using multi-pass flood filling instead of recursion
  20011213 fixed bug in Internal
}

interface

Uses Voxel,normals,Voxel_Engine,math,math3d,Dialogs,Sysutils;

type
   TApplyNormalsResult = record
      applied,
      confused: Integer;
   end;

   TVoxelSmoothData = record
     Pos : TVector3i;
     V : TVoxelUnpacked;
   end;

   TDistanceUnit = record
      x,
      y,
      z,
      Distance : single;
   end;


const
   // 1.2 Cubed Normals Constants:
   DIST2 = 0.707106781186547524400844362104849; // sqrt(2)/2
   DIST3 = 0.577350269189625764509148780501957; // sqrt(3)/3
   LIMZERO = 0.0000000000001;
   TIP = 1;

type
   TBinaryMap = array of array of array of single; //byte;
   TBooleanMap = array of array of array of boolean;
   TVector3fMap = array of array of array of TVector3f;
   TDistanceArray = array of array of array of TDistanceUnit; //single;


//applies normals
function ApplyNormals(Voxel : TVoxelSection) : TApplyNormalsResult;
function ApplyCubedNormals(Voxel : TVoxelSection; Range,SmoothLevel : single; ContrastLevel : integer; SmoothMe,InfluenceMe,AffectOnlyNonNormalized : Boolean) : TApplyNormalsResult;
function ApplyInfluenceNormals(Voxel : TVoxelSection; Range,SmoothLevel : single; ContrastLevel : integer; SmoothMe,AffectOnlyNonNormalized,ImproveContrast : boolean) : TApplyNormalsResult;
function RemoveRedundantVoxels(Voxel : TVoxelSection) : integer;
function SmoothNormals(var Vxl : TVoxelSection) : TApplyNormalsResult;
function GetSmoothNormal(var Vxl : TVoxelSection; X,Y,Z,Normal : integer) : integer;

// Used in conjunction with new one to make the ultimate one :D
function RemoveRedundantVoxelsOld(voxel: TVoxelSection): integer;

// Random functions
function IsPointOK (const x,y,z,maxx,maxy,maxz : integer) : boolean;
procedure GetPreliminaryNormals(const BM: TBinaryMap; var FloatMap : TVector3fMap; const Dist: TDistanceArray; var V : TVoxelUnpacked; MidPoint,Range,_x,_y,_z : integer);
function GetNonZeroSign(const value : single) : shortint;
function GetTrueSign(var value,signal : single; minVar, maxVar : integer) : shortint;

// Normals functions
Function Norm2IndexRA2(N : TVector3f) : Byte;
Function Norm2IndexTS(N : TVector3f) : Byte;


implementation

{uses SysUtils;}

procedure VecToAngles(const _In: TVector3f ; var angles: TVector3f);
var
  forward_: Single;
  yaw, pitch: Single;
begin
  if (_In.z = 0) and (_In.x = 0) then
  begin
    yaw := 0;
    if (_In.z > 0) then
      pitch := 90
    else
      pitch := 270;
  end
  else
  begin
    if (_In.x <> 0) then
      yaw := Round(ArcTan2(_In.z, _In.x) * 180 / M_PI)
    else if (_In.z > 0) then
      yaw := 90
    else
      yaw := -90;
    if (yaw < 0) then
      yaw := yaw + 360;

    forward_ := sqrt(_In.x*_In.x + _In.z*_In.z);
    pitch := Round(ArcTan2(_In.y, forward_) * 180 / M_PI);
    if (pitch < 0) then
      pitch := pitch + 360;
  end;

  angles.x := -pitch;
  angles.y := yaw;
  angles.z := 0;
end;
       {
Function VectorLength(N : TVector3f) : Single;
begin
Result := sqrt(N.X*N.X + N.Y*N.Y + N.Z*N.Z);
end;   }
            {
Function SubtractVector(V1,V2 : TVector3f) : single;
begin
Result := Max(V1.X,V2.X)-Min(V1.X,V2.X);
Result := Result + Max(V1.Y,V2.Y)-Min(V1.Y,V2.Y);
Result := Result + Max(V1.Z,V2.Z)-Min(V1.Z,V2.Z);
end;

Function SubtractVector3f(V1,V2 : TVector3f) : TVector3f;
begin
Result.X := V1.X-V2.X;
Result.Y := V1.Y-V2.Y;
Result.Z := V1.Z-V2.Z;
end;             }

Function Norm2IndexRA2(N : TVector3f) : Byte;
var
   I : integer;
   thiserr,besterr : single;
   Bestindex : byte;
begin
   Bestindex := 0;
   besterr := VectorDistance(SetVector(0,0,0),SubtractVector(RA2Normals[0],N));
   for i := 1 to 244 do
   begin
      thiserr := VectorDistance(SetVector(0,0,0),SubtractVector(RA2Normals[i],N));
      if thiserr < besterr then
      begin
         besterr:=thiserr;
         bestindex:=i;
      end;
   end;
   Result := Bestindex;
end;

Function Norm2IndexTS(N : TVector3f) : Byte;
var
   I : integer;
   thiserr,besterr : single;
   Bestindex : byte;
begin
   Bestindex := 0;
   besterr := VectorDistance(SetVector(0,0,0),SubtractVector(TSNormals[0],N));
   for i := 1 to 35 do
   begin
      thiserr := VectorDistance(SetVector(0,0,0),SubtractVector(TSNormals[i],N));
      if thiserr < besterr then
      begin
         besterr:=thiserr;
         bestindex:=i;
      end;
   end;
   Result := Bestindex;
end;

Function Norm2Index(N : TVector3f; Vxl : TVoxelSection) : Byte;
begin
   if Vxl.Tailer.Unknown = 2 then
      Result := Norm2IndexTS(N)
   else if Vxl.Tailer.Unknown = 4 then
      Result := Norm2IndexRA2(N);
end;

//*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/
// Create Binary Map to aid calculating the cubed normals
//*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/
procedure CreateBinaryMap(var BinaryMap, FilledMap: TBinaryMap; var FloatMap: TVector3fMap; const Voxel : TVoxelSection; Range : Integer);
var
   x,y,z : integer;
   V : TVoxelUnpacked;
   DoubleRange : integer;
begin
   DoubleRange := 2 * Range;
   // Set memory for binary map. Extra memory avoids bound checking
   SetLength(BinaryMap,Voxel.Tailer.XSize+DoubleRange,Voxel.Tailer.YSize+DoubleRange,Voxel.Tailer.ZSize+DoubleRange);
   SetLength(FilledMap,Voxel.Tailer.XSize+DoubleRange,Voxel.Tailer.YSize+DoubleRange,Voxel.Tailer.ZSize+DoubleRange);
   SetLength(FloatMap,Voxel.Tailer.XSize+DoubleRange,Voxel.Tailer.YSize+DoubleRange,Voxel.Tailer.ZSize+DoubleRange);

   // Fill Filled Map
   for x := Low(FilledMap) to High(FilledMap) do
      for y := Low(FilledMap[x]) to High(FilledMap[x]) do
         for z := Low(FilledMap[x,y]) to High(FilledMap[x,y]) do
         begin
            BinaryMap[x,y,z] := 0;
            FilledMap[x,y,z] := 1;
         end;

   // Fill Float Map
   for x := Low(FloatMap) to High(FloatMap) do
      for y := Low(FloatMap[x]) to High(FloatMap[x]) do
         for z := Low(FloatMap[x,y]) to High(FloatMap[x,y]) do
         begin
            FloatMap[x,y,z].X := 0;
            FloatMap[x,y,z].Y := 0;
            FloatMap[x,y,z].Z := 0;
          end;

   // All used voxels will receive 1. Unused get 0.
   for x := 0 to Voxel.Tailer.XSize-1 do
      for y := 0 to Voxel.Tailer.YSize-1 do
         for z := 0 to Voxel.Tailer.ZSize-1 do
         begin
            // Get voxel data.
            voxel.GetVoxel(x,y,z,v);
            // Check if it's used.
            if v.Used then
            begin
               BinaryMap[x+Range,y+Range,z+Range] := 1;
               FilledMap[x+Range,y+Range,z+Range] := 0;
            end;
         end;
end;

// Verify if the point is valid in a range.
function IsPointOK (const x,y,z,maxx,maxy,maxz : integer) : boolean;
begin
   result := false;
   if (x < 0) or (x > maxx) then exit;
   if (y < 0) or (y > maxy) then exit;
   if (z < 0) or (z > maxz) then exit;
   result := true;
end;

//*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/
// 3D Binary Flood Fill to restore internal voxels
//*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/
procedure Binary3DFloodFill(var FM: TBinaryMap);
type
   T3DPosition = ^T3DPositionItem;
   T3DPositionItem = record
      x,y,z : integer;
      Next : T3DPosition;
   end;
   // Some basic stuff for the queue above.
   procedure AddPoint (var Start,Last : T3DPosition; x,y,z : integer);
   var
      NewPosition : T3DPosition;
   begin
      // This function adds a point to the queue.
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
      end;
      Last := NewPosition;
   end;
   // This Gets an Info from a Point and remove it from the queue
   procedure GetPoint (var Start,Last : T3DPosition; var x,y,z : integer);
   var
      Temp : T3DPosition;
   begin // Start will never be nil here. Flood must verify it
      x := Start^.x;
      y := Start^.y;
      z := Start^.z;
      Temp := Start;
      if Last = Start then
         Last := nil;
      Start := Start^.Next;
      Dispose(Temp);
   end;
//*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/
// 3D Binary Flood Fill starts here
//*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/
var
   List,Last : T3DPosition;
   x,y,z : integer;
   maxx,maxy,maxz : integer;
begin
   // Get max values for each.
   maxx := High(FM);
   maxy := High(FM[0]);
   maxz := High(FM[0,0]);

   // Start on (0,0,0);
   List := nil;
   Last := nil;
   AddPoint(List,Last,0,0,0);
   FM[0,0,0] := 0;
   // It flood and fill until the list is over.
   while List <> nil do
   begin
      // Get the currently position
      GetPoint(List,Last,x,y,z);
      // Check and add neighboors (6 faces)
      if IsPointOK(x-1,y,z,maxx,maxy,maxz) then
         if FM[x-1,y,z] = 1 then
         begin
            FM[x-1,y,z] := 0;
            AddPoint(List,Last,x-1,y,z);
         end;
      if IsPointOK(x+1,y,z,maxx,maxy,maxz) then
         if FM[x+1,y,z] = 1 then
         begin
            FM[x+1,y,z] := 0;
            AddPoint(List,Last,x+1,y,z);
         end;
      if IsPointOK(x,y-1,z,maxx,maxy,maxz) then
         if FM[x,y-1,z] = 1 then
         begin
            FM[x,y-1,z] := 0;
            AddPoint(List,Last,x,y-1,z);
         end;
      if IsPointOK(x,y+1,z,maxx,maxy,maxz) then
         if FM[x,y+1,z] = 1 then
         begin
            FM[x,y+1,z] := 0;
            AddPoint(List,Last,x,y+1,z);
         end;
      if IsPointOK(x,y,z-1,maxx,maxy,maxz) then
         if FM[x,y,z-1] = 1 then
         begin
            FM[x,y,z-1] := 0;
            AddPoint(List,Last,x,y,z-1);
         end;
      if IsPointOK(x,y,z+1,maxx,maxy,maxz) then
         if FM[x,y,z+1] = 1 then
         begin
            FM[x,y,z+1] := 0;
            AddPoint(List,Last,x,y,z+1);
         end;
   end;
end;

//*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/
// Merge Filled Map on Binary Map
//*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/
procedure MergeBinaryMaps(const Source : TBinaryMap; var Destiny : TBinaryMap);
var
   x,y,z : integer;
begin
   // Set '1' on destination for every '1' on source.
   for x := 0 to High(Source) do
      for y := 0 to High(Source[x]) do
         for z := 0 to High(Source[x,y]) do
         begin
            if Source[x,y,z] = 1 then
               Destiny[x,y,z] := 1;
         end;
end;

//*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/
// Calculate new normals
//*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/
procedure GetPreliminaryNormals(const BM: TBinaryMap; var FloatMap : TVector3fMap; const Dist: TDistanceArray; var V : TVoxelUnpacked; MidPoint,Range,_x,_y,_z : integer);
var
   Res : TVector3f;
   Sum : TVector3f;
   {Alpha,}x,y,z : integer;
   xx,yy,zz : byte;
   MyPoint : TVector3i;
   MinPoint,MaxPoint : TVector3i;
   MidPointMinusRange : integer;
begin
   // Security checkup.
   if not V.Used then exit;

   x := MidPoint + _x;
   y := MidPoint + _y;
   z := MidPoint + _z;

   MinPoint.X := x - Range;
   MaxPoint.X := x + Range;
   MinPoint.Y := y - Range;
   MaxPoint.Y := y + Range;
   MinPoint.Z := z - Range;
   MaxPoint.Z := z + Range;

   Res.X := 0;
   Res.Y := 0;
   Res.Z := 0;
   Sum.X := 0;
   Sum.Y := 0;
   Sum.Z := 0;

   // This is the centralizer factor, so it will make MyPoint points to
   // the position of the filter based on its center.
   MidPointMinusRange := MidPoint - Range;

   // Time to get a rx, ry and rz from the binary map
   for xx := MinPoint.X to MaxPoint.X do
      for yy := MinPoint.Y to MaxPoint.Y do
         for zz := MinPoint.Z to MaxPoint.Z do
         begin
            // Set MyPoint
            MyPoint.X := xx - MinPoint.X + MidPointMinusRange;
            MyPoint.Y := yy - MinPoint.Y + MidPointMinusRange;
            MyPoint.Z := zz - MinPoint.Z + MidPointMinusRange;
            //Alpha := abs(x - xx) + abs(y - yy) + abs(z - zz);
            Res.X := Res.X + (BM[xx,yy,zz] * Dist[MyPoint.X,MyPoint.Y,MyPoint.Z].X);
            Res.Y := Res.Y + (BM[xx,yy,zz] * Dist[MyPoint.X,MyPoint.Y,MyPoint.Z].Y);
            Res.Z := Res.Z + (BM[xx,yy,zz] * Dist[MyPoint.X,MyPoint.Y,MyPoint.Z].Z);
            Sum.X := Sum.X + abs(BM[xx,yy,zz] * Dist[MyPoint.X,MyPoint.Y,MyPoint.Z].X);
            Sum.Y := Sum.Y + abs(BM[xx,yy,zz] * Dist[MyPoint.X,MyPoint.Y,MyPoint.Z].Y);
            Sum.Z := Sum.Z + abs(BM[xx,yy,zz] * Dist[MyPoint.X,MyPoint.Y,MyPoint.Z].Z);
         end;

   if Sum.X <> 0 then
      FloatMap[x,y,z].X := Res.X / Sum.X
   else
      FloatMap[x,y,z].X := 0;
   if Sum.Y <> 0 then
      FloatMap[x,y,z].Y := Res.Y / Sum.Y
   else
      FloatMap[x,y,z].Y := 0;
   if Sum.Z <> 0 then
      FloatMap[x,y,z].Z := Res.Z / Sum.Z
   else
      FloatMap[x,y,z].Z := 0;
end;

function GetNonZeroSign(const value : single) : shortint;
begin
   Result := sign(value);
   if Result = 0 then
      Result := 1;
 end;

function GetTrueSign(var value,signal : single; minVar, maxVar: integer) : shortint;
begin
   Result := sign(value);
   if Result = 0 then
   begin
      Result := sign(signal);
      if Result = 0 then
      begin
         // we need to do some magic here.
         if minVar <= (maxVar div 2) then
            Result := 1
         else
            Result := -1;
      end;
   end;
end;

procedure GetNewNormals(var Voxel : TVoxelSection; const BM: TVector3fMap; const Dist : TDistanceArray; var V : TVoxelUnpacked; MidPoint,Range,_x,_y,_z : integer; var applied : integer);
var
   res : TVector3f;
   signal : TVector3f;
   newvalue : single;
   x,y,z : integer;
   xx,yy,zz : integer;
   xcounter,ycounter,zcounter : single;
   MyPoint : TVector3i;
   CurrentVoxel : TVoxelUnpacked;
   MinPoint,MaxPoint : TVector3i;
   MidPointMinusRange : integer;
begin
   // Security checkup.
   if not V.Used then exit;

   x := MidPoint + _x;
   y := MidPoint + _y;
   z := MidPoint + _z;

   MinPoint.X := x - Range;
   MaxPoint.X := x + Range;
   MinPoint.Y := y - Range;
   MaxPoint.Y := y + Range;
   MinPoint.Z := z - Range;
   MaxPoint.Z := z + Range;

   res.X := 0;
   res.Y := 0;
   res.Z := 0;
   signal.X := 0;
   signal.Y := 0;
   signal.Z := 0;
   xcounter := 0;
   ycounter := 0;
   zcounter := 0;

   // This is the centralizer factor, so it will make MyPoint points to
   // the position of the filter based on its center.
   MidPointMinusRange := MidPoint - Range;

   // Let's grab the new res
   for xx := MinPoint.X to MaxPoint.X do
      for yy := MinPoint.Y to MaxPoint.Y do
         for zz := MinPoint.Z to MaxPoint.Z do
         begin
            // Set MyPoint
            MyPoint.X := xx - MinPoint.X + MidPointMinusRange;
            MyPoint.Y := yy - MinPoint.Y + MidPointMinusRange;
            MyPoint.Z := zz - MinPoint.Z + MidPointMinusRange;
            if Voxel.GetVoxelSafe(xx - MidPoint,yy - MidPoint,zz - MidPoint,CurrentVoxel) then
               if CurrentVoxel.Used then
               begin
                  newvalue := Dist[MyPoint.X,MyPoint.Y,MyPoint.Z].Distance * BM[xx,yy,zz].X;
                  res.X := res.X + abs(NewValue);// * GetNonZeroSign(BM[xx,yy,zz].X));
                  signal.X := signal.X + NewValue;
                  xcounter := xcounter + Dist[MyPoint.X,MyPoint.Y,MyPoint.Z].Distance;
                  NewValue := Dist[MyPoint.X,MyPoint.Y,MyPoint.Z].Distance * BM[xx,yy,zz].Y;
                  res.Y := res.Y + abs(NewValue); // * GetNonZeroSign(BM[xx,yy,zz].Y));
                  signal.X := signal.X + NewValue;
                  ycounter := ycounter + Dist[MyPoint.X,MyPoint.Y,MyPoint.Z].Distance;
                  NewValue := Dist[MyPoint.X,MyPoint.Y,MyPoint.Z].Distance * BM[xx,yy,zz].Z;
                  res.Z := res.Z + abs(NewValue); // * GetNonZeroSign(BM[xx,yy,zz].Z));
                  signal.Z := signal.Z + NewValue;
                  zcounter := zcounter + Dist[MyPoint.X,MyPoint.Y,MyPoint.Z].Distance;
               end;
         end;
   if xcounter > 0 then
      res.X := (res.X / xcounter) * GetTrueSign(BM[x,y,z].X,signal.X,_x,Voxel.Tailer.XSize);
   if ycounter > 0 then
      res.Y := (res.Y / ycounter) * GetTrueSign(BM[x,y,z].Y,signal.Y,_y,Voxel.Tailer.YSize);
   if zcounter > 0 then
      res.Z := (res.Z / zcounter) * GetTrueSign(BM[x,y,z].Z,signal.Z,_z,Voxel.Tailer.ZSize);

   // Normalization process.
   Normalize(res);

   if Voxel.Tailer.Unknown = 4 then
      V.Normal := Norm2IndexRA2(res)
   else
      V.Normal := Norm2IndexTS(res);

   // Apply
   Voxel.SetVoxel(_x,_y,_z,V);
   inc(applied);
end;

procedure GetNewNormalsWithNoSmooth(var Voxel: TVoxelSection; const BM: TVector3fMap; var V : TVoxelUnpacked; Range,_x,_y,_z : integer; var applied : integer);
var
   x,y,z : integer;
begin
   // Security checkup.
   if not V.Used then exit;

   x := _x + Range;
   y := _y + Range;
   z := _z + Range;

   // Normalization process.
   Normalize(BM[x,y,z]);

   if Voxel.Tailer.Unknown = 4 then
      V.Normal := Norm2IndexRA2(BM[x,y,z])
   else
      V.Normal := Norm2IndexTS(BM[x,y,z]);

   // Apply
   Voxel.SetVoxel(_x,_y,_z,V);
   inc(applied);
end;

procedure ResetBinaryMap(const Voxel: TVoxelSection; var Map : TBinaryMap; Range: integer);
var
   DoubleRange : integer;
   x,y,z : integer;
begin
   DoubleRange := 2 * Range;
   // Set memory for binary map. Extra memory avoids bound checking
   SetLength(Map,Voxel.Tailer.XSize+DoubleRange,Voxel.Tailer.YSize+DoubleRange,Voxel.Tailer.ZSize+DoubleRange);

   // Clear map
   for x := Low(Map) to High(Map) do
      for y := Low(Map) to High(Map[x]) do
         for z := Low(Map) to High(Map[x,y]) do
             Map[x,y,z] := 0;
end;

//*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/
// Build the influence area for both Influence and Cubed normalizer.
//*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/
procedure BuildInfluenceArea(const Voxel : TVoxelSection; var Map : TBinaryMap; Range : integer; Prize : single);
var
   x,y,z : integer;
   StartPoint,FinalPoint : integer;
   V : TVoxelUnpacked;
begin
   // Scan on z direction.
   for x := Low(Map) to High(Map) do
      for y := Low(Map) to High(Map[x]) do
      begin
         // Lower scan
         z := Low(Map[x,y]);
         StartPoint := -1;
         while (z <= High(Map[x,y])) and (StartPoint = -1) do
         begin
            if Voxel.GetVoxelSafe(x-Range,y-Range,z-Range,v) then
            begin
               if v.Used then
               begin
                  StartPoint := z;
               end;
            end;
            inc(z);
         end;
         // Higher scan (only happens if the lower one worked)
         if StartPoint <> -1 then
         begin
            z := High(Map[x,y]);
            FinalPoint := -1;
            while (z >= Low(Map[x,y])) and (FinalPoint = -1) do
            begin
               if Voxel.GetVoxelSafe(x-Range,y-Range,z-Range,v) then
               begin
                  if v.Used then
                  begin
                     FinalPoint := z;
                  end;
               end;
               dec(z);
            end;
            // Now, we do the painting.
            z := StartPoint;
            while z <= FinalPoint do
            begin
               Map[x,y,z] := Map[x,y,z] + Prize;
               inc(z);
            end;
         end;
      end;

   // Scan on x direction.
   for y := Low(Map[0]) to High(Map[0]) do
      for z := Low(Map[0,y]) to High(Map[0,y]) do
      begin
         // Lower scan
         x := Low(Map);
         StartPoint := -1;
         while (x <= High(Map)) and (StartPoint = -1) do
         begin
            if Voxel.GetVoxelSafe(x-Range,y-Range,z-Range,v) then
            begin
               if v.Used then
               begin
                  StartPoint := x;
               end;
            end;
            inc(x);
         end;
         // Higher scan (only happens if the lower one worked)
         if StartPoint <> -1 then
         begin
            x := High(Map);
            FinalPoint := -1;
            while (x >= Low(Map)) and (FinalPoint = -1) do
            begin
               if Voxel.GetVoxelSafe(x-Range,y-Range,z-Range,v) then
               begin
                  if v.Used then
                  begin
                     FinalPoint := x;
                  end;
               end;
               dec(x);
            end;
            // Now, we do the painting.
            x := StartPoint;
            while x <= FinalPoint do
            begin
               Map[x,y,z] := Map[x,y,z] + Prize;
               inc(x);
            end;
         end;
      end;

   // Scan on y direction.
   for x := Low(Map) to High(Map) do
      for z := Low(Map[x,0]) to High(Map[x,0]) do
      begin
         // Lower scan
         y := Low(Map[x]);
         StartPoint := -1;
         while (y <= High(Map[x])) and (StartPoint = -1) do
         begin
            if Voxel.GetVoxelSafe(x-Range,y-Range,z-Range,v) then
            begin
               if v.Used then
               begin
                  StartPoint := y;
               end;
            end;
            inc(y);
         end;
         // Higher scan (only happens if the lower one worked)
         if StartPoint <> -1 then
         begin
            y := High(Map[x]);
            FinalPoint := -1;
            while (y >= Low(Map[x])) and (FinalPoint = -1) do
            begin
               if Voxel.GetVoxelSafe(x-Range,y-Range,z-Range,v) then
               begin
                  if v.Used then
                  begin
                     FinalPoint := y;
                  end;
               end;
               dec(y);
            end;
            // Now, we do the painting.
            y := StartPoint;
            while y <= FinalPoint do
            begin
               Map[x,y,z] := Map[x,y,z] + Prize;
               inc(y);
            end;
         end;
      end;
end;

procedure AddSurfacePlusOneInfoToInfluenceArea(var Map : TBinaryMap; WinValue : single);
const
   C_PART_OF_VOLUME = 3;
var
   x,y,z : integer;
   IsInsideVolume : boolean;
   MaxX,MaxY,MaxZ : integer;
begin
   MaxX := High(Map);
   MaxY := High(Map[0]);
   MaxZ := High(Map[0,0]);
   // Scan on z direction.
   for x := Low(Map) to High(Map) do
      for y := Low(Map) to High(Map[x]) do
      begin
         // Lower scan
         z := Low(Map[x,y]);
         IsInsideVolume := false;
         while z <= High(Map[x,y]) do
         begin
            if IsInsideVolume then
            begin
               while (z <= High(Map[x,y])) and IsInsideVolume do
               begin
                  if Map[x,y,z] < C_PART_OF_VOLUME then
                  begin
                     Map[x,y,z-1] := WinValue;
                     if (z - 2) >= Low(Map[x,y]) then
                        if Map[x,y,z-2] >= C_PART_OF_VOLUME then
                           Map[x,y,z-2] := WinValue;
                     IsInsideVolume := false;
                  end;
                  inc(z);
               end;
            end
            else // not inside the volume..
            begin
               while (z <= High(Map[x,y])) and (not IsInsideVolume) do
               begin
                  if Map[x,y,z] >= C_PART_OF_VOLUME then
                  begin
                     Map[x,y,z] := WinValue;
                     if (z + 1) <= High(Map[x,y]) then
                        if Map[x,y,z+1] >= C_PART_OF_VOLUME then
                           Map[x,y,z+1] := WinValue;
                     IsInsideVolume := true;
                  end;
                  inc(z);
               end;
            end;
         end;
      end;

   // Scan on x direction.
   for y := Low(Map[0]) to High(Map[0]) do
      for z := Low(Map[0,y]) to High(Map[0,y]) do
      begin
         // Lower scan
         x := Low(Map);
         IsInsideVolume := false;
         while x <= High(Map) do
         begin
            if IsInsideVolume then
            begin
               while (x <= High(Map)) and IsInsideVolume do
               begin
                  if Map[x,y,z] < C_PART_OF_VOLUME then
                  begin
                     Map[x-1,y,z] := WinValue;
                     if (x - 2) >= Low(Map) then
                        if Map[x-2,y,z] >= C_PART_OF_VOLUME then
                           Map[x-2,y,z] := WinValue;
                     IsInsideVolume := false;
                  end;
                  inc(x);
               end;
            end
            else // not inside the volume..
            begin
               while (x <= High(Map)) and (not IsInsideVolume) do
               begin
                  if Map[x,y,z] >= C_PART_OF_VOLUME then
                  begin
                     Map[x,y,z] := WinValue;
                     if (x + 1) <= High(Map) then
                        if Map[x+1,y,z] >= C_PART_OF_VOLUME then
                           Map[x+1,y,z] := WinValue;
                     IsInsideVolume := true;
                  end;
                  inc(x);
               end;
            end;
         end;
      end;

   // Scan on y direction.
   for x := Low(Map) to High(Map) do
      for z := Low(Map[x,0]) to High(Map[x,0]) do
      begin
         // Lower scan
         y := Low(Map[x]);
         IsInsideVolume := false;
         while y <= High(Map[x]) do
         begin
            if IsInsideVolume then
            begin
               while (y <= High(Map[x])) and IsInsideVolume do
               begin
                  if Map[x,y,z] < C_PART_OF_VOLUME then
                  begin
                     Map[x,y-1,z] := WinValue;
                     if (y - 2) >= Low(Map[x]) then
                        if Map[x,y-2,z] >= C_PART_OF_VOLUME then
                           Map[x,y-2,z] := WinValue;
                     IsInsideVolume := false;
                  end;
                  inc(y);
               end;
            end
            else // not inside the volume..
            begin
               while (y <= High(Map[x])) and (not IsInsideVolume) do
               begin
                  if Map[x,y,z] >= C_PART_OF_VOLUME then
                  begin
                     Map[x,y,z] := WinValue;
                     if (y + 1) <= High(Map[x]) then
                        if Map[x,y+1,z] >= C_PART_OF_VOLUME then
                           Map[x,y+1,z] := WinValue;
                     IsInsideVolume := true;
                  end;
                  inc(y);
               end;
            end;
         end;
      end;
end;

procedure AddSurfaceInfoToInfluenceArea(var Map : TBinaryMap; WinValue : single);
const
   C_PART_OF_VOLUME = 3;
var
   x,y,z : integer;
   IsInsideVolume : boolean;
   MaxX,MaxY,MaxZ : integer;
begin
   MaxX := High(Map);
   MaxY := High(Map[0]);
   MaxZ := High(Map[0,0]);
   // Scan on z direction.
   for x := Low(Map) to High(Map) do
      for y := Low(Map) to High(Map[x]) do
      begin
         // Lower scan
         z := Low(Map[x,y]);
         IsInsideVolume := false;
         while z <= High(Map[x,y]) do
         begin
            if IsInsideVolume then
            begin
               while (z <= High(Map[x,y])) and IsInsideVolume do
               begin
                  if Map[x,y,z] < C_PART_OF_VOLUME then
                  begin
                     Map[x,y,z-1] := WinValue;
                    if IsPointOK(x-1,y-1,z-2,MaxX,MaxY,MaxZ) then
                     begin
                        if Map[x-1,y-1,z-2] < C_PART_OF_VOLUME then
                           Map[x,y,z-2] := WinValue;
                     end
                     else if IsPointOK(x,y-1,z-2,MaxX,MaxY,MaxZ) then
                     begin
                        if Map[x,y-1,z-2] < C_PART_OF_VOLUME then
                           Map[x,y,z-2] := WinValue;
                     end
                     else if IsPointOK(x+1,y-1,z-2,MaxX,MaxY,MaxZ) then
                     begin
                        if Map[x+1,y-1,z-2] < C_PART_OF_VOLUME then
                           Map[x,y,z-2] := WinValue;
                     end
                     else if IsPointOK(x-1,y,z-2,MaxX,MaxY,MaxZ) then
                     begin
                        if Map[x-1,y,z-2] < C_PART_OF_VOLUME then
                           Map[x,y,z-2] := WinValue;
                     end
                     else if IsPointOK(x+1,y,z-2,MaxX,MaxY,MaxZ) then
                     begin
                        if Map[x+1,y,z-2] < C_PART_OF_VOLUME then
                           Map[x,y,z-2] := WinValue;
                     end
                     else if IsPointOK(x-1,y+1,z-2,MaxX,MaxY,MaxZ) then
                     begin
                        if Map[x-1,y+1,z-2] < C_PART_OF_VOLUME then
                           Map[x,y,z-2] := WinValue;
                     end
                     else if IsPointOK(x,y+1,z-2,MaxX,MaxY,MaxZ) then
                     begin
                        if Map[x,y+1,z-2] < C_PART_OF_VOLUME then
                           Map[x,y,z-2] := WinValue;
                     end
                     else if IsPointOK(x+1,y+1,z-2,MaxX,MaxY,MaxZ) then
                     begin
                        if Map[x+1,y+1,z-2] < C_PART_OF_VOLUME then
                           Map[x,y,z-2] := WinValue;
                     end;
                     IsInsideVolume := false;
                  end;
                  inc(z);
               end;
            end
            else // not inside the volume..
            begin
               while (z <= High(Map[x,y])) and (not IsInsideVolume) do
               begin
                  if Map[x,y,z] >= C_PART_OF_VOLUME then
                  begin
                     Map[x,y,z] := WinValue;
                     if IsPointOK(x-1,y-1,z+1,MaxX,MaxY,MaxZ) then
                     begin
                        if Map[x-1,y-1,z+1] < C_PART_OF_VOLUME then
                           Map[x,y,z+1] := WinValue;
                     end
                     else if IsPointOK(x,y-1,z+1,MaxX,MaxY,MaxZ) then
                     begin
                        if Map[x,y-1,z+1] < C_PART_OF_VOLUME then
                           Map[x,y,z+1] := WinValue;
                     end
                     else if IsPointOK(x+1,y-1,z+1,MaxX,MaxY,MaxZ) then
                     begin
                        if Map[x+1,y-1,z+1] < C_PART_OF_VOLUME then
                           Map[x,y,z+1] := WinValue;
                     end
                     else if IsPointOK(x-1,y,z+1,MaxX,MaxY,MaxZ) then
                     begin
                        if Map[x-1,y,z+1] < C_PART_OF_VOLUME then
                           Map[x,y,z+1] := WinValue;
                     end
                     else if IsPointOK(x+1,y,z+1,MaxX,MaxY,MaxZ) then
                     begin
                        if Map[x+1,y,z+1] < C_PART_OF_VOLUME then
                           Map[x,y,z+1] := WinValue;
                     end
                     else if IsPointOK(x-1,y+1,z+1,MaxX,MaxY,MaxZ) then
                     begin
                        if Map[x-1,y+1,z+1] < C_PART_OF_VOLUME then
                           Map[x,y,z+1] := WinValue;
                     end
                     else if IsPointOK(x,y+1,z+1,MaxX,MaxY,MaxZ) then
                     begin
                        if Map[x,y+1,z+1] < C_PART_OF_VOLUME then
                           Map[x,y,z+1] := WinValue;
                     end
                     else if IsPointOK(x+1,y+1,z+1,MaxX,MaxY,MaxZ) then
                     begin
                        if Map[x+1,y+1,z+1] < C_PART_OF_VOLUME then
                           Map[x,y,z+1] := WinValue;
                     end;
                     IsInsideVolume := true;
                  end;
                  inc(z);
               end;
            end;
         end;
      end;

   // Scan on x direction.
   for y := Low(Map[0]) to High(Map[0]) do
      for z := Low(Map[0,y]) to High(Map[0,y]) do
      begin
         // Lower scan
         x := Low(Map);
         IsInsideVolume := false;
         while x <= High(Map) do
         begin
            if IsInsideVolume then
            begin
               while (x <= High(Map)) and IsInsideVolume do
               begin
                  if Map[x,y,z] < C_PART_OF_VOLUME then
                  begin
                     Map[x-1,y,z] := WinValue;
                     if IsPointOK(x-2,y-1,z-1,MaxX,MaxY,MaxZ) then
                     begin
                        if Map[x-2,y-1,z-1] < C_PART_OF_VOLUME then
                           Map[x-2,y,z] := WinValue;
                     end
                     else if IsPointOK(x-2,y-1,z,MaxX,MaxY,MaxZ) then
                     begin
                        if Map[x-2,y-1,z] < C_PART_OF_VOLUME then
                           Map[x-2,y,z] := WinValue;
                     end
                     else if IsPointOK(x-2,y-1,z+1,MaxX,MaxY,MaxZ) then
                     begin
                        if Map[x-2,y-1,z+1] < C_PART_OF_VOLUME then
                           Map[x-2,y,z] := WinValue;
                     end
                     else if IsPointOK(x-2,y,z-1,MaxX,MaxY,MaxZ) then
                     begin
                        if Map[x-2,y,z-1] < C_PART_OF_VOLUME then
                           Map[x-2,y,z] := WinValue;
                     end
                     else if IsPointOK(x-2,y,z+1,MaxX,MaxY,MaxZ) then
                     begin
                        if Map[x-2,y,z+1] < C_PART_OF_VOLUME then
                           Map[x-2,y,z] := WinValue;
                     end
                     else if IsPointOK(x-2,y+1,z-1,MaxX,MaxY,MaxZ) then
                     begin
                        if Map[x-2,y+1,z-1] < C_PART_OF_VOLUME then
                           Map[x-2,y,z] := WinValue;
                     end
                     else if IsPointOK(x-2,y+1,z,MaxX,MaxY,MaxZ) then
                     begin
                        if Map[x-2,y+1,z] < C_PART_OF_VOLUME then
                           Map[x-2,y,z] := WinValue;
                     end
                     else if IsPointOK(x-2,y+1,z+1,MaxX,MaxY,MaxZ) then
                     begin
                        if Map[x-2,y+1,z+1] < C_PART_OF_VOLUME then
                           Map[x-2,y,z] := WinValue;
                     end;
                     IsInsideVolume := false;
                  end;
                  inc(x);
               end;
            end
            else // not inside the volume..
            begin
               while (x <= High(Map)) and (not IsInsideVolume) do
               begin
                  if Map[x,y,z] >= C_PART_OF_VOLUME then
                  begin
                     Map[x,y,z] := WinValue;
                     if IsPointOK(x+1,y-1,z-1,MaxX,MaxY,MaxZ) then
                     begin
                        if Map[x+1,y-1,z-1] < C_PART_OF_VOLUME then
                           Map[x+1,y,z] := WinValue;
                     end
                     else if IsPointOK(x+1,y-1,z,MaxX,MaxY,MaxZ) then
                     begin
                        if Map[x+1,y-1,z] < C_PART_OF_VOLUME then
                           Map[x+1,y,z] := WinValue;
                     end
                     else if IsPointOK(x+1,y-1,z+1,MaxX,MaxY,MaxZ) then
                     begin
                        if Map[x+1,y-1,z+1] < C_PART_OF_VOLUME then
                           Map[x+1,y,z] := WinValue;
                     end
                     else if IsPointOK(x+1,y,z-1,MaxX,MaxY,MaxZ) then
                     begin
                        if Map[x+1,y,z-1] < C_PART_OF_VOLUME then
                           Map[x+1,y,z] := WinValue;
                     end
                     else if IsPointOK(x+1,y,z+1,MaxX,MaxY,MaxZ) then
                     begin
                        if Map[x+1,y,z+1] < C_PART_OF_VOLUME then
                           Map[x+1,y,z] := WinValue;
                     end
                     else if IsPointOK(x+1,y+1,z-1,MaxX,MaxY,MaxZ) then
                     begin
                        if Map[x+1,y+1,z-1] < C_PART_OF_VOLUME then
                           Map[x+1,y,z] := WinValue;
                     end
                     else if IsPointOK(x+1,y+1,z,MaxX,MaxY,MaxZ) then
                     begin
                        if Map[x+1,y+1,z] < C_PART_OF_VOLUME then
                           Map[x+1,y,z] := WinValue;
                     end
                     else if IsPointOK(x+1,y+1,z+1,MaxX,MaxY,MaxZ) then
                     begin
                        if Map[x+1,y+1,z+1] < C_PART_OF_VOLUME then
                           Map[x+1,y,z] := WinValue;
                     end;
                     IsInsideVolume := true;
                  end;
                  inc(x);
               end;
            end;
         end;
      end;

   // Scan on y direction.
   for x := Low(Map) to High(Map) do
      for z := Low(Map[x,0]) to High(Map[x,0]) do
      begin
         // Lower scan
         y := Low(Map[x]);
         IsInsideVolume := false;
         while y <= High(Map[x]) do
         begin
            if IsInsideVolume then
            begin
               while (y <= High(Map[x])) and IsInsideVolume do
               begin
                  if Map[x,y,z] < C_PART_OF_VOLUME then
                  begin
                     Map[x,y-1,z] := WinValue;
                     if IsPointOK(x-1,y-2,z-1,MaxX,MaxY,MaxZ) then
                     begin
                        if Map[x-1,y-2,z-1] < C_PART_OF_VOLUME then
                           Map[x,y-2,z] := WinValue;
                     end
                     else if IsPointOK(x,y-2,z-1,MaxX,MaxY,MaxZ) then
                     begin
                        if Map[x,y-2,z-1] < C_PART_OF_VOLUME then
                           Map[x,y-2,z] := WinValue;
                     end
                     else if IsPointOK(x+1,y-2,z-1,MaxX,MaxY,MaxZ) then
                     begin
                        if Map[x+1,y-2,z-1] < C_PART_OF_VOLUME then
                           Map[x,y-2,z] := WinValue;
                     end
                     else if IsPointOK(x-1,y-2,z,MaxX,MaxY,MaxZ) then
                     begin
                        if Map[x-1,y-2,z] < C_PART_OF_VOLUME then
                           Map[x,y-2,z] := WinValue;
                     end
                     else if IsPointOK(x+1,y-2,z,MaxX,MaxY,MaxZ) then
                     begin
                        if Map[x+1,y-2,z] < C_PART_OF_VOLUME then
                           Map[x,y-2,z] := WinValue;
                     end
                     else if IsPointOK(x-1,y-2,z+1,MaxX,MaxY,MaxZ) then
                     begin
                        if Map[x-1,y-2,z+1] < C_PART_OF_VOLUME then
                           Map[x,y-2,z] := WinValue;
                     end
                     else if IsPointOK(x,y-2,z+1,MaxX,MaxY,MaxZ) then
                     begin
                        if Map[x,y-2,z+1] < C_PART_OF_VOLUME then
                           Map[x,y-2,z] := WinValue;
                     end
                     else if IsPointOK(x+1,y-2,z+1,MaxX,MaxY,MaxZ) then
                     begin
                        if Map[x+1,y-2,z+1] < C_PART_OF_VOLUME then
                           Map[x,y-2,z] := WinValue;
                     end;
                     IsInsideVolume := false;
                  end;
                  inc(y);
               end;
            end
            else // not inside the volume..
            begin
               while (y <= High(Map[x])) and (not IsInsideVolume) do
               begin
                  if Map[x,y,z] >= C_PART_OF_VOLUME then
                  begin
                     Map[x,y,z] := WinValue;
                     if IsPointOK(x-1,y+1,z-1,MaxX,MaxY,MaxZ) then
                     begin
                        if Map[x-1,y+1,z-1] < C_PART_OF_VOLUME then
                           Map[x,y+1,z] := WinValue;
                     end
                     else if IsPointOK(x,y+1,z-1,MaxX,MaxY,MaxZ) then
                     begin
                        if Map[x,y+1,z-1] < C_PART_OF_VOLUME then
                           Map[x,y+1,z] := WinValue;
                     end
                     else if IsPointOK(x+1,y+1,z-1,MaxX,MaxY,MaxZ) then
                     begin
                        if Map[x+1,y+1,z-1] < C_PART_OF_VOLUME then
                           Map[x,y+1,z] := WinValue;
                     end
                     else if IsPointOK(x-1,y+1,z,MaxX,MaxY,MaxZ) then
                     begin
                        if Map[x-1,y+1,z] < C_PART_OF_VOLUME then
                           Map[x,y+1,z] := WinValue;
                     end
                     else if IsPointOK(x+1,y+1,z,MaxX,MaxY,MaxZ) then
                     begin
                        if Map[x+1,y+1,z] < C_PART_OF_VOLUME then
                           Map[x,y+1,z] := WinValue;
                     end
                     else if IsPointOK(x-1,y+1,z+1,MaxX,MaxY,MaxZ) then
                     begin
                        if Map[x-1,y+1,z+1] < C_PART_OF_VOLUME then
                           Map[x,y+1,z] := WinValue;
                     end
                     else if IsPointOK(x,y+1,z+1,MaxX,MaxY,MaxZ) then
                     begin
                        if Map[x,y+1,z+1] < C_PART_OF_VOLUME then
                           Map[x,y+1,z] := WinValue;
                     end
                     else if IsPointOK(x+1,y+1,z+1,MaxX,MaxY,MaxZ) then
                     begin
                        if Map[x+1,y+1,z+1] < C_PART_OF_VOLUME then
                           Map[x,y+1,z] := WinValue;
                     end;
                     IsInsideVolume := true;
                  end;
                  inc(y);
               end;
            end;
         end;
      end;
end;


procedure SetupDistanceArray(var Dist : TDistanceArray; Range,SmoothLevel : single; ContrastLevel : integer);
const
   ANG90 =  1.5707963267948966192313216916398; //0.5 * pi;
   ANG180 = 3.1415926535897932384626433832795;
   ANG270 = 4.7123889803846898576939650749193;
var
   x,y,z : integer;
   Size,MidPoint : integer;
   Distance,Distance2D : single;
   AngTheta, AngPhi : single;
begin
   // 1.36 Setup distance array
   MidPoint := Max(Trunc(SmoothLevel),Trunc(Range));
   Size := (2*MidPoint)+1;
   SetLength(Dist,Size,Size,Size);
   Dist[MidPoint,MidPoint,MidPoint].Distance := 1;
   Dist[MidPoint,MidPoint,MidPoint].X := 0;
   Dist[MidPoint,MidPoint,MidPoint].Y := 0;
   Dist[MidPoint,MidPoint,MidPoint].Z := 0;
   for x := Low(Dist) to High(Dist) do
   begin
      for y := Low(Dist[x]) to High(Dist[x]) do
      begin
         for z := Low(Dist[x,y]) to High(Dist[x,y]) do
         begin
            Distance := sqrt(((x - MidPoint) * (x - MidPoint)) + ((y - MidPoint) * (y - MidPoint)) + ((z - MidPoint) * (z - MidPoint)));
            if Distance > 0 then
            begin
               if Distance <= SmoothLevel then
               begin
                  Dist[x,y,z].Distance := 1 / Power(Distance,ContrastLevel);
               end
               else
               begin
                  Dist[x,y,z].Distance := 0;
               end;
               if (Distance <= Range) then
               begin
                  // Here we find the angTheta.
                  angTheta := arctan2(MidPoint - y,MidPoint - x);
                  // Now, we find angPhi
                  Distance2D := Hypot(x - MidPoint,y - MidPoint);
                  angPhi := ANG90 - arctan2((MidPoint - z),Distance2D);
                  if MidPoint <> x then
//                     Dist[x,y,z].X := (sin(angPhi) * cos(AngTheta)) / Power(Distance,Power(abs(MidPoint-x),ContrastLevel))
//                     Dist[x,y,z].X := (3 * abs(MidPoint-x)) / (sin(angPhi) * cos(AngTheta) * power(Distance,3))
//                     Dist[x,y,z].X :=  (sin(angPhi) * cos(AngTheta) * abs(MidPoint-x)) / Distance
//                       Dist[x,y,z].X :=  (sign(MidPoint - x) * abs(MidPoint - x)) / Power(Distance,2)
                       Dist[x,y,z].X :=  (3 * (MidPoint - x)) / Power(Distance,3)
                  else
                     Dist[x,y,z].X := 0;

                  if MidPoint <> y then
//                     Dist[x,y,z].Y := (sin(angPhi) * sin(AngTheta)) / Power(Distance,Power(abs(MidPoint-y),ContrastLevel))
//                   Dist[x,y,z].Y := (3 * abs(MidPoint-y)) / (sin(angPhi) * sin(AngTheta) * power(Distance,3))
//                     Dist[x,y,z].Y := (sin(angPhi) * sin(AngTheta) * abs(MidPoint-y)) / Distance
//                     Dist[x,y,z].Y :=  (sign(MidPoint - y) * abs(MidPoint - y)) / Power(Distance,2)
                     Dist[x,y,z].Y :=  (3 * (MidPoint - y)) / Power(Distance,3)
                  else
                     Dist[x,y,z].Y := 0;

                  if MidPoint <> z then
//                     Dist[x,y,z].Z := cos(angPhi) / Power(Distance,Power(abs(MidPoint-z),ContrastLevel))
//                     Dist[x,y,z].Z := (3 * abs(MidPoint-z)) / (cos(angPhi) * power(Distance,3))
//                     Dist[x,y,z].Z := (cos(angPhi) * abs(MidPoint-z)) / Distance
//                     Dist[x,y,z].Z :=  (sign(MidPoint - z) * abs(MidPoint - z)) / Power(Distance,2)
                     Dist[x,y,z].Z :=  (3 * (MidPoint - z)) / Power(Distance,3)
                  else
                     Dist[x,y,z].Z := 0;
//                ShowMessage('P(' + IntToStr(x) + ',' + IntToStr(y) + ',' + IntToStr(z) + '): (' + FloatToStr(Dist[x,y,z].X) + ',' + FloatToStr(Dist[x,y,z].Y) + ',' + FloatToStr(Dist[x,y,z].Z) + '); with Distance ' + FloatToStr(Dist[x,y,z].Distance) + ' and Theta ' + FloatToStr(angTheta) + ':: Cos(Theta) = ' + FloatToStr(cos(angTheta)) + ' and Sin(Theta) = ' + FloatToStr(sin(angTheta)) + ' :: and Phi ' + FloatToStr(angPhi) + ':: Cos(Phi) = ' + FloatToStr(cos(angPhi)) + ' and Sin(Phi) = ' + FloatToStr(sin(angPhi)) + '.');

{
                  // Temporary working code:
                  Dist[x,y,z].X := sign(MidPoint - x) / Power(Distance,ContrastLevel);
                  Dist[x,y,z].Y := sign(MidPoint - y) / Power(Distance,ContrastLevel);
                  Dist[x,y,z].Z := sign(MidPoint - z) / Power(Distance,ContrastLevel);
}
               end
               else
               begin
                  Dist[x,y,z].X := 0;
                  Dist[x,y,z].Y := 0;
                  Dist[x,y,z].Z := 0;
               end;
            end
            else
            begin
               Dist[x,y,z].Distance := 0;
               Dist[x,y,z].X := 0;
               Dist[x,y,z].Y := 0;
               Dist[x,y,z].Z := 0;
            end;
         end;
      end;
   end;
end;

procedure NormalizeModel(var Voxel : TVoxelSection; const BinaryMap : TBinaryMap; var FloatMap: TVector3fMap; const Dist : TDistanceArray; MidPoint,Range : integer);
var
   x,y,z : integer;
   V : TVoxelUnpacked;
begin
   // Now, let's normalize every voxel.
   for x := 0 to Voxel.Tailer.XSize-1 do
      for y := 0 to Voxel.Tailer.YSize-1 do
         for z := 0 to Voxel.Tailer.ZSize-1 do
         begin
            // Get voxel data and calculate it (added +1 to
            // each, since binary map has propositally a
            // border to avoid bound checking).
            Voxel.GetVoxel(x,y,z,v);
            GetPreliminaryNormals(BinaryMap,FloatMap,Dist,V,MidPoint,Range,x,y,z);
         end;
end;

procedure PolishModel(var Voxel : TVoxelSection; var FloatMap: TVector3fMap; const Dist : TDistanceArray; MidPoint,Range : integer; SmoothMe: boolean; var Applied : integer; AffectOnlyNonNormalized : Boolean);
var
   x,y,z : integer;
   V : TVoxelUnpacked;
begin
   if SmoothMe then
   begin
      for x := 0 to Voxel.Tailer.XSize-1 do
         for y := 0 to Voxel.Tailer.YSize-1 do
            for z := 0 to Voxel.Tailer.ZSize-1 do
            begin
               Voxel.GetVoxel(x,y,z,v);
               if (not AffectOnlyNonNormalized) or (v.Normal > 0) then
                  GetNewNormals(Voxel,FloatMap,Dist,V,MidPoint,Range,x,y,z,Applied);
            end;
   end
   else
   begin
      for x := 0 to Voxel.Tailer.XSize-1 do
         for y := 0 to Voxel.Tailer.YSize-1 do
            for z := 0 to Voxel.Tailer.ZSize-1 do
            begin
               Voxel.GetVoxel(x,y,z,v);
               if (not AffectOnlyNonNormalized) or (v.Normal > 0) then
                  GetNewNormalsWithNoSmooth(Voxel,FloatMap,V,Range,x,y,z,Applied);
            end;
   end;
end;

procedure StretchMapContrast(var Map : TBinaryMap);
var
   x,y,z : integer;
   Data : array [0..4] of single;
begin
   Data[0] := 0;
   Data[1] := LIMZERO;
   Data[2] := 0.0000001;
   Data[3] := 0.0001;
   Data[4] := 1;
   for x := Low(Map) to High(Map) do
      for y := Low(Map[x]) to High(Map[x]) do
         for z := Low(Map[x,y]) to High(Map[x,y]) do
         begin
            Map[x,y,z] := Data[Round(Map[x,y,z])];
         end;
end;


// 1.2 Adition: Cubed Normalizer
//*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/
// Cubed Normals 2x Main Funcion Starts Here
//*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/
function ApplyCubedNormals(Voxel : TVoxelSection; Range,SmoothLevel : single; ContrastLevel : integer; SmoothMe,InfluenceMe,AffectOnlyNonNormalized : Boolean) : TApplyNormalsResult;
var
   BinaryMap,FilledMap : TBinaryMap;
   FloatMap : TVector3fMap;
   x,y,z : integer;
   Dist : TDistanceArray;
   IntRange,IntSmooth,FullRange : integer;
begin
   // 1.36 Setup distance array
   IntRange := Trunc(Range);
   IntSmooth := Trunc(SmoothLevel);
   FullRange := Max(IntSmooth,IntRange);
   SetupDistanceArray(Dist,Range,SmoothLevel,ContrastLevel);
   Result.applied := 0;
   // Create the binary map
   CreateBinaryMap(BinaryMap,FilledMap,FloatMap,Voxel,FullRange);
   // 1.2c: Solves inaucurate cubed normalizer
   // This will fill the internal part for acurate results
   Binary3DFloodFill(FilledMap);
   MergeBinaryMaps(FilledMap,BinaryMap);
   // 1.32: Solves limit (x,y,z) -> (0,0,0) on cubed normalizer.
   if InfluenceMe then
      BuildInfluenceArea(Voxel,BinaryMap,FullRange,LIMZERO);
   // Now, let's normalize every voxel.
   NormalizeModel(Voxel,BinaryMap,FloatMap,Dist,FullRange,IntRange);
   PolishModel(Voxel,FloatMap,Dist,FullRange,IntSmooth,SmoothMe,Result.Applied,AffectOnlyNonNormalized);
   // Now, let's free some memory.
   Finalize(Dist);
   Finalize(BinaryMap);
   Finalize(FilledMap);
   Finalize(FloatMap);
end;


// 1.3 Adition: Influence Normalizer
function ApplyInfluenceNormals(Voxel : TVoxelSection; Range,SmoothLevel : single; ContrastLevel : integer; SmoothMe,AffectOnlyNonNormalized,ImproveContrast : boolean) : TApplyNormalsResult;
//*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/
// Influence Normalizer Main Funcion Starts Here
//*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/*/
var
   BinaryMap : TBinaryMap;
   FloatMap : TVector3fMap;
   x,y,z : integer;
   Dist : TDistanceArray;
   IntRange,IntSmooth,FullRange : integer;
begin
   // 1.36 Setup distance array
   SetupDistanceArray(Dist,Range,SmoothLevel,ContrastLevel);
   // Normalizing algorithm starts here.
   Result.applied := 0;
   // Prepare and Clear FloatMap
   IntRange := Trunc(Range);
   IntSmooth := Trunc(SmoothLevel);
   FullRange := Max(IntSmooth,IntRange);
   SetLength(FloatMap,Voxel.Tailer.XSize+(2*FullRange),Voxel.Tailer.YSize+(2*FullRange),Voxel.Tailer.ZSize+(2*FullRange));
   for x := Low(FloatMap) to High(FloatMap) do
      for y := Low(FloatMap) to High(FloatMap[x]) do
         for z := Low(FloatMap) to High(FloatMap[x,y]) do
         begin
            FloatMap[x,y,z].X := 0;
            FloatMap[x,y,z].Y := 0;
            FloatMap[x,y,z].Z := 0;
         end;
   // 1.34: Create Influence map.
   ResetBinaryMap(Voxel,BinaryMap,FullRange);
   BuildInfluenceArea(Voxel,BinaryMap,FullRange,TIP);
   if ImproveContrast then
   begin
      AddSurfacePlusOneInfoToInfluenceArea(BinaryMap,4);
      StretchMapContrast(BinaryMap);
   end;
   // Now, let's normalize every voxel.
   NormalizeModel(Voxel,BinaryMap,FloatMap,Dist,FullRange,IntRange);
   PolishModel(Voxel,FloatMap,Dist,FullRange,IntSmooth,SmoothMe,Result.Applied,AffectOnlyNonNormalized);

   // Now, let's free some memory.
   Finalize(Dist);
   Finalize(BinaryMap);
   Finalize(FloatMap);
end;

function ApplyNormals(Voxel : TVoxelSection) : TApplyNormalsResult;
var maxx, maxy, maxz,
    x, y, z: integer;
   function Empty(x,y,z: integer): boolean;
   var
      v: TVoxelUnpacked;
   begin
      // check bounds
      Result := True; // outside the voxel is all empty
      if (x < 0) or (x > maxx) then Exit;
      if (y < 0) or (y > maxy) then Exit;
      if (z < 0) or (z > maxz) then Exit;
      // ok, do the real check
      voxel.GetVoxel(x,y,z,v);
      if v.Used then
         Result := False
      else
         Result := true;//BitSet(v.Flags,isOutside);
   end;

   function CheckTopBottom(var b1,b2 : boolean) : boolean;
   var
      ZZ : integer;
      First,Second : boolean;
      v : TVoxelunpacked;
   begin
      First := True;
      if z < maxz then
         for ZZ := z+1 to Maxz do
            if First then
            begin
               voxel.GetVoxel(x,y,ZZ,v);
               if v.Used then
                  First := false;
            end;

      if z = maxz then
         First := true;

      Second := True;

      if z > 0 then
      begin
         for ZZ := z-1 downto 0 do
            if Second then
            begin
               voxel.GetVoxel(x,y,ZZ,v);
               if v.Used then
                  Second := false;
            end;
      end
      else if z = 0 then
         Second := true;

      If (First) and (not Second) then
      begin
         B2 := true;
         B1 := false;
         Result := true;
      end
      else If (not First) and (Second) then
      begin
         B2 := false;
         B1 := true;
         Result := true;
      end
      else
         Result := false;
   end;

   function CheckRightLeft(var b1,b2 : boolean) : boolean;
   var
      YY : integer;
      First,Second : boolean;
      v : TVoxelunpacked;
   begin
      First := True;

      if y < maxy then
         for YY := y+1 to Maxy do
            if First then
            begin
               voxel.GetVoxel(x,yy,z,v);
               if v.Used then
                  First := false;
            end;

      if y = maxy then
         First := true;

      Second := True;

      if y > 0 then
      begin
         for YY := y-1 downto 0 do
            if Second then
            begin
               voxel.GetVoxel(x,yy,z,v);
               if v.Used then
                  Second := false;
            end;
      end
      else if y = 0 then
         Second := true;

      If (First) and (not Second) then
      begin
         B2 := true;
         B1 := false;
         Result := true;
      end
      else If (not First) and (Second) then
      begin
         B2 := false;
         B1 := true;
         Result := true;
      end
      else
         Result := false;
   end;

   function CheckFrontBack(var b1,b2 : boolean) : boolean;
   var
      XX : integer;
      First,Second : boolean;
      v : TVoxelunpacked;
   begin
      First := True;

      if x < maxx then
         for XX := x+1 to Maxx do
            if First then
            begin
               voxel.GetVoxel(xx,y,z,v);
               if v.Used then
                  First := false;
            end;

      if x = maxx then
         First := true;

      Second := True;

      if x > 0 then
      begin
         for xx := x-1 downto 0 do
            if Second then
            begin
               voxel.GetVoxel(xx,y,z,v);
               if v.Used then
                  Second := false;
            end;
      end
      else if x = 0 then
         Second := true;

      If (First) and (not Second) then
      begin
         B2 := false;
         B1 := true;
         Result := true;
      end
      else If (not First) and (Second) then
      begin
         B2 := true;
         B1 := false;
         Result := true;
      end
      else
         Result := false;
   end;

   Procedure CalcFacing;
   var
      v: TVoxelUnpacked;
      B : array [0..5] of boolean;
      I : integer;
      Pos : TVector3i;
   begin
      // get the voxel in question
      voxel.GetVoxel(x,y,z,v);
      // skip empty ones
      if not v.Used then Exit;

      for i := 0 to 5 do
         B[i] := false;

      if Empty(x+1,y,z) then B[0] := true;
      if Empty(x-1,y,z) then B[1] := true;
      if Empty(x,y-1,z) then B[2] := true;
      if Empty(x,y+1,z) then B[3] := true;
      if Empty(x,y,z-1) then B[4] := true;
      if Empty(x,y,z+1) then B[5] := true;

      // cancel out double-sided facings
      if B[0] and B[1] then if not CheckFrontBack(B[0],B[1]) then if x > trunc(maxx/2) then B[1] := false else B[0] := false;  //Dec(Flags,ChooseFrontOrBack(x,y,z));
      if B[2] and B[3] then if not CheckRightLeft(B[2],B[3]) then if y < trunc(maxy/2) then B[3] := false else B[2] := false;
      if B[4] and B[5] then if not CheckTopBottom(B[4],B[5]) then if z < trunc(maxz/2) then B[5] := false else B[4] := false;

      Pos := SetVectorI(0,0,0);
      if B[0] then
         Pos.X := Pos.X + 1;
      if B[1] then
         Pos.X := Pos.X - 1;
      if B[2] then
         Pos.Y := Pos.Y - 1;
      if B[3] then
         Pos.Y := Pos.Y + 1;
      if B[4] then
         Pos.Z := Pos.Z - 1;
      if B[5] then
         Pos.Z := Pos.Z + 1;

      if (Pos.x = 0) and (Pos.z = 0) and (Pos.y = 0) then
         inc(Result.confused)
      else
      begin
         if Voxel.Tailer.Unknown = 4 then
            v.Normal := Norm2IndexRA2(SetVector(Pos.X,Pos.Y,Pos.Z))
         else
            v.Normal := Norm2IndexTS(SetVector(Pos.X,Pos.Y,Pos.Z));

         inc(Result.applied);

         voxel.SetVoxel(x,y,z,v);
      end;
   end;
begin
   maxx := voxel.Tailer.XSize - 1;
   maxy := voxel.Tailer.YSize - 1;
   maxz := voxel.Tailer.ZSize - 1;

   Result.applied := 0;
   Result.confused := 0;
   //Result.redundant := 0; // Not used!

   // examine each used voxel, pass one for outside voxels
   for x := 0 to maxx do
      for y := 0 to maxy do
         for z := 0 to maxz do
            CalcFacing;
end;

function RemoveRedundantVoxels(voxel: TVoxelSection): integer;
var
   maxx, maxy, maxz,
   x, y, z: integer;
   Red : Array of TVector3i;
   Red_No : integer;
   v: TVoxelUnpacked;
   function Empty(x,y,z: integer): boolean;
   var
      v: TVoxelUnpacked;
   begin
      // check bounds
      Result := True; // outside the voxel is all empty
      if (x < 0) or (x > maxx) then Exit;
      if (y < 0) or (y > maxy) then Exit;
      if (z < 0) or (z > maxz) then Exit;
      // ok, do the real check
      voxel.GetVoxel(x,y,z,v);
      if v.Used then
         Result := False
      else
         Result := true;//BitSet(v.Flags,isOutside);
   end;

   function CheckTopBottom(var b1,b2 : boolean) : boolean;
   var
      ZZ : integer;
      First,Second : boolean;
      v : TVoxelunpacked;
   begin
      First := True;

      if z < maxz then
      begin
         for ZZ := z+1 to Maxz do
            if First then
            begin
               voxel.GetVoxel(x,y,ZZ,v);
               if v.Used then
                  First := false;
            end;
      end
      else if z = maxz then
         First := true;

      Second := True;

      if z > 0 then
      begin
         for ZZ := z-1 downto 0 do
            if Second then
            begin
               voxel.GetVoxel(x,y,ZZ,v);
               if v.Used then
               Second := false;
            end;
      end
      else if z = 0 then
         Second := true;

      If (First) and (not Second) then
      begin
         B2 := true;
         B1 := false;
         Result := true;
      end
      else If (not First) and (Second) then
      begin
         B2 := false;
         B1 := true;
         Result := true;
      end
      else
         Result := false;
   end;

   function CheckRightLeft(var b1,b2 : boolean) : boolean;
   var
      YY : integer;
      First,Second : boolean;
      v : TVoxelunpacked;
   begin
      First := True;

      if y < maxy then
      begin
         for YY := y+1 to Maxy do
            if First then
            begin
               voxel.GetVoxel(x,yy,z,v);
               if v.Used then
                  First := false;
            end;
      end
      else if y = maxy then
         First := true;

      Second := True;

      if y > 0 then
      begin
         for YY := y-1 downto 0 do
            if Second then
            begin
               voxel.GetVoxel(x,yy,z,v);
               if v.Used then
                  Second := false;
            end;
      end
      else if y = 0 then
         Second := true;

      If (First) and (not Second) then
      begin
         B2 := true;
         B1 := false;
         Result := true;
      end
      else If (not First) and (Second) then
      begin
         B2 := false;
         B1 := true;
         Result := true;
      end
      else
         Result := false;
   end;

   function CheckFrontBack(var b1,b2 : boolean) : boolean;
   var
      XX : integer;
      First,Second : boolean;
      v : TVoxelunpacked;
   begin
      First := True;

      if x < maxx then
      begin
         for XX := x+1 to Maxx do
            if First then
            begin
               voxel.GetVoxel(xx,y,z,v);
               if v.Used then
                  First := false;
            end;
      end
      else if x = maxx then
         First := true;

      Second := True;

      if x > 0 then
      begin
         for xx := x-1 downto 0 do
            if Second then
            begin
               voxel.GetVoxel(xx,y,z,v);
               if v.Used then
                  Second := false;
            end;
      end
      else if x = 0 then
         Second := true;

      If (First) and (not Second) then
      begin
         B2 := false;
         B1 := true;
         Result := true;
      end
      else If (not First) and (Second) then
      begin
         B2 := true;
         B1 := false;
         Result := true;
      end
      else
         Result := False;
   end;

   Procedure CalcFacing;
   var
      v: TVoxelUnpacked;
      B : array [0..5] of boolean;
      I : integer;
      Pos : TVector3i;
   begin
      // get the voxel in question
      voxel.GetVoxel(x,y,z,v);
      // skip empty ones
      if not v.Used then Exit;

      for i := 0 to 5 do
         B[i] := false;

      if Empty(x+1,y,z) then B[0] := true;
      if Empty(x-1,y,z) then B[1] := true;
      if Empty(x,y-1,z) then B[2] := true;
      if Empty(x,y+1,z) then B[3] := true;
      if Empty(x,y,z-1) then B[4] := true;
      if Empty(x,y,z+1) then B[5] := true;

      // cancel out double-sided facings
      if B[0] and B[1] then if not CheckFrontBack(B[0],B[1]) then if x > trunc(maxx/2) then B[1] := false else B[0] := false;  //Dec(Flags,ChooseFrontOrBack(x,y,z));
      if B[2] and B[3] then if not CheckRightLeft(B[2],B[3]) then if y < trunc(maxy/2) then B[3] := false else B[2] := false;
      if B[4] and B[5] then if not CheckTopBottom(B[4],B[5]) then if z < trunc(maxz/2) then B[5] := false else B[4] := false;

      Pos := SetVectorI(0,0,0);
      if B[0] then
         Pos.X := Pos.X + 1;
      if B[1] then
         Pos.X := Pos.X - 1;
      if B[2] then
         Pos.Y := Pos.Y - 1;
      if B[3] then
         Pos.Y := Pos.Y + 1;
      if B[4] then
         Pos.Z := Pos.Z - 1;
      if B[5] then
         Pos.Z := Pos.Z + 1;

      if (Pos.x = 0) and (Pos.z = 0) and (Pos.y = 0) then
      begin
         inc(Result);

         Red[Red_No].X := X;
         Red[Red_No].Y := Y;
         Red[Red_No].Z := Z;

         inc(Red_No);
         SetLength(Red,Red_No+1);
      end;
   end;

begin
   maxx := voxel.Tailer.XSize - 1;
   maxy := voxel.Tailer.YSize - 1;
   maxz := voxel.Tailer.ZSize - 1;

   Result := 0;

   SetLength(Red,0);
   Red_No := 0;
   SetLength(Red,Red_No+1);

   // examine each used voxel, pass one for outside voxels
   for x := 0 to maxx do
      for y := 0 to maxy do
         for z := 0 to maxz do
            CalcFacing;

   if Red_No > 0 then
      for x := 0 to Red_No-1 do
      begin
         voxel.GetVoxel(Red[x].x,Red[x].y,Red[x].z,v);
         v.Used := false;
         voxel.SetVoxel(Red[x].x,Red[x].y,Red[x].z,v);
      end;
end;

Function GetNormal3f(Vxl : TVoxelSection; N : integer) : TVector3f;
begin
   Result := setvector(0,0,0);

   if Vxl.Tailer.Unknown = 2 then
      Result := TSNormals[n]
   else if Vxl.Tailer.Unknown = 4 then
      Result := RA2Normals[n];
end;

function SmoothNormals(var Vxl : TVoxelSection) : TApplyNormalsResult;
var
   x,y,z,maxx,maxy,maxz,t : integer;
   v : TVoxelUnPacked;
   VoxelSmoothData : array of TVoxelSmoothData;
   Data_no : integer;
   function GetVoxel(x,y,z: integer; var v : TVoxelUnpacked): boolean;
   begin
      // check bounds
      Result := false; // outside the voxel is all empty
      if (x < 0) or (x > maxx) then Exit;
      if (y < 0) or (y > maxy) then Exit;
      if (z < 0) or (z > maxz) then Exit;
      // ok, do the real check
      vxl.GetVoxel(x,y,z,v);
      Result := v.Used;
   end;
   Function GetAverage: integer;
   var
      i : integer;
      Normals : TVector3f;
      Count : integer;
      v : TVoxelUnPacked;
   begin
      GetVoxel(x,y,z,v);
      Count := 0;//1;

      For i := 0 to 25 do
         if GetVoxel(x+trunc(CubeNormals[i].x),y+trunc(CubeNormals[i].y),z+trunc(CubeNormals[i].z),v) then
         begin
            Normals.X := Normals.X + GetNormal3f(Vxl,v.Normal).x;
            Normals.Y := Normals.Y + GetNormal3f(Vxl,v.Normal).y;
            Normals.Z := Normals.Z + GetNormal3f(Vxl,v.Normal).z;
            inc(count);
         end;

      if count > 0 then
      begin
         Normalize(Normals);

         GetVoxel(x,y,z,v);
         Normals.X := Normals.X + GetNormal3f(Vxl,v.Normal).x;
         Normals.Y := Normals.Y + GetNormal3f(Vxl,v.Normal).y;
         Normals.Z := Normals.Z + GetNormal3f(Vxl,v.Normal).z;

         Normalize(Normals);
         Result := Norm2Index(Normals,Vxl);
      end
      else
      begin
         GetVoxel(x,y,z,v);
         Result := v.Normal;
      end;
   end;
begin
   maxx := vxl.Tailer.XSize - 1;
   maxy := vxl.Tailer.YSize - 1;
   maxz := vxl.Tailer.ZSize - 1;

   Result.applied := 0;
   Result.confused := 0;

   Data_no := 0;
   SetLength(VoxelSmoothData,Data_no);

   for x := 0 to maxx do
      for y := 0 to maxy do
         for z := 0 to maxz do
         begin
            vxl.GetVoxel(x,y,z,v);
            if V.Used then
            begin
               T := GetAverage;

               if T = V.Normal then
                  inc(Result.confused)
               else
                  inc(Result.applied);
               V.Normal := T;

               inc(Data_no);
               SetLength(VoxelSmoothData,Data_no);
               VoxelSmoothData[Data_no-1].Pos.X := x;
               VoxelSmoothData[Data_no-1].Pos.Y := y;
               VoxelSmoothData[Data_no-1].Pos.Z := z;
               VoxelSmoothData[Data_no-1].v.Colour := v.Colour;
               VoxelSmoothData[Data_no-1].v.Normal := v.Normal;
               VoxelSmoothData[Data_no-1].v.Flags := v.Flags;
               VoxelSmoothData[Data_no-1].v.Used := v.Used;
               vxl.SetVoxel(x,y,z,v);
            end;
         end;

   SetLength(VoxelSmoothData,0);
end;

// Old Stuff below, used for RemoveRedundentVoxelsOld

const
   isInside = 0;
   isOutside = 1;

function BitSet(value, mask: integer): boolean;
begin
   Result := (value and mask) = mask;
end;

function FindInside(voxel: TVoxelSection): Integer;
{ This function starts by assuming every cell is inside.
  It then does a flood fill from every face of the cube,
  flood-filling all empty cells to outside.
}
var
   maxx, maxy, maxz: integer;
   procedure ClearFlags;
   var
      v: TVoxelUnpacked;
      x, y, z: integer;
   begin
      for x := 0 to maxx do
         for y := 0 to maxy do
            for z := 0 to maxz do
            begin
               voxel.GetVoxel(x,y,z,v);
               v.Flags := isInside;
               voxel.SetVoxel(x,y,z,v);
            end;
   end;
var
   OutsideCount: integer;
   function IsOnOutside(x,y,z: Integer): Boolean;
      procedure DoCheck(x,y,z: Integer);
         var v: TVoxelUnpacked;
      begin
         // get the voxel
         if voxel.GetVoxelSafe(x,y,z,v) then
         begin
            // is it outside already?
            if BitSet(v.Flags,isOutside) then
               Result := True;
         end;
      end;
   begin
      Result := False; // assume not
      DoCheck(x+1,y,z);
      DoCheck(x,y+1,z);
      DoCheck(x,y,z+1);
      DoCheck(x-1,y,z);
      DoCheck(x,y-1,z);
      DoCheck(x,y,z-1);
   end;
var
   x, y, z, {Pass,} PassCount: integer;
   v: TVoxelUnpacked;
begin
   maxx := voxel.Tailer.XSize - 1;
   maxy := voxel.Tailer.YSize - 1;
   maxz := voxel.Tailer.ZSize - 1;
   ClearFlags;
   OutsideCount := 0;
   //Pass := 0;
   repeat
      // Inc(Pass);
      // use pass count for memory too
      PassCount := 0;
      // do a scan of all cells
      for x := 0 to maxx do
         for y := 0 to maxy do
            for z := 0 to maxz do
            begin
               voxel.GetVoxel(x,y,z,v);
               // obvious checks
               if v.Used then Continue; // used voxels are 'inside'
               if BitSet(v.Flags,IsOutside) then Continue; // already found?
               // check for extremities or adjacent to an outside
               if (x = 0) or (x = maxx) or (y = 0) or (y = maxy) or
                     (z = 0) or (z = maxz) or IsOnOutside(x,y,z) then
               begin
                  // then set it to outside
                  v.Flags := isOutside;
                  voxel.SetVoxel(x,y,z,v);
                  Inc(PassCount);
               end;
            end;
      // keep running total
      Inc(OutsideCount,PassCount);
   until (Passcount = 0);
   Result := OutsideCount;
end;

// a voxel is internal if all adjacent voxels (including diagonals) are inside
function Internal(voxel: TVoxelSection; x,y,z: Integer): Boolean;
var maxx, maxy, maxz: Integer;
   function IsExternal(x,y,z:Integer):Boolean;
   var
      v: TVoxelUnpacked;
   begin
      Result := True; // assume is
      if voxel.getVoxelSafe(x,y,z,v) then
         Result := BitSet(v.Flags,isOutside);
   end;
var
   x1, y1, z1: Integer;
begin
   Result := False; // assume not
   maxx := voxel.Tailer.XSize - 1;
   maxy := voxel.Tailer.YSize - 1;
   maxz := voxel.Tailer.ZSize - 1;
   for x1 := Pred(x) to Succ(x) do
      for y1 := Pred(y) to Succ(y) do
         for z1 := Pred(z) to Succ(z) do
            if IsExternal(x1,y1,z1) then
               Exit;
   Result := True;
end;

// This method works with voxels that don't have holes
function RemoveRedundantVoxelsOld(voxel: TVoxelSection): integer;
var
   x,y,z,
   maxx, maxy, maxz: Integer;
   v: TVoxelUnpacked;
begin
   Result := 0;
   // first, make sure flags are ok
   FindInside(voxel);
   // constraints
   maxx := voxel.Tailer.XSize - 1;
   maxy := voxel.Tailer.YSize - 1;
   maxz := voxel.Tailer.ZSize - 1;
   // now test it
   for x := 0 to maxx do
      for y := 0 to maxy do
         for z := 0 to maxz do
            if Internal(voxel,x,y,z) then
            begin
               voxel.GetVoxel(x,y,z,v);
               if v.Used then
               begin
                  v.Used := False;
                  voxel.SetVoxel(x,y,z,v);
                  Inc(Result);
               end;
            end;
end;

function GetSmoothNormal(var Vxl : TVoxelSection; X,Y,Z,Normal : integer) : integer;
var
   t,maxx,maxy,maxz : integer;
   v : TVoxelUnPacked;

   // Get voxel data
   function GetVoxel(x,y,z: integer; var v : TVoxelUnpacked): boolean;
   begin
      // check bounds
      Result := false; // outside the voxel is all empty
      if (x < 0) or (x > maxx) then Exit;
      if (y < 0) or (y > maxy) then Exit;
      if (z < 0) or (z > maxz) then Exit;
      // ok, do the real check
      vxl.GetVoxel(x,y,z,v);
      Result := v.Used;
   end;

   Function GetAverage: integer;
   var
      i : integer;
      Normals : TVector3f;
      Count : integer;
      v : TVoxelUnPacked;
   begin
      GetVoxel(x,y,z,v);
//      Result := V.Normal;
      Count := 0;
      For i := 0 to 25 do
         if GetVoxel(x+trunc(CubeNormals[i].x),y+trunc(CubeNormals[i].y),z+trunc(CubeNormals[i].z),v) then
         begin
            Normals.X := Normals.X + GetNormal3f(Vxl,v.Normal).x;
            Normals.Y := Normals.Y + GetNormal3f(Vxl,v.Normal).y;
            Normals.Z := Normals.Z + GetNormal3f(Vxl,v.Normal).z;
            inc(count);
         end;

      if count > 0 then
      begin
         Normalize(Normals);
         GetVoxel(x,y,z,v);
         Normals.X := Normals.X + GetNormal3f(Vxl,v.Normal).x;
         Normals.Y := Normals.Y + GetNormal3f(Vxl,v.Normal).y;
         Normals.Z := Normals.Z + GetNormal3f(Vxl,v.Normal).z;
         Normalize(Normals);
         Result := Norm2Index(Normals,Vxl);
      end
      else
      begin
         GetVoxel(x,y,z,v);
         Result := v.Normal;
      end;
   end;
//
// Main Smmothing Function Starts HERE
//
begin
   maxx := vxl.Tailer.XSize - 1;
   maxy := vxl.Tailer.YSize - 1;
   maxz := vxl.Tailer.ZSize - 1;

   vxl.GetVoxel(x,y,z,v);
   if V.Used then
   begin
      T := GetAverage;
      Result := T;
   end
   else
      Result := Normal;
end;

end.

