unit math3d;

interface

uses Voxel_Engine;

const
  M_PI = 3.1415926535897932384626433832795;		// matches value in gcc v2 math.h
  M_TWO_PI = 6.283185307179586476925286766559;
  M_180_PI = 57.295779513082320876798154814105;
  M_PI_180 = 0.017453292519943295769236907684886;


function VectorDistance(const v1, v2 : TVector3f) : Single;
function DEG2RAD(a : single) : single;
function RAD2DEG(a : single) : single;
function Normalize(var v : TVector3f) : single;
function ScaleVector(v : TVector3f; s : single) : TVector3f;
function ScaleVector3f(v,s : TVector3f) : TVector3f;
function AddVector(v1, v2 : TVector3f) : TVector3f;
function SubtractVector(v1, v2 : TVector3f) : TVector3f;
function DotProduct(v1, v2 : TVector3f) : single;
function InvertVector(v : TVector3f) : TVector3f;
function planeDistance(point, PlaneNormal : TVector3f; PlaneDistance : single) : single;
function SetVector(x, y, z : single) : TVector3f;
function CrossProduct(const V1, V2: TVector3f): TVector3f;
function ClassifyPoint(point, PlaneNormal : TVector3f; PlaneDistance : single) : single; overload;
function ClassifyPoint(PlaneNormal : TVector3f; PlaneDistance : single) : single; overload;
function ClassifyPoint(Point, PointOnPlane, PlaneNormal : TVector3f) : single; overload;

implementation

function VectorDistance(const v1, v2 : TVector3f) : Single; register;
// EAX contains address of v1
// EDX contains highest of v2
// Result  is passed on the stack
asm
      FLD  DWORD PTR [EAX]
      FSUB DWORD PTR [EDX]
      FMUL ST, ST
      FLD  DWORD PTR [EAX+4]
      FSUB DWORD PTR [EDX+4]
      FMUL ST, ST
      FADD
      FLD  DWORD PTR [EAX+8]
      FSUB DWORD PTR [EDX+8]
      FMUL ST, ST
      FADD
      FSQRT
end;

function SetVector(x, y, z : single) : TVector3f;
begin
  result.x := x;
  result.y := y;
  result.z := z;
end;

function DEG2RAD(a : single) : single;
begin
  result :=  a*M_PI_180;
end;

function RAD2DEG(a : single) : single;
begin
  result := a*M_180_PI;
end;

function Normalize(var v : TVector3f) : single;
var l : single;
begin
  l := sqrt(v.x*v.x+v.y*v.y+v.z*v.z);

  if (l > 0) then begin
    v.x := v.x/l;
    v.y := v.y/l;
    v.z := v.Z/l;
  end;
  result := l;  
end;

function ScaleVector(v : TVector3f; s : single) : TVector3f;
begin
  with result do begin
    x := v.x * s;
    y := v.y * s;
    z := v.z * s;
  end;
end;

function ScaleVector3f(v,s : TVector3f) : TVector3f;
begin
   with result do
   begin
      x := v.x * s.x;
      y := v.y * s.y;
      z := v.z * s.z;
   end;
end;

function AddVector(v1, v2 : TVector3f) : TVector3f;
begin
  with result do begin
    x := v1.x + v2.x;
    y := v1.y + v2.y;
    z := v1.z + v2.z;
  end;
end;

function SubtractVector(v1, v2 : TVector3f) : TVector3f;
begin
  with result do begin
    x := v1.x - v2.x;
    y := v1.y - v2.y;
    z := v1.z - v2.z;
  end;
end;

function DotProduct(v1, v2 : TVector3f) : single;
begin
  Result := v1.X*v2.X + v1.y*v2.y + v1.z*v2.z;
end;

function InvertVector(v : TVector3f) : TVector3f;
begin
  with result do begin
    x := -v.x;
    y := -v.y;
    z := -v.z;
  end;
end;

function CrossProduct(const V1, V2: TVector3f): TVector3f;
begin
   Result.X:=V1.Y * V2.z - V1.z * V2.y;
   Result.Y:=V1.z * V2.x - V1.x * V2.z;
   Result.Z:=V1.x * V2.y - V1.x * V2.x;
end;

function planeDistance(point, PlaneNormal : TVector3f; PlaneDistance : single) : single;
begin
  result := (planeNormal.x * point.x +
	     planeNormal.y * point.y +
	     planeNormal.z * point.z) - PlaneDistance;
end;

// point = test point
// p0 = point on plane = VectorScale(Plane.Normal, -Plane.d)
// pN = plane normal
function ClassifyPoint(point, PlaneNormal : TVector3f; PlaneDistance : single) : single;
var pointOnPlane, tempVect, dir : TVector3f;
    d : single;
begin
 pointOnPlane := ScaleVector(PlaneNormal, -PlaneDistance);
 TempVect.x := pointOnPlane.x - point.x;
 TempVect.y := pointOnPlane.y - point.y;
 TempVect.z := pointOnPlane.z - point.z;
 dir := TempVect;
 d := DotProduct(dir, PlaneNormal);
 if (d < -0.001) then
   result := 1
 else if (d > 0.001) then
   result := -1
 else
   result := 0;
end;

function ClassifyPoint(PlaneNormal : TVector3f; PlaneDistance : single) : single;
var pointOnPlane, tempVect, dir, point : TVector3f;
    d : single;
begin
 point := SetVector(0,0,0);
 pointOnPlane := ScaleVector(PlaneNormal, -PlaneDistance);
 TempVect.x := pointOnPlane.x - point.x;
 TempVect.y := pointOnPlane.y - point.y;
 TempVect.z := pointOnPlane.z - point.z;
 dir := TempVect;
 d := DotProduct(dir, PlaneNormal);
 if (d < -0.001) then
   result := 1
 else if (d > 0.001) then
   result := -1
 else
   result := 0;
end;

function ClassifyPoint(Point, PointOnPlane, PlaneNormal : TVector3f) : single; overload;
var tempVect, dir : TVector3f;
    d : single;
begin
 TempVect.x := pointOnPlane.x - point.x;
 TempVect.y := pointOnPlane.y - point.y;
 TempVect.z := pointOnPlane.z - point.z;
 dir := TempVect;
 d := DotProduct(dir, PlaneNormal);
 if (d < -0.001) then
   result := 1
 else if (d > 0.001) then
   result := -1
 else
   result := 0;
end;

end.
