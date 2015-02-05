{-----------------------------------------------------------------------------
 Unit Name: VectorUtil
 Author:    oranke
 Date:      2015-02-02
 Purpose:
 History:
-----------------------------------------------------------------------------}

unit VectorUtil;

interface

uses
  Windows;

type
  // 형 구분을 명확히 하기 위해 기본형을 재정의.
  PI8         = ^I8;
  PI16        = ^I16;
  PI32        = ^I32;

  PU8         = ^U8;
  PU16        = ^U16;
  PU32        = ^U32;

  PI64        = ^I64;

  PF32        = ^F32;
  PF64        = ^F64;
  PF80        = ^F80;

  //PP32        = ^P32;

  I8          = ShortInt;
  I16         = SmallInt;
  I32         = LongInt;

  U8          = Byte;
  U16         = Word;
  U32         = LongWord;

  I64         = Int64;

  F32         = Single;
  F64         = Double;
  F80         = Extended;

  //P32         = Pointer;

  //xFixed          = I32;      // 정수부 16비트, 실수부 16비트의 고정소수점. Ogl-es 의 GLFixed와 동일.

type
  PVector2sw   = ^TVector2sw;
  TVector2sw   = array [0..1] of I16;  // 패킹된 텍스쳐 좌표에 사용.

  PVector2f    = ^TVector2f;
  TVector2f    = array [0..1] of F32;

  PVector2sb   = ^TVector2sb;
  TVector2sb   = array [0..1] of I8;   // 패킹된 2차원 노멀값 등에 사용. 

  PVector2b    = ^TVector2b;
  TVector2b    = array [0..1] of U8;

  PVector3sb   = ^TVector3sb;
  TVector3sb   = array [0..2] of I8;   // 패킹된 노멀값 등에 사용.

  PVector3b    = ^TVector3b;
  TVector3b    = Array [0..2] of U8;   // 노멀맵의 값 참조에 사용.


  PVector3sw  = ^TVector3sw;
  TVector3sw  = array [0..2] of I16;

  PVector3i   = ^TVector3i;
  TVector3i   = array [0..2] of I32;

  PVector3f   = ^TVector3f;
  TVector3f   = array [0..2] of F32;

  PVector3d   = ^TVector3d;
  TVector3d   = array [0..2] of F64;

  PVector4sb   = ^TVector4sb;
  TVector4sb   = array [0..3] of I8;   

  PVector4sw  = ^TVector4sw;
  TVector4sw  = array [0..3] of I16;  // 패킹된 쿼터니온 등에 사용.

  PVector4i   = ^TVector4i;
  TVector4i   = array [0..3] of I32;

  PVector4f   = ^TVector4f;
  TVector4f   = array [0..3] of F32;

  PVector4d   = ^TVector4d;
  TVector4d   = array [0..3] of F64;

  PVector4p   = ^TVector4p;
  TVector4p   = array[0..3] of Pointer;

  PMatrix3f   = ^TMatrix3f;
  TMatrix3f   = array [0..2] of TVector3f;

  PMatrix4f   = ^TMatrix4f;
  TMatrix4f   = array [0..3] of TVector4f;

  PMatrix4d   = ^TMatrix4d;
  TMatrix4d   = array [0..3] of TVector4d;  

  
  PZhMatrix = ^TZhMatrix;
  TZhMatrix = packed record // 메트릭스의 각 성분을 여러가지 방법으로 접근하기 위한 공용체
  case Integer of
    0: (_11, _12, _13, _14: F32;
        _21, _22, _23, _24: F32;
        _31, _32, _33, _34: F32;
        _41, _42, _43, _44: F32);
    1: (mAR : Array[0..15] of F32);
    2: (m : TMatrix4f);
    3: (vX : TVector3f; v1: F32; // 벡터 성분과 실수부로 뽑아낼 때 사용..
        vY : TVector3f; v2: F32;
        vZ : TVector3f; v3: F32;
        vT : TVector3f; v4: F32);
  end;
  

const
  C_PI_div_180 : F32 =  0.017453292;
  C_180_div_PI : F32 = 57.29577951;
  C_2PI        : F32 =  6.283185307;
  C_PI_div_2   : F32 =  1.570796326;
  C_PI_div_4   : F32 =  0.785398163;
  C_Inv_2PI    : F32 = 1/6.283185307;
  C_Inv_360    : F32 = 1/360;
  C_180        : F32 = 180;
  C_360        : F32 = 360;

  C_RIGHT = 0; C_LEFT = 1; C_MIDDLE = 2;

  c_X = 0;  c_Y = 1;  c_Z = 2;  c_W = 3; // 상수 계산을 헷갈리지 않기 위해 사용..

  // some very small numbers
  C_EPSILON  = 1e-40;
  C_EPSILON2 = 1e-30;
  C_EPSILON3 = 1e-6;
  C_EPSILON4 = 1e-4;

  C_MAX_U16  = 65535;

  // 큰 값과 작은 값. F32에서는 INF로 표시됨. 비교에만 쓸 것.
  C_BigValue      = 1e50;
  C_SmallValue    = -1e50;
  
function D2R(const Degrees : F32) : F32; register;
function R2D(const Radians: F32): F32; register;

function NormalizeDegAngle(angle : F32) : F32;

function MakeVector3f(const x, y, z: F32): TVector3f; register;

function VectorAdd3f(const v1, v2 : TVector3f) : TVector3f; register;
function VectorSubtract3f(const v1, v2 : TVector3f): TVector3f; register;
function VectorScale3f(const v: TVector3f; factor: F32): TVector3f; register;
function VectorDotProduct3f(const v1, v2 : TVector3f): F32; register;
function VectorNorm3f(const v : TVector3f) : F32; register;
function VectorNormalize3f(const v : TVector3f): TVector3f; register;

function RotateVector(const V: TVector3f; const M: TMatrix4f): TVector3f; register;

function InvertMatrixNoScale(const mat : TMatrix4f) : TMatrix4f; register;

function CC_Ray_AABB(const RayOrg, RayDir, BoxMin, BoxMax: TVector3f; var HitPos: TVector3f): Boolean;

implementation

function D2R(const Degrees : F32) : F32; register;
begin
  Result := Degrees * C_PI_div_180;
end;

function R2D(const Radians: F32): F32; register;
begin
  Result := Radians * C_180_div_PI;
end;

function NormalizeDegAngle(angle : F32) : F32;
begin
  Result:=angle-Int(angle*C_Inv_360) * C_360;
  if Result> C_180 then
    Result:=Result - C_360
  else if Result<- C_180 then
    Result:=Result+ C_360;
end;

function MakeVector3f(const x, y, z: F32): TVector3f; register;
begin
  Result[c_X] := x;
  Result[c_Y] := y;
  Result[c_Z] := z;
end;

function VectorAdd3f(const v1, v2 : TVector3f) : TVector3f; register;
asm
  FLD  DWORD PTR [EAX]
  FADD DWORD PTR [EDX]
  FSTP DWORD PTR [ECX]
  FLD  DWORD PTR [EAX+4]
  FADD DWORD PTR [EDX+4]
  FSTP DWORD PTR [ECX+4]
  FLD  DWORD PTR [EAX+8]
  FADD DWORD PTR [EDX+8]
  FSTP DWORD PTR [ECX+8]
end;

function VectorSubtract3f(const v1, v2 : TVector3f): TVector3f; register;
asm
  FLD  DWORD PTR [EAX]
  FSUB DWORD PTR [EDX]
  FSTP DWORD PTR [ECX]
  FLD  DWORD PTR [EAX+4]
  FSUB DWORD PTR [EDX+4]
  FSTP DWORD PTR [ECX+4]
  FLD  DWORD PTR [EAX+8]
  FSUB DWORD PTR [EDX+8]
  FSTP DWORD PTR [ECX+8]
end;

function VectorScale3f(const v: TVector3f; factor: F32): TVector3f; register;
asm
  FLD  DWORD PTR [EAX]
  FMUL DWORD PTR [EBP+8]
  FSTP DWORD PTR [EDX]

  FLD  DWORD PTR [EAX+4]
  FMUL DWORD PTR [EBP+8]
  FSTP DWORD PTR [EDX+4]

  FLD  DWORD PTR [EAX+8]
  FMUL DWORD PTR [EBP+8]
  FSTP DWORD PTR [EDX+8]
end;

function VectorDotProduct3f(const v1, v2 : TVector3f): F32; register;
//Result := v1[c_X]*v2[c_X] + v1[c_Y]*v2[c_Y] + v1[c_Z]*v2[c_Z];
asm
  FLD DWORD PTR [EAX]
  FMUL DWORD PTR [EDX]
  FLD DWORD PTR [EAX + 4]
  FMUL DWORD PTR [EDX + 4]
  FADDP
  FLD DWORD PTR [EAX + 8]
  FMUL DWORD PTR [EDX + 8]
  FADDP
end;

function VectorNorm3f(const v : TVector3f) : F32; register;
asm
  FLD DWORD PTR [EAX];
  FMUL ST, ST
  FLD DWORD PTR [EAX+4];
  FMUL ST, ST
  FADD
  FLD DWORD PTR [EAX+8];
  FMUL ST, ST
  FADD
end;

function VectorNormalize3f(const v : TVector3f): TVector3f; register;
asm
  FLD  DWORD PTR [EAX]
  FMUL ST, ST
  FLD  DWORD PTR [EAX+4]
  FMUL ST, ST
  FADD
  FLD  DWORD PTR [EAX+8]
  FMUL ST, ST
  FADD
  FSQRT
  FLD1
  FDIVR
  FLD  ST
  FMUL DWORD PTR [EAX]
  FSTP DWORD PTR [EDX]
  FLD  ST
  FMUL DWORD PTR [EAX+4]
  FSTP DWORD PTR [EDX+4]
  FMUL DWORD PTR [EAX+8]
  FSTP DWORD PTR [EDX+8]
end;


// 4*4 행렬의 회전부만 사용하는 변환.
function RotateVector(const V: TVector3f; const M: TMatrix4f): TVector3f; register;
begin
  Result[c_X]:=V[c_X]*M[c_X,c_X]+V[c_Y]*M[c_Y,c_X]+V[c_Z]*M[c_Z,c_X];
  Result[c_Y]:=V[c_X]*M[c_X,c_Y]+V[c_Y]*M[c_Y,c_Y]+V[c_Z]*M[c_Z,c_Y];
  Result[c_Z]:=V[c_X]*M[c_X,c_Z]+V[c_Y]*M[c_Y,c_Z]+V[c_Z]*M[c_Z,c_Z];
end;

// 스케일성분이 없는 행렬의 역행렬 쉽게 구하기..
function InvertMatrixNoScale(const mat : TMatrix4f) : TMatrix4f; register;
var
  i : Byte;
begin
  // 3*3 부분 전치시키고..
  i := 0;
  while i < 3 do
  begin
    Result[i,0] := mat[0,i];
    Result[i,1] := mat[1,i];
    Result[i,2] := mat[2,i];
    Result[i,3] := 0;
    Inc(i);
  end;

  // 각 방향 벡터와 이동벡터의 내적을 새로운 이동벡터로..
  Result[3,0] := -VectorDotProduct3f(TZhMatrix(mat).vX, TZhMatrix(mat).vT);
  Result[3,1] := -VectorDotProduct3f(TZhMatrix(mat).vY, TZhMatrix(mat).vT);
  Result[3,2] := -VectorDotProduct3f(TZhMatrix(mat).vZ, TZhMatrix(mat).vT);
  Result[3,3] := 1.0;
end;

function CC_Ray_AABB(const RayOrg, RayDir, BoxMin, BoxMax: TVector3f; var HitPos: TVector3f): Boolean;
var
  inside : Boolean;
  i : I32;
  WhichPlane: I32;
  //HitPos : TVector3f;
  Quadrant : Array[0..2] of Byte;
  MaxT, CandidatePlane: TVector3f;
begin
  Result := false;
  Inside := True;

	(* Find candidate planes; this loop can be avoided if
   	rays cast all from the eye(assume perpsective view) *)
  for i:= 0 to 2 do
  begin
    if RayOrg[i] < BoxMin[i] then
    begin
			Quadrant[i] := C_LEFT;
			CandidatePlane[i] := BoxMin[i];
			inside := FALSE;
    end else
    if RayOrg[i] > BoxMax[i] then
    begin
			Quadrant[i] := C_RIGHT;
			CandidatePlane[i] := BoxMax[i];
			inside := FALSE;
    end else
      Quadrant[i] := C_MIDDLE;
  end;

	(* Ray origin inside bounding box *)
  if inside then
  begin
    HitPos := RayOrg;
    Result := TRUE;
    Exit;
  end;

	(* Calculate T distances to candidate planes *)
  for i:= 0 to 2 do
  begin
    if (Quadrant[i] <> C_MIDDLE) and
       (ABS(RayDir[i]) > C_EPSILON3) {(RayDir[i] <> 0)} then
      MaxT[i] := (CandidatePlane[i] - RayOrg[i]) / RayDir[i]
    else
      MaxT[i] := -1;
  end;

	(* Get largest of the maxT's for final choice of intersection *)
  WhichPlane := 0;
  for i:= 1 to 2 do
    if MaxT[WhichPlane] < MaxT[i] then WhichPlane := i;

	(* Check final candidate actually inside box *)
  if MaxT[WhichPlane] < 0 then Exit;

  for i:= 0 to 2 do
  begin
    if WhichPlane <> i then
    begin
      HitPos[i] := RayOrg[i] + MaxT[WhichPlane] * RayDir[i];
      if (HitPos[i] < BoxMin[i]) or (HitPos[i] > BoxMax[i]) then Exit;
    end else
      HitPos[i] := CandidatePlane[i];
  end;

  Result := True;

end;

initialization


finalization

end.
