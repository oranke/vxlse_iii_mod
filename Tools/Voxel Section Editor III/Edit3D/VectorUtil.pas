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

  c_X = 0;  c_Y = 1;  c_Z = 2;  c_W = 3; // 상수 계산을 헷갈리지 않기 위해 사용..

  
  
implementation

initialization

finalization

end. 
