{-----------------------------------------------------------------------------
 Unit Name: OGLUtil
 Author:    oranke
 Date:      2011-04-08
 Purpose:
    OpenGL 제어용 유틸리티 유니트.
    대섭 세미나용으로 제작.
    가급적 간단한 인터페이스를 유지할 것.

 History:
-----------------------------------------------------------------------------}



unit OGLUtil;

interface

uses
  Windows, Messages, SysUtils, OpenGL15;

function InitOpenGL(aHandle: THandle; var aDC: HDC; var aRC: HGLRC): Boolean;
procedure SetCurrent(aDC: HDC; aRC: HGLRC);
procedure CloseOpenGL(aHandle: THandle; aDC: HDC; aRC: HGLRC);

implementation

// OpenGL 초기화. 
function InitOpenGL(aHandle: THandle; var aDC: HDC; var aRC: HGLRC): Boolean;
var
  pfd : TPIXELFORMATDESCRIPTOR;
  PixelFormat : GLuint;
begin
  Result := false;

  aDC := GetDC(aHandle);
  ZeroMemory(@pfd, SizeOf(TPIXELFORMATDESCRIPTOR));
  with pfd do
  begin
    nSize      := SizeOf(TPIXELFORMATDESCRIPTOR); // Size Of This Pixel Format Descriptor
    nVersion   := 1;                    // The version of this data structure
    dwFlags    := PFD_DRAW_TO_WINDOW    // Buffer supports drawing to window
                  or PFD_SUPPORT_OPENGL // Buffer supports OpenGL drawing
                  or PFD_DOUBLEBUFFER;  // Supports double buffering

    //cColorBits   := 32;
    //cDepthBits   := 8;
    //cAlphaBits   := 8;

    iLayerType := PFD_MAIN_PLANE;       // Ignored
  end;
  PixelFormat := ChoosePixelFormat(aDC, @pfd);

  if PixelFormat = 0 then Exit;
  if (not SetPixelFormat(aDC, PixelFormat, @pfd)) then Exit;

  aRC := wglCreateContext(aDC);

  Result := true;
end;

// 지정된 RC를 현제 RC로. 
procedure SetCurrent(aDC: HDC; aRC: HGLRC);
begin
  wglMakeCurrent(aDC, aRC);
end;

// OepnGL 정리. 
procedure CloseOpenGL(aHandle: THandle; aDC: HDC; aRC: HGLRC);
begin
  if aRC <> 0 then
  begin
    SetCurrent(aDC, 0);
    wglDeleteContext(aRC);
  end;
  ReleaseDC(aDC, aHandle);

end;

initialization

finalization

end.