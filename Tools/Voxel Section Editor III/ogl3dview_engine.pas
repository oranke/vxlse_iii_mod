unit ogl3dview_engine;

{$INCLUDE Global_Conditionals.inc}

interface

uses Geometry,Windows,SysUtils,Graphics,OpenGL15,forms,Voxel,Voxel_Engine,math,
      dialogs,HVA, math3d;
      {
type TVector3f = record
  X, Y, Z : single;
end;      }
type
   TVoxelBox = record
      Color, Normal: integer;
      Position: TVector3f;
      Faces: array [1..6] of boolean;

      // by oranke.
      IsSkin: Boolean;
   end;

   TVoxelBoxSection = record
      Box : array of TVoxelBox;
      List, ID : integer;
   end;

   TVoxelBoxGroup = record
      Section : array of TVoxelBoxSection;
      NumBoxes : integer;
   end;

   TVector3b = record
      R,G,B : Byte;
   end;

   TVector4f = record
      X,Y,Z,W : Single;
   end;

   THVAMATRIXLISTDATA = record
      First : TVector4f;
      Second : TVector4f;
      Third : TVector4f;
   end;

var
   HighestNormal : integer;
   oglloaded : boolean = false;
   RebuildLists : boolean = false;
   XRotB,YRotB, SSM, SSO : boolean;
   BGColor,FontColor : TVector3f;
   oldw,oldh : integer;

   Size,FPS : single;
   VoxelBoxGroup: TVoxelBoxGroup;
   VoxelBox_No : integer;
   base : GLuint;		                	// Base Display List For The Font Set
   dc  : HDC;     // Device Context
   FFrequency : int64;
   FoldTime : int64;    // last system time

   rc : HGLRC;    // Rendering Context
   RemapColour : TVector3f;
   ElapsedTime, DemoStart, LastTime : DWord;
   YRot, XRot, XRot2, YRot2     : glFloat;    // Y Rotation
   Depth  : glFloat;
   Xcoord, Ycoord, Zcoord : Integer;
   MouseButton : Integer;
   WireFrame  : Boolean;
   
{
   RemapColourMap : array [0..8] of TVector3b =
  (
  ( //DarkRed
  R : 146;
  G : 3;
  B : 3;
  ),
  ( //DarkBlue
  R : 9;
  G : 32;
  B : 140;
  ),
  ( //DarkGreen
  R : 13;
  G : 136;
  B : 16;
  ),
  ( //White
  R : 160;
  G : 160;
  B : 160;
  ),
  ( //Orange
  R : 146;
  G : 92;
  B : 3;
  ),
  ( //Magenta
  R : 104;
  G : 43;
  B : 73;
  ),
  ( //Purple
  R : 137;
  G : 12;
  B : 134;
  ),
  ( //Gold
  R : 149;
  G : 119;
  B : 0;
  ),
  ( //DarkSky
  R : 13;
  G : 102;
  B : 136;
  )
  );
}

light0_position:TGLArrayf4=( 5.0, 0.0, 10.0, 0.0);
ambient:  TGLArrayf4=( 0.0, 0.0, 0.0, 1);
Light0_Light:  TGLArrayf4=( 1, 1, 1, 1);
Light0_Spec:  TGLArrayf4=( 1, 0.5, 0, 0);

function SetVector(x, y, z : single) : TVector3f;
function SetVectori(x, y, z : integer) : TVector3i;
Function TColorToTVector3f(Color : TColor) : TVector3f;
Function TVector3fToTColor(Vector3f : TVector3f) : TColor;

procedure Update3dViewWithNormals(Vxl : TVoxelSection);

procedure glDraw();
procedure Update3dView(Vxl : TVoxelSection);

procedure BuildFont;
function CleanAngle(Angle: single): single;
Function CleanVCCol(Color : TColor) : TVector3f;
function CleanV3fCol(Color: TVector3f): TVector3f;
function GetCorrectColour(Color: integer; RemapColour : TVector3f): TVector3f;

function GetPosWithSize(Position: TVector3f; Size: single): TVector3f;
procedure GetScaleWithMinBounds(const _Vxl: TVoxelSection; var _Scale,_MinBounds: TVector3f);

function ScreenShot_BitmapResult : TBitmap;

function checkface(Vxl : TVoxelSection; x,y,z : integer) : boolean;
procedure ClearVoxelBoxes(var _VoxelBoxGroup : TVoxelBoxGroup);
procedure Update3dViewVOXEL(Vxl : TVoxel);

procedure DrawBox(VoxelPosition, Color: TVector3f; Size: TVector3f; VoxelBox: TVoxelBox);

//procedure Update3dViewWithRGBTEST;

implementation

uses Palette,FormMain,FTGifAnimate, GIFImage, normals;

procedure BuildFont;			                // Build Our Bitmap Font
var font: HFONT;                	                // Windows Font ID
begin
  base := glGenLists(256);       	                // Storage For 96 Characters
  font := 0;
  SelectObject(DC, font);		       	        // Selects The Font We Want


  font := CreateFont(9, 0,0,0, FW_NORMAL, 0, 0, 0, OEM_CHARSET, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY	, FF_DONTCARE + DEFAULT_PITCH, 'Terminal');
  SelectObject(DC, font);
  wglUseFontBitmaps(DC, 0, 127, base);
end;

procedure KillFont;     		                // Delete The Font
begin
  glDeleteLists(base, 256); 		                // Delete All 96 Characters
end;

procedure glPrint(text : pchar);	                // Custom GL "Print" Routine
begin
  if (text = '') then   			        // If There's No Text
          Exit;					        // Do Nothing

  glPushAttrib(GL_LIST_BIT);				// Pushes The Display List Bits
  glListBase(base);					// Sets The Base Character
  glCallLists(length(text), GL_UNSIGNED_BYTE, text);	// Draws The Display List Text
  glPopAttrib();								// Pops The Display List Bits
end;

function SetVector(x, y, z : single) : TVector3f;
begin
  result.x := x;
  result.y := y;
  result.z := z;
end;

function SetVectori(x, y, z : integer) : TVector3i;
begin
  result.x := x;
  result.y := y;
  result.z := z;
end;

Function TColorToTVector3f(Color : TColor) : TVector3f;
begin
   Result.X := GetRValue(Color) / 255;
   Result.Y := GetGValue(Color) / 255;
   Result.Z := GetBValue(Color) / 255;
end;

Function TVector3fToTColor(Vector3f : TVector3f) : TColor;
begin
   Result := RGB(trunc(Vector3f.X*255),trunc(Vector3f.Y*255),trunc(Vector3f.Z*255));
end;

procedure ScreenShot(Filename : string);
var
  i: integer;
  t, FN, FN2, FN3 : string;
  SSDir : string;
begin
  // create the scrnshots directory if it doesn't exist
  SSDir := extractfiledir(Paramstr(0))+'\ScreenShots\';
  FN2 := extractfilename(Filename);
  FN2 := copy(FN2,1,length(FN2)-length(Extractfileext(FN2)));

 // sys_mkdir
  {$I-}
  CreateDir(SSDir);
//  MkDir(SSDir);
  {$I+}
  FN := SSDir+FN2;

  for i := 0 to 999 do
  begin
  t := inttostr(i);
  if length(t) < 3 then
  t := '00'+t
  else
  if length(t) < 2 then
  t := '0'+t;
  if not fileexists(FN+'_'+t+'.bmp') then
  begin
  FN3 := FN+'_'+t+'.bmp';
  break;
  end;
  end;

  if FN3 = '' then
  begin
    exit;
  end;

  ScreenShot_BitmapResult.SaveToFile(FN3);
end;

procedure ScreenShotGIF(GIFIMAGE : TGIFImage; Filename : string);
var
  i: integer;
  t, FN, FN2, FN3 : string;
  SSDir : string;
begin
  // create the scrnshots directory if it doesn't exist
  SSDir := extractfiledir(Paramstr(0))+'\ScreenShots\';
  FN2 := extractfilename(Filename);
  FN2 := copy(FN2,1,length(FN2)-length(Extractfileext(FN2)));

 // sys_mkdir
  {$I-}
  CreateDir(SSDir);
//  MkDir(SSDir);
  {$I+}
  FN := SSDir+FN2;

  for i := 0 to 999 do
  begin
  t := inttostr(i);
  if length(t) < 3 then
  t := '00'+t
  else
  if length(t) < 2 then
  t := '0'+t;
  if not fileexists(FN+'_'+t+'.gif') then
  begin
  FN3 := FN+'_'+t+'.gif';
  break;
  end;
  end;

  if FN3 = '' then
  begin
    exit;
  end;

  GIFImage.SaveToFile(FN3);
end;

function ScreenShot_BitmapResult : TBitmap;
var
   buffer: array of byte;
   x,y, i: integer;
   Bitmap : TBitmap;
begin
   SetLength(buffer, (FrmMain.OGL3DPreview.Width * FrmMain.OGL3DPreview.Height * 3 + 18) + 1);

   glReadPixels(0, 0, FrmMain.OGL3DPreview.Width, FrmMain.OGL3DPreview.Height, GL_RGB, GL_UNSIGNED_BYTE, Pointer(Cardinal(buffer) {+ 18}));

   i := 0;
   Bitmap := TBitmap.Create;
   Bitmap.Canvas.Brush.Color := TVector3fToTColor(BGColor);
   Bitmap.Width := FrmMain.OGL3DPreview.Width+2;
   Bitmap.Height := FrmMain.OGL3DPreview.Height+2;
   for y := 0 to FrmMain.OGL3DPreview.Height-1 do
      for x := 0 to FrmMain.OGL3DPreview.Width do
      begin
         if (x <> FrmMain.OGL3DPreview.Width) and (FrmMain.OGL3DPreview.Height-y-1 > 0) then
            Bitmap.Canvas.Pixels[x,FrmMain.OGL3DPreview.Height-y-2] := RGB(buffer[(i*3){+18}],buffer[(i*3){+18}+1],buffer[(i*3){+18}+2]);
         inc(i);
      end;
   Bitmap.width := Bitmap.width -2;
   Bitmap.Height := Bitmap.Height -4;
   SetLength(buffer,0);
   finalize(buffer);
   Result := Bitmap;
end;

procedure DoNormals(Normal: integer);
var
   N: integer;
begin
   N := Normal;
   HighestNormal := Normal;

   if N = -1 then
   begin
      glNormal3f(0, 0, 0);
      exit;
   end;

   if N < 0 then
      N := 0;

   if ActiveSection.Tailer.Unknown = 4 then
   begin
      if N > 243 then
         N := 243;
      glNormal3f(RA2Normals[trunc(N)].X * 1.2, RA2Normals[trunc(N)].Y * 1.2, RA2Normals[trunc(N)].Z * 1.2);
   end
   else if ActiveSection.Tailer.Unknown = 2 then
   begin
      if N > 35 then
         N := 35;
      glNormal3f(TSNormals[trunc(N)].X * 1.2, TSNormals[trunc(N)].Y * 1.2, TSNormals[trunc(N)].Z * 1.2);
   end;
end;

// 1.3: Now with Hyper Speed! Kirov at 59/60fps :D
procedure DrawBox(VoxelPosition, Color: TVector3f; Size: TVector3f; VoxelBox: TVoxelBox);
var
   East,West,South,North,Floor,Ceil : single;
begin
   East := VoxelPosition.X + Size.X;
   West := VoxelPosition.X;
   Ceil := VoxelPosition.Y + Size.Y;
   Floor := VoxelPosition.Y;
   North := VoxelPosition.Z + Size.Z;
   South := VoxelPosition.Z;

   glBegin(GL_QUADS);
   begin
      glColor3f(Color.X, Color.Y, Color.Z);      // Set The Color
      DoNormals(VoxelBox.Normal);

      if VoxelBox.Faces[1] then
      begin
         glVertex3f(East, Ceil, South);      // Top Right Of The Quad (Top)
         glVertex3f(West, Ceil, South);      // Top Left Of The Quad (Top)
         glVertex3f(West, Ceil, North);      // Bottom Left Of The Quad (Top)
         glVertex3f(East, Ceil, North);      // Bottom Right Of The Quad (Top)
      end;
      if VoxelBox.Faces[2] then
      begin
         glVertex3f(East, Floor, North);      // Top Right Of The Quad (Bottom)
         glVertex3f(West, Floor, North);      // Top Left Of The Quad (Bottom)
         glVertex3f(West, Floor, South);      // Bottom Left Of The Quad (Bottom)
         glVertex3f(East, Floor, South);      // Bottom Right Of The Quad (Bottom)
      end;
      if VoxelBox.Faces[3] then
      begin
         glVertex3f(East, Ceil, North);      // Top Right Of The Quad (Front)
         glVertex3f(West, Ceil, North);      // Top Left Of The Quad (Front)
         glVertex3f(West, Floor, North);      // Bottom Left Of The Quad (Front)
         glVertex3f(East, Floor, North);      // Bottom Right Of The Quad (Front)
      end;
      if VoxelBox.Faces[4] then
      begin
         glVertex3f(East, Floor, South);      // Bottom Left Of The Quad (Back)
         glVertex3f(West, Floor, South);      // Bottom Right Of The Quad (Back)
         glVertex3f(West, Ceil, South);      // Top Right Of The Quad (Back)
         glVertex3f(East, Ceil, South);      // Top Left Of The Quad (Back)
      end;
      if VoxelBox.Faces[5] then
      begin
         glVertex3f(West, Ceil, North);      // Top Right Of The Quad (Left)
         glVertex3f(West, Ceil, South);      // Top Left Of The Quad (Left)
         glVertex3f(West, Floor, South);      // Bottom Left Of The Quad (Left)
         glVertex3f(West, Floor, North);      // Bottom Right Of The Quad (Left)
      end;
      if VoxelBox.Faces[6] then
      begin
         glVertex3f(East, Ceil, South);      // Top Right Of The Quad (Right)
         glVertex3f(East, Ceil, North);      // Top Left Of The Quad (Right)
         glVertex3f(East, Floor, North);      // Bottom Left Of The Quad (Right)
         glVertex3f(East, Floor, South);      // Bottom Right Of The Quad (Right)
      end;
   end;
   glEnd();
end;

function GetPosWithSize(Position: TVector3f; Size: single): TVector3f;
begin
   Result.X := Position.X * Size;
   Result.Y := Position.Y * Size;
   Result.Z := Position.Z * Size;
end;

function CleanVCCol(Color: TColor): TVector3f;
begin
   Result.X := GetRValue(Color);
   Result.Y := GetGValue(Color);
   Result.Z := GetBValue(Color);

   if Result.X > 255 then
      Result.X := 255
   else if Result.X < 0 then
      Result.X := 0;

   if Result.Y > 255 then
      Result.Y := 255
   else if Result.Y < 0 then
      Result.Y := 0;

   if Result.Z > 255 then
      Result.Z := 255
   else if Result.Z < 0 then
      Result.Z := 0;

   Result.X := Result.X / 255;
   Result.Y := Result.Y / 255;
   Result.Z := Result.Z / 255;
end;

function CleanV3fCol(Color: TVector3f): TVector3f;
begin
   Result.X := Color.X;
   Result.Y := Color.Y;
   Result.Z := Color.Z;

   if Result.X > 255 then
      Result.X := 255
   else if Result.X < 0 then
      Result.X := 0;

   if Result.Y > 255 then
      Result.Y := 255
   else if Result.Y < 0 then
      Result.Y := 0;

   if Result.Z > 255 then
      Result.Z := 255
   else if Result.Z < 0 then
      Result.Z := 0;

   Result.X := Result.X / 255;
   Result.Y := Result.Y / 255;
   Result.Z := Result.Z / 255;
end;

function GetCorrectColour(Color: integer; RemapColour : TVector3f): TVector3f;
var
   T: TVector3f;
begin
   if (Color > 15) and (Color < 32) then
   begin
      T.X := RemapColour.X * ((32 - Color) / 16);
      T.Y := RemapColour.Y * ((32 - Color) / 16);
      T.Z := RemapColour.Z * ((32 - Color) / 16);

      Result := T; //CleanV3fCol(T);
   end
   else
      Result := TColorToTVector3f(VXLPalette[Color]);
end;

function GetVXLColor(Color, Normal: integer): TVector3f;
begin
   if SpectrumMode = ModeColours then
      Result := GetCorrectColour(color, RemapColour)
   else
      Result := SetVector(0.5, 0.5, 0.5);
end;

function CleanAngle(Angle: single): single;
begin
   Result := Angle;

   if Result < 0 then
      Result := 360 + Result
   else if Result > 360 then
      Result := Result - 360;
end;

{------------------------------------------------------------------}
{  Function to draw the actual scene                               }
{------------------------------------------------------------------}
procedure glDraw();
var
   x : integer;
   Scale,MinBounds : TVector3f;
begin
   glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
   // Clear The Screen And The Depth Buffer
   glClearColor(BGColor.X, BGColor.Y, BGColor.Z, 1.0);      // Black Background

   // 1.2: Removed: This is already checked in the function that calls glDraw;
   //  if FrmMain.Display3dView1.Checked then exit;

   if (not VoxelOpen) then
      exit;

   // If it's set to auto-rotate to X, we increase XRot with
   // adition/subtraction factor XRot2. Ugly name for it.
   if XRotB then
      XRot := XRot + XRot2;

   // If it's set to auto-rotate to Y, we increase YRot with
   // adition/subtraction factor YRot2. Ugly name for it.
   if YRotB then
      YRot := YRot + YRot2;

   // Here we make sure XRot and YRot are between 0 and 360.
   XRot := CleanAngle(XRot);
   YRot := CleanAngle(YRot);

   glEnable(GL_LIGHT0);
   glEnable(GL_LIGHTING);
   glEnable(GL_COLOR_MATERIAL);

   // We'll only render anything if there is a voxel to render.
   if VoxelBoxGroup.NumBoxes > 0 then
   begin
      // Here we make the OpenGL list to speed up the render.
      glLoadIdentity();                                       // Reset The View
      GetScaleWithMinBounds(VoxelFile.Section[VoxelBoxGroup.Section[0].ID],Scale,MinBounds);

      if (VoxelBoxGroup.Section[0].List < 1) or RebuildLists then
      begin
         if (VoxelBoxGroup.Section[0].List > 0) then
            glDeleteLists(VoxelBoxGroup.Section[0].List,1);
         VoxelBoxGroup.Section[0].List := glGenLists(1);
         glNewList(VoxelBoxGroup.Section[0].List, GL_COMPILE);
         // Now, we hunt all voxel boxes...
         glPushMatrix;
         for x := Low(VoxelBoxGroup.Section[0].Box) to High(VoxelBoxGroup.Section[0].Box) do
         begin
            DrawBox(VoxelBoxGroup.Section[0].Box[x].Position, GetVXLColor(VoxelBoxGroup.Section[0].Box[x].Color, VoxelBoxGroup.Section[0].Box[x].Normal), Scale, VoxelBoxGroup.Section[0].Box[x]);
         end;
         glPopMatrix;
         glEndList;
         RebuildLists := false;
      end;
      // The final voxel rendering part.
      glPushMatrix;

      glTranslatef(0, 0, Depth);

      glRotatef(XRot, 1, 0, 0);
      glRotatef(YRot, 0, 0, 1);

      glCallList(VoxelBoxGroup.Section[0].List);
      glPopMatrix;
      // End of the final voxel rendering part.
      glDisable(GL_TEXTURE_2D);

      glLoadIdentity;
      glDisable(GL_DEPTH_TEST);
      glMatrixMode(GL_PROJECTION);
      glPushMatrix;
      glLoadIdentity;
      glOrtho(0, FrmMain.OGL3DPreview.Width, 0, FrmMain.OGL3DPreview.Height, -1, 1);
      glMatrixMode(GL_MODELVIEW);
      glPushMatrix;
      glLoadIdentity;

      glDisable(GL_LIGHT0);
      glDisable(GL_LIGHTING);
      glDisable(GL_COLOR_MATERIAL);

      glColor3f(FontColor.X, FontColor.Y, FontColor.Z);

      glRasterPos2i(1, 2);
      glPrint(PChar('Voxels Used: ' + IntToStr(VoxelBox_No)));

      if (not ssm) and (not SSO) then
      begin
         glRasterPos2i(1, 13);
         glPrint(PChar('Depth: ' + IntToStr(trunc(Depth))));

         glRasterPos2i(1, FrmMain.OGL3DPreview.Height - 9);
         glPrint(PChar('FPS: ' + IntToStr(trunc(FPS))));

         if FrmMain.DebugMode1.Checked then
         begin
            glRasterPos2i(1, FrmMain.OGL3DPreview.Height - 19);
            glPrint(PChar('DEBUG -  XRot:' + floattostr(XRot) + ' YRot:' + floattostr(YRot)));

            glRasterPos2i(1, FrmMain.OGL3DPreview.Height - 38);
            glPrint(PChar('Highest Normal:' + floattostr(HighestNormal)));
         end;
      end;

      glMatrixMode(GL_PROJECTION);
      glPopMatrix;
      glMatrixMode(GL_MODELVIEW);
      glPopMatrix;

      glEnable(GL_DEPTH_TEST);

      if SSO then
      begin
         ScreenShot(VXLFilename);
         SSO := False;
      end;

      if ssm then
      begin
         GifAnimateAddImage(ScreenShot_BitmapResult, False, 10);
         if (YRot = 360) and ssm then
         begin
            ssm := False;
            FrmMain.btn3DRotateY.Down := False;
            ScreenShotGIF(GifAnimateEndGif, VXLFilename);

            FrmMain.btn3DRotateY.Enabled  := True;
            FrmMain.btn3DRotateY2.Enabled := True;
            FrmMain.btn3DRotateX.Enabled  := True;
            FrmMain.btn3DRotateX2.Enabled := True;
            FrmMain.spin3Djmp.Enabled     := True;
            FrmMain.SpeedButton1.Enabled  := True;
            FrmMain.SpeedButton2.Enabled  := True;

            YRot := 225;
            XRotB := False;
            YRotB := False;
         end;
      end;
   end;
end;

procedure ClearVoxelBoxes(var _VoxelBoxGroup : TVoxelBoxGroup);
var
   Section : integer;
begin
   if High(_VoxelBoxGroup.Section) >= 0 then
   begin
      for Section := Low(_VoxelBoxGroup.Section) to High(_VoxelBoxGroup.Section) do
      begin
         if (_VoxelBoxGroup.Section[Section].List > 0) then
         begin
            glDeleteLists(_VoxelBoxGroup.Section[Section].List,1);
            _VoxelBoxGroup.Section[Section].List := 0;
         end;
         SetLength(_VoxelBoxGroup.Section[Section].Box,0);
      end;
   end;
   SetLength(_VoxelBoxGroup.Section,0);
   _VoxelBoxGroup.NumBoxes := 0;
end;

function CheckFace(Vxl : TVoxelSection; x,y,z : integer) : boolean;
var
v: TVoxelUnpacked;
begin
   Result := true;
   if (X < 0) or (X >= Vxl.Tailer.XSize) then Exit;
   if (Y < 0) or (Y >= Vxl.Tailer.YSize) then Exit;
   if (Z < 0) or (Z >= Vxl.Tailer.ZSize) then Exit;

   Vxl.GetVoxel(x,y,z,v);
   if v.Used then
      Result := false;
end;

procedure GetScaleWithMinBounds(const _Vxl: TVoxelSection; var _Scale,_MinBounds: TVector3f);
begin
   _Scale.X := ((_Vxl.Tailer.MaxBounds[1] - _Vxl.Tailer.MinBounds[1]) / _Vxl.Tailer.XSize) * Size;
   _Scale.Y := ((_Vxl.Tailer.MaxBounds[2] - _Vxl.Tailer.MinBounds[2]) / _Vxl.Tailer.YSize) * Size;
   _Scale.Z := ((_Vxl.Tailer.MaxBounds[3] - _Vxl.Tailer.MinBounds[3]) / _Vxl.Tailer.ZSize) * Size;

   _MinBounds.X := _Vxl.Tailer.MinBounds[1] * Size;
   _MinBounds.Y := _Vxl.Tailer.MinBounds[2] * Size;
   _MinBounds.Z := _Vxl.Tailer.MinBounds[3] * Size;
end;

procedure Update3dView(Vxl: TVoxelSection);
var
   x, y, z: byte;
   v:   TVoxelUnpacked;
   Scale,MinBounds : TVector3f;
begin
   if not IsEditable then exit;

   if FrmMain.Display3dView1.Checked then
      exit;

   {$ifdef DEBUG_FILE}
   FrmMain.DebugFile.Add('OpenGL3DViewEngine: Update3DView');
   {$endif}
   // Shutup 3d view for setup purposes.
   FrmMain.Display3dView1.Checked := true;

   VoxelBox_No := 0;
   ClearVoxelBoxes(VoxelBoxGroup);

   GetScaleWithMinBounds(Vxl,Scale,MinBounds);

   SetLength(VoxelBoxGroup.Section,1);
   for z := 0 to (Vxl.Tailer.zSize - 1) do
   begin
      for y := 0 to (Vxl.Tailer.YSize - 1) do
      begin
         for x := 0 to (Vxl.Tailer.xSize - 1) do
         begin
            Vxl.GetVoxel(x, y, z, v);

            if v.Used = True then
            begin
               SetLength(VoxelBoxGroup.Section[0].Box, VoxelBox_No+1);
               VoxelBoxGroup.Section[0].Box[VoxelBox_No].Faces[1]   := CheckFace(Vxl, x, y + 1, z);
               VoxelBoxGroup.Section[0].Box[VoxelBox_No].Faces[2]   := CheckFace(Vxl, x, y - 1, z);
               VoxelBoxGroup.Section[0].Box[VoxelBox_No].Faces[3]   := CheckFace(Vxl, x, y, z + 1);
               VoxelBoxGroup.Section[0].Box[VoxelBox_No].Faces[4]   := CheckFace(Vxl, x, y, z - 1);
               VoxelBoxGroup.Section[0].Box[VoxelBox_No].Faces[5]   := CheckFace(Vxl, x - 1, y, z);
               VoxelBoxGroup.Section[0].Box[VoxelBox_No].Faces[6]   := CheckFace(Vxl, x + 1, y, z);

               VoxelBoxGroup.Section[0].Box[VoxelBox_No].Position.X := (MinBounds.X + (X * Scale.X));
               VoxelBoxGroup.Section[0].Box[VoxelBox_No].Position.Y := (MinBounds.Y + (Y * Scale.Y));
               VoxelBoxGroup.Section[0].Box[VoxelBox_No].Position.Z := (MinBounds.Z + (Z * Scale.Z));

               VoxelBoxGroup.Section[0].Box[VoxelBox_No].Color  := v.Colour;
               VoxelBoxGroup.Section[0].Box[VoxelBox_No].Normal := v.Normal;
               Inc(VoxelBox_No);
            end;
         end;
      end;
   end;
   VoxelBoxGroup.NumBoxes := VoxelBox_No;
   RebuildLists := true;
   // Wake up 3d view, since everything is ready.
   FrmMain.Display3dView1.Checked := false;

end;

procedure Update3dViewVOXEL(Vxl: TVoxel);
var
   x, y, z, i: byte;
   v:   TVoxelUnpacked;
   Scale,MinBounds : TVector3f;
begin
   if FrmMain.Display3dView1.Checked then
      exit;

   {$ifdef DEBUG_FILE}
   FrmMain.DebugFile.Add('OpenGL3DViewEngine: Update3DViewVoxel');
   {$endif}
   // Shutup 3d view for setup purposes.
   FrmMain.Display3dView1.Checked := true;

   VoxelBoxGroup.NumBoxes := 0;
   ClearVoxelBoxes(VoxelBoxGroup);

   SetLength(VoxelBoxGroup.Section,VXL.Header.NumSections);
   for i := Low(VoxelBoxGroup.Section) to High(VoxelBoxGroup.Section) do
   begin
      VoxelBox_No := 0;
      GetScaleWithMinBounds(Vxl.Section[i],Scale,MinBounds);
      for z := 0 to (Vxl.Section[i].Tailer.zSize - 1) do
      begin
         for y := 0 to (Vxl.Section[i].Tailer.YSize - 1) do
         begin
            for x := 0 to (Vxl.Section[i].Tailer.xSize - 1) do
            begin
               Vxl.Section[i].GetVoxel(x, y, z, v);
               if v.Used = True then
               begin
                  SetLength(VoxelBoxGroup.Section[i].Box, VoxelBox_No+1);

                  VoxelBoxGroup.Section[i].Box[VoxelBox_No].Faces[1] := CheckFace(Vxl.Section[i], x, y + 1, z);
                  VoxelBoxGroup.Section[i].Box[VoxelBox_No].Faces[2] := CheckFace(Vxl.Section[i], x, y - 1, z);
                  VoxelBoxGroup.Section[i].Box[VoxelBox_No].Faces[3] := CheckFace(Vxl.Section[i], x, y, z + 1);
                  VoxelBoxGroup.Section[i].Box[VoxelBox_No].Faces[4] := CheckFace(Vxl.Section[i], x, y, z - 1);
                  VoxelBoxGroup.Section[i].Box[VoxelBox_No].Faces[5] := CheckFace(Vxl.Section[i], x - 1, y, z);
                  VoxelBoxGroup.Section[i].Box[VoxelBox_No].Faces[6] := CheckFace(Vxl.Section[i], x + 1, y, z);

                  VoxelBoxGroup.Section[i].Box[VoxelBox_No].Position.X := (Vxl.Section[i].Tailer.Transform[1][3] + MinBounds.X + (X * Scale.X));
                  VoxelBoxGroup.Section[i].Box[VoxelBox_No].Position.Y := (Vxl.Section[i].Tailer.Transform[2][3] + MinBounds.Y + (Y * Scale.Y));
                  VoxelBoxGroup.Section[i].Box[VoxelBox_No].Position.Z := (Vxl.Section[i].Tailer.Transform[3][3] + MinBounds.Z + (Z * Scale.Z));

                  VoxelBoxGroup.Section[i].Box[VoxelBox_No].Color  := v.Colour;
                  VoxelBoxGroup.Section[i].Box[VoxelBox_No].Normal := v.Normal;
                  Inc(VoxelBox_No);
                  Inc(VoxelBoxGroup.NumBoxes);
               end;
            end;
         end;
      end;
   end;
   RebuildLists := true;
   // Wake up 3d view, since everything is ready.
   FrmMain.Display3dView1.Checked := false;
end;

procedure Update3dViewWithNormals(Vxl: TVoxelSection);
var
   x,num:   byte;
begin
   if FrmMain.Display3dView1.Checked then
      exit;

   {$ifdef DEBUG_FILE}
   FrmMain.DebugFile.Add('OpenGL3DViewEngine: Update3DViewWithNormals');
   {$endif}
   // Shutup 3d view for setup purposes.
   FrmMain.Display3dView1.Checked := true;

   VoxelBox_No := 0;
   ClearVoxelBoxes(VoxelBoxGroup);

   if ActiveSection.Tailer.Unknown = 2 then
      num := 35
   else
      num := 243;

   SetLength(VoxelBoxGroup.Section,1);
   for x := 0 to num do
      if ActiveSection.Tailer.Unknown = 4 then
      begin
         SetLength(VoxelBoxGroup.Section[0].Box, VoxelBox_No+1);

         VoxelBoxGroup.Section[0].Box[VoxelBox_No].Faces[1] := CheckFace(Vxl, trunc(RA2Normals[x].x * 30.5), trunc(RA2Normals[x].y * 30.5) + 1, trunc(RA2Normals[x].z * 30.5));
         VoxelBoxGroup.Section[0].Box[VoxelBox_No].Faces[2] := CheckFace(Vxl, trunc(RA2Normals[x].x * 30.5), trunc(RA2Normals[x].y * 30.5) - 1, trunc(RA2Normals[x].z * 30.5));
         VoxelBoxGroup.Section[0].Box[VoxelBox_No].Faces[3] := CheckFace(Vxl, trunc(RA2Normals[x].x * 30.5), trunc(RA2Normals[x].y * 30.5), trunc(RA2Normals[x].z * 30.5) + 1);
         VoxelBoxGroup.Section[0].Box[VoxelBox_No].Faces[4] := CheckFace(Vxl, trunc(RA2Normals[x].x * 30.5), trunc(RA2Normals[x].y * 30.5), trunc(RA2Normals[x].z * 30.5) - 1);
         VoxelBoxGroup.Section[0].Box[VoxelBox_No].Faces[5] := CheckFace(Vxl, trunc(RA2Normals[x].x * 30.5) - 1, trunc(RA2Normals[x].y * 30.5), trunc(RA2Normals[x].z * 30.5));
         VoxelBoxGroup.Section[0].Box[VoxelBox_No].Faces[6] := CheckFace(Vxl, trunc(RA2Normals[x].x * 30.5) + 1, trunc(RA2Normals[x].y * 30.5), trunc(RA2Normals[x].z * 30.5));

         VoxelBoxGroup.Section[0].Box[VoxelBox_No].Position.X := trunc(RA2Normals[x].x * 30.5);
         VoxelBoxGroup.Section[0].Box[VoxelBox_No].Position.Y := trunc(RA2Normals[x].y * 30.5);
         VoxelBoxGroup.Section[0].Box[VoxelBox_No].Position.Z := trunc(RA2Normals[x].z * 30.5);

         VoxelBoxGroup.Section[0].Box[VoxelBox_No].Color  := 15;
         VoxelBoxGroup.Section[0].Box[VoxelBox_No].Normal := 0;
         Inc(VoxelBox_No);
      end
      else
      begin
         SetLength(VoxelBoxGroup.Section[0].Box, VoxelBox_No+1);

         VoxelBoxGroup.Section[0].Box[VoxelBox_No].Faces[1] := CheckFace(Vxl, trunc(TSNormals[x].x * 30.5), trunc(TSNormals[x].y * 30.5) + 1, trunc(TSNormals[x].z * 30.5));
         VoxelBoxGroup.Section[0].Box[VoxelBox_No].Faces[2] := CheckFace(Vxl, trunc(TSNormals[x].x * 30.5), trunc(TSNormals[x].y * 30.5) - 1, trunc(TSNormals[x].z * 30.5));
         VoxelBoxGroup.Section[0].Box[VoxelBox_No].Faces[3] := CheckFace(Vxl, trunc(TSNormals[x].x * 30.5), trunc(TSNormals[x].y * 30.5), trunc(TSNormals[x].z * 30.5) + 1);
         VoxelBoxGroup.Section[0].Box[VoxelBox_No].Faces[4] := CheckFace(Vxl, trunc(TSNormals[x].x * 30.5), trunc(TSNormals[x].y * 30.5), trunc(TSNormals[x].z * 30.5) - 1);
         VoxelBoxGroup.Section[0].Box[VoxelBox_No].Faces[5] := CheckFace(Vxl, trunc(TSNormals[x].x * 30.5) - 1, trunc(TSNormals[x].y * 30.5), trunc(TSNormals[x].z * 30.5));
         VoxelBoxGroup.Section[0].Box[VoxelBox_No].Faces[6] := CheckFace(Vxl, trunc(TSNormals[x].x * 30.5) + 1, trunc(TSNormals[x].y * 30.5), trunc(TSNormals[x].z * 30.5));

         VoxelBoxGroup.Section[0].Box[VoxelBox_No].Position.X := trunc(TSNormals[x].x * 30.5);
         VoxelBoxGroup.Section[0].Box[VoxelBox_No].Position.Y := trunc(TSNormals[x].y * 30.5);
         VoxelBoxGroup.Section[0].Box[VoxelBox_No].Position.Z := trunc(TSNormals[x].z * 30.5);

         VoxelBoxGroup.Section[0].Box[VoxelBox_No].Color  := 15;
         VoxelBoxGroup.Section[0].Box[VoxelBox_No].Normal := 0;
         Inc(VoxelBox_No);
      end;
   RebuildLists := true;
   VoxelBoxGroup.NumBoxes := VoxelBox_No;
   // Wake 3d view, since everything is ready.
   FrmMain.Display3dView1.Checked := false;
end;

begin
   HighestNormal := -1;
end.
