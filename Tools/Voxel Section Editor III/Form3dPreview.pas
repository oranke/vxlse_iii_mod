// Originaly Made by Jan Horn (Quake 3 Model Viewer)
// Modifyed By Stuart Carey (To a VXL Previewer)
// And modified by Carlos "Banshee" Muniz for VXLSE III.
unit Form3dPreview;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, {model,} OpenGL15, {Textures,} Menus, voxel, Spin,
  Buttons, FTGifAnimate, GIFImage,Palette,Voxel_Engine, Normals, Ogl3dview_engine,
  HVA,JPEG,PNGImage, math3d;

type
   TScreenshotType = (stNone,stBmp,stTga,stJpg,stGif,stPng);

  PFrm3DPReview = ^TFrm3DPReview;
  TFrm3DPReview = class(TForm)
    Panel2: TPanel;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    Options1: TMenuItem;
    BackgroundColour1: TMenuItem;
    ColorDialog1: TColorDialog;
    Panel1: TPanel;
    btn3DRotateX2: TSpeedButton;
    btn3DRotateY2: TSpeedButton;
    btn3DRotateY: TSpeedButton;
    spin3Djmp: TSpinEdit;
    Bevel1: TBevel;
    FontColor1: TMenuItem;
    Popup3d: TPopupMenu;
    Views1: TMenuItem;
    Front1: TMenuItem;
    Back1: TMenuItem;
    N1: TMenuItem;
    LEft1: TMenuItem;
    Right1: TMenuItem;
    N2: TMenuItem;
    Bottom1: TMenuItem;
    op1: TMenuItem;
    N3: TMenuItem;
    Cameo1: TMenuItem;
    SpeedButton1: TSpeedButton;
    Cameo21: TMenuItem;
    Cameo31: TMenuItem;
    Cameo41: TMenuItem;
    ools1: TMenuItem;
    akeScreenshot1: TMenuItem;
    ake360DegScreenshots1: TMenuItem;
    RemapColour1: TMenuItem;
    Red1: TMenuItem;
    Blue1: TMenuItem;
    Green1: TMenuItem;
    White1: TMenuItem;
    Orange1: TMenuItem;
    Magenta1: TMenuItem;
    Purple1: TMenuItem;
    Gold1: TMenuItem;
    DarkSky1: TMenuItem;
    SpeedButton2: TSpeedButton;
    btn3DRotateX: TSpeedButton;
    akeScreenshotJPG1: TMenuItem;
    akeScreenshotPNG1: TMenuItem;
    akeScreenshotBMP1: TMenuItem;
    Display1: TMenuItem;
    CurrentSectionOnly1: TMenuItem;
    WholeVoxel1: TMenuItem;
    SpPlay: TSpeedButton;
    SpStop: TSpeedButton;
    Label1: TLabel;
    SpFrame: TSpinEdit;
    AnimationTimer: TTimer;
    procedure AnimationTimerTimer(Sender: TObject);
    procedure SpPlayClick(Sender: TObject);
    procedure SpStopClick(Sender: TObject);
    procedure SpFrameChange(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure CurrentSectionOnly1Click(Sender: TObject);
    procedure akeScreenshotBMP1Click(Sender: TObject);
    procedure akeScreenshotPNG1Click(Sender: TObject);
    procedure akeScreenshotJPG1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Panel2MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Panel2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel2MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Exit1Click(Sender: TObject);
    procedure BackgroundColour1Click(Sender: TObject);
    procedure btn3DRotateX2Click(Sender: TObject);
    procedure btn3DRotateXClick(Sender: TObject);
    procedure btn3DRotateY2Click(Sender: TObject);
    procedure btn3DRotateYClick(Sender: TObject);
    procedure spin3DjmpChange(Sender: TObject);
    procedure FontColor1Click(Sender: TObject);
    procedure Front1Click(Sender: TObject);
    procedure Back1Click(Sender: TObject);
    procedure LEft1Click(Sender: TObject);
    procedure Right1Click(Sender: TObject);
    procedure Bottom1Click(Sender: TObject);
    procedure op1Click(Sender: TObject);
    procedure Cameo1Click(Sender: TObject);
    procedure SpeedButton1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Cameo21Click(Sender: TObject);
    procedure Cameo31Click(Sender: TObject);
    procedure Cameo41Click(Sender: TObject);
    procedure ake360DegScreenshots1Click(Sender: TObject);
    procedure akeScreenshot1Click(Sender: TObject);
    procedure Red1Click(Sender: TObject);
    procedure Blue1Click(Sender: TObject);
    procedure Green1Click(Sender: TObject);
    procedure White1Click(Sender: TObject);
    procedure Orange1Click(Sender: TObject);
    procedure Magenta1Click(Sender: TObject);
    procedure Purple1Click(Sender: TObject);
    procedure Gold1Click(Sender: TObject);
    procedure DarkSky1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
  private
    { Private declarations }
    rc : HGLRC;    // Rendering Context
    RemapColour : TVector3f;
    ElapsedTime, DemoStart, LastTime : DWord;
    YRot, XRot, XRot2, YRot2     : glFloat;    // Y Rotation
    Depth  : glFloat;
    Xcoord, Ycoord, Zcoord : Integer;
    MouseButton : Integer;
    procedure DrawMe;
    procedure ClearRemapClicks;
    procedure BuildFont;
    procedure KillFont;
    procedure MakeMeAScreenshotName(var Filename: string; Ext : string);
  public
    { Public declarations }
    XRotB,YRotB : boolean;
    ScreenieType : TScreenshotType;
    BGColor,FontColor : TVector3f;
    oldw,oldh : integer;
    Size,FPS : single;
    VoxelBoxGroup3D : TVoxelBoxGroup;
    VoxelBox_No : integer;
    base : GLuint;		                	// Base Display List For The Font Set
    dc  : HDC;     // Device Context
    FFrequency : int64;
    FoldTime : int64;    // last system time
    RebuildLists : boolean;
    AnimationState : boolean;
    IsReady : boolean;
    procedure Update3dView(Vxl : TVoxelSection);
    Procedure SetRotationAdders;
    procedure glPrint(text : pchar);
    Function GetVXLColor(Color,Normal : integer) : TVector3f;
    procedure ScreenShot(Filename : string);
    function ScreenShot_BitmapResult : TBitmap;
    procedure ScreenShotBMP(Filename : string);
    procedure ScreenShotJPG(Filename : string; Compression : integer);
    procedure ScreenShotPNG(Filename : string);
    procedure ScreenShotGIF(GIFIMAGE : TGIFImage; Filename : string);
    Procedure Reset3DView;
    procedure Idle(Sender: TObject; var Done: Boolean);
  end;

type TVector3b = record
  R,G,B : Byte;
end;

const

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

function SetVector(x, y, z : single) : TVector3f;
Function TColorToTVector3f(Color : TColor) : TVector3f;
Function TVector3fToTColor(Vector3f : TVector3f) : TColor;

implementation

uses FormMain;

{$R *.DFM}

procedure TFrm3DPReview.BuildFont;			                // Build Our Bitmap Font
var
   font: HFONT;                	                // Windows Font ID
//   gmf : array [0..255] of GLYPHMETRICSFLOAT;		// Address Buffer For Font Storage
begin
   base := glGenLists(256);       	                // Storage For 96 Characters
   SelectObject(DC, font);		       	        // Selects The Font We Want

   font := CreateFont(9, 0,0,0, FW_NORMAL, 0, 0, 0, OEM_CHARSET, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY	, FF_DONTCARE + DEFAULT_PITCH, 'Terminal');
   SelectObject(DC, font);
   wglUseFontBitmaps(DC, 0, 127, base);
end;

procedure TFrm3DPReview.KillFont;     		                // Delete The Font
begin
   glDeleteLists(base, 256); 		                // Delete All 96 Characters
end;

procedure TFrm3DPReview.glPrint(text : pchar);	                // Custom GL "Print" Routine
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

procedure TFrm3DPReview.MakeMeAScreenshotName(var Filename: string; Ext : string);
var
   i: integer;
   t, FN, FN2 : string;
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
      else if length(t) < 2 then
         t := '0'+t;
      if not fileexists(FN+'_'+t+Ext) then
      begin
         Filename := FN+'_'+t+Ext;
         break;
      end;
   end;
end;

procedure TFrm3DPReview.ScreenShot(Filename : string);
var
   buffer: array of byte;
   i, c, temp: integer;
   f: file;
begin
   MakeMeAScreenshotName(Filename,'.tga');

   if Filename = '' then
      exit;

   try
      SetLength(buffer, (Panel2.Width * Panel2.Height * 4) + 18);
      begin
         for i := 0 to 17 do
            buffer[i] := 0;
         buffer[2] := 2; //uncompressed type
         buffer[12] := Panel2.Width and $ff;
         buffer[13] := Panel2.Width shr 8;
         buffer[14] := Panel2.Height and $ff;
         buffer[15] := Panel2.Height shr 8;
         buffer[16] := 24; //pixel size

         glReadPixels(0, 0, Panel2.Width, Panel2.Height, GL_RGBA, GL_UNSIGNED_BYTE, Pointer(Cardinal(buffer) + 18));

         AssignFile(f, Filename);
         Rewrite(f, 1);

         for i := 0 to 17 do
            BlockWrite(f, buffer[i], sizeof(byte) , temp);

         c := 18;
         for i := 0 to (Panel2.Width * Panel2.Height)-1 do
         begin
            BlockWrite(f, buffer[c+2], sizeof(byte) , temp);
            BlockWrite(f, buffer[c+1], sizeof(byte) , temp);
            BlockWrite(f, buffer[c], sizeof(byte) , temp);
            inc(c,4);
         end;
//         ShowMessage('Screenshot taken. Width = ' + IntToStr(Panel2.Width) + ', Height = ' + IntToStr(Panel2.Height) + ' and BufferSize is ' + IntToStr(SizeOf(Buffer)));

         closefile(f);
      end;
   except
      ShowMessage('Not enough RAM to create this screenshot. Unload some programs and try again.');
   end;
   finalize(buffer);
end;

procedure TFrm3DPreview.ScreenShotJPG(Filename : string; Compression : integer);
var
  JPEGImage: TJPEGImage;
  Bitmap : TBitmap;
begin
   MakeMeAScreenshotName(Filename,'.jpg');

   if Filename = '' then
      exit;

  Bitmap := ScreenShot_BitmapResult;
  JPEGImage := TJPEGImage.Create;
  JPEGImage.Assign(Bitmap);
  JPEGImage.CompressionQuality := 100 - Compression;
  JPEGImage.SaveToFile(Filename);
  Bitmap.Free;
  JPEGImage.Free;
end;

procedure TFrm3DPreview.ScreenShotBMP(Filename : string);
var
  Bitmap : TBitmap;
begin
   MakeMeAScreenshotName(Filename,'.bmp');

   if Filename = '' then
      exit;

  Bitmap := ScreenShot_BitmapResult;
  Bitmap.SaveToFile(Filename);
  Bitmap.Free;
end;


procedure TFrm3DPreview.ScreenShotPNG(Filename : string);
var
  PNGImage: TPNGObject;
  Bitmap : TBitmap;
begin
   MakeMeAScreenshotName(Filename,'.png');

   if Filename = '' then
      exit;

  Bitmap := ScreenShot_BitmapResult;
  PNGImage := TPNGObject.Create;
  PNGImage.Assign(Bitmap);
  PNGImage.SaveToFile(Filename);
  Bitmap.Free;
  PNGImage.Free;
end;

procedure TFrm3DPReview.ScreenShotGIF(GIFIMAGE : TGIFImage; Filename : string);
begin
   MakeMeAScreenshotName(Filename,'.gif');

   if Filename = '' then
      exit;

   GIFImage.SaveToFile(Filename);
end;

// Borrowed from the Voxel Engine used on OS: Voxel Viewer 1.7
function TFrm3DPReview.ScreenShot_BitmapResult : TBitmap;
var
   buffer: array of byte;
   x,y, i: integer;
   Bitmap : TBitmap;
begin
   SetLength(buffer, (Panel2.Width * Panel2.Height * 4));

   glReadPixels(0, 0, Panel2.Width, Panel2.Height, GL_RGBA, GL_UNSIGNED_BYTE, Pointer(Cardinal(buffer)));

   i := 0;
   Bitmap := TBitmap.Create;
   Bitmap.Width := Panel2.Width;
   Bitmap.Height := Panel2.Height;
   for y := 1 to Panel2.Height do
      for x := 1 to Panel2.Width do
      begin
         Bitmap.Canvas.Pixels[x,Panel2.Height-y-1] := RGB(buffer[i],buffer[i+1],buffer[i+2]);
         inc(i,4);
      end;
   SetLength(buffer,0);
   finalize(buffer);
   Result := Bitmap;
end;

Function TFrm3DPReview.GetVXLColor(Color,Normal : integer) : TVector3f;
begin
   if SpectrumMode = ModeColours then
      Result := GetCorrectColour(color, RemapColour)
   else
   begin
      Result := SetVector(0.5, 0.5, 0.5);
   end;
end;

{------------------------------------------------------------------}
{  Function to draw the actual scene                               }
{------------------------------------------------------------------}
procedure TFrm3DPReview.DrawMe();
var
   x,Section : integer;
   Scale,MinBounds : TVector3f;
//   Matrix : TGlmatrixf4;
begin
   try
   if (not Showing) then exit;
   glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);    // Clear The Screen And The Depth Buffer
   glClearColor(BGColor.X, BGColor.Y, BGColor.Z, 1.0); 	   // Black Background

   if (not VoxelOpen) then exit;

   if XRotB then
      XRot := XRot + XRot2;

   if YRotB then
      YRot := YRot + YRot2;

   XRot := CleanAngle(XRot);
   YRot := CleanAngle(YRot);

   // For true normals, we activate all OpenGL stuff.
   glEnable(GL_LIGHT0);
   glEnable(GL_LIGHTING);
   glEnable(GL_COLOR_MATERIAL);

   // We'll only render anything if there is a voxel to render.
   if VoxelBoxGroup3D.NumBoxes > 0 then
   begin
      glPushMatrix;
      glLoadIdentity();                                       // Reset The View
      glTranslatef(0, 0, Depth);
      glRotatef(XRot, 1, 0, 0);
      glRotatef(YRot, 0, 0, 1);
      // Here we make the OpenGL list to speed up the render.
      for Section := Low(VoxelBoxGroup3D.Section) to High(VoxelBoxGroup3D.Section) do
      begin
         GetScaleWithMinBounds(VoxelFile.Section[VoxelBoxGroup3D.Section[Section].ID],Scale,MinBounds);
         if (VoxelBoxGroup3D.Section[Section].List < 1) or RebuildLists then
         begin
            if (VoxelBoxGroup3D.Section[Section].List > 0) then
               glDeleteLists(VoxelBoxGroup3D.Section[Section].List,1);
            VoxelBoxGroup3D.Section[Section].List := glGenLists(1);
            glNewList(VoxelBoxGroup3D.Section[Section].List, GL_COMPILE);
            // Now, we hunt all voxel boxes...
            for x := Low(VoxelBoxGroup3D.Section[Section].Box) to High(VoxelBoxGroup3D.Section[Section].Box) do
            begin
               DrawBox(VoxelBoxGroup3D.Section[Section].Box[x].Position, GetVXLColor(VoxelBoxGroup3D.Section[Section].Box[x].Color, VoxelBoxGroup3D.Section[Section].Box[x].Normal), Scale, VoxelBoxGroup3D.Section[Section].Box[x]);
            end;
            glEndList;
         end;
         glPushMatrix;
            ApplyMatrix(Scale,VoxelBoxGroup3D.Section[Section].ID,HVAFrame);
            glCallList(VoxelBoxGroup3D.Section[Section].List);
         glPopMatrix;
      end;
      // The get camera settings.
      glPopMatrix;

      RebuildLists := false;
      // End of the final voxel rendering part.
      glDisable(GL_TEXTURE_2D);

      glLoadIdentity;
      glDisable(GL_DEPTH_TEST);
      glMatrixMode(GL_PROJECTION);
      glPushMatrix;
      glLoadIdentity;
      glOrtho(0,Panel2.Width,0,Panel2.Height,-1,1);
      glMatrixMode(GL_MODELVIEW);
      glPushMatrix;
      glLoadIdentity;

      glDisable(GL_LIGHT0);
      glDisable(GL_LIGHTING);
      glDisable(GL_COLOR_MATERIAL);

      if ScreenieType = stNone then
      begin
         glColor3f(FontColor.X, FontColor.Y, FontColor.Z);

         glRasterPos2i(1, 2);
         glPrint(PChar('Voxels Used: ' + IntToStr(VoxelBox_No)));

         glRasterPos2i(1, 13);
         glPrint(PChar('Depth: ' + IntToStr(trunc(Depth))));

         glRasterPos2i(1, Panel2.Height - 9);
         glPrint(PChar('FPS: ' + IntToStr(trunc(FPS))));

         if FrmMain.DebugMode1.Checked then
         begin
            glRasterPos2i(1, Panel2.Height - 19);
            glPrint(PChar('DEBUG -  XRot:' + floattostr(XRot) + ' YRot:' + floattostr(YRot)));

            glRasterPos2i(1, Panel2.Height - 38);
            glPrint(PChar('Highest Normal:' + floattostr(HighestNormal)));
         end;
      end;

      glMatrixMode(GL_PROJECTION);
      glPopMatrix;
      glMatrixMode(GL_MODELVIEW);
      glPopMatrix;

      if ScreenieType <> stNone then
      begin
         case (ScreenieType) of
            stBmp:
            begin
               ScreenShotBMP(VXLFilename);
               height := oldh;
               width := oldw;
               ScreenieType := stNone;
            end;
            stTga:
            begin
               ScreenShot(VXLFilename);
               height := oldh;
               width := oldw;
               ScreenieType := stNone;
            end;
            stJpg:
            begin
               ScreenshotJPG(VXLFilename,1);
               height := oldh;
               width := oldw;
               ScreenieType := stNone;
            end;
            stPng:
            begin
               ScreenshotPNG(VXLFilename);
               height := oldh;
               width := oldw;
               ScreenieType := stNone;
            end;
            stGif:
            begin
               GifAnimateAddImage(ScreenShot_BitmapResult, False, 10);
               if (YRot = 360) then
               begin
                  ScreenieType := stNone;
                  FrmMain.btn3DRotateY.Down := False;
                  ScreenShotGIF(GifAnimateEndGif, VXLFilename);

                  btn3DRotateY.Enabled  := True;
                  btn3DRotateY2.Enabled := True;
                  btn3DRotateX.Enabled  := True;
                  btn3DRotateX2.Enabled := True;
                  spin3Djmp.Enabled     := True;
                  SpeedButton1.Enabled  := True;
                  SpeedButton2.Enabled  := True;

                  YRot := 225;
                  XRotB := False;
                  YRotB := False;

                  height := oldh;
                  width := oldw;
               end;
            end;
         end;
      end;

      glEnable(GL_DEPTH_TEST);
   end;
   except
      exit;
   end;
end;


{------------------------------------------------------------------}
procedure TFrm3DPReview.FormCreate(Sender: TObject);
var
   pfd : TPIXELFORMATDESCRIPTOR;
   pf  : Integer;
begin
   IsReady := false;
   // OpenGL initialization
   dc:=GetDC(Panel2.Handle);

   BGColor   := SetVector(140/255,170/255,235/255);
   FontColor := SetVector(1,1,1);
   Size      := 0.1;

   RemapColour.X := RemapColourMap[0].R /255;
   RemapColour.Y := RemapColourMap[0].G /255;
   RemapColour.Z := RemapColourMap[0].B /255;

   // PixelFormat
   pfd.nSize:=sizeof(pfd);
   pfd.nVersion:=1;
   pfd.dwFlags:=PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER or 0;
   pfd.iPixelType:=PFD_TYPE_RGBA;      // PFD_TYPE_RGBA or PFD_TYPEINDEX
   pfd.cColorBits:=32;

   pf :=ChoosePixelFormat(dc, @pfd);   // Returns format that most closely matches above pixel format
   SetPixelFormat(dc, pf, @pfd);

   rc :=wglCreateContext(dc);    // Rendering Context = window-glCreateContext
   wglMakeCurrent(dc,rc);        // Make the DC (Form1) the rendering Context
   ActivateRenderingContext(DC, RC);

   glClearColor(0.0, 0.0, 0.0, 0.0); 	   // Black Background
   glShadeModel(GL_SMOOTH);                 // Enables Smooth Color Shading
   glClearDepth(1.0);                       // Depth Buffer Setup
   glEnable(GL_DEPTH_TEST);                 // Enable Depth Buffer
   glDepthFunc(GL_LESS);		           // The Type Of Depth Test To Do

   glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);   //Realy Nice perspective calculations

   glEnable(GL_TEXTURE_2D);                     // Enable Texture Mapping

   BuildFont;

   glEnable(GL_CULL_FACE);
   glCullFace(GL_BACK);

   xRot :=-90;
   yRot :=-85;
   Depth :=-30;
   DemoStart :=GetTickCount();

   QueryPerformanceFrequency(FFrequency); // get high-resolution Frequency
   QueryPerformanceCounter(FoldTime);

   SpFrame.Value := 1;
   SpFrame.MaxValue := HVAFile.Header.N_Frames;
   HVAFrame := 0;

   RebuildLists := false;
   Update3dView(ActiveSection);
   IsReady := true;
end;


{------------------------------------------------------------------}
{------------------------------------------------------------------}
procedure TFrm3DPReview.Idle(Sender: TObject; var Done: Boolean);
var tmp : int64;
    t2 : double;
begin
   if not IsReady then exit;
   Done := FALSE;

   LastTime :=ElapsedTime;
   ElapsedTime :=GetTickCount() - DemoStart;     // Calculate Elapsed Time
   ElapsedTime :=(LastTime + ElapsedTime) DIV 2; // Average it out for smoother movement

   QueryPerformanceCounter(tmp);
   t2 := tmp-FoldTime;
   FPS := t2/FFrequency;
   FoldTime := TMP;
   FPS := 1/FPS;

   wglMakeCurrent(dc,rc);        // Make the DC (Form1) the rendering Context
   DrawMe();                         // Draw the scene
   SwapBuffers(DC);                  // Display the scene
end;


{------------------------------------------------------------------}
{------------------------------------------------------------------}
procedure TFrm3DPReview.FormResize(Sender: TObject);
begin
   if width < 330 then
      Width := 330;
   wglMakeCurrent(dc,rc);        // Make the DC (Form1) the rendering Context
   glViewport(0, 0, Panel2.Width, Panel2.Height);    // Set the viewport for the OpenGL window
   glMatrixMode(GL_PROJECTION);        // Change Matrix Mode to Projection
   glLoadIdentity();                   // Reset View
   gluPerspective(45.0, Panel2.Width/Panel2.Height, 1.0, 500.0);  // Do the perspective calculations. Last value = max clipping depth

   glMatrixMode(GL_MODELVIEW);         // Return to the modelview matrix  }
end;


{------------------------------------------------------------------}
{------------------------------------------------------------------}
procedure TFrm3DPReview.FormDestroy(Sender: TObject);
begin
//   wglMakeCurrent(0,0);
   wglDeleteContext(rc);
   FrmMain.p_Frm3DPreview := nil;
end;


{------------------------------------------------------------------}
{------------------------------------------------------------------}
procedure TFrm3DPReview.Panel2MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
   if MouseButton = 1 then
   begin
      xRot := xRot + (Y - Ycoord)/2;  // moving up and down = rot around X-axis
      yRot := yRot + (X - Xcoord)/2;
      Xcoord := X;
      Ycoord := Y;
   end;
   if MouseButton = 2 then
   begin
      Depth :=Depth - (Y-ZCoord)/3;
      Zcoord := Y;
   end;
end;


{------------------------------------------------------------------}
{------------------------------------------------------------------}
procedure TFrm3DPReview.Panel2MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
   if Button = mbLeft  then
   begin
      MouseButton :=1;
      Xcoord := X;
      Ycoord := Y;
   end;
   if Button = mbRight then
   begin
      MouseButton :=2;
      Zcoord := Y;
   end;
end;

procedure TFrm3DPReview.Panel2MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
   MouseButton :=0;
end;

procedure TFrm3DPReview.Exit1Click(Sender: TObject);
begin
   Close;
end;

function GetMD3Name(Filename : string) : string;
var
   FN,R : string;
   X : integer;
begin
   FN := ExtractFileDir(Filename);
   R := '';

   for X := Length(FN) downto 1 do
      if (Length(R) < 1) or ((copy(FN,X,1) <> '\') and (copy(FN,X,1) <> '/')) then
         R := copy(FN,X,1)+R
      else
         break;

   Result := R;
end;

procedure TFrm3DPReview.Update3dView(Vxl : TVoxelSection);
var x,y,z: Byte;
    v: TVoxelUnpacked;
    Section : integer;
    Scale,MinBounds : TVector3f;
begin
   if not IsEditable then exit;

   ClearVoxelBoxes(VoxelBoxGroup3D);
   VoxelBoxGroup3D.NumBoxes := 0;

   if WholeVoxel1.Checked then
   begin
      SetLength(VoxelBoxGroup3D.Section,VoxelFile.Header.NumSections);
      for Section := Low(VoxelBoxGroup3D.Section) to High(VoxelBoxGroup3D.Section) do
      begin
         GetScaleWithMinBounds(VoxelFile.Section[Section],Scale,MinBounds);
         VoxelBoxGroup3D.Section[Section].ID := Section;
         VoxelBox_No := 0;
         for z := 0 to (VoxelFile.Section[Section].Tailer.zSize - 1) do
         begin
            for y := 0 to (VoxelFile.Section[Section].Tailer.YSize - 1) do
            begin
               for x := 0 to (VoxelFile.Section[Section].Tailer.xSize - 1) do
               begin
                  VoxelFile.Section[Section].GetVoxel(x, y, z, v);

                  if v.Used = True then
                  begin
                     SetLength(VoxelBoxGroup3D.Section[Section].Box, VoxelBox_No+1);
                     VoxelBoxGroup3D.Section[Section].Box[VoxelBox_No].Faces[1]   := CheckFace(VoxelFile.Section[Section], x, y + 1, z);
                     VoxelBoxGroup3D.Section[Section].Box[VoxelBox_No].Faces[2]   := CheckFace(VoxelFile.Section[Section], x, y - 1, z);
                     VoxelBoxGroup3D.Section[Section].Box[VoxelBox_No].Faces[3]   := CheckFace(VoxelFile.Section[Section], x, y, z + 1);
                     VoxelBoxGroup3D.Section[Section].Box[VoxelBox_No].Faces[4]   := CheckFace(VoxelFile.Section[Section], x, y, z - 1);
                     VoxelBoxGroup3D.Section[Section].Box[VoxelBox_No].Faces[5]   := CheckFace(VoxelFile.Section[Section], x - 1, y, z);
                     VoxelBoxGroup3D.Section[Section].Box[VoxelBox_No].Faces[6]   := CheckFace(VoxelFile.Section[Section], x + 1, y, z);

                     VoxelBoxGroup3D.Section[Section].Box[VoxelBox_No].Position.X := (MinBounds.X + (X * Scale.X));
                     VoxelBoxGroup3D.Section[Section].Box[VoxelBox_No].Position.Y := (MinBounds.Y + (Y * Scale.Y));
                     VoxelBoxGroup3D.Section[Section].Box[VoxelBox_No].Position.Z := (MinBounds.Z + (Z * Scale.Z));
                     VoxelBoxGroup3D.Section[Section].Box[VoxelBox_No].Color  := v.Colour;
                     VoxelBoxGroup3D.Section[Section].Box[VoxelBox_No].Normal := v.Normal;
                     Inc(VoxelBox_No);
                  end;
               end;
            end;
         end;
         VoxelBoxGroup3D.NumBoxes := VoxelBoxGroup3D.NumBoxes + VoxelBox_No;
      end;
   end
   else
   begin
      SetLength(VoxelBoxGroup3D.Section,1);
      GetScaleWithMinBounds(Vxl,Scale,MinBounds);
      VoxelBoxGroup3D.Section[0].ID := Vxl.Header.Number;
      for z := 0 to (Vxl.Tailer.zSize - 1) do
      begin
         for y := 0 to (Vxl.Tailer.YSize - 1) do
         begin
            for x := 0 to (Vxl.Tailer.xSize - 1) do
            begin
               Vxl.GetVoxel(x, y, z, v);

               if v.Used = True then
               begin
                  SetLength(VoxelBoxGroup3D.Section[0].Box, VoxelBox_No+1);
                  VoxelBoxGroup3D.Section[0].Box[VoxelBox_No].Faces[1]   := CheckFace(Vxl, x, y + 1, z);
                  VoxelBoxGroup3D.Section[0].Box[VoxelBox_No].Faces[2]   := CheckFace(Vxl, x, y - 1, z);
                  VoxelBoxGroup3D.Section[0].Box[VoxelBox_No].Faces[3]   := CheckFace(Vxl, x, y, z + 1);
                  VoxelBoxGroup3D.Section[0].Box[VoxelBox_No].Faces[4]   := CheckFace(Vxl, x, y, z - 1);
                  VoxelBoxGroup3D.Section[0].Box[VoxelBox_No].Faces[5]   := CheckFace(Vxl, x - 1, y, z);
                  VoxelBoxGroup3D.Section[0].Box[VoxelBox_No].Faces[6]   := CheckFace(Vxl, x + 1, y, z);

                  VoxelBoxGroup3D.Section[0].Box[VoxelBox_No].Position.X := (MinBounds.X + (X * Scale.X));
                  VoxelBoxGroup3D.Section[0].Box[VoxelBox_No].Position.Y := (MinBounds.Y + (Y * Scale.Y));
                  VoxelBoxGroup3D.Section[0].Box[VoxelBox_No].Position.Z := (MinBounds.Z + (Z * Scale.Z));

                  VoxelBoxGroup3D.Section[0].Box[VoxelBox_No].Color  := v.Colour;
                  VoxelBoxGroup3D.Section[0].Box[VoxelBox_No].Normal := v.Normal;
                  Inc(VoxelBox_No);
               end;
            end;
         end;
      end;
      VoxelBoxGroup3D.NumBoxes := VoxelBox_No;
   end;
   RebuildLists := true;
end;

procedure TFrm3DPReview.BackgroundColour1Click(Sender: TObject);
begin
   ColorDialog1.Color := TVector3fToTColor(BGColor);
   if ColorDialog1.Execute then
      BGColor := TColorToTVector3f(ColorDialog1.Color);
end;

Procedure TFrm3DPReview.SetRotationAdders;
var
   V : single;
begin
   try
      V := spin3Djmp.Value / 10;
   except
      exit; // Not a value
   end;

   if btn3DRotateX2.Down then
      XRot2 := -V
   else if btn3DRotateX.Down then
      XRot2 := V;

   if btn3DRotateY2.Down then
      YRot2 := -V
   else if btn3DRotateY.Down then
      YRot2 := V;
end;

procedure TFrm3DPReview.btn3DRotateX2Click(Sender: TObject);
begin
   if btn3DRotateX2.Down then
   begin
      SetRotationAdders;
      XRotB := True;
   end
   else
      XRotB := false;
end;

procedure TFrm3DPReview.btn3DRotateXClick(Sender: TObject);
begin
   if btn3DRotateX.Down then
   begin
      SetRotationAdders;
      XRotB := True;
   end
   else
      XRotB := false;
end;

procedure TFrm3DPReview.btn3DRotateY2Click(Sender: TObject);
begin
   if btn3DRotateY2.Down then
   begin
      SetRotationAdders;
      YRotB := True;
   end
   else
      YRotB := false;
end;

procedure TFrm3DPReview.btn3DRotateYClick(Sender: TObject);
begin
   if btn3DRotateY.Down then
   begin
      SetRotationAdders;
      YRotB := True;
   end
   else
      YRotB := false;
end;

procedure TFrm3DPReview.spin3DjmpChange(Sender: TObject);
begin
   SetRotationAdders;
end;

procedure TFrm3DPReview.SpPlayClick(Sender: TObject);
begin
   // Enable timer here.
   if not AnimationTimer.Enabled then
   begin
      SpPlay.Glyph.LoadFromFile(ExtractFileDir(ParamStr(0)) + '/images/pause.bmp');
      AnimationTimer.Enabled := true;
   end
   else
   begin
      AnimationTimer.Enabled := false;
      SpPlay.Glyph.LoadFromFile(ExtractFileDir(ParamStr(0)) + '/images/play.bmp');
   end;
end;

procedure TFrm3DPReview.SpStopClick(Sender: TObject);
begin
   AnimationTimer.Enabled := false;
   HVAFrame := 0;
   SpFrame.Value := 1;
   SpPlay.Glyph.LoadFromFile(ExtractFileDir(ParamStr(0)) + '/images/play.bmp');
end;

procedure TFrm3DPReview.AnimationTimerTimer(Sender: TObject);
begin
   if HVAFile.Header.N_Frames = 1 then
   begin
      SpStopClick(Sender);
   end;
   HVAFrame := (HVAFrame + 1) mod SpFrame.MaxValue;
   SpFrame.Value := HVAFrame + 1;
end;

procedure TFrm3DPReview.SpFrameChange(Sender: TObject);
begin
   if StrToIntDef(SpFrame.Text,-1) <> -1 then
   begin
      if SpFrame.Value > SpFrame.MaxValue then
         SpFrame.Value := 1
      else if SpFrame.Value < 1 then
         SpFrame.Value := SpFrame.MaxValue;
      HVAFrame := SpFrame.Value-1;
   end;
end;

procedure TFrm3DPReview.FontColor1Click(Sender: TObject);
begin
   ColorDialog1.Color := TVector3fToTColor(FontColor);
   if ColorDialog1.Execute then
      FontColor := TColorToTVector3f(ColorDialog1.Color);
end;

procedure TFrm3DPReview.Front1Click(Sender: TObject);
begin
   XRot := -90;
   YRot := -90;
   //Depth := -30;
end;

procedure TFrm3DPReview.Back1Click(Sender: TObject);
begin
   XRot := -90;
   YRot := 90;
   //Depth := -30;
end;

procedure TFrm3DPReview.LEft1Click(Sender: TObject);
begin
   XRot := -90;
   YRot := 0;
   //Depth := -30;
end;

procedure TFrm3DPReview.Right1Click(Sender: TObject);
begin
   XRot := -90;
   YRot := -180;
   //Depth := -30;
end;

procedure TFrm3DPReview.Bottom1Click(Sender: TObject);
begin
   XRot := 180;
   YRot := 180;
   //Depth := -30;
end;

procedure TFrm3DPReview.op1Click(Sender: TObject);
begin
   XRot := 0;
   YRot := 180;
   //Depth := -30;
end;

procedure TFrm3DPReview.Cameo1Click(Sender: TObject);
begin
   XRot := 287;//-72.5;
   YRot := 225;//237;
   //Depth := -30;
end;

procedure TFrm3DPReview.SpeedButton1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   Popup3d.Popup(Left+SpeedButton1.Left+5,Top+ 60+ SpeedButton1.Top);
end;

procedure TFrm3DPReview.Cameo21Click(Sender: TObject);
begin
   XRot := 287;//-72.5;
   YRot := 315;//302;
   //Depth := -30;
end;

procedure TFrm3DPReview.Cameo31Click(Sender: TObject);
begin
   XRot := 287;//-72.5;
   YRot := 255;//302;
   //Depth := -30;
end;

procedure TFrm3DPReview.Cameo41Click(Sender: TObject);
begin
   XRot := 287;//-72.5;
   YRot := 285;//302;
   //Depth := -30;
end;

procedure TFrm3DPReview.ake360DegScreenshots1Click(Sender: TObject);
begin
   GifAnimateBegin;
   oldh := height;
//   height := 256;

   oldw := width;
//   width := 248;

   btn3DRotateY.Down := true;
   btn3DRotateY2.Down := false;
   btn3DRotateX.Down := false;
   btn3DRotateX2.Down := false;
   btn3DRotateYClick(sender);
   XRotB := false;
   YRot := 0;
   ScreenieType := stGif;
   spin3Djmp.Value := 40;

   btn3DRotateY.Enabled := false;
   btn3DRotateY2.Enabled := false;
   btn3DRotateX.Enabled := false;
   btn3DRotateX2.Enabled := false;
   spin3Djmp.Enabled := false;
   SpeedButton1.Enabled := false;
   SpeedButton2.Enabled := false;

   //Frm3DPReview.Enabled := false;
end;

procedure TFrm3DPReview.akeScreenshot1Click(Sender: TObject);
begin
   oldh := height;
   oldw := width;
   ScreenieType := stTGA;
end;

procedure TFrm3DPReview.akeScreenshotBMP1Click(Sender: TObject);
begin
   oldh := height;
   oldw := width;
   ScreenieType := stBMP;
end;

procedure TFrm3DPReview.akeScreenshotJPG1Click(Sender: TObject);
begin
   oldh := height;
   oldw := width;
   ScreenieType := stJPG;
end;

procedure TFrm3DPReview.akeScreenshotPNG1Click(Sender: TObject);
begin
   oldh := height;
   oldw := width;
   ScreenieType := stPNG;
end;

procedure TFrm3DPReview.ClearRemapClicks;
begin
   Red1.Checked := false;
   Blue1.Checked := false;
   Green1.Checked := false;
   White1.Checked := false;
   Orange1.Checked := false;
   Magenta1.Checked := false;
   Purple1.Checked := false;
   Gold1.Checked := false;
   DarkSky1.Checked := false;
end;

procedure TFrm3DPReview.Red1Click(Sender: TObject);
begin
   ClearRemapClicks;
   Red1.Checked := true;
   RemapColour.X := RemapColourMap[0].R /255;
   RemapColour.Y := RemapColourMap[0].G /255;
   RemapColour.Z := RemapColourMap[0].B /255;
   RebuildLists := true;
end;

procedure TFrm3DPReview.Blue1Click(Sender: TObject);
begin
   ClearRemapClicks;
   Blue1.Checked := true;
   RemapColour.X := RemapColourMap[1].R /255;
   RemapColour.Y := RemapColourMap[1].G /255;
   RemapColour.Z := RemapColourMap[1].B /255;
   RebuildLists := true;
end;

procedure TFrm3DPReview.Green1Click(Sender: TObject);
begin
   ClearRemapClicks;
   Green1.Checked := true;
   RemapColour.X := RemapColourMap[2].R /255;
   RemapColour.Y := RemapColourMap[2].G /255;
   RemapColour.Z := RemapColourMap[2].B /255;
   RebuildLists := true;
end;

procedure TFrm3DPReview.White1Click(Sender: TObject);
begin
   ClearRemapClicks;
   White1.Checked := true;
   RemapColour.X := RemapColourMap[3].R /255;
   RemapColour.Y := RemapColourMap[3].G /255;
   RemapColour.Z := RemapColourMap[3].B /255;
   RebuildLists := true;
end;

procedure TFrm3DPReview.Orange1Click(Sender: TObject);
begin
   ClearRemapClicks;
   Orange1.Checked := true;
   RemapColour.X := RemapColourMap[4].R /255;
   RemapColour.Y := RemapColourMap[4].G /255;
   RemapColour.Z := RemapColourMap[4].B /255;
   RebuildLists := true;
end;

procedure TFrm3DPReview.Magenta1Click(Sender: TObject);
begin
   ClearRemapClicks;
   Magenta1.Checked := true;
   RemapColour.X := RemapColourMap[5].R /255;
   RemapColour.Y := RemapColourMap[5].G /255;
   RemapColour.Z := RemapColourMap[5].B /255;
   RebuildLists := true;
end;

procedure TFrm3DPReview.Purple1Click(Sender: TObject);
begin
   ClearRemapClicks;
   Purple1.Checked := true;
   RemapColour.X := RemapColourMap[6].R /255;
   RemapColour.Y := RemapColourMap[6].G /255;
   RemapColour.Z := RemapColourMap[6].B /255;
   RebuildLists := true;
end;

procedure TFrm3DPReview.Gold1Click(Sender: TObject);
begin
   ClearRemapClicks;
   Gold1.Checked := true;
   RemapColour.X := RemapColourMap[7].R /255;
   RemapColour.Y := RemapColourMap[7].G /255;
   RemapColour.Z := RemapColourMap[7].B /255;
   RebuildLists := true;
end;

procedure TFrm3DPReview.DarkSky1Click(Sender: TObject);
begin
   ClearRemapClicks;
   DarkSky1.Checked := true;
   RemapColour.X := RemapColourMap[8].R /255;
   RemapColour.Y := RemapColourMap[8].G /255;
   RemapColour.Z := RemapColourMap[8].B /255;
   RebuildLists := true;
end;

procedure TFrm3DPReview.SpeedButton2Click(Sender: TObject);
begin
   Depth := -30;
end;

Procedure TFrm3DPReview.Reset3DView;
begin
   SpeedButton2Click(nil); // Reset Depth
   Cameo1Click(nil); // Set To Cameo1
   XRotB := false;
   YRotB := false;

   btn3DRotateY.Down := false;
   btn3DRotateY2.Down := false;
   btn3DRotateX.Down := false;
   btn3DRotateX2.Down := false;
end;

procedure TFrm3DPReview.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   FrmMain.p_Frm3DPreview := nil;
   if FrmMain.Display3dView1.Checked then
      Application.OnIdle := nil;
   Free;
end;

procedure TFrm3DPReview.CurrentSectionOnly1Click(Sender: TObject);
begin
   CurrentSectionOnly1.Checked := not CurrentSectionOnly1.Checked;
   WholeVoxel1.Checked := not CurrentSectionOnly1.Checked;
   FrmMain.RefreshAll;
end;

procedure TFrm3DPReview.FormActivate(Sender: TObject);
begin
   FrmMain.OnActivate(sender);
end;

procedure TFrm3DPReview.FormDeactivate(Sender: TObject);
begin
   FrmMain.OnDeactivate(sender);
   AnimationState := AnimationTimer.Enabled;
   AnimationTimer.Enabled := false;
end;

end.
