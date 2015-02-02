{-----------------------------------------------------------------------------
 Unit Name: FormEdit3D
 Author:    oranke
 Date:      2015-02-02
 Purpose:


 History:
-----------------------------------------------------------------------------}


unit FormEdit3D;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls,

  Voxel_Engine,
  //VectorUtil,

  OpenGL15, StdCtrls;

type
  F32 = Single; 

  TFrmEdit3D = class(TForm)
    CtrlPanel: TPanel;
    Bevel1: TBevel;
    RenderPanel: TPanel;
    procedure RenderPaintMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure RenderPaintMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure RenderPaintMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure RenderPanelResize(Sender: TObject);
  private
    { Private declarations }
    fDC: HDC; // Device Context
    fRC: HGLRC; // Rendering Context

    CubicDrawID : GLuint;

  private
    fRotX, fRotY, fFov, fDist,
    fNear, fFar: Single;
    fLookAtPos: TVector3f;

    //fOrgPanelWindowProc: TWndMethod;
    //procedure RenderPanelWindowProc(var Message: TMessage);

    procedure RenderScene;

  protected
    procedure WndProc(var Message: TMessage); override;
  public
    { Public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetViewParams;
    procedure Idle(Sender: TObject; var Done: Boolean);
  end;

var
  FrmEdit3D: TFrmEdit3D = nil;

implementation

{$R *.dfm}

uses
  OGLUtil,

  ogl3dview_engine,

  FormMain, Voxel;


procedure RenderAxis(aScale: Single);
begin
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix();
  glScalef(aScale, aScale, aScale);

  // 3축 그리기.
  glDisable(GL_DEPTH_TEST);
  //glDepthMask(false);
  glColor3ub($FF, 0, 0);
  glBegin(GL_LINES);
    glVertex3f(0, 0, 0);
    glVertex3f(1, 0, 0);
  glEnd;

  glColor3ub(0, $FF, 0);
  glBegin(GL_LINES);
    glVertex3f(0, 0, 0);
    glVertex3f(0, 1, 0);
  glEnd;

  glColor3ub(0, 0, $FF);
  glBegin(GL_LINES);
    glVertex3f(0, 0, 0);
    glVertex3f(0, 0, 1);
  glEnd;
  //glDepthMask(true);
  glEnable(GL_DEPTH_TEST);

  glPopMatrix();
end;

{ TFrmEdit3D }

constructor TFrmEdit3D.Create(aOwner: TComponent);
begin
  inherited;
  //DoubleBuffered := true;

  //RenderPanel.DoubleBuffered := true;
  //fOrgPanelWindowProc := RenderPanel.WindowProc;
  //RenderPanel.WindowProc := RenderPanelWindowProc;

  InitOpenGL(RenderPanel.Handle, fDC, fRC);

  ActivateRenderingContext(fDC, fRC);

  glShadeModel(GL_SMOOTH); // Enables Smooth Color Shading
  glEnable(GL_DEPTH_TEST); // Enable Depth Buffer
  glDepthFunc(GL_LESS); // The Type Of Depth Test To Do

  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST); //Realy Nice perspective calculations

  //glEnable(GL_CULL_FACE);
  //glCullFace(GL_BACK);
  glEnable(GL_NORMALIZE);

  glEnable(GL_COLOR_MATERIAL);

  
  CubicDrawID := glGenLists(1);
  glNewList(CubicDrawID, GL_COMPILE);
  glBegin(GL_QUADS);
  glNormal3f( 0.0, 0.0, 1.0);					// Normal Pointing Towards Viewer
  glTexCoord2f(0.0, 1.0); glVertex3f(-1.0, -1.0,  1.0);	// Point 1 (Front)
  glTexCoord2f(1.0, 1.0); glVertex3f( 1.0, -1.0,  1.0);	// Point 2 (Front)
  glTexCoord2f(1.0, 0.0); glVertex3f( 1.0,  1.0,  1.0);	// Point 3 (Front)
  glTexCoord2f(0.0, 0.0); glVertex3f(-1.0,  1.0,  1.0);	// Point 4 (Front)
  // Back Face
  glNormal3f( 0.0, 0.0,-1.0);					// Normal Pointing Away From Viewer
  glTexCoord2f(0.0, 1.0); glVertex3f(-1.0, -1.0, -1.0);	// Point 1 (Back)
  glTexCoord2f(1.0, 1.0); glVertex3f(-1.0,  1.0, -1.0);	// Point 2 (Back)
  glTexCoord2f(1.0, 0.0); glVertex3f( 1.0,  1.0, -1.0);	// Point 3 (Back)
  glTexCoord2f(0.0, 0.0); glVertex3f( 1.0, -1.0, -1.0);	// Point 4 (Back)
  // Top Face
  glNormal3f( 0.0, 1.0, 0.0);					// Normal Pointing Up
  glTexCoord2f(0.0, 1.0); glVertex3f(-1.0,  1.0, -1.0);	// Point 1 (Top)
  glTexCoord2f(1.0, 1.0); glVertex3f(-1.0,  1.0,  1.0);	// Point 2 (Top)
  glTexCoord2f(1.0, 0.0); glVertex3f( 1.0,  1.0,  1.0);	// Point 3 (Top)
  glTexCoord2f(0.0, 0.0); glVertex3f( 1.0,  1.0, -1.0);	// Point 4 (Top)
  // Bottom Face
  glNormal3f( 0.0,-1.0, 0.0);					// Normal Pointing Down
  glTexCoord2f(0.0, 1.0); glVertex3f(-1.0, -1.0, -1.0);	// Point 1 (Bottom)
  glTexCoord2f(1.0, 1.0); glVertex3f( 1.0, -1.0, -1.0);	// Point 2 (Bottom)
  glTexCoord2f(1.0, 0.0); glVertex3f( 1.0, -1.0,  1.0);	// Point 3 (Bottom)
  glTexCoord2f(0.0, 0.0); glVertex3f(-1.0, -1.0,  1.0);	// Point 4 (Bottom)
  // Right face
  glNormal3f( 1.0, 0.0, 0.0);					// Normal Pointing Right
  glTexCoord2f(0.0, 1.0); glVertex3f( 1.0, -1.0, -1.0);	// Point 1 (Right)
  glTexCoord2f(1.0, 1.0); glVertex3f( 1.0,  1.0, -1.0);	// Point 2 (Right)
  glTexCoord2f(1.0, 0.0); glVertex3f( 1.0,  1.0,  1.0);	// Point 3 (Right)
  glTexCoord2f(0.0, 0.0); glVertex3f( 1.0, -1.0,  1.0);	// Point 4 (Right)
  // Left Face
  glNormal3f(-1.0, 0.0, 0.0);					// Normal Pointing Left
  glTexCoord2f(0.0, 1.0); glVertex3f(-1.0, -1.0, -1.0);	// Point 1 (Left)
  glTexCoord2f(1.0, 1.0); glVertex3f(-1.0, -1.0,  1.0);	// Point 2 (Left)
  glTexCoord2f(1.0, 0.0); glVertex3f(-1.0,  1.0,  1.0);	// Point 3 (Left)
  glTexCoord2f(0.0, 0.0); glVertex3f(-1.0,  1.0, -1.0);	// Point 4 (Left)
  glEnd();								// Done Drawing Quads
  glEndList();

  //------------------

  SetViewParams;

end;

destructor TFrmEdit3D.Destroy;
begin

  glDeleteLists(CubicDrawID, 1);

  CloseOpenGL(RenderPanel.Handle, fDC, fRC);

  inherited;
end;

procedure TFrmEdit3D.RenderScene;
//const
  //LightPosition : Array [0..3] of GLfloat = (1.5, 0.5, 1.0, 0.0);
var
  ix, iy, iz: Integer;
  v: TVoxelUnpacked;
  VoxelColor: TVector3f;
begin
  // Set the projection matrix
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluPerspective(fFov, RenderPanel.Width/RenderPanel.Height, fNear, fFar);
  //gluPerspective(fFov, RenderPaint.Width/RenderPaint.Height, 10, 1000);

  // Set up current camera
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  glTranslatef(0, 0, -fDist);
  glRotatef(fRotX, 1, 0, 0);
  glRotatef(fRotY, 0, 1, 0);
  glTranslatef(-fLookAtPos.X, -fLookAtPos.Y, -fLookAtPos.Z);

  glDisable(GL_LIGHT0);
  glDisable(GL_LIGHTING);
  glDisable(GL_DEPTH_TEST);

  RenderAxis(fDist / 3);

  
  glEnable(GL_LIGHT0);
  glEnable(GL_LIGHTING);
  glEnable(GL_DEPTH_TEST);

  with ActiveSection do
  begin
    glPushMatrix();
    
    for iz := 0 to Tailer.ZSize - 1 do
    begin
      //iz := 0;

      glPushMatrix();

      for iy := 0 to Tailer.YSize - 1 do
      begin
        //iy := 0;
        glPushMatrix();

        for ix := 0 to Tailer.XSize - 1 do
        begin
          GetVoxel(ix, iy, iz, v);
          if v.Used then
          begin
            VoxelColor := GetVXLColor(v.Colour, 0);
            glColor3f(
              VoxelColor.X,
              VoxelColor.Y,
              VoxelColor.Z
            );

            glPushMatrix();
              glScalef(0.5,0.5,0.5);
              glTranslatef(1,1,1);
              glCallList(CubicDrawID);
            glPopMatrix();
          end;

          glTranslatef(1, 0, 0);
        end;

        glPopMatrix();
        glTranslatef(0, 1, 0);

      end;

      glPopMatrix();
      glTranslatef(0, 0, 1); 
    end;
    
    glPopMatrix();
  end;


  {
  glPushMatrix();

    with ActiveSection.Tailer do
      glScalef(XSize, YSize, ZSize);

    glColor3ub($FF, $FF, $FF);
    glScalef(0.5,0.5,0.5);
    glTranslatef(1,1,1);
    glCallList(CubicDrawID);

  glPopMatrix();
  }




end;

procedure TFrmEdit3D.SetViewParams;
begin
  fRotX := 25;
  fRotY := -45;

  fFov  := 45;

  //with VoxelFile.Section[0].Tailer do
  with ActiveSection.Tailer do
  begin
    //fLookAtPos.X := 0;//XSize / 2;
    //fLookAtPos.Y := 0;//YSize / 2;
    //fLookAtPos.Z := 0;//ZSize / 2;
    fLookAtPos.X := XSize / 2;
    fLookAtPos.Y := YSize / 2;
    fLookAtPos.Z := ZSize / 2;

    fDist :=
      Sqrt(
        XSize * XSize +
        YSize * YSize +
        ZSize * ZSize
      ) * 3;

    fNear := fDist / 10;
    fFar  := fDist * 10; 
  end;
end;

procedure TFrmEdit3D.WndProc(var Message: TMessage);
begin
  inherited;
end;

procedure TFrmEdit3D.Idle(Sender: TObject; var Done: Boolean);
begin

  wglMakeCurrent(fDC, fRC);
  //SetCurrent(fDC, fRC);

  //glClearColor(0.5, 0.5, 1.0, 1.0);
  glClearColor(140 / 255, 170 / 255, 235 / 255, 1.0);
  glClearDepth(1.0); // Depth Buffer Setup
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT); // Clear The Screen And The Depth Buffer

  RenderScene();

  SwapBuffers(fDC); // Display the scene

end;


procedure TFrmEdit3D.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
  FrmEdit3D := nil;
end;

var
  ugMDownPos: TPoint;

procedure TFrmEdit3D.RenderPaintMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ugMDownPos := Point(X, Y);
end;

function NormalizeDegAngle(angle : F32) : F32;
const
  C_Inv_360    : F32 = 1/360;
  C_360        : F32 = 360;
  C_180        : F32 = 180;
begin
  Result:=angle-Int(angle*C_Inv_360) * C_360;
  if Result> C_180 then
    Result:=Result - C_360
  else if Result<- C_180 then
    Result:=Result+ C_360;
end;


procedure TFrmEdit3D.RenderPaintMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if ssLeft in Shift then
  begin
    fRotX := fRotX - (ugMDownPos.Y - Y) / 6;
    if fRotX > 90 then fRotX := 90;
    if fRotX < -90 then fRotX := -90;

    fRotY := NormalizeDegAngle(fRotY - (ugMDownPos.X - X) / 6);
  end;

  ugMDownPos := Point(X, Y);
end;

procedure TFrmEdit3D.RenderPaintMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
//
end;

procedure TFrmEdit3D.RenderPanelResize(Sender: TObject);
begin
  wglMakeCurrent(fDC, fRC);
  glViewport(0, 0, RenderPanel.Width, RenderPanel.Height);
end;


end.
