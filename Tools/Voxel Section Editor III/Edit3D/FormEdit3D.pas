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

  OpenGL15;

type
  TFrmEdit3D = class(TForm)
    CtrlPanel: TPanel;
    Bevel1: TBevel;
    RenderPanel: TPanel;
    RenderPaint: TPaintBox;
    procedure RenderPaintMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure RenderPaintMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure RenderPaintMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure RenderPaintPaint(Sender: TObject);
  private
    { Private declarations }
    fDC: HDC; // Device Context
    fRC: HGLRC; // Rendering Context

    fCubicDrawID: GLUint;

  private
    fRotX, fRotY, fFov, fDist,
    fNear, fFar: Single;
    fLookAtPos: TVector3f;

    procedure RenderScene;
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



{ TFrmEdit3D }

constructor TFrmEdit3D.Create(aOwner: TComponent);
begin
  inherited;

  InitOpenGL(RenderPanel.Handle, fDC, fRC);

  //glEnable(GL_CULL_FACE);
  //glCullFace(GL_BACK);
  glEnable(GL_NORMALIZE);

  fCubicDrawID := glGenLists(1);
  glNewList(fCubicDrawID, GL_COMPILE);
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
  glDeleteLists(fCubicDrawID, 1);

  CloseOpenGL(RenderPanel.Handle, fDC, fRC);
  inherited;
end;

procedure TFrmEdit3D.RenderScene;
begin

end;

procedure TFrmEdit3D.SetViewParams;
begin
  fRotX := 25;
  fRotY := 0;
  fFov  := 45;

  //with VoxelFile.Section[0].Tailer do
  with ActiveSection.Tailer do
  begin
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

procedure TFrmEdit3D.Idle(Sender: TObject; var Done: Boolean);
begin
  wglMakeCurrent(fDC, fRC);
  //SetCurrent(fDC, fRC);

  //glClearColor(0.5, 0.5, 1.0, 1.0);
  glClearColor(140 / 255, 170 / 255, 235 / 255, 1.0);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT); // Clear The Screen And The Depth Buffer

  RenderScene();


  SwapBuffers(fDC); // Display the scene
end;


procedure TFrmEdit3D.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
  FrmEdit3D := nil;
end;


procedure TFrmEdit3D.RenderPaintMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
//
end;

procedure TFrmEdit3D.RenderPaintMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
//
end;

procedure TFrmEdit3D.RenderPaintMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
//
end;

procedure TFrmEdit3D.RenderPaintPaint(Sender: TObject);
begin
//
end;



end.
