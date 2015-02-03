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

  OpenGL15, StdCtrls, Menus, Buttons;

type
  F32 = Single; 

  TFrmEdit3D = class(TForm)
    CtrlPanel: TPanel;
    Bevel1: TBevel;
    RenderPanel: TPanel;
    UpsideMenuBtn: TSpeedButton;
    UpsidePopup: TPopupMenu;
    UpsideMenuX: TMenuItem;
    UpsideMenuY: TMenuItem;
    UpsideMenuZ: TMenuItem;
    ViewButton: TSpeedButton;
    AddButton: TSpeedButton;
    DelButton: TSpeedButton;
    ReplaceButton: TSpeedButton;
    procedure RenderPaintMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure RenderPaintMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure RenderPaintMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure RenderPanelResize(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure UpsideMenuBtnClick(Sender: TObject);
    procedure UpsideMenuClick(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
    fDC: HDC; // Device Context
    fRC: HGLRC; // Rendering Context

    CubicDrawID : GLuint;

  private
    fRotU, fRotV, // X, Y회전을 U, V로 개념변경. 
    fFov, fDist,
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
    procedure Idle(Sender: TObject);
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
  glVertex3f(0.0, 0.0,  1.0);	// Point 1 (Front)
  glVertex3f( 1.0, 0.0,  1.0);	// Point 2 (Front)
  glVertex3f( 1.0,  1.0,  1.0);	// Point 3 (Front)
  glVertex3f(0.0,  1.0,  1.0);	// Point 4 (Front)
  // Back Face
  glNormal3f( 0.0, 0.0,-1.0);					// Normal Pointing Away From Viewer
  glVertex3f(0.0, 0.0, 0.0);	// Point 1 (Back)
  glVertex3f(0.0,  1.0, 0.0);	// Point 2 (Back)
  glVertex3f( 1.0,  1.0, 0.0);	// Point 3 (Back)
  glVertex3f( 1.0, 0.0, 0.0);	// Point 4 (Back)
  // Top Face
  glNormal3f( 0.0, 1.0, 0.0);					// Normal Pointing Up
  glVertex3f(0.0,  1.0, 0.0);	// Point 1 (Top)
  glVertex3f(0.0,  1.0,  1.0);	// Point 2 (Top)
  glVertex3f( 1.0,  1.0,  1.0);	// Point 3 (Top)
  glVertex3f( 1.0,  1.0, 0.0);	// Point 4 (Top)
  // Bottom Face
  glNormal3f( 0.0,-1.0, 0.0);					// Normal Pointing Down
  glVertex3f(0.0, 0.0, 0.0);	// Point 1 (Bottom)
  glVertex3f( 1.0, 0.0, 0.0);	// Point 2 (Bottom)
  glVertex3f( 1.0, 0.0,  1.0);	// Point 3 (Bottom)
  glVertex3f(0.0, 0.0,  1.0);	// Point 4 (Bottom)
  // Right face
  glNormal3f( 1.0, 0.0, 0.0);					// Normal Pointing Right
  glVertex3f( 1.0, 0.0, 0.0);	// Point 1 (Right)
  glVertex3f( 1.0,  1.0, 0.0);	// Point 2 (Right)
  glVertex3f( 1.0,  1.0,  1.0);	// Point 3 (Right)
  glVertex3f( 1.0, 0.0,  1.0);	// Point 4 (Right)
  // Left Face
  glNormal3f(-1.0, 0.0, 0.0);					// Normal Pointing Left
  glVertex3f(0.0, 0.0, 0.0);	// Point 1 (Left)
  glVertex3f(0.0, 0.0,  1.0);	// Point 2 (Left)
  glVertex3f(0.0,  1.0,  1.0);	// Point 3 (Left)
  glVertex3f(0.0,  1.0, 0.0);	// Point 4 (Left)
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

  // 회전 적용 
  glRotatef(fRotU, 1, 0, 0);
  glRotatef(fRotV, 0, 1, 0);

  case UpsideMenuBtn.Tag of
    0: // X축을 위로 뷰 맞추기
    begin
      glRotatef(90, 0, 0, 1);
      glRotatef(90, 1, 0, 0);
    end;

    2: // Z축을 위로 뷰 맞추기.
    begin
      glRotatef(-90, 1, 0, 0);
      glRotatef(-90, 0, 0, 1);
    end;
  end;

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
              //glScalef(0.5,0.5,0.5);
              //glTranslatef(1,1,1);
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


end;

procedure TFrmEdit3D.SetViewParams;
begin
  fRotU := 25;
  fRotV := -45;

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


procedure TFrmEdit3D.WndProc(var Message: TMessage);
begin
  inherited;
end;

procedure TFrmEdit3D.Idle(Sender: TObject);
begin
  //WriteLn('FrmEdit3D Idle, ', GetTickCount);
  wglMakeCurrent(fDC, fRC);
  //SetCurrent(fDC, fRC);

  //glClearColor(0.5, 0.5, 1.0, 1.0);
  glClearColor(140 / 255, 170 / 255, 235 / 255, 1.0);
  glClearDepth(1.0); // Depth Buffer Setup
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT); // Clear The Screen And The Depth Buffer

  RenderScene();

  SwapBuffers(fDC); // Display the scene
end;


procedure TFrmEdit3D.FormActivate(Sender: TObject);
begin
  FrmMain.OnActivate(sender);
end;

procedure TFrmEdit3D.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
  FrmEdit3D := nil;
end;

procedure TFrmEdit3D.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    Ord('1'): ViewButton.Down := true;
    Ord('2'): AddButton.Down := true;
    Ord('3'): DelButton.Down := true;
    Ord('4'): ReplaceButton.Down := true;
  end;
//
end;

procedure TFrmEdit3D.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  i: Integer;
begin
  try
  i := WheelDelta div ABS(WheelDelta);
  except
  i := 0;
  end;

  fFov := fFov + i;

  if fFov < 20 then fFov := 20;
  if fFov > 120 then fFov := 120;
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

  if ViewButton.Down and (ssLeft in Shift) then
  begin
    fRotU := fRotU - (ugMDownPos.Y - Y) / 6;
    if fRotU > 90 then fRotU := 90;
    if fRotU < -90 then fRotU := -90;

    fRotV := NormalizeDegAngle(fRotV - (ugMDownPos.X - X) / 6);
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

procedure TFrmEdit3D.UpsideMenuBtnClick(Sender: TObject);
var
  Pt: TPoint; 
begin
  Pt := Point(TSpeedButton(Sender).Left, TSpeedButton(Sender).Top + TSpeedButton(Sender).Height);
  Pt := TSpeedButton(Sender).ClientToScreen(Pt);

  UpsidePopup.Popup(Pt.X, Pt.Y);
//
end;


procedure TFrmEdit3D.UpsideMenuClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked := true;
  UpsideMenuBtn.Tag := TMenuItem(Sender).Tag; 
end;

end.
