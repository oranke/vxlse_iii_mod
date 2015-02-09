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
  Dialogs, ExtCtrls, Menus, Buttons, StdCtrls, 

  Voxel,
  Voxel_Engine,
  VectorUtil,

  OpenGL15;

type

  TSkinCell = record
    X, Y, Z: Byte; 
    Color: Byte;
  end;  

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
    RepaintButton: TSpeedButton;
    ResetViewButton: TSpeedButton;
    LinkXBtn: TSpeedButton;
    LinkYBtn: TSpeedButton;
    LinkZBtn: TSpeedButton;
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
    procedure ResetViewButtonClick(Sender: TObject);
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
    fLookAtPos: Voxel_Engine.TVector3f;

    //fOrgPanelWindowProc: TWndMethod;
    //procedure RenderPanelWindowProc(var Message: TMessage);

    fSkinCellCount: Integer;
    fSkinCells: array of TSkinCell;

    fHitIndex   : I32;
    fHitFlags   : TVector3sb;

    fMDownPos: TPoint;

    procedure InitViewParams;
    procedure RenderScene;
    procedure UnprojProc;

  protected
    //procedure WndProc(var Message: TMessage); override;
  public
    { Public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    procedure Update3dView(Vxl: TVoxelSection);

    procedure Idle(Sender: TObject);

  end;

var
  FrmEdit3D: TFrmEdit3D = nil;

implementation

{$R *.dfm}

uses
  OGLUtil,

  ogl3dview_engine,
  undo_engine,

  FormMain;



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
  glDepthFunc(GL_LEQUAL); // The Type Of Depth Test To Do

  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

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

  fSkinCellCount:= 0;
  fMDownPos := Point(0,0); 

  InitViewParams;
  Update3dView(ActiveSection);

  fHitIndex := -1; 

end;

destructor TFrmEdit3D.Destroy;
begin

  glDeleteLists(CubicDrawID, 1);

  CloseOpenGL(RenderPanel.Handle, fDC, fRC);

  inherited;
end;

procedure TFrmEdit3D.InitViewParams;
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


procedure TFrmEdit3D.RenderScene;
var
  i: Integer;
  VoxelColor: Voxel_Engine.TVector3f;
begin
  // Set the projection matrix
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  gluPerspective(fFov, RenderPanel.Width/RenderPanel.Height, fNear, fFar);

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


  for i := 0 to fSkinCellCount - 1 do
  begin
    glPushMatrix();

    glTranslatef(fSkinCells[i].X, fSkinCells[i].Y, fSkinCells[i].Z);
    VoxelColor := GetVXLColor(fSkinCells[i].Color, 0);
  
    glColor3f(
      VoxelColor.X,
      VoxelColor.Y,
      VoxelColor.Z
    );

    //glScalef(0.5, 0.5, 0.5);
    glCallList(CubicDrawID);

    if fHitIndex = i then
    begin
      //glBlendFunc(GL_SRC_ALPHA, GL_ONE);
      //glBlendFunc(GL_ONE_MINUS_DST_COLOR, GL_ONE_MINUS_SRC_ALPHA);
      //glDepthFunc(GL_EQUAL);
      glColor4f(1, 0, 0, 0.5);
      //glColor4f(1, 1, 1, 0.5);
      //glColor4f(0, 0, 0, 0.5);
      //glPolygonOffset(1, 0.5);
      glCallList(CubicDrawID);
      //glPolygonOffset(0, 0);
      //glDepthFunc(GL_LEQUAL);
      //glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    end;

    glPopMatrix();
  end;

  {
  if fHitIndex > 0 then
  with fSkinCells[fHitIndex] do
  begin
  
  end;
  }


end;



procedure TFrmEdit3D.UnprojProc;
var
  ViewMat, ProjMat, VInvMat: VectorUtil.TMatrix4f;
  EyePos, EyeDir: VectorUtil.TVector3f;


  f, t: F32;
  HitPos, tmpHitPos: VectorUtil.TVector3f; 

  i: Integer;
begin
  // 뷰 행렬 얻고 역행렬 계산.
  glGetFloatv(GL_MODELVIEW_MATRIX, @ViewMat);
  VInvMat := InvertMatrixNoScale(ViewMat);

  // 프로젝션 행렬 얻기
  glGetFloatv(GL_PROJECTION_MATRIX, @ProjMat);

  // 역행렬의 이동성분은 관찰점.
  EyePos := TZhMatrix(VInvMat).vT;

  // 언프로젝션.
  EyeDir[0] := ((((fMDownPos.X*2.0/RenderPanel.Width)-1.0)) - ProjMat[2,0]) / ProjMat[0,0];
  EyeDir[1] := ((-((fMDownPos.Y*2.0/RenderPanel.Height)-1.0)) - ProjMat[2,1]) / ProjMat[1,1];
  EyeDir[2] := -1;
  // 세계공간으로 회전시키고 노멀라이즈.
  EyeDir := VectorNormalize3f(RotateVector(EyeDir, VInvMat));

  fHitIndex := -1;

  t := C_BigValue;
  for i := 0 to fSkinCellCount - 1 do
  with fSkinCells[i] do
  if CC_Ray_AABB(EyePos, EyeDir, MakeVector3f(X, Y, Z), MakeVector3f(X+1, Y+1, Z+1), tmpHitPos) then
  begin
    f := VectorNorm3f(VectorSubtract3f(tmpHitPos, EyePos));
    if f < t then
    begin
      t := f;
      fHitIndex := i;
      HitPos   := tmpHitPos; 
    end;
  end;

  if fHitIndex > 0 then
  with fSkinCells[fHitIndex] do
  begin
    fHitFlags[C_X] := Trunc((HitPos[C_X]-X-0.5)*2);
    fHitFlags[C_Y] := Trunc((HitPos[C_Y]-Y-0.5)*2);
    fHitFlags[C_Z] := Trunc((HitPos[C_Z]-Z-0.5)*2);
    {
    Caption := 'Hit : ' + Format('%d %d %d : %d %d %d',
      [
        X, Y, Z,
        fHitFlags[C_X],
        fHitFlags[C_Y],
        fHitFlags[C_Z]
      ]);
     }
  end else
  begin
    //Caption := 'NoHit';
  end;
end;


procedure TFrmEdit3D.Update3dView(Vxl: TVoxelSection);
var
  ix, iy, iz: Integer;
  v: TVoxelUnpacked;
begin
  fSkinCellCount:= 0;
  SetLength(fSkinCells, 0);

  with Vxl do
  for iz := 0 to Tailer.ZSize - 1 do
  for iy := 0 to Tailer.YSize - 1 do
  for ix := 0 to Tailer.XSize - 1 do
  begin
    GetVoxel(ix, iy, iz, v);
    if v.Used then
    if CheckFace(Vxl, ix, iy + 1, iz) or
      CheckFace(Vxl, ix, iy - 1, iz) or
      CheckFace(Vxl, ix, iy, iz + 1) or
      CheckFace(Vxl, ix, iy, iz - 1) or
      CheckFace(Vxl, ix - 1, iy, iz) or
      CheckFace(Vxl, ix + 1, iy, iz) then
    begin
      SetLength(fSkinCells, fSkinCellCount+1);

      fSkinCells[fSkinCellCount].X := ix;
      fSkinCells[fSkinCellCount].Y := iy;
      fSkinCells[fSkinCellCount].Z := iz;
      fSkinCells[fSkinCellCount].Color := v.Colour; 

      Inc(fSkinCellCount);
    end;
  end;
end;

{
procedure TFrmEdit3D.WndProc(var Message: TMessage);
begin
  inherited;
end;
}

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
    Ord('4'): RepaintButton.Down := true;
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

  if fFov < 5 then fFov := 5;
  if fFov > 120 then fFov := 120;
end;


procedure TFrmEdit3D.RenderPaintMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  v: TVoxelUnpacked;
  nx, ny, nz: I32;

  procedure ProcSetVoxel(const ax, ay, az: I32; const aCheckUsedValue: Boolean);
  var
    i: Integer;
    rv: TVoxelUnpacked;  
  begin
    ActiveSection.SetVoxel(ax, ay, az, v);

    if LinkXBtn.Down then
    for i := 0 to ActiveSection.Tailer.XSize - 1 do
    begin
      ActiveSection.GetVoxel(i, ay, az, rv);
      if rv.Used = aCheckUsedValue then
        ActiveSection.SetVoxel(i, ay, az, v);
    end; 

    if LinkYBtn.Down then
    for i := 0 to ActiveSection.Tailer.YSize - 1 do
    begin
      ActiveSection.GetVoxel(ax, i, az, rv);
      if rv.Used = aCheckUsedValue then
        ActiveSection.SetVoxel(ax, i, az, v);
    end;

    if LinkZBtn.Down then
    for i := 0 to ActiveSection.Tailer.ZSize - 1 do
    begin
      ActiveSection.GetVoxel(ax, ay, i, rv);
      if rv.Used = aCheckUsedValue then
        ActiveSection.SetVoxel(ax, ay, i, v);
    end;
  end;

begin
  fMDownPos := Point(X, Y);

  if (ssLeft in Shift) then
  begin
    if ViewButton.Down then
    if ssCtrl in Shift then
    begin
      UnprojProc;
      if fHitIndex > 0 then
      with fSkinCells[fHitIndex] do
      begin
        ActiveSection.GetVoxel(X, Y, Z, v);
        FrmMain.SetActiveColor(v.Colour, SpectrumMode = ModeColours);
        FrmMain.SetActiveNormal(v.Normal, SpectrumMode = ModeColours);

        fHitIndex := -1; 
      end;
    end;

    if AddButton.Down then
    if fHitIndex > 0 then
    with fSkinCells[fHitIndex] do
    begin
      nx := X + fHitFlags[C_X];
      ny := Y + fHitFlags[C_Y];
      nz := Z + fHitFlags[C_Z];
      //caption := 'Add ' + Format('%d %d %d , %d %d %d', [x, y, z, nx, ny, nz]);
      with ActiveSection.Tailer do
      if (nx >= 0) and (nx < XSize) and
         (ny >= 0) and (ny < YSize) and
         (nz >= 0) and (nz < ZSize) then
      begin
        CreateVXLRestorePoint(ActiveSection, Undo); // Save Undo

        v.Used := true;
        v.Normal := ActiveNormal;
        v.Colour := ActiveColour;
        //ActiveSection.SetVoxel(nx, ny, nz, v);
        ProcSetVoxel(nx, ny, nz, false);

        FrmMain.RefreshAll;
        FrmMain.UpdateUndo_RedoState;

        fHitIndex := -1; //UnprojProc;
      end;
    end;

    if DelButton.Down then
    if fHitIndex > 0 then
    with fSkinCells[fHitIndex] do
    begin
      CreateVXLRestorePoint(ActiveSection, Undo); // Save Undo

      v.Used := false;
      ProcSetVoxel(X, Y, Z, true);

      FrmMain.RefreshAll;
      FrmMain.UpdateUndo_RedoState;

      fHitIndex := -1; //UnprojProc;
    end;

    if RepaintButton.Down then
    if fHitIndex > 0 then
    with fSkinCells[fHitIndex] do
    begin
      ActiveSection.GetVoxel(X, Y, Z, v);
      if (ssCtrl in Shift) or (v.Colour <> ActiveColour) then
      begin
        CreateVXLRestorePoint(ActiveSection, Undo); // Save Undo

        v.Colour := ActiveColour;
        ProcSetVoxel(X, Y, Z, true);

        FrmMain.RefreshAll;
        FrmMain.UpdateUndo_RedoState;
      end;
    end;




  end; 
end;


procedure TFrmEdit3D.RenderPaintMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  ViewMat: VectorUtil.TMatrix4f;
  DirU, DirV: VectorUtil.TVector3f;
begin

  if ( ViewButton.Down and (ssLeft in Shift) ) or
    ( ssRight in Shift ) then
  begin
    fRotU := fRotU - (fMDownPos.Y - Y) / 6;
    if fRotU > 90 then fRotU := 90;
    if fRotU < -90 then fRotU := -90;

    fRotV := NormalizeDegAngle(fRotV - (fMDownPos.X - X) / 6);
  end;

  if (ssMiddle in Shift) then
  begin
    glGetFloatv(GL_MODELVIEW_MATRIX, @ViewMat);

    DirU := MakeVector3f( ViewMat[c_X,c_X], ViewMat[c_Y,c_X], ViewMat[c_Z,c_X]);
    DirV := MakeVector3f(-ViewMat[c_X,c_Y],-ViewMat[c_Y,c_Y],-ViewMat[c_Z,c_Y]);
{
  Result[c_X,c_X]:= U[c_X]; Result[c_X,c_Y]:= V[c_X]; Result[c_X,c_Z]:= N[c_X]; Result[c_X,c_W]:= 0;
  Result[c_Y,c_X]:= U[c_Y]; Result[c_Y,c_Y]:= V[c_Y]; Result[c_Y,c_Z]:= N[c_Y]; Result[c_Y,c_W]:= 0;
  Result[c_Z,c_X]:= U[c_Z]; Result[c_Z,c_Y]:= V[c_Z]; Result[c_Z,c_Z]:= N[c_Z]; Result[c_Z,c_W]:= 0;

}

    fLookAtPos :=
      Voxel_Engine.TVector3f(
        VectorAdd3f(
          VectorUtil.TVector3f(fLookAtPos),
          VectorAdd3f(
            VectorScale3f(DirU, (fMDownPos.X-X) * 1.0),
            VectorScale3f(DirV, (fMDownPos.Y-Y) * 1.0)
          )
        )
      );


  end;

  fMDownPos := Point(X, Y);
  if not ViewButton.Down then UnprojProc();
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

procedure TFrmEdit3D.ResetViewButtonClick(Sender: TObject);
begin
  InitViewParams;
//
end;


end.
