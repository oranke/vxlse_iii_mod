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
    SpeedButton1: TSpeedButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
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
    procedure SpeedButton1Click(Sender: TObject);
  private
    { Private declarations }
    fDC: HDC; // Device Context
    fRC: HGLRC; // Rendering Context

    CubicDrawID : GLuint;
    MonotoneDrawID : GLuint;

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

  //FormStyle := fsNormal;

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
  //glPolygonMode( GL_FRONT_AND_BACK, GL_LINE );

  
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

  MonotoneDrawID :=  glGenLists(1);

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
  glDeleteLists(MonotoneDrawID, 1);

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


  glEnable(GL_DEPTH_TEST);

  if CheckBox2.Checked then
    glPolygonMode( GL_FRONT_AND_BACK, GL_LINE )
  else
    glPolygonMode( GL_FRONT_AND_BACK, GL_FILL );

  if CheckBox1.Checked then
  begin
    glCallList(MonotoneDrawID);
  end else
  begin
    glEnable(GL_LIGHT0);
    glEnable(GL_LIGHTING);

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

// 복셀 -> Monotone 테스트.
// http://0fps.net/2012/07/07/meshing-minecraft-part-2/
// https://github.com/mikolalysenko/mikolalysenko.github.com/blob/master/MinecraftMeshes2/js/monotone.js

type
  TMeshSide = array[0..1] of I32;

  TMonotoneMesh = class
    Color: I32;
    Left: array of TMeshSide;
    Right : array of TMeshSide;

    constructor Create(c, v, ul, ur: I32);
    procedure close_off(v: I32);
    procedure merge_run(v, u_l, u_r: I32);
  end;

{ TMonotoneMesh }

constructor TMonotoneMesh.Create(c, v, ul, ur: I32);
begin
  Color := c;
  SetLength(Left, 1);
  Left[0][0] := ul;
  Left[0][1] := v;

  SetLength(Right, 1);
  Right[0][0] := ur;
  Right[0][1] := v;
end;

procedure TMonotoneMesh.close_off(v: I32);
var
  cv: I32;
begin
  //lmsd := Left[Length(Left)-1];
  //rmsd := Right[Length(Right)-1];

  cv := Left[Length(Left)-1][0];
  SetLength(Left, Length(Left)+1);
  Left[Length(Left)-1][0] := cv;
  Left[Length(Left)-1][1] := v;

  cv := Right[Length(Right)-1][0];
  SetLength(Right, Length(Right)+1);
  Right[Length(Right)-1][0] := cv;
  Right[Length(Right)-1][1] := v;

end;

procedure TMonotoneMesh.merge_run(v, u_l, u_r: I32);
var
  cv: I32;
begin
  cv := Left[Length(Left)-1][0];
  if cv <> u_l then
  begin
    SetLength(Left, Length(Left)+2);
    Left[Length(Left)-2][0] := cv;
    Left[Length(Left)-2][1] := v;

    Left[Length(Left)-1][0] := u_l;
    Left[Length(Left)-1][1] := v;
  end;

  cv := Right[Length(Right)-1][0];
  if cv <> u_r then
  begin
    SetLength(Right, Length(Right)+2);
    Right[Length(Right)-2][0] := cv;
    Right[Length(Right)-2][1] := v;

    Right[Length(Right)-1][0] := u_r;
    Right[Length(Right)-1][1] := v;
  end;
end;

type
  TPolygons = class (TList)
  private
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  end;


{ TPolygons }

procedure TPolygons.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited;
  if Action = lnDeleted then TObject(Ptr).Free;
end;

type
  TRecords = class (TList)
  private
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  end;

{ TRecords }

procedure TRecords.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited;
  if Action = lnDeleted then Dispose(Ptr); 
end;


procedure TFrmEdit3D.SpeedButton1Click(Sender: TObject);
var
  dims: array [0..2] of U8;

  d,
  i, j, k: I32;
  u, v: I32;
  x, q: array [0..2] of I32;
  runs,
  frontier,
  next_frontier,
  left_index,
  right_index,
  stack,
  tmp: array of I32;
  delta: array [0..1, 0..1] of I32;

  n, nf: I32;
  nr, p, c: I32;

  a, b: I32;
  fp: I32;

  vxl: TVoxelUnpacked;

  Polygons: TPolygons;

  mp, np: TMonotoneMesh;
  p_l, p_r, p_c, r_l, r_r, r_c: I32;
  flipped: Boolean;

  //y: VectorUtil.TVector3f;
  z: TMeshSide; 

  yp: VectorUtil.PVector3f;
  Vertices: TRecords;
  Faces: TRecords; 

  bottom, top, l_i, r_i: I32;
  side: Boolean;

  n_side: Boolean;
  l, r: TMeshSide;
  idx: I32;
  vert: TMeshSide;
  det: I32;
  facep: VectorUtil.PVector4i;

  VoxelColor: Voxel_Engine.TVector3f;
begin
  AllocConsole;

  Polygons:= TPolygons.Create;
  Vertices:= TRecords.Create;
  Faces:= TRecords.Create; ; 

  with ActiveSection.Tailer do
  begin
    dims[0] := XSize;
    dims[1] := YSize;
    dims[2] := ZSize;
  end;


  d := 0;
  while d < 3 do
  begin
    u := (d+1) mod 3;  //u and v are orthogonal directions to d
    v := (d+2) mod 3;

    FillChar(x, SizeOf(I32)*3, #0);
    FillChar(q, SizeOf(I32)*3, #0);

    SetLength(runs, 2 * (dims[u]+1) );
    //FillChar(runs, 2 * (dims[u]+1) * SizeOf(I32), #0);
    SetLength(frontier, dims[u]);
    //FillChar(frontier, dims[u] * SizeOf(I32), #0);
    SetLength(next_frontier, dims[u]);
    //FillChar(next_frontier, dims[u] * sizeOf(I32), #0);
    SetLength(left_index, 2 * dims[v]);
    //FillChar(left_index, 2 * dims[v] * SizeOf(I32), #0);
    SetLength(right_index, 2 * dims[v]);
    //FillChar(right_index, 2 * dims[v] * SizeOf(I32), #0);
    SetLength(stack, 24 * dims[v]);
    //FillChar(stack, 24 * dims[v] * SizeOf(I32), #0);

    //FillChar(delta, SizeOf(I32) * 2 * 2, #0);

    //q points along d-direction
    q[d] := 1;

    //Initialize sentinel
    x[d] := -1;
    while x[d] < dims[d] do
    begin
      // --- Perform monotone polygon subdivision ---
      n := 0;
      nf := 0;

      Polygons.clear;

      x[v] := 0;
      while x[v] < dims[v] do
      begin
        //Make one pass over the u-scan line of the volume to run-length encode polygon
        nr := 0;
        // js 예제에서는 0이 빈 곳을 나타냄. 여기서는 사용되지 않는 복셀은 -1로.
        // -->> 0 그대로 쓰고 칼라값에 1을 더해 쓰자.
        p := 0; //c := 0;

        x[u] := 0;
        while x[u] < dims[u] do
        begin
          //p := c;
          
          //Compute the type for this face
          a := 0;
          if 0 <= x[d] then
          begin
            ActiveSection.GetVoxel(x[0], x[1], x[2], vxl);
            if vxl.Used then
              a := vxl.Colour +1
          end;

          b := 0;
          if x[d] < dims[d]-1 then
          begin
            ActiveSection.GetVoxel(x[0]+q[0], x[1]+q[1], x[2]+q[2], vxl);
            if vxl.Used then
              b := vxl.Colour +1
          end;

          c := a;
          //!! Check!
          if Boolean(a) = Boolean(b) then
            c := 0
          else if not Boolean(a) then
            c := -b;

          //If cell type doesn't match, start a new run
          if p <> c then
          begin
            runs[nr] := x[u];
            inc(nr);
            runs[nr] := c;
            inc(nr);
          end;

          Inc(x[u]);

          p := c;
        end;
        //p := c;

        //Add sentinel run
        runs[nr] := dims[u];
        inc(nr);
        runs[nr] := 0;
        inc(nr);
        
        //Update frontier by merging runs
        fp := 0;
        i :=0; j := 0;
        while (i < nf) and (j < nr-2) do
        begin
          if (frontier[i] >= 0) and (frontier[i] < Polygons.Count) then
          begin
            mp := Polygons[frontier[i]];
            p_l := mp.Left[Length(mp.Left)-1][0];
            p_r := mp.Right[Length(mp.Right)-1][0];
            p_c := mp.Color; 
          end else
          begin
            mp := nil;
            p_l := 0;
            p_r := 0;
            p_c := 0;
          end;

          r_l := runs[j];   // Start of run
          r_r := runs[j+2]; // End of run
          r_c := runs[j+1]; // Color of run

          //Check if we can merge run with polygon
          if (r_r > p_l) and (p_r > r_l) and (r_c = p_c) then
          begin
            //Merge run
            if Assigned(mp) then
              mp.merge_run(x[v], r_l, r_r);
              
            //Insert polygon into frontier
            next_frontier[fp] := frontier[i];
            Inc(fp);

            Inc(i);
            Inc(j, 2);
          end else
          begin
            //Check if we need to advance the run pointer
            if (r_r <= p_r) then
            begin
              //if r_c <> 0 then //(!!r_c) {
              if Boolean(r_c) then
              begin
                np := TMonotoneMesh.Create(r_c, x[v], r_l, r_r);
                //WriteLn('MonotoneMesh Created!');
                next_frontier[fp] := Polygons.Count;
                Inc(fp);
                Polygons.Add(np);
              end;
              Inc(j, 2);
            end;

            //Check if we need to advance the frontier pointer
            if(p_r <= r_r) then
            begin
              if Assigned(mp) then
                mp.close_off(x[v]);

              Inc(i);
              //WriteLn('Inc i-2. ', i);
            end;
          end;

          //WriteLn(i, ', ', j);
        end;

        //Close off any residual polygons
        while i < nf do
        begin
          TMonotoneMesh(Polygons[frontier[i]]).close_off(x[v]);
          Inc(i);
        end;

        //Add any extra runs to frontier
        while j<nr-2 do
        begin
          r_l  := runs[j];
          r_r  := runs[j+2];
          r_c  := runs[j+1];

          //if r_c <> 0 then //(!!r_c) {
          if Boolean(r_c) then
          begin
            np := TMonotoneMesh.Create(r_c, x[v], r_l, r_r);
            //WriteLn('MonotoneMesh Created2!');
            next_frontier[fp] := Polygons.Count;
            Inc(fp);
            Polygons.Add(np);
          end;
          Inc(j, 2);
        end;

        //Swap frontiers
        tmp := next_frontier;
        next_frontier := frontier;
        frontier := tmp;
        nf := fp;

        inc(x[v]);
      end;

      //Close off frontier
      for i := 0 to nf-1 do
        TMonotoneMesh(Polygons[frontier[i]]).close_off(dims[v]);

      //WriteLn('Polygons ', Polygons.Count, ', ', x[d], ', ', dims[d]);

      // --- Monotone subdivision of polygon is complete at this point ---

      Inc(x[d]);

      //Now we just need to triangulate each monotone polygon
      for i := 0 to Polygons.Count - 1 do
      begin
        mp := Polygons[i];
        c := mp.Color;
        flipped := false;

        if c < 0 then
        begin
          flipped := true;
          c := -c;
        end;

        for j := 0 to Length(mp.Left) - 1 do
        begin
          left_index[j] := Vertices.Count;

          new(yp);
          //yp^ := MakeVector3f(0.0, 0.0, 0.0);
          z := mp.Left[j];

          yp^[d] := x[d];
          yp^[u] := z[0];
          yp^[v] := z[1];

          Vertices.Add(yp);
        end;

        for j := 0 to Length(mp.Right) - 1 do
        begin
          right_index[j] := Vertices.Count;

          new(yp);
          //yp^ := MakeVector3f(0.0, 0.0, 0.0);
          z := mp.Right[j];

          yp^[d] := x[d];
          yp^[u] := z[0];
          yp^[v] := z[1];

          Vertices.Add(yp);
        end;

        //Triangulate the monotone polygon
        bottom := 0;
        top := 0;
        l_i := 1;
        r_i := 1;
        side := true;  //true = right, false = left

        stack[top] := left_index[0];
        Inc(top);
        stack[top] := mp.left[0][0];
        Inc(top);
        stack[top] := mp.left[0][1];
        Inc(top);

        stack[top] := right_index[0];
        Inc(top);
        stack[top] := mp.right[0][0];
        Inc(top);
        stack[top] := mp.right[0][1];
        Inc(top);

        while (l_i < Length(mp.left)) or (r_i < Length(mp.right)) do
        begin
          n_side := false;

          if (l_i = Length(mp.left)) then
            n_side := true
          else if (r_i <> Length(mp.Right)) then
          begin
            l := mp.left[l_i];
            r := mp.right[r_i];
            n_side := l[1] > r[1];
          end;

          if n_side then
          begin
            idx := right_index[r_i];
            vert := mp.Right[r_i];
          end else
          begin
            idx := left_index[l_i];
            vert := mp.Left[l_i];
          end;

          if (n_side <> side) then
          begin
            //Opposite side
            while bottom+3 < top do
            begin
              New(facep);

              if(flipped = n_side) then
              begin
                facep^[0] := stack[bottom];
                facep^[1] := stack[bottom+3];
                facep^[2] := idx;
                facep^[3] := c-1;
                //faces.push([ stack[bottom], stack[bottom+3], idx, c]);
              end else
              begin
                facep^[0] := stack[bottom+3];
                facep^[1] := stack[bottom];
                facep^[2] := idx;
                facep^[3] := c-1;
                //faces.push([ stack[bottom+3], stack[bottom], idx, c]);
              end;

              Faces.Add(facep);

              Inc(bottom, 3);
            end;

          end else
          begin
            //Same side
            while bottom+3 < top do
            begin
              for j := 0 to 2 - 1 do
              for k := 0 to 2 - 1 do
                delta[j][k] := stack[top-3*(j+1)+k+1] - vert[k];

              det := delta[0][0] * delta[1][1] - delta[1][0] * delta[0][1];
              if n_side = (det > 0) then Break;

              if (det <> 0) then
              begin
                New(facep);

                if (flipped = n_side) then
                begin
                  facep^[0] := stack[top-3];
                  facep^[1] := stack[top-6];
                  facep^[2] := idx;
                  facep^[3] := c-1;
                  //faces.push([ stack[top-3], stack[top-6], idx, c ]);
                end else
                begin
                  facep^[0] := stack[top-6];
                  facep^[1] := stack[top-3];
                  facep^[2] := idx;
                  facep^[3] := c-1;
                  //faces.push([ stack[top-6], stack[top-3], idx, c ]);
                end;
                
                Faces.Add(facep);
              end;

              Dec(top, 3);
            end;
          end;

          //Push vertex
          stack[top] := idx;
          Inc(top);
          stack[top] := vert[0];
          Inc(top);
          stack[top] := vert[1];
          Inc(top);

          //Update loop index
          if n_side then
            Inc(r_i)
          else
            inc(l_i);

          side := n_side; 
        end;
      end;
    end;


    Inc(d);
  end;


  WriteLn('SkinCells ', fSkinCellCount, ' -> ', fSkinCellCount*6*2, ' faces'); 
  WriteLn('Vertices ', Vertices.Count);
  WriteLn('Faces ', Faces.Count);
  //{
  for i := 0 to Faces.Count - 1 do
  begin
    WriteLn(
      Format(
        '%d: %d %d %d. %d',
        [
          i,
          VectorUtil.PVector4i(Faces[i])^[0],
          VectorUtil.PVector4i(Faces[i])^[1],
          VectorUtil.PVector4i(Faces[i])^[2],
          VectorUtil.PVector4i(Faces[i])^[3]
        ]
      )
    );
  end;
  {}

  glNewList(MonotoneDrawID, GL_COMPILE);
  glBegin(GL_TRIANGLES);

  for i := 0 to Faces.Count - 1 do
  begin
    VoxelColor := GetVXLColor(VectorUtil.PVector4i(Faces[i])^[3], 0);

    glColor3f(
      VoxelColor.X,
      VoxelColor.Y,
      VoxelColor.Z
    );

    for j := 0 to 3 - 1 do
    begin
      yp := Vertices[VectorUtil.PVector4i(Faces[i])^[j]];
      glVertex3f(yp^[C_X], yp^[C_Y], yp^[C_Z]);
    end;

  end;

  glEnd();
  glEndList();

  Polygons.Free;
  Vertices.Free;
  Faces.Free;
//
end;


end.
