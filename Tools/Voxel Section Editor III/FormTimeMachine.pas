unit FormTimeMachine;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,Voxel_Engine, Menus, ExtCtrls, StdCtrls, Voxel, ComCtrls, ToolWin,
  ImgList, Math, palette, Spin, Buttons, ogl3dview_engine,OpenGL15,FTGifAnimate,
  undo_engine,ShellAPI,Constants,cls_Config,pause,FormNewVxlUnit,mouse,Registry,
  Form3dpreview,Debug, FormAutoNormals;

{$INCLUDE Global_Conditionals.inc}

Const
   APPLICATION_TITLE = 'Will VXL Editor';
   APPLICATION_VER = 'Beta 5';

type
  TFrmTimeMain = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    OpenVXLDialog: TOpenDialog;
    Panel1: TPanel;
    LeftPanel: TPanel;
    RightPanel: TPanel;
    MainPaintPanel: TPanel;
    CnvView2: TPaintBox;
    CnvView1: TPaintBox;
    lblView1: TLabel;
    lblView2: TLabel;
    lblView0: TLabel;
    View1: TMenuItem;
    lblSection: TLabel;
    Full1: TMenuItem;
    CrossSection1: TMenuItem;
    EmphasiseDepth1: TMenuItem;
    ImageList1: TImageList;
    Colours1: TMenuItem;
    Normals1: TMenuItem;
    Panel3: TPanel;
    SectionCombo: TComboBox;
    lblTools: TLabel;
    Panel4: TPanel;
    lblpalette: TLabel;
    cnvPalette: TPaintBox;
    pnlPalette: TPanel;
    lblActiveColour: TLabel;
    pnlActiveColour: TPanel;
    ScrollBar2: TScrollBar;
    Panel5: TPanel;
    ScrollBar1: TScrollBar;
    Panel6: TPanel;
    lbl3dview: TLabel;
    OGL3DPreview: TPanel;
    Panel7: TPanel;
    SpeedButton2: TSpeedButton;
    btn3DRotateX2: TSpeedButton;
    btn3DRotateY2: TSpeedButton;
    btn3DRotateY: TSpeedButton;
    Bevel1: TBevel;
    SpeedButton1: TSpeedButton;
    btn3DRotateX: TSpeedButton;
    spin3Djmp: TSpinEdit;
    ColorDialog: TColorDialog;
    Popup3d: TPopupMenu;
    Views1: TMenuItem;
    Front1: TMenuItem;
    Back1: TMenuItem;
    MenuItem1: TMenuItem;
    LEft1: TMenuItem;
    Right1: TMenuItem;
    MenuItem2: TMenuItem;
    Bottom1: TMenuItem;
    op1: TMenuItem;
    N3: TMenuItem;
    Cameo1: TMenuItem;
    Cameo21: TMenuItem;
    Cameo31: TMenuItem;
    Cameo41: TMenuItem;
    Options2: TMenuItem;
    DebugMode1: TMenuItem;
    NormalsTest1: TMenuItem;
    RemapColour1: TMenuItem;
    Gold1: TMenuItem;
    Red1: TMenuItem;
    Orange1: TMenuItem;
    Magenta1: TMenuItem;
    Purple1: TMenuItem;
    Blue1: TMenuItem;
    Green1: TMenuItem;
    DarkSky1: TMenuItem;
    White1: TMenuItem;
    NormalsTest2: TMenuItem;
    N4: TMenuItem;
    BackgroundColour1: TMenuItem;
    extColour1: TMenuItem;
    N5: TMenuItem;
    Normals2: TMenuItem;
    lblLayer: TLabel;
    pnlLayer: TPanel;
    XCursorBar: TTrackBar;
    Label2: TLabel;
    Label3: TLabel;
    YCursorBar: TTrackBar;
    Label4: TLabel;
    ZCursorBar: TTrackBar;
    StatusBar1: TStatusBar;
    mnuDirectionPopup: TPopupMenu;
    mnuEdit: TMenuItem;
    mnuDirTowards: TMenuItem;
    mnuDirAway: TMenuItem;
    mnuCancel1: TMenuItem;
    lblBrush: TLabel;
    Panel8: TPanel;
    Brush_5: TSpeedButton;
    Brush_4: TSpeedButton;
    Brush_3: TSpeedButton;
    Brush_2: TSpeedButton;
    Brush_1: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton9: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    SpeedButton10: TSpeedButton;
    SpeedButton11: TSpeedButton;
    NewProject1: TMenuItem;
    N1: TMenuItem;
    SaveAs1: TMenuItem;
    N8: TMenuItem;
    Exit1: TMenuItem;
    VoxelHeader1: TMenuItem;
    SaveVXLDialog: TSaveDialog;
    ClearEntireSection1: TMenuItem;
    ools2: TMenuItem;
    Edit1: TMenuItem;
    Undo1: TMenuItem;
    N11: TMenuItem;
    CnvView0: TPaintBox;
    Help1: TMenuItem;
    VXLSEHelp1: TMenuItem;
    N14: TMenuItem;
    About1: TMenuItem;
    Nudge1: TMenuItem;
    FlipXswitchFrontBack1: TMenuItem;
    FlipYswitchRightLeft1: TMenuItem;
    MirrorBottomToTop1: TMenuItem;
    MirrorLeftToRight1: TMenuItem;
    Nudge1Left1: TMenuItem;
    Nudge1Right1: TMenuItem;
    Nudge1up1: TMenuItem;
    Nudge1Down1: TMenuItem;
    Palette1: TMenuItem;
    iberianSunPalette1: TMenuItem;
    RedAlert2Palette1: TMenuItem;
    Custom1: TMenuItem;
    N18: TMenuItem;
    blank1: TMenuItem;
    SpeedButton12: TSpeedButton;
    IconList: TImageList;
    N22: TMenuItem;
    OpenDialog1: TOpenDialog;
    SmoothNormals1: TMenuItem;
    Disable3dView1: TMenuItem;
    SpinButton3: TSpinButton;
    SpinButton1: TSpinButton;
    SpinButton2: TSpinButton;
    TopBarImageHolder: TImage;
    MainViewPopup: TPopupMenu;
    Magnification1: TMenuItem;
    N1x1: TMenuItem;
    N3x1: TMenuItem;
    N5x1: TMenuItem;
    N7x1: TMenuItem;
    N9x1: TMenuItem;
    N10x1: TMenuItem;
    N131: TMenuItem;
    N15x1: TMenuItem;
    N17x1: TMenuItem;
    N19x1: TMenuItem;
    N21x1: TMenuItem;
    N23x1: TMenuItem;
    N25x1: TMenuItem;
    CubedAutoNormals1: TMenuItem;
    SpeedButton13: TSpeedButton;
    MirrorFrontToBack2: TMenuItem;
    EnableUndo1: TMenuItem;
    N6: TMenuItem;
    N7: TMenuItem;
    ShowAimLines1: TMenuItem;
    Scale3DView1: TMenuItem;
    N2: TMenuItem;
    N12: TMenuItem;
    N13: TMenuItem;
    N19: TMenuItem;
    LoadPalette1: TMenuItem;
    procedure NewAutoNormals1Click(Sender: TObject);
    procedure Display3DWindow1Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure RevoraCCForums1Click(Sender: TObject);
    procedure RenegadeProjects1Click(Sender: TObject);
    procedure RockTheBattlefield1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure CnvView0Paint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure changecaption(Filename : boolean; FName : string);
    procedure CnvView1Paint(Sender: TObject);
    procedure CnvView2Paint(Sender: TObject);
    Procedure SetIsEditable(Value : boolean);
    Procedure RefreshAll;
    Procedure SetupSections;
    procedure SectionComboChange(Sender: TObject);
    procedure Full1Click(Sender: TObject);
    procedure CrossSection1Click(Sender: TObject);
    procedure EmphasiseDepth1Click(Sender: TObject);
    Procedure SetViewMode(VM : EViewMode);
    Procedure SetSpectrum(SP : ESpectrumMode);
    procedure Colours1Click(Sender: TObject);
    procedure Normals1Click(Sender: TObject);
    procedure cnvPalettePaint(Sender: TObject);
    procedure SetActiveColor(Value : integer; CN : boolean);
    procedure SetActiveNormal(Value : integer; CN : boolean);
    Procedure SetActiveCN(Value : integer);
    procedure ScrollBar1Change(Sender: TObject);
    procedure setupscrollbars;
    procedure FormShow(Sender: TObject);
    procedure cnvPaletteMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormDestroy(Sender: TObject);
    procedure OGL3DPreviewMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure OGL3DPreviewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure OGL3DPreviewMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SpeedButton1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure NormalsTest1Click(Sender: TObject);
    procedure DebugMode1Click(Sender: TObject);
    procedure NormalsTest2Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure btn3DRotateXClick(Sender: TObject);
    Procedure SetRotationAdders;
    procedure btn3DRotateX2Click(Sender: TObject);
    procedure btn3DRotateY2Click(Sender: TObject);
    procedure btn3DRotateYClick(Sender: TObject);
    procedure spin3DjmpChange(Sender: TObject);
    procedure BackgroundColour1Click(Sender: TObject);
    procedure extColour1Click(Sender: TObject);
    procedure ClearRemapClicks;
    procedure Gold1Click(Sender: TObject);
    procedure Red1Click(Sender: TObject);
    procedure Orange1Click(Sender: TObject);
    procedure Magenta1Click(Sender: TObject);
    procedure Purple1Click(Sender: TObject);
    procedure Blue1Click(Sender: TObject);
    procedure Green1Click(Sender: TObject);
    procedure DarkSky1Click(Sender: TObject);
    procedure White1Click(Sender: TObject);
    procedure Front1Click(Sender: TObject);
    procedure Back1Click(Sender: TObject);
    procedure LEft1Click(Sender: TObject);
    procedure Right1Click(Sender: TObject);
    procedure Bottom1Click(Sender: TObject);
    procedure op1Click(Sender: TObject);
    procedure Cameo1Click(Sender: TObject);
    procedure Cameo21Click(Sender: TObject);
    procedure Cameo31Click(Sender: TObject);
    procedure Cameo41Click(Sender: TObject);
    procedure CnvView2MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CnvView1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CnvView1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CnvView1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure CnvView2MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure CnvView2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CnvView0MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure UpdateCursor(P : TVector3i; Repaint : Boolean);
    procedure XCursorBarChange(Sender: TObject);
    Procedure CursorReset;
    Procedure CursorResetNoMAX;
    Procedure SetupStatusBar;
    procedure CnvView0MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure lblView1Click(Sender: TObject);
    procedure lblView2Click(Sender: TObject);
    procedure mnuDirectionPopupPopup(Sender: TObject);
    procedure mnuEditClick(Sender: TObject);
    procedure mnuDirTowardsClick(Sender: TObject);
    procedure mnuDirAwayClick(Sender: TObject);
    procedure DoAfterLoadingThings;
    procedure Brush_1Click(Sender: TObject);
    procedure Brush_2Click(Sender: TObject);
    procedure Brush_3Click(Sender: TObject);
    procedure Brush_4Click(Sender: TObject);
    procedure Brush_5Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    Procedure SetVXLTool(VXLTool_ : Integer);
    procedure CnvView0MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SpeedButton5Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton10Click(Sender: TObject);
    procedure SpeedButton11Click(Sender: TObject);
    procedure SpeedButton6Click(Sender: TObject);
    procedure SpeedButton8Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure SaveAs1Click(Sender: TObject);
    procedure VoxelHeader1Click(Sender: TObject);
    procedure N6Click(Sender: TObject);
    Procedure UpdateUndo_RedoState;
    procedure mnuBarUndoClick(Sender: TObject);
    procedure mnuBarRedoClick(Sender: TObject);
    procedure updatenormals1Click(Sender: TObject);
    procedure Delete1Click(Sender: TObject);
    procedure normalsaphere1Click(Sender: TObject);
    procedure RemoveRedundentVoxels1Click(Sender: TObject);
    procedure ClearEntireSection1Click(Sender: TObject);
    procedure CnCSource1Click(Sender: TObject);
    procedure PPMForUpdates1Click(Sender: TObject);
    procedure CGen1Click(Sender: TObject);
    procedure Dezire1Click(Sender: TObject);
    procedure PlanetCNC1Click(Sender: TObject);
    procedure PixelOps1Click(Sender: TObject);
    procedure ESource1Click(Sender: TObject);
    procedure SavageWarTS1Click(Sender: TObject);
    procedure SavageWarRA21Click(Sender: TObject);
    procedure YRArgentina1Click(Sender: TObject);
    procedure ibEd1Click(Sender: TObject);
    procedure XCC1Click(Sender: TObject);
    procedure OpenHyperlink(HyperLink: PChar);
    procedure VXLSEHelp1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure FlipXswitchFrontBack1Click(Sender: TObject);
    procedure FlipYswitchRightLeft1Click(Sender: TObject);
    procedure FlipZswitchTopBottom1Click(Sender: TObject);
    procedure MirrorBottomToTop1Click(Sender: TObject);
    procedure MirrorLeftToRight1Click(Sender: TObject);
    procedure MirrorBackToFront1Click(Sender: TObject);
    procedure MirrorFrontToBack1Click(Sender: TObject);
    procedure Nudge1Left1Click(Sender: TObject);
    procedure Section2Click(Sender: TObject);
    procedure Copyofthissection1Click(Sender: TObject);
    procedure RedUtils1Click(Sender: TObject);
    procedure RA2FAQ1Click(Sender: TObject);
    procedure BuildReopenMenu;
    procedure mnuHistoryClick(Sender: TObject);
    procedure BarReopenClick(Sender: TObject);
    procedure iberianSunPalette1Click(Sender: TObject);
    procedure RedAlert2Palette1Click(Sender: TObject);
    Procedure LoadPalettes;
    procedure blank1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    Procedure CheckVXLChanged;
    procedure SpeedButton7Click(Sender: TObject);
    procedure SpeedButton12Click(Sender: TObject);
    procedure ToolButton11Click(Sender: TObject);
    procedure ClearLayer1Click(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure Cut1Click(Sender: TObject);
    procedure ClearUndoSystem1Click(Sender: TObject);
    procedure PasteFull1Click(Sender: TObject);
    procedure Paste1Click(Sender: TObject);
    procedure About2Click(Sender: TObject);
    procedure EmptyVoxel1Click(Sender: TObject);
    procedure EmptyVoxel2Click(Sender: TObject);
    Procedure NewVFile(Game : integer);
    Procedure SetCursor;
    procedure SmoothNormals1Click(Sender: TObject);
    procedure VoxelTexture1Click(Sender: TObject);
    procedure test1Click(Sender: TObject);
    procedure AcidVat1Click(Sender: TObject);
    procedure RA2GraphicsHeaven1Click(Sender: TObject);
    procedure CnCGuild1Click(Sender: TObject);
    procedure iberiumSunCom1Click(Sender: TObject);
    procedure Importfrommodel1Click(Sender: TObject);
    procedure Resize1Click(Sender: TObject);
    procedure SpinButton3UpClick(Sender: TObject);
    procedure SpinButton3DownClick(Sender: TObject);
    procedure SpinButton1DownClick(Sender: TObject);
    procedure SpinButton1UpClick(Sender: TObject);
    procedure SpinButton2DownClick(Sender: TObject);
    procedure SpinButton2UpClick(Sender: TObject);
    procedure FullResize1Click(Sender: TObject);
    procedure ToolButton9Click(Sender: TObject);
    procedure SpeedButton9Click(Sender: TObject);
    Procedure SelectCorrectPalette;
    Procedure SelectCorrectPalette2(Palette : String);
    procedure ToolButton13Click(Sender: TObject);
    Procedure CreateVxlError(v,n : Boolean);
    procedure N1x1Click(Sender: TObject);
    procedure CubedAutoNormals1Click(Sender: TObject);
    procedure SpeedButton13Click(Sender: TObject);
  private
    { Private declarations }
    procedure Idle(Sender: TObject; var Done: Boolean);
  public
    { Public declarations }
    {IsEditable,}IsVXLLoading : boolean;
     ShiftPressed : boolean;
     AltPressed : boolean;
     p_Frm3DPreview : PFrm3DPReview;
     {$ifdef DEBUG_FILE}
     DebugFile : TDebugFile;
     {$endif}
  end;

implementation

{$R *.dfm}

uses FormHeaderUnit,LoadForm,FormNewSectionSizeUnit,FormPalettePackAbout,HVA,FormReplaceColour,FormVoxelTexture,
  FormHVA,FormBoundsManager, FormImportSection,FormFullResize,FormPreferences,FormVxlError;

procedure RunAProgram (const theProgram, itsParameters, defaultDirectory : string);
var rslt     : integer;
    msg      : string;
begin
rslt := ShellExecute (0, 'open',
                        pChar (theProgram),
                        pChar (itsParameters),
                        pChar (defaultDirectory),
                        sw_ShowNormal);
if rslt <= 32
then begin
     case rslt of
          0,
          se_err_OOM :             msg := 'Out of memory/resources';
          error_File_Not_Found :   msg := 'File "' + theProgram + '" not found';
          error_Path_Not_Found :   msg := 'Path not found';
          error_Bad_Format :       msg := 'Damaged or invalid exe';
          se_err_AccessDenied :    msg := 'Access denied';
          se_err_NoAssoc,
          se_err_AssocIncomplete : msg := 'Filename association invalid';
          se_err_DDEBusy,
          se_err_DDEFail,
          se_err_DDETimeOut :      msg := 'DDE error';
         se_err_Share :        msg := 'Sharing violation';
          else                    msg := 'no text';
          end; // of case
     raise Exception.Create ('ShellExecute error #' + IntToStr (rslt) + ': ' + msg);
     end;
end;

procedure TFrmTimeMain.Open1Click(Sender: TObject);
begin
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: Open1Click');
   {$endif}
   IsVXLLoading := true;
   Application.OnIdle := nil; //Idle;
   CheckVXLChanged;

   if OpenVXLDialog.Execute then
      SetIsEditable(LoadVoxel(OpenVXLDialog.FileName));

   if IsEditable then
      DoAfterLoadingThings;

   IsVXLLoading := false;
end;

procedure TFrmTimeMain.CnvView0Paint(Sender: TObject);
begin
   PaintView2(0,true,CnvView[0],ActiveSection.View[0]);
end;

procedure TFrmTimeMain.FormCreate(Sender: TObject);
var
   i : integer;
   pfd : TPIXELFORMATDESCRIPTOR;
   pf  : Integer;
begin
   // 1.32: Debug adition
   {$ifdef DEBUG_FILE}
   DebugFile := TDebugFile.Create('debugdev.txt');
   DebugFile.Add('FrmTimeMain: FormCreate');
   {$endif}

   // 1.32: Shortcut aditions
   ShiftPressed := false;
   AltPressed := false;

   CnvView[0] := @CnvView0;
   CnvView[1] := @CnvView1;
   CnvView[2] := @CnvView2;

   lblView[0] := @lblView0;
   lblView[1] := @lblView1;
   lblView[2] := @lblView2;

   BuildReopenMenu;
   Height := 768;

   for i := 0 to 2 do
   begin
      cnvView[i].ControlStyle := cnvView[i].ControlStyle + [csOpaque];
      lblView[i].ControlStyle := lblView[i].ControlStyle + [csOpaque];
   end;

   SetIsEditable(False);
   //FrmTimeMain.DoubleBuffered := true;
   //MainPaintPanel.DoubleBuffered := true;
   LeftPanel.DoubleBuffered := true;
   RightPanel.DoubleBuffered := true;

   SetActiveColor(16,true);
   SetActiveNormal(0,false);

   changecaption(false,'');

   // Resets the 3 views on the right sidebar if the res is too low for 203 high ones.
   if RightPanel.Height < lblView1.Height+CnvView1.Height+lblView2.Height+CnvView2.Height+lbl3dview.Height+Panel7.Height+OGL3DPreview.Height then
   begin
      i := RightPanel.Height - (lblView1.Height+lblView2.Height+lbl3dview.Height+Panel7.Height);

      i := trunc(i / 3);
      CnvView1.Height := i;
      CnvView2.Height := i;
      OGL3DPreview.Height := i;
   end;

   // 1.2 Enable Idle only on certain situations.
   Application.OnIdle := nil;

   // OpenGL initialisieren
   InitOpenGL;
   dc:=GetDC(OGL3DPreview.Handle);

   BGColor   := CleanVCCol(GetVXLPaletteColor(-1));
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

   glEnable(GL_TEXTURE_2D);                     // Enable Texture Mapping
   glClearColor(BGColor.X, BGColor.Y, BGColor.Z, 1.0);
   glShadeModel(GL_SMOOTH);                 // Enables Smooth Color Shading
   glClearDepth(1.0);                       // Depth Buffer Setup
   glEnable(GL_DEPTH_TEST);                 // Enable Depth Buffer
   glDepthFunc(GL_LESS);		           // The Type Of Depth Test To Do

   glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);   //Realy Nice perspective calculations

   BuildFont;

   glEnable(GL_CULL_FACE);
   glCullFace(GL_BACK);

   xRot :=-90;
   yRot :=-85;
   Depth :=-30;
   DemoStart :=GetTickCount();

   QueryPerformanceFrequency(FFrequency); // get high-resolution Frequency
   QueryPerformanceCounter(FoldTime);

   glViewport(0, 0, OGL3DPreview.Width, OGL3DPreview.Height);    // Set the viewport for the OpenGL window
   glMatrixMode(GL_PROJECTION);        // Change Matrix Mode to Projection
   glLoadIdentity();                   // Reset View
   gluPerspective(45.0, OGL3DPreview.Width/OGL3DPreview.Height, 1.0, 500.0);  // Do the perspective calculations. Last value = max clipping depth

   glMatrixMode(GL_MODELVIEW);         // Return to the modelview matrix  }
   oglloaded := true;


   p_Frm3DPreview := nil;
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: FormCreate Loaded');
   {$endif}
end;

{------------------------------------------------------------------}
{------------------------------------------------------------------}
procedure TFrmTimeMain.Idle(Sender: TObject; var Done: Boolean);
var
   tmp : int64;
   t2 : double;
   Form3D : PFrm3DPreview;
begin
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: Idle');
   {$endif}
   Done := FALSE;
   if not iseditable then
   begin
      Done := true;
      exit;
   end;

   if not oglloaded then
   begin
      Done := true;
      exit;
   end;
//   if not Display3dView1.checked then
//   begin
      LastTime :=ElapsedTime;
      ElapsedTime := GetTickCount() - DemoStart;     // Calculate Elapsed Time
      ElapsedTime :=(LastTime + ElapsedTime) DIV 2; // Average it out for smoother movement

      QueryPerformanceCounter(tmp);
      t2 := tmp-FoldTime;
      FPS := t2/FFrequency;
      FoldTime := TMP;
      FPS := 1/FPS;

      wglMakeCurrent(dc,rc);        // Make the DC (Form1) the rendering Context
      glDraw();                         // Draw the scene
      SwapBuffers(DC);                  // Display the scene
//   end;
   // For those wondering why do I need a variable... to avoid problems
   // when closing the form.
   Form3D := p_Frm3DPreview;
   if Form3D <> nil then
   begin
      {$ifdef DEBUG_FILE}
      DebugFile.Add('Preview3D: Idle');
      {$endif}
      Form3D^.Idle(sender,done);
      // Once the window is rendered, the FormMain OGL returns as default.
   end;
end;

procedure TFrmTimeMain.FormResize(Sender: TObject);
begin
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: FormResize');
   {$endif}
   CentreViews;
   setupscrollbars;
end;

procedure TFrmTimeMain.changecaption(Filename : boolean; FName : string);
begin
   Caption := APPLICATION_TITLE + ' v' + APPLICATION_VER;

   if Filename then
      Caption := Caption + ' [' + extractfilename(FName) + ']';
end;

procedure TFrmTimeMain.CnvView1Paint(Sender: TObject);
begin
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: CnvView1Paint');
   {$endif}
   PaintView2(1,false,CnvView[1],ActiveSection.View[1]);
end;

procedure TFrmTimeMain.CnvView2Paint(Sender: TObject);
begin
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: CnvView2Paint');
   {$endif}
   PaintView2(2,false,CnvView[2],ActiveSection.View[2]);
end;

Procedure TFrmTimeMain.SetIsEditable(Value : boolean);
var
i : integer;
begin
   IsEditable := value;

   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: SetIsEditable');
   {$endif}

   for i := 0 to 2 do
   begin
      cnvView[i].Enabled := isEditable;
     //lblView[i].Enabled := isEditable;
   end;

   if iseditable then
   begin
      for i := 0 to 2 do
      begin
         lblView[i].Color := clNavy;
         lblView[i].Font.Color := clYellow;
         lblView[i].Refresh;
      end;

      lblSection.Color := clNavy;
      lblSection.Font.Color := clYellow;
      lblTools.Color := clNavy;
      lblTools.Font.Color := clYellow;
      lblPalette.Color := clNavy;
      lblPalette.Font.Color := clYellow;
      lbl3dview.Color := clNavy;
      lbl3dview.Font.Color := clYellow;
      lblLayer.Color := clNavy;
      lblLayer.Font.Color := clYellow;
      lblBrush.Color := clNavy;
      lblBrush.Font.Color := clYellow;

      lblBrush.refresh;
      lblLayer.refresh;
      lbl3dview.refresh;
      lblSection.refresh;
      lblTools.refresh;
      lblPalette.refresh;

      If SpectrumMode = ModeColours then
         SetActiveColor(ActiveColour,true);
      cnvPalette.refresh;

      // 1.32 Aditions:
      cnvView0.OnMouseDown := cnvView0MouseDown;
      cnvView0.OnMouseUp := cnvView0MouseUp;
      cnvView0.OnMouseMove := cnvView0MouseMove;
      cnvView0.OnPaint := cnvView0Paint;
      cnvView1.OnMouseDown := cnvView1MouseDown;
      cnvView1.OnMouseUp := cnvView1MouseUp;
      cnvView1.OnMouseMove := cnvView1MouseMove;
      cnvView1.OnPaint := cnvView1Paint;
      cnvView2.OnMouseDown := cnvView2MouseDown;
      cnvView2.OnMouseUp := cnvView2MouseUp;
      cnvView2.OnMouseMove := cnvView2MouseMove;
      cnvView2.OnPaint := cnvView2Paint;
      oGL3DPreview.OnMouseDown := ogl3DPreviewMouseDown;
      oGL3DPreview.OnMouseUp := ogl3DPreviewMouseUp;
      oGL3DPreview.OnMouseMove := ogl3DPreviewMouseMove;
   end
   else
   begin
      // We'll force a closure of the 3D Preview window.
      if p_Frm3DPreview <> nil then
      begin
         p_Frm3DPreview^.Release;
         p_Frm3DPreview := nil;
      end;

      // 1.32 Aditions:
      cnvView0.OnMouseDown := nil;
      cnvView0.OnMouseUp := nil;
      cnvView0.OnMouseMove := nil;
      cnvView0.OnPaint := nil;
      cnvView1.OnMouseDown := nil;
      cnvView1.OnMouseUp := nil;
      cnvView1.OnMouseMove := nil;
      cnvView1.OnPaint := nil;
      cnvView2.OnMouseDown := nil;
      cnvView2.OnMouseUp := nil;
      cnvView2.OnMouseMove := nil;
      cnvView2.OnPaint := nil;
      oGL3DPreview.OnMouseDown := nil;
      oGL3DPreview.OnMouseUp := nil;
      oGL3DPreview.OnMouseMove := nil;
   end;

   View1.Visible := IsEditable;
//   Section1.Visible := IsEditable;
   ools2.Visible := IsEditable;
   Edit1.Visible := IsEditable;
//   Scripts1.Visible := IsEditable;

   SectionCombo.Enabled := IsEditable;
//   BarSaveAs.Enabled := IsEditable;
//   ToolButton5.Enabled := IsEditable;
//   ToolButton7.Enabled := IsEditable;
//   ToolButton3.Enabled := IsEditable;
//   ToolButton13.Enabled := IsEditable;
//   ToolButton11.Enabled := IsEditable;
//   ToolButton1.Enabled := IsEditable;
//   ToolButton2.Enabled := IsEditable;

   btn3DRotateY.Enabled := IsEditable;
   btn3DRotateY2.Enabled := IsEditable;
   btn3DRotateX.Enabled := IsEditable;
   btn3DRotateX2.Enabled := IsEditable;
   spin3Djmp.Enabled := IsEditable;
   SpeedButton1.Enabled := IsEditable;
   SpeedButton2.Enabled := IsEditable;
   SpeedButton3.Enabled := IsEditable;
   SpeedButton4.Enabled := IsEditable;
   SpeedButton5.Enabled := IsEditable;
   SpeedButton6.Enabled := IsEditable;
   SpeedButton7.Enabled := IsEditable;
   SpeedButton8.Enabled := IsEditable;
   SpeedButton9.Enabled := IsEditable;
   SpeedButton10.Enabled := IsEditable;
   SpeedButton11.Enabled := IsEditable;
   SpeedButton12.Enabled := IsEditable;
   SpeedButton13.Enabled := IsEditable;

   Brush_1.Enabled := IsEditable;
   Brush_2.Enabled := IsEditable;
   Brush_3.Enabled := IsEditable;
   Brush_4.Enabled := IsEditable;
   Brush_5.Enabled := IsEditable;

   pnlLayer.Enabled := IsEditable;

   mnuDirectionPopup.AutoPopup := IsEditable;

   Magnification1.Enabled := IsEditable;

   N6.Enabled := IsEditable;
   SaveAs1.Enabled := IsEditable;
//   Save1.Enabled := IsEditable;

   if not iseditable then
      OGL3DPreview.Refresh;
end;

Procedure TFrmTimeMain.RefreshAll;
begin
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: Refresh All');
   {$endif}
   RefreshViews;
   RepaintViews;
   Update3dView(ActiveSection);
   if p_Frm3DPreview <> nil then
   begin
      p_Frm3DPreview^.Update3dView(ActiveSection);
   end;
end;

Procedure TFrmTimeMain.SetupSections;
var
   i : integer;
begin
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: Setup Sections');
   {$endif}
   SectionCombo.Clear;

   for i := 0 to (VoxelFile.Header.NumSections - 1) do
      SectionCombo.Items.Add(VoxelFile.Section[i].Name);
   SectionCombo.ItemIndex := CurrentSection;
end;

procedure TFrmTimeMain.SectionComboChange(Sender: TObject);
begin
   if IsVXLLoading then exit;

   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: SectionComboChange');
   {$endif}

   CurrentSection := SectionCombo.ItemIndex;
   ChangeSection;

   ResetUndoRedo;
   UpdateUndo_RedoState;

   RefreshAll;
end;

procedure TFrmTimeMain.Full1Click(Sender: TObject);
begin
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: Full1Click');
   {$endif}
   SetViewMode(ModeFull);
   RepaintViews;
end;

procedure TFrmTimeMain.CrossSection1Click(Sender: TObject);
begin
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: CrossSection1Click');
   {$endif}
   SetViewMode(ModeCrossSection);
   RepaintViews;
end;

procedure TFrmTimeMain.EmphasiseDepth1Click(Sender: TObject);
begin
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: EmphasiseDepth1Click');
   {$endif}
   SetViewMode(ModeEmphasiseDepth);
   RepaintViews;
end;

Procedure TFrmTimeMain.SetViewMode(VM : EViewMode);
begin
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: SetViewMode');
   {$endif}

   Full1.checked := false;
   CrossSection1.checked := false;
   EmphasiseDepth1.checked := false;

   if VM = ModeFull then
      Full1.checked := true;

   if VM = ModeCrossSection then
      CrossSection1.checked := true;

   if VM = ModeEmphasiseDepth then
      EmphasiseDepth1.checked := true;

   ViewMode := VM;
end;

Procedure TFrmTimeMain.SetSpectrum(SP : ESpectrumMode);
begin
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: SetSpectrum');
   {$endif}

   Normals1.checked := false;
   Colours1.checked := false;

   if SP = ModeNormals then
      Normals1.checked := true
   else
      Colours1.checked := true;

   SpectrumMode := SP;
   SetSpectrumMode;

   ActiveSection.View[0].Refresh;
   ActiveSection.View[1].Refresh;
   ActiveSection.View[2].Refresh;

   PaintPalette(cnvPalette,True);

   RebuildLists := true;
   if p_Frm3DPreview <> nil then
   begin
      p_Frm3DPreview^.RebuildLists := true;
   end;

   if not IsVXLLoading then
      RepaintViews;
end;

procedure TFrmTimeMain.Colours1Click(Sender: TObject);
begin
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: Colours1Click');
   {$endif}
   SetSpectrum(ModeColours);
   SetActiveColor(ActiveColour,true);
end;

procedure TFrmTimeMain.Normals1Click(Sender: TObject);
begin
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: Normals1Click');
   {$endif}
   SetSpectrum(ModeNormals);
   SetActiveNormal(ActiveNormal,true);
end;

procedure TFrmTimeMain.cnvPalettePaint(Sender: TObject);
begin
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: cnvPalettePaint');
   {$endif}
   PaintPalette(cnvPalette,true);
end;

procedure TFrmTimeMain.SetActiveColor(Value : integer; CN : boolean);
begin
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: SetActiveColour');
   {$endif}
   ActiveColour := Value;
   if CN then
   if SpectrumMode = ModeColours then
      SetActiveCN(Value);
end;

procedure TFrmTimeMain.SetActiveNormal(Value : integer; CN : boolean);
var
   v : integer;
begin
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: SetActiveNormal');
   {$endif}
   v := Value;

   if ActiveNormalsCount > 0 then
      if v > ActiveNormalsCount-1 then
         v := ActiveNormalsCount-1; // normal too high

   ActiveNormal := v;

   if CN then
      if SpectrumMode = ModeNormals then
         SetActiveCN(V);
end;

Procedure TFrmTimeMain.SetActiveCN(Value : integer);
begin
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: SetActiveCN');
   {$endif}
   if isEditable then
      pnlActiveColour.Color := GetVXLPaletteColor(Value)
   else
      pnlActiveColour.Color := colourtogray(GetVXLPaletteColor(Value));
   lblActiveColour.Caption := IntToStr(Value) + ' (0x' + IntToHex(Value,3) + ')';
   cnvPalette.Repaint;
end;

procedure TFrmTimeMain.ScrollBar1Change(Sender: TObject);
var
x , y, width, height : integer;
begin
   if not isEditable then Exit;
   if not scrollbar_editable then Exit;

   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: ScrollBar1Change');
   {$endif}

   Width := cnvView[0].Width;
   Height := cnvView[0].Height;
   with ActiveSection.Viewport[0] do
   begin
      x := ActiveSection.View[0].Width * Zoom;
      if ScrollBar1.enabled then
         if x > Width then
            Left := 0 - ((x - Width) div 2) -(ScrollBar1.Position - (ScrollBar1.Max div 2))
         else
            Left := ((Width - x) div 2) -(ScrollBar1.Position - (ScrollBar1.Max div 2));
      y := ActiveSection.View[0].Height * Zoom;

      if ScrollBar2.enabled then
         if y > Height then
            Top := 0 - ((y - Height) div 2) -(ScrollBar2.Position - (ScrollBar2.Max div 2))
         else
            Top := (Height - y) div 2 -(ScrollBar2.Position - (ScrollBar2.Max div 2));
   end;
   PaintView2(0,true,CnvView[0],ActiveSection.View[0]);
end;

procedure TFrmTimeMain.setupscrollbars;
begin
   ScrollBar1.Enabled := false;
   ScrollBar2.Enabled := false;

   if not isEditable then Exit;

   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: SetupScrollBars');
   {$endif}
   If (ActiveSection.View[0].Width * ActiveSection.Viewport[0].zoom) > cnvView[0].Width then
   begin
      scrollbar_editable := false;
      // showmessage(inttostr(ActiveSection.View[0].Width * ActiveSection.Viewport[0].zoom - cnvView[0].Width));
      ScrollBar2.Position := 0;
      ScrollBar1.max := ActiveSection.View[0].Width * ActiveSection.Viewport[0].zoom - cnvView[0].Width;
      ScrollBar1.Position := ScrollBar1.max div 2;
      ScrollBar1.Enabled := true;
      scrollbar_editable := true;
   end
   else
      ScrollBar1.Enabled := false;

   If (ActiveSection.View[0].Height * ActiveSection.Viewport[0].zoom) > cnvView[0].Height then
   begin
      scrollbar_editable := false;
      //showmessage(inttostr(ActiveSection.View[0].Height * ActiveSection.Viewport[0].zoom - cnvView[0].Height));
      ScrollBar2.Position := 0;
      ScrollBar2.max := ActiveSection.View[0].Height * ActiveSection.Viewport[0].zoom - cnvView[0].Height;
      ScrollBar2.Position := ScrollBar2.max div 2;
      ScrollBar2.Enabled := true;
      scrollbar_editable := true;
   end
   else
      ScrollBar2.Enabled := false;
end;

Function GetParamStr : String;
var
x : integer;
begin
   Result := '';
   for x := 1 to ParamCount do
      if Result <> '' then
         Result := Result + ' ' +ParamStr(x)
      else
         Result := ParamStr(x);
end;

procedure TFrmTimeMain.FormShow(Sender: TObject);
var
   frm: TLoadFrm;
   l: Integer;
   Reg: TRegistry;
   LatestVersion: string;
begin
   frm:=TLoadFrm.Create(Self);
   if testbuild then
      frm.Label4.Caption := APPLICATION_VER + ' TB '+testbuildversion
   else
      frm.Label4.Caption := APPLICATION_VER;

   // 1.2:For future compatibility with other OS tools, we are
   // using the registry keys to confirm its existance.
   Reg :=TRegistry.Create;
   Reg.RootKey := HKEY_LOCAL_MACHINE;
   if Reg.OpenKey('Software\CnC Tools\VXLSEIII\',true) then
   begin
      LatestVersion := Reg.ReadString('Version');
      if APPLICATION_VER > LatestVersion then
      begin
         Reg.WriteString('Path',ParamStr(0));
         Reg.WriteString('Version',APPLICATION_VER);
      end;
      Reg.CloseKey;
   end;
   Reg.Free;

   frm.Show;
   l := 0;
   while l < 25 do
   begin
      if l = 0 then begin frm.Loading.Caption := 'Loading: 3rd Party Palettes';frm.Loading.Refresh; LoadPalettes; end;
//      if l = 10 then begin frm.Loading.Caption := 'Loading: Colour Schemes'; frm.Loading.Refresh; LoadCScheme; end;
      if l = 23 then begin frm.Loading.Caption := 'Finished Loading'; delay(50); end;
      l := l + 1;
      delay(2);
   end;
   frm.Close;
   frm.Free;

   WindowState := wsMaximized;
//  refresh;
   setupscrollbars;
   UpdateUndo_RedoState;
   VXLTool := 4;

   if ParamCount > 0 then
   If FileExists(GetParamStr) then
   Begin
      IsVXLLoading := true;
      SetIsEditable(LoadVoxel(GetParamStr));
      if IsEditable then
         DoAfterLoadingThings;

      IsVXLLoading := false;
   End;
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: FormShow Loaded');
   {$endif}
end;

procedure TFrmTimeMain.cnvPaletteMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var colwidth, rowheight: Real;
    i, j, idx: Integer;
begin
   If not isEditable then exit;
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: cnvPaletteMouseUp');
   {$endif}
   colwidth := cnvPalette.Width / 8;
   rowheight := cnvPalette.Height / 32;
   i := Trunc(X / colwidth);
   j := Trunc(Y / rowheight);
   idx := (i * 32) + j;
   if SpectrumMode = ModeColours then
      SetActiveColor(idx,true)
   else if not (idx > ActiveNormalsCount-1) then
      SetActiveNormal(idx,true);
end;

// 1.2b: Build the Show Used Colours/Normals array
// Ripped from VXLSE II 2.2 SE OpenGL (never released version)
// Original function apparently made by Stucuk
procedure BuildUsedColoursArray;
var
   x,y,z : integer;
   v : TVoxelUnpacked;
begin
   // This part is modified by Banshee. It's stupid to see it checking if x < 244 everytime
   for x := 0 to 244 do
   begin
      UsedColours[x] := false;
      UsedNormals[x] := false;
   end;
   for x := 245 to 255 do
      UsedColours[x] := false;


   for x := 0 to ActiveSection.Tailer.XSize -1 do
   for y := 0 to ActiveSection.Tailer.YSize -1 do
   for z := 0 to ActiveSection.Tailer.ZSize -1 do
   begin
      ActiveSection.GetVoxel(x,y,z,v);
      if v.Used then
      begin
         UsedColours[v.Colour] := true;
         UsedNormals[v.normal] := true;
      end;
   end;
end;

procedure TFrmTimeMain.FormDestroy(Sender: TObject);
begin
   wglMakeCurrent(0,0);
   wglDeleteContext(rc);
   UpdateHistoryMenu;
   Config.SaveSettings;
end;

procedure TFrmTimeMain.OGL3DPreviewMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   If not isEditable then exit;
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: OGL3DPreviewMouseMove');
   {$endif}
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

procedure TFrmTimeMain.OGL3DPreviewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   If not isEditable then exit;
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: OGL3DPreviewMouseDown');
   {$endif}
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

procedure TFrmTimeMain.OGL3DPreviewMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: OGL3DPreviewMouseUp');
   {$endif}
   MouseButton :=0;
end;

procedure TFrmTimeMain.SpeedButton1MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: SpeedButton1MouseUp');
   {$endif}
   Popup3d.Popup(Left+SpeedButton1.Left+ RightPanel.Left +5,Top+ 90+ Panel7.Top + SpeedButton1.Top);
end;

procedure TFrmTimeMain.NormalsTest1Click(Sender: TObject);
begin
   NormalsTest1.Checked := not NormalsTest1.Checked;
   if NormalsTest1.Checked then
      NormalsTest2.Checked := false;
end;

procedure TFrmTimeMain.DebugMode1Click(Sender: TObject);
begin
   DebugMode1.Checked := not DebugMode1.Checked;
end;

procedure TFrmTimeMain.NormalsTest2Click(Sender: TObject);
begin
   NormalsTest2.Checked := not NormalsTest2.Checked;
   if NormalsTest2.Checked then
   NormalsTest1.Checked := false;
end;

Procedure TFrmTimeMain.SetRotationAdders;
var
   V : single;
begin
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: SetRotationAdders');
   {$endif}
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

procedure TFrmTimeMain.SpeedButton2Click(Sender: TObject);
begin
   Depth := -30;
end;

procedure TFrmTimeMain.btn3DRotateXClick(Sender: TObject);
begin
   if btn3DRotateX.Down then
begin
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: Btn3DRotateXClick');
   {$endif}
   SetRotationAdders;
   XRotB := True;
end
else
   XRotB := false;
end;

procedure TFrmTimeMain.btn3DRotateX2Click(Sender: TObject);
begin
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: Bnt3DRotateX2Click');
   {$endif}
   if btn3DRotateX2.Down then
   begin
      SetRotationAdders;
      XRotB := True;
   end
   else
      XRotB := false;
end;

procedure TFrmTimeMain.btn3DRotateY2Click(Sender: TObject);
begin
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: Btn3DRotateY2Click');
   {$endif}
   if btn3DRotateY2.Down then
   begin
      SetRotationAdders;
      YRotB := True;
   end
   else
      YRotB := false;
end;

procedure TFrmTimeMain.btn3DRotateYClick(Sender: TObject);
begin
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: btn3DRotateYClick');
   {$endif}
   if btn3DRotateY.Down then
   begin
      SetRotationAdders;
      YRotB := True;
   end
   else
      YRotB := false;
end;

procedure TFrmTimeMain.spin3DjmpChange(Sender: TObject);
begin
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: Spin3DJmpChange');
   {$endif}
   SetRotationAdders;
end;

procedure TFrmTimeMain.BackgroundColour1Click(Sender: TObject);
begin
   ColorDialog.Color := TVector3fToTColor(BGColor);
   if ColorDialog.Execute then
      BGColor := TColorToTVector3f(ColorDialog.Color);
end;

procedure TFrmTimeMain.extColour1Click(Sender: TObject);
begin
   ColorDialog.Color := TVector3fToTColor(FontColor);
   if ColorDialog.Execute then
      FontColor := TColorToTVector3f(ColorDialog.Color);
end;

procedure TFrmTimeMain.ClearRemapClicks;
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

procedure TFrmTimeMain.Red1Click(Sender: TObject);
begin
   ClearRemapClicks;
   Red1.Checked := true;
   RemapColour.X := RemapColourMap[0].R /255;
   RemapColour.Y := RemapColourMap[0].G /255;
   RemapColour.Z := RemapColourMap[0].B /255;
end;

procedure TFrmTimeMain.Blue1Click(Sender: TObject);
begin
   ClearRemapClicks;
   Blue1.Checked := true;
   RemapColour.X := RemapColourMap[1].R /255;
   RemapColour.Y := RemapColourMap[1].G /255;
   RemapColour.Z := RemapColourMap[1].B /255;
end;

procedure TFrmTimeMain.Green1Click(Sender: TObject);
begin
   ClearRemapClicks;
   Green1.Checked := true;
   RemapColour.X := RemapColourMap[2].R /255;
   RemapColour.Y := RemapColourMap[2].G /255;
   RemapColour.Z := RemapColourMap[2].B /255;
end;

procedure TFrmTimeMain.White1Click(Sender: TObject);
begin
   ClearRemapClicks;
   White1.Checked := true;
   RemapColour.X := RemapColourMap[3].R /255;
   RemapColour.Y := RemapColourMap[3].G /255;
   RemapColour.Z := RemapColourMap[3].B /255;
end;

procedure TFrmTimeMain.Orange1Click(Sender: TObject);
begin
   ClearRemapClicks;
   Orange1.Checked := true;
   RemapColour.X := RemapColourMap[4].R /255;
   RemapColour.Y := RemapColourMap[4].G /255;
   RemapColour.Z := RemapColourMap[4].B /255;
end;

procedure TFrmTimeMain.Magenta1Click(Sender: TObject);
begin
   ClearRemapClicks;
   Magenta1.Checked := true;
   RemapColour.X := RemapColourMap[5].R /255;
   RemapColour.Y := RemapColourMap[5].G /255;
   RemapColour.Z := RemapColourMap[5].B /255;
end;

procedure TFrmTimeMain.Purple1Click(Sender: TObject);
begin
   ClearRemapClicks;
   Purple1.Checked := true;
   RemapColour.X := RemapColourMap[6].R /255;
   RemapColour.Y := RemapColourMap[6].G /255;
   RemapColour.Z := RemapColourMap[6].B /255;
end;

procedure TFrmTimeMain.Gold1Click(Sender: TObject);
begin
   ClearRemapClicks;
   Gold1.Checked := true;
   RemapColour.X := RemapColourMap[7].R /255;
   RemapColour.Y := RemapColourMap[7].G /255;
   RemapColour.Z := RemapColourMap[7].B /255;
end;

procedure TFrmTimeMain.DarkSky1Click(Sender: TObject);
begin
   ClearRemapClicks;
   DarkSky1.Checked := true;
   RemapColour.X := RemapColourMap[8].R /255;
   RemapColour.Y := RemapColourMap[8].G /255;
   RemapColour.Z := RemapColourMap[8].B /255;
end;

procedure TFrmTimeMain.Front1Click(Sender: TObject);
begin
   XRot := -90;
   YRot := -90;
//Depth := -30;
end;

procedure TFrmTimeMain.Back1Click(Sender: TObject);
begin
   XRot := -90;
   YRot := 90;
//Depth := -30;
end;

procedure TFrmTimeMain.LEft1Click(Sender: TObject);
begin
   XRot := -90;
   YRot := -180;
//Depth := -30;
end;

procedure TFrmTimeMain.Right1Click(Sender: TObject);
begin
   XRot := -90;
   YRot := 0;
//Depth := -30;
end;

procedure TFrmTimeMain.Bottom1Click(Sender: TObject);
begin
   XRot := 180;
   YRot := 180;
//Depth := -30;
end;

procedure TFrmTimeMain.op1Click(Sender: TObject);
begin
   XRot := 0;
   YRot := 180;
//Depth := -30;
end;

procedure TFrmTimeMain.Cameo1Click(Sender: TObject);
begin
   XRot := 287;//-72.5;
   YRot := 225;//237;
//Depth := -30;
end;

procedure TFrmTimeMain.Cameo21Click(Sender: TObject);
begin
   XRot := 287;//-72.5;
   YRot := 315;//302;
//Depth := -30;
end;

procedure TFrmTimeMain.Cameo31Click(Sender: TObject);
begin
   XRot := 287;//-72.5;
   YRot := 255;//302;
//Depth := -30;
end;

procedure TFrmTimeMain.Cameo41Click(Sender: TObject);
begin
   XRot := 287;//-72.5;
   YRot := 285;//302;
//Depth := -30;
end;

procedure TFrmTimeMain.CnvView2MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: CnvView2MouseUp');
   {$endif}
   isLeftMB := false;
end;

procedure TFrmTimeMain.CnvView1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: CnvView1MouseUp');
   {$endif}
   isLeftMB := false;
end;

procedure TFrmTimeMain.CnvView1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: CnvView1MouseDown');
   {$endif}
   if Button = mbLeft then
      isLeftMB := true;

   if isLeftMB then
   begin
      TranslateClick(1,X,Y,LastClick[1].X,LastClick[1].Y,LastClick[1].Z);
      MoveCursor(LastClick[1].X,LastClick[1].Y,LastClick[1].Z,true);
      CursorResetNoMAX;
   end;
end;

procedure TFrmTimeMain.CnvView1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
   if not IsEditable then exit;
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: CnvView1MouseMove');
   {$endif}
   if isLeftMB then
   begin
      TranslateClick(1,X,Y,LastClick[1].X,LastClick[1].Y,LastClick[1].Z);
      MoveCursor(LastClick[1].X,LastClick[1].Y,LastClick[1].Z,true);
      CursorResetNoMAX;
   end;
end;

procedure TFrmTimeMain.CnvView2MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
   if not IsEditable then exit;
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: CnvView2MouseMove');
   {$endif}
   if isLeftMB then
   begin
      TranslateClick(2,X,Y,LastClick[2].X,LastClick[2].Y,LastClick[2].Z);
      MoveCursor(LastClick[2].X,LastClick[2].Y,LastClick[2].Z,true);
      CursorResetNoMAX;
   end;
end;

procedure TFrmTimeMain.CnvView2MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: CnvView2MouseDown');
   {$endif}
   if Button = mbLeft then
      isLeftMB := true;

   if isLeftMB then
   begin
      TranslateClick(2,X,Y,LastClick[2].X,LastClick[2].Y,LastClick[2].Z);
      MoveCursor(LastClick[2].X,LastClick[2].Y,LastClick[2].Z,true);
      CursorResetNoMAX;
   end;
end;

procedure TFrmTimeMain.CnvView0MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
   TempI : integer;
   V : TVoxelUnpacked;
begin
   if not iseditable then exit;
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: CnvView0MouseUp');
   {$endif}

   if ((ssAlt in shift) and (Button = mbLeft)) or ((Button = mbLeft) and (VXLTool = VXLTool_Dropper)) then
   begin
      TempI := GetPaletteColourFromVoxel(X,Y,0);
      if TempI > -1 then
         if SpectrumMode = ModeColours then
            SetActiveColor(TempI,True)
         else
            SetActiveNormal(TempI,True);
   end;

   if (ssCtrl in shift) and (Button = mbLeft) then
   begin
      TranslateClick(0,x,y,LastClick[0].x,LastClick[0].y,LastClick[0].z);
      MoveCursor(LastClick[0].X,LastClick[0].Y,LastClick[0].Z,false);
   end;

   if VXLTool = VXLTool_FloodFill then
   begin
      TranslateClick(0,x,y,LastClick[0].x,LastClick[0].y,LastClick[0].z);
      ActiveSection.GetVoxel(LastClick[0].X,LastClick[0].Y,LastClick[0].Z,v);
      if (SpectrumMode = ModeColours) or (v.Used=False) then
         v.Colour := ActiveColour;
      if (SpectrumMode = ModeNormals) or (v.Used=False) then
         v.Normal := ActiveNormal;

      v.Used := True;
      VXLFloodFillTool(ActiveSection,LastClick[0].X,LastClick[0].Y,LastClick[0].Z,v,ActiveSection.View[0].GetOrient);
   end;

   if VXLTool = VXLTool_FloodFillErase then
   begin
      TranslateClick(0,x,y,LastClick[0].x,LastClick[0].y,LastClick[0].z);
      ActiveSection.GetVoxel(LastClick[0].X,LastClick[0].Y,LastClick[0].Z,v);
      if (SpectrumMode = ModeColours) or (v.Used=False) then
         v.Colour := 0;
      if (SpectrumMode = ModeNormals) or (v.Used=False) then
         v.Normal := 0;

      v.Used := False;
      VXLFloodFillTool(ActiveSection,LastClick[0].X,LastClick[0].Y,LastClick[0].Z,v,ActiveSection.View[0].GetOrient);
   end;

   if VXLTool = VXLTool_Darken then
   begin
      TranslateClick(0,x,y,LastClick[0].x,LastClick[0].y,LastClick[0].z);
      ActiveSection.GetVoxel(LastClick[0].X,LastClick[0].Y,LastClick[0].Z,v);
      if (SpectrumMode = ModeColours) or (v.Used=False) then
         v.Colour := ActiveColour;
      if (SpectrumMode = ModeNormals) or (v.Used=False) then
         v.Normal := ActiveNormal;

      v.Used := True;
      VXLBrushToolDarkenLighten(ActiveSection,LastClick[0].X,LastClick[0].Y,LastClick[0].Z,VXLBrush,ActiveSection.View[0].GetOrient,True);
   end;

   if VXLTool = VXLTool_Lighten then
   begin
      TranslateClick(0,x,y,LastClick[0].x,LastClick[0].y,LastClick[0].z);
      ActiveSection.GetVoxel(LastClick[0].X,LastClick[0].Y,LastClick[0].Z,v);
      if (SpectrumMode = ModeColours) or (v.Used=False) then
         v.Colour := ActiveColour;
      if (SpectrumMode = ModeNormals) or (v.Used=False) then
         v.Normal := ActiveNormal;

      v.Used := True;
      VXLBrushToolDarkenLighten(ActiveSection,LastClick[0].X,LastClick[0].Y,LastClick[0].Z,VXLBrush,ActiveSection.View[0].GetOrient,false);
   end;

   if ApplyTempView(ActiveSection) then
      UpdateUndo_RedoState;

   if isLeftMouseDown then
   begin
      isLeftMouseDown := False;
      RefreshAll;
   end;
end;

Procedure TFrmTimeMain.UpdateCursor(P : TVector3i; Repaint : Boolean);
begin
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: UpdateCursor');
   {$endif}
   XCursorBar.Position := P.X;
   YCursorBar.Position := P.Y;
   ZCursorBar.Position := P.Z;
   StatusBar1.Panels[3].Text :=  'Pos: ' + inttostr(P.X) + ',' + inttostr(P.Y) + ',' + inttostr(P.Z);
   StatusBar1.Refresh;

   MoveCursor(P.X,P.Y,P.Z,Repaint);
end;

procedure TFrmTimeMain.XCursorBarChange(Sender: TObject);
begin
   if IsVXLLoading then exit;
   if isCursorReset then exit;

   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: XCursorBarChange');
   {$endif}
   UpdateCursor(SetVectorI(XCursorBar.Position,YCursorBar.Position,ZCursorBar.Position),true);
end;

Procedure TFrmTimeMain.CursorReset;
begin
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: CursorReset');
   {$endif}
   isCursorReset := true;

   XCursorBar.Position := 0;
   YCursorBar.Position := 0;
   ZCursorBar.Position := 0;

   XCursorBar.Max := ActiveSection.Tailer.XSize-1;
   YCursorBar.Max := ActiveSection.Tailer.YSize-1;
   ZCursorBar.Max := ActiveSection.Tailer.ZSize-1;

   XCursorBar.Position := ActiveSection.X;
   YCursorBar.Position := ActiveSection.Y;
   ZCursorBar.Position := ActiveSection.Z;

   StatusBar1.Panels[3].Text := 'Pos: ' + inttostr(ActiveSection.X) + ',' + inttostr(ActiveSection.Y) + ',' + inttostr(ActiveSection.Z);
   StatusBar1.Refresh;

   isCursorReset := false;
end;

Procedure TFrmTimeMain.CursorResetNoMAX;
begin
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: CursorResetNoMAX');
   {$endif}
   isCursorReset := true;

   XCursorBar.Position := ActiveSection.X;
   YCursorBar.Position := ActiveSection.Y;
   ZCursorBar.Position := ActiveSection.Z;

   StatusBar1.Panels[3].Text := 'Pos: ' + inttostr(ActiveSection.X) + ',' + inttostr(ActiveSection.Y) + ',' + inttostr(ActiveSection.Z);
   StatusBar1.Refresh;

   isCursorReset := false;
end;

Procedure TFrmTimeMain.SetupStatusBar;
begin
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: SetupStatusBar');
   {$endif}
   if ActiveSection.Tailer.Unknown = 2 then
      StatusBar1.Panels[0].Text := 'Type: Tiberian Sun'
   else if ActiveSection.Tailer.Unknown = 4 then
      StatusBar1.Panels[0].Text := 'Type: RedAlert 2'
   else
      StatusBar1.Panels[0].Text := 'Type: Unknown ' + inttostr(ActiveSection.Tailer.Unknown);

   StatusBar1.Panels[1].Text := 'X Size: ' + inttostr(ActiveSection.Tailer.XSize) + ', Y Size: ' + inttostr(ActiveSection.Tailer.YSize) + ', Z Size: ' + inttostr(ActiveSection.Tailer.ZSize);
   StatusBar1.Panels[2].Text := '';
   StatusBar1.Refresh;
end;

procedure TFrmTimeMain.CnvView0MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
   V : TVoxelUnpacked;
   TempI : integer;
begin
   if VoxelOpen and isEditable then //exit;
   begin
      {$ifdef DEBUG_FILE}
      DebugFile.Add('FrmTimeMain: CnvView0MouseMove');
      {$endif}
      TranslateClick(0,x,y,LastClick[0].x,LastClick[0].y,LastClick[0].z);
      StatusBar1.Panels[2].Text := 'X: ' + inttostr(LastClick[0].x) + ', Y: ' + inttostr(LastClick[0].y) + ', Z: ' + inttostr(LastClick[0].z);
      StatusBar1.Refresh;

      if not isLeftMouseDown then
      begin
         TranslateClick2(0,X,Y,LastClick[0].X,LastClick[0].Y,LastClick[0].Z);
         with ActiveSection.Tailer do
            if (LastClick[0].X < 0) or (LastClick[0].Y < 0) or (LastClick[0].Z < 0) or (LastClick[0].X > XSize-1) or (LastClick[0].Y > YSize-1) or (LastClick[0].Z > ZSize-1) then
            begin
               if TempView.Data_no > 0 then
               begin
                  TempView.Data_no := 0;
                  Setlength(TempView.Data,0);
                  CnvView[0].Repaint;
               end;
               Mouse_Current := crDefault;
               CnvView[0].Cursor := Mouse_Current;
               Exit;
            end;
         SetCursor;
         CnvView[0].Cursor := Mouse_Current;

         if TempView.Data_no > 0 then
         begin
            TempView.Data_no := 0;
            Setlength(TempView.Data,0);
            //CnvView[0].Repaint;
         end;

//         if not DisableDrawPreview1.Checked then
//         begin
            if VXLTool = VXLTool_Brush then
               if VXLBrush <> 4 then
               begin
                  ActiveSection.GetVoxel(LastClick[0].X,LastClick[0].Y,LastClick[0].Z,v);
                  if (SpectrumMode = ModeColours) or (v.Used=False) then
                     v.Colour := ActiveColour;
                  if (SpectrumMode = ModeNormals) or (v.Used=False) then
                     v.Normal := ActiveNormal;

                  v.Used := True;
                  VXLBrushTool(ActiveSection,LastClick[0].X,LastClick[0].Y,LastClick[0].Z,v,VXLBrush,ActiveSection.View[0].GetOrient);
                  // ActiveSection.BrushTool(LastClick[0].X,LastClick[0].Y,LastClick[0].Z,v,VXLBrush,ActiveSection.View[0].GetOrient);
                  CnvView[0].Repaint;
                  exit;
               end;

            if VXLTool = VXLTool_SmoothNormal then
               if VXLBrush <> 4 then
               begin
                  ActiveSection.GetVoxel(LastClick[0].X,LastClick[0].Y,LastClick[0].Z,v);
                  VXLSmoothBrushTool(ActiveSection,LastClick[0].X,LastClick[0].Y,LastClick[0].Z,v,VXLBrush,ActiveSection.View[0].GetOrient);
                  CnvView[0].Repaint;
                  exit;
               end;
//         end;
         exit;
      end;


      if ((ssAlt in shift) and (isLeftMouseDown)) or ((isLeftMouseDown) and (VXLTool = VXLTool_Dropper)) then
      begin
         TempI := GetPaletteColourFromVoxel(X,Y,0);
         if TempI > -1 then
            if SpectrumMode = ModeColours then
               SetActiveColor(TempI,True)
            else
               SetActiveNormal(TempI,True);
      end;

      if (ssCtrl in shift) and (isLeftMouseDown) then
      begin
         isLeftMouseDown := false;
         MoveCursor(LastClick[0].X,LastClick[0].Y,LastClick[0].Z,true);
         isLeftMouseDown := true;
      end;

      if (ssCtrl in Shift) or (ssAlt in shift) then Exit;

      if VXLTool = VXLTool_Brush then
      begin
         ActiveSection.GetVoxel(LastClick[0].X,LastClick[0].Y,LastClick[0].Z,v);
         if (SpectrumMode = ModeColours) or (v.Used=False) then
            v.Colour := ActiveColour;
         if (SpectrumMode = ModeNormals) or (v.Used=False) then
            v.Normal := ActiveNormal;

         v.Used := True;
         VXLBrushTool(ActiveSection,LastClick[0].X,LastClick[0].Y,LastClick[0].Z,v,VXLBrush,ActiveSection.View[0].GetOrient);
         RepaintViews;
         exit;
      end;

      if VXLTool = VXLTool_SmoothNormal then
      begin
         ActiveSection.GetVoxel(LastClick[0].X,LastClick[0].Y,LastClick[0].Z,v);
         VXLSmoothBrushTool(ActiveSection,LastClick[0].X,LastClick[0].Y,LastClick[0].Z,v,VXLBrush,ActiveSection.View[0].GetOrient);
         RepaintViews;
         exit;
      end;

      if VXLTool = VXLTool_Erase then
      begin
         v.Used := false;
         v.Colour := 0;
         v.Normal := 0;
         VXLBrushTool(ActiveSection,LastClick[0].X,LastClick[0].Y,LastClick[0].Z,v,VXLBrush,ActiveSection.View[0].GetOrient);

         RepaintViews;
         exit;
      end;

      if VXLTool = VXLTool_Line then
      begin
         V.Used := true;
         V.Colour := ActiveColour;
         V.Normal := ActiveNormal;
         drawstraightline(ActiveSection,TempView,LastClick[0],LastClick[1],V);
         RepaintViews;
         exit;
      end;

      if VXLTool = VXLTool_Rectangle then
      begin
         V.Used := true;
         V.Colour := ActiveColour;
         V.Normal := ActiveNormal;
         VXLRectangle(LastClick[0].X,LastClick[0].Y,LastClick[0].Z,LastClick[1].X,LastClick[1].Y,LastClick[1].Z,false,v);
         RepaintViews;
         exit;
      end;

      if VXLTool = VXLTool_FilledRectangle then
      begin
         V.Used := true;
         V.Colour := ActiveColour;
         V.Normal := ActiveNormal;
         VXLRectangle(LastClick[0].X,LastClick[0].Y,LastClick[0].Z,LastClick[1].X,LastClick[1].Y,LastClick[1].Z,true,v);
         RepaintViews;
         exit;
      end;
   end;
end;

procedure TFrmTimeMain.lblView1Click(Sender: TObject);
begin
   if not isEditable then exit;
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: lblView1Click');
   {$endif}
   ActivateView(1);
   setupscrollbars;
end;

procedure TFrmTimeMain.lblView2Click(Sender: TObject);
begin
   if not isEditable then exit;
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: lblView2Click');
   {$endif}
   ActivateView(2);
   setupscrollbars;
end;

procedure TFrmTimeMain.mnuDirectionPopupPopup(Sender: TObject);
var
   comp: TComponent;
   idx: Integer;
   View: TVoxelView;
begin
   if not isEditable then exit;

   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: MenuDirectionPopupPopup');
   {$endif}
   comp := mnuDirectionPopup.PopupComponent;
   mnuEdit.Visible := (comp <> lblView0); // can't edit as already editing it!
   if comp = lblView0 then
      View := ActiveSection.View[0]
   else if comp = lblView1 then
      View := ActiveSection.View[1]
   else
      View := ActiveSection.View[2];
   idx := (View.getViewNameIdx div 2) * 2;
   with mnuDirTowards do
   begin
      Caption := ViewName[idx];
      Enabled := not (View.getDir = dirTowards);
   end;
   with mnuDirAway do
   begin
      Caption := ViewName[idx+1];
      Enabled := not (View.getDir = dirAway);
   end;
end;

procedure TFrmTimeMain.mnuEditClick(Sender: TObject);
var comp: TComponent;
begin
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: MenuEditClick');
   {$endif}
   comp := mnuDirectionPopup.PopupComponent;
   if comp = lblView1 then
      ActivateView(1)
   else
      ActivateView(2);
end;

procedure TFrmTimeMain.mnuDirTowardsClick(Sender: TObject);
   procedure SetDir(WndIndex: Integer);
//   var idx: Integer;
   begin
      with ActiveSection.View[WndIndex] do
      begin
         setDir(dirTowards);
//         idx := getViewNameIdx;
         //lblView[WndIndex].Caption := ViewName[idx];
         SyncViews;
         Refresh;
         RepaintViews;
         cnvView[WndIndex].Repaint;
      end;
   end;
var
   i: Integer;
   // KnownComponent: Boolean;
   comp: TComponent;
begin
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: MenuDirTowardsClick');
   {$endif}
   comp := mnuDirectionPopup.PopupComponent;
   //KnownComponent := False;
   for i := 0 to 2 do
   begin
       if comp.Name = lblView[i].Name then
       begin
          SetDir(i);
          Break;
       end;
   end;
end;

procedure TFrmTimeMain.mnuDirAwayClick(Sender: TObject);
   procedure SetDir(WndIndex: Integer);
   begin
      with ActiveSection.View[WndIndex] do
      begin
         setDir(dirAway);
         SyncViews;
         Refresh;
         RepaintViews;
         cnvView[WndIndex].Repaint;
      end;
   end;
var
   i: Integer;
   comp: TComponent;
begin
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: MenuDirAwayClick');
   {$endif}
   comp := mnuDirectionPopup.PopupComponent;
   //  KnownComponent := False;
   for i := 0 to 2 do
   begin
      if comp.Name = lblView[i].Name then
      begin
         SetDir(i);
         //   KnownComponent := True;
         Break;
      end;
   end;
end;

Procedure TFrmTimeMain.SelectCorrectPalette2(Palette : String);
begin
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: SelectCorrectPalette2');
   {$endif}
   if Palette = 'TS' then
      iberianSunPalette1Click(nil)
   else if Palette = 'RA2' then
      RedAlert2Palette1Click(nil)
   else if fileexists(ExtractFileDir(ParamStr(0)) + '\palettes\USER\' + Palette) then
   begin
      LoadPaletteFromFile(ExtractFileDir(ParamStr(0)) + '\palettes\USER\' + Palette);
      cnvPalette.Repaint;
   end;
end;

Procedure TFrmTimeMain.SelectCorrectPalette;
begin
   if not Config.Palette then exit;
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: SelectCorrectPalette');
   {$endif}

   if VoxelFile.Section[0].Tailer.Unknown = 2 then
      SelectCorrectPalette2(Config.TS)
   else
   SelectCorrectPalette2(Config.RA2);
end;

Procedure TFrmTimeMain.CreateVxlError(v,n : Boolean);
var
   frm : tFrmVxlError;
begin
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: CreateVxlError');
   {$endif}
   frm:=TFrmVxlError.Create(Self);
   frm.Visible:=False;

   frm.Image1.Picture := TopBarImageHolder.Picture;
   frm.TabSheet1.TabVisible := v;
   frm.TabSheet2.TabVisible := n;

   frm.ShowModal;
   frm.Close;
   frm.Free;
end;

procedure TFrmTimeMain.DoAfterLoadingThings;
var
   v,n : boolean;
begin
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: DoAfterLoadingThings');
   {$endif}
   if VXLFilename = '' then
      changecaption(true,'Untitled')
   else
      changecaption(true,VXLFilename);

   v := not IsVoxelValid;
   n := HasNormalsBug;

   if v or n then
      CreateVxlError(v,n);

   SetupSections;
   SetViewMode(ModeEmphasiseDepth);
   SetSpectrum(SpectrumMode);
   SetActiveNormal(ActiveNormal,true);
   // 1.2b: Refresh Show Use Colours
   if UsedColoursOption then
      BuildUsedColoursArray;
   // End of 1.2b adition
   setupscrollbars;
   SetupStatusBar;
   CursorReset;
   ResetUndoRedo;
   UpdateUndo_RedoState;
   SelectCorrectPalette;
   PaintPalette(cnvPalette,True);
//   if not Display3dView1.Checked then
//      Application.OnIdle := Idle
//   else
//      Application.OnIdle := nil;

   RefreshAll;
end;

procedure TFrmTimeMain.Brush_1Click(Sender: TObject);
begin
   VXLBrush := 0;
end;

procedure TFrmTimeMain.Brush_2Click(Sender: TObject);
begin
   VXLBrush := 1;
end;

procedure TFrmTimeMain.Brush_3Click(Sender: TObject);
begin
   VXLBrush := 2;
end;

procedure TFrmTimeMain.Brush_4Click(Sender: TObject);
begin
   VXLBrush := 3;
end;

procedure TFrmTimeMain.Brush_5Click(Sender: TObject);
begin
   VXLBrush := 4;
end;

procedure TFrmTimeMain.SpeedButton3Click(Sender: TObject);
begin
   SetVXLTool(VXLTool_Brush);
end;

Procedure TFrmTimeMain.SetVXLTool(VXLTool_ : Integer);
begin
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: SetVXLTool');
   {$endif}
   SpeedButton3.Down := false;
   SpeedButton4.Down := false;
   SpeedButton5.Down := false;
   SpeedButton6.Down := false;
   SpeedButton7.Down := false;
   SpeedButton8.Down := false;
   SpeedButton9.Down := false;
   SpeedButton10.Down := false;
   SpeedButton11.Down := false;
   SpeedButton12.Down := false;

   if VXLTool_ = VXLTool_Brush then
      SpeedButton3.Down := true
   else if VXLTool_ = VXLTool_Line then
      SpeedButton4.Down := true
   else if VXLTool_ = VXLTool_Erase then
      SpeedButton5.Down := true
   else if VXLTool_ = VXLTool_FloodFill then
      SpeedButton10.Down := true
   else if VXLTool_ = VXLTool_FloodFillErase then
      SpeedButton13.Down := true
   else if VXLTool_ = VXLTool_Dropper then
      SpeedButton11.Down := true
   else if VXLTool_ = VXLTool_Rectangle then
      SpeedButton6.Down := true
   else if VXLTool_ = VXLTool_FilledRectangle then
      SpeedButton8.Down := true
   else if VXLTool_ = VXLTool_Darken then
      SpeedButton7.Down := true
   else if VXLTool_ = VXLTool_Lighten then
      SpeedButton12.Down := true
   else if VXLTool_ = VXLTool_SmoothNormal then
      SpeedButton9.Down := true;

   VXLTool := VXLTool_;
end;

procedure TFrmTimeMain.CnvView0MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
   TranslateClick2(0,X,Y,LastClick[0].X,LastClick[0].Y,LastClick[0].Z);
   if (LastClick[0].X < 0) or (LastClick[0].Y < 0) or (LastClick[0].Z < 0) then Exit;

   with ActiveSection.Tailer do
      if (LastClick[0].X > XSize-1) or (LastClick[0].Y > YSize-1) or (LastClick[0].Z > ZSize-1) then Exit;

   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: CnvView0MouseDown');
   {$endif}

   if (VXLTool = VXLTool_Line) or (VXLTool = VXLTool_FilledRectangle) or (VXLTool = VXLTool_Rectangle) then
   begin
      LastClick[1].X := LastClick[0].X;
      LastClick[1].Y := LastClick[0].Y;
      LastClick[1].Z := LastClick[0].Z;
   end;

   if button = mbleft then
   begin
      if TempView.Data_no > 0 then
      begin
         TempView.Data_no := 0;
         Setlength(TempView.Data,0);
      end;
      isLeftMouseDown := true;
      CnvView0MouseMove(sender,shift,x,y);
   end;
end;

procedure TFrmTimeMain.SpeedButton5Click(Sender: TObject);
begin
   SetVXLTool(VXLTool_Erase);
end;

procedure TFrmTimeMain.SpeedButton4Click(Sender: TObject);
begin
   SetVXLTool(VXLTool_Line);
end;

procedure TFrmTimeMain.SpeedButton10Click(Sender: TObject);
begin
   SetVXLTool(VXLTool_FloodFill);
end;

procedure TFrmTimeMain.SpeedButton11Click(Sender: TObject);
begin
   SetVXLTool(VXLTool_Dropper);
end;

procedure TFrmTimeMain.SpeedButton6Click(Sender: TObject);
begin
   SetVXLTool(VXLTool_Rectangle);
end;

procedure TFrmTimeMain.SpeedButton8Click(Sender: TObject);
begin
   SetVXLTool(VXLTool_FilledRectangle);
end;

procedure TFrmTimeMain.Save1Click(Sender: TObject);
begin
   if not isEditable then exit;
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: Save1Click');
   {$endif}
   if VXLFilename <> '' then
   begin
      if FileExists(VXLFilename) then
      begin
         // ShowBusyMessage('Saving...');
         VoxelFile.SaveToFile(VXLFilename);
         VXLChanged := false;
         changecaption(true,VXLFilename);
         // HideBusyMessage;
      end
      else
         SaveAs1Click(Sender);
   end;
end;

procedure TFrmTimeMain.Exit1Click(Sender: TObject);
begin
   Close;
end;

procedure TFrmTimeMain.SaveAs1Click(Sender: TObject);
begin
   if not isEditable then exit;

   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: SaveAs1Click');
   {$endif}
   if SaveVXLDialog.Execute then
   begin
      VoxelFile.SaveToFile(SaveVXLDialog.Filename);
      VXLFilename := SaveVXLDialog.Filename;
      VXLChanged := false;
      changecaption(true,VXLFilename);
      Config.AddFileToHistory(VXLFilename);
      UpdateHistoryMenu;
   end;
end;

procedure TFrmTimeMain.VoxelHeader1Click(Sender: TObject);
var
   FrmHeader: TFrmHeader;
begin
   if not isEditable then exit;

   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: VoxelHeader1Click');
   {$endif}
   FrmHeader:=TFrmHeader.Create(Self);
   with FrmHeader do
   begin
      SetValues(@VoxelFile);
      PageControl1.ActivePage := PageControl1.Pages[1];
      Image2.Picture := TopBarImageHolder.Picture;
      ShowModal;
      Free;
   end;
end;

procedure TFrmTimeMain.N6Click(Sender: TObject);
var
  FrmHeader: TFrmHeader;
begin
   if not isEditable then exit;

   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: N6Click');
   {$endif}
   FrmHeader:=TFrmHeader.Create(Self);
   with FrmHeader do
   begin
      SetValues(@VoxelFile);
      PageControl1.ActivePage := PageControl1.Pages[0];
      Image2.Picture := TopBarImageHolder.Picture;
      ShowModal;
      Free;
   end;
end;

Procedure TFrmTimeMain.UpdateUndo_RedoState;
begin
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: UpdateUndo_RedoState');
   {$endif}
//   mnuBarUndo.Enabled := IsUndoRedoUsed(Undo);
//   mnuBarRedo.Enabled := IsUndoRedoUsed(Redo);

//   Undo1.Enabled := mnuBarUndo.Enabled;
//   Redo1.Enabled := mnuBarRedo.Enabled;
end;

procedure TFrmTimeMain.mnuBarUndoClick(Sender: TObject);
begin
   if not isEditable then exit;

   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: MenuBarUndoClick');
   {$endif}
   UndoRestorePoint(Undo,Redo);
   UpdateUndo_RedoState;
   RefreshAll;
   VXLChanged := true;
end;

procedure TFrmTimeMain.mnuBarRedoClick(Sender: TObject);
begin
   if not isEditable then exit;

   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: MenuBarRedoClick');
   {$endif}
   RedoRestorePoint(Undo,Redo);
   UpdateUndo_RedoState;
   RefreshAll;
   VXLChanged := true;
end;

procedure TFrmTimeMain.updatenormals1Click(Sender: TObject);
begin
   if not isEditable then exit;

   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: UpdateNormals1Click');
   {$endif}
   // ask the user to confirm
   if MessageDlg('Autonormals v1.1' +#13#13+
        'This process will modify the voxel''s normals.' +#13+
        'If you choose to do this, you should first save' + #13 +
        'your model under a different name as a backup.',
        mtWarning,mbOKCancel,0) = mrCancel then
                        Exit;
   //ResetUndoRedo;
   CreateVXLRestorePoint(ActiveSection,Undo);
   UpdateUndo_RedoState;

   if ApplyNormalsToVXL(ActiveSection) > 0 then
      if MessageDlg('Some were Confused, This may mean there are redundant voxels.'+#13#13+'Run Remove Redundant Voxels?',mtConfirmation,[mbYes,mbNo],0) = mrYes then
         RemoveRedundentVoxels1Click(Sender);

   Refreshall;
   VXLChanged := true;
end;

procedure TFrmTimeMain.CubedAutoNormals1Click(Sender: TObject);
var
   FrmAutoNormals : TFrmAutoNormals;
begin
   if not isEditable then exit;

   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: CubedAutoNormals1Click');
   {$endif}
   // One AutoNormals to rule them all!
   FrmAutoNormals := TFrmAutoNormals.Create(self);
   FrmAutoNormals.MyVoxel := ActiveSection;
   FrmAutoNormals.ShowModal;
   FrmAutoNormals.Release;
   // Old code
{
   // ask the user to confirm
   if MessageDlg('Autonormals v5.2' +#13#13+
        'This process will modify the voxel''s normals.' +#13+
        'If you choose to do this, you should first save' + #13 +
        'your model under a different name as a backup.',
        mtWarning,mbOKCancel,0) = mrCancel then
                        Exit;
   //ResetUndoRedo;
   CreateVXLRestorePoint(ActiveSection,Undo);
   UpdateUndo_RedoState;

   ApplyCubedNormalsToVXL(ActiveSection);
   Refreshall;
   VXLChanged := true;
}
end;

procedure TFrmTimeMain.Delete1Click(Sender: TObject);
var
  SectionIndex,i: Integer;
begin
   if not isEditable then exit;

   if VoxelFile.Header.NumSections<2 then
   begin
      MessageDlg('Can''t delete if there''s only 1 section!',mtWarning,[mbOK],0);
      Exit;
   end;

   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: Delete1Click');
   {$endif}
   // ask the user to confirm
   if MessageDlg('Delete Section' +#13#13+
        'This process will delete the current section.' +#13+
        'This cannot be undone. If you choose to do this, you should first save' + #13 +
        'your model under a different name as a backup.',
        mtWarning,mbOKCancel,0) = mrCancel then
                        Exit;

   //we can't undo these actions!!!
   ResetUndoRedo;
   UpdateUndo_RedoState;
   SetisEditable(False);

   SectionIndex:=ActiveSection.Header.Number;
   VoxelFile.RemoveSection(SectionIndex);

   SectionCombo.Items.Clear;
   for i:=0 to VoxelFile.Header.NumSections-1 do
   begin
      SectionCombo.Items.Add(VoxelFile.Section[i].Name);
   end;
   SectionCombo.ItemIndex:=0;
   SectionComboChange(Self);
   SetisEditable(True);
   VXLChanged := true;
   Refreshall;
end;

procedure TFrmTimeMain.normalsaphere1Click(Sender: TObject);
begin
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: NormalSphere1Click');
   {$endif}
   Update3dViewWithNormals(ActiveSection);
end;

procedure TFrmTimeMain.RemoveRedundentVoxels1Click(Sender: TObject);
var no{, i}: integer;
   label Done;
begin
   if not isEditable then exit;

   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: RemoveRedundantVoxels1Click');
   {$endif}
   // ensure the user wants to do this!
   if MessageDlg('Remove Redundent Voxels v1.0' +#13#13+
        'This process will remove voxels that are hidden from view.' +#13+
        'If you choose to do this, you should first save' + #13 +
        'your model under a different name as a backup.',
        mtWarning,mbOKCancel,0) = mrCancel then exit;
   // stop undo's
//     ResetUndoRedo;
   CreateVXLRestorePoint(ActiveSection,Undo);
   UpdateUndo_RedoState;

   // ok, do it
   no := RemoveRedundantVoxelsFromVXL(ActiveSection);
   if no = 0 then
      ShowMessage('Remove Redundent Voxels v1.0' +#13#13+ 'Removed: 0')
   else
   begin
      ShowMessage('Remove Redundent Voxels v1.0' +#13#13+ 'Removed: ' + IntToStr(no));
      RefreshAll;
      VXLChanged := true;
   end;
end;

procedure TFrmTimeMain.ClearEntireSection1Click(Sender: TObject);
begin
   if not isEditable then exit;

   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: ClearEntireSection1Click');
   {$endif}
   if MessageDlg('Clear Section' +#13#13+
        'This process will remove all voxels from the current section.' +#13+
        'If you choose to do this, you should first save' + #13 +
        'your model under a different name as a backup.',
        mtWarning,mbOKCancel,0) = mrCancel then exit;

   CreateVXLRestorePoint(ActiveSection,Undo);
   UpdateUndo_RedoState;
   ActiveSection.Clear;
   VXLChanged := true;
   RefreshAll;
end;

procedure TFrmTimeMain.VXLSEHelp1Click(Sender: TObject);
begin
   if not fileexists(extractfiledir(paramstr(0))+'/help.chm') then
   begin
      messagebox(0,'VXLSE Help' + #13#13 + 'help.chm not found','VXLSE Help',0);
      exit;
   end;
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: VXLSEHelp1Click');
   {$endif}
   RunAProgram('help.chm','',extractfiledir(paramstr(0)));
end;

procedure TFrmTimeMain.About1Click(Sender: TObject);
var
   frm: TLoadFrm;
begin
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: About1Click');
   {$endif}
   frm:=TLoadFrm.Create(Self);
   frm.Visible:=False;
   if testbuild then
      frm.Label4.Caption := APPLICATION_VER + ' TB '+testbuildversion
   else
      frm.Label4.Caption := APPLICATION_VER;
   frm.Image2.Visible := false;
   frm.LblWill.Left := 117;
   frm.butOK.Visible:=True;
   frm.Loading.Caption:='';
   frm.ShowModal;
   frm.Close;
   frm.Free;
end;

procedure TFrmTimeMain.FlipXswitchFrontBack1Click(Sender: TObject);
begin
   //Create a transformation matrix...
   CreateVXLRestorePoint(ActiveSection,Undo);
   UpdateUndo_RedoState;

   ActiveSection.FlipMatrix([-1,1,1],[1,0,0]);
   RefreshAll;
   VXLChanged := true;
end;

procedure TFrmTimeMain.FlipYswitchRightLeft1Click(Sender: TObject);
begin
   //Create a transformation matrix...
   CreateVXLRestorePoint(ActiveSection,Undo);
   UpdateUndo_RedoState;

   ActiveSection.FlipMatrix([1,-1,1],[0,1,0]);
   RefreshAll;
   VXLChanged := true;
end;

procedure TFrmTimeMain.FlipZswitchTopBottom1Click(Sender: TObject);
begin
   //Create a transformation matrix...
   CreateVXLRestorePoint(ActiveSection,Undo);
   UpdateUndo_RedoState;

   ActiveSection.FlipMatrix([1,1,-1],[0,0,1]);
   RefreshAll;
   VXLChanged := true;
end;

procedure TFrmTimeMain.MirrorBottomToTop1Click(Sender: TObject);
var
   FlipFirst: Boolean;
begin
   FlipFirst:=False;
   if (Sender.ClassNameIs('TMenuItem')) then
   begin
      if CompareStr((Sender as TMenuItem).Name,'MirrorTopToBottom1')=0 then
      begin
         //flip first!
         FlipFirst:=True;
      end;
  end;

  CreateVXLRestorePoint(ActiveSection,Undo);
  UpdateUndo_RedoState;

  //Based on the current view...
  case ActiveSection.View[0].GetViewNameIdx of
    0:
    begin
      if not FlipFirst then ActiveSection.FlipMatrix([1,1,-1],[0,0,1]);
      ActiveSection.Mirror(oriZ);
    end;
    1:
    begin
      if not FlipFirst then ActiveSection.FlipMatrix([1,1,-1],[0,0,1]);
      ActiveSection.Mirror(oriZ);
    end;
    2:
    begin
      if not FlipFirst then ActiveSection.FlipMatrix([1,1,-1],[0,0,1]);
      ActiveSection.Mirror(oriZ);
    end;
    3:
    begin
      if not FlipFirst then ActiveSection.FlipMatrix([1,1,-1],[0,0,1]);
      ActiveSection.Mirror(oriZ);
    end;
    4:
    begin
      if not FlipFirst then ActiveSection.FlipMatrix([1,-1,1],[0,1,0]);
      ActiveSection.Mirror(oriY);
    end;
    5:
    begin
      if not FlipFirst then ActiveSection.FlipMatrix([1,-1,1],[0,1,0]);
      ActiveSection.Mirror(oriY);
    end;
  end;

  RefreshAll;
  VXLChanged := true;
end;

procedure TFrmTimeMain.MirrorLeftToRight1Click(Sender: TObject);
var
  FlipFirst: Boolean;
begin
  FlipFirst:=False;
  if (Sender.ClassNameIs('TMenuItem')) then begin
    if CompareStr((Sender as TMenuItem).Name,'MirrorLeftToRight1')=0 then begin
      //flip first!
      FlipFirst:=True;
//      ActiveSection.FlipMatrix([1,-1,1],[0,1,0]);
    end;
  end;

  CreateVXLRestorePoint(ActiveSection,Undo);
  UpdateUndo_RedoState;

  //Based on the current view...
  case ActiveSection.View[0].GetViewNameIdx of
    0:
    begin
      if FlipFirst then ActiveSection.FlipMatrix([1,-1,1],[0,1,0]);
      ActiveSection.Mirror(oriY);
    end;
    1:
    begin
      //reverse here :) (reversed view, that's why!)
      if not FlipFirst then ActiveSection.FlipMatrix([1,-1,1],[0,1,0]);
      ActiveSection.Mirror(oriY);
    end;
    2:
    begin
      if FlipFirst then ActiveSection.FlipMatrix([-1,1,1],[1,0,0]);
      ActiveSection.Mirror(oriX);
    end;
    3:
    begin
      if not FlipFirst then ActiveSection.FlipMatrix([-1,1,1],[1,0,0]);
      ActiveSection.Mirror(oriX);
    end;
    4:
    begin
      if FlipFirst then ActiveSection.FlipMatrix([-1,1,1],[1,0,0]);
      ActiveSection.Mirror(oriX);
    end;
    5:
    begin
      if not FlipFirst then ActiveSection.FlipMatrix([-1,1,1],[1,0,0]);
      ActiveSection.Mirror(oriX);
    end;
  end;
  RefreshAll;
  VXLChanged := true;
end;

procedure TFrmTimeMain.MirrorBackToFront1Click(Sender: TObject);
begin
CreateVXLRestorePoint(ActiveSection,Undo);
UpdateUndo_RedoState;

FlipXswitchFrontBack1Click(Sender);
ActiveSection.Mirror(oriX);
RefreshAll;
VXLChanged := true;
end;

procedure TFrmTimeMain.MirrorFrontToBack1Click(Sender: TObject);
begin
CreateVXLRestorePoint(ActiveSection,Undo);
UpdateUndo_RedoState;

ActiveSection.Mirror(oriX);
RefreshAll;
VXLChanged := true;
end;

procedure TFrmTimeMain.Nudge1Left1Click(Sender: TObject);
var
  i: Integer;
  NR: Array[0..2] of Single;
begin
   NR[0]:=0; NR[1]:=0; NR[2]:=0;
   if (Sender.ClassNameIs('TMenuItem')) then
   begin
      if (CompareStr((Sender as TMenuItem).Name,'Nudge1Left1')=0) or (CompareStr((Sender as TMenuItem).Name,'Nudge1Right1')=0) then
      begin
         //left and right
         case ActiveSection.View[0].GetViewNameIdx of
            0: NR[1]:=-1;
            1: NR[1]:=1;
            2: NR[0]:=-1;
            3: NR[0]:=1;
            4: NR[0]:=-1;
            5: NR[0]:=1;
         end;
         if CompareStr((Sender as TMenuItem).Name,'Nudge1Right1')=0 then
         begin
            for i:=0 to 2 do
            begin
               NR[i]:=-NR[i];
            end;
         end;
      end
      else
      begin
         //up and down
         case ActiveSection.View[0].GetViewNameIdx of
            0: NR[2]:=-1;
            1: NR[2]:=-1;
            2: NR[2]:=-1;
            3: NR[2]:=-1;
            4: NR[1]:=-1;
            5: NR[1]:=-1;
         end;
         if CompareStr((Sender as TMenuItem).Name,'Nudge1up1')=0 then
         begin
            for i:=0 to 2 do
            begin
               NR[i]:=-NR[i];
            end;
         end;
      end;
   end;

   CreateVXLRestorePoint(ActiveSection,Undo);
   UpdateUndo_RedoState;

   ActiveSection.FlipMatrix([1,1,1],NR,False);

   RefreshAll;
   VXLChanged := true;
end;

procedure TFrmTimeMain.Section2Click(Sender: TObject);
var
  i, SectionIndex: Integer;
  FrmNewSectionSize: TFrmNewSectionSize;
begin
   if not isEditable then exit;

   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: Section2Click');
   {$endif}
   FrmNewSectionSize:=TFrmNewSectionSize.Create(Self);
   with FrmNewSectionSize do
   begin
      ShowModal;
      if aborted then Exit;
      if MessageDlg('Create Section' + #13#13+ 'Are you sure you want to create a section: ' + #13#13 +
             'Name: ' + AsciizToStr(Name,16) + Chr(10) +
             'Size: ' + IntToStr(X) + 'x' + IntToStr(Y) + 'x' + IntToStr(Z),
             mtConfirmation,[mbYes,mbNo],0) = mrNo then
         exit;

      SetisEditable(False);

      SectionIndex:=ActiveSection.Header.Number;
      if not before then //after
         Inc(SectionIndex);


      VoxelFile.InsertSection(SectionIndex,Name,X,Y,Z);

      SectionCombo.Items.Clear;
      for i:=0 to VoxelFile.Header.NumSections-1 do
      begin
         SectionCombo.Items.Add(VoxelFile.Section[i].Name);
      end;

      VoxelFile.Section[SectionIndex].Tailer.Unknown := VoxelFile.Section[0].Tailer.Unknown;

      //MajorRepaint;
      SectionCombo.ItemIndex:=SectionIndex;
      SectionComboChange(Self);

      ResetUndoRedo;
      SetisEditable(True);
      VXLChanged := true;
   end;
   FrmNewSectionSize.Free;
end;

procedure TFrmTimeMain.Copyofthissection1Click(Sender: TObject);
var
   i, SectionIndex,x,y,z : Integer;
begin
   if not isEditable then exit;

   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: CopyOfThisSection1Click');
   {$endif}
   if MessageDlg('Copy Section' + #13#13+ 'Are you sure you want to make a copy of the current section?',
             mtConfirmation,[mbYes,mbNo],0) = mrNo then
      exit;

   SectionIndex:=ActiveSection.Header.Number;
   Inc(SectionIndex);

   ResetUndoRedo;
   UpdateUndo_RedoState;

   VoxelFile.InsertSection(SectionIndex,'Copy Of '+VoxelFile.Section[SectionIndex-1].Name,VoxelFile.Section[SectionIndex-1].Tailer.XSize,VoxelFile.Section[SectionIndex-1].Tailer.YSize,VoxelFile.Section[SectionIndex-1].Tailer.ZSize);

   for x := 0 to (VoxelFile.Section[SectionIndex-1].Tailer.XSize - 1) do
      for y := 0 to (VoxelFile.Section[SectionIndex-1].Tailer.YSize - 1) do
         for z := 0 to (VoxelFile.Section[SectionIndex-1].Tailer.ZSize - 1) do
            VoxelFile.Section[SectionIndex].Data[x,y,z] := VoxelFile.Section[SectionIndex-1].Data[x,y,z];

   with VoxelFile.Section[SectionIndex-1].Tailer do
   begin
      VoxelFile.Section[SectionIndex].Tailer.Det := Det;
      for x := 1 to 3 do
      begin
         VoxelFile.Section[SectionIndex].Tailer.MaxBounds[x] := MaxBounds[x];
         VoxelFile.Section[SectionIndex].Tailer.MinBounds[x] := MinBounds[x];
      end;
      VoxelFile.Section[SectionIndex].Tailer.SpanDataOfs := SpanDataOfs;
      VoxelFile.Section[SectionIndex].Tailer.SpanEndOfs := SpanEndOfs;
      VoxelFile.Section[SectionIndex].Tailer.SpanStartOfs := SpanStartOfs;
      for x := 1 to 3 do
         for y := 1 to 4 do
            VoxelFile.Section[SectionIndex].Tailer.Transform[x,y] := Transform[x,y];

      VoxelFile.Section[SectionIndex].Tailer.Unknown := Unknown;
   end;

   SectionCombo.Items.Clear;
   for i:=0 to VoxelFile.Header.NumSections-1 do
   begin
      SectionCombo.Items.Add(VoxelFile.Section[i].Name);
   end;

   //MajorRepaint;
   SectionCombo.ItemIndex:=SectionIndex;
   SectionComboChange(Self);
   VXLChanged := true;
end;

procedure TFrmTimeMain.mnuHistoryClick(Sender: TObject);
var
   p: ^TMenuItem;
   s : string;
begin
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: MenuHistoryClick');
   {$endif}
   if Sender.ClassNameIs('TMenuItem') then
   begin //check to see if it is this class
      //and now do some dirty things with pointers...
      p:=@Sender;
      s := Config.GetHistory(p^.Tag);
      CheckVXLChanged;

      if not fileexists(s) then
      begin
         Messagebox(0,'File Doesn''t Exist','Load Voxel',0);
         exit;
      end;

      IsVXLLoading := true;
      SetIsEditable(LoadVoxel(Config.GetHistory(p^.Tag)));
      if IsEditable then
         DoAfterLoadingThings;
      IsVXLLoading := false;
   end;
end;

procedure TFrmTimeMain.BuildReopenMenu;
var
   i : integer;
begin
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: BuildReopenMenu');
   {$endif}
   Config:=TConfiguration.Create;
   for i:=HistoryDepth - 1 downto 0 do
   begin
      mnuHistory[i]:=TMenuItem.Create(Self);
      mnuHistory[i].OnClick:=mnuHistoryClick;
      mnuReOpen.Insert(0,mnuHistory[i]);
   end;
   UpdateHistoryMenu;
end;

procedure TFrmTimeMain.BarReopenClick(Sender: TObject);
begin
   //ReOpen1.Caption := Config.GetHistory(0);
end;

procedure TFrmTimeMain.iberianSunPalette1Click(Sender: TObject);
begin
   LoadPaletteFromFile(ExtractFileDir(ParamStr(0)) + '\palettes\TS\unittem.pal');
   cnvPalette.Repaint;
end;

procedure TFrmTimeMain.RedAlert2Palette1Click(Sender: TObject);
begin
   LoadPaletteFromFile(ExtractFileDir(ParamStr(0)) + '\palettes\RA2\unittem.pal');
   cnvPalette.Repaint;
end;

Procedure TFrmTimeMain.LoadPalettes;
var
   f: TSearchRec;
   path: String;
   item: TMenuItem;
begin
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: LoadPalettes');
   {$endif}
   // prepare
   PaletteList.Data_no := 0;
   SetLength(PaletteList.Data,PaletteList.Data_no);
   Custom1.Visible := false;

   path := Concat(ExtractFilePath(ParamStr(0)) + '\palettes\USER\','*.pal');
   // find files
   if FindFirst(path,faAnyFile,f) = 0 then
   repeat
      SetLength(PaletteList.Data,PaletteList.Data_no+1);

      item := TMenuItem.Create(Owner);
      item.Caption := copy(f.Name,1,length(f.Name)-length(extractfileext(f.Name)));
      PaletteList.Data[PaletteList.Data_no] := ExtractFilePath(ParamStr(0)) + '\palettes\USER\' + f.Name;
      item.Tag := PaletteList.Data_no; // so we know which it is
      item.OnClick := blank1Click;

      Custom1.Insert(PaletteList.Data_no,item);
      Custom1.Visible := true;
      N18.Visible := true;
      inc(PaletteList.Data_no);
   until FindNext(f) <> 0;
   FindClose(f);
end;

procedure TFrmTimeMain.blank1Click(Sender: TObject);
begin
   LoadPaletteFromFile(PaletteList.Data[TMenuItem(Sender).tag]);
   cnvPalette.Repaint;
end;

Procedure TFrmTimeMain.CheckVXLChanged;
var
   T : string;
begin
   if VXLChanged then
   begin
      {$ifdef DEBUG_FILE}
      DebugFile.Add('FrmTimeMain: CheckVXLChanged');
      {$endif}
      T := ExtractFileName(VXLFilename);
      if t = '' then
         T := 'Save Untitled'
      else
         T := 'Save changes in ' + T;
      if MessageDlg('Last changes not saved. ' + T +' ?',mtWarning,[mbYes,mbNo],0) = mrYes then begin
//         Save1.Click;
   end;
end;

end;

procedure TFrmTimeMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   CheckVXLChanged;
   if p_Frm3DPreview <> nil then
   begin
      try
         p_Frm3DPreview^.Release;
      except;
      end;
      p_Frm3DPreview := nil;
   end;
end;

procedure TFrmTimeMain.SpeedButton7Click(Sender: TObject);
begin
   SetVXLTool(VXLTool_Darken);
end;

procedure TFrmTimeMain.SpeedButton12Click(Sender: TObject);
begin
   SetVXLTool(VXLTool_Lighten);
end;

procedure TFrmTimeMain.SpeedButton13Click(Sender: TObject);
begin
   SetVXLTool(VXLTool_FloodFillErase);
end;

procedure TFrmTimeMain.ToolButton11Click(Sender: TObject);
var
   x,y,z : integer;
   v : tvoxelunpacked;
begin
   if not isEditable then exit;

   if MessageDlg('Clear Voxel Colour' +#13#13+
        'This process will clear the voxels colours/normals with the currently selected colour.' +#13+
        'If you choose to do this, you should first save' + #13 +
        'your model under a different name as a backup.',
        mtWarning,mbOKCancel,0) = mrCancel then exit;

   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: ToolButton1Click');
   {$endif}
   CreateVXLRestorePoint(ActiveSection,Undo);
   UpdateUndo_RedoState;

   for x := 0 to ActiveSection.Tailer.XSize-1 do
      for y := 0 to ActiveSection.Tailer.YSize-1 do
         for z := 0 to ActiveSection.Tailer.ZSize-1 do
         begin
            ActiveSection.GetVoxel(x,y,z,v);
            if SpectrumMode = ModeColours then
               v.Colour := ActiveColour
            else
               v.Normal := ActiveNormal;
            ActiveSection.SetVoxel(x,y,z,v);
         end;
   RefreshAll;
   VXLChanged := true;
end;

procedure TFrmTimeMain.ClearLayer1Click(Sender: TObject);
begin
   if not isEditable then exit;

   if MessageDlg('Clear Layer' +#13#13+
        'This process will remove all voxels from the current layer.' +#13+
        'If you choose to do this, you should first save' + #13 +
        'your model under a different name as a backup.',
        mtWarning,mbOKCancel,0) = mrCancel then exit;

   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: ClearLayer1Click');
   {$endif}
   ClearVXLLayer(ActiveSection);

   UpdateUndo_RedoState;
   RefreshAll;
   VXLChanged := true;
end;

procedure TFrmTimeMain.Copy1Click(Sender: TObject);
begin
   if not isEditable then exit;

   VXLCopyToClipboard(ActiveSection);
end;

procedure TFrmTimeMain.Cut1Click(Sender: TObject);
begin
   if not isEditable then exit;

   VXLCutToClipboard(ActiveSection);
   UpdateUndo_RedoState;
   RefreshAll;
   VXLChanged := true;
end;

procedure TFrmTimeMain.ClearUndoSystem1Click(Sender: TObject);
begin
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: ClearUndoSystem1Click');
   {$endif}
   ResetUndoRedo;
   UpdateUndo_RedoState;
end;

procedure TFrmTimeMain.PasteFull1Click(Sender: TObject);
begin
   if not isEditable then exit;

   CreateVXLRestorePoint(ActiveSection,Undo);
   UpdateUndo_RedoState;

   PasteFullVXL(ActiveSection);
   RefreshAll;
   VXLChanged := true;
end;

procedure TFrmTimeMain.Paste1Click(Sender: TObject);
begin
   if not isEditable then exit;

   CreateVXLRestorePoint(ActiveSection,Undo);
   UpdateUndo_RedoState;

   // --- 1.2: Removed
   // PasteFullVXL(ActiveSection);
   // --- Replaced with
   PasteVXL(ActiveSection);
   RefreshAll;
   VXLChanged := true;
end;

procedure firstlastword(const words : string; var first,rest : string);
var
   x,w : integer;
   seps : array[0..50] of string;
   endofword : boolean;
   text : string;
begin
   text := words;
   seps[0] := ' ';
   seps[1] := '(';
   seps[2] := ')';
   seps[3] := '[';
   seps[4] := #09;
   seps[5] := ']';
   seps[6] := ':';
   seps[7] := '''';
   seps[8] := '"';
   seps[9] := '=';
   seps[10] := ',';
   seps[11] := ';';
   repeat
      w := 0;
      endofword:=false;
      for x := 1 to length(text) do
         if endofword=false then
            if (copy(text,x,1) <> seps[0]) and (copy(text,x,1) <> seps[1])and (copy(text,x,1) <> seps[2]) and (copy(text,x,1) <> seps[3]) and (copy(text,x,1) <> seps[4]) and (copy(text,x,1) <> seps[5]) and (copy(text,x,1) <> seps[6]) and (copy(text,x,1) <> seps[7]) and (copy(text,x,1) <> seps[8]) and (copy(text,x,1) <> seps[9]) and (copy(text,x,1) <> seps[10]) and (copy(text,x,1) <> seps[11]) then
               w := w + 1
            else
               endofword := true;

      if w = 0 then
         text := copy(text,2,length(text));
   until (w > 0) or (length(text) = 0);
   first := copy(text,1,w);
   rest := copy(text,w+1,length(text));
end;

procedure firstlastword2(const words : string; var first,rest : string);
var
   x,w : integer;
   seps : array[0..50] of string;
   endofword : boolean;
   text : string;
begin
   text := words;
   seps[0] := #09;
   seps[1] := ';';
   seps[2] := '=';
   repeat
      w := 0;
      endofword:=false;
      for x := 1 to length(text) do
         if endofword=false then
            if (copy(text,x,1) <> seps[0]) and (copy(text,x,1) <> seps[1]) and (copy(text,x,1) <> seps[2]) then
               w := w + 1
            else
               endofword := true;

      if w = 0 then
         text := copy(text,2,length(text));
   until (w > 0) or (length(text) = 0);
   first := copy(text,1,w);
   rest := copy(text,w+1,length(text));
end;

function searchcscheme(s : tstringlist; f : string) : string;
var
   x : integer;
   first,rest : string;
begin
   result := '!ERROR!';

   for x := 0 to s.Count-1 do
      if ansilowercase(copy(s.Strings[x],1,length(f))) = ansilowercase(f) then
      begin
         firstlastword(s.Strings[x],first,rest);
         if f = 'name=' then
            firstlastword2(rest,first,rest)
         else
            firstlastword(rest,first,rest);
         result := first;
      end;
end;

procedure TFrmTimeMain.About2Click(Sender: TObject);
var
   frm: TFrmPalettePackAbout;
begin
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: About2Click');
   {$endif}
   frm:=TFrmPalettePackAbout.Create(Self);
   frm.Visible:=False;
   frm.ShowModal;
   frm.Close;
   frm.Free;
end;

procedure TFrmTimeMain.EmptyVoxel1Click(Sender: TObject);
begin
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: EmptyVoxel1Click');
   {$endif}
   NewVFile(2);
end;

procedure TFrmTimeMain.EmptyVoxel2Click(Sender: TObject);
begin
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: EmptyVoxel2Click');
   {$endif}
   NewVFile(4);
end;

Procedure TFrmTimeMain.NewVFile(Game : integer);
var
   FrmNew: TFrmNew;
begin
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: NewVFile');
   {$endif}
   IsVXLLoading := true;
   Application.OnIdle := nil;
   CheckVXLChanged;

   FrmNew:=TFrmNew.Create(Self);
   with FrmNew do
   begin
      //FrmNew.Caption := ' New Voxel File';
      Label9.Caption := 'New Voxel';
      Label9.Caption := 'Enter the size you want the voxel to be';
      //grpCurrentSize.Left := 400;
      grpCurrentSize.Visible := false;
      //grpNewSize.Width := 201;
      grpNewSize.Left := (FrmNew.Width div 2) - (grpNewSize.Width div 2);
      grpNewSize.Caption := 'Size';
      Button2.Visible := false;
      Button4.Left := Button2.Left;
      Image1.Picture := TopBarImageHolder.Picture;
      //grpCurrentSize.Visible := true;
      grpVoxelType.Visible := true;

      x := 10;//ActiveSection.Tailer.XSize;
      y := 10;//ActiveSection.Tailer.YSize;
      z := 10;//ActiveSection.Tailer.ZSize;
      ShowModal;
   end;

   // 1.3: Before creating the new voxel, we add the type support.
   if FrmNew.rbLand.Checked then
      VoxelType := vtLand
   else
      VoxelType := vtAir;

   SetIsEditable(NewVoxel(Game,FrmNew.x,FrmNew.y,FrmNew.z));

   if IsEditable then
      DoAfterLoadingThings;

   IsVXLLoading := false;
end;

Procedure TFrmTimeMain.SetCursor;
begin
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: SetCursor');
   {$endif}
   //CnvView[0].Cursor := Mouse_Current;
   if VXLTool = VXLTool_Brush then
      if VXLBrush = 4 then
         Mouse_Current := MouseSpray
      else
         Mouse_Current := MouseDraw
   else if VXLTool = VXLTool_Line then
      Mouse_Current := MouseLine
   else if VXLTool = VXLTool_Erase then
      if VXLBrush = 4 then
         Mouse_Current := MouseSpray
      else
         Mouse_Current := MouseDraw
   else if VXLTool = VXLTool_FloodFill then
      Mouse_Current := MouseFill
   else if VXLTool = VXLTool_FloodFillErase then
      Mouse_Current := MouseFill
   else if VXLTool = VXLTool_Dropper then
      Mouse_Current := MouseDropper
   else if VXLTool = VXLTool_Rectangle then
      Mouse_Current := MouseLine
   else if VXLTool = VXLTool_FilledRectangle then
      Mouse_Current := MouseLine
   else if VXLTool = VXLTool_Darken then
      if VXLBrush = 4 then
         Mouse_Current := MouseSpray
      else
         Mouse_Current := MouseDraw
   else if VXLTool = VXLTool_Lighten then
      if VXLBrush = 4 then
         Mouse_Current := MouseSpray
      else
         Mouse_Current := MouseDraw;

   if not iseditable then
      Mouse_Current := crDefault;
end;

procedure TFrmTimeMain.SmoothNormals1Click(Sender: TObject);
begin
   if not isEditable then exit;

   // ask the user to confirm
   if MessageDlg('Smooth Normals v1.0' +#13#13+
        'This process will modify the voxel''s normals.' +#13+
        'If you choose to do this, you should first save' + #13 +
        'your model under a different name as a backup.',
        mtWarning,mbOKCancel,0) = mrCancel then
      Exit;
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: SmoothNormals1Click');
   {$endif}
   CreateVXLRestorePoint(ActiveSection,Undo);
   UpdateUndo_RedoState;
   SmoothVXLNormals(ActiveSection);
   RefreshAll;
   VXLChanged := true;
end;

procedure TFrmTimeMain.VoxelTexture1Click(Sender: TObject);
var
   frm: TFrmVoxelTexture;
begin
   if not isEditable then exit;
   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: VoxelTexture1Click');
   {$endif}
   frm:=TFrmVoxelTexture.Create(Self);
   frm.Visible:=False;
   frm.Image3.Picture := TopBarImageHolder.Picture;
   frm.ShowModal;
   frm.Close;
   frm.Free;
end;

procedure TFrmTimeMain.OpenHyperlink(HyperLink: PChar);
begin
  ShellExecute(Application.Handle,nil,HyperLink,'','',SW_SHOWNORMAL);
end;

procedure TFrmTimeMain.CnCSource1Click(Sender: TObject);
begin
   OpenHyperLink('http://www.cnc-source.com/');
end;

procedure TFrmTimeMain.PPMForUpdates1Click(Sender: TObject);
begin
   OpenHyperLink('http://www.ppmsite.com/');
end;

procedure TFrmTimeMain.CGen1Click(Sender: TObject);
begin
   OpenHyperLink('http://cannis.net/');
end;

procedure TFrmTimeMain.Dezire1Click(Sender: TObject);
begin
   OpenHyperLink('http://www.deezire.net/');
end;

procedure TFrmTimeMain.PlanetCNC1Click(Sender: TObject);
begin
   OpenHyperLink('http://www.planetcnc.com/');
end;

procedure TFrmTimeMain.PixelOps1Click(Sender: TObject);
begin
   OpenHyperLink('http://cannis.net/pixelops/');
end;

procedure TFrmTimeMain.ESource1Click(Sender: TObject);
begin
   OpenHyperLink('http://cnc.raminator.de/');
end;

procedure TFrmTimeMain.SavageWarTS1Click(Sender: TObject);
begin
   OpenHyperLink('http://ts.savagewar.co.uk/');
end;

procedure TFrmTimeMain.SavageWarRA21Click(Sender: TObject);
begin
   OpenHyperLink('http://ra2.savagewar.co.uk/');
end;

procedure TFrmTimeMain.YRArgentina1Click(Sender: TObject);
begin
   OpenHyperLink('http://yrarg.cncguild.net/');
end;

procedure TFrmTimeMain.ibEd1Click(Sender: TObject);
begin
   OpenHyperLink('http://www.tibed.net/');
end;

procedure TFrmTimeMain.XCC1Click(Sender: TObject);
begin
   OpenHyperLink('http://xccu.sf.net/');
end;

procedure TFrmTimeMain.RedUtils1Click(Sender: TObject);
begin
   OpenHyperLink('http://dc.strategy-x.com/');
end;

procedure TFrmTimeMain.RockTheBattlefield1Click(Sender: TObject);
begin
   OpenHyperLink('http://rtb.strategy-x.com/');
end;

procedure TFrmTimeMain.RA2FAQ1Click(Sender: TObject);
begin
   OpenHyperLink('http://ra2faq.savagewar.co.uk/');
end;

procedure TFrmTimeMain.RenegadeProjects1Click(Sender: TObject);
begin
   OpenHyperLink('http://www.renegadeprojects.com/');
end;

procedure TFrmTimeMain.RevoraCCForums1Click(Sender: TObject);
begin
   OpenHyperLink('http://forums.revora.net/index.php?showforum=1078');
end;

procedure TFrmTimeMain.AcidVat1Click(Sender: TObject);
begin
   OpenHyperLink('http://www.gamesmodding.com/');
end;

procedure TFrmTimeMain.RA2GraphicsHeaven1Click(Sender: TObject);
begin
   OpenHyperLink('http://revenge.cncguild.net/');
end;

procedure TFrmTimeMain.CnCGuild1Click(Sender: TObject);
begin
   OpenHyperLink('http://www.cncguild.net/');
end;

procedure TFrmTimeMain.iberiumSunCom1Click(Sender: TObject);
begin
   OpenHyperLink('http://www.tiberiumweb.com/');
end;

procedure TFrmTimeMain.test1Click(Sender: TObject);
begin
   Update3dViewVOXEL(VoxelFile);
end;

procedure TFrmTimeMain.Importfrommodel1Click(Sender: TObject);
var
   tempvxl : TVoxel;
   i, SectionIndex,tempsectionindex: Integer;
   frm: Tfrmimportsection;
begin
   if not isEditable then exit;

   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: ImportFromModel1Click');
   {$endif}
   tempvxl := TVoxel.Create;

   if OpenVXLDialog.execute then
   begin
      tempvxl.LoadFromFile(OpenVXLDialog.Filename);
      tempsectionindex := 0;
      if tempvxl.Header.NumSections > 1 then
      begin
         frm:=Tfrmimportsection.Create(Self);
         frm.Visible:=False;

         frm.ComboBox1.Items.Clear;
         for i:=0 to tempvxl.Header.NumSections-1 do
         begin
            frm.ComboBox1.Items.Add(tempvxl.Section[i].Name);
         end;
         frm.ComboBox1.ItemIndex:=0;
         frm.ShowModal;
         tempsectionindex := frm.ComboBox1.ItemIndex;
         frm.Free;
      end;

      SectionIndex:=ActiveSection.Header.Number;
      Inc(SectionIndex);
      ResetUndoRedo;
      UpdateUndo_RedoState;

      VoxelFile.InsertSection(SectionIndex,tempvxl.Section[tempsectionindex].Name,tempvxl.Section[tempsectionindex].Tailer.XSize,tempvxl.Section[tempsectionindex].Tailer.YSize,tempvxl.Section[tempsectionindex].Tailer.ZSize);
      VoxelFile.Section[SectionIndex].Assign(tempvxl.Section[tempsectionindex]);

      tempvxl.Free;
      SetupSections;
      VXLChanged := true;
   end;
end;

procedure TFrmTimeMain.Resize1Click(Sender: TObject);
var
  FrmNew: TFrmNew;
begin
   if not isEditable then exit;

   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: Resize1Click');
   {$endif}
   FrmNew:=TFrmNew.Create(Self);
   with FrmNew do
   begin
      //FrmNew.Caption := ' Resize';
      //grpCurrentSize.Left := 8;
      //grpNewSize.Left := 112;
      //grpNewSize.Width := 97;
      Label9.caption := 'Enter the size you want the canvas to be';
      Label10.caption := 'Resize Canvas';
      grpCurrentSize.Visible := true;
      Image1.Picture := TopBarImageHolder.Picture;
      grpVoxelType.Visible := false;

      x := ActiveSection.Tailer.XSize;
      y := ActiveSection.Tailer.YSize;
      z := ActiveSection.Tailer.ZSize;
      ShowModal;
      if changed then
      begin
         //Save undo information
         ResetUndoRedo;
         UpdateUndo_RedoState;

         SetIsEditable(false);
         ActiveSection.Resize(x,y,z);
         SetIsEditable(true);
         SectionComboChange(Sender);
         VXLChanged := true;
      end;
   end;
   FrmNew.Free;
end;

procedure TFrmTimeMain.SpinButton3UpClick(Sender: TObject);
begin
   YCursorBar.Position := YCursorBar.Position +1;
end;

procedure TFrmTimeMain.SpinButton3DownClick(Sender: TObject);
begin
   YCursorBar.Position := YCursorBar.Position -1;
end;

procedure TFrmTimeMain.SpinButton1DownClick(Sender: TObject);
begin
   ZCursorBar.Position := ZCursorBar.Position -1;
end;

procedure TFrmTimeMain.SpinButton1UpClick(Sender: TObject);
begin
   ZCursorBar.Position := ZCursorBar.Position +1;
end;

procedure TFrmTimeMain.SpinButton2DownClick(Sender: TObject);
begin
   XCursorBar.Position := XCursorBar.Position -1;
end;

procedure TFrmTimeMain.SpinButton2UpClick(Sender: TObject);
begin
   XCursorBar.Position := XCursorBar.Position +1;
end;

procedure TFrmTimeMain.FullResize1Click(Sender: TObject);
var
   frm: TFrmFullResize;
begin
   if not isEditable then exit;

   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: FullResize1Click');
   {$endif}
   frm:=TFrmFullResize.Create(Self);
   with frm do
   begin
      x := ActiveSection.Tailer.XSize;
      y := ActiveSection.Tailer.YSize;
      z := ActiveSection.Tailer.ZSize;
      Image1.Picture := TopBarImageHolder.Picture;
      ShowModal;
      if Changed then
      begin
         ResetUndoRedo;
         UpdateUndo_RedoState;
         SetIsEditable(false);
         ActiveSection.ResizeBlowUp(Scale);
         SetIsEditable(true);
         SectionComboChange(Sender);
         VXLChanged := true;
      end;
   end;
   frm.Free;
end;

procedure TFrmTimeMain.ToolButton9Click(Sender: TObject);
var
   frm: TFrmPreferences;
begin
   frm:=TFrmPreferences.Create(Self);
   frm.ShowModal;
   frm.Free;
end;

procedure TFrmTimeMain.SpeedButton9Click(Sender: TObject);
begin
   SetVXLTool(VXLTool_SmoothNormal);
   Normals1.Click;
end;

procedure TFrmTimeMain.ToolButton13Click(Sender: TObject);
var
  frm: TFrmReplaceColour;
begin
  frm:=TFrmReplaceColour.Create(Self);
  frm.Visible:=False;
  frm.Image1.Picture := TopBarImageHolder.Picture;

  frm.ShowModal;
  frm.Close;
  frm.Free;
end;

Function CharToStr : String;
var
x : integer;
begin
   for x := 1 to 16 do
      Result := VoxelFile.Header.FileType[x];
end;

Function CleanString(S : string) : String;
var
   x : integer;
begin
   Result := '';
   for x := 1 to Length(s) do
      if (S[x] <> '&') and (S[x] <> 'x') then
         Result := Result + S[x];
end;

procedure TFrmTimeMain.N1x1Click(Sender: TObject);
begin
   ActiveSection.Viewport[0].Zoom := Strtoint(CleanString(TMenuItem(Sender).caption));
   CentreViews;
   setupscrollbars;
   CnvView0.Refresh;
end;

procedure TFrmTimeMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
   if not isEditable then exit;

   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: FormKeyDown');
   {$endif}
   if (Key = Ord('X')) or (Key = Ord('x')) then
   begin
      if ssShift in shift then
      begin
         if YCursorBar.Position > 0 then
            YCursorBar.Position := YCursorBar.Position - 1;
      end
      else
      begin
         if YCursorBar.Position < YCursorBar.Max then
            YCursorBar.Position := YCursorBar.Position + 1;
      end;
      XCursorBarChange(nil);
   end
   else if (Key = Ord('Y')) or (Key = Ord('y')) then
   begin
      if ssShift in shift then
      begin
         if ZCursorBar.Position > 0 then
            ZCursorBar.Position := ZCursorBar.Position - 1;
      end
      else
      begin
         if ZCursorBar.Position < ZCursorBar.Max then
            ZCursorBar.Position := ZCursorBar.Position + 1;
      end;
      XCursorBarChange(nil);
   end;
   if (Key = Ord('Z')) or (Key = Ord('z')) then
   begin
      if ssShift in shift then
      begin
         if XCursorBar.Position > 0 then
            XCursorBar.Position := XCursorBar.Position - 1;
      end
      else
      begin
         if XCursorBar.Position < XCursorBar.Max then
            XCursorBar.Position := XCursorBar.Position + 1;
      end;
      XCursorBarChange(nil);
   end;
end;

procedure TFrmTimeMain.Display3DWindow1Click(Sender: TObject);
begin
   if p_Frm3DPreview = nil then
   begin
      {$ifdef DEBUG_FILE}
      DebugFile.Add('FrmTimeMain: Display3DWindow1Click');
      {$endif}
      Application.OnIdle := nil;
      new(p_Frm3DPreview);
      p_Frm3DPreview^ := TFrm3DPreview.Create(self);
      p_Frm3DPreview^.Show;
      Application.OnIdle := Idle;
   end;
end;

procedure TFrmTimeMain.NewAutoNormals1Click(Sender: TObject);
begin
   if not isEditable then exit;

   {$ifdef DEBUG_FILE}
   DebugFile.Add('FrmTimeMain: CubedAutoNormals1Click');
   {$endif}
   // ask the user to confirm
   if MessageDlg('Autonormals v6.1' +#13#13+
        'This process will modify the voxel''s normals.' +#13+
        'If you choose to do this, you should first save' + #13 +
        'your model under a different name as a backup.',
        mtWarning,mbOKCancel,0) = mrCancel then
                        Exit;
   //ResetUndoRedo;
   CreateVXLRestorePoint(ActiveSection,Undo);
   UpdateUndo_RedoState;

   ApplyInfluenceNormalsToVXL(ActiveSection);
   Refreshall;
   VXLChanged := true;
end;

end.
