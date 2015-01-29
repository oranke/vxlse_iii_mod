unit Voxel_Engine;

interface

uses Windows,Voxel,Palette,StdCtrls,ExtCtrls,Graphics,Math,SysUtils,Types,cls_config,Constants,
     Menus,Clipbrd,mouse, forms, Dialogs;

{$INCLUDE Global_Conditionals.inc}
type
   PPaintBox = ^TPaintBox;
   PLabel = ^TLabel;
   PMenuItem = ^TMenuItem;

   TVector3f = record
      X, Y, Z : single;
   end;

   TVector3i = record
      X, Y, Z : integer;
   end;

   TTempViewData = record
      V : TVoxelUnpacked;
      VC : TVector3i; // Cordinates of V
      VU : Boolean; // Is V Used, if so we can do this to the VXL file when needed
      X,Y : Integer;
   end;

   TTempView = record
      Data : Array of TTempViewData;
      Data_no : integer;
   end;

   TPaletteList = record
      Data : array of String;
      Data_no : integer;
   end;

   TColourSchemes = array of packed record
        Name,Filename,By,Website : string;
        Data : array [0..255] of byte;
   end;

Var
   VoxelFile : TVoxel;
   ActiveSection : TVoxelSection;
   CurrentSection : cardinal;
   SpectrumMode : ESpectrumMode;
   ViewMode: EViewMode;
   VXLFilename : String;
   VoxelOpen : Boolean = false;
   isEditable : Boolean = false;
   isLeftMouseDown : Boolean = false;
   isLeftMB : Boolean = false;
   isCursorReset : Boolean = false;
   scrollbar_editable : Boolean = false;
   UsedColoursOption : Boolean = false;
   BGViewColor : TColor;
   CnvView : array [0..2] of PPaintBox;
   lblView : array [0..2] of PLabel;
   ActiveNormalsCount,ActiveColour,ActiveNormal : integer;
   VXLBrush : integer = 0;
   VXLTool : integer = 0;
   DarkenLighten : integer = 1;
   LastClick : array [0..2] of TVector3i;
   TempView : TTempView;
   mnuReopen : PMenuItem;
   PaletteList : TPaletteList;
   VXLChanged : Boolean = false;
   ColourSchemes : TColourSchemes;
   // 1.2b adition:
   UsedColours : array [0..255] of boolean;
   UsedNormals : array [0..244] of boolean;

   Config: TConfiguration; // For File History
   mnuHistory: Array[0..HistoryDepth-1] of TMenuItem;
Const
   TestBuild = false;
   TestBuildVersion = '5';
   DefultZoom = 7;
   VIEWBGCOLOR = -1;

   VXLTool_Brush = 0;
   VXLTool_Line = 1;
   VXLTool_Erase = 2;
   VXLTool_FloodFill = 3;
   VXLTool_Dropper = 4;
   VXLTool_Rectangle = 5;
   VXLTool_FilledRectangle = 6;
   VXLTool_Darken = 7;
   VXLTool_Lighten = 8;
   VXLTool_SmoothNormal = 9;
   VXLTool_FloodFillErase = 10;

   ViewName: array[0..5] of string = (
      ' Back',
      ' Front',
      ' Right',
      ' Left',
      ' Top',
      ' Bottom');

Function LoadVoxel(Filename : String) : boolean;
Function NewVoxel(Game,x,y,z : integer) : boolean;
Procedure ChangeSection;
Procedure SetupViews;
Procedure SetSpectrumMode;
Procedure SetNormalsCount;
Function CleanVCol(Color : TVector3f) : TColor;
Function GetVXLPaletteColor(Color : integer) : TColor;
Procedure PaintView(WndIndex: Integer; isMouseLeftDown : boolean; var Cnv: PPaintBox; var View: TVoxelView);
function colourtogray(colour : cardinal): cardinal;
procedure SplitColour(raw: TColor; var red, green, blue: Byte);
Procedure PaintPalette(var cnvPalette : TPaintBox; Mark : boolean);
Procedure CentreView(WndIndex: Integer);
Procedure CentreViews;
procedure ZoomToFit(WndIndex: Integer);
Procedure RepaintViews;
procedure TranslateClick(WndIndex, sx, sy: Integer; var lx, ly, lz: Integer);
procedure TranslateClick2(WndIndex, sx, sy: Integer; var lx, ly, lz: Integer);
procedure MoveCursor(lx, ly, lz: Integer; Repaint : boolean);
Function GetPaletteColourFromVoxel(x,y, WndIndex : integer) : integer;
procedure ActivateView(Idx: Integer);
procedure SyncViews;
procedure RefreshViews;
procedure drawstraightline(const a : TVoxelSection; var tempview : Ttempview; var last,first : TVector3i; v: TVoxelUnpacked);
procedure VXLRectangle(Xpos,Ypos,Zpos,Xpos2,Ypos2,Zpos2:Integer; Fill: Boolean; v : TVoxelUnpacked);
Function ApplyNormalsToVXL(var VXL : TVoxelSection) : integer;
Function ApplyCubedNormalsToVXL(var VXL : TVoxelSection) : integer;
Function ApplyInfluenceNormalsToVXL(var VXL : TVoxelSection) : integer;
Function RemoveRedundantVoxelsFromVXL(var VXL : TVoxelSection) : integer;
procedure UpdateHistoryMenu;
procedure VXLBrushTool(VXL : TVoxelSection; Xc,Yc,Zc: Integer; V: TVoxelUnpacked; BrushMode: Integer; BrushView: EVoxelViewOrient);
procedure VXLBrushToolDarkenLighten(VXL : TVoxelSection; Xc,Yc,Zc: Integer; BrushMode: Integer; BrushView: EVoxelViewOrient; Darken : Boolean);
procedure ClearVXLLayer(var Vxl : TVoxelSection);
Function ApplyTempView(var vxl :TVoxelSection): Boolean;
Procedure VXLCopyToClipboard(Vxl : TVoxelSection);
Procedure VXLCutToClipboard(Vxl : TVoxelSection);
procedure VXLFloodFillTool(Vxl : TVoxelSection; Xpos,Ypos,Zpos: Integer; v: TVoxelUnpacked; EditView: EVoxelViewOrient);
Procedure RemoveDoublesFromTempView;
Procedure PasteFullVXL(var Vxl : TVoxelsection);
Procedure PasteVXL(var Vxl : TVoxelsection);
procedure PaintView2(WndIndex: Integer; isMouseLeftDown : boolean; var Cnv: PPaintBox; var View: TVoxelView);
Procedure SmoothVXLNormals(var Vxl : TVoxelSection);

procedure VXLSmoothBrushTool(VXL : TVoxelSection; Xc,Yc,Zc: Integer; V: TVoxelUnpacked; BrushMode: Integer; BrushView: EVoxelViewOrient);

function SetVector(x, y, z : single) : TVector3f;
function SetVectorI(x, y, z : integer) : TVector3i;

Procedure SetVoxelFileDefults;
Function IsVoxelValid : Boolean;
Function HasNormalsBug : Boolean;
Procedure SetNormals(Normal : Integer);

implementation

uses Voxel_Tools,undo_engine, Controls, FormMain;

function SetVector(x, y, z : single) : TVector3f;
begin
   result.x := x;
   result.y := y;
   result.z := z;
end;

function SetVectorI(x, y, z : integer) : TVector3i;
begin
   result.x := x;
   result.y := y;
   result.z := z;
end;

Function HasNormalsBug : Boolean;
var
   x,N : integer;
begin
   {$ifdef DEBUG_FILE}
   FrmMain.DebugFile.Add('VoxelEngine: HasNormalsBug');
   {$endif}
   Result := False;
   N := VoxelFile.Section[0].Tailer.Unknown;
   for x := 0 to VoxelFile.Header.NumSections -1 do
      if VoxelFile.Section[x].Tailer.Unknown <> N then
         Result := True;
end;

Function IsVoxelValid : Boolean;
begin
   {$ifdef DEBUG_FILE}
   FrmMain.DebugFile.Add('VoxelEngine: IsVoxelValid');
   {$endif}
   if AsciizToStr(VoxelFile.Header.FileType,16) <> 'Voxel Animation' then
      Result := False
   else if VoxelFile.Header.Unknown <> 1 then
      Result := False
   else
      Result := True;
end;

Function LoadVoxel(Filename : String) : boolean;
begin
   Result := false;
   try
      VoxelFile.Free;
      VoxelFile := TVoxel.Create;
      VoxelFile.LoadFromFile(Filename);
      CurrentSection := 0;

      ActiveSection := VoxelFile.Section[CurrentSection];
   except
      VoxelOpen := false;
      exit;
   end;

   {$ifdef DEBUG_FILE}
   FrmMain.DebugFile.Add('VoxelEngine: LoadVoxel');
   {$endif}
   VXLFilename := Filename;
   Config.AddFileToHistory(VXLFilename);
   UpdateHistoryMenu;
   VoxelOpen := true;
   Result := true;

   SetupViews;
   SetNormalsCount;
   SetSpectrumMode;
   VXLChanged := false;
end;

procedure SetHeaderFileType(Name: String);
const
   MAX_LEN = 16;
var
   i: integer;
begin
   {$ifdef DEBUG_FILE}
   FrmMain.DebugFile.Add('VoxelEngine: SetHeaderFileType');
   {$endif}
   for i:=1 to 16 do
      VoxelFile.Header.FileType[i]:=#0;
      for i := 1 to Length(Name) do
      begin
         if i > MAX_LEN then break;
         VoxelFile.Header.FileType[i] := Name[i];
      end;
end;

Procedure SetVoxelFileDefults;
begin
   {$ifdef DEBUG_FILE}
   FrmMain.DebugFile.Add('VoxelEngine: SetVoxelFileDefaults');
   {$endif}
   SetHeaderFileType('Voxel Animation');
   VoxelFile.Header.Unknown := 1;
   VoxelFile.Header.StartPaletteRemap := 16;
   VoxelFile.Header.EndPaletteRemap := 31;
   VoxelFile.Header.BodySize := 0;
end;

Function NewVoxel(Game,x,y,z : integer) : boolean;
begin
   Result := false;
   try
      VoxelFile.Free;
      VoxelFile := TVoxel.Create;

      SetVoxelFileDefults;
      VoxelFile.Header.NumSections := 0;
      VoxelFile.Header.NumSections2 := 0;

      CurrentSection := 0;
      VoxelFile.InsertSection(0,'Body',x,y,z);
      ActiveSection := VoxelFile.Section[CurrentSection];
      ActiveSection.Tailer.Unknown := Game;
   except
      VoxelOpen := false;
      exit;
   end;

   {$ifdef DEBUG_FILE}
   FrmMain.DebugFile.Add('VoxelEngine: NewVoxel');
   {$endif}
   VXLFilename := '';
   VoxelOpen := true;
   VoxelFile.Loaded := true;
   Result := true;

   if FrmMain.p_Frm3DPreview <> nil then
   begin
      FrmMain.p_Frm3DPreview^.SpStopClick(nil);
      FrmMain.p_Frm3DPreview^.SpFrame.MaxValue := 1;
   end;
   SetupViews;
   SetNormalsCount;
   SetSpectrumMode;
   VXLChanged := true;
end;

Procedure ChangeSection;
begin
   {$ifdef DEBUG_FILE}
   FrmMain.DebugFile.Add('VoxelEngine: ChangeSection');
   {$endif}
   ActiveSection := VoxelFile.Section[CurrentSection];
   SetupViews;
end;

Procedure SetupViews;
begin
   {$ifdef DEBUG_FILE}
   FrmMain.DebugFile.Add('VoxelEngine: SetupViews');
   {$endif}
   ActiveSection.View[0].Refresh;
   ActiveSection.View[1].Refresh;
   ActiveSection.View[2].Refresh;

   ActiveSection.Viewport[0].Zoom := DefultZoom;
   ActiveSection.Viewport[1].Zoom := DefultZoom;
   ActiveSection.Viewport[2].Zoom := DefultZoom;

   ZoomToFit(1);
   ZoomToFit(2);

// CentreView(0);
   CentreViews;
end;

Procedure SetSpectrumMode;
begin
   {$ifdef DEBUG_FILE}
   FrmMain.DebugFile.Add('VoxelEngine: SetSpectrumMode');
   {$endif}
   VoxelFile.setSpectrum(SpectrumMode);
end;

Procedure SetNormalsCount;
begin
   {$ifdef DEBUG_FILE}
   FrmMain.DebugFile.Add('VoxelEngine: SetNormalsCount');
   {$endif}
   ActiveNormalsCount := MAXNORM_TIBERIAN_SUN;

   if ActiveSection.Tailer.Unknown = 4 then
      ActiveNormalsCount := MAXNORM_RED_ALERT2;
end;

Function CleanVCol(Color : TVector3f) : TColor;
Var
   T : TVector3f;
begin
   {$ifdef DEBUG_FILE}
   FrmMain.DebugFile.Add('VoxelEngine: CleanVCol');
   {$endif}
   T.X := Color.X;
   T.Y := Color.Y;
   T.Z := Color.Z;

   If T.X > 255 then
      T.X := 255
   else If T.X < 0 then
      T.X := 0;

   If T.Y > 255 then
      T.Y := 255
   else If T.Y < 0 then
      T.Y := 0;

   If T.Z > 255 then
      T.Z := 255
   else if T.Z < 0 then
      T.Z := 0;

   Result := RGB(trunc(T.X),trunc(T.Y),trunc(T.Z));
end;

Function GetVXLPaletteColor(Color : integer) : TColor;
Var
   T : TVector3f;
   NormalNum : Integer;
   NormalDiv : single;
   N : integer;
begin
   if Color < 0 then
   begin
      Result := BGViewColor;
      exit;
   end;

   if SpectrumMode = ModeColours then
      Result := VXLPalette[color]
   else
   begin
      if ActiveSection.Tailer.Unknown = 4 then
      begin
         NormalNum := 244;
         NormalDiv := 3;
      end
      else
      begin
         NormalNum := 35;
         NormalDiv := 0.5;
      end;

      N := Color;
      If N > NormalNum Then
         N := NormalNum;
      T.X := 127 + (N - (NormalNum/2))/NormalDiv;
      T.Y := 127 + (N - (NormalNum/2))/NormalDiv;
      T.Z := 127 + (N - (NormalNum/2))/NormalDiv;
//      T.X := N;
//      T.Y := N;
//      T.Z := N;
      Result := CleanVCol(T);
   end;
end;

procedure PaintView(WndIndex: Integer; isMouseLeftDown : boolean; var Cnv: PPaintBox; var View: TVoxelView);
var
   x,xx, y, txx,tyy: Integer;
   r: TRect;
   PalIdx : integer;
   Viewport: TViewport;
   f : boolean;
begin
   {$ifdef DEBUG_FILE}
   FrmMain.DebugFile.Add('VoxelEngine: PaintView');
   {$endif}
   if (not Cnv.Enabled) or (not IsEditable) then
   begin // draw it empty then
      with Cnv.Canvas do
      begin
         r.Left := 0;
         r.Top := 0;
         r.Right := Cnv.Width;
         r.Bottom := Cnv.Height;
         Brush.Color := clBtnFace;
         FillRect(r);
      end;
      Exit; // don't do anything else then
   end;
   if View = nil then Exit;
   Viewport := ActiveSection.Viewport[WndIndex];
   // fill margins around shape
   Cnv.Canvas.Brush.Style := bsSolid;
   Cnv.Canvas.Brush.Color := BGViewColor;
   // left side?
   if (Viewport.Left > 0) then
   begin
      with r do
      begin // left size
         Left := 0;
         Right := Viewport.Left;
         Top := 0;
         Bottom := Cnv.Height;
      end;
      with Cnv.Canvas do
         FillRect(r);
   end;
   // right side?
   if (Viewport.Left + (Viewport.Zoom * View.Width)) < Cnv.Width then
   begin
      with r do
      begin // right side
         Left := Viewport.Left + (Viewport.Zoom * View.Width);
         Right := Cnv.Width;
         Top := 0;
         Bottom := Cnv.Height;
      end;
      with Cnv.Canvas do
         FillRect(r);
   end;
   // top
   if (Viewport.Top > 0) then
   begin
      with r do
      begin // top
         Left := 0;
         Right := Cnv.Width;
         Top := 0;
         Bottom := Viewport.Top;
      end;
      with Cnv.Canvas do
         FillRect(r);
   end;
   // bottom
   if (Viewport.Top + (Viewport.Zoom * View.Height)) < Cnv.Height then
   begin
      with r do
      begin // bottom
         Top := Viewport.Top + (Viewport.Zoom * View.Height);
         Bottom := Cnv.Height;
         Left := 0;
         Right := Cnv.Width;
      end;
      with Cnv.Canvas do
         FillRect(r);
   end;
   Cnv.Canvas.Brush.Style := bsSolid;
   // paint view
   Cnv.Canvas.Pen.Color := clRed; //VXLPalette[Transparent];
   for x := 0 to (View.Width - 1) do
   begin
      r.Left := (x * Viewport.Zoom) + Viewport.Left;
      r.Right := r.Left + Viewport.Zoom;
      if r.Right < 0 then Continue; // not visible checks
      if r.Left > Cnv.Width then Continue; // not visible checks
      r.Top := Viewport.Top;
      r.Bottom := r.Top + Viewport.Zoom;
      for y := 0 to (View.Height - 1) do
      begin
         if (r.Bottom >= 0) and (r.Top <= Cnv.Height) then
         begin // not visible checks
            if (ViewMode = ModeCrossSection) and (wndindex=0) then
            begin
               if View.Canvas[x,y].Depth = View.Foreground then
                  PalIdx := View.Canvas[x,y].Colour
               else
                  PalIdx := VIEWBGCOLOR;
            end
            else if View.Canvas[x,y].Colour = VTRANSPARENT then // ModeFull or ModeEmpDepth
               PalIdx := VIEWBGCOLOR
            else
               PalIdx := View.Canvas[x,y].Colour;

            f := false;

            if WndIndex = 0 then
            if tempview.Data_no > 0 then
            begin
               for xx := 1 to tempview.Data_no do
               begin
                  if (View.getViewNameIdx = 1) or (View.getViewNameIdx = 3) or (View.getViewNameIdx = 5) then
                     txx := View.Width - 1-tempview.data[xx].X
                  else
                     txx := tempview.data[xx].X;

                  if (txx = x) and (View.Height - 1-tempview.data[xx].Y = y) then
                  begin
                     if tempview.data[xx].VU then
                     begin
                        if tempview.data[xx].V.Used then
                           PalIdx := tempview.data[xx].V.Normal;
                     end
                     else
                     begin
                        if SpectrumMode = ModeColours then
                           PalIdx := ActiveColour
                        else
                           PalIdx := ActiveNormal;
                     end;

                     f := true;
                  end;
               end;
            end;

            with Cnv.Canvas do
            begin
               Brush.Color := GetVXLPaletteColor(PalIdx);
               if ((ViewMode = ModeEmphasiseDepth) and (wndindex=0) and (View.Canvas[x,y].Depth = View.Foreground)) or (f = true) then
               begin
                  if Viewport.Zoom = 1 then
                     Pixels[r.Left,r.Top] := Pen.Color
                  else
                     Rectangle(r.Left,r.Top,r.Right,r.Bottom)
               end
               else
                  FillRect(r);
{$IFDEF DEBUG_NORMALS}
               if (SpectrumMode = ModeNormals) then
                  if (PalIdx > -1) then
                     if Viewport.Zoom > 10 then
                     begin
                        Font.Color := VXLPalette[PalIdx] shr 2;
                        TextOut(r.Left,r.Top,IntToStr(View.Canvas[x,y].Colour));
                     end;
{$ENDIF}
            end;
         end;
         Inc(r.Top,Viewport.Zoom);
         Inc(r.Bottom,Viewport.Zoom);
      end;
   end;

   // draw cursor, but not if drawing!
   if not isMouseLeftDown then
   begin
      View.getPhysicalCursorCoords(x,y);

      //set brush color, or else it will get the color of the bottom-right voxel
      Cnv.Canvas.Brush.Color:= BGViewColor;

      // vert
      r.Top := Max(Viewport.Top,0);
      r.Bottom := Min(Viewport.Top + (View.Height * Viewport.Zoom), Cnv.Height);
      r.Left := (x * Viewport.Zoom) + Viewport.Left + (Viewport.Zoom div 2) - 1;
      r.Right := r.Left + 2;
      Cnv.Canvas.DrawFocusRect(r);

      // horiz
      r.Left := Max(Viewport.Left,0);
      r.Right := Min(Viewport.Left + (View.Width * Viewport.Zoom), Cnv.Width);
      r.Top := (y * Viewport.Zoom) + Viewport.Top + (Viewport.Zoom div 2) - 1;
      r.Bottom := r.Top + 2;
      Cnv.Canvas.DrawFocusRect(r);
   end
   else
   begin
      View.getPhysicalCursorCoords(x,y);

      //set brush color, or else it will get the color of the bottom-right voxel
      Cnv.Canvas.Brush.Color:= clbtnface;
      // vert
      r.Top := Max(Viewport.Top,0)-1;
      r.Bottom := Min(Viewport.Top + (View.Height * Viewport.Zoom), Cnv.Height)+1;
      r.Left := Max(Viewport.Left,0)-1;
      r.Right := Min(Viewport.Left + (View.Width * Viewport.Zoom), Cnv.Width)+1;
      Cnv.Canvas.FrameRect(r);
   end;
end;

procedure PaintView2(WndIndex: Integer; isMouseLeftDown : boolean; var Cnv: PPaintBox; var View: TVoxelView);
var
   x,xx, y, txx,tyy: Integer;
   r: TRect;
   PalIdx : integer;
   Viewport: TViewport;
   f : boolean;
   Bitmap : TBitmap;
begin
   {$ifdef DEBUG_FILE}
   FrmMain.DebugFile.Add('VoxelEngine: PaintView2');
   {$endif}
   Bitmap := nil;
   if (not Cnv.Enabled) or (not IsEditable) then
   begin // draw it empty then
      with Cnv.Canvas do
      begin
         r.Left := 0;
         r.Top := 0;
         r.Right := Cnv.Width;
         r.Bottom := Cnv.Height;
         Brush.Color := clBtnFace;
         FillRect(r);
      end;
      Exit; // don't do anything else then
   end;
   if View = nil then Exit;
   Viewport := ActiveSection.Viewport[WndIndex];
   // fill margins around shape
   Bitmap := TBitmap.Create;
   Bitmap.Canvas.Brush.Style := bsSolid;
   Bitmap.Canvas.Brush.Color := BGViewColor;
   Bitmap.Width := Cnv.Width;
   Bitmap.Height := Cnv.Height;
   // left side?
   if (Viewport.Left > 0) then
   begin
      with r do
      begin // left size
         Left := 0;
         Right := Viewport.Left;
         Top := 0;
         Bottom := Cnv.Height;
      end;
      with Bitmap.Canvas do
         FillRect(r);
   end;
   // right side?
   if (Viewport.Left + (Viewport.Zoom * View.Width)) < Cnv.Width then
   begin
      with r do
      begin // right side
         Left := Viewport.Left + (Viewport.Zoom * View.Width);
         Right := Cnv.Width;
         Top := 0;
         Bottom := Cnv.Height;
      end;
      with Bitmap.Canvas do
         FillRect(r);
   end;
   // top
   if (Viewport.Top > 0) then
   begin
      with r do
      begin // top
         Left := 0;
         Right := Cnv.Width;
         Top := 0;
         Bottom := Viewport.Top;
      end;
      with Bitmap.Canvas do
         FillRect(r);
   end;
   // bottom
   if (Viewport.Top + (Viewport.Zoom * View.Height)) < Cnv.Height then
   begin
      with r do
      begin // bottom
         Top := Viewport.Top + (Viewport.Zoom * View.Height);
         Bottom := Cnv.Height;
         Left := 0;
         Right := Cnv.Width;
      end;
      with Bitmap.Canvas do
         FillRect(r);
   end;
   Bitmap.Canvas.Brush.Style := bsSolid;
   // paint view
   Bitmap.Canvas.Pen.Color := clRed; //VXLPalette[Transparent];
   for x := 0 to (View.Width - 1) do
   begin
         r.Left := (x * Viewport.Zoom) + Viewport.Left;
         r.Right := r.Left + Viewport.Zoom;
         if r.Right < 0 then Continue; // not visible checks
         if r.Left > Cnv.Width then Continue; // not visible checks
         r.Top := Viewport.Top;
         r.Bottom := r.Top + Viewport.Zoom;
         for y := 0 to (View.Height - 1) do begin
             if (r.Bottom >= 0) and (r.Top <= Cnv.Height) then begin // not visible checks
                if (ViewMode = ModeCrossSection) and (wndindex=0) then begin
                   if View.Canvas[x,y].Depth = View.Foreground then
                      PalIdx := View.Canvas[x,y].Colour
                   else
                       PalIdx := VIEWBGCOLOR;
                end else if View.Canvas[x,y].Colour = VTRANSPARENT then // ModeFull or ModeEmpDepth
                    PalIdx := VIEWBGCOLOR
                else
                    PalIdx := View.Canvas[x,y].Colour;

                f := false;

                {if WndIndex = 0 then
                if tempview.Data_no > 0 then
                begin
                if View.getViewNameIdx = 3 then
                txx := tempview.Data[xx].X
                else
                txx := tempview.Data[xx].X;
                end;}

                if WndIndex = 0 then
                if tempview.Data_no > 0 then
                begin
                for xx := 1 to tempview.Data_no do
                begin

                if (View.getViewNameIdx = 1) or(View.getViewNameIdx = 3) or (View.getViewNameIdx = 5) then
                txx := View.Width - 1-tempview.data[xx].X
                else
                txx := tempview.data[xx].X;

                if (txx = x) and (View.Height - 1-tempview.data[xx].Y = y) then
                if tempview.data[xx].VU then
                begin
                if SpectrumMode = ModeColours then
                PalIdx := tempview.data[xx].V.Colour
                else
                PalIdx := tempview.data[xx].V.Normal;
                f := true;
                end;

                end;
                end;

                with Bitmap.Canvas do begin
                     Brush.Color := GetVXLPaletteColor(PalIdx);
                     if ((ViewMode = ModeEmphasiseDepth) and (wndindex=0) and
                        (View.Canvas[x,y].Depth = View.Foreground)) or (f = true) then begin
                        if Viewport.Zoom = 1 then
                           Pixels[r.Left,r.Top] := Pen.Color
                        else
                            Rectangle(r.Left,r.Top,r.Right,r.Bottom)
                     end else
                         FillRect(r);
{$IFDEF DEBUG_NORMALS}
                     if (SpectrumMode = ModeNormals) then
                        if (PalIdx > -1) then
                           if Viewport.Zoom > 10 then begin
                              Font.Color := VXLPalette[PalIdx] shr 2;
                              TextOut(r.Left,r.Top,IntToStr(View.Canvas[x,y].Colour));
                           end;
{$ENDIF}
                end;
             end;
             Inc(r.Top,Viewport.Zoom);
             Inc(r.Bottom,Viewport.Zoom);
         end;
     end;

     // draw cursor, but not if drawing!
     if not isMouseLeftDown then begin
       View.getPhysicalCursorCoords(x,y);

       //set brush color, or else it will get the color of the bottom-right voxel
       Bitmap.Canvas.Brush.Color:= BGViewColor;

       // vert
       r.Top := Max(Viewport.Top,0);
       r.Bottom := Min(Viewport.Top + (View.Height * Viewport.Zoom), Cnv.Height);
       r.Left := (x * Viewport.Zoom) + Viewport.Left + (Viewport.Zoom div 2) - 1;
       r.Right := r.Left + 2;
       Bitmap.Canvas.DrawFocusRect(r);

       // horiz
       r.Left := Max(Viewport.Left,0);
       r.Right := Min(Viewport.Left + (View.Width * Viewport.Zoom), Cnv.Width);
       r.Top := (y * Viewport.Zoom) + Viewport.Top + (Viewport.Zoom div 2) - 1;
       r.Bottom := r.Top + 2;
       Bitmap.Canvas.DrawFocusRect(r);
     end
     else
     begin
       View.getPhysicalCursorCoords(x,y);

       //set brush color, or else it will get the color of the bottom-right voxel
       Bitmap.Canvas.Brush.Color:= clbtnface;
       // vert
       r.Top := Max(Viewport.Top,0)-1;
       r.Bottom := Min(Viewport.Top + (View.Height * Viewport.Zoom), Cnv.Height)+1;
       r.Left := Max(Viewport.Left,0)-1;
       r.Right := Min(Viewport.Left + (View.Width * Viewport.Zoom), Cnv.Width)+1;
       Bitmap.Canvas.DrawFocusRect(r);

      { // horiz
       r.Left := Max(Viewport.Left,0);
       r.Right := Min(Viewport.Left + (View.Width * Viewport.Zoom), Cnv.Width);
       r.Top := Viewport.Top;
       r.Bottom := r.Top + 2;
       Cnv.Canvas.DrawFocusRect(r);    }
     end;

     Cnv.Canvas.Draw(0,0,Bitmap); // Draw to Canvas, should stop flickerings
     //Cnv.Canvas.TextOut(0,0,'hmm');
     Bitmap.Free;
end;

function colourtogray(colour : cardinal): cardinal;
var
   temp : char;
begin
   temp := char((GetBValue(colour)*29 + GetGValue(colour)*150 + GetRValue(colour)*77) DIV 256);
   Result := RGB(ord(temp),ord(temp),ord(temp));
end;

procedure SplitColour(raw: TColor; var red, green, blue: Byte);
begin
   red := (raw and $00FF0000) shr 16;
   green := (raw and $0000FF00) shr 8;
   blue := raw and $000000FF;
end;

Procedure PaintPalette(var cnvPalette : TPaintBox; Mark : boolean);
var
   colwidth, rowheight: Real;
   i, j, idx: Integer;
   r: TRect;
   red, green, blue, mixcol: Byte;
begin
   {$ifdef DEBUG_FILE}
   FrmMain.DebugFile.Add('VoxelEngine: PaintPalette');
   {$endif}
   // Start colour mode.
   if SpectrumMode = ModeColours then
   begin
      // Get basic measures.
      colwidth := cnvPalette.Width / 8;
      rowheight := cnvPalette.Height / 32;
      // starts palette painting procedures...
      idx := 0;
      for i := 0 to 8 do
      begin
         r.Left := Trunc(i * colwidth);
         r.Right := Ceil(r.Left + colwidth);
         for j := 0 to 31 do
         begin
             r.Top := Trunc(j * rowheight);
             r.Bottom := Ceil(r.Top + rowheight);
             // dimensions are set. Now the colour.
             with cnvPalette.Canvas do
             begin
                // This set if it's the original palette or...
                // Greyscale (when no file is opened)
                if isEditable then
                   Brush.Color := GetVXLPaletteColor(idx)
                else
                   Brush.Color := colourtogray(GetVXLPaletteColor(idx));

                // Check if it's suposed to be marked, it's active colour
                // and... it's not used. Why? -- Banshee
                if Mark and (((not UsedColoursOption) and (Idx = ActiveColour))) or ((UsedColoursOption) and (UsedColours[idx]))  then
                begin // the current pen
                   // This part makes a square and a 'X' through the colour box.
                   SplitColour(GetVXLPaletteColor(idx),red,green,blue);
                   mixcol := (red + green + blue);
                   Pen.Color := RGB(128 + mixcol,255 - mixcol, mixcol);
                   //Pen.Mode := pmNotXOR;
                   Rectangle(r.Left,r.Top,r.Right,r.Bottom);
                   MoveTo(r.Left,r.Top);
                   LineTo(r.Right,r.Bottom);
                   MoveTo(r.Right,r.Top);
                   LineTo(r.Left,r.Bottom);
                end
                else // Otherwise it just square it with the selected colour
                   FillRect(r);
             end; // Next index...
             Inc(Idx);
         end;
     end;
  end
  else
  begin // SpectrumMode = ModeNormals
     colwidth := cnvPalette.Width / 8;
     rowheight := cnvPalette.Height / 32;
     // clear background
     r.Left := 0;
     r.Right :=cnvPalette.Width;
     r.Top := 0;
     r.Bottom := cnvPalette.Height;
     with cnvPalette.Canvas do
     begin
          Brush.Color := clBtnFace;
          FillRect(r);
     end;
     // and draw Normals palette
     idx := 0;
     for i := 0 to 8 do begin
         r.Left := Trunc(i * colwidth);
         r.Right := Ceil(r.Left + colwidth);
         for j := 0 to 31 do begin
             r.Top := Trunc(j * rowheight);
             r.Bottom := Ceil(r.Top + rowheight);
             with cnvPalette.Canvas do begin
             if isEditable then
                  Brush.Color := GetVXLPaletteColor(idx)
               else
                  Brush.Color := colourtogray(GetVXLPaletteColor(idx));
                  FillRect(r);
                  if Mark and (((not UsedColoursOption) and (Idx = ActiveNormal))) or ((UsedColoursOption) and (UsedNormals[idx]))  then
                  begin // the current pen
                     SplitColour(GetVXLPaletteColor(idx),red,green,blue);
                     mixcol := (red + green + blue);
                     Pen.Color := RGB(128 + mixcol,255 - mixcol, mixcol);
                     //Pen.Mode := pmNotXOR;
                     Rectangle(r.Left,r.Top,r.Right,r.Bottom);
                     MoveTo(r.Left,r.Top);
                     LineTo(r.Right,r.Bottom);
                     MoveTo(r.Right,r.Top);
                     LineTo(r.Left,r.Bottom);
                  end;
             end;
             Inc(idx);
             if idx > ActiveNormalsCount-1 then Break;
         end;
         if idx > ActiveNormalsCount-1 then Break;
     end;
  end;
end;

procedure CentreView(WndIndex: Integer);
var Width, Height, x, y: Integer;
begin
   if not VoxelOpen then Exit;
   {$ifdef DEBUG_FILE}
   FrmMain.DebugFile.Add('VoxelEngine: CentreView');
   {$endif}
   Width := CnvView[WndIndex].Width;
   Height := CnvView[WndIndex].Height;
   with ActiveSection.Viewport[WndIndex] do
   begin
      x := ActiveSection.View[WndIndex].Width * Zoom;
      if x > Width then
         Left := 0 - ((x - Width) div 2)
      else
         Left := (Width - x) div 2;
      y := ActiveSection.View[WndIndex].Height * Zoom;
      if y > Height then
         Top := 0 - ((y - Height) div 2)
      else
         Top := (Height - y) div 2;
   end;
end;

procedure CentreViews;
var
   WndIndex: Integer;
begin
   {$ifdef DEBUG_FILE}
   FrmMain.DebugFile.Add('VoxelEngine: CentreViews');
   {$endif}
   for WndIndex := 0 to 2 do
       CentreView(WndIndex);
end;

procedure ZoomToFit(WndIndex: Integer);
var Width, Height: Integer;
begin
   if not VoxelOpen then Exit;
   {$ifdef DEBUG_FILE}
   FrmMain.DebugFile.Add('VoxelEngine: ZoomToFit');
   {$endif}
   Width := CnvView[WndIndex].Width;
   Height := CnvView[WndIndex].Height;
   with ActiveSection.Viewport[WndIndex] do
   begin
      Left := 0;
      Top := 0;
      Zoom := Trunc(Min(Width / ActiveSection.View[WndIndex].Width,Height / ActiveSection.View[WndIndex].Height));
      if Zoom <= 0 then
         Zoom := 1;
   end;
end;

Procedure RepaintViews;
var
   I : integer;
begin
   {$ifdef DEBUG_FILE}
   FrmMain.DebugFile.Add('VoxelEngine: RepaintViews');
   {$endif}
   for i := 0 to 2 do
   begin
      CnvView[i].Refresh;
   end;
end;

procedure TranslateClick(WndIndex, sx, sy: Integer; var lx, ly, lz: Integer);
var
  p, q: Integer; // physical coords
begin
   {$ifdef DEBUG_FILE}
   FrmMain.DebugFile.Add('VoxelEngine: TranslateClick');
   {$endif}
   //following code needs to be fixed/replaced
   with ActiveSection do
   begin
      //new conversion routines?
      p:=(sx - Viewport[WndIndex].Left) div Viewport[WndIndex].Zoom;
      q:=(sy - Viewport[WndIndex].Top) div Viewport[WndIndex].Zoom;
      p:=Max(p,0);  //make sure p is >=0
      q:=Max(q,0);  //same for q
      //also make sure they're in range of 0..(Width/Height/Depth)-1
      p:=Min(p,View[WndIndex].Width - 1);
      q:=Min(q,View[WndIndex].Height - 1);

      //px/py were from original version, but they give wrong values sometimes (because they use / and trunc instead of div)!
      //px := Min(Max(Trunc((sx - Viewport[WndIndex].Left) / Viewport[WndIndex].Zoom),0),View[WndIndex].Width - 1);
      //py := Min(Max(Ceil((sy - Viewport[WndIndex].Top) / Viewport[WndIndex].Zoom),0),View[WndIndex].Height - 1);

      View[WndIndex].TranslateClick(p,q,lx,ly,lz);
   end;
   //I want range checks - on lx,ly and lz
   //Range checks are already performed somewhere else, but where?!
   //They are flawed!
   //the only reason the program doesn't crash with X is because it can write in
   //other parts of the file!!!
{   if not (lx in [0..ActiveSection.Tailer.XSize-1]) then ShowMessage('X range error');
   if not (ly in [0..ActiveSection.Tailer.YSize-1]) then ShowMessage('Y range error');
   if not (lz in [0..ActiveSection.Tailer.ZSize-1]) then ShowMessage('Z range error');}
end;

procedure TranslateClick2(WndIndex, sx, sy: Integer; var lx, ly, lz: Integer);
var
   p, q: Integer; // physical coords
begin
   {$ifdef DEBUG_FILE}
   FrmMain.DebugFile.Add('VoxelEngine: TranslateClick2');
   {$endif}
   //following code needs to be fixed/replaced
   with ActiveSection do
   begin
      //new conversion routines?
      p:=(sx - Viewport[WndIndex].Left) div Viewport[WndIndex].Zoom;
      q:=(sy - Viewport[WndIndex].Top) div Viewport[WndIndex].Zoom;
      //p:=Max(p,0);  //make sure p is >=0
      //q:=Max(q,0);  //same for q
      //also make sure they're in range of 0..(Width/Height/Depth)-1
      // p:=Min(p,View[WndIndex].Width - 1);
      // q:=Min(q,View[WndIndex].Height - 1);

      //px/py were from original version, but they give wrong values sometimes (because they use / and trunc instead of div)!
      //px := Min(Max(Trunc((sx - Viewport[WndIndex].Left) / Viewport[WndIndex].Zoom),0),View[WndIndex].Width - 1);
      //py := Min(Max(Ceil((sy - Viewport[WndIndex].Top) / Viewport[WndIndex].Zoom),0),View[WndIndex].Height - 1);

      View[WndIndex].TranslateClick(p,q,lx,ly,lz);
   end;
   //I want range checks - on lx,ly and lz
   //Range checks are already performed somewhere else, but where?!
   //They are flawed!
   //the only reason the program doesn't crash with X is because it can write in
   //other parts of the file!!!
{   if not (lx in [0..ActiveSection.Tailer.XSize-1]) then ShowMessage('X range error');
   if not (ly in [0..ActiveSection.Tailer.YSize-1]) then ShowMessage('Y range error');
   if not (lz in [0..ActiveSection.Tailer.ZSize-1]) then ShowMessage('Z range error');}
end;

procedure MoveCursor(lx, ly, lz: Integer; Repaint : boolean);
begin
   {$ifdef DEBUG_FILE}
   FrmMain.DebugFile.Add('VoxelEngine: Move Cursor');
   {$endif}
   with ActiveSection do
   begin
      SetX(lx);
      SetY(ly);
      SetZ(lz);
      View[0].Refresh;
      View[1].Refresh;
      View[2].Refresh;
   end;
   if Repaint then
      RepaintViews;
end;

Function GetPaletteColourFromVoxel(x,y, WndIndex : integer) : integer;
var
   Pos : TVector3i;
   v : TVoxelUnpacked;
begin
   {$ifdef DEBUG_FILE}
   FrmMain.DebugFile.Add('VoxelEngine: GetPaletteColourFromVoxel');
   {$endif}
   Result := -1;

   TranslateClick(WndIndex,x,y,Pos.x,Pos.y,Pos.z);
   ActiveSection.GetVoxel(Pos.x,Pos.y,Pos.z,v);

   if v.Used then
      if SpectrumMode = ModeColours then
         Result := v.Colour
      else
         Result := v.Normal;
end;

procedure ActivateView(Idx: Integer);
var
   swapView: TVoxelView;
   swapThumb: TThumbNail;
begin
   {$ifdef DEBUG_FILE}
   FrmMain.DebugFile.Add('VoxelEngine: ActivateView');
   {$endif}
   with ActiveSection do
   begin
      swapView := View[0];
      View[0] := View[Idx];
      View[Idx] := swapView;
      swapThumb := Thumb[0];
      Thumb[0] := Thumb[Idx];
      Thumb[Idx] := swapThumb;
      Viewport[0].Zoom := DefultZoom;
   end;
   SyncViews;
   CentreView(0);
   ZoomToFit(Idx);
   CentreView(Idx);
   RepaintViews;
end;

procedure SyncViews;
begin
   {$ifdef DEBUG_FILE}
   FrmMain.DebugFile.Add('VoxelEngine: SyncViews');
   {$endif}
   lblView[0].Caption := '  Editing View: ' + ViewName[ActiveSection.View[0].GetViewNameIdx];
   lblView[1].Caption := '  View: ' + ViewName[ActiveSection.View[1].GetViewNameIdx];
   lblView[2].Caption := '  View: ' + ViewName[ActiveSection.View[2].GetViewNameIdx];
end;

procedure RefreshViews;
begin
   {$ifdef DEBUG_FILE}
   FrmMain.DebugFile.Add('VoxelEngine: RefreshViews');
   {$endif}
   with ActiveSection do
   begin
      View[0].Refresh;
      View[1].Refresh;
      View[2].Refresh;
   end;
end;

function getgradient(last,first : TVector3i) : single;
begin
   {$ifdef DEBUG_FILE}
   FrmMain.DebugFile.Add('VoxelEngine: GetGradient');
   {$endif}
   if (first.X-last.X = 0) or (first.Y-last.Y = 0) then
      result := 0
   else
      result := (first.Y-last.Y) / (first.X-last.X);
end;

procedure drawstraightline(const a : TVoxelSection; var tempview : Ttempview; var last,first : TVector3i; v: TVoxelUnpacked);
var
x,y,ss : integer;
gradient,c : single;
o : byte;
begin
// Straight Line Equation : Y=MX+C
  o := 0;

  if (a.View[0].getOrient = oriX) then
  begin
  ss := last.x;
  last.X := last.Z;
  first.X := first.Z;
  o := 1;
  end
  else
  if (a.View[0].getOrient = oriY) then
  begin
  ss := last.y;
  last.Y := last.Z;
  first.Y := first.Z;
  o := 2;
  end
  else
  if (a.View[0].getOrient = oriZ) then
  ss := last.z
  else
  begin
  messagebox(0,'Error: Can`t Draw 3D Line','Math Error',0);
  exit;
  end;

gradient := getgradient(last,first);

c := last.Y-(last.X * gradient);

tempview.Data_no := 0;
setlength(tempview.Data,0);

if (first.X = last.X) then
for y := min(first.Y,last.y) to max(first.Y,last.y) do
begin
tempview.Data_no := tempview.Data_no +1;
setlength(tempview.Data,tempview.Data_no+1);

if o = 1 then
begin
tempview.Data[tempview.Data_no].X := y;
tempview.Data[tempview.Data_no].Y := first.X;

tempview.Data[tempview.Data_no].VU := true;
tempview.Data[tempview.Data_no].VC.X :=ss;
tempview.Data[tempview.Data_no].VC.Y :=y;
tempview.Data[tempview.Data_no].VC.Z :=first.X;
tempview.Data[tempview.Data_no].V := V;
end
else
if o = 2 then
begin
tempview.Data[tempview.Data_no].X := first.X;
tempview.Data[tempview.Data_no].Y := y;

tempview.Data[tempview.Data_no].VU := true;
tempview.Data[tempview.Data_no].VC.X :=first.X;
tempview.Data[tempview.Data_no].VC.Y :=ss;
tempview.Data[tempview.Data_no].VC.Z :=y;
tempview.Data[tempview.Data_no].V := V;
end
else
begin
tempview.Data[tempview.Data_no].X := first.X;
tempview.Data[tempview.Data_no].Y := y;

tempview.Data[tempview.Data_no].VU := true;
tempview.Data[tempview.Data_no].VC.X :=first.X;
tempview.Data[tempview.Data_no].VC.Y :=y;
tempview.Data[tempview.Data_no].VC.Z :=ss;
tempview.Data[tempview.Data_no].V := V;
end;
end
else
if (first.Y = last.Y) then
for x := min(first.x,last.x) to max(first.x,last.x) do
begin
tempview.Data_no := tempview.Data_no +1;
setlength(tempview.Data,tempview.Data_no+1);

if o = 1 then
begin
tempview.Data[tempview.Data_no].X := first.Y;
tempview.Data[tempview.Data_no].Y := x;

tempview.Data[tempview.Data_no].VU := true;
tempview.Data[tempview.Data_no].VC.X :=ss;
tempview.Data[tempview.Data_no].VC.Y :=first.y;
tempview.Data[tempview.Data_no].VC.Z :=x;
tempview.Data[tempview.Data_no].V := V;
end
else
if o = 2 then
begin
tempview.Data[tempview.Data_no].X := x;
tempview.Data[tempview.Data_no].Y := first.Y;

tempview.Data[tempview.Data_no].VU := true;
tempview.Data[tempview.Data_no].VC.X :=x;
tempview.Data[tempview.Data_no].VC.Y :=ss;
tempview.Data[tempview.Data_no].VC.Z :=first.y;
tempview.Data[tempview.Data_no].V := V;
end
else
begin
tempview.Data[tempview.Data_no].X := x;
tempview.Data[tempview.Data_no].Y := first.Y;

tempview.Data[tempview.Data_no].VU := true;
tempview.Data[tempview.Data_no].VC.X :=x;
tempview.Data[tempview.Data_no].VC.Y :=first.y;
tempview.Data[tempview.Data_no].VC.Z :=ss;
tempview.Data[tempview.Data_no].V := V;
end;
end
else
begin

for x := min(first.X,last.X) to max(first.X,last.X) do
begin
tempview.Data_no := tempview.Data_no +1;
setlength(tempview.Data,tempview.Data_no+1);

if o = 1 then
begin
tempview.Data[tempview.Data_no].X := round((gradient*x)+c);
tempview.Data[tempview.Data_no].Y := x;

tempview.Data[tempview.Data_no].VU := true;
tempview.Data[tempview.Data_no].VC.X :=ss;
tempview.Data[tempview.Data_no].VC.Y :=round((gradient*x)+c);
tempview.Data[tempview.Data_no].VC.Z :=x;
tempview.Data[tempview.Data_no].V := V;
end
else
if o = 2 then
begin
tempview.Data[tempview.Data_no].X := x;
tempview.Data[tempview.Data_no].Y := round((gradient*x)+c);

tempview.Data[tempview.Data_no].VU := true;
tempview.Data[tempview.Data_no].VC.X :=x;
tempview.Data[tempview.Data_no].VC.Y :=ss;
tempview.Data[tempview.Data_no].VC.Z :=round((gradient*x)+c);
tempview.Data[tempview.Data_no].V := V;
end
else
begin
tempview.Data[tempview.Data_no].X := x;
tempview.Data[tempview.Data_no].Y := round((gradient*x)+c);

tempview.Data[tempview.Data_no].VU := true;
tempview.Data[tempview.Data_no].VC.X :=x;
tempview.Data[tempview.Data_no].VC.Y :=round((gradient*x)+c);
tempview.Data[tempview.Data_no].VC.Z :=ss;
tempview.Data[tempview.Data_no].V := V;
end;

end;

for y := min(first.Y,last.Y) to max(first.Y,last.Y) do
begin

tempview.Data_no := tempview.Data_no +1;
setlength(tempview.Data,tempview.Data_no+1);

if o = 1 then
begin
tempview.Data[tempview.Data_no].X := y;
tempview.Data[tempview.Data_no].Y := round((y-c)/ gradient);

tempview.Data[tempview.Data_no].VU := true;
tempview.Data[tempview.Data_no].VC.X :=ss;
tempview.Data[tempview.Data_no].VC.Y :=y;
tempview.Data[tempview.Data_no].VC.Z :=round((y-c)/ gradient);
tempview.Data[tempview.Data_no].V := V;
end
else
if o = 2 then
begin
tempview.Data[tempview.Data_no].X := round((y-c)/ gradient);
tempview.Data[tempview.Data_no].Y := y;

tempview.Data[tempview.Data_no].VU := true;
tempview.Data[tempview.Data_no].VC.X :=round((y-c)/ gradient);
tempview.Data[tempview.Data_no].VC.Y :=ss;
tempview.Data[tempview.Data_no].VC.Z :=y;
tempview.Data[tempview.Data_no].V := V;
end
else
begin
tempview.Data[tempview.Data_no].X := round((y-c)/ gradient);
tempview.Data[tempview.Data_no].Y := y;

tempview.Data[tempview.Data_no].VU := true;
tempview.Data[tempview.Data_no].VC.X :=round((y-c)/ gradient);
tempview.Data[tempview.Data_no].VC.Y :=y;
tempview.Data[tempview.Data_no].VC.Z :=ss;
tempview.Data[tempview.Data_no].V := V;
end;

end;

end;

RemoveDoublesFromTempView;

end;

procedure VXLRectangle(Xpos,Ypos,Zpos,Xpos2,Ypos2,Zpos2:Integer; Fill: Boolean; v : TVoxelUnpacked);
{type
  EOrientRect = (oriUnDef, oriX, oriY, oriZ);  }
var
  i,j,k: Integer;
  O: EVoxelViewOrient;
  Inside,Exact: Integer;
begin
  O:=ActiveSection.View[0].getOrient;

  tempview.Data_no := 0;
  setlength(tempview.Data,0);

  {  //this isn't efficient...
  for i:=0 to Tailer.XSize do begin
    for j:=0 to Tailer.YSize do begin
      for k:=0 to Tailer.ZSize do begin}
  //this is better
  for i:=Min(Xpos,Xpos2) to Max(Xpos,Xpos2) do begin
    for j:=Min(Ypos,Ypos2) to Max(Ypos,Ypos2) do begin
      for k:=Min(Zpos,Zpos2) to Max(Zpos,Zpos2) do begin
        Inside:=0; Exact:=0;
        case O of
          oriX: begin
            if (i=Xpos) then begin //we're in the right slice
              if (j>Min(Ypos,Ypos2)) and (j<Max(Ypos,Ypos2)) then Inc(Inside);
              if (k>Min(Zpos,Zpos2)) and (k<Max(Zpos,Zpos2)) then Inc(Inside);
              if (j=Min(Ypos,Ypos2)) or (j=Max(Ypos,Ypos2)) then Inc(Exact);
              if (k=Min(Zpos,Zpos2)) or (k=Max(Zpos,Zpos2)) then Inc(Exact);
            end;
          end;
          oriY: begin
            if (j=Ypos) then begin //we're in the right slice
              if (i>Min(Xpos,Xpos2)) and (i<Max(Xpos,Xpos2)) then Inc(Inside);
              if (k>Min(Zpos,Zpos2)) and (k<Max(Zpos,Zpos2)) then Inc(Inside);
              if (i=Min(Xpos,Xpos2)) or (i=Max(Xpos,Xpos2)) then Inc(Exact);
              if (k=Min(Zpos,Zpos2)) or (k=Max(Zpos,Zpos2)) then Inc(Exact);
            end;
          end;
          oriZ: begin
            if (k=Zpos) then begin //we're in the right slice
              if (i>Min(Xpos,Xpos2)) and (i<Max(Xpos,Xpos2)) then Inc(Inside);
              if (j>Min(Ypos,Ypos2)) and (j<Max(Ypos,Ypos2)) then Inc(Inside);
              if (i=Min(Xpos,Xpos2)) or (i=Max(Xpos,Xpos2)) then Inc(Exact);
              if (j=Min(Ypos,Ypos2)) or (j=Max(Ypos,Ypos2)) then Inc(Exact);
            end;
          end;
        end;
        if Fill then begin
          if Inside+Exact=2 then begin
           tempview.Data_no := tempview.Data_no +1;
           setlength(tempview.Data,tempview.Data_no +1);
           if O = oriX then
           begin
           tempview.Data[tempview.Data_no].X := j;
           tempview.Data[tempview.Data_no].Y := k;
           end
           else
           if O = oriY then
           begin
           tempview.Data[tempview.Data_no].X := i;
           tempview.Data[tempview.Data_no].Y := k;
           end
           else
           if O = oriZ then
           begin
           tempview.Data[tempview.Data_no].X := i;
           tempview.Data[tempview.Data_no].Y := j;
           end;
           tempview.Data[tempview.Data_no].VU := true;
           tempview.Data[tempview.Data_no].VC.X := i;
           tempview.Data[tempview.Data_no].VC.Y := j;
           tempview.Data[tempview.Data_no].VC.Z := k;
           tempview.Data[tempview.Data_no].V := v;
           // SetVoxel(i,j,k,v);
          end;
        end else begin
          if (Exact>=1) and (Inside+Exact=2) then begin
          tempview.Data_no := tempview.Data_no +1;
           setlength(tempview.Data,tempview.Data_no +1);
           if O = oriX then
           begin
           tempview.Data[tempview.Data_no].X := j;
           tempview.Data[tempview.Data_no].Y := k;
           end
           else
           if O = oriY then
           begin
           tempview.Data[tempview.Data_no].X := i;
           tempview.Data[tempview.Data_no].Y := k;
           end
           else
           if O = oriZ then
           begin
           tempview.Data[tempview.Data_no].X := i;
           tempview.Data[tempview.Data_no].Y := j;
           end;
           tempview.Data[tempview.Data_no].VU := true;
           tempview.Data[tempview.Data_no].VC.X := i;
           tempview.Data[tempview.Data_no].VC.Y := j;
           tempview.Data[tempview.Data_no].VC.Z := k;
           tempview.Data[tempview.Data_no].V := v;
            //SetVoxel(i,j,k,v);
          end;
        end;
      end;
    end;
  end;
end;

Function ApplyNormalsToVXL(var VXL : TVoxelSection) : integer;
var
   Res : TApplyNormalsResult;
begin
   Res := ApplyNormals(VXL);
   VXLChanged := true;
   Result := Res.confused;
   MessageBox(0,pchar('AutoNormals v1.1' + #13#13 + 'Total: ' + inttostr(Res.applied + Res.confused) + #13 +'Applied: ' + inttostr(Res.applied) + #13 + 'Confused: ' +inttostr(Res.confused) {+ #13+ 'Redundent: ' +inttostr(Res.redundant)}),'6-Faced Auto Normal Results',0);
end;

Function ApplyCubedNormalsToVXL(var VXL : TVoxelSection) : integer;
var
Res : TApplyNormalsResult;
begin
    Res := ApplyCubedNormals(VXL,1.74,1,1,true,true,false);
    VXLChanged := true;
    Result := Res.applied;
    MessageBox(0,pchar('AutoNormals v5.2' + #13#13 + 'Total: ' + inttostr(Res.applied) + ' voxels modified.'),'Cubed Auto Normal Results',0);
end;

Function ApplyInfluenceNormalsToVXL(var VXL : TVoxelSection) : integer;
var
   Res : TApplyNormalsResult;
begin
    Res := ApplyInfluenceNormals(VXL,3.55,1,1,true,false,false);
    VXLChanged := true;
    Result := Res.applied;
    MessageBox(0,pchar('AutoNormals v6.1' + #13#13 + 'Total: ' + inttostr(Res.applied) + ' voxels modified.'),'Cubed Auto Normal Results',0);
end;


Function RemoveRedundantVoxelsFromVXL(var VXL : TVoxelSection) : integer;
begin
   result := RemoveRedundantVoxels(VXL);
   result := result + RemoveRedundantVoxelsOld(VXL);
   VXLChanged := true;
end;

procedure UpdateHistoryMenu;
var
   i: Integer;
   S: String;
begin
   {$ifdef DEBUG_FILE}
   FrmMain.DebugFile.Add('VoxelEngine: UpdateHistoryMenu');
   {$endif}
   //update the history menu (recently used file menu, whatever it's called)
   for i:=0 to HistoryDepth - 1 do
   begin
      S := Config.GetHistory(i);
      mnuHistory[i].Tag:=i;
      if S='' then
         mnuHistory[i].Visible:=False
      else
         mnuHistory[i].Visible:=True;
      mnuHistory[i].Caption:='&'+IntToStr(i+1)+' '+S;
   end;
end;

procedure VXLBrushTool(VXL : TVoxelSection; Xc,Yc,Zc: Integer; V: TVoxelUnpacked; BrushMode: Integer; BrushView: EVoxelViewOrient);
var
   Shape: Array[-5..5,-5..5] of 0..1;
   i,j,r1,r2,x,y: Integer;
begin
   {$ifdef DEBUG_FILE}
   FrmMain.DebugFile.Add('VoxelEngine: VXLBrushTool');
   {$endif}
   randomize;
   for i:=-5 to 5 do
      for j:=-5 to 5 do
         Shape[i,j]:=0;
   Shape[0,0]:=1;
   if BrushMode>=1 then
   begin
      Shape[0,1]:=1; Shape[0,-1]:=1; Shape[1,0]:=1; Shape[-1,0]:=1;
   end;
   if BrushMode>=2 then
   begin
      Shape[1,1]:=1; Shape[1,-1]:=1; Shape[-1,-1]:=1; Shape[-1,1]:=1;
   end;
   if BrushMode>=3 then
   begin
      Shape[0,2]:=1; Shape[0,-2]:=1; Shape[2,0]:=1; Shape[-2,0]:=1;
   end;
   if BrushMode =4 then
   begin
      for i:=-5 to 5 do
         for j:=-5 to 5 do
            Shape[i,j]:=0;

      for i:=1 to 3 do
      begin
         r1 := random(7)-3; r2 := random(7)-3;
         Shape[r1,r2]:=1;
      end;
   end;
   //Brush completed, now actually use it!
   //for every pixel of the brush, check if we need to draw it (Shape),
   //if so, use the correct view to set the voxel (with range checking!)

   for i:=-5 to 5 do
   begin
      for j:=-5 to 5 do
      begin
         if Shape[i,j]=1 then
         begin
            tempview.Data_no := tempview.Data_no +1;
            setlength(tempview.Data,tempview.Data_no +1);

            case BrushView of
               oriX:
               Begin
                  tempview.Data[tempview.Data_no].VC.X := Xc;
                  tempview.Data[tempview.Data_no].VC.Y := Max(Min(Yc+i,VXL.Tailer.YSize-1),0);
                  tempview.Data[tempview.Data_no].VC.Z := Max(Min(Zc+j,VXL.Tailer.ZSize-1),0);

                  tempview.Data[tempview.Data_no].X := tempview.Data[tempview.Data_no].VC.Y;
                  tempview.Data[tempview.Data_no].Y := tempview.Data[tempview.Data_no].VC.Z;

                  //VXL.SetVoxel(Xc,Max(Min(Yc+i,VXL.Tailer.YSize-1),0),Max(Min(Zc+j,VXL.Tailer.ZSize-1),0),v);
               end;
               oriY:
               Begin
                  tempview.Data[tempview.Data_no].VC.X := Max(Min(Xc+i,VXL.Tailer.XSize-1),0);
                  tempview.Data[tempview.Data_no].VC.Y := Yc;
                  tempview.Data[tempview.Data_no].VC.Z := Max(Min(Zc+j,VXL.Tailer.ZSize-1),0);

                  tempview.Data[tempview.Data_no].X := Max(Min(Xc+i,VXL.Tailer.XSize-1),0);
                  tempview.Data[tempview.Data_no].Y := Max(Min(Zc+j,VXL.Tailer.ZSize-1),0);

                  //VXL.SetVoxel(Max(Min(Xc+i,VXL.Tailer.XSize-1),0),Yc,Max(Min(Zc+j,VXL.Tailer.ZSize-1),0),v);
               end;
               oriZ:
               Begin
                  tempview.Data[tempview.Data_no].VC.X := Max(Min(Xc+i,VXL.Tailer.XSize-1),0);
                  tempview.Data[tempview.Data_no].VC.Y := Max(Min(Yc+j,VXL.Tailer.YSize-1),0);
                  tempview.Data[tempview.Data_no].VC.Z := Zc;

                  tempview.Data[tempview.Data_no].X := Max(Min(Xc+i,VXL.Tailer.XSize-1),0);
                  tempview.Data[tempview.Data_no].Y := Max(Min(Yc+j,VXL.Tailer.YSize-1),0);

                  //VXL.SetVoxel(Max(Min(Xc+i,VXL.Tailer.XSize-1),0),Max(Min(Yc+j,VXL.Tailer.YSize-1),0),Zc,v);
               end;
            end;
            tempview.Data[tempview.Data_no].VU := true;
            tempview.Data[tempview.Data_no].V := v;
         end;
      end;
   end;

   if tempview.Data_no > 0 then
      for x := 1 to tempview.Data_no do
         for y := x to tempview.Data_no do
            if y <> x then
               if tempview.Data[x].VU then
                  if (tempview.Data[x].X = tempview.Data[y].X) and (tempview.Data[x].Y = tempview.Data[y].Y) then
                     tempview.Data[x].VU := false;
end;

function DarkenLightenEnv(V: TVoxelUnpacked; Darken : Boolean) : TVoxelUnpacked;
var
   VUP : TVoxelUnpacked;
begin
   {$ifdef DEBUG_FILE}
   FrmMain.DebugFile.Add('VoxelEngine: DarkenLightenEnv');
   {$endif}
   VUP := V;
   if Darken then
      if VUP.Used then
      begin
         if (SpectrumMode = ModeColours) then
            VUP.Colour := VUP.Colour + DarkenLighten;
         if (SpectrumMode = ModeNormals) then
            VUP.Normal := VUP.Normal + DarkenLighten;
      end;

   if not Darken then
      if VUP.Used then
      begin
         if (SpectrumMode = ModeColours) then
            VUP.Colour := VUP.Colour - DarkenLighten;
         if (SpectrumMode = ModeNormals) then
            VUP.Normal := VUP.Normal - DarkenLighten;
      end;

   if VUP.Normal > ActiveNormalsCount then
      VUP.Normal := ActiveNormalsCount;

   result := VUP;
end;

procedure VXLBrushToolDarkenLighten(VXL : TVoxelSection; Xc,Yc,Zc: Integer; BrushMode: Integer; BrushView: EVoxelViewOrient; Darken : Boolean);
var
   Shape: Array[-5..5,-5..5] of 0..1;
   i,j,r1,r2: Integer;
   v : TVoxelUnpacked;
begin
   {$ifdef DEBUG_FILE}
   FrmMain.DebugFile.Add('VoxelEngine: VXLBrushToolDarkenLighten');
   {$endif}
   randomize;
   for i:=-5 to 5 do
      for j:=-5 to 5 do
         Shape[i,j]:=0;
   Shape[0,0]:=1;
   if BrushMode>=1 then
   begin
      Shape[0,1]:=1; Shape[0,-1]:=1; Shape[1,0]:=1; Shape[-1,0]:=1;
   end;
   if BrushMode>=2 then
   begin
      Shape[1,1]:=1; Shape[1,-1]:=1; Shape[-1,-1]:=1; Shape[-1,1]:=1;
   end;
   if BrushMode>=3 then
   begin
      Shape[0,2]:=1; Shape[0,-2]:=1; Shape[2,0]:=1; Shape[-2,0]:=1;
   end;
   if BrushMode =4 then
   begin
      for i:=-5 to 5 do
         for j:=-5 to 5 do
            Shape[i,j]:=0;

      for i:=1 to 3 do
      begin
         r1 := random(7)-3; r2 := random(7)-3;
         Shape[r1,r2]:=1;
      end;
   end;
   //Brush completed, now actually use it!
   //for every pixel of the brush, check if we need to draw it (Shape),
   //if so, use the correct view to set the voxel (with range checking!)

   for i:=-5 to 5 do
   begin
      for j:=-5 to 5 do
      begin
         if Shape[i,j]=1 then
         begin
            tempview.Data_no := tempview.Data_no +1;
            setlength(tempview.Data,tempview.Data_no +1);

            case BrushView of
               oriX:
               Begin
                  tempview.Data[tempview.Data_no].VC.X := Xc;
                  tempview.Data[tempview.Data_no].VC.Y := Max(Min(Yc+i,VXL.Tailer.YSize-1),0);
                  tempview.Data[tempview.Data_no].VC.Z := Max(Min(Zc+j,VXL.Tailer.ZSize-1),0);

                  tempview.Data[tempview.Data_no].X := tempview.Data[tempview.Data_no].VC.Y;
                  tempview.Data[tempview.Data_no].Y := tempview.Data[tempview.Data_no].VC.Z;

                  //VXL.SetVoxel(Xc,Max(Min(Yc+i,VXL.Tailer.YSize-1),0),Max(Min(Zc+j,VXL.Tailer.ZSize-1),0),v);
               end;
               oriY:
               Begin
                  tempview.Data[tempview.Data_no].VC.X := Max(Min(Xc+i,VXL.Tailer.XSize-1),0);
                  tempview.Data[tempview.Data_no].VC.Y := Yc;
                  tempview.Data[tempview.Data_no].VC.Z := Max(Min(Zc+j,VXL.Tailer.ZSize-1),0);

                  tempview.Data[tempview.Data_no].X := Max(Min(Xc+i,VXL.Tailer.XSize-1),0);
                  tempview.Data[tempview.Data_no].Y := Max(Min(Zc+j,VXL.Tailer.ZSize-1),0);

                  //VXL.SetVoxel(Max(Min(Xc+i,VXL.Tailer.XSize-1),0),Yc,Max(Min(Zc+j,VXL.Tailer.ZSize-1),0),v);
               end;
               oriZ:
               Begin
                  tempview.Data[tempview.Data_no].VC.X := Max(Min(Xc+i,VXL.Tailer.XSize-1),0);
                  tempview.Data[tempview.Data_no].VC.Y := Max(Min(Yc+j,VXL.Tailer.YSize-1),0);
                  tempview.Data[tempview.Data_no].VC.Z := Zc;

                  tempview.Data[tempview.Data_no].X := Max(Min(Xc+i,VXL.Tailer.XSize-1),0);
                  tempview.Data[tempview.Data_no].Y := Max(Min(Yc+j,VXL.Tailer.YSize-1),0);

                  //VXL.SetVoxel(Max(Min(Xc+i,VXL.Tailer.XSize-1),0),Max(Min(Yc+j,VXL.Tailer.YSize-1),0),Zc,v);
               end;
            end;

            tempview.Data[tempview.Data_no].VU := true;
            VXL.GetVoxel(tempview.Data[tempview.Data_no].VC.X,tempview.Data[tempview.Data_no].VC.Y,tempview.Data[tempview.Data_no].VC.Z,v);
            tempview.Data[tempview.Data_no].V := DarkenLightenEnv(v,Darken);
         end;
      end;
   end;

   RemoveDoublesFromTempView;
end;

Procedure RemoveDoublesFromTempView;
var
x,y : integer;
begin
   {$ifdef DEBUG_FILE}
   FrmMain.DebugFile.Add('VoxelEngine: RemoveDoublesFromTempView');
   {$endif}
   if tempview.Data_no > 0 then
      for x := 1 to tempview.Data_no do
         for y := x to tempview.Data_no do
            if y <> x then
               if tempview.Data[x].VU then
                  if (tempview.Data[x].X = tempview.Data[y].X) and (tempview.Data[x].Y = tempview.Data[y].Y) then
                     tempview.Data[x].VU := false;
end;

Function ApplyTempView(var vxl :TVoxelSection): Boolean;
var
   v : TVoxelUnpacked;
   i : integer;
begin
   {$ifdef DEBUG_FILE}
   FrmMain.DebugFile.Add('VoxelEngine: ApplyTempView');
   {$endif}
   // 1.2: Fix Flood Fill Outside Voxel Undo Bug
   Result := false;
   if TempView.Data_no > 0 then
   begin
      // 1.2: Fix Flood Fill Outside Voxel Undo Bug
      Result := CreateRestorePoint(TempView,Undo);

      for i := 1 to TempView.Data_no do
      if TempView.Data[i].VU then // If VU = true apply to VXL
      begin
         vxl.GetVoxel(TempView.Data[i].VC.X,TempView.Data[i].VC.Y,TempView.Data[i].VC.Z,v);
         if (SpectrumMode = ModeColours) or (v.Used=False) then
            v.Colour := TempView.Data[i].V.Colour;
         if (SpectrumMode = ModeNormals) or (v.Used=False) then
            v.Normal := TempView.Data[i].V.Normal;
         v.Used := TempView.Data[i].V.Used;

         vxl.SetVoxel(TempView.Data[i].VC.X,TempView.Data[i].VC.Y,TempView.Data[i].VC.Z,v);
      end;

      TempView.Data_no := 0;
      SetLength(TempView.Data,0);
      // 1.2 Fix: CreateRestorePoint will confirm VXLChanged.
      //   VXLChanged := true;
   end;
end;

procedure ClearVXLLayer(var Vxl : TVoxelSection);
var
   v : TVoxelUnpacked;
   x,y,z : integer;
begin
   {$ifdef DEBUG_FILE}
   FrmMain.DebugFile.Add('VoxelEngine: ClearVXLLayer');
   {$endif}
   tempview.Data_no := 0;
   setlength(tempview.Data,0);

   if vxl.View[0].GetOrient = oriX then
   begin
      for z := 0 to vxl.Tailer.ZSize-1 do
         for y := 0 to vxl.Tailer.YSize-1 do
         begin
            vxl.GetVoxel(vxl.X,y,z,v);
            v.Used := false;

            tempview.Data_no := tempview.Data_no +1;
            setlength(tempview.Data,tempview.Data_no +1);

            tempview.Data[tempview.Data_no].VC.X := vxl.X;
            tempview.Data[tempview.Data_no].VC.Y := y;
            tempview.Data[tempview.Data_no].VC.Z := z;

            tempview.Data[tempview.Data_no].X := 0;
            tempview.Data[tempview.Data_no].Y := 0;

            tempview.Data[tempview.Data_no].VU := true;
            tempview.Data[tempview.Data_no].V := v;

            //vxl.SetVoxel(vxl.X,y,z,v);
         end;
   end;

   if vxl.View[0].GetOrient = oriY then
   begin
      for x := 0 to vxl.Tailer.XSize-1 do
         for z := 0 to vxl.Tailer.ZSize-1 do
         begin
            vxl.GetVoxel(x,vxl.Y,z,v);
            v.Used := false;

            tempview.Data_no := tempview.Data_no +1;
            setlength(tempview.Data,tempview.Data_no +1);

            tempview.Data[tempview.Data_no].VC.X := x;
            tempview.Data[tempview.Data_no].VC.Y := vxl.Y;
            tempview.Data[tempview.Data_no].VC.Z := z;

            tempview.Data[tempview.Data_no].X := 0;
            tempview.Data[tempview.Data_no].Y := 0;

            tempview.Data[tempview.Data_no].VU := true;
            tempview.Data[tempview.Data_no].V := v;

            //vxl.SetVoxel(x,vxl.Y,z,v);
         end;
   end;

   if vxl.View[0].GetOrient = oriZ then
   begin
      for x := 0 to vxl.Tailer.XSize-1 do
         for y := 0 to vxl.Tailer.YSize-1 do
         begin
            vxl.GetVoxel(x,y,vxl.z,v);
            v.Used := false;

            tempview.Data_no := tempview.Data_no +1;
            setlength(tempview.Data,tempview.Data_no +1);

            tempview.Data[tempview.Data_no].VC.X := x;
            tempview.Data[tempview.Data_no].VC.Y := y;
            tempview.Data[tempview.Data_no].VC.Z := vxl.z;

            tempview.Data[tempview.Data_no].X := 0;
            tempview.Data[tempview.Data_no].Y := 0;

            tempview.Data[tempview.Data_no].VU := true;
            tempview.Data[tempview.Data_no].V := v;

            //vxl.SetVoxel(x,y,vxl.z,v);
         end;
   end;
   ApplyTempView(Vxl);
end;

Procedure VXLCopyToClipboard(Vxl : TVoxelSection);
var
   x,y,z : integer;
   v : tvoxelunpacked;
   image : TBitmap;
begin
   {$ifdef DEBUG_FILE}
   FrmMain.DebugFile.Add('VoxelEngine: VXLCopyToClipboard');
   {$endif}
   image := TBitmap.Create;
   image.Canvas.Brush.Color := GetVXLPaletteColor(-1);
   image.Canvas.Brush.Style := bsSolid;

   if ActiveSection.View[0].GetOrient = oriX then
   begin
      image.Width := ActiveSection.Tailer.ZSize;
      image.Height := ActiveSection.Tailer.YSize;
      for z := 0 to ActiveSection.Tailer.ZSize-1 do
         for y := 0 to ActiveSection.Tailer.YSize-1 do
         begin
            ActiveSection.GetVoxel(ActiveSection.X,y,z,v);

            if v.Used then
               If SpectrumMode = ModeColours then
                  image.Canvas.Pixels[z,y] := GetVXLPaletteColor(v.Colour)
               else
                  image.Canvas.Pixels[z,y] := GetVXLPaletteColor(v.Normal);
         end;
   end;

   if ActiveSection.View[0].GetOrient = oriY then
   begin
      image.Width := ActiveSection.Tailer.XSize;
      image.Height := ActiveSection.Tailer.ZSize;
      for x := 0 to ActiveSection.Tailer.XSize-1 do
         for z := 0 to ActiveSection.Tailer.ZSize-1 do
         begin
            ActiveSection.GetVoxel(x,ActiveSection.Y,z,v);

            if v.Used then
               If SpectrumMode = ModeColours then
                  image.Canvas.Pixels[x,z] := GetVXLPaletteColor(v.Colour)
               else
                  image.Canvas.Pixels[x,z] := GetVXLPaletteColor(v.Normal);
         end;
   end;

   if ActiveSection.View[0].GetOrient = oriZ then
   begin
      image.Width := ActiveSection.Tailer.XSize;
      image.Height := ActiveSection.Tailer.YSize;
      for x := 0 to ActiveSection.Tailer.XSize-1 do
         for y := 0 to ActiveSection.Tailer.YSize-1 do
         begin
            ActiveSection.GetVoxel(x,y,ActiveSection.z,v);

            if v.Used then
               If SpectrumMode = ModeColours then
                  image.Canvas.Pixels[x,y] := GetVXLPaletteColor(v.Colour)
               else
                  image.Canvas.Pixels[x,y] := GetVXLPaletteColor(v.Normal);
         end;
   end;

   Clipboard.Clear;
   Clipboard.Assign(image);
end;

Procedure VXLCutToClipboard(Vxl : TVoxelSection);
begin
   {$ifdef DEBUG_FILE}
   FrmMain.DebugFile.Add('VoxelEngine: VXLCutToClipboard');
   {$endif}
   VXLCopyToClipboard(VXL);
   ClearVXLLayer(VXL);
end;

Procedure AddtoTempView(X,Y,Z : integer; V : TVoxelUnpacked; O : EVoxelViewOrient);
begin
   {$ifdef DEBUG_FILE}
   FrmMain.DebugFile.Add('VoxelEngine: AddToTempView');
   {$endif}
   tempview.Data_no := tempview.Data_no +1;
   setlength(tempview.Data,tempview.Data_no +1);

   tempview.Data[tempview.Data_no].VC.X := x;
   tempview.Data[tempview.Data_no].VC.Y := y;
   tempview.Data[tempview.Data_no].VC.Z := z;

   case O of
      oriX:
      begin
         tempview.Data[tempview.Data_no].X := Y;
         tempview.Data[tempview.Data_no].Y := Z;
      end;
      oriY:
      begin
         tempview.Data[tempview.Data_no].X := X;
         tempview.Data[tempview.Data_no].Y := Z;
      end;
      oriZ:
      begin
         tempview.Data[tempview.Data_no].X := X;
         tempview.Data[tempview.Data_no].Y := Y;
      end;
   end;

   tempview.Data[tempview.Data_no].VU := true;
   tempview.Data[tempview.Data_no].V := v;
end;

// Paste the full voxel into another O.o
Procedure PasteFullVXL(var Vxl : TVoxelsection);
var
   x,y,z : integer;
   image : TBitmap;
   v : tvoxelunpacked;
begin
   Image := nil;
   // Security Check
   if not isEditable then exit;

   {$ifdef DEBUG_FILE}
   FrmMain.DebugFile.Add('VoxelEngine: PasteFullVXL');
   {$endif}
   // Prepare the voxel mapping image.
   image := TBitmap.Create;
   image.Canvas.Brush.Color := GetVXLPaletteColor(-1);
   image.Canvas.Brush.Style := bsSolid;
   image.Assign(Clipboard);

   // Check if it's oriented to axis x
   if Vxl.View[0].GetOrient = oriX then
   begin
      for z := 0 to Vxl.Tailer.ZSize-1 do
      for y := 0 to Vxl.Tailer.YSize-1 do
      begin
         // Get current voxel block data
         Vxl.GetVoxel(Vxl.X,y,z,v);

         // Check if voxel is used
         if image.Canvas.Pixels[z,y] <> GetVXLPaletteColor(-1) then
            v.Used := true
         else
            v.Used := false;

         // Verify the colour/normal
         If SpectrumMode = ModeColours then
            v.Colour := getsquarecolour(z,y,image)
         else
            v.Normal := getnormalcolour(z,y,image);

         // Update voxel
         Vxl.SetVoxel(Vxl.X,y,z,v);
      end;
   end;

   // Check if it's oriented to axis y
   if Vxl.View[0].GetOrient = oriY then
   begin
      for x := 0 to Vxl.Tailer.XSize-1 do
      for z := 0 to Vxl.Tailer.ZSize-1 do
      begin
         // Get current voxel block data
         Vxl.GetVoxel(x,Vxl.y,z,v);

         // Check if voxel is used
         if image.Canvas.Pixels[x,z] <> GetVXLPaletteColor(-1) then
            v.Used := true
         else
            v.Used := false;

         // Verify the colour/normal
         If SpectrumMode = ModeColours then
            v.Colour := getsquarecolour(x,z,image)
         else
            v.Normal := getnormalcolour(x,z,image);

         // Update voxel
         Vxl.SetVoxel(x,Vxl.y,z,v);
      end;
   end;

   // Check if it's oriented to axis z
   if Vxl.View[0].GetOrient = oriZ then
   begin
      for x := 0 to Vxl.Tailer.XSize-1 do
      for y := 0 to Vxl.Tailer.YSize-1 do
      begin
         // Get current voxel block data
         Vxl.GetVoxel(x,y,Vxl.z,v);

         // Check if voxel is used
         if image.Canvas.Pixels[x,y] <> GetVXLPaletteColor(-1) then
            v.Used := true
         else
            v.Used := false;

         // Verify the colour/normal
         If SpectrumMode = ModeColours then
            v.Colour := getsquarecolour(x,y,image)
         else
            v.Normal := getnormalcolour(x,y,image);

         // Update voxel
         Vxl.SetVoxel(x,y,Vxl.z,v);
      end;
   end;
end;

Procedure PasteVXL(var Vxl : TVoxelsection);
var
   x,y,z : integer;
   image : TBitmap;
   v : tvoxelunpacked;
begin
   // Security Check
   if not isEditable then exit;

   {$ifdef DEBUG_FILE}
   FrmMain.DebugFile.Add('VoxelEngine: PasteVXL');
   {$endif}

   image := TBitmap.Create;
   image.Canvas.Brush.Color := GetVXLPaletteColor(-1);
   image.Canvas.Brush.Style := bsSolid;
   image.Assign(Clipboard);

   if Vxl.View[0].GetOrient = oriX then
   begin
      for z := 0 to Vxl.Tailer.ZSize-1 do
      for y := 0 to Vxl.Tailer.YSize-1 do
      begin
         Vxl.GetVoxel(Vxl.X,y,z,v);

         if image.Canvas.Pixels[z,y] <> GetVXLPaletteColor(-1) then
            v.Used := true
         else
            v.Used := false;

         If SpectrumMode = ModeColours then
            v.Colour := getsquarecolour(z,y,image)
         else
            v.Normal := getnormalcolour(z,y,image);

         if v.Used = true then
            Vxl.SetVoxel(Vxl.X,y,z,v);
      end;
   end;

   if Vxl.View[0].GetOrient = oriY then
   begin
      for x := 0 to Vxl.Tailer.XSize-1 do
      for z := 0 to Vxl.Tailer.ZSize-1 do
      begin
         Vxl.GetVoxel(x,Vxl.y,z,v);

         if image.Canvas.Pixels[x,z] <> GetVXLPaletteColor(-1) then
            v.Used := true
         else
            v.Used := false;

         If SpectrumMode = ModeColours then
            v.Colour := getsquarecolour(x,z,image)
         else
            v.Normal := getnormalcolour(x,z,image);

         if v.Used = true then
            Vxl.SetVoxel(x,Vxl.y,z,v);
      end;
   end;

   if Vxl.View[0].GetOrient = oriZ then
   begin
      for x := 0 to Vxl.Tailer.XSize-1 do
      for y := 0 to Vxl.Tailer.YSize-1 do
      begin
         Vxl.GetVoxel(x,y,Vxl.z,v);

         if image.Canvas.Pixels[x,y] <> GetVXLPaletteColor(-1) then
            v.Used := true
         else
            v.Used := false;

         If SpectrumMode = ModeColours then
            v.Colour := getsquarecolour(x,y,image)
         else
            v.Normal := getnormalcolour(x,y,image);

         if v.Used = true then
            Vxl.SetVoxel(x,y,Vxl.z,v);
      end;
   end;

end;

procedure VXLFloodFillTool(Vxl : TVoxelSection; Xpos,Ypos,Zpos: Integer; v: TVoxelUnpacked; EditView: EVoxelViewOrient);
type
  FloodSet = (Left,Right,Up,Down);
  Flood3DPoint = record
    X,Y,Z: Integer;
  end;
  StackType = record
    Dir: set of FloodSet;
    p: Flood3DPoint;
  end;
  function PointOK(l: Flood3DPoint): Boolean;
  begin
    PointOK:=False;
    if (l.X<0) or (l.Y<0) or (l.Z<0) then Exit;
    if (l.X>=vxl.Tailer.XSize) or (l.Y>=vxl.Tailer.YSize) or (l.Z>=vxl.Tailer.ZSize) then Exit;
    PointOK:=True;
  end;
var
   z1,z2: TVoxelUnpacked;
   i,j,k: Integer;         //this isn't 100% FloodFill, but function is very handy for user;
   Stack: Array of StackType; //this is the floodfill stack for my code
   SC,Sp: Integer; //stack counter and stack pointer
   po: Flood3DPoint;
   Full: set of FloodSet;
   Done: Array of Array of Array of Boolean;
begin
   {$ifdef DEBUG_FILE}
   FrmMain.DebugFile.Add('VoxelEngine: VXLFloodFillTool');
   {$endif}

   tempview.Data_no := 0;
   setlength(tempview.Data,0);

   SetLength(Done,vxl.Tailer.XSize,vxl.Tailer.YSize,vxl.Tailer.ZSize);
   SetLength(Stack,vxl.Tailer.XSize*vxl.Tailer.YSize*vxl.Tailer.ZSize);
   //this array avoids creation of extra stack objects when it isn't needed.
   for i:=0 to vxl.Tailer.XSize - 1 do
     for j:=0 to vxl.Tailer.YSize - 1 do
       for k:=0 to vxl.Tailer.ZSize - 1 do
         Done[i,j,k]:=False;

   vxl.GetVoxel(Xpos,Ypos,Zpos,z1);
   AddtoTempView(Xpos,Ypos,Zpos,v,EditView);

   Full:=[Left,Right,Up,Down];
   Sp:=0;
   Stack[Sp].Dir:=Full;
   Stack[Sp].p.X:=Xpos; Stack[Sp].p.Y:=Ypos; Stack[Sp].p.Z:=Zpos;
   SC:=1;
   while (SC>0) do
   begin
      if Left in Stack[Sp].Dir then
      begin //it's in there - check left
         //not in there anymore! we're going to do that one now.
         Stack[Sp].Dir:=Stack[Sp].Dir - [Left];
         po:=Stack[Sp].p;
         case EditView of
            oriX: Dec(po.Y);
            oriY: Dec(po.X);
            oriZ: Dec(po.X);
         end;
        //now check this point - only if it's within range, check it.
        if PointOK(po) then
        begin
           vxl.GetVoxel(po.X,po.Y,po.Z,z2);
           if z2.Colour=z1.Colour then
           begin
              AddtoTempView(po.X,po.Y,po.Z,v,EditView);
              if not Done[po.X,po.Y,po.Z] then
              begin
                 Stack[SC].Dir:=Full-[Right]; //Don't go back
                 Stack[SC].p:=po;
                 Inc(SC);
                 Inc(Sp); //increase stack pointer
              end;
              Done[po.X,po.Y,po.Z]:=True;
           end;
        end;
      end;
      if Right in Stack[Sp].Dir then
      begin //it's in there - check left
         //not in there anymore! we're going to do that one now.
         Stack[Sp].Dir:=Stack[Sp].Dir - [Right];
         po:=Stack[Sp].p;
         case EditView of
            oriX: Inc(po.Y);
            oriY: Inc(po.X);
            oriZ: Inc(po.X);
         end;
         //now check this point - only if it's within range, check it.
         if PointOK(po) then
         begin
            vxl.GetVoxel(po.X,po.Y,po.Z,z2);
            if z2.Colour=z1.Colour then
            begin
               AddtoTempView(po.X,po.Y,po.Z,v,EditView);
               if not Done[po.X,po.Y,po.Z] then
               begin
                  Stack[SC].Dir:=Full-[Left]; //Don't go back
                  Stack[SC].p:=po;
                  Inc(SC);
                  Inc(Sp); //increase stack pointer
               end;
               Done[po.X,po.Y,po.Z]:=True;
            end;
         end;
      end;
      if Up in Stack[Sp].Dir then
      begin //it's in there - check left
         //not in there anymore! we're going to do that one now.
         Stack[Sp].Dir:=Stack[Sp].Dir - [Up];
         po:=Stack[Sp].p;
         case EditView of
            oriX: Dec(po.Z);
            oriY: Dec(po.Z);
            oriZ: Dec(po.Y);
         end;
         //now check this point - only if it's within range, check it.
         if PointOK(po) then
         begin
            vxl.GetVoxel(po.X,po.Y,po.Z,z2);
            if z2.Colour=z1.Colour then
            begin
               AddtoTempView(po.X,po.Y,po.Z,v,EditView);
               if not Done[po.X,po.Y,po.Z] then
               begin
                  Stack[SC].Dir:=Full-[Down]; //Don't go back
                  Stack[SC].p:=po;
                  Inc(SC);
                  Inc(Sp); //increase stack pointer
               end;
               Done[po.X,po.Y,po.Z]:=True;
            end;
         end;
      end;
      if Down in Stack[Sp].Dir then
      begin //it's in there - check left
         //not in there anymore! we're going to do that one now.
         Stack[Sp].Dir:=Stack[Sp].Dir - [Down];
         po:=Stack[Sp].p;
         case EditView of
            oriX: Inc(po.Z);
            oriY: Inc(po.Z);
            oriZ: Inc(po.Y);
         end;
         //now check this point - only if it's within range, check it.
         if PointOK(po) then
         begin
            vxl.GetVoxel(po.X,po.Y,po.Z,z2);
            if z2.Colour=z1.Colour then
            begin
               AddtoTempView(po.X,po.Y,po.Z,v,EditView);
               if not Done[po.X,po.Y,po.Z] then
               begin
                  Stack[SC].Dir:=Full-[Up]; //Don't go back
                  Stack[SC].p:=po;
                  Inc(SC);
                  Inc(Sp); //increase stack pointer
               end;
               Done[po.X,po.Y,po.Z]:=True;
            end;
         end;
      end;
      if (Stack[Sp].Dir = []) then
      begin
         Dec(Sp);
         Dec(SC);
         //decrease stack pointer and stack count
      end;
   end;
   SetLength(Stack,0); // Free Up Memory
   RemoveDoublesFromTempView;
   for i := Low(Done) to High(Done) do
   begin
      for j := Low(Done[i]) to High(Done[i]) do
         SetLength(Done[i,j],0);
      SetLength(Done[i],0);
   end;
   SetLength(Done,0);
end;

Procedure SmoothVXLNormals(var Vxl : TVoxelSection);
var
   Res : TApplyNormalsResult;
begin
   Res := SmoothNormals(Vxl);
   MessageBox(0,pchar('Smooth Normals v1.0' + #13#13 + 'Total: ' + inttostr(Res.applied + Res.confused) + #13 +'Applyed: ' + inttostr(Res.applied) + #13 + 'Confused: ' +inttostr(Res.confused) {+ #13+ 'Redundent: ' +inttostr(Res.redundant)}),'Smooth Normals Result',0);
end;

procedure VXLSmoothBrushTool(VXL : TVoxelSection; Xc,Yc,Zc: Integer; V: TVoxelUnpacked; BrushMode: Integer; BrushView: EVoxelViewOrient);
var
   Shape: Array[-5..5,-5..5] of 0..1;
   i,j,r1,r2,x,y: Integer;
   VV : TVoxelUnpacked;
begin
   {$ifdef DEBUG_FILE}
   FrmMain.DebugFile.Add('VoxelEngine: VXLSmoothBrushTool');
   {$endif}
   randomize;
   for i:=-5 to 5 do
      for j:=-5 to 5 do
         Shape[i,j]:=0;
   Shape[0,0]:=1;
   if BrushMode>=1 then
   begin
      Shape[0,1]:=1; Shape[0,-1]:=1; Shape[1,0]:=1; Shape[-1,0]:=1;
   end;
   if BrushMode>=2 then
   begin
      Shape[1,1]:=1; Shape[1,-1]:=1; Shape[-1,-1]:=1; Shape[-1,1]:=1;
   end;
   if BrushMode>=3 then
   begin
      Shape[0,2]:=1; Shape[0,-2]:=1; Shape[2,0]:=1; Shape[-2,0]:=1;
   end;
   if BrushMode =4 then
   begin
      for i:=-5 to 5 do
         for j:=-5 to 5 do
            Shape[i,j]:=0;

      for i:=1 to 3 do
      begin
         r1 := random(7)-3; r2 := random(7)-3;
         Shape[r1,r2]:=1;
      end;
   end;
   //Brush completed, now actually use it!
   //for every pixel of the brush, check if we need to draw it (Shape),
   //if so, use the correct view to set the voxel (with range checking!)

   for i:=-5 to 5 do
   begin
      for j:=-5 to 5 do
      begin
         if Shape[i,j]=1 then
         begin
            tempview.Data_no := tempview.Data_no +1;
            setlength(tempview.Data,tempview.Data_no +1);

            case BrushView of
               oriX:
               Begin
                  tempview.Data[tempview.Data_no].VC.X := Xc;
                  tempview.Data[tempview.Data_no].VC.Y := Max(Min(Yc+i,VXL.Tailer.YSize-1),0);
                  tempview.Data[tempview.Data_no].VC.Z := Max(Min(Zc+j,VXL.Tailer.ZSize-1),0);

                  tempview.Data[tempview.Data_no].X := tempview.Data[tempview.Data_no].VC.Y;
                  tempview.Data[tempview.Data_no].Y := tempview.Data[tempview.Data_no].VC.Z;
               end;
               oriY:
               Begin
                  tempview.Data[tempview.Data_no].VC.X := Max(Min(Xc+i,VXL.Tailer.XSize-1),0);
                  tempview.Data[tempview.Data_no].VC.Y := Yc;
                  tempview.Data[tempview.Data_no].VC.Z := Max(Min(Zc+j,VXL.Tailer.ZSize-1),0);

                  tempview.Data[tempview.Data_no].X := Max(Min(Xc+i,VXL.Tailer.XSize-1),0);
                  tempview.Data[tempview.Data_no].Y := Max(Min(Zc+j,VXL.Tailer.ZSize-1),0);
               end;
               oriZ:
               Begin
                  tempview.Data[tempview.Data_no].VC.X := Max(Min(Xc+i,VXL.Tailer.XSize-1),0);
                  tempview.Data[tempview.Data_no].VC.Y := Max(Min(Yc+j,VXL.Tailer.YSize-1),0);
                  tempview.Data[tempview.Data_no].VC.Z := Zc;

                  tempview.Data[tempview.Data_no].X := Max(Min(Xc+i,VXL.Tailer.XSize-1),0);
                  tempview.Data[tempview.Data_no].Y := Max(Min(Yc+j,VXL.Tailer.YSize-1),0);
               end;
            end;

            with tempview.Data[tempview.Data_no].VC do
               ActiveSection.GetVoxel(X,Y,Z,VV);

            if VV.Used then
               tempview.Data[tempview.Data_no].VU := true;

            tempview.Data[tempview.Data_no].V := V;

            with tempview.Data[tempview.Data_no].VC do
               tempview.Data[tempview.Data_no].V.Normal := GetSmoothNormal(Vxl,X,Y,Z,V.Normal);
         end;
      end;
   end;

   if tempview.Data_no > 0 then
      for x := 1 to tempview.Data_no do
         for y := x to tempview.Data_no do
            if y <> x then
               if tempview.Data[x].VU then
                  if (tempview.Data[x].X = tempview.Data[y].X) and (tempview.Data[x].Y = tempview.Data[y].Y) then
                     tempview.Data[x].VU := false;
end;

Procedure SetNormals(Normal : Integer);
var
x : integer;
begin
   {$ifdef DEBUG_FILE}
   FrmMain.DebugFile.Add('VoxelEngine: SetNormals');
   {$endif}
   for x := 0 to VoxelFile.Header.NumSections -1 do
      VoxelFile.Section[x].Tailer.Unknown := Normal;
end;

begin
   // Load default palette
   LoadPaletteFromFile(ExtractFileDir(ParamStr(0)) + '\palettes\TS\unittem.pal');

   BGViewColor := RGB(140,170,235);
   VoxelFile := TVoxel.Create;
   VoxelFile.InsertSection(0,'Dummy',1,1,1);
   ActiveSection := VoxelFile.Section[0];

   SpectrumMode := ModeColours;
   ViewMode := ModeEmphasiseDepth;

   if not LoadMouseCursors then
   begin
      messagebox(0,'Files Missing Load Aborted','Files Missing',0);
      Application.Terminate;
      exit;
   end;

   if TestBuild then
      messagebox(0,'Voxel Section Editor III' + #13#13 + 'Version: TB '+testbuildversion+ #13#13#13 + 'THIS IS NOT TO BE DISTRIBUTED','Test Build Message',0);
end.
