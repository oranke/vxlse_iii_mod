unit Voxel;

interface

uses
   {LoadForm,} Forms, Classes, OpenGL;

{$INCLUDE Global_Conditionals.inc}

const
   VTRANSPARENT = 256;
   MAXNORM_TIBERIAN_SUN = 36;
   MAXNORM_RED_ALERT2 = 244;

type
   EError = (OK, ReadFailed, WriteFailed, InvalidSpanDataSizeCalced, InvalidSpan,
     BadSpan_SecondVoxelCount, Unhandled_Exception);

   EDrawMode = (ModeDraw, ModeFloodFill, ModeRectFill, ModeMagnify, ModeLine, ModeColSelect, ModeBrush,  ModeRect, ModeSelect, ModePippet,
     ModeErase, ModeBumpColour, ModeBumpDownColour,ModeFloodFillErase);
   EClickMode = (ModeSingleClick, ModeDoubleClick);
   ESpectrumMode = (ModeColours, ModeNormals);
   EViewMode = (ModeFull, ModeEmphasiseDepth, ModeCrossSection);

   EVoxelViewOrient = (oriX, oriY, oriZ);
   EVoxelViewDir = (dirTowards, dirAway);
   TVoxelType = (vtLand, vtAir);
   TGLMatrixf4 = array[0..3, 0..3] of Single;

   TVoxelHeader = packed record
      FileType: packed array[1..16] of Char; // always "Voxel Animation"
      Unknown, // always 1
      NumSections,
      NumSections2,
      BodySize: LongInt; // as above
      StartPaletteRemap,
      EndPaletteRemap: Byte; // colour indexes
      PaletteData: packed array[1..256*3] of Byte; // never used
   end;

   TVoxelSectionHeader = packed record
      Name: array[1..16] of Char; // ASCIIZ
      Number, // ordinal
      Unknown1, // always 1
      Unknown2: LongInt; // always 2
   end;

   TVoxelSectionTailer = packed record
      SpanStartOfs,
      SpanEndOfs,
      SpanDataOfs: LongInt;
      Det: Single;
      Transform: packed array[1..3,1..4] of Single;
      MinBounds, MaxBounds: packed array[1..3] of Single;
      XSize,
      YSize,
      ZSize,
      Unknown: Byte; // always 2 (or 4?); possibly normals-encoding scheme selection
   end;

   TVoxelUnpacked = record
      Colour,
      Normal,
      Flags: Byte;
      Used: Boolean;
   end;

   TVoxelPacked = LongInt;

   TThumbnail = record
      Width, Height: Integer;
   end;

   TViewport = record
      Left, Top, Zoom: Integer;
      hasBeenUsed: Boolean; // this flag lets the ui know if this
                   // view has been used already (so zoom is a user setting)
   end;

   TVoxelView = class; // forward dec

   TVoxelSection = class
   private
      spectrum: ESpectrumMode;
      // for storing / accessing the stored data
      procedure InitViews;
      procedure SetDataSize(XSize,YSize,ZSize: Integer);
      procedure DefaultTransforms;
      function PackVoxel(Unpacked: TVoxelUnpacked): TVoxelPacked;
      procedure UnpackVoxel(PackedVoxel: TVoxelPacked; var dest: TVoxelUnpacked);
   public
      Data: array of array of array of TVoxelPacked; // as is 32-bit type, should be packed anyway
      MaxNormal, // the highest normal value
      X, Y, Z: Integer; // cursor location
      // other
      Header: TVoxelSectionHeader;
      Tailer: TVoxelSectionTailer;
      View: array[0..2] of TVoxelView;
      Viewport: array[0..2] of TViewport; // so that each window can have a unique zoom etc
      Thumb: array[0..2] of TThumbnail;
      ThumbVisible: array[0..2] of Boolean; // does not rotate with viewport changes
      constructor Create(); overload;
      constructor Create(Name: string; Number, XSize,YSize,ZSize: Integer); overload;
      destructor Destroy; override;
      procedure Resize(XSize,YSize,ZSize: Integer);
	//Plasmadroid v1.4+ drawing tools
      //View-fix, combined with RectangleFill (for shorter code, less bugs :)
      procedure Rectangle(Xpos,Ypos,Zpos,Xpos2,Ypos2,Zpos2:Integer; v: TVoxelUnpacked; Fill: Boolean);
      //Replaced: it now uses the Rectangle code
//        procedure RectangleFill(Xpos,Ypos,Zpos,Xpos2,Ypos2,Zpos2:Integer; v: TVoxelUnpacked);
      //Fixed BrushTool
      procedure BrushTool(Xc,Yc,Zc: Integer; V: TVoxelUnpacked; BrushMode: Integer; BrushView: EVoxelViewOrient);
      //TODO: FIX
      procedure FloodFillTool(Xpos,Ypos,Zpos: Integer; v: TVoxelUnpacked; EditView: EVoxelViewOrient);
      //blow-up tool for content resizing
      procedure ResizeBlowUp(Scale: Integer);
      procedure setSpectrum(newspectrum: ESpectrumMode);
      // viewport cursor
      procedure SetX(newX: Integer);
      procedure SetY(newY: Integer);
      procedure SetZ(newZ: Integer);
      procedure Clear; // blanks the entire voxel model
      procedure SetVoxel(x,y,z: Integer; src: TVoxelUnpacked);
      procedure GetVoxel(x,y,z: Integer; var dest: TVoxelUnpacked);
      function GetVoxelSafe(x,y,z: Integer; var dest: TVoxelUnpacked): Boolean;
      // loading and saving
      function LoadFromFile(var F: File; HeadOfs, BodyOfs, TailOfs : Integer): EError;
//    function SaveToFileHeader(var F: File): EError;
      function SaveToFileBody(var F: File): EError;
 //   function SaveToFileTailer(var F: File): EError;
      // utility methods
      function Name: string;
      // new by Koen
      procedure SetHeaderName(Name: String);

      //New undo system by Koen - SaveUndoDump - saves data to an undo stream
      procedure SaveUndoDump(fs: TStream);
      //loads data of this voxel section from a stream
      procedure LoadUndoDump(fs: TStream);

      //a directional and a positional vector (x,y,z)=PosVector+t*DirectionVector
      procedure FlipMatrix(VectorDir, VectorPos: Array of Single; Multiply: Boolean=True);
      procedure Mirror(MirrorView: EVoxelViewOrient);
      procedure Assign(const _VoxelSection : TVoxelSection);
      function GetTransformAsOpenGLMatrix : TGlmatrixf4;
   end;
   PVoxelSection = ^TVoxelSection;

   TVoxel = class
   public
      Loaded: Boolean;
      ErrorCode: EError; // if an error, says what it is
      ErrorMsg, // if an error, says what it is
      Filename: string;
      Header: TVoxelHeader;
      Section: array of TVoxelSection;
      constructor Create;
      destructor Destroy; override;
      procedure LoadFromFile(Fname: string); // examine ErrorCode for success
      procedure SaveToFile(Fname: string); // examine ErrorCode for success
      function isOpen: Boolean;
      procedure setSpectrum(newspectrum: ESpectrumMode);
      procedure InsertSection(SectionIndex: Integer; Name: String; XSize,YSize,ZSize: Integer);
      procedure RemoveSection(SectionIndex: Integer);
   end;

   TVoxelViewCell = packed record
      Colour: Word; // 16-bits; 256 is transparent
      Depth: Byte; // a z-buffer
   end;

   TVoxelView = class
   private
      Orient: EVoxelViewOrient;
      Dir: EVoxelViewDir;
      swapX, swapY, swapZ: boolean; // are these inversed for viewing?
      procedure CreateCanvas;
      procedure Clear;
      procedure CalcSwapXYZ;
   public
      Foreground, // the Depth in Canvas[] that is the active slice
      Width, Height: Integer;
      Canvas: {packed} array of {packed} array of TVoxelViewCell;
      Voxel: TVoxelSection; // owner
      constructor Create(Owner: TVoxelSection; o: EVoxelViewOrient; d: EVoxelViewDir);
      destructor Destroy; override;
      function getViewNameIdx: Integer;
      function getDir: EVoxelViewDir;
      function getOrient: EVoxelViewOrient;
      procedure TranslateClick(i, j: Integer; var X, Y, Z: Integer);
      procedure getPhysicalCursorCoords(var X, Y: integer);
      procedure setDir(newdir: EVoxelViewDir);
      procedure Refresh; // like a paint on the canvas
      procedure Assign(const _VoxelView : TVoxelView);
   end;

   var
      VoxelType: TVoxelType;


   function AsciizToStr(var src: array of Char; maxlen: Byte): string;

implementation

uses
   SysUtils, Math, Dialogs;

function AsciizToStr(var src: array of Char; maxlen: Byte): string;
var
   ch: Char;
   i: Integer;
begin
   SetLength(Result,maxlen);
   for i := 1 to maxlen do
   begin
      ch := src[i-1];
      if Ord(ch) = 0 then
      begin
         SetLength(Result,i-1);
         Break;
      end;
      Result[i] := ch;
   end;
end;

procedure DebugMsg(Msg: string; Typ: TMsgDlgType);
begin
   // in this version, just show a messsage box for the user
   if (MessageDlg('Debug:' + Chr(10) + Msg,Typ,[mbOK,mbCancel],0) > 0) then
      Halt;
end;

(*** TVoxelSection members *********************************************)

// Min/MaxBounds Fixed! - Stucuk
procedure TVoxelSection.DefaultTransforms;
var
   i, j: integer;
   x,y,z : single;
begin
   Tailer.Det := 0.83333;

   for i := 1 to 3 do
      for j := 1 to 3 do // from memory, don't think this transform is ever used.
         if i = j then
            Tailer.Transform[i,j] := 1
         else
            Tailer.Transform[i,j] := 0;

   X := Tailer.MaxBounds[1] - (Tailer.MaxBounds[1]-Tailer.MinBounds[1]/2);
   Y := Tailer.MaxBounds[2] - (Tailer.MaxBounds[2]-Tailer.MinBounds[2]/2);
   Z := Tailer.MaxBounds[3] - (Tailer.MaxBounds[3]-Tailer.MinBounds[3]/2);

   Tailer.MaxBounds[1] := (Tailer.XSize / 2);
   Tailer.MaxBounds[2] := (Tailer.YSize / 2);
   Tailer.MinBounds[1] := 0 - (Tailer.XSize / 2);
   Tailer.MinBounds[2] := 0 - (Tailer.YSize / 2);

   if VoxelType = vtAir then
   begin
      Tailer.MaxBounds[3] := (Tailer.ZSize / 2);
      Tailer.MinBounds[3] := 0 - (Tailer.ZSize / 2);
   end
   else
   begin
      Tailer.MaxBounds[3] := Tailer.ZSize;
      Tailer.MinBounds[3] := 0;
   end;

   Tailer.MaxBounds[1] := Tailer.MaxBounds[1] + X;
   Tailer.MaxBounds[2] := Tailer.MaxBounds[2] + Y;
   Tailer.MaxBounds[3] := Tailer.MaxBounds[3] + Z;

   Tailer.MinBounds[1] := Tailer.MinBounds[1] + X;
   Tailer.MinBounds[2] := Tailer.MinBounds[2] + Y;
   Tailer.MinBounds[3] := Tailer.MinBounds[3] + Z;
end;

procedure TVoxelSection.Resize(XSize,YSize,ZSize: Integer);
begin
   // memory alloc
   SetDataSize(XSize,YSize,ZSize); // will preserve contents where possible I believe
   // set tailer
   Tailer.XSize := XSize;
   Tailer.YSize := YSize;
   Tailer.ZSize := ZSize;
   DefaultTransforms;
   // and (re)create views
   InitViews;
end;

procedure TVoxelSection.InitViews;
var
   i: integer;
begin
   // deallocate memory for old views
   for i := 0 to 2 do
      //Viewport[i].Free;
      View[i].Free;
   // cursor is by default in the middle of the voxel
   X := (Tailer.XSize div 2);
   Y := (Tailer.YSize div 2);
   Z := (Tailer.ZSize div 2);
   // set views
   View[0] := TVoxelView.Create(Self,oriX,dirTowards);
   View[1] := TVoxelView.Create(Self,oriY,dirTowards);
   View[2] := TVoxelView.Create(Self,oriZ,dirTowards);
   for i := 0 to 2 do
   begin
      with ViewPort[i] do
      begin
         Top := 0;
         Left := 0;
         Zoom := 15;
         hasBeenUsed := False; // so will zoom-to-fit first use
      end;
      with Thumb[i] do
      begin
         Width := Min(Max(View[i].Width,50),150);
         Height := Min(Max(View[i].Height,50),150);
         if Width > Height then // aspect ratio
            Height := Ceil(Width * (View[i].Height / View[i].Width))
         else
            Width := Ceil(Height * (View[i].Width / View[i].Height));
         ThumbVisible[i] := False;
      end;
   end;
end;

function TVoxelSection.GetTransformAsOpenGLMatrix : TGlmatrixf4;
begin
   Result[0,0] := Tailer.Transform[1,1];
   Result[0,1] := Tailer.Transform[2,1];
   Result[0,2] := Tailer.Transform[3,1];
   Result[0,3] := 0;

   Result[1,0] := Tailer.Transform[1,2];
   Result[1,1] := Tailer.Transform[2,2];
   Result[1,2] := Tailer.Transform[3,2];
   Result[1,3] := 0;

   Result[2,0] := Tailer.Transform[1,3];
   Result[2,1] := Tailer.Transform[2,3];
   Result[2,2] := Tailer.Transform[3,3];
   Result[2,3] := 0;

   Result[3,0] := Tailer.Transform[1,4];
   Result[3,1] := Tailer.Transform[2,4];
   Result[3,2] := Tailer.Transform[3,4];
   Result[3,3] := 1;
end;

procedure TVoxelSection.SetHeaderName(Name: String);
const
   MAX_LEN = 15;
var
   i: integer;
begin
   for i:=1 to 16 do
      Header.Name[i]:=#0;
   for i := 1 to Length(Name) do
   begin
      if i > MAX_LEN then break;
      Header.Name[i] := Name[i];
   end;
end;

constructor TVoxelSection.Create(Name: string; Number, XSize,YSize,ZSize: Integer);
begin
   // allocate the memory
   SetDataSize(XSize,YSize,ZSize);
   // create header
   SetHeaderName(Name);
   Header.Number := Number;
   Header.Unknown1 := 1; // TODO: review if this is correct in all cases etc
   Header.Unknown2 := 2; // TODO: review if this is correct in all cases etc
   // create tailer
   Tailer.XSize := XSize;
   Tailer.YSize := YSize;
   Tailer.ZSize := ZSize;
   Tailer.Unknown := 2; // or 4 in RA2?  TODO: review if this is correct in all cases etc

   Tailer.Det:=1/12; //oops, forgot to set Det part correctly

   Tailer.MaxBounds[1] := 0;
   Tailer.MaxBounds[2] := 0;
   Tailer.MaxBounds[3] := 0;

   Tailer.MinBounds[1] := 0;
   Tailer.MinBounds[2] := 0;
   Tailer.MinBounds[3] := 0;

   DefaultTransforms;
   // clear it
   Clear;
end;

procedure TVoxelSection.Assign(const _VoxelSection : TVoxelSection);
var
   i,x,y,z : integer;
begin
   // Copy Header.
   for i := Low(Header.Name) to High(Header.Name) do
      Header.Name[i] := _VoxelSection.Header.Name[i];
   Header.Number := _VoxelSection.Header.Number;
   Header.Unknown1 := _VoxelSection.Header.Unknown1;
   Header.Unknown2 := _VoxelSection.Header.Unknown2;
   // Copy Tailer.
   Tailer.SpanStartOfs := _VoxelSection.Tailer.SpanStartOfs;
   Tailer.SpanEndOfs := _VoxelSection.Tailer.SpanEndOfs;
   Tailer.SpanDataOfs := _VoxelSection.Tailer.SpanDataOfs;
   Tailer.Det := _VoxelSection.Tailer.Det;
   for i := 1 to 3 do
   begin
      Tailer.Transform[i,1] := _VoxelSection.Tailer.Transform[i,1];
      Tailer.Transform[i,2] := _VoxelSection.Tailer.Transform[i,2];
      Tailer.Transform[i,3] := _VoxelSection.Tailer.Transform[i,3];
      Tailer.Transform[i,4] := _VoxelSection.Tailer.Transform[i,4];
   end;
   for i := 1 to 3 do
   begin
      Tailer.MinBounds[i] := _VoxelSection.Tailer.MinBounds[i];
      Tailer.MaxBounds[i] := _VoxelSection.Tailer.MaxBounds[i];
   end;
   Tailer.XSize := _VoxelSection.Tailer.XSize;
   Tailer.YSize := _VoxelSection.Tailer.YSize;
   Tailer.ZSize := _VoxelSection.Tailer.ZSize;
   Tailer.Unknown := _VoxelSection.Tailer.Unknown;
   SetDataSize(Tailer.XSize,Tailer.YSize,Tailer.ZSize);
   // Copy Data
   for x := Low(Data) to High(Data) do
      for y := Low(Data[x]) to High(Data[x]) do
         for z := Low(Data[x,y]) to High(Data[x,y]) do
         begin
            Data[x,y,z] := _VoxelSection.Data[x,y,z];
         end;
   // Copy anything else.
   spectrum := _VoxelSection.spectrum;
   MaxNormal := _VoxelSection.MaxNormal;
   X := _VoxelSection.X;
   Y := _VoxelSection.Y;
   Z := _VoxelSection.Z;
   for i := 0 to 2 do
   begin
      View[i].Assign(_VoxelSection.View[i]);
      View[i].Voxel := self;
      Viewport[i].Left := _VoxelSection.Viewport[i].Left;
      Viewport[i].Top := _VoxelSection.Viewport[i].Top;
      Viewport[i].Zoom := _VoxelSection.Viewport[i].Zoom;
      Viewport[i].hasBeenUsed := _VoxelSection.Viewport[i].hasBeenUsed;
      Thumb[i].Width := _VoxelSection.Thumb[i].Width;
      Thumb[i].Height := _VoxelSection.Thumb[i].Height;
      ThumbVisible[i] := _VoxelSection.ThumbVisible[i];
   end;
end;

constructor TVoxelSection.Create; begin end;

destructor TVoxelSection.Destroy;
begin
   SetDataSize(0,0,0);
   inherited Destroy;
end;


function TVoxelSection.Name: string;
begin
   Result := AsciizToStr(Header.Name,16);
end;

procedure TVoxelSection.SetX(newX: Integer);
begin
   X := Max(Min(newX,Tailer.XSize-1),0);
end;

procedure TVoxelSection.SetY(newY: Integer);
begin
   Y := Max(Min(newY,Tailer.YSize-1),0);
end;

procedure TVoxelSection.SetZ(newZ: Integer);
begin
   Z := Max(Min(newZ,Tailer.ZSize-1),0);
end;

function TVoxelSection.PackVoxel(Unpacked: TVoxelUnpacked): TVoxelPacked;
begin
   Result := Unpacked.Flags shl 16;
   if Unpacked.Used then
      Result := Result or $100 or Unpacked.Colour
   else
      Result := Result or Unpacked.Colour;
   Result := Result shl 8;
   Result := Result or Unpacked.Normal;
end;

procedure TVoxelSection.UnpackVoxel(PackedVoxel: TVoxelPacked; var dest: TVoxelUnpacked);
begin
   dest.Normal := (PackedVoxel and $000000FF);
   dest.Colour := (PackedVoxel and $0000FF00) shr 8;
   dest.Used :=   (PackedVoxel and $00010000) > 0;
   dest.Flags :=  (PackedVoxel and $FF000000) shr 24;
end;

procedure TVoxelSection.Clear; // blanks the entire voxel model
var
   x, y, z: Integer;
   Empty: TVoxelUnpacked;
   PackedVoxel: TVoxelPacked;
begin
// prepare empty voxel
   with Empty do
   begin
      Colour := 0;
      Normal := 0;
      Used := False;
   end;
   PackedVoxel := PackVoxel(Empty);
// and set each and every voxel
   for x := 0 to (Tailer.XSize - 1) do
      for y := 0 to (Tailer.YSize - 1) do
         for z := 0 to (Tailer.ZSize - 1) do
            Data[x,y,z] := PackedVoxel;
end;

procedure TVoxelSection.SetDataSize(XSize,YSize,ZSize: Integer);
var
   x, y: Integer;
begin
   // 1.71: Let's clear the memory from the stuff after XSize.
   if (High(Data) >= XSize) then
   begin
      for x := XSize to High(Data) do
      begin
         for y := 0 to High(Data[x]) do
         begin
            SetLength(Data[x,y],0);
         end;
      end;
   end;
   // Old code continues here.
   SetLength(Data,XSize);
   for x := 0 to (XSize - 1) do
   begin
      // 1.71: Let's clear the memory stuff after YSize
      if (High(Data[x]) >= YSize) then
      begin
         for y := YSize to High(Data[x]) do
         begin
            SetLength(Data[x,y],0);
         end;
      end;
      // Old code continues here.
      SetLength(Data[x],YSize);
      for y := 0 to (YSize - 1) do
         SetLength(Data[x,y],ZSize);
   end;
end;

//This catches the crashes!
{$RANGECHECKS ON}
procedure TVoxelSection.SetVoxel(x,y,z: Integer; src: TVoxelUnpacked);
begin
   Data[x,y,z] := PackVoxel(src);
end;

procedure TVoxelSection.GetVoxel(x,y,z: Integer; var dest: TVoxelUnpacked);
begin
   UnpackVoxel(Data[x,y,z],dest);
end;

function TVoxelSection.GetVoxelSafe(x,y,z: Integer; var dest: TVoxelUnpacked): Boolean;
begin
   Result := False;
   if (x >= 0) and (x < Tailer.XSize) and (y >= 0) and (y < Tailer.YSize) and (z >= 0) and (z < Tailer.ZSize) then
   begin
      UnpackVoxel(Data[x,y,z],dest);
      Result := true;
   end;
end;

{$OPTIMIZATION OFF}
{$RANGECHECKS ON}

function TVoxelSection.LoadFromFile(var F: File; HeadOfs, BodyOfs, TailOfs : Integer): EError;
var
   BytesToRead,
   BytesRead,
   i, SpanCount, Ofs,
   x, y, z,
   voxel_num, num_voxels,
   SpanDataLen: Integer;
   UnpackedVoxel: TVoxelUnpacked;
   SpanStart, SpanEnd: packed array of LongInt;
   SpanData: packed array of Byte; // the raw data
   procedure CleanUp(Err: Boolean);
   begin
      SetLength(SpanData,0);
      SetLength(SpanEnd,0);
      SetLength(SpanStart,0);
      if Err then
         SetDataSize(0,0,0);
   end;
begin
   MaxNormal := 0;
   try
      Result := OK; // assume ok
// read in tailer first
      Seek(F,TailOfs); // move to tail offset
      BytesToRead := SizeOf(TVoxelSectionTailer);
      BlockRead(F,Tailer,BytesToRead,BytesRead);
      if (BytesToRead <> BytesRead) then
      begin
         Result := ReadFailed;
         CleanUp(true);
         Exit;
      end;
      {with Tailer do
          DebugMsg('Size = [' + IntToStr(XSize) + ',' + IntToStr(YSize) + ',' + IntToStr(ZSize) + ']',mtInformation);}
// read in head next
      Seek(F,HeadOfs);
      BytesToRead := SizeOf(TVoxelSectionHeader);
      BlockRead(F,Header,BytesToRead,BytesRead);
      if (BytesToRead <> BytesRead) then
      begin
         Result := ReadFailed;
         CleanUp(True);
         Exit;
      end;
{$IFDEF DEBUG_NORMALS}
      DebugMsg('Header.Section name is : ' + Chr(10) + '[' + AsciizToStr(Header.Name,16) + ']' + Chr(10) +
        'Header.Number is : ' + IntToStr(Header.Number) + Chr(10) +
        'Header.Unknown1 is : ' + IntToStr(Header.Unknown1) + Chr(10) +
        'Header.Unknown2 is : ' + IntToStr(Header.Unknown2) + Chr(10) +
        'Tailer.Unknown is : ' + IntToStr(Tailer.Unknown)
        ,mtInformation);
{$ENDIF}
// create memory for storing the actual voxels; note the creation order
      with Tailer do
      begin
         SetDataSize(XSize,YSize,ZSize);
         Clear; // empty data
         SpanCount := XSize * YSize;
      end;
      SetLength(SpanStart,SpanCount);
      SetLength(SpanEnd,SpanCount);
      // read in the span start offsets
      Seek(F,BodyOfs + Tailer.SpanStartOfs);
      BytesToRead := SpanCount * SizeOf(LongInt);
      BlockRead(F,SpanStart[0],BytesToRead,BytesRead);
      if (BytesToRead <> BytesRead) then
      begin
         Result := ReadFailed;
         CleanUp(True);
         Exit;
      end;
      Seek(F,BodyOfs + Tailer.SpanEndOfs);
      BlockRead(F,SpanEnd[0],BytesToRead,BytesRead);
      if (BytesToRead <> BytesRead) then
      begin
         Result := ReadFailed;
         CleanUp(True);
         Exit;
      end;
      // and load the spans data into a buffer to be parsed
      // find last span end
      i := SpanCount - 1;
      while (SpanEnd[i] = -1) and (i > 0) do
         Dec(i);
      SpanDataLen := SpanEnd[i];
      // find first span end
      i := 0;
      while (SpanStart[i] = -1) and ((i+1) < SpanCount) do
         Inc(i);
      Dec(SpanDataLen,SpanStart[i]);
      Inc(SpanDataLen); // safety
      if SpanDataLen < 1 then
      begin
         Result := InvalidSpanDataSizeCalced;
         CleanUp(True);
         Exit;
      end;
      SetLength(SpanData,SpanDataLen);
      BlockRead(F,SpanData[0],SpanDataLen,BytesRead);
      if (SpanDataLen <> BytesRead) then
      begin
         Result := ReadFailed;
         CleanUp(True);
         Exit;
      end;
      // Banshee's adition here:
      // -- Speed up operations and avoid future mistakes.
      if Tailer.Unknown = 2 then
         MaxNormal := 35
      else if Tailer.Unknown = 4 then
         MaxNormal := 243;
      if Tailer.Det = 0 then
         Tailer.Det := 1;
      // -- End of Banshee's speed up operation.
      // and parse the spans
      i := -1; // will be inc'ed before anything else
      UnpackedVoxel.Used := True;
      for y := 0 to (Tailer.YSize - 1) do
      begin
         for x := 0 to (Tailer.XSize - 1) do
         begin
            Inc(i);
            if (SpanStart[i] = -1) or (SpanEnd[i] = -1) then // skip empty span?
               Continue;
            z := 0;
            Ofs := SpanStart[i];
            while (z < Tailer.ZSize) and (z < SpanEnd[i]) do
            begin
               // this is one z - span
               Inc(z,SpanData[Ofs]); Inc(Ofs); // skip count
               num_voxels := SpanData[Ofs]; Inc(Ofs);
               if (z >= Tailer.ZSize) then
                  Break;
               if (z + num_voxels > Tailer.ZSize) then
                  DebugMsg('Err',mtError);
               for voxel_num := 1 to num_voxels do
               begin
                  with UnpackedVoxel do
                  begin
                     Colour := SpanData[Ofs]; Inc(Ofs);
                     Normal := SpanData[Ofs]; Inc(Ofs);
                     // Banshee: I've changed this, so it avoids
                     // invalid normal values to be placed when
                     // the file loads
                     if (Normal > MaxNormal) then
                        Normal := MaxNormal;
                  end;
                  SetVoxel(x,y,z,UnpackedVoxel);
                  Inc(z);
               end;
               // check second span number
               voxel_num := SpanData[Ofs]; Inc(Ofs);
               if voxel_num <> num_voxels then
               begin
                  Result := BadSpan_SecondVoxelCount;
                  CleanUp(True);
                  Exit;
               end;
            end;
         end;
      end;
{$IFDEF DEBUG_NORMALS}
      DebugMsg('Max normals = ' + IntToStr(MaxNormal),mtInformation);
{$ENDIF}
   // done
   except
      Result := Unhandled_Exception;
      CleanUp(True);
   end;
   InitViews;
   // done
   CleanUp(False);
end;

function TVoxelSection.SaveToFileBody(var F: File): EError;
const
   MaskUsed: TVoxelPacked = $00010000;

   function SpanUsed(x, y: integer): boolean;
   var
      z: integer;
   begin
      Result := True; // assume it is
      for z := 0 to (Tailer.ZSize - 1) do
         if (Data[x,y,z] and MaskUsed) > 0 then // used flag set?
            Exit;
      Result := False; // if here, span wasn't used after all
   end;

   function SpanLength(x,y,z: integer): integer;
   var
      v: TVoxelPacked;
   begin
      Result := 0;
      while z < Tailer.ZSize do
      begin
         v := Data[x,y,z];
         if (v and MaskUsed) > 0 then
            Inc(Result)
         else
            Exit;
         Inc(z);
      end;
   end;
var
   FSpanStart, FSpanEnd, FBody,
   SpanStart, SpanEnd: LongInt;
   x, y, z, SpanSize, s: integer;
   v: TVoxelPacked;
   skip, spanlen, colour, normal: byte;
begin
   // work out offsets
   FBody := FilePos(F);
   SpanSize := Tailer.XSize * Tailer.YSize * SizeOf(LongInt);
   FSpanEnd := FBody - SpanSize;
   FSpanStart := FSpanEnd - SpanSize;
   // and save some info now to the tailer
   Tailer.SpanStartOfs := FSpanStart;
   Tailer.SpanEndOfs := FSpanEnd;
   Tailer.SpanDataOfs := FBody;
   for y := 0 to (Tailer.YSize - 1) do
      for x := 0 to (Tailer.XSize - 1) do
      begin
         if not SpanUsed(x,y) then
         begin
            SpanStart := -1;
            SpanEnd := -1;
         end
         else
         begin
            SpanStart := FilePos(F) - Tailer.SpanDataOfs;
            skip := 0;
            z := 0;
            while z < (Tailer.ZSize) do
            begin
               spanlen := SpanLength(x,y,z);
               if (spanlen > 0) then
               begin
                  BlockWrite(F,skip,1); // write skip
                  BlockWrite(F,spanlen,1); // write span length again
                  for s := 1 to spanlen do
                  begin
                     v := Data[x,y,z + s - 1]; // get voxel
                     colour := (v and $0000FF00) shr 8;
                     normal := (v and $000000FF);
                     BlockWrite(F,colour,1);
                     BlockWrite(F,normal,1);
                  end;
                  BlockWrite(F,spanlen,1); // write span length again
                  Inc(z,spanlen);
                  skip := 0;
               end
               else
               begin
                  Inc(skip);
                  Inc(z);
               end;
            end;
            if skip > 0 then
            begin // write a dummy?
               BlockWrite(F,skip,1);
               spanlen := 0;
               BlockWrite(F,spanlen,1);
               BlockWrite(F,spanlen,1);
            end;
            SpanEnd := FilePos(F) - Tailer.SpanDataOfs - 1;
         end;
         // write span data
         FBody := FilePos(F);
         Seek(F,FSpanStart);
         BlockWrite(F,SpanStart,SizeOf(SpanStart));
         Inc(FSpanStart,SizeOf(SpanStart));
         Seek(F,FSpanEnd);
         BlockWrite(F,SpanEnd,SizeOf(SpanEnd));
         Inc(FSpanEnd,SizeOf(SpanEnd));
         Seek(F,FBody); //return to span data
      end;
   Result := OK;
end;

{$OPTIMIZATION ON}
{$RANGECHECKS OFF}

constructor TVoxel.Create;
begin
   ErrorCode := OK;
   Filename := '';
   Loaded := False;
end;

destructor TVoxel.Destroy;
var
   x : integer;
begin
   for x := High(Section) downto Low(Section) do
   begin
      RemoveSection(x);
   end;
   SetLength(Section,0);
   inherited Destroy;
end;

procedure TVoxel.LoadFromFile(FName: string);
var
   F: File;
   BytesToRead,
   BytesRead,
   i,
   HeadOfs, TailOfs, BodyOfs: Integer;
begin
     {DebugMsg(      'SizeOf(Header) = ' + IntToStr(SizeOf(Header)) + Chr(10) +
        'SizeOf(TVoxelSectionHeader) = ' + IntToStr(SizeOf(TVoxelSectionHeader)) + Chr(10) +
        'SizeOf(TVoxelSectionTailer) = ' + IntToStr(SizeOf(TVoxelSectionTailer)),mtInformation);}
   ErrorCode := OK;
   Loaded := False;
   try  // if file doesn't exist...
      Filename := FName;
      // open voxel file to read
      AssignFile(F,Filename);
      FileMode := fmOpenRead; // read only
      Reset(F,1); // file of byte
      // read in main header
      BytesToRead := SizeOf(Header);
      BlockRead(F,Header,BytesToRead,BytesRead);
      if (BytesRead <> BytesToRead) then
      begin
         ErrorCode := ReadFailed;
         Showmessage('Error: Could not read Voxel Header');
         CloseFile(F);
         Exit;
      end;
      //DebugMsg('there are ' + IntToStr(Header.NumSections) + ' sections',mtInformation);
      // read in sections
      // prepare section objects
      SetLength(Section,Header.NumSections);
      for i := 1 to Header.NumSections do
         Section[i-1] := TVoxelSection.Create;
      HeadOfs := SizeOf(Header); // starts immediately after header
      BodyOfs := HeadOfs + (Header.NumSections * SizeOf(TVoxelSectionHeader)); // after all section headers
      TailOfs := BodyOfs + Header.BodySize; // last part of file
      // and load them
      for i := 1 to Header.NumSections do
      begin
         ErrorCode := Section[i-1].LoadFromFile(F,HeadOfs,BodyOfs,TailOfs);
         if (ErrorCode <> OK) then
         begin
            ErrorMsg := 'Could not read Voxel Section Tailer[' + IntToStr(i) + ']';
            DebugMsg(ErrorMsg,mtError);
            Exit;
         end;
         Inc(HeadOfs,SizeOf(TVoxelSectionHeader)); // next header
         // BodyOfs doesn't change, as relative offset is in tailer
         Inc(TailOfs,SizeOf(TVoxelSectionTailer)); // next tailer
      end;
      Loaded := True;
      CloseFile(F);
   except on E : EInOutError do // VK 1.36 U
      MessageDlg('Error: ' + E.Message + Char($0A) + Filename, mtError, [mbOK], 0);
   end;
end;

procedure TVoxel.SaveToFile(Fname: string);
{ First, we create the file.  We write blanks to represent the
  header. We then write the section headers.  We then encode the
  section bodies.  We then return to the section headers and
  write them, then the section tailers.  We then write the voxel
  header.
}
var
   F: File;
   BytesToWrite,
   BytesWritten: integer;

   function WriteBlank(count: integer): EError;
   var
      i: integer;
      ch: byte;
   begin
      ch := 0;
      BytesToWrite := 1;
      for i := 1 to count do
      begin
         BlockWrite(F,ch,BytesToWrite,BytesWritten);
         if (BytesWritten <> BytesToWrite) then
         begin
            Result := WriteFailed;
            Exit;
         end;
      end;
      Result := OK;
   end;
var
   i, BodyStart, BodyEnd: Integer;
begin
   if not Loaded then Exit; // cannot save an empty voxel?
   //-- when we save an empty voxel - this voxel will be unsupported - we must change this [Kamil ^aka Plasmadroid]
   ErrorCode := OK;
   try
      Filename := Fname;
      // create voxel file to write
      AssignFile(F,Filename);
      FileMode := fmOpenWrite; // we save file, so write mode [VK]
      Rewrite(F,1); // file of byte
      // write main header (overwritten later)
      ErrorCode := WriteBlank(SizeOf(TVoxelHeader));
      if (ErrorCode <> OK) then
      begin
         ErrorMsg := 'Could not write Voxel Header (pass 2)';
         DebugMsg(ErrorMsg,mtError);
         Exit;
      end;
      // write section heads (real data)
      BytesToWrite := SizeOf(TVoxelSectionHeader);
      for i := 1 to Header.NumSections do
      begin
         BlockWrite(F,Section[Pred(i)].Header,BytesToWrite,BytesWritten);
         if (BytesWritten <> BytesToWrite) then
         begin
            ErrorCode := WriteFailed;
            ErrorMsg := 'Could not write Voxel Section Header #' + IntToStr(i) + '(pass 2)';
            DebugMsg(ErrorMsg,mtError);
            Exit;
         end;
      end;
      // write section bodies (real data)
      BodyStart := FilePos(F); // for later; could have calc'ed it instead
      for i := 0 to (Header.NumSections - 1) do
      begin
         with Section[i] do
         begin
            // first, write blank data where the span starts / ends will be
            ErrorCode := WriteBlank(Tailer.XSize*Tailer.YSize*4*2);
            if (ErrorCode <> OK) then
            begin
               ErrorMsg := 'Could not write Voxel Section Body #' + IntToStr(Succ(i)) + ' (pass 1)';
               DebugMsg(ErrorMsg,mtError);
               Exit;
            end;
            // now write the actual data
            ErrorCode := SaveToFileBody(F);
            if (ErrorCode <> OK) then
            begin
               ErrorMsg := 'Could not write Voxel Section Body #' + IntToStr(Succ(i)) + ' (pass 2)';
               DebugMsg(ErrorMsg,mtError);
               Exit;
            end;
         end;
      end;
      BodyEnd := FilePos(F); // for later; could have calc'ed it instead
      // write section tailers (real data)
      BytesToWrite := SizeOf(TVoxelSectionTailer);
      for i := 0 to (Header.NumSections - 1) do
      begin
         with Section[i].Tailer do
         begin
            Dec(SpanStartOfs,BodyStart);
            Dec(SpanEndOfs,BodyStart);
            Dec(SpanDataOfs,BodyStart);
         end;
         BlockWrite(F,Section[i].Tailer,BytesToWrite,BytesWritten);
         if (BytesWritten <> BytesToWrite) then
         begin
            ErrorCode := WriteFailed;
            ErrorMsg := 'Could not write Voxel Section Tailer #' + IntToStr(i) + '(pass 2)';
            DebugMsg(ErrorMsg,mtError);
            Exit;
         end;
      end;
      // write main header (real data)
      Seek(F,0); // go to start of file again
      Header.BodySize := BodyEnd - BodyStart;
      BytesToWrite := SizeOf(TVoxelHeader);
      BlockWrite(F,Header,BytesToWrite,BytesWritten);
      if (BytesWritten <> BytesToWrite) then
      begin
         ErrorMsg := 'Could not write Voxel Header (pass 2)';
         DebugMsg(ErrorMsg,mtError);
         Exit;
      end;
      CloseFile(F);
   except on E : EInOutError do // VK 1.36 U
		MessageDlg('Error: ' + E.Message + Char($0A) + Filename, mtError, [mbOK], 0);
   end;
end;

function TVoxel.isOpen: Boolean; begin Result := Loaded; end;

(*** TVoxelView members *********************************************)

procedure TVoxelView.CreateCanvas;
var
   x: Integer;
begin
   with Voxel.Tailer do
   begin
      case Orient of
         oriX:
         begin
            Width := YSize;
            Height := ZSize;
         end;
         oriY:
         begin
            Width := XSize;
            Height := ZSize;
         end;
         oriZ:
         begin
            Width := XSize;
            Height := YSize;
         end;
      end;
   end;
   SetLength(Canvas,Width);
   for x := 0 to (Width - 1) do
      SetLength(Canvas[x],Height);
   //CalcIncs;
end;

procedure TVoxelView.Clear;
var
   x, y: Integer;
begin
   for x := 0 to (Width - 1) do
      for y := 0 to (Height - 1) do
         with Canvas[x,y] do
         begin
            Colour := VTRANSPARENT;
            Depth := 0; // far away
         end;
end;

constructor TVoxelView.Create(Owner: TVoxelSection; o: EVoxelViewOrient; d: EVoxelViewDir);
begin
   Voxel := Owner;
   Orient := o;
   Dir := d;
   CreateCanvas;
   CalcSwapXYZ;
   Refresh;
end;

destructor TVoxelView.Destroy;
var
   x : integer;
begin
   for x := Low(Canvas) to high(Canvas) do
   begin
      SetLength(Canvas[x],0);
   end;
   SetLength(Canvas,0);
   finalize(Canvas);
   inherited Destroy;
end;

procedure TVoxelView.Assign(const _VoxelView : TVoxelView);
var
   i,j : integer;
begin
   Orient := _VoxelView.Orient;
   Dir := _VoxelView.Dir;
   swapX := _VoxelView.swapX;
   swapY := _VoxelView.swapY;
   swapZ := _VoxelView.swapZ;
   Foreground := _VoxelView.Foreground;
   Width := _VoxelView.Width;
   Height := _VoxelView.Height;
   Voxel := _VoxelView.Voxel;
   SetLength(Canvas,Width);
   for i := Low(Canvas) to High(Canvas) do
   begin
      SetLength(Canvas[i],Height);
      for j := Low(Canvas[i]) to High(Canvas[i]) do
      begin
         Canvas[i,j].Colour := _VoxelView.Canvas[i,j].Colour;
         Canvas[i,j].Depth := _VoxelView.Canvas[i,j].Depth;
      end;
   end;
end;


function TVoxelView.getViewNameIdx: Integer;
begin
   Result := 0;
   case Orient of
      oriX: Result := 0;
      oriY: Result := 2;
      oriZ: Result := 4;
   end;
   if Dir = dirAway then
      Inc(Result);
end;

procedure TVoxelView.Refresh;
   procedure DrawX;
   var
      x, y, z, // coords in model
      i, j: Integer; // screen coords
      v: TVoxelUnpacked;
   begin
      Foreground := Voxel.X;
      for y := 0 to (Width - 1) do
         for z := 0 to (Height - 1) do
         begin
            x := Foreground;
            Voxel.GetVoxel(x,y,z,v);
            // find voxel on the x axis
            while not v.Used do
            begin
               // increment on the x axis
               if Dir = dirTowards then
               begin
                  Inc(x);
                  if x >= Voxel.Tailer.XSize then // range check
                     Break; // Ok, no voxels ever to show
               end
               else
               begin // Dir = dirAway
                  Dec(x);
                  if x < 0 then // range check
                     Break; // Ok, no voxels ever to show
               end;
               // get next
               Voxel.GetVoxel(x,y,z,v);
            end;
            // and set the voxel appropriately
            if SwapY then
               i := Width - 1 - y
            else
               i := y;
            if SwapZ then
               j := Height - 1 - z
            else
               j := z;
            with Canvas[i,j] do
            begin
               Depth := x;
               if v.Used then
               begin
                  if voxel.spectrum = ModeNormals then
                     Colour := v.Normal
                  else
                     Colour := v.Colour;
               end
               else
                  Colour := VTRANSPARENT; // 256
            end;
         end;
   end;
   procedure DrawY;
   var
      x, y, z, // coords in model
      i, j: Integer; // screen coords
      v: TVoxelUnpacked;
   begin
      Foreground := Voxel.Y;
      for x := 0 to (Width - 1) do
         for z := 0 to (Height - 1) do
         begin
            y := Foreground;
            Voxel.GetVoxel(x,y,z,v);
            // find voxel on the x axis
            while not v.Used do
            begin
               // increment on the x axis
               if Dir = dirTowards then
               begin
                  Inc(y);
                  if y >= Voxel.Tailer.YSize then // range check
                     Break; // Ok, no voxels ever to show
               end
               else
               begin // Dir = dirAway
                  Dec(y);
                  if y < 0 then // range check
                     Break; // Ok, no voxels ever to show
               end;
               // get next
               Voxel.GetVoxel(x,y,z,v);
             end;
             // and set the voxel appropriately
             if SwapX then
               i := Width - 1 - x
             else
               i := x;
             if SwapZ then
               j := Height - 1 - z
             else
               j := z;
             with Canvas[i,j] do
             begin
                Depth := y;
                if v.Used then
                begin
                   if voxel.spectrum = ModeNormals then
                      Colour := v.Normal
                   else
                      Colour := v.Colour;
                end
                else
                   Colour := VTRANSPARENT; // 256
             end;
         end;
   end;
   procedure DrawZ;
   var
      x, y, z, // coords in model
      i, j: Integer; // screen coords
      v: TVoxelUnpacked;
   begin
      Foreground := Voxel.Z;
      for x := 0 to (Width - 1) do
         for y := 0 to (Height - 1) do
         begin
            z := Foreground;
            Voxel.GetVoxel(x,y,z,v);
            // find voxel on the x axis
            while not v.Used do
            begin
               // increment on the x axis
               if Dir = dirTowards then
               begin
                  Inc(z);
                  if z >= Voxel.Tailer.ZSize then // range check
                     Break; // Ok, no voxels ever to show
               end
               else
               begin // Dir = dirAway
                  Dec(z);
                  if z < 0 then // range check
                     Break; // Ok, no voxels ever to show
               end;
               // get next
               Voxel.GetVoxel(x,y,z,v);
            end;
            // and set the voxel appropriately
            if SwapX then
               i := Width - 1 - x
            else
               i := x;
            if SwapY then
               j := Height - 1 - y
            else
               j := y;
            with Canvas[i,j] do
            begin
               Depth := z;
               if v.Used then
               begin
                  if voxel.spectrum = ModeNormals then
                     Colour := v.Normal
                  else
                     Colour := v.Colour;
               end
               else
                  Colour := VTRANSPARENT; // 256
            end;
         end;
   end;
begin
   case Orient of
      oriX: DrawX;
      oriY: DrawY;
      oriZ: DrawZ;
   end;
end;

procedure TVoxelView.TranslateClick(i, j: Integer; var X, Y, Z: Integer);

   procedure TranslateX;
   begin
      X := Foreground;
      if SwapY then
         Y := Width - 1 - i
      else
         Y := i;
      if SwapZ then
         Z := Height - 1 - j
      else
         Z := j;
   end;
   procedure TranslateY;
   begin
      if SwapX then
         X := Width - 1 - i
      else
         X := i;
      Y := Foreground;
      if SwapZ then
         Z := Height - 1 - j
      else
         Z := j;
   end;

   procedure TranslateZ;
   begin
      if SwapX then
         X := Width - 1 - i
      else
         X := i;
      if SwapY then
         Y := Height - 1 - j
      else
         Y := j;
      Z := Foreground;
  end;

begin
   case Orient of
      oriX: TranslateX;
      oriY: TranslateY;
      oriZ: TranslateZ;
   end;
end;

procedure TVoxelView.getPhysicalCursorCoords(var X, Y: integer);
   procedure TranslateX;
   begin
      if SwapY then
         X := Width - Voxel.Y
      else
         X := Voxel.Y;
      if SwapZ then
         Y := Height - Voxel.Z
      else
         Y := Voxel.Z;
   end;
   procedure TranslateY;
   begin
      if SwapX then
         X := Width - Voxel.X
      else
         X := Voxel.X;
      if SwapZ then
         Y := Height - Voxel.Z
      else
         Y := Voxel.Z;
   end;
   procedure TranslateZ;
   begin
      if SwapX then
         X := Width - Voxel.X
      else
         X := Voxel.X;
      if SwapY then
         Y := Height - Voxel.Y
      else
         Y := Voxel.Y;
   end;
begin
   case Orient of
      oriX: TranslateX;
      oriY: TranslateY;
      oriZ: TranslateZ;
   end;
   Dec(y);
end;

procedure TVoxelView.setDir(newdir: EVoxelViewDir);
begin
   Dir := newdir;
   CalcSwapXYZ;
   Refresh;
end;

function TVoxelView.getDir: EVoxelViewDir;
begin
   Result := Dir;
end;

function TVoxelView.getOrient: EVoxelViewOrient;
begin
   Result := Orient;
end;

procedure TVoxelSection.setSpectrum(newspectrum: ESpectrumMode);
begin
   spectrum := newspectrum;
end;

procedure TVoxel.setSpectrum(newspectrum: ESpectrumMode);
var i: integer;
begin
   for i := Low(Section) to High(Section) do
      Section[i].setSpectrum(newspectrum);
end;

procedure TVoxelView.CalcSwapXYZ;
var idx: integer;
begin
   idx := getViewNameIdx;
   case idx of
      0:
      begin // Back to Front
         SwapX := False;
         SwapY := False;
         SwapZ := True;
      end;
      1:
      begin // Front to Back
         SwapX := False;
         SwapY := True;
         SwapZ := True;
      end;
      2:
      begin // Right to Left
         SwapX := False;
         SwapY := True;
         SwapZ := True;
      end;
      3:
      begin // Left to Right
         SwapX := True;
         SwapY := True;
         SwapZ := True;
      end;
      4:
      begin // Top to Bottom
         SwapX := False;
         SwapY := True;
         SwapZ := False;
      end;
      5:
      begin // Bottom to Top
         SwapX := True;
         SwapY := True;
         SwapZ := False;
      end;
   end;
end;

procedure TVoxelSection.Rectangle(Xpos,Ypos,Zpos,Xpos2,Ypos2,Zpos2:Integer; v: TVoxelUnpacked; Fill: Boolean);
type
   EOrientRect = (oriUnDef, oriX, oriY, oriZ);
var
   i,j,k: Integer;
   O: EOrientRect;
   Inside,Exact: Integer;
begin
   O:=oriUnDef; //the view direction isn't defined yet
   if (Xpos=Xpos2) then O:=oriX;
   if (Ypos=Ypos2) then O:=oriY;
   if (Zpos=Zpos2) then O:=oriZ;
   if (O=oriUnDef) then MessageDlg('Impossible to draw 3D rectangles!!!',mtError,[mbOK],0);
{  //this isn't efficient...
  for i:=0 to Tailer.XSize do begin
    for j:=0 to Tailer.YSize do begin
      for k:=0 to Tailer.ZSize do begin}
  //this is better
   for i:=Min(Xpos,Xpos2) to Max(Xpos,Xpos2) do
   begin
      for j:=Min(Ypos,Ypos2) to Max(Ypos,Ypos2) do
      begin
         for k:=Min(Zpos,Zpos2) to Max(Zpos,Zpos2) do
         begin
            Inside:=0; Exact:=0;
            case O of
               oriX:
               begin
                  if (i=Xpos) then
                  begin //we're in the right slice
                     if (j>Min(Ypos,Ypos2)) and (j<Max(Ypos,Ypos2)) then Inc(Inside);
                     if (k>Min(Zpos,Zpos2)) and (k<Max(Zpos,Zpos2)) then Inc(Inside);
                     if (j=Min(Ypos,Ypos2)) or (j=Max(Ypos,Ypos2)) then Inc(Exact);
                     if (k=Min(Zpos,Zpos2)) or (k=Max(Zpos,Zpos2)) then Inc(Exact);
                  end;
               end;
               oriY:
               begin
                  if (j=Ypos) then
                  begin //we're in the right slice
                     if (i>Min(Xpos,Xpos2)) and (i<Max(Xpos,Xpos2)) then Inc(Inside);
                     if (k>Min(Zpos,Zpos2)) and (k<Max(Zpos,Zpos2)) then Inc(Inside);
                     if (i=Min(Xpos,Xpos2)) or (i=Max(Xpos,Xpos2)) then Inc(Exact);
                     if (k=Min(Zpos,Zpos2)) or (k=Max(Zpos,Zpos2)) then Inc(Exact);
                  end;
               end;
               oriZ:
               begin
                  if (k=Zpos) then
                  begin //we're in the right slice
                     if (i>Min(Xpos,Xpos2)) and (i<Max(Xpos,Xpos2)) then Inc(Inside);
                     if (j>Min(Ypos,Ypos2)) and (j<Max(Ypos,Ypos2)) then Inc(Inside);
                     if (i=Min(Xpos,Xpos2)) or (i=Max(Xpos,Xpos2)) then Inc(Exact);
                     if (j=Min(Ypos,Ypos2)) or (j=Max(Ypos,Ypos2)) then Inc(Exact);
                  end;
               end;
            end;
            if Fill then
            begin
               if Inside+Exact=2 then
               begin
                  SetVoxel(i,j,k,v);
               end;
            end
            else
            begin
               if (Exact>=1) and (Inside+Exact=2) then
               begin
                  SetVoxel(i,j,k,v);
               end;
            end;
         end;
      end;
   end;
end;

procedure TVoxelSection.SaveUndoDump(fs: TStream);
var
   x,y,z: Integer;
begin
   fs.Write(Header,SizeOf(Header));
   fs.Write(Tailer,SizeOf(Tailer));
   for x := 0 to (Tailer.XSize - 1) do
      for y := 0 to (Tailer.YSize - 1) do
         for z := 0 to (Tailer.ZSize - 1) do
            fs.Write(Data[x,y,z],SizeOf(TVoxelPacked));
end;

procedure TVoxelSection.LoadUndoDump(fs: TStream);
var
   x,y,z: Integer;
begin
   fs.Read(Header,SizeOf(Header));
   fs.Read(Tailer,SizeOf(Tailer));
   for x := 0 to (Tailer.XSize - 1) do
      for y := 0 to (Tailer.YSize - 1) do
         for z := 0 to (Tailer.ZSize - 1) do
            fs.Read(Data[x,y,z],SizeOf(TVoxelPacked));
end;

// Insert a new section with SectionIndex :)
procedure TVoxel.InsertSection(SectionIndex: Integer; Name: String; XSize,
  YSize, ZSize: Integer);
var
   i: Integer;
begin
   //SectionIndex contains the index of the *new* section to create...
   SetLength(Section,Header.NumSections+1);
   for i:=Header.NumSections-1 downto SectionIndex do
   begin
      Section[i+1]:=Section[i];
      Section[i+1].Header.Number:=i+1;
   end;
   Section[SectionIndex]:=TVoxelSection.Create(Name,SectionIndex,XSize,YSize,ZSize);
   Section[SectionIndex].InitViews;
   Inc(Header.NumSections);
   Inc(Header.NumSections2);
end;

procedure TVoxel.RemoveSection(SectionIndex: Integer);
var
   i: Integer;
begin
   Section[SectionIndex].Free;
   for i:= SectionIndex to Header.NumSections - 2 do
   begin
      Section[i]:=Section[i+1];
      Section[i].Header.Number:=i;
   end;
   Dec(Header.NumSections);
   Dec(Header.NumSections2);
end;

//this function uses basic matrix/vector operations (nice hybrid Koen) to
//allow flipping and nudging.
procedure TVoxelSection.FlipMatrix(VectorDir,
  VectorPos: array of Single; Multiply: Boolean=True);
var
   NewData: array of array of array of TVoxelPacked; // as is 32-bit type, should be packed anyway
   i,j,k,a,b,c: Integer;
   Empty: TVoxelUnpacked;
   PackedVoxel: TVoxelPacked;
begin
   // prepare empty voxel
   with Empty do
   begin
      Colour := 0;
      Normal := 0;
      Used := False;
   end;
   PackedVoxel := PackVoxel(Empty);

   //create new data matrix
   SetLength(NewData,Tailer.XSize,Tailer.YSize,Tailer.ZSize);
   //fill it with empty voxels
   for i:=0 to Tailer.XSize - 1 do
   begin
      for j:=0 to Tailer.YSize - 1 do
      begin
         for k:=0 to Tailer.ZSize - 1 do
         begin
            NewData[i,j,k]:=PackedVoxel;
         end;
      end;
   end;
   if Multiply then
   begin
      VectorPos[0]:=Max(VectorPos[0]*Tailer.XSize-1,0);
      VectorPos[1]:=Max(VectorPos[1]*Tailer.YSize-1,0);
      VectorPos[2]:=Max(VectorPos[2]*Tailer.ZSize-1,0);
   end;
   for i:=0 to Tailer.XSize - 1 do
   begin
      for j:=0 to Tailer.YSize - 1 do
      begin
         for k:=0 to Tailer.ZSize - 1 do
         begin
            a:=Round(i*VectorDir[0]+VectorPos[0]);
            b:=Round(j*VectorDir[1]+VectorPos[1]);
            c:=Round(k*VectorDir[2]+VectorPos[2]);
            //perform range checking
            if not (a<0) and not (b<0) and not (c<0) then
            begin
               if (a<Tailer.XSize) and (b<Tailer.YSize) and (c<Tailer.ZSize) then
                  NewData[a,b,c]:=Data[i,j,k];
            end;
         end;
      end;
   end;
   //That wasn't so hard...
   //now copy it back
   for i:=0 to Tailer.XSize - 1 do
   begin
      for j:=0 to Tailer.YSize - 1 do
      begin
         for k:=0 to Tailer.ZSize - 1 do
         begin
            Data[i,j,k] := NewData[i,j,k];
         end;
      end;
   end;
   // 1.3: Now, let's clear the memory
   for i := Low(NewData) to High(NewData) do
   begin
      for j := Low(NewData[i]) to High(NewData[i]) do
      begin
         SetLength(NewData[i,j],0);
      end;
      SetLength(NewData[i],0);
   end;
   SetLength(NewData,0);
end;

procedure TVoxelSection.Mirror(MirrorView: EVoxelViewOrient);
var
   NewData: array of array of array of TVoxelPacked; // as is 32-bit type, should be packed anyway
   i,j,k: Integer;
   Xm,Ym,Zm: Integer;
   OddMod: Integer;
begin
   OddMod:=0;
   SetLength(NewData,Tailer.XSize,Tailer.YSize,Tailer.ZSize);
   //First, copy the entire voxel into the new data array
   for i:=0 to Tailer.XSize - 1 do
   begin
      for j:=0 to Tailer.YSize - 1 do
      begin
         for k:=0 to Tailer.ZSize - 1 do
         begin
            NewData[i,j,k]:=Data[i,j,k];
         end;
      end;
   end;
   Xm:=Tailer.XSize div 2-1;
   Ym:=Tailer.YSize div 2-1;
   Zm:=Tailer.ZSize div 2-1;
   case MirrorView of
      oriX:
      begin
         if (Tailer.XSize mod 2)=1 then OddMod:=1;
         for i:=0 to Xm do
         begin
            for j:=0 to Tailer.YSize-1 do
            begin
               for k:=0 to Tailer.ZSize-1 do
               begin
                  NewData[-i+Xm,j,k]:=Data[i+Xm+1+OddMod,j,k];
               end;
            end;
         end;
      end;
      oriY:
      begin
         if (Tailer.YSize mod 2)=1 then OddMod:=1;
         for i:=0 to Tailer.XSize-1 do
         begin
            for j:=0 to Ym do
            begin
               for k:=0 to Tailer.ZSize-1 do
               begin
                  NewData[i,-j+Ym,k]:=Data[i,j+Ym+1+OddMod,k];
               end;
            end;
         end;
      end;
      oriZ:
      begin
         if (Tailer.ZSize mod 2)=1 then OddMod:=1;
         for i:=0 to Tailer.XSize-1 do
         begin
            for j:=0 to Tailer.YSize-1 do
            begin
               for k:=0 to Zm do
               begin
                  NewData[i,j,-k+Zm]:=Data[i,j,k+Zm+1+OddMod];
               end;
            end;
         end;
      end;
   end;
   //That wasn't so hard...
   //now copy it back
   for i:=0 to Tailer.XSize - 1 do
   begin
      for j:=0 to Tailer.YSize - 1 do
      begin
         for k:=0 to Tailer.ZSize - 1 do
         begin
            Data[i,j,k]:=NewData[i,j,k];
         end;
      end;
   end;
   // 1.3: Now, let's clear the memory
   for i := Low(NewData) to High(NewData) do
   begin
      for j := Low(NewData[i]) to High(NewData[i]) do
      begin
         SetLength(NewData[i,j],0);
      end;
      SetLength(NewData[i],0);
   end;
   SetLength(NewData,0);
   finalize(NewData);
end;

procedure TVoxelSection.ResizeBlowUp(Scale: Integer);
var
   NewData: array of array of array of TVoxelPacked; // as is 32-bit type, should be packed anyway
   i,j,k: Integer;
   Empty: TVoxelUnpacked;
   PackedVoxel: TVoxelPacked;
begin
// prepare empty voxel
   with Empty do
   begin
      Colour := 0;
      Normal := 0;
      Used := False;
   end;
   PackedVoxel := PackVoxel(Empty);

   //create new data matrix
   SetLength(NewData,Tailer.XSize*Scale,Tailer.YSize*Scale,Tailer.ZSize*Scale);
   //fill it with empty voxels
   for i:=0 to Tailer.XSize * Scale - 1 do
   begin
      for j:=0 to Tailer.YSize * Scale - 1 do
      begin
         for k:=0 to Tailer.ZSize * Scale - 1 do
         begin
            NewData[i,j,k]:=PackedVoxel;
         end;
      end;
   end;
   for i:=0 to Tailer.XSize * Scale - 1 do
   begin
      for j:=0 to Tailer.YSize * Scale - 1 do
      begin
         for k:=0 to Tailer.ZSize * Scale - 1 do
         begin
            NewData[i,j,k]:=Data[i div Scale,j div Scale,k div Scale];
         end;
      end;
   end;
   Resize(Tailer.XSize*Scale,Tailer.YSize*Scale,Tailer.ZSize*Scale);
   for i:=0 to Tailer.XSize - 1 do
   begin
      for j:=0 to Tailer.YSize - 1 do
      begin
         for k:=0 to Tailer.ZSize - 1 do
         begin
            Data[i,j,k]:=NewData[i,j,k];
         end;
      end;
   end;
   // 1.3: Now, let's clear the memory
   for i := Low(NewData) to High(NewData) do
   begin
      for j := Low(NewData[i]) to High(NewData[i]) do
      begin
         SetLength(NewData[i,j],0);
      end;
      SetLength(NewData[i],0);
   end;
   SetLength(NewData,0);
end;

//brushview contains the view of the current editing Window.
procedure TVoxelSection.BrushTool(Xc,Yc,Zc: Integer; V: TVoxelUnpacked; BrushMode: Integer; BrushView: EVoxelViewOrient);
var
   Shape: Array[-5..5,-5..5] of 0..1;
   i,j,r1,r2: Integer;
begin
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
            case BrushView of
               oriX: SetVoxel(Xc,Max(Min(Yc+i,Tailer.YSize-1),0),Max(Min(Zc+j,Tailer.ZSize-1),0),v);
               oriY: SetVoxel(Max(Min(Xc+i,Tailer.XSize-1),0),Yc,Max(Min(Zc+j,Tailer.ZSize-1),0),v);
               oriZ: SetVoxel(Max(Min(Xc+i,Tailer.XSize-1),0),Max(Min(Yc+j,Tailer.YSize-1),0),Zc,v);
            end;
         end;
      end;
   end;
   //all old Brush code was deleted, because this one doesn't have problems with
   //multiple views.
end;

procedure TVoxelSection.FloodFillTool(Xpos,Ypos,Zpos: Integer; v: TVoxelUnpacked; EditView: EVoxelViewOrient);
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
      if (l.X>=Tailer.XSize) or (l.Y>=Tailer.YSize) or (l.Z>=Tailer.ZSize) then Exit;
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
   SetLength(Done,Tailer.XSize,Tailer.YSize,Tailer.ZSize);
   SetLength(Stack,Tailer.XSize*Tailer.YSize*Tailer.ZSize);
   //this array avoids creation of extra stack objects when it isn't needed.
   for i:=0 to Tailer.XSize - 1 do
      for j:=0 to Tailer.YSize - 1 do
         for k:=0 to Tailer.ZSize - 1 do
            Done[i,j,k]:=False;

   GetVoxel(Xpos,Ypos,Zpos,z1);
   SetVoxel(Xpos,Ypos,Zpos,v);

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
            GetVoxel(po.X,po.Y,po.Z,z2);
            if z2.Colour=z1.Colour then
            begin
               SetVoxel(po.X,po.Y,po.Z,v);
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
            GetVoxel(po.X,po.Y,po.Z,z2);
            if z2.Colour=z1.Colour then
            begin
               SetVoxel(po.X,po.Y,po.Z,v);
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
            GetVoxel(po.X,po.Y,po.Z,z2);
            if z2.Colour=z1.Colour then
            begin
               SetVoxel(po.X,po.Y,po.Z,v);
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
            GetVoxel(po.X,po.Y,po.Z,z2);
            if z2.Colour=z1.Colour then
            begin
               SetVoxel(po.X,po.Y,po.Z,v);
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
   // 1.3: Now, let's clear the memory
   for i := Low(Done) to High(Done) do
   begin
      for j := Low(Done[i]) to High(Done[i]) do
      begin
         SetLength(Done[i,j],0);
      end;
      SetLength(Done[i],0);
   end;
   SetLength(Done,0);
end;

end.
