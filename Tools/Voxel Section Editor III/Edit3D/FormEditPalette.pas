{-----------------------------------------------------------------------------
 Unit Name: FormEditPalette
 Author:    oranke
 Date:      2015-02-27
 Purpose:

    Palette info editor. 

    
 History:
-----------------------------------------------------------------------------}


unit FormEditPalette;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Math,

  Voxel_Engine, 


  Palette, StdCtrls, ExtCtrls;

type
  TTFrmEditPallette = class(TForm)
    Panel5: TPanel;
    Bevel1: TBevel;
    cnvPalette: TPaintBox;
    BtOK: TButton;
    BtCancel: TButton;
    Image1: TImage;
    SelectButton: TButton;
    ImportButton: TButton;
    ColorDialog1: TColorDialog;
    OpenDialog1: TOpenDialog;
    procedure cnvPaletteMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure cnvPalettePaint(Sender: TObject);
    procedure BtOKClick(Sender: TObject);
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SelectButtonClick(Sender: TObject);
    procedure ImportButtonClick(Sender: TObject);
  private
    { Private declarations }
    fEditPalette: TPalette;
    fActiveColour: integer;
    procedure PaintPalette();
 public
    { Public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  TFrmEditPallette: TTFrmEditPallette;

implementation

{$R *.dfm}

{ TTFrmEditPallette }

constructor TTFrmEditPallette.Create(aOwner: TComponent);
begin
  inherited;
  fActiveColour := 16;
  fEditPalette := VXLPalette; 
end;

destructor TTFrmEditPallette.Destroy;
begin

  inherited;
end;

procedure TTFrmEditPallette.PaintPalette;
var
   colwidth, rowheight: Real;
   i, j, idx: Integer;
   r: TRect;
   red, green, blue, mixcol: Byte;
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
            Brush.Color := fEditPalette[idx];

            // Check if it's suposed to be marked, it's active colour
            // and... it's not used. Why? -- Banshee
            if (Idx = fActiveColour) then
            begin // the current pen
               // This part makes a square and a 'X' through the colour box.
               SplitColour(fEditPalette[idx],red,green,blue);
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

end;

procedure TTFrmEditPallette.BtOKClick(Sender: TObject);
begin
  VXLPalette := fEditPalette;
//
end;

procedure TTFrmEditPallette.cnvPaletteMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  colwidth, rowheight: Real;
  i, j, idx: Integer;
begin
  colwidth:=cnvPalette.Width / 8;
  rowheight:=cnvPalette.Height / 32;
  i:=Trunc(X / colwidth);
  j:=Trunc(Y / rowheight);
  idx:=(i * 32) + j;

  fActiveColour := idx;
  PaintPalette();

//
end;

procedure TTFrmEditPallette.Image1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  fEditPalette[fActiveColour] := Image1.Canvas.Pixels[x, y];
  PaintPalette();
//
end;


procedure TTFrmEditPallette.cnvPalettePaint(Sender: TObject);
begin
  PaintPalette();
//
end;


procedure TTFrmEditPallette.SelectButtonClick(Sender: TObject);
begin
  ColorDialog1.Color := fEditPalette[fActiveColour];
  if ColorDialog1.Execute then
    fEditPalette[fActiveColour] := ColorDialog1.Color;
  PaintPalette(); 
//
end;

procedure TTFrmEditPallette.ImportButtonClick(Sender: TObject);
var
  Pal: PLogPalette;
  //HPal: hPalette;
  Bmp: TBitmap;
  NewColor: TColor;

  BitmapPalEntries: DWORD;
  i, j: Integer;

  function IsBasicColor(const aColor: TColor): Boolean;
  var
    m: Integer;
  begin
    Result := false;
    for m := 0 to 16 - 1 do
    if fEditPalette[m] = aColor then
    begin
      Result := true;
      Exit;
    end;

  end;
begin
  OpenDialog1.Filter:= 'Bitmap|*.bmp';
  if not OpenDialog1.Execute then Exit;

  Bmp := TBitmap.Create;
  GetMem( Pal, Sizeof( TLogPalette ) + Sizeof( TPaletteEntry ) * 255 );
  try
    Bmp.LoadFromFile(OpenDialog1.FileName);

    Pal^.palVersion := $300;
    Pal^.palNumEntries := 256;

    BitmapPalEntries := GetPaletteEntries( Bmp.Palette, 0, 256, Pal^.palPalEntry[ 0 ] );

    // 0~15 까지는 기본색상. 16~31 까지는 팀색상.
    // 새로운 팔래트값이 기존 색상에 존재하지 않는 경우만 추가해 간다.
    j := 32;
    for i := 0 to BitmapPalEntries - 1 do
    begin
      //fEditPalette[j] :=
      NewColor :=
        Pal^.PalPalEntry[ i ].PeBlue shl 16 +
        Pal^.PalPalEntry[ i ].PeGreen shl 8 +
        Pal^.PalPalEntry[ i ].PeRed;

      if IsBasicColor(NewColor) then Continue;
      fEditPalette[j] := NewColor; 
      Inc(j);

      if j >= Length(fEditPalette) then Break;
    end;
  finally
    Bmp.Free;
    FreeMem(Pal);
  end;
  
  PaintPalette();
//
end;




end.
