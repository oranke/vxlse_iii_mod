unit FormVoxelTexture;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls,voxel, ExtDlgs,math, ComCtrls, Voxel_Engine, Palette,
  undo_engine;

type
  TFrmVoxelTexture = class(TForm)
    Panel1: TPanel;
    Image1: TImage;
    Button1: TButton;
    Button6: TButton;
    Button2: TButton;
    Button3: TButton;
    OpenPictureDialog1: TOpenPictureDialog;
    SavePictureDialog1: TSavePictureDialog;
    Button4: TButton;
    Image2: TImage;
    CheckBox1: TCheckBox;
    Bevel2: TBevel;
    Panel2: TPanel;
    Image3: TImage;
    Label2: TLabel;
    Label3: TLabel;
    Bevel3: TBevel;
    Panel3: TPanel;
    Button8: TButton;
    Button9: TButton;
    ProgressBar1: TProgressBar;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }

  end;

var
  FrmVoxelTexture: TFrmVoxelTexture;

implementation

uses FormMain;

{$R *.dfm}

procedure TFrmVoxelTexture.Button1Click(Sender: TObject);
var
x,y,z,zmax,xmax,ymax,lastheight : integer;
v : tvoxelunpacked;
begin
Image1.Picture.Bitmap.Canvas.Brush.Color := GetVXLPaletteColor(-1);
Image1.Picture.Bitmap.Width := 0;
Image1.Picture.Bitmap.Height := 0;

Image1.Picture.Bitmap.Width := ActiveSection.Tailer.XSize;
Image1.Picture.Bitmap.Height := ActiveSection.Tailer.YSize;
zmax := ActiveSection.Tailer.ZSize-1;

for x := 0 to ActiveSection.Tailer.XSize-1 do
for y := 0 to ActiveSection.Tailer.YSize-1 do
begin
z := 0;
repeat
ActiveSection.GetVoxel(x,y,z,v);
if v.Used then
begin

Image1.Picture.Bitmap.Canvas.Brush.Color := GetVXLPaletteColor(v.colour);
Image1.Picture.Bitmap.Canvas.Pixels[x,y] := Image1.Picture.Bitmap.Canvas.Brush.Color;
end;
z := z + 1;
until (z > zmax) or (v.Used = true);
end;

Image1.Picture.Bitmap.Canvas.Brush.Color := GetVXLPaletteColor(-1);

lastheight := Image1.Picture.Bitmap.Height+1;
Image1.Picture.Bitmap.Height := lastheight + ActiveSection.Tailer.YSize;
zmax := ActiveSection.Tailer.ZSize-1;

for x := 0 to ActiveSection.Tailer.XSize-1 do
for y := 0 to ActiveSection.Tailer.YSize-1 do
begin
z := zmax;
repeat
ActiveSection.GetVoxel(x,y,z,v);

if v.Used then
begin

Image1.Picture.Bitmap.Canvas.Brush.Color := GetVXLPaletteColor(v.colour);
Image1.Picture.Bitmap.Canvas.Pixels[x,lastheight+y] := Image1.Picture.Bitmap.Canvas.Brush.Color;
end;
z := z - 1;
until (z < 0) or (v.Used = true);
end;

Image1.Picture.Bitmap.Canvas.Brush.Color := GetVXLPaletteColor(-1);

lastheight := Image1.Picture.Bitmap.Height+1;
Image1.Picture.Bitmap.Height := lastheight + ActiveSection.Tailer.ZSize;
ymax := ActiveSection.Tailer.YSize-1;

for x := 0 to ActiveSection.Tailer.XSize-1 do
for z := 0 to ActiveSection.Tailer.ZSize-1 do
begin
y := 0;
repeat
ActiveSection.GetVoxel(x,y,z,v);
if v.Used then
begin
Image1.Picture.Bitmap.Canvas.Brush.Color := GetVXLPaletteColor(v.colour);
Image1.Picture.Bitmap.Canvas.Pixels[x,lastheight+z] := Image1.Picture.Bitmap.Canvas.Brush.Color;
end;
y := y + 1;
until (y > ymax) or (v.Used = true);
end;

Image1.Picture.Bitmap.Canvas.Brush.Color := GetVXLPaletteColor(-1);

lastheight := Image1.Picture.Bitmap.Height+1;
Image1.Picture.Bitmap.Height := lastheight + ActiveSection.Tailer.ZSize;
ymax := ActiveSection.Tailer.YSize-1;

for x := 0 to ActiveSection.Tailer.XSize-1 do
for z := 0 to ActiveSection.Tailer.ZSize-1 do
begin
y := ymax;
repeat
ActiveSection.GetVoxel(x,y,z,v);
if v.Used then
begin
Image1.Picture.Bitmap.Canvas.Brush.Color := GetVXLPaletteColor(v.colour);
Image1.Picture.Bitmap.Canvas.Pixels[x,lastheight+z] := Image1.Picture.Bitmap.Canvas.Brush.Color;
end;
y := y - 1;
until (y < 0) or (v.Used = true);
end;

Image1.Picture.Bitmap.Canvas.Brush.Color := GetVXLPaletteColor(-1);

lastheight := Image1.Picture.Bitmap.Height+1;
Image1.Picture.Bitmap.Height := lastheight + ActiveSection.Tailer.YSize;
xmax := ActiveSection.Tailer.XSize-1;

for z := 0 to ActiveSection.Tailer.ZSize-1 do
for y := 0 to ActiveSection.Tailer.YSize-1 do
begin
x := 0;
repeat
ActiveSection.GetVoxel(x,y,z,v);
if v.Used then
begin
Image1.Picture.Bitmap.Canvas.Brush.Color := GetVXLPaletteColor(v.colour);
Image1.Picture.Bitmap.Canvas.Pixels[z,lastheight+y] := Image1.Picture.Bitmap.Canvas.Brush.Color;
end;
x := x + 1;
until (x > xmax) or (v.Used = true);
end;

Image1.Picture.Bitmap.Canvas.Brush.Color := GetVXLPaletteColor(-1);

lastheight := Image1.Picture.Bitmap.Height+1;
Image1.Picture.Bitmap.Height := lastheight + ActiveSection.Tailer.YSize;
xmax := ActiveSection.Tailer.XSize-1;

for z := 0 to ActiveSection.Tailer.ZSize-1 do
for y := 0 to ActiveSection.Tailer.YSize-1 do
begin
x := xmax;
repeat
ActiveSection.GetVoxel(x,y,z,v);
if v.Used then
begin
Image1.Picture.Bitmap.Canvas.Brush.Color := GetVXLPaletteColor(v.colour);
Image1.Picture.Bitmap.Canvas.Pixels[z,lastheight+y] := Image1.Picture.Bitmap.Canvas.Brush.Color;
end;
x := x - 1;
until (x < 0) or (v.Used = true);
end;

Image1.refresh;
end;

procedure TFrmVoxelTexture.FormShow(Sender: TObject);
begin
Image1.Picture.Bitmap.FreeImage;
Button1Click(sender);
end;

procedure TFrmVoxelTexture.Button2Click(Sender: TObject);
begin
if OpenPictureDialog1.Execute then
Image1.Picture.LoadFromFile(OpenPictureDialog1.filename);
end;

procedure TFrmVoxelTexture.Button3Click(Sender: TObject);
begin
if SavePictureDialog1.Execute then
image1.Picture.SaveToFile(SavePictureDialog1.FileName);
end;

procedure TFrmVoxelTexture.Button4Click(Sender: TObject);
var x,y,c,cmax,size : integer;
begin
if SavePictureDialog1.Execute then
begin

if SpectrumMode = ModeColours then
cmax := 256
else
cmax := ActiveNormalsCount;

size := 5;

Image2.Picture.Bitmap.width := 0;
Image2.Picture.Bitmap.height := 0;

Image2.Picture.Bitmap.Canvas.Brush.Color := GetVXLPaletteColor(-1);

Image2.Picture.Bitmap.width := 8*size;
Image2.Picture.Bitmap.height := 32*size;
c := 0;
for x := 0 to 7 do
for y := 0 to 31 do
begin
if c < cmax then
begin
Image2.Picture.Bitmap.Canvas.Brush.Color := GetVXLPaletteColor(c);
Image2.Picture.Bitmap.Canvas.FillRect(rect(x*size,y*size,x*size+size,y*size+size));
end;
c := c +1;
end;

Image2.Picture.SaveToFile(SavePictureDialog1.FileName);
end;
end;

procedure TFrmVoxelTexture.Button6Click(Sender: TObject);
var
x,y,z,zmax,xmax,ymax,lastheight,col,pp : integer;
v : tvoxelunpacked;
begin
//zmax := ActiveSection.Tailer.ZSize-1;

ProgressBar1.Visible := true;

if CheckBox1.Checked then
begin
Label1.Visible := true;
Label1.Caption := 'Applying Bottom View To Layers';
Label1.Refresh;
lastheight := 0;

ProgressBar1.Position := 0;
ProgressBar1.Max := ActiveSection.Tailer.XSize *2;

for x := 0 to ActiveSection.Tailer.XSize-1 do
begin
ProgressBar1.Position := x;
for y := 0 to ActiveSection.Tailer.YSize-1 do
for z := 0 to ((ActiveSection.Tailer.ZSize-1) div 2) do
begin

ActiveSection.GetVoxel(x,y,((ActiveSection.Tailer.ZSize-1) div 2)-z,v);
if v.Used then
begin
col := getsquarecolour(x,lastheight+y,Image1.Picture.Bitmap);

if col <> GetVXLPaletteColor(-1) then
if SpectrumMode = ModeColours then
v.Colour := col
else
v.Normal := col;

ActiveSection.SetVoxel(x,y,((ActiveSection.Tailer.ZSize-1) div 2)-z,v);
end;

end;
end;

Label1.Caption := 'Applying Top View To Layers';
Label1.Refresh;
lastheight := ActiveSection.Tailer.YSize+1;

for x := 0 to ActiveSection.Tailer.XSize-1 do
begin
ProgressBar1.Position := ActiveSection.Tailer.XSize+x;
for y := 0 to ActiveSection.Tailer.YSize-1 do
for z := 0 to ((ActiveSection.Tailer.ZSize-1) div 2) do
begin

ActiveSection.GetVoxel(x,y,((ActiveSection.Tailer.ZSize-1) div 2)+z,v);
if v.Used then
begin
col := getsquarecolour(x,lastheight+y,Image1.Picture.Bitmap);

if col <> GetVXLPaletteColor(-1) then
if SpectrumMode = ModeColours then
v.Colour := col
else
v.Normal := col;

ActiveSection.SetVoxel(x,y,((ActiveSection.Tailer.ZSize-1) div 2)+z,v);
end;

end;
end;

end;

Label1.Visible := true;
Label1.Caption := 'Applying Texture To Left And Right Sides';
Label1.Refresh;

ProgressBar1.Max := ((ActiveSection.Tailer.XSize-1)*4)+ ((ActiveSection.Tailer.ZSize-1)*2);
{
for x := 0 to ActiveSection.Tailer.XSize-1 do
for y := 0 to ActiveSection.Tailer.YSize-1 do
begin
z := 0;
ProgressBar1.Position := x;
repeat
ActiveSection.GetVoxel(x,y,z,v);
if v.Used then
begin
col := getsquarecolour(x,y);

if col <> GetVXLPaletteColor(-1) then
if SpectrumMode = ModeColours then
v.Colour := col
else
v.Normal := col;

ActiveSection.SetVoxel(x,y,z,v);
end;
z := z + 1;
until (z > zmax) or (v.Used = true);
end;
   }
lastheight := ActiveSection.Tailer.YSize+1;
zmax := ActiveSection.Tailer.ZSize-1;
{
for x := 0 to ActiveSection.Tailer.XSize-1 do
for y := 0 to ActiveSection.Tailer.YSize-1 do
begin
z := zmax;
ProgressBar1.Position := x + (ActiveSection.Tailer.XSize-1);
repeat
ActiveSection.GetVoxel(x,y,z,v);
if v.Used then
begin
col := getsquarecolour(x,lastheight+y);

if col <> GetVXLPaletteColor(-1) then
if SpectrumMode = ModeColours then
v.Colour := col
else
v.Normal := col;

ActiveSection.SetVoxel(x,y,z,v);
end;
z := z - 1;
until (z < 0) or (v.Used = true);
end;                        }

lastheight := lastheight +ActiveSection.Tailer.YSize+1;
ymax := ActiveSection.Tailer.YSize-1;
pp := 0;

for x := 0 to ActiveSection.Tailer.XSize-1 do
for z := 0 to ActiveSection.Tailer.ZSize-1 do
begin
y := 0;
ProgressBar1.Position := x +pp;
repeat
ActiveSection.GetVoxel(x,y,z,v);
if v.Used then
begin

col := getsquarecolour(x,lastheight+z,Image1.Picture.Bitmap);

if col <> GetVXLPaletteColor(-1) then
if SpectrumMode = ModeColours then
v.Colour := col
else
v.Normal := col;

ActiveSection.SetVoxel(x,y,z,v);
end;
y := y + 1;
until (y > ymax) or (v.Used = true);
end;

lastheight := lastheight +ActiveSection.Tailer.ZSize+1;
ymax := ActiveSection.Tailer.YSize-1;
pp := ProgressBar1.Position;

for x := 0 to ActiveSection.Tailer.XSize-1 do
for z := 0 to ActiveSection.Tailer.ZSize-1 do
begin
y := ymax;
ProgressBar1.Position := x + pp;
repeat
ActiveSection.GetVoxel(x,y,z,v);
if v.Used then
begin
col := getsquarecolour(x,lastheight+z,Image1.Picture.Bitmap);

if col <> GetVXLPaletteColor(-1) then
if SpectrumMode = ModeColours then
v.Colour := col
else
v.Normal := col;

ActiveSection.SetVoxel(x,y,z,v);
end;
y := y - 1;
until (y < 0) or (v.Used = true);
end;

lastheight := lastheight +ActiveSection.Tailer.ZSize+1;
xmax := ActiveSection.Tailer.XSize-1;
pp := ProgressBar1.Position;

Label1.Caption := 'Applying Texture To Front And Back Sides';
Label1.Refresh;

for z := 0 to ActiveSection.Tailer.ZSize-1 do
for y := 0 to ActiveSection.Tailer.YSize-1 do
begin
x := 0;
ProgressBar1.Position := z + pp;
repeat
ActiveSection.GetVoxel(x,y,z,v);
if v.Used then
begin
col := getsquarecolour(z,lastheight+y,Image1.Picture.Bitmap);

if col <> GetVXLPaletteColor(-1) then
if SpectrumMode = ModeColours then
v.Colour := col
else
v.Normal := col;

ActiveSection.SetVoxel(x,y,z,v);
end;
x := x + 1;
until (x > xmax) or (v.Used = true);
end;

lastheight := lastheight +ActiveSection.Tailer.YSize+1;
xmax := ActiveSection.Tailer.XSize-1;
pp := ProgressBar1.Position;

for z := 0 to ActiveSection.Tailer.ZSize-1 do
for y := 0 to ActiveSection.Tailer.YSize-1 do
begin
x := xmax;
ProgressBar1.Position := z + pp;
repeat
ActiveSection.GetVoxel(x,y,z,v);
if v.Used then
begin
col := getsquarecolour(z,lastheight+y,Image1.Picture.Bitmap);

if col <> GetVXLPaletteColor(-1) then
if SpectrumMode = ModeColours then
v.Colour := col
else
v.Normal := col;

ActiveSection.SetVoxel(x,y,z,v);
end;
x := x - 1;
until (x < 0) or (v.Used = true);
end;

// Apply top and bottom images
pp := ProgressBar1.Position;

Label1.Caption := 'Applying Texture To Top And Bottom Sides';
Label1.Refresh;

for x := 0 to ActiveSection.Tailer.XSize-1 do
for y := 0 to ActiveSection.Tailer.YSize-1 do
begin
z := 0;
ProgressBar1.Position := x + pp;
repeat
ActiveSection.GetVoxel(x,y,z,v);
if v.Used then
begin
col := getsquarecolour(x,y,Image1.Picture.Bitmap);

if col <> GetVXLPaletteColor(-1) then
if SpectrumMode = ModeColours then
v.Colour := col
else
v.Normal := col;

ActiveSection.SetVoxel(x,y,z,v);
end;
z := z + 1;
until (z > zmax) or (v.Used = true);
end;

lastheight := ActiveSection.Tailer.YSize+1;
zmax := ActiveSection.Tailer.ZSize-1;
pp := ProgressBar1.Position;

for x := 0 to ActiveSection.Tailer.XSize-1 do
for y := 0 to ActiveSection.Tailer.YSize-1 do
begin
z := zmax;
ProgressBar1.Position := x + pp;
repeat
ActiveSection.GetVoxel(x,y,z,v);
if v.Used then
begin
col := getsquarecolour(x,lastheight+y,Image1.Picture.Bitmap);

if col <> GetVXLPaletteColor(-1) then
if SpectrumMode = ModeColours then
v.Colour := col
else
v.Normal := col;

ActiveSection.SetVoxel(x,y,z,v);
end;
z := z - 1;
until (z < 0) or (v.Used = true);
end;

ProgressBar1.Visible := false;
Label1.Visible := false;

FrmMain.RefreshAll;
//showmessage('Texture Applyed to Voxel');
end;

procedure TFrmVoxelTexture.Button8Click(Sender: TObject);
begin
CreateVXLRestorePoint(ActiveSection,Undo);
Button6Click(sender);

FrmMain.UpdateUndo_RedoState;
//FrmMain.RefreshAll;
VXLChanged := true;
Close;
end;

procedure TFrmVoxelTexture.Button9Click(Sender: TObject);
begin
Close;
end;

procedure TFrmVoxelTexture.FormCreate(Sender: TObject);
begin
Panel2.DoubleBuffered := true;
end;

end.
