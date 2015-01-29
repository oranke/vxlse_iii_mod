unit FormReplaceColour;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Voxel_Engine, palette, voxel, mouse, undo_engine;

Type
TReplaceColourData = record
   Col1 : byte;
   Col2 : byte;
end;

type
  TFrmReplaceColour = class(TForm)
    Bevel3: TBevel;
    PanelTitle: TPanel;
    Image1: TImage;
    Label5: TLabel;
    Label6: TLabel;
    Bevel4: TBevel;
    Panel4: TPanel;
    BtOK: TButton;
    BtCancel: TButton;
    Panel5: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    LabelReplace: TLabel;
    LabelWith: TLabel;
    PanelReplace: TPanel;
    PanelWith: TPanel;
    ListBox1: TListBox;
    BtAdd: TButton;
    BtEdit: TButton;
    BtDelete: TButton;
    Bevel1: TBevel;
    cnvPalette: TPaintBox;
    pnlPalette: TPanel;
    lblActiveColour: TLabel;
    pnlActiveColour: TPanel;
    procedure FormDestroy(Sender: TObject);
    procedure cnvPalettePaint(Sender: TObject);
    procedure PanelReplaceClick(Sender: TObject);
    procedure PanelWithClick(Sender: TObject);
    procedure cnvPaletteMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure BtAddClick(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure BtEditClick(Sender: TObject);
    procedure BtDeleteClick(Sender: TObject);
    procedure cnvPaletteMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BtOKClick(Sender: TObject);
    procedure BtCancelClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmReplaceColour: TFrmReplaceColour;
  ReplaceColourData,TempColourData : array of TReplaceColourData;
  Data_no : integer;
  Replace : integer = -1;
  ReplaceW : integer = -1;

implementation

uses FormMain;

{$R *.dfm}

procedure TFrmReplaceColour.cnvPalettePaint(Sender: TObject);
begin
   PaintPalette(cnvPalette, false);
end;

procedure TFrmReplaceColour.PanelReplaceClick(Sender: TObject);
begin
   PanelReplace.BevelOuter := bvRaised;
   PanelWith.BevelOuter := bvLowered;
end;

procedure TFrmReplaceColour.PanelWithClick(Sender: TObject);
begin
   PanelWith.BevelOuter := bvRaised;
   PanelReplace.BevelOuter := bvLowered;
end;

procedure TFrmReplaceColour.cnvPaletteMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
   colwidth, rowheight: Real;
   i, j, idx: Integer;
begin
   If not isEditable then exit;

   colwidth := cnvPalette.Width / 8;
   rowheight := cnvPalette.Height / 32;
   i := Trunc(X / colwidth);
   j := Trunc(Y / rowheight);
   idx := (i * 32) + j;

   if SpectrumMode = ModeColours then
   begin
      if PanelReplace.BevelOuter = bvRaised then
      begin
         Replace := idx;
         PanelReplace.Color := VXLPalette[idx];
         LabelReplace.Caption := inttostr(idx);
         PanelWith.BevelOuter := bvRaised;
         PanelReplace.BevelOuter := bvLowered;
      end
      else
      begin
         ReplaceW := idx;
         PanelWith.Color := VXLPalette[idx];
         LabelWith.Caption := inttostr(idx);
         PanelReplace.BevelOuter := bvRaised;
         PanelWith.BevelOuter := bvLowered;
      end;
   end
   else if not (idx > ActiveNormalsCount-1) then
      if PanelReplace.BevelOuter = bvRaised then
      begin
         Replace := idx;
         PanelReplace.Color := GetVXLPaletteColor(idx);
         LabelReplace.Caption := inttostr(idx);
         PanelWith.BevelOuter := bvRaised;
         PanelReplace.BevelOuter := bvLowered;
      end
   else
   begin
      ReplaceW := idx;
      PanelWith.Color := GetVXLPaletteColor(idx);
      LabelWith.Caption := inttostr(idx);
      PanelReplace.BevelOuter := bvRaised;
      PanelWith.BevelOuter := bvLowered;
   end;
end;

procedure TFrmReplaceColour.FormCreate(Sender: TObject);
begin
   cnvPalette.Cursor := MouseBrush;
   PanelTitle.DoubleBuffered := true;
end;

procedure TFrmReplaceColour.BtAddClick(Sender: TObject);
var
   x : integer;
   B : Boolean;
begin
   if (Replace = -1) or (ReplaceW = -1) then
   begin
      Messagebox(0,'Please Select 2 Colours','Colour Error',0);
      exit;
   end;

   B := false;

   if Replace = ReplaceW then exit; // U can't replace a colour with its self... pointless

   if Data_no > 0 then
      for x := 0 to Data_no-1 do
         if b = false then
            if ReplaceColourData[x].Col1 = Replace then
               B := true;

   if B = true then exit; // Stops pointless additions

   inc(Data_no);
   SetLength(ReplaceColourData,Data_no);
   ReplaceColourData[Data_no-1].Col1 := Replace;
   ReplaceColourData[Data_no-1].Col2 := ReplaceW;
   ListBox1.Items.Add(Inttostr(Replace) + ' -> ' + Inttostr(ReplaceW));
end;

procedure TFrmReplaceColour.ListBox1Click(Sender: TObject);
begin
   if Data_no > 0 then
   begin
      Replace := ReplaceColourData[ListBox1.ItemIndex].Col1;
      ReplaceW := ReplaceColourData[ListBox1.ItemIndex].Col2;
      PanelReplace.Color := GetVXLPaletteColor(Replace);
      PanelWith.Color := GetVXLPaletteColor(ReplaceW);
      LabelReplace.Caption := inttostr(Replace);
      LabelWith.Caption := inttostr(ReplaceW);
   end;
end;

procedure TFrmReplaceColour.BtEditClick(Sender: TObject);
begin
   if Data_no > 0 then
   begin
      ReplaceColourData[ListBox1.ItemIndex].Col1 := Replace;
      ReplaceColourData[ListBox1.ItemIndex].Col2 := ReplaceW;
      ListBox1.Items.Strings[ListBox1.ItemIndex] := Inttostr(Replace) + ' -> ' + Inttostr(ReplaceW);
   end;
end;

procedure TFrmReplaceColour.BtDeleteClick(Sender: TObject);
var
   x,c,ItemIndex : integer;
begin
   if data_no < 1 then exit;

   // Get victim
   ItemIndex := ListBox1.ItemIndex;
   // Copy replace data to tempcolour data.
   Setlength(TempColourData,data_no);
   for x := Low(TempColourData) to High(TempColourData) do
   begin
      TempColourData[x].Col1 := ReplaceColourData[x].Col1;
      TempColourData[x].Col2 := ReplaceColourData[x].Col2;
   end;
   // Reduce Temp colour data.
   Dec(data_no);
   Setlength(ReplaceColourData,data_no);
   // Clear list.
   ListBox1.Items.Clear;
   // If empty, leaves.
   if data_no < 1 then
   begin
      Setlength(TempColourData,0);
      exit;
   end;
   // else, rebuid the list.
   C := 0;
   X := 0;
   Repeat
      if x <> ItemIndex then
      begin
         ReplaceColourData[c].Col1 := TempColourData[x].Col1;
         ReplaceColourData[c].Col2 := TempColourData[x].Col2;
         ListBox1.Items.Add(Inttostr(TempColourData[x].Col1) + ' -> ' + Inttostr(TempColourData[x].Col2));
         inc(c);
      end;
      inc(x);
   until c = data_no;
   // and wipe tem colour data.
   Setlength(TempColourData,0);
end;

procedure TFrmReplaceColour.cnvPaletteMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
   colwidth, rowheight: Real;
   i, j, idx: Integer;
begin
   If not isEditable then exit;

   colwidth := cnvPalette.Width / 8;
   rowheight := cnvPalette.Height / 32;
   i := Trunc(X / colwidth);
   j := Trunc(Y / rowheight);
   idx := (i * 32) + j;

   if (SpectrumMode = ModeNormals) and (idx > ActiveNormalsCount-1) then exit;

   lblActiveColour.Caption := inttostr(idx);
   pnlActiveColour.Color := GetVXLPaletteColor(idx);
end;

procedure TFrmReplaceColour.FormClose(Sender: TObject;
var
   Action: TCloseAction);
begin
   data_no := 0;
   Setlength(ReplaceColourData,0);
end;

procedure TFrmReplaceColour.BtOKClick(Sender: TObject);
var
   ColourArray : array [0..255] of Byte;
   x,y,z : integer;
   v : TVoxelUnPacked;
begin
   if data_no < 1 then Close; // nothing to do, so close.

   CreateVXLRestorePoint(ActiveSection,Undo); // Save Undo

   For x := 0 to 255 do
      ColourArray[x] := x;

   For x := 0 to data_no-1 do
      ColourArray[ReplaceColourData[x].Col1] := ReplaceColourData[x].Col2;

   For x := 0 to ActiveSection.Tailer.XSize -1 do
   For y := 0 to ActiveSection.Tailer.YSize -1 do
   For z := 0 to ActiveSection.Tailer.ZSize -1 do
   begin
      ActiveSection.GetVoxel(x,y,z,v);

      if SpectrumMode = ModeColours then
         V.Colour := ColourArray[V.Colour]
      else
         V.Normal := ColourArray[V.Normal];

      ActiveSection.SetVoxel(x,y,z,v);
   end;

   FrmMain.RefreshAll;
   FrmMain.UpdateUndo_RedoState;
   VXLChanged := true;
   Close;
end;

procedure TFrmReplaceColour.BtCancelClick(Sender: TObject);
begin
   Close;
end;

procedure TFrmReplaceColour.FormDestroy(Sender: TObject);
begin
   SetLength(ReplaceColourData,0);
   SetLength(TempColourData,0);
end;

end.
