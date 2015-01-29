unit FormAutoNormals;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Voxel_Engine, Voxel_Tools, Voxel, Undo_Engine, Voxel_AutoNormals;

const
   // I've found these magic numbers in the sphere area:
   // sqrt(NumNormals / (4 * pi) = MagicNumber

   // NumNormals is 244 for RA2 and 36 for TS.
   // It technically finds the ray of the sphere.
   TS_MAGIC_NUMBER = 1.6925687506432688608442383546829;
   RA2_MAGIC_NUMBER = 3.5449077018110320545963349666812;
   RANGE_DEFAULT = 1;
   RANGE_CUSTOM = 2;

   AUTONORMALS_TANGENT = '8.2';
   AUTONORMALS_INFLUENCE = '7.1';
   AUTONORMALS_CUBED = '5.6';
   AUTONORMALS_6FACED = '1.1';

type
   TFrmAutoNormals = class(TForm)
      GbNormalizationMethod: TGroupBox;
      RbInfluence: TRadioButton;
      RbCubed: TRadioButton;
      Rb6Faced: TRadioButton;
      GbInfluenceOptions: TGroupBox;
      LbRange: TListBox;
      Label1: TLabel;
      Label2: TLabel;
      EdRange: TEdit;
      Label3: TLabel;
      CbSmoothMe: TCheckBox;
      BtOK: TButton;
      BtCancel: TButton;
      Label4: TLabel;
      Label5: TLabel;
      Label6: TLabel;
      CbInfluenceMap: TCheckBox;
      BtTips: TButton;
      CbNewPixelsOnly: TCheckBox;
      CbIncreaseContrast: TCheckBox;
      Label7: TLabel;
      EdContrast: TEdit;
      EdSmooth: TEdit;
      Label8: TLabel;
      RbTangent: TRadioButton;
      procedure RbTangentClick(Sender: TObject);
      procedure BtTipsClick(Sender: TObject);
      procedure BtOKClick(Sender: TObject);
      procedure Rb6FacedClick(Sender: TObject);
      procedure RbCubedClick(Sender: TObject);
      procedure RbInfluenceClick(Sender: TObject);
      procedure EdRangeChange(Sender: TObject);
      procedure LbRangeClick(Sender: TObject);
      procedure FormShow(Sender: TObject);
      procedure BtCancelClick(Sender: TObject);
   private
      { Private declarations }
      procedure ShowInfluenceOptions(value : boolean);
   public
      { Public declarations }
      MyVoxel : TVoxelSection;
   end;

implementation

{$R *.dfm}

uses FormMain;

procedure TFrmAutoNormals.ShowInfluenceOptions(value : boolean);
begin
   GbInfluenceOptions.Enabled := value;
   LbRange.Enabled := value;
   EdRange.Enabled := value and (LbRange.ItemIndex = RANGE_CUSTOM);
   CbSmoothMe.Enabled := value and (not RbTangent.Checked);
   CbInfluenceMap.Enabled := value and RbCubed.Checked;
   CbIncreaseContrast.Enabled := value and RbInfluence.Checked;
   EdContrast.Enabled := value and (not RbTangent.Checked);
   EdSmooth.Enabled := value and (not RbTangent.Checked);
   CbNewPixelsOnly.Enabled := value and (not RbTangent.Checked);
   if RbInfluence.Checked then
   begin
      CbIncreaseContrast.Caption := 'Stretch Influence Map';
      CbIncreaseContrast.Enabled := true;
   end
   else if RbTangent.Checked then
   begin
      CbIncreaseContrast.Caption := 'Treat Descontinuities';
      CbIncreaseContrast.Enabled := true;
   end
   else
   begin
      CbIncreaseContrast.Caption := 'Stretch Influence Map';
      CbIncreaseContrast.Enabled := false;
   end;
end;

procedure TFrmAutoNormals.FormShow(Sender: TObject);
begin
   RbTangent.Checked := true;
   ShowInfluenceOptions(true);
   LbRange.ItemIndex := RANGE_DEFAULT;
   LbRangeClick(Sender);
   Caption := 'AutoNormals ' + AUTONORMALS_TANGENT;
   RbTangent.Caption := 'Tangent Plane Auto Normals (v' + AUTONORMALS_TANGENT + ', recommended)';
   RbInfluence.Caption := 'Influence Auto Normals (v' + AUTONORMALS_INFLUENCE + ')';
   RbCubed.Caption := 'Cubed Auto Normals (v' + AUTONORMALS_CUBED + ')';
   Rb6Faced.Caption := '6-Faced Auto Normals (v' + AUTONORMALS_6FACED + ')';
end;

procedure TFrmAutoNormals.LbRangeClick(Sender: TObject);
begin
   if LbRange.ItemIndex = -1 then
      LbRange.ItemIndex := RANGE_DEFAULT;
   case (LbRange.ItemIndex) of
      0: EdRange.Text := FloatToStr(TS_MAGIC_NUMBER);
      1: EdRange.Text := FloatToStr(RA2_MAGIC_NUMBER);
   end;
   EdRange.Enabled := (LbRange.ItemIndex = RANGE_CUSTOM);
end;

procedure TFrmAutoNormals.EdRangeChange(Sender: TObject);
var
   Range : single;
   Contrast : integer;
   Smooth : single;
begin
   BtOK.Enabled := true;
   Range := StrToFloatDef(EdRange.Text,0);
   Contrast := StrToIntDef(EdContrast.Text,1);
   Smooth := StrToFloatDef(EdSmooth.Text,1);
   if (Range < 1) or (Contrast < 1) or (Smooth < 1) then
      BtOK.Enabled := false;
end;

procedure TFrmAutoNormals.RbTangentClick(Sender: TObject);
begin
   ShowInfluenceOptions(RbTangent.Checked);
   GbInfluenceOptions.Caption := 'Tangent Plane Normalizer Options..';
end;

procedure TFrmAutoNormals.RbInfluenceClick(Sender: TObject);
begin
   ShowInfluenceOptions(RbInfluence.Checked);
   GbInfluenceOptions.Caption := 'Influence Normalizer Options..';
end;

procedure TFrmAutoNormals.RbCubedClick(Sender: TObject);
begin
   ShowInfluenceOptions(RbCubed.Checked);
   GbInfluenceOptions.Caption := 'Smooth Cubed Normalizer Options..';
end;

procedure TFrmAutoNormals.Rb6FacedClick(Sender: TObject);
begin
   ShowInfluenceOptions(not Rb6Faced.Checked);
end;

procedure TFrmAutoNormals.BtCancelClick(Sender: TObject);
begin
   close;
end;

procedure TFrmAutoNormals.BtOKClick(Sender: TObject);
var
   Res : TApplyNormalsResult;
   Range,Smooth : single;
   Contrast : integer;
begin
   CreateVXLRestorePoint(ActiveSection,Undo);
   FrmMain.UpdateUndo_RedoState;

   If RbTangent.Checked then
   begin
      Range := StrToFloatDef(EdRange.Text,0);
      if Range < 1 then
      begin
         MessageBox(0,pchar('Range Value Is Invalid. It Must Be Positive and Higher Than 1. Using 3.54 (Default).'),'Auto Normals Warning',0);
         Range := RA2_MAGIC_NUMBER;
      end;
      Res := AcharNormais(MyVoxel,Range,CbIncreaseContrast.Checked);
      MessageBox(0,pchar('AutoNormals v' + AUTONORMALS_TANGENT + #13#13 + 'Total: ' + inttostr(Res.applied) + ' voxels modified.'),'Tangent Plane Auto Normal Results',0);
   end
   else If RbInfluence.Checked then
   begin
      Range := StrToFloatDef(EdRange.Text,0);
      Contrast := StrToIntDef(EdContrast.Text,1);
      if CbSmoothMe.Checked and CbSmoothMe.Enabled then
      begin
         Smooth := StrToFloatDef(EdSmooth.Text,1);
         if (Smooth < 1) and CbSmoothMe.Checked and CbSmoothMe.Enabled then
         begin
            MessageBox(0,pchar('Smooth Value Is Invalid. It Must Be Positive, Integer and Higher Or Equal Than 1. Using 1 (Default).'),'Auto Normals Warning',0);
            Smooth := 1;
         end;
      end
      else
         Smooth := Range;
      if Range < 1 then
      begin
         MessageBox(0,pchar('Range Value Is Invalid. It Must Be Positive and Higher Than 1. Using 3.54 (Default).'),'Auto Normals Warning',0);
         Range := RA2_MAGIC_NUMBER;
      end;
      if Contrast < 1 then
      begin
         MessageBox(0,pchar('Contrast Value Is Invalid. It Must Be Positive, Integer and Higher Or Equal Than 1. Using 1 (Default).'),'Auto Normals Warning',0);
         Contrast := 1;
      end;
      Res := ApplyInfluenceNormals(MyVoxel,Range,Smooth,Contrast,CbSmoothMe.Checked,CbNewPixelsOnly.checked,CbIncreaseContrast.checked);
      MessageBox(0,pchar('AutoNormals v' + AUTONORMALS_INFLUENCE + #13#13 + 'Total: ' + inttostr(Res.applied) + ' voxels modified.'),'Influence Auto Normal Results',0);
   end
   else if RbCubed.Checked then
   begin
      Range := StrToFloatDef(EdRange.Text,0);
      Contrast := StrToIntDef(EdContrast.Text,0);
      if CbSmoothMe.Checked and CbSmoothMe.Enabled then
      begin
         Smooth := StrToFloatDef(EdSmooth.Text,1);
         if (Smooth < 1) and CbSmoothMe.Checked and CbSmoothMe.Enabled then
         begin
            MessageBox(0,pchar('Smooth Value Is Invalid. It Must Be Positive, Integer and Higher Or Equal Than 1. Using 1 (Default).'),'Auto Normals Warning',0);
            Smooth := 1;
         end;
      end
      else
         Smooth := Range;
      if Range < 1 then
      begin
         MessageBox(0,pchar('Range Value Is Invalid. It Must Be Positive and Higher Than 1. Using 3.54 (Default).'),'Auto Normals Warning',0);
         Range := RA2_MAGIC_NUMBER;
      end;
      if Contrast < 1 then
      begin
         MessageBox(0,pchar('Contrast Value Is Invalid. It Must Be Positive, Integer and Higher Or Equal Than 1. Using 1 (Default).'),'Auto Normals Warning',0);
         Contrast := 1;
      end;
      Res := ApplyCubedNormals(MyVoxel,Range,Smooth,Contrast,CbSmoothMe.Checked,CbInfluenceMap.Checked,CbNewPixelsOnly.checked);
      MessageBox(0,pchar('AutoNormals v' + AUTONORMALS_CUBED + #13#13 + 'Total: ' + inttostr(Res.applied) + ' voxels modified.'),'Cubed Auto Normal Results',0);
   end
   else
   begin
      Res := ApplyNormals(MyVoxel);
      MessageBox(0,pchar('AutoNormals v' + AUTONORMALS_6FACED + #13#13 + 'Total: ' + inttostr(Res.applied + Res.confused) + #13 +'Applied: ' + inttostr(Res.applied) + #13 + 'Confused: ' +inttostr(Res.confused)),'6-Faced Auto Normal Results',0);
      if Res.confused > 0 then
         if MessageDlg('Some were Confused, This may mean there are redundant voxels.'+#13#13+'Run Remove Redundant Voxels?',mtConfirmation,[mbYes,mbNo],0) = mrYes then
            FrmMain.RemoveRedundentVoxels1Click(Sender);
   end;

   VXLChanged := true;
   FrmMain.Refreshall;
   Close;
end;

procedure TFrmAutoNormals.BtTipsClick(Sender: TObject);
begin
   FrmMain.OpenHyperLink('http://www.ppmsite.com/index.php?go=normalstips');
end;

end.
