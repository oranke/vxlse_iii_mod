unit FormPreferences;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, StdCtrls, ComCtrls, ExtCtrls, Registry, Voxel_Engine;

type
  TFrmPreferences = class(TForm)
    GroupBox1: TGroupBox;
    Pref_List: TTreeView;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    AssociateCheck: TCheckBox;
    GroupBox3: TGroupBox;
    IconPrev: TImage;
    IconID: TTrackBar;
    BtnApply: TButton;
    TabSheet2: TTabSheet;
    CheckBox1: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    ComboBox2: TComboBoxEx;
    ComboBox1: TComboBoxEx;
    Bevel2: TBevel;
    Panel1: TPanel;
    Image1: TImage;
    Label9: TLabel;
    Label10: TLabel;
    Bevel3: TBevel;
    Panel2: TPanel;
    Button4: TButton;
    Button1: TButton;
    procedure BtnApplyClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure IconIDChange(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Pref_ListClick(Sender: TObject);
    procedure Pref_ListKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Pref_ListKeyPress(Sender: TObject; var Key: Char);
    procedure Pref_ListKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure CheckBox1Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    IconPath: String;
    procedure ExtractIcon;
    procedure GetSettings;
  end;

var
  FrmPreferences: TFrmPreferences;

implementation

uses FormMain;

{$R *.dfm}

procedure TFrmPreferences.ExtractIcon;
var
  sWinDir: String;
  iLength: Integer;
  {Res: TResourceStream; }
  MIcon: TIcon;
begin
   // Initialize Variable
   iLength := 255;
   setLength(sWinDir, iLength);
   iLength := GetWindowsDirectory(PChar(sWinDir), iLength);
   setLength(sWinDir, iLength);
   IconPath := sWinDir + '\vxlse'+inttostr(IconID.Position)+'.ico';

   MIcon := TIcon.Create;
   FrmMain.IconList.GetIcon(IconID.Position,MIcon);
   MIcon.SaveToFile(IconPath);
   MIcon.Free;
end;

procedure TFrmPreferences.GetSettings;
begin
   IconIDChange(Self);
end;

procedure TFrmPreferences.BtnApplyClick(Sender: TObject);
var
  Reg: TRegistry;
begin
   ExtractIcon;
   Reg :=TRegistry.Create;
   Reg.RootKey := HKEY_CLASSES_ROOT;

   Config.Icon := IconID.Position;
   Config.Assoc := AssociateCheck.Checked;
   Config.Palette := CheckBox1.Checked;

   if ComboBox1.ItemIndex = 0 {< 1}then
      Config.TS := 'TS'
   else if ComboBox1.ItemIndex = 1 then
      Config.TS := 'RA2'
   else
   begin
      ShowMessage('I am going here with ' + IntToStr(ComboBox1.ItemIndex));
      Config.TS := ComboBox1.Items.Strings[ComboBox1.ItemIndex];
   end;

   if ComboBox2.ItemIndex = 0 then
      Config.RA2 := 'TS'
   else if (ComboBox2.ItemIndex = 1) then// or (frm.ComboBox2.ItemIndex = -1) then
      Config.RA2 := 'RA2'
   else
      Config.RA2 := ComboBox2.Items.Strings[ComboBox2.ItemIndex];

   if Reg.OpenKey('\VXLSe\DefaultIcon\',true) then
   begin
      Reg.WriteString('',IconPath);
      Reg.CloseKey;
   end;

   if AssociateCheck.Checked = true then
   begin
      Reg.RootKey := HKEY_CLASSES_ROOT;
      if Reg.OpenKey('\.vxl\',true) then
      begin
         Reg.WriteString('','VXLSe');
         Reg.CloseKey;
         Reg.RootKey := HKEY_CLASSES_ROOT;
         if Reg.OpenKey('\VXLSe\shell\',true) then
         begin
            Reg.WriteString('','Open');
            if Reg.OpenKey('\VXLSe\shell\open\command\',true) then
            begin
               Reg.WriteString('',ParamStr(0)+' %1');
            end;
            Reg.CloseKey;
            Reg.RootKey := HKEY_CURRENT_USER;
            if Reg.OpenKey('\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\FileExts\.vxl\',true) then
            begin
               Reg.WriteString('Application',ParamStr(0)+' "%1"');
               Reg.CloseKey;
            end
            else
            begin
               ShowMessage('VXLSE III was not able to associate .vxl files because you are under a non-administrative account or does not have enough rights to do it. Contact the system administrator if you need any help');
            end;
         end
         else
         begin
            ShowMessage('VXLSE III was not able to associate .vxl files because you are under a non-administrative account or does not have enough rights to do it. Contact the system administrator if you need any help');
            close;
         end;
      end
      else
      begin
         ShowMessage('VXLSE III was not able to associate .vxl files because you are under a non-administrative account or does not have enough rights to do it. Contact the system administrator if you need any help');
         close;
      end;
   end
   else
   begin
      Reg.RootKey := HKEY_CLASSES_ROOT;
      Reg.DeleteKey('.vxl');
      Reg.CloseKey;
      Reg.RootKey := HKEY_CLASSES_ROOT;
      Reg.DeleteKey('\VXLSe\');
      Reg.CloseKey;
      Reg.RootKey := HKEY_CURRENT_USER;
      Reg.DeleteKey('\SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\FileExts\.vxl\');
      Reg.CloseKey;
   end;
   Reg.Free;
end;

function GetComboBoxNo(filename:string; default : cardinal): cardinal;
var
   f_name : string;
begin
   f_name := ansilowercase(filename);
   result := default;

   if f_name = 'vxlseii_ts.vpal' then
      result := 0
   else if f_name = 'vxlseii_ra2.vpal' then
      result := 1
   else if f_name = 'vxlseii.vpal' then
      result := 2
end;

function GetFilenameFromNo(itemindex:cardinal): string;
begin
   if itemindex = 0 then
      result := 'vxlseii_ts.vpal'
   else if itemindex = 1 then
      result := 'vxlseii_ra2.vpal'
   else if itemindex = 2 then
      result := 'vxlseii.vpal';
end;


procedure TFrmPreferences.FormShow(Sender: TObject);
begin
   GetSettings;
   PageControl1.ActivePageIndex := 0;
   GroupBox1.Caption := 'File Associations';

   ComboBox1.ItemIndex := 0;
   ComboBox2.ItemIndex := 1;
end;

procedure TFrmPreferences.IconIDChange(Sender: TObject);
var
   MIcon: TIcon;//Icon: TResourceStream;
begin
   // Icon := TResourceStream.Create(hInstance,'Icon_'+IntToStr(IconID.Position+1),RT_RCDATA);
   MIcon := TIcon.Create;
   FrmMain.IconList.GetIcon(IconID.Position,MIcon);
   IconPrev.Picture.Icon := MIcon;
   // Icon.Free;
   MIcon.Free;
end;

procedure TFrmPreferences.Button2Click(Sender: TObject);
begin
   Close;
end;

procedure TFrmPreferences.Pref_ListClick(Sender: TObject);
begin
   if pref_list.SelectionCount > 0 then
   begin
      if pref_list.Selected.Text = 'File Associations' then
         PageControl1.ActivePageIndex := 0;
      if pref_list.Selected.Text = 'Palette' then
         PageControl1.ActivePageIndex := 1;
      GroupBox1.Caption := pref_list.Selected.Text;
   end;
end;

procedure TFrmPreferences.Pref_ListKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
   Pref_ListClick(sender);
end;

procedure TFrmPreferences.Pref_ListKeyPress(Sender: TObject;
  var Key: Char);
begin
   Pref_ListClick(sender);
end;

procedure TFrmPreferences.Pref_ListKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
   Pref_ListClick(sender);
end;

procedure TFrmPreferences.CheckBox1Click(Sender: TObject);
begin
   Label1.Enabled := CheckBox1.Checked;
   Label2.Enabled := CheckBox1.Checked;
   ComboBox1.Enabled := CheckBox1.Checked;
   ComboBox2.Enabled := CheckBox1.Checked;
end;

procedure TFrmPreferences.Button4Click(Sender: TObject);
begin
   BtnApplyClick(Sender);
   Close;
end;

procedure TFrmPreferences.FormCreate(Sender: TObject);
var
   x : integer;
begin
   Panel1.DoubleBuffered := true;
   Image1.Picture := FrmMain.TopBarImageHolder.Picture;

   IconID.Max := FrmMain.IconList.Count-1;
   IconID.Position := Config.Icon;
   AssociateCheck.Checked := Config.Assoc;

   CheckBox1.Checked := Config.Palette;

   ComboBox1.Clear;
   ComboBox1.Items.Add('unittem.pal');
   ComboBox1.ItemsEX.Items[ComboBox1.Items.Count-1].ImageIndex := 15;

   ComboBox1.Items.Add('unittem.pal');
   ComboBox1.ItemsEX.Items[ComboBox1.Items.Count-1].ImageIndex := 16;

   if PaletteList.Data_no > 0 then
      for x := 0 to PaletteList.Data_no-1 do
      begin
         ComboBox1.Items.Add(ExtractFileName(PaletteList.Data[x]));
         ComboBox1.ItemsEX.Items[ComboBox1.Items.Count-1].ImageIndex := 24;
      end;

   ComboBox2.Clear;
   ComboBox2.Items.Add('unittem.pal');
   ComboBox2.ItemsEX.Items[ComboBox2.Items.Count-1].ImageIndex := 15;

   ComboBox2.Items.Add('unittem.pal');
   ComboBox2.ItemsEX.Items[ComboBox2.Items.Count-1].ImageIndex := 16;

   if PaletteList.Data_no > 0 then
      for x := 0 to PaletteList.Data_no-1 do
      begin
         ComboBox2.Items.Add(ExtractFileName(PaletteList.Data[x]));
         ComboBox2.ItemsEX.Items[ComboBox2.Items.Count-1].ImageIndex := 24;
      end;
end;

end.
