unit FormBoundsMAnager;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, jpeg;

type
  TFrmBoundsManager = class(TForm)
    Image1: TImage;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Label1: TLabel;
    Ok: TButton;
    Button1: TButton;
    Label3: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label2: TLabel;
    XOffset: TEdit;
    YOffset: TEdit;
    ZOffset: TEdit;
    Label4: TLabel;
    SizeInGame: TComboBox;
    ManualDiv: TEdit;
    Label7: TLabel;
    Label8: TLabel;
    procedure FormShow(Sender: TObject);
    procedure OkClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure SizeInGameChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    O : boolean;
  end;

var
  FrmBoundsManager: TFrmBoundsManager;

implementation

{$R *.dfm}

procedure TFrmBoundsManager.FormShow(Sender: TObject);
begin
SizeInGame.ItemIndex := 0;
O := false;
end;

procedure TFrmBoundsManager.OkClick(Sender: TObject);
begin
O := true;
close;
end;

procedure TFrmBoundsManager.Button1Click(Sender: TObject);
begin
close;
end;

procedure TFrmBoundsManager.SizeInGameChange(Sender: TObject);
begin
if SizeInGame.ItemIndex = 4 then
ManualDiv.Enabled := true
else
ManualDiv.Enabled := false;

Label7.Enabled := ManualDiv.Enabled;
end;

end.
