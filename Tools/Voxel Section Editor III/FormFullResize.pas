unit FormFullResize;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, math;

type
  TFrmFullResize = class(TForm)
    grpNewSize: TGroupBox;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    txtNewX: TEdit;
    txtNewY: TEdit;
    txtNewZ: TEdit;
    grpCurrentSize: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    Label7: TLabel;
    txtCurrentX: TEdit;
    txtCurrentY: TEdit;
    txtCurrentZ: TEdit;
    UD: TUpDown;
    Label1: TLabel;
    Bevel1: TBevel;
    Panel1: TPanel;
    Image1: TImage;
    Label9: TLabel;
    Label10: TLabel;
    Bevel2: TBevel;
    Panel2: TPanel;
    Button4: TButton;
    Button2: TButton;
    txtScale: TEdit;
    procedure UDClick(Sender: TObject; Button: TUDBtnType);
    procedure FormShow(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Scale,
    x, y, z: integer; //size of voxel section currently
    IllegalVal,Changed: Boolean;
  end;

var
  FrmFullResize: TFrmFullResize;

implementation

{$R *.DFM}

procedure TFrmFullResize.UDClick(Sender: TObject; Button: TUDBtnType);
var
  a: Integer;
  b,c,d: Integer;
begin
  a:=UD.Position;
  Scale:=a;
  txtScale.Text:=IntToStr(a);
  b:=Max(Min(Round(X*a),256),1);
  c:=Max(Min(Round(Y*a),256),1);
  d:=Max(Min(Round(Z*a),256),1);
  txtNewX.Text:=IntToStr(b);
  txtNewY.Text:=IntToStr(c);
  txtNewZ.Text:=IntToStr(d);
  IllegalVal:=False;
  if b>255 then begin txtNewX.Text:='Err'; IllegalVal:=True; end;
  if c>255 then begin txtNewY.Text:='Err'; IllegalVal:=True; end;
  if d>255 then begin txtNewZ.Text:='Err'; IllegalVal:=True; end;
end;

procedure TFrmFullResize.FormShow(Sender: TObject);
begin
  txtCurrentX.Text := IntToStr(x); txtNewX.Text := txtCurrentX.Text;
  txtCurrentY.Text := IntToStr(y); txtNewY.Text := txtCurrentY.Text;
  txtCurrentZ.Text := IntToStr(z); txtNewZ.Text := txtCurrentZ.Text;
  Scale:=UD.Position;
  UDClick(Self,btNext);
end;

procedure TFrmFullResize.Button4Click(Sender: TObject);
begin
if IllegalVal then begin
    MessageDlg('All sizes must be an integer number between 1 and 255',mtError,[mbOK],0);
    Exit;
  end;
  Changed:=True;
  Scale:=UD.Position;
  x:=Max(Min(Round(X*Scale),256),1);
  y:=Max(Min(Round(Y*Scale),256),1);
  z:=Max(Min(Round(Z*Scale),256),1);
  Close;
end;

procedure TFrmFullResize.Button2Click(Sender: TObject);
begin
Changed:=False;
Close;
end;

procedure TFrmFullResize.FormCreate(Sender: TObject);
begin
Panel1.DoubleBuffered := true;
end;



end.
