unit FormVxlError;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, Voxel_Engine;

type
  TFrmVxlError = class(TForm)
    Bevel3: TBevel;
    Panel3: TPanel;
    Image1: TImage;
    Label5: TLabel;
    Label6: TLabel;
    Bevel4: TBevel;
    Panel4: TPanel;
    Button7: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Label1: TLabel;
    InfoBox1: TRichEdit;
    Button1: TButton;
    TabSheet2: TTabSheet;
    Label2: TLabel;
    RichEdit1: TRichEdit;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmVxlError: TFrmVxlError;

implementation

{$R *.dfm}

procedure TFrmVxlError.Button1Click(Sender: TObject);
begin
 SetVoxelFileDefults;
 VXLChanged := true;
 Button1.Enabled := False;
end;

procedure TFrmVxlError.Button7Click(Sender: TObject);
begin
Close;
end;

procedure TFrmVxlError.Button2Click(Sender: TObject);
var
N : integer;
begin

N := VoxelFile.Section[0].Tailer.Unknown;
SetNormals(N);

MessageBox(0,Pchar('Normals Set To: ' + inttostr(N)),'Information',0);

VXLChanged := true;
Button2.Enabled := False;
end;

procedure TFrmVxlError.FormCreate(Sender: TObject);
begin
TabSheet1.TabVisible := false;
TabSheet2.TabVisible := false;
Panel3.DoubleBuffered := true;
end;

end.
