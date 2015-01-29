unit LoadForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons, GIFImage, jpeg;

{$INCLUDE Global_Conditionals.inc}

type
  TLoadFrm = class(TForm)
    Image1: TImage;
    Label4: TLabel;
    lblWill: TLabel;
    loading: TLabel;
    butOK: TSpeedButton;
    Bevel1: TBevel;
    Image2: TImage;
    procedure OKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  LoadFrm: TLoadFrm;

implementation

{$R *.DFM}

procedure TLoadFrm.OKClick(Sender: TObject);
//var l: integer;
begin
  Close;
end;

procedure TLoadFrm.FormCreate(Sender: TObject);
begin
//loading.Left := (Width div 2) - (loading.Width div 2);
end;

end.
