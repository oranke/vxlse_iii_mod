unit FormImportSection;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TFrmImportSection = class(TForm)
    Label1: TLabel;
    ComboBox1: TComboBox;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmImportSection: TFrmImportSection;

implementation

{$R *.dfm}

procedure TFrmImportSection.Button1Click(Sender: TObject);
begin
close;
end;

end.
