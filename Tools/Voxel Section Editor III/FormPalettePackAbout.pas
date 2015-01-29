unit FormPalettePackAbout;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, jpeg, ShellAPI;

type
  TFrmPalettePackAbout = class(TForm)
    Image1: TImage;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Image2: TImage;
    Label6: TLabel;
    Ok: TButton;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    Label1: TLabel;
    Label9: TLabel;
    procedure OkClick(Sender: TObject);
    procedure Label2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TFrmPalettePackAbout.OkClick(Sender: TObject);
begin
   close;
end;

procedure TFrmPalettePackAbout.Label2Click(Sender: TObject);
begin
   ShellExecute(Application.Handle,nil,'http://yrarg.cncguild.net/','','',SW_SHOWNORMAL);
end;

end.
