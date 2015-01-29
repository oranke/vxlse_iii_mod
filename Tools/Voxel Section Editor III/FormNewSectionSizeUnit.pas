unit FormNewSectionSizeUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TFrmNewSectionSize = class(TForm)
    lblCaption: TLabel;
    txtX: TEdit;
    lblX: TLabel;
    lblY: TLabel;
    txtY: TEdit;
    lblZ: TLabel;
    txtZ: TEdit;
    lblName: TLabel;
    txtName: TEdit;
    lblPosition: TLabel;
    chkBefore: TRadioButton;
    chkAfter: TRadioButton;
    Bevel1: TBevel;
    Button1: TButton;
    Button2: TButton;
    procedure FormActivate(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    before,
    aborted: boolean;
    X, Y, Z: integer;
    Name: array[1..16] of Char;
  end;

var
  FrmNewSectionSize: TFrmNewSectionSize;

implementation

{$R *.DFM}

procedure TFrmNewSectionSize.FormActivate(Sender: TObject);
begin
     aborted := true;
end;

procedure TFrmNewSectionSize.btnCancelClick(Sender: TObject);
begin
     aborted := true;
     Close;
end;

procedure TFrmNewSectionSize.btnOKClick(Sender: TObject);
function CheckName: boolean;
var ch: char;
    i: integer;
begin
     Result := (Length(txtName.Text) in [1..16]);
     if not Result then begin
        MessageDlg('Name must be between 1 and 16 characters long!',mtError,[mbOK],0);
        Exit;
     end;
     txtName.Text := UpperCase(txtName.Text);
     for i := 1 to Length(txtName.Text) do begin
         ch := txtName.Text[i];
         if not (ch in ['A'..'Z','0'..'9']) then begin
            Result := False;
            MessageDlg('Name can only contain letters and digits!',mtError,[mbOK],0);
            txtName.SetFocus;
            Exit;
         end;
         Name[i] := ch;
     end;
     //Code changed to get rid of compiler warning. This is better anyway.
     for i := Length(txtName.Text)+1 to 16 do begin
        Name[i] := #0; // zero-terminated
     end;
end;
var code: integer;
procedure ValError(v: string; Ctrl: TEdit);
begin
     MessageDlg(v + ' must be an integer number between 1 and 255',
                  mtError,[mbOK],0);
     Ctrl.SetFocus;
end;
begin
     // Name
     if not CheckName then
        Exit;
     // X
     Val(txtX.Text,X,code);
     if (code <> 0) or not (X in [1..255]) then begin
        ValError('x',txtX);
        Exit;
     end;
     // Y
     Val(txtY.Text,Y,code);
     if (code <> 0) or not (Y in [1..255]) then begin
        ValError('y',txtY);
        Exit;
     end;
     // Z
     Val(txtZ.Text,Z,code);
     if (code <> 0) or not (Z in [1..255]) then begin
        ValError('z',txtZ);
        Exit;
     end;
     before := chkBefore.Checked;
     aborted := false;
     Close;
end;

end.
