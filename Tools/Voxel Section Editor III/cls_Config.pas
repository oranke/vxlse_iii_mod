unit cls_Config;

// TConfiguration class
// ====================
//
// Created by Koen van de Sande
//
// Revision 1.0 (11-06-2001)
//
// New class for Voxel editor: This class keeps track of the used file list
// adding a file is easy: just call AddFileToHistory(FileName), and retrieving
// them can be done through the GetHistory() function.
// They are saved upon Destruction of this class (call Free when closing
// application)

interface

uses Registry, SysUtils, Windows, Constants;

type
   TConfiguration = class(TObject)
      public
         Icon: Integer;
         Assoc,Palette: Boolean;
         TS,RA2 : string;
         constructor Create();
         destructor Destroy(); override;
         procedure AddFileToHistory(FileName: String);
         function GetHistory(Index: Integer): String;
         procedure SaveSettings;
      private
         History: Array[0..HistoryDepth] of String;
   end;

implementation

{ TConfiguration }

procedure TConfiguration.AddFileToHistory(FileName: String);
var
  i,j: Integer;
begin
  //Add a file to the history list :)
  for i:=HistoryDepth downto 1 do begin
    History[i]:=History[i-1];
  end;
  History[0]:=FileName;

  //now check for doubles!
  for i:=1 to HistoryDepth-1 do begin
    if CompareText(FileName,History[i])=0 then begin  //the same!!!
      for j:=i to HistoryDepth - 1 do begin
        History[j]:=History[j+1];
      end;
      History[HistoryDepth]:='';
    end;
  end;
end;

constructor TConfiguration.Create;
var
  Reg: TRegistry;
  i: Integer;
begin

  Reg:=TRegistry.Create;
  try
    //Retrieve history information
    Reg.RootKey := HKEY_CURRENT_USER;
    Reg.OpenKey(RegPath,true);

    for i:=0 to HistoryDepth - 1 do
    begin
      History[i] := Reg.ReadString('History'+IntToStr(i));
    end;
    if Reg.ValueExists('Icon') then
      Icon:=Reg.ReadInteger('Icon')
    else
      Icon:=0;

    if Reg.ValueExists('Assoc') then
      Assoc:=Reg.ReadBool('Assoc')
    else
      Assoc:=False;

    if Reg.ValueExists('Palette') then
      Palette:=Reg.ReadBool('Palette')
    else
      Palette:=False;

    if Reg.ValueExists('TS') then
      TS:=Reg.ReadString('TS')
    else
      TS:='TS';

    if Reg.ValueExists('RA2') then
      RA2:=Reg.ReadString('RA2')
    else
      RA2:='RA2';

  finally
    Reg.CloseKey;
    Reg.Free;
  end;
end;

destructor TConfiguration.Destroy;
begin
  inherited;
  SaveSettings;
end;

function TConfiguration.GetHistory(Index: Integer): String;
begin
  GetHistory:='';
  if (Index>=0) and (Index<HistoryDepth) then
    GetHistory:=History[Index];
end;

procedure TConfiguration.SaveSettings;
var
  Reg: TRegistry;
  i: Integer;
begin
  //Retrieve history information
  Reg:=TRegistry.Create;
  Reg.RootKey := HKEY_CURRENT_USER;
  Reg.OpenKey(RegPath,true);

  for i:=0 to HistoryDepth - 1 do begin
    Reg.WriteString('History'+IntToStr(i),History[i]);
  end;
  Reg.WriteInteger('Icon',Icon);
  Reg.WriteBool('Assoc',Assoc);
  Reg.WriteBool('Palette',Palette);
  Reg.WriteString('TS',TS);
  Reg.WriteString('RA2',RA2);

  Reg.CloseKey;
  Reg.Free;
end;

end.
