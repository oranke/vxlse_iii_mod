unit Debug;

interface

uses
   Dialogs, SysUtils; // VK for EInOutError & MessageDlg

type
   TDebugFile = class
        FileName : string;
      private
      public
         constructor Create(const _FileName: String);
         procedure Add(const _Content:string);
   end;



implementation

   constructor TDebugFile.Create(const _FileName: String);
   var
      MyFile : System.Text;
   begin
      Filename := _Filename;
      try
         AssignFile(MyFile,Filename);
         FileMode := fmOpenWrite;
         Rewrite(MyFile);
         CloseFile(MyFile);
      except on E : EInOutError do // VK 1.36 U
         MessageDlg('Error: ' + E.Message + Char($0A) + Filename, mtError, [mbOK], 0);
      end;
   end;

   procedure TDebugFile.Add(const _Content:string);
   var
      MyFile : System.Text;
   begin
      try
         AssignFile(MyFile,Filename);
         FileMode := fmOpenWrite;
         Append(MyFile);
         Writeln(MyFile,_Content);
         CloseFile(MyFile);
      except on E : EInOutError do // VK 1.36 U
         MessageDlg('Error: ' + E.Message + Char($0A) + Filename, mtError, [mbOK], 0);
      end;
   end;

end.
