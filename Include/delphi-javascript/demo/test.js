/* BE WARNED JAVASCRIPT IS CASE SENSITIVE */
const
   fmOpenRead = 0,
   fmCreate   = 0xFF00,

   fmOpenWrite = 1,
   fmOpenReadWrite =2,
   fmExclusive = 4;

const fileName = 'd:\\temp\\n.txt';

function OnExit(Sender)
{
  ShowMessage('OnExit');
  Sender.Text  = 'OnExit fired...';
}

function FormMouseUp(Sender, Button, Shift, X, Y)
{
  Sender.Memo1.Lines.Add('FormMouseUp: '+Button);
}

function main()
{


   // Sample bellow will work with Delphi XE2 or later
   // For been able to access Indexed properties from JS you need Delphi XE2
   // or later cause this version implement index properties access with RTTI
   //var c = MainForm.Controls[0].Name;

    /*var z = TStreamWriter ("d:\\Temp\\test9.txt", True);
    z.WriteLine ("abcdef100500!");
    z.Free ();*/

	ad = new Date(MainForm.TestDateTime);

   MainForm.TestDateTime = ad;

   ShowMessage('Test datetime '+ad);
   if (FileExists(fileName))
   {
      var file = new TFileStream(fileName, fmOpenRead);
      delete file;

   }

   MainForm.Edit1.Unknown = 1;
   MainForm.Edit1.Text = 'Sample text';

   MainForm.OnMouseUp = FormMouseUp;
   MainForm.OnMouseUp = null;

   // OnExit event
   // Events supported is all TNotify events and keyevents  and mousevents
   MainForm.Edit1.OnExit = OnExit;
   MainForm.Caption = 'Dummy cvapto ';

   // Add some controls
   var newLabel = new TLabel(MainForm);
   newLabel.Parent = MainForm;
   newLabel.Caption = 'Test Caption';
   newLabel.Visible = true;
   newLabel.Left = 10;
   newLabel.Top = 10;

   var newEdit = new TEdit(MainForm);
   newEdit.Parent = MainForm;
   newEdit.Visible = true;
   newEdit.Left = 10;
   newEdit.Top = 30;

   App.testCall();
//   ShowMessage('Test message');

}

