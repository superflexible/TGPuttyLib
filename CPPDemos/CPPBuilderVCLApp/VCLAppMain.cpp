//---------------------------------------------------------------------------

#include <vcl.h>
#include <System.Win.Registry.hpp>
#include <System.DateUtils.hpp>
#pragma hdrstop

#include "VCLAppMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TFVCLAppMain *FVCLAppMain;
//---------------------------------------------------------------------------
__fastcall TFVCLAppMain::TFVCLAppMain(TComponent* Owner)
    : TForm(Owner)
{
}
//---------------------------------------------------------------------------


void TFVCLAppMain::SaveSettings()
{
  TRegistry *Reg = new TRegistry;
  try
  {
     Reg->RootKey=HKEY_CURRENT_USER;
     if (Reg->OpenKey("SOFWARE\tgputty",true))
     {
         Reg->WriteString("URL",edURL->Text);
         Reg->WriteString("UserName",edUserName->Text);
         if (cbSavePassword->Checked)
            Reg->WriteString("Password",edPassword->Text);
         else
            Reg->DeleteValue("Password");
         Reg->WriteInteger("Port",StrToIntDef(edPort->Text,22));
         Reg->WriteString("FolderPath",edFolderPath->Text);
         Reg->WriteString("LocalPath",DirectoryListBox1->Directory);
      }
  }
  __finally
  {
     delete Reg;
  }
}

void TFVCLAppMain::LoadSettings()
{
  FInLoadSettings = true;
  TRegistry *Reg = new TRegistry;
  try
  {
     Reg->RootKey=HKEY_CURRENT_USER;
     if (Reg->OpenKey("SOFWARE\tgputty",true))
     {
         edURL->Text=Reg->ReadString("URL");
         edUserName->Text=Reg->ReadString("UserName");
         edPassword->Text=Reg->ReadString("Password");
         cbSavePassword->Checked=edPassword->Text!="";
         try
         {
           edPort->Text=IntToStr(Reg->ReadInteger("Port"));
         }
         catch(...)
         {
             edPort->Text="22";
         }
         edFolderPath->Text=Reg->ReadString("FolderPath");
         DirectoryListBox1->Directory=Reg->ReadString("LocalPath");
     }
  }
  __finally
  {
     delete Reg;
     FInLoadSettings=false;
  }
}

void TFVCLAppMain::GetListing()
{
  sgRemoteFiles->RowCount=1;
  sgRemoteFiles->ColCount=3;
  sgRemoteFiles->ColWidths[0]=480;
  sgRemoteFiles->ColWidths[1]=300;
  sgRemoteFiles->ColWidths[2]=150;
  sgRemoteFiles->Cells[0][0]="Name";
  sgRemoteFiles->Cells[1][0]="Timestamp";
  sgRemoteFiles->Cells[2][0]="Size";
  TGPuttySFTPClient1->ListDir("");
  if (sgRemoteFiles->RowCount>1)
     sgRemoteFiles->FixedRows=1;
  sgRemoteFiles->FixedCols=0;
}


void __fastcall TFVCLAppMain::btConnectClick(TObject *Sender)
{
  SaveSettings();
  TGPuttySFTPClient1->HostName=edURL->Text;
  TGPuttySFTPClient1->UserName=edUserName->Text;
  TGPuttySFTPClient1->Password=edPassword->Text;
  TGPuttySFTPClient1->Port=StrToIntDef(edPort->Text,22);

  TGPuttySFTPClient1->Connect();

  if (edFolderPath->Text>"")
  {
     TGPuttySFTPClient1->ChangeDir(edFolderPath->Text);

     edFolderPath->Text=TGPuttySFTPClient1->WorkDir;
  }

  GetListing();

  if (TGPuttySFTPClient1->Connected)
     memLog->Lines->Add("Connected");
  else
     memLog->Lines->Add("Connected=false?");
}
//---------------------------------------------------------------------------


void __fastcall TFVCLAppMain::TGPuttySFTPClient1SFTPMessage(TObject *Sender, const UnicodeString msg,const bool isstderr)
{
  if (memLog)
     memLog->Lines->Add(msg);
}
//---------------------------------------------------------------------------

void __fastcall TFVCLAppMain::FormShow(TObject *Sender)
{
  LoadSettings();
  ProgressBar1->Visible=false;
  memLog->Lines->Add("Library version: "+TGPuttySFTPClient1->LibVersion);
}
//---------------------------------------------------------------------------

void __fastcall TFVCLAppMain::btDisconnectClick(TObject *Sender)
{
  if (TGPuttySFTPClient1->Connected)
     memLog->Lines->Add("Connected");
  else
     memLog->Lines->Add("Connected=false?");
  TGPuttySFTPClient1->Disconnect();
  sgRemoteFiles->RowCount=0;
  sgRemoteFiles->ColCount=0;
  if (TGPuttySFTPClient1->Connected)
     memLog->Lines->Add("Connected");
  else
     memLog->Lines->Add("Connected=false?");
}
//---------------------------------------------------------------------------


bool __fastcall TFVCLAppMain::TGPuttySFTPClient1SFTPListing(TObject *Sender, TSFTPItems *Items)
{
  int StartRow,i;

  StartRow=sgRemoteFiles->RowCount;
  sgRemoteFiles->RowCount=StartRow+Items->SFTPItems.Length;
  for (int i=0;i<Items->SFTPItems.Length;i++)
  {
    sgRemoteFiles->Cells[0][StartRow+i]=Items->SFTPItems[i].filename;
    sgRemoteFiles->Cells[1][StartRow+i]=DateTimeToStr(TTimeZone::Local->ToLocalTime(UnixToDateTime(Items->SFTPItems[i].attrs.mtime)));
    if ((Items->SFTPItems[i].attrs.permissions & 0xF000) == 0x4000)
       sgRemoteFiles->Cells[2][StartRow+i]="<dir>";
    else
       sgRemoteFiles->Cells[2][StartRow+i]=IntToStr((__int64)Items->SFTPItems[i].attrs.size);
  }
  return true;
}
//---------------------------------------------------------------------------


void __fastcall TFVCLAppMain::DirectoryListBox1Change(TObject *Sender)
{
  if (!FInLoadSettings)
     SaveSettings();
}
//---------------------------------------------------------------------------

void __fastcall TFVCLAppMain::sgRemoteFilesDblClick(TObject *Sender)
{
  if (sgRemoteFiles->Selection.Top==sgRemoteFiles->Selection.Bottom)
  {
     if (sgRemoteFiles->Cells[2][sgRemoteFiles->Selection.Top]==UnicodeString(L"<dir>"))
     {
        TGPuttySFTPClient1->ChangeDir(sgRemoteFiles->Cells[0][sgRemoteFiles->Selection.Top]);
        edFolderPath->Text=TGPuttySFTPClient1->WorkDir;
        SaveSettings();
        GetListing();
     }
  }
}
//---------------------------------------------------------------------------

void __fastcall TFVCLAppMain::btUploadClick(TObject *Sender)
{
  if (TGPuttySFTPClient1->Connected)
     memLog->Lines->Add("Connected");
  else
     memLog->Lines->Add("Connected=false?");

  TDateTimeInfoRec LDateTime;

  for (int i=0;i<FileListBox1->Count;i++)
  {
    if (FileListBox1->Selected[i])
    {
       UnicodeString APath = DirectoryListBox1->Directory+
                             PathDelim+
                             FileListBox1->Items->Strings[i];
       FileGetDateTimeInfo(APath,LDateTime);
       TStream *UploadStream=new TFileStream(APath,fmOpenRead);
       try
       {
         FTotalToCopy=UploadStream->Size;
         ProgressBar1->Min=0;
         ProgressBar1->Max=FTotalToCopy / 1024;
         ProgressBar1->Position=0;
         ProgressBar1->Visible=true;
         Application->ProcessMessages();
         TGPuttySFTPClient1->UploadStream(FileListBox1->Items->Strings[i],UploadStream,false);
         TGPuttySFTPClient1->SetModifiedDate(FileListBox1->Items->Strings[i],LDateTime.TimeStamp,false);
         GetListing();
       }
       __finally
       {
         delete UploadStream;
         ProgressBar1->Visible=false;
       }
     }
  }
  if (TGPuttySFTPClient1->Connected)
     memLog->Lines->Add("Connected");
  else
     memLog->Lines->Add("Connected=false?");
}
//---------------------------------------------------------------------------

void __fastcall TFVCLAppMain::btDownloadClick(TObject *Sender)
{
  for (int i=sgRemoteFiles->Selection.Top;i<=sgRemoteFiles->Selection.Bottom;i++)
  {
     if (sgRemoteFiles->Cells[2][i]!=UnicodeString(L"<dir>"))
     {
        UnicodeString APath=DirectoryListBox1->Directory+PathDelim+sgRemoteFiles->Cells[0][i];
        TStream *DownloadStream=new TFileStream(APath,fmCreate);
        try
        {
          FTotalToCopy=StrToInt64Def(sgRemoteFiles->Cells[2][i],0);
          ProgressBar1->Min=0;
          ProgressBar1->Max=FTotalToCopy / 1024;
          ProgressBar1->Position=0;
          ProgressBar1->Visible=true;
          Application->ProcessMessages();
          TGPuttySFTPClient1->DownloadStream(sgRemoteFiles->Cells[0][i],
                               DownloadStream,
                               false);
          FileListBox1->Update();
        }
        __finally
        {
            ProgressBar1->Visible=false;
            delete DownloadStream;
        }
     }
  }
}
//---------------------------------------------------------------------------

void __fastcall TFVCLAppMain::btDeleteLocalClick(TObject *Sender)
{
  int count=0;
  for (int i=0;i<FileListBox1->Count;i++)
    if (FileListBox1->Selected[i])
       count++;

  if (count==0)
     return;

  if (Application->MessageBox((L"Please confirm deleting "+IntToStr(count)+L" file locally (left side)").c_str(),
                            L"Confirm Deletion",
                            MB_YESNO | MB_ICONQUESTION)==IDYES)
  {
     for (int i=0;i<FileListBox1->Count;i++)
       if (FileListBox1->Selected[i])
       {
          UnicodeString APath=DirectoryListBox1->Directory+PathDelim+FileListBox1->Items->Strings[i];
          DeleteFile(APath);
       }
     FileListBox1->Update();
  }
}
//---------------------------------------------------------------------------

void __fastcall TFVCLAppMain::btDeleteRemoteClick(TObject *Sender)
{
  int count=sgRemoteFiles->Selection.Bottom-sgRemoteFiles->Selection.Top+1;
  if (count==0)
     return;

  if (Application->MessageBox((L"Please confirm deleting "+IntToStr(count)+L" file(s) remotely (right side)").c_str(),
                            L"Confirm Deletion",
                            MB_YESNO | MB_ICONQUESTION)==IDYES)
  {
     for (int i=sgRemoteFiles->Selection.Top;i<=sgRemoteFiles->Selection.Bottom;i++)
       if (sgRemoteFiles->Cells[2][i]!=UnicodeString(L"<dir>"))
          TGPuttySFTPClient1->Delete_File(sgRemoteFiles->Cells[0][i]);
  }
  GetListing();
}
//---------------------------------------------------------------------------

void __fastcall TFVCLAppMain::btMkDirClick(TObject *Sender)
{
  UnicodeString AName=InputBox(L"Make Directory",L"Enter new Directory Name:",L"");
  if (AName.Length()>0)
  {
     TGPuttySFTPClient1->MakeDir(AName);
     GetListing();
  }
}
//---------------------------------------------------------------------------

void __fastcall TFVCLAppMain::btRemoveDirClick(TObject *Sender)
{
  int count=sgRemoteFiles->Selection.Bottom-sgRemoteFiles->Selection.Top+1;
  if (count==0)
     return;

  if (Application->MessageBox((L"Please confirm deleting "+IntToStr(count)+L" directory/ies remotely (right side)").c_str(),
                               L"Confirm Deletion",
                               MB_YESNO | MB_ICONQUESTION)==IDYES)
  {
     for (int i=sgRemoteFiles->Selection.Top;i<=sgRemoteFiles->Selection.Bottom;i++)
       if (sgRemoteFiles->Cells[2][i]==UnicodeString(L"<dir>"))
          TGPuttySFTPClient1->RemoveDir(sgRemoteFiles->Cells[0][i]);
  }
  GetListing();
}
//---------------------------------------------------------------------------

void __fastcall TFVCLAppMain::btMoveClick(TObject *Sender)
{
  UnicodeString AName = InputBox(L"Rename / Move",L"Enter new name and/or destination directory:",L"");

  if (AName.Length()==0)
     return;

  int count=sgRemoteFiles->Selection.Bottom-sgRemoteFiles->Selection.Top+1;
  if (count==0)
     return;

  if (Application->MessageBox((L"Please confirm moving "+IntToStr(count)+L" file(s) remotely (right side)").c_str(),
                            L"Confirm Moving",
                            MB_YESNO | MB_ICONQUESTION)==IDYES)
  {
     for (int i=sgRemoteFiles->Selection.Top;i<=sgRemoteFiles->Selection.Bottom;i++)
       if (sgRemoteFiles->Cells[2][i]!=UnicodeString(L"<dir>"))
          TGPuttySFTPClient1->Move(sgRemoteFiles->Cells[0][i],AName);
  }
  GetListing();
}
//---------------------------------------------------------------------------

void __fastcall TFVCLAppMain::cbVerboseClick(TObject *Sender)
{
  TGPuttySFTPClient1->Verbose = cbVerbose->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TFVCLAppMain::edFolderPathExit(TObject *Sender)
{
  if (TGPuttySFTPClient1->Connected)
  {
     try
     {
       if (edFolderPath->Text.Length()>0)
          TGPuttySFTPClient1->ChangeDir(edFolderPath->Text);

       edFolderPath->Text=TGPuttySFTPClient1->WorkDir;
       GetListing();
     }
     catch(...)
     {
       Application->MessageBox(L"Error",L"Error");
     }
  }
}
//---------------------------------------------------------------------------


bool __fastcall TFVCLAppMain::TGPuttySFTPClient1SFTPProgress(TObject *Sender, TSFTPProgressInfo *ProgressInfo)

{
  ProgressBar1->Position=ProgressInfo->bytescopied / 1024;
  Application->ProcessMessages();
  return true;
}
//---------------------------------------------------------------------------

bool __fastcall TFVCLAppMain::TGPuttySFTPClient1SFTPVerifyHostKey(System::TObject* Sender,
                      const System::UnicodeString host, const int port,
                      const System::UnicodeString fingerprint,
                      const int verificationstatus, bool &storehostkey)
{
  if (verificationstatus==0)
     return true;

  bool res=Application->MessageBox(
                (L"Please confirm the SSH host key fingerprint for "+host+
                L", port "+IntToStr(port)+L":"+sLineBreak+
                fingerprint).c_str(),
                L"Server Verification",
                MB_YESNO | MB_ICONQUESTION) == IDYES;
  storehostkey=res;
  return res;
}
//---------------------------------------------------------------------------


bool __fastcall TFVCLAppMain::TGPuttySFTPClient1SFTPGetInput(TObject *Sender, UnicodeString &AnInput)

{
  AnInput = L"";
  memLog->Lines->Add(L"Replying with empty line.");
  return true;
}
//---------------------------------------------------------------------------

