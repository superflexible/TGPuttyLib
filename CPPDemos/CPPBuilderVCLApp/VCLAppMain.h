//---------------------------------------------------------------------------

#ifndef VCLAppMainH
#define VCLAppMainH
//---------------------------------------------------------------------------
#include <System.Classes.hpp>
#include <Vcl.Controls.hpp>
#include <Vcl.StdCtrls.hpp>
#include <Vcl.Forms.hpp>

#include "tgputtycbsftpclient.h"
#include <Vcl.ComCtrls.hpp>
#include <Vcl.FileCtrl.hpp>
#include <Vcl.Grids.hpp>

//---------------------------------------------------------------------------
class TFVCLAppMain : public TForm
{
__published:	// IDE-managed Components
    TTGPuttyCBSFTPClient *TGPuttySFTPClient1;
    TLabel *Label1;
    TLabel *Label2;
    TLabel *Label3;
    TLabel *Label4;
    TLabel *Label5;
    TLabel *Label6;
    TLabel *Label7;
    TLabel *Label8;
    TLabel *Label9;
    TDirectoryListBox *DirectoryListBox1;
    TDriveComboBox *DriveComboBox1;
    TFileListBox *FileListBox1;
    TEdit *edURL;
    TEdit *edPort;
    TEdit *edUserName;
    TEdit *edPassword;
    TEdit *edFolderPath;
    TStringGrid *sgRemoteFiles;
    TButton *btConnect;
    TButton *btDisconnect;
    TCheckBox *cbVerbose;
    TMemo *memLog;
    TButton *btUpload;
    TButton *btDownload;
    TCheckBox *cbSavePassword;
    TProgressBar *ProgressBar1;
    TButton *btDeleteLocal;
    TButton *btDeleteRemote;
    TButton *btMkDir;
    TButton *btRemoveDir;
    TButton *btMove;
    void __fastcall btConnectClick(TObject *Sender);
    void __fastcall TGPuttySFTPClient1SFTPMessage(TObject *Sender, const UnicodeString msg,const bool isstderr);
    void __fastcall FormShow(TObject *Sender);
    void __fastcall btDisconnectClick(TObject *Sender);
    bool __fastcall TGPuttySFTPClient1SFTPListing(TObject *Sender, TSFTPItems *Items);
    void __fastcall DirectoryListBox1Change(TObject *Sender);
    void __fastcall sgRemoteFilesDblClick(TObject *Sender);
    void __fastcall btUploadClick(TObject *Sender);
    void __fastcall btDownloadClick(TObject *Sender);
    void __fastcall btDeleteLocalClick(TObject *Sender);
    void __fastcall btDeleteRemoteClick(TObject *Sender);
    void __fastcall btMkDirClick(TObject *Sender);
    void __fastcall btRemoveDirClick(TObject *Sender);
    void __fastcall btMoveClick(TObject *Sender);
    void __fastcall cbVerboseClick(TObject *Sender);
    void __fastcall edFolderPathExit(TObject *Sender);
    bool __fastcall TGPuttySFTPClient1SFTPProgress(TObject *Sender, TSFTPProgressInfo *ProgressInfo);
    bool __fastcall TGPuttySFTPClient1SFTPVerifyHostKey(System::TObject* Sender, const System::UnicodeString host,
                           const int port, const System::UnicodeString fingerprint,
                           const int verificationstatus, bool &storehostkey);
    bool __fastcall TGPuttySFTPClient1SFTPGetInput(TObject *Sender, UnicodeString &AnInput);

private:
    __int64 FTotalToCopy;
    bool FInLoadSettings;

public:
    __fastcall TFVCLAppMain(TComponent* Owner);

    void SaveSettings();
    void LoadSettings();
    void GetListing();
};
//---------------------------------------------------------------------------
extern PACKAGE TFVCLAppMain *FVCLAppMain;
//---------------------------------------------------------------------------
#endif
