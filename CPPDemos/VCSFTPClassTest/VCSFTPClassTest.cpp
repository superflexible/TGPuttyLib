// VCSFTPClassTest.cpp : This file contains the 'main' function. Program execution begins and ends there.
//

#include <iostream>
#include "tgputtylibvcclass.h"

class TClassDemo
{
	TTGPuttySFTP* PSFTP;
public:
	bool DoVerifyHostKey(const char* host, const int port,
		const char* fingerprint, const int verificationstatus,
		bool& storehostkey);
	void DoMessage(const char* Msg, const bool isstderr);
	bool DoProgress(const __int64 bytescopied, const bool isupload);
	bool DoListing(const struct fxp_names* names);
	bool DoGetInput(char* linebuf, const int maxchars);
	void InitSFTP();
	void Run();
};


bool TClassDemo::DoVerifyHostKey(const char* host, const int port,
	const char* fingerprint, const int verificationstatus,
	bool& storehostkey)
{
	printf("Blindly accepting SSH fingerprint %s for %s\n", fingerprint, host);
	return true;
}

void TClassDemo::DoMessage(const char* Msg, const bool isstderr)
{
	printf("%s",Msg);
}

bool TClassDemo::DoProgress(const __int64 bytescopied, const bool isupload)
{
	printf("Progress: %I64u\n", bytescopied);
	return true;
}

bool TClassDemo::DoListing(const struct fxp_names* names)
{
	for (int i = 0; i < names->nnames; i++)
	{
		printf("%40s  %10I64u\n",names->names[i].filename,names->names[i].attrs.size);
	}
	return true;
}

bool TClassDemo::DoGetInput(char* linebuf, const int maxchars)
{
	printf("Please provide input: ");
	fgets(linebuf, maxchars, stdin);
	return true;
}

void TClassDemo::InitSFTP()
{
	PSFTP = new TTGPuttySFTP(true);

	printf("Using %s\n", PSFTP->GetLibVersion());

	printf("Enter hostname (URL/IP):\n");
	std::string host;
	std::getline(std::cin, host);

	printf("Enter username:\n");
	std::string user;
	std::getline(std::cin, user);

	PSFTP->HostName = host;
	PSFTP->UserName = user;
	PSFTP->Port = 22;

	printf("Enter password for %s:\n", PSFTP->HostName.c_str());

	std::string pw;
	std::getline(std::cin, pw);

	PSFTP->Password = pw;

	PSFTP->OnVerifyHostKey = [this](const char* host, const int port, const char* fingerprint, const int verificationstatus, bool& storehostkey)->bool
                                 { return this->DoVerifyHostKey(host, port, fingerprint, verificationstatus, storehostkey); };

	PSFTP->OnMessage = [this](const char* Msg, const bool isstderr) { this->DoMessage(Msg, isstderr); };

	PSFTP->OnProgress = [this](const __int64 bytescopied, const bool isupload)->bool { return this->DoProgress(bytescopied, isupload); };
	PSFTP->OnListing = [this](const struct fxp_names* names)->bool { return this->DoListing(names); };
	PSFTP->OnGetInput = [this](char* linebuf, const int maxchars)->bool { return this->DoGetInput(linebuf,maxchars); };
}

void TClassDemo::Run()
{
	InitSFTP();

	PSFTP->Connect();

	printf("Home Directory: %s\n", PSFTP->HomeDir);
	printf("Current Directory: %s\n", PSFTP->WorkDir);

	PSFTP->ChangeDir("/share/CACHEDEV1_DATA/Public/Tests");
	printf("Current Directory Now: %s\n", PSFTP->WorkDir);

	PSFTP->UploadFile("C:\\Tests\\Test.txt", "Test.txt", false);

	printf("File Upload Successful.\n");

	printf("Listing:\n");
	PSFTP->ListDir(".");

	delete PSFTP;
}

int main()
{
    std::cout << "Welcome to the Visual C++ SFTP Class Demo!\n";

	TClassDemo* CD = new TClassDemo;

	try
	{
		CD->Run();
	}
	catch (const std::exception & e)
	{
		printf("Exception: %s\n", e.what());
	}


	printf("Press ENTER to Continue\n");
	char dummy=getchar();
	return 0;
}

// Run program: Ctrl + F5 or Debug > Start Without Debugging menu
// Debug program: F5 or Debug > Start Debugging menu

// Tips for Getting Started: 
//   1. Use the Solution Explorer window to add/manage files
//   2. Use the Team Explorer window to connect to source control
//   3. Use the Output window to see build output and other messages
//   4. Use the Error List window to view errors
//   5. Go to Project > Add New Item to create new code files, or Project > Add Existing Item to add existing code files to the project
//   6. In the future, to open this project again, go to File > Open > Project and select the .sln file
