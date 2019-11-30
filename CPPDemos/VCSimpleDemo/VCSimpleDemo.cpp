// VCSimpleDemo.cpp : This file contains the 'main' function. Program execution begins and ends there.
//

#include <iostream>
#include "../../tgputtylib.hpp"

int main()
{
    std::cout << "Welcome to the Visual Studio C++ Simple Demo!\n";
	if (LoadTGPuttyLib())
		std::cout << "Success loading tgputtylib!\n";
	else
	{
		std::cout << "Failure loading tgputtylib!\n";
		getchar();
		return 1;
	}

	TTGLibraryContext Ctx = { sizeof(TTGLibraryContext) };

	int res = tgputty_initcontext(true, &Ctx);
	if (res != 0)
	{
		std::cout << "Failure initializing context!\n";
		getchar();
		return 1;
	}

	res = tgsftp_connect("192.168.12.45", "admin", 22, "password", &Ctx);
	if (res == 0)
		std::cout << "Connected successfully!\n";
	else
	{
		std::cout << "Failure connecting!\n";
		getchar();
		return 1;
	}

	res = tgsftp_cd("/share/CACHEDEV1_DATA/Public/Tests", &Ctx);
	if (res == 1)
		std::cout << "cd successful.\n";
	else
		std::cout << "cd failed.\n";

	res = tgsftp_putfile("C:\\Tests\\Test.txt", "Test.txt", false, &Ctx);
	if (res == 1)
		std::cout << "putfile successful.\n";
	else
		std::cout << "putfile failed.\n";


	tgsftp_close(&Ctx);
	std::cout << "Disconnected successfully!\n";

	std::cout << "Press Enter to Continue\n";
	getchar();
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
