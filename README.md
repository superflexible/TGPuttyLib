# TGPuttyLib
A dynamic link library with Delphi units and C++ bindings based on PuTTY

The new TGPuttyLib SFTP Library is a DLL conversion of the psftp program from the well-known PuTTY suite by Simon Tatham.

It allows developers to transfer files with the highest possible transfer rates. Some buffer sizes have been increased to achieve this.

TGPuttyLib Build 3 is based on PuTTY Release 0.73. Ready-to-use classes are currently available for Delphi and Free Pascal.

C++ bindings are provided in the files tgputtylib.cpp and tgputtylib.hpp. There's a small C++ demo in CPPDemos/Simple. So far the C++ bindings are only basic DLL imports, not classes or even a component like the Delphi files. But as you can see in the demo, you can absolutely work with this. A C++ class and a native C++Builder component will be added later.

The library is currently available for Windows. MacOS and Linux adaptations will follow around the end of 2019.

For more information, please visit the project web site: 
https://www.syncovery.com/tgputtylib

---------------------
DIRECTORIES AND FILES
---------------------

tgputtylib.pas         -   Delphi Unit interfacing with the DLL

tgputtysftp.pas        -   Delphi SFTP Client Class (using 8-bit strings)

tgputtysftpclient.pas  -   Delphi SFTP Client Component (using UnicodeStrings)

---------------------

TGPuttyLib.dpk etc.    -   Delphi Package for SFTP Client Component

---------------------

DelphiSimpleDemos      -   Delphi Command Line Demos

DelphiVCLComponentDemo -   Fully working SFTP client application using the SFTP Client Component

DelphiVCLDemo          -   Fully working SFTP client application using the SFTP Client Class

FPCSimpleDemos         -   FPC Command Line Demos

---------------------

tgputtylib             -   DLL source code based on PuTTY

---------------------

bpl                    -   compiled Delphi Package for XE 10.3

Win32, Win64           -   compiled binaries
