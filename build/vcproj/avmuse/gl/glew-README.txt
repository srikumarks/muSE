NOTE: 
You don't need to install glew into system directories any more.
All required libraries are contained in this directory itself and
MESSln links directly to these. However, the following information
from external sources could be useful if you want to place these 
DLLs in system locations for other apps.


From http://glew.sourceforge.net/install.html -
---------------------------------------------

To use the shared library version of GLEW, you need to copy the 
headers and libraries into their destination directories. 
On Windows this typically boils down to copying:

bin/glew32.dll	    to     	%SystemRoot%/system32
lib/glew32.lib	    to     	{VC Root}/Lib
include/GL/glew.h	    to     	{VC Root}/Include/GL
include/GL/wglew.h	    to     	{VC Root}/Include/GL

where {VC Root} is the Visual C++ root directory, 
typically C:/Program Files/Microsoft Visual Studio/VC98 for Visual Studio 6.0 or 
C:/Program Files/Microsoft Visual Studio .NET 2003/Vc7/PlatformSDK 
for Visual Studio .NET. 

Note by Kumar on install locations -
----------------------------------

{Program Files}\Microsoft Visual Studio 8\VC\PlatformSDK\Include\gl
    glew.h
    wglew.h
    
{Program Files}\Microsoft Visual Studio 8\VC\PlatformSDK\Lib
    glew32mx.lib
    
{Root}:\WINDOWS\system32 (or if you're running 64-bit OS - C:\WINDOWS\SysWOW64)
    glew32mx.dll
                