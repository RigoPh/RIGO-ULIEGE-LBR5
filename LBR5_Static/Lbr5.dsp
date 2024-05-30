# Microsoft Developer Studio Project File - Name="VFLBR5" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) QuickWin Application" 0x0107

CFG=VFLBR5 - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "Lbr5.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "Lbr5.mak" CFG="VFLBR5 - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "VFLBR5 - Win32 Release" (based on "Win32 (x86) QuickWin Application")
!MESSAGE "VFLBR5 - Win32 Debug" (based on "Win32 (x86) QuickWin Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "VFLBR5 - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /include:"Release/" /libs:qwin /nologo /warn:nofileopt
# ADD F90 /compile_only /debug:full /include:"Release/" /libs:qwin /nologo /warn:nofileopt
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /o "NUL" /win32
# ADD BASE RSC /l 0x80c /d "NDEBUG"
# ADD RSC /l 0x80c /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /entry:"WinMainCRTStartup" /subsystem:windows /machine:I386 /nodefaultlib:"dfconsol.lib"
# ADD LINK32 kernel32.lib /nologo /entry:"WinMainCRTStartup" /subsystem:windows /machine:I386 /nodefaultlib:"dfconsol.lib"

!ELSEIF  "$(CFG)" == "VFLBR5 - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /debug:full /include:"Debug/" /libs:qwin /nologo /warn:nofileopt
# ADD F90 /browser /compile_only /debug:full /include:"Debug/" /libs:qwin /nologo /warn:nofileopt
# ADD CPP /FR
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /o "NUL" /win32
# ADD BASE RSC /l 0x80c /d "_DEBUG"
# ADD RSC /l 0x80c /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /entry:"WinMainCRTStartup" /subsystem:windows /debug /machine:I386 /nodefaultlib:"dfconsol.lib" /pdbtype:sept
# ADD LINK32 kernel32.lib /nologo /entry:"WinMainCRTStartup" /subsystem:windows /debug /machine:I386 /nodefaultlib:"dfconsol.lib" /out:"C:\Users\Admin\Desktop\LBR5\LBR5_Static\Debug\Lbr5.exe" /pdbtype:sept

!ENDIF 

# Begin Target

# Name "VFLBR5 - Win32 Release"
# Name "VFLBR5 - Win32 Debug"
# Begin Group "SOURCES FILES"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\SRC\ABC.FOR
# End Source File
# Begin Source File

SOURCE=.\SRC\Analys.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Angle.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Bateau.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Bending.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Bendyield.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Bo1.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Bo2.for
# End Source File
# Begin Source File

SOURCE=.\SRC\BORD.FOR
# End Source File
# Begin Source File

SOURCE=.\SRC\Buckdcn.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Carac.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Carac2.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Caracd.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Coe.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Combine.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Compbuck.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Comple.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Compu.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Conlin.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Contr.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Coord.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Copy.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Costmain.for
# End Source File
# Begin Source File

SOURCE=.\SRC\DARG.FOR
# End Source File
# Begin Source File

SOURCE=.\SRC\DERIV.FOR
# End Source File
# Begin Source File

SOURCE=.\SRC\DHYDRO.FOR
# End Source File
# Begin Source File

SOURCE=.\SRC\Dhyp.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Discu.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Dloc.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Dlocy.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Dlocz.for
# End Source File
# Begin Source File

SOURCE=.\SRC\DMOM.FOR
# End Source File
# Begin Source File

SOURCE=.\SRC\Dpar.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Dpress.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Ecri.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Ecri2.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Ecri3.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Ecri4.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Ent.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Equil.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Flam.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Geom.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Graphic.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Hughes.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Hull.for
# End Source File
# Begin Source File

SOURCE=.\SRC\HYDRO.FOR
# End Source File
# Begin Source File

SOURCE=.\SRC\HYP1.FOR
# End Source File
# Begin Source File

SOURCE=.\SRC\HYP2.FOR
# End Source File
# Begin Source File

SOURCE=.\SRC\Imprtau.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Inertia.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Inteq.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Lbr5.for
# End Source File
# Begin Source File

SOURCE=.\SRC\LINDE.FOR
# End Source File
# Begin Source File

SOURCE=.\SRC\Linea.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Loc.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Locy.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Locz.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Main.for
# End Source File
# Begin Source File

SOURCE=.\SRC\MANQ.FOR
# End Source File
# Begin Source File

SOURCE=.\SRC\Mdr.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Mdr2.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Modif.for
# End Source File
# Begin Source File

SOURCE=.\SRC\MOM.FOR
# End Source File
# Begin Source File

SOURCE=.\SRC\Objct1.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Objct2.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Objin1.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Objin2.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Objpd1.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Objpd2.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Opti.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Paik.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Participation.for
# End Source File
# Begin Source File

SOURCE=.\SRC\PCONTI.FOR
# End Source File
# Begin Source File

SOURCE=.\SRC\PEDGE.FOR
# End Source File
# Begin Source File

SOURCE=.\SRC\PIMPR.FOR
# End Source File
# Begin Source File

SOURCE=.\SRC\PINF1.FOR
# End Source File
# Begin Source File

SOURCE=.\SRC\PINF2.FOR
# End Source File
# Begin Source File

SOURCE=.\SRC\PINF3.FOR
# End Source File
# Begin Source File

SOURCE=.\SRC\PINTEG.FOR
# End Source File
# Begin Source File

SOURCE=.\SRC\Plaque.for
# End Source File
# Begin Source File

SOURCE=.\SRC\PLIMIT.FOR
# End Source File
# Begin Source File

SOURCE=.\SRC\Polrt.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Press.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Rang.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Rang2.for
# End Source File
# Begin Source File

SOURCE=.\SRC\RECIP.FOR
# End Source File
# Begin Source File

SOURCE=.\SRC\RESOLU.FOR
# End Source File
# Begin Source File

SOURCE=.\SRC\Resul.for
# End Source File
# Begin Source File

SOURCE=.\SRC\RESUL2.FOR
# End Source File
# Begin Source File

SOURCE=.\SRC\Save.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Sbega.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Sens.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Sensibcout.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Sensibcout2.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Shear.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Shearbuck.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Shearyield.for
# End Source File
# Begin Source File

SOURCE=.\SRC\SORTI.FOR
# End Source File
# Begin Source File

SOURCE=.\SRC\Stifbend.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Stifbuck.for
# End Source File
# Begin Source File

SOURCE=.\SRC\STIFF.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Stifyield.for
# End Source File
# Begin Source File

SOURCE=.\SRC\System.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Ushull.for
# End Source File
# Begin Source File

SOURCE=.\SRC\VCOS.FOR
# End Source File
# Begin Source File

SOURCE=.\SRC\VECTO.FOR
# End Source File
# Begin Source File

SOURCE=.\SRC\Vision.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Visionn.for
# End Source File
# Begin Source File

SOURCE=.\SRC\VSIN.FOR
# End Source File
# End Group
# End Target
# End Project
