# Microsoft Developer Studio Project File - Name="LBR5Dynamic" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=LBR5Dynamic - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "LBR5Dyn.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "LBR5Dyn.mak" CFG="LBR5Dynamic - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "LBR5Dynamic - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "LBR5Dynamic - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "LBR5Dynamic - Win32 Release"

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
# ADD BASE F90 /compile_only /include:"Release/" /nologo /warn:nofileopt
# ADD F90 /compile_only /debug:none /define:"DEBUGFRED" /fpp /include:"Release/" /nologo /optimize:3 /real_size:64 /traceback /warn:errors
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD BASE RSC /l 0x80c /d "NDEBUG"
# ADD RSC /l 0x80c /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /profile /debug /machine:I386 /out:"Work\LBR5Dyn.exe"
# Begin Special Build Tool
SOURCE="$(InputPath)"
PostBuild_Cmds=copy lib\glcmc.dll work	copy lib\glcmc.cfg work	copy lib\waxdllXML.dll work
# End Special Build Tool

!ELSEIF  "$(CFG)" == "LBR5Dynamic - Win32 Debug"

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
# ADD BASE F90 /check:bounds /compile_only /debug:full /include:"Debug/" /nologo /warn:argument_checking /warn:nofileopt
# ADD F90 /assume:minus0 /check:bounds /check:power /check:overflow /check:underflow /compile_only /debug:full /fltconsistency /fpe:0 /fpp /include:"Debug/" /nopdbfile /real_size:64 /traceback /warn:argument_checking /warn:nofileopt /warn:unused
# SUBTRACT F90 /check:format /warn:declarations /warn:truncated_source
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD BASE RSC /l 0x80c /d "_DEBUG"
# ADD RSC /l 0x80c /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /profile /debug /machine:I386 /out:"TEST\LBR5Dyn.exe" /libpath:"Lib"
# Begin Special Build Tool
SOURCE="$(InputPath)"
PostBuild_Cmds=copy lib\glcmc.dll test	copy lib\glcmc.cfg test	copy lib\waxdllXML.dll test
# End Special Build Tool

!ENDIF 

# Begin Target

# Name "LBR5Dynamic - Win32 Release"
# Name "LBR5Dynamic - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;f90;for;f;fpp"
# Begin Group "Vibration"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\SRC\Vibration\daxpy.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\dcopy.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\ddot.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\dgebak.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\dgebal.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\dgeev.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\dgehd2.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\dgehrd.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\dgemm.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\dgemv.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\dger.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\dgetf2.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\dgetrf.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\dgetri.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\dhseqr.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\dlabad.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\dlacpy.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\dladiv.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\dlaexc.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\dlahqr.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\dlahr2.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\dlaln2.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\dlamch.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\dlange.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\dlanv2.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\dlapy2.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\dlaqr0.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\dlaqr1.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\dlaqr2.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\dlaqr3.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\dlaqr4.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\dlaqr5.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\dlarf.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\dlarfb.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\dlarfg.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\dlarft.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\dlarfx.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\dlartg.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\dlascl.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\dlaset.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\dlassq.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\dlaswp.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\dlasy2.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\dnrm2.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\dorg2r.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\dorghr.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\dorgqr.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\drot.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\dscal.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\dswap.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\dtrevc.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\dtrexc.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\dtrmm.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\dtrmv.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\dtrsm.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\dtrti2.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\dtrtri.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\idamax.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\ieeeck.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\ilaenv.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\iparmq.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\lsame.f
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\sorting_dp.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Vibration\xerbla.f
# End Source File
# End Group
# Begin Group "Ansys"

# PROP Default_Filter ""
# Begin Source File

SOURCE=.\SRC\Ansys\ptsinpoly.for
# End Source File
# End Group
# Begin Source File

SOURCE=.\SRC\allocate_panel_data.f90
DEP_F90_ALLOC=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_read_write.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\allocate_param_fatigue.f90
DEP_F90_ALLOCA=\
	".\Debug\param_fatigue_private.mod"\
	".\Debug\param_fatigue_public.mod"\
	".\Debug\param_section.mod"\
	

!IF  "$(CFG)" == "LBR5Dynamic - Win32 Release"

# ADD F90 /warn:general

!ELSEIF  "$(CFG)" == "LBR5Dynamic - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\SRC\allocate_param_materiau.f90
DEP_F90_ALLOCAT=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\allocate_param_materiau_vector.f90
DEP_F90_ALLOCATE=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_section_header_vector.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\allocate_param_multi.f90
DEP_F90_ALLOCATE_=\
	".\Debug\param_multi.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\allocate_param_opti_global.f90
DEP_F90_ALLOCATE_P=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_read_write_vector.mod"\
	".\Debug\param_section_header_vector.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\allocate_param_read_write_vector.f90
DEP_F90_ALLOCATE_PA=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_read_write_vector.mod"\
	".\Debug\param_section_header_vector.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\allocate_param_restri_global_vector.f90
DEP_F90_ALLOCATE_PAR=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_read_write_vector.mod"\
	".\Debug\param_restri_global_vector.mod"\
	".\Debug\param_section_header_vector.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\allocate_param_section.f90
DEP_F90_ALLOCATE_PARA=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_restri_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\allocate_param_section_header.f90
DEP_F90_ALLOCATE_PARAM=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_section_header_vector.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\allocate_section.f90
DEP_F90_ALLOCATE_S=\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\analys.f90
DEP_F90_ANALY=\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\anglesr.for
DEP_F90_ANGLE=\
	".\Debug\param_multi.mod"\
	".\Debug\param_restri_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\ansys.f90
DEP_F90_ANSYS=\
	".\Debug\param_multi.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\assemb.f90
# End Source File
# Begin Source File

SOURCE=.\SRC\bateau.f90
DEP_F90_BATEA=\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\beam_theory.f90
DEP_F90_BEAM_=\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_restri_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\bending.f90
DEP_F90_BENDI=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\bendyield.f90
DEP_F90_BENDY=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\bo1.f90
DEP_F90_BO1_F=\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_read_write.mod"\
	".\Debug\param_restri_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\bo2.f90
DEP_F90_BO2_F=\
	".\Debug\param_fatigue_public.mod"\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_read_write.mod"\
	".\Debug\param_restri_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\bord.for
# End Source File
# Begin Source File

SOURCE=.\SRC\borne.f90
DEP_F90_BORNE=\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\Buckdcn07.for
DEP_F90_BUCKD=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\Buckdcn08.for
DEP_F90_BUCKDC=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\carac.for
DEP_F90_CARAC=\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\carac2.for
DEP_F90_CARAC2=\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\caracd.for
DEP_F90_CARACD=\
	".\Debug\param_multi.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\check_multi.f90
DEP_F90_CHECK=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_section_header_vector.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\close.f90
DEP_F90_CLOSE=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\close_XML.f90
DEP_F90_CLOSE_=\
	".\Debug\param_XML.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\coe.for
DEP_F90_COE_F=\
	".\Debug\param_multi.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\combine.for
DEP_F90_COMBI=\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\comp_obj.f90
DEP_F90_COMP_=\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_restri_global.mod"\
	".\Debug\param_restri_global_vector.mod"\
	".\Debug\param_section.mod"\
	".\Debug\param_section_header_vector.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\comp_obj_global.f90
DEP_F90_COMP_O=\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_restri_global.mod"\
	".\Debug\param_restri_global_vector.mod"\
	".\Debug\param_section.mod"\
	".\Debug\param_section_header_vector.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\comp_reduced_var.f90
DEP_F90_COMP_R=\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_read_write_vector.mod"\
	".\Debug\param_restri_global.mod"\
	".\Debug\param_restri_global_vector.mod"\
	".\Debug\param_section.mod"\
	".\Debug\param_section_header_vector.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\comp_reduced_var_global.f90
DEP_F90_COMP_RE=\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_read_write.mod"\
	".\Debug\param_read_write_vector.mod"\
	".\Debug\param_restri_global.mod"\
	".\Debug\param_restri_global_vector.mod"\
	".\Debug\param_section.mod"\
	".\Debug\param_section_header_vector.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\comp_restri_local.f90
DEP_F90_COMP_RES=\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_read_write_vector.mod"\
	".\Debug\param_restri_global.mod"\
	".\Debug\param_restri_global_vector.mod"\
	".\Debug\param_section.mod"\
	".\Debug\param_section_header_vector.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\Compbuck.for
DEP_F90_COMPB=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\comple.f90
# End Source File
# Begin Source File

SOURCE=.\SRC\compu.for
# End Source File
# Begin Source File

SOURCE=.\SRC\compute_panel_weight.f90
DEP_F90_COMPU=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_read_write.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\compute_save_panel_data.f90
# End Source File
# Begin Source File

SOURCE=.\SRC\compute_stiffness_coeff.f90
DEP_F90_COMPUT=\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_read_write.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\conlin.for
DEP_F90_CONLI=\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_restri_global_vector.mod"\
	".\Debug\param_section.mod"\
	".\Debug\param_section_header_vector.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\contr.for
DEP_F90_CONTR=\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\coord.for
DEP_F90_COORD=\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_restri_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\coord_transform.f90
DEP_F90_COORD_=\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_restri_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\cost.f90
DEP_F90_COST_=\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_read_write.mod"\
	".\Debug\param_section.mod"\
	".\Debug\param_section_header_vector.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\costmain.for
DEP_F90_COSTM=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\cout.for
DEP_F90_COUT_=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\dargsr.for
DEP_F90_DARGS=\
	".\Debug\param_multi.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\dcns_geom_n50_51.for
DEP_F90_DCNS_=\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\dcns_geom_n52_53.for
DEP_F90_DCNS_G=\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\deallocate_panel_data.f90
DEP_F90_DEALL=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_read_write.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\deallocate_param_fatigue.f90
DEP_F90_DEALLO=\
	".\Debug\param_fatigue_private.mod"\
	".\Debug\param_fatigue_public.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\deallocate_param_materiau.f90
DEP_F90_DEALLOC=\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\deallocate_param_materiau_vector.f90
DEP_F90_DEALLOCA=\
	".\Debug\param_section_header_vector.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\deallocate_param_multi.f90
DEP_F90_DEALLOCAT=\
	".\Debug\param_multi.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\deallocate_param_opti_global.f90
DEP_F90_DEALLOCATE=\
	".\Debug\param_opti_global.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\deallocate_param_read_write_vector.f90
DEP_F90_DEALLOCATE_=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_read_write_vector.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\deallocate_param_restri_global_vector.f90
DEP_F90_DEALLOCATE_P=\
	".\Debug\param_restri_global_vector.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\deallocate_param_section.f90
DEP_F90_DEALLOCATE_PA=\
	".\Debug\param_multi.mod"\
	".\Debug\param_restri_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\deallocate_param_section_header.f90
DEP_F90_DEALLOCATE_PAR=\
	".\Debug\param_section_header_vector.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\deallocate_section.f90
# End Source File
# Begin Source File

SOURCE=.\SRC\deallocate_section_header.f90
DEP_F90_DEALLOCATE_S=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_section.mod"\
	".\Debug\param_section_header_vector.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\deriv.for
# End Source File
# Begin Source File

SOURCE=.\SRC\dhydro.for
DEP_F90_DHYDR=\
	".\Debug\param_multi.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\dhyps.f90
DEP_F90_DHYPS=\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\discu.for
DEP_F90_DISCU=\
	".\Debug\param_multi.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\Dloc.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Dlocy.for
DEP_F90_DLOCY=\
	".\Debug\param_multi.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\Dlocz.for
# End Source File
# Begin Source File

SOURCE=.\SRC\dmomsr.for
# End Source File
# Begin Source File

SOURCE=.\SRC\dpar.for
# End Source File
# Begin Source File

SOURCE=.\SRC\dpress.for
# End Source File
# Begin Source File

SOURCE=.\SRC\ecri.f90
DEP_F90_ECRI_=\
	".\Debug\param_multi.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\ecri2.f90
DEP_F90_ECRI2=\
	".\Debug\param_multi.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\ecri3.f90
DEP_F90_ECRI3=\
	".\Debug\param_multi.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\ecri4.f90
DEP_F90_ECRI4=\
	".\Debug\param_multi.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\ecrivar.f90
DEP_F90_ECRIV=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\edge_moments.f90
DEP_F90_EDGE_=\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_read_write.mod"\
	".\Debug\param_restri_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\Effort_Tranchant.f90
# End Source File
# Begin Source File

SOURCE=.\SRC\equal_restrictions.f90
DEP_F90_EQUAL=\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\equil.for
DEP_F90_EQUIL=\
	".\Debug\param_multi.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\fatigue_amirouche.f90
DEP_F90_FATIG=\
	".\Debug\param_fatigue_public.mod"\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_read_write.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\fatigue_damage.f90
DEP_F90_FATIGU=\
	".\Debug\param_fatigue_public.mod"\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_read_write.mod"\
	".\Debug\param_section.mod"\
	

!IF  "$(CFG)" == "LBR5Dynamic - Win32 Release"

# ADD F90 /warn:general

!ELSEIF  "$(CFG)" == "LBR5Dynamic - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\SRC\fatigue_debug1.f90
DEP_F90_FATIGUE=\
	".\Debug\param_fatigue_public.mod"\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_read_write.mod"\
	".\Debug\param_restri_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\fatigue_debug1_u.f90
DEP_F90_FATIGUE_=\
	".\Debug\param_fatigue_public.mod"\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_read_write.mod"\
	".\Debug\param_restri_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\fatigue_debug2.f90
DEP_F90_FATIGUE_D=\
	".\Debug\param_fatigue_public.mod"\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_read_write.mod"\
	".\Debug\param_restri_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\fatigue_debug3.f90
DEP_F90_FATIGUE_DE=\
	".\Debug\param_fatigue_public.mod"\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_read_write.mod"\
	".\Debug\param_restri_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\fatigue_debug4.f90
DEP_F90_FATIGUE_DEB=\
	".\Debug\param_fatigue_public.mod"\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_read_write.mod"\
	".\Debug\param_restri_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\fatigue_debug_sens.f90
DEP_F90_FATIGUE_DEBU=\
	".\Debug\param_fatigue_public.mod"\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_read_write.mod"\
	".\Debug\param_restri_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\fatigue_element.f90
DEP_F90_FATIGUE_E=\
	".\Debug\param_fatigue_private.mod"\
	".\Debug\param_fatigue_public.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\fatigue_frame_compute.f90
DEP_F90_FATIGUE_F=\
	".\Debug\param_fatigue_public.mod"\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\fatigue_girders_compute.f90
DEP_F90_FATIGUE_G=\
	".\Debug\param_fatigue_public.mod"\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\fatigue_girders_init.f90
DEP_F90_FATIGUE_GI=\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\fatigue_init.f90
DEP_F90_FATIGUE_I=\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\fatigue_output.f90
DEP_F90_FATIGUE_O=\
	".\Debug\param_fatigue_private.mod"\
	".\Debug\param_fatigue_public.mod"\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\fatigue_scratch_files.f90
DEP_F90_FATIGUE_S=\
	".\Debug\param_fatigue_public.mod"\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_read_write.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\fatigue_sens.f90
DEP_F90_FATIGUE_SE=\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\fatigue_stiffened_panel_compute.f90
DEP_F90_FATIGUE_ST=\
	".\Debug\param_fatigue_public.mod"\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\flamb.for
DEP_F90_FLAMB=\
	".\Debug\param_multi.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\geom.for
DEP_F90_GEOM_=\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\get_main_parameter.f90
DEP_F90_GET_M=\
	".\Debug\param_multi.mod"\
	".\Debug\param_section.mod"\
	".\Debug\param_section_header_vector.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\get_optim_data.f90
DEP_F90_GET_O=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_read_write.mod"\
	".\Debug\param_read_write_vector.mod"\
	".\Debug\param_restri_global.mod"\
	".\Debug\param_restri_global_vector.mod"\
	".\Debug\param_section.mod"\
	".\Debug\param_section_header_vector.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\get_panel_data_global.f90
DEP_F90_GET_P=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_read_write.mod"\
	".\Debug\param_read_write_vector.mod"\
	".\Debug\param_restri_global.mod"\
	".\Debug\param_restri_global_vector.mod"\
	".\Debug\param_section.mod"\
	".\Debug\param_section_header_vector.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\get_param_fatigue.f90
DEP_F90_GET_PA=\
	".\Debug\param_fatigue_public.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\get_param_materiau.f90
DEP_F90_GET_PAR=\
	".\Debug\param_multi.mod"\
	".\Debug\param_section.mod"\
	".\Debug\param_section_header_vector.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\get_param_sens_fatigue.f90
DEP_F90_GET_PARA=\
	".\Debug\param_fatigue_public.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\get_section_header.f90
DEP_F90_GET_S=\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\get_section_header_vector.f90
DEP_F90_GET_SE=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_section.mod"\
	".\Debug\param_section_header_vector.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\hughes.f90
DEP_F90_HUGHE=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_read_write.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\hull.for
DEP_F90_HULL_=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_restri_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\hydro.for
# End Source File
# Begin Source File

SOURCE=.\SRC\hyp1.for
DEP_F90_HYP1_=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_read_write.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\hyp2.for
DEP_F90_HYP2_=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\Imprtau.for
DEP_F90_IMPRT=\
	".\Debug\param_multi.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\Inertia.for
DEP_F90_INERT=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\init_objective.f90
DEP_F90_INIT_=\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_restri_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\init_panel_data.f90
DEP_F90_INIT_P=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_read_write.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\init_param_multi.f90
DEP_F90_INIT_PA=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\init_XML.f90
DEP_F90_INIT_X=\
	".\Debug\param_XML.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\initialize_loop.f90
DEP_F90_INITI=\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\input_recapitulatif.f90
DEP_F90_INPUT=\
	".\Debug\param_fatigue_public.mod"\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_read_write.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\inteq.f90
DEP_F90_INTEQ=\
	".\Debug\param_read_write.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\lbr4.f90
DEP_F90_LBR4_=\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_read_write.mod"\
	".\Debug\param_restri_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\lbr5.f90
DEP_F90_LBR5_=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\lcc.f90
DEP_F90_LCC_F=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_section.mod"\
	".\Debug\param_section_header_vector.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\linde.for
# End Source File
# Begin Source File

SOURCE=.\SRC\linea.for
DEP_F90_LINEA=\
	".\Debug\param_multi.mod"\
	
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

SOURCE=.\SRC\loop.f90
DEP_F90_LOOP_=\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_read_write.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\manq.f90
DEP_F90_MANQ_=\
	".\Debug\param_multi.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\mdr.for
DEP_F90_MDR_F=\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\mdr2.for
DEP_F90_MDR2_=\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\modif.for
DEP_F90_MODIF=\
	".\Debug\param_multi.mod"\
	".\Debug\param_restri_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\mom.for
# End Source File
# Begin Source File

SOURCE=.\SRC\Objct1.f90
DEP_F90_OBJCT=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\Objin1.for
DEP_F90_OBJIN=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\objlcc.for
DEP_F90_OBJLC=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\objpd1.for
DEP_F90_OBJPD=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\open_local.f90
DEP_F90_OPEN_=\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\open_scratch_files.f90
DEP_F90_OPEN_S=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_section_header_vector.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\open_section_file.f90
DEP_F90_OPEN_SE=\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\open_section_output_files.f90
DEP_F90_OPEN_SEC=\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\opti_global_multisection.f90
DEP_F90_OPTI_=\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_read_write_vector.mod"\
	".\Debug\param_restri_global.mod"\
	".\Debug\param_restri_global_vector.mod"\
	".\Debug\param_section.mod"\
	".\Debug\param_section_header_vector.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\opti_local.f90
DEP_F90_OPTI_L=\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_read_write_vector.mod"\
	".\Debug\param_restri_global.mod"\
	".\Debug\param_restri_global_vector.mod"\
	".\Debug\param_section.mod"\
	".\Debug\param_section_header_vector.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\opti_local_to_global.f90
DEP_F90_OPTI_LO=\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_read_write.mod"\
	".\Debug\param_read_write_vector.mod"\
	".\Debug\param_restri_global.mod"\
	".\Debug\param_restri_global_vector.mod"\
	".\Debug\param_section.mod"\
	".\Debug\param_section_header_vector.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\paik.for
DEP_F90_PAIK_=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\param_fatigue_private.f90

!IF  "$(CFG)" == "LBR5Dynamic - Win32 Release"

!ELSEIF  "$(CFG)" == "LBR5Dynamic - Win32 Debug"

!ENDIF 

F90_MODOUT=\
	"param_fatigue_private"

# End Source File
# Begin Source File

SOURCE=.\SRC\param_fatigue_public.f90

!IF  "$(CFG)" == "LBR5Dynamic - Win32 Release"

!ELSEIF  "$(CFG)" == "LBR5Dynamic - Win32 Debug"

!ENDIF 

F90_MODOUT=\
	"param_fatigue_public"

# End Source File
# Begin Source File

SOURCE=.\SRC\param_init.f90

!IF  "$(CFG)" == "LBR5Dynamic - Win32 Release"

!ELSEIF  "$(CFG)" == "LBR5Dynamic - Win32 Debug"

!ENDIF 

F90_MODOUT=\
	"param_init"

# End Source File
# Begin Source File

SOURCE=.\SRC\param_multi.f90

!IF  "$(CFG)" == "LBR5Dynamic - Win32 Release"

!ELSEIF  "$(CFG)" == "LBR5Dynamic - Win32 Debug"

!ENDIF 

F90_MODOUT=\
	"param_multi"

# End Source File
# Begin Source File

SOURCE=.\SRC\param_opti_global.f90

!IF  "$(CFG)" == "LBR5Dynamic - Win32 Release"

!ELSEIF  "$(CFG)" == "LBR5Dynamic - Win32 Debug"

!ENDIF 

F90_MODOUT=\
	"param_opti_global"

# End Source File
# Begin Source File

SOURCE=.\SRC\param_read_write.f90
DEP_F90_PARAM=\
	".\Debug\param_section.mod"\
	

!IF  "$(CFG)" == "LBR5Dynamic - Win32 Release"

!ELSEIF  "$(CFG)" == "LBR5Dynamic - Win32 Debug"

!ENDIF 

F90_MODOUT=\
	"param_read_write"

# End Source File
# Begin Source File

SOURCE=.\SRC\param_read_write_vector.f90
DEP_F90_PARAM_=\
	".\Debug\param_section.mod"\
	

!IF  "$(CFG)" == "LBR5Dynamic - Win32 Release"

!ELSEIF  "$(CFG)" == "LBR5Dynamic - Win32 Debug"

!ENDIF 

F90_MODOUT=\
	"param_read_write_vector"

# End Source File
# Begin Source File

SOURCE=.\SRC\param_restri_global.f90

!IF  "$(CFG)" == "LBR5Dynamic - Win32 Release"

!ELSEIF  "$(CFG)" == "LBR5Dynamic - Win32 Debug"

!ENDIF 

F90_MODOUT=\
	"param_restri_global"

# End Source File
# Begin Source File

SOURCE=.\SRC\param_restri_global_vector.f90

!IF  "$(CFG)" == "LBR5Dynamic - Win32 Release"

!ELSEIF  "$(CFG)" == "LBR5Dynamic - Win32 Debug"

!ENDIF 

F90_MODOUT=\
	"param_restri_global_vector"

# End Source File
# Begin Source File

SOURCE=.\SRC\param_section.f90
DEP_F90_PARAM_S=\
	".\Debug\param_multi.mod"\
	

!IF  "$(CFG)" == "LBR5Dynamic - Win32 Release"

!ELSEIF  "$(CFG)" == "LBR5Dynamic - Win32 Debug"

!ENDIF 

F90_MODOUT=\
	"param_section"

# End Source File
# Begin Source File

SOURCE=.\SRC\param_section_header_vector.f90
DEP_F90_PARAM_SE=\
	".\Debug\param_multi.mod"\
	

!IF  "$(CFG)" == "LBR5Dynamic - Win32 Release"

!ELSEIF  "$(CFG)" == "LBR5Dynamic - Win32 Debug"

!ENDIF 

F90_MODOUT=\
	"param_section_header_vector"

# End Source File
# Begin Source File

SOURCE=.\SRC\param_XML.f90

!IF  "$(CFG)" == "LBR5Dynamic - Win32 Release"

!ELSEIF  "$(CFG)" == "LBR5Dynamic - Win32 Debug"

!ENDIF 

F90_MODOUT=\
	"param_XML"

# End Source File
# Begin Source File

SOURCE=.\SRC\participation.for
DEP_F90_PARTI=\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\pconti.for
# End Source File
# Begin Source File

SOURCE=.\SRC\pedge.for
# End Source File
# Begin Source File

SOURCE=.\SRC\pimpr.for
DEP_F90_PIMPR=\
	".\Debug\param_multi.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\pinf1.f90
# End Source File
# Begin Source File

SOURCE=.\SRC\pinf2.f90
# End Source File
# Begin Source File

SOURCE=.\SRC\pinf3.f90
# End Source File
# Begin Source File

SOURCE=.\SRC\pinteg.for
# End Source File
# Begin Source File

SOURCE=.\SRC\plaque.for
DEP_F90_PLAQU=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\plimit.for
DEP_F90_PLIMI=\
	".\Debug\param_multi.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\poids.for
DEP_F90_POIDS=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\polrt.for
# End Source File
# Begin Source File

SOURCE=.\SRC\press.for
DEP_F90_PRESS=\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\print_panel_data.f90
DEP_F90_PRINT=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_read_write.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\print_restrictions.f90
DEP_F90_PRINT_=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_read_write.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\put_optim_data.f90
DEP_F90_PUT_O=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_read_write.mod"\
	".\Debug\param_read_write_vector.mod"\
	".\Debug\param_restri_global.mod"\
	".\Debug\param_restri_global_vector.mod"\
	".\Debug\param_section.mod"\
	".\Debug\param_section_header_vector.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\put_panel_data_global.f90
DEP_F90_PUT_P=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_read_write.mod"\
	".\Debug\param_read_write_vector.mod"\
	".\Debug\param_restri_global.mod"\
	".\Debug\param_restri_global_vector.mod"\
	".\Debug\param_section.mod"\
	".\Debug\param_section_header_vector.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\put_param_materiau.f90
DEP_F90_PUT_PA=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_section.mod"\
	".\Debug\param_section_header_vector.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\put_section_header_vector.f90
DEP_F90_PUT_S=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_section.mod"\
	".\Debug\param_section_header_vector.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\rang.for
DEP_F90_RANG_=\
	".\Debug\param_multi.mod"\
	".\Debug\param_restri_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\rang2.for
DEP_F90_RANG2=\
	".\Debug\param_multi.mod"\
	".\Debug\param_restri_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\read_comput_points.f90
DEP_F90_READ_=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_read_write.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\read_ega_multisection.f90
DEP_F90_READ_E=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_section_header_vector.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\read_fatigue_restrictions.f90
DEP_F90_READ_F=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_read_write.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\read_following_panels.f90
DEP_F90_READ_FO=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_read_write.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\read_geom_restrictions.f90
DEP_F90_READ_G=\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_read_write.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\read_panel_data.f90
DEP_F90_READ_P=\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_read_write.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\read_panel_restrictions.f90
DEP_F90_READ_PA=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_read_write.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\read_panel_vibration.f90
DEP_F90_READ_PAN=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_read_write.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\read_panel_XML.f90
DEP_F90_READ_PANE=\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_read_write.mod"\
	".\Debug\param_restri_global.mod"\
	".\Debug\param_section.mod"\
	".\Debug\param_XML.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\read_ship_input_file.f90
DEP_F90_READ_S=\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_section.mod"\
	".\Debug\param_section_header_vector.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\read_slosh_restrictions.f90
DEP_F90_READ_SL=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_read_write.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\read_struct_restrictions.f90
DEP_F90_READ_ST=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_read_write.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\read_write_panel.f90
DEP_F90_READ_W=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_read_write.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\recip.for
DEP_F90_RECIP=\
	".\Debug\param_multi.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\recompute_objective_multisection.f90
DEP_F90_RECOM=\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_read_write_vector.mod"\
	".\Debug\param_restri_global.mod"\
	".\Debug\param_section.mod"\
	".\Debug\param_section_header_vector.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\resolu.for
DEP_F90_RESOL=\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\resul.for
DEP_F90_RESUL=\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\resul2.for
DEP_F90_RESUL2=\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\rewind_all.f90
DEP_F90_REWIN=\
	".\Debug\param_multi.mod"\
	".\Debug\param_section.mod"\
	".\Debug\param_section_header_vector.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\save_fatigue.f90
DEP_F90_SAVE_=\
	".\Debug\param_fatigue_public.mod"\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_read_write.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\save_panel_data_for_loads.f90
DEP_F90_SAVE_P=\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_read_write.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\save_restrictions.f90
DEP_F90_SAVE_R=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_read_write.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\sbega.f90
DEP_F90_SBEGA=\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\security.f90
DEP_F90_SECUR=\
	".\Debug\param_multi.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\select.f90
DEP_F90_SELEC=\
	".\Debug\param_multi.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\semel.f90
DEP_F90_SEMEL=\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\sens.f90
DEP_F90_SENS_=\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\sensib_diff_finies.f90
DEP_F90_SENSI=\
	".\Debug\param_fatigue_private.mod"\
	".\Debug\param_fatigue_public.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\sensibcout.for
DEP_F90_SENSIB=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\set_param_fatigue.f90
DEP_F90_SET_P=\
	".\Debug\param_fatigue_public.mod"\
	".\Debug\param_read_write.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\set_param_sens_fatigue.f90
DEP_F90_SET_PA=\
	".\Debug\param_fatigue_public.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\Shear.for
DEP_F90_SHEAR=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_restri_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\Shearbuck.for
DEP_F90_SHEARB=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\Shearyield.for
DEP_F90_SHEARY=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\sloshing.f90
DEP_F90_SLOSH=\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_read_write.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\sorti.f90
# End Source File
# Begin Source File

SOURCE=.\SRC\Spepite.f90
DEP_F90_SPEPI=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\stifbend.f90
DEP_F90_STIFB=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_read_write.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\Stifbuck.for
DEP_F90_STIFBU=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\Stifbuck_dcns.for
DEP_F90_STIFBUC=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\stiff.f90
DEP_F90_STIFF=\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\Stifyield.for
DEP_F90_STIFY=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\system.f90
DEP_F90_SYSTE=\
	".\Debug\param_multi.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\tools.f90
# End Source File
# Begin Source File

SOURCE=.\SRC\ultimate_resistance.f90
DEP_F90_ULTIM=\
	".\Debug\param_multi.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\update.f90
DEP_F90_UPDAT=\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\ushull.f90
DEP_F90_USHUL=\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_restri_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\vcos.f90
# End Source File
# Begin Source File

SOURCE=.\SRC\vecto.f90
# End Source File
# Begin Source File

SOURCE=.\SRC\vibration.f90
DEP_F90_VIBRA=\
	".\Debug\param_multi.mod"\
	".\Debug\param_restri_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\vibration_mod_input_file.for
# End Source File
# Begin Source File

SOURCE=.\SRC\vibration_module_plus_v6.for
# End Source File
# Begin Source File

SOURCE=.\SRC\vsin.f90
# End Source File
# Begin Source File

SOURCE=.\SRC\write_amirouche_global.f90
DEP_F90_WRITE=\
	".\Debug\param_fatigue_public.mod"\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\write_amirouche_panel_data.f90
DEP_F90_WRITE_=\
	".\Debug\param_fatigue_public.mod"\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_read_write.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\write_amirouche_panel_loads.f90
DEP_F90_WRITE_A=\
	".\Debug\param_fatigue_public.mod"\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_read_write.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\write_amirouche_stiffened_panel_loads.f90
DEP_F90_WRITE_AM=\
	".\Debug\param_fatigue_public.mod"\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_read_write.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\write_lotStat.f90
DEP_F90_WRITE_L=\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\write_panel_data.f90
DEP_F90_WRITE_P=\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_read_write.mod"\
	".\Debug\param_read_write_vector.mod"\
	".\Debug\param_restri_global.mod"\
	".\Debug\param_section.mod"\
	
# End Source File
# Begin Source File

SOURCE=.\SRC\write_panel_XML.f90
DEP_F90_WRITE_PA=\
	".\Debug\param_init.mod"\
	".\Debug\param_multi.mod"\
	".\Debug\param_opti_global.mod"\
	".\Debug\param_read_write.mod"\
	".\Debug\param_restri_global.mod"\
	".\Debug\param_section.mod"\
	".\Debug\param_XML.mod"\
	
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# End Group
# End Target
# End Project
