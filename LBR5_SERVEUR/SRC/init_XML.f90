subroutine init_XML()

use dfwin
use param_XML

implicit double precision (a-h,o-z)




!	logical status

!	First, locate the dll and load it into memory
p = loadlibrary("WaxDllXML.dll"C)
if (p == 0) then
	type *, "Error occurred opening WaxDllXML.dll"
	type *, "Program aborting"
	stop   
endif


!	Set up a pointer to the routines of interest
q1 = getprocaddress(p, "_wXML_Pop_@0"C) ! Z'10001050' 
if (q1 == 0) then
	type *, "Error occurred finding wXML_Pop_ in DLLXML.dll"
	type *, "Program aborting"
	stop
endif
	

q2 = getprocaddress(p, "_wXML_getAttributeDouble_@8"C)
if (q2 == 0) then
	type *, "Error occurred finding wXML_getAttributeDouble_ in DLLVC.dll"
	type *, "Program aborting"
	stop
endif

q3 = getprocaddress(p, "_wXML_TraverseToFirstSon_@0"C)
if (q3 == 0) then
	type *, "Error occurred finding wXML_TraverseToFirstSon_ in DLLVC.dll"
	type *, "Program aborting"
	stop
endif

q4 = getprocaddress(p, "_wXML_TraverseToSon_@8"C)
if (q4 == 0) then
	type *, "Error occurred finding wXML_TraverseToSon_ in DLLVC.dll"
	type *, "Program aborting"
	stop
endif
							
q5 = getprocaddress(p, "_wXML_Reader_LoadXML_@8"C)
if (q5 == 0) then
	type *, "Error occurred finding wXML_Reader_LoadXML_ in DLLVC.dll"
	type *, "Program aborting"
	stop
endif
	
q6 = getprocaddress(p, "_wXML_getAttributeDouble_@8"C)
if (q6 == 0) then
	type *, "Error occurred finding wXML_getAttributeLong_ in DLLVC.dll"
	type *, "Program aborting"
	stop
endif

q7 = getprocaddress(p, "_wXML_Values_@8"C)
if (q7 == 0) then
	type *, "Error occurred finding wXML_Values_ in DLLVC.dll"
	type *, "Program aborting"
	stop
endif

q8 = getprocaddress(p, "_wXML_getAttributeString_@16"C)
if (q8 == 0) then
	type *, "Error occurred finding wXML_getAttributeString_ in DLLVC.dll"
	type *, "Program aborting"
	stop
endif

q9 = getprocaddress(p, "_wXML_Reader_Init_@4"C)
if (q9 == 0) then
	type *, "Error occurred finding XML_Reader_Init_ in DLLXML.dll"
	type *, "Program aborting"
	stop
endif

q10 = getprocaddress(p, "_wXML_Reader_Terminate_@0"C)
if (q10 == 0) then
	type *, "Error occurred finding wXML_Reader_Terminate_ in DLLXML.dll"
	type *, "Program aborting"
	stop
endif

q11=getprocaddress(p,"_wXML_TraverseToNextSibling_@0"C)
if (q11 == 0) then
	type *, "Error occurred finding wXML_TraverseToNextSibling_ in DLL XML.dll"
	type *, "Program aborting"
	stop
endif

q12=getprocaddress(p, "_wXML_TraverseToNextFirstSibling_@8"C)
if (q12 == 0) then
	type *, "Error occurred finding wXML_TraverseToNextFirstSiblin g_ inDLL XML.dll"
	type *, "Program aborting"
	stop
endif

q13 = getprocaddress(p, "_wXML_Push_@0"C)
if (q13 == 0) then
	type *, "Error occurred finding wXML_Push_ in DLLXML.dll"
	type *, "Program aborting"
	stop
endif

q14 = getprocaddress(p, "_wXML_Reader_SaveXML_@8"C)
if (q14 == 0) then
	type *, "Error occurred finding _wXML_Reader_SaveXML_@@YAHPADH@Z in DLLXML.dll"
	type *, "Program aborting"
	stop
endif

q16 = getprocaddress(p, "_wXML_addAttribute@16"C)
if (q16 == 0) then
	type *, "Error occurred finding _wXML_addAttribute@@YAHPADH0H@Z in DLLXML.dll"
	type *, "Program aborting"
	stop
endif

q17 = getprocaddress(p, "_wXML_addAttributeDouble@12"C)
if (q17 == 0) then
	type *, "Error occurred finding _wXML_addAttributeDouble@12 in DLLXML.dll"
	type *, "Program aborting"
	stop
endif

q15 = getprocaddress(p, "_wXML_addSon@8"C)
if (q15 == 0) then
	type *, "Error occurred finding _wXML_addSon@@YAHPADH@Z in DLLXML.dll"
	type *, "Program aborting"
	stop
endif

1000 continue

return
end subroutine
