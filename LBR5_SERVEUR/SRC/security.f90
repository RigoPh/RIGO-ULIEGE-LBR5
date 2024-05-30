subroutine security()

use param_section, CHA_=>CHA
use dfport
use dfwin

implicit double precision (a-h,o-z)
integer   seed_														
character*1 cha							

character*80       fileName
integer*4           hFile
integer*4			retcode
type (T_FILETIME)   ftCreation, ftAccessed, ftWrittenTo
type (T_SYSTEMTIME) SysTime
integer*4           dwFileSize
integer*2           wDosDate, wDosTime
integer			DD, MM, YY, HH, MN, SS

!sécurité !bibliotheque de characters							
character (len=*),parameter :: alphanumeric = '0123456789&
abcdefghijklmnoprstxyuvwzABCDEFGHIJKLMNOPRSTXYUVWZ0123456&
789abcdefghijklmnoprstxyuvwzABCDEFGHIJKLMNOPRSTXYUVWZ0123&
456789abcdefghijklmnoprstxyuvwzABCDEFGHIJKLMNOPRSTXYUVWZ0&
123456789abcdefghijklmnoprstxyuvwzABCDEFGHIJKLMNOPRSTXYUV&
WZ0123456789abcde'								
		
!-------------------------------------------------------------------------

do iboat=1,nfile	!TODO vérifier que ça marche

	!	calcul du seed_ 

	fileName = section_files(iboat)

	hFile = CreateFile (fileName, GENERIC_READ, FILE_SHARE_READ, NULL_SECURITY_ATTRIBUTES, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL)

	retCode = GetFileTime (hFile , ftCreation ,ftAccessed ,ftWrittenTo)

	retCode = FileTimeToDosDateTime(ftWrittenTo, LOC(wDosDate), LOC(wDosTime))

	dwFileSize = GetFileSize (hFile, NULL)

	retCode = CloseHandle (hFile)

	retCode = FileTimeToSystemTime (LOC(ftWrittenTo), LOC(SysTime))
	YY = SysTime%wYear
	MM = SysTime%wMonth
	DD = SysTime%wDay
	SS = SysTime%wSecond
	MN = SysTime%wMinute
	HH = SysTime%wHour
														
	seed_ = YY + MM + DD + SS + MN + HH + dwFileSize  

	!	génération de la signature
	!	--------------------------
	!16 est la taille du code  
	do i = 1, 16	
		 j = mod((57 * (mod((57 * seed_ + 1), 256)) + 1), 256)
		 if(j.eq.0) j=1
		 cha = alphanumeric(j:j)
		 key_lbr5(i:i) = cha		
		 seed_ = j
	enddo
		
	!	comparaison des signatures gui et solveur
	!	-----------------------------------------
	if(key_gui(iboat).ne.key_lbr5) then 	 		 
		write(*,*) 'security check failed. execution stopped.'
		read(*,*)
		stop
	else
		write(*,*) 'security check passed.'
	endif	

enddo ! fin de la boucle sur iboat

return
end
