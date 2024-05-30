subroutine close_XML()

use dfwin
use param_XML

iResult = wXML_Reader_Terminate_()

!nullify (q1)
!deallocate(q1)


!	Unload the dll
status = freelibrary(p)

!	Check if the dll was unloaded (T for true)
!type *, "freelibrary status was: ", status

return
end subroutine
