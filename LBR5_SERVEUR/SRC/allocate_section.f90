subroutine allocate_section()

use param_section

implicit double precision (a-h,o-z)

! ====================================================================================================
! affichage du nom du fichier de donnees 
! ====================================================================================================

i=len_trim(section_file)
write(*,*)

if(langue.eq.1)write(*,*)'fichier de donnees: '//section_file(1:i)
if(langue.eq.2)write(*,*)'data file         : '//section_file(1:i)


if(langue.eq.1)write(*,*)'fichier solution  : '//iunit2(1:(i+4))
if(langue.eq.2)write(*,*)'solution file     : '//iunit2(1:(i+4))

! allocation dynamique ===============================================================================
call allocate_param_section()	 !déjà fait

if (langue.eq.1) write(*,100) 'Nbr de panneaux   : ',nmax
if (langue.eq.2) write(*,100) 'Pannel number     : ',nmax

return

 100 format(1x,a,i8)                                                                                          

end
