subroutine ultimate_resistance()

use param_section, ONLY : langue

implicit double precision (a-h,o-z)

!     ==================================================================
!13.0 calcul de la resistance ultime de la poutre navire
!     ==================================================================

if(langue.eq.1)then
   write(*,100)'resistance ultime (sr hull)'
else
   write(*,100)'ultimate strength (sr hull)'
endif

call hull()


 100 format(1x,a,i8)                                                                                          


return
end
