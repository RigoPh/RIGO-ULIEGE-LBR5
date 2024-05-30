subroutine deallocate_section_header()


use param_section
use PARAM_SECTION_VECTOR
!use param_multiobj



deallocate(casename)
deallocate(is_loadcase_fatigue)
!deallocate(is_loadcase_selected)
IF (ALLOCATED(NNSOL_BV_TYPE)) deallocate(NNSOL_BV_TYPE)		!fat_new
deallocate(tirant)
deallocate(is_selected_loadcase_fatigue)
deallocate(nnsol)


nom      =     ''
header1  =     ''
header2  =     ''
              
iunit1   =     ''
iunit2   =     ''
iunit3   =     ''
iunit4   =     ''
iunit5   =     ''
iunit6   =     ''
iunit7   =     ''
iunit8   =     ''
              
  neto   =     0
  iana   =     0
indaig   =     0
indrai   =     0
 ibusc   =     0
ipoids   =     0
 nsolm   =     0
  nsol   =     0
    ne   =     0
  imax   =     0
  jlph   =     0
 jlph2   =     0
iprint   =     0
              
 depth           =     0.d00
length           =     0.d00
 width           =     0.d00
part_full        =     0.d00
method_fatigue   =     0
tfl              =     0.d00
              
   dis(:)=     0.d00
   fam(:)=     0.d00

icout  = 0

poids  = 0.d00
spoids = 0.d00

fmat11 = 0.d00
fmat22 = 0.d00
fmat33 = 0.d00
fsou11 = 0.d00
fsou22 = 0.d00
fsou33 = 0.d00
fmdo11 = 0.d00
fmdo22 = 0.d00
fmdo33 = 0.d00
fmdo44 = 0.d00
fmdo55 = 0.d00
fmdo66 = 0.d00
fmdo77 = 0.d00

rend_global   = 0.d00
!eqp_global    = 0.d00

imulti = 0    
w1	   = 0.d00
w2     = 0.d00       
w3     = 0.d00       
rho    = 0
fk(:)  = 0.d00   

end
