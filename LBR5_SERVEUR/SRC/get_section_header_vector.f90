subroutine get_section_header_vector()


use param_section
use PARAM_SECTION_VECTOR


nom      =        nom_vector(iboat)
header1  =    header1_vector(iboat)
header2  =    header2_vector(iboat)
              
iunit1   =     iunit1_vector(iboat)
iunit2   =     iunit2_vector(iboat)
iunit3   =     iunit3_vector(iboat)
iunit4   =     iunit4_vector(iboat)
iunit5   =     iunit5_vector(iboat)
iunit6   =     iunit6_vector(iboat)
iunit7   =     iunit7_vector(iboat)
iunit8   =     iunit8_vector(iboat)
iunit9   =     iunit9_vector(iboat)
!iunit10  =    iunit10_vector(iboat)
!iunit11  =    iunit11_vector(iboat)
iunit12  =    iunit12_vector(iboat)

allocate(nnsol(nsolmax))		!new4

nnsol(:) =    nnsol_vector(:,iboat)

              
  neto   =       neto_vector(iboat)
  ngmax  =       ngmax_vector(iboat)
  iana   =       iana_vector(iboat)
indaig   =     indaig_vector(iboat)
indrai   =     indrai_vector(iboat)
 ibusc   =      ibusc_vector(iboat)
ipoids   =     ipoids_vector(iboat)
 nsolm   =      nsolm_vector(iboat)
nsol_fatigue     =  nsol_fatigue_vector(iboat)
nsolm_fatigue     =  nsolm_fatigue_vector(iboat)
  nsol   =       nsol_vector(iboat)
    ne   =         ne_vector(iboat)
  imax   =       imax_vector(iboat)
  jlph   =       jlph_vector(iboat)
 jlph2   =      jlph2_vector(iboat)
iprint   =     iprint_vector(iboat)
              
 depth   =      depth_vector(iboat)
length   =     length_vector(iboat)
 width   =      width_vector(iboat)

part_full        =  part_full_vector(iboat)
method_fatigue   =     method_fatigue_vector(iboat)
tfl              =        tfl_vector(iboat)
              
dis(:)=      dis_vector(:,iboat)
fam(:)=      fam_vector(:,iboat)

allocate(casename(nsolm))
allocate(is_loadcase_fatigue(nsolm))

if (nsol_fatigue.gt.0) allocate (NNSOL_BV_TYPE(nsol_fatigue))!fat_new

allocate(tirant(nsolm))
allocate(is_selected_loadcase_fatigue(nsol))

do i=1,nsolm
              casename(i)  =            casename_vector(i,iboat)
   is_loadcase_fatigue(i)  = is_loadcase_fatigue_vector(i,iboat)
                tirant(i)  =              tirant_vector(i,iboat)
enddo

do i=1,nsol_fatigue		!fat_new
   if (nsol_fatigue.gt.0) NNSOL_BV_TYPE(i)=NNSOL_BV_TYPE_vector(i,iboat)	!fat_new		!new4
enddo

do i=1,nsol		!fat_new

   is_selected_loadcase_fatigue(i)=is_selected_loadcase_fatigue_vector(i,iboat)
enddo



icout     = icout_vector   (iboat)
iredress  = iredress_vector(iboat)
refthick_input = refthick_input_vector(iboat)

lcc2	  = lcc2_vector(iboat)
lcc3	  = lcc3_vector(iboat)
lcc4	  =lcc4_vector(iboat)
lcc5	  =lcc5_vector(iboat)
lightweight = lightweight_vector(iboat)
lightweight_init = lightweight_init_vector(iboat)
deadweight = deadweight_vector(iboat)
deadweight_init = deadweight_init_vector(iboat)
poidsLBR5_init = poidsLBR5_init_vector(iboat)
lccscenario = lccscenario_vector(iboat)

poids     = poids_vector   (iboat)
spoids    = spoids_vector  (iboat)

fmat11    = fmat11_vector  (iboat)
fmat22    = fmat22_vector  (iboat)
fmat33    = fmat33_vector  (iboat)
fsou11    = fsou11_vector  (iboat)
fsou22    = fsou22_vector  (iboat)
fsou33    = fsou33_vector  (iboat)
fmdo11    = fmdo11_vector  (iboat)
fmdo22    = fmdo22_vector  (iboat)
fmdo33    = fmdo33_vector  (iboat)
fmdo44    = fmdo44_vector  (iboat)
fmdo55    = fmdo55_vector  (iboat)
fmdo66    = fmdo66_vector  (iboat)
fmdo77    = fmdo77_vector  (iboat)

rend_global     = rend_global_vector   (iboat)
!eqp_global      =  eqp_global_vector   (iboat)

nbrMat   =       nbrMat_vector(iboat)

call allocate_param_materiau()

call get_param_materiau()


imulti    =      imulti_vector(iboat)  
w1		  =		 w1_vector    (iboat)  			
w2        =      w2_vector    (iboat)  
w3        =      w3_vector    (iboat)  
rho       =      rho_vector   (iboat)  

do i=1,3
   fk(i) =    fk_vector(i,iboat)
enddo

ipareto      =   ipareto_vector    (iboat)
nsteppar     =   nsteppar_vector   (iboat)
iparcout     =   iparcout_vector   (iboat)
iparpoids    =   iparpoids_vector  (iboat)
iparinertia  =   iparinertia_vector(iboat)

return
end
