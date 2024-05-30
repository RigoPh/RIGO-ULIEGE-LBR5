subroutine put_section_header_vector()


use param_section
use PARAM_SECTION_VECTOR
!use param_cout_vector
!use param_cout
!use param_multiobj
!use param_multiobj_vector




    nom_vector(iboat)= nom
header1_vector(iboat)= header1
header2_vector(iboat)= header2

 iunit1_vector(iboat)= iunit1
 iunit2_vector(iboat)= iunit2
 iunit3_vector(iboat)= iunit3
 iunit4_vector(iboat)= iunit4
 iunit5_vector(iboat)= iunit5  
 iunit6_vector(iboat)= iunit6
 iunit7_vector(iboat)= iunit7
 iunit8_vector(iboat)= iunit8
 iunit9_vector(iboat)= iunit9
!iunit10_vector(iboat)= iunit10
!iunit11_vector(iboat)= iunit11
iunit12_vector(iboat)= iunit12

nnsol_vector(:,iboat)= nnsol(:)

   neto_vector(iboat)=   neto
  ngmax_vector(iboat)=   ngmax
   iana_vector(iboat)=   iana
 indaig_vector(iboat)= indaig
 indrai_vector(iboat)= indrai
  ibusc_vector(iboat)=  ibusc
 ipoids_vector(iboat)= ipoids
  nsolm_vector(iboat)=  nsolm
   nsol_fatigue_vector(iboat)=nsol_fatigue    
   nsolm_fatigue_vector(iboat)=nsolm_fatigue    

   nsol_vector(iboat)=   nsol
   nnsol_vector(:,iboat)=nnsol(:)
     ne_vector(iboat)=     ne
   imax_vector(iboat)=   imax
   jlph_vector(iboat)=   jlph
  jlph2_vector(iboat)=  jlph2
 iprint_vector(iboat)= iprint

  depth_vector(iboat)=  depth
 length_vector(iboat)= length
  width_vector(iboat)=  width

part_full_vector(iboat)=part_full 
   method_fatigue_vector(iboat)=method_fatigue
      tfl_vector(iboat)=tfl

  dis_vector(:,iboat)=    dis(:)
  fam_vector(:,iboat)=    fam(:)

do i=1,nsolm			!fat_new
               casename_vector(i,iboat)=            casename(i)
    is_loadcase_fatigue_vector(i,iboat)= is_loadcase_fatigue(i)
                 tirant_vector(i,iboat)=tirant(i)			!fat_new
enddo			!fat_new





do i=1,nsol_fatigue		!fat_new
                  NNSOL_BV_TYPE_vector(i,iboat)=NNSOL_BV_TYPE(i)		!fat_new
enddo
 
do i=1,nsol			!fat_new 
   is_selected_loadcase_fatigue_vector(i,iboat)=is_selected_loadcase_fatigue(i)
enddo			!fat_new

icout_vector(iboat)    =icout
iredress_vector(iboat) =iredress
refthick_input_vector(iboat) = refthick_input

lcc2_vector(iboat)		= lcc2
lcc3_vector(iboat)		= lcc3
lcc4_vector(iboat)		= lcc4
lcc5_vector(iboat)	    = lcc5
!lightweight_vector(iboat) =lightweight !pas encore calculé
lightweight_init_vector(iboat) = lightweight_init
!deadweight_vector(iboat) = deadweight !pas encore calculé
deadweight_init_vector(iboat) = deadweight_init
!poidsLBR5_init_vector(iboat) = poidsLBR5_init !pas encore calculé
lccscenario_vector(iboat) = lccscenario
if (itera.eq.0) then
	lightweight_vector(iboat) =lightweight_init
	deadweight_vector(iboat) = deadweight_init !pas encore calculé
	!call poidstotal(poidsLBR5_init,0,0)
	poidsLBR5_init_vector(iboat) = poidsLBR5_init !pas encore calculé
endif

poids_vector (iboat)=poids
spoids_vector(iboat)=spoids

fmat11_vector(iboat)=fmat11
fmat22_vector(iboat)=fmat22
fmat33_vector(iboat)=fmat33
fsou11_vector(iboat)=fsou11
fsou22_vector(iboat)=fsou22
fsou33_vector(iboat)=fsou33
fmdo11_vector(iboat)=fmdo11
fmdo22_vector(iboat)=fmdo22
fmdo33_vector(iboat)=fmdo33
fmdo44_vector(iboat)=fmdo44
fmdo55_vector(iboat)=fmdo55
fmdo66_vector(iboat)=fmdo66
fmdo77_vector(iboat)=fmdo77  

rend_global_vector (iboat)=rend_global
!eqp_global_vector (iboat)=eqp_global


nbrMat_vector(iboat)=   nbrMat


imulti_vector(iboat)  = imulti
w1_vector    (iboat)  =	w1						
w2_vector    (iboat)  = w2
w3_vector    (iboat)  = w3
rho_vector   (iboat)  = rho

do i=1,3
   fk_vector(i,iboat)= fk(i)
enddo


ipareto_vector(iboat)       =  ipareto
nsteppar_vector(iboat)      =  nsteppar
iparcout_vector(iboat)      =  iparcout
iparpoids_vector(iboat)     =  iparpoids
iparinertia_vector(iboat)   =  iparinertia

end
