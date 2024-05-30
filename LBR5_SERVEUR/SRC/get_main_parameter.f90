subroutine get_main_parameter()

use param_section
use PARAM_SECTION_VECTOR

implicit double precision (a-h,o-z)

section_file=section_files(iboat)
open(iu_10(iboat),file=section_file,status='old',err=900)

read (iu_10(iboat),'(a80)') !header1
read (iu_10(iboat),*)       !iana			 
read (iu_10(iboat),'(a80)') !header2
read (iu_10(iboat),*)   impr,impr2,indaig,indrai,dessin,jlph2,ibusc,neto

neto_vector(iboat) = neto
ngmax_vector(iboat) = ngmax

read(iu_10(iboat),*) !imulti,rho,w1,w2,w3,fk(1),fk(2),fk(3)	
read(iu_10(iboat),*) !ipareto, nsteppar, iparcout, iparpoids, iparinertia
read(iu_10(iboat),*) !lcc2,lcc3,lcc4,lcc5,lightweight_init,deadweight_init,lccscenario
read(iu_10(iboat),*) !IOPTI
read(iu_10(iboat),*) !icout             
read(iu_10(iboat),*) !iredress            
read(iu_10(iboat),*) !rend_global,eqp_global  
read(iu_10(iboat),*) !refthick_input		  
read(iu_10(iboat),*) nbrMat

nbrMat_vector(iboat) = nbrMat

rewind(iu_10(iboat))
close(iu_10(iboat))

return                                                                           

900 write(*,'(a)') 'Fichier d''input .txt manquant !!!'
read(*,*)
stop
           
end
