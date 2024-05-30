subroutine read_write_panel()

use param_section

implicit double precision (a-h,o-z)

! =============================================================================
!  introduction des donnees relatives a chaque panneau,
!  ecriture des donnees,calcul des coefficients de raideur du panneau,
!  introduction du poids propre sur les panneaux horizontaux.
! =============================================================================

do nel=1,neto

   !call init_panel_data(nel)
!  ne pas changer d'endroit !!!!!!!!!

   panneau(nel).m1cont=0
   call read_panel_data(nel)
   call read_panel_restrictions(nel)
   call compute_save_panel_data(nel)

   ncont = ncont + nvar(nel,iboat)
!   if((panneau(nel).icha).eq.1) ichag=1   !  icha indice de charge variable selon ox
   
enddo

if(iff.eq.1)  then
   write(*,*)            'some errors in your data'
   write(iu_14(iboat),*) 'subroutine read_write_panel.f90'										
   write(iu_14(iboat),*) 'some errors in your data'								
   write(*,*) 'stop'
   read(*,*)
   stop
endif 

! fichier avec liste des restriction cjmax, lcont, inv 
!if(iopti.ge.1) rewind(iu_32(iboat))  
pot=pot*width+spoids1			

if(langue==1) write(iu_11(iboat),405) pot
if(langue==1) write(iu_12(iboat),405) pot	

if(langue==2) write(iu_11(iboat),'(/2a,e11.4,2a/)') &
'total weight of the structure = ',pot,             &
'(if ipa=1, reduced specific weight is ',           &
'considered due to buoyancy force)'

return

 405 format(/' le poids total de la structure est = ',e14.7,' n.',/		 & 		                                   
       	     'ce poids reprend la structure principale, secondaire,'     &                                       
     		 'les traverses et la corrosion.'/					         &                                       
     		 'avant itération d''optimisation.'/				         &                                       
             ' (! 1/2 structure si symétrique et structure déjaugée'	 &                                       
    		 'si ipa= 1)'/)														

end
