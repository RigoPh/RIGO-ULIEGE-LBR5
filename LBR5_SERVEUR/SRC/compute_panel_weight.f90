subroutine compute_panel_weight(nel)

use param_section

implicit double precision (a-h,o-z)


! 4.5 poids du panneau et de la structure
!     ---------------------------------
pot=pot+poids9                 ! pot= poids total struct par mètre

! 4.6 action du poids propre (poids=poids panneau par mètre)
!     ----------------------------------------------------

if(ipoids.eq.0)goto 533
write(iu_11(iboat),534) poids9
if(ipa(nel).eq.1) then                               
   poids9=poids9*(spec(indMateriau(nel))-9.81d03)/spec(indMateriau(nel))         
	 do k=1,9                                          
	    dpoids(k)=dpoids(k)*(spec(indMateriau(nel))-9.81d03)/spec(indMateriau(nel)) 
	 enddo                                             
endif        
                                       
do is=1,nsol
   if((panneau(nel).icha).eq.1) then
      do i=1,npt(is,nel)
         cha(i,1,is,nel)=cha(i,1,is,nel)+poids9     
		     do k=1,9                                 
		        dcha(i,is,k,nel)=dcha(i,is,k,nel)+dpoids(k)		 
		     enddo                                    
      enddo
   else
      npt(is,nel)=1
      cha(1,1,is,nel)=poids9
      cha(1,2,is,nel)=0.
      cha(1,3,is,nel)=0.
	      do k=1,9                          
	        dcha(1,is,k,nel)=dpoids(k)					
	      enddo
   endif
enddo
533  continue


return 

! 6.  les formats.
!     ============
534  format(/'le poids propre par metre (selon ox) est de ',e14.7,' n/m'/)

end
