subroutine comp_obj_global()

use param_section
use PARAM_SECTION_VECTOR

implicit double precision (a-h,o-z)

! ====================================================================================================
! 4.0  fct objectif et restrictions (géométriques, structurelles et particulières)
! ====================================================================================================

! 4.1 fonction objectif
!     =================

obj_global       =0.0d+00
objmulti_global  =0.0d+00					

fiopt_global  (1:ntot_global)=0.0d+00
fimulti_global(1:ntot_global)=0.0d+00

ncont=0
fact =0						

if (idebug.eq.1) then
	write(iu_conlin_global,*)  'objective function before'
	write(iu_conlin_global,*)  'obj    =',obj_global
	write(iu_conlin_global,*)  'fiopt  =',fiopt_global(1:ntot_global)	
	write(iu_conlin_global,*)  'ntot   =',ntot_global
	write(iu_conlin_global,*)
endif

if (nfile.le.1) then

   if(imulti.eq.1) then
      if(w3.ne.0) then
         call objin1(objmulti_global,fimulti_global(1:ntot_global),1,1)
   	     fact=fact+(w3*fk(3)/objmulti_global)**rho
   	     do i=1,ntot_global
   	        fiopt_global(i)=fiopt_global(i)+rho*(w3*fk(3)/objmulti_global)**(rho-1.) *	        &
   	                            w3*fk(3)*(-1.)/(objmulti_global**2.)*(-fimulti_global(i))
   	     enddo
   	     objmulti_global=0.0d+00
   	     call annuld(fimulti_global(1:ntot_global),ntot_global)								
      endif														
   
      if(w2.ne.0) then
		 call objpd1(objmulti_global,fimulti_global(1:ntot_global),1,1)															
         fact=fact+(w2*objmulti_global/fk(2))**rho								
         do i=1,ntot_global
            fiopt_global(i)=fiopt_global(i)+rho*(w2*objmulti_global/fk(2))**(rho-1.)*w2/fk(2)*fimulti_global(i)
         enddo
         objmulti_global=0.0d+00
         call annuld(fimulti_global(1:ntot_global),ntot_global)
         ncont=0
      endif
      
      if(w1.ne.0) then
         if(icout.eq.1) then
			call objct1(objmulti_global,fimulti_global(1:ntot_global),1,1)													
            fact=fact+(w1*objmulti_global/fk(1))**rho
            do i=1,ntot_global									
               fiopt_global(i)=fiopt_global(i)+rho*(w1*objmulti_global/fk(1))**(rho-1.)*w1/fk(1)*fimulti_global(i)	
            enddo													
         endif														
   
         if(icout.gt.1) then							
            call sensibcout(objmulti_global,fimulti_global(1:ntot_global),1,1)
            fact=fact+(w1*objmulti_global/fk(1))**rho						
            do i=1,ntot_global														
               fiopt_global(i)=fiopt_global(i)+rho*(w1*objmulti_global/fk(1))**(rho-1.)*w1/fk(1)*fimulti_global(i)									
            enddo														
         endif															
      endif																
   
      obj_global=fact**(1./rho)									
      do i=1,ntot_global															
         fiopt_global(i)=(1./rho)*fact**((1.-rho)/rho)*fiopt_global(i)					
      enddo																
   
	  if (idebug.eq.1) then
		write(iu_conlin_global,*)  'fact  =',fact
		write(iu_conlin_global,*)  'fiopt  =',fiopt_global  (1:ntot_global)	
		write(iu_conlin_global,*)  'fimulti=',fimulti_global(1:ntot_global)
      endif
   
   
      if(iprint.ge.1) write(*,54) obj_global
   
!      if (impr2.ge.0) then		
!         write (iu_31(1),*)'dérivées fct obj. n variables (dér. non modifiées)' 
!         write (iu_31(1),16) (fiopt_global(i),i=1,ntot_global)
!      endif						
   
	  if (idebug.eq.1) then
		write(iu_conlin_global,*)  'objective function after'
		write(iu_conlin_global,*)  'obj    =',obj_global
		write(iu_conlin_global,*)  'fiopt  =',fiopt_global  (1:ntot_global)
		write(iu_conlin_global,*)  'ntot   =',ntot_global	
		write(iu_conlin_global,*)
   	  endif													
   
   endif		
   

endif

														
if(iprint.ge.1) write(*,54) obj_global

!if (impr2.ge.0) then		
!    write (iu_31(1),*)'dérivées fct obj. n variables (dér. non modifiées)' 
!    write (iu_31(1),16) (fiopt_global(i),i=1,ntot_global)
!endif						

if (idebug.eq.1) then
	write(iu_conlin_global,*)  'objective function after'
	write(iu_conlin_global,*)  'obj    =',obj_global
	write(iu_conlin_global,*)  'fiopt  =',fiopt_global(1:ntot_global)
	write(iu_conlin_global,*)  'ntot   =',ntot_global	
	write(iu_conlin_global,*)
endif

return

! ====================================================================================================
! 9.0 les formats
! ====================================================================================================
16 format('d fct /dx =  ',(t14,9(e10.3,1x)))
54 format('fct. objectif = ',e14.7/)

end

