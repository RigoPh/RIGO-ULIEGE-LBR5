subroutine recompute_objective_multisection()

use param_section
use PARAM_SECTION_VECTOR

implicit double precision (a-h,o-z)

pi1=pi/180.d00

call get_section_header_vector()                       
call allocate_param_section()	
call allocate_panel_data()
call get_panel_data_global()
call get_optim_data()


!  ====================================================================
! 8.0 calcul de la fct objectif avec les nouveaux xi c.à.d. s(i).
!  ====================================================================

obj2(iboat)=0.d00
obj2multi(iboat)=0.
fact=0.		
!rewind(iu_25(iboat))		
nc=1
    
if(itera.eq.iteram) then
   itt=1
   write(iu_31(iboat),63)
else
   itt=0
endif
 
      !write(iu_conlin(iboat),*) 'itt    =',itt
      !write(iu_conlin(iboat),*) 'fact   =',fact
      !write(iu_conlin(iboat),*) 'nc     =',nc
      !write(iu_conlin(iboat),*) 'itype  =',itype
      !write(iu_conlin(iboat),*) 'spec   =',spec
      !write(iu_conlin(iboat),*) 'isect  =',isect
      !write(iu_conlin(iboat),*) 'ntot   =',ntot(iboat)
      !write(iu_conlin(iboat),*) 'nbrxi  =',nbrxi
      !write(iu_conlin(iboat),*) 'obj2   =',obj2(iboat)
      !write(iu_conlin(iboat),*) 'spec   =',spec
      !write(iu_conlin(iboat),*) 'itype  =',itype
      !write(iu_conlin(iboat),*) 'isect  =',isect

      !write(iu_conlin(iboat),*) 'nxit   ='

      !do i=1,9
         !write(iu_conlin(iboat),*)  nxit(i,1:nmax,iboat)
      !enddo

      !write(iu_conlin(iboat),*) 'width  =',width
      !write(iu_conlin(iboat),*) 'iprint =',iprint


63 format(/' resultat final (gross thicknesses) :'/20(1h*)) 

!  ====================================================================
!  multiobj
!  ====================================================================

	
if(imulti.eq.1) then												

   call ecriVar(xicou(:,iboat),itt) ! TODO 1 fois pour tous normalement
   

   !Ecriture des vrais fcts objectif
   if(w3.ne.0) then		
      !call ecriVar(sopt,itt) ! TODO 1 fois pour tous normalement   										
      call objin1(obj2multi(iboat),fimulti(1:ntot(iboat),iboat),0,0)

	  fact=fact+(w3*fk(3)/obj2multi(iboat))**rho		
	  write (iu_31(iboat),64) obj2multi(iboat)
	  obj2multi(iboat)=0.d+00	  									

   endif														
  
   if(w2.ne.0) then							
      !call ecriVar(sopt,itt) ! TODO 1 fois pour tous normalement   										
	  call objpd1(obj2multi(iboat),fimulti(1:ntot(iboat),iboat),0,0)					

	  fact=fact+(w2*obj2multi(iboat)/fk(2))**rho			
	  write (iu_31(iboat),62) obj2multi(iboat)
	  obj2multi(iboat)=0.d+00
	  
	  nc=1											

   endif															
	  
   if(w1.ne.0) then								
	  if(icout.eq.1) then							
      !call ecriVar(sopt,itt) ! TODO 1 fois pour tous normalement   										
	  call objct1(obj2multi(iboat),fimulti(1:ntot(iboat),iboat),0,0)					

		 fact=fact+(w1*obj2multi(iboat)/fk(1))**rho	
		 write (iu_31(iboat),60) obj2multi(iboat)
							
	  endif																

	  if(icout.gt.1) then
		 call sensibcout(obj2multi(iboat),fimulti(1:ntot(iboat),iboat),0,0)
	     !call sensibcout2(obj2multi(iboat),xicou(1:nmax,iboat),itt,ntot(iboat))	
		 fact=fact+(w1*obj2multi(iboat)/fk(1))**rho								
		 write (iu_31(iboat),60) obj2multi(iboat)
	  endif											
	
   endif

   obj2(iboat)=fact**(1./rho)					
   
   !Ecriture des autres fct obj, pour information
   if (w3.eq.0) then
	  obj2multi(iboat)=0.d+00
	  call objin1(obj2multi(iboat),fimulti(1:ntot(iboat),iboat),0,0)
	  write (iu_31(iboat),64) obj2multi(iboat)
	  obj2multi(iboat)=0.d+00
   endif
   
   if (w2.eq.0) then
   	  obj2multi(iboat)=0.d+00
	  call objpd1(obj2multi(iboat),fimulti(1:ntot(iboat),iboat),0,0)
	  write (iu_31(iboat),62) obj2multi(iboat)
      obj2multi(iboat)=0.d+00
   endif

   if (w1.eq.0) then
	  obj2multi(iboat)=0.d+00
	  if(icout.eq.1) then
		 call objct1(obj2multi(iboat),fimulti(1:ntot(iboat),iboat),0,0)
	  elseif(icout.gt.1) then
		  call sensibcout(obj2multi(iboat),fimulti(1:ntot(iboat),iboat),0,0)
	  endif
	  write (iu_31(iboat),60) obj2multi(iboat)
	  obj2multi(iboat)=0.d+00
   endif
   
   goto 31												

endif																


call ecriVar(xicou(:,iboat),itt)

if(icout.eq.(-1)) then	
   !inertie
   !call ecriVar(sopt,itt) ! TODO 1 fois pour tous normalement
   call objin1(obj2(iboat),fiopt(1:ntot(iboat),iboat),0,0)
endif

if(icout.eq.(-2)) then !Life Cycle Cost
   !call ecriVar(sopt,itt) ! TODO 1 fois pour tous normalement
   call objlcc(obj2(iboat),fiopt(1:ntot(iboat),iboat),0,0) ! 0-> pas de calcul des dérivées
endif
	
if(icout.eq.(0)) then !poids
   !call ecriVar(sopt,itt) ! TODO 1 fois pour tous normalement
   call objpd1(obj2(iboat),fiopt(1:ntot(iboat),iboat),0,0) ! 1 -> calcul des dérivées ; iprint
endif


if (icout.eq.1) then
   !call ecriVar(sopt,itt) ! TODO 1 fois pour tous normalement
   call objct1(obj2(iboat),fiopt(1:ntot(iboat),iboat),0,0) ! 0-> pas de calcul des dérivées
endif


if (icout.gt.1) then                                    
   call sensibcout(obj2(iboat),fiopt(1:ntot(iboat),iboat),0,0)
   !call sensibcout2(obj2(iboat),xicou(1:nmax,iboat),itt,ntot(iboat))
endif                                                       


if ((icout.gt.0).and.(iredress.eq.1)) then
   !call ecriVar(sopt,itt) ! TODO 1 fois pour tous normalement
   call spepite(totalstrait,fiopt(1:ntot(iboat),iboat),0,0) ! 0-> pas de calcul des dérivées
   obj2(iboat) = obj2(iboat) + totalstrait
endif														   

      !write(iu_conlin(iboat),*) 'obj2   =',obj2(iboat)

31 if (imulti.eq.1) then				

      write (iu_31(iboat),65)  obj2(iboat)			
      write (iu_12(iboat),165) obj2(iboat)	
      write (*,  65) obj2(iboat)			
      write (iu_11(iboat),165) obj2(iboat)
else						

   if(icout.ge.1) then  
      write (iu_31(iboat),60)  obj2(iboat)
      write (iu_12(iboat),160) obj2(iboat)					
      write (iu_11(iboat),160) obj2(iboat)					
      write (*,  60) obj2(iboat)
   endif

   if(icout.eq.0) then
      write (iu_31(iboat),62)  obj2(iboat)
      write (iu_12(iboat),162) obj2(iboat)					
      write (*,  62) obj2(iboat)
      write (iu_11(iboat),162) obj2(iboat)					
   endif

   if(icout.eq.(-1)) then					
      write (iu_31(iboat),64) obj2(iboat)
      write (iu_12(iboat),164) obj2(iboat)					
      write (*,  64) obj2(iboat)
      write (iu_11(iboat),164) obj2(iboat)					
   endif


endif							

!write(iu_conlin(iboat),*) 'obj2   =',obj2(iboat)

!  ====================================================================


call put_optim_data()

call deallocate_panel_data()
call deallocate_param_section()	
call deallocate_section_header()
call deallocate_param_materiau()
call rewind_all()


return

900 write(iu_14(iboat),*)' eof lors de la lecture de conlin.dat (subr opti)'
    write(*,*) 'stop'
    read(*,*)
    stop


!  ====================================================================
! 9.0 les formats
!  ====================================================================

! Rem. : ne pas changer format !!! Car lu par l'interface d'Eugen !!!

60  format(/'fct objectif cout(recalculée)    = ',e14.7, 'euro or $'/)
62  format(/'fct objectif poids (recalculée)    = ',e14.7, 'n'/)
64  format(/'fct objectif inertie (recalculée)    = ',e14.7, 'm**4'/)		
65  format(/'fct objectif combinée (recalculée)    = ',e14.7/)				

160 format(/'fct objectif cout(après optimisation)= ',e14.7, 'euro'/)
162 format(/'fct objectif poids (après optimisation)= ',e14.7, 'n'/)
164 format(/'fct objectif inertie (après optimisation)= ',e14.7, 'm**4'/)
165 format(/'fct objectif combinée (après optimisation)= ',e14.7/)		
    
  
  
end
