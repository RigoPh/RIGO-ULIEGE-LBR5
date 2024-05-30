subroutine comp_obj()

use param_section
use PARAM_SECTION_VECTOR


implicit double precision (a-h,o-z)

! ====================================================================================================
! 4.0  fct objectif et restrictions (géométriques, structurelles et particulières)
! ====================================================================================================
if (iboat.gt.1) then
    jj=sum(ntot(1:iboat-1))
   shift_opt(iboat)=jj
   else
   shift_opt(iboat)=0
endif

! 4.1 fonction objectif
!     =================

obj     (iboat)=0.d+00
objmulti(iboat)=0.d+00					

!fiopt  (shift_opt(iboat)+1:shift_opt(iboat)+ntot(iboat))=0.d+00
!fimulti(shift_opt(iboat)+1:shift_opt(iboat)+ntot(iboat))=0.d+00
fiopt  (:,iboat)=0.d+00
fimulti(:,iboat)=0.d+00

ncont=0
fact =0						

if (idebug_conlin.eq.1) then
write(iu_conlin(iboat),*)  'objective function before'
write(iu_conlin(iboat),*)  'obj    =',obj(iboat)
write(iu_conlin(iboat),*)  'fiopt  =',fiopt(1:ntot(iboat),iboat)	
write(iu_conlin(iboat),*)  'ntot   =',ntot(iboat)	
write(iu_conlin(iboat),*)
endif

!if (nfile.le.1) then !pas de raison qu'on ne puisse pas a priori
! TODO Théoriquement, il faudrait ptêt additionner les fonctions objectifs respectives
! avant de les modifier (avec rho etc), mais a priori ça ne devrait pas changer gd chose
! à l'optimum si on additionne plutôt les "fact" respectifs. A vérifier

if(imulti.eq.1) then

	! Test des valeurs de pondération
	if (((w1+w2+w3).lt.0.99).and.((w1+w2+w3).gt.1.01)) then
		if (langue.eq.1) then 
			write(*,*) 'Attention ! La somme des facteurs de ponderation ne vaut pas 1 !'
			write(*,*) 'Veuillez corriger vos donnees - appuyer sur ENTER pour poursuivre'
		elseif (langue.eq.2) then 
			write(*,*) 'Attention! The sum of weight factors is not equal to 1!'
			write(*,*) 'Please correct your data - press ENTER to resume'
		endif
		read(*,*)
		stop
	endif
	!

	if(w3.ne.0) then
		call objin1(objmulti(iboat),fimulti(1:ntot(iboat),iboat),1,1)		
		if (fk_vector(3,iboat).gt.objmulti(iboat)) then
			fact=fact+(w3*(fk_vector(3,iboat)-objmulti(iboat))/fk_vector(3,iboat))**rho
		else
			fact=fact+(w3*(objmulti(iboat)-fk_vector(3,iboat))/fk_vector(3,iboat))**rho
		endif

		!fact=fact+(w3*fk(3)/objmulti(iboat))**rho
   	    do i=1,ntot(iboat)
			if (fk_vector(3,iboat).gt.objmulti(iboat)) then
				fiopt(i,iboat)=fiopt(i,iboat)+rho*(w3*(fk_vector(3,iboat)-objmulti(iboat))/fk_vector(3,iboat))**(rho-1.)*w3/fk_vector(3,iboat)*fimulti(i,iboat)
			else
				fiopt(i,iboat)=fiopt(i,iboat)+rho*(w3*(objmulti(iboat)-fk_vector(3,iboat))/fk_vector(3,iboat))**(rho-1.)*w3/fk_vector(3,iboat)*(-1)*fimulti(i,iboat)
			endif
   			!fiopt(i,iboat)=fiopt(i,iboat)+rho*(w3*fk(3)/objmulti(iboat))**(rho-1.) *	&
   	        !               w3*fk(3)*(-1.)/(objmulti(iboat)**2.)*(-fimulti(i,iboat))
							! rem.- normal, car fimulti est la dérivée de inertie au signe près
							! TODO à changer (donc ds coord aussi) car ça prête à confusion
   	    enddo
		write(*,*) 'Inertie :',objmulti(iboat)
   	    objmulti(iboat)=0.
   	    call annuld(fimulti(1:ntot(iboat),iboat),ntot(iboat))
!   	    rewind iu_25(iboat)									
	endif														
   
    if(w2.ne.0) then		
		call objpd1(objmulti(iboat),fimulti(1:ntot(iboat),iboat),1,1)
		if (fk_vector(2,iboat).gt.objmulti(iboat)) then							
			fact=fact+(w2*(fk_vector(2,iboat) - objmulti(iboat))/fk_vector(2,iboat))**rho
		else
			fact=fact+(w2*(objmulti(iboat)-fk_vector(2,iboat))/fk_vector(2,iboat))**rho
		endif

        do i=1,ntot(iboat)
			if (fk_vector(2,iboat).gt.objmulti(iboat)) then
				fiopt(i,iboat)=fiopt(i,iboat)+rho*(w2*(fk_vector(2,iboat)-objmulti(iboat))/fk_vector(2,iboat))**(rho-1.)*w2/fk_vector(2,iboat)*(-1)*fimulti(i,iboat)
			else
				fiopt(i,iboat)=fiopt(i,iboat)+rho*(w2*(objmulti(iboat)-fk_vector(2,iboat))/fk_vector(2,iboat))**(rho-1.)*w2/fk_vector(2,iboat)*fimulti(i,iboat)
			endif
        enddo
		write(*,*) 'Poids :',objmulti(iboat)
        objmulti(iboat)=0.
        call annuld(fimulti(1:ntot(iboat),iboat),ntot(iboat))
        ncont=0
!        rewind iu_25(iboat)
    endif
      
    if(w1.ne.0) then
		if(icout.eq.1) then			
			call objct1(objmulti(iboat),fimulti(1:ntot(iboat),iboat),1,1)
			write(*,*) 'Cout :',objmulti(iboat)
			if (fk_vector(1,iboat).gt.objmulti(iboat)) then
				fact=fact+(w1*(fk_vector(1,iboat)-objmulti(iboat))/fk_vector(1,iboat))**rho
			else
				fact=fact+(w1*(objmulti(iboat)-fk_vector(1,iboat))/fk_vector(1,iboat))**rho
			endif
            do i=1,ntot(iboat)
				if (fk_vector(1,iboat).gt.objmulti(iboat)) then					
					fiopt(i,iboat)=fiopt(i,iboat)+rho*(w1*(fk_vector(1,iboat)-objmulti(iboat))/fk_vector(1,iboat))**(rho-1.)*w1/fk_vector(1,iboat)*(-1)*fimulti(i,iboat)
				else
					fiopt(i,iboat)=fiopt(i,iboat)+rho*(w1*(objmulti(iboat)-fk_vector(1,iboat))/fk_vector(1,iboat))**(rho-1.)*w1/fk_vector(1,iboat)*fimulti(i,iboat)
				endif
            enddo													
        elseif(icout.gt.1) then			
			call sensibcout(objmulti(iboat),fimulti(1:ntot(iboat),iboat),1,1)
            if (fk_vector(1,iboat).gt.objmulti(iboat)) then
				fact=fact+(w1*(fk_vector(1,iboat)-objmulti(iboat))/fk_vector(1,iboat))**rho
			else
				fact=fact+(w1*(objmulti(iboat)-fk_vector(1,iboat))/fk_vector(1,iboat))**rho
			endif
            do i=1,ntot(iboat)
				if (fk_vector(1,iboat).gt.objmulti(iboat)) then
					fiopt(i,iboat)=fiopt(i,iboat)+rho*(w1*(fk_vector(1,iboat)-objmulti(iboat))/fk_vector(1,iboat))**(rho-1.)*w1/fk_vector(1,iboat)*(-1)*fimulti(i,iboat)
				else
					fiopt(i,iboat)=fiopt(i,iboat)+rho*(w1*(objmulti(iboat)-fk_vector(1,iboat))/fk_vector(1,iboat))**(rho-1.)*w1/fk_vector(1,iboat)*fimulti(i,iboat)
				endif
            enddo														
		else
			if (langue.eq.1) then 
				write(*,*) 'Attention ! Cout selectionne en multiobjectif, mais le parametre icout n''est pas clairement defini/compatible.'
				write(*,*) 'Veuillez corriger vos donnees - appuyer sur ENTER pour poursuivre'
			elseif (langue.eq.2) then
				write(*,*) 'Attention! Cost selected for multi-objective, but the cost parameter is not clearly defined.'
				write(*,*) 'Please correct your data - press ENTER to resume'
			endif
			read(*,*)
			stop
        endif	
    endif																
   
    obj(iboat)=fact**(1./rho)									
    do i=1,ntot(iboat)														
		fiopt(i,iboat)=(1./rho)*fact**((1.-rho)/rho)*fiopt(i,iboat)					
    enddo																
   
    !Ecriture des autres fct obj, pour information
    if (w3.eq.0) then
	  temp=0.d00
	  call objin1(temp,fimulti(1:ntot(iboat),iboat),0,1) !pour afficher l'inertie qd même
    endif 
    if (w2.eq.0) then
	  temp=0.d00
	  call objpd1(temp,fimulti(1:ntot(iboat),iboat),0,1) !pour afficher le pds qd même
    endif
    if (w1.eq.0) then
	  temp=0.d00
	  if(icout.eq.1) then
		call objct1(temp,fimulti(1:ntot(iboat),iboat),0,1) !pour afficher le cout qd même
	  elseif(icout.gt.1) then
		call sensibcout(temp,fimulti(1:ntot(iboat),iboat),0,1) !pour afficher le cout qd même 
	  endif
    endif

    if (idebug_conlin.eq.1) then

		write(iu_conlin(iboat),*)  'fact  =',fact
		write(iu_conlin(iboat),*)  'fiopt  =',fiopt  (1:ntot(iboat),iboat)	
		write(iu_conlin(iboat),*)  'fimulti=',fimulti(1:ntot(iboat),iboat)
	endif

    if(iprint.ge.1) write(*,54) obj(iboat)
   
    if (impr2.ge.0) then		
         write (iu_31(iboat),*)'dérivées fct obj. n variables (dér. non modifiées)' 
         write (iu_31(iboat),16) (fiopt(i,iboat),i=1,ntot(iboat))
    endif						
   

    if (idebug_conlin.eq.1) then

		write(iu_conlin(iboat),*)  'objective function after'
		write(iu_conlin(iboat),*)  'obj    =',obj    (iboat)
		write(iu_conlin(iboat),*)  'fiopt  =',fiopt  (1:ntot(iboat),iboat)
		write(iu_conlin(iboat),*)  'ntot   =',ntot(iboat)	
		write(iu_conlin(iboat),*)
   	endif													
   
else !imulti=0

	if(icout.eq.(-1)) then							
		call objin1(obj(iboat),fiopt(1:ntot(iboat),iboat),1,1)	
		!do i=1,ntot(iboat)
		!	if (fiopt(i,iboat).eq.0) fiopt(i,iboat) = 10e-10 !car prob si = 0
		!enddo
		!Impression des autres fcts objectifs à titre informatif
		temp=0.d00
		call objpd1(temp,fimulti(1:ntot(iboat),iboat),0,1) !pour afficher le poids qd même
		! A corriger car là on ne met que le coût sophistiqué
		temp=0.d00
		!!! sensibcout uniquement si icout.eq.2 (sinon ça plante) call sensibcout(temp,fimulti(1:ntot(iboat),iboat),0,1) !pour afficher le coût qd même
	endif																
	
	if(icout.eq.(-2)) then
		call objlcc(obj(iboat),fiopt(1:ntot(iboat),iboat),1,1) 
	   ! 1 -> calcul des dérivées ; iprint
	endif

	if(icout.eq.0) then
		call objpd1(obj(iboat),fiopt(1:ntot(iboat),iboat),1,1)
		!Impression des autres fcts objectifs à titre informatif
		temp=0.d00
		call objin1(temp,fimulti(1:ntot(iboat),iboat),0,1) !pour afficher l'inertie qd même
		! A corriger car là on ne met que le coût sophistiqué
		temp=0.d00
		!!! sensibcout uniquement si icout.eq.2 (sinon ça plante) call sensibcout(temp,fimulti(1:ntot(iboat),iboat),0,1) !pour afficher le coût qd même
	endif

	if ((icout.gt.0).and.(iredress.eq.1)) then                        
		write(iu_redress(iboat),*) 'iteration  ',itera                               
		write(iu_redress(iboat),*) '------------------'                              
		write(iu_redress(iboat),*) '   '                                             
		write(iu_redress(iboat),*) 'panneau, ','redressage(h/m²) ','redressage(euro) ','poidslot'

		call spepite(totalstrait,derredress,1,1)
		obj=obj + totalstrait
	endif  

	if(icout.eq.1) then		

    ! TODO Ces write à supprimer normalement
    ! voir avec Eugen si ça pose problème
		write(iu_31(iboat),*) 
		write(iu_31(iboat),*)'*** cost objective function (subr. objct1)'
		write(iu_31(iboat),*)'    --------------------------------------'
		write(iu_31(iboat),*)'panneau/panel (with additional corrosion thickness)'

		call objct1(obj(iboat),fiopt(1:ntot(iboat),iboat),1,1)   	
	endif

	if (icout.gt.1) then                                          
		call sensibcout(obj(iboat),fiopt(1:ntot(iboat),iboat),1,1)
		!Impression des autres fcts objectifs à titre informatif
		temp=0.d00
		call objpd1(temp,fimulti(1:ntot(iboat),iboat),0,1) !pour afficher le poids qd même
		! A corriger car là on ne met que le coût sophistiqué
		temp=0.d00
		call objin1(temp,fimulti(1:ntot(iboat),iboat),0,1) !pour afficher l'inertie qd même
	endif                                                           

	if ((icout.gt.0).and.(iredress.eq.1)) then                        

		obj_temp = obj(iboat) - totalstrait     !dad
		write(iu_redress(iboat),*) 'cout sans redress =',obj_temp                    
		write(iu_redress(iboat),*) 'cout total=',obj(iboat)		                          
								      
	    do i=1,ntot(iboat)   !!!!!!!!!!!   do i=1,nvmax		
			!fiopt_temp=fiopt(shift_opt(iboat)+i)                                                 
			fiopt(i,iboat)=fiopt(i,iboat)+derredress(i)		    					  
			!write(7779,*) i,fiopt_temp,derredress(i),fiopt(i)                   
		enddo															  
	endif

endif !end of if imulti.1	
   

!endif

if(iprint.ge.1) write(*,54) obj(iboat)

if (impr2.ge.0) then		
    write (iu_31(iboat),*)'dérivées fct obj. n variables (dér. non modifiées)' 
    write (iu_31(iboat),16) (fiopt(i,iboat),i=1,ntot(iboat))
endif						

if (idebug_conlin.eq.1) then
	write(iu_conlin(iboat),*)  'objective function after'
	write(iu_conlin(iboat),*)  'obj    =',obj(iboat)
	write(iu_conlin(iboat),*)  'fiopt  =',fiopt(1:ntot(iboat),iboat)
	write(iu_conlin(iboat),*)  'ntot   =',ntot(iboat)	
	write(iu_conlin(iboat),*)
endif


return



! ====================================================================================================
! 9.0 les formats
! ====================================================================================================
16 format('d fct /dx =  ',(t14,9(e10.3,1x)))
54 format('fct. objectif = ',e14.7/)

end
