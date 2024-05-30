	subroutine objlcc(obj_,derivee,iderivee,iprint_)

      

      use param_section
!      use param_cout
!      use param_opti_local
	

	implicit double precision (a-h,o-z)

	dimension derivee(ntot(iboat))


c	Calcul Fonction objectif
	
	call poidstotal(poids,0,0)
	call LCC(poids,obj_,codo,acof,rtm,edis)

	if (iprint_.ne.0) then
		write(iu_31(iboat),*)
		write(iu_31(iboat),*)' Life Cycle Cost. (subr.objlcc)'
		write(iu_31(iboat),*)' ------------------------------'
		write(iu_31(iboat),*)' Life Cycle Cost = ',obj_,' Euros'
	endif
	

c	Calcul Dérivées de la fonction objectif

	if (iderivee.eq.1) then

	k=0
	do 1 nel=1,neto
	  nbrxi=nvar(nel,iboat)

	  if (nbrxi.eq.0) goto 1

        if(itype(nel).ne.5) then
	  
		do i=1,nbrxi
	      j=nxit(i,nel,iboat)		! no de ref de chaque var. de conception

		  derivee(k+i) = dlcc(obj_,nel,j)

	    enddo

	  else
	    do i=1,nbrxi
	      derivee(k+i)=1.e-10
	    enddo
	  endif
	  k=k+nbrxi
    1 continue

	endif
	
	return
	end

c	*************************************************************************************
c	CALCUL de la dérivée du LIFE CYCLE COST par rapport à la variable var du panneau nel
c	*************************************************************************************
	double precision function dlcc(obj,nel,var)

      
      use param_section, OBJ_=>OBJ

	implicit double precision (a-h,o-z)
	double precision newpoids,newobj
	integer*4 var


	if (var.eq.1) then
		val = panneau(nel).delta*.01
	elseif (var.eq.2) then
		val = panneau(nel).hya*.01
	elseif (var.eq.3) then
		val = panneau(nel).dya*.01
	elseif (var.eq.4) then
		val = panneau(nel).wya*.01
	elseif (var.eq.5) then
		val = panneau(nel).epsa*.01
	elseif (var.eq.6) then
		val = panneau(nel).hxr*.01
	elseif (var.eq.7) then
		val = panneau(nel).dxr*.01
	elseif (var.eq.8) then
		val = panneau(nel).wxr*.01
	elseif (var.eq.9) then
		val = panneau(nel).epsr*.01
	endif

	call poidstotal(newpoids,nel,var) !!! nel et var !
	call lcc(newpoids,newobj,codo,acof,rtm,edis)

	dlcc = (newobj - obj)/val

	return
	end function
