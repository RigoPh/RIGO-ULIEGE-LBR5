      subroutine objin1(obj_,derivee,iderivee,iprint_)			

      

      use param_section
!      use param_cout
!      use param_opti_local
	
	
	implicit double precision(a-h,o-z)
	dimension derivee(ntot(iboat))							

c ******************************************************************************
c     calcul de la fonction objectif inertie (obj) et
c     calcul des dérivées de la fonction objectif (derivee)
c
c     version du : JANVIER 2009                                créé 14-2-06
c
c ******************************************************************************

c les variables
c -------------
c	1	delta =  épaisseur du bordage
c	2	hya   =  hauteur de l'âme des aiguilles
c	3	dya   =  épaisseur de l'âme des aiguilles
c	4	wya   =  largeur des semelles des aiguilles
c	5	epsa  =  entredistance entre aiguilles
c	6	hxr   =  hauteur de l'âme des raidisseurs
c	7	dxr   =  épaisseur de l'âme des raidisseurs
c	8	wxr   =  largeur des semelles des raidisseurs
c	9	epsr  =  entredistance entre raidisseurs

c 	autres paramètres :
c	 	tya = épaisseur semelle aiguilles
c     	txr = épaisseur semelle raidisseurs 
c
c  les variables de conception pour epontille					
c  ------------------------------------------					
c	1	hya		=	hauteur demi âme ou diam ou côté ext.	
c	2	dya		=	épaisseur d'âme							
c	3	wya		=	largeur de semelle						
c	4	epais	=	épaisseur de paroi mince				
c	5	epsa	=	entredistance entre épontilles			
c

c ******************************************************************************

c	Calcul Fonction objectif
	

	call momentstatiques(omesx,omesy)
	call axesneutres(nnoeud,nnoeud1,nnoeud2,omesx,omesy,x1,x2)
	call calcul_inerties_flexionnelles()

	obj_ = aixxtotpart !calculé dans calcul_inerties_flexionnelles
	

	if (iprint_.ne.0) then			!oct06
		write(iu_31(iboat),*)
		write(iu_31(iboat),*)' Inertie autour axe neutre hor. (subr.ob
     *jin1)'
		write(iu_31(iboat),*)' ---------------------------------------
     *-----'
		write(iu_31(iboat),*)' Inertie - Inertia iyy = ',obj_,' m**4'
	endif



c	Calcul Dérivées de la fonction objectif

	if (iderivee.eq.1) then

	k=0
	do 1 nel=1,neto
	  nbrxi=nvar(nel,iboat)

	  if(nbrxi.eq.0) goto 1

        if(itype(nel).ne.5) then
	  
		do i=1,nbrxi
	      j=nxit(i,nel,iboat)		! no de ref de chaque var. de conception

		  ! Fonction dérivée définie dans COORD
		  derivee(k+i) = deriveeixx(nel,j)
	    enddo
	  else
	    do i=1,nbrxi
	      derivee(k+i)=1.e-10
	    enddo
	  endif
	  k=k+nbrxi
    1	continue

	endif

 	
	return
      end
