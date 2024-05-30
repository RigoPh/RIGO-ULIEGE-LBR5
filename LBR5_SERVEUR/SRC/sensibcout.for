      subroutine sensibcout(obj_,derivee,iderivee,iprint_)										!18.03.04

!      use param_multi
      use param_section
      

      implicit double precision(a-h,o-z)
	double precision inva
      dimension x(10),y(10),s(3),w(2),bsc(2),c(2)
      dimension asc(2,2),inva(2,2),derivee(ntot(iboat)) !,dfctsc(nn)

	double precision, allocatable, save :: dfctsc(:)

      common/alstom/idiff,nbr,pas

	allocate (dfctsc(ntot(iboat)))

	nbr=5 ! provisoire (il est défini plus loin)
	nn = ntot(iboat)

c*********************************************************************************
c     subroutine sensibcout
c     ===================
c     cette sous-routine est appelée dans optis.
c     elle calcule une sensibilité de la fonction objectif
c     coût total pour chaque variable de conception, pour chaque panneau.
c     elle calcule aussi la valeur de la fonction objectif.
c     pour ce faire, 2 versions différentes sont implémentées :
c
c     1) calcul des sensibilités au niveau du coût global (icout = 3)
c     ---------------------------------------------------
c        ce calcul peut s'effectuer par différence finie (idiff=1) ou
c        par la méthode de gauss (idiff=0) : on détermine la valeur du coût en 
c        différents points autour du point qui nous intéresse et interpole une 
c        courbe par ces points grâce à la méthode des moindres carres de gauss. 
c        de cette courbe,on en déduit facilement la valeur de la dérivée.
c
c     2) calcul des sensibilités au niveau des coûts unitaires (icout = 2)
c     --------------------------------------------------------
c        le calcul des sensibilités des coûts unitaires est fait à la fois par 
c        différence finie et analytiquement (les coûts unitaires n'étant pas donnés 
c        par une fonction analytique, on doit encore procéder par diff. finie) dans 
c        costmain.
c
c     rappel : si icout = 1, le calcul du coût est effectué de manière simplifiée
c              sans passer dans cette sous-routine.
c
c inputs : nbr : nombre de points à droite (et à gauche) utilisés pour déterminer
c                la dérivée de la fonction objectif. 
c                on utilise 2*nbr+1 points (= n) pour la méthode de gauss
c          pas   : espacement entre les différents points.
c          neto  : nombre de panneaux de la structure.
c          nvar  : vecteur stockant les numéros des variables de conception utilisées.
c          nn    : dimension max du vecteur dérivée.      
c          spec  : poids spécifique (n/m3) (à spécifier pour chaque panneau)
c          width : longueur totale de la structure   
c
c          
c outputs : derivee: valeur approchée de la dérivée du coût par rapport aux variables
c                    de conception utilisées, pour chaque panneau.
c           obj: coût total de fabrication de la structure.
c                       
c créer: f. bair (juin 2003)
c
c modifications:
c   - juin 2003 :
c
c dernière mise à jour: 16 juin 2003 (ph. rigo)
c                       11 septembre 2003 (f. bair) : combinaison des 2 versions   
c					  18 mars 2004 (j.pirard) : introduction des épontilles          
c
c*********************************************************************************
	

      call annuld(derivee,nn)

c  1. lecture des données courantes

	! On ne travaille pas par panneau avec c1
	!pour c1, faire une moyenne pondérée de tous les c1???		!jan09
	! (en fait, ça pourrait être la même chose pour eqp...)		!jan09
	c1_local=0.d00
	long_tot=0.d00
	labour_cost_local=0.d00

	do i=1,neto
	 vepaiss(i) = panneau(i).delta + panneau(i).corro(1)
	 veac(i) = panneau(i).dya + panneau(i).corro(2)
	 vesc(i) = panneau(i).tya + panneau(i).corro(2)
	 vdiffc(i) = panneau(i).epais + panneau(i).corro(2)
	 vear(i) = panneau(i).dxr + panneau(i).corro(3)
	 vesr(i) = panneau(i).txr + panneau(i).corro(3)


	 vhac(i) = panneau(i).hya
	 vlsc(i) = panneau(i).wya

	 vhar(i) = panneau(i).hxr
	 vlsr(i) = panneau(i).wxr
	 deltacsc(i) = panneau(i).epsa
	 deltar(i) = panneau(i).epsr
       entrsc(i) = panneau(i).entr
	 philsc(i) = abs(panneau(i).phil)
	 qsc(i) = panneau(i).q

	 c1_local = c1_local + c1(indMateriau(i))*abs(panneau(i).hight)
	 long_tot = long_tot + abs(panneau(i).hight)
	 labour_cost_local = labour_cost_local + 
     *			     labour_cost(indMateriau(i))*abs(panneau(i).hight)
	enddo

	c1_local = c1_local/long_tot
	labour_cost_local = labour_cost_local/long_tot
	
	eqpc1 = labour_cost_local !eqp_global*c1_local*1000.0  ! (=euro/heure) ,pour transfert de rro en euro	!jan09


c  2. calcul de fmat= coût des matériaux (tôles, raid, cadres)
c    --------------------------------------------------------
c        coutmat(i) = coût matériaux du panneau i
	fmat = 0.d00
	do nel=1,neto
!		call coutpanneau_mat(ffmat,nel)
		call coutpanneau_mat(nel)	            !bug2011	
	    coutmat(nel) = ffmat
		fmat = fmat + ffmat
	enddo

c  3. calcul des coûts de mdo + sensibilités (boucles sur les variables de conception)
c  --------------------------------------------------------------------------------------
      non = 0 !si non=*** -> certaines variables de conception sortent du jeu de données
	        ! - 1er chiffre : si =1 -> variable trop petite; 
			!                 si =2 -> variable trop grande
	        ! - les 2 autres chiffres: donnent le numéro de l'opération en question

      ncont = 0

      if (icout.eq.2) then
	pas = 1./20.
	idiff = 1
	nbr = 1
	call costmain(coutrro,width,i1,derivee,nn,totalti)
c	    coutpan = rro de chaque panneau
c	    coutrro = rro structure globale
      derivee = derivee*eqpc1  ! = dérivée du coût de fabrication en euro
	goto 1
      endif


c     paramètres de controle pour le calcul par différence finie

      idiff = 1   ! si = 0, interpolation des moindres carrés sur 2nbr+1 points
	            ! si = 1, simple différence finie sur 2 points (1 à gauche et 1 à droite)

c     le nombre total de points utilisés pour le calcul des dérivées  !newcost
c        = nbr*2 + 1  (nbr doit être >0! )                            !newcost

	if (iprint_.eq.1) then

      write(iu_31(iboat),'(/a,a/60(1h-))')
     *'les paramètres de contrôle du ',
     *   'calcul des sensibilités sont les suivants:'

      if(idiff.eq.1) then
	  nbr=1  ! pas besoin de calculer d'autres points
	  write(iu_31(iboat),*) ' idiff=1 : nbr=1'
	  write(iu_31(iboat),*) ' calcul direct des sensibilités '
	  write(iu_31(iboat),*) '      via 2 pts: xi-pas*xi  et xi+pas*xi'
	else
	  nbr=1 ! au choix de l'utilisateur
	  write(iu_31(iboat),*) ' idiff=',idiff ,' nbr=',nbr
	  write(iu_31(iboat),*) 
     *' calcul direct des sensibilités (moindres carrés)'
	  write(iu_31(iboat),*) ' via ',2*nbr+1,' pts: '
	  write(iu_31(iboat),*)
     * ' à savoir: xi-nbr*pas*xi  à  xi+nbr*pas*xi'
	endif

c     choix du pas à gauche et à droite pour le calcul par difference finie.
c     ... x+2*pass*x,  x-pass*x  et x+pass*x , x+2*pass*x  , ... (x = valeur)
	pass = 1/20.0   ! càd 1/20 ème de la valeur de la variable de conception

	write(iu_31(iboat),'(a,f8.5,a)')
     * ' et pas =',pass, '(à multiplier par chaque variable de concep)'

	endif

  1   continue

	IF (iderivee.eq.1) THEN  !sinon on ne calcule pas les dérivées

      do 5 j=1,neto ! boucle sur le nombre de panneaux

		drefy_local=drefy(indMateriau(j))
		c3_local=c3(indMateriau(j))
		dw3_local=dw3(indMateriau(j))
		drefx_local=drefx(indMateriau(j))
		c2_local=c2(indMateriau(j))
		dw2_local=dw2(indMateriau(j))

		if(((refthick_input.eq.1).and.(itype(j).ne.5))) then

		! On utilise drefy_local qui est :
		! - soit drefy
		! - soit dref_b, dref_c ou dref_l (plat boudin, Té composite, Té laminé
		! Et pas drefy=dref_b car ça modifierait les valeurs de drefy qui est une variable globale
		!  => on modifierait les données

			if (panneau(j).itype_transv.eq.14)then   !Plat Boudin
				drefy_local=dref_b(indMateriau(j))
		        c3_local=c_pb(indMateriau(j))
			    dw3_local=dw_b(indMateriau(j))

			elseif(panneau(j).itype_transv.eq.7) then   ! Té Composite
				drefy_local=dref_c(indMateriau(j))
				c3_local=c_tc(indMateriau(j))
				dw3_local=dw_c(indMateriau(j))

			elseif(panneau(j).itype_transv.eq.17)then   ! Té Laminé
				drefy_local=dref_l(indMateriau(j))
				c3_local=c_tl(indMateriau(j))
				dw3_local=dw_l(indMateriau(j))

			else 
			  write(* ,*)'type de profilé inexistant'
			endif

			if (panneau(j).itype_longit.eq.14)then   !Plat Boudin
				drefx_local=dref_b(indMateriau(j))
				c2_local=c_pb(indMateriau(j))
				dw2_local=dw_b(indMateriau(j))

			elseif(panneau(j).itype_longit.eq.7) then   ! Té Composite
				drefx_local=dref_c(indMateriau(j))
				c2_local=c_tc(indMateriau(j))
				dw2_local=dw_c(indMateriau(j))

		   elseif(panneau(j).itype_longit.eq.17)then   ! Té Laminé
				drefx_local=dref_l(indMateriau(j))
			    c2_local=c_tl(indMateriau(j))
				dw2_local=dw_l(indMateriau(j))
			else 
				write(* ,*)'type de profilé inexistant'
			endif

		endif

	nbrxi = nvar(j,iboat)
	dens = spec(indMateriau(j))/9.81
	temp = philsc(j) * qsc(j) * width * pi/180.

      do 10 i=1,nbrxi  ! boucle sur les variables de conception du panneau j
      ii = i + ncont	! compteur du nbre de variable de conception
      i1 = nxit(i,j,iboat)

      if (icout.eq.2) goto 2
     	
c	if (i1.eq.1.or.i1.eq.2.or.i1.eq.5.or.i1.eq.6.or.i1.eq.9) then  ! sinon dérivée nulle
	if (i1.eq.4.or.i1.eq.7.or.i1.eq.8) goto 777  ! sinon dérivée nulle
      
c 3.1 lecture de la valeur de la variable de conception i du panneau j
c      et des bornes min et max de cette variable de conception (cfr base de données)

	if(isect(j).eq.0) then											!18.03.04
      if (i1.eq.1) then          ! rem. : on ne se sert pas du paramètre pente pour l'instant               
	  valeur = vepaiss(j)      ! pente = +1 : la pente "globale" est positive 
	else if (i1.eq.2) then
	  valeur = panneau(j).hya !vhac(j)
	else if (i1.eq.3) then
	  valeur = veac(j)
	else if (i1.eq.4) then
	  valeur = vlsc(j)
	else if (i1.eq.5) then
	  valeur = deltacsc(j)
	else if (i1.eq.6) then
	  valeur = vhar(j)
	else if (i1.eq.7) then
	  valeur = vear(j)
	else if (i1.eq.8) then
	  valeur = vlsr(j)
	else if (i1.eq.9) then
	  valeur = deltar(j)
	endif
	else										!18.03.04
      if (i1.eq.1) then		   				    !18.03.04               
	  valeur = panneau(j).hya							!18.03.04
      elseif (i1.eq.2) then						!18.03.04               
	  valeur = veac(j)							!18.03.04
      elseif (i1.eq.3) then						!18.03.04               
	  valeur = vlsc(j)							!18.03.04
      elseif (i1.eq.4) then						!18.03.04               
	  valeur = vdiffc(j)						!18.03.04
      elseif (i1.eq.5) then						!18.03.04               
	  valeur = deltacsc(j)						!18.03.04
	endif										!18.03.04
	endif				!18.03.04

c     calcul du pas à gauche et à droite pour le calcul par difference finie.
c     ... x+2*pass*x,  x-pass*x  et x+pass*x , x+2*pass*x  , ... (x = valeur)
	pas = pass* valeur   ! avec pass= 1/20 ème de la valeur de la variable de conception
	
c 3.2  bornes min et max: choix

c faut-il aller lire directement ces bornes dans le fichier base.txt ?
c difficile compte tenu du fait qu'il n'est écrit nulle part quelle
c variable de conception est associée à chaque opération => de toute
c façon, la procédure ne peut donc être totalement automatique =>
c c'est pas trop grave si on introduit les bornes ici.   
c !!!attention si on change les valeurs du fichier base.txt !!!!!!!!
c rem. : il faut prendre comme bornes celles des opérations les plus 
c        restrictives (càd dont l'intervalle de donnée est le plus petit)

c 3.3 détermination de la dérivée en valeur (càd au point courant de la variable de conception considérée)

	if(isect(j).eq.0) then							!18.03.04
      do k=1,2*nbr+1  ! boucle pour le calcul par différences finies
        x(k) = valeur  + (k-1-nbr)*pas
	  if (i1.eq.1) vepaiss(j) = x(k)
	  if (i1.eq.2) vhac(j)    = x(k)
	  if (i1.eq.3) veac(j)    = x(k)
	  if (i1.eq.4) vlsc(j)    = x(k)
	  if (i1.eq.5) deltacsc(j)  = x(k)	
	  if (i1.eq.6) vhar(j)    = x(k)
	  if (i1.eq.7) vear(j)    = x(k)
	  if (i1.eq.8) vlsr(j)    = x(k)
	  if (i1.eq.9) deltar(j)  = x(k)

	  call costmain(y(k),width,i1,derivee,nn,totalti)

c	    coutpan = rro de chaque panneau
c	    coutrro = rro structure globale

        if (k.eq.(nbr+1).and.i.eq.1) then
c         sauvegarde de coutrro et des coutpan(j) au point courant.
	     coutrro = y(k)
	     do jj=1,neto
             coutpan(jj)=costpan(jj)
	     enddo
	  endif
	  if (k.eq.2*nbr+1) then
	    if (i1.eq.1) vepaiss(j) = valeur
          if (i1.eq.2) vhac(j)    = valeur
	    if (i1.eq.3) veac(j)    = valeur
	    if (i1.eq.4) vlsc(j)    = valeur
	    if (i1.eq.5) deltacsc(j)  = valeur
	    if (i1.eq.6) vhar(j)    = valeur
	    if (i1.eq.7) vear(j)    = valeur
	    if (i1.eq.8) vlsr(j)    = valeur
	    if (i1.eq.9) deltar(j)  = valeur
	  endif 

	enddo  ! do k=1,2*nbr+1
	endif										!18.03.04

c     choix du mode de calcul des dérivées:
c       - soit locale au ppoint courant
c       - soit approchée par la méthode des moindres carrées (idiff=0)

	if (idiff.eq.0) then    ! moindres carrés
	 s = 0.0
	 w = 0.0
       n = 2*nbr + 1 
       p = 1
       do m=1,2*p
	    do i2=1,n
	      s(m) = s(m) + x(i2)**m
  	    enddo 
       enddo
       do m=1,p+1
	    do i2=1,n
	      w(m) = w(m) + y(i2)*x(i2)**(m-1)
   	    enddo 
       enddo
       asc(1,1) = n
	 asc(1,2) = s(1)
	 asc(2,1) = s(1)
	 asc(2,2) = s(2)
	 c(1) = w(1)
	 c(2) = w(2)
	 dtm = asc(1,1)*asc(2,2) - asc(2,1)*asc(1,2)
	 inva(1,1) =  asc(2,2)/dtm
	 inva(1,2) = -asc(1,2)/dtm
	 inva(2,1) = -asc(2,1)/dtm
	 inva(2,2) =  asc(1,1)/dtm
	 bsc(1) = inva(1,1)*c(1) + inva(1,2)*c(2)
	 bsc(2) = inva(2,1)*c(1) + inva(2,2)*c(2)  ! dérivée obtenue au point courant (valeur)

	 derivee(ii) = bsc(2)       ! (en rro)   selon la méthode des moindres carrés             

	else      ! si idiff = 1                  
       derivee(ii) = (y(nbr+2) - y(nbr))/(2*pas) ! (en rro) , calcul local
	endif

	derivee(ii) = derivee(ii)*eqpc1  ! = dérivée du coût de fabrication en euro

  777 continue
   2  continue

c 3.4 calcul de la dérivée du coût des matériaux (dfct) en euro
	if(isect(j).eq.0) then							!18.03.04
	if (i1.eq.1) then
	  dfctsc(ii) = temp*dens*c1(indMateriau(j))*(1 + 
     *   (2*vepaiss(j)-dref(indMateriau(j)))*1000.*dc1(indMateriau(j))) ! = dérivée du coût des matériaux
	                        
	else if (i1.eq.2) then
	  dfctsc(ii) = temp*dens*c3_local/deltacsc(j)*
     *        (1 + (veac(j) - dref(indMateriau(j)))*1000.*
     *		dc1(indMateriau(j)))*(1. + dw3_local)*veac(j)
	  
	else if (i1.eq.3) then
	  dfctsc(ii) = temp*dens*c3_local/deltacsc(j)*
     *        ( 1000.*dc1(indMateriau(j))*(1 + dw3_local)*
     *		(veac(j)*vhac(j) + vesc(j)*vlsc(i))
     *      +   (1 + (veac(j) - dref(indMateriau(j)))*1000.*
     *		dc1(indMateriau(j)))*(1 + dw3_local)*vhac(j)     )
	  
	else if (i1.eq.4) then
	  dfctsc(ii) = temp*dens*c3_local/deltacsc(j)*
     *        (1 + (veac(j) - dref(indMateriau(j)))*1000.*
     *		dc1(indMateriau(j)))*(1 + dw3_local)*vesc(j)
        
	else if (i1.eq.5) then 
	  dfctsc(ii) = -temp*dens*c3_local/deltacsc(j)**2 *
     *        (1 + (veac(j) - dref(indMateriau(j)))*1000.*
     *		dc1(indMateriau(j)))*(1 + dw3_local)   
     *         *(veac(j)*vhac(j) + vesc(j)*vlsc(j))
	  
	else if (i1.eq.6) then
	  dfctsc(ii) = temp*dens*c2_local/deltar(j)*
     *        (1 + (vear(j) - dref(indMateriau(j)))*1000.*
     *		dc1(indMateriau(j)))*(1. + dw2_local)*vear(j)

	else if (i1.eq.7) then
	  dfctsc(ii) = temp*dens*c2_local/deltar(j)*
     *        ( 1000.*dc1(indMateriau(j))*(1 + dw2_local)*
     *		(vear(j)*vhar(j) + vesr(j)*vlsr(i))
     *		+   (1 + (vear(j) - dref(indMateriau(j)))*1000.*
     *		dc1(indMateriau(j)))*(1 + dw2_local)*vhar(j)     )
	  
      else if (i1.eq.8) then
	  dfctsc(ii) = temp*dens*c2_local/deltar(j)*
     *        (1 + (vear(j) - dref(indMateriau(j)))*1000.*
     *		dc1(indMateriau(j)))*(1 + dw2_local)*vesr(j)
	
	else if (i1.eq.9) then
	  dfctsc(ii) = -temp*dens*c2_local/deltar(j)**2 *
     *        (1 + (vear(j) - dref(indMateriau(j)))*1000.*
     *		dc1(indMateriau(j)))*(1 + dw2_local)   
     *         *(vear(j)*vhar(j) + vesr(j)*vlsr(j))
	endif
	endif										!18.03.04
c 3.5 dérivée total : mdo + coût des matériaux (en euro)

      derivee(ii) = derivee(ii) + dfctsc(ii)       ! = dérivée totale du coût en euro

  10  continue ! fin de la boucle sur les variables de conception
      ncont = ncont + nbrxi

   5  continue ! fin de la boucle sur le nombre de panneaux (sur j)


	ENDIF ! fin de la condition sur le calcul de la dérivée

c =====================================================================================

c 3.6 cout total (mat + mdo)
      obj_ = fmat + coutrro*eqpc1

c  --------------------------------------------------------------------------------
c 3.7 warning si bornes des variables de conception dépassée dans la banque de données
      if (non.gt.1.and.non.lt.200) then
	 numop = non - 100
	 write(iu_31(iboat),*)
     * 'attention, certaines variables de conception ont', 
     * ' une valeur qui sort de la base de donnees permettant de',
     * ' calculer le cout (-->> valeur trop petite)'
	 write(iu_31(iboat),*) 'il s''agit de l''operation : ',numop
	 write(*,*)'attention, certaines variables de conception ont une', 
     * ' valeur qui sort de la base de donnees permettant de calculer',
     * ' le cout (valeur trop petite)'
	 write(*,*) 'il s''agit de l''operation : ',numop

	 write(iu_14(iboat),*)
     * 'attention, certaines variables de conception ont',		
     * ' une valeur qui sort de la base de donnees permettant de',		
     * ' calculer le cout (-->> valeur trop petite)'						
	 write(iu_14(iboat),*) 'il s''agit de l''operation : ',numop					

c	 pause
 
      elseif (non.gt.200) then
	 numop = non - 200
	 write(iu_31(iboat),*)
     * 'attention, certaines variables de conception ont', 
     * ' une valeur qui sort de la base de donnees permettant de',
     * ' calculer le cout (-->> valeur trop grande)'
	 write(iu_31(iboat),*) 'il s''agit de l''operation : ',numop
	 write(*,*)'attention, certaines variables de conception ont une', 
     * ' valeur qui sort de la base de donnees permettant de calculer',
     * ' le cout (valeur trop grande)'
	 write(*,*) 'il s''agit de l''operation : ',numop

	write(iu_14(iboat),*)
     * 'attention, certaines variables de conception ont',		
     * ' une valeur qui sort de la base de donnees permettant de',		
     * ' calculer le cout (-->> valeur trop grande)'						
	 write(iu_14(iboat),*) 'il s''agit de l''operation : ',numop					

	endif

c =========================================================================================
c =========================================================================================
c
c 4.  impression des différents coûts dans opt
c -------------------------------------------
      
	IF (iprint_.eq.1) THEN ! on n'imprime pas tout ça après le passage dans CONLIN
	
	poids=0.0

      do j=1,neto

		drefy_local=drefy(indMateriau(j))
		c3_local=c3(indMateriau(j))
		dw3_local=dw3(indMateriau(j))
		drefx_local=drefx(indMateriau(j))
		c2_local=c2(indMateriau(j))
		dw2_local=dw2(indMateriau(j))
		
		if(((refthick_input.eq.1).and.(itype(j).ne.5))) then

		! On utilise drefy_local qui est :
		! - soit drefy
		! - soit dref_b, dref_c ou dref_l (plat boudin, Té composite, Té laminé
		! Et pas drefy=dref_b car ça modifierait les valeurs de drefy qui est une variable globale
		!  => on modifierait les données

			if (panneau(j).itype_transv.eq.14)then   !Plat Boudin
				drefy_local=dref_b(indMateriau(j))
		        c3_local=c_pb(indMateriau(j))
			    dw3_local=dw_b(indMateriau(j))

			elseif(panneau(j).itype_transv.eq.7) then   ! Té Composite
				drefy_local=dref_c(indMateriau(j))
				c3_local=c_tc(indMateriau(j))
				dw3_local=dw_c(indMateriau(j))

			elseif(panneau(j).itype_transv.eq.17)then   ! Té Laminé
				drefy_local=dref_l(indMateriau(j))
				c3_local=c_tl(indMateriau(j))
				dw3_local=dw_l(indMateriau(j))

			else 
			  write(* ,*)'type de profilé inexistant'
			endif

			if (panneau(j).itype_longit.eq.14)then   !Plat Boudin
				drefx_local=dref_b(indMateriau(j))
				c2_local=c_pb(indMateriau(j))
				dw2_local=dw_b(indMateriau(j))

			elseif(panneau(j).itype_longit.eq.7) then   ! Té Composite
				drefx_local=dref_c(indMateriau(j))
				c2_local=c_tc(indMateriau(j))
				dw2_local=dw_c(indMateriau(j))

		   elseif(panneau(j).itype_longit.eq.17)then   ! Té Laminé
				drefx_local=dref_l(indMateriau(j))
			    c2_local=c_tl(indMateriau(j))
				dw2_local=dw_l(indMateriau(j))
			else 
				write(* ,*)'type de profilé inexistant'
			endif

		endif


c      fcp = poids = bordage + ames + semelles  (du panneau nel)
       temp = philsc(j) * qsc(j) * width * pi/180.
	 dens = spec(indMateriau(j))/9.81
	if(isect(j).eq.0) then												!18.03.04
       fcp= vepaiss(j) 
     *  +  (1.0+dw3_local)*(veac(j) * vhac(j) + vesc(j)* vlsc(j)) 
     *	/deltacsc(j)
     *  +  (1.0+dw2_local)*(vear(j) * vhar(j) + vesr(j)* vlsr(j))
     *	/deltar(j)
	 elseif(isect(j).eq.1) then											!cercle !18.03.04
       fcp=(1.0+dw3_local)*pi*(vhac(j)*vhac(j)-(vhac(j)-2*vdiffc(j))**2) 		!18.03.04
     *	 /(4*deltacsc(j))													!18.03.04
c     *  +  (1.0+dw2)*(vear(j) * vhar(j) + vesr(j)* vlsr(j)) /deltar(j)	!sept06
	 elseif(isect(j).eq.2) then											!carre !18.03.04
      fcp=(1.0+dw3_local)*(vhac(j)*vhac(j)-(vhac(j)-2*vdiffc(j))**2)
     *	/deltacsc(j)	!18.03.04
c     *  +  (1.0+dw2)*(vear(j) * vhar(j) + vesr(j)* vlsr(j)) /deltar(j)	!sept06
	 elseif(isect(j).eq.3) then											!doublet !18.03.04
       fcp=(1.0+dw3_local)*(veac(j)*2* vhac(j)+2*vesc(j)* vlsc(j))
     *	/deltacsc(j)	!18.03.04
c     *  +  (1.0+dw2)*(vear(j) * vhar(j) + vesr(j)* vlsr(j)) /deltar(j)	!sept06
	endif																!18.03.04

       fcp=  fcp * temp * spec(indMateriau(j))  ! poids du panneau (borde, raid et cadre only)
	 poids=poids+ fcp

       if (j.eq.1) then
        write(iu_31(iboat),*) 
	  write(iu_31(iboat),*)
     *'*** cost objective function (subr. sensibcout)'
	  write(iu_31(iboat),*)
     *'    ------------------------------------------'
	  write(iu_31(iboat),*)
     *'         (with additional corrosion thickness)'
	  write(iu_31(iboat),*)'           (girder not considered !!!)'
	  write(iu_31(iboat),*)
	  write(iu_31(iboat),42)

   42 format(3x,'panel',t13,' weight',      t26,'cost(mat+labor)',
     *                  t42,'material cost',t58,'labor cost',
     *                  t73,' (rro labor) '/
     *       ' panneau',t13,' poids',       t26,'cout(mat+labor)',
     *                  t42,'coût matière ',t57,' cost mdo',
     *                  t73,' (rro mdo) ' /
     *                 ,t13,'  newton',     t26,' euro (or ...)',
     *                  t42,' euro (or...)',t57,' euro (or ...)',
     *                  t73 ' hours(heures)')
    	 endif

       ctpanel=coutmat(j)+coutpan(j)*eqpc1

       write(iu_31(iboat),43) j,fcp,ctpanel,coutmat(j),
     *                             coutpan(j)*eqpc1,coutpan(j)
   43  format(4x,i3,t10,5(f14.3,1x) )   


       if (j.eq.neto) then
        write(iu_31(iboat),*) 
        write(iu_31(iboat),* )
     *' total for the structure (with gross thickness)'
        write(iu_31(iboat),* )
     *' ----------------------------------------------'
        write(iu_31(iboat),46)
     *' cout-cost(mat+mdo) = ',obj_  ,' euro (or...)'
        write(iu_31(iboat),46)' - total labor cost = ',coutrro*eqpc1,
     *                                                ' euro (or...)'
        write(iu_31(iboat),46)'   labor time (rro) = ',coutrro,' hours'
        write(iu_31(iboat),46)'   labor time (ti)  = ',totalti,' hours'		
	  write(iu_31(iboat),46)
     *'    [rate: 1 hour   = ',eqpc1,' euro (or...) ]'
        write(iu_31(iboat),46) 
     *' - total mat. cost  = ',fmat ,' euro (or...)'
        write(iu_31(iboat),46)'   poids - weight   = ',poids,' n'
	 endif

	enddo  ! do j=1,neto

c
c impression des différents coûts dans sol2
c -----------------------------------------

      write(iu_12(iboat),*)
	write(iu_12(iboat),*)
	 
	do j=1,neto

		drefy_local=drefy(indMateriau(j))
		c3_local=c3(indMateriau(j))
		dw3_local=dw3(indMateriau(j))
		drefx_local=drefx(indMateriau(j))
		c2_local=c2(indMateriau(j))
		dw2_local=dw2(indMateriau(j))

		if(((refthick_input.eq.1).and.(itype(j).ne.5))) then

		! On utilise drefy_local qui est :
		! - soit drefy
		! - soit dref_b, dref_c ou dref_l (plat boudin, Té composite, Té laminé
		! Et pas drefy=dref_b car ça modifierait les valeurs de drefy qui est une variable globale
		!  => on modifierait les données

			if (panneau(j).itype_transv.eq.14)then   !Plat Boudin
				drefy_local=dref_b(indMateriau(j))
		        c3_local=c_pb(indMateriau(j))
			    dw3_local=dw_b(indMateriau(j))

			elseif(panneau(j).itype_transv.eq.7) then   ! Té Composite
				drefy_local=dref_c(indMateriau(j))
				c3_local=c_tc(indMateriau(j))
				dw3_local=dw_c(indMateriau(j))

			elseif(panneau(j).itype_transv.eq.17)then   ! Té Laminé
				drefy_local=dref_l(indMateriau(j))
				c3_local=c_tl(indMateriau(j))
				dw3_local=dw_l(indMateriau(j))

			else 
			  write(* ,*)'type de profilé inexistant'
			endif

			if (panneau(j).itype_longit.eq.14)then   !Plat Boudin
				drefx_local=dref_b(indMateriau(j))
				c2_local=c_pb(indMateriau(j))
				dw2_local=dw_b(indMateriau(j))

			elseif(panneau(j).itype_longit.eq.7) then   ! Té Composite
				drefx_local=dref_c(indMateriau(j))
				c2_local=c_tc(indMateriau(j))
				dw2_local=dw_c(indMateriau(j))

		   elseif(panneau(j).itype_longit.eq.17)then   ! Té Laminé
				drefx_local=dref_l(indMateriau(j))
			    c2_local=c_tl(indMateriau(j))
				dw2_local=dw_l(indMateriau(j))
			else 
				write(* ,*)'type de profilé inexistant'
			endif

		endif


c     fmat= coût des matériaux (acier)
c     fmdo= coût main d'oeuvre
c     fct = fmat + fmdo  (d'un panneau nel),
c     obj = coût total          (de la structure complete)

      if(j.eq.1) then
		cout = 0.d00
		poids = 0.d00
		fmat11 = 0.d00
		fmat22 = 0.d00
		fmat33 = 0.d00
		fmat = 0.d00
	endif

c     dcor = epaisseur de corrosion pour bordé

	write(iu_11(iboat),*) 
	write(iu_11(iboat),*) ' avec sur-épaisseurs de corrosion'
	write(iu_11(iboat),*) ' --------------------------------'
	write(iu_11(iboat),*) ' *** sur-épaisseur=',panneau(j).corro(1),' 
     *m'
	write(iu_11(iboat),*) ' - epaiss bordé      =',vepaiss(j)
	write(iu_11(iboat),*) ' - epaiss âme cadres =',veac(j)
	write(iu_11(iboat),*) ' - epaiss sem cadres =',vesc(j)
	write(iu_11(iboat),*) ' - epaiss âme raid   =',vear(j)
	write(iu_11(iboat),*) ' - epaiss sem raid   =',vesr(j)

	dens =  spec(indMateriau(j))/9.81
      temp =  philsc(j) * qsc(j) * width * pi/180.
	temp1=  temp * dens  
	temp2=  temp * rend_global * eqpc1 

c     supplément de poids (en n) (sans les traverses)
	if(isect(j).eq.0) then												!18.03.04
      spoid1 = temp*spec(indMateriau(j)) * ( panneau(j).corro(1) +
     *   (1.0+dw2_local)*(panneau(j).corro(3)*vhar(j)+
     *	panneau(j).corro(3)*vlsr(j))/deltar(j) +
     *   (1.0+dw3_local)*(panneau(j).corro(2)*vhac(j)+
     *	panneau(j).corro(2)*vlsc(j))/deltacsc(j))
       poid1 = temp*spec(indMateriau(j)) * ( vepaiss(j) +
     *   (1.0+dw2_local)*(vear(j)*vhar(j)+vesr(j)*vlsr(j))/deltar(j) +
     *   (1.0+dw3_local)*(veac(j)*vhac(j)+vesc(j)*vlsc(j))/deltacsc(j) )
	 elseif(isect(j).eq.1) then												!18.03.04
      spoid1 = temp*spec(indMateriau(j)) *									!18.03.04
c     *   ((1.0+dw2)*(corro(j,3)*vhar(j)+corro(j,3)*vlsr(j))/deltar(j) +		!sept06
     *   (1.0+dw3_local)*pi*(vhac(j)*vhac(j)-(vhac(j)-
     *	2*panneau(j).corro(2))**2)/(4*deltacsc(j)  )!)													!sept06
       poid1 = temp*spec(indMateriau(j)) * (! vepaiss(j) +					!sept06
c     *   (1.0+dw2)*(vear(j)*vhar(j)+vesr(j)*vlsr(j))/deltar(j) +				!sept06
     *   (1.0+dw3_local)*pi*(vhac(j)*vhac(j)-(vhac(j)-2*vdiffc(j))**2)/		!18.03.04
     *   (4*deltacsc(j)))													!18.03.04
	elseif(isect(j).eq.2) then												!18.03.04
	 spoid1 = temp*spec(indMateriau(j))*												!18.03.04
c     *   ((1.0+dw2)*(corro(j,3)*vhar(j)+corro(j,3)*vlsr(j))/deltar(j) +		!sept06
     *   (1.0+dw3_local)*(vhac(j)*vhac(j)-(vhac(j)-
     *	2*panneau(j).corro(2))**2)/deltacsc(j) ! )														!sept06
       poid1 = temp*spec(indMateriau(j)) * ( vepaiss(j) +									!18.03.04
c     *   (1.0+dw2)*(vear(j)*vhar(j)+vesr(j)*vlsr(j))/deltar(j) +				!sept06
     *   (1.0+dw3_local)*(vhac(j)*vhac(j)-(vhac(j)-2*vdiffc(j))**2) /		!18.03.04
     *   (deltacsc(j)))													!18.03.04
	elseif(isect(j).eq.3) then												!18.03.04
	 spoid1 = temp*spec(indMateriau(j)) *												!sept06
c     * ((1.0+dw2)*(corro(j,3)*vhar(j)+corro(j,3)*vlsr(j))/deltar(j) +		!sept06
     * (1.0+dw3_local)*(panneau(j).corro(2)*2*vhac(j)+
     * 2*panneau(j).corro(3)*vlsc(j))/deltacsc(j)!)		!sept06
       poid1 = temp*spec(indMateriau(j)) * ( !vepaiss(j) +									!sept06
c     *   (1.0+dw2)*(vear(j)*vhar(j)+vesr(j)*vlsr(j))/deltar(j) +				!sept06
     *   (1.0+dw3_local)*(veac(j)*2*vhac(j)+2*vesc(j)*vlsc(j))
     *	/deltacsc(j))		!18.03.04
	endif																	!18.03.04

	spoids=spoids+spoid1 ! supplément de poids dû à la corrosion
	poids =poids+ poid1 ! poids + poids additionnel dû à dw2 et dw3

	if(isect(j).eq.0) then													!18.03.04
c     coût des plaques
      fmat1 = temp1 *c1(indMateriau(j))* (1.+(vepaiss(j)-
     *		dref(indMateriau(j)))*1000.*dc1(indMateriau(j))) *
     *		vepaiss(j) 
c     coût des lisses
      fmat2 = temp1 *c2_local* (1.+dw2_local) * (1.+(vear(j)-
     *		dref(indMateriau(j)))*1000.*dc1(indMateriau(j)))
     *                     * (vear(j)*vhar(j)+vesr(j)*vlsr(j))/deltar(j)  
c     coût des cadres
      fmat3 = temp1 *c3_local* (1.+dw3_local) * (1.+(veac(j)-
     *		dref(indMateriau(j)))*1000.*dc1(indMateriau(j)))
     *      * (veac(j)*vhac(j)+vesc(j)*vlsc(j))/deltacsc(j)  
      
	elseif(isect(j).eq.1) then												!26.05.04
	fmat1 = 0.000															!26.05.04
	fmat2 = 0.000															!26.05.04
	fmat3 =  temp1 *c3_local* (1.+dw3_local) *(1.+(veac(j)-
     *	dref(indMateriau(j)))*1000.*dc1(indMateriau(j)))				!26.05.04
     *   *pi*(vhac(j)*vhac(j)-(vhac(j)-2*vdiffc(j))**2) /(4*deltacsc(j))		!26.05.04
      elseif(isect(j).eq.2) then												!26.05.04
	fmat1 = 0.000															!26.05.04
	fmat2 = 0.000															!26.05.04
	fmat3 =  temp1 *c3_local* (1.+dw3_local) *(1.+(veac(j)-
     *	dref(indMateriau(j)))*1000.*dc1(indMateriau(j)))				!26.05.04
     *   *(vhac(j)*vhac(j)-(vhac(j)-2*vdiffc(j))**2) /deltacsc(j)				!26.05.04
      elseif(isect(j).eq.3) then												!26.05.04
	fmat1 = 0.000															!26.05.04
	fmat2 = 0.000															!26.05.04
	fmat3 =  temp1 *c3_local* (1.+dw3_local) *(1.+(veac(j)-
     *	dref(indMateriau(j)))*1000.*dc1(indMateriau(j)))				!26.05.04
     *   *(veac(j)*2* vhac(j)+2*vesc(j)* vlsc(j)) /deltacsc(j)				!26.05.04
      endif		

c     coûts cumulés: for all the panels 
        fmat11 = fmat11+fmat1 ! matière bordé
        fmat22 = fmat22+fmat2 ! matière lisses
        fmat33 = fmat33+fmat3 ! matière cadres
	  ffmat  = fmat1+fmat2+fmat3
	  fmat   = fmat + ffmat
c	 else													!18.03.04
c	  ffmat = 0.0000										!18.03.04
c	  fmat = 0.0000											!18.03.04
c	 endif													!18.03.04
	  fct    = ffmat+ coutpan(j)*eqpc1

	  ppo=poid1-spoid1
	  coutpan(j) = coutpan(j)*eqpc1

       if (j.eq.1) then
        write(iu_12(iboat),*) 
	  write(iu_12(iboat),*)'*** cost and weight (subr. sensibcout)'
	  write(iu_12(iboat),*)'--------------------------------------'
	  write(iu_12(iboat),*)'  (with additional corrosion thickness)'
	  write(iu_12(iboat),*)'        (girder not considered !!!)'
	  write(iu_12(iboat),*)
	  write(iu_12(iboat),'(5x,2a)')
     *'  weight with corrosion thickness  ',
     *                   '            cost (using database)'
	  write(iu_12(iboat),'(5x,2a)')'         (in newton)   ',
     *                   '              (in euro , $ or ...) '
	  write(iu_12(iboat),44)
   44   format(/' panel',t12,'net weight',t24,'gross weight',
     *                   t37,'extra weight ',t51,'total cost',
     *                   t62,'material cost',t76,'labor cost '/)
    	 endif

       write(iu_12(iboat),45) j,ppo,poid1,spoid1,fct,ffmat,coutpan(j)
   45  format(4x,i3,t10,6(f12.3,1x) )   

      if(j.eq.neto) then
	 coutrro = coutrro 
	 cout    = fmat + coutrro*eqpc1  !pour avoir un coût en euro
	 ppo=poids-spoids

       iunit=iu_12(iboat)
	 write(iunit,'(/70(1h*)/)') 
	 write(iunit,* )
     *'total weight: complete structure (girders excluded)'
	 write(iunit,* )
     *'---------------------------------------------------'
       write(iunit,46)'   weight (net   thickness)       = ',ppo   ,' n'
       write(iunit,46)'   weight (corrosion thickness)    = ',spoids,'n'			!sept06
       write(iunit,46)'   weight (gross thickness)       = ',poids ,' n'
       write(iunit,* )
	 write(iunit,* )
     *'total cost: complete structure (girders excluded)'
	 write(iunit,* )
     *'-------------------------------------------------'
       write(iunit,46)'1* material from plating       = ',
     *fmat11,' euro,$'
       write(iunit,46)'   material from long stiff.   = ',
     *fmat22,' euro,$'
       write(iunit,46)'   material from trans. frame  = ',
     *fmat33,' euro,$'
       write(iunit,46)'   material cost (total)       = ',
     *fmat  ,' euro,$'
       write(iunit,* )
       write(iunit,46)'2* labor cost (total)          = ',
     *coutrro*eqpc1,' euro,$'
       write(iunit,46)'     rro      (total)          = ',
     *coutrro,' hours'
       write(iunit,46)'     ti       (total)          = ',
     *totalti,' hours'		
	 write(iunit,46)
       write(iunit,46)'*** total cost  (1+2)          = ',
     *cout  ,' euro,$'
       write(iunit,* )
	endif

      enddo ! fin de la boucle sur le nombre de panneaux

	ENDIF ! fin de la condition sur iprint_

	deallocate (dfctsc)

      return
   46  format(a,f14.3,a)   
	end
