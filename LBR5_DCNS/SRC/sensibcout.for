      subroutine sensibcout(derivee,obj,nn,width)										!18.03.04
      use sharedvar
      implicit real*8(a-h,o-z)
	real*8 inva
      dimension x(10),y(10),s(3),w(2),bsc(2),c(2)
      dimension asc(2,2),inva(2,2),derivee(nn),dfctsc(nn)
      common/alstom/idiff,nbr,pas
	nbr=5 ! provisoire (il est défini plus loin)

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

	eqpc1=eqp*c1*1000.0  ! (=euro/heure) ,pour transfert de rro en euro

c  1. lecture des données courantes	
	rewind 305				!extension neto	!fev2007	
	rewind 302				!dad redressage

	read(305,*) vlength		!extension neto	!fev2007

	do i=1,neto
       read(305,*) nopan,vlarg1,vepaiss1                                  ! nécessaire pour lire entr		!extension neto	!fev2007
	 read(305,*) vhac1,veac1,vlsc1,vesc1,deltac1,vdiffc1				!18.03.04	!extension neto	!fev2007
	 read(305,*) vhar1,vear1,vlsr1,vesr1,deltar1,entrsc(i)				!extension neto	!fev2007
	 read(302)deltacsc(i),deltar(i),vepaiss(i),vhac(i),veac(i),			!extension neto	!fev2007
     *          vlsc(i),vesc(i),vhar(i),vear(i),vlsr(i),vesr(i),			!extension neto	!fev2007
     *		  philsc(i),qsc(i),vdiffc(i)								!18.03.04

       vlarg(i)=philsc(i)*qsc(i)*pi/180.

       dcorsc(i)  = corro(i,1)	!aout2006	!corrosion
	 dcorsc2(i) = corro(i,2)	!aout2006	!corrosion
	 dcorsc3(i) = corro(i,3)	!aout2006	!corrosion

c	 if(i.eq.1) then
c	  rewind 57
c	  read(57,'(////,a1)') abidon
c	  read(57,*) ipan,dcorsc(i),dcorsc2(i),dcorsc3(i)
c	 else
c	  read(57,*) ipan,dcorsc(i),dcorsc2(i),dcorsc3(i)
c	 endif

c      dcorsc  = épaisseur de corrosion pour bordé
c	 dcorsc2 = épaisseur de corrosion pour cadres
c	 dcorsc3 = épaisseur de corrosion pour lisses

	 vepaiss(i) = vepaiss(i) + dcorsc(i)
	 veac(i)    = veac(i)    + dcorsc2(i)
	 vesc(i)    = vesc(i)    + dcorsc2(i)
	 vdiffc(i)	= vdiffc(i)	 + dcorsc2(i)		!18.03.04
	 vear(i)    = vear(i)    + dcorsc3(i)
	 vesr(i)    = vesr(i)    + dcorsc3(i)
	
	enddo

      rewind(305)		!extension neto	!fev2007

c  2. calcul de fmat= coût des matériaux (tôles, raid, cadres)
c    --------------------------------------------------------
c        coutmat(i) = coût matériaux du panneau i
      fmat = 0.
      do i=1,neto
	 temp = philsc(i) * qsc(i) * width * pi/180.
       dens = spec(i)/9.81
	if(isect(i).eq.0) then														!18.03.04
	 coutmat(i) = temp * dens * 
     *  (c1 *(1.+(vepaiss(i)-dref)*1000.*dc1) * vepaiss(i) 
     * + c2 *(1.+(vear(i)   -dref)*1000.*dc1) * (1.0+dw2)   ! dref et pas drefx
     *       * (vear(i)*vhar(i)+vesr(i)*vlsr(i))/deltar(i) 
     * + c3 *(1.+(veac(i)   -dref)*1000.*dc1) * (1.0+dw3)   ! dref et pas drefy
     *       * (veac(i)*vhac(i)+vesc(i)*vlsc(i))/deltacsc(i) ) 
	 elseif(isect(i).eq.1) then													!18.03.04
	 coutmat(i) = temp * dens *													!18.03.04
c     *  ( c2 *(1.+(vear(i)   -dref)*1000.*dc1) * (1.0+dw2)   ! dref et pas drefx	!sept06
c     *                   * (vear(i)*vhar(i)+vesr(i)*vlsr(i))/deltar(i)			!sept06
     * ( c3 *(1.+(veac(i)   -dref)*1000.*dc1) * (1.0+dw3)   ! dref et pas drefy	!sept06
     * *pi*(vhac(i)*vhac(i)-(vhac(i)-2*vdiffc(i))**2) /(4*deltacsc(i)))			!18.03.04
	 elseif(isect(i).eq.2) then													!18.03.04
	 coutmat(i) = temp * dens *													!18.03.04
c     *  ( c2 *(1.+(vear(i)   -dref)*1000.*dc1) * (1.0+dw2)   ! dref et pas drefx	!sept06
c     *                   * (vear(i)*vhar(i)+vesr(i)*vlsr(i))/deltar(i)			!sept06
     * ( c3 *(1.+(veac(i)   -dref)*1000.*dc1) * (1.0+dw3)   ! dref et pas drefy	!18.03.04
     *   *(vhac(i)*vhac(i)-(vhac(i)-2*vdiffc(i))**2) /deltacsc(i))				!18.03.04
	 elseif(isect(i).eq.3) then													!18.03.04
	 coutmat(i) = temp * dens *													!18.03.04
c     *  ( c2 *(1.+(vear(i)   -dref)*1000.*dc1) * (1.0+dw2)   ! dref et pas drefx	!sept06
c     *                   * (vear(i)*vhar(i)+vesr(i)*vlsr(i))/deltar(i)			!sept06
     * ( c3 *(1.+(veac(i)   -dref)*1000.*dc1) * (1.0+dw3)   ! dref et pas drefy	!sept06
     *   *(veac(i)*2* vhac(i)+2*vesc(i)* vlsc(i)) /deltacsc(i))					!18.03.04
	endif																		!18.03.04
      
	 fmat = fmat + coutmat(i)
	enddo


c  3. calcul des coûts de mdo + sensivilités (boucles sur les variables de conception)
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
	call costmain(coutrro,vlength,non,i1,derivee,nn,totalti)
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

      write(666,'(/a,a/60(1h-))')'les paramètres de contrôle du ',
     *   'calcul des sensibilités sont les suivants:'

      if(idiff.eq.1) then
	  nbr=1  ! pas besoin de calculer d'autres points
	  write(666,*) ' idiff=1 : nbr=1'
	  write(666,*) ' calcul direct des sensibilités '
	  write(666,*) '      via 2 pts: xi-pas*xi  et xi+pas*xi'
	else
	  nbr=1 ! au choix de l'utilisateur
	  write(666,*) ' idiff=',idiff ,' nbr=',nbr
	  write(666,*) ' calcul direct des sensibilités (moindres carrés)'
	  write(666,*) ' via ',2*nbr+1,' pts: '
	  write(666,*) ' à savoir: xi-nbr*pas*xi  à  xi+nbr*pas*xi'
	endif

c     choix du pas à gauche et à droite pour le calcul par difference finie.
c     ... x+2*pass*x,  x-pass*x  et x+pass*x , x+2*pass*x  , ... (x = valeur)
	pass = 1/20.0   ! càd 1/20 ème de la valeur de la variable de conception

	write(666,'(a,f8.5,a)')
     * ' et pas =',pass, '(à multiplier par chaque variable de concep)'


  1   continue

      do 5 j=1,neto ! boucle sur le nombre de panneaux

	nbrxi = nvar(j)
	dens = spec(j)/9.81
	temp = philsc(j) * qsc(j) * width * pi/180.

      do 10 i=1,nbrxi  ! boucle sur les variables de conception du panneau j
      ii = i + ncont	! compteur du nbre de variable de conception
      i1 = nxit(i,j)

      if (icout.eq.2) goto 2
     	
c	if (i1.eq.1.or.i1.eq.2.or.i1.eq.5.or.i1.eq.6.or.i1.eq.9) then  ! sinon dérivée nulle
	if (i1.eq.4.or.i1.eq.7.or.i1.eq.8) goto 777  ! sinon dérivée nulle
      
c 3.1 lecture de la valeur de la variable de conception i du panneau j
c      et des bornes min et max de cette variable de conception (cfr base de données)

	if(isect(j).eq.0) then											!18.03.04
      if (i1.eq.1) then          ! rem. : on ne se sert pas du paramètre pente pour l'instant               
	  valeur = vepaiss(j)      ! pente = +1 : la pente "globale" est positive 
	else if (i1.eq.2) then
	  valeur = vhac(j)
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
	  valeur = vhac(j)							!18.03.04
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

	  call costmain(y(k),vlength,non,i1,derivee,nn,totalti)

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
	  dfctsc(ii) = temp*dens*c1*(1 + (2*vepaiss(j)-dref)*1000.*dc1) ! = dérivée du coût des matériaux
	                        
	else if (i1.eq.2) then
	  dfctsc(ii) = temp*dens*c3/deltacsc(j)*
     *        (1 + (veac(j) - dref)*1000.*dc1)*(1. + dw3)*veac(j)
	  
	else if (i1.eq.3) then
	  dfctsc(ii) = temp*dens*c3/deltacsc(j)*
     *        ( 1000.*dc1*(1 + dw3)*(veac(j)*vhac(j) + vesc(j)*vlsc(i))
     *      +   (1 + (veac(j) - dref)*1000.*dc1)*(1 + dw3)*vhac(j)     )
	  
	else if (i1.eq.4) then
	  dfctsc(ii) = temp*dens*c3/deltacsc(j)*
     *        (1 + (veac(j) - dref)*1000.*dc1)*(1 + dw3)*vesc(j)
        
	else if (i1.eq.5) then 
	  dfctsc(ii) = -temp*dens*c3/deltacsc(j)**2 *
     *        (1 + (veac(j) - dref)*1000.*dc1)*(1 + dw3)   
     *         *(veac(j)*vhac(j) + vesc(j)*vlsc(j))
	  
	else if (i1.eq.6) then
	  dfctsc(ii) = temp*dens*c2/deltar(j)*
     *        (1 + (vear(j) - dref)*1000.*dc1)*(1. + dw2)*vear(j)

	else if (i1.eq.7) then
	  dfctsc(ii) = temp*dens*c2/deltar(j)*
     *        ( 1000.*dc1*(1 + dw2)*(vear(j)*vhar(j) + vesr(j)*vlsr(i))
     *      +   (1 + (vear(j) - dref)*1000.*dc1)*(1 + dw2)*vhar(j)     )
	  
      else if (i1.eq.8) then
	  dfctsc(ii) = temp*dens*c2/deltar(j)*
     *        (1 + (vear(j) - dref)*1000.*dc1)*(1 + dw2)*vesr(j)
	
	else if (i1.eq.9) then
	  dfctsc(ii) = -temp*dens*c2/deltar(j)**2 *
     *        (1 + (vear(j) - dref)*1000.*dc1)*(1 + dw2)   
     *         *(vear(j)*vhar(j) + vesr(j)*vlsr(j))
	endif
	endif										!18.03.04
c 3.5 dérivée total : mdo + coût des matériaux (en euro)

      derivee(ii) = derivee(ii) + dfctsc(ii)       ! = dérivée totale du coût en euro

  10  continue ! fin de la boucle sur les variables de conception
      ncont = ncont + nbrxi

   5  continue ! fin de la boucle sur le nombre de panneaux (sur j)

c =====================================================================================

c 3.6 cout total (mat + mdo)
      obj = fmat + coutrro*eqpc1

c  --------------------------------------------------------------------------------
c 3.7 warning si bornes des variables de conception dépassée dans la banque de données
      if (non.gt.1.and.non.lt.200) then
	 numop = non - 100
	 write(666,*)'attention, certaines variables de conception ont', 
     * ' une valeur qui sort de la base de donnees permettant de',
     * ' calculer le cout (-->> valeur trop petite)'
	 write(666,*) 'il s''agit de l''operation : ',numop
	 write(*,*)'attention, certaines variables de conception ont une', 
     * ' valeur qui sort de la base de donnees permettant de calculer',
     * ' le cout (valeur trop petite)'
	 write(*,*) 'il s''agit de l''operation : ',numop


	 write(6970,*)
     *'attention, certaines variables de conception ont une', 
     * ' valeur qui sort de la base de donnees permettant de calculer',
     * ' le cout (valeur trop petite)'
	 write(6970,*) 'il s''agit de l''operation : ',numop


	 write(29,*)'attention, certaines variables de conception ont',			!sept06		!bug
     * ' une valeur qui sort de la base de donnees permettant de',			!sept06		!bug
     * ' calculer le cout (-->> valeur trop petite)'							!sept06		!bug
	 write(29,*) 'il s''agit de l''operation : ',numop						!sept06		!bug

c	 pause
 
      elseif (non.gt.200) then
	 numop = non - 200
	 write(666,*)'attention, certaines variables de conception ont', 
     * ' une valeur qui sort de la base de donnees permettant de',
     * ' calculer le cout (-->> valeur trop grande)'
	 write(666,*) 'il s''agit de l''operation : ',numop
	 write(*,*)'attention, certaines variables de conception ont une', 
     * ' valeur qui sort de la base de donnees permettant de calculer',
     * ' le cout (valeur trop grande)'
	 write(*,*) 'il s''agit de l''operation : ',numop

	 write(6970,*)
     *'attention, certaines variables de conception ont une', 
     * ' valeur qui sort de la base de donnees permettant de calculer',
     * ' le cout (valeur trop grande)'
	 write(6970,*) 'il s''agit de l''operation : ',numop


	write(29,*)'attention, certaines variables de conception ont',			!sept06		!bug
     * ' une valeur qui sort de la base de donnees permettant de',			!sept06		!bug
     * ' calculer le cout (-->> valeur trop grande)'							!sept06		!bug
	 write(29,*) 'il s''agit de l''operation : ',numop						!sept06		!bug



	endif

c =========================================================================================
c =========================================================================================
c
c 4.  impression des différents coûts dans opt
c -------------------------------------------
      poids=0.0

      do j=1,neto
c      fcp = poids = bordage + ames + semelles  (du panneau nel)
       temp = philsc(j) * qsc(j) * width * pi/180.
	 dens = spec(j)/9.81
	if(isect(j).eq.0) then												!18.03.04
       fcp= vepaiss(j) 
     *  +  (1.0+dw3)*(veac(j) * vhac(j) + vesc(j)* vlsc(j)) /deltacsc(j)
     *  +  (1.0+dw2)*(vear(j) * vhar(j) + vesr(j)* vlsr(j)) /deltar(j)
	 elseif(isect(j).eq.1) then											!cercle !18.03.04
       fcp=(1.0+dw3)*pi*(vhac(j)*vhac(j)-(vhac(j)-2*vdiffc(j))**2) /		!18.03.04
     *	 (4*deltacsc(j))													!18.03.04
c     *  +  (1.0+dw2)*(vear(j) * vhar(j) + vesr(j)* vlsr(j)) /deltar(j)	!sept06
	 elseif(isect(j).eq.2) then											!carre !18.03.04
      fcp=(1.0+dw3)*(vhac(j)*vhac(j)-(vhac(j)-2*vdiffc(j))**2)
     *  /deltacsc(j)	!18.03.04
c     *  +  (1.0+dw2)*(vear(j) * vhar(j) + vesr(j)* vlsr(j)) /deltar(j)	!sept06
	 elseif(isect(j).eq.3) then											!doublet !18.03.04
       fcp=(1.0+dw3)*(veac(j)*2* vhac(j)+2*vesc(j)* vlsc(j))/deltacsc(j)	!18.03.04
c     *  +  (1.0+dw2)*(vear(j) * vhar(j) + vesr(j)* vlsr(j)) /deltar(j)	!sept06
	endif																!18.03.04

       fcp=  fcp * temp * spec(j)  ! poids du panneau (borde, raid et cadre only)
	 poids=poids+ fcp

       if (j.eq.1) then
        write(666,*) 
	  write(666,*)'*** cost objective function (subr. sensibcout)'
	  write(666,*)'    ------------------------------------------'
	  write(666,*)'         (with additional corrosion thickness)'
	  write(666,*)'           (girder not considered !!!)'
	  write(666,*)
	  write(666,42)

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

       write(666,43) j,fcp,ctpanel,coutmat(j),
     *                             coutpan(j)*eqpc1,coutpan(j)
   43  format(4x,i3,t10,5(f14.3,1x) )   


       if (j.eq.neto) then
        write(666,*) 
        write(666,* )' total for the structure (with gross thickness)'
        write(666,* )' ----------------------------------------------'
        write(666,46)' cout-cost(mat+mdo) = ',obj  ,' euro (or...)'
        write(666,46)' - total labor cost = ',coutrro*eqpc1,
     *                                                ' euro (or...)'
        write(666,46)'   labor time (rro) = ',coutrro,' hours'
        write(666,46)'   labor time (ti)  = ',totalti,' hours'		!janv2007
	  write(666,46)'    [rate: 1 hour   = ',eqpc1,' euro (or...) ]'
        write(666,46)' - total mat. cost  = ',fmat ,' euro (or...)'
        write(666,46)'   poids - weight   = ',poids,' n'
	 endif

	enddo  ! do j=1,neto

c
c impression des différents coûts dans sol2
c -----------------------------------------

      write(67,*)
	write(67,*)
	 
	do j=1,neto

c     fmat= coût des matériaux (acier)
c     fmdo= coût main d'oeuvre
c     fct = fmat + fmdo  (d'un panneau nel),
c     obj = coût total          (de la structure complete)

      if(j.eq.1) then
	 cout=0. 
	 poids=0.
c	 call annuld(poids,18)!janv2007
	endif

c     dcor = epaisseur de corrosion pour bordé

	write(66,*) 
	write(66,*) ' avec sur-épaisseurs de corrosion'
	write(66,*) ' --------------------------------'
	write(66,*) ' *** sur-épaisseur=',dcorsc(j),' m'
	write(66,*) ' - epaiss bordé      =',vepaiss(j)
	write(66,*) ' - epaiss âme cadres =',veac(j)
	write(66,*) ' - epaiss sem cadres =',vesc(j)
	write(66,*) ' - epaiss âme raid   =',vear(j)
	write(66,*) ' - epaiss sem raid   =',vesr(j)

	dens =  spec(j)/9.81
      temp =  philsc(j) * qsc(j) * width * pi/180.
	temp1=  temp * dens  
	temp2=  temp * rend_global * eqpc1 

c     supplément de poids (en n) (sans les traverses)
	if(isect(j).eq.0) then												!18.03.04
      spoid1 = temp*spec(j) * ( dcorsc(j) +
     *   (1.0+dw2)*(dcorsc3(j)*vhar(j)+dcorsc3(j)*vlsr(j))/deltar(j) +
     *   (1.0+dw3)*(dcorsc2(j)*vhac(j)+dcorsc2(j)*vlsc(j))/deltacsc(j))
       poid1 = temp*spec(j) * ( vepaiss(j) +
     *   (1.0+dw2)*(vear(j)*vhar(j)+vesr(j)*vlsr(j))/deltar(j) +
     *   (1.0+dw3)*(veac(j)*vhac(j)+vesc(j)*vlsc(j))/deltacsc(j)  )
	 elseif(isect(j).eq.1) then												!18.03.04
      spoid1 = temp*spec(j) *													!18.03.04
c     *   ((1.0+dw2)*(dcorsc3(j)*vhar(j)+dcorsc3(j)*vlsr(j))/deltar(j) +		!sept06
     *   (1.0+dw3)*pi*(vhac(j)*vhac(j)-(vhac(j)-2*dcorsc2(j))**2)/			!18.03.04
     *   (4*deltacsc(j)  )!)													!sept06
       poid1 = temp*spec(j) * (! vepaiss(j) +									!sept06
c     *   (1.0+dw2)*(vear(j)*vhar(j)+vesr(j)*vlsr(j))/deltar(j) +				!sept06
     *   (1.0+dw3)*pi*(vhac(j)*vhac(j)-(vhac(j)-2*vdiffc(j))**2)/		!18.03.04
     *   (4*deltacsc(j)))													!18.03.04
	elseif(isect(j).eq.2) then												!18.03.04
	 spoid1 = temp*spec(j) *												!18.03.04
c     *   ((1.0+dw2)*(dcorsc3(j)*vhar(j)+dcorsc3(j)*vlsr(j))/deltar(j) +		!sept06
     *   (1.0+dw3)*(vhac(j)*vhac(j)-(vhac(j)-2*dcorsc2(j))**2)/			!18.03.04
     *   deltacsc(j) ! )														!sept06
       poid1 = temp*spec(j) * ( vepaiss(j) +									!18.03.04
c     *   (1.0+dw2)*(vear(j)*vhar(j)+vesr(j)*vlsr(j))/deltar(j) +				!sept06
     *   (1.0+dw3)*(vhac(j)*vhac(j)-(vhac(j)-2*vdiffc(j))**2) /		!18.03.04
     *   (deltacsc(j)))													!18.03.04
	elseif(isect(j).eq.3) then												!18.03.04
	 spoid1 = temp*spec(j) *												!sept06
c     * ((1.0+dw2)*(dcorsc3(j)*vhar(j)+dcorsc3(j)*vlsr(j))/deltar(j) +		!sept06
     * (1.0+dw3)*(dcorsc2(j)*2*vhac(j)+2*dcorsc2(j)*vlsc(j))
     * /deltacsc(j)!)		!sept06
       poid1 = temp*spec(j) * ( !vepaiss(j) +									!sept06
c     *   (1.0+dw2)*(vear(j)*vhar(j)+vesr(j)*vlsr(j))/deltar(j) +				!sept06
     *   (1.0+dw3)*(veac(j)*2*vhac(j)+2*vesc(j)*vlsc(j))/deltacsc(j))		!18.03.04
	endif																	!18.03.04

	spoids=spoids+spoid1 ! supplément de poids dû à la corrosion
	poids =poids+ poid1 ! poids + poids additionnel dû à dw2 et dw3

	if(isect(j).eq.0) then													!18.03.04
c     coût des plaques
      fmat1 = temp1 *c1* (1.+(vepaiss(j)-dref)*1000.*dc1) * vepaiss(j) 
c     coût des lisses
      fmat2 = temp1 *c2* (1.+dw2) * (1.+(vear(j)-dref)*1000.*dc1)
     *                     * (vear(j)*vhar(j)+vesr(j)*vlsr(j))/deltar(j)  
c     coût des cadres
      fmat3 = temp1 *c3* (1.+dw3) * (1.+(veac(j)-dref)*1000.*dc1)
     *      * (veac(j)*vhac(j)+vesc(j)*vlsc(j))/deltacsc(j)  
      
	elseif(isect(j).eq.1) then												!26.05.04
	fmat1 = 0.000															!26.05.04
	fmat2 = 0.000															!26.05.04
	fmat3 =  temp1 *c3* (1.+dw3) *(1.+(veac(j)-dref)*1000.*dc1)				!26.05.04
     *   *pi*(vhac(j)*vhac(j)-(vhac(j)-2*vdiffc(j))**2) /(4*deltacsc(j))		!26.05.04
      elseif(isect(j).eq.2) then												!26.05.04
	fmat1 = 0.000															!26.05.04
	fmat2 = 0.000															!26.05.04
	fmat3 =  temp1 *c3* (1.+dw3) *(1.+(veac(j)-dref)*1000.*dc1)				!26.05.04
     *   *(vhac(j)*vhac(j)-(vhac(j)-2*vdiffc(j))**2) /deltacsc(j)				!26.05.04
      elseif(isect(j).eq.3) then												!26.05.04
	fmat1 = 0.000															!26.05.04
	fmat2 = 0.000															!26.05.04
	fmat3 =  temp1 *c3* (1.+dw3) *(1.+(veac(j)-dref)*1000.*dc1)				!26.05.04
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
        write(67,*) 
	  write(67,*)'*** cost and weight (subr. sensibcout)'
	  write(67,*)'--------------------------------------'
	  write(67,*)'  (with additional corrosion thickness)'
	  write(67,*)'        (girder not considered !!!)'
	  write(67,*)
	  write(67,'(5x,2a)')'  weight with corrosion thickness  ',
     *                   '            cost (using database)'
	  write(67,'(5x,2a)')'         (in newton)               ',
     *                   '              (in euro , $ or ...) '
	  write(67,44)
   44   format(/' panel',t12,'net weight',t24,'gross weight',
     *                   t37,'extra weight ',t51,'total cost',
     *                   t62,'material cost',t76,'labor cost '/)
    	 endif

       write(67,45) j,ppo,poid1,spoid1,fct,ffmat,coutpan(j)
   45  format(4x,i3,t10,6(f12.3,1x) )   

      if(j.eq.neto) then
	 coutrro = coutrro 
	 cout    = fmat + coutrro*eqpc1  !pour avoir un coût en euro
	 ppo=poids-spoids

       iz=67
	 write(iz,'(/70(1h*)/)') 
	 write(iz,* )'total weight: complete structure (girders excluded)'
	 write(iz,* )'---------------------------------------------------'
       write(iz,46)'   weight (net   thickness)       = ',ppo   ,' n'
       write(iz,46)'   weight (corrosion thickness)       = ',spoids,'n'			!sept06
       write(iz,46)'   weight (gross thickness)       = ',poids ,' n'
       write(iz,* )
	 write(iz,* )'total cost: complete structure (girders excluded)'
	 write(iz,* )'-------------------------------------------------'
       write(iz,46)'1* material from plating       = ',fmat11,' euro,$'
       write(iz,46)'   material from long stiff.   = ',fmat22,' euro,$'
       write(iz,46)'   material from trans. frame  = ',fmat33,' euro,$'
       write(iz,46)'   material cost (total)       = ',fmat  ,' euro,$'
       write(iz,* )
       write(iz,46)'2* labor cost (total)          = ',coutrro*eqpc1,
     *                                                        ' euro,$'
       write(iz,46)'     rro      (total)          = ',coutrro,' hours'
       write(iz,46)'     ti       (total)          = ',totalti,' hours'			!sept06
	 write(iz,46)
       write(iz,46)'*** total cost  (1+2)          = ',cout  ,' euro,$'
       write(iz,* )
	endif

      enddo ! fin de la boucle sur le nombre de panneaux

      return
   46  format(a,f14.3,a)   
	end