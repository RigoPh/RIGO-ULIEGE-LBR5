      SUBROUTINE COORD(NETO,PHILM,QN,IMPR,TETAS,DELT,DELT2,
     *                 XNEUT,YNEUT,Z,NOEUD,NVAR,NXIT,IMG,
     *                 A,ITRAV,ITYPE,ISECT,IMPR2,						!15.10.05   !Vecteurs de travail   !février 2004
     *				 NCONDI,NNO,SYMX,SYMY,INERT,IMOD,				!flexion d'axe vert.   !restri inertie   !restri module
     *				 IXXTOT,OMET,SYM,PART,IXXTOTPART,YNEUTPART)		!obj inertie  !r&d13  !r&d13_tranchant			

      IMPLICIT REAL *8 (A-H,O-Z)
	DIMENSION A(1),ITRAV(1)		! Vecteurs de travail
	REAL *8 IXXTOT				!obj inertie

      I1 =1			! PHILN(NETO)
	I2 =I1 +NETO	! XNEU(NETO)
	I3 =I2 +NETO	! YNEU(NETO)
	I4 =I3 +NETO	! DD1(NETO)
	I5 =I4 +NETO	! DD2(NETO)
	I6 =I5 +NETO	! OMEGA(NETO)
	I7 =I6 +NETO	! OMEGA2(NETO)
	I8 =I7 +NETO	! AONO(2NETO+2)
	I9 =I8 +2*NETO+2 ! IXX(NETO)
	I10=I9 +NETO	! IYY(NETO)
	I11=I10+NETO	! IXX2(NETO)
	I12=I11+NETO	! IYY2(NETO)
	I13=I12+NETO	! DKG(9NETO)
	I14=I13+9*NETO	! DC(9NETO)
	I15=I14+9*NETO	! DS(9)
	I16=I15+9		! IXXPART(NETO)		!r&d13
	I17=I16+9*NETO	! = 40*NETO +11

      CALL COORD2(NETO,PHILM,QN,IMPR,TETAS,DELT,DELT2,
     *            XNEUT,YNEUT,Z,NOEUD,NVAR,NXIT,IMG,
     *            A(1),A(I2),A(I3),A(I4),A(I5),A(I6),A(I7),A(I8),			! Vecteurs de travail
     *            A(I9),A(I10),A(I11),A(I12),A(I13),A(I14),A(I15),		! Vecteurs de travail
     *            A(I16),ITRAV(1),ITRAV(1+NETO),ITYPE,ISECT,IMPR2, 		! 15.10.05   !Vecteurs de travail   !février 2004	!r&d13
     *			NCONDI,NNO,SYMX,SYMY,INERT,IMOD,IXXTOT,OMET,SYM,		! flexion d'axe vert.   !restri inertie   !restri module   !obj inertie	  
     *			PART,IXXTOTPART,YNEUTPART)								!r&d13  !r&d13_tranchant

      RETURN
	END
C***********************************************************************
C***********************************************************************
      SUBROUTINE COORD2(NETO,PHILM,QN,IMPR,TETAS,DELT,DELT2,
     *                  XNEUT,YNEUT,Z,NOEUD,NVAR,NXIT,IMG,
     *                  PHILN,XNEU,YNEU,DD1,DD2,OMEGA,OMEGA2,AONO,		! Vecteurs de travail
     *                  IXX,IYY,IXX2,IYY2,DKG,DC,DS,IXXPART,				! Vecteurs de travail	!r&d13
     *                  NCH,N1,ITYPE,ISECT,IMPR2,							! 15.10.05   ! Vecteurs de travail   !février 2004
     *				  NCONDI,NNO,SYMX,SYMY,INERT,IMOD,IXXTOT,OMET,SYM,	! flexion d'axe vert.   !restri inertie   !restri module   !obj inertie
     *				  PART,IXXTOTPART,YNEUTPART)						! r&d13
 
      IMPLICIT REAL *8 (A-H,O-Z)

	INTEGER INO(2),IPAN(2),SYMX,SYMY,SYM							! flexion d'axe vert.  !obj inertie
      
	REAL *8 IYY(NETO),IXX(NETO),IYYTOT,IXXTOT,IUUTOT,
     *        IYYTOT2,IXXTOT2,IUUTOT2,								!obj inertie
     *        IYY2(NETO),IXX2(NETO),KGMIN,KGMAX,KGX,KGY,IMIN,			! 5-11-2000   !restri inertie
     *		MOD,MODMIN,												!restri module
     *		IXXPART(NETO),IXXTOTPART								!r&d13	
	CHARACTER *12 TEXT												! 5-11-2000

	COMMON/DIM1/Nmax    !février 2004

	DIMENSION NVAR(NETO),NXIT(9,NETO)	   							! 5-11-2000

      DIMENSION PHILM(NETO),QN(NETO),TETAS(NETO),DELT(NETO),Z(NETO,4),
     *          NOEUD(NETO,2),DELT2(NETO)
         
	DIMENSION OMEGA2(NETO),DKG(9*NETO),DC(9*NETO),DS(9),DIXX(9)		 ! vect de travail   ! 5-11-2000  !restri inertie
	DIMENSION PHILN(NETO),XNEU(NETO),YNEU(NETO),DD1(NETO),DD2(NETO), ! Vecteurs de travail
     *          OMEGA(NETO),AONO(NETO+1,2)						     ! Vecteurs de travail
	DIMENSION NCH(NETO),N1(NETO+1)                                   ! Vecteurs de travail
	DIMENSION NNO(NETO+1,2)											 ! flexion d'axe vert.

	DIMENSION ITYPE(NMax) !février 2004

	DIMENSION PART(NETO)  !r&d13
 
      COMMON/NOEUD/NTN    ! NTN = Nbr de noeuds
      COMMON/PY/PI
      COMMON/LANGUE/LANGUE
      COMMON/OPTI/  IOPTI,NTOT   ! NTOT = Nbr total de variable de conception
	COMMON/GRAV/KGY,KGMIN,KGMAX,XK,YK,IGRAV

C***********************************************************************
C
C     CALCUL DES COORDONNEES X ET Y des extremites des panneaux dans le repere utilisateur (KXY).
c     CALCUL des axes neutres et des inerties flexionnelles relatives a ces axes neutres.
C     CALCUL DU CENTRE DE GRAVITE (CG dans repere utilisateur) et
c                                 des inerties de masse relatives aux axes passant par le CG
C
c
C     DELT = EPAISSEUR MOYENNE (RAIDISSEURS ET TRAVERSES COMPRIS),
c            utilise pour le calcul des axes neutres et des inerties flexionnelles
C     DELT2= EPAISSEUR MOYENNE (RAIDISSEURS, CADRES ET TRAVERSES COMPRIS),
c            utilise pour le calcul du centre de gravite et des inerties de masse
c
c   ATTENTION:
c   Pour le calcul des inerties, centre de gravite, etc, les panneaux ortotopes sont
c   transformes en panneaux a epaisseur constante. Cela signifie que les inerties 
c   locales des paneaux autour de leur axe faible (oy) sont negligees. Ce signifie aussi
c   que le centre de gravite d'un panneau est approximatif. Pour une plaque il est toujours
c   situe au niveau du borde (z=0) et a mi-hauteur (y=H/2).
c
C
C     Modifications:                       Création: Thèse Ph. RIGO, LBR-4 (1988)
c     ------------						 --------- 
C        7-10-99 :  Version LBR 5.1
c        5-11-2000: Definition d'un repere utilisateur
C       10-11-2000: restriction sur Centre de gravite LBR-5.3c
c
C***********************************************************************
c
C   Lecture des donnees sur le repere utilisateur et sur le centre de gravite
c   ==========================================================================
c Le repere initial de LBR-5 (OXgYg) a son origine a l'extremite du panneau 1.
c  - Axe OXg est horizontal et positif vers la droite,
c  - Axe OYg est vertical et positif vers le haut,
c
c Le repere UTILISATEUR de LBR-5 (OKY) a son origine au point de K.
c  - Le point K a les coord (XK,YK) dans le repere initial (fournies dans les donnees)
c          X = Xg - XK
c          Y = Yg - YK
c  - Axe OX est horizontal et positif vers la droite,
c  - Axe OY est vertical et positif vers le haut,

c
      PI1=PI/180.						!juillet2003
      READ(55,*) TEXT					! titre
      READ(55,*) XK,YK				! XK et YK = Coord du centre K du repere utilisateur
      READ(55,*) IGRAV,KGMIN,KGMAX	! KGMIN=KG(min) et KGMAX=KG(max)

	READ(55,*) TEXT					!restri inertie
	READ(55,*) INERT,IMIN			!restri inertie

	READ(55,*) TEXT					!restri module
	READ(55,*) IMOD,MODMIN,IELT		!restri module


c
c  IGRAV		= 0			Pas de restriction sur la position du centre de gravite,
c			= 1,2 ou 3  Restriction sur le centre de gravite (si IOPTI>0)
c        IGRAV=1  KG > KG(min)  valeur minimale imposee (1 RESTRICTION)
c        IGRAV=2  KG < KG(min)  valeur maximale imposee (1 RESTRICTION)
c        IGRAV=3  KG > KG(min) et  (2 RESTRICTIONS)
c                 KG < KG(min)    
c
C            avec KG la position du centre de gravite (G)
c                    par rapport au point (K) , centre du repere utilisateur
c ---------------------------------------------------------------
c
      DO 2 I=1,NETO
        NCH(I)=0
        PHILN(I)=-PHILM(I)
      DO 2 J=1,4
        Z(I,J)=0.0
   2  CONTINUE
C
	IF(IMPR2.GE.1) THEN				!15.10.05
        IF(LANGUE==1) WRITE(66,121)XK,YK,NTN  ! XK et YK = Coord du repere utilisateur
        IF(LANGUE==2) WRITE(66,221)XK,YK,NTN 
	ENDIF				!15.10.05

C
C 1  Calcul des AONO, Coord des NTN Noeuds dans le repere initial (OXgYg)a l'ext. du panneau 1
C -------------------
      DO 110 J2=1,NTN  ! NTN = Nbre total de noeuds
        N1(J2)=0
        AONO(J2,1)=0.0
        AONO(J2,2)=0.0
 110  CONTINUE
C
      N2=1
      N1(N2)=1
C
c     DO 2000 KT=1,NTN      ! NTN = Nbre total de noeuds
      DO 2000 KT=1,NETO+1   ! NETO+1 = Nbre theorique max de noeuds
c  	  WRITe(67,*)
c	  WRITe(67,*)' **** Boucle 2000: KT=',KT, 'N2=',N2
        IF(KT.EQ.N2) GOTO 118
        N6=KT+1
c	  WRITe(67,*)' * Boucle 500 '
        DO 500 JJ=N6,N2
        DO 500 KK=1,NETO
c	    WRITe(67,*)' JJ=',JJ,' pan=',KK
c	    WRITe(67,*)'         NCH(',KK,')=',NCH(KK)
          IF(NCH(KK).EQ.1) GOTO 500
          NDD=NOEUD(KK,1)
          NAA=NOEUD(KK,2)
          IF((N1(KT).EQ.NDD).AND.(N1(JJ).EQ.NAA)) NCH(KK)=1
          IF((N1(KT).EQ.NAA).AND.(N1(JJ).EQ.NDD)) NCH(KK)=1
c	    WRITe(67,*)' NDD,NAA,N1(KT),N1(JJ)=',NDD,NAA,N1(KT),N1(JJ)
c	    WRITe(67,*)'    new  NCH(',KK,')=',NCH(KK)
  500   CONTINUE
C
  118   NN=N1(KT)
c	  WRITe(67,*)
c	  WRITe(67,*)' * Boucle 200 NN=',NN
        DO 200 J=1,NETO  ! NETO = Nbre total de panneaux
c	    WRITe(67,*)'3- NCH(',J,')=',NCH(J)
          IF(NCH(J).EQ.1) GOTO 200
          NDD=NOEUD(J,1)
          NAA=NOEUD(J,2)
          X=+1.0
          NQ=NAA

c	    WRITe(67,*)'    NDD,NAA,NN=',NDD,NAA,NN
          IF(NDD.EQ.NN) GOTO 190
          IF(NAA.EQ.NN) GOTO 180
          GOTO 200
  180     X=-1.
          NQ=NDD
  190     SPH2=VSIN(PHILN(J)/2.,0.D00)

          S2=VSIN(TETAS(J),PHILN(J)/2.)
          C2=VCOS(TETAS(J),PHILN(J)/2.)
          AONO(NQ,1)=AONO(NN,1)+X*2.*QN(J)*SPH2*S2
          AONO(NQ,2)=AONO(NN,2)-X*2.*QN(J)*SPH2*C2

c	    WRITe(67,*)'$$ AONO(',NQ,',1 et 2) calculé'
          N2=N2+1
          N1(N2)=NQ
          NCH(J)=1
c	    WRITe(67,*)'   N2=',N2,' N1(',N2,')=',NQ,' NCH(',J,')=1'
  200   CONTINUE
 2000 CONTINUE

C 2.0  REPERE UTILISATEUR
c ------------------------
C 2.1 CHANGEMENT DE REPERE, COORD DE AONO dans repere utilisateur KXY 
C

      DO J2=1,NTN  ! NTN = Nbre total de noeuds
        AONO(J2,1)=AONO(J2,1)-XK  ! Coord de K =(XK,YK) dans repere initial
        AONO(J2,2)=AONO(J2,2)-YK
      ENDDO

C
C 2.2   Z(KD,i) = Coord (X,Y) DES PTS ORIGINES ET ARRIVEES DES PANNEAUX (KD=1,NETO)
c                           dans le repere utilisateur

	IF(IMPR2.GE.0) THEN		!15.10.05
        IF(LANGUE==1) WRITE(66,120)
        IF(LANGUE==2) WRITE(66,130)
	ENDIF				!15.10.05
      DO 600 KD=1,NETO
        NDD=NOEUD(KD,1)
        NAA=NOEUD(KD,2)
        Z(KD,1)=AONO(NDD,1)  ! Coord  X du noeud depart
        Z(KD,2)=AONO(NAA,1)  ! Coord  X du noeud d'arrivee
        Z(KD,3)=AONO(NDD,2)  ! Coord  Y du noeud depart
        Z(KD,4)=AONO(NAA,2)  ! Coord  Y du noeud d'arrivee
	  IF(IMPR2.GE.0) THEN		!15.10.05
          WRITE(66,104) KD,Z(KD,1),Z(KD,3),Z(KD,2),Z(KD,4)
	  ENDIF				!15.10.05
  600 CONTINUE


C     Vérification de la longueur des panneaux                           !juillet2003
C     ----------------------------------------                           !juillet2003

      DO NEL=1,NETO
	  CORDE = 2*DABS(QN(NEL)*DSIN(PHILN(NEL)/2*PI1))
	  IF (PHILN(NEL).GT.0.) THEN
	    TETA = (90. - TETAS(NEL))*PI1
	  ELSE
	    TETA = (-90. - TETAS(NEL))*PI1
	  ENDIF
	  X2 = CORDE*DCOS(-TETA + (PHILN(NEL)/2)*PI1)
	  Y2 = CORDE*DSIN(-TETA + (PHILN(NEL)/2)*PI1)

        ERREUR = DSQRT((Z(NEL,1)+X2-Z(NEL,2))**2
     *                 + (Z(NEL,3)+Y2-Z(NEL,4))**2)
        IF (ERREUR.GT.0.001) THEN
	    WRITE(*,*) 'ATTENTION : ERROR dans le Panneau ',NEL            
		WRITE(*,*) '****************************************'          
	WRITE(*,*)'Les coordonnées de ce panneau sont incohérentes avec la 
     *longueur introduite par l''utilisateur'	
	WRITE(*,*)'Ordre de grandeur de l''erreur (en m) :',ERREUR
	WRITE(29,*) 'ATTENTION : ERROR dans le Panneau ',NEL						!bug         
	WRITE(29,*) '****************************************'						!bug        
	WRITE(29,*)'Les coordonnées de ce panneau sont incohérentes avec			!bug 
     *la longueur introduite par l''utilisateur'									!bug 
	WRITE(29,*)'Ordre de grandeur de l''erreur (en m) :',ERREUR					!bug 
	WRITE(29,*)' '																!bug 

c        PAUSE   
        ENDIF
	ENDDO

!!!      DO NEL=1,NETO                                                      !juillet2003
!!!       VLONPAN1 = DABS(PHILN(NEL))*QN(NEL)*PI1                           !juillet2003
!!!	 VLONPAN2 = DSQRT((Z(NEL,4)-Z(NEL,3))**2 + (Z(NEL,2)-Z(NEL,1))**2) !juillet2003
!!!	IF(VLONPAN1.LT.0.99*VLONPAN2.OR.VLONPAN1.GT.1.01*VLONPAN2)THEN     !juillet2003
!!!	    WRITE(*,*) 'ATTENTION : ERROR dans le Panneau ',NEL            !juillet2003
!!!		WRITE(*,*) '****************************************'          !juillet2003
!!!	WRITE(*,*)'Les coordonnées de ce panneau sont incohérentes avec la 
!!!     *longueur introduite par l''utilisateur'                            !juillet2003
!!!c          STOP                                                          !juillet2003
!!!        ENDIF                                                            !juillet2003
!!!	ENDDO                                                              !juillet2003

C
C 3   CALCUL DES INERTIES FLEXIONNELLES (IXX et IYY) 
C ---------------------------------------------------
c     autour des AXES KX et KY (REPERE UTILISATEUR)

      OMET =0.
      OMESX=0.
      OMESY=0.
      OMET2 =0.
      OMESX2=0.
      OMESY2=0.
	OMETPART=0.		!r&d13
	OMESYPART=0.	!r&d13

      DO 300 J=1,NETO
      SPH1=VSIN(PHILN(J),0.D00)
      SPH2=VSIN(PHILN(J)/2.,0.D00)
      C1=VCOS(TETAS(J),0.D00)
      C2=VCOS(TETAS(J),PHILN(J)/2.)
      C3=VCOS(2.*TETAS(J),PHILN(J))
      S1=VSIN(TETAS(J),0.D00)
      S2=VSIN(TETAS(J),PHILN(J)/2.)
      DELTA =DELT(J)  ! epaisseur moyenne sans les cadres
      DELTA2=DELT2(J) ! epaisseur moyenne avec les cadres
      Q=QN(J)
      PHIL=PHILN(J)*PI/180.
      D1=Q*(C1-2.*SPH2*C2/PHIL) ! (D1,D2)= Coord du centre des axes neutres du panneau J
      D2=Q*(S1-2.*SPH2*S2/PHIL) !          par rapport au pt de depart du panneau
      DD1(J)=D1
      DD2(J)=D2
      OMEGA (J)=DABS(DELTA *Q*PHIL) ! section du panneau J (sans cadre)
      OMEGA2(J)=DABS(DELTA2*Q*PHIL) ! section du panneau J (avec cadre)
      IF(DABS(PHIL).GT.0.00001) THEN
        TEMP =-D1+Q*C1                    ! ** Cas des coques
        TEMP2= D2-Q*S1
        IYY(J)=DELTA*Q*DABS(Q*Q*(PHIL/2.+0.5*SPH1*C3)			!Inertie autour axe vertical
     *                -4.*Q*TEMP*SPH2*C2+TEMP*TEMP*PHIL)
        IXX(J)=DELTA*Q*DABS(Q*Q*(PHIL/2.-0.5*SPH1*C3)			!Inertie autour axe horizontal
     *                +4.*Q*TEMP2*SPH2*S2+TEMP2*TEMP2*PHIL)
	  IXXPART(J)=PART(J)*IXX(J)								!Inertie autour axe horizontal avec coef. part.	!r&d13
      ELSE
        IYY(J)=((S1*Q*PHIL)**2+(C1*DELTA)**2)*DELTA*Q*DABS(PHIL)/12. ! ** Cas des plaques
        IXX(J)=((C1*Q*PHIL)**2+(S1*DELTA)**2)*DELTA*Q*DABS(PHIL)/12.
	  IXXPART(J)=((C1*Q*PHIL)**2+(S1*PART(J)*DELTA)**2)*		   !r&d13
     *			 PART(J)*DELTA*Q*DABS(PHIL)/12.					   !avec coef. part.
      ENDIF
	! sans cadre
      OMET =OMET +OMEGA(J)				! section totale
      OMESX=OMESX+OMEGA(J) *(Z(J,1)+D1)	! moment statique horizontal
      OMESY=OMESY+OMEGA(J) *(Z(J,3)+D2)	! moment statique vertical
	OMETPART=OMETPART  +PART(J)*OMEGA(J)				! section totale avec coef. part.			!r&d13
	OMESYPART=OMESYPART+PART(J)*OMEGA(J)*(Z(J,3)+D2)	! moment statique vertical avec coef. part.	!r&d13
	! avec cadre
	IXX2(J)=IXX(J)*DELTA2/DELTA
	IYY2(J)=IYY(J)*DELTA2/DELTA
      OMET2 =OMET2 +OMEGA2(J)               ! section totale
      OMESX2=OMESX2+OMEGA2(J) *(Z(J,1)+D1)  ! moment statique horizontal
      OMESY2=OMESY2+OMEGA2(J) *(Z(J,3)+D2)  ! moment statique vertical
  300 CONTINUE
C
c   Position des axes neutres (dans le repere utilisateur)
c   ------------------------------------------------------
	! sans cadre
      SYMX=0
	SYMY=0
	SYM=0
	IF(NCONDI.EQ.0) THEN											!flexion d'axe vert.
	  GOTO 17
	ELSE
	  DO I=1,2
	    INO(I)=0
	    IPAN(I)=0
	  ENDDO
	  DO I=1,NTN
	    IF(NNO(I,2).EQ.7) THEN
		  IF(SYM.EQ.0) THEN
		    SYM=SYM+1
	        INO(SYM)=I
	        DO L=1,NETO
		     IF((NOEUD(L,1).EQ.I).OR.(NOEUD(L,2).EQ.I)) IPAN(SYM)=L
		    ENDDO 
		  ELSE
			XI=1000*AONO(I,1)
			XK=1000*AONO(INO(SYM),1)
			YI=1000*AONO(I,2)
			YK=1000*AONO(INO(SYM),2)
			IF((DABS(XI-XK).GT.10.).AND.(DABS(YI-YK).GT.10.)) THEN
			  SYM=SYM+1
		      INO(SYM)=I
	          DO L=1,NETO
		        IF((NOEUD(L,1).EQ.I).OR.(NOEUD(L,2).EQ.I)) IPAN(SYM)=L
		      ENDDO 
	        ENDIF
		  ENDIF
	    ENDIF
	  ENDDO
	ENDIF
	IF(SYM.EQ.0) GOTO 17												!flexion d'axe vert.
	IF(SYM.EQ.1) THEN
	  NNOEUD=INO(1)
	  NOEUD1=NOEUD(IPAN(1),1)
	  NOEUD2=NOEUD(IPAN(1),2)
	  X1=1000*AONO(NOEUD1,1)
	  X2=1000*AONO(NOEUD2,1)
	  IF(IABS(IDNINT(X1)-IDNINT(X2)).LT.1) THEN
	    YNEUT=AONO(NNOEUD,2)
		YNEUTPART=AONO(NNOEUD,2)					!r&d13
		SYMX=1
		XNEUT=OMESX/OMET
	  ELSE
		XNEUT=AONO(NNOEUD,1)
		SYMY=1
		YNEUT=OMESY/OMET
	    YNEUTPART=OMESYPART/OMETPART				!r&d13
	  ENDIF
	  GOTO 18
	ENDIF
	IF(SYM.EQ.2) THEN
	  NNOEUD1=INO(1)
	  NNOEUD2=INO(2)
	  NOEUD1=NOEUD(IPAN(1),1)
	  NOEUD2=NOEUD(IPAN(1),2)
	  X1=1000*AONO(NOEUD1,1)
	  X2=1000*AONO(NOEUD2,1)
	  SYMX=1
	  SYMY=1
	  IF(IABS(IDNINT(X1)-IDNINT(X2)).LT.1) THEN
	    YNEUT=AONO(NNOEUD1,2)
	    YNEUTPART=AONO(NNOEUD1,2)					!r&d13
	    XNEUT=AONO(NNOEUD2,1)
	  ELSE
		XNEUT=AONO(NNOEUD1,1)
	    YNEUT=AONO(NNOEUD2,2)
		YNEUTPART=AONO(NNOEUD2,2)					!r&d13
	  ENDIF
	  GOTO 18
	ENDIF
		
   17	XNEUT=OMESX/OMET
      YNEUT=OMESY/OMET
	YNEUTPART=OMESYPART/OMETPART					!r&d13

   18 CONTINUE
						
C
c   Position du centre de gravite (KGX,KGY) (dans le repere utilisateur, KXY)
c   -------------------------------------------------------------------------
	! avec cadre
    4	IF(SYM.EQ.0) GOTO 19											!flexion d'axe vert.
	
	IF(SYM.EQ.1) THEN
	  IF(IABS(IDNINT(X1)-IDNINT(X2)).LT.1) THEN
	    KGY=AONO(NNOEUD,2)
		KGX=OMESX2/OMET2
	  ELSE
		KGX=AONO(NNOEUD,1)
		KGY=OMESY2/OMET2
	  ENDIF
	  GOTO 20
	ENDIF
	IF(SYM.EQ.2) THEN
	  IF(IABS(IDNINT(X1)-IDNINT(X2)).LT.1) THEN
	    KGY=AONO(NNOEUD1,2)
	    KGX=AONO(NNOEUD2,1)
	  ELSE
		KGX=AONO(NNOEUD1,1)
	    KGY=AONO(NNOEUD2,2)
	  ENDIF
	  GOTO 20
	ENDIF
		
   19	KGX=OMESX2/OMET2
      KGY=OMESY2/OMET2

   20 CONTINUE														!flexion d'axe vert.


C 4  RESTRICTION SUR LA POSITION DU CENTRE DE GRAVITE KG (=KGY)
C ---------------------------------------------------
      IF(IOPTI.EQ.0) GOTO 14 ! pas d'optimisation

c     Coordonnees dans les axes utilisateurs (KXY)
c
c     IGRAV	= 0			Pas de restriction sur la position du centre de gravite,
c			= 1,2 ou 3  Restriction sur le centre de gravite (si IOPTI>0)
c        IGRAV=1  KG > KG(min)  valeur minimale imposee (1 RESTRICTION)
c        IGRAV=2  KG < KG(min)  valeur maximale imposee (1 RESTRICTION)
c        IGRAV=3  KG > KG(min) et  (2 RESTRICTIONS)
c                 KG < KG(min)    
c
C            avec KG la position du centre de gravite (G)
c                    par rapport au point (K) , centre du repere utilisateur

	WRITE(666,*)'IGRAV ',IGRAV							!eugen (14.10.2007)
	IF(IGRAV==0) THEN ! pas de restriction sur le KG
	  WRITE(66,'(/A/1x,55(1H*)/)')
     *    ' PAS DE RESTRICTION SUR LA POSITION DU CENTRE DE GRAVITE'
	  WRITE(666,'(/A/1x,55(1H*)/)')
     *	' PAS DE RESTRICTION SUR LA POSITION DU CENTRE DE GRAVITE'
	  WRITE(666,*)'  KG actuel  =', KGY,' m'			!eugen (14.10.2007)
	  WRITE(29 ,'(/A/1x,55(1H*)/)')													!bug
     *	' PAS DE RESTRICTION SUR LA POSITION DU CENTRE DE GRAVITE'					!bug
	  GOTO 14
	ENDIF

	WRITE(66,'(/A/1x,48(1H*)/)')
     *    ' RESTRICTION SUR LA POSITION DU CENTRE DE GRAVITE'
	WRITE(666,'(/A/1x,48(1H*)/)')
     *	' RESTRICTION SUR LA POSITION DU CENTRE DE GRAVITE'
	WRITE(66 ,*)'  KG actuel  =', KGY,' m'
	WRITE(666,*)'  KG actuel  =', KGY,' m'

	IF(IGRAV==1) THEN ! KGmin impose
	  WRITE(29,*)'  1 restriction est imposee : KGMIN < KG'							!bug
	  WRITE(66,*)'  1 restriction est imposee : KGMIN < KG'
	  WRITE(66,*)'  KG minimum =', KGMIN,' m'
	  WRITE(666,*)'  1 restriction est imposee : KGMIN < KG'
	  WRITE(666,*)'  KG minimum =', KGMIN,' m'
	ELSE IF(IGRAV==2) THEN
	  WRITE(29,*)'  1 restriction est imposee : KG < KGMAX'							!bug
	  WRITE(66,*)'  1 restriction est imposee : KG < KGMAX'
	  WRITE(66,*)'  KG maximum =', KGMAX,' m'
	  WRITE(666,*)'  1 restriction est imposee : KG < KGMAX'
	  WRITE(666,*)'  KG maximum =', KGMAX,' m'
	ELSE IF(IGRAV==3) THEN
	  WRITE(29,*)'  2 restrictions sont imposees KGMIN < KG < KGMAX'					!bug
	  WRITE(66,*)'  2 restrictions sont imposees KGMIN < KG < KGMAX'
	  WRITE(66,*)'  KG maximum =', KGMAX,' m'
	  WRITE(66,*)'  KG minimum =', KGMIN,' m'
	  WRITE(666,*)'  2 restrictions sont imposees KGMIN < KG < KGMAX'
	  WRITE(666,*)'  KG maximum =', KGMAX,' m'
	  WRITE(666,*)'  KG minimum =', KGMIN,' m'
	ENDIF
	 
      REWIND(302)		!extension neto

      CALL ANNULD(DKG,9*NETO)
      IM=0

      DO 11 NEL=1,NETO

       READ(302) KSA,KSR,KSE,EPSA,EPSR,DELTA,HYA,DYA,WYA,TYA,					!extension neto
     *		HXR,DXR,WXR,TXR,PHIL,Q,EPAIS									!février 2004
c      WRITE(666,*)'EPSA,EPSR,DELTA,HYA,DYA,WYA,TYA,HXR,DXR,WXR,TXR'
c      WRITE(666,*) EPSA,EPSR,DELTA,HYA,DYA,WYA,TYA,HXR,DXR,WXR,TXR

       NBRXI=NVAR(NEL)  ! nbre de variable de conception dans le pannneau NEL

c      WRITE(666,*) 'NBRXI=',NBRXI
c      WRITE(666,'(A,9I3)') 'NXIT=',(NXIT(K,NEL),K=1,9)
c      WRITE(666,*) 'YNEU(NEL)=',YNEU(NEL)

       IF (NBRXI.EQ.0) THEN
	   GOTO 11
	 ELSE
c        DS = d[section(j)]/d xi(j) avec j= Numero du panneau
	   PHIL=PHILN(NEL)*PI/180
	   IF(ITYPE(NEL).NE.5) THEN													!février 2004
	   DS(1)=DABS(Q*PHIL)								! XI=1 epaisseur borde
	   DS(2)=DYA/EPSA*DABS(Q*PHIL)						! XI=2 hauteur ame cadre
	   DS(3)=HYA/EPSA*DABS(Q*PHIL)						! XI=3 epaiss ame cadre
	   DS(4)=TYA/EPSA*DABS(Q*PHIL)						! XI=4 largeur sem cadre
	   DS(5)=-(HYA*DYA+WYA*TYA)/(EPSA*EPSA)*DABS(Q*PHIL)! XI=5 entredistance cadre (EPSA)
	   DS(6)=DXR/EPSR*DABS(Q*PHIL)						! XI=2 haut ame raid
	   DS(7)=HXR/EPSR*DABS(Q*PHIL)						! XI=3 epaiss ame raid
	   DS(8)=TXR/EPSR*DABS(Q*PHIL)						! XI=4 larg sem raid
	   DS(9)=-(HXR*DXR+WXR*TXR)/(EPSR*EPSR)*DABS(Q*PHIL)! XI=5 entredistance raid (EPSR)
		ELSE																!février 2004
		IF(ISECT.EQ.3) THEN													!février 2004
	   DS(1)=2*DYA/EPSA*DABS(Q*PHIL)						 ! XI=1 hauteur ame double T		!février 2004
	   DS(2)=2*HYA/EPSA*DABS(Q*PHIL)						 ! XI=2 epaiss ame double T		!février 2004
	   DS(3)=2*TYA/EPSA*DABS(Q*PHIL)						 ! XI=3 largeur sem double T		!février 2004
	   DS(5)=-(2*HYA*DYA+2*WYA*TYA)/(EPSA*EPSA)*DABS(Q*PHIL) ! XI=5 entredistance épontille (EPSA)	!février 2004		
		ELSEIF(ISECT.EQ.1) THEN												!février 2004
		DS(1)=(PI*EPAIS)/EPSA*DABS(Q*PHIL)				       ! XI=1 diamètre extérieur		!février 2004
		DS(4)=(PI*(HYA-2*EPAIS))/EPSA*DABS(Q*PHIL)		       ! XI=4 épaisseur de paroi mince !février 2004
		DS(5)=-(PI*(HYA-EPAIS)*EPAIS)/(EPSA*EPSA)*DABS(Q*PHIL) ! XI=5 entredistance épontille !février 2004 
		ELSEIF(ISECT.EQ.2) THEN												!février 2004
       	DS(1)=(4*EPAIS)/EPSA*DABS(Q*PHIL)				! XI=1 côté extérieur			!février 2004
		DS(4)=(4*(HYA-2*EPAIS))/EPSA*DABS(Q*PHIL)		! XI=4 épaisseur de paroi mince !février 2004
		DS(5)=-(4*(HYA-EPAIS)*EPAIS)/(EPSA*EPSA)*DABS(Q*PHIL) ! XI=5 entredistance épontille !février 2004 
		ENDIF																!février 2004
	   ENDIF																!février 2004
        ENDIF

c      WRITE(666,*) 'DS' 		
c      WRITE(666,'(9(E10.3,1X))') (DS(I),I=1,9)  		

c	call annuld(orange,600) !fredtest
c	call annuld(test,600) !fredtest

        SPH2=VSIN(PHILN(NEL)/2.,0.D00)
        S1=VSIN(TETAS(NEL),0.D00)
        S2=VSIN(TETAS(NEL),PHILN(NEL)/2.)
	  D2=Q*(S1-2.*SPH2*S2/PHIL)
	 
	  DO 12 J=1,NBRXI
          JJ=NXIT(J,NEL)  ! No de ref de chaque var. de conception
          IMM=IM+J
c         WRITE(666,*) 'K,KK,IMM',K,KK,IMM
		IF(SYMX.EQ.1) THEN					!15.02.06
		  DKG(IMM)=0.
		ELSE	
	      DKG(IMM)=DS(JJ)*((Z(NEL,3)+D2)-KGY)/OMET2
	    ENDIF
c         WRITE(666,*) 'DKG(IMM)',DKG(IMM)
	
   12  CONTINUE
       
	 IM=IM+NBRXI

   11 CONTINUE

      REWIND(302)			!extension neto

c     SAUVETAGE Restrictions et sensibilites dans 'File 223' 
C     ------------------------------------------------------
C     BORNE MIN : KGMIN < KG avec KG > 0 et KGMAX > 0
C     Restriction : C(x)= KGMIN-KG < 0
C        dC(x)/dxi= - dKG/dxi
      IF((IGRAV==1).OR.(IGRAV==3)) THEN
        TEXT='KGMIN < KG'
	  C=KGMIN-KGY
        CM=0.0
	  DO I=1,NTOT
          DC(I)= - DKG(I)
        ENDDO
	  WRITE(303) C,CM,TEXT			!extension neto
	  WRITE(303) (DC(I),I=1,NTOT)	!extension neto
	  IMG=IMG+1  ! +1 au compteur des restrictions struc
        IF(IOPTI.GT.2) THEN
		IF(IMPR2.NE.-3) THEN			!15.10.05
	    WRITE(666,*) 
	    WRITE(666,*) 'C,CM,TEXT=',C,CM,'  ',TEXT
          WRITE(666,*) 'DC dans COORD après write 223:'	
          WRITE(666,'(9(E10.3,1X))') (DC(I),I=1,NTOT)
		ENDIF			!15.10.05
	  ENDIF  		
	ENDIF

c     BORNE MAX : KG < KGMAX avec KG > 0 et KGMAX > 0
C     Restriction : C(x)= KG/KGMAX < 1
C        dC(x)/dxi= +1/KGMAX * dKG/dxi
      IF((IGRAV==2).OR.(IGRAV==3)) THEN
        TEXT='KG < KGMAX'
	  C=KGY-KGMAX
        CM=0.0
	  DO I=1,NTOT
          DC(I)= DKG(I)
        ENDDO
	  WRITE(303) C,CM,TEXT			!extension neto
	  WRITE(303) (DC(I),I=1,NTOT)	!extension neto
	  IMG=IMG+1  ! +1 au compteur des restrictions struc
        IF(IOPTI.GT.2) THEN
		IF(IMPR2.NE.-3) THEN			!15.10.05
          WRITE(666,*) 
	    WRITE(666,*) 'C,CM,TEXT=',C,CM,'  ',TEXT
          WRITE(666,*) 'DC après write 223:'	
          WRITE(666,'(9(E10.3,1X))') (DC(I),I=1,NTOT)  		
		ENDIF			!15.10.05
	  ENDIF  		
	ENDIF

C
C 5  CALCUL DES INERTIES FLEXIONNELLES (IXX et IYY) 
C -------------------------------------------------
c     autour des axes neutres passant par le point (XNEUT,YNEUT)
   
   14 WRITE(66,'(//A/1x,34(1H*))')
     *    ' INERTIES FLEXIONNELLES ET DE MASSE'		!restri inertie
	IF(IOPTI.NE.0)
     *WRITE(666,'(//A/1x,34(1H*))')
     *	' INERTIES FLEXIONNELLES ET DE MASSE'		!restri inertie

	IYYTOT=0.
      IXXTOT=0.
	IXXTOTPART=0.			!r&d13
      IUUTOT=0.
      DO 301 J=1,NETO
       XNEU(J)=Z(J,1)+DD1(J)	! Coord du centre de gravite d'un panneau dans repere utilisateur
       YNEU(J)=Z(J,3)+DD2(J)
       IYYTOT=IYYTOT+IYY(J)+OMEGA(J)*((XNEU(J)-XNEUT)**2)
       IUUTOT=IUUTOT+IYY(J)+OMEGA(J)*((XNEU(J)      )**2)
       IXXTOT=IXXTOT+IXX(J)+OMEGA(J)*((YNEU(J)-YNEUT)**2)
	 IXXTOTPART=IXXTOTPART+IXXPART(J)+PART(J)*OMEGA(J)*		!r&d13
     *			((YNEU(J)-YNEUTPART)**2)
  301 CONTINUE

	
	IF(SYM.EQ.0) GOTO 21				!flexion d'axe vert.		
	IF(SYM.EQ.1) THEN					
	  IYYTOT=2*IYYTOT				
	  IXXTOT=2*IXXTOT
	  IXXTOTPART=2*IXXTOTPART		!r&d13				
	  GOTO 21						
	ENDIF		
	IF(SYM.EQ.2) THEN					
	  IYYTOT=4*IYYTOT				
	  IXXTOT=4*IXXTOT
	  IXXTOTPART=4*IXXTOTPART		!r&d13		
	  GOTO 21						
	ENDIF							
		
   21 CONTINUE							!flexion d'axe vert.


c     Impressions
c	IF(IMPR2.GE.0) THEN				!15.10.05
      if(LANGUE==1) THEN
	  WRITE(66,124) XNEUT,YNEUT,IYYTOT,IXXTOT
	  IF(SYM.NE.0) WRITE(66,125)
	  WRITE(66,126) IUUTOT
	  IF(SYM.NE.0) WRITE(66,127)
        WRITE(66,122)
	ELSE
        WRITE(66,224)XNEUT,YNEUT,IYYTOT,IXXTOT
        IF(SYM.NE.0) WRITE(66,225)
        WRITE(66,226) IUUTOT
	  IF(SYM.NE.0) WRITE(66,227)
        WRITE(66,222)
	ENDIF


      DO 315 J=1,NETO
        WRITE(66,123)J,OMEGA(J),XNEU(J),YNEU(J),DELT(J),IYY(J),IXX(J)
 315  CONTINUE
c 	ENDIF				!15.10.05


C 6	RESTRICTION SUR L'INERTIE IXX		!restri inertie		!r&d13
C ---------------------------------
	IF(IOPTI.EQ.0) GOTO 15
	IF(INERT.NE.0) THEN
        WRITE(66,'(/A/1x,27(1H.))')
     *    ' Restriction sur inertie Iyy'
	  WRITE(666,'(/A/1x,27(1H.))')
     *    ' Restriction sur inertie Iyy'
	  WRITE(66,'(A,T16,E14.7,T32,A)') ' Iyy actuel  = ',
     *    IXXTOTPART,'m**4'
	  WRITE(666,'(A,T16,E14.7,T32,A)') ' Iyy actuel  = ',
     *	IXXTOTPART,'m**4'
	  WRITE(66,'(A,T16,E14.7,T32,A)') ' Iyy minimum = ',IMIN,'m**4'
	  WRITE(666,'(A,T16,E14.7,T32,A)') ' Iyy minimum = ',IMIN,'m**4'
	  IM=0
	  CALL ANNULD(DC,9*NETO)
	  DO 13 NEL=1,NETO
	    READ(302) KSA,KSR,KSE,EPSA,EPSR,DELTA,HYA,DYA,WYA,TYA,		
     *			  HXR,DXR,WXR,TXR,PHIL,Q,EPAIS
          NBRXI=NVAR(NEL)  ! nbre de variable de conception dans le pannneau NEL																	
	    IF (NBRXI.EQ.0) GOTO 13
		IF(ITYPE(NEL).NE.5) THEN
	      PHIL=PHILN(NEL)*PI/180.
		  C1=VCOS(TETAS(NEL),0.D00)
		  S1=VSIN(TETAS(NEL),0.D00)
	      DIXX(1)=Q*DABS(PHIL)/12.*
     *	          (PART(NEL)*((C1*Q*PHIL)**2+
     *			  (S1*PART(NEL)*DELT(NEL))**2)+
     *		      PART(NEL)*DELT(NEL)*
     *			  (2.*(S1*PART(NEL)*DELT(NEL))*S1*PART(NEL)))			! XI=1 épaisseur borde
		  DIXX(6)=Q*DABS(PHIL)/12.*				
     *	          (PART(NEL)*DXR/EPSR*((C1*Q*PHIL)**2+
     *			  (S1*PART(NEL)*DELT(NEL))**2)+		
     *			  PART(NEL)*DELT(NEL)*
     *			  (2.*(S1*PART(NEL)*DELT(NEL))*S1*PART(NEL)*DXR/EPSR))	! XI=6 hauteur ame raidisseur
            DIXX(7)=Q*DABS(PHIL)/12.*				
     *	          (PART(NEL)*HXR/EPSR*((C1*Q*PHIL)**2+		
     *			  (S1*PART(NEL)*DELT(NEL))**2)+
     *			  PART(NEL)*DELT(NEL)*
     *			  (2.*(S1*PART(NEL)*DELT(NEL))*S1*PART(NEL)*HXR/EPSR))	! XI=7 epaiss ame raidisseur
            DIXX(8)=Q*DABS(PHIL)/12.*				
     *	          (PART(NEL)*TXR/EPSR*((C1*Q*PHIL)**2		
     *			  +(S1*PART(NEL)*DELT(NEL))**2)+
     *			  PART(NEL)*DELT(NEL)*
     *			  (2.*(S1*PART(NEL)*DELT(NEL))*S1*PART(NEL)*TXR/EPSR))	! XI=8 larg sem raidisseur
		  DIXX(9)=Q*DABS(PHIL)/12.*				
     *	          ((-1.)*PART(NEL)*(HXR*DXR+WXR*TXR)/(EPSR**2)*
     *			  ((C1*Q*PHIL)**2+(S1*PART(NEL)*DELT(NEL))**2)+		
     *			  PART(NEL)*DELT(NEL)*(2.*(S1*PART(NEL)*DELT(NEL))*S1*
     *			  (-1.)*PART(NEL)*(HXR*DXR+WXR*TXR)/(EPSR**2)))			! XI=9 larg sem raidisseur
		  DIXX(2)=DIXX(6)/1.e3								! XI=2 hauteur ame cadre
		  DIXX(3)=DIXX(7)/1.e3								! XI=3 epaiss ame cadre
		  DIXX(4)=DIXX(8)/1.e3								! XI=4 largeur sem cadre
		  DIXX(5)=DIXX(9)/1.e3								! XI=5 entredistance cadre (EPSA)
		  DS(1)=DABS(Q*PHIL)								! XI=1 epaisseur borde
		  DS(6)=DXR/EPSR*DABS(Q*PHIL)						! XI=6 haut ame raid
		  DS(7)=HXR/EPSR*DABS(Q*PHIL)						! XI=7 epaiss ame raid
		  DS(8)=TXR/EPSR*DABS(Q*PHIL)						! XI=8 larg sem raid
		  DS(9)=-(HXR*DXR+WXR*TXR)/(EPSR*EPSR)*DABS(Q*PHIL)	! XI=9 entredistance raid (EPSR)
		  DS(2)=DS(6)/1.e3									! XI=2 hauteur ame cadre
		  DS(3)=DS(7)/1.e3									! XI=3 epaiss ame cadre
		  DS(4)=DS(8)/1.e3									! XI=4 largeur sem cadre
		  DS(5)=DS(9)/1.e3									! XI=5 entredistance cadre (EPSA)
		  DO J=1,NBRXI
	        JJ=NXIT(J,NEL)		! No de ref de chaque var. de conception
	        IMM=IM+J
			IF(SYMX.EQ.0) THEN
			  DC(IMM)=(-1.)*(DIXX(JJ)+PART(NEL)*DS(JJ)*
     *				  (YNEU(NEL)-YNEUTPART)**2+
     *			      PART(NEL)*OMEGA(NEL)*(2.*(YNEU(NEL)-YNEUTPART)*
     *			      (-1.)*PART(NEL)*DS(JJ)*(YNEU(NEL)-YNEUTPART)/
     *				  OMETPART))
		    ELSE
	          DC(IMM)=(-1.)*(DIXX(JJ)+PART(NEL)*DS(JJ)*
     *				  ((YNEU(NEL)-YNEUTPART)**2))
	        ENDIF
			IF(SYM.EQ.1) DC(IMM)=2.*DC(IMM)
	        IF(SYM.EQ.2) DC(IMM)=4.*DC(IMM)
	      ENDDO
	    ELSE
	      DO J=1,NBRXI
	        JJ=NXIT(J,NEL)		! No de ref de chaque var. de conception
	        IMM=IM+J
			DC(IMM)=1.e-10
	      ENDDO
	    ENDIF
	    IM=IM+NBRXI
   13	  CONTINUE
        TEXT='Iyy > Imin'
	  C=-IXXTOTPART
	  CM=-IMIN
	  WRITE(303) C,CM,TEXT			
	  WRITE(303) (DC(I),I=1,NTOT)	
	  IMG=IMG+1					! +1 au compteur des restrictions struc
	  IF(IOPTI.GT.2) THEN
		IF(IMPR2.NE.-3) THEN			
	    WRITE(666,*) 
	    WRITE(666,*) 'C,CM,TEXT=',C,CM,'  ',TEXT
          WRITE(666,*) 'DC après write 303:'	
          WRITE(666,'(9(E10.3,1X))') (DC(I),I=1,NTOT)
		ENDIF			
	  ENDIF
	  REWIND 302	  
	ENDIF						! restri inertie

C 6	RESTRICTION SUR LE MODULE Ixx/v		!restri module		!r&d13
C -----------------------------------
	IF(IMOD.NE.0) THEN
        V=MAX(DABS(Z(IELT,3)-YNEUTPART),DABS(Z(IELT,4)-YNEUTPART))
	  MOD=IXXTOTPART/V
	  WRITE(66,'(/A/1x,28(1H.))')
     *    ' Restriction sur module Iyy/v'
	  WRITE(666,'(/A/1x,28(1H.))')
     *    ' Restriction sur module Iyy/v'
	  WRITE(66,'(A,T18,E14.7,T34,A)') ' Iyy/v actuel  = ',MOD,'m**3'
	  WRITE(666,'(A,T18,E14.7,T34,A)') ' Iyy/v actuel  = ',MOD,'m**3'
	  WRITE(66,'(A,T18,E14.7,T34,A)')
     *     ' Iyy/v minimum = ',MODMIN,'m**3'
	  WRITE(666,'(A,T18,E14.7,T34,A)')
     *     ' Iyy/v minimum = ',MODMIN,'m**3'
	  IM=0
	  CALL ANNULD(DC,9*NETO)
	  DO 16 NEL=1,NETO
	    READ(302) KSA,KSR,KSE,EPSA,EPSR,DELTA,HYA,DYA,WYA,TYA,		
     *			  HXR,DXR,WXR,TXR,PHIL,Q,EPAIS
          NBRXI=NVAR(NEL)  ! nbre de variable de conception dans le pannneau NEL																	
	    IF (NBRXI.EQ.0) GOTO 16
		IF(ITYPE(NEL).NE.5) THEN
	      PHIL=PHILN(NEL)*PI/180.
		  C1=VCOS(TETAS(NEL),0.D00)
		  S1=VSIN(TETAS(NEL),0.D00)
	      DIXX(1)=Q*DABS(PHIL)/12.*
     *	          (PART(NEL)*((C1*Q*PHIL)**2+
     *			  (S1*PART(NEL)*DELT(NEL))**2)+
     *		      PART(NEL)*DELT(NEL)*
     *			  (2.*(S1*PART(NEL)*DELT(NEL))*S1*PART(NEL)))			! XI=1 épaisseur borde
		  DIXX(6)=Q*DABS(PHIL)/12.*				
     *	          (PART(NEL)*DXR/EPSR*((C1*Q*PHIL)**2+
     *			  (S1*PART(NEL)*DELT(NEL))**2)+		
     *			  PART(NEL)*DELT(NEL)*
     *			  (2.*(S1*PART(NEL)*DELT(NEL))*S1*PART(NEL)*DXR/EPSR))	! XI=6 hauteur ame raidisseur
            DIXX(7)=Q*DABS(PHIL)/12.*				
     *	          (PART(NEL)*HXR/EPSR*((C1*Q*PHIL)**2+		
     *			  (S1*PART(NEL)*DELT(NEL))**2)+
     *			  PART(NEL)*DELT(NEL)*
     *			  (2.*(S1*PART(NEL)*DELT(NEL))*S1*PART(NEL)*HXR/EPSR))	! XI=7 epaiss ame raidisseur
            DIXX(8)=Q*DABS(PHIL)/12.*				
     *	          (PART(NEL)*TXR/EPSR*((C1*Q*PHIL)**2		
     *			  +(S1*PART(NEL)*DELT(NEL))**2)+
     *			  PART(NEL)*DELT(NEL)*
     *			  (2.*(S1*PART(NEL)*DELT(NEL))*S1*PART(NEL)*TXR/EPSR))	! XI=8 larg sem raidisseur
		  DIXX(9)=Q*DABS(PHIL)/12.*				
     *	          ((-1.)*PART(NEL)*(HXR*DXR+WXR*TXR)/(EPSR**2)*
     *			  ((C1*Q*PHIL)**2+(S1*PART(NEL)*DELT(NEL))**2)+		
     *			  PART(NEL)*DELT(NEL)*(2.*(S1*PART(NEL)*DELT(NEL))*S1*
     *			  (-1.)*PART(NEL)*(HXR*DXR+WXR*TXR)/(EPSR**2)))			! XI=9 larg sem raidisseur
		  DIXX(2)=DIXX(6)/1.e3										! XI=2 hauteur ame cadre
		  DIXX(3)=DIXX(7)/1.e3										! XI=3 epaiss ame cadre
		  DIXX(4)=DIXX(8)/1.e3										! XI=4 largeur sem cadre
		  DIXX(5)=DIXX(9)/1.e3										! XI=5 entredistance cadre (EPSA)
		  DS(1)=DABS(Q*PHIL)										! XI=1 epaisseur borde
		  DS(6)=DXR/EPSR*DABS(Q*PHIL)								! XI=6 haut ame raid
		  DS(7)=HXR/EPSR*DABS(Q*PHIL)								! XI=7 epaiss ame raid
		  DS(8)=TXR/EPSR*DABS(Q*PHIL)								! XI=8 larg sem raid
		  DS(9)=-(HXR*DXR+WXR*TXR)/(EPSR*EPSR)*DABS(Q*PHIL)			! XI=9 entredistance raid (EPSR)
		  DS(2)=DS(6)/1.e3											! XI=2 hauteur ame cadre
		  DS(3)=DS(7)/1.e3											! XI=3 epaiss ame cadre
		  DS(4)=DS(8)/1.e3											! XI=4 largeur sem cadre
		  DS(5)=DS(9)/1.e3											! XI=5 entredistance cadre (EPSA)
		  DO J=1,NBRXI
	        JJ=NXIT(J,NEL)		! No de ref de chaque var. de conception
	        IMM=IM+J
			IF(SYMX.EQ.0) THEN
			  DC(IMM)=DIXX(JJ)+PART(NEL)*DS(JJ)*
     *				  (YNEU(NEL)-YNEUTPART)**2+
     *			      PART(NEL)*OMEGA(NEL)*(2.*(YNEU(NEL)-YNEUTPART)*
     *			      (-1.)*PART(NEL)*DS(JJ)*(YNEU(NEL)-YNEUTPART)/
     *				  OMETPART)
			  IF(SYM.EQ.1) DC(IMM)=2.*DC(IMM)
	          IF(SYM.EQ.2) DC(IMM)=4.*DC(IMM)
			  DC(IMM)=(-1.)*(DC(IMM)*V-IXXTOTPART*
     *                  (-1.)*PART(NEL)*DS(JJ)*(YNEU(NEL)-YNEUTPART)
     *				  /OMETPART)/V**2
		    ELSE
	          DC(IMM)=DIXX(JJ)+PART(NEL)*DS(JJ)*
     *				  ((YNEU(NEL)-YNEUTPART)**2)
			  IF(SYM.EQ.1) DC(IMM)=2.*DC(IMM)
	          IF(SYM.EQ.2) DC(IMM)=4.*DC(IMM)
	          DC(IMM)=(-1.)*DC(IMM)/V
			ENDIF
	      ENDDO
	    ELSE
	      DO J=1,NBRXI
	        JJ=NXIT(J,NEL)		! No de ref de chaque var. de conception
	        IMM=IM+J
			DC(IMM)=1.e-10
	      ENDDO
	    ENDIF
	    IM=IM+NBRXI
   16	  CONTINUE
        TEXT='I/v > I/vmin'
	  C=-MOD
	  CM=-MODMIN
	  WRITE(303) C,CM,TEXT			
	  WRITE(303) (DC(I),I=1,NTOT)	
	  IMG=IMG+1					! +1 au compteur des restrictions struc
	  IF(IOPTI.GT.2) THEN
		IF(IMPR2.NE.-3) THEN			
	    WRITE(666,*) 
	    WRITE(666,*) 'C,CM,TEXT=',C,CM,'  ',TEXT
          WRITE(666,*) 'DC après write 303:'	
          WRITE(666,'(9(E10.3,1X))') (DC(I),I=1,NTOT)
		ENDIF			
	  ENDIF
	  REWIND 302	  
	ENDIF						! restri module

C
C 7  CALCUL DES INERTIES DE MASSES : Ig(X) et Ig(Y) 
C -------------------------------------------------
c     autour des axes passant par le centre de gravite en (KGX,KGY)
   15 IYYTOT2=0.
      IXXTOT2=0.
      IUUTOT2=0.
      DO  J=1,NETO
       IYYTOT2=IYYTOT2+IYY2(J)+OMEGA2(J)*((XNEU(J)-KGX)**2)
       IUUTOT2=IUUTOT2+IYY2(J)+OMEGA2(J)*((XNEU(J)    )**2)
       IXXTOT2=IXXTOT2+IXX2(J)+OMEGA2(J)*((YNEU(J)-KGY)**2)
      ENDDO

	
	IF((NCONDI.EQ.0).OR.(SYM.EQ.0)) THEN			!flexion d'axe vert.	
	  GOTO 22					
	ENDIF						
	IF(SYM.EQ.1) THEN				
	  IYYTOT2=2*IYYTOT2			
	  IXXTOT2=2*IXXTOT2			
	  GOTO 22					
	ENDIF						
	IF(SYM.EQ.2) THEN				
	  IYYTOT2=4*IYYTOT2			
	  IXXTOT2=4*IXXTOT2			
	  GOTO 22					
	ENDIF						
		
   22 CONTINUE									!flexion d'axe vert.


c     Impressions
c	IF(IMPR2.GE.0) THEN				!15.10.05
      if(LANGUE==1) THEN
	  WRITE(66,134) KGX,KGY,IYYTOT2,IXXTOT2
	  IF(SYM.NE.0) WRITE(66,135)
	  WRITE(66,136) IUUTOT2
	  IF(SYM.NE.0) WRITE(66,137)
        WRITE(66,122)
	ELSE
	  WRITE(66,234)KGX,KGY,IYYTOT2,IXXTOT2
        IF(SYM.NE.0) WRITE(66,235)
        WRITE(66,236) IUUTOT2
	  IF(SYM.NE.0) WRITE(66,237)
        WRITE(66,222)
	ENDIF
      DO  J=1,NETO
       WRITE(66,123)J,OMEGA2(J),XNEU(J),YNEU(J),DELT2(J),IYY2(J),IXX2(J)
      ENDDO
c	ENDIF							!15.10.05


C     FORMAT
c     ---------
  104 FORMAT(2X,I2,T8,4(E11.4,1X)/)
  115 FORMAT(20I4)

  120 FORMAT(T2,'PANNEAU',T11,'Y DEPART',T22,'Z DEPART',T33,'Y ARRIVEE',
     *      T44,'Z ARRIVEE'/)
  121 FORMAT(/' Les axes globaux initiaux de la structure (horiz. ',
     * 'OY et vert. OZ) ont leur origine'/' au point de depart du ',
     * 'panneau 1'/' Dans ce repere, les coordonnees du repere ',
     * 'utilisateur sont:'/T4,'(Yk,Zk)= ',E11.4,1x,E11.4' m'/
     * ' C''est ce repere ',
     * 'qui est utilise dans tous les resultats qui suivent!!'//
     * ' NBRE DE NOEUDS',I3//' COORDONNEES DES NOEUDS dans le',
     * ' repere utilisateur:'/T2,50(1H-))										!flexion d'axe vert.
  122 FORMAT(/T2,'Pour chaque panneau'/T2,19(1H.)/T2,
     *'Panneau',T11,'Surface(m**2)',T25,'Coord.(Y,Z) Centre',
     *' de Gravite',T55,'Epaiss Equiv.',
     *  T70,'Inerties Izz et Iyy (m**4)'/)
  123 FORMAT(T2,I3,T11,E11.4,T26,E11.4,T39,E11.4,T55,E11.4,T70,
     *          E11.4,T83,E11.4)
  124 FORMAT(/' 1- Inertie flexionnelle (bordé, raid et trav.)'/
     * T2,46(1H-)/' A UTILISER DANS LE DIMENSIONNEMENT STRUCTUREL'//
     *' Pour toute la structure'/ 
     * T2,23(1H.)/
     *' Yg = ',T8,E11.4,T24,'m',T30,'(position axe neutre vertical
     * suivant axe OY du repère utilisateur)'/
     *' Zg = ',T8,E11.4,T24,'m',T30,'(position axe neutre horizontal
     * suivant axe OZ du repère utilisateur)'//
     *' Izz = ',T8,E14.7,T24,'m**4',T30,'(inertie autour axe neutre
     * vertical)'/
     *' Iyy = ',T8,E14.7,T24,'m**4',T30,'(inertie autour axe neutre
     * horizontal)')
  125 FORMAT(' !!! Les axes de symétrie sont pris en compte dans
     * le calcul des inerties Izz et Iyy. Les valeurs Izz et'/T6,
     * 'Iyy données ci-dessus correspondent ainsi à la structure
     * complète et pas à la structure modélisée !!!')
  126 FORMAT(/' Iuu =',T8,E14.7,T24,'m**4',T30,'(inertie autour axe
     * vertical du repère utilisateur)')
  127 FORMAT(' !!! La valeur Iuu donnée ci-dessus correspond
     * à la structure modélisée et pas à la structure complète !!!')			!flexion d'axe vert.
  130 FORMAT(T2,'PANEL',T11,'Y DEPART',T22,'Z DEPART',T33,'Y ARRIVAL',
     *      T44,'Z ARRIVAL'/)
  134 FORMAT(/' 2- Inertie de masse (bordé, raid, cadre et trav.)'/
     * T2,49(1H-)/' A UTILISER DANS LA VERIFICATION DE LA STABILITE A LA
     * FLOTTAISON'//' Pour toute la structure'/ 
     * T2,23(1H.)/
     *' Yg = ',T9,E11.4,T24,'m',T30,'(position du centre de gravité
     * suivant axe OY du repère utilisateur)'/
     *' Zg = ',T9,E11.4,T24,'m',T30,'(position du centre de gravité
     * suivant axe OZ du repère utilisateur)'//
     * /' Izz = ',T8,E14.7,T24,'m**4',T30,'(inertie autour axe vertical
     * passant par le centre de gravité (Yg,Zg))'/
     *' Iyy = ',T8,E14.7,T24,'m**4',T30,'(inertie autour axe horizontal
     * passant par le centre de gravité (Yg,Zg))')
  135 FORMAT(' !!! Les axes de symétrie sont pris en compte dans
     * le calcul des inerties Izz et Iyy. Les valeurs Izz et'/T6,
     * 'Iyy données ci-dessus correspondent ainsi à la structure
     * complète et pas à la structure modélisée !!!')
  136 FORMAT(/' Iuu =',T8,E14.7,T24,'m**4',T30,'(inertie autour axe
     * vertical du repère utilisateur)')
  137 FORMAT(' !!! La valeur Iuu donnée ci-dessus correspond
     * à la structure modélisée et pas à la structure complète !!!')
  221 FORMAT(/' The Basic Frames (OY horiz. and OZ vertic.)',
     * ' have their origine at the arriving node of PANEL 1'/
     * ' In this frame, the origin of the User Coordinate-Frame is:',
     *  /T4,'(Yk,Zk)= ',E11.4,1x,E11.4' m'/
     * ' Any future coordinates will refer to this User-Frames!!'//
     * ' NBRE OF NODES',I3//T2,'NODES COORDINATES :'/T2,45(1H-))
  222 FORMAT(/T2,'For each panel'/T2,14(1H.)/T2,
     *'Panel',T11,'Area (m**2)',T25,'Coord.(Y,Z) Centre',
     *' of Gravity',T55,'Equiv.Thickness',
     *  T70,'Inertia Izz and Iyy (m**4)'/)
  224 FORMAT(/' 1- Flexional inertia (plate, stiff and long. girders)'/
     * T2,53(1H-)/' TO USE IN THE STRUCTURAL DESIGN'//
     *' For the structure'/ 
     * T2,17(1H.)/
     *' Yg = ',T8,E11.4,T24,'m',T30,'(location of the vertical neutral
     * axis along the OY axis of the User-Frames)'/
     *' Zg = ',T8,E11.4,T24,'m',T30,'(location of the horizontal neutral
     * axis along the OZ axis of the User-Frames)'//
     *' Izz = ',T8,E14.7,T24,'m**4',T30,'(inertia around the vertical
     * neutral axis)'/
     *' Iyy = ',T8,E14.7,T24,'m**4',T30,'(inertia around the horizontal
     * neutral axis)')
  225 FORMAT(' !!! The symmetry axes are taken into account to determine
     * the inertia Izz and Iyy. The values Izz'/T6,'and Iyy
     * given above are then relative to the full structure and not
     * to the modeled structure !!!')
  226 FORMAT(/' Iuu =',T8,E14.7,T25,'(inertia around the vertical axis
     * of the User-Frames)')
  227 FORMAT(' !!! The value Iuu given above is relative to the modeled
     * structure and not to the full structure !!!')								
  234 FORMAT(/' 2- Mass inertia (plate, stiff, frame and long. girders)'
     * /T2,55(1H-)/' TO USE IN THE FLOTATION STABILITY ANALYSIS'//
     * ' For the structure'/ 
     * T2,17(1H.)/
     *' Yg = ',T9,E11.4,T24,'m',T30,'(location of the centre of gravity
     * along the OY axis of the User-Frames)'/
     *' Zg = ',T9,E11.4,T24,'m',T30,'(location of the centre of gravity
     * along the OZ axis of the User-Frames)'//
     * ' Izz = ',T8,E14.7,T24,'m**4',T30,'(inertia around the vertical
     * axis going through the centre of gravity (Yg,Zg))'/
     *' Iyy = ',T8,E14.7,T24,'m**4',T30,'(inertia around the horizontal
     * axis going through the centre of gravity (Yg,Zg))')
  235 FORMAT(' !!! The symmetry axes are taken into account to determine
     * the inertia Izz and Iyy. The values Izz'/T6,'and Iyy
     * given above are then relative to the full structure and not
     * to the modeled structure !!!')
  236 FORMAT(/' Iuu =',T8,E14.7,T25,'(inertia around the vertical axis
     * of the User-Frames)')
  237 FORMAT(' !!! The value Iuu given above is relative to the modeled
     * structure and not to the full structure !!!')

      RETURN
      END
