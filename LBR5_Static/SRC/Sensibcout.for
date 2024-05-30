      SUBROUTINE SENSIBCOUT(DERIVEE,NETO,OBJ,NVAR,NN,NXIT,SPEC,
     *                      WIDTH,ISECT,CORRO)								!18.03.04 + !corrosion

      IMPLICIT REAL*8(A-H,O-Z)
	DIMENSION DERIVEE(NN),NXIT(9,NETO),SPEC(1),NVAR(1)
	DIMENSION TRAV(218357)													!18.03.04  !extension neto
	DIMENSION ISECT(neto)													!18.03.04
      DIMENSION CORRO(NETO,3)			!corrosion 
      COMMON/COUT/ Poids,SPoids,
     *        Fmat,Fsou,FMdO,Fmat11,Fmat22,Fmat33,Fsou11,Fsou22,
     *        Fsou33,FMdO11,FMdO22,FMdO33,FMdO44,FMdO55,FMdO66,FMdO77,
     *        REND,EQP,Dref,DrefX,DrefY,C1,C2,C3,DC1,DW2,DW3,P10,DP10,
     *        P4,P5,DP4,DP5,P9X,P9Y,DP9X,DP9Y,BER,BET,P6,P7,C8,DC8,COUT,
     *        ICOUT,IALR,IALT

      NBR=5 ! provisoire (il est défini plus loin)

      I1 = 1             !X(2*NBR+1)
	I2 = I1 + 2*NBR +1 !Y(2*NBR+1)
	I3 = I2 + 2*NBR +1 ! indice non utilisé !!!!!!!!!
	I4 = I3          !VLARG(NETO)   
	I5 = I4 + NETO   !VEPAISS(NETO)
	I6 = I5 + NETO   !VHAC(NETO)
	I7 = I6 + NETO   !VEAC(NETO)
	I8 = I7 + NETO   !VLSC(NETO)
	I9 = I8 + NETO   !VESC(NETO)
	I10 = I9 + NETO   !DELTAC(NETO)
	I11 = I10 + NETO   !VHAR(NETO)
	I12 = I11 + NETO   !VEAR(NETO)
	I13 = I12 + NETO   !VLSR(NETO)
	I14 = I13 + NETO   !VESR(NETO)
	I15 = I14 + NETO   !DELTAR(NETO)
	I16 = I15 + NETO   !ENTR(NETO)
	I17 = I16 + NETO   !PHIL(NETO)
	I18 = I17 + NETO   !Q(NETO)
	I19 = I18 + NETO   !DFCT(NN)
	I20 = I19 + NN     !CoutPAN(NETO)
	I21 = I20 + NETO   !CostPAN(NETO)
	I22 = I21 + NETO   !CoutMAT(NETO)
	I23 = I22 + NETO   !VDIFFC(NETO)				!18.03.04
	I24	= I23 + NETO	!vecteur de travail pour COSTMAIN (214685)	!23.03.04 !extension neto

C Total = 2*(2*NBR+1) + 19*NETO + NN     + 214685						!19.05.04 !extension neto
C       = 22          + 2850    + 800    + 214685   (pour NBR=5)		!19.05.04 !extension neto
C       = 218357                 										!19.05.04

	CALL SENSIB1(DERIVEE,NETO,OBJ,NVAR,NN,NXIT,SPEC,WIDTH,ISECT,		!18.03.04
     *TRAV(I1), TRAV(I2), TRAV(I4), TRAV(I5), TRAV(I6), TRAV(I7),
     *TRAV(I8), TRAV(I9), TRAV(I10),TRAV(I11),TRAV(I12),TRAV(I13),
     *TRAV(I14),TRAV(I15),TRAV(I16),TRAV(I17),TRAV(I18),TRAV(I19),
     *TRAV(I20),TRAV(I21),TRAV(I22),TRAV(I23),TRAV(I24),					!18.03.04
     *CORRO)																!corrosion
      RETURN
	END

C ============================================================================
	SUBROUTINE SENSIB1(DERIVEE,NETO,OBJ,NVAR,NN,NXIT,SPEC,WIDTH,ISECT,		!18.03.04
     *  X,   Y,   VLARG, VEPAISS,VHAC,VEAC,
     *  VLSC,VESC,DELTAC,VHAR,   VEAR,VLSR,
     *  VESR,DELTAR,ENTR,PHIL,   Q,   DFCT,
     *  CoutPAN,CostPAN,COUTMAT,VDIFFC,TRAV,									!18.03.04
     *  CORRO)																!corrosion

      
	IMPLICIT REAL*8(A-H,O-Z)
	REAL*8 INVA
      DIMENSION X(10),Y(10),S(3),W(2),B(2),C(2)
      DIMENSION A(2,2),INVA(2,2),DERIVEE(NN),NXIT(9,NETO),NVAR(NETO)
	DIMENSION ISECT(neto)												!18.03.04
      DIMENSION VLARG(NETO),VEPAISS(NETO),VHAC(NETO),
     *  VEAC(NETO),VLSC(NETO),VESC(NETO),DELTAC(NETO),VHAR(NETO),
     *  VEAR(NETO),VLSR(NETO),VESR(NETO),DELTAR(NETO),ENTR(NETO),
     *  PHIL(NETO),Q(NETO),CoutPAN(NETO),CostPAN(NETO),CoutMAT(NETO),
     *  DCOR(NETO),DCOR2(NETO),DCOR3(NETO),SPEC(NETO),DFCT(NN),
     *  VDIFFC(NETO)										!18.03.04
	DIMENSION TRAV(1)   !(214685)  !extension neto

      DIMENSION CORRO(NETO,3)			!corrosion 

      COMMON/PY/PI
	COMMON/COUT/ Poids,SPoids,
     *        Fmat,Fsou,FMdO,Fmat11,Fmat22,Fmat33,Fsou11,Fsou22,
     *        Fsou33,FMdO11,FMdO22,FMdO33,FMdO44,FMdO55,FMdO66,FMdO77,
     *        REND,EQP,Dref,DrefX,DrefY,C1,C2,C3,DC1,DW2,DW3,P10,DP10,
     *        P4,P5,DP4,DP5,P9X,P9Y,DP9X,DP9Y,BER,BET,P6,P7,C8,DC8,COUT,
     *        ICOUT,IALR,IALT
      COMMON/ALSTOM/IDIFF,NBR,PAS
C*********************************************************************************
C     SUBROUTINE SENSIBCOUT
C     ===================
C     Cette sous-routine est appelée dans OPTIS.
C     Elle calcule une sensibilité de la fonction objectif
C     coût total pour chaque variable de conception, pour chaque panneau.
C     Elle calcule aussi la valeur de la fonction objectif.
C     Pour ce faire, 2 versions différentes sont implémentées :
C
C     1) Calcul des sensibilités au niveau du coût global (ICOUT = 3)
C     ---------------------------------------------------
C        Ce calcul peut s'effectuer par différence finie (IDIFF=1) ou
C        par la méthode de GAUSS (IDIFF=0) : on détermine la valeur du Coût en 
C        différents points autour du point qui nous intéresse et interpole une 
C        courbe par ces points grâce à la méthode des MOINDRES CARRES DE GAUSS. 
C        De cette courbe,on en déduit facilement la valeur de la dérivée.
C
C     2) Calcul des sensibilités au niveau des coûts unitaires (ICOUT = 2)
C     --------------------------------------------------------
C        Le calcul des sensibilités des coûts unitaires est fait à la fois par 
C        différence finie et analytiquement (les coûts unitaires n'étant pas donnés 
C        par une fonction analytique, on doit encore procéder par diff. finie) dans 
C        Costmain.
C
C     Rappel : Si ICOUT = 1, le calcul du coût est effectué de manière simplifiée
C              sans passer dans cette sous-routine.
C
C INPUTS : NBR : nombre de points à droite (et à gauche) utilisés pour déterminer
C                la dérivée de la fonction objectif. 
C                On utilise 2*NBR+1 points (= N) pour la méthode de GAUSS
C          PAS   : Espacement entre les différents points.
C          NETO  : nombre de panneaux de la structure.
C          NVAR  : vecteur stockant les numéros des variables de conception utilisées.
C          NN    : dimension max du vecteur dérivée.      
C          SPEC  : poids spécifique (N/m3) (à spécifier pour chaque panneau)
C          WIDTH : longueur totale de la structure   
C
C          
C OUTPUTS : DERIVEE: valeur approchée de la dérivée du coût par rapport aux variables
C                    de conception utilisées, pour chaque panneau.
C           OBJ: coût total de fabrication de la structure.
C                       
C Créer: F. Bair (Juin 2003)
C
C Modifications:
c   - juin 2003 :
c
c Dernière mise à jour: 16 juin 2003 (Ph. rigo)
C                       11 septembre 2003 (F. Bair) : combinaison des 2 versions   
c					  18 mars 2004 (J.Pirard) : introduction des épontilles          
C
C*********************************************************************************
    
      CALL ANNULD(DERIVEE,NN)

	EQPC1=EQP*C1*1000.0  ! (=Euro/heure) ,pour transfert de RRO en Euro

C  1. Lecture des données courantes	
	REWIND 305	!extension neto	

	READ(305,*) VLENGTH		!extension neto

	DO I=1,NETO
       READ(305,*) NOPAN,VLARG1,VEPAISS1 ! nécessaire pour lire ENTR					!extension neto
	 READ(305,*) VHAC1,VEAC1,VLSC1,VESC1,DELTAC1,VDIFFC1				!18.03.04	!extension neto
	 READ(305,*) VHAR1,VEAR1,VLSR1,VESR1,DELTAR1,ENTR(i)							!extension neto
c      READ(206,*) NOPAN,VLARG(i),VEPAISS(i) 
c	 READ(206,*) VHAC(i),VEAC(i),VLSC(i),VESC(i),DELTAC(i)
c	 READ(206,*) VHAR(i),VEAR(i),VLSR(i),VESR(i),DELTAR(i),ENTR(i)
	
	 READ(302) KSA,KSR,KSE,DELTAC(i),DELTAR(i),VEPAISS(i),			!extension neto
     *           VHAC(i),VEAC(i),VLSC(i),VESC(i),VHAR(i),VEAR(i),
     *		   VLSR(i),VESR(i),PHIL(i),Q(i),VDIFFC(i)				!18.03.04

       VLARG(i)=PHIL(i)*Q(i)*PI/180.

	 DCOR(i)  = CORRO(i,1)				!corrosion
	 DCOR2(i) = CORRO(i,2)				!corrosion
	 DCOR3(i) = CORRO(i,3)				!corrosion

c       IF(I.EQ.1) THEN
c	  REWIND 57
c	  READ(57,'(////,A1)') Abidon
c	  READ(57,*) IPAN,DCOR(i),DCOR2(i),DCOR3(i)
c	 ELSE
c	  READ(57,*) IPAN,DCOR(i),DCOR2(i),DCOR3(i)
c	 ENDIF

C      DCOR  = épaisseur de corrosion pour bordé
C	 DCOR2 = épaisseur de corrosion pour cadres
C	 DCOR3 = épaisseur de corrosion pour lisses

	 VEPAISS(i) = VEPAISS(i) + DCOR(i)
	 VEAC(i)    = VEAC(i)    + DCOR2(i)
	 VESC(i)    = VESC(i)    + DCOR2(i)
	 VDIFFC(i)	= VDIFFC(i)	 + DCOR2(i)		!18.03.04
	 VEAR(i)    = VEAR(i)    + DCOR3(i)
	 VESR(i)    = VESR(i)    + DCOR3(i)
	
c	 IF(IPRINT.GE.1) THEN
c       WRITE(666,*) 'dans SensibCout'
c       WRITE(666,*) 'PANNEAU Nø ',I
c       WRITE(666,*) 'Bord =',VEPAISS(i)
c       WRITE(666,*) 'Cadre=',VHAC(i),VEAC(i),VLSC(i),VESC(i),' EPSA=',
c       *            DELTAC(i)
c       WRITE(666,*) 'Raid.=',VHAR(i),VEAR(i),VLSR(i),VESR(i),' EPSR=',
c       *            DELTAR(i),' ENTR=',ENTR(i)
c	 ENDIF
	ENDDO

      REWIND(305)		!extension neto

C  2. Calcul de Fmat= Coût des matériaux (tôles, raid, cadres)
c    --------------------------------------------------------
C        COUTMAT(I) = coût matériaux du panneau i
      Fmat = 0.
      DO I=1,NETO
	 TEMP = PHIL(i) * Q(i) * WIDTH * PI/180.
       DENS = SPEC(i)/9.81
	IF(ISECT(I).EQ.0) THEN														!18.03.04
	 COUTMAT(I) = TEMP * DENS * 
     *  (C1 *(1.+(VEPAISS(i)-Dref)*1000.*DC1) * VEPAISS(i) 
     * + C2 *(1.+(VEAR(i)   -Dref)*1000.*DC1) * (1.0+DW2)   ! Dref et pas DrefX
     *                   * (VEAR(i)*VHAR(i)+VESR(i)*VLSR(i))/DELTAR(i) 
     * + C3 *(1.+(VEAC(i)   -Dref)*1000.*DC1) * (1.0+DW3)   ! Dref et pas DrefY
     *                   * (VEAC(i)*VHAC(i)+VESC(i)*VLSC(i))/DELTAC(i) ) 
	 ELSEIF(ISECT(I).EQ.1) THEN													!18.03.04
	 COUTMAT(I) = TEMP * DENS *													!18.03.04
c     *  ( C2 *(1.+(VEAR(i)   -Dref)*1000.*DC1) * (1.0+DW2)   ! Dref et pas DrefX	!05.12.05
c     *                   * (VEAR(i)*VHAR(i)+VESR(i)*VLSR(i))/DELTAR(i)			!05.12.05
     * ( C3 *(1.+(VEAC(i)   -Dref)*1000.*DC1) * (1.0+DW3)   ! Dref et pas DrefY	!05.12.05
     *   *PI*(VHAC(i)*VHAC(i)-(VHAC(i)-2*VDIFFC(i))**2) /(4*DELTAC(i)))			!18.03.04
	 ELSEIF(ISECT(I).EQ.2) THEN													!18.03.04
	 COUTMAT(I) = TEMP * DENS *													!18.03.04
c     *  ( C2 *(1.+(VEAR(i)   -Dref)*1000.*DC1) * (1.0+DW2)   ! Dref et pas DrefX	!05.12.05
c     *                   * (VEAR(i)*VHAR(i)+VESR(i)*VLSR(i))/DELTAR(i)			!05.12.05
     * (C3 *(1.+(VEAC(i)   -Dref)*1000.*DC1) * (1.0+DW3)   ! Dref et pas DrefY	!05.12.05
     *   *(VHAC(i)*VHAC(i)-(VHAC(i)-2*VDIFFC(i))**2) /DELTAC(i))					!18.03.04
	 ELSEIF(ISECT(I).EQ.3) THEN													!18.03.04
	 COUTMAT(I) = TEMP * DENS *													!18.03.04
c     *  ( C2 *(1.+(VEAR(i)   -Dref)*1000.*DC1) * (1.0+DW2)   ! Dref et pas DrefX	!05.12.05
c     *                   * (VEAR(i)*VHAR(i)+VESR(i)*VLSR(i))/DELTAR(i)			!05.12.05
     * (C3 *(1.+(VEAC(i)   -Dref)*1000.*DC1) * (1.0+DW3)   ! Dref et pas DrefY	!05.12.05
     *   *(VEAC(i)*2* VHAC(i)+2*VESC(i)* VLSC(i)) /DELTAC(i))						!18.03.04
	ENDIF																		!18.03.04
      
	 Fmat = Fmat + COUTMAT(I)
	ENDDO


C  3. Calcul des coûts de Mdo + sensivilités (Boucles sur les variables de conception)
c  --------------------------------------------------------------------------------------
      NON = 0 !si NON=*** -> certaines variables de conception sortent du jeu de données
	        ! - 1er chiffre : si =1 -> variable trop petite; 
			!                 si =2 -> variable trop grande
	        ! - les 2 autres chiffres: donnent le numéro de l'opération en question

      NCONT = 0

      IF (ICOUT.EQ.2) THEN
	PAS = 1./20.
	IDIFF = 1
	NBR = 1
	CALL COSTMAIN(COUTRRO,VLARG,VEPAISS,VHAC,VEAC,VLSC,VESC,
     *    DELTAC,VHAR,VEAR,VLSR,VESR,DELTAR,ENTR,VLENGTH,NETO,NON,I1,
     *    CoutPAN,DERIVEE,NN,NVAR,NXIT,TRAV,totalTI)
c	    COUTPAN = RRO de chaque panneau
c	    COUTRRO = RRO Structure globale
      DERIVEE = DERIVEE*EQPC1  ! = dérivée du coût de fabrication en Euro
	GOTO 1
      ENDIF

C     Paramètres de controle pour le calcul par différence finie

      IDIFF = 1   ! si = 0, interpolation des moindres carrés sur 2NBR+1 points
	            ! si = 1, simple différence finie sur 2 points (1 à gauche et 1 à droite)

c     Le nombre total de points utilisés pour le calcul des dérivées  !newcost
C        = NBR*2 + 1  (NBR doit être >0! )                            !newcost

      WRITE(666,'(/A,A/60(1H-))')'Les paramètres de contrôle du ',
     *   'calcul des sensibilités sont les suivants:'

      IF(IDIFF.EQ.1) THEN
	  NBR=1  ! pas besoin de calculer d'autres points
	  WRITE(666,*) ' IDIFF=1 : NBR=1'
	  WRITE(666,*) ' Calcul direct des sensibilités '
	  WRITE(666,*) '      via 2 pts: XI-PAS*XI  et XI+PAS*XI'
	ELSE
	  NBR=1 ! au choix de l'utilisateur
	  WRITE(666,*) ' IDIFF=',IDIFF ,' NBR=',NBR
	  WRITE(666,*) ' Calcul direct des sensibilités (moindres carrés)'
	  WRITE(666,*) ' via ',2*NBR+1,' pts: '
	  WRITE(666,*) ' à savoir: XI-NBR*PAS*XI  à  XI+NBR*PAS*XI'
	ENDIF

C     Choix du PAS à gauche et à droite pour le calcul par difference finie.
c     ... X+2*PASS*X,  X-PASS*X  et X+PASS*X , X+2*PASS*X  , ... (X = valeur)
	PASS = 1/20.0   ! càd 1/20 ème de la valeur de la variable de conception

	WRITE(666,'(A,F8.5,A)')
     * ' et PAS =',PASS, '(à multiplier par chaque variable de concep)'


  1   CONTINUE

      DO 5 J=1,NETO ! boucle sur le nombre de panneaux

	NBRXI = NVAR(J)
	DENS = SPEC(J)/9.81
	TEMP = PHIL(J) * Q(J) * WIDTH * PI/180.

      DO 10 I=1,NBRXI  ! boucle sur les variables de conception du panneau J
      II = I + NCONT	! compteur du nbre de variable de conception
      I1 = NXIT(I,J)

      IF (ICOUT.EQ.2) GOTO 2
     	
c	IF (I1.EQ.1.OR.I1.EQ.2.OR.I1.EQ.5.OR.I1.EQ.6.OR.I1.EQ.9) THEN  ! sinon dérivée nulle
	IF (I1.EQ.4.OR.I1.EQ.7.OR.I1.EQ.8) GOTO 777  ! sinon dérivée nulle
      
C 3.1 Lecture de la valeur de la variable de conception i du panneau j
c      et des bornes min et max de cette variable de conception (cfr base de données)

	IF(ISECT(J).EQ.0) THEN											!18.03.04
      IF (I1.EQ.1) THEN          ! Rem. : on ne se sert pas du paramètre PENTE pour l'instant               
	  VALEUR = VEPAISS(J)      ! PENTE = +1 : la pente "globale" est positive 
c	  PENTE = 1                ! PENTE = -1 : la pente "globale" est négative                  
c       BORNEMIN = 0.012    !!!!les bornes sur les variables de conception doivent être
c	  BORNEMAX = 0.040    !!!!prises entre ces valeurs   	                                             
	ELSE IF (I1.EQ.2) THEN
	  VALEUR = VHAC(J)
c	  PENTE = 1
c	  BORNEMIN = 0.
c	  BORNEMAX = 99999.
	ELSE IF (I1.EQ.3) THEN
	  VALEUR = VEAC(J)
c	  PENTE  = 1
c	  BORNEMIN = 0.
c	  BORNEMAX = 99999.
	ELSE IF (I1.EQ.4) THEN
	  VALEUR = VLSC(J)
c	  PENTE = 1
c	  BORNEMIN = 0.
c	  BORNEMAX = 99999.
	ELSE IF (I1.EQ.5) THEN
	  VALEUR = DELTAC(J)
c	  PENTE = -1
c	  BORNEMIN = 0.
c	  BORNEMAX = 99999.
	ELSE IF (I1.EQ.6) THEN
	  VALEUR = VHAR(J)
c	  PENTE = 1
c	  BORNEMIN = 0.250
c	  BORNEMAX = 0.430
	ELSE IF (I1.EQ.7) THEN
	  VALEUR = VEAR(J)
c	  PENTE = 1
c	  BORNEMIN = 0.      !0.010
c	  BORNEMAX = 99999.  !0.017
	ELSE IF (I1.EQ.8) THEN
	  VALEUR = VLSR(J)
c	  PENTE = 1
c	  BORNEMIN = 0.      !0.100
c	  BORNEMAX = 99999.  !0.250
	ELSE IF (I1.EQ.9) THEN
	  VALEUR = DELTAR(J)
c	  PENTE = -1
c	  BORNEMIN = 0.
c	  BORNEMAX = 99999.
	ENDIF
	ELSE										!18.03.04
      IF (I1.EQ.1) THEN		   				    !18.03.04               
	  VALEUR = VHAC(J)							!18.03.04
      ELSEIF (I1.EQ.2) THEN						!18.03.04               
	  VALEUR = VEAC(J)							!18.03.04
      ELSEIF (I1.EQ.3) THEN						!18.03.04               
	  VALEUR = VLSC(J)							!18.03.04
      ELSEIF (I1.EQ.4) THEN						!18.03.04               
	  VALEUR = VDIFFC(J)						!18.03.04
      ELSEIF (I1.EQ.5) THEN						!18.03.04               
	  VALEUR = DELTAC(J)						!18.03.04
	ENDIF										!18.03.04
	ENDIF				!18.03.04

C     Calcul du PAS à gauche et à droite pour le calcul par difference finie.
c     ... X+2*PASS*X,  X-PASS*X  et X+PASS*X , X+2*PASS*X  , ... (X = valeur)
	PAS = PASS* VALEUR   ! avec PASS= 1/20 ème de la valeur de la variable de conception
	
C 3.2  Bornes min et max: CHOIX

c Faut-il aller lire directement ces bornes dans le fichier base.txt ?
c Difficile compte tenu du fait qu'il n'est écrit nulle part quelle
c variable de conception est associée à chaque opération => de toute
c façon, la procédure ne peut donc être totalement automatique =>
c c'est pas trop grave si on introduit les bornes ici.   
c !!!attention si on change les valeurs du fichier base.txt !!!!!!!!
c Rem. : Il faut prendre comme bornes celles des opérations les plus 
c        restrictives (càd dont l'intervalle de donnée est le plus petit)

C     Modification du pas pour ne pas sortir de la base de données :
c	IF ((VALEUR-NBR*PAS).LT.BORNEMIN)  ! si borne min dépassée
c     *    PAS=(VALEUR - BORNEMIN)/NBR - 10.0E-8
c	IF ((VALEUR+NBR*PAS).GT.BORNEMAX)  ! si borne max dépassée
c     *    PAS=(BORNEMAX - VALEUR)/NBR - 10.0E-8 ! coefficient de sécurité vis à vis des erreurs de calcul

C 3.3 Détermination de la dérivée en VALEUR (càd au point courant de la variable de conception considérée)

	IF(ISECT(J).EQ.0) THEN							!18.03.04
      DO K=1,2*NBR+1  ! boucle pour le calcul par différences finies
        X(K) = VALEUR  + (K-1-NBR)*PAS
	  IF (I1.EQ.1) VEPAISS(J) = X(K)
	  IF (I1.EQ.2) VHAC(J)    = X(K)
	  IF (I1.EQ.3) VEAC(J)    = X(K)
	  IF (I1.EQ.4) VLSC(J)    = X(K)
	  IF (I1.EQ.5) DELTAC(J)  = X(K)	
	  IF (I1.EQ.6) VHAR(J)    = X(K)
	  IF (I1.EQ.7) VEAR(J)    = X(K)
	  IF (I1.EQ.8) VLSR(J)    = X(K)
	  IF (I1.EQ.9) DELTAR(J)  = X(K)

	  CALL COSTMAIN(Y(K),VLARG,VEPAISS,VHAC,VEAC,VLSC,VESC,
     *    DELTAC,VHAR,VEAR,VLSR,VESR,DELTAR,ENTR,VLENGTH,NETO,NON,I1,
     *    CostPAN,,NN,NVAR,NXIT,TRAV,totalTI)

c	    COUTPAN = RRO de chaque panneau
c	    COUTRRO = RRO Structure globale
c         TotalTI = TI Structure globale

c        CALL ANNULD(TRAV,67743)

        IF (K.EQ.(NBR+1).AND.I.EQ.1) then
c         Sauvegarde de COUTRRO et des COUTPAN(J) au point courant.
	     COUTRRO = Y(K)
	     DO JJ=1,NETO
             CoutPAN(JJ)=CostPAN(JJ)
	     ENDDO
	  ENDIF
	  IF (K.EQ.2*NBR+1) THEN
	    IF (I1.EQ.1) VEPAISS(J) = VALEUR
          IF (I1.EQ.2) VHAC(J)    = VALEUR
	    IF (I1.EQ.3) VEAC(J)    = VALEUR
	    IF (I1.EQ.4) VLSC(J)    = VALEUR
	    IF (I1.EQ.5) DELTAC(J)  = VALEUR
	    IF (I1.EQ.6) VHAR(J)    = VALEUR
	    IF (I1.EQ.7) VEAR(J)    = VALEUR
	    IF (I1.EQ.8) VLSR(J)    = VALEUR
	    IF (I1.EQ.9) DELTAR(J)  = VALEUR
	  ENDIF 

	ENDDO  ! DO K=1,2*NBR+1
	ENDIF										!18.03.04

c     Choix du mode de calcul des dérivées:
c       - soit locale au ppoint courant
c       - soit approchée par la méthode des moindres carrées (IDIFF=0)

c      IDIFF = 0   ! si = 0, interpolation des moindres carrés
	            ! si = 1, simple différence finie pour le calcul de la dérivée

	IF (IDIFF.EQ.0) THEN    ! moindres carrés
	 S = 0.0
	 W = 0.0
       N = 2*NBR + 1 
       P = 1
       DO M=1,2*P
	    DO I2=1,N
	      S(M) = S(M) + X(I2)**M
  	    ENDDO 
       ENDDO
       DO M=1,P+1
	    DO I2=1,N
	      W(M) = W(M) + Y(I2)*X(I2)**(M-1)
   	    ENDDO 
       ENDDO
       A(1,1) = N
	 A(1,2) = S(1)
	 A(2,1) = S(1)
	 A(2,2) = S(2)
	 C(1) = W(1)
	 C(2) = W(2)
	 DTM = A(1,1)*A(2,2) - A(2,1)*A(1,2)
	 INVA(1,1) =  A(2,2)/DTM
	 INVA(1,2) = -A(1,2)/DTM
	 INVA(2,1) = -A(2,1)/DTM
	 INVA(2,2) =  A(1,1)/DTM
	 B(1) = INVA(1,1)*C(1) + INVA(1,2)*C(2)
	 B(2) = INVA(2,1)*C(1) + INVA(2,2)*C(2)  ! dérivée obtenue au point courant (valeur)

	 DERIVEE(II) = B(2)       ! (en RRO)   selon la méthode des moindres carrés             

	ELSE      ! si IDIFF = 1                  
       DERIVEE(II) = (Y(NBR+2) - Y(NBR))/(2*PAS) ! (en RRO) , calcul local
	ENDIF

	DERIVEE(II) = DERIVEE(II)*EQPC1  ! = dérivée du coût de fabrication en Euro

  777 CONTINUE
c      ENDIF !! IF (I1.EQ.1.OR.I1.EQ.2.OR.I1.EQ.5.OR.I1.EQ.6.OR.I1.EQ.9) THEN 


   2  CONTINUE

C 3.4 Calcul de la dérivée du coût des matériaux (DFCT) en Euro
	IF(ISECT(J).EQ.0) THEN							!18.03.04
	IF (I1.EQ.1) THEN
	  DFCT(II) = TEMP*DENS*C1*(1 + (2*VEPAISS(J)-Dref)*1000.*DC1) ! = dérivée du coût des matériaux
	                        
	ELSE IF (I1.EQ.2) THEN
	  DFCT(II) = TEMP*DENS*C3/DELTAC(J)*
     *        (1 + (VEAC(J) - Dref)*1000.*DC1)*(1. + DW3)*VEAC(J)
	  
	ELSE IF (I1.EQ.3) THEN
	  DFCT(II) = TEMP*DENS*C3/DELTAC(J)*
     *        ( 1000.*DC1*(1 + DW3)*(VEAC(J)*VHAC(J) + VESC(J)*VLSC(I))
     *      +   (1 + (VEAC(J) - Dref)*1000.*DC1)*(1 + DW3)*VHAC(J)     )
	  
	ELSE IF (I1.EQ.4) THEN
	  DFCT(II) = TEMP*DENS*C3/DELTAC(J)*
     *        (1 + (VEAC(J) - Dref)*1000.*DC1)*(1 + DW3)*VESC(J)
        
	ELSE IF (I1.EQ.5) THEN 
	  DFCT(II) = -TEMP*DENS*C3/DELTAC(J)**2 *
     *        (1 + (VEAC(J) - Dref)*1000.*DC1)*(1 + DW3)   
     *         *(VEAC(J)*VHAC(J) + VESC(J)*VLSC(J))
	  
	ELSE IF (I1.EQ.6) THEN
	  DFCT(II) = TEMP*DENS*C2/DELTAR(J)*
     *        (1 + (VEAR(J) - Dref)*1000.*DC1)*(1. + DW2)*VEAR(J)

	ELSE IF (I1.EQ.7) THEN
	  DFCT(II) = TEMP*DENS*C2/DELTAR(J)*
     *        ( 1000.*DC1*(1 + DW2)*(VEAR(J)*VHAR(J) + VESR(J)*VLSR(I))
     *      +   (1 + (VEAR(J) - Dref)*1000.*DC1)*(1 + DW2)*VHAR(J)     )
	  
      ELSE IF (I1.EQ.8) THEN
	  DFCT(II) = TEMP*DENS*C2/DELTAR(J)*
     *        (1 + (VEAR(J) - Dref)*1000.*DC1)*(1 + DW2)*VESR(J)
	
	ELSE IF (I1.EQ.9) THEN
	  DFCT(II) = -TEMP*DENS*C2/DELTAR(J)**2 *
     *        (1 + (VEAR(J) - Dref)*1000.*DC1)*(1 + DW2)   
     *         *(VEAR(J)*VHAR(J) + VESR(J)*VLSR(J))
	ENDIF
	ENDIF										!18.03.04
C 3.5 Dérivée total : Mdo + coût des matériaux (en Euro)

      DERIVEE(II) = DERIVEE(II) + DFCT(II)       ! = dérivée TOTALE du coût en Euro

c	IF ((DERIVEE(II).LE.+0.01).AND.(PENTE.GT.0)) DERIVEE(II) = +0.01
c	IF ((DERIVEE(II).GE.-0.01).AND.(PENTE.LT.0)) DERIVEE(II) = -0.01


  10  CONTINUE ! Fin de la boucle sur les variables de conception
      NCONT = NCONT + NBRXI

   5  CONTINUE ! Fin de la boucle sur le nombre de panneaux (sur J)

c =====================================================================================

C 3.6 COUT TOTAL (Mat + Mdo)
      OBJ = Fmat + COUTRRO*EQPC1

c  --------------------------------------------------------------------------------
C 3.7 WARNING si bornes des variables de conception dépassée dans la banque de données
      IF (NON.GT.1.AND.NON.LT.200) THEN
	 NUMOP = NON - 100
	 WRITE(666,*)'ATTENTION, CERTAINES VARIABLES DE CONCEPTION ONT', 
     * ' UNE VALEUR QUI SORT DE LA BASE DE DONNEES PERMETTANT DE',
     * ' CALCULER LE COUT (-->> VALEUR TROP PETITE)'
	 WRITE(666,*) 'IL S''AGIT DE L''OPERATION : ',NUMOP
	 WRITE(*,*)'ATTENTION, CERTAINES VARIABLES DE CONCEPTION ONT UNE', 
     * ' VALEUR QUI SORT DE LA BASE DE DONNEES PERMETTANT DE CALCULER',
     * ' LE COUT (VALEUR TROP PETITE)'
	 WRITE(*,*) 'IL S''AGIT DE L''OPERATION : ',NUMOP

	 WRITE(29,*)'ATTENTION, CERTAINES VARIABLES DE CONCEPTION ONT',					!bug
     * ' UNE VALEUR QUI SORT DE LA BASE DE DONNEES PERMETTANT DE',					!bug
     * ' CALCULER LE COUT (-->> VALEUR TROP PETITE)'									!bug
	 WRITE(29,*) 'IL S''AGIT DE L''OPERATION : ',NUMOP								!bug

c	 PAUSE 

      ELSEIF (NON.GT.200) THEN
	 NUMOP = NON - 200
	 WRITE(666,*)'ATTENTION, CERTAINES VARIABLES DE CONCEPTION ONT', 
     * ' UNE VALEUR QUI SORT DE LA BASE DE DONNEES PERMETTANT DE',
     * ' CALCULER LE COUT (-->> VALEUR TROP GRANDE)'
	 WRITE(666,*) 'IL S''AGIT DE L''OPERATION : ',NUMOP
	 WRITE(*,*)'ATTENTION, CERTAINES VARIABLES DE CONCEPTION ONT UNE', 
     * ' VALEUR QUI SORT DE LA BASE DE DONNEES PERMETTANT DE CALCULER',
     * ' LE COUT (VALEUR TROP GRANDE)'
	 WRITE(*,*) 'IL S''AGIT DE L''OPERATION : ',NUMOP

	 WRITE(29,*)'ATTENTION, CERTAINES VARIABLES DE CONCEPTION ONT',					!bug
     * ' UNE VALEUR QUI SORT DE LA BASE DE DONNEES PERMETTANT DE',					!bug
     * ' CALCULER LE COUT (-->> VALEUR TROP GRANDE)'									!bug
	 WRITE(29,*) 'IL S''AGIT DE L''OPERATION : ',NUMOP								!bug

c	 PAUSE

	ENDIF

c =========================================================================================
c =========================================================================================
C
C 4.  Impression des différents coûts dans OPT
C -------------------------------------------
      POIDS=0.0

      DO J=1,NETO
C      FCP = Poids = bordage + ames + semelles  (du panneau NEL)
       TEMP = PHIL(j) * Q(j) * WIDTH * PI/180.
	 DENS = SPEC(j)/9.81
	IF(ISECT(J).EQ.0) THEN												!18.03.04
       FCP= VEPAISS(j) 
     *  +  (1.0+DW3)*(VEAC(j) * VHAC(j) + VESC(j)* VLSC(j)) /DELTAC(j)
     *  +  (1.0+DW2)*(VEAR(j) * VHAR(j) + VESR(j)* VLSR(j)) /DELTAR(j)
	 ELSEIF(ISECT(J).EQ.1) THEN											!cercle !18.03.04
       FCP=(1.0+DW3)*PI*(VHAC(j)*VHAC(j)-(VHAC(j)-2*VDIFFC(J))**2) /		!18.03.04
     *	 (4*DELTAC(j))													!18.03.04
c     *  +  (1.0+DW2)*(VEAR(j) * VHAR(j) + VESR(j)* VLSR(j)) /DELTAR(j)	!05.12.05
	 ELSEIF(ISECT(J).EQ.2) THEN											!carre !18.03.04
      FCP=(1.0+DW3)*(VHAC(j)*VHAC(j)-(VHAC(j)-2*VDIFFC(J))**2)/DELTAC(j)	!18.03.04
c     *  +  (1.0+DW2)*(VEAR(j) * VHAR(j) + VESR(j)* VLSR(j)) /DELTAR(j)	!05.12.05
	 ELSEIF(ISECT(J).EQ.3) THEN											!doubleT !18.03.04
       FCP=(1.0+DW3)*(VEAC(j)*2* VHAC(j)+2*VESC(j)* VLSC(j)) /DELTAC(j)	!18.03.04
c     *  +  (1.0+DW2)*(VEAR(j) * VHAR(j) + VESR(j)* VLSR(j)) /DELTAR(j)	!05.12.05
	ENDIF																!18.03.04

       FCP=  FCP * TEMP * SPEC(j)  ! poids du panneau (borde, raid et cadre only)
	 POIDS=POIDS+ FCP

       IF (J.EQ.1) THEN
        WRITE(666,*) 
	  WRITE(666,*)'*** COST Objective Function (Subr. SENSIBCOUT)'
	  WRITE(666,*)'    ------------------------------------------'
	  WRITE(666,*)'         (with additional corrosion thickness)'
	  WRITE(666,*)'           (girder not considered !!!)'
	  WRITE(666,*)
	  WRITE(666,42)

   42 FORMAT(3x,'Panel',T13,' WEIGHT',      T26,'COST(Mat+Labor)',
     *                  T42,'Material Cost',T58,'Labor Cost',
     *                  T73,' (RRO Labor) '/
     *       ' Panneau',T13,' POIDS',       T26,'COUT(Mat+Labor)',
     *                  T42,'Coût Matière ',T57,' Cost MDO',
     *                  T73,' (RRO MDO) ' /
     *                 ,T13,'  Newton',     T26,' Euro (or ...)',
     *                  T42,' Euro (or...)',T57,' Euro (or ...)',
     *                  T73 ' Hours(heures)')
    	 ENDIF

       CTPANEL=COUTMAT(j)+CoutPAN(j)*EQPC1

       WRITE(666,43) J,FCP,CTPANEL,COUTMAT(j),
     *                             CoutPAN(j)*EQPC1,CoutPAN(j)
   43  FORMAT(4x,I3,T10,5(F14.3,1x) )   


       IF (J.EQ.NETO) THEN
        WRITE(666,*) 
        WRITE(666,* )' TOTAL for the structure (with Gross Thickness)'
        WRITE(666,* )' ----------------------------------------------'
        WRITE(666,46)' COUT-COST(Mat+Mdo) = ',OBJ  ,' Euro (or...)'
        WRITE(666,46)' - Total Labor Cost = ',COUTRRO*EQPC1,
     *                                                ' Euro (or...)'
        WRITE(666,46)'   Labor time (RRO) = ',COUTRRO,' hours'
	  WRITE(666,46)'   Labor time (TI)  = ',TotalTI,' hours'
        WRITE(666,46)'    [Rate: 1 hour   = ',EQPC1,' Euro (or...) ]'
        WRITE(666,46)' - Total Mat. cost  = ',Fmat ,' Euro (or...)'
        WRITE(666,46)'   POIDS - WEIGHT   = ',POIDS,' N'
	 ENDIF

	ENDDO  ! DO J=1,NETO

C
C Impression des différents coûts dans Sol2
C -----------------------------------------

      WRITE(67,*)
	WRITE(67,*)
	 
	DO J=1,NETO

C     Fmat= Coût des matériaux (acier)
C     FMdO= Coût Main d'oeuvre
C     FCT = Fmat + FMdO  (d'un panneau NEL),
C     OBJ = Coût total          (de la structure complete)

      IF(J.EQ.1) Then
	 COUT=0. 
	 POIDS=0.
	 CALL ANNULD(Poids,18)
	ENDIF

C     DCOR = epaisseur de corrosion pour bordé

	WRITE(66,*) 
	WRITE(66,*) ' Avec sur-épaisseurs de corrosion'
	WRITE(66,*) ' --------------------------------'
	WRITE(66,*) ' *** Sur-épaisseur=',DCOR(j),' m'
	WRITE(66,*) ' - Epaiss bordé      =',VEPAISS(j)
	WRITE(66,*) ' - Epaiss âme cadres =',VEAC(j)
	WRITE(66,*) ' - Epaiss sem cadres =',VESC(j)
	WRITE(66,*) ' - Epaiss âme raid   =',VEAR(j)
	WRITE(66,*) ' - Epaiss sem raid   =',VESR(j)

	DENS =  SPEC(j)/9.81
      TEMP =  PHIL(j) * Q(j) * WIDTH * PI/180.
	TEMP1=  TEMP * DENS  
	TEMP2=  TEMP * REND * EQPC1 

c     Supplément de Poids (en N) (sans les traverses)
	IF(ISECT(J).EQ.0) THEN												!18.03.04
      SPoid1 = TEMP*SPEC(j) * ( DCOR(j) +
     *        (1.0+DW2)*(DCOR3(j)*VHAR(j)+DCOR3(j)*VLSR(j))/DELTAR(j) +
     *        (1.0+DW3)*(DCOR2(j)*VHAC(j)+DCOR2(j)*VLSC(j))/DELTAC(j)  )
       Poid1 = TEMP*SPEC(j) * ( VEPAISS(j) +
     *          (1.0+DW2)*(VEAR(j)*VHAR(j)+VESR(j)*VLSR(j))/DELTAR(j) +
     *          (1.0+DW3)*(VEAC(j)*VHAC(j)+VESC(j)*VLSC(j))/DELTAC(j)  )
	 ELSEIF(ISECT(J).EQ.1) THEN												!18.03.04
      SPoid1 = TEMP*SPEC(j) *													!18.03.04
c     *        ((1.0+DW2)*(DCOR3(j)*VHAR(j)+DCOR3(j)*VLSR(j))/DELTAR(j) +		!05.12.05
     *	    (1.0+DW3)*PI*(VHAC(j)*VHAC(j)-(VHAC(j)-2*DCOR2(j))**2)/			!18.03.04
     *	    (4*DELTAC(j)  )!)												!05.12.05
       Poid1 = TEMP*SPEC(j) * (												!26.05.04
c     *        (1.0+DW2)*(VEAR(j)*VHAR(j)+VESR(j)*VLSR(j))/DELTAR(j) +		!05.12.05
     *		(1.0+DW3)*PI*(VHAC(j)*VHAC(j)-(VHAC(j)-2*VDIFFC(J))**2)/		!18.03.04
     * 	   	(4*DELTAC(j)))													!18.03.04
	ELSEIF(ISECT(J).EQ.2) THEN												!18.03.04
	 SPoid1 = TEMP*SPEC(j) *												!18.03.04
c     *        ((1.0+DW2)*(DCOR3(j)*VHAR(j)+DCOR3(j)*VLSR(j))/DELTAR(j) +		!05.12.05
     *	    (1.0+DW3)*(VHAC(j)*VHAC(j)-(VHAC(j)-2*DCOR2(j))**2)/			!18.03.04
     *	    DELTAC(j) ! )													!05.12.05
       Poid1 = TEMP*SPEC(j) * (												!26.05.04
c     *          (1.0+DW2)*(VEAR(j)*VHAR(j)+VESR(j)*VLSR(j))/DELTAR(j) +		!05.12.05
     *		  (1.0+DW3)*(VHAC(j)*VHAC(j)-(VHAC(j)-2*VDIFFC(J))**2) /		!18.03.04
     * 	  	  (DELTAC(j)))													!18.03.04
	ELSEIF(ISECT(J).EQ.3) THEN												!18.03.04
	 SPoid1 = TEMP*SPEC(j) *												!18.03.04
c     *      ((1.0+DW2)*(DCOR3(j)*VHAR(j)+DCOR3(j)*VLSR(j))/DELTAR(j) +		!05.12.05
     *      (1.0+DW3)*(DCOR2(j)*2*VHAC(j)+2*DCOR2(j)*VLSC(j))/DELTAC(j)!)		!05.12.05
       Poid1 = TEMP*SPEC(j) * (												!26.05.04
c     *          (1.0+DW2)*(VEAR(j)*VHAR(j)+VESR(j)*VLSR(j))/DELTAR(j) +		!05.12.05
     *        (1.0+DW3)*(VEAC(j)*2*VHAC(j)+2*VESC(j)*VLSC(j))/DELTAC(j))		!18.03.04
	ENDIF																	!18.03.04

	SPoids=SPoids+SPoid1 ! Supplément de poids dû à la corrosion
	 Poids =Poids+ Poid1 ! Poids + poids additionnel dû à DW2 et DW3

	IF(ISECT(J).EQ.0) THEN													!18.03.04
c     Coût des plaques
      Fmat1 = TEMP1 *C1* (1.+(VEPAISS(j)-Dref)*1000.*DC1) * VEPAISS(j) 
c     Coût des lisses
      Fmat2 = TEMP1 *C2* (1.+DW2) * (1.+(VEAR(j)-Dref)*1000.*DC1)
     *                     * (VEAR(j)*VHAR(j)+VESR(j)*VLSR(j))/DELTAR(j)  
c     Coût des cadres
      Fmat3 = TEMP1 *C3* (1.+DW3) * (1.+(VEAC(j)-Dref)*1000.*DC1)
     *                     * (VEAC(j)*VHAC(j)+VESC(j)*VLSC(j))/DELTAC(j) 
      elseif(isect(j).eq.1) then												!26.05.04
	Fmat1 = 0.000															!26.05.04
	Fmat2 = 0.000															!26.05.04
	Fmat3 =  TEMP1 *C3* (1.+DW3) *(1.+(VEAC(j)-Dref)*1000.*DC1)				!26.05.04
     *   *PI*(VHAC(j)*VHAC(j)-(VHAC(j)-2*VDIFFC(j))**2) /(4*DELTAC(j))		!26.05.04
      elseif(isect(j).eq.2) then												!26.05.04
	Fmat1 = 0.000															!26.05.04
	Fmat2 = 0.000															!26.05.04
	Fmat3 =  TEMP1 *C3* (1.+DW3) *(1.+(VEAC(j)-Dref)*1000.*DC1)				!26.05.04
     *   *(VHAC(j)*VHAC(j)-(VHAC(j)-2*VDIFFC(j))**2) /DELTAC(j)				!26.05.04
      elseif(isect(j).eq.3) then												!26.05.04
	Fmat1 = 0.000															!26.05.04
	Fmat2 = 0.000															!26.05.04
	Fmat3 =  TEMP1 *C3* (1.+DW3) *(1.+(VEAC(j)-Dref)*1000.*DC1)				!26.05.04
     *   *(VEAC(j)*2* VHAC(j)+2*VESC(j)* VLSC(j)) /DELTAC(j)					!26.05.04
      endif																	!26.05.04
c     Coûts cumulés: for all the panels 
        Fmat11 = Fmat11+Fmat1 ! matière bordé
        Fmat22 = Fmat22+Fmat2 ! matière lisses
        Fmat33 = Fmat33+Fmat3 ! matière cadres
	  FFmat  = Fmat1+Fmat2+Fmat3
	  Fmat   = Fmat + FFmat

	  FCT    = FFmat+ CoutPAN(j)*EQPC1

	  PPo=Poid1-SPoid1
	  CoutPAN(j) = CoutPAN(j)*EQPC1

       IF (J.EQ.1) THEN
        WRITE(67,*) 
	  WRITE(67,*)'*** COST AND WEIGHT (Subr. SENSIBCOUT)'
	  WRITE(67,*)'--------------------------------------'
	  WRITE(67,*)'  (with additional corrosion thickness)'
	  WRITE(67,*)'        (girder not considered !!!)'
	  WRITE(67,*)
	  WRITE(67,'(5x,2A)')'  Weight with Corrosion Thickness  ',
     *                   '            Cost (using database)'
	  WRITE(67,'(5x,2A)')'         (in NEWTON)               ',
     *                   '              (in Euro , $ or ...) '
	  WRITE(67,44)
   44   FORMAT(/' Panel',T12,'Net weight',T24,'Gross weight',
     *                   T37,'Extra weight ',T51,'Total Cost',
     *                   T62,'Material Cost',T76,'Labor Cost '/)
    	 ENDIF

       WRITE(67,45) J,PPo,Poid1,SPoid1,FCT,FFmat,CoutPAN(j)
   45  FORMAT(4x,I3,T10,6(F12.3,1x) )   

      IF(J.EQ.NETO) THEN
	 COUTRRO = COUTRRO 
	 COUT    = Fmat + COUTRRO*EQPC1  !pour avoir un coût en euro
	 PPo=Poids-Spoids

       IZ=67
	 WRITE(IZ,'(/70(1H*)/)') 
	 WRITE(IZ,* )'Total Weight: Complete structure (girders excluded)'
	 WRITE(IZ,* )'---------------------------------------------------'
       WRITE(IZ,46)'   Weight (net   thickness)       = ',PPo   ,' N'
       WRITE(IZ,46)'   Weight (corrosion thickness)       = ',SPoids,'N' !15.10.05
       WRITE(IZ,46)'   Weight (Gross thickness)       = ',Poids ,' N'
       WRITE(IZ,* )
	 WRITE(IZ,* )'Total cost: Complete structure (girders excluded)'
	 WRITE(IZ,* )'-------------------------------------------------'
       WRITE(IZ,46)'1* Material from plating       = ',Fmat11,' Euro,$'
       WRITE(IZ,46)'   Material from long stiff.   = ',Fmat22,' Euro,$'
       WRITE(IZ,46)'   Material from trans. frame  = ',Fmat33,' Euro,$'
       WRITE(IZ,46)'   MATERIAL COST (TOTAL)       = ',Fmat  ,' Euro,$'
       WRITE(IZ,* )
       WRITE(IZ,46)'2* LABOR COST (TOTAL)          = ',COUTRRO*EQPC1,
     *                                                        ' Euro,$'
       WRITE(IZ,46)'     RRO      (TOTAL)          = ',COUTRRO,' hours'
	 WRITE(IZ,46)'     TI       (TOTAL)          = ',TotalTI,' hours'
       WRITE(IZ,46)
       WRITE(IZ,46)'*** TOTAL COST  (1+2)          = ',COUT  ,' Euro,$'
       WRITE(IZ,* )
	ENDIF

      ENDDO ! fin de la boucle sur le nombre de panneaux

      RETURN
   46  FORMAT(A,F14.3,A)   
	END