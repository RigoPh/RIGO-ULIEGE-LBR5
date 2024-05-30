      SUBROUTINE OPTIS(ITERA,ITERAM,NETO,WIDTH,IPRINT,NSOL,
     *                 XI,FI,CJ,CJM,CIJ,S,
     *                 DX,Z1,Z2,Z3,FIMULTI,VNOM2,SPEC,					!multi obj
     *                 NVAR,M1TABL,M1CONT,M2CONT,NXIT,NVARR,NXITR,		!dcn
     *                 XICOU,XIMIN,XIMAX,
     *                 EGA,MEGA,NXI2,NEGAL,NEGALT,ITYPE,ISECT,			!février 2004
     *				 MODES,PHILN,QN,CORRO,IMPR,IWEIGHT,IPRICE,INERT,	!janvier 2005  + !corrosion  !15.10.05  +  !restri poids
c																		!restri inertie  + !restri cout 
     *			     IMOD,IXXTOT,OMET,TETAS,Z,DELT,SYMX,SYM,			!restri module + !obj inertie  
     *				 YNEUTPART,PART,									!r&d13
     *				 IMULTI,FK,IICOUT,RHO,W1,W2,W3)						!multi obj

C  *****************************************************************
c  Soit "NVmax" variables et "Mmax" restrictions
C        ****************     ******************
C            avec NSOL le nombre de cas de charge (max ISmax=5)
C
C  Créer: 25-4-94
C  Modif: 1-12-95
C         8- 5-96 - Suppression des relectures du fichier 201. Maintenant,
C                   les variables de conception sont stockées ds le
C                   common/opti5/
C         4-06-96 - restrictions géométriques (M2 et M2CONT)
C         1-08-96 - formatage des WRITE(IUIN,*)
C        20-03-97 - Restrictions relatives à la resistance ultime d'ensemble
C                   (USHULL)
C        18-05-98 - Fonction objectif COUT (subr OBJEC2 et OBJEC3)
C        10-04-99 - Allocation mémoire pour LBR-5.1
c        09-11-00 - restr. sur centre de gravite LBR-5.3
c        18-11-01 - sur-épaisseur de corrosion, LBR5-5
c	   05-02-04 - optimisation plaques sans opti épontilles
C
C  Dernière modif: 05-02-04
C  *****************************************************************

      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER *30 TITRE
      CHARACTER *12 VNOM2
	CHARACTER*3  MODE								!janvier 2005
	CHARACTER *3  MODES(NETO)						!janvier 2005

      COMMON/DIM1/Nmax,NEmax,NVmax,
     *            M1max,M1Tmax,M2max,M2Tmax,Mmax,NGmax,ISmax,IPTmax ! dimension max des vecteurs

	DIMENSION VNOM(9),SPEC(NETO)
	DIMENSION ITYPE(neto),ISECT(neto)				!février 2004

C     COMMON/ALLONG/XI(200),FI(200),CJ(700),CJM(700),
C    *              CIJ(200,700),VNOM2(700),S(1000)
      DIMENSION XI(NVmax),FI(NVmax),CJ(Mmax),CJM(Mmax),
     *          CIJ(NVmax,Mmax),VNOM2(Mmax),S(1),
     *          DX(NVmax,NVmax),Z1(NVmax),Z2(NVmax),Z3(NVmax)

C     ENT,CONTR,CONLIN  NVAR,NXIT
      COMMON/OPTI/  IOPTI,N,M1,M2
      DIMENSION     NVAR(NETO),M1CONT(NETO),M2CONT(NETO),NXIT(9,NETO),
     *              NVARR(NETO),NXITR(9,NETO)

C     ENT,CONTR     COMMON/OPTI4/  
c     COMMON/OPTI4/ XIMIN(200),XIMAX(200),CJMAX(400),INV(400),NEGALT,
c    *              MEGA(100,4),EGA(100),NVS(30),NXI2(9,30),NEGAL(30)
      DIMENSION     XIMIN(NVmax),XIMAX(NVmax),
     *              EGA(NGmax),MEGA(NGmax,4),NXI2(9,NETO),NEGAL(NETO)

c     ENT,OPTI  COMMON/OPTI5/
      DIMENSION XICOU(NVmax)    
	DIMENSION CORRO(NETO,3)			!corrosion
  
      COMMON/USHULL/FICTIF(9),IULT,IRESTR

	COMMON/GRAV/BID(5),IGRAV  ! COORD,OPTI
 
      COMMON/COUT/ Poids,SPoids,
     *        Fmat,Fsou,FMdO,Fmat11,Fmat22,Fmat33,Fsou11,Fsou22,
     *        Fsou33,FMdO11,FMdO22,FMdO33,FMdO44,FMdO55,FMdO66,FMdO77,
     *        REND,EQP,Dref,DrefX,DrefY,C1,C2,C3,DC1,DW2,DW3,P10,DP10,
     *        P4,P5,DP4,DP5,P9X,P9Y,DP9X,DP9Y,BER,BET,P6,P7,C8,DC8,COUT,
     *        ICOUT,IALR,IALT            ! MAIN,COST(ENT),OPTI,OBJECT

	COMMON/PY/   PI					   !janvier 2005

	DIMENSION PHILN(NETO),QN(NETO)	   !janvier 2005

	REAL *8 IXXTOT					   !obj inertie
	INTEGER SYMX,SYM				   !obj inertie
	DIMENSION TETAS(NETO),Z(NETO,4)	   !obj inertie
	DIMENSION DELT(NETO)			   !obj inertie
	DIMENSION PART(NETO)			   !r&d13
	DIMENSION FK(3)					   !multi obj
	DIMENSION FIMULTI(NVmax)		   !multi obj

                   
C ******************************************************************************

      DATA VNOM/'Ep. Bord','Hâme Cad','Eâme Cad','Lsem Cad','EPSA Cad',
     *      	  'Hâme Rai','Eâme Rai','Lsem Rai','EPSR Rai'/
C ******************************************************************************

C	LCONT(1,j+IM) = Nø de référence de la restriction j du panneau NEL
C
C  ====================================================================
C
C     MS = Dimension maximale pour le vecteur S(1) utile pour la subr.CONLIN
C     MS =8*N+3*M+N*M+2*MAX0(N,M)+3
C     MS = 145.103   si N=200 et M=700  (NETO=30
C     MS = 813.203   si N=400 et M=2000 (NETO=50)
C     MS =5641.403   si N=800 et M=7000 (NETO=100)

	PI1=PI/180.

      MS=8*NVmax+3*Mmax+NVmax*Mmax+2*MAX0(NVmax,Mmax)+3

C     File 301 : Contient les données pour CONLIN.
      IUIN=301		!extension neto

C     File 302 : Contient les dimensions des panneaux pour le calcul
C                des fcts objectifs (cfr. Subr ENT et OBJECT)
C     File 303 : Les sensibilités calculées par les subr. GEOM, CONTR et USHULL
C     Le COMMON/OPTI5/ conserve les variables de conception entre 2 itérations

C     Indice d'impression (pour Subr. CONLIN)
      IOP=IOPTI
c    
C     Error indicator of the Subr. CONLIN (initially set at 0 )
      NON=0
   
C     M1= Le nombre de restrictions structurelles pour chaque cas de charge 
C         max=M1Tmax pour 1 cas de charge
C     M2= Le nombre de restrictions géométriques (max 20 par panneau),
C     M3= Le nombre de restrictions particulières
C         Soit : Rest. sur centre de gravite (Subr. COORD).
C                   si IGRAV=1 ou 2, on impose 1 restriction  (Min ou max)
C                   si IGRAV=3     , on impose 2 restrictions (Min et max)
C         Soit : Ult. Strength of Ship Hull (Subr. HULL).
C                   si IRESTR=1, on impose 2 restrictions (Hogging et Sagging)
      M3=0
      IF(IRESTR.GE.1)		M3=M3+2
      IF(IGRAV==1)		M3=M3+1
      IF(IGRAV==2)		M3=M3+1
      IF(IGRAV==3)		M3=M3+2
	IF(IWEIGHT.NE.0)	M3=M3+1		!restri poids
	IF(IPRICE.NE.0)		M3=M3+1		!restri cout
	IF(INERT.NE.0)		M3=M3+1		!restri inertie
	IF(IMOD.NE.0)		M3=M3+1		!restri module

C     M = Le nombre total de restrictions pour les NSOL cas de charge.
C         Il faut M  < Mmax (nombre maximum)
c      M=M1*NSOL+M2+M3
      M=M1+M2+M3
    
      IF(M.GT.Mmax) THEN
        WRITE(*,*)
        WRITE(29,*)  'ERROR: Le nbre total de restrictions =',M,'>',Mmax !bug
        WRITE(666,*)'ERROR: Le nbre total de restrictions =',M,'>',Mmax
        WRITE(*,*)
	  PAUSE 'STOP'
        STOP
      ENDIF

C23456789012345678901234567890123456789012345678901234567890123456789012

C  ====================================================================
C 1.   Reduction des vecteurs NVAR et NXIT en NVARR et NXITR
C     ******************************************************
C     Soit NR le nbre total de variables XI indépendantes

      NR=N-NEGALT
	
c	PAUSE'OPT1'
c     WRITE(*,*) NR
              
      IF(ITERA.EQ.1) THEN				
         DO 100 NEL=1,NETO
           NVARR(NEL)=NVAR(NEL)-NEGAL(NEL)
c          WRITE(*,*) NEL
           DO 100 K=1,9
             NXITR(K,NEL)=NXIT(K,NEL)
c            WRITE(*,*) K
  100    CONTINUE

c        WRITE(*,*) ITERA
c	   PAUSE'OPT2'
  
         IF(NEGALT.GT.0) THEN
         WRITE(666,*)
         WRITE(666,*) 'Variables de conception (réduites), NXI(reduit)'
         INE=0
         IVAR=0

         DO 101 NEL=1,NETO                   ! Nø du panneau concerné
           DO 102 I=1,NEGAL(NEL)             
             INE=INE+1                       ! Nø de la contr d'égalité
             KK=NXI2(MEGA(INE,1),NEL) - IVAR ! Ordre du XI concerné ds panneau concerné
             DO 103 K=KK+1-I,(NVAR(NEL)-1)
               NXITR(K,NEL)=NXITR(K+1,NEL)
  103        CONTINUE
	     NXITR(NVAR(NEL),NEL)=0
  102      CONTINUE
         IVAR=IVAR+NVAR(NEL)
         WRITE(666,'(A,I3,A,9I3)')'NXITR(',NEL,')=',(NXITR(L,NEL),L=1,9)
  101    CONTINUE                            
         ENDIF                               ! IF(NEGALT.GT.0)
	
        ENDIF                                ! IF(ITERA.EQ.1)

c	PAUSE'OPT3'
   
C 2.  RESTRICTIONS D'EGALITE ENTRE VARIABLES XI
C     (Calcul de DX =dXI1/dXI2)
C     ******************************************************
      IF(NEGALT.GT.0) THEN
	DO 116 I=1,N
	DO 115 J=1,N
  115 DX(I,J)=0.
  116 DX(I,I)=1.

	DO 105 L=1,NEGALT
	I=NXI2(MEGA(L,1),MEGA(L,2))
	J=NXI2(MEGA(L,3),MEGA(L,4))
      DX(I,J)=EGA(L)
  105 CONTINUE

c	WRITE(666,*) 'Matrice des d(XI1)/d(XI2)'
c	DO 109 I=1,N
c  109 WRITE(666,*) I,' ',(DX(I,J),J=1,N)
      ENDIF

C  ====================================================================
C  ====================================================================
		
C  ====================================================================
C 3.0 Préparation des données pour CONLIN avec les résultats de LBR-5
C  ====================================================================

      IF(ITERA.EQ.1) THEN
        WRITE(*,20)
        WRITE(666,20)
      ENDIF
      WRITE(*,50) ITERA

      REWIND(IUIN)

      IF(IPRINT.GE.1) WRITE(*,51)
      IF(IPRINT.GE.1) WRITE(*,*)' NR=',NR,' M1=',M1,'et M2=',M2

      K=0
      WRITE(*,'(A,5x,9(1X,A8,3X))') ' Panel',VNOM
      DO 922 NEL=1,NETO
        IF(NVAR(NEL).NE.0) THEN
          K2=K+NVAR(NEL)
c         WRITE (*,33) (XIMIN(I),I=K+1,K2)
c         WRITE (*,34) (XICOU(I),I=K+1,K2)
          WRITE (*,'(3x,I3,5x,9(E11.4,1X))') NEL,(XICOU(I),I=K+1,K2)
c         WRITE (*,35) (XIMAX(I),I=K+1,K2)
          K=K2
        ENDIF
  922 CONTINUE

c      IF(IPRINT.GE.1) THEN
c        WRITE(*,*)'CJMAX (bornes des restrictions structurelles)'
c        WRITE(*,18)(CJMAX(I),I=1,M1)
c        WRITE(*,*)'INV =',(INV(I),I=1,M1)
c      ENDIF
	
C 3.1 Données générales
C     -----------------
	TOL=0.0
	CUT=0.0
	PREC=0.0
	PDS=0.0
	JT=0
      JOBJ=0
	
      WRITE (IUIN,*) NR,M             ! NR=N*NSOL, M=Nbr restrict.
      WRITE (IUIN,*) TOL,CUT,PREC,PDS

C 3.2 Variables de conception XI et les bornes
C     ---------------------------------------
                                      !
      DO 39 I=1,N
   39 XI(I)=XICOU(I)                  ! Tous les XI (1 à N)
c     WRITE (*,*)'XI=',(XI(I),I=1,9)
	
      CALL RED1D(XI,XI,N,NXI2,MEGA,NEGALT,NETO,NGmax) ! Réduction à NR

      CALL RED1D(XIMIN,XIMIN,N,NXI2,MEGA,NEGALT,NETO,NGmax)				
      CALL RED1D(XIMAX,XIMAX,N,NXI2,MEGA,NEGALT,NETO,NGmax)		
	
C     Impressions des variables de conception XI et des bornes
	IF(IOPTI.LE.3) GOTO 40
      WRITE (*,53)
  3   K=0
      DO 38 NEL=1,NETO
      IF(NVARR(NEL).NE.0) THEN
        K2=K+NVARR(NEL)
c       WRITE (666,*)'NVARR(NEL)=',NVARR(NEL)
c       WRITE (666,*)'NXITR=',(NXITR(I,NEL),I=1,NVARR(NEL))
        WRITE (*,32) NEL,(VNOM(NXITR(I,NEL)),I=1,NVARR(NEL))
        WRITE (*,33) (XIMIN(I),I=K+1,K2)
        WRITE (*,34) (XI(I),   I=K+1,K2)
        WRITE (*,35) (XIMAX(I),I=K+1,K2)
	  K=K2
      ENDIF
   38 CONTINUE

C     Modifications variables de conception XI et des bornes
      WRITE (*,*) ' '
	ICO=0
      WRITE (*,*) ' VOULEZ VOUS MODIFIEZ UNE BORNE (min ou max) ?'
      WRITE (*,*) ' si NON : taper O ( par défaut)'
      WRITE (*,*) ' si OUI : taper 1 ( Excécution stoppée) '
c     WRITE (*,*) ' si OUI : taper -1 borne min, +1 borne max '
      READ(*,*) ICO
	IF(ICO.EQ.1)  STOP

c	IF((ICO.EQ.-1).OR.(ICO.EQ.+1)) THEN
c      WRITE (*,*) ' De quelle variable voulez vous changer la borne ?'
c      WRITE (*,*) ' '
c        READ(*,*) ICO1
c	IF(ICO.EQ.-1)  THEN
c       WRITE (*,*) ' La valeur actuelle est :',XIMIN(ICO1)
c       WRITE (*,*) ' Quelle sera la nouvelle borne ? Tapez sa valeur.'
c       WRITE (*,*)
c         READ(*,*)XIMIN(ICO1)
c       WRITE (*,*)'La nouvelle borne inf. de XI(',ICO1,')=',XIMIN(ICO1)
c       WRITE(666,*)'!! Mofification d''une borne !!!' 
c       WRITE(666,*)'La nouvelle borne inf. de XI(',ICO1,')=',XIMIN(ICO1)
c	ELSE
c       WRITE (*,*) ' La valeur actuelle est :',XIMAX(ICO1)
c       WRITE (*,*) ' Quelle sera la nouvelle borne ? Tapez sa valeur.'
c       WRITE (*,*)
c         READ(*,*)XIMAX(ICO1)
c       WRITE (*,*)'La nouvelle borne max. de XI(',ICO1,')=',XIMAX(ICO1)
c       WRITE(666,*)'!! Mofification d''une borne !!!' 
c       WRITE(666,*)'La nouvelle borne max. de XI(',ICO1,')=',XIMAX(ICO1)
c	ENDIF
c	GOTO 3
c	ENDIF
  40  CONTINUE

C     Sauvetage pour CONLIN des variables de conception XI et des bornes
      WRITE (IUIN,'(9E14.7)') (XI(I),   I=1,NR)      ! XI (1 à NR)
      WRITE (IUIN,'(9E14.7)') (XIMIN(I),I=1,NR)
      WRITE (IUIN,'(9E14.7)') (XIMAX(I),I=1,NR)


C	+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C	+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c 4.0  Fct Objectif et Restrictions (géométriques, structurelles et particulières)
C	******************************
C	+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

c	PAUSE'OPT4'

C 4.1 Fonction objectif
C     -----------------
      OBJ=0.
	OBJMULTI=0.					!multi obj
	CALL ANNULD(FI,N)
	CALL ANNULD(FIMULTI,N)		!multi obj
	NCONT=0
	FACT=0.						!multi obj

	IF(IMULTI.EQ.1) THEN													!multi obj
	  
	  IF(IICOUT.EQ.(-1)) THEN												!multi obj
	    CALL OBJIN1(NETO,N,NXIT,NVAR,OBJ,FI,ITYPE,PHILN,TETAS,Z,			!multi obj
     *			    DELT,YNEUTPART,SYM,SYMX,PART)							!multi obj	
		FK(3)=OBJ															!multi obj
        ENDIF																	!multi obj

	  IF(IICOUT.EQ.0) THEN													!multi obj
	    DO NEL=1,NETO														!multi obj
            NBRXI=NVAR(NEL)													!multi obj
		  CALL OBJPD1(NEL,NETO,N,NCONT,NXIT(1,NEL),NBRXI,OBJ,FI,WIDTH,		!multi obj
     *                  1,SPEC(NEL),ITYPE(NEL),ISECT(NEL),CORRO)				!multi obj
     		  NCONT=NCONT+NBRXI													!multi obj
	    ENDDO																!multi obj
	    FK(2)=OBJ															!multi obj
        ENDIF																	!multi obj

	  IF(IICOUT.EQ.1) THEN													!multi obj
	    DO NEL=1,NETO														!multi obj
            NBRXI=NVAR(NEL)													!multi obj
            CALL OBJCT1(NEL,NETO,N,NCONT,NXIT(1,NEL),NBRXI,OBJ,FI,WIDTH,		!multi obj	
     *                  IPRINT,SPEC(NEL),ITYPE(NEL),ISECT(NEL),				!multi obj
     *				  CORRO)												!multi obj
	      NCONT=NCONT+NBRXI													!multi obj
	    ENDDO																!multi obj
		FK(1)=OBJ															!multi obj
        ENDIF																	!multi obj
	  
	  IF(IICOUT.GT.1) THEN													!multi obj
	    CALL SENSIBCOUT(FI,NETO,OBJ,NVAR,N,NXIT,SPEC,WIDTH,ISECT,			!multi obj
     *                    CORRO)												!multi obj
		FK(1)=OBJ															!multi obj
	  ENDIF																	!multi obj
	  
	  
	  IF(IICOUT.EQ.(-2)) THEN												!multi obj
        
 	    IF(W3.NE.0) THEN													!multi obj
	      CALL OBJIN1(NETO,N,NXIT,NVAR,OBJMULTI,FIMULTI,ITYPE,PHILN,		!multi obj
     *			      TETAS,Z,DELT,YNEUTPART,SYM,SYMX,PART)					!multi obj	
	      FACT=FACT+(W3*FK(3)/OBJMULTI)**RHO								!multi obj
	      DO I=1,N															!multi obj
	        FI(I)=FI(I)+RHO*(W3*FK(3)/OBJMULTI)**(RHO-1.)*					!multi obj
     *		      W3*FK(3)*(-1.)/(OBJMULTI**2.)*(-FIMULTI(I))				!multi obj
	      ENDDO																!multi obj
	      OBJMULTI=0.														!multi obj
	      CALL ANNULD(FIMULTI,N)											!multi obj
	      REWIND 302														!multi obj
		ENDIF																!multi obj
  
	    IF(W2.NE.0) THEN													!multi obj
		  DO NEL=1,NETO														!multi obj
              NBRXI=NVAR(NEL)													!multi obj
		    CALL OBJPD1(NEL,NETO,N,NCONT,NXIT(1,NEL),NBRXI,OBJMULTI,		!multi obj
     *                    FIMULTI,WIDTH,1,SPEC(NEL),ITYPE(NEL),				!multi obj
     *					ISECT(NEL),CORRO)									!multi obj
		    NCONT=NCONT+NBRXI												!multi obj
	      ENDDO																!multi obj
	      FACT=FACT+(W2*OBJMULTI/FK(2))**RHO								!multi obj
	      DO I=1,N															!multi obj
	        FI(I)=FI(I)+RHO*(W2*OBJMULTI/FK(2))**(RHO-1.)*					!multi obj
     *                 W2/FK(2)*FIMULTI(I)									!multi obj
	      ENDDO																!multi obj
	      OBJMULTI=0.														!multi obj
	      CALL ANNULD(FIMULTI,N)											!multi obj
	      NCONT=0															!multi obj
	      REWIND 302														!multi obj
		ENDIF																!multi obj
	  
	    IF(W1.NE.0) THEN													!multi obj
		  IF(ICOUT.EQ.1) THEN												!multi obj
              DO NEL=1,NETO													!multi obj
                NBRXI=NVAR(NEL)												!multi obj
			  CALL OBJCT1(NEL,NETO,N,NCONT,NXIT(1,NEL),NBRXI,				!multi obj	
     *                      OBJMULTI,FIMULTI,WIDTH,IPRINT,SPEC(NEL),			!multi obj
     *				      ITYPE(NEL),ISECT(NEL),CORRO)						!multi obj
	          NCONT=NCONT+NBRXI												!multi obj
	        ENDDO															!multi obj
		    FACT=FACT+(W1*OBJMULTI/FK(1))**RHO								!multi obj
              DO I=1,N														!multi obj
		      FI(I)=FI(I)+RHO*(W1*OBJMULTI/FK(1))**(RHO-1.)*			!multi obj
     *			       W1/FK(1)*FIMULTI(I)									!multi obj
	        ENDDO															!multi obj
	      ENDIF																!multi obj

	      IF(ICOUT.GT.1) THEN												!multi obj
	        CALL SENSIBCOUT(FIMULTI,NETO,OBJMULTI,NVAR,N,NXIT,SPEC,			!multi obj
     *                        WIDTH,ISECT,CORRO)								!multi obj
		    FACT=FACT+(W1*OBJMULTI/FK(1))**RHO								!multi obj
              DO I=1,N														!multi obj
		      FI(I)=FI(I)+RHO*(W1*OBJMULTI/FK(1))**(RHO-1.)*			!multi obj
     *			       W1/FK(1)*FIMULTI(I)									!multi obj
	        ENDDO															!multi obj
	      ENDIF																!multi obj
	    ENDIF																!multi obj

	    OBJ=FACT**(1./RHO)													!multi obj
	    DO I=1,N															!multi obj
	      FI(I)=(1./RHO)*FACT**((1.-RHO)/RHO)*FI(I)					!multi obj
	    ENDDO																!multi obj
	  
	  ENDIF																	!multi obj

	  GOTO 36																!multi obj

	ENDIF																	!multi obj
	
		
	IF(ICOUT.EQ.(-1)) THEN													!obj inertie
        CALL OBJIN1(NETO,N,NXIT,NVAR,OBJ,FI,ITYPE,PHILN,TETAS,Z,
     *			  DELT,YNEUTPART,SYM,SYMX,PART)								!r&d13
c	  GOTO 36																!multi obj
	ENDIF
     
	IF(ICOUT.EQ.0) THEN   ! Obj Poids										!multi obj
	  DO NEL=1,NETO
          NBRXI=NVAR(NEL)
          CALL OBJPD1(NEL,NETO,N,NCONT,NXIT(1,NEL),NBRXI,OBJ,FI,WIDTH,
     *                1,SPEC(NEL),ITYPE(NEL),ISECT(NEL),CORRO)				!février 2004 + !corrosion
	    NCONT=NCONT+NBRXI
	  ENDDO
	ENDIF

	IF(ICOUT.EQ.1) THEN   ! Obj Cout										!multi obj
	  DO NEL=1,NETO
          NBRXI=NVAR(NEL)
          CALL OBJCT1(NEL,NETO,N,NCONT,NXIT(1,NEL),NBRXI,OBJ,FI,WIDTH,		!restri cout
     *              IPRINT,SPEC(NEL),ITYPE(NEL),ISECT(NEL),CORRO)				!février 2004 + !corrosion
	    NCONT=NCONT+NBRXI
	  ENDDO
	ENDIF

      IF (ICOUT.GT.1) THEN                                            !newcost
	  CALL SENSIBCOUT(FI,NETO,OBJ,NVAR,N,NXIT,SPEC,WIDTH,ISECT,     !newcost	!18.03.04
     *                  CORRO )										!newcost    + !corrosion
	ENDIF                                                           !newcost

   36 IF(IPRINT.GE.1) WRITE(*,54) OBJ									!multi obj

	IF (IMPR.GE.0) THEN					!15.10.05
       WRITE (666,*)'Dérivées Fct Obj. N variables (Dér. non modifiées)' ! Juin 2003
       WRITE (666,16) (FI(I),I=1,N)
	ENDIF									!15.10.05

c	PAUSE'OPT5'
		
C 4.2 Restrictions géométriques
C     --------------------------
	DO 21 I=1,N
	DO 21 J=1,M
  21  CIJ(I,J)=0.  

      KCONT=0

	IF(IPRINT.GE.1) WRITE(*,*) 'Restrictions geometriques'		!03.02.06
	
	DO 10 NEL=1,NETO
	  MNEL=M2CONT(NEL)
c       IF((IPRINT.GE.1).AND.(MNEL.GE.1)) WRITE(*,55)NEL
      DO 12 J=1,MNEL
	  KCONTR=KCONT+J
	  READ(303) NCONT			!extension neto
	  READ(303) (CIJ(I+NCONT,KCONTR),I=1,NVAR(NEL)),	!extension neto
     *            CJ(KCONTR),CJM(KCONTR),VNOM2(KCONTR)
c       IF(IPRINT.GE.1) THEN
c       WRITE(*,*)'La restriction nø',KCONTR,':',VNOM2(KCONTR),
c    *            '        CJ(',KCONTR,') =',CJ(KCONTR)
c       WRITE(*,17)(CIJ(I,KCONTR),I=1,N)
c       ENDIF
   12 CONTINUE
c       IF((IOPTI.GE.3).AND.(MNEL.GE.1)) PAUSE 'pause'
        KCONT=KCONT+MNEL
   10 CONTINUE

C 4.3 Restrictions structurelles + restrictions particulières (Sagging & Hogging)
C     ===========================================================================

c 4.3.1 Restrictions sur Centre de gravite
C       ---------------------------------
c        IF(IPRINT.GE.1) WRITE(*,*) 'Restrictions geometriques'		!03.02.06
        IF(IGRAV.GE.1) THEN
           IF(IPRINT.GE.1) WRITE(*,*)'Restrictions centre de gravite'
           IG=1
	     IF(IGRAV==3) IG=2
           DO J=1,IG
             KCONT=KCONT+1
             READ(303) CJ(KCONT),CJM(KCONT),VNOM2(KCONT)	!extension neto
             READ(303) (CIJ(I,KCONT),I=1,N)				!extension neto
c            IF(IPRINT.GE.1) THEN
c            WRITE(*,*)'La restriction nø',KCONT,':',VNOM2(KCONT),
c     *              '       CJ(',KCONT,') =',CJ(KCONT)
c            WRITE(*,17)(CIJ(I,KCONT),I=1,N)
c            ENDIF
c            IF(IOPTI.GE.3) PAUSE 'pause'
           ENDDO
        ENDIF

c	  Restriction sur inertie							!restri inertie
c	  -----------------------
	  IF(INERT.NE.0) THEN
          IF(IPRINT.GE.1) WRITE(*,*)'Restrictions sur inertie'
          KCONT=KCONT+1
          READ(303) CJ(KCONT),CJM(KCONT),VNOM2(KCONT)
          READ(303) (CIJ(I,KCONT),I=1,N)
c         IF(IPRINT.GE.1) THEN
c         WRITE(*,*)'La restriction nø',KCONT,':',VNOM2(KCONT),
c     *           '       CJ(',KCONT,') =',CJ(KCONT)
c         WRITE(*,17)(CIJ(I,KCONT),I=1,N)
c         ENDIF
c         IF(IOPTI.GE.3) PAUSE 'pause'
        ENDIF												!restri inertie

c	  Restriction sur module							!restri module
c	  ----------------------
	  IF(IMOD.NE.0) THEN
          IF(IPRINT.GE.1) WRITE(*,*)'Restrictions sur module sectionnel'
          KCONT=KCONT+1
          READ(303) CJ(KCONT),CJM(KCONT),VNOM2(KCONT)
          READ(303) (CIJ(I,KCONT),I=1,N)
c         IF(IPRINT.GE.1) THEN
c         WRITE(*,*)'La restriction nø',KCONT,':',VNOM2(KCONT),
c     *           '       CJ(',KCONT,') =',CJ(KCONT)
c         WRITE(*,17)(CIJ(I,KCONT),I=1,N)
c         ENDIF
c         IF(IOPTI.GE.3) PAUSE 'pause'
        ENDIF												!restri module


c	  Restriction sur poids								!restri poids
c       ---------------------
	  IF(IWEIGHT.NE.0) THEN
          IF(IPRINT.GE.1) WRITE(*,*)'Restrictions sur poids'
          KCONT=KCONT+1
          READ(303) CJ(KCONT),CJM(KCONT),VNOM2(KCONT)
          READ(303) (CIJ(I,KCONT),I=1,N)
c         IF(IPRINT.GE.1) THEN
c         WRITE(*,*)'La restriction nø',KCONT,':',VNOM2(KCONT),
c     *           '       CJ(',KCONT,') =',CJ(KCONT)
c         WRITE(*,17)(CIJ(I,KCONT),I=1,N)
c         ENDIF
c         IF(IOPTI.GE.3) PAUSE 'pause'
        ENDIF												!restri poids


c	  Restriction sur cout								!restri cout
c       ---------------------
	  IF(IPRICE.NE.0) THEN
          IF(IPRINT.GE.1) WRITE(*,*)'Restrictions sur cout'
          KCONT=KCONT+1
          READ(303) CJ(KCONT),CJM(KCONT),VNOM2(KCONT)
          READ(303) (CIJ(I,KCONT),I=1,N)
c         IF(IPRINT.GE.1) THEN
c         WRITE(*,*)'La restriction nø',KCONT,':',VNOM2(KCONT),
c     *           '       CJ(',KCONT,') =',CJ(KCONT)
c         WRITE(*,17)(CIJ(I,KCONT),I=1,N)
c         ENDIF
c         IF(IOPTI.GE.3) PAUSE 'pause'
        ENDIF												!restri cout


c 4.3.2 Restrictions structurelles standards
c       -----------------------------------
      IF(IPRINT.GE.1) WRITE(*,*) 'Restrictions structurelles'
c	DO 70 IS=1,NSOL
	DO 71 NEL=1,NETO
	  MNEL=M1CONT(NEL)
c       IF((IPRINT.GE.1).AND.(MNEL.GE.1)) WRITE(*,57)NEL
      DO 72 J=1,MNEL
	  KCONTR=KCONT+J
	  READ(303) CJ(KCONTR),CJM(KCONTR),VNOM2(KCONTR)	!extension neto
	  READ(303) (CIJ(I,KCONTR),I=1,N)					!extension neto
c       IF(IPRINT.GE.1) THEN
c       WRITE(66,*)'La restriction ',KCONTR,':',VNOM2(KCONTR), 
c     *            '       CJ(',KCONTR,') =',CJ(KCONTR)
c       WRITE(66,17)(CIJ(I,KCONTR),I=1,N)
c	  ENDIF
   72 CONTINUE
c       IF(IOPTI.GE.3) PAUSE 'pause'
        KCONT=KCONT+MNEL
   71 CONTINUE
c  70 CONTINUE

c 4.3.2 Restrictions sur le MOMENT ULTIME (Poutre navire)
C       ---------------------------------------------------
        IF(IRESTR.GE.1) THEN
           IF(IPRINT.GE.1) WRITE(*,*)'restrictions moment ultime'
C          Restriction sur Ult (Sagging)
c          ............................
           KCONT=KCONT+1
           READ(303) CJ(KCONT),CJM(KCONT),VNOM2(KCONT)	!extension neto
           READ(303) (CIJ(I,KCONT),I=1,N)					!extension neto
c          IF(IPRINT.GE.1) THEN
c          WRITE(666,*)'SAGGING'
c          WRITE(666,*)'La restriction nø',KCONT,':',VNOM2(KCONT),
c     *               '       CJ(',KCONT,') =',CJ(KCONT)
c          WRITE(666,17)(CIJ(I,KCONT),I=1,N)
c	     ENDIF
C          Restriction sur Ult (Hogging)
c          ............................
	     KCONT=KCONT+1
	     READ(303) CJ(KCONT),CJM(KCONT),VNOM2(KCONT)	!extension neto
	     READ(303) (CIJ(I,KCONT),I=1,N)					!extension neto
c          IF(IPRINT.GE.1) THEN
c          WRITE(*,*)'La restriction nø',KCONT,':',VNOM2(KCONT),
c     *              '       CJ(',KCONT,') =',CJ(KCONT)
c          WRITE(*,17)(CIJ(I,KCONT),I=1,N)
c          ENDIF
c          IF(IOPTI.GE.3) PAUSE 'pause'
        ENDIF
	
C 4.4 Calcul des dérivées réduites pour les NR variables (var. indépendantes)
C     -----------------------------------------------------------------------
      IF(NEGALT.GT.0) THEN
	CALL ANNULD(Z1,N)
      DO 106 J=1,N
        DO 107 I=1,N
        Z1(J)=Z1(J)+FI(I)*DX(I,J)
  107   CONTINUE
  106 CONTINUE
      DO 108 J=1,N
  108 FI(J)=Z1(J)

      CALL RED1D(FI,FI,N,NXI2,MEGA,NEGALT,NETO,NGmax)

c      IF(IPRINT.GE.1) THEN
c      WRITE (666,*)'Dérivées Fct Obj. NR variables (Dér. mofifiées)'
c      WRITE (666,16) (FI(I),I=1,NR)
c	 ENDIF

      DO 113 K=1,M
	  CALL ANNULD(Z1,N)

        DO 110 J=1,N
          DO 111 I=1,N
            Z1(J)=Z1(J)+CIJ(I,K)*DX(I,J)
  111     CONTINUE
  110   CONTINUE  

        CALL RED1D(Z1,Z1,N,NXI2,MEGA,NEGALT,NETO,NGmax)

        DO 112 J=1,NR
  112   CIJ(J,K)=Z1(J)
c       WRITE (666,17) (CIJ(J,K),J=1,NR)
  113   CONTINUE
      ENDIF
	
C 4.5 Sauvetage pour Conlin (Fct objectif)
C     ------------------------------------     !
      WRITE (IUIN,*) JOBJ                      ! Type Fct Object
      WRITE (IUIN,'(9E14.7)') OBJ              ! Fct Obj
      WRITE (IUIN,'(9E14.7)') (FI(I),I=1,NR)   ! Dérivées

c      WRITE (666,'(A8,9E11.4)') 'OBJ= ',OBJ								 
c      WRITE (666,*) 'FI= (dans Subr OPTI)'								  
c      WRITE (666,'(9E11.4)') (FI(I),I=1,NR)							


C 4.6 Modifications de bornes de restrictions
C     ----------------------------------------
C !!!  CETTE FONCTIONALITE A ETE SUPPRIMEE LE 31 JANVIER 2002

c   6  CONTINUE
c 	IF(IOPTI.LE.3) GOTO 41
c
c      WRITE(*,'(A/16(1H=))') 'LES RESTRICTIONS'
c      KCONT=0                                       ! compteur restrictions
c
c      IF(IPRINT.GE.1) WRITE(*,*)  'Restrictions géométriques'
c	DO 15 NEL=1,NETO
c	  MNEL=M2CONT(NEL)
c        IF(MNEL.GE.1) WRITE(*,*)'Panneau nø',NEL
c        DO 14 J=1,MNEL
c	    KCONTR=KCONT+J
c          WRITE (*,*) 'C(',KCONTR,')=',CJ(KCONTR),'<',CJM(KCONTR),
c     *                ': Type = ',VNOM2(KCONTR)
c  14    CONTINUE
c      KCONT=KCONT+MNEL
c  15  CONTINUE
c
c        IF(IPRINT.GE.1) WRITE(*,*)  'Restrictions structurelles'
c	DO 4 IS=1,NSOL
c         WRITE(*,*)  'Cas de charge nø',IS
c	 DO 5 NEL=1,NETO
c	   MNEL=M1CONT(NEL)
c         IF(MNEL.GE.1) WRITE(*,*)'Panneau nø',NEL
c         DO 13 J=1,MNEL
c	     KCONTR=KCONT+J
c           WRITE (*,*) 'C(',KCONTR,')=',CJ(KCONTR),'<',CJM(KCONTR),
c     *                 ': Type = ',VNOM2(KCONTR)
c  13     CONTINUE
c         KCONT=KCONT+MNEL
c   5   CONTINUE
c	 IF(IS.EQ.NSOL.AND.IRESTR.GE.1) THEN
c           WRITE(*,*)  'Restrictions particulières'
C        Restriction sur Ult(sagging)
c	   KCONT=KCONT+1
c         WRITE (*,*) 'C(',KCONT,')=',CJ(KCONT),'<',CJM(KCONT),
c     *               ': Type = ',VNOM2(KCONT)
C        Restriction sur Ult(sagging)
c	   KCONT=KCONT+1
c         WRITE (*,*) 'C(',KCONT,')=',CJ(KCONT),'<',CJM(KCONT),
c     *               ': Type = ',VNOM2(KCONT)
c       ENDIF
c   4  CONTINUE
c
c	IF(IOPTI.EQ.1) GOTO 41
c      J=0
c      WRITE (*,*)' VOULEZ VOUS MODIFIEZ LA BORNE D''UNE RESTRICTION '
c      WRITE (*,*)' si NON : taper 0 ( par défaut)'
c      WRITE (*,*)' si OUI : taper le nø de la restriction'
c        READ(*,*) J
c	IF(J.GT.0) THEN
c	   IF(J.GT.M) THEN
c        WRITE (*,*)' Error: J=',J,' > nbre de restrictions =',M
c		PAUSE 'pause'
c		GOTO 6
c	   ENDIF
c	   IF(J.LE.M2.OR.J.GT.(M-2)) THEN
c        WRITE (*,*)' Il s''agit d''une restriction géométrique'
c        WRITE (*,*)'   ou relative à la résistance ult. du bateau'
c        WRITE (*,*)' Cette borne ne peut pas être modifiée !!'
c		PAUSE 'pause'
c		GOTO 6
c	   ENDIF
c	   J1=J
c	   IF(J.GT.(M1+M2)) THEN
C          Recherche du Nø de la restriction pour le cas de charge nø 1		
c   45 	 J1=J1-M1
c		 IF(J1.GT.(M1+M2)) GOTO 45
c	   ENDIF
c         WRITE (*,*)' Actuellement cette borne est de ',CJM(J1)
c         WRITE (*,*)' Quelle doit être sa nouvelle borne ?'
c         READ(*,*)  CJM(J1)
c	   CJMAX(J1-M2)=CJM(J1)*INV(J1-M2)		
c
C        Modification de la borne pour les autres cas de charges		
c	   IF(NSOL.GT.1) THEN
c   46 	 CJM(J1+M1)=CJM(J1)
c		 J1=J1+M1
c		 IF(J1.LT.J) GOTO 46
c	   ENDIF
c         WRITE (*,*) ' Pour chaque cas de charge,',
c     *               ' la nouvelle borne est ',CJM(J)
c         WRITE(666,*)' !!! Mofification d''une borne de restr. !!!' 
c         WRITE(666,*)' La nouvelle borne est de C(',J,')=',CJM(J)
c         PAUSE 'pause'
c	GOTO 6
c	ENDIF
c  41  CONTINUE
   
C 4.7 Sauvetage pour Conlin (Restrictions)
C     ------------------------------------
      TEMP =0.0

      DO 7 J=1,M
        WRITE (IUIN,*) JT
        WRITE (IUIN,'(9E14.7)') CJ(J),CJM(J),TEMP,TEMP
        WRITE (IUIN,'(9E14.7)') (CIJ(I,J),I=1,NR)
c       WRITE (66,*) ' Ds OPTI : impression des CIJ (avt CONLIN)'   !degugging
c       WRITE (66,'(9(E10.3,1X))') (CIJ(I,J),I=1,NR)
   7  CONTINUE
   
      REWIND(IUIN)

   
C 4.8 Impressions des données (XI,XIMAX et XIMIN) avant CONLIN
C     --------------------------------------------------------
C     Utilisation des vecteurs temporaires Z1=XI,Z2=XIMIN et Z3=XIMAX

      WRITE(*,*)" Module d'optimisation (CONLIN), Itération nø",ITERA
      WRITE(*,*)' ++++++++++++++++++++++++++++++++++++++++++++++++++++'

      WRITE(666,50)ITERA

      IF(ITERA.EQ.1) THEN
      DO 913 I=1,NR
        Z1(I)=XI(I)
	  Z2(I)=XIMIN(I)
	  Z3(I)=XIMAX(I)
  913 CONTINUE
      IF(NEGALT.GT.0) THEN
      DO 914 I=1,NEGALT
	  J=NXI2(MEGA(I,1),MEGA(I,2))
        DO 915 K=1,N-J
          Z2(N-K+1)=Z2(N-K)
          Z3(N-K+1)=Z3(N-K)
  915     Z1(N-K+1)=Z1(N-K)
  914 CONTINUE
  
      DO 916 I=1,NEGALT
	  J =NXI2(MEGA(I,1),MEGA(I,2))
        JJ=NXI2(MEGA(I,3),MEGA(I,4))
	  IF(MEGA(I,1).EQ.9) THEN									!janvier 2005
	    PHILJ=PHILN(MEGA(I,2))
	    PHILJJ=PHILN(MEGA(I,4))
	    QJ=QN(MEGA(I,2))
	    QJJ=QN(MEGA(I,4))
	    WJ=DABS(PHILJ)*QJ*PI1
	    WJJ=DABS(PHILJJ)*QJJ*PI1
	    MODE=MODES(MEGA(I,2))
		IF(MODE.EQ.'EE1'.OR.MODE.EQ.'ES4') THEN
            XmodeJ=-1.0
          ELSEIF(MODE.EQ.'ES1'.OR.MODE.EQ.'ES3'.OR.
     *      MODE.EQ.'EC1'.OR.MODE.EQ.'EL3')  THEN
            XmodeJ=-0.5
          ELSEIF(MODE.EQ.'SL1'.OR.MODE.EQ.'SL2'.OR.MODE.EQ.'EL2') THEN
            XmodeJ=+0.5
          ELSEIF(MODE.EQ.'LL1') THEN
            XmodeJ=+1.0
          ELSE
            XmodeJ=0.0
          ENDIF
	    MODE=MODES(MEGA(I,4))
		IF(MODE.EQ.'EE1'.OR.MODE.EQ.'ES4') THEN
            XmodeJJ=-1.0
          ELSEIF(MODE.EQ.'ES1'.OR.MODE.EQ.'ES3'.OR.
     *      MODE.EQ.'EC1'.OR.MODE.EQ.'EL3')  THEN
            XmodeJJ=-0.5
          ELSEIF(MODE.EQ.'SL1'.OR.MODE.EQ.'SL2'.OR.MODE.EQ.'EL2') THEN
            XmodeJJ=+0.5
          ELSEIF(MODE.EQ.'LL1') THEN
            XmodeJJ=+1.0
          ELSE
            XmodeJJ=0.0
          ENDIF
	    Z1(J)=1./(1./(EGA(I)*Z1(JJ))-XmodeJJ/(EGA(I)*WJJ)+XmodeJ/WJ)
		Z2(J)=1./(1./(EGA(I)*Z2(JJ))-XmodeJJ/(EGA(I)*WJJ)+XmodeJ/WJ)
	    Z3(J)=1./(1./(EGA(I)*Z3(JJ))-XmodeJJ/(EGA(I)*WJJ)+XmodeJ/WJ)	!janvier 2005
	  ELSE
	    Z1(J)=Z1(JJ)*EGA(I)
	    Z2(J)=Z2(JJ)*EGA(I)										!janvier 2005
	    Z3(J)=Z3(JJ)*EGA(I)										!janvier 2005
	  ENDIF
  916 CONTINUE
      ENDIF

      K=0
      DO 414 NEL=1,NETO
        IF(NVAR(NEL).NE.0) THEN
          K2=K+NVAR(NEL)
          WRITE (666,132) NEL,(VNOM(NXIT(I,NEL)),I=1,NVAR(NEL))
          WRITE (666,133) (Z2(I),I=K+1,K2)
          WRITE (666,131) (Z1(I),I=K+1,K2)
          WRITE (666,135) (Z3(I),I=K+1,K2)
          K=K2
        ENDIF
  414 CONTINUE
      ENDIF

C  ====================================================================
C 5.0 CONLIN (S vecteur de travail et MS taille max. de ce vecteur)
C  ====================================================================

      CALL CONLIN (S,MS,IUIN,IOP,NON,NETO,VNOM2,NVARR,NXITR,M1TABL,  !octobre2003	!dcn
     *             M2CONT,IG,IRESTR,NSOL,IWEIGHT,IPRICE,INERT,IMOD)  !fev 2004	!restri poids	 !restri cout   !restri inertie   !restri module
C     ++++++++++++++++++++++++++++++++++++++++++++

      IF(NON.GE.899) THEN
	WRITE(666,*)' ERROR dans CONLIN (NON=',NON,')'
      WRITE(29,*)  ' ERROR dans CONLIN (NON=',NON,')'	!bug
      IF(NON.GE.900) STOP
C     Si NON=899, on continue !
      ENDIF
	
C  ====================================================================
C 6.0 Estimation des restrictions par CONLIN sur base des dérivées
C  ====================================================================
	DO 223 J=1,M
	 TPY=0.
c      WRITE(666,*)' DCIJ=',(CIJ(I,J),I=1,NR)              ! Facultatif
	 DO 224 I=1,NR
c        TTD=(S(I)-XI(I))                                  ! Facultatif
c        TTT=TTD*CIJ(I,J)                                  ! Facultatif
c        WRITE(666,*)'S(I)-XI(I)=',TTD,' Dcij=',CIJ(I,J),  ! Facultatif
c     *              ' variation=',TTT
         TPY= TPY + CIJ(I,J)*(S(I)-XI(I))
  224  CONTINUE
c      WRITE(666,*)'Valeur de  C(J)  avant l''itération    =',CJ(J) ! Facultatif
c      WRITE(666,*)'Estimation de la variation de C(',J,') =',TPY   ! Facultatif
       TPY=CJ(J)+TPY
       IF(IOPTI.GE.2) WRITE(666,*)'Estimation de C(',J,') =',TPY    ! Facultatif
c      WRITE(666,*)'Estimation de C(',J,') =',TPY                   ! Facultatif
c      WRITE(666,*)
  223 CONTINUE

C 7.0 VECTEUR SOLUTION XI pour les N var. de Conception (XI1 et XI2 ensemble)
C     et reconstruction des vecteurs complets XIMIN et XIMAX (N variables)
C     ---------------------------------------------------------------------
      IF(NEGALT.GT.0) THEN
      DO 122 I=1,NEGALT
	  J=NXI2(MEGA(I,1),MEGA(I,2))
        DO 120 K=1,N-J
          XIMIN(N-K+1)=XIMIN(N-K)
          XIMAX(N-K+1)=XIMAX(N-K)
          S(N-K+1)    =S(N-K)
  120   CONTINUE
  122 CONTINUE
  
      DO 121 I=1,NEGALT
	  J =NXI2(MEGA(I,1),MEGA(I,2))
        JJ=NXI2(MEGA(I,3),MEGA(I,4))
	  IF(MEGA(I,1).EQ.9) THEN									!janvier 2005
	    PHILJ=PHILN(MEGA(I,2))
	    PHILJJ=PHILN(MEGA(I,4))
	    QJ=QN(MEGA(I,2))
	    QJJ=QN(MEGA(I,4))
	    WJ=DABS(PHILJ)*QJ*PI1
	    WJJ=DABS(PHILJJ)*QJJ*PI1
	    MODE=MODES(MEGA(I,2))
		IF(MODE.EQ.'EE1'.OR.MODE.EQ.'ES4') THEN
            XmodeJ=-1.0
          ELSEIF(MODE.EQ.'ES1'.OR.MODE.EQ.'ES3'.OR.
     *      MODE.EQ.'EC1'.OR.MODE.EQ.'EL3')  THEN
            XmodeJ=-0.5
          ELSEIF(MODE.EQ.'SL1'.OR.MODE.EQ.'SL2'.OR.MODE.EQ.'EL2') THEN
            XmodeJ=+0.5
          ELSEIF(MODE.EQ.'LL1') THEN
            XmodeJ=+1.0
          ELSE
            XmodeJ=0.0
          ENDIF
	    MODE=MODES(MEGA(I,4))
		IF(MODE.EQ.'EE1'.OR.MODE.EQ.'ES4') THEN
            XmodeJJ=-1.0
          ELSEIF(MODE.EQ.'ES1'.OR.MODE.EQ.'ES3'.OR.
     *      MODE.EQ.'EC1'.OR.MODE.EQ.'EL3')  THEN
            XmodeJJ=-0.5
          ELSEIF(MODE.EQ.'SL1'.OR.MODE.EQ.'SL2'.OR.MODE.EQ.'EL2') THEN
            XmodeJJ=+0.5
          ELSEIF(MODE.EQ.'LL1') THEN
            XmodeJJ=+1.0
          ELSE
            XmodeJJ=0.0
          ENDIF
	    S(J)=1./(1./(EGA(I)*S(JJ))-XmodeJJ/(EGA(I)*WJJ)+XmodeJ/WJ)
		XIMIN(J)=1./(1./(EGA(I)*XIMIN(JJ))-XmodeJJ/(EGA(I)*WJJ)+
     *    XmodeJ/WJ)
	    XIMAX(J)=1./(1./(EGA(I)*XIMAX(JJ))-XmodeJJ/(EGA(I)*WJJ)+
     *    XmodeJ/WJ)														!janvier 2005
	  ELSE
	    S(J)    =S(JJ)*EGA(I)
	    XIMIN(J)=XIMIN(JJ)*EGA(I)
	    XIMAX(J)=XIMAX(JJ)*EGA(I)
	  ENDIF
  121 CONTINUE
      ENDIF

C 7.1 Impressions
C     -----------
      WRITE(*,59)
      WRITE(666,59)
      K=0

      WRITE(*,'(A,5x,9(1X,A8,3X))') ' PANEL',VNOM
      DO 130 NEL=1,NETO
      IF(NVAR(NEL).NE.0) THEN
        K2=K+NVAR(NEL)
c       WRITE (*,32)NEL,(VNOM(NXIT(I,NEL)),I=1,NVAR(NEL))     ! Noms des XI
c       WRITE (*,34) (S(I),I=K+1,K2)                          ! Var. Concept.
        WRITE (*,'(3x,I3,5x,9(E11.4,1X))') NEL,(S(I),I=K+1,K2)
        WRITE (666,132)NEL,(VNOM(NXIT(I,NEL)),I=1,NVAR(NEL))  ! Noms des XI
        WRITE (666,131)(S(I),I=K+1,K2)						! Var. Concept.
	  K=K2
      ENDIF
  130 CONTINUE

C 7.2 Sauvetage de la solution SI(I) (variables de conception)
C     ---------------------------------------------------------
c      REWIND IUOUT
      DO 56 I=1,N
   56 XICOU(I)=S(I)

C  ====================================================================
C 8.0 Calcul de la fct objectif avec les nouveaux XI c.à.d. S(I).
C     ------------------------------------------------------------	
      OBJ2=0.
	OBJ2MULTI=0.	!multi obj
	FACT=0.			!multi obj
c     REWIND(IUOUT)
      REWIND(302)		!extension neto
	NC=1
      
      IF(ITERA.EQ.ITERAM) THEN
	 ITT=1
	 WRITE(666,63)
	ELSE
	 ITT=0
	ENDIF
   63 FORMAT(/' RESULTAT FINAL (GROSS THICKNESSES) :'/20(1H*)) 


	IF(IMULTI.EQ.1) THEN													!multi obj
	  
	  IF(IICOUT.EQ.(-1)) THEN												!multi obj
	    CALL OBJIN2(NETO,OBJ2,S,NVAR,NXIT,DELT,PHILN,TETAS,Z,				!multi obj
     *                ITT,SYMX,SYM,YNEUTPART,PART)							!multi obj
        ENDIF																	!multi obj

	  IF(IICOUT.EQ.0) THEN													!multi obj
	    DO NEL=1,NETO														!multi obj
		  NBRXI=NVAR(NEL)													!multi obj
		  CALL OBJPD2(N,S(NC),NXIT(1,NEL),NBRXI,OBJ2,WIDTH,IPRINT,NEL,		!multi obj
     *                 SPEC(NEL),ITT,ITYPE(NEL),ISECT(NEL),CORRO(NEL,:))		!multi obj
	      NC=NC+NBRXI														!multi obj
		ENDDO
        ENDIF																	!multi obj

	  IF(IICOUT.EQ.1) THEN													!multi obj
	    DO NEL=1,NETO														!multi obj
		  NBRXI=NVAR(NEL)													!multi obj
	      CALL OBJCT2(N,S(NC),NXIT(1,NEL),NBRXI,OBJ2,WIDTH,IPRINT,NEL,		!multi obj
     *                 SPEC(NEL),ITT,ITYPE(NEL),ISECT(NEL),CORRO(NEL,:))		!multi obj
	      NC=NC+NBRXI														!multi obj
		ENDDO																!multi obj
        ENDIF																	!multi obj
	  
	  IF(IICOUT.GT.1) THEN													!multi obj
	    CALL SENSIBCOUT2(NETO,OBJ2,NVAR,S,NXIT,SPEC,ITT,WIDTH,ISECT,		!multi obj
     *                     CORRO )											!multi obj
	  ENDIF																	!multi obj
	  
	  
	  IF(IICOUT.EQ.(-2)) THEN												!multi obj
        
 	    IF(W3.NE.0) THEN													!multi obj
		  CALL OBJIN2(NETO,OBJ2MULTI,S,NVAR,NXIT,DELT,PHILN,TETAS,Z,		!multi obj
     *                  ITT,SYMX,SYM,YNEUTPART,PART)							!multi obj
	      FACT=FACT+(W3*FK(3)/OBJ2MULTI)**RHO								!multi obj
	      WRITE (666,64) OBJ2MULTI											!multi obj
	      OBJ2MULTI=0.														!multi obj
		  REWIND 302														!multi obj
		ENDIF																!multi obj
  
	    IF(W2.NE.0) THEN													!multi obj
	      DO NEL=1,NETO														!multi obj
		    NBRXI=NVAR(NEL)													!multi obj
		    CALL OBJPD2(N,S(NC),NXIT(1,NEL),NBRXI,OBJ2MULTI,WIDTH,			!multi obj
     *                    IPRINT,NEL,SPEC(NEL),ITT,ITYPE(NEL),				!multi obj
     *					ISECT(NEL),CORRO(NEL,:))							!multi obj	
     	        NC=NC+NBRXI														!multi obj
		  ENDDO																!multi obj
	      FACT=FACT+(W2*OBJ2MULTI/FK(2))**RHO								!multi obj
	      WRITE (666,62) OBJ2MULTI											!multi obj
		  OBJ2MULTI=0.														!multi obj
		  NC=1																!multi obj
		  REWIND 302														!multi obj
		ENDIF																!multi obj
	  
	    IF(W1.NE.0) THEN													!multi obj
		  IF(ICOUT.EQ.1) THEN												!multi obj
	        DO NEL=1,NETO													!multi obj
		      NBRXI=NVAR(NEL)												!multi obj
                CALL OBJCT2(N,S(NC),NXIT(1,NEL),NBRXI,OBJ2MULTI,WIDTH,		!multi obj
     *                      IPRINT,NEL,SPEC(NEL),ITT,ITYPE(NEL),				!multi obj
     *					  ISECT(NEL),CORRO(NEL,:))							!multi obj
     	          NC=NC+NBRXI													!multi obj
		    ENDDO															!multi obj
		    FACT=FACT+(W1*OBJ2MULTI/FK(1))**RHO								!multi obj
			WRITE (666,60) OBJ2MULTI										!multi obj
	      ENDIF																!multi obj

	      IF(ICOUT.GT.1) THEN												!multi obj
	        CALL SENSIBCOUT2(NETO,OBJ2MULTI,NVAR,S,NXIT,SPEC,ITT,			!multi obj
     *                         WIDTH,ISECT,CORRO)								!multi obj
              FACT=FACT+(W1*OBJ2MULTI/FK(1))**RHO								!multi obj
			WRITE (666,60) OBJ2MULTI										!multi obj
	      ENDIF																!multi obj
	    ENDIF																!multi obj

	    OBJ2=FACT**(1./RHO)													!multi obj

	  ENDIF																	!multi obj

	  GOTO 31																!multi obj

	ENDIF																	!multi obj


	IF(ICOUT.EQ.(-1)) THEN	!Inertie
        CALL OBJIN2(NETO,OBJ2,S,NVAR,NXIT,DELT,PHILN,TETAS,Z,
     *              ITT,SYMX,SYM,YNEUTPART,PART)						!obj inertie	!r&d13
c	  GOTO 31														!multi obj
	ENDIF
     
	IF(ICOUT.EQ.0) THEN  !Poids												!multi obj
        DO NEL=1,NETO
          NBRXI=NVAR(NEL)
		CALL OBJPD2(N,S(NC),NXIT(1,NEL),NBRXI,OBJ2,WIDTH,IPRINT,NEL,
     *                SPEC(NEL),ITT,ITYPE(NEL),ISECT(NEL),CORRO(NEL,:))		!19.05.04  + !corrosion
		NC=NC+NBRXI
	  ENDDO
	ENDIF

      IF(ICOUT.EQ.1) THEN  !Cout												!multi obj
        DO NEL=1,NETO
          NBRXI=NVAR(NEL)
		CALL OBJCT2(N,S(NC),NXIT(1,NEL),NBRXI,OBJ2,WIDTH,IPRINT,NEL,
     *              SPEC(NEL),ITT,ITYPE(NEL),ISECT(NEL),CORRO(NEL,:))	!19.05.04  + !corrosion			!19.05.04
		NC=NC+NBRXI
	  ENDDO
	ENDIF

      IF (ICOUT.GT.1) THEN                                           !newcost
	  CALL SENSIBCOUT2(NETO,OBJ2,NVAR,S,NXIT,SPEC,ITT,WIDTH,ISECT, !newcost		!18.03.04
     *                     CORRO )								   !newcost    + !corrosion
	ENDIF                                                          !newcost

   31 IF (IMULTI.EQ.1) THEN				!multi obj
	  IF(IICOUT.GE.1) THEN				!multi obj
	    WRITE (666,60) OBJ2				!multi obj
          WRITE (67,160) OBJ2				!multi obj
          WRITE (66,160) OBJ2				!multi obj
          WRITE (*,  60) OBJ2				!multi obj
        ENDIF								!multi obj
	  IF(IICOUT.EQ.0) THEN				!multi obj
          WRITE (666,62) OBJ2				!multi obj
          WRITE (67,162) OBJ2				!multi obj
          WRITE (*,  62) OBJ2				!multi obj
          WRITE (66,162) OBJ2				!multi obj
	  ENDIF								!multi obj
	  IF(IICOUT.EQ.(-1)) THEN			!multi obj
          WRITE (666,64) OBJ2				!multi obj
          WRITE (67,164) OBJ2				!multi obj
          WRITE (*,  64) OBJ2				!multi obj
          WRITE (66,164) OBJ2				!multi obj
	  ENDIF								!multi obj
	  IF(IICOUT.EQ.(-2)) THEN			!multi obj
          WRITE (666,65) OBJ2				!multi obj
          WRITE (67,165) OBJ2				!multi obj
          WRITE (*,  65) OBJ2				!multi obj
          WRITE (66,165) OBJ2				!multi obj
	  ENDIF								!multi obj
	ELSE								!multi obj
	  IF(ICOUT.GE.1) THEN  ! cost
	    WRITE (666,60) OBJ2
          WRITE (67,160) OBJ2					!15.10.05
          WRITE (66,160) OBJ2					!15.10.05
          WRITE (*,  60) OBJ2
        ENDIF
	  IF(ICOUT.EQ.0) THEN
          WRITE (666,62) OBJ2
          WRITE (67,162) OBJ2					!15.10.05
          WRITE (*,  62) OBJ2
          WRITE (66,162) OBJ2					!15.10.05
	  ENDIF
	  IF(ICOUT.EQ.(-1)) THEN					!obj inertie
          WRITE (666,64) OBJ2
          WRITE (67,164) OBJ2					
          WRITE (*,  64) OBJ2
          WRITE (66,164) OBJ2					
	  ENDIF
	ENDIF								!multi obj

C  ====================================================================

	RETURN

  900 WRITE(29,*)' EOF lors de la lecture de CONLIN.DAT (Subr OPTI)'	!bug
      PAUSE 'STOP'
      STOP
C  ====================================================================
C 9.0 LES FORMATS
C     ************
   16 FORMAT('d Fct /dx =  ',(T14,9(E10.3,1X)))
   17 FORMAT('d C(j)/dx =  ',(T14,9(E10.3,1X)))
   18 FORMAT(9(E10.3))
   20 FORMAT(///' ENTREE DANS LE MODULE OPTIMISATION (CONLIN-LBR5)'/
     *        50(1H*)//
     *       ' CONLIN Optimizer                      ',
     *       ' Logiciel des bordages raidis'/
     *       ' Version 2.0                            ',
     *       ' Version : L.B.R.- 5.0'/
     *       ' Author: Prof. C. Fleury                ',
     *       ' Author: Dr. Ph. Rigo'/)
   32 FORMAT('PANEL nø ',I3,9(1X,A8,3X))
   33 FORMAT('Bornes Min:',9(E11.4,1X))
   34 FORMAT('Variables_:',9(E11.4,1X))
   35 FORMAT('Bornes Max:',9(E11.4,1X))
   50 FORMAT(/T30,'ITERATION nø ',I3/T30,18(1H*))
   51 FORMAT('Initialisation des données dans OPTI'/40(1H+)) 
   52 FORMAT('LECTURE des anciennes données'/40(1H=))
   53 FORMAT('LES NOUVELLES DONNEES'/25(1H-))
   54 FORMAT('Fct. OBJECTIF = ',E14.7/)
   55 FORMAT('RESTRICTIONS  GEOMETRIQUES : Panneau nø',I3/50(1H-))
   57 FORMAT('RESTRICTIONS  STRUCTURELLES: Panneau nø',I3/50(1H-))
   58 FORMAT('RESTRICTIONS  D''ENSEMBLE (Hogg & Sagg)'/50(1H-))
   59 FORMAT(/' LES RESULTATS (toutes les N variables) :'/20(1H-)) 
   60 FORMAT(/'Fct objectif cout(recalculée)    = ',E14.7, 'Euro or $'/)
   62 FORMAT(/'Fct objectif poids (recalculée)    = ',E14.7, 'N'/)
   64 FORMAT(/'Fct objectif inertie (recalculée)    = ',E14.7, 'm**4'/)		!obj inertie
   65 FORMAT(/'Fct objectif combinée (recalculée)    = ',E14.7/)				!multi obj
   61 FORMAT('RESTRICTIONS  SUR LE CENTRE DE GRAVITE '/50(1H-))
  131 FORMAT('Variables_:',9(E11.4))
  132 FORMAT('PANEL nø',I2,2X,9(1X,A8,2X))
  133 FORMAT('Bornes Min:',9(E11.4))
  135 FORMAT('Bornes Max:',9(E11.4))
  160 FORMAT(/'Fct objectif cout(après optimisation)= ',E14.7, 'Euro'/)
  162 FORMAT(/'Fct objectif poids (après optimisation)= ',E14.7, 'N'/)
  164 FORMAT(/'Fct objectif inertie (après optimisation)= ',
     *         E14.7, 'm**4'/)												!obj inertie
  165 FORMAT(/'Fct objectif combinée (après optimisation)= ',E14.7/)			!multi obj
      END
	
C     *****************************************************************
C     *****************************************************************
C     *****************************************************************

      SUBROUTINE RED1D(X,XR,ND,NXI2,MEGA,NEGALT,NETO,NGmax)
C     ==================
C
C     Modif: 5-9-95                                   ! Créer : 5-9-95
C
C     *****************************************************************

      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION MEGA(NGmax,4),NXI2(9,NETO),X(ND),XR(ND)

      DO 3 I=1,ND
    3 XR(I)=X(I)
C
      DO 1 I=1,NEGALT
        J=NXI2(MEGA(I,1),MEGA(I,2))
        JJ=J-I+1
        DO 2 K=1,ND-JJ
          JK=JJ+K-1
          XR(JK)=XR(JK+1)
   2  	CONTINUE
        XR(ND)=0.E+00
   1  CONTINUE

      RETURN
      END
