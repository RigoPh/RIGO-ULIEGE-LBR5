      SUBROUTINE PAIK(IPRINT,NETO,NEL,E,ETA,SIGY,WIDTH,LAMB,Q,DELTA,
     *                PHIL,EPSA,HXR,DXR,WXR,TXR,EPSR,ENTR,KSR,
     *                NVAR,NXIT,
     *                DSPAIK,SPAIK,OMT)

      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 LAMB,LAMB2,IS

      DIMENSION NVAR(NETO),NXIT(9,NETO)
      DIMENSION DSPAIK(9)

      COMMON /PY/ PI
      COMMON /OPTI/IOPTI,NTOT
    
C*******************************************************************************
C     SUBROUTINE PAIK 
C     ===================
C     Cette subrourine calcule la résistance ultime d'une plaque raidie
C     londitudinalement et comprimée, également, longitudinalement.
C     (La pression latéral n'est pas prise en compte)
C
C     Il s'agit de la formule empirique de J. PAIK.  qui est basée 
C     sur le principe de la poutre colonne.
C                                                                       
C   INPUT:E,ETA,SIGY,WIDTH,LAMB,Q,DELTA,PHIL 
C         EPSA  et  HXR,DXR,WXR,TXR,EPSR,ENTR,KSR
C         avec ENTR : entredistance réelle entre raidisseurs = D(raid)
C              EPSR : Largeur effective associée à 1 raid (= L/nbre de raid)
C                                                                       
C  OUTPUT: DCPAIK,SPAIK,OMT 
C	OMT = section raidiseur + bordage (complet)
C	OM  = Section du raid
C     SPAIK = résistance ultime selon la formule de J. PAIK
C     DCPAIK = dérivée de la résistance ultime SPAIK par rapport aux XI
C
C     Créer : le 22-7-96  par  Ph. Rigo
C
C     Modifications : 
C       - Formulations de PAIK + Sensibilités (pour subr CSENS4) ! 22-7-96
C       - limite sur BETA et SLEN                                ! 1-8-96
C       - Prise en Compte des _ entre D(raid) et de EPSR         ! 6-3-97
C          d(C)/d(EPSR)= d(C)/d(D)  *  (D/EPSR)**2
C                               
C     Dernière modif : 15-07-1997 				     	
C                    :  5-03-2001				     	
C                                                                       
C*************************************************************************
C*************************************************************************
C*************************************************************************
C               ^ Y et Sigma Y
C               I
C               *****************************
C               *                           *  b=EPSR (pour LBR4)
C               *                           *  b=ENTR = D (pour PAIK)
C               *                           *
C               ***************************** --------> X et Sigma X
c               <----------- a = EPSA ------>
c
c      EPSA  	= a        (m)
c      ENTR,EPSR	= b        (m)
c      SIGY		= SIG Elastique (N/m2), (ex, Re=240 N/mm2)
  
C 1.0 Calcul de SPAIK de référence
C     ****************************

C 1.1 Caractéristiques géométriques et calcul de á et Lamda  
C     -----------------------------------------------------
      TMRX=WXR*TXR                              ! section semelle
      OM  =TMRX+DXR*HXR                         ! section raid
      OMT =OM+ENTR*DELTA                        ! section raid + D(raid)
      SMCR=DELTA/2.+HXR+TXR/2.                                   	
      SMDR=DELTA/2.+HXR/2.                                   		
      TPBR=TMRX*SMCR                                				
      HX=TPBR+DXR*HXR*SMDR                       ! Moment statique (Z=0)
	
c	WRITE(666,*)'ds subr PAIK, NEL=',NEL
c	WRITE(666,*)'-----------------'
c	WRITE(666,*)'E,ETA,SIGY,WIDTH,LAMB,Q,DELTA = '
c	WRITE(666,*) E,ETA,SIGY,WIDTH,LAMB,Q,DELTA
c	WRITE(666,*)'HXR,DXR,WXR,TXR,EPSR,ENTR ='
c	WRITE(666,*) HXR,DXR,WXR,TXR,EPSR,ENTR
c	WRITE(666,*)'OM ,OMT,HX ,SMCR,SMDR= ',OM,OMT,HX,SMCR,SMDR

	GX=2.*HX		
      YG=GX/(2.*OMT)                                      ! YG=position du centre de gravité.
        IS=(ENTR*DELTA**3 + DXR*HXR**3 + WXR*TXR**3)/12.  ! IS=Inertie par rapport au centre de gravité
     *    + DELTA*ENTR*YG*YG  +  HXR*DXR*(SMDR-YG)**2     !    au centre de gravité.
     *                        +  WXR*TXR*(SMCR-YG)**2
      ROT=DSQRT(SIGY/E)
      BETA=ENTR * ROT / DELTA
      SLEN=EPSA * ROT* DSQRT(OMT/IS) / PI

c	WRITE(666,*)'YG,IS = ',YG,IS
c	WRITE(666,*)' BETA = ',BETA,' et SLEN(non modif) = ',SLEN

C 1.2 TEST SUR BETA et Lamb=SLEN (SLIM= élancement maximum pour un BETA donné)
C     -----------------------------------------------------------------------
      ILIM=0
	
	IF(BETA.LE.2) THEN
	   SLIM=3.0+(BETA-1.)/2.
	ELSE
	   SLIM=3.5+(BETA-2.)
	ENDIF
	
	IF(SLEN.GT.SLIM) THEN
	  WRITE(666,*)'ATTENTION: SLEN limite dépassé =',SLIM,
     *                  ' << SLEN(réel) =',SLEN
	  WRITE(666,*)'=========='
	  WRITE(29,*)'ATTENTION: SLEN limite dépassé =',SLIM,							!bug
     *                  ' << SLEN(réel) =',SLEN										!bug
	  WRITE(29,*)'=========='														!bug

	  ILIM=1
	  SLEN=SLIM
	ENDIF

C 1.3 Calcul de SPAIK de référence
C     -----------------------------
	SLEN2=SLEN*SLEN
	BETA2=BETA*BETA
	FCT=DSQRT(0.995 + 0.936*SLEN2 + 0.17*BETA2 + 0.188*BETA2*SLEN2
     *                - 0.067*SLEN2*SLEN2)
      SPAIK=SIGY/FCT

c	WRITE(666,*)' BETA = ',BETA,' et SLEN(modif) = ',SLEN
c	WRITE(666,*)'FCT = ',FCT
c	WRITE(666,*)'****** SPAIK (modif)= ',SPAIK
	

C 2.0 Dérivée de SIG(PAIK) : DSPAIK
C     *****************************
      IF(ILIM.EQ.0) GOTO 3
	
C 2.1 Cas ou les valeurs limites de Béta et Lamda  sont dépassées
C     ---------------------------------------------------------
C     Dans ce cas le calcul des sensibilités se fait par différences finies.

      NBRXI=NVAR(NEL)

      DO 5 K=1,NBRXI

      KK=NXIT(K,NEL)

        IF(KK.EQ.1) THEN
c          WRITE(666,*)' XI=1  DELTA'
c          WRITE(666,*)' $$$$$$$$$$$'
           RATIO=1.05
         XXX=RATIO *DELTA
         CALL ULK(IPRINT,E,SIGY,XXX,EPSA,HXR,DXR,WXR,TXR,ENTR,SPAIK1)
           DSPAIK(KK)=(SPAIK1-SPAIK)/(XXX-DELTA)
c          WRITE(666,*)'Subr ULK: SPAIK1= ',SPAIK1
c	   WRITE(666,*)'DELTA1=',XXX,'  et DSPAIK1=',DSPAIK(KK)
        ELSE IF (KK.EQ.5) THEN                      ! XI=EPSA
c	   WRITE(666,*)' XI=5  EPSA'
c	   WRITE(666,*)' $$$$$$$$$$$'
	   RATIO=0.5
         XXX=RATIO *EPSA
         CALL ULK(IPRINT,E,SIGY,DELTA,XXX,HXR,DXR,WXR,TXR,ENTR,SPAIK1)
	   DSPAIK(KK)=(SPAIK1-SPAIK)/(XXX-EPSA)
c	   WRITE(666,*)'Subr ULK: SPAIK5= ',SPAIK1
c	   WRITE(666,*)'EPSA5=',XXX,'  et DSPAIK5=',DSPAIK(KK)
        ELSE IF (KK.EQ.6) THEN                      ! XI=HXR
	   IND=0
   16    IND=IND+1
c	   WRITE(666,*)' XI=6  HXR'
c	   WRITE(666,*)' $$$$$$$$$$$'
	   RATIO=5.0*IND
         XXX=RATIO *HXR
         CALL ULK(IPRINT,E,SIGY,DELTA,EPSA,XXX,DXR,WXR,TXR,ENTR,SPAIK1)
           DSPAIK(KK)=(SPAIK1-SPAIK)/(XXX-HXR)
c          WRITE(666,*)'Subr ULK: SPAIK6= ',SPAIK1
c          WRITE(666,*)'HXR6=',XXX,'  et DSPAIK6=',DSPAIK(KK)
           IF((DABS(DSPAIK(KK)).LE.1000.).AND.(IND.LE.4)) GOTO 16
        ELSE IF (KK.EQ.7) THEN                      ! XI=Tweb
c          WRITE(666,*)' XI=7   Tweb'
c          WRITE(666,*)' $$$$$$$$$$$'
           RATIO=10.0
         XXX=RATIO *DXR
         CALL ULK(IPRINT,E,SIGY,DELTA,EPSA,HXR,XXX,WXR,TXR,ENTR,SPAIK1)
           DSPAIK(KK)=(SPAIK1-SPAIK)/(XXX-DXR)
c	   WRITE(666,*)'Subr ULK: SPAIK7= ',SPAIK1
c	   WRITE(666,*)'DXR7=',XXX,'  et DSPAIK7=',DSPAIK(KK)
        ELSE IF (KK.EQ.8) THEN                       ! XI= D flange
c	   WRITE(666,*)' XI=8   D flange'
c	   WRITE(666,*)' $$$$$$$$$$$'
	   RATIO=10.0
         XXX=RATIO *WXR
         CALL ULK(IPRINT,E,SIGY,DELTA,EPSA,HXR,DXR,XXX,TXR,ENTR,SPAIK1)	BBO00340
	   DSPAIK(KK)=(SPAIK1-SPAIK)/(XXX-WXR)
c	   WRITE(666,*)'Subr ULK: SPAIK8= ',SPAIK1
c	   WRITE(666,*)'WXR8=',XXX,'  et DSPAIK8=',DSPAIK(KK)
        ELSE IF (KK.EQ.9) THEN                  ! XI=EPSR
c	   WRITE(666,*)' XI=9  EPSR'
c	   WRITE(666,*)' $$$$$$$$$$$'
	   RATIO=0.95
         XXX=RATIO *ENTR
         CALL ULK(IPRINT,E,SIGY,DELTA,EPSA,HXR,DXR,WXR,TXR,XXX,SPAIK1)
           DSPAIK(KK)=(SPAIK1-SPAIK)/(XXX-ENTR)
C        Prise en compte de  d(D)/d(EPSR) = (D/EPSR)**2
	   DSPAIK(KK)=DSPAIK(KK)*(ENTR/EPSR)**2
c	   WRITE(666,*)'Subr ULK: SPAIK9= ',SPAIK1
c	   WRITE(666,*)'EPSR9=',XXX,'  et DSPAIK9=',DSPAIK(KK)
	ELSE
	   DSPAIK(KK)=0.
	ENDIF
  5   CONTINUE
	
      RETURN

   3  CONTINUE

C 2.2 Cas normal: les valeurs de Béta et Lamda sont dans les limites
C     ---------------------------------------------------------
C     Dans ce cas le calcul des sensibilités se fait par calcul direct (analytique)

     	FAC1=(0.936+0.188*BETA2-2.*0.067*SLEN2)*SLEN
      FAC2=(0.17+0.188*SLEN2)*BETA
	TEMP= DELTA*ENTR*YG - HXR*DXR*(SMDR-YG) - WXR*TXR*(SMCR-YG)
      NBRXI=NVAR(NEL)

      DO 1 K=1,NBRXI

        KK=NXIT(K,NEL)	
          IF(KK.EQ.1) THEN                           ! XI=DELTA
            DEPSA=0.                                 ! d(EPSA)/dXI  * SLEN/EPSA
            DBETA=-BETA/DELTA                        ! DBETA=d(BETA)/dXI
            DGX=OM                                   ! DGX  =d(GX) /dXI
            DOMT=ENTR                                ! DOMT =d(OMT)/dXI
            DIS=ENTR*(0.25*DELTA**2+YG**2) + HXR*DXR*(SMDR-YG)  ! DIS =d(IS) /dXI
     *                                   + WXR*TXR*(SMCR-YG)    !
          ELSE IF (KK.EQ.5) THEN                                ! XI=EPSA
            DEPSA=SLEN/EPSA                          ! d(EPSA)/dXI  * SLEN/EPSA
	    DBETA=0.
	    DGX=0.	
	    DOMT=0.									
	    DIS=0.
          ELSE IF (KK.EQ.6) THEN                     ! XI=HXR
	    DEPSA=0.
	    DBETA=0.
	    DGX=OM*2.+DXR*DELTA	
	    DOMT=DXR									
	    DIS=0.25*DXR*HXR**2 + DXR*(SMDR-YG)*(SMDR-YG+HXR)
     *                        + 2.*WXR*TXR*(SMCR-YG)
          ELSE IF (KK.EQ.7) THEN                     ! XI=DXR=Tweb
	    DEPSA=0.
	    DBETA=0.
	    DGX=2.*HXR*SMDR	
	    DOMT=HXR									
            DIS=(HXR**3)/12. +  HXR*(SMDR-YG)**2     !
          ELSE IF (KK.EQ.8) THEN                     ! XI=WXR =Lsem.
	    DEPSA=0.
	    DBETA=0.
	    DGX=2.*TXR*SMCR
	    DOMT=TXR									
	    DIS=(TXR**3)/12. +  TXR*(SMCR-YG)**2
          ELSE IF (KK.EQ.9) THEN                     ! XI=EPSR
	    DEPSA=0.
	    DBETA=BETA/ENTR
	    DGX=0.	
	    DOMT=DELTA									
	    DIS=(DELTA**3)/12. +  DELTA* YG**2
	  ELSE
	    DEPSA=0.
	    DBETA=0.
	    DGX=0.	
	    DOMT=0.									
	    DIS=0.
	  ENDIF
        
	  DYG=(OMT*DGX-GX*DOMT)/(2.*OMT*OMT)
        DIS=DIS +2.*DYG* TEMP                                 ! DIS =d(IS)/dXI
	  DSLEN=SLEN * (IS*DOMT-OMT*DIS) / (2.*OMT*IS) + DEPSA
        DSPAIK(KK)=-SIGY*(FAC1*DSLEN+FAC2*DBETA)/(FCT**3)     ! d(SPAIK)/dXI
	  
C       Prise en compte de  d(D)/d(EPSR) = (D/EPSR)**2 
        IF (KK.EQ.9) THEN
	     DSPAIK(KK)=DSPAIK(KK)*(ENTR/EPSR)**2
	  ENDIF
 
c	  WRITE(666,*)'------  KK = ',KK
c	  WRITE(666,*)'FAC1, FAC2 = ',FAC1,FAC2
c	  WRITE(666,*)'DOMT, OMT,IS = ',DOMT,OMT,IS
c	  WRITE(666,*)'DEPSA, = ',DEPSA,' DYG, DIS = ',DYG,DIS
c	  WRITE(666,*)'SLEN,DBETA, DSLEN = ',SLEN,DBETA,DSLEN
c	  WRITE(666,*)'DSPAIK(',KK,')= ', DSPAIK(KK)
   1  CONTINUE
   
      RETURN 
      END

C ******************************************************************************
C ******************************************************************************
C ******************************************************************************
	
      SUBROUTINE ULK(IPRINT,E,SIGY,DELTA,EPSA,HXR,DXR,WXR,TXR,ENTR,
     *               SPAIK)

      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 IS
      COMMON /PY/ PI

C*******************************************************************************
C     SUBROUTINE ULK
C     ===================
C     Cette subrourine calcule la résistance ultime d'une plaque raidie
C     londitudinalement et comprimée, également, longitudinalement.
C     (La pression latérael n'est pas prise en compte)
C
C     Il s'agit de la formule empirique de J. PAIK.  qui est basée 
C     sur le principe de la poutre colonne.
C                                                                      
C     ENTR = D(raid) remplace EPSR qui était une approximation (bordé collaborant)
C
C*******************************************************************************
C   RAIDISSEURS                                                   
C  -------------
      TMRX=WXR*TXR                               ! section semelle
      OM=TMRX+DXR*HXR                            ! section raid
      OMT=OM+ENTR*DELTA                          ! section raid + bordé
      SMCR=DELTA/2.+HXR+TXR/2.                                   	
      SMDR=DELTA/2.+HXR/2.
      TPBR=TMRX*SMCR
      HX=TPBR+DXR*HXR*SMDR                       ! Moment statique (Z=0)
	
c	WRITE(666,*)'ds subr ULK' 
c	WRITE(666,*)'------------'
c	WRITE(666,*)'HXR,DXR,WXR,TXR,ENTR = ',HXR,DXR,WXR,TXR,ENTR
c	WRITE(666,*)'EPSA,DELTA= ',EPSA,DELTA

	GX=2.*HX		
      YG=GX/(2.*OMT)                                     ! YG=position du centre de gravité.
        IS=(ENTR*DELTA**3 + DXR*HXR**3 + WXR*TXR**3)/12. ! IS=Inertie par rapport au centre de gravité
     *    + DELTA*ENTR*YG*YG  +  HXR*DXR*(SMDR-YG)**2    !    au centre de gravité.
     *                        +  WXR*TXR*(SMCR-YG)**2
      ROT=DSQRT(SIGY/E)
      BETA=ENTR * ROT / DELTA
      SLEN=EPSA * ROT* DSQRT(OMT/IS) / PI

c	WRITE(666,*)' BETA = ',BETA,' et SLEN(non modif) = ',SLEN

C   TEST SUR BETA et Lamb=SLEN (SLIM= élancement maximum pour un BETA donné)
C   ----------------------------------------------------------------------
      ILIM=0
	IF(BETA.LE.2) THEN
	   SLIM=3.0+(BETA-1.)/2.
	ELSE
	   SLIM=3.5+(BETA-2.)
	ENDIF
c	WRITE(666,*)'SLEN maximum = ',SLIM,' >?> SLEN = ',SLEN
	IF(SLEN.GT.SLIM) THEN
	  ILIM=1
	  SLEN=SLIM
c	  WRITE(666,*)' BETA = ',BETA,' et SLEN(modif) = ',SLEN
	ENDIF

C   Calcul de SPAIK
C   ----------------
	BETA2=BETA*BETA

      IF(HXR.LE.0.00001) THEN
        FCT=DSQRT(0.995+0.17*BETA2)
      ELSE
	  SLEN2=SLEN*SLEN
	  FCT=DSQRT(0.995 + 0.936*SLEN2 + 0.17*BETA2 + 0.188*BETA2*SLEN2
     *                - 0.067*SLEN2*SLEN2)
      ENDIF
	
      SPAIK=SIGY/FCT

c	WRITE(666,*)'FCT = ',FCT
c	WRITE(666,*)'****** SPAIK (modif)= ',SPAIK
	
      RETURN 
        END
