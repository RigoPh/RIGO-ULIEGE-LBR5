      SUBROUTINE RESUL(WYA,TYA,DYA,HYA,KSA,WXR,TXR,DXR,HXR,KSR,
     *     DELTA,ETA,LAMB,E,NZ,HXTR,TXTR,WXTR,DXTR,MT,KST,
     *     SIGY,EPSA,EPSR,ENTR,ITYPE,
     *     CONST,Q,DIS,FAM,JJK,INDAIG,INDRAI,IMPR,NETO,NEL,IPRINT,
     *     EFF,EFF2,TEMP,TEMP2,TEMP3,CONC,Z2,
     *     SXM,SPK,SXMM,
     *     SENS1,SENS2,SENS3,
     *     NVAR,NXIT,IPTS,IPTS2,IPTS3)

      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 LAMB,LAMB2
      CHARACTER *18 NOM2

      DIMENSION HXTR(10),TXTR(10),WXTR(10),DXTR(10),DIS(5),CONST(74),
     *          FAM(6),NOM2(15)
     
	DIMENSION SENS1(9,IPTmax,9,NETO),SENS2(16,IPTmax,9,NETO),     !avril2003
     *          SENS3(21,IPTmax,9,NETO),DC(6,9,3),
     *          EFF(9690),EFF2(2040),TEMP(1530),TEMP2(1530),TEMP3(300),
     *          CONC(750),Z2(2295),
     *          SXM(IPTmax),DAA(9),DBB(9),DCC(9)
      DIMENSION NVAR(NETO),NXIT(9,NETO),
     *          IPTS(NETO),IPTS2(IPTmax,NETO),IPTS3(IPTmax,NETO),
     *          SXMM(NETO),SPK(NETO)

      COMMON/DIM1/Nmax,NEmax,NVmax,
     *            M1max,M1Tmax,M2max,M2Tmax,Mmax,MGmax,ISmax,IPTmax ! dimension max des vecteurs
      COMMON /OPTI/IOPTI,NTOT
      COMMON/PY/   PI
      
C*******************************************************************************
c	Ordre ds SENS1(JJ,4,9,NETO) avec JJ=1 à 9
c     DATA NOM/'W','Wø','Wøø','Wøøø','U','Uø','V','Vø ','Vøø'/
C		      1   2    3     4      5   6    7   8     9 
C	Ordre ds SENS2(J,IPTmax=4,9,NETO) avec J=1 à 15
      DATA NOM2/'Sbord Z=0 X=L/2','Tbord Z=0 X=0','Sbord z=-d/2 X=L/2',
     *         'Sbord z=+d/2 X=L/2','Tcadre JAS X=0','Scadre JAS X=L/2',
     *         'Tcadre JAB X=0','Scadre JAB X=L/2','Scadre Sem X=L/2',
     *         'Traid JAS X=0' ,'Traid JAB X=0'   ,'Sraid Sem X=L/2',
     *         'Ttrav JAS X=0' ,'Ttrav JAB X=0'   ,'Strav Sem X=L/2'/
c	Ordre ds SENS3(JJ,4,9,NETO) avec JJ=1 à 20
       ! 1 et 2  dSx=d(Sx/dX) et dSy dans borde  en Z= 0
       ! 3 et 4  dSx=d(Sx/dX) et dSy dans borde  en Z= -d/2
       ! 5 et 6  dSx=d(Sx/dX) et dSy dans borde  en Z= +d/2
       ! 7 et 8  dSy=d(Sy/dX) et dTxy  JAS cadre en x=L/2
       ! 9 et10  dSy=d(Sy/dX) et dTxy  JAB cadre en x=L/2
       !11 et12  dSy=d(Sy/dX) et dTxy  SEM cadre en x=L/2
       !13 et14  dTxy(1) et dTxy(2)  JAS Raid en x=0 
       !15 et16  dTxy(1) et dTxy(2)  JAB Raid en x=0 
       !17 et18  dTxy(1) et dTxy(2)  JAS Trav en x=0 
       !19 et20  dTxy(1) et dTxy(2)  JAB Trav en x=0 
       !21       d(Sx moyen) pour la restriction de PAIK. 
C*******************************************************************************
C     SUBROUTINE RESULT 
C     ===================
C     SUBROUTINE DE MISE EN FORME DES RESULTATS POUR 5 POINTS DE L'AXE  
C     DES X A PARTIR DU VECTEUR Z2,RESULTAT DU CALCUL SUR LES 31+20 DE  
C     L'AXE DES Y(PHI).                                                 
C     LES RESULTATS SONT CONTENUS DANS LES VECTEURS EFF,EFF2 et CONC 
C                                                                       
C    Dernières modifications: Ph. Rigo
C	- contraintes tangentielles dans les aiguilles,  7-09-1993  
C	- contraintes en Z=(+/-)delta/2 (EFF2),            ??-1993  
C	- modif pour LBR5 (Subr OPTI)                   23-08-1994  
C     - Calcul des sensibilités sur les contraintes   20-04-1995 & 3-5-95
C          (solution dans SENS2)
C     - Corrections des Traid et Ttrav (temp2,temp3)   9-02-1996
C     - Test si l'épaisseur semelle traverse = 0      14-04-1997
C     - Correction Sig Trav JAB (sensibilité)         13-02-1998
C     - suppression du common/DI						23-03-1999
C     - Cumul des sensibilites (termes de Fourier)	15-02-2001
c     - combiner RESUL avec STIFF (voir Pt10):        27-05-2003
C                                                                                                                                           
C    Modif :27-5-2003				      Créer: Thèse de doctorat (1988)
C                                                                       
C*******************************************************************************

      SH=E/(2.*(1.+ETA))
      SN=E/(1.-ETA*ETA)
      LAMB2=LAMB*LAMB
C CADRES/Aiguilles
      KSAA=(-1)**KSA
      TM=WYA*TYA
      TMR=TM+DYA*HYA
      SM=(DELTA/2.+HYA+TYA)*KSAA
      SMA=SM-TYA*KSAA
      SMB=(DELTA/2.)*KSAA
      SMC=(DELTA/2.+HYA+TYA/2.)*KSAA
      SMD=(DELTA/2.+HYA/2.)*KSAA
      TPB=TM*SMC
      TPR=TPB+DYA*HYA*SMD
C RAIDISSEURS
      KSRR=(-1)**KSR
      TMRX=WXR*TXR
      TMRR=TMRX+DXR*HXR
      SMR=(DELTA/2.+HXR+TXR)*KSRR
      SMAR=SMR-TXR*KSRR
      SMBR=(DELTA/2.)*KSRR
      SMCR=(DELTA/2.+HXR+TXR/2.)*KSRR
      SMDR=(DELTA/2.+HXR/2.)*KSRR
      TPBR=TMRX*SMCR
      TPRR=TPBR+DXR*HXR*SMDR
		
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                                       PRE00340
C                                                                       
C     CALCUL DES DEFORMATIONS U,V,W et CONTRAINTES (EFF, EFF2, CONC)                                                                    
C                                                                       
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                                                                       PRE00340
C
C    Ordre des variables classées dans EFF (1 à 38)
C    --------------------------------------------------
C       1       V               L=2     U                L=3    W
C       4       dW/dy             5     Nx                 6    Ny
C       7       Nxy               8     Nyx                9    Mx
C      10       My               11     Mxy               12    Myx
C      13       Qx               14     Qy                15    Ry
C      16       Rx

C      17	Sig Y bordage	 18	Sig X bordage	  19	Tau XY bordage
C      20	Sig Comp bordage

C      21	Sig Y JAS (aig)	 22   Sig Y JAB (aig)	  23	Tau JAS (aig) 
C      24	Tau JAB   (aig)	 25   Sig COMP JAS (aig)  26	Sig COMP JAB (aig)
C      27	Sig Y Sem (aig)	 28   Tau sem   (aig)     29    Sig COMP sem (aig)

C      30 Sig X JAS (raid) 	  31   Sig X JAB (raid)	    32	Sig X Sem (raid)  
C      33 Tau   JAS (raid) 	  34   Tau JAB (raid)       35	Tau sem (raid)   
C      36 Sig COMP JAS (raid)   37   Sig COMP JAB (raid)  38  Sig COMP sem (raid)

C    Ordre des variables classées dans EFF2 (1 à 8) 
C    --------------------------------------------------
C      1	Sig Y bord.(z=+d/2)  2	Sig X bord.(z=+d/2)  3	Tau XY bord.(z=+d/2) 
C      4	Sig Comp bord.(z=+d/2) 
C      5	Sig Y bord.(z=-d/2)  6	Sig X bord.(z=-d/2)  7	Tau XY bord.(z=-d/2)
C      8	Sig Comp bord.(z=-d/2)

C    Ordre des variables classées dans CONC (1 à 8) 
C    --------------------------------------------------
C       1 NX    2 NXY    3 MX    4 MXY    5 QX    6 RX
C       7	SIG X SEM       8	SIG X JAS       9	SIG X JAB 
C      10	TYX   SEM      11	TXZ   JAS      12	TXZ   JAB
C      13	SIG COMP SEM   14	SIG COMP JAS   15	SIG COMP JAB

C    Vecteur Z2:contient 51 valeurs par variables (31 + 20 au droit des traverses)
C    --------------------------------------------------
C     Z2(  0+1 à   51) : V     Z2(153+1  à  204) : Xo(1)        
C     Z2( 51+1 à  102) : U     Z2(204+1  à  255) : Zo(1)      Z2(1479+1 à  1530) : V°  
C     Z2(102+1 à  153) : W     Z2(255+1  à  306) : Xo(2)      Z2(1530+1 à  1581) : U°  
C                              Z2(306+1  à  357) : Zo(2)      Z2(1581+1 à  1632) : W°  
C                              ... etc.                       Z2(1632+1 à  1683) : V°°  
C                              Z2(1071+1 à 1122) : Xo(10)     Z2(1683+1 à  1734) : U°°  
C                              Z2(1122+1 à 1173) : Zo(10)     Z2(1734+1 à  1785) : W°°  
C                                                             Z2(1785+1 à  1836) : V°°°  
C                              Z2(1173+1 à 1224) : My         Z2(1836+1 à  1887) : U°°°  
C                              Z2(1224+1 à 1275) : Ny         Z2(1887+1 à  1938) : W°°°  
C                              Z2(1275+1 à 1326) : Ry         Z2(1938+1 à  1989) : Nx  
C                              Z2(1326+1 à 1377) : Nyx        Z2(1989+1 à  2040) : Nxy  
C                              Z2(1377+1 à 1428) : Qy         Z2(2040+1 à  2091) : Mx  
C                              Z2(1428+1 à 1479) : W°         Z2(2091+1 à  2142) : Mxy  
C                                                             Z2(2142+1 à  2193) : Myx  
C                                                             Z2(2193+1 à  2244) : Qx  
C                                                             Z2(2244+1 à  2295) : Rx  

      KH=31+2*MT
      DO 114 K=1,5
      SP=DSIN(LAMB*DIS(K))
      SQ=DCOS(LAMB*DIS(K))
C
C     CALCUL EN 5 PTS SELON X ET EN 31+20(MAX) PTS SELON Y DE:
C     --------------------------------------------------------
      DO 112 I=1,KH
      IJ=I+(K-1)*51
C     CALCUL DE  V,U,W,DW/DY,NX,NY,NXY,NYX,MX,MY,MXY,MYX,QX,QY,RY,RX,
      EFF(IJ)=EFF(IJ)+Z2(I)*SP                  ! V
      EFF(IJ+255)=EFF(IJ+255)+Z2(I+51)*SQ       ! U
      EFF(IJ+510)=EFF(IJ+510)+Z2(I+102)*SP      ! W
      EFF(IJ+765)=EFF(IJ+765)+Z2(I+1581)*SP     ! Wø
      IF(IMPR.LE.-1) GOTO 555                   !
      EFF(IJ+1020)=EFF(IJ+1020)+Z2(I+1938)*SP   ! Nx
      EFF(IJ+1275)=EFF(IJ+1275)+Z2(I+1224)*SP   ! Ny
      EFF(IJ+1530)=EFF(IJ+1530)+Z2(I+1989)*SQ   ! Nxy
      EFF(IJ+1785)=EFF(IJ+1785)+Z2(I+1326)*SQ   ! Nyx
      EFF(IJ+2040)=EFF(IJ+2040)+Z2(I+2040)*SP   ! Mx
      EFF(IJ+2295)=EFF(IJ+2295)+Z2(I+1173)*SP   ! My
      EFF(IJ+2550)=EFF(IJ+2550)+Z2(I+2091)*SQ   ! Mxy
      EFF(IJ+2805)=EFF(IJ+2805)+Z2(I+2142)*SQ   ! Myx
      EFF(IJ+3060)=EFF(IJ+3060)+Z2(I+2193)*SQ   ! Qx
      EFF(IJ+3315)=EFF(IJ+3315)+Z2(I+1377)*SP   ! Qy
      EFF(IJ+3570)=EFF(IJ+3570)+Z2(I+1275)*SP   ! Ry
      EFF(IJ+3825)=EFF(IJ+3825)+Z2(I+2244)*SQ   ! Rx
  555 CONTINUE

C  calcul direct de NX (pour comparer avec NX obtenu dans RESUL= EFF(IJ+1020))
c	CALCUL DE Nx en fct DE u,vø,w
c     DV=Vø=dV/dy	=Z2(I+1479)		
c      IF(K.EQ.5) THEN
c       CTER1=-LAMB*(CONST(1)+CONST(9))
c	  CTER2=CONST(1)*(ETA)
c  	  CTER3=(CONST(1)*(ETA/Q)+LAMB2*CONST(10))
c	  CTER=( CTER1*Z2(I+51)+CTER2*Z2(I+1479)+CTER3*Z2(I+102) )*SP
c	  FNX(I)=FNX(I)
c	  WRITE(66,*)'CTER1,CTER2,CTER3= ',CTER1,CTER2,CTER3
c	  WRITE(66,*)'FNX(',I,')= ',CTER,' 1 terme'
C       NX obtenu dans RESUL (dans EFF); voir common ci-avant
c	  WRITE(66,*)'NX (1 terme)= ',Z2(I+1938), '  et SP=',SP
c	  WRITE(66,*)
c      ENDIF


C     LE BORDAGE
C     **********
C     CALCUL DANS LE BORDAGE A MI EPAISSEUR (Z=0)   
C     - SIGMAY,SIGMAX,TXY,SIGMA COMP             
	IF((ITYPE.EQ.1).OR.(ITYPE.EQ.2)) THEN
       EFF(IJ+4080)=EFF(IJ+4080)+SN*(Z2(I+1479)-LAMB*ETA*Z2(I+51)     ! SIGMAY
     *                              +Z2(I+102)/Q)*SP                                    !
      ELSE IF((ITYPE.EQ.3).OR.(ITYPE.EQ.4)) THEN ! sans contribution selon OY
       EFF(IJ+4080)=EFF(IJ+4080)+SN*(-LAMB*ETA*Z2(I+51))*SP           ! SIGMAY
	ENDIF
      EFF(IJ+4335)=EFF(IJ+4335)+SN*(-LAMB*Z2(I+51)+ETA*(Z2(I+1479)    ! SIGMAX
     *            +Z2(I+102)/Q))*SP                                   !
      EFF(IJ+4590)=EFF(IJ+4590)+SH*(Z2(I+1530)+LAMB*Z2(I))*SQ         ! TXY
      IF (NZ.NE.JJK) GOTO 888                                         !
      EFF(IJ+4845)=DSQRT(EFF(IJ+4080)**2+EFF(IJ+4335)**2-EFF(IJ+4080)*! SIGMA COMP
     *             EFF(IJ+4335)+3.D0*(EFF(IJ+4590)**2))                 
 888  CONTINUE                                                          

      IF((IMPR.LE.-2).AND.(IOPTI.EQ.0)) GOTO 2888
C     CALCUL DANS LE BORDAGE A LA FIBRE POSITIVE  (Z=+DELTA/2)  
C     - SIGMAY,SIGMAX,TXY,SIGMA COMP   
	IF((ITYPE.EQ.1).OR.(ITYPE.EQ.2)) THEN
       EFF2(IJ)=EFF2(IJ)+SN*(Z2(I+1479)-LAMB*ETA*Z2(I+51)+Z2(I+102)/Q ! SIGMAY
     *           -(DELTA/2.)*( Z2(I+1734)-ETA*LAMB2*Z2(I+102) ) )*SP
      ELSE IF((ITYPE.EQ.3).OR.(ITYPE.EQ.4)) THEN ! sans contribution selon OY
       EFF2(IJ)=EFF2(IJ)+SN* ( -LAMB*ETA*Z2(I+51)                     ! SIGMAY
     *          -(DELTA/2.)*(-ETA*LAMB2*Z2(I+102) ) )*SP
	ENDIF
      EFF2(IJ+255)=EFF2(IJ+255)+SN*(-LAMB*Z2(I+51)+ETA*(Z2(I+1479)    ! SIGMAX
     *         +Z2(I+102)/Q)
     *         -(DELTA/2.)*( ETA*Z2(I+1734)-LAMB2*Z2(I+102) ) )*SP
      EFF2(IJ+510)=EFF2(IJ+510)+SH*(Z2(I+1530)+LAMB*Z2(I)             ! TXY
     *                         -DELTA*LAMB*Z2(I+1581) )*SQ
      IF (NZ.NE.JJK) GOTO 1888
      EFF2(IJ+765)=DSQRT(EFF2(IJ)**2+EFF2(IJ+255)**2-EFF2(IJ)*        ! SIGMA COMP
     *             EFF2(IJ+255)+3.D0*(EFF2(IJ+510)**2))  
 1888  CONTINUE                                                         
C     CALCUL DANS LE BORDAGE A LA FIBRE NEGATIVE (Z=-DELTA/2) 
C     - SIGMAY,SIGMAX,TXY,SIGMA COMP   
	IF((ITYPE.EQ.1).OR.(ITYPE.EQ.2)) THEN
       EFF2(IJ+1020)=EFF2(IJ+1020)+SN*(Z2(I+1479)-LAMB*ETA*Z2(I+51)   ! SIGMAY
     *           +Z2(I+102)/Q
     *           +(DELTA/2.)*( Z2(I+1734)-ETA*LAMB2*Z2(I+102) ) )*SP
      ELSE IF((ITYPE.EQ.3).OR.(ITYPE.EQ.4)) THEN ! sans contribution selon OY
       EFF2(IJ+1020)=EFF2(IJ+1020)+SN*(-LAMB*ETA*Z2(I+51)             ! SIGMAY
     *           +(DELTA/2.)*(-ETA*LAMB2*Z2(I+102) ) )*SP
	ENDIF
      EFF2(IJ+1275)=EFF2(IJ+1275)+SN*(-LAMB*Z2(I+51)+ETA*(Z2(I+1479)  ! SIGMAX
     *            +Z2(I+102)/Q)
     *           +(DELTA/2.)*( ETA*Z2(I+1734)-LAMB2*Z2(I+102) ) )*SP
      EFF2(IJ+1530)=EFF2(IJ+1530)+SH*(Z2(I+1530)+LAMB*Z2(I)           ! TXY
     *                         +DELTA*LAMB*Z2(I+1581) )*SQ
      IF (NZ.NE.JJK) GOTO 2888
      EFF2(IJ+1785)=DSQRT(EFF2(IJ+1020)**2+EFF2(IJ+1275)**2-          ! SIGMA COMP
     *            EFF2(IJ+1020)*EFF2(IJ+1275)+3.D0*(EFF2(IJ+1530)**2))
 2888  CONTINUE                                                         

C            CADRES/AIGUILLES
C            ****************
C   CALCUL DE : SIGMAY JAS, SIGMAY JAB, TYZ JAS, TYZ JAB,               
C               SIGMA COMP JAS, SIGMA COMP JAB .... DS AMES CADRES
      IF((INDAIG.EQ.0).AND.(IOPTI.EQ.0)) GOTO 113
      IF(DYA.EQ.0.)GOTO 889
      EFF(IJ+5100)=EFF(IJ+5100)+E*(Z2(I+1479)+Z2(I+102)/Q				! SIGMAY JAS
     *             -SMA*Z2(I+1734))*SP
      EFF(IJ+5355)=EFF(IJ+5355)+E*(Z2(I+1479)+Z2(I+102)/Q				! SIGMAY JAB
     *             -SMB*Z2(I+1734))*SP
      TEMP(IJ)    =TEMP(IJ)   +										! TYZ(Ry) JAS =T1
     *     E*( TM*(Z2(I+1632)+Z2(I+1581)/Q)-TPB*Z2(I+1887))*SP/DYA
      TEMP(IJ+255)=TEMP(IJ+255)+										! TYZ(Ry) JAB =T1
     *     E*(TMR*(Z2(I+1632)+Z2(I+1581)/Q)-TPR*Z2(I+1887))*SP/DYA
      TEMP(IJ+510)=TEMP(IJ+510)+ SH*DYA*LAMB*Z2(I+1581)*SQ			! TYZ(Mxy) JAB = TYZ(Mxy) JAS = T2
      IF (NZ==JJK) THEN
       EFF(IJ+5610)=DABS(TEMP(IJ))    +DABS(TEMP(IJ+510))				! TYZ JAS = T1+T2
       EFF(IJ+5865)=DABS(TEMP(IJ+255))+DABS(TEMP(IJ+510))				! TYZ JAB = T1+T2
       EFF(IJ+6120)=DSQRT(EFF(IJ+5100)**2+3.D0*(EFF(IJ+5610)**2))		! SIGMA COMP JAS
       EFF(IJ+6375)=DSQRT(EFF(IJ+5355)**2+3.D0*(EFF(IJ+5865)**2))		! SIGMA COMP JAB
      ENDIF
 889  CONTINUE
C     CALCUL DE : SIGMAY SEMELLE,TXY SEM, SIGMA COMP SEMELLE CADRE
      IF(TYA.EQ.0.)GOTO 113
      EFF(IJ+6630)=EFF(IJ+6630) +										! SIGMAY SEMELLE
     *              E*(Z2(I+1479)+Z2(I+102)/Q - SM*Z2(I+1734))*SP
      TEMP(IJ+765)=TEMP(IJ+765)+ SP*E*								! TXY(Ry) SEM = T1
     *  ( TM*(Z2(I+1632)+Z2(I+1581)/Q)/2. - TPB*Z2(I+1887)/2. ) /TYA
      TEMP(IJ+1020)=TEMP(IJ+1020)+									! TXY(??) SEM = T2
     *                   SQ *SH*1.25*FAM(1)*(Z2(I+1530)-LAMB*Z2(I))
      TEMP(IJ+1275)=TEMP(IJ+1275)+ SQ*SH*TYA*LAMB*Z2(I+1581)			! TXY(Mxy) SEM T3
      IF (NZ==JJK) THEN
       EFF(IJ+6885)=DABS(TEMP(IJ+765))+DABS(TEMP(IJ+1020))			! TXY SEM =T1+T2+T3
     *                                +DABS(TEMP(IJ+1275)) 
       EFF(IJ+7140)=DSQRT(EFF(IJ+6630)**2+3.D0*(EFF(IJ+6885)**2))		! SIGMA COMP SEM AIG.
	ENDIF
 113  CONTINUE

C          RAIDISSEURS
C         **************
C   CALCUL DE : SIGMAX JAS, SIGMAX JAB, SIGMAX SEMELLE,

      IF((INDRAI.EQ.0).AND.(IOPTI.EQ.0)) GOTO 112
      IF(DXR.NE.0.) THEN
       EFF(IJ+7395)=EFF(IJ+7395)+									! SIGMAX JAS
     *              E*LAMB* (-Z2(I+51)+SMAR*LAMB*Z2(I+102))*SP
       EFF(IJ+7650)=EFF(IJ+7650)+									! SIGMAX JAB
     *              E*LAMB* (-Z2(I+51)+SMBR*LAMB*Z2(I+102))*SP
	ENDIF
      IF(TXR.NE.0.)THEN
       EFF(IJ+7905)=EFF(IJ+7905)+									! SIGMAX SEMELLE
     *              E*LAMB* (-Z2(I+51)+SMR* LAMB*Z2(I+102))*SP       
      ENDIF 
C   CALCUL DE: TXZ JAS, TXZ JAB, Tyx SEM, 
      IF(DXR.NE.0.) THEN
       TEMP2(IJ)=TEMP2(IJ)+ SH*DXR*LAMB*Z2(I+1581) *SQ			! TXZ(Mxy)JAS = TXZ(Mxy) JAB
       TEMP2(IJ+255)=TEMP2(IJ+255)+								! TXZ(Rx) JAS
     *    (-LAMB2*E*(Z2(I+51)*TMRX-LAMB*TPBR*Z2(I+102))/DXR) *SQ	
       TEMP2(IJ+510)=TEMP2(IJ+510)+								! TXZ(Rx) JAB
     *    (-LAMB2*E*(Z2(I+51)*TMRR-LAMB*TPRR*Z2(I+102))/DXR) *SQ    
	ENDIF
      IF(TXR.NE.0.) THEN                                           
       TEMP2(IJ+765) =TEMP2(IJ+765) +  SH*TXR*LAMB*Z2(I+1581) * SQ		! Tyx(Rx) SEM
       TEMP2(IJ+1020)=TEMP2(IJ+1020)+       
     * (-LAMB2*E* (Z2(I+51)*TMRX/2.-LAMB*TPBR*Z2(I+102)/2.) /TXR) * SQ	! Tyx(Mxy) SEM   
       TEMP2(IJ+1275)=TEMP2(IJ+1275)+
     *            ( (-LAMB*Z2(I)+Z2(I+1530)) *CONST(11)*1.5/TMRX) * SQ	! Tyx(??) SEM  
      ENDIF     
C   CALCUL DE : SIGMA COMP JAS, SIGMA COMP JAB , SIGMA COMP SEMELLE     
      IF (NZ.NE.JJK) GOTO 112                                         
      EFF(IJ+8160)=DABS(TEMP2(IJ+255))+DABS(TEMP2(IJ))			! TXZ JAS
      EFF(IJ+8415)=DABS(TEMP2(IJ+510))+DABS(TEMP2(IJ))			! TXZ JAB
      EFF(IJ+8925)=DSQRT(EFF(IJ+7395)**2+3.D0*(EFF(IJ+8160)**2))	! SIGMA COMP JAS
      EFF(IJ+9180)=DSQRT(EFF(IJ+7650)**2+3.D0*(EFF(IJ+8415)**2))	! SIGMA COMP JAB
      IF(TXR.NE.0.) THEN
       EFF(IJ+8670)=DABS(TEMP2(IJ+765))+DABS(TEMP2(IJ+1020))		! Tyx SEM
     *                                 +DABS(TEMP2(IJ+1275))
       EFF(IJ+9435)=DSQRT(EFF(IJ+7905)**2+3.D0*(EFF(IJ+8670)**2))	! SIGMA COMP SEMELLE
	ENDIF
  112 CONTINUE   ! boucle sur les 31+2*MT points (selon Y)                                                
C
      IF(MT.EQ.0) GOTO 114
C
C     CALCUL DES EFFETS CONCENTRES AUX NIVEAUX DES TRAVERSES POUR 5 PTS
C     SELON X ET A L'EMPLACEMENT DES TRAVERSES SELON Y.
C     SONT CLASSES PAR ORDRE CROISSANT DANS LE VECTEUR CONC.
C
C     Ordre des variables classées dans CONC (1 à 8)
C     --------------------------------------------------
C       1 NX    2 NXY    3 MX    4 MXY    5 QX    6 RX
C       7	SIG X SEM       8	SIG X JAS       9	SIG X JAB 
C      10	TYX   SEM      11	TXZ   JAS      12	TXZ   JAB
C      13	SIG COMP SEM   14	SIG COMP JAS   15	SIG COMP JAB

	KSTT=(-1)**KST
      DO 116 I=1,MT
      JI=6*(I-1)
      IJ=I+(K-1)*10

      WYT=WXTR(I)                                   ! Larg    sem
      TYT=TXTR(I)                                   ! Epaiss. sem
      HYT=HXTR(I)                                   ! Haut.   âme
      DYT=DXTR(I)                                   ! Epaiss. âme
	
      TMT =WYT*TYT
      TMRT=TMT+DYT*HYT
      SMT =(DELTA/2.+HYT+TYT)*KSTT
      SMAT=SMT-TYT*KSTT
      SMBT=(DELTA/2.)*KSTT
      TPBT=TMT*(SMAT+0.5*TYT*KSTT)
      TPRT=TPBT+DYT*HYT*(SMAT-0.5*HYT*KSTT)
	
C     REM: CONC(1 A 50) CONTIENT "NX CONC." SANS LA COMPOSANTE SELON X  
C            DE NYX (= SX*DV/DY + D2U/D2Y / LAMB ) C.A.D. LA            
C            RECIPROQUE DE "NXY CONC.                                   
      CONC(IJ)=CONC(IJ)+LAMB*( -CONST(15+JI)*Z2(2*I+81)               ! NX
     *                         +LAMB*CONST(16+JI)*Z2(2*I+132) ) *SP     
      CONC(IJ+50)=CONC(IJ+50)+ CONST(17+JI)*SQ *( Z2(2*I+30)*LAMB     ! NXY
     *                       + Z2(2*I+1560))                            
      CONC(IJ+100)=CONC(IJ+100)+LAMB*(-LAMB*CONST(18+JI)*Z2(2*I+132)+ ! MX
     *   CONST(16+JI)*Z2(2*I+81))*SP
      CONC(IJ+150)=CONC(IJ+150) +( LAMB*CONST(19+JI)*Z2(2*I+1611)+    ! MXY
     *           CONST(20+JI) * (Z2(2*I+30)*LAMB+Z2(2*I+1560)) )*SQ     
      CONC(IJ+200)=CONC(IJ+200)+(LAMB2*(-LAMB*CONST(18+JI)*Z2(2*I+132)! QX
     *       +CONST(16+JI)*Z2(2*I+81))+                                 
     *    CONST(8) *(Z2(2*I+1561)-Z2(2*I+1560)))*SQ                     
      CONC(IJ+250)=CONC(IJ+250)+ SQ *( CONST(19+JI)*LAMB*Z2(2*I+1764) ! RX (partiel)
     *  + CONST(14)*(Z2(2*I+1561)-Z2(2*I+1560))                         
     *  +  CONST(20+JI)*(LAMB*Z2(2*I+1509)+Z2(2*I+1713))  )             
      IF((TYT.GT.0.).AND.(WYT.GT.0.)) THEN                                             
       CONC(IJ+300)=CONC(IJ+300)+  E*LAMB*								! SIG X SEM
     *                (-Z2(2*I+81)+LAMB*SMT*Z2(2*I+132))*SP
      ENDIF
      CONC(IJ+350)=CONC(IJ+350)+E*LAMB*(-Z2(2*I+81)						! SIG X JAS
     *                            +SMAT*LAMB*Z2(2*I+132))*SP
      CONC(IJ+400)=CONC(IJ+400)+E*LAMB*(-Z2(2*I+81)						! SIG X JAB
     *                            +SMBT*LAMB*Z2(2*I+132))*SP
      IF((TYT.GT.0.).AND.(WYT.GT.0.)) THEN
       TEMP3(IJ)    =TEMP3(IJ) + SQ*										! TYX(Nxy)  SEM
     *    (CONST(17+JI)*(-LAMB*Z2(2*I+30)+Z2(2*I+1560))*1.5/TMT)*FAM(5)           
       TEMP3(IJ+50) =TEMP3(IJ+50)  + SQ* SH*TYT*LAMB*Z2(2*I+1611)			! TYX(Mxy)  SEM
       TEMP3(IJ+100)=TEMP3(IJ+100) + SQ* 
     *  (-LAMB2* E*(Z2(2*I+81)*TMT/2.-LAMB*TPBT*Z2(2*I+132)/2.)) /TYT		! TYX(Rx)  SEM
      ENDIF
      IF(DYT.NE.0.) THEN
       TEMP3(IJ+150)=TEMP3(IJ+150)+ (-LAMB**2*E*(Z2(2*I+81)*TMT-			! TXZ(Rx) JAS
     *            LAMB*TPBT*Z2(2*I+132))/DYT)    *SQ                    
       TEMP3(IJ+200)=TEMP3(IJ+200)+ (-LAMB**2*E*(Z2(2*I+81)*TMRT-			! TXZ(Rx) JAB
     *            LAMB*TPRT*Z2(2*I+132))/DYT) *SQ                    
       TEMP3(IJ+250)=TEMP3(IJ+250)+ SH*DYT*LAMB*Z2(2*I+1611) *SQ			! TXZ(Mxy)JAS=TXZ(Mxy)JAB
      ENDIF

      IF(NZ.EQ.JJK) THEN                                           
       CONC(IJ+250)=CONC(IJ+250)+CONC(IJ+200)							! RX
       IF((TYT.GT.0.).AND.(WYT.GT.0.)) THEN
        CONC(IJ+450)=DABS(TEMP3(IJ))+DABS(TEMP3(IJ+50))				! TYX  SEM
     *                              +DABS(TEMP3(IJ+100))
        CONC(IJ+600)=DSQRT(CONC(IJ+300)**2+3.*CONC(IJ+450)**2)		! SIG COMP SEM
	 ENDIF
       CONC(IJ+500)=DABS(TEMP3(IJ+150))+DABS(TEMP3(IJ+250) )			! TXZ JAS
       CONC(IJ+550)=DABS(TEMP3(IJ+200))+DABS(TEMP3(IJ+250) )			! TXZ JAB
       CONC(IJ+650)=DSQRT(CONC(IJ+350)**2+3.*CONC(IJ+500)**2)			! SIG COMP JAS
       CONC(IJ+700)=DSQRT(CONC(IJ+400)**2+3.*CONC(IJ+550)**2)			! SIG COMP JAB
      ENDIF

  116 CONTINUE  ! boucle sur les traverses
  114 CONTINUE  ! boucle sur les 5 coupes (selon X)

C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  

C    CONTRAINTE  Sx Moyenne dans borde + RAIDISSEURS  en X=L/2 (Sigma X) 
C    -------------------------------------------------------------------
C     Pour le calcul de la restriction de PAIK 
c       SXM(IPT) =   Sx Moyen  = contrainte moyenne (plaque et raid inclus) 
c     d(SXM)/dx  = d(Sx Moyen)/dx  = SENS3(21,I,K,IPAN) 

      IF(NZ==1) CALL ANNULD(SXM,IPT)


C      SIG(moyen) = Nx/section        avec  Nx(axial):
C                 = a U + b V° + c W
  !    Nx = (D+Ox) dU/dx   + (D ETA/Q) dV/dy + (D ETA/Q) W  - Hx  W''  (avec W' = dW/dx)
  !    Nx =-(D+Ox)LAMB * U + (D ETA/Q) DV    + (D ETA/Q) W  + Hx  LAMB2 W
  !     !!! comme U est en COS, on a U' (en SIN) = - Lamb U 
C       avec
C        TMRR= section raidisseur
C        OMT = section raidisseur + largeur effective (EPSR)
C        HX  = moment statique
c        SN   = E/(1.-ETA*ETA)    Deja calcule ci-avant
c        KSRR = (-1)**KSR 
c	   LAMB2= LAMB*LAMB
c        TMRR = Section raid (ame + sem)
        OMT=TMRR +EPSR*DELTA     ! section raid + bordé
	  OMT2=OMT*OMT
	  COF1=SN*ETA
	  HX=DABS(TPRR) ! Moment statique en Z=0
        SPP=DSIN(NZ*PI/2.)
	
        AA=-LAMB*(SN*DELTA*EPSR+E*TMRR)/OMT   ! a/section
        BB=COF1*DELTA*EPSR/OMT				! b/section
	  CC=BB/Q+KSRR*LAMB2*E*HX/OMT			! c/section

c	  WRITE(666,*)'** NEL  = ',NEL
c	  WRITE(666,*)'   NZ   = ',NZ
c	  WRITE(666,*)'AA,BB,CC  = ',AA,BB,CC	

      IPT=IPTS(NEL)
	IF (IOPTI==0) IPT=1 ! soit un seul pt
	SXMM(NEL)=0.
      DO I=1,IPT
	 IP=IPTS2(I,NEL)
	 IF (IOPTI==0) IP=16 ! soit au milieu du panneau

c	  WRITE(666,*)'I  = ',I,'   IP= ',IP
c	  WRITE(666,*)'Z2(IP+1479) DV= ',Z2(IP+1479)	
c	  WRITE(666,*)'Z2(IP+102)   W= ',Z2(IP+102)	

	  SXM(I)=SXM(I) + SPP *
     *           ( AA*Z2(IP+51) + BB* Z2(IP+1479) + CC *Z2(IP+102) )
c       SMOYEN=    AA*U(IP)     + BB* DV(IP)      + CC *W(IP)


	  IF(SXMM(NEL).GE.SXM(I)) SXMM(NEL)=SXM(I)
c	  WRITE(666,*)'SXM(I) = ',SXM(I),'  SXMM(NEL) = ',SXMM(NEL)	

	ENDDO

c	WRITE(666,*) '***************'	

c     Resist. Ult selon la Form. Empirique de PAIK (Subr ULK incluse dans SUBR PAIK)
      IF(NZ==JJK) THEN
        CALL ULK(IPRINT,E,SIGY,DELTA,EPSA,HXR,DXR,WXR,TXR,ENTR,SPAIK) !! ENTR et pas EPSR
      
	  SPK(NEL)=SPAIK
c	  WRITE(67,*)'Panneau',NEL
c	  WRITE(67,*)' Sigma Ultime Raid.(Form. Paik)  =',SPK(NEL) ,' N/mm2'
c       WRITE(67,*)' Sig X max panel (in compr.[S<0])=',SXMM(NEL),' N/mm2'
	 
	ENDIF


C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
C                                                                       
C     CALCUL DES SENSIBILITES SUR LES CONTRAINTES  
C     --------------------------------------------
C	- [calcul de SENS2(16,IPTmax,9,NETO) ] : contraintes bordage, aig., raid. et traverses  !avril2003
C     
C     Dérivées des contraintes par rapport aux variables de conception. 
C                        
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ 
C     Boucle 284/283:Boucle sur toutes les variables de conception;
C                  (Il y a NBRXI variable de conception par panneau)
C     Boucle 280 = Boucle sur les points de calcul et les fonctions
c
C     SENS2 est relatif au panneau étudié par BO2 cad le panneau NEL,
C     mais il donne les sensibilités pour les variables de conception
C     de tous les panneaux (IPAN=1,NETO)

      IF(IOPTI.EQ.0) GO TO 549

	IPT=IPTS(NEL)

	SP=DSIN(NZ*PI/2)	! Sin(x=L/2)
	SQ=+1.0				! COS (x=0) = COS(x=L)

	
	DO 284 IPAN=1,NETO
      NBRXI=NVAR(IPAN)

	CALL ANNULD(DC,162)
	IF(NEL.EQ.IPAN) THEN
C      DC(I,K,J) ; DC = matrice contenant les dérivées des coef TM,TMR,etc.
C         avec   I = nø de la contrainte (i=1,6)
C                        Contr. 1  : 0             ;  Contr. 2  : 0
C                        Contr. 3  : DC(1,K,1)     ;  Contr. 4  : DC(1,K,2)
C                        Contr. 5/7: 0
C                        Contr. 6  : DC(2,K,J=1à3) ;  Contr. 9  : DC(2,K,J=1à3)
C                        Contr. 8  : DC(3,K,J=1à3)
C                        Contr.10  : DC(4,K,J=1à2)
C                        Contr.11  : DC(5,K,J=1à2)
C                        Contr.12  : DC(6,K,J=1)
C                K = nø de la variables de conception (1 à 9)
C                J = nø des paramètres (K=1 à 3 max)
C  rem DC=O si NEL est différent de IPAN (cad variables XI d'un autre panneau)

      DC(1,1,1)=-0.5                                     ! Sig en -d/2 Contr. 3
      DC(1,1,2)=+0.5                                     ! Sig en +d/2 Contr. 4

      DC(2,1,2)=KSAA/2.                                  ! Saig JAS-SEM, Contr. 6/9
      DC(2,1,3)=TM*KSAA/2.                               ! Saig JAS-SEM, Contr. 6/9
      DC(2,2,2)=KSAA                                     ! Saig JAS-SEM, Contr. 6/9
      DC(2,2,3)=TM*KSAA                                  ! Saig JAS-SEM, Contr. 6/9
      DC(2,4,1)=TYA                                      ! Saig JAS-SEM, Contr. 6/9
      DC(2,4,3)=TYA*SMC                                  ! Saig JAS-SEM, Contr. 6/9
                                                         !
      DC(3,1,2)=KSAA/2.                                  ! Saig JAB, Contr. 8
      DC(3,1,3)=TMR*KSAA/2.                              ! Saig JAB, Contr. 8
      DC(3,2,1)=DYA                                      ! Saig JAB, Contr. 8
      DC(3,2,3)=(TMR+DYA*DELTA/2.)*KSAA                  ! Saig JAB, Contr. 8
      DC(3,3,1)=HYA                                      ! Saig JAB, Contr. 8
      DC(3,3,3)=HYA*SMD                                  ! Saig JAB, Contr. 8
      DC(3,4,1)=TYA                                      ! Saig JAB, Contr. 8
      DC(3,4,3)=TYA*SMC                                  ! Saig JAB, Contr. 8
                                                         !
      DC(4,1,2)=TMRX*KSRR/2.                             ! Traid JAS, Contr. 10
      DC(4,6,2)=TMRX*KSRR                                ! Traid JAS, Contr. 10
      DC(4,8,1)=TXR                                      ! Traid JAS, Contr. 10
      DC(4,8,2)=TXR*SMCR                                 ! Traid JAS, Contr. 10
                                                         !
      DC(5,1,2)=TMRR*KSRR/2.                             ! Traid JAB, Contr. 11
      DC(5,6,1)=DXR                                      ! Traid JAB, Contr. 11
      DC(5,6,2)=(TMRR+DXR*DELTA/2.)*KSRR                 ! Traid JAB, Contr. 11
      DC(5,7,1)=HXR                                      ! Traid JAB, Contr. 11
      DC(5,7,2)=HXR*SMDR                                 ! Traid JAB, Contr. 11
      DC(5,8,1)=TXR                                      ! Traid JAB, Contr. 11
      DC(5,8,2)=TXR*SMCR                                 ! Traid JAB, Contr. 11
                                                         !
      DC(6,1,1)=KSRR/2.                                  ! Sraid SEM, Contr. 12
      DC(6,6,1)=KSRR                                     ! Sraid SEM, Contr. 12
	ENDIF

      DO 283 KK=1,NBRXI
      K=NXIT(KK,IPAN)
 
C	nø 1	 Sigma comp. (Sx,Sy) bordage (Z=0)      en x=L/2  (en N/mm2)
C	nø 2	 Sigma comp. (Txy)   bordage (Z=0)      en x=0
C	nø 3	 Sigma comp. (Sx,Sy) bordage (Z=-d/2)   en x=L/2
C	nø 4	 Sigma comp. (Sx,Sy) bordage (Z=+d/2)   en x=L/2

C	nø 5	 Sigma comp. (Tâme)   aiguille   JAS     en X=0    (en N/mm2)
C	nø 6	 Sigma comp. (Sy,Txy) aiguille   JAS     en X=L/2
C	nø 7	 Sigma comp. (Tâme)   aiguille   JAB     en X=0
C	nø 8	 Sigma comp. (Sy,Txy) aiguille   JAB     en X=L/2
C	nø 9	 Sigma comp. (Sy,Txy) aiguille   Semelle en X=L/2

C	nø 10	 Sigma comp. (Tâme)   raidisseur JAB     en X=L/2    (en N/mm2)   !avril2003
C	nø 11	 Sigma comp. (Tâme)   raidisseur JAB     en X=0
C	nø 12	 Sigma comp. (Sx)     raidisseur Semelle en X=L/2

C	nø 13	 Sigma comp. (Tâme)   Traverse JAS       en X=0    (en N/mm2)
C	nø 14	 Sigma comp. (Tâme)   Traverse JAB       en X=0
C	nø 15	 Sigma comp. (Sx)     Traverse Semelle   en X=L/2
 
C  1- CONTRAINTE ds BORDAGE en Z=0 et X=L/2  (SigmaX,SigmaY,Txy,SIGMA Comp) 
C  --------------------------------------------------------------------------

c 	WRITE(66,*)'1 CONTRAINTE ds BORDAGE en Z=0 et X=L/2'
      DO 380 I=1,IPT                                             		
	IJ=4*51+IPTS2(I,NEL)

c 	WRITE(66,*)'** 1 CONTRAINTE ds BORDAGE en Z=0 et X=L/2'
c 	WRITE(66,*)'IPT=',I
c 	WRITE(66,*)'LAMB=',LAMB,' SN=',SN,' SP=',SP
c 	WRITE(66,*)'SENS1(1,I,K,IPAN)=',SENS1(1,I,K,IPAN)
c  	WRITE(66,*)'SENS1(5,I,K,IPAN)=',SENS1(5,I,K,IPAN)
c  	WRITE(66,*)'EFF(IJ+4335)=',EFF(IJ+4335)
c  	WRITE(66,*)'EFF(IJ+4080)=',EFF(IJ+4080)
c  	WRITE(66,*)'EFF(IJ+4845)=',EFF(IJ+4845)
c  	WRITE(66,*)'SENS2(1,I,K,IPAN)=',SENS2(1,I,K,IPAN)
c  	WRITE(66,*)'SENS3(1,I,K,IPAN)=',SENS3(1,I,K,IPAN)
c  	WRITE(66,*)'SENS3(2,I,K,IPAN)=',SENS3(2,I,K,IPAN)

      SENS3(1,I,K,IPAN)= SENS3(1,I,K,IPAN)+     != DSX= d(SIGMAX)/dx
     *    SP* SN * ( - LAMB * SENS1(5,I,K,IPAN)							! DSX= d(SIGMAX)/dx
     *               + ETA  * (SENS1(8,I,K,IPAN)+SENS1(1,I,K,IPAN)/Q)  )
      SENS3(2,I,K,IPAN)= SENS3(2,I,K,IPAN) +    !=DSY
     *    SP* SN * ( SENS1(8,I,K,IPAN)									! DSY= d(SIGMAY)/dx
     *              - LAMB*ETA*SENS1(5,I,K,IPAN) + SENS1(1,I,K,IPAN)/Q )  
	IF (NZ==JJK) THEN
	 DEN=EFF(IJ+4845)
	 IF(DABS(DEN).LE.(+1.E-10)) DEN =1.E-10
       SENS2(1,I,K,IPAN)=
     *    0.5*( (2.*EFF(IJ+4335)-EFF(IJ+4080))*SENS3(1,I,K,IPAN) +			! d(SIGMA COMP)
     *          (2.*EFF(IJ+4080)-EFF(IJ+4335))*SENS3(2,I,K,IPAN)   )/DEN
      ENDIF
	

c  	WRITE(66,*)'new calculation'
c  	WRITE(66,*)'SENS3(1,I,K,IPAN)=',SENS3(1,I,K,IPAN)
c  	WRITE(66,*)'SENS3(2,I,K,IPAN)=',SENS3(2,I,K,IPAN)
c  	WRITE(66,*)'SENS2(1,I,K,IPAN)=',SENS2(1,I,K,IPAN)


  380 CONTINUE

C  2- CONTRAINTE ds BORDAGE en Z=0 et X=0  (SigmaX=0,SigmaY=0,Txy,SIGMA Comp) 
C  --------------------------------------------------------------------------

c 	WRITE(66,*)'2 CONTRAINTE Tau, BORDAGE en Z=0 et X=0 '
      DO 381 I=1,IPT                                             		
	IJ= IPTS2(I,NEL)

c 	WRITE(66,*)'** 2 CONTRAINTE Tau, BORDAGE en Z=0 et X=0 '
c 	WRITE(66,*)'IPT=',I
c 	WRITE(66,*)'LAMB=',LAMB
c 	WRITE(66,*)'SENS1(6,I,K,IPAN)=',SENS1(6,I,K,IPAN)
c  	WRITE(66,*)'SENS1(7,I,K,IPAN)=',SENS1(7,I,K,IPAN)
c  	WRITE(66,*)'EFF(IJ+4590)=',EFF(IJ+4590)
c  	WRITE(66,*)'EFF(IJ+4845)=',EFF(IJ+4845)
c  	WRITE(66,*)'SENS2(2,I,K,IPAN)=',SENS2(2,I,K,IPAN)

      SENS2(2,I,K,IPAN)=  SENS2(2,I,K,IPAN) +
     *     SQ* SH * ( SENS1(6,I,K,IPAN) + LAMB  * SENS1(7,I,K,IPAN)  )	! DTXY=d(TXY)

	IF (NZ==JJK) THEN
       DEN=EFF(IJ+4590)
       IF(DABS(DEN).LE.(+1.E-10)) DEN =1.E-10
       SENS2(2,I,K,IPAN)=1.732*SENS2(2,I,K,IPAN) *DEN/DABS(DEN)			! d(SIGMA COMP)
      ENDIF

c  	WRITE(66,*)'SENS2(2,I,K,IPAN)=',SENS2(2,I,K,IPAN)


  381 CONTINUE

C  3- CONTRAINTE ds BORDAGE en Z=-d/2 et X=L/2  (SigmaX,SigmaY,Txy=0,SIGMA Comp) 
C  --------------------------------------------------------------------------

c 	WRITE(66,*)'3 CONTRAINTE ds BORDAGE en Z=-d/2 et X=L/2  '
      DO 382 I=1,IPT                                             		
	IP=IPTS2(I,NEL)
	IJ=4*51+IP

c      DSX=SN * ( - LAMB * SENS1(5,I,K,IPAN)                            ! SIGMAX
c     *           + ETA  * (SENS1(8,I,K,IPAN)+SENS1(1,I,K,IPAN)/Q)
c     * +(DELTA/2.)*( ETA*SENS1(3,I,K,IPAN)-LAMB2*SENS1(1,I,K,IPAN)) 
c     * - DC(1,K,1)*( ETA*Z2(IP+1734)-LAMB2*Z2(IP+102))  ) 
c      DSY= SN * ( SENS1(8,I,K,IPAN)                                    ! SIGMAY
c     *            - LAMB*ETA*SENS1(5,I,K,IPAN) + SENS1(1,I,K,IPAN)/Q  
c     * +(DELTA/2.)*( SENS1(3,I,K,IPAN)-ETA*LAMB2*SENS1(1,I,K,IPAN))   
c     * - DC(1,K,1)*( Z2(IP+1734)-ETA*LAMB2*Z2(IP+102))  ) 
c	DEN=EFF2(IJ+1785)
c	IF(DABS(DEN).LE.(+1.E-10)) DEN =1.E-10
c      SENS2(3,I,K,IPAN) =0.5 * ( (2.*EFF2(IJ+1275)-EFF2(IJ+1020))*DSX ! SIGMA COMP
c     *                  + (2.*EFF2(IJ+1020)-EFF2(IJ+1275))*DSY )/DEN

c 	WRITE(66,*)'IPT=',I,' DSX=',DSX,' DSY=',DSY

      SENS3(3,I,K,IPAN)= SENS3(3,I,K,IPAN) +							! d(SIGMAX)/dx
     *  SP* SN*( - LAMB * SENS1(5,I,K,IPAN) 
     *           + ETA  * (SENS1(8,I,K,IPAN)+SENS1(1,I,K,IPAN)/Q)
     * +(DELTA/2.)*( ETA*SENS1(3,I,K,IPAN)-LAMB2*SENS1(1,I,K,IPAN)) 
     * - DC(1,K,1)*( ETA*Z2(IP+1734)-LAMB2*Z2(IP+102))  ) 
      SENS3(4,I,K,IPAN)= SENS3(4,I,K,IPAN) +							 ! d(SIGMAY)/dx
     *   SP* SN*( SENS1(8,I,K,IPAN) 
     *         - LAMB*ETA*SENS1(5,I,K,IPAN) + SENS1(1,I,K,IPAN)/Q  
     * +(DELTA/2.)*( SENS1(3,I,K,IPAN)-ETA*LAMB2*SENS1(1,I,K,IPAN))   
     * - DC(1,K,1)*( Z2(IP+1734)-ETA*LAMB2*Z2(IP+102))  ) 

      IF(NZ==JJK) THEN
	 DEN=EFF2(IJ+1785)
	 IF(DABS(DEN).LE.(+1.E-10)) DEN =1.E-10
       SENS2(3,I,K,IPAN)=												! d(SIGMA COMP)/dx
     *   0.5*( (2.*EFF2(IJ+1275)-EFF2(IJ+1020))*SENS3(3,I,K,IPAN)
     *        +(2.*EFF2(IJ+1020)-EFF2(IJ+1275))*SENS3(4,I,K,IPAN) )/DEN
      ENDIF

  382 CONTINUE

c	ENDIF

C  4- CONTRAINTE ds BORDAGE en Z=+d/2 et X=L/2  (SigmaX,SigmaY,Txy=0,SIGMA Comp) 
C  --------------------------------------------------------------------------

c 	WRITE(66,*)'4 CONTRAINTE ds BORDAGE en Z=+d/2 et X=L/2  '
      DO 383 I=1,IPT                                             		
	IP=IPTS2(I,NEL)
	IJ=4*51+IP

c     DSX=SN * ( - LAMB * SENS1(5,I,K,IPAN)                            ! SIGMAX
c     *           + ETA  * (SENS1(8,I,K,IPAN)+SENS1(1,I,K,IPAN)/Q)
c     * -(DELTA/2.)*( ETA*SENS1(3,I,K,IPAN)-LAMB2*SENS1(1,I,K,IPAN))  
c     * - DC(1,K,2)*( ETA*Z2(IP+1734)-LAMB2*Z2(IP+102))   ) 
c      DSY= SN * ( SENS1(8,I,K,IPAN)                                    ! SIGMAY
c     *            - LAMB*ETA*SENS1(5,I,K,IPAN) + SENS1(1,I,K,IPAN)/Q  
c     * -(DELTA/2.)*( SENS1(3,I,K,IPAN)-ETA*LAMB2*SENS1(1,I,K,IPAN))  
c     * - DC(1,K,2)*( Z2(IP+1734)-ETA*LAMB2*Z2(IP+102))  ) 
c	DEN=EFF2(IJ+765)
c	IF(DABS(DEN).LE.(+1.E-10)) DEN =1.E-10
c      SENS2(4,I,K,IPAN) =0.5 * ( (2.*EFF2(IJ+255)-EFF2(IJ))*DSX +     ! SIGMA COMP
c     *                           (2.*EFF2(IJ)-EFF2(IJ+255))*DSY )/DEN

c 	WRITE(66,*)'IPT=',I,' DSX=',DSX,' DSY=',DSY

      SENS3(5,I,K,IPAN)= SENS3(5,I,K,IPAN) +                          ! d(SIGMAX)/dx
     *   SP*SN* ( - LAMB * SENS1(5,I,K,IPAN)  
     *             + ETA  * (SENS1(8,I,K,IPAN)+SENS1(1,I,K,IPAN)/Q)
     * -(DELTA/2.)*( ETA*SENS1(3,I,K,IPAN)-LAMB2*SENS1(1,I,K,IPAN))  
     * - DC(1,K,2)*( ETA*Z2(IP+1734)-LAMB2*Z2(IP+102))   ) 
      SENS3(6,I,K,IPAN)=SENS3(6,I,K,IPAN) +                            ! d(SIGMAY)
     *   SP*SN* ( SENS1(8,I,K,IPAN)
     *            - LAMB*ETA*SENS1(5,I,K,IPAN) + SENS1(1,I,K,IPAN)/Q  
     * -(DELTA/2.)*( SENS1(3,I,K,IPAN)-ETA*LAMB2*SENS1(1,I,K,IPAN))  
     * - DC(1,K,2)*( Z2(IP+1734)-ETA*LAMB2*Z2(IP+102))  ) 

      IF(NZ==JJK) THEN
	  DEN=EFF2(IJ+765)
	  IF(DABS(DEN).LE.(+1.E-10)) DEN =1.E-10
        SENS2(4,I,K,IPAN) =										  ! d(SIGMA COMP)
     *     0.5*( (2.*EFF2(IJ+255)-EFF2(IJ)    )*SENS3(5,I,K,IPAN) 
     *          +(2.*EFF2(IJ)    -EFF2(IJ+255))*SENS3(6,I,K,IPAN) )/DEN
	ENDIF

  383 CONTINUE

C  5&7- CONTRAINTE ds CADRES JAS et JAB en X=0 [Txy (du à Mxy) = b * Wø'/q] 
C  --------------------------------------------------------------------------
c 	WRITE(66,*)'5/7 CONTRAINTE ds CADRES JAS et JAB en X=0 (Txy)'
      DO 384 I=1,IPT                                             		
	IJ=IPTS2(I,NEL)
	IP=IJ

c	Seul le terme en SQ est pris en compte
      SENS2(5,I,K,IPAN)= SENS2(5,I,K,IPAN)							! d(TYZ(Mxy) JAS)dx
     *                   + SQ* SH*LAMB*DYA*SENS1(2,I,K,IPAN)
      IF((K.EQ.3).AND.(NEL.EQ.IPAN)) THEN
	 SENS2(5,I,K,IPAN)= SENS2(5,I,K,IPAN)+ SQ*SH*LAMB*Z2(IP+1581)
	ENDIF 

	IF(NZ==JJK) THEN
	 DEN=TEMP(IJ+510)  ! TEMP = TYZ(Mxy) JAS  calcule ci-avant dans RESUL
	 IF(DABS(DEN).LE.(+1.E-10)) DEN =1.E-10
       SENS2(5,I,K,IPAN) = 1.732 * SENS2(5,I,K,IPAN) * DEN/DABS(DEN)  ! d(COMP JAS)dx
       SENS2(7,I,K,IPAN) = SENS2(5,I,K,IPAN)							! d(COMP JAB)dx
	ENDIF

c 	WRITE(66,*)'IPT=',I,' SENS2(7,I,K,IPAN)=',SENS2(7,I,K,IPAN)
  384 CONTINUE

C  6&8- CONTRAINTES ds CADRES JAS et JAB en X=L/2 (Sigma Y, Txy et Scomp) 
C  --------------------------------------------------------------------------
c 	WRITE(66,*)'6/8 CONTRAINTES ds CADRES JAS et JAB en X=L/2'
      DO 385 I=1,IPT
	IP=IPTS2(I,NEL)
	IJ=4*51+IP

c     EFF(IJ+5100) = SIGMAY JAS       
C     EFF(IJ+5610) = SP * DABS(TYZ(Ry))  + SQ *DABS(Tyz(Mxy))  en JAS 
C     TEMP(IJ)     = SP * (TYZ(Ry))  en JAS 
c     SENS3(7,I,K,IPAN) = d(SIGMAY JAS)/dx  
c     SENS3(8,I,K,IPAN) = d(TYZ JAS)/dx 	seul le terme en SP sont pris en compte
      SENS3(7,I,K,IPAN)= SENS3(7,I,K,IPAN) +	SP*							!d(SIGMAY JAS)/dx 
     *   E*(SENS1(8,I,K,IPAN)+SENS1(1,I,K,IPAN)/Q-SMA*SENS1(3,I,K,IPAN)
     *                       -DC(2,K,2)*Z2(IP+1734) )
      SENS3(8,I,K,IPAN)= SENS3(8,I,K,IPAN) +	SP*E*						!d(TYZ JAS)/dx 
     *     (   TM * (SENS1(9,I,K,IPAN)+SENS1(2,I,K,IPAN)/Q)          
     *       + DC(2,K,1) * (Z2(IP+1632)+Z2(IP+1581)/Q)
     *       - TPB* SENS1(4,I,K,IPAN) -DC(2,K,3)*Z2(IP+1887)  )/DYA
	STT7 =SP *E*(TM*(Z2(IP+1632)+Z2(IP+1581)/Q) - TPB*Z2(IP+1887))/DYA	! TYZ JAS (non cumule)
c     STT7 = TEMP(IJ) =TYZ JAS  mais non cumule
      IF((K.EQ.3).AND.(NEL.EQ.IPAN)) THEN
	  SENS3(8,I,K,IPAN)= SENS3(8,I,K,IPAN) - STT7/DYA
	ENDIF

      IF(NZ==JJK) THEN
	 DEN=EFF(IJ+6120)
	 IF(DABS(DEN).LE.(+1.0E-10)) DEN =1.E-10
       SENS2(6,I,K,IPAN)=													! d(SIGMA COMP JAS)/dx (sensib)
     *  0.5* ( 2.*EFF(IJ+5100)*SENS3(7,I,K,IPAN) 
     *        +6.*TEMP(IJ)    *SENS3(8,I,K,IPAN)  )/DEN     
c 	 WRITE(66,*)' SENS2(6,I,K,IPAN)=',SENS2(6,I,K,IPAN)
      ENDIF

c     EFF(IJ+5355)      = SIGMAY JAB        
c	EFF(IJ+5865)      = SP * DABS(TYZ(Ry))  + SQ *DABS(Tyz(Mxy))  en JAB
c	TEMP(IJ+255) = TXY(JAB) = SP * TYZ(Ry) 
c     SENS3( 9,I,K,IPAN) = d(SIGMAY JAB)/dx  
C     SENS3(10,I,K,IPAN) = d(TYZ JAB)/dx 	seul le terme en SP est pris en compte
      SENS3(9,I,K,IPAN)= SENS3(9,I,K,IPAN) +	SP*							!d(SIGMAY JAB)/dx 
     *  E*(SENS1(8,I,K,IPAN)+SENS1(1,I,K,IPAN)/Q-SMB*SENS1(3,I,K,IPAN) 
     *                      -DC(3,K,2)*Z2(IP+1734)  )
      SENS3(10,I,K,IPAN)= SENS3(10,I,K,IPAN) + SP*E*						!d(TYZ JAB)/dx = d(STT8)/dx
     *    (   TMR * (SENS1(9,I,K,IPAN)+SENS1(2,I,K,IPAN)/Q)   
     *      + DC(3,K,1) * (Z2(IP+1632)+Z2(IP+1581)/Q)
     *      - TPR* SENS1(4,I,K,IPAN) - DC(3,K,3)*Z2(IP+1887)   )/DYA
	STT8=SP *E*(TMR*(Z2(IP+1632)+Z2(IP+1581)/Q) - TPR*Z2(IP+1887))/DYA	!TYZ JAB (non cumule)
c     STT8 = TEMP(IJ+255) mais non cumule
      IF((K.EQ.3).AND.(NEL.EQ.IPAN)) THEN
	  SENS3(10,I,K,IPAN)=SENS3(10,I,K,IPAN)- STT8/DYA
	ENDIF

      IF(NZ==JJK) THEN
	 DEN=EFF(IJ+6375)
	 IF(DABS(DEN).LE.(+1.E-10)) DEN =1.E-10
       SENS2(8,I,K,IPAN) =               ! d(SIGMA COMP JAB)/dx 
     *  0.5*(  2.*EFF(IJ+5355)*SENS3( 9,I,K,IPAN)  
     *       + 6.*TEMP(IJ+255)*SENS3(10,I,K,IPAN) )/DEN   
c 	 WRITE(66,*)' SENS2(8,I,K,IPAN)=',SENS2(8,I,K,IPAN)
      ENDIF

  385 CONTINUE

C  9- CONTRAINTES ds CADRES SEMELLES en X=L/2 (Sigma Y, Txy et Scomp) 
C  ---------------------------------------------------------------
c 	WRITE(66,*)'** 9 CONTR. ds CADRES SEMELLES en X=L/2'
c 	WRITE(66,*)'   Variables XI=',K,' I PAN=',IPAN

      DO 386 I=1,IPT
	IP=IPTS2(I,NEL)
	IJ=4*51+IP

c     EFF(IJ+6630)   = SIGMAY SEM        
c	EFF(IJ+6885)   = SP * DABS(TYZ(1)) + SQ*DABS(TYZ(2)) + SQ* DABS(TYZ(3)) Semelle
c	TEMP(IJ+765)   = TYZ(Ry) Sem     Cumule
c                    = SP*E*(TM*(Z2(IP+1632)+Z2(IP+1581)/Q)/2.-TPB*Z2(IP+1887)/2.)/TYA
      SENS3(11,I,K,IPAN)= SENS3(11,I,K,IPAN) + SP * E *					! d(SIGMAY Sem)/dx
     *  ( SENS1(8,I,K,IPAN)+SENS1(1,I,K,IPAN)/Q-SM*SENS1(3,I,K,IPAN)
     *   -DC(2,K,2)*Z2(IP+1734)  )
      SENS3(12,I,K,IPAN)= SENS3(12,I,K,IPAN) +  SP * 0.5*E* 				! d(TYZ(Ry) Sem)/dx             
     *   (  TM * (SENS1(9,I,K,IPAN)+SENS1(2,I,K,IPAN)/Q)       
     *    + DC(2,K,1) * (Z2(IP+1632)+Z2(IP+1581)/Q)
     *    - TPB* SENS1(4,I,K,IPAN) - DC(2,K,3)*Z2(IP+1887)  )/TYA

      IF(NZ==JJK) THEN
	  DEN=EFF(IJ+7140)
	  IF(DABS(DEN).LE.(+1.E-10)) DEN =1.E-10
        SENS2(9,I,K,IPAN) =         ! d(SIGMA COMP Sem)/dx 
     *    0.5*( 2.*EFF(IJ+6630)*SENS3(11,I,K,IPAN) 
     *         +6.*TEMP(IJ+765)*SENS3(12,I,K,IPAN))/DEN     
      ENDIF

c 	WRITE(66,*)'   -- POINT=',I
c	WRITE(66,*)'      SP=',SP,'  SP=',SP
c	WRITE(66,*)'      EFF(IJ+6885)=',EFF(IJ+6885)
c	WRITE(66,*)'      EFF(IJ+7140)=',EFF(IJ+7140)
c	WRITE(66,*)'      TEMP(IJ+765)=',TEMP(IJ+765)
c	WRITE(66,*)'      SENS3(11,I,K,IPAN)=',SENS3(11,I,K,IPAN)
c	WRITE(66,*)'      SENS3(12,I,K,IPAN)=',SENS3(12,I,K,IPAN)
c	WRITE(66,*)'      - SENS1(9,I,K,IPAN) =',SENS1(9,I,K,IPAN)
c	WRITE(66,*)'      - SENS1(2,I,K,IPAN) =',SENS1(2,I,K,IPAN)
c	WRITE(66,*)'      - SENS1(4,I,K,IPAN) =',SENS1(4,I,K,IPAN)
c	WRITE(66,*)'      - Z2(IP+1632) =',Z2(IP+1632)
c	WRITE(66,*)'      - Z2(IP+1581) =',Z2(IP+1581)
c	WRITE(66,*)'      - Z2(IP+1887) =',Z2(IP+1887)
c	WRITE(66,*)'      - DC(2,K,1) =',DC(2,K,1)
c	WRITE(66,*)'      - DC(2,K,3) =',DC(2,K,3)
c	WRITE(66,*)'      - TM =',TM,' TPB =',TPB
c	WRITE(66,*)'      SENS2(9,I,K,IPAN)=',SENS2(9,I,K,IPAN)

  386 CONTINUE

C  10- CONTRAINTES ds RAIDISSEURS JAB en X=L/2 (Sigma X)       !avril2003 comme tout le point 10!!!
C  -----------------------------------------------------

      DO 389 I=1,IPT
	IP=IPTS2(I,NEL)
	IJ=4*51+IP

      SENS2(10,I,K,IPAN)= SENS2(10,I,K,IPAN) + SP* E*LAMB*		    ! d(SIGMAX JAB)/dx (=sensib)
     *     ( - SENS1(5,I,K,IPAN)                                 
     *       + LAMB*(SMBR*SENS1(1,I,K,IPAN)+DC(6,K,1)*Z2(IP+102)) )

	IF (NZ==JJK) THEN
	  DEN=EFF(IJ+7650) ! = Sx jab raid
	  IF(DABS(DEN).LE.(+1.E-10)) DEN=1.E-10
        SENS2(10,I,K,IPAN)= SENS2(10,I,K,IPAN)*DEN/DABS(DEN)      ! d(SIGMA COMP JAB) (=sensib)
      ENDIF

  389 CONTINUE



C   11- CONTRAINTES ds RAIDISSEURS JAB en X=0 (Txy )           !avril2003 ainsi que les !!!
C  -------------------------------------------------
c 	WRITE(66,*)'10/11 CONTRAINTES ds RAIDISSEURS JAS et JAB en X=0'
      DO 387 I=1,IPT
	IP=IPTS2(I,NEL)
	IJ=IP

c     TEMP2(IJ+255)      =  TYZ(Rx)JAS      
c     SENS3(13,I,K,IPAN)= d(TYZ(Rx)JAS)/dx  
!!!      SENS3(13,I,K,IPAN)= SENS3(13,I,K,IPAN) - SQ * LAMB2*E*				! d(TYZ(Rx)JAS)/dx = d(STT1)/dx
!!!     *    ( SENS1(5,I,K,IPAN)*TMRX + Z2(IP+51)*DC(4,K,1) 
!!!     *     -LAMB*(TPBR*SENS1(1,I,K,IPAN) + DC(4,K,2)*Z2(IP+102)) )/DXR 
c     STT1= TYZ (Rx) JAS  idem TEMP2(IJ+255) mais sans cumuler les termes
!!!      STT1=-SQ* LAMB2*E* ( Z2(IP+51)*TMRX-LAMB*TPBR*Z2(IP+102) ) /DXR		! TYZ (Rx) JAS non cumule
!!!      IF((K.EQ.7).AND.(NEL.EQ.IPAN)) THEN
!!!       SENS3(13,I,K,IPAN)= SENS3(13,I,K,IPAN) -  STT1/DXR
!!!	ENDIF

C     TEMP2(IJ+0)       =   TYZ(Mxy)JAS 
C     SENS3(14,I,K,IPAN)= d(TYZ(Mxy)JAS)/dx 
!!!      SENS3(14,I,K,IPAN)= SENS3(14,I,K,IPAN)								!d(TYZ(Mxy)JAS)/dx 
!!!     *                    + SH*DXR*LAMB * SENS1(2,I,K,IPAN)                    
!!!      IF((K.EQ.7).AND.(NEL.EQ.IPAN)) THEN
!!!	 SENS3(14,I,K,IPAN)= SENS3(14,I,K,IPAN) + SQ* SH*LAMB*Z2(IP+1581)	!d(TYZ(Mxy)JAS)/dx 
!!!	ENDIF

!!!      IF(NZ==JJK) THEN
!!!	 DEN =TEMP2(IJ+255) !TYZ(Rx)JAS
!!!	 IF(DABS(DEN).LE.(+1.E-10))  DEN =1.E-10
!!!	 DEN2=TEMP2(IJ)		!TYZ(Mxy)JAS 
!!!	 IF(DABS(DEN2).LE.(+1.E-10)) DEN2=1.E-10
!!!       SENS2(10,I,K,IPAN)=  SENS3(13,I,K,IPAN) * DEN /DABS(DEN)  ! d(DABS(TYZ(Rx))+DABS(TYX(Mxy))JAS)/dx
!!!     *                    + SENS3(14,I,K,IPAN) * DEN2/DABS(DEN2)    
!!!       SENS2(10,I,K,IPAN)=1.732051*SENS2(10,I,K,IPAN)                  ! d(SIGMA COMP JAS )/dx
!!!      ENDIF



c     TEMP2(IJ+510)     = TYZ(Rx) JAB       
c     SENS3(15,I,K,IPAN)= d(TYZ(Rx)JAB)/dx  
      SENS3(15,I,K,IPAN)= SENS3(15,I,K,IPAN) - SQ* LAMB2*E*			 ! d(TYZ(Rx)JAB)/dx
     *    (  SENS1(5,I,K,IPAN)*TMRR + Z2(IP+51)*DC(5,K,1)           
     *     - LAMB*(TPRR*SENS1(1,I,K,IPAN) + DC(5,K,2)*Z2(IP+102)) ) /DXR 
    ! STT2= TYZ (Rx) JAB  idem TEMP2(IJ+510) mais sans cumuler les termes
      STT2=-SQ * LAMB2*E* ( Z2(IP+51)*TMRR-LAMB*TPRR*Z2(IP+102) ) /DXR ! TYZ (Rx) JAB (contraintes)
      IF((K.EQ.7).AND.(NEL.EQ.IPAN)) THEN
	 SENS3(15,I,K,IPAN)= SENS3(15,I,K,IPAN) -  STT2 /DXR            
	ENDIF                                                   

C     TEMP2(IJ)         =   TYZ(Mxy)JAB      =  TYZ(Mxy)JAS
c     SENS3(16,I,K,IPAN)= d(TYZ(Mxy)JAB)/dx 
      SENS3(16,I,K,IPAN)= SENS3(16,I,K,IPAN)								!d(TYZ(Mxy)JAB)/dx  !avril2003 
     *                    + SH*LAMB*DXR* SENS1(2,I,K,IPAN)                                   !avril2003
      IF((K.EQ.7).AND.(NEL.EQ.IPAN)) THEN
	 SENS3(16,I,K,IPAN)= SENS3(16,I,K,IPAN) + SQ* SH*LAMB*Z2(IP+1581)	!d(TYZ(Mxy)JAB)/dx  !avril2003
	ENDIF
	
	IF(NZ==JJK) THEN
	  DEN =TEMP2(IJ+510)
	  IF(DABS(DEN).LE.(+1.E-10)) DEN =1.E-10
	  DEN2=TEMP2(IJ)
	  IF(DABS(DEN2).LE.(+1.E-10)) DEN2 =1.E-10
        SENS2(11,I,K,IPAN)=  SENS3(15,I,K,IPAN) * DEN /DABS(DEN)	! d(DABS(TYZ(Rx))+DABS(TYX(Mxy))JAB)/dx
     *                     + SENS3(16,I,K,IPAN) * DEN2/DABS(DEN2)      
        SENS2(11,I,K,IPAN) =1.732051*SENS2(11,I,K,IPAN)				! d(SIGMA COMP JAB)/dx 
      ENDIF

c 	WRITE(66,*)' IPT=',I,' DTXY=',DTXY, 'pour JAB'
  387 CONTINUE



C  12- CONTRAINTES ds RAIDISSEURS SEM en X=L/2 (Sigma X) 
C  -------------------------------------------------------------------
c 	WRITE(66,*)'12 CONTR. ds RAIDISSEURS SEMELLES en X=L/2'
      DO 388 I=1,IPT
	IP=IPTS2(I,NEL)
	IJ=4*51+IP

      SENS2(12,I,K,IPAN)= SENS2(12,I,K,IPAN) + SP* E*LAMB*		    ! d(SIGMAX Sem)/dx (=sensib)
     *     ( - SENS1(5,I,K,IPAN)                                 
     *       + LAMB*(SMR*SENS1(1,I,K,IPAN)+DC(6,K,1)*Z2(IP+102)) )

	IF (NZ==JJK) THEN
	  DEN=EFF(IJ+7905) ! = Sx sem raid
	  IF(DABS(DEN).LE.(+1.E-10)) DEN=1.E-10
        SENS2(12,I,K,IPAN)= SENS2(12,I,K,IPAN)*DEN/DABS(DEN)          ! d(SIGMA COMP Sem) (=sensib)
      ENDIF

c  	WRITE(66,*)'SENS2(12,I,K,IPAN)=',SENS2(12,I,K,IPAN)

  388 CONTINUE

C  BOUCLE (391) SUR LES TRAVERSES
C  ==============================
      DERIV=0.
      IF(MT.EQ.0) GOTO 392	

      DO 391 I=1,IPT
	 IT=IPTS3(I,NEL) ! No de la traverse concernee
c	 WRITE(9,*) 'IPT=', I, 'NEL=',NEL, 'IPTS3=IT=', IT
       IF(IT.EQ.0) GOTO 391	

      WYT=WXTR(IT)                                          ! Larg.   sem
      TYT=TXTR(IT)                                          ! Epaiss. sem
      HYT=HXTR(IT)                                          ! Haut.   âme
      DYT=DXTR(IT)                                          ! Epaiss. âme
      TMT=WYT*TYT                                                       
      TMRT=TMT+DYT*HYT
      TPBT=TMT*(DELTA/2.+HYT+0.5*TYT)*KSTT
      TPRT=TPBT+DYT*HYT*(DELTA+HYT)*KSTT/2.

C  13&14- CONTRAINTES ds TRAVERSES JAS et JAB en X=0 (Txy ) 
C  ---------------------------------------------------------------
c 	WRITE(66,*)'13/14 CONTRAINTES ds TRAVERSES JAS et JAB en X=0'
	IJ=IT ! car x=0
c     TEMP3(IJ+150)     =   TYZ(Rx) JAS     
c     TEMP3(IJ+250)     =   TYZ(Mxy)JAS 
c     SENS3(17,I,K,IPAN)= d(TYZ(Rx)JAS)/dx 
c     SENS3(18,I,K,IPAN)= d(TYZ(Mxy)JAS)/dx 
      IF(K.EQ.1) DERIV=TMT*KSTT/2.
c     TYZ(Rx)=-SQ*LAMB2*E*(Z2(2*IT+81)*TMT-LAMB*TPBT*Z2(2*IT+132))/DYT 
      SENS3(17,I,K,IPAN)=SENS3(17,I,K,IPAN) - SQ* LAMB2*E*			! d(TYZ(Rx) JAS)dx
     *      ( SENS1(5,I,K,IPAN)*TMT -                      
     *        LAMB*(TPBT*SENS1(1,I,K,IPAN)+DERIV*Z2(2*IT+132))  ) /DYT 
c     TYZ(Mxy)= SQ* SH*DYT*LAMB*Z2(2*IT+1611)							! TYZ (Mxy) JAS 
      SENS3(18,I,K,IPAN)= SENS3(18,I,K,IPAN)							! d(TYZ(Mxy) JAS)/dx
     *                  + SQ* SH*DYT*LAMB * SENS1(2,I,K,IPAN)	
      
	IF (NZ==JJK) THEN
	  DEN =TEMP3(IJ+150)
	  IF(DABS(DEN).LE.(+1.E-10)) DEN =1.E-10
	  DEN2=TEMP3(IJ+250)
	  IF(DABS(DEN2).LE.(+1.E-10)) DEN2 =1.E-10
        SENS2(13,I,K,IPAN)=   SENS3(17,I,K,IPAN) * DEN /DABS(DEN) 
     *                      + SENS3(18,I,K,IPAN) * DEN2/DABS(DEN2)	! d(ABS[TYZ(Rx)]+ABS[TYZ(Mxy)]JAS)/dx 
        SENS2(13,I,K,IPAN) = 1.732051*SENS2(13,I,K,IPAN)              ! d(SIGMA COMP JAS)/dx 
      ENDIF


c     TEMP3(IJ+200)     =   TYZ(Rx) JAB     
c     TEMP3(IJ+250)     =   TYZ(Mxy)JAB 
c     SENS3(17,I,K,IPAN)= d(TYZ(Rx)JAB)/dx  
c     SENS3(18,I,K,IPAN)= d(TYZ(Mxy)JAB)/dx 
	IF(K.EQ.1) DERIV=TMRT*KSTT/2.
c     TYZ(Rx)=-SQ* LAMB2*E*(Z2(2*IT+81)*TMRT-LAMB*TPRT*Z2(2*IT+132))/DYT 
      SENS3(19,I,K,IPAN)=SENS3(19,I,K,IPAN)  - SQ* LAMB2*E*			!  d(TYZ(Rx)JAB)/dx
     *      (  SENS1(5,I,K,IPAN)*TMRT              
     *       - LAMB*(TPRT*SENS1(1,I,K,IPAN)+DERIV*Z2(2*IT+132))  ) /DYT 
c     TYZ(Mxy) JAB = TYZ(Mxy) JAS                                     !   TYZ(Mxy) JAB 
      SENS3(20,I,K,IPAN)=SENS3(18,I,K,IPAN)							! d(TYZ(Mxy) JAB)/dx

	IF (NZ==JJK) THEN
	  DEN =TEMP3(IJ+200)
	  IF(DABS(DEN).LE.(+1.E-10)) DEN =1.E-10
	  DEN2=TEMP3(IJ+250)
	  IF(DABS(DEN2).LE.(+1.E-10)) DEN2 =1.E-10
        SENS2(14,I,K,IPAN)=   SENS3(19,I,K,IPAN) * DEN /DABS(DEN) 
     *                      + SENS3(20,I,K,IPAN) * DEN2/DABS(DEN2)	! d(ABS[TYZ(Rx)]+ABS[TYZ(Mxy)]JAB)/dx 
        SENS2(14,I,K,IPAN) = 1.732051*SENS2(14,I,K,IPAN)              ! d(SIGMA COMP JAB)/dx 
      ENDIF

C  15- CONTRAINTES ds TRAVERSES SEM en X=L/2 (Sigma X) 
C  -------------------------------------------------------------------
c 	WRITE(66,*)'15 CONTR. ds TRAVERSES SEMELLES en X=L/2'

	IF(K.EQ.1) DERIV=KSTT/2.
	IJ=40+IT

      SENS2(15,I,K,IPAN) = SENS2(15,I,K,IPAN) +
     *   SP* E*LAMB*(-SENS1(5,I,K,IPAN)                               ! d(SIGMAY Sem)/dx (Sensib)
     *            +LAMB*(SMT*SENS1(1,I,K,IPAN)+DERIV*Z2(2*IT+132)) )

	IF (NZ==JJK) THEN
	  DEN=CONC(IJ+300)												! CONC(IJ+300) = SIGMAY Sem trav
	  IF(DABS(DEN).LE.(+1.E-10)) DEN =1.E-10
        SENS2(15,I,K,IPAN)=SENS2(15,I,K,IPAN) *DEN/DABS(DEN)			! d(SIGMA COMP Sem)/dx  (Sensib)
      ENDIF

c 	WRITE(66,*)'15. CONTR. ds TRAVERSES SEMELLES en X=L/2'
c 	WRITE(66,*)'   -- POINT= ',I, '  VAR= ',K,'  Panneau=',IPAN
c 	WRITE(66,*)'   --  IJ ',IJ, '  IT=',IT
c	WRITE(66,*)'      SP=',SP
c	WRITE(66,*)'      CONC(IJ+300)=',CONC(IJ+300)
c	WRITE(66,*)'      - SENS1(1,I,K,IPAN) =',SENS1(1,I,K,IPAN)
c	WRITE(66,*)'      - SENS1(5,I,K,IPAN) =',SENS1(5,I,K,IPAN)
c	WRITE(66,*)'      - Z2(2*IT+132)      =',Z2(2*IT+132)
c	WRITE(66,*)'      SENS2(15,I,K,IPAN)  =',SENS2(15,I,K,IPAN)

  391 CONTINUE ! Boucle sur IPT (les pts de calcul des sensibilites sur le panneau)
  392 CONTINUE ! si pas de traverse

C  16'- CONTRAINTES ds RAIDISSEURS JAS en X=L/2 (Sigma X)    !avril2003 comme tout le point 16'!!!
C  ------------------------------------------------------

      DO 393 I=1,IPT
	IP=IPTS2(I,NEL)
	IJ=4*51+IP

      SENS2(16,I,K,IPAN)= SENS2(16,I,K,IPAN) + SP* E*LAMB*		    ! d(SIGMAX JAS)/dx (=sensib)
     *     ( - SENS1(5,I,K,IPAN)                                 
     *       + LAMB*(SMAR*SENS1(1,I,K,IPAN)+DC(6,K,1)*Z2(IP+102)) )

	IF (NZ==JJK) THEN
	  DEN=EFF(IJ+7395) ! = Sx jas raid
	  IF(DABS(DEN).LE.(+1.E-10)) DEN=1.E-10
        SENS2(16,I,K,IPAN)= SENS2(16,I,K,IPAN)*DEN/DABS(DEN)          ! d(SIGMA COMP JAS) (=sensib)
      ENDIF

  393 CONTINUE
 
C  16- CONTRAINTE  Sx Moyenne dans borde + RAIDISSEURS  en X=L/2 (Sigma X) 
C  -------------------------------------------------------------------
c 	WRITE(66,*)'16 CONTR. Sx Moyenne en X=L/2'
C     Pour le calcul de la restriction de PAIK (cfr Subr. SENS4 dans Subr CONTR)
       ! Resultat dans SENS3(21,..) et pas dans SENS2
	 ! car il ne s'agit pas d'une contrainte de comparaison
c       SXM(IPT) =   Sx Moyen  = contrainte moyenne (plaque et raid inclus) 
C                    est calcule ci-avant
c     d(SXM)/dx  = d(Sx Moyen)/dx  = SENS3(21,I,K,IPAN) 

C      SIG(moyen) = Nx/section           cfr Nx (axial):
C                 = AA U + BB V° + CC W
  !   Nx = (D+Ox) dU/dx   + (D ETA/Q) dV/dy + (D ETA/Q) W  - Hx  W''  (avec W' = dW/dx)
  !   Nx =-(D+Ox)LAMB * U + (D ETA/Q) V°    + (D ETA/Q) W  + Hx  LAMB2 W
  !     !!! comme U est en COS, on a U'(x=L/2) = - Lamb U(x=0)
C     avec
C       TMRR= section raidisseur
C       OMT = section raidisseur + largeur effective (EPSR)
C       HX  = moment statique

c    Deja calcule ci-avant:
c       SN=E/(1.-ETA*ETA)  
c       KSRR=(-1)**KSR 
c	  LAMB2=LAMB*LAMB
c       TMRR = Section raid (ame + sem)
c       OMT=TMRR +EPSR*DELTA     ! section raid + bordé
c	  OMT2=OMT*OMT
c	  COF1=SN*ETA	
c       AA=-LAMB*(SN*DELTA*EPSR+E*TMRR)/OMT   ! a/section
c       BB=COF1*DELTA*EPSR/OMT				! b/section
c	  CC=BB/Q+KSRR*LAMB2*E*HX/OMT			! c/section
c       SXM(I)=SXM(I) + AA*Z2(IP+51) + BB* Z2(IP+1480) + CC *Z2(IP+102)

      IF(IPAN.EQ.NEL) THEN  
          IF(K.EQ.1) THEN                        ! XI=DELTA
           R1=EPSR*TMRR/OMT2
	     DBB(K)=R1*COF1
	     DAA(K)=-LAMB*(SN-E)*R1
	     DCC(K)=DBB(K)/Q+KSRR*LAMB2*E*(0.5*OMT*TMRR-HX*EPSR)/OMT2
          ELSE IF (K.EQ.6) THEN                  ! XI=HXR
	     R1=-DELTA*EPSR*DXR/OMT2
	     DBB(K)=R1*COF1
	     DAA(K)=-LAMB*(SN-E)*R1
	     DCC(K)=DBB(K)/Q
     *             +KSRR*LAMB2*E*(OMT*(TMRR+DXR*DELTA/2.)-HX*DXR)/OMT2
          ELSE IF (K.EQ.7) THEN                  ! XI=DXR=Tweb
	     R1=-DELTA*EPSR*HXR/OMT2
	     DBB(K)=R1*COF1
	     DAA(K)=-LAMB*(SN-E)*R1
	     DCC(K)=DBB(K)/Q+KSRR*LAMB2*E*HXR*(OMT*DABS(SMDR)-HX)/OMT2
          ELSE IF (K.EQ.8) THEN                  ! XI=WXR =Lsem.
	     R1=-DELTA*EPSR*TXR/OMT2
	     DBB(K)=R1*COF1
	     DAA(K)=-LAMB*(SN-E)*R1
	     DCC(K)=DBB(K)/Q+KSRR*LAMB2*E*TXR*(OMT*DABS(SMCR)-HX)/OMT2
          ELSE IF (K.EQ.9) THEN                  ! XI=EPSR
	     R1=DELTA*TMRR/OMT2
	     DBB(K)=R1*COF1
	     DAA(K)=-LAMB*(SN-E)*R1
	     DCC(K)=DBB(K)/Q-KSRR*LAMB2*E*HX*DELTA/OMT2
	    ELSE
	     DBB(K)=0.
	     DAA(K)=0.
	     DCC(K)=0.
	    ENDIF
c	    WRITE(666,*)'Subr RESULT'
c	    WRITE(666,*)'----------  K = ',K
c	    WRITE(666,*)'AA,BB,CC   = ',AA,BB,CC	
c	    WRITE(666,*)'DAA,DBB,DCC= ',DAA(K),DBB(K),DCC(K)	
	ENDIF

      DO I=1,IPT
	  IP=IPTS2(I,NEL)

c	  WRITE(666,*)'== IP  = ',IP
c	  WRITE(666,*)'Z2(IP+51)    U= ',Z2(IP+51)	
c	  WRITE(666,*)'Z2(IP+1479) DV= ',Z2(IP+1479)	
c	  WRITE(666,*)'Z2(IP+102)   W= ',Z2(IP+102)	
c	  WRITE(666,*)'SENS1(5,I,K,IPAN)  U= ',SENS1(5,I,K,IPAN)	
c	  WRITE(666,*)'SENS1(8,I,K,IPAN) DV= ',SENS1(8,I,K,IPAN)
c	  WRITE(666,*)'SENS1(1,I,K,IPAN)  W= ',SENS1(1,I,K,IPAN)

c       ! 1er partie de   d(Sx Moyen)/dx 
        SENS3(21,I,K,IPAN)= SENS3(21,I,K,IPAN) + SPP* 		! d(Sx Moyen)/dx 
     *				   (  AA*SENS1(5,I,K,IPAN)   
     *					+ BB*SENS1(8,I,K,IPAN) 
     *					+ CC*SENS1(1,I,K,IPAN) )
       ! 2eme partie de   d(Sx Moyen)/dx 
        IF(IPAN.EQ.NEL) THEN
          SENS3(21,I,K,IPAN)= SENS3(21,I,K,IPAN) + SPP* 		 ! d(Sx Moyen)/dx suite
     *                  (   DAA(K)*Z2(IP+51) 
     *                    + DBB(K)*Z2(IP+1479) 
     *                    + DCC(K)*Z2(IP+102)     )    
        ENDIF

c   	  WRITE(666,*)'SENS3(21,I,K,IPAN)=',SENS3(21,I,K,IPAN)
	ENDDO
     
   
  283 CONTINUE ! Boucle sur KK=1,NBRXI                                           		
  284 CONTINUE ! Boucle sur les panneaux IPAN (liste des variables de conception)
  
C ******************************************************************************
C ******************************************************************************
	
	IF ((NZ==JJK).and.(IPRINT.GE.1)) THEN
	 WRITE(66,*) ' SENSIBILITES (contraintes) '
       WRITE(66,*) ' ************************** '

       DO 281 IPAN=1,NETO
	   NBRXI=NVAR(IPAN)
       DO 281 KK=1,NBRXI
         K=NXIT(KK,IPAN)
         WRITE(66,288) K,IPAN
         WRITE(66,294) 
         DO 290 J=1,12
  290    WRITE(66,282) NOM2(J),(SENS2(J,I,K,IPAN),I=1,IPT)

         IF(MT.GE.1) THEN
           DO 292 J=13,15
  292      WRITE(66,282) NOM2(J),(SENS2(J,I,K,IPAN),I=1,IPT)
         ENDIF

         WRITE(66,282) 'Contr. Sx moyenne-Raid',
     *                  (SENS3(21,I,K,IPAN),I=1,IPT)
   
  281  CONTINUE
      ENDIF

C     Fin de la boucle (549) relative aux calculs des sensibilités des contraintes
  549 CONTINUE
  
      RETURN 
C ******************************************************************************
C 	LES FORMATS
C 	-----------
  282 FORMAT(A18,1X,5(1X,E13.6)) !avril2003
  288 FORMAT(' Variable de conception nø',I3,' du panneau nø',I3/  !avril2003
     *         50(1H-))
  294 FORMAT('     Fct.             Pt.1          Pt.2',
     *     '          Pt.3          Pt.4          Pt.5')
      END
