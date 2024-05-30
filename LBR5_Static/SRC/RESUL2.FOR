      SUBROUTINE RESUL2(WYA,TYA,DYA,HYA,KSA,WXR,TXR,DXR,HXR,KSR,
     *     AIRE,AIy,LAMB,E,NZ,HXTR,TXTR,WXTR,DXTR,MT,KST,
     *     SIGY,EPSA,EPSR,ENTR,TORK,ISECT,EPAIS,
     *     CONST,Q,DIS,FAM,JJK,INDAIG,INDRAI,IMPR,NETO,NEL,IPRINT,
     *     EFF,EFF2,TEMP,TEMP2,TEMP3,CONC,Z2,
     *     SXM,SPK,SXMM,
     *     SENS1,SENS2,SENS3,SENS5,
     *     NVAR,NXIT,IPTS,IPTS2,IPTS3)

      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 LAMB,LAMB2
      CHARACTER *18 NOM3

      DIMENSION HXTR(10),TXTR(10),WXTR(10),DXTR(10),DIS(5),CONST(74),
     *          FAM(6),NOM3(11)
     
	DIMENSION SENS1(9,IPTmax,9,NETO),SENS2(16,IPTmax,9,NETO),
     *          SENS3(21,IPTmax,9,NETO),DC(5,5,3),
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
	DATA NOM3/'Same JAS sup I' ,'Same JAS inf I' ,'Same an I' ,
     *			'Ssemel sup I' ,'Ssemel inf I' ,'Sy sup cercle/carre',
     *			'Sy inf cercle/carre' ,'Sy an cercle/carre' ,
     *			'Ttrav JAS X=0' ,'Ttrav JAB X=0'   ,'Strav Sem X=L/2'/
c	Ordre ds SENS3(JJ,4,9,NETO) avec JJ=1 à 12
       ! 1 et 2  dSy=d(Sy/dX) sup et inf dans l'âme ou dans cercle/carré
       ! 3 et 4  dTyx=d(Tyx/dX) a.n. et JAS pour double T et sommet et a.n. pour cercle/carré
       ! 5 et 6  dSy=d(Sy/dX) sup et inf de semelle de double T 
       ! 7	   dTyx=d(Tyx/dX) semelle de double T
       !8 et9  dTxy(1) et dTxy(2)  JAS Trav en x=0 
       !10 et11  dTxy(1) et dTxy(2)  JAB Trav en x=0 
       !12       d(Sx moyen) pour la restriction de PAIK. 
C*******************************************************************************
C     SUBROUTINE RESULT2 
C     ===================
C     SUBROUTINE DE MISE EN FORME DES RESULTATS POUR 5 POINTS DE L'AXE  
C     DES X A PARTIR DU VECTEUR Z2,RESULTAT DU CALCUL SUR LES 31+20 DE  
C     L'AXE DES Y(PHI).                                                 
C     LES RESULTATS SONT CONTENUS DANS LES VECTEURS EFF,EFF2 et CONC 
C                                                                       
C    Dernière modification : J.Pirard
C	- contraintes dans les épontilles	!22-01-04
			 
C    Modif :22-01-04 				      Créer : Thèse de doctorat	
C                                                                       
C*******************************************************************************

      LAMB2=LAMB*LAMB
	DELTA=0.0000
	SH=0.0000
C CADRES/Aiguilles
      KSAA=(-1)**KSA
      TM=2*HYA*DYA						!aire âme
      TMR=HYA*DYA							!aire demi-âme
	TMS=WYA*TYA							!aire semelle
	SM=HYA+ TYA							!hauteur hors-tout de demi section
	SM2=(HYA/2)				        	!distance demi-ame
	SMC2=(HYA+TYA/2.)		    		!distance à AN			 
	SMD2=HYA		   		        	!distance JAS à AN
	SMA=HYA*DYA*DYA*DYA
	TPB2=TMS*SMC2						!moment statique semelle
	TPR2=TPB2*TMR*SM2					!moment statique section
	REX=HYA/2							!rayon ext du cercle
	RIN=(HYA-2*EPAIS)/2					!rayon int du cercle
	RTAN=(REX*REX*REX-RIN*RIN*RIN)		!diff rayon pour Ta.n.
	CM=HYA-EPAIS						!coté milieu du carre
	CIN=HYA-2*EPAIS						!coté int du carre
	S=(CM*CM)-(CIN*CIN)					!aire int à ligne médiane du carre
	S2=PI*(((CM*CM)/4)-(CIN*CIN/4))	    !aire int à ligne médiane du cercle
	ASC=((3*HYA*HYA)+(4*EPAIS*EPAIS)-(6*HYA*EPAIS))/8	!moment statique carre max/EPAIS
	ASC2= HYA*(HYA-EPAIS)/4						!moment statique carre 45/epais
C RAIDISSEURS
      KSRR=(-1)**KSR
      TMRX=WXR*TXR
      TMRR=TMRX+DXR*HXR
      SMR2=(HXR+TXR)*KSRR
      SMAR2=SMR2-TXR*KSRR
      SMBR=(DELTA/2.)*KSRR
      SMCR2=(HXR+TXR/2.)*KSRR
      SMDR2=(HXR/2.)*KSRR
      TPBR2=TMRX*SMCR2
      TPRR2=TPBR2+DXR*HXR*SMDR2
		
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
      EFF(IJ+1020)=EFF(IJ+1020)+0				  ! Nx
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


c	Section de l'épontille en double T
	IF(ISECT.EQ.3)THEN

      IF(DYA.EQ.0.)GOTO 889
        TEMP(IJ+765)=TEMP(IJ+765)+(Z2(I+1224)/AIRE)*SP						! SIGMAY compression
        EFF(IJ+4080)=EFF(IJ+4080)+((Z2(I+1224)/AIRE)
     *					+(Z2(I+1173)*HYA/AIY))*SP		! SIGMAY JASS
        EFF(IJ+4335)=EFF(IJ+4335)+((Z2(I+1224)/AIRE)
     *					-(Z2(I+1173)*HYA/AIY))*SP		! SIGMAY JASI
        TEMP(IJ)    =TEMP(IJ) + (Z2(I+1377)*SP*(TPB2/TYA))/(AIY)			! TYZ (My) JAS =T1 (à J/S)
        TEMP(IJ+255)=TEMP(IJ+255)+((Z2(I+1377)*((TPB2/DYA)+(HYA*SM2))*SP
     *					)/(AIY))												! TYZ (My) an =T1 (MAX)
       TEMP(IJ+510)=TEMP(IJ+510)+ (Z2(I+2091)*SQ*DYA)/(TORK)				    ! TYZ(Mxy) JAS = T2 (rectangle)
	  IF (NZ==JJK) THEN
        EFF(IJ+4590)=DABS(TEMP(IJ))    +DABS(TEMP(IJ+510))				! TYZ JAS = T1+T2
        EFF(IJ+4845)=DABS(TEMP(IJ+255))									! TYZ an = T1
        EFF(IJ+5100)=DSQRT(EFF(IJ+4080)**2+3.D0*(EFF(IJ+4590)**2))		! SIGMA  COMP JAS SUP
        EFF(IJ+5355)=DSQRT(EFF(IJ+4335)**2+3.D0*(EFF(IJ+4590)**2))		! SIGMA  COMP JAS INF
        EFF(IJ+5610)=DSQRT(TEMP(IJ+765)**2+3.D0*(EFF(IJ+4845)**2))		! SIGMA  COMP an
        ENDIF
 889    CONTINUE
C     CALCUL DE : SIGMAY SUP SEMELLE,SIGMAY INF SEMELLE, TXY SEM (torsion),
C				 SIGMA COMP SEMELLE CADRE
        IF(TYA.EQ.0.)GOTO 113
       EFF(IJ+5865)=EFF(IJ+5865) +((Z2(I+1224)/AIRE)
     *							+((Z2(I+1173)*SMC2)/AIY))*SP	 ! SIGMAY SUP SEMELLE
       EFF(IJ+6120)=EFF(IJ+6120) +((Z2(I+1224)/AIRE)
     *							-((Z2(I+1173)*SMC2)/AIY))*SP	 ! SIGMAY INF SEMELLE
        TEMP(IJ+1275)=TEMP(IJ+1275)+ (Z2(I+1377)*SP*(TPB2/(2*TYA))
     *					/(AIY))										 ! TXY(My) SEM = T1 (MAX SEMELLE)
C       TEMP(IJ+1020)=TEMP(IJ+1020)+									 ! TXY(??) SEM = T2
C     *                   SQ *(E/2)*1.25*FAM(1)*(Z2(I+1530)-LAMB*Z2(I)) 
        TEMP(IJ+1020)=TEMP(IJ+1020)+ ((Z2(I+2091)*SQ*TYA)/TORK)		 ! TXY(Mxy) SEM=T3 (torsion)
        IF (NZ==JJK) THEN
        EFF(IJ+6375)=DABS(TEMP(IJ+1275))+DABS(TEMP(IJ+1020)) 			! TXY SEM =T1+T2+T3
C     *                                +DABS(TEMP(IJ+1020))	 
        EFF(IJ+6630)=DSQRT(EFF(IJ+5865)**2+3.D0*(EFF(IJ+6375)**2))		! SIGMA  COMP SEM AIG.SUP
        EFF(IJ+6885)=DSQRT(EFF(IJ+6120)**2+3.D0*(EFF(IJ+6375)**2))		! SIGMA  COMP SEM AIG.INF
	  ENDIF
  113  CONTINUE

C          RAIDISSEURS (DOUBLET)
C         **************
C   CALCUL DE : SIGMAX JAS, SIGMAX JAB, SIGMAX SEMELLE,

      IF((INDRAI.EQ.0).AND.(IOPTI.EQ.0)) GOTO 112
      IF(DXR.NE.0.) THEN
       EFF(IJ+7395)=EFF(IJ+7395)+									! SIGMAX JAS
     *              E*LAMB* (-Z2(I+51)+SMAR2*LAMB*Z2(I+102))*SP
       EFF(IJ+7650)=EFF(IJ+7650)+									! SIGMAX JAB
     *              E*LAMB* (-Z2(I+51)+SMBR*LAMB*Z2(I+102))*SP
	ENDIF
      IF(TXR.NE.0.)THEN
       EFF(IJ+7905)=EFF(IJ+7905)+									! SIGMAX SEMELLE
     *              E*LAMB* (-Z2(I+51)+SMR2* LAMB*Z2(I+102))*SP       
      ENDIF 
C   CALCUL DE: TXZ JAS, TXZ JAB, Tyx SEM, 
      IF(DXR.NE.0.) THEN
       TEMP2(IJ)=TEMP2(IJ)+ SH*DXR*LAMB*Z2(I+1581) *SQ			! TXZ(Mxy)JAS = TXZ(Mxy) JAB
       TEMP2(IJ+255)=TEMP2(IJ+255)+								! TXZ(Rx) JAS
     *    (-LAMB2*E*(Z2(I+51)*TMRX-LAMB*TPBR2*Z2(I+102))/DXR) *SQ	
       TEMP2(IJ+510)=TEMP2(IJ+510)+								! TXZ(Rx) JAB
     *    (-LAMB2*E*(Z2(I+51)*TMRR-LAMB*TPRR2*Z2(I+102))/DXR) *SQ    
	ENDIF
      IF(TXR.NE.0.) THEN                                           
       TEMP2(IJ+765) =TEMP2(IJ+765) +  SH*TXR*LAMB*Z2(I+1581) * SQ		! Tyx(Rx) SEM
       TEMP2(IJ+1020)=TEMP2(IJ+1020)+       
     * (-LAMB2*E* (Z2(I+51)*TMRX/2.-LAMB*TPBR2*Z2(I+102)/2.) /TXR) * SQ	! Tyx(Mxy) SEM   
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
c	Section de l'épontille en cercle
	ELSE
		IF(isect.EQ.1)THEN
       IF(EPAIS.EQ.0.)GOTO 890
        EFF(IJ+4080)=EFF(IJ+4080)+((Z2(I+1224)/AIRE)
     *				+(Z2(I+1173)*HYA/(AIY*2)))*SP						! SIGMAY SUP
        EFF(IJ+4335)=EFF(IJ+4335)+((Z2(I+1224)/AIRE)
     *				-(Z2(I+1173)*HYA/(2*AIY)))*SP						! SIGMAY INF
        TEMP(IJ)    =0												! TYZ (My) au sommet
        TEMP(IJ+255)=TEMP(IJ+255)
     *				+ Z2(I+1377)*SP*RTAN/(EPAIS*AIY)					! TYZ (My) an =T1 (MAX)
        TEMP(IJ+510)=TEMP(IJ+510)+ (Z2(I+2091)*SQ/(2*S2*EPAIS))		! TYZ(Mxy) TORSION MAX
	  TEMP(IJ+765)=TEMP(IJ+765)+(Z2(I+1224)/AIRE)*SP					! SIGMAY COMPRESSION
        TEMP(IJ+1020)=TEMP(IJ+1020)
     *				+ Z2(I+1377)*0.707*SP*RTAN/(EPAIS*AIY)			! TYZ (My) 45° =T1 (MAX)
	  IF (NZ==JJK) THEN
        EFF(IJ+4590)=DABS(TEMP(IJ))    +DABS(TEMP(IJ+510))			! TYZ sommet = T1+T2
        EFF(IJ+4845)=DABS(TEMP(IJ+255))+DABS(TEMP(IJ+510))			!TYZ an = Torsion+T2
        EFF(IJ+5100)=DSQRT(EFF(IJ+4080)**2+3.D0*(EFF(IJ+4590)**2))	! SIGMA COMP SUP
       EFF(IJ+5355)=DSQRT(EFF(IJ+4335)**2+3.D0*(EFF(IJ+4590)**2))		! SIGMA COMP INF
	 EFF(IJ+5610)=DSQRT(TEMP(IJ+765)**2+3.D0*(EFF(IJ+4845)**2))		! SIGMA COMP an
	 ENDIF
 890    CONTINUE

c	Section de l'épontille en carré
		ELSEIF(isect.EQ.2)THEN
       IF(EPAIS.EQ.0.)GOTO 891
        EFF(IJ+4080)=EFF(IJ+4080)+((Z2(I+1224)/AIRE)
     *				+(Z2(I+1173)*HYA/(AIy*2)))*SP						! SIGMAY SUP
        EFF(IJ+4335)=EFF(IJ+4335)+((Z2(I+1224)/AIRE)
     *				-(Z2(I+1173)*HYA/(2*AIy)))*SP						! SIGMAY INF
        TEMP(IJ)    =0												! TYZ (My) au sommet
        TEMP(IJ+255)=TEMP(IJ+255)
     *				+ Z2(I+1377)*SP*ASC/(AIy)					! TYZ (My) an =T1 (MAX)
        TEMP(IJ+510)=TEMP(IJ+510)+ (Z2(I+2091)*SQ/(2*S*EPAIS))		! TYZ(Mxy) TORSION MAX
	  TEMP(IJ+765)=TEMP(IJ+765)+(Z2(I+1224)/AIRE)*SP					! SIGMAY COMPRESSION
        TEMP(IJ+1020)=TEMP(IJ+1020)
     *				+ Z2(I+1377)*0.707*SP*ASC2/(AIY)			! TYZ (My) 45° =T1 (MAX)
	  IF (NZ==JJK) THEN
        EFF(IJ+4590)=DABS(TEMP(IJ))    +DABS(TEMP(IJ+510))			! TYZ sommet = T1+T2
        EFF(IJ+4845)=DABS(TEMP(IJ+255))+DABS(TEMP(IJ+510))			!TYZ an = Torsion+T2
        EFF(IJ+5100)=DSQRT(EFF(IJ+4080)**2+3.D0*(EFF(IJ+4590)**2))	! SIGMA COMP SUP
       EFF(IJ+5355)=DSQRT(EFF(IJ+4335)**2+3.D0*(EFF(IJ+4590)**2))		! SIGMA COMP INF
	 EFF(IJ+5610)=DSQRT(TEMP(IJ+765)**2+3.D0*(EFF(IJ+4845)**2))		! SIGMA COMP an
       ENDIF
 891    CONTINUE
			ENDIF
C          RAIDISSEURS (CERCLE ET CARRE)
C         **************
C   CALCUL DE : SIGMAX JAS, SIGMAX JAB, SIGMAX SEMELLE,
      IF(DXR.NE.0.) THEN
       EFF(IJ+7395)=EFF(IJ+7395)+									! SIGMAX JAS
     *              E*LAMB* (-Z2(I+51)+SMAR2*LAMB*Z2(I+102))*SP
       EFF(IJ+7650)=EFF(IJ+7650)+									! SIGMAX an
     *              E*LAMB* (-Z2(I+51)+0*LAMB*Z2(I+102))*SP
	ENDIF
      IF(TXR.NE.0.)THEN
       EFF(IJ+7905)=EFF(IJ+7905)+									! SIGMAX SEMELLE
     *              E*LAMB* (-Z2(I+51)+SMR2* LAMB*Z2(I+102))*SP  
      ENDIF 
C   CALCUL DE: TXZ JAS, TXZ JAB, Tyx SEM, 
      IF(DXR.NE.0.) THEN
       TEMP2(IJ)=TEMP2(IJ)+ (E/2)*DXR*LAMB*Z2(I+1581) *SQ			! TXZ(Mxy)JAS = TXZ(Mxy) JAB
       TEMP2(IJ+255)=TEMP2(IJ+255)+								! TXZ(Rx) JAS
     *    (-LAMB2*E*(Z2(I+51)*TMRX-LAMB*TPBR2*Z2(I+102))/DXR) *SQ	
       TEMP2(IJ+510)=TEMP2(IJ+510)+								! TXZ(Rx) JAB
     *    (-LAMB2*E*(Z2(I+51)*TMRR-LAMB*TPRR2*Z2(I+102))/DXR) *SQ 
	ENDIF
      IF(TXR.NE.0.) THEN   
       TEMP2(IJ+765) =TEMP2(IJ+765) +  (E/2)*TXR*LAMB*Z2(I+1581) * SQ		! Tyx(Rx) SEM
       TEMP2(IJ+1020)=TEMP2(IJ+1020)+       
     * (-LAMB2*E* (Z2(I+51)*TMRX/2.-LAMB*TPBR2*Z2(I+102)/2.) /TXR) * SQ	! Tyx(Mxy) SEM   
       TEMP2(IJ+1275)=TEMP2(IJ+1275)+
     *            ( (-LAMB*Z2(I)+Z2(I+1530)) *CONST(11)*1.5/TMRX) * SQ	! Tyx(??) SEM  
      ENDIF     
C   CALCUL DE : SIGMA COMP JAS, SIGMA COMP JAB , SIGMA COMP SEMELLE     
      IF (NZ.NE.JJK) GOTO 112                                         
      EFF(IJ+8160)=DABS(TEMP2(IJ+255))+DABS(TEMP2(IJ))			! TXZ JAS
      EFF(IJ+8415)=DABS(TEMP2(IJ+510))+DABS(TEMP2(IJ))			! TXZ JAB
      EFF(IJ+8925)=DSQRT(EFF(IJ+5865)**2+3.D0*(EFF(IJ+6630)**2))	! SIGMA COMP JAS
      EFF(IJ+9180)=DSQRT(EFF(IJ+6120)**2+3.D0*(EFF(IJ+6885)**2))	! SIGMA COMP JAB
      IF(TXR.NE.0.) THEN
       EFF(IJ+8670)=DABS(TEMP2(IJ+765))+DABS(TEMP2(IJ+1020))		! Tyx SEM
     *                                 +DABS(TEMP2(IJ+1275))
       EFF(IJ+9435)=DSQRT(EFF(IJ+6375)**2+3.D0*(EFF(IJ+7140)**2))	! SIGMA COMP SEMELLE
	ENDIF
	ENDIF					!fin cercle-carre
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
 	SMT2=(HYT+TYT)*KSTT									
      SMAT=SMT-TYT*KSTT
      SMAT2=SMT2-TYT*KSTT									
      SMBT=(DELTA/2.)*KSTT
      TPBT=TMT*(SMAT+0.5*TYT*KSTT)
	TPBT2=TMT*(SMAT2+0.5*TYT*KSTT)						
      TPRT=TPBT+DYT*HYT*(SMAT-0.5*HYT*KSTT)
      TPRT2=TPBT2+DYT*HYT*(SMAT2-0.5*HYT*KSTT)			
	
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
     *                (-Z2(2*I+81)+LAMB*SMT2*Z2(2*I+132))*SP
      ENDIF
      CONC(IJ+350)=CONC(IJ+350)+E*LAMB*(-Z2(2*I+81)						! SIG X JAS
     *                            +SMAT2*LAMB*Z2(2*I+132))*SP
      CONC(IJ+400)=CONC(IJ+400)+E*LAMB*(-Z2(2*I+81)						! SIG X JAB
     *                            +SMBT*LAMB*Z2(2*I+132))*SP
      IF((TYT.GT.0.).AND.(WYT.GT.0.)) THEN
       TEMP3(IJ)    =TEMP3(IJ) + SQ*										! TYX(Nxy)  SEM
     *    (CONST(17+JI)*(-LAMB*Z2(2*I+30)+Z2(2*I+1560))*1.5/TMT)*FAM(5)           
       TEMP3(IJ+50) =TEMP3(IJ+50)  + SQ* (E/2)*TYT*LAMB*Z2(2*I+1611)			! TYX(Mxy)  SEM
       TEMP3(IJ+100)=TEMP3(IJ+100) + SQ* 
     *  (-LAMB2* E*(Z2(2*I+81)*TMT/2.-LAMB*TPBT2*Z2(2*I+132)/2.)) /TYT		! TYX(Rx)  SEM
      ENDIF
      IF(DYT.NE.0.) THEN
       TEMP3(IJ+150)=TEMP3(IJ+150)+ (-LAMB**2*E*(Z2(2*I+81)*TMT-			! TXZ(Rx) JAS
     *            LAMB*TPBT2*Z2(2*I+132))/DYT)    *SQ                    
       TEMP3(IJ+200)=TEMP3(IJ+200)+ (-LAMB**2*E*(Z2(2*I+81)*TMRT-			! TXZ(Rx) JAB
     *            LAMB*TPRT2*Z2(2*I+132))/DYT) *SQ                    
       TEMP3(IJ+250)=TEMP3(IJ+250)+ (E/2)*DYT*LAMB*Z2(2*I+1611) *SQ			! TXZ(Mxy)JAS=TXZ(Mxy)JAB
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
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
C                                                                       
C     CALCUL DES SENSIBILITES SUR LES CONTRAINTES  
C     --------------------------------------------
C	- [calcul de SENS2(15,IPTmax,9,NETO) ] : contraintes bordage, aig., raid. et traverses
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

C      IF(IOPTI.EQ.0) GO TO 549

C	IPT=IPTS(NEL)

C	SP=DSIN(NZ*PI/2)	! Sin(x=L/2)
C	SQ=+1.0				! COS (x=0) = COS(x=L)

	
C      DO 284 IPAN=1,NETO
C      NBRXI=NVAR(IPAN)

C	CALL ANNULD(DC,162)
C	IF(NEL.EQ.IPAN) THEN
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

C      DC(1,1,1)=-0.5                                     ! Sig en -d/2 Contr. 3
C      DC(1,1,2)=+0.5                                     ! Sig en +d/2 Contr. 4
C
C      DC(2,1,2)=KSAA/2.                                  ! Saig JAS-SEM, Contr. 6/9
C      DC(2,1,3)=TM*KSAA/2.                               ! Saig JAS-SEM, Contr. 6/9
C      DC(2,2,2)=KSAA                                     ! Saig JAS-SEM, Contr. 6/9
C      DC(2,2,3)=TM*KSAA                                  ! Saig JAS-SEM, Contr. 6/9
C      DC(2,4,1)=TYA                                      ! Saig JAS-SEM, Contr. 6/9
C      DC(2,4,3)=TYA*SMC                                  ! Saig JAS-SEM, Contr. 6/9
C                                                         !
C      DC(3,1,2)=KSAA/2.                                  ! Saig JAB, Contr. 8
C      DC(3,1,3)=TMR*KSAA/2.                              ! Saig JAB, Contr. 8C
C      DC(3,2,1)=DYA                                      ! Saig JAB, Contr. 8
C      DC(3,2,3)=(TMR+DYA*DELTA/2.)*KSAA                  ! Saig JAB, Contr. 8
C      DC(3,3,1)=HYA                                      ! Saig JAB, Contr. 8
C      DC(3,3,3)=HYA*SMD                                  ! Saig JAB, Contr. 8
C      DC(3,4,1)=TYA                                      ! Saig JAB, Contr. 8
C      DC(3,4,3)=TYA*SMC                                  ! Saig JAB, Contr. 8
                                                         !
C      DC(4,1,2)=TMRX*KSRR/2.                             ! Traid JAS, Contr. 10
C      DC(4,6,2)=TMRX*KSRR                                ! Traid JAS, Contr. 10
C      DC(4,8,1)=TXR                                      ! Traid JAS, Contr. 10
C      DC(4,8,2)=TXR*SMCR                                 ! Traid JAS, Contr. 10
                                                         !
C      DC(5,1,2)=TMRR*KSRR/2.                             ! Traid JAB, Contr. 11
C      DC(5,6,1)=DXR                                      ! Traid JAB, Contr. 11
C     DC(5,6,2)=(TMRR+DXR*DELTA/2.)*KSRR                 ! Traid JAB, Contr. 11
C      DC(5,7,1)=HXR                                      ! Traid JAB, Contr. 11
C      DC(5,7,2)=HXR*SMDR                                 ! Traid JAB, Contr. 11
C      DC(5,8,1)=TXR                                      ! Traid JAB, Contr. 11
C      DC(5,8,2)=TXR*SMCR                                 ! Traid JAB, Contr. 11
                                                         !
C      DC(6,1,1)=KSRR/2.                                  ! Sraid SEM, Contr. 12
C      DC(6,6,1)=KSRR                                     ! Sraid SEM, Contr. 12
C	ENDIF

C      DO 283 KK=1,NBRXI
C      K=NXIT(KK,IPAN)
C 
C	nø 1	 Sigma comp. (Sx,Sy) bordage (Z=0)      en x=L/2  (en N/mm2)
C	nø 2	 Sigma comp. (Txy)   bordage (Z=0)      en x=0
C	nø 3	 Sigma comp. (Sx,Sy) bordage (Z=-d/2)   en x=L/2
C	nø 4	 Sigma comp. (Sx,Sy) bordage (Z=+d/2)   en x=L/2

C	nø 5	 Sigma comp. (Tâme)   aiguille   JAS     en X=0    (en N/mm2)
C	nø 6	 Sigma comp. (Sy,Txy) aiguille   JAS     en X=L/2
C	nø 7	 Sigma comp. (Tâme)   aiguille   JAB     en X=0
C	nø 8	 Sigma comp. (Sy,Txy) aiguille   JAB     en X=L/2
C	nø 9	 Sigma comp. (Sy,Txy) aiguille   Semelle en X=L/2

C	nø 10	 Sigma comp. (Tâme)   raidisseur JAS     en X=0    (en N/mm2)
C	nø 11	 Sigma comp. (Tâme)   raidisseur JAB     en X=0
C	nø 12	 Sigma comp. (Sx)     raidisseur Semelle en X=L/2

C	nø 13	 Sigma comp. (Tâme)   Traverse JAS       en X=0    (en N/mm2)
C	nø 14	 Sigma comp. (Tâme)   Traverse JAB       en X=0
C	nø 15	 Sigma comp. (Sx)     Traverse Semelle   en X=L/2
 
C  1- CONTRAINTE ds BORDAGE en Z=0 et X=L/2  (SigmaX,SigmaY,Txy,SIGMA Comp) 
C  --------------------------------------------------------------------------

c 	WRITE(66,*)'1 CONTRAINTE ds BORDAGE en Z=0 et X=L/2'
C      DO 380 I=1,IPT                                             		
C	IJ=4*51+IPTS2(I,NEL)
C
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

C      SENS3(1,I,K,IPAN)= SENS3(1,I,K,IPAN)+     != DSX= d(SIGMAX)/dx
C     *    SP* SN * ( - LAMB * SENS1(5,I,K,IPAN)							! DSX= d(SIGMAX)/dx
C     *               + ETA  * (SENS1(8,I,K,IPAN)+SENS1(1,I,K,IPAN)/Q)  )
C      SENS3(2,I,K,IPAN)= SENS3(2,I,K,IPAN) +    !=DSY
C     *    SP* SN * ( SENS1(8,I,K,IPAN)									! DSY= d(SIGMAY)/dx
C     *              - LAMB*ETA*SENS1(5,I,K,IPAN) + SENS1(1,I,K,IPAN)/Q )  
C	IF (NZ==JJK) THEN
C	 DEN=EFF(IJ+4845)
C	 IF(DABS(DEN).LE.(+1.E-10)) DEN =1.E-10
C       SENS2(1,I,K,IPAN)=
C     *    0.5*( (2.*EFF(IJ+4335)-EFF(IJ+4080))*SENS3(1,I,K,IPAN) +			! d(SIGMA COMP)
C     *          (2.*EFF(IJ+4080)-EFF(IJ+4335))*SENS3(2,I,K,IPAN)   )/DEN
C      ENDIF

c  	WRITE(66,*)'new calculation'
c  	WRITE(66,*)'SENS3(1,I,K,IPAN)=',SENS3(1,I,K,IPAN)
c  	WRITE(66,*)'SENS3(2,I,K,IPAN)=',SENS3(2,I,K,IPAN)
c  	WRITE(66,*)'SENS2(1,I,K,IPAN)=',SENS2(1,I,K,IPAN)


C  380 CONTINUE

C  2- CONTRAINTE ds BORDAGE en Z=0 et X=0  (SigmaX=0,SigmaY=0,Txy,SIGMA Comp) 
C  --------------------------------------------------------------------------

c 	WRITE(66,*)'2 CONTRAINTE Tau, BORDAGE en Z=0 et X=0 '
C      DO 381 I=1,IPT                                             		
C	IJ= IPTS2(I,NEL)

c 	WRITE(66,*)'** 2 CONTRAINTE Tau, BORDAGE en Z=0 et X=0 '
c 	WRITE(66,*)'IPT=',I
c 	WRITE(66,*)'LAMB=',LAMB
c 	WRITE(66,*)'SENS1(6,I,K,IPAN)=',SENS1(6,I,K,IPAN)
c  	WRITE(66,*)'SENS1(7,I,K,IPAN)=',SENS1(7,I,K,IPAN)
c  	WRITE(66,*)'EFF(IJ+4590)=',EFF(IJ+4590)
c  	WRITE(66,*)'EFF(IJ+4845)=',EFF(IJ+4845)
c  	WRITE(66,*)'SENS2(2,I,K,IPAN)=',SENS2(2,I,K,IPAN)

C      SENS2(2,I,K,IPAN)=  SENS2(2,I,K,IPAN) +
C     *     SQ* SH * ( SENS1(6,I,K,IPAN) + LAMB  * SENS1(7,I,K,IPAN)  )	! DTXY=d(TXY)

C	IF (NZ==JJK) THEN
C       DEN=EFF(IJ+4590)
C       IF(DABS(DEN).LE.(+1.E-10)) DEN =1.E-10
C       SENS2(2,I,K,IPAN)=1.732*SENS2(2,I,K,IPAN) *DEN/DABS(DEN)			! d(SIGMA COMP)
C      ENDIF

c  	WRITE(66,*)'SENS2(2,I,K,IPAN)=',SENS2(2,I,K,IPAN)


C  381 CONTINUE

C  3- CONTRAINTE ds BORDAGE en Z=-d/2 et X=L/2  (SigmaX,SigmaY,Txy=0,SIGMA Comp) 
C  --------------------------------------------------------------------------

c 	WRITE(66,*)'3 CONTRAINTE ds BORDAGE en Z=-d/2 et X=L/2  '
C      DO 382 I=1,IPT                                             		
C	IP=IPTS2(I,NEL)
C	IJ=4*51+IP

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

C      SENS3(3,I,K,IPAN)= SENS3(3,I,K,IPAN) +							! d(SIGMAX)/dx
C     *  SP* SN*( - LAMB * SENS1(5,I,K,IPAN) 
C     *           + ETA  * (SENS1(8,I,K,IPAN)+SENS1(1,I,K,IPAN)/Q)
C     * +(DELTA/2.)*( ETA*SENS1(3,I,K,IPAN)-LAMB2*SENS1(1,I,K,IPAN)) 
C     * - DC(1,K,1)*( ETA*Z2(IP+1734)-LAMB2*Z2(IP+102))  ) 
C      SENS3(4,I,K,IPAN)= SENS3(4,I,K,IPAN) +							 ! d(SIGMAY)/dx
C     *   SP* SN*( SENS1(8,I,K,IPAN) 
C     *         - LAMB*ETA*SENS1(5,I,K,IPAN) + SENS1(1,I,K,IPAN)/Q  
C     * +(DELTA/2.)*( SENS1(3,I,K,IPAN)-ETA*LAMB2*SENS1(1,I,K,IPAN))   
C     * - DC(1,K,1)*( Z2(IP+1734)-ETA*LAMB2*Z2(IP+102))  ) 

C      IF(NZ==JJK) THEN
C	 DEN=EFF2(IJ+1785)
C	 IF(DABS(DEN).LE.(+1.E-10)) DEN =1.E-10
C       SENS2(3,I,K,IPAN)=												! d(SIGMA COMP)/dx
C     *   0.5*( (2.*EFF2(IJ+1275)-EFF2(IJ+1020))*SENS3(3,I,K,IPAN)
C     *        +(2.*EFF2(IJ+1020)-EFF2(IJ+1275))*SENS3(4,I,K,IPAN) )/DEN
C      ENDIF

C  382 CONTINUE

c	ENDIF

C  4- CONTRAINTE ds BORDAGE en Z=+d/2 et X=L/2  (SigmaX,SigmaY,Txy=0,SIGMA Comp) 
C  --------------------------------------------------------------------------

c 	WRITE(66,*)'4 CONTRAINTE ds BORDAGE en Z=+d/2 et X=L/2  '
C      DO 383 I=1,IPT                                             		
C	IP=IPTS2(I,NEL)
C	IJ=4*51+IP

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

C      SENS3(5,I,K,IPAN)= SENS3(5,I,K,IPAN) +                          ! d(SIGMAX)/dx
C     *   SP*SN* ( - LAMB * SENS1(5,I,K,IPAN)  
C     *             + ETA  * (SENS1(8,I,K,IPAN)+SENS1(1,I,K,IPAN)/Q)
C     * -(DELTA/2.)*( ETA*SENS1(3,I,K,IPAN)-LAMB2*SENS1(1,I,K,IPAN))  
C     * - DC(1,K,2)*( ETA*Z2(IP+1734)-LAMB2*Z2(IP+102))   ) 
C      SENS3(6,I,K,IPAN)=SENS3(6,I,K,IPAN) +                            ! d(SIGMAY)
C     *   SP*SN* ( SENS1(8,I,K,IPAN)
C     *            - LAMB*ETA*SENS1(5,I,K,IPAN) + SENS1(1,I,K,IPAN)/Q  
C     * -(DELTA/2.)*( SENS1(3,I,K,IPAN)-ETA*LAMB2*SENS1(1,I,K,IPAN))  
C    * - DC(1,K,2)*( Z2(IP+1734)-ETA*LAMB2*Z2(IP+102))  ) 
C
C      IF(NZ==JJK) THEN
C	  DEN=EFF2(IJ+765)
C	  IF(DABS(DEN).LE.(+1.E-10)) DEN =1.E-10
C        SENS2(4,I,K,IPAN) =										  ! d(SIGMA COMP)
C     *     0.5*( (2.*EFF2(IJ+255)-EFF2(IJ)    )*SENS3(5,I,K,IPAN) 
C     *          +(2.*EFF2(IJ)    -EFF2(IJ+255))*SENS3(6,I,K,IPAN) )/DEN
C	ENDIF

C  383 CONTINUE

C  5&7- CONTRAINTE ds CADRES JAS et JAB en X=0 [Txy (du à Mxy) = b * Wø'/q] 
C  --------------------------------------------------------------------------
c 	WRITE(66,*)'5/7 CONTRAINTE ds CADRES JAS et JAB en X=0 (Txy)'
C      DO 384 I=1,IPT                                             		
C	IJ=IPTS2(I,NEL)
C	IP=IJ

c	Seul le terme en SQ est pris en compte
C      SENS2(5,I,K,IPAN)= SENS2(5,I,K,IPAN)							! d(TYZ(Mxy) JAS)dx
C     *                   + SQ* SH*LAMB*DYA*SENS1(2,I,K,IPAN)
C      IF((K.EQ.3).AND.(NEL.EQ.IPAN)) THEN
C	 SENS2(5,I,K,IPAN)= SENS2(5,I,K,IPAN)+ SQ*SH*LAMB*Z2(IP+1581)
C	ENDIF 

C	IF(NZ==JJK) THEN
C	 DEN=TEMP(IJ+510)  ! TEMP = TYZ(Mxy) JAS  calcule ci-avant dans RESUL
C	 IF(DABS(DEN).LE.(+1.E-10)) DEN =1.E-10
C      SENS2(5,I,K,IPAN) = 1.732 * SENS2(5,I,K,IPAN) * DEN/DABS(DEN)  ! d(COMP JAS)dx
C       SENS2(7,I,K,IPAN) = SENS2(5,I,K,IPAN)							! d(COMP JAB)dx
C	ENDIF

c 	WRITE(66,*)'IPT=',I,' SENS2(7,I,K,IPAN)=',SENS2(7,I,K,IPAN)
C  384 CONTINUE

C  6&8- CONTRAINTES ds CADRES JAS et JAB en X=L/2 (Sigma Y, Txy et Scomp) 
C  --------------------------------------------------------------------------
c 	WRITE(66,*)'6/8 CONTRAINTES ds CADRES JAS et JAB en X=L/2'
C      DO 385 I=1,IPT
C	IP=IPTS2(I,NEL)
C	IJ=4*51+IP

c     EFF(IJ+5100) = SIGMAY JAS       
C     EFF(IJ+5610) = SP * DABS(TYZ(Ry))  + SQ *DABS(Tyz(Mxy))  en JAS 
C     TEMP(IJ)     = SP * (TYZ(Ry))  en JAS 
c     SENS3(7,I,K,IPAN) = d(SIGMAY JAS)/dx  
c     SENS3(8,I,K,IPAN) = d(TYZ JAS)/dx 	seul le terme en SP sont pris en compte
C      SENS3(7,I,K,IPAN)= SENS3(7,I,K,IPAN) +	SP*							!d(SIGMAY JAS)/dx 
C     *   E*(SENS1(8,I,K,IPAN)+SENS1(1,I,K,IPAN)/Q-SMA*SENS1(3,I,K,IPAN)
C     *                       -DC(2,K,2)*Z2(IP+1734) )
C      SENS3(8,I,K,IPAN)= SENS3(8,I,K,IPAN) +	SP*E*						!d(TYZ JAS)/dx 
C     *     (   TM * (SENS1(9,I,K,IPAN)+SENS1(2,I,K,IPAN)/Q)          
C     *       + DC(2,K,1) * (Z2(IP+1632)+Z2(IP+1581)/Q)
C     *       - TPB* SENS1(4,I,K,IPAN) -DC(2,K,3)*Z2(IP+1887)  )/DYA
C	STT7 =SP *E*(TM*(Z2(IP+1632)+Z2(IP+1581)/Q) - TPB*Z2(IP+1887))/DYA	! TYZ JAS (non cumule)
c     STT7 = TEMP(IJ) =TYZ JAS  mais non cumule
C      IF((K.EQ.3).AND.(NEL.EQ.IPAN)) THEN
C	  SENS3(8,I,K,IPAN)= SENS3(8,I,K,IPAN) - STT7/DYA
C	ENDIF

C      IF(NZ==JJK) THEN
C	 DEN=EFF(IJ+6120)
C	 IF(DABS(DEN).LE.(+1.0E-10)) DEN =1.E-10
C       SENS2(6,I,K,IPAN)=													! d(SIGMA COMP JAS)/dx (sensib)
C     *  0.5* ( 2.*EFF(IJ+5100)*SENS3(7,I,K,IPAN) 
C     *        +6.*TEMP(IJ)    *SENS3(8,I,K,IPAN)  )/DEN     
c 	 WRITE(66,*)' SENS2(6,I,K,IPAN)=',SENS2(6,I,K,IPAN)
C      ENDIF

c     EFF(IJ+5355)      = SIGMAY JAB        
c	EFF(IJ+5865)      = SP * DABS(TYZ(Ry))  + SQ *DABS(Tyz(Mxy))  en JAB
c	TEMP(IJ+255) = TXY(JAB) = SP * TYZ(Ry) 
c     SENS3( 9,I,K,IPAN) = d(SIGMAY JAB)/dx  
C     SENS3(10,I,K,IPAN) = d(TYZ JAB)/dx 	seul le terme en SP est pris en compte
C      SENS3(9,I,K,IPAN)= SENS3(9,I,K,IPAN) +	SP*							!d(SIGMAY JAB)/dx 
C     *  E*(SENS1(8,I,K,IPAN)+SENS1(1,I,K,IPAN)/Q-SMB*SENS1(3,I,K,IPAN) 
C     *                      -DC(3,K,2)*Z2(IP+1734)  )
C      SENS3(10,I,K,IPAN)= SENS3(10,I,K,IPAN) + SP*E*						!d(TYZ JAB)/dx = d(STT8)/dx
C     *    (   TMR * (SENS1(9,I,K,IPAN)+SENS1(2,I,K,IPAN)/Q)   
C     *      + DC(3,K,1) * (Z2(IP+1632)+Z2(IP+1581)/Q)
C     *      - TPR* SENS1(4,I,K,IPAN) - DC(3,K,3)*Z2(IP+1887)   )/DYA
C	STT8=SP *E*(TMR*(Z2(IP+1632)+Z2(IP+1581)/Q) - TPR*Z2(IP+1887))/DYA	!TYZ JAB (non cumule)
c     STT8 = TEMP(IJ+255) mais non cumule
C      IF((K.EQ.3).AND.(NEL.EQ.IPAN)) THEN
C	  SENS3(10,I,K,IPAN)=SENS3(10,I,K,IPAN)- STT8/DYA
C	ENDIF

C      IF(NZ==JJK) THEN
C	 DEN=EFF(IJ+6375)
C	 IF(DABS(DEN).LE.(+1.E-10)) DEN =1.E-10
C      SENS2(8,I,K,IPAN) =               ! d(SIGMA COMP JAB)/dx 
C     *  0.5*(  2.*EFF(IJ+5355)*SENS3( 9,I,K,IPAN)  
C     *       + 6.*TEMP(IJ+255)*SENS3(10,I,K,IPAN) )/DEN   
Cc 	 WRITE(66,*)' SENS2(8,I,K,IPAN)=',SENS2(8,I,K,IPAN)
C      ENDIF

C  385 CONTINUE

C  9- CONTRAINTES ds CADRES SEMELLES en X=L/2 (Sigma Y, Txy et Scomp) 
C  ---------------------------------------------------------------
c 	WRITE(66,*)'** 9 CONTR. ds CADRES SEMELLES en X=L/2'
c 	WRITE(66,*)'   Variables XI=',K,' I PAN=',IPAN

C      DO 386 I=1,IPT
C	IP=IPTS2(I,NEL)
C	IJ=4*51+IP

c     EFF(IJ+6630)   = SIGMAY SEM        
c	EFF(IJ+6885)   = SP * DABS(TYZ(1)) + SQ*DABS(TYZ(2)) + SQ* DABS(TYZ(3)) Semelle
c	TEMP(IJ+765)   = TYZ(Ry) Sem     Cumule
c                    = SP*E*(TM*(Z2(IP+1632)+Z2(IP+1581)/Q)/2.-TPB*Z2(IP+1887)/2.)/TYA
C      SENS3(11,I,K,IPAN)= SENS3(11,I,K,IPAN) + SP * E *					! d(SIGMAY Sem)/dx
C     *  ( SENS1(8,I,K,IPAN)+SENS1(1,I,K,IPAN)/Q-SM*SENS1(3,I,K,IPAN)
C     *   -DC(2,K,2)*Z2(IP+1734)  )
C      SENS3(12,I,K,IPAN)= SENS3(12,I,K,IPAN) +  SP * 0.5*E* 				! d(TYZ(Ry) Sem)/dx             
C     *   (  TM * (SENS1(9,I,K,IPAN)+SENS1(2,I,K,IPAN)/Q)       
C     *    + DC(2,K,1) * (Z2(IP+1632)+Z2(IP+1581)/Q)
C     *    - TPB* SENS1(4,I,K,IPAN) - DC(2,K,3)*Z2(IP+1887)  )/TYA

C      IF(NZ==JJK) THEN
C	  DEN=EFF(IJ+7140)
C	  IF(DABS(DEN).LE.(+1.E-10)) DEN =1.E-10
C        SENS2(9,I,K,IPAN) =         ! d(SIGMA COMP Sem)/dx 
C     *    0.5*( 2.*EFF(IJ+6630)*SENS3(11,I,K,IPAN) 
C     *         +6.*TEMP(IJ+765)*SENS3(12,I,K,IPAN))/DEN     
C      ENDIF

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

C  386 CONTINUE

C  10&11- CONTRAINTES ds RAIDISSEURS JAS et JAB en X=0 (Txy ) 
C  ---------------------------------------------------------------
c 	WRITE(66,*)'10/11 CONTRAINTES ds RAIDISSEURS JAS et JAB en X=0'
C      DO 387 I=1,IPT
C	IP=IPTS2(I,NEL)
C	IJ=IP

c     TEMP2(IJ+255)      =  TYZ(Rx)JAS      
c     SENS3(13,I,K,IPAN)= d(TYZ(Rx)JAS)/dx  
C      SENS3(13,I,K,IPAN)= SENS3(13,I,K,IPAN) - SQ * LAMB2*E*				! d(TYZ(Rx)JAS)/dx = d(STT1)/dx
C     *    ( SENS1(5,I,K,IPAN)*TMRX + Z2(IP+51)*DC(4,K,1) 
C     *     -LAMB*(TPBR*SENS1(1,I,K,IPAN) + DC(4,K,2)*Z2(IP+102)) )/DXR 
c     STT1= TYZ (Rx) JAS  idem TEMP2(IJ+255) mais sans cumuler les termes
C      STT1=-SQ* LAMB2*E* ( Z2(IP+51)*TMRX-LAMB*TPBR*Z2(IP+102) ) /DXR		! TYZ (Rx) JAS non cumule
C      IF((K.EQ.7).AND.(NEL.EQ.IPAN)) THEN
C	  SENS3(13,I,K,IPAN)= SENS3(13,I,K,IPAN) -  STT1/DXR
C	ENDIF

C     TEMP2(IJ+0)       =   TYZ(Mxy)JAS 
C     SENS3(14,I,K,IPAN)= d(TYZ(Mxy)JAS)/dx 
C      SENS3(14,I,K,IPAN)= SENS3(14,I,K,IPAN)								!d(TYZ(Mxy)JAS)/dx 
C     *                    + SH*DXR*LAMB * SENS1(2,I,K,IPAN)                    
C      IF((K.EQ.7).AND.(NEL.EQ.IPAN)) THEN
C	 SENS3(14,I,K,IPAN)= SENS3(14,I,K,IPAN) + SQ* SH*LAMB*Z2(IP+1581)	!d(TYZ(Mxy)JAS)/dx 
C	ENDIF

C      IF(NZ==JJK) THEN
C	 DEN =TEMP2(IJ+255) !TYZ(Rx)JAS
C	 IF(DABS(DEN).LE.(+1.E-10))  DEN =1.E-10
C	 DEN2=TEMP2(IJ)		!TYZ(Mxy)JAS 
C	 IF(DABS(DEN2).LE.(+1.E-10)) DEN2=1.E-10
C       SENS2(10,I,K,IPAN)=  SENS3(13,I,K,IPAN) * DEN /DABS(DEN)  ! d(DABS(TYZ(Rx))+DABS(TYX(Mxy))JAS)/dx
C     *                    + SENS3(14,I,K,IPAN) * DEN2/DABS(DEN2)    
C       SENS2(10,I,K,IPAN)=1.732051*SENS2(10,I,K,IPAN)                  ! d(SIGMA COMP JAS )/dx
C      ENDIF

c     TEMP2(IJ+510)     = TYZ(Rx) JAB       
c     SENS3(15,I,K,IPAN)= d(TYZ(Rx)JAB)/dx  
C      SENS3(15,I,K,IPAN)= SENS3(15,I,K,IPAN) - SQ* LAMB2*E*			 ! d(TYZ(Rx)JAB)/dx
C     *    (  SENS1(5,I,K,IPAN)*TMRR + Z2(IP+51)*DC(5,K,1)           
C     *     - LAMB*(TPRR*SENS1(1,I,K,IPAN) + DC(5,K,2)*Z2(IP+102)) ) /DXR 
    ! STT2= TYZ (Rx) JAB  idem TEMP2(IJ+510) mais sans cumuler les termes
C      STT2=-SQ * LAMB2*E* ( Z2(IP+51)*TMRR-LAMB*TPRR*Z2(IP+102) ) /DXR ! TYZ (Rx) JAB (contraintes)
C      IF((K.EQ.7).AND.(NEL.EQ.IPAN)) THEN
C	 SENS3(15,I,K,IPAN)= SENS3(15,I,K,IPAN) -  STT2 /DXR            
C	ENDIF                                                   

C     TEMP2(IJ)         =   TYZ(Mxy)JAB      =  TYZ(Mxy)JAS
c     SENS3(16,I,K,IPAN)= d(TYZ(Mxy)JAB)/dx 
C      SENS3(16,I,K,IPAN)= SENS3(14,I,K,IPAN)	! car TYZ(Mxy) JAB =  TYZ(Mxy) JAS 

C      IF(NZ==JJK) THEN
C	  DEN =TEMP2(IJ+510)
C	  IF(DABS(DEN).LE.(+1.E-10)) DEN =1.E-10
C	  DEN2=TEMP2(IJ)
C	  IF(DABS(DEN2).LE.(+1.E-10)) DEN2 =1.E-10
C       SENS2(11,I,K,IPAN)=  SENS3(15,I,K,IPAN) * DEN /DABS(DEN)	! d(DABS(TYZ(Rx))+DABS(TYX(Mxy))JAB)/dx
C     *                     + SENS3(16,I,K,IPAN) * DEN2/DABS(DEN2)      
C        SENS2(11,I,K,IPAN) =1.732051*SENS2(11,I,K,IPAN)				! d(SIGMA COMP JAB)/dx 
C      ENDIF

c 	WRITE(66,*)' IPT=',I,' DTXY=',DTXY, 'pour JAB'
C  387 CONTINUE

C  12- CONTRAINTES ds RAIDISSEURS sem en X=L/2 (Sigma X) 
C  -------------------------------------------------------------------
c 	WRITE(66,*)'12 CONTR. ds RAIDISSEURS SEMELLES en X=L/2'
C      DO 388 I=1,IPT
C	IP=IPTS2(I,NEL)
C	IJ=4*51+IP

C     SENS2(12,I,K,IPAN)= SENS2(12,I,K,IPAN) + SP* E*LAMB*		    ! d(SIGMAX Sem)/dx (=sensib)
C     *     ( - SENS1(5,I,K,IPAN)                                 
C     *       + LAMB*(SMR*SENS1(1,I,K,IPAN)+DC(6,K,1)*Z2(IP+102)) )

C	IF (NZ==JJK) THEN
C	  DEN=EFF(IJ+7905) ! = Sx sem raid
C	  IF(DABS(DEN).LE.(+1.E-10)) DEN=1.E-10
C        SENS2(12,I,K,IPAN)= SENS2(12,I,K,IPAN)*DEN/DABS(DEN)          ! d(SIGMA COMP Sem) (=sensib)
C      ENDIF

c  	WRITE(66,*)'SENS2(12,I,K,IPAN)=',SENS2(12,I,K,IPAN)

C  388 CONTINUE

C  BOUCLE (391) SUR LES TRAVERSES
C  ==========================
C      DERIV=0.
C      IF(MT.EQ.0) GOTO 392	

C      DO 391 I=1,IPT
C	 IT=IPTS3(I,NEL) ! No de la traverse concernee
c	 WRITE(9,*) 'IPT=', I, 'NEL=',NEL, 'IPTS3=IT=', IT
C       IF(IT.EQ.0) GOTO 391	

C      WYT=WXTR(IT)                                          ! Larg.   sem
C      TYT=TXTR(IT)                                          ! Epaiss. sem
C      HYT=HXTR(IT)                                          ! Haut.   âme
C      DYT=DXTR(IT)                                          ! Epaiss. âme
C      TMT=WYT*TYT                                                       
C      TMRT=TMT+DYT*HYT
C      TPBT=TMT*(DELTA/2.+HYT+0.5*TYT)*KSTT
C      TPRT=TPBT+DYT*HYT*(DELTA+HYT)*KSTT/2.

C  13&14- CONTRAINTES ds TRAVERSES JAS et JAB en X=0 (Txy ) 
C  ---------------------------------------------------------------
c 	WRITE(66,*)'13/14 CONTRAINTES ds TRAVERSES JAS et JAB en X=0'
C	IJ=IT ! car x=0
c     TEMP3(IJ+150)     =   TYZ(Rx) JAS     
c     TEMP3(IJ+250)     =   TYZ(Mxy)JAS 
c     SENS3(17,I,K,IPAN)= d(TYZ(Rx)JAS)/dx 
c     SENS3(18,I,K,IPAN)= d(TYZ(Mxy)JAS)/dx 
C      IF(K.EQ.1) DERIV=TMT*KSTT/2.
c     TYZ(Rx)=-SQ*LAMB2*E*(Z2(2*IT+81)*TMT-LAMB*TPBT*Z2(2*IT+132))/DYT 
C      SENS3(17,I,K,IPAN)=SENS3(17,I,K,IPAN) - SQ* LAMB2*E*			! d(TYZ(Rx) JAS)dx
C     *      ( SENS1(5,I,K,IPAN)*TMT -                      
C     *        LAMB*(TPBT*SENS1(1,I,K,IPAN)+DERIV*Z2(2*IT+132))  ) /DYT 
c     TYZ(Mxy)= SQ* SH*DYT*LAMB*Z2(2*IT+1611)							! TYZ (Mxy) JAS 
C      SENS3(18,I,K,IPAN)= SENS3(18,I,K,IPAN)							! d(TYZ(Mxy) JAS)/dx
C     *                  + SQ* SH*DYT*LAMB * SENS1(2,I,K,IPAN)	
C      
C	IF (NZ==JJK) THEN
C	  DEN =TEMP3(IJ+150)
C	  IF(DABS(DEN).LE.(+1.E-10)) DEN =1.E-10
C	  DEN2=TEMP3(IJ+250)
C	  IF(DABS(DEN2).LE.(+1.E-10)) DEN2 =1.E-10
C       SENS2(13,I,K,IPAN)=   SENS3(17,I,K,IPAN) * DEN /DABS(DEN) 
C     *                      + SENS3(18,I,K,IPAN) * DEN2/DABS(DEN2)	! d(ABS[TYZ(Rx)]+ABS[TYZ(Mxy)]JAS)/dx 
C        SENS2(13,I,K,IPAN) = 1.732051*SENS2(13,I,K,IPAN)              ! d(SIGMA COMP JAS)/dx 
C      ENDIF


c     TEMP3(IJ+200)     =   TYZ(Rx) JAB     
c     TEMP3(IJ+250)     =   TYZ(Mxy)JAB 
c     SENS3(17,I,K,IPAN)= d(TYZ(Rx)JAB)/dx  
c     SENS3(18,I,K,IPAN)= d(TYZ(Mxy)JAB)/dx 
C	IF(K.EQ.1) DERIV=TMRT*KSTT/2.
c     TYZ(Rx)=-SQ* LAMB2*E*(Z2(2*IT+81)*TMRT-LAMB*TPRT*Z2(2*IT+132))/DYT 
C      SENS3(19,I,K,IPAN)=SENS3(19,I,K,IPAN)  - SQ* LAMB2*E*			!  d(TYZ(Rx)JAB)/dx
C     *      (  SENS1(5,I,K,IPAN)*TMRT              
C     *       - LAMB*(TPRT*SENS1(1,I,K,IPAN)+DERIV*Z2(2*IT+132))  ) /DYT 
c     TYZ(Mxy) JAB = TYZ(Mxy) JAS                                     !   TYZ(Mxy) JAB 
C      SENS3(20,I,K,IPAN)=SENS3(18,I,K,IPAN)							! d(TYZ(Mxy) JAB)/dx

C	IF (NZ==JJK) THEN
C	  DEN =TEMP3(IJ+200)
C	  IF(DABS(DEN).LE.(+1.E-10)) DEN =1.E-10
C	  DEN2=TEMP3(IJ+250)
C	  IF(DABS(DEN2).LE.(+1.E-10)) DEN2 =1.E-10
C        SENS2(14,I,K,IPAN)=   SENS3(19,I,K,IPAN) * DEN /DABS(DEN) 
C     *                      + SENS3(20,I,K,IPAN) * DEN2/DABS(DEN2)	! d(ABS[TYZ(Rx)]+ABS[TYZ(Mxy)]JAB)/dx 
C        SENS2(14,I,K,IPAN) = 1.732051*SENS2(14,I,K,IPAN)              ! d(SIGMA COMP JAB)/dx 
C      ENDIF

C  15- CONTRAINTES ds TRAVERSES SEM en X=L/2 (Sigma X) 
C  -------------------------------------------------------------------
c 	WRITE(66,*)'15 CONTR. ds TRAVERSES SEMELLES en X=L/2'

C	IF(K.EQ.1) DERIV=KSTT/2.
C	IJ=40+IT

C     SENS2(15,I,K,IPAN) = SENS2(15,I,K,IPAN) +
C     *   SP* E*LAMB*(-SENS1(5,I,K,IPAN)                               ! d(SIGMAY Sem)/dx (Sensib)
C     *            +LAMB*(SMT*SENS1(1,I,K,IPAN)+DERIV*Z2(2*IT+132)) )

C	IF (NZ==JJK) THEN
C	  DEN=CONC(IJ+300)												! CONC(IJ+300) = SIGMAY Sem trav
C	  IF(DABS(DEN).LE.(+1.E-10)) DEN =1.E-10
C        SENS2(15,I,K,IPAN)=SENS2(15,I,K,IPAN) *DEN/DABS(DEN)			! d(SIGMA COMP Sem)/dx  (Sensib)
C      ENDIF

c 	WRITE(66,*)'15. CONTR. ds TRAVERSES SEMELLES en X=L/2'
c 	WRITE(66,*)'   -- POINT= ',I, '  VAR= ',K,'  Panneau=',IPAN
c 	WRITE(66,*)'   --  IJ ',IJ, '  IT=',IT
c	WRITE(66,*)'      SP=',SP
c	WRITE(66,*)'      CONC(IJ+300)=',CONC(IJ+300)
c	WRITE(66,*)'      - SENS1(1,I,K,IPAN) =',SENS1(1,I,K,IPAN)
c	WRITE(66,*)'      - SENS1(5,I,K,IPAN) =',SENS1(5,I,K,IPAN)
c	WRITE(66,*)'      - Z2(2*IT+132)      =',Z2(2*IT+132)
c	WRITE(66,*)'      SENS2(15,I,K,IPAN)  =',SENS2(15,I,K,IPAN)

C  391 CONTINUE ! Boucle sur IPT (les pts de calcul des sensibilites sur le panneau)
C  392 CONTINUE ! si pas de traverse

 
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

C      IF(IPAN.EQ.NEL) THEN  
C          IF(K.EQ.1) THEN                        ! XI=DELTA
C           R1=EPSR*TMRR/OMT2
C	     DBB(K)=R1*COF1
C	     DAA(K)=-LAMB*(SN-E)*R1
C	     DCC(K)=DBB(K)/Q+KSRR*LAMB2*E*(0.5*OMT*TMRR-HX*EPSR)/OMT2
C          ELSE IF (K.EQ.6) THEN                  ! XI=HXR
C	     R1=-DELTA*EPSR*DXR/OMT2
C	     DBB(K)=R1*COF1
C	     DAA(K)=-LAMB*(SN-E)*R1
C	     DCC(K)=DBB(K)/Q
C     *             +KSRR*LAMB2*E*(OMT*(TMRR+DXR*DELTA/2.)-HX*DXR)/OMT2
C          ELSE IF (K.EQ.7) THEN                  ! XI=DXR=Tweb
C	     R1=-DELTA*EPSR*HXR/OMT2
C	     DBB(K)=R1*COF1
C	     DAA(K)=-LAMB*(SN-E)*R1
C	     DCC(K)=DBB(K)/Q+KSRR*LAMB2*E*HXR*(OMT*DABS(SMDR)-HX)/OMT2
C          ELSE IF (K.EQ.8) THEN                  ! XI=WXR =Lsem.
C	     R1=-DELTA*EPSR*TXR/OMT2
C	     DBB(K)=R1*COF1
C	     DAA(K)=-LAMB*(SN-E)*R1
C	     DCC(K)=DBB(K)/Q+KSRR*LAMB2*E*TXR*(OMT*DABS(SMCR)-HX)/OMT2
C          ELSE IF (K.EQ.9) THEN                  ! XI=EPSR
C	     R1=DELTA*TMRR/OMT2
C	     DBB(K)=R1*COF1
C	     DAA(K)=-LAMB*(SN-E)*R1
C	     DCC(K)=DBB(K)/Q-KSRR*LAMB2*E*HX*DELTA/OMT2
C	    ELSE
C	     DBB(K)=0.
C	     DAA(K)=0.
C	     DCC(K)=0.
C	    ENDIF
c	    WRITE(666,*)'Subr RESULT'
c	    WRITE(666,*)'----------  K = ',K
c	    WRITE(666,*)'AA,BB,CC   = ',AA,BB,CC	
c	    WRITE(666,*)'DAA,DBB,DCC= ',DAA(K),DBB(K),DCC(K)	
C	ENDIF

C      DO I=1,IPT
C	  IP=IPTS2(I,NEL)

c	  WRITE(666,*)'== IP  = ',IP
c	  WRITE(666,*)'Z2(IP+51)    U= ',Z2(IP+51)	
c	  WRITE(666,*)'Z2(IP+1479) DV= ',Z2(IP+1479)	
c	  WRITE(666,*)'Z2(IP+102)   W= ',Z2(IP+102)	
c	  WRITE(666,*)'SENS1(5,I,K,IPAN)  U= ',SENS1(5,I,K,IPAN)	
c	  WRITE(666,*)'SENS1(8,I,K,IPAN) DV= ',SENS1(8,I,K,IPAN)
c	  WRITE(666,*)'SENS1(1,I,K,IPAN)  W= ',SENS1(1,I,K,IPAN)

c       ! 1er partie de   d(Sx Moyen)/dx 
C        SENS3(21,I,K,IPAN)= SENS3(21,I,K,IPAN) + SPP* 		! d(Sx Moyen)/dx 
C     *				   (  AA*SENS1(5,I,K,IPAN)   
C     *					+ BB*SENS1(8,I,K,IPAN) 
C     *					+ CC*SENS1(1,I,K,IPAN) )
C       ! 2eme partie de   d(Sx Moyen)/dx 
C        IF(IPAN.EQ.NEL) THEN
C          SENS3(21,I,K,IPAN)= SENS3(21,I,K,IPAN) + SPP* 		 ! d(Sx Moyen)/dx suite
C     *                  (   DAA(K)*Z2(IP+51) 
C     *                    + DBB(K)*Z2(IP+1479) 
C     *                    + DCC(K)*Z2(IP+102)     )    
C        ENDIFC

c   	  WRITE(666,*)'SENS3(21,I,K,IPAN)=',SENS3(21,I,K,IPAN)
C	ENDDO
     
   
C  283 CONTINUE ! Boucle sur KK=1,NBRXI                                           		
C  284 CONTINUE ! Boucle sur les panneaux IPAN (liste des variables de conception)
  
C ******************************************************************************
C ******************************************************************************
	
C	IF ((NZ==JJK).and.(IPRINT.GE.1)) THEN
C	 WRITE(66,*) ' SENSIBILITES (contraintes) '
C       WRITE(66,*) ' ************************** '

C       DO 281 IPAN=1,NETO
C	   NBRXI=NVAR(IPAN)
C     DO 281 KK=1,NBRXI
C         K=NXIT(KK,IPAN)
C         WRITE(66,288) K,IPAN
C         WRITE(66,294) 
C         DO 290 J=1,12
C  290    WRITE(66,282) NOM3(J),(SENS2(J,I,K,IPAN),I=1,IPT)

C         IF(MT.GE.1) THEN
C           DO 292 J=13,15
C  292      WRITE(66,282) NOM3(J),(SENS2(J,I,K,IPAN),I=1,IPT)
C         ENDIF

C         WRITE(66,282) 'Contr. Sx moyenne-Raid',
C     *                  (SENS3(21,I,K,IPAN),I=1,IPT)
   
C  281  CONTINUE
C      ENDIF

C     Fin de la boucle (549) relative aux calculs des sensibilités des contraintes
  549 CONTINUE
  
      RETURN 
C ******************************************************************************
C 	LES FORMATS
C 	-----------
  282 FORMAT(A18,1X,5(1X,E13.6))
  288 FORMAT(' Variable de conception nø',I3,' du panneau nø',I3/
     *         50(1H-))
  294 FORMAT('     Fct.             Pt.1          Pt.2',
     *     '          Pt.3          Pt.4          Pt.5')
      END
