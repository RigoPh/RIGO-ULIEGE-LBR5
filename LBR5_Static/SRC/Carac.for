      SUBROUTINE CARAC(E,ETA,A,B,C,D,V,F,G,H,O,P,Q,R,S,T,U,Z,SH,FAM,MT,
     *                 KSR,KSA,KST,SPEC,PO,PHIL,RAYON,W,DELT,DELT2,IMPR,
     *				 EPAIS,ITYPE,ISECT,AIRE,AIy,AIx,Sredy,Sredx,TORK)	!12.05.04
      IMPLICIT REAL *8(A-H,O-Z)
      DIMENSION Z(74),FAM(6),R(10),S(10),T(10),U(10)
      COMMON /PY/PI
	COMMON/LANGUE/LANGUE
C**********************************************************************
C
C     SUBROUTINE CARACT
C     CALCUL DES COEFFICIENTS DE RAIDEUR DU PANNEAU.
C
C       A=EPSA, B=EPSR      C=DELTA,  W=WIDTH
C       D=HYA,  V=DYA,      F=WYA,  G=TYA,  (cadres)
C       H=HXR,  O=DXR,      P=WXR,  Q=TXR,  (raidisseurs)
C       R=HXTR, S=DXTR,     T=WXTR, U=TXTR, (traverses)
C
C     modif: 8-6-96                            Création: 1988 Thèse Ph. Rigo
c            7-11-2000 : calcul de DELT2
C***********************************************************************
c      CONST(1)= D       CONST(2)= K
c      CONST(3)= Oy      CONST( 9)= Ox
c      CONST(4)= Hy      CONST(10)= Hx
c      CONST(5)= Sy      CONST(11)= Sx
c      CONST(6)= Ry      CONST(12)= Rx
c      CONST(7)= Ty      CONST(13)= Tx
c      CONST(8)= Ly      CONST(14)= Lx

C     POT = Section des traverses (somme)
C     PO  = Poids total par metre (sans les nerv. complémentaires)

      POT=0.
      KKSA=(-1)**KSA
      KKSR=(-1)**KSR

	IF (ITYPE.NE.5) THEN									!février 2004
C     BORDAGE
      Z(1)=E*C/(1.-ETA*ETA)
      Z(2)=Z(1)*C*C/12.

C     CADRES
      Z(3)=E*(D*V+F*G)/A
      Z(4)=E*(D*V*(C/2.+D/2.)+F*G*(C/2.+D+G/2.))*KKSA/A
      TEMP=SH*5.*F*G/(6.*A)
      Z(5)=TEMP*FAM(1)
      Z(6)=E*(V*D**3/12.+V*D*(C/2.+D/2.)**2+F*G**3/12.+
     1          F*G*(C/2.+D+G/2.)**2)/A
      Z(7)=SH*(D*V**3+F*G**3)/(3.*A)
      Z(8)=-(C/2.+D+G/2.)*TEMP*FAM(2)*KKSA
	ELSE								!épontilles					!février 2004
C     BORDAGE															!février 2004
	GG=E/2															!février 2004
      Z(1)=0															!février 2004
      Z(2)=0															!février 2004
C     EPONTILLE														!février 2004
	IF(ISECT.EQ.3)THEN							! double T			!février 2004
	AIRE=2*D*V+2*F*G												!février 2004
	AIy=2*((F*((D+G)**3))-((F-V)*(D**3)))/3		!Iy  (axe fort)		!20.05.04	axe neutre à mi-hauteur
c	AIy=	2*(F*G**3)/12.+ V*(2*D)**3/12.+V*2*D*(D+G/2.)**2
c     *		+F*G*(2*D+G)**2											!Iy  (axe fort) axe neutre dans semelle 20.02.04
	AIx= (2*G*F*F*F)/12							!Ix  (axe faible)	!février 2004
	Sredy= V*2*D													!février 2004
	Sredx= 5*F*G/3													!février 2004
	TORK=((2*F*G*G*G)+(2*D*V*V*V))/3			!rigid de torsion	!février 2004

	ELSEIF(ISECT.EQ.1)THEN						!cercle				!février 2004
	DMIL=D - EPAIS													!février 2004
C	AIRE=PI*DMIL*EPAIS												!février 2004
	AIRE=(PI/4)*((D*D)-(D-2*EPAIS)**2)								!20.02.04
	AIy=(PI/64)*((D**4)-(D-2*EPAIS)**4)								!20.02.04
	AIx=AIy															!février 2004
	Sredy=AIRE/2													!février 2004
	Sredx=AIRE/2													!février 2004
	TORK=PI*DMIL*DMIL*DMIL*EPAIS/4									!février 2004
					
	ELSEIF(ISECT.EQ.2)THEN											!février 2004
	CM=D - EPAIS													!février 2004
	C1=D - 2*EPAIS													!février 2004
	AIRE=(D*D)-(C1*C1)												!20.02.04
	AIy=(D*D*D*D/12)-(C1*C1*C1*C1/12)								!février 2004
	AIx=AIy															!février 2004
	Sredy=2*EPAIS*C1												!février 2004
	Sredx=Sredy														!février 2004
	TORK=CM*CM*CM*EPAIS												!février 2004
	ENDIF															!février 2004

      Z(3)=E*AIRE/A		!OMEGAy										!février 2004
      Z(4)=0				!Hy par symétrie							!février 2004
      TEMP=GG*Sredy/A													!février 2004
C      Z(5)=TEMP*FAM(1)	!Sy											!février 2004
      Z(5)=TEMP*1										!26.07.05
      Z(6)=E*AIy/A		!Ry											!février 2004
      Z(7)=GG*TORK/A	    !Ty											!février 2004
      Z(8)=0				!Ly											!février 2004
	ENDIF															!février 2004
      
C     RAIDISSEURS
      Z(9)=E*(H*O+P*Q)/B
      Z(10)=E*(H*O*(C/2.+H/2.)+P*Q*(C/2.+H+Q/2.))/B*KKSR
      TEMP=SH*5.*P*Q/(6.*B)
      Z(11)=TEMP*FAM(3)
      Z(12)=E*(O*H**3/12.+O*H*(C/2.+H/2.)**2+P*Q**3/12.+
     1            P*Q*(C/2.+H+Q/2.)**2)/B
      Z(13)=SH*(H*O**3+P*Q**3)/(3.*B)
      Z(14)=-(C/2.+H+Q/2.)*TEMP*FAM(4)*KKSR

C     TRAVERSES
      IF(MT.EQ.0) GOTO 2
      KKST=(-1)**KST
      DO 1 I=1,MT
      J=6*(I-1)
      Z(15+J)=E*(R(I)*S(I)+T(I)*U(I))
      Z(16+J)=E*(R(I)*S(I)*(C/2.+R(I)/2.)+T(I)*U(I)*(C/2.+R(I)+U(I)/2.))
     *         *KKST
      TEMP=SH*5.*T(I)*U(I)/6.
      Z(17+J)=TEMP*FAM(5)
      Z(18+J)=E*(S(I)*R(I)**3/12.+R(I)*S(I)*(C/2.+R(I)/2.)**2+
     *           T(I)*U(I)**3/12.+T(I)*U(I)*(C/2.+R(I)+U(I)/2.)**2)
      Z(19+J)=SH*(R(I)*S(I)**3+T(I)*U(I)**3)/3.
      POT=POT+Z(15+J)
    1 Z(20+J)=-(C/2.+R(I)+U(I)/2.)*TEMP*FAM(6)*KKST
   2  CONTINUE

C     IMPRESSION DE "CONST"
      IF(IMPR.GE.-1) THEN						!15.10.05
 		IF(ITYPE.NE.5) THEN								!février 2004
        WRITE(66,15)                                                      
        WRITE(66,11)(Z(I),I=1,2)
        WRITE(66,11)(Z(I),I=3,14)
		ELSE											!février 2004
        WRITE(66,16)							            !février 2004              
        WRITE(66,11)(Z(I),I=3,8)							!février 2004
		ENDIF											!février 2004
        IF(MT.GE.1) THEN
          DO 20 J=1,MT
            K=9+6*J
            WRITE(66,11)(Z(I),I=K,K+5)
   20     CONTINUE
        ENDIF
	ENDIF

C     CALCUL DES POIDS   (SPEC =poids spécifique)
      SPECC=SPEC*W
 	IF(ITYPE.NE.5) THEN								!05.12.05
      PBORD=(C*RAYON*PHIL*PI/180.)*SPECC
      PAIG= (Z(3)*(RAYON+KKSA*D/2.)*PHIL*PI/180.)*SPECC/E
      PRAID=(Z(9)*RAYON*PHIL*PI/180.)*SPECC/E
      PTRAV=POT*SPECC/E
	ELSE												!05.12.05
      PBORD=0												!05.12.05
      PAIG= (Z(3)*(RAYON+KKSA*D/2.)*PHIL*PI/180.)*SPECC/E	!05.12.05
      PRAID=0												!05.12.05
      PTRAV=0												!05.12.05
	ENDIF												!05.12.05


      POTEL=PBORD+PAIG+PRAID+PTRAV
      PO=POTEL/W                   ! Poids du panneau par m selon X

      IF(LANGUE==1) THEN
        WRITE(66,12)PBORD,PAIG,PRAID,PTRAV,POTEL
	ELSE
        WRITE(66,13)PBORD,PAIG,PRAID,PTRAV,POTEL
	ENDIF

C    CALCUL DE L'EPAISSEUR MOYENNE COMPTE TENU DES TRAV. ET DES RAID.

      DELT=  POT/(PHIL*RAYON*PI/180.)/E +Z(9)/E +C

C    CALCUL DE L'EPAISSEUR MOYENNE COMPTE TENU DES TRAV., RAID. et CADRES

      DELT2= DELT + Z(3)/E 

      RETURN

   11 FORMAT(6(1X,E11.4))
   12 FORMAT(//' POIDS A SEC (non déjaugé)'/T2,24(1H-)/
     *     ' 1- Poids du bordage      = ',E11.4, 'N.'/
     *     ' 2- Poids des cadres      = ',E11.4, 'N.'/
     *     ' 3- Poids des raidisseurs = ',E11.4, 'N.'/
     *     ' 4- Poids des traverses   = ',E11.4, 'N.'/
     *     ' POIDS TOTAL (1-4) (sans nervures second.) = ',E11.4,' N'/)
   13 FORMAT(//' DRY WEIGHT (out of water)'/T2,24(1H-)/
     *     ' 1- Weight of Plating      = ',E11.4, 'N.'/
     *     ' 2- Weight of Frames       = ',E11.4, 'N.'/
     *     ' 3- Weight of Stiffeners   = ',E11.4, 'N.'/
     *     ' 4- Weight of Girders      = ',E11.4, 'N.'/
     * ' TOTAL WEIGHT (1-4) (without secondary members) = ',E11.4,' N'/)
   15 FORMAT(/' COEFFICIENTS DE RAIDEUR DU PANNEAU',/35(1H+)/
     * ' (ILS CORRESPONDENT SUCCESSIVEMENT A )'/
     * T10,'D K'/T10,'OMYR HYR SYR RYR TYR LYR'/T10,
     * 'OMXR HXR SXR RXR TXR LXR '/T10,'OMXTR*DX HXTR*DX ',
     * 'SXTR*DX RXTR*DX TXTR*DX LXTR*DX'/T10,'( 60 VALEURS EN TR POUR ',
     * 'MAXIMUM 10 TRAVERSES )',/)
   16 FORMAT(/' COEFFICIENTS DE RAIDEUR DU PANNEAU',/35(1H+)/					!février 2004
     * ' (ILS CORRESPONDENT SUCCESSIVEMENT A )'/								!février 2004
     * T10,'OMYR HYR SYR RYR TYR LYR'/T10,'(IL N''Y A PAS DE RAIDISSEURS		!février 2004
     * SUR L''EPONTILLE)',/)													!février 2004

      END
