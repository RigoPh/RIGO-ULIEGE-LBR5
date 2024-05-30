      SUBROUTINE CARACD(E,ETA,A,B,C,D,V,F,G,H,O,P,Q,CONST,SH,
     *  FAM,MT,KSR,KSA,KST,PHIL,RAYON,W,IMPR,CONST2,CONST3,
     *  EPAIS,ITYPE,ISECT,dPO,SPEC)						!février 2004,!!!aout04

      IMPLICIT REAL *8(A-H,O-Z)
      DIMENSION CONST(74),CONST2(6,9),CONST3(2,10),FAM(6)

	DIMENSION dPBORD(9),dPAIG(9),dPRAID(9),dPTRAV(9),dPO(9)  !!!aout04

      COMMON /PY/PI
C***********************************************************************
C     SUBROUTINE CARACT(opti)
C     CALCUL DES DERIVEES DES COEFFICIENTS DE RAIDEUR DU PANNEAU
C     par rapport aux variables de conception.
C
C       A=EPSA, B=EPSR     C=DELTA,  W=WIDTH
C       D=HYA,  V=DYA,      F=WYA,  G=TYA,  (aiguilles)
C       H=HXR,  O=DXR,      P=WXR,  Q=TXR,  (raidisseurs)
C
C     modif : 23-8-95                                   Créer : 24-1-94
C***********************************************************************
C      Var. de concept. = Epaisseur du bordage (K=1)
C      ---------------------------------------------
c        CONST(1) = D  -> CONST2(1,1)=d(D)/d(épaiss)
c        CONST(2) = K  -> CONST2(2,1)=d(K)/d(épaiss)
c        CONST(4) = Hy -> CONST2(3,1)=d(Hy)/d(épaiss)   aiguille
c        CONST(6) = Ry -> CONST2(4,1)=d(Ry)/d(épaiss)   aiguille
c        CONST(10)= Hx -> CONST2(5,1)=d(Hx)/d(épaiss)   raidisseur
c        CONST(12)= Rx -> CONST2(6,1)=d(Rx)/d(épaiss)   raidisseur

c        Traverses  Hx -> CONST3(1,MT)=d(H)/d(épaiss)
c                   Rx -> CONST3(2,MT)=d(R)/d(épaiss)

C      Var. de concept. relative aux Aig.(k=2,5) et aux Raid.(k=6,9).
C      --------------------------------------------------------------
C           AIGUILLES                         RAIDISSEURS
C      Constante           Dérivée(k=2,5)    Constante   Dérivée(k=6,9)
C    1 CONST(3)= Oy -> CONST2(1,k)     CONST( 9)= Ox -> CONST2(1,k)
C    2 CONST(4)= Hy -> CONST2(2,k)     CONST(10)= Hx -> CONST2(2,k)
C    3 CONST(5)= Sy -> CONST2(3,k)     CONST(11)= Sx -> CONST2(3,k)
C    4 CONST(6)= Ry -> CONST2(4,k)     CONST(12)= Rx -> CONST2(4,k)
C    5 CONST(7)= Ty -> CONST2(5,k)     CONST(13)= Tx -> CONST2(5,k)
C    6 CONST(8)= Ly -> CONST2(6,k)     CONST(14)= Lx -> CONST2(6,k)

C       WRITE(66,*) 'A=',A
C       WRITE(66,*) 'B=',B
C       WRITE(66,*) 'C=',C
C       WRITE(66,11) CONST

        BKSA=1.
        IF(KSA.EQ.1)BKSA=-1.
        BKSR=1.
        IF(KSR.EQ.1)BKSR=-1.
	IF (ITYPE.NE.5) THEN											!février 2004
C  - Dérivée par rapport à l'épaisseur (Delta=C) : CONST2(i,1) i=1,2
      IF(C.LE.0.000001)  GOTO 13
        CONST2(1,1)=CONST(1)/C
      CONST2(2,1)=3.*CONST(2)/C
        CONST2(3,1)=BKSA* CONST(3)/2.
      CONST2(4,1)=DABS(CONST(4))
        CONST2(5,1)=BKSR* CONST(9)/2.
      CONST2(6,1)=DABS(CONST(10))

C       AIGUILLES (dérivées des CONST(3) à CONST(8) )
C  - Dérivée par rapport à H(âme) (D=HYA) : CONST2(i,2) i=1,6

  13    IF(D.LE.0.000001)  GOTO 7
        CONST2(1,2)=E*V/A
      CONST2(2,2)=E*(D*V+F*G+V*C*0.5)*BKSA/A
      CONST2(4,2)=(V*(C/2.+D)**2+2.*F*G*(C/2.+D+G/2.))*E/A
      CONST2(5,2)=(SH*V**3)/(3.*A)
      CONST2(6,2)=-CONST(5)*BKSA*FAM(2)
C       - Dérivée par rapport à Ep(âme) (V=DYA) : CONST2(i,3) i=1,6
   7    IF(V.LE.0.000001)  GOTO 3
        CONST2(1,3)=E*D/A
      CONST2(2,3)=E*D*(C/2.+D/2.)*BKSA/A
      CONST2(4,3)=D*(D*D/12.+(C/2.+D/2.)**2)*E/A
      CONST2(5,3)=SH*D*V*V/A
C       - Dérivée par rapport à L(semelle) (F=WYA) : CONST2(i,4) i=1,6
   3  IF(F.LE.0.000001)  GOTO 4
        CONST2(1,4)=E*G/A
      CONST2(2,4)=E*G*(C/2.+D+G/2.) *BKSA/A
      CONST2(3,4)=CONST(5)/F
      CONST2(4,4)=E*G*(G*G/12.+(C/2.+D+G/2.)**2) /A
      CONST2(5,4)=(SH*G**3)/(3.*A)
      CONST2(6,4)=CONST(8)/F
C	- Dérivée par rapport à l'entredistance (A=EPSA) : CONST2(i,5) i=1,6	
   4  IF(A.LE.0.000001)  GOTO 10  ! Modifié en avril 2003 (A au lieu de F)
      CONST2(1,5)=-CONST(3)/A                                                
      CONST2(2,5)=-CONST(4)/A                                 
      CONST2(3,5)=-CONST(5)/A                                    
      CONST2(4,5)=-CONST(6)/A                                     
      CONST2(5,5)=-CONST(7)/A                                      
      CONST2(6,5)=-CONST(8)/A                                      
	
C	RAIDISSEURS	 (dérivées des CONST(9) à CONST(14) )
C	- Dérivée par rapport à H(âme) (H=HXR) : CONST2(i,6) i=1,6
  10	IF(H.LE.0.000001)  GOTO 8
        CONST2(1,6)=E*O/B
      CONST2(2,6)=E*(O*H+P*Q+O*C*0.5)*BKSR/B
      CONST2(4,6)=(O*(C/2.+H)**2+2.*P*Q*(C/2.+H+Q/2.))*E/B
      CONST2(5,6)=(SH*O**3)/(3.*B)
      CONST2(6,6)=-CONST(11)*BKSR*FAM(4)

C       - Dérivée par rapport à Ep(âme) (O=DXR) : CONST2(i,7) i=1,6
    8 IF(O.LE.0.000001)  GOTO 5
        CONST2(1,7)=E*H/B
      CONST2(2,7)=E*H*(C/2.+H/2.)*BKSR/B
      CONST2(4,7)=H*(H*H/12.+(C/2.+H/2.)**2)*E/B
      CONST2(5,7)=SH*H*O*O/B
C       - Dérivée par rapport à L(semelle) (P=WXR) : CONST2(i,8) i=1,6
    5 IF(P.LE.0.000001)  GOTO 6
        CONST2(1,8)=E*Q/B
      CONST2(2,8)=E*Q*(C/2.+H+Q/2.) *BKSR /B
      CONST2(3,8)=CONST(11)/P
      CONST2(4,8)=E*Q*(Q*Q/12.+(C/2.+H+Q/2.)**2) /B
      CONST2(5,8)=(SH*Q**3)/(3.*B)
      CONST2(6,8)=CONST(14)/P
C	- Dérivée par rapport à l'entredistance (B=EPSR) : CONST2(i,9) i=1,6	
    6 IF(B.LE.0.000001)  GOTO 9
      CONST2(1,9)=-CONST(9)/B                                               
      CONST2(2,9)=-CONST(10)/B                                 
      CONST2(3,9)=-CONST(11)/B                                    
      CONST2(4,9)=-CONST(12)/B                                     
      CONST2(5,9)=-CONST(13)/B                                      
      CONST2(6,9)=-CONST(14)/B                                      
	ELSE															!février 2004
c	- Dans le cas des épontille nous travaillons avec de nouvelles variables	!février 2004
c			k=1 --> HYA												!février 2004
C			k=2 --> DYA												!février 2004
C			k=3 --> WYA												!février 2004
C			k=4 --> EPAIS											!février 2004
C			k=5 --> EPSA											!février 2004
C	- Dérivée par rapport HYA pour épontilles						!février 2004
      IF(D.LE.0.000001)  GOTO 15										!février 2004
      	IF (ISECT.EQ.3) THEN										!février 2004
	CONST2(1,1)=2*E*V/A								!dOy			!février 2004
      CONST2(2,1)=0									!dHy			!février 2004
      CONST2(3,1)=E*V/A								!dSy			!février 2004
      CONST2(4,1)=(2*E/A)*((F*(D+G)**2)-((F-V)*D**2))	!dRy			!février 2004
      CONST2(5,1)=E*V*V*V/(3*A)						!dTy			!février 2004
		ELSEIF (ISECT.EQ.1) THEN									!février 2004
	CONST2(1,1)=PI*E*EPAIS/A										!février 2004
      CONST2(2,1)=0													!février 2004
      CONST2(3,1)=PI*E*EPAIS/(4*A)									!février 2004
      CONST2(4,1)=(PI*E/(16*A))*((D**3)-(D-2*EPAIS)**3)				!février 2004
      CONST2(5,1)=CONST2(4,1)											!février 2004
		ELSEIF (ISECT.EQ.2) THEN									!février 2004
	CONST2(1,1)=4*E*EPAIS/A											!février 2004
      CONST2(2,1)=0													!février 2004
      CONST2(3,1)=E*EPAIS/(A)											!février 2004
      CONST2(4,1)=E*(D*D*D-((D-2*EPAIS)**3))/(3*A)					!février 2004
      CONST2(5,1)=E*EPAIS*3*(D-EPAIS)*(D-EPAIS)/(2*A)					!février 2004
		ENDIF														!février 2004
C	-Dérivée par rapport à DYA pour épontille						!février 2004
 15   IF(V.LE.0.000001)  GOTO 16										!février 2004
      CONST2(1,2)=2*E*D/A												!février 2004
      CONST2(2,2)=0													!février 2004
      CONST2(3,2)= E*D/A												!février 2004
	CONST2(4,2)=2*E*D*D*D/(3*A)										!février 2004
      CONST2(5,2)=E*D*V*V/A											!février 2004
C       - Dérivée par rapport à WYA pour épontille					!février 2004
  16  IF(F.LE.0.000001)  GOTO 17										!février 2004
      CONST2(1,3)=2*E*G/A												!février 2004
      CONST2(2,3)=0													!février 2004
      CONST2(3,3)=0													!février 2004
      CONST2(4,3)=(2*E/(3*A))*(((D+G)**3)-D**3)						!février 2004
      CONST2(5,3)=E*G*G*G/(3.*A)										!février 2004
C	  -Dérivée par rapport à EPAIS pour épontille					!février 2004
  17  IF(EPAIS.LE.0.000001)  GOTO 18									!février 2004
        IF(ISECT.EQ.1) THEN											!février 2004
	CONST2(1,4)=PI*E*(D-2*EPAIS)/A									!février 2004
      CONST2(2,4)=0													!février 2004
      CONST2(3,4)=PI*E*(D-2*EPAIS)/(4*A)								!février 2004
      CONST2(4,4)=(E*PI/(8*A))*(D-2*EPAIS)**3							!février 2004
      CONST2(5,4)=CONST2(4,4)											!février 2004
		ELSEIF(ISECT.EQ.2) THEN										!février 2004
	CONST2(1,4)=4*E*(D-2*EPAIS)/A									!février 2004
      CONST2(2,4)=0													!février 2004
      CONST2(3,4)=E*(D-4*EPAIS)/A										!février 2004
      CONST2(4,4)=E*2*((D-2*EPAIS)**3)/(3*A)							!février 2004
      CONST2(5,4)=E*(((D-EPAIS)**2)*(D-4*EPAIS))/(2*A)				!février 2004
		ELSE														!février 2004
	WRITE(66,*) 'ERREUR cette section n''a pas de variable'' EPAIS'	!février 2004		
	stop															!février 2004	
		ENDIF														!février 2004
C	- Dérivée par rapport à l'entredistance EPSA pour épontille		!février 2004
   18  IF(A.LE.0.000001)  GOTO 9  									!février 2004
      CONST2(1,5)=-CONST(3)/A											!février 2004	                    
      CONST2(2,5)=-CONST(4)/A											!février 2004	
      CONST2(3,5)=-CONST(5)/A											!février 2004		
      CONST2(4,5)=-CONST(6)/A											!février 2004
      CONST2(5,5)=-CONST(7)/A											!février 2004
	ENDIF															!février 2004
   9  CONTINUE

C   LES TRAVERSES
c   ----------------
	BKST=1.
        IF(KST.EQ.1)BKST=-1.
      IF(MT.EQ.0) GOTO 2
      DO 1 I=1,MT
      J=6*(I-1)
      CONST3(1,I)=CONST(15+J)*BKST/2.        ! H trav
      CONST3(2,I)=DABS(CONST(16+J))          ! R trav
    1 CONTINUE
    2 CONTINUE

C     CALCUL DE LA DERIVEE DES POIDS  !!!aout04 (ainsi que tout ce qui suit)
c     ------------------------------
	
	KKSA=(-1)**KSA
      SPECC=SPEC*W
	CALL ANNULD(dPO,9)
	CALL ANNULD(dPBORD,9)
	CALL ANNULD(dPAIG,9)
	CALL ANNULD(dPRAID,9)
	CALL ANNULD(dPTRAV,9)

C  - Dérivée par rapport à l'épaisseur (Delta=C) :	
      dPBORD(1)=(RAYON*PHIL*PI/180.)*SPECC
C  - Dérivée par rapport à H(âme) (D=HYA) :
      dPAIG(2)=((CONST2(1,2)*(RAYON+KKSA*D/2.)+ CONST(3)*KKSA/2.)
     *			*PHIL*PI/180.) *SPECC/E
C  - Dérivée par rapport à Ep(âme) (V=DYA) :
      dPAIG(3)=(CONST2(1,3)*(RAYON+KKSA*D/2.)*PHIL*PI/180.)*SPECC/E
C  - Dérivée par rapport à L(semelle) (F=WYA) :
      dPAIG(4)=(CONST2(1,4)*(RAYON+KKSA*D/2.)*PHIL*PI/180.)*SPECC/E
C  - Dérivée par rapport à l'entredistance (A=EPSA) :
	dPAIG(5)=(CONST2(1,5)*(RAYON+KKSA*D/2.)*PHIL*PI/180.)*SPECC/E
C  - Dérivée par rapport à H(âme) (H=HXR) :
	dPRAID(6)=(CONST2(1,6)*RAYON*PHIL*PI/180.)*SPECC/E
C  - Dérivée par rapport à Ep(âme) (O=DXR) :
	dPRAID(7)=(CONST2(1,7)*RAYON*PHIL*PI/180.)*SPECC/E
C  - Dérivée par rapport à L(semelle) (P=WXR) :
	dPRAID(8)=(CONST2(1,8)*RAYON*PHIL*PI/180.)*SPECC/E
C  - Dérivée par rapport à l'entredistance (B=EPSR) :
	dPRAID(9)=(CONST2(1,9)*RAYON*PHIL*PI/180.)*SPECC/E

c      PAIG= (Z(3)*(RAYON+KKSA*D/2.)*PHIL*PI/180.)*SPECC/E
c      PRAID=(Z(9)*RAYON*PHIL*PI/180.)*SPECC/E
c      PTRAV=POT*SPECC/E

	DO I=1,9
	  dPO(I)=(dPBORD(I)+dPAIG(I)+dPRAID(I)+dPTRAV(I))/W
      ENDDO

c      POTEL=PBORD+PAIG+PRAID+PTRAV
c      PO=POTEL/W                   ! Poids du panneau par m selon X

c     Fin calcul dérivée des poids  !!!aout04
c     ----------------------------------------------------------------------


      RETURN
      END
