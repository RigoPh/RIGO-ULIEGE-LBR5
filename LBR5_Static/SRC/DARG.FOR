      SUBROUTINE DARGS(LAM,DEQU,Q,CONST,A,B,C,D,F,H,G,X,W,Z,BC,CW,BW,
     *        XW,DXY,WC,OXY,RXY,GH,ETA,ETA2,K,DA,NXI,NBRXI,SS,TT,ITYPE)	!février 2004

C     avec K = le numéro de la variables de conception (K=1,9)

      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 LAM
      DIMENSION CONST(74),LAM(8),DEQU(9),DA(9),NXI(9)

      COMMON/OPTI3/ CONST2(6,9),DARG(8,9)

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C       La routine DARG calcule les dérivées des coéfficients (EQU) de l
C     du 8ème ordre.
C
C  Modif: 14-5-95                                  Créer : 25-1-94
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       NXI(i) = LES NUMEROS DES VARIABLES SELECTIONNEES ( i=1,NBRXI )                                                       880
c          Bordage      1   Epaisseur du bordage
c          Aiguilles    2   Hauteur de l'âme des aiguilles
c                       3   Epaisseur de l'âme des aiguilles
c                       4   Largeur de la semelle des aiguilles
c                       5   Entredistance entre aiguilles
c          Raidisseurs  6   Hauteur de l'âme des raidisseurs
c                       7   Epaisseur de l'âme des raidisseurs
c                       8   Largeur de la semelle des raidisseur
c                       9   Entredistance entre raidisseurs
C  INPUT:
C      Var. de concept. = Epaisseur du bordage (K=1)
C      ---------------------------------------------
c        CONST(1)= D  -> CONST2(1,1)=d(D)/d(épaiss)
c        CONST(2)= K  -> CONST2(2,1)=d(K)/d(épaiss)
c        CONST(3)= Hy -> CONST2(3,1)=d(Hy)/d(épaiss)
c        CONST(4)= Ry -> CONST2(4,1)=d(Ry)/d(épaiss)
c        CONST(5)= Hx -> CONST2(5,1)=d(Hx)/d(épaiss)
c        CONST(6)= Rx -> CONST2(6,1)=d(Rx)/d(épaiss)

C      Var. de concept. relative aux aig.(k=2,5) et aux raid.(k=6,9)
C      ---------------------------------------------------------------
C      Constante      Dérivée(k=2,5)   Constante       Dérivée(k=6,9)
c    1 CONST(3)= Oy -> CONST2(1,k)     CONST( 9)= Ox -> CONST2(1,k)
c    2 CONST(4)= Hy -> CONST2(2,k)     CONST(10)= Hx -> CONST2(1,k)
c    3 CONST(5)= Sy -> CONST2(3,k)     CONST(11)= Sx -> CONST2(1,k)
c    4 CONST(6)= Ry -> CONST2(4,k)     CONST(12)= Rx -> CONST2(1,k)
c    5 CONST(7)= Ty -> CONST2(5,k)     CONST(13)= Tx -> CONST2(1,k)
c    6 CONST(8)= Ly -> CONST2(6,k)     CONST(14)= Lx -> CONST2(1,k)

C     Z= D*(1-ETA)/2+(Sx+Sy)            ETA2 = (1-ETA)/2
C     A= D*(1+ETA)/2+(Sx+Sy)            BC =(K+Rx)*(D+Ox)-(Hx)**2
C     B= (K+Rx)                         XW =(K+Ry)*(D+Oy)-(Hy)**2
C     C= (D+Ox)                         CW =(D+Ox)*(D+Oy)-(D*ETA)**2
C     D= 2K+(Tx+Ty)                     BW =(K+Rx)*(D+Oy)+(K+Ry)*(D+Ox)
C     F= (Lx+Ly)                        DXY=2D*(1.-ETA**2)+Ox+Oy
C     G= D*ETA                          OXY=2D+(Ox+Oy)
C     H= D+2(Sx+Sy)                     RXY=2K+(Rx+Ry)
C     X= (K+Ry)                         WC =Hx*(D+Oy)+Hy*(D+Ox)
C     W= (D+Oy)                         GH =(D+Ox)*(D+Oy)-D*ETA *(D+2(Sx

C  OUTPUT:
c     AETOIL=DA(1), BETOIL=DA(2), CETOIL=DA(3), DETOIL=DA(4), EETOIL=DA(
c     FETOIL=DA(6), GETOIL= 0     IETOIL=DA(7), JETOIL=DA(8), KETOIL=DA(

c      WRITE(66,*) 'Subr DARG','  K=',K

	IF(ITYPE.NE.5) THEN													!février 2004
        GOTO(1,2,2,2,2,3,3,3,3),K
	ELSE																!février 2004
	  GOTO (5,5,5,5,5),K												!février 2004
	ENDIF																!février 2004
C       Variable de conception nø1 : EPAISSEUR DU BORDAGE
C     __________________________________________________
   1    C1=CONST2(1,1)
        C2=CONST2(2,1)

      DA(1)=ETA2*BC*CONST2(1,1) + Z*( C*(CONST2(2,1)+CONST2(6,1))
     *                        +B*CONST2(1,1)-2.*CONST(10)*CONST2(5,1) )
      DA(3)=( (-ETA*F-CONST(10))*CONST(10) + (B*W+D*Z) - ETA*B*H )*
     *  CONST2(1,1) + C*((W+2.*Z)*CONST2(2,1)+(B+ETA2*D)*CONST2(1,1))
     *              - G*(H*CONST2(2,1)+B*CONST2(1,1))
     *              - (G*F+2.*CONST(10)*W)*CONST2(5,1) + GH*CONST2(6,1)
      DA(6)=CONST2(1,1) * ( (1.+ETA)*CONST(4)*CONST(10)
     *  + F*(CONST(4)+CONST(10)) + D*(OXY-ETA*H-G) + ETA2*BW )
     *  +2.*CONST2(2,1)*GH + Z*(OXY*CONST2(2,1)+RXY*CONST2(1,1))
     *  +2.*A* (CONST(10)*CONST2(3,1)+CONST(4)*CONST2(5,1))
     *  +   F* (   C     *CONST2(3,1)+   W    *CONST2(5,1))
     *  +   Z* (   C     *CONST2(4,1)+   W    *CONST2(6,1))
      DA(8)=((-ETA*F-CONST(4))*CONST(4)+(X*C+D*Z)-ETA*X*H )*CONST2(1,1)
     *              + W*((C+2.*Z)*CONST2(2,1)+(X+ETA2*D)*CONST2(1,1))
     *              - G*(H*CONST2(2,1)+X*CONST2(1,1))
     *              - (G*F+2.*CONST(4)*C)*CONST2(3,1) + GH*CONST2(4,1)
      DA(9)=ETA2*XW*CONST2(1,1) + Z*( W*(CONST2(2,1)+CONST2(4,1))
     *                        +X*CONST2(1,1)-2.*CONST(4)*CONST2(3,1) )
      IF(Q.GE.(1.E5)) THEN
C         CAS D'UNE PLAQUE RAIDIE  (Rayon très grand)
          DA(2)=0.D+00
          DA(4)=0.D+00
          DA(5)=0.D+00
          DA(7)=0.D+00
        ELSE
C         CAS D'UNE COQUE RAIDIE  (Rayon <100.000 m)
          DA(2)=2.*ETA*CONST(10)*(Z+CONST(1)*ETA2)*CONST2(1,1)/Q
     *        + 2.*G*Z*CONST2(5,1)/Q
          DA(4)=CONST2(1,1)*(ETA2*CW+Z*DXY)/(Q*Q)
          DA(5)=-( F*DXY+2.* (ETA2*WC+Z*(CONST(4)+CONST(10)) )  )
     *                                              * CONST2(1,1)/Q
     *         - 2.*Z* (W*CONST2(5,1)+C*CONST2(3,1)) /Q
          DA(7)= 2.*ETA*CONST(4)*CONST2(1,1)*(Z+CONST(1)*ETA2) /Q
     *         + 2.*G*Z*CONST2(3,1)/Q
      ENDIF
      GOTO 4

C       Variable de conception liées aux AIGUILLES : k=2,3,4,5
C     _____________________________________________________
   2    DOY=CONST2(1,K)
        DHY=CONST2(2,K)
        DSY=CONST2(3,K)
        DRY=CONST2(4,K)
        DTY=CONST2(5,K)
        DLY=CONST2(6,K)

        DA(1)= DSY * BC
      DA(3)=-G*(CONST(10)*DLY+2.*B*DSY) - CONST(10)*CONST(10)*DOY
     *        + C*(B*DOY+D*DSY+Z*DTY)
      DA(6)=2.*CONST(10)*(CONST(4)*DSY+A*DHY) + WC*DLY
     *  + DOY*(F*CONST(10)+D*C+Z*B) + F*C*DHY + (C*W-G*H)*DTY
     *  + (-2.*G*D+BW)*DSY + Z*C*DRY
      DA(8)=-G*(F*DHY+CONST(4)*DLY+H*DRY+2.*X*DSY) - 2.*C*DHY*CONST(4)
     *     + DOY*(X*C+D*Z) + W*(C*DRY+D*DSY+Z*DTY)
      DA(9)=DSY*XW + Z*(-2.*CONST(4)*DHY+W*DRY+X*DOY)
      IF(Q.GE.(1.E5)) THEN
C         CAS D'UNE PLAQUE RAIDIE  (Rayon très grand)
          DA(2)=0.D+00
          DA(4)=0.D+00
          DA(5)=0.D+00
          DA(7)=0.D+00
        ELSE
C         CAS D'UNE COQUE RAIDIE  (Rayon <10000.000 m)
          DA(2)=2.*G* CONST(10)*DSY /Q
          DA(4)=(CW*DSY+Z*C*DOY)/(Q*Q)
          DA(5)=-(CW*DLY+F*C*DOY+2.*(WC*DSY+Z*(CONST(10)*DOY+C*DHY)))/Q
          DA(7)= 2.*G* (Z*DHY+CONST(4)*DSY) /Q
      ENDIF
      GOTO 4

C       Variable de conception liées aux RAIDISSEURS : k=6,7,8,9
C     _______________________________________________________
   3    DOX=CONST2(1,K)
        DHX=CONST2(2,K)
        DSX=CONST2(3,K)
        DRX=CONST2(4,K)
        DTX=CONST2(5,K)
        DLX=CONST2(6,K)

      DA(1)=DSX*BC + Z*(-2.*CONST(10)*DHX+C*DRX+B*DOX)
      DA(3)=-G*(F*DHX+CONST(10)*DLX+H*DRX+2.*B*DSX) -2.*W*DHX*CONST(10)
     *     + DOX*(B*W+D*Z) + C*(W*DRX+D*DSX+Z*DTX)
      DA(6)=2.*CONST(4)*(CONST(10)*DSX+A*DHX) + WC*DLX
     *  + DOX*(F*CONST(4)+D*W+Z*X) + F*W*DHX + (C*W-G*H)*DTX
     *  + (-2.*G*D+BW)*DSX + Z*W*DRX
        DA(8)=-G*(CONST(4)*DLX+2.*X*DSX) - CONST(4)*CONST(4)*DOX
     *        + W*(X*DOX+D*DSX+Z*DTX)
      DA(9)=DSX*XW
      IF(Q.GE.(1.E5)) THEN
C         CAS D'UNE PLAQUE RAIDIE  (Rayon très grand)
          DA(2)=0.D+00
          DA(4)=0.D+00
          DA(5)=0.D+00
          DA(7)=0.D+00
        ELSE
C         CAS D'UNE COQUE RAIDIE  (Rayon <10000.000 m)
          DA(2)=2.*G* (Z*DHX+CONST(10)*DSX) /Q
          DA(4)=(CW*DSX+Z*W*DOX)/(Q*Q)
          DA(5)=-(CW*DLX+F*W*DOX+2.*(WC*DSX+Z*(CONST(4)*DOX+W*DHX)) )/Q
          DA(7)= 2.*G*CONST(4)*DSX /Q
      ENDIF
      GOTO 4
c	 Variable de conception liées aux épontilles : k=1,2,3,4,5			!février 2004
C	 -----------------------------------------------------------		!février 2004
C      Constante      Dérivée(k=1,5)   Constante							!février 2004
c    1 CONST(3)= Oy -> CONST2(1,k)     CONST( 9)= Ox						!février 2004
c    2 CONST(4)= Hy -> CONST2(2,k)     CONST(10)= Hx						!février 2004
c    3 CONST(5)= Sy -> CONST2(3,k)     CONST(11)= Sx						!février 2004
c    4 CONST(6)= Ry -> CONST2(4,k)     CONST(12)= Rx						!février 2004
c    5 CONST(7)= Ty -> CONST2(5,k)     CONST(13)= Tx						!février 2004
c    6 CONST(8)= Ly -> CONST2(6,k)     CONST(14)= Lx						!février 2004

    5   IF(ITYPE.NE.5) GOTO 4												!février 2004
        DOY=CONST2(1,K)													!février 2004
        DHY=CONST2(2,K)													!février 2004
        DSY=CONST2(3,K)													!février 2004
        DRY=CONST2(4,K)													!février 2004
        DTY=CONST2(5,K)													!février 2004
        DLY=CONST2(6,K)													!février 2004

      DA(1)= DSY *( CONST(12)*CONST(9)-CONST(10)**2)						!février 2004
      DA(3)=- CONST(10)*CONST(10)*DOY										!février 2004
     *        + CONST(9)*(CONST(12)*DOY+TT*DSY+SS*DTY)					!février 2004
      DA(6)=2.*CONST(10)*SS*DHY											!février 2004
     *  + DOY*(TT*CONST(9)+SS*CONST(12)) + CONST(9)*CONST(3)*DTY			!février 2004
     *  + (CONST(12)*CONST(3)+CONST(6)*CONST(9)							!février 2004
     *  +2*CONST(4)*CONST(10))*DSY + SS*CONST(9)*DRY						!février 2004
      DA(8)=DOY*(CONST(6)*CONST(9)+TT*SS)									!février 2004
     *   + CONST(3)*(CONST(9)*DRY+TT*DSY+SS*DTY)-2*CONST(4)*CONST(9)*DHY	!février 2004		
      DA(9)=DSY*(CONST(6)*CONST(3)-CONST(4)*CONST(4))						!février 2004
     *	 + SS*(CONST(3)*DRY+CONST(6)*DOY-2*CONST(4)*DHY)				!février 2004

          DA(2)=0.D+00													!février 2004
          DA(4)=0.D+00													!février 2004
         DA(5)=0.D+00														!février 2004
          DA(7)=0.D+00													!février 2004

    4 CONTINUE
      CALL ASSEMB(LAM,DEQU,DA(1),DA(2),DA(3),DA(4),DA(5),DA(6),DA(7),
     *                    DA(8),DA(9))

      RETURN
      END


      SUBROUTINE ASSEMB(LAM,EQU,AA,AB,AC,AD,AE,AF,AI,AJ,AK)

      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 LAM
      DIMENSION EQU(9),LAM(8)
C
C     Calcul des coéfficients de l'équation du 8éme ordre  (Cfr Subr. BO1)
C     INPUT : AA, .. AK              OUTPUT : EQU
C     Calcul des coéfficients de l'équation du 8éme ordre  (Cfr Subr. DARG)
C     INPUT : AA, .. AK              OUTPUT : DEQU

C     Version du : 16-3-94     			Créer 28-1-94

      EQU(1)=+AA*LAM(8)-AB*LAM(6)+AD*LAM(4)
      EQU(2)=0.D+00
      EQU(3)=-AC*LAM(6)+AE*LAM(4)
      EQU(4)=0.D+00
      EQU(5)=+AF*LAM(4)-AI*LAM(2)
      EQU(6)=0.D+00
      EQU(7)=-AJ*LAM(2)
      EQU(8)=0.D+00
      EQU(9)=AK
      RETURN
      END
