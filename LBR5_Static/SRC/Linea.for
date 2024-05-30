      SUBROUTINE LINEA(M,COEFF,DCOEFF,J1,J2,J3,J4,X,SX,K,MT,IP,
     *                 ISYM,IMPR,Q2PI,DPX,PX,DARG,NXI,NBRXI,
     *                 AUX2,J,JK,IOPTI)

      IMPLICIT REAL *8(A-H,O-Z)
      DIMENSION COEFF(32,41),SX(360),IP(4),X(464),K(4),DCOEFF(32,33,9),
     *          DPX(8,9),PX(8),DARG(8,9),NXI(9),
     *          AUX2(400),J(4),JK(4)

      COMMON AUX(400),Z(360)
      COMMON /PY/PI,SINU(4),COSI(4),EXPT(4)
c      COMMON /ALLONG/AUX2(400),J(4),JK(4)

C***********************************************************************
C     SUBROUTINE LINEA
C     ================
C     SUBROUTINE DE CALCUL DES EFFETS D'UNE CHARGE LINEAIRE EN X=0.
C     Soit les 4 cas de base (coque non raidie et infinie)
C     LES DONNEES SE TROUVENT DANS COEFF ET LES RESULTATS SE TROUVENT
C     DANS X (DISC, DISB, DISA et DISD).

C     DPX contient les dérivées des solutions PX
C          c.à.d. A,B,C,D correspondant à 1 cas de sollicitation)

C     modif: 12-6-95                                     Créer : 8-2-94
C            13-4-99 (LBR5.1)
C***********************************************************************
      DO 111 I=1,464
  111 X(I)=0.

      J(1)=J1
      J(2)=J2
      J(3)=J3
      J(4)=J4

      Z(1)=0.
      Z(2)=0.
      Z(3)=0.
      Z(4)=-5000.

      DO 11 I=1,4
      AUX(I)=COEFF(IP(1),J(I))-EXPT(K(1))*(COEFF(IP(1),J(I))*COSI(K(1))
     *                                    -COEFF(IP(2),J(I))*SINU(K(1)))
      AUX(I+20)=COEFF(IP(2),J(I))-EXPT(K(2))*(COEFF(IP(2),J(I))*
     *       COSI(K(2))+COEFF(IP(1),J(I))*SINU(K(2)))
      AUX(I+40)=COEFF(IP(3),J(I))-EXPT(K(3))*(COEFF(IP(3),J(I))*
     *       COSI(K(3))-COEFF(IP(4),J(I))*SINU(K(3)))
      AUX(I+60)=COEFF(IP(4),J(I))-EXPT(K(4))*(COEFF(IP(4),J(I))*
     *       COSI(K(4))+COEFF(IP(3),J(I))*SINU(K(4)))
      IF(IMPR.EQ.0) GOTO 11
      WRITE(66,'(I3,4E12.5)') I,AUX(I),AUX(20+I),AUX(40+I),AUX(60+I)
   11 CONTINUE

      DO 13 I=1,400
   13 AUX2(I)=AUX(I)

      CALL SYSTEM(4,1,SX,20)

      IF(IMPR.EQ.0) GOTO 112
      WRITE(66,*) 'terme indép.'
      WRITE(66,'(4E12.5)') (SX(IK),IK=1,4)
      WRITE(66,*)  ' '
  112 CONTINUE

      GO TO(99,17,18,19),M
   18 PX(1)=SX(1)
      PX(2)=0.
      PX(3)=SX(2)
      PX(4)=0.
      PX(5)=SX(3)
      PX(6)=SX(4)
      GO TO 21
   19 DO 20 IK=1,4
        PX(2*IK)=0.
        PX(2*IK-1)=SX(IK)
   20 CONTINUE
      GO TO 21
   17 DO 22 IK=1,4
        PX(IK)=SX(IK)
   22 CONTINUE
   21 CONTINUE

C     ISYM = 1  : si cas de charge symétrique   (DISC et DISB)
C     ISYM = -1 : si cas de charge antimétrique (DISA et DISD)
      CALL FILLX(COEFF,PX,X, -3, 2,M,ISYM)
      CALL FILLX(COEFF,PX,X, 13, 1,M,ISYM)
      CALL FILLX(COEFF,PX,X, 29,16,M,ISYM)
      CALL FILLX(COEFF,PX,X,365, 8,M,ISYM)
      CALL FILLX(COEFF,PX,X,381, 4,M,ISYM)
      CALL FILLX(COEFF,PX,X,397,14,M,ISYM)
      CALL FILLX(COEFF,PX,X,413, 6,M,ISYM)
      CALL FILLX(COEFF,PX,X,429,12,M,ISYM)
      CALL FILLX(COEFF,PX,X,445, 3,M,ISYM)

      IF(MT.EQ.0) GOTO 10
      DO 2 IK=1,MT
        JJ=2*(IK-1)
        IJT1=17+JJ
        IJT2=18+JJ
        JI1=45+32*(IK-1)
        JI2=61+32*(IK-1)
        CALL FILLX(COEFF,PX,X,JI1,IJT1,M,ISYM)
        CALL FILLX(COEFF,PX,X,JI2,IJT2,M,ISYM)
    2 CONTINUE
   10 CONTINUE

C     DEUXIEME PARTIE (calcul des pseudo-charges et résolution)
C     ----------------
      IF(IOPTI.EQ.0) GOTO 99

      DO 50 IK=1,4
C     1 2   3   4  5   6   7  8  9  10  11  12 13 14 15 16
C     U V DW/DY NY NX NYX NXY MY MX MYX MXY QY QX RY RX  W
C     1 2   3   4      5      6                   7      8
C     U V DW/DY NY    NYX     MY                  RY     W
      JK(IK)=J(IK)
      IF(J(IK).EQ.6)  JK(IK)=5
      IF(J(IK).EQ.8)  JK(IK)=6
      IF(J(IK).EQ.14) JK(IK)=7
      IF(J(IK).EQ.16) JK(IK)=8
   50 CONTINUE

C     Q2PI = 2 x PI x Q

C     Termes indépendants (pseudo charge)
      DO 32 L=1,NBRXI
      KL=NXI(L)
c      WRITE(66,*) 'Dérivée de la matrice pour K= ',KL
      DO 12 IK=1,4
c       WRITE(66,*) 'DCOEFF(IP(1),JK(IK),KL)= ',DCOEFF(IP(1),JK(IK),KL)
c       WRITE(66,*) 'EXPT(K(1))             = ',EXPT(K(1))
c       WRITE(66,*) 'COSI(K(1)) et Q2PI     = ',COSI(K(1)), Q2PI
c       WRITE(66,*) 'DARG(2*K(1)-1,KL)      = ',DARG(2*K(1)-1,KL)
c       WRITE(66,*) 'COEFF(IP(2),J(IK))     = ',COEFF(IP(2),J(IK))
      AUX(IK)=    DCOEFF(IP(1),JK(IK),KL) - EXPT(K(1)) *
     * (  COSI(K(1)) * ( Q2PI* (-DARG(2*K(1)-1,KL)*COEFF(IP(1),J(IK))
     *                          -DARG(2*K(1),KL)  *COEFF(IP(2),J(IK)) )
     *                   + DCOEFF(IP(1),JK(IK),KL) )
     *  + SINU(K(1)) * ( Q2PI* (+DARG(2*K(1)-1,KL)*COEFF(IP(2),J(IK))
     *                          -DARG(2*K(1),KL)  *COEFF(IP(1),J(IK)) )
     *                   - DCOEFF(IP(2),JK(IK),KL) )   )
      AUX(IK+20)= DCOEFF(IP(2),JK(IK),KL) - EXPT(K(2)) *
     * (  COSI(K(2)) * ( Q2PI* (-DARG(2*K(2)-1,KL)*COEFF(IP(2),J(IK))
     *                          +DARG(2*K(2),KL)  *COEFF(IP(1),J(IK)) )
     *                   + DCOEFF(IP(2),JK(IK),KL) )
     *  + SINU(K(2)) * ( Q2PI* (-DARG(2*K(2)-1,KL)*COEFF(IP(1),J(IK))
     *                          -DARG(2*K(2),KL)  *COEFF(IP(2),J(IK)) )
     *                   + DCOEFF(IP(1),JK(IK),KL) )   )
      AUX(IK+40)= DCOEFF(IP(3),JK(IK),KL) - EXPT(K(3)) *
     * (  COSI(K(3)) * ( Q2PI* (-DARG(2*K(3)-1,KL)*COEFF(IP(3),J(IK))
     *                          -DARG(2*K(3),KL)  *COEFF(IP(4),J(IK)) )
     *                   + DCOEFF(IP(1),JK(IK),KL) )
     *  + SINU(K(3)) * ( Q2PI* (+DARG(2*K(3)-1,KL)*COEFF(IP(4),J(IK))
     *                          -DARG(2*K(3),KL)  *COEFF(IP(3),J(IK)) )
     *                   - DCOEFF(IP(4),JK(IK),KL) )   )
      AUX(IK+60)= DCOEFF(IP(4),JK(IK),KL) - EXPT(K(4)) *
     * (  COSI(K(4)) * ( Q2PI* (-DARG(2*K(4)-1,KL)*COEFF(IP(4),J(IK))
     *                          +DARG(2*K(4),KL)  *COEFF(IP(3),J(IK)) )
     *                   + DCOEFF(IP(4),JK(IK),KL) )
     *  + SINU(K(4)) * ( Q2PI* (-DARG(2*K(4)-1,KL)*COEFF(IP(3),J(IK))
     *                          -DARG(2*K(4),KL)  *COEFF(IP(4),J(IK)) )
     *                   + DCOEFF(IP(3),JK(IK),KL) )   )
c     IF(IMPR.EQ.0) GOTO 12
c      WRITE(66,*) IK,AUX(IK),AUX(20+IK),AUX(40+IK),AUX(60+IK)
   12 CONTINUE
c      WRITE(66,*) 'X= ',SX(1),SX(2),SX(3),SX(4)

      KK=(L-1)*20
      DO 31 II=1,4
        Z(II+KK)=-( AUX(II)*SX(1)+AUX(II+20)*SX(2)+AUX(II+40)*SX(3)
     *                           +AUX(II+60)*SX(4) )
   31 CONTINUE
c      WRITE(66,*)'Terme ind. =',Z(1+KK),Z(2+KK),Z(3+KK),Z(4+KK)
   32 CONTINUE

C     Corps de la Matrice (idem cas normal)
      DO 14 IK=1,400
   14 AUX(IK)=AUX2(IK)

      CALL SYSTEM(4,9,SX,20)

      DO  46 L=1,NBRXI
        KL=NXI(L)
        LL=(L-1)*20
        GO TO(99,40,41,42),M
   41   DPX(1,KL)=SX(LL+1)
        DPX(2,KL)=0.
        DPX(3,KL)=SX(LL+2)
        DPX(4,KL)=0.
        DPX(5,KL)=SX(LL+3)
        DPX(6,KL)=SX(LL+4)
        GO TO 45
   42   DO 43 IK=1,4
          DPX(2*IK,KL)=0.
          DPX(2*IK-1,KL)=SX(LL+IK)
   43   CONTINUE
        GO TO 45
   40   DO 44 IK=1,4
          DPX(IK,KL)=SX(LL+IK)
   44   CONTINUE
   45   CONTINUE
        IF(IMPR.NE.0) WRITE(66,47) KL,(DPX(I,KL),I=1,8)
   46 CONTINUE

   47 FORMAT(' XI=',I2,8(1X,E14.7))

   99 RETURN
      END



      SUBROUTINE FILLX(COEFF,PX,X,K,L,M,ISYM)
      IMPLICIT REAL *8(A-H,O-Z)
      DIMENSION COEFF(32,41),PX(8),X(464)
C***********************************************************************
C
C     SUBROUTINE FILLX
C     SUBROUTINE DE REMPLISSAGE DES VECTEURS SOLUTIONS (DISC, DISCA, etc
C     A PARTIR DES SOLUTIONS DES SYSTEMES D'EQUATIONS. C'EST UNE SUBROUT
C     DE RANGEMENT ET DE CREATION DES COEFFICIENTS.
C     modif: 8-2-94
C***********************************************************************
      DO 24 I=1,M
      I2=I*2
      I8=I*8
      N=4*I+K
      X(N)  = COEFF(I8-7,L)*PX(I2-1)+COEFF(I8-6,L)*PX(I2)
      X(N+1)= COEFF(I8-5,L)*PX(I2-1)+COEFF(I8-4,L)*PX(I2)
      X(N+2)=(COEFF(I8-3,L)*PX(I2-1)+COEFF(I8-2,L)*PX(I2))*ISYM
   24 X(N+3)=(COEFF(I8-1,L)*PX(I2-1)+COEFF(I8,L)  *PX(I2))*ISYM
      RETURN
      END
