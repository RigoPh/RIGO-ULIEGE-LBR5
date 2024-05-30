      SUBROUTINE  PRESS(ARG,M,XI,XF,PHIL,A,D,Q,IJK,MS,BKSE)

      IMPLICIT REAL *8(A-H,O-Z)
      DIMENSION ARG(8),A(720),D(1710),SI(4),CO(4),EX(4)		!!!aout04
      COMMON /PY/PI,SI,CO,EX
C***********************************************************************
C     SUBROUTINE PRESS
C     ++++++++++++++++
C     SUBROUTINE D'INTEGRATION DE LA CHARGE EXTERIEURE,(CAS D'UNE CHARGE
C     NORNALE AU PANNEAU VARIANT LINEAIREMENT LE LONG DE L'AXE Y DE
C     XI (Y=0) A XF (Y=PHIL).
C     LES RESULTATS SONT PLACES DANS D (C.A.D DISH),
C     LES DONNEES SONT CONTENUES  DANS A (C.A.D. DISC).
C
C     Modif : 12-8-94                            Créer : Thèse Ph. Rigo     
C             12-2-96   (introduction de BKSE)     
C***********************************************************************
      DO 11 I=1,M
      AL=ARG(2*I-1)
      BE=ARG(2*I)
      U1=AL*AL+BE*BE
      R1=-AL*AL+BE*BE
      S1=2.*AL*BE

      P1=XF-XI
      P2=P1/(Q*PHIL*PI/180.)
      P3=P2/U1
      P2=P2*Q

      Z1=1.-EX(I)*CO(I)
      Z2=EX(I)*SI(I)

      DO 3 L=1,38								!!!aout04
      IF((L.GT.(3+MS)).AND.(L.LE.23)) GOTO 3
      K=(L-1)*38
      JK=K+8*I-2
      J=(L-1)*16+4*I-3

      AA=A(J)
      BB=A(J+1)
      T1=( AL*AA+BE*BB)/U1
      T2=(-BE*AA+AL*BB)/U1
      T3=( R1*AA-S1*BB)*P3/U1
      T4=( S1*AA+R1*BB)*P3/U1

      GOTO(2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,1,2	!!!aout04
     *,2,2,1,1,1,2,2),L													!!!aout04


c     Termes U,W, Xo,Zo,  My et Ny (c.à.d. 2,3, 4à23, 24,25)
  1   TEMP=2.*(T1*Z1-T2*Z2)
      D(K+1)=D(K+1)+XI*TEMP
      D(K+2)=D(K+2)+TEMP*P2
      D(JK+1)=-XI*T1-T3
      D(JK+2)=-XI*T2-T4
      D(JK+3)=-XF*T1+T3
      D(JK+4)=-XF*T2+T4
      D(JK+5)= XI*T1-T3
      D(JK+6)= XI*T2-T4
      D(JK+7)= XF*T1+T3
      D(JK+8)= XF*T2+T4
      GOTO 3

c     Termes V,Ry,Nyx,Nxy,Wø  (c.à.d. 1, 26, 27, 28 et 29)
 2    D(K+1)=D(K+1)+2.*(T3*Z1-T4*Z2)
      D(JK+1)=-XI*T1-T3
      D(JK+2)=-XI*T2-T4
      D(JK+3)= XF*T1-T3
      D(JK+4)= XF*T2-T4
      D(JK+5)=-XI*T1+T3
      D(JK+6)=-XI*T2+T4
      D(JK+7)= XF*T1+T3
      D(JK+8)= XF*T2+T4

    3 CONTINUE
   11 CONTINUE

      E=BKSE*4.*0.981/(PI*IJK)
      DO 20 I=1,1710				!!!aout04
 20   D(I)=E*D(I)
      RETURN
      END
