      SUBROUTINE HYP1(D,MT,ABTR,TETA,ZB,K)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION D(1710),ABTR(MT),ZB(2*MT,17)
      COMMON/PY/PI
C***********************************************************************
C
C     CALCULE LES TERMES INDEPENDANTS DU SYSTEME EN VUE DE DETERMINER
C     L'EFFET DES TRAVERSES SUR LA COQUE.
C     ET CELA POUR LA CHARGE EXTERIEURE (DISH=D)
C
C     modifié: le 21-6-95 (Ph. Rigo)                   Créer : Thèse
C***********************************************************************
      PI1=PI/180.
      DO  3 I=1,MT
      ABTRR=ABTR(I)*PI1
      IF(DABS(TETA).NE.180.) GOTO 1
      SIN=-DSIN(ABTRR)
      GOTO 2
  1   SIN=DSIN((TETA+ABTR(I))*PI1)
  2   ZB(I,K)=ZB(I,K)+D(39+76*I)+D(40+76*I)*ABTRR
     *                          +D(41+76*I)*(ABTRR*ABTRR)
     *                          +D(42+76*I)*(ABTRR**3)
     *                          +D(44+76*I)*SIN
      IJ=I+MT
  3   ZB(IJ,K)=ZB(IJ,K)+D(77+76*I)+D(78+76*I)*ABTRR
     *                            +D(79+76*I)*(ABTRR*ABTRR)
     *                            +D(80+76*I)*(ABTRR**3)
     *                            +D(82+76*I)*SIN
      RETURN
      END
