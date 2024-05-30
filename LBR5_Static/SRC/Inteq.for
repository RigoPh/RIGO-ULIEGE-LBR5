      SUBROUTINE INTEQ(ARG,A,MT,M,LR,KC,B,AUX)

      IMPLICIT REAL *8(A-H,O-Z)
      DIMENSION ARG(8),A(360),B(MT),AUX(2*MT,2*MT)
      COMMON/PY/PI

C***********************************************************************
C     SUBROUTINE INTEQU
C     ------------------
C     SUBROUTINE DE REMPLISSAGE DES COEFFICIENTS DE LA MATRICE DES
C     INCONNUES HYPERSTATIQUES DES TRAVERSES, DANS LA MATRICE [ AUX ].
c
C     modifié: le 22-6-95 (Ph. Rigo)                    Créer : Thèse
C***********************************************************************
C       avec KC indice sur les sollicitations: KC = 1  DISB et KC = 2  D
C       avec LR indice sur les forces        : KC = 1  Xo   et KC = 2  Z

      PI1=PI/180.
      PI2=2.*PI

      KI=(KC-1)*MT+1
      KF=KC*MT
      LI=1+(LR-1)*MT
      LF=LR*MT
c      write(66,*) 'KI,KF,LI,LF=',KI,KF,LI,LF

      DO 15 J=KI,KF

      DO 15 I=LI,LF
      K1=J-KI+1
      L1=I-LI+1
      Z=DABS(B(L1)-B(K1))*PI1

      DO 16 K=1,M
      KH=4*K+LR*16+32*(I-(LR-1)*MT)-3
      ZS=ARG(2*K-1)*Z
      ZR=ARG(2*K)*  Z
      ZSS=ARG(2*K-1)*(PI2-Z)
      ZRR=ARG(2*K)*  (PI2-Z)

      AUX(I,J)=AUX(I,J)+( A(KH  )*DCOS(ZR) +A(KH+1)*DSIN(ZR) )*EXPO(ZS)
     *                 +( A(KH+2)*DCOS(ZRR)+A(KH+3)*DSIN(ZRR))*EXPO(ZSS)
 16   CONTINUE

      AUX(I,J)=-AUX(I,J)/10000.
      IF(I.EQ.J)AUX(I,J)=AUX(I,J)+1.

 15   CONTINUE

      RETURN
      END
