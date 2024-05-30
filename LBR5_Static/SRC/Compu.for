      SUBROUTINE COMPU(PHIL,FP,A,C,B,X,M,ARGQ,MT,U,TETA,Z,L,SY,ICAS)
C     =================
      IMPLICIT REAL *8(A-H,O-Z)
      COMMON/PY/PI
      DIMENSION A(1710),B(720),C(720),X(10),ARGQ(8),FP(20),U(51),Z(2295)
C***********************************************************************
C
C     SUBROUTINE COMPU
C     LA SUBROUTINE CALCULE LES VALEURS AUX 31+20 POINTS DE L'AXE OX,
C     DES RESULTATS POUR UN EFFET CONSIDERE.
C
C  Données : A  = DISH,  B=DISB et C=DISC
C            X  = position des 10 traverses (ABTR)
C            FP = valeurs des forces Xo et Zo à appliquer au droit des
C                  traverses
C            U  = coordonnées des 51 pts de calcul selon OY
C	       L  = nø de la fonction calculée (u,v,w, ...)

C  Output :  Z  = vecteur contenu les valeurs de Z2 (par blocs de 51)
C
C     BOUCLE 10:EFFET GENERAL SUR LA PLAQUE ORTHOTROPE.
C     BOUCLE 600:EFFET DES TRAVERSES SUR LA PLAQUE ORTHOTROPE.
C     BOUCLE 60:CALCUL AU DESSUS ET EN DESSOUS DE L'AXE DE CHAQUE
C     TRAVERSES (20 POINTS DE CALCUL AU MAXIMUM).
C
C    Modif : 16-6-95                        Création : Thèse Ph. Rigo
C***********************************************************************
      PI1=PI/180.
C      ITEST1=1

      DO 10 I=1,ICAS
        Y=U(I)
        Y1=Y*PI1
        IL=(L-1)*51+I
        IJ=38*(L-1)+1
            IF(DABS(TETA).NE.180.) GOTO 404
            CC=-DCOS(Y1)
            SS=-DSIN(Y1)
            GOTO 406
 404        IF(DABS(TETA).NE.90.) GOTO 405
            II=1.
            IF(TETA.EQ.-90.) II=-1
            CC=-II*DSIN(Y1)
            SS=II*DCOS(Y1)
            GOTO 406
  405       CC=DCOS((TETA+Y)*PI1)
            SS=DSIN((TETA+Y)*PI1)
  406   Z(IL)=A(IJ)+A(IJ+1)*Y1+A(IJ+2)*(Y1*Y1)
     *             +A(IJ+3)*(Y1**3)+A(IJ+4)*CC+A(IJ+5)*SS
        DO 5 K=1,M
           IJ=38*(L-1)+8*K-1
C
C          CALCUL DE Z(IL)
C          GO TO 200
           AL1=ARGQ(2*K-1)*Y1
           BE1=ARGQ(2*K)*Y1
           AL2=ARGQ(2*K-1)*(PHIL-Y)*PI1
           BE2=ARGQ(2*K)  *(PHIL-Y)*PI1
           AL3=ARGQ(2*K-1)*(360.-Y)*PI1
           BE3=ARGQ(2*K)  *(360.-Y)*PI1
           AL4=ARGQ(2*K-1)*(360.-(PHIL-Y))*PI1
           BE4=ARGQ(2*K)  *(360.-(PHIL-Y))*PI1
           AL1=EXPO(AL1)
           AL2=EXPO(AL2)
           AL3=EXPO(AL3)
           AL4=EXPO(AL4)
           R=AL1*(A(IJ  )*DCOS(BE1)+A(IJ+1)*DSIN(BE1))
     *      +AL2*(A(IJ+2)*DCOS(BE2)+A(IJ+3)*DSIN(BE2))
     *      +AL3*(A(IJ+4)*DCOS(BE3)+A(IJ+5)*DSIN(BE3))
     *      +AL4*(A(IJ+6)*DCOS(BE4)+A(IJ+7)*DSIN(BE4))
C
   12      Z(IL)=Z(IL)+R
    5   CONTINUE
   10 CONTINUE
C
C
      IF(MT.EQ.0)GOTO 19
C      ITEST2=1
      DO 600 I=1,ICAS
      Y=U(I)
      IL=(L-1)*51+I
      DO 3 J=1,MT
      KH=J+MT
      DO 16 K=1,M
      SIGN=1.
      JM=4*K+(L-1)*16-3
C
C   CALCUL DE P,Q,PP,QQ
C      GOTO 201
      DELY=DABS(X(J)-Y)
      DELY1=DELY*PI1
      AL1=ARGQ(2*K-1)*DELY1
      BE1=ARGQ(2*K)*DELY1
      AL2=ARGQ(2*K-1)*(360.-DELY)*PI1
      BE2=ARGQ(2*K)*(360.-DELY)*PI1
      AL1=EXPO(AL1)
      AL2=EXPO(AL2)
      P=AL1*DCOS(BE1)
      Q=AL1*DSIN(BE1)
      PP=AL2*DCOS(BE2)
      QQ=AL2*DSIN(BE2)

  301 IF((DABS((X(J)-Y)/PHIL).LT.(0.00001)).OR.(X(J).LT.Y))GOTO 16
      IF(SY)18,16,16
   18 SIGN=-1.
   16 Z(IL)=Z(IL)+(FP(J)*(B(JM)*P+B(JM+1)*Q+B(JM+2)*PP+B(JM+3)*QQ)
     *      +FP(KH)*(C(JM)*P+C(JM+1)*Q+C(JM+2)*PP+C(JM+3)*QQ))*SIGN
    3 CONTINUE
  600 CONTINUE
C
C
      IF(ICAS.LE.7) GOTO 19
C      ITEST1=2
C      ITEST2=2
      DO 60 I=1,MT
      Y=X(I)
      Y1=Y*PI1
      IL=(L-1)*51+2*I+30
      IJ=38*(L-1)+1
            IF(DABS(TETA).NE.180.) GOTO 504
            CC=-DCOS(Y1)
            SS=-DSIN(Y1)
            GOTO 506
  504       IF(DABS(TETA).NE.90.) GOTO 505
            II=1.
            IF(TETA.EQ.-90.) II=-1
            CC=-II*DSIN(Y1)
            SS= II*DCOS(Y1)
            GOTO 506
  505       CC=DCOS((TETA+Y)*PI1)
            SS=DSIN((TETA+Y)*PI1)
  506 R=A(IJ)+A(IJ+1)*Y1+A(IJ+2)*(Y1*Y1)
     *       +A(IJ+3)*(Y1**3)+A(IJ+4)*CC+A(IJ+5)*SS
      Z(IL)=R
      Z(IL+1)=R
      DO 61 K=1,M
      IJ=38*(L-1)+8*K-1
C
C   CALCUL DE Z(IL)
C      GO TO 200
	AL1=ARGQ(2*K-1)*Y1
      BE1=ARGQ(2*K)*Y1
      AL2=ARGQ(2*K-1)*(PHIL-Y)*PI1
      BE2=ARGQ(2*K)  *(PHIL-Y)*PI1
      AL3=ARGQ(2*K-1)*(360.-Y)*PI1
      BE3=ARGQ(2*K)  *(360.-Y)*PI1
      AL4=ARGQ(2*K-1)*(360.-(PHIL-Y))*PI1
      BE4=ARGQ(2*K)  *(360.-(PHIL-Y))*PI1
      AL1=EXPO(AL1)
      AL2=EXPO(AL2)
      AL3=EXPO(AL3)
      AL4=EXPO(AL4)
      R=AL1*(A(IJ  )*DCOS(BE1)+A(IJ+1)*DSIN(BE1))
     *  +AL2*(A(IJ+2)*DCOS(BE2)+A(IJ+3)*DSIN(BE2))
     *  +AL3*(A(IJ+4)*DCOS(BE3)+A(IJ+5)*DSIN(BE3))
     *  +AL4*(A(IJ+6)*DCOS(BE4)+A(IJ+7)*DSIN(BE4))

   14 Z(IL)=Z(IL)+R
   61 Z(IL+1)=Z(IL+1)+R


      DO 21 J=1,MT
      KH=J+MT
      DO 24 K=1,M
      SIGN=1.
      JM=4*K+(L-1)*16-3
C
C     CALCUL DE P,Q,PP,QQ
C      GOTO 201
	DELY=DABS(X(J)-Y)
      DELY1=DELY*PI1
      AL1=ARGQ(2*K-1)*DELY1
      BE1=ARGQ(2*K)*DELY1
      AL2=ARGQ(2*K-1)*(360.-DELY)*PI1
      BE2=ARGQ(2*K)*(360.-DELY)*PI1
      AL1=EXPO(AL1)
      AL2=EXPO(AL2)
      P=AL1*DCOS(BE1)
      Q=AL1*DSIN(BE1)
      PP=AL2*DCOS(BE2)
      QQ=AL2*DSIN(BE2)
C
  302 R=FP(J)*(B(JM)*P+B(JM+1)*Q+B(JM+2)*PP+B(JM+3)*QQ)
     *      +FP(KH)*(C(JM)*P+C(JM+1)*Q+C(JM+2)*PP+C(JM+3)*QQ)
      IF(SY)53,52,52
   53 IF(DABS((X(J)-Y)/PHIL).LE.(0.01))GOTO 54
      IF(X(J).LT.Y)GOTO 52
      SIGN=-1.
   52 R=R*SIGN
      R1=R
      GOTO 58
   54 SIGN=-1
      R=R*SIGN
      R1=-R
   58 Z(IL)=Z(IL)+R
      Z(IL+1)=Z(IL+1)+R1
   24 CONTINUE
   21 CONTINUE
   60 CONTINUE
   19 CONTINUE
      RETURN
	END

C  ******************************************************************
C     BOUCLE 200 : CALCUL DES EXPONENTIELLES
C  ******************************************************************
C  200 AL1=ARGQ(2*K-1)*Y1
C      BE1=ARGQ(2*K)*Y1
C      AL2=ARGQ(2*K-1)*(PHIL-Y)*PI1
C      BE2=ARGQ(2*K)  *(PHIL-Y)*PI1
C      AL3=ARGQ(2*K-1)*(360.-Y)*PI1
C      BE3=ARGQ(2*K)  *(360.-Y)*PI1
C      AL4=ARGQ(2*K-1)*(360.-(PHIL-Y))*PI1
C      BE4=ARGQ(2*K)  *(360.-(PHIL-Y))*PI1
C      IF((L.NE.30).AND.(L.NE.25)) GOTO 6745
C      IF(I.GE.10) GOTO 6745
C     WRITE(6,*)PHIL,Y,L
C     WRITE(6,*)AL1,AL2,AL3,AL4
C     WRITE(6,*)BE1,BE2,BE3,BE4
C 6745 AL1=EXPO(AL1)
C      AL2=EXPO(AL2)
C      AL3=EXPO(AL3)
C      AL4=EXPO(AL4)
C      R=AL1*(A(IJ  )*DCOS(BE1)+A(IJ+1)*DSIN(BE1))
C     * +AL2*(A(IJ+2)*DCOS(BE2)+A(IJ+3)*DSIN(BE2))
C     * +AL3*(A(IJ+4)*DCOS(BE3)+A(IJ+5)*DSIN(BE3))
C     * +AL4*(A(IJ+6)*DCOS(BE4)+A(IJ+7)*DSIN(BE4))
C      IF((L.NE.30).AND.(L.NE.25)) GOTO 6746
C      IF(I.GE.10) GOTO 6746
C     WRITE(6,*)AL1,AL2,AL3,AL4
C     WRITE(6,*)K,R
C6746  GOTO(12,14),ITEST1

C  ******************************************************************
C     BOUCLE 201 : CALCUL DES EXPONENTIELLES
C  ******************************************************************
C 201  DELY=DABS(X(J)-Y)
C      DELY1=DELY*PI1
C      AL1=ARGQ(2*K-1)*DELY1
C      BE1=ARGQ(2*K)*DELY1
C      AL2=ARGQ(2*K-1)*(360.-DELY)*PI1
C      BE2=ARGQ(2*K)*(360.-DELY)*PI1
C      AL1=EXPO(AL1)
C      AL2=EXPO(AL2)
C      P=AL1*DCOS(BE1)
C      Q=AL1*DSIN(BE1)
C      PP=AL2*DCOS(BE2)
C      QQ=AL2*DSIN(BE2)
C      GOTO(301,302),ITEST2
C      RETURN
C      END
