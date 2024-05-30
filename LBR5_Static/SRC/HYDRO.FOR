      SUBROUTINE HYDRO(ARG,M,XI,XF,PHIL,A,B,TETA,Q,IJK,MS,BKSE)

      IMPLICIT REAL *8(A-H,O-Z)
      DIMENSION ARG(8),A(720),B(1710),SI(4),CO(4),EX(4)	!!!aout04
      COMMON /PY/PI,SI,CO,EX
C***********************************************************************
C
C     SUBROUTINE HYDRO
C     *****************
C     SUBROUTINE D'INTEGRATION DE LA CHARGE EXTERIEURE,(CAS DE LA PRES-
C     SION HYDRAULIQUE),SUPPOSEE CONTINUE entre Y=0 et Y=Yo.
C     LES RESULTATS SONT PLACES DANS B (C.A.D DISH),
C     LES DONNEES SONT CONTENUES  DANS A (C.A.D. DISC).
C
C     Modif : 11-8-94                            Créer : Thèse Ph. Rigo
C             12-2-96   (introduction de BKSE)
C***********************************************************************
      PI1=PI/180
      PHIL1=PHIL*PI1
      TETA1=TETA*PI1

      JJ=1
      IF(PHIL.LE.1.E-05) JJ=0
      KK=1
      IF(DABS(TETA).NE.180.) GOTO 4
      COST=-1.
      SINT=0.
      COSTT=-DCOS(PHIL1)
      SINTT=-DSIN(PHIL1)
      GOTO 6
   4  IF(DABS(TETA).NE.90.) GOTO 5
      II=1
      IF(TETA.EQ.-90.) II=-1
      COST=0.
      SINT=II
      COSTT=-DSIN(PHIL1)*II
      SINTT=DCOS(PHIL1)*II
      IF(JJ.EQ.0) KK=0
      GOTO 6
    5 COST=DCOS(TETA1)
      SINT=DSIN(TETA1)
      COSTT=DCOS(TETA1+PHIL1)
      SINTT=DSIN(TETA1+PHIL1)
    6 CONTINUE

      DO 11 I=1,M
      AL=ARG(2*I-1)
      BE=ARG(2*I)
      U=AL*AL+BE*BE
      UP=U+1./(Q*Q)
      UM=U-1./(Q*Q)
      BP=BE+1./Q
      BM=BE-1./Q
      USP=UP-2.*BE*BE
      Z1=AL*AL+BP*BP
      Z2=AL*AL+BM*BM
      DPP=BP*Q/(2.*Z1)
      DMM=BM*Q/(2.*Z2)
      DP=AL*Q/(2.*Z1)
      DM=AL*Q/(2.*Z2)

      DO 3 L=1,38									!!!aout04
      IF((L.GT.(3+MS)).AND.(L.LE.23)) GOTO 3
      K=(L-1)*38
      JK=K+8*I-2
      J=(L-1)*16+4*I-3
      AA=A(J)
      BB=A(J+1)
      T1= AL*AA+BE*BB
      T2=-BE*AA+AL*BB
      SINT1=Q*SINT*T1/U
      SINT2=Q*SINT*T2/U

      GOTO(2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,1,2	!!!aout04
     *,2,2,1,1,1,2,2),L

c     Termes U,W, Xo,Zo,  My et Ny (c.….d. 2,3, 4 … 23, 24 et 25)
  1   B(K+1)=B(K+1)+2.*(XI-Q*SINT*KK)*(T1-EX(I)*(T1*CO(I)+T2*SI(I)))/U
      B(K+6)=B(K+6)+2.*Q*KK*(AL*UP*AA+BE*UM*BB-EX(I)*(AA*(-BE*UM*SI(I)+
     *      AL*UP*CO(I))+BB*(AL*UP*SI(I)+BE*UM*CO(I))))/(Z1*Z2)
      B(JK+1)=(AA*(-DMM+DPP)+BB*(DM-DP))*COST+
     *         ((-AA*(DM+DP)-BB*(DMM+DPP))*SINT+SINT1)*JJ
      B(JK+1)=B(JK+1)-XI*T1/U
      B(JK+2)=(AA*(-DM+DP)+BB*(DPP-DMM))*COST+
     *      ((AA*(DMM+DPP)-BB*(DP+DM))*SINT+SINT2)*JJ
      B(JK+2)=B(JK+2)-XI*T2/U
      B(JK+3)=-(AA*(-DMM+DPP)+BB*(DM-DP))*COSTT+
     *          ((-AA*(DM+DP)-BB*(DMM+DPP))*SINTT+SINT1)*KK
      B(JK+3)=B(JK+3)-XI*T1/U
      B(JK+4)=-(AA*(-DM+DP)+BB*(DPP-DMM))*COSTT+
     *       ((AA*(DMM+DPP)-BB*(DP+DM))*SINTT+SINT2)*KK
      B(JK+4)=B(JK+4)-XI*T2/U
      B(JK+5)=+(AA*(-DMM+DPP)+BB*(DM-DP))*COST+
     *         (-(-AA*(DM+DP)-BB*(DMM+DPP))*SINT-SINT1)*JJ
      B(JK+5)=B(JK+5)+XI*T1/U
      B(JK+6)=+(AA*(-DM+DP)+BB*(DPP-DMM))*COST+
     *      (-(AA*(DMM+DPP)-BB*(DP+DM))*SINT-SINT2)*JJ
      B(JK+6)=B(JK+6)+XI*T2/U
      B(JK+7)=-(AA*(-DMM+DPP)+BB*(DM-DP))*COSTT+
     *         (-(-AA*(DM+DP)-BB*(DMM+DPP))*SINTT-SINT1)*KK
      B(JK+7)=B(JK+7)+XI*T1/U
      B(JK+8)=-(AA*(-DM+DP)+BB*(DPP-DMM))*COSTT+
     *      (-(AA*(DMM+DPP)-BB*(DP+DM))*SINTT-SINT2)*KK
      B(JK+8)=B(JK+8)+XI*T2/U
      GOTO 3

c     Termes V,Ry,Nyx,Nxy,Wø  (c.….d. 1, 26, 27, 28 et 29)
  2   B(K+5)=B(K+5)-2.*(AA*USP+2.*AL*BE*BB-EX(I)*
     *     (AA*(USP*CO(I)-2.*AL*BE*SI(I))+BB*(2.*AL*BE*CO(I)+
     *     USP*SI(I))))/(Z1*Z2)
      B(JK+1)=(AA*(-DMM+DPP)+BB*(DM-DP))*COST+
     *         ((-AA*(DM+DP)-BB*(DMM+DPP))*SINT+SINT1)*JJ
      B(JK+1)=B(JK+1)-XI*T1/U
      B(JK+2)=(AA*(-DM+DP)+BB*(DPP-DMM))*COST+
     *      ((AA*(DMM+DPP)-BB*(DP+DM))*SINT+SINT2)*JJ
      B(JK+2)=B(JK+2)-XI*T2/U
      B(JK+3)=(AA*(-DMM+DPP)+BB*(DM-DP))*COSTT+
     *        (-(-AA*(DM+DP)-BB*(DMM+DPP))*SINTT-SINT1)*KK
      B(JK+3)=B(JK+3)+XI*T1/U
      B(JK+4)=(AA*(-DM+DP)+BB*(DPP-DMM))*COSTT+
     *     (-(AA*(DMM+DPP)-BB*(DP+DM))*SINTT-SINT2)*KK
      B(JK+4)=B(JK+4)+XI*T2/U
      B(JK+5)=-(AA*(-DMM+DPP)+BB*(DM-DP))*COST+
     *          ((-AA*(DM+DP)-BB*(DMM+DPP))*SINT+SINT1)*JJ
      B(JK+5)=B(JK+5)-XI*T1/U
      B(JK+6)=-(AA*(-DM+DP)+BB*(DPP-DMM))*COST+
     *       ((AA*(DMM+DPP)-BB*(DP+DM))*SINT+SINT2)*JJ
      B(JK+6)=B(JK+6)-XI*T2/U
      B(JK+7)=-(AA*(-DMM+DPP)+BB*(DM-DP))*COSTT+
     *         (-(-AA*(DM+DP)-BB*(DMM+DPP))*SINTT-SINT1)*KK
      B(JK+7)=B(JK+7)+XI*T1/U
      B(JK+8)=-(AA*(-DM+DP)+BB*(DPP-DMM))*COSTT+
     *      (-(AA*(DMM+DPP)-BB*(DP+DM))*SINTT-SINT2)*KK
      B(JK+8)=B(JK+8)+XI*T2/U
  3   CONTINUE
 11   CONTINUE

      E=BKSE*4.*0.981/(IJK*PI)
      DO 20 I=1,1710				!!!aout04
 20   B(I)=E*B(I)
      RETURN
      END
