      SUBROUTINE LOCZ(ARG,M,XI,PHIL,A,B,DISH,TETA,Q,IJK,LAMB,WIDTH,
     *                IMPR,MS,IS,CHA,NPTS,NSOL,BZ)

      IMPLICIT REAL *8(A-H,O-Z)
      REAL*8 LAMB
      DIMENSION ARG(8),A(720),B(1710),DISH(1710),SI(4),CO(4),		!!!aout04
     *          EX(4),Y(21),CHA(100,3,NSOL),NPTS(NSOL),BZ(1710)	!!!aout04

      COMMON /PY/PI,SI,CO,EX
C***********************************************************************
C     SUBROUTINE  LOCZ
C     ++++++++++++++++
C     SUBROUTINE D'INTEGRATION DE LA CHARGE EXTERIEURE VERTICAL DE TYPE GRAVITE
C     OU POIDS PROPRE (CAS DE LA COMPOSANTE Z NORMALE AU PANNEAU)          
C                                                                       
C     VOIR SUBR. LOCY POUR LA COMPOSANTE TANGANTIELLE                   
C                                                                       
C     LA CHARGE VARIE SELON OX, PAS PAR PAS MAIS EST CONSTANTE SELON OY 
C                                                                       
C     LES RESULTATS SONT PLACES dans B (puis additioner à DISH), 
C              (B = vecteur de travail)
C     LES DONNEES SONT CONTENUES DANS A (C.A.D. DISC, charge unitaire)
C     et dans CHA(I,1,IS) pour la sollicitation.                 
C                                                                       
C                                                                       
C     Modif: 12-2-96                               Création : Version Bateau 1990
C***********************************************************************
c      IF(IMPR.NE.0) WRITE(9,*) ' ENTREE DANS BLOCZ  '                                                                         '
      IF(DABS(TETA).NE.180.) GOTO 4
      COST=-1.
      SINT=0.
      COSTT=-DCOS(PHIL*PI/180.)
      SINTT=-DSIN(PHIL*PI/180.)
      GOTO 6
   4  IF(DABS(TETA).NE.90.) GOTO 5
      II=1
      IF(TETA.EQ.-90.) II=-1
      COST=0.
      SINT=II
      COSTT=-DSIN(PHIL*PI/180.)*II
      SINTT=DCOS(PHIL*PI/180.)*II
      GOTO 6
    5 COST=DCOS(TETA*PI/180.)
      SINT=DSIN(TETA*PI/180.)
      COSTT=DCOS((TETA+PHIL)*PI/180.)
      SINTT=DSIN((TETA+PHIL)*PI/180.)
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
      DPP=BP/(2.*Z1)
      DMM=BM/(2.*Z2)
      DP=AL/(2.*Z1)
      DM=AL/(2.*Z2)

      DO 3 L=1,38															!!!aout04
      IF((L.GT.(3+MS)).AND.(L.LE.23)) GOTO 3
      K=(L-1)*38+1
      JK=K+8*I-2
      J=(L-1)*16+4*I-3

      AA=A(J)
      BB=A(J+1)

      GOTO(2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,1,2	!!!aout04
     *,2,2,1,1,1,2,2),L													!!!aout04

c     Termes U,W, Xo,Zo,  My et Ny (c.à.d. 2,3, 4à23, 24,25)
   1  B(K+5)=B(K+5)+2.*(AL*UP*AA+BE*UM*BB-EX(I)*(AA*(-BE*UM*SI(I)+
     *      AL*UP*CO(I))+BB*(AL*UP*SI(I)+BE*UM*CO(I))))/(Z1*Z2)
      B(JK)=(AA*(-DMM+DPP)+BB*(DM-DP))*COST+
     *    (-AA*(DM+DP)-BB*(DMM+DPP))*SINT
      B(JK+1)=(AA*(-DM+DP)+BB*(DPP-DMM))*COST+
     *    (AA*(DMM+DPP)-BB*(DP+DM))*SINT
      B(JK+2)=-(AA*(-DMM+DPP)+BB*(DM-DP))*COSTT+
     *    (-AA*(DM+DP)-BB*(DMM+DPP))*SINTT
      B(JK+3)=-(AA*(-DM+DP)+BB*(DPP-DMM))*COSTT+
     *    (AA*(DMM+DPP)-BB*(DP+DM))*SINTT
      B(JK+4)=+(AA*(-DMM+DPP)+BB*(DM-DP))*COST-
     *    (-AA*(DM+DP)-BB*(DMM+DPP))*SINT
      B(JK+5)=+(AA*(-DM+DP)+BB*(DPP-DMM))*COST-
     *    (AA*(DMM+DPP)-BB*(DP+DM))*SINT
      B(JK+6)=-(AA*(-DMM+DPP)+BB*(DM-DP))*COSTT-
     *    (-AA*(DM+DP)-BB*(DMM+DPP))*SINTT
      B(JK+7)=-(AA*(-DM+DP)+BB*(DPP-DMM))*COSTT-
     *    (AA*(DMM+DPP)-BB*(DP+DM))*SINTT


	if (JK.eq.61) then
	continue
      endif
	 
      GOTO  3

c     Termes V,Ry,Nyx,Nxy,Wø  (c.à.d. 1, 26, 27, 28 et 29)
 2    B(K+4)=B(K+4)-2.*(AA*USP+2.*AL*BE*BB-EX(I)*
     *     (AA*(USP*CO(I)-2.*AL*BE*SI(I))+BB*(2.*AL*BE*CO(I)+
     *     USP*SI(I))))/(Z1*Z2*Q)
      B(JK)=(AA*(-DMM+DPP)+BB*(DM-DP))*COST+
     *    (-AA*(DM+DP)-BB*(DMM+DPP))*SINT
      B(JK+1)=(AA*(-DM+DP)+BB*(DPP-DMM))*COST+
     *    (AA*(DMM+DPP)-BB*(DP+DM))*SINT
      B(JK+2)=+(AA*(-DMM+DPP)+BB*(DM-DP))*COSTT-
     *    (-AA*(DM+DP)-BB*(DMM+DPP))*SINTT
      B(JK+3)=+(AA*(-DM+DP)+BB*(DPP-DMM))*COSTT-
     *    (AA*(DMM+DPP)-BB*(DP+DM))*SINTT
      B(JK+4)=-(AA*(-DMM+DPP)+BB*(DM-DP))*COST+
     *    (-AA*(DM+DP)-BB*(DMM+DPP))*SINT
      B(JK+5)=-(AA*(-DM+DP)+BB*(DPP-DMM))*COST+
     *    (AA*(DMM+DPP)-BB*(DP+DM))*SINT
      B(JK+6)=-(AA*(-DMM+DPP)+BB*(DM-DP))*COSTT-
     *    (-AA*(DM+DP)-BB*(DMM+DPP))*SINTT
      B(JK+7)=-(AA*(-DM+DP)+BB*(DPP-DMM))*COSTT-
     *    (AA*(DMM+DPP)-BB*(DP+DM))*SINTT
  3   CONTINUE
 11   CONTINUE

      C=-2./(PI*IJK*10000.)
      E=0.
      Y(1)=0.
      IPT=NPTS(IS)
      DW=WIDTH/FLOAT(IPT)
      DO 155 I=1,IPT
 155  Y(I+1)=Y(I)+DW
      DO 156 I=1,IPT
 156  E=E+CHA(I,1,IS)*(DCOS(LAMB*Y(I+1))-DCOS(LAMB*Y(I)))
      E=E*C*XI
      DO 20 I=1,1710				!!!aout04
	if (I.EQ.61) then
	continue
	endif
      DISH(I)=DISH(I)+E*B(I)
	BZ(I)=B(I)  !!!aout04
 20   B(I)=0.
      RETURN
      END
