      SUBROUTINE ANGLE(NEL,PHIL,NVSMB,
     *                 ANGL,NOEUD,NNO,NSIGN,NETO)
C
C**********************************************************************
C**********************************************************************
C
C ANGLE
C
C LA SUBROUTINE "ANGLE"  CALCULE LES RELATIONS DE TRANFORMATIONS   
C POUR PERMETTRE L'ASSEMBLAGE AUTOMATIQUE DES PANNEAUX.
C
C     Modifié : 27-11-95 (MODULE OPTIMISATION)      Créer: Thèse de Ph. Rigo
C                                                                       
C***********************************************************************
C**********************************************************************

      IMPLICIT REAL*8 (A-H,O-Z)

	DIMENSION A(400),B(360)	!!!aout04
	DIMENSION ANGL(NETO),NOEUD(NETO,2),NNO(NETO+1,2),NSIGN(NETO)	!!!aout04

      COMMON A,B   
      COMMON/PY/PI
c      DIMENSION A(400),B(360) !!!aout04

c     COMMON/ANG/ANGL,NOH,NOEUD,MCOMP,NNO,NC,NSIGN
c	DIMENSION ANGL(NETO),NOEUD(NETO,2),NNO(NETO+1,2),NSIGN(NETO)	!!!aout04

      NS =NSIGN(NEL)
      AN =ANGL(NEL)*PI/180.
      AN2=AN-PHIL*PI/180.

      IF(DABS(ANGL(NEL)).NE.180.) GOTO 4
      C=-1.
      S=0.
      C2=-DCOS(-PHIL*PI/180.)
      S2=-DSIN(-PHIL*PI/180.)
      GOTO 6

   4  IF(DABS(ANGL(NEL)).NE.90.) GOTO 5
      II=1
      IF(ANGL(NEL).EQ.-90.) II=-1
      C=0.
      S=II
      C2=-DSIN(-PHIL*PI/180.)*II
      S2=+DCOS(-PHIL*PI/180.)*II
      GOTO 6

  5   C =DCOS(AN)
      S =DSIN(AN)
      C2=DCOS(AN2)
      S2=DSIN(AN2)

  6   KK=0
      ND=NOEUD(NEL,1)
      NA=NOEUD(NEL,2)
      III=0
      IF(NS.EQ.-1) III=1
      IF(NNO(ND,2).NE.0) KK=1+III
      IF(NNO(NA,2).NE.0) KK=2-III
      IF(KK.EQ.1) GOTO 30
C
C CHANGEMENT DE REPERE DES GRANDEURS RELATIVES AU DEPART DU PANNEAU
C
      DO 10 I=1,8
        LR=(I-1)*20
        A(LR+201)=A(LR+ 1)*S-A(LR+13)*C
        A(LR+207)=A(LR+ 7)*S-A(LR+11)*C
        A(LR+211)=A(LR+11)*S+A(LR+ 7)*C
        A(LR+213)=A(LR+13)*S+A(LR+ 1)*C
        A(LR+ 1)=A(LR+201)
        A(LR+ 7)=A(LR+207)*NS
        A(LR+11)=A(LR+211)*NS
        A(LR+13)=A(LR+213)
        A(LR+ 5)=A(LR+5)*NS
        A(LR+ 9)=A(LR+9)*NS
   10 CONTINUE
      L=0
      DO 101 I=1,NVSMB
        L=(I-1)*16
        B(201)=B( 1+L)*S-B(13+L)*C
        B(207)=B( 7+L)*S-B(11+L)*C
        B(211)=B(11+L)*S+B( 7+L)*C
        B(213)=B(13+L)*S+B( 1+L)*C
        B( 1+L)=B(201)
        B( 7+L)=B(207)*NS
        B(11+L)=B(211)*NS
        B(13+L)=B(213)
        B( 5+L)=B(5+L)*NS
        B( 9+L)=B(9+L)*NS
  101 CONTINUE
      IF(KK.EQ.2) RETURN
C
C CHANGEMENT DE REPERE DES GRANDEURS RELATIVES A L'ARRIVEE DU PANNEAU
C
   30 DO 25 I=1,8
        LR=(I-1)*20
        A(LR+202)=A(LR+ 2)*S2-A(LR+14)*C2
        A(LR+208)=A(LR+ 8)*S2-A(LR+12)*C2
        A(LR+212)=A(LR+12)*S2+A(LR+ 8)*C2
        A(LR+214)=A(LR+14)*S2+A(LR+ 2)*C2
        A(LR+ 2)=A(LR+202)
        A(LR+ 8)=A(LR+208)*NS
        A(LR+12)=A(LR+212)*NS
        A(LR+14)=A(LR+214)
        A(LR+ 6)=A(LR+ 6)*NS
        A(LR+10)=A(LR+10)*NS
   25 CONTINUE

      L=0
      DO 103 I=1,NVSMB
        L=(I-1)*16
        B(202)=B(2+L)*S2-B(14+L)*C2
        B(208)=B(8+L)*S2-B(12+L)*C2
        B(212)=B(12+L)*S2+B(8+L)*C2
        B(214)=B(14+L)*S2+B(2+L)*C2
        B( 2+L)=B(202)
        B( 8+L)=B(208)*NS
        B(12+L)=B(212)*NS
        B(14+L)=B(214)
        B( 6+L)=B( 6+L)
        B(10+L)=B(10+L)
 103  CONTINUE

  40  RETURN
      END
