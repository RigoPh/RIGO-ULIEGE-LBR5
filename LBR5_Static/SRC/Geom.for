      SUBROUTINE GEOM(EPSA,EPSR,DELTA,HYA,DYA,WYA,TYA,HXR,DXR,WXR,TXR,
     *                II,IT,NXI,NBRXI,ITERA,IPRINT,LM2,M2max,IMPR)		!15.10.05

      IMPLICIT REAL *8 (A-H,O-Z)
      CHARACTER*12 VNOM

      DIMENSION NXI(9),DCJ(9),LM2(M2max)

c     COMMON /ALLONG/BID(300),IBID(5),DCJ(9),CJM,CJ,LM2(20),VNOM

      COMMON /OPTI/  IOPTI,NTOT,M1TOT,M2TOT

C***********************************************************************
C     SUBROUTINE GEOM
C     ****************
C     Cette subroutine calcule les restrictions géométriques
C         CJ(xi) < CJMax  et les sensibilités = DCJ(N) = d(CJ)/dXI.
C
C     Il y a aussi un TEST pour vérifier si les variables intervenant da
C     les restrictions choisies sont bien des variables de conception (S
C
C     Création     : 4-6-96 (Ph. Rigo pour L.B.R.-5)
C     Modifications:
C            1-7-96 RESTRICTIONS 104,105, 204 et 205
C           13-3-98 Restrictions géométriques de Hughes
C		  27-7-05 Nouvelles Restrictions :
C                       cadre      : 120 -> 126
C                       raidisseur : 220 -> 223
C                                    230 -> 240 (pseudo girder)
C                       croisée    : 320
C

C***********************************************************************
C   Les variables de conception ; NBRXI = NVAR(NEL)
C   -------------------------------------------------
C       1       DELTA =  épaisseur du bordage (d=delta)
C       2       HYA   =  hauteur de l'âme des cadres (Dw)
C       3       DYA   =  épaisseur de l'âme des cadres (Tw)
C       4       WYA   =  largeur des semelles des cadres (Df)
C       5       EPSA  =  entredistance entre cadres
C       6       HXR   =  hauteur de l'âme des raidisseurs  (Dw)
C       7       DXR   =  épaisseur de l'âme des raidisseurs (Tw)
C       8       WXR   =  largeur des semelles des raidisseurs (Df)
C       9       EPSR  =  entredistance entre raidisseurs


C ****************************************************************
c                RESTRICTIONS GEOMETRIQUES DES TRANSVERSAUX (cadres)
c                -------------------------------------------
C                SET de RAHMAN-Caldwell
c                nø 101 :  Df-Dw     <0         (Df = haut. ame)
c                nø 102 :  Dw-2*Df   <0         (Dw = larg. semelle)
c                nø 103 :  0.0065+Dw/170-Tw<0   (Tw = épaisseur ame)
C                SET A:
c                nø 104 :  Delta-2*Tw<0         (Delta = épaiss. bordé)
c                nø 105 :  3*Delta-Dw<0
C                SET de Hughes
c                        nø 109 :  0.625Df-Dw<0
c                        nø 110 :  Dw-2.5*Df <0
c                nø 111 :  Dw-120*Tw <0
c                nø 112 :  Tw-2*Delta<0  (complétaire à C104)
C                Pour Situation Inverse (si Raid/Girder >> Transv. frame
c                nø 113 :  Dw-36*Tw  <0  (complément set Hughes = C211
c                nø 114 :  Dw-40*Tw  <0  (complément set Rahman = C203
c                Si profil en cornières (à la place de C 109 et C110)
c                        nø 115 :  1.25*Df-Dw<0
c                        nø 116 :  Dw-5*Df   <0
c
c            RESTRICTIONS GEOMETRIQUES DES RAIDISSEURS  (longitudinaux)
c            ----------------------------------------------------------
C                SET de RAHMAN-Caldwell
c                nø 201 :  Df-Dw     <0
c                nø 202 :  Dw-2*Df   <0
c                nø 203 :  Dw-40*Tw  <0
C                SET A:
c                nø 204 :  Delta-2*Tw<0
c                nø 205 :  3*Delta-Dw<0

c                SET de Hughes
c                        nø 209 :  0.625Df-Dw<0
c                        nø 210 :  Dw-2.5*Df <0
c                        nø 211 :  Dw-36*Tw  <0
c                        nø 212 :  Tw-2*Delta<0  (complétaire à C204)
c                Pour Situation Inverse (si Raid/Girder >> Transv. frame
c                        nø 213 :  Dw-120*Tw <0         (complément set
c                        nø 214 :  0.0065+Dw/170-Tw<0   (complément set
c                Si profil en cornières (à la place de C 209 et C210)
c                        nø 215 :  1.25*Df-Dw<0
c                        nø 216 :  Dw-5*Df   <0
c
c            RESTRICTIONS CROISEES (Transversaux - raidisseurs)
c            --------------------------------------------------
C                SET de Hughes
c                nø 301 :  Dw(raid)-Dw(aig)   <0
c                nø 302 :  Tw(raid)-4*Tw(aig) <0
c                nø 303 :  Tw(aig) -4*Tw(raid)<0
C                Pour Situation Inverse (si Raid/Girder >> Transv. frame
c                nø 304 :  Dw(aig)-Dw(raid)   <0

C ****************************************************************
C 1.    RESTRICTIONS GEOMETRIQUES
C ****************************************************************
c     II = nø local  de la restriction géométrique dans le panneau (I=1,
c     IT = nø global de la restriction géométrique (pour la structure)
c     IC = Référence de la restriction géométrique (cfr LM2)

      IC=LM2(II)
      CALL ANNULD(DCJ,9)

C ****************************************************************
C 2.    RESTRICTIONS GEOMETRIQUES DES AIGUILLES - CADRES
C ****************************************************************
C     RESTRICTIONS nø 101 :  Df-Dw<0
C     -----------------------------------------------------------
      IF(IC.EQ.101) THEN
        VNOM='Df-Dw<0    A'
        CJ=WYA-HYA
        CJM=0.

c     CALCUL de DCJ = dérivées = sensibilités
        L=0
        DO 101 IK=1,NBRXI
        IF (NXI(IK).EQ.2)  THEN
          DCJ(IK)=-1.
          L=L+1
        ELSE IF(NXI(IK).EQ.4)  THEN
          DCJ(IK)=+1.
          L=L+1
        ENDIF
  101   CONTINUE

        CALL TEST(L)


C     RESTRICTIONS nø 102 :  Dw-2*Df<0
C     -----------------------------------------------------------
      ELSE IF(IC.EQ.102) THEN
        VNOM='Dw-2*Df<0  A'
        CJ=HYA-2.*WYA
        CJM=0.

c      CALCUL de DCJ = dérivées = sensibilités
        L=0
        DO 102 IK=1,NBRXI
        IF     (NXI(IK).EQ.2)  THEN
          DCJ(IK)=1.
          L=L+1
        ELSE IF(NXI(IK).EQ.4)  THEN
          DCJ(IK)=-2.
          L=L+1
        ENDIF
  102   CONTINUE

        CALL TEST(L)

C     RESTRICTIONS nø 103 :  0.0065+Dw/170-Tw<0
C     -----------------------------------------------------------
      ELSE IF(IC.EQ.103) THEN
        VNOM='Dw/a-Tw<b  A'
        CJ=HYA/170-DYA
        CJM=-0.0065

c      CALCUL de DCJ = dérivées = sensibilités
        L=0
        DO 103 IK=1,NBRXI
        IF     (NXI(IK).EQ.2)  THEN
          DCJ(IK)=1./170.
          L=L+1
        ELSE IF(NXI(IK).EQ.3)  THEN
          DCJ(IK)=-1.
          L=L+1
        ENDIF
  103   CONTINUE

        CALL TEST(L)

C     RESTRICTIONS nø 104 :  d-2Tw<0
C     -----------------------------------------------------------
      ELSE IF(IC.EQ.104) THEN
        VNOM='d-2Tw<0    A'
        CJ=DELTA-2.*DYA
        CJM=0.

c      CALCUL de DCJ = dérivées = sensibilités
        L=0
        DO 104 IK=1,NBRXI
        IF     (NXI(IK).EQ.1)  THEN
          DCJ(IK)=1.
          L=L+1
        ELSE IF(NXI(IK).EQ.3)  THEN
          DCJ(IK)=-2.
          L=L+1
        ENDIF
  104 CONTINUE

      CALL TEST(L)

C     RESTRICTIONS nø 105 : 3d-Dw<0
C     -----------------------------------------------------------
      ELSE IF(IC.EQ.105) THEN
          VNOM='3d-Dw<0    A'
        CJ=3.*DELTA-HYA
        CJM=0.

c      CALCUL de DCJ = dérivées = sensibilités
        L=0
        DO 105 IK=1,NBRXI
        IF     (NXI(IK).EQ.1)  THEN
          DCJ(IK)=3.
          L=L+1
        ELSE IF(NXI(IK).EQ.2)  THEN
          DCJ(IK)=-1.
          L=L+1
        ENDIF
  105 CONTINUE

      CALL TEST(L)

C     RESTRICTIONS nø 109 : 0.625*Df-Dw<0
C     -----------------------------------------------------------
      ELSE IF(IC.EQ.109) THEN
          VNOM='0.6Df-Dw<0 A'
        CJ=0.625*WYA-HYA
        CJM=0.

c      CALCUL de DCJ = dérivées = sensibilités
        L=0
        DO 109 IK=1,NBRXI
        IF     (NXI(IK).EQ.2)  THEN
          DCJ(IK)=-1.
          L=L+1
        ELSE IF(NXI(IK).EQ.4)  THEN
          DCJ(IK)=0.625
          L=L+1
        ENDIF
  109 CONTINUE

      CALL TEST(L)


C     RESTRICTIONS nø 110 : Dw-2.5*Df   <0
C     -----------------------------------------------------------
      ELSE IF(IC.EQ.110) THEN
          VNOM='Dw-2.5Df<0 A'
        CJ=HYA-2.5*WYA
        CJM=0.

c      CALCUL de DCJ = dérivées = sensibilités
        L=0
        DO 110 IK=1,NBRXI
        IF     (NXI(IK).EQ.2)  THEN
          DCJ(IK)=1.
          L=L+1
        ELSE IF(NXI(IK).EQ.4)  THEN
          DCJ(IK)=-2.5
          L=L+1
        ENDIF
  110 CONTINUE

      CALL TEST(L)


C     RESTRICTIONS nø 111 : Dw-120*Tw <0
C     -----------------------------------------------------------
      ELSE IF(IC.EQ.111) THEN
          VNOM='Dw-120Tw<0 A'
        CJ=HYA-120.*DYA
        CJM=0.

c      CALCUL de DCJ = dérivées = sensibilités
        L=0
        DO 111 IK=1,NBRXI
        IF     (NXI(IK).EQ.2)  THEN
          DCJ(IK)=1.
          L=L+1
        ELSE IF(NXI(IK).EQ.3)  THEN
          DCJ(IK)=-120.
          L=L+1
        ENDIF
  111 CONTINUE

      CALL TEST(L)


C     RESTRICTIONS nø 112 : Tw-2*Delta<0
C     -----------------------------------------------------------
      ELSE IF(IC.EQ.112) THEN
          VNOM='Tw-2Delt<0 A'
        CJ=DYA-2.*DELTA
        CJM=0.

c      CALCUL de DCJ = dérivées = sensibilités
        L=0
        DO 112 IK=1,NBRXI
        IF     (NXI(IK).EQ.1)  THEN
          DCJ(IK)=-2.
          L=L+1
        ELSE IF(NXI(IK).EQ.3)  THEN
          DCJ(IK)=1.
          L=L+1
        ENDIF
  112 CONTINUE

      CALL TEST(L)


C     RESTRICTIONS nø 113 : Dw-36*Tw  <0
C     -----------------------------------------------------------
      ELSE IF(IC.EQ.113) THEN
          VNOM='Dw-36*Tw<0 A'
        CJ=HYA-36.*DYA
        CJM=0.

c      CALCUL de DCJ = dérivées = sensibilités
        L=0
        DO 113 IK=1,NBRXI
        IF     (NXI(IK).EQ.2)  THEN
          DCJ(IK)=1.
          L=L+1
        ELSE IF(NXI(IK).EQ.3)  THEN
          DCJ(IK)=-36.
          L=L+1
        ENDIF
  113 CONTINUE

      CALL TEST(L)


C     RESTRICTIONS nø 114 : Dw-40*Tw  <0
C     -----------------------------------------------------------
      ELSE IF(IC.EQ.114) THEN
          VNOM='Dw-40*Tw<0 A'
        CJ=HYA-40.*DYA
        CJM=0.

c      CALCUL de DCJ = dérivées = sensibilités
        L=0
        DO 114 IK=1,NBRXI
        IF     (NXI(IK).EQ.2)  THEN
          DCJ(IK)=1.
          L=L+1
        ELSE IF(NXI(IK).EQ.3)  THEN
          DCJ(IK)=-40.
          L=L+1
        ENDIF
  114 CONTINUE

      CALL TEST(L)


C     RESTRICTIONS nø 115 : 1.25*Df-Dw<0
C     -----------------------------------------------------------
      ELSE IF(IC.EQ.115) THEN
          VNOM='1.2Df-Dw<0 A'
        CJ=1.25*WYA-HYA
        CJM=0.

c      CALCUL de DCJ = dérivées = sensibilités
        L=0
        DO 115 IK=1,NBRXI
        IF     (NXI(IK).EQ.2)  THEN
          DCJ(IK)=-1.
          L=L+1
        ELSE IF(NXI(IK).EQ.4)  THEN
          DCJ(IK)=1.25
          L=L+1
        ENDIF
  115 CONTINUE

      CALL TEST(L)


C     RESTRICTIONS nø 116 : Dw-5*Df   <0
C     -----------------------------------------------------------
      ELSE IF(IC.EQ.116) THEN
          VNOM='Dw-5*Df <0 A'
        CJ=HYA-5.*WYA
        CJM=0.

c      CALCUL de DCJ = dérivées = sensibilités
        L=0
        DO 116 IK=1,NBRXI
        IF     (NXI(IK).EQ.2)  THEN
          DCJ(IK)=1.
          L=L+1
        ELSE IF(NXI(IK).EQ.4)  THEN
          DCJ(IK)=-5.
          L=L+1
        ENDIF
  116 CONTINUE

      CALL TEST(L)

C     RESTRICTIONS nø 120 : -Df/Tf   <-12
C     -----------------------------------------------------------
c      ELSE IF(IC.EQ.120) THEN
c          VNOM='Df/Tf>12   A'
c        CJ=-WYA + 12.*TYA
c        CJM=0.

c      CALCUL de DCJ = dérivées = sensibilités
c        L=0
c        DO 120 IK=1,NBRXI
c        IF     (NXI(IK).EQ.4)  THEN
c          DCJ(IK)=-1.
c          L=L+1
c        ENDIF
c  120 CONTINUE

c      CALL TEST(L)

C     RESTRICTIONS nø 121 : Df/Tf   <30
C     -----------------------------------------------------------
c      ELSE IF(IC.EQ.121) THEN
c          VNOM='Df/Tf<30   A'
c        CJ=WYA - 30.*TYA
c        CJM=0.

c      CALCUL de DCJ = dérivées = sensibilités
c        L=0
c        DO 121 IK=1,NBRXI
c        IF     (NXI(IK).EQ.4)  THEN
c          DCJ(IK)=1.
c          L=L+1
c        ENDIF
c  121 CONTINUE

c      CALL TEST(L)

C     RESTRICTIONS nø 120 : -Df + 0,2.Dw   < 0
C     -----------------------------------------------------------
      ELSE IF(IC.EQ.120) THEN
          VNOM='Df/Dw>0.2  A'
        CJ=-WYA + 0.2*HYA
        CJM=0.

c      CALCUL de DCJ = dérivées = sensibilités
        L=0
	  DO 120 IK=1,NBRXI
        IF     (NXI(IK).EQ.2)  THEN
          !DCJ(IK)=WYA/(HYA**2)
		DCJ(IK)=0.2
          L=L+1
        ELSE IF(NXI(IK).EQ.4)  THEN
          DCJ(IK)=-1.
          L=L+1
        ENDIF
  120 CONTINUE

      CALL TEST(L)

C     RESTRICTIONS nø 121 : Df/Dw   <0,5
C     -----------------------------------------------------------
      ELSE IF(IC.EQ.121) THEN
          VNOM='Df/Dw<0.5  A'
        CJ=WYA - 0.5*HYA
        CJM=0.

c      CALCUL de DCJ = dérivées = sensibilités
        L=0
	  DO 121 IK=1,NBRXI
        IF     (NXI(IK).EQ.2)  THEN
          DCJ(IK)=-0.5
          L=L+1
        ELSE IF(NXI(IK).EQ.4)  THEN
          DCJ(IK)=1.
          L=L+1
        ENDIF
  121 CONTINUE

      CALL TEST(L)

C     RESTRICTIONS nø 122 : -Dw/Tw   <-28
C     -----------------------------------------------------------
      ELSE IF(IC.EQ.122) THEN
          VNOM='Dw/Tw>28   A'
        CJ=-HYA + 28.*DYA
        CJM=0.

c      CALCUL de DCJ = dérivées = sensibilités
        L=0
	  DO 122 IK=1,NBRXI
        IF     (NXI(IK).EQ.2)  THEN
          DCJ(IK)=-1.
          L=L+1
        ELSE IF(NXI(IK).EQ.3)  THEN
          DCJ(IK)=28.
          L=L+1
        ENDIF
  122 CONTINUE

      CALL TEST(L)

C     RESTRICTIONS nø 123 : Dw/Tw   <90
C     -----------------------------------------------------------
      ELSE IF(IC.EQ.123) THEN
          VNOM='Dw/Tw<90   A'
        CJ=HYA - 90.*DYA
        CJM=0.

c      CALCUL de DCJ = dérivées = sensibilités
        L=0
	  DO 123 IK=1,NBRXI
        IF     (NXI(IK).EQ.2)  THEN
          DCJ(IK)=1.
          L=L+1
        ELSE IF(NXI(IK).EQ.3)  THEN
          DCJ(IK)=-90.
          L=L+1
        ENDIF
  123 CONTINUE

      CALL TEST(L)

C     RESTRICTIONS nø 126 : -Tf/Tw   <-1
C     -----------------------------------------------------------
c      ELSE IF(IC.EQ.126) THEN
c          VNOM='Tf/Tw>1    A'
c        CJ=-TYA + DYA
c        CJM=0.

c      CALCUL de DCJ = dérivées = sensibilités
c        L=0
c	  DO 126 IK=1,NBRXI
c        IF     (NXI(IK).EQ.3)  THEN
c          DCJ(IK)=1.
c          L=L+1
c        ENDIF
c  126 CONTINUE

c      CALL TEST(L)


C ****************************************************************
C 3.    RESTRICTIONS GEOMETRIQUES DES RAIDISSEURS
C ****************************************************************
C     RESTRICTIONS nø 201 :  Df-Dw<0
C     -----------------------------------------------------------
      ELSE IF(IC.EQ.201) THEN
          VNOM='Df-Dw<0    R'
        CJ=WXR-HXR
        CJM=0.

c      CALCUL de DCJ = dérivées = sensibilités
        L=0
        DO 201 IK=1,NBRXI
        IF     (NXI(IK).EQ.6)  THEN
          DCJ(IK)=-1.
          L=L+1
        ELSE IF(NXI(IK).EQ.8)  THEN
          DCJ(IK)=+1.
          L=L+1
        ENDIF
  201 CONTINUE

      CALL TEST(L)


C     RESTRICTIONS nø 202 :  Dw-2*Df<0
C     -----------------------------------------------------------
      ELSE IF(IC.EQ.202) THEN
          VNOM='Dw-2*Df<0  R'
        CJ=HXR-2.*WXR
        CJM=0.

c      CALCUL de DCJ = dérivées = sensibilités
        L=0
        DO 202 IK=1,NBRXI
        IF     (NXI(IK).EQ.6)  THEN
          DCJ(IK)=1.
          L=L+1
        ELSE IF(NXI(IK).EQ.8)  THEN
          DCJ(IK)=-2.
          L=L+1
        ENDIF
  202 CONTINUE

      CALL TEST(L)

C     RESTRICTIONS nø 203 :  Dw-40*Tw<0
C     -----------------------------------------------------------
      ELSE IF(IC.EQ.203) THEN
          VNOM='Dw-40*Tw<0 R'
        CJ=HXR-40.*DXR
        CJM=0.

c      CALCUL de DCJ = dérivées = sensibilités
        L=0
        DO 203 IK=1,NBRXI
        IF     (NXI(IK).EQ.6)  THEN
          DCJ(IK)=1.
          L=L+1
        ELSE IF(NXI(IK).EQ.7)  THEN
          DCJ(IK)=-40.
          L=L+1
        ENDIF
  203 CONTINUE

      CALL TEST(L)

C     RESTRICTIONS nø 204 :  d-2Tw<0
C     -----------------------------------------------------------
      ELSE IF(IC.EQ.204) THEN
          VNOM='d-2Tw<0    R'
        CJ=DELTA-2.*DXR
        CJM=0.

c      CALCUL de DCJ = dérivées = sensibilités
        L=0
        DO 204 IK=1,NBRXI
        IF     (NXI(IK).EQ.1)  THEN
          DCJ(IK)=1.
          L=L+1
        ELSE IF(NXI(IK).EQ.7)  THEN
          DCJ(IK)=-2.
          L=L+1
        ENDIF
  204 CONTINUE

      CALL TEST(L)

C     RESTRICTIONS nø 205 : 3d-Dw<0
C     -----------------------------------------------------------
      ELSE IF(IC.EQ.205) THEN
          VNOM='3d-Dw<0    R'
        CJ=3.*DELTA-HXR
        CJM=0.

c     CALCUL de DCJ = dérivées = sensibilités
        L=0
        DO 205 IK=1,NBRXI
        IF     (NXI(IK).EQ.1)  THEN
          DCJ(IK)=3.
          L=L+1
        ELSE IF(NXI(IK).EQ.6)  THEN
          DCJ(IK)=-1.
          L=L+1
        ENDIF
  205 CONTINUE

      CALL TEST(L)


C     RESTRICTIONS nø 209 : 0.625*Df-Dw<0
C     -----------------------------------------------------------
      ELSE IF(IC.EQ.209) THEN
          VNOM='0.6Df-Dw<0 R'
        CJ=0.625*WXR-HXR
        CJM=0.

c     CALCUL de DCJ = dérivées = sensibilités
        L=0
        DO 209 IK=1,NBRXI
        IF     (NXI(IK).EQ.6)  THEN
          DCJ(IK)=-1.
          L=L+1
        ELSE IF(NXI(IK).EQ.8)  THEN
          DCJ(IK)=0.625
          L=L+1
        ENDIF
  209 CONTINUE

      CALL TEST(L)


C     RESTRICTIONS nø 210 : Dw-2.5*Df   <0
C     -----------------------------------------------------------
      ELSE IF(IC.EQ.210) THEN
          VNOM='Dw-2.5Df<0 R'
        CJ=HXR-2.5*WXR
        CJM=0.

c      CALCUL de DCJ = dérivées = sensibilités
        L=0
        DO 210 IK=1,NBRXI
        IF     (NXI(IK).EQ.6)  THEN
          DCJ(IK)=1.
          L=L+1
        ELSE IF(NXI(IK).EQ.8)  THEN
          DCJ(IK)=-2.5
          L=L+1
        ENDIF
  210 CONTINUE

      CALL TEST(L)


C     RESTRICTIONS nø 211 : Dw-36*Tw <0
C     -----------------------------------------------------------
      ELSE IF(IC.EQ.211) THEN
          VNOM='Dw-36*Tw<0 R'
        CJ=HXR-36.*DXR
        CJM=0.

c     CALCUL de DCJ = dérivées = sensibilités
        L=0
        DO 211 IK=1,NBRXI
        IF     (NXI(IK).EQ.6)  THEN
          DCJ(IK)=1.
          L=L+1
        ELSE IF(NXI(IK).EQ.7)  THEN
          DCJ(IK)=-36.
          L=L+1
        ENDIF
  211 CONTINUE

      CALL TEST(L)


C     RESTRICTIONS nø 212 : Tw-2*Delta<0
C     ------------------------------------
      ELSE IF(IC.EQ.212) THEN
          VNOM='Tw-2Delt<0 R'
        CJ=DXR-2.*DELTA
        CJM=0.

c     CALCUL de DCJ = dérivées = sensibilités
        L=0
        DO 212 IK=1,NBRXI
        IF     (NXI(IK).EQ.1)  THEN
          DCJ(IK)=-2.
          L=L+1
        ELSE IF(NXI(IK).EQ.7)  THEN
          DCJ(IK)=1.
          L=L+1
        ENDIF
  212 CONTINUE

      CALL TEST(L)


C     RESTRICTIONS nø 213 : Dw-120*Tw  <0
C     -------------------------------------
      ELSE IF(IC.EQ.213) THEN
          VNOM='Dw-120Tw<0 R'
        CJ=HXR-120.*DXR
        CJM=0.

c     CALCUL de DCJ = dérivées = sensibilités
        L=0
        DO 213 IK=1,NBRXI
        IF     (NXI(IK).EQ.6)  THEN
          DCJ(IK)=1.
          L=L+1
        ELSE IF(NXI(IK).EQ.7)  THEN
          DCJ(IK)=-120.
          L=L+1
        ENDIF
  213 CONTINUE

      CALL TEST(L)


C     RESTRICTIONS nø 214 : 0.0065+Dw/170-Tw  <0
C     --------------------------------------------
      ELSE IF(IC.EQ.214) THEN
          VNOM='Dw/a-Tw<b  R'
        CJ=0.0065+HXR/170.-DXR
        CJM=0.

c     CALCUL de DCJ = dérivées = sensibilités
        L=0
        DO 214 IK=1,NBRXI
        IF     (NXI(IK).EQ.6)  THEN
          DCJ(IK)=1/170.
          L=L+1
        ELSE IF(NXI(IK).EQ.7)  THEN
          DCJ(IK)=-1.
          L=L+1
        ENDIF
  214 CONTINUE

      CALL TEST(L)


C     RESTRICTIONS nø 215 : 1.25*Df-Dw<0
C     -----------------------------------------------------------
      ELSE IF(IC.EQ.215) THEN
          VNOM='1.2Df-Dw<0 R'
        CJ=1.25*WXR-HXR
        CJM=0.

c     CALCUL de DCJ = dérivées = sensibilités
        L=0
        DO 215 IK=1,NBRXI
        IF     (NXI(IK).EQ.6)  THEN
          DCJ(IK)=-1.
          L=L+1
        ELSE IF(NXI(IK).EQ.8)  THEN
          DCJ(IK)=1.25
          L=L+1
        ENDIF
  215 CONTINUE

      CALL TEST(L)


C     RESTRICTIONS nø 216 : Dw-5*Df   <0
C     -----------------------------------------------------------
      ELSE IF(IC.EQ.216) THEN
          VNOM='Dw-5*Df<0  R'
        CJ=HXR-5.*WXR
        CJM=0.

c     CALCUL de DCJ = dérivées = sensibilités
        L=0
        DO 216 IK=1,NBRXI
        IF     (NXI(IK).EQ.6)  THEN
          DCJ(IK)=1.
          L=L+1
        ELSE IF(NXI(IK).EQ.8)  THEN
          DCJ(IK)=-5.
          L=L+1
        ENDIF
  216 CONTINUE

      CALL TEST(L)

C     RESTRICTIONS nø 220 : -Df/Tf   <-1,4
C     -----------------------------------------------------------
c      ELSE IF(IC.EQ.220) THEN
c          VNOM='Df/Tf>1.4  R'
c        CJ=-WXR + 1.4*TXR
c        CJM=0.

c     CALCUL de DCJ = dérivées = sensibilités
c        L=0
c        DO 220 IK=1,NBRXI
c        IF     (NXI(IK).EQ.8)  THEN
c          DCJ(IK)=-1.
c          L=L+1
c        ENDIF
c  220 CONTINUE

c      CALL TEST(L)

C     RESTRICTIONS nø 221 : Df/Tf    <15
C     -----------------------------------------------------------
c      ELSE IF(IC.EQ.221) THEN
c          VNOM='Df/Tf<15   R'
c        CJ=WXR - 15.*TXR
c        CJM=0.

c     CALCUL de DCJ = dérivées = sensibilités
c        L=0
c        DO 221 IK=1,NBRXI
c        IF     (NXI(IK).EQ.8)  THEN
c          DCJ(IK)=1.
c          L=L+1
c        ENDIF
c  221 CONTINUE

c      CALL TEST(L)

C     RESTRICTIONS nø 220 : -Dw/Tw   <-8
C     -----------------------------------------------------------
      ELSE IF(IC.EQ.220) THEN
          VNOM='Dw/Tw>8    R'
        CJ=-HXR + 8.*DXR
        CJM=0.

c     CALCUL de DCJ = dérivées = sensibilités
        L=0
        DO 220 IK=1,NBRXI
        IF     (NXI(IK).EQ.7)  THEN
          DCJ(IK)=8.
          L=L+1
        ELSE IF(NXI(IK).EQ.6)  THEN
          DCJ(IK)=-1.
          L=L+1
        ENDIF
  220 CONTINUE

      CALL TEST(L)

C     RESTRICTIONS nø 221 : Dw/Tw   <50
C     -----------------------------------------------------------
      ELSE IF(IC.EQ.221) THEN
          VNOM='Dw/Tw<50   R'
        CJ=HXR - 50.*DXR
        CJM=0.

c     CALCUL de DCJ = dérivées = sensibilités
        L=0
        DO 221 IK=1,NBRXI
        IF     (NXI(IK).EQ.7)  THEN
          DCJ(IK)=-50.
          L=L+1
        ELSE IF(NXI(IK).EQ.6)  THEN
          DCJ(IK)=1.
          L=L+1
        ENDIF
  221 CONTINUE

      CALL TEST(L)

C     RESTRICTIONS nø 230 : -Df/Dw   <-0,2
C     -----------------------------------------------------------
      ELSE IF(IC.EQ.230) THEN
          VNOM='Df/Dw>0.2  R'
        CJ=-WXR + 0.2*HXR
        CJM=0.

c     CALCUL de DCJ = dérivées = sensibilités
        L=0
        DO 230 IK=1,NBRXI
        IF     (NXI(IK).EQ.6)  THEN
          DCJ(IK)=0.2
          L=L+1
        ELSE IF(NXI(IK).EQ.8)  THEN
          DCJ(IK)=-1.
          L=L+1
        ENDIF
  230 CONTINUE

      CALL TEST(L)

C     RESTRICTIONS nø 231 : Df/Dw   <0,5
C     -----------------------------------------------------------
      ELSE IF(IC.EQ.231) THEN
          VNOM='Df/Dw<0.5  R'
        CJ=WXR - 0.5*HXR
        CJM=0.

c     CALCUL de DCJ = dérivées = sensibilités
        L=0
        DO 231 IK=1,NBRXI
        IF     (NXI(IK).EQ.6)  THEN
          DCJ(IK)=-0.5
          L=L+1
        ELSE IF(NXI(IK).EQ.8)  THEN
          DCJ(IK)=1.
          L=L+1
        ENDIF
  231 CONTINUE

      CALL TEST(L)

C     RESTRICTIONS nø 232 : -Dw/Tw   <-28
C     -----------------------------------------------------------
      ELSE IF(IC.EQ.232) THEN
          VNOM='Dw/Tw>28   R'
        CJ=-HXR + 28.*DXR
        CJM=0.

c     CALCUL de DCJ = dérivées = sensibilités
        L=0
        DO 232 IK=1,NBRXI
        IF     (NXI(IK).EQ.7)  THEN
          DCJ(IK)=28.
          L=L+1
        ELSE IF(NXI(IK).EQ.6)  THEN
          DCJ(IK)=-1.
          L=L+1
        ENDIF
  232 CONTINUE

      CALL TEST(L)

C     RESTRICTIONS nø 233 : Dw/Tw   <90
C     -----------------------------------------------------------
      ELSE IF(IC.EQ.233) THEN
          VNOM='Dw/Tw<90   R'
        CJ=HXR - 90.*DXR
        CJM=0.

c     CALCUL de DCJ = dérivées = sensibilités
        L=0
        DO 233 IK=1,NBRXI
        IF     (NXI(IK).EQ.7)  THEN
          DCJ(IK)=-90.
          L=L+1
        ELSE IF(NXI(IK).EQ.6)  THEN
          DCJ(IK)=1.
          L=L+1
        ENDIF
  233 CONTINUE

      CALL TEST(L)

C     RESTRICTIONS nø 234 : -Tw   <-8
C     -----------------------------------------------------------
      ELSE IF(IC.EQ.234) THEN
          VNOM='Tw>8       R'
        CJ=-DXR
        CJM=-0.008 !Unité

c     CALCUL de DCJ = dérivées = sensibilités
        L=0
        DO 234 IK=1,NBRXI
        IF     (NXI(IK).EQ.7)  THEN
          DCJ(IK)=-1.
          L=L+1
	  ENDIF
  234 CONTINUE

      IF (L.EQ.0) THEN
	  WRITE(*,1)
	ENDIF

C     RESTRICTIONS nø 235 : Tw   <20
C     -----------------------------------------------------------
      ELSE IF(IC.EQ.235) THEN
          VNOM='Tw<20      R'
        CJ=DXR
        CJM=0.020  !Unité

c     CALCUL de DCJ = dérivées = sensibilités
        L=0
        DO 235 IK=1,NBRXI
        IF     (NXI(IK).EQ.7)  THEN
          DCJ(IK)=1.
          L=L+1
	  ENDIF
  235 CONTINUE

      IF (L.EQ.0) THEN
	  WRITE(*,1)
	ENDIF

C     RESTRICTIONS nø 236 : -Df/Dw   <-0,1
C     -----------------------------------------------------------
      ELSE IF(IC.EQ.236) THEN
          VNOM='Df/Dw>0.1  R'
        CJ=-WXR + 0.1*HXR
        CJM=0.

c     CALCUL de DCJ = dérivées = sensibilités
        L=0
        DO 236 IK=1,NBRXI
        IF     (NXI(IK).EQ.6)  THEN
          DCJ(IK)=0.1
          L=L+1
        ELSE IF(NXI(IK).EQ.8)  THEN
          DCJ(IK)=-1.
          L=L+1
        ENDIF
  236 CONTINUE

      CALL TEST(L)

C     RESTRICTIONS nø 237 : Dw/Tw   <130
C     -----------------------------------------------------------
      ELSE IF(IC.EQ.237) THEN
          VNOM='Dw/Tw<130  R'
        CJ=HXR - 130.*DXR
        CJM=0.

c     CALCUL de DCJ = dérivées = sensibilités
        L=0
        DO 237 IK=1,NBRXI
        IF     (NXI(IK).EQ.7)  THEN
          DCJ(IK)=-130.
          L=L+1
        ELSE IF(NXI(IK).EQ.6)  THEN
          DCJ(IK)=1.
          L=L+1
        ENDIF
  237 CONTINUE

      CALL TEST(L)

C     RESTRICTIONS nø 240 : -Df/Dw   <-0,1 (Lq on simule traverse sur axe sym)
C     -----------------------------------------------------------
      ELSE IF(IC.EQ.240) THEN
          VNOM='2Df/Dw>0.2 R'
        CJ=-WXR + 0.1*HXR
        CJM=0.

c     CALCUL de DCJ = dérivées = sensibilités
        L=0
        DO 240 IK=1,NBRXI
        IF     (NXI(IK).EQ.6)  THEN
          DCJ(IK)=0.1
          L=L+1
        ELSE IF(NXI(IK).EQ.8)  THEN
          DCJ(IK)=-1.
          L=L+1
        ENDIF
  240 CONTINUE

      CALL TEST(L)

C     RESTRICTIONS nø 241 : Df/Dw   <0,25 (Lq on simule traverse sur axe sym)
C     -----------------------------------------------------------
      ELSE IF(IC.EQ.241) THEN
          VNOM='2Df/Dw<0.5 R'
        CJ=WXR - 0.25*HXR
        CJM=0.

c     CALCUL de DCJ = dérivées = sensibilités
        L=0
        DO 241 IK=1,NBRXI
        IF     (NXI(IK).EQ.6)  THEN
          DCJ(IK)=-0.25
          L=L+1
        ELSE IF(NXI(IK).EQ.8)  THEN
          DCJ(IK)=1.
          L=L+1
        ENDIF
  241 CONTINUE

      CALL TEST(L)

C     RESTRICTIONS nø 242 : -Dw/Tw   <-56 (Lq on simule traverse sur axe sym)
C     -----------------------------------------------------------
      ELSE IF(IC.EQ.242) THEN
          VNOM='Dw/2.Tw>28 R'
        CJ=-HXR + 56.*DXR
        CJM=0.

c     CALCUL de DCJ = dérivées = sensibilités
        L=0
        DO 242 IK=1,NBRXI
        IF     (NXI(IK).EQ.7)  THEN
          DCJ(IK)=56.
          L=L+1
        ELSE IF(NXI(IK).EQ.6)  THEN
          DCJ(IK)=-1.
          L=L+1
        ENDIF
  242 CONTINUE

      CALL TEST(L)

C     RESTRICTIONS nø 243 : Dw/Tw   <180 (Lq on simule traverse sur axe sym)
C     -----------------------------------------------------------
      ELSE IF(IC.EQ.243) THEN
          VNOM='Dw/2.Tw<90 R'
        CJ=HXR - 180.*DXR
        CJM=0.

c     CALCUL de DCJ = dérivées = sensibilités
        L=0
        DO 243 IK=1,NBRXI
        IF     (NXI(IK).EQ.7)  THEN
          DCJ(IK)=-180.
          L=L+1
        ELSE IF(NXI(IK).EQ.6)  THEN
          DCJ(IK)=1.
          L=L+1
        ENDIF
  243 CONTINUE

      CALL TEST(L)

C     RESTRICTIONS nø 244 : -Tw   <-4 (Lq on simule traverse sur axe sym)
C     -----------------------------------------------------------
      ELSE IF(IC.EQ.244) THEN
          VNOM='2.Tw>8     R'
        CJ=-DXR
        CJM=-0.004 !Unité

c     CALCUL de DCJ = dérivées = sensibilités
        L=0
        DO 244 IK=1,NBRXI
        IF     (NXI(IK).EQ.7)  THEN
          DCJ(IK)=-1.
          L=L+1
	  ENDIF
  244 CONTINUE

      IF (L.EQ.0) THEN
	  WRITE(*,1)
	ENDIF

C     RESTRICTIONS nø 245 : Tw   <10 (Lq on simule traverse sur axe sym)
C     -----------------------------------------------------------
      ELSE IF(IC.EQ.245) THEN
          VNOM='2.Tw<20    R'
        CJ=DXR
        CJM=0.010  !Unité

c     CALCUL de DCJ = dérivées = sensibilités
        L=0
        DO 245 IK=1,NBRXI
        IF     (NXI(IK).EQ.7)  THEN
          DCJ(IK)=1.
          L=L+1
	  ENDIF
  245 CONTINUE

      IF (L.EQ.0) THEN
	  WRITE(*,1)
	ENDIF

C     RESTRICTIONS nø 246 : -Df/Dw   <-0,05 (Lq on simule traverse sur axe sym)
C     -----------------------------------------------------------
      ELSE IF(IC.EQ.246) THEN
          VNOM='2Df/Dw>0.1 R'
        CJ=-WXR + 0.05*HXR
        CJM=0.

c     CALCUL de DCJ = dérivées = sensibilités
        L=0
        DO 246 IK=1,NBRXI
        IF     (NXI(IK).EQ.6)  THEN
          DCJ(IK)=0.05
          L=L+1
        ELSE IF(NXI(IK).EQ.8)  THEN
          DCJ(IK)=-1.
          L=L+1
        ENDIF
  246 CONTINUE

      CALL TEST(L)

C     RESTRICTIONS nø 247 : Dw/Tw   <260 (Lq on simule traverse sur axe sym)
C     -----------------------------------------------------------
      ELSE IF(IC.EQ.247) THEN
          VNOM='Dw/2Tw<130 R'
        CJ=HXR - 260.*DXR
        CJM=0.

c     CALCUL de DCJ = dérivées = sensibilités
        L=0
        DO 247 IK=1,NBRXI
        IF     (NXI(IK).EQ.7)  THEN
          DCJ(IK)=-260.
          L=L+1
        ELSE IF(NXI(IK).EQ.6)  THEN
          DCJ(IK)=1.
          L=L+1
        ENDIF
  247 CONTINUE

      CALL TEST(L)



C ****************************************************************
C 4.    RESTRICTIONS CROISEES ENTRE CADRES et RAIDISSEURS
C ****************************************************************

C     RESTRICTIONS nø 301 : Dw(R)-Dw(A)<0
C     -----------------------------------------------------------
      ELSE IF(IC.EQ.301) THEN
          VNOM='Dw(R) <Dw(A)'
        CJ=HXR-HYA
        CJM=0.

c      CALCUL de DCJ = dérivées = sensibilités
        L=0
        DO 301 IK=1,NBRXI
        IF     (NXI(IK).EQ.2)  THEN
          DCJ(IK)=-1.
          L=L+1
        ELSE IF(NXI(IK).EQ.6)  THEN
          DCJ(IK)=1.
          L=L+1
        ENDIF
  301 CONTINUE

      CALL TEST(L)

C     RESTRICTIONS nø 302 : Tw(R)-4*Tw(A)<0
C     -----------------------------------------------------------
      ELSE IF(IC.EQ.302) THEN
          VNOM='Tw(R)<4Tw(A)'
        CJ=DXR-4.*DYA
        CJM=0.

c      CALCUL de DCJ = dérivées = sensibilités
        L=0
        DO 302 IK=1,NBRXI
        IF     (NXI(IK).EQ.3)  THEN
          DCJ(IK)=1.
          L=L+1
        ELSE IF(NXI(IK).EQ.7)  THEN
          DCJ(IK)=-1.
          L=L+1
        ENDIF
  302 CONTINUE

      CALL TEST(L)

C     RESTRICTIONS nø 303 : Tw(A)-4*Tw(R)<0
C     -----------------------------------------------------------
      ELSE IF(IC.EQ.303) THEN
          VNOM='Tw(A)<4Tw(R)'
        CJ=DYA-4.*DXR
        CJM=0.

c      CALCUL de DCJ = dérivées = sensibilités
        L=0
        DO 303 IK=1,NBRXI
        IF     (NXI(IK).EQ.3)  THEN
          DCJ(IK)=1.
          L=L+1
        ELSE IF(NXI(IK).EQ.7)  THEN
          DCJ(IK)=-1.
          L=L+1
        ENDIF
  303 CONTINUE

      CALL TEST(L)


C     RESTRICTIONS nø 304 : Dw(A)-Dw(R)<0
C     -----------------------------------------------------------
      ELSE IF(IC.EQ.304) THEN
          VNOM='Dw(A) <Dw(R)'
        CJ=HYA-HXR
        CJM=0.

c      CALCUL de DCJ = dérivées = sensibilités
        L=0
        DO 304 IK=1,NBRXI
        IF     (NXI(IK).EQ.2)  THEN
          DCJ(IK)=1.
          L=L+1
        ELSE IF(NXI(IK).EQ.6)  THEN
          DCJ(IK)=-1.
          L=L+1
        ENDIF
  304 CONTINUE

C     RESTRICTIONS nø 320 : -Dw(A)/Dw(R)<-2
C     -----------------------------------------------------------
      ELSE IF(IC.EQ.320) THEN
          VNOM='DwA/Dw(R) >2'
        CJ=-HYA +2.*HXR
        CJM=0.

c      CALCUL de DCJ = dérivées = sensibilités
        L=0
        DO 320 IK=1,NBRXI
        IF     (NXI(IK).EQ.2)  THEN
          DCJ(IK)=-1.
          L=L+1
        ELSE IF(NXI(IK).EQ.6)  THEN
          DCJ(IK)=2.
          L=L+1
        ENDIF
  320 CONTINUE

      CALL TEST(L)

C ****************************************************************
C 5.    Autres restrictions
C ****************************************************************
      ELSE
       WRITE(*,*)  ' Nø de la restr. géométrique ',IC,'est incorrecte'
       WRITE(666,*)' Nø de la restr. géométrique ',IC,'est incorrecte'
       WRITE(29,*)' Nø de la restr. géométrique ',IC,'est incorrecte'				!bug
       PAUSE 'STOP'
         STOP
      END IF

C     SAUVETAGE sur File 204 et IMPRESSIONS sur FILE 666
C     ---------------------------------------------------
        WRITE(303) NTOT		!extension neto
        WRITE(303) (DCJ(I),I=1,NBRXI),CJ,CJM,VNOM		!extension neto

c     IF(IPRINT.GE.1) WRITE(*,100)IT,IC,CJ,CJM,VNOM,(DCJ(I),I=1,NBRXI)
      IF((ITERA.EQ.0).AND.(IMPR.GE.-1)) THEN					!13.12.05
        WRITE(666,427) IT,CJ,CJM,IC,VNOM
        ENDIF

      RETURN

   1  FORMAT(' ERROR'/'********'/
     *       ' Les variables liées à cette restriction ne sont pas'
     *       ' définies comme variables de conception !!!')

  100 FORMAT(' Restr .nø',I3,' de type ',I3,'; C=',E14.7,' < ',E11.5,
     *       ' (',A12,')'/'DC=',(T4,9(E11.4,1X)))
  427 FORMAT('C(',I3,')= ',E11.5,' < ',E11.5,' de type nø',I3,
     *       ' (',A12,')')
      END


C **********************************************************************
      SUBROUTINE TEST(L)
C     -------------------
C     ERROR Si les variables (hauteur, épaisseur, etc.) n'interviennent
C     comme variables de conception.

      IMPLICIT REAL *8 (A-H,O-Z)

      IF(L.EQ.0) THEN
          WRITE(*,1)
          WRITE(666,1)
		WRITE(29,1)																	!bug
          PAUSE 'STOP'
          RETURN
      ELSE IF (L.EQ.1) THEN
          WRITE(*,2)
          WRITE(666,2)
		WRITE(29,1)																	!bug
      ENDIF

      RETURN
   1  FORMAT(' ERROR'/'********'/
     *       ' Les variables liées à cette restriction ne sont pas'
     *       ' définies comme variables de conception !!!')
   2  FORMAT(' ATTENTION'/'**********'/
     *       ' UNE SEULE VARIABLE liée à cette restriction est'
     *       ' définie comme variable de conception !!!')
      END
C **********************************************************************
