      SUBROUTINE DHYPS(PHIL,TETA,Q,M,ARGQ,DISH,DISA,DISB,DISC,DISD,
     *           DVARA,DVARB,DVARC,DVARD,DVARH,IMPR,MT,MS,ABTR,
     *           NBRXI,NXI,HYP,AUX2,DAUX,DZB,DHYP,NSOL)

      IMPLICIT REAL *8(A-H,O-Z)
      DIMENSION DISH(1710,NSOL),DISA(720),DISB(720),DISC(720),DISD(720),
     * DVARA(33,9,16),DVARB(33,9,16),DVARC(33,9,16),DVARD(33,9,16),
     * DVARH(33,9,38,NSOL),ARGQ(8),NXI(9),ABTR(10)

      DIMENSION HYP(360),AUX2(MS,MS),DZB(MS,8+ISmax),DAUX(MS,MS),
     *          DHYP(20,8+ISmax,9)
      COMMON/PY/PI
      COMMON/OPTI3/ BID1(54),BID2(20),DARG(8,9)
      COMMON/DIM1/Nmax,NEmax,NVmax,      ! dimension max des vecteurs
     *            M1max,M1Tmax,M2max,M2Tmax,Mmax,NGmax,ISmax,IPTmax 

C***********************************************************************
C     SUBROUTINE DHYP (Optimisation)
C     ===============================
C     La subr. calcule les dérivées des effets des traverses (Xo et Zo)
C
C     Rappel: HYP est la solution du système [ AUX ] * HYP = ZB
C                          (pour les "8+NSOL" vecteurs indépendants, max
C
C                             -1
C   Calculons: DHYP = [ AUX ]     * (DZB - [ DAUX ] * HYP)
C              ============================================
C   Avec:
C   AUX2(MS,MS) = Inverse de la matrice AUX.
C                 Dans BO1 la mat. inverse est contenue dans AUX (cfr. subr RESOLU
C   DAUX(MS,MS) = Dérivée de la matrice AUX par la variable XI=1,NBRXI
C   DZB (MS,13) = Dérivée des "8+NSOL" vecteurs ZB par la variable XI=1,NBRXI
C   DHYP(I,J,K) = Dérivée des vecteurs solutions de forces Xo et Zo (traverses)
C        si K  la variable de conception (1 à 9)
C           J  les "8+NSOL" cas de charge (DISH(IS=1,NSOL), DISC en y=0 et en Yo, DISB, ...)
C           I  les dérivées de Xo pour les MT traverses + idem avec Zo
C
C     Créer :   21-6-95       par Ph. Rigo
C     *******
C     Modifier: le  5-07-95
C     ********* le 25-03-96 Supprimer KSE comme arg.
C               le 29-03-96 DZB1 : termes en y**2 et y**3
C               le  1-08-96 COMMON/opti3/ ds subr. DHYP comme dans DZB1,
C               le  4-08-96 Correction dans DINTEQ
C***********************************************************************
C  Ordre des variables dans DVARH
C      U,V,W; ... de 1 à 13
C      (Xo et Zo) de (14 et 15) à (32 et 33) pour les traverses 1 à 10

        CALL ANNULD(DHYP,9*20*(8+NSOL))

c     BOUCLE SUR LES VARIABLES DE CONCEPTION
C     ========================================
      DO 1 IVAR=1,NBRXI
        K=NXI(IVAR)

      IF(IMPR.GE.1) THEN
        WRITE(66,*) 'Soit la variable Xi nø',K
      WRITE(66,*) '++++++++++++++++++++++++++++'
        ENDIF

        CALL ANNULD(DAUX,MS*MS)
        CALL ANNULD(DZB,MS*(8+NSOL))

c     Calcul de la dérivée de matrice AUX = [ DAUX ]
C     -----------------------------------------------
      DO 2 LR=1,2
      CALL DINTEQ(ARGQ,Q,DISB,MT,M,LR,1,ABTR,DAUX,K,DVARB)
      CALL DINTEQ(ARGQ,Q,DISC,MT,M,LR,2,ABTR,DAUX,K,DVARC)
    2 CONTINUE

        IF(IMPR.GE.1) THEN
        WRITE(66,*) ' dérivée de AUX (subr DHYP )'
      WRITE(66,*) ' ---------------------------'
      DO 14 I=1,MS
      WRITE(66,16) (DAUX(I,J),J=1,MS)
   14 CONTINUE
      ENDIF

c     Calcul de la dérivée de ZB (soit DZB)
C     ----------------------------------------
      DO 19 IS=1,NSOL
          IF(IS.GT.1) THEN
            IS1=IS+8
          ELSE
            IS1=1
          ENDIF
        CALL DZB1(PHIL,TETA,ARGQ,Q,DISH(1,IS),MT,M,ABTR,DZB(1,IS1),K,
     *            DVARH(1,1,1,IS))
   19 CONTINUE

      CALL DZB2(PHIL,ARGQ,Q,DISC,MT,M,ABTR,DZB,K,DVARC,2,+1)
      CALL DZB2(PHIL,ARGQ,Q,DISC,MT,M,ABTR,DZB,K,DVARC,3,+1)
      CALL DZB2(PHIL,ARGQ,Q,DISD,MT,M,ABTR,DZB,K,DVARD,4,+1)
      CALL DZB2(PHIL,ARGQ,Q,DISD,MT,M,ABTR,DZB,K,DVARD,5,-1)
      CALL DZB2(PHIL,ARGQ,Q,DISA,MT,M,ABTR,DZB,K,DVARA,6,+1)
      CALL DZB2(PHIL,ARGQ,Q,DISA,MT,M,ABTR,DZB,K,DVARA,7,-1)
      CALL DZB2(PHIL,ARGQ,Q,DISB,MT,M,ABTR,DZB,K,DVARB,8,+1)
      CALL DZB2(PHIL,ARGQ,Q,DISB,MT,M,ABTR,DZB,K,DVARB,9,+1)

        IF(IMPR.GE.1) THEN
      WRITE(66,*) ' Dérivée de ZB (DISH ) cas de charge nø 1'
      WRITE(66,*) ' Dérivée de ZB (DISC (Y=0) ,DISC (Y=Yo))'
      WRITE(66,*) '                     (idem avec DISD, DISA, DISB )'
      WRITE(66,*) ' Dérivée de ZB (DISH ) cas de charge nø2 à NSOL'
      WRITE(66,*) ' -------------------------------------------------'
      DO 15 I=1,8+NSOL
      WRITE(66,17) I,(DZB(J,I),J=1,MS)
   15 CONTINUE
      ENDIF

c     Calcul de DHYP=  DZB - [ DAUX ] * HYP
C     --------------------------------------
C     DHYP(n,j,K) = [DHYP]  (MS,8+NSOL,NBRXI)
C     DZB = [d(ZB)/dx]         (MS,8+NSOL) car 8+NSOL cas de charge
C     DAUX = [d(AUX)/dx]       (MS,MS)
C     HYP = vecteurs solutions (MS*(8+NSOL)) car 8+NSOL cas de charge

      DO 10 J=1,8+NSOL
      DO 10 I=1,MS
        DHYP(I,J,K)=DZB(I,J)
        DO 11 L=1,MS
                JL=(J-1)*20+L
        DHYP(I,J,K)=DHYP(I,J,K) - DAUX(I,L) * HYP(JL)
   11   CONTINUE
   10 CONTINUE

c      IF(IMPR.EQ.1) THEN
c      WRITE(66,*) '[ DAUX ] * HYP + DZB pour la variable nø ',K
c      DO 12 J=1,8+NSOL
c   12 WRITE(66,17) J,(DHYP(I,J,K),I=1,MS)
c      ENDIF

    1 CONTINUE

c     Multiplication par la matrice inverse (AUX2) (faire DHYP=AUX2* DHY
C     ------------------------------------------------------------------
C     (le résultat est placé dans DHYP et DZB sert de vecteur de travail
      IF(IMPR.GE.1) WRITE(66,18)

      DO 7 IVAR=1,NBRXI
        K=NXI(IVAR)
        DO 6 J=1,8+NSOL
          CALL ANNULD(DZB,MS)
         DO 8  I=1,MS
            DO 9 KH=1,MS
                   DZB(I,1)=DZB(I,1)+AUX2(I,KH)*DHYP(KH,J,K)
    9       CONTINUE
    8    CONTINUE
         DO 4 KH=1,MS
            DHYP(KH,J,K)=DZB(KH,1)
    4    CONTINUE
    6   CONTINUE

        IF(IMPR.GE.1) THEN
        WRITE(66,*)' '
        WRITE(66,*)'VARIABLE DE CONCEPTION nø',K
        DO 5 J=1,8+NSOL
    5     WRITE(66,17) J,(DHYP(KH,J,K),KH=1,MS)
        ENDIF
    7 CONTINUE

      RETURN
  16  FORMAT(6E11.4)
  17  FORMAT(I2,6E11.4)
  18  FORMAT(/'Dérivées des HYP (Xo et Zo) pour chaque variable XI'
     *       /40(1H=))
      END

C***********************************************************************
C***********************************************************************

      SUBROUTINE DINTEQ (ARGQ,Q,DISX,MT,M,LR,KC,ABTR,DAUX,K,DVAR)
C     ==================
      IMPLICIT REAL *8(A-H,O-Z)
      DIMENSION ARGQ(8),DISX(360),ABTR(MT),DAUX(2*MT,2*MT),
     *          DVAR(33,9,16)
      COMMON/PY/PI
      COMMON/OPTI3/ BID1(54),BID2(20),DARG(8,9)
C***********************************************************************
C     SUBROUTINE DINTEQ
C     ------------------
C     SUBROUTINE DE REMPLISSAGE DES COEFFICIENTS DE LA DERIVEE DE LA
C     MATRICE DES INCONNUES HYPERSTATIQUES DES TRAVERSES
C
C     Resultat : La matrice [ DAUX ].
C     ---------
c
C     modifié: le 4-8-96 (Ph. Rigo)                  Créer : Thèse
C***********************************************************************
C   Avec:ABTR = ABTR(I),I=1,MT
C          DISX = DISB ou DISC
C           KC  = indice sur les sollicitations: KC = 1  DISB et KC = 2
C           LR  = indice sur les forces        : KC = 1  Xo   et KC = 2
C            K  = nø de la variable de conception

      PI1=PI/180.
      PI2=2.*PI

      KI=(KC-1)*MT+1
      KF=KC*MT
      LI=1+(LR-1)*MT
      LF=LR*MT

c       WRITE(66,*) 'LR=',LR,' et KC=',KC
c       WRITE(66,*) '===================='
c       WRITE(66,*)' ARGQ',ARGQ
c       WRITE(66,*)' Q',Q

      DO 14 J=KI,KF

c       WRITE(66,*) 'J=',J
c       WRITE(66,*) '------'

      DO 15 I=LI,LF
      N=11+2*(I-(LR-1)*MT)+LR

      K9=J-KI+1
      L9=I-LI+1
      Z1=DABS(ABTR(L9)-ABTR(K9))*PI1
      Z1Q=Z1*Q
      Z11=PI2-Z1
      Z11Q=Z11*Q

c       WRITE(66,*) 'Z1=',Z1,' et Z1Q=',Z1Q

      DO 16 L=1,M
      L2=2*L
      L1=L2-1
      JJ=4*(L-1)
      KH=JJ+LR*16+32*(I-(LR-1)*MT)

c       WRITE(66,*)'M,L,L1,L2,JJ,KH,N=',M,L,L1,L2,JJ,KH,N

      AL1=ARGQ(L1)*Z1
      BE1=ARGQ(L2)*Z1
c       WRITE(66,*)'AL1,BE1=',AL1,BE1

      AL3=ARGQ(L1)*Z11
      BE3=ARGQ(L2)*Z11
      AL1=EXPO(AL1)
      AL3=EXPO(AL3)
      COS1=AL1*DCOS(BE1)
      SIN1=AL1*DSIN(BE1)
      COS3=AL3*DCOS(BE3)
      SIN3=AL3*DSIN(BE3)
c       WRITE(66,*)'AL3,BE3=',AL3,BE3

      DAUX(I,J)=DAUX(I,J)
     * - Z1Q * ( COS1* (DARG(L1,K)*DISX(KH+1)-DARG(L2,K)*DISX(KH+2) )
     *          +SIN1* (DARG(L1,K)*DISX(KH+2)+DARG(L2,K)*DISX(KH+1) ) )
     * - Z11Q* ( COS3* (DARG(L1,K)*DISX(KH+3)-DARG(L2,K)*DISX(KH+4) )
     *          +SIN3* (DARG(L1,K)*DISX(KH+4)+DARG(L2,K)*DISX(KH+3) ) )
     * + COS1 * DVAR(N,K,JJ+1)  + SIN1 * DVAR(N,K,JJ+2)
     * + COS3 * DVAR(N,K,JJ+3)  + SIN3 * DVAR(N,K,JJ+4)

c       WRITE(66,*)'COS1,SIN1=',COS1,SIN1, ' COS3,SIN3=',COS3,SIN3
c       WRITE(66,*)'DARG(L1,K),DARG(L2,K)=',DARG(L1,K),DARG(L2,K)
c       WRITE(66,*)'DISX(KH+1à4)=',DISX(KH+1),DISX(KH+2),DISX(KH+3),
c     *                           DISX(KH+4)
c       WRITE(66,*)'DVAR(N,K,JJ+1 à 4)=',DVAR(N,K,JJ+1),DVAR(N,K,JJ+2),
c     *                                 DVAR(N,K,JJ+3),DVAR(N,K,JJ+4)
c       WRITE(66,*)'DAUX(I,J)=',DAUX(I,J)

 16   CONTINUE

      DAUX(I,J)=-DAUX(I,J)/10000.

 15   CONTINUE
 14   CONTINUE

      RETURN
      END


C***********************************************************************
C     Subroutine pour la charge extérieure (DISH)
C     +++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE DZB1(PHIL,TETA,ARGQ,Q,DISH,MT,M,ABTR,DZB,K,DVARH)

      IMPLICIT REAL *8(A-H,O-Z)
      DIMENSION ARGQ(8),DISH(1710),ABTR(MT),DZB(2*MT),DVARH(33,9,38)

      COMMON/PY/ PI
      COMMON/OPTI3/ BID1(54),BID2(20),DARG(8,9)

C***********************************************************************

      PI1=PI/180.
      PI2=2.*PI
      PHIL1=PHIL*PI1
      TETA1=TETA*PI1

C     Boucle sur les traverses (MT)
C       ------------------------------
      DO 3 I=1,MT

C     Soit Z1 la coordonnée du point de calcul par rapport à Y=O
      Z1=ABTR(I)*PI1
      Z1Q=Z1 * Q
      Z11=PI2-Z1
      Z11Q=Z11*Q
C     Soit Z2 la coordonnée du point de calcul  par rapport à Y=Yo (PHIL
      Z2=PHIL1-Z1
      Z2Q=Z2*Q
      Z22=PI2-Z2
      Z22Q=Z22*Q

c      WRITE(66,*)' Z1 =',Z1, ' Z1Q =',Z1Q
c      WRITE(66,*)' Z2 =',Z2, ' Z2Q =',Z2Q
c      WRITE(66,*)' Z11=',Z11,' Z11Q=',Z11Q
c      WRITE(66,*)' Z22=',Z22,' Z22Q=',Z22Q

      IF(DABS(TETA).EQ.180.) THEN
        COST=-DCOS(Z1)
        SINT=-DSIN(Z1)
      ELSE
        IF(DABS(TETA).EQ.90.) THEN
           ISS=1
               IF(TETA.EQ.-90.) ISS=-1
               COST=-DSIN(Z1)*ISS
               SINT= DCOS(Z1)*ISS
            ELSE
           COST=DCOS(TETA1+Z1)
           SINT=DSIN(TETA1+Z1)
        ENDIF
      ENDIF

c      WRITE(66,*)' COST =',COST, ' SINT =',SINT
c      WRITE(66,*)' TETA1 =',TETA1, ' Z1 =',Z1

C     II=1 pour Xo et II=2 pour Zo
C       ------------------------------
      DO 4 II=1,2

c       WRITE(66,*)'II=1 pour Xo et II=2 pour Zo : II=',II
c       WRITE(66,*)'------------------------------------'

      JJ=2*(I-1)+II
      J=13+JJ
      IK=(II-1)*MT+I

c      WRITE(66,*)'JJ=',JJ
c      WRITE(66,*)'IK=',IK
c      WRITE(66,*)'1 DZB(IK)=',DZB(IK)
c      WRITE(66,*)'  DVARH(J,K,1)=',DVARH(J,K,1)
c      WRITE(66,*)'  DVARH(J,K,5)=',DVARH(J,K,5)
c      WRITE(66,*)'  DVARH(J,K,6)=',DVARH(J,K,6)

      DZB(IK)=DVARH(J,K,1)+DVARH(J,K,2)*Z1
     *                    +DVARH(J,K,3)*Z1*Z1+DVARH(J,K,4)*(Z1**3)
     *                    +DVARH(J,K,5)*COST +DVARH(J,K,6)*SINT
c      WRITE(66,*)'2 DZB(IK)=',DZB(IK)

C     Boucle sur M
C       --------------
      DO 5 L=1,M
      L2=2*L
      L1=L2-1

        LK=(L-1)*8 + 6
      IJ=38*(JJ+2)+LK

c       WRITE(66,*)'L=',L
c       WRITE(66,*)'IJ=',IJ

      AL1=ARGQ(L1)*Z1
      BE1=ARGQ(L2)*Z1
      AL2=ARGQ(L1)*Z2
      BE2=ARGQ(L2)*Z2
      AL3=ARGQ(L1)*Z11
      BE3=ARGQ(L2)*Z11
      AL4=ARGQ(L1)*Z22
      BE4=ARGQ(L2)*Z22
      AL1=EXPO(AL1)
      AL2=EXPO(AL2)
      AL3=EXPO(AL3)
      AL4=EXPO(AL4)
      COS1=AL1*DCOS(BE1)
      SIN1=AL1*DSIN(BE1)
      COS2=AL2*DCOS(BE2)
      SIN2=AL2*DSIN(BE2)
      COS3=AL3*DCOS(BE3)
      SIN3=AL3*DSIN(BE3)
      COS4=AL4*DCOS(BE4)
      SIN4=AL4*DSIN(BE4)

c      WRITE(66,*)' COS1 =',COS1, ' SIN1 =',SIN1
c      WRITE(66,*)' COS2 =',COS2, ' SIN2 =',SIN2
c      WRITE(66,*)' COS3 =',COS3, ' SIN3 =',SIN3
c      WRITE(66,*)' COS4 =',COS4, ' SIN4 =',SIN4

      DZB(IK)=DZB(IK)
     * - Z1Q * ( COS1* (DARG(L1,K)*DISH(IJ+1)-DARG(L2,K)*DISH(IJ+2) )
     *          +SIN1* (DARG(L2,K)*DISH(IJ+1)+DARG(L1,K)*DISH(IJ+2) ) )
     * - Z2Q * ( COS2* (DARG(L1,K)*DISH(IJ+3)-DARG(L2,K)*DISH(IJ+4) )
     *          +SIN2* (DARG(L2,K)*DISH(IJ+3)+DARG(L1,K)*DISH(IJ+4) ) )
     * - Z11Q* ( COS3* (DARG(L1,K)*DISH(IJ+5)-DARG(L2,K)*DISH(IJ+6) )
     *          +SIN3* (DARG(L2,K)*DISH(IJ+5)+DARG(L1,K)*DISH(IJ+6) ) )
     * - Z22Q* ( COS4* (DARG(L1,K)*DISH(IJ+7)-DARG(L2,K)*DISH(IJ+8) )
     *          +SIN4* (DARG(L2,K)*DISH(IJ+7)+DARG(L1,K)*DISH(IJ+8) ) )
     * + COS1 * DVARH(J,K,LK+1)  + SIN1 * DVARH(J,K,LK+2)
     * + COS2 * DVARH(J,K,LK+3)  + SIN2 * DVARH(J,K,LK+4)
     * + COS3 * DVARH(J,K,LK+5)  + SIN3 * DVARH(J,K,LK+6)
     * + COS4 * DVARH(J,K,LK+7)  + SIN4 * DVARH(J,K,LK+8)

c      WRITE(66,*)'3  DZB(IK)=',DZB(IK)

    5 CONTINUE

    4 CONTINUE
    3 CONTINUE


      RETURN
      END

C***********************************************************************
C     Subroutine pour les 8 charges de bords (DISC, D, A, B) en Y=0 et Y
C     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      SUBROUTINE DZB2(PHIL,ARGQ,Q,DISX,MT,M,ABTR,DZB,K,DVARX,NBR,ISIGN)

      IMPLICIT REAL *8(A-H,O-Z)
      DIMENSION ARGQ(8),DISX(720),ABTR(MT),DZB(2*MT,9),
     *          DVARX(33,9,16)

      COMMON/PY/ PI
      COMMON/OPTI3/ BID1(54),BID2(20),DARG(8,9)

C***********************************************************************

      PI1=PI/180.
      PI2=2.*PI

C     Boucle sur les traverses (MT)
C       ------------------------------
      DO 3 I=1,MT

c       WRITE(66,*)'Traverse nø',I
c       WRITE(66,*)'=============='

        IF((NBR.EQ.2).OR.(NBR.EQ.4).OR.(NBR.EQ.6).OR.(NBR.EQ.8)) THEN
C     Soit Z1 la coordonnée du point de calcul par rapport à Y=O
      Z1=ABTR(I)*PI1
      Z1Q=Z1 * Q
      Z11=PI2-Z1
      Z11Q=Z11*Q
      ELSE
C     Soit Z1 la coordonnée du point de calcul  par rapport à Y=Yo (PHIL
      Z1=(PHIL-ABTR(I))*PI1
      Z1Q=Z1*Q
      Z11=PI2-Z1
      Z11Q=Z11*Q
      ENDIF

c      WRITE(66,*)' Z1 =',Z1, ' Z1Q =',Z1Q
c      WRITE(66,*)' SIGN =',SIGN,' NBR =',NBR


C     II=1 pour Xo et II=2 pour Zo
C       ------------------------------
      DO 4 II=1,2

c       WRITE(66,*)'II=1 pour Xo et II=2 pour Zo : II=',II
c       WRITE(66,*)'------------------------------------'

      JJ=2*(I-1)+II
      J=13+JJ
      IK=(II-1)*MT+I

c       WRITE(66,*)'JJ=',JJ
c       WRITE(66,*)'J=',J
c       WRITE(66,*)'IK=',IK

C     Boucle sur M
C       -------------
      DO 5 L=1,M
      L2=2*L
      L1=L2-1

      LK=(L-1)*4
      IJ=16*(JJ+2)+LK

c       WRITE(66,*)'LK=',LK
c       WRITE(66,*)'IJ=',IJ

      AL1=ARGQ(L1)*Z1
      BE1=ARGQ(L2)*Z1
      AL3=ARGQ(L1)*Z11
      BE3=ARGQ(L2)*Z11
      AL1=EXPO(AL1)
      AL3=EXPO(AL3)
      COS1=AL1*DCOS(BE1)
      SIN1=AL1*DSIN(BE1)
      COS3=AL3*DCOS(BE3)
      SIN3=AL3*DSIN(BE3)

c      WRITE(66,*)' COS1 =',COS1, ' SIN1 =',SIN1
c      WRITE(66,*)' DZB(IK,NBR)=',DZB(IK,NBR)
c      WRITE(66,*)' DISX(IJ+1)=',DISX(IJ+1),DISX(IJ+2)
c      WRITE(66,*)'DARG(L1,K) et (DARG(L2,K)=',DARG(L1,K),DARG(L2,K)
c      WRITE(66,*)'DVARX(J,K,LK+1)=',DVARX(J,K,LK+1),DVARX(J,K,LK+2)

      DZB(IK,NBR)=DZB(IK,NBR)
     * - Z1Q * ( COS1* (DARG(L1,K)*DISX(IJ+1)-DARG(L2,K)*DISX(IJ+2) )
     *          +SIN1* (DARG(L1,K)*DISX(IJ+2)+DARG(L2,K)*DISX(IJ+1) ) )
     * - Z11Q* ( COS3* (DARG(L1,K)*DISX(IJ+3)-DARG(L2,K)*DISX(IJ+4) )
     *          +SIN3* (DARG(L1,K)*DISX(IJ+4)+DARG(L2,K)*DISX(IJ+3) ) )
     * + COS1 * DVARX(J,K,LK+1)  + SIN1 * DVARX(J,K,LK+2)
     * + COS3 * DVARX(J,K,LK+3)  + SIN3 * DVARX(J,K,LK+4)

c      WRITE(66,*)' DZB(IK,NBR)=',DZB(IK,NBR)

    5 CONTINUE

      DZB(IK,NBR)=DZB(IK,NBR) * ISIGN

    4 CONTINUE
    3 CONTINUE


      RETURN
      END

