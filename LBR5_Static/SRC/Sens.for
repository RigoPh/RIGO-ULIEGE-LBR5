      SUBROUTINE SENS(PHIL,TETA,Q,M,ARGQ,DISH,DISA,DISB,DISC,DISD,
     *    DVARA,DVARB,DVARC,DVARD,DVARH,IMPR,MT,NBRXI,NXI,IPT,YPT,
     *    ABTR,ALIX,ALIZ,DALIX,DALIZ,DHYP,NSOL)

      IMPLICIT REAL *8(A-H,O-Z)
      DIMENSION DISH(1710,NSOL),DISA(720),DISB(720),DISC(720),DISD(720),
     * DVARA(33,9,16),DVARB(33,9,16),DVARC(33,9,16),DVARD(33,9,16),
     * DVARH(33,9,38,NSOL),ARGQ(8),ID(13),ID2(13),NXI(9),YPT(IPTmax),
     * ABTR(10),ALIX(10,13),ALIZ(10,13),DALIX(10,13,9),DALIZ(10,13,9),
     * DHYP(20,8+ISmax,9)

      COMMON/PY/PI
      COMMON/HYP/HYP(360)
      COMMON/DIM1/Nmax,NEmax,NVmax,
     *            M1max,M1Tmax,M2max,M2Tmax,Mmax,NGmax,ISmax,IPTmax ! dimension max des vecteurs

c     COMMOM OPTI2 (IPT = 4 et NSOL=10 a changer manuellement!!!!)
      COMMON/OPTI2/ DEFA(13,4) ,DEFB(13,4) ,DEFC(13,4) ,DEFD(13,4),
     *              DEFAA(13,4),DEFBB(13,4),DEFCC(13,4),DEFDD(13,4),
     * SENSH(13,4,9,10),
     * SENSA(13,4,9) ,SENSB(13,4,9) ,SENSC(13,4,9) ,SENSD(13,4,9) ,
     * SENSAA(13,4,9),SENSBB(13,4,9),SENSCC(13,4,9),SENSDD(13,4,9)

      COMMON/OPTI3/ BID1(54),BID2(20),DARG(8,9)

      DATA ID /2, 1,29,25, 27,24,26,3, 35, 38, 31,30,33/   ! (Ordre dans DISC, ...)
      DATA ID2/1,-1,-1,+1, -1,+1,-1,1, +1, -1, -1,+1,-1/   ! (+1 Symétrique
C     c.à.d.   U, V,Wø,Ny,Nyx,My,Ry,W,Wøø,Wøøø,Uø,Vø,Vøø   ! (-1 Antimétrique

C*******************************************************************************
C     SUBROUTINE SENS (Optimisation)
C     ===============================
C     La subr. calcule les dérivées des 13 fonctions (U,V,W,Ny, ... ,Wøøø)
C     en 10 pts selon l'axe OY, pour les 8 cas de base (DISA, DISB, etc.) 
c     et pour la charge extérieure (DISH) 
C
C     Modifier: le 19-6-95 (Traverses: Subr. ALI2)             Créer : 3-3-94
C     		le 20-8-95 
C     		le 1-12-95 (plusieurs cas de charge, NSOL)
C     		le 27-3-96 (sens2 , termes en y2 et y3) 
C
C         Last chanhe : 15-2-2001
C*******************************************************************************
C  YPT = Coord. selon Oy des pts de calcul.
C  IPT = Nbre de pts de calcul (max 10 pts )
C  SENS(13,10,9) = SENS(J,I,K) = vecteurs résultats
C           avec J relatif aux variables (u,v,w,Ny, ...);  J=1,13
C           avec I relatif aux points de calcul ;          I=1,IPT
C           avec K relatif aux variables de conception XI; K=1,NBRXI.
C  DHYP(I,J,K) = Dérivée des vecteurs solutions de forces Xo et Zo (traverses)
C            si K  la variable de conception (1 à 9)
C               J  les 9 cas de charge (DISH, DISC en y=0, en y=Yo, DISB, ...)
C               I  les dérivées de Xo pour les MT traverses + idem avec Zo (1 à MS)
C  Ordre des var. dans DVARX et SENSX :
C    U=1 V=2 Wø=3 Ny=4 Nyx=5 My=6 Ry=7 W=8 Wøø=9 Wøøø=10 Uø=11 Vø=12 et Vøø=13
C    puis les (Xo et Zo) des traverses de (14 et 15)  à (32 et 33)

	
      PI1=PI/180.                                     ! PI/180
      PI2=2.*PI                                       ! 2 PI
      PHIL1=PHIL*PI1                                  ! Yo PI/180
      TETA1=TETA*PI1
      	
      DO 10 I=1,IPT                                                    
C       Soit Y1 la coordonnée du point de calcul par rapport à Y=O 
        Y1=YPT(I)*PI1                               !     Y PI/180
        Y1Q=Y1 * Q                                  !   Q Y PI/180
        Y11=PI2-Y1                                  !    (2PI-Y)
        Y11Q=Y11*Q                                  !   Q(2PI-Y)
C       Soit Y2 la coordonnée du point de calcul  par rapport à Y=Yo (PHIL) 
        Y2=PHIL1-Y1                                 !     (Yo-Y)
        Y2Q=Y2*Q                                    !    Q(Yo-Y)
        Y22=PI2-Y2                                  !     (2 PI-(Yo-Y))
        Y22Q=Y22*Q                                  !    Q(2 PI-(Yo-Y))

c      WRITE(66,*)' Y1 =',Y1, ' Y1Q =',Y1Q 
c      WRITE(66,*)' Y2 =',Y2, ' Y2Q =',Y2Q 
c      WRITE(66,*)' Y11=',Y11,' Y11Q=',Y11Q 
c      WRITE(66,*)' Y22=',Y22,' Y22Q=',Y22Q 

      TETAT=TETA1+Y1
      IF(PHIL.GE.1.E-05) THEN
      	SINT=DSIN(TETAT)
      	COST=DCOS(TETAT)
      ELSE
        IF(DABS(TETA).EQ.180.) THEN
                COST=-DCOS(Y1)
                SINT=-DSIN(Y1)
        ENDIF
        IF(DABS(TETA).EQ.90.) THEN
                II=1
                IF(TETA.EQ.-90.) II=-1
                COST=-DSIN(Y1)*II
                SINT=+DCOS(Y1)*II
        ENDIF
        COST=DCOS(TETAT)
        SINT=DSIN(TETAT)
      ENDIF
      
      DO 11 L=1,M                                                    

      AL1=ARGQ(2*L-1)*Y1                          ! Y
      BE1=ARGQ(2*L)  *Y1                          !
      AL2=ARGQ(2*L-1)*Y2                          ! Yo-Y
      BE2=ARGQ(2*L)  *Y2                          !
      AL3=ARGQ(2*L-1)*Y11                         ! 2 PI - Y
      BE3=ARGQ(2*L)  *Y11                         !
      AL4=ARGQ(2*L-1)*Y22                         ! 2 PI-(Yo-Y)
      BE4=ARGQ(2*L)  *Y22                         !
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
      
      IF(MT.GE.1) THEN
      CALL ALI2(ALIX,ALIZ,DISC,DISB,ABTR,MT,Y1,Q,PI1,PI2,L,ID,ID2,
     *          ARGQ,DALIX,DALIZ,DVARB,DVARC,DARG,NBRXI,NXI,PHIL)	
      ENDIF
	
      CALL DEF1(SIN1,COS1,SIN3,COS3,DISC,DEFC, I,L,M,ID,1,  ! DISC (Y=0)
     *          MT,ALIX,ALIZ)                               !
      CALL DEF1(SIN2,COS2,SIN4,COS4,DISC,DEFCC,I,L,M,ID,2,  ! DISC (Y=Yo)
     *          MT,ALIX,ALIZ)                               !
      CALL DEF1(SIN1,COS1,SIN3,COS3,DISD,DEFD, I,L,M,ID,3,  ! DISD (Y=0)
     *          MT,ALIX,ALIZ)                               !
      CALL DEF1(SIN2,COS2,SIN4,COS4,DISD,DEFDD,I,L,M,ID,4,  ! DISD (Y=Yo)
     *          MT,ALIX,ALIZ)                               !
      CALL DEF1(SIN1,COS1,SIN3,COS3,DISA,DEFA, I,L,M,ID,5,  ! DISA (Y=0)
     *          MT,ALIX,ALIZ)                               !
      CALL DEF1(SIN2,COS2,SIN4,COS4,DISA,DEFAA,I,L,M,ID,6,  ! DISA (Y=Yo)
     *          MT,ALIX,ALIZ)                               !
      CALL DEF1(SIN1,COS1,SIN3,COS3,DISB,DEFB, I,L,M,ID,7,  ! DISB (Y=0)
     *          MT,ALIX,ALIZ)                               !
      CALL DEF1(SIN2,COS2,SIN4,COS4,DISB,DEFBB,I,L,M,ID,8,  ! DISB (Y=Yo)
     *          MT,ALIX,ALIZ)                               !

      IF(NBRXI.NE.0) THEN
      CALL SENS1(Y1Q,Y11Q,SIN1,COS1,SIN3,COS3,DISC,DVARC,SENSC, 1, ! DISC (Y=0)
     *           I,L,M,ID,NXI,NBRXI,MT,ALIX,ALIZ,DALIX,DALIZ,DHYP) !
      CALL SENS1(Y2Q,Y22Q,SIN2,COS2,SIN4,COS4,DISC,DVARC,SENSCC,2, ! DISC (Y=Yo)
     *           I,L,M,ID,NXI,NBRXI,MT,ALIX,ALIZ,DALIX,DALIZ,DHYP) !
      CALL SENS1(Y1Q,Y11Q,SIN1,COS1,SIN3,COS3,DISD,DVARD,SENSD, 3, ! DISD (Y=0)
     *           I,L,M,ID,NXI,NBRXI,MT,ALIX,ALIZ,DALIX,DALIZ,DHYP) !
      CALL SENS1(Y2Q,Y22Q,SIN2,COS2,SIN4,COS4,DISD,DVARD,SENSDD,4, ! DISD (Y=Yo)
     *           I,L,M,ID,NXI,NBRXI,MT,ALIX,ALIZ,DALIX,DALIZ,DHYP) !
      CALL SENS1(Y1Q,Y11Q,SIN1,COS1,SIN3,COS3,DISA,DVARA,SENSA, 5, ! DISA (Y=0)
     *           I,L,M,ID,NXI,NBRXI,MT,ALIX,ALIZ,DALIX,DALIZ,DHYP) !
      CALL SENS1(Y2Q,Y22Q,SIN2,COS2,SIN4,COS4,DISA,DVARA,SENSAA,6, ! DISA (Y=Yo)
     *           I,L,M,ID,NXI,NBRXI,MT,ALIX,ALIZ,DALIX,DALIZ,DHYP) !
      CALL SENS1(Y1Q,Y11Q,SIN1,COS1,SIN3,COS3,DISB,DVARB,SENSB, 7, ! DISB (Y=0)
     *           I,L,M,ID,NXI,NBRXI,MT,ALIX,ALIZ,DALIX,DALIZ,DHYP) !
      CALL SENS1(Y2Q,Y22Q,SIN2,COS2,SIN4,COS4,DISB,DVARB,SENSBB,8, ! DISB (Y=Yo)
     *           I,L,M,ID,NXI,NBRXI,MT,ALIX,ALIZ,DALIX,DALIZ,DHYP) !

	IS1=1
        DO 17 IS=1,NSOL
	IF(IS.GT.1) IS1=8+IS
        CALL SENS2(Y1,Y1Q,Y11Q,Y2Q,Y22Q,SIN1,COS1,SIN2,COS2,SIN3,COS3, !  DISH
     *  SIN4,COS4,COST,SINT,DISH(1,IS),DVARH(1,1,1,IS),SENSH(1,1,1,IS),
     *  I,L,M,ID,NXI,NBRXI,MT,ALIX,ALIZ,DALIX,DALIZ,DHYP,IS1)
  17    CONTINUE

      ELSE                                            ! Pour annuler
      CALL ANNULD(SENSH,13*9*IPTmax*(8+NSOL))         ! SENSH et
      ENDIF                                           ! SENSA à SENSDD
                                                      !
   11 CONTINUE

      IF((IMPR.EQ.1).AND.(MT.GE.1))THEN
      WRITE(66,*) 'IPT',I
      WRITE(66,*) '======='
      WRITE(66,*) 'ALIX ET ALIZ'
      DO 209 IK=1,13
      WRITE(66,567) 'ALIX(J,',IK,')',(ALIX(J,IK),J=1,MT)
 209  WRITE(66,567) 'ALIZ(J,',IK,')',(ALIZ(J,IK),J=1,MT)
      DO 210 IVAR=1,NBRXI
      K=NXI(IVAR)
      WRITE(66,*) 'DALIX ET DALIZ (POUR XI=1)'
      DO 210 IK=1,13
      WRITE(66,568) 'DALIX(J,',IK,K,')=',(DALIX(J,IK,K),J=1,MT)
 210  WRITE(66,568) 'DALIZ(J,',IK,K,')=',(DALIZ(J,IK,K),J=1,MT)
      ENDIF
 567  FORMAT(A8,I2,A3,4(5E14.7/))
 568  FORMAT(A8,I3,I3,A3,4(5E14.7/))

   10 CONTINUE

C     IMPRESSIONS
C     ------------

      IF(IMPR.EQ.1) THEN
      WRITE(66,16)
      DO 14 I=1,IPT
      WRITE(66,*)
      DO 14 J=1,13
      WRITE(66,7)I,J,DEFA(J,I),DEFB(J,I),DEFC(J,I),DEFD(J,I),
     *                DEFAA(J,I),DEFBB(J,I),DEFCC(J,I),DEFDD(J,I)
   14 CONTINUE

      IF((NBRXI.GE.1)) THEN
      WRITE(66,8) NSOL
      DO 5 I=1,IPT
      Y1=YPT(I)*PI1*Q
      WRITE(66,*)'                      Pt nø= ',I,' YPT(I) =',Y1,' m'
      DO 5 IVAR=1,NBRXI
      K=NXI(IVAR)
      DO 5 J=1,13
      WRITE(66,6) J,K,SENSA(J,I,K) ,SENSB (J,I,K),SENSC (J,I,K),
     *                SENSD(J,I,K) ,SENSAA(J,I,K),SENSBB(J,I,K),
     *                SENSCC(J,I,K),SENSDD(J,I,K),
     *                (SENSH (J,I,K,IS),IS=1,NSOL)
    5 CONTINUE 
      ENDIF
      ENDIF
	
	
C     FORMATS
C     ------------
    6 FORMAT(1X,I2,4X,I2,1X,4(1X,E13.6)/10X,4(1X,E13.6)/10X,5(1X,E13.6))
    7 FORMAT(1X,I2,4X,I2,1X,4(1X,E13.6)/10X,4(1X,E13.6))
    8 FORMAT(/,'La valeur de la dérivée des fct. (U,V,Wø,Ny,Nyx,My,',
     *'Ry,W,Wøø,Wøøø,Uø,Vø,Vøø) aux points sont :'/
     *'Fct  Var.XI    SENSA         SENSB         SENSC         SENSD' /
     *'               SENSAA        SENSBB        SENSCC        SENSDD'/
     *'               SENSH (cas de charge nø1 à ',I2/)
   16 FORMAT(/'La valeur des fct. (U,V,Wø,Ny,Nyx,My,Ry,W,Wøø,Wøøø,Uø,',
     *'Vø,Vøø) aux points sont :'/
     *'Pt.    Fct      DEFA          DEFB         DEFC          DEFD '/
     *'                DEFAA         DEFBB        DEFCC         DEFDD') 
      RETURN
      END



C***********************************************************************
C***********************************************************************
C***********************************************************************
C
      SUBROUTINE ALI2(ALIX,ALIZ,DISC,DISB,ABTR,MT,Y1,Q,PI1,PI2,L,
     *    ID,ID2,ARGQ,DALIX,DALIZ,DVARB,DVARC,DARG,NBRXI,NXI,PHIL)

      IMPLICIT REAL *8(A-H,O-Z)
      DIMENSION DISC(720),DISB(720),ALIX(10,13),ALIZ(10,13),ABTR(10),
     *          ID(13),ID2(13),ARGQ(8),DALIX(10,13,9),DALIZ(10,13,9),
     *          DVARB(33,9,16),DVARC(33,9,16),DARG(8,9),NXI(9)

C     SUBROUTINE ALI2   (effet des traverses - calcul des sensibilités)
C     ------------------
C     LA SUBROUTINE CALCULE les 13 GRANDEURS UTILES (ALI)
C     (u,v,w,wø,My,Ny,Ry,Nxy + Wøø,Wøøø,Uø,Vø et Vøø)
C     SOUS L'ACTION DES EFFETS CONCENTRES unitaires
C     de type XODY ET ZODY agissant au NIVEAU DES TRAVERSES. 
C
C     Ces 13 grandeurs (u,v,w,..) sont calculées en 1 seul point Y1 choisis
C     pour le calcul des sensibilités (voir boucle "DO 10" ds Subr SENS).
C     Soit ALIX(MS,13) val. des 13 grandeurs pour Xo=1000 COS(lamb*X),
C          ALIZ(MS,13) val. des 13 grandeurs pour Zo=1000 SIN(lamb*X),
C                    avec Xo et Zo agissant au droit de la traverse I=1,MT.
C     Soit DALIX(MS,13,IVAR) dérivées de ALIX pour IVAR=1,9
C          DALIZ(MS,13,IVAR) dérivées de ALIZ pour IVAR=1,9
C                                                                       
C     Variables:
C	L =1 à M (nbre de racine à l'Equ. différentielle) 
C	Y1=Y *PI/180

      L2=2*L
      L1=L2-1

c      WRITE(66,*)'L = ',L
c      WRITE(66,*)'***********'
c      WRITE(66,*)'ALIX(1,J) ',(ALIX(1,J),J=1,MT)
c      WRITE(66,*)'ALIZ(1,J) ',(ALIZ(1,J),J=1,MT)
c      WRITE(66,*)'ABTR', ABTR
c      WRITE(66,*)'ARGQ ', ARGQ
c      WRITE(66,*)'Y1 ', Y1

 
      DO 1 I=1,MT
      XMT=ABTR(I)*PI1
      DY1=DABS(XMT-Y1)                                     ! IY-ABTRI
      IF(((DY1/PHIL).LT.(0.000001)).OR.(XMT.LT.Y1)) THEN   ! si YòABTR
      SIGN=1.                                              !
      ELSE                                                 !
      SIGN=-1.
      ENDIF
      DY1Q=Q*DY1
      
c      WRITE(66,*)'TRAVERSE Nø',I
c      WRITE(66,*)'---------------'

      DY11=PI2-DY1                                 ! 2PI-(I Y-ABTR I)
      DY11Q=Q*DY11                                 !
      AL1=ARGQ(2*L-1)*DY1                          !
      BE1=ARGQ(2*L)  *DY1                          !
      AL3=ARGQ(2*L-1)*DY11                                  	
      BE3=ARGQ(2*L)  *DY11                              			
      AL1=EXPO(AL1)
      AL3=EXPO(AL3)
      COS1=AL1*DCOS(BE1)
      SIN1=AL1*DSIN(BE1)
      COS3=AL3*DCOS(BE3)
      SIN3=AL3*DSIN(BE3)

c      WRITE(66,*)'DY1 ', DY1,'; DY11 ', DY11
c      WRITE(66,*)'DY1Q ', DY1Q,'; DY11Q ', DY11Q
c      WRITE(66,*)'COS1 ', COS1,'; SIN1 ', SIN1
c      WRITE(66,*)'COS3 ', COS3,'; SIN3 ', SIN3

      JJ=4*(L-1)

      DO 2 J=1,13
c      DO 2 J=11,11

c      WRITE(66,*)'FCT Nø',J
c      WRITE(66,*)'ID(J) ', ID(J),'ID2(J) ', ID2(J)

      KH=JJ+(ID(J)-1)*16
      SIGN2=SIGN                            !
      IF(ID2(J).EQ.1) SIGN2=1.              ! Si fct symétrique (ID2=1)
      IF(L.EQ.1) ALIX(I,J)=0.               !
      IF(L.EQ.1) ALIZ(I,J)=0.               !

c      WRITE(66,*)'SIGN2 ', SIGN2
c      WRITE(66,*)'ALIX(I,j) ', ALIX(I,J),' ALIZ(I,j) ', ALIZ(I,J)
c      WRITE(66,*)'KH ', KH,' DISB(KH) ',DISB(KH+1),DISB(KH+2)
c      WRITE(66,*)'KH ', KH,' DISC(KH) ',DISC(KH+1),DISC(KH+2)

c      RSF=SIGN2* (DISB(KH+1)*COS1+DISB(KH+2)*SIN1)  	
c      WRITE(66,*)'SIGN2* (DISB(KH+1)*COS1+DISB(KH+2)*SIN1) ',RSF
c      RSF=SIGN2* (DISC(KH+1)*COS1+DISC(KH+2)*SIN1)  	
c      WRITE(66,*)'SIGN2* (DISC(KH+1)*COS1+DISC(KH+2)*SIN1) ',RSF
      
      ALIX(I,J)=ALIX(I,J)+ SIGN2* (DISB(KH+1)*COS1+DISB(KH+2)*SIN1+
     *                             DISB(KH+3)*COS3+DISB(KH+4)*SIN3 )
      ALIZ(I,J)=ALIZ(I,J)+ SIGN2* (DISC(KH+1)*COS1+DISC(KH+2)*SIN1+
     *                             DISC(KH+3)*COS3+DISC(KH+4)*SIN3 )

c      WRITE(66,*)'ALIX(I,j) ', ALIX(I,J),' ALIZ(I,j) ', ALIZ(I,J)

      DO 3 IVAR=1,NBRXI
      K=NXI(IVAR)

      CALCX =
     * - DY1Q * ( COS1* (DARG(L1,K)*DISB(KH+1)-DARG(L2,K)*DISB(KH+2) )
     *           +SIN1* (DARG(L1,K)*DISB(KH+2)+DARG(L2,K)*DISB(KH+1) ) )
     * - DY11Q* ( COS3* (DARG(L1,K)*DISB(KH+3)-DARG(L2,K)*DISB(KH+4) )
     *           +SIN3* (DARG(L1,K)*DISB(KH+4)+DARG(L2,K)*DISB(KH+3) ) )
     * + COS1 * DVARB(J,K,JJ+1)  + SIN1 * DVARB(J,K,JJ+2)
     * + COS3 * DVARB(J,K,JJ+3)  + SIN3 * DVARB(J,K,JJ+4)
      CALCZ =
     * - DY1Q * ( COS1* (DARG(L1,K)*DISC(KH+1)-DARG(L2,K)*DISC(KH+2) )
     *           +SIN1* (DARG(L1,K)*DISC(KH+2)+DARG(L2,K)*DISC(KH+1) ) )
     * - DY11Q* ( COS3* (DARG(L1,K)*DISC(KH+3)-DARG(L2,K)*DISC(KH+4) )
     *           +SIN3* (DARG(L1,K)*DISC(KH+4)+DARG(L2,K)*DISC(KH+3) ) )
     * + COS1 * DVARC(J,K,JJ+1)  + SIN1 * DVARC(J,K,JJ+2)
     * + COS3 * DVARC(J,K,JJ+3)  + SIN3 * DVARC(J,K,JJ+4)

      IF(L.EQ.1) DALIX(I,J,K)=0.
      IF(L.EQ.1) DALIZ(I,J,K)=0.
      DALIX(I,J,K)=DALIX(I,J,K) + SIGN2 * CALCX
      DALIZ(I,J,K)=DALIZ(I,J,K) + SIGN2 * CALCZ

c      WRITE(66,*)'DALIX(I,J,K)',DALIX(I,J,K)
c      WRITE(66,*)'DALIZ(I,J,K)',DALIZ(I,J,K)

    3 CONTINUE

    2 CONTINUE

    1 CONTINUE

      RETURN
      END

C***********************************************************************
C***********************************************************************
C     Subroutine pour les 4 cas de charge de base (DISA, DISB, etc.)
C       - calcul des déplacements crées par DISA, DISB, ..
C     **************************************************************
      SUBROUTINE DEF1(SIN1,COS1,SIN3,COS3,DISX,DEFX,IPT,L,M,ID,ICAS,
     *                MT,ALIX,ALIZ)

      IMPLICIT REAL *8(A-H,O-Z)
      DIMENSION DISX(720),DEFX(13,IPTmax),ID(13),ALIX(10,13),ALIZ(10,13)
      COMMON/HYP/HYP(360)
      COMMON/PY/PI
      COMMON/DIM1/Nmax,NEmax,NVmax,
     *            M1max,M1Tmax,M2max,M2Tmax,Mmax,NGmax,ISmax,IPTmax ! dimension max des vecteurs

c      WRITE(66,*)'Dans DEF1 :  ICAS=',ICAS,' IPT=',IPT,' M=',L

      JJ=(L-1)*4
      
c      WRITE(66,*)'ICAS ',ICAS, '  PT Nø ',IPT
c      WRITE(66,*)'==========='

      DO 4 J=1,13
      IJ=(ID(J)-1)*16 + JJ
      IF(L.EQ.1) DEFX(J,IPT)=0.

c      WRITE(66,*)'FCT ',J
c      WRITE(66,*)'-----------'
c      WRITE(66,*)'DEFX(',J,',IPT)=',DEFX(J,IPT)

      DEFX(J,IPT)=DEFX(J,IPT)
     *          + COS1 * DISX(IJ+1)  + SIN1 * DISX(IJ+2)
     *          + COS3 * DISX(IJ+3)  + SIN3 * DISX(IJ+4)


c      WRITE(66,*)'DEFX(',J,',IPT)=',DEFX(J,IPT)

      IF(L.NE.M) GOTO 5
C     =================
C     Fct symétrique (GOTO 32) et fct Antimétrique (GOTO 31)
      GOTO(32,31,31,32,31,32,31,32,32,31,31,32,31),J
   31 IF((ICAS.EQ.2).OR.(ICAS.EQ.8)) DEFX(J,IPT)=-DEFX(J,IPT)
C     Si charge sym (DISC et DISB)
      GOTO 30
   32 IF((ICAS.EQ.4).OR.(ICAS.EQ.6)) DEFX(J,IPT)=-DEFX(J,IPT)
C     Si charge Antimétrique (DISA et DISA)      
   30 CONTINUE

c      WRITE(66,*)' *** DEFX(',J,',IPT)=',DEFX(J,IPT)

      IF(MT.GE.1) THEN
C     ------------------
      DO 1 K=1,MT
      JMX=K+ICAS*20
      JMZ=JMX+MT
      DEFX(J,IPT)=DEFX(J,IPT)
     *   +( ALIX(K,J) * HYP(JMX)  + ALIZ(K,J) * HYP(JMZ) )/10000.

c      WRITE(66,*)'TRAVERSE ', K
c      WRITE(66,*)' ALIX(K,J)',ALIX(K,J),' ALIZ(K,J)',ALIZ(K,J)
c      WRITE(66,*)' HYP(JMX) ',HYP(JMX), ' HYP(JMZ) ',HYP(JMZ)      

    1 CONTINUE

c      WRITE(66,*)'DEFX(',J,',IPT)=',DEFX(J,IPT)

      ENDIF

    5 CONTINUE

    4 CONTINUE
      RETURN
      END


C     **************************************************************
C     **************************************************************
C     Subroutine pour les 4 cas de charge de base (DISA, DISB, etc.) 
C       - calcul des dérivées des déplacements crées par DISA, DISB, ..
C     **************************************************************
      SUBROUTINE SENS1(Y1Q,Y11Q,SIN1,COS1,SIN3,COS3,DISX,DVAR,
     *  SENSX,ICAS,IPT,L,M,ID,NXI,NBRXI,MT,ALIX,ALIZ,DALIX,DALIZ,DHYP)

      IMPLICIT REAL *8(A-H,O-Z)
      DIMENSION DISX(720),DVAR(33,9,16),SENSX(13,IPTmax,9),ID(13),
     *   NXI(9),ALIX(10,13),ALIZ(10,13),DALIX(10,13,9),DALIZ(10,13,9),
     *   DHYP(20,8+ISmax,9)
 
      COMMON/PY/PI
      COMMON/HYP/HYP(360)
      COMMON/OPTI3/ BID1(54),BID2(20),DARG(8,9)
      COMMON/DIM1/Nmax,NEmax,NVmax,
     *            M1max,M1Tmax,M2max,M2Tmax,Mmax,NGmax,ISmax,IPTmax ! dimension max des vecteurs

      L2=2*L
      L1=L2-1
      JJ=(L-1)*4
      ICAS1=ICAS+1
      
c      WRITE(66,*) 'ICAS=',ICAS

      DO 4 J=1,13
      IJ=(ID(J)-1)*16 + JJ

c      WRITE(66,*) 'fct=',J

      DO 5 IVAR=1,NBRXI
      K=NXI(IVAR)

c      WRITE(66,*) 'IVAR=',K

      IF(L.EQ.1) SENSX(J,IPT,K)=0.

      SENSX(J,IPT,K)=SENSX(J,IPT,K)
     * - Y1Q * ( COS1* (DARG(L1,K)*DISX(IJ+1)-DARG(L2,K)*DISX(IJ+2) )
     *          +SIN1* (DARG(L1,K)*DISX(IJ+2)+DARG(L2,K)*DISX(IJ+1) ) )
     * - Y11Q* ( COS3* (DARG(L1,K)*DISX(IJ+3)-DARG(L2,K)*DISX(IJ+4) )
     *          +SIN3* (DARG(L1,K)*DISX(IJ+4)+DARG(L2,K)*DISX(IJ+3) ) )
     * + COS1 * DVAR(J,K,JJ+1)  + SIN1 * DVAR(J,K,JJ+2)
     * + COS3 * DVAR(J,K,JJ+3)  + SIN3 * DVAR(J,K,JJ+4)

      IF(L.NE.M) GOTO 6
C     ================= 
C     Fct symétrique (GOTO 32) et fct Antimétrique (GOTO 31)
      GOTO(32,31,31,32,31,32,31,32,32,31,31,32,31),J
   31 IF((ICAS.EQ.2).OR.(ICAS.EQ.8)) SENSX(J,IPT,K)=-SENSX(J,IPT,K)
C     Si charge sym (DISC et DISB)
      GOTO 30
   32 IF((ICAS.EQ.4).OR.(ICAS.EQ.6)) SENSX(J,IPT,K)=-SENSX(J,IPT,K)
C     Si charge Antimétrique (DISA et DISD)
   30 CONTINUE

c      WRITE(66,*)'SENSX(J,IPT,K)=',SENSX(J,IPT,K)

      IF(MT.GE.1) THEN
C     ------------------
      DO 1 I=1,MT
      JMX=I+ICAS*20
      JMZ=JMX+MT

      SENSX(J,IPT,K)=SENSX(J,IPT,K) + (1./10000.) *
     *  (  ALIX(I,J)*DHYP(I,ICAS1,K)+ALIZ(I,J)*DHYP(I+MT,ICAS1,K)
     *   + HYP(JMX) *DALIX(I,J,K)   +HYP(JMZ) *DALIZ(I,J,K)        )

c      WRITE(66,*)'Trav.=',I
c      WRITE(66,*)' ALIX(I,J)',ALIX(I,J),' ALIZ(I,J)',ALIZ(I,J)
c      WRITE(66,*)' DALIX=',DALIX(I,J,K),' DALIZ=',DALIZ(I,J,K)
c      WRITE(66,*)' HYP(JMX) ',HYP(JMX), ' HYP(JMZ) ',HYP(JMZ)
c      WRITE(66,*)' DHYP(I,ICAS1,K)=',DHYP(I,ICAS1,K),
c     *           ' DHYP(I+MT,ICAS1,K)=',DHYP(I+MT,ICAS1,K)
c      WRITE(66,*)'SENSX(J,IPT,K)=',SENSX(J,IPT,K)

    1 CONTINUE
      ENDIF

    6  CONTINUE


    5  CONTINUE
    4  CONTINUE
      RETURN
      END


C     **************************************************************
C     **************************************************************
C     Subroutine pour la charge extérieure (DISH)
C     ***************************************************************
      SUBROUTINE SENS2(Y1,Y1Q,Y11Q,Y2Q,Y22Q,SIN1,COS1,SIN2,COS2,SIN3,
     * COS3,SIN4,COS4,COST,SINT,DISH,DVARH,SENSH,IPT,L,M,ID,NXI,NBRXI,
     * MT,ALIX,ALIZ,DALIX,DALIZ,DHYP,IS1)

      IMPLICIT REAL *8(A-H,O-Z)
      DIMENSION DISH(1710),DVARH(33,9,38),SENSH(13,IPTmax,9),ID(13),
     *   NXI(9),ALIX(10,13),ALIZ(10,13),DALIX(10,13,9),DALIZ(10,13,9),
     *   DHYP(20,8+ISmax,9)

      COMMON/PY/PI
      COMMON/HYP/HYP(360)
      COMMON/OPTI3/ BID1(54),BID2(20),DARG(8,9)
      COMMON/DIM1/Nmax,NEmax,NVmax,
     *            M1max,M1Tmax,M2max,M2Tmax,Mmax,NGmax,ISmax,IPTmax ! dimension max des vecteurs

      L2=2*L
      L1=L2-1
      JJ=(L-1)*8 + 6

      DO 4 J=1,13
      IJ=(ID(J)-1)*38 + JJ

      DO 5 IVAR=1,NBRXI
      K=NXI(IVAR)

c       Write(66,*) 'DANS SENS  et DVARH pour variable ',J
c       Write(66,*) (DVARH(J,K,I),I=1,6)
c       WRITE(66,*) (DVARH(J,K,I),I=7,14)
c       Write(66,*) 'DISH pour variable 1'
c       Write(66,*) (DISH(I),I=1,6)
c       WRITE(66,*) (DISH(I),I=7,14)
c      	Write(66,*) 'DISH pour variable 2'
c      	Write(66,*) (DISH(I+38),I=1,6)
c	WRITE(66,*) (DISH(I+38),I=7,14)
c      	Write(66,*) 'DISH pour variable 3'
c      	Write(66,*) (DISH(I+76),I=1,6)
c	WRITE(66,*) (DISH(I+76),I=7,14)

c	WRITE(66,*) 'Y1Q,Y2Q,Y11Q,Y22Q=',Y1Q,Y2Q,Y11Q,Y22Q
c	WRITE(66,*) 'COS1,SIN1,COS2,SIN2=',COS1,SIN1,COS2,SIN2

      
      IF(L.EQ.1) SENSH(J,IPT,K)=DVARH(J,K,1)+DVARH(J,K,2)*Y1
     *			       +DVARH(J,K,3)*Y1*Y1+DVARH(J,K,4)*(Y1**3)
     *                         +DVARH(J,K,5)*COST+DVARH(J,K,6)*SINT

c      WRITE(66,*)'SEN =',SENSH(J,IPT,K)

      SENSH(J,IPT,K)=SENSH(J,IPT,K)
     * - Y1Q * ( COS1* (DARG(L1,K)*DISH(IJ+1)-DARG(L2,K)*DISH(IJ+2) )
     *          +SIN1* (DARG(L2,K)*DISH(IJ+1)+DARG(L1,K)*DISH(IJ+2) ) )
     * - Y2Q * ( COS2* (DARG(L1,K)*DISH(IJ+3)-DARG(L2,K)*DISH(IJ+4) )
     *          +SIN2* (DARG(L2,K)*DISH(IJ+3)+DARG(L1,K)*DISH(IJ+4) ) )
     * - Y11Q* ( COS3* (DARG(L1,K)*DISH(IJ+5)-DARG(L2,K)*DISH(IJ+6) )
     *          +SIN3* (DARG(L2,K)*DISH(IJ+5)+DARG(L1,K)*DISH(IJ+6) ) )
     * - Y22Q* ( COS4* (DARG(L1,K)*DISH(IJ+7)-DARG(L2,K)*DISH(IJ+8) )
     *          +SIN4* (DARG(L2,K)*DISH(IJ+7)+DARG(L1,K)*DISH(IJ+8) ) )
     * + COS1 * DVARH(J,K,JJ+1)  + SIN1 * DVARH(J,K,JJ+2)
     * + COS2 * DVARH(J,K,JJ+3)  + SIN2 * DVARH(J,K,JJ+4)
     * + COS3 * DVARH(J,K,JJ+5)  + SIN3 * DVARH(J,K,JJ+6)
     * + COS4 * DVARH(J,K,JJ+7)  + SIN4 * DVARH(J,K,JJ+8)

c      WRITE(66,*)'SEN(new) =',SENSH(J,IPT,K)

      IF((L.EQ.M).AND.(MT.GE.1)) THEN
C     -------------------------------
      DO 1 I=1,MT
      I1=I+(IS1-1)*20
      SENSH(J,IPT,K)=SENSH(J,IPT,K) +  (1./10000.) *
     *   ( ALIX(I,J)*DHYP(I,IS1,K) + ALIZ(I,J) *DHYP(I+MT,IS1,K)
     *   + HYP(I1)  *DALIX(I,J,K)  + HYP(I1+MT)*DALIZ(I,J,K)  )

c      WRITE(66,*)' Trav.=',I
c      WRITE(66,*)' ALIX(I,J)',ALIX(I,J),' ALIZ(I,J)',ALIZ(I,J)
c      WRITE(66,*)' DALIX=',DALIX(I,J,K),' DALIZ=',DALIZ(I,J,K)
c      WRITE(66,*)' HYP(I1) ',HYP(I1), ' HYP(I1+MT) ',HYP(I1+MT)
c      WRITE(66,*)' DHYP(I,IS1,K)=',DHYP(I,IS1,K),
c     *           ' DHYP(I+MT,IS1,K)=',DHYP(I+MT,IS1,K)      
c      WRITE(66,*)'SENSH(J,IPT,K)=',SENSH(J,IPT,K)

    1 CONTINUE
      ENDIF

    5 CONTINUE
    4 CONTINUE
      RETURN
      END


C     **************************************************************
C     **************************************************************
C     **************************************************************