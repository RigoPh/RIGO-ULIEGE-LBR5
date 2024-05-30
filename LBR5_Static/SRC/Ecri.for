
      SUBROUTINE ECRI(DIS,NEL,EFF,EFFCOMB,CONC,NE,INDAIG,INDRAI,NS,Q, !avril2003
     *                IMPR,VMAXSIG,VMAXSIGC)                          !avril2003

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION DIS(5),EFF(9690),EFFCOMB(9690),CONC(750)     !avril2003
	CHARACTER *50 A(38),B(15),C(38),D(15)
      COMMON/ECR/U(51),MT
      COMMON/PY/PI
      COMMON/LANGUE/LANGUE
C
C***********************************************************************
C
C     SUBROUTINE ECRIT(FORME COMPLETE)
C     SUBROUTINE DE MISE EN FORME DES RESULTATS POUR ECRITURE SUR
C     UN LISTING.
C
C    Modif :13- 3-1995                           Créer : Thèse Ph. Rigo;
C            1-11-2000
c            4-12-2002  22 variables classées dans SOL2 (valeurs extrêmes)
C
C***********************************************************************
c             12345678901234567890123456789012345678901234567890
      DATA A/'V (m) - DEPLACEMENT TRANSVERSAL selon OY (Z=0)',		! 1
     *       'U (m) - DEPLACEMENT LONGITUDINAL selon OX (Z=0)',
     *       'W (m) - FLECHE, Deplacement selon OZ (Z=0)',			! 3
     *       'dW/dY - ROTATION autour de l axe OX ',
     *       'Nx (N/m) - Effort Normal OX (+ Participation Raid)',	! 5
     *       'Ny (N/m) - Effort Transv OY (+ Participation Cadr)',
     *       'Nxy (N/m) - Effort Cisaillement (+ Part renforts)',
     *       'Nyx (N/m) - Effort Cisaillement (+ Part renforts)',
     *       'Mx (N.m/m)- Moment autour de OY (+ Particip Raid)',		! 9
     *       'My (N.m/m)- Moment autour de OX (+ Particip Cadr)',
     *       'Mxy (N.m/m) - Moment de Torsion',
     *       'Myx (N.m/m) - Moment de Torsion',
     *       'Qx (N/m) - Reaction sur bord OY (avec Part. Raid)',		! 13
     *       'Qy (N/m) - Reaction sur bord OY (avec Part. Cadre)',
     *       'Ry (N/m) - Kirchoff Reaction sur OX (= Qy + ...)',
     *       'Rx (N/m) - Kirchoff Reaction sur OY (= Qx + ...)',		! 16
     *       'Sy (N/m2) - Contr. Trans. selon OX, Plaque (Z=0)',
     *       'Sx (N/m2) - Contr. Long.  selon OY, Plaque (Z=0)',
     *       'Txy (N/m2) - Contr. Cisaillement, Plaque (Z=0)',
     *       'Scomp (N/m2) - Contr. de Von Mises, Plaque(Z=0)',		! 20
     *       'Sy Cadre (JAS)- Contr. Jonct. AME-SEM, CADRE',		    ! 21
     *       'Sy Cadre (JAB)- Contr. Jonct. AME-BORDE, CADRE',		! 22
     *       'Tyz Cadre (JAS)- Tau (shear)Jonct AME-SEM,CADRE',		! 23
     *       'Tyz Cadre (JAB)- Tau (shear)Jonct AME-BORDE,CADRE',	    ! 24
     *       'Sc Cadre (JAS)- Von Mises, J. AME-SEM, CADRE',		    ! 25
     *       'Sc Cadre (JAB)- Von Mises, J. AME-BORDE,CADRE',		    ! 26
     *       'Sy Cadre (SEM)- Contrainte dans SEMELLE, CADRE',		! 27
     *       'Txy Cadre (SEM)- Tau (shear) dans SEMELLE CADRE',		! 28
     *       'Sc Cadre (SEM)- Von Mises, dans SEMELLE, CADRE',		! 29
     *       'Sx Raid (JAS)- Contr. Jonction AME-SEM, RAID',			! 30
     *       'Sx Raid (JAB)- Contr. Jonction AME-BORDE, RAID',		! 31
     *       'Sx Raid (SEM)- Contrainte dans SEMELLE, RAID',			! 32
     *       'Txz Raid (JAS)- Tau (shear), Jonct. AME-SEM',			! 33
     *       'Txz Raid (JAB)- Tau (shear), Jonct. AME-BORDE',			! 34
     *       'Tyx Raid (SEM)- Tau (shear) dans SEMELLE RAID',			! 35
     *       'Sc Raid (JAS)- Von Mises, Jonct AME-SEM,RAID',			! 36
     *       'Sc Raid (JAB)- Von Mises, Jonct AME-BORDE, RAID',		! 37
     *       'Sc Raid (SEM)- Von Mises, dans SEMELLE, RAID'/  		! 38
c             12345678901234567890123456789012345678901234567890

      DATA B/	'NX  CONCENTRE',
     *		'NXY CONCENTRE  = NYX CONCENTRE',
     *		'MX  CONCENTRE',
     *		'MXY CONCENTRE',
     *		'QX  CONCENTRE',
     *		'RX  CONCENTRE',
     *		'SIGMA X SEMELLE    TRAVERSE',
     *		'SIGMA X  JAS       TRAVERSE',
     *		'SIGMA X  JAB       TRAVERSE',
     *		'TAU YX MAX SEMELLE TRAVERSE',
     *		'TAU XZ AME JAS     TRAVERSE',
     *        'TAU XZ AME JAB     TRAVERSE',
     *		'SIGMA Von-Mises SEMELLE TRAVERSE',
     *		'SIGMA Von-Mises JAS  TRAVERSE',
     *		'SIGMA Von-Mises JAB  TRAVERSE'/

      DATA C/'V (m) - TRANSV. DISPL. along OY (Z=0)',					! 1
     *       'U (m) - LONG. DISPL. along OX (Z=0)',
     *       'W (m) - LATERAL DEFLECTION along OZ(Z=0)',				! 3
     *       'dW/dY - ROTATION around OX ',
     *       'Nx (N/m) - Long. Force along OX(with Stiff. Effect)',	! 5
     *       'Ny (N/m) - Transv Force along OY(with Frame Effect)',
     *       'Nxy (N/m) - Inplane Shear Force (with Memb. Effect)',
     *       'Nyx (N/m) - Inplane Shear Force (with Memb. Effect)',
     *       'Mx (N.m/m)- Moment around OY (with Stiff. Effect)',		! 9
     *       'My (N.m/m)- Moment around OX (with Frame Effect)',
     *       'Mxy (N.m/m) - Twisting Moment',
     *       'Myx (N.m/m) - Twisting Moment',
     *       'Qx (N/m) - Reaction along OY(with Stiff. Effect)',		! 13
     *       'Qy (N/m) - Reaction along OX(with Stiff. Effect)',
     *       'Ry (N/m) - Kirchoff Reaction along OX (= Qy + ...)',
     *       'Rx (N/m) - Kirchoff Reaction along OY (= Qx + ...)',	! 16
     *       'Sy (N/m2) - Transv. Stress in Plate (Z=0)',
     *       'Sx (N/m2) - Longit. Stress in Plate (Z=0)',
     *       'Txy (N/m2) - Inplane Shear Stress in Plate (Z=0)',
     *       'Scomp (N/m2) - Von-Mises in Plate (Z=0)',				! 20
     *       'Sy Frame (JWF)- Stress, Junct. Web-Flange, FRAME',		! 21
     *       'Sy Frame (JWP)- Stress, Junct. Web-Plate, FRAME',		! 22
     *       'Tyz Frame (JWF)- Shear Stress, Junct. Web-Flange',		! 23
     *       'Tyz Frame (JWP)- Shear Stress, Junct. Web-Plate',		! 24
     *       'Sc Frame (JWF)- Von-Mises at J. Web-Flange,FRAME',		! 25
     *       'Sc Frame (JWP)- Von-Mises at J. Web-Plate,FRAME',		! 26
     *       'Sy Frame (FLANGE)- Stress in Flange, FRAME',			! 27
     *       'Txy Frame (FLANGE)- Shear Stress in Flange FRAME',		! 28
     *       'Sc Frame (FLANGE)- Von-Mises in Flange, FRAME',			! 29
     *       'Sx Stiff (JWF)- Stress, Junct. Web-Flange, STIFF',		! 30
     *       'Sx Stiff (JWP)- Stress, Junct. Web-Plate, STIFF',		! 31
     *       'Sx Stiff (FLANGE)- Stress in Flange, STIFF',			! 32
     *       'Txz Stiff (JWF)- Shear Stress, Junct. Web-Flange',		! 33
     *       'Txz Stiff (JWP)- Shear Stress, Junct. Web-Plate',		! 34
     *       'Tyx Stiff (FLANGE)- Shear Stress in Flange STIFF',		! 35
     *       'Sc Stiff (JWF)- Von-Mises at J. Web-Flange,STIFF',		! 36
     *       'Sc Stiff (JWP)- Von-Mises at J. Web-Plate, STIFF',		! 37
     *       'Sc Stiff (FLANGE)- Von-Mises in Flange, STIFF'/			! 38
c             12345678901234567890123456789012345678901234567890
      DATA D/	'NX  , Local force',
     *		'NXY = NYX , Local force',
     *		'MX  , Local force',
     *		'MXY , Local force',
     *		'QX  , Local force',
     *		'RX  , Local force',
     *		'SIGMA X  FLANGE  , Stress in Girder',
     *		'SIGMA X  JWF     , Stress in Girder',
     *		'SIGMA X  JWP     , Stress in Girder',
     *		'TAU YX FLANGE  , Stress in Girder',
     *		'TAU XZ WEB JWF , Stress in Girder',
     *        'TAU XZ WEB JWP , Stress in Girder',
     *		'SIGMA COMP FLANGE , Von-Mises Stress in Girder',
     *		'SIGMA COMP WEB JWF, Von-Mises Stress in Girder',
     *		'SIGMA COMP WEB JWP, Von-Mises Stress in Girder'/
C
C     L = indice relatif à la variables étudiées (L=1 à 38) dans EFF
C     L=1       V               L=2     U                L=3    W
C       4       dW/dy             5     Nx                 6    Ny
C       7       Nxy               8     Nyx                9    Mx
C      10       My               11     Mxy               12    Myx
C      13       Qx               14     Qy                15    Ry
C      16       Rx

C      17       Sig Y bordage    18     Sig X bordage      19    Tau XY bordage
C      20       Sig Comp bordage

C      21     Sig Y JAS (cadre)  22   Sig Y JAB (cadre)    23    Tau JAS (cadre)
C      24     Tau JAB (cadre)    25   Sig COMP JAS (cadre) 26    Sig COMP (cadre)
C      27     Sig Y Sem (cadre)  28   Tau sem (cadre)      29    Sig COMP (cadre)

C      30    Sig X JAS (raid)    31   Sig X JAB (raid)     32    Sig X Sem (raid)
C      33    Tau   JAS (raid)    34   Tau JAB (raid)       35    Tau sem (raid)
C      36    Sig COMP JAS (raid) 37   Sig COMP JAB (raid)  38    Sig COMP (raid)

      IF(LANGUE.NE.1) THEN
        DO I=1,38
	    A(I)=C(I)
	  ENDDO
        DO I=1,15
	    B(I)=D(I)
	  ENDDO
	ENDIF
C
      KH=2*MT+31
      DO 1 I=1,KH
  1   U(I)=U(I)*Q*PI/180.
      WRITE(66,150) NEL
      L=0

      DO 157 KR=1,38
        L=L+1
        IF((IMPR.LE.-1).AND.(L.EQ.5)) L=17
        IF(L.EQ.39) GOTO 201
        IF((L.EQ.21).AND.(INDAIG.EQ.0)) L=30
        IF((L.EQ.30).AND.(INDRAI.EQ.0)) GOTO 201
        WRITE(66,50) A(L)
  50    FORMAT(/T15,A50/T15,50(1H+))
        WRITE(66,156) DIS
C
C     NS = 1 si PHIL positif (sens normal) ; NS=-1 si PHIL négatif
C
        IF(NS.EQ.1) THEN
          DO 163 I=1,KH
              JM=(L-1)*255+I
              IJ=JM+204
  163     WRITE(66,158) U(I),(EFF(K),K=JM,IJ,51)
        ELSE
          IF(L.GE.20) GOTO 2
          GOTO(3,2,2,3,2,2,3,3,2,2,3,3,2,3,3,2,2,2,3),L
   3          JM=(L-1)*255+1
              IJ=L*255
              DO 4 KK=JM,IJ
   4          EFF(KK)=-EFF(KK)
   2      DO 159 I=1,31
              JM=(L-1)*255+32-I
              IJ=JM+204
  159     WRITE(66,158) U(I),(EFF(K),K=JM,IJ,51)
          DO 161 I=32,KH
              JM=(L-1)*255+(KH+32-I)
              IJ=JM+204
              UTRAV=U(31)-U(KH+32-I)
  161     WRITE(66,158) UTRAV,(EFF(K),K=JM,IJ,51)
        ENDIF
      CALL EXTRE1(L,KH,EFF,EFFCOMB,A,VMAXSIG,VMAXSIGC)  !avril2003
  157 CONTINUE
  201 WRITE(304,*) '99'		!extension neto
C
      IF(MT.EQ.0) GOTO 200
      IF(LANGUE==1) THEN
        WRITE(66,301)
	ELSE
        WRITE(66,302)
	ENDIF
      DO 160 L=1,15
      IF((IMPR.LE.-1).AND.(L.LE.6)) GOTO 160
         WRITE(66,50) B(L)
         WRITE(66,156) DIS
         IF(NS.EQ.1) THEN
         DO 164 I=1,MT
            JM=(L-1)*50+I
            IJ=JM+40
 164     WRITE(66,158) U(2*I+30),(CONC(K),K=JM,IJ,10)
         ELSE
         DO 162 I=1,MT
            JM=(L-1)*50+MT-I+1
            IJ=JM+40
            UTRAV=U(31)-U(KH+1-2*I)
  162    WRITE(66,158) UTRAV,(CONC(K),K=JM,IJ,10)
         ENDIF
  160 CONTINUE
C
  150 FORMAT(T3,'PANNEAU-PANEL No ',I2,/T3,18(1H+))
  156 FORMAT(/10X,5(4X,'X=',F6.3,' m')/)
  158 FORMAT(' Y=',F6.3,'m',5(1X,E13.6))
  301 FORMAT(//T17,'CALCUL RELATIF AUX TRAVERSES DES EFFETS ',
     *             'CONCENTRES '/T15,53(1H+)//)
  302 FORMAT(//T17,'STRESSES IN THE LONGITUDINAL GIRDERS'/
     *         T15,35(1H+)//)

  200 RETURN
      END

c ------------------------------------------------------------
      SUBROUTINE EXTRE1(L,KH,EFF,EFFCOMB,A,VMAXSIG,VMAXSIGC) !avril2003
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION EFF(9690),EFFCOMB(9690)     !avril2003
	CHARACTER *50 A(38)
      COMMON/PY/PI
      COMMON/LANGUE/LANGUE
C
C***********************************************************************
C
C     SUBROUTINE EXTRE1
C     ++++++++++++++++++
C     Recherche les valeurs extrêmes (max et min)
C
C     Modif : Nov 2000                                    Créer : 13-3-95
C
C***********************************************************************

      VMIN=0.
	VMINC=0.  !avril2003
      VMAX=0.
	VMAXC=0.  !avril2003
      IXMIN=0
      IXMAX=0
      IYMIN=0
      IYMAX=0
      I1=(L-1)*255
	VMIN=EFF(I1+1) !avril2003   Sinon, on ne détecte pas le min s'il est positif
      VMAX=EFF(I1+1) !avril2003   Sinon, on ne détecte pas le max s'il est négatif
      
      DO 1 KX=1,5
      I2=I1+(KX-1)*51
        DO 2 KY=1,KH
        K1=I2+KY
        IF(VMIN.GE.EFF(K1)) THEN     !avril2003
                 VMIN=EFF(K1)
	           VMINC=EFFCOMB(K1)   !avril2003
                IXMIN=KX
                IYMIN=KY
        ENDIF
        IF(VMAX.LE.EFF(K1)) THEN     !avril2003 il vaut mieux mettre LE plutôt que LT
                VMAX=EFF(K1)
	          VMAXC=EFFCOMB(K1)    !avril2003
                IXMAX=KX
                IYMAX=KY
        ENDIF
   2    CONTINUE
   1  CONTINUE

      IF (L.EQ.18) THEN  !avril2003
	 IF (DABS(VMIN).GT.DABS(VMAX)) THEN  !avril2003
	   VMAXSIG  = VMIN                   !avril2003
	   VMAXSIGC = VMINC                  !avril2003
	 ELSE                                !avril2003
	   VMAXSIG  = VMAX                   !avril2003
	   VMAXSIGC = VMAXC                  !avril2003
	 ENDIF                               !avril2003
	ENDIF                                !avril2003

      IF(LANGUE==1) WRITE(66,102) VMIN,IXMIN,IYMIN,VMAX,IXMAX,IYMAX
      IF(LANGUE==2) WRITE(66,100) VMIN,IXMIN,IYMIN,VMAX,IXMAX,IYMAX

      IF (L.EQ.18.OR.L.EQ.20.OR.L.EQ.30.OR.L.EQ.31.OR.L.EQ.32.OR.L.EQ.34 !avril2003
     *.OR.L.EQ.36.OR.L.EQ.37.OR.L.EQ.38) THEN                            !avril2003
	 IF (LANGUE==1) WRITE(66,112) VMINC,IXMIN,IYMIN,VMAXC,IXMAX,IYMAX  !avril2003
	 IF (LANGUE==2) WRITE(66,110) VMINC,IXMIN,IYMIN,VMAXC,IXMAX,IYMAX  !avril2003
	ENDIF

C     Sauvetage pour le module d'analyse des extrêmes (subr. ANALYS)
      IF(L.LE.3)  GOTO 101
      IF((L.GE.17).AND.(L.LE.26)) GOTO 101
      IF(L.EQ.29) GOTO 101
      IF(L.GE.36) GOTO 101
      GOTO 999
  101 CONTINUE
      WRITE(304,50) L,A(L)		!extension neto
      WRITE(304,*) VMIN,IXMIN,IYMIN,VMAX,IXMAX,IYMAX	!extension neto

c      WRITE(*,*)' L et titre'
c      WRITE(*,50) L,(A(J),J=3*L-2,3*L)
c      WRITE(*,*) 'VMIN,IXMIN,IYMIN,VMAX,IXMAX,IYMAX'
c      WRITE(*,*) VMIN,IXMIN,IYMIN,VMAX,IXMAX,IYMAX
c      PAUSE 'ECRI'

  999 RETURN

  50  FORMAT(I2,T15,50A)
  100 FORMAT(/' Min. Value =',E11.4,'   in (X,Y)=(',I2,' , ',I2,')'/
     *        ' Max. Value =',E11.4,'   in (X,Y)=(',I2,' , ',I2,')')
  102 FORMAT(/' Valeur min =',E11.4,'   en (X,Y)=(',I2,' , ',I2,')'/
     *        ' Valeur max =',E11.4,'   en (X,Y)=(',I2,' , ',I2,')')
  110 FORMAT(/' Min. Value =',E11.4,'   in (X,Y)=(',I2,' , ',I2,') with   !avril2003
     *local flexion of girders'/                                          !avril2003
     *        ' Max. Value =',E11.4,'   in (X,Y)=(',I2,' , ',I2,') with   !avril2003
     *local flexion of girders')                                        !avril2003
  112 FORMAT(/' Valeur min =',E11.4,'   en (X,Y)=(',I2,' , ',I2,') avec   !avril2003
     *flexion locale des raidisseurs'/                                    !avril2003
     *        ' Valeur max =',E11.4,'   en (X,Y)=(',I2,' , ',I2,') avec   !avril2003
     *flexion locale des raidisseurs')                                  !avril2003
	RETURN
      END

c ------------------------------------------------------------
