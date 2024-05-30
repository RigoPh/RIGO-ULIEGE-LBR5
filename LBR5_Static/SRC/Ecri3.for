
      SUBROUTINE ECRI3(DIS,NEL,NE,NS,Q,EFF2)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION DIS(5),EFF2(2040)
	CHARACTER *40 B(8),BB(8)

      COMMON/ECR/U(51),MT
      COMMON/PY/PI
      COMMON/LANGUE/LANGUE
C***********************************************************************
C
C     SUBROUTINE ECRI3 (EFF2)
C     Impression des contraintes dans le bordage au niveau des faces du borde
C     z=+delta/2 et Z=-delta/2.

C  Creer    :  7-9-93   Ph. Rigo
C  Modifier :  8-5-95   Mod. d'optimisation: subr. Extrem2
C            15-12-2000 Version anglaise

C***********************************************************************
      IF(LANGUE==1) THEN
      DATA B/'CONTR. TRANSV.  Sy  Bordé (Z=+d/2)',
     *       'CONTR. LONGIT.  Sx  Bordé (Z=+d/2)',
     *       'CONTR. CISAILL. Txy Bordé (Z=+d/2)',
     *       'CONTR. de Von MISES, Bordé (Z=+d/2)',
     *       'CONTR. TRANSV.  Sy  Bordé (Z=-d/2)',
     *       'CONTR. LONGIT.  Sx  Bordé (Z=-d/2)',
     *       'CONTR. CISAILL. Txy Bordé (Z=-d/2)',
     *       'CONTR. de Von MISES, Bordé (Z=-d/2)'/
	ELSE
c             1234567890123456789012345678901234567890
      DATA BB/'TRANSV. STRESS Sy Plate (Z=+d/2)',
     *        'LONGIT. STRESS Sx Plate (Z=+d/2)',
     *        'SHEAR STRESS, Txy Plate (Z=+d/2)',
     *        'Von Mises STRESS, Plate (Z=+d/2)',
     *        'TRANSV. STRESS Sy Plate (Z=-d/2)',
     *        'LONGIT. STRESS Sx Plate (Z=-d/2)',
     *        'SHEAR STRESS, Txy Plate (Z=-d/2)',
     *        'Von Mises STRESS, Plate (Z=-d/2)'/
	ENDIF


      IF(LANGUE==1) THEN
        WRITE(66,150)NEL
  150   FORMAT(/5X,'Résultats complémentaires - panneau ',I3,/5X,
     *         45(1H*)/)
	ELSE
        WRITE(66,151)NEL
  151   FORMAT(/5X,'Additional Results - Panel ',I3,/5X,
     *          45(1H*)/)
        DO I=1,8
	    B(I)=BB(I)
	  ENDDO
	ENDIF

      KH=2*MT+31
      L=0

      DO 157 KR=1,8
        L=L+1
        WRITE(66,50) B(L)
  50    FORMAT(/T15,A41/T15,40(1H+))
        WRITE(66,156) DIS
        IF(NS.EQ.1) THEN
          DO 163 I=1,KH
              JM=(L-1)*255+I
              IJ=JM+204
  163     WRITE(66,158) U(I),(EFF2(K),K=JM,IJ,51)
        ELSE
          GOTO(2,2,3,2,2,2,3,2),L
   3          JM=(L-1)*255+1
              IJ=L*255
              DO 4 KK=JM,IJ
   4          EFF2(KK)=-EFF2(KK)
   2      DO 159 I=1,31
              JM=(L-1)*255+32-I
              IJ=JM+204
  159     WRITE(66,158) U(I),(EFF2(K),K=JM,IJ,51)
          DO 161 I=32,KH
              JM=(L-1)*255+(KH+32-I)
              IJ=JM+204
              UTRAV=U(31)-U(KH+32-I)
  161     WRITE(66,158) UTRAV,(EFF2(K),K=JM,IJ,51)
        ENDIF
      CALL EXTRE2(L,KH,EFF2,B)
  157 CONTINUE

      WRITE(304,*) '88'		!extension neto
C
  156 FORMAT(/10X,5(4X,'X=',F6.3,' m')/)
  158 FORMAT(' Y=',F6.3,'m',5(1X,E13.6))
      RETURN
      END

C***********************************************************************
C***********************************************************************

      SUBROUTINE EXTRE2(L,KH,EFF2,A)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION EFF2(2040)
	CHARACTER *40 A(8)
      COMMON/PY/PI
      COMMON/LANGUE/LANGUE
C
C***********************************************************************
C
C     SUBROUTINE EXTRE2
C     ++++++++++++++++++
C     Recherche les valeurs extrêmes (max et min)
C
C     Modif : Nov 2000                                 Créer :  5-5-95
C
C***********************************************************************

      VMIN=0.
      VMAX=0.
      IXMIN=0
      IXMAX=0
      IYMIN=0
      IYMAX=0
      I1=(L-1)*255

      DO 1 KX=1,5
      I2=I1+(KX-1)*51
        DO 2 KY=1,KH
        K1=I2+KY
        IF(VMIN.GT.EFF2(K1)) THEN
                 VMIN=EFF2(K1)
                IXMIN=KX
                IYMIN=KY
        ENDIF
        IF(VMAX.LT.EFF2(K1)) THEN
                VMAX=EFF2(K1)
                IXMAX=KX
                IYMAX=KY
        ENDIF
   2    CONTINUE
   1  CONTINUE

      IF(LANGUE==1) WRITE(66,102) VMIN,IXMIN,IYMIN,VMAX,IXMAX,IYMAX
      IF(LANGUE==2) WRITE(66,100) VMIN,IXMIN,IYMIN,VMAX,IXMAX,IYMAX

C     Sauvetage pour le module d'analyse des extrêmes (subr. ANALYS)
      IF(L.EQ.4) GOTO 101
      IF(L.EQ.8) GOTO 101
      GOTO 999
  101 CONTINUE
      WRITE(304,50) L,A(L)							!extension neto
      WRITE(304,*) VMIN,IXMIN,IYMIN,VMAX,IXMAX,IYMAX	!extension neto

  999 RETURN

  50  FORMAT(I2,T15,40A)
  100 FORMAT(/' Min. Value =',E11.4,'   in (X,Y)=(',I2,' , ',I2,')'/
     *        ' Max. Value =',E11.4,'   in (X,Y)=(',I2,' , ',I2,')')
  102 FORMAT(/' Valeur min =',E11.4,'   en (X,Y)=(',I2,' , ',I2,')'/
     *        ' Valeur max =',E11.4,'   en (X,Y)=(',I2,' , ',I2,')')
      RETURN
      END
