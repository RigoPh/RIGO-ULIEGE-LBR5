
      SUBROUTINE ECRI2(DIS,NEL,EFF,CONC,NE,INDAIG,INDRAI,NS,Q)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION DIS(5),EFF(9690),CONC(750)
	CHARACTER *50 A(3),B(3)
      COMMON/ECR/U(51),MT
      COMMON/PY/PI
      COMMON/LANGUE/LANGUE
C
C***********************************************************************
C     20-1-94
c
C     SUBROUTINE ECRIT(forme réduite)
C     SUBROUTINE DE MISE EN FORME DES RESULTATS POUR ECRITURE DANS UN FICHIER
c
c     Dernière modif: Nov. 2000
C***********************************************************************
      DATA A/'V (m) - DEPLACEMENT TRANSVERSAL selon OY (Z=0)',		! 1
     *       'U (m) - DEPLACEMENT LONGITUDINAL selon OX (Z=0)',
     *       'W (m) - FLECHE, Deplacement selon OZ (Z=0)'/			! 3

      DATA B/'V (m) - TRANSV. DISPL. along OY (Z=0)',				! 1
     *       'U (m) - LONG. DISPL. along OX (Z=0)',
     *       'W (m) - LATERAL DEFLECTION along OZ(Z=0)'/			! 3

      IF(LANGUE.NE.1) THEN
        DO I=1,3
	    A(I)=B(I)
	  ENDDO
	ENDIF
C
      KH=2*MT+31
      DO 1 I=1,KH
  1   U(I)=U(I)*Q*PI/180.
      WRITE(66,150)NEL
      L=0
      DO 157 L=1,3
      WRITE(66,50) A(L)
  50  FORMAT(/T15,A50/T15,40(1H+))
      WRITE(66,156) DIS
        IF(NS.EQ.1) THEN
          DO 163 I=1,KH
              JM=(L-1)*255+I
              IJ=JM+204
  163     WRITE(66,158) U(I),(EFF(K),K=JM,IJ,51)
        ELSE
          DO 159 I=1,31
              JM=(L-1)*255+31-I
              IJ=JM+204
  159     WRITE(66,158) U(31-I),(EFF(K),K=JM,IJ,51)
          DO 161 I=32,KH
              JM=(L-1)*255+(KH+32-I)
              IJ=JM+204
  161     WRITE(66,158) U(KH+32-I),(EFF(K),K=JM,IJ,51)
        ENDIF
	CALL EXTRE5(L,KH,EFF,A)					!15.10.05
  201 WRITE(304,*) '99'						!extension neto  !15.10.05 
  157 CONTINUE

  150 FORMAT(T3,'PANNEAU-PANEL No ',I2,/T3,18(1H+))
  156 FORMAT(/10X,5(4X,'X=',F6.3,' m')/)
  158 FORMAT(' Y=',F6.3,'m',5(1X,E13.6))

      RETURN
      END
c ------------------------------------------------------------		!15.10.05
      SUBROUTINE EXTRE5(L,KH,EFF,A) 
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION EFF(9690)
	CHARACTER *50 A(3)
      COMMON/PY/PI
      COMMON/LANGUE/LANGUE
C
C***********************************************************************
C
C     SUBROUTINE EXTRE5
C     ++++++++++++++++++
C     Recherche les valeurs extrêmes (max et min)
C
C     Modif : Nov 2000                                    Créer : 13-3-95
C
C***********************************************************************

      VMIN=0.
      VMAX=0.
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
        IF(VMIN.GE.EFF(K1)) THEN    !avril2003
                 VMIN=EFF(K1)
                IXMIN=KX
                IYMIN=KY
        ENDIF
        IF(VMAX.LE.EFF(K1)) THEN     !avril2003 il vaut mieux mettre LE plutôt que LT
                VMAX=EFF(K1)
                IXMAX=KX
                IYMAX=KY
        ENDIF
   2    CONTINUE
   1  CONTINUE


      IF(LANGUE==1) WRITE(66,102) VMIN,IXMIN,IYMIN,VMAX,IXMAX,IYMAX
      IF(LANGUE==2) WRITE(66,100) VMIN,IXMIN,IYMIN,VMAX,IXMAX,IYMAX


C     Sauvetage pour le module d'analyse des extrêmes (subr. ANALYS)
      WRITE(304,50) L,A(L)
      WRITE(304,*) VMIN,IXMIN,IYMIN,VMAX,IXMAX,IYMAX

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
