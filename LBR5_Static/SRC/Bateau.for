      SUBROUTINE BATEAU(MX1,MX2,MY1,MY2,A,NETO,PHILN,QN,NSOL,			!flexion d'axe vert.
     *                  TETA,DELT,XNEU,YNEU,Z,SYMX,SYMY,ITYPE,IMPR2)	!15.10.05 !flexion d'axe vert.	!15.10.05

      IMPLICIT REAL*8 (A-H,O-Z)
	INTEGER SYMX,SYMY												!flexion d'axe vert.
      REAL *8 MX1,MX2,MY1,MY2											!flexion d'axe vert.

      DIMENSION MX1(10),MX2(10),MY1(10),MY2(10),A(8,NETO),			!flexion d'axe vert.
     *          PHILN(NETO),QN(NETO),TETA(NETO),DELT(NETO),Z(NETO,4),
     *		  ITYPE(NETO)											!flexion d'axe vert.

      DIMENSION SOL(20),VALF(11),ABCD(8)				!vecteur de travail
 
      COMMON/PY/PI
      COMMON B(400),ZB(360)						!flexion d'axe vert.
C*********************************************************************************
C                                                                       
C    CALCUL DE LA REPARTITION DES FORCES DE BORD SUR LES EXTREMITES.
C    Ces forces créent aux deux extrémités des moments (MX1,MY1) et (MX2,MY2)		!flexion d'axe vert.
c    qui correspondent à ceux induit par la proue et la poupe.
c
C    DONNEES : MX1,MY1,MX2,MY2													!flexion d'axe vert.
C       MX: Positif si le pont sup. est en traction et le fond en compression = HOGGING
c	  MY: Positif si le vecteur moment est dirigé vers le bas					!flexion d'axe vert.

c	On ne peut pas considérer un moment autour d'un axe de symétrie!

C    RESULTAT: A(8,NETO)  
C              ABCD(8) vect SOLUTION pour le sauvetage des résultats
C                      via  WRITE(97) ABCD
C    Vecteur de travail SOL(20)
                                                                       
C     Créer   : LBR-4, Const Navale, 1990

C     Modifiés: 
c       - Nbre de cas de charge :13-2-96 (NSOL) 
C       - DELT : Epaisseur moyenne (bordé + raid + Trav)   5-2-97
C       - LBR5.1  Mai 1999

C*********************************************************************************
	CALL ANNULD(A,8*NETO)
      CALL ANNULD(B,400)
	CALL ANNULD(ZB,360)
	CALL ANNULD(SOL,20)
	CALL ANNULD(ABCD,8)								!flexion d'axe vert.
C
      FTMOMYX=0.
	FTMOMXX=0.
	FORCTX=0.
	FTMOMXY=0.
	FTMOMYY=0.
      FORCTY=0.									    !flexion d'axe vert.


C
C ===MAIN Loop 10 Forces de bord créées par un moment autour de X et autour de Y===	!flexion d'axe vert.
	
	DO 10 I=1,NETO

c	 WRITE(66,99) I
c	 WRITE(66,*) 'Importance relative (Ep.moyen) =', DELT(I)

      IF(ITYPE(I).EQ.5) GOTO 10
	PHI=-PHILN(I)
      Q=QN(I)
      PHI2=PHI/3.
      PHI3=2.*PHI2
      SPH1=VSIN(PHI2/2.,0.D00)
      SPH2=VSIN(PHI3/2.,0.D00)
      CX1=VCOS(TETA(I),PHI2/2.)
      CX2=VCOS(TETA(I),PHI3/2.)
	CY1=VSIN(TETA(I),PHI2/2.)
	CY2=VSIN(TETA(I),PHI3/2.)						!flexion d'axe vert.
C
      ZB(1)=DELT(I) *( Z(I,3)-YNEU   )
      ZB(2)=DELT(I) *( Z(I,3)-Q*2.*CX1*SPH1-YNEU   )	!flexion d'axe vert.
      ZB(3)=DELT(I) *( Z(I,3)-Q*2.*CX2*SPH2-YNEU   )	!flexion d'axe vert.
      ZB(4)=DELT(I) *( Z(I,4)-YNEU   )
C
      QPHI=PHI*Q*PI/180.
      QPHI2=QPHI*QPHI
      QPHI3=QPHI2*QPHI

C     A L'ORIGINE Y=0
      B(1)=0.
      B(21)=0.
      B(41)=0.
      B(61)=1.
C     AU POINT Y=1/3 DE PHI
      B(2)=QPHI3/27.
      B(22)=QPHI2/9.
      B(42)=QPHI/3.
      B(62)=1.
C     AU POINT Y=2/3 DE PHI
      B(3)=8.*QPHI3/27.
      B(23)=4.*QPHI2/9.
      B(43)=2.*QPHI/3.
      B(63)=1.
C     AU POINT Y = PHI
      B(4)=QPHI3
      B(24)=QPHI2
      B(44)=QPHI
      B(64)=1.

c      WRITE(66,*)''
c	 WRITE(66,*)'Le système est, pour un moment autour de X, A(4,4)
c    * et ZB(4)'
c       WRITE(66,*) B(1),B(21),B(41),B(61),ZB(1)
c       WRITE(66,*) B(2),B(22),B(42),B(62),ZB(2)
c       WRITE(66,*) B(3),B(23),B(43),B(63),ZB(3)
c       WRITE(66,*) B(4),B(24),B(44),B(64),ZB(4)

      CALL SYSTEM(4,1,SOL,20)

c      WRITE(66,'(T2,A,4(1x,E11.4))')
c     * 'LES COEFFICIENTS A,B,C,D SONT (Forces de bord), pour un moment 
c     * autour de X:',(SOL(K),K=1,4)

	DO 11 J=1,4
 11   A(J,I)=SOL(J)

	ZB(1)=DELT(I) *( Z(I,1)-XNEU   )					!flexion d'axe vert.
      ZB(2)=DELT(I) *( Z(I,1)+Q*2.*CY1*SPH1-XNEU   )		!flexion d'axe vert.
      ZB(3)=DELT(I) *( Z(I,1)+Q*2.*CY2*SPH2-XNEU   )		!flexion d'axe vert.
      ZB(4)=DELT(I) *( Z(I,2)-XNEU   )					!flexion d'axe vert.

C     A L'ORIGINE Y=0
      B(1)=0.
      B(21)=0.
      B(41)=0.
      B(61)=1.
C     AU POINT Y=1/3 DE PHI
      B(2)=QPHI3/27.
      B(22)=QPHI2/9.
      B(42)=QPHI/3.
      B(62)=1.
C     AU POINT Y=2/3 DE PHI
      B(3)=8.*QPHI3/27.
      B(23)=4.*QPHI2/9.
      B(43)=2.*QPHI/3.
      B(63)=1.
C     AU POINT Y = PHI
      B(4)=QPHI3
      B(24)=QPHI2
      B(44)=QPHI
      B(64)=1.					!flexion d'axe vert.

c       WRITE(66,*)''
c	 WRITE(66,*)'Le système est, pour un moment autour de Y, A(4,4) 
c     * et ZB(4)'
c       WRITE(66,*) B(1),B(21),B(41),B(61),ZB(1)
c       WRITE(66,*) B(2),B(22),B(42),B(62),ZB(2)
c       WRITE(66,*) B(3),B(23),B(43),B(63),ZB(3)
c       WRITE(66,*) B(4),B(24),B(44),B(64),ZB(4)

      CALL SYSTEM(4,1,SOL,20)

c      WRITE(66,'(T2,A,4(1x,E11.4))')
c     * 'LES COEFFICIENTS A,B,C,D SONT (Forces de bord), pour un moment 
c     * autour de Y:',(SOL(K),K=1,4)

	DO 12 J=1,4
 12     A(J+4,I)=SOL(J)										!flexion d'axe vert.


C     FMOMYX MOMENT AUTOUR DE L'AXE VERT. Y DU AUX FORCES DE BORD CREEES PAR UN MOMENT AUTOUR DE X
C     FMOMXX MOMENT AUTOUR DE L'AXE HORIZ.X DU AUX FORCES DE BORD CREEES PAR UN MOMENT AUTOUR DE X
C     FORCX LA SOMME SELON OX DES FORCES DE BORD CREEES PAR UN MOMENT AUTOUR DE X(FORCTX LA SOMME DE FORCX)
C     FMOMYY MOMENT AUTOUR DE L'AXE VERT. Y DU AUX FORCES DE BORD CREEES PAR UN MOMENT AUTOUR DE Y
C     FMOMXY MOMENT AUTOUR DE L'AXE HORIZ.X DU AUX FORCES DE BORD CREEES PAR UN MOMENT AUTOUR DE Y
C     FORCY LA SOMME SELON OX DES FORCES DE BORD CREEES PAR UN MOMENT AUTOUR DE Y(FORCTY LA SOMME DE FORCY)

C     FTMOMXX,FTMOMYX LES SOMMES DES MOMENTS FMOMXX ET FMOMYX
C     FTMOMXY,FTMOMYX LES SOMMES DES MOMENTS FMOMXY ET FMOMYX			!flexion d'axe vert.

C     LES AXES CONSIDERES ETANT CEUX AU CENTRE DE GRAVITE.

      PHIL=DABS(PHILN(I))
      QPH=Q*PHIL*PI/180.
      CC1=VCOS(TETA(I),0.D00)
      CC2=VCOS(TETA(I),PHIL)
      CC3=VCOS(TETA(I),PHIL/2.)
      S1=VSIN(TETA(I),0.D00)
      S2=VSIN(TETA(I),PHIL)
      S3=VSIN(TETA(I),PHIL/2.)
      SPH1=VSIN(PHIL/2.,0.D00)

C     FORCES
C     *******
      FORCX=(A(1,I)*QPH**3/4.+A(2,I)*QPH*QPH/3.+A(3,I)*QPH/2.+A(4,I))
     *	  *QPH
	FORCY=(A(5,I)*QPH**3/4.+A(6,I)*QPH*QPH/3.+A(7,I)*QPH/2.+A(8,I))
     *         *QPH
	
    	IF(SYMX.EQ.0.AND.SYMY.EQ.0) THEN
	  FORCTX=FORCTX+FORCX
	  FORCTY=FORCTY+FORCY
	ELSE
	  IF(SYMX.EQ.0.AND.SYMY.EQ.1) FORCTX=FORCTX+2*FORCX
	  IF(SYMX.EQ.1.AND.SYMY.EQ.0) FORCTY=FORCTY+2*FORCY
	ENDIF													!flexion d'axe vert.

	
C     MOMENTS
C     *******                                                                                                                                  BBA01030
      IF (PHIL.GE.1.E-05) THEN
        
	  TERMX1=(A(1,I)*QPH**2+A(2,I)*QPH+A(3,I)-Q*Q*6.*A(1,I))  *QPH
	  TERMX2=(3.*A(1,I)*QPH+2.*A(2,I)) *QPH
        TERMX3=Q* (-6.*A(1,I)*Q*Q+A(3,I))
        TERMX4=-2.*A(2,I)*Q*Q+A(4,I)
	  TERMY1=(A(5,I)*QPH**2+A(6,I)*QPH+A(7,I)-Q*Q*6.*A(5,I))  *QPH
        TERMY2=(3.*A(5,I)*QPH+2.*A(6,I)) *QPH
        TERMY3=Q* (-6.*A(5,I)*Q*Q+A(7,I))
        TERMY4=-2.*A(6,I)*Q*Q+A(8,I)
        
	  FMOMYX=-FORCX * (Z(I,1)-XNEU+Q*CC1)
        FMOMYX=FMOMYX+Q*Q*  (S2*TERMX1+Q*CC2*TERMX2-2.*SPH1*S3*TERMX3+
     *                       2.*SPH1 *CC3*TERMX4)
        FMOMXX= - FORCX * (Z(I,3)-YNEU+Q*S1)
        FMOMXX=FMOMXX+Q*Q* (-CC2*TERMX1+Q*S2*TERMX2+2.*SPH1*S3*TERMX4+
     *                      2.*SPH1*CC3*TERMX3)
	  FMOMYY=-FORCY * (Z(I,1)-XNEU+Q*CC1)
        FMOMYY=FMOMYY+Q*Q*  (S2*TERMY1+Q*CC2*TERMY2-2.*SPH1*S3*TERMY3+
     *                       2.*SPH1 *CC3*TERMY4)
        FMOMXY= - FORCY * (Z(I,3)-YNEU+Q*S1)
        FMOMXY=FMOMXY+Q*Q* (-CC2*TERMY1+Q*S2*TERMY2+2.*SPH1*S3*TERMY4+
     *                       2.*SPH1*CC3*TERMY3)

	ELSE
        TEMPX=QPH*QPH* (A(1,I)*  QPH**3 /5. + A(2,I)  *QPH**2 /4.
     *                 +A(3,I)*  QPH /3.    + 0.5 * A(4,I) )
	  TEMPY=QPH*QPH* (A(5,I)*  QPH**3 /5. + A(6,I)  *QPH**2 /4.
     *                  +A(7,I)*  QPH /3.    + 0.5 * A(8,I) )
	  
         FMOMYX=-(Z(I,1)-XNEU) * FORCX  -  TEMPX * S1
	   FMOMXX=-(Z(I,3)-YNEU) * FORCX  +  TEMPX * CC1
	   FMOMYY=-(Z(I,1)-XNEU) * FORCY  -  TEMPY * S1
         FMOMXY=-(Z(I,3)-YNEU) * FORCY  +  TEMPY * CC1
	ENDIF

      IF(SYMX.EQ.1.OR.SYMY.EQ.1) THEN
	  IF(SYMX.EQ.1.AND.SYMY.EQ.1) THEN
	    FTMOMXX=FTMOMXX+4*FMOMXX
	    FTMOMYY=FTMOMYY+4*FMOMYY
	  ELSE
	    FTMOMXX=FTMOMXX+2*FMOMXX
	    FTMOMYY=FTMOMYY+2*FMOMYY
	  ENDIF 
	ELSE
	  FTMOMXX=FTMOMXX+FMOMXX
	  FTMOMYX=FTMOMYX+FMOMYX
	  FTMOMYY=FTMOMYY+FMOMYY
        FTMOMXY=FTMOMXY+FMOMXY
     	ENDIF								!flexion d'axe vert.

	VALF(1)=A(4,I)
      CALL PVAL(A(1,I),A(2,I),A(3,I),A(4,I),PHIL,Q,0.1D+00,VALF(2))
      CALL PVAL(A(1,I),A(2,I),A(3,I),A(4,I),PHIL,Q,0.2D+00,VALF(3))
      CALL PVAL(A(1,I),A(2,I),A(3,I),A(4,I),PHIL,Q,0.3D+00,VALF(4))
      CALL PVAL(A(1,I),A(2,I),A(3,I),A(4,I),PHIL,Q,0.4D+00,VALF(5))
      CALL PVAL(A(1,I),A(2,I),A(3,I),A(4,I),PHIL,Q,0.5D+00,VALF(6))
      CALL PVAL(A(1,I),A(2,I),A(3,I),A(4,I),PHIL,Q,0.6D+00,VALF(7))
      CALL PVAL(A(1,I),A(2,I),A(3,I),A(4,I),PHIL,Q,0.7D+00,VALF(8))
      CALL PVAL(A(1,I),A(2,I),A(3,I),A(4,I),PHIL,Q,0.8D+00,VALF(9))
      CALL PVAL(A(1,I),A(2,I),A(3,I),A(4,I),PHIL,Q,0.9D+00,VALF(10))
      CALL PVAL(A(1,I),A(2,I),A(3,I),A(4,I),PHIL,Q,1.0D+00,VALF(11))			!flexion d'axe vert.

C     LES IMPRESSIONS
C     ---------------
c      WRITE(66,100) VALF,FMOMYX,FMOMXX,FORCX				!flexion d'axe vert.

	VALF(1)=A(8,I)
      CALL PVAL(A(5,I),A(6,I),A(7,I),A(8,I),PHIL,Q,0.1D+00,VALF(2))
      CALL PVAL(A(5,I),A(6,I),A(7,I),A(8,I),PHIL,Q,0.2D+00,VALF(3))
      CALL PVAL(A(5,I),A(6,I),A(7,I),A(8,I),PHIL,Q,0.3D+00,VALF(4))
      CALL PVAL(A(5,I),A(6,I),A(7,I),A(8,I),PHIL,Q,0.4D+00,VALF(5))
      CALL PVAL(A(5,I),A(6,I),A(7,I),A(8,I),PHIL,Q,0.5D+00,VALF(6))
      CALL PVAL(A(5,I),A(6,I),A(7,I),A(8,I),PHIL,Q,0.6D+00,VALF(7))
      CALL PVAL(A(5,I),A(6,I),A(7,I),A(8,I),PHIL,Q,0.7D+00,VALF(8))
      CALL PVAL(A(5,I),A(6,I),A(7,I),A(8,I),PHIL,Q,0.8D+00,VALF(9))
      CALL PVAL(A(5,I),A(6,I),A(7,I),A(8,I),PHIL,Q,0.9D+00,VALF(10))
      CALL PVAL(A(5,I),A(6,I),A(7,I),A(8,I),PHIL,Q,1.0D+00,VALF(11))			
	
C     LES IMPRESSIONS
C     ---------------
c      WRITE(66,101) VALF,FMOMYY,FMOMXY,FORCY				!flexion d'axe vert.

 10   CONTINUE
C
C =====End of Loop 10 ==================================================
C
	IF(IMPR2.GE.-1) THEN			!15.10.05
        WRITE(66,102)FTMOMYX
        WRITE(66,103)FTMOMXX
        WRITE(66,104)FORCTX
        WRITE(66,105)FTMOMYY
        WRITE(66,106)FTMOMXY
        WRITE(66,107)FORCTY			
	ENDIF							!15.10.05
	

C   Calcul des coéfficients pour chaque cas de charges et sauvetage des résultats 
C   (pour Subr. MOM dans BO1) sur un fichier temporaire (FILE 97)
C    -------------------------------------------------

      DO 15 IS=1,NSOL														!coefficient de participation
	
	  B(1)=FTMOMXX
	  B(21)=FTMOMXY
	  B(2)=FTMOMYX
	  B(22)=FTMOMYY
	  ZB(1)=MX1(IS)
	  ZB(2)=MY1(IS)
	  CALL SYSTEM(2,1,SOL,20)
	  COEFX1=SOL(1)
	  COEFY1=SOL(2)						!flexion d'axe vert.
	
	  B(1)=FTMOMXX
	  B(21)=FTMOMXY
	  B(2)=FTMOMYX
	  B(22)=FTMOMYY
	  ZB(1)=MX2(IS)
	  ZB(2)=MY2(IS)
	  CALL SYSTEM(2,1,SOL,20)
	  COEFX2=SOL(1)
	  COEFY2=SOL(2)						!flexion d'axe vert.
	 	
	  DO 15 I=1,NETO
	    IF(ITYPE(I).EQ.5) GOTO 15														!coefficient de participation
          IF(IMPR2.GE.-1) THEN			!15.10.05
            WRITE(66,*)
            WRITE(66,'(A,I2)') 'PANNEAU nø ',I
            WRITE(66,'(A,A)')'Cas de charge :  ABCD(1à4) for MX1 & MY1'	    !flexion d'axe vert.
     *									 ,'et ABCD(5à8) for MX2 & MY2'		!flexion d'axe vert.
      	  
          ENDIF			!15.10.05
          DO J=1,4
	  
          ABCD(J)=A(J,I)*COEFX1+A(J+4,I)*COEFY1
  	    ABCD(J+4)=A(J,I)*COEFX2+A(J+4,I)*COEFY2	
	  
	    ENDDO								!flexion d'axe vert.
	  
 		IF(IMPR2.GE.-1) THEN			!15.10.05
            WRITE(66,'(T4,I2,A4,4(1x,E11.4),A4,4(1x,E11.4))')
     *            IS,' : ',(ABCD(K),K=1,4),'   ',(ABCD(K),K=5,8)

		ENDIF			!15.10.05

          WRITE(97) ABCD ! SAUVETAGE SUR FILE 97 DES ABCD (pour lecture dans BO1)

  15  CONTINUE

C     LES FORMATS
C    -------------------------------------------------
   99 FORMAT(T2,'PANNEAU',I2/T2,10(1H-))
  100 FORMAT(/T2,'VALEURS EN 11 PTS , Y=0,...,Y=Q*PHIL',  
     *' DES FORCES DE BORD CREEES PAR UN MOMENT AUTOUR DE X'/
     * T2,11(E10.3,1X)/
     * T2,'MOMENT D''AXE VERT.Y DU A CES FORCES  =',E11.4,' N.M'/
     * T2,'MOMENT D''AXE HORIZ.X DU A CES FORCES  =',E11.4,' N.M'/
     * T2,'FORCE RESULTANTE SELON OX ',E11.4,' N. '/)
  101 FORMAT(T2,'VALEURS EN 11 PTS , Y=0, ,  ...,Y=Q*PHIL',
     *' DES FORCES DE BORD CREEES PAR UN MOMENT AUTOUR DE Y'/
     *T2,11(E10.3,1X)/
     *T2,'MOMENT D''AXE VERT.Y DU A CES FORCES =',E11.4,' N.M'/
     *T2,'MOMENT D''AXE HORIZ.X DU A CES FORCES  =',E11.4,' N.M'/
     *T2,'FORCE RESULTANTE SELON OX ',E11.4,' N. '/)
  102 FORMAT(T2,'MOMENT GENERAL AUTOUR DE L''AXE VERT.Y POUR TOUS LES ',
     *'PANNEAUX DU AUX FORCES DE BORD CREEES PAR UN MOMENT AUTOUR DE X'/
     *T2,116(1H=)/T2,' MOMENT MYX TOTAL = ',E11.4,' N.M'/)
  103 FORMAT(T2,'MOMENT GENERAL AUTOUR DE L''AXE HORIZ.X POUR TOUS LES 
     *','PANNEAUX DU AUX FORCES DE BORD CREEES PAR UN MOMENT AUTOUR DE X
     *'/T2,116(1H=)/T2,' MOMENT MXX TOTAL = ',E11.4,' N.M'/)
  104 FORMAT(T2,'SOMME DES FORCES SELON OX POUR TOUS LES PANNEAUX ',
     *'DU AUX FORCES DE BORD CREEES PAR UN MOMENT AUTOUR DE X'/
     *T2,102(1H=)/T2,'FORCE TOTALE GENERALE =',E11.4,' N.'/)
  105 FORMAT(T2,'MOMENT GENERAL AUTOUR DE L''AXE VERT.Y POUR TOUS LES ',
     *'PANNEAUX DU AUX FORCES DE BORD CREEES PAR UN MOMENT AUTOUR DE Y'/
     *T2,116(1H=)/T2,' MOMENT MYX TOTAL = ',E11.4,' N.M'/)
  106 FORMAT(T2,'MOMENT GENERAL AUTOUR DE L''AXE HORIZ.X POUR TOUS LES 
     *','PANNEAUX DU AUX FORCES DE BORD CREEES PAR UN MOMENT AUTOUR DE Y
     *'/T2,116(1H=)/T2,' MOMENT MXX TOTAL = ',E11.4,' N.M'/)
  107 FORMAT(T2,'SOMME DES FORCES SELON OX POUR TOUS LES PANNEAUX ',
     *'DU AUX FORCES DE BORD CREEES PAR UN MOMENT AUTOUR DE Y'/
     *T2,102(1H=)/T2,'FORCE TOTALE GENERALE =',E11.4,' N.'/)					!flexion d'axe vert.

      RETURN
      END
C
c **********************************************************************
c
      SUBROUTINE PVAL(A,B,C,D,PH,Q,RAP,VAL)

      IMPLICIT REAL*8(A-H,O-Z)
      COMMON/PY/PI

      QPH=Q*PH*RAP*PI/180.
      VAL=A*QPH*QPH*QPH+B*QPH*QPH+C*QPH+D

      RETURN
      END
