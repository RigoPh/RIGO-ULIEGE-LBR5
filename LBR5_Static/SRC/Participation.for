      SUBROUTINE PARTICIPATION(NETO,COEF,PART,PHILN,QN,SYMY,Z,DELT,TETA,
     *						 YNEUT,M1,M2,NSOL,ITYPE,IMPR)				!15.10.05

      IMPLICIT REAL*8 (A-H,O-Z)
	INTEGER SYMY
	REAL *8 M1,M2												
      
      DIMENSION COEF(NETO,8),PART(NETO),PHILN(NETO),QN(NETO),
     *          Z(NETO,4),ABCD(8),SOL(20),DELT(NETO),TETA(NETO),
     *		  ITYPE(NETO)

	DIMENSION M1(10),M2(10)
 
      COMMON/PY/PI
	COMMON B(400),ZB(520)
C*********************************************************************************
                                                                      

C*********************************************************************************
	REWIND 97
	
	YMAX=MAX(Z(1,3),Z(1,4))
	IYMAX=1
	YMIN=MIN(Z(1,3),Z(1,4))
	IYMIN=1
	DO NEL=2,NETO
	  DO K=3,4
	    IF(Z(NEL,K).GT.YMAX) THEN
	      YMAX=Z(NEL,K)
	      IYMAX=NEL
	    ENDIF
	    IF(Z(NEL,K).LT.YMIN) THEN
	      YMIN=Z(NEL,K)
	      IYMIN=NEL
	    ENDIF
	  ENDDO
	ENDDO

c	Boucle sur le nombre de cas de charge
c	-------------------------------------

	DO IS=1,NSOL

c	  1er point de calcul
c	  -------------------
	  YSUP1=YNEUT
	  YSUP2=YNEUT
	  FORCTSUP1=0.
	  FORCTSUP2=0.	
	  DO 4 NEL=1,NETO
	    IF(ITYPE(NEL).EQ.5) GOTO 4
	    READ(97) ABCD
	    DO I=1,8
	      COEF(NEL,I)=ABCD(I)
	    ENDDO
	    IF(PART(NEL).LT.1.0) THEN
	      DO I=1,8
	        COEF(NEL,I)=COEF(NEL,I)*PART(NEL)
	      ENDDO
	    ENDIF
	    PHIL=DABS(PHILN(NEL))
	    QPH=QN(NEL)*PHIL*PI/180.
	    FORC1=(COEF(NEL,1)*QPH**3/4.+COEF(NEL,2)*QPH*QPH/3.+
     *           COEF(NEL,3)*QPH/2.+COEF(NEL,4))*QPH
	    FORC2=(COEF(NEL,5)*QPH**3/4.+COEF(NEL,6)*QPH*QPH/3.+
     *           COEF(NEL,7)*QPH/2.+COEF(NEL,8))*QPH
	  
	    IF(SYMY.EQ.0) THEN
	      FORCTSUP1=FORCTSUP1+FORC1
	      FORCTSUP2=FORCTSUP2+FORC2
	    ELSE
	      FORCTSUP1=FORCTSUP1+2*FORC1
	      FORCTSUP2=FORCTSUP2+2*FORC2
	    ENDIF									
    4	  CONTINUE
	  
c	  2e point de calcul
c	  ------------------
	  DO NEL=1,NETO
	    IF(ITYPE(NEL).NE.5) BACKSPACE(97)
	  ENDDO
	  DO NEL=1,IYMAX-1
	    IF(ITYPE(NEL).NE.5) READ(97)
	  ENDDO
	  READ(97) ABCD
	  DO NEL=IYMAX+1,NETO
	    IF(ITYPE(NEL).NE.5) READ(97)
	  ENDDO
	  PHI=-PHILN(IYMAX)
        Q=QN(IYMAX)
	  QPHI=PHI*Q*PI/180.
        QPHI2=QPHI*QPHI
        QPHI3=QPHI2*QPHI
	  FSUP1=MAX(ABCD(4),ABCD(1)*QPHI3+ABCD(2)*QPHI2+ABCD(3)*QPHI+
     *            ABCD(4))
	  FSUP2=MAX(ABCD(8),ABCD(5)*QPHI3+ABCD(6)*QPHI2+ABCD(7)*QPHI+
     *            ABCD(8))

	  ITERA=1
	  YMOD1=YMIN
	  YMOD2=YMIN
	  
c	    Déplacement de l'axe neutre
c	    ---------------------------
	  DO 2
	    FORCTINF1=0.
	    FORCTINF2=0.
	    DO 5 NEL=1,NETO
	      IF(ITYPE(NEL).EQ.5) GOTO 5
	      PHI=-PHILN(NEL)
            Q=QN(NEL)
            PHI2=PHI/3.
            PHI3=2.*PHI2
            SPH1=VSIN(PHI2/2.,0.D00)
            SPH2=VSIN(PHI3/2.,0.D00)
            CX1=VCOS(TETA(NEL),PHI2/2.)
            CX2=VCOS(TETA(NEL),PHI3/2.)
C
            ZB(1)=DELT(NEL)/DELT(IYMAX)*(Z(NEL,3)-YMOD1)/(YMAX-YMOD1)
     *            *FSUP1
            ZB(2)=DELT(NEL)/DELT(IYMAX)*(Z(NEL,3)-Q*2.*CX1*SPH1-YMOD1)/
     *            (YMAX-YMOD1)*FSUP1
            ZB(3)=DELT(NEL)/DELT(IYMAX) *(Z(NEL,3)-Q*2.*CX2*SPH2-YMOD1)/
     *	        (YMAX-YMOD1)*FSUP1	
            ZB(4)=DELT(NEL)/DELT(IYMAX) *(Z(NEL,4)-YMOD1)/(YMAX-YMOD1)
     *            *FSUP1
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

            CALL SYSTEM(4,1,SOL,20)

	      DO J=1,4
              COEF(NEL,J)=SOL(J)
	      ENDDO

	    
		  ZB(1)=DELT(NEL)/DELT(IYMAX)*(Z(NEL,3)-YMOD2)/(YMAX-YMOD2)
     *            *FSUP2
            ZB(2)=DELT(NEL)/DELT(IYMAX)*(Z(NEL,3)-Q*2.*CX1*SPH1-YMOD2)/
     *            (YMAX-YMOD2)*FSUP2
            ZB(3)=DELT(NEL)/DELT(IYMAX)*(Z(NEL,3)-Q*2.*CX2*SPH2-YMOD2)/
     *	        (YMAX-YMOD2)*FSUP2	
            ZB(4)=DELT(NEL)/DELT(IYMAX)*(Z(NEL,4)-YMOD2)/(YMAX-YMOD2)
     *            *FSUP2
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

            CALL SYSTEM(4,1,SOL,20)

	      DO J=1,4
              COEF(NEL,J+4)=SOL(J)
	      ENDDO
		
		
		  IF(PART(NEL).LT.1.0) THEN
	        DO I=1,8
	          COEF(NEL,I)=COEF(NEL,I)*PART(NEL)
	        ENDDO
	      ENDIF
	    
		  PHIL=DABS(PHILN(NEL))
            QPH=Q*PHIL*PI/180.

            FORC1=(COEF(NEL,1)*QPH**3/4.+COEF(NEL,2)*QPH*QPH/3.+
     *             COEF(NEL,3)*QPH/2.+COEF(NEL,4))*QPH
		  FORC2=(COEF(NEL,5)*QPH**3/4.+COEF(NEL,6)*QPH*QPH/3.+
     *             COEF(NEL,7)*QPH/2.+COEF(NEL,8))*QPH
	  
	      IF(SYMY.EQ.0) THEN
	        FORCTINF1=FORCTINF1+FORC1
	        FORCTINF2=FORCTINF2+FORC2
	      ELSE
	        FORCTINF1=FORCTINF1+2*FORC1
		    FORCTINF2=FORCTINF2+2*FORC2
	      ENDIF	
    5	    CONTINUE
	  
	    YINF1=YMOD1
		YINF2=YMOD2

		IF((DABS(FORCTINF1).LE.10.).AND.(DABS(FORCTINF2).LE.10.)) 
     *      GOTO 3

	    YMOD1=(YSUP1-YINF1)/(FORCTINF1-FORCTSUP1)*FORCTINF1+YINF1
	    YMOD2=(YSUP2-YINF2)/(FORCTINF2-FORCTSUP2)*FORCTINF2+YINF2
		IF(YMOD1.GT.(0.95*YMAX)) YMOD1=0.95*YMAX
		IF(YMOD2.GT.(0.95*YMAX)) YMOD2=0.95*YMAX
		IF(YMOD1.LT.(1.05*YMIN)) YMOD1=1.05*YMIN
		IF(YMOD2.LT.(1.05*YMIN)) YMOD2=1.05*YMIN

	    ITERA=ITERA+1
	
    2   CONTINUE
	

    3	  WRITE(66,10) IS,ITERA,YMOD1,YMOD2	
        FTMOM1=0.
	  FTMOM2=0.

c       Moment créé par la distribution de forces obtenue
c	  -------------------------------------------------
    	  DO 6 NEL=1,NETO
    	  
	    IF(ITYPE(NEL).EQ.5) GOTO 6
		Q=QN(NEL)
	    PHIL=DABS(PHILN(NEL))
          QPH=Q*PHIL*PI/180.
          CC1=VCOS(TETA(NEL),0.D00)
          CC2=VCOS(TETA(NEL),PHIL)
          CC3=VCOS(TETA(NEL),PHIL/2.)
          S1=VSIN(TETA(NEL),0.D00)
          S2=VSIN(TETA(NEL),PHIL)
          S3=VSIN(TETA(NEL),PHIL/2.)
          SPH1=VSIN(PHIL/2.,0.D00)
	  
	  
	    FORC1=(COEF(NEL,1)*QPH**3/4.+COEF(NEL,2)*QPH*QPH/3.+
     *           COEF(NEL,3)*QPH/2.+COEF(NEL,4))*QPH
	    FORC2=(COEF(NEL,5)*QPH**3/4.+COEF(NEL,6)*QPH*QPH/3.+
     *           COEF(NEL,7)*QPH/2.+COEF(NEL,8))*QPH										
	  
	  
          TEMP1=QPH*QPH* (COEF(NEL,1)*QPH**3 /5.+COEF(NEL,2)*QPH**2 /4.
     *                    +COEF(NEL,3)*QPH /3.+0.5*COEF(NEL,4))
          TEMP2=QPH*QPH* (COEF(NEL,5)*QPH**3 /5.+COEF(NEL,6)*QPH**2 /4.
     *                    +COEF(NEL,7)*QPH /3.+0.5*COEF(NEL,8))
	    
          FMOM1=-(Z(NEL,3)-YMOD1)*FORC1 + TEMP1*CC1
	    FMOM2=-(Z(NEL,3)-YMOD2)*FORC2 + TEMP2*CC1

          IF(SYMY.EQ.0) THEN
	      FTMOM1=FTMOM1+FMOM1
	      FTMOM2=FTMOM2+FMOM2
	    ELSE
	      FTMOM1=FTMOM1+2*FMOM1
	      FTMOM2=FTMOM2+2*FMOM2
	    ENDIF	
		
    6	  CONTINUE

c	  Multiplication des forces de bord pour avoir le moment appliqué
c	  ---------------------------------------------------------------
	  ALPHA1=M1(IS)/FTMOM1
	  ALPHA2=M2(IS)/FTMOM2
	  
	  DO 7 NEL=1,NETO
	    IF(ITYPE(NEL).EQ.5) GOTO 7
	    DO I=1,4
	      ABCD(I)=ALPHA1*COEF(NEL,I)
		  ABCD(I+4)=ALPHA2*COEF(NEL,I+4)
	    ENDDO
	    WRITE(94) ABCD
	    IF(IMPR.GE.-1) THEN					!15.10.05
	      WRITE(66,*)
            WRITE(66,'(A,I2)') 'PANNEAU nø ',NEL
            WRITE(66,'(A,A)')
     *				   'Cas de charge :  ABCD(1à4) for MX1 & MY1 et'	    
     *									  ,'ABCD(5à8) for MX2 & MY2'
            WRITE(66,'(T4,I2,A4,4(1x,E11.4),A4,4(1x,E11.4))')
     *            IS,' : ',(ABCD(K),K=1,4),'   ',(ABCD(K),K=5,8)
          	  
		ENDIF								!15.10.05 	  
    7	  CONTINUE
        WRITE(66,*)
	ENDDO

	REWIND 97
	REWIND 94
	DO IS=1,NSOL
	  DO 8 NEL=1,NETO
	    IF(ITYPE(NEL).EQ.5) GOTO 8
	    READ(94)ABCD
	    WRITE(97)ABCD
    8	  CONTINUE
      ENDDO

C	LES FORMATS
c================================================================================

   10 FORMAT('Pour le cas de charge',I2,' et après',I2,' itérations, la
     * position de l axe neutre KY'/'(repère utilisateur) modifiée par
     * la prise en compte des coefficients de participation est'/'à
     * gauche     ',E11.4,/'à droite     ',E11.4)


      RETURN
      END