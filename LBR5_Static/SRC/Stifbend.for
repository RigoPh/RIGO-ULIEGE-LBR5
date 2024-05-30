      SUBROUTINE STIFBEND(NEL,NETO,KSR,KSE,EPSA,EPSR,DELTA,
     *					HXR,DXR,WXR,TXR,EPSA2,IS,TRAV,NSOL,
     *					IOPTI,SIG,dSIG)

      IMPLICIT REAL*8 (A-H,O-Z)
	DIMENSION TRAV(1)

     	I1=1			!XI(NSOL)
	I2=I1+NSOL		!XF(NSOL)
	I3=I2+NSOL	

	IMAX=I3-1		!2*NSOL

	CALL SSTIFBEND(NEL,NETO,KSR,KSE,EPSA,EPSR,DELTA,
     *			   HXR,DXR,WXR,TXR,EPSA2,IS,TRAV(I1),TRAV(I2),NSOL,
     *			   IOPTI,SIG,dSIG)

	RETURN
	END

c	******************************************************************************************************
c	******************************************************************************************************
c	******************************************************************************************************

	SUBROUTINE SSTIFBEND(NEL,NETO,KSR,KSE,EPSA,EPSR,DELTA,
     *					 HXR,DXR,WXR,TXR,EPSA2,IS,XI,XF,NSOL,
     *					 IOPTI,SIG,dSIG)


      IMPLICIT REAL*8 (A-H,O-Z)
	REAL*8 INER

	DIMENSION XI(NSOL),XF(NSOL)

	DIMENSION dSIG(9)

	DIMENSION dSECT(9),dSTAT(9),dANEU(9),dINER(9),dSMOD(9),dPLIN(9)
	
	DIMENSION ABTR(10)

	READ(99) ABTR,PHIL,MT,TETA,XI,XF

	IF(HXR.LE.0.01) THEN
	  SIG=0.
	  CALL ANNULD(dSIG,9)
	  RETURN
	ENDIF

c	Module de résistance I/v
c	------------------------
	SECT=DELTA*EPSR+HXR*DXR+WXR*TXR
	STAT=(DELTA*EPSR)*(DELTA/2.+HXR+TXR)+(HXR*DXR)*(HXR/2.+TXR)+
     *	 (WXR*TXR)*(TXR/2.)
	ANEU=STAT/SECT
	INER=EPSR*DELTA**3/12.+(DELTA*EPSR)*(DELTA/2.+HXR+TXR-ANEU)**2+
     *	 DXR*HXR**3/12.+(HXR*DXR)*(HXR/2.+TXR-ANEU)**2+
     *	 WXR*TXR**3/12.+(WXR*TXR)*(TXR/2.-ANEU)**2
	SMOD=INER/ANEU

c	Sensibilités du module de résistance I/v
c	----------------------------------------
	IF(IOPTI.GE.1) THEN
	  CALL ANNULD(dSECT,9)
	  dSECT(1)=EPSR
	  dSECT(6)=DXR
	  dSECT(7)=HXR
	  dSECT(8)=TXR
	  dSECT(9)=DELTA
	  CALL ANNULD(dSTAT,9)
	  dSTAT(1)=EPSR*(DELTA/2.+HXR+TXR)+(DELTA*EPSR)*0.5
	  dSTAT(6)=(DELTA*EPSR)*1.+DXR*(HXR/2.+TXR)+(HXR*DXR)*0.5
	  dSTAT(7)=HXR*(HXR/2.+TXR)
	  dSTAT(8)=TXR*(TXR/2.)
	  dSTAT(9)=DELTA*(DELTA/2.+HXR+TXR)
	  CALL ANNULD(dANEU,9)
	  DO I=1,9
	    dANEU(I)=(dSTAT(I)*SECT-STAT*dSECT(I))/SECT**2
	  ENDDO
	  CALL ANNULD(dINER,9)
	  dINER(1)=3.*EPSR*DELTA**2/12.+
     *           EPSR*(DELTA/2.+HXR+TXR-ANEU)**2+
     *		   (DELTA*EPSR)*2.*(DELTA/2.+HXR+TXR-ANEU)*(0.5-dANEU(1))+
     *		   (HXR*DXR)*2.*(HXR/2.+TXR-ANEU)*(-dANEU(1))+
     *		   (WXR*TXR)*2.*(TXR/2.-ANEU)*(-dANEU(1))
	  dINER(6)=(DELTA*EPSR)*2.*(DELTA/2.+HXR+TXR-ANEU)*(1.-dANEU(6))+
     *		   3.*DXR*HXR**2/12.+
     *		   DXR*(HXR/2.+TXR-ANEU)**2+
     *	       (HXR*DXR)*2.*(HXR/2.+TXR-ANEU)*(0.5-dANEU(6))+
     *		   (WXR*TXR)*2.*(TXR/2.-ANEU)*(-dANEU(6))
	  dINER(7)=(DELTA*EPSR)*2.*(DELTA/2.+HXR+TXR-ANEU)*(-dANEU(7))+
     *		   HXR**3/12.+HXR*(HXR/2.+TXR-ANEU)**2+
     *		   (HXR*DXR)*2.*(HXR/2.+TXR-ANEU)*(-dANEU(7))+
     *		   (WXR*TXR)*2.*(TXR/2.-ANEU)*(-dANEU(7))
	  dINER(8)=(DELTA*EPSR)*2.*(DELTA/2.+HXR+TXR-ANEU)*(-dANEU(8))+
     *		   (HXR*DXR)*2.*(HXR/2.+TXR-ANEU)*(-dANEU(8))+
     *		   TXR**3/12.+TXR*(TXR/2.-ANEU)**2+
     *		   (WXR*TXR)*2.*(TXR/2.-ANEU)*(-dANEU(8))
        dINER(9)=DELTA**3/12.+DELTA*(DELTA/2.+HXR+TXR-ANEU)**2+
     *		   (DELTA*EPSR)*2.*(DELTA/2.+HXR+TXR-ANEU)*(-dANEU(9))+
     *		   (HXR*DXR)*2.*(HXR/2.+TXR-ANEU)*(-dANEU(9))+
     *		   (WXR*TXR)*2.*(TXR/2.-ANEU)*(-dANEU(9))
	  CALL ANNULD(dSMOD,9)
	  DO I=1,9
	    dSMOD(I)=(dINER(I)*ANEU-INER*dANEU(I))/ANEU**2
	  ENDDO
	ENDIF

c	Contraintes normales dues à la flexion locale des lisses
c	--------------------------------------------------------
	IF(DABS(XI(IS)).GE.DABS(XF(IS))) THEN
c	  PMAX=DABS(XI(IS))*9.81E3
	PMAX=DABS(XI(IS))*10.E3
	ELSE
c	  PMAX=DABS(XF(IS))*9.81E3
	PMAX=DABS(XF(IS))*10.E3
	ENDIF

	COEF=1.
c	IF(KSR.EQ.1) THEN
c	  IF(KSE.EQ.1) COEF=-1.
c	ELSEIF(KSR.EQ.2) THEN
c	  IF(KSE.EQ.2) COEF=-1.
c	ENDIF
		
	IF(EPSA2.GE.(0.00001)) THEN
	  IF(EPSA2.LT.EPSA) THEN
	    PLIN=PMAX*(1.-EPSR/(2.*EPSA2))*EPSR
		SIG=COEF*PLIN*EPSA2**2/(12.*SMOD)
	    GOTO 1
	  ENDIF
	ENDIF
	PLIN=PMAX*(1.-EPSR/(2.*EPSA))*EPSR
	SIG=COEF*PLIN*EPSA**2/(12.*SMOD)

    1	CONTINUE

c	Sensibilités des contraintes normales dues à la flexion locale des lisses
c	-------------------------------------------------------------------------
	IF(IOPTI.GE.1) THEN
	  CALL ANNULD(dPLIN,9)
	  IF(EPSA2.GE.(0.00001)) THEN
	    IF(EPSA2.LT.EPSA) THEN
	      dPLIN(9)=PMAX*((-1./(2.*EPSA2))*EPSR+(1.-EPSR/(2.*EPSA2)))
		  DO I=1,9
		    dSIG(I)=(COEF*EPSA2**2/12.)*
     *			    ((dPLIN(I)*SMOD-PLIN*dSMOD(I))/SMOD**2)
		  ENDDO
	      GOTO 2
	    ENDIF
	  ENDIF
	  dPLIN(5)=PMAX*EPSR*EPSR/(2*EPSA**2)
	  dPLIN(9)=PMAX*((-1./(2.*EPSA))*EPSR+(1.-EPSR/(2.*EPSA))*1.)
	  DO I=1,9
	    dSIG(I)=(COEF*EPSA**2/12.)*
     *            ((dPLIN(I)*SMOD-PLIN*dSMOD(I))/SMOD**2)
	    IF(I.EQ.5) dSIG(I)=dSIG(I)+(2.*COEF*EPSA/12.)*(PLIN/SMOD)
	  ENDDO
    2	  CONTINUE
	ENDIF

c	*****************************************************************************************************

      RETURN
      END
