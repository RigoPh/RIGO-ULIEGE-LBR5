
      SUBROUTINE FLAMB(ITYPE,HIGHT,AIRE,AIx,SIGY,HYA,ISECT,
     *					TYA,WYA,E,VNyMAX,VMyMAX,SyMAX,SyMIN,
     *					VM1,VM2,EPSA)							

      IMPLICIT REAL*8(A-H,O-Z)
	REAL*8 AIX
      COMMON/PY/PI
	INTEGER*2 ISECT
C	CHARACTER *7 SECTION
 
	  IF(SyMIN.GT.SyMAX)THEN
	  Sy=SyMIN
	  ELSE
	  Sy=SyMAX
	  ENDIF
        HI = HIGHT
	VNyMAX = EPSA*VNyMAX				
	VMyMAX = EPSA*VMyMAX				
C	**************************************************
c	IF (FLAM.EQ.1) THEN
c	  Flambement selon Bureau Veritas
	 aX=AIX*1.0E+8  !en cm^4
	 sur=AIRE*1.0E+4  !en cm�
	 SIG=SIGY*1.0E-6   !en n/mm�
	 WY=WYA*1.0E+2   !en cm
	 HY=HYA*1.0E+2
	 aVM1= EPSA*VM1*1.0E-3  !en kN.m
	 aVM2=EPSA*VM2*1.0E-3  !en kN.m
	 aE=E*1.0E-6   ! en N/mm�
	 EULER1= PI*PI*aE*(ax/(sur*(0.5*HI)**2))/10000	!BIENCASTRE	
	 EULER2= PI*PI*aE*(ax/(sur*(HI)**2))/10000		!BIAPPUYE
	 FL1= SIG/2											
	 FL2= SIG*(1-(SIG/(4*EULER1)))					
       FL3= SIG*(1-(SIG/(4*EULER2)))					
	 IF (EULER1.LE.FL1) THEN									
	 CFL1=EULER1									
	 ELSE									
	 CFL1=FL2										
	 ENDIF
c	 CFLU1=CFL1*1.0E+6	!en N/m�										
       IF (EULER2.LE.FL1) THEN									
	 CFL2=EULER2									
	 ELSE									
	 CFL2=FL3										
	 ENDIF
c	 CFLU2=CFL2*1.0E+6			!en N/m�		
	 IF(CFL1.GE.SIG) THEN
	 CFLU1 = SIG
	 ENDIF
	 IF(CFL2.GE.SIG) THEN
	 CFL2 = SIG
	 ENDIF						
C	 WRITE(66,*) 'La contrainte de flambement VB si biappuy� vaut'
C    *			,CFLU2 
c	 WRITE(66,31) CFLU1,CFLU2,Sy   
c	 WRITE(66,*) SIGY
c	 Compression pure
	  SOL=Sy*1.0E-6  !en N/mm�
	  RES1=CFL1/(1.02*1.25) 
	  RES2=CFL2/(1.02*1.25) 
	 IF(VMyMAX.LE.100)THEN
	 WRITE(66,31) RES1,RES2,Sol   

	  IF (RES1.LE.SOL) THEN								
	  WRITE(66,32) 
	  WRITE(29,32)																!bug
c	  WRITE(66,*) 'IL Y AURA FLAMBEMENT (biencastr�)'
	 ELSE
	  WRITE(66,33) 
c	  WRITE(66,*) ' le FLAMBEMENT (biencastr�) n''est pas � craindre'
	  ENDIF
	  IF (RES2.LE.SOL) THEN								
	  WRITE(66,34) 
	  WRITE(29,34)																!bug 
c	  WRITE(66,*) 'IL Y AURA FLAMBEMENT(biappuy�)'
	 ELSE
	  WRITE(66,35) 
c	  WRITE(66,*) ' le FLAMBEMENT (biappuy�) n''est pas � craindre'
	  ENDIF				
	 ELSE  ! VMyMAX .GT. 100
c	 Flexion compos�e 
	 
	  F= VNyMAX*1.0E-3	!en KN
	  U1=0.5*PI*DSQRT(F/(EULER1*SUR))
c	  U2=0.5*PI*DSQRT(F/(EULER2*SUR))		! dans le cas biappuy�, les M d'extr�mit� sont nuls.
C	  U1=0.5*PI*DSQRT(SOL/EULER1)
C	  U2=0.5*PI*DSQRT(SOL/EULER2)
	  T1=((aVM2-aVM1)/(aVM2+aVM1))/DTAN(U1)
c	  T2=((aVM2-aVM1)/(aVM2+aVM1))/DTAN(U2)
	  VM01=(0.5*(aVM1+aVM2)*DSQRT(1+(T1*T1)))/DCOS(U1)
c	  VM02=(0.5*(aVM1+aVM2)*DSQRT(1+(T2*T2)))/DCOS(U2)
	  VMMM1=DMAX1(ABS(aVM1),ABS(aVM2),ABS(VM01))
c	  VMMM2=DMAX1(ABS(aVM1),ABS(aVM2),ABS(VM02))
c	excentricit�
C	EX= excentricit� en cm
C	EXC1= 1/(1-(F/(SUR*EULER1)))
C	EXC2= 1/(1-(F/(SUR*EULER2)))
	  IF (ISECT.EQ.3) THEN
	  WP=aX*2/WY
	  ELSE
	  WP= aX*2/HY
	  ENDIF
	  SOLM1=(VMMM1/WP)*1000   !biencastr�
c	  SOLM2=(VMMM2/WP)*1000	  !biappuy�
	  SOLS=10*F*(1/SUR)  !en kN/cm�?
C	  SOLS1 = 10*F*((1/SUR)+(EXC1*EX/WP))	!si excentricit�
C	  SOLS2 = 10*F*((1/SUR)+(EXC2*EX/WP))
	  SOL1=SOLS+SOLM1	
	  SOL2=SOLS			
	  RES=SIG/(1.02*1.15) 
		WRITE(66,38) SOL1,SOL2,RES		

	  IF (RES.LE.SOL1) THEN								
	  WRITE(66,32) 
	  WRITE(29,32)																!bug
c	  WRITE(66,*) 'IL Y AURA FLAMBEMENT (biencastr�)'
	 ELSE
	  WRITE(66,33) 
c	  WRITE(66,*) ' le FLAMBEMENT (biencastr�) n''est pas � craindre'
	  ENDIF
	  IF (RES.LE.SOL2) THEN								
	  WRITE(66,34) 
	  WRITE(29,34)																!bug 
c	  WRITE(66,*) 'IL Y AURA FLAMBEMENT(biappuy�)'
	 ELSE
	  WRITE(66,35) 
c	  WRITE(66,*) ' le FLAMBEMENT (biappuy�) n''est pas � craindre'
	  ENDIF				
	 ENDIF
C	****************************************************************									
c	ELSE    ! CTICM
	 AIX=AIX*1.0E+12		!en mm^4
	 AIRE= AIRE*1.0E+6  !en mm�
c	 SIG=SIGY*1.0E-6	!N/mm�
	 aSY=SY*1.0E-6  !N/mm�
	 HI=HI*1000   ! mm
c	 E=E*1.0E-06		!N/mm�
	 RAYG= DSQRT(AIX/AIRE)
	 ELAN1=0.5*HI/RAYG		!biencastr�
	 ELAN2=HI/RAYG	    	!biappuy�
c	 EE= E/SIG
	 EE= aE/SIG
	 ELANE= PI*DSQRT(EE)
	 ELANB1=ELAN1/ELANE
	 ELANB2=ELAN2/ELANE


	 IF(ISECT.EQ.3)THEN
	 CLASS=2*(HYA+TYA)/WYA
	 IF(CLASS - 1.2) 21,20,20
 20	 ALF=0.339
	 GOTO 22
 21	 ALF=0.489
	 GOTO 22
	 ELSE
	 ALF=0.206
	 ENDIF
 22	 CONTINUE
	 PHI1= 0.5*(1+ALF*(ELANB1-0.2)+ELANB1*ELANB1)
	 CKHI1= 1/(PHI1+DSQRT(PHI1*PHI1-ELANB1*ELANB1))
	 SbRd1= CKHI1*SIG/1.1   !r�sistance
c	 SbRdU1=SbRd1*1.0E+6   !en  N/m� 
c	WRITE(66,*)'La contrainte de flambement CTICM si biencastr� vaut'
c    *				,SbRdU1
	 PHI2= 0.5*(1+ALF*(ELANB2-0.2)+ELANB2*ELANB2)
	 CKHI2= 1/(PHI2+DSQRT(PHI2*PHI2-ELANB2*ELANB2))
	 SbRd2= CKHI2*SIG/1.1
c	 SbRdU2=SbRd2*1.0E+6  !en N/m�
	IF(SbRd1.GE.SIG) THEN
	SbRd1= SIG
	ENDIF
	IF(SbRd2.GE.SIG) THEN
	SbRd2= SIG
	ENDIF

c	 WRITE(66,*) SIGY
	 IF(VMyMAX.GE.100)THEN
C	 Flambement en flexion compos�e selon le CTICM (eurocode3)
	 COND1=(VNyMAX*1.1)/(AIRE*CKHI1*SIG)
	 COND2=(VNyMAX*1.1)/(AIRE*CKHI2*SIG)
       IF (((ELANB1.LT.0.2).OR.(COND1.LT.0.1)).AND.((ELANB2.LT.0.2).OR.
     *            (COND2.LT.0.1))) THEN
       WRITE(66,37) 
c	 WRITE(66,*) ' le FLAMBEMENT en flexion compos�e
c    *				 n''est pas � craindre'
	 GOTO 23
	 ELSE IF ((ELANB2.GT.0.2).AND.(COND2.GT.0.1))THEN		!biappuy�
			IF((ELANB1.LT.0.2).OR.(COND1.LT.0.1)) THEN		!biencastr� OK
			WRITE(66,40) 
			Write(29,40)																!bug
	 	 WRITE(66,36) SbRd1,SbRd2,aSy		!en N/mm�

c	 WRITE(66,*) ' le FLAMBEMENT en flexion compos�e biencastr�e
c    *				 n''est pas � craindre'
			GOTO 24
			ELSE IF ((ELANB1.GT.0.2).AND.(COND1.GT.0.1)) THEN !biencastr� KO
			 WRITE(66,39) 
		     WRITE(29,39)																!bug
	 	 WRITE(66,36) SbRd1,SbRd2,aSy		!en N/mm�

			GOTO 25
			ENDIF
		 ELSE IF ((ELANB1.GT.0.2).AND.(COND1.GT.0.1).AND.
     *  (ELANB2.LT.0.2).AND.(COND2.LT.0.1)) THEN		!biappuy�
		WRITE(66,*) 'BIZARRE, flamb biencastr� et pas biappuy�'
	 	 WRITE(66,36) SbRd1,SbRd2,aSy		!en N/mm�


	 ENDIF
	ENDIF
 
 	 WRITE(66,36) SbRd1,SbRd2,aSy		!en N/mm�
c	 Flambement en compression selon le CTICM (eurocode3)
  25	 IF(SbRd1.LE.aSy)THEN
	  WRITE(66,32) 
	  WRITE(29,32)																!bug 
c	 WRITE(66,*)'IL Y AURA FLAMBEMENT (biencastr�)'
	 ELSE
	  WRITE(66,33) 
c	  WRITE(66,*) ' le FLAMBEMENT (biencastr�) n''est pas � craindre'
	ENDIF

  24	 IF(SbRd2.LE.aSy)THEN
	  WRITE(66,34) 
	  WRITE(29,34)																!bug 
c	 WRITE(66,*)'IL Y AURA FLAMBEMENT (biappuy�)'
	 ELSE
	  WRITE(66,35) 
c	  WRITE(66,*) ' le FLAMBEMENT (biappuy�e) n''est pas � craindre'
	 ENDIF
  23   CONTINUE	
c	ENDIF
C***********************************************************************
C***********************************************************************
C	  LES FORMATS
C     ------------
  31	FORMAT(/'selon le BUREAU VERITAS'/
     *		'La R�sistance au flambement si biencastr� vaut',E11.4,
     *'N/mm�'/
     *		'La R�sistance au flambement si biappuy� vaut',E11.4,
     *'N/mm�'/
     *		'et la Sollicitation appliqu�e vaut',E11.4,'N/mm�')
 32   FORMAT('donc on aura FLAMBEMENT dans le cas biencastr�')
 33   FORMAT('donc le flambement n''est pas � craindre dans le cas'
     *				'biencastr�')
 34   FORMAT('donc on aura FLAMBEMENT dans le cas biappuy�'/)
 35   FORMAT('donc le flambement n''est pas � craindre dans le cas' 
     *				'biappuy�'/)
  36	FORMAT(/'selon le CTICM (eurocode3)'/
     *		'La R�sistance au flambement si biencastr� vaut',E11.4,
     *'N/mm�'/
     *		'La R�sistance au flambement si biappuy� vaut',E11.4,
     *'N/mm�'/
     *		'et la Sollicitation appliqu�e vaut',E11.4,'N/mm�')
 37   FORMAT(/'selon le CTICM (eurocode3)'/
     *		' il n''est pas n�cessaire de calculer le flambement'
     *		' dans ce cas de flexion compos�e.'/) 
  38	FORMAT(/'selon le BUREAU VERITAS'/
     *		'La Sollicitation � consid�rer si biencastr� vaut',E11.4,
     *'N/mm�'/
     *		'et la Sollicitation appliqu�e si biappuy�e vaut',E11.4,
     *'N/mm�'/
     *		'et la R�sistance au flambement vaut',E11.4,'N/mm�'/)
  39	FORMAT(/'Selon le CTICM,'/
     *		'Les conditions sont remplies, il y a risque de 
     * flambement'/
     *		'Le flambement est calcul� comme dans le cas de la 
     * compression pure')
  40  FORMAT(/'Selon le CTICM,'/
     *		'Les conditions sont remplies, il y a risque de 
     * flambement pour le cas biappuy�'/
     *		'Le flambement est calcul� comme dans le cas de la 
     * compression pure')
	RETURN
	END
