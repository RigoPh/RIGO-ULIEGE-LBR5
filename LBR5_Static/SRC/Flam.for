
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
	 sur=AIRE*1.0E+4  !en cm²
	 SIG=SIGY*1.0E-6   !en n/mm²
	 WY=WYA*1.0E+2   !en cm
	 HY=HYA*1.0E+2
	 aVM1= EPSA*VM1*1.0E-3  !en kN.m
	 aVM2=EPSA*VM2*1.0E-3  !en kN.m
	 aE=E*1.0E-6   ! en N/mm²
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
c	 CFLU1=CFL1*1.0E+6	!en N/m²										
       IF (EULER2.LE.FL1) THEN									
	 CFL2=EULER2									
	 ELSE									
	 CFL2=FL3										
	 ENDIF
c	 CFLU2=CFL2*1.0E+6			!en N/m²		
	 IF(CFL1.GE.SIG) THEN
	 CFLU1 = SIG
	 ENDIF
	 IF(CFL2.GE.SIG) THEN
	 CFL2 = SIG
	 ENDIF						
C	 WRITE(66,*) 'La contrainte de flambement VB si biappuyé vaut'
C    *			,CFLU2 
c	 WRITE(66,31) CFLU1,CFLU2,Sy   
c	 WRITE(66,*) SIGY
c	 Compression pure
	  SOL=Sy*1.0E-6  !en N/mm²
	  RES1=CFL1/(1.02*1.25) 
	  RES2=CFL2/(1.02*1.25) 
	 IF(VMyMAX.LE.100)THEN
	 WRITE(66,31) RES1,RES2,Sol   

	  IF (RES1.LE.SOL) THEN								
	  WRITE(66,32) 
	  WRITE(29,32)																!bug
c	  WRITE(66,*) 'IL Y AURA FLAMBEMENT (biencastré)'
	 ELSE
	  WRITE(66,33) 
c	  WRITE(66,*) ' le FLAMBEMENT (biencastré) n''est pas à craindre'
	  ENDIF
	  IF (RES2.LE.SOL) THEN								
	  WRITE(66,34) 
	  WRITE(29,34)																!bug 
c	  WRITE(66,*) 'IL Y AURA FLAMBEMENT(biappuyé)'
	 ELSE
	  WRITE(66,35) 
c	  WRITE(66,*) ' le FLAMBEMENT (biappuyé) n''est pas à craindre'
	  ENDIF				
	 ELSE  ! VMyMAX .GT. 100
c	 Flexion composée 
	 
	  F= VNyMAX*1.0E-3	!en KN
	  U1=0.5*PI*DSQRT(F/(EULER1*SUR))
c	  U2=0.5*PI*DSQRT(F/(EULER2*SUR))		! dans le cas biappuyé, les M d'extrémité sont nuls.
C	  U1=0.5*PI*DSQRT(SOL/EULER1)
C	  U2=0.5*PI*DSQRT(SOL/EULER2)
	  T1=((aVM2-aVM1)/(aVM2+aVM1))/DTAN(U1)
c	  T2=((aVM2-aVM1)/(aVM2+aVM1))/DTAN(U2)
	  VM01=(0.5*(aVM1+aVM2)*DSQRT(1+(T1*T1)))/DCOS(U1)
c	  VM02=(0.5*(aVM1+aVM2)*DSQRT(1+(T2*T2)))/DCOS(U2)
	  VMMM1=DMAX1(ABS(aVM1),ABS(aVM2),ABS(VM01))
c	  VMMM2=DMAX1(ABS(aVM1),ABS(aVM2),ABS(VM02))
c	excentricité
C	EX= excentricité en cm
C	EXC1= 1/(1-(F/(SUR*EULER1)))
C	EXC2= 1/(1-(F/(SUR*EULER2)))
	  IF (ISECT.EQ.3) THEN
	  WP=aX*2/WY
	  ELSE
	  WP= aX*2/HY
	  ENDIF
	  SOLM1=(VMMM1/WP)*1000   !biencastré
c	  SOLM2=(VMMM2/WP)*1000	  !biappuyé
	  SOLS=10*F*(1/SUR)  !en kN/cm²?
C	  SOLS1 = 10*F*((1/SUR)+(EXC1*EX/WP))	!si excentricité
C	  SOLS2 = 10*F*((1/SUR)+(EXC2*EX/WP))
	  SOL1=SOLS+SOLM1	
	  SOL2=SOLS			
	  RES=SIG/(1.02*1.15) 
		WRITE(66,38) SOL1,SOL2,RES		

	  IF (RES.LE.SOL1) THEN								
	  WRITE(66,32) 
	  WRITE(29,32)																!bug
c	  WRITE(66,*) 'IL Y AURA FLAMBEMENT (biencastré)'
	 ELSE
	  WRITE(66,33) 
c	  WRITE(66,*) ' le FLAMBEMENT (biencastré) n''est pas à craindre'
	  ENDIF
	  IF (RES.LE.SOL2) THEN								
	  WRITE(66,34) 
	  WRITE(29,34)																!bug 
c	  WRITE(66,*) 'IL Y AURA FLAMBEMENT(biappuyé)'
	 ELSE
	  WRITE(66,35) 
c	  WRITE(66,*) ' le FLAMBEMENT (biappuyé) n''est pas à craindre'
	  ENDIF				
	 ENDIF
C	****************************************************************									
c	ELSE    ! CTICM
	 AIX=AIX*1.0E+12		!en mm^4
	 AIRE= AIRE*1.0E+6  !en mm²
c	 SIG=SIGY*1.0E-6	!N/mm²
	 aSY=SY*1.0E-6  !N/mm²
	 HI=HI*1000   ! mm
c	 E=E*1.0E-06		!N/mm²
	 RAYG= DSQRT(AIX/AIRE)
	 ELAN1=0.5*HI/RAYG		!biencastré
	 ELAN2=HI/RAYG	    	!biappuyé
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
	 SbRd1= CKHI1*SIG/1.1   !résistance
c	 SbRdU1=SbRd1*1.0E+6   !en  N/m² 
c	WRITE(66,*)'La contrainte de flambement CTICM si biencastré vaut'
c    *				,SbRdU1
	 PHI2= 0.5*(1+ALF*(ELANB2-0.2)+ELANB2*ELANB2)
	 CKHI2= 1/(PHI2+DSQRT(PHI2*PHI2-ELANB2*ELANB2))
	 SbRd2= CKHI2*SIG/1.1
c	 SbRdU2=SbRd2*1.0E+6  !en N/m²
	IF(SbRd1.GE.SIG) THEN
	SbRd1= SIG
	ENDIF
	IF(SbRd2.GE.SIG) THEN
	SbRd2= SIG
	ENDIF

c	 WRITE(66,*) SIGY
	 IF(VMyMAX.GE.100)THEN
C	 Flambement en flexion composée selon le CTICM (eurocode3)
	 COND1=(VNyMAX*1.1)/(AIRE*CKHI1*SIG)
	 COND2=(VNyMAX*1.1)/(AIRE*CKHI2*SIG)
       IF (((ELANB1.LT.0.2).OR.(COND1.LT.0.1)).AND.((ELANB2.LT.0.2).OR.
     *            (COND2.LT.0.1))) THEN
       WRITE(66,37) 
c	 WRITE(66,*) ' le FLAMBEMENT en flexion composée
c    *				 n''est pas à craindre'
	 GOTO 23
	 ELSE IF ((ELANB2.GT.0.2).AND.(COND2.GT.0.1))THEN		!biappuyé
			IF((ELANB1.LT.0.2).OR.(COND1.LT.0.1)) THEN		!biencastré OK
			WRITE(66,40) 
			Write(29,40)																!bug
	 	 WRITE(66,36) SbRd1,SbRd2,aSy		!en N/mm²

c	 WRITE(66,*) ' le FLAMBEMENT en flexion composée biencastrée
c    *				 n''est pas à craindre'
			GOTO 24
			ELSE IF ((ELANB1.GT.0.2).AND.(COND1.GT.0.1)) THEN !biencastré KO
			 WRITE(66,39) 
		     WRITE(29,39)																!bug
	 	 WRITE(66,36) SbRd1,SbRd2,aSy		!en N/mm²

			GOTO 25
			ENDIF
		 ELSE IF ((ELANB1.GT.0.2).AND.(COND1.GT.0.1).AND.
     *  (ELANB2.LT.0.2).AND.(COND2.LT.0.1)) THEN		!biappuyé
		WRITE(66,*) 'BIZARRE, flamb biencastré et pas biappuyé'
	 	 WRITE(66,36) SbRd1,SbRd2,aSy		!en N/mm²


	 ENDIF
	ENDIF
 
 	 WRITE(66,36) SbRd1,SbRd2,aSy		!en N/mm²
c	 Flambement en compression selon le CTICM (eurocode3)
  25	 IF(SbRd1.LE.aSy)THEN
	  WRITE(66,32) 
	  WRITE(29,32)																!bug 
c	 WRITE(66,*)'IL Y AURA FLAMBEMENT (biencastré)'
	 ELSE
	  WRITE(66,33) 
c	  WRITE(66,*) ' le FLAMBEMENT (biencastré) n''est pas à craindre'
	ENDIF

  24	 IF(SbRd2.LE.aSy)THEN
	  WRITE(66,34) 
	  WRITE(29,34)																!bug 
c	 WRITE(66,*)'IL Y AURA FLAMBEMENT (biappuyé)'
	 ELSE
	  WRITE(66,35) 
c	  WRITE(66,*) ' le FLAMBEMENT (biappuyée) n''est pas à craindre'
	 ENDIF
  23   CONTINUE	
c	ENDIF
C***********************************************************************
C***********************************************************************
C	  LES FORMATS
C     ------------
  31	FORMAT(/'selon le BUREAU VERITAS'/
     *		'La Résistance au flambement si biencastré vaut',E11.4,
     *'N/mm²'/
     *		'La Résistance au flambement si biappuyé vaut',E11.4,
     *'N/mm²'/
     *		'et la Sollicitation appliquée vaut',E11.4,'N/mm²')
 32   FORMAT('donc on aura FLAMBEMENT dans le cas biencastré')
 33   FORMAT('donc le flambement n''est pas à craindre dans le cas'
     *				'biencastré')
 34   FORMAT('donc on aura FLAMBEMENT dans le cas biappuyé'/)
 35   FORMAT('donc le flambement n''est pas à craindre dans le cas' 
     *				'biappuyé'/)
  36	FORMAT(/'selon le CTICM (eurocode3)'/
     *		'La Résistance au flambement si biencastré vaut',E11.4,
     *'N/mm²'/
     *		'La Résistance au flambement si biappuyé vaut',E11.4,
     *'N/mm²'/
     *		'et la Sollicitation appliquée vaut',E11.4,'N/mm²')
 37   FORMAT(/'selon le CTICM (eurocode3)'/
     *		' il n''est pas nécessaire de calculer le flambement'
     *		' dans ce cas de flexion composée.'/) 
  38	FORMAT(/'selon le BUREAU VERITAS'/
     *		'La Sollicitation à considérer si biencastré vaut',E11.4,
     *'N/mm²'/
     *		'et la Sollicitation appliquée si biappuyée vaut',E11.4,
     *'N/mm²'/
     *		'et la Résistance au flambement vaut',E11.4,'N/mm²'/)
  39	FORMAT(/'Selon le CTICM,'/
     *		'Les conditions sont remplies, il y a risque de 
     * flambement'/
     *		'Le flambement est calculé comme dans le cas de la 
     * compression pure')
  40  FORMAT(/'Selon le CTICM,'/
     *		'Les conditions sont remplies, il y a risque de 
     * flambement pour le cas biappuyé'/
     *		'Le flambement est calculé comme dans le cas de la 
     * compression pure')
	RETURN
	END
