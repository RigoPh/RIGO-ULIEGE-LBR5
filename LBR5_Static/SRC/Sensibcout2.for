      SUBROUTINE SENSIBCOUT2(NETO,OBJ,NVAR,XI,NXIT,SPEC,ITT,WIDTH,ISECT,		!18.03.04
     *                        CORRO)											!18.03.04 + !corrosion

      IMPLICIT REAL*8(A-H,O-Z)
	DIMENSION XI(1),NXIT(9,NETO),SPEC(1),NVAR(1)
	DIMENSION TRAV(217235)												!18.03.04	!extension neto
	DIMENSION ISECT(neto)												!18.03.04
	DIMENSION CORRO(NETO,3)			!corrosion 
	COMMON/COUT/ Poids,SPoids,
     *        Fmat,Fsou,FMdO,Fmat11,Fmat22,Fmat33,Fsou11,Fsou22,
     *        Fsou33,FMdO11,FMdO22,FMdO33,FMdO44,FMdO55,FMdO66,FMdO77,
     *        REND,EQP,Dref,DrefX,DrefY,C1,C2,C3,DC1,DW2,DW3,P10,DP10,
     *        P4,P5,DP4,DP5,P9X,P9Y,DP9X,DP9Y,BER,BET,P6,P7,C8,DC8,COUT,
     *        ICOUT,IALR,IALT
	COMMON/OPTI/  IOPTI,NN

	I1 = 1             !VLARG(NETO)
	I2 = I1 + NETO     !VEPAISS(NETO)
	I3 = I2 + NETO     !VHAC(NETO)
	I4 = I3 + NETO     !VEAC(NETO)
	I5 = I4 + NETO     !VLSC(NETO)
	I6 = I5 + NETO     !VESC(NETO)
	I7 = I6 + NETO     !DELTAC(NETO)
	I8 = I7 + NETO     !VHAR(NETO)
	I9 = I8 + NETO     !VEAR(NETO)
	I10 = I9 + NETO    !VLSR(NETO)
	I11 = I10 + NETO   !VESR(NETO)
	I12 = I11 + NETO   !DELTAR(NETO)
	I13 = I12 + NETO   !ENTR(NETO) 
	I14 = I13 + NETO   !PHIL(NETO)
	I15 = I14 + NETO   !Q(NETO)
	I16 = I15 + NETO   !CoutPAN(NETO)
	I17 = I16 + NETO   !VDIFFC(NETO)				!18.03.04
	I18	= I17 + NETO	!vecteur de travail pour COSTMAIN (214685)	!18.03.04	!extension neto

C Total = 17*NETO + 214685			!18.03.04	!extension neto
C       = 2550    + 214685			!18.03.04	!extension neto
C       = 217235						!18.03.04	!extension neto


	CALL SENSIB2(NETO,OBJ,NVAR,XI,NXIT,SPEC,ITT,WIDTH,ISECT,				!18.03.04
     * TRAV(I1), TRAV(I2), TRAV(I3), TRAV(I4), TRAV(I5), TRAV(I6),
     * TRAV(I7), TRAV(I8), TRAV(I9),TRAV(I10),TRAV(I11),TRAV(I12),
     * TRAV(I13),TRAV(I14),TRAV(I15),TRAV(I16),TRAV(I17),TRAV(I18),			!18.03.04
     * NN,CORRO)															!corrosion 																															!corrosion
      RETURN
	END

c ====================================================================
	SUBROUTINE SENSIB2(NETO,OBJ,NVAR,XI,NXIT,SPEC,ITT,WIDTH,ISECT,		!18.03.04
     * VLARG,VEPAISS,VHAC,VEAC,VLSC,VESC,
     * DELTAC,VHAR,   VEAR,VLSR,VESR,DELTAR,
     * ENTR,  PHIL,   Q,   CoutPAN,VDIFFC,TRAV,NN,CORRO)				!18.03.04  +  !corrosion 	

      
	IMPLICIT REAL*8(A-H,O-Z)
	
      DIMENSION XI(9*NETO),NXIT(9,NETO),NVAR(NETO)
 	DIMENSION ISECT(neto)												!18.03.04
      DIMENSION VLARG(NETO),VEPAISS(NETO),VHAC(NETO),
     * VEAC(NETO),VLSC(NETO),VESC(NETO),DELTAC(NETO),VHAR(NETO),
     * VEAR(NETO),VLSR(NETO),VESR(NETO),DELTAR(NETO),ENTR(NETO),
     * PHIL(NETO),Q(NETO),CoutPAN(NETO),DERIVEE(NN),VDIFFC(NETO),		!18.03.04
     * DCOR(NETO),DCOR2(NETO),DCOR3(NETO),SPEC(NETO),TRAV(1)

      DIMENSION CORRO(NETO,3)			!corrosion 

	COMMON/PY/PI
	COMMON/COUT/ Poids,SPoids,
     *        Fmat,Fsou,FMdO,Fmat11,Fmat22,Fmat33,Fsou11,Fsou22,
     *        Fsou33,FMdO11,FMdO22,FMdO33,FMdO44,FMdO55,FMdO66,FMdO77,
     *        REND,EQP,Dref,DrefX,DrefY,C1,C2,C3,DC1,DW2,DW3,P10,DP10,
     *        P4,P5,DP4,DP5,P9X,P9Y,DP9X,DP9Y,BER,BET,P6,P7,C8,DC8,COUT,
     *        ICOUT,IALR,IALT
	
	

C******************************************************************************
C     SUBROUTINE SENSIBCOUT2
C     ======================
C     Cette sous-routine est appelée dans OPTIS.
C     Elle recalcule la fonction objectif après être passé dans le module
C     d'optimisation (cfr SENSIBCOUT, mais sans calcul des sensibilités).
C
C INPUTS : NETO : nombre de panneaux de la structure.
C          NVAR : vecteur stockant les numéros des variables de conception utilisées.
C          SPEC : poids spécifique (N/m3) (à spécifier pour chaque panneau)
C          WIDTH : longueur totale de la structure
C          ITT : paramètre d'option d'impression des résultats
C          XI : valeurs des variables de conception après optimisation
C          
C          
C OUTPUTS : OBJ : coût total de fabrication de la structure.
C      
C Créer: F. Bair (Juin 2003)
C
C Modifications:
c  - juin 2003
c
c Dernière mise à jour: 16 juin 2003 (Ph. rigo)      
c					  18 mars 2004 (J.Pirard)         
C
C******************************************************************************
C******************************************************************************
C******************************************************************************
      I1=0
      NCONT = 0

	REWIND 305		!extension neto
	
	READ(305,*) VLENGTH		!extension neto

	DO I=1,NETO

      READ(305,*) NOPAN,VLARG1,VEPAISS1													!extension neto
	READ(305,*) VHAC1,VEAC1,VLSC1,VESC1,DELTAC1,VDIFFC1					!18.03.04		!extension neto
	READ(305,*) VHAR1,VEAR1,VLSR1,VESR1,DELTAR1,ENTR(i)									!extension neto

      READ(302) KSA,KSR,KSE,DELTAC(i),DELTAR(i),VEPAISS(i),VHAC(i),	!extension neto
     *          VEAC(i),VLSC(i),VESC(i),VHAR(i),VEAR(i),VLSR(i),
     *		  VESR(i),PHIL(i),Q(i),VDIFFC(i)						!18.03.04

      VLARG(i)=PHIL(i)*Q(i)*PI/180.

      NBRXI=NVAR(I)
      DO 123 I1=1,NBRXI
	IF(ISECT(I).EQ.0) THEN												!18.03.04
	GOTO(124,125,120,126,127,128,121,129,130),NXIT(I1,I)
  124 VEPAISS(i)=XI(I1+NCONT)
      GOTO 123
  125 VHAC(i)=XI(I1+NCONT)
      GOTO 123
  120 VEAC(i)=XI(I1+NCONT)
      GOTO 123
  126 VLSC(i)=XI(I1+NCONT)
      GOTO 123
  127 DELTAC(i)=XI(I1+NCONT)
      GOTO 123
  128 VHAR(i)=XI(I1+NCONT)
      GOTO 123
  121 VEAR(i)=XI(I1+NCONT)
      GOTO 123
  129 VLSR(i)=XI(I1+NCONT)
      GOTO 123
  130 DELTAR(i)=XI(I1+NCONT)
	ELSE													!18.03.04
	GOTO(134,135,133,136,137),NXIT(I1,I)					!18.03.04
  134 VHAC(i)=XI(I1+NCONT)									!18.03.04
      GOTO 123												!18.03.04
  135 VEAC(i)=XI(I1+NCONT)									!18.03.04
      GOTO 123												!18.03.04
  133 VLSC(i)=XI(I1+NCONT)									!18.03.04
      GOTO 123												!18.03.04
  136 VDIFFC(i)=XI(I1+NCONT)									!18.03.04
      GOTO 123												!18.03.04
  137 DELTAC(i)=XI(I1+NCONT)									!18.03.04
      GOTO 123												!18.03.04
	ENDIF													!18.03.04			
  123 CONTINUE

C     Epaisseur de corrosion
c     -----------------------
C     DCOR = epaisseur de corrosion pour bordé
	DCOR(i)  = CORRO(i,1)				!corrosion
	DCOR2(i) = CORRO(i,2)				!corrosion
	DCOR3(i) = CORRO(i,3)				!corrosion

c	IF(I.EQ.1) THEN
c	  REWIND 57
c	  READ(57,'(////,A1)') Abidon
c	  READ(57,*) IPAN,DCOR(i),DCOR2(i),DCOR3(i)
c	ELSE
c	  READ(57,*) IPAN,DCOR(i),DCOR2(i),DCOR3(i)
c	ENDIF

	IF(ISECT(I).EQ.0) THEN										!18.03.04
	VEPAISS(i) = VEPAISS(i) + DCOR(i)
	ELSE														!18.03.04
	VEPAISS(i) = 0.0000											!18.03.04
	ENDIF														!18.03.04
c	DCOR2   ! epaisseur de corrosion pour cadres
	VEAC(i)  = VEAC(i) + DCOR2(i)
	VESC(i)  = VESC(i) + DCOR2(i)
      VDIFFC(i)	= VDIFFC(i)	 + DCOR2(i)							!18.03.04
c	DCOR3   ! epaisseur de corrosion pour lisses
	VEAR(i)  = VEAR(i) + DCOR3(i)
	VESR(i)  = VESR(i) + DCOR3(i)

c	IF(IPRINT.GE.1) THEN
c       WRITE(666,*) 'dans SensibCout2'
c       WRITE(666,*) 'PANNEAU Nø ',i
c       WRITE(666,*) 'NBRXI= ',NBRXI,' et  NXI =',(NXIT(II,I),II=1,NBRXI)
c       WRITE(666,*) 'Bord = ',VEPAISS(i)
c       WRITE(666,*) 'Cadre= ',VHAC(i),VEAC(i),VLSC(i),VESC(i),' EPSA=',
c      *            DELTAC(i)
c       WRITE(666,*) 'Raid = ',VHAR(i),VEAR(i),VLSR(i),VESR(i),'  EPSR=',
c      *            DELTAR(i)
c	  PAUSE'OBJ2'
c	ENDIF

C     Impressions (avec epaiss corrosion)
C     -----------
      IF(ITT.EQ.1) THEN
	 IF(ISECT(I).EQ.0) THEN												!18.03.04
        WRITE(666,132)I,'Ep. Bord','Hâme Cad','Eâme Cad','Lsem Cad',
     *      	'Tsem Cad','EPSA Cad','Hâme Rai','Eâme Rai','Lsem Rai',
     *        'Tsem Rai','EPSR Rai'  
        WRITE(666,131)VEPAISS(i),VHAC(i),VEAC(i),VLSC(i),VESC(i),
     *           DELTAC(i),VHAR(i),VEAR(i),VLSR(i),VESR(i),DELTAR(i)
	 ELSEIF(ISECT(I).EQ.3) THEN											!18.03.04
        WRITE(666,142)I,'Hâme Epo','Eâme Epo','Lsem Epo',					!18.03.04
     *      	'Tsem Epo','EPSA Epo','Hâme Rai','Eâme Rai','Lsem Rai',		!18.03.04
     *        'Tsem Rai','EPSR Rai'										!18.03.04
        WRITE(666,141)VHAC(i),VEAC(i),VLSC(i),VESC(i),					!18.03.04
     *           DELTAC(i),VHAR(i),VEAR(i),VLSR(i),VESR(i),DELTAR(i)		!18.03.04
	 ELSE																!18.03.04
        WRITE(666,152)I,'Diam Epo','Epai Epo','EPSA Epo',					!18.03.04
     *		'Hâme Rai','Eâme Rai','Lsem Rai','Tsem Rai','EPSR Rai'		!18.03.04
        WRITE(666,151)VHAC(i),VDIFFC(i),									!18.03.04
     *           DELTAC(i),VHAR(i),VEAR(i),VLSR(i),VESR(i),DELTAR(i)		!18.03.04
	 ENDIF																!18.03.04		
	ENDIF

      NCONT = NCONT + NBRXI

	ENDDO	! fin boucle sur panneaux

	REWIND(305)		!extension neto
    
c ===============================================
      Fmat = 0.0

C     Calcul de Fmat= Coût des matériaux (acier)
      DO I=1,NETO
	 TEMP = PHIL(i) * Q(i) * WIDTH * PI/180.
       DENS = SPEC(I)/9.81
	IF(ISECT(I).EQ.0) THEN														!18.03.04
	 Fmat1 = TEMP * DENS * 
     *  (C1 *(1.+(VEPAISS(i)-Dref)*1000.*DC1) * VEPAISS(i) 
     * + C2 *(1.+(VEAR(i)  -Dref)*1000.*DC1) * (1.0+DW2)   ! Dref et pas DrefX
     *                   * (VEAR(i)*VHAR(i)+VESR(i)*VLSR(i))/DELTAR(i) 
     * + C3 *(1.+(VEAC(i)  -Dref)*1000.*DC1) * (1.0+DW3)   ! Dref et pas DrefY
     *                   * (VEAC(i)*VHAC(i)+VESC(i)*VLSC(i))/DELTAC(i) ) 
	 ELSEIF(ISECT(I).EQ.1) THEN													!27.05.04
	 Fmat1 = TEMP * DENS *													!27.05.04
c     *  ( C2 *(1.+(VEAR(i)   -Dref)*1000.*DC1) * (1.0+DW2)   ! Dref et pas DrefX	!05.12.05
c     *                   * (VEAR(i)*VHAR(i)+VESR(i)*VLSR(i))/DELTAR(i)			!05.12.05
     * + C3 *(1.+(VEAC(i)   -Dref)*1000.*DC1) * (1.0+DW3)   ! Dref et pas DrefY	!27.05.04
     *   *PI*(VHAC(i)*VHAC(i)-(VHAC(i)-2*VDIFFC(i))**2) /(4*DELTAC(i))!)			!05.12.05
	 ELSEIF(ISECT(I).EQ.2) THEN													!27.05.04
	 Fmat1 = TEMP * DENS *													!27.05.04
c     *  ( C2 *(1.+(VEAR(i)   -Dref)*1000.*DC1) * (1.0+DW2)   ! Dref et pas DrefX	!05.12.05
c     *                   * (VEAR(i)*VHAR(i)+VESR(i)*VLSR(i))/DELTAR(i)			!05.12.05
     * + C3 *(1.+(VEAC(i)   -Dref)*1000.*DC1) * (1.0+DW3)   ! Dref et pas DrefY	!27.05.04
     *   *(VHAC(i)*VHAC(i)-(VHAC(i)-2*VDIFFC(i))**2) /DELTAC(i)!)					!05.12.05
	 ELSEIF(ISECT(I).EQ.3) THEN													!27.05.04
	 Fmat1 = TEMP * DENS *													!27.05.04
c     *  ( C2 *(1.+(VEAR(i)   -Dref)*1000.*DC1) * (1.0+DW2)   ! Dref et pas DrefX	!05.12.05
c     *                   * (VEAR(i)*VHAR(i)+VESR(i)*VLSR(i))/DELTAR(i)			!05.12.05
     * + C3 *(1.+(VEAC(i)   -Dref)*1000.*DC1) * (1.0+DW3)   ! Dref et pas DrefY	!27.05.04
     *   *(VEAC(i)*2* VHAC(i)+2*VESC(i)* VLSC(i)) /DELTAC(i)!)						!05.12.05
	ENDIF																		!18.03.04
      
	 Fmat = Fmat + Fmat1
c       WRITE (666,*) 'Panel',I,'cout MAT=',Fmat1
	ENDDO


C     Calcul Coût main d'oeuvre (COUTRRO et CoutPAN en RRO)

c      NN = 800 ! pas d'importance ici...
	CALL COSTMAIN(COUTRRO,VLARG,VEPAISS,VHAC,VEAC,VLSC,
     * VESC,DELTAC,VHAR,VEAR,VLSR,VESR,DELTAR,ENTR,VLENGTH,NETO,NON,I1,
     * CoutPAN,DERIVEE,NN,NVAR,NXIT,TRAV,totalTI)      


      OBJ = Fmat + COUTRRO*EQP*C1*1000
      
c     WRITE (666,*) 'Total cout MAT=',Fmat
c	DO I=1,NETO
c       WRITE (666,*) 'Panel',I,'COUTPAN=',COUTPAN(I)*EQP*C1*1000
c	ENDDO
c       WRITE (666,*) 'Total COUT PAN=',COUTRRO*EQP*C1*1000

      RETURN
c -----------------------------------------------------------------
  131 FORMAT('Variables_:',11(E11.4))
  132 FORMAT('PANEL n°',I2,2X,11(1X,A8,2X))
  141 FORMAT('Variables_:',10(E11.4))							!18.03.04
  142 FORMAT('PANEL n°',I2,2X,10(1X,A8,2X))					!18.03.04
  151 FORMAT('Variables_:',8(E11.4))							!18.03.04
  152 FORMAT('PANEL n°',I2,2X,8(1X,A8,2X))					!18.03.04
 	
	END