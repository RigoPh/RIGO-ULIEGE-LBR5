      SUBROUTINE SBEGA(TEXT,NVAR,NXIT,NETO,
     *                 MEGA,EGA,NVS,NXI2,NEGAL,NEGALT,NGmax,IMPR2)		!15.10.05

      IMPLICIT REAL *8(A-H,O-Z)
      DIMENSION NVAR(NETO),NXIT(9,NETO),TEXT(15)


      DIMENSION  MEGA(NGmax,4),EGA(NGmax),
     *           NVS(NETO),NXI2(9,NETO),NEGAL(NETO)  ! utilis� dans  ENT,CONTR,OPTI
C     COMMON /OPTI4/ BIDON(800),INV(400),NEGALT,    
c    *               MEGA(100,4),EGA(100),NVS(30),NXI2(9,30),NEGAL(30)

C***********************************************************************
C     SUBROUTINE SBEGA
c     =================
C	Cette subroutine est relative aux restrictions d'�galit�s [XI1= Kcst * XI2 ]
C     ---------------------------------------------------------------------------
C     MAXIMUM 100 RESTRICTIONS D'EGALITE
C
C 	Donn�es : NVAR(NEL)	    Nbre de variables de conception pour le panneau NEL
C               NXIT(9,NEL)   Liste des variables de conception (1 � 9) du panneau NEL
c
C	Lecture : NEGALT	     Nbre de contr. d'�galit� pour toute la structure	
C		      MEGA(NEGALT,4) XI1= Kcst * XI2  avec XI1 du panneau NEL1 
C                                                    XI2 du panneau NEL2
C                                                    Kste = XI1/XI2 = constante (real)
C		       - MEGA(J,1)  XI1(=1�9), le n� de r�f. de la var. XI1 (var. d�pendant)
C                             ex: pour Haig, MEGA(J,1)=2 
C                                 pour EPSR, MEGA(J,1)=9
C		       - MEGA(J,2)  n� du panneau de cette variable XI1
C		       - MEGA(J,3)  XI2(=1�9), le n� de r�f. de la var. XI2 (var. ind�pendant)
C		       - MEGA(J,4)  n� du panneau de cette variable XI2
C
C		      EGA(J)		la constante = XI1/XI2 
c
C     Calcul� : NEGAL(NEL)	Nbre de restr. d'�galit� pour le panneau NEL
C		      NVS(NEL)	Nbre de restr. d'�galit� pour les panneaux 1 � NEL-1    	
C
C       NXI2(9,NEL)  N� d'ordre des variables XI (1�9) du panneau NEL
C                    au niveau global de la structure (cad pour les neto panneaux)
C                    si = 0 : signifie que ce n'est pas une variable de conception.
C
c     Cr�er   :  4- 9-95 
C
C     Modifi� : 19- 3-97	  Test de v�rification sur l'existance des XI choisis
C                6-12-99				             
C***********************************************************************


C     Impressions des donn�es
C     -----------------------
      WRITE(666,2) TEXT

        WRITE(666,*)' Nbr de restr. d''�galit�=',NEGALT
	  IF ((IMPR2.GE.-2).AND.(NEGALT.NE.0)) THEN					!15.10.05
        WRITE(666,*)
        WRITE(666,*)' N�    XI1  NEL1    XI2  NEL2    XI1/XI2'

	  DO 4 I=1,NEGALT
	    WRITE(666,5)I,(MEGA(I,K),K=1,4),EGA(I)
    4   CONTINUE
	  ENDIF								!15.10.05
	

C     V�rifications des donn�es
C     ---------------------------
        IT=0
        IXI=0

        DO 3 I=1,NEGALT
        IF(MEGA(I,2).EQ.MEGA(I,4))THEN
          IF(MEGA(I,1).EQ.MEGA(I,3))THEN
          WRITE(*,*) 'Error 1 ds les donn�es des contraintes d''�galit�'
          WRITE(666,*)'Error1 ds les donn�es des contraintes d''�galit�'
          WRITE(666,*) 'Egalit� n�',I
          WRITE(666,*) 'MEGA(I,1�4)=',(MEGA(I,KK),KK=1,4)
 
          WRITE(29,*)'Error1 ds les donn�es des contraintes d''�galit�'				!bug	
          WRITE(29,*) 'Egalit� n�',I													!bug
          WRITE(29,*) 'MEGA(I,1�4)=',(MEGA(I,KK),KK=1,4)								!bug

          PAUSE 'STOP'
          STOP
	  ENDIF	
	ENDIF

        IF(MEGA(I,2).LT.IT)THEN
          WRITE(*,*)  'Error 2 : contraintes d''�galit� mal class�es'
          WRITE(666,*)'Error 2 : contraintes d''�galit� mal class�es'
          WRITE(666,*) 'Egalit� n�',I
          WRITE(666,*) 'MEGA(I,1�4)=',(MEGA(I,KK),KK=1,4)
          
		WRITE(29,*)  'Error 2 : contraintes d''�galit� mal class�es'				!bug
          WRITE(29,*) 'Egalit� n�',I													!bug
          WRITE(29,*) 'MEGA(I,1�4)=',(MEGA(I,KK),KK=1,4)								!bug
          
		PAUSE 'STOP'
          STOP
        ENDIF

        IF(MEGA(I,2).EQ.IT)THEN
          IF(MEGA(I,1).LE.IXI)THEN
          WRITE(*,*)   'Error 3: contraintes d''�galit� mal class�es'
          WRITE(666,*) 'Error 3: contraintes d''�galit� mal class�es'
          WRITE(666,*) 'Egalit� n�',I
          WRITE(666,*) 'MEGA(I,1�4)=',(MEGA(I,KK),KK=1,4)

	    WRITE(29,*) 'Error 3: contraintes d''�galit� mal class�es'					!bug
          WRITE(29,*) 'Egalit� n�',I													!bug
          WRITE(29,*) 'MEGA(I,1�4)=',(MEGA(I,KK),KK=1,4)								!bug

            PAUSE 'STOP'
            STOP
          ENDIF
	  ENDIF
	
	  IT=MEGA(I,2)	
	  IXI=MEGA(I,1)		

    3 CONTINUE


C       CALCUL DE NEGAL(NEL)
C     --------------------
      DO 6 NEL=1,NETO
        NEGAL(NEL)=0
          DO 13 I=1,NEGALT
          IF((MEGA(I,2)).EQ.NEL) NEGAL(NEL)=NEGAL(NEL)+1
   13   CONTINUE
    6 CONTINUE

          
C       CALCUL DE  NVS(NEL)
C     ---------------------
      NVS(1)=0
        DO 7 NEL=2,NETO
          NVS(NEL)=NVS(NEL-1)+NEGAL(NEL-1)
    7 CONTINUE


C	CALCUL DE  NXI2(9,NEL)
C     ---------------------
	CALL ANNULI(NXI2,270)
	
	L=1
	DO 8 NEL=1,NETO
	   DO 9 K=1,9
	      DO 10 I=1,NVAR(NEL)
                IF((NXIT(I,NEL)).EQ.K) THEN
                       NXI2(K,NEL)=L
                         L=L+1
                       GOTO 11
                 ENDIF
               IF((NXIT(I,NEL)).GT.K) GOTO 11
   10       CONTINUE
   11    CONTINUE
    9    CONTINUE
    8 CONTINUE


C       V�rification de l'existance des variables
C     -----------------------------------------
        DO 15 I=1,NEGALT

      IC=NXI2(MEGA(I,1),MEGA(I,2)) * NXI2(MEGA(I,3),MEGA(I,4))
      IF(IC.EQ.0)THEN
        WRITE(*,*) 'Error : Restriction d''�galit� n�',I,' incorrecte'
        WRITE(*,*) '        car faisant intervenir une variable ',
     *                     'de conception non selectionn�e'
        WRITE(666,*) 'Error : Restriction d''�galit� n�',I,' incorrecte'
        WRITE(666,*) '        car faisant intervenir une variable ',
     *                       'de conception non selectionn�e'
        WRITE(29,*) 'Error : Restriction d''�galit� n�',I,' incorrecte'				!bug
        WRITE(29,*) '        car faisant intervenir une variable ',					!bug
     *                       'de conception non selectionn�e'							!bug

        PAUSE 'STOP'
        STOP
      ENDIF
  15  CONTINUE


c     Impressions de NEGAL,NVS et NXI2
C     ----------------------------------
	IF(IMPR2.GE.-2) THEN					!15.10.05
      WRITE(666,*)
      WRITE(666,*)'NEL,NEGAL, NVS,     NXI2'
	
      DO 12 NEL=1,NETO
        WRITE(666,'(I3,I4,I3,T14,9I4)')
     *   NEL,NEGAL(NEL),NVS(NEL),(NXI2(I,NEL),I=1,9)
   12 CONTINUE	
	ENDIF									!15.10.05

      RETURN

C     ---------------------------------------------------------------      
  900 WRITE(*,*) 'Error � la lecture des restrictions d''�galit�'
	WRITE(29,*) 'Error � la lecture des restrictions d''�galit�'					!bug
      PAUSE 'STOP'
      STOP
	
C	Formats
C     ========
    2 FORMAT(/' DONNEES RELATIVES AUX RESTRICTIONS D''EGALITE'/
     *  T2,50(1H*)/15A4/)
    5 FORMAT(1X,I4,3X,I2,3X,I2,6X,I2,3X,I2,5X,E14.7)
      END
