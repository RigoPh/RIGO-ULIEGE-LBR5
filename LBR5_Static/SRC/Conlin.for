      SUBROUTINE CONLIN (S,MS,IUIN,IOP,NON,NETO,VNOM2,NVAR,NXIT,M1TABL,	!octobre2003	!dcn
     *                   M2CONT,IGRAV,IRESTR,NSOL,IWEIGHT,IPRICE,			!fev 2004	 !restri poids	 !restri cout
     *				   INERT,IMOD)										!restri inertie   !restri module
C     **********************************************************
      IMPLICIT REAL*8 (A-H,O-Z)				
      DIMENSION S(1)
      CHARACTER *12 VNOM2(1)

      DIMENSION NVAR(NETO),NXIT(9,NETO)	         !  COMMON OPTI
c	DIMENSION M1TABL(1),M2CONT(1)                !octobre2003	!dcn

C     Earlier Versions (V.0, V.1, V.1.5)  
C     VERSION UTILISE par LBR-5. Ph. Rigo, 24 Nov. 1995
C       - avec modification des tests sur IS (dimension max)
C
C   Dernière modification:  
C          : 10 juin 1996
C          : 1-08-96 - formatage des READ(IUIN,*)
C          :10-06-98 - Nbre max d'itération porté à 100 au lieu de  40
C          :18-06-03 - Nbre max d'itération porté à 200 au lieu de 100
C                       (voir Subr. SCP0 : MAX =200)
c          :19-2-2004
c		 :26-01-2006 - Nbre max d'itérations porté à 500 au lieu de 200	!multiobj
C
C
C     METH > 5  ===> New CONLIN        (CONVEX)
C     ========
C
      READ (IUIN,*) N,M   				
C     N     | NUMBER OF DESIGN VARIABLES        |  I
C     M     | NUMBER OF DESIGN CONSTRAINTS      |  I
C
C     Implementation of Arrays in S(MS)
C     ---------------------------------
C
      L = MAX0 (N,M) + 1
      I1 =1
      I2 =I1+N
      I3 =I2+N
      I4 =I3+N
      I5 =I4+N
      I6 =I5+N
      I7 =I6+L
      I8 =I7+M
      I9 =I8+L
      I10=I9+M
      I11=I10+M
      I12=I11+N*M					!		NEW !
      IS =I12+3*N
C     IS =8*N+3*M+N*M+2*MAX(N,M)+2
C     IS = 145.102   si N=200 et M=700  (NETO=30
C     IS = 813.202   si N=400 et M=2000 (NETO=50)
c     IS =6258.000   si N=800 et M=7000 (NETO=100)


      IF(IS.GT.MS) GOTO 901			!		NEW !
c      WRITE (666,*) 'IS,MS=',IS,MS
c      PAUSE'CONL1'
C      
      CALL V2V1 (S(I1),S(I2),S(I3),S(I4),S(I5),
     *           S(I6),S(I7),S(I8),S(I9),S(I10),S(I11),	
     *           S(IS),												! Modif 
     *           N,M,IUIN,IOP,NON,NETO,VNOM2,NVAR,NXIT,				!octobre2003
     *           M1TABL,M2CONT,IGRAV,IRESTR,NSOL,IWEIGHT,IPRICE,		!octobre2003	!restri poids   !restri cout	
     *		   INERT,IMOD)											!restri inertie   !restri module
      GOTO 900
C
 901  WRITE (666,291) IS,MS ! taille memoire insuffisante --> stop
	WRITE (29 ,291) IS,MS														!bug
      NON=900
C
 900  RETURN
 291  FORMAT('Error - Not enough memory for CONLIN V.1 ',I5,'>',I5)
      END

C========================================================================
C========================================================================
      
      SUBROUTINE V2V1 (XI,IX,XIMIN,XIMAX,FI,
     *                 RJ,JR,CJ,CJMAX,DCJ,CIJ,
     *                 S,
     *                 N,M,IUIN,IOP,NON,NETO,VNOM2,NVAR,NXIT,				!octobre2003
     *                 M1TABL,M2CONT,IGRAV,IRESTR,NSOL,IWEIGHT,IPRICE,    !octobre2003	!restri poids   !restri cout	
     *				 INERT,IMOD)										!restri inertie  !restri module	
C
C     CONLIN Version 1.5 - Pure Dual Approach
C     =======================================
C
      IMPLICIT REAL*8 (A-H,O-Z)

      COMMON/DIM1/Nmax,NEmax,NVmax,
     *            M1max,M1Tmax,M2max,M2Tmax,Mmax,NGmax,ISmax,IPTmax ! dimension max des vecteurs

      CHARACTER *12 VNOM2(M)
      DIMENSION NVAR(NETO),NXIT(9,NETO)	        	    !  COMMON OPTI

      DIMENSION XI(N),IX(N),XIMIN(N),XIMAX(N),FI(N),
     *          RJ(M),JR(M),CJ(M),CJMAX(M),DCJ(M),
     *          CIJ(N,M),S(3*N)

      DIMENSION VNOM(9),VNOM3(5)												!18.03.04
	DIMENSION M1TABL(Nmax,ISmax),M2CONT(NETO)     !octobre2003	!dcn
      DATA VNOM/'Ep. Bord','Hâme Cad','Eâme Cad','Lsem Cad','EPSA Cad',
     *      	  'Hâme Rai','Eâme Rai','Lsem Rai','EPSR Rai'/
      DATA VNOM3/'Hâme Epo','Eâme Epo','Lsem Epo','Ep.  Epo','EPSA Epo'/		!18.03.04

      READ (IUIN,*,END=900) TOL,CUT,PREC,PDS
      READ (IUIN,'(9E14.7)',END=900) XI
      READ (IUIN,'(9E14.7)',END=900) XIMIN
      READ (IUIN,'(9E14.7)',END=900) XIMAX
      READ (IUIN,*,END=900) JOBJ
      READ (IUIN,'(9E14.7)',END=900) OBJ
      READ (IUIN,'(9E14.7)',END=900) FI
C
c     IF(IOP.GE.3) WRITE (666,62) TOL,CUT,PREC,PDS

      IF(IOP.GE.3) THEN
        WRITE (*,61) 
        K=0
        DO 14 NEL=1,NETO
        IF(NVAR(NEL).NE.0) THEN
          K2=K+NVAR(NEL)
          WRITE (*,10) NEL,(VNOM(NXIT(I,NEL)),I=1,NVAR(NEL))
          WRITE (*, 3) (XIMIN(I),I=K+1,K2)
          WRITE (*,44) (XI(I),I=K+1,K2)
          WRITE (*, 5) (XIMAX(I),I=K+1,K2)
          WRITE (666,9) NEL,(VNOM(NXIT(I,NEL)),I=1,NVAR(NEL))
          WRITE (666,3) (XIMIN(I),I=K+1,K2)
          WRITE (666,4) (XI(I),I=K+1,K2)
          WRITE (666,5) (XIMAX(I),I=K+1,K2)
          K=K2
        ENDIF
   14   CONTINUE
      ENDIF

C     Impressions de la Fct objectif.
      WRITE (*  ,63) OBJ,JOBJ
      WRITE (666,*) 
      WRITE (666,63) OBJ,JOBJ
      WRITE (666,*) 

C     Impressions des dérivées de la fct objectif			
      K=0
      IF(IOP.LE.2) GO TO 15
      DO 13 NEL=1,NETO
        IF(NVAR(NEL).NE.0) THEN
          K2=K+NVAR(NEL)
          WRITE (666,6) (FI(I),I=K+1,K2)
	    K=K2
        ENDIF
   13 CONTINUE
      WRITE (666,*) 
   15 CONTINUE

C     Lecture et Impressions des restrictions et de leurs dérivées	
      MNC=0
      ME=0
      IF(IOP.GE.2) THEN
        WRITE (*,65)
        WRITE (666,65)
      ENDIF

      J1=0
c      DO 2 J=1,M                   ! tous ces commentaires : !octobre2003
c       READ (IUIN,*,END=900) JT		
c       READ (IUIN,'(9E14.7)',END=900) CJ(J),CJMAX(J),DCJ(J),RJ(J)
cc       IF(IOP.GE.3) WRITE (*,'(A,I5,2(A,E15.6),2A)')	
cc     *   'C(',J,')=',CJ(J),' ?<? ',CJMAX(J),': Type = ',VNOM2(J)
c       WRITE (666,'(A,I5,2(A,E15.6),2A)')	
c     *    'C(',J,')=',CJ(J),' ?<? ',CJMAX(J),': Type = ',VNOM2(J)
c       IF(JT.NE.0) MNC=MNC+1
c       IF(JT.LT.0) ME=ME+1
c       READ (IUIN,'(9E14.7)',END=900) (CIJ(I,J),I=1,N)
c      
cC     Impression des sensibilités
c       IF(IOP.LE.2) GO TO 2
c       K=0
c       DO 12 NEL=1,NETO
c         IF(NVAR(NEL).NE.0) THEN
c           K2=K+NVAR(NEL)
c           WRITE (666,7) (CIJ(I,J),I=K+1,K2)
c	     K=K2
c         ENDIF
c   12  CONTINUE
c   2  CONTINUE

c!!!!!!!!!!!!!!!!!!!!!!!!début !octobre2003 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      J=1
	DO II1=1,NETO  ! rest geom
	  DO II2=1,M2CONT(II1)
	    READ (IUIN,*,END=900) JT		
          READ (IUIN,'(9E14.7)',END=900) CJ(J),CJMAX(J),DCJ(J),RJ(J)
c          IF(IOP.GE.3) WRITE (*,'(A,I5,2(A,E15.6),2A)')	
c     *      'C(',J,')=',CJ(J),' ?<? ',CJMAX(J),': Type = ',VNOM2(J)
		IF (IMPR.GE.-1) THEN				!15.10.05
          WRITE (666,'(A,I5,2(A,E15.6),2A,3x,A,I3)')	
     *       'C(',J,')=',CJ(J),' ?<? ',CJMAX(J),': Type = ',VNOM2(J),
     *       'Panneau :',II1
		ENDIF				!15.10.05
          IF(JT.NE.0) MNC=MNC+1
          IF(JT.LT.0) ME=ME+1
          READ (IUIN,'(9E14.7)',END=900) (CIJ(I,J),I=1,N)  
C     Impression des sensibilités
          IF(IOP.GT.2) THEN
            K=0
            DO NEL=1,NETO
              IF(NVAR(NEL).NE.0) THEN
                K2=K+NVAR(NEL)
                WRITE (666,7) (CIJ(I,J),I=K+1,K2)
	          K=K2
              ENDIF
            ENDDO
	    ENDIF
	    J = J+1
	  ENDDO
	ENDDO

c ! *** centre de gravite
	DO II2=1,IGRAV
	    READ (IUIN,*,END=900) JT		
          READ (IUIN,'(9E14.7)',END=900) CJ(J),CJMAX(J),DCJ(J),RJ(J)
		IF (IMPR.GE.-1) THEN				!15.10.05
          WRITE (666,'(A,I5,2(A,E15.6),2A)')	
     *       'C(',J,')=',CJ(J),' ?<? ',CJMAX(J),': Type = ',VNOM2(J)
 		ENDIF				!15.10.05
          IF(JT.NE.0) MNC=MNC+1
          IF(JT.LT.0) ME=ME+1
          READ (IUIN,'(9E14.7)',END=900) (CIJ(I,J),I=1,N)  
C     Impression des sensibilités
          IF(IOP.GT.2) THEN
            K=0
            DO NEL=1,NETO
              IF(NVAR(NEL).NE.0) THEN
                K2=K+NVAR(NEL)
                WRITE (666,7) (CIJ(I,J),I=K+1,K2)
	          K=K2
              ENDIF
            ENDDO
	    ENDIF
	    J=J+1
	ENDDO

C Rest sur inertie		!restri inertie
      IF(INERT.NE.0) THEN
	  READ (IUIN,*,END=900) JT		
        READ (IUIN,'(9E14.7)',END=900) CJ(J),CJMAX(J),DCJ(J),RJ(J)
	  IF (IMPR.GE.-1) THEN				
        WRITE (666,'(A,I5,2(A,E15.6),2A)')	
     *     'C(',J,')=',CJ(J),' ?<? ',CJMAX(J),': Type = ',VNOM2(J)
 	  ENDIF			
        IF(JT.NE.0) MNC=MNC+1
        IF(JT.LT.0) ME=ME+1
        READ (IUIN,'(9E14.7)',END=900) (CIJ(I,J),I=1,N)  
C     Impression des sensibilités
        IF(IOP.GT.2) THEN
          K=0
          DO NEL=1,NETO
            IF(NVAR(NEL).NE.0) THEN
              K2=K+NVAR(NEL)
              WRITE (666,7) (CIJ(I,J),I=K+1,K2)
	        K=K2
            ENDIF
          ENDDO
	  ENDIF
	  J=J+1
	ENDIF				!restri inertie


C Rest sur module sectionnel		!restri module
      IF(IMOD.NE.0) THEN
	  READ (IUIN,*,END=900) JT		
        READ (IUIN,'(9E14.7)',END=900) CJ(J),CJMAX(J),DCJ(J),RJ(J)
	  IF (IMPR.GE.-1) THEN				
        WRITE (666,'(A,I5,2(A,E15.6),2A)')	
     *     'C(',J,')=',CJ(J),' ?<? ',CJMAX(J),': Type = ',VNOM2(J)
 	  ENDIF			
        IF(JT.NE.0) MNC=MNC+1
        IF(JT.LT.0) ME=ME+1
        READ (IUIN,'(9E14.7)',END=900) (CIJ(I,J),I=1,N)  
C     Impression des sensibilités
        IF(IOP.GT.2) THEN
          K=0
          DO NEL=1,NETO
            IF(NVAR(NEL).NE.0) THEN
              K2=K+NVAR(NEL)
              WRITE (666,7) (CIJ(I,J),I=K+1,K2)
	        K=K2
            ENDIF
          ENDDO
	  ENDIF
	  J=J+1
	ENDIF						!restri module


C Rest sur poids			!restri poids
      IF(IWEIGHT.NE.0) THEN
	  READ (IUIN,*,END=900) JT		
        READ (IUIN,'(9E14.7)',END=900) CJ(J),CJMAX(J),DCJ(J),RJ(J)
	  IF (IMPR.GE.-1) THEN				
        WRITE (666,'(A,I5,2(A,E15.6),2A)')	
     *     'C(',J,')=',CJ(J),' ?<? ',CJMAX(J),': Type = ',VNOM2(J)
 	  ENDIF			
        IF(JT.NE.0) MNC=MNC+1
        IF(JT.LT.0) ME=ME+1
        READ (IUIN,'(9E14.7)',END=900) (CIJ(I,J),I=1,N)  
C     Impression des sensibilités
        IF(IOP.GT.2) THEN
          K=0
          DO NEL=1,NETO
            IF(NVAR(NEL).NE.0) THEN
              K2=K+NVAR(NEL)
              WRITE (666,7) (CIJ(I,J),I=K+1,K2)
	        K=K2
            ENDIF
          ENDDO
	  ENDIF
	  J=J+1
	ENDIF				!restri poids

C Rest sur cout			!restri cout
      IF(IPRICE.NE.0) THEN
	  READ (IUIN,*,END=900) JT		
        READ (IUIN,'(9E14.7)',END=900) CJ(J),CJMAX(J),DCJ(J),RJ(J)
	  IF (IMPR.GE.-1) THEN				
        WRITE (666,'(A,I5,2(A,E15.6),2A)')	
     *     'C(',J,')=',CJ(J),' ?<? ',CJMAX(J),': Type = ',VNOM2(J)
 	  ENDIF			
        IF(JT.NE.0) MNC=MNC+1
        IF(JT.LT.0) ME=ME+1
        READ (IUIN,'(9E14.7)',END=900) (CIJ(I,J),I=1,N)  
C     Impression des sensibilités
        IF(IOP.GT.2) THEN
          K=0
          DO NEL=1,NETO
            IF(NVAR(NEL).NE.0) THEN
              K2=K+NVAR(NEL)
              WRITE (666,7) (CIJ(I,J),I=K+1,K2)
	        K=K2
            ENDIF
          ENDDO
	  ENDIF
	  J=J+1
	ENDIF				!restri cout

C Rest struc

	DO II1=1,NSOL   !Rest Struct
	  DO II2=1,NETO
	    DO II3=1,M1TABL(II2,II1)
            READ (IUIN,*,END=900) JT		
            READ (IUIN,'(9E14.7)',END=900) CJ(J),CJMAX(J),DCJ(J),RJ(J)
c            IF(IOP.GE.3) WRITE (*,'(A,I5,2(A,E15.6),2A)')	
c     *       'C(',J,')=',CJ(J),' ?<? ',CJMAX(J),': Type = ',VNOM2(J)
		IF (IMPR.GE.-1) THEN				!15.10.05
            WRITE (666,'(A,I5,2(A,E15.6),2A,2(3x,A,I3))')	
     *         'C(',J,')=',CJ(J),' ?<? ',CJMAX(J),': Type = ',VNOM2(J),
     *         'Panneau :',II2,'Cas de charge:',II1
		ENDIF								!15.10.05
            IF(JT.NE.0) MNC=MNC+1
            IF(JT.LT.0) ME=ME+1
            READ (IUIN,'(9E14.7)',END=900) (CIJ(I,J),I=1,N)
C     Impression des sensibilités
            IF(IOP.GT.2) THEN
              K=0
              DO NEL=1,NETO
                IF(NVAR(NEL).NE.0) THEN
                  K2=K+NVAR(NEL)
                  WRITE (666,7) (CIJ(I,J),I=K+1,K2)
	            K=K2
                ENDIF
              ENDDO
	      ENDIF
	      J = J+1
	    ENDDO
	  ENDDO
	ENDDO

c M ult 
c ! *** MOMENT ULTIME (Poutre navire)
      IF(IRESTR.NE.0) THEN
	DO II2=1,2
	    READ (IUIN,*,END=900) JT		
          READ (IUIN,'(9E14.7)',END=900) CJ(J),CJMAX(J),DCJ(J),RJ(J)
		IF (IMPR.GE.-1) THEN				!15.10.05
          WRITE (666,'(A,I5,2(A,E15.6),2A)')	
     *       'C(',J,')=',CJ(J),' ?<? ',CJMAX(J),': Type = ',VNOM2(J)
			ENDIF								!15.10.05
          IF(JT.NE.0) MNC=MNC+1
          IF(JT.LT.0) ME=ME+1
          READ (IUIN,'(9E14.7)',END=900) (CIJ(I,J),I=1,N)  
C     Impression des sensibilités
          IF(IOP.GT.2) THEN
            K=0
            DO NEL=1,NETO
              IF(NVAR(NEL).NE.0) THEN
                K2=K+NVAR(NEL)
                WRITE (666,7) (CIJ(I,J),I=K+1,K2)
	          K=K2
              ENDIF
            ENDDO
	    ENDIF
	    J=J+1
	ENDDO
	ENDIF

c     Liste des contraintes approchant la valeur maximale...
      WRITE (666,*)
      WRITE (666,'(6x,A/(6x,61(1H-)))')'Liste des contraintes approchant   
     * la valeur maximale autorisée (à 98%) (ou > -0.0001)'
	WRITE(666,*)
	J=1
	I=0
	DO II1=1,NETO     !restrictions géométriques
	  DO II2=1,M2CONT(II1)
	    IF (CJ(J).GT.0.98*CJMAX(J).OR.
     *                      (CJMAX(J).EQ.0.AND.CJ(J).GT.-0.0001)) THEN
	      IF (CJMAX(J).NE.0) THEN
	        POURC=(CJ(J)-CJMAX(J))/CJ(J)*100
              WRITE (666,'(A,I5,2(A,E15.6),2A,3x,A,I3,3x,A,F8.2)')	
     *         'C(',J,')=',CJ(J),' ?<? ',CJMAX(J),': Type = ',VNOM2(J),
     *         'Panneau :',II1,'% de diff.',POURC
	      ELSE !ça ne sert à rien d'écrire le %, c'est tjs 100%
			WRITE (666,'(A,I5,2(A,E15.6),2A,3x,A,I3)')	
     *         'C(',J,')=',CJ(J),' ?<? ',CJMAX(J),': Type = ',VNOM2(J),
     *         'Panneau :',II1
	      ENDIF
	      I=1
	    ENDIF
	    J = J+1
	  ENDDO
	ENDDO
	DO II2=1,IGRAV   !restriction sur la position du centre de gravité
	  IF (CJ(J).GT.0.98*CJMAX(J)) THEN
	    POURC=(CJ(J)-CJMAX(J))/CJ(J)*100
	    WRITE (666,'(A,I5,2(A,E15.6),2A,3x,A,I3)')							!03.02.06
     *         'C(',J,')=',CJ(J),' ?<? ',CJMAX(J),': Type = ',VNOM2(J),
     *         'IGRAV :',II2	
	    I=1
	  ENDIF
	  J=J+1
	ENDDO
	IF(INERT.NE.0) THEN		!restri inertie
	  IF (CJ(J).GT.0.98*CJMAX(J)) THEN
	    POURC=(CJ(J)-CJMAX(J))/CJ(J)*100
		WRITE (666,'(A,I5,2(A,E15.6),2A,3x,A,F8.2)')							
     *         'C(',J,')=',CJ(J),' ?<? ',CJMAX(J),': Type = ',VNOM2(J),
     *		 '% de diff.',POURC	
	    I=1
	  ENDIF
	  J=J+1
	ENDIF					!restri inertie
	IF(IMOD.NE.0) THEN		!restri module
	  IF (CJ(J).GT.0.98*CJMAX(J)) THEN
	    POURC=(CJ(J)-CJMAX(J))/CJ(J)*100
		WRITE (666,'(A,I5,2(A,E15.6),2A,3x,A,F8.2)')							
     *         'C(',J,')=',CJ(J),' ?<? ',CJMAX(J),': Type = ',VNOM2(J),
     *		 '% de diff.',POURC	
	    I=1
	  ENDIF
	  J=J+1
	ENDIF					!restri module
	IF(IWEIGHT.NE.0) THEN	!restri poids
	  IF (CJ(J).GT.0.98*CJMAX(J)) THEN
	    POURC=(CJ(J)-CJMAX(J))/CJ(J)*100
		WRITE (666,'(A,I5,2(A,E15.6),2A,3x,A,F8.2)')							
     *         'C(',J,')=',CJ(J),' ?<? ',CJMAX(J),': Type = ',VNOM2(J),
     *		 '% de diff.',POURC	
	    I=1
	  ENDIF
	  J=J+1
	ENDIF					!restri poids
	IF(IPRICE.NE.0) THEN	!restri cout
	  IF (CJ(J).GT.0.98*CJMAX(J)) THEN
	    POURC=(CJ(J)-CJMAX(J))/CJ(J)*100
		WRITE (666,'(A,I5,2(A,E15.6),2A,3x,A,F8.2)')							
     *         'C(',J,')=',CJ(J),' ?<? ',CJMAX(J),': Type = ',VNOM2(J),
     *		 '% de diff.',POURC	
	    I=1
	  ENDIF
	  J=J+1
	ENDIF					!restri cout
      DO II1=1,NSOL     !restrictions structurelles
	  DO II2=1,NETO
	    DO II3=1,M1TABL(II2,II1)
            IF (CJ(J).GT.0.98*CJMAX(J).OR.
     *                      (CJMAX(J).EQ.0.AND.CJ(J).GT.-0.0001)) THEN
	        IF (CJMAX(J).NE.0) THEN
			  POURC=(CJ(J)-CJMAX(J))/CJ(J)*100
                WRITE (666,'(A,I5,2(A,E15.6),2A,2(3x,A,I3),3x,A,F8.2)')	
     *           'C(',J,')=',CJ(J),' ?<? ',CJMAX(J),': Type = ',VNOM2(J)
     *           ,'Panneau :',II2,'Cas de charge:',II1,'% de diff.',
     *         POURC
	        ELSE !ça ne sert à rien d'écrire le %, c'est tjs 100%
                WRITE (666,'(A,I5,2(A,E15.6),2A,2(3x,A,I3))')	
     *           'C(',J,')=',CJ(J),' ?<? ',CJMAX(J),': Type = ',VNOM2(J)
     *           ,'Panneau :',II2,'Cas de charge:',II1
	        ENDIF
	        I=1
            ENDIF
	      J = J+1
	    ENDDO
	  ENDDO
	ENDDO
	IF(IRESTR.NE.0) THEN  !restrictions sur le moment ultime
	DO II2=1,2
	  IF (CJ(J).GT.0.98*CJMAX(J)) THEN
	    POURC=(CJ(J)-CJMAX(J))/CJ(J)*100
	    WRITE (666,'(A,I5,2(A,E15.6),2A,3x,A,I3,3x,A,F8.2)')	
     *       'C(',J,')=',CJ(J),' ?<? ',CJMAX(J),': Type = ',VNOM2(J),
     *       'Numéro :',II1,'% de diff.',POURC
	      I=1
	  ENDIF
	  J=J+1
	ENDDO
	ENDIF
	IF (I.EQ.0) WRITE(666,'(A)') 'Néant'
	WRITE(666,*)
	
      
c!!!!!!!!!!!!!!!!!!!!!!!!Fin !octobre2003 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      IF(IOP.GE.3) PAUSE 'pause'
      REWIND IUIN

      NITER=0
      I1 =1
      I2 =I1+N
      I3 =I2+N
c     PAUSE'CONL3'
      
      CALL SCP (XI,IX,XIMIN,XIMAX,FI,
     *          S(I1),S(I2),S(I3),				!	Modif!
     *          RJ,JR,CJ,CJMAX,DCJ,CIJ,
     *          TOL,CUT,PREC,OBJ,
     *          N,M,IACT,JACT,NITER,IOP,NON)

C     Write CONLIN Results 
C     ----------------------
      DELTA=0.

  58  IF(IOP.GE.2) THEN
        WRITE (*  ,59) 						!	Impression
        WRITE (666,*) 						!	  des
        WRITE (666,59)						!	résultats
        K=0
        DO 11 NEL=1,NETO
        IF(NVAR(NEL).NE.0) THEN
      	K2=K+NVAR(NEL)
          WRITE (*,10)NEL,(VNOM(NXIT(I,NEL)),I=1,NVAR(NEL)) !	Noms des XI
          WRITE (*,44) (XI(I),I=K+1,K2)					  ! Var. Concept.
          WRITE (666,9)NEL,(VNOM(NXIT(I,NEL)),I=1,NVAR(NEL))
          WRITE (666,4)(XI(I),I=K+1,K2)
	    K=K2
        ENDIF
  11    CONTINUE
        WRITE (666,60) OBJ  					!	Fct Objectif
        WRITE (*,  60) OBJ
      ENDIF
      
  57  IF(IOP.LE.3) GOTO 67
      ICO=0
      WRITE (*,*)
      WRITE (*,*) 'Pour modifier une variable XI ',
     *  "pour l'itération suivante: taper le nø de cette variable "
      WRITE (*,*) 'Sinon, taper 0 (par défaut)'
      READ  (*,*) ICO
      IF(ICO.LT.0) GOTO 57
      IF(ICO.GT.N) GOTO 57
      IF(ICO.NE.0) THEN
      WRITE (*,*) 'La variable nø ',ICO,'  est :',XI(ICO)
      WRITE (*,*) 'Quelle doit être sa nouvelle valeur ?'
      READ  (*,*) XI(ICO)
      WRITE (666,*) 
      WRITE (666,*) '!! La variable nø ',ICO,' modifiée est =',XI(ICO)
      WRITE (666,*) '   ----------------------'
      GOTO 58
      ENDIF
   67 CONTINUE
      
      RETURN
C
 900  WRITE (666,290) IUIN
	WRITE (29 ,290) IUIN															!bug
      NON=901
      RETURN
C
    3 FORMAT('Bornes Min:',9(E10.3,1X))
    4 FORMAT('Variables_:',9(E11.4))
   44 FORMAT('Variables_:',9(E10.3,1X))
    5 FORMAT('Bornes Max:',9(E10.3,1X))
    6 FORMAT('d Fct /dx.:',9(E10.3,1X))
    7 FORMAT('d C(j)/dx.:',9(E10.3,1X))
    9 FORMAT('PANEL nø',I2,2X,9(1X,A8,2X))
   10 FORMAT('PANEL nø',I2,2X,9(1X,A8,4X))
  59  FORMAT(' LES RESULTATS (variables indépendantes) :'/20(1H-)) 
  60  FORMAT(/'Fct objectif (approximation) = ',E14.7)
  61  FORMAT( ' LES DONNEES :'/20(1H-))
  62  FORMAT(/' LES DONNEES :(par.= ',4E11.4,')'/20(1H-))
  63  FORMAT('Fct objectif =',E14.7,'  (type = ',I2,')')
  64  FORMAT('Restrictions nø ',I3,' (type of approximation =',I2,')')
  65  FORMAT('Les restrictions'/15(1H-))
 290  FORMAT('Error in CONLIN data base  -  End of file on unit',I3)
      END
      
C ***********************************************************************      
C ***********************************************************************      
C ***********************************************************************      
      
      SUBROUTINE SCP (XI,IX,XIMIN,XIMAX,FI,AI,SI,X2,
     *                RJ,JR,CJ,CJBAR,CG,CIJ,
     *                TOL,CUT,PREC,OBJ,
     *                N,M,IACT,JACT,NITER,IOP,NON)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION XI(N),IX(N),XIMIN(N),XIMAX(N),FI(N),AI(N),SI(N),X2(N),
     *          RJ(M),JR(M),CJ(M),CJBAR(M),CG(M),CIJ(N,M)
C                 
C  ----------------------------------------------  I = INPUT
C
C     XI    | DESIGN VARIABLES                  |  I/O
C     IX    | DESIGN VARIABLE STATUS            |  Internal
C     XIMIN | LOWER BOUNDS ON DESIGN VARIABLES  |  I
C     XIMAX | UPPER BOUNDS ON DESIGN VARIABLES  |  I
C     FI    | OBJECTIVE FUNCTION GRADIENT       |  I
C     AI    | HESSIAN OF LAGRANGIAN FUNCTION    |  Internal 
C     SI    | SEARCH DIRECTION IN PRIMAL SPACE  |  Internal 
C     X2    | SQUARE OF DESIGN VARIABLES        |  Internal
C     RJ    | DUAL VARIABLES                    |  I/O
C     JR    | LIST OF ACTIVE CONSTRAINTS        |  Internal
C     CJ    | CONSTRAINT VALUES                 |  I
C     CJBAR | UPPER BOUNDS ON CONSTRAINTS       |  I
C     CG    | CONJUGATE GRADIENT IN DUAL SPACE  |  Internal
C     CIJ   | CONSTRAINT GRADIENTS              |  I
C     TOL   | TOLERANCE FOR CONVERGENCE         |  I
C     CUT   | CUT-OFF FACTOR                    |  I
C     PREC  | PRECISION                         |  I
C     OBJ   | OBJECTIVE FUNCTION VALUE          |  I/O
C     N     | NUMBER OF DESIGN VARIABLES        |  I
C     M     | NUMBER OF DESIGN CONSTRAINTS      |  I
C     IACT  | NUMBER OF FREE VARIABLES          |  O
C     JACT  | NUMBER OF ACTIVE CONSTRAINTS      |  O
C     NITER | NUMBER OF ITERATIONS PERFORMED    |  O
C     IOP   | PRINTING OPTION (0 TO 5)          |  I
C     NON   | ERROR INDICATOR (ò 899 ==> ERROR) |  O
C
C  ----------------------------------------------  O = OUTPUT
C
      ZERO=0.D 00
      UN  =1.D 00
      DEUX=2.D 00
      IF(TOL.LE.ZERO)  TOL=1.D-03
      IF(CUT.LE.UN)    CUT=DEUX
      IF(PREC.LE.ZERO) PREC=1.D-06
      IF(TOL.LT.PREC)  TOL=PREC
C
C     Verification of input data - Initialization
C     -------------------------------------------
C
      IF(IOP.GE.1) WRITE (666,200) 
      IF(IOP.GE.3) WRITE (666,201) TOL,CUT,PREC
C
      DO 3 I=1,N
      IX(I)=0
      IF(XI(I).LE.XIMIN(I)) IX(I)=-1
      IF(XI(I).GE.XIMAX(I)) IX(I)= 1
      IF(IOP.GT.2) WRITE (666,202) I,IX(I),XIMIN(I),XI(I),XIMAX(I),FI(I)
      IF(XIMIN(I).GT.XIMAX(I)) WRITE (666,291) I
      IF(XIMIN(I).GT.XIMAX(I)) WRITE (29 ,291) I										!bug
      OBJ=OBJ-DABS(FI(I))*XI(I)
      IF(FI(I)) 2,1,3
   1  WRITE (666,292) I
	WRITE (29 ,292) I																!bug
      FI(I)=PREC
      GOTO 3
   2  FI(I)=FI(I)*XI(I)*XI(I)
   3  CONTINUE
C
      IF(IOP.GT.3) WRITE (666,203)
C
      JACT=0
      DO 12 J=1,M
      JA=0
      IF(RJ(J).LE.ZERO) GOTO 4
      JACT=JACT+1
      JA=JACT
      JR(JACT)=J
      RJ(JACT)=RJ(J)
   4  ZZ=CJBAR(J)-CJ(J)
C
      IF(IOP.GT.3) WRITE (666,202) J,JA,CJ(J),CJBAR(J),RJ(J)
      IF(IOP.GE.6) WRITE (666,204) (CIJ(I,J),I=1,N)
C
      DO 6 I=1,N
      IF(CIJ(I,J).LT.ZERO) CIJ(I,J)=CIJ(I,J)*XI(I)*XI(I)
   6  CONTINUE
C
      Z0=PROMIX (XI,CIJ(1,J),N)
      ZZ=ZZ+Z0
C
      IF(ZZ.GT.ZERO) GOTO 8
      ZZ=-ZZ
      WRITE (666,293) J,ZZ
      WRITE (29 ,293) J,ZZ														!bug
      NON=902
      GOTO 12 
C
   8  DO 10 I=1,N
  10  CIJ(I,J)=CIJ(I,J)/ZZ
C
      IF(IOP.GE.6) WRITE (666,204) (CIJ(I,J),I=1,N)
  12  CJBAR(J)=UN
C
      IF(IOP.GE.3) WRITE (666,205) N,M
      IF(NON.GE.899) RETURN
C
C     Construction of Explicit Problem (change of variables)
C     ------------------------------------------------------
C
      DUMAX=OBJ
      DO 24 I=1,N
      SS=-FI(I)
      IF(SS.GE.ZERO) GOTO 16
      SS=-SS
C
      VAR=XIMIN(I)
      XIMIN(I)=UN/XIMAX(I)
      XIMAX(I)=UN/VAR
C
      DO 14 J=1,M
  14  CIJ(I,J)=-CIJ(I,J)
C
  16  XIMIN(I)=XIMIN(I)/SS
      XIMAX(I)=XIMAX(I)/SS
C
      DO 22 J=1,M
      IF(CIJ(I,J)) 18,22,20
  18  CIJ(I,J)=CIJ(I,J)/SS
      GOTO 22
  20  CIJ(I,J)=CIJ(I,J)*SS
  22  CONTINUE
C
  24  DUMAX=DUMAX+UN/XIMIN(I)
C
C     Newton iterations in the subspace of active constraints
C     -------------------------------------------------------

      CALL SCP0 (XI,IX,XIMIN,XIMAX,AI,SI,X2,
     *           RJ,JR,CJ,CJBAR,CG,CIJ,
     *           TOL,CUT,PREC,OBJ,DUMAX,
     *           N,M,IACT,JACT,NITER,IOP,NON)

C     Return back to initial primal variables
C     ---------------------------------------
      DO 26 I=1,N
      XI(I)=-XI(I)*FI(I)
      IF(FI(I).LE.ZERO) GOTO 26
      XI(I)=-UN/XI(I)
  26  CONTINUE

      OBJ=OBJ+PROMIX (XI,FI,N)

C     Print Final Results
C     -------------------
      IF(IOP.GE.3) WRITE (666,206) OBJ
      IF(IOP.GT.2) WRITE (666,207) (J,JR(J),RJ(J),CJ(J),J=1,JACT)
      IF(IOP.GT.3) WRITE (666,208) (I,XI(I),-IX(I),I=1,N)

      DO 28 J=1,M
      CJ(J)=RJ(J)
  28  RJ(J)=ZERO

      DO 30 J=1,JACT
      JU=JR(J)
  30  RJ(JU)=CJ(J)

 900  RETURN

 200  FORMAT(/T25,'===== CONLIN OPTIMIZER ====='/T30,18(1H=))
 201  FORMAT(' Tolerance for Convergence       TOL ',E15.6/
     *       ' Cut-off Factor                  CUT ',E15.6/
     *       ' Precision                       PREC',E15.6/
     *       ' Information on Design Variables'/1X,31(1H-)/
     *       '    I   IX          XIMIN',
     *       '             XI          XIMAX             FI'/
     *       1X,69(1H-))
 202  FORMAT(2I5,4F15.4)
 203  FORMAT(' Information on Design Constraints'/1X,33(1H-)/
     *       '    J JACT             CJ          CJBAR',
     *       '             RJ'/
     *       1X,54(1H-))
 204  FORMAT(' Constraint Gradient'/(8E10.2))
 205  FORMAT(' Number of Design Variables',T30,I5/
     *       ' Number of Design Constraints',T30,I5)
 206  FORMAT(' Objective Function (approximated)      ',E15.6)
 207  FORMAT(' Design Constraints (Dual Variable; Value)'/1X,18(1H-)//
     *       (2I5,2F15.6))
 208  FORMAT(' Design Variables (-1: Lower Bound; +1: Upper Bound)'/
     *       1X,16(1H-)//(4(I5,F10.4,' (',I2,')')))
 291  FORMAT(' Severe warning  -  ',
     *       ' Lower bound larger than upper bound',I5)
 292  FORMAT(' Warning  -  Objective function derivative is zero',I5)
 293  FORMAT(' No Feasible Solution to This Problem'//
     *       ' Upper bound on constraint',I5,' should be increased to',
     *       ' at least',E15.6)
      END
      
C========================================================================
C========================================================================
      
      SUBROUTINE SCP0 (XI,IX,XIMIN,XIMAX,AI,SI,X2,
     *                 RJ,JR,CJ,CJBAR,CG,CIJ,
     *                 TOL,CUT,PREC,OBJ,DUMAX,
     *                 N,M,IACT,JACT,NITER,IOP,NON)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION XI(N),IX(N),XIMIN(N),XIMAX(N),AI(N),SI(N),X2(N),
     *          RJ(M),JR(M),CJ(M),CJBAR(M),CG(M),CIJ(N,M)
C
      ZERO=0.D 00
      UN  =1.D 00
      DEUX=2.D 00
      NITER=0
c  MAX= Nbre maximun d'itération : valeur standard = 40 (version Fleury)
C (valeur modifié par Ph. rigo le 9-6-98 pour application barge FSO)
      MAX=500		!multiobj
      NLS=0
      IF(IOP.GT.2) WRITE (666,200)
C
C     Newton iterations in the subspace of active constraints
C     -------------------------------------------------------
C
C     1. Compute primal variables and constraints
C        ----------------------------------------
C
   1  PRIM=OBJ
      IACT=0
      DO 12 I=1,N
      CI=UN
      DI=ZERO
C
      IF(JACT.EQ.0) GOTO 10
      DO 6 J=1,JACT
      JU=JR(J)
      VAR=CIJ(I,JU)*RJ(J)
      IF(VAR) 2,6,4
C
   2  CI=CI-VAR
      GOTO 6
C
   4  DI=DI+VAR
   6  CONTINUE
C
C     Evaluation of primal variables
C
      IF(DI.EQ.ZERO) GOTO 10
      DI=DSQRT (CI/DI)
      IF(DI.LE.XIMIN(I)) GOTO 8
      IF(DI.GE.XIMAX(I)) GOTO 10
      IX(I)=0
      XI(I)=DI
      IACT=IACT+1
      GOTO 12
C
   8  IX(I)=-1
      XI(I)=XIMIN(I)
      GOTO 12
C
  10  IX(I)= 1
      XI(I)=XIMAX(I)
  12  PRIM=PRIM+UN/XI(I)
C
      DO 14 J=1,M
      CJ(J)=RJ(J)
  14  RJ(J)=ZERO
C
      IF(JACT.EQ.0) GOTO 18
      DO 16 J=1,JACT
      	JU=JR(J)
      	RJ(JU)=CJ(J)
  16  CONTINUE
C
C     Evaluation of primal constraints
C     *********************************
  18  DUAL=PRIM
      VMAX=ZERO

      DO 22 J=1,M 
      	VIO=PROMIX (XI,CIJ(1,J),N)-UN
      	CJ(J)=VIO
      	IF(RJ(J).LE.ZERO) GOTO 20
      	DUAL=DUAL+RJ(J)*VIO
      	IF(VIO.LT.ZERO) VIO=-VIO
  20  	IF(VIO.GT.VMAX) VMAX=VIO
  22  CONTINUE

      FAC=VMAX/CUT
      VIOL=ZERO
      JACT=0

      DO 26 J=1,M
      	VIO=CJ(J)
      	IF(RJ(J).GT.ZERO) GOTO 24
      	IF(VIO.LE.FAC) GOTO 26
  24  	JACT=JACT+1
      	JR(JACT)=J
      	CJ(JACT)=VIO
      	RJ(JACT)=RJ(J)
      	VIOL=VIOL+VIO*VIO
  26  CONTINUE
C
      IF(IOP.GT.2) WRITE (666,201) NITER,NLS,DUAL,PRIM,VMAX,JACT,IACT
c      WRITE (666,*) 'K=',K
C
C     Termination tests
C
      IF(VMAX.LT.TOL) GOTO 900
      NITER=NITER+1
      IF(NITER.GT.MAX) GOTO 901
c Changer le 18-5-95 (Cfr Mr. Zhang)      
      IF(DUAL.GT.DUMAX) GOTO 902	

C
C     2. Generation of quadratic subproblem
C        ----------------------------------
C
      PRIM=OBJ
      DO 32 I=1,N
      XI2=XI(I)*XI(I)
      X2(I)=XI2
      BB=-UN/XI2
      AA=-BB
C
      DO 28 J=1,JACT
      JU=JR(J)
      C=CIJ(I,JU)
      IF (C.GE.ZERO) GOTO 28
C
      C=C/XI2
      AA=AA-RJ(J)*C
  28  CIJ(I,JU)=-C
C
      AA=DEUX*AA/XI(I)
      AI(I)=AA
      IF(IX(I).EQ.0) GOTO 32
C
      PRIM=PRIM+UN/XI(I)
      DO 30 J=1,JACT
      JU=JR(J)
  30  BB=BB-CIJ(I,JU)*RJ(J)
      XI(I)=XI(I)-BB/AA
  32  CONTINUE
C
      DO 36 J=1,JACT
      ZZ=CJ(J)
      JU=JR(J)
C
      DO 34 I=1,N
      IF(IX(I).EQ.0) ZZ=ZZ+CIJ(I,JU)*XI(I)
  34  CONTINUE
C
  36  CJBAR(J)=ZZ
C
C     3. Solution of quadratic subproblem
C        --------------------------------
C
      TOLD=DSQRT(VIOL)/CUT
      PRC=VMAX*PREC
      PRC=PRC*PRC
      CALL SCP1 (XI,IX,XIMIN,XIMAX,AI,SI,
     *           RJ,JR,CJ,CJBAR,CG,CIJ,
     *           TOLD,PRC,VIOL,PRIM,DUMAX,
     *           N,JACT,IACT,NODM,IOP)
      NLS=NLS+NODM
C
C     4. Recover original CONLIN data
C        ----------------------------
C
      DO 38 J=1,JACT
      JU=JR(J)
      DO 38 I=1,N
      IF (CIJ(I,JU).GT.ZERO) CIJ(I,JU)=CIJ(I,JU)*X2(I)
  38  CIJ(I,JU)=-CIJ(I,JU)
      GOTO 1
C
C     Optimization loop on full CONLIN problem is completed
C     -----------------------------------------------------
C
 900  IF(IOP.GT.0) 
     *WRITE (666,202) NITER,NLS,DUAL,PRIM,VMAX,JACT,IACT,
     *              (JR(J),J=1,JACT)
      RETURN
C
C     Error Messages
C     --------------
C
 901  WRITE (666,291) MAX
	WRITE (29 ,291) MAX															!bug
      NON=903
      GO TO 900
C
 902  WRITE (666,292)
	WRITE (29 ,292)																!bug
      NON=899
      GO TO 900
C
 200  FORMAT(//1X,58(1H-)/
     *       ' Iter  Nls         Dual       Primal         Viol',
     *       ' Jact Iact'/1X,58(1H-)/)
 201  FORMAT(2I5,3F13.4,2I5)
 202  FORMAT(' Number of Iterations            Iter  ',I5/
     *       ' Number of Line Searches         Nls   ',I5/
     *       ' Dual Function                   Dual  ',E15.6/
     *       ' Primal Function                 Primal',E15.6/
     *       ' Constraint Violation            Viol  ',E15.6/
     *       ' Number of Active Constraints    Jact  ',I5/
     *       ' Number of Active Variables      Iact  ',I5/
     *       ' List of Active Constraints'/(10I5))
 291  FORMAT(' Maximum number of iterations (Conlin) ',I5)
 292  FORMAT(' No feasible solution to this problem')
      END
      
C========================================================================
C========================================================================
      
      SUBROUTINE SCP1 (XI,IX,XIMIN,XIMAX,AI,SI,
     *                 RJ,JR,CJ,CJBAR,CG,CIJ,
     *                 TOL,PREC,VIOL,OBJ,DUMAX,
     *                 N,M,IACT,NODM,IOP)
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION XI(N),IX(N),XIMIN(N),XIMAX(N),AI(N),SI(N),
     *          RJ(M),JR(M),CJ(M),CJBAR(M),CG(M),CIJ(N,M)
C
      ZERO=0.D 00
      UN  =1.D 00
      NODM=0
      IGC=M
      JACT=M
      MAXODM=(M+N)*5
C
C     Conjugate Gradient
C
   2  NODM=NODM+1
      IF(NODM.GT.MAXODM) GOTO 900
      IGC=IGC+1
      IF(IGC.LE.JACT) GOTO 6
      IGC=0
C
      DO 4 J=1,M
   4  CG(J)=CJ(J)
      GOTO 10
C
   6  GAM=VIOL/OLD
      DO 8 J=1,M
   8  CG(J)=CJ(J)+GAM*CG(J)
C
  10  STS=ZERO
      DO 14 I=1,N
      S=ZERO
C
      DO 12 J=1,M
      JU=JR(J)
  12  S=S+CIJ(I,JU)*CG(J)
C
      SS=S/AI(I)
      SI(I)=SS
      IF(IX(I).EQ.0) STS=STS+S*SS
  14  CONTINUE
C
      IF(STS.LE.ZERO) STS=PREC
      PTG=VIOL
C
C     One Dimensional Maximization
C
  20  II=0
      IUPT=0
      TAU=PTG/STS
      DO 22 J=1,M
      IF(CG(J).GE.ZERO) GOTO 22
      TAU1=-RJ(J)/CG(J)
      IF(TAU1.GE.TAU) GOTO 22
      TAU=TAU1
      II=J
      IUPT=1
  22  CONTINUE
C
      DO 28 I=1,N
      IF(IX(I)) 24,28,26
C
  24  IF(SI(I).LE.ZERO) GOTO 28
      TAU1=(XIMIN(I)-XI(I))/SI(I)
      IF(TAU1.GE.TAU) GOTO 28
      TAU=TAU1
      II=-I
      IUPT=2
      GOTO 28
C
  26  IF(SI(I).GE.ZERO) GOTO 28
      TAU1=(XIMAX(I)-XI(I))/SI(I)
      IF(TAU1.GE.TAU) GOTO 28
      TAU=TAU1
      II=I
      IUPT=2
  28  CONTINUE
C
      IF(IUPT.NE.2) GOTO 40
C
      IACT=IACT+1
      I=II
      IF(I.GT.0) GOTO 30
      I=-I
      XX=XIMIN(I)
      GOTO 32
C
  30  XX=XIMAX(I)
  32  IX(I)=0
      OBJ=OBJ-UN/XX
      DO 34 J=1,M
      JU=JR(J)
  34  CJBAR(J)=CJBAR(J)+XX*CIJ(I,JU)
C
      SK2=SI(I)*SI(I)*AI(I)
      PTG=PTG+TAU*SK2
      STS=STS+SK2
C
      IF(IOP.LE.3) GOTO 20
      VIO=DSQRT(PTG-TAU*STS)
      WRITE (666,200) VIO,IACT,II
      GOTO 20
C
  40  DO 42 J=1,M
  42  RJ(J)=RJ(J)+TAU*CG(J)
C
      IF(IUPT.EQ.0) GOTO 50
      RJ(II)=ZERO
      CG(II)=ZERO
      IGC=JACT
      JACT=JACT-1
C
  50  PRIM=OBJ
      DO 52 I=1,N
      XI(I)=XI(I)+TAU*SI(I)
      IF(IX(I).EQ.0) PRIM=PRIM+UN/XI(I)
  52  CONTINUE
C
      OLD=VIOL
      VIOL=ZERO
      DUAL=PRIM
      DO 70 J=1,M
      ZZ=ZERO
      IF(RJ(J).EQ.ZERO) GOTO 70
C
      JU=JR(J)
      DO 68 I=1,N
      IF(IX(I).EQ.0) ZZ=ZZ+CIJ(I,JU)*XI(I)
  68  CONTINUE
C
      ZZ=CJBAR(J)-ZZ
      VIOL=VIOL+ZZ*ZZ
      DUAL=DUAL+RJ(J)*ZZ
  70  CJ(J)=ZZ
C
      VIO=DSQRT(VIOL)
      IF(IOP.GT.2) WRITE (666,201) NODM,DUAL,PRIM,VIO,JACT,IACT
c Changer le 18-5-95 (Cfr Mr. Zhang)      
      IF(DUAL.GT.DUMAX) GOTO 900
      IF(VIO.GT.TOL) GOTO 2
      RETURN
C
 900  IF(IOP.GT.0) WRITE (666,290)
	IF(IOP.GT.0) WRITE (29 ,290)												!bug
      RETURN
 200  FORMAT(F49.4,I10,2I5)
 201  FORMAT(I10,3F13.4,2I5)
 290  FORMAT(' Warning from CONVX1 - Maximization algorithm failed')
      END
      
      FUNCTION PROMIX (X,F,N)
C     ========================
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION X(N),F(N)
      PROMIX=0.D 00     
      DO 3 I=1,N
      IF(F(I)) 1,3,2
   1  PROMIX=PROMIX-F(I)/X(I)
      GOTO 3
   2  PROMIX=PROMIX+F(I)*X(I)
   3  CONTINUE
      RETURN
      END
