
      SUBROUTINE VISIONN(NETO,WIDTH,COORD,IMPR,NSOL,
     *                   DI,AL,IAL)

      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION DI(1),AL(1),IAL(1)

c  Pour VAR, vecteur tempo servant à stoquer les données du READ(46) contenant les dimensions des panneaux
	I1=1			! VAR(11,NETO,11)
	I2=I1+11*11*NETO

	
	J1=1					!XA(11,NETO)
	J2=J1+11*NETO			!YA(11,NETO)
	J3=J2+11*NETO			!XB(11,NETO)
	J4=J3+11*NETO			!YB(11,NETO)
	J5=J4+11*NETO		!CHA(100,NETO,3) pas utitise dans VISIONN mais necesaire pour les READ
	J6=J5+100*NETO*3	    !HXTR(NETO,10)
	J7=J6+10*NETO		!WXTR(NETO,10)
	J8=J7+10*NETO		!ABTR(NETO,10)
	J9=J8+10*NETO		!XP(NETO,10,4)
	J10=J9+10*NETO*4	!YP(NETO,10,4)
	J11=J10+10*NETO*4	!Q(NETO)
	J12=J11+NETO		!TETA(NETO)
	J13=J12+NETO		!PHILN(NETO)
	J14=J13+NETO		!HIGHT(NETO)
	J15=J14+NETO		!ANGL(NETO)
	J16=J15+NETO		!XI(NETO,NSOL) pas utilise mais necesaire a cause du READ
	J17=J16+NETO*NSOL	!XF(NETO,NSOL) pas utilise mais necesaire a cause du READ
	J18=J17+NETO*NSOL	!POIDS(NETO)   pas utilise 
	J19=J18+NETO		!HX(10)
	J20=J19+10			!WX(10)
	J21=J20+10			!AB(10)
	J22=J21+10			!C(4)
	J23=J22+4	

	K1=1			!ITYPE(NETO)
	K2=K1+NETO	
	K3=K2+NETO	
	K4=K3+NETO		!IPTS(NETO,NSOL)
	K5=K4+NETO*NSOL	!MT(NETO)
	K6=K5+NETO		!KST(NETO)
	K7=K6+NETO		!JDEF(NETO)
	K8=K7+NETO		!


      CALL VISION3(NETO,WIDTH,COORD,IMPR,NSOL,
     *     DI(I1),
     *     AL(J1),AL(J2),AL(J3),AL(J4),AL(J5),AL(J6),AL(J7),AL(J8),
     *     AL(J9),AL(J10),AL(J11),AL(J12),AL(J13),AL(J14),AL(J15),
     *     AL(J16),AL(J17),AL(J19),AL(J20),AL(J21),AL(J22),
     *     IAL(K1),IAL(K4),IAL(K5),IAL(K6),IAL(K7))


      RETURN
	END
C******************************************************************************
C******************************************************************************
C******************************************************************************

      SUBROUTINE VISION3 (NETO,WIDTH,COORD,IMPR,NSOL,
     *					VAR,
     *					XA,YA,XB,YB,CHA,HXTR,WXTR,ABTR,
     *					XP,YP,Q,TETA,PHILN,HIGHT,ANGL,
     *					XI,XF,HX,WX,AB,C,
     *					ITYPE,IPTS,MT,KST,JDEF)

C******************************************************************************
C
C     Subroutine de préparation des fichiers dessins 
C
C	Créer:  Ph. Rigo (Dec 99)
C
C     Modifié :  1-11-2000
C     Modifié : 03-03-2001  NSOL cas de charge
C
C******************************************************************************

      IMPLICIT REAL*8 (A-H,O-Z)
	CHARACTER*11 NOM
      CHARACTER*22 NOM1  !' A.MULTI.STRUC\STRUC-a\ '

      DIMENSION COORD(NETO,4),FF(11,NETO),VAR(11,NETO,11)

      DIMENSION XA(11,NETO),YA(11,NETO),XB(11,NETO),YB(11,NETO),
     *  HXTR(NETO,10),WXTR(NETO,10),ABTR(NETO,10),
     *  XP(NETO,10,4),YP(NETO,10,4),Q(NETO),TETA(NETO),PHILN(NETO),
     *  HIGHT(NETO),ANGL(NETO),HX(10),
     *  WX(10),AB(10),C(4),
     *  XI(NETO,NSOL),XF(NETO,NSOL)
      DIMENSION ITYPE(NETO),IPTS(NETO,NSOL),
     *          MT(NETO),KST(NETO),JDEF(NETO)

	DIMENSION XPP(4),YPP(4)
 
      COMMON/NOM/NOM,NOM1  
      COMMON/PY/PI


C     --Lecture des données: Read(42 et 45)

      DO 1 I=1,NETO
       READ(42) ITYPE(I),PHILN(I),Q(I),HIGHT(I),ANGL(I),Itemp,
     *          (XI(I,J),J=1,NSOL),(XF(I,J),J=1,NSOL),Itemp,TETA(I),
     *          Temp,(IPTS(I,J),J=1,NSOL)

       READ(45)NEL,MT(I),KST(I)

       IF(MT(I).NE.0) THEN
	   READ(45) HX,WX,AB
         DO 111 J=1,10
           HXTR(I,J)=HX(J)
           WXTR(I,J)=WX(J)
           ABTR(I,J)=AB(J)
 111     CONTINUE
       ENDIF

       DO  IS=1,NSOL
          DO  J=1,IPTS(I,IS)
            READ(42) Temp,temp,temp    ! CHA(J,I,1),CHA(J,I,2),CHA(J,I,3)
          ENDDO
        ENDDO

   1  CONTINUE

C     --Lecture des données: Read(46)
c	  Cfr dans Subr ENT Write(46) Delta,HYA,DYA,WYA,TYA,EPSA,HXR,DXR,WXR,TXR,EPSR
      REWIND(46)
	DO 345 NEL=1,NETO
        READ(46) (VAR(1,NEL,J),J=1,11) ! J= nbre de variables

	  DO 346 J=1,11
	  DO 346 K=2,11                  ! K= indice des 11 pts par panneau
	    VAR(K,NEL,J)=VAR(1,NEL,J)    ! la meme valeur pour les 11pts du dessin car valeur constante
 346    CONTINUE

 345  CONTINUE
c
c    --Main loop -------------------

      DO 17 I=1,NETO
            PHI=PHILN(I)*PI/180.
            ANG=ANGL(I) *PI/180.
            IF((ITYPE(I) .EQ. 2).OR.(ITYPE(I).EQ.5)) THEN		!février 2004
                XA(1,I)=COORD(I,1)
                YA(1,I)=COORD(I,3)
                AX=    (COORD(I,2)-COORD(I,1))/10.
                AY=    (COORD(I,4)-COORD(I,3))/10.
                DO 20 J=1,10
                  XA(J+1,I)=COORD(I,1)+J*AX
 20               YA(J+1,I)=COORD(I,3)+J*AY
            ELSE
                IF(PHI .GT. 0. ) THEN
                  XA(1,I)=COORD(I,2)
                  YA(1,I)=COORD(I,4)
                ELSE
                  XA(1,I)=COORD(I,1)
                  YA(1,I)=COORD(I,3)
                ENDIF
                PHI=ABS(PHI/10.)
                C1 =DSIN( PHI /2.)
                ANG=ANG+PHI
                DO 18 J=2,11
                   ANG=ANG-PHI
                   C2=DSIN(ANG-PHI/2.)
                   C3=DCOS(ANG-PHI/2.)
                   XA(J,I)=XA(J-1,I)+2.*Q(I)*C1*C3
                   YA(J,I)=YA(J-1,I)-2.*Q(I)*C1*C2
  18            CONTINUE
           ENDIF
c
C     LES TRAVERSES
c    ---------------
C		DESG= Distance du Segment depuis son Origine (Y=0)
C		NSEG= Numéro du segment (1 à 10)

      IF(MT(I).EQ.0) GOTO 17

      DO 177 J=1,MT(I)
        DSEG1=0.
        NSEG=0
 155    NSEG=NSEG+1
        DSEG2=DSEG1
        IF(NSEG.GT.10)THEN
         WRITE(66,*)'La position de la traverse ',J,' est fausse.'
         WRITE(*,*) 'La position de la traverse ',J,' est fausse.'
         READ(*,*) gfgg
         STOP
        ENDIF
        C(1)=XA(NSEG,I)
        C(2)=XA(NSEG+1,I)
        C(3)=YA(NSEG,I)
        C(4)=YA(NSEG+1,I)
        DSEG1=DSEG2+SQRT( (C(1)-C(2))**2 + (C(3)-C(4))**2 )
        IF(ABTR(I,J).GT.DSEG1)GOTO 155
        DELTA=ABTR(I,J)-DSEG2 

        CALL RAIDI(C,XPP,YPP,HXTR(I,J),WXTR(I,J),DELTA,KST(I))

        DO 177 K=1,4
          XP(I,J,K)=XPP(K)
          YP(I,J,K)=YPP(K)
 178    CONTINUE
 177  CONTINUE

  17  CONTINUE  ! End loop on panels
c     --------------------------------------

      VMINX=0.
      VMINY=0.
      VMAXX=0.
      VMAXY=0.
      DO 21 I=1,NETO
       VMAXX=DMAX1(COORD(I,1),COORD(I,2),VMAXX)
       VMINX=DMIN1(COORD(I,1),COORD(I,2),VMINX)
       VMAXY=DMAX1(COORD(I,3),COORD(I,4),VMAXY)
       VMINY=DMIN1(COORD(I,3),COORD(I,4),VMINY)
  21  Continue
      DELTAX=VMAXX-VMINX
      DELTAY=VMAXY-VMINY
      DREF  =(DMAX1(DELTAX,DELTAY) ) /10.


C    TRACES DE VARIABLES ISOLEES ( PERPEND. aux panneaux)
C    =====================================================
c	- XA, YA  Coord point de départ
c	- XB, YB  Coord point finil
c	- XP, YP  Coord des traverses
C     - FF vect temporaire (11,Neto)

      NOM='FichDessin\'

      OPEN(22,file=NOM1//NOM//'T bordage.des')      ! Epaisseur Bordé
      CALL DEFOR4(VAR(1,1,1),1,NETO,DREF,TETA,XA,YA,PHILN,XB,YB,
     *            MT,XP,YP,FF)
	CLOSE(22)

      OPEN(22,file=NOM1//NOM//'H ame Cadres.des')   ! Hauteur âme cadres
      CALL DEFOR4(VAR(1,1,2),2,NETO,DREF,TETA,XA,YA,PHILN,XB,YB,
     *            MT,XP,YP,FF)
	CLOSE(22)

      OPEN(22,file=NOM1//NOM//'T ame Cadres.des')   ! Epaisseur âme cadres
      CALL DEFOR4(VAR(1,1,3),3,NETO,DREF,TETA,XA,YA,PHILN,XB,YB,
     *            MT,XP,YP,FF)
	CLOSE(22)

      OPEN(22,file=NOM1//NOM//'L sem Cadres.des')   ! Largeur sem cadres
      CALL DEFOR4(VAR(1,1,4),4,NETO,DREF,TETA,XA,YA,PHILN,XB,YB,
     *            MT,XP,YP,FF)
	CLOSE(22)

      OPEN(22,file=NOM1//NOM//'T sem Cadres.des')   ! Epaisseur sem cadres
      CALL DEFOR4(VAR(1,1,5),5,NETO,DREF,TETA,XA,YA,PHILN,XB,YB,
     *            MT,XP,YP,FF)
	CLOSE(22)

      OPEN(22,file=NOM1//NOM//'Espac Cadres.des')   ! Entredistance entre cadres
      CALL DEFOR4(VAR(1,1,6),6,NETO,DREF,TETA,XA,YA,PHILN,XB,YB,
     *            MT,XP,YP,FF)
	CLOSE(22)

      OPEN(22,file=NOM1//NOM//'H ame Raid.des')   ! Hauteur âme raidisseurs
      CALL DEFOR4(VAR(1,1,7),7,NETO,DREF,TETA,XA,YA,PHILN,XB,YB,
     *            MT,XP,YP,FF)
	CLOSE(22)

      OPEN(22,file=NOM1//NOM//'T ame Raid.des')   ! Epaisseur âme raidisseurs
      CALL DEFOR4(VAR(1,1,8),8,NETO,DREF,TETA,XA,YA,PHILN,XB,YB,
     *            MT,XP,YP,FF)
	CLOSE(22)

      OPEN(22,file=NOM1//NOM//'L sem Raid.des')   ! Largeur sem raidisseurs
      CALL DEFOR4(VAR(1,1,9),9,NETO,DREF,TETA,XA,YA,PHILN,XB,YB,
     *            MT,XP,YP,FF)
	CLOSE(22)

      OPEN(22,file=NOM1//NOM//'T sem Raid.des')   ! Epaisseur sem raidisseurs
      CALL DEFOR4(VAR(1,1,10),10,NETO,DREF,TETA,XA,YA,PHILN,XB,YB,
     *            MT,XP,YP,FF)
	CLOSE(22)

      OPEN(22,file=NOM1//NOM//'Espac Raid.des')   ! Entredistance entre raidisseurs
      CALL DEFOR4(VAR(1,1,11),11,NETO,DREF,TETA,XA,YA,PHILN,XB,YB,
     *            MT,XP,YP,FF)
	CLOSE(22)

      RETURN
      END
C
C  ------------------------------------------------------------------------------
C
      SUBROUTINE RAIDI(C,X,Y,HXTR,WXTR,DELTA,KST)
C     **********************************************
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION C(4),X(4),Y(4)
C
        SEG=SQRT( (C(1)-C(2))**2 + (C(3)-C(4))**2 )
        S1=(C(4)-C(3))/SEG
        C1=(C(2)-C(1))/SEG
        MI=(-1)**KST
        WXTR2=WXTR/2.
        X(1)=C(1)+DELTA*C1
        Y(1)=C(3)+DELTA*S1
        X(2)=X(1)+MI*S1*HXTR
        Y(2)=Y(1)-MI*C1*HXTR
        X(3)=X(2)+C1*WXTR2
        Y(3)=Y(2)+S1*WXTR2
        X(4)=X(2)-C1*WXTR2
        Y(4)=Y(2)-S1*WXTR2
      RETURN
      END

C
      SUBROUTINE EXTREM2(F,VMAX,NETO)
C     *******************************
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION F(11,NETO)

      VMAX=F(1,1)
      VMIN=F(1,1)

      DO 1 I=1,NETO
      DO 1 K=1,11
        VMAX=DMAX1(VMAX,F(K,I))
        VMIN=DMIN1(VMIN,F(K,I))
 1    CONTINUE

      IF(DABS(VMAX).LE.DABS(VMIN)) VMAX=VMIN
      VMAX=DABS(VMAX)

      RETURN
      END

C
C     DESSIN DES VARIABLES, COUPE TRANSV. (Idem DEFOR1 dans VISION.sub)
C     **************************************************
      SUBROUTINE DEFOR4 (F,L,NETO,DREF,TETA,XA,YA,PHILN,XB,YB,
     *                   MT,XP,YP,FF)
C
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*40 TITRE(13),TITLE(13)
      CHARACTER*6  UNITE(13)

      DIMENSION XA(11,NETO),YA(11,NETO),XB(11,NETO),YB(11,NETO),
     *          XP(NETO,10,4),YP(NETO,10,4),
     *          F(11,NETO),FF(11,NETO),
     *		  TETA(NETO),PHILN(NETO),MT(NETO)

      COMMON/PY/PI
      COMMON/LANGUE/LANGUE ! 1 French, 2 English


      DATA TITRE/'Epaisseur du bordé                      ',
     &           'Hauteur   ame des cadres (transversaux) ',
     &           'Epaisseur ame des cadres (transversaux) ',
     &           'Largeur   semelle - cadres(transversaux)',
     &           'Epaisseur semelle - cadres(transversaux)',
     &           'Entredistance entre cadres(transversaux)',
     &           'Hauteur   ame des raidisseurs (long.)   ',
     &           'Epaisseur ame des raidisseurs (long.)   ',
     &           'Largeur   semelle - raidisseurs(long.)  ',
     &           'Epaisseur semelle - raidisseurs(long.)  ',
     &           'Entredistance entre raidisseurs(long.)  ',
     &           '...                                     ',
     &           '...                                     '/
      DATA TITLE/'Plate Thickness                         ',
     &           'Web Height    (Frames/Cadres)           ',
     &           'Web Thickness (Frames/cadres)           ',
     &           'Flange width  (Frames/cadres)           ',
     &           'Flange Thickness(Frames/cadres)         ',
     &           'Spacing of Frames/cadres                ',
     &           'Web Height    (Longitudinals)           ',
     &           'Web Thickness (Longitudinals)           ',
     &           'Flange width  (Longitudinals)           ',
     &           'Flange Thickness(Longitudinals)         ',
     &           'Spacing of Longitudinals                ',
     &           '...                                     ',
     &           '...                                     '/
      DATA UNITE/' mm.  ',' mm.  ',' mm.  ',' mm.  ',' mm.  ',
     &           ' mm.  ',' mm.  ',' mm.  ',' mm.  ',
     &           ' mm.  ',' mm.  ',' mm.  ',' mm.  '/
C     ---------------------------------
      IF(LANGUE.NE.1) THEN
        DO I=1,13
	    TITRE(I)=TITLE(I)
	  ENDDO
	ENDIF
c
      CALL EXTREM2 (F,VMAX,NETO)

      IF(VMAX.LE.1.E-08) VMAX=1.E-08
      ECH=DREF/VMAX

      DO 2 I=1,NETO
      DO 2 K=1,11
 2    FF(K,I)=F(K,I)*ECH

      DO 3 I=1,NETO
        PHI=DABS(PHILN(I)) * 0.1
        DO 3 K=1,11
          S1=VSIN(TETA(I),(K-1)*PHI)
          C1=VCOS(TETA(I),(K-1)*PHI)
          XB(K,I)=XA(K,I)-FF(K,I)*C1
          YB(K,I)=YA(K,I)-FF(K,I)*S1
  3   CONTINUE

      VMAX=VMAX*1000.  ! pour avoir des mm
      WRITE(22,*) VMAX,DREF
      WRITE(22,*) UNITE(L)
      WRITE(22,*) TITRE(L)

      DO 6 I=1,NETO
       WRITE(22,*) I,MT(I)
       DO 5 K=1,11
 5     WRITE(22,100) I,XA(K,I),YA(K,I),XB(K,I),YB(K,I)

       IF(MT(I).EQ.0) GOTO 6
        DO 4 K=1,MT(I)
        WRITE(22,100) I,XP(I,K,1),YP(I,K,1),XP(I,K,2),YP(I,K,2)
 4      WRITE(22,100) I,XP(I,K,3),YP(I,K,3),XP(I,K,4),YP(I,K,4)
 6    CONTINUE

  100 FORMAT(1X,I3,2X,F12.4,5X,F12.4,5X,F12.4,5X,F12.4)			!extension neto
      RETURN
      END
