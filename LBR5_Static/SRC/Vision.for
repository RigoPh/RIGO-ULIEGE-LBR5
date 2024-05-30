
      SUBROUTINE VISION(NETO,WIDTH,COORD,IMPR,INDAIG,INDRAI,NSOL,
     *                  DI,AL,IAL)

      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION DI(1),AL(1),IAL(1)

c  Pour VECT,NX,NY,MX,MY,NXY,U,V,W,RX,RY,SX,SY,TXY,COMP,
c	  SAIG,TAIG,SCJAS,SCJAB,PRES
	I1=1			! Vect
	I2=I1+11*5*NETO
	I3=I2+11*NETO
	I4=I3+11*NETO
	I5=I4+11*NETO
	I6=I5+11*NETO
	I7=I6+11*NETO
	I8=I7+11*NETO
	I9=I8+11*NETO
	I10=I9+11*NETO
	I11=I10+11*NETO
	I12=I11+11*NETO
	I13=I12+11*NETO
	I14=I13+11*NETO
	I15=I14+11*NETO
	I16=I15+11*NETO
	I17=I16+11*NETO
	I18=I17+11*NETO
	I19=I18+11*NETO
	I20=I19+11*NETO	! Pres
	I21=I20+11*NETO*(NSOL+4)

	
	J1=1			    !XA(11,NETO)
	J2=J1+11*NETO		!YA(11,NETO)
	J3=J2+11*NETO		!XB(11,NETO)
	J4=J3+11*NETO		!YB(11,NETO)
	J5=J4+11*NETO		!CHA(100,NETO,3,NSOL)
	J6=J5+100*NETO*3*NSOL	    !HXTR(NETO,10)
	J7=J6+10*NETO		!WXTR(NETO,10)
	J8=J7+10*NETO		!ABTR(NETO,10)
	J9=J8+10*NETO		!XP(NETO,10,4)
	J10=J9 +10*NETO*4	!YP(NETO,10,4)
	J11=J10+10*NETO*4	!Q(NETO)
	J12=J11+NETO		!TETA(NETO)
	J13=J12+NETO		!PHILN(NETO)
	J14=J13+NETO		!HIGHT(NETO)
	J15=J14+NETO		!ANGL(NETO)
	J16=J15+NETO		!XI(NETO,NSOL+4) 
	J17=J16+NETO*(4+NSOL)	!XF(NETO,NSOL+4)
	J18=J17+NETO*(4+NSOL)	!POIDS(NETO)
	J19=J18+NETO		!HX(10)
	J20=J19+10			!WX(10)
	J21=J20+10			!AB(10)
	J22=J21+10			!C(4)
	J23=J22+4			!XPP(4)
	J24=J23+4			!YPP(4)
	J25=J24+4			!


	K1=1			!ITYPE(NETO)
	K2=K1+NETO		!KSE(NETO)
	K3=K2+NETO		!IVARIA(NETO)
	K4=K3+NETO		!IPTS(NETO,NSOL)
	K5=K4+NETO*NSOL	!MT(NETO)
	K6=K5+NETO		!KST(NETO)
	K7=K6+NETO		!JDEF(NETO)
	K8=K7+NETO		!


      CALL VISION2(NETO,WIDTH,COORD,IMPR,INDAIG,INDRAI,NSOL,
     *     DI(I1),DI(I2),DI(I3),DI(I4),DI(I5),DI(I6),DI(I7),DI(I8),
     *     DI(I9),DI(I10),DI(I11),DI(I12),DI(I13),DI(I14),DI(I15),
     *     DI(I16),DI(I17),DI(I18),DI(I19),DI(I20),
     *     AL(J1),AL(J2),AL(J3),AL(J4),AL(J5),AL(J6),AL(J7),AL(J8),
     *     AL(J9),AL(J10),AL(J11),AL(J12),AL(J13),AL(J14),AL(J15),
     *     AL(J16),AL(J17),AL(J18),AL(J19),AL(J20),AL(J21),AL(J22),
     *     AL(J23),AL(J24),
     *     IAL(K1),IAL(K2),IAL(K3),IAL(K4),IAL(K5),IAL(K6),IAL(K7))


      RETURN
	END
C******************************************************************************
C******************************************************************************
C******************************************************************************

      SUBROUTINE VISION2(NETO,WIDTH,COORD,IMPR,INDAIG,INDRAI,NSOL,
     *         VECT,NX,NY,MX,MY,NXY,U,V,W,RX,RY,SX,SY,TXY,COMP,
     *         SAIG,TAIG,SCJAS,SCJAB,PRES,
     *  XA,YA,XB,YB,CHA,HXTR,WXTR,ABTR,XP,YP, 
     *  Q,TETA,PHILN,HIGHT,ANGL,XI,XF,POIDS,HX,WX,AB,C,XPP,YPP,
     *  ITYPE,KSE,IVARIA,IPTS,MT,KST,JDEF)

C******************************************************************************
C
C     Subroutine de préparation des fichiers dessins 
C
C	Créer: Thése de Ph. Rigo
C
C     Modifié : 25-01-1995  Hengfeng Wang (Version PC)
C     Modifié : 01-11-2000  Rigo, version anglaise
C     Modifié : 03-03-2001  NSOL cas de charge
C
C******************************************************************************

      IMPLICIT REAL*8 (A-H,O-Z)
	CHARACTER*11 NOM
      CHARACTER*22 NOM1  ! 'A.MULTI.STRUC\STRUC-a\'
	CHARACTER*2  NUMBER
      REAL*8  NX(11,NETO),NY(11,NETO),MX(11,NETO),MY(11,NETO),
     *        NXY(11,NETO)

      DIMENSION COORD(NETO,4),VECT(11,5,NETO),U(11,NETO),V(11,NETO),
     * W(11,NETO),RX(11,NETO),RY(11,NETO),SX(11,NETO),SY(11,NETO),
     * TXY(11,NETO),COMP(11,NETO),
     * SAIG(11,NETO),TAIG(11,NETO),SCJAS(11,NETO), SCJAB(11,NETO),
     * FF(11,NETO),PRES(11,NETO,NSOL+4)

      DIMENSION XA(11,NETO),YA(11,NETO),XB(11,NETO),YB(11,NETO),
     *  CHA(100,NETO,3,NSOL),HXTR(NETO,10),WXTR(NETO,10),ABTR(NETO,10),
     *  XP(NETO,10,4),YP(NETO,10,4),Q(NETO),TETA(NETO),PHILN(NETO),
     *  HIGHT(NETO),ANGL(NETO),XI(NETO,NSOL),XF(NETO,NSOL),POIDS(NETO),
     *  HX(10),WX(10),AB(10),C(4),XPP(4),YPP(4)

      DIMENSION ITYPE(NETO),KSE(NETO),IVARIA(NETO),IPTS(NETO,NSOL),
     *  MT(NETO),KST(NETO),JDEF(NETO)


      COMMON/PY/PI 
      COMMON/LANGUE/LANGUE  ! 1 French, 2 English
      COMMON/NOM/NOM,NOM1
c -----------------------------------------------------------

  1   CONTINUE
      IF (LANGUE==1) THEN  !Francais
       WRITE(*,*)'LES DIAGRAMMES DES SOLLICITATIONS SUIVANTES SONT ',
     *           'DISPONIBLES'
       WRITE(*,'(50(1H-))')
       WRITE(*,*)' 1-10 LA PRESSION (XI,XF) UNIFORME selon OX'
       WRITE(*,*)'      (pour chaque cas de charge)'
       WRITE(*,*)' 11   LE POIDS PROPRE'
       WRITE(*,*)' 12   LA CHARGE DE GRAVITE TOTALE variable selon OX',
     *                 '(IBAT =1, CHA1) pour un seul cas de charge'
       WRITE(*,*)' 13   LA PRESSION VARIANT selon OX',
     *                 '(IBAT =1, CHA2 ET CHA3), pour 1 cas de charge'
       WRITE(*,*)' 14   Pas de representation de la sollicitation.'
       WRITE(*,*)
       WRITE(*,*)' Pour passer a la visualisation des graphiques',
     *           ' ENTER 0 (par defaut)'
       WRITE(*,*)'        ENTRER 12 ou 13 pour specifier les',
     *           '          caracteristiques des diagrammes, '
       ISOLL=1
       READ(*,*) ISOLL
       IF((ISOLL.EQ.12).OR.(ISOLL.EQ.13)) THEN
          WRITE(*,*)' VOUS SOUHAITEZ OBTENIR LE DIAGRAMME D''UNE ',
     *              'CHARGE (CHA1 ou CHA2/CHA3) VARIANT SELON OX'
          ISS=1
          WRITE(*,*)'QUEL CAS DE CHARGE VOULEZ VOUS CONSIDERER ?'
          WRITE(*,*)'.......................................... '
          WRITE(*,*)' Par défaut ce sera le cas nø 1'
          READ(*,*) ISS
          IF(ISS.GT.NSOL) THEN
             WRITE(*,*)'Ce cas de charge est incorrect.'
             WRITE(*,*)'Il y a ',NSOL,' cas de charges.'
             WRITE(*,*)'Recommencer votre selection'
	       PAUSE 'OK?'
	       GOTO 1
          ENDIF
          INTERV=1
          WRITE(*,*)
          WRITE(*,*)'QUEL INTERVAL VOULEZ VOUS CONSIDERER ?'
          WRITE(*,*)'..................................... '
          WRITE(*,*)' Par défaut ce sera l''interval nø 1.'
          READ(*,*) INTERV
          WRITE(*,*)' '
       ENDIF
	ENDIF

      IF (LANGUE==2) THEN  !English
        WRITE(*,*)'THE AVAILABLE PLOTTED LOAD CASES ARE'
        WRITE(*,'(40(1H-))')
	  WRITE(*,*)' 1-10 LATERAL PRESSURE (XI,XF), UNIFORM along OX'
        WRITE(*,*)'       (for each load case)'
        WRITE(*,*)' 11   DEADWEIGHT'
        WRITE(*,*)' 12   Vertical loads, varying along OX ',
     *            '       (IBAT=1, CHA1) for only one load case'
        WRITE(*,*)' 13   Lateral Pressure, varying along OX ',
     *            '       (IBAT=1, CHA2 & CHA3) for only one load case'
        WRITE(*,*)' 14   No drawing of the sollicitation.'
        WRITE(*,*)
        WRITE(*,*)' ENTER 0  To visualize the output graphics'
        WRITE(*,*)'          Or ENTER 12 or 13 to specify the characteri
     *stics of plots #12 & #13'
        ISOLL=1
        ISS=1
        INTERV=1
        READ(*,*) ISOLL
        IF((ISOLL.EQ.12).OR.(ISOLL.EQ.13)) THEN
          WRITE(*,*)' You asked to plot the load pattern of a load',
     *              '         varying along OX (CHA1 or CHA2/CHA3)'
          WRITE(*,*)'Which Load Case do you want to plot?'
          WRITE(*,*)'..................................... '
          WRITE(*,*)' The default Load Case number is nø 1'
          READ(*,*) ISS
          IF(ISS.GT.NSOL) THEN
             WRITE(*,*)'This number is wrong'
             WRITE(*,*)'There are only ',NSOL,' load cases.'
             WRITE(*,*)'Correct yours Parameters'
	       PAUSE 'OK?'
	       GOTO 1
          ENDIF
          WRITE(*,*)'Which INTERVAL do you want to plot ?'
          WRITE(*,*)'................................... '
          WRITE(*,*)' Default value = interval Nø 1.'
          READ(*,*) INTERV
          WRITE(*,*)' '
        ENDIF
	ENDIF

c --------------------------------------------------------------

      IPTMAX=0
      DO 50 I=1,NETO
        READ(42) ITYPE(I),PHILN(I),Q(I),HIGHT(I),ANGL(I),IVARIA(I),
     *           (XI(I,J),J=1,NSOL),(XF(I,J),J=1,NSOL),KSE(I),
     *           TETA(I),POIDS(I),(IPTS(I,J),J=1,NSOL)

        IF(IMPR.NE.0) WRITE(*,*)IPTS(I,1)
        IPTMAX=MAX0(IPTMAX,IPTS(I,1))

        READ(45)NEL,MT(I),KST(I)

        IF(MT(I).NE.0) THEN
	    READ(45) HX,WX,AB
          DO  J=1,10
            HXTR(I,J)=HX(J)
            WXTR(I,J)=WX(J)
            ABTR(I,J)=AB(J)
          ENDDO
	  ENDIF
c	WRITE(666,*) 'NEL=',I
c	WRITE(666,*) 'POIDS(I)=',POIDS(I)
c	WRITE(666,*) '(XI(I,J),J=1,NSOL)=',(XI(I,J),J=1,NSOL)
c	WRITE(666,*) '(XF(I,J),J=1,NSOL)=',(XF(I,J),J=1,NSOL)
c	WRITE(666,*) '(IPTS(I,J),J=1,NSOL)=',(IPTS(I,J),J=1,NSOL)

        DO  IS=1,NSOL
          DO  J=1,IPTS(I,IS)
            READ(42) CHA(J,I,1,IS),CHA(J,I,2,IS),CHA(J,I,3,IS)
c            WRITE(666,*) 'IS=',IS
c            WRITE(666,*) CHA(J,I,1,IS),CHA(J,I,2,IS),CHA(J,I,3,IS)
          ENDDO
        ENDDO

      IF((INTERV.GT.IPTMAX).and.(INTERV.GE.2)) THEN
         WRITE(*,*)'L''interval choisi est incorrecte (=',INTERV,')'
         WRITE(*,*)'Il y a ',IPTMAX,' intervals.'
         WRITE(*,*)'Recommencer votre selection.'
	   PAUSE 'OK?'
	   REWIND(42)
	   REWIND(45)
	   GOTO 1
	ENDIF

  50  CONTINUE

c ------------------------------------------------
      IF (LANGUE==1) THEN  !Francais
       WRITE(*,*)' '
       WRITE(*,*)' PRENEZ VOUS LES ABSCISSES DE COUPE STANDARD ?'
       WRITE(*,*)' ______________________________________________'
       ICOUPE=0
       WRITE(*,*)' Si OUI taper sur "ENTER ou 0"     Si NON taper "1"'
       READ(*,*) ICOUPE
      ENDIF

      IF (LANGUE==2) THEN  !English
       WRITE(*,*)' '
       WRITE(*,*)' Do you select the standard PLOT Transv. sections ?'
       WRITE(*,*)' __________________________________________________'
       ICOUPE=0
       WRITE(*,*)' If YES enter " 0 " (default)   ;  If NO enter "1"'
       READ(*,*) ICOUPE
      ENDIF
      WRITE(*,*)' '

c -------------------------------------

      DO 81 I=1,NETO
        IF(KSE(I).NE.2) THEN  !	si KSE(I)=1 ou 0
		XI(I,NSOL+1)=POIDS(I)
		XF(I,NSOL+1)=POIDS(I)
	  ELSE                  !	si KSE(I)=2
		XI(I,NSOL+1)=-POIDS(I)
		XF(I,NSOL+1)=-POIDS(I)
        ENDIF
        ! CHA1
	  XI(I,NSOL+2)=CHA(INTERV,I,1,ISS) ! pour IS = ISS
	  XF(I,NSOL+2)=CHA(INTERV,I,1,ISS) ! pour IS = ISS
        ! CHA2, CHA3
	  XI(I,NSOL+3)=CHA(INTERV,I,2,ISS) ! pour IS = ISS
	  XF(I,NSOL+3)=CHA(INTERV,I,3,ISS) ! pour IS = ISS
        ! Mesh model sans charge
	  XI(I,NSOL+4)=0.
	  XF(I,NSOL+4)=0.
  81  CONTINUE
c --------------------------------------

      DO 17 I=1,NETO
            PHI=PHILN(I)*PI/180.
            SIGN=(-1)**KSE(I)
            ANG=ANGL(I) *PI/180.
	      DO II=1,NSOL+4
              PRES(1,I,II)=XI(I,II)*SIGN
	      ENDDO
            IF((ITYPE(I).EQ.2).OR.(ITYPE(I).EQ.5)) THEN			!février 2004
                 XA(1,I)=COORD(I,1)
                 YA(1,I)=COORD(I,3)
                 AX=(COORD(I,2)-COORD(I,1))/10.
                 AY=(COORD(I,4)-COORD(I,3))/10.
                 DO J=1,10
                    XA(J+1,I)=COORD(I,1)+J*AX
                    YA(J+1,I)=COORD(I,3)+J*AY
                 ENDDO
 			   DO II=1,NSOL+4
                    AP=((XF(I,II)-XI(I,II))*SIGN/10.)
                    DO J=1,10
					PRES(J+1,I,II)=PRES(1,I,II)+J*AP
				  ENDDO
			   ENDDO
            ELSE
               IF(PHI.GT.0.) THEN
                  XA(1,I)=COORD(I,2)
                  YA(1,I)=COORD(I,4)
               ELSE
                  XA(1,I)=COORD(I,1)
                  YA(1,I)=COORD(I,3)
               ENDIF
               PHI=ABS(PHI/10.)
               C1 =DSIN(PHI/2.)
               ANG=ANG+PHI
               DO 18 J=2,11
                  ANG=ANG-PHI
                  C2=DSIN(ANG-PHI/2.)
                  C3=DCOS(ANG-PHI/2.)
                  XA(J,I)=XA(J-1,I)+2.*Q(I)*C1*C3
                  YA(J,I)=YA(J-1,I)-2.*Q(I)*C1*C2
  18           CONTINUE
 			 DO II=1,NSOL+4
                 IF(IVARIA(I).EQ.1)THEN
                   AP=((XF(I,II)-XI(I,II))*SIGN/10.)
                   DO J=1,10
                     PRES(J+1,I,II)=PRES(1,I,II)+J*AP
                   ENDDO
                 ELSE
                   XF(I,II)=XI(I,II)+COORD(I,4)-COORD(I,3)
                   DO  J=1,10
                     PRES(J+1,I,II)=PRES(1,I,II) + Q(I)*SIGN* 
     *                     ( DSIN((TETA(I)-J*PHILN(I)/10.)*PI/180.)
     *                                        -DSIN(TETA(I)*PI/180.) )
	             ENDDO
                 ENDIF
	         ENDDO
            ENDIF
c
C       LES TRAVERSES
C		DESG= Distance du Segment depuis son Origine (Y=0)
C		NSEG= Numéro du segment (1 à 10)
        IF(MT(I).EQ.0) GOTO 17
        DO 177 J=1,MT(I)
         DSEG1=0.
         NSEG=0
 155     NSEG=NSEG+1
         DSEG2=DSEG1
         IF(NSEG.GT.10)THEN
           WRITE(*,*) 'Subr VISION (panneau ',I,')'
           WRITE(*,*) 'La position de la traverse ',J,' est fausse.'
           WRITE(*,*) 'Le programme est arrete.'
           PAUSE 'OK?'
           STOP
         ENDIF
         C(1)=XA(NSEG,I)
         C(2)=XA(NSEG+1,I)
         C(3)=YA(NSEG,I)
         C(4)=YA(NSEG+1,I)
         DSEG1=DSEG2+SQRT( (C(1)-C(2))**2 + (C(3)-C(4))**2 )
         IF(ABTR(I,J).GT.DSEG1) GOTO 155
         DELTA=ABTR(I,J)-DSEG2

         CALL RAIDIS(C,XPP,YPP,HXTR(I,J),WXTR(I,J),DELTA,KST(I))

         DO  K=1,4
           XP(I,J,K)=XPP(K)
           YP(I,J,K)=YPP(K)
	   ENDDO
 177    CONTINUE

  17  CONTINUE
c
C
C

      VMINX=0.
      VMINY=0.
      VMAXX=0.
      VMAXY=0.
      DO 21 I=1,NETO
      VMAXX=DMAX1(COORD(I,1),COORD(I,2),VMAXX)
      VMINX=DMIN1(COORD(I,1),COORD(I,2),VMINX)
      VMAXY=DMAX1(COORD(I,3),COORD(I,4),VMAXY)
  21  VMINY=DMIN1(COORD(I,3),COORD(I,4),VMINY)
      DELTAX=VMAXX-VMINX
      DELTAY=VMAXY-VMINY
      DREF  =(DMAX1(DELTAX,DELTAY) ) /8.


      DO 11  IS=1,NSOL  ! Boucle sur les cas de Charges

      DO 2 NEL=1,NETO
      CALL READ(V,  VECT,NEL,'  V   ',JDEF,5,ICOUPE,1,NETO,IS)
      CALL READ(U,  VECT,NEL,'  U   ',JDEF,1,ICOUPE,2,NETO,IS)
      CALL READ(W,  VECT,NEL,'  W   ',JDEF,5,ICOUPE,3,NETO,IS)
      CALL READ(NX, VECT,NEL,'  NX  ',JDEF,5,ICOUPE,4,NETO,IS)
      CALL READ(NY, VECT,NEL,'  NY  ',JDEF,5,ICOUPE,5,NETO,IS)
      CALL READ(NXY,VECT,NEL,' NXY  ',JDEF,1,ICOUPE,6,NETO,IS)
      CALL READ(MX, VECT,NEL,'  MX  ',JDEF,5,ICOUPE,7,NETO,IS)
      CALL READ(MY, VECT,NEL,'  MY  ',JDEF,5,ICOUPE,8,NETO,IS)
      CALL READ(RY, VECT,NEL,'  RY  ',JDEF,5,ICOUPE,9,NETO,IS)
      DO  K=1,11
       RY(K,NEL)=-RY(K,NEL)
      ENDDO

      CALL READ(RX,  VECT,NEL,'  RX  ',JDEF,1,ICOUPE,10,NETO,IS)
      CALL READ(SY,  VECT,NEL,'  SY  ',JDEF,5,ICOUPE,11,NETO,IS)
      CALL READ(SX,  VECT,NEL,'  SX  ',JDEF,5,ICOUPE,12,NETO,IS)
      CALL READ(TXY, VECT,NEL,' TXY  ',JDEF,1,ICOUPE,13,NETO,IS)
      CALL READ(COMP,VECT,NEL,' COMP ',JDEF,5,ICOUPE,14,NETO,IS)
        
	IF(INDAIG.EQ.0)GOTO 2  !8
       CALL READ(TAIG,VECT,NEL,'Tau JAB ',JDEF,5,ICOUPE,15,NETO,IS)
       CALL READ(SAIG,VECT,NEL,'Sy JAS C',JDEF,5,ICOUPE,16,NETO,IS)
c  8   CONTINUE

c      IF(INDRAI.EQ.0) GOTO 2
c      CALL READ(TRAID,VECT,NEL,' T raid ',JDEF,1,ICOUPE,17,NETO,IS)
c      CALL READ(SRAID,VECT,NEL,' S raid ',JDEF,5,ICOUPE,18,NETO,IS)
       CALL READ(SCJAS,VECT,NEL,' Sc JAS ',JDEF,5,ICOUPE,17,NETO,IS)
       CALL READ(SCJAB,VECT,NEL,' Sc JAB ',JDEF,5,ICOUPE,18,NETO,IS)
  2   CONTINUE

      NOM='FichDessin\'
	IF(IS.EQ.1) NUMBER='01'
	IF(IS.EQ.2) NUMBER='02'
	IF(IS.EQ.3) NUMBER='03'
	IF(IS.EQ.4) NUMBER='04'
	IF(IS.EQ.5) NUMBER='05'
	IF(IS.EQ.6) NUMBER='06'
	IF(IS.EQ.7) NUMBER='07'
	IF(IS.EQ.8) NUMBER='08'
	IF(IS.EQ.9) NUMBER='09'
	IF(IS.EQ.10)NUMBER='10'

C    TRACES DE VARIABLES ISOLEES ( PERPEND. A L' AXE NEUTRE)
C    *******************************************************
      OPEN(22,file=NOM1//NOM//'MX'//NUMBER//'.DES')
c     WRITE(*,*)'MX.DES'
      CALL DEFOR1 (MX,22,NETO,DREF,TETA,XA,YA,PHILN,XB,YB,1,MT,XP,YP,
     *             FF)
	CLOSE(22)

      OPEN(22,file=NOM1//NOM//'MY'//NUMBER//'.DES')
c     OPEN(22,file='MY.DES')
c     WRITE(*,*)'MY.DES'
      CALL DEFOR1 (MY,22,NETO,DREF,TETA,XA,YA,PHILN,XB,YB,2,MT,XP,YP,
     *             FF)
	CLOSE(22)

      OPEN(22,file=NOM1//NOM//'NXY'//NUMBER//'.DES')
c     WRITE(*,*)'NXY.DES'
      CALL DEFOR1 (NXY,22,NETO,DREF,TETA,XA,YA,PHILN,XB,YB,3,MT,XP,YP,
     *             FF)
	CLOSE(22)

      OPEN(22,file=NOM1//NOM//'RX'//NUMBER//'.DES')
c     WRITE(*,*)'RX.DES'
      CALL DEFOR1 (RX,22,NETO,DREF,TETA,XA,YA,PHILN,XB,YB,4,MT,XP,YP,
     *             FF)
	CLOSE(22)

      OPEN(22,file=NOM1//NOM//'SY'//NUMBER//'.DES')
c     WRITE(*,*)'SY.DES'
      CALL DEFOR1 (SY,22,NETO,DREF,TETA,XA,YA,PHILN,XB,YB,5,MT,XP,YP,
     *             FF)
	CLOSE(22)

      OPEN(22,file=NOM1//NOM//'TXY'//NUMBER//'.DES')
c     WRITE(*,*)'TXY.DES'
      CALL DEFOR1 (TXY,22,NETO,DREF,TETA,XA,YA,PHILN,XB,YB,6,MT,XP,YP,
     *             FF)
	CLOSE(22)

      OPEN(22,file=NOM1//NOM//'COMP'//NUMBER//'.DES')
c     WRITE(*,*)'COMP.DES'
      CALL DEFOR1 (COMP,22,NETO,DREF,TETA,XA,YA,PHILN,XB,YB,7,MT,XP,YP,
     *             FF)
	CLOSE(22)

      OPEN(22,file=NOM1//NOM//'RY'//NUMBER//'.DES')
c     WRITE(*,*)'RY.DES'
      CALL DEFOR1 (RY,22,NETO,DREF,TETA,XA,YA,PHILN,XB,YB,8,MT,XP,YP,
     *             FF)
	CLOSE(22)

c  --- Cas de Charges ------
      OPEN(22,file=NOM1//NOM//'PRESS'//NUMBER//'.DES')
c     WRITE(*,*)'PRESS.DES'
      CALL DEFOR1 (PRES(1,1,IS),
     *               22,NETO,DREF,TETA,XA,YA,PHILN,XB,YB,9,MT,XP,YP,FF)
	CLOSE(22)

      IF(IS.EQ.1) THEN
       OPEN(22,file=NOM1//NOM//'POIDS.DES')
c      WRITE(*,*)'POIDS.DES'
       CALL DEFOR1 (PRES(1,1,NSOL+1),
     *               22,NETO,DREF,TETA,XA,YA,PHILN,XB,YB,14,MT,XP,YP,FF)
	 CLOSE(22)

       OPEN(22,file=NOM1//NOM//'CHA1.DES')
c      WRITE(*,*)'CHA1.DES'
       CALL DEFOR1 (PRES(1,1,NSOL+2),
     *               22,NETO,DREF,TETA,XA,YA,PHILN,XB,YB,15,MT,XP,YP,FF)
	 CLOSE(22)

       OPEN(22,file=NOM1//NOM//'CHA2.DES')
c      WRITE(*,*)'CHA2.DES'
       CALL DEFOR1 (PRES(1,1,NSOL+3),
     *               22,NETO,DREF,TETA,XA,YA,PHILN,XB,YB,16,MT,XP,YP,FF)
	 CLOSE(22)

       OPEN(22,file=NOM1//NOM//'MESH.DES')
c      WRITE(*,*)'MESH.DES'
       CALL DEFOR1 (PRES(1,1,NSOL+4),
     *               22,NETO,DREF,TETA,XA,YA,PHILN,XB,YB,17,MT,XP,YP,FF)
	 CLOSE(22)
	ENDIF

C ----- cadres -------
      IF(INDAIG.EQ.0)GOTO 10   ! 9
      OPEN(22,file=NOM1//NOM//'S cadre'//NUMBER//'.DES')
c     WRITE(*,*)'S cadre.DES'  ! (Sy JAS)
      CALL DEFOR1 (SAIG,22,NETO,DREF,TETA,XA,YA,PHILN,XB,YB,10,MT,XP,YP
     *             ,FF)
	CLOSE(22)

      OPEN(22,file=NOM1//NOM//'T cadre'//NUMBER//'.DES')
c     WRITE(*,*)'T cadre.DES'  ! (Tau JAB)
      CALL DEFOR1 (TAIG,22,NETO,DREF,TETA,XA,YA,PHILN,XB,YB,11,MT,XP,YP
     *             ,FF)
	CLOSE(22)

C ---- RAID ----------
c 9   IF(INDRAI.EQ.0) GOTO 10
c     OPEN(22,file=NOM1//NOM//'S raid'//NUMBER//'.DES')
      OPEN(22,file=NOM1//NOM//'Sc JAS'//NUMBER//'.DES')
c     WRITE(*,*)'S raid.DES'
c     WRITE(*,*)'Sc JAS.DES'
      CALL DEFOR1 (SCJAS,22,NETO,DREF,TETA,XA,YA,PHILN,XB,YB,12,MT,XP,
     *             YP,FF)
	CLOSE(22)

c     OPEN(22,file=NOM1//NOM//'T raid'//NUMBER//'.DES')
      OPEN(22,file=NOM1//NOM//'Sc JAB'//NUMBER//'.DES')
c     WRITE(*,*)'T raid.DES'
c     WRITE(*,*)'Sc JAB.DES'
      CALL DEFOR1 (SCJAB,22,NETO,DREF,TETA,XA,YA,PHILN,XB,YB,13,MT,XP,
     *             YP,FF)
	CLOSE(22)

  10  CONTINUE
C
C    TRACES DE VARIABLES COMBINEES.
C
      OPEN(22,file=NOM1//NOM//'W-V'//NUMBER//'.DES')
c      WRITE(*,*)'W-V.DES'
      CALL DEFOR2 (W,V,22,NETO,DREF,TETA,XA,YA,PHILN,XB,YB,1,
     *        MT,XP,YP)
	CLOSE(22)

      OPEN(22,file=NOM1//NOM//'RY-NY'//NUMBER//'.DES')
c      WRITE(*,*)'RY-NY.DES'
      CALL DEFOR2 (RY,NY,22,NETO,DREF,TETA,XA,YA,PHILN,XB,YB,2,
     *        MT,XP,YP)
	CLOSE(22)
C
C    TRACES DE VARIABLES ISOLEES (VUE EN PERPECTIVE)
C
      OPEN(22,file=NOM1//NOM//'U'//NUMBER//'.DES')
c      WRITE(*,*)'U.DES'
      CALL DEFOR3 (U,22,NETO,DREF,TETA,XA,YA,PHILN,XB,YB,1,MT,XP,YP)
	CLOSE(22)

      OPEN(22,file=NOM1//NOM//'NX'//NUMBER//'.DES')
c      WRITE(*,*)'NX.DES'
      CALL DEFOR3 (NX,22,NETO,DREF,TETA,XA,YA,PHILN,XB,YB,2,MT,XP,YP)
	CLOSE(22)

      OPEN(22,file=NOM1//NOM//'SX'//NUMBER//'.DES')
c     WRITE(*,*)'SX.DES'
      CALL DEFOR3 (SX,22,NETO,DREF,TETA,XA,YA,PHILN,XB,YB,3,MT,XP,YP)
	CLOSE(22)

   11 CONTINUE  ! Boucle sur le nbre de charges

      RETURN
      END
C
C
C
      SUBROUTINE RAIDIS(C,X,Y,HXTR,WXTR,DELTA,KST)
C     ___________________________________________
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
C
C
      SUBROUTINE READ (B,A,NEL,CARACT,JDEF,JSTAND,ICOUPE,NUM,NETO,IS)
C     ____________________________________________
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*8 CARACT
      DIMENSION A(11,5,NETO),B(11,NETO),JDEF(NETO)
      COMMON/LANGUE/LANGUE ! 1 French, 2 English

c	WRITE(*,*) CARACT

      IF(IS.NE.1) GOTO 99
      IF(NEL.NE.1)GOTO 99
      JJ=JSTAND
      IF(ICOUPE.EQ.1) THEN
       WRITE(*,*)' '
       WRITE(*,*)'QUELLE COUPE VOULEZ VOUS POUR LA VARIABLE ',
     *            CARACT,' ?'
       WRITE(*,*)'TAPEZ VOTRE CHOIX (coupe 1 à 5). Par défaut coupe ',
     *            'nø',JSTAND
       READ(*,*) JJ
       WRITE(*,*)' '
      ENDIF
      JDEF(NUM)=JJ
  99  JJ=JDEF(NUM)

      DO 1  J=1,5
        READ(43) (A(K,J,NEL),K=1,11)
   1  CONTINUE
      DO 2  K=1,11
        B(K,NEL)=A(K,JJ,NEL)
   2  CONTINUE
      RETURN
      END
C
C
C
      SUBROUTINE EXTREM (F,VMAX,NETO)
C     ________________________________
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION F(11,NETO)
C
      VMAX=F(1,1)
      VMIN=F(1,1)
      DO 1 I=1,NETO
      DO 1 K=1,11
      VMAX=DMAX1(VMAX,F(K,I))
 1    VMIN=DMIN1(VMIN,F(K,I))
      IF(DABS(VMAX).LE.DABS(VMIN)) VMAX=VMIN
      VMAX=DABS(VMAX)
      RETURN
      END

C
C
C     DESSIN DES VARIABLES NON COMBINEES, COUPE TRANSV.
C     **************************************************
      SUBROUTINE DEFOR1 (F,IFICH,NETO,DREF,TETA,XA,YA,PHILN,XB,YB,L,
     *             MT,XP,YP,FF)
C
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*40 TITRE(17),TITLE(17)
      CHARACTER*6 UNITE(17),UNIT(17)
      DIMENSION F(11,NETO),XB(11,NETO),YB(11,NETO),
     *  TETA(NETO),PHILN(NETO),XA(11,NETO),YA(11,NETO),
     *  MT(NETO),XP(NETO,10,4),YP(NETO,10,4),FF(11,NETO)
      COMMON/PY/PI
	COMMON/LANGUE/LANGUE

      DATA TITRE/'MOMENT Unitaire, Mx, selon axe OX       ',
     &           'MOMENT Unitaire, My, selon axe OY       ',
     &           'CISAILLEMENT Unitaire, Nxy (plan OXY)   ',
     &           'REACTION Unitaire Rx, bord OY(avec Raid)',
     &           'CONTRAINTES selon OY (bordage, Z=0)     ',
     &           'CONTRAINTES TAU xy (bordage, Z=0)       ',
     &           'CONTR. de von Mises (bordage, Z=0)      ',
     &           'REACTION Unitaire Ry, bord Ox(avec Cadr)',
     &           'PRESSION HYDROSTATIQUE(Equiv. metre eau)',
     &           'Contr. Sy      - cadres(JAS), selon OY  ',
     &           'Txy AME (JAB)  - cadres(JAB)            ',
     &           'Contr. Comp    - cadres(JAS)            ',
     &           'Contr. Comp    - cadres(JAB)            ',
c    &           'CONTR. Semelle - raid  (JAS) , selon OX ',
c    &           'CONTR. TAU AME - raid. (JAB)            ',
     &           'Distribution du poids propre            ',
     &           'CAS DE CHARGE CHA1 (charge verticale)   ',
     &           'CAS DE CHARGE CHA2-CHA3 (pression)      ',
     &           'Modelisaton de la structure             '/
      DATA UNITE/' N.m  ',' N.m  ',' N.   ',' N.   ',' N/m2 ',
     &           ' N/m2 ',' N/m2 ',' N.   ','m(EAU)',
     &           ' N/m2 ',' N/m2 ',' N/m2 ',' N/m2 ',
     &           ' N/m  ',' N/m2 ','m(EAU)','      '/
      DATA TITLE /'Bending MOMENT,Unitary,(Mx)along OX axis',
     &           'Bending MOMENT,Unitary,(My)along OY axis',
     &           'Shear Force,Unitary, Nxy, in-plane OXY  ',
     &           'REACTION, Unitary,Rx,Edge OY(with Stiff)',
     &           'STRESS, in-plane, along OY (plate, Z=0) ',
     &           'Shear Stress, in-plane, Txy (plate, Z=0)',
     &           'COMBINED STRESS,von Mises (plate, Z=0)  ',
     &           'REACTION,Unitary,Ry, Edge OX(with frame)',
     &           'LATERAL PRESSURE (Equiv. Hydrostatic P.)',
     &           'STRESS Flange(JWF) - Frames- along OY   ',
     &           'SHEAR STRESS, WEB(JWP) - Frames         ',
     &           'Combined STRESS Flange(JWF) - Frames    ',
     &           'Combined STRESS Flange(JWB) - Frames    ',
c    &           'STRESS Flange(JWF) - Stiff.- along OX   ',
c    &           'SHEAR STRESS, WEB(JWP) - Stiff.         ',
     &           'DEADWEIGHT                              ',
     &           'LOADS CHA1 (vertical load)              ',
     &           'LOADS CHA2-CHA3 (pressure)              ',
     &           'Mesh Model of the Structure             '/
      DATA UNIT /' N.m  ',' N.m  ',' N.   ',' N.   ',' N/m2 ',
     &           ' N/m2 ',' N/m2 ',' N.   ','m(H2O)',
     &           ' N/m2 ',' N/m2 ',' N/m2 ',' N/m2 ',
     &           ' N/m  ',' N/m2 ','m(H2O)','      '/
c
      IF(LANGUE.NE.1) THEN
        DO I=1,17
	    TITRE(I)=TITLE(I)
	    UNITE(I)=UNIT(I)
	  ENDDO
	ENDIF
C
      CALL EXTREM (F,VMAX,NETO)
C
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
  3   YB(K,I)=YA(K,I)-FF(K,I)*S1

      WRITE(IFICH,*) VMAX,DREF
      WRITE(IFICH,*) UNITE(L)
      WRITE(IFICH,*) TITRE(L)

      DO 6 I=1,NETO
        WRITE(IFICH,*) I,MT(I)
        DO 5 K=1,11
          WRITE(IFICH,100) I,XA(K,I),YA(K,I),XB(K,I),YB(K,I)
 5      CONTINUE
        IF(MT(I).EQ.0) GOTO 6
        DO 4 K=1,MT(I)
          WRITE(IFICH,100) I,XP(I,K,1),YP(I,K,1),XP(I,K,2),YP(I,K,2)
          WRITE(IFICH,100) I,XP(I,K,3),YP(I,K,3),XP(I,K,4),YP(I,K,4)
 4      CONTINUE
 6    CONTINUE

  100 FORMAT(1X,I3,2X,F12.4,5X,F12.4,5X,F12.4,5X,F12.4)		!extension neto
      RETURN
      END


C
C     DESSIN DES VARIABLES COMBINEES, COUPE TRANSV.
C     *********************************************
      SUBROUTINE DEFOR2(F,G,IFICH,NETO,DREF,TETA,XA,YA,PHILN,XB,YB,L,
     *             MT,XP,YP)
C
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*40 TITRE(2),TITLE(2)
      CHARACTER*6 UNITE(2)
      DIMENSION F(11,NETO),G(11,NETO),XB(11,NETO),YB(11,NETO),
     *  TETA(NETO),PHILN(NETO),XA(11,NETO),YA(11,NETO),
     *  MT(NETO),XP(NETO,10,4),YP(NETO,10,4)
      COMMON/PY/PI
      COMMON/LANGUE/LANGUE ! 1 French, 2 English

      DATA TITRE/'DEPLACEMENTS Combinés, V-W (plan OYZ)   ',
     &           'EFFORTS COMBINES RY-NY (sur un axe OY)  '/
      DATA TITLE/'COMBINED DISPLACEMENTS, V-W (in OYZ)    ',
     &           'COMBINED FORCED, RY-NY (on a OY line)   '/

      DATA UNITE/' m.   ',' N.   '/

      IF(LANGUE.NE.1) THEN
        DO I=1,2
	    TITRE(I)=TITLE(I)
	  ENDDO
	ENDIF
C
      CALL EXTREM (F,VMAXF,NETO)
      CALL EXTREM (G,VMAXG,NETO)
      VMAX=DMAX1(VMAXG,VMAXF)
C
      IF(VMAX.LE.1.E-20) VMAX=1.E-20
      ECH=DREF/VMAX
      DO 2 I=1,NETO
      DO 2 K=1,11
      F(K,I)=F(K,I)*ECH
 2    G(K,I)=G(K,I)*ECH
C
C
      DO 3 I=1,NETO
      PHI=DABS(PHILN(I)) * 0.1
      DO 3 K=1,11
      S1=VSIN(TETA(I),(K-1)*PHI)
      C1=VCOS(TETA(I),(K-1)*PHI)
      XB(K,I)=XA(K,I)-F(K,I)*C1+G(K,I)*S1
  3   YB(K,I)=YA(K,I)-F(K,I)*S1-G(K,I)*C1
C
C
      LLL=L+8
      WRITE(IFICH,*) VMAX,DREF
      WRITE(IFICH,*) UNITE(L)
      WRITE(IFICH,*) TITRE(L)
      DO 6 I=1,NETO
      WRITE(IFICH,*) I,MT(I)
      DO 5 K=1,11
 5    WRITE(IFICH,100) I,XA(K,I),YA(K,I),XB(K,I),YB(K,I)
        IF(MT(I).EQ.0) GOTO 6
      DO 4 K=1,MT(I)
      WRITE(IFICH,100) I,XP(I,K,1),YP(I,K,1),XP(I,K,2),YP(I,K,2)
 4    WRITE(IFICH,100) I,XP(I,K,3),YP(I,K,3),XP(I,K,4),YP(I,K,4)
 6    CONTINUE
  100 FORMAT(1X,I3,2X,F12.4,5X,F12.4,5X,F12.4,5X,F12.4)		!extension neto
      RETURN
      END


C
C     DESSIN DES VARIABLES NON COMBINEES, PERPECTIVE
C     **********************************************
      SUBROUTINE DEFOR3 (F,IFICH,NETO,DREF,TETA,XA,YA,PHILN,XB,YB,L,
     *             MT,XP,YP)
C
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*40 TITRE(3),TITLE(3)
      CHARACTER*6 UNITE(3)
      DIMENSION F(11,NETO),XB(11,NETO),YB(11,NETO),
     *  TETA(NETO),PHILN(NETO),XA(11,NETO),YA(11,NETO),
     *  MT(NETO),XP(NETO,10,4),YP(NETO,10,4)
      COMMON/PY/PI
      COMMON/LANGUE/LANGUE ! 1 French, 2 English

      DATA TITRE/'DEPLACEMENT U   (selon OX)              ',
     *           'EFFORT NORMAL NX, Unitaire (selon OX)   ',
     *           'CONTRAINTES selon OX  (bordage, z=0)    '/
      DATA TITLE/'DISPLACEMENT U, in-plane   (along OX)   ',
     *           'UNITARY FORCES,NX,(along OX)(With Stiff)',
     *           'LONG. STRESS along OX  (Plate, Z=0)     '/
      DATA UNITE/' m.   ',' N.   ',' N/m2 '/

      IF(LANGUE.NE.1) THEN
        DO I=1,3
	    TITRE(I)=TITLE(I)
	  ENDDO
	ENDIF
C
      CALL EXTREM (F,VMAX,NETO)
C
      IF(VMAX.LE.1.E-20) VMAX=1.E-20
        DREF3=3.*DREF
        ECH=DREF3/VMAX
      DO 2 I=1,NETO
      DO 2 K=1,11
 2    F(K,I)=F(K,I)*ECH
C
C
      S1=DSIN(PI/6.)
      C1=DCOS(PI/6.)
      DO 3 I=1,NETO
      DO 3 K=1,11
      XB(K,I)=XA(K,I)+F(K,I)*C1
  3   YB(K,I)=YA(K,I)+F(K,I)*S1
C
C
      LLL=L+10
      WRITE(IFICH,*) VMAX,DREF3
      WRITE(IFICH,*) UNITE(L)
      WRITE(IFICH,*) TITRE(L)
      DO 6 I=1,NETO
      WRITE(IFICH,*) I,MT(I)
      DO 5 K=1,11
 5    WRITE(IFICH,100) I,XA(K,I),YA(K,I),XB(K,I),YB(K,I)
        IF(MT(I).EQ.0) GOTO 6
      DO 4 K=1,MT(I)
      WRITE(IFICH,100) I,XP(I,K,1),YP(I,K,1),XP(I,K,2),YP(I,K,2)
 4    WRITE(IFICH,100) I,XP(I,K,3),YP(I,K,3),XP(I,K,4),YP(I,K,4)
 6    CONTINUE

  100 FORMAT(1X,I3,2X,F12.4,5X,F12.4,5X,F12.4,5X,F12.4)		!extension neto
      RETURN
      END
