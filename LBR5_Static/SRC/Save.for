      SUBROUTINE SAVE(NEL,INDAIG,INDRAI,EFF,ITYPE,ISECT)

C     ***********************************************************************
C     Subroutine de sauvetage des données en vue des dessins

C     Modifié : 8-8-94                     Créer: Thèse de Ph. Rigo
C               Avril 99 (LBR5.1)
C                1-11-2000 (version anglaise)
C               25-02-2001 Impression de tous les cas de charge.
c               15-12-2002 Impression de 4 types de Contr. dans les cadres (au lieu des raid.)
c			  04-02-2004 Impression des contraintes des épontilles à la place de certaintes autres contraintes
C     ***********************************************************************

      IMPLICIT REAL *8 (A-H,O-Z)

      DIMENSION EFF(9690),ICODE(19)
	DIMENSION P(1275)			!Vecteur de travail		!février 2004
      COMMON/GRAF/NCAS,ICODE
c     COMMON/DI/EFF

      IF(NEL.NE.1) GOTO 20
	                                               
C     DESCRIPTION  :   VARIABLES
C     ***********      *********
      NCAS=10             !février 2004
      ICODE(1)=1          ! V
      ICODE(2)=2          ! U
      ICODE(3)=3          ! W
      ICODE(4)=5          ! NX
      ICODE(5)=6          ! NY
      ICODE(6)=7          ! NXY
      ICODE(7)=9          ! MX
      ICODE(8)=10         ! MY
      ICODE(9)=15         ! RY
      ICODE(10)=16        ! RX
      ICODE(11)=17        ! SY
      ICODE(12)=18        ! SX
      ICODE(13)=19        ! TXY
      ICODE(14)=20        ! COMP
                          !
      ICODE(15)=24        ! Tau Cadre JAB		Tau épon a.n.
      ICODE(16)=21        ! Sig Cadre JAS		Sig épon JAS
                          !
      ICODE(17)=25        ! Sig COMP JAS (cadre)  le 15-12-2002
      ICODE(18)=26        ! Sig COMP JAB (cadre)

c      ICODE(17)=34        ! Tau Raid  JAB		
c      ICODE(18)=30        ! Sig Raid  JAS		
    
  20  CONTINUE
      DO 12 I=1,NCAS
      DO 12 J=1,5
       K1=(ICODE(I)-1)*255+1+(J-1)*51
       K2=K1+30
  12  WRITE(43)(EFF(K),K=K1,K2,3)

	IF(ITYPE.NE.5) THEN					!février 2004
      DO 18 I=11,14						!février 2004
      DO 18 J=1,5							!février 2004
      K1=(ICODE(I)-1)*255+1+(J-1)*51		!février 2004
      K2=K1+30							!février 2004
  18  WRITE(43)(EFF(K),K=K1,K2,3)			!février 2004
	GOTO 19								!février 2004
	ELSE				! si épontille	!février 2004
 25	IF(I.GT.14) GOTO 19					!février 2004
	L=I-10								!février 2004
 	GOTO (24,23,24,24),L				!février 2004
 24   DO 22 J=1,5							!février 2004
	ICODE(11)=23				!Sy an  !février 2004                  		
      ICODE(13)=20				!T an	!février 2004
	ICODE(14)=23				!Sy an	!février 2004
      K1=(ICODE(I)-1)*255+1+(J-1)*51		!février 2004
      K2=K1+30							!février 2004
  22  WRITE(43)(EFF(K),K=K1,K2,3)			!février 2004
	 I=I+1								!février 2004
	GOTO 25								!février 2004
	
  23  DO 21 J=1,5							!février 2004
      K1=(ICODE(12)-1)*255+1+(J-1)*51		!février 2004
      K2=K1+30							!février 2004
	P(K)=0.0000							!février 2004
  21  WRITE(43)(P(K),K=K1,K2,3)			!février 2004
	I=I+1								!février 2004
	GOTO 25								!février 2004
	ENDIF								!février 2004
c     IF(INDAIG.EQ.0)  GOTO 15
c     DO 13 I=15,16
  19  CONTINUE							!février 2004
	IF(INDAIG.EQ.0) RETURN ! le 15-12-2002
      DO 13 I=15,18          ! le 15-12-2002
      DO 13 J=1,5
 	IF(ITYPE.EQ.5)THEN											!février 2004
	ICODE(15)=20                          ! Tau épon JAB		!février 2004
      ICODE(16)=17                          ! Sig épon JAS sup	!février 2004
	ICODE(17)=21						  ! Sig comp JAS sup	!février 2004
	ICODE(18)=23						  ! Sig comp a.n.		!février 2004
	ENDIF														!février 2004
      K1=(ICODE(I)-1)*255+1+(J-1)*51
       K2=K1+30
  13  WRITE(43)(EFF(K),K=K1,K2,3)

      RETURN                ! le 15-12-2002

  15  CONTINUE
      IF(INDRAI.EQ.0) GOTO 16
      DO 14 I=17,18
      DO 14 J=1,5
 	IF(ITYPE.EQ.5)THEN											!février 2004
	ICODE(17)=21						  ! Sig comp JAS sup	!février 2004
	ICODE(18)=23						  ! Sig comp a.n.		!février 2004
	ENDIF														!février 2004
       K1=(ICODE(I)-1)*255+1+(J-1)*51
       K2=K1+30
  14  WRITE(43)(EFF(K),K=K1,K2,3)

  16  RETURN
      END
