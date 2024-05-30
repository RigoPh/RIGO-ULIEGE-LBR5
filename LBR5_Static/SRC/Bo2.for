      SUBROUTINE BO2(NETO,ITYPE,NEL,JLMAX,INDAIG,INDRAI,IMPR,E,
     *		ETA,SIGY,SIGM,DIS,NE,FAM,DZSN,ZSN,
     *		WIDTH,SOLT,ABC,A,B,C,D,AM,BM,CM,DM,
     *		IFONCT,ICHA,IMOM,IPT,IPRINT,IS,NSOL,IM,IMG,ITERA,
     *		SENS1,DELT,DI,I66,I67,PLOC,
     *		TETAQ,
     *		NVAR,M1TABL,M2CONT,NXIT,IPTS,YPTS,IPTS2,IPTS3,
     *		LCONT,CJMAX,INV,NXI2,TRAV,ISECT,					!février 2004	!r&d13
     *		Z,PHILN,TETAS,PART,YNEUTPART,SYM,SYMX,BM1,CORRO)	!r&d13


      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION DI(1),TRAV(1)
      COMMON/DIM1/ Nmax,NEmax,NVmax,
     *             M1max,M1Tmax,M2max,M2Tmax,Mmax,NGmax,ISmax,IPTmax   !Dimension max. des vecteurs

c     DI(1) à 17077  DTPL,TPLD,TPLA,TPLR,
c                    LAMB,Q,DELTA,PHIL,
c                    DYA,TYA,WYA,HYA,EPSA,KSA
c                    HXR,DXR,WXR,TXR,EPSR,ENTR,KSR
      I1 =1+21				!EFF(9690)
      I2 =I1+9690				!EFF2(5400) = EFF2(2040)+TEMP(1530)+TEMP2(1530)+TEMP3(300)
      I3 =I2+5400				!CONC(750)
      I4 =I3+750				!Z2(2295)
      I5 =I4+2295				!SXM(IPTmax+2*NETO+11)
      I6 =I5+IPTmax+2*NETO+11	!USHULL(12*9*NETO=10800) 
	 I66=I5+IPTmax       ! Compteur pour utilisation dans MAIN
	 I67=I5+IPTmax+NETO  ! Compteur pour utilisation dans MAIN

      I7 =I6+12*9*NETO		!DISH
      I8 =I7+1710*NSOL		!DISB
      I9 =I8+720				!DISC
      I10=I9+720				!DISA
      I11=I10+720				!DISD
      I12=I11+720					!SENS2(16,IPT,9,NETO)                         !avril2003
      I13=I12+16*IPTmax*9*NETO    !SENS3(21,IPT,9,NETO), Vecteur pour Subr RESUL !avril2003
      I14=I13+21*IPTmax*9*NETO    !SENS4( 9,IPT,9,NETO), Idem SENS1 mais cumule pour Subr. CONTR
      Imax=I14+9*IPTmax*9*NETO	
	!   =  21.048 + 1710 *NSOL + 414*IPTmax*NETO +110*NETO + IPTMAX    !avril2003
	!   =  21.048 + 17.100     + 165.600         +11.000   + 4          !avril2003
	!   = 214.752  pour NETO = 100 et NSOL = 10 et IPT=4              !avril2003
      
      J1=1                   !XI(NSOL)
      J2=J1+NSOL             !XF(NSOL)
      J3=J2+NSOL             !CHAMAX(5,NSOL)
      J4=J3+5*NSOL           !DC(9*NETO)
      J5=J4+9*NETO           !VS(8*9=72)  Vect.Travail pour STIFF:dSIG1,dSIG2,dSIG3,dSIG4,dTau,dFlr,dWpl1,dWpl2 !avril2003
      J6=J5+72               !Plat(NETO)  !avril2003
      J7=J6+NETO             !Fl(NETO)
      J8=J7+NETO             !SIG1(NETO)
      J9=J8+NETO             !SIG2(NETO)
	J10=J9+NETO            !SIG3(NETO) !avril2003
	J11=J10+NETO           !SIG4(NETO) !avril2003
      J12=J11+NETO           !TAU (NETO) !avril2003
      J13=J12+NETO           !Wpl1(NETO) !avril2003
      J14=J13+NETO           !Wpl2(NETO) !avril2003
	J15=J14+NETO           !VMAXSIG(NETO)  !avril2003
	J16=J15+NETO           !VMAXSIGC(NETO) !avril2003
	J17=J16+NETO           !SIGMAG(NETO)   !avril2003
	J18=J17+NETO           !SigPlaque(IPTmax)    !avril2003
	J19=J18+IPTmax         !dSigPlaque(IPTmax*9) !avril2003
	J20=J19+IPTmax*9       !SIGXPlaque(IPTmax*NETO)  !fev04
	J21=J20+IPTmax*NETO    !SIGYPlaque(IPTmax*NETO)  !fev04
	J22=J21+IPTmax*NETO    !PHIPlaque(IPTmax*NETO)   !fev04
	J23=J22+IPTmax*NETO    !SIGVMTPlaque(IPTmax*NETO)!fev04
	J24=J23+IPTmax*NETO    !SIGVMCPlaque(IPTmax*NETO)!fev04
	J25=J24+IPTmax*NETO    !INDPlaque(IPTmax*NETO)   !fev04
      J26=J25+IPTmax*NETO    !SIGMX(IPTmax*NETO)       !fev04
	J27=J26+IPTmax*NETO    !SIGMY(IPTmax*NETO)       !fev04
      J28=J27+IPTmax*NETO	   !dCHAMAX(5,NSOL,9)		!!!juillet04
      J29=J28+5*NSOL*9	   !           !fev04		!!!juillet04	!dDSIGPL(9,NETO)  !r&d13
	J30=J29+9*NETO		   !WORK(99*NETO+5)			 !r&d13
	J31=J30+99*NETO+5	   !dDSIGSTIF(9,NETO)
	J32=J31+9*NETO		   !dSIGX(9,NETO)			 !r&d13
	J33=J32+9*NETO

      Jmax=J33-1    ! = 7NSOL+9NETO+72+12*NETO+10*IPTmax+8*IPTmax*NETO+5*NSOL*9+9NETO+99NETO+5+9NETO+9NETO	!!!aout04	!r&d13
	              ! = 7*10+9*150+72+12*150+10*4+8*4*150+5*10*9+9*150+99*150+5+9*150+9*150=27487			!fev04	!!!aout04	!extension neto  !r&d13

             
      IF(NEL.EQ.1) CALL ANNULD(TRAV,Jmax)  ! Mise à zéro (au début de chaque cas de charge)
      CALL ANNULD(TRAV,J6-1)  ! il ne faut pas mettre à zéro TRAV(J8àJ15) ! avril2003
	                        ! car stokage des résultats de la subr. STIFF

      CALL BO2A(NETO,ITYPE,NEL,JLMAX,INDAIG,INDRAI,IMPR,E,ETA,PLOC,
     * SIGY,SIGM,DIS,NE,FAM,DZSN,ZSN,WIDTH,SOLT,ABC,A,B,C,D,AM,BM,CM,DM,
     * IFONCT,ICHA,IMOM,IPT,IPRINT,IS,NSOL,IM,IMG,ITERA,
     * SENS1,DELT,TETAQ,
     * DI(1),DI(2),DI(3),DI(4),						   !DTPL,TPLD,TPLA,TPLR,
     * DI(5),DI(6),DI(7),DI(8),  ! 48						   !LAMB,Q,DELTA,PHIL,
     * DI( 9),DI(10),DI(11),DI(12),DI(13),DI(14),        !DYA,TYA,WYA,HYA,EPSA,KSA
     * DI(15),DI(16),DI(17),DI(18),DI(19),DI(20),DI(21), !HXR,DXR,WXR,TXR,EPSR,ENTR,KSR
     * DI(I1),DI(I2),DI(I3),DI(I4),           !EFF(9690),EFF2(5400),CONC(750),Z2(2295)
     * DI(I5),DI(I6),							!SXM(IPT+2NETO+11),USHULL(12*9*NETO=3240)
     * DI(I7),DI(I8),DI(I9),DI(I10),DI(I11),	!DISH,DISB,DISC,DISA,DISD,   
     * DI(I12),DI(I13),DI(I14),				!SENS2(16,4,9,NETO),SENS3(20,4,9,NETO),SENS4(9,4,9,NETO)  !avril2003
     * TRAV(J1),TRAV(J2),TRAV(J3),TRAV(J4),	!XI,XF,CHAMAX,DC (vecteurs de travail)
     * TRAV(J5),TRAV(J6),TRAV(J7),            !VS(54+2*9),Plat(NETO),Fl(NETO), !avril2003
     * TRAV(J8),TRAV(J9),TRAV(J10),TRAV(J11),TRAV(J12),
     * TRAV(J13),TRAV(J14),TRAV(J15),TRAV(J16),TRAV(J17),!SIG1(NETO),SIG2,SIG3,SIG4,      !avril2003
c                                                TAU,Wpl1,Wpl2(NETO),VMAXSIG,VMAXSIGC     !avril2003
     * TRAV(J18),TRAV(J19),                   !SigPlaque,DSigPlaque                       !avril2003
     * TRAV(J20),TRAV(J21),TRAV(J22),TRAV(J23),TRAV(J24),TRAV(J25),!SIGXPlaque,SIGYPlaque,PHIPlaque,SIGVMTPlaque,SIGVMCPlaque,INDPlaque
     * TRAV(J26),TRAV(J27), !SIGMX,SIGMY    !fev04
     * TRAV(J28),			  !dCHAMAX		!!!aout04
     * TRAV(J29),TRAV(J30),TRAV(J31),TRAV(J32),		!dDSIGPL,WORK,dDSIGSTIF,dSIGX	  !r&d13
     * NVAR,M1TABL,M2CONT,NXIT,IPTS,YPTS,IPTS2,IPTS3,
     * LCONT,CJMAX,INV,NXI2,ISECT,						!section épontille	!février 2004  !r&d13
     * Z,PHILN,TETAS,PART,YNEUTPART,SYM,SYMX,BM1,CORRO)	!r&d13
   
      RETURN
      END

C***********************************************************************
C***********************************************************************
C***********************************************************************
C***********************************************************************
C***********************************************************************
C***********************************************************************

      SUBROUTINE BO2A(NETO,ITYPE,NEL,JLMAX,INDAIG,INDRAI,IMPR,E,ETA,PLOC
     *,SIGY,SIGM,DIS,NE,FAM,DZSN,ZSN,WIDTH,SOLT,ABC,A,B,C,D,AM,BM,CM,DM,
     *           IFONCT,ICHA,IMOM,IPT,IPRINT,IS,NSOL,IM,IMG,ITERA,
     *           SENS1,DELT,TETAQ,
     *           DTPL,TPLD,TPLA,TPLR,
     *           LAMB,Q,DELTA,PHIL,
     *           DYA,TYA,WYA,HYA,EPSA,KSA,HXR,DXR,WXR,TXR,EPSR,ENTR,KSR,
     *           EFF,EFF2,CONC,Z2,
     *           SXM,US,
     *           DISH,DISB,DISC,DISA,DISD,SENS2,SENS3,SENS4,
     *           XI,XF,CHAMAX,DC,
     *           VS,Plat,Fl,
     *           SIG1,SIG2,SIG3,SIG4,TAU,            !avril2003
     *           Wpl1,Wpl2,VMAXSIG,VMAXSIGC,SIGMAG,  !avril2003
     *           SigPlaque,DSigPlaque,               !avril2003
     *           SIGXPlaque,SIGYPlaque,PHIPlaque,                  !fev04
     *           SIGVMTPlaque,SIGVMCPlaque,INDPlaque,SIGMX,SIGMY,  !fev04
     *		   dCHAMAX,								!!!aout04
     *		   dDSIGPL,WORK,dDSIGSTIF,dSIGX,			!r&d13
     *           NVAR,M1TABL,M2CONT,NXIT,IPTS,YPTS,IPTS2,IPTS3,
     *           LCONT,CJMAX,INV,NXI2,ISECT,						!février 2004  !r&d13
     *		   Z,PHILN,TETAS,PART,YNEUTPART,SYM,SYMX,BM1,CORRO)	!r&d13


C***********************************************************************
C     SUBROUTINE BO2
C     ===============
C     SUBROUTINE DE MISE EN FORME DES RESULTATS PAR SOMMATION SUR LES   
C     INCONNUES HYPERSTATIQUES DU SYSTEME.
C     (LA RESOLUTION SE FAIT PANNEAU PAR PANNEAU ).                     
C                                                                       
C    Créer : Thèse de doctorat, Ph. Rigo (1987)	
c    ------
C    Modif :9-02-96 				    	
C    ------ 6-05-96 : Subr. HUGHES, Read(99), CHA2M et CHA3M
C          10-05-96 : COMMON/DI/ ... TPL, ...
C          31-05-96 : Modification des arguments de la Subr. HUGHES
C          22-07-96 : Subr CONTR dans BO2 et développement de la subr PAIK
C          18-02-97 : Subr USHULL pour calcul de la resistance ultime 
C           6-03-97 : Introduction de D(raid) c.à.d. ENTR différent de EPSR 
C          23-03-99 : Modification de l'allocation mémoire 
C          20-11-00 : Introduction de la subr. STIFF (flexion locale)
C          15-01-02 : Modification subr. Hughes (et impression dans SOL2)
C          15-04-03 : Combinaison RESUL + STIFF (F. Bair)
c           4-02-04 : Subr PLAQUE pour RESUL + STIFF + PLTBEN (Bair)
C		 04-02-04 : Epontilles
c
C   Dernière modif: 04-02-2004
C   ---------------
C***********************************************************************
      IMPLICIT REAL*8(A-H,O-Z)
      INTEGER*4 DIM
	INTEGER SYM,SYMX			!r&d13
      REAL*8 LAMB

      DIMENSION CONST(74),HXTR(10),WXTR(10),TXTR(10),DXTR(10),ABTR(10),
     *          ARGQ(8),EQU(8),DISL(1710),
     *          FP(20),DIS(5),U(51),FAM(6),NF(22),DW(11,5),
     *          IFONCT(2)
	DIMENSION PLOC(NETO),Plat(NETO),Fl(NETO),
     *          SIG1(NETO),SIG2(NETO),SIG3(NETO),SIG4(NETO),TAU(NETO),  !avril2003
     *          Wpl1(NETO),Wpl2(NETO),VMAXSIG(NETO),VMAXSIGC(NETO),     !avril2003
     *          SIGMAG(NETO),VS(72),SigPlaque(IPTmax),                  !avril2003
     *          DSigPlaque(IPTmax*9)                                    !avril2003

      DIMENSION XI(NSOL),XF(NSOL),CHAMAX(5,NSOL)  !!!

C     En remplacement du common/DI
      DIMENSION EFF(9690),EFF2(5400),CONC(750),Z2(2295),
     *          EFFCOMB(9690), !avril2003
     *          DISH(1710,NSOL),DISA(720),DISB(720),DISC(720),DISD(720),
     *          US(1),
     *          SENS1( 9,IPTmax,9,NETO),SENS2(16,IPTmax,9,NETO),       !avril2003
     *          SENS3(21,IPTmax,9,NETO),SENS4( 9,IPTmax,9,NETO),
     *          SXM(1) !  Voir Subr RESUL, CONTR (Restr. de PAIK )

	DIMENSION DELT(NETO)												!r&d13
	DIMENSION ITYPE(NETO)												!r&d13
	DIMENSION Z(NETO,4),PHILN(NETO),TETAS(NETO),PART(NETO)			    !r&d13 
      DIMENSION ZSN(8,NSOL),DZSN(NE,9*NETO,NSOL),SOLT(NE,NE),
     *          ABC(8*NETO),A(NETO),B(NETO),C(NETO),D(NETO),
     *          AM(NETO),BM(NETO),CM(NETO),DM(NETO)

      DIMENSION NXI2(9,NETO),NVAR(NETO),M1TABL(NETO),M2CONT(NETO),
     *          NXIT(9,NETO),IPTS(NETO),LCONT(2,M1Tmax)

c     Voir COMMOM OPTI2 (IPT = 4 et NSOL=10 a changer manuellement)
      DIMENSION SENSH(13,4,9,10),   !SENSH(13,IPT,9,NSOL), 
     *  SENSA(13,4,9) ,SENSB(13,4,9), SENSC(13,4,9) ,SENSD(13,4,9) ,
     *  SENSAA(13,4,9),SENSBB(13,4,9),SENSCC(13,4,9),SENSDD(13,4,9),
     *    DEFA(13,4),   DEFB(13,4),    DEFC(13,4),    DEFD(13,4),
     *    DEFAA(13,4),  DEFBB(13,4),   DEFCC(13,4),   DEFDD(13,4)

      DIMENSION  DEQU(8),NOM(9),ID(9)

      DIMENSION SIGXPlaque(IPTmax*NETO),SIGYPlaque(IPTmax*NETO), !fev04
     *PHIPlaque(IPTmax*NETO),SIGVMTPlaque(IPTmax*NETO),          !fev04
     *SIGVMCPlaque(IPTmax*NETO),INDPlaque(IPTmax*NETO),          !fev04
     *SIGMX(IPTmax*NETO),SIGMY(IPTmax*NETO)                      !fev04

	DIMENSION dCHAMAX(5,NSOL,9)  !!!aout04

	DIMENSION dDSIGPL(9,NETO),WORK(54*NETO),dDSIGSTIF(9,NETO),	!r&d13
     *		  dSIGX(9,NETO)										!r&d13

	DIMENSION CORRO(NETO,3)			!r&d13

      COMMON/PY/   PI
      COMMON/LANGUE/ LANGUE        ! 1 French (par defaut), 2 English 
      COMMON/DIM1/ Nmax,NEmax,NVmax,
     *             M1max,M1Tmax,M2max,M2Tmax,Mmax,NGmax,ISmax,IPTmax   !Dimension max. des vecteurs
      COMMON/OPTI/ IOPTI,NTOT,M1T,M2T

      COMMON/ECR/  U,MT
      COMMON/ENT/  ABTR,ARGQ,CONST,HXTR,DXTR,WXTR,TXTR
      COMMON/HYP/  HYP(360)
      COMMON/USHULL/FICTIF(9),IULT

      COMMON/OPTI2/DEFA,DEFB,DEFC,DEFD,DEFAA,DEFBB,DEFCC,DEFDD,
     *             SENSH, SENSA, SENSB, SENSC, SENSD,
     *		     SENSAA,SENSBB,SENSCC,SENSDD
      COMMON/OPTI3/BID1(54),BID2(20)         ! Main,ENT,BO1

C***********************************************************************
C	Ordre ds SENS1(JJ,4,9,NETO) avec JJ=1 à 9
      DATA NOM/'W','Wø','Wøø','Wøøø','U','Uø','V','Vø ','Vøø'/
C		      1   2    3     4      5   6    7   8     9 
C	Ordre ds DEFH,DEFA,DEFB, ... et SENSH,SENSA,SENSB, ...
C               U,V,Wø,Ny,Nyx,My,Ry,W,Wøø,Wøøø,Uø,Vø,Vøø
C               1 2 3  4  5    6  7 8  9  10   11 12 13
C	Relation de position entre SENS1 et SENSH,SENSA,...,DEFH,DEFA,DEFB, ...
C	SENS1(I) = Fct[SENSH(IDI)]
      DATA ID/8,3,9,10,1,11,2,12,13/
C***********************************************************************
C***********************************************************************

	IF(IMPR.GE.-1) THEN				!15.10.05
        WRITE(66,73) NEL
	ENDIF							!15.10.05

      CALL ANNULD(EFF2,5400)
      CALL ANNULD(EFF,9690)
	CALL ANNULD(CONC,750)
      ! SENS2 sensibilites cumulees relatives au contraintes (Sc borde, raid, cadre, trav)
      CALL ANNULD(SENS2,16*IPTmax*9*NETO)  ! calcule dans Subr RESUL !avril2003
      ! SENS3 sensibilites cumulees des contraintes non combinees (Sx, Sy, Txy, etc
      CALL ANNULD(SENS3,21*IPTmax*9*NETO)  ! matr. de travail de la Subr. Resul
      ! SENS4 = sensibilites cumulees de SENS1 (V,U,W, etc.
      CALL ANNULD(SENS4, 9*IPTmax*9*NETO)  ! calcules dans BO2


c     READ(97) ABCD     (utile pour subr. EQUIL, calcul des charges, cfr Subr Bateau)                                         
c     READ(98) NPT,CHA  (utile pour subr. EQUIL, calcul des charges)

      READ(99)  ABTR,PHIL,MT,TETA,XI,XF,HXTR,WXTR,Q,KSE,KST,KSA,
     *          TXTR,DXTR,KSR,AA,AA,AA,AA,DELTO,IVARIA,CONST,BID1,BID2,
     *          DELTA,DYA,TYA,WYA,HYA,EPSA,HXR,DXR,WXR,TXR,EPSR,ENTR,
     *          CHAMAX,dCHAMAX,NS,EPAIS,HIGHT,Heff						!février 2004	!!!aout04
	IF(ITYPE(NEL).EQ.5)THEN												!février 2004	!r&d13
	READ(306) AIRE,AIY,AIX,Sredy,Sredx,TORK								!février 2004	!extension neto
	ENDIF																!février 2004

      A1=0.0
      B1=0.0
      C1=0.0
      D1=0.0
      A2=0.0
      B2=0.0
      C2=0.0
      D2=0.0

C*********************************************************************************
C 1.  Début de la boucle sur le nbre de termes de la série de Fourier "DO 9 ..."
C*********************************************************************************
C 1.1 LECTURE DES RESULTATS INTERMEDIAIRES (pour le panneau NEL considéré) 
C     --------------------------------------------------------------------- 
      DO 9 NZ=1,JLMAX

      IF(MOD(NZ,2).EQ.1) THEN  
	 ITERM=1						 ! ITERM = 1 si IJK est impair
	ELSE       
       ITERM=2						 ! ITERM = 2 si IJK est pair
	ENDIF
      IF(IFONCT(ITERM).EQ.0) GOTO 9	 !Il s'agit d'un terme pair + charge symétrique

c      WRITE(*,*)'          TERME (term) ',NZ

      READ(NEL+100)DISA,DISB,DISC,DISD,ARGQ,M,HYP,LAMB,IJK

      IF((KSE.EQ.0).AND.(ICHA.EQ.0).AND.(IMOM.EQ.0)) THEN
        DO 821 I=1,1710
 821    DISH(I,IS)=0.
      ELSE
        READ(NEL+100)DISH
      ENDIF

C     LECTURE des FORCES DE BORD  (ZSN solution de MDR)
C     -------------------------------------------------
      DO J=1,NSOL
         READ(700+NEL) (ZSN(I,J),I=1,8)
      END DO

      DO I=1,8
         EQU(I)=ZSN(I,IS) ! IS est le cas de charge étudié
      END DO

c ************************************
      IND=0 ! cad sans effets de bord (JLBORD)
c      IF(IND.EQ.0) GOTO 93
c
c      DO 90 IPAN=1,NETO
c      DO 90 KK=1,8
c        IJJ=8*(IPAN-1)+KK
c        IF(IPAN.NE.NEL) GOTO 92
c        JNEL=NEL-1
c        JJNEL=NETO-NEL
c        IF(JNEL.EQ.0)GOTO 110
c            DO 111 J=1,JNEL
c  111       READ(KK+10)AA
c  110   CONTINUE
c        READ(KK+10)DISL
c        IF(JJNEL.EQ.0)GOTO 112
c            DO 113 J=1,JJNEL
c  113       READ(KK+10)AA
c  112   CONTINUE
c        DO 91 J=1,1710                                      
c   91   DISH(1,J)=DISH(1,J)-DISL(J)*ABC(IJJ)                 
c
c   92   DO 2108 I=1,8
c         IF(I==1) THEN
c	   WRITE(*,*) 'Erreur dans BO2, I1 pas défini -> STOP'
c	!  Il y a une erreur I1 n'est pas défini : a Corriger
c	   STOP
c	   ENDIF
c         EQU(I)=EQU(I)-SOLT(I+I1,IJJ)*ABC(IJJ)  !   I1 n'est pas défini ?????
c 2108   CONTINUE
c  90  CONTINUE
c
c  93  CONTINUE
c ************************************


C  NB:A utiliser pour obtenir les resultats relatifs à DISC seul (plaque infinie).                                                 
c	DO 567 I=1,8
c  567 EQU(I)=0.0
c	 EQU(1)=1.0
c      DO 568 I=1,1710
c  568 DISH(1,I)=0.

C 1.2 Impression des Inc. Hyperstatiques agissant sur les bords (Y=0 et Y=Yo)                                                    
C     ------------------------------------------------------------------------
      NBRE=3
      IF(IMPR.EQ.-3) GOTO 215
      NBRE=22
      IF((IMPR.EQ.-1).OR.(IMPR.EQ.-2)) NBRE=10
      IF(IMPR.GE.-1) THEN				!15.10.05
        WRITE(66,74)NZ
        WRITE(66,72)(EQU(I),I=1,8)
	ENDIF				!15.10.05

  215 CONTINUE

C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C 1.3 ++++++++ Début de la boucle relative aux calculs des sensibilités ++++++++
c ++++++++++++   de  U,V,Wø,Ny,Nyx,My,Ry,W,Wøø,Wøøø,Uø,Vø,Vøø		++++++++
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

C     Calcul des  SENSIBILITES    c.à.d  SENS1(9,4,NTOT) non cumule et SENS4(cumule) 
C     Dérivées des (U,V,W,Uø,etc.) par rapport aux variables de conception. 

	IF(IOPTI.EQ.0) GOTO 194
      READ(NEL+400) SENSH,SENSA, SENSB, SENSC, SENSD,		!extension neto
     *                    SENSAA,SENSBB,SENSCC,SENSDD,
     *           DEFA,DEFB,DEFC,DEFD,DEFAA,DEFBB,DEFCC,DEFDD
	
C     LECTURE des derivees des FORCES DE BORD  (DZSN solution de MDR2)    
     	J1=8*(NEL-1)
	DO  ISS=1,NSOL
		DO  J=J1+1,J1+8
 			READ(700+NEL) (DZSN(J,IVAR,ISS),IVAR=1,NTOT) 	!DZSN(NE,9*NETO,NSOL)
		END DO
	END DO

C     Boucle 184/183:Boucle sur toutes les variables de conception (KVAR=1,NTOT);
C                  (Il y a NBRXI variable de conception par panneau)
C     Boucle 180 = Boucle sur les points de calcul et les fonctions

	KVAR=0
      DO 184 IPAN=1,NETO
        I2=8*(NEL-1)
        NBRXI=NVAR(IPAN)
        DO 183 KK=1,NBRXI
                KK1=KVAR+KK
                K=NXIT(KK,IPAN)
            DO 2109 I=1,8
 2109       DEQU(I)=DZSN(I+I2,KK1,IS)

            DO 180 I=1,IPT

            DO 180 JJ=1,9
	      J=ID(JJ)
 
c      IF(IS.EQ.1) THEN
c       WRITE(66,*)'J=',J,' I=',I,' K=',K,' IPAN=',IPAN  
c       WRITE(66,*)'SENS?=',SENSA(J,I,K),SENSB(J,I,K),SENSCC(J,I,K),
c     *             SENSDD(J,I,K) 
c       WRITE(66,*)'SENSH(J,I,K,IS)=',SENSH(J,I,K,IS)
c       WRITE(66,*)'DEF?=',DEFA(J,I),DEFB(J,I),DEFCC(J,I),DEFDD(J,I) 
c       WRITE(66,*)'DEQU=',DEQU
c       WRITE(66,*)'EQU =',EQU
c	ENDIF
 
C SENS1 est relatif au panneau étudié par BO2 cad le panneau NEL,
C      mais il donne les sensibilités pour les variables de conception
C      de tous les panneaux (IPAN)

      SENS1(JJ,I,K,IPAN) = DEQU(1)*DEFC(J,I)   + DEQU(2)*DEFCC(J,I)
     *                   + DEQU(3)*DEFD(J,I)   + DEQU(4)*DEFDD(J,I)
     *                   + DEQU(5)*DEFA(J,I)   + DEQU(6)*DEFAA(J,I)
     *                   + DEQU(7)*DEFB(J,I)   + DEQU(8)*DEFBB(J,I)
      IF(IPAN.EQ.NEL)THEN
      SENS1(JJ,I,K,IPAN)=SENS1(JJ,I,K,IPAN) + SENSH(J,I,K,IS)
     *                + EQU(1)*SENSC(J,I,K) + EQU(2)*SENSCC(J,I,K)
     *                + EQU(3)*SENSD(J,I,K) + EQU(4)*SENSDD(J,I,K)
     *                + EQU(5)*SENSA(J,I,K) + EQU(6)*SENSAA(J,I,K)
     *                + EQU(7)*SENSB(J,I,K) + EQU(8)*SENSBB(J,I,K)
      ENDIF    
	SENS4(JJ,I,K,IPAN) = SENS4(JJ,I,K,IPAN) + SENS1(JJ,I,K,IPAN) ! version cumulee de SENS1
	
  180       CONTINUE                                                		
  183   CONTINUE                                                		
        KVAR=KVAR+NBRXI
  184 CONTINUE                                                		
	
	IF ((NZ==JLMAX).and.(IPRINT.GE.1)) THEN
	 WRITE(66,*) ' SENSIBILITES Cumulées (déplacements) '
       WRITE(66,*) ' ************************************ '

       DO 181 IPAN=1,NETO
	  NBRXI=NVAR(IPAN)
        WRITE(66,*) 'Fct.       Pt.1          Pt.2          Pt.3',
     *              '          Pt.4          Pt.5'
       DO 181 KK=1,NBRXI
        K=NXIT(KK,IPAN)
        WRITE(66,*)' Variable de conception nø',K,' du panneau nø',IPAN
        WRITE(66,*)' --------------------------------------------------'
        DO 190 JJ=1,9
c          WRITE(66,182) NOM(JJ),(SENS1(JJ,I,K,IPAN),I=1,IPT)
           WRITE(66,182) NOM(JJ),(SENS4(JJ,I,K,IPAN),I=1,IPT)
c          WRITE(66,182) ' cum',(SENS4(JJ,I,K,IPAN),I=1,IPT)
  190   CONTINUE  
  181  CONTINUE
      ENDIF

  194 CONTINUE

C +++++ Fin de la boucle relative aux calculs des sensibilités de U,V,W, ... +++
C ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

C 1.4 CUMMUL DES EFFETS DE LA CHARGE EXTERIEURE ET DES CHARGES DE BORDS POUR 
C     CHAQUE PANNEAU                                                     
C     ---------------------------------------------------------------------
	DO 699 I=1,22												!03.03.04
  699	NF(I) =0													!03.03.04
      DO 700 I=1,3                                                      
  700 NF(I)=I                                                           
      IF(IMPR.NE.-3) THEN
        J=3                                                             !BBO01110
        DO 701 I=24,45                                                    
          IF((IMPR.EQ.-1).OR.(IMPR.EQ.-2)) THEN     
			IF (ITYPE(NEL).EQ.5) THEN							!03.03.04  !r&d13
			IF((I.EQ.26).or.(I.EQ.27)) GOTO 701					!03.03.04
			IF((I.GE.29).AND.(I.LE.31)) GOTO 701				!03.03.04
			IF((I.GE.33).AND.(I.LE.39)) GOTO 701				!03.03.04
			IF((I.EQ.41).or.(I.GE.43)) GOTO 701					!03.03.04
			ELSE												!03.03.04
             IF(I.GE.39) GOTO 701                                       !Nx,Mx,etc.
             IF((I.GE.24).AND.(I.LE.28)) GOTO 701                       !My,Ny,etc.
			ENDIF												!03.03.04
	    ENDIF
        IF((I.EQ.29).OR.(I.EQ.36).OR.(I.EQ.37)) GOTO 701                !Wø,VøøøetUøøø
        J=J+1                                                             
        NF(J)=I
 701  CONTINUE
      ENDIF
C
      DO 80 K=1,M
      DO 80 I=1,NBRE
        NFONCT=NF(I)
        GOTO(2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
     *       2,2,2,2,1,2,2,2,1,1,1,2,2,1,2,1,2,2,1,1),NFONCT
    1   SY=1.
        GOTO 813
    2   SY=-1.
  813   CALL PCONTI(DISA,DISB,DISC,DISD,EQU,K,NFONCT,DISH(1,IS),SY)
   80 CONTINUE

      IF(MT.EQ.0) GOTO 205
      IF(IS.GT.1)THEN
        IS1=140+IS*20
      ELSE
        IS1=0
      ENDIF

      DO 81 I=1,MT
        FP(I)=HYP(I+IS1)
        JM=I+MT
        FP(JM)=HYP(JM+IS1)
        IF(IND.EQ.0) GOTO 84  ! cad sans effets de bord Nb et Mb (JLBORD)
c         DO KK=1,8
c           IJJ=8*(NEL-1)+KK
c           II=160+KK*20
c           FP(I)=FP(I)-HYP(I+II)*ABC(IJJ)
c           IJ=I+MT
c           FP(IJ)=FP(IJ)-HYP(IJ+II)*ABC(IJJ)
c	    ENDDO
  84    CONTINUE
        DO 81 J=1,8
          IJ=I+20*J
          FP(I)=FP(I)+HYP(IJ)*EQU(J)
          IJ=IJ+MT
          FP(JM)=FP(JM)+HYP(IJ)*EQU(J)
   81 CONTINUE

      DO 83 I=1,2*MT
        FP(I)=FP(I)/10000.
  83  CONTINUE
C
C
      DO 114 I=1,MT
        U(2*I+30)=ABTR(I)
        U(2*I+31)=ABTR(I)
  114 CONTINUE
  205 SM=PHIL/30.
      DO 109 I=1,31
  109 U(I)=FLOAT(I-1)*SM
C
      DO 902 II=1,2295
  902 Z2(II)=0.

c      WRITE(66,*)  'DISH'
c      CALL PIMPR(DISH(1,IS),0,2)
C
C 1.5 CALCUL DES RESULTATS EN 31 POINTS SELON LA HAUTEUR ET EN 5 POINTS 
C     SELON LA LARGEUR DU PANNEAU AINSI QU'AU DROIT DESTRAVERSES.       
C     ------------------------------------------------------------------
      DO 901 I=1,NBRE
      NFONCT=NF(I)
      GOTO(17,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,
     *18,18,18,18,18,17,17,17,17,18,17,17,17,18,18,18,17,17,18,17,18,17,
     *17,18,18),NFONCT
   17 SY=-1.
      GOTO4
   18 SY=1.
    4 CALL COMPU(PHIL,FP,DISH(1,IS),DISC,DISB,ABTR,M,ARGQ,MT,U,TETA,Z2,
     *           NFONCT,SY,31)
  901 CONTINUE

C     Ne pas effacer !!!! (parfois utile)
c      WRITE(66,*)' Fct Z2'                     		
c      WRITE(66,75)'V ' ,(Z2(IH),IH=1,1+30)                             !V
c      WRITE(66,75)'Vø' ,(Z2(IH),IH=1480,1480+30)                       !Vø
c      WRITE(66,75)'Vøø',(Z2(IH),IH=1633,1633+30)                       !Vøø
c      WRITE(66,75)'U ' ,(Z2(IH),IH=52,52+30)                           !U
c      WRITE(66,75)'Uø' ,(Z2(IH),IH=1531,1531+30)                       !Uø
c      WRITE(66,75)'Uøø',(Z2(IH),IH=1684,1684+30)                       !Uøø
c      WRITE(66,75)'W '  ,(Z2(IH),IH=103,103+30)                        !W
c      WRITE(66,75)'Wø'  ,(Z2(IH),IH=1429,1429+30)                      !Wø
c      WRITE(66,75)'Wø'  ,(Z2(IH),IH=1582,1582+30)                      !Wø
c      WRITE(66,75)'Wøø' ,(Z2(IH),IH=1735,1735+30)                      !Wøø
c      WRITE(66,75)'Wøøø',(Z2(IH),IH=1888,1888+30)                      !Wøøø
c   75 FORMAT(A5/6(6E14.7,/))          						

C 1.6 CALCULS des d(W)/dx  c.à.d des rotations selon X
C     -----------------------------------------------------
c      IF(NZ.NE.1) GOTO 9902
c      DO 9901 I=1,11
c      DO 9901 J=1,5
c 9901 DW(I,J)=0.
c 9902 WRITE(66,782)
c      DO 9903 I=1,11
c      TEMP=Z2(103+3*(I-1))*LAMB
c      DO 9900 J=1,5
c 9900 DW(I,J)=DW(I,J)+TEMP*DCOS(LAMB*DIS(J))
c 9903 WRITE(66,641)I,(DW(I,J),J=1,5)

C 1.7 CALCULS DES RESULTATS (déplacements et contraintes)
C     -----------------------------------------------------
c     IF(IMPR.NE.-3) GOTO 3000			!15.10.05
c      CALL SORTI(LAMB,DIS,MT,EFF,Z2)	!15.10.05
c      GOTO 9						!15.10.05
c 3000 CONTINUE					!15.10.05

	IF(ITYPE(NEL).NE.5) THEN										!février 2004  !r&d13			
      CALL RESUL(WYA,TYA,DYA,HYA,KSA,WXR,TXR,DXR,HXR,KSR,
     *   DELTA,ETA,LAMB,E,NZ,HXTR,TXTR,WXTR,DXTR,MT,KST,
     *   SIGY,EPSA,EPSR,ENTR,ITYPE(NEL),								!r&d13
     *   CONST,Q,DIS,FAM,JLMAX,INDAIG,INDRAI,IMPR,NETO,NEL,IPRINT,
     *   EFF,EFF2,EFF2(2041),EFF2(3571),EFF2(5101),CONC,Z2,
     *   SXM,SXM(IPTmax+1),SXM(IPTmax+NETO+1),
     *   SENS1,SENS2,SENS3,
     *   NVAR,NXIT,IPTS,IPTS2,IPTS3)
	ELSE						! nouvelle sousroutine				!février 2004
      CALL RESUL2(WYA,TYA,DYA,HYA,KSA,WXR,TXR,DXR,HXR,KSR,			!février 2004
     *   AIRE,AIy,LAMB,E,NZ,HXTR,TXTR,WXTR,DXTR,MT,KST,				!février 2004
     *   SIGY,EPSA,EPSR,ENTR,TORK,ISECT,EPAIS,						!février 2004
     *   CONST,Q,DIS,FAM,JLMAX,INDAIG,INDRAI,IMPR,NETO,NEL,IPRINT,	!février 2004
     *   EFF,EFF2,EFF2(2041),EFF2(3571),EFF2(5101),CONC,Z2,			!février 2004
     *   SXM,SXM(IPTmax+1),SXM(IPTmax+NETO+1),						!février 2004
     *   SENS1,SENS2,SENS3,SENS5,										!février 2004
     *   NVAR,NXIT,IPTS,IPTS2,IPTS3)									!février 2004
	ENDIF															

C     FLEXION LOCALE DES RAIDISSEURS
c     --------------------------------

      IF(NZ.EQ.1) THEN ! premier terme de la serie de fourier
        IF(IOPTI.GE.1) THEN
C         Recherche des restrictions choisies : IQ=1 oui et IQ=0 non
	    IQ50=0
	    DO I=1,M1TABL(NEL)            !  IM=Compteur des restr. struct.
            IF((LCONT(1,IM+I).EQ.12).OR.    ! restr nø 12 : Bord L/2 + SIG STIFF                 !avril2003
     *         (LCONT(1,IM+I).EQ.13).OR.    ! restr nø 13 : Bord L/2 + SIG STIFF + SIG PLTB      !avril2003
     *         (LCONT(1,IM+I).EQ.33).OR.    ! restr nø 33 : TauJAB x=0 + TAU STIFF               !avril2003
     *         (LCONT(1,IM+I).EQ.34).OR.    ! restr nø 34 : SigSEM x=L/2 + SIG STIFF             !avril2003
     *         (LCONT(1,IM+I).EQ.35).OR.    ! restr nø 35 : SigJAB x=L/2 + SIG STIFF + TAU STIFF !avril2003
     *         (LCONT(1,IM+I).EQ.36).OR.    ! restr nø 36 : SigJAS x=L/2 + SIG STIFF             !avril2003
     *         (LCONT(1,IM+I).EQ.51).OR.    ! restr nø 51 : Flèche 1/384
     *         (LCONT(1,IM+I).EQ.52).OR.    ! restr nø 52 : Flèche 5/384
     *         (LCONT(1,IM+I).EQ.54).OR.    ! restr nø 54 : Sigma semelle
     *         (LCONT(1,IM+I).EQ.55).OR.    ! restr nø 55 : Sigma bordé
     *         (LCONT(1,IM+I).EQ.56).OR.    ! restr nø 56 : 1.73*Tau
     *         (LCONT(1,IM+I).EQ.57).OR.    ! restr nø 57 : Flèche Maille 1/384
     *         (LCONT(1,IM+I).EQ.58)) THEN  ! restr nø 58 : Flèche Maille 5/384
		    IQ50=1  
		    GOTO 46
		  ENDIF  
          ENDDO
  46      CONTINUE
	  ENDIF

 	IF (ITYPE(NEL).NE.5) THEN										!12.05.04  !r&d13

      CALL STIFF(IPRINT,NETO,NEL,E,ETA,SIGY,WIDTH,Q,PHIL,
     *           PLOC(NEL),XI,XF,CHAMAX,dCHAMAX,KSE,NSOL,IS,			!!!aout04
     *           DELTA,EPSA,HXR,DXR,WXR,TXR,EPSR,ENTR,KSR,
     *           NVAR,NXIT,
     *           Plat(NEL),Fl(NEL),
     *		   SIG1(NEL),SIG2(NEL),SIG3(NEL),SIG4(NEL),TAU(NEL),Flr,      !avril2003
     *           Wpl1(NEL),Wpl2(NEL),				      
     *           VS(1),VS(10),VS(19),VS(28),VS(37),VS(46),VS(55),VS(64),    !avril2003
     *           IQ50,IOPTI,NTOT,IMPR) !15.10.05	! dSIG1,dSIG2,dSIG3,DSIG4,dTau,dFlr,dWpl1,dWpl2,IQ50 !avril2003	
                                                                ! = vecteurs de travail
      ENDIF														!12.05.04
	ENDIF

C
C 1.8 VERIFICATION DE L'EQUILIBRE DU PANNEAU (si IMPR = 0 ou 1)
C     --------------------------------------
      IF(IMPR.LE.-1) GOTO 9
      IF((NZ.GT.1).AND.(IMPR==0)) GOTO 9

      CALL EQUIL(MT,Z2,LAMB,CONST,XI(IS),XF(IS),IVARIA,PHIL,IJK,
     *     KSE,Q,NEL,ABTR,IND,A1,B1,C1,D1,A2,B2,C2,D2,DELTO,WIDTH,
     *     TETAQ,NETO)

   9  CONTINUE ! Boucle sur NZ (nbre de termes)
   !!! Fin de la boucle sur le nbre de termes de la série de Fourier


C***********************************************************************
C***********************************************************************

C 2.0 PRISE EN COMPTE DE LA FLEXION LOCALE DES RAIDISSEURS              !avril2003
C     ----------------------------------------------------              !avril2003
C     Combinaison des résultats donnés par RESUL et par STIFF           !avril2003

	CALL COMBINE(EFF,SIG1(NEL),SIG2(NEL),SIG3(NEL),                   !avril2003
     *             SIG4(NEL),TAU(NEL),                                  !avril2003
     *             MT,EFFCOMB,NETO,NVAR,NXIT,NEL,IPTS,IPTS2,TXR)        !avril2003


C 2.1 CALCUL DU SIGMA PLAQUE ET DE SA DERIVEE                           !avril2003
C     ---------------------------------------                           !avril2003

	IF(ITYPE(NEL).NE.5) THEN										  !février 2004	 !r&d13			
	
      III=(NEL-1)*IPTmax+1 !fev04
      CALL PLAQUE(EPSA,EPSR,ENTR,DELTA,SENS3,VS(1),NEL,
     *HYA,HXR,WIDTH,ABTR,PHIL,Q,IPTS2,EFFCOMB,
     *CHAMAX(1,IS),dCHAMAX(1,IS,1),PLOC(NEL),NETO,E,ETA,NVAR(NEL),			!!!aout04
     *SIG1(NEL),NXIT,EFF,IPT,SigPlaque,										!!!aout04
     *DSigPlaque,SIGXPlaque(III),SIGYPlaque(III),PHIPlaque(III),         !fev04
     *SIGVMTPlaque(III),SIGVMCPlaque(III),INDPlaque(III),SIGMX(III),     !fev04
     *SIGMY(III))                                                        !fev04
                                                   
	endif													!février 2004
C***********************************************************************
C***********************************************************************
C 3.  CALCUL DE L'EPAISSEUR MINIMALE (cfr Progr. de HUGHES, Edt. SNAME)
C     ------------------------------------------------------------------
C	TPL  = Epaisseur minimale de HUGHES
C	DTPL = DELTA- TPL (HUGHES) = valeur de la restriction C(J) = DELTA-TPL<0
C	TPLA = d(TPL)/d(XI=EPSA)   = dérivées de C(J) par rapport à EPSA
C	TPLR = d(TPL)/d(XI=EPSR)   = dérivées de C(J) par rapport à EPSR
 
c Minimum plate thickness assessment : Yielding and buckling
c--------------------------------------------------------------
c      SR1 = Ratio applied stress/yield Stress (?<? 1.0)
c      SR2 = Global Interaction Ratio: Applied Stress/Critical Stress (?<? 1.0)
c
cPanel <-- Panel Dimensions -->  Minimum Thickness <- Collapse Scenario & applied stress --->  YIELDING   BUCKLING  Critical Stresses
c      L(long) B(trans) d(thick)  d(min)  d/d(min)   Mode  Section   Sx    Sy     Tau    Plat  (S/Sy<1)  (S/Scr<1) Sx(cr) Sy(cr) T(cr)
c         (m)     (m)    (mm)      (mm)   (> 1 ??)           Y/Yo   N/mm2  N/mm2  N/mm2  (m)   Von-Mises Interact  N/mm2  N/mm2  N/mm2
c                                                                                                SR1     Form(SR2)
c  1    0.371   0.873     8.00     7.82    1.02    Yielding  1.00    47.1   51.7    0.0   0.00  1.000      0.000   337.9  117.8  513.0
c1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890

      IF(IOPTI.GE.1) THEN		!r&d13
	  
	  IF(NEL.EQ.1) THEN
          IF(LANGUE==1 ) THEN ! French
            WRITE(67,'(A/62(1H-))')
     *      ' Epaisseur minimale de bordé: Plastification et voilement'
	    ELSE                ! English
            WRITE(67,'(A/62(1H-))') 
     *     ' Minimum plate thickness assessment : Yielding and buckling'
	    ENDIF
          IF(ITERA.GE.1) WRITE(666,562)		!13.12.05
          WRITE( 67,562)
	  ENDIF

 562  FORMAT(5x,' SR1 = Ratio of Applied stress/Yield ',
     *   'Stress (?<? 1.0)'/5x,' SR2 = Global Interaction Ratio:',
     *   ' Applied Stress/Critical Stress (?<? 1.0)'//
     *  5x,' For Yielding (Global stresses(Sx,Sy) are combined with ',
     *                     'the stress induced by the stiff bending)'/
     *  5x,' For Buckling (Global stresses(Sx,Sy) are considered alone)'
     *   //'Panel <-- Panel Dimensions -->  Minimum ',
     *   'Thickness <- Collapse Scenario & applied stress ---> ',
     *   ' YIELDING   BUCKLING  Elast.Crit.Stress'/
     *   '      L(long) B(trans) d(thick)  d(min)',
     *   '  d/d(min)   Mode  Section   Sx    Sy     Tau    Plat  ',
     *   '(S/So <1) (S/Scr<1) Sx(cr) Sy(cr) T(cr)'/
     *   '         (m)     (m)    (mm)      (mm) ',
     *   '  (> 1 ??)           Y/Yo   N/mm2  N/mm2  N/mm2  (m)   ',
     *   'Von-Mises Interact  N/mm2  N/mm2  N/mm2'/
     *    T97,'SR1     Form(SR2)'/)
  
      
C     Vérification si la restriction "dmin-d<0" est choisie : IQ=1 oui et IQ=0 non
	  IQ=0
	  IF(ITYPE(NEL).NE.5) THEN								!12.05.04  !r&d13
	    DO  I=1,M1TABL(NEL)
            IF(LCONT(1,IM+I).EQ.4) THEN    ! dmin-d<0 =  restrict nø4
	        IQ=1  ! restriction 4 selectionnée
	        GOTO 45
	      ENDIF
          ENDDO
  	  ENDIF													!février 2004		
45      CONTINUE
	  IF(ITYPE(NEL).NE.5) THEN									!12.05.04  !r&d13
          IF(PHIL.GE.1.) THEN  ! Vérification si paneau est une PLAQUE (et pas une COQUE
	      WRITE(666,197)   ! pas de calcul car COQUE
		  write(29, 197)	 !bug														!bug
	      WRITE(9  ,197)
c		  PAUSE'OK?'
          ELSE
            IF((IOPTI.GE.1).AND.(IQ.EQ.1)) THEN
              DTPL = -DELTA                                         ! dmin-d
              TPLA = 0.                                             ! d(dmin-d)/d(EPSA)
              TPLR = 0.                                             ! d(dmin-d)/d(EPSR)
              TPLD = -1.                                            ! d(dmin-d)/d(Delta)

c         Parmi DELTA, EPSA et EPSR, il y a-t-il bien une variable de conception sélectionnée?
c         Si NON : STOP  impossible d'utiliser cette restriction.
		    KXI=NXI2(1,NEL)+NXI2(5,NEL)+NXI2(9,NEL)
              IF(KXI.EQ.0) THEN
	          WRITE(*,*)  
	          WRITE(*,*)' STOP - ERROR/ERREUR '
	          WRITE(*,*)' --------------------'
	          WRITE(*,*)' PANNEAU     - PANEL      :',NEL
	          WRITE(*,*)' RESTRICTION - CONSTRAINT : n°4 Hughes'
	          WRITE(*,*) ' see explanation in the output file OPT-*.*'
	          WRITE(666,*)  
	          WRITE(666,*)' STOP - ERROR/ERREUR '
	          WRITE(666,*)' --------------------'
	          WRITE(666,*)' PANNEAU     - PANEL      :',NEL
	          WRITE(666,*)' RESTRICTION - CONSTRAINT : n°4 Hughes'
	          WRITE(666,*)
	          WRITE(29,*)														!bug		
	          WRITE(29,*)' ERREUR : subr BO2'									!bug
	          WRITE(29,*)' PANNEAU :',NEL										!bug
	          WRITE(29,*)' RESTRICTION - CONSTRAINT : n°4 Hughes'				!bug
	          WRITE(29,*)' Vous avez selectionné la restriction n°4 ',			!bug
     *          '(dite de Hughes) et aucune des variables de conception'			!bug
	          WRITE(29,*)' suivantes n''a été sélectionnée pour le ',			!bug
     *          'panneau ',NEL													!bug
	          WRITE(29,*)' - 1: épaisseur du panneau            ',				!bug
     *		  '(DELTA)'
	          WRITE(29,*)' - 5: entredistance entre raidisseurs ',				!bug
     *		  '(EPSR)'
	          WRITE(29,*)' - 9: entredistance entre cadres      ',				!bug
     *		  '(EPSA)'
	          WRITE(29,*)' IL FAUT OBLIGATOIREMENT UNE ',						!bug
     *          '(ou plusieurs) DE CES 3 VARIABLES DE CONCEPTION POUR ',			!bug
     *          'QUE LA RESTRICTION n°4 SOIT ACTIVE.'								!bug
		      WRITE(29,*)' LBR-5 est arreté. Veuillez corriger vos',			!bug
     *                  ' données'												!bug
                IF(LANGUE.EQ.1) THEN
	            WRITE(666,*)' Vous avez selectionné la restriction ',
     *            'n°4 (dite de Hughes) et aucune des variables de ',
     *			'conception'
			    WRITE(666,*)' suivantes n''a été sélectionnée pour ',
     *            'le panneau ',NEL
	            WRITE(666,*)' - 1: épaisseur du panneau            ',
     *		    '(DELTA)'
	            WRITE(666,*)' - 5: entredistance entre raidisseurs ',
     *		    '(EPSR)'
	            WRITE(666,*)' - 9: entredistance entre cadres      ',
     *		    '(EPSA)'
	            WRITE(666,*)' IL FAUT OBLIGATOIREMENT UNE ',
     *            '(ou plusieurs) DE CES 3 VARIABLES DE CONCEPTION ',
     *            'POUR QUE LA RESTRICTION n°4 SOIT ACTIVE.'
		        WRITE(*,*) ' LBR-5 est arreté. Veuillez corriger vos',
     *                  ' données'
		        WRITE(666,*)' LBR-5 est arreté. Veuillez corriger ',
     *                  'vos données'
                ELSE
	            WRITE(666,*)' Constraint n°4 ("Hughes") is selected ',
     *           'and none of the following design variables were ',
     *		   'selected'
	            WRITE(666,*)' for the panel n°',NEL
	            WRITE(666,*)' - 1: panel thickness      (DELTA)'
	            WRITE(666,*)' - 5: longitudinal spacing (EPSR)'
	            WRITE(666,*)' - 9: frame spacing        (EPSA)'
	            WRITE(666,*)' AT LEAST ONE OF THESE 3 DESIGN VAR',
     *            'IABLES MUST BE SELECTED TO ACTIVATE CONSTRAINT n°4'
		        WRITE(666,*)' LBR-5 is stopped. Please correct ',
     *			'your data'
		        WRITE(*,*)  ' LBR-5 is stopped. Please correct ',
     *			'your data'
                ENDIF
                PAUSE 'STOP'
	          STOP
              ENDIF  ! IF(KXI.NE.0)

	        ISTOP=0  ! ré-initialisation
		   	
              CALL HUGHES (IPRINT,CHAMAX(1,IS),PLOC(NEL),E,ETA,SIGY,
     *        SIGM,WIDTH,PHIL,Q,MT,ABTR,HYA,EPSA,HXR,EPSR,ENTR,DELTA,
     *        EFF,TPL,TPLA,TPLR,TPLD,
     *        NXI2(1,NEL),NXI2(5,NEL),NXI2(9,NEL),ISTOP,NEL,IQ,ITERA,IS,
     *        EFFCOMB,SIGMAG(NEL))											!avril2003

   
              IF(ISTOP.EQ.0) THEN  ! OK convergence dans HUGHES
                DTPL=TPL-DELTA                              ! dmin-d
              ELSE                 ! pas de convergence dans HUGHES                                        !
                TPL=1.2*DELTA                               ! posons dmin=1.2*delta
                DTPL=TPL-DELTA                              ! dmin-d
                TPLA = 0.001                                ! d(dmin-d)/d(EPSA)
                TPLR = 0.01                                 ! d(dmin-d)/d(EPSR)
                TPLD = -1.3                                 ! d(dmin-d)/d(Delta)
                IF(IPRINT.EQ.1) THEN
			    WRITE(*,  198)NEL,TPL,TPLA,TPLR,TPLD
		        WRITE(666,198)NEL,TPL,TPLA,TPLR,TPLD
	          ENDIF
		    ENDIF     
c		    WRITE(666,198)NEL,TPL,TPLA,TPLR,TPLD  !mode debug

	      ENDIF    ! IF((IOPTI.GE.1).etc.
 
          ENDIF  ! test plaque-coque
	
	  ELSE													!12.05.04
		IF(IOPTI.GE.1) THEN						!26.07.05
		WRITE(666,*) 'PANNEAU N0 =',NEL
		WRITE(666,*)'--------------------'
		WRITE(666,*) NEL,' est une épontille'					!12.05.04
		ENDIF									!26.07.05					
	
	  ENDIF													!12.05.04

	ENDIF

c     Après Hughes, EFF devient EFFCOMB pour le calcul d'optimisation !avril2003
c      DO I=1,9690    !avril2003
c	  EFF=EFFCOMB  !avril2003  (NON garder les 2 EFF et EFFCOMB, Rigo 27-5-03)
c	ENDDO          !avril2003


C***********************************************************************
C***********************************************************************
C 4.  CALCUL DES CONTRAINTES CRITIQUES DE FLAMBEMENT (cfr IACS - S11)		!r&d13
C     ---------------------------------------------------------------
 2500	IF(IOPTI.GE.1) THEN							!r&d14
	  
        IF((NEL.EQ.1).AND.(ITERA.GE.1)) WRITE(666,563)		

  563   FORMAT('Panel        Elast. Buck. Stress          '
     *   'Crit. Buck. Stress'/
     *   '        Column     Torsion     Web'/
     *   '                     Se                      Sc       Sa/Sc'/
     *   '                   (N/mm2)                 (N/mm2)    (<1?)'/)

	  IBUCK=0
	  IF(ITYPE(NEL).NE.5) THEN											!r&d13									
	    DO I=1,M1TABL(NEL)
            IF(LCONT(1,IM+I).EQ.14) IBUCK=1					! flambt tôle = restrict nø14
	      IF(LCONT(1,IM+I).EQ.37) IBUCK=2					! flambt raidisseur = restrict nø37
          ENDDO
  	  ENDIF																		
	  
	  IF(IBUCK.NE.0) THEN
	    CALL BUCKDCN(NEL,E,SIGY,SIGM,DELTA,HXR,DXR,WXR,TXR,EPSR,
     *				 EPSA,WYA,EFF,SENS2,SENS3,NVAR,NXIT,NETO,
     *				 DSIGPL,dDSIGPL,DSIGSTIF,dDSIGSTIF,WORK,
     *				 ITERA,Z,PHILN,TETAS,ITYPE,PART,DELT,
     *				 YNEUTPART,SYM,SYMX,BM1,IBUCK)
	  ENDIF

	ENDIF


C***********************************************************************
C***********************************************************************
C 4.  CALCUL DES CONTRAINTES NORMALES EN GROSS SCANTLING (cfr IACS)		!r&d13
C     -------------------------------------------------------------
c	IF(IOPTI.GE.1) THEN

c	  IF(ITYPE(NEL).NE.5) THEN											!r&d13									
c	    DO I=1,M1TABL(NEL)
c            IF(LCONT(1,IM+I).EQ.10) THEN
c		    CALL YIELDIACS(NEL,NVAR,NXIT,NETO,SIGX,dSIGX,WORK,
c     *				       Z,PHILN,TETAS,ITYPE,PART,DELT,YNEUTPART,
c     *				       SYM,SYMX,BM1,CORRO,EFF,SENS3)				! plast tôle = restrict nø10
c	        GOTO 47
c	      ENDIF
c          ENDDO
c  	  ENDIF																		
c   47   CONTINUE
	  
c	ENDIF


C***********************************************************************
C***********************************************************************
C 5. IMPRESSION DES RESULTATS 
C    --------------------------
C 5.1 Dans fichier SOL2 - Résultats de STIFF (flexion locale raidisseurs)
c
c12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
c       Laterale   <-Bending Stress (for M=PL^2/10)->  Stiff. Deflection   <-- Plate Deflections ->
cPanel  Pressure    Sx(Plate)  Sx(Flange)   TAU(web)   (Simply supported)  (Clamped)  (Simply Supp)  
c         (m)        (N/mm2)    (N/mm2)     (N/mm2)         (mm)               (mm)       (mm)
c  1    -25.22      -2250.20    -2250.20    -2250.20       -120.23            -120.23    -120.23 

	IF(IMPR.GE.-1) THEN				!15.10.05
      IF(NEL.EQ.NETO) THEN
        IF(LANGUE==1 ) THEN ! French
         WRITE(67,'(/A/60(1H-))')
     *              ' Flexion Locale Raid. & Déformation de la Maille'
	  ELSE                ! English
           WRITE(67,'(/A/60(1H-))') 
     *              ' Local Stiffener Bending & Plate Deflection'
	  ENDIF

	  WRITE(67,'(2A,10x,A)')  '       Laterale   ',						
     *   '<-  Stiffener Bending Stress (for M=PL^2/10)',					
     *   '->  Stiff. Deflection   <-- Plate Deflections ->'				
	  WRITE(67,'(2A)')'Panel  Pressure     Sx(Plate)  Sx(Flange)   ',	
     *   'Sx(JAB)    Sx(JAS)    TAU(web)  (Simply supported)  (Clamped)   
     *  (Simply Supp)  Sigma(PLTBEN)'                                     !avril2003
	  WRITE(67,'(2A)')'         (m)       (N/mm2)     (N/mm2)     ',    
     *           '(N/mm2)     (N/mm2)     (N/mm2)        (mm)             !avril2003
     * (mm)           (mm)        (N/mm²)'                                !avril2003
        DO I=1,NETO
          WRITE(67,'(I3,3x,6(F8.2,4x),3x,F6.2,9x,3(F6.2,9x))')            !avril2003
     *      I,Plat(I)/9810.,SIG1(I)/1.E06,SIG2(I)/1.E06,SIG3(I)/1.E06,    !avril2003
     *      SIG4(I)/1.E06,TAU(I)/1.E06,1000.*Fl(I),                       !avril2003
     *      1000.*Wpl1(I),1000.*Wpl2(I),SIGMAG(I)/1.E06                   !avril2003
	  ENDDO
      ENDIF

	ENDIF				!15.10.05
C 5.2 Dans fichier SOL - Résultats de RESULT (déplacements et contraintes)
      IF(ITYPE(NEL).EQ.5) THEN											!février 2004  !r&d13
	 
      IF(IMPR.EQ.-3) THEN										!03.03.04
         CALL ECRI2(DIS,NEL,EFF,CONC,NE,INDAIG,INDRAI,NS,Q)	!03.03.04
      ELSE													!03.03.04
		CALL ECRI4(DIS,NEL,EFF,CONC,NE,INDAIG,INDRAI,Q,NS,IMPR,	    !février 2004
     *				ISECT,VNyMAX,VMyMAX,SyMAX,SyMIN,VM1,VM2)		!février 2004
	ENDIF													!03.03.04
	ELSE															!février 2004

      IF(IMPR.EQ.-3) THEN

        CALL ECRI2(DIS,NEL,EFF,CONC,NE,INDAIG,INDRAI,NS,Q)

      ELSE

        CALL ECRI(DIS,NEL,EFF,EFFCOMB,CONC,NE,INDAIG,INDRAI,NS,Q,IMPR,   !avril2003
     *            VMAXSIG(NEL),VMAXSIGC(NEL))                            !avril2003

        IF((NEL.EQ.NETO).AND.(IMPR.GE.-1)) THEN                 !15.10.05                            !avril2003
	    WRITE(67,*)                                                    !avril2003
	    WRITE(67,*)'Détail des contraintes utilisées dans la restricti
     *on RESUL+STIFF+PLTBEN'
          WRITE(67,*)'--------------------------------------------------
     *---------------------'                                   
	    WRITE(67,'(3A)') 
     *'Panel    Sx(RESUL+STIFF)    Sy(RESUL)      ',                     !fev04
     *'  Sx(Plaque avec PHI)  Sy(Plaque avec PHI)  ',                    !fev04
     *'PHI   SIGVonMises SIGVonMises  IND2'                              !fev04
	    WRITE(67,'(3A)') '            (N/mm²)           (N/mm²)  ',    !fev04
     *'         (N/mm²)              (N/mm²)',                           !fev04
     *'                  Traction   Compression'
	    DO I=1,NETO                                                    !fev04
		  III=(I-1)*4+1                                                !fev04
		  IF (INDPlaque(III).NE.0) THEN                                !fev04           
      WRITE(67,'(I3,8x,2(F8.2,9x),F8.2,14x,F8.2,11x,F5.2,1x,2(F8.2,5x),
     *I3)') I,                                                           !fev04
     *SIGMX(III)/1.E06,SIGMY(III)/1.E06,SIGXPlaque(III)/1.E06,           !fev04
     *SIGYPlaque(III)/1.E06,PHIPlaque(III),SIGVMTPlaque(III)/1.E06,      !fev04
     *SIGVMCPlaque(III)/1.E06,INDPlaque(III)                             !fev04
	      ENDIF                                                        !fev04

	      DO J=2,4                                                     !fev04
              III=(I-1)*4+J                                              !fev04
	        IF (INDPlaque(III).NE.0) THEN                              !fev04
      WRITE(67,'(11x,2(F8.2,9x),F8.2,14x,F8.2,11x,F5.2,1x,2(F8.2,5x),
     *I3)')                                                              !fev04
     *SIGMX(III)/1.E06,SIGMY(III)/1.E06,SIGXPlaque(III)/1.E06,           !fev04
     *SIGYPlaque(III)/1.E06,PHIPlaque(III),SIGVMTPlaque(III)/1.E06,      !fev04
     *SIGVMCPlaque(III)/1.E06,INDPlaque(III)                             !fev04
	        ENDIF                                                      !fev04

	      ENDDO
	    ENDDO
		WRITE(67,*)'Résultats de différentes contraintes maximales'
	    WRITE(67,*)'----------------------------------------------'
	    WRITE(67,'(A)') 
     *       'Panel    Sx max(RESUL)      Sx max(RESUL+STIFF)'
	      WRITE(67,'(A)')
     *      '            (N/mm²)              (N/mm²)'
		DO I=1,NETO
	      WRITE(67,'(I3,8x,2(F8.2,13x))') I,VMAXSIG(I)/1.E06,
     *                                     VMAXSIGC(I)/1.E06
		ENDDO
	   ENDIF
	   	  
	  IF (IMPR.GE.-1) THEN			!15.10.05
C       Impression des contraintes dans le bordage au niveau des faces du borde
C       z=+delta/2 et Z=-delta/2.
	  CALL ECRI3(DIS,NEL,NE,NS,Q,EFF2)
	ENDIF				!impr 0

      ENDIF				!impr-3 et else
      ENDIF				!épon ou plaque					! février 2004

C***********************************************************************
C***********************************************************************
C 6. Calcul des Restrictions C<Cmax (OPTI)
C     ------------------------------------
C      IF(ITYPE(NEL).NE.5)THEN			!pas de restrictions pour épontille pour le moment	!février 2004

 2501 IF(IOPTI.GE.1) THEN											  !r&d14

      CALL CONTR(NETO,NEL,IPRINT,IM,IMG,ITERA,IS,
     *           E,ETA,WIDTH,SIGY,CONST,EFFCOMB,EFF,        !avril2003
     *           EFF,EFF2,CONC,Z2,IMPR,					  !15.10.05
     *           DTPL,TPLD,TPLA,TPLR,
     *		   DSIGPL,dDSIGPL,DSIGSTIF,dDSIGSTIF,SIGX,dSIGX,	  !r&d13
     *           LAMB,Q,DELTA,PHIL,
     *           DYA,TYA,WYA,HYA,EPSA,KSA,HXR,DXR,WXR,TXR,EPSR,ENTR,KSR,
     *           SXM,SXM(IPTmax+2*NETO+1),SENS2,SENS3,SENS4,DC,
     *           NVAR,M1TABL,M2CONT,NXIT,IPTS,YPTS,IPTS2,IPTS3,
     *           LCONT,CJMAX,INV,NXI2,
     *           SIG1(NEL),SIG2(NEL),SIG3(NEL),SIG4(NEL),Tau(NEL),Flr,           !avril2003
     *           Wpl1(NEL),Wpl2(NEL), ! SIG1,SIG2,SIG3,SIG4,Tau,Flr,Wpl1,Wpl2
     *           VS(1),VS(10),VS(19),VS(28),VS(37),VS(46),VS(55),VS(64),         !avril2003
                                       ! dSIG1,dSIG2,dSIG3,dSIG4,dTau,dFlr,dWpl1,dWpl2
     *           SigPlaque,DSigPlaque)                                           !avril2003

c     IF(IPRINT.GE.1) PAUSE 'Pause'

	ENDIF
C	ENDIF																!février 2004

C*********************************************************************************
C23456789012345678901234567890123456789012345678901234567890123456789012
C*********************************************************************************
C 7.  Ultimate strength of Hull girder
C     ------------------------------------
      IF(IS.EQ.NSOL.AND.IULT.GT.0) THEN
	
      IF(IULT.EQ.1) THEN

C       Methode de PAIK/Caldwell : IULT=1
C       ---------------------------------
C       Calcul de la section transversale du panneau  (sur base de l'épaisseur moyenne)

	  SECT=DELT(NEL)*Q*PHIL*PI/180.

c	  WRITE(66,*)' Ultimate strength of Hull girder (Subr USHULL)'
c	  WRITE(66,*)' NEL =',NEL
c	  WRITE(66,*)' DELT(NEL)=',DELT(NEL)
c	  WRITE(66,*)' Q   =',Q
c	  WRITE(66,*)' PHIL=',PHIL
c	  WRITE(66,*)' SECT=',SECT

        J1=1                   !9*NETO
        J2=J1+9*NETO           !9*NETO
        J3=J2+9*NETO           !9*NETO
        J4=J3+9*NETO           !9*NETO
        J5=J4+9*NETO           !9*NETO
        J6=J5+9*NETO           !9*NETO
        J7=J6+9*NETO           !9*NETO
        J8=J7+9*NETO           !9*NETO
        J9=J8+9*NETO           !9*NETO
        J10=J9 +9*NETO         !9*NETO
        J11=J10+9*NETO         !9*NETO
        J12=J11+9*NETO         !9*NETO
        Jmax=J12+9*NETO          
             
        IF(NEL.EQ.1) CALL ANNULD(US,Jmax)

        CALL USHULLS(NETO,NEL,SECT,SIGY,E,Q,DELTA,PHIL,									
     *         DYA,TYA,WYA,HYA,EPSA,KSA,HXR,DXR,WXR,TXR,EPSR,ENTR,KSR,
     *         US(J1),US(J2),US(J3),US(J4),US(J5),US(J6),US(J7),    ! dont DHULL
     *         US(J8),US(J9),US(J10),US(J11),US(J12),
     *         NVAR,NXIT)

c        PAUSE'BO2'
        
	ELSE
        WRITE(66,'(//A/)')'This method (Ult. Strength) is not defined'
	ENDIF

c     WRITE(*,*) 'end of BO2'
c	PAUSE

	
	ENDIF
C***************************************************************** 
C.8  Vérification au flambement de l'épontille			!nouvelle sousroutine !février 2004
C    -----------------------------------------					 

	IF(ITYPE(NEL).EQ.5) THEN							!r&d13											 
	IF(VNyMAX.LE.1000) GOTO 558		!pas de compression			 
	CALL FLAMB(ITYPE(NEL),Heff,AIRE,AIx,SIGY,HYA,ISECT,	!r&d13			 
     *			TYA,WYA,E,VNyMAX,VMyMAX,SyMAX,SyMIN,			 
     *			VM1,VM2,EPSA)
 558	CONTINUE
	ENDIF														 

C***********************************************************************
C***********************************************************************
C 9.  LES FORMATS
C     ------------
   73 FORMAT(' PANNEAU-PANEL No ',I2,/20(1H+) )
   74 FORMAT(/' TERME NUMERO ',I2,' DE LA SERIE DE FOURIER')
   72 FORMAT(/' INCONNUES HYPERSTATIQUES CORRESPONDANT'/' AUX CONDITIO',
     *'NS AUX LIMITES DE LA PLAQUE REELLE'/1X,44(1H+)/' ELLES SONT ',
     *'ORDONNEES DANS L''ORDRE C D A B'//4(2X,E14.7)/4(2X,E14.7)/)
  182 FORMAT(A4,1X,5(1X,E13.6))
  197 FORMAT(/'!!! Le calcul de l''épaisseur minimale (Hughes)',
     *       ' n''est pas valable pour une coque cylindrique !!!'/
     *       ' Le calcul ne sera donc pas fait .'/
     *       ' Veuillez vérifier la restriction manuellement'/)
  198 FORMAT(' NEL=',I2,': d min =',E13.6,' d TPL/EPSA =',E11.4,
     *              ' d TPL/EPSR =',E11.4,' d TPL/Delta=',E11.4,
     *              ' !! Valeurs par défaut !!')
  641 FORMAT(T2,I2,5E14.7)
  782 FORMAT(/'Pente DW/DX (c.à.d. la rotation autour axe OY)'/)

      RETURN
      END

