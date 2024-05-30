      SUBROUTINE COSTMAIN(totalRRo,VLARG,VEPAISS,VHAC,VEAC,VLSC,
     *                    VESC,DELTAC,VHAR,VEAR,VLSR,VESR,DELTAR,ENTR,
     *                    VLENGTH,NETO,NON,NUMV,CoutPAN,DERIVEE,NDER,
     *                    NVAR,NXIT,TRAV,totalTI)
      
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccc     MODULE COUT (ALSTOM)     - LBR.5  - ANAST  ULG 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Développé par :  Catalin TODERAN et Frederic BAIR  
c                   Décembre 2002  -    Avril 2003   
c     
c Ce module est appelé si IOPTI = 2 (sinon c'est le module OBJCT1 qui est utilisé, calcul simplifié)
c
C Modifications:
c   19 Juin 2003 : Cout des soudures # MIN(vepaiss(i),veac(i)); Ligne 1885 = 
c   11 septembre 2003 : quelques adaptations pour pouvoir combiner les 2 versions                  
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c	   OPEN (unit=1, file='DBinput.txt',          status='old',ERR=906) !juillet2003
c	   OPEN (unit=2, file='DBfractionnement.txt', status='old')
c	   OPEN (unit=3, file='DBacces.txt',          status='old',ERR=908) !juillet2003
c        OPEN (unit=7, file='DBbase.txt',           status='old',ERR=904) !juillet2003
c 	   OPEN (unit=44,file='DBsoudures.txt',       status='old',ERR=905) !juillet2003
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

	IMPLICIT REAL*8 (A-G,K,O-Z)

c      DIMENSION VLARG(1),VEPAISS(1),VHAC(1),VEAC(1),VLSC(1),
c     *   VESC(1),DELTAC(1),VHAR(1),VEAR(1),VLSR(1),DELTAR(1),ENTR(1),
c     *   TRAV(234965),CoutPAN(NETO),DERIVEE(1),NVAR(1),NXIT(1)
      DIMENSION TRAV(214685),CoutPAN(NETO)	!extension neto
      
      OPEN (unit=7, file='DBbase.txt',     status='old',ERR=904) !juillet2003
	OPEN (unit=44,file='DBsoudures.txt', status='old',ERR=905) !juillet2003

	READ(7,*) NNN  ! = nombre d'opérations
	REWIND 7
	CLOSE(7)
      NOID = 25      ! = nombre ID soudure (il faudrait pouvoir le lire directement
	               !                      dans le fichier base.txt)
      READ(44,*)
	READ(44,*) NROPID ! = nombre d'opérations qui nécessitent ID soudure  (max=8)
	REWIND 44
	CLOSE(44)

      MAXD = 35     ! = nombre maximal de points de données à notre disposition
	              !   (dans toutes les opérations)
	MAXS = 21     ! idem mais pour les soudures
	J2 = MAX0(MAXD,MAXS)

      IROWMAX = 10  ! = nombre max de ligne par opération dans le fichier base.txt   
	IRIDMAX = 3   ! = idem pour les soudures (base.txt aussi)
	J1 = MAX0(IROWMAX,IRIDMAX)

      I1  = 1                  !iop(NNN)
	I2  = I1  + NNN          !ivtolier(NNN)
	I3  = I2  + NNN          !ivsoudeur(NNN)
	I4  = I3  + NNN          !tolier(NNN,MAXD) 
	I5  = I4  + NNN*MAXD     !soudeur(NNN,MAXD)
	I6  = I5  + NNN*MAXD     !variablet(NNN,MAXD)
	I7  = I6  + NNN*MAXD     !variables(NNN,MAXD)
	I8  = I7  + NNN*MAXD     !c(J1,J2)
	I9  = I8  + J1*J2        !iv(NNN,IROWMAX)
	I10 = I9  + NNN*IROWMAX  !irow(NNN)
	I11 = I10 + NNN          !nci(NETO)
	I12 = I11 + NETO         !nanp(NETO)
	I13 = I12 + NETO         !idpanneau(NETO)
	I14 = I13 + NETO         !ippanneau(NETO)
	I15 = I14 + NETO         !itpanneau(NETO)
	I16 = I15 + NETO         !nttapes(NETO)
	I17 = I16 + NETO         !npabouts(NETO)
	I18 = I17 + NETO         !K(NNN,NETO)
	I19 = I18 + NNN*NETO     !ca(NNN,NETO) 
	I20 = I19 + NNN*NETO     !cat(NNN,NETO)
	I21 = I20 + NNN*NETO     !variables1(NNN,MAXD)
	I22 = I21 + NNN*MAXD      !soudeur1(NNN,MAXD)
	I23 = I22 + NNN*MAXD      !variablet1(NNN,MAXD)
	I24 = I23 + NNN*MAXD      !tolier1(NNN,MAXD)
	I25 = I24 + NNN*MAXD      !iid(NOID)
	I26 = I25 + NOID         !irid(NOID)
	I27 = I26 + NOID         !is(NOID,IRIDMAX)
	I28 = I27 + NOID*IRIDMAX !ts(NOID,MAXS)
	I29 = I28 + NOID*MAXS    !vst(NOID,MAXS)
	I30 = I29 + NOID*MAXS    !ss(NOID,MAXS)
	I31 = I30 + NOID*MAXS    !vss(NOID,MAXS)
	I32 = I31 + NOID*MAXS    !ivts(NOID)
	I33 = I32 + NOID         !ivss(NOID)
	I34 = I33 + NOID         !idsoudures(NROPID,NETO)
	I35 = I34 + NROPID*NETO  !vgorsoud(NROPID,NETO)
	I36 = I35 + NROPID*NETO  !Q(NNN,NETO)
	I37 = I36 + NNN*NETO     !cut(NNN,NETO)
	I38 = I37 + NNN*NETO     !cus(NNN,NETO)
	I39 = I38 + NNN*NETO     !RROtol(NNN,NETO)
	I40 = I39 + NNN*NETO     !RROsoud(NNN,NETO)
	I41 = I40 + NNN*NETO     !TItol(NNN,NETO)
	I42 = I41 + NNN*NETO     !TIsoud(NNN,NETO)
	I43 = I42 + NNN*NETO     !memovart(MAXD)
	I44 = I43 + MAXD         !memovars(MAXD)
	I45 = I44 + MAXD         !memosou(MAXD)
	I46 = I45 + MAXD         !memotol(MAXD)
	I47 = I46 + MAXD         !PANTOLTI(NETO)
      I48 = I47 + NETO         !PANTOLRRO(NETO)
	I49 = I48 + NETO         !PANSOUDTI(NETO)
	I50 = I49 + NETO         !PANSOUDRRO(NETO)
	I51 = I50 + NETO         !PANTotalTI(NETO)
	I52 = I51 + NETO         !T12(NNN,NETO)
	I53 = I52 + NNN*NETO     !T15(NNN,NETO)
	I54 = I53 + NNN*NETO     !T16(NNN,NETO)
	I55 = I54 + NNN*NETO     !T19(NNN,NETO)
	I56 = I55 + NNN*NETO     !T21s(NNN,NETO)
	I57 = I56 + NNN*NETO     !T23s(NNN,NETO)
	I58 = I57 + NNN*NETO     !T26s(NNN,NETO)
	I59 = I58 + NNN*NETO     !T21t(NNN,NETO)
	I60 = I59 + NNN*NETO     !T23t(NNN,NETO)
	I61 = I60 + NNN*NETO     !T26t(NNN,NETO)
      Itest=4*NNN+20*NNN*NETO+J1*J2+NNN*IROWMAX+12*NETO+8*NNN
     *       *MAXD+4*NOID+NOID*IRIDMAX+4*NOID*MAXS+2*NROPID*NETO+4*MAXD
c	CALL ANNULD(TRAV,4*NNN+20*NNN*NETO+J1*J2+NNN*IROWMAX+12*NETO+8*NNN
c     *       *MAXD+4*NOID+NOID*IRIDMAX+4*NOID*MAXS+2*NROPID*NETO+4*MAXD)
c      CALL ANNULD(TRAV,Itest)
      CALL ANNULD(TRAV,214685)				!extension neto

c      En prenant NNN max = 100 :

c total = 4*NNN + 20*NNN*NETO + J1*J2 + NNN*IROWMAX + 12*NETO + 8*NNN*MAXD + 4*NOID + NOID*IRIDMAX + 4*NOID*MAXS +2*NROPID*NETO + 4*MAXD
C       = 400   + 240000      + 350   + 1000        + 1440    + 28000      + 100    + 75           + 2100        + 1920         + 140
C       = 214685   

	CALL COSTMAIN2(totalRRo,VLARG,VEPAISS,VHAC,VEAC,VLSC,
     *  VESC,DELTAC,VHAR,VEAR,VLSR,VESR,DELTAR,ENTR,VLENGTH,NETO,NON,
     *  NUMV,NNN,MAXD,MAXS,IROWMAX,IRIDMAX,NOID,NROPID,J1,J2,DERIVEE,
     *  NDER,NVAR,NXIT,totalTI,
     *  TRAV(I1),TRAV(I2),TRAV(I3),TRAV(I4),TRAV(I5),TRAV(I6),TRAV(I7),
     *  TRAV(I8),TRAV(I9),TRAV(I10),TRAV(I11),TRAV(I12),TRAV(I13),
     *  TRAV(I14),TRAV(I15),TRAV(I16),TRAV(I17),TRAV(I18),TRAV(I19),
     *  TRAV(I20),TRAV(I21),TRAV(I22),TRAV(I23),TRAV(I24),TRAV(I25),
     *  TRAV(I26),TRAV(I27),TRAV(I28),TRAV(I29),TRAV(I30),TRAV(I31),
     *  TRAV(I32),TRAV(I33),TRAV(I34),TRAV(I35),TRAV(I36),TRAV(I37),
     *  TRAV(I38),TRAV(I39),TRAV(I40),TRAV(I41),TRAV(I42),TRAV(I43),
     *  TRAV(I44),TRAV(I45),TRAV(I46),TRAV(I47),TRAV(I48),TRAV(I49),
     *  TRAV(I50),TRAV(I51),CoutPAN,
     *  TRAV(I52),TRAV(I53),TRAV(I54),TRAV(I55),TRAV(I56),TRAV(I57),
     *  TRAV(I58),TRAV(I59),TRAV(I60),TRAV(I61))
        
	RETURN
  904 WRITE(*,*) 'STOP : The "base.txt" File is missing'     !juillet2003
	WRITE(29,*)'STOP : The "base.txt" File is missing'	   !bug
      STOP                                                   !juillet2003
  905 WRITE(*,*) 'STOP : The "soudures.txt" File is missing' !juillet2003
	WRITE(29,*)'STOP : The "soudures.txt" File is missing' !bug
	STOP
	END
	     
c =================================================================================
c =================================================================================

	SUBROUTINE COSTMAIN2(totalRRo,VLARG,VEPAISS,VHAC,VEAC,VLSC,
     *  VESC,DELTAC,VHAR,VEAR,VLSR,VESR,DELTAR,ENTR,VLENGTH,NETO,NON,
     *  NUMV,NNN,MAXD,MAXS,IROWMAX,IRIDMAX,NOID,NROPID,J1,J2,DERIVEE,
     *  NDER,NVAR,NXIT,totalTI,
     *  iop,ivtolier,ivsoudeur,tolier,soudeur,variablet,variables,
     *  c,iv,irow,nci,nanp,idpanneau,
     *  ippanneau,itpanneau,nttapes,npabouts,K,ca,
     *  cat,variables1,soudeur1,variablet1,tolier1,iid,
     *  irid,is,ts,vst,ss,vss,
     *  ivts,ivss,idsoudures,vgorsoud,Q,cut,
     *  cus,RROtol,RROsoud,TItol,TIsoud,memovart,
     *  memovars,memosou,memotol,PANTOLTI,PANTOLRRO,PANSOUDTI,
     *  PANSOUDRRO,PANTotalTI,PANTotalRRO,
     *  T12,T15,T16,T19,T21s,T23s,
     *  T26s,T21t,T23t,T26t)

	IMPLICIT real*8 (a-g,k,o-z)
	INTEGER kst,kkjj,k1,kk
	REAL*8 memosou,memovart,memovars,memotol
	DIMENSION iop(NNN),ivtolier(NNN),ivsoudeur(NNN),tolier(NNN,NETO),
     *   soudeur(NNN,NETO), variablet(NNN,MAXD), variables(NNN,MAXD),
     *   c(J1,J2),iv(NNN,IROWMAX),irow(NNN),
     *   nci(NETO),nanp(NETO),idpanneau(NETO),ippanneau(NETO),
     *   itpanneau(NETO),nttapes(NETO),npabouts(NETO),K(NNN,NETO),
     *   ca(NNN,NETO),cat(NNN,NETO),div(2),variables1(NNN,MAXD),
     *   soudeur1(NNN,MAXD),variablet1(NNN,MAXD),tolier1(NNN,MAXD),
     *   iid(NOID),irid(NOID),is(NOID,IRIDMAX),ts(NOID,MAXS),
     *   vst(NOID,MAXS),ss(NOID,MAXS),vss(NOID,MAXS),ivts(NOID),
     *   ivss(NOID),idsoudures(NROPID,NETO),vgorsoud(NROPID,NETO),
c ----------------
     *   vlarg(NETO),vepaiss(NETO),vhac(NETO),veac(NETO),
     *   vlsc(NETO),vesc(NETO),deltac(NETO),vhar(NETO),vear(NETO),
     *   vlsr(NETO),vesr(NETO),deltar(NETO),ENTR(NETO),
     *   Q(NNN,NETO),
c ----------------
     *   cut(NNN,NETO),cus(NNN,NETO),RROtol(NNN,NETO),RROsoud(NNN,NETO),
     *   TItol(NNN,NETO),TIsoud(NNN,NETO),
     *   memovart(MAXD),memovars(MAXD),memosou(MAXD),
     *   memotol(MAXD),
c ----------------
     *   PANTOLTI(NETO),PANTOLRRO(NETO),PANSOUDTI(NETO),
     *   PANSOUDRRO(NETO),PANTotalTI(NETO),PANTotalRRO(NETO),
c ----------------
     *   T12(NNN,NETO),T15(NNN,NETO),T16(NNN,NETO),T19(NNN,NETO),
     *   T21s(NNN,NETO),T23s(NNN,NETO),T26s(NNN,NETO),
     *   T21t(NNN,NETO),T23t(NNN,NETO),T26t(NNN,NETO),
c -----------------
     *   DERIVEE(NDER),NVAR(NETO),NXIT(9,NETO)

ccc	DIMENSION tolier2(52,35),soudeur2(52,35),num(35) 


      COMMON/COUT/ Poids,SPoids,
     *        Fmat,Fsou,FMdO,Fmat11,Fmat22,Fmat33,Fsou11,Fsou22,
     *        Fsou33,FMdO11,FMdO22,FMdO33,FMdO44,FMdO55,FMdO66,FMdO77,
     *      REND,EQP,Dref,DrefX,DrefY,C11,C22,C33,DC11,DW2,DW3,P10,DP10,
     *        P4,P5,DP4,DP5,P9X,P9Y,DP9X,DP9Y,BER,BET,P6,P7,C8,DC8,COUT,
     *        ICOUT,IALR,IALT      !nécessaire pour avoir ICOUT
c	COMMON/COUT/ICOUT
	COMMON/ALSTOM/IDIFF,NBR,PAS
     	
	IF (ICOUT.EQ.3) THEN
	  XXX = PAS
	  PAS = 0.   !car dans costmain, on ne se sert du pas que pour ICOUT=2
      ENDIF
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     LISTE DES VARIABLES UTILISEES		
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c	nnn   = nombre total des opérations
c	iop(i)= numero de l'opération (pré-pré, pre, montage)	i=1,nnn 
c	tolier(i,j) = couts unitaires tolier					i=i
c	soudeur(i,j)= couts unitaires soudeur					i=i
c	variablet(i,j)= matrice des valeurs de la variable des CU tôlier
c	variables(i,j)= matrice des valeurs de la variable des CU tôlier
c	c(i,j) =matrice intermediaire de lecture de la base de données
c	nci(i) =nombre couples intermediaires                   i=indice du panneau
c	nanp(i)=nombre abouts de la nappe plane					i=indice du panneau
c	idpanneau(i)=paramètre ID PANNEAU						i=indice du panneau
c	ippanneau(i)=paramètre IP PANNEAU						i=indice du panneau
c	itpanneau(i)=paramètre IT PANNEAU						i=indice du panneau
c	nttapes(i)  =paramètre TYPE TAPES						i=indice du panneau
c	npabouts(i) =paramètre P ABOUTS							i=indice du panneau
c	k(i,j)  =coeff. de fractionnement		i=indice panneau,j=indice opération
c	ca(i,j) =coeff. de acces				i=indice panneau,j=indice opération
c	cat(i,j)=coeff. de atelier				i=indice panneau,j=indice opération

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  0.0   Initialisation des variables
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
 
	iop= 0
	ivtolier= 0
	ivsoudeur= 0
	tolier= 0.
	soudeur= 0.
	variablet= 0.
	variables= 0.
      c= 0.
	iv= 0
	irow= 0
      nci= 0
	nanp= 0
	idpanneau= 0
	ippanneau= 0
     	itpanneau= 0
      nttapes= 0
	npabouts= 0
	K= 0
	ca= 0.
	cat= 0.
	div= 0.
	variables1= 0.
      soudeur1= 0.
	variablet1= 0.
	tolier1= 0.
      iid= 0
	irid= 0
	is= 0
	ts= 0.
	vst= 0.
      ss= 0.
	vss= 0.
	ivts= 0
	ivss= 0
	idsoudures= 0.
	vgorsoud= 0.

	Q= 0.

    	cut= 0.
	cus= 0.
	RROtol= 0.
	RROsoud= 0.
     	TItol= 0.
	TIsoud= 0.
	TOTALTI=0.
	memovart =0.
      memovars =0.
	memotol = 0.
	memosou = 0.

	KKJJ = 0
	KST = 0
	PARAS = 0.
	PARAT = 0.
	IIJJ = 0
	V1 = 0.
	V2 = 0.
	C1 = 0.
      C2 = 0.
	ID = 0
	NRPANNEAUX = 0
	NK = 0
	NN = 0
	NKKKK = 0
	NNNNN = 0
	CRP = 0.
	CBP = 0.
	CMN = 0.
	CBN = 0.
	CTTPP = 0.
	CTSPP = 0.
	IJK = 0
	I = 0
	J = 0
	PMB = 0.
	NAM = 0
	NCATELIER = 0
	NCACCES = 0
	NFRACT = 0
	IDSO = 0
	ID = 0

	SUMTOLTI=0. ! ceux-là, indispensable de les annuler !!!
	SUMSOUDTI=0.
	SUMTOLRRO=0.
	SUMSOUDRRO=0.
	sumprepreti=0.
	sumpreprerro=0.
	sumprenti=0.
	sumprenrro=0.
	sumpreassti=0.
	sumpreassrro=0.
	sumpremonti=0.
	sumpremonrro=0.

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 1.0	OPEN FILES			
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	open (unit=1, file='DBinput.txt',          status='old',ERR=906) !juillet2003
c	open (unit=2, file='DBfractionnement.txt', status='old')
	open (unit=7, file='DBbase.txt',           status='old')
	open (unit=8, file='RESULTATS COUT.txt', status='unknown')
	open (unit=88,file='SENSIBILITES.txt',   status='unknown')
c	open (unit=9, file='VERIF INPUT.txt' ,   status='unknown')


c 	FORMATS		
c     =======
111	format (f4.2)
112	format (I3)
44	format ('COUT TOLIER  =',f4.2)
45	format ('COUT SOUDEUR =',f4.2)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 2.0	LECTURE DU FICHIER INPUT        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	write(8,*) 'FICHIER DES RESULTATS'
	write(8,*) '====================='
	write(8,*) ' '
c
	write(8,*) 'VERIFICATION INPUT MODULE COUT'
	write(8,*) '------------------------------'
	write(8,*) ' '
		read(1,*) NETO
		read(1,*) nfract
		read(1,*) ncacces
		read(1,*) ncatelier
	write(8,*) 'NOMBRE PANNEAUX'
	write(8,112) NETO
	write(8,*) ' '

	If (nfract.eq.0) then
	  write(8,*) 'FRACTIONNEMENT PAR DEFAUT'
	  write(8,*) ' '
	else
	  write(8,*) 'FRACTIONNEMENT DEFINI PAR L UTILISATEUR'
	  write(8,*) ' '
	endif

	If (ncacces.eq.0) then
	  write(8,*) 'COEFF ACCES PAR DEFAUT'
	  write(8,*) ' '
	else
	  write(8,*) 'COEFF ACCES DEFINI PAR L UTILISATEUR'
	  write(8,*) ' '
	endif

	If (ncatelier.eq.0) then
	  write(8,*) 'COEFF ATELIER PAR DEFAUT'
	  write(8,*) ' '
	else
	  write(8,*) 'COEFF ATELIER DEFINI PAR L UTILISATEUR'
	  write(8,*) ' '
	endif

		read(1,*) nam
		read(1,*) pmb
		read(1,*) (div(i),i=1,2)
	write(8,*) ' '
	write(8,*) 'Nombre abouts montage','    ',nam
	write(8,*) 'Cout Preparer-mettre-balancer','  ',pmb
	write(8,*) 'Cout DIVERS tolier','  ',div(1)
	write(8,*) 'Cout DIVERS soudeur','  ',div(2)
	write(8,*) ' '	
		
		read(1,*) (nci(i),i=1,NETO)
		read(1,*) (nanp(i),i=1,NETO)
		read(1,*) (idpanneau(i),i=1,NETO)
		read(1,*) (ippanneau(i),i=1,NETO)
		read(1,*) (itpanneau(i),i=1,NETO)
		read(1,*) (nttapes(i),i=1,NETO)
		read(1,*) (npabouts(i),i=1,NETO)

	write(8,*) 'No','   ','NCI','   ','NANP','   ','ID PANNEAU',
     *'   ','IP PANNEAU','   ','IT PANNEAU','   ','TYPE TAPES','   ',
     *'P ABOUTS'

	do i=1,NETO
	  write(8,*) i,nci(i),nanp(i),idpanneau(i)
     *        ,ippanneau(i),itpanneau(i),nttapes(i),npabouts(i)
	enddo

	write(8,*) '****************************************************'
c	write(*,*) 'LECTURE INPUT TERMINEE'


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  3.0   LECTURE DE LA BASE DE DONNEES	
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	write(8,*) 'VERIFICATION DE LA LECTURE DE LA BASE DE DONNEES'
	write(8,*) '------------------------------------------------'
 
	READ(7,*) nnn
	write(8,*) 'NOMBRE OPERATIONS = ',nnn
	write(8,*) ' '

      do i=1,nnn
	  ivtolier(i)=1
	  ivsoudeur(i)=1
	enddo
c ===========================
C     Boucle 778 et 777
c     -----------------
      DO 778 i=1,nnn  
	  read(7,*) iop(i),irow(i)
c	  write(8,*) ' OPERATION',' ',iop(i)
c	  write(8,*) ' '
	  do j=1,irow(i)
	    read(7,*) iv(i,j),(c(j,ijk),ijk=1,iv(i,j))
	  enddo

C     Boucle 777 (interne à 778)
c     -------------------------
122	do 777 j=1,irow(i)  

	if(iv(i,j).eq.1.and.j.eq.1) then 
	  tolier(i,1)=c(j,1)
c	  write(8,*) 'cout unitaire tolier =',tolier(i,1)
	  ivtolier(i)=1
	endif

	if(iv(i,j).eq.1.and.j.ne.1) then
	  soudeur(i,1)=c(j,1)
c	  write(8,*) 'cout unitaire soudeur =',soudeur(i,1)
	  ivsoudeur(i)=1
	endif

	if(iv(i,j).ne.1.and.j.eq.1) then 
	  do ijk=1,iv(i,j)
	    variablet(i,ijk)=c(j,ijk)
	    tolier(i,ijk)=c(j+1,ijk)
	  enddo
	  ivtolier(i)=iv(i,j)
	endif

	if(iv(i,1).eq.1.and.iv(i,2).ne.1) then 
	  do ijk=1,iv(i,2)
	    variables(i,ijk)=c(2,ijk)
	    soudeur(i,ijk)=c(3,ijk)
	  enddo
	  ivsoudeur(i)=iv(i,3)
	endif

	if(iv(i,1).ne.1) then 
	  do ijk=1,iv(i,3)
	    variables(i,ijk)=c(3,ijk)
	    soudeur(i,ijk)=c(4,ijk)
	  enddo
c	  write(8,*) 'variable',(variablet(i,ijk),ijk=1,iv(i,j))
c	  write(8,*) 'tolier  ',(tolier(i,ijk),ijk=1,iv(i,j))
c	  write(8,*) ' '
   	  ivsoudeur(i)=iv(i,3)
	endif

cccccccccccccccccccccccccccccccccc
	if(i.eq.19.or.i.eq.20) then
	  do ijk=1,iv(i,j)
	    variables(i,ijk)=c(2,ijk)
	    soudeur(i,ijk)=c(3,ijk)
	    variables1(i,ijk)=c(4,ijk)
	    soudeur1(i,ijk)=c(5,ijk)
	  enddo
c	  write(8,*) 'cout unitaire soudeur'
c	  write(8,*) soudeur(i,1),'  si epaiss <',variables(i,1)
c	  write(8,*) soudeur(i,2),'  si epaiss >',variables(i,2)
c	  write(8,*) ' '
	  ivsoudeur(i)=iv(i,2)
	endif

ccccccccccccccccccccccccccccccccccc
	if(i.eq.25.or.i.eq.26) then
	  do ijk=1,iv(i,j)
	     variablet(i,ijk) =c(1,ijk)
	     variables(i,ijk) =c(1,ijk)
	     variables1(i,ijk)=c(2,ijk)
	     variablet1(i,ijk)=c(2,ijk)
	     tolier(i,ijk)  =c(3,ijk)
	     soudeur(i,ijk) =c(4,ijk)
	     tolier1(i,ijk) =c(7,ijk)
	     soudeur1(i,ijk)=c(8,ijk)
	  enddo
c	  write(8,*) 'hauteur             ',(variablet(i,ijk),ijk=1,iv(i,j))
c	  write(8,*) 'largeur             ',(variablet1(i,ijk),
c      *                                  ijk=1,iv(i,j))
c	  write(8,*) 'tolier recouvrement ',(tolier(i,ijk),ijk=1,iv(i,j))
c	  write(8,*) 'soudeur recouvrement',(soudeur(i,ijk),ijk=1,iv(i,j))
c	  write(8,*) 'tolier encastrees   ',(tolier1(i,ijk),ijk=1,iv(i,j))
c	  write(8,*) 'soudeur encastrees  ',(soudeur1(i,ijk),ijk=1,iv(i,j))
c	  write(8,*) ' '
	  ivtolier(i)=iv(i,j)
	  ivsoudeur(i)=iv(i,j)
	endif

ccccccccccccccccccccccccccccccccc
	if(i.eq.39.or.i.eq.40) then
	  do ijk=1,iv(i,j)
	    variablet(i,ijk) =c(1,ijk)
	    variables(i,ijk) =c(1,ijk)
	    variables1(i,ijk)=c(2,ijk)
	    variablet1(i,ijk)=c(2,ijk)
	    tolier(i,ijk)  =c(3,ijk)
	    soudeur(i,ijk) =c(4,ijk)
	    tolier1(i,ijk) =c(7,ijk)
	    soudeur1(i,ijk)=c(8,ijk)
	  enddo
c	  write(8,*) 'hauteur             ',(variablet(i,ijk),ijk=1,iv(i,j))
c	  write(8,*) 'largeur             ',(variablet1(i,ijk),
c       *          ijk=1,iv(i,j))
c	  write(8,*) 'tolier recouvrement ',(tolier(i,ijk),ijk=1,iv(i,j))
c	  write(8,*) 'soudeur recouvrement',(soudeur(i,ijk),ijk=1,iv(i,j))
c	  write(8,*) 'tolier encastrees   ',(tolier1(i,ijk),ijk=1,iv(i,j))
c	  write(8,*) 'soudeur encastrees  ',(soudeur1(i,ijk),ijk=1,iv(i,j))
c	  write(8,*) ' '
	  ivtolier(i)=iv(i,j)
	  ivsoudeur(i)=iv(i,j)
	endif
cccccccccccccccccccccccccccccccccccc	
	if(i.eq.30.or.i.eq.31) then
	  do ijk=1,iv(i,j)
	    variablet(i,ijk) =c(1,ijk)
	    variables(i,ijk) =c(1,ijk)
	    variables1(i,ijk)=c(2,ijk)
	    variablet1(i,ijk)=c(2,ijk)
	    tolier(i,ijk) =c(3,ijk)
	    soudeur(i,ijk)=c(4,ijk)
	  enddo
c	write(8,*) 'hauteur ',(variablet(i,ijk),ijk=1,iv(i,j))
c	write(8,*) 'largeur ',(variablet1(i,ijk),ijk=1,iv(i,j))
c	write(8,*) 'tolier  ',(tolier(i,ijk),ijk=1,iv(i,j))
c	write(8,*) 'soudeur ',(soudeur(i,ijk),ijk=1,iv(i,j))
c	write(8,*) ' '
	  ivtolier(i)=iv(i,j)
	  ivsoudeur(i)=iv(i,j)

	endif

ccccccccccccccccccccccccccccccccccccc
	if(i.eq.44.or.i.eq.45) then
	  do ijk=1,iv(i,j)
	    variablet(i,ijk) =c(1,ijk)
	    variables(i,ijk) =c(1,ijk)
	    variables1(i,ijk)=c(2,ijk)
	    variablet1(i,ijk)=c(2,ijk)
	    tolier(i,ijk) =c(3,ijk)
	    soudeur(i,ijk)=c(4,ijk)
	    ivtolier(i)=iv(i,j)
	    ivsoudeur(i)=iv(i,j)
	  enddo
c	write(8,*) 'hauteur ',(variablet(i,ijk),ijk=1,iv(i,j))
c	write(8,*) 'largeur ',(variablet1(i,ijk),ijk=1,iv(i,j))
c	write(8,*) 'tolier  ',(tolier(i,ijk),ijk=1,iv(i,j))
c	write(8,*) 'soudeur ',(soudeur(i,ijk),ijk=1,iv(i,j))
c	write(8,*) ' '
	endif

c      if(i.eq.49) then
c	  do ijk=1,iv(i,j)
c	    variablet(i,ijk) = c(1,ijk)
c	    variablet1(i,ijk) = c(2,ijk)
c	    tolier(i,ijk) = c(3,ijk)
c          tolier1(i,ijk) = c(4,ijk)
c	    tolier2(i,ijk) = c(5,ijk)
c          variables(i,ijk) = c(6,ijk)
c          variables1(i,ijk) = c(7,ijk)
c	    soudeur(i,ijk) = c(8,ijk)
c          soudeur1(i,ijk) = c(9,ijk)
c	    soudeur2(i,ijk) = c(10,ijk)
c	    ivtolier(i)=iv(i,j)
c	    ivsoudeur(i)=iv(i,j)
c	  enddo
c	endif
c
c      if(i.eq.50) then
c	  tolier(i,1) = c(1,1)
c        tolier1(i,1) = c(2,1)
c	  tolier2(i,1) = c(3,1)
c	  ivtolier(i) = 1
c	  do ijk=1,iv(i,j)  
c          variables(i,ijk) = c(4,ijk)
c          variables1(i,ijk) = c(5,ijk)
c	    soudeur(i,ijk) = c(6,ijk)
c          soudeur1(i,ijk) = c(7,ijk)
c	    soudeur2(i,ijk) = c(8,ijk)	    
c	    ivsoudeur(i)=iv(i,j)
c	  enddo
c	endif
   

	go to 777

c ---------------------------
c	if(iv(i,j).ne.1.and.j.eq.1) then 
c	do ijk=1,iv(i,j)
c	variablet(i,ijk)=c(j,ijk)
c	tolier(i,ijk)=c(j+1,ijk)
c	enddo
c	write(8,*) 'variable',(variablet(i,ijk),ijk=1,iv(i,j))
c	write(8,*) 'tolier  ',(tolier(i,ijk),ijk=1,iv(i,j))
c	write(8,*) ' '
c	endif
c	if(iv(i,j).ne.1.and.j.ne.1) then
c	do ijk=1,iv(i,j)
c	variables(i,ijk)=c(j,ijk)
c	soudeur(i,ijk)=c(j+1,ijk)
c	enddo
c	write(8,*) 'variable',(variables(i,ijk),ijk=1,iv(i,j))
c	write(8,*) 'soudeur ',(soudeur(i,ijk),ijk=1,iv(i,j))
c	write(8,*) ' '
c	endif
c ----------------------------
		
777	CONTINUE

778	CONTINUE
c ==================================================

	do i=1,nnn
	  write(8,*) ' OPERATION',' ',iop(i)
	  write(8,*) ' '
	  write(8,*) ' '
	  write(8,*) 'variable tolier'
	  write(8,*) (variablet(i,j),j=1,ivtolier(i))
	  write(8,*) ' '
	  write(8,*) 'cout tolier'
	  write(8,*) (tolier(i,j),j=1,ivtolier(i))
	  write(8,*) ' '
	  write(8,*) 'variable soudeur'
	  write(8,*) (variables(i,j),j=1,ivsoudeur(i))
	  write(8,*) ' '
	  write(8,*) 'cout soudeur'
	  write(8,*) (soudeur(i,j),j=1,ivsoudeur(i))
	  write(8,*) ' '
	enddo
c	write(*,*) 'LECTURE BASE DE DONNEES TERMINEE'


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 4.0 	LECTURE COEFF ET ID SOUDURES           
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	read(7,*) ctspp
	write (8,*) '%%%%%%%%%%%%%%%%%%%%%%'
	write (8,*) 'coeff pre-pre sur soudeur', ctspp
	read(7,*) cttpp
	write (8,*) 'coeff pre-pre sur tolier', cttpp
	read(7,*) cbn
	write (8,*) 'coeff balayage nappe', cbn
	read(7,*) cmn
	write (8,*) 'coeff minorateur nappe', cmn
	read(7,*) cbp
	write (8,*) 'coeff balayage pre', cbp
	read(7,*) crp
	write (8,*) 'coeff reformage pre', crp

	write (8,*) '%%%%%%%%%%%%%%%%%%%%%%'	
	write (8,*) ' '
	write (8,*) 'ID SOUDURES'
	write (8,*) '==========='
	write (8,*) ' '
	read(7,*) noid
	write(8,*) 'Nombre ID Soudures =',noid
	write (8,*) ' '
c
	DO 780 i=1,noid  ! 780

	read(7,*) iid(i),irid(i)
c	write(8,*) ' ID',' ',iid(i)
c	write(8,*) ' '

	do j=1,irid(i)
	  read(7,*) is(i,j),(c(j,ijk),ijk=1,is(i,j))
	enddo
c
	DO j=1,irid(i) 
	  if(is(i,j).eq.1.and.j.eq.1) then 
  	    ts(i,1)=c(j,1)
c	    write(8,*) 'cout unitaire tolier =',ts(i,1)
	    ivts(i)=1
	  endif
	  if(is(i,j).eq.1.and.j.ne.1) then
	    ss(i,1)=c(j,1)
c	    write(8,*) 'cout unitaire soudeur =',ss(i,1)
	    ivss(i)=1
	  endif

	  if(is(i,j).ne.1.and.j.eq.1) then 
	    do ijk=1,is(i,j)
	      vst(i,ijk)=c(j,ijk)
	      ts(i,ijk)=c(j+1,ijk)
	    enddo
c	    write(8,*) 'variable',(vst(i,ijk),ijk=1,is(i,j))
c	    write(8,*) 'tolier  ',(ts(i,ijk),ijk=1,is(i,j))
c	    write(8,*) ' '
	    ivts(i)=is(i,j)
	  endif

	  if(is(i,1).eq.1.and.is(i,2).ne.1) then 
	    do ijk=1,is(i,2)
	      vss(i,ijk)=c(2,ijk)
	      ss(i,ijk)=c(3,ijk)
	    enddo
	    ivss(i)=is(i,3)
	  endif
	  if(is(i,1).ne.1) then 
	    do ijk=1,is(i,3)
	      vss(i,ijk)=c(3,ijk)
	      ss(i,ijk)=c(4,ijk)
	    enddo
	    ivss(i)=is(i,3)
	  endif
	ENDDO
 780	CONTINUE ! fin boucle 780

c	do i=1,noid
c	write(8,*) ' ID SOUDURE',' ',iid(i)
c	write(8,*) ' '
c	write(8,*) ' '
c	write(8,*) 'variable tolier'
c	write(8,*) (vst(i,j),j=1,ivts(i))
c	write(8,*) ' '
c	write(8,*) 'cout tolier'
c	write(8,*) (ts(i,j),j=1,ivts(i))
c	write(8,*) ' '
c	write(8,*) 'variable soudeur'
c	write(8,*) (vss(i,j),j=1,ivss(i))
c	write(8,*) ' '
c	write(8,*) 'cout soudeur'
c	write(8,*) (ss(i,j),j=1,ivss(i))
c	write(8,*) ' '
c	enddo

!!!	write(*,*)'LECTURE ID SOUDURES TERMINEE'
	
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 5.0  	LECTURE DES COEFF DE FRACTIONNEMENT       
c      et	GENERATION DES FRACTIONNEMENT PAR DEFAUT  
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	IF (nfract.EQ.1) then

	  open (unit=2,file='DBfractionnement.txt',status='old',ERR=907) !juillet2003

	  read(2,*) nnnnn
c	  write(*,*) 'nnnnn',nnnnn
	  read(2,*) nkkkk
c	  write(*,*) nkkkk

	  if(nnnnn.NE.NETO) then
	    write(*,*)'Nbr de panneaux incompatible: FRACTIONNEMENT-INPUT'
		write(29,*)'Nbr de panneaux incompatible: FRACTIONNEMENT-INPUT'			!bug
	    STOP
	  endif

	  read(2,*) ((K(i,j),j=1,nnnnn),i=1,nkkkk)

	ELSE  ! Valeur par défaut

	  nnnnn=NETO
	  nkkkk=nnn
	  do i=1,nnn
	  do j=1,NETO
	     K(i,j)=0.
	  enddo
	  enddo

	DO i=1,nnn
	DO j=1,NETO

	 if (i.eq.1) then
	   if (idpanneau(j).EQ.1.and.itpanneau(j).eq.2) then
	    K(i,j)=1.
	   else
	    K(i,j)=0.
	   endif
	 endif

	 if (i.eq.2) then
	   if (idpanneau(j).EQ.1.and.itpanneau(j).eq.2) then
	    K(i,j)=1.
	   else
	    K(i,j)=0.
	   endif
	   if (idpanneau(j).EQ.2) then
	     K(i,j)=1.
	   else
	     K(i,j)=0.
	   endif
	 endif

	 if (i.eq.3) then
	  if (idpanneau(j).EQ.1) then
	  K(i,j)=1.
	  else
	  K(i,j)=0.
	  endif
	 endif

	 if (i.eq.4) then
	  if (idpanneau(j).EQ.2) then
	  K(i,j)=1.
	  else
	  K(i,j)=0.
	  endif
	 endif

	 if (i.eq.5) then
	  if (idpanneau(j).EQ.2) then
	  K(i,j)=1.
	  else
	  K(i,j)=0.
	  endif
	 endif

	if (i.eq.6) then
	if (idpanneau(j).EQ.1.and.itpanneau(j).eq.2) then
	K(i,j)=1.
	else
	K(i,j)=0.
	endif
	endif 

	if (i.eq.7) then
	if (idpanneau(j).EQ.1.and.itpanneau(j).eq.2) then
	K(i,j)=1.
	else
	K(i,j)=0.
	endif
	endif

	if (i.eq.8) then
	if (idpanneau(j).EQ.1) then
	K(i,j)=1.
	else
	K(i,j)=0.
	endif
	endif

	if (i.eq.9) then
	if (idpanneau(j).EQ.1.and.itpanneau(j).eq.2) then
	K(i,j)=1.
	else
	K(i,j)=0.
	endif
	endif

	if (i.eq.10) then
	K(i,j)=0.
	endif

	if (i.eq.11) then
	if (idpanneau(j).EQ.1) then
	K(i,j)=1.
	else
	K(i,j)=0.
	endif
	endif

	if (i.eq.12) then
	if (idpanneau(j).EQ.1) then
	K(i,j)=1.
	else
	K(i,j)=0.
	endif
	endif

	if (i.eq.13) then
	if (idpanneau(j).EQ.1) then
	K(i,j)=1.
	else
	K(i,j)=0.
	endif
	endif

	if (i.eq.14) then
	if (idpanneau(j).EQ.1) then
	K(i,j)=1.
	else
	K(i,j)=0.
	endif
	endif

	if (i.eq.15) then
	if (idpanneau(j).EQ.1) then
	K(i,j)=1.
	else
	K(i,j)=0.
	endif
	endif

	if (i.eq.16) then
	if (idpanneau(j).EQ.1) then
	K(i,j)=1.
	else
	K(i,j)=0.
	endif
	endif

	if (i.eq.17) then
	if (idpanneau(j).EQ.1) then
	K(i,j)=1.
	else
	K(i,j)=0.
	endif
	endif

	if (i.eq.18) then
	if (idpanneau(j).EQ.1) then
	K(i,j)=1.
	else
	K(i,j)=0.
	endif
	endif

	if (i.eq.19) then
	if (idpanneau(j).EQ.1) then
	K(i,j)=1.
	else
	K(i,j)=0.
	endif
	endif

	if (i.eq.20) then
	K(i,j)=0.
	endif

	if (i.eq.21) then
	if (itpanneau(j).eq.1) then
	K(i,j)=1.
	else
	K(i,j)=0.
	endif
	endif

	if (i.eq.22) then
	if (idpanneau(j).EQ.2) then
	K(i,j)=1.
	else
	K(i,j)=0.
	endif
	endif

	if (i.eq.23) then
	if (idpanneau(j).EQ.2) then
	K(i,j)=1.
	else
	K(i,j)=0.
	endif
	endif
	if (i.eq.24) then
	if (itpanneau(j).eq.1) then
	K(i,j)=1.
	else
	K(i,j)=0.
	endif
	endif

	if (i.eq.25) then
	if (itpanneau(j).eq.1) then
	K(i,j)=1.
	else
	K(i,j)=0.
	endif
	endif

	if (i.eq.26) then
	K(i,j)=0.
	endif

	if (i.eq.27) then
	if (itpanneau(j).eq.1) then
	K(i,j)=1.
	else
	K(i,j)=0.
	endif
	endif

	if (i.eq.28) then
	if (itpanneau(j).eq.1) then
	K(i,j)=1.
	else
	K(i,j)=0.
	endif
	endif

	if (i.eq.29) then
	if (idpanneau(j).eq.1.and.itpanneau(j).eq.1) then
	K(i,j)=1.
	else
	K(i,j)=0.
	endif
	endif

	if (i.eq.30) then
	if (itpanneau(j).eq.1) then
	K(i,j)=1.
	else
	K(i,j)=0.
	endif
	endif

	if (i.eq.31) then
	K(i,j)=0.
	endif

	if (i.eq.32) then
	if (idpanneau(j).EQ.3) then
	K(i,j)=1.
	else
	K(i,j)=0.
	endif
	endif

	if (i.eq.33) then
	if (idpanneau(j).EQ.3) then
	K(i,j)=1.
	else
	K(i,j)=0.
	endif
	endif

	if (i.eq.34) then
	if (idpanneau(j).EQ.3) then
	K(i,j)=1.
	else
	K(i,j)=0.
	endif
	endif

	if (i.eq.35) then
	K(i,j)=0.
	endif

	if (i.eq.36) then
	if (itpanneau(j).eq.2) then
	K(i,j)=1.
	else
	K(i,j)=0.
	endif
	endif

	if (i.eq.37) then
	if (idpanneau(j).EQ.2) then
	K(i,j)=1.
	else
	K(i,j)=0.
	endif
	endif

	if (i.eq.38) then
	if (itpanneau(j).eq.2) then
	K(i,j)=1.
	else
	K(i,j)=0.
	endif
	endif

	if (i.eq.39) then
	if (itpanneau(j).eq.2) then
	K(i,j)=1.
	else
	K(i,j)=0.
	endif
	endif

	if (i.eq.40) then
	K(i,j)=0.
	endif

	if (i.eq.41) then
	if (itpanneau(j).eq.2) then
	K(i,j)=1.
	else
	K(i,j)=0.
	endif
	endif

	if (i.eq.42) then
	if (itpanneau(j).eq.2) then
	K(i,j)=1.
	else
	K(i,j)=0.
	endif
	endif

	if (i.eq.43) then
	if (itpanneau(j).eq.2) then
	K(i,j)=1.
	else
	K(i,j)=0.
	endif
	endif

	if (i.eq.44) then
	if (itpanneau(j).eq.2) then
	K(i,j)=1.
	else
	K(i,j)=0.
	endif
	endif

	if (i.eq.45) then
	K(i,j)=0.
	endif

	if (i.eq.46) then
	K(i,j)=1.
	endif

	if (i.eq.47) then
	if (ippanneau(j).eq.2) then
	K(i,j)=1.
	else
	K(i,j)=0.
	endif
	if(idpanneau(j).eq.3) then
	K(i,j)=1.
	endif
	if(idpanneau(j).eq.4) then
	k(i,j)=1.
	endif
	endif

	if (i.eq.48) then
	K(i,j)=1.
	endif

	if (i.eq.49) then
	if (idpanneau(j).eq.1.or.idpanneau(j).eq.3) then
	K(i,j)=1.
	else
	K(i,j)=0.
	endif
	endif

	if (i.eq.50) then
	if (idpanneau(j).eq.1.or.idpanneau(j).eq.3) then
	K(i,j)=1.
	else
	K(i,j)=0.
	endif
	endif

	if (i.eq.51) then
	if (idpanneau(j).eq.2) then
	K(i,j)=1.
	else
	K(i,j)=0.
	endif
	endif

	  if (i.eq.52) then
	    K(i,j)=1.
	  endif

	ENDDO  ! fin boucle sur nnn
	ENDDO  ! fin boucle sur NETO

	ENDIF

c  5.1  	VERIFICATION DU FRACTIONNEMENT PAR DEFAUT  
c ---------------------------------------------------
	write(8,*) ' '
	write(8,*) 'VERIFICATION FRACTIONNEMENT'
	write(8,*) '==========================='

	DO j=1,NETO
	 write(8,*) 'panneau ', j
	 write(8,*) '============'
	 write(8,*) ' '
	 do i=1,nnn
	   write(8,*) 'operation',i,'   ',K(i,j)
	 enddo
	 write(8,*) '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
	 write(8,*) ' '
	ENDDO

	write(8,*) '*****************************************************'


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  6.0	LECTURE DES COEFF DE ACCES ET D'ATELIER      
c     	GENERATION DES COEFFICIENTS PAR DEFAUT    
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	IF (nfract.EQ.1.and.ncatelier.eq.1) then

	  open (unit=3,file='DBacces.txt', status='old',ERR=908) !juillet2003

	  read(3,*) nn
c	  write(*,*) 'nn',nn
	  read(3,*) nk
c	  write(*,*) 'nk',nk

	  if(nn.NE.NETO) then
	    write(*,*) 'Nombre des panneaux incompatible ACCES-INPUT'
		write(29,*)' '																!bug
		write(29,*)'subroutine costmain'											!bug
		write(29,*)'Nombre des panneaux incompatible ACCES-INPUT'					!bug
c	    STOP
	  endif
	  if(nk.NE.nkkkk) then
	    write(*,*) 'Nombre des opérations incompatible ACCES',
     *               '-fractionnement'
		write(29,*)' '																!bug
		write(29,*)'subroutine costmain'											!bug
		write(29,*)'Nombre des opérations incompatible ACCES',
     *			   '-fractionnement'												!bug
c	    STOP
	  endif

	  read(3,*) ((ca(i,j) ,j=1,nn),i=1,nk)
	  read(3,*) ((cat(i,j),j=1,nn),i=1,nk)

	ELSE  ! Valeur par défaut

	 do i=1,10
	 do j=1,NETO
	   ca(i,j) =1.
	   cat(i,j)=1.25
	 enddo
	 enddo

	do i=11,20
	do j=1,NETO
	ca(i,j) =1.
	cat(i,j)=0.45
	enddo
	enddo

	do i=21,31
	do j=1,NETO
	ca(i,j) =1.12
	cat(i,j)=1.32
	enddo
	enddo

	do i=21,31
	do j=1,NETO
	ca(i,j) =1.12
	cat(i,j)=1.32
	enddo
	enddo

	do i=32,34
	do j=1,NETO
	ca(i,j) =1.12
	cat(i,j)=1.32
	enddo
	enddo

	do i=35,45
	do j=1,NETO
	ca(i,j) =1.22
	cat(i,j)=1.32
	enddo
	enddo

	 do i=46,51
	 do j=1,NETO
	    ca(i,j) =1.27
	    cat(i,j)=2.4
	 enddo
	 enddo

	 do j=1,NETO
	   ca(52,j)=1.
	   cat(52,j)=1.
	 enddo

	ENDIF

c  6.1  	VERIFICATION des coef. D'ACCES ET D'ATELIER  
c ---------------------------------------------------
	write(8,*) ' '
	write(8,*) 'VERIFICATION ACCES ET ATELIER'
	write(8,*) '==========================='
	
	do i=1,NETO
	  write(8,*) 'Panneau ',i
	  write(8,*) '==========='
	  write(8,*) ' '
	  do j=1,nnn
	    write(8,*) 'operation',j,'   ',ca(j,i),'    ',cat(j,i)
	  enddo
	  write(8,*) '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
	  write(8,*) ' '
	enddo 

	write(8,*) '*****************************************************'


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 7.0 	LECTURE DES PARAMETRES DES PANNEAUX 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     vlenght = longueur des panneaux
c     nopan   = numéro du panneau (à supprimer éventuellement)
c     vlarg   = largeur des panneaux      
c     vepaiss = épaisseur des tôles
c     vhac   = hauteur de l'âme des cadres
c     veac   = épaisseur de l'âme des cadres
c     vlsc   = largeur des semelles des cadres
c     vesc   = épaisseur des semelles des cadres
c     deltac = écartement entre cadres
c     vhar   = hauteur de l'âme des raidisseurs
c     vear   = épaisseur de l'âme des raidisseurs
c     vlsr   = largeur des semelles des raidisseurs
c     vesr   = épaisseur des semelles des raidisseurs
c     deltar = écartement entre raidisseurs
c     ENTR

      write(8,*)'VERIFICATION DE LA LECTURE DES PARAMETRES DES PANNEAUX'
	write(8,*) '-----------------------------------------------------'


!!!avril2003	open (unit=10, file='Alstom.txt', status='old')
!!!avril2003	read(10,*) vlength

      write(8,*) 'LONGUEUR DES PANNEAUX =',vlength
	write(8,*) ' '

c Imprimer sur le fichier "8" chaque paramètre de chaque panneau ??????

!!!avril2003  do i=1,NETO
!!!avril2003  read(10,*) nopan  ,vlarg(i),vepaiss(i)
!!!avril2003	read(10,*) vhac(i),veac(i),vlsc(i),vesc(i),deltac(i)
!!!avril2003	read(10,*) vhar(i),vear(i),vlsr(i),vesr(i),deltar(i),ENTR(i)
cc	vv1(i)=vepaiss(i)
cc	vv2(i)=vhac(i)
cc	vv3(i)=veac(i)
cc	vv4(i)=vlsc(i)
cc	vv5(i)=deltac(i)
cc	vv6(i)=vhar(i)
cc	vv7(i)=vear(i)
cc	vv8(i)=vlsr(i)
cc	vv9(i)=deltar(i)
!!!avril2003	enddo
c	write(*,*) (deltac(i),i=1,NETO)



cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 8.0 	LECTURE DES ID DES SOUDURES;          
c     	LECTURE DES GORGES DES SOUDURES     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

	open (unit=44,file='DBsoudures.txt', status='old')

	write(8,*) ' '
	write(8,*) 'VERIFICATION ID SOUDURES'
	write(8,*) '========================'

	read(44,*) nrpanneaux
	read(44,*) nropid

	if(nnnnn.NE.nrpanneaux) then
	  write(*,*) 'Le Nombre de panneaux est incompatible:',
     *  'FRACTIONNEMENT-SOUDURES'
	  write(29,*)' '																!bug
	  write(29,*)'subroutine costmain'												!bug
	  write(29,*) 'Le Nombre de panneaux est incompatible:',						!bug
     *  'FRACTIONNEMENT-SOUDURES'														!bug
	  STOP
	endif

	if(NETO.NE.nrpanneaux) then
	  write(*,*)'Le Nombre de panneaux est incompatible:SOUDURE-INPUT'
	  write(29,*)' '																!bug
	  write(29,*)'subroutine costmain'												!bug
	 write(29,*)'Le Nombre de panneaux est incompatible:SOUDURE-INPUT'				!bug
	  STOP
	endif

c	write(8,*) 
c	write(8,*) 'Nombre panneaux =',nrpanneaux
c	write(8,*) 'Nombre operations qui necessitent ID',nropid

	read(44,*) ((idsoudures(i,j),j=1,nrpanneaux),i=1,nropid)		
	read(44,*) ((vgorsoud(i,j),j=1,nrpanneaux),i=1,nropid)
	
c	DO j=1,nrpanneaux
c	  write(8,*) ' '
c	  write(8,*) 'PANNEAU',j
c	  write(8,*) ' '
c	  write(8,*) 'ID SOUDURES'
c	  write(8,*) 'OP 21',idsoudures(1,j)
c	  write(8,*) 'OP 22',idsoudures(2,j)
c	  write(8,*) 'OP 23',idsoudures(3,j)
c	  write(8,*) 'OP 36',idsoudures(4,j)
c	  write(8,*) 'OP 37',idsoudures(5,j)
c	  write(8,*) 'OP 47',idsoudures(6,j)
c	  write(8,*) 'OP 48',idsoudures(7,j)
c	  write(8,*) 'OP 51',idsoudures(8,j)
c	  write(8,*) ' '
c	  write(8,*) 'Gorges soudures'
c	  write(8,*) 'OP 21',vgorsoud(1,j)
c	  write(8,*) 'OP 22',vgorsoud(2,j)
c	  write(8,*) 'OP 23',vgorsoud(3,j)
c	  write(8,*) 'OP 36',vgorsoud(4,j)
c	  write(8,*) 'OP 37',vgorsoud(5,j)
c	  write(8,*) 'OP 47',vgorsoud(6,j)
c	  write(8,*) 'OP 48',vgorsoud(7,j)
c	  write(8,*) 'OP 51',vgorsoud(8,j)
c	ENDDO

	write(8,*) '***********************'

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 9.0         CALCUL DES QUANTITES              
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     Q(j,i) = quantité relative à l'opération "j" du panneau i

      DO i=1,NETO  ! Boucle générale
      DO j=1,nnn
      	
	if (j.eq.1) then
	Q(j,i) = (vlarg(i)/deltar(i))*(vlength/deltac(i))
	endif

	if (j.eq.2.and.idpanneau(i).eq.1) then
	Q(j,i) = (vlarg(i)/deltar(i))*(vlength/deltac(i))
	endif
	if (j.eq.2.and.idpanneau(i).eq.2) then					! fevrier 2004
	Q(j,i) = 2*nci(i)*(vlength/deltac(i))					! fevrier 2004
	endif													! fevrier 2004
	if (j.eq.2.and.idpanneau(i).eq.4) then					! fevrier 2004
	Q(j,i) = (vlength/deltac(i))+(vlarg(i)/deltar(i))		! fevrier 2004
	endif													! fevrier 2004

	if (j.eq.3) then
	Q(j,i) = (0.5*vhac(i) - vhar(i))*(vlarg(i)/deltar(i))*(vlength/
     *               deltac(i))
      endif
	if (j.eq.3.and.idpanneau(i).eq.4) then					! fevrier 2004
	Q(j,i) = vlarg(i)*(vlength/deltac(i))					! fevrier 2004
      endif													! fevrier 2004

	if (j.eq.4.or.j.eq.5) then
	  Q(j,i) = 2*vlarg(i)*nci(i)*(vlength/deltac(i))
	endif

	if (j.eq.6) then
	if (idpanneau(i).eq.1.and.itpanneau(i).eq.2.and.ippanneau(i).eq.1)
     *then
	  Q(j,i) = 1*(vlength/deltac(i))
	endif
      if (idpanneau(i).eq.1.and.itpanneau(i).eq.2.and.ippanneau(i).eq.2)
     *then	
	  Q(j,i) = 0.5*(vlength/deltac(i))
	endif
	endif

	if (j.eq.7) then
        Q(j,i) = 2*(vlength/deltac(i))
	endif

	if (j.eq.8) then
        Q(j,i) = (vlarg(i)/deltar(i))*(vlength/deltac(i))
	endif

	if (j.eq.9) then
        Q(j,i) = vhac(i)*(vlength/deltac(i))
	endif

      if (j.eq.10) then
	  Q(j,i) = (0.5*vhac(i) - vhar(i))*(vlarg(i)/deltar(i))*(vlength/
     *                deltac(i))
	endif

	if (j.eq.11) then
	 if (idpanneau(i).eq.1.and.ippanneau(i).eq.1) then
	  Q(j,i) = 1*(nam + 1)*(nanp(i) + 1)
       endif
	 if (idpanneau(i).eq.1.and.ippanneau(i).eq.2) then
        Q(j,i) = 0.5*(nam + 1)*(nanp(i) + 1)
	 endif
	endif

	if (j.eq.12.or.j.eq.17) then
       if (idpanneau(i).eq.1.and.ippanneau(i).eq.1) then
	  Q(j,i) = 1*vlength
	 endif
	 if (idpanneau(i).eq.1.and.ippanneau(i).eq.2) then
	  Q(j,i) = 0.5*vlength
	 endif
	endif

	if ((j.eq.13.or.j.eq.18).and.idpanneau(i).eq.1) then
	Q(j,i) = vlarg(i)*(nam + 1)*nanp(i)
	endif
	if (j.eq.14.and.idpanneau(i).eq.1) then
	Q(j,i) = vlarg(i)*vlength
	endif

      if (j.eq.15) then
	if (idpanneau(i).eq.1.and.ippanneau(i).eq.1) then
	Q(j,i) = 2*vlarg(i)*(nam + 1)
	endif
      if (idpanneau(i).eq.1.and.ippanneau(i).eq.2) then
      Q(j,i) = 2*vlarg(i)*(nam + 1) + vlength
	endif
	endif

	if ((j.eq.16.or.j.eq.19).and.idpanneau(i).eq.1) then
	Q(j,i) = (vlarg(i)/deltar(i))*vlength
	endif

      if ((j.eq.20).and.idpanneau(i).eq.1) then
	Q(j,i) = (vlarg(i)/deltar(i))*vlength
	endif

	if (j.eq.21) then
	Q(j,i) = (vlength/deltac(i))*vlarg(i)
	endif

	if (j.eq.22.or.j.eq.37) then
	Q(j,i) = vlength
	endif

	if (j.eq.23) then
	Q(j,i) = vlarg(i)*(vlength/deltac(i))
	endif

      if (j.eq.24.or.j.eq.25.or.j.eq.44.or.j.eq.27.or.j.eq.38.or.j.eq.39
     *           .or.j.eq.41.or.j.eq.30) then
	Q(j,i) = (vlarg(i)/deltar(i))*(vlength/deltac(i))
	endif

	if (j.eq.26.or.j.eq.40.or.j.eq.45.or.j.eq.31) then
      Q(j,i) = (vlarg(i)/deltar(i))*1
	endif

      if (j.eq.28.or.j.eq.29.or.j.eq.42.or.j.eq.43) then
	Q(j,i) = nci(i)*(vlength/deltac(i))
	endif

      if (j.eq.32) then
	Q(j,i) = vlarg(i)/deltar(i)
	endif

	if (j.eq.33.or.j.eq.35) then
	Q(j,i) = 1
	endif

	if (j.eq.34) then
	Q(j,i) = (vlarg(i)/deltar(i))*vlength
	endif

	if (j.eq.36) then
	Q(j,i) = (vlength/deltac(i))*vlarg(i)
	endif

      if (j.eq.47) then
	Q(j,i) = vlength
	endif

	if (j.eq.48) then
	Q(j,i) = vlarg(i)*nam
	endif

	if (j.eq.49.or.j.eq.50) then
	Q(j,i) = (vlarg(i)/deltar(i))*nam
	endif

      if (j.eq.51) then
	Q(j,i) = vlarg(i)*(vlength/deltac(i))
	endif

c     opération 52 (+46) ?????????????????????      
      if (j.eq.53.and.(idpanneau(i).eq.1.or.idpanneau(i).eq.4)) then	! mars 2004
	Q(j,i) = vlarg(i)*(vlength/deltac(i))			! fevrier 2004
	endif											! fevrier 2004
      if (j.eq.54.and.(idpanneau(i).eq.1.or.idpanneau(i).eq.4)) then	! mars 2004
	Q(j,i) =vlength									! fevrier 2004
	endif											! fevrier 2004
      if (j.eq.55.and.(idpanneau(i).eq.1.or.idpanneau(i).eq.4)) then	! mars 2004
	Q(j,i) =(vlength/deltac(i))						! fevrier 2004
	endif											! fevrier 2004
      if (j.eq.56.and.(idpanneau(i).eq.1.or.idpanneau(i).eq.4)) then	! mars 2004
	Q(j,i) =(vlength/deltac(i))						! fevrier 2004
	endif											! fevrier 2004
      if (j.eq.57.and.(idpanneau(i).eq.1.or.idpanneau(i).eq.4)) then	! mars 2004
	Q(j,i) =(vlength/deltac(i))						! fevrier 2004
	endif											! fevrier 2004
      if (j.eq.58.and.idpanneau(i).eq.5) then			! fevrier 2004
	Q(j,i) =(vlength/deltac(i))						! fevrier 2004
	endif											! fevrier 2004
      if (j.eq.59.and.idpanneau(i).eq.5) then			! fevrier 2004
	Q(j,i) =(vlength/deltac(i))						! fevrier 2004
	endif											! fevrier 2004
      if (j.eq.60.and.K(60,i).eq.1) then				! fevrier 2004
	Q(j,i) =(vlength/deltac(i))						! fevrier 2004
	endif											! fevrier 2004
      if (j.eq.60.and.K(60,i).eq.2) then				! fevrier 2004
	Q(j,i) =(vlarg(i)/deltar(i))					! fevrier 2004
	endif											! fevrier 2004
      if (j.eq.60.and.K(60,i).eq.3) then						! fevrier 2004
	Q(j,i) =(vlarg(i)/deltar(i))+(vlength/deltac(i))		! fevrier 2004
	endif											! fevrier 2004

	ENDDO
	ENDDO


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 10.0   CALCUL DES COUTS UNITAIRES (INTERPOLATION LINEAIRE)    
c        + CALCUL DES DERIVEES   
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     CUs(j,i) = coût unitaire soudeur de l'opération "j" du panneau i
c     CUt(j,i) = coût unitaire tôlier de l'opération "j" du panneau i
c     parametres = valeur de la variable du panneau quand interpolation
c                  soudeur est nécessaire
c     parametret = valeur de la variable du panneau quand interpolation
c                  tolier est nécessaire
c     parametres1 = idem parametres mais tape à recouvrement
c     parametret1 = idem parametret mais tape à recouvrement
c     V1 et V2 = bornes dans laquelle se trouve la valeur de la variable
c     C1 et C2 = coûts unitaires correspondant à V1 et V2
c     T2(j,i) = dérivée des quantités relative à l'opération "j" du panneau i
c     il existe 9 différents T2 : T21, T22,..., T29 où le deuxième chiffre
c     correspond au ID de la variable de conception (voir II-p.60)
c     On rajoute en plus un indice 't' ou 's' suivant le fait que ce soit une
c     sensibilité relative au CUtolier ou au CUsoudeur (donc, on double le 
c     nombre de matrices T2)

c     write(8,*) 'CALCUL DES COUTS UNITAIRES (INTERPOLATION LINEAIRE)'
c	write(8,*) '---------------------------------------------------'
      
	eps = 0.01

	DO 781 i=1,NETO  ! Boucles générales
	DO 781 j=1,nnn
    
      parametres  = 0.
      parametret  = 0.
	parametres1 = 0.
	parametret1 = 0.

c   Soit 4 vecteurs spéciaux pour op. 19 et 20
c	memovars(j,1:ivsoudeur(j)) = variables(j,1:ivsoudeur(j)) 
c	memosou(j,1:ivsoudeur(j))  = soudeur(j,1:ivsoudeur(j))
c	memovart(j,1:ivtolier(j))  = variablet(j,1:ivtolier(j))
c	memotol(j,1:ivtolier(j))   = tolier(j,1:ivtolier(j))

	if (j.eq.9.or.j.eq.17.or.j.eq.18) then 
	parametres = vepaiss(i)*1000.   ! *1000 pour transformer en mm
	ID = 1
	endif

	if (j.eq.12.or.j.eq.13) then
      parametret = vepaiss(i)*1000.
	ID = 1
	endif

	if (j.eq.15) then
	parametret = vepaiss(i)*1000.
	ID = 1
	endif

	if (j.eq.16) then
	parametret = vhar(i)*1000.
	ID = 6
	endif

	if (j.eq.19.or.j.eq.20) then
c       if ((vepaiss(i)*1000.).ge.variables1(j,i).or.((vepaiss(i)*1000.).
c         *  lt.variables1(j,i).and.(vhar(i)*1000.).lt.variables(j,i))) then
	  if ((vepaiss(i)*1000.).le.variables(j,1)) then
c         !  On doit prendre soudeur comme vecteur des CU et variables
	    parametres = vepaiss(i)*1000.
	    ID = 1
	  else
	    parametres = vhar(i)*1000.
	    ID = 6
c         on doit prendre soudeur1 comme vecteur des CU et variables1
c         les 2 lignes suivantes effacent ce que contenait variables et soudeur

        memovars = variables(j,1:ivsoudeur(j))
	  memosou = soudeur(j,1:ivsoudeur(j))
        variables(j,1:ivsoudeur(j)) = variables1(j,1:ivsoudeur(j))
	  soudeur(j,1:ivsoudeur(j)) = soudeur1(j,1:ivsoudeur(j))
	  endif
	endif

	if (j.eq.25.or.j.eq.26.or.j.eq.39.or.j.eq.40) then
	if (nttapes(i).eq.1) then ! tape à recouvrement
	parametres = vhar(i)*1000.
	parametret = vhar(i)*1000.
c     on doit prendre tolier comme vecteur des CU et variablet
	ID = 6
	else                      ! tape encastrée
	parametres1 = vhar(i)*1000.
	parametret1 = vhar(i)*1000.
c     on doit prendre tolier1 comme vecteur des CU et variablet1
c     les 2 lignes suivantes effacent ce que contenait variablet et tolier

      memovart(1:ivtolier(j)) = variablet(j,1:ivtolier(j))
	memotol(1:ivtolier(j)) = tolier(j,1:ivtolier(j)) 
      variablet(j,1:ivtolier(j)) = variablet1(j,1:ivtolier(j))
	tolier(j,1:ivtolier(j)) = tolier1(j,1:ivtolier(j))
	ID = 6
	endif
	endif

	if (j.eq.30.or.j.eq.31.or.j.eq.44.or.j.eq.45) then
	parametres = vhar(i)*1000.
	parametret = vhar(i)*1000.
	ID = 6
	endif

c opérations 49 et 50 à implémenter !!!!!!!!!!!!!!!!!

      if (ivsoudeur(j).eq.1) then
	  CUs(j,i) = soudeur(j,1) 
      endif
      if (ivtolier(j).eq.1) then
	  CUt(j,i) = tolier(j,1)  
      endif


c 10.2  Interpolation soudeur :
c -----------------------------
      IF (parametres.ne.0) then

c	if (ivsoudeur(j).eq.2) then
c	   if (parametres.lt.variables(j,1)-pas*parametres) then
c	    CUs(j,i) = soudeur(j,1)
cc         CUs(j,i) = -(soudeur(j,2)-soudeur(j,1))/2*(variables(j,1)-parametres)
cc             *  /((variables(j,1)-parametres)**2+eps**2)**(.5)+(soudeur(j,1)
cc             * + soudeur(j,2))/2
c	   elseif (parametres.gt.variables(j,1)+pas*parametres) then
c	    CUs(j,i) = soudeur(j,2)
cc         CUs(j,i) = +(soudeur(j,2)-soudeur(j,1))/2*(parametres-variables
cc              *(j,1))/((parametres-variables(j,1))**2+eps**2)**(.5)+(soudeur(j,1)
cc              * + soudeur(j,2))/2
c         else           
c		 if(parametres.eq.variables(j,1)) then
c		   CUs(j,i) = (soudeur(j,1) + soudeur(j,2))/2
c	     elseif(parametres.lt.variables(j,1)) then 
c	       CUs(j,i) = soudeur(j,1)
c	     else
c	       CUs(j,i) = soudeur(j,2)
c	     endif
c		 C2 = soudeur(j,2)
c	     C1 = soudeur(j,1)
c	     V2 = parametres*(1 + pas) !variables(j,2)
c	     V1 = parametres*(1 - pas) !variables(j,1)
cc	     if (ID.eq.1) then
cc	     T21s(j,i) = (soudeur(j,2)-soudeur(j,1))/2*eps**2/((variables(
cc     *                  j,1)-parametres)**2+eps**2)**(3./2.)*1000.
cc	     endif
cc	     if (ID.eq.6) then
cc	     T26s(j,i) = (soudeur(j,2)-soudeur(j,1))/2*eps**2/((variables(
cc     *                  j,1)-parametres)**2+eps**2)**(3./2.)*1000.
cc           endif
c	     if (ID.eq.1) T21s(j,i) = 1000.*(C2-C1)/(V2-V1)
c	     if (ID.eq.6) T26s(j,i) = 1000.*(C2-C1)/(V2-V1) 
c	   endif   
c         goto 800
c	endif
		
c     Ordonner le vecteur variables ET soudeur :
      do ijk=1,ivsoudeur(j)
	  c(1,ijk) = variables(j,ijk)
	  c(2,ijk) = soudeur(j,ijk)
	enddo
	
	call ORD(2,ivsoudeur(j),c(1:2,1:ivsoudeur(j)))

	do ijk=1,ivsoudeur(j)
	  variables(j,ijk) = c(1,ijk)
	  soudeur(j,ijk) = c(2,ijk)
	enddo
	
	do ijk=1,ivsoudeur(j)
	  if (parametres.le.variables(j,1)+pas*parametres) then  !on est avant le premier point de
c         Extrapolation ???????????                            ! donnee + pas
          CUs(j,i) = soudeur(j,1)
	    IF (ID.EQ.NUMV.AND.K(j,i).gt.0) NON = 100 + J  ! dans ce cas, message d'alerte !          
		if (parametres.gt.variables(j,1)-pas*parametres) then
	      V2 = parametres + pas*parametres
	      V1 = parametres - pas*parametres
	      CALL interpol(V2,C2,ivsoudeur(j),variables,soudeur,j,nnn)
	      CALL interpol(V1,C1,ivsoudeur(j),variables,soudeur,j,nnn)
	      if (ID.eq.1) T21s(j,i) = 1000.*(C2-C1)/(V2-V1)
	      if (ID.eq.6) T26s(j,i) = 1000.*(C2-C1)/(V2-V1)
	    endif		
		goto 800
        endif
	  if (parametres.le.variables(j,ijk)+pas*parametres.and. !on est entre 2 points de donnée 
     *      parametres.lt.variables(j,ivsoudeur(j))-pas*parametres) then !(+pas pour le deuxième point)
	    C2 = soudeur(j,ijk)
	    C1 = soudeur(j,ijk-1)
	    V2 = variables(j,ijk)
	    V1 = variables(j,ijk-1)
	    CUs(j,i) = (C2 - C1)*(parametres - V1)/(V2 - V1) + C1
		if (ID.eq.1) T21s(j,i) = 1000.*(C2 - C1)/(V2 - V1)
	    if (ID.eq.6) T26s(j,i) = 1000.*(C2 - C1)/(V2 - V1)
	    if (parametres.gt.variables(j,ijk)-pas*parametres) then !on est à 1 point de donnée (au pas près)
		  V2 = parametres + pas*parametres
	      V1 = parametres - pas*parametres
	      CALL interpol(V2,C2,ivsoudeur(j),variables,soudeur,j,nnn)
	      CALL interpol(V1,C1,ivsoudeur(j),variables,soudeur,j,nnn)
	      if (ID.eq.1) T21s(j,i) = 1000.*(C2-C1)/(V2-V1)
	      if (ID.eq.6) T26s(j,i) = 1000.*(C2-C1)/(V2-V1)
	    endif 
	    goto 800
	  endif
	  if (parametres.ge.variables(j,ivsoudeur(j))-pas*parametres) then !on est après le dernier point - pas
c         Extrapolation ???????????????
          CUs(j,i) = soudeur(j,ivsoudeur(j))
	    IF (ID.EQ.NUMV.AND.K(j,i).gt.0) NON = 200 + J
	    if(parametres.lt.variables(j,ivsoudeur(j))+pas*parametres)then !on est ds l'interv. autour du dernier point
	      V2 = parametres + pas*parametres
	      V1 = parametres - pas*parametres
	      CALL interpol(V2,C2,ivsoudeur(j),variables,soudeur,j,nnn)
	      CALL interpol(V1,C1,ivsoudeur(j),variables,soudeur,j,nnn)
	      if (ID.eq.1) T21s(j,i) = 1000.*(C2-C1)/(V2-V1)
	      if (ID.eq.6) T26s(j,i) = 1000.*(C2-C1)/(V2-V1)
	    endif
          goto 800
        endif
	enddo
800   continue   
      ENDIF  ! Fin interpolation soudeur


c 10.3  Interpolation tolier :
c --------------------------------
      IF (parametret.ne.0) then

c	if (ivtolier(j).eq.2) then
c	   if (parametret.lt.variablet(j,1)-pas*parametret) then
c	    CUt(j,i) = tolier(j,1)
cc         CUt(j,i) = -(tolier(j,2)-tolier(j,1))/2*(variablet(j,1)-parametret)
cc           *  /((variablet(j,1)-parametret)**2+eps**2)**(.5)+(tolier(j,1) + 
cc           *    tolier(j,2))/2	   
c	   elseif (parametret.gt.variablet(j,1)+pas*parametret) then
c	    CUt(j,i) = tolier(j,2)
cc         CUt(j,i) = +(tolier(j,2)-tolier(j,1))/2*(parametret-variablet(j,1))
cc            * /((parametret-variablet(j,1))**2+eps**2)**(.5)+(tolier(j,1) + 
cc            *  tolier(j,2))/2	   	   
c	   else  !si on est au saut (au pas près)
c	     if (parametret.eq.variablet(j,1)) then
c             CUt(j,i) = (tolier(j,1) + tolier(j,2))/2
c	     elseif (parametret.lt.variablet(j,1)) then
c	       CUt(j,i) = tolier(j,1)
c	     else
c	       CUt(j,i) = tolier(j,2)
c	     endif
c		 C2 = tolier(j,2)
c	     C1 = tolier(j,1)
c	     V2 = parametret*(1 + 1/20.) !variablet(j,2)
c	     V1 = parametret*(1 - 1/20.) !variablet(j,1)
cc	     if (ID.eq.1) then
cc	     T21t(j,i) = (tolier(j,2)-tolier(j,1))/2*eps**2/((variablet(j,
cc     *                  1)-parametret)**2+eps**2)**(3./2.)*1000.
cc	     endif
cc	     if (ID.eq.6) then
cc	     T26t(j,i) = (tolier(j,2)-tolier(j,1))/2*eps**2/((variablet(j,
cc     *                  1)-parametret)**2+eps**2)**(3./2.)*1000.
cc           endif
c	     if (ID.eq.1) T21t(j,i) = 1000.*(C2-C1)/(V2-V1)
c	     if (ID.eq.6) T26t(j,i) = 1000.*(C2-C1)/(V2-V1)    
c	   endif
c        goto 805
c	endif

c     ordonner le vecteur variablet ET tolier :
	do ijk=1,ivtolier(j)
	  c(1,ijk) = variablet(j,ijk)
	  c(2,ijk) = tolier(j,ijk)
	enddo

	call ORD(2,ivtolier(j),c(1:2,1:ivtolier(j)))

	do ijk=1,ivtolier(j)
	  variablet(j,ijk) = c(1,ijk)
	  tolier(j,ijk) = c(2,ijk)
	enddo

	DO ijk=1,ivtolier(j)
	  if (parametret.le.variablet(j,1)+parametret*pas) then
c         Extrapolation ???????????
          CUt(j,i) = tolier(j,1)
	    IF (ID.EQ.NUMV.AND.K(j,i).gt.0) NON = 100 + J
	    if (parametret.gt.variablet(j,1)-pas*parametret) then
	      V2 = parametret + pas*parametret
	      V1 = parametret - pas*parametret
	      CALL interpol(V2,C2,ivtolier(j),variablet,tolier,j,nnn)
	      CALL interpol(V1,C1,ivtolier(j),variablet,tolier,j,nnn)
	      if (ID.eq.1) T21t(j,i) = 1000.*(C2-C1)/(V2-V1)
	      if (ID.eq.6) T26t(j,i) = 1000.*(C2-C1)/(V2-V1)
	    endif
          goto 805
        endif
	  if (parametret.le.variablet(j,ijk)+pas*parametret.and.
     *      parametret.lt.variablet(j,ivtolier(j))-pas*parametret) then
	    C2 = tolier(j,ijk)
	    C1 = tolier(j,ijk-1)
	    V2 = variablet(j,ijk)
	    V1 = variablet(j,ijk-1)
	    CUt(j,i) = (C2 - C1)*(parametret - V1)/(V2 - V1) + C1
	    if (ID.eq.1) T21t(j,i) = 1000.*(C2-C1)/(V2-V1)
	    if (ID.eq.6) T26t(j,i) = 1000.*(C2-C1)/(V2-V1)
          if (parametret.gt.variablet(j,ijk)-pas*parametret) then
		  V2 = parametret + pas*parametret
	      V1 = parametret - pas*parametret
	      CALL interpol(V2,C2,ivtolier(j),variablet,tolier,j,nnn)
	      CALL interpol(V1,C1,ivtolier(j),variablet,tolier,j,nnn)
	      if (ID.eq.1) T21t(j,i) = 1000.*(C2-C1)/(V2-V1)
	      if (ID.eq.6) T26t(j,i) = 1000.*(C2-C1)/(V2-V1)
	    endif
	    goto 805
	  endif
	  if (parametret.ge.variablet(j,ivtolier(j))-pas*parametret) then !on est après le dernier point - pas
c         Extrapolation ???????????????
          CUt(j,i) = tolier(j,ivtolier(j))
	    IF (ID.EQ.NUMV.AND.K(j,i).gt.0) NON = 200 + J
	    if (parametret.lt.variablet(j,ivtolier(j))+pas*parametret)then !on est ds l'interv. autour du dernier point
	      V2 = parametret + pas*parametret
	      V1 = parametret - pas*parametret
	      CALL interpol(V2,C2,ivtolier(j),variablet,tolier,j,nnn)
	      CALL interpol(V1,C1,ivtolier(j),variablet,tolier,j,nnn)
	      if (ID.eq.1) T21t(j,i) = 1000.*(C2-C1)/(V2-V1)
	      if (ID.eq.6) T26t(j,i) = 1000.*(C2-C1)/(V2-V1)
	    endif
          goto 805
	  endif
	ENDDO
805   continue   

      ENDIF  !  Fin interpolation tolier


c 10.4  Interpolation soudeur (cas tape encastrée) :
c ---------------------------------------------------
      IF (parametres1.ne.0) then

c     ordonner le vecteur variables1 ET soudeur :
      do ijk=1,ivsoudeur(j)
	c(1,ijk) = variables1(j,ijk)
	c(2,ijk) = soudeur(j,ijk)
	enddo

	call ORD(2,ivsoudeur(j),c(1:2,1:ivsoudeur(j)))

	do ijk=1,ivsoudeur(j)
	variables1(j,ijk) = c(1,ijk)
	soudeur(j,ijk) = c(2,ijk)
	enddo

      do ijk=1,ivsoudeur(j)
	c(1,ijk) = variables1(j,ijk)
	c(2,ijk) = soudeur(j,ijk)
	enddo

	call ORD(2,ivsoudeur(j),c(1:2,1:ivsoudeur(j)))

	do ijk=1,ivsoudeur(j)
	variables1(j,ijk) = c(1,ijk)
	soudeur(j,ijk) = c(2,ijk)
	enddo

	do ijk=1,ivsoudeur(j)
	  if (parametres1.le.variables1(j,1)+parametres1*pas) then
c         Extrapolation ???????????
          CUs(j,i) = soudeur(j,1)
	    IF (ID.EQ.NUMV.AND.K(j,i).gt.0) NON = 100 + J
	    if (parametres1.lt.variables1(j,1)-parametres1*pas) then
	      V2 = parametres1 + pas*parametres1
	      V1 = parametres1 - pas*parametres1
	      CALL interpol(V2,C2,ivsoudeur(j),variables1,soudeur,j,nnn)
	      CALL interpol(V1,C1,ivsoudeur(j),variables1,soudeur,j,nnn)
	      if (ID.eq.1) T21s(j,i) = 1000.*(C2-C1)/(V2-V1)
	      if (ID.eq.6) T26s(j,i) = 1000.*(C2-C1)/(V2-V1)
	    endif
          goto 810
        endif
	  if(parametres1.le.variables1(j,ijk)+parametres1*pas.and.
     *    parametres1.lt.variables1(j,ivsoudeur(j))-parametres1*pas)then
	    C2 = soudeur(j,ijk)
	    C1 = soudeur(j,ijk-1)
	    V2 = variables1(j,ijk)
	    V1 = variables1(j,ijk-1)
	    CUs(j,i) = (C2 - C1)*(parametres1 - V1)/(V2 - V1)
	    if (ID.eq.1) T21s(j,i) = 1000.*(C2-C1)/(V2-V1)
	    if (ID.eq.6) T26s(j,i) = 1000.*(C2-C1)/(V2-V1)
	    if (parametres1.gt.variables1(j,ijk)-parametres1*pas) then
	      V2 = parametres1 + pas*parametres1
	      V1 = parametres1 - pas*parametres1
	      CALL interpol(V2,C2,ivsoudeur(j),variables1,soudeur,j,nnn)
	      CALL interpol(V1,C1,ivsoudeur(j),variables1,soudeur,j,nnn)
	      if (ID.eq.1) T21s(j,i) = 1000.*(C2-C1)/(V2-V1)
	      if (ID.eq.6) T26s(j,i) = 1000.*(C2-C1)/(V2-V1)
	    endif
	    goto 810
	  endif
	  if (parametres1.ge.variables1(j,ivsoudeur(j))-parametres1*pas)
     *                                                              then
c         Extrapolation ???????????????
          CUs(j,i) = soudeur(j,ivsoudeur(j))
	    IF (ID.EQ.NUMV.AND.K(j,i).gt.0) NON = 200 + J
	    if (parametres1.lt.variables1(j,ivsoudeur(j))-parametres1*pas)
     *                                                              then
	      V2 = parametres1 + pas*parametres1
	      V1 = parametres1 - pas*parametres1
	      CALL interpol(V2,C2,ivsoudeur(j),variables1,soudeur,j,nnn)
	      CALL interpol(V1,C1,ivsoudeur(j),variables1,soudeur,j,nnn)
	      if (ID.eq.1) T21s(j,i) = 1000.*(C2-C1)/(V2-V1)
	      if (ID.eq.6) T26s(j,i) = 1000.*(C2-C1)/(V2-V1)
          endif
          goto 810
        endif
	enddo
810   continue   
      ENDIF  !Fin interpolation soudeur (cas tape encastrée)


c 10.5 Interpolation tolier (cas tape encastrée) :
c--------------------------------------------------
      IF (parametret1.ne.0) then

c      Ordonner le vecteur variablet1 ET tolier :
	 do ijk=1,ivtolier(j)
	   c(1,ijk) = variablet1(j,ijk)
	   c(2,ijk) = tolier(j,ijk)
	 enddo

	 call ORD(2,ivtolier(j),c(1:2,1:ivtolier(j)))

	 do ijk=1,ivtolier(j)
	   variablet1(j,ijk) = c(1,ijk)
	   tolier(j,ijk) = c(2,ijk)
	 enddo

	 do ijk=1,ivtolier(j)
	   if (parametret1.le.variablet1(j,1)+parametret1*pas) then
c          Extrapolation ???????????
           CUt(j,i) = tolier(j,1)
	     IF (ID.EQ.NUMV.AND.K(j,i).gt.0) NON = 100 + J
	     if (parametret1.gt.variablet1(j,1)-parametret1*pas) then
	       V2 = parametret1 + pas*parametret1
	       V1 = parametret1 - pas*parametret1
	       CALL interpol(V2,C2,ivtolier(j),variablet1,tolier,j,nnn)
	       CALL interpol(V1,C1,ivtolier(j),variablet1,tolier,j,nnn)
	       if (ID.eq.1) T21t(j,i) = 1000.*(C2-C1)/(V2-V1)
	       if (ID.eq.6) T26t(j,i) = 1000.*(C2-C1)/(V2-V1)
	     endif
           goto 815
         endif
	   if(parametret1.le.variablet1(j,ijk)+parametret1*pas.and.
     *    parametret1.lt.variablet1(j,ivtolier(j))-parametret1*pas) then
	     C2 = tolier(j,ijk)
	     C1 = tolier(j,ijk-1)
	     V2 = variablet1(j,ijk)
	     V1 = variablet1(j,ijk-1)
	     CUt(j,i) = (C2 - C1)*(parametret1 - V1)/(V2 - V1) + C1	
		 if (ID.eq.1) T21t(j,i) = 1000.*(C2-C1)/(V2-V1)
	     if (ID.eq.6) T26t(j,i) = 1000.*(C2-C1)/(V2-V1) 
		 if (parametret1.gt.variablet1(j,ijk)-parametret1*pas) then
		   V2 = parametret1 + pas*parametret1
	       V1 = parametret1 - pas*parametret1
	       CALL interpol(V2,C2,ivtolier(j),variablet1,tolier,j,nnn)
	       CALL interpol(V1,C1,ivtolier(j),variablet1,tolier,j,nnn)
	       if (ID.eq.1) T21t(j,i) = 1000.*(C2-C1)/(V2-V1)
	       if (ID.eq.6) T26t(j,i) = 1000.*(C2-C1)/(V2-V1)	 	
	     endif
	     goto 815
	   endif
	   if (parametret1.ge.variablet1(j,ivtolier(j))-parametret1*pas)
     *                                                              then
c          Extrapolation ???????????????
           CUt(j,i) = tolier(j,ivtolier(j))
	     IF (ID.EQ.NUMV.AND.K(j,i).gt.0) NON = 200 + J
	     if (parametret1.lt.variablet1(j,ivtolier(j))+parametret1*pas)
     *                                                              then
	       V2 = parametret1 + pas*parametret1
	       V1 = parametret1 - pas*parametret1
	       CALL interpol(V2,C2,ivtolier(j),variablet1,tolier,j,nnn)
	       CALL interpol(V1,C1,ivtolier(j),variablet1,tolier,j,nnn)
	       if (ID.eq.1) T21t(j,i) = 1000.*(C2-C1)/(V2-V1)
	       if (ID.eq.6) T26t(j,i) = 1000.*(C2-C1)/(V2-V1)
           endif
           goto 815
         endif
	 enddo
815    continue   
      ENDIF  !Fin interpolation tolier (cas tape encastrée)


c     Interpolation sur les opérations 49 et 50
c     -----------------------------------------
c      
c	IF (j.eq.49) then
c    
cc     Tolier
c	do ijk=1,ivtolier(j)
c	  c(1,ijk) = variablet(j,ijk)
c	  c(2,ijk) = variablet1(j,ijk)
c	  if (npabouts(i).eq.1) c(3,ijk) = tolier(j,ijk)   !cad soudure à plat
c       if (npabouts(i).eq.2) c(3,ijk) = tolier1(j,ijk)  !cad soudure verticale
c	  if (npabouts(i).eq.3) c(3,ijk) = tolier2(j,ijk)  !cad soudure horizontale
c	enddo
c	call ORD(3,ivtolier(j),c(1:3,1:ivtolier(j)))
c	call ANNULD(num,35) !!!!!!!!à changer
c	call ORD2(3,ivtolier(j),c(1:3,1:ivtolier(j)),num)
c	do ijk=1,ivtolier(j)
c       variablet(j,ijk) = c(1,ijk)
c	  variablet1(j,ijk) = c(2,ijk)
c	  if (npabouts(i).eq.1) tolier(j,ijk) = c(3,ijk)   !cad soudure à plat
c       if (npabouts(i).eq.2) tolier1(j,ijk) = c(3,ijk)  !cad soudure verticale
c	  if (npabouts(i).eq.3) tolier2(j,ijk) = c(3,ijk)  !cad soudure horizontale
c	enddo
c	
c	if (vhar(i)*1000.le.variablet(j,1)) then !
c	  call interpol2(num(1),npabouts(i),vear(i),CUt(j,i),
c   *                 variablet1(j,num(1)),tolier(j,num(1)),
c    *                 tolier1(j,num(1)),tolier2(j,num(1)),j,nnn)
c	  goto 816
c	endif
c
c     k1=2
c	do while (vhar(i)*1000.gt.variablet(j,k1))
c	  if (kk.eq.ivtolier(j)) then
c	    kk=1
c		do while (num(k1-kk).gt.1)
c	      kk=kk+1
c	    enddo
c       call interpol2(kk,npabouts(i),vear(i),CUt(j,i),
c   *                   variablet1(j,k1-kk+1),tolier(j,k1-kk+1),
c    *                   tolier1(j,k1-kk+1),tolier2(j,k1-kk+1),j,nnn) 
c	    goto 816
c	  endif
c       k1=k1+1
c	enddo
c
c	V1 = variablet(j,k1-1)
c	V2 = variablet(j,k1)
c	kk=1
c	do while (k1-1-kk.ge.1.and.num(k1-1-kk).gt.1)
c	  kk=kk+1
c	enddo
c	call interpol2(kk,npabouts(i),vear(i),C1,
c   *               variablet1(j,k1-kk),tolier(j,k1-kk),
c    *               tolier1(j,k1-kk),tolier2(j,k1-kk),j,nnn)
c	call interpol2(num(k1),npabouts(i),vear(i),C2,
c   *               variablet1(j,k1),tolier(j,k1),
c    *               tolier1(j,k1),tolier2(j,k1),j,nnn)
c	CUt(j,i)= (C2 - C1)*(vhar(i)*1000 - V1)/(V2 - V1) + C1
cc     Fin tolier
c
cc     Soudeur
c816 	ENDIF
c
c
c     IF (j.eq.50) then
c
cc   Tolier
c    if (npabouts(i).eq.1) CUt(j,i) = tolier(j,1)   !cad soudure à plat
c     if (npabouts(i).eq.2) CUt(j,i) = tolier1(j,1)  !cad soudure verticale
c	if (npabouts(i).eq.3) CUt(j,i) = tolier2(j,1)  !cad soudure horizontale
cc   Fin tolier

cc    Soudeur
c     do ijk=1,ivsoudeur(j)
c	  c(1,ijk) = variables(j,ijk)
c	  c(2,ijk) = variables1(j,ijk)
c	  if (npabouts(i).eq.1) c(3,ijk) = soudeur(j,ijk)   !cad soudure à plat
c       if (npabouts(i).eq.2) c(3,ijk) = soudeur1(j,ijk)  !cad soudure verticale
c	  if (npabouts(i).eq.3) c(3,ijk) = soudeur2(j,ijk)  !cad soudure horizontale
c	enddo
c	call ORD(3,ivsoudeur(j),c(1:3,1:ivsoudeur(j)))
c	call ANNULD(num,35) !!!!!!!!à changer
c	call ORD2(3,ivsoudeur(j),c(1:3,1:ivsoudeur(j)),num)
c	do ijk=1,ivsoudeur(j)
c       variables(j,ijk) = c(1,ijk)
c	  variables1(j,ijk) = c(2,ijk)
c	  if (npabouts(i).eq.1) soudeur(j,ijk) = c(3,ijk)   !cad soudure à plat
c       if (npabouts(i).eq.2) soudeur1(j,ijk) = c(3,ijk)  !cad soudure verticale
c	  if (npabouts(i).eq.3) soudeur2(j,ijk) = c(3,ijk)  !cad soudure horizontale
c	enddo
c	
c	if (vhar(i)*1000.le.variables(j,1)) then !
c	  call interpol2(num(1),npabouts(i),vlsr(i),CUs(j,i),
c   *                 variables1(j,num(1)),soudeur(j,num(1)),
c    *                 soudeur1(j,num(1)),soudeur2(j,num(1)),j,nnn)
c	  goto 817
c	endif
c
c     k1=2
c	do while (vhar(i)*1000.gt.variables(j,k1))
c	  if (kk.eq.ivsoudeur(j)) then
c	    kk=1
c		do while (num(k1-kk).gt.1)
c	      kk=kk+1
c	    enddo
c        call interpol2(kk,npabouts(i),vlsr(i),CUs(j,i),
c     *                   variables1(j,k1-kk+1),soudeur(j,k1-kk+1),
c    *                   soudeur1(j,k1-kk+1),soudeur2(j,k1-kk+1),j,nnn) 
c	    goto 817
c	  endif
c        k1=k1+1
c	enddo
c
c	V1 = variables(j,k1-1)
c	V2 = variables(j,k1)
c	kk=1
c	do while (k1-1-kk.ge.1.and.num(k1-1-kk).gt.1)
c	  kk=kk+1
c	enddo
c	call interpol2(kk,npabouts(i),vlsr(i),C1,
c     *              variables1(j,k1-kk:k1-1),soudeur(j,k1-kk:k1-1),
c     *              soudeur1(j,k1-kk:k1-1),soudeur2(j,k1-kk:k1-1),j,nnn)
c	call interpol2(num(k1),npabouts(i),vlsr(i),C2,
c     *    variables1(j,k1:k1+num(k1)-1),soudeur(j,k1:k1+num(k1)-1),
c     *    soudeur1(j,k1:k1+num(k1)-1),soudeur2(j,k1:k1+num(k1)-1),j,nnn)
c	CUs(j,i)= (C2 - C1)*(vhar(i)*1000 - V1)/(V2 - V1) + C1
cc     Fin soudeur
c      ENDIF
c      
c817   CONTINUE

c 10.6  INTERPOLATION SUR LES COUTS DES SOUDURES (ID) 
c -------------------------------------------------------------
      IF (j.eq.21.or.j.eq.22.or.j.eq.23.or.j.eq.36.or.j.eq.37.
     *            or.j.eq.47.or.j.eq.48.or.j.eq.51)  THEN

	if(j.eq.21) iijj=1
	if(j.eq.22) iijj=2
	if(j.eq.23) iijj=3
	if(j.eq.36) iijj=4
	if(j.eq.37) iijj=5
	if(j.eq.47) iijj=6
	if(j.eq.48) iijj=7
	if(j.eq.51) iijj=8

      coeff = 1000. !*1000 ou pas ???????

	if(vgorsoud(iijj,i).ne.0.) then
	  parat=vgorsoud(iijj,i)
	  paras=vgorsoud(iijj,i)
	  V2=vgorsoud(iijj,i)*(1+pas)
	  V1=vgorsoud(iijj,i)*(1-pas)
c	  if(idsoudures(iijj,i).le.13) then
c	  T21t(i,j)=0.
c	  T21s(i,j)=0.
c	  endif
	else
	  if(idsoudures(iijj,i).le.13) then
	    parat=0.5+abs(MIN(vepaiss(i),veac(i))*0.35 *coeff)
	    paras=0.5+abs(MIN(vepaiss(i),veac(i))*0.35 *coeff)
	    V2=0.5+abs(MIN(vepaiss(i)*(1+pas),veac(i)*(1+pas))*0.35*coeff)
	    V1=0.5+abs(MIN(vepaiss(i)*(1-pas),veac(i)*(1-pas))*0.35*coeff)
	  else
	    parat=vepaiss(i) *coeff
	    paras=vepaiss(i) *coeff
	    V2=vepaiss(i)*(1+pas) *coeff
	    V1=vepaiss(i)*(1-pas) *coeff
	  endif
	endif

	idso=idsoudures(iijj,i)

c ----------------------------------------------
	DO 782 kst=1,noid  

	IF(iid(kst).eq.idso) then

	if(vst(kst,1).eq.0.and.vst(kst,2).eq.0.and.vst(kst,3).eq.0)then !càd si 1 seule donnée 
	  cut(j,i)=ts(kst,1)                                            !(pas d'interpolation)

      else
c	  V2 = parat + pas*parat
c	  V1 = parat - pas*parat
	  CALL interpol(V2,C2,is(kst,irid(kst)),vst,ts,kst,noid)
	  CALL interpol(V1,C1,is(kst,irid(kst)),vst,ts,kst,noid)
        CALL interpol(parat,cut(j,i),is(kst,irid(kst)),vst,ts,kst,noid)
c        CUt(j,i) = (C2-C1)*(parat-V1)/(V2-V1) + C1
        if(vgorsoud(iijj,i).EQ.0) then
	    
          if (idsoudures(iijj,i).le.13) then
	      coeff2 = 0.35*coeff
	    else
	      coeff2 = coeff
	    endif
c          dV2 = (1+pas)*coeff2
c	    dV1 = (1-pas)*coeff2
c	    CALL interpol(V2+1E-5,dC22,is(kst,irid(kst)),vst,ts,kst,noid)
c	    CALL interpol(V2-1E-5,dC21,is(kst,irid(kst)),vst,ts,kst,noid)
c	    dC2 = (dC22 - dC21)/2.E-5
c	    CALL interpol(V1+1E-5,dC12,is(kst,irid(kst)),vst,ts,kst,noid)
c	    CALL interpol(V1-1E-5,dC11,is(kst,irid(kst)),vst,ts,kst,noid)
c		dC1 = (dC12 - dC11)/2.E-5 
		  
c          dT2 = (((dC2-dC1)*(parat-V1)+(C2-C1)*(coeff2-dV1))*(V2-V1) -
c     *	       (C2-C1)*(parat-V1)*(dV2-dV1)) /
c     *	       (V2-V1)**2 + dC1
c	    dT2 = (C2-C1)/(V2-V1)*coeff2
	    if(idsoudures(iijj,i).le.13) then    
	      if (vepaiss(i).LT.veac(i)) then
		    T21t(j,i) = (C2-C1)/(2*vepaiss(i)*pas)  !dT2 !(C2 - C1)/(V2 - V1)*0.35 *coeff
	      else
	        T23t(j,i) = (C2-C1)/(2*veac(i)*pas)  !dT2 !(C2 - C1)/(V2 - V1)*0.35 *coeff
            endif
	    else
            T21t(j,i) = (C2-C1)/(2*vepaiss(i)*pas)    !dT2 !(C2 - C1)/(V2 - V1) *coeff
	    endif
	  endif

c	do kkjj=1,is(kst,irid(kst))-1
c	 if((vst(kst,kkjj)-parat*pas.le.parat.or.
c    *         vst(kst,kkjj)+parat*pas.ge.parat).and.kkjj.ne.1) then !on est dans un interv.de
c	   cut(j,i)=ts(kst,kkjj)                                       !pas autour d'une donnée
c        if(vgorsoud(iijj,i).EQ.0) then
c	     V2 = parat + pas*parat
c	     V1 = parat - pas*parat
c	     CALL interpol(V2,C2,is(kst,irid(kst)),vst,ts,j,nnn)
c	     CALL interpol(V1,C1,is(kst,irid(kst)),vst,ts,j,nnn)
c	     if(idsoudures(iijj,i).le.13) then
c	       if (vepaiss(i).LT.veac(i)) then
c		     T21t(j,i) = (C2 - C1)/(V2 - V1)*0.35 *coeff
c	       else
c	         T23t(j,i) = (C2 - C1)/(V2 - V1)*0.35 *coeff
c            endif
c	     else
c            T21t(j,i) = (C2 - C1)/(V2 - V1) *coeff
c	     endif
c	   endif
c	 endif
c	 if(vst(kst,kkjj)+parat*pas.lt.parat.and.     !on est entre deux points de donnée
c    *    vst(kst,kkjj+1)-parat*pas.gt.parat) then  !en tenant compte du pas
c	   C2 = ts(kst,kkjj+1)
c	   C1 = ts(kst,kkjj)
c        V2 = vst(kst,kkjj+1)
c	   V1 = vst(kst,kkjj)
c	   cut(j,i) = C1 + (C2 - C1)*(paras - V1)/(V2 - V1)
c        if(vgorsoud(iijj,i).EQ.0) then
c	     if(idsoudures(iijj,i).le.13) then
c	       if (vepaiss(i).LT.veac(i)) then
c		     T21t(j,i) = (C2 - C1)/(V2 - V1)*0.35 *coeff
c	       else
c	         T23t(j,i) = (C2 - C1)/(V2 - V1)*0.35 *coeff
c            endif
c	     else
c             T21t(j,i) = (C2 - C1)/(V2 - V1) *coeff
c	     endif
c	   endif
c	 endif
c	enddo
c
c	if(vst(kst,1)+parat*pas.GE.parat) then       !on est avant le premier point de donnee + pas
c	  cut(j,i)=ts(kst,1)
c       if (vst(kst,1)-parat*pas.le.parat) then
c	    if(vgorsoud(iijj,i).EQ.0) then
c	      V2 = parat + pas*parat
c	      V1 = parat - pas*parat
c	      CALL interpol(V2,C2,is(kst,irid(kst)),vst,ts,j,nnn)
c	      CALL interpol(V1,C1,is(kst,irid(kst)),vst,ts,j,nnn)
c	      if(idsoudures(iijj,i).le.13) then
c	        if (vepaiss(i).LT.veac(i)) then
c		      T21t(j,i) = (C2 - C1)/(V2 - V1)*0.35 *coeff
c	        else
c	          T23t(j,i) = (C2 - C1)/(V2 - V1)*0.35 *coeff
c             endif
c	      else
c             T21t(j,i) = (C2 - C1)/(V2 - V1) *coeff
c	      endif
c	    endif
c	  endif
c	endif
c	if(vst(kst,is(kst,irid(kst)))-parat*pas.LE.parat) then !on est après le dernier point de donne - pas
c	  cut(j,i)=ts(kst,is(kst,irid(kst)))
c	  if (vst(kst,is(kst,irid(kst)))+parat*pas.ge.parat) then
c	    if(vgorsoud(iijj,i).EQ.0) then
c	      V2 = parat + pas*parat
c	      V1 = parat - pas*parat
c	      CALL interpol(V2,C2,is(kst,irid(kst)),vst,ts,j,nnn)
c	      CALL interpol(V1,C1,is(kst,irid(kst)),vst,ts,j,nnn)
c	      if(idsoudures(iijj,i).le.13) then
c	        if (vepaiss(i).LT.veac(i)) then
c		      T21t(j,i) = (C2 - C1)/(V2 - V1)*0.35 *coeff
c	        else
c	          T23t(j,i) = (C2 - C1)/(V2 - V1)*0.35 *coeff
c             endif
c	      else
c             T21t(j,i) = (C2 - C1)/(V2 - V1) *coeff
c	      endif
c	    endif
c	  endif
c	endif
	
	endif
c     -----------------------
      if(vss(kst,1).eq.0.and.vss(kst,2).eq.0.and.vss(kst,3).eq.0)then
	  cus(j,i)=ss(kst,1)
	
      else

c        V2 = paras + pas*paras
c        V1 = paras - pas*paras
	  CALL interpol(V2,C2,is(kst,irid(kst)),vss,ss,kst,noid)
	  CALL interpol(V1,C1,is(kst,irid(kst)),vss,ss,kst,noid)
	  CALL interpol(paras,cus(j,i),is(kst,irid(kst)),vss,ss,kst,noid)
c        CUs(j,i) = (C2-C1)*(paras-V1)/(V2-V1) + C1
        if(vgorsoud(iijj,i).EQ.0) then
  	    
	    if (idsoudures(iijj,i).le.13) then
	      coeff2 = 0.35*coeff
	    else
	      coeff2= coeff
	    endif
c	    dV2 = (1+pas)*coeff2
c	    dV1 = (1-pas)*coeff2
c	    CALL interpol(V2+1E-5,dC22,is(kst,irid(kst)),vss,ss,kst,noid)
c	    CALL interpol(V2-1E-5,dC21,is(kst,irid(kst)),vss,ss,kst,noid)
c	    dC2 = (dC22 - dC21)/2.E-5
c	    CALL interpol(V1+1E-5,dC12,is(kst,irid(kst)),vss,ss,kst,noid)
c	    CALL interpol(V1-1E-5,dC11,is(kst,irid(kst)),vss,ss,kst,noid)
c		dC1 = (dC12 - dC11)/2.E-5 
		  
c          dT2 = (((dC2-dC1)*(paras-V1)+(C2-C1)*(coeff2-dV1))*(V2-V1) -
c     *	       (C2-C1)*(paras-V1)*(dV2-dV1)) /
c     *	       (V2-V1)**2 + dC1
c	    dT2 = (C2-C1)/(V2-V1)*coeff2
ccc	    dT2 = (C2-C1)/(2*v)
	    if(idsoudures(iijj,i).le.13) then
	      if (vepaiss(i).LT.veac(i)) then
	        T21s(j,i) = (C2-C1)/(2*vepaiss(i)*pas) !dT2 !(C2 - C1)/(V2 - V1)*0.35 *coeff
            else
	        T23s(j,i) = (C2-C1)/(2*veac(i)*pas) !dT2 !(C2 - C1)/(V2 - V1)*0.35 *coeff
            endif
	    else
            T21s(j,i) = (C2-C1)/(2*vepaiss(i)*pas)  !dT2 !(C2 - C1)/(V2 - V1) *coeff
	    endif
	  endif

c	do kkjj=1,is(kst,irid(kst))-1
c	  if(vss(kst,kkjj)-paras*pas.le.paras.and.
c    *      vss(kst,kkjj)+paras*pas.ge.paras.and.kkjj.ne.1.and.
c   *      vss(kst,kkjj-1).lt.paras) then
c	    cus(j,i)=ss(kst,kkjj)
c	    if(vgorsoud(iijj,i).EQ.0) then
c	      V2 = paras + pas*paras
c	      V1 = paras - pas*paras
c	      CALL interpol(V2,C2,is(kst,irid(kst)),vss,ss,j,nnn)
c	      CALL interpol(V1,C1,is(kst,irid(kst)),vss,ss,j,nnn)
c	      if(idsoudures(iijj,i).le.13) then
c	        if (vepaiss(i).LT.veac(i)) then
c		      T21s(j,i) = (C2 - C1)/(V2 - V1)*0.35 *coeff
c	        else
c	          T23s(j,i) = (C2 - C1)/(V2 - V1)*0.35 *coeff
c             endif
c	      else
c             T21s(j,i) = (C2 - C1)/(V2 - V1) *coeff
c	      endif
c	    endif
c	  endif
c	  if(vss(kst,kkjj)+paras*pas.lt.paras.and.     !on est entre deux points de donnee
c    *     vss(kst,kkjj+1)-paras*pas.gt.paras) then  !en tenant compte du pas
c	    C2 = ss(kst,kkjj+1)
c	    C1 = ss(kst,kkjj)
c         V2 = vss(kst,kkjj+1)
c	    V1 = vss(kst,kkjj)
c	    cus(j,i) = C1 + (C2 - C1)*(paras - V1)/(V2 - V1)
c         if(vgorsoud(iijj,i).EQ.0) then
c	      if(idsoudures(iijj,i).le.13) then
c	        if (vepaiss(i).LT.veac(i)) then
c		      T21s(j,i) = (C2 - C1)/(V2 - V1)*0.35 *coeff
c	        else
c	          T23s(j,i) = (C2 - C1)/(V2 - V1)*0.35 *coeff
c             endif
c	      else
c             T21s(j,i) = (C2 - C1)/(V2 - V1) *coeff
c	      endif
c	    endif
c	  endif
c	enddo
c
c	if(vss(kst,1)+paras*pas.GE.paras) then  !on est avant le premier point de donne + pas
c	  cus(j,i)=ss(kst,1)
c	  if (vss(kst,1)-parat*pas.le.paras) then
c         if(vgorsoud(iijj,i).EQ.0) then
c	      V2 = paras + pas*paras
c	      V1 = paras - pas*paras
c	      CALL interpol(V2,C2,is(kst,irid(kst)),vss,ss,j,nnn)
c	      CALL interpol(V1,C1,is(kst,irid(kst)),vss,ss,j,nnn)
c	      if(idsoudures(iijj,i).le.13) then
c	        if (vepaiss(i).LT.veac(i)) then
c		      T21s(j,i) = (C2 - C1)/(V2 - V1)*0.35 *coeff
c	        else
c	          T23s(j,i) = (C2 - C1)/(V2 - V1)*0.35 *coeff
c             endif
c	      else
c             T21s(j,i) = (C2 - C1)/(V2 - V1) *coeff
c	      endif
c	    endif
c	  endif
c	endif
c	if(vss(kst,is(kst,irid(kst)))-parat*pas.LE.paras) then !on est après le premier point de donnee - pas
c	  cus(j,i)=ss(kst,is(kst,irid(kst)))
c       if (vss(kst,is(kst,irid(kst)))+parat*pas.ge.paras) then
c	    IND = is(kst,irid(kst))
c	    if(vgorsoud(iijj,i).EQ.0) then
c	      V2 = paras + pas*paras
c	      V1 = paras - pas*paras
c	      CALL interpol(V2,C2,is(kst,irid(kst)),vss,ss,j,nnn)
c	      CALL interpol(V1,C1,is(kst,irid(kst)),vss,ss,j,nnn)
c	      if(idsoudures(iijj,i).le.13) then
c	        if (vepaiss(i).LT.veac(i)) then
c		      T21s(j,i) = (C2 - C1)/(V2 - V1)*0.35 *coeff
c	        else
c	          T23s(j,i) = (C2 - C1)/(V2 - V1)*0.35 *coeff
c             endif
c	      else
c             T21s(j,i) = (C2 - C1)/(V2 - V1) *coeff
c	      endif
c	    endif
c	  endif
c	endif

	endif

	ENDIF 

 782  CONTINUE  ! DO 782 kst=1,noid

c	continue
c	write(*,*) cut(j,i),cus(j,i)
	ENDIF   !    IF (j.eq.21.or.j.eq.22.or.j.e


c ---------------
c     variables(j,1:ivsoudeur(j)) = memovars(j,1:ivsoudeur(j))
c	soudeur  (j,1:ivsoudeur(j)) = memosou (j,1:ivsoudeur(j))
c	variablet(j,1:ivtolier(j))  = memovart(j,1:ivtolier(j))
c	tolier   (j,1:ivtolier(j))  = memotol (j,1:ivtolier(j))
	
	IF (j.eq.19.or.j.eq.20) then
c       if ((vepaiss(i)*1000.).ge.variables(j,i).or.
c          ((vepaiss(i)*1000.).lt.variables(j,i).and.
C           (vhar(i)   *1000.).lt.variables1(j,i))) then
        if ((vepaiss(i)*1000.).le.memovars(1)) then
          CONTINUE
	  else
          variables(j,1:ivsoudeur(j)) = memovars(1:ivsoudeur(j))
	    soudeur(j,1:ivsoudeur(j))   = memosou(1:ivsoudeur(j))
	  endif
	ENDIF

      IF (j.eq.25.or.j.eq.26.or.j.eq.39.or.j.eq.40) then
	 if (nttapes(i).eq.1) then ! tape à recouvrement
	   continue
	 else                      ! tape encastrée
	   parametres1 = vhar(i)*1000.
	   parametret1 = vhar(i)*1000.
         variablet(j,1:ivtolier(j)) = memovart(1:ivtolier(j))
	   tolier(j,1:ivtolier(j))    = memotol(1:ivtolier(j))       
	  endif
	ENDIF


 781  CONTINUE  ! Fin boucle principale


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccc            CALCUL DES SENSIBILITES T1              ccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     T1(j,i) = dérivée des quantités relative à l'opération "j" du panneau i
c     il existe 9 différents T1 : T11, T12,..., T19 où le deuxième chiffre
c     correspond au ID de la variable de conception (voir II-p.60)

      IF (ICOUT.EQ.3) GOTO 783

      do i=1,neto
ccccccccccccccccccccccc
      do j=1,nnn
      if (j.eq.1) then
	T15(j,i) = -(vlarg(i)/deltar(i))*(vlength/deltac(i)**2)
	T19(j,i) = -(vlarg(i)/deltar(i)**2)*(vlength/deltac(i))
	endif
	if (j.eq.2.and.idpanneau(i).eq.1) then
	T15(j,i) = -(vlarg(i)/deltar(i))*(vlength/deltac(i)**2)
	T19(j,i) = -(vlarg(i)/deltar(i)**2)*(vlength/deltac(i))
	endif
      if (j.eq.2.and.idpanneau(i).eq.2) then                 ! fevrier 2004
	T15(j,i) = -2*nci(i)*vlength/deltac(i)**2              ! fevrier 2004
	endif                                                  ! fevrier 2004
      if (j.eq.2.and.idpanneau(i).eq.4) then                 ! fevrier 2004
	T15(j,i) = -vlength/deltac(i)**2                       ! fevrier 2004
	T19(j,i) = -vlarg(i)/deltar(i)**2                      ! fevrier 2004
      endif                                                  ! fevrier 2004
	if (j.eq.3) then
	T15(j,i) = -(0.5*vhac(i) - vhar(i))*(vlarg(i)/deltar(i))*(vlength/
     *deltac(i)**2)
	T19(j,i) = -(0.5*vhac(i) - vhar(i))*(vlarg(i)/deltar(i)**2)*(vleng
     *th/deltac(i))
	T12(j,i) = (0.5)*(vlarg(i)/deltar(i))*(vlength/deltac(i))
	T16(j,i) = -(vlarg(i)/deltar(i))*(vlength/deltac(i))
      endif
      if (j.eq.3.and.idpanneau(i).eq.4) then                 ! fevrier 2004
      T15(j,i) = -vlarg(i)*vlength/deltac(i)**2              ! fevrier 2004
	endif                                                  ! fevrier 2004
	if (j.eq.4.or.j.eq.5) then
	T15(j,i) = -2*vlarg(i)*nci(i)*(vlength/deltac(i)**2)
	endif
	if (j.eq.6) then
	if (idpanneau(i).eq.1.and.itpanneau(i).eq.2.and.ippanneau(i).eq.1)
     *then
	T15(j,i) = -1*(vlength/deltac(i)**2)
	endif
      if (idpanneau(i).eq.1.and.itpanneau(i).eq.2.and.ippanneau(i).eq.2)
     *then	
	T15(j,i) = -0.5*(vlength/deltac(i)**2)
	endif
	endif
	if (j.eq.7) then
      T15(j,i) = -2*(vlength/deltac(i)**2)
	endif
	if (j.eq.8) then
      T15(j,i) = -(vlarg(i)/deltar(i))*(vlength/deltac(i)**2)
	T19(j,i) = -(vlarg(i)/deltar(i)**2)*(vlength/deltac(i))
	endif
	if (j.eq.9) then
      T15(j,i) = -vhac(i)*(vlength/deltac(i)**2)
	T12(j,i) = (vlength/deltac(i))
	endif
      if (j.eq.10) then
	T15(j,i) = -(0.5*vhac(i) - vhar(i))*(vlarg(i)/deltar(i))*(vlength/
     *deltac(i)**2)
	T19(j,i) = -(0.5*vhac(i) - vhar(i))*(vlarg(i)/deltar(i)**2)*(vleng
     *th/deltac(i))
	T12(j,i) = (0.5)*(vlarg(i)/deltar(i))*(vlength/deltac(i))
	T16(j,i) = -(vlarg(i)/deltar(i))*(vlength/deltac(i))
	endif
	if ((j.eq.16.or.j.eq.19.or.j.eq.20).and.idpanneau(i).eq.1) then 
      T19(j,i) = -(vlarg(i)/deltar(i)**2)*vlength
	endif
      if (j.eq.21.or.j.eq.23) then
	T15(j,i) = -(vlength/deltac(i)**2)*vlarg(i)
	endif
	if (j.eq.24.or.j.eq.25.or.j.eq.27.or.j.eq.30.or.j.eq.38.or.j.eq.39
     *.or.j.eq.41.or.j.eq.44) then
	T15(j,i) = -(vlarg(i)/deltar(i))*(vlength/deltac(i)**2)
	T19(j,i) = -(vlarg(i)/deltar(i)**2)*(vlength/deltac(i))
	endif
	if (j.eq.26..or.j.eq.31.or.j.eq.40.or.j.eq.45) then
      T19(j,i) = -(vlarg(i)/deltar(i)**2)
	endif
      if (j.eq.28.or.j.eq.29.or.j.eq.42.or.j.eq.43) then
	T15(j,i) = -nci(i)*(vlength/deltac(i)**2)
	endif
      if (j.eq.32) then
	T19(j,i) = -vlarg(i)/deltar(i)**2
	endif
	if (j.eq.34) then
	T19(j,i) = -(vlarg(i)/deltar(i)**2)*vlength
	endif
	if (j.eq.36) then
	T15(j,i) = -(vlength/deltac(i)**2)*vlarg(i)
	endif	
	if (j.eq.49.or.j.eq.50) then
	T19(j,i) = -(vlarg(i)/deltar(i)**2)*nam
	endif
      if (j.eq.51) then
	T15(j,i) = -vlarg(i)*(vlength/deltac(i)**2)
	endif
c     opération 52 (+46) ?????????????????????      
	enddo

      if (j.eq.53.and.(idpanneau(i).eq.1.or.idpanneau(i).eq.4)) then   ! fevrier 2004
	T15(j,i) = -vlarg(i)*vlength/deltac(i)**2                        ! fevrier 2004
	endif                                                            ! fevrier 2004
	if ((j.eq.55.or.j.eq.56.or.j.eq.57).and.				         ! fevrier 2004
     *(idpanneau(i).eq.1.or.idpanneau(i).eq.4)) then                   ! fevrier 2004
	T15(j,i) = -vlength/deltac(i)**2                                 ! fevrier 2004
	endif                                                            ! fevrier 2004
	if (j.eq.59.or.j.eq.58.and.idpanneau(i).eq.5) then               ! mars 2004
	T15(j,i) = -vlength/deltac(i)**2                                 ! fevrier 2004
	endif                                                            ! fevrier 2004
	if (j.eq.60) then                                                ! fevrier 2004
        if (K(j,i).eq.1.or.K(j,i).eq.3) then                           ! fevrier 2004
	    T15(j,i) = -vlength/deltac(i)**2                             ! fevrier 2004
	  elseif (K(j,i).eq.2.or.K(j,i).eq.3) then                       ! fevrier 2004
		T19(j,i) = -vlarg(i)/deltar(i)**2                            ! fevrier 2004
	  endif                                                          ! fevrier 2004
	endif                                                            ! fevrier 2004

	enddo
  783 CONTINUE

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C
c 11.0	CALCUL DES COUTS TOLIER+SOUDEUR ET COUT TOTAL  
c ---------------------------------------------------------
c  Variables utilisées :                        
c	CUT(i,j) - matrice des couts unitaires tolier  
c	CUS(i,j) - matrice des couts unitaires soudeur  
c	i = indice operation							
c	j = indice panneaux							
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

	do i=1,10
	do j=1,NETO
	 TItol(i,j) =Q(i,j)*cut(i,j)*k(i,j)*ca(i,j)*cttpp
	 TIsoud(i,j)=Q(i,j)*cus(i,j)*k(i,j)*ca(i,j)*ctspp
	enddo
	enddo				
ccccccccccccccccccccccccc
	do i=11,20
	do j=1,NETO
	 TItol(i,j)= Q(i,j)*cut(i,j)*k(i,j)*ca(i,j)*cmn
	 TIsoud(i,j)=Q(i,j)*cus(i,j)*k(i,j)*ca(i,j)*cmn*(1.+cbn)
	enddo
	enddo				
cccccccccccccccccccccccccccc
	do i=21,45
	do j=1,NETO
	 TItol(i,j) =Q(i,j)*cut(i,j)*k(i,j)*ca(i,j)*(1+crp)
	 TIsoud(i,j)=Q(i,j)*cus(i,j)*k(i,j)*ca(i,j)*(1+cbp)
	enddo
	enddo				
cccccccccccccccccccccccccccc
	do i=46,nnn
	do j=1,NETO
	  TItol(i,j) =Q(i,j)*cut(i,j)*k(i,j)*ca(i,j)
	  TIsoud(i,j)=Q(i,j)*cus(i,j)*k(i,j)*ca(i,j)
	enddo
	enddo				

ccccccccccccccccccccccccc
	do i=1,nnn
	do j=1,NETO
	  RROtol(i,j) =TItol(i,j) *cat(i,j)
	  RROsoud(i,j)=TIsoud(i,j)*cat(i,j)
	enddo
	enddo				
ccccccccccccccccccccccccccccc

c      call annuld(PANTOLTI,   NETO)
c	call annuld(PANSOUDTI,  NETO)
c	call annuld(PANTOLRRO,  NETO)
c	call annuld(PANSOUDRRO, NETO)
c	call annuld(PANTotalTI, NETO)
c	call annuld(PANTotalRRO,NETO)

	do j=1,NETO
	do i=1,nnn
 	 PANTOLTI(j) =PANTOLTI(j) +TItol(i,j)      ! = coût tôlier pour toutes les op. par panneau (TI)
	 PANSOUDTI(j)=PANSOUDTI(j)+TIsoud(i,j)     ! = coût soudeur pour toutes les op. par panneau (TI)
	 PANTOLRRO(j)=PANTOLRRO(j)+RROtol(i,j)     ! = coût tôlier pour toutes les op. par panneau (RRO)
	 PANSOUDRRO(j) =PANSOUDRRO(j)+RROsoud(i,j) ! = coût soudeur pour toutes les op. par panneau (RRO)
	 PANTotalTI(j) =PANTOLTI(j)  +PANSOUDTI(j) ! = coût total par panneau (TI)
	 PANTotalRRO(j)=PANTOLRRO(j) +PANSOUDRRO(j)! = coût total par panneau (RRO) -> c'est ce qui nous
	enddo                                      !                                   intéresse
	enddo

cccccccccccccccccccccccccccccc
      SUMTOLTI = 0.
	SUMSOUDTI = 0.
	SUMTOLRRO = 0.
	SUMSOUDRRO = 0.

      do j=1,NETO
	 SUMTOLTI  =SUMTOLTI  +PANTOLTI(j)     ! = coût tôlier pour toute la structure (TI)
	 SUMSOUDTI =SUMSOUDTI +PANSOUDTI(j)    ! = coût soudeur pour toute la structure (TI)
	 SUMTOLRRO =SUMTOLRRO +PANTOLRRO(j)    ! = coût tôlier pour toute la structure (RRO)
	 SUMSOUDRRO=SUMSOUDRRO+PANSOUDRRO(j)   ! = coût soudeur pour toute la structure (RRO)
	enddo

cccccccccccccccccccccccccccccc
c      sumprepreti = 0.
c	sumpreprerro = 0.
c	sumpreassti = 0.
c	sumpreassrro = 0.
c	sumpremonti = 0.
c	sumpremonrro = 0.
 
	do i=1,10
	do j=1,NETO
	sumprepreti =sumprepreti +TItol(i,j) +TIsoud(i,j)
	sumpreprerro=sumpreprerro+RROtol(i,j)+RROsoud(i,j)
	enddo
	enddo

	do i=11,20
	do j=1,NETO
	sumprenti =sumprenti +TItol(i,j) +TIsoud(i,j)
	sumprenrro=sumprenrro+RROtol(i,j)+RROsoud(i,j)
	enddo
	enddo	

	do i=21,45
	do j=1,NETO
	sumpreassti =sumpreassti +TItol(i,j) +TIsoud(i,j)
	sumpreassrro=sumpreassrro+RROtol(i,j)+RROsoud(i,j)
	enddo
	enddo

	do i=46,nnn
	do j=1,NETO
	sumpremonti =sumpremonti +TItol(i,j) +TIsoud(i,j)
	sumpremonrro=sumpremonrro+RROtol(i,j)+RROsoud(i,j)
	enddo
	enddo
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c	call somme(TItol,nnn,NETO,SUMTOLTI)
c	call somme(TIsoud,nnn,NETO,SUMSOUDTI)
c	call somme(RROtol,nnn,NETO,SUMTOLRRO)
c	call somme(RROsoud,nnn,NETO,SUMSOUDRRO)


	SUMTOLTI  =SUMTOLTI  +pmb  +div(1)
	SUMSOUDTI =SUMSOUDTI +div(2)
	SUMTOLRRO =SUMTOLRRO +pmb  +div(1)
	SUMSOUDRRO=SUMSOUDRRO+div(2)
	totalTI   =SUMTOLTI  +SUMSOUDTI
	totalRRO  =SUMTOLRRO +SUMSOUDRRO

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  11.1    CALCUL DES SENSIBILITES
C  -------------------------------
C
Ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      
      IF (ICOUT.EQ.3) GOTO 784

	CALL ANNULD(DERIVEE,NN)
      NCONT = 0
	DO J=1,NETO
	 NBRXI = NVAR(J)
	 DO I=1,NBRXI
	  II = I + NCONT
	  I1 = NXIT(I,J)
	  IF (I1.EQ.1) THEN
	   DO L=1,10
	     DERIVEE(II) = DERIVEE(II) + 
     *     (0. 
     *      + (T21s(L,J)*ctspp + T21t(L,J)*cttpp)*Q(L,J))
     *      *K(L,J)*CA(L,J)*CAT(L,J)
	   ENDDO
	   DO L=11,20
	     DERIVEE(II) = DERIVEE(II) + 
     *     (0.
     *      + (T21s(L,J)*cmn*(1.+cbn) + T21t(L,J)*cmn)*Q(L,J))
     *      *K(L,J)*CA(L,J)*CAT(L,J)
	   ENDDO
	   DO L=21,45
	     DERIVEE(II) = DERIVEE(II) + 
     *     (0.
     *      + (T21s(L,J)*(1.+cbp) + T21t(L,J)*(1.+crp))*Q(L,J))
     *      *K(L,J)*CA(L,J)*CAT(L,J)
	   ENDDO
	   DO L=46,NNN
	     DERIVEE(II) = DERIVEE(II) + 
     *     (0.
     *      + (T21s(L,J) + T21t(L,J))*Q(L,J))
     *      *K(L,J)*CA(L,J)*CAT(L,J)
	   ENDDO
	  ENDIF
	  IF (I1.EQ.2) THEN
	   DO L=1,10
	     DERIVEE(II) = DERIVEE(II) + 
     *     (T12(L,J)*(CUs(L,J)*ctspp + CUt(L,J)*cttpp)
     *      + 0.)
     *      *K(L,J)*CA(L,J)*CAT(L,J)
	   ENDDO
	   DO L=11,20
	     DERIVEE(II) = DERIVEE(II) + 
     *     (T12(L,J)*(CUs(L,J)*cmn*(1.+cbn) + CUt(L,J)*cmn)
     *      + 0.)
     *      *K(L,J)*CA(L,J)*CAT(L,J)
	   ENDDO
	   DO L=21,45
	     DERIVEE(II) = DERIVEE(II) + 
     *     (T12(L,J)*(CUs(L,J)*(1.+cbp) + CUt(L,J)*(1.+crp))
     *      + 0.)
     *      *K(L,J)*CA(L,J)*CAT(L,J)
	   ENDDO
	   DO L=46,NNN
	     DERIVEE(II) = DERIVEE(II) + 
     *     (T12(L,J)*(CUs(L,J) + CUt(L,J))
     *      + 0.)
     *      *K(L,J)*CA(L,J)*CAT(L,J)
	   ENDDO
	  ENDIF

        IF (I1.EQ.3) THEN
	   DO L=1,10
           DERIVEE(II) = DERIVEE(II) + 
     *     (0. 
     *      + (T23s(L,J)*ctspp + T23t(L,J)*cttpp)*Q(L,J))
     *      *K(L,J)*CA(L,J)*CAT(L,J)
	   ENDDO
	   DO L=11,20
	     DERIVEE(II) = DERIVEE(II) + 
     *     (0.
     *      + (T23s(L,J)*cmn*(1.+cbn) + T23t(L,J)*cmn)*Q(L,J))
     *      *K(L,J)*CA(L,J)*CAT(L,J)
	   ENDDO
	   DO L=21,45
	     DERIVEE(II) = DERIVEE(II) + 
     *     (0.
     *      + (T23s(L,J)*(1.+cbp) + T23t(L,J)*(1.+crp))*Q(L,J))
     *      *K(L,J)*CA(L,J)*CAT(L,J)
	   ENDDO
	   DO L=46,NNN
	     DERIVEE(II) = DERIVEE(II) + 
     *     (0.
     *      + (T23s(L,J) + T23t(L,J))*Q(L,J))
     *      *K(L,J)*CA(L,J)*CAT(L,J)
	   ENDDO
	  ENDIF

	  IF (I1.EQ.5) THEN
	   DO L=1,10
	     DERIVEE(II) = DERIVEE(II) + 
     *     (T15(L,J)*(CUs(L,J)*ctspp + CUt(L,J)*cttpp)
     *      + 0.)
     *      *K(L,J)*CA(L,J)*CAT(L,J)
	   ENDDO
	   DO L=11,20
	     DERIVEE(II) = DERIVEE(II) + 
     *     (T15(L,J)*(CUs(L,J)*cmn*(1.+cbn) + CUt(L,J)*cmn)
     *      + 0.)
     *      *K(L,J)*CA(L,J)*CAT(L,J)
	   ENDDO
	   DO L=21,45
	     DERIVEE(II) = DERIVEE(II) + 
     *     (T15(L,J)*(CUs(L,J)*(1.+cbp) + CUt(L,J)*(1.+crp))
     *      + 0.)
     *      *K(L,J)*CA(L,J)*CAT(L,J)
	   ENDDO
	   DO L=46,NNN
	     DERIVEE(II) = DERIVEE(II) + 
     *     (T15(L,J)*(CUs(L,J) + CUt(L,J))
     *      + 0.)
     *      *K(L,J)*CA(L,J)*CAT(L,J)
	   ENDDO
	  ENDIF
	  IF (I1.EQ.6) THEN
	   DO L=1,10
	     DERIVEE(II) = DERIVEE(II) + 
     *     (T16(L,J)*(CUs(L,J)*ctspp + CUt(L,J)*cttpp)
     *      + (T26s(L,J)*ctspp + T26t(L,J)*cttpp)*Q(L,J))
     *      *K(L,J)*CA(L,J)*CAT(L,J)
	   ENDDO
	   DO L=11,20
	     DERIVEE(II) = DERIVEE(II) + 
     *     (T16(L,J)*(CUs(L,J)*cmn*(1.+cbn) + CUt(L,J)*cmn)
     *      + (T26s(L,J)*cmn*(1.+cbn) + T26t(L,J)*cmn)*Q(L,J))
     *      *K(L,J)*CA(L,J)*CAT(L,J)
	   ENDDO
	   DO L=21,45
	     DERIVEE(II) = DERIVEE(II) + 
     *     (T16(L,J)*(CUs(L,J)*(1.+cbp) + CUt(L,J)*(1.+crp))
     *      + (T26s(L,J)*(1.+cbp) + T26t(L,J)*(1.+crp))*Q(L,J))
     *      *K(L,J)*CA(L,J)*CAT(L,J)
	   ENDDO
	   DO L=46,NNN
	     DERIVEE(II) = DERIVEE(II) + 
     *     (T16(L,J)*(CUs(L,J) + CUt(L,J))
     *      + (T26s(L,J) + T26t(L,J))*Q(L,J))
     *      *K(L,J)*CA(L,J)*CAT(L,J)
	   ENDDO
	  ENDIF
	  IF (I1.EQ.9) THEN
	   DO L=1,10
	     DERIVEE(II) = DERIVEE(II) + 
     *     (T19(L,J)*(CUs(L,J)*ctspp + CUt(L,J)*cttpp)
     *      + 0.)
     *      *K(L,J)*CA(L,J)*CAT(L,J)
	   ENDDO
	   DO L=11,20
	     DERIVEE(II) = DERIVEE(II) + 
     *     (T19(L,J)*(CUs(L,J)*cmn*(1.+cbn) + CUt(L,J)*cmn)
     *      + 0.)
     *      *K(L,J)*CA(L,J)*CAT(L,J)
	   ENDDO
	   DO L=21,45
	     DERIVEE(II) = DERIVEE(II) + 
     *     (T19(L,J)*(CUs(L,J)*(1.+cbp) + CUt(L,J)*(1.+crp))
     *      + 0.)
     *      *K(L,J)*CA(L,J)*CAT(L,J)
	   ENDDO
	   DO L=46,NNN
	     DERIVEE(II) = DERIVEE(II) + 
     *     (T19(L,J)*(CUs(L,J) + CUt(L,J))
     *      + 0.)
     *      *K(L,J)*CA(L,J)*CAT(L,J)
	   ENDDO
	  ENDIF
	 ENDDO
	 NCONT = NCONT + NBRXI
	ENDDO

  784 CONTINUE

ccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  12.0    ECRITURE RESULTATS   
ccccccccccccccccccccccccccccccccccccccccccccccccccc

      IF (ICOUT.EQ.3) PAS = XXX !remettre la bonne valeur du pas pour SENSIBCOUT
  
	REWIND 1
	REWIND 2
	REWIND 3
	REWIND 7
	REWIND 8
	REWIND 10
	REWIND 44
	REWIND 88

	close(7)
	close(8)
	close(88)
	close(1)
	close(2)
	close(3)
	close(10)
	close(44)

      RETURN
  906 WRITE(*,*) 'STOP : The "input.txt" File is missing'           !juillet2003
	WRITE(29,*) 'STOP : The "input.txt" File is missing'							!bug
  907 WRITE(*,*) 'STOP : The "fractionnement.txt" File is missing'  !juillet2003
	WRITE(29,*) 'STOP : The "fractionnement.txt" File is missing'					!bug
  908 WRITE(*,*) 'STOP : The "acces.txt" File is missing'           !juillet2003
	WRITE(29,*) 'STOP : The "acces.txt" File is missing'							!bug
      STOP                                                          !juillet2003
	END





c################################################################################
cccccc  SUBROUTINE de CALCUL DES SOMMES DES TERMES D'UNE MARTICE    cccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	SUBROUTINE somme(A,n,m,suma)
	implicit real*8 (a-g,o-z)
	dimension A(n,m)

	suma=0.
	do i=1,n
	  do j=1,m
	    suma=suma+A(i,j)
	  enddo
	enddo

	return
	end




c################################################################################
cccccc  SUBROUTINE: ASCENDING ORDERING OF A VECTOR   
ccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	SUBROUTINE ORD(nn,n,sir)
	implicit real*8 (a-g,o-z)
     	dimension sir(nn,n)	

      DO 1 i=1,n-1
	 m=i+1
	 DO 2 j=m,n
	   IF(sir(1,i).gt.sir(1,j)) then
	     do k=1,nn
	       x=sir(k,i)
	       sir(k,i)=sir(k,j)
	       sir(k,j)=x
	     enddo
	   else
	     go to 2
	   endif
2	 continue
1	continue

	return
	end

c################################################################################
cccccc  SUBROUTINE: SPECIAL ASCENDING ORDERING OF A VECTOR   
ccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c	SUBROUTINE ORD2(nn,n,c,num)
c	implicit real*8 (a-g,o-z)
c     	dimension c(nn,n),num(n)
C     Tri spécial de 2 variables de conception 
c     (utilisé spécialement pour les opérations 49 et 50)
c
c     DO i=1,n-1
c	  k = 1
c	  do while (c(1,i).eq.c(1,i+k).and.(k+i).le.n)
c	    k = k+1
c	  enddo
c	  num(i)=k
c	ENDDO
c      num(n) = 1
c
c      DO i=1,n-1
c        if (num(i).gt.1) then
c	    do k=1,num(i)-1
c	      do j=k+1,num(i)
c	        if (c(2,k+i-1).gt.c(2,j+i-1)) then
c	          x = c(2,k+i-1)
c	          c(2,k+i-1) = c(2,j+i-1)
c	          c(2,j+i-1) = x
c	          x = c(3,k+i-1)
c	          c(3,k+i-1) = c(3,j+i-1)
c	          c(3,j+i-1) = x
c	        endif
c	      enddo
c	    enddo
c	  endif
c	ENDDO

c	return
c	end

cc################################################################################
cccccc  SUBROUTINE: INTERPOLATION
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      SUBROUTINE interpol(param,CU,nbrdon,var,cun,j,nnn)
      implicit real*8 (a-g,o-z)
    	dimension var(nnn,nbrdon),cun(nnn,nbrdon)

	DO ijk=1,nbrdon
	  if (param.le.var(j,1)) then
	    CU = cun(j,1)
	    goto 1
 	  else if(param.le.var(j,ijk)) then
	    C2 = cun(j,ijk)
	    C1 = cun(j,ijk-1)
	    V2 = var(j,ijk)
	    V1 = var(j,ijk-1)
          CU = (C2 - C1)*(param - V1)/(V2 - V1) + C1
	    goto 1
	  else if(param.ge.var(j,nbrdon)) then
          Cu = cun(j,nbrdon)
          goto 1
	  endif
      ENDDO

   1	return
	end

cc################################################################################
cccccc  SUBROUTINE: INTERPOLATION
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      SUBROUTINE interpol2(num,npabouts,param,CUt,variablet1,tolier,
c     *                     tolier1,tolier2,j,nnn)
c      implicit real*8 (a-g,o-z)
c     	dimension variablet1(num),tolier(num),tolier1(num),tolier2(num)
c
cc     Interpolation spéciale pour les opérations 49 et 50 (2 var de conception)

c	if (num.gt.1) then
c	  if (npabouts.eq.1) then
c	    call interpol(param*1000,CUt,num,variablet1,tolier,1,nnn)
c	  else if (npabouts.eq.2) then
c          call interpol(param*1000,CUt,num,variablet1,tolier1,1,nnn)
c	  else if (npabouts.eq.3) then
c          call interpol(param*1000,CUt,num,variablet1,tolier2,1,nnn)
c	  endif
c	else
c	  if (npabouts.eq.1) CUt = tolier(1)
c	  if (npabouts.eq.2) CUt = tolier1(1)
c	  if (npabouts.eq.3) CUt = tolier2(1)
c	endif
      
c	return
c	end

	
