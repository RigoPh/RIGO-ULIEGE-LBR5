      subroutine costmain(totalrro,vlength,non,numv,derivee,
     *	nder,totalti) ! ,trav) janv2006
      use sharedvar
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccc     module cout (alstom)     - lbr.5  - anast  ulg 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c développé par :  catalin toderan et frederic bair  
c                   décembre 2002  -    avril 2003   
c     
c ce module est appelé si iopti = 2 (sinon c'est le module objct1 qui est utilisé, calcul simplifié)
c
c modifications:
c   19 juin 2003 : cout des soudures # min(vepaiss(i),veac(i)); ligne 1885 = 
c   11 septembre 2003 : quelques adaptations pour pouvoir combiner les 2 versions                  
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c	   open (unit=1, file='dbinput.txt',          status='old',err=906) !juillet2003
c	   open (unit=2, file='dbfractionnement.txt', status='old')
c	   open (unit=3, file='dbacces.txt',          status='old',err=908) !juillet2003
c        open (unit=7, file='dbbase.txt',           status='old',err=904) !juillet2003
c 	   open (unit=44,file='dbsoudures.txt',       status='old',err=905) !juillet2003
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

	implicit real*8 (a-g,k,o-z)
      dimension trav(277565)			!sept06
	dimension derivee(nder)
   
      open (unit=7, file='dbbase.txt',     status='old',err=904) !juillet2003
	open (unit=44,file='dbsoudures.txt', status='old',err=905) !juillet2003

	read(7,*) nnn  ! = nombre d'opérations
	rewind 7
	close(7)
      noid = 25      ! = nombre id soudure (il faudrait pouvoir le lire directement
	               !                      dans le fichier base.txt)
      read(44,*)
	read(44,*) nropid ! = nombre d'opérations qui nécessitent id soudure  (max=8)
	rewind 44
	close(44)

      maxd = 35     ! = nombre maximal de points de données à notre disposition
	              !   (dans toutes les opérations)
	maxs = 21     ! idem mais pour les soudures
	j2 = max0(maxd,maxs)

      irowmax = 10  ! = nombre max de ligne par opération dans le fichier base.txt   
	iridmax = 8   ! = idem pour les soudures (base.txt aussi)	!iridmax = 3 janv2007
	j1 = max0(irowmax,iridmax)

      i1  = 1                  !iop(nnn)
	i2  = i1  + nnn          !ivtolier(nnn)
	i3  = i2  + nnn          !ivsoudeur(nnn)
	i4  = i3  + nnn          !tolier(nnn,maxd) 
	i5  = i4  + nnn*maxd     !soudeur(nnn,maxd)
	i6  = i5  + nnn*maxd     !variablet(nnn,maxd)
	i7  = i6  + nnn*maxd     !variables(nnn,maxd)
	i8  = i7  + nnn*maxd     !c(j1,j2)
	i9  = i8  + j1*j2        !iv(nnn,irowmax)
	i10 = i9  + nnn*irowmax  !irow(nnn)
	i11 = i10 + nnn          !nci(neto)
	i12 = i11 + neto         !nanp(neto)
	i13 = i12 + neto         !idpanneau(neto)
	i14 = i13 + neto         !ippanneau(neto)
	i15 = i14 + neto         !itpanneau(neto)
	i16 = i15 + neto         !nttapes(neto)
	i17 = i16 + neto         !npabouts(neto)
	i18 = i17 + neto         !k(nnn,neto)
	i19 = i18 + nnn*neto     !ca(nnn,neto) 
	i20 = i19 + nnn*neto     !cat(nnn,neto)
	i21 = i20 + nnn*neto     !variables1(nnn,maxd)
	i22 = i21 + nnn*maxd      !soudeur1(nnn,maxd)
	i23 = i22 + nnn*maxd      !variablet1(nnn,maxd)
	i24 = i23 + nnn*maxd      !tolier1(nnn,maxd)
	i25 = i24 + nnn*maxd      !iid(noid)
	i26 = i25 + noid         !irid(noid)
	i27 = i26 + noid         !is(noid,iridmax)
	i28 = i27 + noid*iridmax !ts(noid,maxs)
	i29 = i28 + noid*maxs    !vst(noid,maxs)
	i30 = i29 + noid*maxs    !ss(noid,maxs)
	i31 = i30 + noid*maxs    !vss(noid,maxs)
	i32 = i31 + noid*maxs    !ivts(noid)
	i33 = i32 + noid         !ivss(noid)
	i34 = i33 + noid         !idsoudures(nropid,neto)
	i35 = i34 + nropid*neto  !vgorsoud(nropid,neto)
	i36 = i35 + nropid*neto  !q(nnn,neto)
	i37 = i36 + nnn*neto     !cut(nnn,neto)
	i38 = i37 + nnn*neto     !cus(nnn,neto)
	i39 = i38 + nnn*neto     !rrotol(nnn,neto)
	i40 = i39 + nnn*neto     !rrosoud(nnn,neto)
	i41 = i40 + nnn*neto     !titol(nnn,neto)
	i42 = i41 + nnn*neto     !tisoud(nnn,neto)
	i43 = i42 + nnn*neto     !memovart(maxd)
	i44 = i43 + maxd         !memovars(maxd)
	i45 = i44 + maxd         !memosou(maxd)
	i46 = i45 + maxd         !memotol(maxd)
	i47 = i46 + maxd         !pantolti(neto)
      i48 = i47 + neto         !pantolrro(neto)
	i49 = i48 + neto         !pansoudti(neto)
	i50 = i49 + neto         !pansoudrro(neto)
	i51 = i50 + neto         !pantotalti(neto)
	i52 = i51 + neto         !t12(nnn,neto)
	i53 = i52 + nnn*neto     !t15(nnn,neto)
	i54 = i53 + nnn*neto     !t16(nnn,neto)
	i55 = i54 + nnn*neto     !t19(nnn,neto)
	i56 = i55 + nnn*neto     !t21s(nnn,neto)
	i57 = i56 + nnn*neto     !t23s(nnn,neto)
	i58 = i57 + nnn*neto     !t26s(nnn,neto)
	i59 = i58 + nnn*neto     !t21t(nnn,neto)
	i60 = i59 + nnn*neto     !t23t(nnn,neto)
	i61 = i60 + nnn*neto     !t26t(nnn,neto)
      itest=4*nnn+20*nnn*neto+j1*j2+nnn*irowmax+12*neto+8*nnn
     *       *maxd+4*noid+noid*iridmax+4*noid*maxs+2*nropid*neto+4*maxd
c	call annuld(trav,4*nnn+20*nnn*neto+j1*j2+nnn*irowmax+12*neto+8*nnn
c     *       *maxd+4*noid+noid*iridmax+4*noid*maxs+2*nropid*neto+4*maxd)
      call annuld(trav,275525) 

c      en prenant nnn max = 100 :

c total = 4*nnn + 20*nnn*neto + j1*j2 + nnn*irowmax + 12*neto + 8*nnn*maxd + 4*noid + noid*iridmax + 4*noid*maxs +2*nropid*neto + 4*maxd
c       = 400   + 240000      + 350   + 1000        + 1440    + 28000      + 100    + 75           + 2100        + 1920         + 140
c       = 275525   

	call costmain2(totalrro,icout,
     *  deltacsc,deltar,vhar,vlarg,vepaiss,vhac,veac,vlsc,vesc,vear,
     *  vlsr,vesr,entr,vlength,neto,non,
     *  numv,nnn,maxd,maxs,irowmax,iridmax,noid,nropid,j1,j2,derivee,
     *  nder,nvar,nxit,totalti,
     *  trav(i1),trav(i2),trav(i3),trav(i4),trav(i5),trav(i6),trav(i7),
     *  trav(i8),trav(i9),trav(i10),trav(i11),trav(i12),trav(i13),
     *  trav(i14),trav(i15),trav(i16),trav(i17),trav(i18),trav(i19),
     *  trav(i20),trav(i21),trav(i22),trav(i23),trav(i24),trav(i25),
     *  trav(i26),trav(i27),trav(i28),trav(i29),trav(i30),trav(i31),
     *  trav(i32),trav(i33),trav(i34),trav(i35),trav(i36),trav(i37),
     *  trav(i38),trav(i39),trav(i40),trav(i41),trav(i42),trav(i43),
     *  trav(i44),trav(i45),trav(i46),trav(i47),trav(i48),trav(i49),
     *  trav(i50),trav(i51),coutpan,
     *  trav(i52),trav(i53),trav(i54),trav(i55),trav(i56),trav(i57),
     *  trav(i58),trav(i59),trav(i60),trav(i61))
        
	return
  904 write(*,*) 'stop : the "base.txt" file is missing'     !juillet2003
      write(6970,*) 'stop : the "base.txt" file is missing'     !juillet2003
      write(29,*)'stop : the "base.txt" file is missing'	   !sept06	!bug
	stop                                                   !juillet2003
  905 write(*,*) 'stop : the "soudures.txt" file is missing' !juillet2003
      write(6970,*) 'stop : the "soudures.txt" file is missing' !juillet2003
      write(29,*)'stop : the "soudures.txt" file is missing' !sept06	!bug
	stop                                                   !juillet2003
	end
      
c =================================================================================
c =================================================================================

	subroutine costmain2(totalrro,icout,
     *  deltacsc,deltar,vhar,vlarg,vepaiss,vhac,veac,vlsc,vesc,vear,
     *  vlsr,vesr,entr,vlength,neto,non,
     *  numv,nnn,maxd,maxs,irowmax,iridmax,noid,nropid,j1,j2,derivee,
     *  nder,nvar,nxit,totalti,
     *  iop,ivtolier,ivsoudeur,tolier,soudeur,variablet,variables,
     *  c,iv,irow,nci,nanp,idpanneau,
     *  ippanneau,itpanneau,nttapes,npabouts,k,ca,
     *  cat,variables1,soudeur1,variablet1,tolier1,iid,
     *  irid,is,ts,vst,ss,vss,
     *  ivts,ivss,idsoudures,vgorsoud,q,cut,
     *  cus,rrotol,rrosoud,titol,tisoud,memovart,
     *  memovars,memosou,memotol,pantolti,pantolrro,pansoudti,
     *  pansoudrro,pantotalti,pantotalrro,
     *  t12,t15,t16,t19,t21s,t23s,
     *  t26s,t21t,t23t,t26t)

	implicit real*8 (a-g,k,o-z)
	integer kst,kkjj,k1,kk
	real*8 memosou,memovart,memovars,memotol
	dimension iop(nnn),ivtolier(nnn),ivsoudeur(nnn),tolier(nnn,100),	!tolier(nnn,neto) janv2007
     *   soudeur(nnn,100), variablet(nnn,maxd), variables(nnn,maxd),		!soudeur(nnn,neto) janv2007
     *   c(j1,j2),iv(nnn,irowmax),irow(nnn),
     *   nci(neto),nanp(neto),idpanneau(neto),ippanneau(neto),
     *   itpanneau(neto),nttapes(neto),npabouts(neto),k(nnn,neto),
     *   ca(nnn,neto),cat(nnn,neto),div(2),variables1(nnn,maxd),
     *   soudeur1(nnn,maxd),variablet1(nnn,maxd),tolier1(nnn,maxd),
     *   iid(noid),irid(noid),is(noid,iridmax),ts(noid,maxs),
     *   vst(noid,maxs),ss(noid,maxs),vss(noid,maxs),ivts(noid),
     *   ivss(noid),idsoudures(nropid,neto),vgorsoud(nropid,neto),
c ----------------
     *   vlarg(neto),vepaiss(neto),vhac(neto),veac(neto),
     *   vlsc(neto),vesc(neto),deltacsc(neto),vhar(neto),vear(neto),
     *   vlsr(neto),vesr(neto),deltar(neto),entr(neto),
     *   q(nnn,neto),
c ----------------
     *   cut(nnn,neto),cus(nnn,neto),rrotol(nnn,neto),rrosoud(nnn,neto),
     *   titol(nnn,neto),tisoud(nnn,neto),
     *   memovart(maxd),memovars(maxd),memosou(maxd),
     *   memotol(maxd),
c ----------------
     *   pantolti(neto),pantolrro(neto),pansoudti(neto),
     *   pansoudrro(neto),pantotalti(neto),pantotalrro(neto),
c ----------------
     *   t12(nnn,neto),t15(nnn,neto),t16(nnn,neto),t19(nnn,neto),
     *   t21s(nnn,neto),t23s(nnn,neto),t26s(nnn,neto),
     *   t21t(nnn,neto),t23t(nnn,neto),t26t(nnn,neto),
c -----------------
     *   derivee(nder),nvar(neto),nxit(9,neto)

ccc	dimension tolier2(52,35),soudeur2(52,35),num(35) 


      common/cout/ poids,spoids,
     *        fmat,fsou,fmdo,fmat11,fmat22,fmat33,fsou11,fsou22,
     *        fsou33,fmdo11,fmdo22,fmdo33,fmdo44,fmdo55,fmdo66,fmdo77,
     *        rend_global,eqp,dref,drefx,drefy,c11,c22,c33,dc11,dw2,dw3,
     *        p10,dp10,p4,p5,dp4,dp5,p9x,p9y,dp9x,dp9y,ber,bet,p6,p7,c8,
     *        dc8,cout,ialr,ialt      !nécessaire pour avoir icout
c	common/cout/icout
	common/alstom/idiff,nbr,pas
     	
	call annuld(pantotalrro,neto)
	call annuld(derivee,nder)!,nn	!janv2007
	
	if (icout.eq.3) then
	  xxx = pas
	  pas = 0.   !car dans costmain, on ne se sert du pas que pour icout=2
      endif
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     liste des variables utilisees		
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c	nnn   = nombre total des opérations
c	iop(i)= numero de l'opération (pré-pré, pre, montage)	i=1,nnn 
c	tolier(i,j) = couts unitaires tolier					i=i
c	soudeur(i,j)= couts unitaires soudeur					i=i
c	variablet(i,j)= matrice des valeurs de la variable des cu tôlier
c	variables(i,j)= matrice des valeurs de la variable des cu tôlier
c	c(i,j) =matrice intermediaire de lecture de la base de données
c	nci(i) =nombre couples intermediaires                   i=indice du panneau
c	nanp(i)=nombre abouts de la nappe plane					i=indice du panneau
c	idpanneau(i)=paramètre id panneau						i=indice du panneau
c	ippanneau(i)=paramètre ip panneau						i=indice du panneau
c	itpanneau(i)=paramètre it panneau						i=indice du panneau
c	nttapes(i)  =paramètre type tapes						i=indice du panneau
c	npabouts(i) =paramètre p abouts							i=indice du panneau
c	k(i,j)  =coeff. de fractionnement		i=indice panneau,j=indice opération
c	ca(i,j) =coeff. de acces				i=indice panneau,j=indice opération
c	cat(i,j)=coeff. de atelier				i=indice panneau,j=indice opération

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  0.0   initialisation des variables
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
	k= 0
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

	q= 0.

    	cut= 0.
	cus= 0.
	rrotol= 0.
	rrosoud= 0.
     	titol= 0.
	tisoud= 0.
	totalti=0.
	memovart =0.
      memovars =0.
	memotol = 0.
	memosou = 0.

	kkjj = 0
	kst = 0
	paras = 0.
	parat = 0.
	iijj = 0
	v1 = 0.
	v2 = 0.
	c1 = 0.
      c2 = 0.
	id = 0
	nrpanneaux = 0
	nk = 0
	nn = 0
	nkkkk = 0
	nnnnn = 0
	crp = 0.
	cbp = 0.
	cmn = 0.
	cbn = 0.
	cttpp = 0.
	ctspp = 0.
	ijk = 0
	i = 0
	j = 0
	pmb = 0.
	nam = 0
	ncatelier = 0
	ncacces = 0
	nfract = 0
	idso = 0
	id = 0

	sumtolti=0. ! ceux-là, indispensable de les annuler !!!
	sumsoudti=0.
	sumtolrro=0.
	sumsoudrro=0.
	sumprepreti=0.
	sumpreprerro=0.
	sumprenti=0.
	sumprenrro=0.
	sumpreassti=0.
	sumpreassrro=0.
	sumpremonti=0.
	sumpremonrro=0.

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 1.0	open files			
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	open (unit=1, file='dbinput.txt',          status='old',err=906) !juillet2003
c	open (unit=2, file='dbfractionnement.txt', status='old')
	open (unit=7, file='dbbase.txt',           status='old')
	open (unit=8, file='resultats cout.txt', status='unknown')
	open (unit=88,file='sensibilites.txt',   status='unknown')
c	open (unit=9, file='verif input.txt' ,   status='unknown')


c 	formats		
c     =======
111	format (f4.2)
112	format (i3)
44	format ('cout tolier  =',f4.2)
45	format ('cout soudeur =',f4.2)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 2.0	lecture du fichier input        
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	write(8,*) 'fichier des resultats'
	write(8,*) '====================='
	write(8,*) ' '
c
	write(8,*) 'verification input module cout'
	write(8,*) '------------------------------'
	write(8,*) ' '
		read(1,*) neto
		read(1,*) nfract
		read(1,*) ncacces
		read(1,*) ncatelier
	write(8,*) 'nombre panneaux'
	write(8,112) neto
	write(8,*) ' '

	if (nfract.eq.0) then
	  write(8,*) 'fractionnement par defaut'
	  write(8,*) ' '
	else
	  write(8,*) 'fractionnement defini par l utilisateur'
	  write(8,*) ' '
	endif

	if (ncacces.eq.0) then
	  write(8,*) 'coeff acces par defaut'
	  write(8,*) ' '
	else
	  write(8,*) 'coeff acces defini par l utilisateur'
	  write(8,*) ' '
	endif

	if (ncatelier.eq.0) then
	  write(8,*) 'coeff atelier par defaut'
	  write(8,*) ' '
	else
	  write(8,*) 'coeff atelier defini par l utilisateur'
	  write(8,*) ' '
	endif

		read(1,*) nam
		read(1,*) pmb
		read(1,*) (div(i),i=1,2)
	write(8,*) ' '
	write(8,*) 'nombre abouts montage','    ',nam
	write(8,*) 'cout preparer-mettre-balancer','  ',pmb
	write(8,*) 'cout divers tolier','  ',div(1)
	write(8,*) 'cout divers soudeur','  ',div(2)
	write(8,*) ' '	
		
		read(1,*) (nci(i),i=1,neto)
		read(1,*) (nanp(i),i=1,neto)
		read(1,*) (idpanneau(i),i=1,neto)
		read(1,*) (ippanneau(i),i=1,neto)
		read(1,*) (itpanneau(i),i=1,neto)
		read(1,*) (nttapes(i),i=1,neto)
		read(1,*) (npabouts(i),i=1,neto)

	write(8,*) 'no','   ','nci','   ','nanp','   ','id panneau',
     *'   ','ip panneau','   ','it panneau','   ','type tapes','   ',
     *'p abouts'

	do i=1,neto
	  write(8,*) i,nci(i),nanp(i),idpanneau(i)
     *        ,ippanneau(i),itpanneau(i),nttapes(i),npabouts(i)
	enddo

	write(8,*) '****************************************************'
c	write(*,*) 'lecture input terminee'


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  3.0   lecture de la base de donnees	
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	write(8,*) 'verification de la lecture de la base de donnees'
	write(8,*) '------------------------------------------------'
 
	read(7,*) nnn
	write(8,*) 'nombre operations = ',nnn
	write(8,*) ' '

      do i=1,nnn
	  ivtolier(i)=1
	  ivsoudeur(i)=1
	enddo
c ===========================
c     boucle 778 et 777
c     -----------------
      do 778 i=1,nnn  
	  read(7,*) iop(i),irow(i)
c	  write(8,*) ' operation',' ',iop(i)
c	  write(8,*) ' '
	  do j=1,irow(i)
	    read(7,*) iv(i,j),(c(j,ijk),ijk=1,iv(i,j))
	  enddo

c     boucle 777 (interne à 778)
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
		
777	continue

778	continue
c ==================================================

	do i=1,nnn
	  write(8,*) ' operation',' ',iop(i)
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
c	write(*,*) 'lecture base de donnees terminee'


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 4.0 	lecture coeff et id soudures           
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
	write (8,*) 'id soudures'
	write (8,*) '==========='
	write (8,*) ' '
	read(7,*) noid
	write(8,*) 'nombre id soudures =',noid
	write (8,*) ' '
c
	do 780 i=1,noid  ! 780

	read(7,*) iid(i),irid(i)
c	write(8,*) ' id',' ',iid(i)
c	write(8,*) ' '

	do j=1,irid(i)
	  read(7,*) is(i,j),(c(j,ijk),ijk=1,is(i,j))
	enddo
c
	do j=1,irid(i) 
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
	enddo
 780	continue ! fin boucle 780

c	do i=1,noid
c	write(8,*) ' id soudure',' ',iid(i)
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

!!!	write(*,*)'lecture id soudures terminee'
	
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 5.0  	lecture des coeff de fractionnement       
c      et	generation des fractionnement par defaut  
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	if (nfract.eq.1) then

	  open (unit=2,file='dbfractionnement.txt',status='old',err=907) !juillet2003

	  read(2,*) nnnnn
c	  write(*,*) 'nnnnn',nnnnn
	  read(2,*) nkkkk
c	  write(*,*) nkkkk

	  if(nnnnn.ne.neto) then
	    write(*,*)'nbr de panneaux incompatible: fractionnement-input'
	    write(6970,*)
     *'nbr de panneaux incompatible: fractionnement-input'
	    write(29,*)'nbr de panneaux incompatible: fractionnement-input'		!sept06	!bug
	    stop
	  endif

	  read(2,*) ((k(i,j),j=1,nnnnn),i=1,nkkkk)

	else  ! valeur par défaut

	  nnnnn=neto
	  nkkkk=nnn
	  do i=1,nnn
	  do j=1,neto
	     k(i,j)=0.
	  enddo
	  enddo

	do i=1,nnn
	do j=1,neto

	 if (i.eq.1) then
	   if (idpanneau(j).eq.1.and.itpanneau(j).eq.2) then
	    k(i,j)=1.
	   else
	    k(i,j)=0.
	   endif
	 endif

	 if (i.eq.2) then
	   if (idpanneau(j).eq.1.and.itpanneau(j).eq.2) then
	    k(i,j)=1.
	   else
	    k(i,j)=0.
	   endif
	   if (idpanneau(j).eq.2) then
	     k(i,j)=1.
	   else
	     k(i,j)=0.
	   endif
	 endif

	 if (i.eq.3) then
	  if (idpanneau(j).eq.1) then
	  k(i,j)=1.
	  else
	  k(i,j)=0.
	  endif
	 endif

	 if (i.eq.4) then
	  if (idpanneau(j).eq.2) then
	  k(i,j)=1.
	  else
	  k(i,j)=0.
	  endif
	 endif

	 if (i.eq.5) then
	  if (idpanneau(j).eq.2) then
	  k(i,j)=1.
	  else
	  k(i,j)=0.
	  endif
	 endif

	if (i.eq.6) then
	if (idpanneau(j).eq.1.and.itpanneau(j).eq.2) then
	k(i,j)=1.
	else
	k(i,j)=0.
	endif
	endif 

	if (i.eq.7) then
	if (idpanneau(j).eq.1.and.itpanneau(j).eq.2) then
	k(i,j)=1.
	else
	k(i,j)=0.
	endif
	endif

	if (i.eq.8) then
	if (idpanneau(j).eq.1) then
	k(i,j)=1.
	else
	k(i,j)=0.
	endif
	endif

	if (i.eq.9) then
	if (idpanneau(j).eq.1.and.itpanneau(j).eq.2) then
	k(i,j)=1.
	else
	k(i,j)=0.
	endif
	endif

	if (i.eq.10) then
	k(i,j)=0.
	endif

	if (i.eq.11) then
	if (idpanneau(j).eq.1) then
	k(i,j)=1.
	else
	k(i,j)=0.
	endif
	endif

	if (i.eq.12) then
	if (idpanneau(j).eq.1) then
	k(i,j)=1.
	else
	k(i,j)=0.
	endif
	endif

	if (i.eq.13) then
	if (idpanneau(j).eq.1) then
	k(i,j)=1.
	else
	k(i,j)=0.
	endif
	endif

	if (i.eq.14) then
	if (idpanneau(j).eq.1) then
	k(i,j)=1.
	else
	k(i,j)=0.
	endif
	endif

	if (i.eq.15) then
	if (idpanneau(j).eq.1) then
	k(i,j)=1.
	else
	k(i,j)=0.
	endif
	endif

	if (i.eq.16) then
	if (idpanneau(j).eq.1) then
	k(i,j)=1.
	else
	k(i,j)=0.
	endif
	endif

	if (i.eq.17) then
	if (idpanneau(j).eq.1) then
	k(i,j)=1.
	else
	k(i,j)=0.
	endif
	endif

	if (i.eq.18) then
	if (idpanneau(j).eq.1) then
	k(i,j)=1.
	else
	k(i,j)=0.
	endif
	endif

	if (i.eq.19) then
	if (idpanneau(j).eq.1) then
	k(i,j)=1.
	else
	k(i,j)=0.
	endif
	endif

	if (i.eq.20) then
	k(i,j)=0.
	endif

	if (i.eq.21) then
	if (itpanneau(j).eq.1) then
	k(i,j)=1.
	else
	k(i,j)=0.
	endif
	endif

	if (i.eq.22) then
	if (idpanneau(j).eq.2) then
	k(i,j)=1.
	else
	k(i,j)=0.
	endif
	endif

	if (i.eq.23) then
	if (idpanneau(j).eq.2) then
	k(i,j)=1.
	else
	k(i,j)=0.
	endif
	endif
	if (i.eq.24) then
	if (itpanneau(j).eq.1) then
	k(i,j)=1.
	else
	k(i,j)=0.
	endif
	endif

	if (i.eq.25) then
	if (itpanneau(j).eq.1) then
	k(i,j)=1.
	else
	k(i,j)=0.
	endif
	endif

	if (i.eq.26) then
	k(i,j)=0.
	endif

	if (i.eq.27) then
	if (itpanneau(j).eq.1) then
	k(i,j)=1.
	else
	k(i,j)=0.
	endif
	endif

	if (i.eq.28) then
	if (itpanneau(j).eq.1) then
	k(i,j)=1.
	else
	k(i,j)=0.
	endif
	endif

	if (i.eq.29) then
	if (idpanneau(j).eq.1.and.itpanneau(j).eq.1) then
	k(i,j)=1.
	else
	k(i,j)=0.
	endif
	endif

	if (i.eq.30) then
	if (itpanneau(j).eq.1) then
	k(i,j)=1.
	else
	k(i,j)=0.
	endif
	endif

	if (i.eq.31) then
	k(i,j)=0.
	endif

	if (i.eq.32) then
	if (idpanneau(j).eq.3) then
	k(i,j)=1.
	else
	k(i,j)=0.
	endif
	endif

	if (i.eq.33) then
	if (idpanneau(j).eq.3) then
	k(i,j)=1.
	else
	k(i,j)=0.
	endif
	endif

	if (i.eq.34) then
	if (idpanneau(j).eq.3) then
	k(i,j)=1.
	else
	k(i,j)=0.
	endif
	endif

	if (i.eq.35) then
	k(i,j)=0.
	endif

	if (i.eq.36) then
	if (itpanneau(j).eq.2) then
	k(i,j)=1.
	else
	k(i,j)=0.
	endif
	endif

	if (i.eq.37) then
	if (idpanneau(j).eq.2) then
	k(i,j)=1.
	else
	k(i,j)=0.
	endif
	endif

	if (i.eq.38) then
	if (itpanneau(j).eq.2) then
	k(i,j)=1.
	else
	k(i,j)=0.
	endif
	endif

	if (i.eq.39) then
	if (itpanneau(j).eq.2) then
	k(i,j)=1.
	else
	k(i,j)=0.
	endif
	endif

	if (i.eq.40) then
	k(i,j)=0.
	endif

	if (i.eq.41) then
	if (itpanneau(j).eq.2) then
	k(i,j)=1.
	else
	k(i,j)=0.
	endif
	endif

	if (i.eq.42) then
	if (itpanneau(j).eq.2) then
	k(i,j)=1.
	else
	k(i,j)=0.
	endif
	endif

	if (i.eq.43) then
	if (itpanneau(j).eq.2) then
	k(i,j)=1.
	else
	k(i,j)=0.
	endif
	endif

	if (i.eq.44) then
	if (itpanneau(j).eq.2) then
	k(i,j)=1.
	else
	k(i,j)=0.
	endif
	endif

	if (i.eq.45) then
	k(i,j)=0.
	endif

	if (i.eq.46) then
	k(i,j)=1.
	endif

	if (i.eq.47) then
	if (ippanneau(j).eq.2) then
	k(i,j)=1.
	else
	k(i,j)=0.
	endif
	if(idpanneau(j).eq.3) then
	k(i,j)=1.
	endif
	if(idpanneau(j).eq.4) then
	k(i,j)=1.
	endif
	endif

	if (i.eq.48) then
	k(i,j)=1.
	endif

	if (i.eq.49) then
	if (idpanneau(j).eq.1.or.idpanneau(j).eq.3) then
	k(i,j)=1.
	else
	k(i,j)=0.
	endif
	endif

	if (i.eq.50) then
	if (idpanneau(j).eq.1.or.idpanneau(j).eq.3) then
	k(i,j)=1.
	else
	k(i,j)=0.
	endif
	endif

	if (i.eq.51) then
	if (idpanneau(j).eq.2) then
	k(i,j)=1.
	else
	k(i,j)=0.
	endif
	endif

	  if (i.eq.52) then
	    k(i,j)=1.
	  endif

	enddo  ! fin boucle sur nnn
	enddo  ! fin boucle sur neto

	endif

c  5.1  	verification du fractionnement par defaut  
c ---------------------------------------------------
	write(8,*) ' '
	write(8,*) 'verification fractionnement'
	write(8,*) '==========================='

	do j=1,neto
	 write(8,*) 'panneau ', j
	 write(8,*) '============'
	 write(8,*) ' '
	 do i=1,nnn
	   write(8,*) 'operation',i,'   ',k(i,j)
	 enddo
	 write(8,*) '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
	 write(8,*) ' '
	enddo

	write(8,*) '*****************************************************'


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  6.0	lecture des coeff de acces et d'atelier      
c     	generation des coefficients par defaut    
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	if (nfract.eq.1.and.ncatelier.eq.1) then

	  open (unit=3,file='dbacces.txt', status='old',err=908) !juillet2003

	  read(3,*) nn
c	  write(*,*) 'nn',nn
	  read(3,*) nk
c	  write(*,*) 'nk',nk

	  if(nn.ne.neto) then
	    write(*,*) 'nombre des panneaux incompatible acces-input'
	    write(6970,*) 'nombre des panneaux incompatible acces-input'
		write(29,*)' '													!sept06			!bug
		write(29,*)'subroutine costmain'								!sept06			!bug
		write(29,*)'nombre des panneaux incompatible acces-input'		!sept06			!bug
c	    stop
	  endif
	  if(nk.ne.nkkkk) then
	    write(*,*) 'nombre des opérations incompatible acces',
     *               '-fractionnement'
	    write(6970,*) 'nombre des opérations incompatible acces',
     *               '-fractionnement'
		write(29,*)' '													!sept06			!bug
		write(29,*)'subroutine costmain'								!sept06			!bug
		write(29,*)'nombre des opérations incompatible acces',			!sept06
     *			   '-fractionnement'									!sept06
c	    stop
	  endif

	  read(3,*) ((ca(i,j) ,j=1,nn),i=1,nk)
	  read(3,*) ((cat(i,j),j=1,nn),i=1,nk)

	else  ! valeur par défaut

	 do i=1,10
	 do j=1,neto
	   ca(i,j) =1.
	   cat(i,j)=1.25
	 enddo
	 enddo

	do i=11,20
	do j=1,neto
	ca(i,j) =1.
	cat(i,j)=0.45
	enddo
	enddo

	do i=21,31
	do j=1,neto
	ca(i,j) =1.12
	cat(i,j)=1.32
	enddo
	enddo

	do i=21,31
	do j=1,neto
	ca(i,j) =1.12
	cat(i,j)=1.32
	enddo
	enddo

	do i=32,34
	do j=1,neto
	ca(i,j) =1.12
	cat(i,j)=1.32
	enddo
	enddo

	do i=35,45
	do j=1,neto
	ca(i,j) =1.22
	cat(i,j)=1.32
	enddo
	enddo

	 do i=46,51
	 do j=1,neto
	    ca(i,j) =1.27
	    cat(i,j)=2.4
	 enddo
	 enddo

	 do j=1,neto
	   ca(52,j)=1.
	   cat(52,j)=1.
	 enddo

	endif

c  6.1  	verification des coef. d'acces et d'atelier  
c ---------------------------------------------------
	write(8,*) ' '
	write(8,*) 'verification acces et atelier'
	write(8,*) '==========================='
	
	do i=1,neto
	  write(8,*) 'panneau ',i
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
c 7.0 	lecture des parametres des panneaux 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     vlenght = longueur des panneaux
c     nopan   = numéro du panneau (à supprimer éventuellement)
c     vlarg   = largeur des panneaux      
c     vepaiss = épaisseur des tôles
c     vhac   = hauteur de l'âme des cadres
c     veac   = épaisseur de l'âme des cadres
c     vlsc   = largeur des semelles des cadres
c     vesc   = épaisseur des semelles des cadres
c     deltacsc = écartement entre cadres
c     vhar   = hauteur de l'âme des raidisseurs
c     vear   = épaisseur de l'âme des raidisseurs
c     vlsr   = largeur des semelles des raidisseurs
c     vesr   = épaisseur des semelles des raidisseurs
c     deltar = écartement entre raidisseurs
c     entr

      write(8,*)'verification de la lecture des parametres des panneaux'
	write(8,*) '-----------------------------------------------------'


!!!avril2003	open (unit=10, file='alstom.txt', status='old')
!!!avril2003	read(10,*) vlength

      write(8,*) 'longueur des panneaux =',vlength
	write(8,*) ' '

c imprimer sur le fichier "8" chaque paramètre de chaque panneau ??????

!!!avril2003  do i=1,neto
!!!avril2003  read(10,*) nopan  ,vlarg(i),vepaiss(i)
!!!avril2003	read(10,*) vhac(i),veac(i),vlsc(i),vesc(i),deltac(i)
!!!avril2003	read(10,*) vhar(i),vear(i),vlsr(i),vesr(i),deltar(i),entr(i)
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
c	write(*,*) (deltac(i),i=1,neto)



cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 8.0 	lecture des id des soudures;          
c     	lecture des gorges des soudures     
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

	open (unit=44,file='dbsoudures.txt', status='old')

	write(8,*) ' '
	write(8,*) 'verification id soudures'
	write(8,*) '========================'

	read(44,*) nrpanneaux
	read(44,*) nropid

	if(nnnnn.ne.nrpanneaux) then
	  write(*,*) 'le nombre de panneaux est incompatible:',
     *  'fractionnement-soudures'
	  write(6970,*) 'le nombre de panneaux est incompatible:',
     *  'fractionnement-soudures'
	  write(29,*)' '																!bug
	  write(29,*)'subroutine costmain'												!bug
	  write(29,*) 'le nombre de panneaux est incompatible:',						!bug
     *  'fractionnement-soudures'	
		stop
	endif

	if(neto.ne.nrpanneaux) then
	  write(*,*)'le nombre de panneaux est incompatible:soudure-input'
	  write(6970,*)
     *'le nombre de panneaux est incompatible:soudure-input'
	  write(29,*)' '																!bug
	  write(29,*)'subroutine costmain'												!bug
	 write(29,*)'le nombre de panneaux est incompatible:soudure-input'				!bug
	  stop
	endif

c	write(8,*) 
c	write(8,*) 'nombre panneaux =',nrpanneaux
c	write(8,*) 'nombre operations qui necessitent id',nropid

	read(44,*) ((idsoudures(i,j),j=1,nrpanneaux),i=1,nropid)		
	read(44,*) ((vgorsoud(i,j),j=1,nrpanneaux),i=1,nropid)
	
c	do j=1,nrpanneaux
c	  write(8,*) ' '
c	  write(8,*) 'panneau',j
c	  write(8,*) ' '
c	  write(8,*) 'id soudures'
c	  write(8,*) 'op 21',idsoudures(1,j)
c	  write(8,*) 'op 22',idsoudures(2,j)
c	  write(8,*) 'op 23',idsoudures(3,j)
c	  write(8,*) 'op 36',idsoudures(4,j)
c	  write(8,*) 'op 37',idsoudures(5,j)
c	  write(8,*) 'op 47',idsoudures(6,j)
c	  write(8,*) 'op 48',idsoudures(7,j)
c	  write(8,*) 'op 51',idsoudures(8,j)
c	  write(8,*) ' '
c	  write(8,*) 'gorges soudures'
c	  write(8,*) 'op 21',vgorsoud(1,j)
c	  write(8,*) 'op 22',vgorsoud(2,j)
c	  write(8,*) 'op 23',vgorsoud(3,j)
c	  write(8,*) 'op 36',vgorsoud(4,j)
c	  write(8,*) 'op 37',vgorsoud(5,j)
c	  write(8,*) 'op 47',vgorsoud(6,j)
c	  write(8,*) 'op 48',vgorsoud(7,j)
c	  write(8,*) 'op 51',vgorsoud(8,j)
c	enddo

	write(8,*) '***********************'

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 9.0         calcul des quantites              
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     q(j,i) = quantité relative à l'opération "j" du panneau i

      do i=1,neto  ! boucle générale
      do j=1,nnn
      	
	if (j.eq.1) then
	q(j,i) = (vlarg(i)/deltar(i))*(vlength/deltacsc(i))
	endif

	if (j.eq.2.and.idpanneau(i).eq.1) then
	q(j,i) = (vlarg(i)/deltar(i))*(vlength/deltacsc(i))
	endif
	if (j.eq.2.and.idpanneau(i).eq.2) then					! fevrier 2004
	q(j,i) = 2*nci(i)*(vlength/deltacsc(i))					! fevrier 2004
	endif													! fevrier 2004
	if (j.eq.2.and.idpanneau(i).eq.4) then					! fevrier 2004
	q(j,i) = (vlength/deltacsc(i))+(vlarg(i)/deltar(i))		! fevrier 2004
	endif													! fevrier 2004

	if (j.eq.3) then
	q(j,i) = (0.5*vhac(i) - vhar(i))*(vlarg(i)/deltar(i))*(vlength/
     *               deltacsc(i))
      endif
	if (j.eq.3.and.idpanneau(i).eq.4) then					! fevrier 2004
	q(j,i) = vlarg(i)*(vlength/deltacsc(i))					! fevrier 2004
      endif													! fevrier 2004

	if (j.eq.4.or.j.eq.5) then
	  q(j,i) = 2*vlarg(i)*nci(i)*(vlength/deltacsc(i))
	endif

	if (j.eq.6) then
	if (idpanneau(i).eq.1.and.itpanneau(i).eq.2.and.ippanneau(i).eq.1)
     *then
	  q(j,i) = 1*(vlength/deltacsc(i))
	endif
      if (idpanneau(i).eq.1.and.itpanneau(i).eq.2.and.ippanneau(i).eq.2)
     *then	
	  q(j,i) = 0.5*(vlength/deltacsc(i))
	endif
	endif

	if (j.eq.7) then
        q(j,i) = 2*(vlength/deltacsc(i))
	endif

	if (j.eq.8) then
        q(j,i) = (vlarg(i)/deltar(i))*(vlength/deltacsc(i))
	endif

	if (j.eq.9) then
        q(j,i) = vhac(i)*(vlength/deltacsc(i))
	endif

      if (j.eq.10) then
	  q(j,i) = (0.5*vhac(i) - vhar(i))*(vlarg(i)/deltar(i))*(vlength/
     *                deltacsc(i))
	endif

	if (j.eq.11) then
	 if (idpanneau(i).eq.1.and.ippanneau(i).eq.1) then
	  q(j,i) = 1*(nam + 1)*(nanp(i) + 1)
       endif
	 if (idpanneau(i).eq.1.and.ippanneau(i).eq.2) then
        q(j,i) = 0.5*(nam + 1)*(nanp(i) + 1)
	 endif
	endif

	if (j.eq.12.or.j.eq.17) then
       if (idpanneau(i).eq.1.and.ippanneau(i).eq.1) then
	  q(j,i) = 1*vlength
	 endif
	 if (idpanneau(i).eq.1.and.ippanneau(i).eq.2) then
	  q(j,i) = 0.5*vlength
	 endif
	endif

	if ((j.eq.13.or.j.eq.18).and.idpanneau(i).eq.1) then
	q(j,i) = vlarg(i)*(nam + 1)*nanp(i)
	endif
	if (j.eq.14.and.idpanneau(i).eq.1) then
	q(j,i) = vlarg(i)*vlength
	endif

      if (j.eq.15) then
	if (idpanneau(i).eq.1.and.ippanneau(i).eq.1) then
	q(j,i) = 2*vlarg(i)*(nam + 1)
	endif
      if (idpanneau(i).eq.1.and.ippanneau(i).eq.2) then
      q(j,i) = 2*vlarg(i)*(nam + 1) + vlength
	endif
	endif

	if ((j.eq.16.or.j.eq.19).and.idpanneau(i).eq.1) then
	q(j,i) = (vlarg(i)/deltar(i))*vlength
	endif

      if ((j.eq.20).and.idpanneau(i).eq.1) then
	q(j,i) = (vlarg(i)/deltar(i))*vlength
	endif

	if (j.eq.21) then
	q(j,i) = (vlength/deltacsc(i))*vlarg(i)
	endif

	if (j.eq.22.or.j.eq.37) then
	q(j,i) = vlength
	endif

	if (j.eq.23) then
	q(j,i) = vlarg(i)*(vlength/deltacsc(i))
	endif

      if (j.eq.24.or.j.eq.25.or.j.eq.44.or.j.eq.27.or.j.eq.38.or.j.eq.39
     *           .or.j.eq.41.or.j.eq.30) then
	q(j,i) = (vlarg(i)/deltar(i))*(vlength/deltacsc(i))
	endif

	if (j.eq.26.or.j.eq.40.or.j.eq.45.or.j.eq.31) then
      q(j,i) = (vlarg(i)/deltar(i))*1
	endif

      if (j.eq.28.or.j.eq.29.or.j.eq.42.or.j.eq.43) then
	q(j,i) = nci(i)*(vlength/deltacsc(i))
	endif

      if (j.eq.32) then
	q(j,i) = vlarg(i)/deltar(i)
	endif

	if (j.eq.33.or.j.eq.35) then
	q(j,i) = 1
	endif

	if (j.eq.34) then
	q(j,i) = (vlarg(i)/deltar(i))*vlength
	endif

	if (j.eq.36) then
	q(j,i) = (vlength/deltacsc(i))*vlarg(i)
	endif

      if (j.eq.47) then
	q(j,i) = vlength
	endif

	if (j.eq.48) then
	q(j,i) = vlarg(i)*nam
	endif

	if (j.eq.49.or.j.eq.50) then
	q(j,i) = (vlarg(i)/deltar(i))*nam
	endif

      if (j.eq.51) then
	q(j,i) = vlarg(i)*(vlength/deltacsc(i))
	endif

c     opération 52 (+46) ?????????????????????      
      if (j.eq.53.and.(idpanneau(i).eq.1.or.idpanneau(i).eq.4)) then	! mars 2004
	q(j,i) = vlarg(i)*(vlength/deltacsc(i))			! fevrier 2004
	endif											! fevrier 2004
      if (j.eq.54.and.(idpanneau(i).eq.1.or.idpanneau(i).eq.4)) then	! mars 2004
	q(j,i) =vlength									! fevrier 2004
	endif											! fevrier 2004
      if (j.eq.55.and.(idpanneau(i).eq.1.or.idpanneau(i).eq.4)) then	! mars 2004
	q(j,i) =(vlength/deltacsc(i))						! fevrier 2004
	endif											! fevrier 2004
      if (j.eq.56.and.(idpanneau(i).eq.1.or.idpanneau(i).eq.4)) then	! mars 2004
	q(j,i) =(vlength/deltacsc(i))						! fevrier 2004
	endif											! fevrier 2004
      if (j.eq.57.and.(idpanneau(i).eq.1.or.idpanneau(i).eq.4)) then	! mars 2004
	q(j,i) =(vlength/deltacsc(i))						! fevrier 2004
	endif											! fevrier 2004
      if (j.eq.58.and.idpanneau(i).eq.5) then			! fevrier 2004
	q(j,i) =(vlength/deltacsc(i))						! fevrier 2004
	endif											! fevrier 2004
      if (j.eq.59.and.idpanneau(i).eq.5) then			! fevrier 2004
	q(j,i) =(vlength/deltacsc(i))						! fevrier 2004
	endif											! fevrier 2004
      if (j.eq.60.and.k(60,i).eq.1) then				! fevrier 2004
	q(j,i) =(vlength/deltacsc(i))						! fevrier 2004
	endif											! fevrier 2004
      if (j.eq.60.and.k(60,i).eq.2) then				! fevrier 2004
	q(j,i) =(vlarg(i)/deltar(i))					! fevrier 2004
	endif											! fevrier 2004
      if (j.eq.60.and.k(60,i).eq.3) then						! fevrier 2004
	q(j,i) =(vlarg(i)/deltar(i))+(vlength/deltacsc(i))		! fevrier 2004
	endif											! fevrier 2004

	enddo
	enddo


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c 10.0   calcul des couts unitaires (interpolation lineaire)    
c        + calcul des derivees   
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     cus(j,i) = coût unitaire soudeur de l'opération "j" du panneau i
c     cut(j,i) = coût unitaire tôlier de l'opération "j" du panneau i
c     parametres = valeur de la variable du panneau quand interpolation
c                  soudeur est nécessaire
c     parametret = valeur de la variable du panneau quand interpolation
c                  tolier est nécessaire
c     parametres1 = idem parametres mais tape à recouvrement
c     parametret1 = idem parametret mais tape à recouvrement
c     v1 et v2 = bornes dans laquelle se trouve la valeur de la variable
c     c1 et c2 = coûts unitaires correspondant à v1 et v2
c     t2(j,i) = dérivée des quantités relative à l'opération "j" du panneau i
c     il existe 9 différents t2 : t21, t22,..., t29 où le deuxième chiffre
c     correspond au id de la variable de conception (voir ii-p.60)
c     on rajoute en plus un indice 't' ou 's' suivant le fait que ce soit une
c     sensibilité relative au cutolier ou au cusoudeur (donc, on double le 
c     nombre de matrices t2)

c     write(8,*) 'calcul des couts unitaires (interpolation lineaire)'
c	write(8,*) '---------------------------------------------------'
      
	eps = 0.01

	do 781 i=1,neto  ! boucles générales
	do 781 j=1,nnn
    
      parametres  = 0.
      parametret  = 0.
	parametres1 = 0.
	parametret1 = 0.

c   soit 4 vecteurs spéciaux pour op. 19 et 20
c	memovars(j,1:ivsoudeur(j)) = variables(j,1:ivsoudeur(j)) 
c	memosou(j,1:ivsoudeur(j))  = soudeur(j,1:ivsoudeur(j))
c	memovart(j,1:ivtolier(j))  = variablet(j,1:ivtolier(j))
c	memotol(j,1:ivtolier(j))   = tolier(j,1:ivtolier(j))

	if (j.eq.9.or.j.eq.17.or.j.eq.18) then 
	parametres = vepaiss(i)*1000.   ! *1000 pour transformer en mm
	id = 1
	endif

	if (j.eq.12.or.j.eq.13) then
      parametret = vepaiss(i)*1000.
	id = 1
	endif

	if (j.eq.15) then
	parametret = vepaiss(i)*1000.
	id = 1
	endif

	if (j.eq.16) then
	parametret = vhar(i)*1000.
	id = 6
	endif

	if (j.eq.19.or.j.eq.20) then
c       if ((vepaiss(i)*1000.).ge.variables1(j,i).or.((vepaiss(i)*1000.).
c         *  lt.variables1(j,i).and.(vhar(i)*1000.).lt.variables(j,i))) then
	  if ((vepaiss(i)*1000.).le.variables(j,1)) then
c         !  on doit prendre soudeur comme vecteur des cu et variables
	    parametres = vepaiss(i)*1000.
	    id = 1
	  else
	    parametres = vhar(i)*1000.
	    id = 6
c         on doit prendre soudeur1 comme vecteur des cu et variables1
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
c     on doit prendre tolier comme vecteur des cu et variablet
	id = 6
	else                      ! tape encastrée
	parametres1 = vhar(i)*1000.
	parametret1 = vhar(i)*1000.
c     on doit prendre tolier1 comme vecteur des cu et variablet1
c     les 2 lignes suivantes effacent ce que contenait variablet et tolier

      memovart(1:ivtolier(j)) = variablet(j,1:ivtolier(j))
	memotol(1:ivtolier(j)) = tolier(j,1:ivtolier(j)) 
      variablet(j,1:ivtolier(j)) = variablet1(j,1:ivtolier(j))
	tolier(j,1:ivtolier(j)) = tolier1(j,1:ivtolier(j))
	id = 6
	endif
	endif

	if (j.eq.30.or.j.eq.31.or.j.eq.44.or.j.eq.45) then
	parametres = vhar(i)*1000.
	parametret = vhar(i)*1000.
	id = 6
	endif

c opérations 49 et 50 à implémenter !!!!!!!!!!!!!!!!!

      if (ivsoudeur(j).eq.1) then
	  cus(j,i) = soudeur(j,1) 
      endif
      if (ivtolier(j).eq.1) then
	  cut(j,i) = tolier(j,1)  
      endif


c 10.2  interpolation soudeur :
c -----------------------------
      if (parametres.ne.0) then

c	if (ivsoudeur(j).eq.2) then
c	   if (parametres.lt.variables(j,1)-pas*parametres) then
c	    cus(j,i) = soudeur(j,1)
cc         cus(j,i) = -(soudeur(j,2)-soudeur(j,1))/2*(variables(j,1)-parametres)
cc             *  /((variables(j,1)-parametres)**2+eps**2)**(.5)+(soudeur(j,1)
cc             * + soudeur(j,2))/2
c	   elseif (parametres.gt.variables(j,1)+pas*parametres) then
c	    cus(j,i) = soudeur(j,2)
cc         cus(j,i) = +(soudeur(j,2)-soudeur(j,1))/2*(parametres-variables
cc              *(j,1))/((parametres-variables(j,1))**2+eps**2)**(.5)+(soudeur(j,1)
cc              * + soudeur(j,2))/2
c         else           
c		 if(parametres.eq.variables(j,1)) then
c		   cus(j,i) = (soudeur(j,1) + soudeur(j,2))/2
c	     elseif(parametres.lt.variables(j,1)) then 
c	       cus(j,i) = soudeur(j,1)
c	     else
c	       cus(j,i) = soudeur(j,2)
c	     endif
c		 c2 = soudeur(j,2)
c	     c1 = soudeur(j,1)
c	     v2 = parametres*(1 + pas) !variables(j,2)
c	     v1 = parametres*(1 - pas) !variables(j,1)
cc	     if (id.eq.1) then
cc	     t21s(j,i) = (soudeur(j,2)-soudeur(j,1))/2*eps**2/((variables(
cc     *                  j,1)-parametres)**2+eps**2)**(3./2.)*1000.
cc	     endif
cc	     if (id.eq.6) then
cc	     t26s(j,i) = (soudeur(j,2)-soudeur(j,1))/2*eps**2/((variables(
cc     *                  j,1)-parametres)**2+eps**2)**(3./2.)*1000.
cc           endif
c	     if (id.eq.1) t21s(j,i) = 1000.*(c2-c1)/(v2-v1)
c	     if (id.eq.6) t26s(j,i) = 1000.*(c2-c1)/(v2-v1) 
c	   endif   
c         goto 800
c	endif
		
c     ordonner le vecteur variables et soudeur :
      do ijk=1,ivsoudeur(j)
	  c(1,ijk) = variables(j,ijk)
	  c(2,ijk) = soudeur(j,ijk)
	enddo
	
	call ord(2,ivsoudeur(j),c(1:2,1:ivsoudeur(j)))

	do ijk=1,ivsoudeur(j)
	  variables(j,ijk) = c(1,ijk)
	  soudeur(j,ijk) = c(2,ijk)
	enddo
	
	do ijk=1,ivsoudeur(j)
	  if (parametres.le.variables(j,1)+pas*parametres) then  !on est avant le premier point de
c         extrapolation ???????????                            ! donnee + pas
          cus(j,i) = soudeur(j,1)
	    if (id.eq.numv.and.k(j,i).gt.0) non = 100 + j  ! dans ce cas, message d'alerte !          
		if (parametres.gt.variables(j,1)-pas*parametres) then
	      v2 = parametres + pas*parametres
	      v1 = parametres - pas*parametres
	      call interpol(v2,c2,ivsoudeur(j),variables,soudeur,j,nnn)
	      call interpol(v1,c1,ivsoudeur(j),variables,soudeur,j,nnn)
	      if (id.eq.1) t21s(j,i) = 1000.*(c2-c1)/(v2-v1)
	      if (id.eq.6) t26s(j,i) = 1000.*(c2-c1)/(v2-v1)
	    endif		
		goto 800
        endif
	  if (parametres.le.variables(j,ijk)+pas*parametres.and. !on est entre 2 points de donnée 
     *      parametres.lt.variables(j,ivsoudeur(j))-pas*parametres) then !(+pas pour le deuxième point)
	    c2 = soudeur(j,ijk)
	    c1 = soudeur(j,ijk-1)
	    v2 = variables(j,ijk)
	    v1 = variables(j,ijk-1)
	    cus(j,i) = (c2 - c1)*(parametres - v1)/(v2 - v1) + c1
		if (id.eq.1) t21s(j,i) = 1000.*(c2 - c1)/(v2 - v1)
	    if (id.eq.6) t26s(j,i) = 1000.*(c2 - c1)/(v2 - v1)
	    if (parametres.gt.variables(j,ijk)-pas*parametres) then !on est à 1 point de donnée (au pas près)
		  v2 = parametres + pas*parametres
	      v1 = parametres - pas*parametres
	      call interpol(v2,c2,ivsoudeur(j),variables,soudeur,j,nnn)
	      call interpol(v1,c1,ivsoudeur(j),variables,soudeur,j,nnn)
	      if (id.eq.1) t21s(j,i) = 1000.*(c2-c1)/(v2-v1)
	      if (id.eq.6) t26s(j,i) = 1000.*(c2-c1)/(v2-v1)
	    endif 
	    goto 800
	  endif
	  if (parametres.ge.variables(j,ivsoudeur(j))-pas*parametres) then !on est après le dernier point - pas
c         extrapolation ???????????????
          cus(j,i) = soudeur(j,ivsoudeur(j))
	    if (id.eq.numv.and.k(j,i).gt.0) non = 200 + j
	    if(parametres.lt.variables(j,ivsoudeur(j))+pas*parametres)then !on est ds l'interv. autour du dernier point
	      v2 = parametres + pas*parametres
	      v1 = parametres - pas*parametres
	      call interpol(v2,c2,ivsoudeur(j),variables,soudeur,j,nnn)
	      call interpol(v1,c1,ivsoudeur(j),variables,soudeur,j,nnn)
	      if (id.eq.1) t21s(j,i) = 1000.*(c2-c1)/(v2-v1)
	      if (id.eq.6) t26s(j,i) = 1000.*(c2-c1)/(v2-v1)
	    endif
          goto 800
        endif
	enddo
800   continue   
      endif  ! fin interpolation soudeur


c 10.3  interpolation tolier :
c --------------------------------
      if (parametret.ne.0) then

c	if (ivtolier(j).eq.2) then
c	   if (parametret.lt.variablet(j,1)-pas*parametret) then
c	    cut(j,i) = tolier(j,1)
cc         cut(j,i) = -(tolier(j,2)-tolier(j,1))/2*(variablet(j,1)-parametret)
cc           *  /((variablet(j,1)-parametret)**2+eps**2)**(.5)+(tolier(j,1) + 
cc           *    tolier(j,2))/2	   
c	   elseif (parametret.gt.variablet(j,1)+pas*parametret) then
c	    cut(j,i) = tolier(j,2)
cc         cut(j,i) = +(tolier(j,2)-tolier(j,1))/2*(parametret-variablet(j,1))
cc            * /((parametret-variablet(j,1))**2+eps**2)**(.5)+(tolier(j,1) + 
cc            *  tolier(j,2))/2	   	   
c	   else  !si on est au saut (au pas près)
c	     if (parametret.eq.variablet(j,1)) then
c             cut(j,i) = (tolier(j,1) + tolier(j,2))/2
c	     elseif (parametret.lt.variablet(j,1)) then
c	       cut(j,i) = tolier(j,1)
c	     else
c	       cut(j,i) = tolier(j,2)
c	     endif
c		 c2 = tolier(j,2)
c	     c1 = tolier(j,1)
c	     v2 = parametret*(1 + 1/20.) !variablet(j,2)
c	     v1 = parametret*(1 - 1/20.) !variablet(j,1)
cc	     if (id.eq.1) then
cc	     t21t(j,i) = (tolier(j,2)-tolier(j,1))/2*eps**2/((variablet(j,
cc     *                  1)-parametret)**2+eps**2)**(3./2.)*1000.
cc	     endif
cc	     if (id.eq.6) then
cc	     t26t(j,i) = (tolier(j,2)-tolier(j,1))/2*eps**2/((variablet(j,
cc     *                  1)-parametret)**2+eps**2)**(3./2.)*1000.
cc           endif
c	     if (id.eq.1) t21t(j,i) = 1000.*(c2-c1)/(v2-v1)
c	     if (id.eq.6) t26t(j,i) = 1000.*(c2-c1)/(v2-v1)    
c	   endif
c        goto 805
c	endif

c     ordonner le vecteur variablet et tolier :
	do ijk=1,ivtolier(j)
	  c(1,ijk) = variablet(j,ijk)
	  c(2,ijk) = tolier(j,ijk)
	enddo

	call ord(2,ivtolier(j),c(1:2,1:ivtolier(j)))

	do ijk=1,ivtolier(j)
	  variablet(j,ijk) = c(1,ijk)
	  tolier(j,ijk) = c(2,ijk)
	enddo

	do ijk=1,ivtolier(j)
	  if (parametret.le.variablet(j,1)+parametret*pas) then
c         extrapolation ???????????
          cut(j,i) = tolier(j,1)
	    if (id.eq.numv.and.k(j,i).gt.0) non = 100 + j
	    if (parametret.gt.variablet(j,1)-pas*parametret) then
	      v2 = parametret + pas*parametret
	      v1 = parametret - pas*parametret
	      call interpol(v2,c2,ivtolier(j),variablet,tolier,j,nnn)
	      call interpol(v1,c1,ivtolier(j),variablet,tolier,j,nnn)
	      if (id.eq.1) t21t(j,i) = 1000.*(c2-c1)/(v2-v1)
	      if (id.eq.6) t26t(j,i) = 1000.*(c2-c1)/(v2-v1)
	    endif
          goto 805
        endif
	  if (parametret.le.variablet(j,ijk)+pas*parametret.and.
     *      parametret.lt.variablet(j,ivtolier(j))-pas*parametret) then
	    c2 = tolier(j,ijk)
	    c1 = tolier(j,ijk-1)
	    v2 = variablet(j,ijk)
	    v1 = variablet(j,ijk-1)
	    cut(j,i) = (c2 - c1)*(parametret - v1)/(v2 - v1) + c1
	    if (id.eq.1) t21t(j,i) = 1000.*(c2-c1)/(v2-v1)
	    if (id.eq.6) t26t(j,i) = 1000.*(c2-c1)/(v2-v1)
          if (parametret.gt.variablet(j,ijk)-pas*parametret) then
		  v2 = parametret + pas*parametret
	      v1 = parametret - pas*parametret
	      call interpol(v2,c2,ivtolier(j),variablet,tolier,j,nnn)
	      call interpol(v1,c1,ivtolier(j),variablet,tolier,j,nnn)
	      if (id.eq.1) t21t(j,i) = 1000.*(c2-c1)/(v2-v1)
	      if (id.eq.6) t26t(j,i) = 1000.*(c2-c1)/(v2-v1)
	    endif
	    goto 805
	  endif
	  if (parametret.ge.variablet(j,ivtolier(j))-pas*parametret) then !on est après le dernier point - pas
c         extrapolation ???????????????
          cut(j,i) = tolier(j,ivtolier(j))
	    if (id.eq.numv.and.k(j,i).gt.0) non = 200 + j
	    if (parametret.lt.variablet(j,ivtolier(j))+pas*parametret)then !on est ds l'interv. autour du dernier point
	      v2 = parametret + pas*parametret
	      v1 = parametret - pas*parametret
	      call interpol(v2,c2,ivtolier(j),variablet,tolier,j,nnn)
	      call interpol(v1,c1,ivtolier(j),variablet,tolier,j,nnn)
	      if (id.eq.1) t21t(j,i) = 1000.*(c2-c1)/(v2-v1)
	      if (id.eq.6) t26t(j,i) = 1000.*(c2-c1)/(v2-v1)
	    endif
          goto 805
	  endif
	enddo
805   continue   

      endif  !  fin interpolation tolier


c 10.4  interpolation soudeur (cas tape encastrée) :
c ---------------------------------------------------
      if (parametres1.ne.0) then

c     ordonner le vecteur variables1 et soudeur :
      do ijk=1,ivsoudeur(j)
	c(1,ijk) = variables1(j,ijk)
	c(2,ijk) = soudeur(j,ijk)
	enddo

	call ord(2,ivsoudeur(j),c(1:2,1:ivsoudeur(j)))

	do ijk=1,ivsoudeur(j)
	variables1(j,ijk) = c(1,ijk)
	soudeur(j,ijk) = c(2,ijk)
	enddo

      do ijk=1,ivsoudeur(j)
	c(1,ijk) = variables1(j,ijk)
	c(2,ijk) = soudeur(j,ijk)
	enddo

	call ord(2,ivsoudeur(j),c(1:2,1:ivsoudeur(j)))

	do ijk=1,ivsoudeur(j)
	variables1(j,ijk) = c(1,ijk)
	soudeur(j,ijk) = c(2,ijk)
	enddo

	do ijk=1,ivsoudeur(j)
	  if (parametres1.le.variables1(j,1)+parametres1*pas) then
c         extrapolation ???????????
          cus(j,i) = soudeur(j,1)
	    if (id.eq.numv.and.k(j,i).gt.0) non = 100 + j
	    if (parametres1.lt.variables1(j,1)-parametres1*pas) then
	      v2 = parametres1 + pas*parametres1
	      v1 = parametres1 - pas*parametres1
	      call interpol(v2,c2,ivsoudeur(j),variables1,soudeur,j,nnn)
	      call interpol(v1,c1,ivsoudeur(j),variables1,soudeur,j,nnn)
	      if (id.eq.1) t21s(j,i) = 1000.*(c2-c1)/(v2-v1)
	      if (id.eq.6) t26s(j,i) = 1000.*(c2-c1)/(v2-v1)
	    endif
          goto 810
        endif
	  if(parametres1.le.variables1(j,ijk)+parametres1*pas.and.
     *    parametres1.lt.variables1(j,ivsoudeur(j))-parametres1*pas)then
	    c2 = soudeur(j,ijk)
	    c1 = soudeur(j,ijk-1)
	    v2 = variables1(j,ijk)
	    v1 = variables1(j,ijk-1)
	    cus(j,i) = (c2 - c1)*(parametres1 - v1)/(v2 - v1)
	    if (id.eq.1) t21s(j,i) = 1000.*(c2-c1)/(v2-v1)
	    if (id.eq.6) t26s(j,i) = 1000.*(c2-c1)/(v2-v1)
	    if (parametres1.gt.variables1(j,ijk)-parametres1*pas) then
	      v2 = parametres1 + pas*parametres1
	      v1 = parametres1 - pas*parametres1
	      call interpol(v2,c2,ivsoudeur(j),variables1,soudeur,j,nnn)
	      call interpol(v1,c1,ivsoudeur(j),variables1,soudeur,j,nnn)
	      if (id.eq.1) t21s(j,i) = 1000.*(c2-c1)/(v2-v1)
	      if (id.eq.6) t26s(j,i) = 1000.*(c2-c1)/(v2-v1)
	    endif
	    goto 810
	  endif
	  if (parametres1.ge.variables1(j,ivsoudeur(j))-parametres1*pas)
     *                                                              then
c         extrapolation ???????????????
          cus(j,i) = soudeur(j,ivsoudeur(j))
	    if (id.eq.numv.and.k(j,i).gt.0) non = 200 + j
	    if (parametres1.lt.variables1(j,ivsoudeur(j))-parametres1*pas)
     *                                                              then
	      v2 = parametres1 + pas*parametres1
	      v1 = parametres1 - pas*parametres1
	      call interpol(v2,c2,ivsoudeur(j),variables1,soudeur,j,nnn)
	      call interpol(v1,c1,ivsoudeur(j),variables1,soudeur,j,nnn)
	      if (id.eq.1) t21s(j,i) = 1000.*(c2-c1)/(v2-v1)
	      if (id.eq.6) t26s(j,i) = 1000.*(c2-c1)/(v2-v1)
          endif
          goto 810
        endif
	enddo
810   continue   
      endif  !fin interpolation soudeur (cas tape encastrée)


c 10.5 interpolation tolier (cas tape encastrée) :
c--------------------------------------------------
      if (parametret1.ne.0) then

c      ordonner le vecteur variablet1 et tolier :
	 do ijk=1,ivtolier(j)
	   c(1,ijk) = variablet1(j,ijk)
	   c(2,ijk) = tolier(j,ijk)
	 enddo

	 call ord(2,ivtolier(j),c(1:2,1:ivtolier(j)))

	 do ijk=1,ivtolier(j)
	   variablet1(j,ijk) = c(1,ijk)
	   tolier(j,ijk) = c(2,ijk)
	 enddo

	 do ijk=1,ivtolier(j)
	   if (parametret1.le.variablet1(j,1)+parametret1*pas) then
c          extrapolation ???????????
           cut(j,i) = tolier(j,1)
	     if (id.eq.numv.and.k(j,i).gt.0) non = 100 + j
	     if (parametret1.gt.variablet1(j,1)-parametret1*pas) then
	       v2 = parametret1 + pas*parametret1
	       v1 = parametret1 - pas*parametret1
	       call interpol(v2,c2,ivtolier(j),variablet1,tolier,j,nnn)
	       call interpol(v1,c1,ivtolier(j),variablet1,tolier,j,nnn)
	       if (id.eq.1) t21t(j,i) = 1000.*(c2-c1)/(v2-v1)
	       if (id.eq.6) t26t(j,i) = 1000.*(c2-c1)/(v2-v1)
	     endif
           goto 815
         endif
	   if(parametret1.le.variablet1(j,ijk)+parametret1*pas.and.
     *    parametret1.lt.variablet1(j,ivtolier(j))-parametret1*pas) then
	     c2 = tolier(j,ijk)
	     c1 = tolier(j,ijk-1)
	     v2 = variablet1(j,ijk)
	     v1 = variablet1(j,ijk-1)
	     cut(j,i) = (c2 - c1)*(parametret1 - v1)/(v2 - v1) + c1	
		 if (id.eq.1) t21t(j,i) = 1000.*(c2-c1)/(v2-v1)
	     if (id.eq.6) t26t(j,i) = 1000.*(c2-c1)/(v2-v1) 
		 if (parametret1.gt.variablet1(j,ijk)-parametret1*pas) then
		   v2 = parametret1 + pas*parametret1
	       v1 = parametret1 - pas*parametret1
	       call interpol(v2,c2,ivtolier(j),variablet1,tolier,j,nnn)
	       call interpol(v1,c1,ivtolier(j),variablet1,tolier,j,nnn)
	       if (id.eq.1) t21t(j,i) = 1000.*(c2-c1)/(v2-v1)
	       if (id.eq.6) t26t(j,i) = 1000.*(c2-c1)/(v2-v1)	 	
	     endif
	     goto 815
	   endif
	   if (parametret1.ge.variablet1(j,ivtolier(j))-parametret1*pas)
     *                                                              then
c          extrapolation ???????????????
           cut(j,i) = tolier(j,ivtolier(j))
	     if (id.eq.numv.and.k(j,i).gt.0) non = 200 + j
	     if (parametret1.lt.variablet1(j,ivtolier(j))+parametret1*pas)
     *                                                              then
	       v2 = parametret1 + pas*parametret1
	       v1 = parametret1 - pas*parametret1
	       call interpol(v2,c2,ivtolier(j),variablet1,tolier,j,nnn)
	       call interpol(v1,c1,ivtolier(j),variablet1,tolier,j,nnn)
	       if (id.eq.1) t21t(j,i) = 1000.*(c2-c1)/(v2-v1)
	       if (id.eq.6) t26t(j,i) = 1000.*(c2-c1)/(v2-v1)
           endif
           goto 815
         endif
	 enddo
815    continue   
      endif  !fin interpolation tolier (cas tape encastrée)


c     interpolation sur les opérations 49 et 50
c     -----------------------------------------
c      
c	if (j.eq.49) then
c    
cc     tolier
c	do ijk=1,ivtolier(j)
c	  c(1,ijk) = variablet(j,ijk)
c	  c(2,ijk) = variablet1(j,ijk)
c	  if (npabouts(i).eq.1) c(3,ijk) = tolier(j,ijk)   !cad soudure à plat
c       if (npabouts(i).eq.2) c(3,ijk) = tolier1(j,ijk)  !cad soudure verticale
c	  if (npabouts(i).eq.3) c(3,ijk) = tolier2(j,ijk)  !cad soudure horizontale
c	enddo
c	call ord(3,ivtolier(j),c(1:3,1:ivtolier(j)))
c	call annuld(num,35) !!!!!!!!à changer
c	call ord2(3,ivtolier(j),c(1:3,1:ivtolier(j)),num)
c	do ijk=1,ivtolier(j)
c       variablet(j,ijk) = c(1,ijk)
c	  variablet1(j,ijk) = c(2,ijk)
c	  if (npabouts(i).eq.1) tolier(j,ijk) = c(3,ijk)   !cad soudure à plat
c       if (npabouts(i).eq.2) tolier1(j,ijk) = c(3,ijk)  !cad soudure verticale
c	  if (npabouts(i).eq.3) tolier2(j,ijk) = c(3,ijk)  !cad soudure horizontale
c	enddo
c	
c	if (vhar(i)*1000.le.variablet(j,1)) then !
c	  call interpol2(num(1),npabouts(i),vear(i),cut(j,i),
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
c       call interpol2(kk,npabouts(i),vear(i),cut(j,i),
c   *                   variablet1(j,k1-kk+1),tolier(j,k1-kk+1),
c    *                   tolier1(j,k1-kk+1),tolier2(j,k1-kk+1),j,nnn) 
c	    goto 816
c	  endif
c       k1=k1+1
c	enddo
c
c	v1 = variablet(j,k1-1)
c	v2 = variablet(j,k1)
c	kk=1
c	do while (k1-1-kk.ge.1.and.num(k1-1-kk).gt.1)
c	  kk=kk+1
c	enddo
c	call interpol2(kk,npabouts(i),vear(i),c1,
c   *               variablet1(j,k1-kk),tolier(j,k1-kk),
c    *               tolier1(j,k1-kk),tolier2(j,k1-kk),j,nnn)
c	call interpol2(num(k1),npabouts(i),vear(i),c2,
c   *               variablet1(j,k1),tolier(j,k1),
c    *               tolier1(j,k1),tolier2(j,k1),j,nnn)
c	cut(j,i)= (c2 - c1)*(vhar(i)*1000 - v1)/(v2 - v1) + c1
cc     fin tolier
c
cc     soudeur
c816 	endif
c
c
c     if (j.eq.50) then
c
cc   tolier
c    if (npabouts(i).eq.1) cut(j,i) = tolier(j,1)   !cad soudure à plat
c     if (npabouts(i).eq.2) cut(j,i) = tolier1(j,1)  !cad soudure verticale
c	if (npabouts(i).eq.3) cut(j,i) = tolier2(j,1)  !cad soudure horizontale
cc   fin tolier

cc    soudeur
c     do ijk=1,ivsoudeur(j)
c	  c(1,ijk) = variables(j,ijk)
c	  c(2,ijk) = variables1(j,ijk)
c	  if (npabouts(i).eq.1) c(3,ijk) = soudeur(j,ijk)   !cad soudure à plat
c       if (npabouts(i).eq.2) c(3,ijk) = soudeur1(j,ijk)  !cad soudure verticale
c	  if (npabouts(i).eq.3) c(3,ijk) = soudeur2(j,ijk)  !cad soudure horizontale
c	enddo
c	call ord(3,ivsoudeur(j),c(1:3,1:ivsoudeur(j)))
c	call annuld(num,35) !!!!!!!!à changer
c	call ord2(3,ivsoudeur(j),c(1:3,1:ivsoudeur(j)),num)
c	do ijk=1,ivsoudeur(j)
c       variables(j,ijk) = c(1,ijk)
c	  variables1(j,ijk) = c(2,ijk)
c	  if (npabouts(i).eq.1) soudeur(j,ijk) = c(3,ijk)   !cad soudure à plat
c       if (npabouts(i).eq.2) soudeur1(j,ijk) = c(3,ijk)  !cad soudure verticale
c	  if (npabouts(i).eq.3) soudeur2(j,ijk) = c(3,ijk)  !cad soudure horizontale
c	enddo
c	
c	if (vhar(i)*1000.le.variables(j,1)) then !
c	  call interpol2(num(1),npabouts(i),vlsr(i),cus(j,i),
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
c        call interpol2(kk,npabouts(i),vlsr(i),cus(j,i),
c     *                   variables1(j,k1-kk+1),soudeur(j,k1-kk+1),
c    *                   soudeur1(j,k1-kk+1),soudeur2(j,k1-kk+1),j,nnn) 
c	    goto 817
c	  endif
c        k1=k1+1
c	enddo
c
c	v1 = variables(j,k1-1)
c	v2 = variables(j,k1)
c	kk=1
c	do while (k1-1-kk.ge.1.and.num(k1-1-kk).gt.1)
c	  kk=kk+1
c	enddo
c	call interpol2(kk,npabouts(i),vlsr(i),c1,
c     *              variables1(j,k1-kk:k1-1),soudeur(j,k1-kk:k1-1),
c     *              soudeur1(j,k1-kk:k1-1),soudeur2(j,k1-kk:k1-1),j,nnn)
c	call interpol2(num(k1),npabouts(i),vlsr(i),c2,
c     *    variables1(j,k1:k1+num(k1)-1),soudeur(j,k1:k1+num(k1)-1),
c     *    soudeur1(j,k1:k1+num(k1)-1),soudeur2(j,k1:k1+num(k1)-1),j,nnn)
c	cus(j,i)= (c2 - c1)*(vhar(i)*1000 - v1)/(v2 - v1) + c1
cc     fin soudeur
c      endif
c      
c817   continue

c 10.6  interpolation sur les couts des soudures (id) 
c -------------------------------------------------------------
      if (j.eq.21.or.j.eq.22.or.j.eq.23.or.j.eq.36.or.j.eq.37.
     *            or.j.eq.47.or.j.eq.48.or.j.eq.51)  then

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
	  v2=vgorsoud(iijj,i)*(1+pas)
	  v1=vgorsoud(iijj,i)*(1-pas)
c	  if(idsoudures(iijj,i).le.13) then
c	  t21t(i,j)=0.
c	  t21s(i,j)=0.
c	  endif
	else
	  if(idsoudures(iijj,i).le.13) then
	    parat=0.5+abs(min(vepaiss(i),veac(i))*0.35 *coeff)
	    paras=0.5+abs(min(vepaiss(i),veac(i))*0.35 *coeff)
	    v2=0.5+abs(min(vepaiss(i)*(1+pas),veac(i)*(1+pas))*0.35*coeff)
	    v1=0.5+abs(min(vepaiss(i)*(1-pas),veac(i)*(1-pas))*0.35*coeff)
	  else
	    parat=vepaiss(i) *coeff
	    paras=vepaiss(i) *coeff
	    v2=vepaiss(i)*(1+pas) *coeff
	    v1=vepaiss(i)*(1-pas) *coeff
	  endif
	endif

	idso=idsoudures(iijj,i)

c ----------------------------------------------
	do 782 kst=1,noid  

	if(iid(kst).eq.idso) then

	if(vst(kst,1).eq.0.and.vst(kst,2).eq.0.and.vst(kst,3).eq.0)then !càd si 1 seule donnée 
	  cut(j,i)=ts(kst,1)                                            !(pas d'interpolation)

      else
c	  v2 = parat + pas*parat
c	  v1 = parat - pas*parat
	  call interpol(v2,c2,is(kst,irid(kst)),vst,ts,kst,noid)
	  call interpol(v1,c1,is(kst,irid(kst)),vst,ts,kst,noid)
        call interpol(parat,cut(j,i),is(kst,irid(kst)),vst,ts,kst,noid)
c        cut(j,i) = (c2-c1)*(parat-v1)/(v2-v1) + c1
        if(vgorsoud(iijj,i).eq.0) then
	    
          if (idsoudures(iijj,i).le.13) then
	      coeff2 = 0.35*coeff
	    else
	      coeff2 = coeff
	    endif
c          dv2 = (1+pas)*coeff2
c	    dv1 = (1-pas)*coeff2
c	    call interpol(v2+1e-5,dc22,is(kst,irid(kst)),vst,ts,kst,noid)
c	    call interpol(v2-1e-5,dc21,is(kst,irid(kst)),vst,ts,kst,noid)
c	    dc2 = (dc22 - dc21)/2.e-5
c	    call interpol(v1+1e-5,dc12,is(kst,irid(kst)),vst,ts,kst,noid)
c	    call interpol(v1-1e-5,dc11,is(kst,irid(kst)),vst,ts,kst,noid)
c		dc1 = (dc12 - dc11)/2.e-5 
		  
c          dt2 = (((dc2-dc1)*(parat-v1)+(c2-c1)*(coeff2-dv1))*(v2-v1) -
c     *	       (c2-c1)*(parat-v1)*(dv2-dv1)) /
c     *	       (v2-v1)**2 + dc1
c	    dt2 = (c2-c1)/(v2-v1)*coeff2
	    if(idsoudures(iijj,i).le.13) then    
	      if (vepaiss(i).lt.veac(i)) then
		    t21t(j,i) = (c2-c1)/(2*vepaiss(i)*pas)  !dt2 !(c2 - c1)/(v2 - v1)*0.35 *coeff
	      else
	        t23t(j,i) = (c2-c1)/(2*veac(i)*pas)  !dt2 !(c2 - c1)/(v2 - v1)*0.35 *coeff
            endif
	    else
            t21t(j,i) = (c2-c1)/(2*vepaiss(i)*pas)    !dt2 !(c2 - c1)/(v2 - v1) *coeff
	    endif
	  endif

c	do kkjj=1,is(kst,irid(kst))-1
c	 if((vst(kst,kkjj)-parat*pas.le.parat.or.
c    *         vst(kst,kkjj)+parat*pas.ge.parat).and.kkjj.ne.1) then !on est dans un interv.de
c	   cut(j,i)=ts(kst,kkjj)                                       !pas autour d'une donnée
c        if(vgorsoud(iijj,i).eq.0) then
c	     v2 = parat + pas*parat
c	     v1 = parat - pas*parat
c	     call interpol(v2,c2,is(kst,irid(kst)),vst,ts,j,nnn)
c	     call interpol(v1,c1,is(kst,irid(kst)),vst,ts,j,nnn)
c	     if(idsoudures(iijj,i).le.13) then
c	       if (vepaiss(i).lt.veac(i)) then
c		     t21t(j,i) = (c2 - c1)/(v2 - v1)*0.35 *coeff
c	       else
c	         t23t(j,i) = (c2 - c1)/(v2 - v1)*0.35 *coeff
c            endif
c	     else
c            t21t(j,i) = (c2 - c1)/(v2 - v1) *coeff
c	     endif
c	   endif
c	 endif
c	 if(vst(kst,kkjj)+parat*pas.lt.parat.and.     !on est entre deux points de donnée
c    *    vst(kst,kkjj+1)-parat*pas.gt.parat) then  !en tenant compte du pas
c	   c2 = ts(kst,kkjj+1)
c	   c1 = ts(kst,kkjj)
c        v2 = vst(kst,kkjj+1)
c	   v1 = vst(kst,kkjj)
c	   cut(j,i) = c1 + (c2 - c1)*(paras - v1)/(v2 - v1)
c        if(vgorsoud(iijj,i).eq.0) then
c	     if(idsoudures(iijj,i).le.13) then
c	       if (vepaiss(i).lt.veac(i)) then
c		     t21t(j,i) = (c2 - c1)/(v2 - v1)*0.35 *coeff
c	       else
c	         t23t(j,i) = (c2 - c1)/(v2 - v1)*0.35 *coeff
c            endif
c	     else
c             t21t(j,i) = (c2 - c1)/(v2 - v1) *coeff
c	     endif
c	   endif
c	 endif
c	enddo
c
c	if(vst(kst,1)+parat*pas.ge.parat) then       !on est avant le premier point de donnee + pas
c	  cut(j,i)=ts(kst,1)
c       if (vst(kst,1)-parat*pas.le.parat) then
c	    if(vgorsoud(iijj,i).eq.0) then
c	      v2 = parat + pas*parat
c	      v1 = parat - pas*parat
c	      call interpol(v2,c2,is(kst,irid(kst)),vst,ts,j,nnn)
c	      call interpol(v1,c1,is(kst,irid(kst)),vst,ts,j,nnn)
c	      if(idsoudures(iijj,i).le.13) then
c	        if (vepaiss(i).lt.veac(i)) then
c		      t21t(j,i) = (c2 - c1)/(v2 - v1)*0.35 *coeff
c	        else
c	          t23t(j,i) = (c2 - c1)/(v2 - v1)*0.35 *coeff
c             endif
c	      else
c             t21t(j,i) = (c2 - c1)/(v2 - v1) *coeff
c	      endif
c	    endif
c	  endif
c	endif
c	if(vst(kst,is(kst,irid(kst)))-parat*pas.le.parat) then !on est après le dernier point de donne - pas
c	  cut(j,i)=ts(kst,is(kst,irid(kst)))
c	  if (vst(kst,is(kst,irid(kst)))+parat*pas.ge.parat) then
c	    if(vgorsoud(iijj,i).eq.0) then
c	      v2 = parat + pas*parat
c	      v1 = parat - pas*parat
c	      call interpol(v2,c2,is(kst,irid(kst)),vst,ts,j,nnn)
c	      call interpol(v1,c1,is(kst,irid(kst)),vst,ts,j,nnn)
c	      if(idsoudures(iijj,i).le.13) then
c	        if (vepaiss(i).lt.veac(i)) then
c		      t21t(j,i) = (c2 - c1)/(v2 - v1)*0.35 *coeff
c	        else
c	          t23t(j,i) = (c2 - c1)/(v2 - v1)*0.35 *coeff
c             endif
c	      else
c             t21t(j,i) = (c2 - c1)/(v2 - v1) *coeff
c	      endif
c	    endif
c	  endif
c	endif
	
	endif
c     -----------------------
      if(vss(kst,1).eq.0.and.vss(kst,2).eq.0.and.vss(kst,3).eq.0)then
	  cus(j,i)=ss(kst,1)
	
      else

c        v2 = paras + pas*paras
c        v1 = paras - pas*paras
	  call interpol(v2,c2,is(kst,irid(kst)),vss,ss,kst,noid)
	  call interpol(v1,c1,is(kst,irid(kst)),vss,ss,kst,noid)
	  call interpol(paras,cus(j,i),is(kst,irid(kst)),vss,ss,kst,noid)
c        cus(j,i) = (c2-c1)*(paras-v1)/(v2-v1) + c1
        if(vgorsoud(iijj,i).eq.0) then
  	    
	    if (idsoudures(iijj,i).le.13) then
	      coeff2 = 0.35*coeff
	    else
	      coeff2= coeff
	    endif
c	    dv2 = (1+pas)*coeff2
c	    dv1 = (1-pas)*coeff2
c	    call interpol(v2+1e-5,dc22,is(kst,irid(kst)),vss,ss,kst,noid)
c	    call interpol(v2-1e-5,dc21,is(kst,irid(kst)),vss,ss,kst,noid)
c	    dc2 = (dc22 - dc21)/2.e-5
c	    call interpol(v1+1e-5,dc12,is(kst,irid(kst)),vss,ss,kst,noid)
c	    call interpol(v1-1e-5,dc11,is(kst,irid(kst)),vss,ss,kst,noid)
c		dc1 = (dc12 - dc11)/2.e-5 
		  
c          dt2 = (((dc2-dc1)*(paras-v1)+(c2-c1)*(coeff2-dv1))*(v2-v1) -
c     *	       (c2-c1)*(paras-v1)*(dv2-dv1)) /
c     *	       (v2-v1)**2 + dc1
c	    dt2 = (c2-c1)/(v2-v1)*coeff2
ccc	    dt2 = (c2-c1)/(2*v)
	    if(idsoudures(iijj,i).le.13) then
	      if (vepaiss(i).lt.veac(i)) then
	        t21s(j,i) = (c2-c1)/(2*vepaiss(i)*pas) !dt2 !(c2 - c1)/(v2 - v1)*0.35 *coeff
            else
	        t23s(j,i) = (c2-c1)/(2*veac(i)*pas) !dt2 !(c2 - c1)/(v2 - v1)*0.35 *coeff
            endif
	    else
            t21s(j,i) = (c2-c1)/(2*vepaiss(i)*pas)  !dt2 !(c2 - c1)/(v2 - v1) *coeff
	    endif
	  endif

c	do kkjj=1,is(kst,irid(kst))-1
c	  if(vss(kst,kkjj)-paras*pas.le.paras.and.
c    *      vss(kst,kkjj)+paras*pas.ge.paras.and.kkjj.ne.1.and.
c   *      vss(kst,kkjj-1).lt.paras) then
c	    cus(j,i)=ss(kst,kkjj)
c	    if(vgorsoud(iijj,i).eq.0) then
c	      v2 = paras + pas*paras
c	      v1 = paras - pas*paras
c	      call interpol(v2,c2,is(kst,irid(kst)),vss,ss,j,nnn)
c	      call interpol(v1,c1,is(kst,irid(kst)),vss,ss,j,nnn)
c	      if(idsoudures(iijj,i).le.13) then
c	        if (vepaiss(i).lt.veac(i)) then
c		      t21s(j,i) = (c2 - c1)/(v2 - v1)*0.35 *coeff
c	        else
c	          t23s(j,i) = (c2 - c1)/(v2 - v1)*0.35 *coeff
c             endif
c	      else
c             t21s(j,i) = (c2 - c1)/(v2 - v1) *coeff
c	      endif
c	    endif
c	  endif
c	  if(vss(kst,kkjj)+paras*pas.lt.paras.and.     !on est entre deux points de donnee
c    *     vss(kst,kkjj+1)-paras*pas.gt.paras) then  !en tenant compte du pas
c	    c2 = ss(kst,kkjj+1)
c	    c1 = ss(kst,kkjj)
c         v2 = vss(kst,kkjj+1)
c	    v1 = vss(kst,kkjj)
c	    cus(j,i) = c1 + (c2 - c1)*(paras - v1)/(v2 - v1)
c         if(vgorsoud(iijj,i).eq.0) then
c	      if(idsoudures(iijj,i).le.13) then
c	        if (vepaiss(i).lt.veac(i)) then
c		      t21s(j,i) = (c2 - c1)/(v2 - v1)*0.35 *coeff
c	        else
c	          t23s(j,i) = (c2 - c1)/(v2 - v1)*0.35 *coeff
c             endif
c	      else
c             t21s(j,i) = (c2 - c1)/(v2 - v1) *coeff
c	      endif
c	    endif
c	  endif
c	enddo
c
c	if(vss(kst,1)+paras*pas.ge.paras) then  !on est avant le premier point de donne + pas
c	  cus(j,i)=ss(kst,1)
c	  if (vss(kst,1)-parat*pas.le.paras) then
c         if(vgorsoud(iijj,i).eq.0) then
c	      v2 = paras + pas*paras
c	      v1 = paras - pas*paras
c	      call interpol(v2,c2,is(kst,irid(kst)),vss,ss,j,nnn)
c	      call interpol(v1,c1,is(kst,irid(kst)),vss,ss,j,nnn)
c	      if(idsoudures(iijj,i).le.13) then
c	        if (vepaiss(i).lt.veac(i)) then
c		      t21s(j,i) = (c2 - c1)/(v2 - v1)*0.35 *coeff
c	        else
c	          t23s(j,i) = (c2 - c1)/(v2 - v1)*0.35 *coeff
c             endif
c	      else
c             t21s(j,i) = (c2 - c1)/(v2 - v1) *coeff
c	      endif
c	    endif
c	  endif
c	endif
c	if(vss(kst,is(kst,irid(kst)))-parat*pas.le.paras) then !on est après le premier point de donnee - pas
c	  cus(j,i)=ss(kst,is(kst,irid(kst)))
c       if (vss(kst,is(kst,irid(kst)))+parat*pas.ge.paras) then
c	    ind = is(kst,irid(kst))
c	    if(vgorsoud(iijj,i).eq.0) then
c	      v2 = paras + pas*paras
c	      v1 = paras - pas*paras
c	      call interpol(v2,c2,is(kst,irid(kst)),vss,ss,j,nnn)
c	      call interpol(v1,c1,is(kst,irid(kst)),vss,ss,j,nnn)
c	      if(idsoudures(iijj,i).le.13) then
c	        if (vepaiss(i).lt.veac(i)) then
c		      t21s(j,i) = (c2 - c1)/(v2 - v1)*0.35 *coeff
c	        else
c	          t23s(j,i) = (c2 - c1)/(v2 - v1)*0.35 *coeff
c             endif
c	      else
c             t21s(j,i) = (c2 - c1)/(v2 - v1) *coeff
c	      endif
c	    endif
c	  endif
c	endif

	endif

	endif 

 782  continue  ! do 782 kst=1,noid

c	continue
c	write(*,*) cut(j,i),cus(j,i)
	endif   !    if (j.eq.21.or.j.eq.22.or.j.e


c ---------------
c     variables(j,1:ivsoudeur(j)) = memovars(j,1:ivsoudeur(j))
c	soudeur  (j,1:ivsoudeur(j)) = memosou (j,1:ivsoudeur(j))
c	variablet(j,1:ivtolier(j))  = memovart(j,1:ivtolier(j))
c	tolier   (j,1:ivtolier(j))  = memotol (j,1:ivtolier(j))
	
	if (j.eq.19.or.j.eq.20) then
c       if ((vepaiss(i)*1000.).ge.variables(j,i).or.
c          ((vepaiss(i)*1000.).lt.variables(j,i).and.
c           (vhar(i)   *1000.).lt.variables1(j,i))) then
        if ((vepaiss(i)*1000.).le.memovars(1)) then
          continue
	  else
          variables(j,1:ivsoudeur(j)) = memovars(1:ivsoudeur(j))
	    soudeur(j,1:ivsoudeur(j))   = memosou(1:ivsoudeur(j))
	  endif
	endif

      if (j.eq.25.or.j.eq.26.or.j.eq.39.or.j.eq.40) then
	 if (nttapes(i).eq.1) then ! tape à recouvrement
	   continue
	 else                      ! tape encastrée
	   parametres1 = vhar(i)*1000.
	   parametret1 = vhar(i)*1000.
         variablet(j,1:ivtolier(j)) = memovart(1:ivtolier(j))
	   tolier(j,1:ivtolier(j))    = memotol(1:ivtolier(j))       
	  endif
	endif


 781  continue  ! fin boucle principale


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccc            calcul des sensibilites t1              ccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     t1(j,i) = dérivée des quantités relative à l'opération "j" du panneau i
c     il existe 9 différents t1 : t11, t12,..., t19 où le deuxième chiffre
c     correspond au id de la variable de conception (voir ii-p.60)

      if (icout.eq.3) goto 783

      do i=1,neto
ccccccccccccccccccccccc
      do j=1,nnn
      if (j.eq.1) then
	t15(j,i) = -(vlarg(i)/deltar(i))*(vlength/deltacsc(i)**2)
	t19(j,i) = -(vlarg(i)/deltar(i)**2)*(vlength/deltacsc(i))
	endif
	if (j.eq.2.and.idpanneau(i).eq.1) then
	t15(j,i) = -(vlarg(i)/deltar(i))*(vlength/deltacsc(i)**2)
	t19(j,i) = -(vlarg(i)/deltar(i)**2)*(vlength/deltacsc(i))
	endif
      if (j.eq.2.and.idpanneau(i).eq.2) then                 ! fevrier 2004
	t15(j,i) = -2*nci(i)*vlength/deltacsc(i)**2              ! fevrier 2004
	endif                                                  ! fevrier 2004
      if (j.eq.2.and.idpanneau(i).eq.4) then                 ! fevrier 2004
	t15(j,i) = -vlength/deltacsc(i)**2                       ! fevrier 2004
	t19(j,i) = -vlarg(i)/deltar(i)**2                      ! fevrier 2004
      endif                                                  ! fevrier 2004
	if (j.eq.3) then
	t15(j,i) = -(0.5*vhac(i) - vhar(i))*(vlarg(i)/deltar(i))*(vlength/
     *deltacsc(i)**2)
	t19(j,i) = -(0.5*vhac(i) - vhar(i))*(vlarg(i)/deltar(i)**2)*(vleng
     *th/deltacsc(i))
	t12(j,i) = (0.5)*(vlarg(i)/deltar(i))*(vlength/deltacsc(i))
	t16(j,i) = -(vlarg(i)/deltar(i))*(vlength/deltacsc(i))
      endif
      if (j.eq.3.and.idpanneau(i).eq.4) then                 ! fevrier 2004
      t15(j,i) = -vlarg(i)*vlength/deltacsc(i)**2              ! fevrier 2004
	endif                                                  ! fevrier 2004
	if (j.eq.4.or.j.eq.5) then
	t15(j,i) = -2*vlarg(i)*nci(i)*(vlength/deltacsc(i)**2)
	endif
	if (j.eq.6) then
	if (idpanneau(i).eq.1.and.itpanneau(i).eq.2.and.ippanneau(i).eq.1)
     *then
	t15(j,i) = -1*(vlength/deltacsc(i)**2)
	endif
      if (idpanneau(i).eq.1.and.itpanneau(i).eq.2.and.ippanneau(i).eq.2)
     *then	
	t15(j,i) = -0.5*(vlength/deltacsc(i)**2)
	endif
	endif
	if (j.eq.7) then
      t15(j,i) = -2*(vlength/deltacsc(i)**2)
	endif
	if (j.eq.8) then
      t15(j,i) = -(vlarg(i)/deltar(i))*(vlength/deltacsc(i)**2)
	t19(j,i) = -(vlarg(i)/deltar(i)**2)*(vlength/deltacsc(i))
	endif
	if (j.eq.9) then
      t15(j,i) = -vhac(i)*(vlength/deltacsc(i)**2)
	t12(j,i) = (vlength/deltacsc(i))
	endif
      if (j.eq.10) then
	t15(j,i) = -(0.5*vhac(i) - vhar(i))*(vlarg(i)/deltar(i))*(vlength/
     *deltacsc(i)**2)
	t19(j,i) = -(0.5*vhac(i) - vhar(i))*(vlarg(i)/deltar(i)**2)*(vleng
     *th/deltacsc(i))
	t12(j,i) = (0.5)*(vlarg(i)/deltar(i))*(vlength/deltacsc(i))
	t16(j,i) = -(vlarg(i)/deltar(i))*(vlength/deltacsc(i))
	endif
	if ((j.eq.16.or.j.eq.19.or.j.eq.20).and.idpanneau(i).eq.1) then 
      t19(j,i) = -(vlarg(i)/deltar(i)**2)*vlength
	endif
      if (j.eq.21.or.j.eq.23) then
	t15(j,i) = -(vlength/deltacsc(i)**2)*vlarg(i)
	endif
	if (j.eq.24.or.j.eq.25.or.j.eq.27.or.j.eq.30.or.j.eq.38.or.j.eq.39
     *.or.j.eq.41.or.j.eq.44) then
	t15(j,i) = -(vlarg(i)/deltar(i))*(vlength/deltacsc(i)**2)
	t19(j,i) = -(vlarg(i)/deltar(i)**2)*(vlength/deltacsc(i))
	endif
	if (j.eq.26..or.j.eq.31.or.j.eq.40.or.j.eq.45) then
      t19(j,i) = -(vlarg(i)/deltar(i)**2)
	endif
      if (j.eq.28.or.j.eq.29.or.j.eq.42.or.j.eq.43) then
	t15(j,i) = -nci(i)*(vlength/deltacsc(i)**2)
	endif
      if (j.eq.32) then
	t19(j,i) = -vlarg(i)/deltar(i)**2
	endif
	if (j.eq.34) then
	t19(j,i) = -(vlarg(i)/deltar(i)**2)*vlength
	endif
	if (j.eq.36) then
	t15(j,i) = -(vlength/deltacsc(i)**2)*vlarg(i)
	endif	
	if (j.eq.49.or.j.eq.50) then
	t19(j,i) = -(vlarg(i)/deltar(i)**2)*nam
	endif
      if (j.eq.51) then
	t15(j,i) = -vlarg(i)*(vlength/deltacsc(i)**2)
	endif
c     opération 52 (+46) ?????????????????????      
	enddo

      if (j.eq.53.and.(idpanneau(i).eq.1.or.idpanneau(i).eq.4)) then   ! fevrier 2004
	t15(j,i) = -vlarg(i)*vlength/deltacsc(i)**2                        ! fevrier 2004
	endif                                                            ! fevrier 2004
	if ((j.eq.55.or.j.eq.56.or.j.eq.57).and.				         ! fevrier 2004
     *(idpanneau(i).eq.1.or.idpanneau(i).eq.4)) then                   ! fevrier 2004
	t15(j,i) = -vlength/deltacsc(i)**2                                 ! fevrier 2004
	endif                                                            ! fevrier 2004
	if (j.eq.59.or.j.eq.58.and.idpanneau(i).eq.5) then               ! mars 2004
	t15(j,i) = -vlength/deltacsc(i)**2                                 ! fevrier 2004
	endif                                                            ! fevrier 2004
	if (j.eq.60) then                                                ! fevrier 2004
        if (k(j,i).eq.1.or.k(j,i).eq.3) then                           ! fevrier 2004
	    t15(j,i) = -vlength/deltacsc(i)**2                             ! fevrier 2004
	  elseif (k(j,i).eq.2.or.k(j,i).eq.3) then                       ! fevrier 2004
		t19(j,i) = -vlarg(i)/deltar(i)**2                            ! fevrier 2004
	  endif                                                          ! fevrier 2004
	endif                                                            ! fevrier 2004

	enddo
  783 continue

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c 11.0	calcul des couts tolier+soudeur et cout total  
c ---------------------------------------------------------
c  variables utilisées :                        
c	cut(i,j) - matrice des couts unitaires tolier  
c	cus(i,j) - matrice des couts unitaires soudeur  
c	i = indice operation							
c	j = indice panneaux							
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

	do i=1,10
	do j=1,neto
	 titol(i,j) =q(i,j)*cut(i,j)*k(i,j)*ca(i,j)*cttpp
	 tisoud(i,j)=q(i,j)*cus(i,j)*k(i,j)*ca(i,j)*ctspp
	enddo
	enddo				
ccccccccccccccccccccccccc
	do i=11,20
	do j=1,neto
	 titol(i,j)= q(i,j)*cut(i,j)*k(i,j)*ca(i,j)*cmn
	 tisoud(i,j)=q(i,j)*cus(i,j)*k(i,j)*ca(i,j)*cmn*(1.+cbn)
	enddo
	enddo				
cccccccccccccccccccccccccccc
	do i=21,45
	do j=1,neto
	 titol(i,j) =q(i,j)*cut(i,j)*k(i,j)*ca(i,j)*(1+crp)
	 tisoud(i,j)=q(i,j)*cus(i,j)*k(i,j)*ca(i,j)*(1+cbp)
	enddo
	enddo				
cccccccccccccccccccccccccccc
	do i=46,nnn
	do j=1,neto
	  titol(i,j) =q(i,j)*cut(i,j)*k(i,j)*ca(i,j)
	  tisoud(i,j)=q(i,j)*cus(i,j)*k(i,j)*ca(i,j)
	enddo
	enddo				

ccccccccccccccccccccccccc
	do i=1,nnn
	do j=1,neto
	  rrotol(i,j) =titol(i,j) *cat(i,j)
	  rrosoud(i,j)=tisoud(i,j)*cat(i,j)
	enddo
	enddo				
ccccccccccccccccccccccccccccc

c      call annuld(pantolti,   neto)
c	call annuld(pansoudti,  neto)
c	call annuld(pantolrro,  neto)
c	call annuld(pansoudrro, neto)
c	call annuld(pantotalti, neto)
c	call annuld(pantotalrro,neto)

	do j=1,neto
	do i=1,nnn
 	 pantolti(j) =pantolti(j) +titol(i,j)      ! = coût tôlier pour toutes les op. par panneau (ti)
	 pansoudti(j)=pansoudti(j)+tisoud(i,j)     ! = coût soudeur pour toutes les op. par panneau (ti)
	 pantolrro(j)=pantolrro(j)+rrotol(i,j)     ! = coût tôlier pour toutes les op. par panneau (rro)
	 pansoudrro(j) =pansoudrro(j)+rrosoud(i,j) ! = coût soudeur pour toutes les op. par panneau (rro)
	 pantotalti(j) =pantolti(j)  +pansoudti(j) ! = coût total par panneau (ti)
	 pantotalrro(j)=pantolrro(j) +pansoudrro(j)! = coût total par panneau (rro) -> c'est ce qui nous
	enddo                                      !                                   intéresse
	enddo

cccccccccccccccccccccccccccccc
      sumtolti = 0.
	sumsoudti = 0.
	sumtolrro = 0.
	sumsoudrro = 0.

      do j=1,neto
	 sumtolti  =sumtolti  +pantolti(j)     ! = coût tôlier pour toute la structure (ti)
	 sumsoudti =sumsoudti +pansoudti(j)    ! = coût soudeur pour toute la structure (ti)
	 sumtolrro =sumtolrro +pantolrro(j)    ! = coût tôlier pour toute la structure (rro)
	 sumsoudrro=sumsoudrro+pansoudrro(j)   ! = coût soudeur pour toute la structure (rro)
	enddo

cccccccccccccccccccccccccccccc
c      sumprepreti = 0.
c	sumpreprerro = 0.
c	sumpreassti = 0.
c	sumpreassrro = 0.
c	sumpremonti = 0.
c	sumpremonrro = 0.
 
	do i=1,10
	do j=1,neto
	sumprepreti =sumprepreti +titol(i,j) +tisoud(i,j)
	sumpreprerro=sumpreprerro+rrotol(i,j)+rrosoud(i,j)
	enddo
	enddo

	do i=11,20
	do j=1,neto
	sumprenti =sumprenti +titol(i,j) +tisoud(i,j)
	sumprenrro=sumprenrro+rrotol(i,j)+rrosoud(i,j)
	enddo
	enddo	

	do i=21,45
	do j=1,neto
	sumpreassti =sumpreassti +titol(i,j) +tisoud(i,j)
	sumpreassrro=sumpreassrro+rrotol(i,j)+rrosoud(i,j)
	enddo
	enddo

	do i=46,nnn
	do j=1,neto
	sumpremonti =sumpremonti +titol(i,j) +tisoud(i,j)
	sumpremonrro=sumpremonrro+rrotol(i,j)+rrosoud(i,j)
	enddo
	enddo
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c	call somme(titol,nnn,neto,sumtolti)
c	call somme(tisoud,nnn,neto,sumsoudti)
c	call somme(rrotol,nnn,neto,sumtolrro)
c	call somme(rrosoud,nnn,neto,sumsoudrro)


	sumtolti  =sumtolti  +pmb  +div(1)
	sumsoudti =sumsoudti +div(2)
	sumtolrro =sumtolrro +pmb  +div(1)
	sumsoudrro=sumsoudrro+div(2)
	totalti   =sumtolti  +sumsoudti
	totalrro  =sumtolrro +sumsoudrro

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  11.1    calcul des sensibilites
c  -------------------------------
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      
      if (icout.eq.3) goto 784

c	call annuld(derivee,nder)!,nn	!janv2007
      ncont = 0
	do j=1,neto
	 nbrxi = nvar(j)
	 do i=1,nbrxi
	  ii = i + ncont
	  i1 = nxit(i,j)
	  if (i1.eq.1) then
	   do l=1,10
	     derivee(ii) = derivee(ii) + 
     *     (0. 
     *      + (t21s(l,j)*ctspp + t21t(l,j)*cttpp)*q(l,j))
     *      *k(l,j)*ca(l,j)*cat(l,j)
	   enddo
	   do l=11,20
	     derivee(ii) = derivee(ii) + 
     *     (0.
     *      + (t21s(l,j)*cmn*(1.+cbn) + t21t(l,j)*cmn)*q(l,j))
     *      *k(l,j)*ca(l,j)*cat(l,j)
	   enddo
	   do l=21,45
	     derivee(ii) = derivee(ii) + 
     *     (0.
     *      + (t21s(l,j)*(1.+cbp) + t21t(l,j)*(1.+crp))*q(l,j))
     *      *k(l,j)*ca(l,j)*cat(l,j)
	   enddo
	   do l=46,nnn
	     derivee(ii) = derivee(ii) + 
     *     (0.
     *      + (t21s(l,j) + t21t(l,j))*q(l,j))
     *      *k(l,j)*ca(l,j)*cat(l,j)
	   enddo
	  endif
	  if (i1.eq.2) then
	   do l=1,10
	     derivee(ii) = derivee(ii) + 
     *     (t12(l,j)*(cus(l,j)*ctspp + cut(l,j)*cttpp)
     *      + 0.)
     *      *k(l,j)*ca(l,j)*cat(l,j)
	   enddo
	   do l=11,20
	     derivee(ii) = derivee(ii) + 
     *     (t12(l,j)*(cus(l,j)*cmn*(1.+cbn) + cut(l,j)*cmn)
     *      + 0.)
     *      *k(l,j)*ca(l,j)*cat(l,j)
	   enddo
	   do l=21,45
	     derivee(ii) = derivee(ii) + 
     *     (t12(l,j)*(cus(l,j)*(1.+cbp) + cut(l,j)*(1.+crp))
     *      + 0.)
     *      *k(l,j)*ca(l,j)*cat(l,j)
	   enddo
	   do l=46,nnn
	     derivee(ii) = derivee(ii) + 
     *     (t12(l,j)*(cus(l,j) + cut(l,j))
     *      + 0.)
     *      *k(l,j)*ca(l,j)*cat(l,j)
	   enddo
	  endif

        if (i1.eq.3) then
	   do l=1,10
           derivee(ii) = derivee(ii) + 
     *     (0. 
     *      + (t23s(l,j)*ctspp + t23t(l,j)*cttpp)*q(l,j))
     *      *k(l,j)*ca(l,j)*cat(l,j)
	   enddo
	   do l=11,20
	     derivee(ii) = derivee(ii) + 
     *     (0.
     *      + (t23s(l,j)*cmn*(1.+cbn) + t23t(l,j)*cmn)*q(l,j))
     *      *k(l,j)*ca(l,j)*cat(l,j)
	   enddo
	   do l=21,45
	     derivee(ii) = derivee(ii) + 
     *     (0.
     *      + (t23s(l,j)*(1.+cbp) + t23t(l,j)*(1.+crp))*q(l,j))
     *      *k(l,j)*ca(l,j)*cat(l,j)
	   enddo
	   do l=46,nnn
	     derivee(ii) = derivee(ii) + 
     *     (0.
     *      + (t23s(l,j) + t23t(l,j))*q(l,j))
     *      *k(l,j)*ca(l,j)*cat(l,j)
	   enddo
	  endif

	  if (i1.eq.5) then
	   do l=1,10
	     derivee(ii) = derivee(ii) + 
     *     (t15(l,j)*(cus(l,j)*ctspp + cut(l,j)*cttpp)
     *      + 0.)
     *      *k(l,j)*ca(l,j)*cat(l,j)
	   enddo
	   do l=11,20
	     derivee(ii) = derivee(ii) + 
     *     (t15(l,j)*(cus(l,j)*cmn*(1.+cbn) + cut(l,j)*cmn)
     *      + 0.)
     *      *k(l,j)*ca(l,j)*cat(l,j)
	   enddo
	   do l=21,45
	     derivee(ii) = derivee(ii) + 
     *     (t15(l,j)*(cus(l,j)*(1.+cbp) + cut(l,j)*(1.+crp))
     *      + 0.)
     *      *k(l,j)*ca(l,j)*cat(l,j)
	   enddo
	   do l=46,nnn
	     derivee(ii) = derivee(ii) + 
     *     (t15(l,j)*(cus(l,j) + cut(l,j))
     *      + 0.)
     *      *k(l,j)*ca(l,j)*cat(l,j)
	   enddo
	  endif
	  if (i1.eq.6) then
	   do l=1,10
	     derivee(ii) = derivee(ii) + 
     *     (t16(l,j)*(cus(l,j)*ctspp + cut(l,j)*cttpp)
     *      + (t26s(l,j)*ctspp + t26t(l,j)*cttpp)*q(l,j))
     *      *k(l,j)*ca(l,j)*cat(l,j)
	   enddo
	   do l=11,20
	     derivee(ii) = derivee(ii) + 
     *     (t16(l,j)*(cus(l,j)*cmn*(1.+cbn) + cut(l,j)*cmn)
     *      + (t26s(l,j)*cmn*(1.+cbn) + t26t(l,j)*cmn)*q(l,j))
     *      *k(l,j)*ca(l,j)*cat(l,j)
	   enddo
	   do l=21,45
	     derivee(ii) = derivee(ii) + 
     *     (t16(l,j)*(cus(l,j)*(1.+cbp) + cut(l,j)*(1.+crp))
     *      + (t26s(l,j)*(1.+cbp) + t26t(l,j)*(1.+crp))*q(l,j))
     *      *k(l,j)*ca(l,j)*cat(l,j)
	   enddo
	   do l=46,nnn
	     derivee(ii) = derivee(ii) + 
     *     (t16(l,j)*(cus(l,j) + cut(l,j))
     *      + (t26s(l,j) + t26t(l,j))*q(l,j))
     *      *k(l,j)*ca(l,j)*cat(l,j)
	   enddo
	  endif
	  if (i1.eq.9) then
	   do l=1,10
	     derivee(ii) = derivee(ii) + 
     *     (t19(l,j)*(cus(l,j)*ctspp + cut(l,j)*cttpp)
     *      + 0.)
     *      *k(l,j)*ca(l,j)*cat(l,j)
	   enddo
	   do l=11,20
	     derivee(ii) = derivee(ii) + 
     *     (t19(l,j)*(cus(l,j)*cmn*(1.+cbn) + cut(l,j)*cmn)
     *      + 0.)
     *      *k(l,j)*ca(l,j)*cat(l,j)
	   enddo
	   do l=21,45
	     derivee(ii) = derivee(ii) + 
     *     (t19(l,j)*(cus(l,j)*(1.+cbp) + cut(l,j)*(1.+crp))
     *      + 0.)
     *      *k(l,j)*ca(l,j)*cat(l,j)
	   enddo
	   do l=46,nnn
	     derivee(ii) = derivee(ii) + 
     *     (t19(l,j)*(cus(l,j) + cut(l,j))
     *      + 0.)
     *      *k(l,j)*ca(l,j)*cat(l,j)
	   enddo
	  endif
	 enddo
	 ncont = ncont + nbrxi
	enddo

  784 continue

ccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  12.0    ecriture resultats   
ccccccccccccccccccccccccccccccccccccccccccccccccccc

      if (icout.eq.3) pas = xxx !remettre la bonne valeur du pas pour sensibcout
  
	rewind 1
	rewind 2
	rewind 3
	rewind 7
	rewind 8
	rewind 10
	rewind 44
	rewind 88

	close(7)
	close(8)
	close(88)
	close(1)
	close(2)
	close(3)
	close(10)
	close(44)

      return
  906 write(*,*) 'stop : the "input.txt" file is missing'           !juillet2003
      write(6970,*) 'stop : the "input.txt" file is missing'           !juillet2003
	write(29,*) 'stop : the "input.txt" file is missing'							!bug
  907 write(*,*) 'stop : the "fractionnement.txt" file is missing'  !juillet2003
      write(6970,*) 'stop : the "fractionnement.txt" file is missing'  !juillet2003
	write(29,*) 'stop : the "fractionnement.txt" file is missing'					!bug
  908 write(*,*) 'stop : the "acces.txt" file is missing'           !juillet2003
      write(6970,*) 'stop : the "acces.txt" file is missing'           !juillet2003
      write(29,*) 'stop : the "acces.txt" file is missing'							!bug
	stop                                                          !juillet2003
	end





c################################################################################
cccccc  subroutine de calcul des sommes des termes d'une martice    cccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	subroutine somme(a,n,m,suma)
	implicit real*8 (a-g,o-z)
	dimension a(n,m)

	suma=0.
	do i=1,n
	  do j=1,m
	    suma=suma+a(i,j)
	  enddo
	enddo

	return
	end




c################################################################################
cccccc  subroutine: ascending ordering of a vector   
ccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	subroutine ord(nn,n,sir)
	implicit real*8 (a-g,o-z)
     	dimension sir(nn,n)	

      do 1 i=1,n-1
	 m=i+1
	 do 2 j=m,n
	   if(sir(1,i).gt.sir(1,j)) then
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
cccccc  subroutine: special ascending ordering of a vector   
ccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c	subroutine ord2(nn,n,c,num)
c	implicit real*8 (a-g,o-z)
c     	dimension c(nn,n),num(n)
c     tri spécial de 2 variables de conception 
c     (utilisé spécialement pour les opérations 49 et 50)
c
c     do i=1,n-1
c	  k = 1
c	  do while (c(1,i).eq.c(1,i+k).and.(k+i).le.n)
c	    k = k+1
c	  enddo
c	  num(i)=k
c	enddo
c      num(n) = 1
c
c      do i=1,n-1
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
c	enddo

c	return
c	end

cc################################################################################
cccccc  subroutine: interpolation
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine interpol(param,cu,nbrdon,var,cun,j,nnn)
      implicit real*8 (a-g,o-z)
    	dimension var(nnn,nbrdon),cun(nnn,nbrdon)

	do ijk=1,nbrdon
	  if (param.le.var(j,1)) then
	    cu = cun(j,1)
	    goto 1
 	  else if(param.le.var(j,ijk)) then
	    c2 = cun(j,ijk)
	    c1 = cun(j,ijk-1)
	    v2 = var(j,ijk)
	    v1 = var(j,ijk-1)
          cu = (c2 - c1)*(param - v1)/(v2 - v1) + c1
	    goto 1
	  else if(param.ge.var(j,nbrdon)) then
          cu = cun(j,nbrdon)
          goto 1
	  endif
      enddo

   1	return
	end

cc################################################################################
cccccc  subroutine: interpolation
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c      subroutine interpol2(num,npabouts,param,cut,variablet1,tolier,
c     *                     tolier1,tolier2,j,nnn)
c      implicit real*8 (a-g,o-z)
c     	dimension variablet1(num),tolier(num),tolier1(num),tolier2(num)
c
cc     interpolation spéciale pour les opérations 49 et 50 (2 var de conception)

c	if (num.gt.1) then
c	  if (npabouts.eq.1) then
c	    call interpol(param*1000,cut,num,variablet1,tolier,1,nnn)
c	  else if (npabouts.eq.2) then
c          call interpol(param*1000,cut,num,variablet1,tolier1,1,nnn)
c	  else if (npabouts.eq.3) then
c          call interpol(param*1000,cut,num,variablet1,tolier2,1,nnn)
c	  endif
c	else
c	  if (npabouts.eq.1) cut = tolier(1)
c	  if (npabouts.eq.2) cut = tolier1(1)
c	  if (npabouts.eq.3) cut = tolier2(1)
c	endif
      
c	return
c	end

	
