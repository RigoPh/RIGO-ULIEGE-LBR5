      subroutine sensibcout2(obj,xi,itt,width,nn)		!18.03.04
      use sharedvar
      implicit real*8(a-h,o-z)
	dimension xi(9*neto),derivee(nn)

c******************************************************************************
c     subroutine sensibcout2
c     ======================
c     cette sous-routine est appelée dans optis.
c     elle recalcule la fonction objectif après être passé dans le module
c     d'optimisation (cfr sensibcout, mais sans calcul des sensibilités).
c
c inputs : neto : nombre de panneaux de la structure.
c          nvar : vecteur stockant les numéros des variables de conception utilisées.
c          spec : poids spécifique (n/m3) (à spécifier pour chaque panneau)
c          width : longueur totale de la structure
c          itt : paramètre d'option d'impression des résultats
c          xi : valeurs des variables de conception après optimisation
c          
c          
c outputs : obj : coût total de fabrication de la structure.
c      
c créer: f. bair (juin 2003)
c
c modifications:
c  - juin 2003
c
c dernière mise à jour: 16 juin 2003 (ph. rigo)      
c					  18 mars 2004 (j.pirard)         
c
c******************************************************************************
c******************************************************************************
c******************************************************************************
      call annuld(coutpan,neto)
	i1=0
      ncont = 0
	rewind 305				!extension neto	!fev2007
	rewind 302				!dad redressage

	read(305,*) vlength		!extension neto	!fev2007

	do i=1,neto
      read(305,*) nopan,vlarg1,vepaiss1									!extension neto	!fev2007
	read(305,*) vhac1,veac1,vlsc1,vesc1,deltac1,vdiffc1					!18.03.04	!extension neto	!fev2007
	read(305,*) vhar1,vear1,vlsr1,vesr1,deltar1,entrsc(i)				!extension neto	!fev2007
      read(302)deltacsc(i),deltar(i),vepaiss(i),vhac(i),veac(i),vlsc(i),	!extension neto	!fev2007
     *         vesc(i),vhar(i),vear(i),vlsr(i),vesr(i),philsc(i),qsc(i),	!extension neto	!fev2007
     *		 vdiffc(i)													!18.03.04
      vlarg(i)=philsc(i)*qsc(i)*pi/180.

      nbrxi=nvar(i)
      do 123 i1=1,nbrxi
	if(isect(i).eq.0) then												!18.03.04
	goto(124,125,120,126,127,128,121,129,130),nxit(i1,i)
  124 vepaiss(i)=xi(i1+ncont)
      goto 123
  125 vhac(i)=xi(i1+ncont)
      goto 123
  120 veac(i)=xi(i1+ncont)
      goto 123
  126 vlsc(i)=xi(i1+ncont)
      goto 123
  127 deltacsc(i)=xi(i1+ncont)
      goto 123
  128 vhar(i)=xi(i1+ncont)
      goto 123
  121 vear(i)=xi(i1+ncont)
      goto 123
  129 vlsr(i)=xi(i1+ncont)
      goto 123
  130 deltar(i)=xi(i1+ncont)
	else													!18.03.04
	goto(134,135,133,136,137),nxit(i1,i)					!18.03.04
  134 vhac(i)=xi(i1+ncont)									!18.03.04
      goto 123												!18.03.04
  135 veac(i)=xi(i1+ncont)									!18.03.04
      goto 123												!18.03.04
  133 vlsc(i)=xi(i1+ncont)									!18.03.04
      goto 123												!18.03.04
  136 vdiffc(i)=xi(i1+ncont)									!18.03.04
      goto 123												!18.03.04
  137 deltacsc(i)=xi(i1+ncont)									!18.03.04
      goto 123												!18.03.04
	endif													!18.03.04			
  123 continue

c     epaisseur de corrosion
c     -----------------------
c     dcor = epaisseur de corrosion pour bordé
       dcorsc(i)  = corro(i,1)	!aout2006	!corrosion
	 dcorsc2(i) = corro(i,2)	!aout2006	!corrosion
	 dcorsc3(i) = corro(i,3)	!aout2006	!corrosion

c	if(i.eq.1) then
c	  rewind 57
c	  read(57,'(////,a1)') abidon
c	  read(57,*) ipan,dcorsc(i),dcorsc2(i),dcorsc3(i)
c	else
c	  read(57,*) ipan,dcorsc(i),dcorsc2(i),dcorsc3(i)
c	endif

	if(isect(i).eq.0) then										!18.03.04
	vepaiss(i) = vepaiss(i) + dcorsc(i)
	else														!18.03.04
	vepaiss(i) = 0.0000											!18.03.04
	endif														!18.03.04
c	dcor2   ! epaisseur de corrosion pour cadres
	veac(i)  = veac(i) + dcorsc2(i)
	vesc(i)  = vesc(i) + dcorsc2(i)
      vdiffc(i)	= vdiffc(i)	 + dcorsc2(i)							!18.03.04
c	dcor3   ! epaisseur de corrosion pour lisses
	vear(i)  = vear(i) + dcorsc3(i)
	vesr(i)  = vesr(i) + dcorsc3(i)


c     impressions (avec epaiss corrosion)
c     -----------
      if(itt.eq.1) then
	 if(isect(i).eq.0) then												!18.03.04
        write(666,132)i,'ep. bord','hâme cad','eâme cad','lsem cad',
     *      	'tsem cad','epsa cad','hâme rai','eâme rai','lsem rai',
     *        'tsem rai','epsr rai'  
        write(666,131)vepaiss(i),vhac(i),veac(i),vlsc(i),vesc(i),
     *           deltacsc(i),vhar(i),vear(i),vlsr(i),vesr(i),deltar(i)
	 elseif(isect(i).eq.3) then											!18.03.04
        write(666,142)i,'hâme epo','eâme epo','lsem epo',					!18.03.04
     *      	'tsem epo','epsa epo','hâme rai','eâme rai','lsem rai',		!18.03.04
     *        'tsem rai','epsr rai'										!18.03.04
        write(666,141)vhac(i),veac(i),vlsc(i),vesc(i),					!18.03.04
     *           deltacsc(i),vhar(i),vear(i),vlsr(i),vesr(i),deltar(i)		!18.03.04
	 else																!18.03.04
        write(666,152)i,'diam epo','epai epo','epsa epo',					!18.03.04
     *		'hâme rai','eâme rai','lsem rai','tsem rai','epsr rai'		!18.03.04
        write(666,151)vhac(i),vdiffc(i),									!18.03.04
     *           deltacsc(i),vhar(i),vear(i),vlsr(i),vesr(i),deltar(i)		!18.03.04
	 endif																!18.03.04		
	endif

      ncont = ncont + nbrxi

	enddo	! fin boucle sur panneaux

	rewind(305)			!extension neto	!fev2007
    
c ===============================================
      fmat = 0.0

c     calcul de fmat= coût des matériaux (acier)
      do i=1,neto
	 temp = philsc(i) * qsc(i) * width * pi/180.
       dens = spec(i)/9.81
	if(isect(i).eq.0) then														!sept06
	 fmat1 = temp * dens *														!sept06
     *  (c1 *(1.+(vepaiss(i)-dref)*1000.*dc1) * vepaiss(i)						!sept06
     * + c2 *(1.+(vear(i)  -dref)*1000.*dc1) * (1.0+dw2)   ! dref et pas drefx	!sept06
     *                  * (vear(i)*vhar(i)+vesr(i)*vlsr(i))/deltar(i)				!sept06
     * + c3 *(1.+(veac(i)  -dref)*1000.*dc1) * (1.0+dw3)   ! dref et pas drefy	!sept06
     *                  * (veac(i)*vhac(i)+vesc(i)*vlsc(i))/deltacsc(i))			!sept06
	 elseif(isect(i).eq.1) then													!sept06
	 fmat1 = temp * dens *														!sept06
c     *  ( c2 *(1.+(vear(i)   -dref)*1000.*dc1) * (1.0+dw2)   ! dref et pas drefx	!sept06
c     *                   * (vear(i)*vhar(i)+vesr(i)*vlsr(i))/deltar(i)			!sept06
     * + c3 *(1.+(veac(i)   -dref)*1000.*dc1) * (1.0+dw3)   ! dref et pas drefy	!sept06
     *   *pi*(vhac(i)*vhac(i)-(vhac(i)-2*vdiffc(i))**2) /(4*deltacsc(i))!)		!sept06
	 elseif(isect(i).eq.2) then													!sept06
	 fmat1 = temp * dens *														!sept06
c     *  ( c2 *(1.+(vear(i)   -dref)*1000.*dc1) * (1.0+dw2)   ! dref et pas drefx	!sept06
c     *                   * (vear(i)*vhar(i)+vesr(i)*vlsr(i))/deltar(i)			!sept06
     * + c3 *(1.+(veac(i)   -dref)*1000.*dc1) * (1.0+dw3)   ! dref et pas drefy	!sept06
     *   *(vhac(i)*vhac(i)-(vhac(i)-2*vdiffc(i))**2) /deltacsc(i)!)				!sept06
	 elseif(isect(i).eq.3) then													!sept06
	 fmat1 = temp * dens *														!sept06
c     *  ( c2 *(1.+(vear(i)   -dref)*1000.*dc1) * (1.0+dw2)   ! dref et pas drefx	!sept06
c     *                   * (vear(i)*vhar(i)+vesr(i)*vlsr(i))/deltar(i)			!sept06
     * + c3 *(1.+(veac(i)   -dref)*1000.*dc1) * (1.0+dw3)   ! dref et pas drefy	!sept06
     *   *(veac(i)*2* vhac(i)+2*vesc(i)* vlsc(i)) /deltacsc(i)!)					!sept06
	endif																		!sept06
      
	 fmat = fmat + fmat1
c       write (666,*) 'panel',i,'cout mat=',fmat1
	enddo


c     calcul coût main d'oeuvre (coutrro et coutpan en rro)

c      nn = 800 ! pas d'importance ici
	
	
	call costmain(coutrro,vlength,non,i1,derivee,nn,totalti)

      obj = fmat + coutrro*eqp*c1*1000
      
      return
c -----------------------------------------------------------------
  131 format('variables_:',11(e11.4))
  132 format('panel n°',i2,2x,11(1x,a8,2x))
  141 format('variables_:',10(e11.4))							!18.03.04
  142 format('panel n°',i2,2x,10(1x,a8,2x))					!18.03.04
  151 format('variables_:',8(e11.4))							!18.03.04
  152 format('panel n°',i2,2x,8(1x,a8,2x))					!18.03.04
 	
	end