      subroutine copy(itera,txr_dcns,tya_dcns)
      use sharedvar
      implicit real *8(a-h,o-z)
      character *1  num
      character *20 text(5)
      integer *2 dessin,code,fami,lot			!r&d15
      dimension nno9(2)

	double precision txr_dcns(nmax)
	double precision tya_dcns(nmax)

c
c***********************************************************************
c     subroutine copy
c     *****************
c     subroutine de sauvetage des données après chaque itération
c     en vue d'une réutilisation ultérieure.
c
c     création: 21 mars 1997 ; dr. ph. rigo
c
c     modif.  : 05-03-99
c               06-12-99
c               29-12-99 (sauvetage file 268 pour desin)
c               10-11-00 (rest. centre de gravité)
c               26-11-03
c			  05-02-04 (épontilles)
c***********************************************************************

      rewind 55

      num=char(96+itera)
      open(77,file='up-'//num//'-'//don1)

c     ligne 0							!nom de la version
      read (55,'(5a20)') text
      write(77,'(5a20)') text

	read(55,*) iana						!r&d14	!fev2007
	write(77,'(i2)') iana				!r&d14	!fev2007

c     ligne 1
      read (55,'(5a20)') text
      write(77,'(5a20)') text

c     ligne 2
      read(55,*)  impr,impr2,indaig,indrai,dessin,jlph,jlbord,neto
      write(77,'(5(i2,1x),i3,1x,i2,1x,i3,21x,a)') impr,impr2,indaig,
     *indrai,dessin,jlph,jlbord,neto,'commentaire'

	read(55,*) imulti,rho,w1,w2,w3									!multi obj	!fev2007	
	write(77,'(i2,1x,4(f8.4,1x),t45,a))') imulti,rho,w1,w2,w3,		!multi obj	!fev2007
     *     'imulti,rho,w1,w2,w3'										!multi obj	!fev2007

      read(55,*) ipareto, nsteppar, iparcout, iparpoids, iparinertia
	write(77,'(i2,1x,i4,3(i2,1x),t45,a))')  ipareto, nsteppar, 
     *iparcout, iparpoids, iparinertia,		!multi obj
     *     'frontière pareto: active (0 = non, 1 = oui); nombre des pas; 
     *coût (0 = non, 1 = oui); poids (0 = non, 1 = oui); 
     *inertia (0 = non, 1 = oui)'										!multi obj

c     ligne 3
      read(55,*)  iopti,iteram
      write(77,'(i2,1x,i3,t40,a))') iopti,iteram,
     *     'iopti (1 oui, 0 non) et itemax = nbre d''itération'

c     ligne 4 à 13
      read(55,*) icout             ! icout
      read(55,*) iredress          ! iredress
      read(55,*) rend_global, eqp  ! rend_global, eqp
      read(55,*) refthick_input    ! reference thickness input

      read(55,*) dref,drefx,drefy  ! epaisseurs de référence
      read(55,*) dref_b,dref_c,dref_l 
	                             ! epaisseurs de référence 
								 !                (Bulbe, Composite, Laminé)

      read(55,*) c1,c2,c3,dc1      ! cout des matériaux
      read(55,*) c_pb,c_tc,c_tl    !(tole.Euro/kg). (Bulbe, Composite, Laminé)
      read(55,*) dw2,dw3           ! extra weight
      read(55,*) dw_b,dw_c,dw_l    ! extra weight   (Bulbe, Composite, Laminé)

      read(55,*) p10,dp10          ! mdo bordé
      read(55,*) p4,p5,dp4,dp5     ! mdo assembl. membr-bordé
      read(55,*) p9x,p9y,dp9x,dp9y ! mdo constr. membr.
      read(55,*) p6,p7,ber,bet     ! mdo intersect et goussets
      read(55,*) c8,dc8,ialr,ialt  ! coût soudure (energ + consom.


      write(77,'(i2,t45,a)')		      icout,   'icout'
      write(77,'(i2,t45,a)')		      iredress,'iredress'   !dad 14.12.2007
      write(77,'(f8.4,1x,f8.4,t45,a)')  rend_global,eqp,
     *'rend_global,eqp(0.01<k=eqp<0.1)'
      write(77,'(i2,t45,a)')		      refthick_input,
     *'Reference thickness input (0 = LBR5, 1 = DCNS)'


      write(77,'(3(f8.5,1x),t45,a)') dref,drefx,drefy,
     *'eo, eox, eoy  (épaisseur de référence en m)'
      write(77,'(3(f8.5,1x),t45,a)') dref_b,dref_c,dref_l,
     *'Reference thickness for profiles (Bulb, Composite, Rolled)'




      write(77,'(4(f8.3,1x),t45,a)')  c1,c2,c3,dc1,
     *'prix matér. : c1,c2,c3(tole,euro/kg), dc1(tole,variation par mm)'
      write(77,'(3(f8.3,1x),t45,a)')  c_pb,c_tc,c_tl,
     *'Prix Matér. : C_PB.C_TC.C_TL(tole.Euro/kg). (Bulb, Composite, 
     *Rolled)'


      write(77,'(2(f8.3,1x),t45,a,a)')  dw2,dw3,'extra weight: ',
     *'dw2(long,variation sur c2),dw3(trans,variation sur c3)'
      write(77,'(3(f8.3,1x),t45,a,a)')  dw_b,dw_c,dw_l,
     *'Extra weight on profiles(%)  (Bulb, Composite, Rolled)'




      write(77,'(2(f8.3,1x),t45,a)')  p10,dp10,
     *'mdo bordé   : p10(h-h/m),  pc10(variation par mm) '
      write(77,'(4(f8.3,1x),t45,a,a)')  p4,p5,dp4,dp5,'mdo assembl.: ',
     *'p4x(long,h-h/m),p5y(trans,h-h/m),dp4x(long),dp5y(trans)'
      write(77,'(2x,4(f8.3,1x),t45,a,a)')  p9x,p9y,dp9x,dp9y,
     *'mdo membr.  ',
     *': p9x(long,h-h/m),p9y(trans,h-h/m),dp9x(long),dp9y(trans)'
      write(77,'(4(f8.3,1x),t45,a,a)')  p6,p7,ber,bet,'mdo joints  ',
     *': p6(intersect),p7(gousset),béta-x(long.),béta-y(transv), '
      write(77,'(2(f8.3,1x),2(i2,6x),t45,a,a)')  c8,dc8,ialr,ialt,
     *'consommables: c8(euro/m),',
     *'dc8(variation par mm),alpha-x(long.),alpha-y(transv)'

c     lignes 14,15,16,17
      read(55,*)  width
      read(55,*) (dis(i),i=1,5)
      read(55,*) (fam(i),i=1,6)
      read(55,*) ipoids

        fam(5)=0.
        fam(6)=0.

      write(77,'(f12.5,t70,a)') width,'longueur de la structure (width)'
      write(77,'(5(f12.5,1x),t70,a)') 
     *           (dis(i),i=1,5),'section de calcul (dis)'
      write(77,'(6(f6.3,1x),t70,a)') 
     *           (fam(i),i=1,6),'cisaillement des semelles (fam)'
      write(77,'(i2,t30,a)') ipoids,'ipoids ( 1 oui, 0 non)'

c     ligne 18
      read(55,*)   nsolm, nsol,(nnsol(i),i=1,nsol)    
      write(77,12) nsolm, nsol,(nnsol(i),i=1,nsol)

      do i=1,nsolm                !nov2003
	  read (55,'(5a20)') text   !nov2003
	  write (77,'(5a20)') text  !nov2003
	enddo                       !nov2003

  12  format(12(i3),5x,
     *  'load cases: nbr available; nbr selected, list of selected')


c 1.0 lecture des donnees relatives a chaque panneau
c     ================================================
      ntot=0

c     file opened in  main.for
      rewind(46)

      do 1 nel=1,neto
      call ent2(nvar(nel),nxit(1,nel),nsolm,modes(nel),dessin,nel,
     *           txr_dcns,tya_dcns)
    1 continue

c ----------------------------------------------------------------------

c **********************************************************************
c 2.0 transformations des donnees   (subr. modif et coord)
c     ====================================================

      read (55,*) text(1)
      write(77,'(5a29)') 'boundary conditions; y=0, y=h'
      read(55,*) ncondi
      write(77,'(i3,t30,a)')  ncondi,'nbre de conditions de bords'
      if(ncondi.gt.0) then
      do 910 i=1,ncondi
      read(55,*) (nno9(j),j=1,2)
      write(77,'(i3,3x,i2,t30,a,i3,a,i3)')(nno9(j),j=1,2),
     *                      'cond nø',i,',  panel ',nno9(1)
  910 continue
      endif

      read (55,'(5a20)') text
      write(77,'(5a65)') 'gravity repere utilisateur + restriction sur l
     *e centre de gravite'
      read(55,*) xk,yk  ! xk et yk = coord du centre k du repere utilisateur
      write(77,'(f12.6,2x,f12.6,t30,a)') xk,yk,
     *                '(xk,yk) coord du centre du repere utilisateur'
      read(55,*) igrav,xkgmin,xkgmax  ! kgmin=kg(min) et kgmax=kg(max)
      write(77,'(i2,2x,f9.6,2x,f9.6,t35,a)') igrav,xkgmin,xkgmax,
     *           'igrav(no:0;min:1;max:2;min et max:3),xkgmin,xkgmax'

c **********************************************************************

c     restriction sur inertie									!restri inertie
c     =======================

      read (55,'(5a20)') text
	write(77,'(5a18)') 'inertia constraint'
	read(55,*)inert,imin
	write(77,'(i2,2x,e14.7,t30,a)') inert,imin,'inert,imin'	!restri inertie


c     restriction sur module sectionnel							!restri module
c     =================================

      read (55,'(5a20)') text
	write(77,'(5a26)') 'section modulus constraint'
	read(55,*) imod,modmin,ipan
	write(77,'(i2,2x,e14.7,2x,i2,t40,a)') imod,modmin,ipan,
     *'imod,modmin,ipan'											!restri module


c     restriction sur poids									!restri poids
c     =====================

      read (55,'(5a20)') text
	write(77,'(5a17)') 'weight constraint'
	read(55,*) iweight,wmax
	write(77,'(i2,2x,e14.7,t30,a)') iweight,wmax,'iweight,wmax'		!restri poids

c	read(55,*)	!bypass restriction coût							!fev2007
c	read(55,*)														!fev2007
	

c     restriction sur cout											!restri cout
c     =====================

      read (55,'(5a20)') text											!fev2007
	write(77,'(5a15)') 'cost constraint'							!fev2007
	read(55,*) iprice,pricemax,icost								!fev2007
	write(77,'(i2,2x,e14.7,2x,i2,t30,a)') iprice,pricemax,icost,	!fev2007
     *									  'iprice,pricemax,icost'	!restri cout

c **********************************************************************

c 3.0 moments d'extremites   (subr. bateau)
c     ======================================
      read (55,'(5a20)') text
      write(77,'(5a6)') 'moment'
      read(55,*) imom
      write(77,'(i2,t30,a)')imom,'(= 0:pas de moment, = 1:avec moment)'

      if (imom.ne.0) then
         read (55,*) yred
         write(77,'(f8.5,t30,a)')yred,
     *          'coeff. de réduction 1.24 pour sig. et 1.04 pour w'
         do 733 i=1,nsolm
           read (55,*) bm11,bm21,bm31,bm41                                  !nov2003
           write(77,'(4(1x,e14.7),t63,a)') bm11,bm21,bm31,bm41,             !nov2003
     *                        'mx> 0 en hogging = pont tendu (n.m); my' !nov2003
  733    continue
      endif

c **********************************************************************
c 4.0 lecture des contraintes d'egalite (et impression)
c     =================================================
      read (55,'(5a20)') text
      write(77,'(5a21)') 'equality restrictions'
      read(55,*) negalt
      write(77,'(i4,t30,a,a)')negalt,'nbre de restrictions d''égalité'

      do 1333 i=1,negalt
       read(55,*)
       write(77,'(i3,2x,i3,6x,i3,2x,i3,6x,f8.4)')			!fev2007
     *		   (mega(i,k),k=1,4),ega(i)					!fev2007
 1333 continue

c **********************************************************************
c 5.0 donnees pour le calcul de la resistance ultime de la poutre navire
c     ==================================================================

      read (55,'(5a20)') text
      write(77,'(5a17)') 'ultimate strenght'

      read(55,*) irestr
      write(77,'(i2,t15,a)')irestr,
     *  '(irestr: utilisation comme restriction ; 0 = non;1 = oui)'

      if(irestr.eq.1) then
          read(55,*) uhogm,usagm
        write(77,'(t2,e14.7,2x,e14.7,4x,a)')uhogm,usagm,
     *     '(if irest=1 : uhogg>0 et usagg<0 (n.m)'
        endif

      read(55,*) iult
      write(77,'(i2,t15,a,a)')iult,
     *           '(iult =0 no analysis, = 1 for paik/caldwell,',
     *                ' =2 for procol(smith), = 3  for...)'

      if(iult.eq.1) then
        read(55,*) depth,db
        write(77,'(t4,2(f8.4,2x),t30,a)')depth,db,
     *                'hull depth & double bottom hight (m.)'

        read(55,*) nd, (id (i), i=1,nd)
        read(55,*) nb1,(ib1(i), i=1,nb1)
        read(55,*) nb, (ib (i), i=1,nb)
        read(55,*) ns, (is9 (i), i=1,ns)

        write(77,301)  nd, (id(i), i=1,nd)
        write(77,302)  nb1,(ib1(i),i=1,nb1)
        write(77,303)  nb, (ib(i), i=1,nb)
        write(77,304)  ns, (is9(i), i=1,ns)
 301    format(4x,i2,(20i3/),' deck') 
 302    format(4x,i2,(20i3/),' inner bottom')
 303    format(4x,i2,(20i3/),' outer bottom')
 304    format(4x,i2,(20i3/),' both sides plates')

        read(55,*) kd,  sysd
        read(55,*) kb1, sysb1
        read(55,*) kb,  sysb
        read(55,*) ksup,syssup
        read(55,*) ksdw,syssdw

        write(77,'(6x,i2,2x,e14.7,t30,a)')
     *             kd,  sysd, 'ref panel & sy for upper deck'
        write(77,'(6x,i2,2x,e14.7,t30,a)')
     *             kb1, sysb1,'ref panel & sy for inner bottom'
        write(77,'(6x,i2,2x,e14.7,t30,a)')
     *             kb,  sysb, 'ref panel & sy for outer bottom'
        write(77,'(6x,i2,2x,e14.7,t30,a)')ksup,syssup,
     *    'ref panel & sy for upper part of the side plate'
        write(77,'(6x,i2,2x,e14.7,t30,a)')ksdw,syssdw,
     *    'ref panel & sy for lower part of the side plate'
        endif

      close(77)
      return

c ======================================================================
c11.0 les formats
c     ============

      end

c **********************************************************************
c **********************************************************************
c **********************************************************************
c **********************************************************************
      subroutine ent2(nbrxi,nxi,nsolm,mode,dessin,nel,txr_dcns,tya_dcns)
      use sharedvar
      implicit real *8 (a-h,o-z)
      character*9  types									!février 2004
      character*3  mode,bid
      character*7  section								!février 2004
      character*20 text(5)
      integer *2 dessin,code,fami,lot			!r&d15
      dimension nxi(9),xmin(9),xmax(9),cha9(3),ypt(10),noh9(10)
      dimension lcont9(2)
	dimension dvmin(9),dvmax(9)							!eugen	!fev2007
	character*20  name_type_tr1
	double precision txr_dcns(nmax)
	double precision tya_dcns(nmax)

c***********************************************************************
c     subroutine ent2
c     ****************
c     subroutine de lecture des donnees relatives a chaque panneau.
c     cette subr. est calquée sur la subr ent
c***********************************************************************
      pi1=pi/180.

c 1.a   lecture du types de panneau : coque ou plaque
c     -----------------------------------------------
      read(55,'(a9)') types						!février 2004

      if(types.eq.'PLAQUE') then
c       cas de la "plaque"
        read(55,*,end=900) hight,delta
        q=10000000000.
        phil=hight/q/pi1
      elseif(types.eq.'EPONTILLE') then			!février 2004 
c	  cas de l'"épontille"						!février 2004
	  read(55,*,end=900) hight,epais			!février 2004 
	  read(55,3,end=900) section			    !février 2004  
 3	  format(a7)								!février 2004
	  q=10000000000.							!février 2004
	  phil=hight/q/pi1							!février 2004
	  delta=0.00000								!février 2004
	else				
c       cas de la "coque"
        read(55,*,end=900) phil,q,delta
      endif

c 1.b   donnees relatives au raidissage (aig. et raid)
c     ----------------------------------------------
c     entr = entredistance réelle des raid.=(d)raid
c     epsr = largeur collaborante compte tenu du nbre de raid. défini
c            par mode (types de répartition)
c     epsa=(d)aig

	if(types.ne.'EPONTILLE') then							!février 2004
        read(55,*,end=900) epsa,entr,epsa2,entr2,corro(nel,1)
        read(55,*,end=900) hya,dya,wya,tya, corro(nel,2),
     *                     itype_transv(nel),name_type_transv(nel)        
        read(55,*,end=900) hxr,dxr,wxr,txr, corro(nel,3),
     *                     itype_longit(nel),name_type_longit(nel)
	  isect1=0												!février 2004
	  epais=0.000											!février 2004
     	else													!février 2004
        read(55,*,end=900) epsa,heff							!février 2004
	  if(section.eq.'CERCLE')then							!février 2004
	    isect1=1											!février 2004
	    read(55,*,end=900) hya,corro(nel,2)
     								!février 2004
	    dya=0.00000											!février 2004
	    wya=0.00000											!février 2004
	    tya=0.00000											!février 2004
	  elseif(section.eq.'CARRE')then						!février 2004
	    isect1=2											!février 2004
          read(55,*,end=900) hya,corro(nel,2)
	    dya=0.00000											!février 2004
	    wya=0.00000											!février 2004
	    tya=0.00000											!février 2004
	  elseif(section.eq.'DOUBLET')then					!février 2004
	    isect1=3												!février 2004
          read(55,*,end=900) hya,dya,wya,tya,corro(nel,2)
	    epais=0.0000										!février 2004
	  endif												!février 2004
        hxr=0.001												!février 2004
	  dxr=0.001												!février 2004
	  wxr=0.001												!février 2004
	  txr=0.001												!février 2004
	  epsr=hight/5			!sept06							!février 2004
	  entr=hight/5			!sept06							!février 2004
	  epsa2=0.00000											!février 2004
	  entr2=0.00000											!février 2004
	endif													!février 2004

      if((epsa2.ge.(0.00001)).or.(entr2.ge.(0.00001))) then
        read(55,*,end=900) hya2,dya2,wya2,tya2,ksa2,
     *            itype_transv_second(nel),name_type_transv_second(nel)
        read(55,*,end=900) hxr2,dxr2,wxr2,txr2,ksr2,
     *            itype_longit_second(nel),name_type_longit_second(nel)
      endif

	if(types.ne.'EPONTILLE') then							!février 2004
        read(55,*,end=900) bid   !lecture de mode (bypass)
	else													!février 2004
	  bid='EE1'												!février 2004
	endif												!février 2004
      if(mode.eq.'EE1'.or.mode.eq.'ES4') then
            xmode=-1.0
      elseif(mode.eq.'ES1'.or.mode.eq.'ES3'.or.
     *      mode.eq.'EC1'.or.mode.eq.'EL3')  then
            xmode=-0.5
      elseif(mode.eq.'SL1'.or.mode.eq.'SL2'.or.mode.eq.'EL2') then
            xmode=+0.5
      elseif(mode.eq.'LL1') then
            xmode=+1.0
      else
            xmode=0.0
      endif

      denom = (1.0/entr  + xmode/(dabs(phil)*q*pi1) )

 	if((denom.le.0.0).or.(entr/(dabs(phil)*q*pi1).gt.1.01)) then
        mode='EE2'
	  xmode=0.
        denom = 1.0/entr  ! --> epsr=entr
	endif
      epsr = 1.0/denom
  
c-----------------------------------------------------------------------
c 1.c   utilisation des nouvelles valeurs des variables de conception.
c     ==================================================================

      do 123 i=1,nbrxi
        goto(124,125,120,126,127,128,121,129,130),nxi(i)
  124   delta=xicou(i+ntot)
        goto 123
  125   hya =xicou(i+ntot)
        goto 123
  120   dya =xicou(i+ntot)
        goto 123
  126   wya =xicou(i+ntot)
        goto 123
  127   epsa=xicou(i+ntot)
        goto 123
  128   hxr =xicou(i+ntot)
        goto 123
  121   dxr =xicou(i+ntot)
        goto 123
  129   wxr =xicou(i+ntot)
        goto 123
  130   epsr=xicou(i+ntot)

        temp= ((1.0/epsr) - (xmode/(dabs(phil)*q*pi1)) )
 	  if(temp.le.0.0) then
	      write(666,*)
	      write(666,*)'attention : error dans le panneau ',nel
	      write(666,*)'  (subr. copy)'
	      write(666,*)'****************************************'
            write(666,*)'le paramètre mode est incompatible avec les'
	      write(666,*)'bornes de variation de l''entredistance entre'
	      write(666,*)'les raidisseurs et la largeur du panneau.'
	      write(666,*)'vous devez corriger cette situation !!'
	      write(666,*)'essayer en modifiant le paramère mode'
	      write(666,*)' - actuellement on a mode =',mode
	      write(666,*)' - prenez plutôt     mode = ee2'
	      write(*,*)
	      write(*,*)'attention : error dans le panneau ',nel
	      write(*,*)'  (subr. copy)'
	      write(*,*)'****************************************'
            write(*,*)'le paramètre mode est incompatible avec les'
	      write(*,*)'bornes de variation de l''entredistance entre'
	      write(*,*)'les raidisseurs et la largeur du panneau.'
	      write(*,*)'vous devez corriger cette situation !!'
	      write(*,*)'essayer en modifiant le paramère mode'
	      write(*,*)' - actuellement on a mode =',mode
	      write(*,*)' - prenez plutôt     mode = ee2'


	      write(6970,*)
	      write(6970,*)'attention : error dans le panneau ',nel
	      write(6970,*)'  (subr. copy)'
	      write(6970,*)'****************************************'
            write(6970,*)'le paramètre mode est incompatible avec les'
	      write(6970,*)'bornes de variation de l''entredistance entre'
	      write(6970,*)'les raidisseurs et la largeur du panneau.'
	      write(6970,*)'vous devez corriger cette situation !!'
	      write(6970,*)'essayer en modifiant le paramère mode'
	      write(6970,*)' - actuellement on a mode =',mode
	      write(6970,*)' - prenez plutôt     mode = ee2'


	      write(29,*)														!sept06			!bug		
	      write(29,*)'attention : error dans le panneau ',nel				!sept06			!bug
	      write(29,*)'  (subr. copy)'										!sept06			!bug
	      write(29,*)'****************************************'				!sept06			!bug
            write(29,*)'le paramètre mode est incompatible avec les'			!sept06			!bug
	      write(29,*)'bornes de variation de l''entredistance entre'		!sept06			!bug
	      write(29,*)'les raidisseurs et la largeur du panneau.'			!sept06			!bug
	      write(29,*)'vous devez corriger cette situation !!'				!sept06			!bug
	      write(29,*)'essayer en modifiant le paramère mode'				!sept06			!bug
	      write(29,*)' - actuellement on a mode =',mode						!sept06			!bug
	      write(29,*)' - prenez plutôt     mode = ee2'						!sept06			!bug
		  pause 'stop!'
	      stop
	  endif
        entr=1.0/ temp
        if(nel.eq.1)  write(666,'(/a/1x,50(1h-)/a)') 
     *        ' espacement effectif entre raidisseurs long.(lisses)',
     *        ' (spacing to consider for the long. stiffeners)'

        write(666,'(a,i3,a,f8.4,a)') 
     *            ' panneau=',nel,': entredistance(raid.)=',entr,' (m)'

  123 continue

        tya=tfa(nel)
        txr=tfr(nel)

c-----------------------------------------------------------------------
c 1.d   impression du types de panneau : coque ou plaque
c     -----------------------------------------------
      write(77,'(a9,t15,a,i3)') types,'panneau nø ',nel			!février 2004

      if(types.eq.'PLAQUE') then
c       cas de la "plaque"
        write(77,'(f12.5,2x,f8.5,t50,a)')hight,delta,'haut.,epaiss.(m)'
      elseif(types.eq.'EPONTILLE') then									!février 2004
c	   cas de l'"épontille"												!février 2004
      write(77,'(f12.5,2x,f8.5,t50,a)')hight,epais,'haut.(m),epaiss.(m)'  !février 2004
	if(section.eq.'CERCLE') write(77,'(a6)') 'CERCLE'                   !sept06	
	if(section.eq.'CARRE') write(77,'(a5)') 'CARRE'						!sept06
	if(section.eq.'DOUBLET') write(77,'(a7)') 'DOUBLET'					!sept06
      else
c       cas de la "coque"
        write(77,'(f10.5,1x,e14.7,1x,f8.5,t50,a)') 
     *                    phil,q,delta,'angl.(deg), rayon et epaiss (m)'
      endif

c 1.e   impression relatives au raidissage (aig. et raid)
c     ----------------------------------------------
      
	if (types.ne.'EPONTILLE') then	
	

       write(77,'(5(f9.6,1x),t60,a)')epsa,entr,epsa2,entr2,corro(nel,1),		
     *      'entr. aig. et raid, idem raid. compl.; ép. corrosion tôle'				

       write(77,'(5(f9.6,1x),i2,1x,a,t60,a)') 
     *                     hya,dya,wya,tya_dcns(nel), corro(nel,2),
     *                     itype_transv(nel),name_type_transv(nel),
     *            '  Cadres Transversaux (m). Hauteur âme. Epaisseur âme. 
     *Largeur semelle. Epaisseur semelle; ép. corrosion cadre; type de 
     *profilé; nom profilé'	

       write(77,'(5(f9.6,1x),i2,1x,a,t60,a)') 
     *                     hxr,dxr,wxr,txr_dcns(nel), corro(nel,3),
     *                     itype_longit(nel),name_type_longit(nel),
     *            '  Raidisseurs  Long.  (m). Hauteur âme. Epaisseur âme. 
     *Largeur semelle. Epaisseur semelle; ép. corrosion raidisseur; 
     *type de profilé; nom profilé'


											!février 2004
!	  write(77,'(5(f9.6,1x),t60,a)')epsa,entr,epsa2,entr2,corro(nel,1),		!fev2007
!    *            'entr. aig. et raid, idem raid. compl. (m)'					!fev2007
!       write(77,'(5(f9.6,1x),t60,a)') hya,dya,wya,tya,corro(nel,2),			!fev2007
!    *            'cadres transversaux (m)'									!fev2007
!       write(77,'(5(f9.6,1x),t60,a)') hxr,dxr,wxr,txr,corro(nel,3),			!fev2007
!    *            'raidisseurs  long.  (m)'

        if((epsa2.ge.(0.00001)).or.(entr2.ge.(0.00001))) then
          write(77,'(4(f9.6,1x),2x,i2,1x,i2,1x,a,t50,a)') 
     *               hya2,dya2,wya2,tya2,ksa2,
     *            itype_transv_second(nel),name_type_transv_second(nel),
     *          '  cadres transversaux compl. (m)'

          write(77,'(4(f9.6,1x),2x,i2,1x,i2,1x,a,t50,a)') 
     *               hxr2,dxr2,wxr2,txr2,ksr2,
     *            itype_longit_second(nel),name_type_longit_second(nel),
     *          '  raidisseurs long. compl. (m)'
        endif
        write(77,'(a3,t50,a)') mode,'code : répartition raid. long.'
	else  
	
	  write(77,'(2(f9.6),27x,a)') epsa,heff,                     !février 2004
     *	       'ecartement, hauteur sans hiloire'                !février 2004
	  if (section.ne.'DOUBLET') then                             !février 2004
	    write(77,'(2(f9.6,1x),i2,1x,a)') hya,corro(nel,2)
!     *                      ,itype_transv(nel),name_type_transv(nel)                                   !février 2004
	  else                                                       !février 2004
	    write(77,'(5(f9.6,1x),i2,1x,a)') hya,dya,wya,tya,corro(nel,2)
!     *                      ,itype_transv(nel),name_type_transv(nel)
                                                                   !février 2004
	  endif                                                      !février 2004
	endif                                                        !février 2004

c-----------------------------------------------------------------------
c-----------------------------------------------------------------------
c
c     lecture du coefficient de participation			             !coefficient de participation
c	=======================================
	if(types.ne.'EPONTILLE') then 
!	  read(55,*,end=900) part1		                             !coefficient de participation  !février 2004           !redressage !septembre 2007
	  read(55,*,end=900) part1,code,fami,lot                     !coefficient de participation                          !redressage !septembre 2007
!	  write(77,'(f12.7,t45,a)') part1,'coeff. de participation'  !coefficient de participation  !février 2004 !redressage !septembre 2007
	  write(77,'(f12.7,2x,i2,2x,i2,2x,i2,t37,a)') 
     *  part1,code,fami,lot,                                                          !redressage !septembre 2007
     *    'Coefficient de participation. Code de position. Famille. Lot'				!coefficient de participation  !février 2004  !redressage !septembre 2007
      read(55,*,end=900)  rend_panel1
	write(77,'(f12.7)') rend_panel1
	endif
c  2. suite de la lecture des données (caract. mat., charge et angle)
c     ================================
      read(55,*,end=900) e1,eta1,sigy1,sigm1,spec1
      read(55,*,end=900) mt,ksr,ksa,kst,kse,ipa,ivaria
      read(55,*,end=900) angle1

      write(77,'(e14.7,1x,f6.3,1x,3(e14.7,1x),t70,a)') 
     *       e1,eta1,sigy1,sigm1,spec1,
     *       'e, poisson, sy, sadm, poids spec'
      write(77,'(i2,3x,4(i2,1x),4x,i2,3x,i2,t45,a)') 
     *    mt,ksr,ksa,kst,kse,ipa,ivaria,'mt,ksr,ksa,kst,kse,ipa,ivaria'
      write(77,'(f12.7,t45,a)') angle1,'angle (degré))'

      do 195 i=1,nsolm
        if(ivaria.eq.1) goto 191
        read(55,*,end=900) xi
        write(77,'(f12.4,t45,a,i2,a)') xi,'xi  cas nø',i,' (ivaria=0)'
        goto 192
  191   read(55,*,end=900) xi,xf
        write(77,'(f12.4,1x,f12.4,t45,a,i2,a)') xi,xf,
     *            'xi, xf  cas nø',i,' (ivaria=1)'
  192   continue
  195 continue

      read(55,*,end=900) ploc1
      write(77,'(f12.4,t45,a)') 
     *          ploc1,'ploc = pression max locale (lisse et maille)'

c 2.6  lecture des charges variables selon ox
c     -------------------------------------------
      read(55,*,end=900)icha9
      write(77,'(i2,t45,a,a)') icha9,'charge variable selon x',
     *      ' (= 0 pas de surcharges, =1 avec surcharge)'
      if(icha9.gt.0) then
      do 215 is=1,nsolm
	  read (55,'(5a20)') text
        write(77,'(5a20)') text
        read(55,*,end=900) npt9
        write(77,'(2x,i2,t45,a,a)') npt9,
     *            'nbre d''intervalles'
        do 211 i=1,npt9
          read(55,*,end=900) cha9
          write(77,'(2x,f12.4,4x,f10.4,1x,f10.4,t45,a)') cha9,
     *            'charges : cha1 (n/m) et cha2,cha3 (m)'
 211    continue
 215  continue
      endif

c 2.7 lecture des noh
c     -----------------
      read(55,*,end=900) noh9
      write(77,'(10(i3,1x),t45,a)') 
     *     noh9,'liste des 10 panneaux qui suivent'

c 2.8 lectures des traverses
c     ------------------------
      if(mt.eq.0) goto 41
      do 2001 i=1,mt
        read(55,*,end=900) 
     *hxtr1,dxtr1,wxtr1,txtr1,abtr1,itype_tr1,name_type_tr1
        write(77,'(4(f8.6,1x),3x,f10.6,t55,i2,1x,a20,5x,a)') 
     *            hxtr1,dxtr1,wxtr1,txtr1,abtr1,
     *            itype_tr1,name_type_tr1,
     *            ' Raidisseurs longitudinaux (m)'
 2001 continue
  41  continue

c ______________________________________________________________________
c ______________________________________________________________________
c 3.    lecture de variables de conception , xi, ximax et ximin
c     ==========================================================
	read (55,'(5a20)') text
      write(77,'(5a16)') 'design variables' 
      read(55,*,end=900) nbrxi
      write(77,'(i2,t10,a)') nbrxi,'nbre de var. de conception'

c      if(nbrxi.ge.1) then
c        read(55,*,end=900) (nxi(i),i=1,nbrxi)
	if(nbrxi.ne.0) then								!eugen	!fev2007
	  read(55,*,end=900) (nxi(i),i=1,nbrxi)			!eugen	!fev2007
c       lecture de nxi(i) (nø des var. de conception)
        write(77,501)(nxi(i),i=1,nbrxi)
 501    format(9(i2,1x),t30,'nø des var. de conception')
	endif											!eugen	!fev2007
c       lecture des ximax et ximin
	  read (55,*,end=900) (dvmin(i),i=1,9)		!eugen	!fev2007
	  read (55,*,end=900) (dvmax(i),i=1,9)		!eugen	!fev2007
	  write(77,502)(dvmin(i),i=1,9)				!eugen	!fev2007
	  write(77,502)(dvmax(i),i=1,9)				!eugen	!fev2007
 502    format(9(f8.6,1x))						!eugen	!fev2007
c       bornes de epsr (entredistance moyenne) = vrai variable de conception = ximax
c       bornes de entr (entredistance réelle)  = valeur dans le fichier données = xmax
c        do 2345 i=1,nbrxi
c           xmax(i)=ximax(ntot+i)
c           xmin(i)=ximin(ntot+i)
c           if(nxi(i).eq.9) then
c             temp=xmode/(dabs(phil)*q*pi1)
c	       denom1= (1.0/xmax(i)) - temp 
c	       denom2= (1.0/xmin(i)) - temp 
c 	       if((denom1.lt.0.0).or.(denom2.lt.0.0)) then
c	       write(666,*)
c	       write(666,*)'attention : error dans le panneau ',nel
c	       write(666,*)'subr copy'
c	       write(666,*)'****************************************'
c             write(666,*)'le paramètre mode est incompatible ou'
c	       write(666,*)'les bornes min max sont trop incorrectes.'
c	       write(666,*)'vous devez corriger cette situation !!'
c	       write(666,*)'essayer en modifiant le paramère mode'
c	       write(666,*)' - actuellement on a mode =',mode
c	       write(666,*)' - prenez plutôt     mode = ee2'
c	       write(*,*)
c	       write(*,*)'attention : error dans le panneau ',nel
c	       write(*,*)'subr copy'
c	       write(*,*)'****************************************'
c             write(*,*)'le paramètre mode est incompatible ou'
c	       write(*,*)'les bornes min max sont trop incorrectes.'
c	       write(*,*)'vous devez corriger cette situation !!'
c	       write(*,*)'essayer en modifiant le paramère mode'
c	       write(*,*)' - actuellement on a mode =',mode
c	       write(*,*)' - prenez plutôt     mode = ee2'
c	       write(29,*)															!bug	
c	       write(29,*)'attention : error dans le panneau ',nel					!bug
c	       write(29,*)'subr copy'												!bug
c	       write(29,*)'****************************************'				!bug
c             write(29,*)'le paramètre mode est incompatible ou'					!bug
c	       write(29,*)'les bornes min max sont trop incorrectes.'				!bug
c	       write(29,*)'vous devez corriger cette situation !!'					!bug
c	       write(29,*)'essayer en modifiant le paramère mode'					!bug
c	       write(29,*)' - actuellement on a mode =',mode						!bug
c	       write(29,*)' - prenez plutôt     mode = ee2'							!bug
		  
c		   pause 'stop!'
c	       stop
c	    endif

c             xmax(i)=1.0/ denom1
c             xmin(i)=1.0/ denom2
c          endif
c 2345   continue

c        write(77,5021)(xmin(i),i=1,nbrxi)
c        write(77,5022)(xmax(i),i=1,nbrxi)

c 5021   format(9(f9.5,1x),' ximin')
c 5022   format(9(f9.5,1x),' ximax')
c        endif										!eugen	!fev2007

        ntot=ntot+nbrxi

c 3.2 restrictions structurelles: 
c     ----------------------------
	read (55,'(5a20)') text
      write(77,'(5a22)') 'structural constraints'
      read (55,*,end=900) im1  
      write(77,'(i2,t10,2a)') im1,'(0 without structural constraints',
     *                           ', 1 with structural constraints)'
      if(im1.le.0) goto 516

      read (55,*,end=900) ipt
      read (55,*,end=900) (ypt(i),i=1,ipt)
      write(77,'(3x,i2,t40,a)')ipt,
     *               'nbre de pts pour le calcul des sensibilités'
      write(77,601)(ypt(i),i=1,ipt)
  601 format(10(f9.4,1x),
     *            '  pts choisis pour le calcul des sensibilités')

      do 517 is=1,nsolm
	read (55,'(5a20)') text
      write(77,'(5a20)') text
      read (55,*,end=900) m1
      write(77,'(i2,t10,a)') m1,'nbre de restrictions structurelles'
      if((m1.gt.0).and.(m1.le.20)) then
        do 515 i=1,m1
c         lecture de
c             - icn,lcont9(1,i)  nø de réf de la contr.icn
c             - cjmax(i),inv(i) c(max) : c<cmax
c             - lcont9(2,i)              nø du pt d'appl. ypt de la contr  
          read (55,*,end=900)icn,lcont9(1),cjmax1,inv1,lcont9(2) 
          write(77,'(2x,i3,t8,i3,3x,e14.7,2x,i2,2x,i2,2x,a)') 
     *                  i,lcont9(1),cjmax1,inv1,lcont9(2),
     *          '(#	constraint type		cjmax	inv   ipt)'
  515   continue
      endif
  517 continue
  516 continue

c 3.4 lecture des m2 restrictions géométriques (ibm2 = nø du set de rest
c     ------------------------------------------------------------------
c     m2 =  nbre de restr. géom. du panneau nel

	read (55,'(5a20)') text
      write(77,'(5a23)') 'geometrical constraints'
      read (55,*,end=900) m2
      write(77,'(i2,t10,a)') m2,'m2=nbre de restrictions geometriques'

      if(m2.eq.99) then       ! cas ou un set de restrictions est choisi
        read (55,*,end=900) ibm2
        read (55,*,end=900) isema(nel),isemr(nel)
        write(77,'(i3,t20,a)') ibm2,'ibm2= nø du set de restr. géom.)'
        write(77,'(i3,2x,i3,t20,a)') isema(nel),isemr(nel),'isema,isemr'
      else if(m2.ne.0) then ! cas ou les restriction sont définies individuellement
        read (55,*,end=900) (lm2(i),i=1,m2)
        read (55,*,end=900) isema(nel),isemr(nel)
        write(77,503)(lm2(i),i=1,m2)
 503    format(20(i3,1x),'nø des m2 restrictions géométriques')
        write(77,'(i3,2x,i3,t20,a)') isema(nel),isemr(nel),'isema,isemr'
      endif

c 4.0 sauvetage pour dessins des données courantes (pour visionn.for)
c     ---------------------------------------------------------------
      if(dessin.ne.0) then
	  write(46) delta,hya,dya,wya,tya_dcns(nel),epsa,hxr,dxr,wxr,
     *            txr_dcns(nel),epsr
      endif

c ----------------------------------------------------------------------------
      return

  900 write(9 ,*)'erreur de lecture : "end of file"'
      pause 'stop'      
	stop

c 6.  les formats.
c     ============
      end
