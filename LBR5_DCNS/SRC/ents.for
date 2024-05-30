      subroutine ents(nel,width,ibusc,dessin,pot,ipoids,itera,
     *                iprint,nbrxi,nsolm,nsol,iff,nxi,length,
     *                txr_dcns,tya_dcns)			!r&d15                                                                                                 !coefficient de participation
      use sharedvar
      implicit real *8 (a-h,o-z)
      integer *2 dessin,code			!r&d15
	integer *2 fami					!redressage !septembre 2007
	real*8 length					!r&d15
      character*9 types
      character*7 section
      character*3 mode,mod,gfh
      character*80 texte
      dimension abtr2(10)
      dimension xi(nsol),xf(nsol),xxi(nsol),xxf(nsol),chamax(5,nsol)
      dimension nxi(9),dpoids(9)
      dimension m11(20)
      dimension cjmax9(ismax,nmax,m1max)
	dimension dchamax(5,nsol,9),dt1(9),dt2(9),dt3(9),dt4(9),dt5(9)   !!!aout04
	dimension dvmin(9),dvmax(9)				!eugen	!fev2007
	double precision txr_dcns(nmax)
	double precision tya_dcns(nmax)

c***********************************************************************
c  subroutine ents
c  ***************
c  subroutine d'introduction des donnees relatives a chaque panneau,
c  ecriture des donnees,calcul des coefficients de raideur du panneau,
c  introduction du poids propre sur les panneaux horizontaux.
c
c  création :thèse ph. rigo, l.b.r.-3 (1987)
c  modif:   1994   l.b.r-5
c    : 8- 5-96  charge max pour subr hughes chamax(5,is); § 4.7
c    : 4- 6-96  subroutine geom (restrictions géométriques de rahman);§ 3.4
c    : 3- 3-97  definition par panneau de e,eta,sigy, etc.; § 2.o
c    :          definition du mode de raidissage long. (entr, epsr et mode)

c    :13- 3-98  subroutine geom (restrictions géométriques de hughes); § 3.4
c    :30- 5-98  calcul du coût, §4.8 : subr cost,
c    :25- 5-99  lbr-5.1 : modification des common
c    :30-12-99  write(46) données graphiques pour visionn.for (§5.1)
c    :01-11-00  version anglaise + delt2
c    :16-11-00  ypt donnes en valeur relative 0<ypt<1 cad  (0<ypt<q*phil)
c    :20-11-00  subr select (calcul de chamax avec son signe)
c        11-01  calcul du coût
c     20-01-02  changement de la structures des données (lbr-5.6)
c    :04-02-04  données pour l'élément épontille
c
c  dernière modification:04.02.04
c***********************************************************************

      do i=1,nsolm
      do j=1,neto
      do k=1,m1max
       cjmax9(i,j,k)=cjmax3(i,j,k)
      enddo
      enddo
      enddo

c facteur de conversion en radians
      pi1=pi/180.
c
c 0.a   lecture du type de panneau : coque ou plaque
c     -----------------------------------------------
      read(55,2,end=900) texte
    2 format(a9)
      ! itype= 1  coque       coque  standard
      ! itype= 2  plaque      plaque standard
      ! itype= 3  coque1      coque  sans contribution transversale du bordé
      ! itype= 4  plaque1     plaque sans contribution transversale du bordé
      ! itype= 5  epontille   epontille élément poutre     !février 2004

      if(index(texte,'PLAQUE').ne.0) then
        types='PLAQUE'
        read(texte,'(t7,i1)',err=900) i
        if(i.eq.1) then
          itype(nel)=4
        else
          itype(nel)=2
        endif
      elseif(index(texte,'COQUE').ne.0) then
        types='COQUE'
        read(texte,'(t6,i1)',err=900) i
        if(i.eq.1) then
          itype(nel)=3
        else
          itype(nel)=1
        endif
      elseif (index(texte,'EPONTILLE').ne.0) then
        types='EPONTILLE'
        itype(nel)=5
      endif
      if(types.eq.'PLAQUE') then
        read(55,*,end=900) hight,delta
        q=10000000000.
        phil=hight/q/pi1
      elseif(types.eq.'EPONTILLE') then
        read(55,*,end=900) hight,epais
        read(55,'(a7)',end=900) section           !carré, cercle ou double t
        q=10000000000.
        phil=hight/q/pi1
        delta=0.d00
      else
        read(55,*,end=900) phil,q,delta
      endif

c 0.b   donnees relatives au raidissage (aig. et raid)
c     ----------------------------------------------
c     entr = entredistance réelle entre raid.=(d)raid
c     epsr = largeur collaborante moyenne compte tenu du nbre de raid. défini
c            par mode (type de répartition)
c     epsa=(d)aig

      if(itype(nel).ne.5) then
        read(55,*,end=900) epsa,entr,epsa2,entr2,corro(nel,1)			!aout2006	!juil07
        read(55,*,end=900) hya,dya,wya,tya, corro(nel,2),
     *                     itype_transv(nel),name_type_transv(nel)	!aout2006
        read(55,*,end=900) hxr,dxr,wxr,txr, corro(nel,3),
     *                     itype_longit(nel),name_type_longit(nel)	!aout2006
        epais=0.0000
        isect(nel)=0
      else
        read(55,*,end=900) epsa,heff       ! heff: hauteur sans hiloire
        if(section.eq.'CERCLE')then
          isect(nel)=1
          read(55,*,end=900) hya,corro(nel,2)
!          itype_transv(nel)

          dya=0.00000
          wya=0.00000
          tya=0.00000
          corro(nel,1)=0.	!aout2006									!corrosion
		corro(nel,3)=0. !aout2006
        elseif(section.eq.'CARRE')then
          isect(nel)=2
          read(55,*,end=900) hya,corro(nel,2)
!          itype_transv(nel)
          dya=0.00000
          wya=0.00000
          tya=0.00000
          corro(nel,1)=0.												!corrosion
		corro(nel,3)=0.
        elseif(section.eq.'DOUBLET')then
          isect(nel)=3
          read(55,*,end=900) hya,dya,wya,tya,corro(nel,2)
!          itype_transv(nel)
          epais=0.0000
          corro(nel,1)=0.												!corrosion
		corro(nel,3)=0. 
        endif
        hxr=0.001
        dxr=0.001
        wxr=0.001
        txr=0.001
        epsr=hight/5     !aout2006  epsr=1.000
        entr=hight/5     !aout2006  entr=1.000
        epsa2=0.00000
        entr2=0.00000
      endif

      tya_dcns(nel)=tya
      txr_dcns(nel)=txr


      if(epsa.le.(0.00001)) epsa=1.0
      if(entr.le.(0.00001)) entr=q*dabs(phil)*pi1

      if((epsa2.ge.(0.00001)).or.(entr2.ge.(0.00001))) then
        if(epsa2.le.(0.00001)) epsa2=1.0
        if(entr2.le.(0.00001)) entr2=q*dabs(phil)*pi1

        read(55,*,end=900) hya2,dya2,wya2,tya2,ksa2,
     *            itype_transv_second(nel),name_type_transv_second(nel)
        read(55,*,end=900) hxr2,dxr2,wxr2,txr2,ksr2,
     *            itype_longit_second(nel),name_type_longit_second(nel)

      endif

      if(itype(nel).ne.5) then
        if((iopti.eq.0).or.(itera.eq.0)) then		!sept2006
          read(55,*,end=900) mode
        else
          read(55,*,end=900) mod ! bidon
		mode=modes(nel)				!nov2006
        endif
      else
        mod='EE1'
        mode= 'EE1'
      endif

c      if((iopti.eq.0).or.(itera.eq.0)) then
c	modes(nel)=mode         ! lulu
c	else
c      mode=modes(nel)
c	endif

c     if((iopti.ne.0).and.(itera.ne.0)) then									!oct2006
c     mode=modes(nel)															!oct2006														
c	endif																	!oct2006
   

      if(mode.eq.'EE1'.or.mode.eq.'ES4') then
        xmode=-1.0
      elseif(mode.eq.'ES1'.or.mode.eq.'ES3'.or.
     *       mode.eq.'EC1'.or.mode.eq.'EL3') then
        xmode=-0.5
      elseif(mode.eq.'SL1'.or.mode.eq.'SL2'.or.mode.eq.'EL2') then
        xmode=+0.5
      elseif(mode.eq.'LL1') then
        xmode=+1.0
      else
        xmode=0.0
      endif

      denom = (1.0/entr  + xmode/(dabs(phil)*q*pi1) )

      if(denom.le.0.0) then
	  write(66,*)
	  write(66,*)'attention : error dans le panneau ',nel
	  write(66,*)'****************************************'
        write(66,*)'le paramètre mode =',mode,' est incompatible avec'
	  write(66,*)'    l''entredistance entre les raidisseurs=',entr
        write(66,*)'    et la largeur du panneau.'
	  write(66,*)'l''écartement entre les raidisseurs est '
	  write(66,*)'probablement supérieur à la largeur(l) du panneau.'
        write(66,*)'    entr=',entr,' et mode =',mode
	  write(66,*)'cela peut induire un epsr erroné (<0 ou infini)!!!'     
	  write(66,*)'il faut corriger cette situation!!'
        
	  write(29,*)														!bug	!sept2006
	  write(29,*)'attention : error dans le panneau ',nel				!bug	!sept2006
	  write(29,*)'****************************************'				!bug	!sept2006
        write(29,*)'le paramètre mode =',mode,' est incompatible avec'	!bug	!sept2006
	  write(29,*)'    l''entredistance entre les raidisseurs=',entr		!bug	!sept2006
        write(29,*)'    et la largeur du panneau.'						!bug	!sept2006
	  write(29,*)'l''écartement entre les raidisseurs est '				!bug	!sept2006
	  write(29,*)'probablement supérieur à la largeur(l) du panneau.'	!bug	!sept2006
        write(29,*)'    entr=',entr,' et mode =',mode						!bug	!sept2006
	  write(29,*)'cela peut induire un epsr erroné (<0 ou infini)!!!'	!bug	!sept2006
	  write(29,*)'il faut corriger cette situation!!'
	  	  
	  mode='EE2'
        xmode=0.
        epsr=entr
        denom = 1.0/entr
	  write(66,*)'correction automatique: on a'
	  write(66,*)'  mode  =',mode
	  write(66,*)'  xmode =',xmode
	  write(66,*)'  entr  =',entr
	  write(66,*)'  epsr  =',epsr
	  write(66,*)
      endif
      epsr = 1.0/denom

      if (entr/(dabs(phil)*q*pi1).gt.1.01)  then
	  write(66,*)
	  write(66,*)'attention : error dans le panneau ',nel
	  write(66,*)'****************************************'
        write(66,*)'le paramètre mode =',mode,' est incompatible avec'
	  write(66,*)'    l''entredistance entre les raidisseurs=',entr
        write(66,*)'    et la largeur du panneau.'
	  write(66,*)'l''écartement entre les raidisseurs est '
	  write(66,*)'probablement supérieur à la largeur(l) du panneau.'
        write(66,*)'    entr=',entr,' et mode =',mode
	  write(66,*)'cela peut induire un epsr erroné ou négatif!!!'
        write(66,*)'epsr=',epsr,'  !!!'
	  write(66,*)'il faut corriger cette situation!!'
	  write(66,*)
        
	  write(29,*)														!bug	!sept2006
	  write(29,*)'attention : error dans le panneau ',nel				!bug	!sept2006
	  write(29,*)'****************************************'				!bug	!sept2006
        write(29,*)'le paramètre mode =',mode,' est incompatible avec'	!bug	!sept2006
	  write(29,*)'    l''entredistance entre les raidisseurs=',entr		!bug	!sept2006
        write(29,*)'    et la largeur du panneau.'						!bug	!sept2006
	  write(29,*)'l''écartement entre les raidisseurs est '				!bug	!sept2006
	  write(29,*)'probablement supérieur à la largeur(l) du panneau.'	!bug	!sept2006
        write(29,*)'    entr=',entr,' et mode =',mode						!bug	!sept2006
	  write(29,*)'cela peut induire un epsr erroné ou négatif!!!'		!bug	!sept2006
        write(29,*)'epsr=',epsr,'  !!!'									!bug	!sept2006
	  write(29,*)'il faut corriger cette situation!!'					!bug	!sept2006
	  write(29,*)																!sept2006
	  
	  mode='EE2'
        xmode=0.
        epsr=entr
	  write(66,*)'correction automatique: on a'
	  write(66,*)'  mode  =',mode
	  write(66,*)'  xmode =',xmode
	  write(66,*)'  entr  =',entr
	  write(66,*)'  epsr  =',epsr
	  write(66,*)
      endif

	if((iopti.eq.0).or.(itera.eq.0)) then										!nov2006
	modes(nel)=mode         ! lulu												!nov2006
	endif																		!nov2006

      if((epsa2.ge.(0.00001)).or.(entr2.ge.(0.00001))) then
        epsr2=1.0/ ( 1.0/entr2 + xmode/(dabs(phil)*q*pi1) )
      else						!05.12.05										!sept2006
	  epsr2 = entr2				!05.12.05										!sept2006
	endif

      if(itype(nel).eq.5) goto 648                     !pas de division par 0              !février 2004
      qdelta=q/delta
      if(qdelta.gt.199.) goto 648
      write(66,649)qdelta

      if(qdelta.lt.101.)then
        write(66,'(/a,i3/15(1h=))')'panneau no ',nel
        write(* ,'(/a,i3/15(1h=))')'panneau no ',nel
        write(6970,'(/a,i3/15(1h=))')'panneau no ',nel
	  write(29,'(/a,i3/15(1h=))')'panneau no ',nel								!sept2006	
        write(66,*)' le rapport limite 1/100 est depasse.',
     *             ' excecution stoppee. '
	  write(29,*)' le rapport limite 1/100 est depasse.',			!bug		!sept2006
     *             ' excecution stoppee. '										!sept2006
        write(*,*)' le rayon est ',q,' et l''épaisseur est ',delta
        write(*,*)
        write(*,*)' le rapport limite 1/100 est depasse.',
     *            ' excecution stoppee. '

        write(6970,*)' le rayon est ',q,' et l''épaisseur est ',delta
        write(6970,*)
        write(6970,*)' le rapport limite 1/100 est depasse.',
     *            ' excecution stoppee. '


	  pause     'stop'															!sept2006
        iff=1        ! stop après subr. ent
      else
       write(66,'(/a,i3/15(1h=))')'panneau no ',nel
       write(* ,'(/a,i3/15(1h=))')'panneau no ',nel
       write(6970 ,'(/a,i3/15(1h=))')'panneau no ',nel
	 write(29,'(/a,i3/15(1h=))')'panneau no ',nel								!sept2006
       write(66,*)' le rapport limite est compris entre 1/100 et 1/200.'
       write(*,*) ' le rapport limite est compris entre 1/100 et 1/200.'
       write(6970,*) 
     *' le rapport limite est compris entre 1/100 et 1/200.'
	 write(29,*)' le rapport limite est compris entre 1/100 et 1/200.'			!sept2006	
       write(66,*)' attention :',
     *            ' la validite des resultats n''est plus assuree.'
       write(66,*)' *********  '
	 write(29,*)' attention :',											!bug	!sept2006
     *            ' la validite des resultats n''est plus assuree.'		!bug	!sept2006
       write(29,*)' *********  '													!sept2006
       write(*,*) ' attention :',
     *            ' la validite des resultats n''est plus assuree.'

       write(6970,*) ' attention :',
     *            ' la validite des resultats n''est plus assuree.'

      endif
  648 continue

c     impression du numero du panneau (titre)
c     ---------------------------------------
      if(langue.eq.1) then
	  write(66,4)nel,types
   4    format(/' panneau no ',i3,' (c''est une "',a9,'")'/36(1h=))		!février 2004
      else
        write(66,'(/a,i3,a,a9,a/36(1h=))')								!février 2004
     *          'panel no ',nel,' (it is a "',types,'")'
      endif

c------------------------------------------------------------------------------
c 1.       transfer des nouvelles valeurs des variables de conception (cfr opti).
c     (itération nø 2 et suivante)
c     ==================================================================
      if((iopti.ge.1).and.(itera.gt.0)) then		!sept2006

        do 123 i=1,nbrxi
          goto(124,125,120,126,127,128,121,129,130),nxi(i)
  124     delta=xicou(i+ntot)
          goto 123
  125     hya=xicou(i+ntot)
          goto 123
  120     dya=xicou(i+ntot)
          goto 123
  126     wya=xicou(i+ntot)
          goto 123
  127     epsa=xicou(i+ntot)
          goto 123
  128     hxr=xicou(i+ntot)
          goto 123
  121     dxr=xicou(i+ntot)
          goto 123
  129     wxr=xicou(i+ntot)
          goto 123
  130     epsr=xicou(i+ntot)
          entr=1.0/( 1.0/epsr - xmode/(dabs(phil)*q*pi1) )
  123   continue

        tya=tfa(nel)
        txr=tfr(nel)
      endif
c
c-----------------------------------------------------------------------

c     lecture du coefficient de participation et du code de position			!coefficient de participation		!r&d15
c       ============================================================
c     if(itype(nel).ne.5) read(55,*,end=900) part(nel),code					!coefficient de participation		!r&d15   !redressage !septembre 2007
	if(itype(nel).ne.5)  then
	   read(55,*,end=900) part(nel),code,fami,lot		    !coefficient de participation +              !redressage !septembre 2007
	   read(55,*,end=900) rend_panel(nel)
	endif


c  2. suite de la lectures des donnees: caract. des mat., charges, angle,...)
c     =======================================================================
      read(55,*,end=900) e(nel),eta(nel),sigy(nel),coefk(nel),spec(nel)		!r&d15

      if(langue==1) write(66,420) e(nel),eta(nel),sigy(nel),coefk(nel),		!r&d15
     *                            spec(nel)
      if(langue==2) write(66,421) e(nel),eta(nel),sigy(nel),coefk(nel),		!r&d15
     *                            spec(nel)
      if(itype(nel).eq.5)then
        eta(nel)=0.000 
      endif                                                                                          !février 2004
      sh=e(nel)/(2.*(1.+eta(nel)))
  420 format( ' module de young       = ',e14.7,' n/m2'/
     *        ' coefficient de poisson= ',f6.3/
     *        ' limite elastique      = ',e14.7,' n/m2'/
     *        ' coefficient de materiau = ',e14.7/
     *        ' poids specifique      = ',e14.7,' n/m3' )
  421 format( ' young  modulus        = ',e14.7,' n/m2'/
     *        ' poisson coefficient   = ',f6.3/
     *        ' yield stress          = ',e14.7,' n/m2'/
     *        ' material factor      = ',e14.7/
     *        ' specific weight       = ',e14.7,' n/m3' )

      read(55,*,end=900) mt,ksr,ksa,kst,kse,ipa,ivaria
      read(55,*,end=900) angle(nel)

c 2.1 charges xi,xf
c     -------------
      call annuld(xi,nsol)
      call annuld(xf,nsol)
      call annuld(xxi,nsol)
      call annuld(xxf,nsol)

      is=0
      do 195 i=1,nsolm
        icc=0
        do k=1,nsol
          if (nnsol(k).eq.i) icc=1  ! icc=1 cas de charge retenu
        enddo
        if (icc.eq.0) then           ! icc=0 cas de charge pas retenu
          read(55,*,end=900) temp
          goto 195
        else
          is=is+1
        endif
        if(ivaria.eq.1) goto 191
        read(55,*,end=900) xi(is)
        goto 192
  191   read(55,*,end=900) xi(is),xf(is)
  192   ang=angle(nel)
        if(ivaria.eq.0) then                     ! pression variant avec la prof.
          if(types.ne.'COQUE') then             ! plaque et epontille              !février 2004
            xf(is)=xi(is)+dabs(hight)*dsin(ang*pi1)
            if(i.eq.nsolm) ivaria=1
          else                                   ! coque
            teta=90.-ang                         ! calculé pour subr hughes
            xf(is)=xi(is)+q*(dsin((teta-phil)*pi1)-dsin(teta*pi1)) ! 7-5-96
          endif
        endif
        xxi(is)=xi(is)
        xxf(is)=xf(is)
  195 continue

      read(55,*,end=900) ploc(nel)

c 2.2 panneau modifie car phil>0 n'est pas compatible avec hydros (isign=-1)
c     ----------------------------------------------------------------
      if(phil.le.0.) goto 80
      teta=-angle(nel)-90.
      nsign(nel)=-1
      do 196 i=1,nsol
        if(ivaria.eq.0) goto 193
        xi(i)=xf(i)
        xf(i)=xxi(i)
        goto 194
  193   xi(i)=xi(i)+q*(dsin((teta-phil)*pi1)-dsin(teta*pi1))
        xf(i)=xxi(i)              ! calculé pour subr hughes, 7-5-96
  194   continue
  196 continue
      if(angle(nel).lt.-0.00001) goto 751
      if(angle(nel).gt.0.00001) goto 752
      angle(nel)=180.
      goto 753
 751  angle(nel)=angle(nel)+180.
      goto 753
 752  angle(nel)=angle(nel)-180.
 753  continue
      if(types.ne.'COQUE') goto 81
      teta=teta-phil
      angle(nel)=angle(nel)+phil
      goto 81
   80 teta=-angle(nel)+90.
      nsign(nel)=1
   81 continue

c 2.3 test sur teta (-180. < teta < 180. )
c     --------------------------------------
      if(dabs(teta).lt.180.) goto 721
      if(teta.ge.0.) goto 722
      teta=teta+360.
      goto 721
 722  teta=teta-360.
 721  continue

c 2.4 mise en memoire pour la subroutine equil
c     --------------------------------------
      tetaq(nel)=teta
      philn(nel)=phil
      qn(nel)=q

c 2.5 mise en memoire pour le calcul des coordonnees.
c     -----------------------------------------------
      if(phil.le.0) then
        tetas(nel)=-ang+90.
      else
        tetas(nel)=-ang-90.
      endif
c
c 2.6 impressions diverses
c     ---------------------
      if(langue==1) then  ! francais
        write(66,801) delta,width
        if(types.ne.'COQUE') then
          write(66,803) hight
        else
          write(66,802) phil,q
        endif
        write(66,8) ang
 	  if(itype(nel).ne.5) then									!février 2004
        write(66,11) epsa,hya,dya,wya,tya
        write(66,13) entr,hxr,dxr,wxr,txr,epsr,mode,xmode
	  else													!février 2004
		write(66,804) section,epsa							!février 2004
		if(section.eq.'CERCLE')then							!février 2004
	      write(66,806) hya,epais							!février 2004
	 	elseif(section.eq.'CARRE')then						!février 2004
	      write(66,807) hya,epais							!février 2004
	 	elseif(section.eq.'DOUBLET')then					!février 2004
	      write(66,808) hya,dya,wya,tya						!février 2004
	    endif												!février 2004
	  endif											

        if((epsa2.ge.(0.00001)).or.(entr2.ge.(0.00001))) then
          write(66,14) epsa2,ksa2,hya2,dya2,wya2,tya2,
     *                 entr2,ksr2,hxr2,dxr2,wxr2,txr2
        endif

        write(66,1)
        do is=1,nsol
          if(ivaria.eq.1) then
            write(66,7)is,xxi(is),xxf(is)
          else
            write(66,5)is,xxi(is),xxf(is)
          endif
        enddo

        write(66,16) ploc(nel)
  16  format(/' pression locale maximale = ',f8.3,' m. (charge locale,',
     * 'ponctuelle, pour dimensionner les lisses et le bordé)')

      else   ! english
        write(66,901) delta,width
        if(types.ne.'COQUE') then
          write(66,903) hight
        else
          write(66,902) phil,q
        endif
        write(66,'(2a,f7.3,a)')
     *  '    angle at origin (y=0) between the horizontal right hand',
     *  ' side line and the panel (its tangent at y=0) = ',ang,' deg.'
	  if(itype(nel).ne.5) then									!février 2004
        write(66,904) epsa,hya,dya,wya,tya
        write(66,905) entr,hxr,dxr,wxr,txr,epsr,mode,xmode
	  else													!février 2004
		write(66,805) section,epsa							!février 2004
		if(section.eq.'CERCLE')then							!février 2004
	      write(66,809) hya,epais							!février 2004
	 	elseif(section.eq.'CARRE')then						!février 2004
	      write(66,810) hya,epais							!février 2004
	 	elseif(section.eq.'DOUBLET')then					!février 2004
	      write(66,811) hya,dya,wya,tya						!février 2004
	    endif												!février 2004
	  endif

        if((epsa2.ge.(0.00001)).or.(entr2.ge.(0.00001))) then
          write(66,906) epsa2,ksa2,hya2,dya2,wya2,tya2,
     *                  entr2,ksr2,hxr2,dxr2,wxr2,txr2
        endif

        write(66,'(/a/1x,40(1h+))')
     *           ' lateral hydrostatique pressure on the panel'
        do is=1,nsol
          if(ivaria.eq.1) then
            write(66,907)nnsol(is),xxi(is),xxf(is)
          else
            write(66,908)nnsol(is),xxi(is),xxf(is)
          endif
        enddo

        write(66,'(/a,f8.3,a,a/)') 'maximum local pressure =',ploc(nel),
     *     ' m. (local load, concentrated,',
     *     ' for sizing of stiffeners and plate thickness).' 

	endif

c 2.7  lecture des charges variables selon ox
c     -------------------------------------------
      read(55,*,end=900)icha(nel)

      do i=1,ismax         ! initialisation de npt (remplace sr annuli)
       npt(i)=0
      enddo

      do i=1,100           ! initialisation de cha (remplace sr annuld)
       do j=1,ismax
        do k=1,3
         cha(i,k,j)=0.d00
        enddo
        do k=1,9
         dcha(i,j,k)=0.d00
        enddo
       enddo
      enddo

      if(icha(nel).eq.0) goto 210
      is=0
      do 215 i=1,nsolm
        icc=0
        do k=1,nsol
          if (nnsol(k).eq.i) icc=1   ! icc=1 cas de charge retenu
        enddo
        if (icc.eq.0) then           ! icc=0 cas de charge pas retenu
          read(55,*,end=900) gfh    ! titre
          read(55,*,end=900) itemp   ! npt
          do j=1,itemp
            read(55,*,end=900) temp
          enddo
          goto 215
        else
          is=is+1
        endif

        read(55,*) gfh        ! ligne titre, ex: "cas de charge 3"
        read(55,*,end=900) npt(is)
        write(66,212)nnsol(is),npt(is)
        if(npt(is).gt.100) then
          write(*,*)  ' *** error in panel ',nel
          write(*,*)  ' charges variables selon ox (max=100 pas)'

          write(6970,*)  ' *** error in panel ',nel
          write(6970,*)  ' charges variables selon ox (max=100 pas)'

          write(29,*)  ' *** error in panel ',nel						!bug	!sept2006
	    write(29,*)  ' charges variables selon ox (max=100 pas)'	!bug	!sept2006
 	    write(666,*)' charges variables selon ox (max=100 pas)'
          write(666,*)' load distributed along ox (max=100 steps)'
          iff=1
		pause 'error'
        endif
        write(66,213)
        do  j=1,npt(is)
          read(55,*,end=900) (cha(j,ij,is),ij=1,3)
        enddo
        if(kse.eq.0) then
          kse=2
          xi(is)=0.
          xf(is)=0.
          call annuld(cha(1,2,is),nsol)
          call annuld(cha(1,3,is),nsol)
          write(66,218)nel
	    write(29,218)nel											!bug	!sept2006
          write(66,7)nnsol(is),xi(is),xf(is)
          write(*,*) 'panneau n° ',nel
          write(*,*) ' kse=0 est incompatible avec icha=1'
          write(*,*) ' nous proposons de prendre kse=2 avec'
          write(*,*) '       xi=xf=0  et  cha2=cha3=0 '
          write(*,*)' si oui taper "0"'
          write(*,*)' si non taper "1" '
          write(*,*)'    (c.à.d stop + corriger vos données)'

          write(6970,*) 'panneau n° ',nel
          write(6970,*) ' kse=0 est incompatible avec icha=1'
          write(6970,*) ' nous proposons de prendre kse=2 avec'
          write(6970,*) '       xi=xf=0  et  cha2=cha3=0 '
          write(6970,*)' si oui taper "0"'
          write(6970,*)' si non taper "1" '
          write(6970,*)'    (c.à.d stop + corriger vos données)'


          read (*,*) iff

		write(29,*) 'panneau n° ',nel											!bug	
          write(29,*) ' kse=0 est incompatible avec icha=1'						!bug
          write(29,*) ' nous proposons de prendre kse=2 avec'					!bug
          write(29,*) '       xi=xf=0  et  cha2=cha3=0 '						!bug
	    write(29,*)'    (c.à.d stop + corriger vos données)'					!bug
c		iff=1        
	  endif
	  do j=1,npt(is)
          write(66,214) j,cha(j,1,is),cha(j,2,is),cha(j,3,is)
        enddo
 215  continue

 210  continue

c 2.8 lecture des noh
c     -----------------
      read(55,*,end=900) (noh(nel,j),j=1,10)
      write(66,*)
	if(langue==1) then
       write(66,*)'liste des panneaux (noh) qui suivent celui-ci: '
	else
       write(66,*)'list of the panels (noh) that follows this one:'
	endif
      write(66,*)'------------------------------------------------'
      write(66,'(10i3)') (noh(nel,j),j=1,10)

c 2.9 lectures des traverses
c     ------------------------
      if(mt.eq.0) goto 41
      do 2001 i=1,mt
        read(55,*,end=900) hxtr(i),dxtr(i),wxtr(i),txtr(i),abtr(i),
     *                     itype_tr(i),name_type_tr(i)
        if (abtr(i).gt.dabs(phil)*q*pi1.or.abtr(i).lt.0) then           !juillet2003
          write(*,*)                                                    !juillet2003
          write(*,*)'attention : error dans le panneau ',nel            !juillet2003
          write(*,*)'****************************************'          !juillet2003
          write(*,*)'une traverse n''est pas positionnée sur le panneau'!juillet2003
          write(*,*)'vérifiez vos données'                              !juillet2003
          

          write(6970,*)                                                    !juillet2003
          write(6970,*)'attention : error dans le panneau ',nel            !juillet2003
          write(6970,*)'****************************************'          !juillet2003
          write(6970,*)
     *'une traverse n''est pas positionnée sur le panneau'!juillet2003
          write(6970,*)'vérifiez vos données'                              !juillet2003

		write(29,*)																!bug	!sept2006
	    write(29,*)'attention : error dans le panneau ',nel						!bug	!sept2006
	    write(29,*)'****************************************'					!bug	!sept2006
	    write(29,*)'une traverse n''est pas positionnée sur le panneau'			!bug	!sept2006
	    write(29,*)'vérifiez vos données'										!bug	!sept2006
	    
		stop                                                          !juillet2003
        endif                                                           !juillet2003
        abtr2(i)=abtr(i)
 2001 continue
      if(langue==1) write(66,39)
      if(langue==2) write(66,909)
      do 40 i=1,mt
        write(66,441) i,abtr(i), txtr(i), wxtr(i), dxtr(i), hxtr(i)
        abtr(i)=abtr(i)/(pi1*q)
        if(phil.gt.0.) abtr(i)=phil-abtr(i)
  40  continue
  41  continue

c 2.10 phil=dabs(phil)
c     ----------------
      phil=dabs(phil)

c 2.11 les 4 cas de base pour les effets de bords a(y**3)+b(y**2)+c*y+d
c      ----------------------------------------------------------------
      qphil=q*phil*pi1
      ddd=1000.
      ccc=ddd/qphil
      bbb=ccc/qphil
      aaa=bbb/qphil

      delto=width*0.05
      if(ibusc.ne.0) then
        write(66,6) delto
      endif

c ______________________________________________________________________
c ______________________________________________________________________
c 3.    lecture de variables de conception , xi, ximax et ximin
c     ==========================================================
      read(55,*,end=900) gfh                                 ! titre
      read(55,*,end=900) nbrxi                               ! nbrxi
c      if(nbrxi.ge.1) then
c        read(55,*,end=900) (nxi(i),i=1,nbrxi)                ! nxi
c        if(itera.eq.0)then
c          read (55,*,end=900) (ximin(i),i=ntot+1,ntot+nbrxi) ! xi min
c          read (55,*,end=900) (ximax(i),i=ntot+1,ntot+nbrxi) ! xi max
c        else
cc         itera>1 : on conserve les ximin et ximax modifiés dans subr. opti
c          read (55,*,end=900)
c          read (55,*,end=900)
c        endif
c      endif

	if(nbrxi.ne.0) read(55,*,end=900) (nxi(i),i=1,nbrxi)	!eugen	!fev2007
	if(itera.eq.0)then										!eugen	!fev2007
	  read (55,*,end=900) (dvmin(i),i=1,9)					!eugen	!fev2007
	  read (55,*,end=900) (dvmax(i),i=1,9)					!eugen	!fev2007
	  do i=1,nbrxi											!eugen	!fev2007					
	    ximin(ntot+i)=dvmin(nxi(i))							!eugen	!fev2007
	    ximax(ntot+i)=dvmax(nxi(i))							!eugen	!fev2007
	  enddo													!eugen	!fev2007
	else													!eugen	!fev2007
c       itera>1 : on conserve les ximin et ximax modifiés dans subr. opti
        read (55,*,end=900) temp                           ! xi min	!eugen	!fev2007
        read (55,*,end=900) temp                           ! xi max	!eugen	!fev2007
	endif							!eugen	!fev2007
	
	if(iopti.eq.0) nbrxi=0

c 3.1 vérification des variables de conception : ximin < xi < ximax
c     et sauvetage des variables de conception dans le vecteur xicou(n)
c     (lors de la première itération)
c     ----------------------------------------------------------------------
      if((itera.eq.0).and.(iopti.ge.1)) then
        write(666,283) nel,nbrxi
        write(666,'(a,9i2)') 'soit les variables locales:',
     *                         (nxi(i),i=1,nbrxi)
        do 131 i=ntot+1,ntot+nbrxi
          if(itype(nel).ne.5) then                                   !si plaques,coques              !février 2004
          goto(134,135,141,136,137,138,142,139,140),nxi(i-ntot)
  134     call borne(delta,i,'= épaisseur bordage          ',
     *               nel,ximin,ximax,iff)
          xicou(i)=delta
          goto 131
  135     call borne(hya,i,  '= hauteur âme transversaux   ',
     *               nel,ximin,ximax,iff)
          xicou(i)=hya
          goto 131
  141     call borne(dya,i,  '= épaisseur âme transversaux ',
     *               nel,ximin,ximax,iff)
          xicou(i)=dya
          goto 131
  136     call borne(wya,i,  '= largeur semelle transvers. ',
     *               nel,ximin,ximax,iff)
          xicou(i)=wya
          goto 131
  137     call borne(epsa,i, '= larg. effect. transversaux ',
     *               nel,ximin,ximax,iff)
          xicou(i)=epsa
          goto 131
  138     call borne(hxr,i,  '= hauteur âme raidisseurs    ',
     *               nel,ximin,ximax,iff)
          xicou(i)=hxr
          goto 131
  142     call borne(dxr,i,  '= épaisseur âme raidisseurs  ',
     *               nel,ximin,ximax,iff)
          xicou(i)=dxr
          goto 131
  139     call borne(wxr,i,  '= largeur semelle raidisseurs',
     *               nel,ximin,ximax,iff)
          xicou(i)=wxr
          goto 131
  140     continue  ! entredistance entre raidisseurs entr et epsr
          write(666,'(a,i3,a)') 'var. nø',i,': entr et epsr'
          write(666,'(a,e11.4,a,e11.4,a,e11.4)')
     *              'pour entr, les limites min et max sont:',
     *               ximin(i),' < entr=',entr,' < ',ximax(i)
          write(666,*) 'mode =',mode,' et xmode =',xmode
          temp= xmode/(dabs(phil)*q*pi1)
          denom1 = (1.0/ximax(i)  + temp )
          denom2 = (1.0/ximin(i)  + temp )
          if((denom1.lt.0.0).or.(denom2.le.0.0)) then
            write(666,*)
            write(666,*)'attention : error dans le panneau ',nel
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
            write(6970,*)'****************************************'
            write(6970,*)'le paramètre mode est incompatible avec les'
            write(6970,*)'bornes de variation de l''entredistance entre'
            write(6970,*)'les raidisseurs et la largeur du panneau.'
            write(6970,*)'vous devez corriger cette situation !!'
            write(6970,*)'essayer en modifiant le paramère mode'
            write(6970,*)' - actuellement on a mode =',mode
            write(6970,*)' - prenez plutôt     mode = ee2'
            
		  write(29,*)															!bug	!sept2006
	      write(29,*)'attention : error dans le panneau ',nel					!bug	!sept2006
	      write(29,*)'****************************************'					!bug	!sept2006
            write(29,*)'le paramètre mode est incompatible avec les'				!bug	!sept2006
	      write(29,*)'bornes de variation de l''entredistance entre'			!bug	!sept2006
	      write(29,*)'les raidisseurs et la largeur du panneau.'				!bug	!sept2006
	      write(29,*)'vous devez corriger cette situation !!'					!bug	!sept2006
	      write(29,*)'essayer en modifiant le paramère mode'					!bug	!sept2006
	      write(29,*)' - actuellement on a mode =',mode							!bug	!sept2006
	      write(29,*)' - prenez plutôt     mode = ee2'							!bug	!sept2006
	      
		  pause 'error'																	!sept2006
		  iff=1
          endif
            if (denom1.eq.0.) then
            write(666,*)
            write(666,*)'attention : error dans le panneau ',nel
            write(666,*)'****************************************'
            write(666,*)'la borne maxi de epsr est incompatible avec'
            write(666,*)'le parametre mode et la largeur du panneau.'
            write(666,*)'vous devez corriger cette situation !!'
            write(666,*)'essayer en modifiant le paramère mode'
            write(666,*)' - on avait       ximax =',ximax(i)
            write(*,*)  'attention : error dans le panneau ',nel
            write(*,*)  '****************************************'
            write(*,*)  ' - on avait       ximax =',ximax(i)

            write(6970,*)  'attention : error dans le panneau ',nel
            write(6970,*)  '****************************************'
            write(6970,*)  ' - on avait       ximax =',ximax(i)

            ximax(i)=ximax(i)*0.99
            denom1 = (1.0/ximax(i)  + temp )
            write(666,*)' - on change pour ximax =',ximax(i)
            write(*,*)  ' - on change pour ximax =',ximax(i)
            write(6970,*)  ' - on change pour ximax =',ximax(i)
          
		  write(29,*)															!bug
	      write(29,*)'attention : error dans le panneau ',nel					!bug
	      write(29,*)'****************************************'					!bug
            write(29,*)'la borne maxi de epsr est incompatible avec'				!bug
	      write(29,*)'le parametre mode et la largeur du panneau.'				!bug
	    
		endif

          write(666,*)'pour epsr (vrai variable de conception), on a:'
          ximax(i)= 1.0/ denom1
          ximin(i)= 1.0/ denom2
          call borne(epsr,i, '= larg. effect. raidisseurs  ',
     *               nel,ximin,ximax,iff)
          xicou(i)=epsr
         else
          goto(143,144,145,146,147),nxi(i-ntot)
  143     call borne(hya,i,  '= 1/2 hauteur âme, diam,côté ',
     *               nel,ximin,ximax,iff)
          xicou(i)=hya
          goto 131
  144     call borne(dya,i,  '= épaisseur âme épont.doublet',
     *               nel,ximin,ximax,iff)
          xicou(i)=dya
          goto 131
  145     call borne(wya,i,  '= largeur semel.épont.doublet',
     *               nel,ximin,ximax,iff)
          xicou(i)=wya
          goto 131
  146     call borne(epais,i,'=épaisseur paroi mince               ',
     *               nel,ximin,ximax,iff)
             xicou(i)=epais
             goto 131
  147     call borne(epsa,i, '= larg. effect. épontille        ',
     *               nel,ximin,ximax,iff)
          xicou(i)=epsa
          goto 131
         endif
  131   continue
        if((ntot+nbrxi).gt.m1tmax) then
          write(*,*)  'le nombre max de variables de conception est',
     *                ' dépassé :ntot>nvmax ',ntot,' > ',nvmax

          write(6970,*)  'le nombre max de variables de conception est',
     *                ' dépassé :ntot>nvmax ',ntot,' > ',nvmax

          write(666,*)'le nombre max de variables de conception est',
     *                ' dépassé :ntot>nvmax ',ntot,' > ',nvmax
          write(29,*)'le nombre max de variables de conception est',				!bug
     *                ' dépassé :ntot>nvmax ',ntot,' > ',nvmax					!bug
		
		pause 'error'
		iff=1 !stop à la sortie de la subr ent
        endif
      endif

c 3.2 restrictions structurelles: lecture des ipt (points de calcul)
c     ----------------------------------------------------------------
      read (55,*,end=900) gfh          ! titre sur restrictions structurelles:
      read (55,*,end=900) im1          ! im1 (nbre de restrictions struct.du panneau nel)

c     il faut tjrs avoir ypt(1)=0. et ypt(ipt)=phil (cfr subr. mdr2)
c     donc ipt(min) = 2   et ipt(max) = 10

      if(im1.eq.0) then  !im1=0 : pas de restrictions structurelles
        ipts(nel)=2
        ypts(1,nel)=0*dabs(phil)     !juin 2006 (spécifier les points de calcul même sans restrictions,..
        ypts(2,nel)=1*dabs(phil)     !juin 2006 ..on choisit le noeud de départ et d'arrivée)
        ipts2(1,nel)=1               !juin 2006 (pour éviter d'avoir des ipts2(i,nel)=0)
        ipts2(2,nel)=31              !juin 2006 (pour éviter d'avoir des ipts2(i,nel)=0)  
	   do iit=1,mt
            ipts3(iit,nel)=iit       !juin 2006 (mettre n'importe quelle valeur pour les traverses car  
         enddo                       !normalement elle ne ssera pas utilisé dans la suite du programme).  
        
	  goto 219          ! pas de restr struc. pour ce panneau: goto end of this section.

      else if(im1.eq.1) then
        read (55,*,end=900) ipts(nel)    ! ipt(nel)=nbre de pts pour le calcul des restrictions
        if(iopti.eq.0) goto 110
          if((ipts(nel).gt.4).or.(ipts(nel).lt.2)) then
               write(*,*)'**** panneau ',nel
               write(*,*)'ipt=',ipts(nel),
     *                ' est incorrecte, il faut 2=<ipt=<4)'

               write(6970,*)'**** panneau ',nel
               write(6970,*)'ipt=',ipts(nel),
     *                ' est incorrecte, il faut 2=<ipt=<4)'

               write(666,*)'ipt=',ipts(nel),
     *                  ' est incorrecte, il faut 2=<ipt=<4)'
               write(29,*)'ipt=',ipts(nel),											!bug
     *                  ' est incorrecte, il faut 2=<ipt=<4)'						!bug
		     pause 'error'
			 iff=1
          endif
 110    continue

        ! ypt = en valeur relative c.à.d.  (0<ypt<1) pour les donnees
        !       mais en valeur relle pour le programme  0<ypt<q*phil
        read (55,*,end=900) (ypts(i,nel),i=1,ipts(nel))  ! ypt (ordonnées relatives des pts)

        if(iopti.ge.1) then
          if((ypts(1,nel).ge.0.01).or.(ypts(ipts(nel),nel).le.0.99))then
            write(666,431) ! message d'erreur
            write(* , 431)
            write(6970 , 431)
		  write(29, 431)									!sept2006
          endif
          ypts(1,nel)  =0.0
          ypts(ipts(nel),nel)=1.0
          ! calcul des coordonnées réelles (= angle en degrés)
          do i =1,ipts(nel)
            ypts(i,nel)=ypts(i,nel)*dabs(phil)  ! (ordonnées ang. réelles des pts)
          enddo
        endif

      else if ((im1.le.-nel).or.(im1.gt.1)) then
        write(*,*)    ' *** panneau',nel
        write(*,*)  ' fausse valeur de im1 =',im1,'=< ',-nel
        write(*,*)  ' ou im1 > 1'
        write(*,*)  ' wrong value for im1 =',im1,'=< ',-nel
        write(*,*)  ' or im1 > 1'

        write(6970,*)    ' *** panneau',nel
        write(6970,*)  ' fausse valeur de im1 =',im1,'=< ',-nel
        write(6970,*)  ' ou im1 > 1'
        write(6970,*)  ' wrong value for im1 =',im1,'=< ',-nel
        write(6970,*)  ' or im1 > 1'

        write(666,*)' wrong value for im1 =',im1,'=< ',-nel
        write(666,*)' or im1 > 1'
        
	  write(29,*)    ' *** panneau',nel											!bug
	  write(29,*)  ' fausse valeur de im1 =',im1,'=< ',-nel						!bug
	  write(29,*)  ' ou im1 > 1'												!bug
 	  
	  iff=1
	  pause 'error'
        return

      else    ! im1<0 cad mêmes restrictions que celles du panneau im1
        im1=-im1
        ipts(nel)=ipts(im1)
        do i=1,ipts(nel)
           ypts (i,nel)=ypts (i,im1)*philn(nel)/philn(im1)
           ipts2(i,nel)=ipts2(i,im1)
           ipts3(i,nel)=ipts3(i,im1)
        enddo
        do is=1,nsol    ! is = n° d'un cas de charge selectionné
          ij=nnsol(is)  ! ij = n° de ce cas de charge comme defini dans les donnees
          m1=m1tabl(im1,is)
          m1tabl(nel,is)=m1
          m1cont(nel)=m1cont(nel)+m1  ! nbre de restriction pour tout les cas de charges (pour le panneau nel)
          m1tot =m1tot +m1  ! nbre de restriction pour tout les cas de charges (pour la structure)
          if(itera.eq.0) then
            do  i=1,m1
              lcont4(ij,nel,i,1)=lcont4(ij,im1,i,1)
              lcont4(ij,nel,i,2)=lcont4(ij,im1,i,2)
              cjmax9(ij,nel,i)  =cjmax9(ij,im1,i)
              inv3  (ij,nel,i)  =inv3  (ij,im1,i)
            enddo
            ! vérification de la compatibilité entre variable xi et restriction 4 (dmin)
            if((lcont4(ij,nel,i,1).eq.4).and.(nxi(1).ne.1)) then
              write(*  ,220)
              write(*  ,220)
              write(*  ,220)
              write(*  ,220)
              write(666,220)
              write(66 ,220)
			write(29 ,220)					!sept2006
            endif
            ! vérification du choix des points de calcul des sensibilités vis à vis de traverses
            if(mt.ge.1) then
              if((lcont4(ij,nel,i,1).ge.40).and.
     *          (lcont4(ij,nel,i,1).le.45))     then
                do it=1,mt
                  ax=dabs( abtr(it)/ypts(lcont4(ij,nel,i,2),nel) )
                  if(ax.ge.(0.99)) then                                !  ipt3= nø de la trav. concernee
                    if(ax.le.(1.01)) ipts3(lcont4(ij,nel,i,2),nel)=it
                  endif
                enddo
              endif
            endif

          endif
        enddo
        goto 219
      endif   ! if(im1.eq.0) then


      if((iopti.ge.1).and.(itera.eq.0)) then
        do i=1,ipts(nel)                                       ! pt. de calcul des restrictions
          ipts2(i,nel)=idnint(30.*ypts(i,nel)/dabs(phil)) + 1   ! c.à.d. un pt de 1 à 31
        enddo
      endif

c 3.3 lecture des m1 restrictions structurelles et de leur borne
c     -----------------------------------------------------------
      if(itera.eq.0) call annuli (ipts3(1,nel),iptmax)       !        ipt3=0

      is=0  ! is = compteur des cas de charge selectionnés (1 à nsol)

      do 216 ij=1,nsolm  ! ij = compteur des cas de charge, selectionnés ou non (1 à nsolm)
                         ! is = compteur des cas de charge selectionnés (1 à nsol)
      m11(ij)=0  ! mise à zéro
      icc=0      ! icc=0 cas de charge non selectionné

      do k=1,nsol
        if(nnsol(k).eq.ij) then
          icc=1   ! icc=1 cas de charge selectionné
          is=is+1
          m1tabl(nel,is)=0  ! mise à zéro
        endif
      enddo

      read (55,*,end=900) gfh              ! titre cas de charge
      read (55,*,end=900) m1               ! m1 (nbre de restrictions struct.du panneau nel)

c     lecture de la liste des restrictions (en fct de la valeur de m1)
c     lcont(nsolm,neto,m1max,2),inv(nsolm,neto,m1max),cjmax(nsolm,neto,m1max)

      if(m1.gt.20) then
        write(*,*)  ' *** panneau',nel
        write(*,*)  ' nbre de restr. struct. incorrecte (max=20) =',m1

        write(6970,*) ' *** panneau',nel
        write(6970,*) ' nbre de restr. struct. incorrecte (max=20) =',m1

        write(666,*)' nbre de restr. struct. incorrecte (max=20) =',m1
        write(666,*)' too much struct. const. (max=20 per panel) =',m1
        write(29,*)  ' *** panneau',nel													!bug
	  write(29,*)  ' nbre de restr. struct. incorrecte (max=20) =',m1					!bug
  	  iff=1
        pause 'error'
	  return

      else if (m1.eq.0) then
        goto 216          ! vers cas de charge suivant

      elseif((m1.ge.1).and.(m1.le.20)) then
c	   if (is.ne.0) then
	  if(icc.eq.1) m1tabl(nel,is)=m1
        m11(ij)=m1
c         endif
        if(itera.eq.0) then
          do  i=1,m1
            read (55,*,end=900) icn,lcont4(ij,nel,i,1),cjmax9(ij,nel,i),
     *                          inv3(ij,nel,i),  lcont4(ij,nel,i,2)
          enddo
        else
          do  i=1,m1
            read (55,*,end=900)
          enddo
        endif

      elseif((m1.le.-1).and.(m1.gt.-ij))  then
          iss=-m1    ! iss cas de charge de référence
          m1=m11(iss)
          m11       (ij)=m1
          if(icc.eq.1) m1tabl(nel,is)=m1
          if(itera.eq.0) then
            do  i=1,m1
              lcont4(ij,nel,i,1)=lcont4(iss,nel,i,1)
              cjmax9(ij,nel,i)  =cjmax9(iss,nel,i)
              inv3  (ij,nel,i)  =inv3  (iss,nel,i)
              lcont4(ij,nel,i,2)=lcont4(iss,nel,i,2)
            enddo
          endif

      elseif (m1.le.-ij) then
        write(*,*)  ' *** panneau',nel
        write(*,*)  ' fausse valeur de m1 =',m1,'=< ',-ij
        write(*,*)  ' wrong value for m1  =',m1,'=< ',-ij

        write(6970,*)  ' *** panneau',nel
        write(6970,*)  ' fausse valeur de m1 =',m1,'=< ',-ij
        write(6970,*)  ' wrong value for m1  =',m1,'=< ',-ij

        write(666,*)' wrong value for m1  =',m1,'=< ',-ij
        write(29,*)  ' *** panneau',nel										!bug	!sept2006
	  write(29,*)  ' fausse valeur de m1 =',m1,'=< ',-ij					!bug	!sept2006
	  iff=1
	  pause 'error'																	!sept2006
        stop 55
      endif


      if (icc.eq.1) then
        m1cont(nel)=m1cont(nel)+m1  ! nbre de restriction pour tous les cas de charges (pour le panneau nel)
        m1tot =m1tot +m1  ! nbre de restriction pour tous les cas de charges et toute la structure
      endif


      if((iopti.ge.1).and.(itera.eq.0).and.(icc.eq.1)) then
       do 217  i=1,m1
        ! vérification de la compatibilité entre variable xi et restriction 4 (dmin)
        if((lcont4(ij,nel,i,1).eq.4).and.(nxi(1).ne.1)) then
          write(*  ,220)
          write(*  ,220)
          write(*  ,220)
          write(*  ,220)

          write(6970  ,220)
          write(6970  ,220)
          write(6970  ,220)
          write(6970  ,220)

          write(666,220)
          write(66 ,220)
		write(29 ,220)		!sept2006
        endif
        ! vérification du choix des points de calcul
        if(lcont4(ij,nel,i,2).gt.ipts(nel)) then
          yp=lcont4(ij,nel,i,2)
          write(*  ,*)'iy =',yp,' > ipt=',ipts(nel),' est incorrecte'
          write(6970  ,*)'iy =',yp,' > ipt=',ipts(nel),' est incorrecte'
          write(666,*)'iy =',yp,' > ipt=',ipts(nel),' est incorrecte'
          write(29 ,*)'iy =',yp,' > ipt=',ipts(nel),' est incorrecte'		!bug	!sept2006
	    pause 'stop'															!sept2006
		iff=1
        endif

        ! vérification du choix des points de calcul des sensibilités vis à vis de traverses
        if(mt.ge.1) then
          if((lcont4(ij,nel,i,1).ge.40).and.
     *       (lcont4(ij,nel,i,1).le.45))     then
             do it=1,mt
               ax=dabs( abtr(it)/ypts(lcont4(ij,nel,i,2),nel) )
               if(ax.ge.(0.99)) then         !  ipt3= nø de la trav. concernee
                 if(ax.le.(1.01)) ipts3(lcont4(ij,nel,i,2),nel)=it
               endif
             enddo
          endif
        endif

 217   continue
      endif  !        if((iopti.ge.1).and.(itera.eq.1)) then

 216  continue   ! boucle sur les cas de charges

 219       continue  ! vient de goto (2)


c     save list of constraints in subr. ent to be re-used in subr. contr
      if((iopti.ge.1).and.(itera.eq.0).and.(nel.eq.neto)) then
        do  is=1,nsol
          ij=nnsol(is)
          do  ip=1,neto
            do  i=1,m1tabl(ip,is)
               write(78) lcont4(ij,ip,i,1),lcont4(ij,ip,i,2),
     *                   inv3(ij,ip,i),cjmax9(ij,ip,i)
            enddo
          enddo
        enddo
      endif
      do i=1,nsolm
      do j=1,neto
      do k=1,m1max
       cjmax3(i,j,k)=cjmax9(i,j,k)
      enddo
      enddo
      enddo


      if(iopti.ge.1) then
        ia=m1tot+m2tot
        if(ia.gt.mmmax) then
          write(*,*)'le nombre max de restrictions est dépassé !!'
          write(*,*)'too much much constraints are considered  !!'
          write(*,*) m1tot,' restr. struct. / struct. constraints'
          write(*,*) m2tot,' restr. géom. /geometrical constraints'
          write(*,*) 'change your input data (données)'

          write(6970,*)'le nombre max de restrictions est dépassé !!'
          write(6970,*)'too much much constraints are considered  !!'
          write(6970,*) m1tot,' restr. struct. / struct. constraints'
          write(6970,*) m2tot,' restr. géom. /geometrical constraints'
          write(6970,*) 'change your input data (données)'


          write(666,*)'le nombre max de restrictions est dépassé !!'
          write(666,*)'too much much constraints are considered  !!'
          write(666,*) m1tot,' restr. struct. / struct. constraints'
          write(666,*) m2tot,' restr. géom. /geometrical constraints'
          write(666,*) 'change your input data (données)'
          write(29 ,*)'le nombre max de restrictions est dépassé !!'			!bug	!sept2006
          write(29 ,*)'too much much constraints are considered  !!'			!bug	!sept2006
          write(29 ,*) m1tot,' restr. struct. / struct. constraints'			!bug	!sept2006
          write(29 ,*) m2tot,' restr. géom. /geometrical constraints'			!bug	!sept2006
          write(29 ,*) 'change your input data (données)'						!bug	!sept2006
          pause 'error'																!sept2006
		iff=1
        endif
      else
        m1cont(nel)=0
      endif


c 3.4 lecture des m2 restrictions géométriques (ibm2 = nø du set de restrictions)
c     ----------------------------------------------------------------------
c          m2 =  nbre de restr. géom. du panneau nel

      read (55,*,end=900) gfh                             ! titre
      read (55,*,end=900) m2



      if(m2.eq.99) then
        read (55,*,end=900) ibm2    ! avec ibm2  = nø du set de restrictions
        read (55,*,end=900) isema(nel),isemr(nel)
c           avec isema > 0  l'épaisseur des semelles des transversaux est
c                           modifiée automatiquement
c                           isema ou isemr =1 : largeur semelle < 16 *
c                                 epaiss. semelle
c                           isema ou isemr =2 : largeur semelle < 32 *
c                                 epaiss. semelle
c           avec isemr > 0  l'épaisseur des semelles des raidisseurs  est
c                           modifiée automatiquement
        if(ibm2.eq.1) then
c           set de restrictions de hughes (pour profil t)
          m2=11
          lm2(1)= 109
          lm2(2)= 110
          lm2(3)= 111
          lm2(4)= 112
          lm2(5)= 209
          lm2(6)= 210
          lm2(7)= 211
          lm2(8)= 212
          lm2(9) =301
          lm2(10)=302
          lm2(11)=303
        else if(ibm2.eq.2) then
c           set de restrictions de hughes + set complémentaire (a)
          m2=15
          lm2(1)= 109
          lm2(2)= 110
          lm2(3)= 111
          lm2(4)= 112
          lm2(5)= 209
          lm2(6)= 210
          lm2(7)= 211
          lm2(8)= 212
          lm2(9) =301
          lm2(10)=302
          lm2(11)=303
          lm2(12)=104
          lm2(13)=105
          lm2(14)=204
          lm2(15)=205
        else if(ibm2.eq.3) then
c           set de restrictions de hughes (pour cornières)
          m2=11
          lm2(1)= 115
          lm2(2)= 116
          lm2(3)= 111
          lm2(4)= 112
          lm2(5)= 209
          lm2(6)= 215
          lm2(7)= 216
          lm2(8)= 212
          lm2(9) =301
          lm2(10)=302
          lm2(11)=303
        else if(ibm2.eq.4) then
c           set de restrictions de rahman
          m2=6
          lm2(1)=101
          lm2(2)=102
          lm2(3)=103
          lm2(4)=201
          lm2(5)=202
          lm2(6)=203
        else if(ibm2.eq.5) then
c           set de restrictions de rahman + set complémentaire (a)
          m2=10
          lm2(1)=101
          lm2(2)=102
          lm2(3)=103
          lm2(4)=201
          lm2(5)=202
          lm2(6)=203
          lm2(7)=104
          lm2(8)=105
          lm2(9)=204
          lm2(10)=205


        else if(ibm2.eq.6) then
c           set de restrictions DCNS1 (développement décmbre 2008)
          m2=6

          lm2(1)= 201  
          lm2(2)= 202  
          lm2(3)= 203  
          lm2(4)= 101  
          lm2(5)= 102  
          lm2(6)= 114

          isema(nel)=6
		isemr(nel)=6

        else if(ibm2.eq.7) then
c           set de restrictions DCNS2 (développement décmbre 2008)
          m2=6

          lm2(1)= 201  
          lm2(2)= 202  
          lm2(3)= 203  
          lm2(4)= 101  
          lm2(5)= 102  
          lm2(6)= 114

          isema(nel)=7
		isemr(nel)=7

        else
          iff=1               !  message d'erreur
          write(*,*)   ' set des restr. géométriques inconnu! =',ibm2
          write(6970,*)' set des restr. géométriques inconnu! =',ibm2

          write(666,*)' set des restr. géométriques inconnu! =',ibm2
          write(29 ,*)' set des restr. géométriques inconnu! =',ibm2			!bug	!sept2006
          return
        endif ! if(ibm2.eq.1)

        if(iopti.ge.3) then
	      write(*,*)'nbre de restr. géométriques  = ',m2
	      write(6970,*)'nbre de restr. géométriques  = ',m2
	  endif




      else if((m2.lt.0).or.(m2.gt.20)) then
        iff=1                              !  message d'erreur
        write(*,*)  ' nbre de restr. géométriques incorrecte = ',m2
        write(6970,*)  ' nbre de restr. géométriques incorrecte = ',m2
        write(666,*)' nbre de restr. géométriques incorrecte = ',m2
        write(29 ,*)' nbre de restr. géométriques incorrecte = ',m2			!bug	!sept2006
	  return


      else if(m2.ne.0) then
        if(iopti.ge.3) then
	     write(*,*)'nbre de restr. géométriques  = ',m2
	     write(6970,*)'nbre de restr. géométriques  = ',m2
	  endif
        read (55,*,end=900) (lm2(i),i=1,m2)  !  liste des m2 restrictions
        read (55,*,end=900) isema(nel),isemr(nel)      !  indice relatif aux semelles
        if(iopti.ge.3) then
	     write(*,*)' set  = ',(lm2(i),i=1,m2)
	     write(6970,*)' set  = ',(lm2(i),i=1,m2)
	  endif

!        do i=1,m2
!	      if     (lm2(i).eq.150) then
!               m2=m2-1
!	         isema=6
!	      elseif (lm2(i).eq.151) then
!               m2=m2-1
!	         isema=7
!	      elseif (lm2(i).eq.152) then
!               m2=m2-1
!	         isema=8
!	      elseif (lm2(i).eq.153) then
!               m2=m2-1
!	         isema=9
!	      elseif (lm2(i).eq.250) then
!               m2=m2-1
!	         isemr=6
!	      elseif (lm2(i).eq.251) then
!               m2=m2-1
!	         isemr=7
!	      elseif (lm2(i).eq.252) then
!               m2=m2-1
!	         isemr=8
!	      elseif (lm2(i).eq.253) then
!               m2=m2-1
!	         isemr=9
!            endif
!
!         enddo

      else if(m2.eq.0) then
        isema(nel)=0
        isemr(nel)=0                              !
      endif                                  ! if(m2.eq.99)













      m2cont(nel)=m2                         ! lulu

      if(iopti.ge.1) then
        if(m2.gt.0) then                      ! si m2>0
          if(itera.eq.0) then                 ! si itera=1
            if(isema(nel).ge.1) then
              write(666,203) 'transversaux : isema= ',isema(nel)
            else
              write(666,207) 'transversaux'
            endif
            if(isemr(nel).ge.1) then
              write(666,203) 'raidisseurs  : isemr= ',isemr(nel)
            else
              write(666,207) 'raidisseurs'
            endif
            if((isemr(nel).ge.1).or.(isema(nel).ge.1)) then
              write(666,204)
            endif                           !
          else                              ! si itera>1
            if((isema(nel).ge.1).or.(isemr(nel).ge.1)) then
              if(nel.eq.1) write(666,204)
            endif
          endif
        endif

        if(isema(nel).ge.1) then
           do 205 i=1,nbrxi

	        if(isema(nel).gt.5) then 
                 if ((itype(nel).ne.5).and.(nxi(i).eq.3))then !on vérifie si l'on
	              nn=i+ntot                                 ! a bien sélectionné 
                                                              !comme variable de 
                                                              !conception dya
         
                    if      (isema(nel).eq.6) then 
                               call dcns_geom_n50_51(hya,dya,wya,              
     *                                        tya_dcns(nel),
     *                                        ximin(nn),ximax(nn),
     *                                        inda,iprint,nel,
     *                                        ityp1,isect1,impr2)
                               xicou(nn)=dya

	              endif


                 elseif ((itype(nel).ne.5).and.(nxi(i).eq.4))then !on vérifie si l'on
	              nn=i+ntot                                 ! a bien sélectionné 
                                                              !comme variable de 
                                                              !conception wya



                    if (isema(nel).eq.7) then
                               call dcns_geom_n52_53(hya,dya,wya,
     *						                tya_dcns(nel),              
     *                                        ximin(nn),ximax(nn),
     *                                        inda,iprint,nel,
     *                                        ityp1,isect1,impr2)
                                xicou(nn)=wya


	              endif



                 endif

              else 


                 if ((itype(nel).ne.5).and.(nxi(i).eq.4))then  !on vérifie si l'on
	              nn=i+ntot                                  ! a bien sélectionné 
                                                               !comme variable de 
                                                               !conception wya

                    call semel(hya,dya,wya,
     *                      tya_dcns(nel),ximin(nn),ximax(nn),    ! cadres	       
     *                      inda,1,iprint,nel,isema(nel),itype(nel),
     *			          isect(nel),impr2)    	
                    xicou(nn)=wya



                 elseif ((isect(nel).eq.3).and.(nxi(i).eq.3)) then
                    nn=i+ntot

                    call semel(hya,dya,wya,
     *                   tya_dcns(nel),ximin(nn),ximax(nn),    ! épontille
     *                   inda,3,iprint,nel,isema(nel),itype(nel)
     *				  ,isect(nel),impr2)    
                    xicou(nn)=wya

                 endif
              endif
  205      continue
        endif






        if(isemr(nel).ge.1) then
           do 206 i=1,nbrxi

	        if(isemr(nel).gt.3) then 
                 if ((itype(nel).ne.5).and.(nxi(i).eq.7))then !on vérifie si l'on
	              nn=i+ntot                                 ! a bien sélectionné 
                                                              !comme variable de 
                                                              !conception dxr
         
                    if      (isemr(nel).eq.6) then
                               call dcns_geom_n50_51(hxr,dxr,wxr,
     *                                        txr_dcns(nel),              
     *                                        ximin(nn),ximax(nn),
     *                                        indr,iprint,nel,
     *                                        ityp1,isect1,impr2)
                                 xicou(nn)=dxr

	              endif


                 elseif ((itype(nel).ne.5).and.(nxi(i).eq.8))then !on vérifie si l'on
	              nn=i+ntot                                 ! a bien sélectionné 
                                                              !comme variable de 
                                                              !conception wxr



                    if (isemr(nel).eq.7) then
                               call dcns_geom_n52_53(hxr,dxr,wxr,
     *                                        txr_dcns(nel),              
     *                                        ximin(nn),ximax(nn),
     *                                        indr,iprint,nel,
     *                                        ityp1,isect1,impr2)
                                xicou(nn)=wxr

	              endif

                 endif

              else 


                 if(nxi(i).eq.8) then  !on vérifie si l'on
	              nn=i+ntot          ! a bien sélectionné 
                                       !comme variable de 
                                       !conception wxr

                    call semel(hxr,dxr,wxr,
     *                   txr_dcns(nel),ximin(nn),ximax(nn),   
     *                   indr,2,iprint,nel,isema(nel),itype(nel),
     *				   isect(nel),impr2)        ! raidisseurs		
                    xicou(nn)=wxr


                 endif
              endif
  206      continue
        endif






        if(itera.eq.0) write(666,208) m2,(lm2(i),i=1,m2)

        do 202 ic=1,m2
          it=ic+m2tot
          call geom(epsa,epsr,delta,hya,dya,wya,tya,hxr,dxr,wxr,txr,
     *              ic,it,nxi,nbrxi,itera,iprint,lm2(ic),m2max,ntot,
     *			  impr2,code,coefk(nel),length,isect(nel),itype(nel),
     *              ximin,ximax,xicou,neto)						
  202   continue
        tfa(nel)=tya
        tfr(nel)=txr
      endif

      ntot =ntot +nbrxi
      m2tot=m2tot+m2

c ======================================================================
c  4.  calcul des coefficients de raideur du panneau
c     ----------------------------------------------
c     entreposes dans le vecteur const,const2 et const3
c ======================================================================

      call annuld(const,74)

c 4.1 raidissage principale
c     ----------------------
      call carac(e(nel),eta(nel),epsa,epsr,delta,hya,dya,wya,tya,hxr,
     *           dxr,wxr,txr,
     *           hxtr,dxtr,wxtr,txtr,const,sh,fam,mt,ksr,ksa,kst,
     *           spec(nel),poids9,phil,q,
     *           width,delt(nel),delt2(nel),impr2,epais,itype(nel),
     *           isect(nel),langue,
     *           aire,aiy,aix,sredy,sredx,tork)
	if(itype(nel).eq.5) then
	  write(306)aire,aiy,aix,sredy,sredx,tork							!extension neto	!fev2007	!12.05.04
	endif				

c 4.2 derivee de const (opti)
c     ----------------------
      if (iopti.ge.1) then
        call annuld(const2,54)
        call annuld(const3,20)
        call caracd(e(nel),eta(nel),epsa,epsr,delta,hya,dya,wya,tya,
     *              hxr,dxr,wxr,
     *              txr,const,sh,fam,mt,ksr,ksa,kst,phil,q,width,
     *              const2,const3,
     *              epais,itype(nel),isect(nel),dpoids,spec(nel))                                                                                           !février 2004
      endif

c 4.3 raidissage secondaire
c     ----------------------

      if((epsa2.ge.0.00001).or.(epsr2.ge.0.00001)) then
        call carac2(e(nel),eta(nel),epsa2,epsr2,delta,hya2,dya2,wya2,
     *              tya2,hxr2,
     *              dxr2,wxr2,txr2,const,sh,fam,ksr2,ksa2,spec(nel),
     *              poids9,phil,q,width,
     *              delt(nel),delt2(nel),impr2,const2)
      endif

c 4.4 impression de const2
c     ----------------------
      if((impr.ge.1).and.(iopti.ge.1)) then
	  if(itype(nel).ne.5) then									!février 2004
        write(66,15)
        write(66,*)' bordage : x(1) = epaisseur bordage'
        write(66,12)    (const2(i,1),i=1,6)
        write(66,*)' cadres  : x(i),i=2,5'
        write(66,*)'    x(2) = hauteur âme'
        write(66,12)    (const2(i,2),i=1,6)
        write(66,*)'    x(3) = epaisseur âme'
        write(66,12)    (const2(i,3),i=1,6)
        write(66,*)'    x(4) = largeur semelle'
        write(66,12)    (const2(i,4),i=1,6)
        write(66,*)'    x(5) = larg. effective'
        write(66,12)    (const2(i,5),i=1,6)
        write(66,*)' raidisseurs: x(i),i=6,9'
        write(66,*)'    x(6) = hauteur âme'
        write(66,12)    (const2(i,6),i=1,6)
        write(66,*)'    x(7) = epaisseur âme'
        write(66,12)    (const2(i,7),i=1,6)
        write(66,*)'    x(8) = largeur semelle'
        write(66,12)    (const2(i,8),i=1,6)
        write(66,*)'    x(9) = larg. effective'
        write(66,12)    (const2(i,9),i=1,6)
        write(66,*)' '
	  else										!épontille	!février 2004
	  if(isect(nel).eq.3) then										!février 2004
        write(66,15)												!février 2004
        write(66,*)' epontille :x(1) = hauteur de l''âme '		!février 2004
        write(66,12)    (const2(i,1),i=1,5)						!février 2004
        write(66,*)'    x(2) = epaisseur âme'						!février 2004
        write(66,12)    (const2(i,2),i=1,5)						!février 2004
        write(66,*)'    x(3) = largeur semelle'					!février 2004
        write(66,12)    (const2(i,3),i=1,5)						!février 2004
        write(66,*)'    x(5) = larg. effective'					!février 2004
        write(66,12)    (const2(i,5),i=1,5)						!février 2004
	  else													!février 2004
        write(66,15)												!février 2004
        write(66,*)' epontille :x(1) = diamètre ou côté extérieur '	!février 2004	
        write(66,12)    (const2(i,1),i=1,5)							!février 2004
        write(66,*)'    x(2) = epaisseur de la paroi mince'			!février 2004
        write(66,12)    (const2(i,4),i=1,5)							!février 2004
        write(66,*)'    x(5) = larg. effective'						!février 2004
        write(66,12)    (const2(i,5),i=1,5)							!février 2004
        endif														!février 2004
 	  endif														!février 2004
        if(mt.ge.1) then
          write(66,*)' traverses:  x(1) = epaisseur bordage'
          do 20 j=1,mt
  20      write(66,*)'trav nø',j,'  : ',(const3(i,j),i=1,2)
        endif
      endif


c 4.5 poids du panneau et de la structure
c     ---------------------------------
      pot=pot+poids9                 ! pot= poids total struct par mètre

c 4.6 action du poids propre (poids=poids panneau par mètre)
c     ----------------------------------------------------

      if(ipoids.eq.0)goto 533
      write(66,534) poids9
      if(ipa.eq.1) then                                     !!!aout04
	  poids9=poids9*(spec(nel)-9810.)/spec(nel)           !!!aout04
	  do k=1,9                                            !!!aout04
	    dpoids(k)=dpoids(k)*(spec(nel)-9810.)/spec(nel)   !!!aout04
	  enddo                                               !!!aout04
	endif                                                 !!!aout04
        do 535 is=1,nsol
          if(icha(nel).eq.1) then
            do 532 i=1,npt(is)
            cha(i,1,is)=cha(i,1,is)+poids9              !!!aout04
		  do k=1,9                                   !!!aout04
		    dcha(i,is,k)=dcha(i,is,k)+dpoids(k)		 !!!aout04
		  enddo                                      !!!aout04
 532	      continue                                   !!!aout04
          else
            npt(is)=1
            cha(1,1,is)=poids9
            cha(1,2,is)=0.
            cha(1,3,is)=0.
	      do k=1,9                                   !!!aout04
	        dcha(1,is,k)=dpoids(k)					 !!!aout04
	      enddo
          endif
 535    continue
 533  continue

c     sauvetage [ write(98) ] de cha1, cha2 et cha3
c     --------------------------------------------
      if((ipoids.eq.1).or.(icha(nel).eq.1)) then
        do 689 is=1,nsol
        do 689 i=1,npt(is)
          cha(i,1,is)=cha(i,1,is)/qphil
	    do k=1,9                                !!!aout04
	      dcha(i,is,k)=dcha(i,is,k)/qphil       !!!aout04
	    enddo
 689    continue                             !
        if(itera.eq.0) write(98)
     *        (npt(i),i=1,nsol),
     *        (((cha(i,j,k),i=1,100),j=1,3),k=1,nsol),
     *        (((dcha(i,j,k),i=1,100),j=1,nsol),k=1,9)
      endif

c 4.7 sauvetage, des pressions max pour la subr  hughes ! updated: mai 1996
c                                    et la subr. stiff
c     -------------------------------------------------
c    la pression max est calculée en y= 0, 1/3, 1/2, 2/3 et 1
      ip=1
      if(kse.eq.2) ip=-1
      if(kse.eq.0) ip=0
      call annuld(chamax,5*nsol)
	call annuld(dchamax,5*nsol*9)    !!!aout04
      do 690 is=1,nsol

        c11=dcos((ang)          *pi1)/9810.       ! pi1=pi/180.
        c22=dcos((ang+phil/3.)  *pi1)/9810.
        c33=dcos((ang+phil/2.)  *pi1)/9810.
        c4=dcos((ang+2.*phil/3.)*pi1)/9810.
        c5=dcos((ang+phil)      *pi1)/9810.

        xif= xf(is)-xi(is) + cha(1,3,is)-cha(1,2,is)
        chamax(1,is)=(xi(is)+cha(1,2,is))          *ip+c11*cha(1,1,is)
        chamax(2,is)=(xi(is)+cha(1,2,is)+xif/3.)   *ip+c22*cha(1,1,is)
        chamax(3,is)=(xi(is)+cha(1,2,is)+xif/2.)   *ip+c33*cha(1,1,is)
        chamax(4,is)=(xi(is)+cha(1,2,is)+2.*xif/3.)*ip+c4*cha(1,1,is)
        chamax(5,is)=(xf(is)+cha(1,3,is))          *ip+c5*cha(1,1,is)
c         calcul des dérivées des chamax    !!!aout04	  
	  do k=1,9
	    dchamax(1,is,k)=c11*dcha(1,is,k)
		dchamax(2,is,k)=c22*dcha(1,is,k)
		dchamax(3,is,k)=c33*dcha(1,is,k)
      	dchamax(4,is,k)=c4*dcha(1,is,k)
	    dchamax(5,is,k)=c5*dcha(1,is,k)
	  enddo

        if(npt(is).ge.2) then
           do i=2,npt(is)
              xif= xf(is)-xi(is) + cha(i,3,is)-cha(i,2,is)
              t1=(xi(is)+cha(i,2,is))          *ip+c11*cha(i,1,is)
              t2=(xi(is)+cha(i,2,is)+xif/3.)   *ip+c22*cha(i,1,is)
              t3=(xi(is)+cha(i,2,is)+xif/2.)   *ip+c33*cha(i,1,is)
              t4=(xi(is)+cha(i,2,is)+2.*xif/3.)*ip+c4*cha(i,1,is)
              t5=(xf(is)+cha(i,3,is))          *ip+c5*cha(i,1,is)

c			calcul des dérivées des t1	    !!!aout04	  
	   		 do k=1,9
				dt1(k)=c11*dcha(i,is,k)
				dt2(k)=c22*dcha(i,is,k)
				dt3(k)=c33*dcha(i,is,k)
				dt4(k)=c4*dcha(i,is,k)
				dt5(k)=c5*dcha(i,is,k)
			enddo

c             recherche de la valeur max avec son signe
	        call select (chamax(1,is),t1,dchamax(1,is,1),dt1)	!!!aout04  !à revérifier
	        call select (chamax(2,is),t2,dchamax(2,is,1),dt2)	!!!aout04
	        call select (chamax(3,is),t3,dchamax(3,is,1),dt3)	!!!aout04
	        call select (chamax(4,is),t4,dchamax(4,is,1),dt4)	!!!aout04
	        call select (chamax(5,is),t5,dchamax(5,is,1),dt5)	!!!aout04
           enddo
        endif

        if((ivaria.eq.0).and.(types.eq.'COQUE')) then
           write(*  ,691)
           write(666,691)
		 write(29 ,691)	
        endif

 690  continue

	if(impr2.ge.-1) then						!15.10.05 
      write(66,874)mt,ksr,ksa,kst,kse,ipa,ivaria	!15.10.05
	endif

c 4.8 calcul du coût  (voir subr cost ci-dessous: mai 1998)
c     ----------------

      call cost(nel,nxi,nbrxi,width,
     *          epsa,epsr,delta,hya,dya,wya,tya,hxr,dxr,wxr,txr,phil,q,
     *          epais,itype(nel),isect(nel))

c
c***********************************************************************
c 5.  sauvetage des donnees
c     ---------------------
c      write(66,874)mt,ksr,ksa,kst,kse,ipa,ivaria


c     sauvetage pour subr. bo1 et bo2
c      --------------------------------------
      write(99)  abtr,phil,mt,teta,xi,xf,hxtr,wxtr,q,kse,kst,ksa,
     *           txtr,dxtr,ksr,aaa,bbb,ccc,ddd,delto,ivaria,const,
     *           const2,const3,
     *           delta,dya,tya,wya,hya,epsa,hxr,dxr,wxr,txr,epsr,entr,
     *           chamax,dchamax,nsign(nel),epais,hight,heff                                   !février 2004
c     sauvetage pour subr. pepite (opti)                                                      !redressage !septembre 2007
c      --------------------------------------                                                 !redressage !septembre 2007
	if(itype(nel).ne.5) then			!redressage
	  write(93) nel,delta,epsa,epsr,fami,lot,poid1,sigy(nel),
     *            spec(nel),hight
	  write(7777) fami,lot,sigy(nel),spec(nel),hight					
	
	endif				!redressage			                                                                    !redressage !septembre 2007


c     sauvetage pour subr. objec1, objec2 (test sur sub. objectif alstom), mars 2003
c      ---------------------------------------------------------
      if(nel.eq.1) then
        rewind(305)			!extension neto	!fev2007
        write(305,*)width		!extension neto	!fev2007
      endif
      write(305,*)nel,dabs(phil*q*pi1),delta				!extension neto	!fev2007
      write(305,'(5e14.7)')hya,dya,wya,tya,epsa,epais		!extension neto	!fev2007
      write(305,'(6e14.7)')hxr,dxr,wxr,txr,epsr,entr		!extension neto	!fev2007

c     sauvetage pour subr. objec1, objec2  et coord (gravite)
c      ---------------------------------------------------------
      if((iopti.ge.1).or.(iana.eq.2)) then									!r&d14	!fev2007
        if(itype(nel).ne.5) then
	    if(epsa2.ge.(0.00001)) then										
		  write(302) epsa,epsr,delta,hya,dya,wya,tya,			!r&d14	!extension neto	!fev2007
     *			     hxr,dxr,wxr,txr,phil,q,epais,epsa2,
     *				 hya2,dya2,wya2,tya2
	    else
		  write(302) epsa,epsr,delta,hya,dya,wya,tya,			!extension neto	!r&d14	!fev2007
     *			     hxr,dxr,wxr,txr,phil,q,epais,epsa2
          endif														    
	  else
	    write(302) epsa,epsr,delta,hya,dya,wya,tya,				!extension neto	!r&d14	!fev2007
     *			   hxr,dxr,wxr,txr,phil,q,epais						
	  endif
      endif


c     sauvetage pour le dessin (pour subr. vision)
c     --------------------------------------------
      if(dessin.ne.1) return

    ! ityp= 1 si coque ou coque1 et =2 si plaque ou coque1
      ityp1=itype(nel)  ! ok si coque (itype=1) ou si plaque (itype=2)
      if(itype(nel).eq.3) ityp=1  ! si coque1
      if((itype(nel).eq.4).or.(itype(nel).eq.5)) ityp=2  !si plaque1 ou epontille       !février 2004

      write(42) ityp,philn(nel),q,hight,angle(nel),ivaria,xi,
     *          xf,kse,teta,poids9,npt

      do  ns=1,nsol
       do  i=1,npt(ns)
         write(42) cha(i,1,ns),cha(i,2,ns),cha(i,3,ns)
       enddo
      enddo
      write(45) nel,mt,kst
      if(mt.ne.0) write(45) hxtr,wxtr,abtr2

c     sauvetage des données courantes pour subr. visionn (dessin)
      write(46) delta,hya,dya,wya,tya,epsa,hxr,dxr,wxr,txr,epsr

      return
c -------------------------------------------------------------------------------
c -------------------------------------------------------------------------------
  900 write(* ,*)'erreur de lecture : "end of file"'
      write(66,*)'erreur de lecture : "end of file"'
	write(29,*)'erreur de lecture : "end of file"'
      pause 'stop'
	stop 55

c 6.  les formats.
c     ============
   1  format(/' charge en metre de la colonne d''eau sur le panneau'
     */1x,49(1h+))
   5  format(' cas de charge nø ',i2/
     * '  variation lineaire de la p.h.avec la profondeur (hydrostat)'/
     * '  sur le bord de depart (y=0), xi=',f7.3,' m.'/
     * '  (nb: sur le bord d''arrivee,  xf=',f7.3,' m )'/)
   6  format(' efforts de bords dans le plan du panneau selon l''axe x'/
     *      ,' a repartir sur une distance de ',f7.3,'  (m)'/)
   7  format(' cas nø',i2,' variation lineaire de la p.h. (xi a xf) :'/
     * '    au bord de depart  (y=0)   , xi=',f7.3,' m.'/
     * '    au bord d''arrivee (y=phil) , xf=',f7.3,' m.'/
     * '     nb: 1m = 1 m d''eau (colonne) = 9.81kn/m2')
   8  format('    angle que fait le panneau en y=0 avec l''horizontale',
     *       ' = ',f9.3,' degré')
  11  format(/' dimensions des cadres (transversaux)'/1x,36(1h+)/
     *'    distance entre cadres    = ',f8.5,' m.'//
     *'    hauteur de l''ame         = ',f8.5,' m.',
     *     ' (épaisseurs du bordé et semelle non comprises)'/
     *'    epaisseur de l''ame       = ',f8.5,' m.'/
     *'    largeur de la semelle    = ',f8.5,' m.'/
     *'    epaisseur de la semelle  = ',f8.5,' m.')
  12  format(6(1x,e11.4))
  13  format(/' dimensions des raidisseurs longitudinaux'/1x,40(1h+)/
     *'    distance entre raid. (=d) = ',f8.5,' m.'//
     *'    hauteur de l''ame          = ',f8.5,' m.',
     *     ' (épaisseurs du bordé et semelle non comprises)'/
     *'    epaisseur de l''ame        = ',f8.5,' m.'/
     *'    largeur de la semelle     = ',f8.5,' m.'/
     *'    epaisseur de la semelle   = ',f8.5,' m.'//
     *'    epsr (larg. collaborante) = ',f8.5,' m.'/
     *'    mode (répartition des cadres): ',a3,' n=l/d + ',f4.1)
  14  format(/'nervures complémentaires (2ème lit; aig et raid)'/45(1h=)
     */' les cadres'/2x,12(1h-)/
     * '   distance entre cadres compl.= ',f8.5,' m.  et  ksa2= ',i2/
     * '   hauteur de l''ame        ',f8.5,' m.'/
     * '   epaisseur de l''ame      ',f8.5,' m.'/
     * '   largeur de la semelle   ',f8.5,' m.'/
     * '   epaisseur de la semelle ',f8.5,' m.'/
     * ' les raidisseurs'/2x,12(1h-)/
     * '   distance entre raidisseurs compl.= ',f8.5,
     *                                       ' m. et  ksr2= ',i2/
     * '   hauteur de l''ame        ',f8.5,' m.'/
     * '   epaisseur de l''ame      ',f8.5,' m.'/
     * '   largeur de la semelle   ',f8.5,' m.'/
     * '   epaisseur de la semelle ',f8.5,' m.'/)
  15  format('derivées des coef. de raideur par rapport aux var. de ',
     * 'conception :'/65(1h-))
  39  format(/' caracteristiques des traverses'/1x,30(1h+)/
     * t20,'distance',t32,'epaisseur de',t45,'largeur de',t57,
     * 'epaisseur',t67,'hauteur'/t20,'par rapport',t32,'la semelle',
     *t45,'la semelle',t57,'de l ame',t67,'de l ame'/t20,'au sommet '/
     *t23,'(m)',t36,'(m)',t50,'(m)',t62,'(m)',t72,'(m)')
 203  format('avec ajustement des dimensions des semelles des ',a,i2)
 204  format(/'ajustement des dimensions des semelles'/38(1h-)/
     * 'panneau',5x,'anc. dimensions',15x,'nouvelles dimensions',6x,
     * 'type','  ind')
 207  format('les dimensions des semelles des ',a12,' sont fixes.')
 208  format(/'nbre de restr. géom.= ',i2,'  set = ',20i4)
 212  format(/'cas nø',i2,': charges variables selon ox,',
     *                    ' definies en ',i2,' intervalles'/
     * t11,'regulierement espaces (charge en kn par metre courant)')
 213  format(' intervalle nø     charge verticale (n/m)     pression'
     * ' (m)  en y=0  et en y=phil')
 214  format(t4,i2,t20,e14.7,' n/m',t45,e14.7,' m. ',e14.7,' m.')
 218  format(/'panneau nø',i2,': kse=0 est incompatible avec icha=1'/
     * '!!! nous prenons kse=2 avec xi=xf=0  et  cha2=cha3=0 !!!!!!!'/)
 220  format(/' attention'/1x,14(1h*)/' vous avez sélectionné la ',
     * 'restrictions nø 4 (d<dmin) dite de hughes'/' alors que d n''est'
     * ' pas une variable de conception.'/' cela peut entraîner des'
     * ' difficultés dans conlin !!'/)
 237  format(/'ajustement des dimensions des semelles'/38(1h-)/
     * 'panneau',5x,'anc. dimensions',20x,'nouvelles dimensions',8x,
     * 'type','    ind')
 283  format(/'panneau nø',i2/15(1h-)/
     *       'nbre de variables de conception = ',i3)
 427  format('c(',i2,') > ',e11.5,' de type nø',i2,
     *  ' au pt ypt nø',i2,' et au pt de calcul nø',i2)
 428  format('c(',i2,') < ',e11.5,' de type nø',i2,
     *  ' au pt ypt nø',i2,' et au pt de calcul nø',i2)
 431  format(/' attention: la position des pts de calculs',
     *        ' des sensibilites est errone.'/5x,
     *        ' il faut que le premier et le dernier pts',
     *        ' correspondent aux extremites.'/5x,
     *        ' !!!  la correction est automatique !!!'/)
 441  format(' traverse n ',i2,6x,5(f8.5,4x))
 534  format(/'le poids propre par metre (selon ox) est de ',
     *         e14.7,' n/m'/)
 649  format(//' le rapport rayon/epaisseur est de ',e11.4,' m.'/)
 691  format(/' approximation (coque avec ivaria=0) !!!!!'/,
     *  ' les pressions pour la subr hughes sont calculées en ',
     *  'considérant une variation linéaire selon oy.'/
     *  ' l''erreur peut être importante si phil > 60 degré'/)
 801  format(/' dimensions du panneau'/1x,22(1h+)/
     *        '    epaisseur de bordage = ',f9.6,' m.'/
     *        '    longueur du panneau  = ',f9.3,' m.')
 802  format( '    angle d''ouverture    = ',f11.5,' degré'/
     *        '    rayon du panneau     = ',f10.3,' m.')
 803  format( '    largeur de la plaque = ',f9.5,' m.')
 804  format( '	la section est de type	= ',a7/					!février 2004
     *		'	l''entredistance des epontilles =',f9.5,'m.')	!février 2004
 805  format(	'	section''s type =',a7/							!février 2004
     *		'	pillar''s spacing =',f9.5,'m.')					!février 2004
 806  format(	'	diametre exterieur	=',f9.5,' m.'/				!février 2004
     *		'	epaisseur de la paroi mince =',f9.5,'m.')		!février 2004
 807  format(	'	longueur exterieure du cote	=',f9.5,' m.'/		!février 2004
     *		'	epaisseur de la paroi mince =',f9.5,'m.')		!février 2004
 808  format(	'	hauteur de la demi ame	=',f9.5,' m.'/			!février 2004
     *		'	epaisseur de ame =',f9.5,' m.'/					!février 2004
     *		'	largeur de la semelle	=',f9.5,' m.'/			!février 2004
     *		'	epaisseur de la semelle	=',f9.5,' m.')			!février 2004
 809  format(	'	exterior diameter 	=',f9.5,' m.'/				!février 2004
     *		'	cercle thickness =',f9.5,'m.')					!février 2004
 810  format(	'	exterior length of side	=',f9.5,' m.'/			!février 2004
     *		'	square thickness =',f9.5,'m.')					!février 2004
 811  format(	'	half web hight	=',f9.5,' m.'/					!février 2004
     *		'	web thickness =',f9.5,' m.'/					!février 2004
     *		'	flange width =',f9.5,' m.'/						!février 2004
     *		'	flange thickness =',f9.5,' m.')					!février 2004
 874  format(' mt',i2,' ; ksr',i2,' ; ksa',i2,' ; kst',i2,' ; kse',i2,
     *       ' ; ipa',i2,' ; ivaria',i2//)
 901  format(/' dimensions of the panel'/1x,22(1h+)/
     *        '    plate thickness     = ',f9.6,' m.'/
     *        '    length of the panel = ',f9.3,' m (=structure length')
 902  format( '    shell opening angle = ',f11.5,' degré'/
     *        '    shell radius        = ',f10.3,' m.')
 903  format( '    width of the panel  = ',f9.5,' m.')
 904  format(/' dimensions of the frames'/1x,30(1h+)/
     *'    frame spacing             = ',f8.5,' m.'//
     *'    web height                = ',f8.5,' m. ',
     *       '(flange and plate thickness not included)'/
     *'    web thickness             = ',f8.5,' m.'/
     *'    flange width              = ',f8.5,' m.'/
     *'    flange thickness          = ',f8.5,' m.')
 905  format(/' dimensions of longitudinal stiffeners'/1x,40(1h+)/
     *'    geometric spacing (=d)    = ',f8.5,' m. (as given)'//
     *'    web height                = ',f8.5,' m. ',
     *       '(flange and plate thickness not included)'/
     *'    web thickness             = ',f8.5,' m.'/
     *'    flange width              = ',f8.5,' m.'/
     *'    flange thickness          = ',f8.5,' m.'/
     *'    effective spacing (epsr)  = ',f8.5,' m. (calculated)'/
     *'    mode (frames distribution): ',a3,' n=l/d + ',f4.1)
 906  format(/'complementary members (2nd layer; frame & stiff)'/45(1h=)
     */' frames'/2x,12(1h-)/
     * '   spacing                 ',f8.5,' m.  and  ksa2= ',i2/
     * '   web height              ',f8.5,' m.'/
     * '   web thickness           ',f8.5,' m.'/
     * '   flange width            ',f8.5,' m.'/
     * '   flange thickness        ',f8.5,' m.'/
     * ' stiffeners'/2x,12(1h-)/
     * '   spacing                 ',f8.5,' m.  and  ksr2= ',i2/
     * '   web height              ',f8.5,' m.'/
     * '   web thickness           ',f8.5,' m.'/
     * '   flange width            ',f8.5,' m.'/
     * '   flange thickness        ',f8.5,' m.'/)
 907  format(' load case nø',i2,' linear variation of pressure between'/
     * '    departure edge (y=0)   , xi=',f7.3,' m.'/
     * '    arrival edge   (y=phil), xf=',f7.3,' m.'/
     * '     nb: 1 m means a pressure of 9.81kn/m2 = 1 meter of water')
 908  format(' load case nø',i2,' acting (phi=0):',f7.3,' m.'/
     * ' variation lineaire de la p.h.avec la profondeur'/
     * '  (nb: sur le bord inf., xf=',f7.3,' m )'/)
 909  format(/' longitudinal girder dimensions & positions'/1x,30(1h+)/
     * t20,'positions',t32,'flange',t45,'flange',t57,
     * ' web',t67,' web'/t20,'related to',t32,'thickness',
     * t45,'width',t57,'thickness',t67,'height'/t20,'depart edge(y=0)'/
     * t23,'(m)',t36,'(m)',t50,'(m)',t62,'(m)',t72,'(m)')

      end


c***********************************************************************
c***********************************************************************

      subroutine  borne(xi,i,text,nel,xm,xp,iff)
      use sharedvar
      implicit real*8 (a-h,o-z)
      character*29 text
      dimension xm(nvmax),xp(nvmax)  ! à la place du common opti4

c***********************************************************************
c     subroutine borne
c     **************
c     verification de la solution de depart
c       il faut que les xi de départ soient inclus entre les bornes
c      c.à.d.   xi(min) < xi  < xi(max)
c
c     modif:10-5-95                                  création :7-5-95
c***********************************************************************
      if(impr2.ge.-2) then						!sept2006
	  write(666,285) i,text,xm(i),xi,xp(i)
	endif										!sept2006

      if((xi.lt.xm(i)).or.(xp(i).lt.xi)) then
        write(*,*) 'bornes non compatibles avec les valeurs initiales',
     *              ' des variables de conception.'
        write(*,*) 'xi(min) <  xi < xi(max) pas vérifié'
        write(*,*) 'panneau nø',nel,'   variable nø',i
        write(*,*)  xm(i),' ?<? ',xi,' ?<? ',xp(i),' pas vérifié'

        write(6970,*)
     * 'bornes non compatibles avec les valeurs initiales',
     *              ' des variables de conception.'
        write(6970,*) 'xi(min) <  xi < xi(max) pas vérifié'
        write(6970,*) 'panneau nø',nel,'   variable nø',i
        write(6970,*)  xm(i),' ?<? ',xi,' ?<? ',xp(i),' pas vérifié'

        
	  write(29,*) 'bornes non compatibles avec les valeurs initiales',			!bug	!sept2006
     *              ' des variables de conception.'								!bug	!sept2006
        write(29,*) 'xi(min) <  xi < xi(max) pas vérifié'							!bug	!sept2006
        write(29,*) 'panneau nø',nel,'   variable nø',i							!bug	!sept2006
        write(29,*)  xm(i),' ?<? ',xi,' ?<? ',xp(i),' pas vérifié'				!bug	!sept2006

        pause 'error'
	  iff=1 !stop à la sortie de la subr ent
      endif
      return

 285  format('var. nø',i3,1x,a29,2x,e11.4,'<',e11.4,' < ', e11.4)
      end



c***********************************************************************
c***********************************************************************

      subroutine cost(nel,nxi,nbrxi,width,
     *           epsa,epsr,delta,hya,dya,wya,tya,hxr,dxr,wxr,txr,phil,q,
     *           epais,ityp1,isect1)                                                                             !février 2004
      use sharedvar
      implicit real*8(a-h,o-z)
      dimension nxi(nbrxi)


c ********************************************************************
c     calcul de la fonction objectif cout dans la subr. ent.
c
c     version du : 30-5-98                                   créer 30-5-98
c
c     modif: 11-01-02
c
c *********************************************************************
c
c les variables de conception
c ---------------------------
c       1       delta =  épaisseur du bordage
c       2       hya   =  hauteur de l'âme des aiguilles
c       3       dya   =  épaisseur de l'âme des aiguilles
c       4       wya   =  largeur des semelles des aiguilles
c       5       epsa  =  entredistance entre aiguilles
c       6       hxr   =  hauteur de l'âme des raidisseurs
c       7       dxr   =  épaisseur de l'âme des raidisseurs
c       8       wxr   =  largeur des semelles des raidisseurs
c       9       epsr  =  entredistance entre raidisseurs
c
c variables associées :
c ---------------------
c            tya = épaisseur semelle aiguilles
c            txr = épaisseur semelle raidisseurs
c
c  les variables de conception pour epontille
c  ------------------------------------------
c       1       hya       =       hauteur demi âme ou diam ou côté ext.
c       2       dya       =       épaisseur d'âme
c       3       wya       =       largeur de semelle
c       4       epais     =       épaisseur de paroi mince
c       5       epsa      =       entredistance entre épontilles
c
c ********************************************************************


c  -------------------------

c     fmat= coût des matériaux (acier)
c     fsou= coût du soudage (energie + consommables)
c     fmdo= coût main d'oeuvre
c     fct = fmat + fsou + fmdo  (d'un panneau nel),
c     obj = coût total          (de la structure complete)

      if(nel.eq.1) then
       cout=0.
       call annuld(poids,18)
      endif

      if(isect1.eq.3) then                            !âme double t entree=1/2 âme
      hyb=2*hya
      endif

      dcor  = corro(nel,1) !aout2006		!corrosion
	dcor2 = corro(nel,2) !aout2006		!corrosion
	dcor3 = corro(nel,3) !aout2006		!corrosion


c     dcor = epaisseur de corrosion pour bordé
c      if(nel.eq.1) then
c        rewind 57
c        read(57,'(////,a1)') abidon
c        read(57,*) ipan,dcor,dcor2,dcor3
c      else
c        read(57,*) ipan,dcor,dcor2,dcor3
c      endif

      if(ityp1.ne.5) then                                                                                    !février 2004
        deltac=delta+dcor
      else                                                                                                         !février 2004
        deltac =0                                                                                                  !février 2004
      endif                                                                                                         !février 2004
c       dcor2 ! epaisseur de corrosion pour cadres
      dyac  =       dya +dcor2
      tyac  = tya +dcor2
      epaisc = epais +dcor2                                                                             !février 2004
c       dcor3 ! epaisseur de corrosion pour lisses
      dxrc  =       dxr +dcor3
      txrc  = txr +dcor3
	if(impr2.ge.-1) then			!15.10.05
	if(ityp1.ne.5) then                                                                                    !février 2004
	  write(66,*) 
  	  write(66,*) ' avec sur-épaisseurs de corrosion'
	  write(66,*) ' --------------------------------'
 	  write(66,*) ' *** sur-épaisseur=',dcor,' m'
	  write(66,*) ' - epaiss bordé      =',deltac
	  write(66,*) ' - epaiss âme cadres =',dyac
	  write(66,*) ' - epaiss sem cadres =',tyac
	  write(66,*) ' - epaiss âme raid   =',dxrc
	  write(66,*) ' - epaiss sem raid   =',txrc
      else                                                                                                         !février 2004
        if(isect1.eq.3) then                                                                             !février 2004
     	    write(66,*)														!février 2004
	    write(66,*) ' avec sur-épaisseurs de corrosion'					!février 2004
	    write(66,*) ' --------------------------------'					!février 2004
	    write(66,*) ' - epaiss âme épont. =',dyac						!février 2004
	    write(66,*) ' - epaiss sem épont. =',tyac						!février 2004
	    write(66,*) ' - epaiss âme raid   =',dxrc						!février 2004
	    write(66,*) ' - epaiss sem raid   =',txrc						!février 2004
        else                                                                                                         !février 2004
	    write(66,*)														!février 2004
	    write(66,*) ' avec sur-épaisseurs de corrosion'					!février 2004
	    write(66,*) ' --------------------------------'					!février 2004
	    write(66,*) ' - epaiss paroi mince =',epaisc					!février 2004
	    write(66,*) ' - epaiss âme raid   =',dxrc						!février 2004
	    write(66,*) ' - epaiss sem raid   =',txrc						!février 2004
        endif
      endif
	endif

      dens =  spec(nel)/9.81
      temp =  phil * q * width * pi/180.
      temp1=  temp * dens


	if((refthick_input.eq.1).and.(ityp1.ne.5)) then

      temp2=  temp * rend_global* rend_panel(nel) * eqp * 1000. * c1  !x 1000 car eqp en t/h

      else

      temp2=  temp * rend_global* rend_panel(nel) * eqp * 1000. * c1  !x 1000 car eqp en t/h

      endif


c     supplément de poids (en n) (sans les traverses)
      if(ityp1.ne.5) then                                                 !plaque                            !février 2004
      spoid1 = temp*spec(nel) * ( dcor +
     *                     (1.0+dw2)*(dcor3*hxr+dcor3*wxr)/epsr +
     *                     (1.0+dw3)*(dcor2*hya+dcor2*wya)/epsa    )
       poid1 = temp*spec(nel) * ( deltac +
     *                     (1.0+dw2)*(dxrc*hxr+txrc*wxr)/epsr +
     *                     (1.0+dw3)*(dyac*hya+tyac*wya)/epsa    )
      else                                                                      !epontille                     !février 2004
        if(isect1.eq.3)then                                                                             !février 2004
      poid1 = temp*spec(nel) * ( deltac +                                                               !février 2004
c     *                     (1.0+dw2)*(dxrc*hxr+txrc*wxr)/epsr +              !sept2006		!février 2004
     *                     (1.0+dw3)*(dyac*hyb+2*tyac*wya)/epsa    )       !février 2004
      spoid1 = temp*spec(nel) * (!(1.0+dw2)*(dcor3*hxr+dcor3*wxr)/epsr +       !sept2006		!février 2004
     *                     (1.0+dw3)*(dcor2*hyb+2*dcor2*wya)/epsa   ) !février 2004
        elseif(isect1.eq.1)then                                                                      !février 2004
      poid1 = temp*spec(nel) * ( deltac +                                                               !février 2004
c     *                     (1.0+dw2)*(dxrc*hxr+txrc*wxr)/epsr +              !sept2006		!février 2004
     *                     (1.0+dw3)*(pi*(hya*hya-(hya-2*epaisc)**2))       !février 2004
     *                                      /(4*epsa))                                                               !février 2004
      spoid1 = temp*spec(nel) * (!(1.0+dw2)*(dcor3*hxr+dcor3*wxr)/epsr +       !sept2006		!février 2004
     *                     (1.0+dw3)*(pi*(hya*hya-(hya-2*dcor2)**2))       !février 2004
     *                                      /(4*epsa))                                                               !février 2004
        elseif(isect1.eq.2) then                                                                      !février 2004
      poid1 = temp*spec(nel) * ( deltac +                                                               !février 2004
c     *                     (1.0+dw2)*(dxrc*hxr+txrc*wxr)/epsr +              !sept2006		!février 2004
     *                     (1.0+dw3)*(hya*hya-(hya-2*epaisc)**2)/epsa) !20.02.04
      spoid1 = temp*spec(nel) * (!(1.0+dw2)*(dcor3*hxr+dcor3*wxr)/epsr +       !sept2006		!février 2004
     *                     (1.0+dw3)*((hya*hya-(hya-2*dcor2)**2))/epsa)       !20.02.04
        endif                                                                                                         !février 2004
      endif                                                                                                         !février 2004
      spoids=spoids+spoid1 ! supplément de poids dû à la corrosion
       poids =poids+ poid1 ! poids + poids additionnel dû à dw2 et dw3

c     coût des plaques
	if(ityp1.ne.5)then												!février 2004		
        fmat1 = temp1 *c1* (1.+(deltac-dref)*1000.*dc1) * deltac 
c     coût des lisses
        fmat2 = temp1 *c2* (1.+dw2) * (1.+(dxrc-dref)*1000.*dc1)
     *                              * (dxrc*hxr+txrc*wxr)/epsr  
	else															!12.05.04
	  fmat1=0.000														!12.05.04		
        fmat2 =0.000													!12.05.04
	endif															!12.05.04

c     coût des cadres
	if(isect1.eq.0) then												!février 2004		    
        fmat3 = temp1 *c3* (1.+dw3) * (1.+(dyac-dref)*1000.*dc1)
     *                              * (dyac*hya+tyac*wya)/epsa  
	elseif(isect1.eq.1) then											!12.05.04
	  fmat3=temp1 * c3 *(1.+(epaisc  -dref)*1000.*dc1) * (1.0+dw3)    !12.05.04
     *                   * ((pi*(hya*hya-(hya-2*epaisc)**2))/(4*epsa )) !12.05.04	
	elseif(isect1.eq.2) then											!12.05.04
	  fmat3=temp1 *  c3 *(1.+(epaisc  -dref)*1000.*dc1) * (1.0+dw3)   !12.05.04
     *                    * ((hya*hya-(hya-2*epaisc)**2)/epsa )			!12.05.04
	elseif(isect1.eq.3) then
	  fmat3=temp1 * c3 *(1.+(dyac  -dref)*1000.*dc1) * (1.0+dw3)		!12.05.04
     *                   * ((2*dyac*hya+2*tyac*wya)/epsa )			!12.05.04
	endif															!février 2004	

c     coût de la matière de soudage des lisses
      if(ityp1.ne.5) then                                                                                    !février 2004
      fsou1 = temp * c8 * (1.+(dxrc-drefx)*1000.*dc8)*(2.-ialr)/epsr
      else                                                                                                         !février 2004
      fsou1=0.000                                                                                                  !février 2004
      endif                                                                                                         !février 2004
c     coût de la matière de soudage des cadres
      if(ityp1.ne.5) then                                                                                    !février 2004
      fsou2 = temp * c8 * (1.+(dyac-drefy)*1000.*dc8)*(2.-ialt)/epsa
      else                                                                                                         !février 2004
      fsou2 = 0.0000                                                                                           !février 2004
      endif                                                                                                         !février 2004
c     coût de la matière de soudage des nappes
      fsou3=0.  ! à introduire
c     fsou3 = temp * c8 * ??

c     coût de la main d'oeuvre : soudage des lisses sur bordé : p4
      if(ityp1.ne.5) then                                                                                    !février 2004
      fmdo1 = temp2 * p4 *(1.+(dxrc-drefx)*1000.*dp4)  /epsr
c     coût de la main d'oeuvre : constitution des lisses + extra : p9x
      fmdo2 = temp2 * p9x*(1.+(dxrc-drefx)*1000.*dp9x) /epsr
c     coût de la main d'oeuvre : soudage des cadres : p5
      fmdo3 = temp2 * p5 *(1.+(dyac-drefy)*1000.*dp5)  /epsa
c     coût de la main d'oeuvre : constitution cadres + extra : p9y
      fmdo4 = temp2 * p9y*(1.+(dyac-drefy)*1000.*dp9y) /epsa
c     coût de la main d'oeuvre : slot : p6
      fmdo5 = temp2 * p6         /(epsa*epsr)
c     coût de la main d'oeuvre : goussets et tap: p7
      fmdo6 = temp2 * ber*bet*p7 /(epsa*epsr)
c     coût de la main d'oeuvre : assemblmage des bordés nappes): p10
      fmdo7 = temp2 * p10*(1.+(deltac-dref)*1000.*dp10)
      else                                                                                                         !février 2004
      fmdo1 =0.0000                                                                                           !février 2004
      fmdo2 =0.0000                                                                                           !février 2004
      fmdo3 =0.0000                                                                                           !février 2004
      fmdo4 =0.0000                                                                                           !février 2004
      fmdo5 =0.0000                                                                                           !février 2004
      fmdo6 =0.0000                                                                                           !février 2004
      fmdo7 =0.0000                                                                                           !février 2004
      endif                                                                                                         !février 2004

c     coûts cumulés: for all the panels

      fmat11 = fmat11+fmat1 ! matière bordé
      fmat22 = fmat22+fmat2 ! matièrelisses
      fmat33 = fmat33+fmat3 ! matièrecadres
      fsou11 = fsou11+fsou1 ! matière soudage lisses
      fsou22 = fsou22+fsou2 ! matière soudage cadres
      fsou33 = fsou33+fsou3 ! matière soudage nappes
      fmdo11 = fmdo11+fmdo1 ! mdo soudage lisses sur nappe
      fmdo22 = fmdo22+fmdo2 ! mdo constitution de lisses synthetiques + extra works
      fmdo33 = fmdo33+fmdo3 ! mdo soudage cadres sur nappe
      fmdo44 = fmdo44+fmdo4 ! mdo constitution de cadres synthetiques + extra works
      fmdo55 = fmdo55+fmdo5 ! mdo pour découpage des slot
      fmdo66 = fmdo66+fmdo6 ! mdo soudage des tap et goussets
      fmdo77 = fmdo77+fmdo7 ! mdo assemblage et soudage des nappes

      ffmat = fmat1+fmat2+fmat3
      ffsou = fsou1+fsou2+fsou3
      ffmdo = fmdo1+fmdo2+fmdo3+fmdo4+fmdo5+fmdo6+fmdo7
      fmat  = fmat  + ffmat
      fsou  = fsou  + ffsou
      fmdo  = fmdo  + ffmdo
      fct   = ffmat + ffsou + ffmdo
      cout  = cout  + fct

	write(66,*)
	write(66,*) 'extra weight induced by corrosion',					!05.12.05
     *            ' (girder and secondary members not considered !!!)'	!05.12.05
	write(66,*) '-------------------------------------------'
      ppo=poid1-spoid1
	write(66,*) 'net   weight =',ppo   , 'in n'
	write(66,*) 'gross weight =', poid1, 'in n'
	write(66,*) 'extra weight =',spoid1, 'in n'
      write(66,*)

	write(66,*) 'cost of the panel (without girder)',
     *            ' with the simplified cost model'
	write(66,'(70(1h-))') 
	write(66,*) 'total cost =     fmat   +     fsou    +    fmdo ',
     *              '   in euro,$,...'
      write(66,'(4(2x,e11.4) )') fct,ffmat,ffsou,ffmdo
      write(66,*)

      if(icout.lt.2) then	!sept2006		! imp.diff. selon module coût simplifié ou pas !newcost		!obj inertie
       if(nel.eq.1) then
        write(67,'(2a)')'panel#:   cost =    fmat   +    fsou +   fmdo',
     *         ' (euro,$ or..) - gross weight and corrosion weight(n)'
        write(67,'(98(1h-))')
       endif
       write(67,'(2x,i3,6(1x,e11.4) )') nel,fct,ffmat,ffsou,ffmdo,
     *                                 poid1,spoid1
      endif !newcost

      if(nel.eq.neto) then
       ppo=poids-spoids
	 spoids1 = spoids			!sept2006
       iz=66
  20   continue
       write(iz,*) 'total weight: complete structure (girders and'
       write(iz,*)	 'secondary members excluded)'						!05.12.05
	 write(iz,*) '-------------------------------------------------'
       write(iz,*) '   weight (net   thickness)       = ',ppo   ,' n'
       write(iz,*) '   weight (corrosion thickness)   = ',spoids,' n'
       write(iz,*) '   weight (gross thickness)       = ',poids ,' n'
       write(iz,*)
       if(impr2.ge.0)then
c	 write(iz,*) 'total cost: complete structure (girders excluded)'!redressage !septembre 2007
       write(iz,*) 'total cost: complete structure (girders and'		!redressage !septembre 2007
	 write(iz,*)	' straightening excluded)'						!redressage !septembre 2007
	 write(iz,*) '-------------------------------------------------'

       write(iz,*) '-------------------------------------------------'
       write(iz,*) '1* material from plating       = ',fmat11,' euro,$'
       write(iz,*) '   material from long stiff.   = ',fmat22,' euro,$'
       write(iz,*) '   material from trans. frame  = ',fmat33,' euro,$'
       write(iz,*) '   material (total)            = ',fmat  ,' euro,$'
       write(iz,*)
       write(iz,*) '2* consomables :stiff          = ',fsou11,' euro,$'
       write(iz,*) '   consomables :frame          = ',fsou22,' euro,$'
       write(iz,*) '   consomables :plate          =    not considered'
       write(iz,*) '   consomables (total)         = ',fsou  ,' euro,$'
       write(iz,*)
       write(iz,*) '3* labor cost for'
       write(iz,*) '   - stiff. on plate      (p4) = ',fmdo11,' euro,$'
       write(iz,*) '   - stiff buiding        (p9) = ',fmdo22,' euro,$'
       write(iz,*) '   - frames. on plate     (p5) = ',fmdo33,' euro,$'
       write(iz,*) '   - frames. building     (p9) = ',fmdo44,' euro,$'
       write(iz,*) '   - slot cutting         (p6) = ',fmdo55,' euro,$'
       write(iz,*) '   - stiff.bracket,tap    (p7) = ',fmdo66,' euro,$'
       write(iz,*) '   - plating building     (p10)= ',fmdo77,' euro,$'
       write(iz,*) '   labor cost (total)          = ',fmdo  ,' euro,$'
       write(iz,*)
	endif
       write(iz,*) '*** total cost  (1+2+3)        = ',cout  ,' euro,$'
       write(iz,*) '      !!! using the simplified lbr5 model !!! '
       write(iz,*) ' this result will differ using a detailled database'
       write(iz,*)
       if(icout.lt.2) then !sept2006		 !opt. poids ou coût selon module coût simplifié !newcost	!obj inertie
         if(iz.eq.66) then
           iz=67
           goto 20
         endif
       endif  !newcost
c       rewind 57
      endif

      return
      end

c***********************************************************************
c***********************************************************************

      subroutine semel(hya,dya,wya,tya,xmin,xmax,ind,kk,iprint,
     *                 nel,isem,ityp1,isect1,impr2)	!sept2006                                                 !février 2004
      implicit real *8 (a-h,o-z)
      character*6 type(3)

c***********************************************************************
c     subroutine semel
c     ****************
c     cette subroutine de modification des dimensions des semelles en fonction
c     de restrictions géométriques.
c
c     dw = hauteur   de l'âme (web),
c     tw = épaisseur de l'âme (web),
c     df = largeur   de la semelle (flange),
c     tf = épaisseur de la semelle (flange),
c
c     création     : 6-06-96 (ph. rigo pour l.b.r.-5)
c     modif: 13-3-98 : isem=1: df < 16.tf  et isem=2: df < 32.tf
c
c     last modification:13-3-98
c
c***********************************************************************
c   les variables de conception  ; nbrxi = nvar(nel)
c   -------------------------------------------------
c       1       delta =  épaisseur du bordage
c       2       hya   =  hauteur de l'âme des aiguilles
c       3       dya   =  épaisseur de l'âme des aiguilles
c       4       wya   =  largeur des semelles des aiguilles
c       5       epsa  =  entredistance entre aiguilles
c       6       hxr   =  hauteur de l'âme des raidisseurs
c       7       dxr   =  épaisseur de l'âme des raidisseurs
c       8       wxr   =  largeur des semelles des raidisseurs
c       9       epsr  =  entredistance entre raidisseurs
c       variables de conceptions pour les épontilles:            !février 2004
c       1       hya   =  hauteur de la demi-âme                  !février 2004
c       2       dya   =  épaisseur de l'âme                      !février 2004
c       3       wya   =  largeur de la semelle                   !février 2004
c       4       epais =  épaisseur de la paroi mince             !février 2004
c       5       epsa  =  entredistance entre épontilles          !février 2004

      type(1)='cadre'
      type(2)='raid.'
      type(3)='épont.'                                                                             !février 2004

c ****************************************************************
c 1.    initialisations et calcul des ratios al1, al2 et al3
c ****************************************************************
      dw=hya
      tw=dya
      df=wya
      tf=tya
      if (isect1.eq.3) then                                                                      !février 2004
      dw=2*hya                                                                                           !février 2004
      endif                                                                                                  !février 2004

c     pour condition tf < 2.tw
      al1= 2.*tw/tf

	if(isem.eq.1) then
c       pour condition  df < 16.tf
        sq2= sqrt(2.)
c       pour condition 8.tf < df
        al2= dsqrt(df/(8.*tf))
	elseif(isem.eq.2) then
c       pour condition  df < 32.tf
        sq2= 2.
c       pour condition 8.tf < df
        al2= dsqrt(df/(8.*tf))
	elseif(isem.eq.3) then
c       pour condition  df < 30.tf
        sq2= sqrt(2.5)
c       pour condition 12.tf < df
        al2= dsqrt(df/(12.*tf))
	elseif(isem.eq.4) then
c       pour condition  df < 15.tf
        sq2= 3.273
c       pour condition 1.4.tf < df
        al2= dsqrt(df/(1.4*tf))
	elseif(isem.eq.5) then
	  al1 = al1*2.
c       pour condition  df < 15.tf
        sq2= 1.581
c       pour condition 6.tf < df
        al2= dsqrt(df/(6*tf))
	endif
	al3= sq2/al2

      if(iprint.ge.1) then
           write(666,*) 'al1, al2, al3 =',al1,'  ',al2,'  ',al3
      endif


c ****************************************************************
c 2.  algorithme  pour al1>1    c.à.d  tf < 2.tw  est vérifié
c ****************************************************************
      if (al1.lt.1) goto 1

	al11 = al1/2.
	if (al11.lt.1) then !            !!!condition tw<tf respectée
	!(al11.lt.1.or.isem.eq.1.or.isem.eq.2) then 

c 2.1  1 < al2 < 1.41 (ou 2)
c      ---------------------
      if ((al2.ge.1).and.(al2.le.sq2)) goto2

c 2.2  al2 < 1
c      ---------
      if (al2.lt.1) then
	   df=df/al2
	   tf=tf*al2
	endif

c 2.3  al2 > 1.41 (ou 2)  (al3 < 1)
c      ----------------------------
      if (al2.gt.sq2) then
	   df=df*al3
	   tf=tf/al3
	endif

	else							!!!condition tw<tf pas respectée
	
c      1 < al2 < 1.41 (ou 2)
c      ---------------------
      if ((al2.ge.1).and.(al2.le.sq2)) then
	   df=df/al11
	   tf=tf*al11
	endif

c       al2 < 1
c      ---------
      if (al2.lt.1) then
	   al33 = 0.667*al2 + 0.333*al11
	   if (al33.gt.1) al33=1.
	   df=df/al33
	   tf=tf*al33
	endif
	
c      al2 > 1.41 (ou 2)  (al3 < 1)
c      ----------------------------
      if (al2.gt.sq2) then
	   al33=1./al3
	   coeff = max(al11,al33)
	   df=df/coeff
	   tf=tf*coeff
	endif

	endif !fin condition sur tw/tf
		
      goto 2

   1  continue
c ****************************************************************
c 3.  algorithme pour al1<1     c.à.d  tf < 2.tw  n'est pas vérifié
c ****************************************************************
      al22= al2/al1

c 3.1  1 < al22 < 1.41 (ou 2)
c      ---------------------
      if ((al2.ge.1).and.(al2.le.sq2)) then
           df=df/al1
           tf=tf*al1
      endif

c 3.2  al22 < 1
c      ---------
      if (al2.lt.1) then
	   coeff = min(al1,al2)
	   df=df/coeff !al2
	   tf=tf*coeff !al2
      endif

c 3.3  al2 > 1.41 (ou 2)  (al3 < 1)   (! il s'agit bien de al2 et pas de al22)
c      -------------------------------------
      if (al2.gt.sq2) then
           al33=0.6667 * al3 + 0.333 /al1
         if (al33.gt.1.) al33=1.
         df=df*al33
         tf=tf/al33
      endif

   2  continue
c ****************************************************************
c 4.  vérification des bornes
c ****************************************************************
        k=0
      if (df.lt.xmin) then
        write(666,*) ' borne atteinte  df=',df,'  xmin=',xmin
        write(29 ,*) ' borne atteinte  df=',df,'  xmin=',xmin			!sept2006			!bug
	  df=xmin
        k=1
      else if (df.gt.xmax) then
        write(666,*) ' borne atteinte  df=',df,'  xmax=',xmax
        write(29 ,*) ' borne atteinte  df=',df,'  xmax=',xmax			!sept2006			!bug
	  df=xmax
        k=1
      endif

c      if (df.lt.(dw/2.)) then
c         df=dw/2.
c         k=1
c       else if (df.gt.dw) then
c         df=dw
c         k=1
c       endif

      if (k.eq.1) then
         tf=wya*tya/df
c          write(666,*) ' après modif des bornes'
c          write(666,*) 'df=',df,'  tf=',tf
        endif

   3  continue


c ****************************************************************
c 5.  calcul des coéficients pour modifier les sensibilités
c     concernant les restrictions  2.df < dw  < df (code 201 et 202)
c ****************************************************************
c     pour la condition tf < 2.tw avec les nouvelles valeurs df et tf
      al1= 2.*tw/tf
      if (al1.lt.1) then
        ind=1
        if(iprint.ge.1) then
           write(666,*) 'la condition (tf < 2.tw) n''est pas',
     *                 ' respectée'
		 write(29 ,*) 'la condition (tf < 2.tw) n''est pas',	!sept06				!bug
     *                 ' respectée'								!sept06				!bug
        endif
      else
        ind=0
      endif



c ****************************************************************
c 6.  résultats
c ****************************************************************
      if(impr2.ge.-1)then			!15.10.05	!sept2006
	write(666,100) nel,wya,tya,df,tf,type(kk),ind
c       write(  *,100) nel,wya,tya,df,tf,type(kk),ind
	endif						!15.10.05	!sept2006

      wya=df
      tya=tf

      return

 100  format(3x,i2,2x,e13.6,1x,e13.6,4x,e13.6,1x,e13.6,4x,a5,3x,i2)
      end

c ----------------------------------------------------

	subroutine select(a,b,d,e)        !!!aout04
c     *****************************
      implicit real*8 (a-h,o-z)

	dimension d(9),e(9)	!!!aout04
c     a,b  inputs
c     c    output avec c la valeur de maximun de a et b mais avec le signe
c       ex: a=-6 et b=5, d'où a= -6 car abs(a) > abs(b)

      c=dmin1(a,b)
      a=dmax1(a,b)
	if (b.gt.a) then				!!!aout04
	  do i=1,9						!!!aout04
	    d(i)=e(i)					!!!aout04
	  enddo							!!!aout04
	endif							!!!aout04
	if(dabs(a).le.dabs(c)) then		!!!aout04
	  a=c							!!!aout04
	  if (b.lt.a) then				!!!aout04
	    do i=1,9					!!!aout04
	      d(i)=e(i)					!!!aout04
	    enddo						!!!aout04
	  endif							!!!aout04
	endif							!!!aout04

      return
      end

