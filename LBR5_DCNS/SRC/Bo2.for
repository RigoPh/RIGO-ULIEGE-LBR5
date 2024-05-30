      subroutine bo2(nel,jlmax,indaig,indrai,
     *		   ne,width,ich,imom,iprint,is,nsol,im,img,itera,
     *		   i66,i67,m1tabl1,sxm,nbuck)							!dcn07							
      use sharedvar
      implicit real*8(a-h,o-z)
      real*8 lamb
      dimension equ(8),disl(1710),fp(20),nf(22),dw(11,5)
	dimension plat(neto),fl(neto),
     *          sig1(neto),sig2(neto),sig3(neto),sig4(neto),tau(neto),
     *          wpl1(neto),wpl2(neto),vmaxsig(neto),vmaxsigc(neto),
     *          sigmag(neto),vs(72),sigplaque(iptmax),
     *          dsigplaque(iptmax*9)
      dimension xi(nsol),xf(nsol),chamax(5,nsol)
	dimension dchamax(5,nsol,9)
      dimension eff(9690),eff2(5400),conc(750),z2(2295),
     *          effcomb(9690),us(20000),sxm(iptmax+2*neto+11)
      dimension dequ(8),nom1(9),id1(9),m1tabl1(nmax)
      dimension sigxplaque(iptmax*neto),sigyplaque(iptmax*neto),
     *          phiplaque(iptmax*neto),sigvmtplaque(iptmax*neto),
     *          sigvmcplaque(iptmax*neto),indplaque(iptmax*neto),
     *          sigmx(iptmax*neto),sigmy(iptmax*neto)
      data nom1/'w','wø','wøø','wøøø','u','uø','v','vø ','vøø'/
      data id1/8,3,9,10,1,11,2,12,13/

c***********************************************************************
c     subroutine bo2
c     ===============
c     subroutine de mise en forme des resultats par sommation sur les   
c     inconnues hyperstatiques du systeme.
c     (la resolution se fait panneau par panneau ).                     
c                                                                       
c    créer : thèse de doctorat, ph. rigo (1987)	
c    ------
c    modif :9-02-96 				    	
c    ------ 6-05-96 : subr. hughes, read(99), cha2m et cha3m
c          10-05-96 : common/di/ ... tpl, ...
c          31-05-96 : modification des arguments de la subr. hughes
c          22-07-96 : subr contr dans bo2 et développement de la subr paik
c          18-02-97 : subr ushull pour calcul de la resistance ultime 
c           6-03-97 : introduction de d(raid) c.à.d. entr différent de epsr 
c          23-03-99 : modification de l'allocation mémoire 
c          20-11-00 : introduction de la subr. stiff (flexion locale)
c          15-01-02 : modification subr. hughes (et impression dans sol2)
c          15-04-03 : combinaison resul + stiff (f. bair)
c           4-02-04 : subr plaque pour resul + stiff + pltben (bair)
c		 04-02-04 : epontilles
c
c   dernière modif: 04-02-2004
c   ---------------
c***********************************************************************

c***********************************************************************
c	ordre ds sens1(jj,4,9,neto) avec jj=1 à 9
c		      1   2    3     4      5   6    7   8     9 
c	ordre ds defh,defa,defb, ... et sensh,sensa,sensb, ...
c               u,v,wø,ny,nyx,my,ry,w,wøø,wøøø,uø,vø,vøø
c               1 2 3  4  5    6  7 8  9  10   11 12 13
c	relation de position entre sens1 et sensh,sensa,...,defh,defa,defb, ...
c	sens1(i) = fct[sensh(idi)]
c***********************************************************************
	call annuld(sig2,neto) !dad   25.01.2008
	if(nel.eq.1) call annuld(sigmag,neto) !mise à zéro (au début de chaque cas de charge)    !nov 2006
	call annuld(vs,72)		!mise à zéro (pour chaque panneau)								 !nov 2006
c caracteristiques panneau
      ipt=ipts(nel)
      ityp1=itype(nel)
      isect1=isect(nel)
      e1=e(nel)
      eta1=eta(nel)
      sigy1=sigy(nel)
      sigm1=coefk(nel)			!r&d15

	if(impr2.ge.-1) then		!sept06
      write(66,73) nel
	endif						!sept06

      call annuld(eff2,5400)
      call annuld(eff,9690)
	call annuld(conc,750)
      ! sens2 sensibilites cumulees relatives au contraintes (sc borde, raid, cadre, trav)
      call annuld(sens2,16*iptmax*9*neto)  ! calcule dans subr resul !avril2003
      ! sens3 sensibilites cumulees des contraintes non combinees (sx, sy, txy, etc
      call annuld(sens3,21*iptmax*9*neto)  ! matr. de travail de la subr. resul
      ! sens4 = sensibilites cumulees de sens1 (v,u,w, etc.
      call annuld(sens4, 9*iptmax*9*neto)  ! calcules dans bo2

      read(99)  abtr,phil,mt,teta,xi,xf,hxtr,wxtr,q,kse,kst,ksa,
     *          txtr,dxtr,ksr,aa,aa,aa,aa,delto,ivaria,const,
     *          const2,const3,
     *          delta,dya,tya,wya,hya,epsa,hxr,dxr,wxr,txr,epsr,entr,
     *          chamax,dchamax,ns,epais,hight,heff								            !février 2004
	if(ityp1.eq.5)then													!février 2004
	  read(306) aire,aiy,aix,sredy,sredx,tork	            			!février 2004	!extension neto	!fev2007
	endif																!février 2004

      a1=0.0
      b1=0.0
      cc1=0.0
      d1=0.0
      a2=0.0
      b2=0.0
      cc2=0.0
      d2=0.0

c*********************************************************************************
c 1.  début de la boucle sur le nbre de termes de la série de fourier "do 9 ..."
c*********************************************************************************
c 1.1 lecture des resultats intermediaires (pour le panneau nel considéré) 
c     --------------------------------------------------------------------- 
      do 9 nz=1,jlmax

      if(mod(nz,2).eq.1) then  
	 iterm=1						 ! iterm = 1 si ijk est impair
	else       
       iterm=2						 ! iterm = 2 si ijk est pair
	endif
      if(ifonct(iterm).eq.0) goto 9	 !il s'agit d'un terme pair + charge symétrique

      read(nel+100)disa,disb,disc,disd,argq,m,hyp,lamb,ijk

      if((kse.eq.0).and.(ich.eq.0).and.(imom.eq.0)) then
        do 821 i=1,1710
 821    dish(i,is)=0.
      else
        read(nel+100)((dish(l,ll),l=1,1710),ll=1,nsol)
      endif

c     lecture des forces de bord  (zsn solution de mdr)
c     -------------------------------------------------
      do j=1,nsol
         read(700+nel) (zsn(i,j),i=1,8)
      end do

      do i=1,8
         equ(i)=zsn(i,is) ! is est le cas de charge étudié
      end do

c ************************************
      ind=0 ! cad sans effets de bord (jlbord)


c  nb:a utiliser pour obtenir les resultats relatifs à disc seul (plaque infinie).                                                 
c	do 567 i=1,8
c  567 equ(i)=0.0
c	 equ(1)=1.0
c      do 568 i=1,1710
c  568 dish(1,i)=0.

c 1.2 impression des inc. hyperstatiques agissant sur les bords (y=0 et y=yo)                                                    
c     ------------------------------------------------------------------------
      nbre=3
      if(impr2.eq.-3) goto 215
      nbre=22
      if((impr2.eq.-1).or.(impr2.eq.-2)) nbre=10
      if(impr2.ge.-1) then		!sept06
	write(66,74)nz
      write(66,72)(equ(i),i=1,8)
	endif					!sept06

  215 continue

c ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c 1.3 ++++++++ début de la boucle relative aux calculs des sensibilités ++++++++
c ++++++++++++   de  u,v,wø,ny,nyx,my,ry,w,wøø,wøøø,uø,vø,vøø		++++++++
c ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

c     calcul des  sensibilites    c.à.d  sens1(9,4,ntot) non cumule et sens4(cumule) 
c     dérivées des (u,v,w,uø,etc.) par rapport aux variables de conception. 

	if(iopti.eq.0) goto 194
      read(nel+400) sensh,sensa, sensb, sensc, sensd,			!extension neto	!fev2007
     *                    sensaa,sensbb,senscc,sensdd,
     *           defa,defb,defc,defd,defaa,defbb,defcc,defdd

c     lecture des derivees des forces de bord  (dzsn solution de mdr2)    
     	j1=8*(nel-1)
	do  iss=1,nsol
		do  j=j1+1,j1+8
 			read(700+nel) (dzsn(j,ivar,iss),ivar=1,ntot) 	!dzsn(ne,9*neto,nsol)
		end do
	end do

c     boucle 184/183:boucle sur toutes les variables de conception (kvar=1,ntot);
c                  (il y a nbrxi variable de conception par panneau)
c     boucle 180 = boucle sur les points de calcul et les fonctions

	kvar=0
      do 184 ipan=1,neto
        i2=8*(nel-1)
        nbrxi=nvar(ipan)
        do 183 kk=1,nbrxi
                kk1=kvar+kk
                k=nxit(kk,ipan)
            do 2109 i=1,8
 2109       dequ(i)=dzsn(i+i2,kk1,is)

        do 180 i=1,ipt
          do 180 jj=1,9
	      j=id1(jj)

c sens1 est relatif au panneau étudié par bo2 cad le panneau nel,
c      mais il donne les sensibilités pour les variables de conception
c      de tous les panneaux (ipan)

      sens1(jj,i,k,ipan) = dequ(1)*defc(j,i)   + dequ(2)*defcc(j,i)
     *                   + dequ(3)*defd(j,i)   + dequ(4)*defdd(j,i)
     *                   + dequ(5)*defa(j,i)   + dequ(6)*defaa(j,i)
     *                   + dequ(7)*defb(j,i)   + dequ(8)*defbb(j,i)
      if(ipan.eq.nel)then
      sens1(jj,i,k,ipan)=sens1(jj,i,k,ipan) + sensh(j,i,k,is)
     *                + equ(1)*sensc(j,i,k) + equ(2)*senscc(j,i,k)
     *                + equ(3)*sensd(j,i,k) + equ(4)*sensdd(j,i,k)
     *                + equ(5)*sensa(j,i,k) + equ(6)*sensaa(j,i,k)
     *                + equ(7)*sensb(j,i,k) + equ(8)*sensbb(j,i,k)
      endif    
	sens4(jj,i,k,ipan) = sens4(jj,i,k,ipan) + sens1(jj,i,k,ipan) ! version cumulee de sens1
	
  180   continue                                                		
  183   continue                                                		
        kvar=kvar+nbrxi
  184 continue                                                		

	if ((nz==jlmax).and.(iprint.ge.1)) then
	 write(66,*) ' sensibilites cumulées (déplacements) '
       write(66,*) ' ************************************ '

       do 181 ipan=1,neto
	  nbrxi=nvar(ipan)
        write(66,*) 'fct.       pt.1          pt.2          pt.3',
     *              '          pt.4          pt.5'
       do 181 kk=1,nbrxi
        k=nxit(kk,ipan)
        write(66,*)' variable de conception nø',k,' du panneau nø',ipan
        write(66,*)' --------------------------------------------------'
        do 190 jj=1,9
           write(66,182) nom1(jj),(sens4(jj,i,k,ipan),i=1,ipt)
  190   continue  
  181  continue
      endif

  194 continue

c +++++ fin de la boucle relative aux calculs des sensibilités de u,v,w, ... +++
c ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

c 1.4 cummul des effets de la charge exterieure et des charges de bords pour 
c     chaque panneau                                                     
c     ---------------------------------------------------------------------
	do 699 i=1,22												!03.03.04
  699	nf(i) =0													!03.03.04
      do 700 i=1,3                                                      
  700 nf(i)=i                                                           
      if(impr2.ne.-3) then		!sept06
        j=3                                                             !bbo01110
        do 701 i=24,45                                                    
          if((impr2.eq.-1).or.(impr2.eq.-2)) then     
			if (ityp1.eq.5) then								!03.03.04
			if((i.eq.26).or.(i.eq.27)) goto 701					!03.03.04
			if((i.ge.29).and.(i.le.31)) goto 701				!03.03.04
			if((i.ge.33).and.(i.le.39)) goto 701				!03.03.04
			if((i.eq.41).or.(i.ge.43)) goto 701					!03.03.04
			else												!03.03.04
             if(i.ge.39) goto 701                                       !nx,mx,etc.
             if((i.ge.24).and.(i.le.28)) goto 701                       !my,ny,etc.
			endif												!03.03.04
	    endif
        if((i.eq.29).or.(i.eq.36).or.(i.eq.37)) goto 701                !wø,vøøøetuøøø
        j=j+1                                                             
        nf(j)=i
 701  continue
      endif
c
      do 80 k=1,m
      do 80 i=1,nbre
        nfonct=nf(i)
        if (nfonct.eq.0) goto 80
        goto(2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
     *       2,2,2,2,1,2,2,2,1,1,1,2,2,1,2,1,2,2,1,1),nfonct
    1   sy=1.
        goto 813
    2   sy=-1.
  813   call pconti(disa,disb,disc,disd,equ,k,nfonct,dish(1,is),sy)
   80 continue

      if(mt.eq.0) goto 205
      if(is.gt.1)then
        is1=140+is*20
      else
        is1=0
      endif

      do 81 i=1,mt
        fp(i)=hyp(i+is1)
        jm=i+mt
        fp(jm)=hyp(jm+is1)
        if(ind.eq.0) goto 84  ! cad sans effets de bord nb et mb (jlbord)
  84    continue
        do 81 j=1,8
          ij=i+20*j
          fp(i)=fp(i)+hyp(ij)*equ(j)
          ij=ij+mt
          fp(jm)=fp(jm)+hyp(ij)*equ(j)
   81 continue

      do 83 i=1,2*mt
        fp(i)=fp(i)/10000.
  83  continue
c
c
      do 114 i=1,mt
        uecr(2*i+30)=abtr(i)
        uecr(2*i+31)=abtr(i)
  114 continue
  205 sm=phil/30.
      do 109 i=1,31
  109 uecr(i)=float(i-1)*sm
      do 902 ii=1,2295
  902 z2(ii)=0.

c
c 1.5 calcul des resultats en 31 points selon la hauteur et en 5 points 
c     selon la largeur du panneau ainsi qu'au droit destraverses.       
c     ------------------------------------------------------------------
      do 901 i=1,nbre
      nfonct=nf(i)
      goto(17,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,
     *18,18,18,18,18,17,17,17,17,18,17,17,17,18,18,18,17,17,18,17,18,17,
     *17,18,18),nfonct
   17 sy=-1.
      goto4
   18 sy=1.
    4 if (nfonct.eq.0) goto 901
      call compu(phil,fp,dish(1,is),disc,disb,abtr,m,argq,mt,uecr,teta,
     *           z2,nfonct,sy,31)
  901 continue


c 1.7 calculs des resultats (déplacements et contraintes)
c     -----------------------------------------------------
c      if(impr.ne.-3) goto 3000
c      call sorti(lamb,dis,mt,eff,z2)
c      goto 9
c 3000 continue

	if(ityp1.ne.5) then
      call resul(wya,tya,dya,hya,ksa,wxr,txr,dxr,hxr,ksr,
     *           delta,eta1,lamb,e1,nz,mt,kst,
     *           sigy1,epsa,epsr,entr,ityp1,
     *           q,jlmax,indaig,indrai,nel,iprint,
     *           eff(1),eff2(1),eff2(2041),eff2(3571),eff2(5101),
     *           conc,z2,sxm(1),sxm(iptmax+1),sxm(iptmax+neto+1))
	else
      call resul2(wya,tya,dya,hya,ksa,wxr,txr,dxr,hxr,ksr,
     *            aire,aiy,lamb,e1,nz,mt,kst,
     *            epsa,epsr,entr,tork,isect1,epais,
     *            q,jlmax,indaig,indrai,nel,iprint,
     *            eff(1),eff2(1),eff2(2041),eff2(3571),eff2(5101),
     *            conc,z2,
     *            sxm(1),sxm(iptmax+1),sxm(iptmax+neto+1))
	endif															


c     flexion locale des raidisseurs
c     --------------------------------

      if(nz.eq.1) then ! premier terme de la serie de fourier
        if(iopti.ge.1) then
c         recherche des restrictions choisies : iq=1 oui et iq=0 non
	    iq50=0
	    do i=1,m1tabl1(nel)            !  im=compteur des restr. struct.
            if((lcont(1,im+i).eq.12).or.    ! restr nø 12 : bord l/2 + sig stiff                 !avril2003
     *         (lcont(1,im+i).eq.13).or.    ! restr nø 13 : bord l/2 + sig stiff + sig pltb      !avril2003
     *         (lcont(1,im+i).eq.33).or.    ! restr nø 33 : taujab x=0 + tau stiff               !avril2003
     *         (lcont(1,im+i).eq.34).or.    ! restr nø 34 : sigsem x=l/2 + sig stiff             !avril2003
     *         (lcont(1,im+i).eq.35).or.    ! restr nø 35 : sigjab x=l/2 + sig stiff + tau stiff !avril2003
     *         (lcont(1,im+i).eq.36).or.    ! restr nø 36 : sigjas x=l/2 + sig stiff             !avril2003
     *         (lcont(1,im+i).eq.51).or.    ! restr nø 51 : flèche 1/384
     *         (lcont(1,im+i).eq.52).or.    ! restr nø 52 : flèche 5/384
     *         (lcont(1,im+i).eq.54).or.    ! restr nø 54 : sigma semelle
     *         (lcont(1,im+i).eq.55).or.    ! restr nø 55 : sigma bordé
     *         (lcont(1,im+i).eq.56).or.    ! restr nø 56 : 1.73*tau
     *         (lcont(1,im+i).eq.57).or.    ! restr nø 57 : flèche maille 1/384
     *         (lcont(1,im+i).eq.58)) then  ! restr nø 58 : flèche maille 5/384
		    iq50=1  
		    goto 46
		  endif  
          enddo
  46      continue
	  endif

	if (ityp1.ne.5) then					!nov2006

      call stiff(iprint,nel,e1,eta1,sigy1,width,q,phil,
     *           ploc(nel),xi,xf,chamax,dchamax,kse,nsol,is,
     *           delta,epsa,hxr,dxr,wxr,txr,epsr,entr,ksr,
     *           plat(nel),fl(nel),
     *	     sig1(nel),sig2(nel),sig3(nel),sig4(nel),tau(nel),flr,
     *           wpl1(nel),wpl2(nel),				      
     *           vs(1),vs(10),vs(19),vs(28),vs(37),vs(46),vs(55),vs(64),
     *           iq50)
      endif										!nov2006
	endif

c
c 1.8 verification de l'equilibre du panneau (si impr = 0 ou 1)
c     --------------------------------------
      if(impr2.le.-1) goto 9
      if((nz.gt.1).and.(impr2==0)) goto 9

      call equil(mt,z2,lamb,xi(is),xf(is),ivaria,phil,ijk,
     *           kse,q,nel,ind,a1,b1,cc1,d1,a2,b2,cc2,d2,delto,width)

   9  continue ! boucle sur nz (nbre de termes)
   !!! fin de la boucle sur le nbre de termes de la série de fourier


c***********************************************************************
c***********************************************************************

c 2.0 prise en compte de la flexion locale des raidisseurs              !avril2003
c     ----------------------------------------------------              !avril2003
c     combinaison des résultats donnés par resul et par stiff           !avril2003

      call combine(eff,sig1(nel),sig2(nel),sig3(nel),                   !avril2003
     *             sig4(nel),tau(nel),                                  !avril2003
     *             mt,effcomb,nel,txr)                                  !avril2003


c 2.1 calcul du sigma plaque et de sa derivee                           !avril2003
c     ---------------------------------------                           !avril2003

	if(ityp1.ne.5) then								!février 2004
       iii=(nel-1)*iptmax+1                                              !fev04
      call plaque(epsa,epsr,entr,delta,vs(1),nel,
     *            hya,hxr,width,phil,q,effcomb,
     *            chamax(1,is),dchamax(1,is,1),
     *            ploc(nel),e1,eta1,nvar(nel),sig1(nel),eff,ipt,
     *            sigplaque,dsigplaque,sigxplaque(iii),sigyplaque(iii),
     *            phiplaque(iii),sigvmtplaque(iii),sigvmcplaque(iii),
     *            indplaque(iii),sigmx(iii),sigmy(iii))                                                        !fev04
                                                  
	endif


c***********************************************************************
c***********************************************************************
c 3.  calcul de l'epaisseur minimale (cfr progr. de hughes, edt. sname)
c     ------------------------------------------------------------------
c	tpl  = epaisseur minimale de hughes
c	dtpl = tpl (hughes)-delta  = valeur de la restriction c(j) = tpl-delta<0
c	tpla = d(tpl)/d(xi=epsa)   = dérivées de c(j) par rapport à epsa
c	tplr = d(tpl)/d(xi=epsr)   = dérivées de c(j) par rapport à epsr
 
c minimum plate thickness assessment : yielding and buckling
c--------------------------------------------------------------
c      sr1 = ratio applied stress/yield stress (?<? 1.0)
c      sr2 = global interaction ratio: applied stress/critical stress (?<? 1.0)
c
cpanel <-- panel dimensions -->  minimum thickness <- collapse scenario & applied stress --->  yielding   buckling  critical stresses
c      l(long) b(trans) d(thick)  d(min)  d/d(min)   mode  section   sx    sy     tau    plat  (s/sy<1)  (s/scr<1) sx(cr) sy(cr) t(cr)
c         (m)     (m)    (mm)      (mm)   (> 1 ??)           y/yo   n/mm2  n/mm2  n/mm2  (m)   von-mises interact  n/mm2  n/mm2  n/mm2
c                                                                                                sr1     form(sr2)
c  1    0.371   0.873     8.00     7.82    1.02    yielding  1.00    47.1   51.7    0.0   0.00  1.000      0.000   337.9  117.8  513.0
c1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890

      if(nel.eq.1) then
        if(langue==1 ) then ! french
           write(67,'(a/62(1h-))')
     *     ' epaisseur minimale de bordé: plastification et voilement'
	  else                ! english
           write(67,'(a/62(1h-))') 
     *     ' minimum plate thickness assessment : yielding and buckling'
	  endif
        if((itera.ge.1).and.(iopti.ge.1)) write(666,562)
        write( 67,562)
	endif

 562  format(5x,' sr1 = ratio of applied stress/yield ',
     *   'stress (?<? 1.0)'/5x,' sr2 = global interaction ratio:',
     *   ' applied stress/critical stress (?<? 1.0)'//
     *  5x,' for yielding (global stresses(sx,sy) are combined with ',
     *                     'the stress induced by the stiff bending)'/
     *  5x,' for buckling (global stresses(sx,sy) are considered alone)'
     *   //'panel <-- panel dimensions -->  minimum ',
     *   'thickness <- collapse scenario & applied stress ---> ',
     *   ' yielding   buckling  elast.crit.stress'/
     *   '      l(long) b(trans) d(thick)  d(min)',
     *   '  d/d(min)   mode  section   sx    sy     tau    plat  ',
     *   '(s/so <1) (s/scr<1) sx(cr) sy(cr) t(cr)'/
     *   '         (m)     (m)    (mm)      (mm) ',
     *   '  (> 1 ??)           y/yo   n/mm2  n/mm2  n/mm2  (m)   ',
     *   'von-mises interact  n/mm2  n/mm2  n/mm2'/
     *    t97,'sr1     form(sr2)'/)
  
      if(iopti.ge.1) then
c     vérification si la restriction "dmin-d<0" est choisie : iq=1 oui et iq=0 non
	    iq=0
		if(ityp1.eq.5) then										!février 2004
		iq=0													!février 2004
		else										   			!février 2004
	    do  i=1,m1tabl1(nel)
            if(lcont(1,im+i).eq.4) then    ! dmin-d<0 =  restrict nø4
	        iq=1  ! restriction 4 selectionnée
	        !sigm1=cjmax3(nnsol(is),nel,i)						!r&d15
			goto 45
	      endif
          enddo
  		endif													!février 2004		
45      continue
	endif
	if(ityp1.ne.5) then	
      if(phil.ge.1.) then  ! vérification si paneau est une plaque (et pas une coque
	    write(666,197)   ! pas de calcul car coque
	    write(29, 197)	 !sept06	!bug
		write(9  ,197)
      else

        if((iopti.ge.1).and.(iq.eq.1)) then
          dtpl = -delta                                         ! dmin-d
          tpla = 0.                                             ! d(dmin-d)/d(epsa)
          tplr = 0.                                             ! d(dmin-d)/d(epsr)
          tpld = -1.                                            ! d(dmin-d)/d(delta)

c         parmi delta, epsa et epsr, il y a-t-il bien une variable de conception sélectionnée?
c         si non : stop  impossible d'utiliser cette restriction.
		kxi=nxi2(1,nel)+nxi2(5,nel)+nxi2(9,nel)
          if(kxi.eq.0) then
	      write(*,*)  
	      write(*,*)' stop - error/erreur '
	      write(*,*)' --------------------'
	      write(*,*)' panneau     - panel      :',nel
	      write(*,*)' restriction - constraint : n°4 hughes'
	      write(*,*) ' see explanation in the output file opt-*.*'

	      write(6970,*)  
	      write(6970,*)' stop - error/erreur '
	      write(6970,*)' --------------------'
	      write(6970,*)' panneau     - panel      :',nel
	      write(6970,*)' restriction - constraint : n°4 hughes'
	      write(6970,*) ' see explanation in the output file opt-*.*'


	      write(666,*)  
	      write(666,*)' stop - error/erreur '
	      write(666,*)' --------------------'
	      write(666,*)' panneau     - panel      :',nel
	      write(666,*)' restriction - constraint : n°4 hughes'
	      write(666,*)
		  write(29,*)															!sept06	!bug		
	      write(29,*)' erreur : subr bo2'										!sept06										!bug
	      write(29,*)' panneau :',nel											!sept06											!bug
	      write(29,*)' restriction - constraint : n°4 hughes'					!sept06	!bug
	      write(29,*)' vous avez selectionné la restriction n°4 ',				!sept06!bug
     *         '(dite de hughes) et aucune des variables de conception'			!sept06!bug
	      write(29,*)' suivantes n''a été sélectionnée pour le pan',			!sept06!bug
     *         'neau ',nel														!sept06!bug
	      write(29,*)' - 1: épaisseur du panneau            (delta)'			!sept06!bug
	      write(29,*)' - 5: entredistance entre raidisseurs (epsr)'				!sept06!bug
	      write(29,*)' - 9: entredistance entre cadres      (epsa)'				!sept06!bug
	      write(29,*)' il faut obligatoirement une (ou plusieurs) ',			!sept06!bug
     *         'de ces 3 variables de conception pour que la ',					!sept06!bug
     *         'restriction n°4 soit active.'										!sept06!bug
		  write(29,*)' lbr-5 est arreté. veuillez corriger vos',				!sept06!bug
     *                 ' données'													!sept06
           if(langue.eq.1) then
	      write(666,*)' vous avez selectionné la restriction n°4 ',
     *         '(dite de hughes) et aucune des variables de conception'
	      write(666,*)' suivantes n''a été sélectionnée pour le pan',
     *         'neau ',nel
	      write(666,*)' - 1: épaisseur du panneau            (delta)'
	      write(666,*)' - 5: entredistance entre raidisseurs (epsr)'
	      write(666,*)' - 9: entredistance entre cadres      (epsa)'
	      write(666,*)' il faut obligatoirement une (ou plusieurs) ',
     *         'de ces 3 variables de conception pour que la ',
     *         'restriction n°4 soit active.'
		  write(*,*) ' lbr-5 est arreté. veuillez corriger vos',
     *                 ' données'

		  write(6970,*) ' lbr-5 est arreté. veuillez corriger vos',
     *                 ' données'

		  write(666,*)' lbr-5 est arreté. veuillez corriger vos',
     *                 ' données'
           else
	      write(666,*)' constraint n°4 ("hughes") is selected and',
     *         '  none of the following design variables were selected'
	      write(666,*)' for the panel n°',nel
	      write(666,*)' - 1: panel thickness      (delta)'
	      write(666,*)' - 5: longitudinal spacing (epsr)'
	      write(666,*)' - 9: frame spacing        (epsa)'
	      write(666,*)' at least one of these 3 design variables',
     *         'must be selected to activate constraint n°4'
		  write(666,*)' lbr-5 is stopped. please correct your data'
		  write(*,*)  ' lbr-5 is stopped. please correct your data'
		  write(6970,*)  ' lbr-5 is stopped. please correct your data'
           endif
           pause 'stop'
	     stop
          endif  ! if(kxi.ne.0)
	  endif    ! if((iopti.ge.1).etc.

	  istop=0  ! ré-initialisation
c	if ((is.eq.3) .and. (nel.eq.1) .and.(itera.eq.0)) pause
      call hughes(iprint,chamax(1,is),ploc(nel),e1,eta1,sigy1,sigm1,
     *            width,phil,q,mt,hya,epsa,hxr,epsr,entr,delta,
     *            eff,tpl,tpla,tplr,tpld,
     *            nxi2(1,nel),nxi2(5,nel),nxi2(9,nel),istop,nel,iq,
     *            itera,is,effcomb,sigmag(nel))

   
        if((iopti.ge.1).and.(iq.eq.1)) then
           if(istop.eq.0) then  ! ok convergence dans hughes
                 dtpl=tpl-delta                              ! dmin-d
           else                 ! pas de convergence dans hughes                                        !
                 tpl=1.2*delta                               ! posons dmin=1.2*delta
                 dtpl=tpl-delta                              ! dmin-d
                 tpla = 0.001                                ! d(dmin-d)/d(epsa)
                 tplr = 0.01                                 ! d(dmin-d)/d(epsr)
                 tpld = -1.3                                 ! d(dmin-d)/d(delta)
                 if(iprint.eq.1) then
			      write(*,  198)nel,tpl,tpla,tplr,tpld
			      write(6970,  198)nel,tpl,tpla,tplr,tpld
		          write(666,198)nel,tpl,tpla,tplr,tpld
	           endif
		 endif     
	  endif    ! if((iopti.ge.1).etc.

	if(tpl.ne.0.) ratio=tpl/delta   !dad
 
      endif  ! test plaque-coque
      else													!12.05.04
		if(iopti.ge.1) then						!+++ 26.07.05
		write(666,*) 'panneau n0 =',nel
		write(666,*)'--------------------'
		write(666,*) nel,' est une épontille'					!12.05.04
		endif									!+++ 26.07.05					
	endif

c***********************************************************************
c***********************************************************************
c 3.bis  calcul des contraintes critiques de flambement (euler et/ou johnson)	!dcn07
c        --------------------------------------------------------------------
	if(iopti.ge.1) then
	  
        if((nel.eq.1).and.(itera.ge.1)) write(666,563)		

  563   format('panel        elast. buck. stress          '
     *   'crit. buck. stress'/
     *   '        column     torsion     web'/
     *   '                     se                      sc       sa/sc'/
     *   '                   (n/mm2)                 (n/mm2)    (<1?)'/)

	  ibuck_07=0
	  ibuck_08=0
	  if(ityp1.ne.5) then																					
	    do i=1,m1tabl1(nel)
             if(lcont(1,im+i).eq.14) ibuck_07=1								! flambt tôle = restrict nø14
	       if(lcont(1,im+i).eq.37) ibuck_07=2								! flambt raidisseur = restrict nø37
	       if(lcont(1,im+i).eq.39) ibuck_08=1	    						! flambt raidisseur = restrict nø38


	       if(ibuck_07.ne.0) then
	          call buckdcn07(nel,e1,sigy1,delta,hxr,dxr,wxr,txr,epsr,
     *				 epsa,wya,q,eff,itera,ibuck_07,is)					!juil07
	          nbuck=1
	       endif

  	       if(ibuck_08.ne.0) then
	          call buckdcn08(nel,e1,sigy1,delta,hxr,dxr,wxr,txr,epsr,
     *				 epsa,wya,q,eff,itera,ibuck_08,is)					!juil07

	          nbuck=1
	       endif

          enddo


  	  endif																		
	  

	endif																	!dcn07


c***********************************************************************
c***********************************************************************
c 4. impression des resultats 
c    --------------------------
c 4.1 dans fichier sol2 - résultats de stiff (flexion locale raidisseurs)
c
c12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
c       laterale   <-bending stress (for m=pl^2/10)->  stiff. deflection   <-- plate deflections ->
cpanel  pressure    sx(plate)  sx(flange)   tau(web)   (simply supported)  (clamped)  (simply supp)  
c         (m)        (n/mm2)    (n/mm2)     (n/mm2)         (mm)               (mm)       (mm)
c  1    -25.22      -2250.20    -2250.20    -2250.20       -120.23            -120.23    -120.23 

      if(impr2.ge.-1) then				!sept06
	if(nel.eq.neto) then
        if(langue==1 ) then ! french
         write(67,'(/a/60(1h-))')
     *              ' flexion locale raid. & déformation de la maille'
	  else                ! english
           write(67,'(/a/60(1h-))') 
     *              ' local stiffener bending & plate deflection'
	  endif

	  write(67,'(2a,10x,a)')  '       laterale   ',						
     *   '<-  stiffener bending stress (for m=pl^2/10)',					
     *   '->  stiff. deflection   <-- plate deflections ->'				
	  write(67,'(2a)')'panel  pressure     sx(plate)  sx(flange)   ',	
     *   'sx(jab)    sx(jas)    tau(web)  (simply supported)  (clamped)   
     *  (simply supp)  sigma(pltben)'                                     !avril2003
	  write(67,'(2a)')'         (m)       (n/mm2)     (n/mm2)     ',    
     *           '(n/mm2)     (n/mm2)     (n/mm2)        (mm)             !avril2003
     * (mm)           (mm)        (n/mm²)'                                !avril2003
        do i=1,neto
          write(67,'(i3,3x,6(f8.2,4x),3x,f6.2,9x,3(f6.2,9x))')            !avril2003
     *      i,plat(i)/9810.,sig1(i)/1.e06,sig2(i)/1.e06,sig3(i)/1.e06,    !avril2003
     *      sig4(i)/1.e06,tau(i)/1.e06,1000.*fl(i),                       !avril2003
     *      1000.*wpl1(i),1000.*wpl2(i),sigmag(i)/1.e06                   !avril2003
	  enddo
      endif

	endif				!sept06
c 4.2 dans fichier sol - résultats de result (déplacements et contraintes)
      if(ityp1.eq.5) then												!février 2004
	 
      if(impr2.eq.-3) then										!03.03.04
        call ecri2(dis,nel,eff,conc,ne,indaig,indrai,ns,q,mt,
     *             uecr,langue)                                         	      !03.03.04
      else													!03.03.04
	  call ecri4(dis,nel,eff,conc,ne,indaig,indrai,q,ns,impr2,	                  !février 2004
     *		 isect1,vnymax,vmymax,symax,symin,vm1,vm2,
     *             mt,uecr,langue)		                                          !février 2004
	endif													!03.03.04
	else															!février 2004

      if(impr2.eq.-3) then
        call ecri2(dis,nel,eff,conc,ne,indaig,indrai,ns,q,mt,
     *             uecr,langue)
      else
        call ecri(dis,nel,eff,effcomb,conc,ne,indaig,indrai,ns,q,impr2,   !avril2003
     *            vmaxsig(nel),vmaxsigc(nel),mt,uecr,langue)             !avril2003

        if((nel.eq.neto).and.(impr2.ge.-1)) then		                   !sept06
	    write(67,*)                                                    !avril2003
	    write(67,*)'détail des contraintes utilisées dans la restricti
     *on resul+stiff+pltben'
          write(67,*)'--------------------------------------------------
     *---------------------'                                   
	    write(67,'(3a)') 
     *'panel    sx(resul+stiff)    sy(resul)      ',                     !fev04
     *'  sx(plaque avec phi)  sy(plaque avec phi)  ',                    !fev04
     *'phi   sigvonmises sigvonmises  ind2'                              !fev04
	    write(67,'(3a)') '            (n/mm²)           (n/mm²)  ',    !fev04
     *'         (n/mm²)              (n/mm²)',                           !fev04
     *'                  traction   compression'
	    do i=1,neto                                                    !fev04
		  iii=(i-1)*4+1                                                !fev04
		  if (indplaque(iii).ne.0) then                                !fev04           
      write(67,'(i3,8x,2(f8.2,9x),f8.2,14x,f8.2,11x,f5.2,1x,2(f8.2,5x),
     *i3)') i,                                                           !fev04
     *sigmx(iii)/1.e06,sigmy(iii)/1.e06,sigxplaque(iii)/1.e06,           !fev04
     *sigyplaque(iii)/1.e06,phiplaque(iii),sigvmtplaque(iii)/1.e06,      !fev04
     *sigvmcplaque(iii)/1.e06,indplaque(iii)                             !fev04
	      endif                                                        !fev04

	      do j=2,4                                                     !fev04
              iii=(i-1)*4+j                                              !fev04
	        if (indplaque(iii).ne.0) then                              !fev04
      write(67,'(11x,2(f8.2,9x),f8.2,14x,f8.2,11x,f5.2,1x,2(f8.2,5x),
     *i3)')                                                              !fev04
     *sigmx(iii)/1.e06,sigmy(iii)/1.e06,sigxplaque(iii)/1.e06,           !fev04
     *sigyplaque(iii)/1.e06,phiplaque(iii),sigvmtplaque(iii)/1.e06,      !fev04
     *sigvmcplaque(iii)/1.e06,indplaque(iii)                             !fev04
	        endif                                                      !fev04

	      enddo
	    enddo
		write(67,*)'résultats de différentes contraintes maximales'
	    write(67,*)'----------------------------------------------'
	    write(67,'(a)') 
     *       'panel    sx max(resul)      sx max(resul+stiff)'
	      write(67,'(a)')
     *      '            (n/mm²)              (n/mm²)'
		do i=1,neto
	      write(67,'(i3,8x,2(f8.2,13x))') i,vmaxsig(i)/1.e06,
     *                                     vmaxsigc(i)/1.e06
		enddo
	   endif
	   	  
	  if (impr2.ge.-1) then		!sept006
c       impression des contraintes dans le bordage au niveau des faces du borde
c       z=+delta/2 et z=-delta/2.
	  call ecri3(dis,nel,ne,ns,q,eff2,mt,uecr,langue)
	endif				!impr 0

      endif				!impr-3 et else
      endif				!épon ou plaque					! février 2004

c***********************************************************************
c***********************************************************************
c 5. calcul des restrictions c<cmax (opti)
c     ------------------------------------

      if(iopti.ge.1) then
      call contr(nel,iprint,im,img,itera,is,
     *           e1,eta1,width,sigy1,effcomb,eff,
     *           dtpl,tpld,tpla,tplr,lamb,q,delta,phil,
     *           dya,tya,wya,hya,epsa,ksa,hxr,dxr,wxr,txr,epsr,
     *           entr,ksr,
     *           sxm,sxm(iptmax+2*neto+1),sxm(iptmax+2*neto+10),
     *           sxm(iptmax+2*neto+11),
     *           m1tabl1,
     *           sig1(nel),sig2(nel),sig3(nel),sig4(nel),tau(nel),flr,
     *           wpl1(nel),wpl2(nel),
     *           vs(1),vs(10),vs(19),vs(28),vs(37),vs(46),vs(55),vs(64),
     *           sigplaque,dsigplaque,ratio)   !dad
	endif


c*********************************************************************************
c23456789012345678901234567890123456789012345678901234567890123456789012
c*********************************************************************************
c 6.  ultimate strength of hull girder
c     ------------------------------------
      if(is.eq.nsol.and.iult.gt.0) then
	
      if(iult.eq.1) then

c       methode de paik/caldwell : iult=1
c       ---------------------------------
c       calcul de la section transversale du panneau  (sur base de l'épaisseur moyenne)

	  sect=delt(nel)*q*phil*pi/180.
        if(nel.eq.1)then
         do i=1,9
          do j=1,neto
           dsud(i,j)=0.d00
           dsub(i,j)=0.d00
           dsub1(i,j)=0.d00
           dsusu(i,j)=0.d00
           dsusl(i,j)=0.d00
           dh(i,j)=0.d00
           dg(i,j)=0.d00
           dad(i,j)=0.d00
           dab(i,j)=0.d00
           dab1(i,j)=0.d00
           das(i,j)=0.d00
           dult(i*j)=0.d00
          enddo
         enddo
        endif
        call ushulls(nel,sect,sigy1,e1,q,delta,phil,									
     *               dya,tya,wya,hya,epsa,ksa,
     *               hxr,dxr,wxr,txr,epsr,entr,ksr)
	else
        write(66,'(//a/)')'this method (ult. strength) is not defined'
	endif
	endif
c***************************************************************** 
c.6  vérification au flambement de l'épontille			!nouvelle sousroutine !février 2004
c    -----------------------------------------					 

	if(ityp1.eq.5) then											 
	if(vnymax.le.1000) goto 558		!pas de compression			 
	call flamb(heff,aire,aix,sigy1,hya,isect1,				 
     *	     tya,wya,e1,vnymax,vmymax,symax,symin,			 
     *	     vm1,vm2,epsa)
 558	continue
	endif														 
	

c***********************************************************************
c***********************************************************************
c 7.  les formats
c     ------------
   73 format(' panneau-panel no ',i2,/20(1h+) )
   74 format(/' terme numero ',i2,' de la serie de fourier')
   72 format(/' inconnues hyperstatiques correspondant'/' aux conditio',
     *'ns aux limites de la plaque reelle'/1x,44(1h+)/' elles sont ',
     *'ordonnees dans l''ordre c d a b'//4(2x,e14.7)/4(2x,e14.7)/)
  182 format(a4,1x,5(1x,e13.6))
  197 format(/'!!! le calcul de l''épaisseur minimale (hughes)',
     *       ' n''est pas valable pour une coque cylindrique !!!'/
     *       ' le calcul ne sera donc pas fait .'/
     *       ' veuillez vérifier la restriction manuellement'/)
  198 format(' nel=',i2,': d min =',e13.6,' d tpl/epsa =',e11.4,
     *              ' d tpl/epsr =',e11.4,' d tpl/delta=',e11.4,
     *              ' !! valeurs par défaut !!')
  641 format(t2,i2,5e14.7)
  782 format(/'pente dw/dx (c.à.d. la rotation autour axe oy)'/)

      return
      end

