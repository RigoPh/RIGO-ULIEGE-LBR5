      subroutine contr(nel,iprint,im,img,itera,is,
     *                 e1,eta1,width,sigy1,effcomb,eff,
     *                 dtpl,tpld,tpla,tplr,lamb,q,delta,phil,
     *                 dya,tya,wya,hya,epsa,ksa,hxr,dxr,wxr,txr,epsr,
     *                 entr,ksr,
     *                 sxm,dspaik,spaik,omt,
     *                 m1tabl1,
     *                 sig1,sig2,sig3,sig4,tau,flr,
     *                 wpl1,wpl2,
     *                 dsig1,dsig2,dsig3,dsig4,dtau,dflr,dwpl1,dwpl2,
     *                 sigplaque,dsigplaque,ratio)     !dad
      use sharedvar
      implicit real*8(a-h,o-z)
      real*8 lamb,nx(51,5)
      character *30 vnom2    !dad
      dimension v(51,5),u(51,5),w(51,5),sbord(51,5),sajas(51,5),
     *   sajab(51,5),sasem(51,5),srjas(51,5),srjab(51,5),srsem(51,5),
     *   sbord1(51,5),sbord2(51,5),
     *   stjas(10,5),stjab(10,5),stsem(10,5),
     *   dv(51),effcomb(9690),eff(9690)          !avril2003
      dimension dc(9*neto),sxm(iptmax),dspaik(9)
      dimension dsig1(9),dsig2(9),dsig3(9),dsig4(9),dtau(9),dflr(9),   !avril2003
     *          dwpl1(9),dwpl2(9)                                      ! vecteur de travail
      dimension m1tabl1(neto)
	dimension sigplaque(iptmax),dsigplaque(iptmax,9)    !avril2003

c **********************************************************************
c     subroutine contraintes (c.à.d) "restrictions"
c     ==============================================
c     calcul des m restrictions. c(j) du pan. nel , (j=1+im,m+im)  (max 7000 pour la struct.)
c     im = nbre de restrictions déjà considérées par les panneaux 1 à nel-1.
c
c     calcul des nbrxi dérivées des restrictions dc(i,j), i=1,nbrxi [nbrxi=nvar(nel)]
c     ipts = nbre de points de calcul des restrictions (par panneau) (max 10)
c     ypts = ordonnées de ces ipts points de calcul des restrictions (par panneau)
c	lcont(1,j+im) = nø de référence de la restriction j du panneau nel
c                     ex: 11 pour sigma comp bordage
c	lcont(2,j+im) = nø du pt d'applic. de la restr. j du panneau nel (sensibilité)
c                   = 1 à 10
c	ipt2(i,nel) = nø des ipt pts d'applic. pour le panneau nel (1 à 31)
c	ipt2(lcont(2,j+im),nel) = nø du pt d'applic. de la restr. j du panneau nel (1 à 31).
c	ipt3(lcont(2,j+im),nel) = nø de la trav. correspondant au pt ipt (pour le panneau nel)
c
c     créer   :  29-04-94   (dr. ph. rigo)
c
c     modifier:  15-05-96   restriction nø 4  (dmin - d < 0)
c                 5- 6-96   restrictions géométriques
c                 4- 7-96   restrictions nø5 (w relatif) et subr. sens3
c                22- 7-96   restrictions nø15 sx(uls) de paik et subr. sens4
c                 6- 3-97   prise en compte des différences entre d(raid) et de epsr	       
c                16-11-00   restrictions 51 à 56: flexion locale raid.	       
c                 4-02-02   restrictions struct par cas de charge	    
c                  -07-03   ajouts de restrictions combinées, changement du numéro de 
c                           quelques restrictions   
c
c    derniere modification : juillet 2003
c                
c ******************************************************************************

c   les variables de conception  ; nbrxi = nvar(nel)
c   -------------------------------------------------
c       1       delta =  épaisseur du bordage
c       2       hya   =  hauteur de l'âme des cadres   
c       3       dya   =  épaisseur de l'âme des cadres   
c       4       wya   =  largeur des semelles des cadres   
c       5       epsa  =  entredistance entre cadres   
c       6       hxr   =  hauteur de l'âme des raidisseurs
c       7       dxr   =  épaisseur de l'âme des raidisseurs
c       8       wxr   =  largeur des semelles des raidisseurs
c       9       epsr  =  entredistance entre raidisseurs

c  restrictions:
c  -------------
c  ic(j) = le numéro de référence des m restrictions struct. (j=1,m) du panneau en question. 

c  [ix(j),iy(j)] coordonnées (x,y) du point où la restriction est appliquée.
c    - ix est choisi par défaut (mi-portée ou appuis selon la fct sinus ou cosinus).
c    - iy est une donnée.
c       iy1 = nø du pt d'applic. de la restriction =1à4  (pts d'applic. des restrictions)
c       iy2 = nø du pt d'applic. de la restriction =1à31 (pts de calcul des déplac. et contr.)

c  dérivées :sens4(j,i,k,ipan) valeurs cumulees.
c        avec      j=1,9   les fonctions (ex: pour w, j=1)
c				   w,wø,wøø,wøøø,u,uø,v,vø,vøø
c		           1 2  3   4    5 6  7 8  9 
c  			 i=1,ipts les points de calcul (correspondant aux pts de calcul des restrictions et des sensibilites)
c  			 k=1,9    les variables de conception (si ep. bord., k=1)
c              ipan=1,neto le panneau relatif à ces k variables de conception

c ****************************************************************
c 1.  restrictions structurelles  (boucle 1)
c ****************************************************************
c       nø de référence de la restriction = ic=lcont(1,icont)
c   ----------------------------------------------------------
c     nø 1     v déplacement (en m)
c     nø 2     u déplacement (en m)
c     nø 3     w déplacement (en m)

c	nø 4	 epaisseur minimale du bordé (en m): dmin - d < 0 [restriction dite de "hughes"] 

c	nø 5	 w(relatif) dépl. relatif par rapport au bord de départ (y=0)  du panneau (en m)
c	nø 6	 w(relatif) dépl. relatif par rapport au bord d'arrivée (y=yo) du panneau (en m)
c	nø 7	 w(relatif) dépl. relatif par rapport au dépl. moyen des extrémités du panneau (en m)

c	nø 11	 sigma comp. (sx,sy) bordage (z=0)      en x=l/2 (en n/m2)  sens2(1,..
c	nø 12	 sigma comp. (sx,sy) bordage (z=0)      en x=l/2 (en n/m2)  sens2(1,.. avec flex.loc.des raid.  !avril2003
c	nø 13	 sigma comp. (sx,sy) bordage (z=0)      en x=l/2 (en n/m2)  sens2(1,.. idem + effet plaque      !avril2003

c	nø 15	 sx < s(ult. strength)  faxial(moyen)   en x=l/2  (cfr subr paik)

c	nø 16	 sigma comp. (txy)   bordage (z=0)      en x=0			sens2(2,..       !avril2003
c	nø 17	 sigma comp. (sx,sy) bordage (z=+d/2)   en x=l/2		sens2(4,..       !avril2003
c	nø 18	 sigma comp. (sx,sy) bordage (z=-d/2)   en x=l/2		sens2(3,..       !avril2003

c	nø 21	 sigma comp. (tâme)   cadre      jas/jab en x=0  (en n/m2)	sens2(5,..
c	nø 22	 sigma comp. (sy,txy) cadre      jas     en x=l/2		sens2(6,..
c	nø 23	 idem cond. nø21 car tau(jas)=tau(jab)				    sens2(7,..
c	nø 24	 sigma comp. (sy,txy) cadre      jab     en x=l/2		sens2(8,..
c	nø 25	 sigma comp. (sy,txy) cadre      semelle en x=l/2		sens2(9,..

c	nø 31	 sigma comp. (tâme)   raidisseur jab     en x=0			sens2(11,.. !avril2003
c	nø 32	 sigma comp. (sx)     raidisseur semelle en x=l/2		sens2(12,.. !avril2003

c     nø 33    sigma comp. (tâme)   raidisseur jab     en x=0			sens2(11,.. avec flex.loc.des raid.  !avril2003
c	nø 34	 sigma comp. (sx)     raidisseur semelle en x=l/2		sens2(12,.. avec flex.loc.des raid.  !avril2003
c     nø 35    sigma comp. (sx,tâme) raidisseur jab    en x=l/2		sens2(10,.. avec flex.loc.des raid.  !avril2003
c     nø 36    sigma comp. (sx,tâme) raidisseur jas    en x=l/2		sens2(16,.. avec flex.loc.des raid.  !avril2003

c	nø 41	 sigma comp. (tâme)   traverse  jas      en x=0  (en n/m2)	sens2(13,..
c	nø 42	 sigma comp. (tâme)   traverse  jab      en x=0			sens2(14,..
c	nø 43	 sigma comp. (sx)     traverse  semelle  en x=l/2		sens2(15,..

c     flexion locale du raidisseur:
c	nø 51	 flèche relative raid: extr. encastrées : f/l=1*pl^3/384ei < ratio (1/200, ...
c	nø 52	 flèche relative raid: appuis simples   : f/l=5*pl^3/384ei < ratio (1/200, ..
c	nø 54	 contrainte max semelle:  (m=pl^2/10) ; sig < s(adm)
c	nø 55	 contrainte max borde:    (m=pl^2/10) ; sig < s(adm)
c	nø 56	 contrainte max cisaillement dans âme : 1.73*tau < t(adm);(tau à l'axe neutre et m=0)
c	nø 57	 flèche abs. maille: bords encastrés : w=1*pl^4/384ei < wmax (m)
c	nø 58	 flèche abs. maille: bords articulés : w=5*pl^4/384ei < wmax (m)

c rem: dans les donnees, les restrictions ne doivent pas être classees par ordre  !!!
c **********************************************************************************

      ratiostruc=0.   !dad
c------------------------------------
c vecteur eff -> sous-vecteurs
      do j=1,5
       do i=1,51
        jj=(j-1)*51+i
        v(i,j)=eff(jj)              ! 1er element = 1
        u(i,j)=eff(255+jj)          ! 1er element = 256
        w(i,j)=eff(510+jj)          ! 1er element = 511
        sbord(i,j)=eff(4845+jj)     ! 1er element = 4846
        nx(i,j)=eff(1020+jj)        ! 1er element = 1021
        sajas(i,j)=eff(6120+jj)     ! 1er element = 6121
        sajab(i,j)=eff(6375+jj)     ! 1er element = 6376
        sasem(i,j)=eff(7140+jj)     ! 1er element = 7141
        srjas(i,j)=eff(8925+jj)     ! 1er element = 8926
        srjab(i,j)=eff(9180+jj)     ! 1er element = 9181
        srsem(i,j)=eff(9435+jj)     ! 1er element = 9436
        sbord1(i,j)=eff(765+jj)     ! 1er element = 766
        sbord2(i,j)=eff(1785+jj)    ! 1er element = 1786
       enddo
      enddo
c
c -----------------------------------

	m=m1tabl1(nel)  ! nbre de restrictions du cas de charge is pour le panneau nel
				
      if((itera.eq.0).and.(m.ne.0)) then		!sept06
	  write(666,'(a,i2)') 'panneau nø ', nel
	  write(666,'(a)')    '-----------------'
        write(666,284) m,(ypts(i,nel),i=1,ipts(nel))   ! ypts (ordonnées des pts)
      endif
	
	
c     boucle principale sur les "m" restrictions du panneau "nel"
c     ===========================================================
      do 1 icont=im+1,im+m

        ic =lcont(1,icont)
        iy1=lcont(2,icont)
        iy2=ipts2(iy1,nel)
        it =ipts3(iy1,nel)

        invv=inv(icont)
        cm=cjmax(icont)*invv

	  call annuld(dc,ntot)		!dcn07

      if(ic.eq.1) then
c     restrictions nø 1 :  déformée v à mi-portée (x=l/2 m)
c     -----------------------------------------------------------
c     déformée v(iy,ix) ; iy=iy2 et ix=5 pour x=l/2
        vnom2='dépl. v; l/2'
        ix=5
        c=v(iy2,ix)*invv

c     dérivée de v à mi-portée = sens4(7,iy,kk,ipan) et iy = iy1
        j=7
        call csens(invv,iy1,j,dc)

      else if(ic.eq.2) then
c     restrictions nø 2 :  déformée u aux extr. (x=0 m)
c     -----------------------------------------------------------
c     déformée u(iy,ix); iy=iy2 et ix=1 pour x=0
        vnom2='dépl. u; x=0'
        ix=1
        c=u(iy2,ix)*invv

c     dérivée de u aux extr. = sens4(5,iy,kk,ipan) et iy = iy1
        j=5
      call csens(invv,iy1,j,dc)

      else if(ic.eq.3) then
c     restrictions nø 3 :  déformée w à mi-portée  (x=l/2 m)
c     -----------------------------------------------------------
c     déformée w(iy,ix);  iy=iy2  et ix=5 pour x=l/2
        vnom2='dépl. w; l/2'
        ix=5
        c=w(iy2,ix)*invv

c     dérivée de w à mi-portée = sens4(1,iy,kk,ipan) et iy = iy1
        j=1
        call csens(invv,iy1,j,dc)

      else if(ic.eq.4) then
c     restrictions nø 4 : epaisseur minimale du bordé (hughes) : dmin - d < 0
c     ------------------------------------------------------------------
        vnom2='dmin - d < 0'
        c=0.
        cm=0.
c        call annuld (dc,ntot)		!dcn07

        if(nxi2(1,nel).ne.0)then
c         delta = d est une variable de conception (xi=1)
          c=dtpl
          dc(nxi2(1,nel))=tpld
        endif
        if(nxi2(5,nel).ne.0)then
c         epsa est une variable de conception (xi=5)
          c=dtpl
          dc(nxi2(5,nel))=tpla
        endif
        if(nxi2(9,nel).ne.0)then
c         epsr est une variable de conception (xi=9)
          c=dtpl
          dc(nxi2(9,nel))=tplr
        endif

      else if((ic.ge.5).and.(ic.le.7)) then
c     restrictions nø 5,6,7 :  déformée relative de w   (x=l/2 m)
c     ---------------------------------------------------------------
c     déformée w(iy,ix);  iy=iy2  et ix=5 pour x=l/2
        vnom2='depl w relat'
        ix=5
        if(ic.eq.5) c=w(iy2,ix) - w(1,ix)
        if(ic.eq.6) c=w(iy2,ix) - w(31,ix)
        if(ic.eq.7) c=w(iy2,ix) - (w(1,ix)+w(31,ix))/2.
        c=c*invv

c       dérivée de w à mi-portée = sens4(1,iy,kk,ipan) et iy = iy1
        j=1
        call csens3(invv,iy1,j,ipts(nel),ic,dc)

      else if(ic.eq.11) then
c     restrictions nø 11: contrainte de comp. dans le bordage en z=0 (x=l/2)
c     ----------------------------------------------------------------------
c       contrainte sbord(iy,ix);    iy=iy2  et ix=5 pour x=l/2
        vnom2='sbor z=0;l/2'
        ix=5
        c=sbord(iy2,ix)*invv

c       dérivée de sbord à mi-portée = sens2(1,iy,kk,ipan) et iy = iy1
        j=1
        call csens2(invv,iy1,j,dc)
	continue

      else if(ic.eq.12) then    !avril2003 (pour tout le point 12!!!!!!!!!!!!!!!!!!!!!!!!!!!!)
c     restrictions nø 12: contrainte de comp. dans le bordage en z=0 (x=l/2) 
c     ----------------------------------------------------------------------
c     (avec flexion locale des raidisseurs)
c       contrainte sbord(iy,ix);    iy=iy2  et ix=5 pour x=l/2
        vnom2='sbord+stiff '
	  ix=5
        c = effcomb(4845+4*51+iy2)*invv

      l1=0

	do ipan=1,neto
	  nbrxi=nvar(ipan)
	  ij = 4*51 + ipts2(iy1,ipan)
        do k=1,nbrxi
          kk=nxit(k,ipan)
          l = l1 + k
          if (ipan.eq.nel) then		!!!aout04
	      dc(l) = sens3(1,iy1,kk,ipan)  ! = d(sigmax)/dx
	      if (eff(ij+4335)*sig1.ne.0) then
	        den = eff(ij+4335)*sig1
	        dc(l) = dc(l) + dsig1(kk)*den/dabs(den)
	      else
	        dc(l) = dc(l) + dsig1(kk) 
	      endif
	      den = effcomb(ij+4845)
	      if(dabs(den).le.(+1.e-10)) den = 1.e-10
	        dc(l)=0.5*invv*((2.*effcomb(ij+4335)-effcomb(ij+4080)) 		! d(sigma comp)
     *       *dc(l) + (2.*effcomb(ij+4080) - effcomb(ij+4335))
     *       *sens3(2,iy1,kk,ipan)  )/den
          else									!!!aout04
	      dc(l) = sens2(1,iy1,kk,ipan)*invv	!!!aout04
	    endif									!!!aout04
	  enddo
	  l1 = l1 + nbrxi
	enddo
  300	continue
  	
	else if(ic.eq.13) then    !avril2003 (pour tout le point 13!!!!!!!!!!!!!!!!!!!!!!!!!!!!)
c     restrictions nø 13: contrainte de comp. dans le bordage en z=0 (x=l/2) 
c     ----------------------------------------------------------------------
c     (avec flexion locale des raidisseurs et contrainte plaque)
c       contrainte sbord(iy,ix);    iy=iy2  et ix=5 pour x=l/2
        vnom2='s+stiff+pltb'
        
	  ix=5
        c = sigplaque(iy1)*invv

	l1=0

	do ipan=1,neto
	  nbrxi=nvar(ipan)
	  if (ipan.eq.nel) then
	    do k=1,nbrxi
	      kk=nxit(k,ipan)
	      l = l1 + k
		  dc(l) = dsigplaque(iy1,kk)*invv
	    enddo
	    goto 301
	  endif
	  l1 = l1 + nbrxi
	enddo
  301 continue
		
      else if(ic.eq.14) then                                                     !avril2003

c	restrictions nø 14: flambement du bordage en z=0 (x=l/2)				   !dcn07
c	--------------------------------------------------------
	  vnom2='fl.bord.;l/2'
c	  cm=1.

c	  call annuld(dc,ntot)		!dcn07

	  c=dbuckpl_07
	  
	  l1=0
        do ipan=1,neto
          nbrxi=nvar(ipan)
          do k=1,nbrxi
            kk=nxit(k,ipan)
            l=l1+k
            dc(l)=ddbuckpl_07(kk,ipan)
		enddo
          l1=l1+nbrxi
	  enddo

	else if(ic.eq.16) then 

c     restrictions nø 16: contrainte de comp. dans le bordage en z=0 (x=0)       !avril2003
c     --------------------------------------------------------------------------
c       contrainte sbord(iy,ix);    iy=iy2  et ix=1 pour x=0
        vnom2='tbor z=0 x=0'
	  ix=1
        c=sbord(iy2,ix)*invv

c       dérivée de sbord à mi-portée = sens2(2,iy,kk,ipan) et iy = iy1	pour t bord j=2 
        j=2
        call csens2(invv,iy1,j,dc)

	else if(ic.eq.17) then                                                     !avril2003
c     restrictions nø 17: contrainte de comp. dans le bordage en z=+d/2 (x=l/2)  !avril2003
c     --------------------------------------------------------------------------
c       contrainte sbord1(iy,ix);    iy=iy2  et ix=5 pour x=l/2
        vnom2='sbord z=+d/2'
	  ix=5
        c=sbord1(iy2,ix)*invv

c       dérivée de sbord à mi-portée = sens2(4,iy,kk,ipan) et iy = iy1	pour s bord en z=d/2
        j=4                                                             !   sens2(4,.....
        call csens2(invv,iy1,j,dc)

	else if(ic.eq.18) then                                                      !avril2003
c     restrictions nø 18: contrainte de comp. dans le bordage en z=-d/2 (x=l/2)   !avril2003
c     --------------------------------------------------------------------------
c       contrainte sbord2(iy,ix);    iy=iy2  et ix=5 pour x=l/2
        vnom2='sbord z=-d/2'
	  ix=5
        c=sbord2(iy2,ix)*invv

c       dérivée de sbord à mi-portée = sens2(3,iy,kk,ipan) et iy = iy1	pour s bord en z=-d/2 
        j=3                                                             ! sens2(3,.....
        call csens2(invv,iy1,j,dc)

	else if(ic.eq.15) then
c     resistance ultime of longitudinally compresses stiffened panels
c     - cfr article de paik dans ship research).
c	- restriction nø 15  sx < s(ult. strength)  ; faxial moyen  en x=l/2 
c     ---------------------------------------------------------------------
	
c       cas d'une coque cylindrique	   
        if(phil.ge.1.) then     ! coque/plaque
	    write(666,196)
	    write(9  ,196)
		write(29 ,196)		!sept06
	  endif										

        vnom2='sx/s(uls)<sc'
        cm=dabs(cm)         ! borne; c(x) < cm (=cmax)

c	  call annuld(dc,9*neto)	!dcn07

c     sxm    = valeur  du sx axial moyen (calcule dans subr resul)
c     d(sxm)/dx = sens3(21,iptmax,9,neto)
c     spaik  = résistance ultime selon la formule de j. paik
c     dspaik = dérivée de la résistance ultime spaik par rapport aux xi

        call paik(iprint,nel,e1,sigy1,width,lamb,q,delta,
     *            phil,epsa,hxr,dxr,wxr,txr,epsr,entr,ksr,
     *            dspaik,spaik,omt)

        smoyen=sxm(iy1)
        c=-smoyen/spaik

c     dérivée de "-smoyen/spaik" à mi-portée
c     .......................................
c      avec spaik > 0  et nx < 0 en compression.
c      si nx>0 la restriction n'a plus de sens et est automatiquement vérifiée.

        call csens4(iy1,nel,dspaik,smoyen,spaik,dc)

      else if(ic.eq.21) then
c     restrictions nø 21: contrainte de comp. cadre jas aux appuis (x=0)
c     ------------------------------------------------------------------
c       contrainte sajas(iy,ix);    iy=iy2  et ix=1 pour x=0
        vnom2='tcad jas x=0'
        ix=1
        c=sajas(iy2,ix)*invv

c       dérivée de sbord à mi-portée = sens2(5,iy,kk,ipan) et iy = iy1
        j=5
        call csens2(invv,iy1,j,dc)

      else if(ic.eq.22) then
c     restrictions nø 22: contrainte de comp. cadre jas à mi-portée (x=l/2)
c     --------------------------------------------------------------------------
c       contrainte sajas(iy,ix);    iy=iy2  et ix=5 pour x=0
        vnom2='scad jas l/2'
	  ix=5
        c=sajas(iy2,ix)*invv

c       dérivée de sbord à mi-portée = sens2(6,iy,kk,ipan) et iy = iy1	pour sig cadre jas
        j=6                 ! sens2(6,.....
        call csens2(invv,iy1,j,dc)

	else if(ic.eq.23) then
c     restrictions nø 23: contrainte de comp. cadre jab aux appuis (x=0)
c     --------------------------------------------------------------------------
c       contrainte sajab(iy,ix);    iy=iy2  et ix=1 pour x=0
        vnom2='tcad jab x=0'
	  ix=1
        c=sajab(iy2,ix)*invv

c       dérivée de sbord à mi-portée = sens2(7,iy,kk,ipan) et iy = iy1	pour sig cadre jab
        j=7  ! sens2(7,.....
        call csens2(invv,iy1,j,dc)

	else if(ic.eq.24) then
c     restrictions nø 24: contrainte de comp. cadre jab à mi-portée (x=l/2)
c     --------------------------------------------------------------------------
c       contrainte sajab(iy,ix);    iy=iy2  et ix=5 pour x=0
        vnom2='scad jab l/2'
	  ix=5
        c=sajab(iy2,ix)*invv

c       dérivée de sbord à mi-portée = sens2(8,iy,kk,ipan) et iy = iy1	pour sig cadre jab
        j=8   ! sens2(8,.....
        call csens2(invv,iy1,j,dc)

	else if(ic.eq.25) then
c     restrictions nø 25: contrainte de comp. cadre sem  (x=l/2)
c     --------------------------------------------------------------------------
c       contrainte sasem(iy,ix);    iy=iy2  et ix=5 pour x=l/2
        vnom2='scad sem l/2'
	  ix=5
        c=sasem(iy2,ix)*invv

c       dérivée de sbord à mi-portée = sens2(9,iy,kk,ipan) et iy = iy1	pour sig cadre sem
        j=9 ! sens2(9,.....
        call csens2(invv,iy1,j,dc)

	else if(ic.eq.31) then                                               !avril2003
c     restrictions nø 31: contrainte de comp. raid jab aux appuis (x=0)    !avril2003
c     -----------------------------------------------------------------
c       contrainte srjab(iy,ix);    iy=iy2  et ix=1 pour x=0
        vnom2='trai jab x=0'
	  ix=1
        c=srjab(iy2,ix)*invv

c       dérivée de sbord à mi-portée = sens2(11,iy,kk,ipan) et iy = iy1	pour sig raid jab
        j=11  ! sens2(11,.....
        call csens2(invv,iy1,j,dc)

	else if(ic.eq.32) then                                         !avril2003
c     restrictions nø 32: contrainte de comp. raid sem  (x=l/2)      !avril2003
c     ---------------------------------------------------------
c       contrainte srsem(iy,ix);    iy=iy2  et ix=5 pour x=l/2
        vnom2='srai sem l/2'
	  ix=5
        c=srsem(iy2,ix)*invv

c       dérivée de sbord à mi-portée = sens2(12,iy,kk,ipan) et iy = iy1	pour sig raid sem
        j=12  !  sens2(12,.....
        call csens2(invv,iy1,j,dc)

      else if(ic.eq.33) then            !avril2003 (pour tout le point 33!!!!!!!!!!!!!!!!!!!!!!!!!!!!)
c     restrictions nø 33: contrainte de comp. raid jab aux appuis (x=0) avec flexion locale des raidisseurs
c     -----------------------------------------------------------------------------------------------------
c       contrainte srjab(iy,ix);    iy=iy2  et ix=1 pour x=0
        vnom2='tjabx=0+stiff'
	  ix=1
	  c = effcomb(9180+iy2)*invv

      l1=0
	do ipan=1,neto
	  nbrxi=nvar(ipan)
	  ij = ipts2(iy1,ipan)
	    do k=1,nbrxi
	      kk=nxit(k,ipan)
	      l = l1 + k
		  if (ipan.eq.nel) then	!!!aout04  
	        den = effcomb(ij+8415)                                     ! pour tau jab
	        if(dabs(den).le.(+1.e-10)) den = 1.e-10
              dc(l) = (sens2(11,iy1,kk,ipan) 
     *                      + 1.732051*dtau(kk)*den/dabs(den))*invv
		  else									!!!aout04
	        dc(l) = sens2(11,iy1,kk,ipan)*invv	!!!aout04
	      endif									!!!aout04
	    enddo
	  l1 = l1 + nbrxi
	enddo
  302	continue

      else if(ic.eq.34) then               !avril2003 (pour tout le point 34!!!!!!!!!!!!!!!!!!!!!!!!!!!!)
c     restrictions nø 34: contrainte de comp. raid sem  (x=l/2) avec flexion locale des raidisseurs
c     ---------------------------------------------------------------------------------------------
c       contrainte srsem(iy,ix);    iy=iy2  et ix=5 pour x=l/2
        vnom2='seml/2+stiff'
	  ix=5
	  c = effcomb(9435+4*51+iy2)*invv

      l1=0
	do ipan=1,neto
	  nbrxi=nvar(ipan)
	  ij=4*51 + ipts2(iy1,ipan)
	    do k=1,nbrxi
	      kk=nxit(k,ipan)
	      l = l1 + k
		  if (ipan.eq.nel) then		!!!aout04
	        den = eff(ij+7905)  ! sig x sem raid (de resul)
	        if(dabs(den).le.(+1.e-10)) den = 1.e-10
	        dc(l) = sens2(12,iy1,kk,ipan)*dabs(den)/den
	        if (eff(ij+7905)*sig2.ne.0) then
 	          den = eff(ij+7905)*sig2
	          dc(l) = dc(l) + dsig2(kk)*den/dabs(den) ! sigmax sem
	        else
                dc(l) = dc(l) + dsig2(kk)
	        endif
	        den = effcomb(ij+7905)
              if(dabs(den).le.(+1.e-10)) den = 1.e-10
              dc(l) = dc(l)*den/dabs(den)*invv
	      else									!!!aout04
	        dc(l)= sens2(12,iy1,kk,ipan)*invv	!!!aout04
	      endif									!!!aout04
	    enddo
        l1 = l1 + nbrxi
	enddo
  303	continue

      else if(ic.eq.35) then               !avril2003 (pour tout le point 35!!!!!!!!!!!!!!!!!!!!!!!!!!!!)
c     restrictions nø 35: contrainte de comp. raid jab  (x=l/2) avec flexion locale des raidisseurs
c     ---------------------------------------------------------------------------------------------
c       contrainte srjab(iy,ix);    iy=iy2  et ix=5 pour x=l/2
        vnom2='jabl/2+stiff'
	  ix=5
        c = effcomb(9180+4*51+iy2)*invv

      l1=0
	do ipan=1,neto
	  nbrxi=nvar(ipan)
	  ij=4*51 + ipts2(iy1,ipan)
	    do k=1,nbrxi
	      kk=nxit(k,ipan)
	      l = l1 + k
		  if (ipan.eq.nel) then	!!!aout04
			dsig3(kk) = dsig3(kk)*10./12.             ! pour avoir pl²/12 (et non pl²/10)
			den=eff(ij+7650) ! = sx jab raid
			if(dabs(den).le.(+1.e-10)) den=1.e-10
			dc(l) = sens2(10,iy1,kk,ipan)/den*dabs(den) ! = d(sx jab raid)
			if (eff(ij+7650)*sig3.ne.0) then
		      den = eff(ij+7650)*sig3
			  dc(l) = dc(l) + dsig3(kk)*den/dabs(den)
			else
			  dc(l) = dc(l) + dsig3(kk) 
			endif ! ici, dc(l) = d(sx(resul)+sx(stiff) à jab raid), c'est donc d(effcomb(ij+7650))
			dc(l) = (effcomb(ij+7650)*dc(l)
     *                    + 3.*tau*dtau(kk))/c*invv ! = d(sigma comp jab) 
		  else									!!!aout04
			dc(l) = sens2(10,iy1,kk,ipan)*invv	!!!aout04
	      endif									!!!aout04
	    enddo
	  l1 = l1 + nbrxi
	enddo
  304	continue

      else if(ic.eq.36) then               !avril2003 (pour tout le point 36!!!!!!!!!!!!!!!!!!!!!!!!!!!!)
c     restrictions nø 36: contrainte de comp. raid jas  (x=l/2) avec flexion locale des raidisseurs
c     ---------------------------------------------------------------------------------------------
c       contrainte srjas(iy,ix);    iy=iy2  et ix=5 pour x=l/2
        vnom2='jasl/2+stiff'
	  ix=5
        c = effcomb(8925+4*51+iy2)*invv

      l1=0

	do ipan=1,neto
	  nbrxi=nvar(ipan)
	  ij=4*51 + ipts2(iy1,ipan)
	    do k=1,nbrxi
	      kk=nxit(k,ipan)
		  l = l1 + k
		  if (ipan.eq.nel) then	!!!aout04
	        den=eff(ij+7395) ! = sx jas raid
		    if(dabs(den).le.(+1.e-10)) den=1.e-10
              dc(l)= sens2(16,iy1,kk,ipan)/den*dabs(den)      ! d(sx jas raid)
		    if (eff(ij+7395)*sig4.ne.0) then
		      den = eff(ij+7395)*sig4
		      dc(l) = dc(l) + dsig4(kk)*den/dabs(den)
		    else
		      dc(l) = dc(l) + dsig4(kk)
		    endif
	        den=effcomb(ij+7395) ! = sx jas raid
	        if(dabs(den).le.(+1.e-10)) den=1.e-10
		    dc(l) = dc(l)*den/dabs(den)*invv            ! d(sigma comp jas)
	      else		!!!aout04
	        dc(l)= sens2(16,iy1,kk,ipan)*invv	!!!aout04
		  endif		!!!aout04
	    enddo
	  l1 = l1 + nbrxi
	enddo
  305	continue

	else if(ic.eq.37) then
c	restrictions nø 37: flambement du raidisseur (x=l/2)				!dcn07
c	----------------------------------------------------
	  vnom2='fl.raid.;l/2,dcn_2007'
c	  cm=1.

c	  call annuld(dc,ntot)		!dcn07

	  c=dbuckstif_07
	  
	  l1=0
        do ipan=1,neto
          nbrxi=nvar(ipan)
          do k=1,nbrxi
            kk=nxit(k,ipan)
            l=l1+k
            dc(l)=ddbuckstif_07(kk,ipan)
		enddo
          l1=l1+nbrxi
	  enddo


	else if(ic.eq.39) then
c	restrictions nø 38: flambement du raidisseur (x=l/2)				!dcn07
c	----------------------------------------------------
	  vnom2='fl.raid.;l/2,dcn_2008'
c	  cm=1.

c	  call annuld(dc,ntot)		!dcn07

	  c=dbuckstif_08
	  
	  l1=0
        do ipan=1,neto
          nbrxi=nvar(ipan)
          do k=1,nbrxi
            kk=nxit(k,ipan)
            l=l1+k
            dc(l)=ddbuckstif_08(kk,ipan)
		enddo
          l1=l1+nbrxi
	  enddo


	else if(ic.eq.41) then
      if(it.eq.0) goto 10
c     restrictions nø 41: contrainte de comp. trav jas aux appuis (x=0)
c     --------------------------------------------------------------------------
c       contrainte srjas(iy,ix);    it= nø de trav.  et ix=1 pour x=0
        vnom2='ttra jas x=0'
	  ix=1
        if(it.gt.0) c=stjas(it,ix)*invv

c       dérivée de sbord à mi-portée = sens2(13,iy,kk,ipan) et iy = iy1	pour sig trav jas
        j=13 ! sens2(13,.....
        call csens2(invv,iy1,j,dc)

	else if(ic.eq.42) then
      if(it.eq.0) goto 10
c     restrictions nø 42: contrainte de comp. trav jab aux appuis (x=0)
c     --------------------------------------------------------------------------
c       contrainte srjab(iy,ix);    it= nø de trav.  et ix=1 pour x=0
        vnom2='ttra jab x=0'
	  ix=1
        if(it.gt.0) c=stjab(it,ix)*invv

c       dérivée de sbord à mi-portée = sens2(14,iy,kk,ipan) et iy = iy1	pour sig trav jab
        j=14 ! sens2(14,.....
        call csens2(invv,iy1,j,dc)

	else if(ic.eq.43) then
      if(it.eq.0) goto 10
c     restrictions nø 43: contrainte de comp. trav sem  (x=l/2)
c     --------------------------------------------------------------------------
c       contrainte srsem(iy,ix);    it= nø de trav.  et ix=5 pour x=l/2
        vnom2='stra sem l/2'
	  ix=5
        if(it.gt.0) c=stsem(it,ix)*invv

c       dérivée de sbord à mi-portée = sens2(15,iy,kk,ipan) et iy = iy1	pour sig trav sem
        j=15  ! sens2(15,.....
        call csens2(invv,iy1,j,dc)

	else if(ic.eq.51) then
c     restrictions nø 51: fleche rel. raid (extr. encastrees): f/l=1*pl^3/384ei < ratio (1/200, ...
c     ----------------------------------------------------------------------------------------------
        vnom2='raid loc fl1'
        c=flr/5.
c	  call annuld(dc,9*neto)		!dcn07
        l1=0
        do ipan=1,neto
          nbrxi=nvar(ipan)
          if(ipan.eq.nel) then
            do k=1,nbrxi
               kk=nxit(k,nel)
               l=l1+k
               dc(l)=dflr(kk) /5.  
            enddo
	      goto 308
	    endif
          l1=l1+nbrxi
	  enddo
  308   continue

	else if(ic.eq.52) then
c     restrictions nø 52:  fleche rel. raid (extr. appuyées): f/l=5*pl^3/384ei < ratio (1/200, ...
c     ----------------------------------------------------------------------------------------------
        vnom2='raid loc fl5'
        c=flr
c	  call annuld(dc,9*neto)	!dcn07
        l1=0
        do ipan=1,neto
          nbrxi=nvar(ipan)
          if(ipan.eq.nel) then
            do k=1,nbrxi
               kk=nxit(k,nel)
               l=l1+k
               dc(l)=dflr(kk)   
            enddo
	      goto 309
	    endif
          l1=l1+nbrxi
	  enddo
  309   continue

	else if(ic.eq.54) then
c     restrictions nø 54: flexion locale raidisseur: contr dans semelle
c     ------------------------------------------------------------------
        vnom2='raid loc sem'   ! (m=pl^2/10) ; sig < s(adm)
	  invv=1
	  if(sig2.le.0.0) invv=-1
        c=sig2*invv
c	  call annuld(dc,9*neto)	!dcn07
        l1=0
        do ipan=1,neto
          nbrxi=nvar(ipan)
          if(ipan.eq.nel) then
            do k=1,nbrxi
               kk=nxit(k,nel)
               l=l1+k
               dc(l)=dsig2(kk) * invv  
            enddo
	      goto 310
	    endif
          l1=l1+nbrxi
	  enddo
  310   continue
        invv=1

	else if(ic.eq.55) then
c     restrictions nø 55: flexion locale raidisseur: contr dans bordé
c     --------------------------------------------------------------------
        vnom2='raid loc bor'   ! (m=pl^2/10) ; sig < s(adm)
	  invv=1
	  if(sig1.le.0.0) invv=-1
        c=sig1*invv
c	  call annuld(dc,9*neto)	!dcn07
        l1=0
        do ipan=1,neto
          nbrxi=nvar(ipan)
          if(ipan.eq.nel) then
            do k=1,nbrxi
               kk=nxit(k,nel)
               l=l1+k
               dc(l)=dsig1(kk) * invv    
            enddo
	      goto 311
	    endif
          l1=l1+nbrxi
	  enddo
  311   continue
        invv=1

	else if(ic.eq.56) then
c     restrictions nø 56:  contrainte max cisaillement ame (flexion local raid) 
c     --------------------------------------------------------------------------
        vnom2='raid loc tau' ! 1.73*tau < t(adm) ;(tau a l'axe neutre et m=0)
        c=tau
c	  call annuld(dc,9*neto)	!dcn07
        l1=0
        do ipan=1,neto
          nbrxi=nvar(ipan)
          if(ipan.eq.nel) then
            do k=1,nbrxi
               kk=nxit(k,nel)
               l=l1+k
               dc(l)=dtau(kk)   
            enddo
	      goto 312
	    endif
          l1=l1+nbrxi
	  enddo
  312   continue


	else if(ic.eq.57) then
c     restrictions nø 57: fleche abs. maille (extr. encastrees): 
c               f/l=1*pl^3/384ei < ratio (1/200, ...
c     -----------------------------------------------------------------------------
        vnom2='w maille uni'
        c=wpl1
c	  call annuld(dc,9*neto)	!dcn07
        l1=0
        do ipan=1,neto
          nbrxi=nvar(ipan)
          if(ipan.eq.nel) then
            do k=1,nbrxi
               kk=nxit(k,nel)
               l=l1+k
               dc(l)=dwpl1(kk)   
            enddo
	      goto 313
	    endif
          l1=l1+nbrxi
	  enddo
  313   continue


	else if(ic.eq.58) then
c     restrictions nø 58: fleche abs. maille (appuis simples):
c                   f/l=1*pl^3/384ei < ratio (1/200, ...
c     ----------------------------------------------------------------
        vnom2='w maille loc'
        c=wpl2
c	  call annuld(dc,9*neto)	!dcn07
        l1=0
        do ipan=1,neto
          nbrxi=nvar(ipan)
          if(ipan.eq.nel) then
            do k=1,nbrxi
               kk=nxit(k,nel)
               l=l1+k
               dc(l)=dwpl2(kk)   
            enddo
	      goto 314
	    endif
          l1=l1+nbrxi
	  enddo
  314   continue


c     -----------------------------------------------------------
c     -----------------------------------------------------------

	else
        write(*,*)' la restriction nø',ic,' n''existe pas !!!'
        write(*,*)' dans panneau n°',nel,' !!!'
        write(*,*)' veuillez svp corriger les données.'
	  
        write(6970,*)' la restriction nø',ic,' n''existe pas !!!'
        write(6970,*)' dans panneau n°',nel,' !!!'
        write(6970,*)' veuillez svp corriger les données.'

	  write(29,*)' la restriction nø',ic,' n''existe pas !!!'					!bug
        write(29,*)' dans panneau n°',nel,' !!!'									!bug
        write(29,*)' veuillez svp corriger les données.'							!bug

	  pause 'stop'
	  stop
	end if

c ***********************************************************************   
c     sauvetage sur disque,  file 303		!extension neto	!fev2007 
c     -----------------------
	write(303) c,cm,vnom2,ic,ratio			!extension neto	!fev2007  dad
	write(303) (dc(i),i=1,ntot)		!extension neto	!fev2007
	

!	write(6969,*) c,cm,vnom2,ic,ratio			!extension neto	!fev2007  dad
!	write(6969,*) (dc(i),i=1,ntot)		!extension neto	!fev2007
!      write(6969,*) 'break_lbr5'

c		do ii=1,ntot
c		open(666660, file = 'dc-contr856-dyn.txt')
c		write (666660,*) dc(ii)
c		enddo
c		pause

c     impressions
c     ------------
c     ir compteur  global de toutes les restrictions (pour la structure complète)
      ir=icont+img+m2tot     !is = load case

      if(itera.eq.0) then		!sept06
	if (impr2.ge.-2) then	!sept06
        if(invv.eq.-1) then
          write(666,427) ir,cjmax(icont),ic,vnom2,iy1,iy2
        else
          write(666,428) ir,cjmax(icont),ic,vnom2,iy1,iy2
        endif
      endif
	endif					!sept06


c **********************************************************************

c     fin de la boucle principale sur le nbre de restrictions
   1  continue

      im=im+m   ! increment du compteur des restr structurelles

      return

  10  write(*,*) 'dans subr. contr : it=0 . impossible -> error'
      write(*,*) 'positions de calcul des restrictions relatives'
      write(*,*) 'aux traverses sont erronés (subr. contr)'

      write(6970,*) 'dans subr. contr : it=0 . impossible -> error'
      write(6970,*) 'positions de calcul des restrictions relatives'
      write(6970,*) 'aux traverses sont erronés (subr. contr)'

      write(666,*) 'dans subr. contr : it=0 . impossible -> error'
      write(666,*) 'positions de calcul des restrictions relatives'
      write(666,*) 'aux traverses sont erronés (subr. contr)'
	
	write(29,*) 'dans subr. contr : it=0 . impossible -> error'						!bug
      write(29,*) 'positions de calcul des restrictions relatives'					!bug
      write(29,*) 'aux traverses sont erronés (subr. contr)'							!bug

	pause 'stop'
	stop
	
c     ----------------------------------------------------------------- 

  100 format(' rest. nø',i4,' de type ',i2,'; c=',e14.7/'dc=',
     *        (t4,6(e11.4,1x)))
  196 format(/'!!! le calcul de sig ultime (paik)',
     *       ' n''est pas prévu pour une coque cylindrique !!!'/
     *       ' mais il est acceptable en 1ère approximation.'/)
  284 format('nombre de restrictions structurelles locales = ',i3/
     *       'aux pts ypt=',5(e11.5,1x),' (deg)')
  427 format('c(',i4,') > ',e11.5,' de type nø',i2,' (',a12,')',
     *       ' au pt ypt nø',i2,' et au pt de calcul nø',i2)
  428 format('c(',i4,') < ',e11.5,' de type nø',i2,' (',a12,')',
     *       ' au pt ypt nø',i2,' et au pt de calcul nø',i2)
      end

************************************************************************
c **********************************************************************

      subroutine csens(invv,iy,j,dc)
      use sharedvar
      implicit real*8(a-h,o-z)
      dimension dc(9*neto)
      l1=0
      do 306 ipan=1,neto
        nbrxi=nvar(ipan)
        do 305 k=1,nbrxi
          kk=nxit(k,ipan)
          l=l1+k
          dc(l)=sens4(j,iy,kk,ipan)*invv
  305   continue
        l1=l1+nbrxi
  306 continue
      return
      end

************************************************************************

      subroutine csens2(invv,iy,j,dc)
      use sharedvar
      implicit real*8(a-h,o-z)
      dimension dc(9*neto)
      l1=0
      do 306 ipan=1,neto
        nbrxi=nvar(ipan)
        do 305 k=1,nbrxi
          kk=nxit(k,ipan)
          l=l1+k
          dc(l)=sens2(j,iy,kk,ipan)*invv
  305   continue
        l1=l1+nbrxi
  306 continue
      return
      end

************************************************************************

      subroutine csens3(invv,iy,j,ipt,ic,dc)
      use sharedvar
      implicit real*8(a-h,o-z)
      dimension dc(9*neto)
      l1=0
      do 306 ipan=1,neto
        nbrxi=nvar(ipan)
        do 305 k=1,nbrxi
            kk=nxit(k,ipan)
            l=l1+k
            if(ic.eq.5) dc(l)=sens4(j,iy,kk,ipan)-sens4(j,1,kk,ipan)
            if(ic.eq.6) dc(l)=sens4(j,iy,kk,ipan)-sens4(j,ipt,kk,ipan)
            if(ic.eq.7) dc(l)=sens4(j,iy,kk,ipan) -
     *                     (sens4(j,1,kk,ipan)+sens4(j,ipt,kk,ipan))/2.
            dc(l)=dc(l)*invv
  305   continue
        l1=l1+nbrxi
  306 continue
      return
      end

************************************************************************

      subroutine csens4(iy,nel,dspaik,smoyen,spaik,dc)
      use sharedvar
      implicit real*8(a-h,o-z)
      dimension dspaik(9),dc(9*neto)
      l1=0
      sp2=spaik*spaik
      do 306 ipan=1,neto
        nbrxi=nvar(ipan)
        do 305 k=1,nbrxi
          kk=nxit(k,ipan)
          l=l1+k
          dc(l)= spaik * sens3(21,iy,kk,ipan) ! 1er partie
          if(ipan==nel) then
            dc(l)= dc(l) - smoyen * dspaik(kk)  ! 2eme partie
          endif
          dc(l)=-dc(l)/sp2
  305   continue
        l1=l1+nbrxi
  306 continue
      return
      end

************************************************************************







