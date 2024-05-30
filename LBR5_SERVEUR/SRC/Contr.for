      subroutine contr(nel,im,img_,itera_,
     *                 e1,sigy1,effcomb,eff,
     *                 dtpl,tpld,tpla,tplr,delta,phil,
     *                 epsa,hxr,dxr,wxr,txr,epsr,entr,
     *                 sxm,dspaik,spaik,omt,
     *                 m1tabl1,
     *                 sig1,sig2,sig3,sig4,tau,flr,
     *                 wpl1,wpl2,
     *                 dsig1,dsig2,dsig3,dsig4,dtau,dflr,dwpl1,dwpl2,
     *                 sigplaque,dsigplaque,ratio)     !dad
      
      
      use param_section, temp_sxm => sxm !pour �viter conflit
      

      implicit double precision(a-h,o-z)
      double precision nx(51,5)
      character*30 vnom2_    !dad
      dimension v(51,5),u(51,5),w(51,5),sbord(51,5),sajas(51,5),
     *   sajab(51,5),sasem(51,5),srjas(51,5),srjab(51,5),srsem(51,5),
     *   sbord1(51,5),sbord2(51,5),
     *   stjas(10,5),stjab(10,5),stsem(10,5),
     *   effcomb(9690),eff(9690)          !avril2003
      
	dimension sxm(iptmax),dspaik(9)
      dimension dsig1(9),dsig2(9),dsig3(9),dsig4(9),dtau(9),dflr(9),   !avril2003
     *          dwpl1(9),dwpl2(9)                                      ! vecteur de travail
      dimension m1tabl1(neto)
	dimension sigplaque(iptmax),dsigplaque(iptmax,9)    !avril2003

	double precision, allocatable, save :: dc(:)

	allocate (dc(ntot(iboat)))

c **********************************************************************
c     subroutine contraintes (c.�.d) "restrictions"
c     ==============================================
c     calcul des m restrictions. c(j) du pan. nel , (j=1+im,m+im)  (max 7000 pour la struct.)
c     im = nbre de restrictions d�j� consid�r�es par les panneaux 1 � nel-1.
c
c     calcul des nbrxi d�riv�es des restrictions dc(i,j), i=1,nbrxi [nbrxi=nvar(nel,iboat)]
c     ipts = nbre de points de calcul des restrictions (par panneau) (max 10)
c     ypts = ordonn�es de ces ipts points de calcul des restrictions (par panneau)
c	lcont(1,j+im) = n� de r�f�rence de la restriction j du panneau nel
c                     ex: 11 pour sigma comp bordage
c	lcont(2,j+im) = n� du pt d'applic. de la restr. j du panneau nel (sensibilit�)
c                   = 1 � 10
c	ipt2(i,nel) = n� des ipt pts d'applic. pour le panneau nel (1 � 31)
c	ipt2(lcont(2,j+im),nel) = n� du pt d'applic. de la restr. j du panneau nel (1 � 31).
c	ipt3(lcont(2,j+im),nel) = n� de la trav. correspondant au pt ipt (pour le panneau nel)
c
c     cr�er   :  29-04-94   (dr. ph. rigo)
c
c     modifier:  15-05-96   restriction n� 4  (dmin - d < 0)
c                 5- 6-96   restrictions g�om�triques
c                 4- 7-96   restrictions n�5 (w relatif) et subr. sens3
c                22- 7-96   restrictions n�15 sx(uls) de paik et subr. sens4
c                 6- 3-97   prise en compte des diff�rences entre d(raid) et de epsr	       
c                16-11-00   restrictions 51 � 56: flexion locale raid.	       
c                 4-02-02   restrictions struct par cas de charge	    
c                  -07-03   ajouts de restrictions combin�es, changement du num�ro de 
c                           quelques restrictions   
c
c    derniere modification : juillet 2003
c                
c ******************************************************************************

c   les variables de conception  ; nbrxi = nvar(nel,iboat)
c   -------------------------------------------------
c       1       delta =  �paisseur du bordage
c       2       hya   =  hauteur de l'�me des cadres   
c       3       dya   =  �paisseur de l'�me des cadres   
c       4       wya   =  largeur des semelles des cadres   
c       5       epsa  =  entredistance entre cadres   
c       6       hxr   =  hauteur de l'�me des raidisseurs
c       7       dxr   =  �paisseur de l'�me des raidisseurs
c       8       wxr   =  largeur des semelles des raidisseurs
c       9       epsr  =  entredistance entre raidisseurs

c  restrictions:
c  -------------
c  ic(j) = le num�ro de r�f�rence des m restrictions struct. (j=1,m) du panneau en question. 

c  [ix(j),iy(j)] coordonn�es (x,y) du point o� la restriction est appliqu�e.
c    - ix est choisi par d�faut (mi-port�e ou appuis selon la fct sinus ou cosinus).
c    - iy est une donn�e.
c       iy1 = n� du pt d'applic. de la restriction =1�4  (pts d'applic. des restrictions)
c       iy2 = n� du pt d'applic. de la restriction =1�31 (pts de calcul des d�plac. et contr.)

c  d�riv�es :sens4(j,i,k,ipan) valeurs cumulees.
c        avec      j=1,9   les fonctions (ex: pour w, j=1)
c				   w,w�,w��,w���,u,u�,v,v�,v��
c		           1 2  3   4    5 6  7 8  9 
c  			 i=1,ipts les points de calcul (correspondant aux pts de calcul des restrictions et des sensibilites)
c  			 k=1,9    les variables de conception (si ep. bord., k=1)
c              ipan=1,neto le panneau relatif � ces k variables de conception

c ****************************************************************
c 1.  restrictions structurelles  (boucle 1)
c ****************************************************************
c       n� de r�f�rence de la restriction = ic=lcont(1,icont)
c   ----------------------------------------------------------
c     n� 1     v d�placement (en m)
c     n� 2     u d�placement (en m)
c     n� 3     w d�placement (en m)

c	n� 4	 epaisseur minimale du bord� (en m): dmin - d < 0 [restriction dite de "hughes"] 

c	n� 5	 w(relatif) d�pl. relatif par rapport au bord de d�part (y=0)  du panneau (en m)
c	n� 6	 w(relatif) d�pl. relatif par rapport au bord d'arriv�e (y=yo) du panneau (en m)
c	n� 7	 w(relatif) d�pl. relatif par rapport au d�pl. moyen des extr�mit�s du panneau (en m)

c	n� 11	 sigma comp. (sx,sy) bordage (z=0)      en x=l/2 (en n/m2)  sens2(1,..
c	n� 12	 sigma comp. (sx,sy) bordage (z=0)      en x=l/2 (en n/m2)  sens2(1,.. avec flex.loc.des raid.  !avril2003
c	n� 13	 sigma comp. (sx,sy) bordage (z=0)      en x=l/2 (en n/m2)  sens2(1,.. idem + effet plaque      !avril2003

c	n� 15	 sx < s(ult. strength)  faxial(moyen)   en x=l/2  (cfr subr paik)

c	n� 16	 sigma comp. (txy)   bordage (z=0)      en x=0			sens2(2,..       !avril2003
c	n� 17	 sigma comp. (sx,sy) bordage (z=+d/2)   en x=l/2		sens2(4,..       !avril2003
c	n� 18	 sigma comp. (sx,sy) bordage (z=-d/2)   en x=l/2		sens2(3,..       !avril2003

c	n� 21	 sigma comp. (t�me)   cadre      jas/jab en x=0  (en n/m2)	sens2(5,..
c	n� 22	 sigma comp. (sy,txy) cadre      jas     en x=l/2		sens2(6,..
c	n� 23	 idem cond. n�21 car tau(jas)=tau(jab)				    sens2(7,..
c	n� 24	 sigma comp. (sy,txy) cadre      jab     en x=l/2		sens2(8,..
c	n� 25	 sigma comp. (sy,txy) cadre      semelle en x=l/2		sens2(9,..

c	n� 31	 sigma comp. (t�me)   raidisseur jab     en x=0			sens2(11,.. !avril2003
c	n� 32	 sigma comp. (sx)     raidisseur semelle en x=l/2		sens2(12,.. !avril2003

c     n� 33    sigma comp. (t�me)   raidisseur jab     en x=0			sens2(11,.. avec flex.loc.des raid.  !avril2003
c	n� 34	 sigma comp. (sx)     raidisseur semelle en x=l/2		sens2(12,.. avec flex.loc.des raid.  !avril2003
c     n� 35    sigma comp. (sx,t�me) raidisseur jab    en x=l/2		sens2(10,.. avec flex.loc.des raid.  !avril2003
c     n� 36    sigma comp. (sx,t�me) raidisseur jas    en x=l/2		sens2(16,.. avec flex.loc.des raid.  !avril2003

c	n� 41	 sigma comp. (t�me)   traverse  jas      en x=0  (en n/m2)	sens2(13,..
c	n� 42	 sigma comp. (t�me)   traverse  jab      en x=0			sens2(14,..
c	n� 43	 sigma comp. (sx)     traverse  semelle  en x=l/2		sens2(15,..

c     flexion locale du raidisseur:
c	n� 51	 fl�che relative raid: extr. encastr�es : f/l=1*pl^3/384ei < ratio (1/200, ...
c	n� 52	 fl�che relative raid: appuis simples   : f/l=5*pl^3/384ei < ratio (1/200, ..
c	n� 54	 contrainte max semelle:  (m=pl^2/10) ; sig < s(adm)
c	n� 55	 contrainte max borde:    (m=pl^2/10) ; sig < s(adm)
c	n� 56	 contrainte max cisaillement dans �me : 1.73*tau < t(adm);(tau � l'axe neutre et m=0)
c	n� 57	 fl�che abs. maille: bords encastr�s : w=1*pl^4/384ei < wmax (m)
c	n� 58	 fl�che abs. maille: bords articul�s : w=5*pl^4/384ei < wmax (m)

c rem: dans les donnees, les restrictions ne doivent pas �tre classees par ordre  !!!
c **********************************************************************************

      ispecial = 0

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
				
      if((itera_.eq.0).and.(m.ne.0)) then		!sept06
	  write(iu_31(iboat),'(a,i3)') 'panneau n� ', nel
	  write(iu_31(iboat),'(a)')    '-----------------'
        write(iu_31(iboat),284) m,(ypts(i,nel),i=1,panneau(nel).ipts)   ! ypts (ordonn�es des pts)
      endif
	
	
c     boucle principale sur les "m" restrictions du panneau "nel"
c     ===========================================================
      do 1 icont=im+1,im+m
!	icont=im
!	do 1 is=1,nsol
!	  ij = nnsol(is)
!	  do 1 i=1,m1tabl(nel,is)	
!	  icont = icont+1

        ic = panneau(nel).lcont(1,icont-im)        
	  iy1= panneau(nel).lcont(2,icont-im)
        iy2= panneau(nel).ipts2(iy1)
        it = panneau(nel).ipts3(iy1)

        invv= panneau(nel).inv(icont-im)
        cm= panneau(nel).cjmax(icont-im)*invv

	  call annuld(dc,ntot(iboat))		!dcn07

      if(ic.eq.1) then
c     restrictions n� 1 :  d�form�e v � mi-port�e (x=l/2 m)
c     -----------------------------------------------------------
c     d�form�e v(iy,ix) ; iy=iy2 et ix=5 pour x=l/2
        vnom2_='d�pl. v; l/2'
        ix=5
        c=v(iy2,ix)*invv

c     d�riv�e de v � mi-port�e = sens4(7,iy,kk,ipan) et iy = iy1
        j=7
        call csens(invv,iy1,j,dc)

      else if(ic.eq.2) then
c     restrictions n� 2 :  d�form�e u aux extr. (x=0 m)
c     -----------------------------------------------------------
c     d�form�e u(iy,ix); iy=iy2 et ix=1 pour x=0
        vnom2_='d�pl. u; x=0'
        ix=1
        c=u(iy2,ix)*invv

c     d�riv�e de u aux extr. = sens4(5,iy,kk,ipan) et iy = iy1
        j=5
      call csens(invv,iy1,j,dc)

      else if(ic.eq.3) then
c     restrictions n� 3 :  d�form�e w � mi-port�e  (x=l/2 m)
c     -----------------------------------------------------------
c     d�form�e w(iy,ix);  iy=iy2  et ix=5 pour x=l/2
        vnom2_='d�pl. w; l/2'
        ix=5
        c=w(iy2,ix)*invv

c     d�riv�e de w � mi-port�e = sens4(1,iy,kk,ipan) et iy = iy1
        j=1
        call csens(invv,iy1,j,dc)

      else if(ic.eq.4) then
c     restrictions n� 4 : epaisseur minimale du bord� (hughes) : dmin - d < 0
c     ------------------------------------------------------------------
        vnom2_='dmin - d < 0'
        c=0.
        cm=0.
c        call annuld (dc,ntot(iboat))		!dcn07
        found=0

        if(nxi2(1,nel,iboat).ne.0)then
c         delta = d est une variable de conception (xi=1)
          c=dtpl
          dc(nxi2(1,nel,iboat))=tpld
		found=1
        endif
        if(nxi2(5,nel,iboat).ne.0)then
c         epsa est une variable de conception (xi=5)
          c=dtpl
          dc(nxi2(5,nel,iboat))=tpla
		found=1
        endif
        if(nxi2(9,nel,iboat).ne.0)then
c         epsr est une variable de conception (xi=9)
          c=dtpl
          dc(nxi2(9,nel,iboat))=tplr
		found=1
        endif

        if (found.eq.0) then
		if (langue.eq.1) then
			write(*,*) 'Erreur restri 4 : restriction selectionnee san
     *s variables de conception adequate'
			write(*,*) 'Veuillez corriger vos donnees - appuyer sur EN
     *TER pour poursuivre'
		elseif (langue.eq.2) then
			write(*,*) 'Error in constraint 4: constraint selected wit
     *hout adequate design variable'
	     write(*,*) 'Please correct your data - press ENTER to resume'
		endif
	    read(*,*)
	    stop
	  endif

      else if((ic.ge.5).and.(ic.le.7)) then
c     restrictions n� 5,6,7 :  d�form�e relative de w   (x=l/2 m)
c     ---------------------------------------------------------------
c     d�form�e w(iy,ix);  iy=iy2  et ix=5 pour x=l/2
        vnom2_='depl w relat'
        	  
	  if (ispecial.eq.0) ix=5
	  if (ispecial.eq.1) ix=2

        if(ic.eq.5) c=w(iy2,ix) - w(1,ix)
        if(ic.eq.6) c=w(iy2,ix) - w(31,ix)
        if(ic.eq.7) c=w(iy2,ix) - (w(1,ix)+w(31,ix))/2.
        c=c*invv

c       d�riv�e de w � mi-port�e = sens4(1,iy,kk,ipan) et iy = iy1
        j=1
        call csens3(invv,iy1,j,panneau(nel).ipts,ic,dc)

      else if(ic.eq.11) then
c     restrictions n� 11: contrainte de comp. dans le bordage en z=0 (x=l/2)
c     ----------------------------------------------------------------------
c       contrainte sbord(iy,ix);    iy=iy2  et ix=5 pour x=l/2
        vnom2_='sbor z=0;l/2'
        
	  if (ispecial.eq.0) ix=5
	  if (ispecial.eq.1) ix=2

        c=sbord(iy2,ix)*invv

c       d�riv�e de sbord � mi-port�e = sens2(1,iy,kk,ipan) et iy = iy1
        j=1
        call csens2(invv,iy1,j,dc)
	continue

      else if(ic.eq.12) then    !avril2003 (pour tout le point 12!!!!!!!!!!!!!!!!!!!!!!!!!!!!)
c     restrictions n� 12: contrainte de comp. dans le bordage en z=0 (x=l/2) 
c     ----------------------------------------------------------------------
c     (avec flexion locale des raidisseurs)
c       contrainte sbord(iy,ix);    iy=iy2  et ix=5 pour x=l/2
        vnom2_='sbord+stiff '
	  
	  if (ispecial.eq.0) ix=5
	  if (ispecial.eq.1) ix=2

        c = effcomb(4845+(ix-1)*51+iy2)*invv !!! (ix-1) au lieu de 4

	! Test si existence de raid !!! Sinon pas ad�quat d'utiliser cette restriction
	if (panneau(nel).hxr.le.0.001) then
		write(*,*) 'Attention ! Panneau :',nel
		write(*,*) 'La restriction 12 est utilisee avec des raidis san
     *s semelles !!! Cela peut poser probleme'
		!read(*,*)
	endif
	!

      l1=0

	do ipan=1,neto
	  nbrxi=nvar(ipan,iboat)
	  ij = (ix-1)*51 + panneau(ipan).ipts2(iy1)	!!! (ix-1) au lieu de 4
        do k=1,nbrxi
          kk=nxit(k,ipan,iboat)
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
c     restrictions n� 13: contrainte de comp. dans le bordage en z=0 (x=l/2) 
c     ----------------------------------------------------------------------
c     (avec flexion locale des raidisseurs et contrainte plaque)
c       contrainte sbord(iy,ix);    iy=iy2  et ix=5 pour x=l/2
        vnom2_='s+stiff+pltb'

	! Test si existence de raid !!! Sinon pas ad�quat d'utiliser cette restriction
	if (panneau(nel).hxr.le.0.001) then
		write(*,*) 'Attention ! Panneau :',nel
		write(*,*) 'La restriction 13 est utilisee avec des raidis san
     *s semelles !!! Cela peut poser probleme'
		!read(*,*)
	endif
	!
        
	  ix=5
        c = sigplaque(iy1)*invv

	l1=0

	do ipan=1,neto
	  nbrxi=nvar(ipan,iboat)
	  if (ipan.eq.nel) then
	    do k=1,nbrxi
	      kk=nxit(k,ipan,iboat)
	      l = l1 + k
		  dc(l) = dsigplaque(iy1,kk)*invv
	    enddo
	    goto 301
	  endif
	  l1 = l1 + nbrxi
	enddo
  301 continue
		
      else if(ic.eq.14) then                                                     !avril2003

c	restrictions n� 14: flambement du bordage en z=0 (x=l/2)				   !dcn07
c	--------------------------------------------------------
	  vnom2_='fl.bord.;l/2'
c	  cm=1.

c	  call annuld(dc,ntot(iboat))		!dcn07

	  c=dbuckpl_07
	  
	  l1=0
        do ipan=1,neto
          nbrxi=nvar(ipan,iboat)
          do k=1,nbrxi
            kk=nxit(k,ipan,iboat)
            l=l1+k
            dc(l)=ddbuckpl_07(kk,ipan)
		enddo
          l1=l1+nbrxi
	  enddo

	else if(ic.eq.16) then 

c     restrictions n� 16: contrainte de comp. dans le bordage en z=0 (x=0)       !avril2003
c     --------------------------------------------------------------------------
c       contrainte sbord(iy,ix);    iy=iy2  et ix=1 pour x=0
        vnom2_='tbor z=0 x=0'
	  ix=1
        c=sbord(iy2,ix)*invv

c       d�riv�e de sbord � mi-port�e = sens2(2,iy,kk,ipan) et iy = iy1	pour t bord j=2 
        j=2
        call csens2(invv,iy1,j,dc) 


	else if(ic.eq.17) then                                                     !avril2003
c     restrictions n� 17: contrainte de comp. dans le bordage en z=+d/2 (x=l/2)  !avril2003
c     --------------------------------------------------------------------------
c       contrainte sbord1(iy,ix);    iy=iy2  et ix=5 pour x=l/2
        vnom2_='sbord z=+d/2'
	  ix=5
        c=sbord1(iy2,ix)*invv

c       d�riv�e de sbord � mi-port�e = sens2(4,iy,kk,ipan) et iy = iy1	pour s bord en z=d/2
        j=4                                                             !   sens2(4,.....
        call csens2(invv,iy1,j,dc)

	else if(ic.eq.18) then                                                      !avril2003
c     restrictions n� 18: contrainte de comp. dans le bordage en z=-d/2 (x=l/2)   !avril2003
c     --------------------------------------------------------------------------
c       contrainte sbord2(iy,ix);    iy=iy2  et ix=5 pour x=l/2
        vnom2_='sbord z=-d/2'
	  ix=5
        c=sbord2(iy2,ix)*invv

c       d�riv�e de sbord � mi-port�e = sens2(3,iy,kk,ipan) et iy = iy1	pour s bord en z=-d/2 
        j=3                                                             ! sens2(3,.....
        call csens2(invv,iy1,j,dc)

	else if(ic.eq.15) then
c     resistance ultime of longitudinally compresses stiffened panels
c     - cfr article de paik dans ship research).
c	- restriction n� 15  sx < s(ult. strength)  ; faxial moyen  en x=l/2 
c     ---------------------------------------------------------------------
	
c       cas d'une coque cylindrique	   
        if(phil.ge.1.) then     ! coque/plaque
	    write(iu_31(iboat),196)
!	    write(9  ,196)
		write(iu_14(iboat) ,196)		!sept06
	  endif										

        vnom2_='sx/s(uls)<sc'
        cm=dabs(cm)         ! borne; c(x) < cm (=cmax)

c	  call annuld(dc,9*neto)	!dcn07

c     sxm    = valeur  du sx axial moyen (calcule dans subr resul)
c     d(sxm)/dx = sens3(21,iptmax,9,neto)
c     spaik  = r�sistance ultime selon la formule de j. paik
c     dspaik = d�riv�e de la r�sistance ultime spaik par rapport aux xi

        call paik(nel,e1,sigy1,delta,epsa,hxr,dxr,wxr,txr,epsr,entr,
     *            dspaik,spaik,omt)

        smoyen=sxm(iy1)
        c=-smoyen/spaik

c     d�riv�e de "-smoyen/spaik" � mi-port�e
c     .......................................
c      avec spaik > 0  et nx < 0 en compression.
c      si nx>0 la restriction n'a plus de sens et est automatiquement v�rifi�e.

        call csens4(iy1,nel,dspaik,smoyen,spaik,dc)

      else if(ic.eq.21) then
c     restrictions n� 21: contrainte de comp. cadre jas aux appuis (x=0)
c     ------------------------------------------------------------------
c       contrainte sajas(iy,ix);    iy=iy2  et ix=1 pour x=0
        vnom2_='tcad jas x=0'
        ix=1
        c=sajas(iy2,ix)*invv

	! Test si existence de semelle !!! Sinon pas ad�quat d'utiliser cette restriction
	if (panneau(nel).wya.le.0.001) then
		write(*,*) 'Attention ! Panneau :',nel
		write(*,*) 'La restriction 21 est utilisee avec des cadres san
     *s semelles !!! Cela peut poser probleme'
		!read(*,*)
	endif
	!

c       d�riv�e de sbord � mi-port�e = sens2(5,iy,kk,ipan) et iy = iy1
        j=5
        call csens2(invv,iy1,j,dc)

      else if(ic.eq.22) then
c     restrictions n� 22: contrainte de comp. cadre jas � mi-port�e (x=l/2)
c     --------------------------------------------------------------------------
c       contrainte sajas(iy,ix);    iy=iy2  et ix=5 pour x=0
        vnom2_='scad jas l/2'
	  
	  if (ispecial.eq.0) ix=5
	  if (ispecial.eq.1) ix=2

        c=sajas(iy2,ix)*invv

! Test si existence de cadres !!! Sinon pas ad�quat d'utiliser cette restriction
	if (panneau(nel).hya.le.0.001) then
		write(*,*) 'Attention ! Panneau :',nel
		write(*,*) 'La restriction 22 est utilisee sans cadres !!!'
		write(*,*) 'Cela peut poser probleme'
		!read(*,*)
	endif
	!

c       d�riv�e de sbord � mi-port�e = sens2(6,iy,kk,ipan) et iy = iy1	pour sig cadre jas
        j=6                 ! sens2(6,.....
        call csens2(invv,iy1,j,dc)

	else if(ic.eq.23) then
c     restrictions n� 23: contrainte de comp. cadre jab aux appuis (x=0)
c     --------------------------------------------------------------------------
c       contrainte sajab(iy,ix);    iy=iy2  et ix=1 pour x=0
        vnom2_='tcad jab x=0'
	  ix=1
        c=sajab(iy2,ix)*invv

	! Test si existence de cadres !!! Sinon pas ad�quat d'utiliser cette restriction
	if (panneau(nel).hya.le.0.001) then
		write(*,*) 'Attention ! Panneau :',nel
		write(*,*) 'La restriction 23 est utilisee sans cadres !!!'
		write(*,*) 'Cela peut poser probleme'
		!read(*,*)
	endif
	!

c       d�riv�e de sbord � mi-port�e = sens2(7,iy,kk,ipan) et iy = iy1	pour sig cadre jab
        j=7  ! sens2(7,.....
        call csens2(invv,iy1,j,dc)

	else if(ic.eq.24) then
c     restrictions n� 24: contrainte de comp. cadre jab � mi-port�e (x=l/2)
c     --------------------------------------------------------------------------
c       contrainte sajab(iy,ix);    iy=iy2  et ix=5 pour x=0
        vnom2_='scad jab l/2'
	  
	  !if (ispecial.eq.0) ix=5
	  ix=5
	  !if (ispecial.eq.1) ix=2

        c=sajab(iy2,ix)*invv

	! Test si existence de cadres !!! Sinon pas ad�quat d'utiliser cette restriction
	if (panneau(nel).hya.le.0.001) then
		write(*,*) 'Attention ! Panneau :',nel
		write(*,*) 'La restriction 24 est utilisee sans cadres !!!'
		write(*,*) 'Cela peut poser probleme'
		!read(*,*)
	endif
	!

c       d�riv�e de sbord � mi-port�e = sens2(8,iy,kk,ipan) et iy = iy1	pour sig cadre jab
        j=8   ! sens2(8,.....
        call csens2(invv,iy1,j,dc)

	else if(ic.eq.25) then
c     restrictions n� 25: contrainte de comp. cadre sem  (x=l/2)
c     --------------------------------------------------------------------------
c       contrainte sasem(iy,ix);    iy=iy2  et ix=5 pour x=l/2
        vnom2_='scad sem l/2'
	  ix=5
        c=sasem(iy2,ix)*invv

! Test si existence de semelle !!! Sinon pas ad�quat d'utiliser cette restriction
	if (panneau(nel).wya.le.0.001) then
		write(*,*) 'Attention ! Panneau :',nel
		write(*,*) 'La restriction 25 est utilisee avec des cadres san
     *s semelles !!! Cela peut poser probleme'
		!read(*,*)
	endif
	!

c       d�riv�e de sbord � mi-port�e = sens2(9,iy,kk,ipan) et iy = iy1	pour sig cadre sem
        j=9 ! sens2(9,.....
        call csens2(invv,iy1,j,dc)

	else if(ic.eq.31) then                                               !avril2003
c     restrictions n� 31: contrainte de comp. raid jab aux appuis (x=0)    !avril2003
c     -----------------------------------------------------------------
c       contrainte srjab(iy,ix);    iy=iy2  et ix=1 pour x=0
        vnom2_='trai jab x=0'
	  ix=1
        c=srjab(iy2,ix)*invv

	! Test si existence de raidisseurs !!! Sinon pas ad�quat d'utiliser cette restriction
	if (panneau(nel).hxr.le.0.001) then
		write(*,*) 'Attention ! Panneau :',nel
	  write(*,*) 'La restriction 31 est utilisee sans raidisseurs !!!'
		write(*,*) 'Cela peut poser probleme'
		!read(*,*)
	endif
	!
c       d�riv�e de sbord � mi-port�e = sens2(11,iy,kk,ipan) et iy = iy1	pour sig raid jab
        j=11  ! sens2(11,.....
        call csens2(invv,iy1,j,dc)

	else if(ic.eq.32) then                                         !avril2003
c     restrictions n� 32: contrainte de comp. raid sem  (x=l/2)      !avril2003
c     ---------------------------------------------------------
c       contrainte srsem(iy,ix);    iy=iy2  et ix=5 pour x=l/2
        vnom2_='srai sem l/2'
	  ix=5
        c=srsem(iy2,ix)*invv

	! Test si existence de semelle !!! Sinon pas ad�quat d'utiliser cette restriction
	if (panneau(nel).wxr.lt.0.005) then
		write(*,*) 'Attention ! Panneau :',nel
		write(*,*) 'La restriction 32 est utilisee avec des raidisseur
     *s sans semelles !!! Cela peut poser probleme'
		!read(*,*)
	endif
	!

c       d�riv�e de sbord � mi-port�e = sens2(12,iy,kk,ipan) et iy = iy1	pour sig raid sem
        j=12  !  sens2(12,.....
        call csens2(invv,iy1,j,dc)

      else if(ic.eq.33) then            !avril2003 (pour tout le point 33!!!!!!!!!!!!!!!!!!!!!!!!!!!!)
c     restrictions n� 33: contrainte de comp. raid jab aux appuis (x=0) avec flexion locale des raidisseurs
c     -----------------------------------------------------------------------------------------------------
c       contrainte srjab(iy,ix);    iy=iy2  et ix=1 pour x=0
        vnom2_='tjabx=0+stiff'
	  ix=1
	  c = effcomb(9180+iy2)*invv

	! Test si existence de raidisseurs !!! Sinon pas ad�quat d'utiliser cette restriction
	if (panneau(nel).hxr.le.0.001) then
		write(*,*) 'Attention ! Panneau :',nel
	   write(*,*) 'La restriction 33 est utilisee sans raidisseur !!!'
		write(*,*) 'Cela peut poser probleme'
		!read(*,*)
	endif
	!

      l1=0
	do ipan=1,neto
	  nbrxi=nvar(ipan,iboat)
	  ij = panneau(ipan).ipts2(iy1)
	    do k=1,nbrxi
	      kk=nxit(k,ipan,iboat)
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
c     restrictions n� 34: contrainte de comp. raid sem  (x=l/2) avec flexion locale des raidisseurs
c     ---------------------------------------------------------------------------------------------
c       contrainte srsem(iy,ix);    iy=iy2  et ix=5 pour x=l/2
        vnom2_='seml/2+stiff'
	  ix=5
	  c = effcomb(9435+4*51+iy2)*invv

	! Test si existence de semelle !!! Sinon pas ad�quat d'utiliser cette restriction
	if (panneau(nel).wxr.lt.0.005) then
		write(*,*) 'Attention ! Panneau :',nel
		write(*,*) 'La restriction 34 est utilisee avec des raidisseur
     *s sans semelles !!! Cela peut poser probleme'
		!read(*,*)
	endif
	!

      l1=0
	do ipan=1,neto
	  nbrxi=nvar(ipan,iboat)
	  ij=4*51 + panneau(ipan).ipts2(iy1)
	    do k=1,nbrxi
	      kk=nxit(k,ipan,iboat)
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
c     restrictions n� 35: contrainte de comp. raid jab  (x=l/2) avec flexion locale des raidisseurs
c     ---------------------------------------------------------------------------------------------
c       contrainte srjab(iy,ix);    iy=iy2  et ix=5 pour x=l/2
        vnom2_='jabl/2+stiff'
	  ix=5
        c = effcomb(9180+4*51+iy2)*invv

	! Test si existence de raidisseurs !!! Sinon pas ad�quat d'utiliser cette restriction
	if (panneau(nel).hxr.le.0.001) then
		write(*,*) 'Attention ! Panneau :',nel
	   write(*,*) 'La restriction 35 est utilisee sans raidisseur !!!'
		write(*,*) 'Cela peut poser probleme'
		!read(*,*)
	endif
	!

      l1=0
	do ipan=1,neto
	  nbrxi=nvar(ipan,iboat)
	  ij=4*51 + panneau(ipan).ipts2(iy1)
	    do k=1,nbrxi
	      kk=nxit(k,ipan,iboat)
	      l = l1 + k
		  if (ipan.eq.nel) then	!!!aout04
			dsig3(kk) = dsig3(kk)*10./12.             ! pour avoir pl�/12 (et non pl�/10)
			den=eff(ij+7650) ! = sx jab raid
			if(dabs(den).le.(+1.e-10)) den=1.e-10
			dc(l) = sens2(10,iy1,kk,ipan)/den*dabs(den) ! = d(sx jab raid)
			if (eff(ij+7650)*sig3.ne.0) then
		      den = eff(ij+7650)*sig3
			  dc(l) = dc(l) + dsig3(kk)*den/dabs(den)
			else
			  dc(l) = dc(l) + dsig3(kk) 
			endif ! ici, dc(l) = d(sx(resul)+sx(stiff) � jab raid), c'est donc d(effcomb(ij+7650))
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
c     restrictions n� 36: contrainte de comp. raid jas  (x=l/2) avec flexion locale des raidisseurs
c     ---------------------------------------------------------------------------------------------
c       contrainte srjas(iy,ix);    iy=iy2  et ix=5 pour x=l/2
        vnom2_='jasl/2+stiff'
	  ix=5
        c = effcomb(8925+4*51+iy2)*invv

	! Test si existence de semelle !!! Sinon pas ad�quat d'utiliser cette restriction
	if (panneau(nel).wxr.lt.0.005) then
		write(*,*) 'Attention ! Panneau :',nel
		write(*,*) 'La restriction 36 est utilisee avec des raidisseur
     *s sans semelles !!! Cela peut poser probleme'
		!read(*,*)
	endif
	!

      l1=0

	do ipan=1,neto
	  nbrxi=nvar(ipan,iboat)
	  ij=4*51 + panneau(ipan).ipts2(iy1)
	    do k=1,nbrxi
	      kk=nxit(k,ipan,iboat)
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
c	restrictions n� 37: flambement du raidisseur (x=l/2)				!dcn07
c	----------------------------------------------------
	  vnom2_='fl.raid.;l/2'
c	  cm=1.

	! Test si existence de raidisseurs !!! Sinon pas ad�quat d'utiliser cette restriction
	if (panneau(nel).hxr.le.0.001) then
		write(*,*) 'Attention ! Panneau :',nel
	   write(*,*) 'La restriction 37 est utilisee sans raidisseur !!!'
		write(*,*) 'Cela peut poser probleme'
		!read(*,*)
	endif
	!
c	  call annuld(dc,ntot(iboat))		!dcn07

	  c=dbuckstif_07
	  
	  l1=0
        do ipan=1,neto
          nbrxi=nvar(ipan,iboat)
          do k=1,nbrxi
            kk=nxit(k,ipan,iboat)
            l=l1+k
            dc(l)=ddbuckstif_07(kk,ipan)
		enddo
          l1=l1+nbrxi
	  enddo


	else if(ic.eq.39) then
c	restrictions n� 38: flambement du raidisseur (x=l/2)				!dcn07
c	----------------------------------------------------
	  vnom2='fl.raid.;l/2,dcn_2008'
c	  cm=1.

	! Test si existence de raidisseurs !!! Sinon pas ad�quat d'utiliser cette restriction
	if (panneau(nel).hxr.le.0.001) then
		write(*,*) 'Attention ! Panneau :',nel
	   write(*,*) 'La restriction 38 est utilisee sans raidisseur !!!'
		write(*,*) 'Cela peut poser probleme'
		!read(*,*)
	endif
	!
c	  call annuld(dc,ntot)		!dcn07

	  c=dbuckstif_08
	  
	  l1=0
        do ipan=1,neto
          nbrxi=nvar(ipan,iboat)
          do k=1,nbrxi
            kk=nxit(k,ipan,iboat)
            l=l1+k
            dc(l)=ddbuckstif_08(kk,ipan)
		enddo
          l1=l1+nbrxi
	  enddo


	else if(ic.eq.41) then
      if(it.eq.0) goto 10
c     restrictions n� 41: contrainte de comp. trav jas aux appuis (x=0)
c     --------------------------------------------------------------------------
c       contrainte srjas(iy,ix);    it= n� de trav.  et ix=1 pour x=0
        vnom2_='ttra jas x=0'
	  ix=1
        if(it.gt.0) c=stjas(it,ix)*invv

c       d�riv�e de sbord � mi-port�e = sens2(13,iy,kk,ipan) et iy = iy1	pour sig trav jas
        j=13 ! sens2(13,.....
        call csens2(invv,iy1,j,dc)

	else if(ic.eq.42) then
      if(it.eq.0) goto 10
c     restrictions n� 42: contrainte de comp. trav jab aux appuis (x=0)
c     --------------------------------------------------------------------------
c       contrainte srjab(iy,ix);    it= n� de trav.  et ix=1 pour x=0
        vnom2_='ttra jab x=0'
	  ix=1
        if(it.gt.0) c=stjab(it,ix)*invv

c       d�riv�e de sbord � mi-port�e = sens2(14,iy,kk,ipan) et iy = iy1	pour sig trav jab
        j=14 ! sens2(14,.....
        call csens2(invv,iy1,j,dc)

	else if(ic.eq.43) then
      if(it.eq.0) goto 10
c     restrictions n� 43: contrainte de comp. trav sem  (x=l/2)
c     --------------------------------------------------------------------------
c       contrainte srsem(iy,ix);    it= n� de trav.  et ix=5 pour x=l/2
        vnom2_='stra sem l/2'
	  ix=5
        if(it.gt.0) c=stsem(it,ix)*invv

c       d�riv�e de sbord � mi-port�e = sens2(15,iy,kk,ipan) et iy = iy1	pour sig trav sem
        j=15  ! sens2(15,.....
        call csens2(invv,iy1,j,dc)

	else if(ic.eq.51) then
c     restrictions n� 51: fleche rel. raid (extr. encastrees): f/l=1*pl^3/384ei < ratio (1/200, ...
c     ----------------------------------------------------------------------------------------------
        vnom2_='raid loc fl1'
        c=flr/5.
c	  call annuld(dc,9*neto)		!dcn07
        l1=0
        do ipan=1,neto
          nbrxi=nvar(ipan,iboat)
          if(ipan.eq.nel) then
            do k=1,nbrxi
               kk=nxit(k,nel,iboat)
               l=l1+k
               dc(l)=dflr(kk) /5.  
            enddo
	      goto 308
	    endif
          l1=l1+nbrxi
	  enddo
  308   continue

	else if(ic.eq.52) then
c     restrictions n� 52:  fleche rel. raid (extr. appuy�es): f/l=5*pl^3/384ei < ratio (1/200, ...
c     ----------------------------------------------------------------------------------------------
        vnom2_='raid loc fl5'
        c=flr
c	  call annuld(dc,9*neto)	!dcn07
        l1=0
        do ipan=1,neto
          nbrxi=nvar(ipan,iboat)
          if(ipan.eq.nel) then
            do k=1,nbrxi
               kk=nxit(k,nel,iboat)
               l=l1+k
               dc(l)=dflr(kk)   
            enddo
	      goto 309
	    endif
          l1=l1+nbrxi
	  enddo
  309   continue

	else if(ic.eq.54) then
c     restrictions n� 54: flexion locale raidisseur: contr dans semelle
c     ------------------------------------------------------------------
        vnom2_='raid loc sem'   ! (m=pl^2/10) ; sig < s(adm)
	  invv=1
	  if(sig2.le.0.0001) invv=-1
        c=sig2*invv

	! Test si existence de semelle !!! Sinon pas ad�quat d'utiliser cette restriction
	if (panneau(nel).wxr.lt.0.005) then
		write(*,*) 'Attention ! Panneau :',nel
		write(*,*) 'La restriction 54 est utilisee avec des raidisseur
     *s sans semelles !!! Cela peut poser probleme'
		!read(*,*)
	endif
	!

c	  call annuld(dc,9*neto)	!dcn07
        l1=0
        do ipan=1,neto
          nbrxi=nvar(ipan,iboat)
          if(ipan.eq.nel) then
            do k=1,nbrxi
               kk=nxit(k,nel,iboat)
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
c     restrictions n� 55: flexion locale raidisseur: contr dans bord�
c     --------------------------------------------------------------------
        vnom2_='raid loc bor'   ! (m=pl^2/10) ; sig < s(adm)
	  invv=1
	  if(sig1.le.0.0001) invv=-1
        c=sig1*invv

	! Test si existence de raidisseurs !!! Sinon pas ad�quat d'utiliser cette restriction
	if (panneau(nel).hxr.le.0.001) then
		write(*,*) 'Attention ! Panneau :',nel
	   write(*,*) 'La restriction 55 est utilisee sans raidisseur !!!'
		write(*,*) 'Cela peut poser probleme'
		!read(*,*)
	endif
	!

c	  call annuld(dc,9*neto)	!dcn07
        l1=0
        do ipan=1,neto
          nbrxi=nvar(ipan,iboat)
          if(ipan.eq.nel) then
            do k=1,nbrxi
               kk=nxit(k,nel,iboat)
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
c     restrictions n� 56:  contrainte max cisaillement ame (flexion local raid) 
c     --------------------------------------------------------------------------
        vnom2_='raid loc tau' ! 1.73*tau < t(adm) ;(tau a l'axe neutre et m=0)
        c=tau

! Test si existence de raidisseurs !!! Sinon pas ad�quat d'utiliser cette restriction
	if (panneau(nel).hxr.le.0.001) then
		write(*,*) 'Attention ! Panneau :',nel
	   write(*,*) 'La restriction 56 est utilisee sans raidisseur !!!'
		write(*,*) 'Cela peut poser probleme'
		!read(*,*)
	endif
	!

c	  call annuld(dc,9*neto)	!dcn07
        l1=0
        do ipan=1,neto
          nbrxi=nvar(ipan,iboat)
          if(ipan.eq.nel) then
            do k=1,nbrxi
               kk=nxit(k,nel,iboat)
               l=l1+k
               dc(l)=dtau(kk)   
            enddo
	      goto 312
	    endif
          l1=l1+nbrxi
	  enddo
  312   continue


	else if(ic.eq.57) then
c     restrictions n� 57: fleche abs. maille (extr. encastrees): 
c               f/l=1*pl^3/384ei < ratio (1/200, ...
c     -----------------------------------------------------------------------------
        vnom2_='w maille uni'
        c=wpl1
c	  call annuld(dc,9*neto)	!dcn07
        l1=0
        do ipan=1,neto
          nbrxi=nvar(ipan,iboat)
          if(ipan.eq.nel) then
            do k=1,nbrxi
               kk=nxit(k,nel,iboat)
               l=l1+k
               dc(l)=dwpl1(kk)   
            enddo
	      goto 313
	    endif
          l1=l1+nbrxi
	  enddo
  313   continue


	else if(ic.eq.58) then
c     restrictions n� 58: fleche abs. maille (appuis simples):
c                   f/l=1*pl^3/384ei < ratio (1/200, ...
c     ----------------------------------------------------------------
        vnom2_='w maille loc'
        c=wpl2
c	  call annuld(dc,9*neto)	!dcn07
        l1=0
        do ipan=1,neto
          nbrxi=nvar(ipan,iboat)
          if(ipan.eq.nel) then
            do k=1,nbrxi
               kk=nxit(k,nel,iboat)
               l=l1+k
               dc(l)=dwpl2(kk)   
            enddo
	      goto 314
	    endif
          l1=l1+nbrxi
	  enddo
  314   continue
	
	else if(ic.eq.101) then
c     restrictions n� 101: sigmaX dans le bord� en z=0 (x=l/2)
c     ----------------------------------------------------------------
        vnom2_='b.SX z=0;l/2'

	  ix = 5
	  ij = iy2 + 51*(ix-1)
	  
	  c=dabs(eff(ij+4335))*invv

        j=1
        call csens5(invv,iy1,j,dc,eff(ij+4335))

	else if(ic.eq.102) then
c     restrictions n� 102: sigmaY dans le bord� en z=0 (x=l/2)
c     ----------------------------------------------------------------
        vnom2_='b.SY z=0;l/2'

	  ix = 5
	  ij = iy2 + 51*(ix-1)
	  
	  c=dabs(eff(ij+4080))*invv

	  j=2
        call csens5(invv,iy1,j,dc,eff(ij+4080))

	else if(ic.eq.103) then
c     restrictions n� 103: sigmaX (+STIFF) dans le bord� en z=0 (x=l/2)
c     ----------------------------------------------------------------
	  vnom2_='b.SX + Stiff'

	  ix = 5
	  ij = iy2 + 51*(ix-1)
	  
	  c=dabs(effcomb(ij+4335))*invv

	! Test si existence de raid !!! Sinon pas ad�quat d'utiliser cette restriction
	if (panneau(nel).hxr.le.0.001) then
		write(*,*) 'Attention ! Panneau :',nel
		write(*,*) 'La restriction 103 est utilisee avec des raidis sa
     *ns semelles !!! Cela peut poser probleme'
		!read(*,*)
	endif
	!
      l1=0

	do ipan=1,neto
	  nbrxi=nvar(ipan,iboat)
	  ij = (ix-1)*51 + panneau(ipan).ipts2(iy1)	!!! (ix-1) au lieu de 4
        do k=1,nbrxi
          kk=nxit(k,ipan,iboat)
          l = l1 + k
          if (ipan.eq.nel) then
	    dc(l) = sens3(1,iy1,kk,ipan)*eff(ij+4335)/(dabs(eff(ij+4335)))  ! = d(sigmax)/dx
	      if (eff(ij+4335)*sig1.ne.0) then
	        den = eff(ij+4335)*sig1
	        dc(l) = dc(l) + dsig1(kk)*den/dabs(den)
	      else
	        dc(l) = dc(l) + dsig1(kk) 
	      endif
          else
	      dc(l) = sens2(1,iy1,kk,ipan)*invv
	    endif
	  enddo
	  l1 = l1 + nbrxi
	enddo
  
	else if(ic.eq.104) then
c     restrictions n� 104: sigmaY cadre jas � mi-port�e (x=l/2)
c     ----------------------------------------------------------------
        vnom2_='c.JAS SY;l/2'

	  ix = 5
	  ij = iy2 + 51*(ix-1)
	  
	  c=dabs(eff(ij+5100))*invv

		! Test si existence de cadres !!! Sinon pas ad�quat d'utiliser cette restriction
	if (panneau(nel).hya.le.0.001) then
		write(*,*) 'Attention ! Panneau :',nel
		write(*,*) 'La restriction 104 est utilisee sans cadres !!!'
		write(*,*) 'Cela peut poser probleme'
		!read(*,*)
	endif

	  j=7
        call csens5(invv,iy1,j,dc,eff(ij+5100))

	if (nel.eq.73) then
		j=j
	endif

	else if(ic.eq.105) then
c     restrictions n� 105: TauYZ cadre jas � mi-port�e (x=l/2)
c     ----------------------------------------------------------------
        vnom2_='c.JAS T; l/2'

	  ix = 5
	  ij = iy2 + 51*(ix-1)
	  
	  c=dabs(eff(ij+5610))*invv

		! Test si existence de cadres !!! Sinon pas ad�quat d'utiliser cette restriction
	if (panneau(nel).hya.le.0.001) then
		write(*,*) 'Attention ! Panneau :',nel
		write(*,*) 'La restriction 105 est utilisee sans cadres !!!'
		write(*,*) 'Cela peut poser probleme'
		!read(*,*)
	endif

	  j=8
        call csens5(invv,iy1,j,dc,eff(ij+5610))

	else if(ic.eq.106) then
c     restrictions n� 106: sigmaY cadre jab � mi-port�e (x=l/2)
c     ----------------------------------------------------------------
        vnom2_='c.JAB SY;l/2'

	  ix = 5
	  ij = iy2 + 51*(ix-1)
	  
	  c=dabs(eff(ij+5355))*invv

		! Test si existence de cadres !!! Sinon pas ad�quat d'utiliser cette restriction
	if (panneau(nel).hya.le.0.001) then
		write(*,*) 'Attention ! Panneau :',nel
		write(*,*) 'La restriction 106 est utilisee sans cadres !!!'
		write(*,*) 'Cela peut poser probleme'
		!read(*,*)
	endif

	  j=9
        call csens5(invv,iy1,j,dc,eff(ij+5355))

	else if(ic.eq.107) then
c     restrictions n� 107: TauYZ cadre jab � mi-port�e (x=l/2)
c     ----------------------------------------------------------------
        vnom2_='c.JAB T; l/2'

	  ix = 5
	  ij = iy2 + 51*(ix-1)
	  
	  c=dabs(eff(ij+5865))*invv

	! Test si existence de cadres !!! Sinon pas ad�quat d'utiliser cette restriction
	if (panneau(nel).hya.le.0.001) then
		write(*,*) 'Attention ! Panneau :',nel
		write(*,*) 'La restriction 107 est utilisee sans cadres !!!'
		write(*,*) 'Cela peut poser probleme'
		!read(*,*)
	endif

	  j=10
        call csens5(invv,iy1,j,dc,eff(ij+5865))

	else if(ic.eq.108) then
c     restrictions n� 108: sigmaY cadre semelle � mi-port�e (x=l/2)
c     ----------------------------------------------------------------
        vnom2_='c.SEM SY;l/2'

	  ix = 5
	  ij = iy2 + 51*(ix-1)
	  
	  c=dabs(eff(ij+6630))*invv

	! Test si existence de cadres !!! Sinon pas ad�quat d'utiliser cette restriction
	if (panneau(nel).hya.le.0.001) then
		write(*,*) 'Attention ! Panneau :',nel
		write(*,*) 'La restriction 108 est utilisee sans cadres !!!'
		write(*,*) 'Cela peut poser probleme'
		!read(*,*)
	endif

	  j=11
        call csens5(invv,iy1,j,dc,eff(ij+6630))

	if (nel.eq.73) then
		j=j
	endif


	else if(ic.eq.109) then
c     restrictions n� 109: TauYZ cadre semelle � mi-port�e (x=l/2)
c     ----------------------------------------------------------------
        vnom2_='c.JSEM T; l/2'

	  ix = 5
	  ij = iy2 + 51*(ix-1)
	  
	  c=dabs(eff(ij+6885))*invv

	! Test si existence de cadres !!! Sinon pas ad�quat d'utiliser cette restriction
	if (panneau(nel).hya.le.0.001) then
		write(*,*) 'Attention ! Panneau :',nel
		write(*,*) 'La restriction 109 est utilisee sans cadres !!!'
		write(*,*) 'Cela peut poser probleme'
		!read(*,*)
	endif

	  j=12
        call csens5(invv,iy1,j,dc,eff(ij+6885))

c     -----------------------------------------------------------
c     -----------------------------------------------------------

	else
        write(*,*)' la restriction n�',ic,' n''existe pas !!!'
        write(*,*)' dans panneau n�',nel,' !!!'
        write(*,*)' veuillez svp corriger les donn�es.'
	  
	  write(iu_14(iboat),*)
     *' la restriction n�',ic,' n''existe pas !!!'					!bug
        write(iu_14(iboat),*)' dans panneau n�',nel,' !!!'									!bug
        write(iu_14(iboat),*)' veuillez svp corriger les donn�es.'							!bug

	  write(*,*) 'stop'
	  read(*,*)
	  stop
	end if

c ***********************************************************************   
c     sauvetage sur disque,  file iu_26(iboat)		!extension neto	!fev2007 
c     -----------------------
	kcontr = kcontr+1
	do i=1,ntot(iboat)
		cijopt(i,kcontr,iboat)=dc(i)
	enddo
	if (kcontr.eq.2434) then
	i=i
	endif

	cjopt(kcontr,iboat) = c
	cjmopt(kcontr,iboat) = cm
	vnom2(kcontr,iboat) = vnom2_
	ic_rest(kcontr,iboat) = ic
	ratio_rest(kcontr,iboat) = ratio
	
!	write(iu_26(iboat),*) c
!	write(iu_26(iboat),*) cm
!	write(iu_26(iboat),*) vnom2_
!	write(iu_26(iboat),*) ic
!	write(iu_26(iboat),*) ratio	!extension neto	!fev2007
!	write(iu_26(iboat),*) (dc(i),i=1,ntot(iboat))		!extension neto	!fev2007
	

c     impressions
c     ------------
c     ir compteur  global de toutes les restrictions (pour la structure compl�te)
      ir=icont+img_+m2tot(iboat)     !is = load case

      if(itera.eq.0) then		!sept06
	if (impr2.ge.-2) then	!sept06
        if(invv.eq.-1) then
          write(iu_31(iboat),427) ir,panneau(nel).cjmax(icont-im),ic,
     *							vnom2_,iy1,iy2
        else
          write(iu_31(iboat),428) ir,panneau(nel).cjmax(icont-im),ic,
     *							vnom2_,iy1,iy2
        endif
      endif
	endif					!sept06


c **********************************************************************

c     fin de la boucle principale sur le nbre de restrictions
   1  continue

      im=im+m   ! increment du compteur des restr structurelles

	deallocate (dc)
      return

  10  write(*,*) 'dans subr. contr : it=0 . impossible -> error'
      write(*,*) 'positions de calcul des restrictions relatives'
      write(*,*) 'aux traverses sont erron�s (subr. contr)'
      write(iu_31(iboat),*)
     * 'dans subr. contr : it=0 . impossible -> error'
      write(iu_31(iboat),*)
     * 'positions de calcul des restrictions relatives'
      write(iu_31(iboat),*)
     * 'aux traverses sont erron�s (subr. contr)'
	
	write(iu_14(iboat),*)
     * 'dans subr. contr : it=0 . impossible -> error'						!bug
      write(iu_14(iboat),*)
     * 'positions de calcul des restrictions relatives'					!bug
      write(iu_14(iboat),*)
     * 'aux traverses sont erron�s (subr. contr)'							!bug

	write(*,*) 'stop'
	read(*,*)
	
	deallocate (dc)

	stop
	
c     ----------------------------------------------------------------- 

  100 format(' rest. n�',i4,' de type ',i2,'; c=',e14.7/'dc=',
     *        (t4,6(e11.4,1x)))
  196 format(/'!!! le calcul de sig ultime (paik)',
     *       ' n''est pas pr�vu pour une coque cylindrique !!!'/
     *       ' mais il est acceptable en 1�re approximation.'/)
  284 format('nombre de restrictions structurelles locales = ',i3/
     *       'aux pts ypt=',5(e11.5,1x),' (deg)')
  427 format('c(',i4,') > ',e15.3,' de type n�',i4,' (',a12,')',
     *       ' au pt ypt n�',i2,' et au pt de calcul n�',i2)
  428 format('c(',i4,') < ',e15.3,' de type n�',i4,' (',a12,')',
     *       ' au pt ypt n�',i2,' et au pt de calcul n�',i2)
      end

************************************************************************
c **********************************************************************

      subroutine csens(invv,iy,j,dc)
      
      use param_section
      

      implicit double precision(a-h,o-z)
      dimension dc(9*neto)
      l1=0
      do 306 ipan=1,neto
        nbrxi=nvar(ipan,iboat)
        do 305 k=1,nbrxi
          kk=nxit(k,ipan,iboat)
          l=l1+k
          dc(l)=sens4(j,iy,kk,ipan)*invv
  305   continue
        l1=l1+nbrxi
  306 continue
      return
      end

************************************************************************

      subroutine csens2(invv,iy,j,dc)
      
      use param_section
      

      implicit double precision(a-h,o-z)
      dimension dc(9*neto)
      l1=0
      do 306 ipan=1,neto
        nbrxi=nvar(ipan,iboat)
        do 305 k=1,nbrxi
          kk=nxit(k,ipan,iboat)
          l=l1+k
          dc(l)=sens2(j,iy,kk,ipan)*invv
  305   continue
        l1=l1+nbrxi
  306 continue
      return
      end

************************************************************************

      subroutine csens3(invv,iy,j,ipt,ic,dc)
      
      use param_section
      

      implicit double precision(a-h,o-z)
      dimension dc(9*neto)
      l1=0
      do 306 ipan=1,neto
        nbrxi=nvar(ipan,iboat)
        do 305 k=1,nbrxi
            kk=nxit(k,ipan,iboat)
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
      
      use param_section
      

      implicit double precision(a-h,o-z)
      dimension dspaik(9),dc(9*neto)
      l1=0
      sp2=spaik*spaik
      do 306 ipan=1,neto
        nbrxi=nvar(ipan,iboat)
        do 305 k=1,nbrxi
          kk=nxit(k,ipan,iboat)
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

	subroutine csens5(invv,iy,j,dc,c)
      
      use param_section
      

      implicit double precision(a-h,o-z)
      dimension dc(9*neto)
      l1=0
      do 306 ipan=1,neto
        nbrxi=nvar(ipan,iboat)
        do 305 k=1,nbrxi
          kk=nxit(k,ipan,iboat)
          l=l1+k
          dc(l)=sens3(j,iy,kk,ipan)*c/(dabs(c))*invv
  305   continue
        l1=l1+nbrxi
  306 continue
      return
      end
