      subroutine plaque(epsa,epsr,entr,delta,dsig1,nel,
     *                  hya,hxr,width,phil,q,
     *                  effcomb,chamax,dchamax,ploc1,e1,eta1,nbrxi,sig1,
     *                  eff,ipt,sigplaque,dsigplaque,sigxplaque,
     *                  sigyplaque,phiplaque,sigvmtplaque,sigvmcplaque,
     *                  indplaque,sigx,siggy)
      use sharedvar
	implicit real*8(a-h,o-z)
      dimension dsig1(9),eff(9690),effcomb(9690),chamax(5),
     *          dchamax(5,9),
     *          dsigplaque(iptmax,9),dsigmag(9),dsgmem1(9),dsgmem2(9),
     *          dsig1t(9),dsig2t(9),dsig1c(9),dsig2c(9),
     *          sigplaque(iptmax)
	dimension dgload(9),dprsabs(9),dsigben(9)							!!!aout04
	dimension pbcoef1(2),sigmem(2),sigvm(2),loop1(2),sgmem1(2),
     *          sgmem2(2),sig1t(2),sig2t(2),sig1c(2),sig2c(2),
     *          sigvmt(2),sigvmc(2),sigmag(2),sigben(2),skben(2),
     *          dsigmem(2),phi(2)
      dimension sigxplaque(iptmax),sigyplaque(iptmax),phiplaque(iptmax),
     *          sigvmtplaque(iptmax),sigvmcplaque(iptmax),
     *          indplaque(iptmax),sigx(iptmax),siggy(iptmax)

c***********************************************************************
c     subroutine plaque
c     ==================
c     cette sous-routine est appelée dans bo2.
c     elle calcule la valeur de la contrainte "plaque" aux différents points
c     ipt ainsi que ses sensibilités, pour le panneau nel.
c     
c
c   inputs  : 

c   outputs : sigplaque(iptmax)    = contrainte "plaque"
c             dsigplaque(iptmax,9) = vecteur contenant les sensibilités de sigplaque

c  
c
c************************************************************************
c************************************************************************
c************************************************************************


      call annuld(dsigplaque,9*iptmax)
	call annuld(sigplaque,iptmax)
	call annuld(dsigmag,9)
	call annuld(dsgmem1,9)
	call annuld(dsgmem2,9)
	call annuld(dsig1t,9)
	call annuld(dsig2t,9)
	call annuld(dsig1c,9)
	call annuld(dsig2c,9)

	pbcoef1(1) = 0.19
	pbcoef1(2) = 0.03
      piq = pi*q/180.0

      tpl = delta
	
      mt=0        ! pas declare avant !!!

c     détermination de xlen,ylen, les dimensions (a et b) du panneau
c     --------------------------------------------------------------

      xlen = epsa
	ylen = entr

      
      if((hya/delta).le.2) then
        xlen=width
      else
        if(epsa.gt.width) then
          xlen=width
        endif
      endif

      haut=q*phil*pi/180.
      if((hxr/delta).le.2) then
        ylen=haut

        if(mt.gt.0) then
          ya=0.
          y1=abtr(1)*piq
          yt=y1-ya
          ylen=yt
          do 2 i=1,mt-1
             y1=abtr(i)*piq
             y2=abtr(i+1)*piq
             yt=y2-y1
             if(ylen.lt.yt) ylen=yt
   2      continue
          ymt=abtr(mt)*piq
          yb=haut
          yt=yb-ymt
          if(ylen.lt.yt) ylen=yt
        endif
      else
        if(entr.gt.haut) then
          ylen=haut
        endif
      endif

      
	if (xlen.gt.ylen) then
	  rlong = xlen
	  short = ylen
	else
	  rlong = ylen
	  short = xlen
	endif

      do 1 ind1=1,ipt
	ind2 = ipts2(ind1,nel)

c     détermination des contraintes nécessaires au calcul de sigplaque
c     ----------------------------------------------------------------
      
	if (xlen.gt.ylen) then
	  sgshrt = effcomb(4080+4*51+ind2)
	  sglong = effcomb(4335+4*51+ind2)
	else
	  sgshrt = effcomb(4335+4*51+ind2)
	  sglong = effcomb(4080+4*51+ind2)
	endif
	tau = 0

c     détermination de la pression nécessaire au calcul de sigplaque
c     --------------------------------------------------------------

      if (ind2.le.11) then
	 gload = (chamax(2) - chamax(1))/(11 - 1)*(ind2 - 1) + chamax(1)
	 do k=1,9															!!!aout04
	   dgload(k) = (dchamax(2,k)-dchamax(1,k))/(11-1)*(ind2-1)			                        !!!aout04
     *               + dchamax(1,k)										            !!!aout04
	 enddo														      !!!aout04
	else if (ind2.le.16) then
       gload = (chamax(3) - chamax(2))/(16 - 11)*(ind2 - 11) + chamax(2)
	 do k=1,9															!!!aout04
	   dgload(k) = (dchamax(3,k)-dchamax(2,k))/(16-11)*(ind2-1)			                        !!!aout04
     *               + dchamax(2,k)										            !!!aout04
	 enddo
	else if (ind2.le.21) then
	 gload = (chamax(4) - chamax(3))/(21 - 16)*(ind2 - 16) + chamax(3)
	 do k=1,9															!!!aout04
	   dgload(k) = (dchamax(4,k)-dchamax(3,k))/(21-16)*(ind2-1)			                        !!!aout04
     *               + dchamax(3,k)										            !!!aout04
	 enddo
	else if (ind2.le.31) then
	 gload = (chamax(5) - chamax(4))/(31 - 21)*(ind2 - 21) + chamax(4)
	 do k=1,9															!!!aout04
	   dgload(k) = (dchamax(5,k)-dchamax(4,k))/(31-21)*(ind2-1)			                        !!!aout04
     *               + dchamax(4,k)										            !!!aout04
	 enddo
	endif
      prsabs = dabs(gload)
	signe=1.															!!!aout04
	if (gload.lt.0) signe=-1.											      !!!aout04
	if (prsabs.lt.dabs(ploc1)) then										      !!!aout04
	  prsabs = dabs(ploc1)												      !!!aout04
c	else																!!!aout04
	  signe=0.															!!!aout04
	endif																!!!aout04
	prsabs = prsabs*9810.0 ! (n/m²)

	do k=1,9															!!!aout04
	  dprsabs(k)=signe*dgload(k)*9810.0									            !!!aout04
	enddo																!!!aout04

c     calcul du sigplaque
c     -------------------

c     (iloop= 1 flexion selon x et iloop=2 flexion selon y)
c     on va prendre la contrainte la plus grande des 2
      do iloop=1,2
	

	if (ylen.lt.xlen) then
	  iloop2 = iloop
	else
	  iloop2 = 3 - iloop
	endif	

      sigmem(1) = sgshrt
      sigmem(2) = sglong
      sgmem1(iloop) = sigmem(iloop2)
      sgmem2(iloop) = sigmem(3-iloop2)
      bovera = short/rlong
      arplt = 1.0/bovera
      bot2 = (short/tpl)**2
      skben(iloop) = 0.31 + pbcoef1(iloop2)*tanh(2.*iloop2*(arplt-1.))
      sigben(iloop) = skben(iloop)*prsabs*bot2
      phi(iloop) = 1.0

      if (iloop2 .eq. 2) go to 24  ! skip; no magnification (phi=1) as bending in the long direction.

      if (sgshrt .ge. 0.) go to 24  ! skip; no membrane stress in the short direction ==> no need for ampllification

      skwide = 0.96*(0.9 + 1.8*bovera**2)**2
      sgshcr = skwide*e1/bot2

      omega = -sgshrt/sgshcr
      xipanl = 0.5*pi*sqrt(abs(omega))
      xizero = (1.0 + 0.35*bovera) * xipanl
      if (xizero .lt. 0.02) then
         xzero2 = xizero*xizero
         xzero4 = xzero2*xzero2
         phi(iloop) = (1. + 0.4*xzero2 + 17./105.*xzero4) /
     *         (1. + xzero2/3.0 + 2.0/15.0*xzero4)
      else if (xizero .le. 3.10) then
         phi(iloop) = 3. * ( tan(xizero) - xizero ) /
     *                    ( xizero**2 * tan(xizero) )
      else
         phi(iloop) = 2.5
      end if

  24  sigmag(iloop) = phi(iloop)*sigben(iloop)

c  tensile (t) bending stress:
c     stress in primary (1) bending direction
      sig1t(iloop) = sgmem1(iloop) + sigmag(iloop)
c     stress in secondary (2) direction (with poisson's ratio bending stress)
      sig2t(iloop) = sgmem2(iloop) + eta1*sigmag(iloop)
      sigvmt(iloop) = sqrt(sig1t(iloop)**2 + sig2t(iloop)**2 
     *                   + 3.*tau**2 - sig1t(iloop)*sig2t(iloop))
c
c  compressive (c) bending stress
c     stress in primary (1) bending direction
      sig1c(iloop) = sgmem1(iloop) - sigmag(iloop)
c     stress in secondary (2) direction (with poisson's ratio bending stress)
      sig2c(iloop) = sgmem2(iloop) - eta1*sigmag(iloop)
      sigvmc(iloop) = sqrt(sig1c(iloop)**2 + sig2c(iloop)**2 
     *                  + 3.*tau**2 - sig1c(iloop)*sig2c(iloop))
      
	if (sigvmt(iloop).gt.sigvmc(iloop)) then
        sigvm(iloop) = sigvmt(iloop)
	  loop1(iloop) = 1      !loop1 = 1 -> traction; = 2 -> compression
      else
        sigvm(iloop) = sigvmc(iloop)
	  loop1(iloop) = 2
      endif

      enddo

	if (sigvm(1).gt.sigvm(2)) then
	  l = 1  ! indice l correspond au cas où la contrainte est la plus sévère
	else
	  l = 2
	endif
	sigplaque(ind1) = sigvm(l)

	
c     enregistrement de certaines valeurs à afficher dans sol2
c     --------------------------------------------------------

      if (ind2.ne.0) then
      if (l.eq.1) then
	  sigxplaque(ind1) = eta1*sigmag(l)
	  sigyplaque(ind1) = sigmag(l)
	else
        sigyplaque(ind1) = eta1*sigmag(l)
	  sigxplaque(ind1) = sigmag(l)
	endif
	phiplaque(ind1) = phi(l)
	sigvmtplaque(ind1) = sigvmt(l)
	sigvmcplaque(ind1) = sigvmc(l)
	indplaque(ind1) = ind2
	endif
	sigx(ind1) = effcomb(4335+4*51+ind2)
	siggy(ind1) = effcomb(4080+4*51+ind2)

c     calcul du dsigplaque
c     --------------------


	if (ylen.lt.xlen) then
	  l2 = l
	else
	  l2 = 3 - l
	endif

c      nbrxi=nvar(nel)

      do 100 kk=1,nbrxi

      k=nxit(kk,nel)

      
	ij=4*51 + ind2  
      
	
	
	dsigmax = sens3(1,ind1,k,nel)  ! = d(sigmax)/dx
      if (eff(ij+4335)*sig1.ne.0) then
	  den = eff(ij+4335)*sig1
	  dsigmax = dsigmax + dsig1(k)*den/dabs(den)
	else
	  dsigmax = dsigmax + dsig1(k) 
	endif
      
	dsigmay = sens3(2,ind1,k,nel)  ! = d(sigmay)/dx
	
	if (xlen.gt.ylen) then
	  dsgshrt = dsigmay
	  dsglong = dsigmax
	else
	  dsgshrt = dsigmax
	  dsglong = dsigmay
	endif

c        calcul de dsigmag
      
      if (k.eq.1) then        ! epaisseur (tpl)
        darplt = 0
	  dbot2 = -2.*short**2/tpl**3
	  dbovera = 0
	else if (k.eq.2) then   ! hauteur âme cadre
        darplt = 0
	  dbot2 = 0
	  dbovera = 0
	else if (k.eq.3) then   ! epaisseur âme cadre
        darplt = 0
	  dbot2 = 0
	  dbovera = 0 
	else if (k.eq.4) then   ! largeur semelle cadre
        darplt = 0
	  dbot2 = 0
	  dbovera = 0
	else if (k.eq.5) then   ! ecartement des cadres (epsa=rlong)
        if (hya/tpl.le.2.or.epsa.gt.width) then
          darplt = 0
	    dbot2 = 0
	    dbovera = 0
	  else
          if (xlen.gt.ylen) then
	      darplt = 1/short
	      dbot2 = 0
	      dbovera = -short/rlong**2
	    else
	      darplt = -rlong/short**2
	      dbot2 = 2.*short/tpl**2
	      dbovera = 1./rlong
	    endif
	  endif
	else if (k.eq.6) then   ! hauteur âme raidisseur
        darplt = 0
	  dbot2 = 0
	  dbovera = 0
	else if (k.eq.7) then   ! epaisseur âme raidisseur
        darplt = 0
	  dbot2 = 0
	  dbovera = 0
	else if (k.eq.8) then   ! largeur semelle raidisseur
        darplt = 0
	  dbot2 = 0
	  dbovera = 0
	else if (k.eq.9) then   ! ecartement des raidisseurs (epsr diff. de entr)
	  if ((hxr/delta).le.2) then
          darplt = 0
	    dbot2 = 0
	    dbovera = 0
	  else
	    if (xlen.gt.ylen) then
		  darplt = -rlong/epsr**2    ! car d(c)/d(epsr)= d(c)/d(entr)*(entr/epsr)**2
	      dbot2 = 2.*short**3/(tpl*epsr)**2
	      dbovera = 1./rlong*(short/epsr)**2
	    else
	      darplt = entr**2/(short*epsr**2)
	      dbot2 = 0
	      dbovera = -short/epsr**2
	    endif
	  endif
	endif
      
      dskwide = 0.96*2*(0.9 + 1.8*bovera**2)*1.8*2*bovera*dbovera
      
      dsgshcr = e1*(dskwide*bot2 - skwide*dbot2)/bot2**2
      
	
	domega = -(sgshcr*dsgshrt - sgshrt*dsgshcr)/sgshcr**2
	if (omega.lt.0) then
	  dxipanl = -0.5*pi*domega/(2*sqrt(abs(omega)))  
	else
        dxipanl = 0.5*pi*domega/(2*sqrt(abs(omega)))  
	endif
      
      dxizero = 0.35*dbovera*xipanl + (1.0 + 0.35*bovera)*dxipanl

      if (l2.eq.1.and.sgshrt.lt.0) then
        if (xizero .lt. 0.02) then
          dphi = ((0.8*xizero*dxizero + 68./105.*xizero**3*dxizero)*
     *         (1. + xizero**2/3 + 2./15.*xizero**4) 
     *       - (1. + 0.4*xizero**2 + 17./105.*xizero**4)*
     *         (2./3.*xizero*dxizero + 8./15.*xizero**3*dxizero))
     *        /(1. + xizero**2./3. + 2./15.*xizero**4)**2
	  else if (xizero .le. 3.10) then
          dphi = 3.*((dxizero*(1 + (tan(xizero))**2) - dxizero)
     *                                         *(xizero**2*tan(xizero)) 
     *           - (tan(xizero) - xizero)*(2*xizero*dxizero*tan(xizero) 
     *                     + xizero**2*dxizero*(1. + (tan(xizero))**2)))
     *        /(xizero**2*tan(xizero))**2
        else
           dphi = 0
        endif
	else
	  dphi = 0
	endif

      dskben = pbcoef1(l2)*(1 - (tanh(2.*l2*(arplt-1.)))**2)*2*l2*darplt

      dsigben(k) = dskben*prsabs*bot2 + skben(l)*prsabs*dbot2				!!!aout04
     *			+ dprsabs(k)*skben(l)*bot2						!!!aout04

      dsigmag(k) = phi(l)*dsigben(k) + sigben(l)*dphi						!!!aout04

c        fin calcul de dsigmag

      
      dsigmem(1) = dsgshrt
      dsigmem(2) = dsglong

      dsgmem1(k) = dsigmem(l2)
	dsgmem2(k) = dsigmem(3 - l2)

      if (loop1(l).eq.1) then        ! la traction est le cas plus restrictif
        dsig1t(k) = dsgmem1(k) + dsigmag(k)
	  dsig2t(k) = dsgmem2(k) + eta1*dsigmag(k)

	 dsigplaque(ind1,k) = (2*sig1t(l)*dsig1t(k) + 2*sig2t(l)*dsig2t(k) 
     *    - sig1t(l)*dsig2t(k) - sig2t(l)*dsig1t(k)) /(2.*sigvmt(l))      
	else                              ! la compression est le cas plus restrictif
        dsig1c(k) = dsgmem1(k) - dsigmag(k)
	  dsig2c(k) = dsgmem2(k) - eta1*dsigmag(k)

	 dsigplaque(ind1,k) = (2*sig1c(l)*dsig1c(k) + 2*sig2c(l)*dsig2c(k) 
     *    - sig1c(l)*dsig2c(k) - sig2c(l)*dsig1c(k)) /(2.*sigvmc(l))
	endif


  100 continue ! fin de la boucle sur les variables de conception

  1   continue ! fin de la boucle sur le nombre de points de calcul des sensibilités

	return
	end