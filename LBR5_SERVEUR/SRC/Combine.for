      subroutine combine(eff,sig1,sig2,sig3,sig4,tau,
     *                   mt,effcomb,txr)
      use param_section

      implicit double precision(a-h,o-z)
     	dimension eff(9690),effcomb(9690)
!	data nom2/'sbord z=0 x=l/2','tbord z=0 x=0','sbord z=-d/2 x=l/2',
!     *         'sbord z=+d/2 x=l/2','tcadre jas x=0','scadre jas x=l/2',
!     *         'tcadre jab x=0','scadre jab x=l/2','scadre sem x=l/2',
!     *         'traid jas x=0' ,'traid jab x=0'   ,'sraid sem x=l/2',
!     *         'ttrav jas x=0' ,'ttrav jab x=0'   ,'strav sem x=l/2'/

c***********************************************************************
c     subroutine combine
c     ==================
c     cette sous-routine est appelée dans bo2.
c     elle combine les résultats donnés par la sous-routine resul avec
c     ceux donnés par la sous-routine stiff.
c
c   inputs : eff,sig1,sig2,sig3,sig4,tau
c            où eff est calculé dans resul
c               sig1,sig2,sig3,sig4 et tau sont calculés dans stiff
c        
c   outputs : effcomb

c  créer: le 15-4-2003  par  f. bair
c
c  modifications: 
c     - 15 avril 2003 : modif en vue de la combinaison des contraintes
c                               
c  dernière modif : 20-5-2003	     	
c
c************************************************************************
c************************************************************************
c************************************************************************

      do i=1,9690
	  effcomb(i) = eff(i)
	end do
c 
c  1) calcul du effcomb
c
      kh = 31 + 2*mt
	do 114 k=1,5  ! boucle sur les 5 points selon x
c
	do 112 i=1,kh ! boucle sur les 31 points selon y
	ij = i + (k-1)*51
c      if (ij .eq. 205) then 
c      oy=1
c      endif
      
c     le bordage
c     **********
c     calcul dans le bordage a mi-epaisseur (z=0)
c     (modification de sigmax, txy et sigma comp)

       
      
      if (eff(ij+4335).ne.0) then
	effcomb(ij+4335) = eff(ij+4335)*(1 + dabs(sig1/eff(ij+4335))) ! sigmax
	else
      effcomb(ij+4335) = sig1 ! quel signe ?
	endif
	effcomb(ij+4845)=dsqrt(effcomb(ij+4080)**2+effcomb(ij+4335)**2 
     *             - effcomb(ij+4080)*effcomb(ij+4335)
     *             + 3.d0*(effcomb(ij+4590)**2))  ! sigma comp             

c     raidisseurs
c     ***********
c     calcul de : sigmax jas, sigmax jab et sigmax semelle
c      effcomb(ij+7395) = eff(ij+7395)  !+ sig4 ! sigmax jas    !!!!!!!attention, changer signe
      if (eff(ij+7395).ne.0) then
	effcomb(ij+7395) = eff(ij+7395)*(1 + dabs(sig4/eff(ij+7395))) ! sigmax jas
	else
      effcomb(ij+7395) = sig4 ! quel signe ?
	endif

      if (k.ne.1) then !!! pas additionner sig3 en x=0 pour que la restriction 33 corresponde à
	                 !!! tau(resul) + tau(stiff) sans sigmajab(stiff)
	if (eff(ij+7650).ne.0) then
	effcomb(ij+7650) = eff(ij+7650)*(1 + 
     *                         dabs(sig3*10./12./eff(ij+7650))) ! sigmax jab
	else
      effcomb(ij+7650) = sig3*10./12. ! quel signe ? ! rem.: *10/12 pour avoir pl²/12 (et non pl²/10)
	endif
	endif

      if (eff(ij+7905).ne.0) then
	effcomb(ij+7905) = eff(ij+7905)*(1 + dabs(sig2/eff(ij+7905))) ! sigmax sem
	else
      effcomb(ij+7905) = sig2 ! quel signe ?
	endif

c     calcul de : txz jas, txz jab et tyx sem 
c     + calcul de : sigma comp jas, sigma comp jab , sigma comp semelle     
                                               
	if (eff(ij+8415).ne.0) then
	effcomb(ij+8415) = eff(ij+8415)*(1 + dabs(tau/eff(ij+8415))) ! txz jab
	else
      effcomb(ij+8415) = tau ! quel signe ?
	endif
	
      effcomb(ij+8925) = dsqrt(effcomb(ij+7395)**2
     *                         + 3.d0*(effcomb(ij+8160)**2))  ! sigma comp jas
      effcomb(ij+9180) = dsqrt(effcomb(ij+7650)**2 
     *                         + 3.d0*(effcomb(ij+8415)**2))	! sigma comp jab
      if(txr.ne.0.) then
       effcomb(ij+8670)= eff(ij+8670) !!!+ ???		! tyx sem
       effcomb(ij+9435)=dsqrt(effcomb(ij+7905)**2
     *                         +3.d0*(effcomb(ij+8670)**2))	! sigma comp semelle
	endif


  112 continue   ! boucle sur les 31+2*mt points (selon y)          

  114 continue  ! boucle sur les 5 coupes (selon x)

      return

c ******************************************************************************
c 	les formats
c 	-----------
  282 format(a18,1x,5(1x,e13.6))
  288 format(' variable de conception nø',i3,' du panneau nø',i3/
     *         50(1h-))
  294 format('     fct.             pt.1          pt.2',
     *     '          pt.3          pt.4          pt.5')

	end
