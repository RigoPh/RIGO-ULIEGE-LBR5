      subroutine flamb(hight,aire,aix,sigy,hya,isect1,
     *		     tya,wya,e,vnymax,vmymax,symax,symin,
     *		     vm1,vm2,epsa)			
      implicit real*8(a-h,o-z)
  
	real*8 aix

c	**************************************************

	pi=acos(-1.d00)

	  if(symin.gt.symax)then
	  sy=symin
	  else
	  sy=symax
	  endif
        hi = hight
	vnymax = epsa*vnymax				!+++
	vmymax = epsa*vmymax				!+++
c	**************************************************
c	if (flam.eq.1) then
c	  flambement selon bureau veritas
	 ax=aix*1.0e+8  !en cm^4
	 sur=aire*1.0e+4  !en cm�
	 sig=sigy*1.0e-6   !en n/mm�
	 wy=wya*1.0e+2   !en cm
	 hy=hya*1.0e+2
	 avm1= epsa*vm1*1.0e-3  !en kn.m
	 avm2=epsa*vm2*1.0e-3  !en kn.m
	 ae=e*1.0e-6   ! en n/mm�
	 euler1= pi*pi*ae*(ax/(sur*(0.5*hi)**2))/10000	!biencastre	
	 euler2= pi*pi*ae*(ax/(sur*(hi)**2))/10000		!biappuye
	 fl1= sig/2											
	 fl2= sig*(1-(sig/(4*euler1)))					
       fl3= sig*(1-(sig/(4*euler2)))					
	 if (euler1.le.fl1) then									
	 cfl1=euler1									
	 else									
	 cfl1=fl2										
	 endif
c	 cflu1=cfl1*1.0e+6	!en n/m�										
       if (euler2.le.fl1) then									
	 cfl2=euler2									
	 else									
	 cfl2=fl3										
	 endif
c	 cflu2=cfl2*1.0e+6			!en n/m�		
	 if(cfl1.ge.sig) then
	 cflu1 = sig
	 endif
	 if(cfl2.ge.sig) then
	 cfl2 = sig
	 endif						
c	 compression pure
	  sol=sy*1.0e-6  !en n/mm�
	  res1=cfl1/(1.02*1.25) 
	  res2=cfl2/(1.02*1.25) 
	 if(vmymax.le.100)then
	 write(66,31) res1,res2,sol   

	  if (res1.le.sol) then								
	  write(66,32) 
	  write(29,32)		!sept06
	 else
	  write(66,33) 
	  endif
	  if (res2.le.sol) then								
	  write(66,34)
	  write(29,34)		!sept06
	 else
	  write(66,35) 
	  endif				
	 else  ! vmymax .gt. 100
c	 flexion compos�e 
	 
	  f= vnymax*1.0e-3	!en kn
	  u1=0.5*pi*dsqrt(f/(euler1*sur))
	  t1=((avm2-avm1)/(avm2+avm1))/dtan(u1)
	  vm01=(0.5*(avm1+avm2)*dsqrt(1+(t1*t1)))/dcos(u1)
	  vmmm1=dmax1(abs(avm1),abs(avm2),abs(vm01))
c	excentricit�
c	ex= excentricit� en cm
c	exc1= 1/(1-(f/(sur*euler1)))
c	exc2= 1/(1-(f/(sur*euler2)))
	  if (isect1.eq.3) then
	  wp=ax*2/wy
	  else
	  wp= ax*2/hy
	  endif
	  solm1=(vmmm1/wp)*1000   !biencastr�
	  sols=10*f*(1/sur)  !en kn/cm�?
	  sol1=sols+solm1	
	  sol2=sols			!+++
	  res=sig/(1.02*1.15) 
		write(66,38) sol1,sol2,res		!+++

	  if (res.le.sol1) then								
	  write(66,32) 
	  write(29,32)			!sept06
	 else
	  write(66,33) 
	  endif
	  if (res.le.sol2) then								
	  write(66,34)
	  write(29,34)			!sept06 
	 else
	  write(66,35) 
	  endif				
	 endif
c	****************************************************************									
c	else    ! cticm
	 aix=aix*1.0e+12		!en mm^4
	 aire= aire*1.0e+6  !en mm�
	 asy=sy*1.0e-6  !n/mm�
	 hi=hi*1000   ! mm
	 rayg= dsqrt(aix/aire)
	 elan1=0.5*hi/rayg		!biencastr�
	 elan2=hi/rayg	    	!biappuy�
	 ee= ae/sig
	 elane= pi*dsqrt(ee)
	 elanb1=elan1/elane
	 elanb2=elan2/elane


	 if(isect1.eq.3)then
	 class=2*(hya+tya)/wya
	 if(class - 1.2) 21,20,20
 20	 alf=0.339
	 goto 22
 21	 alf=0.489
	 goto 22
	 else
	 alf=0.206
	 endif
 22	 continue
	 phi1= 0.5*(1+alf*(elanb1-0.2)+elanb1*elanb1)
	 ckhi1= 1/(phi1+dsqrt(phi1*phi1-elanb1*elanb1))
	 sbrd1= ckhi1*sig/1.1   !r�sistance
	 phi2= 0.5*(1+alf*(elanb2-0.2)+elanb2*elanb2)
	 ckhi2= 1/(phi2+dsqrt(phi2*phi2-elanb2*elanb2))
	 sbrd2= ckhi2*sig/1.1
	if(sbrd1.ge.sig) then
	sbrd1= sig
	endif
	if(sbrd2.ge.sig) then
	sbrd2= sig
	endif

	 if(vmymax.ge.100)then
c	 flambement en flexion compos�e selon le cticm (eurocode3)
	 cond1=(vnymax*1.1)/(aire*ckhi1*sig)
	 cond2=(vnymax*1.1)/(aire*ckhi2*sig)
       if (((elanb1.lt.0.2).or.(cond1.lt.0.1)).and.((elanb2.lt.0.2).or.
     *            (cond2.lt.0.1))) then
       write(66,37) 
	 goto 23
	 else if ((elanb2.gt.0.2).and.(cond2.gt.0.1))then		!biappuy�
			if((elanb1.lt.0.2).or.(cond1.lt.0.1)) then		!biencastr� ok
			write(66,40)
			write(29,40)			!sept06 
	 	 write(66,36) sbrd1,sbrd2,asy		!en n/mm�

			goto 24
			else if ((elanb1.gt.0.2).and.(cond1.gt.0.1)) then !biencastr� ko
			 write(66,39)
			 write(29,39)			!sept06 
	 	 write(66,36) sbrd1,sbrd2,asy		!en n/mm�

			goto 25
			endif
		 else if ((elanb1.gt.0.2).and.(cond1.gt.0.1).and.
     *  (elanb2.lt.0.2).and.(cond2.lt.0.1)) then		!biappuy�
		write(66,*) 'bizarre, flamb biencastr� et pas biappuy�'
	 	 write(66,36) sbrd1,sbrd2,asy		!en n/mm�


	 endif
	endif
 
 	 write(66,36) sbrd1,sbrd2,asy		!en n/mm�
c	 flambement en compression selon le cticm (eurocode3)
  25	 if(sbrd1.le.asy)then
	  write(66,32) 
	  write(29,32)			!sept06
	 else
	  write(66,33) 
	endif

  24	 if(sbrd2.le.asy)then
	  write(66,34)
	  write(29,34)			!sept06 
	 else
	  write(66,35) 
	 endif
  23   continue	
c	endif
c***********************************************************************
c***********************************************************************
c	  les formats
c     ------------
  31	format(/'selon le bureau veritas'/
     *		'la r�sistance au flambement si biencastr� vaut',e11.4,
     *'n/mm�'/
     *		'la r�sistance au flambement si biappuy� vaut',e11.4,
     *'n/mm�'/
     *		'et la sollicitation appliqu�e vaut',e11.4,'n/mm�')
 32   format('donc on aura flambement dans le cas biencastr�')
 33   format('donc le flambement n''est pas � craindre dans le cas'
     *				'biencastr�')
 34   format('donc on aura flambement dans le cas biappuy�'/)
 35   format('donc le flambement n''est pas � craindre dans le cas' 
     *				'biappuy�'/)
  36	format(/'selon le cticm (eurocode3)'/
     *		'la r�sistance au flambement si biencastr� vaut',e11.4,
     *'n/mm�'/
     *		'la r�sistance au flambement si biappuy� vaut',e11.4,
     *'n/mm�'/
     *		'et la sollicitation appliqu�e vaut',e11.4,'n/mm�')
 37   format(/'selon le cticm (eurocode3)'/
     *		' il n''est pas n�cessaire de calculer le flambement'
     *		' dans ce cas de flexion compos�e.'/) 
  38	format(/'selon le bureau veritas'/
     *		'la sollicitation � consid�rer si biencastr� vaut',e11.4,
     *'n/mm�'/
     *		'et la sollicitation appliqu�e si biappuy�e vaut',e11.4,
     *'n/mm�'/
     *		'et la r�sistance au flambement vaut',e11.4,'n/mm�'/)
  39	format(/'selon le cticm,'/
     *		'les conditions sont remplies, il y a risque de 
     * flambement'/
     *		'le flambement est calcul� comme dans le cas de la 
     * compression pure')
  40  format(/'selon le cticm,'/
     *		'les conditions sont remplies, il y a risque de 
     * flambement pour le cas biappuy�'/
     *		'le flambement est calcul� comme dans le cas de la 
     * compression pure')
	return
	end
