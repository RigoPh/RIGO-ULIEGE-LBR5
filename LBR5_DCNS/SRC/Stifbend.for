      subroutine stifbend(nel,epsa,epsr,delta,hxr,dxr,wxr,txr,
     *					epsa2,is,nsol,sig,dsig)

	use sharedvar

      implicit real*8 (a-h,o-z)
	real*8 iner

	dimension xi(nsol),xf(nsol)

	dimension dsect(9),dstat(9),daneu(9),diner(9),dsmod(9),dplin(9)

	dimension dsig(9)
	
c*********************************************************************************

	read(99) abtr,phil,mt,teta,xi,xf

	if(hxr.le.0.01) then
	  sig=0.
	  call annuld(dsig,9)
	  return
	endif

c	module de résistance i/v
c	------------------------
	sect=delta*epsr+hxr*dxr+wxr*txr
	stat=(delta*epsr)*(delta/2.+hxr+txr)+(hxr*dxr)*(hxr/2.+txr)+
     *	 (wxr*txr)*(txr/2.)
	aneu=stat/sect
	iner=epsr*delta**3/12.+(delta*epsr)*(delta/2.+hxr+txr-aneu)**2+
     *	 dxr*hxr**3/12.+(hxr*dxr)*(hxr/2.+txr-aneu)**2+
     *	 wxr*txr**3/12.+(wxr*txr)*(txr/2.-aneu)**2
	smod=iner/aneu

c	sensibilités du module de résistance i/v
c	----------------------------------------
	if(iopti.ge.1) then
	  call annuld(dsect,9)
	  dsect(1)=epsr
	  dsect(6)=dxr
	  dsect(7)=hxr
	  dsect(8)=txr
	  dsect(9)=delta
	  call annuld(dstat,9)
	  dstat(1)=epsr*(delta/2.+hxr+txr)+(delta*epsr)*0.5
	  dstat(6)=(delta*epsr)*1.+dxr*(hxr/2.+txr)+(hxr*dxr)*0.5
	  dstat(7)=hxr*(hxr/2.+txr)
	  dstat(8)=txr*(txr/2.)
	  dstat(9)=delta*(delta/2.+hxr+txr)
	  call annuld(daneu,9)
	  do i=1,9
	    daneu(i)=(dstat(i)*sect-stat*dsect(i))/sect**2
	  enddo
	  call annuld(diner,9)
	  diner(1)=3.*epsr*delta**2/12.+
     *           epsr*(delta/2.+hxr+txr-aneu)**2+
     *		   (delta*epsr)*2.*(delta/2.+hxr+txr-aneu)*(0.5-daneu(1))+
     *		   (hxr*dxr)*2.*(hxr/2.+txr-aneu)*(-daneu(1))+
     *		   (wxr*txr)*2.*(txr/2.-aneu)*(-daneu(1))
	  diner(6)=(delta*epsr)*2.*(delta/2.+hxr+txr-aneu)*(1.-daneu(6))+
     *		   3.*dxr*hxr**2/12.+
     *		   dxr*(hxr/2.+txr-aneu)**2+
     *	       (hxr*dxr)*2.*(hxr/2.+txr-aneu)*(0.5-daneu(6))+
     *		   (wxr*txr)*2.*(txr/2.-aneu)*(-daneu(6))
	  diner(7)=(delta*epsr)*2.*(delta/2.+hxr+txr-aneu)*(-daneu(7))+
     *		   hxr**3/12.+hxr*(hxr/2.+txr-aneu)**2+
     *		   (hxr*dxr)*2.*(hxr/2.+txr-aneu)*(-daneu(7))+
     *		   (wxr*txr)*2.*(txr/2.-aneu)*(-daneu(7))
	  diner(8)=(delta*epsr)*2.*(delta/2.+hxr+txr-aneu)*(-daneu(8))+
     *		   (hxr*dxr)*2.*(hxr/2.+txr-aneu)*(-daneu(8))+
     *		   txr**3/12.+txr*(txr/2.-aneu)**2+
     *		   (wxr*txr)*2.*(txr/2.-aneu)*(-daneu(8))
        diner(9)=delta**3/12.+delta*(delta/2.+hxr+txr-aneu)**2+
     *		   (delta*epsr)*2.*(delta/2.+hxr+txr-aneu)*(-daneu(9))+
     *		   (hxr*dxr)*2.*(hxr/2.+txr-aneu)*(-daneu(9))+
     *		   (wxr*txr)*2.*(txr/2.-aneu)*(-daneu(9))
	  call annuld(dsmod,9)
	  do i=1,9
	    dsmod(i)=(diner(i)*aneu-iner*daneu(i))/aneu**2
	  enddo
	endif

c	contraintes normales dues à la flexion locale des lisses
c	--------------------------------------------------------
	if(dabs(xi(is)).ge.dabs(xf(is))) then
c	  pmax=dabs(xi(is))*9.81e3
	pmax=dabs(xi(is))*10.e3
	else
c	  pmax=dabs(xf(is))*9.81e3
	pmax=dabs(xf(is))*10.e3
	endif

	if(epsa2.ge.(0.00001)) then
	  if(epsa2.lt.epsa) then
	    plin=pmax*(1.-epsr/(2.*epsa2))*epsr
		sig=plin*epsa2**2/(12.*smod)
	    goto 1
	  endif
	endif
	plin=pmax*(1.-epsr/(2.*epsa))*epsr
	sig=plin*epsa**2/(12.*smod)

    1	continue

c	sensibilités des contraintes normales dues à la flexion locale des lisses
c	-------------------------------------------------------------------------
	if(iopti.ge.1) then
	  call annuld(dplin,9)
	  if(epsa2.ge.(0.00001)) then
	    if(epsa2.lt.epsa) then
	      dplin(9)=pmax*((-1./(2.*epsa2))*epsr+(1.-epsr/(2.*epsa2)))
		  do i=1,9
		    dsig(i)=(epsa2**2/12.)*
     *				((dplin(i)*smod-plin*dsmod(i))/smod**2)
		  enddo
	      goto 2
	    endif
	  endif
	  dplin(5)=pmax*epsr*epsr/(2*epsa**2)
	  dplin(9)=pmax*((-1./(2.*epsa))*epsr+(1.-epsr/(2.*epsa))*1.)
	  do i=1,9
	    dsig(i)=(epsa**2/12.)*
     *			((dplin(i)*smod-plin*dsmod(i))/smod**2)
	    if(i.eq.5) dsig(i)=dsig(i)+(2.*epsa/12.)*(plin/smod)
	  enddo
    2	  continue
	endif

c	*****************************************************************************************************

      return
      end
