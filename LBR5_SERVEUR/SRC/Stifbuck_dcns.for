      subroutine stifbuck_dcns(nel,e1,sigy1,				
     *					epsa,epsr,delta,hxr,dxr,wxr,txr,	
     *					dsigr,ddsigr,ntot_)

      
	use param_section
!      use param_cout
!      use param_opti_local
      

      implicit double precision (a-h,o-z)

	double precision ia,mstat

	dimension ddsigr(ntot_)
     
      dimension daire(9),dmstat(9),daxen(9),dia(9)	!longitudinals
	!dimension dsigestif(9,neto)

	double precision, allocatable, save :: dsigestif(:,:)
	
	allocate (dsigestif(9,neto))

c	*****************************************************************************************************



c	elastic buckling of longitudinals
c	---------------------------------
	

   	call annuld(daire,9)
	call annuld(dmstat,9)
	call annuld(daxen,9)
	call annuld(dia,9)
	call annuld(dsigestif,9*neto)
	call annuld(ddsigr,ntot_)

	
	if((signet(1).ge.0).and.(signet(2).ge.0)) then
	  dsigr=0.
	  deallocate (dsigestif)
	  return		
	else
	  if((-signet(1)).ge.(-signet(2))) then
	    siga=-signet(1)
	  else
	    siga=-signet(2)
	  endif

c	column buckling
c	---------------

	  aire=delta*(40*delta)+hxr*dxr+wxr*txr
	  daire(1)=80*delta
	  daire(6)=dxr
	  daire(7)=hxr
	  daire(8)=txr
!	  daire(9)=delta

!	  mstat=delta*epsr*delta/2.+hxr*dxr*(hxr/2.+delta)+
!     *	    wxr*txr*(txr/2.+hxr+delta)

	  mstat=delta*(40*delta)*delta/2.+hxr*dxr*(hxr/2.+delta)+
     *	    wxr*txr*(txr/2.+hxr+delta)

!	  dmstat(1)=epsr*delta+hxr*dxr+wxr*txr
	  dmstat(1)=60*delta**2+hxr*dxr+wxr*txr
	  dmstat(6)=dxr*(hxr/2.+delta)+0.5*hxr*dxr+wxr*txr
	  dmstat(7)=hxr*(hxr/2.+delta)
	  dmstat(8)=txr*(txr/2.+hxr+delta)
!	  dmstat(9)=delta**2/2.

	  axen=mstat/aire
	  do i=1,9
	    daxen(i)=(dmstat(i)*aire-mstat*daire(i))/aire**2
	  enddo

!	  ia=epsr*delta**3/12.+delta*epsr*(delta/2.-axen)**2+
!     *	 dxr*hxr**3/12.+hxr*dxr*(hxr/2.+delta-axen)**2+
!     *	 wxr*txr**3/12.+wxr*txr*(txr/2.+hxr+delta-axen)**2



	  ia=(40*delta)*delta**3/12.+delta*(40*delta)*(delta/2.-axen)**2+
     *	 dxr*hxr**3/12.+hxr*dxr*(hxr/2.+delta-axen)**2+
     *	 wxr*txr**3/12.+wxr*txr*(txr/2.+hxr+delta-axen)**2



!	  dia(1)=epsr*delta**2/4.+epsr*(delta/2.-axen)**2+
!     *		 2.*delta*epsr*(delta/2.-axen)*(0.5-daxen(1))+
!     *		 2.*hxr*dxr*(hxr/2.+delta-axen)*(1.-daxen(1))+
!     *		 2.*wxr*txr*(txr/2.+hxr+delta-axen)*(1.-daxen(1))

	  dia(1)=40.*delta**3/3.+80.*delta*(delta/2.-axen)**2+
     *		 (80.*delta**2)*(delta/2.-axen)*(0.5-daxen(1))+
     *		 2.*hxr*dxr*(hxr/2.+delta-axen)*(1.-daxen(1))+
     *		 2.*wxr*txr*(txr/2.+hxr+delta-axen)*(1.-daxen(1))


	  dia(6)=2.*delta*epsr*(delta/2.-axen)*(-daxen(6))+
     *		 dxr*hxr**2/4.+dxr*(hxr/2.+delta-axen)**2+
     *		 2.*hxr*dxr*(hxr/2.+delta-axen)*(0.5-daxen(6))+
     *		 2.*wxr*txr*(txr/2.+hxr+delta-axen)*(1.-daxen(6))
	  dia(7)=2.*delta*epsr*(delta/2.-axen)*(-daxen(7))+
     *		 hxr**3/12.+hxr*(hxr/2.+delta-axen)**2+
     *		 2.*hxr*dxr*(hxr/2.+delta-axen)*(-daxen(7))+
     *		 2.*wxr*txr*(txr/2.+hxr+delta-axen)*(-daxen(7))
	  dia(8)=2.*delta*epsr*(delta/2.-axen)*(-daxen(8))+
     *		 2.*hxr*dxr*(hxr/2.+delta-axen)*(-daxen(8))+
     *		 txr**3/12.+txr*(txr/2.+hxr+delta-axen)**2+
     *		 wxr*txr*(txr/2.+hxr+delta-axen)*(-daxen(8))
!	  dia(9)=delta**3/12.+delta*(delta/2.-axen)**2+
!     *		 2.*delta*epsr*(delta/2.-axen)*(-daxen(9))+
!     *		 2.*hxr*dxr*(hxr/2.+delta-axen)*(-daxen(9))+
!     *		 2.*wxr*txr*(txr/2.+hxr+delta-axen)*(-daxen(9))

	  sigestif=10.*e1*ia/(aire*epsa**2)


	  do i=1,9
	    dsigestif(i,nel)=10.*e1/epsa**2*(dia(i)*aire-ia*daire(i))/
     *				     aire**2
	  enddo
	  dsigestif(5,nel)=-2.*10.*e1*ia/aire*(1./epsa**3)

!	  beta=1.1
	  if(sigestif.le.sigy1/2.) then
	    sigc=sigestif
	  else
	    sigc=sigy1*(1-0.25*sigy1/sigestif)
	  endif




   	  dsigr=siga/sigc
	  l1=0
	  do ipan=1,neto
	    nbrxi=nvar(ipan,iboat)
		do l=1,nbrxi
	      ll=nxit(l,ipan,iboat)
		  if(sigestif.le.sigy1/2.) then
			  if((-signet(1)).ge.(-signet(2))) then
			    ddsigr(l1+l)=(-dsignet(1,ll,ipan)*sigc-siga*
     *						 dsigestif(ll,ipan))/sigc**2
	          else
			    ddsigr(l1+l)=(-dsignet(2,ll,ipan)*sigc-siga*
     *						 dsigestif(ll,ipan))/sigc**2
			  endif
	      else
			  if((-signet(1)).ge.(-signet(2))) then
		        ddsigr(l1+l)=(-dsignet(1,ll,ipan)*sigc-siga*
     *						 sigy1*(0.25*sigy1*dsigestif(ll,ipan)/
     *						 sigestif**2))/sigc**2							   
			  else
			    ddsigr(l1+l)=(-dsignet(2,ll,ipan)*sigc-siga*
     *						 sigy1*(0.25*sigy1*dsigestif(ll,ipan)/
     *						 sigestif**2))/sigc**2							   
			  endif
		  endif
		enddo
		l1=l1+nbrxi
	  enddo


	endif


    3 format(i3,39x,f8.3,3x,f6.3,6x,'stiffener')
    4 format(i3,4x,f8.3,3x,f8.3,3x,f8.3,5x,f8.3,3x,f6.3,6x,'stiffener')      

	deallocate (dsigestif)

      return
      end
