      subroutine participation(nsol)
      use sharedvar
      implicit real*8 (a-h,o-z)
      dimension coef1(neto,8),sol(20)
	dimension b1(400),zb(360)

c*********************************************************************************
	rewind 97
	
	ysup1=yneut
	ysup2=yneut
	  
	ymax=max(z(1,3),z(1,4))
	iymax=1
	ymin=min(z(1,3),z(1,4))
	iymin=1
	do nel=2,neto
	  do k=3,4
	    if(z(nel,k).gt.ymax) then
	      ymax=z(nel,k)
	      iymax=nel
	    endif
	    if(z(nel,k).lt.ymin) then
	      ymin=z(nel,k)
	      iymin=nel
	    endif
	  enddo
	enddo

	phi=-philn(iymax)
      q=qn(iymax)
	qphi=phi*q*pi/180.
      qphi2=qphi*qphi
      qphi3=qphi2*qphi
	
	 
	do is=1,nsol
	  forctsup1=0.
	  forctsup2=0.	
	  do 4 nel=1,neto
	    if(itype(nel).eq.5) goto 4
	    read(97) abcd
	    do i=1,8
	      coef1(nel,i)=abcd(i)
	    enddo
	    if(part(nel).lt.1.0) then
	      do i=1,8
	        coef1(nel,i)=coef1(nel,i)*part(nel)
	      enddo
	    endif
	    phil=dabs(philn(nel))
	    qph=qn(nel)*phil*pi/180.
	    forc1=(coef1(nel,1)*qph**3/4.+coef1(nel,2)*qph*qph/3.+
     *           coef1(nel,3)*qph/2.+coef1(nel,4))*qph
	    forc2=(coef1(nel,5)*qph**3/4.+coef1(nel,6)*qph*qph/3.+
     *           coef1(nel,7)*qph/2.+coef1(nel,8))*qph
	  
	    if(isymy.eq.0) then
	      forctsup1=forctsup1+forc1
	      forctsup2=forctsup2+forc2
	    else
	      forctsup1=forctsup1+2*forc1
	      forctsup2=forctsup2+2*forc2
	    endif									
    4	  continue
	  

	  do nel=1,neto
	    if(itype(nel).ne.5) backspace(97)
	  enddo
	  do nel=1,iymax-1
	    if(itype(nel).ne.5) read(97)
	  enddo
	  read(97) abcd
	  do nel=iymax+1,neto
	    if(itype(nel).ne.5) read(97)
	  enddo
	  fsup1=max(abcd(4),abcd(1)*qphi3+abcd(2)*qphi2+abcd(3)*qphi+
     *            abcd(4))
	  fsup2=max(abcd(8),abcd(5)*qphi3+abcd(6)*qphi2+abcd(7)*qphi+
     *            abcd(8))

	
	  ymod1=ymin
	  ymod2=ymin
	  itera=0
	  do 2  
	    forct1=0.
	    forct2=0.
	    do 5 nel=1,neto
	      if(itype(nel).eq.5) goto 5
	      phi=-philn(nel)
            q=qn(nel)
            phi2=phi/3.
            phi3=2.*phi2
            sph1=vsin(phi2/2.,0.d00)
            sph2=vsin(phi3/2.,0.d00)
            cx1=vcos(tetas(nel),phi2/2.)
            cx2=vcos(tetas(nel),phi3/2.)
c
            zb(1)=delt(nel)/delt(iymax)*(z(nel,3)-ymod1)/(ymax-ymod1)
     *            *fsup1
            zb(2)=delt(nel)/delt(iymax)*(z(nel,3)-q*2.*cx1*sph1-ymod1)/
     *            (ymax-ymod1)*fsup1
            zb(3)=delt(nel)/delt(iymax) *(z(nel,3)-q*2.*cx2*sph2-ymod1)/
     *	        (ymax-ymod1)*fsup1	
            zb(4)=delt(nel)/delt(iymax) *(z(nel,4)-ymod1)/(ymax-ymod1)
     *            *fsup1
c
            qphi=phi*q*pi/180.
            qphi2=qphi*qphi
            qphi3=qphi2*qphi

c     a l'origine y=0
            b1(1)=0.
            b1(21)=0.
            b1(41)=0.
            b1(61)=1.
c     au point y=1/3 de phi
            b1(2)=qphi3/27.
            b1(22)=qphi2/9.
            b1(42)=qphi/3.
            b1(62)=1.
c     au point y=2/3 de phi
            b1(3)=8.*qphi3/27.
            b1(23)=4.*qphi2/9.
            b1(43)=2.*qphi/3.
            b1(63)=1.
c     au point y = phi
            b1(4)=qphi3
            b1(24)=qphi2
            b1(44)=qphi
            b1(64)=1.

            call system(b1,zb,4,1,sol,20)

	      do j=1,4
              coef1(nel,j)=sol(j)
	      enddo
	    
		  zb(1)=delt(nel)/delt(iymax)*(z(nel,3)-ymod2)/(ymax-ymod2)
     *            *fsup2
            zb(2)=delt(nel)/delt(iymax)*(z(nel,3)-q*2.*cx1*sph1-ymod2)/
     *            (ymax-ymod2)*fsup2
            zb(3)=delt(nel)/delt(iymax)*(z(nel,3)-q*2.*cx2*sph2-ymod2)/
     *	        (ymax-ymod2)*fsup2	
            zb(4)=delt(nel)/delt(iymax)*(z(nel,4)-ymod2)/(ymax-ymod2)
     *            *fsup2
c
            qphi=phi*q*pi/180.
            qphi2=qphi*qphi
            qphi3=qphi2*qphi

c     a l'origine y=0
            b1(1)=0.
            b1(21)=0.
            b1(41)=0.
            b1(61)=1.
c     au point y=1/3 de phi
            b1(2)=qphi3/27.
            b1(22)=qphi2/9.
            b1(42)=qphi/3.
            b1(62)=1.
c     au point y=2/3 de phi
            b1(3)=8.*qphi3/27.
            b1(23)=4.*qphi2/9.
            b1(43)=2.*qphi/3.
            b1(63)=1.
c     au point y = phi
            b1(4)=qphi3
            b1(24)=qphi2
            b1(44)=qphi
            b1(64)=1.

            call system(b1,zb,4,1,sol,20)

	      do j=1,4
              coef1(nel,j+4)=sol(j)
	      enddo
		
		
		  if(part(nel).lt.1.0) then
	        do i=1,8
	          coef1(nel,i)=coef1(nel,i)*part(nel)
	        enddo
	      endif
	    
		  phil=dabs(philn(nel))
            qph=q*phil*pi/180.

            forc1=(coef1(nel,1)*qph**3/4.+coef1(nel,2)*qph*qph/3.+
     *             coef1(nel,3)*qph/2.+coef1(nel,4))*qph
		  forc2=(coef1(nel,5)*qph**3/4.+coef1(nel,6)*qph*qph/3.+
     *             coef1(nel,7)*qph/2.+coef1(nel,8))*qph
	  
	      if(isymy.eq.0) then
	        forct1=forct1+forc1
	        forct2=forct2+forc2
	      else
	        forct1=forct1+2*forc1
		    forct2=forct2+2*forc2
	      endif	
    5	    continue
	  
	    if(itera.eq.1) then
	      forctinf1=forct1
	      forctinf2=forct2
	      yinf1=ymod1
		  yinf2=ymod2
	    else
	      if(forctsup1*forct1.lt.0.) then
		    forctinf1=forct1
		    yinf1=ymod1
		  else
	        forctinf1=forctsup1
		    forctsup1=forct1
		    yinf1=ysup1
		    ysup1=ymod1
		  endif
	      if(forctsup2*forct2.lt.0.) then
		    forctinf2=forct2
		    yinf2=ymod2
		  else
	        forctinf2=forctsup2
		    forctsup2=forct2
		    yinf2=ysup2
		    ysup2=ymod2
		  endif
		endif

	    ymod1=(ysup1-yinf1)/(forctinf1-forctsup1)*(forctsup1+
     *          (forctsup1-forctinf1)/(yinf1-ysup1)*ysup1)
	    ymod2=(ysup2-yinf2)/(forctinf2-forctsup2)*(forctsup2+
     *          (forctsup2-forctinf2)/(yinf2-ysup2)*ysup2)

	    itera=itera+1

	    if(((dabs(forctinf1).le.1.).or.(dabs(forctsup1).le.1.)).and.
     *       ((dabs(forctinf2).le.1.).or.(dabs(forctsup2).le.1.))) 
     *      goto 3
	
    2   continue
	

    3	  write(66,10) is,itera,ymod1,ymod2	
        ftmom1=0.
	  ftmom2=0.

    	  do 6 nel=1,neto
    	  
	    if(itype(nel).eq.5) goto 6
		q=qn(nel)
	    phil=dabs(philn(nel))
          qph=q*phil*pi/180.
          cc1=vcos(tetas(nel),0.d00)
          cc2=vcos(tetas(nel),phil)
          cc3=vcos(tetas(nel),phil/2.)
          s1=vsin(tetas(nel),0.d00)
          s2=vsin(tetas(nel),phil)
          s3=vsin(tetas(nel),phil/2.)
          sph1=vsin(phil/2.,0.d00)
	  
	  
	    forc1=(coef1(nel,1)*qph**3/4.+coef1(nel,2)*qph*qph/3.+
     *           coef1(nel,3)*qph/2.+coef1(nel,4))*qph
	    forc2=(coef1(nel,5)*qph**3/4.+coef1(nel,6)*qph*qph/3.+
     *           coef1(nel,7)*qph/2.+coef1(nel,8))*qph										
	  
	  
	    if (phil.ge.1.e-05) then
        
	      term11=(coef1(nel,1)*qph**2+coef1(nel,2)*qph+coef1(nel,3)
     *              -q*q*6.*coef1(nel,1))*qph
	      term21=(3.*coef1(nel,1)*qph+2.*coef1(nel,2)) *qph
            term31=q* (-6.*coef1(nel,1)*q*q+coef1(nel,3))
            term41=-2.*coef1(nel,2)*q*q+coef1(nel,4)
	      term12=(coef1(nel,5)*qph**2+coef1(nel,6)*qph+coef1(nel,7)
     *               -q*q*6.*coef1(nel,5))*qph
	      term22=(3.*coef1(nel,5)*qph+2.*coef1(nel,6)) *qph
            term32=q* (-6.*coef1(nel,5)*q*q+coef1(nel,7))
            term42=-2.*coef1(nel,6)*q*q+coef1(nel,8)
	  
        
	      fmom1= - forc1 * (z(nel,3)-ymod1+q*s1)
            fmom1=fmom1+q*q* (-cc2*term11+q*s2*term21+2.*sph1*s3*term41+
     *                          2.*sph1*cc3*term31)
	      fmom2= - forc2 * (z(nel,3)-ymod2+q*s1)
            fmom2=fmom2+q*q* (-cc2*term12+q*s2*term22+2.*sph1*s3*term42+
     *                          2.*sph1*cc3*term32)
	    
	    else
            temp1=qph*qph*(coef1(nel,1)*qph**3/5.+coef1(nel,2)*qph**2/4.
     *                    +coef1(nel,3)*qph/3.+0.5*coef1(nel,4))
	      temp2=qph*qph*(coef1(nel,5)*qph**3/5.+coef1(nel,6)*qph**2/4.
     *                    +coef1(nel,7)*qph/3.+0.5*coef1(nel,8))
	    
            fmom1=-(z(nel,3)-ymod1)*forc1 + temp1*cc1
		  fmom2=-(z(nel,3)-ymod2)*forc2 + temp2*cc1
	    endif

          if(isymy.eq.0) then
	      ftmom1=ftmom1+fmom1
	      ftmom2=ftmom2+fmom2
	    else
	      ftmom1=ftmom1+2*fmom1
	      ftmom2=ftmom2+2*fmom2
	    endif	
		
    6	  continue

	  alpha1=bm1(is)/ftmom1
	  alpha2=bm2(is)/ftmom2
	  
	  do 7 nel=1,neto
	    if(itype(nel).eq.5) goto 7
	    do i=1,4
	      abcd(i)=alpha1*coef1(nel,i)
	      abcd(i+4)=alpha2*coef1(nel,i+4)
	    enddo
	    write(94) abcd
    	if(impr2.ge.-1) then
	    write(66,*)
          write(66,'(a,i2)') 'panneau nø ',nel
          write(66,'(a,a)')'cas de charge :  abcd(1à4) for mx1 & my1 et'	    
     *									  ,'abcd(5à8) for mx2 & my2'
          write(66,'(t4,i2,a4,4(1x,e11.4),a4,4(1x,e11.4))')
     *          is,' : ',(abcd(k),k=1,4),'   ',(abcd(k),k=5,8)
          	  
    	endif
    7	  continue
        write(66,*)
	enddo

	rewind 97
	rewind 94
	do is=1,nsol
	  do 8 nel=1,neto
	    if(itype(nel).eq.5) goto 8
	    read(94)abcd
	    write(97)abcd
    8	  continue
      enddo

c	les formats
c================================================================================

   10 format('pour le cas de charge',i2,' et après',i2,' itérations, la
     * position de l axe neutre ky'/'(repère utilisateur) modifiée par
     * la prise en compte des coefficients de participation est'/'à
     * gauche     ',e11.4,/'à droite     ',e11.4)


      return
      end