      subroutine conlin (s,ms,iuin,iop,non,neto,vnom2,nvar,nxit,m1tabl, !octobre2003	!dcn	!fev2007
     *                   m2cont,igrav,irestr,nsol,iweight,iprice,		  !fev2007
     *				   inert,imod,ic_rest1,mmmax1,ratio_rest1)	      !fev2007  dad
c     **********************************************************
	implicit real*8 (a-h,o-z)				
	integer*4 ic_rest1(mmmax1)
	real*8    ratio_rest1(mmmax1)
      dimension s(1)
      character *30 vnom2(1)     !dad

      dimension nvar(neto),nxit(9,neto)	         !  common opti
c	dimension m1tabl(1),m2cont(1)                !octobre2003	!dcn	!fev2007

c     earlier versions (v.0, v.1, v.1.5)  
c     version utilise par lbr-5. ph. rigo, 24 nov. 1995
c       - avec modification des tests sur is (dimension max)
c
c   dernière modification:  
c          : 10 juin 1996
c          : 1-08-96 - formatage des read(iuin,*)
c          :10-06-98 - nbre max d'itération porté à 100 au lieu de  40
c          :18-06-03 - nbre max d'itération porté à 200 au lieu de 100
c                       (voir subr. scp0 : max =200)
c          :19-2-2004
c
c
c     meth > 5  ===> new conlin        (convex)
c     ========
c
      read (iuin,*) n,m   				
c     n     | number of design variables        |  i
c     m     | number of design constraints      |  i
c
c     implementation of arrays in s(ms)
c     ---------------------------------
c
      l = max0 (n,m) + 1
      i1 =1
      i2 =i1+n
      i3 =i2+n
      i4 =i3+n
      i5 =i4+n
      i6 =i5+n
      i7 =i6+l
      i8 =i7+m
      i9 =i8+l
      i10=i9+m
      i11=i10+m
      i12=i11+n*m					!		new !
      is =i12+3*n
c     is =8*n+3*m+n*m+2*max(n,m)+2
c     is = 145.102   si n=200 et m=700  (neto=30
c     is = 813.202   si n=400 et m=2000 (neto=50)
c     is =6258.000   si n=800 et m=7000 (neto=100)


      if(is.gt.ms) goto 901			!		new !
c      write (666,*) 'is,ms=',is,ms
c      pause'conl1'
c      
      call v2v1 (s(i1),s(i2),s(i3),s(i4),s(i5),
     *           s(i6),s(i7),s(i8),s(i9),s(i10),s(i11),	
     *           s(is),						              ! modif 
     *           n,m,iuin,iop,non,neto,vnom2,nvar,nxit,			!octobre2003
     *           m1tabl,m2cont,igrav,irestr,nsol,iweight,iprice,	!fev2007	!dcn	!fev2007
     *		   inert,imod,ic_rest1,mmmax1,ratio_rest1)							    !fev2007  dad
      goto 900
c
 901  write (666,291) is,ms ! taille memoire insuffisante --> stop
      write (29 ,291) is,ms			!sept06														!bug
      non=900
c
 900  return
 291  format('error - not enough memory for conlin v.1 ',i5,'>',i5)
      end

c========================================================================
c========================================================================
      
      subroutine v2v1 (xi,ix,ximin,ximax,fi,
     *                 rj,jr,cj,cjmax,dcj,cij,
     *                 s,
     *                 n,m,iuin,iop,non,neto,vnom2,nvar,nxit,m1tabl,  !octobre2003	!dcn	!fev2007
     *                 m2cont,igrav,irestr,nsol,iweight,iprice,		!fev2007
     *				 inert,imod,ic_rest1,mmmax1,ratio_rest1)		    !fev2007  dad
c
c     conlin version 1.5 - pure dual approach
c     =======================================
c
      implicit real*8 (a-h,o-z)

      character *30 vnom2(m)                !dad
	integer*4 ic_rest1(mmmax1)             !dad
	real*8    ratio_rest1(mmmax1)          !dad

      dimension nvar(neto),nxit(9,neto)	        	    !  common opti

      dimension xi(n),ix(n),ximin(n),ximax(n),fi(n),
     *          rj(m),jr(m),cj(m),cjmax(m),dcj(m),
     *          cij(n,m),s(3*n)

      dimension vnom(9),vnom3(5)												!18.03.04
	dimension m1tabl(neto,nsol),m2cont(neto)     !octobre2003	!dcn	!fev2007
      data vnom/'ep. bord','hâme cad','eâme cad','lsem cad','epsa cad',
     *      	  'hâme rai','eâme rai','lsem rai','epsr rai'/
      data vnom3/'hâme epo','eâme epo','lsem epo','ep.  epo','epsa epo'/		!18.03.04

      read (iuin,*,end=900) tol,cut,prec,pds
      read (iuin,'(9e14.7)',end=900) xi
      read (iuin,'(9e14.7)',end=900) ximin
      read (iuin,'(9e14.7)',end=900) ximax
      read (iuin,*,end=900) jobj
      read (iuin,'(9e14.7)',end=900) obj
      read (iuin,'(9e14.7)',end=900) fi
c
c     if(iop.ge.3) write (666,62) tol,cut,prec,pds

      if(iop.ge.3) then
        write (*,61) 
        write (6970,61) 
        k=0
        do 14 nel=1,neto
        if(nvar(nel).ne.0) then
          k2=k+nvar(nel)
          write (*,10) nel,(vnom(nxit(i,nel)),i=1,nvar(nel))
          write (6970,10) nel,(vnom(nxit(i,nel)),i=1,nvar(nel))
          write (*, 3) (ximin(i),i=k+1,k2)
          write (6970, 3) (ximin(i),i=k+1,k2)
          write (*,44) (xi(i),i=k+1,k2)
          write (6970,44) (xi(i),i=k+1,k2)
          write (*, 5) (ximax(i),i=k+1,k2)
          write (6970, 5) (ximax(i),i=k+1,k2)
          write (666,9) nel,(vnom(nxit(i,nel)),i=1,nvar(nel))
          write (666,3) (ximin(i),i=k+1,k2)
          write (666,4) (xi(i),i=k+1,k2)
          write (666,5) (ximax(i),i=k+1,k2)
          k=k2
        endif
   14   continue
      endif

c     impressions de la fct objectif.
      write (6,663) obj,jobj
      write (6970,663) obj,jobj
 663  format(1x,'fonction objectif =',e14.7,'  (type = ',i2,')')
      write (666,*) 
      write (666,63) obj,jobj
      write (666,*) 

c     impressions des dérivées de la fct objectif			
      k=0
      if(iop.le.2) go to 15
      do 13 nel=1,neto
        if(nvar(nel).ne.0) then
          k2=k+nvar(nel)
          write (666,6) (fi(i),i=k+1,k2)
	    k=k2
        endif
   13 continue
      write (666,*) 
   15 continue

c     lecture et impressions des restrictions et de leurs dérivées	
      mnc=0
      me=0
      if(iop.ge.2) then
        write (*,65)
        write (6970,65)
        write (666,65)
      endif

      j1=0
c      do 2 j=1,m                   ! tous ces commentaires : !octobre2003
c       read (iuin,*,end=900) jt		
c       read (iuin,'(9e14.7)',end=900) cj(j),cjmax(j),dcj(j),rj(j)
cc       if(iop.ge.3) write (*,'(a,i5,2(a,e15.6),2a)')	
cc     *   'c(',j,')=',cj(j),' ?<? ',cjmax(j),': type = ',vnom2(j)
c       write (666,'(a,i5,2(a,e15.6),2a)')	
c     *    'c(',j,')=',cj(j),' ?<? ',cjmax(j),': type = ',vnom2(j)
c       if(jt.ne.0) mnc=mnc+1
c       if(jt.lt.0) me=me+1
c       read (iuin,'(9e14.7)',end=900) (cij(i,j),i=1,n)
c      
cc     impression des sensibilités
c       if(iop.le.2) go to 2
c       k=0
c       do 12 nel=1,neto
c         if(nvar(nel).ne.0) then
c           k2=k+nvar(nel)
c           write (666,7) (cij(i,j),i=k+1,k2)
c	     k=k2
c         endif
c   12  continue
c   2  continue

c!!!!!!!!!!!!!!!!!!!!!!!!début !octobre2003 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      j=1
	do ii1=1,neto  ! rest geom
	  do ii2=1,m2cont(ii1)
	    read (iuin,*,end=900) jt		
          read (iuin,'(9e14.7)',end=900) cj(j),cjmax(j),dcj(j),rj(j)
c          if(iop.ge.3) write (*,'(a,i5,2(a,e15.6),2a)')	
c     *      'c(',j,')=',cj(j),' ?<? ',cjmax(j),': type = ',vnom2(j)
		if (impr2.ge.-1) then				!sept06          
		write (666,'(a,i5,2(a,e15.6),2a,3x,a,i3)')	
     *       'c(',j,')=',cj(j),' ?<? ',cjmax(j),': type = ',vnom2(j),
     *       'panneau :',ii1
          endif								!sept06
		if(jt.ne.0) mnc=mnc+1
          if(jt.lt.0) me=me+1
          read (iuin,'(9e14.7)',end=900) (cij(i,j),i=1,n)  
c     impression des sensibilités
          if(iop.gt.2) then
            k=0
            do nel=1,neto
              if(nvar(nel).ne.0) then
                k2=k+nvar(nel)
                write (666,7) (cij(i,j),i=k+1,k2)
	          k=k2
              endif
            enddo
	    endif
	    j = j+1
	  enddo
	enddo

c ! *** centre de gravite
	do ii2=1,igrav
	    read (iuin,*,end=900) jt		
          read (iuin,'(9e14.7)',end=900) cj(j),cjmax(j),dcj(j),rj(j)
          if (impr2.ge.-1) then				!sept06
		write (666,'(a,i5,2(a,e15.6),2a)')	
     *       'c(',j,')=',cj(j),' ?<? ',cjmax(j),': type = ',vnom2(j)
          endif								!sept06
		if(jt.ne.0) mnc=mnc+1
          if(jt.lt.0) me=me+1
          read (iuin,'(9e14.7)',end=900) (cij(i,j),i=1,n)  
c     impression des sensibilités
          if(iop.gt.2) then
            k=0
            do nel=1,neto
              if(nvar(nel).ne.0) then
                k2=k+nvar(nel)
                write (666,7) (cij(i,j),i=k+1,k2)
	          k=k2
              endif
            enddo
	    endif
	    j=j+1
	enddo

c rest sur inertie		!restri inertie								!sept06
      if(inert.ne.0) then												!sept06
	  read (iuin,*,end=900) jt										!sept06		
        read (iuin,'(9e14.7)',end=900) cj(j),cjmax(j),dcj(j),rj(j)	!sept06
	  if (impr2.ge.-1) then											!sept06			
        write (666,'(a,i5,2(a,e15.6),2a)')							!sept06	
     *     'c(',j,')=',cj(j),' ?<? ',cjmax(j),': type = ',vnom2(j)	!sept06
 	  endif															!sept06
        if(jt.ne.0) mnc=mnc+1											!sept06
        if(jt.lt.0) me=me+1											!sept06
        read (iuin,'(9e14.7)',end=900) (cij(i,j),i=1,n)  				!sept06
c     impression des sensibilités										!sept06
        if(iop.gt.2) then												!sept06
          k=0															!sept06
          do nel=1,neto												!sept06
            if(nvar(nel).ne.0) then									!sept06
              k2=k+nvar(nel)											!sept06
              write (666,7) (cij(i,j),i=k+1,k2)						!sept06
	        k=k2													!sept06
            endif														!sept06
          enddo														!sept06
	  endif															!sept06
	  j=j+1															!sept06
	endif				!restri inertie								!sept06


c rest sur module sectionnel		!restri module						!sept06
      if(imod.ne.0) then												!sept06
	  read (iuin,*,end=900) jt										!sept06		
        read (iuin,'(9e14.7)',end=900) cj(j),cjmax(j),dcj(j),rj(j)	!sept06
	  if (impr2.ge.-1) then											!sept06			
        write (666,'(a,i5,2(a,e15.6),2a)')							!sept06	
     *     'c(',j,')=',cj(j),' ?<? ',cjmax(j),': type = ',vnom2(j)	!sept06
 	  endif															!sept06
        if(jt.ne.0) mnc=mnc+1											!sept06
        if(jt.lt.0) me=me+1											!sept06
        read (iuin,'(9e14.7)',end=900) (cij(i,j),i=1,n)				!sept06  
c     impression des sensibilités										!sept06
        if(iop.gt.2) then												!sept06
          k=0															!sept06
          do nel=1,neto												!sept06
            if(nvar(nel).ne.0) then									!sept06
              k2=k+nvar(nel)											!sept06
              write (666,7) (cij(i,j),i=k+1,k2)						!sept06
	        k=k2													!sept06
            endif														!sept06
          enddo														!sept06
	  endif															!sept06
	  j=j+1															!sept06
	endif						!restri module						!sept06


c rest sur poids			!restri poids								!sept06
      if(iweight.ne.0) then											!sept06
	  read (iuin,*,end=900) jt										!sept06		
        read (iuin,'(9e14.7)',end=900) cj(j),cjmax(j),dcj(j),rj(j)	!sept06
	  if (impr2.ge.-1) then											!sept06				
        write (666,'(a,i5,2(a,e15.6),2a)')							!sept06	
     *     'c(',j,')=',cj(j),' ?<? ',cjmax(j),': type = ',vnom2(j)	!sept06
 	  endif															!sept06			
        if(jt.ne.0) mnc=mnc+1											!sept06
        if(jt.lt.0) me=me+1											!sept06
        read (iuin,'(9e14.7)',end=900) (cij(i,j),i=1,n)				!sept06  
c     impression des sensibilités										!sept06
        if(iop.gt.2) then												!sept06
          k=0															!sept06
          do nel=1,neto												!sept06
            if(nvar(nel).ne.0) then									!sept06
              k2=k+nvar(nel)											!sept06
              write (666,7) (cij(i,j),i=k+1,k2)						!sept06
	        k=k2													!sept06
            endif														!sept06
          enddo														!sept06
	  endif															!sept06
	  j=j+1															!sept06
	endif				!restri poids


c rest sur cout			!restri cout
      if(iprice.ne.0) then											!fev2007
	  read (iuin,*,end=900) jt										!fev2007
        read (iuin,'(9e14.7)',end=900) cj(j),cjmax(j),dcj(j),rj(j)	!fev2007
	  if (impr2.ge.-1) then											!fev2007
        write (666,'(a,i5,2(a,e15.6),2a)')							!fev2007
     *     'c(',j,')=',cj(j),' ?<? ',cjmax(j),': type = ',vnom2(j)	!fev2007
 	  endif															!fev2007		
        if(jt.ne.0) mnc=mnc+1											!fev2007
        if(jt.lt.0) me=me+1											!fev2007
        read (iuin,'(9e14.7)',end=900) (cij(i,j),i=1,n)				!fev2007
c     impression des sensibilités
        if(iop.gt.2) then												!fev2007
          k=0															!fev2007
          do nel=1,neto												!fev2007
            if(nvar(nel).ne.0) then									!fev2007
              k2=k+nvar(nel)											!fev2007
              write (666,7) (cij(i,j),i=k+1,k2)						!fev2007
	        k=k2													!fev2007
            endif														!fev2007
          enddo														!fev2007
	  endif															!fev2007
	  j=j+1															!fev2007
	endif				!restri cout


c rest struc

	do ii1=1,nsol   !rest struct
	  do ii2=1,neto
	    do ii3=1,m1tabl(ii2,ii1)									!dcn	!fev2007
            read (iuin,*,end=900) jt		
            read (iuin,'(9e14.7)',end=900) cj(j),cjmax(j),dcj(j),rj(j)
c            if(iop.ge.3) write (*,'(a,i5,2(a,e15.6),2a)')	
c     *       'c(',j,')=',cj(j),' ?<? ',cjmax(j),': type = ',vnom2(j)
		if (impr2.ge.-1) then			!sept06          
		  write (666,'(a,i5,2(a,e15.6),2a,2(3x,a,i3))')	
     *         'c(',j,')=',cj(j),' ?<? ',cjmax(j),': type = ',vnom2(j),
     *         'panneau :',ii2,'cas de charge:',ii1
          endif							!sept06
		  if(jt.ne.0) mnc=mnc+1
            if(jt.lt.0) me=me+1
            read (iuin,'(9e14.7)',end=900) (cij(i,j),i=1,n)
c     impression des sensibilités
            if(iop.gt.2) then
              k=0
              do nel=1,neto
                if(nvar(nel).ne.0) then
                  k2=k+nvar(nel)
                  write (666,7) (cij(i,j),i=k+1,k2)
	            k=k2
                endif
              enddo
	      endif
	      j = j+1
	    enddo
	  enddo
	enddo

c m ult 
c ! *** moment ultime (poutre navire)
      if(irestr.ne.0) then
	do ii2=1,2
	    read (iuin,*,end=900) jt		
          read (iuin,'(9e14.7)',end=900) cj(j),cjmax(j),dcj(j),rj(j)
       	if (impr2.ge.-1) then				!sept06 
		write (666,'(a,i5,2(a,e15.6),2a)')	
     *       'c(',j,')=',cj(j),' ?<? ',cjmax(j),': type = ',vnom2(j)
			endif							!sept06         
		if(jt.ne.0) mnc=mnc+1
          if(jt.lt.0) me=me+1
          read (iuin,'(9e14.7)',end=900) (cij(i,j),i=1,n)  
c     impression des sensibilités
          if(iop.gt.2) then
            k=0
            do nel=1,neto
              if(nvar(nel).ne.0) then
                k2=k+nvar(nel)
                write (666,7) (cij(i,j),i=k+1,k2)
	          k=k2
              endif
            enddo
	    endif
	    j=j+1
	enddo
	endif

c     liste des contraintes approchant la valeur maximale...
      write (666,*)
      write (666,'(6x,a/(6x,61(1h-)))')'liste des contraintes approchant   
     * la valeur maximale autorisée (à 98%) (ou > -0.0001)'
	write(666,*)

      nactiv=0
      write (666,*) "/nactive"  !dad
	j=1
	i=0
	do ii1=1,neto     !restrictions géométriques
	  do ii2=1,m2cont(ii1)
	    if (cj(j).gt.0.98*cjmax(j).or.
     *                      (cjmax(j).eq.0.and.cj(j).gt.-0.0001)) then
	      if (cjmax(j).ne.0) then
	        pourc=(cj(j)-cjmax(j))/cj(j)*100
!             write (666,'(a,i5,2(a,e15.6),2a,3x,a,i3,3x,a,f8.2)')	          dad
!    *         'c(',j,')=',cj(j),' ?<? ',cjmax(j),': type = ',vnom2(j),         dad
!    *         'panneau :',ii1,'% de diff.',pourc                               dad
		     write (666,'(a,i5,2(3x,a,e15.6),3x,4a,f8.4,a,i4,a,i3)')	      !dad
     *         'c(',j,')=',cj(j),' limit = ',cjmax(j),' type = geo  ',          !dad 
     *         '   str = "',vnom2(j),'"   diff% =',pourc,                       !dad
     *         '     ref = ',ic_rest1(j),'   pannel =',ii1                       !dad
	      else !ça ne sert à rien d'écrire le %, c'est tjs 100%
!			write (666,'(a,i5,2(a,e15.6),2a,3x,a,i3)')	                      !dad
!     *         'c(',j,')=',cj(j),' ?<? ',cjmax(j),': type = ',vnom2(j),        !dad
!     *         'panneau :',ic_rest1(j)                                          !dad
		     write (666,'(a,i5,2(3x,a,e15.6),3x,4a,f8.4,a,i4,a,i3)')	      !dad
     *         'c(',j,')=',cj(j),' limit = ',cjmax(j),' type = geo  ',          !dad 
     *       '   str = "',vnom2(j),'"   diff% =',(ratio_rest1(j)-1)*100,        !dad
     *       '     ref = ',ic_rest1(j),'   pannel =',ii1                        !dad
      	  endif
	      i=1
        	  nactiv=nactiv+1   !dad

	    endif
	    j = j+1
	  enddo
	enddo
	do ii2=1,igrav   !restriction sur la position du centre de gravité
	  if (cj(j).gt.0.98*cjmax(j)) then
	    pourc=(cj(j)-cjmax(j))/cj(j)*100
!          write (666,'(a,i5,2(a,e15.6),2a,3x,a,i3)')!,3x,a,f8.2)')		!sept06	
!     *       'c(',j,')=',cj(j),' ?<? ',cjmax(j),': type = ',vnom2(j),
!     *       'igrav :',ii2 !,'% de diff.',pourc							!sept06

		write (666,'(a,i5,2(3x,a,e15.6),3x,4a,f8.4,a,i3)')	                  !dad
     *         'c(',j,')=',cj(j),' limit = ',cjmax(j),' type = glo  ',          !dad 
     *         '   str = "',vnom2(j),'"   diff% =',pourc,                       !dad
     *         '   igrav =',ii2                                                 !dad

	    i=1
	    nactiv=nactiv+1   !dad
	  endif
	  
	  j=j+1
	enddo      
	if(inert.ne.0) then		!restri inertie								!sept06
	  if (cj(j).gt.0.98*cjmax(j)) then									!sept06
	    pourc=(cj(j)-cjmax(j))/cj(j)*100								!sept06
!		write (666,'(a,i5,2(a,e15.6),2a,3x,a,f8.2)')					!sept06							
!     *         'c(',j,')=',cj(j),' ?<? ',cjmax(j),': type = ',vnom2(j),	!sept06
!     *		 '% de diff.',pourc											!sept06

		write (666,'(a,i5,2(3x,a,e15.6),3x,4a,f8.4)')	                      !dad
     *         'c(',j,')=',cj(j),' limit = ',cjmax(j),' type = glo  ',          !dad 
     *         '   str = "',vnom2(j),'"   diff% =',pourc                        !dad
 

	    i=1																!sept06
	    nactiv=nactiv+1   !dad

	  endif																!sept06
	  j=j+1																!sept06
	endif					!restri inertie								!sept06
	if(imod.ne.0) then		!restri module								!sept06
	  if (cj(j).gt.0.98*cjmax(j)) then									!sept06
	    pourc=(cj(j)-cjmax(j))/cj(j)*100								!sept06
!		write (666,'(a,i5,2(a,e15.6),2a,3x,a,f8.2)')					!sept06							
!     *         'c(',j,')=',cj(j),' ?<? ',cjmax(j),': type = ',vnom2(j),	!sept06
!     *		 '% de diff.',pourc											!sept06	

		write (666,'(a,i5,2(3x,a,e15.6),3x,4a,f8.4)')	                      !dad
     *         'c(',j,')=',cj(j),' limit = ',cjmax(j),' type = glo  ',          !dad 
     *         '   str = "',vnom2(j),'"   diff% =',pourc                        !dad

	    i=1																!sept06
	    nactiv=nactiv+1   !dad
	  endif																!sept06
	  j=j+1																!sept06
	endif					!restri module								!sept06
	if(iweight.ne.0) then	!restri poids								!sept06
	  if (cj(j).gt.0.98*cjmax(j)) then									!sept06
	    pourc=(cj(j)-cjmax(j))/cj(j)*100								!sept06
!		write (666,'(a,i5,2(a,e15.6),2a,3x,a,f8.2)')					!sept06							
!     *         'c(',j,')=',cj(j),' ?<? ',cjmax(j),': type = ',vnom2(j),	!sept06
!     *		 '% de diff.',pourc											!sept06	

		write (666,'(a,i5,2(3x,a,e15.6),3x,4a,f8.4)')	                      !dad
     *         'c(',j,')=',cj(j),' limit = ',cjmax(j),' type = glo  ',          !dad 
     *         '   str = "',vnom2(j),'"   diff% =',pourc                        !dad

	    i=1																!sept06
	    nactiv=nactiv+1   !dad
	  endif																!sept06
	  j=j+1																!sept06
	endif					!restri poids								!sept06
	do ii1=1,nsol     !restrictions structurelles						
	  do ii2=1,neto
	    do ii3=1,m1tabl(ii2,ii1)										!dcn	!fev2007
            if (cj(j).gt.0.98*cjmax(j).or.
     *                      (cjmax(j).eq.0.and.cj(j).gt.-0.0001)) then
	        if (cjmax(j).ne.0) then
			  pourc=(cj(j)-cjmax(j))/cj(j)*100
!                write (666,'(a,i5,2(a,e15.6),2a,2(3x,a,i3),3x,a,f8.2)')	
!     *           'c(',j,')=',cj(j),' ?<? ',cjmax(j),': type = ',vnom2(j)
!     *           ,'panneau :',ii2,'cas de charge:',ii1,'% de diff.',
!     *         pourc


		write (666,'(a,i5,2(3x,a,e15.6),3x,4a,f8.4,a,i4,2(a,i3))')	          !dad
     *         'c(',j,')=',cj(j),' limit = ',cjmax(j),' type = stru ',          !dad 
     *         '   str = "',vnom2(j),'"   diff% =',pourc,                       !dad
     *         '     ref = ',ic_rest1(j),'   pannel =',ii2,                      !dad
     *         '   ccha =',ii1

	        else !ça ne sert à rien d'écrire le %, c'est tjs 100%
!                write (666,'(a,i5,2(a,e15.6),2a,2(3x,a,i3))')	
!     *           'c(',j,')=',cj(j),' ?<? ',cjmax(j),': type = ',vnom2(j)
!     *           ,'panneau :',ii2,'cas de charge:',ii1

!		write (666,'(a,i5,2(3x,a,e15.6),3x,4a,i4,a,i3,a,i3)')	              !dad


!		write (666,'(a,i5,2(3x,a,e15.6),3x,4a,8(1h*),a,i4,2(a,i3))')	      !dad
!     *         'c(',j,')=',cj(j),' limit = ',cjmax(j),' type = stru ',          !dad 
!     *         '   str = "',vnom2(j),'"   diff% =',                             !dad
!     *         '     ref = ',ic_rest1(j),'   pannel =',ii2,                      !dad
!     *         '   ccha =',ii1

		write (666,'(a,i5,2(3x,a,e15.6),3x,4a,f8.4,a,i4,2(a,i3))')	          !dad
     *         'c(',j,')=',cj(j),' limit = ',cjmax(j),' type = stru ',          !dad 
     *       '   str = "',vnom2(j),'"   diff% =',(ratio_rest1(j)-1)*100,         !dad
     *         '     ref = ',ic_rest1(j),'   pannel =',ii2,                      !dad
     *         '   ccha =',ii1


	        endif
              nactiv=nactiv+1   !dad
	        i=1
            endif
	      j = j+1
	    enddo
	  enddo
	enddo
	if(iprice.ne.0) then	!restri cout								!fev2007
	  if (cj(j).gt.0.98*cjmax(j)) then									!fev2007
	    pourc=(cj(j)-cjmax(j))/cj(j)*100								!fev2007
!		write (666,'(a,i5,2(a,e15.6),2a,3x,a,f8.2)')					!fev2007		
!     *         'c(',j,')=',cj(j),' ?<? ',cjmax(j),': type = ',vnom2(j),	!fev2007
!     *		 '% de diff.',pourc											!fev2007

		write (666,'(a,i5,2(3x,a,e15.6),3x,4a,f8.4)')	                      !dad
     *         'c(',j,')=',cj(j),' limit = ',cjmax(j),' type = glo  ',          !dad 
     *         '   str = "',vnom2(j),'"   diff% =',pourc                        !dad

	    i=1																!fev2007
	    nactiv=nactiv+1   !dad
	  endif																!fev2007
	  j=j+1																!fev2007
	endif					!restri cout
	if(irestr.ne.0) then  !restrictions sur le moment ultime
	do ii2=1,2
	  if (cj(j).gt.0.98*cjmax(j)) then
	    pourc=(cj(j)-cjmax(j))/cj(j)*100
!	    write (666,'(a,i5,2(a,e15.6),2a,3x,a,i3,3x,a,f8.2)')	
!     *       'c(',j,')=',cj(j),' ?<? ',cjmax(j),': type = ',vnom2(j),
!     *       'numéro :',ii1,'% de diff.',pourc



		write (666,'(a,i5,2(3x,a,e15.6),3x,4a,f8.4,a,i3)')	                  !dad
     *         'c(',j,')=',cj(j),' limit = ',cjmax(j),' type = glo  ',          !dad 
     *         '   str = "',vnom2(j),'"   diff% =',pourc,                       !dad
     *         '   num   =',ii1                                                 !dad



	      i=1
	      nactiv=nactiv+1   !dad
	  endif
	  j=j+1
	enddo
	endif
      write (666,*) "nactive/" !dad

	if (i.eq.0) write(666,'(a)') 'néant'
!	write(666,*)                    !dad
!	write(666,*) 'nactive=',nactiv  !dad
      write(666,*)
c!!!!!!!!!!!!!!!!!!!!!!!!fin !octobre2003 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      if(iop.ge.3) pause 'pause'
      rewind iuin

      niter=0
      i1 =1
      i2 =i1+n
      i3 =i2+n
c     pause'conl3'
      
      call scp (xi,ix,ximin,ximax,fi,
     *          s(i1),s(i2),s(i3),				!	modif!
     *          rj,jr,cj,cjmax,dcj,cij,
     *          tol,cut,prec,obj,
     *          n,m,iact,jact,niter,iop,non)

c     write conlin results 
c     ----------------------
      delta=0.

  58  if(iop.ge.2) then
        write (*  ,59) 						!	impression
        write (*  ,59) 						!	impression
        write (666,*) 						!	  des
        write (666,59)						!	résultats
        k=0
        do 11 nel=1,neto
        if(nvar(nel).ne.0) then
      	k2=k+nvar(nel)
          write (*,10)nel,(vnom(nxit(i,nel)),i=1,nvar(nel)) !	noms des xi
          write (6970,10)nel,(vnom(nxit(i,nel)),i=1,nvar(nel)) !	noms des xi
          write (*,44) (xi(i),i=k+1,k2)					  ! var. concept.
          write (6970,44) (xi(i),i=k+1,k2)					  ! var. concept.
          write (666,9)nel,(vnom(nxit(i,nel)),i=1,nvar(nel))
          write (666,4)(xi(i),i=k+1,k2)
	    k=k2
        endif
  11    continue
        write (666,60) obj  					!	fct objectif
        write (*,  60) obj
        write (6970,  60) obj
      endif
      
  57  if(iop.le.3) goto 67
      ico=0
      write (*,*)
      write (*,*) 'pour modifier une variable xi ',
     *  "pour l'itération suivante: taper le nø de cette variable "
      write (*,*) 'sinon, taper 0 (par défaut)'

      write (6970,*)
      write (6970,*) 'pour modifier une variable xi ',
     *  "pour l'itération suivante: taper le nø de cette variable "
      write (6970,*) 'sinon, taper 0 (par défaut)'


      read  (*,*) ico
      if(ico.lt.0) goto 57
      if(ico.gt.n) goto 57
      if(ico.ne.0) then
      write (*,*) 'la variable nø ',ico,'  est :',xi(ico)
      write (*,*) 'quelle doit être sa nouvelle valeur ?'

      write (6970,*) 'la variable nø ',ico,'  est :',xi(ico)
      write (6970,*) 'quelle doit être sa nouvelle valeur ?'


      read  (*,*) xi(ico)
      write (666,*) 
      write (666,*) '!! la variable nø ',ico,' modifiée est =',xi(ico)
      write (666,*) '   ----------------------'
      goto 58
      endif
   67 continue
      
      return
c
 900  write (666,290) iuin
	write (29 ,290) iuin				!sept06
      non=901
      return
c
    3 format('bornes min:',9(e10.3,1x))
    4 format('variables_:',9(e11.4))
   44 format('variables_:',9(e10.3,1x))
    5 format('bornes max:',9(e10.3,1x))
    6 format('d fct /dx.:',9(e10.3,1x))
    7 format('d c(j)/dx.:',9(e10.3,1x))
    9 format('panel nø',i2,2x,9(1x,a8,2x))
   10 format('panel nø',i2,2x,9(1x,a8,4x))
  59  format(' les resultats (variables indépendantes) :'/20(1h-)) 
  60  format(/'fct objectif (approximation) = ',e14.7)
  61  format( ' les donnees :'/20(1h-))
  62  format(/' les donnees :(par.= ',4e11.4,')'/20(1h-))
  63  format('fct objectif =',e14.7,'  (type = ',i2,')')
  64  format('restrictions nø ',i3,' (type of approximation =',i2,')')
  65  format('les restrictions'/15(1h-))
 290  format('error in conlin data base  -  end of file on unit',i3)
      end
      
c ***********************************************************************      
c ***********************************************************************      
c ***********************************************************************      
      
      subroutine scp (xi,ix,ximin,ximax,fi,ai,si,x2,
     *                rj,jr,cj,cjbar,cg,cij,
     *                tol,cut,prec,obj,
     *                n,m,iact,jact,niter,iop,non)

      implicit real*8(a-h,o-z)
      dimension xi(n),ix(n),ximin(n),ximax(n),fi(n),ai(n),si(n),x2(n),
     *          rj(m),jr(m),cj(m),cjbar(m),cg(m),cij(n,m)
c                 
c  ----------------------------------------------  i = input
c
c     xi    | design variables                  |  i/o
c     ix    | design variable status            |  internal
c     ximin | lower bounds on design variables  |  i
c     ximax | upper bounds on design variables  |  i
c     fi    | objective function gradient       |  i
c     ai    | hessian of lagrangian function    |  internal 
c     si    | search direction in primal space  |  internal 
c     x2    | square of design variables        |  internal
c     rj    | dual variables                    |  i/o
c     jr    | list of active constraints        |  internal
c     cj    | constraint values                 |  i
c     cjbar | upper bounds on constraints       |  i
c     cg    | conjugate gradient in dual space  |  internal
c     cij   | constraint gradients              |  i
c     tol   | tolerance for convergence         |  i
c     cut   | cut-off factor                    |  i
c     prec  | precision                         |  i
c     obj   | objective function value          |  i/o
c     n     | number of design variables        |  i
c     m     | number of design constraints      |  i
c     iact  | number of free variables          |  o
c     jact  | number of active constraints      |  o
c     niter | number of iterations performed    |  o
c     iop   | printing option (0 to 5)          |  i
c     non   | error indicator (ò 899 ==> error) |  o
c
c  ----------------------------------------------  o = output
c
      zero=0.d 00
      un  =1.d 00
      deux=2.d 00
      if(tol.le.zero)  tol=1.d-03
      if(cut.le.un)    cut=deux
      if(prec.le.zero) prec=1.d-06
      if(tol.lt.prec)  tol=prec
c
c     verification of input data - initialization
c     -------------------------------------------
c
      if(iop.ge.1) write (666,200) 
      if(iop.ge.3) write (666,201) tol,cut,prec
c
      do 3 i=1,n
      ix(i)=0
      if(xi(i).le.ximin(i)) ix(i)=-1
      if(xi(i).ge.ximax(i)) ix(i)= 1
      if(iop.gt.2) write (666,202) i,ix(i),ximin(i),xi(i),ximax(i),fi(i)
      if(ximin(i).gt.ximax(i)) write (666,291) i
      if(ximin(i).gt.ximax(i)) write (29 ,291) i		!sept06
      obj=obj-dabs(fi(i))*xi(i)
      if(fi(i)) 2,1,3
   1  write (666,292) i
	write (29 ,292) i								!sept06
      fi(i)=prec
      goto 3
   2  fi(i)=fi(i)*xi(i)*xi(i)
   3  continue
c
      if(iop.gt.3) write (666,203)
c
      jact=0
      do 12 j=1,m
      ja=0
      if(rj(j).le.zero) goto 4
      jact=jact+1
      ja=jact
      jr(jact)=j
      rj(jact)=rj(j)
   4  zz=cjbar(j)-cj(j)
c
      if(iop.gt.3) write (666,202) j,ja,cj(j),cjbar(j),rj(j)
      if(iop.ge.6) write (666,204) (cij(i,j),i=1,n)

c	do i=1,m
c	open(20, file = 'cj dynamique.txt', status ='new')
c	write (20,*) cj(i)
c	enddo
c
      do 6 i=1,n
      if(cij(i,j).lt.zero) cij(i,j)=cij(i,j)*xi(i)*xi(i)
   6  continue
c
  	z0=promix (xi,cij(1,j),n)
      zz=zz+z0
c
      if(zz.gt.zero) goto 8
      zz=-zz
      write (666,293) j,zz
      write (29 ,293) j,zz						!sept06
      non=902
      goto 12 
c

   8  do 10 i=1,n
c   	if (j.eq.943)pause
  10  cij(i,j)=cij(i,j)/zz
c
      if(iop.ge.6) write (666,204) (cij(i,j),i=1,n)
  12  cjbar(j)=un
c
      if(iop.ge.3) write (666,205) n,m
      if(non.ge.899) return
c
c     construction of explicit problem (change of variables)
c     ------------------------------------------------------
c
      dumax=obj
      do 24 i=1,n
      ss=-fi(i)
      if(ss.ge.zero) goto 16
      ss=-ss
c
      var=ximin(i)
      ximin(i)=un/ximax(i)
      ximax(i)=un/var
c
      do 14 j=1,m
  14  cij(i,j)=-cij(i,j)
c
  16  ximin(i)=ximin(i)/ss
      ximax(i)=ximax(i)/ss
c
      do 22 j=1,m
      if(cij(i,j)) 18,22,20
  18  cij(i,j)=cij(i,j)/ss
      goto 22
  20  cij(i,j)=cij(i,j)*ss
  22  continue
c
  24  dumax=dumax+un/ximin(i)
c
c     newton iterations in the subspace of active constraints
c     -------------------------------------------------------

      call scp0 (xi,ix,ximin,ximax,ai,si,x2,
     *           rj,jr,cj,cjbar,cg,cij,
     *           tol,cut,prec,obj,dumax,
     *           n,m,iact,jact,niter,iop,non)

c     return back to initial primal variables
c     ---------------------------------------
      do 26 i=1,n
      xi(i)=-xi(i)*fi(i)
      if(fi(i).le.zero) goto 26
      xi(i)=-un/xi(i)
  26  continue

      obj=obj+promix (xi,fi,n)

c     print final results
c     -------------------
      if(iop.ge.3) write (666,206) obj
      if(iop.gt.2) write (666,207) (j,jr(j),rj(j),cj(j),j=1,jact)
      if(iop.gt.3) write (666,208) (i,xi(i),-ix(i),i=1,n)

      do 28 j=1,m
      cj(j)=rj(j)
  28  rj(j)=zero

      do 30 j=1,jact
      ju=jr(j)
  30  rj(ju)=cj(j)

 900  return

 200  format(/t25,'===== conlin optimizer ====='/t30,18(1h=))
 201  format(' tolerance for convergence       tol ',e15.6/
     *       ' cut-off factor                  cut ',e15.6/
     *       ' precision                       prec',e15.6/
     *       ' information on design variables'/1x,31(1h-)/
     *       '    i   ix          ximin',
     *       '             xi          ximax             fi'/
     *       1x,69(1h-))
 202  format(2i5,4f15.4)
 203  format(' information on design constraints'/1x,33(1h-)/
     *       '    j jact             cj          cjbar',
     *       '             rj'/
     *       1x,54(1h-))
 204  format(' constraint gradient'/(8e10.2))
 205  format(' number of design variables',t30,i5/
     *       ' number of design constraints',t30,i5)
 206  format(' objective function (approximated)      ',e15.6)
 207  format(' design constraints (dual variable; value)'/1x,18(1h-)//
     *       (2i5,2f15.6))
 208  format(' design variables (-1: lower bound; +1: upper bound)'/
     *       1x,16(1h-)//(4(i5,f10.4,' (',i2,')')))
 291  format(' severe warning  -  ',
     *       ' lower bound larger than upper bound',i5)
 292  format(' warning  -  objective function derivative is zero',i5)
 293  format(' no feasible solution to this problem'//
     *       ' upper bound on constraint',i5,' should be increased to',
     *       ' at least',e15.6)
      end
      
c========================================================================
c========================================================================
      
      subroutine scp0 (xi,ix,ximin,ximax,ai,si,x2,
     *                 rj,jr,cj,cjbar,cg,cij,
     *                 tol,cut,prec,obj,dumax,
     *                 n,m,iact,jact,niter,iop,non)
      implicit real*8(a-h,o-z)
      dimension xi(n),ix(n),ximin(n),ximax(n),ai(n),si(n),x2(n),
     *          rj(m),jr(m),cj(m),cjbar(m),cg(m),cij(n,m)
c
      zero=0.d 00
      un  =1.d 00
      deux=2.d 00
      niter=0
c  max= nbre maximun d'itération : valeur standard = 40 (version fleury)
c (valeur modifié par ph. rigo le 9-6-98 pour application barge fso)
      max=500																	!test improve
      nls=0
      if(iop.gt.2) write (666,200)
c
c     newton iterations in the subspace of active constraints
c     -------------------------------------------------------
c
c     1. compute primal variables and constraints
c        ----------------------------------------
c
   1  prim=obj
      iact=0
      do 12 i=1,n
      ci=un
      di=zero
c
      if(jact.eq.0) goto 10
      do 6 j=1,jact
      ju=jr(j)
      var=cij(i,ju)*rj(j)
      if(var) 2,6,4
c
   2  ci=ci-var
      goto 6
c
   4  di=di+var
   6  continue
c
c     evaluation of primal variables
c
      if(di.eq.zero) goto 10
      di=dsqrt (ci/di)
      if(di.le.ximin(i)) goto 8
      if(di.ge.ximax(i)) goto 10
      ix(i)=0
      xi(i)=di
      iact=iact+1
      goto 12
c
   8  ix(i)=-1
      xi(i)=ximin(i)
      goto 12
c
  10  ix(i)= 1
      xi(i)=ximax(i)
  12  prim=prim+un/xi(i)
c
      do 14 j=1,m
      cj(j)=rj(j)
  14  rj(j)=zero
c
      if(jact.eq.0) goto 18
      do 16 j=1,jact
      	ju=jr(j)
      	rj(ju)=cj(j)
  16  continue
c
c     evaluation of primal constraints
c     *********************************
  18  dual=prim
      vmax=zero

      do 22 j=1,m 
c		if ((niter.eq.27).and.(j.eq.942)) pause
      	vio=promix (xi,cij(1,j),n)-un
      	cj(j)=vio
      	if(rj(j).le.zero) goto 20
      	dual=dual+rj(j)*vio
      	if(vio.lt.zero) vio=-vio
  20  	if(vio.gt.vmax) vmax=vio
  22  continue

      fac=vmax/cut
      viol=zero
      jact=0

      do 26 j=1,m
c		if ((niter.eq.27).and.(j.eq.940)) pause
      	vio=cj(j)
      	if(rj(j).gt.zero) goto 24
      	if(vio.le.fac) goto 26
  24  	jact=jact+1
      	jr(jact)=j
      	cj(jact)=vio
      	rj(jact)=rj(j)
      	viol=viol+vio*vio
  26  continue
c
      if(iop.gt.2) write (666,201) niter,nls,dual,prim,vmax,jact,iact
c      write (666,*) 'k=',k
c
c     termination tests
c
      if(vmax.lt.tol) goto 900
      niter=niter+1
      if(niter.gt.max) goto 901
c changer le 18-5-95 (cfr mr. zhang)      
      if(dual.gt.dumax) goto 902	

c
c     2. generation of quadratic subproblem
c        ----------------------------------
c
      prim=obj
      do 32 i=1,n
      xi2=xi(i)*xi(i)
      x2(i)=xi2
      bb=-un/xi2
      aa=-bb
c
      do 28 j=1,jact
      ju=jr(j)
      c=cij(i,ju)
      if (c.ge.zero) goto 28
c
      c=c/xi2
      aa=aa-rj(j)*c
  28  cij(i,ju)=-c
c
      aa=deux*aa/xi(i)
      ai(i)=aa
      if(ix(i).eq.0) goto 32
c
      prim=prim+un/xi(i)
      do 30 j=1,jact
      ju=jr(j)
  30  bb=bb-cij(i,ju)*rj(j)
      xi(i)=xi(i)-bb/aa
  32  continue
c
      do 36 j=1,jact
      zz=cj(j)
      ju=jr(j)
c
      do 34 i=1,n
      if(ix(i).eq.0) zz=zz+cij(i,ju)*xi(i)
  34  continue
c
  36  cjbar(j)=zz
c
c     3. solution of quadratic subproblem
c        --------------------------------
c
      told=dsqrt(viol)/cut
      prc=vmax*prec
      prc=prc*prc
      call scp1 (xi,ix,ximin,ximax,ai,si,
     *           rj,jr,cj,cjbar,cg,cij,
     *           told,prc,viol,prim,dumax,
     *           n,jact,iact,nodm,iop,m)
      nls=nls+nodm
c
c     4. recover original conlin data
c        ----------------------------
c
      do 38 j=1,jact
      ju=jr(j)
      do 38 i=1,n
      if (cij(i,ju).gt.zero) cij(i,ju)=cij(i,ju)*x2(i)
  38  cij(i,ju)=-cij(i,ju)
      goto 1
c
c     optimization loop on full conlin problem is completed
c     -----------------------------------------------------
c
 900  if(iop.gt.0) 
     *write (666,202) niter,nls,dual,prim,vmax,jact,iact,
     *              (jr(j),j=1,jact)
      return
c
c     error messages
c     --------------
c
 901  write (666,291) max
 	write (29 ,291) max						!sept06
      non=903
      go to 900
c
 902  write (666,292)
 	write (29 ,292)							!sept06
      non=899
      go to 900
c
 200  format(//1x,58(1h-)/
     *       ' iter  nls         dual       primal         viol',
     *       ' jact iact'/1x,58(1h-)/)
 201  format(2i5,3f13.4,2i5)
 202  format(' number of iterations            iter  ',i5/
     *       ' number of line searches         nls   ',i5/
     *       ' dual function                   dual  ',e15.6/
     *       ' primal function                 primal',e15.6/
     *       ' constraint violation            viol  ',e15.6/
     *       ' number of active constraints    jact  ',i5/
     *       ' number of active variables      iact  ',i5/
     *       ' list of active constraints'/(10i5))
 291  format(' maximum number of iterations (conlin) ',i5)
 292  format(' no feasible solution to this problem')
      end
      
c========================================================================
c========================================================================
      
      subroutine scp1 (xi,ix,ximin,ximax,ai,si,
     *                 rj,jr,cj,cjbar,cg,cij,
     *                 tol,prec,viol,obj,dumax,
     *                 n,m,iact,nodm,iop,mm)
      implicit real*8(a-h,o-z)
      dimension xi(n),ix(n),ximin(n),ximax(n),ai(n),si(n),
     *          rj(m),jr(m),cj(m),cjbar(m),cg(m),cij(n,mm)
c
      zero=0.d 00
      un  =1.d 00
      nodm=0
      igc=m
      jact=m
      maxodm=(m+n)*5
c
c     conjugate gradient
c
   2  nodm=nodm+1
      if(nodm.gt.maxodm) goto 900
      igc=igc+1
      if(igc.le.jact) goto 6
      igc=0
c
      do 4 j=1,m
   4  cg(j)=cj(j)
      goto 10
c
   6  gam=viol/old
      do 8 j=1,m
   8  cg(j)=cj(j)+gam*cg(j)
c
  10  sts=zero
      do 14 i=1,n
      s=zero
c
      do 12 j=1,m
      ju=jr(j)
  12  s=s+cij(i,ju)*cg(j)
c
      ss=s/ai(i)
      si(i)=ss
      if(ix(i).eq.0) sts=sts+s*ss
  14  continue
c
      if(sts.le.zero) sts=prec
      ptg=viol
c
c     one dimensional maximization
c
  20  ii=0
      iupt=0
      tau=ptg/sts
      do 22 j=1,m
      if(cg(j).ge.zero) goto 22
      tau1=-rj(j)/cg(j)
      if(tau1.ge.tau) goto 22
      tau=tau1
      ii=j
      iupt=1
  22  continue
c
      do 28 i=1,n
      if(ix(i)) 24,28,26
c
  24  if(si(i).le.zero) goto 28
      tau1=(ximin(i)-xi(i))/si(i)
      if(tau1.ge.tau) goto 28
      tau=tau1
      ii=-i
      iupt=2
      goto 28
c
  26  if(si(i).ge.zero) goto 28
      tau1=(ximax(i)-xi(i))/si(i)
      if(tau1.ge.tau) goto 28
      tau=tau1
      ii=i
      iupt=2
  28  continue
c
      if(iupt.ne.2) goto 40
c
      iact=iact+1
      i=ii
      if(i.gt.0) goto 30
      i=-i
      xx=ximin(i)
      goto 32
c
  30  xx=ximax(i)
  32  ix(i)=0
      obj=obj-un/xx
      do 34 j=1,m
      ju=jr(j)
  34  cjbar(j)=cjbar(j)+xx*cij(i,ju)
c
      sk2=si(i)*si(i)*ai(i)
      ptg=ptg+tau*sk2
      sts=sts+sk2
c
      if(iop.le.3) goto 20
      vio=dsqrt(ptg-tau*sts)
      write (666,200) vio,iact,ii
      goto 20
c
  40  do 42 j=1,m
  42  rj(j)=rj(j)+tau*cg(j)
c
      if(iupt.eq.0) goto 50
      rj(ii)=zero
      cg(ii)=zero
      igc=jact
      jact=jact-1
c
  50  prim=obj
      do 52 i=1,n
      xi(i)=xi(i)+tau*si(i)
      if(ix(i).eq.0) prim=prim+un/xi(i)
  52  continue
c
      old=viol
      viol=zero
      dual=prim
      do 70 j=1,m
      zz=zero
      if(rj(j).eq.zero) goto 70
c
      ju=jr(j)
      do 68 i=1,n
      if(ix(i).eq.0) zz=zz+cij(i,ju)*xi(i)
  68  continue
c
      zz=cjbar(j)-zz
      viol=viol+zz*zz
      dual=dual+rj(j)*zz
  70  cj(j)=zz
c
      vio=dsqrt(viol)
      if(iop.gt.2) write (666,201) nodm,dual,prim,vio,jact,iact
c changer le 18-5-95 (cfr mr. zhang)      
      if(dual.gt.dumax) goto 900
      if(vio.gt.tol) goto 2
      return
c
 900  if(iop.gt.0) write (666,290)
	if(iop.gt.0) write (29 ,290)
      return
 200  format(f49.4,i10,2i5)
 201  format(i10,3f13.4,2i5)
 290  format(' warning from convx1 - maximization algorithm failed')
      end
      
      function promix (x,f,n)
c     ========================
      implicit real*8(a-h,o-z)
      dimension x(n),f(n)
      promix=0.d 00     
      do 3 i=1,n
      if(f(i)) 1,3,2
   1  promix=promix-f(i)/x(i)
      goto 3
   2  promix=promix+f(i)*x(i)
   3  continue
      return
      end
