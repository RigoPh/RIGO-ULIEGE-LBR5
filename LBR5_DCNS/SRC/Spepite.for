      subroutine spepite(obj,nn,width,totalstrait)                           !redressage !septembre 2007 dad
!      subroutine spepite(nn,width,totalstrait)                           !redressage !septembre 2007
      use sharedvar
      implicit real*8(a-h,o-z)
	dimension oops(120,9)                                                  !redressage !septembre 2007

c**********************************************************************
c
c     subroutine pepite
c     calcul du cout de redressage par panneau.
c	et sensibilités
c
c     crée: 22.03.06       j.pirard                     
c***********************************************************************
c
c	paramètres
c	delta_cadre	:epsa
c	delta_lisse	:epsr
c	thickness	:delta
c	famille		:fami
c	special		:toujours 0 car il ne s'agit pas d'une structure spéciale (bulbe...)
c	double_fond	:itype=6 alors double_fond = 1
c	grade		:spec, matériau du panneau
c	sompoids_l0ngueur_lot : plot : poids des éléments par lot/longueur du lot. 
c	poidstole_marsurfpan  : ptol : poids de la tôle d'un lot/ surface de tôle.
c
c	formule pepite
c	a:intermediate[0] = (datad.delta_cadre-2822.565789473684e0)/74.90830341976067e0;
c	b:intermediate[1] = (datad.delta_lisse-711.6447368421053e0)/45.298042864427465e0;
c	c:intermediate[2] = (datad.thickness-7.0855263157894735e0)/3.410043752894369e0;
c	d:intermediate[3] = (datad.famille-2.7565789473684212e0)/0.9420051217244927e0;
c	e:intermediate[4] = (datad.special_num-0.03289473684210526e0)/0.1789507001514622e0;
c	f:intermediate[5] = (datad.double_fond_num-0.046052631578947366e0)/0.21029200100905684e0;
c	g:intermediate[6] = (datad.grade_num-0.19078947368421054e0)/0.39422239045175456e0;
c	h:intermediate[7] = (datad.sompoids_l0ngueur_lot-4.985748581635476e0)/3.680087037555551e0;
c	o:intermediate[8] = (datad.poidstole_marsurfpan-0.06084666512508383e0)/0.020034653830364162e0;
c	p:intermediate[9] = tanh(1.9368069209472492e0 + (-0.9264288509304361e0 * intermediate[0]) + (-1.3851816262063843e0 * intermediate[1]) + (-0.9130433232870326e0 * intermediate[2]) + (0.8329484301877387e0 * intermediate[3]) + (-0.5563069515492396e0 * intermediate[4]) + (-0.8472889924229183e0 * intermediate[5]) + (-0.586404310688777e0 * intermediate[6]) + (1.5063505606322518e0 * intermediate[7]) + (-4.066987573526498e0 * intermediate[8]));
c	r:intermediate[10] = tanh(0.8087474077250932e0 + (-0.34896105918057957e0 * intermediate[0]) + (-0.4592147665094478e0 * intermediate[1]) + (0.3306327933088029e0 * intermediate[2]) + (0.6757548968905716e0 * intermediate[3]) + (-0.39587242378578624e0 * intermediate[4]) + (-0.4333421624891615e0 * intermediate[5]) + (-0.4087872241356317e0 * intermediate[6]) + (0.4932501122941787e0 * intermediate[7]) + (-2.1446197463678445e0 * intermediate[8]));
c	s:intermediate[11] = tanh(-0.10228980686314161e0 + (-1.2263729691363052e0 * intermediate[0]) + (-1.0167071742037233e0 * intermediate[1]) + (-0.1725226923833523e0 * intermediate[2]) + (0.08599557094213019e0 * intermediate[3]) + (0.39550298448896226e0 * intermediate[4]) + (0.17610832730222192e0 * intermediate[5]) + (0.32700148711362204e0 * intermediate[6]) + (-1.208027714084693e0 * intermediate[7]) + (-2.2654562759753216e0 * intermediate[8]));
c	t:intermediate[12] = tanh(-0.1822176347472198e0 + (-0.036204229434917856e0 * intermediate[0]) + (0.48046284302543396e0 * intermediate[1]) + (1.8389296314588444e0 * intermediate[2]) + (0.6332866724703521e0 * intermediate[3]) + (-1.1473156019349544e0 * intermediate[4]) + (-0.9035691846240345e0 * intermediate[5]) + (0.3989099671440577e0 * intermediate[6]) + (-0.4678695574043407e0 * intermediate[7]) + (0.3583131586888217e0 * intermediate[8]));
c	u:intermediate[13] = tanh(5.872989176135237e0 + (0.3512696038203212e0 * intermediate[0]) + (2.115126151076024e0 * intermediate[1]) + (-2.303121469828879e0 * intermediate[2]) + (-2.1199873056315917e0 * intermediate[3]) + (-0.16875097228308644e0 * intermediate[4]) + (-0.8363746917958408e0 * intermediate[5]) + (1.893139241719546e0 * intermediate[6]) + (3.934191388280177e0 * intermediate[7]) + (-0.09820356987358925e0 * intermediate[8]));
c	v:intermediate[14] = 0.17659278965335456e0 + (0.15298110516796812e0 * (3.4798485158795147e0 + (1.1383230531544564e0 * intermediate[9]) + (-1.6408502476638214e0 * intermediate[10]) + (0.7660321433985156e0 * intermediate[11]) + (0.9502124670035074e0 * intermediate[12]) + (-3.311401430855795e0 * intermediate[13])));
c
c	30 lots max, 10 familles max.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccc lecture var de design (= sensibcout) cccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
 	rewind 305	!extension neto	
	rewind(302)  !dad

	read(305,*) vlength		!extension neto


	do i=1,neto
       read(305,*) nopan,vlarg1,vepaiss1 ! nécessaire pour lire entr					!extension neto


	 read(305,*) vhac1,veac1,vlsc1,vesc1,deltac1,vdiffc1				!18.03.04	!extension neto
	 read(305,*) vhar1,vear1,vlsr1,vesr1,deltar1,entrsc(i)							!extension neto
	
	 read(302)deltacsc(i),deltar(i),vepaiss(i),vhac(i),veac(i),			!extension neto	!fev2007
     *          vlsc(i),vesc(i),vhar(i),vear(i),vlsr(i),vesr(i),			!extension neto	!fev2007
     *		  philsc(i),qsc(i),vdiffc(i)								!18.03.04



!     epaisseur de corrosion                                !redressage !septembre 2007
!     -----------------------                               !redressage !septembre 2007
!     dcor = epaisseur de corrosion pour bordé              !redressage !septembre 2007
       dcorsc(i)  = corro(i,1)	        !corrosion        !redressage !septembre 2007
	 dcorsc2(i) = corro(i,2)		    !corrosion        !redressage !septembre 2007
	 dcorsc3(i) = corro(i,3)		    !corrosion        !redressage !septembre 2007
	 vepaiss(i) = vepaiss(i) + dcorsc(i)                  !redressage !septembre 2007
!	dcor2   ! epaisseur de corrosion pour cadres          !redressage !septembre 2007
	 veac(i)  = veac(i) + dcorsc2(i)                      !redressage !septembre 2007
	 vesc(i)  = vesc(i) + dcorsc2(i)                      !redressage !septembre 2007
       vdiffc(i)	= vdiffc(i)	 + dcorsc2(i)				  !redressage !septembre 2007
!	dcor3   ! epaisseur de corrosion pour lisses          !redressage !septembre 2007
	 vear(i)  = vear(i) + dcorsc3(i)                      !redressage !septembre 2007
	 vesr(i)  = vesr(i) + dcorsc3(i)                      !redressage !septembre 2007

	enddo
	rewind 302				!dad redressage

	mui=1
      call pepitemain (width,totalstrait,mui)
	temp=totalstrait
cccccccccccccccccccccccccccccccc
ccccc  redressage dans obj  cccc
cccccccccccccccccccccccccccccccc
!	obj = obj + totalstrait   !dad
***************************************************
********  calcul sensibilites  ********************
***************************************************
ccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccc   calcule dif finies +-12%   cccccccccc
ccccccccc		matrix oops(neto,9)      cccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccc
	mui=0
	do nel=1,neto
      do j=1,9
ccccccccccccccccccccccc
ccccccccccccccccccccccc
	if(itype(nel).ne.5) then   
	! check pour voir si ce sont des épontilles
	! si oui, vari reprend la valeur de l'itération
	! précédente
	if(j.eq.1) then

      vari = vepaiss(nel)
	vari1=	vari+0.01*vari
	vari2=  vari-0.01*vari
	vepaiss(nel)=vari1
		call pepitemain(width,totalstrait,mui)
	vepaiss(nel)=vari2
		call pepitemain(width,totalstrait,mui)
	 oops(nel,j)=(total1-total2)/(0.02*vari)
	vepaiss(nel)=vari
	endif

!**********************
	if(j.eq.2) then
      vari = vhac(nel)
	vari1=	vari+0.01*vari
	vari2=vari-0.01*vari
	vhac(nel)=vari1
	    call pepitemain(width,totalstrait,mui)
	vhac(nel)=vari2
	    call pepitemain(width,totalstrait,mui)
	 oops(nel,j)=(total1-total2)/(0.02*vari)
	vhac(nel)=vari
	endif
!**********************
	if(j.eq.3) then
      vari = veac(nel)
	vari1=	vari+0.01*vari
	vari2=vari-0.01*vari
	veac(nel)=vari1
	    call pepitemain(width,totalstrait,mui)
	veac(nel)=vari2
	    call pepitemain(width,totalstrait,mui)
	 oops(nel,j)=(total1-total2)/(0.02*vari)
	veac(nel)=vari
	endif
!**********************
	if(j.eq.4) then
      vari = vlsc(nel)
	vari1=	vari+0.01*vari
	vari2=vari-0.01*vari
	vlsc(nel)=vari1
	    call pepitemain(width,totalstrait,mui)
	vlsc(nel)=vari2
	    call pepitemain(width,totalstrait,mui)
	 oops(nel,j)=(total1-total2)/(0.02*vari)
	vlsc(nel)=vari
	endif
!**********************
	if(j.eq.5) then
      vari = deltacsc(nel)
	vari1=	vari+0.01*vari
	vari2=vari-0.01*vari
	deltacsc(nel)=vari1                                                  !redressage !septembre 2007
	    call pepitemain(width,totalstrait,mui)
	deltacsc(nel)=vari2                                                  !redressage !septembre 2007
	    call pepitemain(width,totalstrait,mui)
      oops(nel,j)=(total1-total2)/(0.02*vari)
	deltacsc(nel)=vari                                                   !redressage !septembre 2007
	endif
!**********************
	if(j.eq.6) then
      vari = vhar(nel)
	vari1=	vari+0.01*vari
	vari2=vari-0.01*vari
	vhar(nel)=vari1
	    call pepitemain(width,totalstrait,mui)

	vhar(nel)=vari2
	    call pepitemain(width,totalstrait,mui)
	 oops(nel,j)=(total1-total2)/(0.02*vari)
	vhar(nel)=vari
	endif
!**********************
	if(j.eq.7) then
      vari = vear(nel)
	vari1=	vari+0.01*vari
	vari2=vari-0.01*vari
	vear(nel)=vari1
	    call pepitemain(width,totalstrait,mui)
	vear(nel)=vari2
	    call pepitemain(width,totalstrait,mui)
	 oops(nel,j)=(total1-total2)/(0.02*vari)
	vear(nel)=vari
	endif
!**********************
	if(j.eq.8) then
      vari = vlsr(nel)
	vari1=	vari+0.01*vari
	vari2=vari-0.01*vari
	vlsr(nel)=vari1
	    call pepitemain(width,totalstrait,mui)
	vlsr(nel)=vari2
	    call pepitemain(width,totalstrait,mui)
	 oops(nel,j)=(total1-total2)/(0.02*vari)
	vlsr(nel)=vari
	endif
!**********************
	if(j.eq.9) then
      vari = deltar(nel)
	vari1=	vari+0.01*vari
	vari2=vari-0.01*vari
	deltar(nel)=vari1
	    call pepitemain(width,totalstrait,mui)
	deltar(nel)=vari2
	    call pepitemain(width,totalstrait,mui)
	 oops(nel,j)=(total1-total2)/(0.02*vari)
	deltar(nel)=vari
	endif
!cccccccccccccccccccccccc
	else
	oops(nel,j)=0.
	endif
!cccccccccccccccccccccccc		
 	enddo
	enddo		
cccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccc calcul vecteur derredress(i) cccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccc
	call annuld(derredress,nn)
      ncont = 0
	do j=1,neto
	 nbrxi = nvar(j)
	 do i=1,nbrxi
	  ii = i + ncont
	  i1 = nxit(i,j)
	derredress(ii)=oops(j,i1)
	 enddo
	 ncont = ncont + nbrxi
	enddo
cccccccccccccccccccccccccccccccccccccccccccccc	
	totalstrait=temp
	return
	end
****************************************************************************
****************************************************************************
****************************************************************************
      subroutine pepitemain(width,totalstrait,mpix)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccc  la fonction analytique "cout de redressage est   cccccccccc
ccccccccccc  calculee ici. resultat : var "totalstrait" (euro) ccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      use sharedvar
      implicit real*8(a-h,o-z)
      integer *2 fami,llot(neto),lfami(neto)
      dimension plot(neto),ptol(neto)
	dimension ola(neto),v(neto)
      dimension vvsurf(neto),tpmcarre(neto)										!18.03.04
      dimension vsurface(neto)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc			
	plot = 0.
	ptol = 0.
	red=0.
	ola=0.
      v=0.
	vvsurf=0.
	tpmcarre=0.
	vsurface=0.

	llot=0
	lfami=0
	
	eqpc1=eqp*c1*1000.0  ! (=euro/heure) ,pour transfert de rro en euro


	rewind(7777)
c	write(*,*) c1
c	do kk=1,neto
c	write(*,*) itype(kk)
c	enddo
	do 123 i=1,neto

	if(itype(i).ne.5) then
		
	read(7777)fami,lot,vsigy,vspec,vhight
	lfami(i)=fami
	llot(i)=lot


c	write(*,*)fami,lot,vsigy,vspec,vhight
ccccccccccccccccccccccccccccccccccccccccccccccccc
cccccc variables du panneau ccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccc
	delta = vepaiss(i)
!	epsa = deltac(i)          !redressage !septembre 2007
	epsa = deltacsc(i)        !redressage !septembre 2007
	epsr = deltar(i)
	hac = vhac(i)
	har = vhar(i)
	eac = veac(i)
	ear = vear(i)
	lsc = vlsc(i)
	esc = vesc(i)
	lsr = vlsr(i)
	esr = vesr(i)
	vhight=-vhight




ccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccc	
	poids1=	2*(vhight*(width/epsa)*((eac*hac)+(lsc*esc))
     *+(vhight/epsr)*((ear*har)+(lsr*esr))*width
     *+width*vhight*delta)*vspec*1.2/9810.
	 vsurface(i)=width*vhight


ccccccccccccccccccccccccccccccccccccccccccccccccc
cccccc  transform unit messure  ccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccc
	delta = delta*1000.
	epsa = epsa*1000.
	epsr = epsr*1000.
	hac = hac*1000.
	har = har*1000.
	eac = eac*1000.
	ear = ear*1000.
	lsc = lsc*1000.
	esc = esc*1000.
	lsr = lsr*1000.
	esr = esr*1000.		
     
ccccccccccccccccccccccccccccccccccccccccccccccccc      
     	
	  if (vsigy.le.200000000) then
	  write (66,*)'le redressage n''est calculé que pour de l''acier'
	  goto 123
	  endif

		ptol(i) = delta*vspec/10000000.

		do j=1,neto
		if (lot.eq.j) then
		plot(j) = plot(j)+ poids1
c	if(fami.ne.0.) then
c	vvsurf(j)=vvsurf(j)+vsurface(i)
c	endif
	    endif
c		ptol(i) = delta*vspec/10000000.
c	ptol(i)=0.075	

		enddo
	endif
123	continue
		rewind(7777)


		do jk=1,neto
	if(plot(jk).lt.125.) then
	tpmcarre(jk)=1.-plot(jk)*0.8/125.
	else
	tpmcarre(jk)=0.2-(plot(jk)-125.)*0.15/(1025.-125.)
	endif

		enddo
	do kk=1,neto
	if((lfami(kk).ne.0).and.(llot(kk).ne.0)) then
	klm=llot(kk)
	ola(kk)=tpmcarre(klm)
	else
	klm=0      !dad 10.12.2007
	ola(kk)=0.
	endif

	v(kk) = ola(kk)*vsurface(kk)  !pour avoir coût en heures

	
	if(mpix.eq.1) then
	write(7778,*) kk
      if (klm.ne.0) then
		write(7778,226) ola(kk),v(kk),plot(klm)
	else
	    plot_null=0.
      	write(7778,226) ola(kk),v(kk),plot_null
	endif

c	write(7778,*) epsr
c	write(7778,*) 'int 0',a(i)
c	write(7778,*) 'int 1',b(i)
c	write(7778,*) 'int 2',c(i)
c	write(7778,*) 'int 3',d(i)
c	write(7778,*) 'int 4',e(i)
c	write(7778,*) 'int 5',f(i)
c	write(7778,*) 'int 6',g(i)
c	write(7778,*) 'int 7',h(i)
c	write(7778,*) 'int 8',o(i)
c	write(7778,*) 'int 9',p(i)
c	write(7778,*) 'int 10',r(i)
c	write(7778,*) 'int 11',s(i)
c	write(7778,*) 'int 12',t(i)
c	write(7778,*) 'int 13',u(i)
c	write(7778,*) 'int 14',ola(i)

226	format (5(2x,f10.3))
c	write(67,*) 'panneau',i,' cout de redressage',v(i),'heures'
	endif


	red=red+v(kk)

	enddo				!boucle kk


	totalstrait = red*eqpc1				!!! catalin
	
	if(mpix.eq.1) then
	write(7778,*) ' '
	write(7778,*) 'cout total de redressage = ', red,' heures'
	write(7778,*) 'cout total de redressage = ', totalstrait,' euro'
	write(7778,*) ' '
	endif

	rewind(7777)				!!! catalin				


c       write(66,*) '***total  straightening cost=',totalstrait,' euro,$'	!!! catalin
c       write(67,*) '***total  straightening cost=',totalstrait,' euro,$'	!!! catalin
c
c       write(66,*) '*** total cost with straightening=',cout  ,' euro,$'
c	 write(66,*) '      !!! using the simplified lbr5 model !!! '
c      write(66,*) ' this result will differ using a detailled database'
c       write(67,*) '*** total cost with straightening=',cout  ,' euro,$'
c	 write(67,*) '      !!! using the simplified lbr5 model !!! '
c       write(67,*) ' this result will differ using a detailled database'
	

	return
	end
 
 
