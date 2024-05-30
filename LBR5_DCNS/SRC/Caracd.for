      subroutine caracd(e,eta,a,b,c,d,v,f,g,h,o,p,q,const,sh,
     *  fam,mt,ksr,ksa,kst,phil,rayon,w,const2,const3,
     *  epais,itype,isect,dpo,spec)                                                                             !février 2004
      implicit real *8(a-h,o-z)
      dimension const(74),const2(6,9),const3(2,10),fam(6)
	dimension dpbord(9),dpaig(9),dpraid(9),dptrav(9),dpo(9)  !!!aout04

      pi=2.d00*acos(0.)
c***********************************************************************
c     subroutine caract(opti)
c     calcul des derivees des coefficients de raideur du panneau
c     par rapport aux variables de conception.
c
c       a=epsa, b=epsr     c=delta,  w=width
c       d=hya,  v=dya,      f=wya,  g=tya,  (aiguilles)
c       h=hxr,  o=dxr,      p=wxr,  q=txr,  (raidisseurs)
c
c     modif : 23-8-95                                   créer : 24-1-94
c***********************************************************************
c      var. de concept. = epaisseur du bordage (k=1)
c      ---------------------------------------------
c        const(1) = d  -> const2(1,1)=d(d)/d(épaiss)
c        const(2) = k  -> const2(2,1)=d(k)/d(épaiss)
c        const(4) = hy -> const2(3,1)=d(hy)/d(épaiss)   aiguille
c        const(6) = ry -> const2(4,1)=d(ry)/d(épaiss)   aiguille
c        const(10)= hx -> const2(5,1)=d(hx)/d(épaiss)   raidisseur
c        const(12)= rx -> const2(6,1)=d(rx)/d(épaiss)   raidisseur

c        traverses  hx -> const3(1,mt)=d(h)/d(épaiss)
c                   rx -> const3(2,mt)=d(r)/d(épaiss)

c      var. de concept. relative aux aig.(k=2,5) et aux raid.(k=6,9).
c      --------------------------------------------------------------
c           aiguilles                         raidisseurs
c      constante           dérivée(k=2,5)    constante   dérivée(k=6,9)
c    1 const(3)= oy -> const2(1,k)     const( 9)= ox -> const2(1,k)
c    2 const(4)= hy -> const2(2,k)     const(10)= hx -> const2(2,k)
c    3 const(5)= sy -> const2(3,k)     const(11)= sx -> const2(3,k)
c    4 const(6)= ry -> const2(4,k)     const(12)= rx -> const2(4,k)
c    5 const(7)= ty -> const2(5,k)     const(13)= tx -> const2(5,k)
c    6 const(8)= ly -> const2(6,k)     const(14)= lx -> const2(6,k)

        bksa=1.
        if(ksa.eq.1)bksa=-1.
        bksr=1.
        if(ksr.eq.1)bksr=-1.
	if (itype.ne.5) then											!février 2004
c  - dérivée par rapport à l'épaisseur (delta=c) : const2(i,1) i=1,2
      if(c.le.0.000001)  goto 13
        const2(1,1)=const(1)/c
      const2(2,1)=3.*const(2)/c
        const2(3,1)=bksa* const(3)/2.
      const2(4,1)=dabs(const(4))
        const2(5,1)=bksr* const(9)/2.
      const2(6,1)=dabs(const(10))

c       aiguilles (dérivées des const(3) à const(8) )
c  - dérivée par rapport à h(âme) (d=hya) : const2(i,2) i=1,6

  13    if(d.le.0.000001)  goto 7
        const2(1,2)=e*v/a
      const2(2,2)=e*(d*v+f*g+v*c*0.5)*bksa/a
      const2(4,2)=(v*(c/2.+d)**2+2.*f*g*(c/2.+d+g/2.))*e/a
      const2(5,2)=(sh*v**3)/(3.*a)
      const2(6,2)=-const(5)*bksa*fam(2)
c       - dérivée par rapport à ep(âme) (v=dya) : const2(i,3) i=1,6
   7    if(v.le.0.000001)  goto 3
        const2(1,3)=e*d/a
      const2(2,3)=e*d*(c/2.+d/2.)*bksa/a
      const2(4,3)=d*(d*d/12.+(c/2.+d/2.)**2)*e/a
      const2(5,3)=sh*d*v*v/a
c       - dérivée par rapport à l(semelle) (f=wya) : const2(i,4) i=1,6
   3  if(f.le.0.000001)  goto 4
        const2(1,4)=e*g/a
      const2(2,4)=e*g*(c/2.+d+g/2.) *bksa/a
      const2(3,4)=const(5)/f
      const2(4,4)=e*g*(g*g/12.+(c/2.+d+g/2.)**2) /a
      const2(5,4)=(sh*g**3)/(3.*a)
      const2(6,4)=const(8)/f
c	- dérivée par rapport à l'entredistance (a=epsa) : const2(i,5) i=1,6	
   4  if(a.le.0.000001)  goto 10  ! modifié en avril 2003 (a au lieu de f)
      const2(1,5)=-const(3)/a                                                
      const2(2,5)=-const(4)/a                                 
      const2(3,5)=-const(5)/a                                    
      const2(4,5)=-const(6)/a                                     
      const2(5,5)=-const(7)/a                                      
      const2(6,5)=-const(8)/a                                      
	
c	raidisseurs	 (dérivées des const(9) à const(14) )
c	- dérivée par rapport à h(âme) (h=hxr) : const2(i,6) i=1,6
  10	if(h.le.0.000001)  goto 8
        const2(1,6)=e*o/b
      const2(2,6)=e*(o*h+p*q+o*c*0.5)*bksr/b
      const2(4,6)=(o*(c/2.+h)**2+2.*p*q*(c/2.+h+q/2.))*e/b
      const2(5,6)=(sh*o**3)/(3.*b)
      const2(6,6)=-const(11)*bksr*fam(4)

c       - dérivée par rapport à ep(âme) (o=dxr) : const2(i,7) i=1,6
    8 if(o.le.0.000001)  goto 5
        const2(1,7)=e*h/b
      const2(2,7)=e*h*(c/2.+h/2.)*bksr/b
      const2(4,7)=h*(h*h/12.+(c/2.+h/2.)**2)*e/b
      const2(5,7)=sh*h*o*o/b
c       - dérivée par rapport à l(semelle) (p=wxr) : const2(i,8) i=1,6
    5 if(p.le.0.000001)  goto 6
        const2(1,8)=e*q/b
      const2(2,8)=e*q*(c/2.+h+q/2.) *bksr /b
      const2(3,8)=const(11)/p
      const2(4,8)=e*q*(q*q/12.+(c/2.+h+q/2.)**2) /b
      const2(5,8)=(sh*q**3)/(3.*b)
      const2(6,8)=const(14)/p
c	- dérivée par rapport à l'entredistance (b=epsr) : const2(i,9) i=1,6	
    6 if(b.le.0.000001)  goto 9
      const2(1,9)=-const(9)/b                                               
      const2(2,9)=-const(10)/b                                 
      const2(3,9)=-const(11)/b                                    
      const2(4,9)=-const(12)/b                                     
      const2(5,9)=-const(13)/b                                      
      const2(6,9)=-const(14)/b                                      
	else															!février 2004
c	- dans le cas des épontille nous travaillons avec de nouvelles variables	!février 2004
c			k=1 --> hya												!février 2004
c			k=2 --> dya												!février 2004
c			k=3 --> wya												!février 2004
c			k=4 --> epais											!février 2004
c			k=5 --> epsa											!février 2004
c	- dérivée par rapport hya pour épontilles						!février 2004
      if(d.le.0.000001)  goto 15										!février 2004
      	if (isect.eq.3) then										!février 2004
	const2(1,1)=2*e*v/a								!doy			!février 2004
      const2(2,1)=0									!dhy			!février 2004
      const2(3,1)=e*v/a								!dsy			!février 2004
      const2(4,1)=(2*e/a)*((f*(d+g)**2)-((f-v)*d**2))	!dry			!février 2004
      const2(5,1)=e*v*v*v/(3*a)						!dty			!février 2004
		elseif (isect.eq.1) then									!février 2004
	const2(1,1)=pi*e*epais/a										!février 2004
      const2(2,1)=0													!février 2004
      const2(3,1)=pi*e*epais/(4*a)									!février 2004
      const2(4,1)=(pi*e/(16*a))*((d**3)-(d-2*epais)**3)				!février 2004
      const2(5,1)=const2(4,1)											!février 2004
		elseif (isect.eq.2) then									!février 2004
	const2(1,1)=4*e*epais/a											!février 2004
      const2(2,1)=0													!février 2004
      const2(3,1)=e*epais/(a)											!février 2004
      const2(4,1)=e*(d*d*d-((d-2*epais)**3))/(3*a)					!février 2004
      const2(5,1)=e*epais*3*(d-epais)*(d-epais)/(2*a)					!février 2004
		endif														!février 2004
c	-dérivée par rapport à dya pour épontille						!février 2004
 15   if(v.le.0.000001)  goto 16										!février 2004
      const2(1,2)=2*e*d/a												!février 2004
      const2(2,2)=0													!février 2004
      const2(3,2)= e*d/a												!février 2004
	const2(4,2)=2*e*d*d*d/(3*a)										!février 2004
      const2(5,2)=e*d*v*v/a											!février 2004
c       - dérivée par rapport à wya pour épontille					!février 2004
  16  if(f.le.0.000001)  goto 17										!février 2004
      const2(1,3)=2*e*g/a												!février 2004
      const2(2,3)=0													!février 2004
      const2(3,3)=0													!février 2004
      const2(4,3)=(2*e/(3*a))*(((d+g)**3)-d**3)						!février 2004
      const2(5,3)=e*g*g*g/(3.*a)										!février 2004
c	  -dérivée par rapport à epais pour épontille					!février 2004
  17  if(epais.le.0.000001)  goto 18									!février 2004
        if(isect.eq.1) then											!février 2004
	const2(1,4)=pi*e*(d-2*epais)/a									!février 2004
      const2(2,4)=0													!février 2004
      const2(3,4)=pi*e*(d-2*epais)/(4*a)								!février 2004
      const2(4,4)=(e*pi/(8*a))*(d-2*epais)**3							!février 2004
      const2(5,4)=const2(4,4)											!février 2004
		elseif(isect.eq.2) then										!février 2004
	const2(1,4)=4*e*(d-2*epais)/a									!février 2004
      const2(2,4)=0													!février 2004
      const2(3,4)=e*(d-4*epais)/a										!février 2004
      const2(4,4)=e*2*((d-2*epais)**3)/(3*a)							!février 2004
      const2(5,4)=e*(((d-epais)**2)*(d-4*epais))/(2*a)				!février 2004
		else														!février 2004
	write(66,*) 'erreur cette section n''a pas de variable'' epais'	!février 2004		
	stop															!février 2004	
		endif														!février 2004
c	- dérivée par rapport à l'entredistance epsa pour épontille		!février 2004
   18  if(a.le.0.000001)  goto 9  									!février 2004
      const2(1,5)=-const(3)/a											!février 2004	                    
      const2(2,5)=-const(4)/a											!février 2004	
      const2(3,5)=-const(5)/a											!février 2004		
      const2(4,5)=-const(6)/a											!février 2004
      const2(5,5)=-const(7)/a											!février 2004
	endif															!février 2004
   9  continue

c   les traverses
c   ----------------
	bkst=1.
        if(kst.eq.1)bkst=-1.
      if(mt.eq.0) goto 2
      do 1 i=1,mt
      j=6*(i-1)
      const3(1,i)=const(15+j)*bkst/2.        ! h trav
      const3(2,i)=dabs(const(16+j))          ! r trav
    1 continue
    2 continue

c     calcul de la derivee des poids  !!!aout04 (ainsi que tout ce qui suit)
c     ------------------------------
	
	kksa=(-1)**ksa
      specc=spec*w
	call annuld(dpo,9)
	call annuld(dpbord,9)
	call annuld(dpaig,9)
	call annuld(dpraid,9)
	call annuld(dptrav,9)

c  - dérivée par rapport à l'épaisseur (delta=c) :	
      dpbord(1)=(rayon*phil*pi/180.)*specc
c  - dérivée par rapport à h(âme) (d=hya) :
      dpaig(2)=((const2(1,2)*(rayon+kksa*d/2.)+ const(3)*kksa/2.)
     *			*phil*pi/180.) *specc/e
c  - dérivée par rapport à ep(âme) (v=dya) :
      dpaig(3)=(const2(1,3)*(rayon+kksa*d/2.)*phil*pi/180.)*specc/e
c  - dérivée par rapport à l(semelle) (f=wya) :
      dpaig(4)=(const2(1,4)*(rayon+kksa*d/2.)*phil*pi/180.)*specc/e
c  - dérivée par rapport à l'entredistance (a=epsa) :
	dpaig(5)=(const2(1,5)*(rayon+kksa*d/2.)*phil*pi/180.)*specc/e
c  - dérivée par rapport à h(âme) (h=hxr) :
	dpraid(6)=(const2(1,6)*rayon*phil*pi/180.)*specc/e
c  - dérivée par rapport à ep(âme) (o=dxr) :
	dpraid(7)=(const2(1,7)*rayon*phil*pi/180.)*specc/e
c  - dérivée par rapport à l(semelle) (p=wxr) :
	dpraid(8)=(const2(1,8)*rayon*phil*pi/180.)*specc/e
c  - dérivée par rapport à l'entredistance (b=epsr) :
	dpraid(9)=(const2(1,9)*rayon*phil*pi/180.)*specc/e

	do i=1,9
	  dpo(i)=(dpbord(i)+dpaig(i)+dpraid(i)+dptrav(i))/w
      enddo
      return
      end
