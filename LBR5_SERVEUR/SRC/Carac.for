      subroutine carac(e,eta,a,b,c,d,v,f,g,h,o,p,q,r,s,t,u,z,sh,fam,mt,
     *                 ksr,ksa,kst,spec,po,phil,rayon,w,delt,delt2,
     *                 epais,itype,isect,
     *                 aire,aiy,aix,sredy,sredx,tork)

      use param_section,Z_=>Z,FAM_=>FAM,ITYPE_=>ITYPE,E_=>E,ETA_=>ETA,
     *SH_=>SH,A_=>A,ISECT_=>ISECT,
     *B_=>B,SPEC_=>SPEC,C1_=>C1,XI_=>XI,POT_=>POT

	implicit double precision(a-h,o-z)
	
      dimension z(74),fam(6),r(10),s(10),t(10),u(10)
c**********************************************************************
c
c     subroutine caract
c     calcul des coefficients de raideur du panneau.
c
c       a=epsa, b=epsr      c=delta,  w=width
c       d=hya,  v=dya,      f=wya,  g=tya,  (cadres)
c       h=hxr,  o=dxr,      p=wxr,  q=txr,  (raidisseurs)
c       r=hxtr, s=dxtr,     t=wxtr, u=txtr, (traverses)
c
c     modif: 8-6-96                            création: 1988 thèse ph. rigo
c            7-11-2000 : calcul de delt2
c***********************************************************************
c      const(1)= d       const(2)= k
c      const(3)= oy      const( 9)= ox
c      const(4)= hy      const(10)= hx
c      const(5)= sy      const(11)= sx
c      const(6)= ry      const(12)= rx
c      const(7)= ty      const(13)= tx
c      const(8)= ly      const(14)= lx

c     pot = section des traverses (somme)
c     po  = poids total par metre (sans les nerv. complémentaires)

      pi=2.d00*asin(1.d00)
      pot=0.
      kksa=(-1)**ksa
      kksr=(-1)**ksr

	if (itype.ne.5) then									!février 2004
c     bordage
      z(1)=e*c/(1.-eta*eta)
      z(2)=z(1)*c*c/12.

c     cadres
      z(3)=e*(d*v+f*g)/a
      z(4)=e*(d*v*(c/2.+d/2.)+f*g*(c/2.+d+g/2.))*kksa/a
      temp=sh*5.*f*g/(6.*a)
      z(5)=temp*fam(1)
      z(6)=e*(v*d**3/12.+v*d*(c/2.+d/2.)**2+f*g**3/12.+
     1          f*g*(c/2.+d+g/2.)**2)/a
      z(7)=sh*(d*v**3+f*g**3)/(3.*a)
      z(8)=-(c/2.+d+g/2.)*temp*fam(2)*kksa
	else								!épontilles					!février 2004
c     bordage															!février 2004
	gg=e/2															!février 2004
      z(1)=0															!février 2004
      z(2)=0															!février 2004
c     epontille														!février 2004
	if(isect.eq.3)then							! double t			!février 2004
	aire=2*d*v+2*f*g												!février 2004
	aiy=2*((f*((d+g)**3))-((f-v)*(d**3)))/3		!iy  (axe fort)		!20.05.04	axe neutre à mi-hauteur
c	aiy=	2*(f*g**3)/12.+ v*(2*d)**3/12.+v*2*d*(d+g/2.)**2
c     *		+f*g*(2*d+g)**2											!iy  (axe fort) axe neutre dans semelle 20.02.04
	aix= (2*g*f*f*f)/12							!ix  (axe faible)	!février 2004
	sredy= v*2*d													!février 2004
	sredx= 5*f*g/3													!février 2004
	tork=((2*f*g*g*g)+(2*d*v*v*v))/3			!rigid de torsion	!février 2004

	elseif(isect.eq.1)then						!cercle				!février 2004
	dmil=d - epais													!février 2004
c	aire=pi*dmil*epais												!février 2004
	aire=(pi/4)*((d*d)-(d-2*epais)**2)								!20.02.04
	aiy=(pi/64)*((d**4)-(d-2*epais)**4)								!20.02.04
	aix=aiy															!février 2004
	sredy=aire/2													!février 2004
	sredx=aire/2													!février 2004
	tork=pi*dmil*dmil*dmil*epais/4									!février 2004
					
	elseif(isect.eq.2)then											!février 2004
	cm=d - epais													!février 2004
	c1=d - 2*epais													!février 2004
	aire=(d*d)-(c1*c1)												!20.02.04
	aiy=(d*d*d*d/12)-(c1*c1*c1*c1/12)								!février 2004
	aix=aiy															!février 2004
	sredy=2*epais*c1												!février 2004
	sredx=sredy														!février 2004
	tork=cm*cm*cm*epais												!février 2004
	endif															!février 2004

      z(3)=e*aire/a		!omegay										!février 2004
      z(4)=0				!hy par symétrie							!février 2004
      temp=gg*sredy/a													!février 2004
c      z(5)=temp*fam(1)	!sy											!février 2004
      z(5)=temp*1										!+++ 26.07.05
      z(6)=e*aiy/a		!ry											!février 2004
      z(7)=gg*tork/a	    !ty											!février 2004
      z(8)=0				!ly											!février 2004
	endif															!février 2004
      
c     raidisseurs
      z(9)=e*(h*o+p*q)/b
      z(10)=e*(h*o*(c/2.+h/2.)+p*q*(c/2.+h+q/2.))/b*kksr
      temp=sh*5.*p*q/(6.*b)
      z(11)=temp*fam(3)
      z(12)=e*(o*h**3/12.+o*h*(c/2.+h/2.)**2+p*q**3/12.+
     1            p*q*(c/2.+h+q/2.)**2)/b
      z(13)=sh*(h*o**3+p*q**3)/(3.*b)
      z(14)=-(c/2.+h+q/2.)*temp*fam(4)*kksr

c     traverses
      if(mt.eq.0) goto 2
      kkst=(-1)**kst
      do 1 i=1,mt
      j=6*(i-1)
      z(15+j)=e*(r(i)*s(i)+t(i)*u(i))
      z(16+j)=e*(r(i)*s(i)*(c/2.+r(i)/2.)+t(i)*u(i)*(c/2.+r(i)+u(i)/2.))
     *         *kkst
      temp=sh*5.*t(i)*u(i)/6.
      z(17+j)=temp*fam(5)
      z(18+j)=e*(s(i)*r(i)**3/12.+r(i)*s(i)*(c/2.+r(i)/2.)**2+
     *           t(i)*u(i)**3/12.+t(i)*u(i)*(c/2.+r(i)+u(i)/2.)**2)
      z(19+j)=sh*(r(i)*s(i)**3+t(i)*u(i)**3)/3.
      pot=pot+z(15+j)
    1 z(20+j)=-(c/2.+r(i)+u(i)/2.)*temp*fam(6)*kkst
   2  continue

c     impression de "const"
      if(impr2.ge.-1) then								!sept2006
 		if(itype.ne.5) then								!février 2004
        write(iu_11(iboat),15)                                                      
        write(iu_11(iboat),11)(z(i),i=1,2)
        write(iu_11(iboat),11)(z(i),i=3,14)
		else											!février 2004
        write(iu_11(iboat),16)							            !février 2004              
        write(iu_11(iboat),11)(z(i),i=3,8)							!février 2004
		endif											!février 2004
        if(mt.ge.1) then
          do 20 j=1,mt
            k=9+6*j
            write(iu_11(iboat),11)(z(i),i=k,k+5)
   20     continue
        endif
	endif

c     calcul des poids   (spec =poids spécifique)
      specc=spec*w
	if(itype.ne.5) then									!sept2006
      pbord=(c*rayon*phil*pi/180.d00)*specc
      paig= (z(3)*(rayon+kksa*d/2.)*phil*pi/180.d00)*specc/e
      praid=(z(9)*rayon*phil*pi/180.d00)*specc/e
      ptrav=pot*specc/e
	else												!05.12.05	sept2006
      pbord=0												!05.12.05	sept2006
      paig= (z(3)*(rayon+kksa*d/2.)*phil*pi/180.d00)*specc/e	!05.12.05	sept2006
      praid=0												!05.12.05	sept2006
      ptrav=0												!05.12.05	sept2006
	endif												!			sept2006
      	
	
	potel=pbord+paig+praid+ptrav
      po=potel/w                   ! poids du panneau par m selon x

      if(langue==1) then
        write(iu_11(iboat),12)pbord,paig,praid,ptrav,potel
	else
        write(iu_11(iboat),13)pbord,paig,praid,ptrav,potel
	endif

c    calcul de l'epaisseur moyenne compte tenu des trav. et des raid.

      delt=  pot/(phil*rayon*pi/180.d00)/e +z(9)/e +c

c    calcul de l'epaisseur moyenne compte tenu des trav., raid. et cadres

      delt2= delt + z(3)/e 

      return

   11 format(6(1x,e11.4))
   12 format(//' poids a sec (non déjaugé)'/t2,24(1h-)/
     *     ' 1- poids du bordage      = ',e11.4, 'n.'/
     *     ' 2- poids des cadres      = ',e11.4, 'n.'/
     *     ' 3- poids des raidisseurs = ',e11.4, 'n.'/
     *     ' 4- poids des traverses   = ',e11.4, 'n.'/
     *     ' poids total (1-4) (sans nervures second.) = ',e11.4,' n'/)
   13 format(//' dry weight (out of water)'/t2,24(1h-)/
     *     ' 1- weight of plating      = ',e11.4, 'n.'/
     *     ' 2- weight of frames       = ',e11.4, 'n.'/
     *     ' 3- weight of stiffeners   = ',e11.4, 'n.'/
     *     ' 4- weight of girders      = ',e11.4, 'n.'/
     * ' total weight (1-4) (without secondary members) = ',e11.4,' n'/)
   15 format(/' coefficients de raideur du panneau',/35(1h+)/
     * ' (ils correspondent successivement a )'/
     * t10,'d k'/t10,'omyr hyr syr ryr tyr lyr'/t10,
     * 'omxr hxr sxr rxr txr lxr '/t10,'omxtr*dx hxtr*dx ',
     * 'sxtr*dx rxtr*dx txtr*dx lxtr*dx'/t10,'( 60 valeurs en tr pour ',
     * 'maximum 10 traverses )',/)
   16 format(/' coefficients de raideur du panneau',/35(1h+)/					!février 2004
     * ' (ils correspondent successivement a )'/								!février 2004
     * t10,'omyr hyr syr ryr tyr lyr'/t10,'(il n''y a pas de raidisseurs		!février 2004
     * sur l''epontille)',/)													!février 2004

      end
