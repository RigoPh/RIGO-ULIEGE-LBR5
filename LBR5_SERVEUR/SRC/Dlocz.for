      subroutine dlocz(arg,darg,m,xi,phil,disc,dvarc,dtemp,dvarh,teta,q,
     *                 ijk,
     *                 lamb,width,cha,dcha,npts,nxi,nbrxi,
     *                 mt,bz,
     *                 si,co,ex)
      implicit double precision(a-h,o-z)
      double precision lamb
	integer*4 npts

      dimension arg(8),nxi(9),y(21),cha(100,3),
     *  disc(720),dvarc(33,9,16),dtemp(33,9,38),dvarh(33,9,38)
      dimension si(4),co(4),ex(4)
      dimension darg(8,9)
	dimension dcha(100,9),de(9),bz(1710),transp(33)   !!!juin06
	data transp/2,1,29,25,27,24,26,3,35,38,31,30,33,4,5,6,7,8,9,10,11
     *,12,13,14,15,16,17,18,19,20,21,22,23/                !!!juin06
c     (permet de passer le l'indice ii à l'indice i)       !!!juin06

c***********************************************************************
c
c    expression (partiel) des dérivées de dish (cfr subr locz)
c    -----------------------------------------------------------
c     dvarh(13,9,38) avec 13 variables (u v wø ny nyx my ry w wøø wøøø u
c                          9 variables de conception,
c                      et 38 termes (cfr dish).

c       dvarc = contient les dérivées de disc

c     modif: 19-3-96                                     créer : 18-3-96
c***********************************************************************
c     ordre des termes indép. (1 à 6)
c       1 : const       2 : y           3 : y**2         4 : y**3
c       5 : cos(téta+y) 6 : sin(téta+y)
c     ordre des termes exp(y) (7 à 38 = 4 x 8 termes)
c       1 : exp(-alpha q y)            * cos (béta q y)
c       2 : exp(-alpha q y)            * sin (béta q y)
c       3 : exp(-alpha q (yo-y))       * cos (béta q (yo-y))
c       4 : exp(-alpha q (yo-y))       * sin (béta q (yo-y))
c       5 : exp(-alpha q (2pi-y))      * cos (béta q (2pi-y))
c       6 : exp(-alpha q (2pi-y))      * sin (béta q (2pi-y))
c       7 : exp(-alpha q (2pi-(yo-y))) * cos (béta q (2pi-(yo-y)))
c       8 : exp(-alpha q (2pi-(yo-y))) * sin (béta q (2pi-(yo-y)))

      pi=2.d00*acos(0.d00)
      pi1=pi/18
      phil1=phil*pi1
      teta1=teta*pi1

      if(dabs(teta).ne.180.) goto 4
      cost=-1.
      sint=0.
      costt=-dcos(phil1)
      sintt=-dsin(phil1)
      goto 6
   4  if(dabs(teta).ne.90.) goto 5
      ii=1
      if(teta.eq.-90.) ii=-1
      cost=0.
      sint=ii
      costt=-dsin(phil1)*ii
      sintt=dcos(phil1)*ii
      goto 6
    5 cost=dcos(teta1)
      sint=dsin(teta1)
      costt=dcos(teta1+phil1)
      sintt=dsin(teta1+phil1)
    6 continue

      call ar(arg,darg,m,q,disc,dvarc,dtemp, 2,1,
     *        sint,cost,sintt,costt,nxi,nbrxi,si,co,ex)
      call ar(arg,darg,m,q,disc,dvarc,dtemp, 1,2,
     *        sint,cost,sintt,costt,nxi,nbrxi,si,co,ex)
      call ar(arg,darg,m,q,disc,dvarc,dtemp,29,3,
     *        sint,cost,sintt,costt,nxi,nbrxi,si,co,ex)
      call ar(arg,darg,m,q,disc,dvarc,dtemp,25,4,
     *        sint,cost,sintt,costt,nxi,nbrxi,si,co,ex)
      call ar(arg,darg,m,q,disc,dvarc,dtemp,27,5,
     *        sint,cost,sintt,costt,nxi,nbrxi,si,co,ex)
      call ar(arg,darg,m,q,disc,dvarc,dtemp,24,6,
     *        sint,cost,sintt,costt,nxi,nbrxi,si,co,ex)
      call ar(arg,darg,m,q,disc,dvarc,dtemp,26,7,
     *        sint,cost,sintt,costt,nxi,nbrxi,si,co,ex)
      call ar(arg,darg,m,q,disc,dvarc,dtemp, 3,8,
     *        sint,cost,sintt,costt,nxi,nbrxi,si,co,ex)
      call ar(arg,darg,m,q,disc,dvarc,dtemp,35,9,
     *        sint,cost,sintt,costt,nxi,nbrxi,si,co,ex)
      call ar(arg,darg,m,q,disc,dvarc,dtemp,38,10,
     *        sint,cost,sintt,costt,nxi,nbrxi,si,co,ex)
      call ar(arg,darg,m,q,disc,dvarc,dtemp,31,11,
     *        sint,cost,sintt,costt,nxi,nbrxi,si,co,ex)
      call ar(arg,darg,m,q,disc,dvarc,dtemp,30,12,
     *        sint,cost,sintt,costt,nxi,nbrxi,si,co,ex)
      call ar(arg,darg,m,q,disc,dvarc,dtemp,33,13,
     *        sint,cost,sintt,costt,nxi,nbrxi,si,co,ex)
      do 2 j=1,mt
      k1=2+j*2
      k2=12+j*2
      call ar(arg,darg,m,q,disc,dvarc,dtemp,k1,k2,
     *        sint,cost,sintt,costt,nxi,nbrxi,si,co,ex)
      k1=k1+1
      k2=k2+1
      call ar(arg,darg,m,q,disc,dvarc,dtemp,k1,k2,
     *        sint,cost,sintt,costt,nxi,nbrxi,si,co,ex)
    2 continue

      c=-2./(pi*ijk*10000.)
      e=0.
	call annuld(de,9)		!!!aout04
      y(1)=0.
      ipt=npts
      dw=width/float(ipt)
      do 155 i=1,ipt
 155  y(i+1)=y(i)+dw
      do 156 i=1,ipt
	e=e+cha(i,1)*(dcos(lamb*y(i+1))-dcos(lamb*y(i)))				!!!aout04
      do k=1,9														!!!aout04
      de(k)=de(k) + 
     *            dcha(i,k)*(dcos(lamb*y(i+1))-dcos(lamb*y(i)))	!!!aout04
 	enddo															!!!aout04
 156	continue
      e=e*c*xi
	do k=1,9                !!!aout04
	de(k)=de(k)*c*xi        !!!aout04
	enddo                   !!!aout04

      j1=6+8*m
      do 1 ii=1,13+2*mt
      do 1 kc=1,nbrxi
      k=nxi(kc)
      do 1 j=1,j1
	jj=j-1													!!!aout04
	i=(transp(ii)-1)*38 + 1 + jj							!!!aout04
	b=bz(i)													!!!aout04
      dvarh(ii,k,j)=dvarh(ii,k,j)+e*dtemp(ii,k,j)+de(k)*b		!!!aout04
   1  dtemp(ii,k,j)=0.
      return
      end

      subroutine ar(arg,darg,m,q,disc,dvarc,dtemp,i,ii,
     *              sint,cost,sintt,costt,nxi,nbrxi,si,co,ex)
      implicit double precision(a-h,o-z)
      dimension arg(8),disc(720),dvarc(33,9,16),dtemp(33,9,38),nxi(9)
      dimension si(4),co(4),ex(4)
      dimension darg(8,9)

c  i = class. des var.: u=2 v=1 wø=29 ny=25 nyx=27 my=24 ry=26 w=3 dans
c                       wøø=35  wøøø=38   uø=31 vø=30      vøø=33
c                       xo,zo de 4 à 23
c  ii= class. des var.: u=1 v=2 wø=3  ny=4  nyx=5  my=6  ry=7  w=8 dans
c                       wøø=9   wøøø=10   uø=11 vø=12      vøø=13
c                       xo,zo de 14 à 33
c  k = indice des 9 variables de conception  ; k=1,nbrxi
c  j = indice sur alpha(i) et béta(i) ; j=1,m

      pi=2.d00*acos(0.d00)
      piq2=2.*pi*q

c  boucle sur les variables (1 à nbrxi)
      do 4 kc=1,nbrxi
        k=nxi(kc)

      do 5 j=1,m
      l=4*(j-1)
      jk=(i-1)*16+l
      ij=8*j-2

      j2=j*2
      j1=j2-1
      al=arg(j1)
      be=arg(j2)
      dal=darg(j1,k)
      dbe=darg(j2,k)
      dal2=piq2*dal
      dbe2=piq2*dbe

      u1= al*al+be*be
      u2= u1*u1
      du1=2.*( al*dal+be*dbe)
      dr1=2.*(-al*dal+be*dbe)
      s1=2.*al*be
      ds1=2.*( al*dbe+be*dal)

      up=u1+1./(q*q)
      um=u1-1./(q*q)
      dup=2.*(up*dal+al*du1)
      dum=2.*(um*dbe+be*du1)
      bp=be+1./q
      bm=be-1./q

      usp=up-2.*be*be
      z1=al*al+bp*bp
      z2=al*al+bm*bm
      z11=z1*z1
      z22=z2*z2
      z3=z1*z2
      z4=z3*z3
      dz1=2.*(al*dal+bp*dbe)
      dz2=2.*(al*dal+bm*dbe)

      bpp=bp/(2.*z1)
      bmm=bm/(2.*z2)
      ap =al/(2.*z1)
      am =al/(2.*z2)
      dbmm=(-dz2*bm + dbe*z2)/(2.*z22)
      dbpp=(-dz1*bp + dbe*z1)/(2.*z11)
      dap =(-dz1*al + dal*z1)/(2.*z11)
      dam =(-dz2*al + dal*z2)/(2.*z22)

      cos1=ex(j)*co(j)
      sin1=ex(j)*si(j)

      aa=disc(jk+1)
      bb=disc(jk+2)
      daa=dvarc(ii,k,l+1)
      dbb=dvarc(ii,k,l+2)
      t3=-2.*(usp*aa+s1 *bb)
      t4= 2.*(s1 *aa-usp*bb)
      dt3=-2.*(-aa*dr1+bb *ds1+usp*daa+s1*dbb)
      dt4= 2.*( s1*daa-usp*dbb+ds1* aa+dr1*bb)

      temp3= 2.*(be*um*sin1-al*up*cos1)
      temp4=-2.*(al*up*sin1+be*um*cos1)
      dnom=2.*up*du1-8.*be*dbe/(q*q)

      if(ii.ge.14) goto 1
      goto(1,2,2,1,2,1,2,1,1,2,2,1,2),ii

c     termes u, ny, my ,w + wøø, vø et xo zo  (c.à.d. 1,4,6,8+9,12 et 14
c     ------------------------------------------------------------------
   1  temp= 2.*(daa*al*up+dbb*be*um) + aa*dup+bb*dum
     *  + daa*temp3 + dbb* temp4
     *  + piq2*( -dal*(aa*temp3+bb*temp4)+dbe*(-aa*temp4+bb*temp3) )
     *  + dup*(aa*sin1-bb*cos1) - dum*(aa*cos1+bb*sin1)
      temp2 =  (aa*(temp3+2.*al*up)+bb*(temp4+2.*be*um)) * dnom
      dtemp(ii,k,6)=dtemp(ii,k,6) + (z3*temp-temp2)/ z4

      dtemp(ii,k,ij+1)=cost*( aa*(-dbmm+dbpp)+ bb*(dam-dap)
     *                      +daa*(- bmm+ bpp)+dbb*( am- ap))
     *              +  sint*( aa*(-dam-dap)  + bb*(-dbmm-dbpp)
     *                      +daa*(- am- ap)  +dbb*(- bmm- bpp) )
      dtemp(ii,k,ij+2)=cost*( aa*(-dam+dap)+ bb*(-dbmm+dbpp)
     *                      +daa*(- am+ ap)+dbb*( -bmm+ bpp))
     *  +  sint* (  aa*(dbmm+dbpp)+ bb*(-dam-dap)
     *               +daa*( bmm+ bpp)+dbb*(- am- ap) )

      dtemp(ii,k,ij+3)=costt*( aa*(dbmm-dbpp)+ bb*(-dam+dap)
     *                       +daa*( bmm- bpp)+dbb*(- am+ ap))
     *  +  sintt* (  aa*(-dam-dap)+ bb*(-dbmm-dbpp)
     *                +daa*(- am- ap)+dbb*(- bmm- bpp) )
      dtemp(ii,k,ij+4)=costt*( aa*(dam-dap)+ bb*(dbmm-dbpp)
     *                       +daa*( am- ap)+dbb*( bmm- bpp))
     *  +  sintt* (  aa*(dbmm+dbpp)+ bb*(-dam-dap)
     *                +daa*( bmm+ bpp)+dbb*(- am- ap) )

      dtemp(ii,k,ij+5)=-cost*( aa*(dbmm-dbpp)+ bb*(-dam+dap)
     *                      +daa*( bmm- bpp)+dbb*(- am+ ap))
     *  -  sint* (  aa*(-dam-dap)+ bb*(-dbmm-dbpp)
     *               +daa*(- am- ap)+dbb*(- bmm- bpp) )
      dtemp(ii,k,ij+6)=-cost*( aa*(dam-dap)+ bb*(dbmm-dbpp)
     *                      +daa*( am- ap)+dbb*( bmm- bpp))
     *  -  sint* (  aa*(dbmm+dbpp)+ bb*(-dam-dap)
     *               +daa*( bmm+ bpp)+dbb*(- am- ap) )

      dtemp(ii,k,ij+7)=-costt*( aa*(-dbmm+dbpp)+ bb*(dam-dap)
     *                      +daa*(- bmm+ bpp)+dbb*( am- ap))
     *  -  sintt* (  aa*(-dam-dap)+ bb*(-dbmm-dbpp)
     *               +daa*(- am- ap)+dbb*(- bmm- bpp) )
      dtemp(ii,k,ij+8)=-costt*( aa*(-dam+dap)+ bb*(-dbmm+dbpp)
     *                       +daa*(- am+ ap)+dbb*( -bmm+ bpp))
     *  -  sintt* (  aa*(dbmm+dbpp)+ bb*(-dam-dap)
     *                +daa*( bmm+ bpp)+dbb*(- am- ap) )

      goto 3

c     termes v, wø, nyx, ry + wøøø, uø et vøø (c.à.d. 2,3,5,7 +10,11,13)
c     ------------------------------------------------------------------
   2  temp=t3*cos1+t4*sin1
      dtemp(ii,k,5)=dtemp(ii,k,5) + (
     * z3*(dt3 + dal2*temp - cos1*(dt3+dbe2*t4) - sin1*(dt4-dbe2*t3) )
     *  - (t3-temp)*dnom      )/(z4*q)

      dtemp(ii,k,ij+1)=cost*( aa*(-dbmm+dbpp)+ bb*(dam-dap)
     *                      +daa*(- bmm+ bpp)+dbb*( am- ap))
     *  +  sint* (  aa*(-dam-dap)+ bb*(-dbmm-dbpp)
     *               +daa*(- am- ap)+dbb*(- bmm- bpp) )
      dtemp(ii,k,ij+2)=cost*( aa*(-dam+dap)+ bb*(-dbmm+dbpp)
     *                      +daa*(- am+ ap)+dbb*( -bmm+ bpp))
     *  +  sint* (  aa*(dbmm+dbpp)+ bb*(-dam-dap)
     *               +daa*( bmm+ bpp)+dbb*(- am- ap) )

      dtemp(ii,k,ij+3)=-costt*( aa*(dbmm-dbpp)+ bb*(-dam+dap)
     *                       +daa*( bmm- bpp)+dbb*(- am+ ap))
     *  -  sintt* (  aa*(-dam-dap)+ bb*(-dbmm-dbpp)
     *                +daa*(- am- ap)+dbb*(- bmm- bpp) )
      dtemp(ii,k,ij+4)=-costt*( aa*(dam-dap)+ bb*(dbmm-dbpp)
     *                       +daa*( am- ap)+dbb*( bmm- bpp))
     *  -  sintt* (  aa*(dbmm+dbpp)+ bb*(-dam-dap)
     *                +daa*( bmm+ bpp)+dbb*(- am- ap) )

      dtemp(ii,k,ij+5)=cost*( aa*(dbmm-dbpp)+ bb*(-dam+dap)
     *                      +daa*( bmm- bpp)+dbb*(- am+ ap))
     *  +  sint* (  aa*(-dam-dap)+ bb*(-dbmm-dbpp)
     *               +daa*(- am- ap)+dbb*(- bmm- bpp) )
      dtemp(ii,k,ij+6)=cost*( aa*(dam-dap)+ bb*(dbmm-dbpp)
     *                      +daa*( am- ap)+dbb*( bmm- bpp))
     *  +  sint* (  aa*(dbmm+dbpp)+ bb*(-dam-dap)
     *               +daa*( bmm+ bpp)+dbb*(- am- ap) )

      dtemp(ii,k,ij+7)=-costt*( aa*(-dbmm+dbpp)+ bb*(dam-dap)
     *                      +daa*(- bmm+ bpp)+dbb*( am- ap))
     *  -  sintt* (  aa*(-dam-dap)+ bb*(-dbmm-dbpp)
     *               +daa*(- am- ap)+dbb*(- bmm- bpp) )
      dtemp(ii,k,ij+8)=-costt*( aa*(-dam+dap)+ bb*(-dbmm+dbpp)
     *                       +daa*(- am+ ap)+dbb*( -bmm+ bpp))
     *  -  sintt* (  aa*(dbmm+dbpp)+ bb*(-dam-dap)
     *                +daa*( bmm+ bpp)+dbb*(- am- ap) )

    3 continue
    5 continue
    4 continue
      return
      end
