      subroutine dlocy(arg,darg,m,xi,phil,disa,dvara,dtemp,dvarh,teta,
     *                 q,ijk,
     *                 lamb,width,impr,cha,dcha,npts,nxi,nbrxi,
     *                 mt,is,nsol,by,
     *                 si,co,ex)
      implicit real *8(a-h,o-z)
      real*8 lamb
      dimension arg(8),nxi(9),y(21),cha(100,3,10),npts(10),
     *          disa(720),dvara(33,9,16),dtemp(33,9,38),dvarh(33,9,38)
      dimension si(4),co(4),ex(4)
      dimension darg(8,9)
	dimension dcha(100,10,9),de(9),by(1710),transp(33)  !!!aout06
	data transp/2,1,29,25,27,24,26,3,35,38,31,30,33,4,5,6,7,8,9,10,11
     *,12,13,14,15,16,17,18,19,20,21,22,23/                !!!juin06
c     (permet de passer le l'indice ii � l'indice i)       !!!aout04

c***********************************************************************
c
c    expression (partiel) des d�riv�es de dish (cfr subr locy)
c    -----------------------------------------------------------
c     dvarh(13,9,38) avec 13 variables (u v w� ny nyx my ry w w�� w��� u
c                          9 variables de conception,
c                      et 38 termes (cfr dish).

c       dvara = contient les d�riv�es de disa

c     modif: 19-3-96                                     cr�er : 18-3-96
c***********************************************************************
c     ordre des termes ind�p. (1 � 6)
c       1 : const       2 : y           3 : y**2         4 : y**3
c       5 : cos(t�ta+y) 6 : sin(t�ta+y)
c     ordre des termes exp(y) (7 � 38 = 4 x 8 termes)
c       1 : exp(-alpha q y)            * cos (b�ta q y)
c       2 : exp(-alpha q y)            * sin (b�ta q y)
c       3 : exp(-alpha q (yo-y))       * cos (b�ta q (yo-y))
c       4 : exp(-alpha q (yo-y))       * sin (b�ta q (yo-y))
c       5 : exp(-alpha q (2pi-y))      * cos (b�ta q (2pi-y))
c       6 : exp(-alpha q (2pi-y))      * sin (b�ta q (2pi-y))
c       7 : exp(-alpha q (2pi-(yo-y))) * cos (b�ta q (2pi-(yo-y)))
c       8 : exp(-alpha q (2pi-(yo-y))) * sin (b�ta q (2pi-(yo-y)))

      pi=2.d00*acos(0.)
      pi1=pi/180
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

      call br(arg,darg,m,phil,q,disa,dvara,dtemp, 2,1,
     *        sint,cost,sintt,costt,nxi,nbrxi,si,co,ex)
      call br(arg,darg,m,phil,q,disa,dvara,dtemp, 1,2,
     *        sint,cost,sintt,costt,nxi,nbrxi,si,co,ex)
      call br(arg,darg,m,phil,q,disa,dvara,dtemp,29,3,
     *        sint,cost,sintt,costt,nxi,nbrxi,si,co,ex)
      call br(arg,darg,m,phil,q,disa,dvara,dtemp,25,4,
     *        sint,cost,sintt,costt,nxi,nbrxi,si,co,ex)
      call br(arg,darg,m,phil,q,disa,dvara,dtemp,27,5,
     *        sint,cost,sintt,costt,nxi,nbrxi,si,co,ex)
      call br(arg,darg,m,phil,q,disa,dvara,dtemp,24,6,
     *        sint,cost,sintt,costt,nxi,nbrxi,si,co,ex)
      call br(arg,darg,m,phil,q,disa,dvara,dtemp,26,7,
     *        sint,cost,sintt,costt,nxi,nbrxi,si,co,ex)
      call br(arg,darg,m,phil,q,disa,dvara,dtemp, 3,8,
     *        sint,cost,sintt,costt,nxi,nbrxi,si,co,ex)
      call br(arg,darg,m,phil,q,disa,dvara,dtemp,35,9,
     *        sint,cost,sintt,costt,nxi,nbrxi,si,co,ex)
      call br(arg,darg,m,phil,q,disa,dvara,dtemp,38,10,
     *        sint,cost,sintt,costt,nxi,nbrxi,si,co,ex)
      call br(arg,darg,m,phil,q,disa,dvara,dtemp,31,11,
     *        sint,cost,sintt,costt,nxi,nbrxi,si,co,ex)
      call br(arg,darg,m,phil,q,disa,dvara,dtemp,30,12,
     *        sint,cost,sintt,costt,nxi,nbrxi,si,co,ex)
      call br(arg,darg,m,phil,q,disa,dvara,dtemp,33,13,
     *        sint,cost,sintt,costt,nxi,nbrxi,si,co,ex)
      do 2 j=1,mt
      k1=2+j*2
      k2=12+j*2
      call br(arg,darg,m,phil,q,disa,dvara,dtemp,k1,k2,
     *        sint,cost,sintt,costt,nxi,nbrxi,si,co,ex)
      k1=k1+1
      k2=k2+1
      call br(arg,darg,m,phil,q,disa,dvara,dtemp,k1,k2,
     *        sint,cost,sintt,costt,nxi,nbrxi,si,co,ex)
    2 continue

      c=-2./(pi*ijk*10000.)
      e=0.
	call annuld(de,9)			!!!aout04
      y(1)=0.
      ipt=npts(is)
      dw=width/float(ipt)
      do 155 i=1,ipt
 155  y(i+1)=y(i)+dw
      do 156 i=1,ipt
	e=e+cha(i,1,is)*(dcos(lamb*y(i+1))-dcos(lamb*y(i)))            !!!aout04
      do k=1,9											           !!!aout04
      de(k)=de(k) + 
     *            dcha(i,is,k)*(dcos(lamb*y(i+1))-dcos(lamb*y(i)))   !!!aout04
 	enddo													       !!!aout04
 156  continue													   !!!aout04
      e=e*c*xi
	do k=1,9          !!!aout04
	de(k)=de(k)*c*xi  !!!aout04
	enddo             !!!aout04

      j1=6+8*m
      do 1 ii=1,13+2*mt
      do 1 kc=1,nbrxi
      k=nxi(kc)
      do 1 j=1,j1
	jj=j-1													!!!aout04
	i=(transp(ii)-1)*38 + 1 + jj							!!!aout04
	b=by(i)													!!!aout04
      dvarh(ii,k,j)=dvarh(ii,k,j)+e*dtemp(ii,k,j)+de(k)*b		!!!aout04
   1  dtemp(ii,k,j)=0.
      return
      end

      subroutine br(arg,darg,m,phil,q,disa,dvara,dtemp,i,ii,
     *              sint,cost,sintt,costt,nxi,nbrxi,si,co,ex)
c     ==============
      implicit real *8(a-h,o-z)
      dimension arg(8),disa(720),dvara(33,9,16),dtemp(33,9,38),nxi(9)
      dimension si(4),co(4),ex(4)
      dimension darg(8,9)

c  i = class. des var.: u=2 v=1 w�=29 ny=25 nyx=27 my=24 ry=26 w=3 dans
c                       w��=35  w���=38   u�=31 v�=30      v��=33
c                       xo,zo de 4 � 23
c  ii= class. des var.: u=1 v=2 w�=3  ny=4  nyx=5  my=6  ry=7  w=8 dans
c                       w��=9   w���=10   u�=11 v�=12      v��=13
c                       xo,zo de 14 � 33
c  k = indice des 9 variables de conception  ; k=1,nbrxi
c  j = indice sur alpha(i) et b�ta(i) ; j=1,m

      pi=2.d00*acos(0.)
      piq2=2.*pi*q

c  boucle sur les variables (1 � nbrxi)
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

c      write(66,*)'al,be,dal,dbe'
c      write(66,'(6e14.7)')al,be,dal,dbe

      u1= al*al+be*be
      u2= u1*u1
      du1=2.*( al*dal+be*dbe)
      dr1=2.*(-al*dal+be*dbe)
      s1=2.*al*be
      ds1=2.*( al*dbe+be*dal)

c      write(66,*)'u1,du1,s1,ds1,dr1'
c      write(66,'(6e14.7)')u1,du1,s1,ds1,dr1

      up=u1+1./(q*q)
      um=u1-1./(q*q)
      dup=2.*(up*dal+al*du1)
      dum=2.*(um*dbe+be*du1)
      bp=be+1./q
      bm=be-1./q

c      write(66,*)'up,um,dup,dum,bp,bm'
c      write(66,'(6e12.5)')up,um,dup,dum,bp,bm

      usp=up-2.*be*be
      z1=al*al+bp*bp
      z2=al*al+bm*bm
      z11=z1*z1
      z22=z2*z2
      z3=z1*z2
      z4=z3*z3
      dz1=2.*(al*dal+bp*dbe)
      dz2=2.*(al*dal+bm*dbe)

c      write(66,*)'usp,z1,z2,dz1,dz2'
c      write(66,'(6e14.7)')usp,z1,z2,dz1,dz2

      bpp=bp/(2.*z1)
      bmm=bm/(2.*z2)
      ap =al/(2.*z1)
      am =al/(2.*z2)
      dbmm=(-dz2*bm + dbe*z2)/(2.*z22)
      dbpp=(-dz1*bp + dbe*z1)/(2.*z11)
      dap =(-dz1*al + dal*z1)/(2.*z11)
      dam =(-dz2*al + dal*z2)/(2.*z22)

c      write(66,*)'bpp,bmm,ap,am'
c      write(66,'(6e14.7)')bpp,bmm,ap,am
c      write(66,*)'dbpp,dbmm,dap,dam'
c      write(66,'(6e14.7)')dbpp,dbmm,dap,dam

      cos1=ex(j)*co(j)
      sin1=ex(j)*si(j)

c      write(66,*)' cos1    sin1=',cos1,sin1
c      write(66,*)' cost    sint=',cost,sint
c      write(66,*)' costt  sintt=',costt,sintt

      aa=disa(jk+1)
      bb=disa(jk+2)
      daa=dvara(ii,k,l+1)
      dbb=dvara(ii,k,l+2)
      t3=-2.*(usp*aa+s1 *bb)
      t4= 2.*(s1 *aa-usp*bb)
      dt3=-2.*(-aa*dr1+bb *ds1+usp*daa+s1*dbb)
      dt4= 2.*( s1*daa-usp*dbb+ds1* aa+dr1*bb)

      temp3= 2.*(be*um*sin1-al*up*cos1)
      temp4=-2.*(al*up*sin1+be*um*cos1)
      dnom=2.*up*du1-8.*be*dbe/(q*q)

c      write(66,*)' aa,bb,daa,dbb'
c      write(66,'(6e14.7)')aa,bb,daa,dbb
c      write(66,*)'t3,t4'
c      write(66,'(6e14.7)')t3,t4
c      write(66,*)'dt3,dt4'
c      write(66,'(6e14.7)')dt3,dt4

      if(ii.ge.14) goto 1
      goto(1,2,2,1,2,1,2,1,1,2,2,1,2),ii

c     termes u, ny, my ,w + w��,v� + xo,zo  (c.�.d. 1,4,6,8 + 9,12 + 14,
c     ------------------------------------------------------------------
   1  temp=t3*cos1+t4*sin1
      dtemp(ii,k,6)=dtemp(ii,k,6) - (
     * z3*(dt3 + dal2*temp - cos1*(dt3+dbe2*t4) - sin1*(dt4-dbe2*t3) )
     *  - (t3-temp)*dnom      )/(z4*q)
c      write(66,*)'temp=',temp

      dtemp(ii,k,ij+1)=-sint*( aa*(-dbmm+dbpp)+ bb*(dam-dap)
     *                      +daa*(- bmm+ bpp)+dbb*( am- ap))
     *  +  cost* (  aa*(-dam-dap)+ bb*(-dbmm-dbpp)
     *               +daa*(- am- ap)+dbb*(- bmm- bpp) )
      dtemp(ii,k,ij+2)=-sint*( aa*(-dam+dap)+ bb*(-dbmm+dbpp)
     *                      +daa*(- am+ ap)+dbb*( -bmm+ bpp))
     *  +  cost* (  aa*(dbmm+dbpp)+ bb*(-dam-dap)
     *               +daa*( bmm+ bpp)+dbb*(- am- ap) )

      dtemp(ii,k,ij+3)=+sintt*( aa*(dbmm-dbpp)+ bb*(-dam+dap)
     *                       +daa*( bmm- bpp)+dbb*(- am+ ap))
     *  -  costt* (  aa*(-dam-dap)+ bb*(-dbmm-dbpp)
     *                +daa*(- am- ap)+dbb*(- bmm- bpp) )
      dtemp(ii,k,ij+4)=+sintt*( aa*(dam-dap)+ bb*(dbmm-dbpp)
     *                       +daa*( am- ap)+dbb*( bmm- bpp))
     *  -  costt* (  aa*(dbmm+dbpp)+ bb*(-dam-dap)
     *                +daa*( bmm+ bpp)+dbb*(- am- ap) )

      dtemp(ii,k,ij+5)=-sint*( aa*(dbmm-dbpp)+ bb*(-dam+dap)
     *                      +daa*( bmm- bpp)+dbb*(- am+ ap))
     *  +  cost* (  aa*(-dam-dap)+ bb*(-dbmm-dbpp)
     *               +daa*(- am- ap)+dbb*(- bmm- bpp) )
      dtemp(ii,k,ij+6)=-sint*( aa*(dam-dap)+ bb*(dbmm-dbpp)
     *                      +daa*( am- ap)+dbb*( bmm- bpp))
     *  +  cost* (  aa*(dbmm+dbpp)+ bb*(-dam-dap)
     *               +daa*( bmm+ bpp)+dbb*(- am- ap) )

      dtemp(ii,k,ij+7)=+sintt*( aa*(-dbmm+dbpp)+ bb*(dam-dap)
     *                      +daa*(- bmm+ bpp)+dbb*( am- ap))
     *  -  costt* (  aa*(-dam-dap)+ bb*(-dbmm-dbpp)
     *               +daa*(- am- ap)+dbb*(- bmm- bpp) )
      dtemp(ii,k,ij+8)=+sintt*( aa*(-dam+dap)+ bb*(-dbmm+dbpp)
     *                       +daa*(- am+ ap)+dbb*( -bmm+ bpp))
     *  -  costt* (  aa*(dbmm+dbpp)+ bb*(-dam-dap)
     *                +daa*( bmm+ bpp)+dbb*(- am- ap) )
      goto 3

c     termes v, w�, nyx, ry + w���, u� et v�� (c.�.d. 2,3,5,7 +10,11,13)
c     ------------------------------------------------------------------

   2  temp= 2.*(daa*al*up+dbb*be*um) + aa*dup+bb*dum
     *  + daa*temp3 + dbb* temp4
     *  + piq2*( -dal*(aa*temp3+bb*temp4)+dbe*(-aa*temp4+bb*temp3) )
     *  + dup*(aa*sin1-bb*cos1) - dum*(aa*cos1+bb*sin1)
      temp2 =  (aa*(temp3+2.*al*up)+bb*(temp4+2.*be*um)) * dnom
      dtemp(ii,k,5)=dtemp(ii,k,5) + (z3*temp-temp2)/ z4

c      write(66,*)'z3 et dz3=dnom'
c      write(66,'(6e14.7)')z3,dnom
c      fctie=(aa*(temp3+2.*al*up)+bb*(temp4+2.*be*um))
c      write(66,*)'fct et dfct'
c      write(66,'(6e14.7)')fctie,temp

c      write(66,*)' z4=',z4
c      write(66,*)' q=',q
c      write(66,*)' dtemp(ii,k,5)=',dtemp(ii,k,5)
c      write(66,*)' '

      dtemp(ii,k,ij+1)=-sint*( aa*(-dbmm+dbpp)+ bb*(dam-dap)
     *                      +daa*(- bmm+ bpp)+dbb*( am- ap))
     *              +  cost*( aa*(-dam-dap)  + bb*(-dbmm-dbpp)
     *                      +daa*(- am- ap)  +dbb*(- bmm- bpp) )
      dtemp(ii,k,ij+2)=-sint*( aa*(-dam+dap)+ bb*(-dbmm+dbpp)
     *                      +daa*(- am+ ap)+dbb*( -bmm+ bpp))
     *  +  cost* (  aa*(dbmm+dbpp)+ bb*(-dam-dap)
     *               +daa*( bmm+ bpp)+dbb*(- am- ap) )

      dtemp(ii,k,ij+3)=-sintt*( aa*(dbmm-dbpp)+ bb*(-dam+dap)
     *                       +daa*( bmm- bpp)+dbb*(- am+ ap))
     *  +  costt* (  aa*(-dam-dap)+ bb*(-dbmm-dbpp)
     *                +daa*(- am- ap)+dbb*(- bmm- bpp) )
      dtemp(ii,k,ij+4)=-sintt*( aa*(dam-dap)+ bb*(dbmm-dbpp)
     *                       +daa*( am- ap)+dbb*( bmm- bpp))
     *  +  costt* (  aa*(dbmm+dbpp)+ bb*(-dam-dap)
     *                +daa*( bmm+ bpp)+dbb*(- am- ap) )

      dtemp(ii,k,ij+5)=+sint*( aa*(dbmm-dbpp)+ bb*(-dam+dap)
     *                      +daa*( bmm- bpp)+dbb*(- am+ ap))
     *  -  cost* (  aa*(-dam-dap)+ bb*(-dbmm-dbpp)
     *               +daa*(- am- ap)+dbb*(- bmm- bpp) )
      dtemp(ii,k,ij+6)=+sint*( aa*(dam-dap)+ bb*(dbmm-dbpp)
     *                      +daa*( am- ap)+dbb*( bmm- bpp))
     *  -  cost* (  aa*(dbmm+dbpp)+ bb*(-dam-dap)
     *               +daa*( bmm+ bpp)+dbb*(- am- ap) )

      dtemp(ii,k,ij+7)=+sintt*( aa*(-dbmm+dbpp)+ bb*(dam-dap)
     *                      +daa*(- bmm+ bpp)+dbb*( am- ap))
     *  -  costt* (  aa*(-dam-dap)+ bb*(-dbmm-dbpp)
     *               +daa*(- am- ap)+dbb*(- bmm- bpp) )
      dtemp(ii,k,ij+8)=+sintt*( aa*(-dam+dap)+ bb*(-dbmm+dbpp)
     *                       +daa*(- am+ ap)+dbb*( -bmm+ bpp))
     *  -  costt* (  aa*(dbmm+dbpp)+ bb*(-dam-dap)
     *                +daa*( bmm+ bpp)+dbb*(- am- ap) )


    3 continue
    5 continue

    4 continue
      return
      end
