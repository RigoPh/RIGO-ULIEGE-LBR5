      subroutine dhydro(arg,darg,m,xi,xf,phil,q,disc,dvarc,dvarh,
     *                  bkse,ijk,teta,nxi,nbrxi,mt,si,co,ex)
      implicit real *8(a-h,o-z)
      dimension arg(8),disc(720),dvarc(33,9,16),dvarh(33,9,38),nxi(9)
      dimension si(4),co(4),ex(4)
      dimension darg(8,9)

c***********************************************************************
c
c    expression (partiel) des dérivées de dish (cfr subr hydro)
c    -----------------------------------------------------------
c     dvarh(13,9,38) avec 13 variables (u v wø ny nyx my ry w wøø wøøø u
c                          9 variables de conception,
c                      et 38 termes (cfr dish).

c       dvarc = contient les dérivées de disc

c     modif: 14-6-95                                     créer : 7-8-94
c             1-4-96 (correction à l'indice k1 si traverses: k1=2=j*2)
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

      pi=2.d00*acos(0.)
      pi1=pi/180
      phil1=phil*pi1
      teta1=teta*pi1
      jj=1
      if(phil.le.1.e-05) jj=0
      kk=1
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
      if(jj.eq.0) kk=0
      goto 6
    5 cost=dcos(teta1)
      sint=dsin(teta1)
      costt=dcos(teta1+phil1)
      sintt=dsin(teta1+phil1)
    6 continue


      call rr(arg,darg,m,xi,xf,phil,q,disc,dvarc,dvarh, 2,1,
     *        jj,kk,sint,cost,sintt,costt,nxi,nbrxi,
     *        si,co,ex)
      call rr(arg,darg,m,xi,xf,phil,q,disc,dvarc,dvarh, 1,2,
     *        jj,kk,sint,cost,sintt,costt,nxi,nbrxi,
     *        si,co,ex)
      call rr(arg,darg,m,xi,xf,phil,q,disc,dvarc,dvarh,29,3,
     *        jj,kk,sint,cost,sintt,costt,nxi,nbrxi,
     *        si,co,ex)
      call rr(arg,darg,m,xi,xf,phil,q,disc,dvarc,dvarh,25,4,
     *        jj,kk,sint,cost,sintt,costt,nxi,nbrxi,
     *        si,co,ex)
      call rr(arg,darg,m,xi,xf,phil,q,disc,dvarc,dvarh,27,5,
     *        jj,kk,sint,cost,sintt,costt,nxi,nbrxi,
     *        si,co,ex)
      call rr(arg,darg,m,xi,xf,phil,q,disc,dvarc,dvarh,24,6,
     *        jj,kk,sint,cost,sintt,costt,nxi,nbrxi,
     *        si,co,ex)
      call rr(arg,darg,m,xi,xf,phil,q,disc,dvarc,dvarh,26,7,
     *        jj,kk,sint,cost,sintt,costt,nxi,nbrxi,
     *        si,co,ex)
      call rr(arg,darg,m,xi,xf,phil,q,disc,dvarc,dvarh, 3,8,
     *        jj,kk,sint,cost,sintt,costt,nxi,nbrxi,
     *        si,co,ex)
      call rr(arg,darg,m,xi,xf,phil,q,disc,dvarc,dvarh,35,9,
     *        jj,kk,sint,cost,sintt,costt,nxi,nbrxi,
     *        si,co,ex)
      call rr(arg,darg,m,xi,xf,phil,q,disc,dvarc,dvarh,38,10,
     *        jj,kk,sint,cost,sintt,costt,nxi,nbrxi,
     *        si,co,ex)
      call rr(arg,darg,m,xi,xf,phil,q,disc,dvarc,dvarh,31,11,
     *        jj,kk,sint,cost,sintt,costt,nxi,nbrxi,
     *        si,co,ex)
      call rr(arg,darg,m,xi,xf,phil,q,disc,dvarc,dvarh,30,12,
     *        jj,kk,sint,cost,sintt,costt,nxi,nbrxi,
     *        si,co,ex)
      call rr(arg,darg,m,xi,xf,phil,q,disc,dvarc,dvarh,33,13,
     *        jj,kk,sint,cost,sintt,costt,nxi,nbrxi,
     *        si,co,ex)
      do 2 j=1,mt
      k1=2+j*2
      k2=12+j*2
      call rr(arg,darg,m,xi,xf,phil,q,disc,dvarc,dvarh,k1,k2,
     *        jj,kk,sint,cost,sintt,costt,nxi,nbrxi,
     *        si,co,ex)
      k1=k1+1
      k2=k2+1
      call rr(arg,darg,m,xi,xf,phil,q,disc,dvarc,dvarh,k1,k2,
     *        jj,kk,sint,cost,sintt,costt,nxi,nbrxi,
     *        si,co,ex)
    2 continue

      e=bkse*4.*0.981/(pi*ijk)
      j1=6+8*m
      do 1 ii=1,13+2*mt
      do 1 kc=1,nbrxi
      k=nxi(kc)
      do 1 j=1,j1
   1  dvarh(ii,k,j)=e*dvarh(ii,k,j)

c      if(igh.eq.0) stop
      return
      end

      subroutine rr(arg,darg,m,xi,xf,phil,q,disc,dvarc,dvarh,i,ii,
     *              jj,kk,sint,cost,sintt,costt,nxi,nbrxi,
     *              si,co,ex)
      implicit real *8(a-h,o-z)
      dimension arg(8),disc(720),dvarc(33,9,16),dvarh(33,9,38),nxi(9)
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

      pi=2.d00*acos(0.)
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

      bpp=bp*q/(2.*z1)
      bmm=bm*q/(2.*z2)
      ap=al*q/(2.*z1)
      am=al*q/(2.*z2)
      dbmm=q*(-dz2*bm + dbe*z2)/(2.*z22)
      dbpp=q*(-dz1*bp + dbe*z1)/(2.*z11)
      dap =q*(-dz1*al + dal*z1)/(2.*z11)
      dam =q*(-dz2*al + dal*z2)/(2.*z22)

      cos1=ex(j)*co(j)
      sin1=ex(j)*si(j)

      aa=disc(jk+1)
      bb=disc(jk+2)
      daa=dvarc(ii,k,l+1)
      dbb=dvarc(ii,k,l+2)
      t1=( al*aa+be*bb)
      t2=(-be*aa+al*bb)
      t3=-2.*(usp*aa+s1 *bb)
      t4= 2.*(s1 *aa-usp*bb)
      dt1=( dal*aa+al*daa+dbe*bb+be*dbb)
      dt2=(-dbe*aa-be*daa+dal*bb+al*dbb)
      dt3=-2.*(-aa*dr1+bb *ds1+usp*daa+s1*dbb)
      dt4= 2.*( s1*daa-usp*dbb+ds1* aa+dr1*bb)

      temp3= 2.*(be*um*sin1-al*up*cos1)
      temp4=-2.*(al*up*sin1+be*um*cos1)
      dnom=2.*up*du1-8.*be*dbe/(q*q)

      c1=(u1*dt1-du1*t1)/u2
      c2=(u1*dt2-du1*t2)/u2

c      write(66,*)' aa,bb,daa,dbb'
c      write(66,'(6e14.7)')aa,bb,daa,dbb
c      write(66,*)'t1,t2,t3,t4'
c      write(66,'(6e14.7)')t1,t2,t3,t4
c      write(66,*)'dt1,dt2,dt3,dt4'
c      write(66,'(6e14.7)')dt1,dt2,dt3,dt4
c      write(66,*)' c1, c2 =', c1,c2

      if(ii.ge.14) goto 1
      goto(1,2,2,1,2,1,2,1,1,2,2,1,2),ii

c     termes u, ny, my ,w + wøø, vø et xo zo  (c.à.d. 1,4,6,8+9,12 et 14
c     ------------------------------------------------------------------
   1  temp=t1*cos1+t2*sin1
      temp2= 2.* (  du1*(-t1+temp)
     *   + u1*( dt1 + dal2*temp - cos1*(dt1+dbe2*t2)
     *                          - sin1*(dt2-dbe2*t1) )  )/u2
      dvarh(ii,k,1)=dvarh(ii,k,1) + temp2 * (xi-q*sint*kk)

c      write(66,*)' temp=',temp
c      write(66,*)' temp2=',temp2
c      write(66,*)' dvarh(ii,k,1)=',dvarh(ii,k,1)

      temp= 2.*(daa*al*up+dbb*be*um) + aa*dup+bb*dum
     *  + daa*temp3 + dbb* temp4
     *  + piq2*( -dal*(aa*temp3+bb*temp4)+dbe*(-aa*temp4+bb*temp3) )
     *  + dup*(aa*sin1-bb*cos1) - dum*(aa*cos1+bb*sin1)
      temp2 =  (aa*(temp3+2.*al*up)+bb*(temp4+2.*be*um)) * dnom
      dvarh(ii,k,6)=dvarh(ii,k,6) + q*kk*(z3*temp-temp2) / z4

c      write(66,*)'z3 et dz3=dnom'
c      write(66,'(6e14.7)')z3,dnom
c      fctie=(aa*(temp3+2.*al*up)+bb*(temp4+2.*be*um))
c      write(66,*)'fct et dfct'
c      write(66,'(6e14.7)')fctie,temp

c      write(66,*)' z4=',z4
c      write(66,*)' q=',q
c      write(66,*)' dvarh(ii,k,6)=',dvarh(ii,k,6)
c      write(66,*)' '

      dvarh(ii,k,ij+1)=cost*( aa*(-dbmm+dbpp)+ bb*(dam-dap)
     *                      +daa*(- bmm+ bpp)+dbb*( am- ap))
     *  + jj*(sint* (  aa*(-dam-dap)+ bb*(-dbmm-dbpp)
     *               +daa*(- am- ap)+dbb*(- bmm- bpp) ) + sint*q*c1 )
      dvarh(ii,k,ij+1)=dvarh(ii,k,ij+1)-xi*c1
      dvarh(ii,k,ij+2)=cost*( aa*(-dam+dap)+ bb*(-dbmm+dbpp)
     *                      +daa*(- am+ ap)+dbb*( -bmm+ bpp))
     *  + jj*(sint* (  aa*(dbmm+dbpp)+ bb*(-dam-dap)
     *               +daa*( bmm+ bpp)+dbb*(- am- ap) ) + sint*q*c2 )
      dvarh(ii,k,ij+2)=dvarh(ii,k,ij+2)-xi*c2

      dvarh(ii,k,ij+3)=costt*( aa*(dbmm-dbpp)+ bb*(-dam+dap)
     *                       +daa*( bmm- bpp)+dbb*(- am+ ap))
     *  + kk*(sintt* (  aa*(-dam-dap)+ bb*(-dbmm-dbpp)
     *                +daa*(- am- ap)+dbb*(- bmm- bpp) ) + sint*q*c1 )
      dvarh(ii,k,ij+3)=dvarh(ii,k,ij+3)-xi*c1
      dvarh(ii,k,ij+4)=costt*( aa*(dam-dap)+ bb*(dbmm-dbpp)
     *                       +daa*( am- ap)+dbb*( bmm- bpp))
     *  + kk*(sintt* (  aa*(dbmm+dbpp)+ bb*(-dam-dap)
     *                +daa*( bmm+ bpp)+dbb*(- am- ap) ) + sint*q*c2 )
      dvarh(ii,k,ij+4)=dvarh(ii,k,ij+4)-xi*c2

      dvarh(ii,k,ij+5)=cost*( aa*(dbmm-dbpp)+ bb*(-dam+dap)
     *                      +daa*( bmm- bpp)+dbb*(- am+ ap))
     *  + jj*(sint* (  aa*(-dam-dap)+ bb*(-dbmm-dbpp)
     *               +daa*(- am- ap)+dbb*(- bmm- bpp) ) + sint*q*c1 )
      dvarh(ii,k,ij+5)=-(dvarh(ii,k,ij+5)-xi*c1)
      dvarh(ii,k,ij+6)=cost*( aa*(dam-dap)+ bb*(dbmm-dbpp)
     *                      +daa*( am- ap)+dbb*( bmm- bpp))
     *  + jj*(sint* (  aa*(dbmm+dbpp)+ bb*(-dam-dap)
     *               +daa*( bmm+ bpp)+dbb*(- am- ap) ) + sint*q*c2 )
      dvarh(ii,k,ij+6)=-(dvarh(ii,k,ij+6)-xi*c2)

      dvarh(ii,k,ij+7)=costt*( aa*(-dbmm+dbpp)+ bb*(dam-dap)
     *                      +daa*(- bmm+ bpp)+dbb*( am- ap))
     *  + kk*(sintt* (  aa*(-dam-dap)+ bb*(-dbmm-dbpp)
     *               +daa*(- am- ap)+dbb*(- bmm- bpp) ) + sint*q*c1 )
      dvarh(ii,k,ij+7)=-(dvarh(ii,k,ij+7)-xi*c1)
      dvarh(ii,k,ij+8)=costt*( aa*(-dam+dap)+ bb*(-dbmm+dbpp)
     *                       +daa*(- am+ ap)+dbb*( -bmm+ bpp))
     *  + kk*(sintt* (  aa*(dbmm+dbpp)+ bb*(-dam-dap)
     *                +daa*( bmm+ bpp)+dbb*(- am- ap) ) + sint*q*c2 )
      dvarh(ii,k,ij+8)=-(dvarh(ii,k,ij+8)-xi*c2)

      goto 3

c     termes v, wø, nyx, ry + wøøø,uø etvøø (c.à.d. 2,3,5,7 +10,11,13)
c     ------------------------------------------
   2  temp=t3*cos1+t4*sin1
      dvarh(ii,k,5)=dvarh(ii,k,5) + (
     * z3*(dt3 + dal2*temp - cos1*(dt3+dbe2*t4) - sin1*(dt4-dbe2*t3) )
     *  - (t3-temp)*dnom      )/z4
c      write(66,*)'temp=',temp

      dvarh(ii,k,ij+1)=cost*( aa*(-dbmm+dbpp)+ bb*(dam-dap)
     *                      +daa*(- bmm+ bpp)+dbb*( am- ap))
     *  + jj*(sint* (  aa*(-dam-dap)+ bb*(-dbmm-dbpp)
     *               +daa*(- am- ap)+dbb*(- bmm- bpp) ) + sint*q*c1 )
      dvarh(ii,k,ij+1)=dvarh(ii,k,ij+1)-xi*c1
      dvarh(ii,k,ij+2)=cost*( aa*(-dam+dap)+ bb*(-dbmm+dbpp)
     *                      +daa*(- am+ ap)+dbb*( -bmm+ bpp))
     *  + jj*(sint* (  aa*(dbmm+dbpp)+ bb*(-dam-dap)
     *               +daa*( bmm+ bpp)+dbb*(- am- ap) ) + sint*q*c2 )
      dvarh(ii,k,ij+2)=dvarh(ii,k,ij+2)-xi*c2

      dvarh(ii,k,ij+3)=costt*( aa*(dbmm-dbpp)+ bb*(-dam+dap)
     *                       +daa*( bmm- bpp)+dbb*(- am+ ap))
     *  + kk*(sintt* (  aa*(-dam-dap)+ bb*(-dbmm-dbpp)
     *                +daa*(- am- ap)+dbb*(- bmm- bpp) ) + sint*q*c1 )
      dvarh(ii,k,ij+3)=-(dvarh(ii,k,ij+3)-xi*c1)
      dvarh(ii,k,ij+4)=costt*( aa*(dam-dap)+ bb*(dbmm-dbpp)
     *                       +daa*( am- ap)+dbb*( bmm- bpp))
     *  + kk*(sintt* (  aa*(dbmm+dbpp)+ bb*(-dam-dap)
     *                +daa*( bmm+ bpp)+dbb*(- am- ap) ) + sint*q*c2 )
      dvarh(ii,k,ij+4)=-(dvarh(ii,k,ij+4)-xi*c2)

      dvarh(ii,k,ij+5)=cost*( aa*(dbmm-dbpp)+ bb*(-dam+dap)
     *                      +daa*( bmm- bpp)+dbb*(- am+ ap))
     *  + jj*(sint* (  aa*(-dam-dap)+ bb*(-dbmm-dbpp)
     *               +daa*(- am- ap)+dbb*(- bmm- bpp) ) + sint*q*c1 )
      dvarh(ii,k,ij+5)= (dvarh(ii,k,ij+5)-xi*c1)
      dvarh(ii,k,ij+6)=cost*( aa*(dam-dap)+ bb*(dbmm-dbpp)
     *                      +daa*( am- ap)+dbb*( bmm- bpp))
     *  + jj*(sint* (  aa*(dbmm+dbpp)+ bb*(-dam-dap)
     *               +daa*( bmm+ bpp)+dbb*(- am- ap) ) + sint*q*c2 )
      dvarh(ii,k,ij+6)= (dvarh(ii,k,ij+6)-xi*c2)

      dvarh(ii,k,ij+7)=costt*( aa*(-dbmm+dbpp)+ bb*(dam-dap)
     *                      +daa*(- bmm+ bpp)+dbb*( am- ap))
     *  + kk*(sintt* (  aa*(-dam-dap)+ bb*(-dbmm-dbpp)
     *               +daa*(- am- ap)+dbb*(- bmm- bpp) ) + sint*q*c1 )
      dvarh(ii,k,ij+7)=-(dvarh(ii,k,ij+7)-xi*c1)
      dvarh(ii,k,ij+8)=costt*( aa*(-dam+dap)+ bb*(-dbmm+dbpp)
     *                       +daa*(- am+ ap)+dbb*( -bmm+ bpp))
     *  + kk*(sintt* (  aa*(dbmm+dbpp)+ bb*(-dam-dap)
     *                +daa*( bmm+ bpp)+dbb*(- am- ap) ) + sint*q*c2 )
      dvarh(ii,k,ij+8)=-(dvarh(ii,k,ij+8)-xi*c2)

    3 continue
    5 continue

    4 continue
      return
      end
