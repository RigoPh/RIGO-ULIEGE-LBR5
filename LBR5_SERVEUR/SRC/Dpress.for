      subroutine dpress(arg,darg,m,xi,xf,phil,q,disc,dvarc,dvarh,bkse,
     *                  ijk,
     *                  nxi,nbrxi,mt,si,co,ex)
      implicit double precision(a-h,o-z)
      dimension arg(8),disc(720),dvarc(33,9,16),dvarh(33,9,38),nxi(9)
      dimension si(4),co(4),ex(4)
      dimension darg(8,9)

c***********************************************************************
c
c    expression (partiel) des dérivées de dish (cfr subr press)
c    -----------------------------------------------------------
c    dvarh(13,9,38) avec 13 variables (u v wø ny nyx my ry w)wøø wøøø uø
c                         9 variables de conception,
c                     et 38 termes (cfr dish).
c
c    dvarc = contient les dérivées de disc
c
c     modif: 14-6-95                                     créer : 28-2-94
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
      call tt(arg,darg,m,xi,xf,phil,q,disc,dvarc,dvarh, 2,1,nxi,nbrxi,
     *        si,co,ex)
      call tt(arg,darg,m,xi,xf,phil,q,disc,dvarc,dvarh, 1,2,nxi,nbrxi,
     *        si,co,ex)
      call tt(arg,darg,m,xi,xf,phil,q,disc,dvarc,dvarh,29,3,nxi,nbrxi,
     *        si,co,ex)
      call tt(arg,darg,m,xi,xf,phil,q,disc,dvarc,dvarh,25,4,nxi,nbrxi,
     *        si,co,ex)
      call tt(arg,darg,m,xi,xf,phil,q,disc,dvarc,dvarh,27,5,nxi,nbrxi,
     *        si,co,ex)
      call tt(arg,darg,m,xi,xf,phil,q,disc,dvarc,dvarh,24,6,nxi,nbrxi,
     *        si,co,ex)
      call tt(arg,darg,m,xi,xf,phil,q,disc,dvarc,dvarh,26,7,nxi,nbrxi,
     *        si,co,ex)
      call tt(arg,darg,m,xi,xf,phil,q,disc,dvarc,dvarh, 3,8,nxi,nbrxi,
     *        si,co,ex)
      call tt(arg,darg,m,xi,xf,phil,q,disc,dvarc,dvarh,35,9,nxi,nbrxi,
     *        si,co,ex)
      call tt(arg,darg,m,xi,xf,phil,q,disc,dvarc,dvarh,38,10,nxi,nbrxi,
     *        si,co,ex)
      call tt(arg,darg,m,xi,xf,phil,q,disc,dvarc,dvarh,31,11,nxi,nbrxi,
     *        si,co,ex)
      call tt(arg,darg,m,xi,xf,phil,q,disc,dvarc,dvarh,30,12,nxi,nbrxi,
     *        si,co,ex)
      call tt(arg,darg,m,xi,xf,phil,q,disc,dvarc,dvarh,33,13,nxi,nbrxi,
     *        si,co,ex)

      do 2 j=1,mt
      k1=2+j*2
      k2=12+j*2
      call tt(arg,darg,m,xi,xf,phil,q,disc,dvarc,dvarh,k1,k2,nxi,nbrxi,
     *        si,co,ex)
      k1=k1+1
      k2=k2+1
      call tt(arg,darg,m,xi,xf,phil,q,disc,dvarc,dvarh,k1,k2,nxi,nbrxi,
     *        si,co,ex)
    2 continue

      e=bkse*4.*0.981/(pi*ijk)
	
      j1=6+8*m
      do 1 ii=1,13+2*mt
      do 1 kk=1,nbrxi
      k=nxi(kk)
      do 1 j=1,j1
   1  dvarh(ii,k,j)=e*dvarh(ii,k,j)

      return

      end

c    *********************************************************************
c    *********************************************************************

      subroutine tt(arg,darg,m,xi,xf,phil,q,disc,dvarc,dvarh,i,ii,
     *              nxi,nbrxi,si,co,ex)
      implicit double precision(a-h,o-z)
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

      pi=2.d00*acos(0.d00)
      piq2=2.*pi*q
      do 4 kk=1,nbrxi
         k=nxi(kk)
      do 5 j=1,m
      l=4*(j-1)
      jk=(i-1)*16+l
      jj=8*j-2

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
      u3= u2*u1
      r1=-al*al+be*be
      s1=2.*al*be
      du1=2.*( al*dal+be*dbe)
      dr1=2.*(-al*dal+be*dbe)
      ds1=2.*( al*dbe+be*dal)

      p1=xf-xi
      p2=p1/(phil*pi/180.)
      p3=p2/q

      cos1=ex(j)*co(j)
      sin1=ex(j)*si(j)

      aa=disc(jk+1)
      bb=disc(jk+2)
      daa=dvarc(ii,k,l+1)
      dbb=dvarc(ii,k,l+2)
      t1=( al*aa+be*bb)
      t2=(-be*aa+al*bb)
      t3=( r1*aa-s1*bb)
      t4=( s1*aa+r1*bb)
      dt1=( dal*aa+al*daa+dbe*bb+be*dbb)
      dt2=(-dbe*aa-be*daa+dal*bb+al*dbb)
      dt3=( dr1*aa+r1*daa-ds1*bb-s1*dbb)
      dt4=( ds1*aa+s1*daa+dr1*bb+r1*dbb)
      t11=(u1*dt1-du1*t1)/u2
      t22=(u1*dt2-du1*t2)/u2
      t33=p3*(u1*dt3-2.*t3*du1)/u3
      t44=p3*(u1*dt4-2.*t4*du1)/u3

      if(ii.ge.14) goto 1
      goto(1,2,2,1,2,1,2,1,1,2,2,1,2),ii

c     termes u, ny, my ,w + wøø, vø et xo zo  (c.à.d. 1,4,6,8+9,12 et 14
c     ------------------------------------------------------------------
   1  temp=t1*cos1+t2*sin1
      temp2= 2.* (  du1*(-t1+temp)
     *   + u1*( dt1 + dal2*temp - cos1*(dt1+dbe2*t2)
     *                          - sin1*(dt2-dbe2*t1) )  )/u2
      dvarh(ii,k,1)=dvarh(ii,k,1) + temp2 * xi
      dvarh(ii,k,2)=dvarh(ii,k,2) + temp2 * p2

      dvarh(ii,k,jj+1)=-xi*t11 - t33
      dvarh(ii,k,jj+2)=-xi*t22 - t44
      dvarh(ii,k,jj+3)=-xf*t11 + t33
      dvarh(ii,k,jj+4)=-xf*t22 + t44
      dvarh(ii,k,jj+5)= xi*t11 - t33
      dvarh(ii,k,jj+6)= xi*t22 - t44
      dvarh(ii,k,jj+7)= xf*t11 + t33
      dvarh(ii,k,jj+8)= xf*t22 + t44
      goto 3

c     termes v, wø, nyx, ry + wøøø,uø etvøø (c.à.d. 2,3,5,7 +10,11,13)
c     ------------------------------------------
   2  temp=t3*cos1+t4*sin1
      dvarh(ii,k,1)=dvarh(ii,k,1) + 2.*p3*(-2.*du1*(t3-temp)
     *   + u1*( dt3 + dal2*temp - cos1*(dt3+dbe2*t4)
     *                          - sin1*(dt4-dbe2*t3) )  )/u3
      dvarh(ii,k,jj+1)=-xi*t11 - t33
      dvarh(ii,k,jj+2)=-xi*t22 - t44
      dvarh(ii,k,jj+3)= xf*t11 - t33
      dvarh(ii,k,jj+4)= xf*t22 - t44
      dvarh(ii,k,jj+5)=-xi*t11 + t33
      dvarh(ii,k,jj+6)=-xi*t22 + t44
      dvarh(ii,k,jj+7)= xf*t11 + t33
      dvarh(ii,k,jj+8)= xf*t22 + t44
    3 continue
    5 continue
    4 continue
      return
      end
