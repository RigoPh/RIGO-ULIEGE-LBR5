      subroutine dmomsr(arg,darg,m,phil,disb,dvarb,dtemp,dvarh,teta,q,
     *                  impr,nxi,nbrxi,mt,a,b,c,d,si,co,ex)
      implicit real *8(a-h,o-z)
      real*8 lamb
      dimension arg(8),nxi(9),
     *  disb(720),dvarb(33,9,16),dtemp(33,9,38),dvarh(33,9,38)
      dimension si(4),co(4),ex(4)
      dimension darg(8,9)

c***********************************************************************
c
c    expression (partiel) des dérivées de dish (cfr subr mom)
c    -----------------------------------------------------------
c    dvarh(13,9,38) avec 13 variables (u v wø ny nyx my ry w)wøø wøøø uø
c                         9 variables de conception,
c                     et 38 termes (cfr dish).

c     les donnees sont contenues  dans dvarb.
c              dvarb = contient les dérivées de disb

c     les resultats sont places dans "dtemp" (puis additionner à "dvarh"
c       (le vecteur "dtemp" est un vecteur de travail temporaire)

c     modif: 29-3-96                                     créer : 20 mars
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

      call cr(arg,darg,m,a,b,c,d,phil,q,disb,dvarb,dtemp, 2,1,nxi,nbrxi,
     *        si,co,ex)
      call cr(arg,darg,m,a,b,c,d,phil,q,disb,dvarb,dtemp, 1,2,nxi,nbrxi,
     *        si,co,ex)
      call cr(arg,darg,m,a,b,c,d,phil,q,disb,dvarb,dtemp,29,3,nxi,nbrxi,
     *        si,co,ex)
      call cr(arg,darg,m,a,b,c,d,phil,q,disb,dvarb,dtemp,25,4,nxi,nbrxi,
     *        si,co,ex)
      call cr(arg,darg,m,a,b,c,d,phil,q,disb,dvarb,dtemp,27,5,nxi,nbrxi,
     *        si,co,ex)
      call cr(arg,darg,m,a,b,c,d,phil,q,disb,dvarb,dtemp,24,6,nxi,nbrxi,
     *        si,co,ex)
      call cr(arg,darg,m,a,b,c,d,phil,q,disb,dvarb,dtemp,26,7,nxi,nbrxi,
     *        si,co,ex)
      call cr(arg,darg,m,a,b,c,d,phil,q,disb,dvarb,dtemp, 3,8,nxi,nbrxi,
     *        si,co,ex)
      call cr(arg,darg,m,a,b,c,d,phil,q,disb,dvarb,dtemp,35,9,nxi,nbrxi,
     *        si,co,ex)
      call cr(arg,darg,m,a,b,c,d,phil,q,disb,dvarb,dtemp,38,10,nxi,
     *        nbrxi,si,co,ex)
      call cr(arg,darg,m,a,b,c,d,phil,q,disb,dvarb,dtemp,31,11,nxi,
     *        nbrxi,si,co,ex)
      call cr(arg,darg,m,a,b,c,d,phil,q,disb,dvarb,dtemp,30,12,nxi,
     *        nbrxi,si,co,ex)
      call cr(arg,darg,m,a,b,c,d,phil,q,disb,dvarb,dtemp,33,13,nxi,
     *        nbrxi,si,co,ex)

      do 2 j=1,mt
      k1=2+j*2
      k2=12+j*2
      call cr(arg,darg,m,a,b,c,d,phil,q,disb,dvarb,dtemp,k1,k2,nxi,
     *        nbrxi,si,co,ex)
      k1=k1+1
      k2=k2+1
      call cr(arg,darg,m,a,b,c,d,phil,q,disb,dvarb,dtemp,k1,k2,nxi,
     *        nbrxi,si,co,ex)
    2 continue

      j1=6+8*m
      do 1 ii=1,13+2*mt
      do 1 kk=1,nbrxi
      k=nxi(kk)
      do 1 j=1,j1
      dvarh(ii,k,j)=dvarh(ii,k,j)+dtemp(ii,k,j)
      dtemp(ii,k,j)=0.
   1  continue


      return
      end

      subroutine cr(arg,darg,m,a,b,c,d,phil,q,disb,dvarb,dtemp,
     *              i,ii,nxi,nbrxi,si,co,ex)
      implicit real *8(a-h,o-z)
      dimension arg(8),disb(720),dvarb(33,9,16),dtemp(33,9,38),nxi(9)
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
      p1=q*phil*pi/180.
      p2=p1*p1
      p3=p2*p1

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

      u1=al*al+be*be
      u2=u1*u1
      u3=u2*u1
      u4=u2*u2
      u5=u2*u3
      du1=2.*( al*dal+be*dbe)

      alu=al/u1
      beu=be/u1
      r1=-al*al+be*be
      s1=2.*al*be
      r1u=r1/u2
      s1u=s1/u2

      dalu= r1u*dal-s1u*dbe
      dbeu=-r1u*dbe-s1u*dal
      dr1=2.*(-al*dal+be*dbe)
      ds1=2.*( al*dbe+be*dal)
      dr1u=(u1*dr1-2.*r1*du1)/u3
      ds1u=(u1*ds1-2.*s1*du1)/u3

      r2=(-al*al+3.*be*be)*2.*al
      s2=(-3.*al*al+be*be)*2.*be
      r2u=r2/u3
      s2u=s2/u3
      dr2u=(6.*u1*(r1*dal+s1*dbe)-3.*r2*du1)/u4
      ds2u=(6.*u1*(r1*dbe-s1*dal)-3.*s2*du1)/u4

      r3=6.*(al**4+be**4-6.*((al*be)**2))
      s3=24.*al*be*(-r1)
      r3u=r3/u4
      s3u=s3/u4
      dr3u=( 12.*u1*(-r2*dal+s2*dbe)-4.*r3*du1)/u5
      ds3u=(-12.*u1*( s2*dal+r2*dbe)-4.*s3*du1)/u5

      cos1=ex(j)*co(j)
      sin1=ex(j)*si(j)

      aa=disb(jk+1)
      bb=disb(jk+2)
      daa=dvarb(ii,k,l+1)
      dbb=dvarb(ii,k,l+2)

      t1u= alu*aa+beu*bb
      t2u=-beu*aa+alu*bb
      t3u= r1u*aa-s1u*bb
      t4u= s1u*aa+r1u*bb
      t5u= r2u*aa+s2u*bb
      t6u=-s2u*aa+r2u*bb
      t7u= r3u*aa+s3u*bb
      t8u=-s3u*aa+r3u*bb

      dt1u= dalu*aa+alu*daa+dbeu*bb+beu*dbb
      dt2u=-dbeu*aa-beu*daa+dalu*bb+alu*dbb
      dt3u= dr1u*aa+r1u*daa-ds1u*bb-s1u*dbb
      dt4u= ds1u*aa+s1u*daa+dr1u*bb+r1u*dbb
      dt5u= dr2u*aa+r2u*daa+ds2u*bb+s2u*dbb
      dt6u=-ds2u*aa-s2u*daa+dr2u*bb+r2u*dbb
      dt7u= dr3u*aa+r3u*daa+ds3u*bb+s3u*dbb
      dt8u=-ds3u*aa-s3u*daa+dr3u*bb+r3u*dbb

      if(ii.ge.14) goto 1
      goto(1,2,2,1,2,1,2,1,1,2,2,1,2),ii

c     termes u, ny, my ,w + wøø, vø et xo zo  (c.à.d. 1,4,6,8+9,12 et 14
c     ------------------------------------------------------------------
   1  continue

      temp1=piq2*(dal*cos1+dbe*sin1)*t1u
     *     +piq2*(dal*sin1-dbe*cos1)*t2u
     *     + (1.-cos1)*dt1u - sin1*dt2u
      temp2=-piq2*(dal*cos1+dbe*sin1)*t5u
     *      -piq2*(dal*sin1-dbe*cos1)*t6u
     *      -(1.-cos1)*dt5u + sin1*dt6u

      dtemp(ii,k,1)=dtemp(ii,k,1)+2.*     (temp1*d+temp2*   b)
      dtemp(ii,k,2)=dtemp(ii,k,2)+2.*q*   (temp1*c+temp2*3.*a)
      dtemp(ii,k,3)=dtemp(ii,k,3)+2.*q*q*  temp1*b
      dtemp(ii,k,4)=dtemp(ii,k,4)+2.*q*q*q*temp1*a

      dtemp(ii,k,jj+1)=-dt1u*d-dt3u*c+dt5u*b+dt7u*a
      dtemp(ii,k,jj+2)=-dt2u*d-dt4u*c+dt6u*b+dt8u*a
      dtemp(ii,k,jj+5)=+dt1u*d-dt3u*c-dt5u*b+dt7u*a
      dtemp(ii,k,jj+6)=+dt2u*d-dt4u*c-dt6u*b+dt8u*a

      dtemp(ii,k,jj+3)=-dtemp(ii,k,jj+5)
     *     + p3* ( -dt1u*a )
     *     + p2* ( -dt1u*b + dt3u*3.*a )
     *     + p1* ( -dt1u*c + dt3u*2.*b + dt5u*3.*a )
      dtemp(ii,k,jj+4)=-dtemp(ii,k,jj+6)
     *     + p3* ( -dt2u*a )
     *     + p2* ( -dt2u*b + dt4u*3.*a )
     *     + p1* ( -dt2u*c + dt4u*2.*b + dt6u*3.*a )
      dtemp(ii,k,jj+7)=-dtemp(ii,k,jj+1)
     *     + p3* (  dt1u*a )
     *     + p2* (  dt1u*b + dt3u*3.*a )
     *     + p1* (  dt1u*c + dt3u*2.*b - dt5u*3.*a )
      dtemp(ii,k,jj+8)=-dtemp(ii,k,jj+2)
     *     + p3* (  dt2u*a )
     *     + p2* (  dt2u*b + dt4u*3.*a )
     *     + p1* (  dt2u*c + dt4u*2.*b - dt6u*3.*a )
      goto 3

c     termes v, wø, nyx, ry + wøøø,uø etvøø (c.à.d. 2,3,5,7 +10,11,13)
c     ------------------------------------------
   2  temp1=piq2*(dal*cos1+dbe*sin1)*t3u
     *     +piq2*(dal*sin1-dbe*cos1)*t4u
     *     + (1.-cos1)*dt3u - sin1*dt4u
      temp2=-piq2*(dal*cos1+dbe*sin1)*t7u
     *      -piq2*(dal*sin1-dbe*cos1)*t8u
     *      -(1.-cos1)*dt7u + sin1*dt8u

      dtemp(ii,k,1)=dtemp(ii,k,1)+2.*     (temp1*c+temp2*   a)
      dtemp(ii,k,2)=dtemp(ii,k,2)+4.*q*    temp1*b
      dtemp(ii,k,3)=dtemp(ii,k,3)+6.*q*q*  temp1*a

      dtemp(ii,k,jj+1)=-dt1u*d-dt3u*c+dt5u*b+dt7u*a
      dtemp(ii,k,jj+2)=-dt2u*d-dt4u*c+dt6u*b+dt8u*a
      dtemp(ii,k,jj+5)=-dt1u*d+dt3u*c+dt5u*b-dt7u*a
      dtemp(ii,k,jj+6)=-dt2u*d+dt4u*c+dt6u*b-dt8u*a

      dtemp(ii,k,jj+3)=-dtemp(ii,k,jj+5)
     *     - p3* ( -dt1u*a )
     *     - p2* ( -dt1u*b + dt3u*3.*a )
     *     - p1* ( -dt1u*c + dt3u*2.*b + dt5u*3.*a )
      dtemp(ii,k,jj+4)=-dtemp(ii,k,jj+6)
     *     - p3* ( -dt2u*a )
     *     - p2* ( -dt2u*b + dt4u*3.*a )
     *     - p1* ( -dt2u*c + dt4u*2.*b + dt6u*3.*a )
      dtemp(ii,k,jj+7)=-dtemp(ii,k,jj+1)
     *     + p3* (  dt1u*a )
     *     + p2* (  dt1u*b + dt3u*3.*a )
     *     + p1* (  dt1u*c + dt3u*2.*b - dt5u*3.*a )
      dtemp(ii,k,jj+8)=-dtemp(ii,k,jj+2)
     *     + p3* (  dt2u*a )
     *     + p2* (  dt2u*b + dt4u*3.*a )
     *     + p1* (  dt2u*c + dt4u*2.*b - dt6u*3.*a )
    3 continue
    5 continue
    4 continue
      return
      end
