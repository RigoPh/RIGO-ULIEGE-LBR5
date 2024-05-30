      subroutine hydro(arg,m,xi,phil,disc,dish,teta,q,ijk,ms,bkse,
     *                 si,co,ex)
      implicit double precision(a-h,o-z)
      dimension arg(8),si(4),co(4),ex(4)
      dimension dish(1710),disc(720)

c***********************************************************************
c
c     subroutine hydro
c     *****************
c     subroutine d'integration de la charge exterieure,(cas de la pres-
c     sion hydraulique),supposee continue entre y=0 et y=yo.
c     les resultats sont places dans b (c.a.d dish),
c     les donnees sont contenues  dans a (c.a.d. disc).
c
c     modif : 11-8-94                            créer : thèse ph. rigo
c             12-2-96   (introduction de bkse)
c***********************************************************************

      pi=2.d00*acos(0.d00)
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

      do 11 i=1,m
      al=arg(2*i-1)
      be=arg(2*i)
      u=al*al+be*be
      up=u+1./(q*q)
      um=u-1./(q*q)
      bp=be+1./q
      bm=be-1./q
      usp=up-2.*be*be
      z1=al*al+bp*bp
      z2=al*al+bm*bm
      dpp=bp*q/(2.*z1)
      dmm=bm*q/(2.*z2)
      dp=al*q/(2.*z1)
      dm=al*q/(2.*z2)

      do 3 l=1,38
      if((l.gt.(3+ms)).and.(l.le.23)) goto 3
      k=(l-1)*38
      jk=k+8*i-2
      j=(l-1)*16+4*i-3
      aa=disc(j)
      bb=disc(j+1)
      t1= al*aa+be*bb
      t2=-be*aa+al*bb
      sint1=q*sint*t1/u
      sint2=q*sint*t2/u

      goto(2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,1,2	!!!aout04
     *,2,2,1,1,1,2,2),l

c     termes u,w, xo,zo,  my et ny (c.….d. 2,3, 4 … 23, 24 et 25)
  1   dish(k+1)=dish(k+1)+2.*(xi-q*sint*kk)*
     *      (t1-ex(i)*(t1*co(i)+t2*si(i)))/u
      dish(k+6)=dish(k+6)+2.*q*kk*(al*up*aa+be*um*bb-ex(i)*
     *      (aa*(-be*um*si(i)+
     *      al*up*co(i))+bb*(al*up*si(i)+be*um*co(i))))/(z1*z2)
      dish(jk+1)=(aa*(-dmm+dpp)+bb*(dm-dp))*cost+
     *         ((-aa*(dm+dp)-bb*(dmm+dpp))*sint+sint1)*jj
      dish(jk+1)=dish(jk+1)-xi*t1/u
      dish(jk+2)=(aa*(-dm+dp)+bb*(dpp-dmm))*cost+
     *      ((aa*(dmm+dpp)-bb*(dp+dm))*sint+sint2)*jj
      dish(jk+2)=dish(jk+2)-xi*t2/u
      dish(jk+3)=-(aa*(-dmm+dpp)+bb*(dm-dp))*costt+
     *          ((-aa*(dm+dp)-bb*(dmm+dpp))*sintt+sint1)*kk
      dish(jk+3)=dish(jk+3)-xi*t1/u
      dish(jk+4)=-(aa*(-dm+dp)+bb*(dpp-dmm))*costt+
     *       ((aa*(dmm+dpp)-bb*(dp+dm))*sintt+sint2)*kk
      dish(jk+4)=dish(jk+4)-xi*t2/u
      dish(jk+5)=+(aa*(-dmm+dpp)+bb*(dm-dp))*cost+
     *         (-(-aa*(dm+dp)-bb*(dmm+dpp))*sint-sint1)*jj
      dish(jk+5)=dish(jk+5)+xi*t1/u
      dish(jk+6)=+(aa*(-dm+dp)+bb*(dpp-dmm))*cost+
     *      (-(aa*(dmm+dpp)-bb*(dp+dm))*sint-sint2)*jj
      dish(jk+6)=dish(jk+6)+xi*t2/u
      dish(jk+7)=-(aa*(-dmm+dpp)+bb*(dm-dp))*costt+
     *         (-(-aa*(dm+dp)-bb*(dmm+dpp))*sintt-sint1)*kk
      dish(jk+7)=dish(jk+7)+xi*t1/u
      dish(jk+8)=-(aa*(-dm+dp)+bb*(dpp-dmm))*costt+
     *      (-(aa*(dmm+dpp)-bb*(dp+dm))*sintt-sint2)*kk
      dish(jk+8)=dish(jk+8)+xi*t2/u
      goto 3

c     termes v,ry,nyx,nxy,wø  (c.….d. 1, 26, 27, 28 et 29)
  2   dish(k+5)=dish(k+5)-2.*(aa*usp+2.*al*be*bb-ex(i)*
     *     (aa*(usp*co(i)-2.*al*be*si(i))+bb*(2.*al*be*co(i)+
     *     usp*si(i))))/(z1*z2)
      dish(jk+1)=(aa*(-dmm+dpp)+bb*(dm-dp))*cost+
     *         ((-aa*(dm+dp)-bb*(dmm+dpp))*sint+sint1)*jj
      dish(jk+1)=dish(jk+1)-xi*t1/u
      dish(jk+2)=(aa*(-dm+dp)+bb*(dpp-dmm))*cost+
     *      ((aa*(dmm+dpp)-bb*(dp+dm))*sint+sint2)*jj
      dish(jk+2)=dish(jk+2)-xi*t2/u
      dish(jk+3)=(aa*(-dmm+dpp)+bb*(dm-dp))*costt+
     *        (-(-aa*(dm+dp)-bb*(dmm+dpp))*sintt-sint1)*kk
      dish(jk+3)=dish(jk+3)+xi*t1/u
      dish(jk+4)=(aa*(-dm+dp)+bb*(dpp-dmm))*costt+
     *     (-(aa*(dmm+dpp)-bb*(dp+dm))*sintt-sint2)*kk
      dish(jk+4)=dish(jk+4)+xi*t2/u
      dish(jk+5)=-(aa*(-dmm+dpp)+bb*(dm-dp))*cost+
     *          ((-aa*(dm+dp)-bb*(dmm+dpp))*sint+sint1)*jj
      dish(jk+5)=dish(jk+5)-xi*t1/u
      dish(jk+6)=-(aa*(-dm+dp)+bb*(dpp-dmm))*cost+
     *       ((aa*(dmm+dpp)-bb*(dp+dm))*sint+sint2)*jj
      dish(jk+6)=dish(jk+6)-xi*t2/u
      dish(jk+7)=-(aa*(-dmm+dpp)+bb*(dm-dp))*costt+
     *         (-(-aa*(dm+dp)-bb*(dmm+dpp))*sintt-sint1)*kk
      dish(jk+7)=dish(jk+7)+xi*t1/u
      dish(jk+8)=-(aa*(-dm+dp)+bb*(dpp-dmm))*costt+
     *      (-(aa*(dmm+dpp)-bb*(dp+dm))*sintt-sint2)*kk
      dish(jk+8)=dish(jk+8)+xi*t2/u
  3   continue
 11   continue

      e=bkse*4.*0.981/(ijk*pi)
      do 20 i=1,1710
 20   dish(i)=e*dish(i)
      return
      end
