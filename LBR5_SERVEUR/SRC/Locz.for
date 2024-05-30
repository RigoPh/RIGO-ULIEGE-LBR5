      subroutine locz(arg,m,xi,phil,a,b,dish,teta,q,ijk,lamb,width,
     *                ms,cha,npts,bz,si,co,ex)
      implicit double precision(a-h,o-z)
      double precision lamb
	integer*4 npts

      dimension arg(8),a(720),b(11286),dish(1710),si(4),co(4),
     *          ex(4),y(21),cha(100,3),bz(1710)

c***********************************************************************
c     subroutine  locz
c     ++++++++++++++++
c     subroutine d'integration de la charge exterieure vertical de type gravite
c     ou poids propre (cas de la composante z normale au panneau)          
c                                                                       
c     voir subr. locy pour la composante tangantielle                   
c                                                                       
c     la charge varie selon ox, pas par pas mais est constante selon oy 
c                                                                       
c     les resultats sont places dans b (puis additioner à dish), 
c              (b = vecteur de travail)
c     les donnees sont contenues dans a (c.a.d. disc, charge unitaire)
c     et dans cha(i,1,is) pour la sollicitation.                 
c                                                                       
c                                                                       
c     modif: 12-2-96                               création : version bateau 1990
c***********************************************************************

      pi=2.d00*acos(0.d00)
      if(dabs(teta).ne.180.) goto 4
      cost=-1.
      sint=0.
      costt=-dcos(phil*pi/180.)
      sintt=-dsin(phil*pi/180.)
      goto 6
   4  if(dabs(teta).ne.90.) goto 5
      ii=1
      if(teta.eq.-90.) ii=-1
      cost=0.
      sint=ii
      costt=-dsin(phil*pi/180.)*ii
      sintt=dcos(phil*pi/180.)*ii
      goto 6
    5 cost=dcos(teta*pi/180.)
      sint=dsin(teta*pi/180.)
      costt=dcos((teta+phil)*pi/180.)
      sintt=dsin((teta+phil)*pi/180.)
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
      dpp=bp/(2.*z1)
      dmm=bm/(2.*z2)
      dp=al/(2.*z1)
      dm=al/(2.*z2)

      do 3 l=1,38															!!!aout04
      if((l.gt.(3+ms)).and.(l.le.23)) goto 3
      k=(l-1)*38+1
      jk=k+8*i-2
      j=(l-1)*16+4*i-3

      aa=a(j)
      bb=a(j+1)

      goto(2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,1,2	!!!aout04
     *,2,2,1,1,1,2,2),l													!!!aout04

c     termes u,w, xo,zo,  my et ny (c.à.d. 2,3, 4à23, 24,25)
   1  b(k+5)=b(k+5)+2.*(al*up*aa+be*um*bb-ex(i)*(aa*(-be*um*si(i)+
     *      al*up*co(i))+bb*(al*up*si(i)+be*um*co(i))))/(z1*z2)
      b(jk)=(aa*(-dmm+dpp)+bb*(dm-dp))*cost+
     *    (-aa*(dm+dp)-bb*(dmm+dpp))*sint
      b(jk+1)=(aa*(-dm+dp)+bb*(dpp-dmm))*cost+
     *    (aa*(dmm+dpp)-bb*(dp+dm))*sint
      b(jk+2)=-(aa*(-dmm+dpp)+bb*(dm-dp))*costt+
     *    (-aa*(dm+dp)-bb*(dmm+dpp))*sintt
      b(jk+3)=-(aa*(-dm+dp)+bb*(dpp-dmm))*costt+
     *    (aa*(dmm+dpp)-bb*(dp+dm))*sintt
      b(jk+4)=+(aa*(-dmm+dpp)+bb*(dm-dp))*cost-
     *    (-aa*(dm+dp)-bb*(dmm+dpp))*sint
      b(jk+5)=+(aa*(-dm+dp)+bb*(dpp-dmm))*cost-
     *    (aa*(dmm+dpp)-bb*(dp+dm))*sint
      b(jk+6)=-(aa*(-dmm+dpp)+bb*(dm-dp))*costt-
     *    (-aa*(dm+dp)-bb*(dmm+dpp))*sintt
      b(jk+7)=-(aa*(-dm+dp)+bb*(dpp-dmm))*costt-
     *    (aa*(dmm+dpp)-bb*(dp+dm))*sintt


	if (jk.eq.61) then
	continue
      endif

      goto  3

c     termes v,ry,nyx,nxy,wø  (c.à.d. 1, 26, 27, 28 et 29)
 2    b(k+4)=b(k+4)-2.*(aa*usp+2.*al*be*bb-ex(i)*
     *     (aa*(usp*co(i)-2.*al*be*si(i))+bb*(2.*al*be*co(i)+
     *     usp*si(i))))/(z1*z2*q)
      b(jk)=(aa*(-dmm+dpp)+bb*(dm-dp))*cost+
     *    (-aa*(dm+dp)-bb*(dmm+dpp))*sint
      b(jk+1)=(aa*(-dm+dp)+bb*(dpp-dmm))*cost+
     *    (aa*(dmm+dpp)-bb*(dp+dm))*sint
      b(jk+2)=+(aa*(-dmm+dpp)+bb*(dm-dp))*costt-
     *    (-aa*(dm+dp)-bb*(dmm+dpp))*sintt
      b(jk+3)=+(aa*(-dm+dp)+bb*(dpp-dmm))*costt-
     *    (aa*(dmm+dpp)-bb*(dp+dm))*sintt
      b(jk+4)=-(aa*(-dmm+dpp)+bb*(dm-dp))*cost+
     *    (-aa*(dm+dp)-bb*(dmm+dpp))*sint
      b(jk+5)=-(aa*(-dm+dp)+bb*(dpp-dmm))*cost+
     *    (aa*(dmm+dpp)-bb*(dp+dm))*sint
      b(jk+6)=-(aa*(-dmm+dpp)+bb*(dm-dp))*costt-
     *    (-aa*(dm+dp)-bb*(dmm+dpp))*sintt
      b(jk+7)=-(aa*(-dm+dp)+bb*(dpp-dmm))*costt-
     *    (aa*(dmm+dpp)-bb*(dp+dm))*sintt
  3   continue
 11   continue

      c=-2./(pi*ijk*10000.)
      e=0.
      y(1)=0.
      ipt=npts
      dw=width/float(ipt)
      do 155 i=1,ipt
 155  y(i+1)=y(i)+dw
      do 156 i=1,ipt
 156  e=e+cha(i,1)*(dcos(lamb*y(i+1))-dcos(lamb*y(i)))
      e=e*c*xi
      do 20 i=1,1710				!!!aout04
	if (i.eq.61) then
	continue
	endif
      dish(i)=dish(i)+e*b(i)
	bz(i)=b(i)  !!!aout04
 20   b(i)=0.
      return
      end
