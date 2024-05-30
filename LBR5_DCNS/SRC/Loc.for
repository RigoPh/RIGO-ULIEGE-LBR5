      subroutine loc(arg,m,phil,a,d,dish,teta,q,ijk,lamb,width,
     *               impr,ms,is,cha,npts,bkse,nsol,si,co,ex)
      implicit real *8(a-h,o-z)
      real*8 lamb
      dimension arg(8),a(720),d(11286),dish(1710),si(4),co(4),
     *          ex(4),y(21),cha(100,3,10),npts(10)

c***********************************************************************
c     subroutine loc (cha2 et cha3)
c     +++++++++++++++++++++++++++++
c     subroutine d'integration de la charge de pression qui varie
c     selon la longueur de la structure
c     il s'agit d'une pression agissant nornalement au panneau et qui 
c     varie lineairement le long de l'axe y  c.a.d. chi (y=0) et chf (y=phil).                                           
c     chi=cha(l,2,is)  et   chf=cha(l,3,is)   avec is =1,nsol
c                                                                       
c     la charge varie selon ox, pas par pas (npts = nbre de pas).       
c
c
c     les resultats sont places dans d (puis additionner à dish),
c     les donnees sont contenues  dans a (c.a.d. disc).
c     le vecteur d est un vecteur de travail temporaire.
c
c                                                                       
c     modif: 14-2-96                        création : version bateau (1990)
c******************************************************************************
      
      pi=2.d00*acos(0.)
      y(1)=0.
      ipt=npts(is)
      dw=width/float(ipt)
      do 155 i=1,ipt
 155  y(i+1)=y(i)+dw

      qphil=q*phil*pi/180.
      c=-2.*bkse*0.981/(pi*ijk)

      do 156 ll=1,ipt
      chi=cha(ll,2,is)
      chf=cha(ll,3,is)
      
      do 11 i=1,m
      al=arg(2*i-1)
      be=arg(2*i)
      u1=al*al+be*be
      r1=-al*al+be*be
      s1=2.*al*be
c
      p1=chf-chi
      p2=p1/qphil
      p3=p2/u1
      p2=p2*q

      z1=1.-ex(i)*co(i)
      z2=ex(i)*si(i)

      do 3 l=1,29
      if((l.gt.(3+ms)).and.(l.le.23)) goto 3
      k=(l-1)*38
      jk=k+8*i-2
      j=(l-1)*16+4*i-3

      aa=a(j)
      bb=a(j+1)
      t1=( al*aa+be*bb)/u1
      t2=(-be*aa+al*bb)/u1
      t3=( r1*aa-s1*bb)*p3/u1
      t4=( s1*aa+r1*bb)*p3/u1

      goto(2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2),l

c     termes u,w, xo,zo,  my et ny (c.à.d. 2,3, 4à23, 24,25)
  1   temp=2.*(t1*z1-t2*z2)
      d(k+1)=d(k+1)+chi*temp
      d(k+2)=d(k+2)+temp*p2
      d(jk+1)=-chi*t1-t3
      d(jk+2)=-chi*t2-t4
      d(jk+3)=-chf*t1+t3
      d(jk+4)=-chf*t2+t4
      d(jk+5)= chi*t1-t3
      d(jk+6)= chi*t2-t4
      d(jk+7)= chf*t1+t3
      d(jk+8)= chf*t2+t4
      goto 3

c     termes v,ry,nyx,nxy,wø  (c.à.d. 1, 26, 27, 28 et 29)
 2    d(k+1)=d(k+1)+2.*(t3*z1-t4*z2)
      d(jk+1)=-chi*t1-t3
      d(jk+2)=-chi*t2-t4
      d(jk+3)= chf*t1-t3
      d(jk+4)= chf*t2-t4
      d(jk+5)=-chi*t1+t3
      d(jk+6)=-chi*t2+t4
      d(jk+7)= chf*t1+t3
      d(jk+8)= chf*t2+t4
  3   continue
 11   continue

      e=c* ( dcos(lamb*y(ll+1))-dcos(lamb*y(ll)) )

      do 20 i=1,1102
      dish(i)=dish(i)+e*d(i)
 20   d(i)=0.
156   continue
      return
      end
