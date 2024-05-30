      subroutine  press(arg,m,xi,xf,phil,a1,d,q,ijk,ms,bkse)
      use sharedvar
      implicit real *8(a-h,o-z)
      dimension arg(8),a1(720),d(1710)                          
c***********************************************************************
c     subroutine press
c     ++++++++++++++++
c     subroutine d'integration de la charge exterieure,(cas d'une charge
c     nornale au panneau variant lineairement le long de l'axe y de
c     xi (y=0) a xf (y=phil).
c     les resultats sont places dans d (c.a.d dish),
c     les donnees sont contenues  dans a (c.a.d. disc).
c
c     modif : 12-8-94                            créer : thèse ph. rigo     
c             12-2-96   (introduction de bkse)     
c***********************************************************************
      do 11 i=1,m
      al=arg(2*i-1)
      be=arg(2*i)
      u1=al*al+be*be
      r1=-al*al+be*be
      s1=2.*al*be

      p1=xf-xi
      p2=p1/(q*phil*pi/180.)
      p3=p2/u1
      p2=p2*q

      z1=1.-expt(i)*cosi(i)
      z2=expt(i)*sinu(i)

      do 3 l=1,38
      if((l.gt.(3+ms)).and.(l.le.23)) goto 3
      k=(l-1)*38
      jk=k+8*i-2
      j=(l-1)*16+4*i-3

      aa=a1(j)
      bb=a1(j+1)
      t1=( al*aa+be*bb)/u1
      t2=(-be*aa+al*bb)/u1
      t3=( r1*aa-s1*bb)*p3/u1
      t4=( s1*aa+r1*bb)*p3/u1

      goto(2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,1,2	!!!aout04
     *,2,2,1,1,1,2,2),l													!!!aout04

c     termes u,w, xo,zo,  my et ny (c.à.d. 2,3, 4à23, 24,25)
  1   temp=2.*(t1*z1-t2*z2)
      d(k+1)=d(k+1)+xi*temp
      d(k+2)=d(k+2)+temp*p2
      d(jk+1)=-xi*t1-t3
      d(jk+2)=-xi*t2-t4
      d(jk+3)=-xf*t1+t3
      d(jk+4)=-xf*t2+t4
      d(jk+5)= xi*t1-t3
      d(jk+6)= xi*t2-t4
      d(jk+7)= xf*t1+t3
      d(jk+8)= xf*t2+t4
      goto 3

c     termes v,ry,nyx,nxy,wø  (c.à.d. 1, 26, 27, 28 et 29)
 2    d(k+1)=d(k+1)+2.*(t3*z1-t4*z2)
      d(jk+1)=-xi*t1-t3
      d(jk+2)=-xi*t2-t4
      d(jk+3)= xf*t1-t3
      d(jk+4)= xf*t2-t4
      d(jk+5)=-xi*t1+t3
      d(jk+6)=-xi*t2+t4
      d(jk+7)= xf*t1+t3
      d(jk+8)= xf*t2+t4

    3 continue
   11 continue

      e1=bkse*4.*0.981/(pi*ijk)
      do 20 i=1,1710
 20   d(i)=e1*d(i)
      return
      end
