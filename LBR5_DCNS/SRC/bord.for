      subroutine bord(arg,m,a,b,c,d,phil,disb,dl,q,ijk,width,delto,
     *                nc,ms)
      implicit real *8(a-h,o-z)
      dimension arg(8),disb(464),dl(1102),si(4),co(4),ex(4)
      common /py/pi,si,co,ex
c***********************************************************************
c
c     subroutine  bord
c     subroutine d'integration des sollicitations de bord
c                 soit des forces  dans le plan de la coque (disb)
c                 soit des moments dans le plan de la coque (disc)
c     les sollicitations sont de la forme a*(y**3)+b(y**2)+c*y+d
c
c     les donnees (coefficients de la fonction de bord) sont contenues dans  a,b,c,d
c                   et dans disb (cas de charge unitaire de type b)
c     les resultats sont contenus dans dl(disl),

c     modifi� : 29-3-96 (simplification et )   cr�er: lbr-3, th�se,1988
c                       (uniformisation avec subr. mom)

c***********************************************************************
      do 10 i=1,1102
   10 dl(i)=0.

      do 11 i=1,m
      al=arg(2*i-1)
      be=arg(2*i)

      u1=al*al+be*be
      u2=u1*u1
      u3=u2*u1
      u4=u2*u2
      p1=q*phil*pi/180.
      p2=p1*p1
      p3=p2*p1
      cos1=ex(i)*co(i)
      sin1=ex(i)*si(i)

      r1=-al*al+be*be
      s1=2.*al*be
      r2=(-al*al+3.*be*be)*2.*al
      s2=(-3.*al*al+be*be)*2.*be
      r3=6.*(al**4+be**4-6.*((al*be)**2))
      s3=-12.*s1*r1

      alu=al/u1
      beu=be/u1
      r1u=r1/u2
      s1u=s1/u2
      r2u=r2/u3
      s2u=s2/u3
      r3u=r3/u4
      s3u=s3/u4

      do 3 l=1,29
      if((l.gt.(3+ms)).and.(l.le.23)) goto 3
      k=(l-1)*38+1
      jk=k+8*i-2
      j=(l-1)*16+4*i-3

      aa=disb(j)
      bb=disb(j+1)
      t1u= alu*aa+beu*bb
      t2u=-beu*aa+alu*bb
      t3u= r1u*aa-s1u*bb
      t4u= s1u*aa+r1u*bb
      t5u= r2u*aa+s2u*bb
      t6u=-s2u*aa+r2u*bb
      t7u= r3u*aa+s3u*bb
      t8u=-s3u*aa+r3u*bb

      goto(2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2),l

c     termes u,w, xo,zo,  my et ny (c.�.d. 2,3, 4�23, 24,25)
c     -----------------------------------------------------
  1   temp1=2.*( t1u*(1-cos1)-sin1*t2u)
      temp2=2.*(-t5u*(1-cos1)+sin1*t6u)
      dl(k)  =dl(k)  +temp1*d     +temp2*b
      dl(k+1)=dl(k+1)+temp1*c*q   +temp2*a*q*3.
      dl(k+2)=dl(k+2)+temp1*b*q*q
      dl(k+3)=dl(k+3)+temp1*a*q*q*q

      dl(jk  )=-d*t1u-t3u*c+t5u*b+t7u*a
      dl(jk+1)=-d*t2u-t4u*c+t6u*b+t8u*a
      dl(jk+4)= d*t1u-t3u*c-t5u*b+t7u*a
      dl(jk+5)= d*t2u-t4u*c-t6u*b+t8u*a

      dl(jk+2)=-dl(jk+4)
     * + p1*(-t1u*c+t3u*2.*b+t5u*3.*a)
     * + p2*(-t1u*b+t3u*3.*a)
     * + p3*(-t1u*a)
      dl(jk+3)=-dl(jk+5)
     * + p1*(-t2u*c+t4u*2.*b+t6u*3.*a)
     * + p2*(-t2u*b+t4u*3.*a)
     * + p3*(-t2u*a)
      dl(jk+6)=-dl(jk)
     * + p1*( t1u*c+t3u*2.*b-t5u*3.*a)
     * + p2*( t1u*b+t3u*3.*a)
     * + p3*( t1u*a)
      dl(jk+7)=-dl(jk+1)
     * + p1*( t2u*c+t4u*2.*b-t6u*3.*a)
     * + p2*( t2u*b+t4u*3.*a)
     * + p3*( t2u*a)
      goto 3

c     termes v,ry,nyx,nxy,w�  (c.�.d. 1, 26, 27, 28 et 29)
c     -----------------------------------------------------
  2   temp1=2.*( t3u*(1-cos1)-sin1*t4u)
      temp2=2.*(-t7u*(1-cos1)+sin1*t8u)
      dl(k)  =dl(k)  +temp1*c       +temp2*a
      dl(k+1)=dl(k+1)+temp1*b*q*2.
      dl(k+2)=dl(k+2)+temp1*a*q*q*3.

      dl(jk  )=-d*t1u-t3u*c+t5u*b+t7u*a
      dl(jk+1)=-d*t2u-t4u*c+t6u*b+t8u*a
      dl(jk+4)=-d*t1u+t3u*c+t5u*b-t7u*a
      dl(jk+5)=-d*t2u+t4u*c+t6u*b-t8u*a

      dl(jk+2)=-dl(jk+4)
     *  - p1*(-t1u*c+t3u*2.*b+t5u*3.*a)
     *  - p2*(-t1u*b+t3u*3.*a)
     *  - p3*(-t1u*a)
      dl(jk+3)=-dl(jk+5)
     *  - p1*(-t2u*c+t4u*2.*b+t6u*3.*a)
     *  - p2*(-t2u*b+t4u*3.*a)
     *  - p3*(-t2u*a)
      dl(jk+6)=-dl(jk  )
     *  + p1*( t1u*c+t3u*2.*b-t5u*3.*a)
     *  + p2*( t1u*b+t3u*3.*a)
     *  + p3*( t1u*a)
      dl(jk+7)=-dl(jk+1)
     *  + p1*( t2u*c+t4u*2.*b-t6u*3.*a)
     *  + p2*( t2u*b+t4u*3.*a)
     *  + p3*( t2u*a)

  3   continue
 11   continue
c                                                                                                                                    bmo01330

      nfour=2*ijk-1
      e=4.*dsin(nfour*pi/2.)*dcos(nfour*pi*(width-2.*delto)/
     *(2.*width))/(nfour*pi*delto*10000.)
      if(nc.eq.1) goto 687
      e=e*nfour*pi/width
 687  do 20 i=1,1102
 20   dl(i)=e*dl(i)
      return
      end
