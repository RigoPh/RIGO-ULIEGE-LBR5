      subroutine pinteg(phil,teta,l,ll,som,z2,mt,q,pi,abtr,
     *                  c1,c2,s1,s2,it,vmax)
      implicit real*8(a-h,o-z)
      dimension z2(2295),abtr(10)

c***********************************************************************
c
c      subroutine integr
c      subroutine d'integration d'une fonction sur un espace donne par
c      cumulation.
c
c******************************************************************+****

      sm=phil/30.
      ij=(l-1)*51
      ji=(ll-1)*51
      som=0.
c
      vmax=0.
      if(mt.eq.0) then
c
      do 15 i=1,31
      goto(20,21,22),it
c 20  write(6,*)som
  20  som=som+z2(ij+i)*dcos((teta+sm*(i-1))*pi/180.)
     *       +z2(ji+i)*dsin((teta+sm*(i-1))*pi/180.)
c     temp1=z2(ij+i)*dcos((teta+sm*(i-1))*pi/180.)
c     temp2=z2(ji+i)*dsin((teta+sm*(i-1))*pi/180.)
c     write(6,*)z2(ij+i),dcos((teta+sm*(i-1))*pi/180.),temp1
c     write(6,*)z2(ji+i),dsin((teta+sm*(i-1))*pi/180.),temp2,som
      goto 15
  21  som=som+z2(ij+i)
      vval=dabs(z2(ij+i))
      if(vval.gt.vmax)vmax=vval
      goto 15
c     write(6,*) som
  22  som=som+z2(ij+i)*dcos((teta+sm*(i-1))*pi/180.)
     *       -z2(ji+i)*dsin((teta+sm*(i-1))*pi/180.)
c     temp1=z2(ij+i)*dcos((teta+sm*(i-1))*pi/180.)
c     temp2=-z2(ji+i)*dsin((teta+sm*(i-1))*pi/180.)
c     write(6,*)z2(ij+i),dcos((teta+sm*(i-1))*pi/180.),temp1
c     write(6,*)z2(ji+i),dsin((teta+sm*(i-1))*pi/180.),temp2,som
  15  continue
c
      goto(23,24,25),it
  23  som=som-(z2(ij+1)*c1+z2(ij+31)*c2)*0.5
     *       -(z2(ji+1)*s1+z2(ji+31)*s2)*0.5
      goto 26
  24  som=som-(z2(ij+1)+z2(ij+31))*0.5
      goto 26
  25  som=som-(z2(ij+1)*c1+z2(ij+31)*c2)*0.5
     *       +(z2(ji+1)*s1+z2(ji+31)*s2)*0.5
c
  26  continue
c     if(mt.eq.0) goto 16
c     ij=ij+31
c     ji=ji+31
c     do 16 i=1,mt
c     goto(27,28,29),it
c 27  som=som-(z2(ij+2*i)-z2(ij+2*i-1))*0.5*dcos((teta+abtr(i))*pi/180.)
c    *       -(z2(ji+2*i)-z2(ji+2*i-1))*0.5*dsin((teta+abtr(i))*pi/180.)
c     goto 16
c 28  som=som-(z2(ij+2*i)-z2(ij+2*i-1))*0.5
c     goto 16
c 29  som=som-(z2(ij+2*i)-z2(ij+2*i-1))*0.5*dcos((teta+abtr(i))*pi/180.)
c    *       +(z2(ji+2*i)-z2(ji+2*i-1))*0.5*dsin((teta+abtr(i))*pi/180.)
  16  continue
      else
c
      do 115 i=1,30
      imt=0
c     write(6,*) i,imt
c     write(6,*) som
      do 130 j=1,mt
      n=abtr(j)*0.9999/sm
      if(n.ne.(i-1)) goto 130
      imt=1
      a=dmod((abtr(j)*0.9999),sm)/sm
      b=1.-a
c     write(6,*) n,j,i
c     write(6,*) a,b
      goto(127,128,129),it
 127  som=som+0.5*(a*(z2(ij+i)+z2(ij+31+2*j-1))
     *    +b*(z2(ij+i+1)+z2(ij+31+2*j)))*dcos((teta+sm*(i-0.5))*pi/180.)
     *       +0.5*(a*(z2(ji+i)+z2(ji+31+2*j-1))
     *    +b*(z2(ji+i+1)+z2(ji+31+2*j)))*dsin((teta+sm*(i-0.5))*pi/180.)
c     write(6,*) som
      goto 130
 128  som=som+0.5*(a*(z2(ij+i)+z2(ij+31+2*j-1))
     *            +b*(z2(ij+i+1)+z2(ij+31+2*j)))
c     write(6,*) som
      goto 130
 129  som=som+0.5*(a*(z2(ij+i)+z2(ij+31+2*j-1))
     *    +b*(z2(ij+i+1)+z2(ij+31+2*j)))*dcos((teta+sm*(i-0.5))*pi/180.)
     *       -0.5*(a*(z2(ji+i)+z2(ji+31+2*j-1))
     *    +b*(z2(ji+i+1)+z2(ji+31+2*j)))*dsin((teta+sm*(i-0.5))*pi/180.)
c     write(6,*) som
 130  continue
      if(imt.eq.1) goto 115
      goto(117,118,119),it
 117  som=som+0.5*(z2(ij+i)+z2(ij+i+1))*dcos((teta+sm*(i-0.5))*pi/180.)
     *       +0.5*(z2(ji+i)+z2(ji+i+1))*dsin((teta+sm*(i-0.5))*pi/180.)
      goto 115
 118  som=som+0.5*(z2(ij+i)+z2(ij+i+1))
      vval=dabs(z2(ij+i))
      if(vval.gt.vmax)vmax=vval
      goto 115
 119  som=som+0.5*(z2(ij+i)+z2(ij+i+1))*dcos((teta+sm*(i-0.5))*pi/180.)
     *       -0.5*(z2(ji+i)+z2(ji+i+1))*dsin((teta+sm*(i-0.5))*pi/180.)
 115  continue
      endif
      som=som*sm*q*pi/180.
      return
      end
