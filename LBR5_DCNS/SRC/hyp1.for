      subroutine hyp1(d,mt,teta,zb,k)
      use sharedvar
      implicit real*8(a-h,o-z)
      dimension d(1710),zb(2*mt,17)
c***********************************************************************
c
c     calcule les termes independants du systeme en vue de determiner
c     l'effet des traverses sur la coque.
c     et cela pour la charge exterieure (dish=d)
c
c     modifié: le 21-6-95 (ph. rigo)                   créer : thèse
c***********************************************************************
      pi1=pi/180.
      do  3 i=1,mt
      abtrr=abtr(i)*pi1
      if(dabs(teta).ne.180.) goto 1
      sin=-dsin(abtrr)
      goto 2
  1   sin=dsin((teta+abtr(i))*pi1)
  2   zb(i,k)=zb(i,k)+d(39+76*i)+d(40+76*i)*abtrr
     *                          +d(41+76*i)*(abtrr*abtrr)
     *                          +d(42+76*i)*(abtrr**3)
     *                          +d(44+76*i)*sin
      ij=i+mt
  3   zb(ij,k)=zb(ij,k)+d(77+76*i)+d(78+76*i)*abtrr
     *                            +d(79+76*i)*(abtrr*abtrr)
     *                            +d(80+76*i)*(abtrr**3)
     *                            +d(82+76*i)*sin
      return
      end
