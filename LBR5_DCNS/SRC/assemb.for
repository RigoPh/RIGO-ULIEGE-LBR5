      subroutine assemb(lam,equ,aa,ab,ac,ad,ae,af,ai,aj,ak)

      implicit real*8(a-h,o-z)
      real*8 lam
      dimension equ(9),lam(8)
c
c     calcul des coéfficients de l'équation du 8éme ordre  (cfr subr. bo1)
c     input : aa, .. ak              output : equ
c     calcul des coéfficients de l'équation du 8éme ordre  (cfr subr. darg)
c     input : aa, .. ak              output : dequ

c     version du : 16-3-94     			créer 28-1-94

      equ(1)=+aa*lam(8)-ab*lam(6)+ad*lam(4)
      equ(2)=0.d+00
      equ(3)=-ac*lam(6)+ae*lam(4)
      equ(4)=0.d+00
      equ(5)=+af*lam(4)-ai*lam(2)
      equ(6)=0.d+00
      equ(7)=-aj*lam(2)
      equ(8)=0.d+00
      equ(9)=ak
      return
      end
