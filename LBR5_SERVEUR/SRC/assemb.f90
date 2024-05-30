subroutine assemb(lam,equ,aa,ab,ac,ad,ae,af,ai,aj,ak)

implicit double precision(a-h,o-z)

double precision lam
dimension equ(9),lam(8)

! ====================================================================================================
!
!calcul des coefficients de l'équation du 8éme ordre  (cfr subr. bo1)
!input : aa, .. ak              output : equ
!calcul des coefficients de l'équation du 8éme ordre  (cfr subr. darg)
!input : aa, .. ak              output : dequ
!
! ====================================================================================================

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
