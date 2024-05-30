subroutine sorti(lamb,dis,mt,eff,z2)
implicit double precision(a-h,o-z)
double precision lamb
dimension dis(5),eff(9690),z2(2295)

!--------------------------------------------------------------------------
!
!     subroutine result(forme reduite u,v,w)
!     subroutine de mise en forme des resultats pour 5 points de l'axe
!     des x a partir du vecteur z2,resultat du calcul sur les 31+20 de
!     l'axe des y(phi).
!     les resultats sont contenus dans les vecteurs eff
!
!     modifié : 23-8-94 (module optimisation)      créer: thèse de ph. rigo
!               24-3-99 (suppression du common/di)
!                                                                       
!--------------------------------------------------------------------------

kh=31+2*mt

do k=1,5
   sp=dsin(lamb*dis(k))
   sq=dcos(lamb*dis(k))
   !
   !     calcul de v,u,w
   !     en 5 pts selon x et 31+20(max) selon y
   !
   do i=1,kh
      ij=i+(k-1)*51
      eff(ij)=eff(ij)+z2(i)*sp
      eff(ij+255)=eff(ij+255)+z2(i+51)*sq
      eff(ij+510)=eff(ij+510)+z2(i+102)*sp
   enddo
enddo

return
end
