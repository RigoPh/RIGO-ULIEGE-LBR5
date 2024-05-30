      subroutine sorti(lamb,dis,mt,eff,z2)
      implicit real*8(a-h,o-z)
      real*8 lamb
      dimension dis(5),eff(9690),z2(2295)

c***********************************************************************
c
c     subroutine result(forme reduite u,v,w)
c     subroutine de mise en forme des resultats pour 5 points de l'axe
c     des x a partir du vecteur z2,resultat du calcul sur les 31+20 de
c     l'axe des y(phi).
c     les resultats sont contenus dans les vecteurs eff
c
c     modifié : 23-8-94 (module optimisation)      créer: thèse de ph. rigo
c               24-3-99 (suppression du common/di)
c                                                                       
c***********************************************************************

      kh=31+2*mt

      do 114 k=1,5
      sp=dsin(lamb*dis(k))
      sq=dcos(lamb*dis(k))
c
c     calcul de v,u,w
c     en 5 pts selon x et 31+20(max) selon y
c
      do 112 i=1,kh
      ij=i+(k-1)*51
      eff(ij)=eff(ij)+z2(i)*sp
      eff(ij+255)=eff(ij+255)+z2(i+51)*sq
      eff(ij+510)=eff(ij+510)+z2(i+102)*sp
  112 continue
  114 continue

      return
      end
