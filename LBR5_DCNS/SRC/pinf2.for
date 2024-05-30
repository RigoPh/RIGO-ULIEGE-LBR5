      subroutine pinf2(zb,d,s1,s2,c1,c2,ph,ph2,ph3,k)
      implicit real*8(a-h,o-z)
      dimension d(1710),zb(360)
c***********************************************************************
c
c    calcul des termes independants pour la determination des
c    inconnues de bords lors de l'assemblage des panneaux dans mdr.
c    et cela pour la charge exterieure.
c
c    inf2 est relatif aux termes indep., y, y**2 , y**3
c
c***********************************************************************
      zb(1+k)=zb(1+k)-d(77)-d(82)*s1
      zb(2+k)=zb(2+k)-d(77)-d(78)*ph-d(79)*ph2-d(80)*ph3-d(82)*s2
      zb(3+k)=zb(3+k)-d(39)-d(44)*s1
      zb(4+k)=zb(4+k)-d(39)-d(40)*ph-d(41)*ph2-d(42)*ph3-d(44)*s2
      zb(5+k)=zb(5+k)-d(875)-d(880)*s1
      zb(6+k)=zb(6+k)-d(875)-d(876)*ph-d(877)*ph2-d(878)*ph3-d(880)*s2
      zb(7+k)=zb(7+k)-d(913)-d(918)*s1
      zb(8+k)=zb(8+k)-d(913)-d(914)*ph-d(915)*ph2-d(916)*ph3-d(918)*s2
      zb(9+k)=zb(9+k)-d(989)-d(993)*c1
      zb(10+k)=zb(10+k)-d(989)-d(990)*ph-d(991)*ph2-d(993)*c2
      zb(11+k)=zb(11+k)-d(951)-d(955)*c1
      zb(12+k)=zb(12+k)-d(951)-d(952)*ph-d(953)*ph2-d(955)*c2
      zb(13+k)=zb(13+k)-d(1)-d(5)*c1
      zb(14+k)=zb(14+k)-d(1)-d(2)*ph-d(3)*ph2-d(5)*c2
      zb(15+k)=zb(15+k)-d(1065)-d(1069)*c1
      zb(16+k)=zb(16+k)-d(1065)-d(1066)*ph-d(1067)*ph2-d(1069)*c2
      return
      end
