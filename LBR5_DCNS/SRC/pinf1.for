      subroutine pinf1(sum,ali,zb,hyp,kk,k,ms)
      implicit real*8(a-h,o-z)
      dimension ali(360),hyp(360),zb(360)
c***********************************************************************
c
c    calcul des termes independants pour la determination des
c    inconnues de bords lors de l'assemblage des panneaux dans mdr.
c    et cela pour la charge exterieure.
c
c    inf1 est relatif a la presence des traverses.
c
c***********************************************************************
      do 1 i=1,16
      sum=0.
      do 2 j=1,ms ! ms = 2* nbre de traverses
      ij=j+i*20
   2  sum=sum+ali(ij)*hyp(j+kk)
   1  zb(i+k)=-sum/10000.
      return
      end
