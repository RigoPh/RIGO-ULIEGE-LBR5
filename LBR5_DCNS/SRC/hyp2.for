      subroutine hyp2(d,mt,zb,tm,tn,tp,tq,tmm,tnn,tpp,tqq,kh,i,k)

      implicit real*8(a-h,o-z)
      dimension d(1710),zb(2*mt,17)

c***********************************************************************
c    calcule les termes independants du systeme en vue de determiner
c    l'effet des traverses sur la coque.
c    et cela pour la charge exterieure (d=dish)
c                                                                       
c     modifié: le 21-6-95 (ph. rigo)                        créer : thèse        
c***********************************************************************
      zb(i,k)=zb(i,k)+d(kh)*tm+d(kh+1)*tn+d(kh+2)*tp+d(kh+3)*tq
     *    +d(kh+4)*tmm+d(kh+5)*tnn+d(kh+6)*tpp+d(kh+7)*tqq
      j=i+mt
      zb(j,k)=zb(j,k)+d(kh+38)*tm+d(kh+39)*tn+d(kh+40)*tp+d(kh+41)*tq
     *       +d(kh+42)*tmm+d(kh+43)*tnn+d(kh+44)*tpp+d(kh+45)*tqq
      return
      end
