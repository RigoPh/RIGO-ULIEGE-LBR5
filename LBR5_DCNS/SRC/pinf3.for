      subroutine pinf3(zb,a,c2,s2,c3,s3,c4,s4,k,kk)
      implicit real *8(a-h,o-z)
      dimension a(1710),aux(400),zb(360)
c***********************************************************************
c
c    calcul des termes independants pour la determination des
c    inconnues de bords lors de l'assemblage des panneaux dans mdr.
c    et cela pour la charge exterieure.
c
c    inf3 est relatif aux termes en exp(-alpha*y)*(..cos..+..sin..)
c
c***********************************************************************
      zb(1+kk)=zb(1+kk)-a(8*k+75)-a(8*k+77)*c2-a(8*k+78)*s2
     *      -a(8*k+79)*c3-a(8*k+80)*s3-a(8*k+81)*c4-a(8*k+82)*s4
      zb(2+kk)=zb(2+kk)-a(8*k+75)*c2-a(8*k+76)*s2-a(8*k+77)
     *      -a(8*k+79)*c4-a(8*k+80)*s4-a(8*k+81)*c3-a(8*k+82)*s3
      zb(3+kk)=zb(3+kk)-a(8*k+37)-a(8*k+39)*c2-a(8*k+40)*s2
     *      -a(8*k+41)*c3-a(8*k+42)*s3-a(8*k+43)*c4-a(8*k+44)*s4
      zb(4+kk)=zb(4+kk)-a(8*k+37)*c2-a(8*k+38)*s2-a(8*k+39)
     *      -a(8*k+41)*c4-a(8*k+42)*s4-a(8*k+43)*c3-a(8*k+44)*s3
      zb(5+kk)=zb(5+kk)-a(8*k+873)-a(8*k+875)*c2-a(8*k+876)*s2
     *      -a(8*k+877)*c3-a(8*k+878)*s3-a(8*k+879)*c4-a(8*k+880)*s4
      zb(6+kk)=zb(6+kk)-a(8*k+873)*c2-a(8*k+874)*s2-a(8*k+875)
     *      -a(8*k+877)*c4-a(8*k+878)*s4-a(8*k+879)*c3-a(8*k+880)*s3
      zb(7+kk)=zb(7+kk)-a(8*k+911)-a(8*k+913)*c2-a(8*k+914)*s2
     *      -a(8*k+915)*c3-a(8*k+916)*s3-a(8*k+917)*c4-a(8*k+918)*s4
      zb(8+kk)=zb(8+kk)-a(8*k+911)*c2-a(8*k+912)*s2-a(8*k+913)
     *      -a(8*k+915)*c4-a(8*k+916)*s4-a(8*k+917)*c3-a(8*k+918)*s3
      zb( 9+kk)=zb( 9+kk)-a(8*k+989)*c2-a(8*k+990)*s2-a(8*k+987)
     *      -a(8*k+991)*c3-a(8*k+992)*s3-a(8*k+993)*c4-a(8*k+994)*s4
      zb(10+kk)=zb(10+kk)-a(8*k+987)*c2-a(8*k+988)*s2-a(8*k+989)
     *      -a(8*k+991)*c4-a(8*k+992)*s4-a(8*k+993)*c3-a(8*k+994)*s3
      zb(11+kk)=zb(11+kk)-a(8*k+951)*c2-a(8*k+952)*s2-a(8*k+949)
     *      -a(8*k+953)*c3-a(8*k+954)*s3-a(8*k+955)*c4-a(8*k+956)*s4
      zb(12+kk)=zb(12+kk)-a(8*k+949)*c2-a(8*k+950)*s2-a(8*k+951)
     *      -a(8*k+953)*c4-a(8*k+954)*s4-a(8*k+955)*c3-a(8*k+956)*s3
      zb(13+kk)=zb(13+kk)-a(8*k+  1)*c2-a(8*k+  2)*s2-a(8*k- 1)
     *      -a(8*k+  3)*c3-a(8*k+  4)*s3-a(8*k+  5)*c4-a(8*k+  6)*s4
      zb(14+kk)=zb(14+kk)-a(8*k-  1)*c2-a(8*k-  0)*s2-a(8*k+ 1)
     *      -a(8*k+  3)*c4-a(8*k+  4)*s4-a(8*k+  5)*c3-a(8*k+  6)*s3
      zb(15+kk)=zb(15+kk)-a(8*k+1065)*c2-a(8*k+1066)*s2-a(8*k+1063)
     *      -a(8*k+1067)*c3-a(8*k+1068)*s3-a(8*k+1069)*c4-a(8*k+1070)*s4
      zb(16+kk)=zb(16+kk)-a(8*k+1063)*c2-a(8*k+1064)*s2-a(8*k+1065)
     *      -a(8*k+1067)*c4-a(8*k+1068)*s4-a(8*k+1069)*c3-a(8*k+1070)*s3
      return
      end
