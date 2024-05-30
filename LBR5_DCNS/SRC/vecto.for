      subroutine vecto(a,c,s,i,k,mt,l,sy,cc,ss,zb)

      implicit real *8(a-h,o-z)
      dimension a(464),zb(2*mt,17)

c***********************************************************************
c                                                                       
c     subroutine vector (effet des traverses)
c     ------------------------------------------
c     subroutine de creation du vecteur zb representant les termes
c     independants du systeme de calcul des inconnues hyperstatiques
c     des traverses (xo et zo).
c
c     zb correspond aux 8 cas de charge agissant sur les
c     bords du panneau (c.à.d disa, disb, disc et disd en y=0 et y=phil) 
c                                                                       
c     modifié: le 21-6-95 (ph. rigo)                        créer : thèse        
c***********************************************************************
c  avec i nø de la traverse
c       l nø du cas de sollicitation (2 à 9)

      kh=4*k+45+32*(i-1)
      zb(i,l)=zb(i,l)+(a(kh)*c+a(kh+1)*s+a(kh+2)*cc+a(kh+3)*ss)*sy
      j=i+mt
      zb(j,l)=zb(j,l)+(a(kh+16)*c+a(kh+17)*s+a(kh+18)*cc+a(kh+19)*ss)*sy
      return
      end
