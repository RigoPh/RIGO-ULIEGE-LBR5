subroutine vecto(a,c,s,i,k,mt,l,sy,cc,ss,zb,nsolmax)

implicit double precision(a-h,o-z)
dimension a(464),zb(2*mt,(8+nsolmax))

!------------------------------------------------------------------------
!                                                                       
!     subroutine vector (effet des traverses)
!     ------------------------------------------
!     subroutine de creation du vecteur zb representant les termes
!     independants du systeme de calcul des inconnues hyperstatiques
!     des traverses (xo et zo).
!
!     zb correspond aux 8 cas de charge agissant sur les
!     bords du panneau (c.�.d disa, disb, disc et disd en y=0 et y=phil) 
!                                                                       
!     modifi�: le 21-6-95 (ph. rigo)                        cr�er : th�se        
!------------------------------------------------------------------------
!  avec i n� de la traverse
!       l n� du cas de sollicitation (2 � 9)



kh=4*k+45+32*(i-1)
zb(i,l)=zb(i,l)+(a(kh)*c+a(kh+1)*s+a(kh+2)*cc+a(kh+3)*ss)*sy
j=i+mt
zb(j,l)=zb(j,l)+(a(kh+16)*c+a(kh+17)*s+a(kh+18)*cc+a(kh+19)*ss)*sy


return
end
