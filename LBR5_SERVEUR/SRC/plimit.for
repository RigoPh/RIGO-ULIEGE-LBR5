      subroutine plimit(a,b,c,s,cc,ss,i,k,mt,l,n,ali)

      use param_section,A_=>A,B_=>B,ALI_=>ALI
            
      implicit double precision(a-h,o-z)

      dimension a(464),b(464),ali(2*mt_max*(8+nsolmax))
c***********************************************************************
c
c     subroutine limit   (effet des traverses)
c     ------------------
c     la subroutine calcule les 8 grandeurs utiles '(u,v,w,wø,my,ny,ry et nxy)
c     sur les bords du panneaux sous l'action des effets concentres unitaires 
c     de type xody et zody agissant au niveau des traverses.
c
c***********************************************************************
      ij=l*20+i
      kh=4*k+(n-1)*16-3
      ali(ij)=ali(ij)+b(kh)*c+b(kh+1)*s+b(kh+2)*cc+b(kh+3)*ss
      j=ij+mt
      ali(j)=ali(j)+a(kh)*c+a(kh+1)*s+a(kh+2)*cc+a(kh+3)*ss
      return
      end
