      subroutine pimpr(d,ms,m)

      use param_section	

      implicit double precision(a-h,o-z)
      dimension d(1710)
c***********************************************************************
c
c    impression des coefficients contenus
c        dans dish ( charge hydraulique )
c        dans disl ( forces de bord     )
c        dans dism ( moments de bord    )
c
c***********************************************************************
      do 1 i=1,(3+ms)
          j2=(i-1)*38+1
 1    write(iu_11(iboat),3)(d(j),j=j2,(j2+5+8*m))
      do 2   i=24,45
          j2=(i-1)*38+1
 2    write(iu_11(iboat),3)(d(j),j=j2,(j2+5+8*m))
 3    format(6(2x,e14.7)/4(8(2x,e14.7)/))
      return
      end
