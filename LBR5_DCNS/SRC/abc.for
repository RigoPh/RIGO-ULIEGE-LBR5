
      subroutine annuli(mn,nl)
      implicit real*8 (a-h,o-z)
      dimension mn(nl)
      do i=1,nl
       mn(i)=0
      enddo
      return
      end



      subroutine annuld(s,n)
      implicit real*8 (a-h,o-z)
      dimension s(n)
      do i=1,n
       s(i)=0.d00
      enddo
      return
      end




      real*8 function expo(al)
c=======================================================================
c "expo" calcule la valeur de " exp(-al) "
c        et verifie si on ne cree pas un "underflow"
c
c     créer : 1992 (thèse), ph. rigo
c=======================================================================

      implicit real*8(a-h,o-z)
      if(al.ge.150.) then
        expo=0.
      else
        expo=dexp(-al)
      endif

      return
      end
