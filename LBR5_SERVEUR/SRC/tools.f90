subroutine annuli(mn,nl)
implicit double precision (a-h,o-z)
integer*4 mn(nl)

!do i=1,nl
!   mn(i)=0
!enddo

mn(1:nl)=0

return
end



subroutine annuld(s,n)
implicit double precision (a-h,o-z)
double precision s(n)

!do i=1,n
!   s(i)=0.d00
!enddo

s(1:n)=0.d00

return
end




double precision function expo(al)
!=======================================================================
! "expo" calcule la valeur de " exp(-al) "
!        et verifie si on ne cree pas un "underflow"
!
!     créer : 1992 (thèse), ph. rigo
!=======================================================================

implicit double precision(a-h,o-z)
if(al.ge.150.) then
   expo=0.
else
   expo=dexp(-al)
endif

return
end






subroutine lamsr(lamv,ijk,pi,width)
implicit double precision(a-h,o-z)
double precision lamv
dimension lamv(8)

lamv(1)=ijk*pi/width
lamv(2)=lamv(1)*lamv(1)
lamv(3)=lamv(2)*lamv(1)
lamv(4)=lamv(2)*lamv(2)
lamv(5)=lamv(3)*lamv(2)
lamv(6)=lamv(3)*lamv(3)
lamv(7)=lamv(4)*lamv(3)
lamv(8)=lamv(4)*lamv(4)


return                                                            
end
