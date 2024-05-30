subroutine select(a,b,d,e)     



implicit double precision (a-h,o-z)

dimension d(9),e(9)	
!     a,b  inputs
!     c    output avec c la valeur de maximun de a et b mais avec le signe
!     ex: a=-6 et b=5, d'où a= -6 car abs(a) > abs(b)

c=dmin1(a,b)
a=dmax1(a,b)
if (b.gt.a) then	
   do i=1,9				
      d(i)=e(i)			
   enddo						
endif							

if(dabs(a).le.dabs(c)) then		
   a=c						
   if (b.lt.a) then	
      do i=1,9				
         d(i)=e(i)			
      enddo						
   endif							
endif							

return
end

