subroutine manq(lam,eta,c,d,k,q,itype)



implicit double precision(a-h,o-z)
dimension d(1710),c(74),lam(8)
double precision lam
!---------------------------------------------------------------
!
!     subroutine manq
!     subroutine de calcul des coefficients non encore calcules
!     precedemment  nx,nxy,mx,mxy,myx,qx,rx
!     27-1-94
!---------------------------------------------------------------
do i=1,k
   if(itype.ne.5) then			
      d(38*k+i)=-(c(1)+c(9))*lam(1)*d(k+i)+c(1)*eta*(d(29*k+i)+d(2*k+i)/q) + lam(2)*c(10)*d(2*k+i)
      d(39*k+i)=d(26*k+i)
      d(40*k+i)=-lam(2)*(c(2)+c(12))*d(2*k+i)+c(2)*eta*d(34*k+i)+c(10)*lam(1)*d(k+i)
      d(41*k+i)=(c(2)*(1.-eta)+c(13))*lam(1)*d(31*k+i)+c(14)*lam(1)*d(i)
      d(42*k+i)=(c(2)*(1.-eta)+c(7))*lam(1)*d(31*k+i)+c(8)*d(30*k+i)
      d(43*k+i)=(c(2)+c(7))*lam(1)*d(34*k+i)-lam(3)*(c(2)+c(12))*d(2*k+i)+c(8)*d(33*k+i)+lam(2)*c(10)*d(1*k+i)
      d(44*k+i)=d(43*k+i)+(c(2)*(1.-eta)+c(13))*lam(1)*d(34*k+i)+ c(14)*lam(1)*d(29*k+i)
   else
      !nx
      d(38*k+i)=-c(9)*lam(1)*d(k+i)+ lam(2)*c(10)*d(2*k+i)	
      !nxy
      d(39*k+i)=d(26*k+i)
      !mx 
      d(40*k+i)=-lam(2)*c(12)*d(2*k+i)+c(10)*lam(1)*d(k+i)			
      !mxy    
      d(41*k+i)= c(13)*lam(1)*d(31*k+i)		
      !myx    
      d(42*k+i)=c(7)*lam(1)*d(31*k+i)			
      !qx   
      d(43*k+i)=c(7)*lam(1)*d(34*k+i)-lam(3)*c(12)*d(2*k+i)+lam(2)*c(10)*d(1*k+i)
      !rx
      d(44*k+i)=d(43*k+i)+c(13)*lam(1)*d(34*k+i)	
   endif
enddo


return
end
