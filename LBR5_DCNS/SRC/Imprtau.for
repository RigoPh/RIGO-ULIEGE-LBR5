	subroutine imprtau(nel,is)

	use sharedvar
	
	implicit real*8 (a-h,o-z)

	dimension temp(6)

c*********************************************************************************

	write(68,'(1x,i3,t15,a,t31,f8.3,t44,f8.3,t57,
     *	  f8.3,t73,f8.3,t86,f8.3,t99,f8.3)') nel,'/txy/',
     *	  (sf1(is)*dabs(taunet(nel,1))/1.e+06),
     *	  (sf1(is)*dabs(taunet(nel,2))/1.e+06),
     *	  (sf1(is)*dabs(taunet(nel,3))/1.e+06),
     *	  (sf1(is)*dabs(taugro(nel,1))/1.e+06),
     *	  (sf1(is)*dabs(taugro(nel,2))/1.e+06),
     *	  (sf1(is)*dabs(taugro(nel,3))/1.e+06)

      return
      end