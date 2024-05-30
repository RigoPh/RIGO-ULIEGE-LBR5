	!**********************************************
!*   Demonstration program of Bubble sorting  * 
!*        (about n*n comparisons used).       *
!* ------------------------------------------ *
!* Reference: "A book on C By Al Kelley and   *
!* Ira Pohl, The Benjamin/Cummings Publishing *
!* Company, Inc, 1984" [BIBLI 09].            *
!*                                            *
!*                F90 version by J-P Moreau.  *
!* ------------------------------------------ *
!* SAMPLE RUN:                                *
!*                                            *
!* Initial table A:                           *
!* 7  3  66  3  -5  22  -77  2  36  -12       *
!*                                            *
!* Sorted table A:                            *
!* -77  -12  -5  2  3  3  7  22  36  66       *
!*                                            *
!**********************************************


!return p,q in ascending order
	Subroutine Order(p,q)
	double precision p,q,temp
		if (p>q) then
		temp=p
		p=q
		q=temp
	end if
	return
	end

!Buuble sorting of integer array A
	Subroutine Bubble(A, n)
	double precision A(1:n)
	do i=1, n
		do j=n, i+1, -1
			call Order(A(j-1), A(j))
			end do
		end do
	return
	end

!end of file bubble.f90


