	OPTIONS /EXTEND

	subroutine  vibration_mod_input_file(contor_case)

	implicit none

	integer i1,i88,nr_val_input,nr_sections
	
!	double precision T1

	integer irad_lo,irad_la,it_bc !,ddl,err

	double precision dlong,dlarg,dthick_plate  ! dimensions of the plate (shell)
	double precision drho,dE,dnu               ! mechanical characteristics

	double precision, allocatable :: dr_lo(:),dr_la(:)            ! position of the stiffeners
	integer, allocatable :: itype_lo(:),itype_la(:)     ! type of the stiffeners
	character, allocatable :: csect_lo(:),csect_la(:)   ! section type of the stiffeners

	integer, dimension (30) :: itype_sect
	double precision, dimension (30) :: dl2,dep2,dl3,dep3         ! dimensions of the webs and flanges

	integer ctor, contor_case
	
	integer irad_lo_1
	double precision dlong_1
	double precision, allocatable :: dr_lo_1(:)
	integer, allocatable :: itype_lo_1(:)
	character, allocatable :: csect_lo_1(:)
	
	integer irad_lo_2
	double precision dlong_2
	double precision, allocatable :: dr_lo_2(:)
	integer, allocatable :: itype_lo_2(:)
	character, allocatable :: csect_lo_2(:)

	integer irad_la_2
	double precision dlarg_2
	double precision, allocatable :: dr_la_2(:)
	integer, allocatable :: itype_la_2(:)
	character, allocatable :: csect_la_2(:)

	integer irad_la_1
	double precision dlarg_1
	double precision, allocatable :: dr_la_1(:)
	integer, allocatable :: itype_la_1(:)
	character, allocatable :: csect_la_1(:)
	
*------------------------------------------------------------------------------
	
!	contor_case=5


  300 FORMAT(D12.5,D12.5,D12.5,I,I,D12.5,D12.5,D12.5,I)
  400 FORMAT(I,D,D,D,D)
	
	print *,'Read file input_data_full_dp_temp.dat'

	nr_val_input=1000

	open(910,file='input_data_full_dp_temp.dat')
	
	i88=0

	do i88=1,1000

		if (i88.eq.1) then
			read(910,*,end=91) dlong,dlarg,dthick_plate,irad_lo,irad_la,drho,dE,
     *                          dnu,it_bc

	ALLOCATE(dr_lo(irad_lo))
	ALLOCATE(dr_la(irad_la))

	ALLOCATE(itype_lo(irad_lo))
	ALLOCATE(itype_la(irad_la))

	ALLOCATE(csect_lo(irad_lo))
	ALLOCATE(csect_la(irad_la))
		
		elseif ((i88.ge.2).and.(i88.le.(irad_lo+1))) then
			read(910,*,end=91) dr_lo(i88-1),itype_lo(i88-1),csect_lo(i88-1)
		
		elseif ((i88.ge.(irad_lo+2)).and.(i88.le.(irad_lo+1+irad_la))) then
			read(910,*,end=91) dr_la(i88-irad_lo-1),itype_la(i88-irad_lo-1),
     *                          csect_la(i88-irad_lo-1)
		
		elseif (i88.ge.(irad_lo+irad_la+2)) then
			read(910,*,end=91) itype_sect(i88-irad_lo-irad_la-1),
     &								  dl2(i88-irad_lo-irad_la-1),
     &								 dep2(i88-irad_lo-irad_la-1),
     &								  dl3(i88-irad_lo-irad_la-1),
     &								 dep3(i88-irad_lo-irad_la-1)
		
		endif

	enddo
   91 nr_val_input=i88-1
   	close(910)
	
	nr_sections=nr_val_input-1-irad_lo-irad_la

*------------------------------------------------------------------------------
	
	if (contor_case.eq.1) then


******1111111111111111111111111111111111111111111111111111111111111111111111111111111111

		print *,'Case 1 - symmetry axis at left'

	dlong_1=2.*dlong
	
	if (dr_lo(1).le.0.01D+00) then
		irad_lo_1=2*irad_lo-1
*		print *,'A:',irad_lo_1
	else
		irad_lo_1=2*irad_lo
*		print *,'B:',irad_lo_1
	endif
	
	ALLOCATE(dr_lo_1(irad_lo_1))
	ALLOCATE(itype_lo_1(irad_lo_1))
	ALLOCATE(csect_lo_1(irad_lo_1))
	
	i1=0
	ctor=0
	do i1=1,irad_lo-1
		dr_lo_1(i1)=dlong-dr_lo(irad_lo-i1+1)
		itype_lo_1(i1)=itype_lo(irad_lo-i1+1)
		csect_lo_1(i1)=csect_lo(irad_lo-i1+1)

		ctor=ctor+1
	enddo

	if (dr_lo(1).le.0.01D+00) then
		dr_lo_1(ctor+1)=dlong
		itype_lo_1(ctor+1)=itype_lo(1)
		csect_lo_1(ctor+1)=csect_lo(1)

		ctor=ctor+2

	else
		dr_lo_1(ctor+1)=dlong-dr_lo(1)
		itype_lo_1(ctor+1)=itype_lo(1)
		csect_lo_1(ctor+1)=csect_lo(1)

		dr_lo_1(ctor+2)=dlong+dr_lo(1)
		itype_lo_1(ctor+2)=itype_lo(1)
		csect_lo_1(ctor+2)=csect_lo(1)

		ctor=ctor+3
	endif
	
	i1=0
	do i1=1,irad_lo-1
		dr_lo_1(ctor+i1-1)=dlong+dr_lo(i1+1)
		itype_lo_1(ctor+i1-1)=itype_lo(i1+1)
		csect_lo_1(ctor+i1-1)=csect_lo(i1+1)
	enddo

	open(110,file='input_data_full_dp.dat')
		
		if (((dlong-dr_lo(irad_lo)).le.0.01D+00).and.(it_bc.eq.1)) then
			write(110,300) dlong_1,dlarg,dthick_plate,irad_lo_1-2,irad_la,drho,
     *                      dE,dnu,it_bc
		else
			write(110,300) dlong_1,dlarg,dthick_plate,irad_lo_1,irad_la,drho,dE,
     *                      dnu,it_bc
		endif
		
		if (((dlong-dr_lo(irad_lo)).le.0.01D+00).and.(it_bc.eq.1)) then
			i1=0
			do i1=1,irad_lo_1-2
				write(110,*) dr_lo_1(i1+1),itype_lo_1(i1+1),'		',csect_lo_1(i1+1)
			enddo
		elseif (((dlong-dr_lo(irad_lo)).le.0.01D+00).and.(it_bc.ne.1)) then
			dr_lo_1(1)=0.D0
			dr_lo_1(irad_lo_1)=0.D0
			dr_lo_1(1)=0.01D+00
			dr_lo_1(irad_lo_1)=dlong_1-0.01D+00

			i1=0
			do i1=1,irad_lo_1
				write(110,*) dr_lo_1(i1),itype_lo_1(i1),'		',csect_lo_1(i1)
			enddo
		elseif ((dlong-dr_lo(irad_lo)).gt.0.01D+00) then
			i1=0
			do i1=1,irad_lo_1
				write(110,*) dr_lo_1(i1),itype_lo_1(i1),'		',csect_lo_1(i1)
			enddo
		endif
		
		i1=0
			do i1=1,irad_la
				write(110,*) dr_la(i1),itype_la(i1),'		',csect_la(i1)
			enddo

		i1=0
		do i1=1,nr_sections
			write(110,400) itype_sect(i1),dl2(i1),dep2(i1),dl3(i1),dep3(i1)
		enddo

	close(110)


******2222222222222222222222222222222222222222222222222222222222222222222222222222222222222222
	
	elseif (contor_case.eq.2) then

		print *,'Case 2 - symmetry axis at right'

	dlong_2=2.*dlong
	
	if ((dlong-dr_lo(irad_lo)).le.0.01D+00) then
		irad_lo_2=2*irad_lo-1
*		print *,'A:',irad_lo_2
	else
		irad_lo_2=2*irad_lo
*		print *,'B:',irad_lo_2
	endif
	
	ALLOCATE(dr_lo_2(irad_lo_2))
	ALLOCATE(itype_lo_2(irad_lo_2))
	ALLOCATE(csect_lo_2(irad_lo_2))
	
	i1=0
	ctor=0
	do i1=1,irad_lo-1
		dr_lo_2(i1)=dr_lo(i1)
		itype_lo_2(i1)=itype_lo(i1)
		csect_lo_2(i1)=csect_lo(i1)

		ctor=ctor+1
	enddo
	
	if ((dlong-dr_lo(irad_lo)).le.0.01D+00) then
		dr_lo_2(ctor+1)=dlong
		itype_lo_2(ctor+1)=itype_lo(irad_lo)
		csect_lo_2(ctor+1)=csect_lo(irad_lo)

		ctor=ctor+2

	else
		dr_lo_2(ctor+1)=dr_lo(irad_lo)
		itype_lo_2(ctor+1)=itype_lo(irad_lo)
		csect_lo_2(ctor+1)=csect_lo(irad_lo)

		dr_lo_2(ctor+2)=2.*dlong-dr_lo(irad_lo)
		itype_lo_2(ctor+2)=itype_lo(irad_lo)
		csect_lo_2(ctor+2)=csect_lo(irad_lo)

		ctor=ctor+3
	endif
	
	i1=0
	do i1=1,irad_lo-1
		dr_lo_2(ctor+i1-1)=2.*dlong-dr_lo(irad_lo-i1)
		itype_lo_2(ctor+i1-1)=itype_lo(irad_lo-i1)
		csect_lo_2(ctor+i1-1)=csect_lo(irad_lo-i1)
	enddo

	open(120,file='input_data_full_dp.dat')

		if ((dr_lo(1).le.0.01D+00).and.(it_bc.eq.1)) then
			write(120,300) dlong_2,dlarg,dthick_plate,irad_lo_2-2,irad_la,drho,
     *                      dE,dnu,it_bc
		else
			write(120,300) dlong_2,dlarg,dthick_plate,irad_lo_2,irad_la,drho,dE,
     *                      dnu,it_bc
		endif

		if ((dr_lo(1).le.0.01D+00).and.(it_bc.eq.1)) then
			i1=0
			do i1=1,irad_lo_2-2
				write(120,*) dr_lo_2(i1+1),itype_lo_2(i1+1),'		',csect_lo_2(i1+1)
			enddo
		elseif ((dr_lo(1).le.0.01D+00).and.(it_bc.ne.1)) then
			dr_lo_2(1)=0.D0
			dr_lo_2(irad_lo_2)=0.D0
			dr_lo_2(1)=0.01D+00
			dr_lo_2(irad_lo_2)=dlong_2-0.01D+00

			i1=0
			do i1=1,irad_lo_2
				write(120,*) dr_lo_2(i1),itype_lo_2(i1),'		',csect_lo_2(i1)
			enddo
		elseif (dr_lo(1).gt.0.01D+00) then
			i1=0
			do i1=1,irad_lo_2
				write(120,*) dr_lo_2(i1),itype_lo_2(i1),'		',csect_lo_2(i1)
			enddo
		endif

		i1=0
		do i1=1,irad_la
			write(120,*) dr_la(i1),itype_la(i1),'		',csect_la(i1)
		enddo

		i1=0
		do i1=1,nr_sections
			write(120,400) itype_sect(i1),dl2(i1),dep2(i1),dl3(i1),dep3(i1)
		enddo

	close(120)


******333333333333333333333333333333333333333333333333333333333333333333333333333333333333
	
	elseif (contor_case.eq.3) then

		print *,'Case 3 - symmetry axis in front'

	dlarg_2=2.*dlarg
	
	if ((dlarg-dr_la(irad_la)).le.0.01D+00) then
		irad_la_2=2*irad_la-1
*		print *,'A:',irad_la_2
	else
		irad_la_2=2*irad_la
*		print *,'B:',irad_la_2
	endif
	
	ALLOCATE(dr_la_2(irad_la_2))
	ALLOCATE(itype_la_2(irad_la_2))
	ALLOCATE(csect_la_2(irad_la_2))
	
	i1=0
	ctor=0
	do i1=1,irad_la-1
		dr_la_2(i1)=dr_la(i1)
		itype_la_2(i1)=itype_la(i1)
		csect_la_2(i1)=csect_la(i1)

		ctor=ctor+1
	enddo

	if ((dlarg-dr_la(irad_la)).le.0.01D+00) then
		dr_la_2(ctor+1)=dlarg
		itype_la_2(ctor+1)=itype_la(irad_la)
		csect_la_2(ctor+1)=csect_la(irad_la)

		ctor=ctor+2

	else
		dr_la_2(ctor+1)=dr_la(irad_la)
		itype_la_2(ctor+1)=itype_la(irad_la)
		csect_la_2(ctor+1)=csect_la(irad_la)

		dr_la_2(ctor+2)=2.*dlarg-dr_la(irad_la)
		itype_la_2(ctor+2)=itype_la(irad_la)
		csect_la_2(ctor+2)=csect_la(irad_la)

		ctor=ctor+3
	endif
	
	i1=0
	do i1=1,irad_la-1
		dr_la_2(ctor+i1-1)=2.*dlarg-dr_la(irad_la-i1)
		itype_la_2(ctor+i1-1)=itype_la(irad_la-i1)
		csect_la_2(ctor+i1-1)=csect_la(irad_la-i1)
	enddo

	open(130,file='input_data_full_dp.dat')
		
		if ((dr_la(1).le.0.01D+00).and.(it_bc.eq.1)) then
			write(130,300) dlong,dlarg_2,dthick_plate,irad_lo,irad_la_2-2,drho,
     *                      dE,dnu,it_bc
		else
			write(130,300) dlong,dlarg_2,dthick_plate,irad_lo,irad_la_2,drho,dE,
     *                      dnu,it_bc
		endif

		i1=0
		do i1=1,irad_lo
			write(130,*) dr_lo(i1),itype_lo(i1),'		',csect_lo(i1)
		enddo

		if ((dr_la(1).le.0.01D+00).and.(it_bc.eq.1)) then
			i1=0
			do i1=1,irad_la_2-2
				write(130,*) dr_la_2(i1+1),itype_la_2(i1+1),'		',csect_la_2(i1+1)
			enddo
		elseif ((dr_la(1).le.0.01D+00).and.(it_bc.ne.1)) then
			dr_la_2(1)=0.D0
			dr_la_2(irad_la_2)=0.D0
			dr_la_2(1)=0.01D+00
			dr_la_2(irad_la_2)=dlarg_2-0.01D+00

			i1=0
			do i1=1,irad_la_2
				write(130,*) dr_la_2(i1),itype_la_2(i1),'		',csect_la_2(i1)
			enddo
		elseif (dr_la(1).gt.0.01D+00) then
			i1=0
			do i1=1,irad_la_2
				write(130,*) dr_la_2(i1),itype_la_2(i1),'		',csect_la_2(i1)
			enddo
		endif

		i1=0
		do i1=1,nr_sections
			write(130,400) itype_sect(i1),dl2(i1),dep2(i1),dl3(i1),dep3(i1)
		enddo

	close(130)


******4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444
	
	elseif (contor_case.eq.4) then

		print *,'Case 4 - symmetry axis at back'

	dlarg_1=2.*dlarg
	
	if (dr_la(1).le.0.01D+00) then
		irad_la_1=2*irad_la-1
*		print *,'A:',irad_la_1
	else
		irad_la_1=2*irad_la
*		print *,'B:',irad_la_1
	endif
	
	ALLOCATE(dr_la_1(irad_la_1))
	ALLOCATE(itype_la_1(irad_la_1))
	ALLOCATE(csect_la_1(irad_la_1))
	
	i1=0
	ctor=0
	do i1=1,irad_la-1
		dr_la_1(i1)=dlarg-dr_la(irad_la-i1+1)
		itype_la_1(i1)=itype_la(irad_la-i1+1)
		csect_la_1(i1)=csect_la(irad_la-i1+1)

		ctor=ctor+1
	enddo

	if (dr_la(1).le.0.01D+00) then
		dr_la_1(ctor+1)=dlarg
		itype_la_1(ctor+1)=itype_la(1)
		csect_la_1(ctor+1)=csect_la(1)

		ctor=ctor+2

	else
		dr_la_1(ctor+1)=dlarg-dr_la(1)
		itype_la_1(ctor+1)=itype_la(1)
		csect_la_1(ctor+1)=csect_la(1)

		dr_la_1(ctor+2)=dlarg+dr_la(1)
		itype_la_1(ctor+2)=itype_la(1)
		csect_la_1(ctor+2)=csect_la(1)

		ctor=ctor+3
	endif
	
	i1=0
	do i1=1,irad_la-1
		dr_la_1(ctor+i1-1)=dlarg+dr_la(i1+1)
		itype_la_1(ctor+i1-1)=itype_la(i1+1)
		csect_la_1(ctor+i1-1)=csect_la(i1+1)
	enddo

	open(140,file='input_data_full_dp.dat')
	
		if (((dlarg-dr_la(irad_la)).le.0.01D+00).and.(it_bc.eq.1)) then
			write(140,300) dlong,dlarg_1,dthick_plate,irad_lo,irad_la_1-2,drho,
     *                      dE,dnu,it_bc
		else
			write(140,300) dlong,dlarg_1,dthick_plate,irad_lo,irad_la_1,drho,dE,
     *                      dnu,it_bc
		endif
		
		i1=0
		do i1=1,irad_lo
			write(140,*) dr_lo(i1),itype_lo(i1),'		',csect_lo(i1)
		enddo
		
		if (((dlarg-dr_la(irad_la)).le.0.01D+00).and.(it_bc.eq.1)) then
			i1=0
			do i1=1,irad_la_1-2
				write(140,*) dr_la_1(i1+1),itype_la_1(i1+1),'		',csect_la_1(i1+1)
			enddo
		elseif (((dlarg-dr_la(irad_la)).le.0.01D+00).and.(it_bc.ne.1)) then
			dr_la_1(1)=0.D0
			dr_la_1(irad_la_1)=0.D0
			dr_la_1(1)=0.01D+00
			dr_la_1(irad_la_1)=dlarg_1-0.01D+00

			i1=0
			do i1=1,irad_la_1
				write(140,*) dr_la_1(i1),itype_la_1(i1),'		',csect_la_1(i1)
			enddo
		elseif ((dlarg-dr_la(irad_la)).gt.0.01D+00) then
			i1=0
			do i1=1,irad_la_1
				write(140,*) dr_la_1(i1),itype_la_1(i1),'		',csect_la_1(i1)
			enddo
		endif

		i1=0
		do i1=1,nr_sections
			write(140,400) itype_sect(i1),dl2(i1),dep2(i1),dl3(i1),dep3(i1)
		enddo

	close(140)


******555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555

	elseif (contor_case.eq.5) then

		print *,'Case 5 - non-symmetry'


	open(150,file='input_data_full_dp.dat')
		
		write(150,300) dlong,dlarg,dthick_plate,irad_lo,irad_la,drho,dE,dnu,
     *                  it_bc

		if (dr_lo(1).lt.0.01D+00) then
			dr_lo(1)=0.01D+00
		endif

		if ((dlong-dr_lo(irad_lo)).lt.0.01D+00) then
			dr_lo(irad_lo)=dlong-0.01D+00
		endif
		
		if (dr_la(1).lt.0.01D+00) then
			dr_la(1)=0.01D+00
		endif
		
		if ((dlarg-dr_la(irad_la)).lt.0.01D+00) then
			dr_la(irad_la)=dlarg-0.01D+00
		endif

		i1=0
		do i1=1,irad_lo
			write(150,*) dr_lo(i1),itype_lo(i1),'		',csect_lo(i1)
		enddo

		i1=0
		do i1=1,irad_la
				write(150,*) dr_la(i1),itype_la(i1),'		',csect_la(i1)
		enddo

		i1=0
		do i1=1,nr_sections
			write(150,400) itype_sect(i1),dl2(i1),dep2(i1),dl3(i1),dep3(i1)
		enddo

	close(150)

******
	endif

	return
	end
	!endprogram generation_files_LBR5
