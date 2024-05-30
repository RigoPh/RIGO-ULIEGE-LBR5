	OPTIONS /EXTEND_SOURCE
	
*----------------------------------------------------------------------------------------

!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\src\dgetrf.f'
!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\src\dgetri.f'
!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\src\dgeev.f'
!
!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\BLAS\src\dgemm.f'
!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\BLAS\src\dtrsm.f'
!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\BLAS\src\xerbla.f'
!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\BLAS\src\lsame.f'
!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\BLAS\src\idamax.f'
!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\BLAS\src\dswap.f'
!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\BLAS\src\dscal.f'
!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\BLAS\src\dger.f'
!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\BLAS\src\dgemv.f'
!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\BLAS\src\dtrmm.f'
!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\BLAS\src\dtrmv.f'
!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\BLAS\src\daxpy.f'
!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\BLAS\src\dnrm2.f'
!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\BLAS\src\drot.f'
!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\BLAS\src\dcopy.f'
!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\BLAS\src\ddot.f'
!
!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\Install\dlamch.f'

*----------------------------------------------------------------------------------------

!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\src\dlaswp.f'
!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\src\dgetf2.f'
!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\src\ilaenv.f'
!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\src\ieeeck.f'
!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\src\iparmq.f'
!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\src\dtrtri.f'
!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\src\dtrti2.f'	
!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\src\dgebal.f'
!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\src\dlabad.f'
!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\src\dhseqr.f'
!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\src\dlange.f'
!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\src\dlascl.f'
!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\src\dgehrd.f'
!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\src\dlacpy.f'
!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\src\dorghr.f'
!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\src\dtrevc.f'
!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\src\dgebak.f'
!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\src\dlapy2.f'
!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\src\dlartg.f'
!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\src\dlaqr0.f'
!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\src\dlaset.f'
!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\src\dlahqr.f'
!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\src\dlassq.f'
!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\src\dlahr2.f'
!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\src\dlarfb.f'
!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\src\dgehd2.f'
!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\src\dorgqr.f'
!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\src\dlaln2.f'
!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\src\dlaqr3.f'
!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\src\dlaqr4.f'
!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\src\dlaqr5.f'
!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\src\dlanv2.f'
!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\src\dlarfg.f'
!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\src\dorg2r.f'
!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\src\dlarft.f'
!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\src\dladiv.f'
!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\src\dtrexc.f'
!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\src\dlaqr2.f'
!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\src\dlaqr1.f'
!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\src\dlarf.f'
!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\src\dlaexc.f'
!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\src\dlasy2.f'
!	INCLUDE 'C:\Program Files\University Of Tennessee\LAPACK 3.1.1\src\dlarfx.f'
	
*----------------------------------------------------------------------------------------
!	INCLUDE 'F:\Work\Fortran_vibrations\test1\sorting_dp.for'


*	INCLUDE 'F:\Work\Fortran_vibrations\test1\determinant.for'
*	INCLUDE 'F:\Work\Fortran_vibrations\test1\inverse_matrix.for'
*	INCLUDE 'F:\Work\Fortran_vibrations\test1\determinant_LU.for'
	
*	INCLUDE 'F:\Work\Fortran_vibrations\test1\determinant_Lapack_1.for'
*	INCLUDE 'F:\Work\Fortran_vibrations\test1\determinant_Lapack_2.for'

*	INCLUDE 'F:\Work\Fortran_vibrations\test1\determinant_analytic.for'
	
*******************************************************************************

	subroutine vibration_module_v5(long,larg,thick_plate,rad_lo2,rad_la2,
     *                              rho1,E1,niu1,t_bc,
     &                               p_lo2,type_lo2,sect_lo2,
     &							   p_la2,type_la2,sect_la2,
     &							   type_sect,l2,ep2,l3,ep3,
     &							   Deter,ddl,err)

*******!DEC$ ATTRIBUTES DLLEXPORT:: vibration_module

	implicit none
	
	double precision ::  long,larg,thick_plate
	double precision ::  rho1,E1,niu1
	
	integer ::  t_bc
	integer ::  rad_lo2,rad_la2
	
	double precision, dimension(rad_lo2) ::  p_lo2
	double precision, dimension(rad_la2) ::  p_la2

	integer, dimension(rad_lo2) ::  type_lo2
	integer, dimension(rad_la2) ::  type_la2

	character, dimension(rad_lo2) ::  sect_lo2
	character, dimension(rad_la2) ::  sect_la2

	integer, dimension (30) :: type_sect
	double precision, dimension (30) :: l2,ep2,l3,ep3
	
	double precision :: Deter
	
	integer ::  err
*-----

	double precision, allocatable ::  p_lo(:),p_la(:)
	integer, allocatable :: type_lo(:),type_la(:)
	character, allocatable :: sect_lo(:),sect_la(:)

	integer rad_lo,rad_la,dim_type_sect,t_shell

*     nn_val - nombre des noeuds
*     np_val - nombre des poutres
*	rho - masse volumique
*	niu - coeff.Poisson
*	Iz - moment d'inertie
*	Iy - moment d'inertie
*	S - surface
*	J1 - torsion
*	G1 - moment torsion
	
*******************************************************************************
*                   Declaration variables programme principal                 *
*******************************************************************************

* integer
	
!	integer ID3, ICODE3

	integer LWORK
	integer INFO1, INFO2, INFO3, INFO4, INFO6
	
	integer ddl, ddl_maxg, ddl_maxn !, cddl2
!	integer noeds_free, nvn_val

	integer i, j, i1, i2, j2, i3, j3, i4, j4, i5, j5 !, i6, j6
	integer j8, i9, j9, i11, j11
	integer i44, i55, i56, i66, i67
	integer i70, i71
	integer nn_val, np_val, b_dl, nr_dl,nr_dl_i,nr_dl_j

	integer contor_poz, contor_ddl, contor_poz_ddl
	integer c_beams, c_beams2, c_beams3,c_beams6
	
	integer, allocatable :: IPIV1(:),IPIV2(:)

	integer, allocatable :: n_1(:),n_2(:),l_1(:),l_2(:),poz(:),poz_ddl(:)
	integer, allocatable :: t_l(:,:)

	integer sv0,sv1,sv2,sv3,sv4,sv5,sv6,sv7,sv8,sv9,sv10 !,sv11

* real

	double precision Pi,T1,G1 !,val_freq,val_omega

	double precision theta_calcul,phi_calcul
	double precision theta_deg,phi_deg

	double precision coef1,coef2,coef3,coef4,coef5,coef6
	
	double precision, dimension (12,12) :: K3,M3,MChrep,IMChrep
	double precision, dimension (12,12) :: GM_K,GM_M,GM_K1,GM_M1

	double precision, allocatable :: WORK2(:)
	double precision, allocatable :: WORK4(:)
	double precision, allocatable :: WORK6(:)

	double precision, allocatable :: x_p(:),y_p(:),z_p(:)
	
	double precision, allocatable :: lung(:,:)
	double precision, allocatable :: theta1(:,:),phi1(:,:)
	double precision, allocatable :: SIN_t(:,:),SIN_p(:,:),COS_t(:,:),
     *                                  COS_p(:,:)

	double precision, allocatable :: S_1(:),J_1(:),Iy_1(:),Iz_1(:)
	double precision, allocatable :: S1(:,:),J1(:,:),Iz1(:,:),Iy1(:,:)

	double precision, allocatable :: K_TEM(:,:,:),M_TEM(:,:,:) !**
	double precision, allocatable :: MKpN(:,:),MMpN(:,:)
	double precision, allocatable :: GM_K_FT(:,:),GM_M_FT(:,:)
	double precision, allocatable :: GM_K_FF(:,:)
!	double precision, allocatable :: GM_M_FF2(:,:)

	double precision, allocatable :: GM_M_FF(:,:)
	double precision, allocatable :: IMg_Kg(:,:)
	double precision,allocatable :: IMg_Kg2(:,:)
	double precision, allocatable :: KE(:,:,:),ME(:,:,:)

	integer, allocatable :: c_beams5(:,:)

!	double precision, allocatable :: val_det(:)
	
	double precision, allocatable :: WR1(:),WI1(:)
	double precision, allocatable :: VL1(:,:),VR1(:,:)

!	character(1) :: JOBZ, UPLO

*******************************************************************************
*                 Declaration variables programme secondaire                  *
*******************************************************************************

* integer

	integer j22,i100
	integer     j23
	integer k_1,k_2,k_3
	integer contor_noeuds,contor_poutres
	integer nr_noeuds,nr_poutres
	integer rad_lo_to,rad_la_to
	
* real

	double precision S_lat,zc1_lat,yc1_lat,S_lat_total
	double precision zc_lat,yc_lat
	
	double precision S_lon,zc1_lon,yc1_lon,S_lon_total
	double precision zc_lon,yc_lon

!	double precision yc_str

	double precision, allocatable :: S2_lat(:),zc2_lat(:),yc2_lat(:),
     *                                  S2_zc2_lat(:),S2_yc2_lat(:)
	double precision, allocatable :: S3_lat(:),zc3_lat(:),yc3_lat(:),
     *                                  S3_zc3_lat(:),S3_yc3_lat(:)

	double precision, allocatable :: S2_lon(:),zc2_lon(:),yc2_lon(:),
     *                                  S2_zc2_lon(:),S2_yc2_lon(:)
	double precision, allocatable :: S3_lon(:),zc3_lon(:),yc3_lon(:),
     *                                  S3_zc3_lon(:),S3_yc3_lon(:)

	double precision, allocatable :: l_la(:),l_lo(:)

	double precision, allocatable :: S1_la_l(:),S2_la_l(:),S3_la_l(:)
	double precision, allocatable :: zc1_la_l(:),zc2_la_l(:),zc3_la_l(:)
	double precision, allocatable :: yc1_la_l(:),yc2_la_l(:),yc3_la_l(:)
	double precision, allocatable :: S1_zc1_la_l(:),S2_zc2_la_l(:),
     *                                     S3_zc3_la_l(:)
	double precision, allocatable :: S1_yc1_la_l(:),S2_yc2_la_l(:),
     *                                      S3_yc3_la_l(:)

	double precision, allocatable :: S1_lo_l(:),S2_lo_l(:),S3_lo_l(:)
	double precision, allocatable :: zc1_lo_l(:),zc2_lo_l(:),zc3_lo_l(:)
	double precision, allocatable :: yc1_lo_l(:),yc2_lo_l(:),yc3_lo_l(:)
	double precision, allocatable :: S1_zc1_lo_l(:),S2_zc2_lo_l(:),
     *                                  S3_zc3_lo_l(:)
	double precision, allocatable :: S1_yc1_lo_l(:),S2_yc2_lo_l(:),
     *                                  S3_yc3_lo_l(:)

	double precision, allocatable :: zc_la_r(:),yc_la_r(:),zc_lo_r(:),
     *                                  yc_lo_r(:)

	double precision, allocatable :: Iz_la_1(:),Iz_la_2(:),Iz_la_3(:)
	double precision, allocatable :: Iy_la_1(:),Iy_la_2(:),Iy_la_3(:)
	double precision, allocatable :: Izc_la(:),Iyc_la(:)

	double precision, allocatable :: Iz_lo_1(:),Iz_lo_2(:),Iz_lo_3(:)
	double precision, allocatable :: Iy_lo_1(:),Iy_lo_2(:),Iy_lo_3(:)
	double precision, allocatable :: Izc_lo(:),Iyc_lo(:)

	double precision, allocatable :: dz_la_1(:),dz_la_2(:),dz_la_3(:)
	double precision, allocatable :: dy_la_1(:),dy_la_2(:),dy_la_3(:)

	double precision, allocatable :: dz_lo_1(:),dz_lo_2(:),dz_lo_3(:)
      double precision, allocatable :: dy_lo_1(:),dy_lo_2(:),dy_lo_3(:)

	double precision, allocatable :: Iz_la(:,:),Iy_la(:,:),S_la(:,:),
     *  J_la(:,:)
	double precision, allocatable :: Iz_lo(:,:),Iy_lo(:,:),S_lo(:,:),
     *  J_lo(:,:)
	
* character


*******************************************************************************
*                 Declaration variables programme secondaire                  *
*******************************************************************************	

*integer

	integer ddl_max_initial
	integer c_big_panel,c_big_panel_2

	integer rad_lo7, rad_la7
	integer test1_rad_lo2,test1_rad_la2,test2_rad_lo2,test2_rad_la2
	
	double precision long7,larg7
	
	integer, allocatable :: type_lo7(:),type_la7(:)
	double precision, allocatable :: p_lo7(:), p_la7(:)
	character, allocatable :: sect_lo7(:), sect_la7(:)

******
	
	double precision restr1,restr3,restr4
	double precision limit1,limit3,limit4

	double precision rapport_long_per_larg,rapport_larg_per_long

*******************************************************************************
*                 Fin declaration all variables du programme                  *
*******************************************************************************
	
*******************************************************************************
*                 Verification INPUT DATA
	
	err=0
*-----
	rapport_long_per_larg=long/larg
	rapport_larg_per_long=larg/long

	if ((rapport_long_per_larg.gt.2.5D0).or.
     *      (rapport_larg_per_long.gt.2.5D0)) then
		err=-1
		print *,'Panel too long - the first eigenfrequancy can be inexact'
		print *,'To test by FE analysis'
	endif
*-----
	if ((long.le.0.d0).or.(larg.le.0.d0).or.(thick_plate.le.0.d0)) then
		err=1
		goto 8888
	elseif((rad_lo2.le.0).or.(rad_la2.le.0).or.(t_bc.le.0)) then
		err=1
		goto 8888
	elseif ((rho1.le.0.d0).or.(E1.le.0).or.(niu1.le.0.d0)) then
		err=1
		goto 8888
	endif
*-----
	
	i100=0
	do i100=1,rad_lo2
		if (type_lo2(i100).le.0) then
			err=2
			goto 8888
		endif
		
		if (p_lo2(i100).le.0.d0) then
			if (rad_lo2.eq.1) then
				err=0
			else
				err=2
				goto 8888
			endif
		endif

		if ((sect_lo2(i100).eq.'I').or.(sect_lo2(i100).eq.'L').or.  
     *      (sect_lo2(i100).eq.'N')) then
			err=0
		else
			err=2
			goto 8888
		endif
	enddo
	
	i100=0
	do i100=1,rad_la2
		if (type_la2(i100).le.0) then
			err=2
			goto 8888
		endif
		
		if (p_la2(i100).le.0.d0) then
			if (rad_la2.eq.1) then
				err=0
			else
				err=2
				goto 8888
			endif
		endif

		if ((sect_la2(i100).eq.'I').or.(sect_la2(i100).eq.'L').or.
     *      (sect_la2(i100).eq.'N')) then
			err=0		
		else
			err=2
			goto 8888
		endif
	enddo

*-----
*	print *,'type_sect',type_sect
*	print *,'l2',l2
*	print *,'ep2',ep2
*	print *,'l3',l3
*	print *,'ep3',ep3

	i100=0
	do i100=1,30
		if ((type_sect(i100).lt.0).or.(l2(i100).lt.0.d0).or.
     *		(ep2(i100).lt.0.d0).or.(l3(i100).lt.0.d0).or.
     *      (ep3(i100).lt.0.d0)) then
			err=3
			goto 8888
		endif
	enddo

*                 Verification geometrical restrictions
	
	limit1=16.d0
	limit3=0.1d0
	limit4=1.0d0

	restr1=long/larg								! <= 16

*	if (rad_lo.eq.1) then
*		restrict21=0.4d0							! <= 0.4
*	else
*		restrict21=MAXVAL(l2)/(p_lo2(2)-p_lo2(1))	! <= 0.4
*	endif
*
*	if (rad_la.eq.1) then
*		restrict22=0.4d0							! <= 0.4
*	else
*		restrict22=MAXVAL(l2)/(p_la2(2)-p_la2(1))	! <= 0.4
*	endif
		
	if (rad_la.eq.1) then
		restr3=0.1d0								! <= 0.1
	else
		restr3=MAXVAL(l2)/larg						! <= 0.1
	endif

	restr4=MAXVAL(l3)/MAXVAL(l2)					! <=1.0
	
*	print *,'R 1:',restr1
*	print *,'R 21:',restr1
*	print *,'R 22:',restr22
*	print *,'R 3:',restr3
*	print *,'R 4:',restr4

	if (restr1.gt.limit1) then
		err=4
		print *,'GEOMETRICAL RESTRICTIONS - NOT VERIFIED'
	elseif (restr3.gt.limit3) then
		err=5
		print *,'GEOMETRICAL RESTRICTIONS - NOT VERIFIED'
	elseif (restr4.gt.limit4) then
		err=6
		print *,'GEOMETRICAL RESTRICTIONS - NOT VERIFIED'
*		goto 8888
	endif
*							END verifications
*******************************************************************************
	
	long7=0.d0
	long7=long

	larg7=0.d0
	larg7=larg

	rad_lo7=rad_lo2
	rad_la7=rad_la2

	ALLOCATE(p_lo7(rad_lo2))
	ALLOCATE(type_lo7(rad_lo2))
	ALLOCATE(sect_lo7(rad_lo2))
	
	ALLOCATE(p_la7(rad_la2))
	ALLOCATE(type_la7(rad_la2))
	ALLOCATE(sect_la7(rad_la2))
	
	p_lo7=0.d0
	p_lo7=p_lo2
	type_lo7=0
	type_lo7=type_lo2
	sect_lo7='0'
	sect_lo7=sect_lo2

	p_la7=0.d0
	p_la7=p_la2
	type_la7=0
	type_la7=type_la2
	sect_la7='0'
	sect_la7=sect_la2

******

	T1=secnds(0.0)
	
	Pi=0.d0
	Pi=3.141592653589793238462643d0
	
	ddl_maxn=0
	ddl_maxn=6
	
	c_big_panel=0
	c_big_panel_2=0

******

	G1=0.d0
	G1=E1/(2.d0*(1+niu1))

******

 1111	continue

*	print *,'L:',long
*	print *,'l:',larg

	t_shell=0
	t_shell=2

	dim_type_sect=0

*-----Virtual beams construction for panel without transversal stiffeners (t_shell=1)
*	
	if ((rad_lo2.eq.1.).and.(p_lo7(1).eq.0.d0).and.(type_lo7(1).eq.10).and.
     *      (sect_lo7(1).eq.'N')) then
		
*		print *,'A1'
		if (long7.le.10.d0) then
			rad_lo=0
			rad_lo=dint(long7)
		elseif (long7.gt.10.d0) then
			rad_lo=0
			rad_lo=dint(long7/1.5d0)
		endif
		t_shell=1
		
		ALLOCATE(p_lo(rad_lo))
		ALLOCATE(type_lo(rad_lo))
		ALLOCATE(sect_lo(rad_lo))
		p_lo(1:rad_lo)=0.d0
		type_lo(1:rad_lo)=0
		sect_lo(1:rad_lo)='0'

*		calcul dimension type_sect
		dim_type_sect=1
		i55=0
		do i55=1,10
			if (type_sect(i55).ne.0) then
				dim_type_sect=dim_type_sect+1
			endif
		enddo
		
	    i56=0
		do i56=1,rad_lo
			p_lo(i56)=(i56-1)*(long7/rad_lo)+(long7/rad_lo)/2.d0
			type_lo(i56)=dim_type_sect
			sect_lo(i56)='I'
		enddo
		
	else
		
*	    print *,'A2'
		rad_lo=rad_lo7

	    ALLOCATE(p_lo(rad_lo))
		ALLOCATE(type_lo(rad_lo))
		ALLOCATE(sect_lo(rad_lo))
		p_lo(1:rad_lo)=0.d0
		type_lo(1:rad_lo)=0
		sect_lo(1:rad_lo)='0'

		p_lo=p_lo7
		type_lo=type_lo7
		sect_lo=sect_lo7

	endif
*
*-----Virtual beams construction for panel without transversal stiffeners (t_shell=1)


*-----Virtual beams construction for panel without longitudinal stiffeners (t_shell=0)
*	
	if ((rad_la2.eq.1.).and.(p_la7(1).eq.0.d0).and.(type_la7(1).eq.10).and.
     *      (sect_la7(1).eq.'N')) then
	
*		print *,'B1'
		if (larg7.le.10.d0) then
			rad_la=0.d0
			rad_la=dint(larg7)
		elseif (larg7.gt.10.d0) then
			rad_la=0.d0
			rad_la=dint(larg7/1.5d0)
		endif
		t_shell=0
		
		ALLOCATE(p_la(rad_la))
		ALLOCATE(type_la(rad_la))
		ALLOCATE(sect_la(rad_la))
		p_la(1:rad_la)=0.d0
		type_la(1:rad_la)=0
		sect_la(1:rad_la)='0'

*		calcul dimension type_sect
		dim_type_sect=1
		
		i66=0
		do i66=1,10
			if (type_sect(i66).ne.0) then
				dim_type_sect=dim_type_sect+1
			endif
		enddo

*		print *,'dim_type_sect',dim_type_sect
		
		i67=0
		do i67=1,rad_la
			p_la(i67)=(i67-1)*(larg7/rad_la)+(larg7/rad_la)/2.d0
			type_la(i67)=dim_type_sect
			sect_la(i67)='I'
		enddo
		
	else
	
*	    print *,'B2'
		rad_la=rad_la7
	
	    ALLOCATE(p_la(rad_la))
		ALLOCATE(type_la(rad_la))
		ALLOCATE(sect_la(rad_la))
		p_la(1:rad_la)=0.d0
		type_la(1:rad_la)=0
		sect_la(1:rad_la)='0'

		p_la=p_la7
		type_la=type_la7
		sect_la=sect_la7

	endif
	
*
*-----Virtual beams construction for panel without longitudinal stiffeners (t_shell=0)

*	print *,'nr. rad. L:',rad_lo
*	print *,'nr. rad. l:',rad_la
*
*	print *,'p lo:',p_lo(1:rad_lo)
*	print *,'p la:',p_la(1:rad_la)
*
*	print *,'type lo:',type_lo(1:rad_lo)
*	print *,'type la:',type_la(1:rad_la)
*
*	print *,'sect lo:',sect_lo(1:rad_lo)
*	print *,'sect la:',sect_la(1:rad_la)
*	
*	print *,'type_sect:',type_sect
*	print *,'l2:',l2
*	print *,'ep2:',ep2
*	print *,'l3:',l3
*	print *,'ep3:',ep3
*
*	print *,'**** t_shell:',t_shell
	
	nr_noeuds=0
	nr_noeuds=rad_lo*(rad_la+2)+2*rad_la
	nr_poutres=0
	nr_poutres=rad_lo*(rad_la+1)+rad_la*(rad_lo+1)

*	print *,'nr_noeuds:',nr_noeuds
*	print *,'nr_poutres:',nr_poutres

	ddl_max_initial=nr_noeuds*ddl_maxn

	if ((ddl_max_initial.gt.5000).and.(c_big_panel_2.eq.0)) then

		c_big_panel=1
		c_big_panel_2=1
		
		print *,'Panel too big --> analyze of a quarter of panel'
*		pause

		rad_lo7=0
		rad_la7=0
*-----
		DEALLOCATE(p_lo, STAT=sv0)
		DEALLOCATE(type_lo, STAT=sv0)
		DEALLOCATE(sect_lo, STAT=sv0)

		DEALLOCATE(p_la, STAT=sv0)
		DEALLOCATE(type_la, STAT=sv0)
		DEALLOCATE(sect_la, STAT=sv0)
*-----
		long7=0.d0
		test1_rad_lo2=rad_lo2/2
		test2_rad_lo2=2*test1_rad_lo2
	    if (rad_lo2.eq.test2_rad_lo2) then
			rad_lo7=rad_lo2/2
			long7=long/2.d0
		elseif (rad_lo2.gt.test2_rad_lo2) then
			rad_lo7=1+rad_lo2/2
			long7=long/2.d0+0.005d0
		elseif (rad_lo2.eq.1) then
			rad_lo7=1
			long7=long/2.d0
		endif
*-----
		larg7=0.d0
		test1_rad_la2=rad_la2/2
		test2_rad_la2=2*test1_rad_la2
	    if (rad_la2.eq.test2_rad_la2) then
			rad_la7=rad_la2/2
			larg7=larg/2.d0
		elseif (rad_la2.gt.test2_rad_la2) then
			rad_la7=1+rad_la2/2
			larg7=larg/2.d0+0.005d0
		elseif (rad_la2.eq.1) then
			rad_la7=1
			larg7=larg/2.d0
		endif	
*-----

		DEALLOCATE(p_lo7, STAT=sv0)
		DEALLOCATE(type_lo7, STAT=sv0)
		DEALLOCATE(sect_lo7, STAT=sv0)

		DEALLOCATE(p_la7, STAT=sv0)
		DEALLOCATE(type_la7, STAT=sv0)
		DEALLOCATE(sect_la7, STAT=sv0)

		ALLOCATE(p_lo7(rad_lo7))
		ALLOCATE(type_lo7(rad_lo7))
		ALLOCATE(sect_lo7(rad_lo7))

		p_lo7(1:rad_lo7)=0.d0
		type_lo7(1:rad_lo7)=0
		sect_lo7(1:rad_lo7)='0'

		ALLOCATE(p_la7(rad_la7))
		ALLOCATE(type_la7(rad_la7))
		ALLOCATE(sect_la7(rad_la7))

		p_la7(1:rad_la7)=0.d0
		type_la7(1:rad_la7)=0
		sect_la7(1:rad_la7)='0'

*-----
		p_lo7(1:rad_lo7)=p_lo2(1:rad_lo7)
		type_lo7(1:rad_lo7)=type_lo2(1:rad_lo7)
		sect_lo7(1:rad_lo7)=sect_lo2(1:rad_lo7)

		p_la7(1:rad_la7)=p_la2(1:rad_la7)
		type_la7(1:rad_la7)=type_la2(1:rad_la7)
		sect_la7(1:rad_la7)=sect_la2(1:rad_la7)

*	print *,'p lo 7:',p_lo7(1:rad_lo7)
*	print *,'p la 7:',p_la7(1:rad_la7)
*
*	print *,'type lo 7:',type_lo7(1:rad_lo7)
*	print *,'type la 7:',type_la7(1:rad_la7)
*
*	print *,'sect lo 7:',sect_lo7(1:rad_lo7)
*	print *,'sect la 7:',sect_la7(1:rad_la7)
*-----

	goto 1111

	endif
	
*******************************************************************************
*                Debut decoupage en poutres et moments d'inertie              *
*******************************************************************************
	
	ALLOCATE(x_p(nr_noeuds))
	ALLOCATE(y_p(nr_noeuds))
	ALLOCATE(z_p(nr_noeuds))

	x_p(1:nr_noeuds)=0.d0
	y_p(1:nr_noeuds)=0.d0
	z_p(1:nr_noeuds)=0.d0

	rad_la_to=rad_la+2 !; print *,'rad_la_to:',rad_la_to
	rad_lo_to=rad_lo+2 !; print *,'rad_lo_to:',rad_lo_to

	contor_noeuds=0

	k_1=0;k_2=0;k_3=0

	j22=0
	do j22=1,rad_la_to
		if (j22.eq.1) then
			
			k_1=0
			do k_1=1,rad_lo
				x_p(k_1)=p_lo(k_1)
				y_p(k_1)=0.0
				z_p(k_1)=0.0
			enddo
			contor_noeuds=k_1-1
		
		elseif ((j22.ne.1).and.(j22.ne.rad_la_to)) then
			
			k_2=0
			do k_2=1,rad_lo_to

				y_p(contor_noeuds+k_2)=0.0
				z_p(contor_noeuds+k_2)=p_la(j22-1)

				if (k_2.eq.1) then
					x_p(contor_noeuds+k_2)=0.0
				elseif ((k_2.ne.1).and.(k_2.ne.rad_lo_to)) then
					x_p(contor_noeuds+k_2)=p_lo(k_2-1)
				elseif (k_2.eq.rad_lo_to) then
					x_p(contor_noeuds+k_2)=long7
				endif

			enddo
			contor_noeuds=contor_noeuds+k_2-1
*			print *,'contor_noeuds:',contor_noeuds

		elseif (j22.eq.rad_la_to) then
		
			k_3=0
			do k_3=1,rad_lo
				x_p(contor_noeuds+k_3)=p_lo(k_3)
				y_p(contor_noeuds+k_3)=0.0
				z_p(contor_noeuds+k_3)=larg7
			enddo
		
		endif
	enddo
	
*	open(116,file='input_noeuds.dat')
*	  i=0
*	  do i=1,nr_noeuds
*          write(116,*) x_p(i), y_p(i), z_p(i)
*	  enddo
*	close(116)

* decoupage et inertia cote largeur
*------------------------------------------------------------------------------

*	print *,'********** Gravite COTE LARGEUR *****************'
*     calcul centres de gravite

	ALLOCATE(S2_lat(rad_la))
	ALLOCATE(zc2_lat(rad_la))
	ALLOCATE(yc2_lat(rad_la))
	ALLOCATE(S2_zc2_lat(rad_la))
	ALLOCATE(S2_yc2_lat(rad_la))

	ALLOCATE(S3_lat(rad_la))
	ALLOCATE(zc3_lat(rad_la))
	ALLOCATE(yc3_lat(rad_la))
	ALLOCATE(S3_zc3_lat(rad_la))
	ALLOCATE(S3_yc3_lat(rad_la))

	ALLOCATE(S2_lon(rad_lo))
	ALLOCATE(zc2_lon(rad_lo))
	ALLOCATE(yc2_lon(rad_lo))
	ALLOCATE(S2_zc2_lon(rad_lo))
	ALLOCATE(S2_yc2_lon(rad_lo))

	ALLOCATE(S3_lon(rad_lo))
	ALLOCATE(zc3_lon(rad_lo))
	ALLOCATE(yc3_lon(rad_lo))
	ALLOCATE(S3_zc3_lon(rad_lo))
	ALLOCATE(S3_yc3_lon(rad_lo))
	
	S2_lat(1:rad_la)=0.d0
	zc2_lat(1:rad_la)=0.d0
	yc2_lat(1:rad_la)=0.d0
	S2_zc2_lat(1:rad_la)=0.d0
	S2_yc2_lat(1:rad_la)=0.d0

	S3_lat(1:rad_la)=0.d0
	zc3_lat(1:rad_la)=0.d0
	yc3_lat(1:rad_la)=0.d0
	S3_zc3_lat(1:rad_la)=0.d0
	S3_yc3_lat(1:rad_la)=0.d0

	S2_lon(1:rad_lo)=0.d0
	zc2_lon(1:rad_lo)=0.d0
	yc2_lon(1:rad_lo)=0.d0
	S2_zc2_lon(1:rad_lo)=0.d0
	S2_yc2_lon(1:rad_lo)=0.d0

	S3_lon(1:rad_lo)=0.d0
	zc3_lon(1:rad_lo)=0.d0
	yc3_lon(1:rad_lo)=0.d0
	S3_zc3_lon(1:rad_lo)=0.d0
	S3_yc3_lon(1:rad_lo)=0.d0

	S_lat=0.d0
	zc1_lat=0.d0
	yc1_lat=0.d0

	S_lat=larg7*thick_plate
	zc1_lat=larg7/2.d0
	yc1_lat=thick_plate/2.d0
	
	i=0
	do i=1,rad_la
		
*	    print *,'type_la(i)',type_la(i)

		S2_lat(i)=l2(type_la(i))*ep2(type_la(i))

		zc2_lat(i)=p_la(i)
		yc2_lat(i)=thick_plate+l2(type_la(i))/2.d0

	    S2_zc2_lat(i)=S2_lat(i)*zc2_lat(i)
		S2_yc2_lat(i)=S2_lat(i)*yc2_lat(i)
		


		S3_lat(i)=l3(type_la(i))*ep3(type_la(i))
		
		if (sect_la(i).eq.'I') then
			zc3_lat(i)=p_la(i)
		elseif (sect_la(i).eq.'L') then
			zc3_lat(i)=p_la(i)-ep2(type_la(i))/2.d0+l3(type_la(i))/2.d0
		endif

		yc3_lat(i)=thick_plate+l2(type_la(i))+ep3(type_la(i))/2.d0
		S3_zc3_lat(i)=S3_lat(i)*zc3_lat(i)
		S3_yc3_lat(i)=S3_lat(i)*yc3_lat(i)

	enddo
	
	S_lat_total=0.d0
	S_lat_total=S_lat+S_lat+SUM(S2_lat)+SUM(S3_lat)

	zc_lat=0.d0
	zc_lat=(S_lat*zc1_lat+SUM(S2_zc2_lat)+SUM(S3_zc3_lat))/(S_lat+
     *          SUM(S2_lat)+SUM(S3_lat))

	yc_lat=0.d0
	yc_lat=(S_lat*yc1_lat+SUM(S2_yc2_lat)+SUM(S3_yc3_lat))/(S_lat+
     *          SUM(S2_lat)+SUM(S3_lat))

*	print *,'zc_lat:',zc_lat
*	print *,'yc_lat:',yc_lat

*	pause

*	print *,'********** Gravite COTE LONGUEUR *****************'
	
	S_lon=0.d0
	zc1_lon=0.d0
	yc1_lon=0.d0

	S_lon=long7*thick_plate
	zc1_lon=long7/2.d0
	yc1_lon=thick_plate/2.d0
	
	i=0
	do i=1,rad_lo
		
*	    print *,'type_lo(i)',type_lo(i)

		S2_lon(i)=l2(type_lo(i))*ep2(type_lo(i))
		zc2_lon(i)=p_lo(i)
		yc2_lon(i)=thick_plate+l2(type_lo(i))/2.d0

	    S2_zc2_lon(i)=S2_lon(i)*zc2_lon(i)
		S2_yc2_lon(i)=S2_lon(i)*yc2_lon(i)
		

		S3_lon(i)=l3(type_lo(i))*ep3(type_lo(i))
		zc3_lon(i)=p_lo(i)
		yc3_lon(i)=thick_plate+l2(type_lo(i))+ep3(type_lo(i))/2.d0

		S3_zc3_lon(i)=S3_lon(i)*zc3_lon(i)
		S3_yc3_lon(i)=S3_lon(i)*yc3_lon(i)
	
	enddo
	
	S_lon_total=0.d0
	S_lon_total=S_lon+SUM(S2_lon)+SUM(S3_lon)

	zc_lon=0.d0
	zc_lon=(S_lon*zc1_lon+SUM(S2_zc2_lon)+SUM(S3_zc3_lon))/
     *          (S_lon+SUM(S2_lon)+SUM(S3_lon))

	yc_lon=0.d0
	yc_lon=(S_lon*yc1_lon+SUM(S2_yc2_lon)+SUM(S3_yc3_lon))/
     *          (S_lon+SUM(S2_lon)+SUM(S3_lon))

*	print *,'zc_lon:',zc_lon
*	print *,'yc_lon:',yc_lon

*	pause
*	print *,'********** plan neutru al placii *****************'
*	yc_str=(S_lat_total*yc_lat+S_lon_total*yc_lon)/(S_lat_total+S_lon_total)
*	print *,'yc_str:',yc_str
*	pause
	
*	print *,'********** Decoupage COTE LARGEUR *****************'
*     decouplage et ecriture par poutres COTE LARGEUR
	
	ALLOCATE(Iz_la(rad_la,30))
	ALLOCATE(Iy_la(rad_la,30))
	ALLOCATE(S_la(rad_la,30))
	ALLOCATE(J_la(rad_la,30))
	
	Iz_la(1:rad_la,1:30)=0.d0
	Iy_la(1:rad_la,1:30)=0.d0
	S_la(1:rad_la,1:30)=0.d0
	J_la(1:rad_la,1:30)=0.d0

	ALLOCATE(Iz_lo(rad_lo,30))
	ALLOCATE(Iy_lo(rad_lo,30))
	ALLOCATE(S_lo(rad_lo,30))
	ALLOCATE(J_lo(rad_lo,30))
	
	Iz_lo(1:rad_lo,1:30)=0.d0
	Iy_lo(1:rad_lo,1:30)=0.d0
	S_lo(1:rad_lo,1:30)=0.d0
	J_lo(1:rad_lo,1:30)=0.d0

	ALLOCATE(l_la(rad_la))
	ALLOCATE(l_lo(rad_lo))
	
	l_la(1:rad_la)=0.d0
	l_lo(1:rad_lo)=0.d0

	ALLOCATE(S1_la_l(rad_la))
	ALLOCATE(zc1_la_l(rad_la))
	ALLOCATE(yc1_la_l(rad_la))
	ALLOCATE(S1_zc1_la_l(rad_la))
	ALLOCATE(S1_yc1_la_l(rad_la))
	
	S1_la_l(1:rad_la)=0.d0
	zc1_la_l(1:rad_la)=0.d0
	yc1_la_l(1:rad_la)=0.d0
	S1_zc1_la_l(1:rad_la)=0.d0
	S1_yc1_la_l(1:rad_la)=0.d0

	ALLOCATE(S2_la_l(rad_la))
	ALLOCATE(zc2_la_l(rad_la))
	ALLOCATE(yc2_la_l(rad_la))
	ALLOCATE(S2_zc2_la_l(rad_la))
	ALLOCATE(S2_yc2_la_l(rad_la))
	
	S2_la_l(1:rad_la)=0.d0
	zc2_la_l(1:rad_la)=0.d0
	yc2_la_l(1:rad_la)=0.d0
	S2_zc2_la_l(1:rad_la)=0.d0
	S2_yc2_la_l(1:rad_la)=0.d0

	ALLOCATE(S3_la_l(rad_la))
	ALLOCATE(zc3_la_l(rad_la))
	ALLOCATE(yc3_la_l(rad_la))
	ALLOCATE(S3_zc3_la_l(rad_la))
	ALLOCATE(S3_yc3_la_l(rad_la))

	S3_la_l(1:rad_la)=0.d0
	zc3_la_l(1:rad_la)=0.d0
	yc3_la_l(1:rad_la)=0.d0
	S3_zc3_la_l(1:rad_la)=0.d0
	S3_yc3_la_l(1:rad_la)=0.d0

	ALLOCATE(S1_lo_l(rad_lo))
	ALLOCATE(zc1_lo_l(rad_lo))
	ALLOCATE(yc1_lo_l(rad_lo))
	ALLOCATE(S1_zc1_lo_l(rad_lo))
	ALLOCATE(S1_yc1_lo_l(rad_lo))
	
	S1_lo_l(1:rad_lo)=0.d0
	zc1_lo_l(1:rad_lo)=0.d0
	yc1_lo_l(1:rad_lo)=0.d0
	S1_zc1_lo_l(1:rad_lo)=0.d0
	S1_yc1_lo_l(1:rad_lo)=0.d0

	ALLOCATE(S2_lo_l(rad_lo))
	ALLOCATE(zc2_lo_l(rad_lo))
	ALLOCATE(yc2_lo_l(rad_lo))
	ALLOCATE(S2_zc2_lo_l(rad_lo))
	ALLOCATE(S2_yc2_lo_l(rad_lo))
	
	S2_lo_l(1:rad_lo)=0.d0
	zc2_lo_l(1:rad_lo)=0.d0
	yc2_lo_l(1:rad_lo)=0.d0
	S2_zc2_lo_l(1:rad_lo)=0.d0
	S2_yc2_lo_l(1:rad_lo)=0.d0

	ALLOCATE(S3_lo_l(rad_lo))
	ALLOCATE(zc3_lo_l(rad_lo))
	ALLOCATE(yc3_lo_l(rad_lo))
	ALLOCATE(S3_zc3_lo_l(rad_lo))
	ALLOCATE(S3_yc3_lo_l(rad_lo))
	
	S3_lo_l(1:rad_lo)=0.d0
	zc3_lo_l(1:rad_lo)=0.d0
	yc3_lo_l(1:rad_lo)=0.d0
	S3_zc3_lo_l(1:rad_lo)=0.d0
	S3_yc3_lo_l(1:rad_lo)=0.d0

	ALLOCATE(zc_la_r(rad_la))
	ALLOCATE(yc_la_r(rad_la))
	
	zc_la_r(1:rad_la)=0.d0
	yc_la_r(1:rad_la)=0.d0

	ALLOCATE(zc_lo_r(rad_lo))
	ALLOCATE(yc_lo_r(rad_lo))
	
	zc_lo_r(1:rad_lo)=0.d0
	yc_lo_r(1:rad_lo)=0.d0

	ALLOCATE(Iz_la_1(rad_la))
	ALLOCATE(Iz_la_2(rad_la))
	ALLOCATE(Iz_la_3(rad_la))

	Iz_la_1(1:rad_la)=0.d0
	Iz_la_2(1:rad_la)=0.d0
	Iz_la_3(1:rad_la)=0.d0

	ALLOCATE(Iy_la_1(rad_la))
	ALLOCATE(Iy_la_2(rad_la))
	ALLOCATE(Iy_la_3(rad_la))

	Iy_la_1(1:rad_la)=0.d0
	Iy_la_2(1:rad_la)=0.d0
	Iy_la_3(1:rad_la)=0.d0

	ALLOCATE(Izc_la(rad_la))
	ALLOCATE(Iyc_la(rad_la))

	Izc_la(1:rad_la)=0.d0
	Iyc_la(1:rad_la)=0.d0


	ALLOCATE(Iz_lo_1(rad_lo))
	ALLOCATE(Iz_lo_2(rad_lo))
	ALLOCATE(Iz_lo_3(rad_lo))

	Iz_lo_1(1:rad_lo)=0.d0
	Iz_lo_2(1:rad_lo)=0.d0
	Iz_lo_3(1:rad_lo)=0.d0

	ALLOCATE(Iy_lo_1(rad_lo))
	ALLOCATE(Iy_lo_2(rad_lo))
	ALLOCATE(Iy_lo_3(rad_lo))

	Iy_lo_1(1:rad_lo)=0.d0
	Iy_lo_2(1:rad_lo)=0.d0
	Iy_lo_3(1:rad_lo)=0.d0

	ALLOCATE(Izc_lo(rad_lo))
	ALLOCATE(Iyc_lo(rad_lo))

	Izc_lo(1:rad_lo)=0.d0
	Iyc_lo(1:rad_lo)=0.d0

	ALLOCATE(dz_la_1(rad_la))
	ALLOCATE(dz_la_2(rad_la))
	ALLOCATE(dz_la_3(rad_la))
	ALLOCATE(dy_la_1(rad_la))
	ALLOCATE(dy_la_2(rad_la))
	ALLOCATE(dy_la_3(rad_la))
	
	dz_la_1(1:rad_la)=0.d0
	dz_la_2(1:rad_la)=0.d0
	dz_la_3(1:rad_la)=0.d0
	dy_la_1(1:rad_la)=0.d0
	dy_la_2(1:rad_la)=0.d0
	dy_la_3(1:rad_la)=0.d0

	ALLOCATE(dz_lo_1(rad_lo))
	ALLOCATE(dz_lo_2(rad_lo))
	ALLOCATE(dz_lo_3(rad_lo))
	ALLOCATE(dy_lo_1(rad_lo))
	ALLOCATE(dy_lo_2(rad_lo))
	ALLOCATE(dy_lo_3(rad_lo))

	dz_lo_1(1:rad_lo)=0.d0
	dz_lo_2(1:rad_lo)=0.d0
	dz_lo_3(1:rad_lo)=0.d0
	dy_lo_1(1:rad_lo)=0.d0
	dy_lo_2(1:rad_lo)=0.d0
	dy_lo_3(1:rad_lo)=0.d0


	i=0
	do i=1,rad_la
	
			if (i.eq.1) then
				l_la(i)=p_la(i)+(p_la(i+1)-p_la(i))/2.d0
			elseif ((i.ge.2).and.(i.lt.rad_la)) then
				l_la(i)=p_la(i)+(p_la(i+1)-p_la(i))/2.d0-SUM(l_la)
			elseif (i.eq.rad_la) then
				l_la(i)=larg7-SUM(l_la)
			endif
			
*			print *,'----------------------'
*			print *,i
*			print *,'type_la(i):',type_la(i)
*			print *,'l1(type_la(i)):',l_la(i)
*			print *,'thick_plate:',thick_plate
*			print *,'l2:',l2(type_la(i))
*			print *,'ep2:',ep2(type_la(i))
*			print *,'l3:',l3(type_la(i))
*			print *,'ep3:',ep3(type_la(i))
*			print *,'----------------------'

			S1_la_l(i)=l_la(i)*thick_plate
			
			if (i.eq.1) then
				zc1_la_l(i)=l_la(i)/2.d0
			else
				zc1_la_l(i)=SUM(l_la)-l_la(i)+l_la(i)/2.d0
			endif

			yc1_la_l(i)=thick_plate/2.d0
		    S1_zc1_la_l(i)=S1_la_l(i)*zc1_la_l(i)
			S1_yc1_la_l(i)=S1_la_l(i)*yc1_la_l(i)


			S2_la_l(i)=l2(type_la(i))*ep2(type_la(i))
			zc2_la_l(i)=p_la(i)
			yc2_la_l(i)=thick_plate+l2(type_la(i))/2.d0
		    S2_zc2_la_l(i)=S2_la_l(i)*zc2_la_l(i)
			S2_yc2_la_l(i)=S2_la_l(i)*yc2_la_l(i)
		

			S3_la_l(i)=l3(type_la(i))*ep3(type_la(i))

			if (sect_la(i).eq.'I') then
				zc3_la_l(i)=p_la(i)
			elseif (sect_la(i).eq.'L') then
				zc3_la_l(i)=p_la(i)-ep2(type_la(i))/2.d0+l3(type_la(i))/2.d0
			endif

			yc3_la_l(i)=thick_plate+l2(type_la(i))+ep3(type_la(i))/2.d0
			S3_zc3_la_l(i)=S3_la_l(i)*zc3_la_l(i)
			S3_yc3_la_l(i)=S3_la_l(i)*yc3_la_l(i)


			zc_la_r(i)=(S1_zc1_la_l(i)+S2_zc2_la_l(i)+S3_zc3_la_l(i))/
     &				   (S1_la_l(i)+S2_la_l(i)+S3_la_l(i))

			yc_la_r(i)=(S1_yc1_la_l(i)+S2_yc2_la_l(i)+S3_yc3_la_l(i))/
     &				   (S1_la_l(i)+S2_la_l(i)+S3_la_l(i))
	
*			print *,'zc_la_r(i):',zc_la_r(i)
*			print *,'yc_la_r(i):',yc_la_r(i)

			Iz_la_1(i)=l_la(i)*thick_plate**3.d0/12.d0
				dy_la_1(i)=abs(yc_la_r(i)-yc1_la_l(i))
			Iz_la_2(i)=ep2(type_la(i))*l2(type_la(i))**3.d0/12.d0
				dy_la_2(i)=abs(yc_la_r(i)-yc2_la_l(i))
			Iz_la_3(i)=l3(type_la(i))*ep3(type_la(i))**3.d0/12.d0
				dy_la_3(i)=abs(yc_la_r(i)-yc3_la_l(i))


			Izc_la(i)=(Iz_la_1(i)+S1_la_l(i)*dy_la_1(i)**2.d0)+
     &				  (Iz_la_2(i)+S2_la_l(i)*dy_la_2(i)**2.d0)+
     &				  (Iz_la_3(i)+S3_la_l(i)*dy_la_3(i)**2.d0)

	
			Iy_la_1(i)=l_la(i)**3.d0*thick_plate/12.d0
				dz_la_1(i)=abs(zc_la_r(i)-zc1_la_l(i))
			Iy_la_2(i)=ep2(type_la(i))**3.d0*l2(type_la(i))/12.d0
				dz_la_2(i)=abs(zc_la_r(i)-zc2_la_l(i))
			Iy_la_3(i)=l3(type_la(i))**3.d0*ep3(type_la(i))/12.d0
				dz_la_3(i)=abs(zc_la_r(i)-zc3_la_l(i))

			Iyc_la(i)=(Iy_la_1(i)+S1_la_l(i)*dz_la_1(i)**2.d0)+
     &				  (Iy_la_2(i)+S2_la_l(i)*dz_la_2(i)**2.d0)+
     &				  (Iy_la_3(i)+S3_la_l(i)*dz_la_3(i)**2.d0)

*---------------
* Case C1 = Iz_la(i,type_la(i))=Izc_la(i) - plan neutre de la section consideree
* Case C2 = Iz_la(i,type_la(i))=Izc_la(i)+(abs(yc_lat-yc_la_r(i)))**2.d0*.... - plan neutre face
* Case C3 = Iz_la(i,type_la(i))=Izc_la(i)+(abs(yc_str-yc_la_r(i)))**2.d0*.... - plan neutre plaque

			Iz_la(i,type_la(i))=Izc_la(i) !+(abs(yc_lat-yc_la_r(i)))**2.d0*
!     &										(S1_la_l(i)+S2_la_l(i)+S3_la_l(i))

			Iy_la(i,type_la(i))=Iyc_la(i)
*---------------			

			if (long7.ge.larg7) then
				if (t_shell.eq.0) then
					S_la(i,type_la(i))=S1_la_l(i) !!!+S2_la_l(i)+S3_la_l(i)
				elseif (t_shell.eq.1) then
					S_la(i,type_la(i))=S2_la_l(i)+S3_la_l(i)
				elseif (t_shell.eq.2) then
					S_la(i,type_la(i))=S1_la_l(i)+S2_la_l(i)+S3_la_l(i)
				endif
			else
				if (t_shell.eq.0) then
					S_la(i,type_la(i))=S2_la_l(i)+S3_la_l(i) 
				elseif (t_shell.eq.1) then
					S_la(i,type_la(i))=S1_la_l(i) !!!+S2_la_l(i)+S3_la_l(i)
				elseif (t_shell.eq.2) then
					S_la(i,type_la(i))=S2_la_l(i)+S3_la_l(i)
				endif
			endif

	        J_la(i,type_la(i))=(l_la(i)*thick_plate**3.d0+
     &							l2(type_la(i))*ep2(type_la(i))**3.d0+
     &							l3(type_la(i))*ep3(type_la(i))**3.d0)/3.d0

*			print *,'Iz_la:',Iz_la(i,type_la(i))*1.d+8
*			print *,'Iy_la:',Iy_la(i,type_la(i))*1.d+8
*			print *,'S_la:' , S_la(i,type_la(i))*1.d+4
*			print *,'J_la:' , J_la(i,type_la(i))*1.d+8

	enddo
	

* decoupage et inertia cote longueur
*------------------------------------------------------------------------------

*	print *,'********** Decoupage COTE LONGUEUR *****************'
*     decouplage et ecriture par poutres
	
	i=0
	do i=1,rad_lo
	
			if (i.eq.1) then
				l_lo(i)=p_lo(i)+(p_lo(i+1)-p_lo(i))/2.d0
			elseif ((i.ge.2).and.(i.lt.rad_lo)) then
				l_lo(i)=p_lo(i)+(p_lo(i+1)-p_lo(i))/2.d0-SUM(l_lo)
			elseif (i.eq.rad_lo) then
				l_lo(i)=long7-SUM(l_lo)
			endif
			
*			print *,'----------------------'
*	        print *,i
*			print *,'type_lo(i):',type_lo(i)
*			print *,'l_lo(i):',l_lo(i)
*			print *,'thick_plate:',thick_plate
*			print *,'l2:',l2(type_lo(i))
*			print *,'ep2:',ep2(type_lo(i))
*			print *,'l3:',l3(type_lo(i))
*			print *,'ep3:',ep3(type_lo(i))
*			print *,'----------------------'

			S1_lo_l(i)=l_lo(i)*thick_plate
			
			if (i.eq.1) then
				zc1_lo_l(i)=l_lo(i)/2.d0
			else
				zc1_lo_l(i)=SUM(l_lo)-l_lo(i)+l_lo(i)/2.d0
			endif

			yc1_lo_l(i)=thick_plate/2.d0
		    S1_zc1_lo_l(i)=S1_lo_l(i)*zc1_lo_l(i)
			S1_yc1_lo_l(i)=S1_lo_l(i)*yc1_lo_l(i)


			S2_lo_l(i)=l2(type_lo(i))*ep2(type_lo(i))
			zc2_lo_l(i)=p_lo(i)
			yc2_lo_l(i)=thick_plate+l2(type_lo(i))/2.d0
		    S2_zc2_lo_l(i)=S2_lo_l(i)*zc2_lo_l(i)
			S2_yc2_lo_l(i)=S2_lo_l(i)*yc2_lo_l(i)
		

			S3_lo_l(i)=l3(type_lo(i))*ep3(type_lo(i))
			zc3_lo_l(i)=p_lo(i)
			yc3_lo_l(i)=thick_plate+l2(type_lo(i))+ep3(type_lo(i))/2.d0
			S3_zc3_lo_l(i)=S3_lo_l(i)*zc3_lo_l(i)
			S3_yc3_lo_l(i)=S3_lo_l(i)*yc3_lo_l(i)


			zc_lo_r(i)=(S1_zc1_lo_l(i)+S2_zc2_lo_l(i)+S3_zc3_lo_l(i))/
     &				   (S1_lo_l(i)+S2_lo_l(i)+S3_lo_l(i))

			yc_lo_r(i)=(S1_yc1_lo_l(i)+S2_yc2_lo_l(i)+S3_yc3_lo_l(i))/
     &				   (S1_lo_l(i)+S2_lo_l(i)+S3_lo_l(i))
	
*			print *,'zc_lo_r(i):',zc_lo_r(i)
*			print *,'yc_lo_r(i):',yc_lo_r(i)


			Iz_lo_1(i)=l_lo(i)*thick_plate**3.d0/12.d0
				dy_lo_1(i)=abs(yc_lo_r(i)-yc1_lo_l(i))

			Iz_lo_2(i)=ep2(type_lo(i))*l2(type_lo(i))**3.d0/12.d0
				dy_lo_2(i)=abs(yc_lo_r(i)-yc2_lo_l(i))

			Iz_lo_3(i)=l3(type_lo(i))*ep3(type_lo(i))**3.d0/12.d0
				dy_lo_3(i)=abs(yc_lo_r(i)-yc3_lo_l(i))


			Izc_lo(i)=(Iz_lo_1(i)+S1_lo_l(i)*dy_lo_1(i)**2.d0)+
     &				  (Iz_lo_2(i)+S2_lo_l(i)*dy_lo_2(i)**2.d0)+
     &				  (Iz_lo_3(i)+S3_lo_l(i)*dy_lo_3(i)**2.d0)

	
			Iy_lo_1(i)=l_lo(i)**3.d0*thick_plate/12.d0
				dz_lo_1(i)=abs(zc_lo_r(i)-zc1_lo_l(i))

			Iy_lo_2(i)=ep2(type_lo(i))**3.d0*l2(type_lo(i))/12.d0
				dz_lo_2(i)=abs(zc_lo_r(i)-zc2_lo_l(i))

			Iy_lo_3(i)=l3(type_lo(i))**3.d0*ep3(type_lo(i))/12.d0
				dz_lo_3(i)=abs(zc_lo_r(i)-zc3_lo_l(i))

			Iyc_lo(i)=(Iy_lo_1(i)+S1_lo_l(i)*dz_lo_1(i)**2.d0)+
     &				  (Iy_lo_2(i)+S2_lo_l(i)*dz_lo_2(i)**2.d0)+
     &				  (Iy_lo_3(i)+S3_lo_l(i)*dz_lo_3(i)**2.d0)

*---------------
* Case C1 = Iz_lo(i,type_la(i))=Izc_lo(i) - plan neutre de la section consideree
* Case C2 = Iz_lo(i,type_la(i))=Izc_lo(i)+(abs(yc_lan-yc_lo_r(i)))**2.d0*.... - plan neutre face
* Case C3 = Iz_lo(i,type_la(i))=Izc_lo(i)+(abs(yc_str-yc_lo_r(i)))**2.d0*.... - plan neutre plaque
	
			Iz_lo(i,type_lo(i))=Izc_lo(i) !+(abs(yc_lon-yc_lo_r(i)))**2.d0*	!***
!     &										(S1_lo_l(i)+S2_lo_l(i)+S3_lo_l(i))

			Iy_lo(i,type_lo(i))=Iyc_lo(i)
*---------------
			
			if (long7.ge.larg7) then 
				if (t_shell.eq.0) then
					S_lo(i,type_lo(i))=S2_lo_l(i)+S3_lo_l(i)
				elseif (t_shell.eq.1) then
					S_lo(i,type_lo(i))=S1_lo_l(i) !+S2_lo_l(i)+S3_lo_l(i)
				elseif (t_shell.eq.2) then
					S_lo(i,type_lo(i))=S2_lo_l(i)+S3_lo_l(i)
				endif
			else
				if (t_shell.eq.0) then
					S_lo(i,type_lo(i))=S1_lo_l(i) !+S2_lo_l(i)+S3_lo_l(i)
				elseif (t_shell.eq.1) then
					S_lo(i,type_lo(i))=S2_lo_l(i)+S3_lo_l(i)
				elseif (t_shell.eq.2) then
					S_lo(i,type_lo(i))=S1_lo_l(i)+S2_lo_l(i)+S3_lo_l(i)
				endif
			endif

	        J_lo(i,type_lo(i))=(l_lo(i)*thick_plate**3.d0+
     &							l2(type_lo(i))*ep2(type_lo(i))**3.d0+
     &							l3(type_lo(i))*ep3(type_lo(i))**3.d0)/3.d0

*			print *,'Iz_lo:',Iz_lo(i,type_lo(i))*1d+8
*			print *,'Iy_lo:',Iy_lo(i,type_lo(i))*1d+8
*			print *,' S_lo:', S_lo(i,type_lo(i))*1d+4
*			print *,' J_lo:', J_lo(i,type_lo(i))*1d+8

	enddo

* ecriture du troisieme fichier input "input_poutres.dat"
*------------------------------------------------------------------------------

*  300 FORMAT(I,I,I,I,E,E,E,E)

	ALLOCATE(S_1(nr_poutres))
	ALLOCATE(J_1(nr_poutres))
	ALLOCATE(Iy_1(nr_poutres))
	ALLOCATE(Iz_1(nr_poutres))
	
	S_1(1:nr_poutres)=0.d0
	J_1(1:nr_poutres)=0.d0
	Iy_1(1:nr_poutres)=0.d0
	Iz_1(1:nr_poutres)=0.d0

	ALLOCATE(n_1(nr_poutres))
	ALLOCATE(n_2(nr_poutres))
	ALLOCATE(l_1(nr_poutres))
	ALLOCATE(l_2(nr_poutres))
	
	n_1(1:nr_poutres)=0
	n_2(1:nr_poutres)=0
	l_1(1:nr_poutres)=0
	l_2(1:nr_poutres)=0

*	print *,'OK - inertia'

	contor_poutres=0
	k_1=0;k_2=0;k_3=0
	c_beams3=0
	
!     boundary conditions
!	t_bc=1 - all sides clamped
!	t_bc=2 - side  1 simply supported; sides 2 and 3 and 4 clamped
!	t_bc=3 - sides 1 and 2 simply supported; sides 3 and4 clamped
!	t_bc=4 - sides 1 and 2 and 3 simply supported; side 4 clamped
!	t_bc=5 - all sides simply supported (+ 2 deplacement and 1 rotations bloqued)
!	t_bc=6 - sides 1 and 3 (longitudinals) simply supported; sides 2 and 4 (transversals) clamped
!	t_bc=7 - sides 2 and 4 (longitudinals) simply supported; sides 1 and 3 (transversals) clamped

!	l_node=1 - clamped
!	l_node=2 - free
!	l_node=31 - simply supported en 3D - side longitudinal (paralel with Ox)
!	l_node=33 - simply supported en 3D - side transevrsal  (paralel with Oz)

!	l_node=44 - Ox symmetry
!	l_node=55 - Oz symmetry 
	
	j23=0

	do j23=1,rad_la_to-1

		if (j23.eq.1) then	! side 1 - longitudinal side
			
			k_1=0
			do k_1=1,rad_lo

				c_beams3=c_beams3+1

				 n_1(c_beams3)=k_1
				 n_2(c_beams3)=k_1+(rad_lo_to-1)
!----------------------					
					if ((t_bc.eq.1).or.(t_bc.eq.7)) then
						l_1(c_beams3)=1
					elseif ((t_bc.eq.2).or.(t_bc.eq.3).or.(t_bc.eq.4).or.(t_bc.eq.5)
     *                          .or.(t_bc.eq.6)) then
						l_1(c_beams3)=31
					endif
!----------------------
				 l_2(c_beams3)=2
				 S_1(c_beams3)= S_lo(k_1,type_lo(k_1))*1d+4
				 J_1(c_beams3)= J_lo(k_1,type_lo(k_1))*1d+8
				Iy_1(c_beams3)=Iy_lo(k_1,type_lo(k_1))*1d+8
				Iz_1(c_beams3)=Iz_lo(k_1,type_lo(k_1))*1d+8

			enddo
			contor_poutres=k_1-1

		
		elseif ((j23.ne.1).and.(j23.ne.rad_la_to-1)) then
			

			do k_2=1,rad_lo_to

				if (k_2.eq.1) then

					c_beams3=c_beams3+1	! side 2 - transversal side

					 n_1(c_beams3)=contor_poutres+k_2
					 n_2(c_beams3)=contor_poutres+k_2+1
!--------------------------
						if ((t_bc.eq.1).or.(t_bc.eq.2).or.(t_bc.eq.6)) then
							l_1(c_beams3)=1
						elseif ((t_bc.eq.3).or.(t_bc.eq.4).or.(t_bc.eq.5).or.(t_bc.eq.7))
     *                          then
							l_1(c_beams3)=33
						endif
!--------------------------
					 l_2(c_beams3)=2
					 S_1(c_beams3)= S_la(j23-1,type_la(j23-1))*1d+4
					 J_1(c_beams3)= J_la(j23-1,type_la(j23-1))*1d+8
					Iy_1(c_beams3)=Iy_la(j23-1,type_la(j23-1))*1d+8
					Iz_1(c_beams3)=Iz_la(j23-1,type_la(j23-1))*1d+8

				elseif ((k_2.ne.1).and.(k_2.ne.rad_lo_to-1).and.(k_2.ne.rad_lo_to))
     *                  then

					c_beams3=c_beams3+1

					 n_1(c_beams3)=contor_poutres+k_2
					 n_2(c_beams3)=contor_poutres+k_2+1
					 l_1(c_beams3)=2
					 l_2(c_beams3)=2
					 S_1(c_beams3)= S_la(j23-1,type_la(j23-1))*1d+4
					 J_1(c_beams3)= J_la(j23-1,type_la(j23-1))*1d+8
					Iy_1(c_beams3)=Iy_la(j23-1,type_la(j23-1))*1d+8
					Iz_1(c_beams3)=Iz_la(j23-1,type_la(j23-1))*1d+8

					c_beams3=c_beams3+1

					 n_1(c_beams3)=contor_poutres+k_2
					 n_2(c_beams3)=contor_poutres+k_2+rad_lo_to
					 l_1(c_beams3)=2
					 l_2(c_beams3)=2
					 S_1(c_beams3)= S_lo(k_2-1,type_lo(k_2-1))*1d+4
					 J_1(c_beams3)= J_lo(k_2-1,type_lo(k_2-1))*1d+8
					Iy_1(c_beams3)=Iy_lo(k_2-1,type_lo(k_2-1))*1d+8
					Iz_1(c_beams3)=Iz_lo(k_2-1,type_lo(k_2-1))*1d+8

				elseif (k_2.eq.rad_lo_to-1) then

					c_beams3=c_beams3+1	! side 4 - transversal side

					 n_1(c_beams3)=contor_poutres+k_2
					 n_2(c_beams3)=contor_poutres+k_2+1
					 l_1(c_beams3)=2
!--------------------------
						if ((t_bc.eq.1).or.(t_bc.eq.2).or.(t_bc.eq.3).or.(t_bc.eq.4).or.
     *                          (t_bc.eq.6)) then
							if (c_big_panel.eq.0) then
								l_2(c_beams3)=1
							elseif (c_big_panel.eq.1) then
								l_2(c_beams3)=44
							endif
						elseif ((t_bc.eq.5).or.(t_bc.eq.7)) then
							if (c_big_panel.eq.0) then
								l_2(c_beams3)=33
							elseif (c_big_panel.eq.1) then
								l_2(c_beams3)=44
							endif
						endif
!--------------------------
					 S_1(c_beams3)= S_la(j23-1,type_la(j23-1))*1d+4
					 J_1(c_beams3)= J_la(j23-1,type_la(j23-1))*1d+8
					Iy_1(c_beams3)=Iy_la(j23-1,type_la(j23-1))*1d+8
					Iz_1(c_beams3)=Iz_la(j23-1,type_la(j23-1))*1d+8

					c_beams3=c_beams3+1

					 n_1(c_beams3)=contor_poutres+k_2
					 n_2(c_beams3)=contor_poutres+k_2+rad_lo_to
					 l_1(c_beams3)=2
					 l_2(c_beams3)=2
					 S_1(c_beams3)= S_lo(k_2-1,type_lo(k_2-1))*1d+4
					 J_1(c_beams3)= J_lo(k_2-1,type_lo(k_2-1))*1d+8
					Iy_1(c_beams3)=Iy_lo(k_2-1,type_lo(k_2-1))*1d+8
					Iz_1(c_beams3)=Iz_lo(k_2-1,type_lo(k_2-1))*1d+8

				endif

			enddo
			contor_poutres=contor_poutres+k_2-1

		elseif (j23.eq.rad_la_to-1) then

			do k_3=1,rad_lo_to-1

				if (k_3.eq.1) then

					c_beams3=c_beams3+1	! side 2 - transversal side (last point)

					 n_1(c_beams3)=contor_poutres+k_3
					 n_2(c_beams3)=contor_poutres+k_3+1
!--------------------------
						if ((t_bc.eq.1).or.(t_bc.eq.2).or.(t_bc.eq.6)) then
							l_1(c_beams3)=1
						elseif ((t_bc.eq.3).or.(t_bc.eq.4).or.(t_bc.eq.5).or.(t_bc.eq.7)) 
     *                      then
							l_1(c_beams3)=33
						endif
!--------------------------
					 l_2(c_beams3)=2
					 S_1(c_beams3)= S_la(j23-1,type_la(j23-1))*1d+4
					 J_1(c_beams3)= J_la(j23-1,type_la(j23-1))*1d+8
					Iy_1(c_beams3)=Iy_la(j23-1,type_la(j23-1))*1d+8
					Iz_1(c_beams3)=Iz_la(j23-1,type_la(j23-1))*1d+8

				elseif ((k_3.ne.1).and.(k_3.ne.rad_lo_to-1)) then

					c_beams3=c_beams3+1

					 n_1(c_beams3)=contor_poutres+k_3
					 n_2(c_beams3)=contor_poutres+k_3+1
					 l_1(c_beams3)=2
					 l_2(c_beams3)=2
					 S_1(c_beams3)= S_la(j23-1,type_la(j23-1))*1d+4
					 J_1(c_beams3)= J_la(j23-1,type_la(j23-1))*1d+8
					Iy_1(c_beams3)=Iy_la(j23-1,type_la(j23-1))*1d+8
					Iz_1(c_beams3)=Iz_la(j23-1,type_la(j23-1))*1d+8

					c_beams3=c_beams3+1	! side 3 - longitudinal side

					 n_1(c_beams3)=contor_poutres+k_3
					 n_2(c_beams3)=contor_poutres+k_3+rad_lo_to-1
					 l_1(c_beams3)=2
!--------------------------
						if ((t_bc.eq.1).or.(t_bc.eq.2).or.(t_bc.eq.3).or.(t_bc.eq.7)) then
							if (c_big_panel.eq.0) then
								l_2(c_beams3)=1
							elseif (c_big_panel.eq.1) then
								l_2(c_beams3)=55
							endif
						elseif ((t_bc.eq.4).or.(t_bc.eq.5).or.(t_bc.eq.6)) then
							if (c_big_panel.eq.0) then
								l_2(c_beams3)=31
							elseif (c_big_panel.eq.1) then
								l_2(c_beams3)=55
							endif
						endif
!--------------------------
					 S_1(c_beams3)= S_lo(k_3-1,type_lo(k_3-1))*1d+4
					 J_1(c_beams3)= J_lo(k_3-1,type_lo(k_3-1))*1d+8
					Iy_1(c_beams3)=Iy_lo(k_3-1,type_lo(k_3-1))*1d+8
					Iz_1(c_beams3)=Iz_lo(k_3-1,type_lo(k_3-1))*1d+8

				elseif (k_3.eq.rad_lo_to-1) then

					c_beams3=c_beams3+1	! side 4 - transversal side (last point)

					 n_1(c_beams3)=contor_poutres+k_3
					 n_2(c_beams3)=contor_poutres+k_3+1
					 l_1(c_beams3)=2
!--------------------------
						if ((t_bc.eq.1).or.(t_bc.eq.2).or.(t_bc.eq.3).or.(t_bc.eq.4).or.
     *                          (t_bc.eq.6)) then
							if (c_big_panel.eq.0) then
								l_2(c_beams3)=1
							elseif (c_big_panel.eq.1) then
								l_2(c_beams3)=44
							endif
						elseif ((t_bc.eq.5).or.(t_bc.eq.7)) then
							if (c_big_panel.eq.0) then
								l_2(c_beams3)=33
							elseif (c_big_panel.eq.1) then
								l_2(c_beams3)=44
							endif
						endif
!--------------------------
					 S_1(c_beams3)= S_la(j23-1,type_la(j23-1))*1d+4
					 J_1(c_beams3)= J_la(j23-1,type_la(j23-1))*1d+8
					Iy_1(c_beams3)=Iy_la(j23-1,type_la(j23-1))*1d+8
					Iz_1(c_beams3)=Iz_la(j23-1,type_la(j23-1))*1d+8

					c_beams3=c_beams3+1	! side 3 - longitudinal side (last point)

					 n_1(c_beams3)=contor_poutres+k_3
					 n_2(c_beams3)=contor_poutres+k_3+rad_lo_to-1
					 l_1(c_beams3)=2
!--------------------------
						if ((t_bc.eq.1).or.(t_bc.eq.2).or.(t_bc.eq.3).or.(t_bc.eq.7)) then
							if (c_big_panel.eq.0) then
								l_2(c_beams3)=1
							elseif (c_big_panel.eq.1) then
								l_2(c_beams3)=55
							endif
						elseif ((t_bc.eq.4).or.(t_bc.eq.5).or.(t_bc.eq.6)) then
							if (c_big_panel.eq.0) then
								l_2(c_beams3)=31
							elseif (c_big_panel.eq.1) then
								l_2(c_beams3)=55
							endif
						endif
!--------------------------
					 S_1(c_beams3)= S_lo(k_3-1,type_lo(k_3-1))*1d+4
					 J_1(c_beams3)= J_lo(k_3-1,type_lo(k_3-1))*1d+8
					Iy_1(c_beams3)=Iy_lo(k_3-1,type_lo(k_3-1))*1d+8
					Iz_1(c_beams3)=Iz_lo(k_3-1,type_lo(k_3-1))*1d+8

				endif
			
			enddo
		
		endif
	enddo	
	
*	pause
*******************************************************************************
*                  Fin decoupage en poutres et moments d'inertie              *
*******************************************************************************

*------------------------------------------------------------------------------

*******************************************************************************

	nn_val=0
	np_val=0
	nn_val=nr_noeuds
	np_val=nr_poutres

*	print*,'nombre de noeuds :',nn_val
*	print*,'nombre de poutres:',np_val

* - initialisation à zero des longueurs de poutres

	ALLOCATE(lung(nr_noeuds,nr_noeuds))
*	print *,'OK - poutres'
	
	lung(1:nr_noeuds,1:nr_noeuds)=0.d0

      i=0
	do i=1,nn_val
	  do j=1,nn_val
		lung(i,j)=0.d0
	  enddo
	enddo

* - lire les poutres existantes, liaisons entre elles, caracteristiques mecaniques
	
	ALLOCATE(S1(nr_noeuds,nr_noeuds))
	ALLOCATE(J1(nr_noeuds,nr_noeuds))
	ALLOCATE(Iy1(nr_noeuds,nr_noeuds))
	ALLOCATE(Iz1(nr_noeuds,nr_noeuds))

	ALLOCATE(t_l(nr_noeuds,nr_noeuds))

	S1(1:nr_noeuds,1:nr_noeuds)=0.d0
	J1(1:nr_noeuds,1:nr_noeuds)=0.d0
	Iy1(1:nr_noeuds,1:nr_noeuds)=0.d0
	Iz1(1:nr_noeuds,1:nr_noeuds)=0.d0

	t_l(1:nr_noeuds,1:nr_noeuds)=0

*	print *,'OK - valori inertia'
	
	i1=0

	do i1=1,np_val
	    
		t_l(n_1(i1),n_2(i1))=l_1(i1)
		t_l(n_2(i1),n_1(i1))=l_2(i1)

	    S1(n_1(i1),n_2(i1))=S_1(i1)*1d-4
		J1(n_1(i1),n_2(i1))=J_1(i1)*1d-8
		Iy1(n_1(i1),n_2(i1))=Iy_1(i1)*1d-8
		Iz1(n_1(i1),n_2(i1))=Iz_1(i1)*1d-8

	    lung(n_1(i1),n_2(i1))=1.d0

*	    print *,n_1(i1),n_2(i1),t_l(n_1(i1),n_2(i1)),t_l(n_2(i1),n_1(i1))

		if (i1.eq.1) then
			coef1=0.d0
			coef1=E1*S1(n_1(i1),n_2(i1))				![N]

			coef2=0.d0			
			coef2=E1*S1(n_1(i1),n_2(i1))				![N]

			coef3=0.d0		
			coef3=(coef1+coef2)/2.d0					![N]

			coef4=0.d0
			coef4=E1*J1(n_1(i1),n_2(i1))/thick_plate			![Nm]

			coef5=0.d0
			coef5=E1*Iz1(n_1(i1),n_2(i1))/thick_plate			![Nm]

			coef6=0.d0
			coef6=E1*Iy1(n_1(i1),n_2(i1))/thick_plate			![Nm]
		endif
	enddo

*******************************************************************************
*-------------------------- ZONE 1 DEALLOCATION ---------------------------------


	DEALLOCATE(p_lo, STAT=sv1)
	DEALLOCATE(type_lo, STAT=sv1)
	DEALLOCATE(sect_lo, STAT=sv1)
	DEALLOCATE(p_la, STAT=sv1)
	DEALLOCATE(type_la, STAT=sv1)
	DEALLOCATE(sect_la, STAT=sv1)

	DEALLOCATE(n_1, STAT=sv1)
	DEALLOCATE(n_2, STAT=sv1)
	DEALLOCATE(l_1, STAT=sv1)
	DEALLOCATE(l_2, STAT=sv1)

	DEALLOCATE(S_1, STAT=sv1)
	DEALLOCATE(J_1, STAT=sv1)
	DEALLOCATE(Iy_1, STAT=sv1)
	DEALLOCATE(Iz_1, STAT=sv1)

	DEALLOCATE(Iz_lo, STAT=sv1)
	DEALLOCATE(Iy_lo, STAT=sv1)
	DEALLOCATE(S_lo,  STAT=sv1)
	DEALLOCATE(J_lo,  STAT=sv1)

	DEALLOCATE(Iz_la, STAT=sv1)
	DEALLOCATE(Iy_la, STAT=sv1)
	DEALLOCATE(S_la,  STAT=sv1)
	DEALLOCATE(J_la,  STAT=sv1)

	DEALLOCATE(dz_lo_1, STAT=sv1)
	DEALLOCATE(dz_lo_2, STAT=sv1)
	DEALLOCATE(dz_lo_3, STAT=sv1)
	DEALLOCATE(dy_lo_1, STAT=sv1)
	DEALLOCATE(dy_lo_2, STAT=sv1)
	DEALLOCATE(dy_lo_3, STAT=sv1)

	DEALLOCATE(dz_la_1, STAT=sv1)
	DEALLOCATE(dz_la_2, STAT=sv1)
	DEALLOCATE(dz_la_3, STAT=sv1)
	DEALLOCATE(dy_la_1, STAT=sv1)
	DEALLOCATE(dy_la_2, STAT=sv1)
	DEALLOCATE(dy_la_3, STAT=sv1)

	DEALLOCATE(Izc_lo, STAT=sv1)
	DEALLOCATE(Iyc_lo, STAT=sv1)

	DEALLOCATE(Iy_lo_1, STAT=sv1)
	DEALLOCATE(Iy_lo_2, STAT=sv1)
	DEALLOCATE(Iy_lo_3, STAT=sv1)

	DEALLOCATE(Iz_lo_1, STAT=sv1)
	DEALLOCATE(Iz_lo_2, STAT=sv1)
	DEALLOCATE(Iz_lo_3, STAT=sv1)

	DEALLOCATE(Izc_la, STAT=sv1)
	DEALLOCATE(Iyc_la, STAT=sv1)

	DEALLOCATE(Iy_la_1, STAT=sv1)
	DEALLOCATE(Iy_la_2, STAT=sv1)
	DEALLOCATE(Iy_la_3, STAT=sv1)

	DEALLOCATE(Iz_la_1, STAT=sv1)
	DEALLOCATE(Iz_la_2, STAT=sv1)
	DEALLOCATE(Iz_la_3, STAT=sv1)

	DEALLOCATE(zc_lo_r, STAT=sv1)
	DEALLOCATE(yc_lo_r, STAT=sv1)

	DEALLOCATE(zc_la_r, STAT=sv1)
	DEALLOCATE(yc_la_r, STAT=sv1)

	DEALLOCATE(S3_lo_l, STAT=sv1)
	DEALLOCATE(zc3_lo_l, STAT=sv1)
	DEALLOCATE(yc3_lo_l, STAT=sv1)
	DEALLOCATE(S3_zc3_lo_l, STAT=sv1)
	DEALLOCATE(S3_yc3_lo_l, STAT=sv1)

	DEALLOCATE(S2_lo_l, STAT=sv1)
	DEALLOCATE(zc2_lo_l, STAT=sv1)
	DEALLOCATE(yc2_lo_l, STAT=sv1)
	DEALLOCATE(S2_zc2_lo_l, STAT=sv1)
	DEALLOCATE(S2_yc2_lo_l, STAT=sv1)

	DEALLOCATE(S1_lo_l, STAT=sv1)
	DEALLOCATE(zc1_lo_l, STAT=sv1)
	DEALLOCATE(yc1_lo_l, STAT=sv1)
	DEALLOCATE(S1_zc1_lo_l, STAT=sv1)
	DEALLOCATE(S1_yc1_lo_l, STAT=sv1)

	DEALLOCATE(S3_la_l, STAT=sv1)
	DEALLOCATE(zc3_la_l, STAT=sv1)
	DEALLOCATE(yc3_la_l, STAT=sv1)
	DEALLOCATE(S3_zc3_la_l, STAT=sv1)
	DEALLOCATE(S3_yc3_la_l, STAT=sv1)

	DEALLOCATE(S2_la_l, STAT=sv1)
	DEALLOCATE(zc2_la_l, STAT=sv1)
	DEALLOCATE(yc2_la_l, STAT=sv1)
	DEALLOCATE(S2_zc2_la_l, STAT=sv1)
	DEALLOCATE(S2_yc2_la_l, STAT=sv1)

	DEALLOCATE(S1_la_l, STAT=sv1)
	DEALLOCATE(zc1_la_l, STAT=sv1)
	DEALLOCATE(yc1_la_l, STAT=sv1)
	DEALLOCATE(S1_zc1_la_l, STAT=sv1)
	DEALLOCATE(S1_yc1_la_l, STAT=sv1)

	DEALLOCATE(l_la, STAT=sv1)
	DEALLOCATE(l_lo, STAT=sv1)

	DEALLOCATE(S3_lon, STAT=sv1)
	DEALLOCATE(zc3_lon, STAT=sv1)
	DEALLOCATE(yc3_lon, STAT=sv1)
	DEALLOCATE(S3_zc3_lon, STAT=sv1)
	DEALLOCATE(S3_yc3_lon, STAT=sv1)

	DEALLOCATE(S2_lon, STAT=sv1)
	DEALLOCATE(zc2_lon, STAT=sv1)
	DEALLOCATE(yc2_lon, STAT=sv1)
	DEALLOCATE(S2_zc2_lon, STAT=sv1)
	DEALLOCATE(S2_yc2_lon, STAT=sv1)

	DEALLOCATE(S3_lat, STAT=sv1)
	DEALLOCATE(zc3_lat, STAT=sv1)
	DEALLOCATE(yc3_lat, STAT=sv1)
	DEALLOCATE(S3_zc3_lat, STAT=sv1)
	DEALLOCATE(S3_yc3_lat, STAT=sv1)

	DEALLOCATE(S2_lat, STAT=sv1)
	DEALLOCATE(zc2_lat, STAT=sv1)
	DEALLOCATE(yc2_lat, STAT=sv1)
	DEALLOCATE(S2_zc2_lat, STAT=sv1)
	DEALLOCATE(S2_yc2_lat, STAT=sv1)

*******************************************************************************
* - calcul longueur de poutres

	i=0
	j=0
      do i=1,nn_val
	  do j=1,nn_val

	    if (i.ne.j) then
	      if (j.gt.i) then
	        if (lung(i,j).eq.1.d0) then
	lung(i,j)=0.d0
	lung(i,j)=SQRT((x_p(j)-x_p(i))**2.+(y_p(j)-y_p(i))**2.+
     *              (z_p(j)-z_p(i))**2.)
	lung(j,i)=0.d0
	lung(j,i)=lung(i,j)
	        elseif (lung(i,j).eq.0.d0) then
	lung(i,j)=0.d0
	lung(i,j)=0.
	lung(j,i)=0.d0
	lung(j,i)=lung(i,j)

	        endif
	      endif
	    endif

	  enddo
	enddo
	
*******************************************************************************
* - calcul angles theta et phi (pour le changement de repere)

	ALLOCATE(theta1(nr_noeuds,nr_noeuds))
	ALLOCATE(phi1(nr_noeuds,nr_noeuds))

	ALLOCATE(SIN_t(nr_noeuds,nr_noeuds))
	ALLOCATE(SIN_p(nr_noeuds,nr_noeuds))
	ALLOCATE(COS_t(nr_noeuds,nr_noeuds))
	ALLOCATE(COS_p(nr_noeuds,nr_noeuds))

*	print *,'OK - angles'
	
	theta1(1:nr_noeuds,1:nr_noeuds)=0.d0
	phi1(1:nr_noeuds,1:nr_noeuds)=0.d0

	SIN_t(1:nr_noeuds,1:nr_noeuds)=0.d0
	SIN_p(1:nr_noeuds,1:nr_noeuds)=0.d0
	COS_t(1:nr_noeuds,1:nr_noeuds)=0.d0
	COS_p(1:nr_noeuds,1:nr_noeuds)=0.d0

*	pause

	i3=0
	j3=0
	do i3=1,nn_val
	  do j3=1,nn_val
	
		if (i3.ne.j3) then
	      if (j3.gt.i3) then
	        if (lung(i3,j3).ne.0.d0) then

*	          print *,i3,j3

*               Poutre en 3D
		      if ((x_p(j3)-x_p(i3).ne.0.d0).and.(y_p(j3)-y_p(i3).ne.0.d0).and.
     *              (z_p(j3)-z_p(i3).ne.0.d0)) then

*				print *,'3D'
			
				theta_calcul=0.d0
				theta_calcul=ATAN(SQRT((x_p(j3)-x_p(i3))**2.+
     *                       (y_p(j3)-y_p(i3))**2.d0)/(z_p(j3)-z_p(i3)))
				phi_calcul=0.d0
				phi_calcul=ATAN(y_p(j3)-y_p(i3))/(x_p(j3)-x_p(i3))
          
*				cadranul 1 (x>0, y>0, z>0)
				if (((x_p(j3)-x_p(i3)).gt.0.d0).and.((y_p(j3)-y_p(i3)).gt.0.d0).and.
     *                  ((z_p(j3)-z_p(i3)).gt.0.d0)) then
					theta1(i3,j3)=Abs(theta_calcul)
					phi1(i3,j3)=Abs(phi_calcul)
				endif
            
*				cadranul 2 (x>0, y>0, z<0)
				if (((x_p(j3)-x_p(i3)).gt.0.d0).and.((y_p(j3)-y_p(i3)).gt.0.d0).and.
     *                  ((z_p(j3)-z_p(i3)).lt.0.d0)) then
					theta1(i3,j3)=Pi-Abs(theta_calcul)
					phi1(i3,j3)=Abs(phi_calcul)
				endif
            
*				cadranul 3 (x<0, y>0, z<0)
				if (((x_p(j3)-x_p(i3)).lt.0.d0).and.((y_p(j3)-y_p(i3)).gt.0.d0).and.
     *                  ((z_p(j3)-z_p(i3)).lt.0.d0)) then
					theta1(i3,j3)=Pi-Abs(theta_calcul)
					phi1(i3,j3)=Pi-Abs(phi_calcul)
				endif
            
*				cadranul 4 (x<0, y>0, z>0)
				if (((x_p(j3)-x_p(i3)).lt.0.d0).and.((y_p(j3)-y_p(i3)).gt.0.d0).and.
     *                  ((z_p(j3)-z_p(i3)).gt.0.d0)) then
					theta1(i3,j3)=Abs(theta_calcul)
					phi1(i3,j3)=Pi-Abs(phi_calcul)
				endif
            
*				cadranul 5 (x>0, y<0, z>0)
				if (((x_p(j3)-x_p(i3)).gt.0.d0).and.((y_p(j3)-y_p(i3)).lt.0.d0).and.
     *                  ((z_p(j3)-z_p(i3)).gt.0.d0)) then
					theta1(i3,j3)=Abs(theta_calcul)
					phi1(i3,j3)=-Abs(phi_calcul)
				endif
            
*				cadranul 6 (x>0, y<0, z<0)
				if (((x_p(j3)-x_p(i3)).gt.0.d0).and.((y_p(j3)-y_p(i3)).lt.0.d0).and.
     *                  ((z_p(j3)-z_p(i3)).lt.0.d0)) then
					theta1(i3,j3)=Pi-Abs(theta_calcul)
					phi1(i3,j3)=-Abs(phi_calcul)
				endif
            
*				cadranul 7 (x<0, y<0, z<0)
				if (((x_p(j3)-x_p(i3)).lt.0.d0).and.((y_p(j3)-y_p(i3)).lt.0.d0).and.
     *                  ((z_p(j3)-z_p(i3)).lt.0.d0)) then
					theta1(i3,j3)=Pi-Abs(theta_calcul)
					phi1(i3,j3)=-(Pi-Abs(phi_calcul))
				endif
            
*				cadranul 8 (x<0, y<0, z>0)
				if (((x_p(j3)-x_p(i3)).lt.0.d0).and.((y_p(j3)-y_p(i3)).lt.0.d0).and.
     *                  ((z_p(j3)-z_p(i3)).gt.0.d0)) then
					theta1(i3,j3)=Abs(theta_calcul)
					phi1(i3,j3)=-(Pi-Abs(phi_calcul))
				endif
			  endif


*			  Poutre en 2D (plane YoZ)
	          if ((x_p(j3)-x_p(i3)).eq.0.d0) then

*	          print *,'2D - plane YoZ'

				if (((y_p(j3)-y_p(i3)).ne.0.d0).and.((z_p(j3)-z_p(i3)).ne.0.d0))then

					theta_calcul=0.d0
					theta_calcul=ATAN(SQRT((x_p(j3)-x_p(i3))**2.d0+
     *                  (y_p(j3)-y_p(i3))**2.d0)/(z_p(j3)-z_p(i3)))

					if ((y_p(j3)-y_p(i3)).gt.0.d0) then
						phi1(i3,j3)=Pi/2.d0
					endif
					if ((y_p(j3)-y_p(i3)).lt.0.d0) then
						phi1(i3,j3)=-Pi/2.d0
					endif
					if ((z_p(j3)-z_p(i3)).gt.0.d0) then
						theta1(i3,j3)=Abs(theta_calcul);
					endif
					if ((z_p(j3)-z_p(i3)).lt.0.d0) then
						theta1(i3,j3)=Pi-Abs(theta_calcul)
					endif

			    endif
              
				if ((z_p(j3)-z_p(i3)).eq.0.d0) then
					theta1(i3,j3)=Pi/2.d0
					if ((y_p(j3)-y_p(i3)).gt.0.d0) then
						phi1(i3,j3)=Pi/2.d0
					endif
					if ((y_p(j3)-y_p(i3)).lt.0.d0) then
						phi1(i3,j3)=-Pi/2.d0
					endif
				endif
              
				if ((y_p(j3)-y_p(i3)).eq.0.d0) then
					phi1(i3,j3)=0.d0*Pi;
					if ((z_p(j3)-z_p(i3)).gt.0.d0) then
						theta1(i3,j3)=0.d0*Pi;
					endif
					if ((z_p(j3)-z_p(i3)).lt.0.d0) then
						theta1(i3,j3)=Pi
					endif
				endif
			  
			  endif


*			  Poutre en 2D (plane XoZ)
                if ((y_p(j3)-y_p(i3)).eq.0.d0) then
	          
*			  print *,'2D - plane XoZ'

				if (((x_p(j3)-x_p(i3)).ne.0.d0).and.((z_p(j3)-z_p(i3)).ne.0.d0))then

					theta_calcul=0.;
					theta_calcul=ATAN(SQRT((x_p(j3)-x_p(i3))**2.d0+
     *                  (y_p(j3)-y_p(i3))**2.d0)/(z_p(j3)-z_p(i3)))
                
					if ((x_p(j3)-x_p(i3)).gt.0.d0) then
						phi1(i3,j3)=0.d0
					endif
					if ((x_p(j3)-x_p(i3)).lt.0.d0) then
						phi1(i3,j3)=Pi
					endif
					if ((z_p(j3)-z_p(i3)).gt.0.d0) then
						theta1(i3,j3)=Abs(theta_calcul)
					endif
					if ((z_p(j3)-z_p(i3)).lt.0.d0) then
						theta1(i3,j3)=Pi-Abs(theta_calcul)
					endif
				endif
              
				if ((z_p(j3)-z_p(i3)).eq.0.d0) then
					theta1(i3,j3)=Pi/2.d0
					if ((x_p(j3)-x_p(i3)).gt.0.d0) then
						phi1(i3,j3)=0.d0*Pi
					endif
					if ((x_p(j3)-x_p(i3)).lt.0.d0) then
						phi1(i3,j3)=Pi
					endif
				endif
              
				if ((x_p(j3)-x_p(i3)).eq.0.d0) then
					phi1(i3,j3)=0.d0*Pi
					if ((z_p(j3)-z_p(i3)).gt.0.d0) then
						theta1(i3,j3)=0.*Pi
					endif
					if ((z_p(j3)-z_p(i3)).lt.0.d0) then
						theta1(i3,j3)=Pi
					endif
				endif
                
			  endif

*			  Poutre en 2D (plane XoY)
	          if ((z_p(j3)-z_p(i3)).eq.0.d0) then

*	          print *,'2D - plane XoY'

				theta1(i3,j3)=0.d0
				theta1(i3,j3)=Pi/2.d0
              
				if (((x_p(j3)-x_p(i3)).ne.0.d0).and.((y_p(j3)-y_p(i3)).ne.0.d0))then
					phi_calcul=0.0
					phi_calcul=ATAN((y_p(j3)-y_p(i3))/(x_p(j3)-x_p(i3)))
					if ((y_p(j3)-y_p(i3)).gt.0.d0) then
						if ((x_p(j3)-x_p(i3)).gt.0.d0) then
							phi1(i3,j3)=Abs(phi_calcul)
						endif
						if ((x_p(j3)-x_p(i3)).lt.0.d0) then
							phi1(i3,j3)=Pi-Abs(phi_calcul)
						endif
					endif
					if ((y_p(j3)-y_p(i3)).lt.0.d0) then
						if ((x_p(j3)-x_p(i3)).gt.0.d0) then
							phi1(i3,j3)=-Abs(phi_calcul)
						endif
						if ((x_p(j3)-x_p(i3)).lt.0.d0) then
							phi1(i3,j3)=-(Pi-Abs(phi_calcul))
						endif
					endif
				endif
              
				if ((y_p(j3)-y_p(i3)).eq.0.d0) then
					if ((x_p(j3)-x_p(i3)).gt.0.d0) then
						phi1(i3,j3)=0.d0*Pi
					endif
					if ((x_p(j3)-x_p(i3)).lt.0.d0) then
						phi1(i3,j3)=Pi
					endif
				endif
              
				if ((x_p(j3)-x_p(i3)).eq.0.d0) then
					if ((y_p(j3)-y_p(i3)).gt.0.d0) then
						phi1(i3,j3)=Pi/2.d0
					endif
					if ((y_p(j3)-y_p(i3)).lt.0.d0) then
						phi1(i3,j3)=-Pi/2.d0
					endif
				endif
                endif
				
	          theta1(j3,i3)=theta1(i3,j3)
	          phi1(j3,i3)=phi1(i3,j3)

			theta_deg=0.
			theta_deg=theta1(i3,j3)*180.d0/Pi

			phi_deg=0.
			phi_deg=phi1(i3,j3)*180.d0/Pi

*--------------------------------------------
			SIN_t(i3,j3)=SIN(theta1(i3,j3))
			COS_t(i3,j3)=COS(theta1(i3,j3))

			SIN_p(i3,j3)=SIN(phi1(i3,j3))
			COS_p(i3,j3)=COS(phi1(i3,j3))
			
*-------------------------------------------
			if (theta_deg.eq.90.d0) then
				SIN_t(i3,j3)=1.d0
				COS_t(i3,j3)=0.d0
			elseif (theta_deg.eq.180.d0) then
				SIN_t(i3,j3)=0.d0
				COS_t(i3,j3)=-1.d0
	        elseif (theta_deg.eq.270.d0) then
				SIN_t(i3,j3)=-1.d0
				COS_t(i3,j3)=0.d0
			elseif (theta_deg.eq.360.d0) then
				SIN_t(i3,j3)=0.d0
				COS_t(i3,j3)=1.d0
			endif
*-------------------------------------------
			if (theta_deg.eq.-90.d0) then
				SIN_t(i3,j3)=-1.d0
				COS_t(i3,j3)=0.d0
			elseif (theta_deg.eq.-180.d0) then
				SIN_t(i3,j3)=0.d0
				COS_t(i3,j3)=-1.d0
	        elseif (theta_deg.eq.-270.d0) then
				SIN_t(i3,j3)=1.d0
				COS_t(i3,j3)=0.d0
			elseif (theta_deg.eq.-360.d0) then
				SIN_t(i3,j3)=0.d0
				COS_t(i3,j3)=1.d0
			endif
*-------------------------------------------
			if (phi_deg.eq.90.d0) then
				SIN_p(i3,j3)=1.
				COS_p(i3,j3)=0.
			elseif (phi_deg.eq.180) then
				SIN_p(i3,j3)=0.
				COS_p(i3,j3)=-1.
	        elseif (phi_deg.eq.270) then
				SIN_p(i3,j3)=-1.
				COS_p(i3,j3)=0.
			elseif (phi_deg.eq.360.) then
				SIN_p(i3,j3)=0.
				COS_p(i3,j3)=1.
			endif
*--------------------------------------------
			if (phi_deg.eq.-90.) then
				SIN_p(i3,j3)=-1.d0
				COS_p(i3,j3)=0.d0
			elseif (phi_deg.eq.-180.d0) then
				SIN_p(i3,j3)=0.d0
				COS_p(i3,j3)=-1.d0
	        elseif (phi_deg.eq.-270.d0) then
				SIN_p(i3,j3)=1.d0
				COS_p(i3,j3)=0.d0
			elseif (phi_deg.eq.-360.d0) then
				SIN_p(i3,j3)=0.d0
				COS_p(i3,j3)=1.d0
			endif
*-------------------------------------------

			SIN_t(j3,i3)=SIN_t(i3,j3)
			COS_t(j3,i3)=COS_t(i3,j3)
			SIN_p(j3,i3)=SIN_p(i3,j3)
			COS_p(j3,i3)=COS_p(i3,j3)

*	        print *,'Pi:',Pi
*	        print *,'theta:',theta1(i3,j3)*180./Pi,' ','phi:',phi1(i3,j3)*180./Pi

	        endif
	      endif
	    endif

	  enddo
	enddo
	

	DEALLOCATE(x_p, STAT=sv2)
	DEALLOCATE(y_p, STAT=sv2)
	DEALLOCATE(z_p, STAT=sv2)

	DEALLOCATE(theta1, STAT=sv2)
	DEALLOCATE(phi1,   STAT=sv2)
	
*******************************************************************************
* -> calcul du nombre de dddl (prise en compte des conditions aux limites)
	
	ALLOCATE(poz(nn_val))
*	print *,'OK - poz'
	poz(1:nn_val)=0

	ddl_maxg=nn_val*ddl_maxn
*	print *,'ddl total:',ddl_maxg

	ALLOCATE(poz_ddl(ddl_maxg))
*	print *,'OK - poz_ddl'
	poz_ddl(1:ddl_maxg)=0

*	pause
	
	contor_poz=0
	contor_poz=1
	contor_poz_ddl=0
	ddl=0
	
	i11=0
	j11=0
	do i11=1,nn_val
	  contor_ddl=1
	  do j11=1,nn_val

		if (contor_ddl.lt.nn_val) then
		  if (i11.ne.j11) then
              if (lung(i11,j11).ne.0.d0) then

*			  print *,'poutre ',i11,j11
			  
			  if (t_l(i11,j11).eq.1) then

				ddl=ddl+0
				contor_ddl=contor_ddl+nn_val
				
				i44=0
				do i44=1,6
					poz_ddl(contor_poz_ddl+i44)=contor_poz_ddl+i44
				enddo
				
				contor_ddl=contor_ddl+nn_val
				contor_poz_ddl=contor_poz_ddl+6

			  elseif (t_l(i11,j11).eq.2) then

				ddl=ddl+6
				poz(i11)=contor_poz
				
				contor_poz=contor_poz+1
				contor_ddl=contor_ddl+nn_val
				contor_poz_ddl=contor_poz_ddl+6

			  elseif (t_l(i11,j11).eq.31) then
				
				if ((i11.eq.1).and.(t_bc.eq.5)) then
					ddl=ddl+2
					poz(i11)=contor_poz
					
					poz_ddl(contor_poz_ddl+1)=contor_poz_ddl+1
					poz_ddl(contor_poz_ddl+2)=contor_poz_ddl+2
					poz_ddl(contor_poz_ddl+3)=contor_poz_ddl+3
					poz_ddl(contor_poz_ddl+5)=contor_poz_ddl+5

				elseif ((i11.eq.nn_val).eq.(t_bc.eq.5)) then
					ddl=ddl+3
					poz(i11)=contor_poz
				
					poz_ddl(contor_poz_ddl+2)=contor_poz_ddl+2
					poz_ddl(contor_poz_ddl+5)=contor_poz_ddl+5
					poz_ddl(contor_poz_ddl+6)=contor_poz_ddl+6

				else
					ddl=ddl+4
					poz(i11)=contor_poz
				
					poz_ddl(contor_poz_ddl+2)=contor_poz_ddl+2
					poz_ddl(contor_poz_ddl+5)=contor_poz_ddl+5
				endif

				contor_poz=contor_poz+1
				contor_ddl=contor_ddl+nn_val
				contor_poz_ddl=contor_poz_ddl+6

			  elseif (t_l(i11,j11).eq.33) then

				ddl=ddl+4
				poz(i11)=contor_poz
				
				poz_ddl(contor_poz_ddl+2)=contor_poz_ddl+2
				poz_ddl(contor_poz_ddl+4)=contor_poz_ddl+4

				contor_poz=contor_poz+1
				contor_ddl=contor_ddl+nn_val
				contor_poz_ddl=contor_poz_ddl+6

			  elseif (t_l(i11,j11).eq.44) then

				ddl=ddl+3
				poz(i11)=contor_poz
				
				poz_ddl(contor_poz_ddl+1)=contor_poz_ddl+1
				poz_ddl(contor_poz_ddl+5)=contor_poz_ddl+5
				poz_ddl(contor_poz_ddl+6)=contor_poz_ddl+6

				contor_poz=contor_poz+1
				contor_ddl=contor_ddl+nn_val
				contor_poz_ddl=contor_poz_ddl+6

			  elseif (t_l(i11,j11).eq.55) then

				ddl=ddl+3
				poz(i11)=contor_poz
				
				poz_ddl(contor_poz_ddl+3)=contor_poz_ddl+3
				poz_ddl(contor_poz_ddl+4)=contor_poz_ddl+4
				poz_ddl(contor_poz_ddl+6)=contor_poz_ddl+6

				contor_poz=contor_poz+1
				contor_ddl=contor_ddl+nn_val
				contor_poz_ddl=contor_poz_ddl+6

			  endif

		    endif
	      endif
	    endif

	  enddo
	enddo

	print *,'Number of DOF:',ddl

	DEALLOCATE(poz, STAT=sv2)
	DEALLOCATE(t_l, STAT=sv2)

*******************************************************************************
* -> allocation variables (matrices transitoire et matrice rigidite globale)

******

	ALLOCATE(KE(np_val,12,12))
*	print *,'OK - KE'

	ALLOCATE(ME(np_val,12,12))
*	print *,'OK - ME'

	ALLOCATE(IPIV1(12))
*	print *,'OK - IPIV1'

	ALLOCATE(WORK4(12))
*	print *,'OK - WORK4'
	
	KE(1:np_val,1:12,1:12)=0.d0
	ME(1:np_val,1:12,1:12)=0.d0
	
	IPIV1(1:12)=0
	WORK4(1:12)=0
*******************************************************************************
!	ordre déplacements pour un noeud
!	Ux
!	Uy
!	Uz
!	Phix
!	Phiz
!	Phiy

* - ecriture des matrices de rigidite et de masse locales

	i2=0
	j2=0
	c_beams=0

	do i2=1,nn_val
	  do j2=1,nn_val
	
		if (i2.ne.j2) then
	      if (j2.gt.i2) then
	        if (lung(i2,j2).ne.0.d0) then
	
*			print *,i2,j2
			c_beams=c_beams+1

*------------------------------------------------------------------------------
*     Matrice de rigidité élémentaire (KE=MRM(omega=0))

******	
	KE(c_beams,1,1)=    E1*S1(i2,j2)/lung(i2,j2)
	KE(c_beams,1,2:6)=0.d0
	KE(c_beams,1,7)=    -E1*S1(i2,j2)/lung(i2,j2)
	KE(c_beams,1,8:12)=0.d0
******
	KE(c_beams,2,1)=0.d0
	KE(c_beams,2,2)=    12.d0*E1*Iz1(i2,j2)/lung(i2,j2)**3.d0
	KE(c_beams,2,3)=0.d0
	KE(c_beams,2,4)=0.d0
	KE(c_beams,2,5)=    6.d0*E1*Iz1(i2,j2)/lung(i2,j2)**2.d0
	KE(c_beams,2,6)=0.d0
	KE(c_beams,2,7)=0.d0
	KE(c_beams,2,8)=    -12.d0*E1*Iz1(i2,j2)/lung(i2,j2)**3.d0
	KE(c_beams,2,9)=0.d0
	KE(c_beams,2,10)=0.d0
	KE(c_beams,2,11)=   6.d0*E1*Iz1(i2,j2)/lung(i2,j2)**2.d0
	KE(c_beams,2,12)=0.d0
******	
	KE(c_beams,3,1)=0.d0
	KE(c_beams,3,2)=0.d0
	KE(c_beams,3,3)=    12.d0*E1*Iy1(i2,j2)/lung(i2,j2)**3.d0
	KE(c_beams,3,4)=0.d0
	KE(c_beams,3,5)=0.d0
	KE(c_beams,3,6)=    6.d0*E1*Iy1(i2,j2)/lung(i2,j2)**2.d0
	KE(c_beams,3,7)=0.d0
	KE(c_beams,3,8)=0.d0
	KE(c_beams,3,9)=    -12.d0*E1*Iy1(i2,j2)/lung(i2,j2)**3.d0
	KE(c_beams,3,10)=0.d0
	KE(c_beams,3,11)=0.d0
	KE(c_beams,3,12)=   6.d0*E1*Iy1(i2,j2)/lung(i2,j2)**2.d0
******
	KE(c_beams,4,1:3)=0.d0
	KE(c_beams,4,4)=      G1*J1(i2,j2)/lung(i2,j2)
	KE(c_beams,4,5:9)=0.d0
	KE(c_beams,4,10)=     -G1*J1(i2,j2)/lung(i2,j2)
	KE(c_beams,4,11:12)=0.d0
******
	KE(c_beams,5,1)=0.d0
	KE(c_beams,5,2)=    6.d0*E1*Iz1(i2,j2)/lung(i2,j2)**2.d0
	KE(c_beams,5,3)=0.d0
	KE(c_beams,5,4)=0.d0
	KE(c_beams,5,5)=    4.d0*E1*Iz1(i2,j2)/lung(i2,j2)
	KE(c_beams,5,6)=0.d0
	KE(c_beams,5,7)=0.d0
	KE(c_beams,5,8)=    -6.d0*E1*Iz1(i2,j2)/lung(i2,j2)**2.d0
	KE(c_beams,5,9)=0.d0
	KE(c_beams,5,10)=0.d0
	KE(c_beams,5,11)=   2.d0*E1*Iz1(i2,j2)/lung(i2,j2)
	KE(c_beams,5,12)=0.d0
******
	KE(c_beams,6,1)=0.d0
	KE(c_beams,6,2)=0.d0
	KE(c_beams,6,3)=    6.d0*E1*Iy1(i2,j2)/lung(i2,j2)**2.d0
	KE(c_beams,6,4)=0.d0
	KE(c_beams,6,5)=0.d0
	KE(c_beams,6,6)=    4.d0*E1*Iy1(i2,j2)/lung(i2,j2)
	KE(c_beams,6,7)=0.d0
	KE(c_beams,6,8)=0.d0
	KE(c_beams,6,9)=    -6.d0*E1*Iy1(i2,j2)/lung(i2,j2)**2.d0
	KE(c_beams,6,10)=0.d0
	KE(c_beams,6,11)=0.d0
	KE(c_beams,6,12)=   2.d0*E1*Iy1(i2,j2)/lung(i2,j2)

******

	KE(c_beams,7,1)=    -E1*S1(i2,j2)/lung(i2,j2)
	KE(c_beams,7,2:6)=0.d0
	KE(c_beams,7,7)=     E1*S1(i2,j2)/lung(i2,j2)
	KE(c_beams,7,8:12)=0.d0
******
	KE(c_beams,8,1)=0.d0
	KE(c_beams,8,2)=    -12.d0*E1*Iz1(i2,j2)/lung(i2,j2)**3.d0
	KE(c_beams,8,3)=0.d0
	KE(c_beams,8,4)=0.d0
	KE(c_beams,8,5)=    -6.d0*E1*Iz1(i2,j2)/lung(i2,j2)**2.d0
	KE(c_beams,8,6)=0.d0
	KE(c_beams,8,7)=0.d0
	KE(c_beams,8,8)=    12.d0*E1*Iz1(i2,j2)/lung(i2,j2)**3.d0
	KE(c_beams,8,9)=0.d0
	KE(c_beams,8,10)=0.d0
	KE(c_beams,8,11)=   -6.d0*E1*Iz1(i2,j2)/lung(i2,j2)**2.d0
	KE(c_beams,8,12)=0.d0
******	
	KE(c_beams,9,1)=0.d0
	KE(c_beams,9,2)=0.d0
	KE(c_beams,9,3)=    -12.d0*E1*Iy1(i2,j2)/lung(i2,j2)**3.d0
	KE(c_beams,9,4)=0.d0
	KE(c_beams,9,5)=0.d0
	KE(c_beams,9,6)=    -6.d0*E1*Iy1(i2,j2)/lung(i2,j2)**2.d0
	KE(c_beams,9,7)=0.d0
	KE(c_beams,9,8)=0.d0
	KE(c_beams,9,9)=    12.d0*E1*Iy1(i2,j2)/lung(i2,j2)**3.d0
	KE(c_beams,9,10)=0.d0
	KE(c_beams,9,11)=0.d0
	KE(c_beams,9,12)=   -6.d0*E1*Iy1(i2,j2)/lung(i2,j2)**2.d0
******
	KE(c_beams,10,1:3)=0.d0
	KE(c_beams,10,4)=      -G1*J1(i2,j2)/lung(i2,j2)
	KE(c_beams,10,5:9)=0.d0
	KE(c_beams,10,10)=     G1*J1(i2,j2)/lung(i2,j2)
	KE(c_beams,10,11:12)=0.d0
******
	KE(c_beams,11,1)=0.d0
	KE(c_beams,11,2)=    6.d0*E1*Iz1(i2,j2)/lung(i2,j2)**2.d0
	KE(c_beams,11,3)=0.d0
	KE(c_beams,11,4)=0.d0
	KE(c_beams,11,5)=    2.d0*E1*Iz1(i2,j2)/lung(i2,j2)
	KE(c_beams,11,6)=0.d0
	KE(c_beams,11,7)=0.d0
	KE(c_beams,11,8)=    -6.d0*E1*Iz1(i2,j2)/lung(i2,j2)**2.d0
	KE(c_beams,11,9)=0.d0
	KE(c_beams,11,10)=0.d0
	KE(c_beams,11,11)=   4.d0*E1*Iz1(i2,j2)/lung(i2,j2)
	KE(c_beams,11,12)=0.d0
******
	KE(c_beams,12,1)=0.d0
	KE(c_beams,12,2)=0.d0
	KE(c_beams,12,3)=    6.d0*E1*Iy1(i2,j2)/lung(i2,j2)**2.d0
	KE(c_beams,12,4)=0.d0
	KE(c_beams,12,5)=0.d0
	KE(c_beams,12,6)=    2.d0*E1*Iy1(i2,j2)/lung(i2,j2)
	KE(c_beams,12,7)=0.d0
	KE(c_beams,12,8)=0.d0
	KE(c_beams,12,9)=    -6.d0*E1*Iy1(i2,j2)/lung(i2,j2)**2.d0
	KE(c_beams,12,10)=0.d0
	KE(c_beams,12,11)=0.d0
	KE(c_beams,12,12)=   4.d0*E1*Iy1(i2,j2)/lung(i2,j2)

*------------------------------------------------------------------------------
*     Matrice de masse élémentaire (ME=d2MRM(omega)/domega2 quand omega=0)

******
	ME(c_beams,1,1)=    (1.d0/3.d0)*S1(i2,j2)*rho1*lung(i2,j2)
	ME(c_beams,1,2:6)=0.d0
	ME(c_beams,1,7)=    (1.d0/6.d0)*S1(i2,j2)*rho1*lung(i2,j2)
	ME(c_beams,1,8:12)=0.d0
******
	ME(c_beams,2,1)=0.d0
	ME(c_beams,2,2)=    (13.d0/35.d0)*S1(i2,j2)*rho1*lung(i2,j2)
	ME(c_beams,2,3)=0.d0
	ME(c_beams,2,4)=0.d0
	ME(c_beams,2,5)=    (11.d0/210.d0)*S1(i2,j2)*rho1*lung(i2,j2)**2.d0
	ME(c_beams,2,6)=0.d0
	ME(c_beams,2,7)=0.d0
	ME(c_beams,2,8)=    (9.d0/70.d0)*S1(i2,j2)*rho1*lung(i2,j2)
	ME(c_beams,2,9)=0.d0
	ME(c_beams,2,10)=0.d0
	ME(c_beams,2,11)=   (-13.d0/420.d0)*S1(i2,j2)*rho1*lung(i2,j2)**2.d0
	ME(c_beams,2,12)=0.d0
******	
	ME(c_beams,3,1)=0.d0
	ME(c_beams,3,2)=0.d0
	ME(c_beams,3,3)=    (13.d0/35.d0)*S1(i2,j2)*rho1*lung(i2,j2)
	ME(c_beams,3,4)=0.d0
	ME(c_beams,3,5)=0.d0
	ME(c_beams,3,6)=    (11.d0/210.d0)*S1(i2,j2)*rho1*lung(i2,j2)**2.d0
	ME(c_beams,3,7)=0.d0
	ME(c_beams,3,8)=0.d0
	ME(c_beams,3,9)=    (9.d0/70.d0)*S1(i2,j2)*rho1*lung(i2,j2)
	ME(c_beams,3,10)=0.d0
	ME(c_beams,3,11)=0.d0
	ME(c_beams,3,12)=   (-13.d0/420.d0)*S1(i2,j2)*rho1*lung(i2,j2)**2.d0
******

	ME(c_beams,4,1:3)=0.d0
	ME(c_beams,4,4)=  (1.d0/3.d0)*rho1*(Iz1(i2,j2)+Iy1(i2,j2))*lung(i2,j2)
	ME(c_beams,4,5:9)=0.d0
	ME(c_beams,4,10)= (1.d0/6.d0)*rho1*(Iz1(i2,j2)+Iy1(i2,j2))*lung(i2,j2)
	ME(c_beams,4,11:12)=0.d0
******
	ME(c_beams,5,1)=0.d0
	ME(c_beams,5,2)=    (11.d0/210.d0)*S1(i2,j2)*rho1*lung(i2,j2)**2.d0
	ME(c_beams,5,3)=0.d0
	ME(c_beams,5,4)=0.d0
	ME(c_beams,5,5)=    (1.d0/105.)*S1(i2,j2)*rho1*lung(i2,j2)**3.d0
	ME(c_beams,5,6)=0.d0
	ME(c_beams,5,7)=0.d0
	ME(c_beams,5,8)=    (13.d0/420.d0)*S1(i2,j2)*rho1*lung(i2,j2)**2.d0
	ME(c_beams,5,9)=0.d0
	ME(c_beams,5,10)=0.d0
	ME(c_beams,5,11)=   (-1.d0/140.d0)*S1(i2,j2)*rho1*lung(i2,j2)**3.d0
	ME(c_beams,5,12)=0.d0
******
	ME(c_beams,6,1)=0.d0
	ME(c_beams,6,2)=0.d0
	ME(c_beams,6,3)=    (11.d0/210.d0)*S1(i2,j2)*rho1*lung(i2,j2)**2.d0
	ME(c_beams,6,4)=0.d0
	ME(c_beams,6,5)=0.d0
	ME(c_beams,6,6)=    (1.d0/105.)*S1(i2,j2)*rho1*lung(i2,j2)**3.d0
	ME(c_beams,6,7)=0.d0
	ME(c_beams,6,8)=0.d0
	ME(c_beams,6,9)=    (13.d0/420.d0)*S1(i2,j2)*rho1*lung(i2,j2)**2.d0
	ME(c_beams,6,10)=0.d0
	ME(c_beams,6,11)=0.d0
	ME(c_beams,6,12)=   (-1.d0/140.d0)*S1(i2,j2)*rho1*lung(i2,j2)**3.d0

******

	ME(c_beams,7,1)=    (1.d0/6.d0)*S1(i2,j2)*rho1*lung(i2,j2)
	ME(c_beams,7,2:6)=0.d0
	ME(c_beams,7,7)=    (1.d0/3.d0)*S1(i2,j2)*rho1*lung(i2,j2)
	ME(c_beams,7,8:12)=0.d0
******
	ME(c_beams,8,1)=0.d0
	ME(c_beams,8,2)=    (9.d0/70.d0)*S1(i2,j2)*rho1*lung(i2,j2)
	ME(c_beams,8,3)=0.d0
	ME(c_beams,8,4)=0.d0
	ME(c_beams,8,5)=    (13.d0/420.d0)*S1(i2,j2)*rho1*lung(i2,j2)**2.d0
	ME(c_beams,8,6)=0.d0
	ME(c_beams,8,7)=0.d0
	ME(c_beams,8,8)=    (13.d0/35.d0)*S1(i2,j2)*rho1*lung(i2,j2)
	ME(c_beams,8,9)=0.d0
	ME(c_beams,8,10)=0.d0
	ME(c_beams,8,11)=   (-11.d0/210.d0)*S1(i2,j2)*rho1*lung(i2,j2)**2.d0
	ME(c_beams,8,12)=0.d0
******	
	ME(c_beams,9,1)=0.d0
	ME(c_beams,9,2)=0.d0
	ME(c_beams,9,3)=    (9.d0/70.d0)*S1(i2,j2)*rho1*lung(i2,j2)
	ME(c_beams,9,4)=0.d0
	ME(c_beams,9,5)=0.d0
	ME(c_beams,9,6)=    (13.d0/420.d0)*S1(i2,j2)*rho1*lung(i2,j2)**2.d0
	ME(c_beams,9,7)=0.d0
	ME(c_beams,9,8)=0.d0
	ME(c_beams,9,9)=    (13.d0/35.d0)*S1(i2,j2)*rho1*lung(i2,j2)
	ME(c_beams,9,10)=0.d0
	ME(c_beams,9,11)=0.d0
	ME(c_beams,9,12)=   (-11.d0/210.d0)*S1(i2,j2)*rho1*lung(i2,j2)**2.d0
******
	ME(c_beams,10,1:3)=0.d0
	ME(c_beams,10,4)=(1.d0/6.d0)*rho1*(Iz1(i2,j2)+Iy1(i2,j2))*lung(i2,j2)
	ME(c_beams,10,5:9)=0.d0
	ME(c_beams,10,10)=(1.d0/3.d0)*rho1*(Iz1(i2,j2)+Iy1(i2,j2))*lung(i2,j2)
	ME(c_beams,10,11:12)=0.d0
******
	ME(c_beams,11,1)=0.d0
	ME(c_beams,11,2)=    (-13.d0/420.d0)*S1(i2,j2)*rho1*lung(i2,j2)**2.d0
	ME(c_beams,11,3)=0.d0
	ME(c_beams,11,4)=0.d0
	ME(c_beams,11,5)=    (-1.d0/140.d0)*S1(i2,j2)*rho1*lung(i2,j2)**3.d0
	ME(c_beams,11,6)=0.d0
	ME(c_beams,11,7)=0.d0
	ME(c_beams,11,8)=    (-11.d0/210.d0)*S1(i2,j2)*rho1*lung(i2,j2)**2.d0
	ME(c_beams,11,9)=0.d0
	ME(c_beams,11,10)=0.d0
	ME(c_beams,11,11)=   (1.d0/105.d0)*S1(i2,j2)*rho1*lung(i2,j2)**3.d0
	ME(c_beams,11,12)=0.d0
******
	ME(c_beams,12,1)=0.d0
	ME(c_beams,12,2)=0.d0
	ME(c_beams,12,3)=    (-13.d0/420.d0)*S1(i2,j2)*rho1*lung(i2,j2)**2.d0
	ME(c_beams,12,4)=0.d0
	ME(c_beams,12,5)=0.d0
	ME(c_beams,12,6)=    (-1.d0/140.d0)*S1(i2,j2)*rho1*lung(i2,j2)**3.d0
	ME(c_beams,12,7)=0.d0
	ME(c_beams,12,8)=0.d0
	ME(c_beams,12,9)=    (-11.d0/210.d0)*S1(i2,j2)*rho1*lung(i2,j2)**2.d0
	ME(c_beams,12,10)=0.d0
	ME(c_beams,12,11)=0.d0
	ME(c_beams,12,12)=   (1.d0/105.d0)*S1(i2,j2)*rho1*lung(i2,j2)**3.d0
	
*------------------------------------------------------------------------------

	        endif
	      endif
	    endif

	  enddo
	enddo

*	print *, 'Temps calcul 2:', secnds(T1)
	
	DEALLOCATE(S1,  STAT=sv2)
	DEALLOCATE(J1,  STAT=sv2)
	DEALLOCATE(Iz1, STAT=sv2)
	DEALLOCATE(Iy1, STAT=sv2)

*	pause
*******************************************************************************
* - creation matrice generale de changement de repere

*	ALLOCATE(K_TEM(nn_val,nn_val,6,12))
*	ALLOCATE(M_TEM(nn_val,nn_val,6,12))

	ALLOCATE(K_TEM(np_val,12,12))
*	print *,'OK - K_TEM'

	ALLOCATE(M_TEM(np_val,12,12))
*	print *,'OK - M_TEM'
	
	ALLOCATE(c_beams5(nn_val,nn_val))
	c_beams5(1:nn_val,1:nn_val)=0
	
	K_TEM(1:np_val,1:12,1:12)=0.d0
	M_TEM(1:np_val,1:12,1:12)=0.d0

******

	i4=0
	j4=0
	c_beams2=0

	do i4=1,nn_val
	  do j4=1,nn_val
	
		if (i4.ne.j4) then
	      if (j4.gt.i4) then
	        if (lung(i4,j4).ne.0.d0) then
	
*			print *,i4,j4

			c_beams2=c_beams2+1
			c_beams5(i4,j4)=c_beams2
			c_beams5(j4,i4)=-c_beams2

	MChrep(1:12,1:12)=0.d0
******
	MChrep(1,1)=SIN_t(i4,j4)*COS_p(i4,j4)
	MChrep(1,2)=(-1.d0)*SIN_p(i4,j4)
	MChrep(1,3)=(-1.d0)*COS_t(i4,j4)*COS_p(i4,j4)
	MChrep(1,4:12)=0.d0

	MChrep(2,1)=SIN_t(i4,j4)*SIN_p(i4,j4)
	MChrep(2,2)=COS_p(i4,j4)
	MChrep(2,3)=(-1.d0)*COS_t(i4,j4)*SIN_p(i4,j4)
	MChrep(2,4:12)=0.d0

	MChrep(3,1)=COS_t(i4,j4)
	MChrep(3,2)=0.d0
	MChrep(3,3)=SIN_t(i4,j4)
	MChrep(3,4:12)=0.d0
******
	MChrep(4,1:3)=0.d0
	MChrep(4,4)=SIN_t(i4,j4)*COS_p(i4,j4)
	MChrep(4,5)=(-1.d0)*COS_t(i4,j4)*COS_p(i4,j4)
	MChrep(4,6)=(-1.d0)*SIN_p(i4,j4)
	MChrep(4,7:12)=0.d0

	MChrep(5,1:3)=0.d0
	MChrep(5,4)=COS_t(i4,j4)
	MChrep(5,5)=SIN_t(i4,j4)
	MChrep(5,6)=0.d0
	MChrep(5,7:12)=0.d0

	MChrep(6,1:3)=0.d0
	MChrep(6,4)=SIN_t(i4,j4)*SIN_p(i4,j4)
	MChrep(6,5)=(-1.d0)*COS_t(i4,j4)*SIN_p(i4,j4)
	MChrep(6,6)=COS_p(i4,j4)
	MChrep(6,7:12)=0.d0
******
	MChrep(7,1:6)=0.d0
	MChrep(7,7)=SIN_t(i4,j4)*COS_p(i4,j4)
	MChrep(7,8)=(-1.d0)*SIN_p(i4,j4)
	MChrep(7,9)=(-1.d0)*COS_t(i4,j4)*COS_p(i4,j4)
	MChrep(7,10:12)=0.d0

	MChrep(8,1:6)=0.d0
	MChrep(8,7)=SIN_t(i4,j4)*SIN_p(i4,j4)
	MChrep(8,8)=COS_p(i4,j4)
	MChrep(8,9)=(-1.d0)*COS_t(i4,j4)*SIN_p(i4,j4)
	MChrep(8,10:12)=0.d0

	MChrep(9,1:6)=0.d0
	MChrep(9,7)=COS_t(i4,j4)
	MChrep(9,8)=0.d0
	MChrep(9,9)=SIN_t(i4,j4)
	MChrep(9,10:12)=0.d0
******
	MChrep(10,1:9)=0.d0
	MChrep(10,10)=SIN_t(i4,j4)*COS_p(i4,j4)
	MChrep(10,11)=(-1.d0)*COS_t(i4,j4)*COS_p(i4,j4)
	MChrep(10,12)=(-1.d0)*SIN_p(i4,j4)

	MChrep(11,1:9)=0.d0
	MChrep(11,10)=COS_t(i4,j4)
	MChrep(11,11)=SIN_t(i4,j4)
	MChrep(11,12)=0.d0

	MChrep(12,1:9)=0.d0
	MChrep(12,10)=SIN_t(i4,j4)*SIN_p(i4,j4)
	MChrep(12,11)=(-1.d0)*COS_t(i4,j4)*SIN_p(i4,j4)
	MChrep(12,12)=COS_p(i4,j4)
	
*******************************************************************************
* -> calcul inverse matrice generale de changement de repere

	IMChrep(1:12,1:12)=0.d0
	IMChrep(1:12,1:12)=MChrep(1:12,1:12)

	CALL DGETRF(12, 12, IMChrep, 12, IPIV1, INFO3)
	if (INFO3.ne.0) then
		print *,'SGETRF_INFO3=',INFO3
	    pause
	endif

      CALL DGETRI(12, IMChrep, 12, IPIV1, WORK4, 12, INFO4)
	if (INFO4.ne.0) then
		print *,'SGETRI_INFO4=',INFO4
	    pause
	endif

*******************************************************************************
* -> changement de repere pour les matrices de rigidite et de masse
		
*	    print *,'c_beams2:',c_beams2
		
		K3(1:12,1:12)=0.d0
		K3(1:12,1:12)=KE(c_beams2,1:12,1:12)

		GM_K1(1:12,1:12)=0.d0
	    CALL DGEMM('N','N',12,12,12,1.d0,K3,12,IMChrep,12,0.d0,GM_K1,12)
*		GM_K1=MATMUL(K3,IMChrep)

		GM_K(1:12,1:12)=0.d0
	    CALL DGEMM('N','N',12,12,12,1.d0,MChrep,12,GM_K1,12,0.d0,GM_K,12)
*		GM_K=MATMUL(MChrep,GM_K1)

*--------------------------------------
*100	FORMAT(E,E,E,E,E,E,E,E,E,E,E,E)
*	
*	if ((i4.eq.1).and.(j4.eq.13))then
*		open(unit=120,file='aaaaaaaaa.dat')
*		i=0
*		do i=1,12
*		write(120,100) K3(i,1),K3(i,2),K3(i,3),K3(i,4), K3(i,5), K3(i,6),
*    &            	   K3(i,7),K3(i,8),K3(i,9),K3(i,10),K3(i,11),K3(i,12)
*
*		enddo 
*		close(120)
*	endif
*	print *,'valoare:',KE(c_beams2,1,1)
*	print *,'am scris fisierul'
*	pause
*-----------------------------------------
******

		M3(1:12,1:12)=0.d0
		M3(1:12,1:12)=ME(c_beams2,1:12,1:12)

		GM_M1(1:12,1:12)=0.d0
		CALL DGEMM('N','N',12,12,12,1.d0,M3,12,IMChrep,12,0.d0,GM_M1,12)
*		GM_M1=MATMUL(M3,IMChrep)

		GM_M(1:12,1:12)=0.d0
		CALL DGEMM('N','N',12,12,12,1.d0,MChrep,12,GM_M1,12,0.d0,GM_M,12)
*		GM_M=MATMUL(MChrep,GM_M1)

*******************************************************************************
* -> convention de signe

	
		K_TEM(c_beams2,1,1:12)= GM_K(1,1:12)/coef1
		K_TEM(c_beams2,2,1:12)= GM_K(2,1:12)/coef2
		K_TEM(c_beams2,3,1:12)= GM_K(3,1:12)/coef3
		K_TEM(c_beams2,4,1:12)= GM_K(4,1:12)/coef4
		K_TEM(c_beams2,5,1:12)= GM_K(5,1:12)/coef5
		K_TEM(c_beams2,6,1:12)= GM_K(6,1:12)/coef6

		K_TEM(c_beams2,7,1:12)= GM_K(7,1:12)/coef1
		K_TEM(c_beams2,8,1:12)= GM_K(8,1:12)/coef2
		K_TEM(c_beams2,9,1:12)= GM_K(9,1:12)/coef3
		K_TEM(c_beams2,10,1:12)=GM_K(10,1:12)/coef4
		K_TEM(c_beams2,11,1:12)=GM_K(11,1:12)/coef5
		K_TEM(c_beams2,12,1:12)=GM_K(12,1:12)/coef6

*		K_TEM(i4,j4,1,1:12)= GM_K(1,1:12)/coef1
*		K_TEM(i4,j4,2,1:12)= GM_K(2,1:12)/coef2
*		K_TEM(i4,j4,3,1:12)= GM_K(3,1:12)/coef3
*		K_TEM(i4,j4,4,1:12)= GM_K(4,1:12)/coef4
*		K_TEM(i4,j4,5,1:12)= GM_K(5,1:12)/coef5
*		K_TEM(i4,j4,6,1:12)= GM_K(6,1:12)/coef6
*
*		K_TEM(j4,i4,1,1:12)=-GM_K(7,1:12)/coef1
*		K_TEM(j4,i4,2,1:12)=-GM_K(8,1:12)/coef2
*		K_TEM(j4,i4,3,1:12)=-GM_K(9,1:12)/coef3
*		K_TEM(j4,i4,4,1:12)=-GM_K(10,1:12)/coef4
*		K_TEM(j4,i4,5,1:12)=-GM_K(11,1:12)/coef5
*		K_TEM(j4,i4,6,1:12)=-GM_K(12,1:12)/coef6

******

		M_TEM(c_beams2,1,1:12)= GM_M(1,1:12)/coef1
		M_TEM(c_beams2,2,1:12)= GM_M(2,1:12)/coef2
		M_TEM(c_beams2,3,1:12)= GM_M(3,1:12)/coef3
		M_TEM(c_beams2,4,1:12)= GM_M(4,1:12)/coef4
		M_TEM(c_beams2,5,1:12)= GM_M(5,1:12)/coef5
		M_TEM(c_beams2,6,1:12)= GM_M(6,1:12)/coef6

		M_TEM(c_beams2,7,1:12)= GM_M(7,1:12)/coef1
		M_TEM(c_beams2,8,1:12)= GM_M(8,1:12)/coef2
		M_TEM(c_beams2,9,1:12)= GM_M(9,1:12)/coef3
		M_TEM(c_beams2,10,1:12)=GM_M(10,1:12)/coef4
		M_TEM(c_beams2,11,1:12)=GM_M(11,1:12)/coef5
		M_TEM(c_beams2,12,1:12)=GM_M(12,1:12)/coef6

*		M_TEM(i4,j4,1,1:12)= GM_M(1,1:12)/coef1
*		M_TEM(i4,j4,2,1:12)= GM_M(2,1:12)/coef2
*		M_TEM(i4,j4,3,1:12)= GM_M(3,1:12)/coef3
*		M_TEM(i4,j4,4,1:12)= GM_M(4,1:12)/coef4
*		M_TEM(i4,j4,5,1:12)= GM_M(5,1:12)/coef5
*		M_TEM(i4,j4,6,1:12)= GM_M(6,1:12)/coef6
*
*		M_TEM(j4,i4,1,1:12)=-GM_M(7,1:12)/coef1
*		M_TEM(j4,i4,2,1:12)=-GM_M(8,1:12)/coef2
*		M_TEM(j4,i4,3,1:12)=-GM_M(9,1:12)/coef3
*		M_TEM(j4,i4,4,1:12)=-GM_M(10,1:12)/coef4
*		M_TEM(j4,i4,5,1:12)=-GM_M(11,1:12)/coef5
*		M_TEM(j4,i4,6,1:12)=-GM_M(12,1:12)/coef6

	        endif
	      endif
	    endif

	  enddo
	enddo
	
	DEALLOCATE(SIN_t, STAT=sv2)
	DEALLOCATE(COS_t, STAT=sv2)
	DEALLOCATE(SIN_p, STAT=sv2)
	DEALLOCATE(COS_p, STAT=sv2)

	DEALLOCATE(KE,    STAT=sv3)
	DEALLOCATE(ME,    STAT=sv3)
	DEALLOCATE(IPIV1, STAT=sv3)
	DEALLOCATE(WORK4, STAT=sv3)

*	print *, 'Temps calcul 3:', secnds(T1)
*	print *,'c_beams2:',c_beams2

*	pause

*******************************************************************************
* -> ecriture de la matrice globale de rigidité sans C.L. (ddl_maxg x ddl_maxg)

	ALLOCATE(GM_K_FT(ddl_maxg,ddl_maxg))
*	print *,'OK - GM_K_FT'
	
	GM_K_FT(1:ddl_maxg,1:ddl_maxg)=0.d0

*	ALLOCATE(MKpN(nn_val,4,ddl_maxn,ddl_maxg))
	ALLOCATE(MKpN(6,ddl_maxg))
*	print *,'OK - MKpN'
	
*	pause
	
	i5=0
	j5=0
	b_dl=0
*	b_dl_c=0

	do i5=1,nn_val
	  nr_dl=1
	  do j5=1,nn_val

	    b_dl=1
	    j8=0

		if (nr_dl.lt.nn_val) then
		 if (i5.ne.j5) then
            if (lung(i5,j5).ne.0.d0) then

			c_beams6=0
			c_beams6=ABS(c_beams5(i5,j5))

*	        print *,'poutre ',i5,j5
*	        print *,6*(i5-1)+1,6*(i5-1)+6
	
			GM_K_FT(6*(i5-1)+1:6*(i5-1)+6,1:ddl_maxg)=0.d0
			MKpN(1:6,1:ddl_maxg)=0.d0

			if (i5.lt.j5) then
				MKpN(1:6,6*(i5-1)+1:6*(i5-1)+6)=K_TEM(c_beams6,1:6,1:6)
				MKpN(1:6,6*(j5-1)+1:6*(j5-1)+6)=K_TEM(c_beams6,1:6,7:12)
	        else
				MKpN(1:6,6*(i5-1)+1:6*(i5-1)+6)=K_TEM(c_beams6,7:12,7:12)
				MKpN(1:6,6*(j5-1)+1:6*(j5-1)+6)=K_TEM(c_beams6,7:12,1:6)
	        endif

	        GM_K_FT(6*(i5-1)+1:6*(i5-1)+6,1:ddl_maxg)=GM_K_FT(6*(i5-1)+
     *                  1:6*(i5-1)+6,1:ddl_maxg)+MKpN(1:6,1:ddl_maxg)
			b_dl=b_dl+1
*			b_dl_c=b_dl_c+1

	        do j8=j5+1,nn_val
		      if (j8.ne.j5) then
			    if (lung(i5,j8).ne.0.d0) then
					
					c_beams6=0
					c_beams6=ABS(c_beams5(i5,j8))

*					print *,'poutre ',i5,j8,' soudee en noeud ',i5
					
					MKpN(1:6,1:ddl_maxg)=0.d0

					if (i5.lt.j8) then
						MKpN(1:6,6*(i5-1)+1:6*(i5-1)+6)=K_TEM(c_beams6,1:6,1:6)
						MKpN(1:6,6*(j8-1)+1:6*(j8-1)+6)=K_TEM(c_beams6,1:6,7:12)
					else
						MKpN(1:6,6*(i5-1)+1:6*(i5-1)+6)=K_TEM(c_beams6,7:12,7:12)
						MKpN(1:6,6*(j8-1)+1:6*(j8-1)+6)=K_TEM(c_beams6,7:12,1:6)
					endif
			
			GM_K_FT(6*(i5-1)+1:6*(i5-1)+6,1:ddl_maxg)=GM_K_FT(6*(i5-1)+
     *                    1:6*(i5-1)+6,1:ddl_maxg)+ MKpN(1:6,1:ddl_maxg)
			        b_dl=b_dl+1
*					b_dl_c=b_dl_c+1

			    endif
	          endif
	        enddo
	
	        nr_dl=nr_dl+nn_val

		  endif
	     endif
	    endif

	  enddo

*	  b_dl_c=0

	enddo
	
	DEALLOCATE(K_TEM, STAT=sv4)
	DEALLOCATE(MKpN,  STAT=sv4)
	
*	print *,'b_dl_c:',b_dl_c
*	pause
*******************************************************************************
* -> ecriture de la matrice globale de masse sans C.L. (ddl_maxg x ddl_maxg)
	
	ALLOCATE(GM_M_FT(ddl_maxg,ddl_maxg))
*	print *,'OK - GM_M_FT'

	ALLOCATE(MMpN(6,ddl_maxg))
*	print *,'OK - MMpN'

	GM_M_FT(1:ddl_maxg,1:ddl_maxg)=0.d0
	
	i5=0
	j5=0
	b_dl=0

	do i5=1,nn_val
	  nr_dl=1
	  do j5=1,nn_val

	    b_dl=1
	    j8=0

		if (nr_dl.lt.nn_val) then
		 if (i5.ne.j5) then
            if (lung(i5,j5).ne.0.d0) then

			c_beams6=0
			c_beams6=ABS(c_beams5(i5,j5))
*			print *,'c_beam6:',c_beams6
*	        print *,'poutre ',i5,j5
*	        print *,6*(i5-1)+1,6*(i5-1)+6
	
			GM_M_FT(6*(i5-1)+1:6*(i5-1)+6,1:ddl_maxg)=0.d0
			MMpN(1:6,1:ddl_maxg)=0.d0

			if (i5.lt.j5) then
				MMpN(1:6,6*(i5-1)+1:6*(i5-1)+6)=M_TEM(c_beams6,1:6,1:6)
				MMpN(1:6,6*(j5-1)+1:6*(j5-1)+6)=M_TEM(c_beams6,1:6,7:12)

	        else
				MMpN(1:6,6*(i5-1)+1:6*(i5-1)+6)=M_TEM(c_beams6,7:12,7:12)
				MMpN(1:6,6*(j5-1)+1:6*(j5-1)+6)=M_TEM(c_beams6,7:12,1:6)
	        endif
			

	        GM_M_FT(6*(i5-1)+1:6*(i5-1)+6,1:ddl_maxg)=GM_M_FT(6*(i5-1)+
     *				                 1:6*(i5-1)+6,1:ddl_maxg)+MMpN(1:6,1:ddl_maxg)
			b_dl=b_dl+1

	        do j8=j5+1,nn_val
		      if (j8.ne.j5) then
			    if (lung(i5,j8).ne.0.d0) then

					c_beams6=0
					c_beams6=ABS(c_beams5(i5,j8))
*					print *,'c_beam6:',c_beams6
*					print *,'poutre ',i5,j8,' soudee en noeud ',i5
					
					MMpN(1:6,1:ddl_maxg)=0.d0
					
					if (i5.lt.j8) then
						MMpN(1:6,6*(i5-1)+1:6*(i5-1)+6)=M_TEM(c_beams6,1:6,1:6)
						MMpN(1:6,6*(j8-1)+1:6*(j8-1)+6)=M_TEM(c_beams6,1:6,7:12)
					else
						MMpN(1:6,6*(i5-1)+1:6*(i5-1)+6)=M_TEM(c_beams6,7:12,7:12)
						MMpN(1:6,6*(j8-1)+1:6*(j8-1)+6)=M_TEM(c_beams6,7:12,1:6)
					endif
			
			GM_M_FT(6*(i5-1)+1:6*(i5-1)+6,1:ddl_maxg)=GM_M_FT(6*(i5-1)+1:6*(i5-1)
     *				                          +6,1:ddl_maxg)+MMpN(1:6,1:ddl_maxg)
			        b_dl=b_dl+1

			    endif
	          endif
	        enddo

*	        print *,'b_dl:',b_dl-1
	        nr_dl=nr_dl+nn_val

		  endif
	     endif
	    endif

	  enddo
	enddo
	
	DEALLOCATE(M_TEM, STAT=sv5)
	DEALLOCATE(MMpN,  STAT=sv5)
	
*	print *,'pauza inainte de matricea finala'
*	pause
********************************************************************************
* -> ecriture de la matrice globale de rigidité du systeme avec C.L. (ddl x ddl)
	
	ALLOCATE(GM_K_FF(ddl,ddl))
*	print *,'OK - GM_K_FF'

	ALLOCATE(GM_M_FF(ddl,ddl))
*	print *,'OK - GM_M_FF'

	GM_K_FF(1:ddl,1:ddl)=0.d0
	GM_M_FF(1:ddl,1:ddl)=0.d0
	
	nr_dl_i=0
	nr_dl_i=1
	
	i9=0
	do i9=1,ddl_maxg

		if (poz_ddl(i9).eq.0) then
			
			nr_dl_j=0
			nr_dl_j=1
			do j9=1,ddl_maxg
	
				if (poz_ddl(j9).eq.0) then
					GM_K_FF(nr_dl_i,nr_dl_j)=GM_K_FT(i9,j9)
					GM_M_FF(nr_dl_i,nr_dl_j)=GM_M_FT(i9,j9)
					nr_dl_j=nr_dl_j+1
				endif

			enddo
		
		nr_dl_i=nr_dl_i+1

		endif

	enddo
	
*	print *,'nr_dl_i',nr_dl_i
*	print *,'nr_dl_j',nr_dl_j

	DEALLOCATE(GM_K_FT, STAT=sv6)
	DEALLOCATE(GM_M_FT, STAT=sv7)

*	print *,'DEALLOCATE status:',sv6,sv7
*	pause

*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*                                ETAPE FINALE

*******************************************************************************
* -> inversion matrice de masse
	
	ALLOCATE(IPIV2(ddl))
*	print *,'OK - IPIV2'

	ALLOCATE(WORK2(ddl))
*	print *,'OK - WORK2'
	
	IPIV2(1:ddl)=0
	WORK2(1:ddl)=0

	CALL DGETRF(ddl, ddl, GM_M_FF, ddl, IPIV2, INFO1)
	if (INFO1.ne.0) then
	  print *,'SGETRF_INFO1=',INFO1
	  pause
	endif

	CALL DGETRI(ddl, GM_M_FF, ddl, IPIV2, WORK2, ddl, INFO2)
	if (INFO2.ne.0) then
	  print *,'SGETRI_INFO2=',INFO2
	  pause
	endif

*	print *, 'Temps calcul 4:', secnds(T1)
	DEALLOCATE(IPIV2, STAT=sv8)
	DEALLOCATE(WORK2, STAT=sv8)

*	pause
*******************************************************************************
* -> multiplication (Me^-1)*Ke
	
	ALLOCATE(IMg_Kg(ddl,ddl))
*	print *,'OK - IMg_Kg'

	IMg_Kg(1:ddl,1:ddl)=0.d0
	CALL DGEMM('N','N',ddl,ddl,ddl,1.d0,GM_M_FF,ddl,GM_K_FF,ddl,0.d0,IMg_Kg
     *              ,ddl)
	
	DEALLOCATE(GM_K_FF, STAT=sv9)

*	print *,'DEALLOCATE status sv9:',sv9
*	print *, 'Temps calcul 5:', secnds(T1)
*	pause
*******************************************************************************
* -> recherche des valeurs propres (IMg_Kg-lamdba*I=0)
	
	LWORK=0
	LWORK=3*ddl

	ALLOCATE(WORK6(LWORK))
*	print *,'OK - WORK6'

	ALLOCATE(WR1(ddl))
	ALLOCATE(WI1(ddl))

	ALLOCATE(VL1(1,ddl))
	ALLOCATE(VR1(1,ddl))

	WORK6(1:LWORK)=0.d0

	WR1(1:ddl)=0.d0
	WI1(1:ddl)=0

	VL1(1,1:ddl)=0
	VR1(1,1:ddl)=0
	
	CALL DGEEV('N','N',ddl,IMg_Kg,ddl,WR1,WI1,VL1,1,VR1,1,WORK6,LWORK,
     *              INFO6)
	if (INFO6.ne.0) then
	  print *,'SGETRI_INFO6=',INFO6
	  pause
	endif
	
*	JOBZ='N'
*	UPLO='U'
*	CALL DSYEV(JOBZ,UPLO,ddl,IMg_Kg,ddl,WR1,WORK6,LWORK,INFO6)
*	if (INFO6.ne.0) then
*	  print *,'DSYEV_INFO6=',INFO6
*	  pause
*	endif
	

	DEALLOCATE(IMg_Kg2, STAT=sv10)
	
	DEALLOCATE(WORK6, STAT=sv10)

	DEALLOCATE(WI1, STAT=sv10)
	DEALLOCATE(VL1, STAT=sv10)
	DEALLOCATE(VR1, STAT=sv10)
	
	print *,'pause avant Deter'

*	Deter=0.d0
*	Deter=SQRT(MINVAL(WR1))/(2.d0*Pi)

*	print *,'f1:',Deter
*	print *, 'Temps calcul:', secnds(T1)
*	print *,'-------------------------'
	
	Call Bubble(WR1, ddl)
	
	i70=0
	i71=0
	do i70=1,ddl
		if (WR1(i70).gt.0.d0) then
			i71=i70
			exit
		endif
	enddo

*	print *,'f1:',WR1(1)
*	print *,'f2:',WR1(2)
*	print *,'f3:',WR1(3)
*	print *,'f4:',WR1(4)
*	print *,'f5:',WR1(5)
*	print *,'f6:',WR1(6)
*	print *,'f7:',WR1(7)
*	print *,'f8:',WR1(8)
*	print *,'f9:',WR1(9)
*	print *,i71,':',WR1(i71)

*	print *,'val_WR1:',MINVAL(WR1)
*	pause

	Deter=0.d0
	if (i71.eq.1) then
		Deter=SQRT(MINVAL(WR1))/(2.d0*Pi)
	else
		print *,'Error - First eigenvalue negative'
		err=8
*		Deter=SQRT(WR1(i71))/(2.d0*Pi)
	endif
	
	DEALLOCATE(WR1, STAT=sv10)

*******************************************************************************
*						Verification inside subroutines
	if ((INFO1.ne.0).or.(INFO2.ne.0).or.(INFO3.ne.0).or.(INFO4.ne.0).or.
     *      (INFO6.ne.0)) then
		err=7
	endif
*******************************************************************************
	
 8888 continue

	endsubroutine vibration_module_v5

*******************************************************************************
*******************************************************************************
*******************************************************************************

	subroutine vibration_module_sans_dll(first_frequency)

	implicit none
	
	integer i88,nr_val_input,rad_lo1,rad_la1,t_bc1,err1,ddl1

	double precision long1,larg1,thick_plate1
	double precision rho2,E2,niu2

	double precision, allocatable :: p_lo1(:),p_la1(:)
	integer, allocatable :: type_lo1(:),type_la1(:)
	character, allocatable :: sect_lo1(:),sect_la1(:)

	integer, dimension (30) :: type_sect1
	double precision, dimension (30) :: l21,ep21,l31,ep31
	
	double precision first_frequency,T2


	print *,'read Input Data'

	T2=secnds(0.0)
*------------------------------------------------------------------------------	
	nr_val_input=1000

	open(910,file='input_data_full_dp.dat')
	
	i88=0

	do i88=1,10000

		if (i88.eq.1) then
			read(910,*,end=91) long1,larg1,thick_plate1,rad_lo1,rad_la1,rho2,E2,
     *                          niu2,t_bc1
			
*			print *,'OK1'

	ALLOCATE(p_lo1(rad_lo1))
	ALLOCATE(p_la1(rad_la1))

	ALLOCATE(type_lo1(rad_lo1))
	ALLOCATE(type_la1(rad_la1))

	ALLOCATE(sect_lo1(rad_lo1))
	ALLOCATE(sect_la1(rad_la1))

		elseif ((i88.ge.2).and.(i88.le.(rad_lo1+1))) then
			read(910,*,end=91) p_lo1(i88-1),type_lo1(i88-1),sect_lo1(i88-1)
*			print *,'OK2'
		
		elseif ((i88.ge.(rad_lo1+2)).and.(i88.le.(rad_lo1+1+rad_la1))) then
			read(910,*,end=91) p_la1(i88-rad_lo1-1),type_la1(i88-rad_lo1-1),
     *                          sect_la1(i88-rad_lo1-1)
*			print *,'OK3'
		
		elseif (i88.ge.(rad_lo1+rad_la1+2)) then
			read(910,*,end=91) type_sect1(i88-rad_lo1-rad_la1-1),
     &								  l21(i88-rad_lo1-rad_la1-1),
     &								 ep21(i88-rad_lo1-rad_la1-1),
     &								  l31(i88-rad_lo1-rad_la1-1),
     &								 ep31(i88-rad_lo1-rad_la1-1)
*			print *,'OK4'
		endif

	enddo
   91 nr_val_input=i88-1
	close(910)
*------------------------------------------------------------------------------

	call vibration_module_v5(long1,larg1,thick_plate1,rad_lo1,rad_la1,rho2,
     *                          E2,niu2,t_bc1,
     &                               p_lo1,type_lo1,sect_lo1,
     &							   p_la1,type_la1,sect_la1,
     &							   type_sect1,l21,ep21,l31,ep31,
     &							   first_frequency,ddl1,err1)
	
*	print *,'Number of DOF:',ddl1
	print *,'First frequency:',first_frequency,'  Hz'
	print *,'Error:',err1
	print *, 'CPU Time:', secnds(T2)/60.D0,'  minutes'

	!endprogram vibration_module_sans_dll
	return
	end

*******************************************************************************
*******************************************************************************
*******************************************************************************
