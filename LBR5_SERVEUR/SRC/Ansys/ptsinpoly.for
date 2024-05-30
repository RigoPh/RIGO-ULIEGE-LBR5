c Deane Merrill's points-in-polygon routine 4/12/96
c This file is ptsinpoly.f
c
c USAGE:
c	integer maxnpts			! max points to be tested
c	data maxnpts /100000 /		! number of points to be tested
c	integer npts			! number of points to be tested
c	double precision x(1:100000),y(1:100000)	! points to be tested
c     	double precision xinf,yinf			! any point outside polygon
c	integer nmax			! max pts in boundary
c	data nmax / 200 /		! max pts in boundary
c	integer n 			! pts in boundary, no repeated end pt
c	double precision xy(1:2,1:200)		! polygon boundary
c					! can be clockwise or counterclockwise
c	integer iotmsg			! file for error messages
c     	integer inpoly(1:100000)	! output values (1,0) if (in/out)

c	(set npts, x(1:npts), y(1:npts), xinf, yinf, n, xy(1:2,1:n) )
c	call ptsinpoly(npts,x,y,xinf,yinf,n,xy,iotmsg,inpoly)
c	(read out inpoly(1:npts) )

c EXECUTION: (on parep2.lbl.gov)
c	module load lang
c	f77 ptsinpoly.f
c	a.out

c TIMING:
c The following sample program with npts=100000, n=4 takes 5 secs on a Sparc 10.

c SAMPLE PROGRAM:
	subroutine ptsinpolyTEST

!	integer maxnpts			! max points to be tested
!	data maxnpts /100000 /		! number of points to be tested
	integer npts			! number of points to be tested
	double precision x(1:100000),y(1:100000)	! points to be tested
     	double precision xinf,yinf			! any point outside polygon
!	integer nmax			! max pts in boundary
!	data nmax / 200 /		! max pts in boundary
	integer n 			! pts in boundary, no repeated end pt
	double precision xy(1:2,1:200)		! polygon boundary
					! can be clockwise or counterclockwise
	integer iotmsg			! file for error messages
     	integer inpoly(1:100000)	! output values (1,0) if (in/out)

	xinf=100.
	yinf=100.
	n=4
	iotmsg=6

c	polygon is square with corners at +/- 1
	xy(1,1) = -1.
	xy(2,1) = -1.
	xy(1,2) = -1.
	xy(2,2) =  1.
	xy(1,3) =  1.
	xy(2,3) =  1.
	xy(1,4) =  1.
	xy(2,4) = -1.
	
	npts=100000
	do 10 i=1,npts
c	random pt in square with corners at +/- 2
	x(i)=-2. +4.*rand(0)
	y(i)=-2. +4.*rand(0)
10	continue

	call ptsinpoly(npts,x,y,xinf,yinf,n,xy,inpoly)

	ninside = 0

	do 11 i=1,npts
	ninside = ninside + inpoly(i)
	if (i.le.10) write(iotmsg,100) i,x(i),y(i),inpoly(i)
100	format('i, x, y, inpoly=',i5,2f10.3,i3)
11	continue

	write(iotmsg,101) npts, ninside
101	format('npts, ninside=',2i6)

	end

c SUBROUTINE ptsinpoly for multiple test points

	subroutine ptsinpoly(npts,x,y,xinf,yinf,n,xy,inpoly)
	integer npts			! number of points to be tested
	double precision x(1:npts),y(1:npts)	! coords of points to be tested
     	double precision xinf,yinf			! any point outside polygon
	integer n 			! points in polygon, no repeated end pt
	double precision xy(1:2,1:n)		! polygon boundary
 					! can be clockwise or counterclockwise
!	integer iotmsg			! file for error messages
     	integer inpoly(1:npts)		! output values (1,0) if (in/out)

	do 20 i=1,npts
	call ptinpoly(x(i),y(i),xinf,yinf,n,xy,inpoly(i))
20	continue

	end

c SUBROUTINE ptinpoly for a single test point

	subroutine ptinpoly(x,y,xinf,yinf,n,xy,inpoly)
c	returns 1 if x,y is within polygon xy(1:2,1:n)
c	returns 0 if x,y is outside polygon xy(1:2,1:n)
c	(points are counterclockwise, with no repeated endpoint)
c	xinf,yinf is a point guaranteed to be outside the polygon

	integer n 			! no of points in polygon
	double precision x,y			! test point
	double precision xinf,yinf			! a point guaranteed outside
	double precision xy(1:2,1:n)	! polygon boundary, no repeated endpoint
!	integer iotmsg 			! logical unit for messages
	integer*4 inpoly			! output: (1/0) if (inside/outside)

c	local variables
	integer*4 i,nint
	double precision xi,yi			! point of intersection
	double precision f1,f2			! 0<f1<1 and 0<f2<1 if intersection
c

	nint=0
	do 10 i=1,n
	i1=i
	if (i.eq.n) then
		i2=1
	else
		i2=i1+1
	endif
c	returns 0<f1<1 and 0<f2<1 if (x-xinf,y-yinf) intersects segment i
	call xsect(x,y,xy(1,i1),xy(2,i1),xy(1,i2),xy(2,i2),xinf,yinf
     .	,xi,yi,f1,f2)
	if (0.lt.f1.and.f1.lt.1.and.0.lt.f2.and.f2.lt.1) nint=nint+1

c	write(iotmsg,*) 'n,i,i1,i2,x,y=',n,i,i1,i2,x,y
c     	write(iotmsg,*) 'xy=',xy(1,i1),xy(2,i1),xy(1,i2),xy(2,i2)
c     	write(iotmsg,*) 'xinf,yinf,xi,yi,f1,f2,nint=',
c     x		xinf,yinf,xi,yi,f1,f2,nint

10	continue
c	if nint even, point is outside

	if (mod(nint,2).eq.0) then
		inpoly=0
	else
		inpoly=1
	endif

	return
	end

      subroutine xsect(xa,ya,xb,yb,xc,yc,xd,yd,xe,ye,f1,f2)
c		given a segment a-d which crosses a segment b-c,
c		find the intersection e.
c
c	WARNING: if f1<0 or f1>1 or  f2<0 or f2>1, segments do not intersect
c	and the intersection (xe,ye) is not within the segments
c	
c	example:
c	xa=0,ya=1, xd=0,yd=-1
c	xb=-1,yb=0, xc=1,yc=0
c	xe=0,ye=0, f1=0.5,f2=0.5

c	f1 is the fractional distance (e-a)/(d-a)
c	f2 is the fractional distance (e-b)/(c-b)
c
c	xe=xa+f1*(xd-xa)
c	ye=ya+f1*(yd-ya)
c	xe=xb+f2*(xc-xb)
c	ye=yb+f2*(yc-yb)
c	example:
c	0=0+f1*(0-0)
c	0=1+f1*(-1-1)
c	0=-1+f2*(1+1)
c	0=0+f2*(0-0)

c	xa+f1*(xd-xa)=xb+f2*(xc-xb)
c	ya+f1*(yd-ya)=yb+f2*(yc-yb)
c	example:
c	0+f1*(0-0) = -1+f2*(1+1)
c	1+f1*(-1-1) = 0+f2*(0-0)
c	
c	(xd-xa)*f1 + (xb-xc)*f2 = (xb-xa)
c	(yd-ya)*f1 + (yb-yc)*f2 = (yb-ya)
c	example:
c	(0-0)*f1   + (-1-1)*f2  = (-1-0)
c	(-1-1)*f1  + (0-0)*f2   = (0-1)
c	
c	define:
c	A=(xd-xa)	B=(xb-xc)
c	C=(yd-ya)	D=(yb-yc)
c	det=AD-BC
c	example:
c	A=(0-0)=0	B=(-1-1)=-2
c	C=(-1-1)=-2	D=(0-0)=0
c	det=0*0-(-2)(-2)=-4
c
c	inverse matrix
c	A'=D/det	B'=-B/det
c	C'=-C/det	D'=A/det
c	example:
c	A'=0/(-4)=0	B'=2/(-4)=-0.5
c	C'=2/(-4)=-0.5	D'=0/(-4)=0
c
c	f1 = A'*(xb-xa) + B'*(yb-ya)
c	f2 = C'*(xb-xa) + D'*(yb-ya)
c	example:
c	f1 = 0*(-1-0)    + (-0.5)*(0-1) = 0.5
c	f2 = -0.5*(-1-0) + 0*(0-0)      = 0.5
c	

	double precision A,B,C,D,det,AI,BI,CI,DI,f1,f2
	double precision xa,ya,xb,yb,xc,yc,xd,yd,xe,ye
!	integer iotmsg

	xe=-999.
	ye=-999.

c	A=(xd-xa)	B=(xb-xc)
c	C=(yd-ya)	D=(yb-yc)
c	det=AD-BC

	A=xd-xa
	B=xb-xc
	C=yd-ya
	D=yb-yc
	det=A*D-B*C

	if (det.ne.0) then

		AI=D/det
		BI=-B/det
		CI=-C/det
		DI=A/det

		f1 = AI*(xb-xa) + BI*(yb-ya)
		f2 = CI*(xb-xa) + DI*(yb-ya)

		xe=xa+f1*(xd-xa)
		ye=ya+f1*(yd-ya)
c	the following should give the same result
		xe=xb+f2*(xc-xb)
		ye=yb+f2*(yc-yb)

	endif

c	write(iotmsg,'(a,4f8.4)') 'xsect: xa..xd=',xa,xb,xc,xd
c	write(iotmsg,'(a,4f8.4)') 'ysect: ya..yd=',ya,yb,yc,yd
c	write(iotmsg,'(a,5e12.4)') 'xsect: a,b,c,d,det=',a,b,c,d,det
c	write(iotmsg,'(a,4e12.4)') 'xsect: ai,bi,ci,di=',ai,bi,ci,di
c	write(iotmsg,'(a,4f8.4)') 'xsect: f1,f2,xe,ye=',f1,f2,xe,ye

	return
	end
