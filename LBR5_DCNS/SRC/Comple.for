      subroutine comple(equ,dequ,arg,darg,nbrxi,i,k,a,b,it,xx)
      implicit real*8(a-h,o-z)
      dimension a(8,4),b(8,4),xx(400),
     *          equ(9),dequ(9),arg(8),darg(8,9)
      call compl(equ,dequ,arg,darg,nbrxi,i,k,a,b,it,
     *  xx(1) ,xx(5), xx(9),xx(13),xx(17),xx(21),xx(25),xx(29),
     *  xx(33),xx(41),xx(49),xx(57))
	return
	end

c***********************************************************************
c***********************************************************************
      subroutine compl(equ,dequ,arg,darg,nbrxi,i,k,a,b,it,
     *             au1,au2,au3,au4,au5,au6,au7,au8,arg2,arg3,arg4,arg5)
      implicit real*8(a-h,o-z)
      dimension a(8,4),b(8,4),
     *          equ(9),dequ(9),arg(8),darg(8,9)
      dimension au1(4),au2(4),au3(4),au4(4),au5(4),au6(4),au7(4),
     *          au8(4),arg2(8),arg3(8),arg4(8),arg5(8)

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c       modifié : 15-4-99							     créer : 1994
c       --------			                             -------
c	subroutine complex : calcul de la dérivée (darg) des solutions (arg) 
c     de l'équation du 8ème ordre par rapport au variables de conception (xi).
c     avec:
c     i = indice du alpha et béta (i=1,m) avec mmin=2 et mmax=4 
c     k = nø de la variable de conception (xi) par rapport à laquelle on dérive. 
c	
c     equ  = coéfficients de l'équation du 8ème ordre
c     arg  = solutions de l'équation du 8ème ordre (alpha + i béta)
c	       alpha(i) = arg(2i-1) et béta(i) = arg(2i), i=1,4
c     dequ = dérivées des coéfficients equ (voir subr. darg)
c     darg = dérivées des solutions de l'équation du 8ème ordre (alpha + i béta)
c	a,b  = matrice de calcul de stokage (temporaire)
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c  a(1,i) = solutions complexes de l'équation du 8ème ordre (partie réelle= alpha)
c  b(1,i) = solutions complexes de l'équation du 8ème ordre (partie imaginaire=béta)
c           si les solutions sont: (a,b)=(alpha(i),béta(i))=(arg(2i-1),arg(2i))

	if(it.eq.1) then
      do 1 j=2*i-1,2*i
      arg2(j)=arg(j)*arg(j)
      arg3(j)=arg2(j)*arg(j)
      arg4(j)=arg2(j)*arg2(j)
   1  arg5(j)=arg3(j)*arg2(j)
      au1(i)=2.*arg(2*i-1)*arg(2*i)                           !  2 a b
      au2(i)=arg2(2*i-1)-arg2(2*i)                            !  a**2-b**2
      au3(i)=-2.*au1(i)*au2(i)                                !  -4(a**3 b - a b**3)
      au4(i)=arg4(2*i-1)+arg4(2*i)-6.*arg2(2*i-1)*arg2(2*i)   !  a**4+b**4-6 a**2 b**2
      au5(i)=arg3(2*i-1)-3.*arg(2*i-1)*arg2(2*i)              !  +(a**3 - 3 a b**2)
      au6(i)=arg3(2*i)-3.*arg(2*i)*arg2(2*i-1)                !  -(3 a**2 b - b**3)
      au7(i)=arg5(2*i-1)-10.*arg3(2*i-1)*arg2(2*i)            !
     *                  +5.*arg(2*i-1)*arg4(2*i)              !  a**5-10 a**3 b**2 + 5 a b**4
      au8(i)=arg5(2*i)-10.*arg3(2*i)*arg2(2*i-1)
     *                  +5.*arg(2*i)*arg4(2*i-1)              ! b**5-10 b**3 a**2 + 5 b a**4

	a(1,i)=arg(2*i-1)
	b(1,i)=arg(2*i)
	a(2,i)=au2(i)
	b(2,i)=au1(i)
	a(3,i)=au5(i)
	b(3,i)=-au6(i)
	a(4,i)=+au4(i)
	b(4,i)=-au3(i)
	a(5,i)=au7(i)
	b(5,i)=au8(i)
	a(6,i)=+au5(i)*au5(i)-au6(i)*au6(i)
	b(6,i)=-2.*au5(i)*au6(i)
	a(7,i)=+au4(i)*au5(i)-au3(i)*au6(i)
	b(7,i)=-au4(i)*au6(i)-au3(i)*au5(i)
	a(8,i)=+au4(i)*au4(i)-au3(i)*au3(i)
	b(8,i)=-2.*au3(i)*au4(i)
      endif

 	deriva=dequ(9)*a(8,i)+dequ(7)*a(6,i)+dequ(5)*a(4,i)
     *	                 +dequ(3)*a(2,i)+dequ(1)
 	derivb=dequ(9)*b(8,i)+dequ(7)*b(6,i)+dequ(5)*b(4,i)
     *	                 +dequ(3)*b(2,i)     
 	tempa=8.*equ(9)*a(7,i)+6.*equ(7)*a(5,i)
     *                      +4.*equ(5)*a(3,i)+2.*equ(3)*a(1,i)
 	tempb=8.*equ(9)*b(7,i)+6.*equ(7)*b(5,i)
     *                      +4.*equ(5)*b(3,i)+2.*equ(3)*b(1,i)

	temp=tempa*tempa+tempb*tempb

	darg(2*i-1,k)=-(deriva*tempa+derivb*tempb)/temp
	darg(2*i,k)=-(derivb*tempa-deriva*tempb)/temp

      return
      end
