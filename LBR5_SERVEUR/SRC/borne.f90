subroutine borne(xi,i,text,nel,xm,xp,iff)

use param_section, XI_=>XI, IFF_=>IFF

implicit double precision (a-h,o-z)
character*29 text
!dimension xm(nvmax),xp(nvmax)  ! à la place du common opti4
double precision xm,xp

!***********************************************************************
!     subroutine borne
!     **************
!     verification de la solution de depart
!       il faut que les xi de départ soient inclus entre les bornes
!      c.à.d.   xi(min) < xi  < xi(max)
!
!     modif:10-5-95                                  création :7-5-95
!***********************************************************************
if(impr2.ge.-2) then					
   write(iu_31(iboat),285) i,text,xm,xi,xp
endif									

if(((xi-xm).lt.-1e-6).or.((xp-xi).lt.-1e-6)) then
   write(*,*) 'bornes non compatibles avec les valeurs initiales',' des variables de conception.'
   write(*,*) 'xi(min) <  xi < xi(max) pas vérifié'
   write(*,*) 'panneau nø',nel,'   variable nø',i
   write(*,*)  xm,' ?<? ',xi,' ?<? ',xp,' pas vérifié'
   write(iu_14(iboat),*) 'bornes non compatibles avec les valeurs initiales', &		
                         'des variables de conception.'							
   write(iu_14(iboat),*) 'xi(min) <  xi < xi(max) pas vérifié'					
   write(iu_14(iboat),*) 'panneau nø',nel,'   variable nø',i						
   write(iu_14(iboat),*)  xm,' ?<? ',xi,' ?<? ',xp,' pas vérifié'			

   write(*,*) 'error'
   read(*,*)
	 iff=1 !stop à la sortie de la subr ent
endif

return

285  format('var. nø',i3,1x,a29,2x,e11.4,'<',e11.4,' < ', e11.4)

end
