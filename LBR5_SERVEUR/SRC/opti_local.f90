subroutine opti_local()

use param_section
use PARAM_SECTION_VECTOR

! ====================================================================================================
!  soit "nvmax" variables et "mmax" restrictions
!        ================     ==================
!            avec nsol le nombre de cas de charge (max nsolmax=5)

implicit double precision (a-h,o-z)
                   

!16.0 module d'optimisation
!     =======================

!rewind iu_26(iboat)

call comp_reduced_var ()
call comp_obj()
call comp_restri_local()

! 4.4 calcul des dérivées réduites pour les nr variables (var. indépendantes)
!     -----------------------------------------------------------------------
if(negalt(iboat).gt.0) then

   call annuld(z1opt(1:ntot(iboat),iboat),ntot(iboat))
   
   do j=1,ntot(iboat)
      do  i=1,ntot(iboat)
         z1opt(j,iboat)=z1opt(j,iboat)+fiopt(i,iboat)*dxopt(i,j,iboat)
      enddo
   enddo
   
   do j=1,ntot(iboat)
      fiopt(j,iboat)=z1opt(j,iboat)
   enddo

   call red1d(fiopt(1:ntot(iboat),iboat),fiopt(1:ntot(iboat),iboat),ntot(iboat),	&
				nxi2(1:9,1:neto_vector(iboat),iboat),mega(1:ngmax,1:4,iboat),				&
				negalt(iboat),neto_vector(iboat),ngmax)


   do k=1,mtot(iboat)
      call annuld(z1opt(:,iboat),ntot(iboat))

      do j=1,ntot(iboat)
          do i=1,ntot(iboat)
              z1opt(j,iboat)=z1opt(j,iboat)+cijopt(i,k,iboat)*dxopt(i,j,iboat)
          enddo
      enddo

      call red1d(z1opt(1:ntot(iboat),iboat),z1opt(1:ntot(iboat),iboat),ntot(iboat),	&
				   nxi2(1:9,1:neto_vector(iboat),iboat),mega(1:ngmax,1:4,iboat),			&
				   negalt(iboat),neto_vector(iboat),ngmax)

      do j=1,nr(iboat)
         cijopt(j,k,iboat)=z1opt(j,iboat)
	  enddo
   enddo
endif


!  ====================================================================  
! 9.0 les formats
!     ===========
20 format(///' entree dans le module optimisation (conlin-lbr5)'/  &
          50(1h*)//                                                &
         ' conlin optimizer                      ',                &
         ' logiciel des bordages raidis'/                          &
         ' version 2.0                            ',               &
         ' version : l.b.r.- 5.0'/                                 &
         ' author: prof. c. fleury                ',               &
         ' author: dr. ph. rigo'/)
32 format('panel nø ',i3,9(1x,a8,3x))
33 format('bornes min:',9(e11.4,1x))
34 format('variables_:',9(e11.4,1x))
35 format('bornes max:',9(e11.4,1x))
51 format('initialisation des données dans opti'/40(1h+)) 
53 format('les nouvelles donnees'/25(1h-))

return
end

!  ====================================================================

subroutine red1d(x,xr,nd,nxi2,mega,negalt,neto,ngmax)

!  ====================================================================

implicit double precision (a-h,o-z)
dimension mega(ngmax,4),nxi2(9,neto),x(nd),xr(nd)

do i=1,nd
   xr(i)=x(i)
enddo

do i=1,negalt
   j=nxi2(mega(i,1),mega(i,2))
   jj=j-i+1
   do k=1,nd-jj
      jk=jj+k-1
      xr(jk)=xr(jk+1)
   enddo
   xr(nd)=0.d00
enddo

return
end
