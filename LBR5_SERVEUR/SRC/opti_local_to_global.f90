subroutine opti_local_to_global()



use param_section
use PARAM_SECTION_VECTOR

implicit double precision (a-h,o-z)

obj_global= obj_global+ obj(iboat)
jobj_global= 0 !jobj_global+jobj(iboat)

if (iboat.eq.1) then
fiopt_global(:) = 0.0d00

xicou_global (:)      =0.d00
ximin_global (:)      =0.d00
ximax_global (:)      =0.d00

if (ALLOCATED(ega_global)) ega_global  (:)      =0.d00
if (ALLOCATED(mega_global)) mega_global  (:,:)    =0
nvs_global (:)      =0
negal_global (:)      =0
negalt_global         =0

nxi2_global  (:,:)    =0      
nxi2r_global (:,:)    =0      
nxit_global  (:,:)    =0
nvar_global  (:)      =0

nvarr_temp_global (:)      =0
nxitr_temp_global (:,:)    =0
nvarr_global (:)      =0
nxitr_global (:,:)    =0

xiopt_global (:)      =0.d00
cjopt_global (:)      =0.d00
cjmopt_global(:)	  =0.d00
cijopt_global(:,:)    =0.d00

ic_rest_global(:)     =0
ratio_rest_global(:)  =0.d00

dxopt_global(:,:)    =0.d00

endif

! ====================================================================================================
! combinaison des diff. structures (déjà réduites localement) dans une structure globale
! ====================================================================================================

xiopt_global(1+shift_nr:nr(iboat)+shift_nr) = xiopt(1:nr(iboat),iboat)
ximin_global(1+shift_nr:nr(iboat)+shift_nr) = ximin(1:nr(iboat),iboat)
ximax_global(1+shift_nr:nr(iboat)+shift_nr) = ximax(1:nr(iboat),iboat)
fiopt_global(1+shift_nr:nr(iboat)+shift_nr) = fiopt(1:nr(iboat),iboat)

     cjopt_global(1+shift_mtot:mtot(iboat)+shift_mtot) =      cjopt(1:mtot(iboat),iboat)
    cjmopt_global(1+shift_mtot:mtot(iboat)+shift_mtot) =     cjmopt(1:mtot(iboat),iboat)
     vnom2_global(1+shift_mtot:mtot(iboat)+shift_mtot) =      vnom2(1:mtot(iboat),iboat)		
   ic_rest_global(1+shift_mtot:mtot(iboat)+shift_mtot) =    ic_rest(1:mtot(iboat),iboat)
ratio_rest_global(1+shift_mtot:mtot(iboat)+shift_mtot) = ratio_rest(1:mtot(iboat),iboat)

cijopt_global(1+shift_nr:nr(iboat)+shift_nr,1+shift_mtot:mtot(iboat)+shift_mtot) = &
		        cijopt(1:nr(iboat),1:mtot(iboat),iboat)					

modes_global(1+shift_neto:neto_vector(iboat)+shift_neto) = &
				modes_vector(1:neto_vector(iboat),iboat)
m2cont_global(1+shift_neto:neto_vector(iboat)+shift_neto) = &
				m2cont_vector(1:neto_vector(iboat),iboat)

m4cont_global(1+shift_neto:neto_vector(iboat)+shift_neto) = &
				m4cont_vector(1:neto_vector(iboat),iboat)

m5cont_global(1+shift_neto:neto_vector(iboat)+shift_neto) = &
				m5cont_vector(1:neto_vector(iboat),iboat)


		 
m1tabl_global(1+shift_neto:neto_vector(iboat)+shift_neto,1:nsol_vector(iboat)) = &
				m1tabl_vector(1:neto_vector(iboat),1:nsol_vector(iboat),iboat)
		  
nvarr_temp_global(1+shift_neto:neto_vector(iboat)+shift_neto) = &
				nvarr(1:neto_vector(iboat),iboat) !!! déjà réduite localement

nxitr_temp_global(1:9,1+shift_neto:neto_vector(iboat)+shift_neto) = &
				nxitr(1:9,1:neto_vector(iboat),iboat) !!! déjà réduite localement

!nvar_global(1+shift_neto:neto_vector(iboat)+shift_neto) = &
!				nvarr(1:neto_vector(iboat),iboat) !!! déjà réduite localement

!nxit_global(1:9,1+shift_neto:neto_vector(iboat)+shift_neto) = &
!

!nxi2_global(1:9,1+shift_neto:neto_vector(iboat)+shift_neto) = &
!				nxi2 (1:9,1:neto_vector(iboat),iboat)+shift_nr !!! pas réduite localement, nxi2 ne doit pas changer

do nel=1,neto_vector(iboat)
   do k=1,9
    if(nxi2r(k,nel,iboat).gt.0) then
       nxi2r_global(k,nel+shift_neto)=nxi2r(k,nel,iboat)+shift_nr
    endif
   enddo
enddo

do nel=1,neto_vector(iboat)
   do k=1,9
    if(nxi2(k,nel,iboat).gt.0) then
       nxi2_global(k,nel+shift_neto)=nxi2(k,nel,iboat)+shift_ntot
    endif
   enddo
enddo

shift_mtot = shift_mtot + mtot(iboat)
shift_neto = shift_neto + neto_vector(iboat)
shift_ntot = shift_ntot + ntot(iboat)
shift_nr = shift_nr + nr(iboat)
!shift_nsol = shift_nsol + nsol_vector(iboat) !pas sur nsol

return
end
