subroutine allocate_param_opti_global()		

use param_section
use PARAM_SECTION_VECTOR

implicit double precision (a-h,o-z)
integer*4 ok

iptmax_sup   =4  !4
m1max_sup    =50
mmmax  =26240    

!mm=somme(m1t)+m2t  < 8000										


! ====================================================================================================
! allocate optis variables 
! ====================================================================================================
  nmax_sup    =maxval(neto_vector)
 ngmax_sup    =maxval(neto_vector)*9

!m1tmax_sup    =maxval(neto_vector)*20*20 !20restri max*10


  nbrMat_sup    =maxval(nbrMat_vector)

!  nmax_v_global =sum of the number of panels of each section 

neto_global = sum(neto_vector)
nmax_v_global = sum(neto_vector)*9

m1tmax_global = sum(neto_vector)*10*(m1max_sup+10+3)+6*nfile ! 10 cas de charge (50 restri str+10geom+3slosh) max par panneau + 6glob, et 10 cas de charge max

nbrMat_sup = sum(nbrMat_vector)

allocate (qn_global   (neto_global)) 
allocate (philn_global(neto_global))
allocate (modes_global(neto_global))
allocate (m1tabl_global(neto_global,nsolmax))
allocate (m2cont_global(neto_global))
allocate (m4cont_global(neto_global))
allocate (m5cont_global(neto_global))

allocate (igeom_index(2,nfile))
allocate (igrav_index(2,nfile))
allocate (inert_index(2,nfile))
allocate ( imod_index(2,nfile))

allocate (iweight_index(2,nfile))
allocate ( iprice_index(2,nfile))
allocate ( istruc_index(2,nfile))
allocate ( irestr_index(2,nfile))

allocate (vnom2(mmmax,nfile))
allocate (vnom2_global(m1tmax_global))

allocate (ig_conlin(nfile))
allocate (jt_conlin(nfile))
!allocate (ms_conlin(nfile))

allocate (kcont(nfile))

allocate (xicou(ngmax_sup,nfile))
allocate (ximin(ngmax_sup,nfile))
allocate (ximax(ngmax_sup,nfile))

allocate (xicou_global(nmax_v_global))
allocate (ximin_global(nmax_v_global))
allocate (ximax_global(nmax_v_global))

allocate (nr  (nfile))
allocate (ntot(nfile))
allocate (mtot(nfile))
allocate (m1tot(nfile))
allocate (m2tot(nfile))
allocate (m3tot(nfile))
allocate (m4tot(nfile))
allocate (m5tot(nfile))

allocate (ega (ngmax_sup,nfile))
allocate (mega(ngmax_sup,4,nfile))
allocate (nvs ( nmax_sup,nfile))
allocate (negal(nmax_sup,nfile))
allocate (negalt(nfile))

allocate (nxi2(9,nmax_sup,nfile))
allocate (nxit(9,nmax_sup,nfile))
allocate (nvar(  nmax_sup,nfile),stat = ok)

allocate (nvarr(  nmax_sup,nfile))
allocate (nxitr(9,nmax_sup,nfile))
allocate (nxi2r(9,nmax_sup,nfile))

allocate (obj     (nfile))
allocate (obj2    (nfile))
allocate (jobj    (nfile))
allocate (objmulti (nfile))
allocate (obj2multi(nfile))

allocate (fiopt        (ngmax_sup,nfile))			
allocate (fiopt_global (nmax_v_global))			

allocate (xiopt (ngmax_sup,nfile))			
allocate (cjopt (mmmax,nfile))			
allocate (cjmopt(mmmax,nfile))		
allocate (cijopt(ngmax_sup,mmmax,nfile))

allocate (ic_rest(mmmax,nfile))
allocate (ratio_rest(mmmax,nfile))

allocate (dxopt (ngmax_sup,ngmax_sup,nfile))	
allocate (z1opt (ngmax_sup,nfile))			
allocate (z2opt (ngmax_sup,nfile))			
allocate (z3opt (ngmax_sup,nfile))			

!iconlin=8*nmax_v_global + 2*mmmax + 2*nmax_v_global*mmmax + 2*max0(nmax_v_global,mmmax) + 3	
!is = 4*n + 3*m + (max(n,m)+1) + n*m + 3*n + 1 
iconlin = 7*nmax_v_global + 2*(max0(nmax_v_global,mmmax)+1) + 2*mmmax + nmax_v_global*mmmax + 1
allocate (sopt  (iconlin))		

allocate (fimulti(nmax_v_global,nfile))	
allocate (fimulti_global(nmax_v_global))	
allocate (derredress(nmax_v_global))	

allocate (shift_opt(nfile))    !initialisation pour comp_obj

allocate (xiopt_global (nmax_v_global))			
allocate (cjopt_global (m1tmax_global))			
allocate (cjmopt_global(m1tmax_global))		
allocate (cijopt_global(nmax_v_global,m1tmax_global))

allocate (ic_rest_global(m1tmax_global))
allocate (ratio_rest_global(m1tmax_global))

allocate (dxopt_global (nmax_v_global,nmax_v_global))	
allocate (z1opt_global (nmax_v_global))			
allocate (z2opt_global (nmax_v_global))			
allocate (z3opt_global (nmax_v_global))			

allocate (negal_global(neto_global))
allocate (  nvs_global(neto_global))

allocate (nxi2_global (9,neto_global))
allocate (nxi2r_global(9,neto_global))
allocate (nxit_global (9,neto_global))
allocate (nvar_global (neto_global))

allocate (nvarr_temp_global (neto_global))
allocate (nxitr_temp_global(9,neto_global))

allocate (nvarr_global(neto_global))
allocate (nxitr_global(9,neto_global))

fiopt(:,:) = 0.0d00

xicou (:,:)      =0.0d00
ximin (:,:)      =0.0d00
ximax (:,:)      =0.0d00

 ega  (:,:)      =0.d00
mega  (:,:,:)    =0
  nvs (:,:)      =0
negal (:,:)      =0
negalt(:)        =0

nxi2  (:,:,:)    =0      
nxit  (:,:,:)    =0
nvar  (:,:)      =0

nxitr  (:,:,:)   =0
nvarr  (:,:)     =0
nxi2r  (:,:,:)   =0

ic_rest(:,:)     =0
ratio_rest(:,:)  =0.d00

sopt (:)      =0.d00

xiopt (:,:)   =0.d00
cjopt (:,:)   =0.d00
cjmopt(:,:)	  =0.d00	
cijopt(:,:,:) =0.d00

dxopt(:,:,:)    =0.d00

m1tabl_global(:,:)=0
m2cont_global(:)  =0
m4cont_global(:)  =0
m5cont_global(:)  =0

fiopt_global(:) = 0.0d00

xicou_global (:)      =0.d00
ximin_global (:)      =0.d00
ximax_global (:)      =0.d00

if (ALLOCATED(ega_global)) ega_global(:)= 0.d00
if (ALLOCATED(mega_global)) mega_global(:,:) = 0        !TODO check pq ces vecteurs sont ici
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

shift_opt (:)=0

igeom_index(:,:)=0
igrav_index(:,:)=0
inert_index(:,:)=0
imod_index(:,:)=0
iweight_index(:,:)=0
iprice_index(:,:)=0
istruc_index(:,:)=0
irestr_index(:,:)=0

ig_conlin(:)=0
jt_conlin(:)=0
!ms_conlin(:)=0


allocate (bornestiffmin_global(nmax_sup))
allocate (bornestiffmax_global(nmax_sup))
bornestiffmin_global(:)=0.d00
bornestiffmax_global(:)=0.d00

allocate (bornestiffmin(nmax_sup,nfile))
allocate (bornestiffmax(nmax_sup,nfile))
bornestiffmin(:,:)=0.d00
bornestiffmax(:,:)=0.d00

allocate (bornestiff_global(nmax_sup))
bornestiff_global(:)=0.d00
allocate (bornestiff(nmax_sup,nfile))
bornestiff(:,:)=0.d0

allocate (bornestiffmode_global(nmax_sup))
bornestiffmode_global(:)=' '
allocate (bornestiffmode(nmax_sup,nfile))
bornestiffmode(:,:)=' '

allocate (bornestifflargeur_global(nmax_sup))
bornestifflargeur_global(:)=0.d00
allocate (bornestifflargeur(nmax_sup,nfile))
bornestifflargeur(:,:)=0.d0


vnom2_global(:)=' '
vnom2(:,:)=' '

return 
end
