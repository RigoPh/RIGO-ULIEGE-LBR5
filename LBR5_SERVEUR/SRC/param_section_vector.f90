module param_section_vector

use param_section, ONLY : lbr5_element

character*80,allocatable, save ::      nom_vector(:)
character*80,allocatable, save ::  header1_vector(:)
character*80,allocatable, save ::  header2_vector(:)

character*80,allocatable, save ::   iunit1_vector(:)
character*80,allocatable, save ::   iunit2_vector(:)
character*80,allocatable, save ::   iunit3_vector(:)
character*80,allocatable, save ::   iunit4_vector(:)
character*80,allocatable, save ::   iunit5_vector(:)	   
character*80,allocatable, save ::   iunit6_vector(:)
character*80,allocatable, save ::   iunit7_vector(:)
character*80,allocatable, save ::   iunit8_vector(:)
character*80,allocatable, save ::   iunit9_vector(:)
character*80,allocatable, save ::  iunit12_vector(:)

integer*4,allocatable, save ::        nnsol_vector(:,:)

integer*4,allocatable, save ::        neto_vector(:)
integer*4,allocatable, save ::        ngmax_vector(:)
integer*4,allocatable, save ::        iana_vector(:)
integer*4,allocatable, save ::      indaig_vector(:)
integer*4,allocatable, save ::      indrai_vector(:)
integer*4,allocatable, save ::       ibusc_vector(:)
integer*4,allocatable, save ::      ipoids_vector(:)
integer*4,allocatable, save ::       nsolm_vector(:)
integer*4,allocatable, save ::       nsol_fatigue_vector(:)
integer*4,allocatable, save ::       nsolm_fatigue_vector(:)
integer*4,allocatable, save ::        nsol_vector(:)
integer*4,allocatable, save ::          ne_vector(:)
integer*4,allocatable, save ::        imax_vector(:)
integer*4,allocatable, save ::        jlph_vector(:)
integer*4,allocatable, save ::       jlph2_vector(:)
integer*4,allocatable, save ::      iprint_vector(:)


double precision,allocatable, save ::          depth_vector(:)
double precision,allocatable, save ::         length_vector(:)		
double precision,allocatable, save ::          width_vector(:)

double precision, allocatable, save :: part_full_vector(:)
       integer*4, allocatable, save ::    method_fatigue_vector(:)
double precision, allocatable, save ::       tfl_vector(:)

double precision,allocatable, save ::  dis_vector(:,:)
double precision,allocatable, save ::  fam_vector(:,:)

character*80    ,allocatable, save :: casename_vector(:,:)
integer*4       ,allocatable, save :: NNSOL_BV_TYPE_vector(:,:)		!fat_new

integer*4       ,allocatable, save :: is_loadcase_fatigue_vector(:,:)

double precision,allocatable, save :: tirant_vector(:,:)
integer*4       ,allocatable, save :: is_selected_loadcase_fatigue_vector(:,:)

integer*4, allocatable, save ::  imulti_vector(:)
double precision,allocatable, save ::  w1_vector(:)							
double precision,allocatable, save ::  w2_vector(:)
double precision,allocatable, save ::  w3_vector(:)
integer*4, allocatable, save	   ::  rho_vector(:)
double precision,allocatable, save ::  fk_vector(:,:)

integer*4, allocatable, save ::  ipareto_vector(:)
integer*4, allocatable, save ::  nsteppar_vector(:)
integer*4, allocatable, save ::  iparcout_vector(:)
integer*4, allocatable, save ::  iparpoids_vector(:)
integer*4, allocatable, save ::  iparinertia_vector(:)

integer*4,allocatable,save :: icout_vector(:),iredress_vector(:)
integer*4,allocatable,save :: refthick_input_vector(:)

double precision, allocatable, save :: poids_vector(:),spoids_vector(:)

double precision, allocatable, save :: fmat11_vector(:),fmat22_vector(:),fmat33_vector(:)
double precision, allocatable, save :: fsou11_vector(:),fsou22_vector(:),fsou33_vector(:)
double precision, allocatable, save :: fmdo11_vector(:),fmdo22_vector(:),fmdo33_vector(:)
double precision, allocatable, save :: fmdo44_vector(:),fmdo55_vector(:),fmdo66_vector(:)
double precision, allocatable, save :: fmdo77_vector(:)   

double precision, allocatable, save :: rend_global_vector(:) !,eqp_global_vector(:)

!	variables Life Cycle Cost

integer*4,allocatable, save :: lcc2_vector(:),lcc3_vector(:),lcc4_vector(:),lcc5_vector(:)
double precision, allocatable, save :: lightweight_vector(:),lightweight_init_vector(:),deadweight_vector(:),deadweight_init_vector(:),poidsLBR5_init_vector(:)
integer*4,allocatable, save :: lccscenario_vector(:)

!!! Paramètres matériaux

integer*4,allocatable, save ::        nbrMat_vector(:)

double precision,allocatable, save ::  e_vector(:,:)
double precision,allocatable, save ::  eta_vector(:,:)
double precision,allocatable, save ::  sigy_vector(:,:)
double precision,allocatable, save ::  sigyadm_vector(:,:)
double precision,allocatable, save ::  coefk_vector(:,:)					
double precision,allocatable, save ::  spec_vector(:,:)

double precision, allocatable, save :: dref_vector(:,:),drefx_vector(:,:),drefy_vector(:,:)

double precision, allocatable, save :: dref_b_vector(:,:),dref_l_vector(:,:),dref_c_vector(:,:)

double precision, allocatable, save :: c1_vector(:,:),dc1_vector(:,:)
double precision, allocatable, save :: c8_vector(:,:),dc8_vector(:,:)

double precision, allocatable, save :: c2_vector(:,:), c3_vector(:,:),cout_vector(:,:)

double precision, allocatable, save :: c_pb_vector(:,:),c_tc_vector(:,:),c_tl_vector(:,:)

double precision, allocatable, save :: dw2_vector(:,:),dw3_vector(:,:)
double precision, allocatable, save :: dw_b_vector(:,:),dw_l_vector(:,:),dw_c_vector(:,:)

double precision, allocatable, save :: labour_cost_vector(:,:)

double precision, allocatable, save :: p4_vector(:,:),dp4_vector(:,:)
double precision, allocatable, save :: p5_vector(:,:),dp5_vector(:,:)
double precision, allocatable, save :: p6_vector(:,:)
double precision, allocatable, save :: p7_vector(:,:)

double precision, allocatable, save :: p9x_vector(:,:),dp9x_vector(:,:)
double precision, allocatable, save :: p9y_vector(:,:),dp9y_vector(:,:)

double precision, allocatable, save :: p10_vector(:,:),dp10_vector(:,:)

double precision, allocatable, save :: ber_vector(:,:),bet_vector(:,:)

integer*4,allocatable, save :: ialr_vector(:,:),ialt_vector(:,:)

! 
type(lbr5_element), allocatable,save :: panneau_vector(:,:)

double precision, allocatable, save ::  aono_vector(:,:,:)
double precision, allocatable, save ::  z_vector(:,:,:)
double precision, allocatable, save ::  noeud_vector(:,:,:)

integer*4,allocatable,save		 ::  indMateriau_vector(:,:)

character*3,     allocatable,save ::   mode_vector  (:,:)
character*3,     allocatable,save ::  modes_vector  (:,:)
double precision, allocatable, save ::  xmode_vector  (:,:)

integer*4       ,allocatable,save ::  kse_vector   (:,:)
integer*4       ,allocatable,save ::  ipa_vector   (:,:)
integer*4       ,allocatable,save ::  ivaria_vector(:,:)

double precision, allocatable, save ::  xi_vector(:,:,:)
double precision, allocatable, save ::  xf_vector(:,:,:)

integer*4       ,allocatable,save ::  m1tabl_vector    (:,:,:)
integer*4       ,allocatable,save ::  m2cont_vector      (:,:)
integer*4       ,allocatable,save ::  m4cont_vector      (:,:)
integer*4       ,allocatable,save ::  m5cont_vector      (:,:)

integer*4       ,allocatable,save ::  noh_vector       (:,:,:)

double precision, allocatable, save ::  tfa_vector         (:,:)
double precision, allocatable, save ::  tfr_vector         (:,:)

double precision, allocatable, save ::  ypt9_vector      (:,:,:)
double precision, allocatable, save ::  ypts_vector      (:,:,:)
double precision, allocatable, save ::  abcd_vector      (:,:)

integer*4       ,allocatable,save ::  npt_full_vector(:,:,:)
double precision, allocatable, save ::  cha_full_vector(:,:,:,:,:)
integer*4       ,allocatable,save ::  npt_vector(:,:,:)
double precision, allocatable, save ::  cha_vector(:,:,:,:,:)


character*80,    allocatable,save ::  title_cha_full_vector(:,:,:)
character*80,    allocatable,save ::  title_m1_vector (:,:,:)

!character*80,    allocatable ::  title_cha_vector      (:,:,:)
character*80,    allocatable,save ::  title_loadcase_vector (:,:,:)

double precision, allocatable, save ::  bm1_vector(:,:)
double precision, allocatable, save ::  bm2_vector(:,:)
double precision, allocatable, save ::  bm3_vector(:,:)
double precision, allocatable, save ::  bm4_vector(:,:)

double precision, allocatable, save ::  sf1_vector(:,:)
double precision, allocatable, save ::  sf3_vector(:,:)

double precision, allocatable, save ::  bm11_vector(:,:)
double precision, allocatable, save ::  bm31_vector(:,:)
double precision, allocatable, save ::  sf11_vector(:,:)
double precision, allocatable, save ::  sf31_vector(:,:)

!*******************************************************************************************
!===========================================================================================
!!ajout des variables de travail relatives à la routine Effort Tranchant
!===========================================================================================

double precision, allocatable, save :: TEffortT_vector(:,:)
double precision, allocatable, save :: XEffortT_vector(:,:)
double precision, allocatable, save :: MEffortT_vector(:,:)
double precision, allocatable, save :: PEffortT_vector(:,:)
double precision, allocatable, save :: varoli_vector(:,:)
integer*4, allocatable, save :: pointeur_vector(:,:)
double precision, allocatable, save :: aqoli_vector(:,:)
double precision, allocatable, save :: FFF_vector(:,:)
double precision, allocatable, save :: bornestiffmin_vector(:,:)
double precision, allocatable, save :: bornestiffmax_vector(:,:)
!*******************************************************************************************
!===========================================================================================

!double precision, allocatable, save ::  m1_vector   (:,:,:)

integer*4,allocatable,save		 :: itype_vector(:,:)
integer*4,allocatable,save		 :: isect_vector(:,:)

! Stiffener
double precision, allocatable, save :: sx_raid_sem_vector(:,:,:,:,:)
double precision, allocatable, save :: sx_loc_raid_sem_vector(:,:,:) ! Contrainte locale uniquement ou combinaison ?

! Plate
double precision, allocatable, save :: sx_plaque_top_vector(:,:,:,:,:)
double precision, allocatable, save :: sy_plaque_top_vector(:,:,:,:,:)
double precision, allocatable, save :: sxy_plaque_top_vector(:,:,:,:,:)
double precision, allocatable, save :: sx_plaque_bott_vector(:,:,:,:,:)
double precision, allocatable, save :: sy_plaque_bott_vector(:,:,:,:,:)
double precision, allocatable, save :: sxy_plaque_bott_vector(:,:,:,:,:)

! Frame
	!sx_plaque_top
double precision, allocatable, save :: sy_cadre_jab_vector(:,:,:,:,:)
double precision, allocatable, save :: sxy_cadre_jab_vector(:,:,:,:,:)
double precision, allocatable, save :: sy_cadre_sem_vector(:,:,:,:,:)
! fin 

! param_restri_global_vector
double precision, allocatable, save :: xk_vector(:)
double precision, allocatable, save :: yk_vector(:) 	
double precision, allocatable, save :: xkgmin_vector(:)
double precision, allocatable, save :: xkgmax_vector(:)
double precision, allocatable, save :: modmin_vector(:)
double precision, allocatable, save :: imin_vector(:)
double precision, allocatable, save :: wmax_vector(:)
double precision, allocatable, save :: pricemax_vector(:)
double precision, allocatable, save :: uhogm_vector(:)
double precision, allocatable, save :: usagm_vector(:)
double precision, allocatable, save :: yred_vector (:)

integer*4, allocatable, save :: igrav_vector(:)
integer*4, allocatable, save :: inert_vector(:)
integer*4, allocatable, save :: imod_vector(:)
integer*4, allocatable, save :: imom_vector(:)
integer*4, allocatable, save :: irestr_vector(:)
integer*4, allocatable, save :: iweight_vector(:)
integer*4, allocatable, save :: iprice_vector(:)
integer*4, allocatable, save :: ielt_vector(:)
integer*4, allocatable, save :: icost_vector(:)
integer*4, allocatable, save :: ncondi_vector(:)
integer*4, allocatable, save :: nno_vector(:,:,:)
integer*4, allocatable, save :: nno9_vector(:,:,:)
! fin param_restri_global_vector

end
