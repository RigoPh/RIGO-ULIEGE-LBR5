module param_section



!param_init
integer*4    impr2,impr
integer*4    dessin
!integer*4 methodEffortT
!fin param_init 


integer*4, save ::    neto
integer*4    iana
integer*4    indaig,indrai,ibusc
integer*4    ipoids
integer*4    nsolm
integer*4    nsol
integer*4    nsol_fatigue
integer*4    nsolm_fatigue
integer*4    nel_fatigue
integer*4    Fat_pts
integer*4    ne
integer*4    imax
integer*4    jlph
integer*4    jlph2
integer*4    iprint
integer*4    method_fatigue	
integer*4    sens_finite_diff
integer*4    IS_FATIGUE			!fat_new
integer*4    nbr_fat_max		!fat_new
integer*4    ix_max				!fat_new
integer*4    iy_max				!fat_new



integer*4 icout,iredress
integer*4 refthick_input

double precision poids,spoids,poid1,spoid1

double precision fmat11,fmat22,fmat33
double precision ffmat1,ffmat2,ffmat3
double precision fsou11,fsou22,fsou33
double precision fmdo11,fmdo22,fmdo33
double precision fmdo44,fmdo55,fmdo66
double precision fmdo77 
double precision fmdo1,fmdo2,fmdo3
double precision fmdo4,fmdo5,fmdo6
double precision fmdo7   
double precision fct,ffmat,ffsou,ffmdo
double precision fmat,fsou,fmdo
double precision dcor,deltac,dyac,tyac,dxrc,txrc,epaisc

double precision rend_global !,eqp_global

!	variables Life Cycle Cost
integer*4 lcc2,lcc3,lcc4,lcc5
double precision    lightweight,lightweight_init,deadweight,deadweight_init,poidsLBR5_init
integer*4 lccscenario

integer*4              imulti,rho
double precision       w1,w2,w3,fk(3)							
integer*4              ipareto, nsteppar, iparcout, iparpoids, iparinertia

integer*4, allocatable, save :: nnsol(:)


double precision length						
double precision width

integer*4    idebug
integer*4    idebug_conlin

double precision       part_full
double precision       tfl		
double precision       h0,ha,TA,CFL,Ntfl,thF,ksi0,SigmaQ,mi,kpSN		!,nz
double precision       h_Weibull,nze,Ntcyc


integer*4 caschge_fatigue
integer*4 index_fatigue


type lbr5_element
	character*9 types			!plaque, coque ou epontille
	character*9 section			!section si épontille

	double precision delta		!épaisseur tôle
	double precision hya		!hauteur âme cadre
	double precision dya		!épaisseur âme cadre
	double precision wya		!largeur semelle cadre
	double precision tya		!épaisseur semelle cadre
	double precision epsa		!écartement cadre
	double precision hxr		!hauteur âme raidisseur
	double precision dxr		!épaisseur âme raidisseur
	double precision wxr		!largeur semelle raidisseur
	double precision txr		!épaisseur semelle raidisseur
	double precision epsr		!écartement raidisseur
	double precision entr		!écartement raidisseur
	integer*4 kse               !précise le sens des forces sur les panneaux

	double precision part		!coefficient de participation

	double precision wya_fatigue
	double precision tya_fatigue
	double precision wxr_fatigue
	double precision txr_fatigue

	integer*4 itype_longit		!=14 Plat boudin; =7 T Composite; =17 T Laminé
	integer*4 itype_transv		!=14 Plat boudin; =7 T Composite; =17 T Laminé
	integer*4 itype_transv_second
	integer*4 itype_longit_second
	character*80 name_type_transv			!REM: à part les 2 1ers, il semble
	character*80 name_type_transv_second	!     qu'on n'utilise pas les autres.
	character*80 name_type_longit			! TODO : voir si on peut supprimer !
	character*80 name_type_longit_second
    double precision hya2		!hauteur âme cadre secondaire
	double precision dya2		!épaisseur âme cadre secondaire
	double precision wya2		!largeur semelle cadre secondaire
	double precision tya2		!épaisseur semelle cadre secondaire
	double precision epsa2		!écartement cadre secondaire
	double precision hxr2		!hauteur âme raidisseur secondaire
	double precision dxr2		!épaisseur âme raidisseur secondaire
	double precision wxr2		!largeur semelle raidisseur secondaire
	double precision txr2		!épaisseur semelle raidisseur secondaire
	double precision epsr2		!écartement raidisseur secondaire
	double precision entr2		!écartement raidisseur secondaire
	integer*4 ksr2		
	integer*4 ksa2
	double precision dvmin(9)	!borne min des var de conception
	double precision dvmax(9)	!borne max des var de conception
	double precision heff		!hauteur sans hiloire (si épontille)
	double precision corro(3)	!épaisseurs de corrosion
	integer*4 noh9(10)			!Liste des 10 panneaux qui suivent
	double precision, pointer :: xi_full(:) !Pressions locales
	double precision, pointer :: xf_full(:)
	
	integer*4 m2				!nombre de restrictions géométrique
	integer*4 im1				!=0 => pas de resti struc; =1 => restri struct
	integer*4 m1cont			!nbr de restrictions struct. pour tout le panneau
	integer*4, pointer :: m1(:) !nbre de restrictions struct. pour chaque cas de charge
	integer*4 ibm2				!nø du set de restrictions
	integer*4 isema				!coefficient pour la modif automatique des semelles
	integer*4 isemr
	integer*4 inda				!coeff qui indique si restri semelle respectée ou non (TODO : check que c'est ça)
	integer*4 indr
	integer*4 lm2(40)			!liste des restri géom 
								!TODO normalement mettre ça dynamique !!!
	double precision delt		!epaisseur moyenne (raidisseurs et traverses compris) Calculé dans CARAC
	double precision delt2		!epaisseur moyenne (raidisseurs, cadres et traverses compris) Calculé dans CARAC
	double precision xneu		!coord du centre de gravite d'un panneau dans repere utilisateur Calculé dans CARAC
	double precision yneu		!coord du centre de gravite d'un panneau dans repere utilisateur Calculé dans CARAC
	double precision d1			!(d1,d2)= coord du centre des axes neutres du panneau j Calculé dans COORD
	double precision d2			!(d1,d2)= coord du centre des axes neutres du panneau j Calculé dans COORD
	double precision omega		!section du panneau j (sans cadre)
	double precision omega2		!section du panneau j (avec cadre)
	double precision const(74)	!coefficients de raideur du panneau
	double precision const2(6,9)
	double precision const3(2,10)
	double precision, pointer :: chamax(:,:)   !(5,nsol) Pressions max pour la subr hughes (et stiff)
	double precision, pointer :: dchamax(:,:,:)!(5,nsol,9)
	double precision q
	double precision phil
	double precision epais		!variable de conception pour épontille
	double precision hight		!hauteur du panneau
	character*3		 mode		!Mode pour raidisseur (EE1 ou EE2)
	double precision teta
	double precision tetas
    integer*4 mt				!nombre de traverse sur le panneau
	integer*4 mmt               !numero de la traverse
	integer*4 ksr
	integer*4 ksa
	integer*4 kst
    double precision hxtr(10)	!relatifs aux traverses
	double precision dxtr(10)	!relatifs aux traverses
	double precision wxtr(10)	!relatifs aux traverses
	double precision txtr(10)	!relatifs aux traverses
	double precision abtr(10)	!relatifs aux traverses
	double precision abtr2(10)	!position des traverses
	integer*4		 ipts3(10)  !numéro de la traverse concernée 
	double precision sh			!sh=e(nel)/(2.*(1.+eta(nel)))
	double precision ang		!angle, comme écrit dans le fichier txt
	double precision angle		!=ang mais est modifié dans le programme
	integer*4 code				!code
	integer*4 fami				!redressage
	integer*4 lot				!redressage

	double precision rend		!rendement par panneau
	
	double precision ploc		!pression locale

	integer*4 icha				!charge variable selon x (0=non)
	integer*4 elmt_vib			!numéro de l'élément de vibration
	double precision long_vib	!longueur prise en compte pour l'élément de vibration
	integer*4 cl_av_vib			!condition limite avant (vibration)
	integer*4 cl_ar_vib			!condition limite arrière (vibration)
	integer*4, pointer ::		 lcont(:,:) ! Paramètres relatifs aux restrictions structurelles
	integer*4					 ipts		  ! Nbr de points de calcul des restrictions structurelles
	integer*4		 ipts2(4)	  ! (varie de 1 à panneau(nel).ipts) !!! TODO A CHANGER SI iptmax CHANGE !!!
								! (mais ça bug qd pointeur)
	double precision, pointer :: cjmax(:)	  ! valeur max de la restrictions structurelle
	integer*4, pointer ::        inv(:)		  ! Indique s'il s'agit d'une borne min ou max
	integer*4, pointer ::		 lcont4_init(:,:,:)	!Relatifs aux restrictions structurelles
	integer*4, pointer ::		 lcont4(:,:,:)		!Relatifs aux restrictions structurelles
	integer*4, pointer ::		 inv3_init(:,:)			!Relatifs aux restrictions structurelles
	integer*4, pointer ::		 inv3(:,:)			!Relatifs aux restrictions structurelles
	double precision, pointer :: cjmax9_init(:,:)		!Relatifs aux restrictions structurelles
	double precision, pointer :: cjmax9(:,:)		!Relatifs aux restrictions structurelles

	integer*4 nsloshm			!nbre de cas de contraintes de sloshing pour ce panneau
	integer*4 nslosh(3)			!liste des contraintes de sloshing étudiées
	double precision slosh(3)	!valeur des contraintes de sloshing étudiées
	double precision press_slosh !valeur de la pression de sloshing (= seul paramètre d'entrée)

	double precision aire		! paramètres relatifs aux épontilles
	double precision aiy
	double precision aix
	double precision sredy
	double precision sredx
	double precision tork
		
	!!! Paramètres calculés dans BO2
	double precision plat
	double precision fl
	double precision sig1
	double precision sig2
	double precision sig3
	double precision sig4
	double precision tau
	double precision wpl1
	double precision wpl2
	double precision vmaxsig
	double precision vmaxsigc
	double precision sigmag
	double precision, pointer :: sigplaque(:)
	double precision, pointer :: dsigplaque(:)
	double precision, pointer :: sigxplaque(:)
	double precision, pointer :: sigyplaque(:)
	double precision, pointer :: phiplaque(:)
	double precision, pointer :: sigvmtplaque(:)
	double precision, pointer :: sigvmcplaque(:)
	integer*4, pointer		  :: indplaque(:)
	double precision, pointer :: sigmx(:)
	double precision, pointer :: sigmy(:)
		!!!		

	! Définition des paramètres relatifs à la fatigue
	double precision kcor    				!fat_new   

	integer*4 I_Fat_Stif
	integer*4 nbr_Fat_Stif
	integer*4, pointer :: ix_Stif(:)
	integer*4, pointer :: iy_Stif(:)
	integer*4, pointer :: ES_Stif(:)
	double precision, pointer :: Kw_Stif(:)
	double precision, pointer :: Kt_Stif(:)
	double precision, pointer :: lambda_stif(:)
	double precision, pointer :: teta_stif(:)
	double precision, pointer :: BetaIf_stif(:)
	integer*4, pointer :: m_stif(:)
	double precision, pointer :: c_stif(:)
	double precision, pointer :: weib_fact_stif(:)

	integer*4 I_Fat_Plate
	integer*4 nbr_Fat_Plate
	integer*4, pointer :: ix_Plate(:)
	integer*4, pointer :: iy_Plate(:)
	!integer*4, pointer :: ES_Plate(:)
	double precision, pointer :: KT_Plate(:)
	double precision, pointer :: Kw_Plate(:)
	double precision, pointer :: lambda_Plate(:)
	double precision, pointer :: teta_Plate(:)
	double precision, pointer :: BetaIf_Plate(:)
	integer*4, pointer :: m_Plate(:)
	double precision, pointer :: c_Plate(:)
	double precision, pointer :: weib_fact_Plate(:)

	integer*4 I_Fat_Plate_Frame
	integer*4 nbr_Fat_Plate_Frame
	integer*4, pointer :: ix_Plate_Frame(:)
	integer*4, pointer :: iy_Plate_Frame(:)
	!integer*4, pointer :: ES_Plate_Frame(:)
	double precision, pointer :: KT_Plate_Frame(:)
	double precision, pointer :: Kw_Plate_Frame(:)
	double precision, pointer :: lambda_Plate_Frame(:)
	double precision, pointer :: teta_Plate_Frame(:)
	double precision, pointer :: BetaIf_Plate_Frame(:)
	integer*4, pointer :: m_Plate_Frame(:)
	double precision, pointer :: c_Plate_Frame(:)
	double precision, pointer :: weib_fact_Plate_Frame(:)

	integer*4 I_Fat_Frame
	integer*4 nbr_Fat_Frame
	integer*4, pointer :: ix_Frame(:)
	integer*4, pointer :: iy_Frame(:)
	integer*4, pointer :: ES_Frame(:)
	double precision, pointer :: KT_Frame(:)
	double precision, pointer :: Kw_Frame(:)
	double precision, pointer :: lambda_Frame(:)
	double precision, pointer :: teta_Frame(:)
	double precision, pointer :: BetaIf_Frame(:)
	integer*4, pointer :: m_Frame(:)
	double precision, pointer :: c_Frame(:)
	double precision, pointer :: weib_fact_Frame(:)
	!


!	double precision, pointer :: I_Fat_Girder(:)			!fat_new
!	double precision, pointer :: nbr_Fat_Girder(:)			!fat_new
!	double precision, pointer :: ix_Girder(:,:)			!fat_new	
!	double precision, pointer :: ES_Girder(:,:)			!fat_new	
!	double precision, pointer :: Kw_Girder(:,:)			!fat_new
!	double precision, pointer :: Kt_Girder(:,:)			!fat_new
!	double precision, pointer :: lambda_Girder(:,:)			!fat_new
!	double precision, pointer :: teta_Girder(:,:)			!fat_new
!	double precision, pointer :: BetaIf_Girder(:,:)			!fat_new
!	integer*4, pointer :: m_Girder(:,:)			!fat_new
!	double precision, pointer :: c_Girder(:,:)			!fat_new
!	double precision, pointer :: weib_fact_Girder(:,:)			!fat_new

!    integer, allocatable, save ::  MT(:)
 

!    integer, allocatable, save ::  I_Fat_Pillar(:)
!    integer, allocatable, save ::  nbr_pts_Pillar(:)
!    integer, allocatable, save ::  ix_Pillar(:,:)
!    integer, allocatable, save ::  iy_Pillar(:,:)
!    integer, allocatable, save ::  Es_Pillar(:,:)!
!	double precision, allocatable, save :: Kw_Pillar(:,:)
!	double precision, allocatable, save :: Kt_hs_Pillar(:,:)
!	double precision, allocatable, save :: LAMBDA_Pillar(:,:)
 !   double precision, allocatable, save ::  TETA_Pillar(:,:)
!	double precision, allocatable, save :: BetaIF_Pillar(:,:)
!	double precision, allocatable, save :: Weib_fact_Pillar(:,:)
 !   integer*4, save ::  m_Pillar(:,:)
  !  double precision, allocatable, save :: C_Pillar(:,:)




    integer*4         nfatiguem
	double precision  cjmax_fatigue
!    integer*4         i_fatigue_stiffened_panel(4)				!fat_new
!    integer*4         i_fatigue_frame							!fat_new
!    integer*4         i_fatigue_girder							!fat_new

!    integer*4         number_points_considered						!fat_new
!    integer*4         ix_stiffened_pannel(9)						!fat_new
!    integer*4         iy_stiffened_pannel(9)						!fat_new

!		type(fatigue_structural_element)   fatigue_stiffened_panel(10,3, 3),&   ! 3 selon x, 3  selon y, 10 cas de charge
!                                          fatigue_girder         (10,3,10),&   ! 3 selon x, 10 girders, 10 cas de charge
!                                          fatigue_frame          (10,3, 2)     ! 3 selon x, 2  selon y, 10 cas de charge
!	double precision weib_fact				!fat_new
!	double precision h_weibull				!fat_new
!
 !   double precision ptyr,esr,lbr,wbr,tbr				!fat_new
  !  double precision ptya,esa,lba,wba,tba 				!fat_new

   ! double precision ptyg(10),esg(10),lbg(10),wbg(10),tbg(10) 				!fat_new

!    double precision kt_hs_1r				!fat_new
!	double precision kt_hs_2r				!fat_new
!	double precision kt_hs_3r				!fat_new
!	double precision kt_hs_4r	    !HOT SPOT FACTOR (STIFFENERS)
 !   double precision kt_hs_1a				!fat_new
!	double precision kt_hs_2a				!fat_new
!	double precision kt_hs_3a				!fat_new
!	double precision kt_hs_4a	    !HOT SPOT FACTOR (FRAME)

!    double precision kt_hs_1g(10)				!fat_new
!	double precision kt_hs_2g(10)				!fat_new
!	double precision kt_hs_3g(10)				!fat_new
!	double precision kt_hs_4g(10)	!HOT SPOT FACTOR (GIRDER)			


!    double precision betaif				!fat_new
                                 				
 !                                   
  !  double precision tm 				!fat_new                                        
   ! double precision tc    				!fat_new                                      
!    double precision toe_angle(4)     				                                    
 !   double precision lambda_weld(4)				!fat_new
    character*80 panel(9)

 !   double precision nsy				!fat_new
  !  double precision nsz				!fat_new
   ! double precision as				!fat_new
!    double precision iy				!fat_new
 !   double precision iz				!fat_new
  !  double precision ksi(10)				!fat_new
   ! double precision ntcyc				!fat_new

        ! hot spot factor (stiffeners)
!    double precision kt_hs_1		 !fat_new
 !   double precision kt_hs_2		 !fat_new
  !  double precision kt_hs_3		 !fat_new
   ! double precision kt_hs_4		 !fat_new

!    double precision length_girder (10)
 !   double precision epstr(10)
!	double precision girder_length(10)

end type
  


type(lbr5_element), allocatable, save :: panneau(:)

integer*4 kcontr,ncont

double precision, allocatable, save :: sxm(:)

!====================================================================
! fatigue 
!====================================================================


double precision, allocatable, save :: damage_stiffened_pannel(:,:,:)
double precision, allocatable, save :: damage_fatigue(:)
integer*4,        allocatable, save :: index_damage_stiffened_pannel(:)

double precision, allocatable, save :: ddamage_fatigue(:,:,:)

!	stiffened plate sty=1 

double precision, allocatable, save :: dfull_11(:,:,:)
double precision, allocatable, save :: dfull_12(:,:,:)
double precision, allocatable, save :: dfull_13(:,:,:)
double precision, allocatable, save :: dfull_14(:,:,:)

!	girder sty=2 and sty2=1

double precision, allocatable, save :: dfull_21_1(:,:,:)
double precision, allocatable, save :: dfull_22_1(:,:,:)
double precision, allocatable, save :: dfull_23_1(:,:,:)
double precision, allocatable, save :: dfull_24_1(:,:,:)

!	web frame  sty=2 sty2=2

double precision, allocatable, save :: dfull_21_2(:,:,:)
double precision, allocatable, save :: dfull_22_2(:,:,:)
double precision, allocatable, save :: dfull_23_2(:,:,:)
double precision, allocatable, save :: dfull_24_2(:,:,:)


!	stiffened plate sty=1 

double precision, allocatable, save :: dball_11(:,:,:)
double precision, allocatable, save :: dball_12(:,:,:)
double precision, allocatable, save :: dball_13(:,:,:)
double precision, allocatable, save :: dball_14(:,:,:)

!	girder sty=2 and sty2=1

double precision, allocatable, save :: dball_21_1(:,:,:)
double precision, allocatable, save :: dball_22_1(:,:,:)
double precision, allocatable, save :: dball_23_1(:,:,:)
double precision, allocatable, save :: dball_24_1(:,:,:)

!	web frame  sty=2 sty2=2

double precision, allocatable, save :: dball_21_2(:,:,:)
double precision, allocatable, save :: dball_22_2(:,:,:)
double precision, allocatable, save :: dball_23_2(:,:,:)
double precision, allocatable, save :: dball_24_2(:,:,:)

!	stiffened plate sty=1 

double precision, allocatable, save :: ddfull_11(:,:,:,:,:)
double precision, allocatable, save :: ddfull_12(:,:,:,:,:)
double precision, allocatable, save :: ddfull_13(:,:,:,:,:)
double precision, allocatable, save :: ddfull_14(:,:,:,:,:)

!	girder sty=2 and sty2=1
double precision, allocatable, save :: ddfull_21_1(:,:,:,:,:)
double precision, allocatable, save :: ddfull_22_1(:,:,:,:,:)
double precision, allocatable, save :: ddfull_23_1(:,:,:,:,:)
double precision, allocatable, save :: ddfull_24_1(:,:,:,:,:)

!	web frame  sty=2 sty2=2
double precision, allocatable, save :: ddfull_21_2(:,:,:,:,:)
double precision, allocatable, save :: ddfull_22_2(:,:,:,:,:)
double precision, allocatable, save :: ddfull_23_2(:,:,:,:,:)
double precision, allocatable, save :: ddfull_24_2(:,:,:,:,:)
  
!	stiffened plate sty=1 

double precision, allocatable, save :: ddball_11(:,:,:,:,:)
double precision, allocatable, save :: ddball_12(:,:,:,:,:)
double precision, allocatable, save :: ddball_13(:,:,:,:,:)
double precision, allocatable, save :: ddball_14(:,:,:,:,:)

!	girder sty=2 and sty2=1
double precision, allocatable, save :: ddball_21_1(:,:,:,:,:)
double precision, allocatable, save :: ddball_22_1(:,:,:,:,:)
double precision, allocatable, save :: ddball_23_1(:,:,:,:,:)
double precision, allocatable, save :: ddball_24_1(:,:,:,:,:)

!	web frame  sty=2 sty2=2

double precision, allocatable, save :: ddball_21_2(:,:,:,:,:)
double precision, allocatable, save :: ddball_22_2(:,:,:,:,:)
double precision, allocatable, save :: ddball_23_2(:,:,:,:,:)
double precision, allocatable, save :: ddball_24_2(:,:,:,:,:)

!	stiffened plate sty=1 

double precision, allocatable, save :: dtot_11(:,:,:)
double precision, allocatable, save :: dtot_12(:,:,:)
double precision, allocatable, save :: dtot_13(:,:,:)
double precision, allocatable, save :: dtot_14(:,:,:)

!	girder sty=2 and sty2=1

double precision, allocatable, save :: dtot_21_1(:,:,:)
double precision, allocatable, save :: dtot_22_1(:,:,:)
double precision, allocatable, save :: dtot_23_1(:,:,:)
double precision, allocatable, save :: dtot_24_1(:,:,:)

!	web frame  sty=2 sty2=2

double precision, allocatable, save :: dtot_21_2(:,:,:)
double precision, allocatable, save :: dtot_22_2(:,:,:)
double precision, allocatable, save :: dtot_23_2(:,:,:)
double precision, allocatable, save :: dtot_24_2(:,:,:)

!	stiffened plate sty=1 

double precision, allocatable, save :: ddtot_11(:,:,:,:,:)
double precision, allocatable, save :: ddtot_12(:,:,:,:,:)
double precision, allocatable, save :: ddtot_13(:,:,:,:,:)
double precision, allocatable, save :: ddtot_14(:,:,:,:,:)

!	girder sty=2 and sty2=1

double precision, allocatable, save :: ddtot_21_1(:,:,:,:,:)
double precision, allocatable, save :: ddtot_22_1(:,:,:,:,:)
double precision, allocatable, save :: ddtot_23_1(:,:,:,:,:)
double precision, allocatable, save :: ddtot_24_1(:,:,:,:,:)

!	web frame  sty=2 sty2=2


double precision, allocatable, save :: ddtot_21_2(:,:,:,:,:)
double precision, allocatable, save :: ddtot_22_2(:,:,:,:,:)
double precision, allocatable, save :: ddtot_23_2(:,:,:,:,:)
double precision, allocatable, save :: ddtot_24_2(:,:,:,:,:)

!====================================================================


double precision, allocatable, save :: dnx11(:,:,:,:)
double precision, allocatable, save :: dny11(:,:,:,:) 
double precision, allocatable, save :: dqx11(:,:,:,:) 
double precision, allocatable, save :: dqy11(:,:,:,:) 
double precision, allocatable, save :: dmx11(:,:,:,:) 
double precision, allocatable, save :: dmy11(:,:,:,:) 
double precision, allocatable, save :: dq11 (:,:,:,:) 


double precision, allocatable, save :: ds_11_1(:,:,:,:)
double precision, allocatable, save :: ds_12_1(:,:,:,:)
double precision, allocatable, save :: ds_13_1(:,:,:,:)
double precision, allocatable, save :: ds_14_1(:,:,:,:)
            
!  hot spot stress range
double precision, allocatable, save :: dshs_11_1(:,:,:,:)
double precision, allocatable, save :: dshs_12_1(:,:,:,:)
double precision, allocatable, save :: dshs_13_1(:,:,:,:)
double precision, allocatable, save :: dshs_14_1(:,:,:,:)

!  notch stress range
double precision, allocatable, save :: dsln_11_1(:,:,:,:)
double precision, allocatable, save :: dsln_12_1(:,:,:,:)
double precision, allocatable, save :: dsln_13_1(:,:,:,:)
double precision, allocatable, save :: dsln_14_1(:,:,:,:)

!  damage
double precision, allocatable, save :: d_11_1(:,:,:,:)
double precision, allocatable, save :: d_12_1(:,:,:,:)
double precision, allocatable, save :: d_13_1(:,:,:,:)
double precision, allocatable, save :: d_14_1(:,:,:,:)


double precision, allocatable, save :: dd_11_1(:,:,:,:,:,:)
double precision, allocatable, save :: dd_12_1(:,:,:,:,:,:)
double precision, allocatable, save :: dd_13_1(:,:,:,:,:,:)
double precision, allocatable, save :: dd_14_1(:,:,:,:,:,:)


double precision, allocatable, save :: dnx21(:,:,:,:)
double precision, allocatable, save :: dny21(:,:,:,:)
double precision, allocatable, save :: dqx21(:,:,:,:)
double precision, allocatable, save :: dqy21(:,:,:,:)
double precision, allocatable, save :: dmx21(:,:,:,:)
double precision, allocatable, save :: dmy21(:,:,:,:)
double precision, allocatable, save :: dq21 (:,:,:,:)
                                
!		nominal stress range
double precision, allocatable, save :: ds_21_1(:,:,:,:)
double precision, allocatable, save :: ds_22_1(:,:,:,:)
double precision, allocatable, save :: ds_23_1(:,:,:,:)
double precision, allocatable, save :: ds_24_1(:,:,:,:)

!		hot spot stress range
double precision, allocatable, save :: dshs_21_1(:,:,:,:)
double precision, allocatable, save :: dshs_22_1(:,:,:,:)
double precision, allocatable, save :: dshs_23_1(:,:,:,:)
double precision, allocatable, save :: dshs_24_1(:,:,:,:)

!		notch stress range
double precision, allocatable, save :: dsln_21_1(:,:,:,:)
double precision, allocatable, save :: dsln_22_1(:,:,:,:)
double precision, allocatable, save :: dsln_23_1(:,:,:,:)
double precision, allocatable, save :: dsln_24_1(:,:,:,:)

!		damage
double precision, allocatable, save :: d_21_1(:,:,:,:)
double precision, allocatable, save :: d_22_1(:,:,:,:)
double precision, allocatable, save :: d_23_1(:,:,:,:)
double precision, allocatable, save :: d_24_1(:,:,:,:)



double precision, allocatable, save :: dd_21_1(:,:,:,:,:,:)
double precision, allocatable, save :: dd_22_1(:,:,:,:,:,:)
double precision, allocatable, save :: dd_23_1(:,:,:,:,:,:)
double precision, allocatable, save :: dd_24_1(:,:,:,:,:,:)




double precision, allocatable, save :: dnx22(:,:,:,:) 
double precision, allocatable, save :: dny22(:,:,:,:) 
double precision, allocatable, save :: dqx22(:,:,:,:) 
double precision, allocatable, save :: dqy22(:,:,:,:) 
double precision, allocatable, save :: dmx22(:,:,:,:) 
double precision, allocatable, save :: dmy22(:,:,:,:) 
double precision, allocatable, save :: dq22 (:,:,:,:) 

!		nominal stress range
double precision, allocatable, save :: ds_21_2(:,:,:,:)
double precision, allocatable, save :: ds_22_2(:,:,:,:)
double precision, allocatable, save :: ds_23_2(:,:,:,:)
double precision, allocatable, save :: ds_24_2(:,:,:,:)

!		hot spot stress range
double precision, allocatable, save :: dshs_21_2(:,:,:,:)
double precision, allocatable, save :: dshs_22_2(:,:,:,:)
double precision, allocatable, save :: dshs_23_2(:,:,:,:)
double precision, allocatable, save :: dshs_24_2(:,:,:,:)

!		notch stress range
double precision, allocatable, save :: dsln_21_2(:,:,:,:)
double precision, allocatable, save :: dsln_22_2(:,:,:,:)
double precision, allocatable, save :: dsln_23_2(:,:,:,:)
double precision, allocatable, save :: dsln_24_2(:,:,:,:)

!		damage
double precision, allocatable, save :: d_21_2(:,:,:,:)
double precision, allocatable, save :: d_22_2(:,:,:,:)
double precision, allocatable, save :: d_23_2(:,:,:,:)
double precision, allocatable, save :: d_24_2(:,:,:,:)

double precision, allocatable, save :: dd_21_2(:,:,:,:,:,:)
double precision, allocatable, save :: dd_22_2(:,:,:,:,:,:)
double precision, allocatable, save :: dd_23_2(:,:,:,:,:,:)
double precision, allocatable, save :: dd_24_2(:,:,:,:,:,:)

integer*4       , allocatable, save :: is_loadcase_fatigue (:)
integer*4       , allocatable, save :: NNSOL_BV_TYPE(:)

!  Fat_NNSOL(I)			list of nsol load cases selected for fatigue analysis
integer*4       , allocatable, save :: Fat_NNSOL(:)


double precision, allocatable, save :: tirant(:)
integer*4       , allocatable, save :: is_selected_loadcase_fatigue(:)

!integer*4,        allocatable, save ::m1(:,:)


character*80    , allocatable, save :: casename(:)

character*80 nom
character*80 header1
character*80 header2

character*80 iunit1
character*80 iunit2
character*80 iunit3
character*80 iunit4
character*80 iunit5	   
character*80 iunit6
character*80 iunit7
character*80 iunit8
character*80 iunit9
!character*80 iunit10
!character*80 iunit11
character*80 iunit12
character*80 iunit13
character*80 iunit14


double precision  dis(5),fam(6)


! dimension max des vecteurs
integer*4    nmax,nemax,nvmax,m1max,m1tmax,m2max,m2tmax,ngmax   
integer*4    iptmax                                             


character*3     , allocatable, save :: modes(:)
integer*4       , allocatable, save :: isect(:)

!double precision, allocatable, save :: eta9(:)
!double precision, allocatable, save :: angle(:)
!double precision, allocatable, save :: ploc(:)
integer*4, allocatable, save :: nsign(:)
!double precision, allocatable, save :: philn(:)
!double precision, allocatable, save :: qn(:)
!integer*4, allocatable, save :: icha(:)
!double precision, allocatable, save :: part(:)  

double precision, allocatable, save :: cha(:,:,:,:)
double precision, allocatable, save :: cha_full(:,:,:,:)
double precision, allocatable, save :: dcha(:,:,:,:)
integer*4, allocatable, save :: npt(:,:)
integer*4, allocatable, save :: npt_full(:,:)

!double precision, allocatable, save :: delt(:)
!double precision, allocatable, save :: delt2(:)
integer*4, allocatable, save :: noh (:,:)

double precision  argq(8),const(74)

double precision, allocatable, save :: ypts(:,:)
double precision, allocatable, save :: ypt9(:,:)
integer*4       , allocatable, save :: m2cont(:)
integer*4       , allocatable, save :: m4cont(:)
integer*4       , allocatable, save :: m5cont(:)

integer*4       , allocatable, save :: m1tabl(:,:)

integer*4       , allocatable, save :: itype(:)

double precision, allocatable, save :: coef(:,:,:)

double precision, allocatable, save :: tetas(:)
double precision, allocatable, save :: tetaq(:)
integer*4       , allocatable, save :: noeud(:,:)
double precision, allocatable, save :: z(:,:)
integer*4       , allocatable, save :: mcomp(:,:)


double precision, allocatable, save :: zsn(:,:)
double precision, allocatable, save :: dzsn(:,:,:)


double precision, allocatable, save :: a(:,:)
double precision, allocatable, save :: b(:,:)
double precision, allocatable, save :: abc(:)
double precision, allocatable, save :: asol(:)
double precision, allocatable, save :: bsol(:)
double precision, allocatable, save :: csol(:)
double precision, allocatable, save :: dsol(:)
double precision, allocatable, save :: amom(:)
double precision, allocatable, save :: bmom(:)
double precision, allocatable, save :: cmom(:)
double precision, allocatable, save :: dmom(:)
double precision, allocatable, save :: abcd(:)


double precision, allocatable, save :: tfa(:)
double precision, allocatable, save :: tfr(:)
!integer*4       , allocatable, save :: izut(:)
!double precision, allocatable, save :: zut(:)
integer*2       , allocatable, save :: kli(:,:)

integer*4    sym
integer*4    iicout								
integer*4    ifonct(2)


double precision, allocatable, save :: bm1(:)
double precision, allocatable, save :: bm2(:)
double precision, allocatable, save :: bm3(:)
double precision, allocatable, save :: bm4(:)
double precision, allocatable, save :: sf1(:)
double precision, allocatable, save :: sf3(:)	


double precision, allocatable, save :: bm11(:)
double precision, allocatable, save :: bm31(:)
double precision, allocatable, save :: sf11(:)
double precision, allocatable, save :: sf31(:)

!*******************************************************************************************
!===========================================================================================
!!Ajout des variables de travail relatives à l'Effort Tranchant 
!===========================================================================================

double precision, allocatable, save :: TEffortT(:)		!effort tranchant
double precision, allocatable, save :: XEffortT(:)		!section etudiée
double precision, allocatable, save :: MEffortT(:)		!moment recalculée
double precision, allocatable, save :: PEffortT(:)		!différence entre pression verticale descendante et ascendante
double precision, allocatable, save :: varoli(:)	        !variable de travail
integer*4, allocatable, save :: pointeur(:)
double precision, allocatable, save :: aqoli(:)
double precision, allocatable, save :: FFF(:)
double precision, allocatable, save :: bornestiffmin(:,:)
double precision, allocatable, save :: bornestiffmax(:,:)
double precision, allocatable, save :: bornestiffmin_global(:)
double precision, allocatable, save :: bornestiffmax_global(:)
double precision, allocatable, save :: bornestiff_global(:)
double precision, allocatable, save :: bornestiff(:,:)
character*80, allocatable, save :: bornestiffmode_global(:)
character*80, allocatable, save :: bornestiffmode(:,:)
double precision, allocatable, save :: bornestifflargeur_global(:)
double precision, allocatable, save :: bornestifflargeur(:,:)
integer*4 Noli
integer*4 stepoliinf
integer*4 stepolisup
double precision Poidsoli

!*******************************************************************************************
!===========================================================================================

!*******************************************************************************************
!===========================================================================================
!!Ajout des variables de travail relatives au TRIMARAN 

!===========================================================================================
integer*4 decktri
integer*4 castrim
double precision longtri
double precision longtot
double precision largeurtri
double precision vitessemax
integer*4, allocatable, save :: ptri(:)		!effort tranchant
double precision, allocatable, save :: GMT(:)
!*******************************************************************************************
!===========================================================================================	

double precision       spoids1,pot

integer*4    isymx,isymy
integer*4    mdp(15,4),mar(15,4)

double precision, allocatable, save :: blocka(:,:)
double precision, allocatable, save :: solt(:,:)
double precision, allocatable, save :: sens1(:,:,:,:)
double precision, allocatable, save :: sens2(:,:,:,:)
double precision, allocatable, save :: sens3(:,:,:,:)
double precision, allocatable, save :: sens4(:,:,:,:)


! variables ushull     ===============================================================================

double precision, allocatable, save :: dsud(:,:)
double precision, allocatable, save :: dsub(:,:)
double precision, allocatable, save :: dsub1(:,:)
double precision, allocatable, save :: dsusu(:,:)
double precision, allocatable, save :: dsusl(:,:)
double precision, allocatable, save :: dh(:,:)
double precision, allocatable, save :: dg(:,:)
double precision, allocatable, save :: dad(:,:)
double precision, allocatable, save :: dab(:,:)
double precision, allocatable, save :: dab1(:,:)
double precision, allocatable, save :: das(:,:)
double precision, allocatable, save :: dult(:)
integer*4       , allocatable, save :: id(:)		
integer*4       , allocatable, save :: ib1(:)		
integer*4       , allocatable, save :: ib(:)		
integer*4       , allocatable, save :: is9(:)		



! variables bo1         ===============================================================================

double precision coeff (32,41  )
double precision dcoeff(32,33,9)

double precision dvara (33,9,16)
double precision dvarb (33,9,16)
double precision dvarc (33,9,16)
double precision dvard (33,9,16)
double precision, allocatable, save :: dvarh (:,:,:,:)
double precision, allocatable, save :: sensh (:,:,:,:)


double precision, allocatable, save :: ali (:)
double precision, allocatable, save :: alix(:,:)
double precision, allocatable, save :: aliz(:,:)

double precision, allocatable, save :: dalix(:,:,:)
double precision, allocatable, save :: daliz(:,:,:)
double precision, allocatable, save :: dhyp (:,:,:)

double precision disa(720)
double precision disb(720)
double precision disc(720)
double precision disd(720)
double precision, allocatable, save :: dish(:,:)


! variables bo2         ===============================================================================

integer*4    img


! variables sensibcout  ===============================================================================

double precision, allocatable, save :: vlarg(:)
double precision, allocatable, save :: vepaiss(:)
double precision, allocatable, save :: vhac(:)
double precision, allocatable, save :: veac(:)
double precision, allocatable, save :: vlsc(:)
double precision, allocatable, save :: vesc(:)
double precision, allocatable, save :: deltacsc(:)
double precision, allocatable, save :: vhar(:)
double precision, allocatable, save :: vear(:)
double precision, allocatable, save :: vlsr(:)
double precision, allocatable, save :: vesr(:)
double precision, allocatable, save :: deltar(:)
double precision, allocatable, save :: entrsc(:)
double precision, allocatable, save :: philsc(:)
double precision, allocatable, save :: qsc(:)
double precision, allocatable, save :: coutpan(:)
double precision, allocatable, save :: costpan(:)
double precision, allocatable, save :: coutmat(:)
double precision, allocatable, save :: vdiffc(:)


!	variables coord     ===============================================================================

double precision omet,omet2,yneut,xneut	
double precision aiyytot,aixxtot,aiuutot,aiyytot2,aixxtot2,aiuutot2	
double precision yneutpart,aixxtotpart,ometpart,omesypart,omesx2,omesy2									
integer*4        nc


double precision, allocatable, save :: aiyy(:)
double precision, allocatable, save :: aixx(:)
double precision, allocatable, save :: iyy2(:)
double precision, allocatable, save :: aiyy2(:)
double precision, allocatable, save :: aixx2(:)
double precision, allocatable, save :: aixxpart(:)
double precision, allocatable, save :: aono(:,:)


!	variables inertia   ===============================================================================

double precision yneutnet,yneutgro,inet,igro							
double precision yneutplnet,yneutplgro,iplnet,iplgro		
	
double precision, allocatable, save :: dyneutnet(:,:)	
double precision, allocatable, save :: dyneutgro(:,:)	
double precision, allocatable, save :: dinet(:,:)			
double precision, allocatable, save :: digro(:,:)			
double precision, allocatable, save :: dyneutplnet(:,:)	
double precision, allocatable, save :: dyneutplgro(:,:)	
double precision, allocatable, save :: diplnet(:,:)			
double precision, allocatable, save :: diplgro(:,:)			


!	variables bending

double precision signet(2),siggro(2)								
double precision, allocatable, save :: dsignet(:,:,:)		
double precision, allocatable, save :: dsiggro(:,:,:)		


!	variables stifbend

double precision sigstif		
double precision dsigstif(9)		

!	variables shear    ===============================================================================

double precision, allocatable, save :: taunet(:,:)		
double precision, allocatable, save :: taugro(:,:)		
double precision, allocatable, save :: dtaunet(:,:,:)	
double precision, allocatable, save :: dtaugro(:,:,:)	

!	variables buckdcn  ===============================================================================

double precision dbuckpl										
double precision dbuckstif										
double precision inpart											

      
double precision dbuckpl_07,    dbuckpl_08						 
double precision dbuckstif_07,dbuckstif_08     					     					  

double precision, allocatable, save :: ddbuckpl_07 (:,:), ddbuckpl_08  (:,:)   !dcn07
double precision, allocatable, save :: ddbuckstif_07(:,:),ddbuckstif_08(:,:)
double precision inpart_07,inpart_08							      


double precision, allocatable, save :: dinpart_07(:,:), dinpart_08(:,:)    !dcn07


!	variables materiaux (dont notamment paramètres relatifs aux coûts)
	integer*4 nbrMat
	integer, allocatable, save ::          indMateriau(:)	!jan09
	double precision, allocatable, save :: e(:)
    double precision, allocatable, save :: eta(:)
    double precision, allocatable, save :: sigy(:)		!=yield stress
	double precision, allocatable, save :: sigyadm(:)	!=allowable stress
    double precision, allocatable, save :: coefk(:)		!r&d15
    double precision, allocatable, save :: spec(:)

	double precision, allocatable, save :: dref(:)	!jan09
	double precision, allocatable, save :: drefx(:) !jan09
	double precision, allocatable, save :: drefy(:) !jan09
	double precision, allocatable, save :: dref_b(:)!jan09
	double precision, allocatable, save :: dref_c(:)!jan09
	double precision, allocatable, save :: dref_l(:)!jan09
	double precision, allocatable, save :: c1(:)	!jan09
	double precision, allocatable, save :: c2(:)	!jan09
	double precision, allocatable, save :: c3(:)	!jan09
	double precision, allocatable, save :: cout(:)	       !TODO A vérifier
	double precision, allocatable, save :: c_pb(:)	!jan09
	double precision, allocatable, save :: c_tc(:)  !jan09
	double precision, allocatable, save :: c_tl(:)  !jan09
	double precision, allocatable, save :: dc1(:)	!jan09
	double precision, allocatable, save :: dw2(:)	!jan09
	double precision, allocatable, save :: dw3(:)	!jan09
	double precision, allocatable, save :: dw_b(:)	!jan09
	double precision, allocatable, save :: dw_c(:)  !jan09
	double precision, allocatable, save :: dw_l(:)  !jan09
	double precision, allocatable, save :: labour_cost(:)
	double precision, allocatable, save :: p10(:)   !jan09
	double precision, allocatable, save :: dp10(:)  !jan09
	double precision, allocatable, save :: p4(:)    !jan09
	double precision, allocatable, save :: dp4(:)   !jan09
	double precision, allocatable, save :: p5(:)    !jan09
	double precision, allocatable, save :: dp5(:)   !jan09
	double precision, allocatable, save :: p9x(:)   !jan09
	double precision, allocatable, save :: p9y(:)   !jan09
	double precision, allocatable, save :: dp9x(:)  !jan09
	double precision, allocatable, save :: dp9y(:)  !jan09
	double precision, allocatable, save :: ber(:)   !jan09
	double precision, allocatable, save :: bet(:)   !jan09
	double precision, allocatable, save :: p6(:)    !jan09
	double precision, allocatable, save :: p7(:)    !jan09
	double precision, allocatable, save :: c8(:)    !jan09
	double precision, allocatable, save :: dc8(:)   !jan09
	integer*4, allocatable, save :: ialr(:)  !jan09
	integer*4, allocatable, save :: ialt(:)  !jan09

! ancien common opti2  ===============================================================================

double precision defa(13,4),defb(13,4),defc(13,4),defd(13,4)
double precision defaa(13,4),defbb(13,4),defcc(13,4),defdd(13,4)
double precision sensa(13,4,9),sensb(13,4,9),sensc(13,4,9),sensd(13,4,9)
double precision sensaa(13,4,9),sensbb(13,4,9),senscc(13,4,9),sensdd(13,4,9)
 
! ancien common opti3  ===============================================================================

double precision const2(6,9),const3(2,10),darg(8,9)

! ancien common py     ===============================================================================

double precision pi,sinu(4),cosi(4),expt(4)          ! bidon6 sert pour sin(4),cos(4) et exp(4)
double precision lam(8)

! ancien common hyp    ===============================================================================

double precision, allocatable, save :: hyp(:)        ! bo1,sens,bo2 (ok pour 10 traverses et 10 cas de charges

! ancien common noeud  ===============================================================================

integer*4 ntn


! ancien common ph     ===============================================================================

double precision phmax                 ! equil (only), taille 1

! ancien common graf   ===============================================================================

double precision bidon9(20)             ! save (20 = nbre de dessins différents)

! ancien common ecr    ===============================================================================

double precision uecr(51)               ! on remplace u par uecr et mt par mtecr pour ne pas confondre
integer*4 mtecr               ! bo2,ecr,ecri2,ecri3 (51= 31 + 2* mt(=10))

! ancien common str    ===============================================================================

double precision syd,syb,syb1,sysu,sysl,sud,sub,sub1,susu,susl
double precision ad,ab1,ab,as

! ancien common ushull ===============================================================================

double precision depth,db
double precision sysd,sysb1,sysb,syssup,syssdw
integer*4 nd,nb1,nb,ns
integer*4 kd,kb1,kb,ksup,ksdw

! ancien common dede   ===============================================================================
data mdp/5, 1, 1, 1,1, 1, 9 , 5, 3, 3, 1, 1, 0, 0, 0,&
         7, 5, 5, 3,3, 3, 11, 9, 5, 5, 7, 9, 0, 0, 0,&
         9, 7, 9, 5,5, 13,13,11, 7,11, 9,13, 0, 0, 0,&
         11,9, 13,7,13,15,15,13,11,13,15,15, 0, 0, 0/,&
     mar/6, 2, 2, 2,2, 2, 10, 6, 4, 4, 2, 2, 0, 0, 0,&
         8, 6, 6, 4,4, 4, 12,10, 6, 6, 8,10, 0, 0, 0,&
         10,8, 10,6,6, 14,14,12, 8,12,10,14, 0, 0, 0,&
         12,10,14,8,14,16,16,14,12,14,16,16, 0, 0, 0/

! ancien common nom    ===============================================================================

!data nom/'fichdessin\'/


!Stiffeners
double precision, allocatable, save :: sx_raid_sem(:,:,:,:)
double precision, allocatable, save :: sx_loc_raid_sem(:,:) ! Contrainte locale uniquement ou combinaison ?

double precision, allocatable, save :: kc_Stif(:,:,:,:)
double precision, allocatable, save :: NOM_STRESS_Stif(:,:,:,:)
double precision, allocatable, save :: HS_STRESS_Stif(:,:,:,:)
double precision, allocatable, save :: NS_STRESS_Stif(:,:,:,:)
double precision, allocatable, save :: Damage_Stif(:,:,:,:)
double precision, allocatable, save :: Damage_Stif_full(:,:,:)
double precision, allocatable, save :: Damage_Stif_ball(:,:,:)
double precision, allocatable, save :: Damage_Stif_total(:,:,:)



! Plate
double precision, allocatable, save :: sx_plaque_top(:,:,:,:)
double precision, allocatable, save :: sy_plaque_top(:,:,:,:)
double precision, allocatable, save :: sxy_plaque_top(:,:,:,:)
double precision, allocatable, save :: sx_plaque_bott(:,:,:,:)
double precision, allocatable, save :: sy_plaque_bott(:,:,:,:)
double precision, allocatable, save :: sxy_plaque_bott(:,:,:,:)

double precision, allocatable, save :: kc_Plate(:,:,:,:)
double precision, allocatable, save :: NOM_STRESS_Plate(:,:,:,:)
double precision, allocatable, save :: NOM_STRESS_Plate_top(:,:,:,:)
double precision, allocatable, save :: NOM_STRESS_Plate_bott(:,:,:,:)
double precision, allocatable, save :: HS_STRESS_Plate(:,:,:,:)
double precision, allocatable, save :: NS_STRESS_Plate(:,:,:,:)
double precision, allocatable, save :: Damage_Plate(:,:,:,:)
double precision, allocatable, save :: Damage_Plate_full(:,:,:)
double precision, allocatable, save :: Damage_Plate_ball(:,:,:)
double precision, allocatable, save :: Damage_Plate_total(:,:,:)


!Intersection Plate/frame--------------------------------------------------------------

double precision, allocatable, save :: kc_Plate_Frame(:,:,:,:)
double precision, allocatable, save :: NOM_STRESS_Plate_Frame(:,:,:,:)
double precision, allocatable, save :: NOM_STRESS_Plate_Frame_top(:,:,:,:)
double precision, allocatable, save :: NOM_STRESS_Plate_Frame_bott(:,:,:,:)
double precision, allocatable, save :: HS_STRESS_Plate_Frame(:,:,:,:)
double precision, allocatable, save :: NS_STRESS_Plate_Frame(:,:,:,:)
double precision, allocatable, save :: Damage_Plate_Frame(:,:,:,:)
double precision, allocatable, save :: Damage_Plate_Frame_full(:,:,:)
double precision, allocatable, save :: Damage_Plate_Frame_ball(:,:,:)
double precision, allocatable, save :: Damage_Plate_Frame_total(:,:,:)

! Frame
	!sx_plaque_top
double precision, allocatable, save :: sy_cadre_jab(:,:,:,:)
double precision, allocatable, save :: sxy_cadre_jab(:,:,:,:)
double precision, allocatable, save :: sy_cadre_sem(:,:,:,:)

double precision, allocatable, save :: kc_Frame(:,:,:,:)
double precision, allocatable, save :: NOM_STRESS_Frame(:,:,:,:)
double precision, allocatable, save :: HS_STRESS_Frame(:,:,:,:)
double precision, allocatable, save :: NS_STRESS_Frame(:,:,:,:)
double precision, allocatable, save :: Damage_Frame(:,:,:,:)
double precision, allocatable, save :: Damage_Frame_full(:,:,:)
double precision, allocatable, save :: Damage_Frame_ball(:,:,:)
double precision, allocatable, save :: Damage_Frame_total(:,:,:)



!Girders--------------------------------------------------------------


!	double precision, allocatable, save :: kc_Girder(:,:,:,:)
!	double precision, allocatable, save :: NOM_STRESS_Girder(:,:,:,:)
!	double precision, allocatable, save :: HS_STRESS_Girder(:,:,:,:)
!	double precision, allocatable, save :: NS_STRESS_Girder(:,:,:,:)
!	double precision, allocatable, save :: Damage_Girder(:,:,:,:)
!	double precision, allocatable, save :: Damage_Girder_full(:,:,:)
!	double precision, allocatable, save :: Damage_Girder_ball(:,:,:)
!	double precision, allocatable, save :: Damage_Girder_total(:,:,:)


!Pillars--------------------------------------------------------------

!	double precision, allocatable, save :: kc_Pillar(:,:,:,:)
!	double precision, allocatable, save :: NOM_STRESS_Pillar(:,:,:,:)
!	double precision, allocatable, save :: NOM_STRESS_Pillar1(:,:,:,:)
!	double precision, allocatable, save :: NOM_STRESS_Pillar2(:,:,:,:)
!	double precision, allocatable, save :: HS_STRESS_Pillar(:,:,:,:)
!	double precision, allocatable, save :: NS_STRESS_Pillar(:,:,:,:)
!	double precision, allocatable, save :: Damage_Pillar(:,:,:,:)
!	double precision, allocatable, save :: Damage_Pillar_full(:,:,:)
!	double precision, allocatable, save :: Damage_Pillar_ball(:,:,:)
!	double precision, allocatable, save :: Damage_Pillar_total(:,:,:)





! Param_multi
character*16                 key_lbr5
character*16, allocatable, save :: key_gui(:)
character*80                 multi_file
character*80, allocatable, save :: section_files(:)
character*80                 section_file
character*40                 egalt_multi_filename

integer*4 nfile
integer*4 iboat
integer*4 langue
integer*4 imultisection
integer*4 iu_conlin_global
integer*4 iu_opti
integer*4 mt_max
integer*4 nsolmax
integer*4 iwrite            !ecrire correctement le fichier opt-multistructure
integer*4 keep_reading
integer*4 number_pannel_points_fatigue

integer*4, allocatable, save :: iu_10(:)
integer*4, allocatable, save :: iu_11(:)
integer*4, allocatable, save :: iu_12(:)
integer*4, allocatable, save :: iu_13(:)
integer*4, allocatable, save :: iu_14(:)
integer*4, allocatable, save :: iu_19(:)
integer*4, allocatable, save :: iu_20(:)
integer*4, allocatable, save :: iu_21(:)
integer*4, allocatable, save :: iu_23(:)
integer*4, allocatable, save :: iu_27(:)
integer*4, allocatable, save :: iu_30(:)
integer*4, allocatable, save :: iu_31(:)
integer*4, allocatable, save :: iu_33(:)
integer*4, allocatable, save :: iu_34(:)
integer*4, allocatable, save :: iu_36(:)
integer*4, allocatable, save :: iu_37(:)
integer*4, allocatable, save :: iu_38(:)
integer*4, allocatable, save :: iu_39(:)
integer*4, allocatable, save :: iu_40(:)
integer*4, allocatable, save :: iu_42(:)
!integer*4, allocatable, save :: iu_100(:)
integer*4, allocatable, save :: iu_scratch_1(:,:)
integer*4, allocatable, save :: iu_scratch_2(:,:)
integer*4, allocatable, save :: iu_scratch_3(:,:)

integer*4, allocatable, save :: iu_conlin(:)
integer*4, allocatable, save :: iu_redress(:)
! Fin param_multi

! Param_opti_global
integer*4 iopti
integer*4 itera
integer*4 iteram
integer*4 iter1
integer*4 ico

integer*4  nmax_sup 
integer*4 ngmax_sup

integer*4 nbrMat_sup

integer*4 neto_global
integer*4 nmax_v_global
integer*4 m1tmax_global

integer*4 shift_ntot
integer*4 shift_nr
integer*4 shift_mtot
integer*4 shift_neto

integer*4 iptmax_sup
integer*4  m1max_sup
!integer*4 m1tmax_sup
integer*4  mmmax 

double precision, allocatable, save ::  qn_global   (:) 
double precision, allocatable, save ::  philn_global(:) 
character*3,      allocatable, save ::  modes_global(:)
integer*4, allocatable, save :: m1tabl_global(:,:)
integer*4, allocatable, save :: m2cont_global(:)
integer*4, allocatable, save :: m4cont_global(:)
integer*4, allocatable, save :: m5cont_global(:)


integer*4,        allocatable :: igeom_index(:,:)
integer*4,        allocatable :: igrav_index(:,:)
integer*4,        allocatable :: inert_index(:,:)
integer*4,        allocatable ::  imod_index(:,:)

integer*4,        allocatable :: iweight_index(:,:)
integer*4,        allocatable ::  iprice_index(:,:)
integer*4,        allocatable ::  istruc_index(:,:)
integer*4,        allocatable ::  irestr_index(:,:)

character*80 unit_conlin
character*25,allocatable, save :: vnom2(:,:)
character*30,allocatable, save :: vnom2_global(:)

integer*4,        allocatable, save :: shift_opt(:) !utilisé dans comp_obj

integer*4,        allocatable, save :: ig_conlin(:)
integer*4,        allocatable, save :: jt_conlin(:)
!integer*4,        allocatable, save :: ms_conlin(:)

integer*4 ms_conlin_global

integer*4 kcont_global
integer*4,        allocatable, save :: kcont(:)

double precision, allocatable, save :: xicou(:,:)
double precision, allocatable, save :: ximin(:,:)
double precision, allocatable, save :: ximax(:,:)

double precision, allocatable, save :: xicou_global(:)
double precision, allocatable, save :: ximin_global(:)
double precision, allocatable, save :: ximax_global(:)


double precision  obj_global
double precision  obj2_global

integer*4         jobj_global
integer*4         jobj2_global

integer*4 iop
integer*4 non_conlin

integer*4,        allocatable, save :: nr  (:)   
integer*4,        allocatable, save :: ntot(:)   
integer*4,        allocatable, save :: mtot(:)
integer*4,        allocatable, save :: m1tot(:)
integer*4,        allocatable, save :: m2tot(:)
integer*4,        allocatable, save :: m3tot(:)
integer*4,        allocatable, save :: m4tot(:)
integer*4,        allocatable, save :: m5tot(:)


double precision, allocatable, save :: ega(:,:)
integer*4, allocatable, save :: mega(:,:,:)
integer*4, allocatable, save :: nvs(:,:)
integer*4, allocatable, save :: negal(:,:)
integer*4, allocatable, save :: negalt(:)

integer*4, allocatable, save ::  nxi2(:,:,:)
integer*4, allocatable, save :: nxit(:,:,:)
integer*4, allocatable, save :: nvar(:,:)

integer*4, allocatable, save :: nxitr(:,:,:)
integer*4, allocatable, save :: nvarr(:,:)
integer*4, allocatable, save :: nxi2r(:,:,:)


integer*4, allocatable, save :: jobj(:)
double precision, allocatable, save :: obj (:)
double precision, allocatable, save :: obj2(:)
double precision, allocatable, save ::  objmulti(:)
double precision, allocatable, save :: obj2multi(:)

double precision, allocatable, save :: fiopt(:,:)		

double precision, allocatable, save :: xiopt(:,:)		
double precision, allocatable, save :: cjopt(:,:)		
double precision, allocatable, save :: cjmopt(:,:)	
double precision, allocatable, save :: cijopt(:,:,:)

double precision, allocatable, save :: dxopt(:,:,:)	
double precision, allocatable, save :: z1opt(:,:)		
double precision, allocatable, save :: z2opt(:,:)		
double precision, allocatable, save :: z3opt(:,:)		

double precision, allocatable, save :: sopt(:)		
double precision, allocatable, save :: fimulti(:,:)	
double precision, allocatable, save :: derredress(:)	

integer*4, allocatable, save :: ic_rest(:,:)
double precision, allocatable, save :: ratio_rest(:,:) 

integer*4 ntot_global
integer*4 m1tot_global
integer*4 m2tot_global
integer*4 m3tot_global
integer*4 m4tot_global
integer*4 mtot_global
integer*4 negalt_global
integer*4 nr_global
integer*4 negalt_multisection

double precision, allocatable, save :: fiopt_global(:)		

double precision, allocatable, save :: xiopt_global(:)		
double precision, allocatable, save :: cjopt_global(:)		
double precision, allocatable, save :: cjmopt_global(:)	
double precision, allocatable, save :: cijopt_global(:,:)

double precision, allocatable, save :: dxopt_global(:,:)	
double precision, allocatable, save :: z1opt_global(:)		
double precision, allocatable, save :: z2opt_global(:)		
double precision, allocatable, save :: z3opt_global(:)		

double precision, allocatable, save :: fimulti_global(:)	

integer*4, allocatable, save :: ic_rest_global(:)
double precision, allocatable, save :: ratio_rest_global(:) 

double precision, allocatable, save ::  ega_global(:)
integer*4, allocatable, save :: mega_global (:,:)
integer*4, allocatable, save ::   nvs_global(:)
integer*4, allocatable, save :: negal_global(:)
integer*4, allocatable, save :: nxi2_global (:,:)
integer*4, allocatable, save :: nxi2r_global (:,:)
integer*4, allocatable, save :: nxit_global(:,:)
integer*4, allocatable, save :: nvar_global (:)
integer*4, allocatable, save :: nxitr_temp_global(:,:)
integer*4, allocatable, save :: nvarr_temp_global(:)
integer*4, allocatable, save :: nxitr_global(:,:)
integer*4, allocatable, save :: nvarr_global(:)


! Fin param_opti_global

! Param_read_write
!integer*4           nel
character*80        title
character*80        texte
double precision    temp
double precision    pi1
!==============================================================================
!read_panel_stiffeners()
!==============================================================================
integer*4           iff
character*3,      allocatable :: mode(:)
double precision, allocatable, save :: xmode(:),denom(:)
!==============================================================================
!read_panel_properties()
!==============================================================================
integer*4, allocatable, save :: kse(:),ipa(:),ivaria(:) !kse = sens des charges
double precision    sh
!==============================================================================
!read_panel_loads()
!==============================================================================
double precision, allocatable, save :: xi(:,:),xf(:,:),xxi(:,:),xxf(:,:)
!==============================================================================
!comp_edge_forces_coeff()
!==============================================================================
double precision    qphil
double precision    aaa,bbb,ccc,ddd
double precision    delto
!==============================================================================
!read_comput_points()
!==============================================================================
integer*4           m1_read
!==============================================================================
!read_struct_restrictions
!==============================================================================
integer*4    is
integer*4    icc
integer*4    icn
!==============================================================================
!save_restrictions()
!==============================================================================
integer*4    npt9
double precision    cha9
character*80     , allocatable, save :: title_cha_full(:,:)
character*80     , allocatable, save :: title_loadcase(:,:)
!==============================================================================
!compute_stiffness_coeff
!==============================================================================
double precision    poids9,dpoids(9)
! Fin param_read_write

! Param restri_global
double precision :: xk
double precision :: yk 	
double precision :: xk9
double precision :: yk9 	
double precision :: xkgmin
double precision :: xkgmax
double precision :: modmin
double precision :: imin
double precision :: wmax
double precision :: pricemax
double precision :: uhogm
double precision :: usagm
double precision yred
integer*4:: igrav
integer*4:: inert
integer*4:: imod
integer*4:: imom
integer*4:: irestr
integer*4:: iult
integer*4:: iweight
integer*4:: iprice
integer*4:: ielt
integer*4:: icost
integer*4:: ncondi
integer*4,        allocatable,save :: nno (:,:)
integer*4,        allocatable,save :: nno9(:,:)
! Fin param_restri_global


end
