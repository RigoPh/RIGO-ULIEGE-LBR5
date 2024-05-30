subroutine put_optim_data()

use param_section
use PARAM_SECTION_VECTOR

implicit double precision (a-h,o-z)

ypts_vector (1:iptmax,1:nmax,iboat)              =   ypts (1:iptmax,1:nmax)

tfa_vector(1:nmax,iboat) =  tfa(1:nmax)
tfr_vector(1:nmax,iboat) =  tfr(1:nmax)

indMateriau_vector(1:nmax,iboat)  = indMateriau(1:nmax)   

mode_vector(1:nmax,iboat)   = mode(1:nmax)   
modes_vector(1:nmax,iboat)  = modes(1:nmax)   
xmode_vector(1:nmax,iboat)  = xmode(1:nmax)   
abcd_vector (1:8,iboat)     = abcd (1:8)

itype_vector(1:nmax,iboat) = itype(1:nmax)
isect_vector(1:nmax,iboat) = isect(1:nmax)

sx_raid_sem_vector(1:nsol_fatigue,1:nmax,1:ix_max,1:iy_max,iboat) = sx_raid_sem(1:nsol_fatigue,1:nmax,1:ix_max,1:iy_max)
sx_loc_raid_sem_vector(1:nsol_fatigue,1:nmax,iboat) = sx_loc_raid_sem(1:nsol_fatigue,1:nmax)
sx_plaque_top_vector(1:nsol_fatigue,1:nmax,1:ix_max,1:iy_max,iboat) = sx_plaque_top(1:nsol_fatigue,1:nmax,1:ix_max,1:iy_max)
sy_plaque_top_vector(1:nsol_fatigue,1:nmax,1:ix_max,1:iy_max,iboat) = sy_plaque_top(1:nsol_fatigue,1:nmax,1:ix_max,1:iy_max)
sxy_plaque_top_vector(1:nsol_fatigue,1:nmax,1:ix_max,1:iy_max,iboat) = sxy_plaque_top(1:nsol_fatigue,1:nmax,1:ix_max,1:iy_max)
sx_plaque_bott_vector(1:nsol_fatigue,1:nmax,1:ix_max,1:iy_max,iboat) = sx_plaque_bott(1:nsol_fatigue,1:nmax,1:ix_max,1:iy_max)
sy_plaque_bott_vector(1:nsol_fatigue,1:nmax,1:ix_max,1:iy_max,iboat) = sy_plaque_bott(1:nsol_fatigue,1:nmax,1:ix_max,1:iy_max)
sxy_plaque_bott_vector(1:nsol_fatigue,1:nmax,1:ix_max,1:iy_max,iboat) = sxy_plaque_bott(1:nsol_fatigue,1:nmax,1:ix_max,1:iy_max)
sy_cadre_jab_vector(1:nsol_fatigue,1:nmax,1:ix_max,1:iy_max,iboat) = sy_cadre_jab(1:nsol_fatigue,1:nmax,1:ix_max,1:iy_max)
sxy_cadre_jab_vector(1:nsol_fatigue,1:nmax,1:ix_max,1:iy_max,iboat) = sxy_cadre_jab(1:nsol_fatigue,1:nmax,1:ix_max,1:iy_max)
sy_cadre_sem_vector(1:nsol_fatigue,1:nmax,1:ix_max,1:iy_max,iboat) = sy_cadre_sem(1:nsol_fatigue,1:nmax,1:ix_max,1:iy_max)

return
end





























































































