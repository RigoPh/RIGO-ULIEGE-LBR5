subroutine open_scratch_files()

use param_section, ONLY : iopti,iu_scratch_1,iu_scratch_2,iu_scratch_3,iboat
use PARAM_SECTION_VECTOR, ONLY : neto_vector

implicit double precision (a-h,o-z)

! ====================================================================================================
! nom des fichiers temporaires
! ====================================================================================================

!100 + nel   -> iu_scratch_1(iboat,nel)=nel+(iboat)*10000+100
!400 + nel   -> iu_scratch_2(iboat,nel)=nel+(iboat)*10000+400
!700 + nel   -> iu_scratch_3(iboat,nel)=nel+(iboat)*10000+700


do nel=1,neto_vector(iboat)
   iu_scratch_1(iboat,nel)=nel+(iboat)*10000+100
   iu_scratch_2(iboat,nel)=nel+(iboat)*10000+400
   iu_scratch_3(iboat,nel)=nel+(iboat)*10000+700
enddo


do nel=1,neto_vector(iboat)
                  open(iu_scratch_1(iboat,nel),status='scratch',form='unformatted')
   if(iopti.ge.1) open(iu_scratch_2(iboat,nel),status='scratch',form='unformatted')
                  open(iu_scratch_3(iboat,nel),status='scratch',form='unformatted') 
enddo

!open(3768,file='scratch_fatigue_stiffened_panel.dat')
!open(3769,file='scratch_fatigue_girder.dat')
!open(3770,file='scratch_fatigue_frame.dat')
!open(3771,file=' fatigue_stiffened_panel_amirouche.dat')
!open(3772,file='dfatigue_stiffened_panel_amirouche.dat')


return                                                                                         
end

