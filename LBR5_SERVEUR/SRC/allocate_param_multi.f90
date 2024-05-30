subroutine allocate_param_multi()		

!
use param_section !, ONLY : idebug,idebug_conlin

implicit double precision (a-h,o-z)

! allocation des tableaux et matrices ================================================================

allocate(section_files(nfile))
allocate(key_gui(nfile))

! nombre maximum de panneaux fixés à 200
allocate(iu_scratch_1(nfile,300))
allocate(iu_scratch_2(nfile,300))
allocate(iu_scratch_3(nfile,300))

allocate(iu_10(nfile))
allocate(iu_11(nfile))
allocate(iu_12(nfile))
allocate(iu_13(nfile))
allocate(iu_14(nfile))
!allocate(iu_15(nfile))
!allocate(iu_16(nfile))
!allocate(iu_17(nfile))
!allocate(iu_18(nfile)) 
allocate(iu_19(nfile)) 
allocate(iu_20(nfile)) 
allocate(iu_21(nfile)) 
!allocate(iu_22(nfile)) 
allocate(iu_23(nfile)) 
!allocate(iu_24(nfile)) 
!allocate(iu_25(nfile)) 
!allocate(iu_26(nfile))    
allocate(iu_27(nfile))    
!allocate(iu_28(nfile))    
!allocate(iu_29(nfile))    
allocate(iu_30(nfile))    
allocate(iu_31(nfile))    
!allocate(iu_32(nfile))    
allocate(iu_33(nfile))    
allocate(iu_34(nfile))    
!allocate(iu_35(nfile))    
allocate(iu_36(nfile))    
allocate(iu_37(nfile))    
allocate(iu_38(nfile))    
allocate(iu_39(nfile))    
allocate(iu_40(nfile)) 
!allocate(iu_41(nfile)) 
allocate(iu_42(nfile)) 
!allocate(iu_43(nfile)) 
!allocate(iu_44(nfile))    
!allocate(iu_45(nfile))    
!allocate(iu_46(nfile))    
!allocate(iu_47(nfile))    
!allocate(iu_48(nfile))    
!allocate(iu_49(nfile))    
!allocate(iu_50(nfile)) 
!allocate(iu_51(nfile)) 
!allocate(iu_100(nfile))

if (idebug_conlin.eq.1) then
   allocate(iu_conlin(nfile))
endif

allocate(iu_redress(nfile))

do iboat=1,nfile
   iu_10(iboat)=10+iboat*100 
   iu_11(iboat)=11+iboat*100 
   iu_12(iboat)=12+iboat*100 
   iu_13(iboat)=13+iboat*100 
   iu_14(iboat)=14+iboat*100 
                   
!   iu_15(iboat)=15+iboat*100 
!   iu_16(iboat)=16+iboat*100 
!   iu_17(iboat)=17+iboat*100 
!   iu_18(iboat)=18+iboat*100 
   iu_19(iboat)=19+iboat*100 
   iu_20(iboat)=20+iboat*100 
   iu_21(iboat)=21+iboat*100 
!   iu_22(iboat)=22+iboat*100 
                   
!   iu_23(iboat)=23+iboat*100 
!   iu_24(iboat)=24+iboat*100 
!   iu_25(iboat)=25+iboat*100 
!   iu_26(iboat)=26+iboat*100 
   iu_27(iboat)=27+iboat*100 
!   iu_28(iboat)=28+iboat*100 
                   
!   iu_29(iboat)=29+iboat*100 
   iu_30(iboat)=30+iboat*100 
   iu_31(iboat)=31+iboat*100 
!   iu_32(iboat)=32+iboat*100 
   iu_33(iboat)=33+iboat*100 

   iu_34(iboat)=34+iboat*100 
!   iu_35(iboat)=35+iboat*100 

   iu_36(iboat)=36+iboat*100
   iu_37(iboat)=37+iboat*100
   iu_38(iboat)=38+iboat*100
   iu_39(iboat)=39+iboat*100
   iu_40(iboat)=40+iboat*100
!   iu_41(iboat)=41+iboat*100
   iu_42(iboat)=42+iboat*100
   !iu_100(iboat)=10+iboat*100 
   iu_redress(iboat)=52+iboat*100
enddo

iu_scratch_1(:,:)=0
iu_scratch_2(:,:)=0
iu_scratch_3(:,:)=0

iu_opti = 53 !!! ! Un seul fichier opti ! Pas un par structure !!!		                  

return 
end
