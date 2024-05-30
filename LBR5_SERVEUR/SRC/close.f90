subroutine close()

use param_section


! 16.1 fermeture des fichiers 
!=============================

if (iboat.eq.1) then
	close(iu_opti)
endif

close(iu_10(iboat))
close(iu_11(iboat))
close(iu_12(iboat))
close(iu_13(iboat))	   !fichier beam_theory
close(iu_14(iboat))
close(iu_21(iboat))
		
!close(iu_26(iboat))		
close(iu_27(iboat))		
		
close(iu_30(iboat))
!close(iu_100(iboat))
if(iopti.ge.1) then
	close (iu_31(iboat))
!	close(iu_32(iboat))
endif

do nel=1,neto
	close(iu_scratch_1(iboat,nel))
	if(iopti.ge.1) close(iu_scratch_2(iboat,nel))		
	close(iu_scratch_3(iboat,nel))
enddo

close(iu_33(iboat))
close(iu_34(iboat))

close(iu_36(iboat))
close(iu_37(iboat))
close(iu_38(iboat))
close(iu_39(iboat))
close(iu_40(iboat))

close(iu_42(iboat))


close(3768)
close(3769)
close(3770)

close(6800) !ouvert dans init_param_multi


close(iu_redress(iboat)) 


return
end
