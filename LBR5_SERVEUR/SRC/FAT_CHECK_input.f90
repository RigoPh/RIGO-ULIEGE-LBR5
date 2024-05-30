	OPTIONS /EXTEND

!--------------------------------------------------------------------------
	subroutine CHECK_input1		!(IS_FATIGUE,Neto,nsol,nnsol,is_loadcase_fatigue,nsolm_fatigue,nsol_fatigue,nsolm)

	use param_section
!    integer IS_FATIGUE,nsol_fatigue,Neto
!	integer nsolm,nsol
!	dimension nnsol(nsol)
!	dimension is_loadcase_fatigue(nsolm)




!sigy1=sigy(indMateriau(nel_fatigue))


!CHECK 1
	if ((IS_FATIGUE.NE.0.).AND.(IS_FATIGUE.NE.1.)) then						
		write(*,*) ' "IS_FATIGUE" PARAMETER, if 1 :fatigue assessment and not if 0 '		
		write(*,*) 'stop'									
		read(*,*)
		stop											
	endif													

!CHECK 2
	if (Neto.LE.0.) then						
		write(*,*) ' "NUMBER OF PANELS = 0 '		
		write(*,*) 'stop'									
		read(*,*)
		stop											
	endif														


!CHECK 3
	if (nsolm.LE.0.) then						
		write(*,*) ' "NUMBER OF DEFINED LOAD CASES  = 0 '		
		write(*,*) 'stop'									
		read(*,*)
		stop											
	endif														

!CHECK 4
	if (nsol.LE.0.) then						
		write(*,*) ' "NUMBER OF SELECTED LOAD CASES  = 0 '		
		write(*,*) 'stop'									
		read(*,*)
		stop											
	endif													

!CHECK 5
DO I=1,nsol
	if (nnsol(i).GT.nsolm) then							
		write(*,*) ' nnsol(i) > nsolm'
		write(*,*) 'stop'									
		read(*,*)
		stop											
	endif													
ENDDO


!CHECK 6

do i=1,nsolm
	if ((is_loadcase_fatigue(i).NE.0.).AND.(is_loadcase_fatigue(i).NE.1.)) then						
		write(*,*) ' "is_loadcase_fatigue" PARAMETER, If = 1 :fatigue load case , if 0 not fatigue load case '		
		write(*,*) 'stop'									
		read(*,*)
		stop											
	endif													
Enddo

!CHECK 8
	if (nsolm_fatigue.LE.0.) then						
		write(*,*) ' "NUMBER OF DEFINED FATIGUE LOAD CASES = 0 '		
		write(*,*) 'stop'									
		read(*,*)
		stop											
	endif													
	

!CHECK 9
	if (nsol_fatigue.LE.0.) then						
		write(*,*) ' "NUMBER OF SELECTED FATIGUE LOAD CASES = 0 '		
		write(*,*) 'stop'									
		read(*,*)
		stop											
	endif	



!end !CHECK_input1
!--------------------------------------------------------------------------




!--------------------------------------------------------------------------
!subroutine CHECK_input2
!use param_section

!CHECK 10
	if (Part_full.EQ.0.) then							
		write(*,*) ' "Part_full = 0 ": Part of the ship','s life in full load condition '			
		write(*,*) 'stop'										
		read(*,*)
		stop												
	endif
													
!CHECK 11
	if ((method_fatigue.NE.1.).AND.(method_fatigue.NE.2.)) then							
		write(*,*) ' "method_fatigue":DAMAGE CALCULATION method_fatigue MUST BE "1" DNV method_fatigue OR "2" BV method_fatigue '		
		write(*,*) 'stop'										
		read(*,*)
		stop											
	endif												

!CHECK 12
	if ((Tfl.NE.20.).AND.((Tfl.LT.25.).OR.(Tfl.GT.40.))) then							
		write(*,*) ' "Tfl" : Increased design life must be between 25 and 40.if no increase of design life, Tfl=20 '		
		write(*,*) 'stop'										
		read(*,*)
		stop											
	endif												

!CHECK 13

	if (length.EQ.0.) then						
		write(*,*) 'FOR FATIGUE, "RULE LENGTH" MUST BE ENTERED '		
		write(*,*) 'stop'									
		read(*,*)
		stop											
	endif													

!CHECK 14
	if (depth.EQ.0.) then						
		write(*,*) 'depth of the vessel = 0'		
		write(*,*) 'stop'									
		read(*,*)
		stop											
	endif													

!CHECK 15

DO I=1,nsol_fatigue

	if ((NNSOL_BV_TYPE(I).LT.1.).OR.(NNSOL_BV_TYPE(I).GT.7.)) then							
		write(*,*) ' NNSOL_BV_TYPE(I) MUST HAVE THE FOLLOWING VALUES : '		
		write(*,*) ' NNSOL_BV_TYPE(I) = 1 =>load case "a", "full loading condition", OR'	
		write(*,*) ' NNSOL_BV_TYPE(I) = 2 =>load case "b", "full loading condition", OR'	
		write(*,*) ' NNSOL_BV_TYPE(I) = 3 =>load case "c", "full loading condition", OR'	
		write(*,*) ' NNSOL_BV_TYPE(I) = 4 =>load case "d", "full loading condition", OR'			
		write(*,*) ' NNSOL_BV_TYPE(I) = 5 =>load case "a", "ballast loading condition", OR'				
		write(*,*) ' NNSOL_BV_TYPE(I) = 6 =>load case "b", "ballast loading condition", OR'			
		write(*,*) ' NNSOL_BV_TYPE(I) = 7 =>load case "c", "ballast loading condition"'			
		write(*,*) 'stop'										
		read(*,*)
		stop											
	endif
	
ENDDO												

!CHECK 16

do i=1,nsolm
	if (tirant(i).EQ.0.) then						
		write(*,*) ' DRAUGHT = 0 '		
		write(*,*) 'stop'									
		read(*,*)
		stop											
	endif													
Enddo

end !CHECK_input1
!--------------------------------------------------------------------------





!--------------------------------------------------------------------------
subroutine CHECK_stif1
use param_section

	
	!CHECK  17
if (sigy(indMateriau(nel_fatigue)).EQ.0.) then							
	write(*,*) ' Yield	Stress = 0'
	write(*,*) 'stop'									
	write(*,*)
	stop											
endif											

!CHECK
if ((itype(nel_fatigue).NE.2.).and.(itype(nel_fatigue).NE.5.)) then							
	write(*,*) ' itype= 2  plaque'
	write(*,*) ' itype= 5  epontille'			
	write(*,*) 'stop'									
	write(*,*)
	stop											
endif

!CHECK
if (panneau(nel_fatigue).Kcor.EQ.0.) then							
	write(*,*) ' Yield	Stress = 0'
	write(*,*) 'stop'									
	write(*,*)
	stop											
endif													

!CHECK 
if ((panneau(nel_fatigue).I_Fat_Stif.NE.0.).and.(panneau(nel_fatigue).I_Fat_Stif.NE.1.)) then							
	write(*,*) 'panel ',nel_fatigue
	write(*,*) ' PARAMETER "I_Fat_Stif" : if 1 :fatigue assessment for stiffeners and not if 0 '			
	write(*,*) 'stop'										
	write(*,*)
	stop												
endif		

!CHECK
if ((panneau(nel_fatigue).nbr_Fat_Stif.EQ.0).OR.(panneau(nel_fatigue).nbr_Fat_Stif.GT.nbr_Fat_max))THEN
	write(*,*) 'panel ',nel_fatigue			
	write(*,*) 'Number of considered points for fatigue ASSESSEMENT ON STIFFENERS = 0 OR > ',nbr_Fat_max				
	pause 'stop'										
	stop											
endif


end !CHECK_stif1
!--------------------------------------------------------------------------




!--------------------------------------------------------------------------
subroutine CHECK_stif2(j)
use param_section

!CHECK
	if (panneau(nel_fatigue).m_stif(j).lt.3.) then
		write(*,*) 'panel ',nel_fatigue
	write(*,*) 'input value error: invalid value of stiffeners slope of s-n curve (m<3)'
		pause 'stop'
		stop
	endif

!CHECK
	if (panneau(nel_fatigue).C_stif(j).LE.0.) then
		write(*,*) 'panel ',nel_fatigue
	write(*,*) 'input value error: invalid value of stiffeners fatigue capacity (c=0 or c<0)'
		pause 'stop'
		stop
	endif

!CHECK
!	if ((panneau(nel_fatigue).ix_Stif(j).ne.1.).and.(panneau(nel_fatigue).ix_Stif(j).eq.2.).and.(panneau(nel_fatigue).ix_Stif(j).eq.3.)) then
!		write(*,*) 'panel ',nel_fatigue
!	write(*,*) 'Es_Stif (End type of the stiffener) must be equal to 1,2 or 3 (WATERTIGHT(1), BRACKET (2), SCALLOPS(3) '
!		pause 'stop'
!		stop
!	endif

!CHECK

	if ((panneau(nel_fatigue).Weib_fact_Stif(j).ne.1.).and.(panneau(nel_fatigue).Weib_fact_Stif(j).NE.2.).and.(panneau(nel_fatigue).Weib_fact_Stif(j).NE.3.).and.(panneau(nel_fatigue).Weib_fact_Stif(j).NE.4.)) then
		write(*,*) 'panel ',nel_fatigue
	write(*,*) 'weibull_Stif factor must be equal to 1,2,3 or 4'
		pause 'stop'
		stop
	endif


!CHECK	
	if ((panneau(nel_fatigue).TETA_Stif(j).NE.30).AND.(panneau(nel_fatigue).TETA_Stif(j).NE.45)) THEN			
		write(*,*) 'panel ',nel_fatigue
		write(*,*) 'input value error: panneau(nel_fatigue).TETA_Stif (30° for butt joint, 45° for T joint or cruciform)' 			
		pause 'stop'										
		stop												
	endif													


!CHECK
	if (panneau(nel_fatigue).LAMBDA_Stif(j).EQ.0) THEN
		write(*,*) 'panel ',nel_fatigue			
		write(*,*) 'input value error: LAMBDA_Stif = 0' 			
		pause 'stop'										
		stop											
	endif												


!CHECK
	if ((panneau(nel_fatigue).ix_Stif(j).EQ.0).OR.(panneau(nel_fatigue).ix_Stif(j).GT.ix_max)) THEN
		write(*,*) 'panel ',nel_fatigue			
		write(*,*) 'ix_Stif must be equal to 1,2 or 3: ix_Stif=1=>x=0, ix_Stif=2=>x=L/4, ix_Stif=3=>x=L/2' 			
		pause 'stop'										
		stop											
	endif												

!CHECK
	if ((panneau(nel_fatigue).iy_Stif(j).EQ.0).OR.(panneau(nel_fatigue).iy_Stif(j).GT.iy_max)) THEN
		write(*,*) 'panel ',nel_fatigue			
		write(*,*) 'iy_Stif must be equal to 1,2 or 3: iy_Stif=1=>Departure Node, iy_Stif=2=>Mid-Plate Node, iy_Stif=3=>Arrival Node' 			
		pause 'stop'										
		stop											
	endif												
								

!CHECK
	if (panneau(nel_fatigue).BetaIF_Stif(j).EQ.0) THEN
		write(*,*) 'panel ',nel_fatigue			
		write(*,*) 'BetaIF_Stif (Improvement Fatigue life factor for grind technique) generaly to be taken equal to 2,2' 			
		pause 'stop'										
		stop											
	endif												

!CHECK
	if (panneau(nel_fatigue).KT_Stif(j).EQ.0.) THEN
		write(*,*) 'panel ',nel_fatigue			
		write(*,*) 'input value error: Kt_hs_Stif = 0 (stiffener HOT SPOT FACTOR)' 			
		pause 'stop'										
		stop											
	endif												

end !CHECK_stif2
!--------------------------------------------------------------------------


!--------------------------------------------------------------------------
subroutine CHECK_Plate1
use param_section

!CHECK
	if ((panneau(nel_fatigue).I_Fat_Plate.NE.0).AND.(panneau(nel_fatigue).I_Fat_Plate.NE.1)) then							
		write(*,*) 'panel ',nel_fatigue
		write(*,*) ' ERROR : PARAMETER "I_Fat_Plate" if 1 :fatigue assessment for plate and not if 0 '			
		write(*,*) 'stop'										
		read(*,*)
		stop												
	endif												

!check
	if ((panneau(nel_fatigue).nbr_Fat_Plate.EQ.0).OR.(panneau(nel_fatigue).nbr_Fat_Plate.GT.nbr_Fat_max))THEN
		write(*,*) 'panel ',nel_fatigue			
		write(*,*) 'ERROR : Number of considered points for fatigue ASSESSEMENT ON Plates = 0 OR > ',nbr_Fat_max				
		pause 'stop'										
		stop											
	endif


end !CHECK_Plate1
!--------------------------------------------------------------------------




!--------------------------------------------------------------------------
subroutine CHECK_Plate2(j)
use param_section


!check 16
	if ((panneau(nel_fatigue).m_Plate(j).NE.3.).AND.(panneau(nel_fatigue).m_Plate(j).NE.5.)) then
		write(*,*) 'panel ',nel_fatigue
	write(*,*) 'input value error: invalid value of slope of  s-n curve (m must be =3 or 5)'
		pause 'stop'
		stop
	endif

!check 17
	if (panneau(nel_fatigue).C_Plate(j).LE.0.) then
		write(*,*) 'panel ',nel_fatigue
	write(*,*) 'input value error: invalid value of fatigue capacity (c=0 or c<0)'
		pause 'stop'
		stop
	endif

!check 18

	if ((panneau(nel_fatigue).Weib_fact_Plate(j).ne.1.).and.(panneau(nel_fatigue).Weib_fact_Plate(j).NE.2.).and.(panneau(nel_fatigue).Weib_fact_Plate(j).NE.3.).and.(panneau(nel_fatigue).Weib_fact_Plate(j).NE.4.)) then
		write(*,*) 'panel ',nel_fatigue
	write(*,*) 'weibull factor must be equal to 1,2,3 or 4'
		pause 'stop'
		stop
	endif

!check 19	
	if ((panneau(nel_fatigue).TETA_Plate(j).NE.30).AND.(panneau(nel_fatigue).TETA_Plate(j).NE.45)) THEN			
		write(*,*) 'panel ',nel_fatigue
		write(*,*) 'input value error: panneau(nel_fatigue).TETA_Plate (30° for butt joint, 45° for T joint or cruciform)' 			
		pause 'stop'										
		stop												
	endif													

!check 20
	if (panneau(nel_fatigue).LAMBDA_Plate(j).EQ.0) THEN
		write(*,*) 'panel ',nel_fatigue			
		write(*,*) 'input value error: LAMBDA_Plate = 0' 			
		pause 'stop'										
		stop											
	endif														

!check 21
	if ((panneau(nel_fatigue).ix_Plate(j).EQ.0).OR.(panneau(nel_fatigue).ix_Plate(j).GT.ix_max)) THEN
		write(*,*) 'panel ',nel_fatigue			
		write(*,*) 'ix_Plate must be equal to 1,2 or 3: ix_Plate=1=>x=0, ix_Plate=2=>x=L/4, ix_Plate=3=>x=L/2' 			
		pause 'stop'										
		stop											
	endif												

!check 22
	if ((panneau(nel_fatigue).iy_Plate(j).EQ.0).OR.(panneau(nel_fatigue).iy_Plate(j).GT.iy_max)) THEN
		write(*,*) 'panel ',nel_fatigue			
		write(*,*) 'iy_Plate must be equal to 1,2 or 3: iy_Plate=1=>Departure Node, iy_Plate=2=>Mid-Plate Node, iy_Plate=3=>Arrival Node' 			
		pause 'stop'										
		stop											
	endif												

!check 24
	if (panneau(nel_fatigue).BetaIF_Plate(j).EQ.0) THEN
		write(*,*) 'panel ',nel_fatigue			
		write(*,*) 'BetaIF_Plate (Improvement Fatigue life factor for grind technique) generaly to be taken equal to 2,2' 			
		pause 'stop'										
		stop											
	endif												

!check 25
	if (panneau(nel_fatigue).KT_Plate(j).EQ.0) THEN
		write(*,*) 'panel ',nel_fatigue			
		write(*,*) 'input value error: Kt_hs_Plate = 0 (Plate HOT SPOT FACTOR)' 			
		pause 'stop'										
		stop											
	endif


end !CHECK_Plate2
!--------------------------------------------------------------------------




!--------------------------------------------------------------------------
subroutine CHECK_Plate_frame(j)
use param_section


!CHECK 26
	if ((panneau(nel_fatigue).I_Fat_Plate_frame.NE.0.).AND.(panneau(nel_fatigue).I_Fat_Plate_frame.NE.1.)) then							
		write(*,*) 'panel ',nel_fatigue
		write(*,*) ' ERROR : PARAMETER "I_Fat_Plate_frame" if 1 :fatigue assessment for Plate/frame and not if 0 '			
		write(*,*) 'stop'										
		read(*,*)
		stop												
	endif												

!check 27
	if ((panneau(nel_fatigue).nbr_Fat_Plate_frame.EQ.0).OR.(panneau(nel_fatigue).nbr_Fat_Plate_frame.GT.nbr_Fat_max))THEN
		write(*,*) 'panel ',nel_fatigue			
		write(*,*) 'ERROR : Number of considered points for fatigue ASSESSEMENT ON Plate/frame = 0 OR > ',nbr_Fat_max				
		pause 'stop'										
		stop											
	endif

!check 28
	if ((panneau(nel_fatigue).m_Plate_frame(j).NE.3.).AND.(panneau(nel_fatigue).m_Plate_frame(j).NE.5.)) then
		write(*,*) 'panel ',nel_fatigue
	write(*,*) 'input value error: invalid value of slope of  s-n curve (m must be =3 or 5)'
		pause 'stop'
		stop
	endif

!check 29
	if (panneau(nel_fatigue).C_Plate_frame(j).LE.0.) then
		write(*,*) 'panel ',nel_fatigue
	write(*,*) 'input value error: invalid value of fatigue capacity (c=0 or c<0)'
		pause 'stop'
		stop
	endif

!check 30

	if ((panneau(nel_fatigue).Weib_fact_Plate_frame(j).ne.1.).and.(panneau(nel_fatigue).Weib_fact_Plate_frame(j).NE.2.).and.(panneau(nel_fatigue).Weib_fact_Plate_frame(j).NE.3.).and.(panneau(nel_fatigue).Weib_fact_Plate_frame(j).NE.4.)) then
		write(*,*) 'panel ',nel_fatigue
	write(*,*) 'weibull factor must be equal to 1,2,3 or 4'
		pause 'stop'
		stop
	endif

!check 31	
	if ((panneau(nel_fatigue).TETA_Plate_frame(j).NE.30).AND.(panneau(nel_fatigue).TETA_Plate_frame(j).NE.45)) THEN			
		write(*,*) 'panel ',nel_fatigue
		write(*,*) 'input value error: panneau(nel_fatigue).TETA_Plate_frame (30° for butt joint, 45° for T joint or cruciform)' 			
		pause 'stop'										
		stop												
	endif													

!check 32
	if (panneau(nel_fatigue).LAMBDA_Plate_frame(j).EQ.0) THEN
		write(*,*) 'panel ',nel_fatigue			
		write(*,*) 'input value error: LAMBDA_Plate_frame = 0' 			
		pause 'stop'										
		stop											
	endif														

!check 33
	if ((panneau(nel_fatigue).ix_Plate_frame(j).EQ.0).OR.(panneau(nel_fatigue).ix_Plate_frame(j).GT.ix_max)) THEN
		write(*,*) 'panel ',nel_fatigue			
		write(*,*) 'ix_Plate_frame must be equal to 1,2 or 3: ix_Plate_frame=1=>x=0, ix_Plate_frame=2=>x=L/4, ix_Plate_frame=3=>x=L/2' 			
		pause 'stop'										
		stop											
	endif												

!check 34
	if ((panneau(nel_fatigue).iy_Plate_frame(j).EQ.0).OR.(panneau(nel_fatigue).iy_Plate_frame(j).GT.iy_max)) THEN
		write(*,*) 'panel ',nel_fatigue			
		write(*,*) 'iy_Plate_frame must be equal to 1,2 or 3: iy_Plate_frame=1=>Departure Node, iy_Plate_frame=2=>Mid-Plate Node, iy_Plate_frame=3=>Arrival Node' 			
		pause 'stop'										
		stop											
	endif												

!check 36
	if (panneau(nel_fatigue).BetaIF_Plate_frame(j).EQ.0) THEN
		write(*,*) 'panel ',nel_fatigue			
		write(*,*) 'BetaIF_Plate_frame (Improvement Fatigue life factor for grind technique) generaly to be taken equal to 2,2' 			
		pause 'stop'										
		stop											
	endif												

!check 37
	if (panneau(nel_fatigue).KT_Plate_frame(j).EQ.0) THEN
		write(*,*) 'panel ',nel_fatigue			
		write(*,*) 'input value error: Kt_hs_Plate_frame = 0 (Plate/frame HOT SPOT FACTOR)' 			
		pause 'stop'										
		stop											
	endif	

end !CHECK_Plate/frame
!--------------------------------------------------------------------------





!--------------------------------------------------------------------------
subroutine CHECK_Frame(j)
use param_section


!check 40
	if (panneau(nel_fatigue).M_FRAME(j).lt.3.) then
		write(*,*) 'panel ',nel_fatigue
	write(*,*) 'input value error: invalid value of Frame slope of s-n curve (m<3)'
		pause 'stop'
		stop
	endif

!check 41
	if (panneau(nel_fatigue).C_Frame(j).LE.0.) then
		write(*,*) 'panel ',nel_fatigue
	write(*,*) 'input value error: invalid value of frame fatigue capacity (c=0 or c<0)'
		pause 'stop'
		stop
	endif

!check 42

	if ((panneau(nel_fatigue).ES_Frame(j).ne.1.).and.(panneau(nel_fatigue).ES_Frame(j).eq.2.).and.(panneau(nel_fatigue).ES_Frame(j).eq.3.)) then
		write(*,*) 'panel ',nel_fatigue
	write(*,*) 'Es_Frame (End type of the Frame) must be equal to 1,2 or 3 (WATERTIGHT(1), BRACKET (2), SCALLOPS(3) '
		pause 'stop'
		stop
	endif

!check 43

	if ((panneau(nel_fatigue).Weib_fact_Frame(j).ne.1.).and.(panneau(nel_fatigue).Weib_fact_Frame(j).NE.2.).and.(panneau(nel_fatigue).Weib_fact_Frame(j).NE.3.).and.(panneau(nel_fatigue).Weib_fact_Frame(j).NE.4.)) then
		write(*,*) 'panel ',nel_fatigue
	write(*,*) 'weibull_Frame factor must be equal to 1,2,3 or 4'
		pause 'stop'
		stop
	endif

!check 44	
	if ((panneau(nel_fatigue).TETA_Frame(j).NE.30).AND.(panneau(nel_fatigue).TETA_Frame(j).NE.45)) THEN			
		write(*,*) 'panel ',nel_fatigue
		write(*,*) 'input value error: panneau(nel_fatigue).TETA_Frame (30° for butt joint, 45° for T joint or cruciform)' 			
		pause 'stop'										
		stop												
	endif													

!check 45
	if (panneau(nel_fatigue).LAMBDA_Frame(j).EQ.0) THEN
		write(*,*) 'panel ',nel_fatigue			
		write(*,*) 'input value error: LAMBDA_Frame = 0' 			
		pause 'stop'										
		stop											
	endif												

!check 46
	if ((panneau(nel_fatigue).ix_Frame(j).EQ.0).OR.(panneau(nel_fatigue).ix_Frame(j).GT.ix_max)) THEN
		write(*,*) 'panel ',nel_fatigue			
		write(*,*) 'ix_Frame must be equal to 1,2 or 3: ix_Frame=1=>x=0, ix_Frame=2=>x=L/4, ix_Frame=3=>x=L/2' 			
		pause 'stop'										
		stop											
	endif												

!check 47
	if ((panneau(nel_fatigue).iy_Frame(j).EQ.0).OR.(panneau(nel_fatigue).iy_Frame(j).GT.iy_max)) THEN
		write(*,*) 'panel ',nel_fatigue			
		write(*,*) 'iy_Frame must be equal to 1,2 or 3: iy_Frame=1=>Departure Node, iy_Frame=2=>Mid-Plate Node, iy_Frame=3=>Arrival Node' 			
		pause 'stop'										
		stop											
	endif												

!check 49
	if (panneau(nel_fatigue).BetaIF_Frame(j).EQ.0) THEN
		write(*,*) 'panel ',nel_fatigue			
		write(*,*) 'BetaIF_Frame (Improvement Fatigue life factor for grind technique) generaly to be taken equal to 2,2' 			
		pause 'stop'										
		stop											
	endif												

!check 50
	if (panneau(nel_fatigue).KT_Frame(j).EQ.0) THEN
		write(*,*) 'panel ',nel_fatigue			
		write(*,*) 'input value error: Kt_hs_Frame = 0 (Frame HOT SPOT FACTOR)' 			
		pause 'stop'										
		stop											
	endif


end !CHECK_Frame
!--------------------------------------------------------------------------



!--------------------------------------------------------------------------


!subroutine CHECK_Girder(j,k)
!use param_section

!check 40
!	if (panneau(nel_fatigue).m_Girder(nel_fatigue,j,k).lt.3.) then
!		write(*,*) 'panel ',nel_fatigue
!		write(*,*) 'input value error: invalid value of Girders slope of s-n curve (m<3)'
!		pause 'stop'
!		stop
!	endif

!check 41
!	if (panneau(nel_fatigue).C_Girder(nel_fatigue,j,k).LE.0.) then
!		write(*,*) 'panel ',nel_fatigue
!		write(*,*) 'input value error: invalid value of Girders fatigue capacity (c=0 or c<0)'
!		pause 'stop'
!		stop
!	endif

!check 42

!	if ((panneau(nel_fatigue).ES_Girder(nel_fatigue,j,k).ne.1.).and.(panneau(nel_fatigue).ES_Girder(nel_fatigue,j,k).eq.2.).and.(panneau(nel_fatigue).ES_Girder(nel_fatigue,j,k).eq.3.)) then
!		write(*,*) 'panel ',nel_fatigue
!		write(*,*) 'Es_Frame (End type of the Girder) must be equal to 1,2 or 3 (WATERTIGHT(1), BRACKET (2), SCALLOPS(3) '
!		pause 'stop'
!		stop
!	endif

!check 43

!	if ((panneau(nel_fatigue).Weib_fact_Girder(nel_fatigue,j,k).ne.1.).and.(panneau(nel_fatigue).Weib_fact_Girder(nel_fatigue,j,k).NE.2.).and.(panneau(nel_fatigue).Weib_fact_Girder(nel_fatigue,j,k).NE.3.).and.(panneau(nel_fatigue).Weib_fact_Girder(nel_fatigue,j,k).NE.4.)) then
!		write(*,*) 'panel ',nel_fatigue
!		write(*,*) 'weibull_Girder factor must be equal to 1,2,3 or 4'
!		pause 'stop'
!		stop
!	endif


!check 44	
!	if ((panneau(nel_fatigue).TETA_Girder(nel_fatigue,j,k).NE.30).AND.(panneau(nel_fatigue).TETA_Girder(nel_fatigue,j,k).NE.45)) THEN			
!		write(*,*) 'panel ',nel_fatigue
!		write(*,*) 'input value error: panneau(nel_fatigue).TETA_Girder (30° for butt joint, 45° for T joint or cruciform)' 			
!		pause 'stop'										
!		stop												
!	endif													


!check 45
!	if (panneau(nel_fatigue).LAMBDA_Girder(nel_fatigue,j,k).EQ.0) THEN
!		write(*,*) 'panel ',nel_fatigue			
!		write(*,*) 'input value error: LAMBDA_Girder = 0' 			
!		pause 'stop'										
!		stop											
!	endif												


!check 46
!	if ((panneau(nel_fatigue).ix_Girder(nel_fatigue,j,k).EQ.0).OR.(panneau(nel_fatigue).ix_Girder(nel_fatigue,j,k).GT.ix_max)) THEN
!		write(*,*) 'panel ',nel_fatigue			
!		write(*,*) 'ix_Girder must be equal to 1,2 or 3: ix_Girder=1=>x=0, ix_Girder=2=>x=L/4, ix_Girder=3=>x=L/2' 			
!		pause 'stop'										
!		stop											
!	endif												


!check 49
!	if (panneau(nel_fatigue).BetaIF_Girder(nel_fatigue,j,k).EQ.0) THEN
!		write(*,*) 'panel ',nel_fatigue			
!		write(*,*) 'BetaIF_Girder (Improvement Fatigue life factor for grind technique) generaly to be taken equal to 2,2' 			
!		pause 'stop'										
!		stop											
!	endif												

!check 50
!	if (panneau(nel_fatigue).KT_Girder(nel_fatigue,j,k).EQ.0) THEN
!		write(*,*) 'panel ',nel_fatigue			
!		write(*,*) 'input value error: Kt_hs_Girder = 0 (Girder HOT SPOT FACTOR)' 			
!		pause 'stop'										
!		stop											
!	endif


!end !CHECK_Girder
!--------------------------------------------------------------------------


!--------------------------------------------------------------------------

!subroutine CHECK_Pillar(j)
!use param_section

!check 40
!	if (panneau(nel_fatigue).m_Pillar(j).lt.3.) then
!		write(*,*) 'panel ',nel_fatigue
!	write(*,*) 'input value error: invalid value of Pillar slope of s-n curve (m<3)'
!		pause 'stop'
!		stop
!	endif
!
!!check 41
!	if (panneau(nel_fatigue).C_Pillar(j).LE.0.) then
!		write(*,*) 'panel ',nel_fatigue
!	write(*,*) 'input value error: invalid value of frame fatigue capacity (c=0 or c<0)'
!		pause 'stop'
!		stop
!	endif
!
!!check 42
!
!	if ((panneau(nel_fatigue).ES_Pillar(j).ne.1.).and.(panneau(nel_fatigue).ES_Pillar(j).eq.2.).and.(panneau(nel_fatigue).ES_Pillar(j).eq.3.)) then
!		write(*,*) 'panel ',nel_fatigue
!	write(*,*) 'Es_Pillar (End type of the Pillar) must be equal to 1,2 or 3 (WATERTIGHT(1), BRACKET (2), SCALLOPS(3) '
!		pause 'stop'
!		stop
!	endif
!
!!check 43
!
!	if ((panneau(nel_fatigue).Weib_fact_Pillar(j).ne.1.).and.(panneau(nel_fatigue).Weib_fact_Pillar(j).NE.2.).and.(panneau(nel_fatigue).Weib_fact_Pillar(j).NE.3.).and.(panneau(nel_fatigue).Weib_fact_Pillar(j).NE.4.)) then
!		write(*,*) 'panel ',nel_fatigue
!	write(*,*) 'weibull_Pillar factor must be equal to 1,2,3 or 4'
!		pause 'stop'
!		stop
!	endif
!
!
!!check 44	
!	if ((panneau(nel_fatigue).TETA_Pillar(j).NE.30).AND.(panneau(nel_fatigue).TETA_Pillar(j).NE.45)) THEN			
!		write(*,*) 'panel ',nel_fatigue
!		write(*,*) 'input value error: panneau(nel_fatigue).TETA_Pillar (30° for butt joint, 45° for T joint or cruciform)' 			
!		pause 'stop'										
!		stop												
!	endif													
!
!
!!check 45
!	if (panneau(nel_fatigue).LAMBDA_Pillar(j).EQ.0) THEN
!		write(*,*) 'panel ',nel_fatigue			
!		write(*,*) 'input value error: LAMBDA_Pillar = 0' 			
!		pause 'stop'										
!		stop											
!	endif												
!
!
!!check 46
!	if ((panneau(nel_fatigue).ix_Pillar(j).EQ.0).OR.(panneau(nel_fatigue).ix_Pillar(j).GT.ix_max)) THEN
!		write(*,*) 'panel ',nel_fatigue			
!		write(*,*) 'ix_Pillar must be equal to 1,2 or 3: ix_Pillar=1=>x=0, ix_Pillar=2=>x=L/4, ix_Pillar=3=>x=L/2' 			
!		pause 'stop'										
!		stop											
!	endif												
!
!!check 47
!	if ((panneau(nel_fatigue).iy_Pillar(j).EQ.0).OR.(panneau(nel_fatigue).iy_Pillar(j).EQ.2).OR.(panneau(nel_fatigue).iy_Pillar(j).GT.iy_max)) THEN
!		write(*,*) 'panel ',nel_fatigue			
!		write(*,*) 'iy_Pillar must be equal to 1 or 3: iy_Pillar=1=>Departure Node, iy_Pillar=3=>Arrival Node' 			
!		pause 'stop'										
!		stop											
!	endif												
!
!!check 49
!	if (panneau(nel_fatigue).BetaIF_Pillar(j).EQ.0) THEN
!		write(*,*) 'panel ',nel_fatigue			
!		write(*,*) 'BetaIF_Pillar (Improvement Fatigue life factor for grind technique) generaly to be taken equal to 2,2' 			
!		pause 'stop'										
!		stop											
!	endif												
!
!!check 50
!	if (panneau(nel_fatigue).KT_Pillar(j).EQ.0) THEN
!		write(*,*) 'panel ',nel_fatigue			
!		write(*,*) 'input value error: Kt_hs_Pillar = 0 (Frame HOT SPOT FACTOR)' 			
!		pause 'stop'										
!		stop											
!	endif
!
!
!end !CHECK_Pillar(j)
!!--------------------------------------------------------------------------


