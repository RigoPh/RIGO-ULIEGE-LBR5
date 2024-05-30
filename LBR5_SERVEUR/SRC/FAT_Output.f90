	OPTIONS /EXTEND

    SUBROUTINE FAT_Output	
	use param_section
	character*40 FAT_RESULT,inputfile


	sigy1=sigy(indMateriau(nel_fatigue))
	
	inputfile=section_files(iboat)

	FAT_RESULT='FAT-'//inputfile

!	FAT_RESULT='FAT_output'
	open(66,file=FAT_RESULT)

	WRITE(66,*) '**********************************************************************'
	WRITE(66,108) 'RECAPITULATIF DES DONNEES'
	WRITE(66,*) '**********************************************************************'





	WRITE(66,119) IS_FATIGUE,'	IS_FATIGUE'

	WRITE(66,119) Neto,'	Nombre de panneaux'

	WRITE(66,120) nsolm, nsol,'	nsolm,nsol'

	WRITE(66,*) 'liste des numeros des nsol cas étudiés'

	WRITE(66,*) (nnsol(i),i=1,nsol)

	do i=1,nsolm
	WRITE(66,*),'cas de charge',i
	WRITE(66,122) is_loadcase_fatigue(i),'	Flag Cas de charge Fatigue (1:OUI;0:NON)'	
	Enddo   


	do i=1,nsolm
	WRITE(66,*) tirant(i),'Draught for load case',i
	Enddo   

	WRITE(66,121) Part_full,method_fatigue,Tfl,depth,length,'Part_full,method_fatigue,Tfl,depth,length'

	WRITE(66,*) 'cas fatigue sélectionnés'

	WRITE(66,*) (Fat_NNSOL(I),I=1,nsol_fatigue)

!	WRITE(66,*) (NNSOL_BV_TYPE(I),I=1,nsol_fatigue)



	DO nel_fatigue=1,neto

	WRITE(66,*) '================================================================='

	WRITE(66,123) 'PLAQUE    PANNEAU NO', nel_fatigue          


WRITE(66,100) SIGY1, itype(nel_fatigue), panneau(nel_fatigue).Kcor, z(nel_fatigue,3),z(nel_fatigue,4),&
'Y_Stress(N/m2), itype,Kcor, y noeud depart, y noeud arrivee'																					

!WRITE(66,101) epsa(nel_fatigue),entr(nel_fatigue),epsa2(nel_fatigue),entr2(nel_fatigue),'Entr. aig. et raid, idem raid. compl. (m)'		
!WRITE(66,102) hya(nel_fatigue),dya(nel_fatigue),wya(nel_fatigue),tya(nel_fatigue),'Cadres Transversaux (m). Hauteur âme. Epaisseur âme. &
!Largeur semelle. Epaisseur semelle'			
!WRITE(66,103) hxr(nel_fatigue),dxr(nel_fatigue),wxr(nel_fatigue),txr(nel_fatigue),'Raidisseurs  Long.  (m). Hauteur âme. Epaisseur âme. &
!Largeur semelle. Epaisseur semelle'

If(itype(nel_fatigue).ne.5) then		!Panel


WRITE(66,*)'stiffeners-------------------------------------------------------' 
WRITE(66,119) panneau(nel_fatigue).I_Fat_Stif,'	I_Fat_Stif'

if (panneau(nel_fatigue).I_Fat_Stif .NE. 0) THEN

WRITE(66,119) panneau(nel_fatigue).nbr_Fat_stif,'	number of assessment points'

DO j=1,panneau(nel_fatigue).nbr_Fat_stif

WRITE(66,*)

WRITE(66,124) 'ix','iy','Es','Kt','LAMBDA','TETA','BetaIF','m','C    ','Weib_fact'
WRITE(66,1240) panneau(nel_fatigue).ix_Stif(j),panneau(nel_fatigue).iy_Stif(j),panneau(nel_fatigue).Es_Stif(j),&
panneau(nel_fatigue).KT_Stif(j),panneau(nel_fatigue).LAMBDA_Stif(j), panneau(nel_fatigue).TETA_Stif(j),&
panneau(nel_fatigue).BetaIF_Stif(j),panneau(nel_fatigue).m_Stif(j),panneau(nel_fatigue).C_stif(j),&
 panneau(nel_fatigue).Weib_fact_Stif(j)
WRITE(66,*)

!stres



	WRITE(66,132) 'sx raid (sem)','sx_stiff_LOCAL'
	DO caschge_fatigue=1,nsol_fatigue

	WRITE(66,126) sx_raid_sem(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Stif(j),&
	panneau(nel_fatigue).iy_Stif(j))/1000000,sx_Loc_raid_sem(caschge_fatigue,nel_fatigue)/1000000
	ENDDO

ENDDO

ENDIF


!
WRITE(66,*)'Plates--------------------------------------------------------------' 
WRITE(66,119) panneau(nel_fatigue).I_Fat_Plate,'	I_Fat_Plate'		

if (panneau(nel_fatigue).I_Fat_Plate .NE. 0) THEN

WRITE(66,119) panneau(nel_fatigue).nbr_Fat_Plate,'	number of assessment points'

DO j=1,panneau(nel_fatigue).nbr_Fat_Plate
WRITE(66,*)
WRITE(66,1250) 'ix','iy','Kt','LAMBDA','TETA','BetaIF','m','C    ','Weib_fact'

WRITE(66,125) panneau(nel_fatigue).ix_Plate(j),panneau(nel_fatigue).iy_Plate(j),panneau(nel_fatigue).KT_Plate(j),&
panneau(nel_fatigue).LAMBDA_Plate(j), panneau(nel_fatigue).TETA_Plate(j), panneau(nel_fatigue).BetaIF_Plate(j),&
panneau(nel_fatigue).m_Plate(j),panneau(nel_fatigue).C_Plate(j),panneau(nel_fatigue).Weib_fact_Plate(j)
WRITE(66,*)

!WRITE(66,125) panneau(nel_fatigue).ix_Plate(j),panneau(nel_fatigue).iy_Plate(j),panneau(nel_fatigue).KT_Plate(j),&
!panneau(nel_fatigue).LAMBDA_Plate(j), panneau(nel_fatigue).TETA_Plate(j), panneau(nel_fatigue).BetaIF_Plate(j),&
!panneau(nel_fatigue).m_Plate(j),panneau(nel_fatigue).C_Plate(j),panneau(nel_fatigue).Weib_fact_Plate(j),&
!'	ix,iy,Es,Kt,LAMBDA,TETA,BetaIF,m,C,Weib_fact'	

!stress

	WRITE(66,133)  'sx bordé (z=+d/2)','sx bordé (z=-d/2)','sy bordé (z=+d/2)','sy bordé (z=-d/2)',	&
					'txy bordé (z=+d/2)','txy bordé (z=-d/2)'
	DO caschge_fatigue=1,nsol_fatigue

	WRITE(66,127)  sx_plaque_top(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate(j),&
	panneau(nel_fatigue).iy_Plate(j))/1000000,sx_plaque_bott(caschge_fatigue,nel_fatigue,&
	panneau(nel_fatigue).ix_Plate(j),panneau(nel_fatigue).iy_Plate(j))/1000000,sy_plaque_top(caschge_fatigue,&
	nel_fatigue,panneau(nel_fatigue).ix_Plate(j),panneau(nel_fatigue).iy_Plate(j))/1000000,sy_plaque_bott(caschge_fatigue,&
	nel_fatigue,panneau(nel_fatigue).ix_Plate(j),panneau(nel_fatigue).iy_Plate(j))/1000000,sxy_plaque_top(caschge_fatigue,&
	nel_fatigue,panneau(nel_fatigue).ix_Plate(j),panneau(nel_fatigue).iy_Plate(j))/1000000,sxy_plaque_bott(caschge_fatigue,&
	nel_fatigue,panneau(nel_fatigue).ix_Plate(j),panneau(nel_fatigue).iy_Plate(j))/1000000

	ENDDO

ENDDO

ENDIF


!
WRITE(66,*)'Intersection Plate/frame----------------------------------------------' 
WRITE(66,119) panneau(nel_fatigue).I_Fat_Plate_Frame,'	I_Fat_Plate_frame'					

if (panneau(nel_fatigue).I_Fat_Plate_Frame .NE. 0) THEN

WRITE(66,119) panneau(nel_fatigue).nbr_Fat_Plate_frame,'	number of assessment points'

DO j=1,panneau(nel_fatigue).nbr_Fat_Plate_frame
WRITE(66,*)
WRITE(66,1250) 'ix','iy','Kt','LAMBDA','TETA','BetaIF','m','C    ','Weib_fact'

WRITE(66,125) panneau(nel_fatigue).ix_Plate_frame(j),panneau(nel_fatigue).iy_Plate_frame(j),panneau(nel_fatigue).KT_Plate_frame(j), &
panneau(nel_fatigue).LAMBDA_Plate_frame(j),	panneau(nel_fatigue).TETA_Plate_frame(j),panneau(nel_fatigue).BetaIF_Plate_frame(j), &
panneau(nel_fatigue).m_Plate_frame(j),panneau(nel_fatigue).C_Plate_frame(j),panneau(nel_fatigue).Weib_fact_Plate_frame(j)
WRITE(66,*)
!stress

WRITE(66,1280)  'sx bordé (z=+d/2)','sx bordé (z=-d/2)','sy cadre (jab)','sxy cadre (jab)'



DO caschge_fatigue=1,nsol_fatigue

WRITE(66,128) sx_plaque_top(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate_Frame(j),&
panneau(nel_fatigue).iy_Plate_Frame(j))/1000000,sx_plaque_bott(caschge_fatigue,nel_fatigue,&
panneau(nel_fatigue).ix_Plate_Frame(j),panneau(nel_fatigue).iy_Plate_Frame(j))/1000000,&
sy_cadre_jab(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate_Frame(j),&
panneau(nel_fatigue).iy_Plate_Frame(j))/1000000,sxy_cadre_jab(caschge_fatigue,&
nel_fatigue,panneau(nel_fatigue).ix_Plate_Frame(j),panneau(nel_fatigue).iy_Plate_Frame(j))/1000000
ENDDO
			
ENDDO

ENDIF


!
WRITE(66,*)'Frames--------------------------------------------------------------' 
WRITE(66,*) panneau(nel_fatigue).I_Fat_Frame,'	I_Fat_Frame'		


if (panneau(nel_fatigue).I_Fat_Frame .NE. 0) THEN

WRITE(66,119) panneau(nel_fatigue).nbr_Fat_Frame,'	number of assessment points'

DO j=1,panneau(nel_fatigue).nbr_Fat_Frame
WRITE(66,*)
WRITE(66,124) '	ix', 'iy', 'Es', 'Kt', 'LAMBDA', 'TETA', 'BetaIF', 'm', 'C', 'Weib_fact'
WRITE(66,1240) panneau(nel_fatigue).ix_Frame(j),panneau(nel_fatigue).iy_Frame(j),panneau(nel_fatigue).Es_Frame(j),&
panneau(nel_fatigue).KT_Frame(j),panneau(nel_fatigue).LAMBDA_Frame(j),panneau(nel_fatigue).TETA_Frame(j),&
panneau(nel_fatigue).BetaIF_Frame(j),panneau(nel_fatigue).m_Frame(j),panneau(nel_fatigue).C_Frame(j),&
panneau(nel_fatigue).Weib_fact_frame(j)
WRITE(66,*)

!stress
	WRITE(66,1290) 'sy cadre (sem)'
	DO caschge_fatigue=1,nsol_fatigue
	WRITE(66,129) sy_cadre_sem(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Frame(j),&
	panneau(nel_fatigue).iy_Frame(j))/1000000

	ENDDO

ENDDO

ENDIF



!
!WRITE(66,*)'Girders--------------------------------------------------------------' 
!WRITE(66,*) MT(nel_fatigue)

!If (MT(nel_fatigue).NE.0) Then

!	Do K=1,MT(nel_fatigue)

!		WRITE(66,*) I_Fat_Girder(nel_fatigue,k),'	I_Fat_Girder'						
!		
!		WRITE(66,*) 	TXTR(nel_fatigue,k)			!TXTR : épaisseur semelle girder (traverses)
!		WRITE(66,*) 	ABTR(nel_fatigue,k)			!ABTR : position of the girder with reference to the departure node (in meter

!		if (I_Fat_Girder(nel_fatigue,k) .NE. 0) THEN

!		WRITE(66,119) nbr_Fat_Girder(nel_fatigue,k),'	number of assessment points'
!
!		DO j=1,nbr_Fat_Girder(nel_fatigue,k)
!		WRITE(66,*)
!		WRITE(66,1250) 'ix','iy','Es','Kt','LAMBDA','TETA','BetaIF','m','C    ','Weib_fact'
!

!		WRITE(66,125) panneau(nel_fatigue).ix_Girder(nel_fatigue,j,k),panneau(nel_fatigue).Es_Girder(nel_fatigue,j,k),panneau(nel_fatigue).KT_Girder(nel_fatigue,j,k),&
!		panneau(nel_fatigue).LAMBDA_Girder(nel_fatigue,j,k)	,panneau(nel_fatigue).TETA_Girder(nel_fatigue,j,k),panneau(nel_fatigue).BetaIF_Girder(nel_fatigue,j,k),&
!		panneau(nel_fatigue).m_Girder(nel_fatigue,j,k),panneau(nel_fatigue).C_Girder(nel_fatigue,j,k),panneau(nel_fatigue).Weib_fact_Girder(nel_fatigue,j,k)
!
!
!			DO caschge_fatigue=1,nsol_fatigue
!			WRITE(66,129) S_Girder_sem(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Girder(nel_fatigue,j,k),k)
!			ENDDO
!
!		ENDDO
!	
!		ENDIF
!
!	ENDDO
!ENDIF


!

elseif(itype(nel_fatigue).eq.5) then		!Pillar

!WRITE(66,*)'Pillars--------------------------------------------------------------'
!WRITE(66,*) I_Fat_Pillar(nel_fatigue),'	I_Fat_Pillar'
!
!if (I_Fat_Pillar(nel_fatigue).NE.0) THEN
					
!WRITE(66,119) nbr_Fat_Pillar(nel_fatigue),'	number of assessment points'

!DO j=1,nbr_Fat_Pillar(nel_fatigue)
!WRITE(66,*)
!WRITE(66,124) panneau(nel_fatigue).ix_Pillar(j),panneau(nel_fatigue).iy_Pillar(j),panneau(nel_fatigue).Es_Pillar(j),panneau(nel_fatigue).KT_Pillar(j),&
!panneau(nel_fatigue).LAMBDA_Pillar(j),panneau(nel_fatigue).TETA_Pillar(j), panneau(nel_fatigue).BetaIF_Pillar(j),panneau(nel_fatigue).m_Pillar(j),&
!panneau(nel_fatigue).C_Pillar(j),panneau(nel_fatigue).Weib_fact_Pillar(j), '	ix,iy,Es,Kt,LAMBDA,TETA,BetaIF,m,C,Weib_fact'	

!stress

!	DO caschge_fatigue=1,nsol_fatigue!
!	WRITE(66,129)S_sup_Pillar(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Pillar(j),panneau(nel_fatigue).iy_Pillar(j)),	& 
!	S_inf_Pillar(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Pillar(j),panneau(nel_fatigue).iy_Pillar(j))
!	ENDDO


!ENDDO


!Endif
Endif

ENDDO





	WRITE(66,*) '**********************************************************************'
    WRITE(66,108) 'RESULTS'
	WRITE(66,*) '**********************************************************************'


	DO nel_fatigue=1,neto

	If(itype(nel_fatigue).ne.5) then		!Panel

	WRITE(66,*) '------------------------------------------'
    WRITE(66,117) 'PANEL',nel_fatigue				
 	WRITE(66,*) '------------------------------------------'

	!stiffeners--------------------------------------------------------------

	if (panneau(nel_fatigue).I_Fat_Stif .eq. 0) THEN

	! WRITE(*,*) 'STIFFENERS NOT CONSIDERED '

	WRITE(66,*) 'STIFFENERS NOT CONSIDERED '

	ELSE

	DO Fat_pts=1,panneau(nel_fatigue).nbr_Fat_stif

	! WRITE(*,*) '------------------'

!	WRITE(*,118) 'STIFFENERS :','IX =',panneau(nel_fatigue).ix_Stif(Fat_pts),'IY =',panneau(nel_fatigue).iy_Stif(Fat_pts)				
	
	if ((panneau(nel_fatigue).ix_Stif(Fat_pts).eq.1).and.(panneau(nel_fatigue).iy_Stif(Fat_pts).eq.1)) then
	
	! WRITE(*,135) 'STIFFENERS : x = 0 , Departure Node'
					
	elseif ((panneau(nel_fatigue).ix_Stif(Fat_pts).eq.1).and.(panneau(nel_fatigue).iy_Stif(Fat_pts).eq.2)) then

	! WRITE(*,135) 'STIFFENERS :x = 0, Mid-Plate Node'

	elseif ((panneau(nel_fatigue).ix_Stif(Fat_pts).eq.1).and.(panneau(nel_fatigue).iy_Stif(Fat_pts).eq.3)) then

	! WRITE(*,135) 'STIFFENERS :x = 0, Arrival Node'

	elseif ((panneau(nel_fatigue).ix_Stif(Fat_pts).eq.2).and.(panneau(nel_fatigue).iy_Stif(Fat_pts).eq.1)) then

	! WRITE(*,135) 'STIFFENERS :x = L/4, Departure Node'

	elseif ((panneau(nel_fatigue).ix_Stif(Fat_pts).eq.2).and.(panneau(nel_fatigue).iy_Stif(Fat_pts).eq.2)) then

	! WRITE(*,135) 'STIFFENERS :x = L/4, Mid-Plate Node'

	elseif ((panneau(nel_fatigue).ix_Stif(Fat_pts).eq.2).and.(panneau(nel_fatigue).iy_Stif(Fat_pts).eq.3)) then

	! WRITE(*,135) 'STIFFENERS :x = L/4, Arrival Node'

	elseif ((panneau(nel_fatigue).ix_Stif(Fat_pts).eq.3).and.(panneau(nel_fatigue).iy_Stif(Fat_pts).eq.1)) then

	! WRITE(*,135) 'STIFFENERS :x = L/2, Departure Node'

	elseif ((panneau(nel_fatigue).ix_Stif(Fat_pts).eq.3).and.(panneau(nel_fatigue).iy_Stif(Fat_pts).eq.2)) then

	! WRITE(*,135) 'STIFFENERS :x = L/2, Mid-Plate Node'

	elseif ((panneau(nel_fatigue).ix_Stif(Fat_pts).eq.3).and.(panneau(nel_fatigue).iy_Stif(Fat_pts).eq.3)) then

	! WRITE(*,135) 'STIFFENERS :x = L/2, Arrival Node'

	ENDIF


	! WRITE(*,104) 'Damage full = ', Damage_Stif_full(nel_fatigue,panneau(nel_fatigue).ix_Stif(Fat_pts),panneau(nel_fatigue).iy_Stif(Fat_pts))

	! WRITE(*,104) 'Damage ball = ',Damage_Stif_ball(nel_fatigue,panneau(nel_fatigue).ix_Stif(Fat_pts),panneau(nel_fatigue).iy_Stif(Fat_pts))

	! WRITE(*,104) 'Damage total = ',Damage_Stif_total(nel_fatigue,panneau(nel_fatigue).ix_Stif(Fat_pts),panneau(nel_fatigue).iy_Stif(Fat_pts))


	WRITE(66,*) '------------------'

!	WRITE(66,118) 'STIFFENERS :','IX =',panneau(nel_fatigue).ix_Stif(Fat_pts),'IY =',panneau(nel_fatigue).iy_Stif(Fat_pts)

	if ((panneau(nel_fatigue).ix_Stif(Fat_pts).eq.1).and.(panneau(nel_fatigue).iy_Stif(Fat_pts).eq.1)) then
	
	WRITE(66,135) 'STIFFENERS : x = 0 , Departure Node'
					
	elseif ((panneau(nel_fatigue).ix_Stif(Fat_pts).eq.1).and.(panneau(nel_fatigue).iy_Stif(Fat_pts).eq.2)) then

	WRITE(66,135) 'STIFFENERS :x = 0, Mid-Plate Node'

	elseif ((panneau(nel_fatigue).ix_Stif(Fat_pts).eq.1).and.(panneau(nel_fatigue).iy_Stif(Fat_pts).eq.3)) then

	WRITE(66,135) 'STIFFENERS :x = 0, Arrival Node'

	elseif ((panneau(nel_fatigue).ix_Stif(Fat_pts).eq.2).and.(panneau(nel_fatigue).iy_Stif(Fat_pts).eq.1)) then

	WRITE(66,135) 'STIFFENERS :x = L/4, Departure Node'

	elseif ((panneau(nel_fatigue).ix_Stif(Fat_pts).eq.2).and.(panneau(nel_fatigue).iy_Stif(Fat_pts).eq.2)) then

	WRITE(66,135) 'STIFFENERS :x = L/4, Mid-Plate Node'

	elseif ((panneau(nel_fatigue).ix_Stif(Fat_pts).eq.2).and.(panneau(nel_fatigue).iy_Stif(Fat_pts).eq.3)) then

	WRITE(66,135) 'STIFFENERS :x = L/4, Arrival Node'

	elseif ((panneau(nel_fatigue).ix_Stif(Fat_pts).eq.3).and.(panneau(nel_fatigue).iy_Stif(Fat_pts).eq.1)) then

	WRITE(66,135) 'STIFFENERS :x = L/2, Departure Node'

	elseif ((panneau(nel_fatigue).ix_Stif(Fat_pts).eq.3).and.(panneau(nel_fatigue).iy_Stif(Fat_pts).eq.2)) then

	WRITE(66,135) 'STIFFENERS :x = L/2, Mid-Plate Node'

	elseif ((panneau(nel_fatigue).ix_Stif(Fat_pts).eq.3).and.(panneau(nel_fatigue).iy_Stif(Fat_pts).eq.3)) then

	WRITE(66,135) 'STIFFENERS :x = L/2, Arrival Node'

	ENDIF


	WRITE(66,104) 'kw',panneau(nel_fatigue).Kw_Stif(Fat_pts)


!stress
WRITE(66,130) 'NOMINAL STRESS','HOTSPOT STRESS','NOTCH STRESS','DAMAGE','Kc'
DO caschge_fatigue=1,nsol_fatigue

WRITE(66,131) NOM_STRESS_stif(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Stif(Fat_pts),&
panneau(nel_fatigue).iy_Stif(Fat_pts)),	&
HS_STRESS_stif(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Stif(Fat_pts),panneau(nel_fatigue).iy_Stif(Fat_pts)),	&
NS_STRESS_stif(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Stif(Fat_pts),panneau(nel_fatigue).iy_Stif(Fat_pts)),	&
Damage_Stif(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Stif(Fat_pts),panneau(nel_fatigue).iy_Stif(Fat_pts)),	&
kc_Stif(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Stif(Fat_pts),panneau(nel_fatigue).iy_Stif(Fat_pts))


ENDDO


WRITE(66,104) 'Damage full = ', Damage_Stif_full(nel_fatigue,panneau(nel_fatigue).ix_Stif(Fat_pts),&
panneau(nel_fatigue).iy_Stif(Fat_pts))

WRITE(66,104) 'Damage ball = ',Damage_Stif_ball(nel_fatigue,panneau(nel_fatigue).ix_Stif(Fat_pts),&
panneau(nel_fatigue).iy_Stif(Fat_pts))

WRITE(66,104) 'Damage total = ',Damage_Stif_total(nel_fatigue,panneau(nel_fatigue).ix_Stif(Fat_pts),&
panneau(nel_fatigue).iy_Stif(Fat_pts))


	enddo

	endif


	if (panneau(nel_fatigue).I_Fat_Plate .eq. 0) THEN

	! WRITE(*,*) 'plate hot spots NOT CONSIDERED '

	WRITE(66,*) 'plate hot spots NOT CONSIDERED '


	ELSE
	
	DO Fat_pts=1,panneau(nel_fatigue).nbr_Fat_Plate

	! WRITE(*,*) '------------------'

!	! WRITE(*,118) 'Plate :','IX',panneau(nel_fatigue).ix_Plate(Fat_pts),'IY',panneau(nel_fatigue).iy_Plate(Fat_pts)

	if ((panneau(nel_fatigue).ix_Plate(Fat_pts).eq.1).and.(panneau(nel_fatigue).iy_Plate(Fat_pts).eq.1)) then
	
	! WRITE(*,135) 'Plate : x = 0 , Departure Node'
					
	elseif ((panneau(nel_fatigue).ix_Plate(Fat_pts).eq.1).and.(panneau(nel_fatigue).iy_Plate(Fat_pts).eq.2)) then

	! WRITE(*,135) 'Plate :x = 0, Mid-Plate Node'

	elseif ((panneau(nel_fatigue).ix_Plate(Fat_pts).eq.1).and.(panneau(nel_fatigue).iy_Plate(Fat_pts).eq.3)) then

	! WRITE(*,135) 'Plate :x = 0, Arrival Node'

	elseif ((panneau(nel_fatigue).ix_Plate(Fat_pts).eq.2).and.(panneau(nel_fatigue).iy_Plate(Fat_pts).eq.1)) then

	! WRITE(*,135) 'Plate :x = L/4, Departure Node'

	elseif ((panneau(nel_fatigue).ix_Plate(Fat_pts).eq.2).and.(panneau(nel_fatigue).iy_Plate(Fat_pts).eq.2)) then

	! WRITE(*,135) 'Plate :x = L/4, Mid-Plate Node'

	elseif ((panneau(nel_fatigue).ix_Plate(Fat_pts).eq.2).and.(panneau(nel_fatigue).iy_Plate(Fat_pts).eq.3)) then

	! WRITE(*,135) 'Plate :x = L/4, Arrival Node'

	elseif ((panneau(nel_fatigue).ix_Plate(Fat_pts).eq.3).and.(panneau(nel_fatigue).iy_Plate(Fat_pts).eq.1)) then

	! WRITE(*,135) 'Plate :x = L/2, Departure Node'

	elseif ((panneau(nel_fatigue).ix_Plate(Fat_pts).eq.3).and.(panneau(nel_fatigue).iy_Plate(Fat_pts).eq.2)) then

	! WRITE(*,135) 'Plate :x = L/2, Mid-Plate Node'

	elseif ((panneau(nel_fatigue).ix_Plate(Fat_pts).eq.3).and.(panneau(nel_fatigue).iy_Plate(Fat_pts).eq.3)) then

	! WRITE(*,135) 'Plate :x = L/2, Arrival Node'

	ENDIF

	! WRITE(*,104) 'Damage full = ', Damage_Plate_full(nel_fatigue,panneau(nel_fatigue).ix_Plate(Fat_pts),panneau(nel_fatigue).iy_Plate(Fat_pts))

	! WRITE(*,104) 'Damage ball = ',Damage_Plate_ball(nel_fatigue,panneau(nel_fatigue).ix_Plate(Fat_pts),panneau(nel_fatigue).iy_Plate(Fat_pts))

	! WRITE(*,104) 'Damage total = ',Damage_Plate_total(nel_fatigue,panneau(nel_fatigue).ix_Plate(Fat_pts),panneau(nel_fatigue).iy_Plate(Fat_pts))

	WRITE(66,*) '------------------'

!	WRITE(66,118) 'Plate :','IX',panneau(nel_fatigue).ix_Plate(Fat_pts),'IY',panneau(nel_fatigue).iy_Plate(Fat_pts)

	if ((panneau(nel_fatigue).ix_Plate(Fat_pts).eq.1).and.(panneau(nel_fatigue).iy_Plate(Fat_pts).eq.1)) then
	
	WRITE(66,135) 'Plate : x = 0 , Departure Node'
					
	elseif ((panneau(nel_fatigue).ix_Plate(Fat_pts).eq.1).and.(panneau(nel_fatigue).iy_Plate(Fat_pts).eq.2)) then

	WRITE(66,135) 'Plate :x = 0, Mid-Plate Node'

	elseif ((panneau(nel_fatigue).ix_Plate(Fat_pts).eq.1).and.(panneau(nel_fatigue).iy_Plate(Fat_pts).eq.3)) then

	WRITE(66,135) 'Plate :x = 0, Arrival Node'

	elseif ((panneau(nel_fatigue).ix_Plate(Fat_pts).eq.2).and.(panneau(nel_fatigue).iy_Plate(Fat_pts).eq.1)) then

	WRITE(66,135) 'Plate :x = L/4, Departure Node'

	elseif ((panneau(nel_fatigue).ix_Plate(Fat_pts).eq.2).and.(panneau(nel_fatigue).iy_Plate(Fat_pts).eq.2)) then

	WRITE(66,135) 'Plate :x = L/4, Mid-Plate Node'

	elseif ((panneau(nel_fatigue).ix_Plate(Fat_pts).eq.2).and.(panneau(nel_fatigue).iy_Plate(Fat_pts).eq.3)) then

	WRITE(66,135) 'Plate :x = L/4, Arrival Node'

	elseif ((panneau(nel_fatigue).ix_Plate(Fat_pts).eq.3).and.(panneau(nel_fatigue).iy_Plate(Fat_pts).eq.1)) then

	WRITE(66,135) 'Plate :x = L/2, Departure Node'

	elseif ((panneau(nel_fatigue).ix_Plate(Fat_pts).eq.3).and.(panneau(nel_fatigue).iy_Plate(Fat_pts).eq.2)) then

	WRITE(66,135) 'Plate :x = L/2, Mid-Plate Node'

	elseif ((panneau(nel_fatigue).ix_Plate(Fat_pts).eq.3).and.(panneau(nel_fatigue).iy_Plate(Fat_pts).eq.3)) then

	WRITE(66,135) 'Plate :x = L/2, Arrival Node'

	ENDIF




!stress
	WRITE(66,130) 'NOMINAL STRESS','HOTSPOT STRESS','NOTCH STRESS','DAMAGE','Kc'
	DO caschge_fatigue=1,nsol_fatigue

	WRITE(66,131) NOM_STRESS_plate(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_plate(Fat_pts),&
	panneau(nel_fatigue).iy_plate(Fat_pts)),HS_STRESS_plate(caschge_fatigue,nel_fatigue,&
	panneau(nel_fatigue).ix_plate(Fat_pts),panneau(nel_fatigue).iy_plate(Fat_pts)),	&
	NS_STRESS_plate(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_plate(Fat_pts),&
	panneau(nel_fatigue).iy_plate(Fat_pts)),Damage_plate(caschge_fatigue,nel_fatigue,&
	panneau(nel_fatigue).ix_plate(Fat_pts),panneau(nel_fatigue).iy_plate(Fat_pts)),	&
	kc_plate(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_plate(Fat_pts),panneau(nel_fatigue).iy_plate(Fat_pts))

	ENDDO


	WRITE(66,104) 'Damage full = ', Damage_Plate_full(nel_fatigue,panneau(nel_fatigue).ix_Plate(Fat_pts),&
	panneau(nel_fatigue).iy_Plate(Fat_pts))

	WRITE(66,104) 'Damage ball = ',Damage_Plate_ball(nel_fatigue,panneau(nel_fatigue).ix_Plate(Fat_pts),&
	panneau(nel_fatigue).iy_Plate(Fat_pts))

	WRITE(66,104) 'Damage total = ',Damage_Plate_total(nel_fatigue,panneau(nel_fatigue).ix_Plate(Fat_pts),&
	panneau(nel_fatigue).iy_Plate(Fat_pts))

	enddo

	endif




	if (panneau(nel_fatigue).I_Fat_Plate_Frame .eq. 0) THEN

	! WRITE(*,*) '------------------'

	! WRITE(*,*) 'plate/frame connexion hot spots NOT CONSIDERED '

	WRITE(66,*) 'plate/frame connexion hot spots NOT CONSIDERED '

	ELSE
	
	DO Fat_pts=1,panneau(nel_fatigue).nbr_Fat_Plate_Frame

!	! WRITE(*,118) 'plate/frame connexion :','IX',panneau(nel_fatigue).ix_Plate_Frame(Fat_pts),'IY',panneau(nel_fatigue).iy_Plate_Frame(Fat_pts)

	if ((panneau(nel_fatigue).ix_Plate_Frame(Fat_pts).eq.1).and.(panneau(nel_fatigue).iy_Plate_Frame(Fat_pts).eq.1)) then
	
	! WRITE(*,135) 'plate/frame: x = 0 , Departure Node'
					
	elseif ((panneau(nel_fatigue).ix_Plate_Frame(Fat_pts).eq.1).and.(panneau(nel_fatigue).iy_Plate_Frame(Fat_pts).eq.2)) then

	! WRITE(*,135) 'plate/frame :x = 0, Mid-Plate Node'

	elseif ((panneau(nel_fatigue).ix_Plate_Frame(Fat_pts).eq.1).and.(panneau(nel_fatigue).iy_Plate_Frame(Fat_pts).eq.3)) then

	! WRITE(*,135) 'plate/frame :x = 0, Arrival Node'

	elseif ((panneau(nel_fatigue).ix_Plate_Frame(Fat_pts).eq.2).and.(panneau(nel_fatigue).iy_Plate_Frame(Fat_pts).eq.1)) then

	! WRITE(*,135) 'plate/frame :x = L/4, Departure Node'

	elseif ((panneau(nel_fatigue).ix_Plate_Frame(Fat_pts).eq.2).and.(panneau(nel_fatigue).iy_Plate_Frame(Fat_pts).eq.2)) then

	! WRITE(*,135) 'plate/frame :x = L/4, Mid-Plate Node'

	elseif ((panneau(nel_fatigue).ix_Plate_Frame(Fat_pts).eq.2).and.(panneau(nel_fatigue).iy_Plate_Frame(Fat_pts).eq.3)) then

	! WRITE(*,135) 'plate/frame :x = L/4, Arrival Node'

	elseif ((panneau(nel_fatigue).ix_Plate_Frame(Fat_pts).eq.3).and.(panneau(nel_fatigue).iy_Plate_Frame(Fat_pts).eq.1)) then

	! WRITE(*,135) 'plate/frame :x = L/2, Departure Node'

	elseif ((panneau(nel_fatigue).ix_Plate_Frame(Fat_pts).eq.3).and.(panneau(nel_fatigue).iy_Plate_Frame(Fat_pts).eq.2)) then

	! WRITE(*,135) 'plate/frame :x = L/2, Mid-Plate Node'

	elseif ((panneau(nel_fatigue).ix_Plate_Frame(Fat_pts).eq.3).and.(panneau(nel_fatigue).iy_Plate_Frame(Fat_pts).eq.3)) then

	! WRITE(*,135) 'plate/frame :x = L/2, Arrival Node'

	ENDIF


	! WRITE(*,104) 'Damage full = ', Damage_Plate_Frame_full(nel_fatigue,panneau(nel_fatigue).ix_Plate_Frame(Fat_pts),&
!	panneau(nel_fatigue).iy_Plate_Frame(Fat_pts))

	! WRITE(*,104) 'Damage ball = ',Damage_Plate_Frame_ball(nel_fatigue,panneau(nel_fatigue).ix_Plate_Frame(Fat_pts),&
!	panneau(nel_fatigue).iy_Plate_Frame(Fat_pts))

	! WRITE(*,104) 'Damage total = ',Damage_Plate_Frame_total(nel_fatigue,panneau(nel_fatigue).ix_Plate_Frame(Fat_pts),&
!	panneau(nel_fatigue).iy_Plate_Frame(Fat_pts))

	WRITE(66,*) '------------------'

!	WRITE(66,118) 'plate/frame connexion :','IX',panneau(nel_fatigue).ix_Plate_Frame(Fat_pts),'IY',panneau(nel_fatigue).iy_Plate_Frame(Fat_pts)

	if ((panneau(nel_fatigue).ix_Plate_Frame(Fat_pts).eq.1).and.(panneau(nel_fatigue).iy_Plate_Frame(Fat_pts).eq.1)) then
	
	WRITE(66,135) 'plate/frame: x = 0 , Departure Node'
					
	elseif ((panneau(nel_fatigue).ix_Plate_Frame(Fat_pts).eq.1).and.(panneau(nel_fatigue).iy_Plate_Frame(Fat_pts).eq.2)) then

	WRITE(66,135) 'plate/frame :x = 0, Mid-Plate Node'

	elseif ((panneau(nel_fatigue).ix_Plate_Frame(Fat_pts).eq.1).and.(panneau(nel_fatigue).iy_Plate_Frame(Fat_pts).eq.3)) then

	WRITE(66,135) 'plate/frame :x = 0, Arrival Node'

	elseif ((panneau(nel_fatigue).ix_Plate_Frame(Fat_pts).eq.2).and.(panneau(nel_fatigue).iy_Plate_Frame(Fat_pts).eq.1)) then

	WRITE(66,135) 'plate/frame :x = L/4, Departure Node'

	elseif ((panneau(nel_fatigue).ix_Plate_Frame(Fat_pts).eq.2).and.(panneau(nel_fatigue).iy_Plate_Frame(Fat_pts).eq.2)) then

	WRITE(66,135) 'plate/frame :x = L/4, Mid-Plate Node'

	elseif ((panneau(nel_fatigue).ix_Plate_Frame(Fat_pts).eq.2).and.(panneau(nel_fatigue).iy_Plate_Frame(Fat_pts).eq.3)) then

	WRITE(66,135) 'plate/frame :x = L/4, Arrival Node'

	elseif ((panneau(nel_fatigue).ix_Plate_Frame(Fat_pts).eq.3).and.(panneau(nel_fatigue).iy_Plate_Frame(Fat_pts).eq.1)) then

	WRITE(66,135) 'plate/frame :x = L/2, Departure Node'

	elseif ((panneau(nel_fatigue).ix_Plate_Frame(Fat_pts).eq.3).and.(panneau(nel_fatigue).iy_Plate_Frame(Fat_pts).eq.2)) then

	WRITE(66,135) 'plate/frame :x = L/2, Mid-Plate Node'

	elseif ((panneau(nel_fatigue).ix_Plate_Frame(Fat_pts).eq.3).and.(panneau(nel_fatigue).iy_Plate_Frame(Fat_pts).eq.3)) then

	WRITE(66,135) 'plate/frame :x = L/2, Arrival Node'

	ENDIF


!stress
	WRITE(66,130) 'NOMINAL STRESS','HOTSPOT STRESS','NOTCH STRESS','DAMAGE','Kc'
	DO caschge_fatigue=1,nsol_fatigue

	WRITE(66,131) NOM_STRESS_plate_frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_plate_frame(Fat_pts),&
	panneau(nel_fatigue).iy_plate_frame(Fat_pts)),HS_STRESS_plate_frame(caschge_fatigue,nel_fatigue,&
	panneau(nel_fatigue).ix_plate_frame(Fat_pts),panneau(nel_fatigue).iy_plate_frame(Fat_pts)),	&
	NS_STRESS_plate_frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_plate_frame(Fat_pts),&
	panneau(nel_fatigue).iy_plate_frame(Fat_pts)),Damage_plate_frame(caschge_fatigue,nel_fatigue,&
	panneau(nel_fatigue).ix_plate_frame(Fat_pts),panneau(nel_fatigue).iy_plate_frame(Fat_pts)),	&
	kc_plate_frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_plate_frame(Fat_pts),&
	panneau(nel_fatigue).iy_plate_frame(Fat_pts))
	ENDDO


	WRITE(66,104) 'Damage full = ', Damage_Plate_Frame_full(nel_fatigue,panneau(nel_fatigue).ix_Plate_Frame(Fat_pts),&
	panneau(nel_fatigue).iy_Plate_Frame(Fat_pts))

	WRITE(66,104) 'Damage ball = ',Damage_Plate_Frame_ball(nel_fatigue,panneau(nel_fatigue).ix_Plate_Frame(Fat_pts),&
	panneau(nel_fatigue).iy_Plate_Frame(Fat_pts))

	WRITE(66,104) 'Damage total = ',Damage_Plate_Frame_total(nel_fatigue,panneau(nel_fatigue).ix_Plate_Frame(Fat_pts),&
	panneau(nel_fatigue).iy_Plate_Frame(Fat_pts))


	enddo

	endif



	if (panneau(nel_fatigue).I_Fat_Frame .eq. 0) THEN

	! WRITE(*,*) 'frame hot spots NOT CONSIDERED '

	WRITE(66,*) 'frame hot spots NOT CONSIDERED '

	ELSE
	
	DO Fat_pts=1,panneau(nel_fatigue).nbr_Fat_frame

	! WRITE(*,*) '------------------'

!	! WRITE(*,118) 'Frame :','IX',panneau(nel_fatigue).ix_Frame(Fat_pts),'IY',panneau(nel_fatigue).iy_Frame(Fat_pts)


	if ((panneau(nel_fatigue).ix_Frame(Fat_pts).eq.1).and.(panneau(nel_fatigue).iy_Frame(Fat_pts).eq.1)) then
	
	! WRITE(*,135) 'Frame: x = 0 , Departure Node'
					
	elseif ((panneau(nel_fatigue).ix_Frame(Fat_pts).eq.1).and.(panneau(nel_fatigue).iy_Frame(Fat_pts).eq.2)) then

	! WRITE(*,135) 'Frame :x = 0, Mid-Plate Node'

	elseif ((panneau(nel_fatigue).ix_Frame(Fat_pts).eq.1).and.(panneau(nel_fatigue).iy_Frame(Fat_pts).eq.3)) then

	! WRITE(*,135) 'Frame :x = 0, Arrival Node'

	elseif ((panneau(nel_fatigue).ix_Frame(Fat_pts).eq.2).and.(panneau(nel_fatigue).iy_Frame(Fat_pts).eq.1)) then

	! WRITE(*,135) 'Frame :x = L/4, Departure Node'

	elseif ((panneau(nel_fatigue).ix_Frame(Fat_pts).eq.2).and.(panneau(nel_fatigue).iy_Frame(Fat_pts).eq.2)) then

	! WRITE(*,135) 'Frame :x = L/4, Mid-Plate Node'

	elseif ((panneau(nel_fatigue).ix_Frame(Fat_pts).eq.2).and.(panneau(nel_fatigue).iy_Frame(Fat_pts).eq.3)) then

	! WRITE(*,135) 'Frame :x = L/4, Arrival Node'

	elseif ((panneau(nel_fatigue).ix_Frame(Fat_pts).eq.3).and.(panneau(nel_fatigue).iy_Frame(Fat_pts).eq.1)) then

	! WRITE(*,135) 'Frame :x = L/2, Departure Node'

	elseif ((panneau(nel_fatigue).ix_Frame(Fat_pts).eq.3).and.(panneau(nel_fatigue).iy_Frame(Fat_pts).eq.2)) then

	! WRITE(*,135) 'Frame :x = L/2, Mid-Plate Node'

	elseif ((panneau(nel_fatigue).ix_Frame(Fat_pts).eq.3).and.(panneau(nel_fatigue).iy_Frame(Fat_pts).eq.3)) then

	! WRITE(*,135) 'Frame :x = L/2, Arrival Node'

	ENDIF




	! WRITE(*,104) 'Damage full = ', Damage_Frame_full(nel_fatigue,panneau(nel_fatigue).ix_Frame(Fat_pts),panneau(nel_fatigue).iy_Frame(Fat_pts))

	! WRITE(*,104) 'Damage ball = ',Damage_Frame_ball(nel_fatigue,panneau(nel_fatigue).ix_Frame(Fat_pts),panneau(nel_fatigue).iy_Frame(Fat_pts))

	! WRITE(*,104) 'Damage total = ',Damage_Frame_total(nel_fatigue,panneau(nel_fatigue).ix_Frame(Fat_pts),panneau(nel_fatigue).iy_Frame(Fat_pts))

	WRITE(66,*) '------------------'

!	WRITE(66,118) 'Frame :','IX',panneau(nel_fatigue).ix_Frame(Fat_pts),'IY',panneau(nel_fatigue).iy_Frame(Fat_pts)


	if ((panneau(nel_fatigue).ix_Frame(Fat_pts).eq.1).and.(panneau(nel_fatigue).iy_Frame(Fat_pts).eq.1)) then
	
	WRITE(66,135) 'Frame: x = 0 , Departure Node'
					
	elseif ((panneau(nel_fatigue).ix_Frame(Fat_pts).eq.1).and.(panneau(nel_fatigue).iy_Frame(Fat_pts).eq.2)) then

	WRITE(66,135) 'Frame :x = 0, Mid-Plate Node'

	elseif ((panneau(nel_fatigue).ix_Frame(Fat_pts).eq.1).and.(panneau(nel_fatigue).iy_Frame(Fat_pts).eq.3)) then

	WRITE(66,135) 'Frame :x = 0, Arrival Node'

	elseif ((panneau(nel_fatigue).ix_Frame(Fat_pts).eq.2).and.(panneau(nel_fatigue).iy_Frame(Fat_pts).eq.1)) then

	WRITE(66,135) 'Frame :x = L/4, Departure Node'

	elseif ((panneau(nel_fatigue).ix_Frame(Fat_pts).eq.2).and.(panneau(nel_fatigue).iy_Frame(Fat_pts).eq.2)) then

	WRITE(66,135) 'Frame :x = L/4, Mid-Plate Node'

	elseif ((panneau(nel_fatigue).ix_Frame(Fat_pts).eq.2).and.(panneau(nel_fatigue).iy_Frame(Fat_pts).eq.3)) then

	WRITE(66,135) 'Frame :x = L/4, Arrival Node'

	elseif ((panneau(nel_fatigue).ix_Frame(Fat_pts).eq.3).and.(panneau(nel_fatigue).iy_Frame(Fat_pts).eq.1)) then

	WRITE(66,135) 'Frame :x = L/2, Departure Node'

	elseif ((panneau(nel_fatigue).ix_Frame(Fat_pts).eq.3).and.(panneau(nel_fatigue).iy_Frame(Fat_pts).eq.2)) then

	WRITE(66,135) 'Frame :x = L/2, Mid-Plate Node'

	elseif ((panneau(nel_fatigue).ix_Frame(Fat_pts).eq.3).and.(panneau(nel_fatigue).iy_Frame(Fat_pts).eq.3)) then

	WRITE(66,135) 'Frame :x = L/2, Arrival Node'

	ENDIF


!stress
	WRITE(66,130) 'NOMINAL STRESS','HOTSPOT STRESS','NOTCH STRESS','DAMAGE','Kc'
	DO caschge_fatigue=1,nsol_fatigue

	WRITE(66,131) NOM_STRESS_frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_frame(Fat_pts),&
	panneau(nel_fatigue).iy_frame(Fat_pts)),HS_STRESS_frame(caschge_fatigue,nel_fatigue,&
	panneau(nel_fatigue).ix_frame(Fat_pts),panneau(nel_fatigue).iy_frame(Fat_pts)),	&
	NS_STRESS_frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_frame(Fat_pts),&
	panneau(nel_fatigue).iy_frame(Fat_pts)),Damage_frame(caschge_fatigue,nel_fatigue,&
	panneau(nel_fatigue).ix_frame(Fat_pts),panneau(nel_fatigue).iy_frame(Fat_pts)),	&
	kc_frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_frame(Fat_pts),panneau(nel_fatigue).iy_frame(Fat_pts))
	ENDDO




	WRITE(66,104) 'Damage full = ', Damage_Frame_full(nel_fatigue,panneau(nel_fatigue).ix_Frame(Fat_pts),&
	panneau(nel_fatigue).iy_Frame(Fat_pts))

	WRITE(66,104) 'Damage ball = ',Damage_Frame_ball(nel_fatigue,panneau(nel_fatigue).ix_Frame(Fat_pts),&
	panneau(nel_fatigue).iy_Frame(Fat_pts))

	WRITE(66,104) 'Damage total = ',Damage_Frame_total(nel_fatigue,panneau(nel_fatigue).ix_Frame(Fat_pts),&
	panneau(nel_fatigue).iy_Frame(Fat_pts))

	enddo

	endif




!	If (MT(nel_fatigue).NE.0) Then

!		Do MMT=1,MT(nel_fatigue)

!		if (I_Fat_Girder(nel_fatigue,MMT) .eq. 0) THEN

!			! WRITE(*,*) 'Girder (',mmt,') NOT CONSIDERED '
!			WRITE(66,*) 'Girder (',mmt,') NOT CONSIDERED '
!		
!		ELSE

!			DO Fat_pts=1,nbr_Fat_Girder(nel_fatigue,MMT)
!
!			! WRITE(*,*) '------------------'

!			! WRITE(*,134) 'Girder (',mmt,'), IX ',panneau(nel_fatigue).ix_Girder(nel_fatigue,Fat_pts,MMT)

!			if (panneau(nel_fatigue).ix_Girder(nel_fatigue,Fat_pts,MMT).eq.1) then

!			! WRITE(*,136) 'Girder (',mmt,'),  x = 0 '
					
!			elseif (panneau(nel_fatigue).ix_Girder(nel_fatigue,Fat_pts,MMT).eq.2) then

!			! WRITE(*,136) 'Girder (',mmt,'),  x = L/4'

!			elseif (panneau(nel_fatigue).ix_Girder(nel_fatigue,Fat_pts,MMT).eq.3) then

!			! WRITE(*,136) 'Girder (',mmt,'),  x = L/2'

!			ENDIF

!			! WRITE(*,104) 'Damage full = ', Damage_Girder_full(nel_fatigue,panneau(nel_fatigue).ix_Girder(nel_fatigue,Fat_pts,MMT),mmt)

!			! WRITE(*,104) 'Damage ball = ',Damage_Girder_ball(nel_fatigue,panneau(nel_fatigue).ix_Girder(nel_fatigue,Fat_pts,MMT),mmt)

!			! WRITE(*,104) 'Damage total = ',Damage_Girder_total(nel_fatigue,panneau(nel_fatigue).ix_Girder(nel_fatigue,Fat_pts,MMT),mmt)
	
!			WRITE(66,*) '------------------'

!!			WRITE(66,134) 'Girder (',mmt,'), IX ',panneau(nel_fatigue).ix_Girder(nel_fatigue,Fat_pts,MMT)


!			if (panneau(nel_fatigue).ix_Girder(nel_fatigue,Fat_pts,MMT).eq.1) then
!
!			WRITE(66,136) 'Girder (',mmt,'),  x = 0 '
!					
!			elseif (panneau(nel_fatigue).ix_Girder(nel_fatigue,Fat_pts,MMT).eq.2) then

!			WRITE(66,136) 'Girder (',mmt,'),  x = L/4'

!			elseif (panneau(nel_fatigue).ix_Girder(nel_fatigue,Fat_pts,MMT).eq.3) then

!			WRITE(66,136) 'Girder (',mmt,'),  x = L/2'

!			ENDIF


!			stress
!			WRITE(66,130) 'NOMINAL STRESS','HOTSPOT STRESS','NOTCH STRESS','DAMAGE','Kc'
!			DO caschge_fatigue=1,nsol_fatigue

!			WRITE(66,131) NOM_STRESS_Girder(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Girder(nel_fatigue,Fat_pts,MMT),MMT),	&
!				HS_STRESS_Girder(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Girder(nel_fatigue,Fat_pts,MMT),MMT),	&
!				NS_STRESS_Girder(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Girder(nel_fatigue,Fat_pts,MMT),MMT),	&
!				Damage_Girder(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Girder(nel_fatigue,Fat_pts,MMT),MMT),	&
!				kc_Girder(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Girder(nel_fatigue,Fat_pts,MMT),MMT)
!			ENDDO

!			WRITE(66,104) 'Damage full = ', Damage_Girder_full(nel_fatigue,panneau(nel_fatigue).ix_Girder(nel_fatigue,Fat_pts,MMT),mmt)

!			WRITE(66,104) 'Damage ball = ',Damage_Girder_ball(nel_fatigue,panneau(nel_fatigue).ix_Girder(nel_fatigue,Fat_pts,MMT),mmt)

!			WRITE(66,104) 'Damage total = ',Damage_Girder_total(nel_fatigue,panneau(nel_fatigue).ix_Girder(nel_fatigue,Fat_pts,MMT),mmt)

!			enddo

!		endif

!		enddo

!	endif



!Pillars--------------------------------------------------------------

elseif(itype(nel_fatigue).eq.5) then		!Pillar


!	WRITE(66,*) '------------------------------------------'
 !   WRITE(66,117) 'pillar',nel_fatigue				
 !	WRITE(66,*) '------------------------------------------'

!	! WRITE(*,*) '------------------------------------------'
 !   ! WRITE(*,117) 'pillar',nel_fatigue			
!	! WRITE(*,*) '------------------------------------------'
!
!
!
!	if (I_Fat_Pillar(nel_fatigue) .eq. 0) THEN
!
!	! WRITE(*,*) 'Pillar hot spots NOT CONSIDERED '
!	WRITE(66,*) 'Pillar hot spots NOT CONSIDERED '
!
!	ELSE
!	
!	DO Fat_pts=1,nbr_Fat_Pillar(nel_fatigue)
!
!	! WRITE(*,*) '------------------'
!
!	! WRITE(*,118) 'Pillar :','IX',panneau(nel_fatigue).ix_Pillar(Fat_pts),'IY',panneau(nel_fatigue).iy_Pillar(Fat_pts)
!
!
!	if ((panneau(nel_fatigue).ix_Pillar(Fat_pts).eq.1).and.(panneau(nel_fatigue).iy_Pillar(Fat_pts).eq.1)) then
!	
!	WRITE(66,135) 'Pillar: x = 0 , Departure Node'
!					
!	elseif ((panneau(nel_fatigue).ix_Pillar(Fat_pts).eq.1).and.(panneau(nel_fatigue).iy_Pillar(Fat_pts).eq.3)) then
!
!	WRITE(66,135) 'Pillar :x = 0, Arrival Node'
!
!	elseif ((panneau(nel_fatigue).ix_Pillar(Fat_pts).eq.2).and.(panneau(nel_fatigue).iy_Pillar(Fat_pts).eq.1)) then
!
!	WRITE(66,135) 'Pillar :x = L/4, Departure Node'
!
!	elseif ((panneau(nel_fatigue).ix_Pillar(Fat_pts).eq.2).and.(panneau(nel_fatigue).iy_Pillar(Fat_pts).eq.3)) then
!
!	WRITE(66,135) 'Pillar :x = L/4, Arrival Node'
!
!	elseif ((panneau(nel_fatigue).ix_Pillar(Fat_pts).eq.3).and.(panneau(nel_fatigue).iy_Pillar(Fat_pts).eq.1)) then
!
!	WRITE(66,135) 'Pillar :x = L/2, Departure Node'
!
!	elseif ((panneau(nel_fatigue).ix_Pillar(Fat_pts).eq.3).and.(panneau(nel_fatigue).iy_Pillar(Fat_pts).eq.3)) then
!
!	WRITE(66,135) 'Pillar :x = L/2, Arrival Node'
!
!	ENDIF
!
!
!
!	! WRITE(*,104) 'Damage full = ', Damage_Pillar_full(nel_fatigue,panneau(nel_fatigue).ix_Pillar(Fat_pts),panneau(nel_fatigue).iy_Pillar(Fat_pts))
!
!	! WRITE(*,104) 'Damage ball = ',Damage_Pillar_ball(nel_fatigue,panneau(nel_fatigue).ix_Pillar(Fat_pts),panneau(nel_fatigue).iy_Pillar(Fat_pts))
!
!	! WRITE(*,104) 'Damage total = ',Damage_Pillar_total(nel_fatigue,panneau(nel_fatigue).ix_Pillar(Fat_pts),panneau(nel_fatigue).iy_Pillar(Fat_pts))
!
!	WRITE(66,*) '------------------'
!	
!	WRITE(66,118) 'Pillar :','IX',panneau(nel_fatigue).ix_Pillar(Fat_pts),'IY',panneau(nel_fatigue).iy_Pillar(Fat_pts)
!
!!stress
!	WRITE(66,130) 'NOMINAL STRESS','HOTSPOT STRESS','NOTCH STRESS','DAMAGE','Kc'
!	DO caschge_fatigue=1,nsol_fatigue
!
!	WRITE(66,131) NOM_STRESS_Pillar(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Pillar(Fat_pts),panneau(nel_fatigue).iy_Pillar(Fat_pts)),	&
!				HS_STRESS_Pillar(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Pillar(Fat_pts),panneau(nel_fatigue).iy_Pillar(Fat_pts)),	&
!				NS_STRESS_Pillar(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Pillar(Fat_pts),panneau(nel_fatigue).iy_Pillar(Fat_pts)),	&
!				Damage_Pillar(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Pillar(Fat_pts),panneau(nel_fatigue).iy_Pillar(Fat_pts)),	&
!				kc_Pillar(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Pillar(Fat_pts),panneau(nel_fatigue).iy_Pillar(Fat_pts))
!	ENDDO
!
!
!
!	WRITE(66,104) 'Damage full = ', Damage_Pillar_full(nel_fatigue,panneau(nel_fatigue).ix_Pillar(Fat_pts),panneau(nel_fatigue).iy_Pillar(Fat_pts))
!
!	WRITE(66,104) 'Damage ball = ',Damage_Pillar_ball(nel_fatigue,panneau(nel_fatigue).ix_Pillar(Fat_pts),panneau(nel_fatigue).iy_Pillar(Fat_pts))
!
!	WRITE(66,104) 'Damage total = ',Damage_Pillar_total(nel_fatigue,panneau(nel_fatigue).ix_Pillar(Fat_pts),panneau(nel_fatigue).iy_Pillar(Fat_pts))
!
!	enddo
!
!	endif
!
!
	ENDIF
!
!
ENDDO



! Formats .....................................................................


   10 FORMAT (A40,7F20.3)				!NEW3
   11 FORMAT (A40,7A20)				!NEW3
 
 
   12 FORMAT (A40,9F20.3)				!NEW3
   13 FORMAT (A40,9A20)				!NEW3

   14 FORMAT (A40,8F20.3)				!NEW3
   15 FORMAT (A40,8A20)				!NEW3

 100	FORMAT (f8.2,I3,3f8.2,A100)
 101	FORMAT (4f8.3,A100)
 102	FORMAT (4f9.3,A100)
 103	FORMAT (4f9.3,A100)
 104  FORMAT (A25,F20.4) 
 105  FORMAT (A48,F20.4) 
 106  FORMAT (A10,I3) 
 107  FORMAT (A38,F20.4) 
! 107  FORMAT (A38,E20.4) 
!  107  FORMAT (A38,F20.3) 
 108  FORMAT (A50) 
 109  FORMAT (9E12.2) 
 110  FORMAT (A8,F20.3)
 111  FORMAT (A8,A8)
 112  FORMAT (A58,200F20.5)
 113  FORMAT (A20) 
 114  FORMAT (A38,4F20.4) 
 115  FORMAT (A48,I5,A6)
 116  FORMAT (A48,F20.4,A6)
 117  FORMAT (A10,I3) 			
 118  FORMAT (A10,A10,I3,A10,I3,A10,I3) 

119  FORMAT (I3,A30)
120  FORMAT (2I4,A10)
121  FORMAT (F10.2,I8,3F10.2,A50)
122  FORMAT (I3,A50)

 123  FORMAT (A20,I3)


 1240  FORMAT (3I10,4F10.2,I10,F25.2,F10.2)
! 124  FORMAT (A3,A3,A3,A10,A10,A10,A10,A3,A25,A10)
124  FORMAT (8A10,A25,A10)

 125  FORMAT (2I10,4F10.2,I10,F25.2,F10.2)
 1250  FORMAT (7A10,A25,A10)

126  FORMAT (2F20.2)
127  FORMAT (6F20.2)
128  FORMAT (4F20.2)
1280  FORMAT (4A20)
129  FORMAT (F20.2)
1290  FORMAT (A20)
130  FORMAT (5A15)
131  FORMAT (5F15.2)
132  FORMAT (2A20)
133  FORMAT (6A20)
134  FORMAT (A10,I3,A10,I3) 
135  FORMAT (A35)
136  FORMAT (A10,I3,A15)

	END
