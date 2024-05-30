subroutine deallocate_param_read_write_vector()

use param_section, ONLY : nfile,nmax_sup
use PARAM_SECTION_VECTOR

do i=1,nmax_sup
	do  j=1,nfile
		if (ASSOCIATED(panneau_vector(i,j)%xi_full)) deallocate (panneau_vector(i,j)%xi_full)
		if (ASSOCIATED(panneau_vector(i,j)%xf_full)) deallocate (panneau_vector(i,j)%xf_full)
		if (ASSOCIATED(panneau_vector(i,j)%chamax)) deallocate (panneau_vector(i,j)%chamax)
		if (ASSOCIATED(panneau_vector(i,j)%dchamax)) deallocate (panneau_vector(i,j)%dchamax)
		if (ASSOCIATED(panneau_vector(i,j)%lcont)) deallocate (panneau_vector(i,j)%lcont)
		if (ASSOCIATED(panneau_vector(i,j)%cjmax)) deallocate (panneau_vector(i,j)%cjmax)
		if (ASSOCIATED(panneau_vector(i,j)%inv)) deallocate (panneau_vector(i,j)%inv)
		if (ASSOCIATED(panneau_vector(i,j)%lcont4_init)) deallocate (panneau_vector(i,j)%lcont4_init)
		if (ASSOCIATED(panneau_vector(i,j)%lcont4)) deallocate (panneau_vector(i,j)%lcont4)
		if (ASSOCIATED(panneau_vector(i,j)%inv3_init)) deallocate (panneau_vector(i,j)%inv3_init)
		if (ASSOCIATED(panneau_vector(i,j)%inv3)) deallocate (panneau_vector(i,j)%inv3)
		if (ASSOCIATED(panneau_vector(i,j)%cjmax9_init)) deallocate (panneau_vector(i,j)%cjmax9_init)
		if (ASSOCIATED(panneau_vector(i,j)%cjmax9)) deallocate (panneau_vector(i,j)%cjmax9)
		if (ASSOCIATED(panneau_vector(i,j)%sigplaque)) deallocate (panneau_vector(i,j)%sigplaque)
		if (ASSOCIATED(panneau_vector(i,j)%dsigplaque)) deallocate (panneau_vector(i,j)%dsigplaque)
		if (ASSOCIATED(panneau_vector(i,j)%sigxplaque)) deallocate (panneau_vector(i,j)%sigxplaque)
		if (ASSOCIATED(panneau_vector(i,j)%sigyplaque)) deallocate (panneau_vector(i,j)%sigyplaque)
		if (ASSOCIATED(panneau_vector(i,j)%phiplaque)) deallocate (panneau_vector(i,j)%phiplaque)
		if (ASSOCIATED(panneau_vector(i,j)%sigvmtplaque)) deallocate (panneau_vector(i,j)%sigvmtplaque)
		if (ASSOCIATED(panneau_vector(i,j)%sigvmcplaque)) deallocate (panneau_vector(i,j)%sigvmcplaque)
		if (ASSOCIATED(panneau_vector(i,j)%indplaque)) deallocate (panneau_vector(i,j)%indplaque)
		if (ASSOCIATED(panneau_vector(i,j)%sigmx)) deallocate (panneau_vector(i,j)%sigmx)
		if (ASSOCIATED(panneau_vector(i,j)%sigmy)) deallocate (panneau_vector(i,j)%sigmy)

		if (ASSOCIATED(panneau_vector(i,j)%ix_Stif)) deallocate (panneau_vector(i,j)%ix_Stif)
		if (ASSOCIATED(panneau_vector(i,j)%iy_Stif)) deallocate (panneau_vector(i,j)%iy_Stif)
		if (ASSOCIATED(panneau_vector(i,j)%ES_Stif)) deallocate (panneau_vector(i,j)%ES_Stif)
		if (ASSOCIATED(panneau_vector(i,j)%KT_Stif)) deallocate (panneau_vector(i,j)%KT_Stif)
		if (ASSOCIATED(panneau_vector(i,j)%Kw_Stif)) deallocate (panneau_vector(i,j)%Kw_Stif)
		if (ASSOCIATED(panneau_vector(i,j)%lambda_stif)) deallocate (panneau_vector(i,j)%lambda_stif)
		if (ASSOCIATED(panneau_vector(i,j)%teta_stif)) deallocate (panneau_vector(i,j)%teta_stif)
		if (ASSOCIATED(panneau_vector(i,j)%BetaIf_stif)) deallocate (panneau_vector(i,j)%BetaIf_stif)
		if (ASSOCIATED(panneau_vector(i,j)%m_stif)) deallocate (panneau_vector(i,j)%m_stif)
		if (ASSOCIATED(panneau_vector(i,j)%c_stif)) deallocate (panneau_vector(i,j)%c_stif)
		if (ASSOCIATED(panneau_vector(i,j)%weib_fact_stif)) deallocate (panneau_vector(i,j)%weib_fact_stif)

		if (ASSOCIATED(panneau_vector(i,j)%ix_Plate)) deallocate (panneau_vector(i,j)%ix_Plate)
		if (ASSOCIATED(panneau_vector(i,j)%iy_Plate)) deallocate (panneau_vector(i,j)%iy_Plate)
		!if (ASSOCIATED(panneau_vector(i,j)%ES_Plate)) deallocate (panneau_vector(i,j)%ES_Plate)
		if (ASSOCIATED(panneau_vector(i,j)%KT_Plate)) deallocate (panneau_vector(i,j)%KT_Plate)
		if (ASSOCIATED(panneau_vector(i,j)%KT_Plate)) deallocate (panneau_vector(i,j)%Kw_Plate)
		if (ASSOCIATED(panneau_vector(i,j)%lambda_Plate)) deallocate (panneau_vector(i,j)%lambda_Plate)
		if (ASSOCIATED(panneau_vector(i,j)%teta_Plate)) deallocate (panneau_vector(i,j)%teta_Plate)
		if (ASSOCIATED(panneau_vector(i,j)%BetaIf_Plate)) deallocate (panneau_vector(i,j)%BetaIf_Plate)
		if (ASSOCIATED(panneau_vector(i,j)%m_Plate)) deallocate (panneau_vector(i,j)%m_Plate)
		if (ASSOCIATED(panneau_vector(i,j)%c_Plate)) deallocate (panneau_vector(i,j)%c_Plate)
		if (ASSOCIATED(panneau_vector(i,j)%weib_fact_Plate)) deallocate (panneau_vector(i,j)%weib_fact_Plate)

		if (ASSOCIATED(panneau_vector(i,j)%ix_Plate_Frame)) deallocate (panneau_vector(i,j)%ix_Plate_Frame)
		if (ASSOCIATED(panneau_vector(i,j)%iy_Plate_Frame)) deallocate (panneau_vector(i,j)%iy_Plate_Frame)
		!if (ASSOCIATED(panneau_vector(i,j)%ES_Plate_Frame)) deallocate (panneau_vector(i,j)%ES_Plate_Frame)
		if (ASSOCIATED(panneau_vector(i,j)%KT_Plate_Frame)) deallocate (panneau_vector(i,j)%KT_Plate_Frame)
		if (ASSOCIATED(panneau_vector(i,j)%KT_Plate_Frame)) deallocate (panneau_vector(i,j)%Kw_Plate_Frame)
		if (ASSOCIATED(panneau_vector(i,j)%lambda_Plate_Frame)) deallocate (panneau_vector(i,j)%lambda_Plate_Frame)
		if (ASSOCIATED(panneau_vector(i,j)%teta_Plate_Frame)) deallocate (panneau_vector(i,j)%teta_Plate_Frame)
		if (ASSOCIATED(panneau_vector(i,j)%BetaIf_Plate_Frame)) deallocate (panneau_vector(i,j)%BetaIf_Plate_Frame)
		if (ASSOCIATED(panneau_vector(i,j)%m_Plate_Frame)) deallocate (panneau_vector(i,j)%m_Plate_Frame)
		if (ASSOCIATED(panneau_vector(i,j)%c_Plate_Frame)) deallocate (panneau_vector(i,j)%c_Plate_Frame)
		if (ASSOCIATED(panneau_vector(i,j)%weib_fact_Plate_Frame)) deallocate (panneau_vector(i,j)%weib_fact_Plate_Frame)

		if (ASSOCIATED(panneau_vector(i,j)%ix_Frame)) deallocate (panneau_vector(i,j)%ix_Frame)
		if (ASSOCIATED(panneau_vector(i,j)%iy_Frame)) deallocate (panneau_vector(i,j)%iy_Frame)
		if (ASSOCIATED(panneau_vector(i,j)%ES_Frame)) deallocate (panneau_vector(i,j)%ES_Frame)
		if (ASSOCIATED(panneau_vector(i,j)%KT_Frame)) deallocate (panneau_vector(i,j)%KT_Frame)
		if (ASSOCIATED(panneau_vector(i,j)%Kw_Frame)) deallocate (panneau_vector(i,j)%Kw_Frame)
		if (ASSOCIATED(panneau_vector(i,j)%lambda_Frame)) deallocate (panneau_vector(i,j)%lambda_Frame)
		if (ASSOCIATED(panneau_vector(i,j)%teta_Frame)) deallocate (panneau_vector(i,j)%teta_Frame)
		if (ASSOCIATED(panneau_vector(i,j)%BetaIf_Frame)) deallocate (panneau_vector(i,j)%BetaIf_Frame)
		if (ASSOCIATED(panneau_vector(i,j)%m_Frame)) deallocate (panneau_vector(i,j)%m_Frame)
		if (ASSOCIATED(panneau_vector(i,j)%c_Frame)) deallocate (panneau_vector(i,j)%c_Frame)
		if (ASSOCIATED(panneau_vector(i,j)%weib_fact_Frame)) deallocate (panneau_vector(i,j)%weib_fact_Frame)

		! Deallocate fait automatiquement un !NULLIFY après
		!NULLIFY (panneau_vector(i,j)%xi_full)
		!NULLIFY (panneau_vector(i,j)%xf_full)
		!NULLIFY (panneau_vector(i,j)%chamax)
		!NULLIFY (panneau_vector(i,j)%dchamax)
		!NULLIFY (panneau_vector(i,j)%lcont)
		!NULLIFY (panneau_vector(i,j)%ipts2)
		!NULLIFY (panneau_vector(i,j)%cjmax)
		!NULLIFY (panneau_vector(i,j)%inv)
		!NULLIFY (panneau_vector(i,j)%lcont4_init)
		!NULLIFY (panneau_vector(i,j)%lcont4)
		!NULLIFY (panneau_vector(i,j)%inv3_init)
		!NULLIFY (panneau_vector(i,j)%inv3)
		!NULLIFY (panneau_vector(i,j)%cjmax9_init)
		!NULLIFY (panneau_vector(i,j)%cjmax9)
		!NULLIFY (panneau_vector(i,j)%sigplaque)
		!NULLIFY (panneau_vector(i,j)%dsigplaque)
		!NULLIFY (panneau_vector(i,j)%sigxplaque)
		!NULLIFY (panneau_vector(i,j)%sigyplaque)
		!NULLIFY (panneau_vector(i,j)%phiplaque)
		!NULLIFY (panneau_vector(i,j)%sigvmtplaque)
		!NULLIFY (panneau_vector(i,j)%sigvmcplaque)
		!NULLIFY (panneau_vector(i,j)%indplaque)
		!NULLIFY (panneau_vector(i,j)%sigmx)
		!NULLIFY (panneau_vector(i,j)%sigmy)
	enddo
enddo

deallocate (panneau_vector)
deallocate (aono_vector)
deallocate (z_vector)
deallocate (noeud_vector)

deallocate (indMateriau_vector)

deallocate ( mode_vector)
deallocate (modes_vector)
deallocate (xmode_vector)

deallocate (kse_vector   )
deallocate (ipa_vector   )
deallocate (ivaria_vector)

deallocate (xi_vector)
deallocate (xf_vector)

deallocate (m1tabl_vector )
deallocate (m2cont_vector )
deallocate (m4cont_vector )
deallocate (m5cont_vector )

deallocate (noh_vector)

deallocate (tfa_vector )
deallocate (tfr_vector )

deallocate (ypt9_vector)
deallocate (ypts_vector)
deallocate (abcd_vector)

deallocate (npt_vector)
deallocate (npt_full_vector)
deallocate (cha_vector)
deallocate (cha_full_vector)

deallocate (title_cha_full_vector)
deallocate (title_m1_vector )

deallocate (title_loadcase_vector)

deallocate (bm1_vector)
deallocate (bm2_vector)
deallocate (bm3_vector)
deallocate (bm4_vector)
deallocate (sf1_vector)
deallocate (sf3_vector)

deallocate (bm11_vector)
deallocate (bm31_vector)
deallocate (sf11_vector)
deallocate (sf31_vector)

!*******************************************************************************************
!===========================================================================================
!!Ajout des variables de travail relatives à la routine Effort Tranchant
!===========================================================================================

deallocate (TEffortT_vector)
deallocate (XEffortT_vector)
deallocate (MEffortT_vector)
deallocate (PEffortT_vector)
deallocate (varoli_vector)	
deallocate (pointeur_vector)		
deallocate (aqoli_vector)		
deallocate (FFF_vector)			
deallocate (bornestiffmin_vector)
deallocate (bornestiffmax_vector)


!===========================================================================================
!===========================================================================================
!*******************************************************************************************

deallocate (itype_vector)
deallocate (isect_vector)

deallocate (sx_raid_sem_vector)
deallocate (sx_loc_raid_sem_vector) ! Contrainte locale uniquement ou combinaison ?

! Plate
deallocate (sx_plaque_top_vector)
deallocate (sy_plaque_top_vector)
deallocate (sxy_plaque_top_vector)
deallocate (sx_plaque_bott_vector)
deallocate (sy_plaque_bott_vector)
deallocate (sxy_plaque_bott_vector)

! Frame
	!sx_plaque_top
deallocate (sy_cadre_jab_vector)
deallocate (sxy_cadre_jab_vector)
deallocate (sy_cadre_sem_vector)

return
end
