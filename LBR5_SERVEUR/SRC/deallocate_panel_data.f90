subroutine deallocate_panel_data


use param_section



implicit double precision (a-h,o-z)

do i=1,neto
	! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	! on a qqpart dans le code panneau=panneau_vector
	! => panneau pointe maintenant sur panneau-vector
	! => on ne peut pas déallouer panneau, car ça déalloue le panneau_vector
	! corespondant !!! On doit doit simplement faire nullify (pour qu'il ne pointe
	! plus sur rien. Par contre, on devra à la fin du programme déallouer
	! les vecteurs panneau_vector !!!
	!!if (ASSOCIATED(panneau(i)%xi_full)) deallocate (panneau(i)%xi_full)
	!!if (ASSOCIATED(panneau(i)%xf_full)) deallocate (panneau(i)%xf_full)
	!!if (ASSOCIATED(panneau(i)%chamax)) deallocate (panneau(i)%chamax)
	!!if (ASSOCIATED(panneau(i)%dchamax)) deallocate (panneau(i)%dchamax)
	!if (ASSOCIATED(panneau(i)%lcont)) deallocate (panneau(i)%lcont)
	!if (ASSOCIATED(panneau(i)%ipts2)) deallocate (panneau(i)%ipts2)
	!if (ASSOCIATED(panneau(i)%cjmax)) deallocate (panneau(i)%cjmax)
	!if (ASSOCIATED(panneau(i)%inv)) deallocate (panneau(i)%inv)
	!if (ASSOCIATED(panneau(i)%lcont4_init)) deallocate (panneau(i)%lcont4_init)
	!if (ASSOCIATED(panneau(i)%lcont4)) deallocate (panneau(i)%lcont4)
	!if (ASSOCIATED(panneau(i)%inv3_init)) deallocate (panneau(i)%inv3_init)
	!if (ASSOCIATED(panneau(i)%inv3)) deallocate (panneau(i)%inv3)
	!if (ASSOCIATED(panneau(i)%cjmax9_init)) deallocate (panneau(i)%cjmax9_init)
	!if (ASSOCIATED(panneau(i)%cjmax9)) deallocate (panneau(i)%cjmax9)
	!if (ASSOCIATED(panneau(i)%sigplaque)) deallocate (panneau(i)%sigplaque)
	!if (ASSOCIATED(panneau(i)%dsigplaque)) deallocate (panneau(i)%dsigplaque)
	!if (ASSOCIATED(panneau(i)%sigxplaque)) deallocate (panneau(i)%sigxplaque)
	!if (ASSOCIATED(panneau(i)%sigyplaque)) deallocate (panneau(i)%sigyplaque)
	!if (ASSOCIATED(panneau(i)%phiplaque)) deallocate (panneau(i)%phiplaque)
	!if (ASSOCIATED(panneau(i)%sigvmtplaque)) deallocate (panneau(i)%sigvmtplaque)
	!if (ASSOCIATED(panneau(i)%sigvmcplaque)) deallocate (panneau(i)%sigvmcplaque)
	!if (ASSOCIATED(panneau(i)%indplaque)) deallocate (panneau(i)%indplaque)
	!if (ASSOCIATED(panneau(i)%sigmx)) deallocate (panneau(i)%sigmx)
	!if (ASSOCIATED(panneau(i)%sigmy)) deallocate (panneau(i)%sigmy)
	
	NULLIFY (panneau(i)%m1)
	NULLIFY (panneau(i)%xi_full)
	NULLIFY (panneau(i)%xf_full)
	NULLIFY (panneau(i)%chamax)
	NULLIFY (panneau(i)%dchamax)
	NULLIFY (panneau(i)%lcont)
!	NULLIFY (panneau(i)%ipts2)
	NULLIFY (panneau(i)%cjmax)
	NULLIFY (panneau(i)%inv)
	NULLIFY (panneau(i)%lcont4_init)
	NULLIFY (panneau(i)%lcont4)
	NULLIFY (panneau(i)%inv3_init)
	NULLIFY (panneau(i)%inv3)
	NULLIFY (panneau(i)%cjmax9_init)
	NULLIFY (panneau(i)%cjmax9)
	NULLIFY (panneau(i)%sigplaque)
	NULLIFY (panneau(i)%dsigplaque)
	NULLIFY (panneau(i)%sigxplaque)
	NULLIFY (panneau(i)%sigyplaque)
	NULLIFY (panneau(i)%phiplaque)
	NULLIFY (panneau(i)%sigvmtplaque)
	NULLIFY (panneau(i)%sigvmcplaque)
	NULLIFY (panneau(i)%indplaque)
	NULLIFY (panneau(i)%sigmx)
	NULLIFY (panneau(i)%sigmy)

	NULLIFY(panneau(i)%ix_Stif)
	NULLIFY(panneau(i)%iy_Stif)
	NULLIFY(panneau(i)%ES_Stif)
	NULLIFY(panneau(i)%KT_Stif)
	NULLIFY(panneau(i)%Kw_Stif)
	NULLIFY(panneau(i)%lambda_stif)
	NULLIFY(panneau(i)%teta_stif)
	NULLIFY(panneau(i)%BetaIf_stif)
	NULLIFY(panneau(i)%m_stif)
	NULLIFY(panneau(i)%c_stif)
	NULLIFY(panneau(i)%weib_fact_stif)

	NULLIFY(panneau(i)%ix_Plate)
	NULLIFY(panneau(i)%iy_Plate)
	!NULLIFY(panneau(i)%ES_Plate)
	NULLIFY(panneau(i)%KT_Plate)
	NULLIFY(panneau(i)%Kw_Plate)
	NULLIFY(panneau(i)%lambda_Plate)
	NULLIFY(panneau(i)%teta_Plate)
	NULLIFY(panneau(i)%BetaIf_Plate)
	NULLIFY(panneau(i)%m_Plate)
	NULLIFY(panneau(i)%c_Plate)
	NULLIFY(panneau(i)%weib_fact_Plate)

	NULLIFY(panneau(i)%ix_Plate_Frame)
	NULLIFY(panneau(i)%iy_Plate_Frame)
	!NULLIFY(panneau(i)%ES_Plate_Frame)
	NULLIFY(panneau(i)%KT_Plate_Frame)
	NULLIFY(panneau(i)%Kw_Plate_Frame)
	NULLIFY(panneau(i)%lambda_Plate_Frame)
	NULLIFY(panneau(i)%teta_Plate_Frame)
	NULLIFY(panneau(i)%BetaIf_Plate_Frame)
	NULLIFY(panneau(i)%m_Plate_Frame)
	NULLIFY(panneau(i)%c_Plate_Frame)
	NULLIFY(panneau(i)%weib_fact_Plate_Frame)

	NULLIFY(panneau(i)%ix_Frame)
	NULLIFY(panneau(i)%iy_Frame)
	NULLIFY(panneau(i)%ES_Frame)
	NULLIFY(panneau(i)%KT_Frame)
	NULLIFY(panneau(i)%Kw_Frame)
	NULLIFY(panneau(i)%lambda_Frame)
	NULLIFY(panneau(i)%teta_Frame)
	NULLIFY(panneau(i)%BetaIf_Frame)
	NULLIFY(panneau(i)%m_Frame)
	NULLIFY(panneau(i)%c_Frame)
	NULLIFY(panneau(i)%weib_fact_Frame)
enddo

deallocate (panneau)

deallocate (title_loadcase)
deallocate (title_cha_full)

deallocate (mode)

deallocate (xmode)
deallocate (denom)

!deallocate (kse   )
deallocate (ipa   )
deallocate (ivaria)

deallocate (xi )
deallocate (xf )
deallocate (xxi)
deallocate (xxf)
!deallocate (part)

deallocate (cha)
deallocate (cha_full)
deallocate (dcha)
deallocate (npt)
deallocate (npt_full)

!deallocate (m1)
deallocate (sx_raid_sem)
deallocate (sx_loc_raid_sem)

! Plate
deallocate (sx_plaque_top)
deallocate (sy_plaque_top)
deallocate (sxy_plaque_top)
deallocate (sx_plaque_bott)
deallocate (sy_plaque_bott)
deallocate (sxy_plaque_bott)

! Frame
	!sx_plaque_top
deallocate (sy_cadre_jab)
deallocate (sxy_cadre_jab)
deallocate (sy_cadre_sem)


!Stiffeners

deallocate (kc_Stif)
deallocate (NOM_STRESS_Stif)
deallocate (HS_STRESS_Stif)
deallocate (NS_STRESS_Stif)
deallocate (Damage_Stif)
deallocate (Damage_Stif_full)
deallocate (Damage_Stif_ball)
deallocate (Damage_Stif_total)


! Plate

deallocate (kc_Plate)
deallocate (NOM_STRESS_Plate)
deallocate (NOM_STRESS_Plate_top)
deallocate (NOM_STRESS_Plate_bott)
deallocate (HS_STRESS_Plate)
deallocate (NS_STRESS_Plate)
deallocate (Damage_Plate)
deallocate (Damage_Plate_full)
deallocate (Damage_Plate_ball)
deallocate (Damage_Plate_total)


!Intersection Plate/frame--------------------------------------------------------------

deallocate (kc_Plate_Frame)
deallocate (NOM_STRESS_Plate_Frame)
deallocate (NOM_STRESS_Plate_Frame_top)
deallocate (NOM_STRESS_Plate_Frame_bott)
deallocate (HS_STRESS_Plate_Frame)
deallocate (NS_STRESS_Plate_Frame)
deallocate (Damage_Plate_Frame)
deallocate (Damage_Plate_Frame_full)
deallocate (Damage_Plate_Frame_ball)
deallocate (Damage_Plate_Frame_total)

! Frame

deallocate (kc_Frame)
deallocate (NOM_STRESS_Frame)
deallocate (HS_STRESS_Frame)
deallocate (NS_STRESS_Frame)
deallocate (Damage_Frame)
deallocate (Damage_Frame_full)
deallocate (Damage_Frame_ball)
deallocate (Damage_Frame_total)



!Girders--------------------------------------------------------------


!	deallocate (kc_Girder)
!	deallocate (NOM_STRESS_Girder)
!	deallocate (HS_STRESS_Girder)
!	deallocate (NS_STRESS_Girder)
!	deallocate (Damage_Girder)
!	deallocate (Damage_Girder_full)
!	deallocate (Damage_Girder_ball)
!	deallocate (Damage_Girder_total)


!Pillars--------------------------------------------------------------

!	deallocate (kc_Pillar)
!	deallocate (NOM_STRESS_Pillar)
!	deallocate (NOM_STRESS_Pillar1)
!	deallocate (NOM_STRESS_Pillar2)
!	deallocate (HS_STRESS_Pillar)
!	deallocate (NS_STRESS_Pillar)
!	deallocate (Damage_Pillar)
!	deallocate (Damage_Pillar_full)
!	deallocate (Damage_Pillar_ball)
!	deallocate (Damage_Pillar_total)


return
end
