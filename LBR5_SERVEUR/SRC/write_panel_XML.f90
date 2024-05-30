subroutine write_panel_XML()
!------------------------------------------------------------------------------
!							Création fichier up sous forme XML
!
!	Cette sous-routine crée les fichiers up sous un format XML
!	
!	Date de création : Mai 2010
!	Auteur : F.Bair
!
!------------------------------------------------------------------------------


!	Sample Fortran program that finds a dll library and calls
!	a routine in it.
use param_XML
use dfwin


use param_section




implicit double precision (a-h,o-z)

integer*4 inum
character*1 num
character*30 double_to_str,boolean_to_str,int_to_str,side_to_str !,test
character*30 Ap

call init_XML()

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! Création du fichier XML !!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!iResult = wXML_Reader_Init_("Fichier_up")
iResult = wXML_Reader_Init_('Fichier_up'//CHAR(0))

iResult = wXML_addSon_('PROJECTS'//CHAR(0))
iResult = wXML_addAttribute_('version'//CHAR(0),header1//CHAR(0))


call wXML_Push_()
	iResult = wXML_addSon_('PROJECT'//CHAR(0))
	iResult = wXML_addAttribute_('title'//CHAR(0),header2//CHAR(0))
	
	!call wXML_Push_()
	!	iResult = wXML_addSon_('AUTHOR'//CHAR(0))
	!	iResult = wXML_addAttribute_('name'//CHAR(0),'Frederic BAIR'//CHAR(0))
	!call wXML_Pop_()
	
	call wXML_Push_()
		iResult = wXML_addSon_('COMPUTE_OPTIONS'//CHAR(0))		
		iResult = wXML_addAttribute_('fourier_terms'//CHAR(0),int_to_str(jlph2)//CHAR(0))
		!int_to_str(jlbord,text)							!semble ne plus être utile
		!iResult = wXML_addAttribute_('jlbord'//CHAR(0),text)
		!int_to_str(ndq,text)								!semble ne plus etre utile
		!iResult = wXML_addAttribute_('ndq'//CHAR(0),text)		
		iResult = wXML_addAttribute_('deadweight_active'//CHAR(0),int_to_str(ipoids)//CHAR(0))
		
		call wXML_Push_()
			iResult = wXML_addSon_('OUTPUTS'//CHAR(0))			
			iResult = wXML_addAttribute_('impression_option_1'//CHAR(0),int_to_str(impr)//CHAR(0))			
			iResult = wXML_addAttribute_('impression_option_2'//CHAR(0),int_to_str(impr2)//CHAR(0))
			!int_to_str(indaig,text)						!semble ne plus être utile
			!iResult = wXML_addAttribute_('indaig'//CHAR(0),text)
			!int_to_str(indraid,text)						!semble ne plus être utile
			!iResult = wXML_addAttribute_('indraid'//CHAR(0),text)
			!int_to_str(dessin,text)						!semble ne plus être utile
			!iResult = wXML_addAttribute_('dessin'//CHAR(0),text)
		call wXML_Pop_()
		
		call wXML_Push_()
			iResult = wXML_addSon_('SECTION'//CHAR(0))
			iResult = wXML_addAttribute_('first_section_position'//CHAR(0),double_to_str(dis(1),'(f8.5)'//CHAR(0))//CHAR(0))
			iResult = wXML_addAttribute_('second_section_position'//CHAR(0),double_to_str(dis(2),'(f8.5)'//CHAR(0))//CHAR(0))
			iResult = wXML_addAttribute_('third_section_position'//CHAR(0),double_to_str(dis(3),'(f8.5)'//CHAR(0))//CHAR(0))
			iResult = wXML_addAttribute_('fourth_section_position'//CHAR(0),double_to_str(dis(4),'(f8.5)'//CHAR(0))//CHAR(0))
			iResult = wXML_addAttribute_('fifth_section_position'//CHAR(0),double_to_str(dis(5),'(f8.5)'//CHAR(0))//CHAR(0))
		call wXML_Pop_()


		call wXML_Push_()
			iResult = wXML_addSon_('OPTIMISATION_GLOBAL'//CHAR(0))		
			iResult = wXML_addAttribute_('i_multi'//CHAR(0),int_to_str(imulti)//CHAR(0))
			iResult = wXML_addAttribute_('iteration_number'//CHAR(0),int_to_str(iteram)//CHAR(0))
			iResult = wXML_addAttribute_('i_opti'//CHAR(0),int_to_str(iopti)//CHAR(0))
			iResult = wXML_addAttribute_('i_cost'//CHAR(0),int_to_str(icout)//CHAR(0))
			iResult = wXML_addAttribute_('i_straightening'//CHAR(0),int_to_str(iredress)//CHAR(0))
			call wXML_Push_()
				iResult = wXML_addSon_('MULTI_OBJECTIVE'//CHAR(0))
				iResult = wXML_addAttribute_('rho'//CHAR(0),int_to_str(rho)//CHAR(0))
				iResult = wXML_addAttribute_('weight_cost'//CHAR(0),double_to_str(w1,'(f5.3)'//CHAR(0))//CHAR(0))
				iResult = wXML_addAttribute_('weight_weight'//CHAR(0),double_to_str(w2,'(f5.3)'//CHAR(0))//CHAR(0))
				iResult = wXML_addAttribute_('weight_inertia'//CHAR(0),double_to_str(w3,'(f5.3)'//CHAR(0))//CHAR(0))
				iResult = wXML_addAttribute_('optimal_solution_cost'//CHAR(0),double_to_str(fk(1),'(f11.1)'//CHAR(0))//CHAR(0))
				iResult = wXML_addAttribute_('optimal_solution_weight'//CHAR(0),double_to_str(fk(2),'(f11.1)'//CHAR(0))//CHAR(0))			
				iResult = wXML_addAttribute_('optimal_solution_inertia'//CHAR(0),double_to_str(fk(3),'(f11.1)'//CHAR(0))//CHAR(0))
			call wXML_Pop_()			
		call wXML_Pop_()

		call wXML_Push_()
			iResult = wXML_addSon_('COST_OPTIONS'//CHAR(0))			
			!test = double_to_str(rend,'(f8.4)'//CHAR(0))
			iResult = wXML_addAttribute_('rend_global'//CHAR(0),double_to_str(rend_global,'(f8.4)'//CHAR(0))//CHAR(0))			
			!iResult = wXML_addAttribute_('epq_global'//CHAR(0),double_to_str(epq,'(f8.4)'//CHAR(0))//CHAR(0))			
			iResult = wXML_addAttribute_('cost_DCNS'//CHAR(0),int_to_str(refthick_input)//CHAR(0)) !(0 = LBR5=NORMAL_PARAM, 1 = DCNS=DCNS_PARAM)				
			call wXML_Push_()
				iResult = wXML_addSon_('LIFE_CYCLE_COST'//CHAR(0))				
				iResult = wXML_addAttribute_('scenario'//CHAR(0),int_to_str(lccscenario)//CHAR(0))	!(1:dwt cst, 2:displ cst)
				iResult = wXML_addAttribute_('module_2'//CHAR(0),int_to_str(lcc2)//CHAR(0))	!(0=non, 1=oui)
				iResult = wXML_addAttribute_('module_3'//CHAR(0),int_to_str(lcc3)//CHAR(0))
				iResult = wXML_addAttribute_('module_4'//CHAR(0),int_to_str(lcc4)//CHAR(0))
				iResult = wXML_addAttribute_('module_5'//CHAR(0),int_to_str(lcc5)//CHAR(0))
				iResult = wXML_addAttribute_('lightweight'//CHAR(0),double_to_str(lightweight,'(f8.1)'//CHAR(0))//CHAR(0))				
				iResult = wXML_addAttribute_('deadweight'//CHAR(0),double_to_str(deadweight,'(f8.1)'//CHAR(0))//CHAR(0))
				iResult = wXML_addAttribute_('initial_weight'//CHAR(0),double_to_str(poidsLBR5_init,'(f10.1)'//CHAR(0))//CHAR(0))
			call wXML_Pop_()

		call wXML_Pop_()

	call wXML_Pop_()
	
	!!! DEFINITION DU MODELE !!!

	iResult = wXML_addSon_('MODEL'//CHAR(0))
	iResult = wXML_addAttribute_('length'//CHAR(0),double_to_str(width,'(f9.5)'//CHAR(0))//CHAR(0))	
	iResult = wXML_addAttribute_('reglementary_length'//CHAR(0),double_to_str(length,'(f9.5)'//CHAR(0))//CHAR(0))
	
	call wXML_Push_()
		iResult = wXML_addSon_('MATERIALS'//CHAR(0))
		do ind=1,nbrMat
			call wXML_Push_()
				iResult = wXML_addSon_('MATERIAL'//CHAR(0))
				iResult = wXML_addAttribute_('id'//CHAR(0),int_to_str(ind)//CHAR(0))
				iResult = wXML_addAttribute_('name'//CHAR(0),'MATERIAL '//int_to_str(ind)//CHAR(0))
				call wXML_Push_()
					iResult = wXML_addSon_('PROPERTIES'//CHAR(0))					
					iResult = wXML_addAttribute_('young_modulus'//CHAR(0),double_to_str(e(ind),'(e11.4)'//CHAR(0))//CHAR(0))
					iResult = wXML_addAttribute_('poisson_coefficient'//CHAR(0),double_to_str(eta(ind),'(f6.3)'//CHAR(0))//CHAR(0))
					iResult = wXML_addAttribute_('yield_stress'//CHAR(0),double_to_str(sigy(ind),'(e11.4)'//CHAR(0))//CHAR(0))
					iResult = wXML_addAttribute_('allowable_stress'//CHAR(0),double_to_str(sigyadm(ind),'(e11.4)'//CHAR(0))//CHAR(0))
					iResult = wXML_addAttribute_('security_coefficient'//CHAR(0),double_to_str(coefk(ind),'(f4.2)'//CHAR(0))//CHAR(0))			
					iResult = wXML_addAttribute_('specific_weight'//CHAR(0),double_to_str(spec(ind),'(e11.4)'//CHAR(0))//CHAR(0))
				call wXML_Pop_()
				call wXML_Push_()
					iResult = wXML_addSon_('MATERIAL_COST'//CHAR(0))
					call wXML_Push_()
						iResult = wXML_addSon_('NORMAL_PARAM'//CHAR(0))					
						iResult = wXML_addAttribute_('eo'//CHAR(0),double_to_str(dref(ind),'(f8.5)'//CHAR(0))//CHAR(0))
						iResult = wXML_addAttribute_('eox'//CHAR(0),double_to_str(drefx(ind),'(f8.5)'//CHAR(0))//CHAR(0))					
						iResult = wXML_addAttribute_('eoy'//CHAR(0),double_to_str(drefy(ind),'(f8.5)'//CHAR(0))//CHAR(0))
						iResult = wXML_addAttribute_('c1'//CHAR(0),double_to_str(c1(ind),'(f8.3)'//CHAR(0))//CHAR(0))
						iResult = wXML_addAttribute_('c2'//CHAR(0),double_to_str(c2(ind),'(f8.3)'//CHAR(0))//CHAR(0))
						iResult = wXML_addAttribute_('c3'//CHAR(0),double_to_str(c3(ind),'(f8.3)'//CHAR(0))//CHAR(0))
						iResult = wXML_addAttribute_('dc1'//CHAR(0),double_to_str(dc1(ind),'(f8.3)'//CHAR(0))//CHAR(0))
						iResult = wXML_addAttribute_('dw2'//CHAR(0),double_to_str(dw2(ind),'(f8.3)'//CHAR(0))//CHAR(0))
						iResult = wXML_addAttribute_('dw3'//CHAR(0),double_to_str(dw3(ind),'(f8.3)'//CHAR(0))//CHAR(0))
					call wXML_Pop_()
					call wXML_Push_()
						iResult = wXML_addSon_('DCNS_PARAM'//CHAR(0))					
						iResult = wXML_addAttribute_('dref_b'//CHAR(0),double_to_str(dref_b(ind),'(f8.5)'//CHAR(0))//CHAR(0))
						iResult = wXML_addAttribute_('dref_c'//CHAR(0),double_to_str(dref_c(ind),'(f8.5)'//CHAR(0))//CHAR(0))
						iResult = wXML_addAttribute_('dref_l'//CHAR(0),double_to_str(dref_l(ind),'(f8.5)'//CHAR(0))//CHAR(0))
						iResult = wXML_addAttribute_('c_pb'//CHAR(0),double_to_str(c_pb(ind),'(f8.3)'//CHAR(0))//CHAR(0))
						iResult = wXML_addAttribute_('c_tc'//CHAR(0),double_to_str(c_tc(ind),'(f8.3)'//CHAR(0))//CHAR(0))
						iResult = wXML_addAttribute_('c_tl'//CHAR(0),double_to_str(c_tl(ind),'(f8.3)'//CHAR(0))//CHAR(0))
						iResult = wXML_addAttribute_('dw_b'//CHAR(0),double_to_str(dw_b(ind),'(f8.3)'//CHAR(0))//CHAR(0))
						iResult = wXML_addAttribute_('dw_c'//CHAR(0),double_to_str(dw_c(ind),'(f8.3)'//CHAR(0))//CHAR(0))
						iResult = wXML_addAttribute_('dw_l'//CHAR(0),double_to_str(dw_l(ind),'(f8.3)'//CHAR(0))//CHAR(0))
					call wXML_Pop_()
				call wXML_Pop_()
				call wXML_Push_()
					iResult = wXML_addSon_('LABOUR_COST'//CHAR(0))					
					iResult = wXML_addAttribute_('labour_cost'//CHAR(0),double_to_str(labour_cost(ind),'(f8.3)'//CHAR(0))//CHAR(0))
					iResult = wXML_addAttribute_('p10'//CHAR(0),double_to_str(p10(ind),'(f8.3)'//CHAR(0))//CHAR(0))
					iResult = wXML_addAttribute_('dp10'//CHAR(0),double_to_str(dp10(ind),'(f8.3)'//CHAR(0))//CHAR(0))
					iResult = wXML_addAttribute_('p4'//CHAR(0),double_to_str(p4(ind),'(f8.3)'//CHAR(0))//CHAR(0))
					iResult = wXML_addAttribute_('p5'//CHAR(0),double_to_str(p5(ind),'(f8.3)'//CHAR(0))//CHAR(0))
					iResult = wXML_addAttribute_('dp4'//CHAR(0),double_to_str(dp4(ind),'(f8.3)'//CHAR(0))//CHAR(0))
					iResult = wXML_addAttribute_('dp5'//CHAR(0),double_to_str(dp5(ind),'(f8.3)'//CHAR(0))//CHAR(0))
					iResult = wXML_addAttribute_('p9x'//CHAR(0),double_to_str(p9x(ind),'(f8.3)'//CHAR(0))//CHAR(0))
					iResult = wXML_addAttribute_('p9y'//CHAR(0),double_to_str(p9y(ind),'(f8.3)'//CHAR(0))//CHAR(0))
					iResult = wXML_addAttribute_('dp9x'//CHAR(0),double_to_str(dp9x(ind),'(f8.3)'//CHAR(0))//CHAR(0))
					iResult = wXML_addAttribute_('dp9y'//CHAR(0),double_to_str(dp9y(ind),'(f8.3)'//CHAR(0))//CHAR(0))
					iResult = wXML_addAttribute_('p6'//CHAR(0),double_to_str(p6(ind),'(f8.3)'//CHAR(0))//CHAR(0))
					iResult = wXML_addAttribute_('p7'//CHAR(0),double_to_str(p7(ind),'(f8.3)'//CHAR(0))//CHAR(0))
					iResult = wXML_addAttribute_('betax'//CHAR(0),double_to_str(ber(ind),'(f8.3)'//CHAR(0))//CHAR(0))
					iResult = wXML_addAttribute_('betay'//CHAR(0),double_to_str(bet(ind),'(f8.3)'//CHAR(0))//CHAR(0))
				call wXML_Pop_()
				call wXML_Push_()
					iResult = wXML_addSon_('CONSUMABLE_COST'//CHAR(0))
					iResult = wXML_addAttribute_('c8'//CHAR(0),double_to_str(c8(ind),'(f8.3)'//CHAR(0))//CHAR(0))
					iResult = wXML_addAttribute_('dc8'//CHAR(0),double_to_str(dc8(ind),'(f8.3)'//CHAR(0))//CHAR(0))
					iResult = wXML_addAttribute_('alphax'//CHAR(0),int_to_str(ialr(ind))//CHAR(0))
					iResult = wXML_addAttribute_('alphay'//CHAR(0),int_to_str(ialt(ind))//CHAR(0))
				call wXML_Pop_()
			call wXML_Pop_()
		enddo
	call wXML_Pop_()

	call wXML_Push_()
		iResult = wXML_addSon_('LOADCASES'//CHAR(0))
		do is=1,nsolm
			call wXML_Push_()
				iResult = wXML_addSon_('LOADCASE'//CHAR(0))
				iResult = wXML_addAttribute_('id'//CHAR(0),int_to_str(is)//CHAR(0))
				iResult = wXML_addAttribute_('name'//CHAR(0),casename(is)//CHAR(0))
				! Test si le load case est actif
				k=0
				do jj=1, nsol
					if (nnsol(jj).eq.is) k=1
				enddo
				iResult = wXML_addAttribute_('actif'//CHAR(0),boolean_to_str(k)//CHAR(0))
				!call wXML_Push_()
				!	iResult = wXML_addSon_('MOMENTS'//CHAR(0))
					iResult = wXML_addAttribute_('reduction_coeff'//CHAR(0),double_to_str(yred,'(e14.7)')//CHAR(0)) !avant f8.5 mais ne marche plus
					!Vertical Bending Moment
					iResult = wXML_addAttribute_('vertical_bending_moment'//CHAR(0),double_to_str(bm11(is),'(e14.7)')//CHAR(0))
					!Vertical Shear Force
					iResult = wXML_addAttribute_('vertical_shear_force'//CHAR(0),double_to_str(sf11(is),'(e14.7)')//CHAR(0))
					!Horizontal Bending Moment
					iResult = wXML_addAttribute_('horizontal_bending_moment'//CHAR(0),double_to_str(bm31(is),'(e14.7)')//CHAR(0))
					!Horizontal Shear Force
					iResult = wXML_addAttribute_('horizontal_shear_force'//CHAR(0),double_to_str(sf31(is),'(e14.7)')//CHAR(0))
				!call wXML_Pop_()

			call wXML_Pop_()			
		enddo
	call wXML_Pop_()

	call wXML_Push_()
		iResult = wXML_addSon_('BOUNDARY_CONDITIONS'//CHAR(0))	
		do ii=1,ncondi
			call wXML_Push_()
				iResult = wXML_addSon_('CONDITION'//CHAR(0))
				iResult = wXML_addAttribute_('panel_id'//CHAR(0),int_to_str(nno9(ii,1))//CHAR(0))
				iResult = wXML_addAttribute_('condition_id'//CHAR(0),int_to_str(nno9(ii,2))//CHAR(0))
			call wXML_Pop_()
		enddo
	call wXML_Pop_()

	call wXML_Push_()
		iResult = wXML_addSon_('USER_COORDINATE'//CHAR(0))			
		iResult = wXML_addAttribute_('xk'//CHAR(0),double_to_str(xk9,'(f12.6)'//CHAR(0))//CHAR(0))		
		iResult = wXML_addAttribute_('yk'//CHAR(0),double_to_str(yk9,'(f12.6)'//CHAR(0))//CHAR(0))
	call wXML_Pop_()

	call wXML_Push_()
		iResult = wXML_addSon_('OPTIMISATION'//CHAR(0))	
		call wXML_Push_()
			iResult = wXML_addSon_('GLOBAL_CONSTRAINTS'//CHAR(0))
			call wXML_Push_()
				iResult = wXML_addSon_('GRAVITY'//CHAR(0))
				iResult = wXML_addAttribute_('igrav'//CHAR(0),int_to_str(igrav)//CHAR(0))				
				iResult = wXML_addAttribute_('xkg_min'//CHAR(0),double_to_str(xkgmin,'(f9.6)'//CHAR(0))//CHAR(0))
				iResult = wXML_addAttribute_('xkg_max'//CHAR(0),double_to_str(xkgmax,'(f9.6)'//CHAR(0))//CHAR(0))
			call wXML_Pop_()
			call wXML_Push_()
				iResult = wXML_addSon_('INERTIA'//CHAR(0))		
				iResult = wXML_addAttribute_('active'//CHAR(0),int_to_str(inert)//CHAR(0))			
				iResult = wXML_addAttribute_('min_inertia'//CHAR(0),double_to_str(imin,'(e14.7)')//CHAR(0))
			call wXML_Pop_()
			call wXML_Push_()
				iResult = wXML_addSon_('SECTION_MODULUS'//CHAR(0))
				iResult = wXML_addAttribute_('active'//CHAR(0),int_to_str(imod)//CHAR(0))	
				iResult = wXML_addAttribute_('min_section_modulus'//CHAR(0),double_to_str(modmin,'(e14.7)')//CHAR(0))
				iResult = wXML_addAttribute_('pan_id'//CHAR(0),int_to_str(ielt)//CHAR(0)) !!!correspond à quoi ?
			call wXML_Pop_()
			call wXML_Push_()
				iResult = wXML_addSon_('WEIGHT'//CHAR(0))
				iResult = wXML_addAttribute_('active'//CHAR(0),int_to_str(iweight)//CHAR(0))		
				iResult = wXML_addAttribute_('max_weight'//CHAR(0),double_to_str(wmax,'(e14.7)')//CHAR(0))
			call wXML_Pop_()
			call wXML_Push_()
				iResult = wXML_addSon_('COST'//CHAR(0))
				iResult = wXML_addAttribute_('active'//CHAR(0),int_to_str(iprice)//CHAR(0))				
				iResult = wXML_addAttribute_('max_cost'//CHAR(0),double_to_str(pricemax,'(e14.7)')//CHAR(0))
				iResult = wXML_addAttribute_('cost_model'//CHAR(0),int_to_str(icost)//CHAR(0)) !!!correspond à quoi ?
			call wXML_Pop_()
		call wXML_Pop_()
		
		call wXML_Push_()
			iResult = wXML_addSon_('EQUALITY_CONSTRAINTS'//CHAR(0))
			do ii=1,negalt(iboat)
				call wXML_Push_()
					iResult = wXML_addSon_('CONSTRAINT'//CHAR(0))
					!iResult = wXML_addAttribute_('slave_design_variable'//CHAR(0),int_to_str(mega(ii,1,iboat))//CHAR(0)) elle ne sert à rien meme designe variable que la master 
					iResult = wXML_addAttribute_('slave_panel'//CHAR(0),int_to_str(mega(ii,2,iboat))//CHAR(0))
					iResult = wXML_addAttribute_('master_design_variable'//CHAR(0),int_to_str(mega(ii,3,iboat))//CHAR(0))
					iResult = wXML_addAttribute_('master_panel'//CHAR(0),int_to_str(mega(ii,4,iboat))//CHAR(0))
					iResult = wXML_addAttribute_('ratio'//CHAR(0),double_to_str(ega(ii,iboat),'(f8.4)'//CHAR(0))//CHAR(0))	
				call wXML_Pop_()
			enddo
		call wXML_Pop_()
	call wXML_Pop_()
	

	!!! BOUCLE SUR LES PANNEAUX
	call wXML_Push_()
		iResult = wXML_addSon_('ELEMENTS'//CHAR(0))
		
		!call wXML_Push_()
		!	iResult = wXML_addSon_('PANELS'//CHAR(0))
		!call wXML_Pop_()
		!call wXML_Push_()
		!	iResult = wXML_addSon_('BEAMS'//CHAR(0))
		!call wXML_Pop_()
		!call wXML_Push_()
		!	iResult = wXML_addSon_('SHELLS'//CHAR(0))
		!call wXML_Pop_()
		
		do nel=1,neto
				
		!call wXML_Push_()
			if((panneau(nel).types.ne.'epontille').and.(panneau(nel).types.ne.'EPONTILLE')) then
				if ((panneau(nel).types.eq.'coque').or.(panneau(nel).types.eq.'COQUE')) then
					!iResult = wXML_TraverseToSon_('SHELLS'//CHAR(0))
					call wXML_Push_()
						iResult = wXML_addSon_('SHELL'//CHAR(0))
				else
					!iResult = wXML_TraverseToSon_('PANELS'//CHAR(0))
						call wXML_Push_()
					iResult = wXML_addSon_('PANEL'//CHAR(0))
				endif
	
				iResult = wXML_addAttribute_('id'//CHAR(0),int_to_str(nel)//CHAR(0))
				!iResult = wXML_addAttribute_('type'//CHAR(0),panneau(nel).types//CHAR(0))
			
				if ((panneau(nel).types.eq.'coque').or.(panneau(nel).types.eq.'COQUE')) then ! Cas de la coque
					iResult = wXML_addAttribute_('phi'//CHAR(0),double_to_str(panneau(nel).phil,'(f10.5)'//CHAR(0))//CHAR(0))
					iResult = wXML_addAttribute_('radius'//CHAR(0),double_to_str(panneau(nel).q,'(e14.7)'//CHAR(0))//CHAR(0))
				else
					iResult = wXML_addAttribute_('height'//CHAR(0),double_to_str(panneau(nel).hight,'(f12.5)'//CHAR(0))//CHAR(0))
				endif
				
				iResult = wXML_addAttribute_('angle'//CHAR(0),double_to_str(panneau(nel).angle,'(f12.5)'//CHAR(0))//CHAR(0))		
				iResult = wXML_addAttribute_('participation'//CHAR(0),double_to_str(panneau(nel).part,'(f5.3)'//CHAR(0))//CHAR(0))
				iResult = wXML_addAttribute_('material'//CHAR(0),int_to_str(indMateriau(nel))//CHAR(0))
				iResult = wXML_addAttribute_('family'//CHAR(0),int_to_str(panneau(nel).fami)//CHAR(0))
				iResult = wXML_addAttribute_('lot'//CHAR(0),int_to_str(panneau(nel).lot)//CHAR(0))
				iResult = wXML_addAttribute_('position_code'//CHAR(0),int_to_str(panneau(nel).code)//CHAR(0)) !1 à 9 - voir sous-routine Geom
			
				call wXML_Push_()
					iResult = wXML_addSon_('SCANTLING'//CHAR(0))
					call wXML_Push_()
						iResult = wXML_addSon_('PLATE'//CHAR(0))			
						iResult = wXML_addAttribute_('thickness'//CHAR(0),double_to_str(panneau(nel).delta,'(f8.5)'//CHAR(0))//CHAR(0))
						iResult = wXML_addAttribute_('corrosion'//CHAR(0),double_to_str(panneau(nel).corro(1),'(f8.6)'//CHAR(0))//CHAR(0))
					call wXML_Pop_()
					call wXML_Push_()
						iResult = wXML_addSon_('STIFFENER1'//CHAR(0))						
						iResult = wXML_addAttribute_('web_height'//CHAR(0),double_to_str(panneau(nel).hxr,'(f9.6)'//CHAR(0))//CHAR(0))
						iResult = wXML_addAttribute_('web_thick'//CHAR(0),double_to_str(panneau(nel).dxr,'(f9.6)'//CHAR(0))//CHAR(0))
						iResult = wXML_addAttribute_('flange_width'//CHAR(0),double_to_str(panneau(nel).wxr,'(f9.6)'//CHAR(0))//CHAR(0))
						iResult = wXML_addAttribute_('flange_thick'//CHAR(0),double_to_str(panneau(nel).txr,'(f9.6)'//CHAR(0))//CHAR(0))
						iResult = wXML_addAttribute_('spacing'//CHAR(0),double_to_str(panneau(nel).entr,'(f9.6)'//CHAR(0))//CHAR(0))
						iResult = wXML_addAttribute_('corrosion'//CHAR(0),double_to_str(panneau(nel).corro(3),'(f9.6)'//CHAR(0))//CHAR(0))
						iResult = wXML_addAttribute_('side'//CHAR(0),side_to_str(panneau(nel).ksr)//CHAR(0))
						iResult = wXML_addAttribute_('mode'//CHAR(0),mode(nel)//CHAR(0))
					call wXML_Pop_()
					call wXML_Push_()
						iResult = wXML_addSon_('STIFFENER2'//CHAR(0))					
						iResult = wXML_addAttribute_('web_height'//CHAR(0),double_to_str(panneau(nel).hxr2,'(f9.6)'//CHAR(0))//CHAR(0))
						iResult = wXML_addAttribute_('web_thick'//CHAR(0),double_to_str(panneau(nel).dxr2,'(f9.6)'//CHAR(0))//CHAR(0))
						iResult = wXML_addAttribute_('flange_width'//CHAR(0),double_to_str(panneau(nel).wxr2,'(f9.6)'//CHAR(0))//CHAR(0))
						iResult = wXML_addAttribute_('flange_thick'//CHAR(0),double_to_str(panneau(nel).txr2,'(f9.6)'//CHAR(0))//CHAR(0))
						iResult = wXML_addAttribute_('spacing'//CHAR(0),double_to_str(panneau(nel).entr2,'(f9.6)'//CHAR(0))//CHAR(0))
						iResult = wXML_addAttribute_('side'//CHAR(0),side_to_str(panneau(nel).ksr2)//CHAR(0))
					call wXML_Pop_()
					call wXML_Push_()
						iResult = wXML_addSon_('FRAME1'//CHAR(0))
						iResult = wXML_addAttribute_('web_height'//CHAR(0),double_to_str(panneau(nel).hya,'(f9.6)'//CHAR(0))//CHAR(0))						
						iResult = wXML_addAttribute_('web_thick'//CHAR(0),double_to_str(panneau(nel).dya,'(f9.6)'//CHAR(0))//CHAR(0))
						iResult = wXML_addAttribute_('flange_width'//CHAR(0),double_to_str(panneau(nel).wya,'(f9.6)'//CHAR(0))//CHAR(0))
						iResult = wXML_addAttribute_('flange_thick'//CHAR(0),double_to_str(panneau(nel).tya,'(f9.6)'//CHAR(0))//CHAR(0))
						iResult = wXML_addAttribute_('spacing'//CHAR(0),double_to_str(panneau(nel).epsa,'(f9.6)'//CHAR(0))//CHAR(0))
						iResult = wXML_addAttribute_('corrosion'//CHAR(0),double_to_str(panneau(nel).corro(2),'(f9.6)'//CHAR(0))//CHAR(0))
						iResult = wXML_addAttribute_('side'//CHAR(0),side_to_str(panneau(nel).ksa)//CHAR(0))
					call wXML_Pop_()
					call wXML_Push_()
						iResult = wXML_addSon_('FRAME2'//CHAR(0))	
						iResult = wXML_addAttribute_('web_height'//CHAR(0),double_to_str(panneau(nel).hya2,'(f9.6)'//CHAR(0))//CHAR(0))
						iResult = wXML_addAttribute_('web_thick'//CHAR(0),double_to_str(panneau(nel).dya2,'(f9.6)'//CHAR(0))//CHAR(0))
						iResult = wXML_addAttribute_('flange_width'//CHAR(0),double_to_str(panneau(nel).wya2,'(f9.6)'//CHAR(0))//CHAR(0))
						iResult = wXML_addAttribute_('flange_thick'//CHAR(0),double_to_str(panneau(nel).tya2,'(f9.6)'//CHAR(0))//CHAR(0))
						iResult = wXML_addAttribute_('spacing'//CHAR(0),double_to_str(panneau(nel).epsa2,'(f9.6)'//CHAR(0))//CHAR(0))
						iResult = wXML_addAttribute_('side'//CHAR(0),side_to_str(panneau(nel).ksa2)//CHAR(0))
					call wXML_Pop_()
					
					call wXML_Push_()
						iResult = wXML_addSon_('GIRDERS'//CHAR(0))
						do ii=1,panneau(nel).mt
							call wXML_Push_()
								iResult = wXML_addSon_('GIRDER'//CHAR(0))
								iResult = wXML_addAttribute_('web_height'//CHAR(0),double_to_str(panneau(nel).hxtr(ii),'(f9.6)'//CHAR(0))//CHAR(0))
								iResult = wXML_addAttribute_('web_thick'//CHAR(0),double_to_str(panneau(nel).dxtr(ii),'(f9.6)'//CHAR(0))//CHAR(0))
								iResult = wXML_addAttribute_('flange_width'//CHAR(0),double_to_str(panneau(nel).wxtr(ii),'(f9.6)'//CHAR(0))//CHAR(0))
								iResult = wXML_addAttribute_('flange_thick'//CHAR(0),double_to_str(panneau(nel).txtr(ii),'(f9.6)'//CHAR(0))//CHAR(0))
								iResult = wXML_addAttribute_('position'//CHAR(0),double_to_str(panneau(nel).abtr2(ii),'(f9.6)'//CHAR(0))//CHAR(0))
								iResult = wXML_addAttribute_('side'//CHAR(0),side_to_str(panneau(nel).kst)//CHAR(0))
							call wXML_Pop_()
						enddo
					call wXML_Pop_()
				call wXML_Pop_()
				
				call wXML_Push_()
					iResult = wXML_addSon_('ELMT_LOADS'//CHAR(0))	
					iResult = wXML_addAttribute_('local_pressure'//CHAR(0),double_to_str(panneau(nel).ploc,'(f12.4)'//CHAR(0))//CHAR(0))
					iResult = wXML_addAttribute_('side'//CHAR(0),side_to_str(panneau(nel).kse)//CHAR(0))	
					do is=1,nsolm
						call wXML_Push_()
							iResult = wXML_addSon_('ELMT_LOADCASE'//CHAR(0))
							iResult = wXML_addAttribute_('id'//CHAR(0),int_to_str(is)//CHAR(0))
							iResult = wXML_addAttribute_('xi'//CHAR(0),double_to_str(panneau(nel).xi_full(is),'(f12.4)'//CHAR(0))//CHAR(0))
							iResult = wXML_addAttribute_('xf'//CHAR(0),double_to_str(panneau(nel).xf_full(is),'(f12.4)'//CHAR(0))//CHAR(0))
							do jj=1,npt_full(is,nel)
								call wXML_Push_()
									iResult = wXML_addSon_('STEP'//CHAR(0))
									iResult = wXML_addAttribute_('gravity'//CHAR(0),double_to_str(cha_full(jj,1,is,nel),'(f12.4)'//CHAR(0))//CHAR(0))
									iResult = wXML_addAttribute_('xi'//CHAR(0),double_to_str(cha_full(jj,2,is,nel),'(f10.4)'//CHAR(0))//CHAR(0))
									iResult = wXML_addAttribute_('xf'//CHAR(0),double_to_str(cha_full(jj,3,is,nel),'(f10.4)'//CHAR(0))//CHAR(0))
								call wXML_Pop_()	
							enddo
						call wXML_Pop_()
					enddo	
				call wXML_Pop_()
									
				call wXML_Push_()
					iResult = wXML_addSon_('OPTIMISATION'//CHAR(0))
					call wXML_Push_()
						iResult = wXML_addSon_('DESIGN_VARIABLES'//CHAR(0))
						do ii=1,9
							call wXML_Push_()
								iResult = wXML_addSon_('DESIGN_VARIABLE'//CHAR(0))
								if (ii.eq.1) then
									iResult = wXML_addAttribute_('name'//CHAR(0),'plate_thickness'//CHAR(0))
								elseif (ii.eq.2) then
									iResult = wXML_addAttribute_('name'//CHAR(0),'frame_web_height'//CHAR(0))
								elseif (ii.eq.3) then
									iResult = wXML_addAttribute_('name'//CHAR(0),'frame_web_thickness'//CHAR(0))
								elseif (ii.eq.4) then
									iResult = wXML_addAttribute_('name'//CHAR(0),'frame_flange_width'//CHAR(0))
								elseif (ii.eq.5) then
									iResult = wXML_addAttribute_('name'//CHAR(0),'frame_spacing'//CHAR(0))
								elseif (ii.eq.6) then
									iResult = wXML_addAttribute_('name'//CHAR(0),'stiff_web_height'//CHAR(0))
								elseif (ii.eq.7) then
									iResult = wXML_addAttribute_('name'//CHAR(0),'stiff_web_thickness'//CHAR(0))
								elseif (ii.eq.8) then
									iResult = wXML_addAttribute_('name'//CHAR(0),'stiff_flange_width'//CHAR(0))
								elseif (ii.eq.9) then
									iResult = wXML_addAttribute_('name'//CHAR(0),'stiff_spacing'//CHAR(0))
								endif
								!Test pour voir si la variable est active ou pas
								k=0
								do jj=1,nvar(nel,iboat)
									if (nxit(jj,nel,iboat).eq.ii) k=1
								enddo
								!
								iResult = wXML_addAttribute_('actif'//CHAR(0),boolean_to_str(k)//CHAR(0))
								iResult = wXML_addAttribute_('ximin'//CHAR(0),double_to_str(panneau(nel).dvmin(ii),'(f9.6)'//CHAR(0))//CHAR(0))
								iResult = wXML_addAttribute_('ximax'//CHAR(0),double_to_str(panneau(nel).dvmax(ii),'(f9.6)'//CHAR(0))//CHAR(0))
							call wXML_Pop_()
						enddo
					call wXML_Pop_()

					call wXML_Push_()
						iResult = wXML_addSon_('CONSTRAINTS'//CHAR(0))
						call wXML_Push_()
							iResult = wXML_addSon_('STRUCTURAL'//CHAR(0))
							call wXML_Push_()
								iResult = wXML_addSon_('CALCULATION_POINTS'//CHAR(0))
								do ii=1,panneau(nel).ipts
									call wXML_Push_()
										iResult = wXML_addSon_('POINT'//CHAR(0))
										iResult = wXML_addAttribute_('id'//CHAR(0),int_to_str(ii)//CHAR(0))
										iResult = wXML_addAttribute_('position'//CHAR(0),double_to_str(ypt9(ii,nel),'(e14.7)')//CHAR(0)) !f9.4 avant, ne marche plus avec new compilateur
									call wXML_Pop_()
								enddo
							call wXML_Pop_()
							
							
							call wXML_Push_()
								iResult = wXML_addSon_('STRUCTCONSTRAINTS'//CHAR(0))
								inum=0
								do is=1,nsol			
									do jj=1,panneau(nel).m1(is)
										call wXML_Push_()
											inum=inum+1
											iResult = wXML_addSon_('STRUCTCONSTRAINT'//CHAR(0))	
											iResult = wXML_addAttribute_('id'//CHAR(0),int_to_str(inum)//CHAR(0))
											iResult = wXML_addAttribute_('type'//CHAR(0),int_to_str(panneau(nel).lcont4_init(is,jj,1))//CHAR(0))
											iResult = wXML_addAttribute_('max_value'//CHAR(0),double_to_str(panneau(nel).cjmax9_init(is,jj),'(e14.7)')//CHAR(0))										
											iResult = wXML_addAttribute_('side'//CHAR(0),side_to_str(panneau(nel).inv3_init(is,jj))//CHAR(0))
											iResult = wXML_addAttribute_('position'//CHAR(0),int_to_str(panneau(nel).lcont4_init(is,jj,2))//CHAR(0))
											iResult = wXML_addAttribute_('loadcase'//CHAR(0),int_to_str(is)//CHAR(0))
										call wXML_Pop_()
									enddo
								enddo
							call wXML_Pop_()
							

						call wXML_Pop_()
					
						call wXML_Push_()
							iResult = wXML_addSon_('GEOMETRICAL'//CHAR(0))
							iResult = wXML_addAttribute_('adjust_frame_flange'//CHAR(0),int_to_str(panneau(nel).isema)//CHAR(0))
							iResult = wXML_addAttribute_('adjust_stiff_flange'//CHAR(0),int_to_str(panneau(nel).isemr)//CHAR(0))
							
							call wXML_Push_()
							    iResult = wXML_addSon_('GEOMCONSTRAINTS'//CHAR(0))
							    do ii=1,panneau(nel).m2
								    call wXML_Push_()
									    iResult = wXML_addSon_('GEOMCONSTRAINT'//CHAR(0))
									    iResult = wXML_addAttribute_('id'//CHAR(0),int_to_str(panneau(nel).lm2(ii))//CHAR(0))
								    call wXML_Pop_()
							    enddo
							call wXML_Pop_()
							
						call wXML_Pop_()
					
						call wXML_Push_()
							iResult = wXML_addSon_('SLOSHING'//CHAR(0))
							call wXML_Push_()
							    iResult = wXML_addSon_('SLOSHCONSTRAINTS'//CHAR(0))
							    do ii=1,panneau(nel).nsloshm
								    call wXML_Push_()
									    iResult = wXML_addSon_('SLOSHCONSTRAINT'//CHAR(0))
									    iResult = wXML_addAttribute_('id'//CHAR(0),int_to_str(panneau(nel).nslosh(ii))//CHAR(0))
									    iResult = wXML_addAttribute_('press_slosh'//CHAR(0),double_to_str(panneau(nel).press_slosh,'(f9.3)'//CHAR(0))//CHAR(0))
								    call wXML_Pop_()
							    enddo
							call wXML_Pop_()
							
						call wXML_Pop_()
					call wXML_Pop_()


				call wXML_Pop_() !! On remonte à plate

				call add_connected_panels(nel)

				!call wXML_Pop_() !! On remonte à plates

				call wXML_Pop_() !!! On remonte au noeud ELEMENTS
				!************
			
			elseif((panneau(nel).types.eq.'epontille').or.(panneau(nel).types.eq.'EPONTILLE')) then ! Cas de l'épontille
				!call wXML_Push_()
				!	iResult = wXML_TraverseToSon_('BEAMS'//CHAR(0))
					call wXML_Push_()
				
						iResult = wXML_addSon_('BEAM'//CHAR(0))
						iResult = wXML_addAttribute_('id'//CHAR(0),int_to_str(nel)//CHAR(0))
						iResult = wXML_addAttribute_('section'//CHAR(0),panneau(nel).section//CHAR(0))
						iResult = wXML_addAttribute_('height'//CHAR(0),double_to_str(panneau(nel).hight,'(f10.5)'//CHAR(0))//CHAR(0))
						iResult = wXML_addAttribute_('angle'//CHAR(0),double_to_str(panneau(nel).angle,'(f12.5)'//CHAR(0))//CHAR(0))
						iResult = wXML_addAttribute_('material'//CHAR(0),int_to_str(indMateriau(nel))//CHAR(0))

						call wXML_Push_()
							iResult = wXML_addSon_('BEAMSCANTLING'//CHAR(0))
							iResult = wXML_addAttribute_('spacing'//CHAR(0),double_to_str(panneau(nel).epsa,'(f9.6)'//CHAR(0))//CHAR(0))
							iResult = wXML_addAttribute_('height_without_girder'//CHAR(0),double_to_str(panneau(nel).heff,'(f9.6)'//CHAR(0))//CHAR(0))		
							iResult = wXML_addAttribute_('corrosion'//CHAR(0),double_to_str(panneau(nel).corro(2),'(f8.5)'//CHAR(0))//CHAR(0))
							
							if (INDEX(Ap(panneau(nel).section),'D')) then 
							    iResult = wXML_addAttribute_('web_height'//CHAR(0),double_to_str(2*panneau(nel).hya,'(f9.6)'//CHAR(0))//CHAR(0))						
						        iResult = wXML_addAttribute_('web_thick'//CHAR(0),double_to_str(panneau(nel).dya,'(f9.6)'//CHAR(0))//CHAR(0))
						        iResult = wXML_addAttribute_('flange_width'//CHAR(0),double_to_str(panneau(nel).wya,'(f9.6)'//CHAR(0))//CHAR(0))
						        iResult = wXML_addAttribute_('flange_thick'//CHAR(0),double_to_str(panneau(nel).tya,'(f9.6)'//CHAR(0))//CHAR(0))
							else
							    iResult = wXML_addAttribute_('radius'//CHAR(0),double_to_str(panneau(nel).hya,'(f8.5)'//CHAR(0))//CHAR(0))
							    iResult = wXML_addAttribute_('thickness'//CHAR(0),double_to_str(panneau(nel).epais,'(f8.5)'//CHAR(0))//CHAR(0))
							endif	
					    call wXML_Pop_()		
						call wXML_Push_()
						call add_connected_panels(nel)
					call wXML_Push_()
					iResult = wXML_addSon_('OPTIMISATION'//CHAR(0))
					call wXML_Push_()
						iResult = wXML_addSon_('DESIGN_VARIABLES'//CHAR(0))
							call wXML_Push_()
								iResult = wXML_addSon_('DESIGN_VARIABLE'//CHAR(0))
                                iResult = wXML_addAttribute_('name'//CHAR(0),'beam_spacing'//CHAR(0))
								!Test pour voir si la variable est active ou pas
								k=0
								if ((nvar(nel,iboat).eq.1) .AND. (nxit(1,nel,iboat).eq. 5)) k=1
								iResult = wXML_addAttribute_('actif'//CHAR(0),boolean_to_str(k)//CHAR(0))
								iResult = wXML_addAttribute_('ximin'//CHAR(0),double_to_str(panneau(nel).dvmin(5),'(f9.6)'//CHAR(0))//CHAR(0))
								iResult = wXML_addAttribute_('ximax'//CHAR(0),double_to_str(panneau(nel).dvmax(5),'(f9.6)'//CHAR(0))//CHAR(0))
							call wXML_Pop_()
						
					call wXML_Pop_()
				   	call wXML_Pop_()
				   	call wXML_Pop_()
				   	call wXML_Pop_()
			endif
			
	   
		
		
		enddo ! fin de la boucle sur les panneaux
        call wXML_Pop_()
		call wXML_Push_()
			iResult = wXML_addSon_('VIBRATION_ELEMENTS'//CHAR(0))
			do nel=1,neto
				if (panneau(nel).elmt_vib.ne.0) then ! Ajouter/compléter l'élément de vibration
					call update_vibration_element(nel)
				endif
			enddo
		

	call wXML_Pop_()

call wXML_Pop_()

num=char(96+itera)
iResult = wXML_Reader_SaveXML_('up-'//num//'-'//section_file(1:len_trim(section_file)-4)//'.xml'//CHAR(0), 999)
!iResult = wXML_Reader_Terminate_()
	

iResult = wXML_Reader_Terminate_()
!nullify (q1)
!deallocate(q1)

!	Unload the dll
status = freelibrary(p)
!	Check if the dll was unloaded (T for true)
!type *, "freelibrary status was: ", status


return
end
!******************************************************
character*30 function boolean_to_str(a)
! Sous-routine qui convertir un booleen a en string b
integer*4 a
character*5 aa

aa = 'FALSE'
if (a .eq. 1.) then 
aa = 'TRUE'
endif

boolean_to_str=''
write (boolean_to_str,*) aa
boolean_to_str = boolean_to_str//CHAR(0)

return
end function
!******************************************************
character*30 function side_to_str(a)
! Sous-routine qui convertir un entier a en string b
integer*4 a
character*5 aa

aa='NONE'
if (a .eq. 1) then 
aa='LEFT'
elseif (a .eq. 2) then
aa='RIGHT'
endif

side_to_str=''
write (side_to_str,*) aa
side_to_str = side_to_str//CHAR(0)

return
end function

!******************************************************
character*30 function int_to_str(a)
! Sous-routine qui convertir un entier a en string b
integer*4 a

int_to_str=''
write (int_to_str,*) a
int_to_str = int_to_str//CHAR(0)

return
end function

!******************************************************
character*30 function double_to_str(a,form)

character*7 form
double precision a

double_to_str=''
write (double_to_str,form) a
double_to_str = double_to_str//CHAR(0)

RETURN
END FUNCTION


!********************************************
subroutine add_connected_panels(nel)
use param_XML
use param_section, ONLY : panneau

implicit double precision (a-h,o-z)
integer*4 ii,nel,iResult
character*30 int_to_str

call wXML_Push_()
	iResult = wXML_addSon_('CONNECTED_ELEMENTS'//CHAR(0))
	do ii=1,10
		if (panneau(nel).noh9(ii).ne.0) then
			call wXML_Push_()
			iResult = wXML_addSon_('CONNECTED_ELEMENT'//CHAR(0))
			iResult = wXML_addAttribute_('id'//CHAR(0),int_to_str(panneau(nel).noh9(ii))//CHAR(0))
			call wXML_Pop_()
		endif
	enddo
call wXML_Pop_()

return
end
!********************************************
subroutine update_vibration_element(nel)
use param_XML
use param_section, ONLY : panneau

implicit double precision (a-h,o-z)
integer*4 nel,iResult,found
character*30 int_to_str

call wXML_Push_()
	found=0
	!iResult = wXML_Values_(text,taille)
	iResult = wXML_TraverseToFirstSon_()
	if (iResult.eq.0) then ! Alors ajout du nouvel élément de vibration
		!call wXML_Pop_() ! pour revenir à Vibrations_elements
		found=0
	else ! Alors il existe déjà un élément_vibration -> tester si le bon existe (sinon rajout d'un nouveau)
		!iResult = wXML_Values_(text,taille)
		iResult = wXML_getAttributeDouble_('id'//CHAR(0))
		if (iResult.eq.panneau(nel).elmt_vib) then
			found=1
			call wXML_Push_()
				iResult = wXML_addSon_('PANEL'//CHAR(0))
				iResult = wXML_addAttribute_('id'//CHAR(0),int_to_str(nel)//CHAR(0))
			call wXML_Pop_()
		endif
		
		iResult=1
		do while (iResult.eq.1)
			iResult = wXML_TraverseToNextSibling_()
			if (iResult.eq.1) then
				iResult = wXML_getAttributeDouble_('id'//CHAR(0))
				if (iResult.eq.panneau(nel).elmt_vib) then
					found=1
					call wXML_Push_()
						iResult = wXML_addSon_('PANEL'//CHAR(0))
						iResult = wXML_addAttribute_('id'//CHAR(0),int_to_str(nel)//CHAR(0))
					call wXML_Pop_()
				endif
				iResult=1
			endif
		enddo
	endif
call wXML_Pop_()

if (found.eq.0) then ! Alors ajout du nouvel élément de vibration
	call add_vibration_element(nel)	
endif

return
end

!********************************************
subroutine add_vibration_element(nel)
use param_XML
use param_section, ONLY : panneau

implicit double precision (a-h,o-z)
integer*4 nel,iResult
character*30 double_to_str,int_to_str

call wXML_Push_()
	iResult = wXML_addSon_('VIBRATION_ELEMENT'//CHAR(0))
	
	iResult = wXML_addAttribute_('id'//CHAR(0),int_to_str(panneau(nel).elmt_vib)//CHAR(0))
	iResult = wXML_addAttribute_('length'//CHAR(0),double_to_str(panneau(nel).long_vib,'(f12.7)'//CHAR(0))//CHAR(0))
	iResult = wXML_addAttribute_('front_boundary_condition'//CHAR(0),int_to_str(panneau(nel).cl_av_vib)//CHAR(0))
	iResult = wXML_addAttribute_('back_boundary_condition'//CHAR(0),int_to_str(panneau(nel).cl_ar_vib)//CHAR(0))
	call wXML_Push_()
		iResult = wXML_addSon_('PANEL'//CHAR(0))
		iResult = wXML_addAttribute_('id'//CHAR(0),int_to_str(nel)//CHAR(0))
	call wXML_Pop_()
call wXML_Pop_()

return
end

!*********************************
 
Character (len=20) Function Ap (string)
Character(len=*) string
Ap = &
transfer(merge(achar(iachar(transfer(string,"x",len(string)))- &
(ichar('a')-ichar('A')) ), &
transfer(string,"x",len(string)) , &
transfer(string,"x",len(string)) >= "a" .and. &
transfer(string,"x",len(string)) <= "z"), repeat("x", len(string)))
return
end function Ap