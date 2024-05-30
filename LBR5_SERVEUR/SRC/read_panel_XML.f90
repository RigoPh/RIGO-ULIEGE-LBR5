subroutine read_panel_XML()
!------------------------------------------------------------------------------
!							Lecture fichier de données sous forme XML
!
!	Cette sous-routine le fichier de données quand il est écrit sous forme XML
!	
!	Date de création : Mai 2010
!	Auteur : F.Bair
!
!------------------------------------------------------------------------------

use param_section
use dfwin
use param_XML


implicit double precision (a-h,o-z)
integer*4 taille,t_l,count_Elmt,nbr_panel,elmt_vib,cl_av_vib,cl_ar_vib
character(LEN=5) :: elemnt_typ
character*30 Up
character*30 str_to_boolean
double precision long_vib
character*5 k
character*(5) buffer
character*(50) bufferwax
character*1 num
character*1 boubou
call init_XML()

					
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! Lecture du fichier XML !!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!iResult = test_lecture(wXML_Reader_Init_("Fichier_up"))
iResult = wXML_Reader_Init_("Fichier_up")
num='a'
!num=char(96+itera)
iResult = 0;!wXML_Reader_LoadXML_("up-"//num//"-"//section_file(1:len_trim(section_file)-4//".xml"), 999)
!

!iResult = wXML_TraverseToFirstSon_()
!iResult = wXML_Values_(text,taille)


iResult = t_l(wXML_getAttributeString_("version",header1,taille))

call wXML_Push_()
	iResult = t_l(wXML_TraverseToSon_("PROJECT"))
	iResult = t_l(wXML_getAttributeString_("title",header2,taille))
	
	call wXML_Push_()
		iResult = t_l(wXML_TraverseToSon_("COMPUTE_OPTIONS"))
		jlph2 = INT4(wXML_getAttributeDouble_("fourier_terms"))!int4=convert to integer*4
		ipoids = INT4(wXML_getAttributeDouble_("deadweight_active"))
		
		call wXML_Push_()
			iResult = wXML_TraverseToSon_("OUTPUTS")	
			impr = INT4(wXML_getAttributeDouble_("impression_option_1"))	
			impr2 = INT4(wXML_getAttributeDouble_("impression_option_2"))
		call wXML_Pop_()
		
		call wXML_Push_()
			iResult = wXML_TraverseToSon_("SECTION")
			dis(1) = wXML_getAttributeDouble_("first_section_position")
			dis(2) = wXML_getAttributeDouble_("second_section_position")
			dis(3) = wXML_getAttributeDouble_("third_section_position")
			dis(4) = wXML_getAttributeDouble_("fourth_section_position")
			dis(5) = wXML_getAttributeDouble_("fifth_section_position")
		call wXML_Pop_()

		call wXML_Push_()
			iResult = wXML_TraverseToSon_("OPTIMISATION_GLOBAL")
			imulti = INT4(wXML_getAttributeDouble_("i_multi"))
			iteram = INT4(wXML_getAttributeDouble_("iteration_number"))
			iopti = INT4(wXML_getAttributeDouble_("i_opti"))
			icout = INT4(wXML_getAttributeDouble_("i_cost"))
			iredress = INT4(wXML_getAttributeDouble_("i_straightening"))
			call wXML_Push_()
				iResult = wXML_TraverseToSon_("MULTI_OBJECTIVE")
				rho = INT4(wXML_getAttributeDouble_("rho"))
				w1 = wXML_getAttributeDouble_("weight_cost")
				w2 = wXML_getAttributeDouble_("weight_weight")
				w3 = wXML_getAttributeDouble_("weight_inertia")
				fk(1) = wXML_getAttributeDouble_("optimal_solution_cost")
				fk(2) = wXML_getAttributeDouble_("optimal_solution_weight")
				fk(3) = wXML_getAttributeDouble_("optimal_solution_inertia")
			call wXML_Pop_()			
		call wXML_Pop_()

		call wXML_Push_()
			iResult = wXML_TraverseToSon_("COST_OPTIONS")	
			rend_global = wXML_getAttributeDouble_("rend_global")
			!epq = wXML_getAttributeDouble_("epq_global"))
			refthick_input = INT4(wXML_getAttributeDouble_("cost_DCNS")) !(0 = LBR5=NORMAL_PARAM, 1 = DCNS=DCNS_PARAM)				
			call wXML_Push_()
				iResult = wXML_TraverseToSon_("LIFE_CYCLE_COST")		
				lccscenario = INT4(wXML_getAttributeDouble_("scenario"))	!(1:dwt cst, 2:displ cst)
				lcc2 = INT4(wXML_getAttributeDouble_("module_2"))	!(non, 1=oui)
				lcc3 = INT4(wXML_getAttributeDouble_("module_3"))
				lcc4 = INT4(wXML_getAttributeDouble_("module_4"))
				lcc5 = INT4(wXML_getAttributeDouble_("module_5"))
				lightweight = wXML_getAttributeDouble_("lightweight")		
				deadweight = wXML_getAttributeDouble_("deadweight")
				poidsLBR5_init = wXML_getAttributeDouble_("initial_weight")
			call wXML_Pop_()

		call wXML_Pop_()

	call wXML_Pop_()
	
	!!! DEFINITION DU MODELE !!!

	iResult = wXML_TraverseToSon_("MODEL")
	width = wXML_getAttributeDouble_("length")
	length = wXML_getAttributeDouble_("reglementary_length")
	
	call wXML_Push_()
		iResult = wXML_TraverseToSon_("MATERIALS")
		iResult = wXML_TraverseToSon_("MATERIAL")

		if (iResult.eq.1) then
			nbrMat = count_Elmt()
			do ind=1,nbrMat
			!call wXML_Push_()
				! TODO : LIRE CE NOM iResult = wXML_getAttributeDouble_("name"),'MATERIAL '//int_to_str(ind)//CHAR(0))
				call wXML_Push_()
					iResult = wXML_TraverseToSon_("PROPERTIES")			
					e(ind) = wXML_getAttributeDouble_("young_modulus")
					eta(ind) = wXML_getAttributeDouble_("poisson_coefficient")
					sigy(ind) = wXML_getAttributeDouble_("yield_stress")
					sigyadm(ind) = wXML_getAttributeDouble_("allowable_stress")
					coefk(ind) = wXML_getAttributeDouble_("security_coefficient")
					spec(ind) = wXML_getAttributeDouble_("specific_weight")
				call wXML_Pop_()
				call wXML_Push_()
					iResult = wXML_TraverseToSon_("MATERIAL_COST")
					call wXML_Push_()
						iResult = wXML_TraverseToSon_("NORMAL_PARAM")			
						dref(ind) = wXML_getAttributeDouble_("eo")
						drefx(ind) = wXML_getAttributeDouble_("eox")			
						drefy(ind) = wXML_getAttributeDouble_("eoy")
						c1(ind) = wXML_getAttributeDouble_("c1")
						c2(ind) = wXML_getAttributeDouble_("c2")
						c3(ind) = wXML_getAttributeDouble_("c3")
						dc1(ind) = wXML_getAttributeDouble_("dc1")
						dw2(ind) = wXML_getAttributeDouble_("dw2")
						dw3(ind) = wXML_getAttributeDouble_("dw3")
					call wXML_Pop_()
					call wXML_Push_()
						iResult = wXML_TraverseToSon_("DCNS_PARAM")			
						dref_b(ind) = wXML_getAttributeDouble_("dref_b")
						dref_c(ind) = wXML_getAttributeDouble_("dref_c")
						dref_l(ind) = wXML_getAttributeDouble_("dref_l")
						c_pb(ind) = wXML_getAttributeDouble_("c_pb")
						c_tc(ind) = wXML_getAttributeDouble_("c_tc")
						c_tl(ind) = wXML_getAttributeDouble_("c_tl")
						dw_b(ind) = wXML_getAttributeDouble_("dw_b")
						dw_c(ind) = wXML_getAttributeDouble_("dw_c")
						dw_l(ind) = wXML_getAttributeDouble_("dw_l")
					call wXML_Pop_()
				call wXML_Pop_()
				call wXML_Push_()
					iResult = wXML_TraverseToSon_("LABOUR_COST")
					labour_cost(ind) = wXML_getAttributeDouble_("labour_cost")
					p10(ind) = wXML_getAttributeDouble_("p10")
					dp10(ind) = wXML_getAttributeDouble_("dp10")
					p4(ind) = wXML_getAttributeDouble_("p4")
					p5(ind) = wXML_getAttributeDouble_("p5")
					dp4(ind) = wXML_getAttributeDouble_("dp4")
					dp5(ind) = wXML_getAttributeDouble_("dp5")
					p9x(ind) = wXML_getAttributeDouble_("p9x")
					p9y(ind) = wXML_getAttributeDouble_("p9y")
					dp9x(ind) = wXML_getAttributeDouble_("dp9x")
					dp9y(ind) = wXML_getAttributeDouble_("dp9y")
					p6(ind) = wXML_getAttributeDouble_("p6")
					p7(ind) = wXML_getAttributeDouble_("p7")
					ber(ind) = wXML_getAttributeDouble_("betax")
					bet(ind) = wXML_getAttributeDouble_("betay")
				call wXML_Pop_()
				call wXML_Push_()
					iResult = wXML_TraverseToSon_("CONSUMABLE_COST")
					c8(ind) = wXML_getAttributeDouble_("c8")
					dc8(ind) = wXML_getAttributeDouble_("dc8")
					ialr(ind) = INT4(wXML_getAttributeDouble_("alphax"))
					ialt(ind) = INT4(wXML_getAttributeDouble_("alphay"))
				call wXML_Pop_()
				iResult = wXML_TraverseToNextSibling_() !on passe au MATERIAL suivant
			!call wXML_Pop_()
			enddo
		endif
	call wXML_Pop_()

	call wXML_Push_()
		iResult = wXML_TraverseToSon_("LOADCASES")
		iResult = wXML_TraverseToSon_("LOADCASE")

		if (iResult.eq.1) then
			nsolm = count_Elmt()
			jj=1
			do is=1,nsolm
			!call wXML_Push_()
				iResult = wXML_getAttributeString_(("name"),casename(is),taille)
				! Test si le load case est actif
				if (INT4(wXML_getAttributeDouble_("actif")).eq.1) then
					nnsol(jj) = is
					jj=jj+1
				endif
				
				!call wXML_Push_()
				!	iResult = wXML_TraverseToSon_("MOMENTS"))
					yred = wXML_getAttributeDouble_("reduction_coeff")
					!Vertical Bending Moment
					bm11(is) = wXML_getAttributeDouble_("vertical_bending_moment")
					!Vertical Shear Force
					sf11(is) = wXML_getAttributeDouble_("vertical_shear_force")
					!Horizontal Bending Moment
					bm31(is) = wXML_getAttributeDouble_("horizontal_bending_moment")
					!Horizontal Shear Force
					sf31(is) = wXML_getAttributeDouble_("horizontal_shear_force")
				!call wXML_Pop_()
				
				iResult = wXML_TraverseToNextSibling_()
			!call wXML_Pop_()			
			enddo
		endif
	call wXML_Pop_()

	call wXML_Push_()
		iResult = wXML_TraverseToSon_("BOUNDARY_CONDITIONS")
		iResult = wXML_TraverseToSon_("CONDITION")
	
		if (iResult.eq.1) then
			ncondi = count_Elmt()	
			do ii=1,ncondi
			!call wXML_Push_()
				
				nno9(ii,1) = INT4(wXML_getAttributeDouble_("condition_id"))
				nno9(ii,2) = INT4(wXML_getAttributeDouble_("panel_id"))
				iResult = wXML_TraverseToNextSibling_()
			!call wXML_Pop_()
			enddo
		endif
	call wXML_Pop_()

	call wXML_Push_()
		iResult = wXML_TraverseToSon_("USER_COORDINATE")	
		xk9 = wXML_getAttributeDouble_("xk")
		yk9 = wXML_getAttributeDouble_("yk")
	call wXML_Pop_()

	call wXML_Push_()
		iResult = wXML_TraverseToSon_("OPTIMISATION")	
		call wXML_Push_()
			iResult = wXML_TraverseToSon_("GLOBAL_CONSTRAINTS")
			call wXML_Push_()
				iResult = wXML_TraverseToSon_("GRAVITY")
				igrav = INT4(wXML_getAttributeDouble_("igrav"))				
				xkgmin = wXML_getAttributeDouble_("xkg_min")
				xkgmax = wXML_getAttributeDouble_("xkg_max")
			call wXML_Pop_()
			call wXML_Push_()
				iResult = wXML_TraverseToSon_("INERTIA")
				inert = INT4(wXML_getAttributeDouble_("active"))
				imin = wXML_getAttributeDouble_("min_inertia")
			call wXML_Pop_()
			call wXML_Push_()
				iResult = wXML_TraverseToSon_("SECTION_MODULUS")
				imod = INT4(wXML_getAttributeDouble_("active"))	
				modmin = wXML_getAttributeDouble_("min_section_modulus")
				ielt = INT4(wXML_getAttributeDouble_("pan_id")) !!!correspond à quoi ?
			call wXML_Pop_()
			call wXML_Push_()
				iResult = wXML_TraverseToSon_("WEIGHT")
				iweight = INT4(wXML_getAttributeDouble_("active"))
				wmax = wXML_getAttributeDouble_("max_weight")
			call wXML_Pop_()
			call wXML_Push_()
				iResult = wXML_TraverseToSon_("COST")
				iprice = INT4(wXML_getAttributeDouble_("active"))				
				pricemax = wXML_getAttributeDouble_("max_cost")
				icost = INT4(wXML_getAttributeDouble_("cost_model")) !!!correspond à quoi ?
			call wXML_Pop_()
		call wXML_Pop_()
		
		call wXML_Push_()
			iResult = wXML_TraverseToSon_("EQUALITY_CONSTRAINTS")
			iResult = wXML_TraverseToSon_("CONSTRAINT")

			if (iResult.eq.1) then
				negalt(iboat) = count_Elmt()
				do ii=1,negalt(iboat)
				!call wXML_Push_()
					mega(ii,2,iboat) = INT4(wXML_getAttributeDouble_("slave_panel"))
					mega(ii,3,iboat) = INT4(wXML_getAttributeDouble_("master_design_variable"))
					mega(ii,4,iboat) = INT4(wXML_getAttributeDouble_("master_panel"))
					ega(ii,iboat) = wXML_getAttributeDouble_("ratio")
					iResult = wXML_TraverseToNextSibling_()
				!call wXML_Pop_()
				enddo
			endif
		call wXML_Pop_()
	call wXML_Pop_()
	

	!!! BOUCLE SUR LES PANNEAUX
	call wXML_Push_()
		iResult = wXML_TraverseToSon_("ELEMENTS")
		neto        = 0
		nbr_shell   = 0
		nbr_panel   = 0
		nbr_beam    = 0
		nbr_element = 0
		nel         = 0
		call wXML_Push_()
		  	iResultat = wXML_TraverseToFirstSon_()
			
			do 			
			iResult = wXML_Values_(elemnt_typ, iResult)
			
            
		    
			if ((INDEX(elemnt_typ,'P') .eq.1.) .OR. (INDEX(elemnt_typ,'S')) .eq.1.) then
				    nbr_panel=nbr_panel+1
					nel = INT4(wXML_getAttributeDouble_("id"))				
					panneau(nel).types ='panel' !!! pas oublier
					panneau(nel).hight = wXML_getAttributeDouble_("height")
	
					call lecture_PAN_or_SHELL(nel)
					
			else
			        nbr_beam=nbr_beam+1
					nel = INT4(wXML_getAttributeDouble_("id"))			
					iResult = wXML_getAttributeString_("section",panneau(nel).section,taille)
					!iResult = wXML_getAttributeString_("section"),boubou,taille)
					
					!iResult = wXML_Values_(bufferwax, taille)
					
					!panneau(nel).section= wXML_getAttributeString_("section"),panneau(nel).section,taille)
					panneau(nel).hight = wXML_getAttributeDouble_("height")
					panneau(nel).angle = wXML_getAttributeDouble_("angle")
					indMateriau(nel) = wXML_getAttributeDouble_("material")

					call wXML_Push_()
						iResult = wXML_TraverseToSon_("BEAMSCANTLING")
						
						
						panneau(nel).epsa = wXML_getAttributeDouble_("spacing")
						panneau(nel).heff = wXML_getAttributeDouble_("height_without_girder")
						panneau(nel).corro(2)= wXML_getAttributeDouble_("corrosion")
						   if (INDEX(panneau(nel).section,'D').eq.1) then 
						        panneau(nel).hya= wXML_getAttributeDouble_("web_height")
							    panneau(nel).dya= wXML_getAttributeDouble_("web_thick")	
							    panneau(nel).wya= wXML_getAttributeDouble_("flange_width")	
							    panneau(nel).tya= wXML_getAttributeDouble_("flange_thick")
						    else
						       panneau(nel).hya = wXML_getAttributeDouble_("radius")	
						       panneau(nel).hya = wXML_getAttributeDouble_("thickness")	
							endif
							
					call wXML_Pop_()
						
					call read_connected_panels(nel)
					
					call wXML_Push_()
					iResult = wXML_TraverseToSon_("OPTIMISATION")
					call wXML_Push_()
						iResult = wXML_TraverseToSon_("DESIGN_VARIABLES")
							call wXML_Push_()
								iResult = wXML_TraverseToSon_("DESIGN_VARIABLE")
                                !iResult = wXML_addAttribute_("name"),'beam_spacing"))
                                iResult = wXML_getAttributeString_("actif", buffer, taille)
			                	if (INDEX(buffer,'T')) then
					            nxit(1,nel,iboat) = 5.
					            nvar(nel,iboat) = 1.
					            endif
			                	panneau(nel).dvmin(5) = wXML_getAttributeDouble_("ximin")
				                panneau(nel).dvmax(5) = wXML_getAttributeDouble_("ximax")
				                
							call wXML_Pop_()
						
					call wXML_Pop_()
				   	call wXML_Pop_()

			endif	
		nbr_element=nbr_element+1
		if (wXML_TraverseToNextSibling_().eq.0) exit
		enddo
		
		call wXML_Pop_()

		call wXML_Push_()
			iResult = wXML_TraverseToSon_("VIBRATION_ELEMENTS")
			iResult = wXML_TraverseToSon_("VIBRATION_ELEMENT")

			if (iResult.eq.1) then
				nbr_panel = count_Elmt() ! = nombre de panneaux de vibration
				do ii=1,nbr_panel
					call wXML_Push_()
						elmt_vib = INT4(wXML_getAttributeDouble_("id"))
						long_vib = wXML_getAttributeDouble_("length")
						cl_av_vib = INT4(wXML_getAttributeDouble_("front_boundary_condition"))
						cl_ar_vib = INT4(wXML_getAttributeDouble_("back_boundary_condition"))
	
						iResult = wXML_TraverseToSon_("PANEL") ! on voit bien que c'est mal structuré ! :-|
						do while (iResult.eq.1)
							nel = INT4(wXML_getAttributeDouble_("id"))
							
							panneau(nel).elmt_vib = elmt_vib
							panneau(nel).long_vib = long_vib
							panneau(nel).cl_av_vib = cl_av_vib
							panneau(nel).cl_ar_vib = cl_ar_vib
						
							iResult = wXML_TraverseToNextSibling_()
						enddo
					call wXML_Pop_()
					iResult = wXML_TraverseToNextSibling_()
				enddo

			endif

			do nel=1,neto
				if (panneau(nel).elmt_vib.ne.0) then ! Ajouter/compléter l'élément de vibration
					call update_vibration_element(nel)
				endif
			enddo
		call wXML_Pop_()

	!call wXML_Pop_()

write (*,*) 'Je quitte le truc'
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
character*30 function str_to_boolean(aa)
! Sous-routine qui convertir un booleen a en string b
integer*4 a
character*5 aa

if (INDEX(aa,'F') .eq. 1) then  
a=0
else 
a = 1
endif

boolean_to_str=''
write (str_to_boolean,*) a
str_to_boolean = str_to_boolean//CHAR(0)

return
end function
!**********************************************
!Compte le nombre de fils du noeud sélectionné
!**********************************************
integer*4 function count_Elmt()

use param_XML
integer*4 iResult

count_Elmt=0
iResult=1
call wXML_Push_()

do while (iResult.eq.1)
	count_Elmt=count_Elmt+1
	iResult = wXML_TraverseToNextSibling_()
enddo

call wXML_Pop_()

return
end function


!*********************************************************
!Test lecture XML (si problème renvoit un message d'erreur
!*********************************************************
integer*4 function t_l(iResult)

use param_XML

integer*4 iResult

if (iResult.eq.0) then
	write(*,*) 'Error in lecture of XML file!'
	read(*,*)
	stop
endif
t_l = iResult

return
end function


!******************************************************
!!!character*(*) function get_string(a)
! Sous-routine qui convertir un entier a en string b
!!!use param_XML

!!!character*30 a
!!!integer*4 iResult,taille,t_l

!!!iResult = t_l(wXML_getAttributeString_(a//CHAR(0),get_string,taille))

!!!get_string = get_string(1:taille)//CHAR(0) !même pas nécessaire je pense

!!!return
!!!end function

!******************************************************
subroutine lecture_PAN_or_SHELL(nel)
! Cette sous-routine lit tous les attributs communs des panneaux et des coques
use param_XML
use param_section, IS_=>IS

integer*4 nel,iResult,ii,jj,is,taille,count_Elmt

!nel = INT4(wXML_getAttributeDouble_("id")))
panneau(nel).angle = wXML_getAttributeDouble_("angle")
panneau(nel).part = wXML_getAttributeDouble_("participation")
indMateriau(nel) = INT4(wXML_getAttributeDouble_("material"))
panneau(nel).fami = INT4(wXML_getAttributeDouble_("family"))
panneau(nel).lot = INT4(wXML_getAttributeDouble_("lot"))
panneau(nel).code = INT4(wXML_getAttributeDouble_("position_code")) !1 à 9 - voir sous-routine Geom
			
call wXML_Push_()
	iResult = wXML_TraverseToSon_("SCANTLING")
	call wXML_Push_()
		iResult = wXML_TraverseToSon_("PLATE")	
		panneau(nel).delta = wXML_getAttributeDouble_("thickness")
		panneau(nel).corro(1) = wXML_getAttributeDouble_("corrosion")
	call wXML_Pop_()
	call wXML_Push_()
		iResult = wXML_TraverseToSon_("STIFFENER1")				
		panneau(nel).hxr = wXML_getAttributeDouble_("web_height")
		panneau(nel).dxr = wXML_getAttributeDouble_("web_thick")
		panneau(nel).wxr = wXML_getAttributeDouble_("flange_width")
		panneau(nel).txr = wXML_getAttributeDouble_("flange_thick")
		panneau(nel).entr = wXML_getAttributeDouble_("spacing")
		panneau(nel).corro(3) = wXML_getAttributeDouble_("corrosion")
		panneau(nel).ksr = INT4(wXML_getAttributeDouble_("side"))
		iResult = wXML_getAttributeString_("mode",mode(nel),taille)
	call wXML_Pop_()
	call wXML_Push_()
		iResult = wXML_TraverseToSon_("STIFFENER2")			
		panneau(nel).hxr2 = wXML_getAttributeDouble_("web_height")
		panneau(nel).dxr2 = wXML_getAttributeDouble_("web_thick")
		panneau(nel).wxr2 = wXML_getAttributeDouble_("flange_width")
		panneau(nel).txr2 = wXML_getAttributeDouble_("flange_thick")
		panneau(nel).entr2 = wXML_getAttributeDouble_("spacing")
		panneau(nel).ksr2 = INT4(wXML_getAttributeDouble_("side"))
	call wXML_Pop_()
	call wXML_Push_()
		iResult = wXML_TraverseToSon_("FRAME1")
		panneau(nel).hya = wXML_getAttributeDouble_("web_height")
		panneau(nel).dya = wXML_getAttributeDouble_("web_thick")
		panneau(nel).wya = wXML_getAttributeDouble_("flange_width")
		panneau(nel).tya = wXML_getAttributeDouble_("flange_thick")
		panneau(nel).epsa = wXML_getAttributeDouble_("spacing")
		panneau(nel).corro(2) = wXML_getAttributeDouble_("corrosion")
		panneau(nel).ksa= INT4(wXML_getAttributeDouble_("side"))
	call wXML_Pop_()
	call wXML_Push_()
		iResult = wXML_TraverseToSon_("FRAME2")
		panneau(nel).hya2 = wXML_getAttributeDouble_("web_height")
		panneau(nel).dya2 = wXML_getAttributeDouble_("web_thick")
		panneau(nel).wya2 = wXML_getAttributeDouble_("flange_width")
		panneau(nel).tya2 = wXML_getAttributeDouble_("flange_thick")
		panneau(nel).epsa2 = wXML_getAttributeDouble_("spacing")
		panneau(nel).ksa2 = INT4(wXML_getAttributeDouble_("side"))
	call wXML_Pop_()
					
	call wXML_Push_()
		iResult = wXML_TraverseToSon_("GIRDERS")
		
		iResult = wXML_TraverseToSon_("GIRDER")
		if (iResult.eq.1) then
			panneau(nel).mt = count_Elmt()
			do ii=1,panneau(nel).mt
				panneau(nel).hxtr(ii) = wXML_getAttributeDouble_("web_height")
				panneau(nel).dxtr(ii) = wXML_getAttributeDouble_("web_thick")
				panneau(nel).wxtr(ii) = wXML_getAttributeDouble_("flange_width")
				panneau(nel).txtr(ii) = wXML_getAttributeDouble_("flange_thick")
				panneau(nel).abtr2(ii) = wXML_getAttributeDouble_("position")
				panneau(nel).kst = INT4(wXML_getAttributeDouble_("side"))
				iResult = wXML_TraverseToNextSibling_()
			enddo
		endif
	call wXML_Pop_()
call wXML_Pop_()
				
call wXML_Push_()
	iResult = wXML_TraverseToSon_("ELMT_LOADS")
	panneau(nel).ploc = wXML_getAttributeDouble_("local_pressure")
	panneau(nel).kse = INT4(wXML_getAttributeDouble_("side"))
	
	iResult = wXML_TraverseToSon_("ELMT_LOADCASE")
	if (iResult.eq.1) then
		nsolm = count_Elmt()
		do is=1,nsolm
			panneau(nel).xi_full(is) = wXML_getAttributeDouble_("xi")
			panneau(nel).xf_full(is) = wXML_getAttributeDouble_("xf")
			
			iResult = wXML_TraverseToSon_("STEP")
			if (iResult.eq.1) then
				npt_full(is,nel) = count_Elmt()
				do jj=1,npt_full(is,nel)
					cha_full(jj,1,is,nel) = wXML_getAttributeDouble_("gravity")
					cha_full(jj,2,is,nel) = wXML_getAttributeDouble_("xi")
					cha_full(jj,3,is,nel) = wXML_getAttributeDouble_("xf")
					iResult = wXML_TraverseToNextSibling_()
				enddo
			endif
			iResult = wXML_TraverseToNextSibling_()
		enddo	
	endif
call wXML_Pop_()
							
call wXML_Push_()
	iResult = wXML_TraverseToSon_("OPTIMISATION")
	call wXML_Push_()
		iResult = wXML_TraverseToSon_("DESIGN_VARIABLES")
		iResult = wXML_TraverseToSon_("DESIGN_VARIABLE")
		
		jj=1
		do ii=1,9
			!call wXML_Push_()

				! Test si la variable est active ou pas
				if (INT4(wXML_getAttributeDouble_("actif")).eq.1) then
					nxit(jj,nel,iboat) = ii
					nvar(nel,iboat) = nvar(nel,iboat) + 1 !!! pas oublier !!!
					jj=jj+1
				endif

				panneau(nel).dvmin(ii) = wXML_getAttributeDouble_("ximin")
				panneau(nel).dvmax(ii) = wXML_getAttributeDouble_("ximax")
				iResult = wXML_TraverseToNextSibling_() 
			!call wXML_Pop_()
		enddo
	call wXML_Pop_()

	call wXML_Push_()
		iResult = wXML_TraverseToSon_("CONSTRAINTS")
		call wXML_Push_()
			iResult = wXML_TraverseToSon_("STRUCTURAL")
			call wXML_Push_()
				iResult = wXML_TraverseToSon_("CALCULATION_POINTS")
				iResult = wXML_TraverseToSon_("POINT")
				
				panneau(nel).ipts = count_Elmt()
				do ii=1,panneau(nel).ipts
					!call wXML_Push_()
						ypt9(ii,nel) = wXML_getAttributeDouble_("position")
						iResult = wXML_TraverseToNextSibling_()
					!call wXML_Pop_()
				enddo
			call wXML_Pop_()
							
			call wXML_Push_()
				iResult = wXML_TraverseToSon_("CONSTRAINTS")
				iResult = wXML_TraverseToSon_("CONSTRAINT")
				panneau(nel).m1(:) = 0
				
				do while (iResult.eq.1)
					is=INT4(wXML_getAttributeDouble_("loadcase"))
					panneau(nel).m1(is) = panneau(nel).m1(is) + 1
					if (.not.associated(panneau(nel)%lcont4_init).and.nsolm.gt.0.and.m1max.gt.0) then
			 			allocate (panneau(nel)%lcont4_init(nsolm,m1max,2))
						!allocate (panneau(nel)%lcont4(nsolm,m1max,2))
						allocate (panneau(nel)%cjmax9_init(nsolm,m1max))
						!allocate (panneau(nel)%cjmax9(nsolm,m1max))
						allocate (panneau(nel)%inv3_init(nsolm,m1max))
						!allocate (panneau(nel)%inv3(nsolm,m1max))
						!allocate (panneau(nel)%lcont(2,(m1max*nsolm)))
						!allocate (panneau(nel)%ipts2(iptmax))
						!allocate (panneau(nel)%cjmax(m1max*nsolm))
						!allocate (panneau(nel)%inv(m1max*nsolm))
						panneau(nel).lcont4_init(:,:,:) = 0
						!panneau(nel).lcont4(:,:,:) = 0
						panneau(nel).cjmax9_init(:,:) = 0.d00
						!panneau(nel).cjmax9(:,:) = 0.d00
						panneau(nel).inv3_init(:,:) = 0
						!panneau(nel).inv3(:,:) = 0
						!panneau(nel).lcont(:,:) = 0
						!panneau(nel).ipts2(:) = 0
						!panneau(nel).cjmax(:) = 0
						!panneau(nel).inv(:) = 0
					endif

					jj=panneau(nel).m1(is) ! !ici seulement, faire attention !!!
					panneau(nel).lcont4_init(is,jj,1) = INT4(wXML_getAttributeDouble_("type"))
					panneau(nel).cjmax9_init(is,jj) = wXML_getAttributeDouble_("max_value")
					panneau(nel).inv3_init(is,jj) = INT4(wXML_getAttributeDouble_("side"))
					panneau(nel).lcont4_init(is,jj,2) = INT4(wXML_getAttributeDouble_("position"))
					iResult = wXML_TraverseToNextSibling_()
				enddo

			call wXML_Pop_()
							

		call wXML_Pop_()
					
		call wXML_Push_()
			iResult = wXML_TraverseToSon_("GEOMETRICAL")
			panneau(nel).isema = INT4(wXML_getAttributeDouble_("adjust_frame_flange"))
			panneau(nel).isemr = INT4(wXML_getAttributeDouble_("adjust_stiff_flange"))
			
			iResult = wXML_TraverseToSon_("CONSTRAINT")
			if (iResult.eq.1) then
				panneau(nel).m2 = count_Elmt()
				do ii=1,panneau(nel).m2
					panneau(nel).lm2(ii) = INT4(wXML_getAttributeDouble_("id"))
					iResult = wXML_TraverseToNextSibling_()
				enddo
			endif
		call wXML_Pop_()
					
		call wXML_Push_()
			iResult = wXML_TraverseToSon_("SLOSHING")
			iResult = wXML_TraverseToSon_("CONSTRAINT")
			if (iResult.eq.1) then
				panneau(nel).nsloshm = count_Elmt()
				do ii=1,panneau(nel).nsloshm
					panneau(nel).nslosh(ii) = INT4(wXML_getAttributeDouble_("id"))
					panneau(nel).press_slosh = wXML_getAttributeDouble_("press_slosh")
					iResult = wXML_TraverseToNextSibling_()
				enddo
			endif
		call wXML_Pop_()
	call wXML_Pop_()

call wXML_Pop_() !! On remonte à plate

call read_connected_panels(nel)


return
end subroutine

!********************************************
subroutine read_connected_panels(nel)
use param_XML
use param_section, ONLY : panneau

implicit double precision (a-h,o-z)
integer*4 ii,nel,iResult

call wXML_Push_()
	iResult = wXML_TraverseToSon_("CONNECTED_PANELS")
	iResult = wXML_TraverseToSon_("CONNECTED_PANEL")

	ii=1
	do while (iResult.eq.1)
		panneau(nel).noh9(ii) = INT4(wXML_getAttributeDouble_("id"))
		ii = ii+1
		iResult = wXML_TraverseToNextSibling_()
	enddo

call wXML_Pop_()

return
end

!*********************************
 
!Character (len=20) Function Up (string)
!Character(len=*) string
!
!return
!end function Up
