	OPTIONS /EXTEND

	SUBROUTINE FATIGUE_STIFFENER
	use param_section


	call STIF_NominalStress

	call STIF_HotspotStress

	call STIF_NotchStress

	call STIF_Damage

	end

!*****************************
    SUBROUTINE STIF_NominalStress
	use param_section
!      real*8 PI
	
!	SUBMODULE NOMINAL_STRESS
!	************************** 
!    CALCULATION OF NOMINALS STRESSES  -------------------------------------
!    The submodule calculates nominal stress ranges at critical locations 
!    of structural member based on beam or plate theory. 

!sx raid (sem)- contrainte dans semelle, raid   
!	sx_raid_sem(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Stif(Fat_pts),panneau(nel_fatigue).iy_Stif(Fat_pts))

! contrainte flexion locale du raidisseur semelle:  (m=pl^2/10)
!	sx_Loc_raid_sem(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Stif(Fat_pts),panneau(nel_fatigue).iy_Stif(Fat_pts))		


	NOM_STRESS_stif(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Stif(Fat_pts),panneau(nel_fatigue).iy_Stif(Fat_pts))=	abs(	&
	sx_raid_sem(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Stif(Fat_pts),panneau(nel_fatigue).iy_Stif(Fat_pts))+	&
	sx_Loc_raid_sem(caschge_fatigue,nel_fatigue))/10**6	!pour passer en MPa		!fat_new			



!	NOM_STRESS_stif(1,1,ix_Stif(1,Fat_pts),iy_Stif(1,Fat_pts))=179.12
!	NOM_STRESS_stif(2,1,ix_Stif(1,Fat_pts),iy_Stif(1,Fat_pts))=52.48
!	NOM_STRESS_stif(3,1,ix_Stif(1,Fat_pts),iy_Stif(1,Fat_pts))=8.704
!	NOM_STRESS_stif(4,1,ix_Stif(1,Fat_pts),iy_Stif(1,Fat_pts))=15.371
!	NOM_STRESS_stif(5,1,ix_Stif(1,Fat_pts),iy_Stif(1,Fat_pts))=175.419
!	NOM_STRESS_stif(6,1,ix_Stif(1,Fat_pts),iy_Stif(1,Fat_pts))=120.035
!	NOM_STRESS_stif(7,1,ix_Stif(1,Fat_pts),iy_Stif(1,Fat_pts))=21.284

!Panel_fatigue	25	Departure	Node	x	=	L/2

!IF (nel_fatigue.EQ.2) PAUSE

!	NOM_STRESS_stif(1,2,ix_Stif(2,Fat_pts),iy_Stif(2,Fat_pts))=60.289
!	NOM_STRESS_stif(2,2,ix_Stif(2,Fat_pts),iy_Stif(2,Fat_pts))=91.291
!	NOM_STRESS_stif(3,2,ix_Stif(2,Fat_pts),iy_Stif(2,Fat_pts))=18.3
!	NOM_STRESS_stif(4,2,ix_Stif(2,Fat_pts),iy_Stif(2,Fat_pts))=19.125
!	NOM_STRESS_stif(5,2,ix_Stif(2,Fat_pts),iy_Stif(2,Fat_pts))=61.576
!	NOM_STRESS_stif(6,2,ix_Stif(2,Fat_pts),iy_Stif(2,Fat_pts))=113.625
!	NOM_STRESS_stif(7,2,ix_Stif(2,Fat_pts),iy_Stif(2,Fat_pts))=0.763	



	END
!*****************************************************************************************

    SUBROUTINE STIF_HotspotStress
	use param_section
	
!	SUBMODULE FAT_HotspotStress !
!	*************************** !
!	 CALCULATION OF HOT-SPOT STRESSES --------------------------------------
!	 The submodele calculates structural stress ranges based on 
!	 semi-analytical approaches for stress concentration factors. 
!	 Library of different developed solutions is applied.


! HS_STRESS : HOT SPOT STRESS


	HS_STRESS_Stif(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Stif(Fat_pts),panneau(nel_fatigue).iy_Stif(Fat_pts))=	&
	panneau(nel_fatigue).Kt_Stif(Fat_pts)*NOM_STRESS_Stif(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Stif(Fat_pts),panneau(nel_fatigue).iy_Stif(Fat_pts))


!	HS_STRESS_Stif(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Stif(Fat_pts),panneau(nel_fatigue).iy_Stif(Fat_pts))=	abs(&
!	1.5*sx_raid_sem(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Stif(Fat_pts),panneau(nel_fatigue).iy_Stif(Fat_pts))+&
!	1.65*sx_Loc_raid_sem(caschge_fatigue,nel_fatigue))		

  
!	HS_STRESS_stif(1,1,ix_Stif(1,Fat_pts),iy_Stif(1,Fat_pts))=215.9446
!	HS_STRESS_stif(2,1,ix_Stif(1,Fat_pts),iy_Stif(1,Fat_pts))=0
!	HS_STRESS_stif(3,1,ix_Stif(1,Fat_pts),iy_Stif(1,Fat_pts))=18.79214
!	HS_STRESS_stif(4,1,ix_Stif(1,Fat_pts),iy_Stif(1,Fat_pts))=13.63188
!	HS_STRESS_stif(5,1,ix_Stif(1,Fat_pts),iy_Stif(1,Fat_pts))=207.8501
!	HS_STRESS_stif(6,1,ix_Stif(1,Fat_pts),iy_Stif(1,Fat_pts))=161.4691 
!	HS_STRESS_stif(7,1,ix_Stif(1,Fat_pts),iy_Stif(1,Fat_pts))=33.84488




!	Sensibilités de DShs_1,DShs_2,DShs_3,DShs_4

!	CALL ANNUL4(dDShs_1,Neto,9,Fat_NSOLM,Neto)
!	CALL ANNUL4(dDShs_2,Neto,9,Fat_NSOLM,Neto)
!	CALL ANNUL4(dDShs_3,Neto,9,Fat_NSOLM,Neto)
!	CALL ANNUL4(dDShs_4,Neto,9,Fat_NSOLM,Neto)

	
!	DO IPAN=1,NETO
!	NBRXI=NVAR(IPAN)
!	L1=0
!	IF(NBRXI.EQ.0) GOTO 126
!	DO K=1,NBRXI
!	KK=NXIT(K,IPAN)
!	LL=L1+K
			
!		IF(nel_fatigue.EQ.IPAN) THEN	! sensibilités par rapport aux variables de conception du panneau considéré 
!			
!				dDShs_1(nel_fatigue,LL,casChge,IPAN)=dDS_1(nel_fatigue,LL,casChge,IPAN)*Kt_hs_1;
!				dDShs_2(nel_fatigue,LL,casChge,IPAN)=dDS_2(nel_fatigue,LL,casChge,IPAN)*Kt_hs_2;
!				dDShs_3(nel_fatigue,LL,casChge,IPAN)=dDS_3(nel_fatigue,LL,casChge,IPAN)*Kt_hs_3;
!				dDShs_4(nel_fatigue,LL,casChge,IPAN)=dDS_4(nel_fatigue,LL,casChge,IPAN)*Kt_hs_4;
!
!		ELSE					! sensibilités par rapport aux variables de conception des autres panneaux



!		ENDIF
!		ENDDO
 ! 126		CONTINUE
        
!	  	L1=L1+NBRXI
!		ENDDO


       END


	 
!*****************************************************************************************
    SUBROUTINE STIF_NotchStress
	use param_section

!	 SUBMODULE FAT_NotchStress !
!	************************* !
!	CALCULATION OF NOTCH STRESSES -----------------------------------------
!	The submodele calculates notch stress range with the help of additional 
!	stress concentration factor, which defines the relationship between 
!	hot-spot and notch stress.





! NS_STRESS : NOTCH STRESS



if (method_fatigue .eq.1) then	!DNV method_fatigue			

	panneau(nel_fatigue).Kw_Stif(Fat_pts)=1.5

elseif (method_fatigue .eq.2) then	!BV method_fatigue			

	panneau(nel_fatigue).Kw_Stif(Fat_pts)=panneau(nel_fatigue).LAMBDA_Stif(Fat_pts)*SQRT(panneau(nel_fatigue).TETA_Stif(Fat_pts)/30)

endif


! NOTCH STRESS NS_STRESS


	NS_STRESS_Stif(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Stif(Fat_pts),panneau(nel_fatigue).iy_Stif(Fat_pts))=	&
	0.7*panneau(nel_fatigue).Kw_Stif(Fat_pts)*HS_STRESS_Stif(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Stif(Fat_pts),panneau(nel_fatigue).iy_Stif(Fat_pts))




!	Sensibilités de DSln_1,DSln_2,DSln_3,DSln_4				

!	CALL ANNUL4(dDSln_1,Neto,9,Fat_NSOLM,Neto)				
!	CALL ANNUL4(dDSln_2,Neto,9,Fat_NSOLM,Neto)				
!	CALL ANNUL4(dDSln_3,Neto,9,Fat_NSOLM,Neto)				
!	CALL ANNUL4(dDSln_4,Neto,9,Fat_NSOLM,Neto)				

	
!	DO IPAN=1,NETO				
!	NBRXI=NVAR(IPAN)				
!	L1=0
!	IF(NBRXI.EQ.0) GOTO 127				
!	DO K=1,NBRXI				
!	KK=NXIT(K,IPAN)				
!	LL=L1+K
!			
!!		IF(nel_fatigue.EQ.IPAN) THEN	! sensibilités par rapport aux variables de conception du panneau considéré 
!			
!			dDSln_1(nel_fatigue,LL,casChge,IPAN)=dDShs_1(nel_fatigue,LL,casChge,IPAN)*Kw(nel_fatigue,1);				
!			dDSln_2(nel_fatigue,LL,casChge,IPAN)=dDShs_2(nel_fatigue,LL,casChge,IPAN)*Kw(nel_fatigue,2);				
!			dDSln_3(nel_fatigue,LL,casChge,IPAN)=dDShs_3(nel_fatigue,LL,casChge,IPAN)*Kw(nel_fatigue,3);				
!			dDSln_4(nel_fatigue,LL,casChge,IPAN)=dDShs_4(nel_fatigue,LL,casChge,IPAN)*Kw(nel_fatigue,4);				
!
!		ELSE					! sensibilités par rapport aux variables de conception des autres panneaux
!
!
!
!!		ENDIF
!		ENDDO				
 ! 127		CONTINUE				
        
!	  	L1=L1+NBRXI				
!		ENDDO				
!


if (method_fatigue .eq.2) then	!BV method_fatigue	


	IF (NS_STRESS_Stif(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Stif(Fat_pts),panneau(nel_fatigue).iy_Stif(Fat_pts)).EQ.0) GOTO 1

	kc_Stif(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Stif(Fat_pts),panneau(nel_fatigue).iy_Stif(Fat_pts))=	&
	0.4*(sigy(indMateriau(nel_fatigue))/NS_STRESS_Stif(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Stif(Fat_pts),panneau(nel_fatigue).iy_Stif(Fat_pts))) + 0.6

	IF (kc_Stif(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Stif(Fat_pts),panneau(nel_fatigue).iy_Stif(Fat_pts)).LE.0.8) THEN 								

	kc_Stif(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Stif(Fat_pts),panneau(nel_fatigue).iy_Stif(Fat_pts))=0.800											

	ELSEIF(kc_Stif(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Stif(Fat_pts),panneau(nel_fatigue).iy_Stif(Fat_pts)).GT.1.) THEN							

	kc_Stif(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Stif(Fat_pts),panneau(nel_fatigue).iy_Stif(Fat_pts))=1.												

	ENDIF
																		
1	CONTINUE

! NOTCH STRESS NS_STRESS	BV method_fatigue	

!IF (nel_fatigue.EQ.2) PAUSE


	NS_STRESS_Stif(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Stif(Fat_pts),panneau(nel_fatigue).iy_Stif(Fat_pts))= &
	kc_Stif(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Stif(Fat_pts),panneau(nel_fatigue).iy_Stif(Fat_pts))*	&
	NS_STRESS_Stif(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Stif(Fat_pts),panneau(nel_fatigue).iy_Stif(Fat_pts))

A=1
endif	!BV method_fatigue	



!	CALL ANNUL4(dKc_1,Neto,9,Fat_NSOLM,Neto)											
!	CALL ANNUL4(dKc_2,Neto,9,Fat_NSOLM,Neto)											
!	CALL ANNUL4(dKc_3,Neto,9,Fat_NSOLM,Neto)											
!	CALL ANNUL4(dKc_4,Neto,9,Fat_NSOLM,Neto)											

	
!	DO IPAN=1,NETO																		
!	NBRXI=NVAR(IPAN)																	
!	L1=0																				
!	IF(NBRXI.EQ.0) GOTO 528																
!	DO K=1,NBRXI																		
!	KK=NXIT(K,IPAN)																		
!	LL=L1+K																				
!			
!	IF (DSln_1(nel_fatigue,casChge).EQ.0) GOTO 10
!
!		IF ((Kc_1(nel_fatigue,casChge).LE.0.8).OR.(Kc_1(nel_fatigue,casChge).GE.1.)) THEN 											
!			dKc_1(nel_fatigue,LL,casChge,IPAN)=0.0																			
!		ELSE																													
!			dKc_1(nel_fatigue,LL,casChge,IPAN)=-0.4*sigy(indMateriau(nel_fatigue))*dDSln_1(nel_fatigue,LL,casChge,IPAN)/DSln_1(nel_fatigue,casChge)**2			
!		ENDIF																										
!
!10	CONTINUE

		
!	IF (DSln_2(nel_fatigue,casChge).EQ.0) GOTO 11

!		IF ((Kc_2(nel_fatigue,casChge).LE.0.8).OR.(Kc_2(nel_fatigue,casChge).GE.1.)) THEN 											
!			dKc_2(nel_fatigue,LL,casChge,IPAN)=0.0																			
!		ELSE																														
!			dKc_2(nel_fatigue,LL,casChge,IPAN)=-0.4*sigy(indMateriau(nel_fatigue))*dDSln_2(nel_fatigue,LL,casChge,IPAN)/DSln_2(nel_fatigue,casChge)**2				
!		ENDIF																										
!
!11	CONTINUE
!
!	IF (DSln_3(nel_fatigue,casChge).EQ.0) GOTO 12
!
!		IF ((Kc_3(nel_fatigue,casChge).LE.0.8).OR.(Kc_3(nel_fatigue,casChge).GE.1.)) THEN 											
!			dKc_3(nel_fatigue,LL,casChge,IPAN)=0.0																			
!		ELSE																														
!			dKc_3(nel_fatigue,LL,casChge,IPAN)=-0.4*sigy(indMateriau(nel_fatigue))*dDSln_3(nel_fatigue,LL,casChge,IPAN)/DSln_3(nel_fatigue,casChge)**2				
!		ENDIF
!																												
!12	CONTINUE

!	IF (DSln_4(nel_fatigue,casChge).EQ.0) GOTO 13

!		IF ((Kc_4(nel_fatigue,casChge).LE.0.8).OR.(Kc_4(nel_fatigue,casChge).GE.1.)) THEN								 			
!			dKc_4(nel_fatigue,LL,casChge,IPAN)=0.0																			
!		ELSE																														
!			dKc_4(nel_fatigue,LL,casChge,IPAN)=-0.4*sigy(indMateriau(nel_fatigue))*dDSln_4(nel_fatigue,LL,casChge,IPAN)/DSln_4(nel_fatigue,casChge)**2				
!		ENDIF																										
!
!13	CONTINUE
		
		
!	ENDDO															
 ! 528	CONTINUE															
        
!	L1=L1+NBRXI															
!	ENDDO															
	
!	Sensibilités FINALES de DSln_1,DSln_2,DSln_3,DSln_4

	
!	DO IPAN=1,NETO																		
!	NBRXI=NVAR(IPAN)																	
!	L1=0																				
!	IF(NBRXI.EQ.0) GOTO 628																
!	DO K=1,NBRXI																		
!	KK=NXIT(K,IPAN)																		
!	LL=L1+K																				
			

!			dDSln_1(nel_fatigue,LL,casChge,IPAN)=dKc_1(nel_fatigue,LL,casChge,IPAN)*DSln_1(nel_fatigue,casChge)+			
 !    *							Kc_1(nel_fatigue,casChge)*dDSln_1(nel_fatigue,LL,casChge,IPAN)						
!
!			dDSln_2(nel_fatigue,LL,casChge,IPAN)=dKc_2(nel_fatigue,LL,casChge,IPAN)*DSln_2(nel_fatigue,casChge)+			
 !    *							Kc_2(nel_fatigue,casChge)*dDSln_2(nel_fatigue,LL,casChge,IPAN)						
!
!			dDSln_3(nel_fatigue,LL,casChge,IPAN)=dKc_3(nel_fatigue,LL,casChge,IPAN)*DSln_3(nel_fatigue,casChge)+			
 !    *							Kc_3(nel_fatigue,casChge)*dDSln_3(nel_fatigue,LL,casChge,IPAN)						
     		
!			dDSln_4(nel_fatigue,LL,casChge,IPAN)=dKc_4(nel_fatigue,LL,casChge,IPAN)*DSln_4(nel_fatigue,casChge)+			
  !   *							Kc_4(nel_fatigue,casChge)*dDSln_4(nel_fatigue,LL,casChge,IPAN)						
 !    		
!	ENDDO																
 ! 628	CONTINUE															
        
!	L1=L1+NBRXI															
!	ENDDO																
						



!	ENDIF																	
		
	END




!*****************************************************************************************
    SUBROUTINE STIF_Damage
	use param_section
	real*8 rezult_gamma,rezult_1,rezult_2,rezult_3,ksi,nu,mu,h_Weibull_Stif

!	integer m

!  SUBMODEL DAMAGE_SUM.m  !
! *********************** !
!	CALCULATION OF DAMAGE SUM----------------------------------------------
!	The submodule calculates damage sum based on linear damage theory and
!	Palmer-Miner rule. Submodel uses S-N curve, which includes the fatigue 
!	reduction and uncertainness due to dimensional accuracy, welding 
!	distortion, and environment. (FOR EACH LOAD CASE)
 


!	 CHECKING OF STRESS VALUES and weibull factor

	if (NS_STRESS_Stif(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Stif(Fat_pts),panneau(nel_fatigue).iy_Stif(Fat_pts)).LT.0) THEN
	WRITE(*,*) 'Error, notch stress range is negative'
		write(*,*) 'stop'
		read(*,*)
		stop
	ENDIF

	if ((panneau(nel_fatigue).WEIB_FACT_Stif(Fat_pts).LT.0).OR.(panneau(nel_fatigue).WEIB_FACT_Stif(Fat_pts).GT.4)) THEN
	    WRITE(*,*) 'Error, Weib_fact is negative or greater than 4'
	    write(*,*) 'stop'
		read(*,*)
	    stop
	ENDIF



if (method_fatigue .eq.1) then	!DNV method_fatigue

!	basic shape parameter
	h0 = 2.21 - 0.54*log10(length)		 

!	additional factor depending on motion response period
	ha = 0.05 !in general
!	ha = 0.00 !for plating subjected to forces related to roll motions for vessels with roll period TR > 14 sec.

!	Calcul de TA : periode moyenne, en secondes	
	TA=4*LOG10(length)

!	Number of load cycles during design life
	Ntcyc=0.85*6.3*(10**8.0)/TA


!	 CHECK 

	IF (Ntcyc.LT.1.) THEN
	WRITE(*,*) 'Input value error: Invalid number of load cycles (Ntcyc<1)'
		PAUSE 'STOP'
		STOP
	ENDIF

!	h_Weibull : Weibull shape parameter


!	z(nel_fatigue,3) ! coord  y du noeud depart
!	z(nel_fatigue,4) ! coord  y du noeud d'arrivee
!	znel_fatigue = vertical distance from baseline to considered longitudinal (m)


		znel_fatigue=abs(z(nel_fatigue,3)+(z(nel_fatigue,4)-z(nel_fatigue,3))*(panneau(nel_fatigue).iy_Stif(Fat_pts)-1)/2)/1000

	if (panneau(nel_fatigue).WEIB_FACT_Stif(Fat_pts).EQ.1) then
		
		h_Weibull_Stif = h0																	!For deck longitudinals

	elseif (panneau(nel_fatigue).WEIB_FACT_Stif(Fat_pts).EQ.2) then

		if ((Znel_fatigue.GT.Tirant(Fat_NNSOL(caschge_fatigue))).AND.(znel_fatigue.LT.DEPTH)) THEN			!For ship side
		h_Weibull_Stif = h0 + ha*( DEPTH - znel_fatigue ) / ( DEPTH - Tirant(Fat_NNSOL(caschge_fatigue)))				!For ship side above the waterline Tirant < znel_fatigue <DEPTH
	
		elseif (znel_fatigue .EQ.Tirant(Fat_NNSOL(caschge_fatigue))) then
		h_Weibull_Stif = h0 + ha																	!For ship side at the waterline znel_fatigue = Tirant

		elseif (znel_fatigue.LT.Tirant(Fat_NNSOL(caschge_fatigue))) then
		h_Weibull_Stif = h0 + ha*znel_fatigue/Tirant(Fat_NNSOL(caschge_fatigue))-0.005*(Tirant(Fat_NNSOL(caschge_fatigue))-znel_fatigue)		!For ship side below the waterline znel_fatigue < Tirant
		
		ENDIF

	elseif (panneau(nel_fatigue).WEIB_FACT_Stif(Fat_pts).EQ.3) then
	h_Weibull_Stif = h0 - 0.005*Tirant(Fat_NNSOL(caschge_fatigue))											!For bottom longitudinals 
	
	elseif (panneau(nel_fatigue).WEIB_FACT_Stif(Fat_pts).EQ.4) then
	h_Weibull_Stif = h0 + ha																!For longitudinal and transverse bulkheads
	endif


!	CALCULATION OF DAMAGE SUM D
	CALL gamma_complete((panneau(nel_fatigue).m_stif(Fat_pts)/h_Weibull_Stif),rezult_gamma)

Damage_Stif(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Stif(Fat_pts),panneau(nel_fatigue).iy_Stif(Fat_pts))=	&
Ntcyc/panneau(nel_fatigue).C_stif(Fat_pts)*(NS_STRESS_Stif(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Stif(Fat_pts),panneau(nel_fatigue).iy_Stif(Fat_pts))**panneau(nel_fatigue).m_stif(Fat_pts))* &
	(LOG(10.**8))**(-panneau(nel_fatigue).m_stif(Fat_pts)/h_Weibull_Stif)*rezult_gamma







!	Sensibilités de D_1,D_2,D_3,D_4

!	CALL ANNULD(dD_1,NTOT)
!	CALL ANNULD(dD_2,NTOT)
!	CALL ANNULD(dD_3,NTOT)
!	CALL ANNULD(dD_4,NTOT)

	
!	DO IPAN=1,NETO
!	NBRXI=NVAR(IPAN)
!	L1=0
!	IF(NBRXI.EQ.0) GOTO 1
!	DO K=1,NBRXI
!	KK=NXIT(K,IPAN)
!	LL=L1+K
!			
!		IF(nel_fatigue.EQ.IPAN) THEN	! sensibilités par rapport aux variables de conception du panneau considéré 
!			
!
!			dD_1(nel_fatigue,LL,caschge_fatigue,IPAN)=Ntcyc/C*(m*DSln_1(nel_fatigue,caschge_fatigue)**(m-1)*dDSln_1(nel_fatigue,LL,caschge_fatigue,IPAN))* 
 !    *					(LOG(10.**8))**(-m/h_Weibull)*rezult_gamma;
!			dD_2(nel_fatigue,LL,caschge_fatigue,IPAN)=Ntcyc/C*(m*DSln_2(nel_fatigue,caschge_fatigue)**(m-1)*dDSln_2(nel_fatigue,LL,caschge_fatigue,IPAN))* 
 !    *					(LOG(10.**8))**(-m/h_Weibull)*rezult_gamma;
!			dD_3(nel_fatigue,LL,caschge_fatigue,IPAN)=Ntcyc/C*(m*DSln_3(nel_fatigue,caschge_fatigue)**(m-1)*dDSln_3(nel_fatigue,LL,caschge_fatigue,IPAN))* 
 !    *					(LOG(10.**8))**(-m/h_Weibull)*rezult_gamma;
!			dD_4(nel_fatigue,LL,caschge_fatigue,IPAN)=Ntcyc/C*(m*DSln_4(nel_fatigue,caschge_fatigue)**(m-1)*dDSln_4(nel_fatigue,LL,caschge_fatigue,IPAN))* 
 !    *					(LOG(10.**8))**(-m/h_Weibull)*rezult_gamma;



!		ELSE					! sensibilités par rapport aux variables de conception des autres panneaux



!		ENDIF
!		ENDDO
 ! 1		CONTINUE
        
!	  	L1=L1+NBRXI
!		ENDDO

!*******************************************************************************
	
	elseif(method_fatigue.EQ.2) then		!BV method_fatigue


!	probabilité d'excédence 
	PR=10.**(-5.)

!	Calcul de TA : periode moyenne, en secondes	
	TA=4*LOG10(length)

!	alfa0 : facteur de navigation pris égal à 0.8
	alfa0=0.85

!	Calcul de Ntcyc	: nombre total des cycles
	Ntcyc=631*alfa0/TA*10**6.0

!	Calcul de Ntfl (Tfl:Increased design life (between 25 and 40))
	Ntfl=31.55*alfa0*Tfl*10**6.0/TA
	
	IF(Tfl.GE.25) THEN
	CFL=LOG10(0.2*LOG10(Ntfl))/LOG10(0.2*LOG10(Ntcyc))
	else
	CFL=1.
	endif

!	Calcul de ksi,ksi0 : paramêtres de forme de la fonction de WEIBULL
	ksi0=(73-0.07*length)/60*CFL
	IF(ksi0.LT.0.85) THEN
	ksi0=0.85
	ENDIF


!	CALL ANNUL2(Damage_Stif,nsolm_fatigue,NETO,ix_max,iy_max)
!	CALL ANNUL2(Damage_Plate,nsolm_fatigue,NETO,ix_max,iy_max)
!	CALL ANNUL2(Damage_Plate_frame,nsolm_fatigue,NETO,ix_max,iy_max)
!	CALL ANNUL2(Damage_frame,nsolm_fatigue,NETO,ix_max,iy_max)
!	CALL ANNUL2(Damage_Girder,nsolm_fatigue,NETO,ix_max,mt_max)
!	CALL ANNUL2(Damage_Pilar,nsolm_fatigue,NETO,ix_max,iy_max)


!	CALL ANNUL4(dD_1,Neto,9,nsolm_fatigue,Neto)
!	CALL ANNUL4(dD_2,Neto,9,nsolm_fatigue,Neto)
!	CALL ANNUL4(dD_3,Neto,9,nsolm_fatigue,Neto)
!	CALL ANNUL4(dD_4,Neto,9,nsolm_fatigue,Neto)

!	if(Sty.EQ.1) then

!	damage : Hot spot 1
			

!     characteristics if the stiffened elements (epsa,entr,delta,hxr,dxr,wxr,txr,sigy(indMateriau(nel_fatigue)),sigy(indMateriau(nel_fatigue)))
!     ------------------------------------------
!  .  sl = epsa : length of stiffener between transverse frames (m.)
!  .  bp = entr : longitudinal stiffener spacing   (m.)
!  .  tp = delta: thickness of parent plate        (m.)
!  .  hw = hxr  : height of stiffener web          (m.)
!  .  tw = dxr  : thickness of stiffener web       (m.)
!  .  bf = wxr  : breadth of stiffener flange      (m.)
!  .  tf = txr  : thickness of stiffener flange    (m.)
!  .  syp= sigy(indMateriau(nel_fatigue)) : yield strength of parent plate   (n/m2)   
!  .  syw= sigy(indMateriau(nel_fatigue)) : yield strength of stiffener      (n/m2)   

!   les variables de conception  ; nbrxi = nvar(nel_fatigue,iboat)
!   -------------------------------------------------
!       1       delta =  épaisseur du bordage
!       2       hya   =  hauteur de l'âme des cadres   
!       3       dya   =  épaisseur de l'âme des cadres   
!       4       wya   =  largeur des semelles des cadres   
!       5       epsa  =  entredistance entre cadres   
!       6       hxr   =  hauteur de l'âme des raidisseurs
!       7       dxr   =  épaisseur de l'âme des raidisseurs
!       8       wxr   =  largeur des semelles des raidisseurs
!       9       epsr  =  entredistance entre raidisseurs

! 	autres paramètres :
!	 	tya = épaisseur semelle aiguilles
!     	txr = épaisseur semelle raidisseurs 
!
!  	Les variables de conception pour epontille
!     ------------------------------------------					
!	1	hya		=	hauteur demi âme ou diam ou côté ext.	
!	2	dya		=	épaisseur d'âme							
!	3	wya		=	largeur de semelle						
!	4	epais	=	épaisseur de paroi mince				
!	5	epsa	=	entredistance entre épontillesc


!	thF	: épaisseur de l'élément considéré (mm) (minimum thickness of connected elements)

!	txr  : thickness of stiffener flange    (m.)
!	dxr  : thickness of stiffener web 


!IF (nel_fatigue.EQ.2) PAUSE

	znel_fatigue=abs(z(nel_fatigue,3)+(z(nel_fatigue,4)-z(nel_fatigue,3))*(panneau(nel_fatigue).iy_Stif(Fat_pts)-1)/2)/1000

	if (panneau(nel_fatigue).dxr .lt. panneau(nel_fatigue).txr) then
	thF=panneau(nel_fatigue).dxr*1000		
	else
	thF=panneau(nel_fatigue).txr*1000
	endif



!	Calcul de kpSN : constante de la Design S-N curve
	IF(thF.LT.22) THEN
	kpSN=5.802*(10**12.0)
	ELSE
	kpSN=5.802*(10**12.0)*(22.0/thF)**(0.9)
	ENDIF

!	Calcul de SigmaQ : étendue(sigmaMAx-sigmaMin) de contrainte à l'intersection des 02 segments de la courbe S-N.
	SigmaQ=(kpSN*(10**(-7.0)))**(1.0/3.0)

	ksi=ksi0*(1.04-0.14*ABS(znel_fatigue-Tirant(Fat_NNSOL(caschge_fatigue)))/(DEPTH-Tirant(Fat_NNSOL(caschge_fatigue))))

!	CALL ANNULD(nu,nsolm_fatigue)

	nu=0.0

	if (NS_STRESS_Stif(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Stif(Fat_pts),panneau(nel_fatigue).iy_Stif(Fat_pts)).EQ.0) GOTO 10

!	nu : coefficient utilisé pour le calcul de la fonction GAMMA incomplète.
	nu=-((SigmaQ/NS_STRESS_Stif(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Stif(Fat_pts),panneau(nel_fatigue).iy_Stif(Fat_pts)))**(ksi))*LOG(PR)					

!	rezult_1 et rezult_2:résultats de la fct gamma incomplète
	call gamma_incomplete(3/ksi,nu,rezult_1)						
	call gamma_incomplete(5/ksi,nu,rezult_2)							
!	rezult_3 :résultat de la fct gamma complète
	CALL gamma_complete(3/ksi,rezult_3)

!	mu : coefficient qui tient en compte du changement de pente de la courbe S-N
	mu=1-(((rezult_1)-(rezult_2*(nu**(-2.0/ksi))))/rezult_3)		
	
!	DAMAGE		

	Damage_Stif(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Stif(Fat_pts),panneau(nel_fatigue).iy_Stif(Fat_pts))=	&
	Ntcyc*(NS_STRESS_Stif(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Stif(Fat_pts),panneau(nel_fatigue).iy_Stif(Fat_pts))**3.)/	&
	(kpSN*(-LOG(PR))**(3./ksi))*mu*rezult_3




ENDIF		!BV method_fatigue

10	CONTINUE
	
	end






!****************************************************************************************
!****************************************************************************************
!****************************************************************************************
	OPTIONS /EXTEND
	SUBROUTINE FATIGUE_PLATE
	use param_section



!    CALCULATION OF NOMINALS STRESSES  -------------------------------------
!    The submodule calculates nominal stress ranges at critical locations 
!    of structural member based on beam or plate theory. 
	call PLATE_NominalStress


!	 CALCULATION OF HOT-SPOT STRESSES --------------------------------------
!	 The submodele calculates structural stress ranges based on 
!	 semi-analytical approaches for stress concentration factors. 
!	 Library of different developed solutions is applied.
	call PLATE_HotspotStress



!	CALCULATION OF NOTCH STRESSES -----------------------------------------
!	The submodele calculates notch stress range with the help of additional 
!	stress concentration factor, which defines the relationship between 
!	hot-spot and notch stress.
	call PLATE_NotchStress

!	CALCULATION OF DAMAGE SUM----------------------------------------------
!	The submodule calculates damage sum based on linear damage theory and
!	Palmer-Miner rule. Submodel uses S-N curve, which includes the fatigue 
!	reduction and uncertainness due to dimensional accuracy, welding 
!	distortion, and environment. (FOR EACH LOAD CASE)
	call PLATE_Damage

	end


!*****************************************************************************************
      SUBROUTINE PLATE_NominalStress
	use param_section
!      real*8 PI
	
!	SUBMODULE NOMINAL_STRESS
!	************************** 


!Top surface of plate
!sx (n/m2) - contr. longit.  sx  bordé (z=+d/2)
sx= sx_plaque_top(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate(Fat_pts),panneau(nel_fatigue).iy_Plate(Fat_pts))

!sy (n/m2) - contr. transv.  sy  bordé (z=+d/2)     
sy= sy_plaque_top(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate(Fat_pts),panneau(nel_fatigue).iy_Plate(Fat_pts))

!txy (n/m2) - contr. cisaill. txy bordé (z=+d/2)
Sxy=sxy_plaque_top(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate(Fat_pts),panneau(nel_fatigue).iy_Plate(Fat_pts))


if ((abs(Sx)).LT.(abs(Sy)))  then				!fat_new
alfa_deg=abs(1./2.*atan(2*Sxy/(Sx-Sy))*180/pi)
endif

if ((abs(Sx)).GT.(abs(Sy)))  then
alfa_deg=abs(90 - 1./2.*atan(2*Sxy/(Sx-Sy))*180/pi);
endif

if (Sx.EQ.Sy) then
alfa_deg=0;
endif

if ((Sxy.EQ.0).and.(abs(Sx).LE.abs(Sy))) then
alfa_deg=0;
endif

if ((Sxy.EQ.0).and.((abs(Sx)).GT.(abs(Sy))))then
alfa_deg=90;
endif

if ((alfa_deg).LE.(60)) then

Sig1=(Sx+Sy)/2 + 1./2.*((Sx-Sy)**2 + 4*Sxy**2)**0.5;
Sig2=(Sx+Sy)/2 - 1./2.*((Sx-Sy)**2 + 4*Sxy**2)**0.5;  

NOM_STRESS_Plate_top(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate(Fat_pts),panneau(nel_fatigue).iy_Plate(Fat_pts))= &
max(abs(Sig1),abs(Sig2))

endif

if ((alfa_deg).GT.(60)) then
NOM_STRESS_Plate_top(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate(Fat_pts),panneau(nel_fatigue).iy_Plate(Fat_pts))= &
abs(Sy)
endif





!Bottom surface of plate


!sx (n/m2) - contr. longit.  sx  bordé (z=-d/2)
sx= sx_plaque_bott(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate(Fat_pts),panneau(nel_fatigue).iy_Plate(Fat_pts))

!sy (n/m2) - contr. transv.  sy  bordé (z=-d/2)     
sy= sy_plaque_bott(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate(Fat_pts),panneau(nel_fatigue).iy_Plate(Fat_pts))

!txy (n/m2) - contr. cisaill. txy bordé (z=-d/2)
Sxy=sxy_plaque_bott(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate(Fat_pts),panneau(nel_fatigue).iy_Plate(Fat_pts))


if ((abs(Sx)).LT.(abs(Sy)))  then
alfa_deg=abs(1./2.*atan(2*Sxy/(Sx-Sy))*180/pi)
endif

if ((abs(Sx)).GT.(abs(Sy)))  then
alfa_deg=abs(90 - 1./2.*atan(2*Sxy/(Sx-Sy))*180/pi);
endif

if (Sx.EQ.Sy) then
alfa_deg=0;
endif

if ((Sxy.EQ.0).and.(abs(Sx).LE.abs(Sy))) then
alfa_deg=0;
endif

if ((Sxy.EQ.0).and.((abs(Sx)).GT.(abs(Sy))))then
alfa_deg=90;
endif

if ((alfa_deg).LE.(60)) then

Sig1=(Sx+Sy)/2 + 1./2.*((Sx-Sy)**2 + 4*Sxy**2)**0.5;
Sig2=(Sx+Sy)/2 - 1./2.*((Sx-Sy)**2 + 4*Sxy**2)**0.5;  

NOM_STRESS_Plate_bott(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate(Fat_pts),panneau(nel_fatigue).iy_Plate(Fat_pts))= max(abs(Sig1),abs(Sig2))

endif

if ((alfa_deg).GT.(60)) then
NOM_STRESS_Plate_bott(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate(Fat_pts),panneau(nel_fatigue).iy_Plate(Fat_pts))= Sy
endif




!Maximum value evaluation
NOM_STRESS_Plate(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate(Fat_pts),panneau(nel_fatigue).iy_Plate(Fat_pts))=			&
max(abs(NOM_STRESS_Plate_bott(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate(Fat_pts),panneau(nel_fatigue).iy_Plate(Fat_pts))),	&
abs(NOM_STRESS_Plate_top(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate(Fat_pts),panneau(nel_fatigue).iy_Plate(Fat_pts))))&
/10**6	!pour passer en MPa		!fat_new




!	NOM_STRESS_Plate(1,nel_fatigue,panneau(nel_fatigue).ix_Plate(Fat_pts),panneau(nel_fatigue).iy_Plate(Fat_pts))=179.12
!	NOM_STRESS_Plate(2,nel_fatigue,panneau(nel_fatigue).ix_Plate(Fat_pts),panneau(nel_fatigue).iy_Plate(Fat_pts))=52.48
!	NOM_STRESS_Plate(3,nel_fatigue,panneau(nel_fatigue).ix_Plate(Fat_pts),panneau(nel_fatigue).iy_Plate(Fat_pts))=8.704
!	NOM_STRESS_Plate(4,nel_fatigue,panneau(nel_fatigue).ix_Plate(Fat_pts),panneau(nel_fatigue).iy_Plate(Fat_pts))=15.371
!	NOM_STRESS_Plate(5,nel_fatigue,panneau(nel_fatigue).ix_Plate(Fat_pts),panneau(nel_fatigue).iy_Plate(Fat_pts))=175.419
!	NOM_STRESS_Plate(6,nel_fatigue,panneau(nel_fatigue).ix_Plate(Fat_pts),panneau(nel_fatigue).iy_Plate(Fat_pts))=120.035
!	NOM_STRESS_Plate(7,nel_fatigue,panneau(nel_fatigue).ix_Plate(Fat_pts),panneau(nel_fatigue).iy_Plate(Fat_pts))=21.284

!	NOM_STRESS_Plate(1,1,ix_Plate(1,Fat_pts),iy_Plate(1,Fat_pts))=179.12
!	NOM_STRESS_Plate(2,1,ix_Plate(1,Fat_pts),iy_Plate(1,Fat_pts))=52.48
!	NOM_STRESS_Plate(3,1,ix_Plate(1,Fat_pts),iy_Plate(1,Fat_pts))=8.704
!	NOM_STRESS_Plate(4,1,ix_Plate(1,Fat_pts),iy_Plate(1,Fat_pts))=15.371
!	NOM_STRESS_Plate(5,1,ix_Plate(1,Fat_pts),iy_Plate(1,Fat_pts))=175.419
!	NOM_STRESS_Plate(6,1,ix_Plate(1,Fat_pts),iy_Plate(1,Fat_pts))=120.035
!	NOM_STRESS_Plate(7,1,ix_Plate(1,Fat_pts),iy_Plate(1,Fat_pts))=21.284
!
!
!	NOM_STRESS_Plate(1,2,ix_Plate(2,Fat_pts),iy_Plate(2,Fat_pts))=10.223
!	NOM_STRESS_Plate(2,2,ix_Plate(2,Fat_pts),iy_Plate(2,Fat_pts))=100.92
!	NOM_STRESS_Plate(3,2,ix_Plate(2,Fat_pts),iy_Plate(2,Fat_pts))=36.349
!	NOM_STRESS_Plate(4,2,ix_Plate(2,Fat_pts),iy_Plate(2,Fat_pts))=47.594
!	NOM_STRESS_Plate(5,2,ix_Plate(2,Fat_pts),iy_Plate(2,Fat_pts))=12.632
!	NOM_STRESS_Plate(6,2,ix_Plate(2,Fat_pts),iy_Plate(2,Fat_pts))=168.646
!	NOM_STRESS_Plate(7,2,ix_Plate(2,Fat_pts),iy_Plate(2,Fat_pts))=5.562





END



!*****************************************************************************************

    SUBROUTINE PLATE_HotspotStress
	use param_section
	
!	SUBMODULE FAT_HotspotStress !
!	*************************** !


! HS_STRESS : HOT SPOT STRESS


	HS_STRESS_Plate(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate(Fat_pts),panneau(nel_fatigue).iy_Plate(Fat_pts))=	&
	panneau(nel_fatigue).KT_Plate(Fat_pts)*NOM_STRESS_Plate(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate(Fat_pts),panneau(nel_fatigue).iy_Plate(Fat_pts))



       END
	 
!*****************************************************************************************


    SUBROUTINE PLATE_NotchStress
	use param_section

!	 SUBMODULE FAT_NotchStress !
!	************************* !
!




! NS_STRESS : NOTCH STRESS



if (method_fatigue .eq.1) then	!DNV method_fatigue			

	panneau(nel_fatigue).Kw_Plate(Fat_pts)=1.5

elseif (method_fatigue .eq.2) then	!BV method_fatigue			

	panneau(nel_fatigue).Kw_Plate(Fat_pts)=panneau(nel_fatigue).LAMBDA_Plate(Fat_pts)*SQRT(panneau(nel_fatigue).TETA_Plate(Fat_pts)/30)

endif


! NOTCH STRESS NS_STRESS

	NS_STRESS_Plate(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate(Fat_pts),panneau(nel_fatigue).iy_Plate(Fat_pts))=	&
	0.7*panneau(nel_fatigue).Kw_Plate(Fat_pts)*HS_STRESS_Plate(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate(Fat_pts),panneau(nel_fatigue).iy_Plate(Fat_pts))


if (method_fatigue .eq.2) then	!BV method_fatigue	


	IF (NS_STRESS_Plate(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate(Fat_pts),panneau(nel_fatigue).iy_Plate(Fat_pts)).EQ.0) GOTO 2

	kc_Plate(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate(Fat_pts),panneau(nel_fatigue).iy_Plate(Fat_pts))=	&
	0.4*(sigy(indMateriau(nel_fatigue))/NS_STRESS_Plate(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate(Fat_pts),panneau(nel_fatigue).iy_Plate(Fat_pts))) + 0.6

	IF (kc_Plate(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate(Fat_pts),panneau(nel_fatigue).iy_Plate(Fat_pts)).LE.0.8) THEN 								

	kc_Plate(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate(Fat_pts),panneau(nel_fatigue).iy_Plate(Fat_pts))=0.800											

	ELSEIF(kc_Plate(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate(Fat_pts),panneau(nel_fatigue).iy_Plate(Fat_pts)).GT.1.) THEN							

	kc_Plate(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate(Fat_pts),panneau(nel_fatigue).iy_Plate(Fat_pts))=1.												

	ENDIF
																		
2	CONTINUE

! NOTCH STRESS NS_STRESS	BV method_fatigue
	NS_STRESS_Plate(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate(Fat_pts),panneau(nel_fatigue).iy_Plate(Fat_pts))=	&
	kc_Plate(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate(Fat_pts),panneau(nel_fatigue).iy_Plate(Fat_pts))*	&
	NS_STRESS_Plate(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate(Fat_pts),panneau(nel_fatigue).iy_Plate(Fat_pts))

endif	!BV method_fatigue	
		
	END




!*****************************************************************************************

    SUBROUTINE PLATE_Damage
	use param_section
	real*8 rezult_gamma,rezult_1,rezult_2,rezult_3,ksi,nu,mu,h_Weibull_Plate

!	integer m

!  SUBMODEL DAMAGE_SUM.m  !
! *********************** !
 

!	 CHECKING OF STRESS VALUES and weibull factor

	if (NS_STRESS_Plate(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate(Fat_pts),panneau(nel_fatigue).iy_Plate(Fat_pts)).LT.0) THEN
	WRITE(*,*) 'Error, notch stress range is negative'
		pause 'stop'
		stop
	ENDIF

	if ((panneau(nel_fatigue).WEIB_FACT_Plate(Fat_pts).LT.0).OR.(panneau(nel_fatigue).WEIB_FACT_Plate(Fat_pts).GT.4)) THEN
	WRITE(*,*) 'Error, Weib_fact is negative or greater than 4'
		pause 'stop'
		stop
	ENDIF


	if (method_fatigue .eq.1) then	!DNV method_fatigue

!	basic shape parameter
	h0 = 2.21 - 0.54*log10(length)		 

!	additional factor depending on motion response period
	ha = 0.05 !in general
!	ha = 0.00 !for plating subjected to forces related to roll motions for vessels with roll period TR > 14 sec.

!	Calcul de TA : periode moyenne, en secondes	
	TA=4*LOG10(length)

!	Number of load cycles during design life
	Ntcyc=0.85*6.3*(10**8.0)/TA


!	 CHECK 

	IF (Ntcyc.LT.1.) THEN
	WRITE(*,*) 'Input value error: Invalid number of load cycles (Ntcyc<1)'
		PAUSE 'STOP'
		STOP
	ENDIF

!	h_Weibull : Weibull shape parameter


!	z(nel_fatigue,3) ! coord  y du noeud depart
!	z(nel_fatigue,4) ! coord  y du noeud d'arrivee
!	znel_fatigue = vertical distance from baseline to considered longitudinal (m)

		znel_fatigue=abs(z(nel_fatigue,3)+(z(nel_fatigue,4)-z(nel_fatigue,3))*(panneau(nel_fatigue).iy_Plate(Fat_pts)-1)/2)/1000

	if (panneau(nel_fatigue).WEIB_FACT_Plate(Fat_pts).EQ.1) then
		
		h_Weibull_Plate = h0																	!For deck longitudinals

	elseif (panneau(nel_fatigue).WEIB_FACT_Plate(Fat_pts).EQ.2) then

		if ((znel_fatigue.GT.Tirant(Fat_NNSOL(caschge_fatigue))).AND.(znel_fatigue.LT.DEPTH)) THEN			!For ship side
		h_Weibull_Plate = h0 + ha*( DEPTH - znel_fatigue ) / ( DEPTH - Tirant(Fat_NNSOL(caschge_fatigue)))				!For ship side above the waterline Tirant < znel_fatigue <DEPTH
	
		elseif (znel_fatigue .EQ.Tirant(Fat_NNSOL(caschge_fatigue))) then
		h_Weibull_Plate = h0 + ha																	!For ship side at the waterline znel_fatigue = Tirant

		elseif (znel_fatigue.LT.Tirant(Fat_NNSOL(caschge_fatigue))) then
		h_Weibull_Plate = h0 + ha*znel_fatigue/Tirant(Fat_NNSOL(caschge_fatigue))-0.005*(Tirant(Fat_NNSOL(caschge_fatigue))-znel_fatigue)		!For ship side below the waterline znel_fatigue < Tirant
		
		ENDIF

	elseif (panneau(nel_fatigue).WEIB_FACT_Plate(Fat_pts).EQ.3) then
	h_Weibull_Plate = h0 - 0.005*Tirant(Fat_NNSOL(caschge_fatigue))											!For bottom longitudinals 
	
	elseif (panneau(nel_fatigue).WEIB_FACT_Plate(Fat_pts).EQ.4) then
	h_Weibull_Plate = h0 + ha																!For longitudinal and transverse bulkheads
	endif


!	CALCULATION OF DAMAGE SUM D
	CALL gamma_complete((panneau(nel_fatigue).m_Plate(Fat_pts)/h_Weibull_Plate),rezult_gamma)

	Damage_Plate(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate(Fat_pts),panneau(nel_fatigue).iy_Plate(Fat_pts))=	&
	Ntcyc/panneau(nel_fatigue).C_Plate(Fat_pts)*(NS_STRESS_Plate(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate(Fat_pts),panneau(nel_fatigue).iy_Plate(Fat_pts))**panneau(nel_fatigue).m_Plate(Fat_pts))*	&
	(LOG(10.**8))**(-panneau(nel_fatigue).m_Plate(Fat_pts)/h_Weibull_Plate)*rezult_gamma;


 
!*******************************************************************************
	
elseif(method_fatigue.EQ.2) then		!BV method_fatigue


!	probabilité d'excédence 
	PR=10.**(-5.)

!	Calcul de TA : periode moyenne, en secondes	
	TA=4*LOG10(length)

!	alfa0 : facteur de navigation pris égal à 0.8
	alfa0=0.85

!	Calcul de Ntcyc	: nombre total des cycles
	Ntcyc=631*alfa0/TA*10**6.0

!	Calcul de Ntfl (Tfl:Increased design life (between 25 and 40))
	Ntfl=31.55*alfa0*Tfl*10**6.0/TA
	
	IF(Tfl.GE.25) THEN
	CFL=LOG10(0.2*LOG10(Ntfl))/LOG10(0.2*LOG10(Ntcyc))
	else
	CFL=1.
	endif

!	Calcul de ksi,ksi0 : paramêtres de forme de la fonction de WEIBULL
	ksi0=(73-0.07*length)/60*CFL
	IF(ksi0.LT.0.85) THEN
	ksi0=0.85
	ENDIF


!	CALL ANNUL2(Damage_Stif,nsolm_fatigue,NETO,ix_max,iy_max)
!	CALL ANNUL2(Damage_Plate,nsolm_fatigue,NETO,ix_max,iy_max)
!	CALL ANNUL2(Damage_Plate_frame,nsolm_fatigue,NETO,ix_max,iy_max)
!	CALL ANNUL2(Damage_frame,nsolm_fatigue,NETO,ix_max,iy_max)
!	CALL ANNUL2(Damage_Girder,nsolm_fatigue,NETO,ix_max,mt_max)
!	CALL ANNUL2(Damage_Pilar,nsolm_fatigue,NETO,ix_max,iy_max)


!	CALL ANNUL4(dD_1,Neto,9,nsolm_fatigue,Neto)
!	CALL ANNUL4(dD_2,Neto,9,nsolm_fatigue,Neto)
!	CALL ANNUL4(dD_3,Neto,9,nsolm_fatigue,Neto)
!	CALL ANNUL4(dD_4,Neto,9,nsolm_fatigue,Neto)

!	if(Sty.EQ.1) then

!	damage : Hot spot 1
			

!     characteristics if the stiffened elements (epsa,entr,delta,hxr,dxr,wxr,txr,sigy(indMateriau(nel_fatigue)),sigy(indMateriau(nel_fatigue)))
!     ------------------------------------------
!  .  sl = epsa : length of stiffener between transverse frames (m.)
!  .  bp = entr : longitudinal stiffener spacing   (m.)
!  .  tp = delta: thickness of parent plate        (m.)
!  .  hw = hxr  : height of stiffener web          (m.)
!  .  tw = dxr  : thickness of stiffener web       (m.)
!  .  bf = wxr  : breadth of stiffener flange      (m.)
!  .  tf = txr  : thickness of stiffener flange    (m.)
!  .  syp= sigy(indMateriau(nel_fatigue)) : yield strength of parent plate   (n/m2)   
!  .  syw= sigy(indMateriau(nel_fatigue)) : yield strength of stiffener      (n/m2)   

!   les variables de conception  ; nbrxi = nvar(nel_fatigue,iboat)
!   -------------------------------------------------
!       1       delta =  épaisseur du bordage
!       2       hya   =  hauteur de l'âme des cadres   
!       3       dya   =  épaisseur de l'âme des cadres   
!       4       wya   =  largeur des semelles des cadres   
!       5       epsa  =  entredistance entre cadres   
!       6       hxr   =  hauteur de l'âme des raidisseurs
!       7       dxr   =  épaisseur de l'âme des raidisseurs
!       8       wxr   =  largeur des semelles des raidisseurs
!       9       epsr  =  entredistance entre raidisseurs

! 	autres paramètres :
!	 	tya = épaisseur semelle aiguilles
!     	txr = épaisseur semelle raidisseurs 
!
!  	Les variables de conception pour epontille
!     ------------------------------------------					
!	1	hya		=	hauteur demi âme ou diam ou côté ext.	
!	2	dya		=	épaisseur d'âme							
!	3	wya		=	largeur de semelle						
!	4	epais	=	épaisseur de paroi mince				
!	5	epsa	=	entredistance entre épontillesc


!	thF	: épaisseur de l'élément considéré (mm) (minimum thickness of connected elements)

	znel_fatigue=abs(z(nel_fatigue,3)+(z(nel_fatigue,4)-z(nel_fatigue,3))*(panneau(nel_fatigue).iy_Plate(Fat_pts)-1)/2)/1000

	thF=panneau(nel_fatigue).txr*1000	!plate's thickness 

!	Calcul de kpSN : constante de la Design S-N curve
	IF(thF.LT.22) THEN
	kpSN=5.802*(10**12.0)
	ELSE
	kpSN=5.802*(10**12.0)*(22.0/thF)**(0.9)
	ENDIF

!	Calcul de SigmaQ : étendue(sigmaMAx-sigmaMin) de contrainte à l'intersection des 02 segments de la courbe S-N.
	SigmaQ=(kpSN*(10**(-7.0)))**(1.0/3.0)

	ksi=ksi0*(1.04-0.14*ABS(znel_fatigue-Tirant(Fat_NNSOL(caschge_fatigue)))/(DEPTH-Tirant(Fat_NNSOL(caschge_fatigue))))

!	CALL ANNULD(nu,nsolm_fatigue)

	nu=0.0

	if (NS_STRESS_Plate(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate(Fat_pts),panneau(nel_fatigue).iy_Plate(Fat_pts)).EQ.0) GOTO 10

!	nu : coefficient utilisé pour le calcul de la fonction GAMMA incomplète.
	nu=-((SigmaQ/NS_STRESS_Plate(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate(Fat_pts),panneau(nel_fatigue).iy_Plate(Fat_pts)))**(ksi))*LOG(PR)					

!	rezult_1 et rezult_2:résultats de la fct gamma incomplète
	call gamma_incomplete(3/ksi,nu,rezult_1)						
	call gamma_incomplete(5/ksi,nu,rezult_2)							
!	rezult_3 :résultat de la fct gamma complète
	CALL gamma_complete(3/ksi,rezult_3)

!	mu : coefficient qui tient en compte du changement de pente de la courbe S-N
	mu=1-(((rezult_1)-(rezult_2*(nu**(-2.0/ksi))))/rezult_3)		
	
!	DAMAGE		

	Damage_Plate(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate(Fat_pts),panneau(nel_fatigue).iy_Plate(Fat_pts))=	&
	Ntcyc*(NS_STRESS_Plate(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate(Fat_pts),panneau(nel_fatigue).iy_Plate(Fat_pts))**3.)	&
	/(kpSN*(-LOG(PR))**(3./ksi))*mu*rezult_3
A=1

ENDIF

10	CONTINUE
	
end














!****************************************************************************************
!****************************************************************************************
!****************************************************************************************
	OPTIONS /EXTEND
	SUBROUTINE FATIGUE_PLATE_FRAME
	use param_section


!	CALL FAT_Check_Input


!    CALCULATION OF NOMINALS STRESSES  -------------------------------------
!    The submodule calculates nominal stress ranges at critical locations 
!    of structural member based on beam or plate theory. 
	call PLATE_FRAME_NominalStress


!	 CALCULATION OF HOT-SPOT STRESSES --------------------------------------
!	 The submodele calculates structural stress ranges based on 
!	 semi-analytical approaches for stress concentration factors. 
!	 Library of different developed solutions is applied.
	call PLATE_FRAME_HotspotStress



!	CALCULATION OF NOTCH STRESSES -----------------------------------------
!	The submodele calculates notch stress range with the help of additional 
!	stress concentration factor, which defines the relationship between 
!	hot-spot and notch stress.
	call PLATE_FRAME_NotchStress

!	CALCULATION OF DAMAGE SUM----------------------------------------------
!	The submodule calculates damage sum based on linear damage theory and
!	Palmer-Miner rule. Submodel uses S-N curve, which includes the fatigue 
!	reduction and uncertainness due to dimensional accuracy, welding 
!	distortion, and environment. (FOR EACH LOAD CASE)
!if ((caschge_fatigue.EQ.1).AND.(nel_fatigue.EQ.2)) PAUSE
	call PLATE_FRAME_Damage

	end


!*****************************************************************************************
      SUBROUTINE PLATE_FRAME_NominalStress
	use param_section
!      real*8 PI
	
!	SUBMODULE NOMINAL_STRESS
!	************************** 



!NOM_STRESS_Plate_Frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate_Frame(Fat_pts),panneau(nel_fatigue).iy_Plate_Frame(Fat_pts))=


!Top surface of plate
!sx (n/m2) - contr. longit.  sx  bordé (z=+d/2)
sx_top= sx_plaque_top(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate_Frame(Fat_pts),panneau(nel_fatigue).iy_Plate_Frame(Fat_pts))

!sy cadre (jab)- contr. jonct. ame-borde, cadre    
sy= sy_cadre_jab(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate_Frame(Fat_pts),panneau(nel_fatigue).iy_Plate_Frame(Fat_pts))

!tyz cadre (jab)- tau (shear)jonct ame-borde,cadre
Sxy=sxy_cadre_jab(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate_Frame(Fat_pts),panneau(nel_fatigue).iy_Plate_Frame(Fat_pts))


Sig1_top=(sx_top+Sy)/2 + 1./2.*((sx_top-Sy)**2 + 4*Sxy**2)**0.5;
Sig2_top=(sx_top+Sy)/2 - 1./2.*((sx_top-Sy)**2 + 4*Sxy**2)**0.5;  

NOM_STRESS_Plate_Frame_top(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate_Frame(Fat_pts),panneau(nel_fatigue).iy_Plate_Frame(Fat_pts)) &
= max(abs(Sig1_top),abs(Sig2_top))


!Bottom surface of plate
!sx_bott (n/m2) - contr. longit.  sx_bott  bordé (z=-d/2)
sx_bott= sx_plaque_bott(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate_Frame(Fat_pts),panneau(nel_fatigue).iy_Plate_Frame(Fat_pts))

Sig1_bott=(sx_bott+Sy)/2 + 1./2.*((sx_bott-Sy)**2 + 4*Sxy**2)**0.5;
Sig2_bott=(sx_bott+Sy)/2 - 1./2.*((sx_bott-Sy)**2 + 4*Sxy**2)**0.5;  

NOM_STRESS_Plate_Frame_bott(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate_Frame(Fat_pts),panneau(nel_fatigue).iy_Plate_Frame(Fat_pts))	&
= max(abs(Sig1_bott),abs(Sig2_bott))



!Maximum value evaluation
NOM_STRESS_Plate_Frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate_Frame(Fat_pts),panneau(nel_fatigue).iy_Plate_Frame(Fat_pts))	&
=max(abs(NOM_STRESS_Plate_Frame_top(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate_Frame(Fat_pts),panneau(nel_fatigue).iy_Plate_Frame(Fat_pts))),	&
 abs(NOM_STRESS_Plate_Frame_bott(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate_Frame(Fat_pts),panneau(nel_fatigue).iy_Plate_Frame(Fat_pts))))&
/10**6	!pour passer en MPa		!fat_new

!	NOM_STRESS_Plate_Frame(1,nel_fatigue,panneau(nel_fatigue).ix_Plate_Frame(Fat_pts),panneau(nel_fatigue).iy_Plate_Frame(Fat_pts))=179.12
!	NOM_STRESS_Plate_Frame(2,nel_fatigue,panneau(nel_fatigue).ix_Plate_Frame(Fat_pts),panneau(nel_fatigue).iy_Plate_Frame(Fat_pts))=52.48
!	NOM_STRESS_Plate_Frame(3,nel_fatigue,panneau(nel_fatigue).ix_Plate_Frame(Fat_pts),panneau(nel_fatigue).iy_Plate_Frame(Fat_pts))=8.704
!	NOM_STRESS_Plate_Frame(4,nel_fatigue,panneau(nel_fatigue).ix_Plate_Frame(Fat_pts),panneau(nel_fatigue).iy_Plate_Frame(Fat_pts))=15.371
!	NOM_STRESS_Plate_Frame(5,nel_fatigue,panneau(nel_fatigue).ix_Plate_Frame(Fat_pts),panneau(nel_fatigue).iy_Plate_Frame(Fat_pts))=175.419
!	NOM_STRESS_Plate_Frame(6,nel_fatigue,panneau(nel_fatigue).ix_Plate_Frame(Fat_pts),panneau(nel_fatigue).iy_Plate_Frame(Fat_pts))=120.035
!	NOM_STRESS_Plate_Frame(7,nel_fatigue,panneau(nel_fatigue).ix_Plate_Frame(Fat_pts),panneau(nel_fatigue).iy_Plate_Frame(Fat_pts))=21.284




!	NOM_STRESS_Plate_Frame(1,1,ix_Plate_Frame(1,Fat_pts),iy_Plate_Frame(1,Fat_pts))=179.12
!	NOM_STRESS_Plate_Frame(2,1,ix_Plate_Frame(1,Fat_pts),iy_Plate_Frame(1,Fat_pts))=52.48
!	NOM_STRESS_Plate_Frame(3,1,ix_Plate_Frame(1,Fat_pts),iy_Plate_Frame(1,Fat_pts))=8.704
!	NOM_STRESS_Plate_Frame(4,1,ix_Plate_Frame(1,Fat_pts),iy_Plate_Frame(1,Fat_pts))=15.371
!	NOM_STRESS_Plate_Frame(5,1,ix_Plate_Frame(1,Fat_pts),iy_Plate_Frame(1,Fat_pts))=175.419
!	NOM_STRESS_Plate_Frame(6,1,ix_Plate_Frame(1,Fat_pts),iy_Plate_Frame(1,Fat_pts))=120.035
!	NOM_STRESS_Plate_Frame(7,1,ix_Plate_Frame(1,Fat_pts),iy_Plate_Frame(1,Fat_pts))=21.284
!
!
!	NOM_STRESS_Plate_Frame(1,2,ix_Plate_Frame(2,Fat_pts),iy_Plate_Frame(2,Fat_pts))=10.223
!	NOM_STRESS_Plate_Frame(2,2,ix_Plate_Frame(2,Fat_pts),iy_Plate_Frame(2,Fat_pts))=100.92
!	NOM_STRESS_Plate_Frame(3,2,ix_Plate_Frame(2,Fat_pts),iy_Plate_Frame(2,Fat_pts))=36.349
!	NOM_STRESS_Plate_Frame(4,2,ix_Plate_Frame(2,Fat_pts),iy_Plate_Frame(2,Fat_pts))=47.594
!	NOM_STRESS_Plate_Frame(5,2,ix_Plate_Frame(2,Fat_pts),iy_Plate_Frame(2,Fat_pts))=12.632
!	NOM_STRESS_Plate_Frame(6,2,ix_Plate_Frame(2,Fat_pts),iy_Plate_Frame(2,Fat_pts))=168.646
!	NOM_STRESS_Plate_Frame(7,2,ix_Plate_Frame(2,Fat_pts),iy_Plate_Frame(2,Fat_pts))=5.562


END


!*****************************************************************************************

    SUBROUTINE PLATE_FRAME_HotspotStress
	use param_section
	
!	SUBMODULE FAT_HotspotStress !
!	*************************** !


! HS_STRESS : HOT SPOT STRESS



	HS_STRESS_Plate_Frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate_Frame(Fat_pts),panneau(nel_fatigue).iy_Plate_Frame(Fat_pts))=	&
	panneau(nel_fatigue).KT_Plate_Frame(Fat_pts)*NOM_STRESS_Plate_Frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate_Frame(Fat_pts),panneau(nel_fatigue).iy_Plate_Frame(Fat_pts))


       END
	 
!*****************************************************************************************


    SUBROUTINE PLATE_FRAME_NotchStress
	use param_section

!	 SUBMODULE FAT_NotchStress !
!	************************* !
!




! NS_STRESS : NOTCH STRESS


if (method_fatigue .eq.1) then	!DNV method_fatigue			

	panneau(nel_fatigue).Kw_Plate_Frame(Fat_pts)=1.5

elseif (method_fatigue .eq.2) then	!BV method_fatigue			

	panneau(nel_fatigue).Kw_Plate_Frame(Fat_pts)=panneau(nel_fatigue).LAMBDA_Plate_Frame(Fat_pts)*SQRT(panneau(nel_fatigue).TETA_Plate_Frame(Fat_pts)/30)

endif


! NOTCH STRESS NS_STRESS

	NS_STRESS_Plate_Frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate_Frame(Fat_pts),panneau(nel_fatigue).iy_Plate_Frame(Fat_pts))=	&
	0.7*panneau(nel_fatigue).Kw_Plate_Frame(Fat_pts)*HS_STRESS_Plate_Frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate_Frame(Fat_pts),panneau(nel_fatigue).iy_Plate_Frame(Fat_pts))

if (method_fatigue .eq.2) then	!BV method_fatigue	


	IF (NS_STRESS_Plate_Frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate_Frame(Fat_pts),panneau(nel_fatigue).iy_Plate_Frame(Fat_pts)).EQ.0) GOTO 3

	kc_Plate_Frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate_Frame(Fat_pts),panneau(nel_fatigue).iy_Plate_Frame(Fat_pts))=	&
	0.4*(sigy(indMateriau(nel_fatigue))/NS_STRESS_Plate_Frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate_Frame(Fat_pts),panneau(nel_fatigue).iy_Plate_Frame(Fat_pts))) + 0.6

	IF (kc_Plate_Frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate_Frame(Fat_pts),panneau(nel_fatigue).iy_Plate_Frame(Fat_pts)).LE.0.8) THEN 								

	kc_Plate_Frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate_Frame(Fat_pts),panneau(nel_fatigue).iy_Plate_Frame(Fat_pts))=0.800											

	ELSEIF(kc_Plate_Frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate_Frame(Fat_pts),panneau(nel_fatigue).iy_Plate_Frame(Fat_pts)).GT.1.) THEN							

	kc_Plate_Frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate_Frame(Fat_pts),panneau(nel_fatigue).iy_Plate_Frame(Fat_pts))=1.												

	ENDIF
																		
3	CONTINUE

! NOTCH STRESS NS_STRESS	BV method_fatigue
	NS_STRESS_Plate_Frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate_Frame(Fat_pts),panneau(nel_fatigue).iy_Plate_Frame(Fat_pts))=	&
	kc_Plate_Frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate_Frame(Fat_pts),panneau(nel_fatigue).iy_Plate_Frame(Fat_pts))*	&
	NS_STRESS_Plate_Frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate_Frame(Fat_pts),panneau(nel_fatigue).iy_Plate_Frame(Fat_pts))

ENDIF	!BV method_fatigue															
		
	END



!*****************************************************************************************

    SUBROUTINE PLATE_FRAME_Damage
	use param_section
	real*8 rezult_gamma,rezult_1,rezult_2,rezult_3,ksi,nu,mu,temp_

!	integer m

!  SUBMODEL DAMAGE_SUM.m  !
! *********************** !
 


!	 CHECKING OF STRESS VALUES and weibull factor


	if (NS_STRESS_Plate_Frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate_Frame(Fat_pts),panneau(nel_fatigue).iy_Plate_Frame(Fat_pts)).LT.0) THEN
	WRITE(*,*) 'Error, notch stress range is negative'
		pause 'stop'
		stop
	ENDIF

	if ((panneau(nel_fatigue).WEIB_FACT_Plate_Frame(Fat_pts).LT.0).OR.(panneau(nel_fatigue).WEIB_FACT_Plate_Frame(Fat_pts).GT.4)) THEN
	WRITE(*,*) 'Error, Weib_fact is negative or greater than 4'
		pause 'stop'
		stop
	ENDIF


if (method_fatigue .eq.1) then	!DNV method_fatigue

!	basic shape parameter
	h0 = 2.21 - 0.54*log10(length)		 

!	additional factor depending on motion response period
	ha = 0.05 !in general
!	ha = 0.00 !for plating subjected to forces related to roll motions for vessels with roll period TR > 14 sec.

!	Calcul de TA : periode moyenne, en secondes	
	TA=4*LOG10(length)

!	Number of load cycles during design life
	Ntcyc=0.85*6.3*(10**8.0)/TA


!	 CHECK 

	IF (Ntcyc.LT.1.) THEN
	WRITE(*,*) 'Input value error: Invalid number of load cycles (Ntcyc<1)'
		PAUSE 'STOP'
		STOP
	ENDIF

!	h_Weibull : Weibull shape parameter


!	z(nel_fatigue,3) ! coord  y du noeud depart
!	z(nel_fatigue,4) ! coord  y du noeud d'arrivee
!	znel_fatigue = vertical distance from baseline to considered longitudinal (m)


		znel_fatigue=abs(z(nel_fatigue,3)+(z(nel_fatigue,4)-z(nel_fatigue,3))*(panneau(nel_fatigue).iy_Plate_Frame(Fat_pts)-1)/2)/1000

	if (panneau(nel_fatigue).WEIB_FACT_Plate_Frame(Fat_pts).EQ.1) then
		
		h_Weibull_Plate_Frame = h0																	!For deck longitudinals

	elseif (panneau(nel_fatigue).WEIB_FACT_Plate_Frame(Fat_pts).EQ.2) then

		if ((znel_fatigue.GT.Tirant(Fat_NNSOL(caschge_fatigue))).AND.(znel_fatigue.LT.DEPTH)) THEN			!For ship side
		h_Weibull_Plate_Frame = h0 + ha*( DEPTH - znel_fatigue ) / ( DEPTH - Tirant(Fat_NNSOL(caschge_fatigue)))				!For ship side above the waterline Tirant < znel_fatigue <DEPTH
	
		elseif (znel_fatigue .EQ.Tirant(Fat_NNSOL(caschge_fatigue))) then
		h_Weibull_Plate_Frame = h0 + ha																	!For ship side at the waterline znel_fatigue = Tirant

		elseif (znel_fatigue.LT.Tirant(Fat_NNSOL(caschge_fatigue))) then
		h_Weibull_Plate_Frame = h0 + ha*znel_fatigue/Tirant(Fat_NNSOL(caschge_fatigue))-0.005*(Tirant(Fat_NNSOL(caschge_fatigue))-znel_fatigue)		!For ship side below the waterline znel_fatigue < Tirant
		
		ENDIF

	elseif (panneau(nel_fatigue).WEIB_FACT_Plate_Frame(Fat_pts).EQ.3) then
	h_Weibull_Plate_Frame = h0 - 0.005*Tirant(Fat_NNSOL(caschge_fatigue))											!For bottom longitudinals 
	
	elseif (panneau(nel_fatigue).WEIB_FACT_Plate_Frame(Fat_pts).EQ.4) then
	h_Weibull_Plate_Frame = h0 + ha																!For longitudinal and transverse bulkheads
	endif

!	CALCULATION OF DAMAGE SUM D
	!CALL gamma_complete((panneau(nel_fatigue).m_Plate_Frame(Fat_pts)/h_Weibull_Plate_Frame),rezult_gamma)
	CALL gamma_complete(temp_,rezult_gamma)

	Damage_Plate_Frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate_Frame(Fat_pts),panneau(nel_fatigue).iy_Plate_Frame(Fat_pts))=	&
	Ntcyc/panneau(nel_fatigue).C_Plate_Frame(Fat_pts)*(NS_STRESS_Plate_Frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate_Frame(Fat_pts),panneau(nel_fatigue).iy_Plate_Frame(Fat_pts))**panneau(nel_fatigue).m_Plate_Frame(Fat_pts))*	&
	(LOG(10.**8))**(-panneau(nel_fatigue).m_Plate_Frame(Fat_pts)/h_Weibull_Plate_Frame)*rezult_gamma;



!*******************************************************************************
	
elseif(method_fatigue.EQ.2) then		!BV method_fatigue


!	probabilité d'excédence 
	PR=10.**(-5.)

!	Calcul de TA : periode moyenne, en secondes	
	TA=4*LOG10(length)

!	alfa0 : facteur de navigation pris égal à 0.8
	alfa0=0.85

!	Calcul de Ntcyc	: nombre total des cycles
	Ntcyc=631*alfa0/TA*10**6.0

!	Calcul de Ntfl (Tfl:Increased design life (between 25 and 40))
	Ntfl=31.55*alfa0*Tfl*10**6.0/TA
	
	IF(Tfl.GE.25) THEN
	CFL=LOG10(0.2*LOG10(Ntfl))/LOG10(0.2*LOG10(Ntcyc))
	else
	CFL=1.
	endif

!	Calcul de ksi,ksi0 : paramêtres de forme de la fonction de WEIBULL
	ksi0=(73-0.07*length)/60*CFL
	IF(ksi0.LT.0.85) THEN
	ksi0=0.85
	ENDIF


!	CALL ANNUL2(Damage_Stif,nsolm_fatigue,NETO,ix_max,iy_max)
!	CALL ANNUL2(Damage_Plate,nsolm_fatigue,NETO,ix_max,iy_max)
!	CALL ANNUL2(Damage_Plate_frame,nsolm_fatigue,NETO,ix_max,iy_max)
!	CALL ANNUL2(Damage_frame,nsolm_fatigue,NETO,ix_max,iy_max)
!	CALL ANNUL2(Damage_Girder,nsolm_fatigue,NETO,ix_max,mt_max)
!	CALL ANNUL2(Damage_Pilar,nsolm_fatigue,NETO,ix_max,iy_max)


!	CALL ANNUL4(dD_1,Neto,9,nsolm_fatigue,Neto)
!	CALL ANNUL4(dD_2,Neto,9,nsolm_fatigue,Neto)
!	CALL ANNUL4(dD_3,Neto,9,nsolm_fatigue,Neto)
!	CALL ANNUL4(dD_4,Neto,9,nsolm_fatigue,Neto)

!	if(Sty.EQ.1) then

!	damage : Hot spot 1
			

!     characteristics if the stiffened elements (epsa,entr,delta,hxr,dxr,wxr,txr,sigy(indMateriau(nel_fatigue)),sigy(indMateriau(nel_fatigue)))
!     ------------------------------------------
!  .  sl = epsa : length of stiffener between transverse frames (m.)
!  .  bp = entr : longitudinal stiffener spacing   (m.)
!  .  tp = delta: thickness of parent plate        (m.)
!  .  hw = hxr  : height of stiffener web          (m.)
!  .  tw = dxr  : thickness of stiffener web       (m.)
!  .  bf = wxr  : breadth of stiffener flange      (m.)
!  .  tf = txr  : thickness of stiffener flange    (m.)
!  .  syp= sigy(indMateriau(nel_fatigue)) : yield strength of parent plate   (n/m2)   
!  .  syw= sigy(indMateriau(nel_fatigue)) : yield strength of stiffener      (n/m2)   

!   les variables de conception  ; nbrxi = nvar(nel_fatigue,iboat)
!   -------------------------------------------------
!       1       delta =  épaisseur du bordage
!       2       hya   =  hauteur de l'âme des cadres   
!       3       dya   =  épaisseur de l'âme des cadres   
!       4       wya   =  largeur des semelles des cadres   
!       5       epsa  =  entredistance entre cadres   
!       6       hxr   =  hauteur de l'âme des raidisseurs
!       7       dxr   =  épaisseur de l'âme des raidisseurs
!       8       wxr   =  largeur des semelles des raidisseurs
!       9       epsr  =  entredistance entre raidisseurs

! 	autres paramètres :
!	 	tya = épaisseur semelle aiguilles
!     	txr = épaisseur semelle raidisseurs 
!
!  	Les variables de conception pour epontille
!     ------------------------------------------					
!	1	hya		=	hauteur demi âme ou diam ou côté ext.	
!	2	dya		=	épaisseur d'âme							
!	3	wya		=	largeur de semelle						
!	4	epais	=	épaisseur de paroi mince				
!	5	epsa	=	entredistance entre épontillesc


!	thF	: épaisseur de l'élément considéré (mm) (minimum thickness of connected elements)

!	dya		épaisseur de l'âme des cadres
!	delta	thickness of parent plate

	znel_fatigue=abs(z(nel_fatigue,3)+(z(nel_fatigue,4)-z(nel_fatigue,3))*(panneau(nel_fatigue).iy_Plate_Frame(Fat_pts)-1)/2)/1000
	
	if (panneau(nel_fatigue).dya .lt. panneau(nel_fatigue).txr) then
	thF=panneau(nel_fatigue).dya*1000		
	else 
	thF=panneau(nel_fatigue).txr*1000
	endif


!	Calcul de kpSN : constante de la Design S-N curve
	IF(thF.LT.22) THEN
	kpSN=5.802*(10**12.0)
	ELSE
	kpSN=5.802*(10**12.0)*(22.0/thF)**(0.9)
	ENDIF

!	Calcul de SigmaQ : étendue(sigmaMAx-sigmaMin) de contrainte à l'intersection des 02 segments de la courbe S-N.
	SigmaQ=(kpSN*(10**(-7.0)))**(1.0/3.0)

	ksi=ksi0*(1.04-0.14*ABS(znel_fatigue-Tirant(Fat_NNSOL(caschge_fatigue)))/(DEPTH-Tirant(Fat_NNSOL(caschge_fatigue))))

!	CALL ANNULD(nu,nsolm_fatigue)

	nu=0.0

	if (NS_STRESS_Plate_Frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate_Frame(Fat_pts),panneau(nel_fatigue).iy_Plate_Frame(Fat_pts)).EQ.0) GOTO 10

!	nu : coefficient utilisé pour le calcul de la fonction GAMMA incomplète.
	nu=-((SigmaQ/NS_STRESS_Plate_Frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate_Frame(Fat_pts),panneau(nel_fatigue).iy_Plate_Frame(Fat_pts)))**(ksi))*LOG(PR)					

!	rezult_1 et rezult_2:résultats de la fct gamma incomplète

!if ((caschge_fatigue.EQ.1).AND.(nel_fatigue.EQ.2)) PAUSE
	call gamma_incomplete(3/ksi,nu,rezult_1)						
	call gamma_incomplete(5/ksi,nu,rezult_2)							
!	rezult_3 :résultat de la fct gamma complète
	CALL gamma_complete(3/ksi,rezult_3)

!	mu : coefficient qui tient en compte du changement de pente de la courbe S-N
	mu=1-(((rezult_1)-(rezult_2*(nu**(-2.0/ksi))))/rezult_3)		
	

!	DAMAGE		

	Damage_Plate_Frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate_Frame(Fat_pts),panneau(nel_fatigue).iy_Plate_Frame(Fat_pts))=	&
	Ntcyc*(NS_STRESS_Plate_Frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate_Frame(Fat_pts),panneau(nel_fatigue).iy_Plate_Frame(Fat_pts))**3.)	&
				/(kpSN*(-LOG(PR))**(3./ksi))*mu*rezult_3


10	CONTINUE

ENDIF

	
	end




!****************************************************************************************
!****************************************************************************************
!****************************************************************************************
	OPTIONS /EXTEND
	SUBROUTINE FATIGUE_FRAME
	use param_section


!	CALL FAT_Check_Input


!    CALCULATION OF NOMINALS STRESSES  -------------------------------------
!    The submodule calculates nominal stress ranges at critical locations 
!    of structural member based on beam or plate theory. 
	call FRAME_NominalStress


!	 CALCULATION OF HOT-SPOT STRESSES --------------------------------------
!	 The submodele calculates structural stress ranges based on 
!	 semi-analytical approaches for stress concentration factors. 
!	 Library of different developed solutions is applied.
	call FRAME_HotspotStress



!	CALCULATION OF NOTCH STRESSES -----------------------------------------
!	The submodele calculates notch stress range with the help of additional 
!	stress concentration factor, which defines the relationship between 
!	hot-spot and notch stress.
	call FRAME_NotchStress

!	CALCULATION OF DAMAGE SUM----------------------------------------------
!	The submodule calculates damage sum based on linear damage theory and
!	Palmer-Miner rule. Submodel uses S-N curve, which includes the fatigue 
!	reduction and uncertainness due to dimensional accuracy, welding 
!	distortion, and environment. (FOR EACH LOAD CASE)
	call FRAME_Damage

	end


!*****************************************************************************************
    SUBROUTINE FRAME_NominalStress
	use param_section
	 
!      real*8 PI
	
!	SUBMODULE NOMINAL_STRESS
!	************************** 


!sy cadre (sem)- contrainte dans semelle, cadre

NOM_STRESS_Frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Frame(Fat_pts),panneau(nel_fatigue).iy_Frame(Fat_pts))=abs(	&
sy_cadre_sem(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Frame(Fat_pts),panneau(nel_fatigue).iy_Frame(Fat_pts)))&
/10**6	!pour passer en MPa		!fat_new


!	NOM_STRESS_Frame(1,nel_fatigue,panneau(nel_fatigue).ix_Frame(Fat_pts),panneau(nel_fatigue).iy_Frame(Fat_pts))=179.12
!	NOM_STRESS_Frame(2,nel_fatigue,panneau(nel_fatigue).ix_Frame(Fat_pts),panneau(nel_fatigue).iy_Frame(Fat_pts))=52.48
!	NOM_STRESS_Frame(3,nel_fatigue,panneau(nel_fatigue).ix_Frame(Fat_pts),panneau(nel_fatigue).iy_Frame(Fat_pts))=8.704
!	NOM_STRESS_Frame(4,nel_fatigue,panneau(nel_fatigue).ix_Frame(Fat_pts),panneau(nel_fatigue).iy_Frame(Fat_pts))=15.371
!	NOM_STRESS_Frame(5,nel_fatigue,panneau(nel_fatigue).ix_Frame(Fat_pts),panneau(nel_fatigue).iy_Frame(Fat_pts))=175.419
!	NOM_STRESS_Frame(6,nel_fatigue,panneau(nel_fatigue).ix_Frame(Fat_pts),panneau(nel_fatigue).iy_Frame(Fat_pts))=120.035
!	NOM_STRESS_Frame(7,nel_fatigue,panneau(nel_fatigue).ix_Frame(Fat_pts),panneau(nel_fatigue).iy_Frame(Fat_pts))=21.284

!	NOM_STRESS_Frame(1,1,ix_Frame(1,Fat_pts),iy_Frame(1,Fat_pts))=179.12
!	NOM_STRESS_Frame(2,1,ix_Frame(1,Fat_pts),iy_Frame(1,Fat_pts))=52.48
!	NOM_STRESS_Frame(3,1,ix_Frame(1,Fat_pts),iy_Frame(1,Fat_pts))=8.704
!	NOM_STRESS_Frame(4,1,ix_Frame(1,Fat_pts),iy_Frame(1,Fat_pts))=15.371
!	NOM_STRESS_Frame(5,1,ix_Frame(1,Fat_pts),iy_Frame(1,Fat_pts))=175.419
!	NOM_STRESS_Frame(6,1,ix_Frame(1,Fat_pts),iy_Frame(1,Fat_pts))=120.035
!	NOM_STRESS_Frame(7,1,ix_Frame(1,Fat_pts),iy_Frame(1,Fat_pts))=21.284
!
!	NOM_STRESS_Frame(1,2,ix_Frame(2,Fat_pts),iy_Frame(2,Fat_pts))=10.223
!	NOM_STRESS_Frame(2,2,ix_Frame(2,Fat_pts),iy_Frame(2,Fat_pts))=100.92
!	NOM_STRESS_Frame(3,2,ix_Frame(2,Fat_pts),iy_Frame(2,Fat_pts))=36.349
!	NOM_STRESS_Frame(4,2,ix_Frame(2,Fat_pts),iy_Frame(2,Fat_pts))=47.594
!	NOM_STRESS_Frame(5,2,ix_Frame(2,Fat_pts),iy_Frame(2,Fat_pts))=12.632
!	NOM_STRESS_Frame(6,2,ix_Frame(2,Fat_pts),iy_Frame(2,Fat_pts))=168.646
!	NOM_STRESS_Frame(7,2,ix_Frame(2,Fat_pts),iy_Frame(2,Fat_pts))=5.562

	
	END


!*****************************************************************************************

    SUBROUTINE FRAME_HotspotStress
	use param_section
	
	
!	SUBMODULE FAT_HotspotStress !
!	*************************** !


! HS_STRESS : HOT SPOT STRESS


	HS_STRESS_Frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Frame(Fat_pts),panneau(nel_fatigue).iy_Frame(Fat_pts))=	&
	panneau(nel_fatigue).KT_Frame(Fat_pts)*NOM_STRESS_Frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Frame(Fat_pts),panneau(nel_fatigue).iy_Frame(Fat_pts))




!	Sensibilités de DShs_1,DShs_2,DShs_3,DShs_4

!	CALL ANNUL4(dDShs_1,Neto,9,Fat_NSOLM,Neto)
!	CALL ANNUL4(dDShs_2,Neto,9,Fat_NSOLM,Neto)
!	CALL ANNUL4(dDShs_3,Neto,9,Fat_NSOLM,Neto)
!	CALL ANNUL4(dDShs_4,Neto,9,Fat_NSOLM,Neto)

	
!	DO IPAN=1,NETO
!	NBRXI=NVAR(IPAN)
!	L1=0
!	IF(NBRXI.EQ.0) GOTO 126
!	DO K=1,NBRXI
!	KK=NXIT(K,IPAN)
!	LL=L1+K
			
!		IF(nel_fatigue.EQ.IPAN) THEN	! sensibilités par rapport aux variables de conception du panneau considéré 
!			
!				dDShs_1(nel_fatigue,LL,casChge,IPAN)=dDS_1(nel_fatigue,LL,casChge,IPAN)*Kt_hs_1;
!				dDShs_2(nel_fatigue,LL,casChge,IPAN)=dDS_2(nel_fatigue,LL,casChge,IPAN)*Kt_hs_2;
!				dDShs_3(nel_fatigue,LL,casChge,IPAN)=dDS_3(nel_fatigue,LL,casChge,IPAN)*Kt_hs_3;
!				dDShs_4(nel_fatigue,LL,casChge,IPAN)=dDS_4(nel_fatigue,LL,casChge,IPAN)*Kt_hs_4;
!
!		ELSE					! sensibilités par rapport aux variables de conception des autres panneaux



!		ENDIF
!		ENDDO
 ! 126		CONTINUE
        
!	  	L1=L1+NBRXI
!		ENDDO


       END
	 
!*****************************************************************************************


    SUBROUTINE FRAME_NotchStress
	use param_section
	

!	 SUBMODULE FAT_NotchStress !
!	************************* !
!




! NS_STRESS : NOTCH STRESS



if (method_fatigue .eq.1) then	!DNV method_fatigue			

	panneau(nel_fatigue).Kw_Frame(Fat_pts)=1.5

elseif (method_fatigue .eq.2) then	!BV method_fatigue			

	panneau(nel_fatigue).Kw_Frame(Fat_pts)=panneau(nel_fatigue).LAMBDA_Frame(Fat_pts)*SQRT(panneau(nel_fatigue).TETA_Frame(Fat_pts)/30)

endif


! NOTCH STRESS NS_STRESS

	NS_STRESS_Frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Frame(Fat_pts),panneau(nel_fatigue).iy_Frame(Fat_pts))=	&
	0.7*panneau(nel_fatigue).Kw_Frame(Fat_pts)*HS_STRESS_Frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Frame(Fat_pts),panneau(nel_fatigue).iy_Frame(Fat_pts))



!	Sensibilités de DSln_1,DSln_2,DSln_3,DSln_4				

!	CALL ANNUL4(dDSln_1,Neto,9,Fat_NSOLM,Neto)				
!	CALL ANNUL4(dDSln_2,Neto,9,Fat_NSOLM,Neto)				
!	CALL ANNUL4(dDSln_3,Neto,9,Fat_NSOLM,Neto)				
!	CALL ANNUL4(dDSln_4,Neto,9,Fat_NSOLM,Neto)				

	
!	DO IPAN=1,NETO				
!	NBRXI=NVAR(IPAN)				
!	L1=0
!	IF(NBRXI.EQ.0) GOTO 127				
!	DO K=1,NBRXI				
!	KK=NXIT(K,IPAN)				
!	LL=L1+K
!			
!!		IF(nel_fatigue.EQ.IPAN) THEN	! sensibilités par rapport aux variables de conception du panneau considéré 
!			
!			dDSln_1(nel_fatigue,LL,casChge,IPAN)=dDShs_1(nel_fatigue,LL,casChge,IPAN)*Kw(nel_fatigue,1);				
!			dDSln_2(nel_fatigue,LL,casChge,IPAN)=dDShs_2(nel_fatigue,LL,casChge,IPAN)*Kw(nel_fatigue,2);				
!			dDSln_3(nel_fatigue,LL,casChge,IPAN)=dDShs_3(nel_fatigue,LL,casChge,IPAN)*Kw(nel_fatigue,3);				
!			dDSln_4(nel_fatigue,LL,casChge,IPAN)=dDShs_4(nel_fatigue,LL,casChge,IPAN)*Kw(nel_fatigue,4);				
!
!		ELSE					! sensibilités par rapport aux variables de conception des autres panneaux
!
!
!
!!		ENDIF
!		ENDDO				
 ! 127		CONTINUE				
        
!	  	L1=L1+NBRXI				
!		ENDDO				
!


if (method_fatigue .eq.2) then	!BV method_fatigue	

	IF (NS_STRESS_Frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Frame(Fat_pts),panneau(nel_fatigue).iy_Frame(Fat_pts)).EQ.0) GOTO 4

	kc_Frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Frame(Fat_pts),panneau(nel_fatigue).iy_Frame(Fat_pts))=	&
	0.4*(sigy(indMateriau(nel_fatigue))/NS_STRESS_Frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Frame(Fat_pts),panneau(nel_fatigue).iy_Frame(Fat_pts))) + 0.6

	IF (kc_Frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Frame(Fat_pts),panneau(nel_fatigue).iy_Frame(Fat_pts)).LE.0.8) THEN 								

	kc_Frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Frame(Fat_pts),panneau(nel_fatigue).iy_Frame(Fat_pts))=0.800											

	ELSEIF(kc_Frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Frame(Fat_pts),panneau(nel_fatigue).iy_Frame(Fat_pts)).GT.1.) THEN							

	kc_Frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Frame(Fat_pts),panneau(nel_fatigue).iy_Frame(Fat_pts))=1.												

	ENDIF
																		
4	CONTINUE

! NOTCH STRESS NS_STRESS	BV method_fatigue
	NS_STRESS_Frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Frame(Fat_pts),panneau(nel_fatigue).iy_Frame(Fat_pts))=	&
	kc_Frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Frame(Fat_pts),panneau(nel_fatigue).iy_Frame(Fat_pts))*	&
	NS_STRESS_Frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Frame(Fat_pts),panneau(nel_fatigue).iy_Frame(Fat_pts))


endif	!BV method_fatigue	



!	CALL ANNUL4(dKc_1,Neto,9,Fat_NSOLM,Neto)											
!	CALL ANNUL4(dKc_2,Neto,9,Fat_NSOLM,Neto)											
!	CALL ANNUL4(dKc_3,Neto,9,Fat_NSOLM,Neto)											
!	CALL ANNUL4(dKc_4,Neto,9,Fat_NSOLM,Neto)											

	
!	DO IPAN=1,NETO																		
!	NBRXI=NVAR(IPAN)																	
!	L1=0																				
!	IF(NBRXI.EQ.0) GOTO 528																
!	DO K=1,NBRXI																		
!	KK=NXIT(K,IPAN)																		
!	LL=L1+K																				
!			
!	IF (DSln_1(nel_fatigue,casChge).EQ.0) GOTO 10
!
!		IF ((Kc_1(nel_fatigue,casChge).LE.0.8).OR.(Kc_1(nel_fatigue,casChge).GE.1.)) THEN 											
!			dKc_1(nel_fatigue,LL,casChge,IPAN)=0.0																			
!		ELSE																													
!			dKc_1(nel_fatigue,LL,casChge,IPAN)=-0.4*sigy(indMateriau(nel_fatigue))*dDSln_1(nel_fatigue,LL,casChge,IPAN)/DSln_1(nel_fatigue,casChge)**2			
!		ENDIF																										
!
!10	CONTINUE

		
!	IF (DSln_2(nel_fatigue,casChge).EQ.0) GOTO 11

!		IF ((Kc_2(nel_fatigue,casChge).LE.0.8).OR.(Kc_2(nel_fatigue,casChge).GE.1.)) THEN 											
!			dKc_2(nel_fatigue,LL,casChge,IPAN)=0.0																			
!		ELSE																														
!			dKc_2(nel_fatigue,LL,casChge,IPAN)=-0.4*sigy(indMateriau(nel_fatigue))*dDSln_2(nel_fatigue,LL,casChge,IPAN)/DSln_2(nel_fatigue,casChge)**2				
!		ENDIF																										
!
!11	CONTINUE
!
!	IF (DSln_3(nel_fatigue,casChge).EQ.0) GOTO 12
!
!		IF ((Kc_3(nel_fatigue,casChge).LE.0.8).OR.(Kc_3(nel_fatigue,casChge).GE.1.)) THEN 											
!			dKc_3(nel_fatigue,LL,casChge,IPAN)=0.0																			
!		ELSE																														
!			dKc_3(nel_fatigue,LL,casChge,IPAN)=-0.4*sigy(indMateriau(nel_fatigue))*dDSln_3(nel_fatigue,LL,casChge,IPAN)/DSln_3(nel_fatigue,casChge)**2				
!		ENDIF
!																												
!12	CONTINUE

!	IF (DSln_4(nel_fatigue,casChge).EQ.0) GOTO 13

!		IF ((Kc_4(nel_fatigue,casChge).LE.0.8).OR.(Kc_4(nel_fatigue,casChge).GE.1.)) THEN								 			
!			dKc_4(nel_fatigue,LL,casChge,IPAN)=0.0																			
!		ELSE																														
!			dKc_4(nel_fatigue,LL,casChge,IPAN)=-0.4*sigy(indMateriau(nel_fatigue))*dDSln_4(nel_fatigue,LL,casChge,IPAN)/DSln_4(nel_fatigue,casChge)**2				
!		ENDIF																										
!
!13	CONTINUE
		
		
!	ENDDO															
 ! 528	CONTINUE															
        
!	L1=L1+NBRXI															
!	ENDDO															
	













!	Sensibilités FINALES de DSln_1,DSln_2,DSln_3,DSln_4

	
!	DO IPAN=1,NETO																		
!	NBRXI=NVAR(IPAN)																	
!	L1=0																				
!	IF(NBRXI.EQ.0) GOTO 628																
!	DO K=1,NBRXI																		
!	KK=NXIT(K,IPAN)																		
!	LL=L1+K																				
			

!			dDSln_1(nel_fatigue,LL,casChge,IPAN)=dKc_1(nel_fatigue,LL,casChge,IPAN)*DSln_1(nel_fatigue,casChge)+			
 !    *							Kc_1(nel_fatigue,casChge)*dDSln_1(nel_fatigue,LL,casChge,IPAN)						
!
!			dDSln_2(nel_fatigue,LL,casChge,IPAN)=dKc_2(nel_fatigue,LL,casChge,IPAN)*DSln_2(nel_fatigue,casChge)+			
 !    *							Kc_2(nel_fatigue,casChge)*dDSln_2(nel_fatigue,LL,casChge,IPAN)						
!
!			dDSln_3(nel_fatigue,LL,casChge,IPAN)=dKc_3(nel_fatigue,LL,casChge,IPAN)*DSln_3(nel_fatigue,casChge)+			
 !    *							Kc_3(nel_fatigue,casChge)*dDSln_3(nel_fatigue,LL,casChge,IPAN)						
     		
!			dDSln_4(nel_fatigue,LL,casChge,IPAN)=dKc_4(nel_fatigue,LL,casChge,IPAN)*DSln_4(nel_fatigue,casChge)+			
  !   *							Kc_4(nel_fatigue,casChge)*dDSln_4(nel_fatigue,LL,casChge,IPAN)						
 !    		
!	ENDDO																
 ! 628	CONTINUE															
        
!	L1=L1+NBRXI															
!	ENDDO																
						



!	ENDIF																	
		
	END




!*****************************************************************************************

    SUBROUTINE FRAME_Damage
	use param_section
	
	real*8 rezult_gamma,rezult_1,rezult_2,rezult_3,ksi,nu,mu,temp_

!	integer m

!  SUBMODEL DAMAGE_SUM.m  !
! *********************** !
 


	if (NS_STRESS_Frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Frame(Fat_pts),panneau(nel_fatigue).iy_Frame(Fat_pts)).LT.0) THEN
	WRITE(*,*) 'Error, notch stress range is negative'
		pause 'stop'
		stop
	ENDIF

	if ((panneau(nel_fatigue).WEIB_FACT_Frame(Fat_pts).LT.0).OR.(panneau(nel_fatigue).WEIB_FACT_Frame(Fat_pts).GT.4)) THEN
	WRITE(*,*) 'Error, Weib_fact is negative or greater than 4'
		pause 'stop'
		stop
	ENDIF



if (method_fatigue .eq.1) then	!DNV method_fatigue

!	basic shape parameter
	h0 = 2.21 - 0.54*log10(length)		 

!	additional factor depending on motion response period
	ha = 0.05 !in general
!	ha = 0.00 !for plating subjected to forces related to roll motions for vessels with roll period TR > 14 sec.

!	Calcul de TA : periode moyenne, en secondes	
	TA=4*LOG10(length)

!	Number of load cycles during design life
	Ntcyc=0.85*6.3*(10**8.0)/TA


!	 CHECK 

	IF (Ntcyc.LT.1.) THEN
	WRITE(*,*) 'Input value error: Invalid number of load cycles (Ntcyc<1)'
		PAUSE 'STOP'
		STOP
	ENDIF

!	h_Weibull : Weibull shape parameter


!	z(nel_fatigue,3) ! coord  y du noeud depart
!	z(nel_fatigue,4) ! coord  y du noeud d'arrivee
!	znel_fatigue = vertical distance from baseline to considered longitudinal (m)


		znel_fatigue=abs(z(nel_fatigue,3)+(z(nel_fatigue,4)-z(nel_fatigue,3))*(panneau(nel_fatigue).iy_Frame(Fat_pts)-1)/2)/1000

	if (panneau(nel_fatigue).WEIB_FACT_Frame(Fat_pts).EQ.1) then
		
		h_Weibull_Frame = h0																	!For deck longitudinals

	elseif (panneau(nel_fatigue).WEIB_FACT_Frame(Fat_pts).EQ.2) then

		if ((znel_fatigue.GT.Tirant(Fat_NNSOL(caschge_fatigue))).AND.(znel_fatigue.LT.DEPTH)) THEN			!For ship side
		h_Weibull_Frame = h0 + ha*( DEPTH - znel_fatigue ) / ( DEPTH - Tirant(Fat_NNSOL(caschge_fatigue)))				!For ship side above the waterline Tirant < znel_fatigue <DEPTH
	
		elseif (znel_fatigue .EQ.Tirant(Fat_NNSOL(caschge_fatigue))) then
		h_Weibull_Frame = h0 + ha																	!For ship side at the waterline znel_fatigue = Tirant

		elseif (znel_fatigue.LT.Tirant(Fat_NNSOL(caschge_fatigue))) then
		h_Weibull_Frame = h0 + ha*znel_fatigue/Tirant(Fat_NNSOL(caschge_fatigue))-0.005*(Tirant(Fat_NNSOL(caschge_fatigue))-znel_fatigue)		!For ship side below the waterline znel_fatigue < Tirant
		
		ENDIF

	elseif (panneau(nel_fatigue).WEIB_FACT_Frame(Fat_pts).EQ.3) then
	h_Weibull_Frame = h0 - 0.005*Tirant(Fat_NNSOL(caschge_fatigue))											!For bottom longitudinals 
	
	elseif (panneau(nel_fatigue).WEIB_FACT_Frame(Fat_pts).EQ.4) then
	h_Weibull_Frame = h0 + ha																!For longitudinal and transverse bulkheads
	endif

	!	CALCULATION OF DAMAGE SUM D
	temp_ = (panneau(nel_fatigue).m_Frame(Fat_pts)/h_Weibull_Frame)
	CALL gamma_complete(temp_,rezult_gamma)

	Damage_Frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Frame(Fat_pts),panneau(nel_fatigue).iy_Frame(Fat_pts))=	&
	Ntcyc/panneau(nel_fatigue).C_Frame(Fat_pts)*(NS_STRESS_Frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Frame(Fat_pts),panneau(nel_fatigue).iy_Frame(Fat_pts))**panneau(nel_fatigue).m_Frame(Fat_pts))*	&
	(LOG(10.**8))**(-panneau(nel_fatigue).m_Frame(Fat_pts)/h_Weibull_Frame)*rezult_gamma;



!	Sensibilités de D_1,D_2,D_3,D_4

!	CALL ANNULD(dD_1,NTOT)
!	CALL ANNULD(dD_2,NTOT)
!	CALL ANNULD(dD_3,NTOT)
!	CALL ANNULD(dD_4,NTOT)

	
!	DO IPAN=1,NETO
!	NBRXI=NVAR(IPAN)
!	L1=0
!	IF(NBRXI.EQ.0) GOTO 1
!	DO K=1,NBRXI
!	KK=NXIT(K,IPAN)
!	LL=L1+K
!			
!		IF(nel_fatigue.EQ.IPAN) THEN	! sensibilités par rapport aux variables de conception du panneau considéré 
!			
!
!			dD_1(nel_fatigue,LL,caschge_fatigue,IPAN)=Ntcyc/C*(m*DSln_1(nel_fatigue,caschge_fatigue)**(m-1)*dDSln_1(nel_fatigue,LL,caschge_fatigue,IPAN))* 
 !    *					(LOG(10.**8))**(-m/h_Weibull)*rezult_gamma;
!			dD_2(nel_fatigue,LL,caschge_fatigue,IPAN)=Ntcyc/C*(m*DSln_2(nel_fatigue,caschge_fatigue)**(m-1)*dDSln_2(nel_fatigue,LL,caschge_fatigue,IPAN))* 
 !    *					(LOG(10.**8))**(-m/h_Weibull)*rezult_gamma;
!			dD_3(nel_fatigue,LL,caschge_fatigue,IPAN)=Ntcyc/C*(m*DSln_3(nel_fatigue,caschge_fatigue)**(m-1)*dDSln_3(nel_fatigue,LL,caschge_fatigue,IPAN))* 
 !    *					(LOG(10.**8))**(-m/h_Weibull)*rezult_gamma;
!			dD_4(nel_fatigue,LL,caschge_fatigue,IPAN)=Ntcyc/C*(m*DSln_4(nel_fatigue,caschge_fatigue)**(m-1)*dDSln_4(nel_fatigue,LL,caschge_fatigue,IPAN))* 
 !    *					(LOG(10.**8))**(-m/h_Weibull)*rezult_gamma;



!		ELSE					! sensibilités par rapport aux variables de conception des autres panneaux



!		ENDIF
!		ENDDO
 ! 1		CONTINUE
        
!	  	L1=L1+NBRXI
!		ENDDO

!*******************************************************************************
	
elseif(method_fatigue.EQ.2) then		!BV method_fatigue


!	probabilité d'excédence 
	PR=10.**(-5.)

!	Calcul de TA : periode moyenne, en secondes	
	TA=4*LOG10(length)

!	alfa0 : facteur de navigation pris égal à 0.8
	alfa0=0.85

!	Calcul de Ntcyc	: nombre total des cycles
	Ntcyc=631*alfa0/TA*10**6.0

!	Calcul de Ntfl (Tfl:Increased design life (between 25 and 40))
	Ntfl=31.55*alfa0*Tfl*10**6.0/TA
	
	IF(Tfl.GE.25) THEN
	CFL=LOG10(0.2*LOG10(Ntfl))/LOG10(0.2*LOG10(Ntcyc))
	else
	CFL=1.
	endif

!	Calcul de ksi,ksi0 : paramêtres de forme de la fonction de WEIBULL
	ksi0=(73-0.07*length)/60*CFL
	IF(ksi0.LT.0.85) THEN
	ksi0=0.85
	ENDIF


!	CALL ANNUL2(Damage_Stif,nsolm_fatigue,NETO,ix_max,iy_max)
!	CALL ANNUL2(Damage_Plate,nsolm_fatigue,NETO,ix_max,iy_max)
!	CALL ANNUL2(Damage_Plate_frame,nsolm_fatigue,NETO,ix_max,iy_max)
!	CALL ANNUL2(Damage_frame,nsolm_fatigue,NETO,ix_max,iy_max)
!	CALL ANNUL2(Damage_Girder,nsolm_fatigue,NETO,ix_max,mt_max)
!	CALL ANNUL2(Damage_Pilar,nsolm_fatigue,NETO,ix_max,iy_max)


!	CALL ANNUL4(dD_1,Neto,9,nsolm_fatigue,Neto)
!	CALL ANNUL4(dD_2,Neto,9,nsolm_fatigue,Neto)
!	CALL ANNUL4(dD_3,Neto,9,nsolm_fatigue,Neto)
!	CALL ANNUL4(dD_4,Neto,9,nsolm_fatigue,Neto)

!	if(Sty.EQ.1) then

!	damage : Hot spot 1
			

!     characteristics if the stiffened elements (epsa,entr,delta,hxr,dxr,wxr,txr,sigy(indMateriau(nel_fatigue)),sigy(indMateriau(nel_fatigue)))
!     ------------------------------------------
!  .  sl = epsa : length of stiffener between transverse frames (m.)
!  .  bp = entr : longitudinal stiffener spacing   (m.)
!  .  tp = delta: thickness of parent plate        (m.)
!  .  hw = hxr  : height of stiffener web          (m.)
!  .  tw = dxr  : thickness of stiffener web       (m.)
!  .  bf = wxr  : breadth of stiffener flange      (m.)
!  .  tf = txr  : thickness of stiffener flange    (m.)
!  .  syp= sigy(indMateriau(nel_fatigue)) : yield strength of parent plate   (n/m2)   
!  .  syw= sigy(indMateriau(nel_fatigue)) : yield strength of stiffener      (n/m2)   

!   les variables de conception  ; nbrxi = nvar(nel_fatigue,iboat)
!   -------------------------------------------------
!       1       delta =  épaisseur du bordage
!       2       hya   =  hauteur de l'âme des cadres   
!       3       dya   =  épaisseur de l'âme des cadres   
!       4       wya   =  largeur des semelles des cadres   
!       5       epsa  =  entredistance entre cadres   
!       6       hxr   =  hauteur de l'âme des raidisseurs
!       7       dxr   =  épaisseur de l'âme des raidisseurs
!       8       wxr   =  largeur des semelles des raidisseurs
!       9       epsr  =  entredistance entre raidisseurs

! 	autres paramètres :
!	 	tya = épaisseur semelle aiguilles
!     	txr = épaisseur semelle raidisseurs 
!
!  	Les variables de conception pour epontille
!     ------------------------------------------					
!	1	hya		=	hauteur demi âme ou diam ou côté ext.	
!	2	dya		=	épaisseur d'âme							
!	3	wya		=	largeur de semelle						
!	4	epais	=	épaisseur de paroi mince				
!	5	epsa	=	entredistance entre épontillesc


!	thF	: épaisseur de l'élément considéré (mm) (minimum thickness of connected elements)


	znel_fatigue=abs(z(nel_fatigue,3)+(z(nel_fatigue,4)-z(nel_fatigue,3))*(panneau(nel_fatigue).iy_Frame(Fat_pts)-1)/2)/1000

!tya = épaisseur semelle aiguilles

	thF=panneau(nel_fatigue).tya*1000

!	Calcul de kpSN : constante de la Design S-N curve
	IF(thF.LT.22) THEN
	kpSN=5.802*(10**12.0)
	ELSE
	kpSN=5.802*(10**12.0)*(22.0/thF)**(0.9)
	ENDIF

!	Calcul de SigmaQ : étendue(sigmaMAx-sigmaMin) de contrainte à l'intersection des 02 segments de la courbe S-N.
	SigmaQ=(kpSN*(10**(-7.0)))**(1.0/3.0)

	ksi=ksi0*(1.04-0.14*ABS(znel_fatigue-Tirant(Fat_NNSOL(caschge_fatigue)))/(DEPTH-Tirant(Fat_NNSOL(caschge_fatigue))))

!	CALL ANNULD(nu,nsolm_fatigue)

	nu=0.0

	if (NS_STRESS_Frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Frame(Fat_pts),panneau(nel_fatigue).iy_Frame(Fat_pts)).EQ.0) GOTO 10

!	nu : coefficient utilisé pour le calcul de la fonction GAMMA incomplète.
	nu=-((SigmaQ/NS_STRESS_Frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Frame(Fat_pts),panneau(nel_fatigue).iy_Frame(Fat_pts)))**(ksi))*LOG(PR)					

!	rezult_1 et rezult_2:résultats de la fct gamma incomplète
	call gamma_incomplete(3/ksi,nu,rezult_1)						
	call gamma_incomplete(5/ksi,nu,rezult_2)							
!	rezult_3 :résultat de la fct gamma complète
	CALL gamma_complete(3/ksi,rezult_3)

!	mu : coefficient qui tient en compte du changement de pente de la courbe S-N
	mu=1-(((rezult_1)-(rezult_2*(nu**(-2.0/ksi))))/rezult_3)		
	
!	DAMAGE		

	Damage_Frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Frame(Fat_pts),panneau(nel_fatigue).iy_Frame(Fat_pts))=	&
	Ntcyc*(NS_STRESS_Frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Frame(Fat_pts),panneau(nel_fatigue).iy_Frame(Fat_pts))**3.)	&
						/(kpSN*(-LOG(PR))**(3./ksi))*mu*rezult_3


ENDIF

10	CONTINUE
	
	end





!*******************************************************************************
!*******************************************************************************
!*******************************************************************************
!	OPTIONS /EXTEND
!	SUBROUTINE FATIGUE_GIRDER
!	use param_section


!	CALL FAT_Check_Input


!    CALCULATION OF NOMINALS STRESSES  -------------------------------------
!    The submodule calculates nominal stress ranges at critical locations 
!    of structural member based on beam or plate theory. 
!	call GIRDER_NominalStress


!	 CALCULATION OF HOT-SPOT STRESSES --------------------------------------
!	 The submodele calculates structural stress ranges based on 
!	 semi-analytical approaches for stress concentration factors. 
!	 Library of different developed solutions is applied.
!	call GIRDER_HotspotStress



!	CALCULATION OF NOTCH STRESSES -----------------------------------------
!	The submodele calculates notch stress range with the help of additional 
!	stress concentration factor, which defines the relationship between 
!	hot-spot and notch stress.
!	call GIRDER_NotchStress

!	CALCULATION OF DAMAGE SUM----------------------------------------------
!	The submodule calculates damage sum based on linear damage theory and
!	Palmer-Miner rule. Submodel uses S-N curve, which includes the fatigue 
!	reduction and uncertainness due to dimensional accuracy, welding 
!	distortion, and environment. (FOR EACH LOAD CASE)
!	call GIRDER_Damage

!	end


!*****************************************************************************************
!SUBROUTINE GIRDER_NominalStress
!use param_section
!      real*8 PI
	
!	SUBMODULE NOMINAL_STRESS
!	************************** 


!sigma x semelle    traverse 

!NOM_STRESS_Girder(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Girder(Fat_pts,MMT),MMT)=abs(	&
!S_Girder_sem(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Girder(Fat_pts,MMT),MMT))&
!/10**6	!pour passer en MPa		!fat_new

!	NOM_STRESS_Girder(1,nel_fatigue,panneau(nel_fatigue).ix_Girder(Fat_pts,MMT),MMT)=179.12
!	NOM_STRESS_Girder(2,nel_fatigue,panneau(nel_fatigue).ix_Girder(Fat_pts,MMT),MMT)=52.48
!	NOM_STRESS_Girder(3,nel_fatigue,panneau(nel_fatigue).ix_Girder(Fat_pts,MMT),MMT)=8.704
!	NOM_STRESS_Girder(4,nel_fatigue,panneau(nel_fatigue).ix_Girder(Fat_pts,MMT),MMT)=15.371
!	NOM_STRESS_Girder(5,nel_fatigue,panneau(nel_fatigue).ix_Girder(Fat_pts,MMT),MMT)=175.419
!	NOM_STRESS_Girder(6,nel_fatigue,panneau(nel_fatigue).ix_Girder(Fat_pts,MMT),MMT)=120.035
!	NOM_STRESS_Girder(7,nel_fatigue,panneau(nel_fatigue).ix_Girder(Fat_pts,MMT),MMT)=21.284


!	NOM_STRESS_Girder(1,1,ix_Girder(1,Fat_pts,MMT),MMT)=179.12
!	NOM_STRESS_Girder(2,1,ix_Girder(1,Fat_pts,MMT),MMT)=52.48
!	NOM_STRESS_Girder(3,1,ix_Girder(1,Fat_pts,MMT),MMT)=8.704
!	NOM_STRESS_Girder(4,1,ix_Girder(1,Fat_pts,MMT),MMT)=15.371
!	NOM_STRESS_Girder(5,1,ix_Girder(1,Fat_pts,MMT),MMT)=175.419
!	NOM_STRESS_Girder(6,1,ix_Girder(1,Fat_pts,MMT),MMT)=120.035
!	NOM_STRESS_Girder(7,1,ix_Girder(1,Fat_pts,MMT),MMT)=21.284
!
!	NOM_STRESS_Girder(1,2,ix_Girder(2,Fat_pts,MMT),MMT)=10.223
!	NOM_STRESS_Girder(2,2,ix_Girder(2,Fat_pts,MMT),MMT)=100.92
!	NOM_STRESS_Girder(3,2,ix_Girder(2,Fat_pts,MMT),MMT)=36.349
!	NOM_STRESS_Girder(4,2,ix_Girder(2,Fat_pts,MMT),MMT)=47.594
!	NOM_STRESS_Girder(5,2,ix_Girder(2,Fat_pts,MMT),MMT)=12.632
!	NOM_STRESS_Girder(6,2,ix_Girder(2,Fat_pts,MMT),MMT)=168.646
!	NOM_STRESS_Girder(7,2,ix_Girder(2,Fat_pts,MMT),MMT)=5.562




!END

!*****************************************************************************************

 !   SUBROUTINE GIRDER_HotspotStress
!	use param_section
	
!	SUBMODULE FAT_HotspotStress !
!	*************************** !


! HS_STRESS : HOT SPOT STRESS


!	HS_STRESS_Girder(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Girder(Fat_pts,MMT),MMT)=	&									!fat_new
!	panneau(nel_fatigue).KT_Girder(Fat_pts,MMT)*NOM_STRESS_Girder(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Girder(Fat_pts,MMT),MMT)	!fat_new


!	Sensibilités de DShs_1,DShs_2,DShs_3,DShs_4

!	CALL ANNUL4(dDShs_1,Neto,9,Fat_NSOLM,Neto)
!	CALL ANNUL4(dDShs_2,Neto,9,Fat_NSOLM,Neto)
!	CALL ANNUL4(dDShs_3,Neto,9,Fat_NSOLM,Neto)
!	CALL ANNUL4(dDShs_4,Neto,9,Fat_NSOLM,Neto)

	
!	DO IPAN=1,NETO
!	NBRXI=NVAR(IPAN)
!	L1=0
!	IF(NBRXI.EQ.0) GOTO 126
!	DO K=1,NBRXI
!	KK=NXIT(K,IPAN)
!	LL=L1+K
			
!		IF(nel_fatigue.EQ.IPAN) THEN	! sensibilités par rapport aux variables de conception du panneau considéré 
!			
!				dDShs_1(nel_fatigue,LL,casChge,IPAN)=dDS_1(nel_fatigue,LL,casChge,IPAN)*Kt_hs_1;
!				dDShs_2(nel_fatigue,LL,casChge,IPAN)=dDS_2(nel_fatigue,LL,casChge,IPAN)*Kt_hs_2;
!				dDShs_3(nel_fatigue,LL,casChge,IPAN)=dDS_3(nel_fatigue,LL,casChge,IPAN)*Kt_hs_3;
!				dDShs_4(nel_fatigue,LL,casChge,IPAN)=dDS_4(nel_fatigue,LL,casChge,IPAN)*Kt_hs_4;
!
!		ELSE					! sensibilités par rapport aux variables de conception des autres panneaux



!		ENDIF
!		ENDDO
 ! 126		CONTINUE
        
!	  	L1=L1+NBRXI
!		ENDDO


   !    END
	 
!*****************************************************************************************


!    SUBROUTINE GIRDER_NotchStress
!	use param_section

!	 SUBMODULE FAT_NotchStress !
!	************************* !
!

!

! NS_STRESS : NOTCH STRESS



!if (method_fatigue .eq.1) then	!DNV method_fatigue			
!
!	Kw_Girder(nel_fatigue,Fat_pts,MMT)=1.5
!
!elseif (method_fatigue .eq.2) then	!BV method_fatigue			

!	Kw_Girder(nel_fatigue,Fat_pts,MMT)=LAMBDA_Girder(nel_fatigue,Fat_pts,MMT)*SQRT(TETA_Girder(nel_fatigue,Fat_pts,MMT)/30)

!endif


! NOTCH STRESS NS_STRESS


!	NS_STRESS_Girder(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Girder(Fat_pts,MMT),MMT)=	&
!	0.7*Kw_Girder(nel_fatigue,Fat_pts,MMT)*HS_STRESS_Girder(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Girder(Fat_pts,MMT),MMT)


!	Sensibilités de DSln_1,DSln_2,DSln_3,DSln_4				

!	CALL ANNUL4(dDSln_1,Neto,9,Fat_NSOLM,Neto)				
!	CALL ANNUL4(dDSln_2,Neto,9,Fat_NSOLM,Neto)				
!	CALL ANNUL4(dDSln_3,Neto,9,Fat_NSOLM,Neto)				
!	CALL ANNUL4(dDSln_4,Neto,9,Fat_NSOLM,Neto)				

	
!	DO IPAN=1,NETO				
!	NBRXI=NVAR(IPAN)				
!	L1=0
!	IF(NBRXI.EQ.0) GOTO 127				
!	DO K=1,NBRXI				
!	KK=NXIT(K,IPAN)				
!	LL=L1+K
!			
!!		IF(nel_fatigue.EQ.IPAN) THEN	! sensibilités par rapport aux variables de conception du panneau considéré 
!			
!			dDSln_1(nel_fatigue,LL,casChge,IPAN)=dDShs_1(nel_fatigue,LL,casChge,IPAN)*Kw(nel_fatigue,1);				
!			dDSln_2(nel_fatigue,LL,casChge,IPAN)=dDShs_2(nel_fatigue,LL,casChge,IPAN)*Kw(nel_fatigue,2);				
!			dDSln_3(nel_fatigue,LL,casChge,IPAN)=dDShs_3(nel_fatigue,LL,casChge,IPAN)*Kw(nel_fatigue,3);				
!			dDSln_4(nel_fatigue,LL,casChge,IPAN)=dDShs_4(nel_fatigue,LL,casChge,IPAN)*Kw(nel_fatigue,4);				
!
!		ELSE					! sensibilités par rapport aux variables de conception des autres panneaux
!
!
!
!!		ENDIF
!		ENDDO				
 ! 127		CONTINUE				
        
!	  	L1=L1+NBRXI				
!		ENDDO				
!


!if (method_fatigue .eq.2) then	!BV method_fatigue	

!	IF (NS_STRESS_Girder(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Girder(Fat_pts,MMT),MMT).EQ.0) GOTO 5

!	kc_Girder(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Girder(Fat_pts,MMT),MMT)=	&
!	0.4*(sigy(indMateriau(nel_fatigue))/NS_STRESS_Girder(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Girder(Fat_pts,MMT),MMT)) + 0.6
!
!	IF (kc_Girder(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Girder(Fat_pts,MMT),MMT).LE.0.8) THEN 								
!
!	kc_Girder(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Girder(Fat_pts,MMT),MMT)=0.800											
!
!	ELSEIF(kc_Girder(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Girder(Fat_pts,MMT),MMT).GT.1.) THEN							
!
!	kc_Girder(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Girder(Fat_pts,MMT),MMT)=1.												
!
!	ENDIF															
!
!	NS_STRESS_Girder(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Girder(Fat_pts,MMT),MMT)=	&
!	kc_Girder(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Girder(Fat_pts,MMT),MMT)*	&
!	NS_STRESS_Girder(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Girder(Fat_pts,MMT),MMT)
!
!5	CONTINUE
!
!endif	!BV method_fatigue	



!	CALL ANNUL4(dKc_1,Neto,9,Fat_NSOLM,Neto)											
!	CALL ANNUL4(dKc_2,Neto,9,Fat_NSOLM,Neto)											
!	CALL ANNUL4(dKc_3,Neto,9,Fat_NSOLM,Neto)											
!	CALL ANNUL4(dKc_4,Neto,9,Fat_NSOLM,Neto)											

	
!	DO IPAN=1,NETO																		
!	NBRXI=NVAR(IPAN)																	
!	L1=0																				
!	IF(NBRXI.EQ.0) GOTO 528																
!	DO K=1,NBRXI																		
!	KK=NXIT(K,IPAN)																		
!	LL=L1+K																				
!			
!	IF (DSln_1(nel_fatigue,casChge).EQ.0) GOTO 10
!
!		IF ((Kc_1(nel_fatigue,casChge).LE.0.8).OR.(Kc_1(nel_fatigue,casChge).GE.1.)) THEN 											
!			dKc_1(nel_fatigue,LL,casChge,IPAN)=0.0																			
!		ELSE																													
!			dKc_1(nel_fatigue,LL,casChge,IPAN)=-0.4*sigy(indMateriau(nel_fatigue))*dDSln_1(nel_fatigue,LL,casChge,IPAN)/DSln_1(nel_fatigue,casChge)**2			
!		ENDIF																										
!
!10	CONTINUE

		
!	IF (DSln_2(nel_fatigue,casChge).EQ.0) GOTO 11

!		IF ((Kc_2(nel_fatigue,casChge).LE.0.8).OR.(Kc_2(nel_fatigue,casChge).GE.1.)) THEN 											
!			dKc_2(nel_fatigue,LL,casChge,IPAN)=0.0																			
!		ELSE																														
!			dKc_2(nel_fatigue,LL,casChge,IPAN)=-0.4*sigy(indMateriau(nel_fatigue))*dDSln_2(nel_fatigue,LL,casChge,IPAN)/DSln_2(nel_fatigue,casChge)**2				
!		ENDIF																										
!
!11	CONTINUE
!
!	IF (DSln_3(nel_fatigue,casChge).EQ.0) GOTO 12
!
!		IF ((Kc_3(nel_fatigue,casChge).LE.0.8).OR.(Kc_3(nel_fatigue,casChge).GE.1.)) THEN 											
!			dKc_3(nel_fatigue,LL,casChge,IPAN)=0.0																			
!		ELSE																														
!			dKc_3(nel_fatigue,LL,casChge,IPAN)=-0.4*sigy(indMateriau(nel_fatigue))*dDSln_3(nel_fatigue,LL,casChge,IPAN)/DSln_3(nel_fatigue,casChge)**2				
!		ENDIF
!																												
!12	CONTINUE

!	IF (DSln_4(nel_fatigue,casChge).EQ.0) GOTO 13

!		IF ((Kc_4(nel_fatigue,casChge).LE.0.8).OR.(Kc_4(nel_fatigue,casChge).GE.1.)) THEN								 			
!			dKc_4(nel_fatigue,LL,casChge,IPAN)=0.0																			
!		ELSE																														
!			dKc_4(nel_fatigue,LL,casChge,IPAN)=-0.4*sigy(indMateriau(nel_fatigue))*dDSln_4(nel_fatigue,LL,casChge,IPAN)/DSln_4(nel_fatigue,casChge)**2				
!		ENDIF																										
!
!13	CONTINUE
		
		
!	ENDDO															
 ! 528	CONTINUE															
        
!	L1=L1+NBRXI															
!	ENDDO															
	













!	Sensibilités FINALES de DSln_1,DSln_2,DSln_3,DSln_4

	
!	DO IPAN=1,NETO																		
!	NBRXI=NVAR(IPAN)																	
!	L1=0																				
!	IF(NBRXI.EQ.0) GOTO 628																
!	DO K=1,NBRXI																		
!	KK=NXIT(K,IPAN)																		
!	LL=L1+K																				
			

!			dDSln_1(nel_fatigue,LL,casChge,IPAN)=dKc_1(nel_fatigue,LL,casChge,IPAN)*DSln_1(nel_fatigue,casChge)+			
 !    *							Kc_1(nel_fatigue,casChge)*dDSln_1(nel_fatigue,LL,casChge,IPAN)						
!
!			dDSln_2(nel_fatigue,LL,casChge,IPAN)=dKc_2(nel_fatigue,LL,casChge,IPAN)*DSln_2(nel_fatigue,casChge)+			
 !    *							Kc_2(nel_fatigue,casChge)*dDSln_2(nel_fatigue,LL,casChge,IPAN)						
!
!			dDSln_3(nel_fatigue,LL,casChge,IPAN)=dKc_3(nel_fatigue,LL,casChge,IPAN)*DSln_3(nel_fatigue,casChge)+			
 !    *							Kc_3(nel_fatigue,casChge)*dDSln_3(nel_fatigue,LL,casChge,IPAN)						
     		
!			dDSln_4(nel_fatigue,LL,casChge,IPAN)=dKc_4(nel_fatigue,LL,casChge,IPAN)*DSln_4(nel_fatigue,casChge)+			
  !   *							Kc_4(nel_fatigue,casChge)*dDSln_4(nel_fatigue,LL,casChge,IPAN)						
 !    		
!	ENDDO																
 ! 628	CONTINUE															
        
!	L1=L1+NBRXI															
!	ENDDO																
						



!	ENDIF																	
		
!	END




!*****************************************************************************************

!      SUBROUTINE GIRDER_Damage
!	use param_section
!	real*8 rezult_gamma,rezult_1,rezult_2,rezult_3,ksi,nu,mu

!	integer m

!  SUBMODEL DAMAGE_SUM.m  !
! *********************** !
 


!	 CHECKING OF STRESS VALUES and weibull factor

!	if (NS_STRESS_Girder(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Girder(Fat_pts,MMT),MMT).LT.0) THEN
!	WRITE(*,*) 'Error, notch stress range is negative'
!		pause 'stop'
!		stop
!	ENDIF

!	if ((Weib_fact_Girder(nel_fatigue,Fat_pts,MMT).LT.0).OR.(Weib_fact_Girder(nel_fatigue,Fat_pts,MMT).GT.4)) THEN
!	WRITE(*,*) 'Error, Weib_fact is negative or greater than 4'
!		pause 'stop'
!		stop
!	ENDIF
!
!
!if (method_fatigue .eq.1) then	!DNV method_fatigue
!
!	basic shape parameter
!	h0 = 2.21 - 0.54*log10(length)		 

!	additional factor depending on motion response period
!	ha = 0.05 !in general
!	ha = 0.00 !for plating subjected to forces related to roll motions for vessels with roll period TR > 14 sec.

!	Calcul de TA : periode moyenne, en secondes	
!	TA=4*LOG10(length)

!	Number of load cycles during design life
!	Ntcyc=0.85*6.3*(10**8.0)/TA
!

!	 CHECK 

!	IF (Ntcyc.LT.1.) THEN
!	WRITE(*,*) 'Input value error: Invalid number of load cycles (Ntcyc<1)'
!		PAUSE 'STOP'
!		STOP
!	ENDIF
!
!!	h_Weibull : Weibull shape parameter
!
!
!!	z(nel_fatigue,3) ! coord  y du noeud depart
!!	z(nel_fatigue,4) ! coord  y du noeud d'arrivee
!!	znel_fatigue = vertical distance from baseline to considered longitudinal (m)
!
!		znel_fatigue=abs(z(nel_fatigue,3)+(z(nel_fatigue,4)-z(nel_fatigue,3))*abtr(nel_fatigue,MMT)/HIGHT(nel_fatigue))/1000
!
!	if (Weib_fact_Girder(nel_fatigue,Fat_pts,MMT).EQ.1) then
!		
!		h_Weibull_Girder = h0																	!For deck longitudinals
!
!	elseif (Weib_fact_Girder(nel_fatigue,Fat_pts,MMT).EQ.2) then
!
!		if ((znel_fatigue.GT.Tirant(Fat_NNSOL(caschge_fatigue))).AND.(znel_fatigue.LT.DEPTH)) THEN			!For ship side
!		h_Weibull_Girder = h0 + ha*( DEPTH - znel_fatigue ) / ( DEPTH - Tirant(Fat_NNSOL(caschge_fatigue)))				!For ship side above the waterline Tirant < znel_fatigue <DEPTH
!	
!		elseif (znel_fatigue .EQ.Tirant(Fat_NNSOL(caschge_fatigue))) then
!		h_Weibull_Girder = h0 + ha																	!For ship side at the waterline znel_fatigue = Tirant
!
!		elseif (znel_fatigue.LT.Tirant(Fat_NNSOL(caschge_fatigue))) then
!		h_Weibull_Girder = h0 + ha*znel_fatigue/Tirant(Fat_NNSOL(caschge_fatigue))-0.005*(Tirant(Fat_NNSOL(caschge_fatigue))-znel_fatigue)		!For ship side below the waterline znel_fatigue < Tirant
!		
!		ENDIF
!
!	elseif (Weib_fact_Girder(nel_fatigue,Fat_pts,MMT).EQ.3) then
!	h_Weibull_Girder = h0 - 0.005*Tirant(Fat_NNSOL(caschge_fatigue))											!For bottom longitudinals 
!	
!	elseif (Weib_fact_Girder(nel_fatigue,Fat_pts,MMT).EQ.4) then
!	h_Weibull_Girder = h0 + ha																!For longitudinal and transverse bulkheads
!	endif
!
!	!	CALCULATION OF DAMAGE SUM D
!	CALL gamma_complete((m_Girder(nel_fatigue,Fat_pts,mmt)/h_Weibull_Girder),rezult_gamma)
!
!	Damage_Girder(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Girder(Fat_pts,MMT),mmt)=	&
!	Ntcyc/C_Girder(nel_fatigue,Fat_pts,mmt)*(NS_STRESS_Girder(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Girder(Fat_pts,MMT),mmt)**m_Girder(nel_fatigue,Fat_pts,mmt))*	&
!	(LOG(10.**8))**(-m_Girder(nel_fatigue,Fat_pts,mmt)/h_Weibull_Girder)*rezult_gamma;
!
!
!
!
!
!	Sensibilités de D_1,D_2,D_3,D_4

!	CALL ANNULD(dD_1,NTOT)
!	CALL ANNULD(dD_2,NTOT)
!	CALL ANNULD(dD_3,NTOT)
!	CALL ANNULD(dD_4,NTOT)

	
!	DO IPAN=1,NETO
!	NBRXI=NVAR(IPAN)
!	L1=0
!	IF(NBRXI.EQ.0) GOTO 1
!	DO K=1,NBRXI
!	KK=NXIT(K,IPAN)
!	LL=L1+K
!			
!		IF(nel_fatigue.EQ.IPAN) THEN	! sensibilités par rapport aux variables de conception du panneau considéré 
!			
!
!			dD_1(nel_fatigue,LL,caschge_fatigue,IPAN)=Ntcyc/C*(m*DSln_1(nel_fatigue,caschge_fatigue)**(m-1)*dDSln_1(nel_fatigue,LL,caschge_fatigue,IPAN))* 
 !    *					(LOG(10.**8))**(-m/h_Weibull)*rezult_gamma;
!			dD_2(nel_fatigue,LL,caschge_fatigue,IPAN)=Ntcyc/C*(m*DSln_2(nel_fatigue,caschge_fatigue)**(m-1)*dDSln_2(nel_fatigue,LL,caschge_fatigue,IPAN))* 
 !    *					(LOG(10.**8))**(-m/h_Weibull)*rezult_gamma;
!			dD_3(nel_fatigue,LL,caschge_fatigue,IPAN)=Ntcyc/C*(m*DSln_3(nel_fatigue,caschge_fatigue)**(m-1)*dDSln_3(nel_fatigue,LL,caschge_fatigue,IPAN))* 
 !    *					(LOG(10.**8))**(-m/h_Weibull)*rezult_gamma;
!			dD_4(nel_fatigue,LL,caschge_fatigue,IPAN)=Ntcyc/C*(m*DSln_4(nel_fatigue,caschge_fatigue)**(m-1)*dDSln_4(nel_fatigue,LL,caschge_fatigue,IPAN))* 
 !    *					(LOG(10.**8))**(-m/h_Weibull)*rezult_gamma;



!		ELSE					! sensibilités par rapport aux variables de conception des autres panneaux



!		ENDIF
!		ENDDO
 ! 1		CONTINUE
        
!	  	L1=L1+NBRXI
!		ENDDO

!*******************************************************************************
	
!elseif(method_fatigue.EQ.2) then		!BV method_fatigue
!
!
!!	probabilité d'excédence 
!	PR=10.**(-5.)
!
!!	Calcul de TA : periode moyenne, en secondes	
!	TA=4*LOG10(length)
!
!!	alfa0 : facteur de navigation pris égal à 0.8
!	alfa0=0.85
!
!!	Calcul de Ntcyc	: nombre total des cycles
!	Ntcyc=631*alfa0/TA*10**6.0
!
!!	Calcul de Ntfl (Tfl:Increased design life (between 25 and 40))
!	Ntfl=31.55*alfa0*Tfl*10**6.0/TA
!	
!	IF(Tfl.GE.25) THEN
!	CFL=LOG10(0.2*LOG10(Ntfl))/LOG10(0.2*LOG10(Ntcyc))
!	else
!	CFL=1.
!	endif
!
!!	Calcul de ksi,ksi0 : paramêtres de forme de la fonction de WEIBULL
!	ksi0=(73-0.07*length)/60*CFL
!	IF(ksi0.LT.0.85) THEN
!	ksi0=0.85
!	ENDIF
!
!
!!	CALL ANNUL2(Damage_Stif,nsolm_fatigue,NETO,ix_max,iy_max)
!!	CALL ANNUL2(Damage_Plate,nsolm_fatigue,NETO,ix_max,iy_max)
!!	CALL ANNUL2(Damage_Plate_frame,nsolm_fatigue,NETO,ix_max,iy_max)
!!	CALL ANNUL2(Damage_frame,nsolm_fatigue,NETO,ix_max,iy_max)
!!	CALL ANNUL2(Damage_Girder,nsolm_fatigue,NETO,ix_max,mt_max)
!!	CALL ANNUL2(Damage_Pilar,nsolm_fatigue,NETO,ix_max,iy_max)
!
!
!!	CALL ANNUL4(dD_1,Neto,9,nsolm_fatigue,Neto)
!!	CALL ANNUL4(dD_2,Neto,9,nsolm_fatigue,Neto)
!!	CALL ANNUL4(dD_3,Neto,9,nsolm_fatigue,Neto)
!!	CALL ANNUL4(dD_4,Neto,9,nsolm_fatigue,Neto)
!
!!	if(Sty.EQ.1) then
!
!!	damage : Hot spot 1
!			
!
!!     characteristics if the stiffened elements (epsa,entr,delta,hxr,dxr,wxr,txr,sigy(indMateriau(nel_fatigue)),sigy(indMateriau(nel_fatigue)))
! !    ------------------------------------------
!  !.  sl = epsa : length of stiffener between transverse frames (m.)
!  .!  bp = entr : longitudinal stiffener spacing   (m.)
!  . ! tp = delta: thickness of parent plate        (m.)
!  .  hw = hxr  : height of stiffener web          (m.)
!  .  tw = dxr  : thickness of stiffener web       (m.)
!  .  bf = wxr  : breadth of stiffener flange      (m.)
!  .  tf = txr  : thickness of stiffener flange    (m.)
!  .  syp= sigy(indMateriau(nel_fatigue)) : yield strength of parent plate   (n/m2)   
!  .  syw= sigy(indMateriau(nel_fatigue)) : yield strength of stiffener      (n/m2)   

!   les variables de conception  ; nbrxi = nvar(nel_fatigue,iboat)
!   -------------------------------------------------
!       1       delta =  épaisseur du bordage
!       2       hya   =  hauteur de l'âme des cadres   
!       3       dya   =  épaisseur de l'âme des cadres   
!       4       wya   =  largeur des semelles des cadres   
!       5       epsa  =  entredistance entre cadres   
!       6       hxr   =  hauteur de l'âme des raidisseurs
!       7       dxr   =  épaisseur de l'âme des raidisseurs
!       8       wxr   =  largeur des semelles des raidisseurs
!       9       epsr  =  entredistance entre raidisseurs

! 	autres paramètres :
!	 	tya = épaisseur semelle aiguilles
!     	txr = épaisseur semelle raidisseurs 
!
!  	Les variables de conception pour epontille
!     ------------------------------------------					
!	1	hya		=	hauteur demi âme ou diam ou côté ext.	
!	2	dya		=	épaisseur d'âme							
!	3	wya		=	largeur de semelle						
!	4	epais	=	épaisseur de paroi mince				
!	5	epsa	=	entredistance entre épontillesc


!	thF	: épaisseur de l'élément considéré (mm) (minimum thickness of connected elements)

!abtr(nel_fatigue,MMT) : position of the girder with reference to the departure node (in meter

!	znel_fatigue=abs(z(nel_fatigue,3)+(z(nel_fatigue,4)-z(nel_fatigue,3))*abtr(nel_fatigue,MMT)/HIGHT(nel_fatigue))/1000

!	tya = épaisseur semelle girder (traverses)
!	thF=panneau(nel_fatigue).txtr (mmt)*1000


!	Calcul de kpSN : constante de la Design S-N curve
!	IF(thF.LT.22) THEN
!	kpSN=5.802*(10**12.0)
!	ELSE
!	kpSN=5.802*(10**12.0)*(22.0/thF)**(0.9)
!	ENDIF
!
!	Calcul de SigmaQ : étendue(sigmaMAx-sigmaMin) de contrainte à l'intersection des 02 segments de la courbe S-N.
!	SigmaQ=(kpSN*(10**(-7.0)))**(1.0/3.0)
!
!	ksi=ksi0*(1.04-0.14*ABS(znel_fatigue-Tirant(Fat_NNSOL(caschge_fatigue)))/(DEPTH-Tirant(Fat_NNSOL(caschge_fatigue))))
!
!!	CALL ANNULD(nu,nsolm_fatigue)
!
!	nu=0.0
!
!	if (NS_STRESS_Girder(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Girder(Fat_pts,MMT),MMT).EQ.0) GOTO 10
!
!!	nu : coefficient utilisé pour le calcul de la fonction GAMMA incomplète.
!	nu=-((SigmaQ/NS_STRESS_Girder(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Girder(Fat_pts,MMT),MMT))**(ksi))*LOG(PR)					
!
!!	rezult_1 et rezult_2:résultats de la fct gamma incomplète
!	call gamma_incomplete(3/ksi,nu,rezult_1)						
!	call gamma_incomplete(5/ksi,nu,rezult_2)							
!!	rezult_3 :résultat de la fct gamma complète
!	CALL gamma_complete(3/ksi,rezult_3)
!
!!	mu : coefficient qui tient en compte du changement de pente de la courbe S-N
!	mu=1-(((rezult_1)-(rezult_2*(nu**(-2.0/ksi))))/rezult_3)		
!	
!!	DAMAGE		
!
!	Damage_Girder(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Girder(Fat_pts,MMT),MMT)=	&
!	Ntcyc*(NS_STRESS_Girder(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Girder(Fat_pts,MMT),MMT)**3.)	&
!						/(kpSN*(-LOG(PR))**(3./ksi))*mu*rezult_3
!
!ENDIF
!
!10	CONTINUE
	
!	end





!**********************************************************************************************
!**********************************************************************************************
!**********************************************************************************************

!	OPTIONS /EXTEND
!	SUBROUTINE FATIGUE_PILLAR
!	use param_section


!	CALL FAT_Check_Input


!    CALCULATION OF NOMINALS STRESSES  -------------------------------------
!    The submodule calculates nominal stress ranges at critical locations 
!    of structural member based on beam or plate theory. 
!	call PILLAR_NominalStress


!	 CALCULATION OF HOT-SPOT STRESSES --------------------------------------
!	 The submodele calculates structural stress ranges based on 
!	 semi-analytical approaches for stress concentration factors. 
!	 Library of different developed solutions is applied.
!	call PILLAR_HotspotStress



!	CALCULATION OF NOTCH STRESSES -----------------------------------------
!	The submodele calculates notch stress range with the help of additional 
!	stress concentration factor, which defines the relationship between 
!	hot-spot and notch stress.
!	call PILLAR_NotchStress

!	CALCULATION OF DAMAGE SUM----------------------------------------------
!	The submodule calculates damage sum based on linear damage theory and
!	Palmer-Miner rule. Submodel uses S-N curve, which includes the fatigue 
!	reduction and uncertainness due to dimensional accuracy, welding 
!	distortion, and environment. (FOR EACH LOAD CASE)
!	call PILLAR_Damage

!	end


!*****************************************************************************************
!SUBROUTINE PILLAR_NominalStress
!use param_section



!sy ep.+ - contr.supérieure, épontille 
!NOM_STRESS_Pillar1(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Pillar(Fat_pts),panneau(nel_fatigue).iy_Pillar(Fat_pts))=	&
!S_sup_Pillar(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Pillar(Fat_pts),panneau(nel_fatigue).iy_Pillar(Fat_pts))


!sy ep.- - contr. inférieure, épontille
!NOM_STRESS_Pillar2(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Pillar(Fat_pts),panneau(nel_fatigue).iy_Pillar(Fat_pts))=	&
!S_inf_Pillar(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Pillar(Fat_pts),panneau(nel_fatigue).iy_Pillar(Fat_pts))



!NOM_STRESS_Pillar(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Pillar(Fat_pts),panneau(nel_fatigue).iy_Pillar(Fat_pts))=	&
!max(abs(NOM_STRESS_Pillar1(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Pillar(Fat_pts),panneau(nel_fatigue).iy_Pillar(Fat_pts))),	&
!abs(NOM_STRESS_Pillar2(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Pillar(Fat_pts),panneau(nel_fatigue).iy_Pillar(Fat_pts))))&
!/10**6	!pour passer en MPa		!fat_new
!	NOM_STRESS_Pillar(1,nel_fatigue,panneau(nel_fatigue).ix_Pillar(Fat_pts),panneau(nel_fatigue).iy_Pillar(Fat_pts))=179.12
!	NOM_STRESS_Pillar(2,nel_fatigue,panneau(nel_fatigue).ix_Pillar(Fat_pts),panneau(nel_fatigue).iy_Pillar(Fat_pts))=52.48
!	NOM_STRESS_Pillar(3,nel_fatigue,panneau(nel_fatigue).ix_Pillar(Fat_pts),panneau(nel_fatigue).iy_Pillar(Fat_pts))=8.704
!	NOM_STRESS_Pillar(4,nel_fatigue,panneau(nel_fatigue).ix_Pillar(Fat_pts),panneau(nel_fatigue).iy_Pillar(Fat_pts))=15.371
!	NOM_STRESS_Pillar(5,nel_fatigue,panneau(nel_fatigue).ix_Pillar(Fat_pts),panneau(nel_fatigue).iy_Pillar(Fat_pts))=175.419
!	NOM_STRESS_Pillar(6,nel_fatigue,panneau(nel_fatigue).ix_Pillar(Fat_pts),panneau(nel_fatigue).iy_Pillar(Fat_pts))=120.035
!	NOM_STRESS_Pillar(7,nel_fatigue,panneau(nel_fatigue).ix_Pillar(Fat_pts),panneau(nel_fatigue).iy_Pillar(Fat_pts))=21.284



	
!END


!*****************************************************************************************

!    SUBROUTINE PILLAR_HotspotStress
!	use param_section
	
!	SUBMODULE FAT_HotspotStress !
!	*************************** !


! HS_STRESS : HOT SPOT STRESS

!	HS_STRESS_Pillar(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Pillar(Fat_pts),panneau(nel_fatigue).iy_Pillar(Fat_pts))=	&
!	panneau(nel_fatigue).KT_Pillar(Fat_pts)*NOM_STRESS_Pillar(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Pillar(Fat_pts),panneau(nel_fatigue).iy_Pillar(Fat_pts))



!	Sensibilités de DShs_1,DShs_2,DShs_3,DShs_4

!	CALL ANNUL4(dDShs_1,Neto,9,Fat_NSOLM,Neto)
!	CALL ANNUL4(dDShs_2,Neto,9,Fat_NSOLM,Neto)
!	CALL ANNUL4(dDShs_3,Neto,9,Fat_NSOLM,Neto)
!	CALL ANNUL4(dDShs_4,Neto,9,Fat_NSOLM,Neto)

	
!	DO IPAN=1,NETO
!	NBRXI=NVAR(IPAN)
!	L1=0
!	IF(NBRXI.EQ.0) GOTO 126
!	DO K=1,NBRXI
!	KK=NXIT(K,IPAN)
!	LL=L1+K
			
!		IF(nel_fatigue.EQ.IPAN) THEN	! sensibilités par rapport aux variables de conception du panneau considéré 
!			
!				dDShs_1(nel_fatigue,LL,casChge,IPAN)=dDS_1(nel_fatigue,LL,casChge,IPAN)*Kt_hs_1;
!				dDShs_2(nel_fatigue,LL,casChge,IPAN)=dDS_2(nel_fatigue,LL,casChge,IPAN)*Kt_hs_2;
!				dDShs_3(nel_fatigue,LL,casChge,IPAN)=dDS_3(nel_fatigue,LL,casChge,IPAN)*Kt_hs_3;
!				dDShs_4(nel_fatigue,LL,casChge,IPAN)=dDS_4(nel_fatigue,LL,casChge,IPAN)*Kt_hs_4;
!
!		ELSE					! sensibilités par rapport aux variables de conception des autres panneaux



!		ENDIF
!		ENDDO
 ! 126		CONTINUE
        
!	  	L1=L1+NBRXI
!		ENDDO


 !      END
	 
!*****************************************************************************************


!    SUBROUTINE PILLAR_NotchStress
!	use param_section

!	 SUBMODULE FAT_NotchStress !
!	************************* !
!

!

! NS_STRESS : NOTCH STRESS



!if (method_fatigue .eq.1) then	!DNV method_fatigue			

!	Kw_Pillar(nel_fatigue,Fat_pts)=1.5

!elseif (method_fatigue .eq.2) then	!BV method_fatigue			

!	Kw_Pillar(nel_fatigue,Fat_pts)=LAMBDA_Pillar(nel_fatigue,Fat_pts)*SQRT(TETA_Pillar(nel_fatigue,Fat_pts)/30)

!endif


! NOTCH STRESS NS_STRESS

!	NS_STRESS_Pillar(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Pillar(Fat_pts),panneau(nel_fatigue).iy_Pillar(Fat_pts))=	&
!	0.7*Kw_Pillar(nel_fatigue,Fat_pts)*HS_STRESS_Pillar(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Pillar(Fat_pts),panneau(nel_fatigue).iy_Pillar(Fat_pts))









!	Sensibilités de DSln_1,DSln_2,DSln_3,DSln_4				

!	CALL ANNUL4(dDSln_1,Neto,9,Fat_NSOLM,Neto)				
!	CALL ANNUL4(dDSln_2,Neto,9,Fat_NSOLM,Neto)				
!	CALL ANNUL4(dDSln_3,Neto,9,Fat_NSOLM,Neto)				
!	CALL ANNUL4(dDSln_4,Neto,9,Fat_NSOLM,Neto)				

	
!	DO IPAN=1,NETO				
!	NBRXI=NVAR(IPAN)				
!	L1=0
!	IF(NBRXI.EQ.0) GOTO 127				
!	DO K=1,NBRXI				
!	KK=NXIT(K,IPAN)				
!	LL=L1+K
!			
!!		IF(nel_fatigue.EQ.IPAN) THEN	! sensibilités par rapport aux variables de conception du panneau considéré 
!			
!			dDSln_1(nel_fatigue,LL,casChge,IPAN)=dDShs_1(nel_fatigue,LL,casChge,IPAN)*Kw(nel_fatigue,1);				
!			dDSln_2(nel_fatigue,LL,casChge,IPAN)=dDShs_2(nel_fatigue,LL,casChge,IPAN)*Kw(nel_fatigue,2);				
!			dDSln_3(nel_fatigue,LL,casChge,IPAN)=dDShs_3(nel_fatigue,LL,casChge,IPAN)*Kw(nel_fatigue,3);				
!			dDSln_4(nel_fatigue,LL,casChge,IPAN)=dDShs_4(nel_fatigue,LL,casChge,IPAN)*Kw(nel_fatigue,4);				
!
!		ELSE					! sensibilités par rapport aux variables de conception des autres panneaux
!
!
!
!!		ENDIF
!		ENDDO				
 ! 127		CONTINUE				
        
!	  	L1=L1+NBRXI				
!		ENDDO				
!


!if (method_fatigue.eq.2) then	!BV method_fatigue	


!	IF (NS_STRESS_Pillar(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Pillar(Fat_pts),panneau(nel_fatigue).iy_Pillar(Fat_pts)).EQ.0) GOTO 6

!		kc_Pillar(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Pillar(Fat_pts),panneau(nel_fatigue).iy_Pillar(Fat_pts))=	&
!		0.4*(sigy(indMateriau(nel_fatigue))/NS_STRESS_Pillar(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Pillar(Fat_pts),panneau(nel_fatigue).iy_Pillar(Fat_pts))) + 0.6
!
!	IF (kc_Pillar(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Pillar(Fat_pts),panneau(nel_fatigue).iy_Pillar(Fat_pts)).LE.0.8) THEN 								

!		kc_Pillar(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Pillar(Fat_pts),panneau(nel_fatigue).iy_Pillar(Fat_pts))=0.800											

!	ELSEIF(kc_Pillar(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Pillar(Fat_pts),panneau(nel_fatigue).iy_Pillar(Fat_pts)).GT.1.) THEN							

!		kc_Pillar(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Pillar(Fat_pts),panneau(nel_fatigue).iy_Pillar(Fat_pts))=1.												

!	ENDIF
																		
!6	CONTINUE

! NOTCH STRESS NS_STRESS	BV method_fatigue
!	NS_STRESS_Pillar(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Pillar(Fat_pts),panneau(nel_fatigue).iy_Pillar(Fat_pts))=	&
!	kc_Pillar(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Pillar(Fat_pts),panneau(nel_fatigue).iy_Pillar(Fat_pts))*	&
!	NS_STRESS_Pillar(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Pillar(Fat_pts),panneau(nel_fatigue).iy_Pillar(Fat_pts))	


!endif	!BV method_fatigue	



!	CALL ANNUL4(dKc_1,Neto,9,Fat_NSOLM,Neto)											
!	CALL ANNUL4(dKc_2,Neto,9,Fat_NSOLM,Neto)											
!	CALL ANNUL4(dKc_3,Neto,9,Fat_NSOLM,Neto)											
!	CALL ANNUL4(dKc_4,Neto,9,Fat_NSOLM,Neto)											

	
!	DO IPAN=1,NETO																		
!	NBRXI=NVAR(IPAN)																	
!	L1=0																				
!	IF(NBRXI.EQ.0) GOTO 528																
!	DO K=1,NBRXI																		
!	KK=NXIT(K,IPAN)																		
!	LL=L1+K																				
!			
!	IF (DSln_1(nel_fatigue,casChge).EQ.0) GOTO 10
!
!		IF ((Kc_1(nel_fatigue,casChge).LE.0.8).OR.(Kc_1(nel_fatigue,casChge).GE.1.)) THEN 											
!			dKc_1(nel_fatigue,LL,casChge,IPAN)=0.0																			
!		ELSE																													
!			dKc_1(nel_fatigue,LL,casChge,IPAN)=-0.4*sigy(indMateriau(nel_fatigue))*dDSln_1(nel_fatigue,LL,casChge,IPAN)/DSln_1(nel_fatigue,casChge)**2			
!		ENDIF																										
!
!10	CONTINUE

		
!	IF (DSln_2(nel_fatigue,casChge).EQ.0) GOTO 11

!		IF ((Kc_2(nel_fatigue,casChge).LE.0.8).OR.(Kc_2(nel_fatigue,casChge).GE.1.)) THEN 											
!			dKc_2(nel_fatigue,LL,casChge,IPAN)=0.0																			
!		ELSE																														
!			dKc_2(nel_fatigue,LL,casChge,IPAN)=-0.4*sigy(indMateriau(nel_fatigue))*dDSln_2(nel_fatigue,LL,casChge,IPAN)/DSln_2(nel_fatigue,casChge)**2				
!		ENDIF																										
!
!11	CONTINUE
!
!	IF (DSln_3(nel_fatigue,casChge).EQ.0) GOTO 12
!
!		IF ((Kc_3(nel_fatigue,casChge).LE.0.8).OR.(Kc_3(nel_fatigue,casChge).GE.1.)) THEN 											
!			dKc_3(nel_fatigue,LL,casChge,IPAN)=0.0																			
!		ELSE																														
!			dKc_3(nel_fatigue,LL,casChge,IPAN)=-0.4*sigy(indMateriau(nel_fatigue))*dDSln_3(nel_fatigue,LL,casChge,IPAN)/DSln_3(nel_fatigue,casChge)**2				
!		ENDIF
!																												
!12	CONTINUE

!	IF (DSln_4(nel_fatigue,casChge).EQ.0) GOTO 13

!		IF ((Kc_4(nel_fatigue,casChge).LE.0.8).OR.(Kc_4(nel_fatigue,casChge).GE.1.)) THEN								 			
!			dKc_4(nel_fatigue,LL,casChge,IPAN)=0.0																			
!		ELSE																														
!			dKc_4(nel_fatigue,LL,casChge,IPAN)=-0.4*sigy(indMateriau(nel_fatigue))*dDSln_4(nel_fatigue,LL,casChge,IPAN)/DSln_4(nel_fatigue,casChge)**2				
!		ENDIF																										
!
!13	CONTINUE
		
		
!	ENDDO															
 ! 528	CONTINUE															
        
!	L1=L1+NBRXI															
!	ENDDO															
	













!	Sensibilités FINALES de DSln_1,DSln_2,DSln_3,DSln_4

	
!	DO IPAN=1,NETO																		
!	NBRXI=NVAR(IPAN)																	
!	L1=0																				
!	IF(NBRXI.EQ.0) GOTO 628																
!	DO K=1,NBRXI																		
!	KK=NXIT(K,IPAN)																		
!	LL=L1+K																				
			

!			dDSln_1(nel_fatigue,LL,casChge,IPAN)=dKc_1(nel_fatigue,LL,casChge,IPAN)*DSln_1(nel_fatigue,casChge)+			
 !    *							Kc_1(nel_fatigue,casChge)*dDSln_1(nel_fatigue,LL,casChge,IPAN)						
!
!			dDSln_2(nel_fatigue,LL,casChge,IPAN)=dKc_2(nel_fatigue,LL,casChge,IPAN)*DSln_2(nel_fatigue,casChge)+			
 !    *							Kc_2(nel_fatigue,casChge)*dDSln_2(nel_fatigue,LL,casChge,IPAN)						
!
!			dDSln_3(nel_fatigue,LL,casChge,IPAN)=dKc_3(nel_fatigue,LL,casChge,IPAN)*DSln_3(nel_fatigue,casChge)+			
 !    *							Kc_3(nel_fatigue,casChge)*dDSln_3(nel_fatigue,LL,casChge,IPAN)						
     		
!			dDSln_4(nel_fatigue,LL,casChge,IPAN)=dKc_4(nel_fatigue,LL,casChge,IPAN)*DSln_4(nel_fatigue,casChge)+			
  !   *							Kc_4(nel_fatigue,casChge)*dDSln_4(nel_fatigue,LL,casChge,IPAN)						
 !    		
!	ENDDO																
 ! 628	CONTINUE															
        
!	L1=L1+NBRXI															
!	ENDDO																
						



!	ENDIF																	
		
!	END




!*****************************************************************************************

!    SUBROUTINE PILLAR_Damage
!	use param_section
!	real*8 rezult_gamma,rezult_1,rezult_2,rezult_3,ksi,nu,mu

!	integer m

!  SUBMODEL DAMAGE_SUM.m  !
! *********************** !
 


!	CHECKING OF STRESS VALUES and weibull factor

!	if (NS_STRESS_Pillar(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Pillar(Fat_pts),panneau(nel_fatigue).iy_Pillar(Fat_pts)).LT.0) THEN
!	WRITE(*,*) 'Error, notch stress range is negative'
!		pause 'stop'
!		stop
!	ENDIF
!
!
!	if ((Weib_fact_Pillar(nel_fatigue,Fat_pts).LT.0).OR.(Weib_fact_Pillar(nel_fatigue,Fat_pts).GT.4)) THEN
!	WRITE(*,*) 'Error, Weib_fact is negative or greater than 4'
!		pause 'stop'
!		stop
!	ENDIF
!
!
!
!
!
!if (method_fatigue .eq.1) then	!DNV method_fatigue
!
!!	basic shape parameter
!	h0 = 2.21 - 0.54*log10(length)		 
!
!!	additional factor depending on motion response period
!	ha = 0.05 !in general
!!	ha = 0.00 !for plating subjected to forces related to roll motions for vessels with roll period TR > 14 sec.
!
!!	Calcul de TA : periode moyenne, en secondes	
!	TA=4*LOG10(length)
!
!!	Number of load cycles during design life
!	Ntcyc=0.85*6.3*(10**8.0)/TA
!
!
!!	 CHECK 
!
!	IF (Ntcyc.LT.1.) THEN
!	WRITE(*,*) 'Input value error: Invalid number of load cycles (Ntcyc<1)'
!		PAUSE 'STOP'
!		STOP
!	ENDIF
!
!!	h_Weibull : Weibull shape parameter
!
!
!!	z(nel_fatigue,3) ! coord  y du noeud depart
!!	z(nel_fatigue,4) ! coord  y du noeud d'arrivee
!!	znel_fatigue = vertical distance from baseline to considered longitudinal (m)
!
!		znel_fatigue=abs(z(nel_fatigue,3)+(z(nel_fatigue,4)-z(nel_fatigue,3))*(panneau(nel_fatigue).iy_Pillar(Fat_pts)-1)/2)/1000
!
!	if (Weib_fact_Pillar(nel_fatigue,Fat_pts).EQ.1) then
!		h_Weibull_Pillar = h0																	!For deck longitudinals
!
!	elseif (Weib_fact_Pillar(nel_fatigue,Fat_pts).EQ.2) then
!
!		if ((znel_fatigue.GT.Tirant(Fat_NNSOL(caschge_fatigue))).AND.(znel_fatigue.LT.DEPTH)) THEN			!For ship side
!		h_Weibull_Pillar = h0 + ha*( DEPTH - znel_fatigue ) / ( DEPTH - Tirant(Fat_NNSOL(caschge_fatigue)))				!For ship side above the waterline Tirant < znel_fatigue <DEPTH
!	
!		elseif (znel_fatigue .EQ.Tirant(Fat_NNSOL(caschge_fatigue))) then
!		h_Weibull_Pillar = h0 + ha																	!For ship side at the waterline znel_fatigue = Tirant
!
!		elseif (znel_fatigue.LT.Tirant(Fat_NNSOL(caschge_fatigue))) then
!		h_Weibull_Pillar = h0 + ha*znel_fatigue/Tirant(Fat_NNSOL(caschge_fatigue))-0.005*(Tirant(Fat_NNSOL(caschge_fatigue))-znel_fatigue)		!For ship side below the waterline znel_fatigue < Tirant
!		
!		ENDIF
!
!	elseif (Weib_fact_Pillar(nel_fatigue,Fat_pts).EQ.3) then
!	h_Weibull_Pillar = h0 - 0.005*Tirant(Fat_NNSOL(caschge_fatigue))											!For bottom longitudinals 
!	
!	elseif (Weib_fact_Pillar(nel_fatigue,Fat_pts).EQ.4) then
!	h_Weibull_Pillar = h0 + ha																!For longitudinal and transverse bulkheads
!	endif
!
!
!	!	CALCULATION OF DAMAGE SUM D
!	CALL gamma_complete((m_Pillar(nel_fatigue,Fat_pts)/h_Weibull_Pillar),rezult_gamma)
!
!	Damage_Pillar(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Pillar(Fat_pts),panneau(nel_fatigue).iy_Pillar(Fat_pts))=	&
!	Ntcyc/C_Pillar(nel_fatigue,Fat_pts)*(NS_STRESS_Pillar(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Pillar(Fat_pts),panneau(nel_fatigue).iy_Pillar(Fat_pts))**m_Pillar(nel_fatigue,Fat_pts))*	&
!	(LOG(10.**8))**(-m_Pillar(nel_fatigue,Fat_pts)/h_Weibull_Pillar)*rezult_gamma;
!
!
 !
!
!
!
!
!!	Sensibilités de D_1,D_2,D_3,D_4
!
!!	CALL ANNULD(dD_1,NTOT)
!	CALL ANNULD(dD_2,NTOT)
!	CALL ANNULD(dD_3,NTOT)
!	CALL ANNULD(dD_4,NTOT)

	
!	DO IPAN=1,NETO
!	NBRXI=NVAR(IPAN)
!	L1=0
!	IF(NBRXI.EQ.0) GOTO 1
!	DO K=1,NBRXI
!	KK=NXIT(K,IPAN)
!	LL=L1+K
!			
!		IF(nel_fatigue.EQ.IPAN) THEN	! sensibilités par rapport aux variables de conception du panneau considéré 
!			
!
!			dD_1(nel_fatigue,LL,caschge_fatigue,IPAN)=Ntcyc/C*(m*DSln_1(nel_fatigue,caschge_fatigue)**(m-1)*dDSln_1(nel_fatigue,LL,caschge_fatigue,IPAN))* 
 !    *					(LOG(10.**8))**(-m/h_Weibull)*rezult_gamma;
!			dD_2(nel_fatigue,LL,caschge_fatigue,IPAN)=Ntcyc/C*(m*DSln_2(nel_fatigue,caschge_fatigue)**(m-1)*dDSln_2(nel_fatigue,LL,caschge_fatigue,IPAN))* 
 !    *					(LOG(10.**8))**(-m/h_Weibull)*rezult_gamma;
!			dD_3(nel_fatigue,LL,caschge_fatigue,IPAN)=Ntcyc/C*(m*DSln_3(nel_fatigue,caschge_fatigue)**(m-1)*dDSln_3(nel_fatigue,LL,caschge_fatigue,IPAN))* 
 !    *					(LOG(10.**8))**(-m/h_Weibull)*rezult_gamma;
!			dD_4(nel_fatigue,LL,caschge_fatigue,IPAN)=Ntcyc/C*(m*DSln_4(nel_fatigue,caschge_fatigue)**(m-1)*dDSln_4(nel_fatigue,LL,caschge_fatigue,IPAN))* 
 !    *					(LOG(10.**8))**(-m/h_Weibull)*rezult_gamma;



!		ELSE					! sensibilités par rapport aux variables de conception des autres panneaux



!		ENDIF
!		ENDDO
 ! 1		CONTINUE
        
!	  	L1=L1+NBRXI
!		ENDDO

!*******************************************************************************
	
!elseif(method_fatigue.EQ.2) then		!BV method_fatigue


!	probabilité d'excédence 
!	PR=10.**(-5.)

!	Calcul de TA : periode moyenne, en secondes	
!	TA=4*LOG10(length)

!	alfa0 : facteur de navigation pris égal à 0.8
!	alfa0=0.85

!	Calcul de Ntcyc	: nombre total des cycles
!	Ntcyc=631*alfa0/TA*10**6.0

!	Calcul de Ntfl (Tfl:Increased design life (between 25 and 40))
!	Ntfl=31.55*alfa0*Tfl*10**6.0/TA
	
!	IF(Tfl.GE.25) THEN
!	CFL=LOG10(0.2*LOG10(Ntfl))/LOG10(0.2*LOG10(Ntcyc))
!	else
!	CFL=1.
!	endif
!
!!	Calcul de ksi,ksi0 : paramêtres de forme de la fonction de WEIBULL
!	ksi0=(73-0.07*length)/60*CFL
!	IF(ksi0.LT.0.85) THEN
!	ksi0=0.85
!	ENDIF
!

!	CALL ANNUL2(Damage_Stif,nsolm_fatigue,NETO,ix_max,iy_max)
!	CALL ANNUL2(Damage_Plate,nsolm_fatigue,NETO,ix_max,iy_max)
!	CALL ANNUL2(Damage_Plate_frame,nsolm_fatigue,NETO,ix_max,iy_max)
!	CALL ANNUL2(Damage_frame,nsolm_fatigue,NETO,ix_max,iy_max)
!	CALL ANNUL2(Damage_Girder,nsolm_fatigue,NETO,ix_max,mt_max)
!	CALL ANNUL2(Damage_Pilar,nsolm_fatigue,NETO,ix_max,iy_max)


!	CALL ANNUL4(dD_1,Neto,9,nsolm_fatigue,Neto)
!	CALL ANNUL4(dD_2,Neto,9,nsolm_fatigue,Neto)
!	CALL ANNUL4(dD_3,Neto,9,nsolm_fatigue,Neto)
!	CALL ANNUL4(dD_4,Neto,9,nsolm_fatigue,Neto)

!	if(Sty.EQ.1) then

!	damage : Hot spot 1
			

!     characteristics if the stiffened elements (epsa,entr,delta,hxr,dxr,wxr,txr,sigy(indMateriau(nel_fatigue)),sigy(indMateriau(nel_fatigue)))
!     ------------------------------------------
!  .  sl = epsa : length of stiffener between transverse frames (m.)
!  .  bp = entr : longitudinal stiffener spacing   (m.)
!  .  tp = delta: thickness of parent plate        (m.)
!  .  hw = hxr  : height of stiffener web          (m.)
!  .  tw = dxr  : thickness of stiffener web       (m.)
!  .  bf = wxr  : breadth of stiffener flange      (m.)
!  .  tf = txr  : thickness of stiffener flange    (m.)
!  .  syp= sigy(indMateriau(nel_fatigue)) : yield strength of parent plate   (n/m2)   
!  .  syw= sigy(indMateriau(nel_fatigue)) : yield strength of stiffener      (n/m2)   

!   les variables de conception  ; nbrxi = nvar(nel_fatigue,iboat)
!   -------------------------------------------------
!       1       delta =  épaisseur du bordage
!       2       hya   =  hauteur de l'âme des cadres   
!       3       dya   =  épaisseur de l'âme des cadres   
!       4       wya   =  largeur des semelles des cadres   
!       5       epsa  =  entredistance entre cadres   
!       6       hxr   =  hauteur de l'âme des raidisseurs
!       7       dxr   =  épaisseur de l'âme des raidisseurs
!       8       wxr   =  largeur des semelles des raidisseurs
!       9       epsr  =  entredistance entre raidisseurs

! 	autres paramètres :
!	 	tya = épaisseur semelle aiguilles
!     	txr = épaisseur semelle raidisseurs 
!
!  	Les variables de conception pour epontille
!     ------------------------------------------					
!	1	hya		=	hauteur demi âme ou diam ou côté ext.	
!	2	dya		=	épaisseur d'âme							
!	3	wya		=	largeur de semelle						
!	4	epais	=	épaisseur de paroi mince				
!	5	epsa	=	entredistance entre épontillesc


!	thF	: épaisseur de l'élément considéré (mm) (minimum thickness of connected elements)

!	znel_fatigue=abs(z(nel_fatigue,3)+(z(nel_fatigue,4)-z(nel_fatigue,3))*(panneau(nel_fatigue).iy_Pillar(Fat_pts)-1)/2)/1000

!	thF=panneau(nel_fatigue).txr*1000
!	write (*,*) 'thF Pillar pas encore défini'
!	pause 
!	stop


!	Calcul de kpSN : constante de la Design S-N curve
!	IF(thF.LT.22) THEN
!	kpSN=5.802*(10**12.0)
!	ELSE
!	kpSN=5.802*(10**12.0)*(22.0/thF)**(0.9)
!	ENDIF
!

!	Calcul de SigmaQ : étendue(sigmaMAx-sigmaMin) de contrainte à l'intersection des 02 segments de la courbe S-N.
!	SigmaQ=(kpSN*(10**(-7.0)))**(1.0/3.0)

!	ksi=ksi0*(1.04-0.14*ABS(znel_fatigue-Tirant(Fat_NNSOL(caschge_fatigue)))/(DEPTH-Tirant(Fat_NNSOL(caschge_fatigue))))

!	CALL ANNULD(nu,nsolm_fatigue)

!	nu1=0.0
!	nu2=0.0
!
!if (NS_STRESS_Pillar(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Pillar(Fat_pts),panneau(nel_fatigue).iy_Pillar(Fat_pts)).EQ.0) GOTO 10
!
!!	nu : coefficient utilisé pour le calcul de la fonction GAMMA incomplète.
!	nu=-((SigmaQ/NS_STRESS_Pillar(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Pillar(Fat_pts),panneau(nel_fatigue).iy_Pillar(Fat_pts)))**(ksi))*LOG(PR)					

!	rezult_1 et rezult_2:résultats de la fct gamma incomplète
!	call gamma_incomplete(3/ksi,nu,rezult_1)						
!	call gamma_incomplete(5/ksi,nu,rezult_2)							
!!	rezult_3 :résultat de la fct gamma complète
!	CALL gamma_complete(3/ksi,rezult_3)

!	mu : coefficient qui tient en compte du changement de pente de la courbe S-N
!	mu=1-(((rezult_1)-(rezult_2*(nu**(-2.0/ksi))))/rezult_3)		
	
!	DAMAGE		

!	Damage_Pillar(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Pillar(Fat_pts),panneau(nel_fatigue).iy_Pillar(Fat_pts))=	&
!	Ntcyc*(NS_STRESS_Pillar(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Pillar(Fat_pts),panneau(nel_fatigue).iy_Pillar(Fat_pts))**3.)	&
!						/(kpSN*(-LOG(PR))**(3./ksi))*mu*rezult_3

!10	CONTINUE
	
!ENDIF


!end
