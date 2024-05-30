	OPTIONS /EXTEND

 ! MAIN PROGRAM OF FATIGUE MODULE  
! ******************************** C

    SUBROUTINE Fatigue_Damage
	use param_section
!    integer IS_FATIGUE_A,nsol_fatigue_A,Neto_A
!    character FAT_DON*80	!,RES1*80
!	integer nsolm_A, nsol_A
!	integer, allocatable, save :: NNSOL_A(:)
!	integer, allocatable, save :: is_loadcase_fatigue_A(:)
!	integer, allocatable, save :: Fat_NNSOL_A(:)


!sigy1=sigy(indMateriau(nel_fatigue))


	WRITE(*,*)
    WRITE(*,108) 'FATIGUE'
	WRITE(*,108) '======='

call CHECK_input1	!(IS_FATIGUE_A,Neto_A,nsol_A,nnsol_A,IS_LOADCASE_FATIGUE_A,nsolm_fatigue_A,nsol_fatigue_A,nsolm_A)


! Calcul fatigue

call FATIGUE 



! Formats .....................................................................

! Rem de Fred : Variable ERREUR is used before its value has been defined
  999  IF (erreur.GT.0) THEN
		WRITE(*,*) 'STOP : The FATIGUE File is missing'	
		PAUSE 'STOP'
		STOP
	ENDIF
	 
  906  IF (erreur.GT.0) THEN
	 WRITE(*,*) 'STOP : The "FATIGUE_FILE_NAME.txt" File is missing'		!Modif Batch
		PAUSE 'STOP'
		STOP
	ENDIF
  
  108  FORMAT (A8) 
     
	END
!******************************************************************************************************************** 

SUBROUTINE FATIGUE 
use param_section

T1=secnds(0.0)
T2=secnds(0.0)

PI=2.d00*asin(1.d00)

! List of variables
! Part_full				Part of the ship’s life in full load condition, given in Tab 9 (BV rules, PtB,Ch7,Sec 4)	
! method_fatigue				DAMAGE CALCULATION method_fatigue. IF 1 : DNV method_fatigue , ELSEIF 2 BV method_fatigue	
! Tfl					Increased design life (between 25 and 40).if no incrfease of design life, Tfl=20
! depth 				depth of the vessel          (m.)
! NNSOL_BV_TYPE(I)		Specification of the nature of each Fat_nsol fatigue load cases
!		if NNSOL_BV_TYPE(I) = 1 =>!load case "a", "full loading condition"	
!		if NNSOL_BV_TYPE(I) = 2 =>!load case "b", "full loading condition"	
!		if NNSOL_BV_TYPE(I) = 3 =>!load case "c", "full loading condition"	
!		if NNSOL_BV_TYPE(I) = 4 =>!load case "d", "full loading condition"			
!		if NNSOL_BV_TYPE(I) = 5 =>!load case "a", "ballast loading condition"				
!		if NNSOL_BV_TYPE(I) = 6 =>!load case "b", "ballast loading condition"				
!		if NNSOL_BV_TYPE(I) = 7 =>!load case "c", "ballast loading condition"				

! itype= 1  coque       coque  standard
! itype= 2  plaque      plaque standard
! itype= 3  coque1      coque  sans contribution transversale du bordé
! itype= 4  plaque1     plaque sans contribution transversale du bordé
! itype= 5  epontille   epontille élément poutre

!***************** Read of General Fatigue Input*********************************


!	read(11,*) sens_A			!SI SENS=1 :calcul des sensibilités par différences fines pour comparaison	!NEW3

!	tirant(i) = list of draughts for each load case
!do i=1,nsolm
!read(11,*) tirant(i)
!Enddo   

!READ(11,*) Part_full,method_fatigue,Tfl,depth,length
!READ(11,*) (NNSOL_BV_TYPE(I),I=1,nsol_fatigue)





!Read of Panel_fatigues Fatigue Inputs *********************************************

!CALL FAT_Read_Input_Panel_fatigues

!***************************************************************************

!Create Output file


DO nel_fatigue=1,Neto


if (panneau(nel_fatigue).I_Fat_Stif .ne. 0) THEN
WRITE(*,117) 'PANEL',nel_fatigue			
endif


call CHECK_stif1
call CHECK_Plate1		!fat_new

	DO caschge_fatigue=1,nsol_fatigue

!	caschge_fatigue=Fat_NNSOL(J)

	! WRITE(*,106) 'cas_Charge :',caschge_fatigue										

	If(itype(nel_fatigue).ne.5) then		!Panel_fatigue

	!stiffeners--------------------------------------------------------------

	if (panneau(nel_fatigue).I_Fat_Stif .NE. 0) THEN

	! WRITE(*,*) 'STIFFENERS HOT SPOTS'

	DO Fat_pts=1,panneau(nel_fatigue).nbr_Fat_Stif

	! WRITE(*,114) 'IX',panneau(nel_fatigue).ix_Stif(Fat_pts),'IY',panneau(nel_fatigue).iy_stif(Fat_pts)									

	call CHECK_stif2(Fat_pts)		!fat_new

	CALL FATIGUE_STIFFENER

	enddo
	endif

	!Plates--------------------------------------------------------------

	if (panneau(nel_fatigue).I_Fat_Plate .NE. 0) THEN



	! WRITE(*,*) 'PLATES HOT SPOTS'

	DO Fat_pts=1,panneau(nel_fatigue).nbr_Fat_Plate

	call CHECK_Plate2(Fat_pts)		!fat_new

	! WRITE(*,114) 'IX',panneau(nel_fatigue).ix_Plate(Fat_pts),'IY',panneau(nel_fatigue).iy_Plate(Fat_pts)									
	
	CALL FATIGUE_PLATE

	enddo
	endif

	!Plates_Frames--------------------------------------------------------------

	if (panneau(nel_fatigue).I_Fat_Plate_Frame .NE. 0) THEN



	! WRITE(*,*) 'PLATES/FRAME INTERSECTION HOT SPOTS'

	DO Fat_pts=1,panneau(nel_fatigue).nbr_Fat_Plate_Frame

	call CHECK_Plate_Frame(Fat_pts)		!fat_new

	! WRITE(*,114) 'IX',panneau(nel_fatigue).ix_Plate_Frame(Fat_pts),'IY',panneau(nel_fatigue).iy_Plate_Frame(Fat_pts)									

	CALL FATIGUE_PLATE_FRAME

	enddo
	endif

	!Plates_Frames--------------------------------------------------------------

	if (panneau(nel_fatigue).I_Fat_Frame .NE. 0) THEN



	! WRITE(*,*) 'FRAME HOT SPOTS'

	DO Fat_pts=1,panneau(nel_fatigue).nbr_Fat_Frame

	call CHECK_Frame(Fat_pts)		!fat_new

	! WRITE(*,114) 'IX',panneau(nel_fatigue).ix_Frame(Fat_pts),'IY',panneau(nel_fatigue).iy_Frame(Fat_pts)									

	CALL FATIGUE_FRAME

	enddo
	endif

	!Girders--------------------------------------------------------------

!	If (MT(nel_fatigue).NE.0) Then
!
!	call CHECK_Girder1

!	! WRITE(*,*) 'GIRDERS HOT SPOTS'
!
!		Do MMT=1,MT(nel_fatigue)
!
!			if (I_Fat_Girder(nel_fatigue,MMT) .NE. 0) THEN
!
!				DO Fat_pts=1,nbr_Fat_Girder(nel_fatigue,MMT)
!
!					call CHECK_Girder2(Fat_pts)

!					! WRITE(*,114) 'MT',MT(nel_fatigue),'IX',ix_GIRDER(nel_fatigue,Fat_pts,MMT)
!
!					CALL FATIGUE_GIRDER
!
!				enddo
!			endif
!		
!		endDO
!
!	ENDIF
	
	ELSEIf(itype(nel_fatigue).EQ.5) then		!Pillar

	!Pillars--------------------------------------------------------------

!	if (I_Fat_Pillar(nel_fatigue) .NE. 0) THEN

!			call CHECK_Pillar1

!	! WRITE(*,*) 'PILLAR HOT SPOTS'

!	DO Fat_pts=1,nbr_Fat_Pillar(nel_fatigue)

!					call CHECK_Pillar2(Fat_pts)

!	! WRITE(*,114) 'IX',ix_PILLAR(nel_fatigue,Fat_pts),'IY',iy_PILLAR(nel_fatigue,Fat_pts)									
	
!	CALL FATIGUE_PILLAR

!	enddo
!	endif

	ENDIF

enddo
enddo

!		call input_file_matlab
!		if (sens.EQ.1) THEN 

! 		WRITE(66,*) '----------------------------------------------------------------------'
!		WRITE(66,*)
! 		WRITE(66,*) 'STIFFENED PLATE------------------------------------------------------'

!		! WRITE(*,*) 'call sensib_Diff_finies'			

!		call sensib_Diff_finies
!		end if




!	CALL INPUT_RECAPITULATIF			



! 		Results

! 		NOMINAL STRESS RANGE
!			DS_21_2(nel_fatigue,caschge_fatigue)=DS_1(nel_fatigue,caschge_fatigue)
!			DS_22_2(nel_fatigue,caschge_fatigue)=DS_2(nel_fatigue,caschge_fatigue)
!			DS_23_2(nel_fatigue,caschge_fatigue)=DS_3(nel_fatigue,caschge_fatigue)
!			DS_24_2(nel_fatigue,caschge_fatigue)=DS_4(nel_fatigue,caschge_fatigue)

! 		HOT SPOT STRESS RANGE
!			DShs_21_2(nel_fatigue,caschge_fatigue)=DShs_1(nel_fatigue,caschge_fatigue)
!			DShs_22_2(nel_fatigue,caschge_fatigue)=DShs_2(nel_fatigue,caschge_fatigue)
!			DShs_23_2(nel_fatigue,caschge_fatigue)=DShs_3(nel_fatigue,caschge_fatigue)
!			DShs_24_2(nel_fatigue,caschge_fatigue)=DShs_4(nel_fatigue,caschge_fatigue)

! 		NOTCH STRESS RANGE
!			DSln_21_2(nel_fatigue,caschge_fatigue)=DSln_1(nel_fatigue,caschge_fatigue)
!			DSln_22_2(nel_fatigue,caschge_fatigue)=DSln_2(nel_fatigue,caschge_fatigue)
!			DSln_23_2(nel_fatigue,caschge_fatigue)=DSln_3(nel_fatigue,caschge_fatigue)
!			DSln_24_2(nel_fatigue,caschge_fatigue)=DSln_4(nel_fatigue,caschge_fatigue)

! 		DAMAGE
!			D_21_2(nel_fatigue,caschge_fatigue)=D_1(nel_fatigue,caschge_fatigue)
!			D_22_2(nel_fatigue,caschge_fatigue)=D_2(nel_fatigue,caschge_fatigue)
!			D_23_2(nel_fatigue,caschge_fatigue)=D_3(nel_fatigue,caschge_fatigue)
!			D_24_2(nel_fatigue,caschge_fatigue)=D_4(nel_fatigue,caschge_fatigue)
!




! CALCULATION OF TOTAL CUMULATIVE DAMAGE ----------------------------------------------
! The submodule calculates CUMULATIVE DAMAGE based on BV rules 2007, Pt, Ch7, Sec 4

if(method_fatigue.EQ.2) then		!BV method_fatigue

CALL TOTAL_DAMAGE
				
ENDIF





! RESULTS REPORTING -----------------------------------------------------

! WRITE(*,*) 'FAT_Output'								
call FAT_Output	

 	
	
	
	
	
	
!	sensibilités_Diff_finies	
	
		
!	do nel_fatigue=1,Neto		
!
!	if (sens.EQ.1) THEN
!
!	! WRITE(*,113) PAnel_fatigue(nel_fatigue)									
!
!	! WRITE(*,*) 'sensib_damage_Diff_finies'								
!
!	CALL sensib_damage_Diff_finies
!	ENDIF
!
!	enddo
!	endif


	A1=secnds(T1)
	A2=secnds(T2)
	! WRITE(*,*)'calculation time before writing results :', A1
	! WRITE(*,*)'calculation time after writing results  :',A2




 105  FORMAT (A48,F20.3) 
 106  FORMAT (A10,I3) 
 107  FORMAT (A38,E15.8) 
!  107  FORMAT (A38,F20.3) 
 108  FORMAT (A50) 
 109  FORMAT (9E12.2) 
 110  FORMAT (A10,F30.3)
 111  FORMAT (A10,A30)
 112  FORMAT (A10,I30) 
 113  FORMAT (A50)			
 114  FORMAT (A10,I3,A10,I3)  
 
 117  FORMAT (A6,I3) 			

	END


!*************************************************************************************************************************

!      SUBROUTINE ANNULD(S,N)
 !     IMPLICIT REAL*8 (A-H,O-Z)
 !     DIMENSION S(N)
  !    DO I=1,N

!		S(I)=0.D00

 !     ENDDO
  !    RETURN
   !   END
!****************************************

 
      SUBROUTINE ANNUL2(S,N,M)
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION S(N,M)
      DO I=1,N
		DO J=1,M
		S(I,J)=0.D00
		ENDDO
      ENDDO
      RETURN
      END
!****************************************

 
      SUBROUTINE ANNUL3(S,N,M,L)
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION S(N,M,L)

      DO I=1,N 
	DO J=1,M
		DO K=1,L
		S(I,J,K)=0.D00
		ENDDO
      ENDDO
	ENDDO

      RETURN
      END

!****************************************

 
      SUBROUTINE ANNUL4(S,N,M,L,H)
	integer N,M,L,H 
      REAL*8 S
      DIMENSION S(N,M,L,H)



      DO I=1,N 
	DO J=1,M
		DO K=1,L
		DO G=1,H
		S(I,J,K,G)=0.D00
		ENDDO
		ENDDO
	ENDDO
	ENDDO

      RETURN
      END





!  Dérivées :SENS4(J,I,K,IPAN) Valeurs cumulees.
 !    avec      J=1,9   les fonctions (ex: pour W, J=1)
! 			   W,Wø,Wøø,Wøøø,U,Uø,V,Vø,Vøø
 ! 	           1 2  3   4    5 6  7 8  9 
!  			 I=1,IPTS les points de calcul (correspondant aux pts de calcul des restrictions et des sensibilites)
!  			 K=1,9    les variables de conception (si Ep. bord., K=1)
 !          IPAN=1,NETO le panneau relatif à ces K variables de conception



 ! SUBROUTINE CONTRaintes (c.à.d) "RESTRICTIONS"
 ! ==============================================
 ! Calcul des M restrictions. C(j) du pan. nel_fatigue , (j=1+IM,M+IM)  (max 7000 pour la struct.)
 ! IM = Nbre de restrictions déjà considérées par les panneaux 1 à nel_fatigue-1.
!
 ! Calcul des NBRXI dérivées des restrictions DC(i,j), i=1,NBRXI [NBRXI=NVAR(nel_fatigue)]
 ! IPTS = Nbre de points de calcul des restrictions (par panneau) (max 10)
 ! YPTS = Ordonnées de ces IPTS points de calcul des restrictions (par panneau)
! LCONT(1,j+IM) = Nø de référence de la restriction j du panneau nel_fatigue
 !                 Ex: 11 pour Sigma Comp Bordage
! LCONT(2,j+IM) = Nø du pt d'applic. de la restr. j du panneau nel_fatigue (sensibilité)
 !               = 1 à 10
 ! IPT2(i,nel_fatigue) = Nø des IPT pts d'applic. pour le panneau nel_fatigue (1 à 31)
! IPT2(LCONT(2,j+IM),nel_fatigue) = Nø du pt d'applic. de la restr. j du panneau nel_fatigue (1 à 31).
! IPT3(LCONT(2,j+IM),nel_fatigue) = Nø de la Trav. correspondant au pt IPT (pour le panneau nel_fatigue)


! SENS1 est relatif au panneau étudié par BO2 cad le panneau nel_fatigue,
 !  mais il donne les sensibilités pour les variables de conception
 !  de tous les panneaux (IPAN)


! sensibilités relatives au panneau nel_fatigue,mais pour les variables de conception de tous les panneaux (IPAN)








!--------------------------------------------------------------------------
	subroutine gamma_complete(X,rezult)

	integer i,n

	real*8 a1,X,nu,h
	real*8 AA,BB,rezult
	real*8 x12,y12		!x_i,y_i,
	dimension x_p(1000),y_p(1000),x12(1000),y12(1000)

	n=400
	a1=0.
	nu=500
	h=(nu-a1)/n

	x_p(1)=a1
	x_p(n+1)=nu

!     fonction à integrer : t^X*exp(-t) dt

!	y_p(1)=x_p(1)**X*EXP(-x_p(1))	!FAT_NEW
	y_p(1)=0.0	!FAT_NEw

	y_p(n+1)=x_p(n+1)**X*EXP(-x_p(n+1))

	AA=0.0

	do i=2,n
	  x_p(i)=x_p(1)+(i-1)*h
	  y_p(i)=x_p(i)**X*EXP(-x_p(i))
	  AA=AA+y_p(i)
	enddo

	BB=0.0

	do j=1,n
	  x12(j)=(x_p(j)+x_p(j+1))/2
	  y12(j)=x12(j)**X*EXP(-x12(j))
	  BB=BB+y12(j)
	enddo

	rezult=(h/6)*(y_p(1)+2*AA+4*BB+y_p(n+1))

	end !gamma_complete
!--------------------------------------------------------------------------


!--------------------------------------------------------------------------
	subroutine gamma_incomplete(X,nu,rezult)
!	use param_section
!	integer i,n
!
!	real*8 a1,X,nu,h,rezult
!	real*8 AA,BB
!	real*8 x12,y12			!x_i,y_i,
!	dimension x_p(1000),y_p(1000),x12(1000),y12(1000)
!	
!
!
!
!	n=400
!	a1=0.
!!	nu=500
!	h=(nu-a1)/n
!
!	x_p(1)=a1
!	x_p(n+1)=nu
!
!!     fonction à integrer : t^X*exp(-t) dt




	integer*4 i,n

	double precision a1,X,nu,h,rezult
	double precision AA,BB,bbbb2
	!real*8 x12,y12			!x_i,y_i,
	double precision x_p(1000),y_p(1000),x12(1000),y12(1000)
	



	n=400
	a1=0.
!	nu=500
	h=(nu-a1)/n

	x_p(1)=a1
	x_p(n+1)=nu

!     fonction à integrer : t^X*exp(-t) dt
!aa=0.0
!aaaa=EXP(-x_p(1))
!EEE=(1.0)**(X)
!bbbb1=x_p(1)**(2)
!bbbb3=aa**(2)
!bbbb2=x_p(1)**(2.1)
!bbbb=x_p(1)**(X)


!	y_p(1)=x_p(1)**X*EXP(-x_p(1))
y_p(1)=0.0

!
!cccc=EXP(-x_p(n+1))
!dddd=x_p(n+1)**X
	y_p(n+1)=x_p(n+1)**X*EXP(-x_p(n+1))


!	CALL ANNUL3(dh,Neto,9,Neto)
!	CALL ANNUL4(dx_p,401,Neto,9,Neto)
!	CALL ANNUL4(dy_p,401,Neto,9,Neto)

	
!	DO IPAN=1,NETO
!	NBRXI=NVAR(IPAN)
!	L1=0
!	IF(NBRXI.EQ.0) GOTO 113
!	DO K=1,NBRXI
!	KK=NXIT(K,IPAN)
!	LL=L1+K
!			
!
!				
!		dh(nel_fatigue,LL,IPAN)=dnu(nel_fatigue,LL,caschge_fatigue,IPAN)/n
!
!		dx_p(n+1,nel_fatigue,LL,IPAN)=dnu(nel_fatigue,LL,caschge_fatigue,IPAN)
!
!		dy_p(n+1,nel_fatigue,LL,IPAN)=X*dx_p(n+1,nel_fatigue,LL,IPAN)*(x_p(n+1)**(X-1))*EXP(-x_p(n+1)) +  
 !    *			x_p(n+1)**X * (-dx_p(n+1,nel_fatigue,LL,IPAN))*EXP(-x_p(n+1)) 
!
!
!			
!	ENDDO
 ! 113	CONTINUE
  !      
!	L1=L1+NBRXI
!	ENDDO




	AA=0.0

!	CALL ANNUL3(dAA,Neto,9,Neto)
	do i=2,n
	  x_p(i)=x_p(1)+(i-1)*h
	  y_p(i)=x_p(i)**X*EXP(-x_p(i))
	  AA=AA+y_p(i)



!		DO IPAN=1,NETO
!		NBRXI=NVAR(IPAN)
!		L1=0
!		IF(NBRXI.EQ.0) GOTO 114
!		DO K=1,NBRXI
!		KK=NXIT(K,IPAN)
!		LL=L1+K
!			

!				IF(KK.EQ.1) THEN		
!		  dx_p(i,nel_fatigue,LL,IPAN)=(i-1)*dh(nel_fatigue,LL,IPAN)
!		  dy_p(i,nel_fatigue,LL,IPAN)=X*(x_p(i)**(X-1))*dx_p(i,nel_fatigue,LL,IPAN)*EXP(-x_p(i)) +  
!     *						(x_p(i)**(X))*(-dx_p(i,nel_fatigue,LL,IPAN))*EXP(-x_p(i))
!		  dAA(nel_fatigue,LL,IPAN)=dAA(nel_fatigue,LL,IPAN)+dy_p(i,nel_fatigue,LL,IPAN)


			
!		ENDDO
 ! 114		CONTINUE
        
!		L1=L1+NBRXI
!		ENDDO


	enddo






	BB=0.0

!	CALL ANNUL3(dBB,Neto,9,Neto)
	do j=1,n
	  x12(j)=(x_p(j)+x_p(j+1))/2
	  y12(j)=x12(j)**X*EXP(-x12(j))
	  BB=BB+y12(j)

	

!		DO IPAN=1,NETO
!		NBRXI=NVAR(IPAN)
!		L1=0
!		IF(NBRXI.EQ.0) GOTO 115
!		DO K=1,NBRXI
!		KK=NXIT(K,IPAN)
!		LL=L1+K
!			
!						
!		 dx12(j,nel_fatigue,LL,IPAN)=(dx_p(j,nel_fatigue,LL,IPAN)+dx_p(j+1,nel_fatigue,LL,IPAN))/2
!		 dy12(j,nel_fatigue,LL,IPAN)=X * dx12(j,nel_fatigue,LL,IPAN) * (x12(j)**(X-1)) *  
 !    *		EXP(-x12(j)) +  (x12(j)**X) * (-dx12(j,nel_fatigue,LL,IPAN)) * EXP(-x12(j))
!
!		 dBB(nel_fatigue,LL,IPAN)=dBB(nel_fatigue,LL,IPAN)+dy12(j,nel_fatigue,LL,IPAN)

			
!		ENDDO
 ! 115		CONTINUE
 !       
!		L1=L1+NBRXI
!		ENDDO

	enddo

	rezult=(h/6)*(y_p(1)+2*AA+4*BB+y_p(n+1))


!	CALL ANNUL4(drezult,Neto,9,nsolm_fatigue,Neto)
		
!		DO IPAN=1,NETO
!		NBRXI=NVAR(IPAN)
!		L1=0
!		IF(NBRXI.EQ.0) GOTO 116
!		DO K=1,NBRXI
!		KK=NXIT(K,IPAN)
!		LL=L1+K
!			
!
!	
!		drezult(nel_fatigue,LL,caschge_fatigue,IPAN)=(dh(nel_fatigue,LL,IPAN)/6.)*(y_p(1)+2*AA+4*BB+y_p(n+1)) +  
 !    *			(h/6.)*(dy_p(1,nel_fatigue,LL,IPAN)+ 
  !   *			2*dAA(nel_fatigue,LL,IPAN)+4*dBB(nel_fatigue,LL,IPAN)+dy_p(n+1,nel_fatigue,LL,IPAN))
!				

!			
!		ENDDO
 ! 116		CONTINUE
        
!		L1=L1+NBRXI
!		ENDDO
!


	end !gamma_complete
	
!--------------------------------------------------------------------------

!****************************************************************************************************
SUBROUTINE TOTAL_DAMAGE
use param_section






do nel_fatigue=1,Neto

If(itype(nel_fatigue).ne.5) then		!Panel_fatigue

	!stiffeners--------------------------------------------------------------

	if (panneau(nel_fatigue).I_Fat_Stif .NE. 0) THEN

	FATIGUE_INDEX=1

	DO Fat_pts=1,panneau(nel_fatigue).nbr_Fat_Stif

	! WRITE(*,*) 'PAnel_fatigue',nel_fatigue,'IX_Stif',panneau(nel_fatigue).ix_Stif(Fat_pts),'IY_Stif',panneau(nel_fatigue).iy_stif(Fat_pts)
	! WRITE(*,*) 'CALL_TOTAL_DAMAGE'									

	do caschge_fatigue=1,nsol_fatigue  				

	   kk=NNSOL_BV_TYPE(caschge_fatigue)				

		if (kk.EQ.1) then	!damage for load case "a", "full loading condition"				

		Damage_Stif_af=Damage_Stif(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Stif(Fat_pts),panneau(nel_fatigue).iy_stif(Fat_pts))

		elseif (kk.EQ.2) then	!damage for load case "b", "full loading condition"				

		Damage_Stif_bf=Damage_Stif(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Stif(Fat_pts),panneau(nel_fatigue).iy_stif(Fat_pts))
		
		elseif (kk.EQ.3) then	!damage for load case "c", "full loading condition"				
		
		Damage_Stif_cf=Damage_Stif(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Stif(Fat_pts),panneau(nel_fatigue).iy_stif(Fat_pts))
		
		elseif (kk.EQ.4) then	!damage for load case "d", "full loading condition"				

		Damage_Stif_df=Damage_Stif(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Stif(Fat_pts),panneau(nel_fatigue).iy_stif(Fat_pts))

		elseif (kk.EQ.5) then	!damage for load case "a", "ballast loading condition"				

		Damage_Stif_ab=Damage_Stif(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Stif(Fat_pts),panneau(nel_fatigue).iy_stif(Fat_pts))
		
		elseif (kk.EQ.6) then	!damage for load case "b", "ballast loading condition"				
		
		Damage_Stif_bb=Damage_Stif(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Stif(Fat_pts),panneau(nel_fatigue).iy_stif(Fat_pts))
		
		elseif (kk.EQ.7) then	!damage for load case "c", "ballast loading condition"				

		Damage_Stif_cb=Damage_Stif(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Stif(Fat_pts),panneau(nel_fatigue).iy_stif(Fat_pts))
		
		endif				
	 
	enddo				


	Damage_Stif_full(nel_fatigue,panneau(nel_fatigue).ix_Stif(Fat_pts),panneau(nel_fatigue).iy_stif(Fat_pts))=	&
				1./6.*Damage_Stif_af + 1./6.*Damage_Stif_bf + 1./3.*Damage_Stif_cf + 1./3.*Damage_Stif_df

	Damage_Stif_ball(nel_fatigue,panneau(nel_fatigue).ix_Stif(Fat_pts),panneau(nel_fatigue).iy_stif(Fat_pts))=	&
				1./3.*Damage_Stif_ab + 1./3.*Damage_Stif_bb + 1./3.*Damage_Stif_cb 

	Damage_Stif_total(nel_fatigue,panneau(nel_fatigue).ix_Stif(Fat_pts),panneau(nel_fatigue).iy_stif(Fat_pts))=	&
		panneau(nel_fatigue).Kcor*(Part_full*Damage_Stif_full(nel_fatigue,panneau(nel_fatigue).ix_Stif(Fat_pts),panneau(nel_fatigue).iy_stif(Fat_pts))	&
				+(1-Part_full)*Damage_Stif_ball(nel_fatigue,panneau(nel_fatigue).ix_Stif(Fat_pts),panneau(nel_fatigue).iy_stif(Fat_pts)))/panneau(nel_fatigue).BetaIF_Stif(Fat_pts)


	AA=1
 	enddo				
	endif


	!Plates--------------------------------------------------------------

	if (panneau(nel_fatigue).I_Fat_Plate .NE. 0) THEN

	DO Fat_pts=1,panneau(nel_fatigue).nbr_Fat_Plate

	FATIGUE_INDEX=2

	! WRITE(*,*) 'PAnel_fatigue',nel_fatigue,'IX_Plate',panneau(nel_fatigue).ix_Plate(Fat_pts),'IY_Plate',panneau(nel_fatigue).iy_Plate(Fat_pts)

	! WRITE(*,*) 'CALL_TOTAL_DAMAGE'									

	do caschge_fatigue=1,nsol_fatigue  				

	   kk=NNSOL_BV_TYPE(caschge_fatigue)				

		if (kk.EQ.1) then	!damage for load case "a", "full loading condition"				

		Damage_Plate_af=Damage_Plate(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate(Fat_pts),panneau(nel_fatigue).iy_Plate(Fat_pts))

		elseif (kk.EQ.2) then	!damage for load case "b", "full loading condition"				

		Damage_Plate_bf=Damage_Plate(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate(Fat_pts),panneau(nel_fatigue).iy_Plate(Fat_pts))
		
		elseif (kk.EQ.3) then	!damage for load case "c", "full loading condition"				
		
		Damage_Plate_cf=Damage_Plate(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate(Fat_pts),panneau(nel_fatigue).iy_Plate(Fat_pts))
		
		elseif (kk.EQ.4) then	!damage for load case "d", "full loading condition"				

		Damage_Plate_df=Damage_Plate(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate(Fat_pts),panneau(nel_fatigue).iy_Plate(Fat_pts))

		elseif (kk.EQ.5) then	!damage for load case "a", "ballast loading condition"				

		Damage_Plate_ab=Damage_Plate(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate(Fat_pts),panneau(nel_fatigue).iy_Plate(Fat_pts))
		
		elseif (kk.EQ.6) then	!damage for load case "b", "ballast loading condition"				
		
		Damage_Plate_bb=Damage_Plate(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate(Fat_pts),panneau(nel_fatigue).iy_Plate(Fat_pts))
		
		elseif (kk.EQ.7) then	!damage for load case "c", "ballast loading condition"				

		Damage_Plate_bc=Damage_Plate(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate(Fat_pts),panneau(nel_fatigue).iy_Plate(Fat_pts))
		
		endif				
	 
	enddo				

	Damage_Plate_full(nel_fatigue,panneau(nel_fatigue).ix_Plate(Fat_pts),panneau(nel_fatigue).iy_Plate(Fat_pts))=	&
				1./6.*Damage_Plate_af + 1./6.*Damage_Plate_bf + 1./3.*Damage_Plate_cf + 1./3.*Damage_Plate_df

! Rem de Fred : Variable DAMAGE_PLATE_CB is used before its value has been defined

	Damage_Plate_ball(nel_fatigue,panneau(nel_fatigue).ix_Plate(Fat_pts),panneau(nel_fatigue).iy_Plate(Fat_pts))=	&
				1./6.*Damage_Plate_ab + 1./6.*Damage_Plate_bb + 1./3.*Damage_Plate_cb 

	Damage_Plate_total(nel_fatigue,panneau(nel_fatigue).ix_Plate(Fat_pts),panneau(nel_fatigue).iy_Plate(Fat_pts))=	&
		panneau(nel_fatigue).Kcor*(Part_full*Damage_Plate_full(nel_fatigue,panneau(nel_fatigue).ix_Plate(Fat_pts),panneau(nel_fatigue).iy_Plate(Fat_pts))		&
				+(1-Part_full)*Damage_Plate_ball(nel_fatigue,panneau(nel_fatigue).ix_Plate(Fat_pts),panneau(nel_fatigue).iy_Plate(Fat_pts)))/panneau(nel_fatigue).BetaIF_Plate(Fat_pts)

A=1
	enddo
	endif

	!Plates_Frames--------------------------------------------------------------

	if (panneau(nel_fatigue).I_Fat_Plate_Frame .NE. 0) THEN

	FATIGUE_INDEX=3

	! WRITE(*,*) 'PAnel_fatigue',nel_fatigue,'IX_Plate_Frame',panneau(nel_fatigue).ix_Plate_Frame(Fat_pts),'IY_Plate_Frame',panneau(nel_fatigue).iy_Plate_Frame(Fat_pts)

	! WRITE(*,*) 'CALL_TOTAL_DAMAGE'									

	DO Fat_pts=1,panneau(nel_fatigue).nbr_Fat_Plate_Frame

	do caschge_fatigue=1,nsol_fatigue  				

	   kk=NNSOL_BV_TYPE(caschge_fatigue)				

		if (kk.EQ.1) then	!damage for load case "a", "full loading condition"				

		Damage_Plate_Frame_af=Damage_Plate_Frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate_Frame(Fat_pts),panneau(nel_fatigue).iy_Plate_Frame(Fat_pts))

		elseif (kk.EQ.2) then	!damage for load case "b", "full loading condition"				

		Damage_Plate_Frame_bf=Damage_Plate_Frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate_Frame(Fat_pts),panneau(nel_fatigue).iy_Plate_Frame(Fat_pts))
		
		elseif (kk.EQ.3) then	!damage for load case "c", "full loading condition"				
		
		Damage_Plate_Frame_cf=Damage_Plate_Frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate_Frame(Fat_pts),panneau(nel_fatigue).iy_Plate_Frame(Fat_pts))
		
		elseif (kk.EQ.4) then	!damage for load case "d", "full loading condition"				

		Damage_Plate_Frame_df=Damage_Plate_Frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate_Frame(Fat_pts),panneau(nel_fatigue).iy_Plate_Frame(Fat_pts))

		elseif (kk.EQ.5) then	!damage for load case "a", "ballast loading condition"				

		Damage_Plate_Frame_ab=Damage_Plate_Frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate_Frame(Fat_pts),panneau(nel_fatigue).iy_Plate_Frame(Fat_pts))
		
		elseif (kk.EQ.6) then	!damage for load case "b", "ballast loading condition"				
		
		Damage_Plate_Frame_bb=Damage_Plate_Frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate_Frame(Fat_pts),panneau(nel_fatigue).iy_Plate_Frame(Fat_pts))
		
		elseif (kk.EQ.7) then	!damage for load case "c", "ballast loading condition"				

		Damage_Plate_Frame_bc=Damage_Plate_Frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Plate_Frame(Fat_pts),panneau(nel_fatigue).iy_Plate_Frame(Fat_pts))
		
		endif				
	 
	enddo				


	Damage_Plate_Frame_full(nel_fatigue,panneau(nel_fatigue).ix_Plate_Frame(Fat_pts),panneau(nel_fatigue).iy_Plate_Frame(Fat_pts))=	&
				1./6.*Damage_Plate_Frame_af + 1./6.*Damage_Plate_Frame_bf + 1./3.*Damage_Plate_Frame_cf + 1./3.*Damage_Plate_Frame_df

! Rem de Fred : Variable DAMAGE_PLATE_FRAME_CB is used before its value has been defined

	Damage_Plate_Frame_ball(nel_fatigue,panneau(nel_fatigue).ix_Plate_Frame(Fat_pts),panneau(nel_fatigue).iy_Plate_Frame(Fat_pts))=	&
				1./6.*Damage_Plate_Frame_ab + 1./6.*Damage_Plate_Frame_bb + 1./3.*Damage_Plate_Frame_cb 

	Damage_Plate_Frame_total(nel_fatigue,panneau(nel_fatigue).ix_Plate_Frame(Fat_pts),panneau(nel_fatigue).iy_Plate_Frame(Fat_pts))=	&
		panneau(nel_fatigue).Kcor*(Part_full*Damage_Plate_Frame_full(nel_fatigue,panneau(nel_fatigue).ix_Plate_Frame(Fat_pts),panneau(nel_fatigue).iy_Plate_Frame(Fat_pts))	&
				+(1-Part_full)*Damage_Plate_Frame_ball(nel_fatigue,panneau(nel_fatigue).ix_Plate_Frame(Fat_pts),panneau(nel_fatigue).iy_Plate_Frame(Fat_pts)))	&
				/panneau(nel_fatigue).BetaIF_Plate_Frame(Fat_pts)
	
	enddo
	endif

	!Plates_Frames--------------------------------------------------------------

	if (panneau(nel_fatigue).I_Fat_Frame .NE. 0) THEN



	! WRITE(*,*) 'PAnel_fatigue',nel_fatigue,'IX_Frame',panneau(nel_fatigue).ix_Frame(Fat_pts),'IY_Frame',panneau(nel_fatigue).iy_Frame(Fat_pts)

	! WRITE(*,*) 'CALL_TOTAL_DAMAGE'									

	DO Fat_pts=1,panneau(nel_fatigue).nbr_Fat_Frame

	do caschge_fatigue=1,nsol_fatigue  				

	   kk=NNSOL_BV_TYPE(caschge_fatigue)				

		if (kk.EQ.1) then	!damage for load case "a", "full loading condition"				

		Damage_Frame_af=Damage_Frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Frame(Fat_pts),panneau(nel_fatigue).iy_Frame(Fat_pts))

		elseif (kk.EQ.2) then	!damage for load case "b", "full loading condition"				

		Damage_Frame_bf=Damage_Frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Frame(Fat_pts),panneau(nel_fatigue).iy_Frame(Fat_pts))
		
		elseif (kk.EQ.3) then	!damage for load case "c", "full loading condition"				
		
		Damage_Frame_cf=Damage_Frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Frame(Fat_pts),panneau(nel_fatigue).iy_Frame(Fat_pts))
		
		elseif (kk.EQ.4) then	!damage for load case "d", "full loading condition"				

		Damage_Frame_df=Damage_Frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Frame(Fat_pts),panneau(nel_fatigue).iy_Frame(Fat_pts))

		elseif (kk.EQ.5) then	!damage for load case "a", "ballast loading condition"				

		Damage_Frame_ab=Damage_Frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Frame(Fat_pts),panneau(nel_fatigue).iy_Frame(Fat_pts))
		
		elseif (kk.EQ.6) then	!damage for load case "b", "ballast loading condition"				
		
		Damage_Frame_bb=Damage_Frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Frame(Fat_pts),panneau(nel_fatigue).iy_Frame(Fat_pts))
		
		elseif (kk.EQ.7) then	!damage for load case "c", "ballast loading condition"				

		Damage_Frame_bc=Damage_Frame(caschge_fatigue,nel_fatigue,panneau(nel_fatigue).ix_Frame(Fat_pts),panneau(nel_fatigue).iy_Frame(Fat_pts))
		
		endif				
	 
	enddo				

	Damage_Frame_full(nel_fatigue,panneau(nel_fatigue).ix_Frame(Fat_pts),panneau(nel_fatigue).iy_Frame(Fat_pts))=	&
				1./6.*Damage_Frame_af + 1./6.*Damage_Frame_bf + 1./3.*Damage_Frame_cf + 1./3.*Damage_Frame_df

!Rem de Fred : Variable DAMAGE_FRAME_CB is used before its value has been defined

	Damage_Frame_ball(nel_fatigue,panneau(nel_fatigue).ix_Frame(Fat_pts),panneau(nel_fatigue).iy_Frame(Fat_pts))=	&
				1./6.*Damage_Frame_ab + 1./6.*Damage_Frame_bb + 1./3.*Damage_Frame_cb 

	Damage_Frame_total(nel_fatigue,panneau(nel_fatigue).ix_Frame(Fat_pts),panneau(nel_fatigue).iy_Frame(Fat_pts))=	&
		panneau(nel_fatigue).Kcor*(Part_full*Damage_Frame_full(nel_fatigue,panneau(nel_fatigue).ix_Frame(Fat_pts),panneau(nel_fatigue).iy_Frame(Fat_pts))	&
				+(1-Part_full)*Damage_Frame_ball(nel_fatigue,panneau(nel_fatigue).ix_Frame(Fat_pts),panneau(nel_fatigue).iy_Frame(Fat_pts)))/panneau(nel_fatigue).BetaIF_Frame(Fat_pts)

	enddo
	endif


	!Girders--------------------------------------------------------------
!
!	If (MT(nel_fatigue).NE.0) Then
!
!	Do MMT=1,MT(nel_fatigue)
!
!	if (I_Fat_Girder(nel_fatigue,MMT) .NE. 0) THEN
!
!		FATIGUE_INDEX=5
!
!		! WRITE(*,*) 'PAnel_fatigue',nel_fatigue,'IX_Girder',ix_Girder(nel_fatigue,Fat_pts,mmt),'mmt',mmt
!
!		! WRITE(*,*) 'CALL_TOTAL_DAMAGE'									
!
!		DO Fat_pts=1,nbr_Fat_Girder(nel_fatigue,MMT)
!
!		do caschge_fatigue=1,nsol_fatigue  				
!
!	    kk=NNSOL_BV_TYPE(caschge_fatigue)				
!
!		if (kk.EQ.1) then	!damage for load case "a", "full loading condition"				
!
!		Damage_Girder_af=Damage_Girder(caschge_fatigue,nel_fatigue,ix_Girder(nel_fatigue,Fat_pts,MMT),MMT)
!
!		elseif (kk.EQ.2) then	!damage for load case "b", "full loading condition"				
!
!		Damage_Girder_bf=Damage_Girder(caschge_fatigue,nel_fatigue,ix_Girder(nel_fatigue,Fat_pts,MMT),MMT)
!		
!		elseif (kk.EQ.3) then	!damage for load case "c", "full loading condition"				
!		
!		Damage_Girder_cf=Damage_Girder(caschge_fatigue,nel_fatigue,ix_Girder(nel_fatigue,Fat_pts,MMT),MMT)
!		
!		elseif (kk.EQ.4) then	!damage for load case "d", "full loading condition"				
!
!		Damage_Girder_df=Damage_Girder(caschge_fatigue,nel_fatigue,ix_Girder(nel_fatigue,Fat_pts,MMT),MMT)
!
!		elseif (kk.EQ.5) then	!damage for load case "a", "ballast loading condition"				
!
!		Damage_Girder_ab=Damage_Girder(caschge_fatigue,nel_fatigue,ix_Girder(nel_fatigue,Fat_pts,MMT),MMT)
!		
!		elseif (kk.EQ.6) then	!damage for load case "b", "ballast loading condition"				
!		
!		Damage_Girder_bb=Damage_Girder(caschge_fatigue,nel_fatigue,ix_Girder(nel_fatigue,Fat_pts,MMT),MMT)
!		
!		elseif (kk.EQ.7) then	!damage for load case "c", "ballast loading condition"				
!
!		Damage_Girder_bc=Damage_Girder(caschge_fatigue,nel_fatigue,ix_Girder(nel_fatigue,Fat_pts,MMT),MMT)
!		
!		endif				
!	 
!	enddo	
!		

!		Damage_Girder_full(nel_fatigue,ix_Girder(nel_fatigue,Fat_pts,MMT),MMT)=	&
!				1./6.*Damage_Girder_af + 1./6.*Damage_Girder_bf + 1./3.*Damage_Girder_cf + 1./3.*Damage_Girder_df
!
!		Damage_Girder_ball(nel_fatigue,ix_Girder(nel_fatigue,Fat_pts,MMT),MMT)=	&
!				1./6.*Damage_Girder_ab + 1./6.*Damage_Girder_bb + 1./3.*Damage_Girder_cb 
!
!		Damage_Girder_total(nel_fatigue,ix_Girder(nel_fatigue,Fat_pts,MMT),MMT)=	&
!					panneau(nel_fatigue).Kcor*(Part_full*Damage_Girder_full(nel_fatigue,ix_Girder(nel_fatigue,Fat_pts,MMT),MMT)	&
!				+(1-Part_full)*Damage_Girder_ball(nel_fatigue,ix_Girder(nel_fatigue,Fat_pts,MMT),MMT))/BetaIF_Girder(nel_fatigue,Fat_pts,MMT)
!
!	enddo	
!	endif
!	enddo
!			
!	endif
!
!
	
elseIf(itype(nel_fatigue).EQ.5) then		!Pillar

	!Pillars--------------------------------------------------------------

!	if (I_Fat_Pillar(nel_fatigue) .NE. 0) THEN
!
!	DO Fat_pts=1,nbr_Fat_Pillar(nel_fatigue)
!
!	FATIGUE_INDEX=5
!
!	! WRITE(*,*) 'PAnel_fatigue',nel_fatigue,'IX_Pillar',ix_Pillar(nel_fatigue,Fat_pts),'IY_Pillar',iy_Pillar(nel_fatigue,Fat_pts)
!
!	! WRITE(*,*) 'CALL_TOTAL_DAMAGE'									
!
!
!	do caschge_fatigue=1,nsol_fatigue  				
!
!	   kk=NNSOL_BV_TYPE(caschge_fatigue)				
!
!		if (kk.EQ.1) then	!damage for load case "a", "full loading condition"				
!
!		Damage_Pillar_af=Damage_Pillar(caschge_fatigue,nel_fatigue,ix_Pillar(nel_fatigue,Fat_pts),iy_Pillar(nel_fatigue,Fat_pts))
!
!		elseif (kk.EQ.2) then	!damage for load case "b", "full loading condition"				
!
!		Damage_Pillar_bf=Damage_Pillar(caschge_fatigue,nel_fatigue,ix_Pillar(nel_fatigue,Fat_pts),iy_Pillar(nel_fatigue,Fat_pts))
!		
!		elseif (kk.EQ.3) then	!damage for load case "c", "full loading condition"				
!		
!		Damage_Pillar_cf=Damage_Pillar(caschge_fatigue,nel_fatigue,ix_Pillar(nel_fatigue,Fat_pts),iy_Pillar(nel_fatigue,Fat_pts))
!		
!		elseif (kk.EQ.4) then	!damage for load case "d", "full loading condition"				
!
!		Damage_Pillar_df=Damage_Pillar(caschge_fatigue,nel_fatigue,ix_Pillar(nel_fatigue,Fat_pts),iy_Pillar(nel_fatigue,Fat_pts))
!
!		elseif (kk.EQ.5) then	!damage for load case "a", "ballast loading condition"				
!
!		Damage_Pillar_ab=Damage_Pillar(caschge_fatigue,nel_fatigue,ix_Pillar(nel_fatigue,Fat_pts),iy_Pillar(nel_fatigue,Fat_pts))
!		
!		elseif (kk.EQ.6) then	!damage for load case "b", "ballast loading condition"				
!		
!		Damage_Pillar_bb=Damage_Pillar(caschge_fatigue,nel_fatigue,ix_Pillar(nel_fatigue,Fat_pts),iy_Pillar(nel_fatigue,Fat_pts))
!		
!		elseif (kk.EQ.7) then	!damage for load case "c", "ballast loading condition"				
!
!		Damage_Pillar_bc=Damage_Pillar(caschge_fatigue,nel_fatigue,ix_Pillar(nel_fatigue,Fat_pts),iy_Pillar(nel_fatigue,Fat_pts))
!		
!		endif						
!
!	enddo				
!
!
!	Damage_Pillar_full(nel_fatigue,ix_Pillar(nel_fatigue,Fat_pts),iy_Pillar(nel_fatigue,Fat_pts))=	&
!				1./6.*Damage_Pillar_af + 1./6.*Damage_Pillar_bf + 1./3.*Damage_Pillar_cf + 1./3.*Damage_Pillar_df
!
!
!
!	Damage_Pillar_ball(nel_fatigue,ix_Pillar(nel_fatigue,Fat_pts),iy_Pillar(nel_fatigue,Fat_pts))=	&
!				1./6.*Damage_Pillar_ab + 1./6.*Damage_Pillar_bb + 1./3.*Damage_Pillar_cb 
!
!
!
!	Damage_Pillar_total(nel_fatigue,ix_Pillar(nel_fatigue,Fat_pts),iy_Pillar(nel_fatigue,Fat_pts))=	&
!		panneau(nel_fatigue).Kcor*(Part_full*Damage_Pillar_full(nel_fatigue,ix_Pillar(nel_fatigue,Fat_pts),iy_Pillar(nel_fatigue,Fat_pts))	&
!				+(1-Part_full)*Damage_Pillar_ball(nel_fatigue,ix_Pillar(nel_fatigue,Fat_pts),iy_Pillar(nel_fatigue,Fat_pts)))/BetaIF_Pillar(nel_fatigue,Fat_pts)
!
!	enddo
!
!
!	ENDIF	

	ENDIF	!itype		



ENDDO	!nel_fatigue



!	DO IPAN=1,NETO
!	NBRXI=NVAR(IPAN)
!	L1=0
!	IF(NBRXI.EQ.0) GOTO 1
!	DO K=1,NBRXI
!	KK=NXIT(K,IPAN)
!	LL=L1+K
!			
!

! DOMMAGE : FULL LOADING___________________________________________________________________________

! STIFFENED PLATE Sty=1 
!	dDfull_11(nel_fatigue,LL,IPAN)=1./6.*dD_11_1(nel_fatigue,LL,1,IPAN)+1./6.*dD_11_1(nel_fatigue,LL,2,IPAN)+
 !    *						1./3.*dD_11_1(nel_fatigue,LL,3,IPAN)+	1./3.*dD_11_1(nel_fatigue,LL,4,IPAN)
!
!	dDfull_12(nel_fatigue,LL,IPAN)=1./6.*dD_12_1(nel_fatigue,LL,1,IPAN)+1./6.*dD_12_1(nel_fatigue,LL,2,IPAN)+
 !    *							1./3.*dD_12_1(nel_fatigue,LL,3,IPAN)+1./3.*dD_12_1(nel_fatigue,LL,4,IPAN)
!
!	dDfull_13(nel_fatigue,LL,IPAN)=1./6.*dD_13_1(nel_fatigue,LL,1,IPAN)+1./6.*dD_13_1(nel_fatigue,LL,2,IPAN)+
 !    *							1./3.*dD_13_1(nel_fatigue,LL,3,IPAN)+1./3.*dD_13_1(nel_fatigue,LL,4,IPAN)
!
!	dDfull_14(nel_fatigue,LL,IPAN)=1./6.*dD_14_1(nel_fatigue,LL,1,IPAN)+1./6.*dD_14_1(nel_fatigue,LL,2,IPAN)+
 !    *							1./3.*dD_14_1(nel_fatigue,LL,3,IPAN)+1./3.*dD_14_1(nel_fatigue,LL,4,IPAN)
!
!! GIRDER Sty=2 AND Sty2=1
!	DO  MMT=1,MT(nel_fatigue)
!
!	dDfull_21_1(nel_fatigue,MMT,LL,IPAN)=1./6.*dD_21_1(nel_fatigue,MMT,LL,1,IPAN)+1./6.*dD_21_1(nel_fatigue,MMT,LL,2,IPAN)+
 !    *							1./3.*dD_21_1(nel_fatigue,MMT,LL,3,IPAN)+1./3.*dD_21_1(nel_fatigue,MMT,LL,4,IPAN)
!
!	dDfull_22_1(nel_fatigue,MMT,LL,IPAN)=1./6.*dD_22_1(nel_fatigue,MMT,LL,1,IPAN)+1./6.*dD_22_1(nel_fatigue,MMT,LL,2,IPAN)+
 !    *							1./3.*dD_22_1(nel_fatigue,MMT,LL,3,IPAN)+1./3.*dD_22_1(nel_fatigue,MMT,LL,4,IPAN)
!
!	dDfull_23_1(nel_fatigue,MMT,LL,IPAN)=1./6.*dD_23_1(nel_fatigue,MMT,LL,1,IPAN)+1./6.*dD_23_1(nel_fatigue,MMT,LL,2,IPAN)+
 !    *							1./3.*dD_23_1(nel_fatigue,MMT,LL,3,IPAN)+1./3.*dD_23_1(nel_fatigue,MMT,LL,4,IPAN)
!
!	dDfull_24_1(nel_fatigue,MMT,LL,IPAN)=1./6.*dD_24_1(nel_fatigue,MMT,LL,1,IPAN)+1./6.*dD_24_1(nel_fatigue,MMT,LL,2,IPAN)+
 !    *							1./3.*dD_24_1(nel_fatigue,MMT,LL,3,IPAN)+1./3.*dD_24_1(nel_fatigue,MMT,LL,4,IPAN)
!	ENDDO
!
!! WEB FRAME  Sty=2 Sty2=2
!	dDfull_21_2(nel_fatigue,LL,IPAN)=1./6.*dD_21_2(nel_fatigue,LL,1,IPAN)+1./6.*dD_21_2(nel_fatigue,LL,2,IPAN)+
 !    *							1./3.*dD_21_2(nel_fatigue,LL,3,IPAN)+1./3.*dD_21_2(nel_fatigue,LL,4,IPAN)
!
!	dDfull_22_2(nel_fatigue,LL,IPAN)=1./6.*dD_22_2(nel_fatigue,LL,1,IPAN)+1./6.*dD_22_2(nel_fatigue,LL,2,IPAN)+
 !    *							1./3.*dD_22_2(nel_fatigue,LL,3,IPAN)+1./3.*dD_22_2(nel_fatigue,LL,4,IPAN)
!
!	dDfull_23_2(nel_fatigue,LL,IPAN)=1./6.*dD_23_2(nel_fatigue,LL,1,IPAN)+1./6.*dD_23_2(nel_fatigue,LL,2,IPAN)+
 !    *							1./3.*dD_23_2(nel_fatigue,LL,3,IPAN)+1./3.*dD_23_2(nel_fatigue,LL,4,IPAN)
!
!	dDfull_24_2(nel_fatigue,LL,IPAN)=1./6.*dD_24_2(nel_fatigue,LL,1,IPAN)+1./6.*dD_24_2(nel_fatigue,LL,2,IPAN)+
 !    *							1./3.*dD_24_2(nel_fatigue,LL,3,IPAN)+1./3.*dD_24_2(nel_fatigue,LL,4,IPAN)
!
!
!
!! DOMMAGE : BALLAST LOADING______________________________________________________________________
!
!! STIFFENED PLATE Sty=1 
!	dDball_11(nel_fatigue,LL,IPAN)=1./3.*dD_11_1(nel_fatigue,LL,5,IPAN)+1./3.*dD_11_1(nel_fatigue,LL,6,IPAN)+
 !    *						 1./3.*dD_11_1(nel_fatigue,LL,7,IPAN)
!	dDball_12(nel_fatigue,LL,IPAN)=1./3.*dD_12_1(nel_fatigue,LL,5,IPAN)+1./3.*dD_12_1(nel_fatigue,LL,6,IPAN)+
 !    *						 1./3.*dD_12_1(nel_fatigue,LL,7,IPAN)
!	dDball_13(nel_fatigue,LL,IPAN)=1./3.*dD_13_1(nel_fatigue,LL,5,IPAN)+1./3.*dD_13_1(nel_fatigue,LL,6,IPAN)+ 
 !    *						1./3.*dD_13_1(nel_fatigue,LL,7,IPAN)
!	dDball_14(nel_fatigue,LL,IPAN)=1./3.*dD_14_1(nel_fatigue,LL,5,IPAN)+1./3.*dD_14_1(nel_fatigue,LL,6,IPAN)+
 !    *						 1./3.*dD_14_1(nel_fatigue,LL,7,IPAN)
!
!! GIRDER Sty=2 AND Sty2=1
!	DO  MMT=1,MT(nel_fatigue)
!	dDball_21_1(nel_fatigue,MMT,LL,IPAN)=1./3.*dD_21_1(nel_fatigue,MMT,LL,5,IPAN)+1./3.*dD_21_1(nel_fatigue,MMT,LL,6,IPAN)+
 !    *						 1./3.*dD_21_1(nel_fatigue,MMT,LL,7,IPAN)
!
!	dDball_22_1(nel_fatigue,MMT,LL,IPAN)=1./3.*dD_22_1(nel_fatigue,MMT,LL,5,IPAN)+1./3.*dD_22_1(nel_fatigue,MMT,LL,6,IPAN)+
 !    *						 1./3.*dD_22_1(nel_fatigue,MMT,LL,7,IPAN)
!
!	dDball_23_1(nel_fatigue,MMT,LL,IPAN)=1./3.*dD_23_1(nel_fatigue,MMT,LL,5,IPAN)+1./3.*dD_23_1(nel_fatigue,MMT,LL,6,IPAN)+
 !    *						 1./3.*dD_23_1(nel_fatigue,MMT,LL,7,IPAN)
!
!	dDball_24_1(nel_fatigue,MMT,LL,IPAN)=1./3.*dD_24_1(nel_fatigue,MMT,LL,5,IPAN)+1./3.*dD_24_1(nel_fatigue,MMT,LL,6,IPAN)+ 
 !    *						1./3.*dD_24_1(nel_fatigue,MMT,LL,7,IPAN)
!	ENDDO
!
!! WEB FRAME  Sty=2 Sty2=2
!	dDball_21_2(nel_fatigue,LL,IPAN)=1./3.*dD_21_2(nel_fatigue,LL,5,IPAN)+1./3.*dD_21_2(nel_fatigue,LL,6,IPAN)+
 !    *						 1./3.*dD_21_2(nel_fatigue,LL,7,IPAN)
!
!	dDball_22_2(nel_fatigue,LL,IPAN)=1./3.*dD_22_2(nel_fatigue,LL,5,IPAN)+1./3.*dD_22_2(nel_fatigue,LL,6,IPAN)+ 
 !    *						1./3.*dD_22_2(nel_fatigue,LL,7,IPAN)
!
!	dDball_23_2(nel_fatigue,LL,IPAN)=1./3.*dD_23_2(nel_fatigue,LL,5,IPAN)+1./3.*dD_23_2(nel_fatigue,LL,6,IPAN)+ 
 !    *						1./3.*dD_23_2(nel_fatigue,LL,7,IPAN)
!
!	dDball_24_2(nel_fatigue,LL,IPAN)=1./3.*dD_24_2(nel_fatigue,LL,5,IPAN)+1./3.*dD_24_2(nel_fatigue,LL,6,IPAN)+ 
 !    *						1./3.*dD_24_2(nel_fatigue,LL,7,IPAN)



! TOTAL_DAMAGE___________________________________________________________________________

! STIFFENED PLATE Sty=1 
!	dDtot_11(nel_fatigue,LL,IPAN)=panneau(nel_fatigue).Kcor*(Part_full*dDfull_11(nel_fatigue,LL,IPAN)+
 !    *						(1-Part_full)*dDball_11(nel_fatigue,LL,IPAN))/BetaIF(nel_fatigue)
!
!	dDtot_12(nel_fatigue,LL,IPAN)=panneau(nel_fatigue).Kcor*(Part_full*dDfull_12(nel_fatigue,LL,IPAN)+
 !    *						(1-Part_full)*dDball_12(nel_fatigue,LL,IPAN))/BetaIF(nel_fatigue)
!
!	dDtot_13(nel_fatigue,LL,IPAN)=panneau(nel_fatigue).Kcor*(Part_full*dDfull_13(nel_fatigue,LL,IPAN)+
 !    *						(1-Part_full)*dDball_13(nel_fatigue,LL,IPAN))/BetaIF(nel_fatigue)
!
!	dDtot_14(nel_fatigue,LL,IPAN)=panneau(nel_fatigue).Kcor*(Part_full*dDfull_14(nel_fatigue,LL,IPAN)+
 !    *						(1-Part_full)*dDball_14(nel_fatigue,LL,IPAN))/BetaIF(nel_fatigue)

! GIRDER Sty=2 AND Sty2=1
!	DO  MMT=1,MT(nel_fatigue)
!	dDtot_21_1(nel_fatigue,MMT,LL,IPAN)=panneau(nel_fatigue).Kcor*(Part_full*dDfull_21_1(nel_fatigue,MMT,LL,IPAN)+
 !    *						(1-Part_full)*dDball_21_1(nel_fatigue,MMT,LL,IPAN))/BetaIF(nel_fatigue)
!
!	dDtot_22_1(nel_fatigue,MMT,LL,IPAN)=panneau(nel_fatigue).Kcor*(Part_full*dDfull_22_1(nel_fatigue,MMT,LL,IPAN)+
 !    *						(1-Part_full)*dDball_22_1(nel_fatigue,MMT,LL,IPAN))/BetaIF(nel_fatigue)
!
!	dDtot_23_1(nel_fatigue,MMT,LL,IPAN)=panneau(nel_fatigue).Kcor*(Part_full*dDfull_23_1(nel_fatigue,MMT,LL,IPAN)+
 !    *						(1-Part_full)*dDball_23_1(nel_fatigue,MMT,LL,IPAN))/BetaIF(nel_fatigue)
!
!	dDtot_24_1(nel_fatigue,MMT,LL,IPAN)=panneau(nel_fatigue).Kcor*(Part_full*dDfull_24_1(nel_fatigue,MMT,LL,IPAN)+
 !    *						(1-Part_full)*dDball_24_1(nel_fatigue,MMT,LL,IPAN))/BetaIF(nel_fatigue)
!	ENDDO
!
!! WEB FRAME  Sty=2 Sty2=2
!!	dDtot_21_2(nel_fatigue,LL,IPAN)=panneau(nel_fatigue).Kcor*(Part_full*dDfull_21_2(nel_fatigue,LL,IPAN)+
  !   *						(1-Part_full)*dDball_21_2(nel_fatigue,LL,IPAN))/BetaIF(nel_fatigue)
!
!	dDtot_22_2(nel_fatigue,LL,IPAN)=panneau(nel_fatigue).Kcor*(Part_full*dDfull_22_2(nel_fatigue,LL,IPAN)+
 !    *						(1-Part_full)*dDball_22_2(nel_fatigue,LL,IPAN))/BetaIF(nel_fatigue)
!
!	dDtot_23_2(nel_fatigue,LL,IPAN)=panneau(nel_fatigue).Kcor*(Part_full*dDfull_23_2(nel_fatigue,LL,IPAN)+
 !    *						(1-Part_full)*dDball_23_2(nel_fatigue,LL,IPAN))/BetaIF(nel_fatigue)
!
!	dDtot_24_2(nel_fatigue,LL,IPAN)=panneau(nel_fatigue).Kcor*(Part_full*dDfull_24_2(nel_fatigue,LL,IPAN)+
 !    *						(1-Part_full)*dDball_24_2(nel_fatigue,LL,IPAN))/BetaIF(nel_fatigue)
!
!
!
!		ENDDO
 ! 1		CONTINUE
  !      
!	  	L1=L1+NBRXI
!		ENDDO
!


! enddo


! ENDIF
      
	END

