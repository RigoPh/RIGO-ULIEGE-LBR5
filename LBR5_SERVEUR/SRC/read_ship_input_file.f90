subroutine read_ship_input_file()
!subroutine get_main_parameter()  ! incluse dans read_ship_input_file()
!subroutine get_section_header()




use param_section
!use param_cout
!use param_multiobj

!use param_opti_local


use PARAM_SECTION_VECTOR  !vient de get_main_parameter()

implicit double precision (a-h,o-z)

! ====================================================================================================
! ouverture du fichier de donnees (unite iu_10(iboat)) 
! ====================================================================================================
section_file=section_files(iboat)
open(iu_10(iboat),file=section_file,status='old',err=600)

! ====================================================================================================
! lecture des donnees (unite iu_10(iboat)) 
! ====================================================================================================

!iana = 1:lbr-4										
!iana = 2:beam theory 

read (iu_10(iboat),'(a80)')header1
read (iu_10(iboat),*)      iana			 
read (iu_10(iboat),'(a80)')header2
read (iu_10(iboat),*)   impr,impr2,indaig,indrai,dessin,jlph2,ibusc,neto
neto_vector(iboat) = neto   !vient de get_main_parameter()
ngmax_vector(iboat) = ngmax !vient de get_main_parameter()
read(iu_10(iboat),*) imulti,rho,w1,w2,w3,fk(1),fk(2),fk(3)	                                !optimisation multicritères
read(iu_10(iboat),*) ipareto, nsteppar, iparcout, iparpoids, iparinertia
read(iu_10(iboat),*) lcc2,lcc3,lcc4,lcc5,lightweight_init,deadweight_init,lccscenario,poidsLBR5_init  ! life cycle cost
!lightweight_init = lightweight_init*1000*9.81     !pour avoir en Newtons
!deadweight_init =   deadweight_init*1000*9.81     !pour avoir en Newtons
read(iu_10(iboat),*) !passe IOPTI car iopti est lu dans init.dat
if(iopti.ne.1)iteram=99
read(iu_10(iboat),*) icout             ! 0->poids; 1->cout
read(iu_10(iboat),*) iredress          ! 1->avec calcul du redressage  
read(iu_10(iboat),*) rend_global !,eqp_global
! rendement du chantier, equivalent poids de la mdo (=k)   
read(iu_10(iboat),*) refthick_input	   ! reference thickness input
read(iu_10(iboat),*) nbrMat            ! nombre de matériaux
nbrMat_vector(iboat) = nbrMat          !vient de get_main_parameter()
if (nbrMat.eq.0) goto 700
call allocate_param_materiau()         ! necessite nbrMat
!call allocate_param_section()         !??TODO A changer de place, mais on doit initialiser e, eta, etc.
do ind=1,nbrMat
   read(iu_10(iboat),*)	               !texte MATERIAU IND
   read(iu_10(iboat),*) e(ind),eta(ind),sigy(ind),sigyadm(ind),coefk(ind),spec(ind)
  !read(iu_10(iboat),*) rend(ind),eqp(ind)					! rendement, equivalent poids de la mdo (=k)
  !read(iu_10(iboat),*) dref(ind),drefx(ind),drefy(ind)		! epaisseurs de référence
  !read(iu_10(iboat),'(1(f8.4,2x))')  dref(ind)
  !read(iu_10(iboat),'(1(f8.4,2x))') drefx(ind)
  !read(iu_10(iboat),'(1(f8.4,2x))') drefy(ind)
   read(iu_10(iboat),'(3(f6.4,2x))') dref(ind),drefx(ind),drefy(ind)
   read(iu_10(iboat),*) dref_b(ind),dref_c(ind),dref_l(ind)	! epaisseurs de référence pour les profiles types
  !write(6,'(1(f6.4,2x))') dref_b(ind)
   read(iu_10(iboat),*) c1(ind),c2(ind),c3(ind),dc1(ind)      ! cout des matériaux
   read(iu_10(iboat),*) c_pb(ind),c_tc(ind),c_tl(ind)         ! cout des matériaux pour les profiles types
   read(iu_10(iboat),*) dw2(ind),dw3(ind)		                ! extra weight
   read(iu_10(iboat),*) dw_b(ind),dw_c(ind),dw_l(ind)		    ! extra weight
   read(iu_10(iboat),*) labour_cost(ind)
   read(iu_10(iboat),*) p10(ind),dp10(ind)			        ! mdo bordé
   read(iu_10(iboat),*) p4(ind),p5(ind),dp4(ind),dp5(ind)     ! mdo assembl. membr-bordé
   read(iu_10(iboat),*) p9x(ind),p9y(ind),dp9x(ind),dp9y(ind) ! mdo constr. membr.
   read(iu_10(iboat),*) p6(ind),p7(ind),ber(ind),bet(ind)     ! mdo intersect et goussets
   read(iu_10(iboat),*) c8(ind),dc8(ind),ialr(ind),ialt(ind)  ! coût soudure (energ + consom.)
enddo
read(iu_10(iboat),*) width,length     
read(iu_10(iboat),*) (dis(i),i=1,5)   
read(iu_10(iboat),*) (fam(i),i=1,6)   
fam(5)=0.
fam(6)=0.
read(iu_10(iboat),*) ipoids

! ====================================================================================================
! lecture des sollicitations
!  nsolm = nbre de cas de sollicitations defini dans les données
!  nsol  = nbre de cas de sollicitations qui seront étudiés
!  nnsol = liste des numeros des nsol cas étudies.
! ====================================================================================================
allocate (nnsol(nsolmax))		!new4  dans init_param_multi.f90(79):nsolmax  =20 
read(iu_10(iboat),*) nsolm, nsol,(nnsol(i),i=1,nsol)
allocate(casename(nsolm))
allocate(is_loadcase_fatigue(nsolm))
allocate(tirant(nsolm))
allocate(is_selected_loadcase_fatigue(nsol))

nsol_fatigue=0
!NNSOL_BV_TYPE(:)=0		!fat_new
nsolm_fatigue=0
tirant(:)=0.d00
is_selected_loadcase_fatigue(:)=0.d00

do i=1,nsolm
   read(iu_10(iboat),'(a80)') casename(i)
  !read(iu_10(iboat),*) is_loadcase_selected(i)
   read(iu_10(iboat),*) is_loadcase_fatigue(i)
enddo
index_fatigue=0

!  nsolm_fatigue = total number of load cases specified for fatigue analysis, 		!fat_new   
!  nsol_fatigue = number of load cases selected for fatigue analysis		!fat_new

do i=1,nsolm
   read(iu_10(iboat),*) tirant(i)
   if(is_loadcase_fatigue(i).eq.1) then
	  index_fatigue=index_fatigue+1
      read(iu_10(iboat),*) tirant(index_fatigue)
      nsolm_fatigue=nsolm_fatigue+1

		do k=1,nsol						!FAT_NEW
			icc=0					!FAT_NEW
			if (NNSOL(k).eq.i) icc=1   	  					!FAT_NEW
			if(icc.ne.0) then           					!FAT_NEW
			nsol_fatigue=nsol_fatigue+1					!FAT_NEW
			endif					!FAT_NEW
		enddo					!FAT_NEW


!      if (is_loadcase_selected(i).eq.1) then
!	      nsol_fatigue=nsol_fatigue+1
!	  endif
   endif
enddo

if (nsol_fatigue.gt.0) allocate(NNSOL_BV_TYPE(nsol_fatigue))		!fat_new
NNSOL_BV_TYPE(:)=0		!fat_new

j=0														!new4
do i=1,nsol
   if(is_loadcase_fatigue(nnsol(i)).eq.1) then				!new4
      is_selected_loadcase_fatigue(i)=1				!new4
	  j=j+1				!new4
!	  NNSOL_BV_TYPE(j)=i						!fat_new
	endif				!new4
enddo											!new4
!do i=1,nsolm!
!	read(iu_10(iboat),*) !TODO à modifier
!enddo

!do i=1,nsolm!
!	read(iu_10(iboat),*) !TODO à modifier
!enddo

!if(nsolm_fatigue.gt.0) then
   read(iu_10(iboat),*) depth	                   !depth of the ship
   read(iu_10(iboat),*) part_full	               !part of the ship’s life in full load condition, given in tab 9 (bV rules, ptb,ch7,sec 4)	
   read(iu_10(iboat),*) method_fatigue		       !damage calculation method. 1: dnV method ,2: bV method	
   read(iu_10(iboat),*) tfl		                   !increased design life (between 25 and 40).if no incrfease of design life, tfl=20
!   read(iu_10(iboat),*) sens_finite_diff           !si sens_finite_diff :calcul des sensibilités par différences fines pour comparaison
   read(iu_10(iboat),*) IS_FATIGUE					! IS_FATIGUE : PARAMETER: if 1 :fatigue assessment and not if 0 


!endif

! NNSOL_BV_TYPE(I)		Specification of the nature of each Fat_nsol fatigue load cases
!		if NNSOL_BV_TYPE(I) = 1 =>!load case "a", "full loading condition"	
!		if NNSOL_BV_TYPE(I) = 2 =>!load case "b", "full loading condition"	
!		if NNSOL_BV_TYPE(I) = 3 =>!load case "c", "full loading condition"	
!		if NNSOL_BV_TYPE(I) = 4 =>!load case "d", "full loading condition"			
!		if NNSOL_BV_TYPE(I) = 5 =>!load case "a", "ballast loading condition"				
!		if NNSOL_BV_TYPE(I) = 6 =>!load case "b", "ballast loading condition"				
!		if NNSOL_BV_TYPE(I) = 7 =>!load case "c", "ballast loading condition"				


   if (nsol_fatigue.gt.0) then           !Maud ajout de la condition et du else
	read(iu_10(iboat),*) (NNSOL_BV_TYPE(I),I=1,nsol_fatigue)		!fat_new 				!new4          
   else 
	read(iu_10(iboat),*)
   endif

!NNSOL_BV_TYPE(1)=1				!new4
!NNSOL_BV_TYPE(2)=2				!new4
!NNSOL_BV_TYPE(3)=5				!new4

!******************** initialisations ******************
!=====================================
! nombre de termes de fourier
!=====================================
! jlph2 = nbre de termes impair, avec son signe > ou <
! jlph  = nbre de termes total (pair et impair)
jlph=2*iabs(jlph2)-1
iprint=0

! calcul du parametre ne (nbre d'inconnues hyperstatiques de la structure)
ne=neto*8
! verification de la taille du vecteur al càd dzsn(ne,9*neto,nsol)<7200000
imax=72*nsolmax*nmax

!******************** messages d'erreur ********************

!=====================================
! flags optimisation multicritère
!=====================================
if (imulti.eq.1) then
	if (w1.gt.0.and.fk(1).eq.0) then
		if (langue.eq.1) then
			write(*,*) 'Cout selectionne pour le multi-obectif sans avoir defini le cout optimum'
			write(*,*) 'en cas d''optimisation individuelle'
			write(*,*) 'Veuillez corriger vos donnees - appuyer sur ENTER pour poursuivre'
		elseif (langue.eq.2) then
			write(*,*) 'Cost selected without defining the optimum cost obtained by a single optimization'
			write(*,*) 'Please correct your data - press ENTER to resume'
		endif
		read(*,*)
		stop
	endif
	if (w2.gt.0.and.fk(2).eq.0) then
		if (langue.eq.1) then
			write(*,*) 'Poids selectionne pour le multi-obectif sans avoir defini le poids optimum'
			write(*,*) 'en cas d''optimisation individuelle'
			write(*,*) 'Veuillez corriger vos donnees - appuyer sur ENTER pour poursuivre'
		elseif (langue.eq.2) then
			write(*,*) 'Weight selected without defining the optimum weight obtained by a single optimization'
			write(*,*) 'Please correct your data - press ENTER to resume'
		endif
		read(*,*)
		stop
	endif
	if (w3.gt.0.and.fk(3).eq.0) then
		if (langue.eq.1) then
			write(*,*) 'Inertie selectionne pour le multi-obectif sans avoir defini l''inertie optimum'
			write(*,*) 'en cas d''optimisation individuelle'
			write(*,*) 'Veuillez corriger vos donnees - appuyer sur ENTER pour poursuivre'
		elseif (langue.eq.2) then
			write(*,*) 'Inertia selected without defining the optimum inertia obtained by a single optimization'
			write(*,*) 'Please correct your data - press ENTER to resume'
		endif
		read(*,*)
		stop
	endif
endif

!if (eqp_global.eq.0) then
!	if (langue.eq.1) then
!		write(*,'(a)') 'Attention ! Parametre eqp_global est nul !!!'
!	elseif (langue.eq.2) then
!		write(*,'(a)') 'Attention! Parameter eqp_global is equal to 0!!!'
!	endif
!endif

if (width.eq.0.) then							!NEW10
	write(*,*) 'input value error: Longueur du tronçon (WIDTH) = 0'			!NEW10
	write(*,*) 'stop'										!NEW10
	read(*,*)
	stop												!NEW10
endif													!NEW10

!	if (length.EQ.0.) then							!NEW10
!	write(*,*) 'input value error: Longueur du tronçon (WIDTH) = 0'			!NEW10
!		pause 'stop'										!NEW10
!		stop												!NEW10
!	endif													!NEW10

if (nsol_fatigue.NE.0.) then	
	if (length.EQ.0.) then							!NEW10
!		write(*,*) 'input value error: Longueur du tronçon (length) = 0'			!NEW10
		write(*,*) 'FOR FATIGUE, "RULE LENGTH" MUST BE ENTERED '			!NEW10
		pause 'stop'										!NEW10
		stop												!NEW10
	endif													!NEW10
endif

if((nsol.lt.1).or.(nsol.gt.nsolmax)) then
 ! erreur sur le nbe de sollicitations   
  if(langue.eq.1)then                                                                 
      write(6,*)' nombre de sollicitations incorrect ... sortie.'                        
  endif                                                                               
  if(langue.eq.2)then                                                                 
      write(6,*)' wrong load case number ... aborting.'                                  
  endif                                                                               
  stop 3                                                                              
endif 

if((nsol.lt.1).or.(nsol.gt.nsolmax)) then
 ! erreur sur le nbe de sollicitations   
  if(langue.eq.1)then                                                                 
      write(6,*)' nombre de sollicitations incorrect ... sortie.'                        
  endif                                                                               
  if(langue.eq.2)then                                                                 
      write(6,*)' wrong load case number ... aborting.'                                  
  endif                                                                               
  stop 3                                                                              
endif 

if(imax.gt.7200000)then
   write(*,*) ' le nbre de panneau est trop élevé pour nsol=',nsol
   write(iu_11(iboat),*)' le nbre de panneaux est trop élevé pour nsol=',nsol
   write(iu_14(iboat),*)'le nbre de sollicitations est incorrect (max.= 10)'
   write(*,*)' le nbre de panneaux est trop élevé pour nsol=',nsol
   write(*,*)'le nbre de sollicitations est incorrect (max.= 10)'
   write(*,*) 'stop'
   read(*,*)
   stop
endif



!******************** impressions ********************

!=====================================
! impressions diverses
!=====================================
write(iu_12(iboat),'(a80)')header1	
write(iu_12(iboat),*)				
write(iu_12(iboat),'(a)') '1 **********************************************************************'
write(iu_12(iboat),'(a,a64,a)') '  *    ',header2,'*'
write(iu_12(iboat),'(a)') '  *    ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++    *'
write(iu_12(iboat),'(a)') '  *                                                                    *'
write(iu_12(iboat),'(a)') '  **********************************************************************'

write(iu_11(iboat),4)header1	
write(iu_11(iboat),*)				
write(iu_11(iboat),'(a)') '1 **********************************************************************'
write(iu_11(iboat),'(a,a64,a)') '  *    ',header2,'*'
write(iu_11(iboat),'(a)') '  *    ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++    *'
write(iu_11(iboat),'(a)') '  *                                                                    *'
write(iu_11(iboat),'(a)') '  **********************************************************************'
write(iu_11(iboat),*) 
write(iu_11(iboat),*) ' noms des fichiers /file names:'
write(iu_11(iboat),*) ' input - donnees    :', section_file
write(iu_11(iboat),*) ' output-resultat #1 :', iunit1
write(iu_11(iboat),*) '                 #3 :', iunit3
write(iu_11(iboat),*)


!=====================================
! structure à un seul panneau
!=====================================
if(neto.eq.1)then
   if(langue.eq.1)then
      write(*,100)' structure a un seul panneau ...' 
   else
      write(*,100)' single panel structure ...'
   endif
endif 

!=====================================
! nombre de panneaux
!=====================================
if(langue==1)then
   write(iu_11(iboat),10) neto  
   write(* ,10) neto	
else
   write(iu_11(iboat), 6) neto  
   write(* , 6) neto  
endif  								
if(langue.eq.2)write(iu_12(iboat),6) neto

!=====================================
! flags optimisation classique
!=====================================
!if(iopti.ne.1)iteram=99
if(iopti.ge.1)then
!  output file for the optimization process opt-nom.
   open(iu_31(iboat),file=iunit2)
!  save list of struct constraint in ent to be re-used in contr        
!   open(iu_32(iboat),status='scratch',form='unformatted')
   write(iu_31(iboat),  3)header2
! variables de conception et restrictions geometriques
   write(iu_31(iboat),511)
   
!   if(iopti.ge.3 ) iprint=1
   if(langue==1) write(iu_11(iboat),'(t22,a)')'avec optimisation'		
   if(langue==2) write(iu_11(iboat),'(t22,a)')'with optimisation'	
   write (iu_11(iboat),400)							
   write (iu_11(iboat),*)'                    (iteration nø 1)'		
   write (iu_11(iboat),*)'                    (max=',iteram,')'		
else												
   if(langue==1) write(iu_11(iboat),'(t22,a)')'sans optimisation'	
   if(langue==2) write(iu_11(iboat),'(t22,a)')'without optimisation'	
   write (iu_11(iboat),400)									
endif 

if(langue==1) then
     write (iu_11(iboat), 11) jlph2,ibusc
     if(impr  .ne.0) write(iu_11(iboat),7)
     if(indaig.eq.0) write(iu_11(iboat),9)
     if(indrai.eq.0) write(iu_11(iboat),'(/a)')' pas de resultats dans les raidisseurs'
     if(impr2 .eq.1) write(iu_11(iboat),8)
else
     write (iu_11(iboat), 14) jlph2,ibusc
     if(impr  .ne.0) write(iu_11(iboat),'(/a)')' with intermediate results'
     if(indaig.eq.0) write(iu_11(iboat),'(/a)')' results in the frames are not printed'
     if(indrai.eq.0) write(iu_11(iboat),'(/a)')' results in the stiffeners are not printed'
     if(impr2 .eq.1) write(iu_11(iboat),*)' final results + equilibrium checks'
endif

write(iu_31(iboat),*)'neto',neto              				!eugen (17.10.2007)
write(iu_31(iboat),*)'iana',iana              				!eugen (17.10.2007)
write(iu_31(iboat),*)'icout',icout							!eugen (11.10.2007)
write(iu_31(iboat),*)'iredress',iredress			  		    !dad   (14.12.2007)
write(iu_31(iboat),*)'iteram',iteram							!eugen (11.10.2007)
write(iu_31(iboat),*)'imulti',imulti							!eugen (11.10.2007)
write(iu_31(iboat),'(a,f9.4,f9.4,f9.4/)')'weights',w1,w2,w3	!eugen (12.10.2007)   

if(langue==1) then 
   write(iu_11(iboat),'(/a,f9.4,a)')' longueur de la structure =',width,' m'		
   write(iu_11(iboat),12)(dis(i),i=1,5)
   write(iu_11(iboat),13)(fam(i),i=1,6)
   write(iu_11(iboat),*)' '
   if(ipoids.eq.1) then
      write(iu_11(iboat),*)' on tient compte du poids propre des panneaux (ipoids =1)'
   else
      write(iu_11(iboat),*)' on ne tient pas compte du poids propre des panneaux (ipoids =0)'
   endif																
   write(iu_11(iboat),'(1x,51(1h-)/)')						
else
   write(iu_11(iboat),'(/a,f9.4,a)')' length of the structure =',width,' m'
   write(iu_11(iboat),'(/2a)')' locations (x) of 5 sections where displ. and stress are given (in meters)'
   write(iu_11(iboat),'(t10,5(2x,f9.4))')(dis(i),i=1,5)
   write(iu_11(iboat),13)(fam(i),i=1,6)
   write(iu_11(iboat),*)' '
   if(ipoids.eq.1) then
      write(iu_11(iboat),*)'deadweight of the structure is considered (ipoids =1)'
   else	
      write(iu_11(iboat),*)'deadweight of the structure is not considered (ipoids =0)'
   endif												
   write(iu_11(iboat),'(1x,46(1h-)/)')		
endif															

if(langue==1) write(iu_11(iboat),419) nsolm, nsol,(nnsol(i),i=1,nsol) 	
if(langue==2) write(iu_11(iboat),420) nsolm, nsol,(nnsol(i),i=1,nsol)		

write(iu_14(iboat),*)'iteration 0'	
write(iu_14(iboat),*)'====================================================='           
write(iu_14(iboat),*)' '														



return


! formats ============================================================================================
 100 format(1x,a,i8)                                                                                          
 101 format(1x,a,e15.6)                                                                                       
 200 format(a)                                                                                                
   3 format(1h1,1x,70(1h*)/,           &                                                                       
            2x,1h*,4x,15a4,4x,1h*/,    &                                                                       
            2x,1h*,4x,60(1h+),4x,1h*/, &                                                                       
            2x,1h*,68x,1h*/,           &                                                                       
            2x,70(1h*)//)
                                                                                    
   4 format(15a4)                                                                                              
   5 format(1h1)                                                                                               
   6 format(///                                                   &                                            
      16x,'analysis of a structure composed of ',i3,' panel(s)'/  &                                            
      15x,49(1h+)/)                                                                                            
   7 format(/' avec resultats intermediaires')                                                                 
   8 format(/' avec resultats finaux + verifications')                                                         
   9 format(/' pas de resultats dans les transversaux (cadres).')                                              
  10 format(///                                                   &                                            
      ,16x,'analyse d''une structure a ',i3,' panneau(x)'/15x,45(1h+)/)                                        
  11 format(///' unites en metres et newtons',//,' nombre de termes', &                                        
      ' de la serie de fourier pour les charges ',i3//                &                                        
      ' efforts de bord nb et mb (buscage) --> file: busc.txt'/       &                                        
      '  ibusc =',i2,'  ( 0:sans nb et mb  et 1 : avec nb et mb)'/)                                            
  12 format(/' coordonnees (x) des 5 sections ou les resultats sont', &                                        
             ' fournis (en metre)'/t10,5(2x,f9.4))                                                             
  13 format(/' coefficients de cisaillement des semelles',        &                                            
               ' des transversaux',2(f6.3,2x)/                    &                                            
           42x,' des raidisseurs ',2(f6.3,2x)/                    &                                            
           42x,' des traverses   ',2(f6.3,2x)//)                                                               
  14 format(///' units: metre, newton and equivalent',                   &                                     
               ' water height (m) for lateral pressure'//                &                                     
      ' number of terms of the fourier series expansion',                &                                     
      ' for loads (jlph) =',i3//                                         &                                     
      ' with end forces nb & end moments mb defined in file: busc.txt'/  &                                     
      ' ibusc =',i2,'  ( 0:without nb & mb  et 1 : with nb & mb)'/)                                            
                                                          
 400 format(21x,20(1h*))                                                                                       
 
 419 format( ' cas de charge considérés:'/1x,25(1h=)/                   &                                      
             ' - nbre de cas de charge disponibles = ',i2/              &                                      
             ' - nbre de cas de charge analysés    = ',i2/              &                                      
             ' - numeros des cas analysés = ',10i3//)                                                          
 420 format( ' considered load cases:'/1x,22(1h=)/                      &                                      
             ' - available  load cases = ',i2/                          &                                      
             ' - considered load cases = ',i2/                          &                                      
             ' - nbr of the considered cases = ',10i3//)                                                       
                                                                                                              
 511 format('variables de conception et restrictions geometriques'/      &                                     
             52(1h=))                                                                                          
                                                                                  
 600 write(*,'(a)') 'Fichier d''input .txt manquant !!!'
     read(*,*)
     stop

 700 if (nbrMat.eq.0) then
	  if (langue.eq.1) then
		write(*,'(a,i2)') '!!! Attention ! Aucun materiau n''a ete defini pour la structure',iboat
		write(*,'(a)') 'Veuillez corriger vos donnees - appuyer sur ENTER pour poursuivre'
  	  endif
	  if (langue.eq.2) then
		write(*,'(a,i2)') '!!! Attention! No material has been defined for the structure',iboat
		write(*,'(a)') 'Please correct your data - press ENTER to resume'
	  endif
	  read(*,*)
	  stop
     endif
end
