program  lbr5

use param_section
!!!use omp_lib ! utilisé pour compilateur Intel pour multi-threads !!!

implicit double precision (a-h,o-z)
 
idebug = 0  !=> pas de debug de lbr4
idebug_conlin=0  !=> debug de conlin

! ====================================================================================================
! begin time counter
! ====================================================================================================

tic=timef()

! ====================================================================================================
! intialization of multisection optimization parameters
! ====================================================================================================

!initialize global parameters as well as global vector storing header's data
! (vérifie aussi la clé de sécurité)
call init_param_multi()

!!!!!!!!!!!!!!!!!!call read_element_sizes()

call allocate_param_section_header()

! ====================================================================================================
! Determination of main parameter (neto, nbr_Mat) in order to allocate vector
! Maud : pas besoin de neto mais bien de nfile (connu)
! Maud nbr_Mat temporairement fixé à 10
! ====================================================================================================
!#pragma omp parallel do
do iboat=1,nfile                    !doit être mis en commentaires
	call get_main_parameter()       !doit être mis en commentaires
enddo                               !doit être mis en commentaires

call allocate_param_materiau_vector()

do iboat=1,nfile
! ====================================================================================================
!  opening section files
! ====================================================================================================

!   call open_section_output_files()   ! necessite iboat et section_files(iboat)
   call open_section_file() !remplacé par open_section_output_files

!call read_ship_input_file()   !lecture de tout le fichier de données
   
   
   call get_section_header()  !inclus dans  read_ship_input_file()  

	     ! call read_element_sizes() !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


   call put_section_header_vector()
   call put_param_materiau()
!   call deallocate_param_section()
   call deallocate_section_header()  ! à mettre sous condition nfile>1 et adapter la suite en conséquence
! ====================================================================================================
!  opening scratch files
! ====================================================================================================
   call open_scratch_files()
enddo

call check_multi()

! ====================================================================================================
! iterations on structural optimization
! ====================================================================================================


call allocate_param_opti_global()
call allocate_param_read_write_vector()
call allocate_param_restri_global_vector()

do while (itera.le.iter1)

   obj_global=0.0d00
   !jobj_global=0.0d+00
   shift_ntot = 0
   shift_nr = 0
   shift_mtot = 0
   shift_neto = 0

   do iboat=1,nfile
      section_file=section_files(iboat)

	  !call allocate_param_opti_global() !!!!!!!!!!!!!!!!!!!

      call get_section_header_vector()
      call allocate_section()

      call open_local()

      call allocate_panel_data()
!      call init_panel_data()
      call get_optim_data()

	  call loop()   ! s'arranger pour que loop recopie des données lues plus tot pour chaque panneau


	  

       call put_panel_data_global()

      if(iopti.eq.1.and.itera.ne.iter1) then 
	  !On analyse le dernier échantillon, mais on ne l'optimise pas
		 if (itera.eq.0)then
			itera=1								
		 endif

  	     call opti_local ()

!		 call write_panel_data()
		 if (itera.eq.1.and.iboat.ne.nfile)then
			itera=0 !on remet à 0 pour les structures 2, 3 -> nfile		
		 endif

         call opti_local_to_global()

	  endif
      call put_optim_data()
      call deallocate_panel_data()

      call deallocate_section()
      call deallocate_section_header()
	  call deallocate_param_materiau()

	  !call deallocate_param_opti_global() !!!!!!!!!!!!!!!!!!!!!!!!!!!

   enddo

   
   if(iopti.eq.1.and.itera.ne.iter1) then !On analyse le dernier échantillon, mais on ne l'optimise pas
      		
!      do iboat=1,nfile
!         rewind iu_26(iboat)
!      enddo

	  call read_ega_multisection()
      call comp_reduced_var_global() 
	  call opti_global_multisection()


	  !!!!!!!!!!!! recalcule L, et M1

   else
		itera=iter1
   endif

   ! Ecriture des résultats dans le fichier up
   if (itera.ne.iter1) then !car last iteration : on refait un calcul sans opt => pas utile de réécrire le fichier
     do iboat=1,nfile
		call get_section_header_vector()
		call allocate_param_section()
		call allocate_panel_data()
!		call allocate_param_opti_local()
		!call allocate_param_read_write_vector()
		call get_optim_data() !<-	lcont, ipts etc (TODO il faudra supprimer ça !)
		call get_panel_data_global() !<- rempli panneau en fct de panneau_vector

		call write_panel_data()
		!call ansys()
		!call write_panel_XML()
		!call read_panel_XML()
		
		!call deallocate_param_read_write_vector()
!		call deallocate_param_opti_local()
		call deallocate_panel_data()
	    call deallocate_param_section()
		call deallocate_section_header()
		call deallocate_param_materiau()
     enddo
    endif
   !

   call rewind_to_first_panel()

   itera=itera+1
enddo


do iboat=1,nfile
	call vibration()
enddo


do iboat=1,nfile
	call close()
enddo

! ====================================================================================================
! end time counter
! ====================================================================================================

call deallocate_param_opti_global()
call deallocate_param_read_write_vector()
call deallocate_param_restri_global_vector()
!call deallocate_param_opti_local_vector()

call deallocate_param_multi()

call deallocate_param_materiau_vector()
call deallocate_param_section_header()

toc=timef()

if(langue.eq.1) write(*,'(1x,a,f8.1,a)') 'temps de calcul   :',toc,' sec'
if(langue.eq.2) write(*,'(1x,a,f8.1,a)') 'elapsed time      :',toc,' sec'


! ====================================================================================================
! exit computations
! ====================================================================================================

stop
end
