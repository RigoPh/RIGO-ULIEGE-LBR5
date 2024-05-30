subroutine read_struct_restrictions(nel)

use param_section

implicit double precision (a-h,o-z)
double precision m11(20),m1_reference
m11(:)=0

! 3.3 lecture des m1 restrictions structurelles et de leur borne
!     -----------------------------------------------------------
       if(itera.eq.0) call annuli (panneau(nel).ipts3(1),iptmax)       !        ipt3=0

       is=0  ! is = compteur des cas de charge selectionnés (1 à nsol)


       do 216 ij=1,nsolm  ! ij = compteur des cas de charge, selectionnés ou non (1 à nsolm)
                          ! is = compteur des cas de charge selectionnés (1 à nsol)
          m11(ij)=0  ! mise à zéro
          icc=0      ! icc=0 cas de charge non selectionné
          
          do k=1,nsol
            if(nnsol(k).eq.ij) then
               icc=1   ! icc=1 cas de charge selectionné
			   !if(is_loadcase_fatigue(ij).eq.0) then
                  is=is+1           ! is = compteur des cas de charges sélectionnés 
				                    !      qui ne sont pas "fatigue"
                  m1tabl(nel,is)=0  ! mise à zéro
			   !endif
			endif
          enddo
          
          read (iu_10(iboat),'(a80)',end=900) title_loadcase(ij,nel)       ! titre cas de charge

!!! why		  if((is_loadcase_fatigue(ij).eq.0)) then

             read (iu_10(iboat),*,end=900) panneau(nel).m1(ij)               ! m1 (nbre de restrictions struct.du panneau nel)
			 if (ij.eq.1) m1_reference = panneau(nel).m1(ij)
			 if (panneau(nel).m1(ij).ne.m1_reference) then
				!write(*,*) ' *** Error pannel',nel
                !write(*,*) ' You must have the same number of structural constraint for each load case!'
				!write(*,*) ' Please correct your data'
				!read(*,*)
				!stop
			 endif
          
          	 !mtest = MAXVAL(panneau(nel)%lcont4_init)
			 m1max = 50 !MAXVAL(m1(:,nel)) !!! TODO A CHANGER
			 if (.not.associated(panneau(nel)%lcont4_init).and.nsolm.gt.0.and.m1max.gt.0) then
			 !if (mtest.le.0.and.nsolm.gt.0.and.m1max.gt.0) then
		 		allocate (panneau(nel)%lcont4_init(nsolm,m1max,2))
				allocate (panneau(nel)%lcont4(nsolm,m1max,2))
				allocate (panneau(nel)%cjmax9_init(nsolm,m1max))
				allocate (panneau(nel)%cjmax9(nsolm,m1max))
				allocate (panneau(nel)%inv3_init(nsolm,m1max))
				allocate (panneau(nel)%inv3(nsolm,m1max))
				
				allocate (panneau(nel)%lcont(2,(m1max*nsolm)))
				!allocate (panneau(nel)%ipts2(iptmax))
				allocate (panneau(nel)%cjmax(m1max*nsolm))
				allocate (panneau(nel)%inv(m1max*nsolm))

				panneau(nel).lcont4_init(:,:,:) = 0
				panneau(nel).lcont4(:,:,:) = 0
				panneau(nel).cjmax9_init(:,:) = 0.d00
				panneau(nel).cjmax9(:,:) = 0.d00
				panneau(nel).inv3_init(:,:) = 0
				panneau(nel).inv3(:,:) = 0

				panneau(nel).lcont(:,:) = 0
				!panneau(nel).ipts2(:) = 0
				panneau(nel).cjmax(:) = 0
				panneau(nel).inv(:) = 0
			 endif
          	
          
!            lecture de la liste des restrictions (en fct de la valeur de m1)
!            lcont(nsolm,neto,m1max,2),inv(nsolm,neto,m1max),cjmax(nsolm,neto,m1max)
          
             if(panneau(nel).m1(ij).gt.50) then
                write(*,*)  ' *** panneau',nel
                write(*,*)  ' nbre de restr. struct. incorrecte (max=50) =',panneau(nel).m1(ij)
                write(iu_31(iboat),*)' nbre de restr. struct. incorrecte (max=50)   =',panneau(nel).m1(ij)
                write(iu_31(iboat),*)' too much struct. const. (max=50 per panel)   =',panneau(nel).m1(ij)
                write(iu_14(iboat),*)' *** panneau',nel									
	            write(iu_14(iboat),*)' nbre de restr. struct. incorrecte (max=50)   =',panneau(nel).m1(ij)		
   	            iff=1
                write(*,*) 'error'
			    read(*,*)
	            return
          
             else if (panneau(nel).m1(ij).eq.0) then
                goto 216          ! vers cas de charge suivant
          
             elseif((panneau(nel).m1(ij).ge.1).and.(panneau(nel).m1(ij).le.50)) then
	            if(icc.eq.1) m1tabl(nel,is)=panneau(nel).m1(ij)
                m11(ij)=panneau(nel).m1(ij)
                !if(itera.eq.0) then
                   do i=1,panneau(nel).m1(ij)
                      read (iu_10(iboat),*,end=900) icn,panneau(nel).lcont4_init(ij,i,1),panneau(nel).cjmax9_init(ij,i),&
	 	   	                                       panneau(nel).inv3_init(ij,i),  panneau(nel).lcont4_init(ij,i,2)
					  panneau(nel).lcont4(ij,i,1) = panneau(nel).lcont4_init(ij,i,1)
					  panneau(nel).lcont4(ij,i,2) = panneau(nel).lcont4_init(ij,i,2)
					  panneau(nel).inv3(ij,i) = panneau(nel).inv3_init(ij,i)
					  panneau(nel).cjmax9(ij,i) = panneau(nel).cjmax9_init(ij,i)
                   enddo
                !else
                !   do i=1,panneau(nel).m1(ij)
                !      read (iu_10(iboat),*,end=900)
                !   enddo
                !endif          
             elseif((panneau(nel).m1(ij).le.-1).and.(panneau(nel).m1(ij).gt.-ij))  then
                
				!!! Condition plus possible via l'interface
				
				iss=-panneau(nel).m1(ij)    ! iss cas de charge de référence
                panneau(nel).m1(ij)=m11(iss)
                m11(ij)=panneau(nel).m1(ij)
                if(icc.eq.1) m1tabl(nel,is)=panneau(nel).m1(ij)
                !if(itera.eq.0) then
                   do i=1,panneau(nel).m1(ij)
                      panneau(nel).lcont4(ij,i,1)=panneau(nel).lcont4(iss,i,1)
                   	  panneau(nel).cjmax9(ij,i) = panneau(nel).cjmax9(iss,i)
                      panneau(nel).inv3  (ij,i)  =panneau(nel).inv3  (iss,i)
                      panneau(nel).lcont4(ij,i,2)=panneau(nel).lcont4(iss,i,2)
                   enddo
                !endif
        
             elseif (panneau(nel).m1(ij).le.-ij) then

				!!! Condition plus possible via l'interface

                write(*,*)  ' *** panneau',nel
                write(*,*)  ' fausse valeur de m1 =',panneau(nel).m1(ij),'=< ',-ij
                write(*,*)  ' wrong value for m1  =',panneau(nel).m1(ij),'=< ',-ij
                write(iu_31(iboat),*)' wrong value for m1  =',panneau(nel).m1(ij),'=< ',-ij
                write(iu_14(iboat),*)  ' *** panneau',nel										
	            write(iu_14(iboat),*)  ' fausse valeur de m1 =',panneau(nel).m1(ij),'=< ',-ij					
	            iff=1
	            write(*,*) 'error'
   	            read(*,*)																	
               stop 55
             endif
        
        
             if (icc.eq.1) then
                panneau(nel).m1cont=panneau(nel).m1cont+panneau(nel).m1(ij)  ! nbre de restriction pour tous les cas de charges (pour le panneau nel)
                m1tot(iboat) =m1tot(iboat) +panneau(nel).m1(ij)            ! nbre de restriction pour tous les cas de charges et toute la structure
             endif
        
               
          if((iopti.ge.1).and.(itera.eq.0).and.(icc.eq.1)) then
             do i=1,panneau(nel).m1(ij)
                ! vérification de la compatibilité entre variable xi et restriction 4 (dmin)
                if((panneau(nel).lcont4(ij,i,1).eq.4).and.(nxit(1,nel,iboat).ne.1)) then
                   write(*,*) 'Error panel:',nel
				   write(*  ,220)
                   write(*  ,220)
                   write(*  ,220)
                   write(*  ,220)
                   write(iu_31(iboat),220)
                   write(iu_11(iboat) ,220)
	 	           write(iu_14(iboat) ,220)		
                endif
                ! vérification du choix des points de calcul
                if(panneau(nel).lcont4(ij,i,2).gt.panneau(nel).ipts) then
                   yp=panneau(nel).lcont4(ij,i,2)
				   write (*,*) 'Error panel:',nel
                   write(*  ,*)          'iy =',yp,' > ipt=',panneau(nel).ipts,' est incorrecte'
                   write(iu_31(iboat),*) 'iy =',yp,' > ipt=',panneau(nel).ipts,' est incorrecte'
                   write(iu_14(iboat) ,*)'iy =',yp,' > ipt=',panneau(nel).ipts,' est incorrecte'

                   write(*,*) 'stop'
                   read(*,*)
                   stop

	 	          iff=1
                endif

             

               ! vérification du choix des points de calcul des sensibilités vis à vis de traverses
                if(panneau(nel).mt.ge.1) then
                   if((panneau(nel).lcont4(ij,i,1).ge.40).and.(panneau(nel).lcont4(ij,i,1).le.45))     then
                      do it=1,panneau(nel).mt
                         ax=dabs(panneau(nel).abtr(it)/ypts(panneau(nel).lcont4(ij,i,2),nel) )
                         if(ax.ge.(0.99)) then         !  ipt3= nø de la trav. concernee
                            if(ax.le.(1.01)) panneau(nel).ipts3(panneau(nel).lcont4(ij,i,2))=it
                         endif
                      enddo
                   endif
             
				endif   
			 enddo
            endif   
 !!!         endif
 216   continue   ! boucle sur les cas de charges


return

! -------------------------------------------------------------------------------
! -------------------------------------------------------------------------------
900 write(* ,*)'erreur de lecture : "end of file"'
    write(iu_11(iboat),*)'erreur de lecture : "end of file"'
    write(iu_14(iboat),*)'erreur de lecture : "end of file"'
    write(*,*) 'stop'
    read(*,*)
    stop

! 6.  les formats.
!     ============
 220  format(/' attention'/1x,14(1h*)/' vous avez sélectionné la ',          &
      'restrictions nø 4 (d<dmin) dite de hughes'/' alors que d n''est'      &
      ' pas une variable de conception.'/' cela peut entraîner des'          &
      ' difficultés dans conlin !!'/)
end
