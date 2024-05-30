subroutine comp_restri_local()

use param_section
use PARAM_SECTION_VECTOR

implicit double precision (a-h,o-z)


		
! 4.2 restrictions géométriques
!     =========================

do i=1,ntot(iboat)
   do j=1,mtot(iboat)
!      cijopt(i,j,iboat)=0.0d00
   enddo
enddo

kcont(iboat)=0

if(iprint.ge.1) write(*,*) 'restrictions geometriques'		
igeom_index(1,iboat)=kcont(iboat)+1

ncont=0
do nel=1,neto_vector(iboat)
   mnel=m2cont_vector(nel,iboat)
   do j=1,mnel
      kcontr=kcont(iboat)+j
!      read(iu_26(iboat),*)       !ncont !TODO Sert à quoi ce ncont ???											
!      read(iu_26(iboat),*)     (cijopt(i+ncont,kcontr,iboat),i=1,nvar(nel,iboat))
!      read(iu_26(iboat),*)       cjopt(kcontr,iboat)
!   	  read(iu_26(iboat),*)      cjmopt(kcontr,iboat)
!   	  read(iu_26(iboat),'(a)')       vnom2(kcontr,iboat)
!      read(iu_26(iboat),*)     ic_rest(kcontr,iboat)
!   	  read(iu_26(iboat),*)  ratio_rest(kcontr,iboat)		
   enddo
   kcont(iboat)=kcont(iboat)+mnel

   ! Restrictions sur les contraintes de sloshing
   ! ============================================
   nsloshm=panneau_vector(nel,iboat).nsloshm !m4cont_vector(nel,iboat)
   do j=1,nsloshm
      kcontr=kcont(iboat)+j											
!      read(iu_26(iboat),*) (cijopt(i+ncont,kcontr,iboat),i=1,nvar(nel,iboat))
!      read(iu_26(iboat),*) cjopt(kcontr,iboat)
!   	  read(iu_26(iboat),*) cjmopt(kcontr,iboat)
!   	  read(iu_26(iboat),'(a)') vnom2(kcontr,iboat)
   enddo
   kcont(iboat)=kcont(iboat)+nsloshm
   ncont = ncont + nvar(nel,iboat)


enddo
igeom_index(2,iboat)=kcont(iboat)


! 4.3 restrictions structurelles + restrictions particulières (sagging & hogging)
!     ===========================================================================


   ! 4.3.1 restrictions sur centre de gravite       
   !       ==================================       

   if(igrav_vector(iboat).ge.1) then
      igrav_index(1,iboat)=kcont(iboat)+1

      if(iprint.ge.1) write(*,*)'restrictions centre de gravite'
      ig_conlin(iboat)=1
      if(igrav_vector(iboat)==3) ig_conlin(iboat)=2
      do j=1,ig_conlin(iboat)
         kcont(iboat)=kcont(iboat)+1
 !        read(iu_26(iboat),*)  cjopt(kcont(iboat),iboat)
 !  	     read(iu_26(iboat),*) cjmopt(kcont(iboat),iboat)
 !        read(iu_26(iboat),'(a)')  vnom2(kcont(iboat),iboat)		
 !        read(iu_26(iboat),*) (cijopt(i,kcont(iboat),iboat),i=1,ntot(iboat))							
      enddo

      igrav_index(2,iboat)=kcont(iboat)
   endif


   !	  restriction sur inertie							
   !	  =======================
   
   if(inert_vector(iboat).ne.0) then													
      inert_index(1,iboat)=kcont(iboat)+1

      if(iprint.ge.1) write(*,*)'restrictions sur inertie'			
      kcont(iboat)=kcont(iboat)+1														  
!      read(iu_26(iboat),*)  cjopt(kcont(iboat),iboat)
!      read(iu_26(iboat),*) cjmopt(kcont(iboat),iboat)
!      read(iu_26(iboat),'(a)')  vnom2(kcont(iboat),iboat)	
!      read(iu_26(iboat),*) (cijopt(i,kcont(iboat),iboat),i=1,ntot(iboat))								

      inert_index(2,iboat)=kcont(iboat)
   endif												                
   
   !	  restriction sur module							            
   !	  ======================
   
   if(imod_vector(iboat).ne.0) then													
      imod_index(1,iboat)=kcont(iboat)+1

      if(iprint.ge.1) write(*,*)'restrictions sur module sectionnel'	
      kcont(iboat)=kcont(iboat)+1													
!      read(iu_26(iboat),*)  cjopt(kcont(iboat),iboat)
!      read(iu_26(iboat),*) cjmopt(kcont(iboat),iboat)
!      read(iu_26(iboat),'(a)')  vnom2(kcont(iboat),iboat)	
!      read(iu_26(iboat),*) (cijopt(i,kcont(iboat),iboat),i=1,ntot(iboat))		

      imod_index(2,iboat)=kcont(iboat)
   endif												              
   
!endif                   


!do iboat=1,nfile	

   !	  restriction sur poids		
   !    ===================== 

  if(iweight_vector(iboat).ne.0) then			
     iweight_index(1,iboat)=kcont(iboat)+1

     if(iprint.ge.1) write(*,*)'restrictions sur poids'	
     kcont(iboat)=kcont(iboat)+1													
!     read(iu_26(iboat),*)  cjopt(kcont(iboat),iboat)
!     read(iu_26(iboat),*) cjmopt(kcont(iboat),iboat)
!     read(iu_26(iboat),'(a)')  vnom2(kcont(iboat),iboat)		
!     read(iu_26(iboat),*) (cijopt(i,kcont(iboat),iboat),i=1,ntot(iboat))								

     iweight_index(2,iboat)=kcont(iboat)
  endif																


   !	  restriction sur cout								                     
   !    ====================
   
  if(iprice_vector(iboat).ne.0) then													
     iprice_index(1,iboat)=kcont(iboat)+1

     if(iprint.ge.1) write(*,*)'restrictions sur cout'				
     kcont(iboat)=kcont(iboat)+1													
!     read(iu_26(iboat),*)  cjopt(kcont(iboat),iboat)
!     read(iu_26(iboat),*) cjmopt(kcont(iboat),iboat)
!     read(iu_26(iboat),'(a)')  vnom2(kcont(iboat),iboat)				
!     read(iu_26(iboat),*) (cijopt(i,kcont(iboat),iboat),i=1,ntot(iboat))		

     iprice_index(2,iboat)=kcont(iboat)
  endif												              

   
   
  ! 4.3.2 restrictions structurelles standards
  !       ====================================
   
  if(iprint.ge.1) write(*,*) 'restrictions structurelles'
  istruc_index(1,iboat)=kcont(iboat)+1

  do nel=1,neto_vector(iboat)
     mnel=panneau_vector(nel,iboat).m1cont
     do j=1,mnel
        kcontr=kcont(iboat)+j
!        read(iu_26(iboat),*)      cjopt(kcontr,iboat)
! 	    read(iu_26(iboat),*)     cjmopt(kcontr,iboat)
!   	    read(iu_26(iboat),'(a)')      vnom2(kcontr,iboat)		
!        read(iu_26(iboat),*)    ic_rest(kcontr,iboat)
!   	    read(iu_26(iboat),*) ratio_rest(kcontr,iboat)
   
!        read(iu_26(iboat),*) (cijopt(i,kcontr,iboat),i=1,ntot(iboat))					
     enddo
     kcont(iboat)=kcont(iboat)+mnel
  enddo
  istruc_index(2,iboat)=kcont(iboat)



  ! Restrictions sur les contraintes de fatigue
  ! ============================================
if(nsol_fatigue.gt.0) then

  do nel=1,neto_vector(iboat)
     nfatiguem=panneau_vector(nel,iboat).nfatiguem   !m5cont_vector(nel,iboat)
     if(nfatiguem.eq.1) then
        kcontr=kcont(iboat)+1											
!        read(iu_26(iboat),*) (cijopt(i,kcontr,iboat),i=1,ntot(iboat))
!        read(iu_26(iboat),*) cjopt(kcontr,iboat)
!  	    read(iu_26(iboat),*) cjmopt(kcontr,iboat)
!  	    read(iu_26(iboat),'(a)') vnom2(kcontr,iboat)


        write(26,*) 'damage_fatigue = ', cjopt(kcontr,iboat), ' nel = ',nel,'    subroutine comp_restri_local'
        write(27,*) '     borne_sup = ',cjmopt(kcontr,iboat), ' nel = ',nel,'    subroutine comp_restri_local'



     endif
     kcont(iboat)=kcont(iboat)+nfatiguem
  enddo

endif

!enddo


! 4.3.2 restrictions sur le moment ultime (poutre navire)
!       =================================================


!if (nfile.le.1) then

   if(irestr_vector(iboat).ge.1) then
      irestr_index(1,iboat)=kcont(iboat)+1

      if(iprint.ge.1) write(*,*)'restrictions moment ultime'
      
      
   !  restriction sur ult (sagging)
   !  =============================
      kcont(iboat)=kcont(iboat)+1
!      read(iu_26(iboat),*)  cjopt(kcont(iboat),iboat)
!      read(iu_26(iboat),*) cjmopt(kcont(iboat),iboat)
!      read(iu_26(iboat),'(a)')  vnom2(kcont(iboat),iboat)
!      read(iu_26(iboat),*) (cijopt(i,kcont(iboat),iboat),i=1,ntot(iboat))	
      
      				
   !  restriction sur ult (hogging)
   !  =============================
      kcont(iboat)=kcont(iboat)+1
!      read(iu_26(iboat),*)  cjopt(kcont(iboat),iboat)
!      read(iu_26(iboat),*) cjmopt(kcont(iboat),iboat)
!      read(iu_26(iboat),'(a)')  vnom2(kcont(iboat),iboat)		
!      read(iu_26(iboat),*) (cijopt(i,kcont(iboat),iboat),i=1,ntot(iboat))		
      
      irestr_index(2,iboat)=kcont(iboat)
      		
   endif
   
if (idebug_conlin.eq.1) then
      write(iu_conlin(iboat),*)  'cjmopt  =',cjmopt(:,iboat)
      write(iu_conlin(iboat),*)  'cjopt   =',cjopt (:,iboat)
      write(iu_conlin(iboat),*)  'kcont   =',kcont(iboat)
      write(iu_conlin(iboat),'(a)')  'vnom2   =',vnom2(1:kcont(iboat),iboat)
      write(iu_conlin(iboat),*)  'cijopt  =',((cijopt(i,j,iboat),j=1,mtot(iboat)),i=1,ntot(iboat))
      write(iu_conlin(iboat),*)  'end comp restri  '
      write(iu_conlin(iboat),*)
endif

return
end
