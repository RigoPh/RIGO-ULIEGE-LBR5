subroutine lbr4()

use param_section

implicit double precision (a-h,o-z)

!     ==================================================================
!14.0 mise en forme des equations de continuite et resolution (bo1)
!     ==================================================================


!     boucle externe sur les termes de la serie de fourier (boucle 2201)
!    14.1  boucle interne (bo1) sur les panneaux et sur les cas de charges,
!          structure bi-appyée           
!    14.2  boucle interne (co1/co4) pour les forces de bords
!    14.3  calcul des sensibilités (mdr2)
       

if(langue.eq.1)then
   write(*,100)'mise en forme des equations de continuite (sr bo1)'
   write(*,*)
else
   write(*,100)'assembly of the rigidity matrix (sr bo1)'
   write(*,*)
endif

do ijk=1,jlph
   igoto2201=0   !

   if(mod(ijk,2).eq.1) then  
      iterm=1              ! iterm = 1 si ijk est impair
      ibat =1              ! 'pour le terme ',ijk,'ibat=1'
   else       
      iterm=2              ! iterm = 2 si ijk est pair
      ibat=0               ! (terme pair)
      if(jlph2.le.0) then  ! jlph negatif, seul les termes impairs sont considerés
         ifonct(iterm)=0   ! skip termes pairs 
  	     igoto2201=1	
      endif

      if(igoto2201.ne.1) then
		 ichag = 0
		 do nel=1,neto
			if(panneau(nel).icha.eq.1) ichag=1
		 enddo
!        if((imom.ne.0).or.(ichag.ne.0).or.(ibusc.ne.0)) then 
         if((imom.eq.0).and.(ichag.eq.0).and.(ibusc.eq.0)) then 
            ifonct(iterm)=0
            igoto2201=1  
	     endif  
      endif   

   endif

   if(igoto2201.ne.1) then  

      if(langue.eq.1) write(*,8000)'serie de fourier : terme no',ijk	
      if(langue.eq.2) write(*,8000)'fourier series: term nr',ijk		
 8000 format('+',a,1x,i2)
  
      ifonct(iterm)=1 

      rewind iu_21(iboat)

      call lamsr(lam,ijk,pi,width)

!      call init_panel_data()

      do nel=1,neto

         argq(:)=0.d00

         const(:)   =0.d00
         const2(:,:)=0.d00
         const3(:,:)=0.d00

         darg(:,:)     =0.d00
         ich=panneau(nel).icha

         if(ipoids.eq.1) ich=1
		 
		 call bo1(ijk,ibat,ich,nel)
		 
      end do

! ==================================================================
! ==================================================================

!14.3 module d'optimisation - calcul des sensibilites (subr. mdr2)
!     ------------------------------------------------------------
!      dzsn(ne,9*neto,10)sauvé dans allong
!      la taille max de dzsn pour neto = 100 est 7.2 mio, soit 57.6 mb

      if(iopti.ge.1)then
         call mdr2()

		 if (idebug.eq.1) then
            write(9999,*) 
            write(9999,*) 'mdr  -----'
            write(9999,*) 
            write(9999,*)' zsn='
            write(9999,*)  zsn(:,1:nsol)
            write(9999,*)'dzsn='
            write(9999,*) dzsn(:,:,1:nsol)
		 endif

      endif
   endif  !dad goto 2201
enddo     ! fin de la boucle sur les termes de fourier (jlph)                                                   

do nel=1,neto
   rewind(iu_scratch_2(iboat,nel))	!extension neto	!fev2007
   rewind(iu_scratch_3(iboat,nel))
   if (idebug.eq.1) rewind(6666)
   if (idebug.eq.1) rewind(6667)
end do


!  =========================================================
!  =========================================================

! 15.0 determination des contraintes et deplacements (subr. bo2)
!     =========================================================

if(langue==1) write(*,408)
if(langue==2) write(*,409)
if((itera.eq.0).and.(iopti.ge.1))write(iu_31(iboat),512)

!     img= compteur des restr. structurelles général (= 1 ou 2 si rest sur centre de gravité)

caschge_fatigue=0

do 2401 is=1,nsol

   if (is_selected_loadcase_fatigue(is).eq.1) then
       caschge_fatigue     =caschge_fatigue+1
   endif

   if (is_selected_loadcase_fatigue(is).eq.1) then
    
	if(langue.eq.1)write(*,8000)'cas de charge (FATIGUE) no',is
	if(langue.eq.2)write(*,8000)'load case (FATIGUE) nr',is
      
   ELSE
   
    if(langue.eq.1)write(*,8000)'cas de charge no',is
	if(langue.eq.2)write(*,8000)'load case nr',is

   ENDIF


 
   if(langue==1 ) then ! french
      write(iu_11(iboat),2402) is
      if(iopti.ge.1) write(iu_31(iboat),2402)is
      write(iu_12(iboat),2402) is
   else  ! english
      write(iu_11(iboat),2403) is
      if(iopti.ge.1) write(iu_31(iboat),2403)is
      write(iu_12(iboat),2403) is
   endif
   !  read (in file iu_32(iboat)) the list of constraints saved in subr. ent 

      im=0
      if(iopti.ge.1) then
         do ip=1,neto
            do i=im+1,im+m1tabl(ip,is)
               !read(iu_32(iboat)) panneau(ip).lcont(1,i-im),panneau(ip).lcont(2,i-im), &
				!				  panneau(ip).inv(i-im),panneau(ip).cjmax(i-im)
			   panneau(ip).lcont(1,i-im) = panneau(ip).lcont4(nnsol(is),i-im,1)
			   panneau(ip).lcont(2,i-im) = panneau(ip).lcont4(nnsol(is),i-im,2)
			   panneau(ip).inv(i-im) = panneau(ip).inv3(nnsol(is),i-im)
			   panneau(ip).cjmax(i-im) = panneau(ip).cjmax9(nnsol(is),i-im)
            enddo
            im=im+m1tabl(ip,is)
         enddo
      endif

   !compteur des restr. structurelles pour le cas de charges en cours
   im=0				
!   rewind(iu_29(iboat))
   nbuck=0
				
   do 2301 nel=1,neto
      ich=panneau(nel).icha
      if(ipoids.eq.1) ich=1
      rewind (iu_scratch_1(iboat,nel))

      call bo2(jlph,ich,im,m1tabl(1,is),nbuck,nel)		

      rewind (iu_scratch_1(iboat,nel))
      rewind (iu_scratch_2(iboat,nel))
      rewind (iu_scratch_3(iboat,nel))
      if (idebug.eq.1) rewind (6666)
      if (idebug.eq.1) rewind (6667)

      !15.1 sauvetage des donnees graphiques
      !---------------------------------
      if(dessin.eq.0)goto 2301

!      call save(nel,indaig,indrai,itype(nel))      

   2301 continue

! compteur general des restr struct
   img=img+im   
   rewind iu_27(iboat)	
   rewind iu_30(iboat)												

    !15.2 impressions de sigma ultime panneaux (paik)
    !--------------------------------------
   if (impr2.ge.-2) then							
      if(langue==2 ) then 
         ! english
         write(iu_12(iboat),'(/a/40(1h-))') ' ultimate strength of panels (paik)'
         !s ult calculé via subr. result
         !nx   calculé dans subr. contr
         write(iu_12(iboat),'(2a)')'nel,  sig ult (n/mm2) > ? >',' mean compressive axial stress, (nx)'       
      else  
         ! french
         write(iu_12(iboat),'(/a/40(1h-))')' resist ultime panneau (paik)'
         write(iu_12(iboat),'(2a)')'nel,  sig ult (n/mm2) > ? >',' contrainte moyenne axiale en compression (nx)'
      endif
   endif											

   do inel=1,neto
      if(itype(inel).ne.5) then													
         write(iu_12(iboat),'(i3,1x,f10.5,18x,f10.5)') inel, sxm(iptmax+inel)/1.e6,-sxm(iptmax+neto+inel)/1.e6		
      else	 
         write(iu_12(iboat),'(i3,1x,a9,18x,a9)') inel,'epontille','epontille'                         
      endif
   enddo
															
								
   !15.3 analyse des résultats extrêmes 
   !-----------------------------------
   call analys()

   rewind iu_27(iboat)	
   rewind iu_30(iboat)

2401 continue


!  =========================================================
!   restrictions fatigues
!  =========================================================

img=img+nsol_fatigue

m5cont(:) = 0
!if(nsol_fatigue.gt.0) then		!fat_new
if(IS_FATIGUE.gt.0) then		!fat_new
   call fatigue_damage()

   do nel=1,neto
!!!      m5cont(nel)=panneau(nel).nfatiguem !

      if((panneau(nel).nfatiguem).eq.1) then
!!!	     call save_fatigue
      endif
   enddo
endif

  100 format(1x,a,i8)                                                                                          
  408 format(/'determination des contraintes et deplacements'/46(1h=))
  409 format(/'calculation of the displacements & stresses'/46(1h=))
  512 format(/'restrictions'/14(1h=))
 2402 format(//30(1h*)/' *** cas de charge nø',i2,' ***'/30(1h*)/)
 2403 format(//25(1h*)/' *** load case no',i2,'  ***'/25(1h*)/)

return
end
