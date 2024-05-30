subroutine beam_theory()
 
use param_section

implicit double precision (a-h,o-z)

character*12 com
double precision, allocatable, save :: dciacs(:)
double precision, allocatable :: thicknet(:)
double precision, allocatable :: thickgro(:)
allocate (dciacs(9*nmax))
allocate (thicknet(neto))
allocate (thickgro(neto))
dciacs(:) = 0.d00
thicknet(:) = 0.d00
thickgro(:) = 0.d00

!     ==================================================================
!14.0 calcul de la poutre navire
!     ==================================================================



   ! listing resultats				
   open(iu_13(iboat),file=iunit5)

   if(itera.lt.iter1)then
      write(iu_13(iboat),'(t40,a,t50,i2/,t40,12(1h=))') 'iteration',itera
   elseif (itera.eq.iter1) then										
	  write(iu_13(iboat),'(t40,a,t50,i2/,t40,12(1h=))') 'iteration',iteram
   endif
   
   write(iu_13(iboat),'(//a)') '!!!structural analysis using beam theory!!!'	



!  neutral axis and moment of inertia (gross, net and plate scantling)
!  ===================================================================
	



   write(iu_13(iboat),'(///a/,34(1h-)/)') '/neutral axis and moment of inertia/'						

!  net scantling
!  -------------

   call inertia(yneutnet,dyneutnet,inet,dinet,0,0)			

!  gross scantling
!  --------------- 
   call inertia(yneutgro,dyneutgro,igro,digro,1,0)				
   write(iu_13(iboat),'(t15,a,t33,a/,48(1h-))') 'net scantling','gross scantling'								
   write(iu_13(iboat),'(a,t18,f6.3,t37,f6.3)')  'yneut [m]',yneutnet,yneutgro	
   write(iu_13(iboat),'(a,t17,f8.3,t36,f8.3)')  'ixx [m4]',inet,igro				

!  plate scantling
!  ---------------

!  net scantling
   call inertia(yneutplnet,dyneutplnet,iplnet,diplnet,0,1)	

!  gross scantling
   call inertia(yneutplgro,dyneutplgro,iplgro,diplgro,1,1)	



!  structural analysis based on beam theory							
!  ========================================
	  

!  hull girder shear stresses
!  -------------------------- 

   do nel=1,neto
!      read(iu_25(iboat)) epsa,epsr,thicknet(nel)
	  thicknet(nel) = panneau(nel).delta 
	  thickgro(nel)= panneau(nel).delta + panneau(nel).corro(1)
   enddo
!   rewind iu_25(iboat)

!  net scantling
!  -------------
   call shear(thicknet,yneutplnet,dyneutplnet,iplnet,diplnet,taunet,dtaunet)

!  gross scantling
!  ---------------

   !thickgro = panneau(nel).delta + panneau(nel).corro(1)
   call shear(thickgro,yneutplgro,dyneutplgro,iplgro,diplgro,taugro,dtaugro)
   write(iu_13(iboat),'(///a/,32(1h-))') '/shear stress and normal stress/'	
   
   do is=1,nsol														
	  write(iu_13(iboat),'(/a,t12,i2/,14(1h*))')    '/load case/',is			
	  write(iu_13(iboat),'(t42,a,t84,a/,106(1h-))')	'net scantling','gross scantling'	
	  write(iu_13(iboat),'(a,t12,a,t33,a,t46,a,t60,a,t75,a,t88,a,t102,a/)' )               &
	                                            'panel','stress [n/mm2]','start','center', &
												'end','start','center','end'

      do nel=1,neto
		 if(itype(nel).ne.5) then

!	  hull girder bending stresses
!	  ----------------------------
		 call imprtau(nel,is)											  						

!	  net scantling
!	  -------------
	     call bending(nel,bm1(is),yneutnet,dyneutnet,inet,dinet,signet,dsignet)

!	  gross scantling
!	  ---------------
	     call bending(nel,bm1(is),yneutgro,dyneutgro,igro,digro,siggro,dsiggro)	
		 write(iu_13(iboat),'(1x,i3,t15,a,t31,f8.3,t57,f8.3,t73,f8.3,t99,f8.3)') nel,'/sx/',(signet(1)/1.e+06), &			
                                                                                            (signet(2)/1.e+06), &
																							(siggro(1)/1.e+06), &
																							(siggro(2)/1.e+06)
!	stiffener bending stresses
!	--------------------------
!         read(iu_25(iboat)) epsa,epsr,delta,hya,dya,wya,tya,hxr,dxr,wxr,txr,phil,q,epais,epsa2
		 epsa = panneau(nel).epsa
	     epsr = panneau(nel).epsr
		 delta = panneau(nel).delta
		 hya = panneau(nel).hya
		 dya = panneau(nel).dya
		 wya = panneau(nel).wya
		 tya = panneau(nel).tya
		 hxr = panneau(nel).hxr
		 dxr = panneau(nel).dxr
		 wxr = panneau(nel).wxr
		 txr = panneau(nel).txr
		 kse = panneau(nel).kse
		 phil = dabs(panneau(nel).phil)
		 q = panneau(nel).q
		 epais = panneau(nel).epais
		 epsa2 = panneau(nel).epsa2

		 if(hxr.gt.0.010) then
			call stifbend(nel,epsa,epsr,delta,hxr,dxr,wxr,txr,epsa2,is,sigstif,dsigstif)	
		 else													
			sigstif=0.											
!			read(iu_23(iboat))											
		 endif													

		 write(iu_13(iboat),'(1x,i3,t12,a,t44,f8.3)') nel,'/sx_stiff/',(sigstif/1.e+06)										
     
			
!	scantling optimization
!	======================

		 if(iopti.ge.1) then										

		    if(epsa2.ge.(0.00001)) then							
		       !backspace(iu_25(iboat))										
	           !read(iu_25(iboat)) epsa,epsr,delta,hya,dya,wya,tya,hxr,dxr,wxr,txr,phil,q,epais,epsa2,hya2,dya2,wya2,tya2
				wya2 = panneau(nel).wya2
				dya2 = panneau(nel).dya2
				tya2 = panneau(nel).tya2
		    endif

!	iacs requirements
!	=================
            ratio=0.d00
			do i=1,m1tabl(nel,is)
			   !read(iu_32(iboat)) ic,iy,invv,cmax
			   ic = panneau(nel).lcont4(nnsol(is),i,1)
			   iy = panneau(nel).lcont4(nnsol(is),i,2)
			   invv = panneau(nel).inv3(nnsol(is),i)
			   cmax = panneau(nel).cjmax9(nnsol(is),i)									
			   if(ic.eq.10) then
			      if (cmax.lt.100E06.or.cmax.gt.100E7) then
				    if (langue.eq.1) then
						write(*,'(2(a,i3),a)') ' Valeur max de la restriction',ic,' du panneau',nel,' non adequate'
						write(*,*) 'Veuillez corriger vos donnees - appuyer sur ENTER pour poursuivre'
					elseif (langue.eq.2) then
						write(*,'(2(a,i3),a)') ' Max Value of',ic,' constraint of the panel',nel,' is not relevant'
						write(*,*) 'Please correct your data - press ENTER to resume'
					endif
					read(*,*)
					stop
				  endif
			      cm=invv*cmax/coefk(indMateriau(nel))						
				  com='el.bord.flex'								
				  call bendyield(ciacs,dciacs,ntot(iboat))	
	           elseif(ic.eq.14) then
				  if (cmax.lt.0.or.cmax.gt.10) then
				    if (langue.eq.1) then
						write(*,'(2(a,i3),a)') ' Valeur max de la restriction',ic,' du panneau',nel,' non adequate'
						write(*,*) 'Veuillez corriger vos donnees - appuyer sur ENTER pour poursuivre'
					elseif (langue.eq.2) then
						write(*,'(2(a,i3),a)') ' Max Value of',ic,' constraint of the panel',nel,' is not relevant'
						write(*,*) 'Please correct your data - press ENTER to resume'
					endif
					read(*,*)
					stop
				  endif				
				  cm=invv*cmax											
	              com='fl.bord.comp'							
				  call compbuck(nel,e(indMateriau(nel)),sigy(indMateriau(nel)),coefk(indMateriau(nel)),epsa,epsr,delta,hxr,wya,q,epsa2,wya2,ciacs,dciacs,ntot(iboat))	
			   elseif(ic.eq.19) then
				  if (cmax.lt.100E06.or.cmax.gt.100E7) then
				    if (langue.eq.1) then
						write(*,'(2(a,i3),a)') ' Valeur max de la restriction',ic,' du panneau',nel,' non adequate'
						write(*,*) 'Veuillez corriger vos donnees - appuyer sur ENTER pour poursuivre'
				    elseif (langue.eq.2) then
						write(*,'(2(a,i3),a)') ' Max Value of',ic,' constraint of the panel',nel,' is not relevant'
						write(*,*) 'Please correct your data - press ENTER to resume'
					endif
					read(*,*)
					stop
				  endif							
				  cm=invv*cmax/coefk(indMateriau(nel))							
	              com='el.bord.cis'							
				  call shearyield(nel,is,ciacs,dciacs,ntot(iboat))
			   elseif(ic.eq.20) then
				  if (cmax.lt.0.or.cmax.gt.10) then
				    if (langue.eq.1) then
						write(*,'(2(a,i3),a)') ' Valeur max de la restriction',ic,' du panneau',nel,' non adequate'
						write(*,*) 'Veuillez corriger vos donnees - appuyer sur ENTER pour poursuivre'
					elseif (langue.eq.2) then
						write(*,'(2(a,i3),a)') ' Max Value of',ic,' constraint of the panel',nel,' is not relevant'
						write(*,*) 'Please correct your data - press ENTER to resume'
					endif
					read(*,*)
					stop
				  endif
			      cm=invv*cmax											
	              com='fl.bord.cis'							
				  call shearbuck(nel,e(indMateriau(nel)),sigy(indMateriau(nel)),q,epsa,epsr,delta,hxr,epsa2,is,ciacs,dciacs,ntot(iboat))		
			   elseif(ic.eq.37) then
			      if (cmax.lt.0.or.cmax.gt.10) then
				    if (langue.eq.1) then
						write(*,'(2(a,i3),a)') ' Valeur max de la restriction',ic,' du panneau',nel,' non adequate'
						write(*,*) 'Veuillez corriger vos donnees - appuyer sur ENTER pour poursuivre'
					elseif (langue.eq.2) then
						write(*,'(2(a,i3),a)') ' Max Value of',ic,' constraint of the panel',nel,' is not relevant'
						write(*,*) 'Please correct your data - press ENTER to resume'
					endif
					read(*,*)
					stop
				  endif						
				  cm=invv*cmax									
	              com='fl.raid.'					
				  call stifbuck(nel,e(indMateriau(nel)),sigy(indMateriau(nel)),coefk(indMateriau(nel)),epsa,epsr,delta,hxr,dxr,wxr,txr,wya,q,epsa2,wya2,ciacs,dciacs,ntot(iboat))			
			   elseif(ic.eq.38) then
			      
			      cmax = cmax*1000000.d00 !TODO corriger dans l'interface (?)
				  if (cmax.lt.100E06.or.cmax.gt.100E7) then
				    if (langue.eq.1) then
						write(*,'(2(a,i3),a)') ' Valeur max de la restriction',ic,' du panneau',nel,' non adequate'
						write(*,*) 'Veuillez corriger vos donnees - appuyer sur ENTER pour poursuivre'
					elseif (langue.eq.2) then
						write(*,'(2(a,i3),a)') ' Max Value of',ic,' constraint of the panel',nel,' is not relevant'
						write(*,*) 'Please correct your data - press ENTER to resume'
					endif
					read(*,*)
					stop
				  endif
				  cm=invv*cmax/coefk(indMateriau(nel))							
	              com='el.raid.'								
				  call stifyield(nel,coefk(indMateriau(nel)),ciacs,dciacs,ntot(iboat))


               elseif(ic.eq.39) then
				  cm=invv*cmax											
	              com='fl.raid.'										
				  call stifbuck_dcns(nel,e(indMateriau(nel)),sigy(indMateriau(nel)),epsa,epsr,delta,hxr,dxr,wxr,txr,ciacs,dciacs,ntot(iboat))				


			   endif													

               if (ic.eq.11) then
                    write(*,*) 'Restriction n°',ic,'selected and is not relevant with the Beam Theory calculation'
                    write(*,*) 'Please correct your data'
                    read(*,*)
                    stop
               endif

			   kcontr = kcontr+1
			   do k=1,ntot(iboat)
					cijopt(k,kcontr,iboat)=dciacs(k)
			   enddo
			   cjopt(kcontr,iboat) = ciacs
			   cjmopt(kcontr,iboat) = cm
			   vnom2(kcontr,iboat) = com
			   ic_rest(kcontr,iboat) = ic
			   ratio_rest(kcontr,iboat) = ratio

!			   write(iu_26(iboat),*) ciacs
!			   write(iu_26(iboat),*) cm
!			   write(iu_26(iboat),*) com
!			   write(iu_26(iboat),*) ic
!			   write(iu_26(iboat),*) ratio 									
!			   write(iu_26(iboat),*) (dciacs(j),j=1,ntot(iboat))					
			enddo												
		 endif		!if(iopti.ge.1) then											
		 
		 					
		 endif		!if(itype(nel).ne.5)											

      enddo
   enddo
                                                                         
deallocate (dciacs)
deallocate (thicknet)
deallocate (thickgro)

return
end
