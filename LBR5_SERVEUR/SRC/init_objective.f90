subroutine init_objective()




use param_section
!use param_cout
!use param_opti_local




implicit double precision (a-h,o-z)
character*12 com
character*80 buffer
double precision, allocatable :: dprice(:)
double precision, allocatable :: dw(:)

allocate (dprice(9*nmax))
allocate (dw(ntot(iboat)))

!	restriction sur poids		
!	=====================		

read(iu_10(iboat),4) buffer				
read(iu_10(iboat),*) iweight,wmax											
																		
if((iopti.ne.0).and.(iweight.ne.0)) then
   weight=0.
   call annuld(dw,ntot(iboat))
   ncont=0
!   do nel=1,neto
!     nbrxi=nvar(nel,iboat)
!     call objpd1(nel,ntot(iboat),ncont,nxit(1,nel,iboat),nbrxi,weight,dw,spec(nel),itype(nel),isect(nel))
!     ncont=ncont+nbrxi					
!   enddo

   call objpd1(weight,dw,1,0) ! 1 -> calcul des dérivées
   
   write(iu_11(iboat),'(//a/1x,22(1h*))')    ' restriction sur poids'
   write(iu_31(iboat),'(//a/1x,22(1h*))')    ' restriction sur poids'
   write(iu_11(iboat),'(a,t17,e14.7,t33,a)') ' poids actuel  = ' ,weight,'n'
   write(iu_31(iboat),'(a,t17,e14.7,t33,a)') ' poids actuel  = ' ,weight,'n'
   write(iu_11(iboat),'(a,t17,e14.7,t33,a)') ' poids maximum = ' ,wmax  ,'n'
   write(iu_31(iboat),'(a,t17,e14.7,t33,a)') ' poids maximum = ' ,wmax  ,'n'

   com='poids < wmax'
   c  =weight
   cm =wmax

   kcontr = kcontr+1
   do i=1,ntot(iboat)
		cijopt(i,kcontr,iboat)=dw(i)
   enddo
   cjopt(kcontr,iboat) = c
   cjmopt(kcontr,iboat) = cm
   vnom2(kcontr,iboat) = com

!   write(iu_26(iboat),*) c
!   write(iu_26(iboat),*) cm
!   write(iu_26(iboat),*) com
!   write(iu_26(iboat),*) (dw(i),i=1,ntot(iboat))				

   ! +1 au compteur des restrictions struc
   
   img=img+1
!   rewind iu_25(iboat)
endif



!	restriction sur cout		
!	=====================		
													
read(iu_10(iboat),4) atext												
read(iu_10(iboat),*) iprice,pricemax,icost						
																	
if((iopti.ne.0).and.(iprice.ne.0)) then			
   price=0.
   call annuld(dprice,ntot(iboat))
   initcout=icout
   if(icost.eq.1) then
      icout=icost
	  ncont=0
!	  do nel=1,neto
!	     nbrxi=nvar(nel,iboat)
!		 call objct1(nel,ntot(iboat),ncont,xiopt(:,iboat),nxit(1,nel,iboat),nbrxi,price,dprice,spec(nel),itype(nel),isect(nel))
!		 ncont=ncont+nbrxi
!	  enddo

   call objct1(price,dprice,1,0) ! 1 -> calcul des dérivées

   endif															

   if(icost.gt.1) then								
	  icout=icost											
	  call sensibcout(price,dprice,1,1)			
   endif
   
   icout=initcout
   write(iu_11(iboat),'(//a/1x,21(1h*))')    ' restriction sur cout'
   write(iu_31(iboat),'(//a/1x,21(1h*))')    ' restriction sur cout'
   write(iu_11(iboat),'(a,t17,e14.7,t33,a)')	' cout actuel = ',price,'euro ($ or ...)'
   write(iu_31(iboat),'(a,t17,e14.7,t33,a)')	' cout actuel = ',price,'euro ($ or ...)'
   write(iu_11(iboat),'(a,t17,e14.7,t33,a)')	' cout maximum = ',pricemax,'euro ($ or ...)'
   write(iu_31(iboat),'(a,t17,e14.7,t33,a)')	' cout maximum = ',pricemax,'euro ($ or ...)'
   
   com='price < pricemax'
   c  =price
   cm =pricemax
   
   kcontr = kcontr+1
   do i=1,ntot(iboat)
		cijopt(i,kcontr,iboat)=dprice(i)
   enddo
   cjopt(kcontr,iboat) = c
   cjmopt(kcontr,iboat) = cm
   vnom2(kcontr,iboat) = com

!   write(iu_26(iboat),*) c
!   write(iu_26(iboat),*) cm
!   write(iu_26(iboat),*) com
!   write(iu_26(iboat),*) (dprice(i),i=1,ntot(iboat))	
   
   ! +1 au compteur des restrictions struc
   img=img+1
   
!   rewind iu_25(iboat)														
endif

deallocate (dw)
deallocate (dprice)

4 format(a80)

return
end
