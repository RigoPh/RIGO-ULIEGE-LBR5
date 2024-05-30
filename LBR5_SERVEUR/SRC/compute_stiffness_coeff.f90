subroutine compute_stiffness_coeff(nel)

use param_section

implicit double precision (a-h,o-z)

! ====================================================================================================
! calcul des coefficients de raideur du panneau (entreposes dans le vecteur const,const2 et const3)
! ====================================================================================================

call annuld(panneau(nel).const,74)

! raidissage principale
! ---------------------
call carac(e(indMateriau(nel)),eta(indMateriau(nel)),										&
           panneau(nel).epsa,panneau(nel).epsr,panneau(nel).delta,                          &
           panneau(nel).hya,panneau(nel).dya,panneau(nel).wya,panneau(nel).tya,             &
		   panneau(nel).hxr,panneau(nel).dxr,panneau(nel).wxr,panneau(nel).txr,             &
           panneau(nel).hxtr(:),panneau(nel).dxtr(:),panneau(nel).wxtr(:),panneau(nel).txtr(:), &
		   panneau(nel).const,sh,fam,panneau(nel).mt,panneau(nel).ksr,panneau(nel).ksa,panneau(nel).kst, &
           spec(indMateriau(nel)),poids9,dabs(panneau(nel).phil),panneau(nel).q,			&
           width,panneau(nel).delt,panneau(nel).delt2,panneau(nel).epais,itype(nel),		&
           isect(nel),panneau(nel).aire,panneau(nel).aiy,panneau(nel).aix,					&
		   panneau(nel).sredy,panneau(nel).sredx,panneau(nel).tork)

		
! derivee de const (opti)
! -----------------------
if (iopti.ge.1) then
  !call annuld(const2,54)
  !call annuld(const3,20)
  call caracd(e(indMateriau(nel)),       &
              panneau(nel).epsa,panneau(nel).epsr,panneau(nel).delta,                  &
              panneau(nel).hya,panneau(nel).dya,panneau(nel).wya,panneau(nel).tya,             &
              panneau(nel).hxr,panneau(nel).dxr,panneau(nel).wxr,panneau(nel).txr,             &
			  panneau(nel).const,sh,fam,panneau(nel).mt,                            &
			  panneau(nel).ksr,panneau(nel).ksa,panneau(nel).kst,dabs(panneau(nel).phil),      &
			  panneau(nel).q,width,panneau(nel).const2,panneau(nel).const3,                      &
              panneau(nel).epais,itype(nel),isect(nel),dpoids,         &
			  spec(indMateriau(nel)))                                                                   
endif

! raidissage secondaire
! ---------------------

if((panneau(nel).epsa2.ge.0.00001).or.(panneau(nel).epsr2.ge.0.00001)) then
  call carac2(e(indMateriau(nel)),						        &
              panneau(nel).epsa2,panneau(nel).epsr2,			&
			  panneau(nel).delta,panneau(nel).hya2,				&
			  panneau(nel).dya2,panneau(nel).wya2,				&
			  panneau(nel).tya2,panneau(nel).hxr2,				&
			  panneau(nel).dxr2,panneau(nel).wxr2,				&
			  panneau(nel).txr2,panneau(nel).const,sh,fam,panneau(nel).ksr2, &
			  panneau(nel).ksa2,spec(indMateriau(nel)),         &
              poids9,dabs(panneau(nel).phil),panneau(nel).q,width,    &
              panneau(nel).delt,panneau(nel).delt2,panneau(nel).const2)
endif

! impression de const2
! --------------------
if((impr.ge.1).and.(iopti.ge.1)) then
   if(itype(nel).ne.5) then								
      write(iu_11(iboat),15)
      write(iu_11(iboat),*)' bordage : x(1) = epaisseur bordage'
      write(iu_11(iboat),12)    (panneau(nel).const2(i,1),i=1,6)
      write(iu_11(iboat),*)' cadres  : x(i),i=2,5'
      write(iu_11(iboat),*)'    x(2) = hauteur âme'
      write(iu_11(iboat),12)    (panneau(nel).const2(i,2),i=1,6)
      write(iu_11(iboat),*)'    x(3) = epaisseur âme'
      write(iu_11(iboat),12)    (panneau(nel).const2(i,3),i=1,6)
      write(iu_11(iboat),*)'    x(4) = largeur semelle'
      write(iu_11(iboat),12)    (panneau(nel).const2(i,4),i=1,6)
      write(iu_11(iboat),*)'    x(5) = larg. effective'
      write(iu_11(iboat),12)    (panneau(nel).const2(i,5),i=1,6)
      write(iu_11(iboat),*)' raidisseurs: x(i),i=6,9'
      write(iu_11(iboat),*)'    x(6) = hauteur âme'
      write(iu_11(iboat),12)    (panneau(nel).const2(i,6),i=1,6)
      write(iu_11(iboat),*)'    x(7) = epaisseur âme'
      write(iu_11(iboat),12)    (panneau(nel).const2(i,7),i=1,6)
      write(iu_11(iboat),*)'    x(8) = largeur semelle'
      write(iu_11(iboat),12)    (panneau(nel).const2(i,8),i=1,6)
      write(iu_11(iboat),*)'    x(9) = larg. effective'
      write(iu_11(iboat),12)    (panneau(nel).const2(i,9),i=1,6)
      write(iu_11(iboat),*)' '
   else										!épontille	
      if(isect(nel).eq.3) then							
         write(iu_11(iboat),15)						
         write(iu_11(iboat),*)' epontille :x(1) = hauteur de l''âme '		
         write(iu_11(iboat),12)    (panneau(nel).const2(i,1),i=1,5)						
         write(iu_11(iboat),*)'    x(2) = epaisseur âme'					
         write(iu_11(iboat),12)    (panneau(nel).const2(i,2),i=1,5)						
         write(iu_11(iboat),*)'    x(3) = largeur semelle'				
         write(iu_11(iboat),12)    (panneau(nel).const2(i,3),i=1,5)						
         write(iu_11(iboat),*)'    x(5) = larg. effective'				
         write(iu_11(iboat),12)    (panneau(nel).const2(i,5),i=1,5)						
      else			
         write(iu_11(iboat),15)											
         write(iu_11(iboat),*)' epontille :x(1) = diamètre ou côté extérieur '	
         write(iu_11(iboat),12)    (panneau(nel).const2(i,1),i=1,5)		
         write(iu_11(iboat),*)'    x(2) = epaisseur de la paroi mince'			
         write(iu_11(iboat),12)    (panneau(nel).const2(i,4),i=1,5)						
         write(iu_11(iboat),*)'    x(5) = larg. effective'				
         write(iu_11(iboat),12)    (panneau(nel).const2(i,5),i=1,5)						
      endif												
   endif														
   
   if(panneau(nel).mt.ge.1) then
      write(iu_11(iboat),*)' traverses:  x(1) = epaisseur bordage'
      do j=1,panneau(nel).mt
         write(iu_11(iboat),*)'trav nø',j,'  : ',(panneau(nel).const3(i,j),i=1,2)
      enddo
   endif
endif

return

900 write(* ,*)'erreur de lecture : "end of file"'
    write(iu_11(iboat),*)'erreur de lecture : "end of file"'
    write(iu_14(iboat),*)'erreur de lecture : "end of file"'
    write(*,*) 'stop'
    read(*,*)
    stop

! 6.  les formats.
!     ============
  12  format(6(1x,e11.4))
  15  format('derivées des coef. de raideur par rapport aux var. de ','conception :'/65(1h-))

end
