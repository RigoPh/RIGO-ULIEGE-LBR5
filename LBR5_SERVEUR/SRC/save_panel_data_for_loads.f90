subroutine save_panel_data_for_loads(nel)

use param_section

implicit real *8 (a-h,o-z)

!     sauvetage [ write(iu_22(iboat)) ] de cha1, cha2 et cha3
!     --------------------------------------------
if((ipoids.eq.1).or.((panneau(nel).icha).eq.1)) then
do is=1,nsol
   do i=1,npt(is,nel)
      cha(i,1,is,nel)=cha(i,1,is,nel)/qphil
      do k=1,9                             
         dcha(i,is,k,nel)=dcha(i,is,k,nel)/qphil   
      enddo
   enddo                           !
enddo

!if ((itera.eq.0)) write(iu_22(iboat))                             &
!              (npt(i,nel),i=1,nsol),                           &
!              (((cha(i,j,k,nel),i=1,100),j=1,3),k=1,nsol),     &
!              (((dcha(i,j,k,nel),i=1,100),j=1,nsol),k=1,9)
endif

! 4.7 sauvetage, des pressions max pour la subr  hughes ! updated: mai 1996
!                                    et la subr. stiff
!     -------------------------------------------------
!    la pression max est calculée en y= 0, 1/3, 1/2, 2/3 et 1

ip=1
if(panneau(nel).kse.eq.2) ip=-1
if(panneau(nel).kse.eq.0) ip=0
!call annuld(chamax,5*nsol)
!call annuld(dchamax,5*nsol*9)
if (.not.ASSOCIATED(panneau(nel)%chamax)) then
	allocate (panneau(nel)%chamax(5,nsol))
	allocate (panneau(nel)%dchamax(5,nsol,9))
	panneau(nel).chamax(:,:) = 0.d00
	panneau(nel).dchamax(:,:,:) = 0.d00
endif

do is=1,nsol
   
   c11=dcos((panneau(nel).ang)  *pi1)/9.81d03       ! pi1=pi/180.
   c22=dcos((panneau(nel).ang)  *pi1)/9.81d03
   c33=dcos((panneau(nel).ang)  *pi1)/9.81d03
   c4 =dcos((panneau(nel).ang)  *pi1)/9.81d03
   c5 =dcos((panneau(nel).ang)  *pi1)/9.81d03
   
   xif1=xf(is,nel) + xi(is,nel) + cha(25,3,is,nel) + cha(25,2,is,nel)
   xif2=xf(is,nel) + xi(is,nel) + cha(26,3,is,nel) + cha(26,2,is,nel)
   
   if (xif1 .ge. xif2) then
   oth=25
   else
   oth=26
   endif
   
   xif=xf(is,nel) - xi(is,nel) + cha(oth,3,is,nel) - cha(oth,2,is,nel)
   
   panneau(nel).chamax(1,is)=(xi(is,nel)+cha(oth,2,is,nel))          *dble(ip)+c11*cha(oth,1,is,nel)
   panneau(nel).chamax(2,is)=(xi(is,nel)+cha(oth,2,is,nel)+xif/3.)   *dble(ip)+c22*cha(oth,1,is,nel)
   panneau(nel).chamax(3,is)=(xi(is,nel)+cha(oth,2,is,nel)+xif/2.)   *dble(ip)+c33*cha(oth,1,is,nel)
   panneau(nel).chamax(4,is)=(xi(is,nel)+cha(oth,2,is,nel)+2.*xif/3.)*dble(ip)+c4 *cha(oth,1,is,nel)
   panneau(nel).chamax(5,is)=(xf(is,nel)+cha(oth,3,is,nel))          *dble(ip)+c5 *cha(oth,1,is,nel)

!         calcul des dérivées des chamax  
	 do k=1,9
	    panneau(nel).dchamax(1,is,k)=c11*dcha(oth,is,k,nel)
	    panneau(nel).dchamax(2,is,k)=c22*dcha(oth,is,k,nel)
	    panneau(nel).dchamax(3,is,k)=c33*dcha(oth,is,k,nel)
     	panneau(nel).dchamax(4,is,k)=c4 *dcha(oth,is,k,nel)
	    panneau(nel).dchamax(5,is,k)=c5 *dcha(oth,is,k,nel)
	 enddo

  
   if((ivaria(nel).eq.0).and.(panneau(nel).types.eq.'coque')) then
      write(*  ,691)
      write(iu_31(iboat),691)
   	 write(iu_14(iboat) ,691)	
   endif
   
enddo

if(impr2.ge.-1) then					
   write(iu_11(iboat),874)panneau(nel).mt,panneau(nel).ksr,panneau(nel).ksa,panneau(nel).kst,panneau(nel).kse,ipa(nel),ivaria(nel)	
endif

! 6.  les formats.
!     ============

691  format(/' approximation (coque avec ivaria=0) !!!!!'/,      &
     ' les pressions pour la subr hughes sont calculées en ',    &
     'considérant une variation linéaire selon oy.'/             &
     ' l''erreur peut être importante si phil > 60 degré'/)


874  format(' mt',i2,' ; ksr',i2,' ; ksa',i2,' ; kst',i2,' ; kse',i2,  &
     ' ; ipa',i2,' ; ivaria',i2//)

end
