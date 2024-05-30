subroutine read_geom_restrictions(nel)

use param_section

implicit double precision (a-h,o-z)

! ====================================================================================================
! lecture des m2 restrictions géométriques (ibm2 = nø du set de restrictions)
! ====================================================================================================

!     m2 =  nbre de restr. géom. du panneau nel

      read (iu_10(iboat),*,end=900) title                          ! titre
      read (iu_10(iboat),*,end=900) panneau(nel).m2
      if(panneau(nel).m2.eq.99) then
         read (iu_10(iboat),*,end=900) panneau(nel).ibm2    ! avec ibm2  = nø du set de restrictions
         read (iu_10(iboat),*,end=900) panneau(nel).isema,panneau(nel).isemr
!           avec isema > 0  l'épaisseur des semelles des transversaux est
!                           modifiée automatiquement
!                           isema ou isemr =1 : largeur semelle < 16 *
!                                 epaiss. semelle
!                           isema ou isemr =2 : largeur semelle < 32 *
!                                 epaiss. semelle
!           avec isemr > 0  l'épaisseur des semelles des raidisseurs  est
!                           modifiée automatiquement
         if(panneau(nel).ibm2.eq.1) then
!           set de restrictions de hughes (pour profil t)
            panneau(nel).m2=11
            panneau(nel).lm2(1)= 109
            panneau(nel).lm2(2)= 110
            panneau(nel).lm2(3)= 111
            panneau(nel).lm2(4)= 112
            panneau(nel).lm2(5)= 209
            panneau(nel).lm2(6)= 210
            panneau(nel).lm2(7)= 211
            panneau(nel).lm2(8)= 212
            panneau(nel).lm2(9) =301
            panneau(nel).lm2(10)=302
            panneau(nel).lm2(11)=303
         else if(panneau(nel).ibm2.eq.2) then
!           set de restrictions de hughes + set complémentaire (a)
            panneau(nel).m2=15
            panneau(nel).lm2(1)= 109
            panneau(nel).lm2(2)= 110
            panneau(nel).lm2(3)= 111
            panneau(nel).lm2(4)= 112
            panneau(nel).lm2(5)= 209
            panneau(nel).lm2(6)= 210
            panneau(nel).lm2(7)= 211
            panneau(nel).lm2(8)= 212
            panneau(nel).lm2(9) =301
            panneau(nel).lm2(10)=302
            panneau(nel).lm2(11)=303
            panneau(nel).lm2(12)=104
            panneau(nel).lm2(13)=105
            panneau(nel).lm2(14)=204
            panneau(nel).lm2(15)=205
         else if(panneau(nel).ibm2.eq.3) then
!           set de restrictions de hughes (pour cornières)
            panneau(nel).m2=11
            panneau(nel).lm2(1)= 115
            panneau(nel).lm2(2)= 116
            panneau(nel).lm2(3)= 111
            panneau(nel).lm2(4)= 112
            panneau(nel).lm2(5)= 209
            panneau(nel).lm2(6)= 215
            panneau(nel).lm2(7)= 216
            panneau(nel).lm2(8)= 212
            panneau(nel).lm2(9) =301
            panneau(nel).lm2(10)=302
            panneau(nel).lm2(11)=303
         else if(panneau(nel).ibm2.eq.4) then
!           set de restrictions de rahman
            panneau(nel).m2=6
            panneau(nel).lm2(1)=101
            panneau(nel).lm2(2)=102
            panneau(nel).lm2(3)=103
            panneau(nel).lm2(4)=201
            panneau(nel).lm2(5)=202
            panneau(nel).lm2(6)=203
         else if(panneau(nel).ibm2.eq.5) then
!           set de restrictions de rahman + set complémentaire (a)
            panneau(nel).m2=10
            panneau(nel).lm2(1)=101
            panneau(nel).lm2(2)=102
            panneau(nel).lm2(3)=103
            panneau(nel).lm2(4)=201
            panneau(nel).lm2(5)=202
            panneau(nel).lm2(6)=203
            panneau(nel).lm2(7)=104
            panneau(nel).lm2(8)=105
            panneau(nel).lm2(9)=204
            panneau(nel).lm2(10)=205
         else if(panneau(nel).ibm2.eq.6) then
            panneau(nel).m2=6
            panneau(nel).lm2(1)=101
            panneau(nel).lm2(2)=102
            panneau(nel).lm2(3)=114
            panneau(nel).lm2(4)=201
            panneau(nel).lm2(5)=202
            panneau(nel).lm2(6)=203
            panneau(nel).isema=6
            panneau(nel).isemr=6
         else if(panneau(nel).ibm2.eq.7) then
            panneau(nel).m2=6
            panneau(nel).lm2(1)=101
            panneau(nel).lm2(2)=102
            panneau(nel).lm2(3)=114
            panneau(nel).lm2(4)=201
            panneau(nel).lm2(5)=202
            panneau(nel).lm2(6)=203
            panneau(nel).isema=7
            panneau(nel).isemr=7
         else
            iff=1               !  message d'erreur
            write(*,*)' set des restr. géométriques inconnu! =',panneau(nel).ibm2
            write(iu_31(iboat),*) ' set des restr. géométriques inconnu! =',panneau(nel).ibm2
            write(iu_14(iboat),*) ' set des restr. géométriques inconnu! =',panneau(nel).ibm2
            return
         endif ! if(ibm2.eq.1)
!        if(iopti.ge.3)write(*,*)'nbre de restr. géométriques  = ',m2
      else if((panneau(nel).m2.lt.0).or.(panneau(nel).m2.gt.20)) then
         iff=1                              !  message d'erreur
         write(*,*)  ' nbre de restr. géométriques incorrecte = ',panneau(nel).m2
         write(iu_31(iboat),*) ' nbre de restr. géométriques incorrecte = ',panneau(nel).m2
         write(iu_14(iboat) ,*) ' nbre de restr. géométriques incorrecte = ',panneau(nel).m2		
  	     return

      else if(panneau(nel).m2.ne.0) then
!        if(iopti.ge.3)write(*,*)'nbre de restr. géométriques  = ',m2
         read (iu_10(iboat),*,end=900) (panneau(nel).lm2(i),i=1,panneau(nel).m2)  !  liste des m2 restrictions
         read (iu_10(iboat),*,end=900) panneau(nel).isema,panneau(nel).isemr      !  indice relatif aux semelles
!        if(iopti.ge.3) write(*,*)' set  = ',(lm2(i),i=1,m2)
      else if(panneau(nel).m2.eq.0) then
        panneau(nel).isema=0
        panneau(nel).isemr=0                              !
      endif                                  ! if(m2.eq.99)


      m2cont(nel)=panneau(nel).m2

      if(iopti.ge.1) then
         if(panneau(nel).m2.gt.0) then     ! si m2>0
            if(itera.eq.0) then                 ! si itera=1

               if(panneau(nel).isema.ge.1) then
                  write(iu_31(iboat),203) 'transversaux : isema= ',panneau(nel).isema
               else
                  write(iu_31(iboat),207) 'transversaux'
               endif

               if(panneau(nel).isemr.ge.1) then
                  write(iu_31(iboat),203) 'raidisseurs  : isemr= ',panneau(nel).isemr
               else
                  write(iu_31(iboat),207) 'raidisseurs'
               endif

               if((panneau(nel).isemr.ge.1).or.(panneau(nel).isema.ge.1)) then
                  write(iu_31(iboat),204)
               endif                           

            else                              ! si itera>1

               if((panneau(nel).isema.ge.1).or.(panneau(nel).isemr.ge.1)) then
                  if(nel.eq.1) write(iu_31(iboat),204)
               endif
         endif
      endif

      if(panneau(nel).isema.ge.1) then
         do i=1,nvar(nel,iboat)
	        if(panneau(nel).isema.gt.5) then
               if((itype(nel).ne.5).and.(nxit(i,nel,iboat).eq.3))then
               !on vérifie si l'on a bien sélectionné comme variable de conception dya
	              nn=i+ntot(iboat)                               
                  if(panneau(nel).isema.eq.6) then
                     call dcns_geom_n50_51(panneau(nel).hya,panneau(nel).dya,panneau(nel).wya,panneau(nel).tya,ximin(nn,iboat),   &
				     ximax(nn,iboat))  ! cadres		
                     xicou(nn,iboat)=panneau(nel).dya
			      endif

               elseif ((itype(nel).ne.5).and.(nxit(i,nel,iboat).eq.4))then 
			   
			   !on vérifie si l'on a bien sélectionné comme variable de conception wya
	              nn=i+ntot(iboat)                               
                  if(panneau(nel).isema.eq.7) then
                     call dcns_geom_n52_53(panneau(nel).hya,panneau(nel).dya,panneau(nel).wya,panneau(nel).tya,ximin(nn,iboat),   &
				     ximax(nn,iboat))  ! cadres		
                     xicou(nn,iboat)=panneau(nel).wya
	              endif
               endif

            else

               if ((itype(nel).ne.5).and.(nxit(i,nel,iboat).eq.4))then 
			   !on vérifie si l'on a bien sélectionné comme variable de conception wya

	              nn=i+ntot(iboat)                               
                  call semel(panneau(nel).hya,panneau(nel).dya,panneau(nel).wya,panneau(nel).tya,ximin(nn,iboat),ximax(nn,iboat),    &         
                         panneau(nel).inda,1,iprint,nel,panneau(nel).isema,isect(nel))    ! cadres
                  xicou(nn,iboat)=panneau(nel).wya

               elseif ((isect(nel).eq.3).and.(nxit(i,nel,iboat).eq.3)) then
                  nn=i+ntot(iboat)
                  call semel(panneau(nel).hya,panneau(nel).dya,panneau(nel).wya,panneau(nel).tya,ximin(nn,iboat),ximax(nn,iboat),    &            
                         panneau(nel).inda,3,iprint,nel,panneau(nel).isema,isect(nel))   ! épontille	

                  xicou(nn,iboat)=panneau(nel).wya

               endif
            endif
         enddo
	  !write(3987,*)'inda      =',inda(nel)
      endif

      if(panneau(nel).isemr.ge.1) then
         do i=1,nvar(nel,iboat)
	        if(panneau(nel).isemr.gt.5) then
               if((itype(nel).ne.5).and.(nxit(i,nel,iboat).eq.7))then

               !on vérifie si l'on a bien sélectionné comme variable de conception dxr

	              nn=i+ntot(iboat)                               
                  if(panneau(nel).isemr.eq.6) then
                     call dcns_geom_n50_51(panneau(nel).hxr,panneau(nel).dxr,panneau(nel).wxr,panneau(nel).txr,ximin(nn,iboat),   &
				     ximax(nn,iboat))  ! cadres
                     xicou(nn,iboat)=panneau(nel).dxr
			      endif

               elseif ((itype(nel).ne.5).and.(nxit(i,nel,iboat).eq.8))then 
			   
			   !on vérifie si l'on a bien sélectionné comme variable de conception wxr

	              nn=i+ntot(iboat)                               
                  if(panneau(nel).isemr.eq.7) then
                     call dcns_geom_n52_53(panneau(nel).hxr,panneau(nel).dxr,panneau(nel).wxr,panneau(nel).txr,ximin(nn,iboat),   &
				     ximax(nn,iboat))  ! cadres		
                     xicou(nn,iboat)=panneau(nel).wxr
	              endif
               endif

            else

               if(nxit(i,nel,iboat).eq.8) then 
			   !on vérifie si l'on a bien sélectionné comme variable de conception wxr

	              nn=i+ntot(iboat)
                  call semel(panneau(nel).hxr,panneau(nel).dxr,panneau(nel).wxr,panneau(nel).txr,ximin(nn,iboat),ximax(nn,iboat),    &         
                         panneau(nel).indr,2,iprint,nel,panneau(nel).isemr,isect(nel))    ! cadres		

                  xicou(nn,iboat)=panneau(nel).wxr

               endif
            endif
         enddo
	  !write(3987,*)'indr      =',indr(nel)
      endif


      if(itera.eq.0) write(iu_31(iboat),208) panneau(nel).m2,(panneau(nel).lm2(i),i=1,panneau(nel).m2) 

         do ic=1,panneau(nel).m2
            it=ic+m2tot(iboat)
            call geom(it,nxit(:,nel,iboat),nvar(nel,iboat),itera,panneau(nel).lm2(ic), &
       	        	  coefk(indMateriau(nel)),length,nel)						
         enddo
         tfa(nel)=panneau(nel).tya
         tfr(nel)=panneau(nel).txr
      endif
  
      ntot(iboat) =ntot(iboat) +nvar(nel,iboat)
      m2tot(iboat)=m2tot(iboat)+panneau(nel).m2


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
 203  format('avec ajustement des dimensions des semelles des ',a,i2)
 204  format(/'ajustement des dimensions des semelles'/38(1h-)/      &
      'panneau',5x,'anc. dimensions',15x,'nouvelles dimensions',6x,  &
      'type','  ind')
 207  format('les dimensions des semelles des ',a12,' sont fixes.')
 208  format(/'nbre de restr. géom.= ',i2,'  set = ',20i4)
end
