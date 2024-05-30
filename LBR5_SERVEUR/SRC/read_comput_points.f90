subroutine read_comput_points(nel)

use param_section

implicit double precision (a-h,o-z)

! ====================================================================================================
! restrictions structurelles: lecture des ipt (points de calcul)
! ====================================================================================================
read (iu_10(iboat),*,end=900) title        ! titre sur restrictions structurelles:
read (iu_10(iboat),*,end=900) panneau(nel).im1  ! im1 (=0 => pas de resti struc; =1 => restri struct)

!   il faut tjrs avoir ypt(1)=0. et ypt(ipt)=phil (cfr subr. mdr2)
!   donc iptmin = 2   et iptmax = 4
m1_read=1

if(panneau(nel).im1.eq.0) then  !im1=0 : pas de restrictions structurelles
 !!!  panneau(nel).ipts=0
   panneau(nel).ipts=2
   ypts(1,nel)=0*dabs(panneau(nel).phil)     !spécifier les points de calcul même sans restrictions,..
   ypts(2,nel)=1*dabs(panneau(nel).phil)     !on choisit le noeud de départ et d'arrivée
!   if (.not.associated(panneau(nel)%ipts2)) then
!	   allocate (panneau(nel)%ipts2(iptmax))
!	   panneau(nel).ipts2(:) = 0
!   endif
   panneau(nel).ipts2(1)=1               !pour éviter d'avoir des ipts2(i,nel)=0
   panneau(nel).ipts2(2)=31              !pour éviter d'avoir des ipts2(i,nel)=0  

   ypt9(1,nel) = 1  !TODO sans doute à supprimer ce vecteur, je ne vois pas pq on a autant de vecteur apparemment redondants !
   ypt9(2,nel) = 31

   do iit=1,panneau(nel).mt
      panneau(nel).ipts3(iit)=iit        !mettre n'importe quelle valeur pour les traverses car  
   enddo                        !normalement elle ne sera pas utilisé dans la suite du programme.  
     
   !goto 219                    ! pas de restr struc. pour ce panneau: goto end of this section.
   m1_read=0
else if(panneau(nel).im1.eq.1) then
   read (iu_10(iboat),*,end=900) panneau(nel).ipts    ! ipt(nel)=nbre de pts pour le calcul des restrictions
   if(iopti.ne.0) then
      if((panneau(nel).ipts.gt.4).or.(panneau(nel).ipts.lt.2)) then

         write(*,*)'**** panneau ',nel
         write(*,*)           ' ipt=',panneau(nel).ipts,' est incorrecte, il faut 2=<ipt=<4)'
         write(iu_31(iboat),*)' ipt=',panneau(nel).ipts,' est incorrecte, il faut 2=<ipt=<4)'
         write(iu_14(iboat),*)' ipt=',panneau(nel).ipts,' est incorrecte, il faut 2=<ipt=<4)'					
         write(*,*) 'error'
		 read(*,*)
         iff=1
      endif
   endif

   ! ypt = en valeur relative c.à.d.  (0<ypt<1) pour les donnees
   !       mais en valeur relle pour le programme  0<ypt<q*phil
   read (iu_10(iboat),*,end=900) (ypts(i,nel),i=1,panneau(nel).ipts)  ! ypt (ordonnées relatives des pts)
   ypt9(1:panneau(nel).ipts,nel)=ypts(1:panneau(nel).ipts,nel)  !dad
   if(iopti.ge.1) then
      if((ypts(1,nel).ge.0.01).or.(ypts(panneau(nel).ipts,nel).le.0.99))then
         write(iu_31(iboat),431) ! message d'erreur
         write(*           ,431)
         write(iu_14(iboat),431)									
      endif
      ypts(1,        nel)=0.0
      ypts(panneau(nel).ipts,nel)=1.0
      ! calcul des coordonnées réelles (= angle en degrés)
      do i =1,panneau(nel).ipts
         ypts(i,nel)=ypts(i,nel)*dabs(panneau(nel).phil)  ! (ordonnées ang. réelles des pts)
      enddo
   endif
   

!!! CE qui suit (jusqu'au endif) ne me semble plus possible avec l'interface !!!
!!! (il n'y a pas de "restrictions d'égalité" de containtes structurelles)   !!!


else if ((panneau(nel).im1.le.-nel).or.(panneau(nel).im1.gt.1)) then
   write(*,*)    ' *** panneau',nel
   write(*,*)  ' fausse valeur de im1 =',panneau(nel).im1,'=< ',-nel
   write(*,*)  ' ou im1 > 1'
   write(*,*)  ' wrong value for im1 =',panneau(nel).im1,'=< ',-nel
   write(*,*)  ' or im1 > 1'
   write(iu_31(iboat),*)' wrong value for im1 =',panneau(nel).im1,'=< ',-nel
   write(iu_31(iboat),*)' or im1 > 1'
    
   write(iu_14(iboat),*)    ' *** panneau',nel											
   write(iu_14(iboat),*)  ' fausse valeur de im1 =',panneau(nel).im1,'=< ',-nel						
   write(iu_14(iboat),*)  ' ou im1 > 1'												
 
   iff=1

   write(*,*) 'error'
   read(*,*)
   return

else    ! im1<0 cad mêmes restrictions que celles du panneau im1
   panneau(nel).im1=-panneau(nel).im1
   panneau(nel).ipts=panneau(panneau(nel).im1).ipts

   do i=1,panneau(nel).ipts
      ypts (i,nel)=ypts (i,panneau(nel).im1)*panneau(nel).phil/panneau(panneau(nel).im1).phil
      panneau(nel).ipts2(i)=panneau(panneau(nel).im1).ipts2(i)
      panneau(nel).ipts3(i)=panneau(panneau(nel).im1).ipts3(i)
   enddo

   do is=1,nsol                           ! is = n° d'un cas de charge selectionné
      ij=nnsol(is)                  ! ij = n° de ce cas de charge comme defini dans les donnees
      panneau(nel).m1(is)    =m1tabl(panneau(nel).im1,is)
      m1tabl(nel,is)=panneau(nel).m1(is)
      panneau(nel).m1cont=panneau(nel).m1cont+panneau(nel).m1(is)  ! nbre de restriction pour tout les cas de charges (pour le panneau nel)
      m1tot(iboat) =m1tot(iboat) +panneau(nel).m1(is)            ! nbre de restriction pour tout les cas de charges (pour la structure)
      if(itera.eq.0) then
         do i=1,panneau(nel).m1(is)
            panneau(nel).lcont4(ij,i,1)=panneau(panneau(nel).im1).lcont4(ij,i,1)
            panneau(nel).lcont4(ij,i,2)=panneau(panneau(nel).im1).lcont4(ij,i,2)
            panneau(nel).cjmax9(ij,i)  =panneau(panneau(nel).im1).cjmax9(ij,i)
            panneau(nel).inv3  (ij,i)  =panneau(panneau(nel).im1).inv3  (ij,i)
         enddo
      ! vérification de la compatibilité entre variable xi et restriction 4 (dmin)
         if((panneau(nel)%lcont4(ij,i,1).eq.4).and.(nxit(1,nel,iboat).ne.1)) then
		    write(*,*) 'Error panel:',nel
            write(*  ,220)
            write(*  ,220)
            write(*  ,220)
            write(*  ,220)
            write(iu_31(iboat),220)
            write(iu_11(iboat),220)
	        write(iu_14(iboat),220)					
         endif
      ! vérification du choix des points de calcul des sensibilités vis à vis de traverses
         if(panneau(nel).mt.ge.1) then
            if((panneau(nel)%lcont4(ij,i,1).ge.40).and.(panneau(nel)%lcont4(ij,i,1).le.45))     then
               do it=1,panneau(nel).mt
                  ax=dabs(panneau(nel).abtr(it)/ypts(panneau(nel)%lcont4(ij,i,2),nel) )
                  if(ax.ge.(0.99)) then                                !  ipt3= nø de la trav. concernee
                     if(ax.le.(1.01)) panneau(nel).ipts3(panneau(nel)%lcont4(ij,i,2))=it
                  endif
               enddo
            endif
         endif

      endif
   enddo
   !goto 219
   m1_read=0
endif   ! if(im1.eq.0) then


!if((iopti.ge.1).and.(itera.eq.0)) then  !dad
if(iopti.ge.1) then
!   if (.not.associated(panneau(nel)%ipts2)) then
!	   allocate (panneau(nel)%ipts2(iptmax))
!	   panneau(nel).ipts2(:) = 0
!	endif
   do i=1,panneau(nel).ipts                                         ! pt. de calcul des restrictions
      panneau(nel).ipts2(i)=idnint(30.*ypts(i,nel)/dabs(panneau(nel).phil)) + 1   ! c.à.d. un pt de 1 à 31
   enddo
endif

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
 431  format(/' attention: la position des pts de calculs',  &
             ' des sensibilites est errone.'/5x,             &
             ' il faut que le premier et le dernier pts',    &
             ' correspondent aux extremites.'/5x,            &
             ' !!!  la correction est automatique !!!'/)

end
