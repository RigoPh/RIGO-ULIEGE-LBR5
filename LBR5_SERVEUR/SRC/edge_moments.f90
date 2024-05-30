subroutine edge_moments()

!use param_multi
!use param_init
use param_section




implicit double precision (a-h,o-z)
character*80 buffer

! =============================================================================
! 11.0 moments d'extremites
! =============================================================================


read(iu_10(iboat),4)       buffer
read(iu_10(iboat),*)       imom

if (imom.ne.0) then
   
   write(iu_11(iboat),5241)   buffer
   read (iu_10(iboat),*)      yred
   write(iu_11(iboat),2313)   yred
   
   ico=0
!  if(itera.eq.0) then				 dad
   if((dabs(1.0-yred).gt.0.001).or.(jlph.eq.1)) then
      if(langue.eq.1)then
         write(*,100)' ... application d''un facteur de reduction'
         write(*,100)'     aux moments de flexion d''extremites.'
         write(*,101)'     facteur de reduction: yred =',yred
      else
         write(*,100)' ... applying a reduction factor at'
         write(*,100)'     the extremity bending moments.'
         write(*,101)'     reduction factor: yred =',yred
      endif
   endif
!  endif
   
!  icc=0 cas de charge pas retenu
!  icc=1 cas de charge retenu
   
   is=0
   do i=1,nsolm
	  read (iu_10(iboat),*) bm11(i),sf11(i),bm31(i),sf31(i)
   enddo   

!EffortT=1
!methodEffortT=3
   do i=1,nsolm
     !read (iu_10(iboat),*) TEffortT(i)
     TeffortT(i) = sf11(i) ! TODO a changer, il y a redondance
   enddo 
call Effort_Tranchant()

!****************TRIMARAN**********************************
!   Nouvelle routine pour le calcul des contraintes 
!   dues aux splitting moments et moment de torsions
!   Llyod's Register rules Volume 1, Part 6, Chapter 3 p. 5

!decktri=0.   ! nombre de decks concernés pour le cross-deck structure
!longtri=0.   ! Longueur de la coque latérale
!bulktri=0.   ! nombre de cloisons sur la longueur
 
!read (iu_10(iboat),*) decktri, castrim, longtri, longtot, largeurtri, vitessemax 
!read (iu_10(iboat),*) (ptri(j),j=1,decktri)   
!read (iu_10(iboat),*) (GMT(i),i=1,castrim)
!call TRIMARAN
!****************TRIMARAN**********************************

   do i=1,nsolm
      icc=0
   
      do k=1,nsol
         if (nnsol(k).eq.i) icc=1   
      enddo
   
      if(icc.ne.0) then           


         is=is+1

!   	     read (iu_10(iboat),*) bm1(is),sf1(is),bm3(is),sf3(is)
         bm1(is)=MeffortT(i) !bm11(i)
	     sf1(is)=sf11(i)
	     bm3(is)=bm31(i)
	     sf3(is)=sf31(i)

         if(neto.eq.1) then
            write(iu_11(iboat),*)'this particular case (only one panel) is not treated!'
   	        write(iu_14(iboat),*)'this particular case (only one panel) is not treated!'
   	        write(*,*)           'this particular case (only one panel) is not treated!'
   	        stop
         endif
      
         if(((bm1(is).ne.(0.)).and.(isymx.ne.0)).or.((bm3(is).ne.(0.)).and.(isymy.ne.0))) then
            write(iu_11(iboat),*) 'it is not possible to consider a bending moment around an axis of symmetry!'
            write(*,*)            'it is not possible to consider a bending moment around an axis of symmetry!'
    	    write(iu_14(iboat),*) 'it is not possible to consider a bending moment around an axis of symmetry!'
            stop
         endif
      
         write (iu_11(iboat),2315) nnsol(is),bm1(is),bm3(is),bm1(is),bm3(is)
         bm1(is)=bm1(is)/yred
         bm2(is)=bm1(is)/yred
         bm3(is)=bm3(is)/yred
         bm4(is)=bm3(is)/yred	
	     
      else

      endif
   
   enddo
   
endif

if (iana.ne.2) then
   if (imom.ne.0) then
   
      ! pour utiliser les termes pairs et impairs
      
      ifonct(1)=1
      ifonct(2)=1
      
      if(langue.eq.1)then
         write(*,100)'moments de flexion primaires (sr bateau)'
      else
         write(*,100)'primary hull bending moments (sr bateau)'
      endif
   
      call bateau(xneut,yneut)
      ! sauvetage dans sbr bateau des forces de bord "abcd" sur file iu_21(iboat) (pour relecture dans bo1)

   endif

! =============================================================================
! moments d'extremite en tenant compte des coefficients de participation
! =============================================================================
   ipart = 0
   do 25 nel=1,neto
      if(itype(nel).eq.5) goto 25														
      if(panneau(nel).part.ne.(1.0)) then
   	  ipart=1
   	  goto 21
      endif
   25 continue
   
   do is=1,nsol
      if((bm3(is).ne.(0.)).or.(bm4(is).ne.(0.))) then
         write(iu_11(iboat),*) 'the coefficients of participation are not taken into account if &
   		                     a bending moment around a vertical axis is considered'
         write(*,*)            'the coefficients of participation are not taken into account if &
                                a bending moment around a vertical axis is considered'
      	 write(iu_14(iboat),*) 'the coefficients of participation are not taken into account if &
   		                     a bending moment around a vertical axis is considered'
         read(*,*)
         
         goto 22
      endif
   enddo 
   
21 if((imom.ne.0).and.(ipart.eq.1)) then
         if(langue.eq.1)then
            write(*,100)'moments d''extremites (sr participation)'
         else
            write(*,100)'extremity moments     (sr participation)'
         endif
   	  
   	     write(iu_11(iboat),5242)

   	     call participation()
   endif															
   
   
   !	rearrangement du fichier de travail iu_21(iboat)
   !   =======================================
22 if(imom.ne.0) then
         rewind iu_21(iboat)												
   	
     	 do is=1,nsol
   	        do 23 nel=1,neto
   	           if(itype(nel).eq.5) goto 23
   		          read(iu_21(iboat)) abcd

   		          do i=1,8
   		             coef(nel,i,is)=abcd(i)
   	              enddo

23	        continue
   	     enddo
   	 
   	     rewind iu_21(iboat)
   	
   	     do 24 nel=1,neto
   	        if(itype(nel).eq.5) goto 24
   	        do is=1,nsol
   	           do i=1,8
   	              abcd(i)=coef(nel,i,is)
   	           enddo
   	           write(iu_21(iboat)) abcd

   	        enddo
   24	 continue
   endif														


endif


   4 format(a80)                                                                                              
 100 format(1x,a,i8)                                                                                          
 101 format(1x,a,e15.6)                                                                                       
2313 format(/'le coefficient de réduction est ',f6.4)
2315 format('cas de charge nø',i2' les moments d''extremites',          &
     ' (non réduits) sont:'/                                            &
     'a gauche bmx1= ',e14.7,' n.m'/,'         bmy1= ',e14.7,' n.m'/,   &
     'a droite bmx2= ',e14.7,' n.m'/,'         bmy2= ',e14.7,' n.m'/)                   
5241 format(///' donnees relatives aux moments d''extremites'/          &
       t2,50(1h*)/15a4)
5242 format(///' donnees relatives aux coefficients de participation'/  &
       t2,50(1h*)/15a4)

return
end
