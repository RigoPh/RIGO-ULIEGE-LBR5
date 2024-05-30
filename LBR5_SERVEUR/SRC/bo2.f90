subroutine bo2(jlmax,ich,im,m1tabl1,nbuck,nel)								

use param_section


implicit double precision(a-h,o-z)
double precision lamb

dimension equ(8),disl(1710),fp(20),nf(22),dw(11,5)
dimension vs(72)
dimension eff(9690),eff2(5400),conc(750),z2(2295),effcomb(9690),us(20000)!, &
          !sxm(iptmax+2*neto+11)
dimension dequ(8),id1(9),m1tabl1(nmax)
character *9 nom1(9) ! Nom1 etait avant dans la ligne precedente ! WAX
double precision dchamax_temp(5,9) 

type fatigue_structural_element_temp !TODO : A ENLEVER DE LA !!! :-|
		double precision u
		double precision v
		double precision w
		double precision nx
		double precision ny
		double precision qx
		double precision qy
		double precision mx
		double precision my
		double precision q
end type

!type(fatigue_structural_element_temp) fatigue_stiffened_panel_temp(255), &
!                                      fatigue_frame_temp(255)          , &
!                                      fatigue_girder_temp(255)


data nom1/'w','wø','wøø','wøøø','u','uø','v','vø ','vøø'/
data id1/8,3,9,10,1,11,2,12,13/

lamb   =0.d00
equ(:) =0.d00
disl(:)=0.d00
fp(:)  =0.d00
nf(:)  =0
dw(:,:)=0.d00

if (nel.eq.1) sxm(:)=0.d00
	  
vs(:)    =0.d00
eff(:)=0.d00
eff2(:)=0.d00
conc(:)=0.d00
z2(:)=0.d00
effcomb(:)=0.d00
us(:)=0.d00

dequ(:)=0.d00
!     m1tabl1(:)=0
	  

!  =========================================================

if (is_selected_loadcase_fatigue(is).eq.1) then

!  =========================================================

!!TODO : A ENLEVER DE LA !!! :-|
!    do i=1,255
!       fatigue_stiffened_panel_temp(i).nx=0.0d+00
!       fatigue_stiffened_panel_temp(i).ny=0.0d+00
!       fatigue_stiffened_panel_temp(i).qx=0.0d+00
!       fatigue_stiffened_panel_temp(i).qy=0.0d+00
!       fatigue_stiffened_panel_temp(i).mx=0.0d+00
!       fatigue_stiffened_panel_temp(i).my=0.0d+00
!       fatigue_stiffened_panel_temp(i).q =0.0d+00
!    enddo
!    
!    do i=1,255
!       fatigue_frame_temp(i).nx=0.0d+00
!       fatigue_frame_temp(i).ny=0.0d+00
!       fatigue_frame_temp(i).qx=0.0d+00
!       fatigue_frame_temp(i).qy=0.0d+00
!       fatigue_frame_temp(i).mx=0.0d+00
!       fatigue_frame_temp(i).my=0.0d+00
!       fatigue_frame_temp(i).q =0.0d+00
!    enddo
!       
!    do i=1,255
!       fatigue_girder_temp(i).nx=0.0d+00
!       fatigue_girder_temp(i).ny=0.0d+00
!       fatigue_girder_temp(i).qx=0.0d+00
!       fatigue_girder_temp(i).qy=0.0d+00
!       fatigue_girder_temp(i).mx=0.0d+00
!       fatigue_girder_temp(i).my=0.0d+00
!       fatigue_girder_temp(i).q =0.0d+00
!    enddo
!    
!    fatigue_stiffened_panel(caschge_fatigue,1:3,1:3)%nx=0.0d+00
!    fatigue_stiffened_panel(caschge_fatigue,1:3,1:3)%ny=0.0d+00
!    fatigue_stiffened_panel(caschge_fatigue,1:3,1:3)%qx=0.0d+00
!    fatigue_stiffened_panel(caschge_fatigue,1:3,1:3)%qy=0.0d+00
!    fatigue_stiffened_panel(caschge_fatigue,1:3,1:3)%mx=0.0d+00
!    fatigue_stiffened_panel(caschge_fatigue,1:3,1:3)%my=0.0d+00
!    fatigue_stiffened_panel(caschge_fatigue,1:3,1:3)%q =0.0d+00
!    
!    fatigue_frame(caschge_fatigue,1:3,1:2)%nx=0.0d+00
!    fatigue_frame(caschge_fatigue,1:3,1:2)%ny=0.0d+00
!    fatigue_frame(caschge_fatigue,1:3,1:2)%qx=0.0d+00
!    fatigue_frame(caschge_fatigue,1:3,1:2)%qy=0.0d+00
!    fatigue_frame(caschge_fatigue,1:3,1:2)%mx=0.0d+00
!    fatigue_frame(caschge_fatigue,1:3,1:2)%my=0.0d+00
!    fatigue_frame(caschge_fatigue,1:3,1:2)%q =0.0d+00
!    
!    fatigue_girder(caschge_fatigue,1:3,1:10)%nx=0.0d+00
!    fatigue_girder(caschge_fatigue,1:3,1:10)%ny=0.0d+00
!    fatigue_girder(caschge_fatigue,1:3,1:10)%qx=0.0d+00
!    fatigue_girder(caschge_fatigue,1:3,1:10)%qy=0.0d+00
!    fatigue_girder(caschge_fatigue,1:3,1:10)%mx=0.0d+00
!    fatigue_girder(caschge_fatigue,1:3,1:10)%my=0.0d+00
!    fatigue_girder(caschge_fatigue,1:3,1:10)%q =0.0d+00

!  =========================================================

endif


!  =========================================================


! ====================================================================================================
!     subroutine bo2
!     ===============
!     subroutine de mise en forme des resultats par sommation sur les   
!     inconnues hyperstatiques du systeme.
!     (la resolution se fait panneau par panneau ).                     

! ====================================================================================================




! ====================================================================================================
!	ordre ds sens1(jj,iptmax,9,neto) avec jj=1 à 9
!		      1   2    3     4      5   6    7   8     9 
!	ordre ds defh,defa,defb, ... et sensh,sensa,sensb, ...
!               u,v,wø,ny,nyx,my,ry,w,wøø,wøøø,uø,vø,vøø
!               1 2 3  4  5    6  7 8  9  10   11 12 13
!	relation de position entre sens1 et sensh,sensa,...,defh,defa,defb, ...
!	sens1(i) = fct[sensh(idi)]
! ====================================================================================================

if (nel.eq.1) then !correction mars09
	phmax = 0.d00
endif

 

!mise à zéro (pour chaque panneau)
call annuld(vs,72)		


! ====================================================================================================
! caracteristiques panneau
! ====================================================================================================
ipt=panneau(nel).ipts
ityp1=itype(nel)
isect1=isect(nel)

e1   =e      (indMateriau(nel))
eta1 =eta    (indMateriau(nel))
sigy1=sigy   (indMateriau(nel))
sigm1=sigyadm(indMateriau(nel))


if(impr2.ge.-1) then		
   write(iu_11(iboat),73) nel
endif					

call annuld(eff2,5400)
call annuld(eff,9690)
call annuld(conc,750)

! ====================================================================================================
! sens2 sensibilites cumulees relatives au contraintes (sc borde, raid, cadre, trav)
! ====================================================================================================
call annuld(sens2,16*iptmax*9*neto)  ! calcule dans subr resul !avril2003

! ====================================================================================================
! sens3 sensibilites cumulees des contraintes non combinees (sx, sy, txy, etc
! ====================================================================================================
call annuld(sens3,21*iptmax*9*neto)  ! matr. de travail de la subr. resul

! ====================================================================================================
! sens4 = sensibilites cumulees de sens1 (v,u,w, etc.
! ====================================================================================================
call annuld(sens4, 9*iptmax*9*neto)  ! calcules dans bo2

phil = dabs(panneau(nel).phil)
teta = panneau(nel).teta
delto = width*0.05
const2(:,:) = panneau(nel).const2(:,:)
const3(:,:) = panneau(nel).const3(:,:)

a1= 0.d00
b1= 0.d00
cc1=0.d00
d1= 0.d00
a2= 0.d00
b2= 0.d00
cc2=0.d00
d2= 0.d00

! ====================================================================================================
! 1.  début de la boucle sur le nbre de termes de la série de fourier "do 9 ..."
! ====================================================================================================
! 1.1 lecture des resultats intermediaires (pour le panneau nel considéré) 
! ====================================================================================================

do 9 nz=1,jlmax

   if(mod(nz,2).eq.1) then  
	  iterm=1						     ! iterm = 1 si ijk est impair
   else                      
      iterm=2						     ! iterm = 2 si ijk est pair
   endif

   !il s'agit d'un terme pair + charge symétrique
   if(ifonct(iterm).ne.0) then	 

      read(iu_scratch_1(iboat,nel))disa,disb,disc,disd,argq,m,hyp,lamb,ijk

      if((panneau(nel).kse.eq.0).and.(ich.eq.0).and.(imom.eq.0)) then
         do i=1,1710
            dish(i,is)=0.d00
         enddo
      
      else
         read(iu_scratch_1(iboat,nel))((dish(l,ll),l=1,1710),ll=1,nsol)
      endif

! ====================================================================================================
!     lecture des forces de bord  (zsn solution de mdr)
! ====================================================================================================
      do j=1,nsol
         read(iu_scratch_3(iboat,nel)) (zsn(i,j),i=1,8)
      end do
      
      do i=1,8
         equ(i)=zsn(i,is) ! is est le cas de charge étudié
      end do

	  !equ(:)=0.d00
	  !equ(8)=1.

      ind=0 ! cad sans effets de bord (jlbord)


! ====================================================================================================                                           
! 1.2 impression des inc. hyperstatiques agissant sur les bords (y=0 et y=yo)                                                    
! ======================================

      nbre=3
      if(impr2.ne.-3) then
         nbre=22
         if((impr2.eq.-1).or.(impr2.eq.-2)) nbre=10
         if(impr2.ge.-1) then		
	        write(iu_11(iboat),74)nz
            write(iu_11(iboat),72)(equ(i),i=1,8)
	     endif				
      
      endif

! ====================================================================================================
! 1.3 début de la boucle relative aux calculs des sensibilités de  u,v,wø,ny,nyx,my,ry,w,wøø,wøøø,uø,vø,vøø	
! ====================================================================================================


! ====================================================================================================
!     calcul des  sensibilites    c.à.d  sens1(9,4,ntot(iboat)) non cumule et sens4(cumule) 
!     dérivées des (u,v,w,uø,etc.) par rapport aux variables de conception. 
! ====================================================================================================

	  if(iopti.ne.0) then
         read(iu_scratch_2(iboat,nel)) sensh,sensa, sensb, sensc, sensd,  &
                                       sensaa,sensbb,senscc,sensdd,       & 
                                       defa,defb,defc,defd,               &
                                       defaa,defbb,defcc,defdd
         
! ====================================================================================================
!        lecture des derivees des forces de bord  (dzsn solution de mdr2)    
! ====================================================================================================

     	 j1=8*(nel-1)
	     do iss=1,nsol
		    do j=j1+1,j1+8
 		       read(iu_scratch_3(iboat,nel)) (dzsn(j,ivar,iss),ivar=1,ntot(iboat)) 	!dzsn(ne,9*neto,nsol)
		    end do
	     end do
         !dzsn(:,:,:)=0.d00
! ====================================================================================================
!        boucle 184/183:boucle sur toutes les variables de conception (kvar=1,ntot(iboat));
!                     (il y a nbrxi variable de conception par panneau)
!        boucle 180 = boucle sur les points de calcul et les fonctions
! ====================================================================================================
         
	     kvar=0
         
         do ipan=1,neto
            i2=8*(nel-1)
            nbrxi=nvar(ipan,iboat)
         
            do kk=1,nbrxi
         
               kk1=kvar+kk
               k=nxit(kk,ipan,iboat)
         
               do i=1,8
                  dequ(i)=dzsn(i+i2,kk1,is)
               enddo
         
               do i=1,iptmax
                  do jj=1,9
	                   j=id1(jj)
                     
! ====================================================================================================
!                    sens1 est relatif au panneau étudié par bo2 cad le panneau nel,
!                    mais il donne les sensibilités pour les variables de conception
!                    de tous les panneaux (ipan)
! ====================================================================================================
                     
                     sens1(jj,i,k,ipan) = dequ(1)*defc(j,i)   + dequ(2)*defcc(j,i)    &
                                        + dequ(3)*defd(j,i)   + dequ(4)*defdd(j,i)    &
                                        + dequ(5)*defa(j,i)   + dequ(6)*defaa(j,i)    &
                                        + dequ(7)*defb(j,i)   + dequ(8)*defbb(j,i)
                     if(ipan.eq.nel)then
                        sens1(jj,i,k,ipan)=sens1(jj,i,k,ipan) + sensh(j,i,k,is)       &
                                        + equ(1)*sensc(j,i,k) + equ(2)*senscc(j,i,k)  &
                                        + equ(3)*sensd(j,i,k) + equ(4)*sensdd(j,i,k)  &
                                        + equ(5)*sensa(j,i,k) + equ(6)*sensaa(j,i,k)  &
                                        + equ(7)*sensb(j,i,k) + equ(8)*sensbb(j,i,k)
                     endif    
                     
! ====================================================================================================
                     ! version cumulee de sens1
! ====================================================================================================
                    
					 ispecial = 0 !0=> calcul normal; 1 = calcul en DIS(2)
					 
					 if (jj.eq.1) then !Déplacement w
						if (ispecial.eq.0) temp = sens1(jj,i,k,ipan)*dsin(lamb*dis(5)) !Car X=L/2
						if (ispecial.eq.1) temp = sens1(jj,i,k,ipan)*dsin(lamb*dis(2)) !Car X=L/4
					 elseif (jj.eq.5) then !Déplacement u
						temp = sens1(jj,i,k,ipan)*dcos(lamb*dis(1)) !Car X=0
					 elseif (jj.eq.7) then
						if (ispecial.eq.0) temp = sens1(jj,i,k,ipan)*dsin(lamb*dis(5)) !Car X=L/2
						if (ispecial.eq.1) temp = sens1(jj,i,k,ipan)*dsin(lamb*dis(2)) !Car X=L/4
					 else
						temp = sens1(jj,i,k,ipan)
					 endif
					 sens4(jj,i,k,ipan) = sens4(jj,i,k,ipan) + temp

	              enddo
               enddo
         
            enddo                                          		
            kvar=kvar+nbrxi
         enddo                                             		
         
	     if ((nz==jlmax).and.(iprint.ge.1)) then
	        write(iu_11(iboat),*) ' sensibilites cumulées (déplacements) '
            write(iu_11(iboat),*) ' ************************************ '
         
            do ipan=1,neto
	           nbrxi=nvar(ipan,iboat)
               write(iu_11(iboat),*)'fct.     pt.1          pt.2          pt.3          pt.4          pt.5          pt.6          pt.7          pt.8          pt.9          pt.10          pt.11'
               do kk=1,nbrxi
                  k=nxit(kk,ipan,iboat)
                  write(iu_11(iboat),*) ' variable de conception nø',k,' du panneau nø',ipan
                  write(iu_11(iboat),*) ' --------------------------------------------------'
                  do jj=1,9
                     write(iu_11(iboat),182) nom1(jj),(sens4(jj,i,k,ipan),i=1,iptmax)
                  enddo
               enddo
            enddo
         endif

      endif

! ====================================================================================================
!     fin de la boucle relative aux calculs des sensibilités de u,v,w, ... 
! ====================================================================================================
      
! ====================================================================================================
!     1.4 cummul des effets de la charge exterieure et des charges de bords pour 
!         chaque panneau                                                     
! ====================================================================================================

	  do i=1,22												
         nf(i) =0												
      enddo

      do i=1,3                                                      
         nf(i)=i                                                           
      enddo
      
      if(impr2.ne.-3) then		
         j=3                                                            
         do 701 i=24,45                                                    
            if((impr2.eq.-1).or.(impr2.eq.-2)) then     
			         if (ityp1.eq.5) then								
			            if((i.eq.26).or.(i.eq.27)) goto 701				
			            if((i.ge.29).and.(i.le.31)) goto 701			
			            if((i.ge.33).and.(i.le.39)) goto 701			
			            if((i.eq.41).or.(i.ge.43)) goto 701				
			         else												
                  if(i.ge.39) goto 701                                       !nx,mx,etc.
                  if((i.ge.24).and.(i.le.28)) goto 701                       !my,ny,etc.
			         endif												
	          endif
            if((i.eq.29).or.(i.eq.36).or.(i.eq.37)) goto 701                 !wø,vøøøetuøøø
            j=j+1                                                             
            nf(j)=i
         701 continue
      endif

      do 80 k=1,m
      do 80 i=1,nbre
        nfonct=nf(i)
        if (nfonct.eq.0) goto 80
        goto(2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,   &
             2,2,2,2,1,2,2,2,1,1,1,2,2,1,2,1,2,2,1,1),nfonct

          1   sy=1.
              goto 813
          2   sy=-1.
        813   call pconti(disa,disb,disc,disd,equ,k,nfonct,dish(1,is),sy)

      80 continue

      if(panneau(nel).mt.eq.0) goto 205
      if(is.gt.1)then
         is1=140+is*20
      else
         is1=0
      endif

      do 81 i=1,panneau(nel).mt
         fp(i)=hyp(i+is1)
         jm=i+panneau(nel).mt
         fp(jm)=hyp(jm+is1)
         if(ind.eq.0) goto 84  ! cad sans effets de bord nb et mb (jlbord)
         84     continue
         do 81 j=1,8
            ij=i+20*j
            fp(i)=fp(i)+hyp(ij)*equ(j)
            ij=ij+panneau(nel).mt
            fp(jm)=fp(jm)+hyp(ij)*equ(j)
      81 continue

      do 83 i=1,2*panneau(nel).mt
         fp(i)=fp(i)/10000.
      83  continue


      do i=1,panneau(nel).mt
         uecr(2*i+30)=panneau(nel).abtr(i)
         uecr(2*i+31)=panneau(nel).abtr(i)
      enddo


      205 sm=dabs(panneau(nel).phil)/30.

      do i=1,31
         uecr(i)=float(i-1)*sm
      enddo

      do ii=1,2295
         z2(ii)=0.
      enddo

! ====================================================================================================
! 1.5 calcul des resultats en 31 points selon la hauteur et en 5 points 
!     selon la largeur du panneau ainsi qu'au droit destraverses.       
! ====================================================================================================

      do 901 i=1,nbre

         nfonct=nf(i)
         goto(17,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,       &
              18,18,18,18,18,17,17,17,17,18,17,17,17,18,18,18,17,17,18,17,18,17, &
              17,18,18),nfonct
         17 sy=-1.
            goto 4
         18 sy=1.
          4 if (nfonct.eq.0) goto 901

         call compu(dabs(panneau(nel).phil),fp,dish(1,is),disc,disb,panneau(nel).abtr(:), &
		                m,argq,panneau(nel).mt,uecr,panneau(nel).teta,z2,nfonct,sy,31)


      901 continue

! ====================================================================================================
! 1.7 calculs des resultats (déplacements et contraintes)
! ====================================================================================================

!      if(impr.ne.-3) goto 3000
!      call sorti(lamb,dis,mt,eff,z2)
!      goto 9
! 3000 continue

	  if(ityp1.ne.5) then
         call resul (panneau(nel).wya,panneau(nel).tya,panneau(nel).dya,panneau(nel).hya,panneau(nel).ksa,                &
		             panneau(nel).wxr,panneau(nel).txr,panneau(nel).dxr,panneau(nel).hxr,panneau(nel).ksr,                &
		             panneau(nel).delta,eta1,lamb,e1,nz, panneau(nel).mt,panneau(nel).kst,sigy1,                          &
		             panneau(nel).epsa,panneau(nel).epsr,panneau(nel).entr,                                               &
					 ityp1,panneau(nel).q,jlmax,nel,                                                                      &
					 eff(1),eff2(1),eff2(2041),eff2(3571),eff2(5101),                                                     &
                     conc,z2,sxm(1),sxm(iptmax+1),sxm(iptmax+neto+1),is)

	  else
         call resul2(panneau(nel).wya,panneau(nel).tya,panneau(nel).dya,panneau(nel).hya,panneau(nel).ksa,                &
		             panneau(nel).wxr,panneau(nel).txr,panneau(nel).dxr,panneau(nel).hxr,panneau(nel).ksr,                &
		             panneau(nel).aire,panneau(nel).aiy,lamb,e1,nz,panneau(nel).mt,panneau(nel).kst,                                                &
					 panneau(nel).tork,isect1,panneau(nel).epais,jlmax,nel,                                      &
					 eff(1),eff2(2041),eff2(3571),eff2(5101),conc,z2)
	  endif															



! ====================================================================================================
   !!!   if (is_selected_loadcase_fatigue(is).eq.0) then
! ====================================================================================================

! ====================================================================================================
!        flexion locale des raidisseurs
! ====================================================================================================

         if(nz.eq.1) then ! premier terme de la serie de fourier
            if(iopti.ge.1) then

! ====================================================================================================
!           recherche des restrictions choisies : iq=1 oui et iq=0 non
! ====================================================================================================

	           iq50=0

	           do i=1,m1tabl1(nel)                 !  im=compteur des restr. struct.                       
                  if((panneau(nel).lcont(1,i).eq.12).or.  &  ! restr nø 12 : bord l/2 + sig stiff               
                     (panneau(nel).lcont(1,i).eq.13).or.  &  ! restr nø 13 : bord l/2 + sig stiff + sig pltb      
                     (panneau(nel).lcont(1,i).eq.33).or.  &  ! restr nø 33 : taujab x=0 + tau stiff               
                     (panneau(nel).lcont(1,i).eq.34).or.  &  ! restr nø 34 : sigsem x=l/2 + sig stiff             
                     (panneau(nel).lcont(1,i).eq.35).or.  &  ! restr nø 35 : sigjab x=l/2 + sig stiff + tau stiff 
                     (panneau(nel).lcont(1,i).eq.36).or.  &  ! restr nø 36 : sigjas x=l/2 + sig stiff             
                     (panneau(nel).lcont(1,i).eq.51).or.  &  ! restr nø 51 : flèche 1/384
                     (panneau(nel).lcont(1,i).eq.52).or.  &  ! restr nø 52 : flèche 5/384
                     (panneau(nel).lcont(1,i).eq.54).or.  &  ! restr nø 54 : sigma semelle
                     (panneau(nel).lcont(1,i).eq.55).or.  &  ! restr nø 55 : sigma bordé
                     (panneau(nel).lcont(1,i).eq.56).or.  &  ! restr nø 56 : 1.73*tau
                     (panneau(nel).lcont(1,i).eq.57).or.  &  ! restr nø 57 : flèche maille 1/384
                     (panneau(nel).lcont(1,i).eq.58)) then   ! restr nø 58 : flèche maille 5/384
            
		             iq50=1  
		             goto 46
		          endif  
               enddo
               46 continue
	        endif

         if (ityp1.ne.5) then					

            call stiff(nel,e1,panneau(nel).ploc,panneau(nel).chamax,panneau(nel).dchamax,is,     &
                       panneau(nel).plat,                                              &
					   panneau(nel).fl,panneau(nel).sig1,panneau(nel).sig2,panneau(nel).sig3,panneau(nel).sig4,    &
					   panneau(nel).tau,flr,panneau(nel).wpl1,panneau(nel).wpl2,      & 
                       vs(1),vs(10),vs(19),vs(28),vs(37),vs(46),vs(55),vs(64),iq50)                           
            endif
			
			!Compteur de cas de charge fatigue
			nbr_lc_fatigue = 0
			do i=1,is
				if (is_loadcase_fatigue(nnsol(i)).eq.1) nbr_lc_fatigue = nbr_lc_fatigue + 1
			enddo	
			if (nbr_lc_fatigue.gt.0) sx_loc_raid_sem(nbr_lc_fatigue,nel) = panneau(nel).sig2
		endif                                                                                       

!  =========================================================
   !!!   endif
!  =========================================================

                                                                                              
! ====================================================================================================
!     1.8 verification de l'equilibre du panneau (si impr = 0 ou 1)
! ====================================================================================================

      if(impr2.gt.0) then
         if(nz.le.1) then

            call equil(panneau(nel).mt,z2,lamb,xi(is,nel),xf(is,nel),ivaria(nel),dabs(panneau(nel).phil),   &
			           ijk,panneau(nel).q,nel,ind,a1,b1,cc1,d1,a2,b2,cc2,d2,delto)
          endif
	  endif

!  =========================================================
      if (is_selected_loadcase_fatigue(is).eq.1) then
!  =========================================================

! ====================================================================================================
!        1.8 calcul sensibilités fatigue (si impr = 0 ou 1)
! ====================================================================================================


 !!!        call fatigue_sens(                                                                                                       & 
!!!		             panneau(nel).wya,panneau(nel).tya,panneau(nel).dya,panneau(nel).hya,panneau(nel).ksa,                        &
!!!		             panneau(nel).wxr,panneau(nel).txr,panneau(nel).dxr,panneau(nel).hxr,panneau(nel).ksr,                        &
  !!!                   panneau(nel).delta,eta1,lamb,e1,nz,panneau(nel).mt,panneau(nel).kst,                                         &
!!!                     panneau(nel).epsa,panneau(nel).epsr,panneau(nel).entr,                                                       &
!!!					 panneau(nel).q,nel,z2,                                                                                       &

                    !panneau(nel).nx_lbr5,                                                                                       &
					!panneau(nel).ny_lbr5,                                                                                       &
                    !panneau(nel).mx_lbr5,                                                                                       &
					!panneau(nel).my_lbr5,                                                                                       &
                    !panneau(nel).nxy_lbr5,                                                                                      &
					!panneau(nel).u_lbr5,panneau(nel).v_lbr5,panneau(nel).w_lbr5,                                                &

!!!                     panneau(nel).length_girder,                              &
!!!                     panneau(nel).abtr,                                       &
!!!					 panneau(nel).hxtr,panneau(nel).wxtr,                     &
!!!					 panneau(nel).dxtr,panneau(nel).txtr,                     &
!!!					 panneau(nel).hight,panneau(nel).sh,panneau(nel).phil,xi(is,nel),xf(is,nel),jlmax, &
!!!					 fatigue_stiffened_panel_temp,&
!!!                     fatigue_frame_temp,          &
!!!					 fatigue_girder_temp)

!  =========================================================
      endif
!  =========================================================



endif !ifonct

9  continue ! boucle sur nz (nbre de termes)


! ====================================================================================================
!!! fin de la boucle sur le nbre de termes de la série de fourier
! ====================================================================================================
!  =========================================================
if (is_selected_loadcase_fatigue(is).eq.1) then
!  =========================================================
!!!   call fatigue_scratch_files()

!  =========================================================
endif
!  =========================================================


!  =========================================================
if (is_selected_loadcase_fatigue(is).eq.0) then
!  =========================================================

! ====================================================================================================
!     2.0 prise en compte de la flexion locale des raidisseurs            
! ====================================================================================================

! ====================================================================================================
!     combinaison des résultats donnés par resul et par stiff          
! ====================================================================================================

      call combine(eff,panneau(nel).sig1,panneau(nel).sig2,panneau(nel).sig3,panneau(nel).sig4, &
                  panneau(nel).tau,panneau(nel).mt,effcomb,panneau(nel).txr)                                 

! ====================================================================================================
!     2.1 calcul du sigma plaque et de sa derivee                          
! ====================================================================================================


      if(ityp1.ne.5) then								
	 
		 !iii=(nel-1)*iptmax+1
		 dchamax_temp(1:5,1:9) = panneau(nel).dchamax(1:5,is,1:9)
         call plaque(panneau(nel).epsa,panneau(nel).epsr,panneau(nel).entr,panneau(nel).delta,vs(1:9),nel,       &
                     panneau(nel).hya, panneau(nel).hxr,dabs(panneau(nel).phil),panneau(nel).q,effcomb,        &
                     panneau(nel).chamax(1:5,is),dchamax_temp(1:5,1:9),panneau(nel).ploc,e1,eta1,nvar(nel,iboat), &
                     panneau(nel).sig1,eff,ipt,panneau(nel).sigplaque,panneau(nel).dsigplaque,panneau(nel).sigxplaque,         &
                     panneau(nel).sigyplaque,panneau(nel).phiplaque,panneau(nel).sigvmtplaque, &
                     panneau(nel).sigvmcplaque,panneau(nel).indplaque,panneau(nel).sigmx,panneau(nel).sigmy)
        panneau(nel).dchamax(1:5,is,1:9) = dchamax_temp(1:5,1:9)
      endif

! ====================================================================================================
!        3.  calcul de l'epaisseur minimale (cfr progr. de hughes, edt. sname)
! ====================================================================================================

! ====================================================================================================
!	       tpl  = epaisseur minimale de hughes
!	       dtpl = delta- tpl (hughes) = valeur de la restriction c(j) = delta-tpl<0
!	       tpla = d(tpl)/d(xi=epsa)   = dérivées de c(j) par rapport à epsa
!	       tplr = d(tpl)/d(xi=epsr)   = dérivées de c(j) par rapport à epsr
! ====================================================================================================



! ====================================================================================================
!        minimum plate thickness assessment : yielding and buckling
!--------------------------------------------------------------
!               sr1 = ratio applied stress/yield stress (?<? 1.0)
!               sr2 = global interaction ratio: applied stress/critical stress (?<? 1.0)
!
!               panel <-- panel dimensions -->  minimum thickness <- collapse scenario & applied stress --->  yielding   buckling  critical stresses
!                     l(long) b(trans) d(thick)  d(min)  d/d(min)   mode  section   sx    sy     tau    plat  (s/sy<1)  (s/scr<1) sx(cr) sy(cr) t(cr)
!                        (m)     (m)    (mm)      (mm)   (> 1 ??)           y/yo   n/mm2  n/mm2  n/mm2  (m)   von-mises interact  n/mm2  n/mm2  n/mm2
!                                                                                                               sr1     form(sr2)
!                 1    0.371   0.873     8.00     7.82    1.02    yielding  1.00    47.1   51.7    0.0   0.00  1.000      0.000   337.9  117.8  513.0
! ====================================================================================================

         if(nel.eq.1) then
            if(langue==1 ) then ! french
               write(iu_12(iboat),'(a/62(1h-))') ' epaisseur minimale de bordé: plastification et voilement'
            else               ! english
               write(iu_12(iboat),'(a/62(1h-))') ' minimum plate thickness assessment : yielding and buckling'
            endif
         
            if((itera.ge.1).and.(iopti.ge.1)) write(iu_31(iboat),562)
            write( iu_12(iboat),562)
         
         endif
         
         562  format(5x,' sr1 = ratio of applied stress/yield ',                       &
                  'stress (?<? 1.0)'/5x,' sr2 = global interaction ratio:',            &
                  ' applied stress/critical stress (?<? 1.0)'//                        &
                  5x,' for yielding (global stresses(sx,sy) are combined with ',       &
                                    'the stress induced by the stiff bending)'/        &
                  5x,' for buckling (global stresses(sx,sy) are considered alone)'     &
                  //'panel <-- panel dimensions -->  minimum ',                        &
                  'thickness <- collapse scenario & applied stress ---> ',             &
                  ' yielding   buckling  elast.crit.stress'/                           &
                  '      l(long) b(trans) d(thick)  d(min)',                           &
                  '  d/d(min)   mode  section   sx    sy     tau    plat  ',           &
                  '(s/so <1) (s/scr<1) sx(cr) sy(cr) t(cr)'/                           &
                  '         (m)     (m)    (mm)      (mm) ',                           &
                  '  (> 1 ??)           y/yo   n/mm2  n/mm2  n/mm2  (m)   ',           &
                  'von-mises interact  n/mm2  n/mm2  n/mm2'/                           &
                   t97,'sr1     form(sr2)'/)                                         
           
         if(iopti.ge.1) then
         !  vérification si la restriction "dmin-d<0" est choisie : iq=1 oui et iq=0 non
            iq=0
         	 if(ityp1.eq.5) then
         	    iq=0										
         	 else										   		
         	    do i=1,m1tabl1(nel)
                    if(panneau(nel).lcont(1,i).eq.4) then    
         		          ! dmin-d<0 =  restrict nø4
         	          iq=1  ! restriction 4 selectionnée
         	              ! sigm1=cjmax3(nnsol(is),nel,i)					
         			  goto 45
         	       endif
                 enddo
              endif													
              45  continue
         endif
         
         if(ityp1.ne.5) then	
            if(dabs(panneau(nel).phil).ge.1.) then     ! vérification si paneau est une plaque (et pas une coque)
               write(iu_31(iboat),197)   ! pas de calcul car coque
               write(iu_14(iboat),197)	 
         !	  write(9           ,197)
            else
         
               if((iopti.ge.1).and.(iq.eq.1)) then
                  dtpl = -panneau(nel).delta                                    ! dmin-d
                  tpla = 0.                                             ! d(dmin-d)/d(epsa)
                  tplr = 0.                                             ! d(dmin-d)/d(epsr)
                  tpld = -1.                                            ! d(dmin-d)/d(delta)
                  
         ! ====================================================================================================
         !        parmi delta, epsa et epsr, il y a-t-il bien une variable de conception sélectionnée?
         !        si non : stop  impossible d'utiliser cette restriction.
         ! ====================================================================================================
         
         		 kxi=nxi2(1,nel,iboat)+nxi2(5,nel,iboat)+nxi2(9,nel,iboat)
                  if(kxi.eq.0) then
         	          write(*,*)  
         	          write(*,*)' stop - error/erreur '
         	          write(*,*)' --------------------'
         	          write(*,*)' panneau     - panel      :',nel
         	          write(*,*)' restriction - constraint : n°4 hughes'
         	          write(*,*) ' see explanation in the output file opt-*.*'
         
         	          write(iu_31(iboat),*)  
         	          write(iu_31(iboat),*)' stop - error/erreur '
         	          write(iu_31(iboat),*)' --------------------'
         	          write(iu_31(iboat),*)' panneau     - panel      :',nel
         	          write(iu_31(iboat),*)' restriction - constraint : n°4 hughes'
         	          write(iu_31(iboat),*)
         
         		      write(iu_14(iboat),*)															
         	          write(iu_14(iboat),*)' erreur : subr bo2'				
         	          write(iu_14(iboat),*)' panneau :',nel						
         	          write(iu_14(iboat),*)' restriction - constraint : n°4 hughes'					
         	          write(iu_14(iboat),*)' vous avez selectionné la restriction n°4 &
         	                                 (dite de hughes) et aucune des variables de conception'		
         	          write(iu_14(iboat),*)' suivantes n''a été sélectionnée pour le panneau ',nel														
                       write(iu_14(iboat),*)' - 1: épaisseur du panneau            (delta)'	
         	          write(iu_14(iboat),*)' - 5: entredistance entre raidisseurs (epsr)'	
         	          write(iu_14(iboat),*)' - 9: entredistance entre cadres      (epsa)'	
         	          write(iu_14(iboat),*)' il faut obligatoirement une (ou plusieurs)  ',  &	
                                          'de ces 3 variables de conception pour que la ',	 &
                                          'restriction n°4 soit active.'										
         		      write(iu_14(iboat),*)' lbr-5 est arreté. veuillez corriger vos données'											
         
                       if(langue.eq.1) then
         	             write(iu_31(iboat),*) ' vous avez selectionné la restriction n°4 &
         	                                     (dite de hughes) et aucune des variables de conception'
         	             write(iu_31(iboat),*) ' suivantes n''a été sélectionnée pour le panneau ',nel
         	             write(iu_31(iboat),*) ' - 1: épaisseur du panneau            (delta)'
         	             write(iu_31(iboat),*) ' - 5: entredistance entre raidisseurs (epsr)'
         	             write(iu_31(iboat),*) ' - 9: entredistance entre cadres      (epsa)'
         	             write(iu_31(iboat),*) ' il faut obligatoirement une (ou plusieurs)  ',  &
         	                                   'de ces 3 variables de conception pour que la ',  &
         	                                   'restriction n°4 soit active.'
         
         		         write(*,*) ' lbr-5 est arreté. veuillez corriger vos données'
         		         write(iu_31(iboat),*)' lbr-5 est arreté. veuillez corriger vos données'
                       else
         	             write(iu_31(iboat),*)' constraint n°4 ("hughes") is selected and',      &
         	                                  ' none of the following design variables were selected'
         	             write(iu_31(iboat),*)' for the panel n°',nel
         	             write(iu_31(iboat),*)' - 1: panel thickness      (delta)'
         	             write(iu_31(iboat),*)' - 5: longitudinal spacing (epsr)'
         	             write(iu_31(iboat),*)' - 9: frame spacing        (epsa)'
         	             write(iu_31(iboat),*)' at least one of these 3 design variables',       &
         	                                  'must be selected to activate constraint n°4'
         		         write(iu_31(iboat),*)' lbr-5 is stopped. please correct your data'
         		         write(*,*)  ' lbr-5 is stopped. please correct your data'
                       endif
         
                       write(*,*) 'stop'
         	           read(*,*)
         	          stop
                 endif     ! if(kxi.ne.0)
         
         
         	 endif           ! if((iopti.ge.1).etc.
         
         	 istop=0  ! ré-initialisation
 
               call hughes(iprint,panneau(nel).chamax(:,is),panneau(nel).ploc,e1,eta1,sigy1,sigm1, &
                          dabs(panneau(nel).phil),panneau(nel).q,panneau(nel).mt,   &
                          eff,tpl,tpla,tplr,tpld,									&
                          nxi2(1,nel,iboat),nxi2(5,nel,iboat),nxi2(9,nel,iboat),	&
         				 istop,nel,iq,itera,effcomb,panneau(nel).sigmag)

                                             
               if((iopti.ge.1).and.(iq.eq.1)) then
                  if(istop.eq.0) then  ! ok convergence dans hughes
                        dtpl=tpl-panneau(nel).delta                                           ! dmin-d
                  else                 ! pas de convergence dans hughes
					 write(*,*) 'Info: Panneau',nel,' derivee approx. ds Hughes'
					 !write(*,*) 'dmin :',tpl,' delta :',panneau(nel).delta
                     tpl  =  1.05*panneau(nel).delta                                           ! posons dmin=1.2*delta
                     dtpl =  tpl-panneau(nel).delta  !<- marche pas qd on met ça            ! dmin-d
                     tpla =  0.0001													          ! d(dmin-d)/d(epsa)
                     tplr =  0.01 !0.01                                                      ! d(dmin-d)/d(epsr)
                     tpld =  -2.d00 !-0.1*panneau(nel).delta !valeur artif. !0 !0.2 !-1.3                                                      ! d(dmin-d)/d(delta)
         
                     if(iprint.eq.1) then
         			   write(*,  198)          nel,tpl,tpla,tplr,tpld
         		       write(iu_31(iboat),198) nel,tpl,tpla,tplr,tpld
         	        endif
         		 endif     
         	  endif    ! if((iopti.ge.1).etc.
          
            if(tpl.ne.0.) ratio=tpl/panneau(nel).delta
         
            endif  ! test plaque-coque
         else													
         	if(iopti.ge.1) then						
         	   write(iu_31(iboat),*) 'panneau n0 =',nel
         	   write(iu_31(iboat),*)'--------------------'
         	   write(iu_31(iboat),*) nel,' est une épontille'					
         	endif						
         endif
   

! ====================================================================================================
!        3.bis  calcul des contraintes critiques de flambement (euler et/ou johnson)	
! ====================================================================================================

         if(iopti.ge.1) then
            if((nel.eq.1).and.(itera.ge.1)) write(iu_31(iboat),563)		
            563   format('panel        elast. buck. stress          '                          &
                         'crit. buck. stress'/                                                 &
                         '        column     torsion     web'/                                 &
                         '                     se                      sc       sa/sc'/        &
                         '                   (n/mm2)                 (n/mm2)    (<1?)'/)
         
            ibuck_07=0
            ibuck_08=0
         
            if(ityp1.ne.5) then																					
               do i=1,m1tabl1(nel)
                  if(panneau(nel).lcont(1,i).eq.14) ibuck_07=1								! flambt tôle = restrict nø14
                  if(panneau(nel).lcont(1,i).eq.37) ibuck_07=2								! flambt raidisseur = restrict nø37
         		 if(panneau(nel).lcont(1,i).eq.39) ibuck_08=1                             ! flambt raidisseur = restrict nø38
               enddo
         	 endif																		
           
            if(ibuck_07.ne.0) then
               call buckdcn07(nel,e1,sigy1,panneau(nel).delta,panneau(nel).hxr,panneau(nel).dxr,panneau(nel).wxr,  &
         	               panneau(nel).txr,panneau(nel).epsr,panneau(nel).epsa,panneau(nel).q,eff,      &
         				   ibuck_07)	
               nbuck=1
            endif
         
         
            if(ibuck_08.ne.0) then
               call buckdcn08(nel,e1,sigy1,delta,hxr,dxr,wxr,txr,epsr,epsa,q,eff)		
         	  nbuck=1
            endif
         
         
         continue
         
         endif																	

!  =========================================================
 endif
!  =========================================================
         
         ! ====================================================================================================
         ! 4. impression des resultats 
         !    --------------------------
         ! 4.1 dans fichier sol2 - résultats de stiff (flexion locale raidisseurs)
         !
         ! ====================================================================================================
         
         
         ! ====================================================================================================
         !       laterale   <-bending stress (for m=pl^2/10)->  stiff. deflection   <-- plate deflections ->
         !panel  pressure    sx(plate)  sx(flange)   tau(web)   (simply supported)  (clamped)  (simply supp)  
         !         (m)        (n/mm2)    (n/mm2)     (n/mm2)         (mm)               (mm)       (mm)
         !  1    -25.22      -2250.20    -2250.20    -2250.20       -120.23            -120.23    -120.23 
         ! ====================================================================================================
         
if(impr2.ge.-1) then				
            if(nel.eq.neto) then
          
               if(langue==1 ) then ! french
                  write(iu_12(iboat),'(/a/60(1h-))') ' flexion locale raid. & déformation de la maille'
         	  else                ! english
                  write(iu_12(iboat),'(/a/60(1h-))') ' local stiffener bending & plate deflection'
         	  endif
         
         
         	  write(iu_12(iboat),'(2a,10x,a)')  '       laterale   ',						      &
                                         '<-  stiffener bending stress (for m=pl^2/10)',		  &		
                                         '->  stiff. deflection   <-- plate deflections ->'				
         
         
         	  write(iu_12(iboat),'(2a)')     'panel  pressure     sx(plate)  sx(flange)   ',	  &
                                  'sx(jab)    sx(jas)    tau(web)  (simply supported)  (clamped)  &
                                   (simply supp)  sigma(pltben)'                                      
         
         
         	  write(iu_12(iboat),'(2a)') &
         	                  '                (m)           (n/mm2)         (n/mm2)         (n/mm2)         (n/mm2)     &
                            (n/mm2)            (mm)                     (mm)               (mm)            (n/mm²)'                                
               do i=1,neto
                  write(iu_12(iboat),'(i3,3x,6(f13.2,4x),3x,f7.2,9x,3(f7.2,9x))')                  &
                                      i,panneau(i).plat/9810.,panneau(i).sig1/1.e06,panneau(i).sig2/1.e06,panneau(i).sig3/1.e06,  &  
                                      panneau(i).sig4/1.e06,panneau(i).tau/1.e06,1000.*panneau(i).fl,                     &  
                                      1000.*panneau(i).wpl1,1000.*panneau(i).wpl2,panneau(i).sigmag/1.e06                   
               enddo
            endif
         
         
         endif
 

        
         ! ====================================================================================================
         ! 4.2 dans fichier sol - résultats de result (déplacements et contraintes)
         ! ====================================================================================================
         
         
         if(ityp1.eq.5) then										
            if(impr2.eq.-3) then								
               call ecri2(dis,nel,eff,ns,panneau(nel).q,panneau(nel).mt,uecr)  
            else										
         	  call ecri4(dis,nel,eff,conc,indaig,indrai,panneau(nel).q,ns,   &
         	             impr2,isect1,vnymax,vmymax,symax,symin,                &
         				 vm1,vm2,panneau(nel).mt,uecr)		                                          
            endif							
         else							
         
            if(impr2.eq.-3) then
               call ecri2(dis,nel,eff,ns,panneau(nel).q,panneau(nel).mt,uecr)
            else
               call ecri (dis,nel,eff,effcomb,conc,indaig,indrai,ns,  &
         	             panneau(nel).q,impr2,panneau(nel).vmaxsig,panneau(nel).vmaxsigc,       &
         				 panneau(nel).mt,uecr)           
         
               if((nel.eq.neto).and.(impr2.ge.-1)) then		            
                  write(iu_12(iboat),*)                                             
                  write(iu_12(iboat),*)'détail des contraintes utilisées dans la restriction resul+stiff+pltben'
                  write(iu_12(iboat),*)'-----------------------------------------------------------------------'                                   
                  write(iu_12(iboat),'(3a)') 'panel    sx(resul+stiff)    sy(resul)      ',   &
                                             '  sx(plaque avec phi)  sy(plaque avec phi)  ',  &
                                             'phi   sigvonmises sigvonmises  ind2'           
                  write(iu_12(iboat),'(3a)')                                                  &
                                                  '            (n/mm²)           (n/mm²)  ',  &  
                                                  '         (n/mm²)              (n/mm²)',    &  
                                                  '                  traction   compression'  
         	     do i=1,neto                                             
         		    !iii=(i-1)*4+1                                           
         		    if (panneau(i).indplaque(1).ne.0) then                                    
                        write(iu_12(iboat),'(i3,8x,2(f8.2,9x),f8.2,14x,f8.2,11x,f5.2,1x,2(f8.2,5x),i3)') i,  &                                                       
                           panneau(i).sigmx(1)/1.e06,panneau(i).sigmy(1)/1.e06,panneau(i).sigxplaque(1)/1.e06,          & 
                           panneau(i).sigyplaque(1)/1.e06,panneau(i).phiplaque(1),panneau(i).sigvmtplaque(1)/1.e06,     & 
                           panneau(i).sigvmcplaque(1)/1.e06,panneau(i).indplaque(1)
         	        endif                                                        
                  
         	        do j=2,4                                                     
                       !iii=(i-1)*4+j                                          
         	           if (panneau(nel).indplaque(j).ne.0) then                              
                           write(iu_12(iboat),'(11x,2(f8.2,9x),f8.2,14x,f8.2,11x,f5.2,1x,2(f8.2,5x),i3)')    &                                                   
                               panneau(i).sigmx(j)/1.e06,panneau(i).sigmy(j)/1.e06,panneau(i).sigxplaque(j)/1.e06,    &
                               panneau(i).sigyplaque(j)/1.e06,panneau(i).phiplaque(j),panneau(i).sigvmtplaque(j)/1.e06,     &
                               panneau(i).sigvmcplaque(j)/1.e06,panneau(i).indplaque(j)
         	           endif                                                      
                  
         	        enddo
         	     enddo
         
         		 write(iu_12(iboat),*)'résultats de différentes contraintes maximales'
         	     write(iu_12(iboat),*)'----------------------------------------------'
         	     write(iu_12(iboat),'(a)')  'panel    sx max(resul)      sx max(resul+stiff)'
         	     write(iu_12(iboat),'(a)')  '            (n/mm²)              (n/mm²)'
         
          		 do i=1,neto
         	        write(iu_12(iboat),'(i3,8x,2(f8.2,13x))') i,panneau(i).vmaxsig/1.e06,panneau(i).vmaxsigc/1.e06
         		 enddo
         	  endif
         	   	  
         	  if (impr2.ge.-1) then		
         	     call ecri3(dis,nel,ns,eff2,panneau(nel).mt,uecr)
         	              !       impression des contraintes dans le bordage au niveau des faces du borde                
         	              !       z=+delta/2 et z=-delta/2.                                                              
         	  endif				
         
            endif		  !impr-3 et else
endif			  !épon ou plaque


!  =========================================================
if (is_selected_loadcase_fatigue(is).eq.0) then
!  =========================================================

         ! ====================================================================================================
         ! 5. calcul des restrictions c<cmax (opti)
         ! ====================================================================================================
         
         if(iopti.ge.1) then
         
            call contr(nel,im,img,itera,                                                              &
                       e1,sigy1,effcomb,eff,                                                               &
                       dtpl,tpld,tpla,tplr,panneau(nel).delta,dabs(panneau(nel).phil),      &
                       panneau(nel).epsa,   &
         	 		   panneau(nel).hxr,panneau(nel).dxr,panneau(nel).wxr,panneau(nel).txr,     &
         			   panneau(nel).epsr,panneau(nel).entr,                                     &
                       sxm,sxm(iptmax+2*neto+1),sxm(iptmax+2*neto+10),                                          &
                       sxm(iptmax+2*neto+11),m1tabl1,                                                           &
                       panneau(nel).sig1,panneau(nel).sig2,panneau(nel).sig3,panneau(nel).sig4,panneau(nel).tau,flr, &
                       panneau(nel).wpl1,panneau(nel).wpl2,                                                                     &
                       vs(1),vs(10),vs(19),vs(28),vs(37),vs(46),vs(55),vs(64),                                  &
                       panneau(nel).sigplaque,panneau(nel).dsigplaque,ratio)

         endif
         
         
         ! ====================================================================================================
         ! 6.  ultimate strength of hull girder
         ! ====================================================================================================
         
         if(is.eq.nsol.and.iult.gt.0) then
            if(iult.eq.1) then
         
         ! ====================================================================================================
         !     methode de paik/caldwell : iult=1
         ! ====================================================================================================
         
         ! ====================================================================================================
         !     calcul de la section transversale du panneau  (sur base de l'épaisseur moyenne)
         ! ====================================================================================================
         
         	     sect=panneau(nel).delt*panneau(nel).q*dabs(panneau(nel).phil)*pi/180.d00
               if(nel.eq.1)then
                  do i=1,9
                     do j=1,neto
                        dsud (i,j)=0.d00
                        dsub (i,j)=0.d00
                        dsub1(i,j)=0.d00
                        dsusu(i,j)=0.d00
                        dsusl(i,j)=0.d00
                        dh(i,j)   =0.d00
                        dg(i,j)   =0.d00
                        dad(i,j)  =0.d00
                        dab(i,j)  =0.d00
                        dab1(i,j) =0.d00
                        das(i,j)  =0.d00
                        dult(i*j) =0.d00
                     enddo
                 enddo
               endif
         
               call ushulls(nel,sect,sigy1,e1,q,dabs(panneau(nel).phil),                                           &
                            panneau(nel).epsa,   &
         				   panneau(nel).hxr,panneau(nel).dxr,panneau(nel).wxr,panneau(nel).txr,panneau(nel).epsr,    &
         				   panneau(nel).entr)
            else
               write(iu_11(iboat),'(//a/)') 'this method (ult. strength) is not defined'
            endif
         
         endif
         
! ====================================================================================================
!.6  vérification au flambement de l'épontille		
! ====================================================================================================

         if(ityp1.eq.5) then
            if(vnymax.gt.1000) then
            !pas de compression			 

                call flamb(panneau(nel).heff,panneau(nel).aire,panneau(nel).aix,sigy1,panneau(nel).hya,isect1,	            &	 
              	     panneau(nel).tya,panneau(nel).wya,e1,vnymax,vmymax,symax,symin,	& 
              	     vm1,vm2,panneau(nel).epsa)
           endif
         endif														 
	
!  =========================================================
endif
!  =========================================================


! ====================================================================================================
! 7.  les formats
! ====================================================================================================


 73 format(' panneau-panel no ',i2,/20(1h+) )
 74 format(/' terme numero ',i2,' de la serie de fourier')
 72 format(/' inconnues hyperstatiques correspondant'/' aux conditions      &
              aux limites de la plaque reelle'/1x,44(1h+)/' elles sont ',   &
              'ordonnees dans l''ordre c d a b'//4(2x,e14.7)/4(2x,e14.7)/)
182 format(a4,1x,5(1x,e13.6))
197 format(/'!!! le calcul de l''épaisseur minimale (hughes)',              &
            ' n''est pas valable pour une coque cylindrique !!!'/           &
            ' le calcul ne sera donc pas fait .'/                           &
            ' veuillez vérifier la restriction manuellement'/)
198 format(' nel=',i2,': d min =',e13.6,' d tpl/epsa =',e11.4,              &
                 ' d tpl/epsr =',e11.4,' d tpl/delta=',e11.4,               &
                 ' !! valeurs par défaut !!')
641 format(t2,i2,5e14.7)
782 format(/'pente dw/dx (c.à.d. la rotation autour axe oy)'/)

return
end

