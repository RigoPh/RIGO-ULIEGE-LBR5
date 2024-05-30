subroutine opti_global_multisection()

use param_section, mode_=>mode
use PARAM_SECTION_VECTOR

implicit double precision (a-h,o-z)

double precision, allocatable, save :: tempPhil(:)
double precision, allocatable, save :: tempQ(:)
integer*4, allocatable, save :: tempMega(:,:)
double precision,allocatable, save :: tempstiff(:,:) 
character *80 vnom(9)
character*3  mode							

data vnom/'ep. bord','hâme cad','eâme cad','lsem cad','epsa cad',  &
          'hâme rai','eâme rai','lsem rai','epsr rai'/

pi1=pi/180.d00


! ====================================================================================================
!     m1= le nombre de restrictions structurelles pour chaque cas de charge 
!         max=m1tmax pour 1 cas de charge
!
!     m2= le nombre de restrictions géométriques (max 20 par panneau),
!
!     m3= le nombre de restrictions particulières
!
!         soit : rest. sur centre de gravite (subr. coord).
!
!                   si igrav=1 ou 2, on impose 1 restriction  (min ou max)
!                   si igrav=3     , on impose 2 restrictions (min et max)
!
!         soit : ult. strength of ship hull (subr. hull).
!
!                   si irestr=1, on impose 2 restrictions (hogging et sagging)

! ====================================================================================================


index_shift   =0

do nel=1,neto_vector(1)

qn_global    (nel)   = panneau_vector(nel,1).q !qn_vector   (nel,1)
philn_global (nel)   = panneau_vector(nel,1).phil !philn_vector(nel,1)  
!modes_global (nel)   = modes_vector(nel,1)

m2cont_global(nel)         = m2cont_vector(nel,1)	!Conflit 24/04/09 : en commentaire ou pas? TODO check
m4cont_global(nel)         = m4cont_vector(nel,1)
m5cont_global(nel)         = m5cont_vector(nel,1)

m1tabl_global(nel,1:nsolmax) = m1tabl_vector(nel,1:nsolmax,1) !Conflit 24/04/09 : en commentaire ou pas? TODO check

enddo


do iboat=2,nfile
   index_shift   =sum(neto_vector(1:iboat-1))

   do nel=1,neto_vector(iboat)


      qn_global(index_shift+nel)  = panneau_vector(nel,iboat).q ! qn_vector(nel,iboat)
   philn_global(index_shift+nel)  = panneau_vector(nel,iboat).phil !philn_vector(nel,iboat)  
!   modes_global(index_shift+nel)  =modes_vector(nel,iboat)
!  m2cont_global(index_shift+nel)  =m2cont_vector(nel,iboat)

!   do is=1,nsolmax
!      m1tabl_global(index_shift+nel,is) =m1tabl_vector(nel,is,iboat)
!   enddo

   enddo

enddo

! 4.4 calcul des dérivées réduites pour les nr variables (var. indépendantes)
!     -----------------------------------------------------------------------

if(negalt_multisection.gt.0) then

   z1opt_global(1:(nr_global+negalt_multisection))=0.0d00


   do j=1,(nr_global+negalt_multisection)
      do i=1,(nr_global+negalt_multisection)
         z1opt_global(j)=z1opt_global(j)+fiopt_global(i)*dxopt_global(i,j)
      enddo
   enddo


   do j=1,(nr_global+negalt_multisection)
      fiopt_global(j)=z1opt_global(j)
   enddo

   call red1d(fiopt_global,fiopt_global,(nr_global+negalt_multisection),nxi2r_global(1:9,1:neto_global),mega_global(1:negalt_multisection,1:4),negalt_multisection,neto_global,negalt_multisection)

   do k=1,mtot_global
	  call annuld(z1opt_global,(nr_global+negalt_multisection))

      do j=1,(nr_global+negalt_multisection)
         do i=1,(nr_global+negalt_multisection)
            z1opt_global(j)=z1opt_global(j)+cijopt_global(i,k)*dxopt_global(i,j)
         enddo
      enddo

	  call red1d(z1opt_global,z1opt_global,(nr_global+negalt_multisection),nxi2r_global(1:9,1:neto_global),mega_global(1:negalt_multisection,1:4),negalt_multisection,neto_global,negalt_multisection)

      do j=1,nr_global
         cijopt_global(j,k)=z1opt_global(j)
      enddo
   enddo
endif


! 4.5 sauvetage pour conlin (fct objectif)
!     ------------------------------------    
 
!write (iuin,*)           jobj_global                      ! type fct object
!write (iuin,*)  obj_global                       ! fct obj
!write (iuin,*) (fiopt_global(i),i=1,nr_global)   ! dérivées


! 4.7 sauvetage pour conlin (restrictions)
!     ------------------------------------

temp =0.0

   
! 4.8 impressions des données (xi,ximax_global et ximin_global) avant conlin
!     ----------------------------------------------------------------------
!     utilisation des vecteurs temporaires z1opt_global=xi,
!                                          z2opt_global=ximin_global et 
!	 									   z3opt_global=ximax_global

write(iu_opti,50)itera

if(itera.eq.1) then
   
   
   do i=1,nr_global
      z1opt_global(i)=xiopt_global(i)
      z2opt_global(i)=ximin_global(i)
      z3opt_global(i)=ximax_global(i)
   enddo
   
   if(negalt_multisection.gt.0) then
      do i=1,negalt_multisection
   	    j=nxi2_global(mega_global(i,1),mega_global(i,2))
         do k=1,(nr_global+negalt_multisection)-j
            z2opt_global((nr_global+negalt_multisection)-k+1)=z2opt_global((nr_global+negalt_multisection)-k)
            z3opt_global((nr_global+negalt_multisection)-k+1)=z3opt_global((nr_global+negalt_multisection)-k)
            z1opt_global((nr_global+negalt_multisection)-k+1)=z1opt_global((nr_global+negalt_multisection)-k)
         enddo        
      enddo
     
      do i=1,negalt_multisection
   	     j =nxi2_global(mega_global(i,1),mega_global(i,2))
         jj=nxi2_global(mega_global(i,3),mega_global(i,4))
         
   	     if(mega_global(i,1).eq.9) then								
   	        philj =philn_global(mega_global(i,2))
   	        philjj=philn_global(mega_global(i,4))
   	        qj =qn_global(mega_global(i,2))
   	        qjj=qn_global(mega_global(i,4))
   	        wj=dabs(philj)*qj*pi1
   	        wjj=dabs(philjj)*qjj*pi1
   	        
   	         
            
   	        mode=modes_global(mega_global(i,2))
            if(mode.eq.'ee1'.or.mode.eq.'es4'.or.	&
				mode.eq.'EE1'.or.mode.eq.'ES4') then
               xmodej=-1.0
            elseif(mode.eq.'es1'.or.mode.eq.'es3'.or.mode.eq.'ec1'.or.mode.eq.'el3'.or.	&
					mode.eq.'ES1'.or.mode.eq.'ES3'.or.mode.eq.'EC1'.or.mode.eq.'EL3')  then
               xmodej=-0.5
            elseif(mode.eq.'sl1'.or.mode.eq.'sl2'.or.mode.eq.'el2'.or.	&
					mode.eq.'SL1'.or.mode.eq.'SL2'.or.mode.eq.'EL2') then
               xmodej=+0.5
            elseif(mode.eq.'ll1'.or.mode.eq.'LL1') then
               xmodej=+1.0
            else
               xmodej=0.0
            endif
             
   	        mode=modes_global(mega_global(i,4))
   		    if(mode.eq.'ee1'.or.mode.eq.'es4'.or.	&
				mode.eq.'EE1'.or.mode.eq.'ES4') then
               xmodejj=-1.0
            elseif(mode.eq.'es1'.or.mode.eq.'es3'.or.mode.eq.'ec1'.or.mode.eq.'el3'.or.	&
					mode.eq.'ES1'.or.mode.eq.'ES3'.or.mode.eq.'EC1'.or.mode.eq.'EL3')  then
               xmodej=-0.5
               xmodejj=-0.5
            elseif(mode.eq.'sl1'.or.mode.eq.'sl2'.or.mode.eq.'el2'.or.	&
					mode.eq.'SL1'.or.mode.eq.'SL2'.or.mode.eq.'EL2') then
               xmodejj=+0.5
            elseif(mode.eq.'ll1'.or.mode.eq.'LL1') then
               xmodejj=+1.0
            else
               xmodejj=0.0
            endif
                        
 !!!  	        z1opt_global(j)=1./(1./(ega_global(i)*z1opt_global(jj))-xmodejj/(ega_global(i)*wjj)+xmodej/wj)										
 !!!  		    z2opt_global(j)=1./(1./(ega_global(i)*z2opt_global(jj))-xmodejj/(ega_global(i)*wjj)+xmodej/wj)										
 !!!  	        z3opt_global(j)=1./(1./(ega_global(i)*z3opt_global(jj))-xmodejj/(ega_global(i)*wjj)+xmodej/wj)										
   
   	     else        
   	    
         	z1opt_global(j)=z1opt_global(jj)*ega_global(i)
   	        z2opt_global(j)=z2opt_global(jj)*ega_global(i)							
   	        z3opt_global(j)=z3opt_global(jj)*ega_global(i)
   	    	 					
   	     endif
      enddo
   
   endif

   k=0
   do nel=1,neto_global
      if(nvar_global(nel).ne.0) then
         k2=k+nvar_global(nel)
!!!         write (iu_opti,132) nel,(vnom(nxit_global(i,nel)),i=1,nvar_global(nel))
!!!         write (iu_opti,133) (z2opt_global(i),i=k+1,k2)
!!!         write (iu_opti,131) (z1opt_global(i),i=k+1,k2)
!!!         write (iu_opti,135) (z3opt_global(i),i=k+1,k2)
         k=k2
      endif
   enddo

endif


!  ====================================================================
! 5.0 conlin (s vecteur de travail et ms_conlin_global taille max. de ce vecteur)
!  ====================================================================

!iboat=1
!irestr_conlin_global=0
!iweight_conlin_global=0
!iprice_conlin_global=0
!inert_conlin_global=0
!imod_conlin_global=0
if(langue.eq.1)then
   write(6,'(1x,a,i3,a,i3,a)') 'optimiseur conlin ... (iteration no',itera,')'
else
   write(6,'(1x,a,i3,a,i3,a)') 'starting conlin optimizer ... (iteration nr',itera,')'
endif

iconlin = 7*nr_global + 2*mtot_global + 2*(max0(nr_global,mtot_global)+1) + nr_global*mtot_global + 1
call conlin (sopt,ms_conlin_global,iop,non_conlin,neto_global,vnom2_global,			&
				nvarr_global,nxitr_global,m1tabl_global,							&
				m2cont_global,m4cont_global,m5cont_global,igrav_vector,irestr_vector,nsol_vector,	&
				iweight_vector,iprice_vector,inert_vector,imod_vector,				&
				ic_rest_global,mmmax,ratio_rest_global,iconlin)     


if(non_conlin.ge.899) then
   write(*,*) ' error dans conlin (non=',non_conlin,')'
   write(iu_opti,*) ' error dans conlin (non=',non_conlin,')'
   do iboat=1,nfile
      write(iu_14(iboat),*) ' error dans conlin (non=',non_conlin,')'
   enddo
   if(non_conlin.ge.900) stop
endif
	

!  ====================================================================
! 6.0 estimation des restrictions par conlin sur base des dérivées
!  ====================================================================   

do j=1,mtot_global
   tpy=0.d00
   do i=1,nr_global
      tpy= tpy + cijopt_global(i,j)*(sopt(i)-xiopt_global(i))
   enddo
   tpy=cjopt_global(j)+tpy
!   if(iopti.ge.2) write(iu_31(1),*)'estimation de c(',j,') =',tpy    
enddo

!  ==========================================================================
! 7.0 vecteur solution xi pour les n var. de conception (xi1 et xi2 ensemble)
!     et reconstruction des vecteurs complets ximin_global et ximax_global 
!     (n variables)
!  ==================================================================== =====  

if (negalt_multisection.gt.0) then
    call reconstruit(ximin_global,ximin_global,(nr_global+negalt_multisection),nxi2r_global,mega_global(1:negalt_multisection,1:4),	         	&
					negalt_multisection,neto_global,negalt_multisection,ega_global(1:negalt_multisection),philn_global,	&
					qn_global,modes_global)
    call reconstruit(ximax_global,ximax_global,(nr_global+negalt_multisection),nxi2r_global,mega_global(1:negalt_multisection,1:4),		        &
					negalt_multisection,neto_global,negalt_multisection,ega_global(1:negalt_multisection),philn_global,	&
					qn_global,modes_global)
    call reconstruit(         sopt,       sopt,(nr_global+negalt_multisection),nxi2r_global,mega_global(1:negalt_multisection,1:4),		        &
                    negalt_multisection,neto_global,negalt_multisection,ega_global(1:negalt_multisection),philn_global,	&
					qn_global,modes_global)
endif

shift_nr = 0
do iboat=1,nfile
	xicou(1:nr(iboat),iboat) = sopt(1+shift_nr:nr(iboat)+shift_nr)
	ximin(1:nr(iboat),iboat) = ximin_global(1+shift_nr:nr(iboat)+shift_nr)
	ximax(1:nr(iboat),iboat) = ximax_global(1+shift_nr:nr(iboat)+shift_nr)

    if (negalt(iboat).gt.0) then
    
        allocate(tempPhil(neto_vector(iboat)))
        allocate(tempQ(neto_vector(iboat)))
        allocate(tempMega(negalt(iboat),4))

        tempMega(1:negalt(iboat),1:4) = mega(1:negalt(iboat),1:4,iboat)
        tempPhil(1:neto_vector(iboat)) = panneau_vector(1:neto_vector(iboat),iboat).phil
        tempQ(1:neto_vector(iboat)) = panneau_vector(1:neto_vector(iboat),iboat).q
	    call reconstruit(ximin(1:ntot(iboat),iboat),ximin(1:ntot(iboat),iboat),ntot(iboat),		&
		    nxi2(1:9,1:neto_vector(iboat),iboat),tempMega,negalt(iboat),	                    &
		    neto_vector(iboat),negalt(iboat),ega(1:negalt(iboat),iboat),						&
		    tempPhil,tempQ,modes_vector(1:neto_vector(iboat),iboat))
	    call reconstruit(ximax(1:ntot(iboat),iboat),ximax(1:ntot(iboat),iboat),ntot(iboat),		&
    		nxi2(1:9,1:neto_vector(iboat),iboat),tempMega,negalt(iboat),	                    &
	    	neto_vector(iboat),negalt(iboat),ega(1:negalt(iboat),iboat),						&
		    tempPhil,tempQ,modes_vector(1:neto_vector(iboat),iboat))
	    call reconstruit(xicou(1:ntot(iboat),iboat),xicou(1:ntot(iboat),iboat),ntot(iboat),		&
    		nxi2(1:9,1:neto_vector(iboat),iboat),tempMega,negalt(iboat),	                    &
	    	neto_vector(iboat),negalt(iboat),ega(1:negalt(iboat),iboat),						&
		    tempPhil,tempQ,modes_vector(1:neto_vector(iboat),iboat))
		    
		deallocate(tempPhil)
        deallocate(tempQ)
        deallocate(tempMega)
    endif

	shift_nr = shift_nr + nr(iboat)
enddo


! 7.1 impressions
!     ===========
allocate(tempstiff(ngmax_sup,nfile))
do iboat=1,nfile
tempstiff(:,:)=0.d00
	write(iu_31(iboat),59)
	k=0
	kbis=1
	ktris=1
	!Erreur lors de l'écriture des variables de conception. En effet, l'espacement des raidisseurs
	!était mentionné en epsr (largeur collaborante) et non en entr (réel espacement). 
	!Epsr est bien la variable de travail mais non celle de l'utiliasteur, or c'est présisément ici
	!qu'il faut les modifier vu que l'interafce utilisateur lis ces variables d'où les boucles avec kbis et ktris.
	do nel=1,neto_vector(iboat)
		do i=1,nvar(nel,iboat)
		if ((nxit(i,nel,iboat) .eq. 9) .and. (bornestiffmode(nel,iboat) .eq. 'EE1')) then
		tempstiff(kbis,iboat)=xicou(kbis,iboat)
		xicou(kbis,iboat)= bornestiff(nel,iboat)
		endif
		kbis=kbis+1
		enddo
			if(nvar(nel,iboat).ne.0) then
				k2=k+nvar(nel,iboat)
				! noms des xi
				write (iu_31(iboat),132)nel,(vnom(nxit(i,nel,iboat)),i=1,nvar(nel,iboat))
				! var. concept.
				write (iu_31(iboat),131)(xicou(i,iboat),i=k+1,k2)
				k=k2
			endif
		do i=1,nvar(nel,iboat)
		if ((nxit(i,nel,iboat) .eq. 9) .and. (bornestiffmode(nel,iboat) .eq. 'EE1')) then
		xicou(ktris,iboat)=tempstiff(ktris,iboat)
		endif
		ktris=ktris+1
		enddo
	enddo
enddo

deallocate(tempstiff) 
!  ====================================================================
! 8.0 calcul de la fct objectif avec les nouveaux xi c.à.d. s(i).
!  ====================================================================

do iboat=1,nfile
   call recompute_objective_multisection()
enddo

return

!  ====================================================================
! 9.0 les formats
!  ====================================================================


50  format(/t30,'iteration nø ',i3/t30,18(1h*))
59  format(/' les resultats (toutes les n variables) :'/20(1h-)) 
131 format('variables_:',9(e11.4))
132 format('panel nø',i3,2x,9(1x,a8,2x))
133 format('bornes min:',9(e11.4))
135 format('bornes max:',9(e11.4))
  
  
end


subroutine reconstruit(xr,x,ntot,nxi2,mega,negalt,neto,ngmax,ega,philn,qn,modes)
! ========================================================================  
! Vecteur solution xi pour les n var. de conception (xi1 et xi2 ensemble)
! et reconstruction des vecteurs complets ximin_global et ximax_global 
! (n variables)
! ========================================================================  
! Méthode plus ou moins inverse à la subroutine RED1
implicit double precision (a-h,o-z)

double precision x(ntot),xr(ntot),ega(ngmax),philn(neto),qn(neto)
integer*4 nxi2(9,neto),mega(ngmax,4)
character*3 mode,modes(neto)

x(:)=xr(:)
if(negalt.gt.0) then
	do i=1,negalt
		j=nxi2(mega(i,1),mega(i,2))
        do k=1,ntot-j
			x(ntot-k+1)=x(ntot-k)
		enddo
	enddo
  
    do i=1,negalt
		j =nxi2(mega(i,1),mega(i,2))
        jj=nxi2(mega(i,3),mega(i,4))
		if(mega(i,1).eq.9) then
			philj=philn(mega(i,2))
			philjj=philn(mega(i,4))
			qj=qn(mega(i,2))
			qjj=qn(mega(i,4))
			pi1=2.d00*asin(1.d00)/180.d00
			wj=dabs(philj)*qj*pi1
			wjj=dabs(philjj)*qjj*pi1
			mode=modes(mega(i,2))
			if(mode.eq.'EE1'.or.mode.eq.'ES4'.or.mode.eq.'ee1'.or.mode.eq.'es4') then
				xmodej=-1.0
			elseif(mode.eq.'ES1'.or.mode.eq.'ES3'.or.mode.eq.'EC1'.or.mode.eq.'EL3'.or.	&
					mode.eq.'es1'.or.mode.eq.'es3'.or.mode.eq.'ec1'.or.mode.eq.'el3')  then
				xmodej=-0.5
			elseif(mode.eq.'SL1'.or.mode.eq.'SL2'.or.mode.eq.'EL2'.or.	&
					mode.eq.'sl1'.or.mode.eq.'sl2'.or.mode.eq.'el2') then
				xmodej=+0.5
			elseif(mode.eq.'LL1'.or.mode.eq.'ll1') then
	            xmodej=+1.0
		     else
			    xmodej=0.0
			endif
			mode=modes(mega(i,4))
			if(mode.eq.'EE1'.or.mode.eq.'ES4'.or.mode.eq.'ee1'.or.mode.eq.'es4') then
	            xmodejj=-1.0
			elseif(mode.eq.'ES1'.or.mode.eq.'ES3'.or.mode.eq.'EC1'.or.mode.eq.'EL3'.or.	&
					mode.eq.'es1'.or.mode.eq.'es3'.or.mode.eq.'ec1'.or.mode.eq.'el3')  then
				xmodejj=-0.5
			elseif(mode.eq.'SL1'.or.mode.eq.'SL2'.or.mode.eq.'EL2'.or.	&
					mode.eq.'sl1'.or.mode.eq.'sl2'.or.mode.eq.'el2') then
	            xmodejj=+0.5
			elseif(mode.eq.'LL1'.or.mode.eq.'ll1') then
				xmodejj=+1.0
			else
				xmodejj=0.0
			endif
			x(j)=1./(1./(ega(i)*x(jj))-xmodejj/(ega(i)*wjj)+xmodej/wj)
		else
			x(j)=x(jj)*ega(i)
		endif
	enddo
endif

return
end
