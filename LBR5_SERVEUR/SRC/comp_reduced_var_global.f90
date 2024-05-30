subroutine comp_reduced_var_global()

use param_section
use PARAM_SECTION_VECTOR

implicit double precision (a-h,o-z)


! ====================================================================================================
!

ms_conlin_global=8*nmax_v_global+3*mmmax+nmax_v_global*mmmax+2*max0(nmax_v_global,mmmax)+3
!
!     ms_conlin_global = dimension maximale pour le vecteur s(1) utile pour la subr.conlin
!     ms_conlin_global =8*n+3*m+n*m+2*max0(n,m)+3
!     ms_conlin_global = 145.103   si n=200 et m=700  (neto=30
!     ms_conlin_global = 813.203   si n=400 et m=2000 (neto=50)
!     ms_conlin_global =5641.403   si n=800 et m=7000 (neto=100)


!     file 301 : contient les données pour conlin.
!     file 302 : contient les dimensions des panneaux pour le calcul				
!                des fcts objectifs (cfr. subr ent et object)
!     file 303 : les sensibilités calculées par les subr. geom, contr et ushull	

iop=iopti

!     indice d'impression (pour subr. conlin)

non_conlin=0      

!     error indicator of the subr. conlin (initially set at 0 )



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
!
! ====================================================================================================

! ====================================================================================================
! calcul de negal_global
! ====================================================================================================

do nel=1,neto_global
   negal_global(nel)=0
   do i=1,negalt_multisection   !negalt_global
      if((mega_global(i,2)).eq.nel) negal_global(nel)=negal_global(nel)+1
   enddo
enddo

          
! ====================================================================================================
! calcul de  nvs_global
! ====================================================================================================

nvs_global(1)=0
do nel=2,neto_global
    nvs_global(nel)=nvs_global(nel-1)+negal_global(nel-1)
enddo

 ntot_global= sum(ntot)
m1tot_global= sum(m1tot) 
m2tot_global= sum(m2tot) 
m3tot_global= sum(m3tot)
m4tot_global= sum(m4tot) 
m5tot_global= sum(m5tot) 

!     mtot = le nombre total de restrictions pour les nsol cas de charge.
!         il faut mtot  < mmax (nombre maximum)

mtot_global=m1tot_global+m2tot_global+m3tot_global+m4tot_global+m5tot_global

itemp = (nfile*mmmax)
if(mtot_global.gt.itemp) then
   write(*,*)   'error: le nbre total de restrictions =',mtot_global,'>',itemp
   write(iu_14(1),*)  'error: le nbre total de restrictions =',mtot_global,'>',itemp
   write(iu_31(1),*)  'error: le nbre total de restrictions =',mtot_global,'>',itemp
   write(*,*) 'stop'
   read(*,*)
   stop
endif


! ====================================================================================================
! calcul de  nxi2_global
! ====================================================================================================

!l=1
!do nel=1,neto_global
!   do k=1,9
!      do i=1,nvar_global(nel)
!         if((nxit_global(i,nel)).eq.k) then
!            nxi2_global(k,nel)=l
!            l=l+1
!            goto 11
!         endif
!         enddo
! 11   continue
!   enddo
!enddo




! ====================================================================================================

if (idebug.eq.1) then
	write(iu_conlin_global,*)  'm1tot =',m1tot_global
	write(iu_conlin_global,*)  'm2tot =',m2tot_global
	write(iu_conlin_global,*)  'm3tot =',m3tot_global
	write(iu_conlin_global,*)  'm4tot =',m4tot_global
	write(iu_conlin_global,*)  'm5tot =',m5tot_global
	write(iu_conlin_global,*)  'm     =',mtot_global
	write(iu_conlin_global,*)  'ntot  =',ntot_global
	write(iu_conlin_global,*)  'negalt=',negalt_global

	do i=1,9
		write(iu_conlin_global,*)  nxit_global(i,1:neto_global)
	enddo

	do i=1,9
		write(iu_conlin_global,*)  nxi2_global(i,1:neto_global)
	enddo

	do i=1,negalt_global
		write(iu_conlin_global,*)  mega_global(i,1:6)
	enddo

	write(iu_conlin_global,*)  'nvar  =',nvar_global(1:neto_global)
	write(iu_conlin_global,*)  'negal =',negal_global(1:neto_global)
	write(iu_conlin_global,*)  'itera =',itera
endif
    
! ====================================================================================================
! 1.   reduction des vecteurs nvar_global et nxit_global en nvarr_global et nxitr_global
! ====================================================================================================



!      soit nr_global le nbre total de variables xi indépendantes

if (ntot_global.eq.0) then
	if (langue.eq.1) then
		write(*,'(a)') '! Optimisation selectionne avec aucune variable de conception !'
		write(*,'(a)') 'Veuillez corriger vos donnees  - appuyer sur ENTER pour poursuivre'
	endif
	if (langue.eq.2) then
		write(*,'(a,i2,a,i2)') '! Optimisation selected with none design variables!!!'
		write(*,'(a)') 'Please correct your data - press ENTER to resume'
	endif
	read(*,*)
	stop
endif

nr_global=ntot_global-negalt_global



do nel=1,neto_global
   nvarr_global(nel)=nvarr_temp_global(nel)-negal_global(nel)   ! nvar_global,nxit_global calculés dans opti_local_to_global
   do k=1,9
      nxitr_global(k,nel)=nxitr_temp_global(k,nel)
   enddo
enddo


if(negalt_multisection.gt.0) then
!   write(iu_31(1),*)
!   write(iu_31(1),*) 'variables de conception (réduites), nxi(reduit)'

   ine=0
   ivar=0

	do l=1,negalt_multisection
		i=mega_global(l,1)
		j=mega_global(l,2) !nel
		!nxitr_global(i,j)=0
		ii=1
		do while (nxitr_global(ii,j).ne.i)
			ii=ii+1
		enddo
		do jj=ii,nvarr_temp_global(j)-1
			nxitr_global(jj,j)=nxitr_global(jj+1,j)
		enddo
		nxitr_global(nvarr_temp_global(j),j)=0
	enddo


!   do nel=1,neto_global                                       ! nø du panneau concerné
!      do i=1,negal_global(nel)             
!         ine=ine+1                                            ! nø de la contr d'égalité
!         kk=nxi2r_global(mega_global(ine,1),nel) - ivar       ! ordre du xi concerné ds panneau concerné
!         do k=kk+1-i,(nvarr_temp_global(nel)-1)               ! on supprime l'indice du xi concerné et on 
!            nxitr_global(k,nel)=nxitr_global(k+1,nel)         ! redécale les autres xi d'un cran
!         enddo
!	     nxitr_global(nvarr_temp_global(nel),nel)=0
!      enddo
!      ivar=ivar+nvarr_global(nel)
!      write(iu_31(1),'(a,i3,a,9i3)')'nxitr(',nel,')=',(nxitr_global(l,nel),l=1,9)
!   enddo                      


endif                           


!  ====================================================================  
! 2.   restrictions d'egalite entre variables xi
!      =====================================================
!      (calcul de dx =dxi1/dxi2)


!if(negalt_global.gt.0) then
if(negalt_multisection.gt.0) then

   do i=1,(nr_global+negalt_multisection) !ntot_global
      do j=1,(nr_global+negalt_multisection) !ntot_global
	     dxopt_global(i,j)=0.0d00
	  enddo
      dxopt_global(i,i)=1.0d00
   enddo
   
   do l=1,negalt_multisection !negalt_global
      i=nxi2r_global(mega_global(l,1),mega_global(l,2))
	  j=nxi2r_global(mega_global(l,3),mega_global(l,4))
	  if (j.eq.0) then
	    itest = 1
		ipanel = mega_global(l,4)
		do while (neto_vector(itest).lt.ipanel)
			ipanel = ipanel - neto_vector(itest)
			itest = itest + 1
		enddo

		write(*,*) 'The variable',mega_global(l,3),' of the panel',ipanel,' of the structure',itest
		write(*,*) 'is not a design variable!'
		write(*,*) 'Please correct your data - press ENTER to resume'
		read(*,*)
		stop
	  endif
      dxopt_global(i,j)=ega_global(l)
   enddo

endif




!  ====================================================================  
! 3.0 préparation des données pour conlin avec les résultats de lbr-5
!  ====================================================================  

!if(itera.eq.1) then
!  write(iu_31(1),20)
!endif

!rewind(iuin)

if(iprint.ge.1) write(*,51)
if(iprint.ge.1) write(*,*)' nr=',nr_global,' m1=',m1tot_global,'et m2=',m2tot_global,'et m4=',m4tot_global


! 3.1 données générales
!     =================

tol=0.0d00
cut=0.0d00
prec=0.0d00
pds=0.0d00
!jt=0
!jobj=0    ! déjà fait dans opti_local_to_global


!write (iuin,*) nr_global,mtot_global      ! nr=n*nsol, m=nbr restrict.
!write (iuin,*) tol,cut,prec,pds

if (negalt_multisection.gt.0) then
    call red1d(xiopt_global,xiopt_global,(nr_global+negalt_multisection),nxi2r_global(1:9,1:neto_global),mega_global(1:negalt_multisection,1:4),negalt_multisection,neto_global,negalt_multisection) 
    call red1d(ximin_global,ximin_global,(nr_global+negalt_multisection),nxi2r_global(1:9,1:neto_global),mega_global(1:negalt_multisection,1:4),negalt_multisection,neto_global,negalt_multisection)				
    call red1d(ximax_global,ximax_global,(nr_global+negalt_multisection),nxi2r_global(1:9,1:neto_global),mega_global(1:negalt_multisection,1:4),negalt_multisection,neto_global,negalt_multisection)
endif
	

!write (iuin,'(9e14.7)') (xiopt_global(i),i=1,nr_global)      
!write (iuin,'(9e14.7)') (ximin_global(i),i=1,nr_global)
!write (iuin,'(9e14.7)') (ximax_global(i),i=1,nr_global)

!write (iuin,*) (xiopt_global(i),i=1,nr_global)      
!write (iuin,*) (ximin_global(i),i=1,nr_global)
!write (iuin,*) (ximax_global(i),i=1,nr_global)

return

!  ====================================================================  
! 9.0 les formats
!     ===========

32 format('panel nø ',i3,9(1x,a8,3x))
51 format('initialisation des données dans opti'/40(1h+)) 

end
