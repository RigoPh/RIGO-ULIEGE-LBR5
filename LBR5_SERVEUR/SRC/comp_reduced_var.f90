subroutine comp_reduced_var()

use param_section
use PARAM_SECTION_VECTOR
 

implicit double precision (a-h,o-z)

! ====================================================================================================
!

!ms_conlin(iboat)=8*nvmax+3*mmmax+nvmax*mmmax+2*max0(nvmax,mmmax)+3
!
! ms_conlin(iboat) = dimension maximale pour le vecteur s(1) utile pour la subr.conlin
! ms_conlin(iboat) =8*n+3*m+n*m+2*max0(n,m)+3
! ms_conlin(iboat) = 145.103   si n=200 et m=700  (neto=30
! ms_conlin(iboat) = 813.203   si n=400 et m=2000 (neto=50)
! ms_conlin(iboat) =5641.403   si n=800 et m=7000 (neto=100)

! file 301 : contient les données pour conlin.
! file 302 : contient les dimensions des panneaux pour le calcul				
!                des fcts objectifs (cfr. subr ent et object)
! file 303 : les sensibilités calculées par les subr. geom, contr et ushull	

iop=iopti

! indice d'impression (pour subr. conlin)

non_conlin=0      

! error indicator of the subr. conlin (initially set at 0 )
! m1= le nombre de restrictions structurelles pour chaque cas de charge 
! max=m1tmax pour 1 cas de charge
!
! m2= le nombre de restrictions géométriques (max 20 par panneau),
!
! m3= le nombre de restrictions particulières
!
! m4 = le nombre de restrictions de sloshing
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

mtot(iboat) = 0					
m1tot(iboat) = 0					
m2tot(iboat) = 0					
m3tot(iboat) = 0
m4tot(iboat) = 0
m5tot(iboat) = 0

if(irestr_vector(iboat).ge.1)		   m3tot(iboat)=m3tot(iboat)+2
if(igrav_vector(iboat)==1)		       m3tot(iboat)=m3tot(iboat)+1
if(igrav_vector(iboat)==2)		       m3tot(iboat)=m3tot(iboat)+1
if(igrav_vector(iboat)==3)             m3tot(iboat)=m3tot(iboat)+2
if(iweight_vector(iboat).ne.0)         m3tot(iboat)=m3tot(iboat)+1	
if(iprice_vector(iboat).ne.0)          m3tot(iboat)=m3tot(iboat)+1	
if(inert_vector(iboat).ne.0)           m3tot(iboat)=m3tot(iboat)+1	
if(imod_vector(iboat).ne.0)	           m3tot(iboat)=m3tot(iboat)+1

do nel=1, neto_vector(iboat)
   m1tot(iboat) = m1tot(iboat) + panneau_vector(nel,iboat).m1cont
   m2tot(iboat) = m2tot(iboat) + m2cont_vector(nel,iboat)
   m4tot(iboat) = m4tot(iboat) + m4cont_vector(nel,iboat)
   m5tot(iboat) = m5tot(iboat) + m5cont_vector(nel,iboat)
enddo

!     mtot = le nombre total de restrictions pour les nsol cas de charge.
!         il faut mtot  < mmax (nombre maximum)

mtot(iboat)=m1tot(iboat)+m2tot(iboat)+m3tot(iboat)+m4tot(iboat)+m5tot(iboat)
   
if(mtot(iboat).gt.mmmax) then
   write(*,*)  'error: le nbre total de restrictions =',mtot(iboat),'>',mmmax
   write(iu_14(iboat),*)  'error: le nbre total de restrictions =',mtot(iboat),'>',mmmax
   write(iu_31(iboat),*)  'error: le nbre total de restrictions =',mtot(iboat),'>',mmmax
   write(*,*) 'stop'
   read(*,*)
   stop
endif

! ====================================================================================================

if (idebug_conlin.eq.1) then
	write(iu_conlin(iboat),*)  'm1tot =',m1tot(iboat)
	write(iu_conlin(iboat),*)  'm2tot =',m2tot(iboat)
	write(iu_conlin(iboat),*)  'm3tot =',m3tot(iboat)
	write(iu_conlin(iboat),*)  'm4tot =',m4tot(iboat)
	write(iu_conlin(iboat),*)  'm5tot =',m5tot(iboat)
	write(iu_conlin(iboat),*)  'm     =', mtot(iboat)
	write(iu_conlin(iboat),*)  'ntot  =',ntot(iboat)
	write(iu_conlin(iboat),*)  'negalt=',negalt(iboat)

	write(iu_conlin(iboat),*)  'nxit  ='

	do i=1,9
		write(iu_conlin(iboat),*)  nxit(i,1:nmax,iboat)
	enddo

	write(iu_conlin(iboat),*)  'nxi2  ='

	do i=1,9
		write(iu_conlin(iboat),*)  nxi2(i,1:nmax,iboat)
	enddo

	write(iu_conlin(iboat),*)  'mega   ='

	do i=1,negalt(iboat)
		write(iu_conlin(iboat),*)  mega(i,1:4,iboat)
	enddo

	write(iu_conlin(iboat),*)  'nvar  =',nvar (1:nmax,iboat)
	write(iu_conlin(iboat),*)  'negal =',negal(1:nmax,iboat)
	write(iu_conlin(iboat),*)  'itera =',itera
endif

!  ====================================================================
! 1.   reduction des vecteurs nvar et nxit en nvarr et nxitr
!      =====================================================
!      soit nr le nbre total de variables xi indépendantes

!negalt_global=sum(negalt(iboat))+negalt_section
!nr_global=ntot_global-negalt_global

nr(iboat)=ntot(iboat)-negalt(iboat)

do nel=1,neto_vector(iboat)
   nvarr(nel,iboat)=nvar(nel,iboat)-negal(nel,iboat)
   do k=1,9
      nxitr(k,nel,iboat)=nxit(k,nel,iboat)
   enddo
enddo
   
if(negalt(iboat).gt.0) then
   write(iu_31(iboat),*)
   write(iu_31(iboat),*) 'variables de conception (réduites), nxi(reduit)'
   ine=0
   ivar=0

   do nel=1,neto_vector(iboat)                         ! nø du panneau concerné
      do i=1,negal(nel,iboat)             
         ine=ine+1                       ! nø de la contr d'égalité
         kk=nxi2(mega(ine,1,iboat),nel,iboat) - ivar        ! ordre du xi concerné ds panneau concerné
         do k=kk+1-i,(nvar(nel,iboat)-1)                    ! on supprime l'indice du xi concerné et on 
            nxitr(k,nel,iboat)=nxitr(k+1,nel,iboat)         ! redécale les autres xi d'un cran

         enddo
	     nxitr(nvar(nel,iboat),nel,iboat)=0
      enddo
      ivar=ivar+nvar(nel,iboat)
      write(iu_31(iboat),'(a,i3,a,9i3)')'nxitr(',nel,')=', &
                                  (nxitr(l,nel,iboat),l=1,9)
   enddo                      
endif


l=1
do nel=1,neto_vector(iboat)
   do k=1,9
      do i=1,nvarr(nel,iboat)
         if((nxitr(i,nel,iboat)).eq.k) then
            nxi2r(k,nel,iboat)=l
            l=l+1
            goto 11
         endif
         if((nxitr(i,nel,iboat)).gt.k) goto 11
         enddo
 11   continue
   enddo
enddo




!  ====================================================================  
! 2.   restrictions d'egalite entre variables xi
!      =====================================================
!      (calcul de dx =dxi1/dxi2)


if(negalt(iboat).gt.0) then
   
   do i=1,ntot(iboat)
      do j=1,ntot(iboat)
         dxopt(i,j,iboat)=0.0d00
      enddo
      dxopt(i,i,iboat)=1.0d00
   enddo

   do l=1,negalt(iboat)
      i=nxi2(mega(l,1,iboat),mega(l,2,iboat),iboat)
      j=nxi2(mega(l,3,iboat),mega(l,4,iboat),iboat)
      dxopt(i,j,iboat)=ega(l,iboat)
   enddo
  
endif   

if (idebug_conlin.eq.1) then
	write(iu_conlin(iboat),*)  'ega   =',ega(1:nmax,iboat)
	write(iu_conlin(iboat),*)  'dxopt ='
	do i=1,ntot(iboat)
		write(iu_conlin(iboat),*)  dxopt(i,1:ntot(iboat),iboat)
	enddo

	write(iu_conlin(iboat),*)  'nxitr ='!

	do i=1,9
		write(iu_conlin(iboat),*)  nxitr(i,1:nmax,iboat)
	enddo
  
	write(iu_conlin(iboat),*)  'xicou =',xicou(1:nmax,iboat)  
endif

!  ====================================================================  
! 3.0 préparation des données pour conlin avec les résultats de lbr-5
!  ====================================================================  
   
write(iu_31(iboat),20)
   
! rewind(iuin(iboat)) !ne se fait pas par structure
   
if(iprint.ge.1) write(*,51)
if(iprint.ge.1) write(*,*)' nr=',nr(iboat),' m1=',m1tot(iboat),'et m2=',m2tot(iboat),'et m4=',m4tot(iboat),'et m54=',m5tot(iboat)
   
   
! 3.1 données générales
!     =================   

jt_conlin(iboat)  =0
jobj(iboat)=0
   	
! nr=n*nsol, m=nbr restrict.	
   
!write (iuin(iboat),*) nr(iboat),mtot(iboat)  !ne se fait pas par structure
!write (iuin(iboat),*) tol_conlin(iboat),cut_conlin(iboat),prec_conlin(iboat),pds_conlin(iboat)  !ne se fait pas par structure
   
! 3.2 variables de conception xi et les bornes
!     ========================================
   
! tous les xi (1 à n)
   
do i=1,ntot(iboat)
   xiopt(i,iboat)=xicou(i,iboat)    
enddo              
   	
!   write(iu_conlin(iboat),*)  'xiopt =',xiopt(1:ntot(iboat),iboat)
   	
! réduction à nr
if (idebug_conlin.eq.1) then
	write(9997,*)  'before red1 '
	write(9997,*)  'mega        ='
   
	do i=1,ngmax
		write(9997,*) mega(i,1:4,iboat)
	enddo
   
	write(9997,*)  'mega_test   ='
	write(9997,*) mega(1:ngmax,1:4,iboat)
   
	write(9997,*)  'nxi2_test   ='
	write(9997,*)  nxi2(1:9,1:nmax,iboat)
endif

call red1d(xiopt(1:ntot(iboat),iboat),xiopt(1:ntot(iboat),iboat),ntot(iboat),nxi2(1:9,1:nmax,iboat),mega(1:ngmax,1:4,iboat),negalt(iboat),neto_vector(iboat),ngmax) 
call red1d(ximin(1:ntot(iboat),iboat),ximin(1:ntot(iboat),iboat),ntot(iboat),nxi2(1:9,1:nmax,iboat),mega(1:ngmax,1:4,iboat),negalt(iboat),neto_vector(iboat),ngmax)				
call red1d(ximax(1:ntot(iboat),iboat),ximax(1:ntot(iboat),iboat),ntot(iboat),nxi2(1:9,1:nmax,iboat),mega(1:ngmax,1:4,iboat),negalt(iboat),neto_vector(iboat),ngmax)		

! sauvetage pour conlin des variables de conception xi et des bornes
   
!write (iuin(iboat),'(9e14.7)') (xiopt(i,iboat),i=1,nr(iboat)) !ne se fait pas par structure
!write (iuin(iboat),'(9e14.7)') (ximin(i,iboat),i=1,nr(iboat)) !ne se fait pas par structure
!write (iuin(iboat),'(9e14.7)') (ximax(i,iboat),i=1,nr(iboat)) !ne se fait pas par structure
   
if (idebug_conlin.eq.1) then
	write(iu_conlin(iboat),*)  'xiopt =',xiopt(1:nmax*9,iboat)
	write(iu_conlin(iboat),*)  'ximin =',ximin(1:nmax*9,iboat)
	write(iu_conlin(iboat),*)  'ximax =',ximax(1:nmax*9,iboat)
endif


!else

!   ximin_global(1:ntot(1))=ximin(1:ntot(1),1)
!   ximin_global(sum(ntot(1:iboat-1))+1:sum(ntot(1:iboat-1)+ntot(iboat)))  &
!				          =ximin(1:ntot(iboat),iboat)
   
!   ximax_global(1:ntot(1))=ximax(1:ntot(1),1)
!   ximax_global(sum(ntot(1:iboat-1))+1:sum(ntot(1:iboat-1)+ntot(iboat)))  &
!				          =ximax(1:ntot(iboat),iboat)


!   write(7790,*) 'ximin=',ximin(1:ntot(iboat),iboat)
!   write(7791,*) 'ximin=',ximin_global
   
!endif


return




!  ====================================================================  
! 9.0 les formats
!     ===========
20 format(///' entree dans le module optimisation (conlin-lbr5)'/  &
          50(1h*)//                                                &
         ' conlin optimizer                      ',                &
         ' logiciel des bordages raidis'/                          &
         ' version 2.0                            ',               &
         ' version : l.b.r.- 5.0'/                                 &
         ' author: prof. c. fleury                ',               &
         ' author: dr. ph. rigo'/)
32 format('panel nø ',i3,9(1x,a8,3x))
33 format('bornes min:',9(e11.4,1x))
34 format('variables_:',9(e11.4,1x))
35 format('bornes max:',9(e11.4,1x))
51 format('initialisation des données dans opti'/40(1h+)) 
53 format('les nouvelles donnees'/25(1h-))

end
