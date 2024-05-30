subroutine equal_restrictions()




use param_section


implicit double precision (a-h,o-z)
character*80 buffer

!12.0 lecture des restrictions d'egalite (et impression)
!     =================================================
read(iu_10(iboat),4,end=903) buffer			
read(iu_10(iboat),*,end=903) negalt(iboat)

do i=1,negalt(iboat)
   read(iu_10(iboat),*,end=903) (mega(i,k,iboat),k=1,4),ega(i,iboat)
                              !(mega_(i,k),k=1,4),ega(i,iboat)
enddo

if (idebug.eq.1) then
	write(9997,*)  'equal restrictions '
	write(9997,*)  'mega        ='


	do i=1,negalt(iboat)
		!mega(i,1:4,iboat)=mega_(i,1:4)
		write(9997,*) mega(i,1:4,iboat)
	enddo
endif

!if((iopti.ge.1).and.(itera.eq.0)) then  dad
if((iopti.ge.1)) then
   if(langue.eq.1)then
      write(*,100)'restrictions d''egalite (sr sbega)'
   else
      write(*,100)'equality restrictions (sr sbega)'
   endif

   call sbega()
endif



return


   4 format(a80)                                                                                              
 100 format(1x,a,i8)                                                                                          
!
! erreur de lecture du fichier de donnees (unite iu_10(iboat)) ..........................
  903 if(langue.eq.1)write(*,*)' fichier d''entree corrompu ... sortie.'
      if(langue.eq.2)write(*,*)' data file corrupted ... aborting.'
      stop 1


end
