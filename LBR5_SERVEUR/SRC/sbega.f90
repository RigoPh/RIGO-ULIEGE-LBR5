subroutine sbega()




use param_section
!use param_cout
!use param_opti_local


implicit double precision(a-h,o-z)

! ====================================================================================================
!     subroutine sbega
!     =================
!	cette subroutine est relative aux restrictions d'�galit�s [xi1= kcst * xi2 ]
!     ---------------------------------------------------------------------------
!     maximum 100 restrictions d'egalite
!
! 	donn�es : nvar(nel,iboat)	        nbre de variables de conception pour le panneau nel
!
!             nxit(9,nel,iboat)         liste des variables de conception (1 � 9) du panneau nel
!
!	lecture : negalt(iboat)	            nbre de contr. d'�galit� pour toute la structure	
!
!
!		      mega(negalt,4,iboat)      xi1= kcst * xi2  avec xi1 du panneau nel1 
!                                                   xi2 du panneau nel2
!                                                   kste = xi1/xi2 = constante (real)
!
!
!		           - mega(j,1,iboat)    xi1(=1�9), le n� de r�f. de la var. xi1 (var. d�pendant)
!
!
!                                                      ex: pour haig, mega(j,1,iboat)=2 
!                                                          pour epsr, mega(j,1,iboat)=9
!
!
!	  	           - mega(j,2,iboat)    n� du panneau de cette variable xi1
!
!		           - mega(j,3,iboat)    xi2(=1�9), le n� de r�f. de la var. xi2 (var. ind�pendant)
!
!		           - mega(j,4,iboat)    n� du panneau de cette variable xi2
!
!
!
!		      ega(j,iboat,iboat)		la constante = xi1/xi2 
!
!
!
!
!
!   calcul� : negal(nel,iboat)   nbre de restr. d'�galit� pour le panneau nel
!
!		      nvs(nel,iboat)	 nbre de restr. d'�galit� pour les panneaux 1 � nel-1    	
!
!             nxi2(9,nel,iboat)  n� d'ordre des variables xi (1�9) du panneau nel
!                                au niveau global de la structure (cad pour les neto panneaux)
!
!
!                                si = 0 : signifie que ce n'est pas une variable de conception.
!
!     cr��   :  4- 9-95 
!
!     modifi� : 19- 3-97	  test de v�rification sur l'existance des xi choisis
!                6-12-99				             
! ====================================================================================================



! ====================================================================================================
!     impressions des donn�es
! ====================================================================================================

write(iu_31(iboat),2) 'EQUALITY CONSTRAINTS'

write(iu_31(iboat),*)' nbr de restr. d''�galit�=',negalt(iboat)
if ((impr2.ge.-2).and.(negalt(iboat).ne.0)) then	
   write(iu_31(iboat),*)
   write(iu_31(iboat),*)' n�    xi1  nel1    xi2  nel2    xi1/xi2'

   do i=1,negalt(iboat)
   write(iu_31(iboat),5)i,(mega(i,k,iboat),k=1,4),ega(i,iboat)
   enddo
endif		

! ====================================================================================================
!     v�rifications des donn�es
! ====================================================================================================

it=0
ixi=0

do i=1,negalt(iboat)
   if(mega(i,2,iboat).eq.mega(i,4,iboat))then
      if(mega(i,1,iboat).eq.mega(i,3,iboat))then
         write(*,*) 'error 1 ds les donnees des contraintes d''egalite'
         write(iu_31(iboat),*)'error1 ds les donnees des contraintes d''egalite'
         write(iu_31(iboat),*) 'egalit� n�',i
         write(iu_31(iboat),*) 'mega(i,1�4)=',(mega(i,kk,iboat),kk=1,4)
     
   		  write(iu_14(iboat),*)'error1 ds les donnees des contraintes d''egalite'
         write(iu_14(iboat),*) 'egalit� n�',i													
         write(iu_14(iboat),*) 'mega(i,1�4)=',(mega(i,kk,iboat),kk=1,4)			
   
   		  write(*,*) 'stop'
		  read(*,*)
         stop
   	 endif	
   endif

   if(mega(i,2,iboat).lt.it)then
      write(*,*)  'error 2 : contraintes d''�galit� mal class�es'
      write(iu_31(iboat),*)'error 2 : contraintes d''�galit� mal class�es'
      write(iu_31(iboat),*) 'egalit� n�',i
      write(iu_31(iboat),*) 'mega(i,1�4)=',(mega(i,kk,iboat),kk=1,4)
           
		  write(iu_14(iboat),*)  'error 2 : contraintes d''�galit� mal class�es'			
      write(iu_14(iboat),*) 'egalit� n�',i													
      write(iu_14(iboat),*) 'mega(i,1�4)=',(mega(i,kk,iboat),kk=1,4)			
		  
		  write(*,*) 'stop'
		  read(*,*)
      stop
   endif

   if(mega(i,2,iboat).eq.it)then
      if(mega(i,1,iboat).le.ixi)then
         write(*,*)   'error 3: contraintes d''�galit� mal class�es'
         write(iu_31(iboat),*) 'error 3: contraintes d''�galit� mal class�es'
         write(iu_31(iboat),*) 'egalit� n�',i
         write(iu_31(iboat),*) 'mega(i,1�4)=',(mega(i,kk,iboat),kk=1,4)
           
		     write(iu_14(iboat),*) 'error 3: contraintes d''�galit� mal class�es'
         write(iu_14(iboat),*) 'egalit� n�',i													
         write(iu_14(iboat),*) 'mega(i,1�4)=',(mega(i,kk,iboat),kk=1,4)			
 
		     write(*,*) 'stop'
			 read(*,*)
         stop
      endif
	 endif
	
	 it =mega(i,2,iboat)	
	 ixi=mega(i,1,iboat)		

enddo


! ====================================================================================================
! calcul de negal(nel)
! ====================================================================================================

do nel=1,neto
   negal(nel,iboat)=0
   do i=1,negalt(iboat)
      if((mega(i,2,iboat)).eq.nel) negal(nel,iboat)=negal(nel,iboat)+1
   enddo
enddo

          
! ====================================================================================================
! calcul de  nvs(nel)
! ====================================================================================================

nvs(1,iboat)=0
do nel=2,neto
    nvs(nel,iboat)=nvs(nel-1,iboat)+negal(nel-1,iboat)
enddo


! ====================================================================================================
!	calcul de  nxi2(9,nel,iboat)
! ====================================================================================================

l=1
do nel=1,neto
   do k=1,9
      do i=1,nvar(nel,iboat)
         if((nxit(i,nel,iboat)).eq.k) then
            nxi2(k,nel,iboat)=l
            l=l+1
            goto 11
         endif
         if((nxit(i,nel,iboat)).gt.k) goto 11
         enddo
 11   continue
   enddo
enddo

! ====================================================================================================
! v�rification de l'existance des variables
! ====================================================================================================

do i=1,negalt(iboat)

   ic=nxi2(mega(i,1,iboat),mega(i,2,iboat),iboat) * nxi2(mega(i,3,iboat),mega(i,4,iboat),iboat)
   if(ic.eq.0)then
      write(*,*) 'error : restriction d''�galit� n�',i,' incorrecte'
      write(*,*) '        car faisant intervenir une variable de conception non selectionn�e'
      write(iu_31(iboat),*) 'error : restriction d''�galit� n�',i,' incorrecte'
      write(iu_31(iboat),*) '        car faisant intervenir une variable de conception non selectionn�e'
      write(iu_14(iboat),*) 'error : restriction d''�galit� n�',i,' incorrecte'		
      write(iu_14(iboat),*) '        car faisant intervenir une variable de conception non selectionn�e'						
	     
	    write(*,*) 'stop'
		read(*,*)
      stop
   endif
enddo


! ====================================================================================================
! impressions de negal,nvs et nxi2
! ====================================================================================================

if(impr2.ge.-2) then	
   write(iu_31(iboat),*)
   write(iu_31(iboat),*)'nel,negal, nvs,     nxi2'
   
   do nel=1,neto
      write(iu_31(iboat),'(i3,i4,i3,t14,9i4)') nel,negal(nel,iboat),nvs(nel,iboat),(nxi2(i,nel,iboat),i=1,9)
   enddo	
endif					

return


write(*,*) 'erreur � la lecture des restrictions d''�galit�'
write(iu_14(iboat),*) 'error � la lecture des restrictions d''�galit�'	
write(*,*) 'stop'
read(*,*)
stop
	
! ====================================================================================================
!	formats
! ====================================================================================================

2 format(/' donnees relatives aux restrictions d''egalite'/t2,50(1h*)/a80/)
5 format(1x,i4,3x,i2,3x,i3,6x,i2,3x,i2,5x,e14.7)
end
