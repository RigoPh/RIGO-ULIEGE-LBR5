! ---------------------------------------------------------------------------------                                                        
!subroutine de relecture (file iu_27(iboat)) des valeurs extrêmes des déplacements		
!et des contraintes sauvées dans les subr ecri et ecri3 (subr extrem)	
!
!modifié : 4-2-2004                           créer: 13-3-95 (ph. rigo)
!
! 20-11-2000  impression des resultats en write(iu_12(iboat),..):fichier résultat sol2
!  4-12-2002  19(22) au lieu de 12 variables fournies.
! 04-02-2004  ajout variables pour épontilles.
!
! ---------------------------------------------------------------------------------

subroutine analys()

use param_section

implicit double precision (a-h,o-z) 

double precision, allocatable, save :: v(:,:)
double precision, allocatable, save :: u(:,:)
double precision, allocatable, save :: w(:,:)
integer*4       , allocatable, save :: iv(:,:)
integer*4       , allocatable, save :: iu(:,:)
integer*4       , allocatable, save :: iw(:,:)
double precision, allocatable, save :: sborx(:,:)
double precision, allocatable, save :: sbory(:,:)
double precision, allocatable, save :: tau(:,:)
integer*4       , allocatable, save :: isborx(:,:)
integer*4       , allocatable, save :: isbory(:,:)
integer*4       , allocatable, save :: itau(:,:)
double precision, allocatable, save :: sbor(:,:)
double precision, allocatable, save :: sbor1(:,:)
double precision, allocatable, save :: sbor2(:,:)
integer*4       , allocatable, save :: isbor(:,:)
integer*4       , allocatable, save :: isbor1(:,:)
integer*4       , allocatable, save :: isbor2(:,:)

double precision, allocatable, save :: sjas(:,:)
double precision, allocatable, save :: sjab(:,:)
double precision, allocatable, save :: tjas(:,:)
double precision, allocatable, save :: tjab(:,:)
integer*4       , allocatable, save :: isjas(:,:)
integer*4       , allocatable, save :: isjab(:,:)
integer*4       , allocatable, save :: itjas(:,:)
integer*4       , allocatable, save :: itjab(:,:)
double precision, allocatable, save :: ajas(:,:)
double precision, allocatable, save :: ajab(:,:)
double precision, allocatable, save :: asem(:,:)
integer*4       , allocatable, save :: iajas(:,:)
integer*4       , allocatable, save :: iajab(:,:)
integer*4       , allocatable, save :: iasem(:,:)
double precision, allocatable, save :: rjas(:,:)
double precision, allocatable, save :: rjab(:,:)
double precision, allocatable, save :: rsem(:,:)
integer*4       , allocatable, save :: irjas(:,:)
integer*4       , allocatable, save :: irjab(:,:)
integer*4       , allocatable, save :: irsem(:,:)

double precision, allocatable, save :: sborxi(:,:)
double precision, allocatable, save :: sboryi(:,:)
integer*4       , allocatable, save :: isborxi(:,:)
integer*4       , allocatable, save :: isboryi(:,:)
double precision, allocatable, save :: sjasi(:,:)
double precision, allocatable, save :: sjabi(:,:)
double precision, allocatable, save :: tjasi(:,:)
double precision, allocatable, save :: tjabi(:,:)
integer*4       , allocatable, save :: isjasi(:,:)
integer*4       , allocatable, save :: isjabi(:,:)
integer*4       , allocatable, save :: itjasi(:,:)
integer*4       , allocatable, save :: itjabi(:,:)
double precision, allocatable, save :: ajasi(:,:)
double precision, allocatable, save :: ajabi(:,:)
double precision, allocatable, save :: asemi(:,:)
integer*4       , allocatable, save :: iajasi(:,:)
integer*4       , allocatable, save :: iajabi(:,:)
integer*4       , allocatable, save :: iasemi(:,:)

integer*4       , allocatable, save :: i4(:),i5(:),i6(:)

allocate (v(neto,2))
allocate (u(neto,2))
allocate (w(neto,2))
allocate (iv(neto,4))
allocate (iu(neto,4))
allocate (iw(neto,4))
allocate (sborx(neto,2))
allocate (sbory(neto,2))
allocate (tau(neto,2))
allocate (isborx(neto,4))
allocate (isbory(neto,4))
allocate (itau(neto,4))
allocate (sbor(neto,2))
allocate (sbor1(neto,2))
allocate (sbor2(neto,2))
allocate (isbor(neto,4))
allocate (isbor1(neto,4))
allocate (isbor2(neto,4))

allocate (sjas(neto,2))
allocate (sjab(neto,2))
allocate (tjas(neto,2))
allocate (tjab(neto,2))
allocate (isjas(neto,4))
allocate (isjab(neto,4))
allocate (itjas(neto,4))
allocate (itjab(neto,4))
allocate (ajas(neto,2))
allocate (ajab(neto,2))
allocate (asem(neto,2))
allocate (iajas(neto,4))
allocate (iajab(neto,4))
allocate (iasem(neto,4))
allocate (rjas(neto,2))
allocate (rjab(neto,2))
allocate (rsem(neto,2))
allocate (irjas(neto,4))
allocate (irjab(neto,4))
allocate (irsem(neto,4))

allocate (sborxi(neto,2))
allocate (sboryi(neto,2))
allocate (isborxi(neto,4))
allocate (isboryi(neto,4))
allocate (sjasi(neto,2))
allocate (sjabi(neto,2))
allocate (tjasi(neto,2))
allocate (tjabi(neto,2))
allocate (isjasi(neto,4))
allocate (isjabi(neto,4))
allocate (itjasi(neto,4))
allocate (itjabi(neto,4))
allocate (ajasi(neto,2))
allocate (ajabi(neto,2))
allocate (asemi(neto,2))
allocate (iajasi(neto,4))
allocate (iajabi(neto,4))
allocate (iasemi(neto,4))

allocate (i4(nmax))
allocate (i5(nmax))
allocate (i6(nmax))

v(:,:) = 0.0d00
u(:,:) = 0.0d00
w(:,:) = 0.0d00
iv(:,:) = 0
iu(:,:) = 0
iw(:,:) = 0
sborx(:,:) = 0.0d00
sbory(:,:) = 0.0d00
tau(:,:) = 0.0d00
isborx(:,:) = 0
isbory(:,:) = 0
itau(:,:) = 0
sbor(:,:) = 0.0d00
sbor1(:,:) = 0.0d00
sbor2(:,:) = 0.0d00
isbor(:,:) = 0
isbor1(:,:) = 0
isbor2(:,:) = 0

sjas(:,:) = 0.0d00
sjab(:,:) = 0.0d00
tjas(:,:) = 0.0d00
tjab(:,:) = 0.0d00
isjas(:,:) = 0
isjab(:,:) = 0
itjas(:,:) = 0
itjab(:,:) = 0
ajas(:,:) = 0.0d00
ajab(:,:) = 0.0d00
asem(:,:) = 0.0d00
iajas(:,:) = 0
iajab(:,:) = 0
iasem(:,:) = 0
rjas(:,:) = 0.0d00
rjab(:,:) = 0.0d00
rsem(:,:) = 0.0d00
irjas(:,:) = 0
irjab(:,:) = 0
irsem(:,:) = 0

sborxi(:,:) = 0.0d00
sboryi(:,:) = 0.0d00
isborxi(:,:) = 0
isboryi(:,:) = 0
sjasi(:,:) = 0.0d00
sjabi(:,:) = 0.0d00
tjasi(:,:) = 0.0d00
tjabi(:,:) = 0.0d00
isjasi(:,:) = 0
isjabi(:,:) = 0
itjasi(:,:) = 0
itjabi(:,:) = 0
ajasi(:,:) = 0.0d00
ajabi(:,:) = 0.0d00
asemi(:,:) = 0.0d00
iajasi(:,:) = 0
iajabi(:,:) = 0
iasemi(:,:) = 0

i4(:) = 0
i5(:) = 0
i6(:) = 0

!dimension    v(neto,2), u(neto,2), w(neto,2),  &                            
!            iv(neto,4),iu(neto,4),iw(neto,4),  &                          
! sborx(neto,2), sbory(neto,2),   tau(neto,2),  &
!isborx(neto,4),isbory(neto,4),  itau(neto,4),  &			  
!  sbor(neto,2), sbor1(neto,2), sbor2(neto,2),  &
! isbor(neto,4),isbor1(neto,4),isbor2(neto,4)

!dimension                                                  &					  
!  sjas(neto,2), sjab(neto,2), tjas(neto,2), tjab(neto,2),  &
! isjas(neto,4),isjab(neto,4),itjas(neto,4),itjab(neto,4),  &
!  ajas(neto,2), ajab(neto,2), asem(neto,2),                &
! iajas(neto,4),iajab(neto,4),iasem(neto,4),                &     			
!  rjas(neto,2), rjab(neto,2), rsem(neto,2),                &   		
! irjas(neto,4),irjab(neto,4),irsem(neto,4)                 
                                                           
! caract pour épontille		

!dimension                          &                  
!   sborxi(neto,2), sboryi(neto,2), &						
!  isborxi(neto,4),isboryi(neto,4), &					
!    sjasi(neto,2), sjabi(neto,2), tjasi(neto,2), tjabi(neto,2),   &
!   isjasi(neto,4),isjabi(neto,4),itjasi(neto,4),itjabi(neto,4),   &
!    ajasi(neto,2), ajabi(neto,2), asemi(neto,2),		              &
!   iajasi(neto,4),iajabi(neto,4),iasemi(neto,4)                    			

!dimension i4(nmax),i5(nmax),i6(nmax)							
                          		

!     z(i,1) = (vmin) valeur minimale de z en (x,y)=(ixmin,iymin) pour le panneau i     
!     z(i,2) = (vmax) valeur maximale de z en (x,y)=(ixmax,iymax) pour le panneau i 
!     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!     vmin	ixmin	iymin		vmax	ixmax	iymax
!     z(i,1)	iz(i,1)	iz(i,2)		z(i,2)	iz(i,3)	iz(i,4)
! ------------------------------------------------------------------------------------

!	call annuld(u,2*neto)
!	call annuld(v,2*neto)
!	call annuld(w,2*neto)

! = nombre total d'épontilles dans la structure
nelo= 0    	
! = nombre total de plaques dans la structure		
neta = 0   

i1 = 0															
i2 = 0															
i3 = 0															
do 2 i=1,neto
if(itype(i).ne.5) then                                        
   i1 = i1 + 1	
   											

20 read(iu_27(iboat),*,end=1) ivar   !saved in ecri
   if(ivar.eq.99) goto 30					
   !1  v
   if(ivar.eq.1) then											 
      read(iu_27(iboat),*)v(i,1),iv(i,1),iv(i,2),v(i,2),iv(i,3),iv(i,4)	
   !2  u 
   else if (ivar.eq.2)  then						
      read(iu_27(iboat),*)u(i,1),iu(i,1),iu(i,2),u(i,2),iu(i,3),iu(i,4)	
   !3  w    			
   else if (ivar.eq.3)  then								
      read(iu_27(iboat),*)w(i,1),iw(i,1),iw(i,2),w(i,2),iw(i,3),iw(i,4)	
   !18   sx bordage  	
   else if (ivar.eq.18)  then						
      read(iu_27(iboat),*)sborx(i1,1),isborx(i1,1),isborx(i1,2),sborx(i1,2),isborx(i1,3),isborx(i1,4)
   !17   sy bordage 
   else if (ivar.eq.17)  then						
      read(iu_27(iboat),*)sbory(i1,1),isbory(i1,1),isbory(i1,2),sbory(i1,2),isbory(i1,3),isbory(i1,4)
   !19   tau bordage 
   else if (ivar.eq.19)  then						
      read(iu_27(iboat),*)tau(i1,1),itau(i1,1),itau(i1,2),tau(i1,2),itau(i1,3),itau(i1,4)
   !20 sigma comp bordage   
   else if (ivar.eq.20)  then					    
      read(iu_27(iboat),*)sbor(i1,1),isbor(i1,1),isbor(i1,2),sbor(i1,2),isbor(i1,3),isbor(i1,4)
   !21 sy  jas (cadre)  
   else if (ivar.eq.21) then						
      read(iu_27(iboat),*)sjas(i1,1),isjas(i1,1),isjas(i1,2),sjas(i1,2),isjas(i1,3),isjas(i1,4) 
   !22 sy  jab (cadre) 
   else if (ivar.eq.22) then						
      read(iu_27(iboat),*)sjab(i1,1),isjab(i1,1),isjab(i1,2),sjab(i1,2),isjab(i1,3),isjab(i1,4)
   !23 tau  jas (cadre)  
   else if (ivar.eq.23) then						
      read(iu_27(iboat),*)tjas(i1,1),itjas(i1,1),itjas(i1,2),tjas(i1,2),itjas(i1,3),itjas(i1,4)
   !24 tau  jab (cadre)
   else if (ivar.eq.24) then						
      read(iu_27(iboat),*)tjab(i1,1),itjab(i1,1),itjab(i1,2),tjab(i1,2),itjab(i1,3),itjab(i1,4)
   !25 sig comp jas (cadre)
   else if (ivar.eq.25) then						
      read(iu_27(iboat),*)ajas(i1,1),iajas(i1,1),iajas(i1,2),ajas(i1,2),iajas(i1,3),iajas(i1,4)
   !26 sig comp jab (cadre)   
   else if (ivar.eq.26) then						
      read(iu_27(iboat),*)ajab(i1,1),iajab(i1,1),iajab(i1,2),ajab(i1,2),iajab(i1,3),iajab(i1,4)
   !29 sig comp sem (cadre)
   else if (ivar.eq.29) then						
      read(iu_27(iboat),*)asem(i1,1),iasem(i1,1),iasem(i1,2),asem(i1,2),iasem(i1,3),iasem(i1,4)
   !36 sig comp jas (raid) 
   else if (ivar.eq.36) then						
      read(iu_27(iboat),*)rjas(i1,1),irjas(i1,1),irjas(i1,2),rjas(i1,2),irjas(i1,3),irjas(i1,4)
   !37 sig comp jab (raid) 
   else if (ivar.eq.37) then						
      read(iu_27(iboat),*)rjab(i1,1),irjab(i1,1),irjab(i1,2),rjab(i1,2),irjab(i1,3),irjab(i1,4)
   !38 sig comp sem (raid)
   else if (ivar.eq.38) then						
      read(iu_27(iboat),*)rsem(i1,1),irsem(i1,1),irsem(i1,2),rsem(i1,2),irsem(i1,3),irsem(i1,4)
   else
   
      write(iu_12(iboat),*)' error1: subr. analys; ivar=',ivar,' est incorrecte'
      write(*,*)           ' error1: subr. analys; ivar=',ivar,' est incorrecte'
      write(iu_14(iboat),*)'subr analysis'													
	    write(iu_14(iboat),*)' error1: subr. analys; ivar=',ivar,' est incorrecte'			
	 
	  write(*,*) 'stop'
	  read(*,*)
      stop
   end if
   goto 20


elseif((itype(i).eq.5).and.(isect(i).eq.3)) then		
	 !épontille double t		
	 i2 = i2 + 1	
   nelo = nelo + 1		
   
21 read(iu_30(iboat),*,end=1) ivar   !saved in ecri4				
   if(ivar.eq.99) goto 1								
   !1  v	
   if(ivar.eq.1) then					
			read(iu_30(iboat),*)v(i,1),iv(i,1),iv(i,2),v(i,2),iv(i,3),iv(i,4)		
   !2  u	   			
   else if (ivar.eq.2)  then									
      read(iu_30(iboat),*)u(i,1),iu(i,1),iu(i,2),u(i,2),iu(i,3),iu(i,4)
   !3  w	  		
   else if (ivar.eq.3)  then									
      read(iu_30(iboat),*)w(i,1),iw(i,1),iw(i,2),w(i,2),iw(i,3),iw(i,4)			
	 !17 sy  jas sup(cadre)
	 else if (ivar.eq.17) then							
      read(iu_30(iboat),*)sjasi(i2,1),isjasi(i2,1),isjasi(i2,2),sjasi(i2,2),isjasi(i2,3),isjasi(i2,4)							
   else if (ivar.eq.18) then						!18 sy  jas inf (cadre)	
      read(iu_30(iboat),*)sjabi(i2,1),isjabi(i2,1),isjabi(i2,2),sjabi(i2,2),isjabi(i2,3),isjabi(i2,4)							
   else if (ivar.eq.19) then						!19 tau  jas (cadre)	
      read(iu_30(iboat),*)tjasi(i2,1),itjasi(i2,1),itjasi(i2,2),tjasi(i2,2),itjasi(i2,3),itjasi(i2,4)							
   else if (ivar.eq.20) then						!20 tau  an (cadre)		
      read(iu_30(iboat),*)tjabi(i2,1),itjabi(i2,1),itjabi(i2,2),tjabi(i2,2),itjabi(i2,3),itjabi(i2,4)							
	
   else if (ivar.eq.21) then						!21 sig comp jas sup (cadre) 
      read(iu_30(iboat),*)ajasi(i2,1),iajasi(i2,1),iajasi(i2,2),ajasi(i2,2),iajasi(i2,3),iajasi(i2,4)							
   else if (ivar.eq.22) then						!22 sig comp jas inf (cadre)  
      read(iu_30(iboat),*)ajabi(i2,1),iajabi(i2,1),iajabi(i2,2),ajabi(i2,2),iajabi(i2,3),iajabi(i2,4)							
   else if (ivar.eq.23) then						!23 sig comp an (cadre)	
      read(iu_30(iboat),*)sborxi(i2,1),isborxi(i2,1),isborxi(i2,2),sborxi(i2,2),isborxi(i2,3),isborxi(i2,4)						
	 else if (ivar.eq.27) then						!27 sig comp sem sup (cadre) 
      read(iu_30(iboat),*)asemi(i2,1),iasemi(i2,1),iasemi(i2,2),asemi(i2,2),iasemi(i2,3),iasemi(i2,4)							
   else if (ivar.eq.28)  then						!28 sig comp sem inf (cadre) 
      read(iu_30(iboat),*)sboryi(i2,1),isboryi(i2,1),isboryi(i2,2),sboryi(i2,2),isboryi(i2,3),isboryi(i2,4)						
   else																	
      write(iu_12(iboat),*)'error2: subr. analys; ivar=',ivar,' est incorrecte'	
      write(*,*) 'error2: subr. analys; ivar=',ivar,' est incorrecte'	
      write(iu_14(iboat),*)'subr analysis'												
	    write(iu_14(iboat),*)'error2: subr. analys; ivar=',ivar,' est incorrecte'			
	 
	  write(*,*) 'stop'
	  read(*,*)															
   stop															
	 endif
	 goto 21											


elseif((itype(i).eq.5).and.(isect(i).ne.3)) then		
   !épontille parois minces
	 nelo= nelo+1
	 i3 = i3 + 1		
22 read(iu_30(iboat),*,end=1) ivar   !saved in ecri4				
   if(ivar.eq.99) goto 1					

   !1  v
   if(ivar.eq.1) then											 
      read(iu_30(iboat),*)v(i,1),iv(i,1),iv(i,2),v(i,2),iv(i,3),iv(i,4)   
   !2  u 
   else if (ivar.eq.2)  then									
      read(iu_30(iboat),*)u(i,1),iu(i,1),iu(i,2),u(i,2),iu(i,3),iu(i,4)    
   !3  w
   else if (ivar.eq.3)  then									  
      read(iu_30(iboat),*)w(i,1),iw(i,1),iw(i,2),w(i,2),iw(i,3),iw(i,4)    
   !17 sy  jas sup(cadre)  
	 else if (ivar.eq.17) then						
      read(iu_30(iboat),*)sjasi(i3,1),isjasi(i3,1),isjasi(i3,2),sjasi(i3,2),isjasi(i3,3),isjasi(i3,4)                     
   !18 sy  jas inf (cadre)
   else if (ivar.eq.18) then						     
      read(iu_30(iboat),*)sjabi(i3,1),isjabi(i3,1),isjabi(i3,2),sjabi(i3,2),isjabi(i3,3),isjabi(i3,4)            
   !19 tau  jas (cadre)            
   else if (ivar.eq.19) then						
      read(iu_30(iboat),*)tjasi(i3,1),itjasi(i3,1),itjasi(i3,2),tjasi(i3,2),itjasi(i3,3),itjasi(i3,4)      
   !20 tau  an (cadre)                
   else if (ivar.eq.20) then						 
      read(iu_30(iboat),*)tjabi(i3,1),itjabi(i3,1),itjabi(i3,2),tjabi(i3,2),itjabi(i3,3),itjabi(i3,4)                      
   !21 sig comp jas sup (cadre) 
   else if (ivar.eq.21) then						 
      read(iu_30(iboat),*)ajasi(i3,1),iajasi(i3,1),iajasi(i3,2),ajasi(i3,2),iajasi(i3,3),iajasi(i3,4)         
   !22 sig comp jas inf (cadre)             
   else if (ivar.eq.22) then						  
      read(iu_30(iboat),*)ajabi(i3,1),iajabi(i3,1),iajabi(i3,2),ajabi(i3,2),iajabi(i3,3),iajabi(i3,4)      
   !23 sig comp an (cadre)                 
   else if (ivar.eq.23) then						  
      read(iu_30(iboat),*)sborxi(i3,1),isborxi(i3,1),isborxi(i3,2),sborxi(i3,2),isborxi(i3,3),isborxi(i3,4)                   
   else	                                                            
        write(iu_12(iboat),*)'error3: subr. analys; ivar=',ivar,' est incorrecte'	
        write(*,*) 'error3: subr. analys; ivar=',ivar,' est incorrecte'	
        write(iu_14(iboat),*)'subr analysis'													!bug
	      write(iu_14(iboat),*)'error3: subr. analys; ivar=',ivar,' est incorrecte'			
		  
  	      write(*,*) 'stop'
	      read(*,*)															
 
        stop	                                                           
	 endif                                                              
	 goto 22                                                               
	 endif													

30 if(impr2.le.-2) goto 1		
31 read(iu_27(iboat),*,end=1) ivar !saved in ecri3	
   if(ivar.eq.88) goto 1	
   ! 4 sig.comp z=+d/2					      
   if(ivar.eq.4) then						         
      read(iu_27(iboat),*)sbor1(i1,1),isbor1(i1,1),isbor1(i1,2),sbor1(i1,2),isbor1(i1,3),isbor1(i1,4)
   ! 8 sig.comp z=-d/2
   else if (ivar.eq.8) then						 
      read(iu_27(iboat),*)sbor2(i1,1),isbor2(i1,1),isbor2(i1,2),sbor2(i1,2),isbor2(i1,3),isbor2(i1,4)
   else
      write(iu_12(iboat),*)' error4: subr. analys; ivar=',ivar,' est incorrecte'
      write(*,*)           ' error4: subr. analys; ivar=',ivar,' est incorrecte'
      write(iu_14(iboat),*)' subr analysis'												
	    write(iu_14(iboat),*)' error4: subr. analys; ivar=',ivar,' est incorrecte'			

  	    write(*,*) 'stop'
	    read(*,*)															

   stop

end if
goto 31

!compte des panneaux qui sont épontilles	
1  if(itype(i).ne.5) then			
	    i4(i1) = i1 + i2 + i3					
	 else if(isect(i).eq.3) then			
	    i5(i2) = i1 + i2 + i3					
	 else										
	    i6(i3) = i1 + i2 + i3	
	 endif										

2  continue						
neta = neto - nelo			
   
! -------------------------------------------------------------------------------
!
!     classement des maximum/minimum entre les panneaux
!
! -------------------------------------------------------------------------------
if(langue==1) write(iu_12(iboat),10)
if(langue==2) write(iu_12(iboat),11)

if(langue==1) write(iu_12(iboat),*)'les valeurs extrêmes de v sont:'
if(langue==2) write(iu_12(iboat),*)'extrem values of v are:'
call classe(v(1,2),iv(1,3),iv(1,4),neto)
call classe(v(1,1),iv(1,1),iv(1,2),neto)
if(langue==1) write(iu_12(iboat),*)'les valeurs extrêmes de u sont:'
if(langue==2) write(iu_12(iboat),*)'extrem values of u are:'
call classe(u(1,2),iu(1,3),iu(1,4),neto)
call classe(u(1,1),iu(1,1),iu(1,2),neto)
if(langue==1) write(iu_12(iboat),*)'les valeurs extrêmes de w sont:'
if(langue==2) write(iu_12(iboat),*)'extrem values of w are:'
call classe(w(1,2),iw(1,3),iw(1,4),neto)
call classe(w(1,1),iw(1,1),iw(1,2),neto)
if (impr2.eq.-3) goto 100		
! -------------------      
write(iu_12(iboat),*)

if(langue==1) then
	 write(iu_12(iboat),*)'les valeurs extrêmes de sx bordage (z=0) sont:'
else
   write(iu_12(iboat),*)'extrem values of sx stress, plate (z=0) are:'
endif

call classe2(sborx(1,2),isborx(1,3),isborx(1,4),neta,i4)	
call classe2(sborx(1,1),isborx(1,1),isborx(1,1),neta,i4)	
      
if(langue==1) then
	 write(iu_12(iboat),*)'les valeurs extrêmes de sy bordage (z=0) sont:'
else
   write(iu_12(iboat),*)'extrem values of sy stress, plate (z=0) are:'
endif

call classe2(sbory(1,2),isbory(1,3),isbory(1,4),neta,i4)	
call classe2(sbory(1,1),isbory(1,1),isbory(1,1),neta,i4)	
      
if(langue==1) then
	 write(iu_12(iboat),*)'les valeurs extrêmes de txy bordage (z=0) sont:'
else
   write(iu_12(iboat),*)'extrem values of txy shear stress, plate (z=0) are:'
endif

call classe2(tau(1,2),itau(1,3),itau(1,4),neta,i4)		
call classe2(tau(1,1),itau(1,1),itau(1,1),neta,i4)		
      
if(langue==1) then
   write(iu_12(iboat),*)'les valeurs extrêmes de sig comp bordage (z=0) sont:'
else
   write(iu_12(iboat),*)'extrem values of combined stress, plate (z=0) are:'
endif

call classe2(sbor(1,2),isbor(1,3),isbor(1,4),neta,i4)		
      
if(impr2.ge.0) then
   if(langue==1) then
      write(iu_12(iboat),*)'les valeurs extrêmes de sigma comp (z=+d/2) sont:'
	 else
      write(iu_12(iboat),*)'extrem values of combined stress, plate(z=+d/2) are:'
	 endif
   call classe2(sbor1(1,2),isbor1(1,3),isbor1(1,4),neta,i4)	

   if(langue==1) then
      write(iu_12(iboat),*)'les valeurs extrêmes de sigma comp (z=-d/2) sont:'
	 else
      write(iu_12(iboat),*)'extrem values of combined stress, plate(z=-d/2) are:'
	 endif
   call classe2(sbor2(1,2),isbor2(1,3),isbor2(1,4),neta,i4)	
endif
! ------------------
write(iu_12(iboat),*)
if(indaig.eq.1) then
   if(langue==1) then
	    write(iu_12(iboat),*)'les valeurs extrêmes de sy jas cadre sont:'
	 else
      write(iu_12(iboat),*)'extrem values of sy stress jwf frame are:'
	 endif
   call classe2(sjas(1,2),isjas(1,3),isjas(1,4),neta,i4)		
   call classe2(sjas(1,1),isjas(1,1),isjas(1,1),neta,i4)		
 
   if(langue==1) then
	    write(iu_12(iboat),*)'les valeurs extrêmes de tau jas cadre sont:'
	 else
      write(iu_12(iboat),*)'extrem values of txy shear stress jwf frame are:'
	 endif
   call classe2(tjas(1,2),itjas(1,3),itjas(1,4),neta,i4)	
   call classe2(tjas(1,1),itjas(1,1),itjas(1,1),neta,i4)	
 
   if(langue==1) then
	    write(iu_12(iboat),*)'les valeurs extrêmes de sy jab cadre sont:'
	 else
      write(iu_12(iboat),*)'extrem values of sy stress jwp frame are:'
	 endif
   call classe2(sjab(1,2),isjab(1,3),isjab(1,4),neta,i4)		
   call classe2(sjab(1,1),isjab(1,1),isjab(1,1),neta,i4)		

      if(langue==1) then
	 write(iu_12(iboat),*)'les valeurs extrêmes de tau jab cadre sont:'
	else
       write(iu_12(iboat),*)'extrem values of txy shear stress jwb frame are:'
	endif
      call classe2(tjab(1,2),itjab(1,3),itjab(1,4),neta,i4)		
      call classe2(tjab(1,1),itjab(1,1),itjab(1,1),neta,i4)		
   
      if(langue==1) then
       write(iu_12(iboat),*)'les valeurs extrêmes de sigma comp jas cadre sont:'
	else
       write(iu_12(iboat),*)'extrem values of sc jwf frame (comb. stress) are:'
	endif
      call classe2(ajas(1,2),iajas(1,3),iajas(1,4),neta,i4)		

      if(langue==1) then
       write(iu_12(iboat),*)'les valeurs extrêmes de sigma comp jab cadre sont:'
	else
       write(iu_12(iboat),*)'extrem values of sc jwp frame (comb. stress) are:'
	endif
      call classe2(ajab(1,2),iajab(1,3),iajab(1,4),neta,i4)		

      if(langue==1) then
       write(iu_12(iboat),*)'les valeurs extrêmes de sigma comp sem cadre sont:'
	else
       write(iu_12(iboat),*)'extrem values of sc flange frame (comb. stress) are:'
	endif
      call classe2(asem(1,2),iasem(1,3),iasem(1,4),neta,i4)		
endif

! -------------------      
! épontilles					
! si on a des épontilles en double t

if (i2.gt.0) then      
	if(langue==1) then                                         
	write(iu_12(iboat),*)'les valeurs extrêmes de sy jas supérieure epon sont:'   
	else                                                                  
       write(iu_12(iboat),*)'extrem  values of sy stress jwf top pillar are:'         
	endif                                                             
      call classe2(sjasi(1,2),isjasi(1,3),isjasi(1,4),nelo,i5)               
      call classe2(sjasi(1,1),isjasi(1,1),isjasi(1,1),nelo,i5)           
  
      if(langue==1) then                                
	 write(iu_12(iboat),*)'les valeurs extrêmes de sy jas inférieur epon sont:'  
	else                                                                 
      write(iu_12(iboat),*)'extrem values of sy stress jwf bottom pillar are:'         
	endif                                                                   
      call classe2(sjabi(1,2),isjabi(1,3),isjabi(1,4),nelo,i5)                    
      call classe2(sjabi(1,1),isjabi(1,1),isjabi(1,1),nelo,i5)                  
 
      if(langue==1) then                                                     
	 write(iu_12(iboat),*)'les valeurs extrêmes de tau jas épontille sont:'         
	else                                                                   
       write(iu_12(iboat),*)'extrem values of shear stress jwf pillar are:'             
	endif                                                                  
      call classe2(tjasi(1,2),itjasi(1,3),itjasi(1,4),nelo,i5)                  
      call classe2(tjasi(1,1),itjasi(1,1),itjasi(1,1),nelo,i5)                  

      if(langue==1) then                                                   
	 write(iu_12(iboat),*)'les valeurs extrêmes de tau axe neutre épon sont:'        
	else                                                                    
       write(iu_12(iboat),*)'extrem values of txy shear stress'                          
       write(iu_12(iboat),*)'at neutral axis pillar are:'                             
	endif                                                               
      call classe2(tjabi(1,2),itjabi(1,3),itjabi(1,4),nelo,i5)                
      call classe2(tjabi(1,1),itjabi(1,1),itjabi(1,1),nelo,i5)                 
   
      if(langue==1) then                                                  
      write(iu_12(iboat),*)'les valeurs extrêmes de sig comp jas sup épon sont:'   
	else                                                                 
      write(iu_12(iboat),*)'extrem values of comb. stress jwf top pillar are:'         
	endif                                                               
      call classe2(ajasi(1,2),iajasi(1,3),iajasi(1,4),nelo,i5)                 
 
       if(langue==1) then                                                
      write(iu_12(iboat),*)'les valeurs extrêmes de sig comp jas inf épon sont:'     
	else                                                                     
      write(iu_12(iboat),*)'extrem values of sc jwf bottom'                            
      write(iu_12(iboat),*)' pillar (comb. stress) are:'                                
	endif                                                                    
      call classe2(ajabi(1,2),iajabi(1,3),iajabi(1,4),nelo,i5)                  

      if(langue==1) then                                                
      write(iu_12(iboat),*)'les valeurs extrêmes de sig comp axe n. épon sont:'    
	else                                                                 
      write(iu_12(iboat),*)'extrem values of sc neutral axis'                          
      write(iu_12(iboat),*)' pillar (comb. stress) are:'                               
	endif                                                                   
      call classe2(sborxi(1,2),isborxi(1,3),isborxi(1,4),nelo,i5)             
 
	if(langue==1) then                                                  
       write(iu_12(iboat),*)'les valeurs extrêmes de sig comp sem sup épon sont:'  
	else                                                                 
       write(iu_12(iboat),*)'extrem values of sc flange top'                         
       write(iu_12(iboat),*)' pillar (comb. stress) are:'                                
	endif                                                                       
      call classe2(asemi(1,2),iasemi(1,3),iasemi(1,4),nelo,i5)                  

     	if(langue==1) then                                                    
       write(iu_12(iboat),*)'les valeurs extrêmes de sig comp sem inf épon sont:'        
	else                                                                  
       write(iu_12(iboat),*)'extrem values of sc flange bottom'                        
       write(iu_12(iboat),*)' pillar (comb. stress) are:'                              
	endif                                                                  
      call classe2(sboryi(1,2),isboryi(1,3),isboryi(1,4),nelo,i5)                
endif
!	--------------
!si on a des épontilles à paroi mince   
if (i3.gt.0.and.ivar.ne.99) then !car si ivar=99 => ces vecteurs ne sont pas lus
 	  if(langue==1) then                                                   
	    write(iu_12(iboat),*)'les valeurs extrêmes de sy supérieure épon sont:'         
	  else                                                                    
         write(iu_12(iboat),*)'extrem  values of sy stress top pillar are:'               
	  endif                                                                
      call classe2(sjasi(1:nelo,2),isjasi(1:nelo,3),isjasi(1:nelo,4),nelo,i6)               
      call classe2(sjasi(1:nelo,1),isjasi(1:nelo,1),isjasi(1:nelo,1),nelo,i6)                 
  
      if(langue==1) then                                           
      write(iu_12(iboat),*)'les valeurs extrêmes de sy inférieur épon sont:'   
	else                                                        
      write(iu_12(iboat),*)'extrem values of sy stress bottom pillar are:'      
	endif                                                        
      call classe2(sjabi(1,2),isjabi(1,3),isjabi(1,4),nelo,i6)          
      call classe2(sjabi(1,1),isjabi(1,1),isjabi(1,1),nelo,i6)            
 
      if(langue==1) then                                                  
	 write(iu_12(iboat),*)'les valeurs extrêmes de tau sommet épon sont:'          
	else                                                                 
       write(iu_12(iboat),*)'extrem values of shear stress top pillar are:'         
	endif                                                               
      call classe2(tjasi(1,2),itjasi(1,3),itjasi(1,4),nelo,i6)                      
      call classe2(tjasi(1,1),itjasi(1,1),itjasi(1,1),nelo,i6)            
  
      if(langue==1) then                                              
	 write(iu_12(iboat),*)'les valeurs extrêmes de tau axe neutre épon sont:' 
	else                                                                 
       write(iu_12(iboat),*)'extrem values of txy shear stress'                     
       write(iu_12(iboat),*)'at neutral axis pillar are:'                             
	endif                                                               
      call classe2(tjabi(1,2),itjabi(1,3),itjabi(1,4),nelo,i6)                
      call classe2(tjabi(1,1),itjabi(1,1),itjabi(1,1),nelo,i6)                
        
      if(langue==1) then                                                   
      write(iu_12(iboat),*)'les valeurs extrêmes de sig comp sup épon sont:'             
	else                                                                  
      write(iu_12(iboat),*)'extrem values of comb. stress jwf top pillar are:'        
	endif                                                              
      call classe2(ajasi(1,2),iajasi(1,3),iajasi(1,4),nelo,i6)               
    
      if(langue==1) then                                                
      write(iu_12(iboat),*)'les valeurs extrêmes de sig comp inf épon sont:'        
	else                                                                 
      write(iu_12(iboat),*)'extrem values of sc jwf bottom'                          
      write(iu_12(iboat),*)' pillar (comb. stress) are:'                           
	endif                                                             
      call classe2(ajabi(1,2),iajabi(1,3),iajabi(1,4),nelo,i6)              

      if(langue==1) then                                                
      write(iu_12(iboat),*)'les valeurs extrêmes de sig comp axe n. épon sont:'  
	else                                                               
      write(iu_12(iboat),*)'extrem values of sc neutral axis'                     
      write(iu_12(iboat),*)' pillar (comb. stress) are:'                         
	endif                                                            
      call classe2(sborxi(1,2),isborxi(1,3),isborxi(1,4),nelo,i6)      
endif															
! -------------------      

write(iu_12(iboat),*)
if(indrai.eq.1) then
    if(langue==1) then
       write(iu_12(iboat),*)'les valeurs extrêmes de sigma comp jas raid sont:'
	else
       write(iu_12(iboat),*)'extrem values of sc jwf stiff (comb. stress) are:'
	endif
    call classe2(rjas(1,2),irjas(1,3),irjas(1,4),neta,i4)				

    if(langue==1) then
       write(iu_12(iboat),*)'les valeurs extrêmes de sigma comp jab raid sont:'
	else
       write(iu_12(iboat),*)'extrem values of sc jwp stiff (comb. stress) are:'
	endif
      call classe2(rjab(1,2),irjab(1,3),irjab(1,4),neta,i4)				

    if(langue==1) then
       write(iu_12(iboat),*)'les valeurs extrêmes de sigma comp sem raid sont:'
	else
       write(iu_12(iboat),*)'extrem values of sc flange stiff (comb. stress) are:'
	endif
    call classe2(rsem(1,2),irsem(1,3),irsem(1,4),neta,i4)				
endif
  100 continue


deallocate (v)
deallocate (u)
deallocate (w)
deallocate (iv)
deallocate (iu)
deallocate (iw)
deallocate (sborx)
deallocate (sbory)
deallocate (tau)
deallocate (isborx)
deallocate (isbory)
deallocate (itau)
deallocate (sbor)
deallocate (sbor1)
deallocate (sbor2)
deallocate (isbor)
deallocate (isbor1)
deallocate (isbor2)

deallocate (sjas)
deallocate (sjab)
deallocate (tjas)
deallocate (tjab)
deallocate (isjas)
deallocate (isjab)
deallocate (itjas)
deallocate (itjab)
deallocate (ajas)
deallocate (ajab)
deallocate (asem)
deallocate (iajas)
deallocate (iajab)
deallocate (iasem)
deallocate (rjas)
deallocate (rjab)
deallocate (rsem)
deallocate (irjas)
deallocate (irjab)
deallocate (irsem)

deallocate (sborxi)
deallocate (sboryi)
deallocate (isborxi)
deallocate (isboryi)
deallocate (sjasi)
deallocate (sjabi)
deallocate (tjasi)
deallocate (tjabi)
deallocate (isjasi)
deallocate (isjabi)
deallocate (itjasi)
deallocate (itjabi)
deallocate (ajasi)
deallocate (ajabi)
deallocate (asemi)
deallocate (iajasi)
deallocate (iajabi)
deallocate (iasemi)

deallocate (i4)
deallocate (i5)
deallocate (i6) 

      
return                                                            
  10  format(/'recapitulatifs de valeurs extremes calculees (deplac.',           &
              ' et contraintes de comparaison'/70(1h-)/                          &
              '(par ordre decroissant ; unités = m et n/m2)'/t2,'valeurs',t18,   &
              'panneaux',t40,'positions (x,y)'/ )
  11  format(/'summary of the extreme values (displ & combined stress)'          &
             /54(1h*)/                                                           &
              '(ranked by values ; units = m and n/m2)'/t2,'values',t18,         &
              'panels',t40,'locations (x,y)'/ )
end                                                               

! --------------------------------------------------------------------------------
!
subroutine classe(x,ix,iy,neto)                            	
!--------------------------------------------------------------------------------
!     subroutine de classement par ordre decroissant des valeurs extrêmes.
!
!     modifié : 13-3-95                     créé: 13-3-95
!
!--------------------------------------------------------------------------------
use param_section, ONLY : iu_12,iboat

implicit double precision (a-h,o-z)                                     
dimension  x(neto),ix(neto),iy(neto)                    		  

!     x(nel) avec nel=1,neto  : valeur à classer     
!     ix et iy  coordonnées des valeurs maximales pour chaque panneau 
!     !! les valeurs de x ne sont pas conservees. 
!     !! le vecteur x n'est donc plus utilisable par la suite
  
      
do j=1,neto 
!  grande valeur négative
   !x1=-1.e30 !fred c'était trop grand ça
   x1=-2**30 !fred							
   n1=0
   do k=1,neto                                                     
      if(x1.lt.x(k)) then
         x1=x(k)
         n1=k
      endif
   enddo

	 if(n1.ne.0) then											
      if(dabs(x(n1)).ge.1.0e-15) write(iu_12(iboat),10)x(n1),n1,ix(n1),iy(n1)
      x(n1)=-1.e30
	 endif														

enddo

! ------------------------------------------------------	                                                           
10 format(e11.4,t20,i3,t45,'(',i3,',',i3,')')

return 
end    
	                                                           
!--------------------------------------------------------------------------------
!
subroutine classe2(xi,ixi,iyi,nelo,ii)                        	
!--------------------------------------------------------------------------------                                                
!     subroutine de classement par ordre decroissant des valeurs extrêmes.
!
!     modifié : 05-02-2004                     créer: 05-02-2004
!
!--------------------------------------------------------------------------------        
use param_section, ONLY : iu_12,iu_27,iboat

implicit double precision (a-h,o-z)                                     
dimension  xi(nelo),ixi(nelo),iyi(nelo),ii(nelo)                    		  

!     x(nel) avec nel=1,neto  : valeur à classer     
!     ix et iy  coordonnées des valeurs maximales pour chaque panneau 
!     !! les valeurs de x ne sont pas conservees. 
!     !! le vecteur x n'est donc plus utilisable par la suite

do j=1,nelo 
   ! grande valeur négative
   xi1=-1.e30							
   ni1=0
   do k=1,nelo                                                     
      if(xi1.lt.xi(k)) then
         xi1=xi(k)
         ni1=ii(k)
         ni2=k
      endif
   enddo

   if(dabs(xi(ni2)).ge.1.0e-15) then
	    write(iu_12(iboat),10)xi(ni2),ni1,ixi(ni2),iyi(ni2)
	 endif
   xi(ni2)=-1.e30

enddo

! ------------------------------------------------------	                                                           
10 format(e11.4,t20,i3,t45,'(',i2,',',i2,')')

return 
end                                                               
