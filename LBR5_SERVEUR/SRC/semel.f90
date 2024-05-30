subroutine semel(hya,dya,wya,tya,xmin,xmax,ind,kk,iprint,nel,isem,isect1)

use param_section,IPRINT_=>IPRINT,COEFF_=>COEFF
		                                    
implicit double precision (a-h,o-z)
character*6 type(3)

! ====================================================================================================
!subroutine semel
! ====================================================================================================
!cette subroutine de modification des dimensions des semelles en fonction
!de restrictions g�om�triques.
!
!dw = hauteur   de l'�me (web),
!tw = �paisseur de l'�me (web),
!df = hauteur   de la semelle (flange),
!tf = �paisseur de la semelle (flange),
!
!cr�ation     : 6-06-96 (ph. rigo pour l.b.r.-5)
!modif: 13-3-98 : isem=1: df < 16.tf  et isem=2: df < 32.tf
!
!last modification:13-3-98
!
! ====================================================================================================
!   les variables de conception  ; nbrxi = nvar(nel)
!   -------------------------------------------------
!  1       delta =  �paisseur du bordage
!  2       hya   =  hauteur de l'�me des aiguilles
!  3       dya   =  �paisseur de l'�me des aiguilles
!  4       wya   =  largeur des semelles des aiguilles
!  5       epsa  =  entredistance entre aiguilles
!  6       hxr   =  hauteur de l'�me des raidisseurs
!  7       dxr   =  �paisseur de l'�me des raidisseurs
!  8       wxr   =  largeur des semelles des raidisseurs
!  9       epsr  =  entredistance entre raidisseurs
!
!
!  variables de conceptions pour les �pontilles:                           
!         1              hya       =       hauteur de la demi-�me                                
!         2              dya       =       �paisseur de l'�me                                      
!         3              wya       =       largeur de la semelle                                  
!         4              epais     =       �paisseur de la paroi mince                          
!         5              epsa      =       entredistance entre �pontilles                    

type(1)='cadre'
type(2)='raid.'
type(3)='�pont.'                                                                            

! ====================================================================================================
! 1.    initialisations et calcul des ratios al1, al2 et al3
! ====================================================================================================
dw=hya
tw=dya
df=wya
tf=tya
if (isect1.eq.3) then                                                                    
   dw=2*hya                                                                                        
endif                                                                                                

!pour condition tw < 2.tf
al1= 2.*tw/tf

if(isem.eq.1) then
   !pour condition  df < 16.tf
   sq2= sqrt(2.)
   !pour condition 8.tf < df
   al2= dsqrt(df/(8.*tf))
elseif(isem.eq.2) then
   !pour condition  df < 32.tf
   sq2= 2.
   !pour condition 8.tf < df
   al2= dsqrt(df/(8.*tf))
elseif(isem.eq.3) then
   !pour condition  df < 30.tf
   sq2= sqrt(2.5)
   !pour condition 12.tf < df
   al2= dsqrt(df/(12.*tf))
elseif(isem.eq.4) then
   !pour condition  df < 15.tf
   sq2= 3.273
   !pour condition 1.4.tf < df
   al2= dsqrt(df/(1.4*tf))
elseif(isem.eq.5) then
   al1 = al1*2.
!pour condition  df < 15.tf
   sq2= 1.581
!pour condition 6.tf < df
   al2= dsqrt(df/(6*tf))
endif

al3= sq2/al2

if(iprint.ge.1) then
   write(iu_31(iboat),*) 'al1, al2, al3 =',al1,'  ',al2,'  ',al3
endif


! ====================================================================================================
! 2.  algorithme  pour al1>1    c.�.d  tw < 2.tf  est v�rifi�
! ====================================================================================================
if (al1.lt.1) goto 1

al11 = al1/2.
if (al11.lt.1.or.isem.eq.1.or.isem.eq.2) then !!!condition tf<tw respect�e

   ! 2.1  1 < al2 < 1.41 (ou 2)
   ! ---------------------
   if ((al2.ge.1).and.(al2.le.sq2)) goto 2

   ! 2.2  al2 < 1
   ! ---------
   if (al2.lt.1) then
      df=df/al2
      tf=tf*al2
   endif

   ! 2.3  al2 > 1.41 (ou 2)  (al3 < 1)
   ! ----------------------------
   if (al2.gt.sq2) then
      df=df*al3
      tf=tf/al3
   endif

else							!!!condition tf<tw pas respect�e
	
   ! 1 < al2 < 1.41 (ou 2)
   ! ---------------------
   if ((al2.ge.1).and.(al2.le.sq2)) then
      df=df/al11
      tf=tf*al11
   endif

   !  al2 < 1
   ! ---------
   if (al2.lt.1) then
      al33 = 0.667*al2 + 0.333*al11
      if (al33.gt.1) al33=1.
      df=df/al33
      tf=tf*al33
   endif
	
   ! al2 > 1.41 (ou 2)  (al3 < 1)
   ! ----------------------------
   if (al2.gt.sq2) then
      al33=1./al3
      coeff = max(al11,al33)
      df=df/coeff
      tf=tf*coeff
   endif

endif !fin condition sur tf/tw
		
goto 2

1  continue


! ====================================================================================================
! 3.  algorithme pour al1<1     c.�.d  tw < 2.tf  n'est pas v�rifi�
! ====================================================================================================
al22= al2/al1

! 3.1  1 < al22 < 1.41 (ou 2)
! ---------------------
if ((al2.ge.1).and.(al2.le.sq2)) then
   df=df/al1
   tf=tf*al1
endif

! 3.2  al22 < 1
! ---------
if (al2.lt.1) then
   coeff = min(al1,al2)
   df=df/coeff !al2
   tf=tf*coeff !al2
endif

! 3.3  al2 > 1.41 (ou 2)  (al3 < 1)   (! il s'agit bien de al2 et pas de al22)
! -------------------------------------
if (al2.gt.sq2) then
   al33=0.6667 * al3 + 0.333 /al1
   if (al33.gt.1.) al33=1.
   df=df*al33
   tf=tf/al33
endif

2  continue
! ====================================================================================================
! 4.  v�rification des bornes
! ====================================================================================================
k=0
if (df.lt.xmin) then
   write(iu_31(iboat),*) ' !Lors de la modification automatique des semelles sur le panneau:',nel
   write(iu_31(iboat),*)  ' borne atteinte  df=',df,'  xmin=',xmin
   write(iu_14(iboat),*) ' !Lors de la modification automatique des semelles sur le panneau:',nel
   write(iu_14(iboat),*)  ' borne atteinte  df=',df,'  xmin=',xmin		
   df=xmin
   k=1
else if (df.gt.xmax) then
   write(iu_31(iboat),*) ' !Lors de la modification automatique des semelles sur le panneau:',nel
   write(iu_31(iboat),*)  ' borne atteinte  df=',df,'  xmax=',xmax
   write(iu_14(iboat),*) ' !Lors de la modification automatique des semelles sur le panneau:',nel
   write(iu_14(iboat),*)  ' borne atteinte  df=',df,'  xmax=',xmax		
   df=xmax
   k=1
endif

! if (df.lt.(dw/2.)) then
!    df=dw/2.
!    k=1
!  else if (df.gt.dw) then
!    df=dw
!    k=1
!  endif

if (k.eq.1) then
   tf=wya*tya/df
endif

3  continue


! ====================================================================================================
! 5.  calcul des co�ficients pour modifier les sensibilit�s
!concernant les restrictions  2.df < dw  < df (code 201 et 202)
! ====================================================================================================
!pour la condition tf < 2.tw avec les nouvelles valeurs df et tf
al1= 2.*tw/tf
if (al1.lt.1) then
   ind=1
   if(iprint.ge.1) then
      write(iu_31(iboat),*) 'la condition (tf < 2.tw) n''est pas respect�e'
      write(iu_14(iboat),*) 'la condition (tf < 2.tw) n''est pas respect�e'							
   endif
else
   ind=0
endif



! ====================================================================================================
! 6.  r�sultats
! ====================================================================================================
if(impr2.ge.-1)then			
   write(iu_31(iboat),100) nel,wya,tya,df,tf,type(kk),ind
	endif					

wya=df
tya=tf

return

100  format(3x,i2,2x,e13.6,1x,e13.6,4x,e13.6,1x,e13.6,4x,a5,3x,i2)
end
