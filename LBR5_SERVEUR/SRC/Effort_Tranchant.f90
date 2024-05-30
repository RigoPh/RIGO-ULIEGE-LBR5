subroutine Effort_Tranchant() 

use  param_section

integer*4 bboli
integer*4 nelH
integer*4 j
double precision angEffortT
double precision pdstotal
double precision FF
double precision FFoli

call poidstotal(pdstotal,0,0)

!***********************************************************************
!     subroutine Effort tranchant 
!     ===========================
!
!  Subroutine appelée dans edge_moment (60)
!
!
!  methodEffortT = 1  : Méthode où la longueur du modèle & le moment sont modifiés
!  methodEffortT = 2  : Méthode où la masse volumique ou le chargement sont modifiés ==> NE PAS UTILISER
!  methodEffortT = 3  : Méthode où 2 forces ponctuelles sont appliquées au step 10,11 et 40,41
!  
!  EffortT  = 1       : Prise en compte de l'effort tranchant
!  EffortT  = 0       : NON prise en compte de l'effort tranchant
!
!  Suffixe EffortT    : Stipule les variables utilisées dans la routine Effort tranchant 	
!
!    créer : - Septembre 10  pour lbr-5 (janvier 1996), anast 	
!    -------
!    modif : - Octobre   10, correction des appellations et explications. 
!    ------  
!            
!           
!    dernière modif: 
!   --------------
!*************************************************************************


!if (EffortT.eq.1) then		! (Sélection de la méthode avec Effort tranchant =1 ou sans Effort Tranchant =0)

varolihversb   =0			! Variable de travail dont la direction est de haut en bas
varolihversb1  =0		
varolibversh   =0			! Variable de travail dont la direction est de bas en haut
varolibversh1  =0
varolitemp     =0           ! Variable de travail intermédiaire pour calcul de la résultante des pressions 


do ip=1,nsolm                ! Boucle sur les cas de chargement
    nelH           =0
    j              =1	
    PEffortT(ip)   =0
    varoli(ip)     =0
    XEffortT(ip)   =0.5

do nel=1,neto               ! Boucle sur les panneaux pour la résultante des pressions

    angEffortT=(pi/180)*panneau(nel).ang

	if ((panneau(nel).ang .ne. 90) .OR. (panneau(nel).ang .ne. 270)) then 
	nelH=nelH+1
	pointeur(j)=nel
	j=j+1
	endif

!***************************************************
!Composante verticale vers le haut s'oppose au poids
!***************************************************

if (((panneau(nel).kse.eq.2.) .AND. (panneau(nel).ang.GE.0.)   .AND.  (panneau(nel).ang.LE.90.)) .OR. &
    ((panneau(nel).kse.eq.2.) .AND. (panneau(nel).ang.GT.270.) .AND.  (panneau(nel).ang.LE.360.)) .OR. & 
    ((panneau(nel).kse.eq.1.) .AND. (panneau(nel).ang.GT.90.)  .AND.  (panneau(nel).ang .LE. 270.))) then 	

varolibversh= varolibversh + .5 * (panneau(nel).xi_full(ip) + panneau(nel).xf_full(ip)) * DABS(panneau(nel).hight) *  DABS(dcos (angEffortT))			
varolitemp  = varolitemp   - .5 * (panneau(nel).xi_full(ip) + panneau(nel).xf_full(ip)) * DABS(panneau(nel).hight) *  DABS(cos (angEffortT))			
if  (((panneau(nel).ang .ne. 90)  .AND. (panneau(nel).icha .EQ. 1.)) &
.OR. ((panneau(nel).ang .ne. 360) .AND. (panneau(nel).icha .EQ. 1.)) &
.OR. ((panneau(nel).ang .ne. 270) .AND. (panneau(nel).icha .EQ. 1.))) then	  
do i=1,npt(ip,nel)
varolitemp  = varolitemp   - .5* DABS (cos (angEffortT))*(cha(i,2,ip,nel)+cha(i,3,ip,nel)) * 1./ npt(ip,nel)* DABS(panneau(nel).hight) 
varolibversh= varolibversh + .5* DABS (cos (angEffortT))*(cha(i,2,ip,nel)+cha(i,3,ip,nel)) * 1./ npt(ip,nel)* DABS(panneau(nel).hight) 
enddo

else
endif


!*******************************************************
!Composante verticale vers le bas s'additionne au poids
!*******************************************************

elseif (((panneau(nel).kse .eq. 2.) .AND. (panneau(nel).ang .GT. 90.)  .AND. (panneau(nel).ang .LE. 270.))  .OR. &
        ((panneau(nel).kse .eq. 1.) .AND. (panneau(nel).ang .GE. 0.)   .AND. (panneau(nel).ang .LE. 90.))   .OR. &
        ((panneau(nel).kse .eq. 1.) .AND. (panneau(nel).ang .GT. 270.) .AND. (panneau(nel).ang .LE. 360.)))  then

varolihversb=varolihversb+.5 * (panneau(nel).xi_full(ip)+panneau(nel).xf_full(ip)) *DABS(cos(angEffortT))* DABS(panneau(nel).hight) 			
varolitemp = varolitemp + .5 * (panneau(nel).xi_full(ip)+panneau(nel).xf_full(ip)) *DABS(cos(angEffortT))* DABS(panneau(nel).hight) 
if (((panneau(nel).ang .ne. 270.) .AND. (panneau(nel).icha .EQ. 1.)) .OR. & 
    ((panneau(nel).ang .ne. 90.)  .AND. (panneau(nel).icha .eq. 1.)) .OR. & 
    ((panneau(nel).ang .ne. 360.) .AND. (panneau(nel).icha .EQ. 1.))) then
do i=1,npt(ip,nel)
varolitemp  =varolitemp + .5* DABS (cos (angEffortT))*(cha(i,2,ip,nel)+cha(i,3,ip,nel))* 1./ npt(ip,nel)* DABS(panneau(nel).hight) 
varolihversb=varolihversb+.5* DABS (cos (angEffortT))*(cha(i,2,ip,nel)+cha(i,3,ip,nel))* 1./ npt(ip,nel)* DABS(panneau(nel).hight) 
enddo

else
endif

endif		
enddo  ! Fin boucle sur les panneaux 

varolitemp = varolitemp * 9806.65				   ! en N/m pression linéique 
varoli(ip)=varoli(ip)+varolitemp   
		
                                
if (ipoids.eq.1) then
PEffortT(ip) = pdstotal/width + varoli(ip)         ! en N/m pression linéique 
else
PEffortT(ip)= varoli(ip)
endif  

!*********************************************************************************************
!     subroutine Effort tranchant 
!     ===========================
!
!  methodEffortT = 3  : Méthode où 2 forces ponctuelles sont appliquées au step 10,11 et 40,41
!
!*********************************************************************************************


varoli(ip)=0
varolitemp=0

do nel=1,neto
	do i=1,50
	cha(i,2,ip,nel)=cha(i,2,ip,nel)+0.
	cha(i,3,ip,nel)=cha(i,3,ip,nel)+0.
	enddo
enddo

Deltaoli=.2*width
FFoli=TEffortT(ip)*(width/(2.*Deltaoli))
FF=FFoli/(2.*nelH)
Noli=Deltaoli/(width/50.)

stepinfEffortT=Noli
stepinfEffortT1=Noli+1
stepsupEffortT=51.- Noli
stepsupEffortT1=50.- Noli

do ioli=1,nelH
bboli=pointeur(ioli)
aqoli(ioli)=(DABS(panneau(bboli).hight)*(DABS(COS((pi/180)*panneau(bboli).ang))))
FFF(ioli)=(((50.*FF)/9806.65) /(width*aqoli(ioli))) 

if (((panneau(bboli).kse .eq. 1.) .AND. (panneau(bboli).ang .GT. 270.) .AND. (panneau(bboli).ang .LE. 360.)) .OR. &
    ((panneau(bboli).kse .eq. 1.) .AND. (panneau(bboli).ang .GE. 0.)   .AND. (panneau(bboli).ang .LE. 90.))  .OR. & 
    ((panneau(bboli).kse .eq. 2.) .AND. (panneau(bboli).ang .GT. 90.)  .AND. (panneau(bboli).ang .LE. 270. ))) then 

cha(stepinfEffortT,2,ip,bboli) =cha(stepinfEffortT,2,ip,bboli)  - FFF(ioli)
cha(stepinfEffortT,3,ip,bboli) =cha(stepinfEffortT,3,ip,bboli)  - FFF(ioli)
cha(stepinfEffortT1,2,ip,bboli)=cha(stepinfEffortT1,2,ip,bboli) - FFF(ioli)
cha(stepinfEffortT1,3,ip,bboli)=cha(stepinfEffortT1,3,ip,bboli) - FFF(ioli)
else
cha(stepinfEffortT,2,ip,bboli) =cha(stepinfEffortT,2,ip,bboli)  + FFF(ioli)
cha(stepinfEffortT,3,ip,bboli) =cha(stepinfEffortT,3,ip,bboli)  + FFF(ioli)
cha(stepinfEffortT1,2,ip,bboli)=cha(stepinfEffortT1,2,ip,bboli) + FFF(ioli)
cha(stepinfEffortT1,3,ip,bboli)=cha(stepinfEffortT1,3,ip,bboli) + FFF(ioli)
endif

if (((panneau(bboli).kse .EQ. 2.) .AND. (panneau(bboli).ang .GT. 90.)  .AND. (panneau(bboli).ang .LE. 270. ))  .OR. & 
    ((panneau(bboli).kse .eq. 1.) .AND. (panneau(bboli).ang .GE. 0.)   .AND. (panneau(bboli).ang .LE. 90.))    .OR. &
    ((panneau(bboli).kse .eq. 1.) .AND. (panneau(bboli).ang .GT. 270.) .AND. (panneau(bboli).ang .LE. 360.))) then

cha(stepsupEffortT,2,ip,bboli) =cha(stepsupEffortT,2,ip,bboli) +FFF(ioli)
cha(stepsupEffortT,3,ip,bboli) =cha(stepsupEffortT,3,ip,bboli) +FFF(ioli)
cha(stepsupEffortT1,2,ip,bboli)=cha(stepsupEffortT1,2,ip,bboli)+FFF(ioli)
cha(stepsupEffortT1,3,ip,bboli)=cha(stepsupEffortT1,3,ip,bboli)+FFF(ioli) 
else 
cha(stepsupEffortT,2,ip,bboli) =cha(stepsupEffortT,2,ip,bboli) -FFF(ioli)
cha(stepsupEffortT,3,ip,bboli) =cha(stepsupEffortT,3,ip,bboli) -FFF(ioli) 
cha(stepsupEffortT1,2,ip,bboli)=cha(stepsupEffortT1,2,ip,bboli)-FFF(ioli)
cha(stepsupEffortT1,3,ip,bboli)=cha(stepsupEffortT1,3,ip,bboli)-FFF(ioli)
endif
enddo 

XEffortT(ip) = XEffortT(ip)*width
MEffortT(ip) = bm11(ip) - (.5 * PEffortT(ip) * XEffortT(ip) * (XEffortT(ip)-width))
MEffortT(ip) = MEffortT(ip) !/100h0.
!!!bm11(ip) = MEffortT(ip) ! pour pas modifier le fichier up !!!



enddo   

!endif

!
!!***********************************************************************************
!!     subroutine Effort tranchant 
!!     ===========================
!!
!!  methodEffortT = 1  : Méthode où la longueur du modèle & le moment sont modifiés
!!
!!***********************************************************************************
!
!if (methodEffortT .EQ. 1) then
!
!	XEffortT(ip) = XEffortT(ip)*width  ! section pour la longueur initiale non modifiée L_avant calcul
!
!	widthEffortT(ip) = 2.*XEffortT(ip)+2.*TEffortT(ip)/PEffortT(ip) ! pense au cas P = 0
!
!	!width = widthEffortT(ip)            si on développe width en vecteur et non plus en grandeur scalaire
!
!	MEffortT(ip) = bm11(ip) - (.5 * PEffortT(ip) * XEffortT(ip) * (XEffortT(ip)-widthEffortT(ip)))
!
!	MEffortT(ip) = MEffortT(ip)/1000.
!
!	bm11(ip) = MEffortT(ip) !pour pas changer dans edge_moment
!
!!***********************************************************************************
!!     subroutine Effort tranchant 
!!     ===========================
!!
!!  methodEffortT = 2  : Méthode où la masse volumique ou le chargement sont modifiés 
!!  
!!  ==> NE PAS UTILISER: Méthode écartée directement dans le TFE
!!***********************************************************************************
!
!!elseif (methodEffortT .EQ. 2) then
!
!!XEffortT(ip) = XEffortT(ip)*width
!!Poli1(ip)= 2.* TEffortT(ip) / (width - 2.*XEffortT(ip))
!!rhooli=(Poli1(ip)-Pdstotal-PEffortT(ip))/volume  
!
!!do nel=1,neto
!!cha(1,1,is,nel)=PEffortT(ip) / neto
!!enddo
!	
!!MEffortT(ip)= bm11(ip) - (.5 * Poli1(ip) * XEffortT(ip) * (XEffortT(ip) - width))
!
!!MEffortT(ip)= MEffortT(ip) / 1000. ! en kN.m
!
!!bm11(ip)= MEffortT(ip)

return
end
