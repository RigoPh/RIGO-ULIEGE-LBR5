subroutine update()

use param_section

implicit double precision (a-h,o-z)
! ====================================================================================================
!	Cette sous-routine remet à jour toutes les valeurs des structures panneaux
!	qui sont calculées en fonction des valeurs de conception
!	=> si une valeur de conception change, on doit lancer cette sous-routine
!
! ====================================================================================================

do nel=1,neto
	call updatepanneau(nel)
enddo



return
end

	
subroutine updatepanneau(nel)
! ====================================================================================================
!	Cette sous-routine remet à jour toutes les valeurs des structures d'1 panneau
!	qui sont calculées en fonction des valeurs de conception
!	=> si une valeur de conception change, on doit lancer cette sous-routine
!
! ====================================================================================================
!

use param_section

implicit double precision (a-h,o-z)


!	Initialisation
!call annuld(panneau(nel).hxtr,10)
!call annuld(panneau(nel).dxtr,10)
!call annuld(panneau(nel).wxtr,10)
!call annuld(panneau(nel).txtr,10)


!	Mise à jour de champs calculés du panneau
if(itype(nel).eq.5)then
	sh=e(indMateriau(nel))/(2.*(1.+0.))
else
	sh=e(indMateriau(nel))/(2.*(1.+eta(indMateriau(nel))))
endif
panneau(nel).sh = sh



!	Mise à jour des coefficients de raideur du panneau (const)
!	rem.:si besoin, faudra faire carac2, etc...
!	ici, on le fait juste pour récupérer !!! delt et delt2 !!!

delta = panneau(nel).delta
hya   = panneau(nel).hya
dya   = panneau(nel).dya
wya   = panneau(nel).wya
tya   = panneau(nel).tya
epsa  = panneau(nel).epsa
hxr   = panneau(nel).hxr
dxr   = panneau(nel).dxr
wxr   = panneau(nel).wxr
txr   = panneau(nel).txr
epsr  = panneau(nel).epsr
phil  = panneau(nel).phil
q     = panneau(nel).q
epais = panneau(nel).epais
mt    = panneau(nel).mt
ksr   = panneau(nel).ksr
ksa   = panneau(nel).ksa
kst   = panneau(nel).kst
delt  = panneau(nel).delt
delt2 = panneau(nel).delt2

!call carac(e(indMateriau(nel)),eta(indMateriau(nel)),epsa,epsr,	 &
!             delta,hya,dya,wya,tya,hxr,dxr,wxr,txr,              &
!             panneau(nel).hxtr,panneau(nel).dxtr,                &
!			 panneau(nel).wxtr,panneau(nel).txtr,                &
!             const,sh,fam,mt,									 &
!			 ksr,ksa,kst,spec(indMateriau(nel)),                 &
!			 poids9,dabs(phil),q,width,delt,delt2,				 &
!			 epais,itype(nel),isect(nel),                        &
!             aire,aiy,aix,sredy,sredx,tork) ! <- calculés dans carac

return
end
