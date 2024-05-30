subroutine ecriVar(xi_,itt)
! ====================================================================================================



use param_section
!use param_cout
!use param_opti_local

	
implicit double precision(a-h,o-z)
dimension xi_(ntot(iboat))


nc=0

do nel=1,neto

	  
nbrxi=nvar(nel,iboat)

do 123 i=1,nbrxi
       goto(124,125,120,126,127,128,121,129,130),nxit(i,nel,iboat) 

124    panneau(nel).delta=xi_(nc+i)
       goto 123
125    panneau(nel).hya  =xi_(nc+i)
       goto 123
120    panneau(nel).dya  =xi_(nc+i)
       goto 123
126    panneau(nel).wya  =xi_(nc+i)
       goto 123
127    panneau(nel).epsa =xi_(nc+i)
       goto 123
128    panneau(nel).hxr=  xi_(nc+i)
       goto 123
121    panneau(nel).dxr=  xi_(nc+i)
       goto 123
129    panneau(nel).wxr=  xi_(nc+i)
       goto 123
130    panneau(nel).epsr= xi_(nc+i)


123    continue

delta_ = panneau(nel).delta
hya_   = panneau(nel).hya
dya_   = panneau(nel).dya
wya_   = panneau(nel).wya
tya_   = panneau(nel).tya
epsa_  = panneau(nel).epsa
hxr_   = panneau(nel).hxr
dxr_   = panneau(nel).dxr
wxr_   = panneau(nel).wxr
txr_   = panneau(nel).txr
epsr_  = panneau(nel).epsr

call update()

nc=nc+nbrxi

if(itt.eq.1) then					!impression
   write (iu_31(iboat),132) nel,'ep. bord','hâme cad','eâme cad','lsem cad','tsem cad','epsa cad','hâme rai','eâme rai','lsem rai','tsem rai','epsr rai'  
   write (iu_31(iboat),131) delta_,hya_,dya_,wya_,tya_,epsa_,hxr_,dxr_,wxr_,txr_,epsr_
endif

enddo


return

131 format('variables_:',11(e11.4))
132 format('panel n°',i3,2x,11(1x,a8,2x))

stop
end
