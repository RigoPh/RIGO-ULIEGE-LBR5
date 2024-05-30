subroutine spepite(obj_,derivee,iderivee,iprint_)                   


use param_section


implicit double precision(a-h,o-z)
dimension derivee(ntot(iboat))                                           

! ====================================================================================================
!
!     subroutine pepite
!     calcul du cout de redressage par panneau.
!	et sensibilités
!
!     crée: 22.03.06       j.pirard                     
! ====================================================================================================

call pepitemain(obj_,iprint_)


!	Calcul Dérivées de la fonction objectif

if (iderivee.eq.1) then

k=0
do 1 nel=1,neto
  nbrxi=nvar(nel,iboat)

  if (nbrxi.eq.0) goto 1

      if(itype(nel).ne.5) then
  
	do i=1,nbrxi
      j=nxit(i,nel,iboat)		! no de ref de chaque var. de conception

	  derivee(k+i) = derivee(k+i) + dtotalstrait(nel,j)

    enddo

  else
    do i=1,nbrxi
      derivee(k+i)= derivee(k+i) + 1.e-10
    enddo
  endif
  k=k+nbrxi
  1	continue

endif

return
end


function dtotalstrait(nel,var)
! ====================================================================================================
! calcul sensibilites   
! ====================================================================================================
!
! calcule dif finies +-12%   
!
!
! ====================================================================================================


use param_section


implicit double precision(a-h,o-z)
integer*4 var 

! ====================================================================================================

if(var.eq.1) then
   vari = panneau(nel).delta
   vari1=	vari+0.01*vari
   vari2=  vari-0.01*vari
   panneau(nel).delta = vari1
   call pepitemain(total1,0)
   panneau(nel).delta = vari2
   call pepitemain(total2,0)
   dtotalstrait=(total1-total2)/(0.02*vari)
   panneau(nel).delta = vari
endif

! ====================================================================================================
if(var.eq.2) then
   vari = panneau(nel).hya
vari1=	vari+0.01*vari
vari2=vari-0.01*vari
panneau(nel).hya=vari1
call pepitemain(total1,0)
panneau(nel).hya=vari2
call pepitemain(total2,0)
dtotalstrait=(total1-total2)/(0.02*vari)
panneau(nel).hya=vari
endif
! ====================================================================================================
if(var.eq.3) then
vari = panneau(nel).dya
vari1=	vari+0.01*vari
vari2=vari-0.01*vari
panneau(nel).dya=vari1
call pepitemain(total1,0)
panneau(nel).dya=vari2
call pepitemain(total2,0)
dtotalstrait=(total1-total2)/(0.02*vari)
panneau(nel).dya=vari
endif
! ====================================================================================================
if(var.eq.4) then
vari = panneau(nel).wya
vari1=	vari+0.01*vari
vari2=vari-0.01*vari
panneau(nel).wya=vari1
call pepitemain(total1,0)
panneau(nel).wya=vari2
call pepitemain(total2,0)
dtotalstrait=(total1-total2)/(0.02*vari)
panneau(nel).wya=vari
endif
! ====================================================================================================
if(var.eq.5) then
vari = panneau(nel).epsa
vari1=	vari+0.01*vari
vari2=vari-0.01*vari
panneau(nel).epsa=vari1                                       
call pepitemain(total1,0)
panneau(nel).epsa=vari2                                       
call pepitemain(total2,0)
dtotalstrait=(total1-total2)/(0.02*vari)
panneau(nel).epsa=vari                                        
endif
! ====================================================================================================
if(var.eq.6) then
vari = panneau(nel).hxr
vari1=	vari+0.01*vari
vari2=vari-0.01*vari
panneau(nel).hxr=vari1
call pepitemain(total1,0)

panneau(nel).hxr=vari2
call pepitemain(total2,0)
dtotalstrait=(total1-total2)/(0.02*vari)
panneau(nel).hxr=vari
endif
! ====================================================================================================
if(var.eq.7) then
vari = panneau(nel).dxr
vari1=	vari+0.01*vari
vari2=vari-0.01*vari
panneau(nel).dxr=vari1
call pepitemain(total1,0)
panneau(nel).dxr=vari2
call pepitemain(total2,0)
dtotalstrait=(total1-total2)/(0.02*vari)
panneau(nel).dxr=vari
endif
! ====================================================================================================
if(var.eq.8) then
vari = panneau(nel).wxr
vari1=	vari+0.01*vari
vari2=vari-0.01*vari
panneau(nel).wxr=vari1
call pepitemain(total1,0)
panneau(nel).wxr=vari2
call pepitemain(total2,0)
dtotalstrait=(total1-total2)/(0.02*vari)
panneau(nel).wxr=vari
endif
! ====================================================================================================
if(var.eq.9) then
vari = panneau(nel).epsr
vari1=	vari+0.01*vari
vari2=vari-0.01*vari
panneau(nel).epsr=vari1
call pepitemain(total1,0)
panneau(nel).epsr=vari2
call pepitemain(total2,0)
dtotalstrait=(total1-total2)/(0.02*vari)
panneau(nel).epsr=vari
endif
! ====================================================================================================

return
end function



subroutine pepitemain(totalstrait,iprint_)

! ====================================================================================================
!  la fonction analytique "cout de redressage est   
!  calculee ici. resultat : var "totalstrait" (euro)
! ====================================================================================================
!
!
!	paramètres
!	delta_cadre	:epsa
!	delta_lisse	:epsr
!	thickness	:delta
!	famille		:fami
!	special		:toujours 0 car il ne s'agit pas d'une structure spéciale (bulbe...)
!	double_fond	:itype=6 alors double_fond = 1
!	grade		:spec, matériau du panneau
!	sompoids_l0ngueur_lot : plot : poids des éléments par lot/longueur du lot. 
!	poidstole_marsurfpan  : ptol : poids de la tôle d'un lot/ surface de tôle.
!
!	formule pepite
!	a:intermediate[0] = (datad.delta_cadre-2822.565789473684e0)/74.90830341976067e0;
!	b:intermediate[1] = (datad.delta_lisse-711.6447368421053e0)/45.298042864427465e0;
!	c:intermediate[2] = (datad.thickness-7.0855263157894735e0)/3.410043752894369e0;
!	d:intermediate[3] = (datad.famille-2.7565789473684212e0)/0.9420051217244927e0;
!	e:intermediate[4] = (datad.special_num-0.03289473684210526e0)/0.1789507001514622e0;
!	f:intermediate[5] = (datad.double_fond_num-0.046052631578947366e0)/0.21029200100905684e0;
!	g:intermediate[6] = (datad.grade_num-0.19078947368421054e0)/0.39422239045175456e0;
!	h:intermediate[7] = (datad.sompoids_l0ngueur_lot-4.985748581635476e0)/3.680087037555551e0;
!	o:intermediate[8] = (datad.poidstole_marsurfpan-0.06084666512508383e0)/0.020034653830364162e0;
!	p:intermediate[9] = tanh(1.9368069209472492e0 + (-0.9264288509304361e0 * intermediate[0]) + (-1.3851816262063843e0 * intermediate[1]) + (-0.9130433232870326e0 * intermediate[2]) + (0.8329484301877387e0 * intermediate[3]) + (-0.5563069515492396e0 * intermediate[4]) + (-0.8472889924229183e0 * intermediate[5]) + (-0.586404310688777e0 * intermediate[6]) + (1.5063505606322518e0 * intermediate[7]) + (-4.066987573526498e0 * intermediate[8]));
!	r:intermediate[10] = tanh(0.8087474077250932e0 + (-0.34896105918057957e0 * intermediate[0]) + (-0.4592147665094478e0 * intermediate[1]) + (0.3306327933088029e0 * intermediate[2]) + (0.6757548968905716e0 * intermediate[3]) + (-0.39587242378578624e0 * intermediate[4]) + (-0.4333421624891615e0 * intermediate[5]) + (-0.4087872241356317e0 * intermediate[6]) + (0.4932501122941787e0 * intermediate[7]) + (-2.1446197463678445e0 * intermediate[8]));
!	s:intermediate[11] = tanh(-0.10228980686314161e0 + (-1.2263729691363052e0 * intermediate[0]) + (-1.0167071742037233e0 * intermediate[1]) + (-0.1725226923833523e0 * intermediate[2]) + (0.08599557094213019e0 * intermediate[3]) + (0.39550298448896226e0 * intermediate[4]) + (0.17610832730222192e0 * intermediate[5]) + (0.32700148711362204e0 * intermediate[6]) + (-1.208027714084693e0 * intermediate[7]) + (-2.2654562759753216e0 * intermediate[8]));
!	t:intermediate[12] = tanh(-0.1822176347472198e0 + (-0.036204229434917856e0 * intermediate[0]) + (0.48046284302543396e0 * intermediate[1]) + (1.8389296314588444e0 * intermediate[2]) + (0.6332866724703521e0 * intermediate[3]) + (-1.1473156019349544e0 * intermediate[4]) + (-0.9035691846240345e0 * intermediate[5]) + (0.3989099671440577e0 * intermediate[6]) + (-0.4678695574043407e0 * intermediate[7]) + (0.3583131586888217e0 * intermediate[8]));
!	u:intermediate[13] = tanh(5.872989176135237e0 + (0.3512696038203212e0 * intermediate[0]) + (2.115126151076024e0 * intermediate[1]) + (-2.303121469828879e0 * intermediate[2]) + (-2.1199873056315917e0 * intermediate[3]) + (-0.16875097228308644e0 * intermediate[4]) + (-0.8363746917958408e0 * intermediate[5]) + (1.893139241719546e0 * intermediate[6]) + (3.934191388280177e0 * intermediate[7]) + (-0.09820356987358925e0 * intermediate[8]));
!	v:intermediate[14] = 0.17659278965335456e0 + (0.15298110516796812e0 * (3.4798485158795147e0 + (1.1383230531544564e0 * intermediate[9]) + (-1.6408502476638214e0 * intermediate[10]) + (0.7660321433985156e0 * intermediate[11]) + (0.9502124670035074e0 * intermediate[12]) + (-3.311401430855795e0 * intermediate[13])));
!
!	30 lots max, 10 familles max.
! ====================================================================================================   


use param_section


implicit double precision(a-h,o-z)

double precision, allocatable,save :: plot(:)
double precision, allocatable,save :: ptol(:)
double precision, allocatable,save :: ola(:)
double precision, allocatable,save :: v(:)
double precision, allocatable,save :: vvsurf(:)
double precision, allocatable,save :: tpmcarre(:)
double precision, allocatable,save :: vsurface(:)

allocate (plot(neto))
allocate (ptol(neto))
allocate (ola(neto))
allocate (v(neto))
allocate (vvsurf(neto))
allocate (tpmcarre(neto))
allocate (vsurface(neto))

!	Initialisation			
call annuld(plot,neto)
call annuld(ptol,neto)
call annuld(ola,neto)
call annuld(v,neto)
call annuld(vvsurf,neto)
call annuld(tpmcarre,neto)
call annuld(vsurface,neto)

red=0.

do 123 i=1,neto

   if(itype(i).ne.5) then
	
! ====================================================================================================
!   variables du panneau  
! ====================================================================================================

      delta =  panneau(i).delta !vepaiss(i)
		  epsa  =  panneau(i).epsa  !deltacsc(i)       
		  epsr  =  panneau(i).epsr  !deltar(i)
		  hya   =  panneau(i).hya	  !vhac(i)
		  hxr   =  panneau(i).hxr	  !vhar(i)
		  dya   =  panneau(i).dya	  !veac(i)
		  dxr   =  panneau(i).dxr	  !vear(i)
		  wya   =  panneau(i).wya	  !vlsc(i)
		  tya   =  panneau(i).tya	  !vesc(i)
		  wxr   =  panneau(i).wxr	  !vlsr(i)
		  txr   =  panneau(i).txr	  !vesr(i)
		  vhight= -panneau(i).hight !hauteur du panneau


! ====================================================================================================
!   Corrosion	
! ====================================================================================================

		  delta= delta + panneau(i).corro(1)
											
! ====================================================================================================
!   Epaisseur de corrosion pour cadres
! ====================================================================================================

		  dya  = dya + panneau(i).corro(2)
		  tya  = tya + panneau(i).corro(2)
		  epais = epais + panneau(i).corro(2)					

! ====================================================================================================
!	Epaisseur de corrosion pour lisses
! ====================================================================================================


		  dxr  = dxr + panneau(i).corro(3)
		  txr  = txr + panneau(i).corro(3)

! ====================================================================================================


		  poids1=	2*(vhight*(width/epsa)*((dya*hya)+(wya*tya))    &
		           +(vhight/epsr)*((dxr*hxr)+(wxr*txr))*width     &
		           + width*vhight*delta)*spec(indMateriau(i))*1.2/9810.
	
		  vsurface(i)=width*vhight


! ====================================================================================================
!   Transform unit messure 
! ====================================================================================================


		  delta = delta*1000.
		  epsa = epsa*1000.
		  epsr = epsr*1000.
		  hya = hya*1000.
		  hxr = hxr*1000.
		  dya = dya*1000.
		  dxr = dxr*1000.
		  wya = wya*1000.
		  tya = tya*1000.
		  wxr = wxr*1000.
		  txr = txr*1000.		
     
! ====================================================================================================
     	
		  if (sigy(indMateriau(i)).le.200000000) then
         write (iu_11(iboat),*)'le redressage n''est calcule que pour de l''acier'
			   goto 123
	    endif

		  ptol(i) = delta*spec(indMateriau(i))/10000000.

		  do j=1,neto
		     !if (panneau(j).lot.eq.j) then
			 if (panneau(j).lot.eq.panneau(i).lot) then
		        plot(j) = plot(j)+ poids1
		     endif
		  enddo
	 endif
123	continue
		
do jk=1,neto
   if(plot(jk).lt.125.) then
      tpmcarre(jk)=1.-plot(jk)*0.8/125.
   else
      tpmcarre(jk)=0.2-(plot(jk)-125.)*0.15/(1025.-125.)
	 endif
enddo

do kk=1,neto
   if((panneau(kk).fami.ne.0).and.(panneau(kk).lot.ne.0)) then
      !klm=panneau(kk).lot
      ola(kk)=tpmcarre(kk) !tpmcarre(klm)
   else
      klm=0      !dad 10.12.2007
      ola(kk)=0.
   endif

	 v(kk) = ola(kk)*vsurface(kk)  !pour avoir coût en heures


!	 if(mpix.eq.1) then
!	    write(iu_redress(iboat),*) kk
!	    if (klm.ne.0) then
!	       write(iu_redress(iboat),226) ola(kk),v(kk),plot(klm)
!	    else
!	       plot_null=0.
!	       write(iu_redress(iboat),226) ola(kk),v(kk),plot_null
!	    endif


!226		format (5(2x,f10.3))

!   endif

	 eqpc1=labour_cost(indMateriau(kk))  ! (=euro/heure) ,pour transfert de rro en euro
	 
	 red=red+v(kk)
enddo				!boucle kk


totalstrait = red*eqpc1				!!! catalin

!if(mpix.eq.1) then
!   write(iu_redress(iboat),*) ' '
!   write(iu_redress(iboat),*) 'cout total de redressage = ', red,' heures'
!   write(iu_redress(iboat),*) 'cout total de redressage = ', totalstrait,' euro'
!   write(iu_redress(iboat),*) ' '
!endif

if (iprint_.ne.0) then		!oct06
  !TODO A VOIR PQ PAS SANS 666 COMME POUR LES AUTRES SOUS-ROUTINES
  write(iu_redress(iboat),*) ' '
  write(iu_redress(iboat),*) 'cout total de redressage = ', red,' heures'
  write(iu_redress(iboat),*) 'cout total de redressage = ', totalstrait,' euro'
endif

deallocate (plot)
deallocate (ptol)
deallocate (ola)
deallocate (v)
deallocate (vvsurf)
deallocate (tpmcarre)
deallocate (vsurface)

return
end
 
 
