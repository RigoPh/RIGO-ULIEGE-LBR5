subroutine objct1(obj_,derivee,iderivee,iprint_)


use param_section


implicit double precision(a-h,o-z)
dimension derivee(ntot(iboat))


!==============================================================================
!     calcul de la fonction objectif cout (obj) après la subr. conlin
!
!     16-11-2001 : sur-épaisseurs de corrosion
!     11-01-2002 : nouveaux paramètres coût (p9x,p9y,c2,c3,dw2,dw3)
!     23-05-2003 : correction ds le calcul des sensibilités (errata)
!	   4-02-2009 : création sous-routine cout.fr
!
!
!==============================================================================
	
	
!	Calcul Fonction objectif


call couttotal(obj_)

if (iprint_.ne.0) then
   write(iu_31(iboat),*)
   write(iu_31(iboat),*)' total for the structure (with gross thickness)'
   write(iu_31(iboat),*)' ----------------------------------------------'
   write(iu_31(iboat),*)'  cout - cost = ',obj_,' €'
endif



!	Calcul Dérivées de la fonction objectif

if (iderivee.eq.1) then

   k=0
   do nel=1,neto
      nbrxi=nvar(nel,iboat)
      
      if (nbrxi.ne.0) then
      
         if(itype(nel).ne.5) then
         
            do i=1,nbrxi
               j=nxit(i,nel,iboat)		! no de ref de chaque var. de conception
               
               derivee(k+i) = derivee(k+i) + dcout(nel,j)
               
            enddo
      
      
         else
            do i=1,nbrxi
               derivee(k+i) = derivee(k+i) + 1.e-10
            enddo
         endif
         k=k+nbrxi
      endif
   enddo

endif


return
end

!==============================================================================
!	CALCUL de la dérivée du COUT (simplifie) par rapport à la variable var du panneau nel
!==============================================================================
double precision function dcout(nel,var)


use param_section

implicit double precision (a-h,o-z)
integer*4 var

!		TODO : remettre ici lecture corrosion + variables, etc

if(itype(nel).ne.5) then											
   delta= panneau(nel).delta + panneau(nel).corro(1)
else																
   delta =0.000											
endif																


!	dcor2   ! epaisseur de corrosion pour cadres
	dya   = panneau(nel).dya + panneau(nel).corro(2)
	tya   = panneau(nel).tya + panneau(nel).corro(2)
	epais = panneau(nel).epais + panneau(nel).corro(2)						


!	dcor3   ! epaisseur de corrosion pour lisses
	dxr   = panneau(nel).dxr + panneau(nel).corro(3)
	txr   = panneau(nel).txr + panneau(nel).corro(3)

	hya   = panneau(nel).hya
	wya   = panneau(nel).wya
	epsa  = panneau(nel).epsa
	hxr   = panneau(nel).hxr
	wxr   = panneau(nel).wxr
	epsr  = panneau(nel).epsr
	phil  = dabs(panneau(nel).phil)	!!!
	q     = panneau(nel).q

  temp  = phil * q * width * pi/180.
	dens  = spec(indMateriau(nel))/9.81

	drefy_local=drefy(indMateriau(nel))
	   c3_local=   c3(indMateriau(nel))
	dw3_local  =  dw3(indMateriau(nel))
	drefx_local=drefx(indMateriau(nel))
	   c2_local=   c2(indMateriau(nel))
	dw2_local  =  dw2(indMateriau(nel))

	if(((refthick_input.eq.1).and.(itype(nel).ne.5))) then

		! On utilise drefy_local qui est :
		! - soit drefy
		! - soit dref_b, dref_c ou dref_l (plat boudin, Té composite, Té laminé
		! Et pas drefy=dref_b car ça modifierait les valeurs de drefy qui est une variable globale
		!  => on modifierait les données

	   if (panneau(nel).itype_transv.eq.14)then   !Plat Boudin
			drefy_local=dref_b(indMateriau(nel))
              c3_local=c_pb(indMateriau(nel))
	        dw3_local=dw_b(indMateriau(nel))

	   elseif(panneau(nel).itype_transv.eq.7) then   ! Té Composite
			drefy_local=dref_c(indMateriau(nel))
              c3_local=c_tc(indMateriau(nel))
              dw3_local=dw_c(indMateriau(nel))

	   elseif(panneau(nel).itype_transv.eq.17)then   ! Té Laminé
			drefy_local=dref_l(indMateriau(nel))
              c3_local=c_tl(indMateriau(nel))
              dw3_local=dw_l(indMateriau(nel))

	   else 
	      write(* ,*)'type de profilé inexistant'
	   endif

	   if (panneau(nel).itype_longit.eq.14)then   !Plat Boudin
			drefx_local=dref_b(indMateriau(nel))
              c2_local=c_pb(indMateriau(nel))
	        dw2_local=dw_b(indMateriau(nel))

	   elseif(panneau(nel).itype_longit.eq.7) then   ! Té Composite
			drefx_local=dref_c(indMateriau(nel))
              c2_local=c_tc(indMateriau(nel))
	        dw2_local=dw_c(indMateriau(nel))

	   elseif(panneau(nel).itype_longit.eq.17)then   ! Té Laminé
			drefx_local=dref_l(indMateriau(nel))
              c2_local=c_tl(indMateriau(nel))
	        dw2_local=dw_l(indMateriau(nel))
	   else 
	      write(* ,*)'type de profilé inexistant'
	   endif

      endif

!TODO à rendre plus propre
! (car là labour_cost(indMateriau(nel))/(1000.*c1(indMateriau(nel))) a été remplacé par labour_cost/(1000*C1)

if(var.eq.1) then

     dcout=temp*c1(indMateriau(nel)) *                                 &
   	 ((1.+(2.*delta-dref(indMateriau(nel)))*1000.*                     &
   		dc1(indMateriau(nel)))*dens                                    &
         + rend_global* panneau(nel).rend *                       &
   		 labour_cost(indMateriau(nel))/(1000.*c1(indMateriau(nel))) *1000.*p10(indMateriau(nel)) *          &
   		dp10(indMateriau(nel)) *1000.)


else if (var.eq.2) then
     dcout= temp * dens * (1.0+(dya-dref(indMateriau(nel)))*           &
   	        1000.*dc1(indMateriau(nel)))                               &
            * (1.+dw3_local) *c3_local   *dya/epsa


else if (var.eq.3) then

     dcout= (temp/epsa) *                                              &
     (  (1.+(dya-dref(indMateriau(nel)) )*1000.*                       &
   	 dc1(indMateriau(nel)) )*(1.0+dw3_local)*hya*                      &
   	 c3_local *dens                                                    &
   	 + (1.0+dw3_local)*(dya*hya+tya*wya)*1000.*                        &
   	 dc1(indMateriau(nel)) *c3_local *dens     +                       &
     1000.0*c8(indMateriau(nel)) *dc8(indMateriau(nel)) *              &
 	   (2.-ialt(indMateriau(nel)) ) + rend_global*                     &
 	   panneau(nel).rend * labour_cost(indMateriau(nel))/(1000.*c1(indMateriau(nel)))                 &
 	   *1000.0*c1(indMateriau(nel)) * 1000.0*(p5(indMateriau(nel)) *   &
 	   dp5(indMateriau(nel)) +p9y(indMateriau(nel)) *                  &
 	   dp9y(indMateriau(nel)) ))


else if (var.eq.4) then

     dcout= temp  * dens *                                             &
     (1.0+(dya-dref(indMateriau(nel)))*1000.0*dc1(indMateriau(nel)))   &
     * (1.0+dw3_local) * c3_local * tya/epsa


else if (var.eq.5) then

      dcout= -(temp/(epsa*epsa)) *                                     &
      ( c3_local *dens*(1.0+(dya-dref(indMateriau(nel)) )*             &
   	  1000.0*dc1(indMateriau(nel)) )*(1.+dw3_local)*                   &
   	  (dya*hya+tya*wya) + (1.+(dya-drefy_local)*1000.*                 &
   	  dc8(indMateriau(nel)) )*c8(indMateriau(nel)) *                   &
   	  (2.-ialt(indMateriau(nel)) )                                     &
   	  + rend_global* panneau(nel).rend *                          &
   	  labour_cost(indMateriau(nel))/(1000.*c1(indMateriau(nel))) *1000.*c1(indMateriau(nel)) *              &
      ( p5(indMateriau(nel))  *(1.+(dya-dref(indMateriau(nel)) )*      &
   	  1000.*dp5(indMateriau(nel)) ) + p9y(indMateriau(nel)) *          &
   	  (1.+(dya-dref(indMateriau(nel)) )*1000.*                         &
   	  dp9y(indMateriau(nel)) )                                         &
      + (p6(indMateriau(nel)) +ber(indMateriau(nel)) *                 &
   	  bet(indMateriau(nel)) *p7(indMateriau(nel)) )/epsr )  )


else if (var.eq.6) then

     dcout= temp * dens * (1.0+(dxr-dref(indMateriau(nel)) )*1000.*    &
   	 dc1(indMateriau(nel)) ) * (1.0+dw2_local) *                       &
   	 c2_local  * dxr/epsr


else if (var.eq.7) then

     dcout= (temp/epsr) *                                              &
     (  (1.+(dxr-dref(indMateriau(nel)) )*1000.*                       &
   	 dc1(indMateriau(nel)) )*(1.0+dw2_local)*hxr*                      &
   	 c2_local *dens + (1.0+dw2_local)*                                 &
   	 (dxr*hxr+txr*wxr)*1000.*dc1(indMateriau(nel)) *                   &
   	 c2_local *dens     +                                              &
     1000.0* c8(indMateriau(nel)) *dc8(indMateriau(nel)) *             &
   	 (2.-ialr(indMateriau(nel)) )    +                                 &
   	 rend_global* panneau(nel).rend * labour_cost(indMateriau(nel))/(1000.*c1(indMateriau(nel)))                       &
   	 *1000.0*c1(indMateriau(nel)) * 1000.0*(p4(indMateriau(nel)) *     &
   	 dp4(indMateriau(nel)) +p9x(indMateriau(nel)) *                    &
   	 dp9x(indMateriau(nel)) ) )


else if (var.eq.8) then

     dcout= temp  * dens * (1.+(dxr-dref(indMateriau(nel)))*1000.0*    &
   	 dc1(indMateriau(nel)) ) * (1.0+dw2_local) *                       &
   	 c2_local  * txr/epsr


else if (var.eq.9) then

       dcout= -(temp/(epsr*epsr)) *                                    &
     	 ( c2_local *dens*(1.0+(dxr-dref(indMateriau(nel)) )*          &
     	 1000.0*dc1(indMateriau(nel)) )*(1.+dw2_local)*                &
     	 (dxr*hxr+txr*wxr) + (1.+(dxr-drefx_local)*1000.*              &
     	 dc8(indMateriau(nel)) )*c8(indMateriau(nel)) *                &
     	 (2.-ialr(indMateriau(nel)) ) + rend_global *                  &
     	 panneau(nel).rend *labour_cost(indMateriau(nel))/(1000.*c1(indMateriau(nel))) *1000.*         &
     	 c1(indMateriau(nel)) * ( p4(indMateriau(nel))  *(1.+          &
     	 (dxr-dref(indMateriau(nel)) )*1000.*dp4(indMateriau(nel)) )   &
       + p9x(indMateriau(nel)) *(1.+(dxr-dref(indMateriau(nel)) )*     &
     	 1000.*dp9x(indMateriau(nel)) ) + (p6(indMateriau(nel)) +      &
     	 ber(indMateriau(nel)) *bet(indMateriau(nel)) *                &
     	 p7(indMateriau(nel)) )/epsa ) )


endif



return
end function
