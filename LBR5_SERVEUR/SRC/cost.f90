subroutine cost(nel)     

use param_section
use PARAM_SECTION_VECTOR, ONLY : poidsLBR5_init_vector

implicit double precision (a-h,o-z)

! ====================================================================================================
! calcul de la fonction objectif cout dans read_data
! ====================================================================================================

! les variables de conception
! ---------------------------
!       1       delta =  épaisseur du bordage
!       2       hya   =  hauteur de l'âme des aiguilles
!       3       dya   =  épaisseur de l'âme des aiguilles
!       4       wya   =  largeur des semelles des aiguilles
!       5       epsa  =  entredistance entre aiguilles
!       6       hxr   =  hauteur de l'âme des raidisseurs
!       7       dxr   =  épaisseur de l'âme des raidisseurs
!       8       wxr   =  largeur des semelles des raidisseurs
!       9       epsr  =  entredistance entre raidisseurs
!
! variables associées :
! ---------------------
!            tya = épaisseur semelle aiguilles
!            txr = épaisseur semelle raidisseurs
!
!  les variables de conception pour epontille
!  ------------------------------------------
!       1       hya       =       hauteur demi âme ou diam ou côté ext.
!       2       dya       =       épaisseur d'âme
!       3       wya       =       largeur de semelle
!       4       epais     =       épaisseur de paroi mince
!       5       epsa      =       entredistance entre épontilles
!
! ====================================================================================================
!
!     fmat= coût des matériaux (acier)
!     fsou= coût du soudage (energie + consommables)
!     fmdo= coût main d'oeuvre
!     fct = fmat + fsou + fmdo  (d'un panneau nel),
!     obj = coût total          (de la structure complete)
!
! ====================================================================================================



call coutpanneau_mat_bordes(fmat1,nel)
fmat11 = fmat11 + fmat1
call coutpanneau_mat_raidisseurs(fmat2,nel)
fmat22 = fmat22 + fmat2
call coutpanneau_mat_cadres(fmat3,nel)
fmat33 = fmat33 + fmat3
call coutpanneau_sou_raidisseurs(fsou1,nel)
fsou11=fsou11+fsou1
call coutpanneau_sou_cadres(fsou2,fsou3,nel)
fsou22=fsou22+fsou2
fsou33=fsou33+fsou3
call coutpanneau_mdo(nel)
fmdo11 = fmdo11+fmdo1 ! mdo soudage lisses sur nappe
fmdo22 = fmdo22+fmdo2 ! mdo constitution de lisses synthetiques + extra works
fmdo33 = fmdo33+fmdo3 ! mdo soudage cadres sur nappe
fmdo44 = fmdo44+fmdo4 ! mdo constitution de cadres synthetiques + extra works
fmdo55 = fmdo55+fmdo5 ! mdo pour découpage des slot
fmdo66 = fmdo66+fmdo6 ! mdo soudage des tap et goussets
fmdo77 = fmdo77+fmdo7 ! mdo assemblage et soudage des nappes

    
ffmat = fmat1+fmat2+fmat3
ffsou = fsou1+fsou2+fsou3
ffmdo = fmdo1+fmdo2+fmdo3+fmdo4+fmdo5+fmdo6+fmdo7
fct   = ffmat + ffsou + ffmdo


	

if(impr2.ge.-1) then
   if(itype(nel).ne.5) then                                                                         
	    write(iu_11(iboat),*) 
  	    write(iu_11(iboat),*) ' avec sur-épaisseurs de corrosion'
	    write(iu_11(iboat),*) ' --------------------------------'
 	    write(iu_11(iboat),*) ' *** sur-épaisseur=',dcor,' m'
	    write(iu_11(iboat),*) ' - epaiss bordé      =',deltac
	    write(iu_11(iboat),*) ' - epaiss âme cadres =',dyac
	    write(iu_11(iboat),*) ' - epaiss sem cadres =',tyac
	    write(iu_11(iboat),*) ' - epaiss âme raid   =',dxrc
	    write(iu_11(iboat),*) ' - epaiss sem raid   =',txrc
   else                                                                                    
      if(isect(nel).eq.3) then                                                
    	   write(iu_11(iboat),*)													
	       write(iu_11(iboat),*) ' avec sur-épaisseurs de corrosion'			
	       write(iu_11(iboat),*) ' --------------------------------'			
	       write(iu_11(iboat),*) ' - epaiss âme épont. =',dyac						
	       write(iu_11(iboat),*) ' - epaiss sem épont. =',tyac						
	       write(iu_11(iboat),*) ' - epaiss âme raid   =',dxrc						
	       write(iu_11(iboat),*) ' - epaiss sem raid   =',txrc						
      else                                                                                  
	       write(iu_11(iboat),*)													
	       write(iu_11(iboat),*) ' avec sur-épaisseurs de corrosion'		
	       write(iu_11(iboat),*) ' --------------------------------'		
	       write(iu_11(iboat),*) ' - epaiss paroi mince =',epaisc				
	       write(iu_11(iboat),*) ' - epaiss âme raid   =',dxrc					
	       write(iu_11(iboat),*) ' - epaiss sem raid   =',txrc					
      endif
   endif
endif


write(iu_11(iboat),*)
write(iu_11(iboat),*) 'extra weight induced by corrosion (girder and secondary members not considered !!!)'
write(iu_11(iboat),*) '-------------------------------------------'
ppo=poid1-spoid1
write(iu_11(iboat),*) 'net   weight =',ppo   , 'in n'
write(iu_11(iboat),*) 'gross weight =', poid1, 'in n'
write(iu_11(iboat),*) 'extra weight =',spoid1, 'in n'
write(iu_11(iboat),*)

write(iu_11(iboat),*) 'cost of the panel (without girder) with the simplified cost model'
write(iu_11(iboat),'(70(1h-))') 
write(iu_11(iboat),*) 'total cost =     fmat   +     fsou    +    fmdo  in euro,$,...'
write(iu_11(iboat),'(4(2x,e11.4) )') fct,ffmat,ffsou,ffmdo
write(iu_11(iboat),*)

if(icout.lt.2) then	
   ! imp.diff. selon module coût simplifié ou pas 
   if(nel.eq.1) then
      write(iu_12(iboat),'(2a)')'panel#:   cost =    fmat   +    &
	                                                 fsou   +    &
	                                                 fmdo (euro,$ or..) - gross weight and corrosion weight(n)'
      write(iu_12(iboat),'(98(1h-))')
   endif
   write(iu_12(iboat),'(2x,i3,6(1x,e11.4) )') nel,fct,ffmat,ffsou,ffmdo,poid1,spoid1
endif



if(nel.eq.neto) then
   ppo     =  poids-spoids
   spoids1 = spoids		
   iunit   =  iu_11(iboat)

   20     continue
   write(iunit,*) 'total weight: complete structure (girders and'
   write(iunit,*)	 'secondary members excluded)'					
   write(iunit,*) '-------------------------------------------------'
   write(iunit,*) '   weight (net   thickness)       = ',ppo   ,' n'
   write(iunit,*) '   weight (corrosion thickness)   = ',spoids,' n'
   write(iunit,*) '   weight (gross thickness)       = ',poids ,' n'
   write(iunit,*)
   if(impr2.ge.0) then
      write(iunit,*) 'total cost: complete structure (girders excluded)'
      write(iunit,*) '-------------------------------------------------'
      write(iunit,*) '1* material from plating       = ',fmat11,' euro,$'
      write(iunit,*) '   material from long stiff.   = ',fmat22,' euro,$'
      write(iunit,*) '   material from trans. frame  = ',fmat33,' euro,$'
      !write(iunit,*) '   material (total)            = ',fmat  ,' euro,$'
      write(iunit,*) '   material (total)            = ',(fmat11+fmat22+fmat33)  ,' euro,$'
      write(iunit,*)
      write(iunit,*) '2* consomables :stiff          = ',fsou11,' euro,$'
      write(iunit,*) '   consomables :frame          = ',fsou22,' euro,$'
      write(iunit,*) '   consomables :plate          =    not considered'
      !write(iunit,*) '   consomables (total)         = ',fsou  ,' euro,$'
      write(iunit,*) '   consomables (total)         = ',(fsou11+fsou22)  ,' euro,$'
      write(iunit,*)
      write(iunit,*) '3* labor cost for'
      write(iunit,*) '   - stiff. on plate      (p4) = ',fmdo11,' euro,$'
      write(iunit,*) '   - stiff buiding        (p9) = ',fmdo22,' euro,$'
      write(iunit,*) '   - frames. on plate     (p5) = ',fmdo33,' euro,$'
      write(iunit,*) '   - frames. building     (p9) = ',fmdo44,' euro,$'
      write(iunit,*) '   - slot cutting         (p6) = ',fmdo55,' euro,$'
      write(iunit,*) '   - stiff.bracket,tap    (p7) = ',fmdo66,' euro,$'
      write(iunit,*) '   - plating building     (p10)= ',fmdo77,' euro,$'
      write(iunit,*) '   labor cost (total)          = ',(fmdo11+fmdo22+fmdo33+fmdo44+fmdo55+fmdo66+fmdo77)  ,' euro,$'
      write(iunit,*)
   endif
   !write(iunit,*) '*** total cost  (1+2+3)        = ',cout_  ,' euro,$'
   write(iunit,*) '*** total cost  (1+2+3)        = ',((fmat11+fmat22+fmat33)+ (fsou11+fsou22)+ (fmdo11+fmdo22+fmdo33+fmdo44+fmdo55+fmdo66+fmdo77)),' euro,$'
   write(iunit,*) '      !!! using the simplified lbr5 model !!! '
   write(iunit,*)' this result will differ using a detailled database'
   write(iunit,*)


!  CALCUL DU LIFE CYCLE COST
   if (itera.eq.0.and.poidsLBR5_init_vector(iboat).lt.0.001d00) then
!     call poidstotal(test,0,0)
      poidsLBR5_init = poids
	  poidsLBR5_init_vector(iboat) = poidsLBR5_init
   endif
   
   call LCC(poids,somme,codo,acof,rtm,edis)
   write(iunit,*) '* Parametres de cout relatifs au LIFE CYCLE COST *'
   write(iunit,*) '--------------------------------------------------'
   write(iunit,*) 'Cost of periodic maintenance: ', codo
   write(iunit,*) 'Cost of fuel oil for main engine(s): ',acof
   write(iunit,*) 'Operational earning or revenue: ',rtm
   write(iunit,*) 'Dismantling earning: ', edis
   write(iunit,*) 'LIFE CYCLE COST (sum) : ',somme
   write(iunit,*)

   if(icout.lt.2) then  
      !opt. poids ou coût selon module coût simplifié 
      if(iunit.eq.iu_11(iboat)) then
         iunit=iu_12(iboat)
         goto 20
      endif

   endif  
endif

return
end
