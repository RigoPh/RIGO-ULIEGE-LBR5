subroutine print_panel_data(nel)

use param_section

implicit double precision (a-h,o-z)


! ====================================================================================================
! impressions diverses
! ====================================================================================================

if(langue==1) then  
   ! francais
   write(iu_11(iboat),801) panneau(nel).delta,width

   if(panneau(nel).types.ne.'coque') then
      write(iu_11(iboat),803) panneau(nel).hight
   else
      write(iu_11(iboat),802) panneau(nel).phil,panneau(nel).q
   endif

   write(iu_11(iboat),8) panneau(nel).ang
   
   if(itype(nel).ne.5) then								
      write(iu_11(iboat),11) panneau(nel).epsa,panneau(nel).hya,panneau(nel).dya,panneau(nel).wya,panneau(nel).tya
      write(iu_11(iboat),13) panneau(nel).entr,panneau(nel).hxr,panneau(nel).dxr,panneau(nel).wxr,panneau(nel).txr,  &
	                         panneau(nel).epsr,mode(nel),xmode(nel)
   else												
	  write(iu_11(iboat),804) panneau(nel).section,panneau(nel).epsa					
	  if ((panneau(nel).section.eq.'cercle').or.(panneau(nel).section.eq.'CERCLE')) then					
	     write(iu_11(iboat),806) panneau(nel).hya,panneau(nel).epais				
	  elseif((panneau(nel).section.eq.'carre').or.(panneau(nel).section.eq.'CARRE'))then					
	     write(iu_11(iboat),807) panneau(nel).hya,panneau(nel).epais					
	  elseif((panneau(nel).section.eq.'doublet').or.(panneau(nel).section.eq.'DOUBLET'))then			
	     write(iu_11(iboat),808) panneau(nel).hya,panneau(nel).dya,panneau(nel).wya,panneau(nel).tya		
	  endif										
   endif											

   if((panneau(nel).epsa2.ge.(1.0d-5)).or.(panneau(nel).entr2.ge.(1.0d-5))) then
      write(iu_11(iboat),14) panneau(nel).epsa2,panneau(nel).ksa2,panneau(nel).hya2,panneau(nel).dya2,panneau(nel).wya2,panneau(nel).tya2,  &
	                         panneau(nel).entr2,panneau(nel).ksr2,panneau(nel).hxr2,panneau(nel).dxr2,panneau(nel).wxr2,panneau(nel).txr2
   endif

   write(iu_11(iboat),1)
   do is=1,nsol
      if(ivaria(nel).eq.1) then
         write(iu_11(iboat),7) is,xxi(is,nel),xxf(is,nel)
      else
         write(iu_11(iboat),5) is,xxi(is,nel),xxf(is,nel)
      endif
   enddo

   write(iu_11(iboat),16) panneau(nel).ploc
16 format(/' pression locale maximale = ',f8.3,' m. (charge locale,',   &
           'ponctuelle, pour dimensionner les lisses et le bordé)')

else   
   ! english
   write(iu_11(iboat),901) panneau(nel).delta,width
   if(panneau(nel).types.ne.'coque') then
      write(iu_11(iboat),903) panneau(nel).hight
   else
      write(iu_11(iboat),902) panneau(nel).phil,panneau(nel).q
   endif
   write(iu_11(iboat),'(2a,f7.3,a)')  ' angle at origin (y=0) between the horizontal right hand', &
		                              ' side line and the panel (its tangent at y=0) = ',ang,' deg.'
   if(itype(nel).ne.5) then									
      write(iu_11(iboat),904) panneau(nel).epsa,panneau(nel).hya,panneau(nel).dya,panneau(nel).wya,panneau(nel).tya
      write(iu_11(iboat),905) panneau(nel).entr,panneau(nel).hxr,panneau(nel).dxr,panneau(nel).wxr,panneau(nel).txr, &
	                          panneau(nel).epsr,mode(nel),xmode(nel)
   else												
      write(iu_11(iboat),805) panneau(nel).section,panneau(nel).epsa						
	  if((panneau(nel).section.eq.'cercle').or.(panneau(nel).section.eq.'CERCLE'))then						
	     write(iu_11(iboat),809) panneau(nel).hya,panneau(nel).epais						
	  elseif((panneau(nel).section.eq.'carre').or.(panneau(nel).section.eq.'CARRE'))then				
	     write(iu_11(iboat),810) panneau(nel).hya,panneau(nel).epais				
   	  elseif((panneau(nel).section.eq.'doublet').or.(panneau(nel).section.eq.'DOUBLET'))then				
	     write(iu_11(iboat),811) panneau(nel).hya,panneau(nel).dya,panneau(nel).wya,panneau(nel).tya						
	  endif											
   endif
   
   if(((panneau(nel).epsa2).ge.(1.0d-5)).or.((panneau(nel).entr2).ge.(1.0d-5))) then
      write(iu_11(iboat),906) panneau(nel).epsa2,panneau(nel).ksa2,panneau(nel).hya2,panneau(nel).dya2,panneau(nel).wya2,panneau(nel).tya2, &
	                          panneau(nel).entr2,panneau(nel).ksr2,panneau(nel).hxr2,panneau(nel).dxr2,panneau(nel).wxr2,panneau(nel).txr2
   endif

   write(iu_11(iboat),'(/a/1x,40(1h+))') ' lateral hydrostatique pressure on the panel'
   do is=1,nsol
      if(ivaria(nel).eq.1) then
         write(iu_11(iboat),907)nnsol(is),xxi(is,nel),xxf(is,nel)
      else
         write(iu_11(iboat),908)nnsol(is),xxi(is,nel),xxf(is,nel)
      endif
   enddo

   write(iu_11(iboat),'(/a,f8.3,a,a/)')                        &
          'maximum local pressure =',panneau(nel).ploc,		   &
          ' m. (local load, concentrated,',                    &
          ' for sizing of stiffeners and plate thickness).' 

endif

return

! -------------------------------------------------------------------------------
! -------------------------------------------------------------------------------
900 write(* ,*)'erreur de lecture : "end of file"'
    write(iu_11(iboat),*)'erreur de lecture : "end of file"'
    write(iu_14(iboat),*)'erreur de lecture : "end of file"'
    write(*,*) 'stop'
    read(*,*)
    stop

! 6.  les formats.
!     ============
   1  format(/' charge en metre de la colonne d''eau sur le panneau'/1x,49(1h+))
   5  format(' cas de charge nø ',i2/                                    &
      '  variation lineaire de la p.h.avec la profondeur (hydrostat)'/   &
      '  sur le bord de depart (y=0), xi=',f7.3,' m.'/                   &
      '  (nb: sur le bord d''arrivee,  xf=',f7.3,' m )'/)
   7  format(' cas nø',i2,' variation lineaire de la p.h. (xi a xf) :'/  &
      '    au bord de depart  (y=0)   , xi=',f11.3,' m.'/                 &
      '    au bord d''arrivee (y=phil) , xf=',f11.3,' m.'/                &
      '     nb: 1m = 1 m d''eau (colonne) = 9.81kn/m2')
   8  format('    angle que fait le panneau en y=0 avec l''horizontale',' = ',f9.3,' degré')
  11  format(/' dimensions des cadres (transversaux)'/1x,36(1h+)/  &
     '    distance entre cadres    = ',f8.5,' m.'//                &
     '    hauteur de l''ame         = ',f8.5,' m.',                &
          ' (épaisseurs du bordé et semelle non comprises)'/       &
     '    epaisseur de l''ame       = ',f8.5,' m.'/                &
     '    largeur de la semelle    = ',f8.5,' m.'/                 &
     '    epaisseur de la semelle  = ',f8.5,' m.')
  13  format(/' dimensions des raidisseurs longitudinaux'/1x,40(1h+)/ &
     '    distance entre raid. (=d) = ',f8.5,' m.'//                  &
     '    hauteur de l''ame          = ',f8.5,' m.',                  &
          ' (épaisseurs du bordé et semelle non comprises)'/          &
     '    epaisseur de l''ame        = ',f8.5,' m.'/                  &
     '    largeur de la semelle     = ',f8.5,' m.'/                   &
     '    epaisseur de la semelle   = ',f8.5,' m.'//                  &
     '    epsr (larg. collaborante) = ',f8.5,' m.'/                   &
     '    mode (répartition des cadres): ',a3,' n=l/d + ',f4.1)
  14  format(/'nervures complémentaires (2ème lit; aig et raid)'/45(1h=)  &
     /' les cadres'/2x,12(1h-)/                                           &
      '   distance entre cadres compl.= ',f8.5,' m.  et  ksa2= ',i2/      &
      '   hauteur de l''ame        ',f8.5,' m.'/                          &
      '   epaisseur de l''ame      ',f8.5,' m.'/                          &
      '   largeur de la semelle   ',f8.5,' m.'/                           &
      '   epaisseur de la semelle ',f8.5,' m.'/                           &
      ' les raidisseurs'/2x,12(1h-)/                                      &
      '   distance entre raidisseurs compl.= ',f8.5,                      &
                                            ' m. et  ksr2= ',i2/          &
      '   hauteur de l''ame        ',f8.5,' m.'/                          &
      '   epaisseur de l''ame      ',f8.5,' m.'/                          &
      '   largeur de la semelle   ',f8.5,' m.'/                           &
      '   epaisseur de la semelle ',f8.5,' m.'/)
 801  format(/' dimensions du panneau'/1x,22(1h+)/                &
              '    epaisseur de bordage = ',f9.6,' m.'/           &
              '    longueur du panneau  = ',f9.3,' m.')
 802  format( '    angle d''ouverture    = ',f11.5,' degré'/      &
              '    rayon du panneau     = ',f10.3,' m.')
 803  format( '    largeur de la plaque = ',f9.5,' m.')
 804  format( '	la section est de type	= ',a7/		                &			
      		    '	l''entredistance des epontilles =',f9.5,'m.')	
 805  format(	'	section''s type =',a7/                            &
              '	pillar''s spacing =',f9.5,'m.')				
 806  format(	'	diametre exterieur	=',f9.5,' m.'/                &
              '	epaisseur de la paroi mince =',f9.5,'m.')		
 807  format(	'	longueur exterieure du cote	=',f9.5,' m.'/	      &
          		'	epaisseur de la paroi mince =',f9.5,'m.')		
 808  format(	'	hauteur de la demi ame	=',f9.5,' m.'/	          &
          		'	epaisseur de ame =',f9.5,' m.'/					          &
          		'	largeur de la semelle	=',f9.5,' m.'/		          &
          		'	epaisseur de la semelle	=',f9.5,' m.')	
 809  format(	'	exterior diameter 	=',f9.5,' m.'/	              &
          		'	cercle thickness =',f9.5,'m.')					
 810  format(	'	exterior length of side	=',f9.5,' m.'/	          &		
          		'	square thickness =',f9.5,'m.')					
 811  format(	'	half web hight	=',f9.5,' m.'/			              &
          		'	web thickness =',f9.5,' m.'/					            &
          		'	flange width =',f9.5,' m.'/						            &
          		'	flange thickness =',f9.5,' m.')					
 901  format(/' dimensions of the panel'/1x,22(1h+)/        &
              '    plate thickness     = ',f9.6,' m.'/      &
              '    length of the panel = ',f9.3,' m (=structure length')
 902  format( '    shell opening angle = ',f11.5,' degré'/  &
              '    shell radius        = ',f10.3,' m.')
 903  format( '    width of the panel  = ',f9.5,' m.')
 904  format(/' dimensions of the frames'/1x,30(1h+)/       &
     '    frame spacing             = ',f8.5,' m.'//        &
     '    web height                = ',f8.5,' m. ',        &
            '(flange and plate thickness not included)'/    &
     '    web thickness             = ',f8.5,' m.'/         &
     '    flange width              = ',f8.5,' m.'/         &
     '    flange thickness          = ',f8.5,' m.')
 905  format(/' dimensions of longitudinal stiffeners'/1x,40(1h+)/  &
     '    geometric spacing (=d)    = ',f8.5,' m. (as given)'//     &
     '    web height                = ',f8.5,' m. ',                &
            '(flange and plate thickness not included)'/            &
     '    web thickness             = ',f8.5,' m.'/                 &
     '    flange width              = ',f8.5,' m.'/                 &
     '    flange thickness          = ',f8.5,' m.'/                 &
     '    effective spacing (epsr)  = ',f8.5,' m. (calculated)'/    &
     '    mode (frames distribution): ',a3,' n=l/d + ',f4.1)
 906  format(/'complementary members (2nd layer; frame & stiff)'/45(1h=)  &
     /' frames'/2x,12(1h-)/                                               &
      '   spacing                 ',f8.5,' m.  and  ksa2= ',i2/           &
      '   web height              ',f8.5,' m.'/                           &
      '   web thickness           ',f8.5,' m.'/                           &
      '   flange width            ',f8.5,' m.'/                           &
      '   flange thickness        ',f8.5,' m.'/                           &
      ' stiffeners'/2x,12(1h-)/                                           &
      '   spacing                 ',f8.5,' m.  and  ksr2= ',i2/           &
      '   web height              ',f8.5,' m.'/                           &
      '   web thickness           ',f8.5,' m.'/                           &
      '   flange width            ',f8.5,' m.'/                           &
      '   flange thickness        ',f8.5,' m.'/)
 907  format(' load case nø',i2,' linear variation of pressure between'/  &
      '    departure edge (y=0)   , xi=',f7.3,' m.'/                      &
      '    arrival edge   (y=phil), xf=',f7.3,' m.'/                      &
      '     nb: 1 m means a pressure of 9.81kn/m2 = 1 meter of water')
 908  format(' load case nø',i2,' acting (phi=0):',f7.3,' m.'/  &
      ' variation lineaire de la p.h.avec la profondeur'/       &
      '  (nb: sur le bord inf., xf=',f7.3,' m )'/)

end
