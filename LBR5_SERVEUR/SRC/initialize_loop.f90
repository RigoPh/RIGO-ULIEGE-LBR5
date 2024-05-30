subroutine initialize_loop()



use param_section


implicit double precision (a-h,o-z)


!==============================================================================
! 8.0 depart d'une nouvelle iteration 
!==============================================================================
																		
if(itera.gt.0)then													
   write(iu_14(iboat),*)'========================================================'				
   write(iu_14(iboat),*)'iteration ',itera							
   write(iu_14(iboat),*)'========================================================'			
   write(iu_14(iboat),*)' '							
endif																

if(langue==1) write(iu_12(iboat),10) neto								
if(langue==2) write(iu_12(iboat), 6) neto								


if (itera.gt.1) then
	if(impr2.eq.1) then		

		if(itera.lt.iter1)then 
			write(iu_11(iboat),418)itera      
			write(iu_12(iboat),418)itera      
		elseif(itera.eq.iter1) then				
			write(iu_11(iboat),418)iteram     
			write(iu_12(iboat),418)iteram     
		endif            
		!analyse structurelle finale  

	else

		close(iu_11(iboat))
		close(iu_12(iboat))
		!sortie listing (sortie standard)    
		open(iu_11(iboat),file=iunit1)                              

		! sortie listing (recapitulatif des resultats)          
		open(iu_12(iboat),file=iunit3)                                

		!write(iu_11(iboat),3) buffer     
		!write(iu_12(iboat),3) buffer
		write(iu_12(iboat),'(a)') '1 **********************************************************************'
		write(iu_12(iboat),'(a,a64,a)') '  *    ',header2,'*'
		write(iu_12(iboat),'(a)') '  *    ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++    *'
		write(iu_12(iboat),'(a)') '  *                                                                    *'
		write(iu_12(iboat),'(a)') '  **********************************************************************'

		write(iu_11(iboat),4)header1	
		write(iu_11(iboat),*)				
		write(iu_11(iboat),'(a)') '1 **********************************************************************'
		write(iu_11(iboat),'(a,a64,a)') '  *    ',header2,'*'
		write(iu_11(iboat),'(a)') '  *    ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++    *'
		write(iu_11(iboat),'(a)') '  *                                                                    *'
		write(iu_11(iboat),'(a)') '  **********************************************************************'



		if(itera.eq.iter1) then
			write(iu_11(iboat),418) iteram		
			write(iu_12(iboat),418) iteram
		else															
			write(iu_11(iboat),418) itera
			write(iu_12(iboat),418) itera
		endif			

		!analyse structurelle finale
		write(iu_11(iboat),10) neto
		write(iu_11(iboat),'(t26,a)')'avec optimisation'
		write(iu_11(iboat),'(t26,a,i3,a)')'(iter. max=',iteram,')'
		write(iu_11(iboat),400)
		write(iu_11(iboat),11) jlph2,ibusc

		if(impr.ne.0)   write(iu_11(iboat),7)
		if(indaig.eq.0) write(iu_11(iboat),9)
		if(impr2.eq.1)  write(iu_11(iboat),8)

		write(iu_11(iboat),12)(dis(i),i=1,5)
		write(iu_11(iboat),*)

		if(ipoids.eq.1) then
		    write(iu_14(iboat),*)' on tient compte du poids propre des panneaux (ipoids =1)'
		else
		    write(iu_11(iboat),*)' on ne tient pas compte du poids propre des panneaux (ipoids =0)'
			write(iu_14(iboat),*)' on ne tient pas compte du poids propre des panneaux (ipoids =0)'
		endif

		write(iu_11(iboat),'(1x,51(1h-)/)')
		write(iu_11(iboat),419) nsolm, nsol,(nnsol(i),i=1,nsol)

	endif

endif

if(icout.lt.2) then						
   write(iu_11(iboat),*)'le cout est calculé par un modèle simplifié '
   write(iu_12(iboat),*)'le cout est calculé par un modèle simplifié '
   write(iu_14(iboat),*)'le cout est calculé par un modèle simplifié '
   if(iopti.ge.1) then 
      write(iu_31(iboat),*)'le cout est calculé par un modèle simplifié '	
   endif
else															
   write(iu_11(iboat),*)'le cout est calculé par un modèle détaillé cost cat'	
   write(iu_12(iboat),*)'le cout est calculé par un modèle détaillé cost cat'	
   write(iu_14(iboat),*)'le cout est calculé par un modèle détaillé cost cat'	
   if(iopti.ge.1) then
      write(iu_31(iboat),*)'le cout est calculé par modèle détaillé cost cat'	
   endif
endif															


!==============================================================================
! 8.2 impression des parametres de couts
!==============================================================================

if(iopti.ge.1)then
   if(icout.eq.(-1)) then					
      write(iu_12(iboat),428)			
      write(iu_31(iboat),428)			
   endif															
   
   if(icout.eq.0) then
      write(iu_12(iboat),421)
      write(iu_31(iboat),421)
   endif

   if(icout.ge.1) then				
      write(iu_12(iboat),422)
      write(iu_31(iboat),422)
   endif
endif
    

if(impr2.ge.0) then							

   write(iu_12(iboat),431) rend_global !,eqp_global

   do ind=1,nbrMat											
      write(iu_12(iboat),430) ind											
      write(iu_12(iboat),423) dref(ind),drefx(ind),drefy(ind),c1(ind),c2(ind),c3(ind),dc1(ind),dw2(ind),dw3(ind)										
      write(iu_12(iboat),424) p10(ind),dp10(ind),p4(ind),dp4(ind),p5(ind),dp5(ind)											
      write(iu_12(iboat),425) p9x(ind),p9y(ind),dp9x(ind),dp9y(ind)		
      write(iu_12(iboat),426) p6(ind),p7(ind),ber(ind),bet(ind)				
      write(iu_12(iboat),427) c8(ind),dc8(ind),ialr(ind),ialt(ind)		
   enddo

endif

!==============================================================================
! 8.3 initialisation de ifonct
!==============================================================================
ifonct(1)=1      
ifonct(2)=0      
                 
! ifonct(terme impair)=1 pour termes 1,3,7,..     
! ifonct(terme pair  )=0 pour termes 2,4,6,...    

! si cas de charges symétriques selon x           
! ifonct(terme impair)=1 pour termes 1,3,7,..    
! ifonct(terme pair  )=0 pour termes 2,4,6,...                

! si cas de charges non symétriques selon x                        
! ifonct =1 pour tous les termes.                                 
                 


!==============================================================================
! 9.0 lecture des donnees relatives a chaque panneau et calcul des 
!     grandeurs caracteristiques   (subr. ent) 
!==============================================================================

! iff=1 si error dans ent  
iff  =0        

! icha  indice de charges variables selon ox
!ichag=0        
pot  =0.d00
ntot (iboat) =0
mtot (iboat) =0
m1tot(iboat) =0
m2tot(iboat) =0
m3tot(iboat) =0
m4tot(iboat) =0
m5tot(iboat) =0

spoids1       =0.d00
!cha(:,:,:)    =0.d00
!dcha(:,:,:)   =0.d00

if(langue.eq.1) write(*,100)'lecture des donnees (sr ent)'
if(langue.eq.2) write(*,100)'reading data (sr ent)'

!   3 format(1h1,1x,70(1h*)/,           &                                                                       
!            2x,1h*,4x,15a4,4x,1h*/,    &                                                                       
!            2x,1h*,4x,60(1h+),4x,1h*/, &                                                                       
!            2x,1h*,68x,1h*/,           &                                                                       
!            2x,70(1h*)//)                                                                                      
   4 format(a80)                                                                                              

   6 format(///                                                   &                                            
      16x,'analysis of a structure composed of ',i3,' panel(s)'/  &                                            
      15x,49(1h+)/)                                                                                            

   7 format(/' avec resultats intermediaires')                                                                 
   8 format(/' avec resultats finaux + verifications')                                                         
   9 format(/' pas de resultats dans les transversaux (cadres).')                                              
  10 format(///                                                   &                                            
      ,16x,'analyse d''une structure a ',i3,' panneau(x)'/15x,45(1h+)/)                                        
  11 format(///' unites en metres et newtons',//,' nombre de termes', &                                        
      ' de la serie de fourier pour les charges ',i3//                &                                        
      ' efforts de bord nb et mb (buscage) --> file: busc.txt'/       &                                        
      '  ibusc =',i2,'  ( 0:sans nb et mb  et 1 : avec nb et mb)'/)                                            
  12 format(/' coordonnees (x) des 5 sections ou les resultats sont', &                                        
             ' fournis (en metre)'/t10,5(2x,f9.4))                                                             
 100 format(1x,a,i8)                                                                                          
 400 format(21x,20(1h*))                                                                                       

 418 format(//30x,'iteration nø',i2/30x,20(1h=)/)                                                              
 419 format( ' cas de charge considérés:'/1x,25(1h=)/                   &                                      
             ' - nbre de cas de charge disponibles = ',i2/              &                                      
             ' - nbre de cas de charge analysés    = ',i2/              &                                      
             ' - numeros des cas analysés = ',10i3//)                                                          
 420 format( ' considered load cases:'/1x,22(1h=)/                      &                                      
             ' - available  load cases = ',i2/                          &                                      
             ' - considered load cases = ',i2/                          &                                      
             ' - nbr of the considered cases = ',10i3//)                                                       
 421 format(/' la fonction objectif est le poids'/1x,35(1h-))                                                  
 422 format(/' la fonction objectif est le cout' /1x,35(1h-))                                                  
 423 format(3x,'rendement global du chantier         =',f8.3/            &                                     
            3x,'equivalent poids de la main d''oeuvre =',f8.3/           &                                     
            3x,'epaisseur de référence borde (dref)  =',f8.3,' m'/       &                                     
            3x,'epaisseur de référence long. (drefx) =',f8.3,' m'/       &                                     
            3x,'epaisseur de référence cadre (drefy) =',f8.3,' m'/       &                                     
            3x,'coût du matériau au kg, borde (c1)   =',f8.3,' euro/kg'/ &                                     
            3x,'coût du matériau au kg, long. (c2)   =',f8.3,' euro/kg'/ &                                     
            3x,'coût du matériau au kg, cadre (c3)   =',f8.3,' euro/kg'/ &                                     
            3x,'variation de c1 par mm de tôle (dc1) =',f8.3/            &                                     
            3x,'poids compl.pour membr.longitud.(dw2)=',f8.3/            &                                     
            3x,'poids compl.pour membr. transv. (dw3)=',f8.3/)                                                 
 424 format(3x,'mdo construction du bordé (p10)      =',f8.3,' h-h/m2'/  &                                     
            3x,'variat. de p10 par mm de bordé (dp10)=',f8.3/            &                                     
            3x,'mdo soudage long. sur bordé (p4)     =',f8.3,' h-h/m'/   &                                     
            3x,'variat. de p4 par mm d''âme (dp4)     =',f8.3/           &                                     
            3x,'mdo soudage transv. sur bordé (p5)   =',f8.3,' h-h/m'/   &                                     
            3x,'variat. de p5 par mm d''âme (dp5)     =',f8.3/)                                                
 425 format(3x,'mdo construct. membrures long..(p9x) =',f8.3,' h-h/m'/   &                                     
            3x,'mdo construct. membrures cadre (p9y) =',f8.3,' h-h/m'/   &                                     
            3x,'variat. de p9x par mm d''âme (dp9x)   =',f8.3/           &                                     
            3x,'variat. de p9y par mm d''âme (dp9y)   =',f8.3/)                                                
 426 format(3x,'mdo pour intersect. long/trans (p6)  =',f8.3,            &                                     
                                                      ' h-h/unité'/      &                                     
            3x,'mdo pour gousset pour long/trans (p7)=',f8.3,            &                                     
                                                      ' h-h/unité'/      &                                     
            3x,'fréq. relat.gousset sur long. (bétar)=',f8.3/            &                                     
            3x,'fréq. relat.gousset sur trans.(bétat)=',f8.3/)                                                 
 427 format(3x,'coût energie+consommable(soudure)(c8)=',f8.3,' euro/m'/  &                                     
            3x,'var. de c8 par mm de la tôle soudée  =',f8.3/            &                                     
            3x,'longitudinaux reconstitués (alpha r) =',i4,' oui=0'/     &                                     
            3x,'transversaux  reconstitués (alpha t) =',i4,' non=1'//)                                         
 428 format(/' la fonction objectif est inertie' /1x,32(1h-))
 430 format('Caracteristiques du MATERIAU ',i4)
 431 format('Rendement global du chantier =',f8.3) !,' Weight equivalent parameter [ton/man hour]',f8.3)	                                                 
                                                                                                               

return
end
