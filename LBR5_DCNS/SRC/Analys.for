c ********************************************************************************                                                        
c     subroutine de relecture (file 304) des valeurs extrêmes des déplacements		!extension neto	!fev2007
c     et des contraintes sauvées dans les subr ecri et ecri3 (subr extrem)	

c     modifié : 4-2-2004                           créer: 13-3-95 (ph. rigo)
c
c      20-11-2000  impression des resultats en write(67,..):fichier résultat sol2
c       4-12-2002  19(22) au lieu de 12 variables fournies.
c	 04-02-2004  ajout variables pour épontilles.

c *********************************************************************************
      subroutine analys(indaig,indrai)!,impr2)   ! février 2004               	
      use sharedvar
      implicit real *8 (a-h,o-z) 
      dimension  v(neto,2), u(neto,2), w(neto,2),                             
     *          iv(neto,4),iu(neto,4),iw(neto,4),                             
     *       sborx(neto,2), sbory(neto,2),   tau(neto,2),
     *      isborx(neto,4),isbory(neto,4),  itau(neto,4),					  
     *        sbor(neto,2), sbor1(neto,2), sbor2(neto,2),
     *       isbor(neto,4),isbor1(neto,4),isbor2(neto,4)
      dimension					  
     *  sjas(neto,2), sjab(neto,2), tjas(neto,2), tjab(neto,2), 
     * isjas(neto,4),isjab(neto,4),itjas(neto,4),itjab(neto,4), 
     *  ajas(neto,2), ajab(neto,2), asem(neto,2),
     * iajas(neto,4),iajab(neto,4),iasem(neto,4),                     			
     *  rjas(neto,2), rjab(neto,2), rsem(neto,2),                   		
     * irjas(neto,4),irjab(neto,4),irsem(neto,4)
      dimension                               ! caract pour épontille		!février 2004
     *       sborxi(neto,2), sboryi(neto,2),						
     *      isborxi(neto,4),isboryi(neto,4),						
     *  sjasi(neto,2), sjabi(neto,2), tjasi(neto,2), tjabi(neto,2), 
     * isjasi(neto,4),isjabi(neto,4),itjasi(neto,4),itjabi(neto,4), 
     *  ajasi(neto,2), ajabi(neto,2), asemi(neto,2),		
     * iajasi(neto,4),iajabi(neto,4),iasemi(neto,4)                     			
      dimension i4(nmax),i5(nmax),i6(nmax)									!février 2004
                          		

c     z(i,1) = (vmin) valeur minimale de z en (x,y)=(ixmin,iymin) pour le panneau i     
c     z(i,2) = (vmax) valeur maximale de z en (x,y)=(ixmax,iymax) pour le panneau i 
c     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
c     vmin	ixmin	iymin		vmax	ixmax	iymax
c     z(i,1)	iz(i,1)	iz(i,2)		z(i,2)	iz(i,3)	iz(i,4)
c ------------------------------------------------------------------------------------

c	call annuld(u,2*neto)
c	call annuld(v,2*neto)
c	call annuld(w,2*neto)


	nelo= 0    ! = nombre total d'épontilles dans la structure		!février 2004
	neta = 0   ! = nombre total de plaques dans la structure		!février 2004
	i1 = 0															!février 2004
	i2 = 0															!février 2004
	i3 = 0															!février 2004
	do 2 i=1,neto
	if(itype(i).ne.5) then                                          !février 2004        
	 
	 i1 = i1 + 1													!février 2004

  20   read(304,*,end=1) ivar   !saved in ecri	!extension neto	!fev2007
       if(ivar.eq.99) goto 30					
      
      if(ivar.eq.1) then											!1  v 
      read(304,*)v(i,1),iv(i,1),iv(i,2),v(i,2),iv(i,3),iv(i,4)	!extension neto	!fev2007
      else if (ivar.eq.2)  then									!2  u
      read(304,*)u(i,1),iu(i,1),iu(i,2),u(i,2),iu(i,3),iu(i,4)	!extension neto	!fev2007
      else if (ivar.eq.3)  then									!3  w
      read(304,*)w(i,1),iw(i,1),iw(i,2),w(i,2),iw(i,3),iw(i,4)	!extension neto	!fev2007
      else if (ivar.eq.18)  then						!18   sx bordage
      read(304,*)sborx(i1,1),isborx(i1,1),isborx(i1,2),			!extension neto	!fev2007
     *           sborx(i1,2),isborx(i1,3),isborx(i1,4)
      else if (ivar.eq.17)  then						!17   sy bordage
      read(304,*)sbory(i1,1),isbory(i1,1),isbory(i1,2),			!extension neto	!fev2007
     *           sbory(i1,2),isbory(i1,3),isbory(i1,4)
      else if (ivar.eq.19)  then						!19   tau bordage
      read(304,*)tau(i1,1),itau(i1,1),itau(i1,2),					!extension neto	!fev2007
     *           tau(i1,2),itau(i1,3),itau(i1,4)
      else if (ivar.eq.20)  then					    !20 sigma comp bordage
      read(304,*)sbor(i1,1),isbor(i1,1),isbor(i1,2),				!extension neto	!fev2007
     *           sbor(i1,2),isbor(i1,3),isbor(i1,4)

      else if (ivar.eq.21) then						!21 sy  jas (cadre)
      read(304,*)sjas(i1,1),isjas(i1,1),isjas(i1,2),				!extension neto	!fev2007
     *           sjas(i1,2),isjas(i1,3),isjas(i1,4)
      else if (ivar.eq.22) then						!22 sy  jab (cadre)
      read(304,*)sjab(i1,1),isjab(i1,1),isjab(i1,2),				!extension neto	!fev2007
     *           sjab(i1,2),isjab(i1,3),isjab(i1,4)
      else if (ivar.eq.23) then						!23 tau  jas (cadre)
      read(304,*)tjas(i1,1),itjas(i1,1),itjas(i1,2),				!extension neto	!fev2007
     *           tjas(i1,2),itjas(i1,3),itjas(i1,4)
      else if (ivar.eq.24) then						!24 tau  jab (cadre)
      read(304,*)tjab(i1,1),itjab(i1,1),itjab(i1,2),				!extension neto	!fev2007
     *           tjab(i1,2),itjab(i1,3),itjab(i1,4)

      else if (ivar.eq.25) then						!25 sig comp jas (cadre)
      read(304,*)ajas(i1,1),iajas(i1,1),iajas(i1,2),				!extension neto	!fev2007
     *           ajas(i1,2),iajas(i1,3),iajas(i1,4)
      else if (ivar.eq.26) then						!26 sig comp jab (cadre)
      read(304,*)ajab(i1,1),iajab(i1,1),iajab(i1,2),				!extension neto	!fev2007
     *           ajab(i1,2),iajab(i1,3),iajab(i1,4)
      else if (ivar.eq.29) then						!29 sig comp sem (cadre)
      read(304,*)asem(i1,1),iasem(i1,1),iasem(i1,2),				!extension neto	!fev2007
     *           asem(i1,2),iasem(i1,3),iasem(i1,4)
      
      else if (ivar.eq.36) then						!36 sig comp jas (raid)
      read(304,*)rjas(i1,1),irjas(i1,1),irjas(i1,2),				!extension neto	!fev2007
     *           rjas(i1,2),irjas(i1,3),irjas(i1,4)
      else if (ivar.eq.37) then						!37 sig comp jab (raid)
      read(304,*)rjab(i1,1),irjab(i1,1),irjab(i1,2),				!extension neto	!fev2007
     *           rjab(i1,2),irjab(i1,3),irjab(i1,4)
      else if (ivar.eq.38) then						!38 sig comp sem (raid)
      read(304,*)rsem(i1,1),irsem(i1,1),irsem(i1,2),				!extension neto	!fev2007
     *           rsem(i1,2),irsem(i1,3),irsem(i1,4)
      else
        write(67,*)' error1: subr. analys; ivar=',ivar,' est incorrecte'
        write(*,*) ' error1: subr. analys; ivar=',ivar,' est incorrecte'
        write(6970,*) ' error1: subr. analys; ivar=',ivar,
     *' est incorrecte'
        write(29,*)'subr analysis'													!sept06!bug
	  write(29,*)' error1: subr. analys; ivar=',ivar,' est incorrecte'				!sept06!bug
	  
	  pause 'stop'
        stop
      end if
      goto 20
	elseif((itype(i).eq.5).and.(isect(i).eq.3)) then		!épontille double t		!février 2004
	i2 = i2 + 1	
  	nelo = nelo + 1		
  21    read(2218,*,end=1) ivar   !saved in ecri4				
       if(ivar.eq.99) goto 1								
      if(ivar.eq.1) then											!1  v	
      read(2218,*)v(i,1),iv(i,1),iv(i,2),v(i,2),iv(i,3),iv(i,4)		
      else if (ivar.eq.2)  then									!2  u	
      read(2218,*)u(i,1),iu(i,1),iu(i,2),u(i,2),iu(i,3),iu(i,4)		
      else if (ivar.eq.3)  then									!3  w	
      read(2218,*)w(i,1),iw(i,1),iw(i,2),w(i,2),iw(i,3),iw(i,4)		
	
	else if (ivar.eq.17) then						!17 sy  jas sup(cadre)	
      read(2218,*)sjasi(i2,1),isjasi(i2,1),isjasi(i2,2),							
     *           sjasi(i2,2),isjasi(i2,3),isjasi(i2,4)							
      else if (ivar.eq.18) then						!18 sy  jas inf (cadre)	
      read(2218,*)sjabi(i2,1),isjabi(i2,1),isjabi(i2,2),							
     *           sjabi(i2,2),isjabi(i2,3),isjabi(i2,4)							
      else if (ivar.eq.19) then						!19 tau  jas (cadre)	
      read(2218,*)tjasi(i2,1),itjasi(i2,1),itjasi(i2,2),							
     *           tjasi(i2,2),itjasi(i2,3),itjasi(i2,4)							
      else if (ivar.eq.20) then						!20 tau  an (cadre)		
      read(2218,*)tjabi(i2,1),itjabi(i2,1),itjabi(i2,2),							
     *           tjabi(i2,2),itjabi(i2,3),itjabi(i2,4)							
	
      else if (ivar.eq.21) then						!21 sig comp jas sup (cadre) 
      read(2218,*)ajasi(i2,1),iajasi(i2,1),iajasi(i2,2),							
     *           ajasi(i2,2),iajasi(i2,3),iajasi(i2,4)							
      else if (ivar.eq.22) then						!22 sig comp jas inf (cadre)  
      read(2218,*)ajabi(i2,1),iajabi(i2,1),iajabi(i2,2),							
     *           ajabi(i2,2),iajabi(i2,3),iajabi(i2,4)							
      else if (ivar.eq.23) then						!23 sig comp an (cadre)	
      read(2218,*)sborxi(i2,1),isborxi(i2,1),isborxi(i2,2),						
     *           sborxi(i2,2),isborxi(i2,3),isborxi(i2,4)						
	else if (ivar.eq.27) then						!27 sig comp sem sup (cadre) 
      read(2218,*)asemi(i2,1),iasemi(i2,1),iasemi(i2,2),							
     *           asemi(i2,2),iasemi(i2,3),iasemi(i2,4)							
      else if (ivar.eq.28)  then						!28 sig comp sem inf (cadre) 
      read(2218,*)sboryi(i2,1),isboryi(i2,1),isboryi(i2,2),						
     *           sboryi(i2,2),isboryi(i2,3),isboryi(i2,4)						
      else																	
        write(67,*)'error2: subr. analys; ivar=',ivar,' est incorrecte'	
        write(*,*) 'error2: subr. analys; ivar=',ivar,' est incorrecte'	
        write(6970,*) 'error2: subr. analys; ivar=',ivar,
     *' est incorrecte'	
        write(29,*)'subr analysis'													!sept06!bug
	  write(29,*)'error2: subr. analys; ivar=',ivar,' est incorrecte'				!sept06!bug
	  
	  pause 'stop'															
        stop															
	endif
	goto 21											
       elseif((itype(i).eq.5).and.(isect(i).ne.3)) then		!épontille parois minces
	 nelo= nelo+1
	 i3 = i3 + 1		
 22   read(2218,*,end=1) ivar   !saved in ecri4				
       if(ivar.eq.99) goto 1					

      if(ivar.eq.1) then											!1  v 
      read(2218,*)v(i,1),iv(i,1),iv(i,2),v(i,2),iv(i,3),iv(i,4)   
      else if (ivar.eq.2)  then									!2  u 
      read(2218,*)u(i,1),iu(i,1),iu(i,2),u(i,2),iu(i,3),iu(i,4)    
      else if (ivar.eq.3)  then									!3  w  
      read(2218,*)w(i,1),iw(i,1),iw(i,2),w(i,2),iw(i,3),iw(i,4)    

	else if (ivar.eq.17) then						!17 sy  jas sup(cadre)  
      read(2218,*)sjasi(i3,1),isjasi(i3,1),isjasi(i3,2),                   
     *           sjasi(i3,2),isjasi(i3,3),isjasi(i3,4)                     
      else if (ivar.eq.18) then						!18 sy  jas inf (cadre)  
      read(2218,*)sjabi(i3,1),isjabi(i3,1),isjabi(i3,2),                  
     *           sjabi(i3,2),isjabi(i3,3),isjabi(i3,4)                      
      else if (ivar.eq.19) then						!19 tau  jas (cadre)  
      read(2218,*)tjasi(i3,1),itjasi(i3,1),itjasi(i3,2),                    
     *           tjasi(i3,2),itjasi(i3,3),itjasi(i3,4)                      
      else if (ivar.eq.20) then						!20 tau  an (cadre) 
      read(2218,*)tjabi(i3,1),itjabi(i3,1),itjabi(i3,2),                   
     *           tjabi(i3,2),itjabi(i3,3),itjabi(i3,4)                      

      else if (ivar.eq.21) then						!21 sig comp jas sup (cadre)  
      read(2218,*)ajasi(i3,1),iajasi(i3,1),iajasi(i3,2),                      
     *           ajasi(i3,2),iajasi(i3,3),iajasi(i3,4)                      
      else if (ivar.eq.22) then						!22 sig comp jas inf (cadre)  
      read(2218,*)ajabi(i3,1),iajabi(i3,1),iajabi(i3,2),                  
     *           ajabi(i3,2),iajabi(i3,3),iajabi(i3,4)                       
      else if (ivar.eq.23) then						!23 sig comp an (cadre)  
      read(2218,*)sborxi(i3,1),isborxi(i3,1),isborxi(i3,2),                
     *           sborxi(i3,2),isborxi(i3,3),isborxi(i3,4)                   
      else	                                                            
        write(67,*)'error3: subr. analys; ivar=',ivar,' est incorrecte'	
        write(*,*) 'error3: subr. analys; ivar=',ivar,' est incorrecte'	
        write(6970,*) 'error3: subr. analys; ivar=',ivar,
     *' est incorrecte'	
        write(29,*)'subr analysis'													!bug
	  write(29,*)'error3: subr. analys; ivar=',ivar,' est incorrecte'				!bug
		  
	  pause 'stop'		                                              
        stop	                                                           
	endif                                                              
	goto 22                                                               
	endif													!février 2004

  30  if(impr2.le.-2) goto 1		!sept06
  31  read(304,*,end=1) ivar !saved in ecri3	!extension neto	!fev2007
      if(ivar.eq.88) goto 1						      
      if(ivar.eq.4) then						        ! 4 sig.comp z=+d/2 
        read(304,*)sbor1(i1,1),isbor1(i1,1),isbor1(i1,2),	!extension neto	!fev2007
     *             sbor1(i1,2),isbor1(i1,3),isbor1(i1,4)
      else if (ivar.eq.8) then						! 8 sig.comp z=-d/2 
        read(304,*)sbor2(i1,1),isbor2(i1,1),isbor2(i1,2),	!extension neto	!fev2007
     *             sbor2(i1,2),isbor2(i1,3),isbor2(i1,4)
      else
        write(67,*)' error4: subr. analys; ivar=',ivar,' est incorrecte'
        write(*,*) ' error4: subr. analys; ivar=',ivar,' est incorrecte'
        write(6970,*) ' error4: subr. analys; ivar=',ivar,
     *' est incorrecte'
        write(29,*)'subr analysis'													!bug
	  write(29,*)'error4: subr. analys; ivar=',ivar,' est incorrecte'				!bug

	  pause 'stop'
        stop
      end if
      goto 31

   1   if(itype(i).ne.5) then			!compte des panneaux qui sont épontilles	!février 2004
	  i4(i1) = i1 + i2 + i3						!février 2004
	else if(isect(i).eq.3) then					!février 2004
	  i5(i2) = i1 + i2 + i3						!février 2004
	else										!février 2004
	  i6(i3) = i1 + i2 + i3						!février 2004
	endif										!février 2004

   2  continue									!février 2004
	 neta = neto - nelo							!février 2004
   
c     cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     classement des maximum/minimum entre les panneaux
c
c     cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      if(langue==1) write(67,10)
      if(langue==2) write(67,11)

      if(langue==1) write(67,*)'les valeurs extrêmes de v sont:'
      if(langue==2) write(67,*)'extrem values of v are:'
      call classe(v(1,2),iv(1,3),iv(1,4),neto)
      call classe(v(1,1),iv(1,1),iv(1,2),neto)
      if(langue==1) write(67,*)'les valeurs extrêmes de u sont:'
      if(langue==2) write(67,*)'extrem values of u are:'
      call classe(u(1,2),iu(1,3),iu(1,4),neto)
      call classe(u(1,1),iu(1,1),iu(1,2),neto)
      if(langue==1) write(67,*)'les valeurs extrêmes de w sont:'
      if(langue==2) write(67,*)'extrem values of w are:'
      call classe(w(1,2),iw(1,3),iw(1,4),neto)
      call classe(w(1,1),iw(1,1),iw(1,2),neto)
	if (impr2.eq.-3) goto 100		!sept06
c -------------------      
      write(67,*)
      if(langue==1) then
	 write(67,*)'les valeurs extrêmes de sx bordage (z=0) sont:'
	else
       write(67,*)'extrem values of sx stress, plate (z=0) are:'
	endif
      call classe2(sborx(1,2),isborx(1,3),isborx(1,4),neta,i4)	!février 2004
      call classe2(sborx(1,1),isborx(1,1),isborx(1,1),neta,i4)	!février 2004
      
      if(langue==1) then
	 write(67,*)'les valeurs extrêmes de sy bordage (z=0) sont:'
	else
       write(67,*)'extrem values of sy stress, plate (z=0) are:'
	endif
      call classe2(sbory(1,2),isbory(1,3),isbory(1,4),neta,i4)	!février 2004
      call classe2(sbory(1,1),isbory(1,1),isbory(1,1),neta,i4)	!février 2004
      
      if(langue==1) then
	 write(67,*)'les valeurs extrêmes de txy bordage (z=0) sont:'
	else
       write(67,*)'extrem values of txy shear stress, plate (z=0) are:'
	endif
      call classe2(tau(1,2),itau(1,3),itau(1,4),neta,i4)			!février 2004
      call classe2(tau(1,1),itau(1,1),itau(1,1),neta,i4)			!février 2004
      
      if(langue==1) then
	 write(67,*)'les valeurs extrêmes de sig comp bordage (z=0) sont:'
	else
       write(67,*)'extrem values of combined stress, plate (z=0) are:'
	endif
      call classe2(sbor(1,2),isbor(1,3),isbor(1,4),neta,i4)		!février 2004
      
      if(impr2.ge.0) then
      if(langue==1) then
       write(67,*)'les valeurs extrêmes de sigma comp (z=+d/2) sont:'
	else
       write(67,*)'extrem values of combined stress, plate(z=+d/2) are:'
	endif
      call classe2(sbor1(1,2),isbor1(1,3),isbor1(1,4),neta,i4)	!février 2004

      if(langue==1) then
       write(67,*)'les valeurs extrêmes de sigma comp (z=-d/2) sont:'
	else
       write(67,*)'extrem values of combined stress, plate(z=-d/2) are:'
	endif
      call classe2(sbor2(1,2),isbor2(1,3),isbor2(1,4),neta,i4)	!février 2004
      endif
c ------------------
      write(67,*)
      if(indaig.eq.1) then
   
      if(langue==1) then
	 write(67,*)'les valeurs extrêmes de sy jas cadre sont:'
	else
       write(67,*)'extrem values of sy stress jwf frame are:'
	endif
      call classe2(sjas(1,2),isjas(1,3),isjas(1,4),neta,i4)		!février 2004
      call classe2(sjas(1,1),isjas(1,1),isjas(1,1),neta,i4)		!février 2004
 
      if(langue==1) then
	 write(67,*)'les valeurs extrêmes de tau jas cadre sont:'
	else
       write(67,*)'extrem values of txy shear stress jwf frame are:'
	endif
      call classe2(tjas(1,2),itjas(1,3),itjas(1,4),neta,i4)		!février 2004
      call classe2(tjas(1,1),itjas(1,1),itjas(1,1),neta,i4)		!février 2004
 
      if(langue==1) then
	 write(67,*)'les valeurs extrêmes de sy jab cadre sont:'
	else
       write(67,*)'extrem values of sy stress jwp frame are:'
	endif
      call classe2(sjab(1,2),isjab(1,3),isjab(1,4),neta,i4)		!février 2004
      call classe2(sjab(1,1),isjab(1,1),isjab(1,1),neta,i4)		!février 2004

      if(langue==1) then
	 write(67,*)'les valeurs extrêmes de tau jab cadre sont:'
	else
       write(67,*)'extrem values of txy shear stress jwb frame are:'
	endif
      call classe2(tjab(1,2),itjab(1,3),itjab(1,4),neta,i4)		!février 2004
      call classe2(tjab(1,1),itjab(1,1),itjab(1,1),neta,i4)		!février 2004
   
      if(langue==1) then
       write(67,*)'les valeurs extrêmes de sigma comp jas cadre sont:'
	else
       write(67,*)'extrem values of sc jwf frame (comb. stress) are:'
	endif
      call classe2(ajas(1,2),iajas(1,3),iajas(1,4),neta,i4)		!février 2004

      if(langue==1) then
       write(67,*)'les valeurs extrêmes de sigma comp jab cadre sont:'
	else
       write(67,*)'extrem values of sc jwp frame (comb. stress) are:'
	endif
      call classe2(ajab(1,2),iajab(1,3),iajab(1,4),neta,i4)		!février 2004

      if(langue==1) then
       write(67,*)'les valeurs extrêmes de sigma comp sem cadre sont:'
	else
       write(67,*)'extrem values of sc flange frame (comb. stress) are:'
	endif
      call classe2(asem(1,2),iasem(1,3),iasem(1,4),neta,i4)		!février 2004
      endif
c -------------------      !épontilles							!février 2004
      if (i2.gt.0) then ! si on a des épontilles en double t      !février 2004       
	if(langue==1) then                                          !février 2004
	write(67,*)'les valeurs extrêmes de sy jas supérieure epon sont:'   
	else                                                                  
       write(67,*)'extrem  values of sy stress jwf top pillar are:'         
	endif                                                             
      call classe2(sjasi(1,2),isjasi(1,3),isjasi(1,4),nelo,i5)               
      call classe2(sjasi(1,1),isjasi(1,1),isjasi(1,1),nelo,i5)           
  
      if(langue==1) then                                
	 write(67,*)'les valeurs extrêmes de sy jas inférieur epon sont:'  
	else                                                                 
      write(67,*)'extrem values of sy stress jwf bottom pillar are:'         
	endif                                                                   
      call classe2(sjabi(1,2),isjabi(1,3),isjabi(1,4),nelo,i5)                    
      call classe2(sjabi(1,1),isjabi(1,1),isjabi(1,1),nelo,i5)                  
 
      if(langue==1) then                                                     
	 write(67,*)'les valeurs extrêmes de tau jas épontille sont:'         
	else                                                                   
       write(67,*)'extrem values of shear stress jwf pillar are:'             
	endif                                                                  
      call classe2(tjasi(1,2),itjasi(1,3),itjasi(1,4),nelo,i5)                  
      call classe2(tjasi(1,1),itjasi(1,1),itjasi(1,1),nelo,i5)                  

      if(langue==1) then                                                   
	 write(67,*)'les valeurs extrêmes de tau axe neutre épon sont:'        
	else                                                                    
       write(67,*)'extrem values of txy shear stress'                          
       write(67,*)'at neutral axis pillar are:'                             
	endif                                                               
      call classe2(tjabi(1,2),itjabi(1,3),itjabi(1,4),nelo,i5)                
      call classe2(tjabi(1,1),itjabi(1,1),itjabi(1,1),nelo,i5)                 
   
      if(langue==1) then                                                  
      write(67,*)'les valeurs extrêmes de sig comp jas sup épon sont:'   
	else                                                                 
      write(67,*)'extrem values of comb. stress jwf top pillar are:'         
	endif                                                               
      call classe2(ajasi(1,2),iajasi(1,3),iajasi(1,4),nelo,i5)                 
 
       if(langue==1) then                                                
      write(67,*)'les valeurs extrêmes de sig comp jas inf épon sont:'     
	else                                                                     
      write(67,*)'extrem values of sc jwf bottom'                            
      write(67,*)' pillar (comb. stress) are:'                                
	endif                                                                    
      call classe2(ajabi(1,2),iajabi(1,3),iajabi(1,4),nelo,i5)                  

      if(langue==1) then                                                
      write(67,*)'les valeurs extrêmes de sig comp axe n. épon sont:'    
	else                                                                 
      write(67,*)'extrem values of sc neutral axis'                          
      write(67,*)' pillar (comb. stress) are:'                               
	endif                                                                   
      call classe2(sborxi(1,2),isborxi(1,3),isborxi(1,4),nelo,i5)             
 
	if(langue==1) then                                                  
       write(67,*)'les valeurs extrêmes de sig comp sem sup épon sont:'  
	else                                                                 
       write(67,*)'extrem values of sc flange top'                         
       write(67,*)' pillar (comb. stress) are:'                                
	endif                                                                       
      call classe2(asemi(1,2),iasemi(1,3),iasemi(1,4),nelo,i5)                  

     	if(langue==1) then                                                    
       write(67,*)'les valeurs extrêmes de sig comp sem inf épon sont:'        
	else                                                                  
       write(67,*)'extrem values of sc flange bottom'                        
       write(67,*)' pillar (comb. stress) are:'                              
	endif                                                                  
      call classe2(sboryi(1,2),isboryi(1,3),isboryi(1,4),nelo,i5)                
      endif
c	--------------
      if (i3.gt.0) then  !si on a des épontilles à paroi mince                                              
 	if(langue==1) then                                                   
	write(67,*)'les valeurs extrêmes de sy supérieure épon sont:'         
	else                                                                    
       write(67,*)'extrem  values of sy stress top pillar are:'               
	endif                                                                
      call classe2(sjasi(1,2),isjasi(1,3),isjasi(1,4),nelo,i6)               
      call classe2(sjasi(1,1),isjasi(1,1),isjasi(1,1),nelo,i6)                 
  
      if(langue==1) then                                           
      write(67,*)'les valeurs extrêmes de sy inférieur épon sont:'   
	else                                                        
      write(67,*)'extrem values of sy stress bottom pillar are:'      
	endif                                                        
      call classe2(sjabi(1,2),isjabi(1,3),isjabi(1,4),nelo,i6)          
      call classe2(sjabi(1,1),isjabi(1,1),isjabi(1,1),nelo,i6)            
 
      if(langue==1) then                                                  
	 write(67,*)'les valeurs extrêmes de tau sommet épon sont:'          
	else                                                                 
       write(67,*)'extrem values of shear stress top pillar are:'         
	endif                                                               
      call classe2(tjasi(1,2),itjasi(1,3),itjasi(1,4),nelo,i6)                      
      call classe2(tjasi(1,1),itjasi(1,1),itjasi(1,1),nelo,i6)            
  
      if(langue==1) then                                              
	 write(67,*)'les valeurs extrêmes de tau axe neutre épon sont:' 
	else                                                                 
       write(67,*)'extrem values of txy shear stress'                     
       write(67,*)'at neutral axis pillar are:'                             
	endif                                                               
      call classe2(tjabi(1,2),itjabi(1,3),itjabi(1,4),nelo,i6)                
      call classe2(tjabi(1,1),itjabi(1,1),itjabi(1,1),nelo,i6)                
        
      if(langue==1) then                                                   
      write(67,*)'les valeurs extrêmes de sig comp sup épon sont:'             
	else                                                                  
      write(67,*)'extrem values of comb. stress jwf top pillar are:'        
	endif                                                              
      call classe2(ajasi(1,2),iajasi(1,3),iajasi(1,4),nelo,i6)               
    
      if(langue==1) then                                                
      write(67,*)'les valeurs extrêmes de sig comp inf épon sont:'        
	else                                                                 
      write(67,*)'extrem values of sc jwf bottom'                          
      write(67,*)' pillar (comb. stress) are:'                           
	endif                                                             
      call classe2(ajabi(1,2),iajabi(1,3),iajabi(1,4),nelo,i6)              

      if(langue==1) then                                                
      write(67,*)'les valeurs extrêmes de sig comp axe n. épon sont:'  
	else                                                               
      write(67,*)'extrem values of sc neutral axis'                     
      write(67,*)' pillar (comb. stress) are:'                         
	endif                                                            
      call classe2(sborxi(1,2),isborxi(1,3),isborxi(1,4),nelo,i6)      !février 2004
	endif															 !février 2004
c -------------------      

      write(67,*)
      if(indrai.eq.1) then
      if(langue==1) then
       write(67,*)'les valeurs extrêmes de sigma comp jas raid sont:'
	else
       write(67,*)'extrem values of sc jwf stiff (comb. stress) are:'
	endif
      call classe2(rjas(1,2),irjas(1,3),irjas(1,4),neta,i4)				!février 2004

      if(langue==1) then
       write(67,*)'les valeurs extrêmes de sigma comp jab raid sont:'
	else
       write(67,*)'extrem values of sc jwp stiff (comb. stress) are:'
	endif
      call classe2(rjab(1,2),irjab(1,3),irjab(1,4),neta,i4)				!février 2004

      if(langue==1) then
       write(67,*)'les valeurs extrêmes de sigma comp sem raid sont:'
	else
       write(67,*)'extrem values of sc flange stiff (comb. stress) are:'
	endif
      call classe2(rsem(1,2),irsem(1,3),irsem(1,4),neta,i4)				!février 2004
      endif
  100 continue

      
      return                                                            
  10  format(/'recapitulatifs de valeurs extremes calculees (deplac.',
     * ' et contraintes de comparaison'/70(1h-)/
     * '(par ordre decroissant ; unités = m et n/m2)'/t2,'valeurs',t18,
     *       'panneaux',t40,'positions (x,y)'/ )
  11  format(/'summary of the extreme values (displ & combined stress)'
     *  /54(1h*)/
     * '(ranked by values ; units = m and n/m2)'/t2,'values',t18,
     *       'panels',t40,'locations (x,y)'/ )
      end                                                               

c***********************************************************************
c***********************************************************************
c***********************************************************************

      subroutine classe(x,ix,iy,neto)                            	
c ***********************************************************************                                                        
c     subroutine de classement par ordre decroissant des valeurs extrêmes.
c
c     modifié : 13-3-95                     créer: 13-3-95
c
c************************************************************************

      implicit real *8 (a-h,o-z)                                     
      dimension  x(neto),ix(neto),iy(neto)                    		  

c     x(nel) avec nel=1,neto  : valeur à classer     
c     ix et iy  coordonnées des valeurs maximales pour chaque panneau 
c     !! les valeurs de x ne sont pas conservees. 
c     !! le vecteur x n'est donc plus utilisable par la suite
  
      
	do 3 j=1,neto 
        x1=-1.e30							! grande valeur négative
        n1=0
        do 4 k=1,neto                                                     
          if(x1.lt.x(k)) then
            x1=x(k)
            n1=k
          endif
   4    continue

	if(n1.ne.0) then												!2006
        if(dabs(x(n1)).ge.1.0e-15) write(67,10)x(n1),n1,ix(n1),iy(n1)
        x(n1)=-1.e30
	endif															!2006

   3  continue

c ------------------------------------------------------	                                                           
  10  format(e11.4,t20,i2,t45,'(',i2,',',i2,')')

      return 
      end    
	                                                           
c************************************************************************
c************************************************************************
c************************************************************************
c
c
      subroutine classe2(xi,ixi,iyi,nelo,ii)               !février 2004             	
c ***********************************************************************                                                        
c     subroutine de classement par ordre decroissant des valeurs extrêmes.
c
c     modifié : 05-02-2004                     créer: 05-02-2004
c
c************************************************************************

      implicit real *8 (a-h,o-z)                                     
      dimension  xi(nelo),ixi(nelo),iyi(nelo),ii(nelo)                    		  

c     x(nel) avec nel=1,neto  : valeur à classer     
c     ix et iy  coordonnées des valeurs maximales pour chaque panneau 
c     !! les valeurs de x ne sont pas conservees. 
c     !! le vecteur x n'est donc plus utilisable par la suite
  
      do 3 j=1,nelo 
        xi1=-1.e30							! grande valeur négative
        ni1=0
        do 4 k=1,nelo                                                     
          if(xi1.lt.xi(k)) then
            xi1=xi(k)
            ni1=ii(k)
	      ni2=k
          endif
   4    continue

       if(dabs(xi(ni2)).ge.1.0e-15) then
	 write(67,10)xi(ni2),ni1,ixi(ni2),iyi(ni2)
	 endif
        xi(ni2)=-1.e30

   3  continue

c ------------------------------------------------------	                                                           
  10  format(e11.4,t20,i2,t45,'(',i2,',',i2,')')

      return 
      end                                                               
