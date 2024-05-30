      subroutine hull()

	

      use param_section
!      use param_cout
!      use param_opti_local
      
	

      implicit double precision(a-h,o-z)
      dimension text(15)

c23456789012345678901234567890123456789012345678901234567890123456789012
c ******************************************************************
c     subroutine hull
c     ================

c     read of the data for the subroutine ushull
c
c     --------------------------------------------------------------
c
c     created   :  18-2-97   (dr. ph. rigo)
c
c     modified  :  3-3-97 : yield stress of stiffeners introduced as dat
c
c     last modification : 25-5-99
c
c *******************************************************************
c  .    department of naval architecture, anast                     .
c  .    university of liege                                         .
c  .    6 quai banning, 4000 liege, belgium                         .
c  .    (tel) +32 4 366 9225  (fax) +32 4 366 9133                  .
c  .    (e-mail) ph.rigo@ulg.ac.be                                  .
c *******************************************************************
c irestr (0 = non;1 = oui)
c     la resistance ultime est elle utilisée comme restrictions lors
c     de l'optimisation de la structure.
c     si oui, il s'agit de 2 restrictions d'ensemble relatives à toute
c     la structure et pas aux panneaux en particulier.
c     les bornes des restrictions sont : usagm et uhogm
c usagm= moment ultime maximum en sagging en n.m
c uhogm= moment ultime maximum en hogging en n.m
c    uhogm (>0) et usagm (<0)  en n.m

      read(iu_10(iboat),101,end=900)text
      read(iu_10(iboat),*,end=900) irestr
      if(irestr.eq.1) then
         read(iu_10(iboat),*,end=900) uhogm,usagm
       if(iopti.ge.1.and.itera.eq.0) then		!sept06
           write(iu_31(iboat),*)
           write(iu_31(iboat),*)' restrictions d''ensemble.'
           write(iu_31(iboat),*)' -------------------------'
         write(iu_31(iboat),*)' ultimate strength of ship girder'
         write(iu_31(iboat),*)
     *' mult(hogging>0) > ',uhogm,' = max hogging moment'
         write(iu_31(iboat),*)
     *' mult(sagging<0) < ',usagm,' = max sagging moment'
       endif
        endif

      read(iu_10(iboat),*,end=900) iult

c     iult = 1,2,3, ...
c       ce réfère à la méthode utilisée pour déterminer la résistance ul
c         de la poutre navire
c         si irestr= 1, alors  iult _ 0 car il faut qu'une méthode de ca

c     iult = 0 : ultimate strength is not considered (no data)
c     iult = 1 : paik  method (based on the caldwell approach)
c     iult = 2 : procol (based on the smith's progressive collapse analy
c     iult = 3 :
c     iult = 4 :
c
c23456789012345678901234567890123456789012345678901234567890123456789012

      if(iult.ne.0) write(*,'(a/)')'subroutine hull'

      iw=iu_11(iboat)

c 1.0 iult=0 : pas de calcul
c     =======================
      if(iult.eq.0) then
        if(irestr.eq.1) then
            write(*,*)
            write(*,*)'iult=0  n''est compatible avec irestr=1'
            write(*,*)'***************************************'
            write(*,*)'changez iult et définisez une méthode de calcul'
            write(*,*)
		  write(iu_14(iboat),*)
		  write(iu_14(iboat),*)
     *'iult=0  n''est compatible avec irestr=1'		
            write(iu_14(iboat),*)
     *'***************************************'			!sept06		!bug
            write(iu_14(iboat),*)
     *'changez iult et définisez une méthode de calcul'	
            write(iu_14(iboat),*)													!sept06		!bug

            write(*,*) 'stop'
 	        read(*,*)
            stop
          else
            return
          endif

c 2.0 iult=1 : paik/calwell approach
c     ==============================
        elseif(iult.eq.1) then

c     ship sizes
c     ----------
c  .  depth = depth of the vessel          (m.)
c  .  db    = depth of double-bottom       (m.)
c
      read(iu_10(iboat),*,end=900) depth,db

c   modeling on the ship components (deck, bottoms, side) based on the n
c   --------------------------------------------------------------------

c   nd = number of panels included in the upper deck   component
c   nb1= number of panels included in the inner bottom component (si dou
c   nb = number of panels included in the outer bottom component
c   ns = number of panels included in the two   sides  component

c   id (i=1,nd)  = list of panels included in the upper deck   component
c   ib1(i=1,nb1) = list of panels included in the inner bottom component
c   ib (i=1,nb)  = list of panels included in the outer bottom component
c   is9 (i=1,ns)  = list of panels included in the two   sides  component

c   number of a panel to use to evaluate the ultimate strength of a comp
c   kd   = nø of a panel for calculation of the upper deck   strength
c   kb1  = nø of a panel for calculation of the inner bottom strength
c   kb   = nø of a panel for calculation of the outer bottom strength
c   ksup = nø of a panel for calculation of the upper part of the side p
c   ksdw = nø of a panel for calculation of the lower part of the side p

c   yield stress in the stiffeners
c   sigy(indMateriau(nel)) = yield stress in the panels
c   sysd    = yield stress in the stiffeners of the deck elements
c   sysb1   = yield stress in the stiffeners of the inner bottom element
c   sysb    = yield stress in the stiffeners of the outer bottom element
c   syssup  = yield stress in the stiffeners of the upper side plate ele
c   syssdw  = yield stress in the stiffeners of the lower side plate ele

c rem : - il faut modéliser la structure complète (et pas une demi)
c       - si seule la moitié de la structures a été modéliésée pour le cas
c         alors il faut répéter 2 fois les éléments de façon à générer l'ensemble complet
c       - il faut donner les panneaux correspondants aux 2 côtés (side plate)

      read(iu_10(iboat),*,end=900) nd, (id(l), l=1,nd)
      read(iu_10(iboat),*,end=900) nb1,(ib1(l),l=1,nb1)
      read(iu_10(iboat),*,end=900) nb, (ib(l), l=1,nb)
      read(iu_10(iboat),*,end=900) ns, (is9(l), l=1,ns)

      read(iu_10(iboat),*,end=900) kd,  sysd
      read(iu_10(iboat),*,end=900) kb1, sysb1
      read(iu_10(iboat),*,end=900) kb,  sysb
      read(iu_10(iboat),*,end=900) ksup,syssup
      read(iu_10(iboat),*,end=900) ksdw,syssdw

        if(sysd.le.1.)  sysd  =sigy(indMateriau(kd))
        if(sysb.le.1.)  sysb  =sigy(indMateriau(kb))
        if(syssup.le.1.)syssup=sigy(indMateriau(ksup))
        if(syssdw.le.1.)syssdw=sigy(indMateriau(ksdw))
        if(kb1.ne.0)  then
          if(sysb1.le.1.) sysb1 =sigy(indMateriau(kb1))
        endif

c23456789012345678901234567890123456789012345678901234567890123456789012
!      if((itera.eq.0.and.iopti.ge.2).or.(iopti.le.1)) then

!	iunit = ??? !TODO C'était pas défini => mis en commentaire... A changer éventuellement
       if (iopti.le.1) then
!        write(iunit,100) text
!        write(iunit,*)
!     *'evaluation of the ultimate strength of ship girder'
!        write(iunit,*)'   using the paik algorithm (caldwell method)'
!        write(iunit,'(50(1h=)/)')
!        write(iunit,'(a,f6.3,a,f6.3,a/)')
!     *         ' depth=',depth,'  high of double bottom=',db,' (m.)'
!        write(iunit,'(a,4(1x,i2),a1)')
!     * 'number of panels included in each components(',nd,ns,nb,nb1,')'
!        write(iunit,'(a,5(20(1x,i2)/))') 'upper deck  :',(id(i),i=1,nd)
!        write(iunit,'(a,5(20(1x,i2)/))') 'two   sides :',(is9(i),i=1,ns)
!        write(iunit,'(a,5(20(1x,i2)/))') 'outer bottom:',(ib(i),i=1,nb)
!         if(kb1.ne.0)  then
!         write(iunit,'(a,5(20(1x,i2)/))')
!     *'inner bottom:',(ib1(i),i=1,nb1)
!         endif
!        write(iunit,*)
!        write(iunit,*) 
!     *'panels used for the ult. str. evaluation of long.',
!     *            ' comp. stiff. plates'
!        write(iunit,'(a,1x,i2)') 'upper deck   : ',kd
!        write(iunit,'(a,e11.4,a,e11.4)') '   sy plate =',sigy(kd),
!     *                                  ' sy stiff.=',sysd
!        write(iunit,'(a,1x,i2)') 'side plate up: ',ksup
!        write(iunit,'(a,e11.4,a,e11.4)') '   sy plate =',sigy(ksup),
!     *		sigy(indMateriau(ksup)),
!     *                                  ' sy stiff.=',syssup
!        write(iunit,'(a,1x,i2)') 'side plate dw: ',ksdw
!        write(iunit,'(a,e11.4,a,e11.4)') '   sy plate =',sigy(ksdw),
!     *		sigy(indMateriau(ksdw)),
!     *                                  ' sy stiff.=',syssdw
!        write(iunit,'(a,1x,i2)') 'outer bottom : ',kb
!        write(iunit,'(a,e11.4,a,e11.4)') '   sy plate =',sigy(kb),
!     *		sigy(indMateriau(kb)),
!     *                                  ' sy stiff.=',sysb
!          if(kb1.ne.0)  then
!          write(iunit,'(a,1x,i2)') 'inner bottom : ',kb1
!          write(iunit,'(a,e11.4,a,e11.4)') '   sy plate =',sigy(kb1),
!     *		sigy(indMateriau(kb1)),
!     *                                    ' sy stiff.=',sysb1
!          endif
!        write(iunit,*)
        endif

c 3.0 iult>1 : other approach
c     ===========================
      else
        write(*,*)
        write(*,*) 'this method is not available : iult=',iult
        write(*,'(45(1h=)/)')
!        write(iunit,*)
!        write(iunit,*) 'this method is not available : iult=',iult
!        write(iunit,'(45(1h=)/)')
          
        write(iu_14(iboat),*)																!bug
	  write(iu_14(iboat),*) 'subroutine hull'												!bug
        write(iu_14(iboat),*) 
     *'this method is not available : iult=',iult					!bug
        write(iu_14(iboat),'(45(1h=)/)')													!bug
 
	  write(*,*) 'ok?'
	  read(*,*)
      endif

      return

c23456789012345678901234567890123456789012345678901234567890123456789012

  900 write(*,*) 'error à la lecture dans subr. hull'
      write(iu_14(iboat),*) 'error à la lecture dans subr. hull'							!bug
	  read(*,*)
      stop

  101 format(15a4)
  100 format(//' donnees relatives à m ultime (hull girder)'/
     *         t2,50(1h*)/15a4/)

      end
