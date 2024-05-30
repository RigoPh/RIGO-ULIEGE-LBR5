      subroutine dcns_geom_n52_53(hya,dya,wya,tya,xmin,xmax)
      
      use param_section
      
      implicit double precision (a-h,o-z)
      character*6 type(3)

c***********************************************************************
c     subroutine semel
c     ****************
c     cette subroutine de modification des dimensions des semelles en fonction
c     de restrictions géométriques.
c
c     dw = hauteur   de l'âme (web),
c     tw = épaisseur de l'âme (web),
c     df = largeur   de la semelle (flange),
c     tf = épaisseur de la semelle (flange),
c
c
c***********************************************************************
c   les variables de conception  ; nbrxi = nvar(nel)
c   -------------------------------------------------
c       1       delta =  épaisseur du bordage
c       2       hya   =  hauteur de l'âme des aiguilles
c       3       dya   =  épaisseur de l'âme des aiguilles
c       4       wya   =  largeur des semelles des aiguilles
c       5       epsa  =  entredistance entre aiguilles
c       6       hxr   =  hauteur de l'âme des raidisseurs
c       7       dxr   =  épaisseur de l'âme des raidisseurs
c       8       wxr   =  largeur des semelles des raidisseurs
c       9       epsr  =  entredistance entre raidisseurs
c       variables de conceptions pour les épontilles:            !février 2004
c       1       hya   =  hauteur de la demi-âme                  !février 2004
c       2       dya   =  épaisseur de l'âme                      !février 2004
c       3       wya   =  largeur de la semelle                   !février 2004
c       4       epais =  épaisseur de la paroi mince             !février 2004
c       5       epsa  =  entredistance entre épontilles          !février 2004

      type(1)='cadre'
      type(2)='raid.'
      type(3)='épont.'                                                                             !février 2004

c ****************************************************************
c 1.    initialisations et calcul du ratio al2
c ****************************************************************
      dw=hya
      tw=dya
      df=wya
      tf=tya

c     pour condition 8.tf < df
      al2= sqrt(df/(8.*tf))
c     pour condition  df < 16.tf
      sq2= sqrt(2.)

	al3= sq2/al2

c ******************************************************************
c 2.  algorithme pour al2<1     c.à.d  8.tf < df  n'est pas vérifié
c ******************************************************************

      if ((al2.lt.1).or.(al2.gt.sq2)) then

c 2.2  al2 < 1
c      ---------
      if (al2.lt.1) then
	   df=df/al2
	   tf=tf*al2
	endif

c 2.3  al2 > 1.41   (al3 < 1)
c      ----------------------------
      if (al2.gt.sq2) then
	   df=df*al3
	   tf=tf/al3
	endif

      endif

c 2.1  1 < al2 < 1.41 (ou 2)
c      ---------------------

!      write(*,*) 'al2=',df/(8.*tf)
!      write(*,*) 'al3=',16.*tf/df


c ****************************************************************
c 4.  vérification des bornes
c ****************************************************************
      k=0
      if (df.lt.xmin) then
        write(iu_31(iboat),*) ' borne atteinte  df=',df,'  xmin=',xmin
        write(29 ,*) ' borne atteinte  df=',df,'  xmin=',xmin			!sept2006			!bug
	  tf=(xmin/df)*tf
	  df= xmin
        k=1
      else if (df.gt.xmax) then
        write(iu_31(iboat),*) ' borne atteinte  df=',df,'  xmax=',xmax
        write(29 ,*) ' borne atteinte  df=',df,'  xmax=',xmax			!sept2006			!bug
	  tf=(xmax/df)*tf
	  df= xmax
        k=1
      endif

c ****************************************************************
c 6.  résultats
c ****************************************************************

      hya=dw
      dya=tw
      wya=df
      tya=tf

      return
      end
