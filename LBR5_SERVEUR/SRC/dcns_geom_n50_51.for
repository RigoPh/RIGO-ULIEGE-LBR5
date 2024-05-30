      subroutine dcns_geom_n50_51(hya,dya,wya,tya,xmin,xmax)
      
      use param_section  

      implicit double precision (a-h,o-z)
      character*6 type(3)

c***********************************************************************
c     subroutine semel
c     ****************
c     cette subroutine de modification des dimensions des semelles en fonction
c     de restrictions g�om�triques.
c
c     dw = hauteur   de l'�me (web),
c     tw = �paisseur de l'�me (web),
c     df = largeur   de la semelle (flange),
c     tf = �paisseur de la semelle (flange),
c
c
c***********************************************************************
c   les variables de conception  ; nbrxi = nvar(nel)
c   -------------------------------------------------
c       1       delta =  �paisseur du bordage
c       2       hya   =  hauteur de l'�me des aiguilles
c       3       dya   =  �paisseur de l'�me des aiguilles
c       4       wya   =  largeur des semelles des aiguilles
c       5       epsa  =  entredistance entre aiguilles
c       6       hxr   =  hauteur de l'�me des raidisseurs
c       7       dxr   =  �paisseur de l'�me des raidisseurs
c       8       wxr   =  largeur des semelles des raidisseurs
c       9       epsr  =  entredistance entre raidisseurs
c       variables de conceptions pour les �pontilles:            !f�vrier 2004
c       1       hya   =  hauteur de la demi-�me                  !f�vrier 2004
c       2       dya   =  �paisseur de l'�me                      !f�vrier 2004
c       3       wya   =  largeur de la semelle                   !f�vrier 2004
c       4       epais =  �paisseur de la paroi mince             !f�vrier 2004
c       5       epsa  =  entredistance entre �pontilles          !f�vrier 2004

      type(1)='cadre'
      type(2)='raid.'
      type(3)='�pont.'                                                                             !f�vrier 2004

c ****************************************************************
c 1.    initialisations et calcul du ratio al2
c ****************************************************************
      dw=hya
      tw=dya
      df=wya
      tf=tya


c     pour condition tw < tf
      al2= sqrt(tf/tw)
c     pour condition  tf < 2.tw
      sq2= sqrt(2.)

	al3= sq2/al2

c ******************************************************************
c 2.  algorithme pour al2<1     c.�.d  tw < tf  n'est pas v�rifi�
c ******************************************************************

      if ((al2.lt.1).or.(al2.gt.sq2)) then


c 2.2  al2 < 1
c      ---------
      if (al2.lt.1) then
	   tf=tf/al2
	   tw=tw*al2
	endif

c 2.3  al2 > 1.41   (al3 < 1)
c      ----------------------------
      if (al2.gt.sq2) then
	   tf=tf*al3
	   tw=tw/al3
	endif

      endif

c 2.1  1 < al2 < 1.41 (ou 2)
c      ---------------------

!      write(*,*) 'al2=',tf/tw
!      write(*,*) 'al3=',2.*tw/tf










c ****************************************************************
c 4.  v�rification des bornes
c ****************************************************************
      k=0
      if (tw.lt.xmin) then
        write(iu_31(iboat),*) ' borne atteinte  df=',df,'  xmin=',xmin
        write(29 ,*) ' borne atteinte  df=',df,'  xmin=',xmin			!sept2006			!bug
	  tf=(xmin/tw)*tf
	  tw= xmin
        k=1
      else if (tw.gt.xmax) then
        write(iu_31(iboat),*) ' borne atteinte  df=',df,'  xmax=',xmax
        write(29 ,*) ' borne atteinte  df=',df,'  xmax=',xmax			!sept2006			!bug
	  tf=(xmax/tw)*tf
	  tw= xmax
        k=1
      endif




c ****************************************************************
c 6.  r�sultats
c ****************************************************************

      hya=dw
      dya=tw
      wya=df
      tya=tf

      return
      end
