      subroutine dcns_geom_n53(hya,dya,wya,tya,xmin,xmax,ind,iprint,
     *                 nel,ityp1,isect1,impr2)	!sept2006                                                 !f�vrier 2004
      implicit real *8 (a-h,o-z)
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

c     pour condition  df < 16.tf
      al2= sqrt((16.*tf)/df)



c ******************************************************************
c 2.  algorithme pour al2<1     c.�.d  df < 16.tf  n'est pas v�rifi�
c ******************************************************************

      if (al2.lt.1) then
	   tf=tf/al2
	   df=df*al2
	endif


      !write(*,*) 'al2=',(16.*tf)/df

c ****************************************************************
c 4.  v�rification des bornes
c ****************************************************************
      k=0
      if (df.lt.xmin) then
        write(666,*) ' borne atteinte  df=',df,'  xmin=',xmin
        write(29 ,*) ' borne atteinte  df=',df,'  xmin=',xmin			!sept2006			!bug
	  tf=(xmin/df)*tf
	  df= xmin
        k=1
      else if (df.gt.xmax) then
        write(666,*) ' borne atteinte  df=',df,'  xmax=',xmax
        write(29 ,*) ' borne atteinte  df=',df,'  xmax=',xmax			!sept2006			!bug
	  tf=(xmax/df)*tf
	  df= xmax
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
