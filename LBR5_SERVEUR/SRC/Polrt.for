      subroutine polrt(xcof,cof,m,rootr,rooti,ier)
      implicit double precision(a-h,o-z)
      dimension xcof(9),cof(9),rootr(8),rooti(8)
c***********************************************************************
c
c     subroutine polrt
c     ****************
c     subroutine de resolution de l'equation du 8 eme ordre de la plaque
c     non chargee,raidie,par iterations succesives.
c
c     données   : xcof coef de l'équation polynomiale d'ordre m
c	            m    ordre de l'équation (m=8)
c	solutions : rootr  parties réelles
c	            rootr  parties imaginaires
c		        ier    indice de validité
c
c	vect de travail : cof
c
c      modif: 17-3-94				création : thèse ph. rigo                                                                 
c***********************************************************************
c      verifications
c      _____________
      ier=0
c     vérification si le terme en x**8 n'est pas nul (si m=8)
      if(xcof(m+1).eq.0) then
      ier=4
      return
      endif
      
c     vérification si m n'est pas nul (m=0) c.à.d equation de degré 0    
      if(m.le.0) then
      ier=1
      return
      endif

c     vérification si on a m < 36  c.à.d  équation de degré inférieur à 35    
      if((m-36).gt.0)then
      ier=2
      return
      endif

c***********************************************************************

c     recherche du coef. equ maximum et division par ce coef. max
      z=xcof(1)
      np=m+1
      do 180 i=2,np
      if(dabs(z)-dabs(xcof(i)))181,180,180
  181 z=xcof(i)
  180 continue
      do 182 i=1,np
  182 xcof(i)=xcof(i)/z

c     classement des coef dans cof par ordre décroissant
      do 40 l=1,m+1
        mt=m-l+2
   40 cof(mt)=xcof(l)

c***********************************************************************
c in  = nbre de fois que l'on change de pt de départ (xo=-10.0*yo ,yo=-10.0*x)
c ict = nbre d'itération avec le même pt de départ (xo + i yo)

      ifit=0
      n=m
      nx=m
      nxx=m+1
      n2=1

c     point de départ (xo + i yo)
   45 xo=.00500101
      yo=0.01000101
      in=0

c     changement de point de départ (xo + i yo)
   50 x=xo
      xo=-10.0*yo
      yo=-10.0*x
      x=xo
      y=yo
      ict=0
      in=in+1
      go to 60

   55 ifit=1
      xpr=x
      ypr=y
c     début d'une nouvelle itération
   60 ux=0.0
      uy=0.0
      v=0.0
      yt=0.0
      xt=1.0
      u=cof(n+1)
      if(u) 65,130,65

c     calcul de la fct (u + i v) et et sa dérivée (ux + i uy) au pt. (x+iy)
c     _____________________________________________________________________
c     (xt,yt) et (xt2,yt2) = nbre complexe = (x,y)**i
   65 do 70 i=1,n
      l=n-i+1
      xt2=x*xt-y*yt
      yt2=x*yt+y*xt
      u=u+cof(l)*xt2
      v=v+cof(l)*yt2
      fi=i
      ux=ux+fi*xt*cof(l)
      uy=uy-fi*yt*cof(l)
      xt=xt2
  70  yt=yt2

c     calcul du nouveau pt (x+dx , y+dy)
c ---------------------------------------
      sumsq=ux*ux+uy*uy
      if(sumsq) 75,110,75
   75 dx=(v*uy-u*ux)/sumsq
      dy=-(u*uy+v*ux)/sumsq
      x=x+dx
      y=y+dy
   78 if(dabs(dy)+dabs(dx)-1.0d-10)100,80,80
   80 ict=ict+1
c     vers nouvelle iteration (si ict<500) 
      if(ict-500) 60,85,85   
   85 if(ifit) 115,90,115
c     vers nouveau pt de départ (max 4 fois)
   90 if(in-5) 50,95,95     
   95 ier=3
      return

  100 do 105 l=1,nxx
        mt=m-l+2
        temp=xcof(mt)
        xcof(mt)=cof(l)
        cof(l)=temp
  105 continue

      itemp=n
      n=nx
      nx=itemp
      if(ifit) 120,55,120
  110 if(ifit) 115,50,115
  115 x=xpr
      y=ypr
  120 ifit=0
c     si y trop petit, solution réelle (x,0) sinon solution complexe (x,y)
  122 if(dabs(y/x)-1.0d-8) 135,125,125
  125 alpha=x+x
      sumsq=x*x+y*y
      n=n-2
      if(n) 155,155,140

  130 x=0.0
      nx=nx-1
      nxx=nxx-1
  135 y=0.0
      sumsq=0.0
      alpha=x
      n=n-1
      if(n) 155,155,140

  140 cof(2)=cof(2)+alpha*cof(1)
  145 do 150 l=2,n
        cof(l+1)=cof(l+1)+alpha*cof(l)-sumsq*cof(l-1)
  150 continue

  155 rooti(n2)=y
      rootr(n2)=x
      n2=n2+1
      if(n2.lt.9) then

      if(sumsq) 160,165,160
  160 y=-y
      sumsq=0.0
      go to 155
c     retour au début
  165 if(n.gt.0) goto 45

      endif

      return
      end
