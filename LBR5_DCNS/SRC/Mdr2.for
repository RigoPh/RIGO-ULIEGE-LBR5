      subroutine mdr2(ne,nsol,nc)
      use sharedvar
      implicit real*8(a-h,o-z)
      dimension a8(ne,8),b1(ne,nsol),nxi(9)
      dimension aux(400),zb(360)

c***********************************************************************
c
c     subroutine mdr2 (module optimisation)
c     ****************
c     calcul des dérivées des equ par la méthode des pseudo-charges
c
c input:
c     a     = matrice inverse du système  : [a] * (x) =(b)
c     zsn   = vecteur solution du système : [a] * (x) =(b)
c     a8,b1 = matrice et vecteur de travail
c     aux,zb= vecteurs de travail;[aux]*(x)=(zb) ; voir subr matr
c output:
c     dzsn= matrice contenant les dérivées de zsn par les variables de conception.
c           chaque colonne correspond à une dérivée (c.à.d. à une variable)
c                 -1
c        dzsn =  a   * ( db/dx - da/dx * zsn) = a * (b1 - a8 * zsn)
c
c***********************************************************************
c     modifié : 28-06-95 			                   créer: 19-8-94
c         	   4-12-95 (pour nsol cas de charge)                                                              
c         	  3-3-2001 (pour jlph termes de la series de fourier)                                                              
c                                                                       
c***********************************************************************
c     ivar = compteur des variables de conception (=1,ntot)
c     ntot = nbre total de variables de conception
c***********************************************************************

      call annuld(aux,160)
      call annuld(zb,16*nsol)

c  boucle 1 : boucle sur les variables de conception (ntot variables de conception) 
c             pour chaque panneau (ipan=1,neto), il y a nbrxi vatiables (k=1,nbrxi)
c     but : création de la matrice dérivée de a(ne,ne)   (ax=b)
      
      ivar=0
      do 1 nel=1,neto
          read(nel+400) sensh,sensa,sensb,sensc,sensd,sensaa,sensbb,		!extension neto	!fev2007
     *                  senscc,sensdd
          nbrxi=nvar(nel)
          ipt=ipts(nel)
          do 18 l=1,nbrxi
  18      nxi(l)=nxit(l,nel)

      do 1 kk=1,nbrxi
          ivar=ivar+1
          it=8*ne
          call annuld(a8,it)
          call annuld(b1,ne*nsol)
      
c  ordre des var. dans sens (u=1 v=2 wø=3 ny=4 nyx=5 my=6  ry=7   w =8) 
c  ordre des var. dans aux  (w=1 u=3 my=5 ny=7 nyx=9 ry=11 v =13  wø=15) 
      
      k=nxi(kk)
   
      call matr( 1,8,  1,k,nsol,aux,zb)           ! w(0)
      call matr( 2,8,ipt,k,nsol,aux,zb)           ! w(yo)
      call matr( 3,1,  1,k,nsol,aux,zb)           ! u(0)
      call matr( 4,1,ipt,k,nsol,aux,zb)           ! u(yo)
      call matr( 5,6,  1,k,nsol,aux,zb)           ! my(0)
      call matr( 6,6,ipt,k,nsol,aux,zb)           ! my(yo)
      call matr( 7,4,  1,k,nsol,aux,zb)           ! ny(0)
      call matr( 8,4,ipt,k,nsol,aux,zb)           ! ny(yo)
      call matr( 9,5,  1,k,nsol,aux,zb)           ! nyx(0)
      call matr(10,5,ipt,k,nsol,aux,zb)           ! nyx(yo)
      call matr(11,7,  1,k,nsol,aux,zb)           ! ry(0)
      call matr(12,7,ipt,k,nsol,aux,zb)           ! ry(yo)
      call matr(13,2,  1,k,nsol,aux,zb)           ! v(0)
      call matr(14,2,ipt,k,nsol,aux,zb)           ! v(yo)
      call matr(15,3,  1,k,nsol,aux,zb)           ! wø(0)
      call matr(16,3,ipt,k,nsol,aux,zb)           ! wø(yo)
      if(impr.eq.1) then
      write(66,*) 
      write(66,*) 'panneau nø',nel,' variable nø',kk
      write(66,*) '+++++++++++++++++++++++++++++++++'
      write(66,*) 
      write(66,*) '(da/dx) et (db/dx) avant subr. angle'
      do 100 i=1,16
  100 write(66,2)(aux((ii-1)*20+i),ii=1,8),(zb(i+(is-1)*16),is=1,nsol)
      endif

c     changement de coordonnees (repère local -> repère global)
c     =========================================================
      if(neto.eq.1) goto 7

      call anglesr(nel,dabs(philn(nel)),nsol,aux,zb)
      if(impr.eq.1) then
      write(66,*) 
      write(66,*) '(da/dx) et (db/dx) après subr angle - panneau nø',nel
      write(66,*) '----------------------------------------------------'
      do 101 i=1,16
  101 write(66,2) (aux((ii-1)*20+i),ii=1,8),(zb(i+(is-1)*16),is=1,nsol)
      endif

  7   continue
  
c     creation de la matrice a8 (da/dx) et du vecteur b1 (db/dx) du syst. (a*x=b).
c     ===========================================================================

      call rang2(nel,ne,a8,b1,nsol,nc,aux,zb)
      if(impr.eq.1) then
      write(66,*) '(da/dx) et (db/dx) après rang2'
      do 102 j=1,ne
      write(66,*)'lign. ',j,'   term ind =',(b1(j,is),is=1,nsol)
 102  write(66,6)(a8(j,i),i=1,8)
      endif


c     calcul de [dzsn](n) = [db/dx](n) - [da/dx](n,8) * zsn(8)
c     ========================================================
c dzsn(n) = [dzsn](n,ivar,is)     pour chaque variable xi et chaque is
c b1 	  = [db/dx](n,is)         car 1 colonne chaque cas de charge is
c a8      = [da/dx](n,8)
c zsn     = vect. solution(n,is)  car 1 colonne chaque cas de charge is 

      nel2=8*(nel-1)
      do 10 is=1,nsol
      do 10 i=1,ne
      	dzsn(i,ivar,is)=b1(i,is)
      	do 11 l=1,8
      	  dzsn(i,ivar,is)=dzsn(i,ivar,is) - a8(i,l) * zsn(l+nel2,is)
   11     continue
   10 continue
      if(impr.eq.1) then
      do 19 is=1,nsol
        write(66,*) 'cas de charge nø',is !fff
        write(66,*) 'zsn  pour panneau ',nel
        write(66,'(8e14.7)') (zsn(i,is),i=1+nel2,8+nel2)  ! plus de decimales                 
        write(66,*) 'db/dx - da/dx*zsn pour la variable no ',ivar
        write(66,6) (dzsn(i,ivar,is),i=1,ne) 
   19 continue
      endif

   1  continue
c     ****************************************************************
c     fin de la boucle principale. la matrice dzsn est constituée.
c     ****************************************************************


c     calcul du produit de matrice inverse a(ne,ne) par dzsn(ne,ntot,nsol)
c     ====================================================================
c     (le résultat est placé dans dzsn, b1 servant de vecteur de travail)
      if(impr.eq.1) write(66,17) 

      ntot=ivar
      do 14 is=1,nsol
      do 14 ivar=1,ntot
         call annuld(b1,ne*nsol)
      	 do 12  i=1,ne
      		do 9 j=1,ne
      		   b1(i,is)=b1(i,is)+a(i,j)*dzsn(j,ivar,is)
    9 		continue
   12 	 continue
      	 do 13 j=1,ne
      		dzsn(j,ivar,is)=b1(j,is)
   13 	 continue
      if(impr.eq.1) then
        write(66,*)'cas de charge nø',is
        write(66,*)'---------------------'
        write(66,*)'variable xi nø',ivar 
        do 104 nel=1,neto
      	j1=8*(nel-1)
          write(66,*)'dzsn relatif au panneau nø',nel 
          write(66,6) (dzsn(j,ivar,is),j=j1+1,j1+8)
  104   continue
      endif 
   14 continue

c     sauvetage des dzsn  (relecture dans bo2)
c     ===================
      do nel=1,neto
     	j1=8*(nel-1)
        do  is=1,nsol
          do  j=j1+1,j1+8
            write(700+nel) (dzsn(j,ivar,is),ivar=1,ntot) !dzsn(8*neto,9*neto,nsol)
          end do
        end do
      end do

    2 format(4(1x,e11.4)/4(1x,e11.4)/5(1x,e11.4)/)
    6 format(8e11.4)
   17 format('dérivées de zsn pour chaque variables xi'/40(1h-))
      return
      end

c***********************************************************************
c***********************************************************************

      subroutine matr(iaux,ivar,ipt,k,nsol,aux,zb)
      use sharedvar
      implicit real*8(a-h,o-z)
      dimension aux(400),zb(360)

c***********************************************************************
c
c     subroutine matr (module optimisation cfr subr. mdr2)
c     ****************
c     transfer des derivees (senc,sencc, ect.) dans aux et zb
c
c     modifié : 1-12-95 			             créer: 19-8-94
c 
c***********************************************************************
      aux(iaux    )= sensc(ivar,ipt,k)
      aux(iaux+ 20)=senscc(ivar,ipt,k)
      aux(iaux+ 40)= sensd(ivar,ipt,k)
      aux(iaux+ 60)=sensdd(ivar,ipt,k)
      aux(iaux+ 80)= sensa(ivar,ipt,k)
      aux(iaux+100)=sensaa(ivar,ipt,k)
      aux(iaux+120)= sensb(ivar,ipt,k)
      aux(iaux+140)=sensbb(ivar,ipt,k)
      
      do 1 is=1,nsol
       iaux2=iaux+(is-1)*16
       zb(iaux2)= -sensh(ivar,ipt,k,is)
   1  continue

      return
      end
