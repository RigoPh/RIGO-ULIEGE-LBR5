      subroutine mdr(nel,ne,phil,nvsmb,
     *               ijk,jlmax,nsol,
     *               nc,aux,zb)
      use sharedvar
      implicit real*8(a-h,o-z)
      dimension aux(400),zb(360)

c***********************************************************************
c
c     subroutine mdr                                                
c     ================
c     subroutine d'initialisation de la matrice de jonction. elle organi-
c     se en blocs de donnees par appel a la subroutine rang. lorsque la 
c     matrice est complete elle la transmet pour resolution dans la     
c     subroutine recip pour en calculer son inverse   

c     modifié : 27-11-95  module optimisation       créer: thèse de ph. rigo
c               29-03-99  nbre de terme non limité
c                                                                       
c***********************************************************************
      if(nel.eq.1) then
       do 51 j=1,ne
        do 50 i=1,nvsmb
   50    b(j,i)=0.
         do 51 i=1,ne
   51     a(i,j)=0.
      endif
      if(neto.eq.1) goto 7

c     changement de coordonnees (repère local -> repère global)
c     =========================================================

      call anglesr(nel,phil,nvsmb,aux,zb)


c     creation des matrices a et b :(a*x=b).
c     ============================================

   7  call rang(nel,ne,nvsmb,nsol,nc,aux,zb)

      if(nel.ne.neto) return


c     determination de l'inverse de la matrice a.
c     ============================================
c     blocka et kli sont des matrices de travail
c     input  : a est la matrice à inverser
c     output : a est la matrice inversée 

      if(impr.eq.1) then
        write(66,*)' la matrice a à inverser est :'
        do 554 j=1,ne                                                     
          write(66,*)'ligne nø ',j
          write(66,401)(a(j,i),i=1,ne)
 554    continue                       
        write(66,*)
        write(66,*)' la matrice b des termes indépendants :'
        do 555 j=1,ne                                                     
           write(66,*)'j=',j,' ',(b(j,is),is=1,nsol)
 555    continue                       
      endif

      call recip(ne,nemax,a)

      if(impr.eq.1) then
        write(66,*)' la matrice inverse est :'
        do 553 j=1,ne                                                     
          write(66,*)'ligne nø ',j
          write(66,401)(a(j,i),i=1,ne)
 553    continue
      endif
                                        
c     calcul des vecteurs solutions (zsn et solt)
c     ============================================
      
      do 2001 is=1,nsol
      do 2001 i=1,ne
        zsn(i,is)=0.
        do 2002 j=1,ne
          zsn(i,is)=zsn(i,is)+a(i,j)*b(j,is)
 2002   continue
 2001 continue

      if(impr.eq.1) then
      write(66,*)' le vecteur solution zsn est :'
      do 2005 is=1,nsol
        write(66,*) 'cas de charge nø ',is
        write(66,401) (zsn(i,is),i=1,ne)
 2005 continue
      endif


c     sauvetage des vecteurs solutions (zsn et solt)
c     ==============================================
      do 2006 j=1,neto
        do  is=1,nsol
	     i1=(j-1)*8+1
           write(700+j) (zsn(i,is),i=i1,i1+7)
        end do
 2006 continue
      

  401 format(8e11.4)
      return
      end
