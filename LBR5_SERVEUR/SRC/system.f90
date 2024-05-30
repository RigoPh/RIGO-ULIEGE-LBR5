subroutine sysmat(a,c,n,ns,x,na)                                  



implicit double precision(a-h,o-z)                                         
dimension d(20),ipos(20),x(360)    !x(ns*n)   
dimension a(400),c(360)

!-----------------------------------------------------------------------
!    sysmat est utilisé par linea mais aussi par bateau !!!!!!!!!!!
!-----------------------------------------------------------------------
!
!     subroutine sysmat (n,ns,x,na) + common a,c
!     subroutine de resolution d'un systeme de n equations a n inconnues
!     par la methode des pivots.
!
!     soit le systeme [a] x = c
!        a la matrice,  c le vect indep. et x les inconnues (solutions)
!        c peut contenir ns vecteurs indépendants
!
!     n  = nbr d'inconnues  (taille du système)
!     na = dimension de la matrice [a] que l'on forme hors du vecteur
!          situe dans le common (na=20)
!     ns = nbre de vecteurs independants (ici "ns" max vaut 340/20=17)
!
!     dimensions nécessaires : a(na,na);x(na*ns);c(na*ns)
!     nb: les vecteurs d et ipos sont des vecteurs de travail.
!
!     modif: 21-8-94                         création : thèse de ph. rigo
!            avril 99 (lbr5.1)
!-----------------------------------------------------------------------

do k=1,n                                                       
   ipos(k)=0                                                         
   d(k)=a(k)                                                         
enddo

call pmaxar(d,1,n,umax,ip,na)                                     
a(1)=umax                                                         
ipos(1)=ip                                                        
d(ip)=d(1)  
                                                      
do i=2,n                                                       
   a(i)=d(i)/a(1)
enddo

                                                 
do i=2,n                                                      

   do j=1,n                                                       
      ia=j+(i-1)*na                                                     
      d(j)=a(ia)                                                        
   enddo

   jaux=i-1                                                          
   do k=1,jaux                                                    
      ip=ipos(k)                                                        
      ia=k+(i-1)*na                                                     
      a(ia)=d(ip)                                                       
      d(ip)=d(k)                                                        
      jauxp=k+1                                                         
   
      do kl=jauxp,n                                                  
         ial=kl+(k-1)*na                                                   
         d(kl)=d(kl)-a(ial)*a(ia)                                          
      enddo
   enddo
   
   call pmaxar(d,i,n,umax,ip,na)                                     
   ii=i+(i-1)*na                                                     
   a(ii)=umax                                                        
   ipos(i)=ip                                                        
   d(ip)=d(i)                                                        
   ix=i+1                                                            
   
   
   if(ix.le.n) then                                                
      do km=ix,n                                                     
         iam=km+(i-1)*na                                                   
         a(iam)=d(km)/a(ii)                                                
      enddo
   endif
enddo

do ij=1,ns                                                    
   do i=1,n                                                      
      ip=i+(ij-1)*na                                                    
      d(i)=c(ip)    
   enddo
                                                
   nm=n-1                                                            
   do i=1,nm                                                     
      ip=ipos(i)                                                        
      ir=i+(ij-1)*na                                                    
      c(ir)=d(ip)                                                       
      d(ip)=d(i)                                                        
      ijk=i+1                                                           
      do j=ijk,n                                                    
         ji=j+(i-1)*na                                                     
         d(j)=d(j)-a(ji)*c(ir)                                             
      enddo
   enddo
   nn=n+(n-1)*na                                                     
   nx=n+(ij-1)*na                                                    
   x(nx)=d(n)/a(nn)                                                  
   naux=n-1                                                          
   do i=1,naux                                                   
      iz=n-i                                                            
      izp=iz+1                                                          
      izc=iz+(ij-1)*na                                                  
      d(iz)=c(izc)                                                      
      do j=izp,n                                                    
         izj=iz+(j-1)*na                                                   
         izx=j+(ij-1)*na                                                   
         d(iz)=d(iz)-a(izj)*x(izx)                                         
      enddo
      
      izi=iz+(iz-1)*na                                                  
      izx=iz+(ij-1)*na                                                  
      x(izx)=d(iz)/a(izi) 
   enddo
                                          
enddo
return                                                            
end                                                               





subroutine pmaxar(x,ib,ie,umax,is,na)                             
implicit double precision(a-h,o-z)                                         
dimension x(na)                                                   
!-----------------------------------------------------------------------
!                                                                       
!     subroutine maxar                                                  
!     subroutine de recherche d'une valeur maximum en valeur absolue    
!     dans un groupe de valeurs donnees.                                
!                                                                       
!-----------------------------------------------------------------------
dumax=x(ib)                                                       
is=ib                                                             

if(ib.lt.ie)then
   ibp=ib+1                                                      
   do i=ibp,ie                                                     
      if(dabs(dumax).lt.dabs(x(i))) then
         dumax=x(i)    
         is=i
      endif                                                       
   enddo
else
   umax=dumax                                                   
endif

umax=dumax
return                                                           
end                                                              
