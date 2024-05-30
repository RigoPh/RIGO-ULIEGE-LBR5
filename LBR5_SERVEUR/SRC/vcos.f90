double precision function vcos(a,b)
implicit double precision(a-h,o-z)                                         


! pi=acos(-1.d00) ancienne valeur de pi mais elle ne donnait pas de bons resultats 
! compte tenu du fait qu elle n est pas toujours calcul�e de la m�me mani�re.
! d'o� pi=2.d00*acos(0.d00), l� on est sur de le calculer tt le temps de la m�me mani�re


pi=2.d00*acos(0.d00)                                                 
x=a                                                              
y=b  
                                                            
d=dcos(((x+y)/180.d00)*pi)

if (dabs(y).gt. 0.5) then                              
   x=x+y                                                            
   y=0.d00                                                          
endif

do while (x.lt.0.) 
    x=x+360.d00                                                         
enddo

nn=1                                                             
n=0                                                              


do while (x.ge.180.)                                            
   x=x-180.d00                                                         
   n=n+1                                                            
enddo

if(n.ne.0) nn=(-1)**n                                            

if((x+y).ge.90.) then                                            
  x=180.d00-x                                                         
  y=-y                                                             
  vcos=-nn*dcos((x+y)*pi/180.d00) ! (*)          
else                                                             
  vcos=nn*dcos((x+y)*pi/180.d00)                                      
endif                                                            

if((x.le.90.).and.(x.gt.89.99)) vcos=nn*dsin(y*pi/180.d00)  
! si x+y=90 r�sultat incorrect avec (*) d'o� cette ligne pour avoir bien 0        


return                                                           
end                                                              
