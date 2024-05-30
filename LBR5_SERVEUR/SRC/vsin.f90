double precision function vsin(a,b)                                    
implicit double precision(a-h,o-z)                                     


! pi=acos(-1.d00) ancienne valeur de pi mais elle ne donnait pas de bons resultats 
! compte tenu du fait qu elle n est pas toujours calculée de la même manière.
! d'où pi=2.d00*acos(0.d00), là on est sur de le calculer tt le temps de la même manière

pi=2.d00*acos(0.d00)                                              
x=a                                                          
y=b                                                          
if(dabs(y).gt.0.5) then
   x=x+y
   y=0.d00                                                      
endif

do while(x.lt.0.)
   x=x+360.                                                     
enddo                                                  

nn=1                                                         
n=0                                                          

do while(x.ge.180.)                                        
   x=x-180.                                                     
   n=n+1                                                        
enddo 


if(n.ne.0) nn=(-1)**n                                        

if((x+y).ge.90.) then
   x=180.-x                                                     
   y=-y                                                         
endif

vsin=nn*dsin((x+y)*pi/180.)                                  
if((x.le.90.).and.(x.gt.89.99)) vsin=nn*dcos(y*pi/180.)      


return                                                       
end                                                          
