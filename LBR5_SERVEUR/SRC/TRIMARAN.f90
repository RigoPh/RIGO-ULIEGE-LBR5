subroutine TRIMARAN()

use param_section

implicit double precision (a-h,o-z)

integer*4 nbhd
integer*4 ncadre
integer*4 aez
double precision aneutrigen
double precision axeneutretri
double precision nai
double precision nai2
double precision disttri
double precision inertritot
double precision zmaxtri
double precision aey
double precision aroll
double precision az 
double precision MsphI
double precision MsphO
double precision MspsI
double precision MspsO
double precision Mtt
double precision Qsph
double precision Qsps
double precision inertri2
double precision inertritot2
double precision k


dimension secttri(decktri), stattri(decktri),inertri(decktri),aneutri(decktri),aneutrigen(decktri), secttri2(decktri) 
dimension inertri2(decktri)
dimension froll(castrim), aroll(castrim), az(castrim), MspsO(castrim), MsphO(castrim), MspsI(castrim), MsphI(castrim), Qsps(castrim)
dimension Qsph(castrim), sigmaspsO(castrim), sigmaspsI(castrim), sigmasphO(castrim), sigmasphI(castrim), tausps(castrim), tausph(castrim)
longtri=longtri*100

nai             =0.d00
nai2            =0.d00
aneutrigen      =0.d00
axeneutretri    =0.d00
inertritot      =0.d00
zmaxtri         =0.d00
aey             =0.d00
inertri2        =0.d00
inertritot2     =0.d00
k               =0.d00
do i=1,decktri

!decktri fixe le nombre de panneau

j=ptri(i)

!indice qui determine le panneau en question - POINTEUR
!en commencant par le cross deck
!*******************************

!UNITES
epsa = 100*panneau(j).epsa
delta = 100*panneau(j).delta
hya = 100*panneau(j).hya
dya = 100*panneau(j).dya
wya = 100*panneau(j).wya
tya = 100*panneau(j).tya

! 2 variables pour le nombre de cloison et de cadres du modele pour la flexion (splitting moment)

nbhd= INT4(AINT(longtri/100*1./width)) + 1.
ncadre= AINT(longtri/epsa)

!ajout manuel des cloisons

if (i .eq. 1) then
secttri(i)=nbhd*1*300
stattri(i)=(150)*(nbhd*1*300)
secttri2(i)=secttri(i)+delta*epsa*ncadre
secttri(i)=secttri(i)+delta*epsa*ncadre+ncadre*hya*dya+ncadre*wya*tya

    if ((((panneau(j).ksa .eq. 2) .AND. (panneau(j).ang .LT. 180)) .OR. ((panneau(j).ksa .eq. 1) .AND. (panneau(j).ang .GE. 180)))) then 
    stattri(i)=stattri(i)+ncadre*(delta*epsa)*(-delta/2)+ncadre*(hya*dya)*(-hya/2.-delta)+ncadre*(wya*tya)*(-delta-hya-tya/2.)
    else
    stattri(i)=stattri(i)+ncadre*(delta*epsa)*(delta/2)+ncadre*(hya*dya)*(hya/2.+delta)+ncadre*(wya*tya)*(delta+hya+tya/2.)
    endif
    
else
secttri(i)=0.d00
stattri(i)=0.d00
secttri2(i)=secttri(i)+delta*epsa*ncadre
secttri(i)=secttri(i)+delta*epsa*ncadre+ncadre*hya*dya+ncadre*wya*tya

    if ((((panneau(j).ksa .eq. 2) .AND. (panneau(j).ang .LT. 180)) .OR. ((panneau(j).ksa .eq. 1) .AND. (panneau(j).ang .GE. 180)))) then 
    stattri(i)=stattri(i)+ncadre*(delta*epsa)*(-delta/2)+ncadre*(hya*dya)*(-hya/2.-delta)+ncadre*(wya*tya)*(-delta-hya-tya/2.)
    else
    stattri(i)=stattri(i)+ncadre*(delta*epsa)*(delta/2)+ncadre*(hya*dya)*(hya/2.+delta)+ncadre*(wya*tya)*(delta+hya+tya/2.)
    endif
    
endif 
aez=ptri(1)
aneutri(i)=stattri(i)/secttri(i)
aneutrigen(i)=aneutri(i)+(z(j,3)-z(aez,3))*100
nai=nai+secttri(i)
nai2=nai2+secttri2(i)
!ajout manuel des cloisons
  
if (i .eq. 1) then
inertri(i)=(nbhd*1*(300**3.)/12.)+(nbhd*1*300)*(150-aneutri(i))**2+ &
                 (epsa*ncadre*(delta**3)/12.)+(delta*epsa*ncadre)*(delta/2.-aneutri(i))**2+ &
                 (ncadre*dya*(hya**3))/12.+(ncadre*hya*dya)*((hya/2.+delta)-aneutri(i))**2+ &
                 (ncadre*wya*tya**3)/12.+(ncadre*wya*tya)*((delta+hya+tya/2.)-aneutri(i))**2
                 
inertri2(i)=(nbhd*1*(300**3.)/12.)+(nbhd*1*300)*(150-aneutri(i))**2+ &
                 (epsa*ncadre*(delta**3)/12.)+(delta*epsa*ncadre)*(delta/2.-aneutri(i))**2                
else 
inertri(i)=      (epsa*ncadre*(delta**3)/12.)+(delta*epsa*ncadre)*(delta/2.-DABS(aneutri(i)))**2+ &
                 (ncadre*dya*(hya**3))/12.+(ncadre*hya*dya)*((hya/2.+delta)-DABS(aneutri(i)))**2+ &
                 (ncadre*wya*tya**3)/12.+(ncadre*wya*tya)*((delta+hya+tya/2.)-DABS(aneutri(i)))**2
                 
inertri2(i)=inertri2(i)+(epsa*ncadre*(delta**3)/12.)+(delta*epsa*ncadre)*(delta/2.-DABS(aneutri(i)))**2               
endif 

axeneutretri=axeneutretri+aneutrigen(i)*secttri(i)

if (i .eq. decktri) then 
axeneutretri=axeneutretri/nai
do p=1,decktri
aey=DABS(axeneutretri-((z(ptri(p),3)-z(aez,3))*100))
zmaxtri=DMAX1(aey,zmaxtri)
disttri=(axeneutretri-aneutrigen(p))**2
inertritot=inertritot+inertri(p)+(secttri(p)*disttri)
inertritot2=inertritot2+inertri2(p)+(secttri2(p)*disttri)
enddo
Zcdtri=inertritot/zmaxtri
zi=inertritot2/zmaxtri
endif
enddo 



!**********requiring data********************
fserv=1.d00                     !
fwv=1.d00                       !                      
Ysh=15.017                      ! remplacer par 34/2 peut etre une bonne idée voir  amirouche
vitesse=vitessemax*2.d00/3.d00  !
Wsh=1972.d00                    !
Ysh=15.017                      !    
Bmh=15.d00
Yo=13.153
Y1=7.5
Sbhd=width
Delta=17028.d00
Deltash=1972.d00
rho=1.025
Vsh=5682.40
Vcd=4567.94
Lsh=101
SigmasphImax= 0.d00
SigmasphOmax= 0.d00
SigmaspsImax= 0.d00
SigmaspsOmax= 0.d00
tauspsmax=0.d00
tauspsmax=0.d00
tausphmax=0.d00
tauspsmax=0.d00
Sigmaydlongi= 0d00
tauydlongi= 0.d00
Sigmaydtrans=0.d00
tauydtrans= 0.d00
Sigmaydtrans2= 0.d00


!********************************************
!**************Accelerations*****************
aheave=0.8*fwv*((0.2*vitesse/sqrt(longtot))+((34-600/longtot)/longtot))

!********************************************
!******Transverse torsional moment***********
 
Mtt=3.75*fserv*rho*(Vsh+Vcd)*Lsh*aheave

!********************************************
!**************Accelerations*****************
do j=1,castrim 
froll(j)= 13.d00*GMT(j)/largeurtri
if (froll(j) .GT.1) then
aroll(j)=aheave*((0.6*Ysh*(froll(j))**1.5)/largeurtri)
else 
aroll(j)=aheave*((0.6*Ysh)/largeurtri)
endif 
az=dsqrt(aheave**2+aroll(j)**2) 
!********************************************
!*************Splitting Moments**************
 
MsphI(j)=9.81*fserv*Wsh*(1+az(j))*(Ysh-Bmh/2)
MsphO(j)=9.81*fserv*Wsh*(1+az(j))*(Ysh-Yo)
MspsI(j)=9.81*fserv*(Delta/2-Deltash)*az(j)*(Ysh-Bmh/2)
MspsO(j)=9.81*fserv*(Delta/2-Deltash)*az(j)*(Ysh-Yo)

!******************************************** 
!*********Splitting shear force************** 
 
Qsph(j)=9.81*fserv*Wsh*(1+az(j))
Qsps(j)=9.81*fserv*(Delta/2-Deltash)*az(j) 

!*********************************************
!*********Calcul contraintes****************** 
 
SigmasphI(j)=(MsphI(j)/Zcdtri)*1000
SigmasphO(j)=(MsphO(j)/Zcdtri)*1000
SigmaspsI(j)=(MspsI(j)/Zcdtri)*1000
SigmaspsO(j)=(MspsO(j)/Zcdtri)*1000

tausph(j)=5*Qsph(j)/nAi
tausps(j)=5*Qsps(j)/nAi

tbhd=1.d00
tdk=12.d00
dcd=Dabs(panneau(16).hight)
k=(2*tdk*tbhd*(Sbhd**2)*(dcd**2))/((Sbhd*tbhd)+(dcd*tdk))

Sigmatt=(3000*(Yo-Y1)*Mtt)/(Sbhd*nbhd*(nbhd+1)*zi)
tautt=((60*Mtt)/(Sbhd*nbhd*(nbhd+1)*nai2))+((38*k*Mtt*(Yo-Y1)**2)/(2*Sbhd*nbhd*(nbhd**2-1)*inertritot2))   

SigmasphImax= max(SigmasphI(j),sigmasphImax)
SigmasphOmax= max(SigmasphO(j),sigmaspsOmax)
SigmaspsImax= max(SigmaspsI(j),sigmaspsOmax)
SigmaspsOmax= max(SigmaspsO(j),sigmaspsOmax)

tauspsmax=max(tausps(j),tauspsmax)			!(abis)
tausphmax=max(tausph(j),tausphmax)			!(bbis)


enddo                                     
!********************************************* 


!*********************************************
!*********Contraintes de Comparaison**********
 
Sigmaspsmax=max(SigmaspsImax,SigmaspsOmax)	!(a)
Sigmasphmax=max(SigmasphImax,SigmasphOmax)	!(b)

Sigmattmodsps=Sigmatt+0.6*Sigmaspsmax		!(c)
Sigmattmodsph=Sigmatt+0.6*Sigmasphmax		!(d)

tauttmodsps=tautt+0.6*tauspsmax				!(cbis)
tauttmodsph=tautt+0.6*tausphmax				!(dbis)

Sigmacd=max(Sigmasphmax,Sigmaspsmax,Sigmattmodsph,Sigmattmodsps)
taucd=max(tausphmax,tauspsmax,tauttmodsph,tauttmodsps)

Sigmaeq1=sqrt(Sigmaspsmax**2+3*tauspsmax**2)
Sigmaeq2=sqrt(Sigmasphmax**2+3*tausphmax**2)
Sigmaeq3=sqrt(Sigmattmodsps**2+3*tauttmodsps**2)
Sigmaeq4=sqrt(Sigmattmodsph**2+3*tauttmodsph**2)

!*********************************************
!***********Allowable stress level************

fsigmaws= 1.2
fsigmahg= 0.75
fhtslongi=0.919
fhtstrans=1

!Sigmaydlongi= 0.72*fhtslongi*SIGY(1)
!Sigmaydlongi=Sigmaydlongi/1000000.d00
!tauydlongi= 0.72*fhtslongi*SIGY(1)/sqrt(3.d00)
!tauydlongi=tauydlongi/1000000.d00

Sigmaydtrans=0.72*fhtstrans*SIGY(2)
Sigmaydtrans=Sigmaydtrans/1000000.d00
tauydtrans= 0.72*fhtstrans*SIGY(2)/sqrt(3.d00)
tauydtrans=tauydtrans/1000000.d00
Sigmaydtrans2=0.9*fhtstrans*SIGY(2)
Sigmaydtrans2=Sigmaydtrans2/1000000.d00


return
end