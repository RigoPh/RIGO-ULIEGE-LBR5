subroutine lcc(pds,somme,codo,acof,rtm,edis)


use param_section
!use param_cout
!use param_opti_local

use PARAM_SECTION_VECTOR, ONLY : poidsLBR5_init_vector,lightweight_init_vector,deadweight_init_vector,lcc2_vector,lcc3_vector,lcc4_vector,lcc5_vector,lccscenario_vector

use dfwin
!==============================================================================
!						LIFE CYCLE COST Subroutine
!
!	Ce fichier permet de calculer les co�ts/revenus cacul�s avec la m�thodologie
!	d�finie par NAME dans le cadre du projet IMPROVE.
!
!	Quatre mod�les peuvent �tre calcul�s
!
!	Mod�le 2 : Cost of periodic maintenance
!	Mod�le 3 : Cost of fuel oil for engine(s)
!	Mod�le 4 : Operational earning or revenue
!	Mod�le 5 : Dismantling earning
!
!	Ces fonctions sont d�finies dans la dll glcmc.dll cr��e en C++
!
!	Input :
!		- Lightweight (Poids Structure + machines + etc.)
!		- Deadweight  (Poids utile => proportionnel aux revenus)
!		- poidsLBR5 (=pds) et poidsLBR5_init (pour calculer le Lightweight)
!
!	Output :
!		- somme = Life Cycle Cost = codo + acof + rtm + edis (si selected)
!		- codo = co�t Mod�le 2
!		- acof = co�t Mod�le 3
!		- rtm  = Revenu Mod�le 4
!		- edis = Revenu Mod�le 5
!
!	Rem.:
!
!	On a toujours Displacement = Lightweight + Deadweight
!
!	Pour l'optimisation, 2 scenarii sont envisageables :
!	Deadweight constant (Scenario 1) ou Displacement constant (Scenario 2)
!
!	Le lightweight n'est pas calcul� dans LBR-5 mais uniquement une partie !
!
!	Rem. :
!	!!! Besoin d'avoir d�j� les valeurs des panneaux pour calculer le poidsLBR5
!		avant d'utiliser cette sous-routine !!!
!==============================================================================

implicit double precision (a-h,o-z)

!	Declare an interface block to the routine to be called.

interface
   subroutine glcmc_init(a, b, c)
   double precision a, b
   integer*4 c
   end subroutine
end interface

interface 
	real function glcmc_codo()
	end function
end interface

interface
	real function glcmc_acof()
	end function
end interface

interface
	real function glcmc_rtm()
	end function
end interface

interface
	real function glcmc_edis()
	end function
end interface

double precision codo

!	Declare a pointer for the dll      
pointer (p,i)

!	Declare pointers for the functions
pointer (q, glcmc_init)
pointer (r, glcmc_codo)
pointer (s, glcmc_acof)
pointer (t, glcmc_rtm)
pointer (u, glcmc_edis)

logical status

! V�rification de la pr�sence des fichiers glcmc.cfg et glcmc.dll
open(4012,file='glcmc.cfg',status='old',ERR=98)
close(4012)
open(4012,file='glcmc.dll',status='old',ERR=99)
close(4012)
!

!	Calcul du LIGHTWEIGHT et du DEADWEIGHT	
!	call poidstotal(poids,0,0) ! Evaluation du poids de la structure LBR5

poidsLBR5_init = poidsLBR5_init_vector(iboat) !Th�oriquement, on ne devrait pas faire comme �a mais bug => � corriger (mais ainsi �a marche)
lightweight_init = lightweight_init_vector(iboat)
deadweight_init = deadweight_init_vector(iboat)
lcc2 = lcc2_vector(iboat)
lcc3 = lcc3_vector(iboat)
lcc4 = lcc4_vector(iboat)
lcc5 = lcc5_vector(iboat)
lccscenario = lccscenario_vector(iboat)

if (poidsLBR5_init.lt.1e-10) then !!! Ne devrait pas arriver
	write(*,*) '!!! VERIFIER dans lcc.for pourquoi poidsLBR5_init pas encore calcule'
	call poidstotal(poidsLBR5_init,0,0)
endif

lightweight = lightweight_init + (pds - poidsLBR5_init)/(1000*9.81)  
!pr avoir en tons
if (lccscenario.eq.2) then	
	! Sc�nario D�placement constant
	deadweight = deadweight_init - (pds - poidsLBR5_init)/(1000*9.81)
else						
    ! Sc�nario Deadweight constant
    deadweight = deadweight_init
endif


!	First, locate the dll and load it into memory
p = loadlibrary("glcmc.dll"C)
if (p == 0) then
   type *, "Error occurred opening glcmc.dll"
   type *, "Program aborting"
   goto 1000
endif	

!	Set up a pointer to the routines of interest
q = getprocaddress(p, "glcmc_init_"C)
if (q == 0) then
   type *, "Error occurred finding glcmc_init_ in glcmc.dll"
   type *, "Program aborting"
   goto 1000
endif

r = getprocaddress(p, "glcmc_codo_"C)
if (r == 0) then
   type *, "Error occurred finding glcmc_codo_ in glcmc.dll"
   type *, "Program aborting"
   goto 1000
endif

s = getprocaddress(p, "glcmc_acof_"C)
if (s == 0) then
   type *, "Error occurred finding glcmc_acof_ in glcmc.dll"
   type *, "Program aborting"
   goto 1000
endif

t = getprocaddress(p, "glcmc_rtm_"C)
if (t == 0) then
   type *, "Error occurred finding glcmc_rtm_ in glcmc.dll"
   type *, "Program aborting"
   goto 1000
endif

u = getprocaddress(p, "glcmc_edis_"C)
if (u == 0) then
   type *, "Error occurred finding glcmc_edis_ in glcmc.dll"
   type *, "Program aborting"
   goto 1000
endif

!	initialize the cost module
indCorrosion = 1

call glcmc_init(lightweight,deadweight,indCorrosion) 
! 0 => Mod�le normal; 1 => Avec corrosion
! Voir rapport d'Improve pour plus de d�tails

!	Call the functions

if (lcc2.eq.1) then
   codo = glcmc_codo()
else
   codo = 0.
endif
if (lcc3.eq.1) then
   acof = glcmc_acof()
else
   acof = 0
endif
if (lcc4.eq.1) then
   rtm = glcmc_rtm()
else
   rtm = 0
endif
if (lcc5.eq.1) then
   edis = glcmc_edis()
else
   edis = 0
endif

somme = codo + acof - rtm - edis

!	Unload the dll
status = freelibrary(p)

!NULLIFY (p)
!NULLIFY (q)
!NULLIFY (r)
!NULLIFY (s)
!NULLIFY (t)
!NULLIFY (u)
!deallocate (p)
!q=>NULL()

!	Check if the dll was unloaded (T for true)
!	type *, "freelibrary status was: ", status

1000	continue
return

98 if (langue.eq.1) then
		write(*,*) 'Fichier glcmc.cfg manquant ! Veuillez le copier dans le dossier de l''executable !'
   elseif (langue.eq.2) then
		write(*,*) 'File glcmc.cfg missing! Please copy it in the same folder than the LBR5.exe file'
   endif
   read(*,*)
   stop

99 if (langue.eq.1) then
		write(*,*) 'Fichier glcmc.dll manquant ! Veuillez le copier dans le dossier de l''executable !'
   elseif (langue.eq.2) then
		write(*,*) 'File glcmc.dll missing! Please copy it in the same folder than the LBR5.exe file'
   endif

   read(*,*)
   stop
return
end



