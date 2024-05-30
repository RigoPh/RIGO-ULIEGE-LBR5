subroutine coord_transform()




use param_section
!use param_cout
!use param_opti_local



implicit double precision (a-h,o-z)

! 10.0 transformations des donnees (subr. modif et coord) =====================
! =============================================================================

if(langue.eq.1)write(*,100)'transformation des donnees (sr modif)'
if(langue.eq.2)write(*,100)'data transformation (sr modif)'
call modif()       

! compteur des restr. struct (img= img+2 si 2 rest. sur centre de gravité)
img=0  

if(langue.eq.1)write(*,100)'coordonnees des panneaux (sr coord)'
if(langue.eq.2)write(*,100)'panel coordinates (sr coord)'

! restictions sur centre de gravité
call coord()

!rewind(iu_15(iboat))
!rewind(iu_17(iboat))

 100 format(1x,a,i8)                                                                                          


return
end
