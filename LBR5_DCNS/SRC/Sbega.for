      subroutine sbega(text)!,negalt)		!sept06
      use sharedvar
      implicit real *8(a-h,o-z)
      dimension text(15)

c***********************************************************************
c     subroutine sbega
c     =================
c	cette subroutine est relative aux restrictions d'égalités [xi1= kcst * xi2 ]
c     ---------------------------------------------------------------------------
c     maximum 100 restrictions d'egalite
c
c 	données : nvar(nel)	    nbre de variables de conception pour le panneau nel
c               nxit(9,nel)   liste des variables de conception (1 à 9) du panneau nel
c
c	lecture : negalt	     nbre de contr. d'égalité pour toute la structure	
c		      mega(negalt,4) xi1= kcst * xi2  avec xi1 du panneau nel1 
c                                                    xi2 du panneau nel2
c                                                    kste = xi1/xi2 = constante (real)
c		       - mega(j,1)  xi1(=1à9), le nø de réf. de la var. xi1 (var. dépendant)
c                             ex: pour haig, mega(j,1)=2 
c                                 pour epsr, mega(j,1)=9
c		       - mega(j,2)  nø du panneau de cette variable xi1
c		       - mega(j,3)  xi2(=1à9), le nø de réf. de la var. xi2 (var. indépendant)
c		       - mega(j,4)  nø du panneau de cette variable xi2
c
c		      ega(j)		la constante = xi1/xi2 
c
c     calculé : negal(nel)	nbre de restr. d'égalité pour le panneau nel
c		      nvs(nel)	nbre de restr. d'égalité pour les panneaux 1 à nel-1    	
c
c       nxi2(9,nel)  nø d'ordre des variables xi (1à9) du panneau nel
c                    au niveau global de la structure (cad pour les neto panneaux)
c                    si = 0 : signifie que ce n'est pas une variable de conception.
c
c     créer   :  4- 9-95 
c
c     modifié : 19- 3-97	  test de vérification sur l'existance des xi choisis
c                6-12-99				             
c***********************************************************************


c     impressions des données
c     -----------------------
      write(666,2) text

      write(666,*)' nbr de restr. d''égalité=',negalt
	if ((impr2.ge.-2).and.(negalt.ne.0)) then		!sept06
        write(666,*)
        write(666,*)' nø    xi1  nel1    xi2  nel2    xi1/xi2'

	  do 4 i=1,negalt
	  write(666,5)i,(mega(i,k),k=1,4),ega(i)
    4   continue
	endif		

c     vérifications des données
c     ---------------------------
        it=0
        ixi=0

        do 3 i=1,negalt
        if(mega(i,2).eq.mega(i,4))then
          if(mega(i,1).eq.mega(i,3))then
          write(*,*) 'error 1 ds les données des contraintes d''égalité'
          write(6970,*) 
     *'error 1 ds les données des contraintes d''égalité'
          write(666,*)'error1 ds les données des contraintes d''égalité'
          write(666,*) 'egalité n°',i
          write(666,*) 'mega(i,1à4)=',(mega(i,kk),kk=1,4)
          
		write(29,*)'error1 ds les données des contraintes d''égalité'				!bug	
          write(29,*) 'egalité n°',i													!bug
          write(29,*) 'mega(i,1à4)=',(mega(i,kk),kk=1,4)								!bug

		pause 'stop'
          stop
	  endif	
	endif

        if(mega(i,2).lt.it)then
          write(*,*)  'error 2 : contraintes d''égalité mal classées'
          write(6970,*)  
     *'error 2 : contraintes d''égalité mal classées'
          write(666,*)'error 2 : contraintes d''égalité mal classées'
          write(666,*) 'egalité n°',i
          write(666,*) 'mega(i,1à4)=',(mega(i,kk),kk=1,4)
          
		write(29,*)  'error 2 : contraintes d''égalité mal classées'				!bug
          write(29,*) 'egalité n°',i													!bug
          write(29,*) 'mega(i,1à4)=',(mega(i,kk),kk=1,4)								!bug
		
		pause 'stop'
          stop
        endif

        if(mega(i,2).eq.it)then
          if(mega(i,1).le.ixi)then
          write(*,*)   'error 3: contraintes d''égalité mal classées'
          write(6970,*)   
     *'error 3: contraintes d''égalité mal classées'
          write(666,*) 'error 3: contraintes d''égalité mal classées'
          write(666,*) 'egalité n°',i
          write(666,*) 'mega(i,1à4)=',(mega(i,kk),kk=1,4)
            
		write(29,*) 'error 3: contraintes d''égalité mal classées'					!bug
          write(29,*) 'egalité n°',i													!bug
          write(29,*) 'mega(i,1à4)=',(mega(i,kk),kk=1,4)								!bug
 
		  pause 'stop'
            stop
          endif
	  endif
	
	  it=mega(i,2)	
	  ixi=mega(i,1)		

    3 continue


c       calcul de negal(nel)
c     --------------------
      do 6 nel=1,neto
        negal(nel)=0
          do 13 i=1,negalt
          if((mega(i,2)).eq.nel) negal(nel)=negal(nel)+1
   13   continue
    6 continue

          
c       calcul de  nvs(nel)
c     ---------------------
      nvs(1)=0
        do 7 nel=2,neto
          nvs(nel)=nvs(nel-1)+negal(nel-1)
    7 continue


c	calcul de  nxi2(9,nel)
c     ---------------------

	l=1
	do 8 nel=1,neto
	   do 9 k=1,9
	      do 10 i=1,nvar(nel)
                if((nxit(i,nel)).eq.k) then
                       nxi2(k,nel)=l
                         l=l+1
                       goto 11
                 endif
               if((nxit(i,nel)).gt.k) goto 11
   10       continue
   11    continue
    9    continue
    8 continue

c       vérification de l'existance des variables
c     -----------------------------------------
        do 15 i=1,negalt

      ic=nxi2(mega(i,1),mega(i,2)) * nxi2(mega(i,3),mega(i,4))
      if(ic.eq.0)then
        write(*,*) 'error : restriction d''égalité nø',i,' incorrecte'
        write(*,*) '        car faisant intervenir une variable ',
     *                     'de conception non selectionnée'

        write(6970,*)'error : restriction d''égalité nø',i,' incorrecte'
        write(6970,*) '        car faisant intervenir une variable ',
     *                     'de conception non selectionnée'

        write(666,*) 'error : restriction d''égalité nø',i,' incorrecte'
        write(666,*) '        car faisant intervenir une variable ',
     *                       'de conception non selectionnée'
        write(29,*) 'error : restriction d''égalité nø',i,' incorrecte'				!bug
        write(29,*) '        car faisant intervenir une variable ',					!bug
     *                       'de conception non selectionnée'							!bug
	  
	  pause 'stop'
        stop
      endif
  15  continue


c     impressions de negal,nvs et nxi2
c     ----------------------------------
     	if(impr2.ge.-2) then	!sept06
      write(666,*)
      write(666,*)'nel,negal, nvs,     nxi2'
	
      do 12 nel=1,neto
        write(666,'(i3,i4,i3,t14,9i4)')
     *   nel,negal(nel),nvs(nel),(nxi2(i,nel),i=1,9)
   12 continue	
	endif					!sept06

      return

c     ---------------------------------------------------------------      
  900 write(*,*) 'erreur à la lecture des restrictions d''égalité'
      write(6970,*) 'erreur à la lecture des restrictions d''égalité'
      write(29,*) 'error à la lecture des restrictions d''égalité'	!sept06			!bug
      pause 'stop'
      stop
	
c	formats
c     ========
    2 format(/' donnees relatives aux restrictions d''egalite'/
     *  t2,50(1h*)/15a4/)
    5 format(1x,i4,3x,i2,3x,i2,6x,i2,3x,i2,5x,e14.7)
      end
