      subroutine modif(nc,ncondi)
      use sharedvar
      implicit real*8(a-h,o-z)
      character*80 texte
      character *40 att(16),aatt(16),text
      data att/' jonction de panneaux    ',             ! 1
     *       ' extremite libre  my=ny=nxy=ry=0',		! 2
     *       ' appui simple     w=0 et my=ny=nxy=0',	! 2
     *       ' appui simple   w=v=0 et my=nxy=0',		! 3
     *       ' appui simple   w=u=0 et my=ny=0',		! 4
     *       ' appui simple w=u=v=0 et my=0',		    ! 5
     *       ' encastrement w=u=v=dw/dy=0',		    ! 6
     *       ' axe de symetrie nxy=ry=v=dw/dy=0',		! 7
     *       ' appui simple    v=my=nxy=ry=0',     	! 8                                                                       
     *       ' appui simple    u=my=ny=ry=0',    		! 9                                                                     
     *       ' appui simple    u=v=0 et my=ry=0',  	! 10                                                                       
     *       ' axe de symetrie w=dw/dy=nxy=ny=0',  	! 11                                                                   
     *       ' double symetrie v=w=dw/dy=nxy=0',  	! 13                                                                     
     *       '     ',                                                                    
     *       '     ',                                                                    
     *       '     '/      
      data aatt/' junction of panels      ',			! 1
     *       ' free extremity  my=ny=nxy=ry=0',		! 1
     *       ' pinned end   w=0 et my=ny=nxy=0',		! 2
     *       ' pinned end   w=v=0 et my=nxy=0',		! 3
     *       ' pinned end   w=u=0 et my=ny=0',		! 4
     *       ' pinned end   w=u=v=0 et my=0',			! 5
     *       ' clamped end  w=u=v=dw/dy=0',		    ! 6
     *       ' symmetry axis(loc.oz)nxy=ry=v=dw/dy=0',! 7
     *       ' pinned end       v=my=nxy=ry=0',     	! 8                                                                       
     *       ' pinned end       u=my=ny=ry=0',    	! 9                                                                     
     *       ' pinned end       u=v=0 et my=ry=0',  	! 10                                                                       
     *       ' symmetry axis(loc.oy)w=dw/dy=nxy=ny=0',! 11                                                                   
     *       ' double symmetry  v=w=dw/dy=nxy=0',  	! 12                                                                     
     *       '     ',                                                                    
     *       '     ',                                                                    
     *       '     '/      

c**********************************************************************
c
c     subroutine modif
c
c
c    modif: juin 99
c           juin 2000 (corrections)
c**********************************************************************

      if(langue.ne.1) then
        do i=1,13
	    att(i)=aatt(i)
	  enddo
	endif
      if(neto.ne.1) goto 51
      nc=0
      noeud(1,1)=1
      noeud(1,2)=2
      ntn=2
      goto 801
c
c cette partie du programme engendre automatiquement les noeuds
c -------------------------------------------------------------
   51 kk=1
      do 110 ii=1,neto
        noeud(ii,1)=0
        noeud(ii,2)=0
  110 continue
      do 120 i=1,neto
        if(i.eq.neto) goto 135
        if (noh(i,1).eq.0) goto 121
        k=i+1
        do 130 l=k,neto
          if (noh(l,1).ne.noh(i,1))  goto 130
          if (noeud(l,2).eq.0)  noeud(l,2)=kk
  130   continue
  135   do 140 ll=1,10
          li=noh(i,ll)
          if(li.eq.0) goto 121
          if (noeud(li,1).eq.0)  noeud(li,1)=kk
  140   continue
  121   if (noeud(i,2).ne.0) goto 120
        noeud(i,2)=kk
        kk=kk+1
  120 continue

      do 150 jj=1,neto
        if(noeud(jj,1).ne.0) goto 150
        noeud(jj,1)=kk
        kk=kk+1
  150 continue
      ntn=kk-1
c
c  elimination  dans noh des coefficients donnant un surplus
c  d'equations de compatibilite
c  ---------------------------------------------------------
      nn=neto-1
      do 25 i=1,nn
        k=i+1
        do 35 l=k,neto
          if((noh(l,1).eq.noh(i,1)).and.(noh(l,2).ne.0)) goto 41
          goto 35
   41     do 45 j=2,10
            noh(l,j)=0
   45     continue
   35   continue
   25 continue

c  initialisation a zero de mcomp
c  ------------------------------
      do 50 i=1,2*neto
        mcomp(i,1)=0
        mcomp(i,2)=0
   50 continue
c
c  ecriture des relations de compatibilite dans mcomp
c  --------------------------------------------------
      mm=1
      do 200 i=1,neto
        do 32 j=1,10
          if(noh(i,j).eq.0) goto 200
          mcomp(mm,1)=i
          mcomp(mm,2)=noh(i,j)
          mm=mm+1
   32   continue
  200 continue
      nc=mm-1 ! nc = nbre d'équations de compatabilité

c  initalisation a zero de nno
c  ---------------------------
  801 do 800 i=1,neto
        nno(i,1)=0
        nno(i,2)=0
  800 continue
c
c  ecriture des conditions aux limites dans nno
c  --------------------------------------------
      read(55,*) texte            ! lecture du titre
      read(55,*) ncondi
      if(ncondi.eq.0) goto 910
      do 900 i=1,ncondi
  900 read(55,*) (nno(i,j),j=1,2)
  910 continue

      if(neto.eq.1) goto 930
      do 700 i=1,neto
        if(nno(i,1).eq.0)goto 920
        nn=nno(i,1)
        nd=noeud(nn,1)
        kk=0
        do 710 j=1,neto
          if((noeud(j,1).eq.nd).or.(noeud(j,2).eq.nd)) kk=kk+1
  710   continue
        nno(i,1)=nd
        if(kk.ne.1) nno(i,1)=noeud(nn,2)
  700 continue

  920 do 925 i=1,neto+1
        izut(i)=0
  925 continue

      do 940 i=1,ntn
        mm=nno(i,1)
        if(mm.eq.0) goto 945
        izut(mm)=nno(i,2)
  940 continue

  945 do 950 j=1,ntn
        nno(j,1)=j
        nno(j,2)=izut(j)
  950 continue
  930 continue

c     impressions
c     ---------------
	if(langue==1) write(66,210)  ! cond. de bord
	if(langue==2) write(66,310)  ! cond. de bord
      do 931 i=1,ntn
        ix=nno(i,2)+1
        write(66,214)(nno(i,j),j=1,2),att(ix)
  931 continue

      if(langue==1) write(66,211) ! n° des noeuds
      if(langue==2) write(66,311) ! n° des noeuds
      do 867 i=1,neto
        write(66,216)i,(noeud(i,j),j=1,2)
  867 continue

      if(nc.ne.0) then
        if(langue==1) write(66,212) ! rel. de compatibilité
        if(langue==2) write(66,312) ! rel. de compatibilité
        do 876 i=1,nc
          write(66,216)i,(mcomp(i,j),j=1,2)
  876   continue
      endif

c     test de vérification des donnees
c     --------------------------------
      i1=neto*8
      i2=4*(ntn+nc)
      if(i1.ne.i2) then
        write(*,*)  'le nombre de relation de compatibilté est errone.'
        write(*,*)  'the number of compatibility equation is wrong'
        write(*,*)  'stop'

        write(6970,*)  
     *'le nombre de relation de compatibilté est errone.'
        write(6970,*)  'the number of compatibility equation is wrong'
        write(6970,*)  'stop'


        write(66,*) 'subr. modif:'										!sept06
        write(66,*) 'le nombre de relation de compatibilté est errone.'	!sept06
        write(66,*) 'il doit y avoir une erreur dans les donnees.'		!sept06
        write(66,*) 'vérifier les noh de chaque panneau.'					!sept06
        write(66,*) 'noh= liste des panneaux qui suivent le panneau',		!sept06
     *              ' concerne'
        
	  write(29,*) 'subr. modif:'										!sept06		!bug
        write(29,*) 'le nombre de relation de compatibilté est errone.'	!sept06		!bug	
        write(29,*) 'il doit y avoir une erreur dans les donnees.'		!sept06		!bug
        write(29,*) 'vérifier les noh de chaque panneau.'					!sept06		!bug
        write(29,*) 'noh= liste des panneaux qui suivent le panneau',		!sept06		!bug
     *              ' concerne'											!sept06													!bug
		  
	  stop
      endif


      return

  210 format('description de l''assemblage des panneaux'/
     * 41(1h*)/' numero du noeud',t35,'type de conditions de bord')
  211 format(/' numero du panneau',t30,'noeud au depart',t60,
     *        'noeud d''arrivee')
  212 format(/' relation de compatibilite',t30,'entre le panneau ',
     *'arrivant',t60,'et le panneau partant'/t12,'numero')

  310 format('presentation of the structure''s mesh model'/41(1h*)/
     *  '  node number',t35,'type of boundary condition (y=0 or y=h)')
  311 format(/' panel number',t30,'node at departure (y=0)',
     *        t60,'node at arrival (y=h)')
  312 format(/' compatibility relation',t30,'between an arriving ',
     *'panel',t60,'and a departing panel'/t12,'number')

  214 format(t5,i7,t35,i4,a40)
  216 format(t5,i7,t35,i7,t65,i7)

      end
