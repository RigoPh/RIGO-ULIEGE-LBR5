      subroutine rang(nel,nvsmb,aux,zb)

      
      use param_section
!      use param_cout
!      use param_opti_local
      

      implicit double precision (a-h,o-z)
      dimension aux(2*mt_max*2*mt_max),zb(2*mt_max*(8+nsolmax))

c***********************************************************************
c
c     subroutine rang ,assemblage automatique
c     ----------------------------------------
c
c     modifié : 29-11-95 (module optimisation)      créer: thèse de ph. rigo
c     =========                                    
c
c les equations sont classees comme suit:
c  - d'abord les 4nc equations de compatibilite (soit nc equ de compatibilite)
c  - ensuite les equations d'equilibre de chaque noeud
c***********************************************************************

      ii=0
      if(nsign(nel).eq.1) goto 9
      ii=1
  9   nd=noeud(nel,1)
      na=noeud(nel,2)
      i1=(nc+nd-1)*4+1
      i2=(nc+na-1)*4+1
      j1=(nel-1)*8+1
      j2=j1+7
      ibb=nsol+1+(nel-1)*8

c
c rangement des coefficients relatifs aux equations d'equilibre et aux
c conditions d'appuis eventuels
c --------------------------------------------------------------------
      m=1
      n=1
      if(nno(nd,2).ne.0) m=nno(nd,2)
      if(nno(na,2).ne.0) n=nno(na,2)
      do 100 j=j1,j2
        k=20*(j-j1)
        do 101 i=1,4
          a(i1+i-1,j)=-aux(mdp(m,i)+k+ii)
          a(i2+i-1,j)= aux(mar(n,i)+k-ii)
  101   continue
  100 continue
c
      do 80 i=1,4
        do 90 is=1,nsol
          is1=(is-1)*16
          b(i1+i-1,is)=b(i1+i-1,is)-zb(mdp(m,i)+ii+is1)
          b(i2+i-1,is)=b(i2+i-1,is)+zb(mar(n,i)-ii+is1)
   90   continue
        if(nvsmb.eq.nsol) goto 80
        kk=0
        do 70 j=ibb,ibb+7
          kk=kk+16
          b(i1+i-1,j)=b(i1+i-1,j)-zb(mdp(m,i)+ii+kk)
          b(i2+i-1,j)=b(i2+i-1,j)+zb(mar(n,i)-ii+kk)
   70   continue
   80 continue
c
c rangement des coefficients relatifs aux equations de compatibilite
c ------------------------------------------------------------------
      if (nc.eq.0) goto 300

      do 200 j=1,nc
      m=(j-1)*4+1
      if (mcomp(j,1).eq.nel) goto 210
      if (mcomp(j,2).eq.nel) goto 220
      goto 200
  210 do 201 i=j1,j2
        k=20*(i-j1)
        a(m+0,i)=aux( 2+k-ii)
        a(m+1,i)=aux( 4+k-ii)
        a(m+2,i)=aux(14+k-ii)
        a(m+3,i)=aux(16+k-ii)
  201 continue
      do 91 is=1,nsol
        is1=(is-1)*16
        b(m+0,is)=b(m+0,is)+zb( 2-ii+is1)
        b(m+1,is)=b(m+1,is)+zb( 4-ii+is1)
        b(m+2,is)=b(m+2,is)+zb(14-ii+is1)
        b(m+3,is)=b(m+3,is)+zb(16-ii+is1)
   91 continue
      if(nvsmb.eq.nsol) goto 200
      kk=0
      do 81 i=ibb,ibb+7
        kk=kk+16
        b(m+0,i)=b(m+0,i)+zb( 2-ii+kk)
        b(m+1,i)=b(m+1,i)+zb( 4-ii+kk)
        b(m+2,i)=b(m+2,i)+zb(14-ii+kk)
   81   b(m+3,i)=b(m+3,i)+zb(16-ii+kk)
      goto  200

  220 do 202 i=j1,j2
        k=20*(i-j1)
        a(m  ,i)=-aux( 1+k+ii)
        a(m+1,i)=-aux( 3+k+ii)
        a(m+2,i)=-aux(13+k+ii)
        a(m+3,i)=-aux(15+k+ii)
  202 continue
      do 92 is=1,nsol
        is1=(is-1)*16
        b(m+0,is)=b(m+0,is)-zb( 1+ii+is1)
        b(m+1,is)=b(m+1,is)-zb( 3+ii+is1)
        b(m+2,is)=b(m+2,is)-zb(13+ii+is1)
        b(m+3,is)=b(m+3,is)-zb(15+ii+is1)
   92 continue
      if(nvsmb.eq.nsol) goto 200
      kk=0
      do 82 i=ibb,ibb+7
        kk=kk+16
        b(m  ,i)=b(m  ,i)-zb( 1+ii+kk)
        b(m+1,i)=b(m+1,i)-zb( 3+ii+kk)
        b(m+2,i)=b(m+2,i)-zb(13+ii+kk)
   82   b(m+3,i)=b(m+3,i)-zb(15+ii+kk)
  200 continue

  300 return
      end
