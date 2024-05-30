      program lbr5
      implicit double precision (a-h,o-z)
      integer*2 narg,larg(10)
      character texte*80,inputfile*80
      character buffer(10)*80
	character*16 code1						!sécurité
c

c valeurs par defaut ..........................................................
      ilangue=1        ! langue = francais
c
c lecture des arguments de la ligne de commandes ..............................
c      narg=iargc()
c	     narg=2
c      if(narg.eq.0.or.narg.gt.10)then
c      write(6,*)'lbr5 file.txt [-fr][-en]'
c       stop 1
c      endif
c      do i=1,narg
c       call getarg(i,buffer(i),larg(i))
c       if(i.gt.1.and.index(buffer(i),'-fr').eq.1)ilangue=1
c       if(i.gt.1.and.index(buffer(i),'-en').eq.1)ilangue=2
c      enddo

c	do i=1,narg
      open(33,file='boss.txt',status='old',err=906)
      open(6970,file='summary.dat',form='formatted')           ! fichier debug

	rewind 33
	read(33,*) buffer(1)
	inputfile=buffer(1)
	read(33,*) code1						!sécurité
	close(33)								!sécurité
c      inputfile=buffer(i)

c      inputfile='o32.txt'
c
c lecture des tailles dans le fichier d'entree ................................
      open(unit=55,file=inputfile,status='old',err=999)
      read(55,200)texte
	read(55,*)iana_a				!r&d14	!fev2007
      read(55,200)texte
      read(55,*)(j,l=1,7),neto_a
      close(55)

c
c allocation dynamique ........................................................
	call allocatevar(ilangue,inputfile,neto_a,iana_a)	!r&d14	!fev2007
c
c appel de la sr main et on chronometre .......................................
      et=timef()
      call main(code1)									!sécurité
      et1=timef()
      if(ilangue.eq.1)then
	   write(6,101)'temps de calcul   :',et1
	   write(6970,101)'temps de calcul   :',et1
	endif
      if(ilangue.eq.2)then
	   write(6,101)'elapsed time      :',et1
	   write(6970,101)'elapsed time      :',et1
	endif
c      enddo
	close (6970)

	stop 0
c
c formats .....................................................................
 100  format(1x,a)
 101  format(1x,a,f8.1,' sec')
 200  format(a)
c
c formats .....................................................................
 999  stop 1
 906  write(*,*) 'stop : the "boss.txt" file is missing'		!modif batch
      write(*,*) '     : (le fichier "boss.txt" manque).'		!modif batch
      write(6970,*) 'stop : the "boss.txt" file is missing'		!modif batch
      write(6970,*) '     : (le fichier "boss.txt" manque).'		!modif batch
      
      end
