c creation de l'entete dans un fichier resultats avec les coordonnees anast.
c
      subroutine entete(iunit)
      use sharedvar
      implicit double precision (a-h,o-z)
      character*80 texte
c
c en francais .................................................................
      if(langue.eq.1)write(iunit,1)
  1   format(///20x,'logiciel des bordages raidis (l.b.r.-5.6)'/20x,
     * 41(1h=)//' université de liege, 1 chemin des chevreuils, b52/3',
     * ' 4000 liege,belgique'/' contact:',
     * ' dr. ph. rigo, tel:+32-(0)43.66.93.66, fax:+32-(0)43.66.91.33'/
     * t30,'email: ph.rigo@ulg.ac.be'////)
c
c en anglais ..................................................................
      if(langue.eq.2)write(iunit,2)
  2   format(///20x,' l.b.r.-5.7  software'/20x,
     * 28(1h=)//' university of liege, 1 chemin des chevreuils, b52/3',
     * ' 4000 liege,belgium'/' contact:',
     * ' dr. ph. rigo, tel:+32-(0)43.66.93.66, fax:+32-(0)43.66.91.33'/
     * t30,'email: ph.rigo@ulg.ac.be'////)
      return
      end