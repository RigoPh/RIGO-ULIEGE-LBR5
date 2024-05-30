c
C    ******************************************************
C    ******************************************************
C	LE LOGICIEL DES BORDAGES RAIDIS (VERSION L.B.R.-5.7)
C	====================================================
C     (avec module d'optimalisation et Multi-structures)
C	    
C	    *********************************
C	    ******  Programme LBR-5.7  ******
C	    *********************************
C********************************************************************
C  .    DEPARTMENT OF NAVAL ARCHITECTURE, ANAST                     .
C  .    UNIVERSITY OF LIEGE                                         .
C  .    1 Chemin des Chevreuils (B52/3), 4000 Liege, BELGIUM        .
C  .    (Tel) +32-43.66.93.66   (Fax) +32-43.66.91.33                .
C  .    (e-mail) Ph.Rigo@ULg.ac.be                                  .
C	      
C	  Dr. Philippe RIGO, Maitre de Recherche, FNRS-NFSR
C     
C********************************************************************
C
C    Modifications			  Crée: Janv. 1994
C    -------------			  -----
C   - 21-3-96 : Equivalence(.., windowkind) et (.. , contrgn)
C   -  8-5-96 : Limite élastique, Sig max et poids spécifique
C   -  4-6-96 : Restrictions géométriques (subr. GEOM)
C   - 22-7-96 : Mise de la Subr. CONTR dans BO2
C   - 18-2-97 : Ult. strength of Hull girder (Subr. USHULL dans BO2)
C   -  3-3-97 : Modifications des données 
C                - E,eta,Sigy, etc.    à introduire par panneau
C                - Introduire le nbre de nervure (Naig et Nraid) et
C                  l'entredistance réelle (Aaig et Araid) au lieu de EPSA
C                  et EPSR
C                - MODE = Type de répartition des Raidisseurs longitudinaux
C   - 25-3-97 : Subr. Copy (to save updated DATA)
C   - 16-3-98 : Excécution en série de plusieurs données.
C   - 15-5-98 : Fonction Objectif COUT
C   - 29-3-99 : Passage à 100 panneaux (au lieu de 30),
c               Modifications des COMMOM (BLOKA,DI et ALLONG),
c               Nbre de termes de la série de Fourier illimité,
c   - 01-11-00: Choix de la Langue (1 Francais, 2 English)
c   - 06-11-00: Choix du repere utilisateur dans Subr COORD
c   - 20-11-00: Impression du class. des valeurs extremes dans un fichier separe.
c   - 15-02-01: Boucle sur les termes de Fourier avec Optimisation
c               Dessins des NSOL cas de charge
c   - 20-11-01: Epaisseur de corrosion
c   - 10-01-02: Modification des parametres de la fonction cout
c   - 17-01-02: Paramètre PLOC (pression locale)
c   - 30-01-02: Cas de charge :NSOL(max),NSOL,(NNSOL(i),i=1,NSOL)
c   - 04-02-02: Struct Constraints defined per load case
c   - 07-03-02: Efforts de buscage Nb et Mb (defini via File BUSC.txt et BUSM.txt)
c   - 16-05-02: Introduction du concept d'analyse multi-structures (Version LBR5.7)

C    Dernière modification: 4-02-2004
c    ----------------------
C********************************************************************
C  I.  DONNEES GENERALES: définition de la structure et des sous-structures
C  =======================================================================
c
C              DONNEES A INTRODUIRE  (FORMAT LIBRE)                     
C             ++++++++++++++++++++++     
C
C   -TITRE SUR 60 CARACTERES MAX                                  
C   -NP		Nbre de structures (principale + sous-structures)
c   -NOM(i),i=1,NP  (1 ligne par nom)
c             Noms des fichiers contenant les données LBR5 de chaque structure 
c             (ou sous-structures)
c   - Liste des variables de conception communes.
C
C
C********************************************************************
C  II.  DONNEES RELATIVES A LA STRUCTURE PRINCIPALE OU UNE SOUS-STRUCTURE
C  =======================================================================
c
C              DONNEES A INTRODUIRE  (FORMAT LIBRE)                     
C             ++++++++++++++++++++++     
C
C	LES UNITES : LONGUEURS (Mètre), PRESSIONS (Mètre d'eau)
C     -----------   et FORCES/POIDS/CONTRAINTES (Newton)
C
C   MAIN: -TITRE SUR 60 CARACTERES MAX                                  
C         -IMPR,IMPR2,INDAIG,INDRAID,DESSIN,JLPH,IBUSC,NETO    (INTEGER) 
C         -IOPTI,ITERAM                                         (INTEGER)
C         -ICOUT  (0 = Fct Obj. Poids ; 1 = Fct Obj. Coût)      (INTEGER) 
C             - REND, EQP                                       (REAL)                  
C             - Eo, Eox, Eoy  (épaisseur de référence en m)     (REAL)                  
C             - Prix Matér.: C1,C2,C3(tole,Euro/kg),
c                            DC1(tole,variation par mm)         (REAL)
c             - Extra weight:DW2(long,variation sur C2),
c                            DW3(trans,variation sur C3)        (REAL)
C             - MdO Bordé  : P10(h-h/m),PC10(variation par mm)  (REAL)
C             - MdO Assembl: P4x(Long,h-h/m),P5y(trans,h-h/m), 
C                            DP4x(long),DP5y(trans)             (REAL)
C             - MdO Membr. : P9x(long,h-h/m),P9y(trans,h-h/m),
C                            DP9x(long),DP9y(trans)             (REAL)
C             - MdO JOINTS : P6(intersect,h-h/joint),
C                            P7(gousset,  h-h/joint),
c                            Béta  R(long.), Béta  T(transv)    (REAL)
C             - Prix Soud. : C8(F/m) , DC8(variation par mm),
c                            Alpha-X(long.),Alpha-Y(transv)     (2REAL,2INT)    
c
C         -WIDTH,													(REAL)                             
C         -DIS(5),												(REAL)                             
C         -FAM(6)													(REAL)                             
C         -IPOIDS  ( 0 ou 1 , Poids Propre)						(INTEGER)                             
C         -NSOL(max),NSOL,(NNSOL(i),i=1,NSOL) (Nbr cas de charge)	(INTEGER)                                                          
C                                                                       
C   ENT : Données à rentrer pour chaque panneau
C         --------------------------------------
C         -TYPE                                         --I         
C         -SI ON A UN PANNEAU COQUE, TYPE = COQUE         I             
C              -PHIL(degré),Q(m)          (REAL)          I             
C          SI ON A UN PANNEAU PLAQUE,TYPE = PLAQUE        I             
C              -HIGHT(m)                  (REAL)          I             
C		-SI ON A UN PANNEAU EPONTILLE, TYPE = EPONTILLE I  
C              -HIGHT(m)                  (REAL)          I             
C         -DELTA,(EPAIS pour épontille)       (REAL)      I
C         -EPSA,EPSR et  EPSA2,EPSR2      (REAL)          I             
C          HYA,DYA,WYA,TYA,               (REAL)          I             
C          HXR,DXR,WXR,TXR,               (REAL)          I A REPETER 
C           si EPSA2_0 ou EPSR2_0                         I
C               - HYA2,DYA2,WYA2,TYA2,KSA2                I
C               - HXT2,DXR2,WXR2,TXR2,KSR2                I
C           sinon : rien ne doit être ajouté              I
C         -MODE (Mode de répartition raid. long.:EC1,...) I
C          ex: EE1: Type deck complet avec "D=Spacing" différent de EPSR (Ship)I
C              EE2: Type deck complet avec D = EPSR       I
C              ES1: Type 1/2 deck , 1 raid au centre      I
C              ES3: Type 1/2 deck , 0 raid au centre      I
C              ES3: Type 1/2 deck , 1 TRAV au centre      I
C              EC1: Panneau continu d'un côté             I
C              CC1: Panneau continu des 2 côtés           I
C              ..., voir liste complète dans mode d'emploiI
C         -E(N/m2),ETA,SIGY,SIGM,SPEC     (REAL)          I                             
C         -MT,KSR,KSA,KST,KSE,IPA,IVARIA  (INTEGER)       I POUR      
C         -ANGLE(degré)                                   I
C         - Pour chaque cas de charge (i=1,NSOL)          I
C             - XI,XF (m)  cas i          (REAL)          I CHAQUE    
C         -PLOC (press. locale max en m)  (REAL)          I PANNEAU 
C         -IBAT (0 OU 1)                  (INTEGER)       I  
C          SI IBAT=1                                      I
c             Pour chaque cas de charge (i=1,NSOL)        I
C               -Une ligne titre                          I             
C               -IPTS (Nbre d'intervalle) (INTEGER)       I             
C               -CHA(I) I=1,3             (REAL)          I
C                c-à-d Pgravite (N/m),                    I
C                      Ppression en Y=0  (mètre           I
C                      Ppression en Y =Yo   d'eau)        I
C                (A REPETER POUR CHAQUE INTERVALLE)       I             
C         -NOH(10)                        (INTEGER)       I             
C         -HXTR,DXTR,WXTR,TXTR,ABTR       (REAL)          I             
C          (A REPETER POUR CHAQUE TRAVERSE)               I             
C                                                         I
C         - Titre : Variables de Conception (Character)   I
C         - NBRXI (nbre de variables)       (INTEGER)     I
C           si NBRXI >0 (sinon passer à M)                I
C             - NXI(i), i=1,NBRXI             (INTEGER)   I
C               Bordé: 1  Epaisseur du bordage
C               Cadre: 2   Hauteur de l'âme des tranversaux (Cadre)
C                      3   Epaisseur de l'âme des tranversaux (Cadre)
C                      4   Largeur de la semelle des tranversaux (Cadre)
C                      5   Entredistance entre tranversaux (Cadre)
C     		  Raid.: 6   Hauteur de l'âme des raidisseurs  
C                      7   Epaisseur de l'âme des raidisseurs
C                      8   Largeur de la semelle des raidisseurs
C                      9   Entredistance entre raidisseurs
c		Epontille:	 1	 Demi-hauteur d'âme, diamètre ou côté extérieur	
c					 2	 Epaisseur d'âme	
c					 3	 Largeur de semelle	
c					 4	 Epaisseur de la paroi mince	
c					 5	 Entredistance entre épontilles	
C           - XIMIN(i),i=1,NBRXI; limites inf. des N var. de concept.                                            
C           - XIMAX(i),i=1,NBRXI; limites sup. des N var. de concept.                                            
C                                                           
C         - Titre : RESTRICTIONS STRUCTURELLES        (Character)   
C         - IM1  (0 without structural constraints, 1 with structural constraints)
c         * Si IM1  =1  (otherwise skip and continue with the Geometrical Constraints)
C           - IPT   Nbre de pts pour le calcul des sensibilités (INTEGER)                                    
C           - YPT(i),i=1,IPT Coord. relatives de ces IPT points (0 à 1) (REAL)                                    
C           - Pour chaque restriction (j=1,M);  CJ < CJMAX 
C             - Titre : RESTRICTIONS STRUCTURELLES        (Character)   
C             - M1  NBRE DE RESTRICTIONS STRUCTURELLES (max 20) (INTEGER)                                          
C             * Si M1 > 0 (sinon passer aux restr. géom, M2)   
C               - Pour chaque restriction (j=1,M);  CJ < CJMAX 
C                 J, ICT CJMAX, INV et IY
C                 avec:
C               ° ICT  nø de référence de la restrictions (INTEGER)
C                  Deformation d'ensemble + Epaiss mini:
C                  nø 1     V déplacement (en m)
C                  nø 2     U déplacement (en m)
C                  nø 3     W déplacement (en m)
C                  nø 4     Epaisseur min. bordé: "d(Hughes) - d < 0"
C                            (Cjmax=0) (Sigma comparaison < SIGM )
C                  nø 5     W(relatif) par rapport au bord de départ (Y=0)
C                  nø 6     W(relatif) par rapport au bord d'arrivée (Y=Yo)
C                  nø 7     W(relatif) par rapport au dépl. moyen des extrémités
C                  Contrainte - flexion d'ensemble - bordé , raid et cadres + Resist Ult Paik
C                  nø 11    Sigma comp.(Sx,Sy)  bordage (Z=0),X=L/2
C                  nø 12    Sigma comp.(Txy)    bordage (Z=0),X=0
C                  nø 13    Sigma comp.(Sx,Sy)  bordage (Z=+d/2)
C                  nø 14    Sigma comp.(Sx,Sy)  bordage (Z=-d/2)
c                  nø 15    Sx/S(Ult. Strength) < Cjmax (X=L/2)
C                  nø 21    Sigma comp.(Tâme)   Cadre    JAS/JAB   en X=0
C                  nø 22    Sigma comp.(Sy,Txy) Cadre    JAS       en X=L/2
C                  nø 23    idem cond. nø 21 car Tau(JAS)=Tau(JAB)
C                  nø 24    Sigma comp.(Sy,Txy) Cadre      JAB     en X=L/2
C                  nø 25    Sigma comp.(Sy,Txy) Cadre      Semelle en X=L/2
C                  nø 31    Sigma comp.(Tâme)   raidisseur JAS     en X=0
C                  nø 32    Sigma comp.(Tâme)   raidisseur JAB     en X=0
C                  nø 33    Sigma comp.(Sx)     raidisseur Semelle en X=L/2
C                  nø 41    Sigma comp.(Tâme)   traverses  JAS     en X=0
C                  nø 42    Sigma comp.(Tâme)   traverses  JAB     en X=0
C                  nø 43    Sigma comp.(Sx)     traverses  Semelle en X=L/2
C                  Flexion locale du raidisseur:
C	             nø 51	 Flèche relative Raid: Extr. Encastrées : F/L=1*Pl^3/384EI < ratio (1/200, ...
C	             nø 52	 Flèche relative Raid: Extr. Libres     : F/L=5*Pl^3/384EI < ratio (1/200, ..
C	             nø 54	 Contrainte Max semelle:  (M=Pl^2/10) ; Sig < S(adm)
C	             nø 55	 Contrainte Max borde:    (M=Pl^2/10) ; Sig < S(adm)
C	             nø 56	 Contrainte Max cisaillement ame : 1.73*Tau < T(adm) ;(Tau a l'axe neutre et M=0)
C	             nø 57	 Flèche Abs. maille: Bords Encastrés : W=1*PL^4/384EI < Wmax (m)
C	             nø 58	 Flèche Abs. maille: Bords articulés : W=5*PL^4/384EI < Wmax (m)
C               ° CJMAX(j)    borne sup. de la restriction(j)  (REAL)
C               ° INV   (1 si borne sup; -1 si borne min)      (INTEGER)
C               ° IY  nø du pt YPT choisi (IY=1 à IPT)         (INTEGER)
c
C         - Titre : RESTRICTIONS GEOMETRIQUES        (Character)   
C         - M2  NBRE DE RESTRICTIONS GEOMETRIQUES (INTEGER)                                               
C           * Si M2 = 0 (passé à la suite sans entrer IBM2 et ISEMA & ISEMR ) !!!!
C           * Si M2 = 99                                     
C         - IBM2 le nø du set de restrictions géométriques
c             nø1  set de Hughes (profils T): 109 110 111 112 + 209 210 211 212 + 301 302 303
c             nø2  set de Hughes + SET A    : 104 105         + 204 205 
c             nø3  set de Hughes (cornières): 115 116 111 112 + 215 216 211 212 + 301 302 303
c             nø4  set de Rahman/Lloyds     : 101 102 103     + 201 202 203
c             nø5  set de Rahman + SET A    : 104 105         + 204 205 
C         - ISEMA, ISEMR (= 0 ou 1)
c              = 0 (l'épaisseur des semelles est fixe)
c              = 1 (l'épaisseur des semelles est modifiée et vérifie Df
C                    < 16*Tf ): CORNIERES
c              = 2 (l'épaisseur des semelles est modifiée et vérifie Df
C                    < 32*Tf ): Profil T
c                Les restrictions liées aux semelles sont:   
c      		     -  Tf-2*Tw   <0						   
c      		     -  8*Tf-Df   <0						   
c      		     -  Df-16*Tf  <0 ou  Df-32*Tf <0           
C           * Si M2>0 et différent de 0 et de 99 : ex M2=4)  
C              - LM2(I=1,M2) les nø de code des restrictions géométriques
c                RESTRICTIONS GEOMETRIQUES DES TRANSVERSAUX (cadres-aiguilles) 
c                -------------------------------------------------------------- 
C                SET de RAHMAN-Caldwell
c      		 nø 101 :  Df-Dw     <0         (Df = haut. ame)
c      		 nø 102 :  Dw-2*Df   <0         (Dw = larg. semelle)
c      		 nø 103 :  0.0065+Dw/170-Tw<0   (Tw = épaisseur ame)
C                SET A:
c      		 nø 104 :  Delta-2*Tw<0         (Delta = épaiss. bordé)
c      		 nø 105 :  3*Delta-Dw<0
C                SET de Hughes
c       		 nø 109 :  0.625Df-Dw<0         
c       		 nø 110 :  Dw-2.5*Df <0         
c      		 nø 111 :  Dw-120*Tw <0       
c      		 nø 112 :  Tw-2*Delta<0   (complétaire à C104)
C                Pour Situation Inverse (si Raid/Girder >> Transv. frame):
c      		 nø 113 :  Dw-36*Tw  <0   (complément set Hughes = C211)
c      		 nø 114 :  Dw-40*Tw  <0   (complément set Rahman = C203)  
c                Si profil en cornières (à la place de C 109 et C110)
c       		 nø 115 :  1.25*Df-Dw<0         
c       		 nø 116 :  Dw-5*Df   <0         
c										 
c     	     RESTRICTIONS GEOMETRIQUES DES RAIDISSEURS  (longitudinaux)
c    		     ----------------------------------------------------------- 
C                SET de RAHMAN-Caldwell
c      		 nø 201 :  Df-Dw     <0
c      		 nø 202 :  Dw-2*Df   <0
c      		 nø 203 :  Dw-40*Tw  <0
C                SET A:
c      		 nø 204 :  Delta-2*Tw<0
c      		 nø 205 :  3*Delta-Dw<0
c                SET de Hughes
c              nø 209 :  0.625Df-Dw<0
c              nø 210 :  Dw-2.5*Df <0
c              nø 211 :  Dw-36*Tw  <0
c              nø 212 :  Tw-2*Delta<0        (complétaire à C204)
c                Pour Situation Inverse      (si Raid/Girder >> Transv. frame):
c              nø 213 :  Dw-120*Tw <0        (complément set Hughes = C111
c              nø 214 :  0.0065+Dw/170-Tw<0  (complément set Rahman = C103
c                Si profil en cornières      (à la place de C 209 et C210)
c              nø 215 :  1.25*Df-Dw<0
c              nø 216 :  Dw-5*Df   <0
c										 
c     	     RESTRICTIONS CROISEES (Transversaux - raidisseurs)
c    		     -------------------------------------------------- 
C                SET de Hughes
c      		 nø 301 :  Dw(raid)-Dw(aig)   <0
c      		 nø 302 :  Tw(raid)-4*Tw(aig) <0
c      		 nø 303 :  Tw(aig) -4*Tw(raid)<0
C                Pour Situation Inverse (si Raid/Girder >> Transv. frame):
c      		 nø 304 :  Dw(aig)-Dw(raid)   <0  
c
C              - ISEMA, ISEMR (restrictions sur l'épais. des semelles)
c                 (si M2=0, ISME et ISEMR ne doivent pas être donnés)
C      						______________                                                           
c
C  DONNEES RELATIVES AUX CONDITIONS DE BORD (Y=0 et Y=L):  
C  =====================================================
C  MODIF: - TITRE SUR LES CONDITIONS AUX EXTREMITES DES PANNEAUX (en Y=0 et Y=H)
c         - NCONDI                                (INTEGER)
C         - NNO(I,1),NNO(I,2) POUR I=1,NCONDI     (INTEGER) 
c             SOIT: 1 EXTREMITE LIBRE  MY=NY=NXY=RY=0 
c                   2 APPUI SIMPLE     W=0     ET MY=NY=NXY=0
c                   3 APPUI SIMPLE     W=V=0   ET MY=NXY=0  
c                   4 APPUI SIMPLE     W=U=0   ET MY=NY=0 (TYPE FOURIER)
c                   5 APPUI SIMPLE     W=U=V=0 ET MY=0
c                   6 ENCASTREMENT     W=U=V=DW/DY=0
c                   7 AXE DE SYMETRIE  V=DW/DY=0 et NXY=RY=0 (symétrie classique)
c
c                   8 APPUI SIMPLE     V=0   ET MY=NXY=RY=0
c                   9 APPUI SIMPLE     U=0   ET MY=NY=RY=0
c                  10 APPUI SIMPLE     U=V=0 ET MY=RY=0
c                  11 AXE DE SYMETRIE  W=DW/DY=0 et NXY=NY=0 (dans l'axe du panneau)
c                  12 DOUBLE SYMETRIE  V=W=DW/DY=0 et NXY=0 
C      						______________                                                           
C                                                                      
C  COORD. DU REPERE UTILISATEUR (KXY) et RESTRICTIONS SUR LE CENTRE DE GRAVITE  
C  ======================================================================
C  COORD: - TITRE RELATIF AU REPERE (KXY0 ET A LA RESTRICTION SUR KG (posit. centre de grav.)
C         - XK et YK  (Coord. du repere utilisateur par rapport ext. du panneau 1)(2 REAL)
C         - IGRAV,  KGMIN, KGMAX         (IGRAV =0, 1, 2 or 3)       (1 INTEGER et 2 REAL)
c                   KGMIN et KGMAX = coord. verticales limites dans le repere utilisateur KXY
C      						______________                                                           
c                                                                       
C  MOMENT DE FLEXION D'ENSEMBLE (sollicitations):  
C  =============================
C  BATEAU:-TITRE SUR LES MOMENTS D'EXTREMITES DE LA PARTIE CYLINDRIQUE DU BATEAU
C         -IMOM (0 OU 1)                                                
C          SI IMOM=1 
C          - YRED (coef. de réduction:1.24 pour SIG et 1.04 pour W si 1 seul terme (sinon prendre 1.0)
C          - MOMENTS D'EXTREMITES
C            BM1(IS) ET BM2(IS) ; Pour chaque cas de charge (IS=1,NSOL)
C               NB: M>0 en Hogging c.à.d. le pont est tendu et le fond comprimé
C                                                                       
C                                                                       
C  EFFETS DE BORD (X=0 et X=L) :!!! pas valable avec LBR5  
C  =============================
C   CO2 : (!!! UNIQUEMENT SI JLBORD EST DIFFERENT DE 0)                     
C         -TITRE SUR LES COND. AUX APPUIS (60 CARACT)                   
C         -ITEMAX         (INTEGER)                                     
C         -POUR CHAQUE PANNEAU                                          
C            - I (NUMERO PANNEAU),NCOND (NBRE DE COND SUR CE BORD)      
C            - LES NUMEROS DES PTS OU L'ON IMPOSSE LES U ET DW/DX       
C              * LES NUMEROS 1 A 7 CORRESPONDENT A DES DEPLACEMENTS U   
C                RESPECTIVEMENT EN (0,0.05,0.333,0.5,0,666,0.95,1)      
C              * LES NUMEROS 8 A 12 CORRESPONDENT AUX ROTATIONS DW/DX   
C                RESPECTIVEMENT EN (0,0.333,0.5,0,666,1)                
C             ATTENTION: IL FAUT AU TOTAL UN NOMBRE DE CONDITION= 8*NETO 
C             ---------                                                 
C         -ROTATY,ROTATX,PRECIS  (REAL)                                 
C                                                                       
C                                                                       
C  RESTRICTIONS D'EGALITE :  
C  =======================
C         -TITRE SUR LES RESTRICTIONS D'EGALITE                                        
C         -NEGALT  (nbre de restrictions d'égalité ; MAXIMUM = 100)
C               Pour chaque restrictions (par ordre croissant de panneau
C               et de variable)
C         - XI1,NEL1(var. dépend.)   et XI2,NEL2(var. ind.)  ,  EGA
C                    (quotien=XI2/XI1)
C                ex: pour Haig, XI1 (ou XI2) =2
C                         EPSR, XI1 (ou XI2) =9
C
C                                                                       
C  ULTIMATE STRENGTH OF SHIP GIRDER :  
C  ==================================
C         -TITRE SUR LA RESISTANCE ULTIME DE LA POUTRE NAVIRE                                        
C         -IRESTR= restriction du module optimisation (0 = NON;1 = OUI)	
C           IF IRESTR=1 (sinon passé)
C            - UHOGM (>0) et USAGM (<0)  en N.m
C         -IULT  = method proposed	
C     	     = 0 : pas d'analyse de la resistance ultime
C     	     = 1 : PAIK  method (based on the Caldwell approach)
C      	     = 2 : PROCOL (based on the Smith's Progressive Collapse Analysis)
C              = 3 :
C          IF IULT=1 (PAIK APPROACH)
C            - DEPTH OF THE VESSEL, DEPTH OF DOUBLE-BOTTOM (m.)
C            - Les nø des panneaux composants le deck, fond, side, ...
C              - ND, (ID(i), i=1,ND)    Deck (full)
C              - NB1,(IB1(i),i=1,NB1)   Inner Bottom (full)
C              - NB, (IB(i), i=1,NB)    Outer bottom (full)
C              - NS, (ISd(i),i=1,NS)    Both Side Plates and long. bulkheads
C            - Le nø du panneau destiné au calcul de Sult du deck, fond, side, ...
C                          + Yield stress of the associated stiffeners
C              - KD   ,SyStiff(Pont)         : Deck
c              - KB1  ,SyStiff(Double fond)  : Inner bottom
c              - KB   ,SyStiff(Fond)         : Bottom
c              - KSup ,SyStiff(Side up)      : Side up
c              - KSdw ,SyStiff(Side down)    : Side down
C                                                                          
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                                                                       
      BLOCK DATA                                                        

      COMMON/DEDE/MDP,MAR
      INTEGER *4 MDP(15,4),MAR(15,4)
      DATA MDP/5, 1, 1, 1,1, 1, 9 , 5, 3, 3, 1, 1, 0, 0, 0,
     *         7, 5, 5, 3,3, 3, 11, 9, 5, 5, 7, 9, 0, 0, 0,
     *         9, 7, 9, 5,5, 13,13,11, 7,11, 9,13, 0, 0, 0,
     *         11,9, 13,7,13,15,15,13,11,13,15,15, 0, 0, 0/,
     *     MAR/6, 2, 2, 2,2, 2, 10, 6, 4, 4, 2, 2, 0, 0, 0,
     *         8, 6, 6, 4,4, 4, 12,10, 6, 6, 8,10, 0, 0, 0,
     *         10,8, 10,6,6, 14,14,12, 8,12,10,14, 0, 0, 0,
     *         12,10,14,8,14,16,16,14,12,14,16,16, 0, 0, 0/

      COMMON/LANGUE/ LANGUE    ! 1 French (par defaut), 2 English 
      INTEGER *4 LANGUE
      DATA LANGUE/1/  ! FRENCH
c     DATA LANGUE/2/  ! ENGLISH

      END
      
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC                                                                       
                                                                       
      SUBROUTINE MAIN
C     *******************
      IMPLICIT REAL *8(A-H,O-Z)
      
C     CHARACTER *1 : 12*MM+2*N
C      - AT(12*MM+3*N)   !(12*7000+3*150)
	CHARACTER*1 CAR(84450)  ! for MM=7000 and N=150		!extension neto

C     INTEGER *2 : 16*N
C      - KLI(16*N)   !(16*150)
	INTEGER*2 I2(2400)			!extension neto

C     INTEGER *4 : 
c	Jtot=254*N   + M2 + N*IS   + 2*IPT*N + 4*NG  + 4				!février 2004
c	Jtot=254*150 + 20 +150*10  + 2*4*150 + 4*1350 + 4  ! = 46224	!février 2004
 	DIMENSION I4(46224)		!extension neto							!février 2004

C     ESPACE de TRAVAIL Complémentaire
      DIMENSION TRAV(70000),ITRAV(1500)  !fev04	!extension neto	 !r&d13

c ---------------------------------------------------------------------------

      COMMON/DIM1/N,NE,NV,
     *            M1,M1T,M2,M2T,MM,NG,IS,IPT   ! dimension max des vecteurs

C ---------------------------------------------------------------------------
C     REAL*8 AA(Ktot)
C       KK1= max(1.844.160 ,  438.172) = 1.844.160    !avril2003
C       KK2= max(10.368.000 ,8.745.800) = 10.368.000
C       Ktot= KK1+KK2+ 1.859.522 = 14.071.682  
	REAL*8,ALLOCATABLE::AA(:)
	ALLOCATE(AA(22500000))  ! N=150 et IS=10  et IPT= 4		!extension neto	 !restri poids  !restri cout  !r&d13_tranchant !r&d14
C ---------------------------------------------------------------------------
C    LES VARIABLES
C    *************
C    N, NE, NV, M1, M2, M1T, M2T,  MM,  NG,  IS,NSOLM, IPT
C   100,.., 800,.., 20,1500,1200, 5000, 800,  5,        10 (Version LBR-5.3)
C   100,.., 800,.., 20,1500,1200, 7000, 800, 10,         4 (le 15-2-2001 - LBR5.5
C   100,.., 800,20, 20,....,...., 7000, 800, 10,  20 ,   4 (le  1-2-2002 - LBR5.6
c     avec
c	N   = Nbr max de panneau
C	NE  = N*8
c     NV  = Le nbre  de variables de conception (max = 9*N)
C	M1  = Nbre max  rest. struct. par panneau (pour 1 cas de charge),
c     M1T = Nbre max  rest. struct. pour la struct. (pour 1 cas de charge),
c	M2  = Nbre max  rest. géom. par panneau,
c     M2T = Nbre max  rest. géom. pour la struct.
c     MM  = Somme(M1T)+M2T < 7000
c	NG  = Nbre max  rest. égalité (pour la struct.)
c     IS  = Nombre max de cas de charges (max=10)
                  !IS est limité à 10 à cause de SENSH(13,4,9,NSOL=10) dans common OPTI2
				! et de DZSN (8*NETO,9*NETO,NSOL)
c     NS0LM=Nombre max de cas de charges figurant dans le jeu de données (max=20).
c     IPT = Nombre max de pts de calcul des sensibilités par panneau (max = 4)
	      !Attention: IPT ne peux pas être modifié !!!
C    ========================================
C   POUR 100 PANNEAUX
C   ------------------
	N   = 150   !Nbr max de panneau		!extension neto
	NE  = N*8
      NV  = 1350  !Le nbre  de variables de conception (max = 9*N)	!extension neto
	M1  =  20   !Nbre max  rest. struct. par panneau (pour 1 cas de charge)
      M1T =20*150 !Nbre max  rest. struct. pour la struct. (pour un cas de charge)	!extension neto
	M2  =  20   !Nbre max  rest. géom. par panneau,
      M2T =20*150 !Nbre max  rest. géom. pour la struct.		!extension neto
      MM  =7000   !MM=Somme(M1T)+M2T  < 7000
	NG  = 1350  !Nbre max  rest. égalité (pour la struct.)	!extension neto
      IS  =  10   !Nombre max de cas de charges retenus pour l'analyse (max=10).
                  !IS est limité à 10 à cause de SENSH(13,4,9,NSOL=10) dans common OPTI2
				! et de DZSN (8*NETO,9*NETO,NSOL)
      NS0LM= 20   !Nombre max de cas de charges figurant dans le jeu de données (max=20).
      IPT =   4   !Nombre max de pts de calcul des sensibilités par panneau
	            !Attention: IPT ne peux pas être modifié !!! (il faut changr ds les subr.)
C ------------------------------------------------------------------------

C TAILLE DE "AA" (y compris DI et AL): REAL *8
C --------------------------------------------
c  BL(128*N*N)+ DI(max (?,?)) + AL(max (?,?)) + C(6*N+2) + ANN(2*N) 
c             + COUT(6*N)     + P(IPT*N)      + R(80*N+3*NV+NG)

C  Taille de DI
C    - dans BO1 :  128*N*N+8*N 
C       =   115.440 pour N= 30
C       =   320.400 pour N= 50
C       = 1.288.000 pour N=100 IPt=10 et NSOL=5
C    - dans BO2
C       = 21.048 + 1710*IS + 414*IPT*N  +110*N + IPT        !avril2003
c       = 214.752  pour NETO = 100 et NSOL = 10 et IPT=4    !avril2003


c  IL FAUT QUE LA DIMENSION DE "AA" soit supérieur à Ktot
      K1=1					 !
	K2=K1+ 128*N*N			 ! BL(1280.000) si N=100

	       KK1= MAX0(128*N*N+8*N, 21048+1710*N+414*IPT*N+110*N+IPT)   !pour DI  !avril2003
      K3=K2+ KK1			 	 ! DI = MAX(?,?)

	       KK2= MAX0(72*N*N*IS,   6*NV+2*MM+NV*NV+NV*MM)     !pour AL  !multi obj
      K4=K3+ KK2		         ! AL = MAX0(?,?)  

      K5=K4+ 7*N+2             ! C(    702)  
      K6=K5+ 2*N				 ! ANN(  200)
      K7=K6+ (7+3+9+9+3+3+3*N+3*N+18+18+9+1+1+72)*N+4	 ! CT(   700)		!coefficient de participation  + !corrosion (+3) + !restri poids + !restri cout  + !r&d13_tranchant + !r&d14        
      K8=K7+ IPT*N             ! P(    400)
c     K9=K8+ 60*N+3*NV+M1T+NG	 ! R(  10700)
      K9=K8+ 80*N+3*NV    +NG	 ! R(  11200)

	Ktot=KK1+KK2+128*N*N+(242+2*3*N+IPT)*N+3*NV+NG+6  != KK1+KK2+ 3.057.306		!coefficient de participation + !corrosion (+3) +!restri poids + !restri cout + !r&d13_tranchant + !r&d14

c	KK1= MAX0(128*N*N+8*N,  21048+1710*N+414*IPT*N+110*N+IPT ) !pour  DI 
c	KK2= MAX0(72*N*N*IS,    6*NV+2*MM+NV*NV+NV*MM            ) !pour  AL

C  si N=150, NV=1350, MM=7000, IS=10, IPT=4, NG=1350,M1=20, M2=20, M1T=3000	!extension neto
C  ---------------------------------------------------------------
C     KK1= max(2.881.200 ,  542.452) = 2.881.200     !avril2003
C     KK2= max(16.200.000 ,11.294.600) = 16.200.000
C     Ktot= KK1+KK2+ 3.057.306 = 22.138.506
C --------------------------------------------------------------

C TAILLE DE "I4": INTEGER*4
C --------------
c  IBL(132*N) + IAL(M2+7*N) + NN(21*N+3)+ IP(N*(63+2*IPT+IS)) + MR(31*N +4*NG+1) 
c  = 254*N + M2 + N*IS + 2*IPT*N + 4*NG + 4  
c  =36984 pour N=120, M1=20, M2=20, IPT= 4, NG=1080

c  IL FAUT QUE LA DIMENSION DE "I4" soit supérieur à Jtot
      J1=1                       !
	J2=J1+ 132*N                ! IBL(13200)					!février 2004  
      J3=J2+ M2+7*N              ! IAL( 720)     
      J4=J3+ 21*N+3              ! NN( 2103)						!février 2004
      J5=J4+ N*(63+2*IPT+IS)     ! IP( 8100) 
	J6=J5+ 31*N +4*NG +1       ! MR( 6301)
	Jtot=254*N   + M2 + N*IS   + 2*IPT*N + 4*NG  + 4			!février 2004
c	Jtot=254*150 + 20 +150*10  + 2*4*150 + 4*1350 + 4  ! = 46224	!février 2004

C --------------------------------------------------------------

      CALL MAIN1(CAR(1),
     *           I2(1),
     *           I4(J1),I4(J2),I4(J3),I4(J4),I4(J5),
     *          AA(K1),AA(K2),AA(K3),AA(K4),AA(K5),AA(K6),AA(K7),AA(K8),
     *          TRAV,ITRAV)

	RETURN
	END
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC                                                                       
                                                                       
      SUBROUTINE MAIN1(AT,
     *                 KLI,
     *                 IBL,IAL,NN,IP,MR,
     *                 BL,DI,AL,C,ANN,CT,P,R,
     *                 TRAV,ITRAV)

      IMPLICIT REAL *8(A-H,O-Z)

      COMMON/DIM1/N,NE,NV,
     *            M1,M1T,M2,M2T,MM,NG,IS,IPT ! dimension max des vecteurs

      COMMON/LANGUE/ LANGUE        ! 1 French (par defaut), 2 English 
	 ! voir selection dans le block data ci-avant
     
! Vecteur  KLI(16*N)
      INTEGER*2  KLI(1)

! Pour le Common BLOKA  :  BL(128*N*N),IBL(48*N)
      DIMENSION BL(1),IBL(1)   
	   
! Pour le Common DI     :  DI() Max de 128*N*N+8*N et de 28507 + 1458*N
      DIMENSION DI(1) 
   !Taille A et B dans BO1 :  128*N*N+8*N   
   !  NB: A est aussi utilise comme espace de travail dans COORD.for (taille=32N)
   !Taille dans BO2   (vect de travail POUR SENS2,SENS3,SENS4 dans BO2)
 	!   =  21.048 + 1710 *NSOL + 414*IPTmax*NETO +110*NETO + IPTMAX    !avril2003
	!   =  21.048 + 17.100     + 165.600         +11.000   + 4         !avril2003
	!   = 214.752  pour NETO = 100 et NSOL = 10 et IPT=4               !avril2003

! Pour COMMON ALLONG    : AL(72*N*N*IS), IAL(M2+7*N), AT(12*MM+3*N)
	CHARACTER*1 AT
      DIMENSION AL(1),IAL(1),AT(1)     

! Pour COMMON COORD     : C(7*N+2)
      DIMENSION C(1)  

! Pour COMMON ANG       : ANN(2*N), NN(21*N+3)						!février 2004
      DIMENSION ANN(1),NN(1)

! Pour COMMON MAT       :  CT(6*N)
      DIMENSION CT(1)
                   
! Pour COMMON OPTI      : P(IPT*N) ,IP=N*(63+2*IPT+IS))
      COMMON /OPTI/   IOPTI,NTOT,M1TOT,M2TOT
      DIMENSION P(1),IP(1)

! Pour COMMON OPTI5/OPTI4: R =80*N+3*NV+NG   et MR=31*N+4*NG+1 
      DIMENSION R(1),MR(1)

! Pour COMMON OPTI2 : 
      COMMON/OPTI2/DEF(13*4*8), SENSH(13,4,9,10),SENS(13*4*9*8)  !Nsol =10 et IPT= 4
       !  à changer dans BO1,SENS,MDR2(+MATR),BO2
       !  DEF  (13,IPT)            13*4=80    * 8  car  8 cas de base 
       !  SENS (13,IPT,NBRXI)      13*4*9=720 * 8  car  8 cas de base DISA,DISB,etc.
       !  SENSH(13,IPT,NBRXI,NSOL)=13*4*9=720 *10  car 10 cas de charge relatif à DISH

! Pour COMMON GRAV : Subr. COORD, OPTI et COPY
	COMMON/GRAV/YKG,XKGMIN,XKGMAX,XK,YK,IGRAV

! ESPACE DE TRAVAIL POUR VECTEURS DE TRAVAIL :
      DIMENSION TRAV(1),ITRAV(1)   !   TRAV(6000),ITRAV(1000)   !fev04

! Pour COMMON USHULL : IIII= 4*150+9 (avec 150 le nbre de panneaux max pour le deck, side, bottom et double bottom)
!            à changer dans HULL,BO2,USHULL,OPTI,COPY
      COMMON /USHULL/ AII(4),AIII(5),III(2),IIII(4*150+9)	!extension neto
      CALL ANNULI(III, 2) 
      CALL ANNULI(IIII,4*150+9) 
      CALL ANNULD(AII ,4) 
      CALL ANNULD(AIII,5) 

C    -------------------------------------------------------------------
C    -------------------------------------------------------------------

C	N  = NETO, 
C	   = Le nbre maximum de panneaux admis par le programme,

C	NV = Le nbre  de variables de conception pour l'ensemble de la structure
C		 qui est admis par le programme,

C     M1 = Nombre maximum de restrictions structurelles par panneau, 

C     M1T = Nombre maximum de restrictions structurelles pour toute la structure,
c                 ( 1 seul cas de charge)

C     M2  = Nombre maximum de restrictions géométriques par panneau,

C     M2T = Nombre maximum de restrictions géométriques pour toute la structure,

C     MM  = Nombre maximum de restrictions pour toute la structure(tous les cas de charge),  

C     NG  = Nombre maximum de restrictions d'égalité (pour toute la structure), 

C     IS  = Nombre maximum de cas de charges (NSOL), 

C     IPT = Nombre maximum de pts de calcul des sensibilités par panneau,
C           Attention: IPT est limité à 4 à cause de plusieurs vecteurs DEF(13,IPT=4),
C                      SENSA, .. SENSH(13,IPT=4,9,IS=10) 

C    -------------------------------------------------------------------
C    N,NE,NV,M1,M2,M1T,M2T,MM,NG,IS,IPT
C    ===================================
c	N   = 100   !Nbr max de panneau
c	NE  = N*8
c     NV  = 800   !Le nbre  de variables de conception (max = 9*NE)
C	M1  = 20    !Nbre max  rest. struct. par panneau (pour 1 cas de charge)
c     M1T =2000   !Nbre max  rest. struct. pour la struct. (pour 1 cas de charge
c	M2  =  20   !Nbre max  rest. géom. par panneau,
c     M2T =2000   !Nbre max  rest. géom. pour la struct.
c     MM  =7000   !MM=SOMME(M1T)+M2T < 700
c	NG  = 800   !Nbre max  rest. égalité (pour la struct.)
c     IS  =  10   !Nombre max de cas de charges (max=10)
c     IPT =   4   !Nombre max de pts de calcul des sensibilités par panneau
c	            !Attention: IPT ne peux pas être modifié !!!
C    -------------------------------------------------------------------

C  Remplacement du COMMON/BLOKA/ avec  BL(I1),IBL(II1),
	I1=1			   ! BLOKA (NE*NE) = vecteur de travail
	I2=I1 + NE*NE	   ! SOLT  (NE,NE) = vecteur de travail (ancien vecteur solution)
	I3=I2 + NE*NE 
c     Imax1=I3= 2*NE*NE= 128 N*N (car NE=8N)
	I4=1  	           ! SENS1(9,IPT,9,NETO) = vecteur sensibilité pour 1 cas de charge
	I5=I4 + 9*IPT*9*N  ! ANALY = Zone de travail pour la subr ANALYS (33 variables)	!février 2004
	I6=I5 + 2*33*N     !février 2004
c     Imax2=I6= 81*N*IPT +44*N

c     Imax= Imax1 car Imax1 >> Imax2     
      Imax=128*N*N       !       > 81*N*IPT +66*N
C         =128*100*100 = 1280000 > 81*100*4 +66*100= 36800
	
	II1=1              ! IANALY = Zone de travail INTEGER pour la subr ANALYS
	II2=II1 + 4*33*N   !            ( pour 33 variables)			!février 2004
	IImax=132*N  !  = 88*100=8800									!février 2004

      CALL ANNULD(BL, Imax)
      CALL ANNULI(IBL,IImax)
C ---------

C  Remplacement du COMMON/DI/ avec  DI(J1),DI(J2)
c     NB : DI est utilise comme espace de travail dans COORD.for (taille=32N)
C          DI est utilise comme vecteur de travail dans VISION et VISIONN (300.000 ou plus ??)
	J1=1			   ! A(NE*NE)  
	J2=J1 + NE*NE	   ! B(NE,NE+1)
	J3=J2 + NE*(NE+1)  
	Jmax=128*N*N     +8*N 
c	    =128* 30* 30 +8* 30 = 115440
c	    =128*100*100 +8*100 =1280800

      CALL ANNULD(DI,Jmax)
C -------------

C  *** VECTEUR AL, IAL
C      Remplacement du COMMON ALLONG par AL,IAL,AT
C      COMMON/ALLONG/ALLONG(200000,1,1)   ! ENT,GEOM,BATEAU,BO1,COE,COMPLE,LINEA
C                                         ! OPTI,VISION
      K1=1        ! -DZSN(8*neto,9,neto,IS)= DZSN(7200.000),
	            ! -CHA(100,3,IS) et V(100.000) , dans Subr ENT,
				!   V = de travail sauvetage des restriction structurelles,
	            !       (CHA et V, se suivent en mémoire)
	            !   Taille de V = NSOLM*NETO*(M1max+2*M1max/2+M1max/2) 
				!               = 100.000 (equivalent REAL*8 ).
				! -AUX2(400);
	            ! -ALLONG pour Subr CO1 et CO4;
	            ! -Subr VISION (Zone de travail);
                  ! -Subr OPTI :
                  !   XI(NV),FI(NV),CJ(MM),CJM(MM),
                  !   CIJ(NV,MM),S(ICONLIN),DX(NV,NV),3*Z(NV)
                  !   avec S  vect. de travail --> utilisé par CONLIN)
C ICONLIN = Dimension maximale pour le vecteur S(1) utile pour la subr. CONLIN
C         = 8*NV+3*MM+NV*MM+2*MAX0(NV,MM)+3
C         =5641.403   si NV=800 et MM=7000 (NETO=100)


C  Si K2 est limité par DZSN(8*NETO,9,NETO,IS)
C     K2= 72*N*N*IS (=7200.000 si N=100 et IS=10)
C  Si K2 est limité par OPTI et CONLIN
c     K2= 5*NV  + 2*MM   + NV * NV  + NV *MM 	
c       = 5*800 + 2*7000 + 800*800  + 800*7000   =6258.000  (NETO=100)

      Kmax=MAX0(8*N*9*N*IS,5*NV+2*MM+NV*NV+NV*MM)   


	KK1=1              ! LM2(20)
	KK2=KK1+M2         ! NPT(5) et NPTS(5) dans BO1
	                   ! et zone de travail (7*NETO) dans VISION
	KK3=KK2+7*N        ! 
	KKmax=M2 +7*N      ! =20+7*100= 720   

      L1=1               ! VNOM2(MM)   (CHARACTER*12 dans OPTI
	L2=L1+12*MM        ! MODES(NETO) (CHARACTER*3 dans OPTI ET COPY
	L3=L2+3*N

      CALL ANNULD(AL, Kmax)
      CALL ANNULI(IAL,KKmax)
C ----------------

C  Remplacement du COMMON/COORD/  par le vecteur C
C     COMMON/COORD/TETAS(Nmax),DELT(Nmax),Z(Nmax,4),XNEUT,YNEUT  

      IC1 =1			!TETAS(30)
	IC2a=IC1 +N		!DELT (30)
	IC2b=IC2a+N		!DELT2(30)
	IC3 =IC2b+N		!Z(30,4)
	IC4 =IC3 +N*4	!XNEUT
	IC5 =IC4 +1	    !YNEUT
	IC6 =IC5 +1		!
	ICmax=7*N+2     ! = 67*30+2 = 212

      CALL ANNULD(C,ICmax)

C  Remplacement du COMMON/ANG/  par ANN et NN
C     COMMON/ANG/ANGLE,  NOH,NOEUD,MCOMP,NNO,NC,NSIGN  
       
      INN1=1			!ANGLE(30)
	INN2=INN1+N   	!TETAQ(30) (cfr COMMON/EQUIL)
	INN3=INN2+N	    !
	INNmax=2*N  ! = 2*30 =60

      CALL ANNULD(ANN,INNmax)

      IN1=1			!NOH(NETO,10)
	IN2=IN1+10*N	!NOEUD(NETO,2)
	IN3=IN2+2*N		!MCOMP(2*NETO,2)
	IN4=IN3+4*N		!NNO(NETO+1,2)
	IN5=IN4+2*N+2	!NC
	IN6=IN5+1		!NSIGN(NETO)
	IN7=IN6+N		!ITYPE(NETO)
	IN8=IN7+N		!ISECT(NETO)		!février 2004		
	IN9=IN8+N							!février 2004	
	INmax=21*N+3  ! =21*100+3 = 2103	!février 2004	

      CALL ANNULI(NN,INmax) 

C     VECTEUR CT:
C     E(100),ETA(100),SIGY(100),SIGM(100),SPEC(100),PLOC(100)

      IT1=1				!E(NETO)
	IT2=IT1+N			!ETA(NETO)
	IT3=IT2+N			!SIGY(NETO)
	IT4=IT3+N			!SIGM(NETO)
	IT5=IT4+N			!SPEC(NETO)
	IT6=IT5+N			!PLOC(NETO)		
	IT7=IT6+N			!PART(NETO)			!coefficient de participation
	IT8=IT7+N			!CORRO(3*NETO)		!corrosion
	IT9=IT8+3*N			!DW(9*NETO)			!restri poids					
	IT10=IT9+9*N		!DCOST(9*NETO)		!restri cout
	IT11=IT10+9*N		!TAUNET(NETO,3)		!r&d13_tranchant
	IT12=IT11+3*N		!TAUGRO(NETO,3)		!r&d14	
	IT13=IT12+3*N		!dTAUNET(NETO,3,NETO)		!r&d14
	IT14=IT13+3*N*N		!dTAUGRO(NETO,3,NETO)		!r&d14	
	IT15=IT14+3*N*N		!dSIGNET(2,9,NETO)			!r&d14
	IT16=IT15+18*N		!dSIGGRO(2,9,NETO)			!r&d14
	IT17=IT16+18*N		!SIGNET(2)					!r&d14
	IT18=IT17+2			!SIGGRO(2)					!r&d14
	IT19=IT18+2			!dCIACS(9*NETO)				!r&d14
	IT20=IT19+9*N		!THICKNET(NETO)				!r&d14
	IT21=IT20+N			!THICKGRO(NETO)				!r&d14
	IT22=IT21+N			!dINET(9,NETO)				!r&d14
	IT23=IT22+9*N		!dIGRO(9,NETO)				!r&d14
	IT24=IT23+9*N		!dIPLNET(9,NETO)			!r&d14
	IT25=IT24+9*N		!dIPLGRO(9,NETO)			!r&d14
	IT26=IT25+9*N		!dYNEUTNET(9,NETO)			!r&d14
	IT27=IT26+9*N		!dYNEUTGRO(9,NETO)			!r&d14
	IT28=IT27+9*N		!dYNEUTPLNET(9,NETO)		!r&d14
	IT29=IT28+9*N		!dYNEUTPLGRO(9,NETO)		!r&d14
	IT30=IT29+9*N
							
	ITmax=(7+3+9+9+3+3+3*N+3*N+18+18+9+1+1+72)*N+4     ! = (34+3*150+3*150+18+18+9+1+1+72)*150+4 = 157954	!extension neto

      CALL ANNULD(CT,ITmax)

C  Remplacement du COMMON/OPTI/   avec P et IP
C     dans ENT,GEOM,BO1,BO2,RESUL,CONTR,OPTI,CONLIN,COPY,PAIK,HULL,USHULL
c      COMMON /OPTI/IOPTI,NTOT,M1TOT,M2TOT,NVAR(30),M1CONT(30),
c     *             M2CONT(30),NXIT(9,30),IPTS(30),YPTS(10,30),
c     *             IPTS2(10,30),IPTS3(10,30),LCONT(2,400),NVARR(30),
c     *             NXITR(9,30)

      JP1=1			!YPTS(4,100)
	JP2=JP1+IPT*N   !
	JPmax=IPT*N     ! = 4*100 = 400 

      KP1=1			!NVAR  (100)
	KP2=KP1+N		!M1CONT(100)
	KP3=KP2+N		!M2CONT(100)
	KP4=KP3+N		!NXIT (9,100)
	KP5=KP4+9*N	    !IPTS (100)
	KP6=KP5+N		!IPTS2(4,100)		
	KP7=KP6+IPT*N	!IPTS3(4,100)		
	KP8=KP7+IPT*N	!LCONT(2,M1T) M1T=NSOLM*NETO=2000		
	KP9=KP8+2*20*N	!NVARR(100)		
	KP10=KP9 +N		!NXITR(9,100)
	KP11=KP10+9*N	!M1TABL(NETO,NSOL)
	KP12=KP11+N*IS	
	KPmax=(63+2*IPT+IS)*N  ! = (63+2*4+10)*100=8100

      CALL ANNULD(P, JPmax)
      CALL ANNULI(IP,KPmax)

C  Remplacement du COMMON/OPTI4 et OPTI5/ avec R et MR
C     OPTI4 dans: MAIN,ENT,OPTI,COPY
c     COMMON/OPTI5/XICOU(200),TFA(30),TFR(30)
c     COMMON/OPTI4/XIMIN(200),XIMAX(200),CJMAX(400),INV(400),NEGALT,
c    *             MEGA(100,4),EGA(100),NVS(30),NXI2(9,30),NEGAL(30)
c        et de
c      ZSN,QN,PHILN,ABC,ASOL,BSOL,CSOL,DSOL,AMOM,BMOM,CMOM,DMOM,    


      IR1=1			 !XICOU(200)
      IR2=IR1+NV       !TFA(30)
      IR3=IR2+N        !TFR(30)
      IR4=IR3+N        !XIMIN(200)
      IR5=IR4+NV       !XIMAX(200)
      IR6=IR5+NV       !CJMAX(NSOLM*NETO)  NSOLM=20
      IR7=IR6+20*N     !EGA(100)
      IR8=IR7+NG       !ZSN(8*30,5)
      IR9=IR8+40*N     !QN(30)
      IR10=IR9 +N      !PHILN(30)
      IR11=IR10+N      !ABC (240)
      IR12=IR11+8*N    !ASOL(30)
      IR13=IR12+N      !BSOL(30)
      IR14=IR13+N      !CSOL(30)
      IR15=IR14+N      !DSOL(30)
      IR16=IR15+N      !AMOM(30)
      IR17=IR16+N      !BMOM(30)
      IR18=IR17+N      !CMOM(30)
      IR19=IR18+N      !DMOM(30)
      IR20=IR19+N      !
      
      IRmax=80*N + 3*NV       + NG 
c     IRmax=60*N + 3*NV + M1T + NG 
c     IRmax=60*30 +3*200+ 400 + 100 = 2900

      JR1=1			!INV(NSOLM*NETO)  NSOLM=20
      JR2=JR1+20*N    !MEGA(100,4)
      JR3=JR2+4*NG    !NVS(30)
      JR4=JR3+N       !NXI2(9,30)
      JR5=JR4+9*N     !NEGAL(30)
      JR6=JR5+N       !NEGALT
      JR7=JR6+1       !
      JRmax=31*N        + 4*NG  + 1  
c     JRmax=11*N  + M1T + 4*NG  + 1  
c     JRmax=11*30 + 400 + 4*100 + 1 = 1131
  
      CALL ANNULD(R, IRmax)
      CALL ANNULI(MR,JRmax)

C     ---------------------------------------------------------------
C     ---------------------------------------------------------------

	CALL MAIN2(BL(I1),BL(I2),BL(I4),BL(I5),IBL(II1),  ! COMMON BLOKA
     *           DI(J1),DI(J2),						  ! COMMON DI
     *           AL(K1),AL(K1),AL(K1+100*3*IS),AL(K1+100*12*IS),AL(K1),  ! COMMON ALLONG	!!!aout04
     *           IAL(KK2),IAL(KK2),IAL(KK1),                !IAL(Vision),NPT,LM2(Copy)
     *           AT(L1),AT(L2),
     *		   C(IC1),C(IC2a),C(IC2b),C(IC3),C(IC4),C(IC5),      ! COMMON COORD
     *           ANN(INN1),ANN(INN2),					             ! COMMON ANGL
     *           NN(IN1),NN(IN2),NN(IN3),NN(IN4),NN(IN5),NN(IN6),  !   suite
     *           NN(IN7),NN(IN8),                                  ! ITYPE,ISECT !février 2004
     *           CT(IT1),CT(IT2),CT(IT3),CT(IT4),CT(IT5),CT(IT6),	 
     *		   CT(IT7),CT(IT8),CT(IT9),CT(IT10),CT(IT11),CT(IT12),		! CT    !coefficient de participation   !corrosion   !restri poids  !restri cout !r&d13_tranchant
     *           CT(IT13),CT(IT14),CT(IT15),CT(IT16),CT(IT17),			! CT    !r&d14
     *		   CT(IT18),CT(IT19),CT(IT20),CT(IT21),CT(IT22),			! CT	!r&d14
     *		   CT(IT23),CT(IT24),CT(IT25),CT(IT26),CT(IT27),			! CT	!r&d14
     *		   CT(IT28),CT(IT29),										! CT	!r&d14
     *		   P(JP1),IP(KP1),IP(KP2),IP(KP3),IP(KP4),IP(KP5),   ! COMMON OPTI
     *           IP(KP6),IP(KP7),IP(KP8),IP(KP9),IP(KP10),IP(KP11),!    suite
     *           R(IR1),R(IR2),R(IR3),R(IR4),R(IR5),R(IR6),R(IR7), ! COMMON OPTI5 et OPTI4
     *           R(IR8),R(IR9),R(IR10),R(IR11),                    ! ZSN,QN,PHILN,ABC,
     *           R(IR12),R(IR13),R(IR14),R(IR15),                  ! ASOL,BSOL,CSOL,DSOL,
     *           R(IR16),R(IR17),R(IR18),R(IR19),                  ! AMOM,BMOM,CMOM,DMOM, 
     *           MR(JR1),MR(JR2),MR(JR3),MR(JR4),MR(JR5),MR(JR6),  ! COMMON OPTI5 et OPTI4
     *           KLI,
     *           TRAV,ITRAV)                                       ! Vecteur temporaire de travail

      RETURN                                                            
	END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC                                                                       
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
                                                                      

      SUBROUTINE MAIN2(BLOKA,SOLT,SENS1,ANALY,IANALY,		! BLOKA(NExNE),SOLT(NE,NE)
     *                 A,B,                               ! DI
     *                 DZSN,CHA,dCHA,V,AL, !!!aout04      ! ALLONG
     *                 IAL,NPT,LM2,                       ! IAL(Vision),NPT,LM2(Copy)
     *                 AT,MODES,
     *                 TETAS,DELT,DELT2,Z,XNEUT,YNEUT,    ! COORD
     *                 ANGLE,TETAQ,                       ! ANGL
     *                 NOH,NOEUD,MCOMP,NNO,NC,NSIGN, 
     *                 ITYPE,ISECT,										!février 2004
     *                 E,ETA,SIGY,SIGM,SPEC,PLOC,							! CT	
     *				 PART,CORRO,DW,DPRICE,TAUNET,TAUGRO,				! CT  coefficient de participation + !corrosion + !restri poids + !restri cout + !r&d13_tranchant + !r&d14
     *                 dTAUNET,dTAUGRO,dSIGNET,dSIGGRO,					! CT  r&d14
     *				 SIGNET,SIGGRO,dCIACS,THICKNET,THICKGRO,			! CT  r&d14
     *				 dINET,dIGRO,dIPLNET,dIPLGRO,dYNEUTNET,dYNEUTGRO,	! CT  r&d14
     *				 dYNEUTPLNET,dYNEUTPLGRO,							! CT  r&d14
     *				 YPTS,NVAR,M1CONT,M2CONT,NXIT,IPTS,		! OPTI
     *                 IPTS2,IPTS3,LCONT,NVARR,NXITR,M1TABL,	
     *                 XICOU,TFA,TFR,XIMIN,XIMAX,CJMAX,EGA,	! OPTI5 
     *                 ZSN,QN,PHILN,ABC,
     *                 ASOL,BSOL,CSOL,DSOL,AMOM,BMOM,CMOM,DMOM,   ! OPTI5 et OPTI4 
     *                 INV,MEGA,NVS,NXI2,NEGAL,NEGALT,
     *                 KLI,
     *                 TRAV,ITRAV)           ! Vecteur temporaire de travail

      IMPLICIT REAL *8(A-H,O-Z)

      COMMON/DIM1/Nmax,NEmax,NVmax,
     *            M1max,M1Tmax,M2max,M2Tmax,Mmax,NGmax,ISmax,IPTmax ! dimension max des vecteurs

      CHARACTER*1  AT(1),MODES(1),NUM
      CHARACTER*3  ILANG,IAU
	CHARACTER*12 COM
      CHARACTER*24 DON1,DON2,RES1,RES2,RES3,RES4	!r&d14

      COMMON/NOM/ NOM,NOM1   
      CHARACTER*11 NOM   !  NOM='FichDessin\'
      CHARACTER*22 NOM1  ! A.MULTI.STRUC\STRUC-a\....

	INTEGER*2    DESSIN
      REAL*8       LAMB,IXXTOT,IXXTOTPART		!obj inertie  !r&d13_tranchant
	REAL*8		 INET,IGRO,IPLNET,IPLGRO	!r&d14 
	 
	DIMENSION  ITYPE(Nmax),ISECT(Nmax)   !ITYPE(NETO) = 1, 2, 3, 4 pour COQUE PLAQUE COQUE1 et PLAQUE1	!février 2004
      DIMENSION  M1TABL(Nmax,ISmax)   !M1TABL(Nmax,ISmax)

      DIMENSION  ICHA(150) ! ICHA(NETO) à changer

      DIMENSION  DIS(5),FAM(6),TEXT(15),IFONCT(2)	
	DIMENSION  BM1(10),BM2(10),BM3(10),BM4(10),SF1(10),SF3(10)      ! BM1(IS),BM2(IS) à changer  !nov2003  !r&d14
      DIMENSION  NNSOL(10)  ! Liste des cas de charges étudiés

      DIMENSION  BLOKA(NEmax,NEmax),SOLT(NEmax,NEmax),
     *           SENS1(9,IPTmax,9,NEmax),ANALY(1),IANALY(1)     
      DIMENSION  A(NEmax*NEmax),B(NEmax,NEmax+1)		               ! Common DI
	DIMENSION  AL(1),DZSN(NEmax,9,Nmax,5),CHA(100,3,ISmax),V(1)     ! REAL du COMMON ALLONG
	DIMENSION  dCHA(100,ISmax,9)		!!!aout04
	DIMENSION  IAL(1),IDZ(1),NPT(ISmax),LM2(NGmax)			       ! INTEGER du COMMON ALLONG
	DIMENSION  TETAS(Nmax),DELT(Nmax),DELT2(Nmax),Z(Nmax,4)        ! COMMON COORD (subr. ENT,COORD,Bateau
      DIMENSION  ANGLE(Nmax),NOH(Nmax,10),NOEUD(Nmax,2),             ! COMMON ANG
     *           MCOMP(2*Nmax,2),NNO(Nmax+1,2),NSIGN(Nmax)
      DIMENSION  E(Nmax),ETA(Nmax),SIGY(Nmax),SIGM(Nmax),SPEC(Nmax), ! CT
     *           PLOC(Nmax)
      DIMENSION  YPTS(IPTmax,Nmax)								   ! COMMON OPTI
      DIMENSION  NVAR(Nmax),M1CONT(Nmax),M2CONT(Nmax),NXIT(9,Nmax),  ! COMMON OPTI
     *           IPTS(Nmax),IPTS2(IPTmax,Nmax),IPTS3(IPTmax,Nmax),   ! COMMON OPTI
     *           LCONT(2,M1Tmax),NVARR(Nmax),NXITR(9,Nmax)
      DIMENSION  XICOU(NVmax)										   ! COMMON OPTI5
      DIMENSION  XIMIN(NVmax),XIMAX(NVmax),CJMAX(M1Tmax),EGA(NGmax), ! COMMON OPTI4
     *           INV(M1Tmax),MEGA(NGmax,4),NVS(Nmax),NXI2(9,Nmax),   ! COMMON OPTI4
     *           NEGAL(Nmax)

      INTEGER*2  KLI
      DIMENSION  KLI(NEmax,2)
      DIMENSION  ZSN(NEmax,ISmax),QN(Nmax),PHILN(Nmax),ABC(NEmax),
     *           ASOL(Nmax),BSOL(Nmax),CSOL(Nmax),DSOL(Nmax),
     *           AMOM(Nmax),BMOM(Nmax),CMOM(Nmax),DMOM(Nmax)      

	DIMENSION PART(Nmax),ABCD(8),COEF(150,8,10)							!coefficient de participation
	DIMENSION CORRO(Nmax,3)												!corrosion
	DIMENSION DW(9*Nmax)												!restri poids
	DIMENSION DPRICE(9*Nmax)											!restri cout
	DIMENSION FK(3),WK(3,11)											!multi obj
	DIMENSION TAUNET(Nmax,3),TAUGRO(Nmax,3),							!r&d13_tranchant !r&d14
     *		  dTAUNET(Nmax,3,Nmax),dTAUGRO(Nmax,3,Nmax)					!r&d14
	DIMENSION SIGNET(2),SIGGRO(2),dSIGNET(2,9,Nmax),dSIGGRO(2,9,Nmax)	!r&d14
	DIMENSION dSIGSTIF(9)												!r&d14
	DIMENSION dCIACS(9*Nmax)											!r&d14
	DIMENSION THICKNET(Nmax),THICKGRO(Nmax)								!r&d14
	DIMENSION dINET(9,Nmax),dIGRO(9,Nmax),								!r&d14
     *		  dIPLNET(9,Nmax),dIPLGRO(9,Nmax)							!r&d14
	DIMENSION dYNEUTNET(9,Nmax),dYNEUTGRO(9,Nmax),						!r&d14
     *		  dYNEUTPLNET(9,Nmax),dYNEUTPLGRO(9,Nmax)					!r&d14

	INTEGER SYMX,SYMY,SYM											!flexion d'axe vert.  !obj inertie

	INTEGER*4 STATARRAY(12),TIMEARRAY(3),SECUR(7),YEAR(12)			!sécurité
	INTEGER*4 ISTAT,TOTDAYS,DAYS,DAYSREM,HOUR						!sécurité

	INTEGER*4 CPT		!multi obj

C
C     Les "COMMON"
C     --------------
      COMMON/LANGUE/ LANGUE,IBATCH                ! 1 French (par defaut), 2 English !Modif Batch
      COMMON/PY/   PI,BIDON6(12),LAMB(8)          ! BIDON6 sert pour SIN(4),COS(4) et EXP(4)
      COMMON/HYP/  HYP(360)                       ! BO1,SENS,BO2 (OK pour 10 traverses et 10 cas de charges
      COMMON/NOEUD/NTN
      COMMON/DEDE/ MDP(15,4),MAR(15,4)            ! RANG,RANG2 (15 = Nbre de types d'appuis)
      COMMON/ENT/  BIDON2(134)                    ! ENT,BO1,BO2 (OK pour 10 traverses)
C                      ! BIDON2 = ABTR,ARGQ,CONST,HXTR,DXTR,WXTR,TXTR

C     COMMON/COORD/BIDON1(61),Z(30,4),BIDON3  ! ENT,COORD,BATEAU,BO2
	             !A(30),A(30),AA,B(30,4),AA  avec 30 le nbre de panneaux
C     COMMON/ANG/  BIDON4(30),IBID1(573)          ! ENT,MODIF,COORD,ANGLE,RANG,RANG2
C     COMMON /ANG/ANGLE,NOH,NOEUD,MCOMP,NNO,NC,NSIGN
C     DIMENSION ANGLE(30),NOH(30,10),NOEUD(30,2),MCOMP(60,2),NNO(30+1,2),NSIGN(30)

C     COMMON/EQUIL/TETAQ(30)                      ! ENT,EQUIL (OK pour 30 panneaux)
      COMMON/PH/   BIDON7                         ! EQUIL (only), taille 1
C     COMMON/BRAS/ BIDON8(4)
      COMMON/GRAF/ BIDON9(20)                     ! SAVE (20 = nbre de dessins différents)
      COMMON/ECR/  BIDONA(51),IBID2  ! BO2,ECR,ECRI2,ECRI3 (51= 31 + 2* MT(=10))
      COMMON       BIDONB(400+520)   ! BATEAU+SYSTEM (OK avec 740),    !juillet04 (400+520 au lieu de 400+360 cfr participation)
	                               ! BO1,EDGE,INF3,MDR,MDR2,RANG,RANG2 (OK pour 10 traverses et 10 cas de charges)


C     For: ENT,GEOM,BO1,BO2,RESUL,CONTR,OPTI,CONLIN,COPY,PAIK,HULL,USHULL
      COMMON /OPTI/IOPTI,NTOT,M1TOT,M2TOT
C     COMMON /OPTI/IOPTI,NTOT,M1TOT,M2TOT,NVAR(30),M1CONT(30),
C    *             M2CONT(30),NXIT(9,30),IPTS(30),YPTS(10,30),
C    *             IPTS2(10,30),IPTS3(10,30),LCONT(2,400),NVARR(30),
C    *             NXITR(9,30)

C     For: BO1,SENS,MDR2(+MATR),BO2   (IPT=4 et NSOL=10 a changer manuellement)
      COMMON/OPTI2/DEF(13*4*8),SENSH(13,4,9,10),SENS(13*4*9*8)

C     For: ENT,BO1,DPRESS,DHYDRO,..,DHYP,SENS,B02
      COMMON/OPTI3/CONST2(54),CONST3(20),DARG(72)

C     For: ENT,SBEGA,BO2,CONTR,OPTI,COPY
C     COMMON/OPTI4/XIMIN(200),XIMAX(200),CJMAX(400),INV(400),NEGALT,
C    *             MEGA(100,4),EGA(100),NVS(30),NXI2(9,30),NEGAL(30)

C     For: MAIN,ENT,OPTI,COPY
C     COMMON/OPTI5/XICOU(200),TFA(30),TFR(30)

C     For: HULL,BO2,USHULL,COPY,OPTI
      COMMON/USHULL/DEPTH,DB,UHOGM,USAGM,FICTIF(5),IULT,IRESTR
      COMMON/STR/   BIDONC(14)                ! USHULL (OK)

C     For: ENT,HULL,BO2,HUGUES,OBJECT1&2
C      COMMON/MAT/   E(30),ETA(30),SIGY(30),SIGM(30),SPEC(30)
C
C     For: COST(ENT),OPTI,OBJECT1&2
      COMMON/COUT/ Poids,SPoids,
     *        Fmat,Fsou,FMdO,Fmat11,Fmat22,Fmat33,Fsou11,Fsou22,
     *        Fsou33,FMdO11,FMdO22,FMdO33,FMdO44,FMdO55,FMdO66,FMdO77,
     *        REND,EQP,Dref,DrefX,DrefY,C1,C2,C3,DC1,DW2,DW3,P10,DP10,
     *        P4,P5,DP4,DP5,P9X,P9Y,DP9X,DP9Y,BER,BET,P6,P7,C8,DC8,COUT,
     *        ICOUT,IALR,IALT

C     For: COORD, OPTI et COPY
	COMMON/GRAV/YKG,XKGMIN,XKGMAX,XK,YK,IGRAV

	DATA YEAR/31,28,31,30,31,30,31,31,30,31,30,31/		!sécurité
		
C                                                                       
C  NBRXI  = nombre de variables de conception par panneau ; NBRXI =NVAR(NEL)
C           (max=9)
C  NTOT   = Nombre de variables de conception pour toute la structure  ;
C           MAX = 800
C  M1     = Nombre de restrictions structurelles par panneau; 
C           M1=M1CONT(NEL) < 20
C  M1TOT  = Nombre de restrictions structurelles pour toute la
C           structure (pour un cas de charge)  < NETO*20
C  M2     = nombre de restrictions géométriques par panneau; 
C           M2=M2CONT(NEL) <20
C  M2TOT  = Nombre de restrictions géométriques pour toute la structure   ; 
C  MTOT   = Nombre de restrictions pour toute la structure   ; 
C           avec MTOT=Somme(M1TOT)+M2TOT < 7000
C  IPT    = nombre de pts de calcul des sensibilités par panneau ;
C           IPT=IPTS(NEL) (max=4)
c   IS    = Nombre de cas de charge (max=10)
C  YPT(I) = YPTS(I,NEL) Coordonnées (degré) des IPT pts de calcul (I=1,IPT)
C  IPT2   = IPTS2(I,NEL) nø (1 à 31) des IPT points               (I=1,IPT)
C  ICT(I) = LCONT(1,I)  = nø de référence de la restriction   (I=1 à M)
C  IY1(I) = LCONT(2,I)  = nø du pt YPT choisi (sensiblilité)  (I=1 à M).
C CJMAX(I)= borne supérieur de la restriction (I=1,800)
C  INV(I) = 1 ou -1 (si borne max ou borne min), (I=1,800)
C  NXI(i) = NXIT(i,NEL) i=1,9 ; 
C           Les N° des variables XI sélectionnées pour le panneau NEL;


***************************************************************************
C  I.  DONNEES GENERALES: définition de la structure et des sous-structures
C  =======================================================================

C   -TITRE SUR 60 CARACTERES MAX 
C   -NP		Nbre de structures (principale + sous-structures)
c   -NOM(i),i=1,NP  (1 ligne par nom)
c             Noms des fichiers contenant les données LBR5 de chaque structure 
c             (ou sous-structures)
c   - Liste des variables de conception communes.
C
c  1.0  Choix de la langue (Anglais ou Français)
c  ========================

       WRITE(*,*)' LOGICIEL LBR-5.7 - The LBR-5.6 SOFTWARE '
       WRITE(*,*)' ****************************************'
       WRITE(*,*)' '
      IF(LANGUE==1) THEN  !French
	 ILANG='101'
       WRITE(*,*)' '
       WRITE(*,*)' The current language is    = FRENCH  '
       WRITE(*,*)' La langue selectionnée est = FRANCAIS'
       WRITE(*,*)' '
       WRITE(*,*)' To used English (Pour utiliser l''anglais)'
       WRITE(*,*)'  type 102 , otherwise 0 (default value)'
       WRITE(*,*)' '
       WRITE(*,*)' Pour continuer en FRANCAIS'
       WRITE(*,*)'  taper  0 (valeur par défault)'
       WRITE(*,*)' '
      IF (IBATCH.EQ.0) THEN	!Modif Batch
		Read(*,*) ILANG
	ELSE					!Modif Batch
		ILANG = '0'			!Modif Batch
	ENDIF					!Modif Batch
       WRITE(*,*) ' '
       IF(ILANG=='102') THEN
	  LANGUE=2
        WRITE(*,*) ' '
        WRITE(*,*) ' You have changed the current language '
        WRITE(*,*) ' Now the language is English'
        WRITE(*,*) ' '
	  PAUSE 
	 ENDIF

      ELSE ! English
	 ILANG='102'
       WRITE(*,*)' La langue selectionnée est l''ANGLAIS'
       WRITE(*,*)' The current language is      ENGLISH'
       WRITE(*,*)' '
       WRITE(*,*)' Pour utiliser le FRANCAIS (To used French)'
       WRITE(*,*)'  Taper 101 (type 101), sinon 0 (par défault)'
       WRITE(*,*)' '
       WRITE(*,*)' To continue with English'
       WRITE(*,*)'  type  0 (default value)'
       WRITE(*,*)' '
       Read(*,*) ILANG
       WRITE(*,*) ' '
       IF(ILANG=='101') THEN
	  LANGUE=1
        WRITE(*,*) ' '
        WRITE(*,*) ' Vous avez modifié la langue du programme'
        WRITE(*,*) ' Il s''agit du Français.'
        WRITE(*,*) ' '
	  PAUSE 
	 ENDIF
	ENDIF

C
c  2.0  Choix du type d'analyse (IAUTO = 0, 1 ou 2)
c  ==================================================
c  IAUTO= 0 : analyse d''une structure unique ,
c  IAUTO= 1 : optimisation couplée de plusieurs structures,
c             (1 struct princ. et plusieurs sous-structures), 
c  IAUTO= 2 : analyse (non couplée) séquentielle de plusieurs structures uniques. 

      IF(LANGUE==1) THEN
	 IAU='0'
       WRITE(*,*)' '
       WRITE(*,*)' Pour exécuter :'
       WRITE(*,*)' - l''analyse d''une structure unique ',
     *                             '(avec ou sans opt), ' 
	 WRITE(*,*)'            -->  Taper 0 (default)'
       WRITE(*,*)' - l''optimisation couplée de plusieurs structures,'
       WRITE(*,*)'    (1 struct princ. et plusieurs sous-structures)'
	 WRITE(*,*)'            -->  Taper 11 '
       WRITE(*,*)' - l''analyse séquentielle (non couplée) , ',
     *                 'de plusieurs structures uniques, ',
     *                             '(avec ou sans opt), ' 
	 WRITE(*,*)'            -->  Taper 22 '
       WRITE(*,*)' '
       IF (IBATCH.EQ.0) THEN	!Modif Batch
		READ(*,*) IAU
	 ELSE					!Modif Batch
	    IAU = '0'			!Modif Batch
	 ENDIF					!Modif Batch
       IF    (IAU=='11') THEN
         IAUTO=1
	 ELSEIF(IAU=='22') THEN
         IAUTO=2
	 ELSE ! IAU=0
         IAUTO=0	   
	 ENDIF
	ELSE ! English (Langue =2)
	 IAU='0'
       WRITE(*,*)' '
       WRITE(*,*)' To execute :'
       WRITE(*,*)' - 1 analysis of a single structure ',
     *                             '(with or without opt),' 
	 WRITE(*,*)'            -->  Type 0 (default value)'
       WRITE(*,*)' - 1 optimisation analysis of a series of ',
     *           '   coupled structures,'
       WRITE(*,*)'   (1 main struct. & several sub-structures)'
	 WRITE(*,*)'            -->  Type 11 '
       WRITE(*,*)' - a series of analyse of single un-coupled',
     *                 ' structures (with or without opt),' 
	 WRITE(*,*)'            -->  Type 22 '
       WRITE(*,*)' '
       READ(*,*) IAU
       IF    (IAU=='11') THEN
         IAUTO=1   ! 1 exécution couplée (optimisation de n struct. couplée) 
	 ELSEIF(IAU=='22') THEN
         IAUTO=2   ! n exécutions en série (n struc non couplée) 
	 ELSE ! IAU=0
         IAUTO=0	 ! 1 exécution standard (1 structure)  
	 ENDIF
      ENDIF


C
c  3.0  Lecture du NOM du fichier général (contenant la liste des fichiers de données)
c  ===================================================================================
c    Cette lecture est faite si IAUTO = 1 ou 2
c    NP = Nbre de structures (ou sous-structures) devant être analysées
c
      IF(IAUTO.EQ.0) THEN
	  NP=1  ! cas standard avec 1 structure ==>lecture interactive du fichier des données
	ELSE ! IAUTO= 1 ou 2
	  IF(LANGUE==1) THEN
		 WRITE(*,*)' '
		 WRITE(*,*)' Entrez le NOM du FICHIER GENERAL(max 16 caract.)'
		 WRITE(*,*)' !! celui qui contient le nom des fichiers ',
     *          'relatifs aux structures et sous-structures étudiées.'
		 WRITE(*,*) ' '
		 WRITE(*,*) ' ATTENTION, il faut entrer LE NOM AVEC SON TYPE'
		 WRITE(*,*) ' *********'
		 WRITE(*,*) ' EXEMPLE: MULTI.DAT OU MULT.TXT'
		 Read(*,*) DON2
		 WRITE(*,*) ' '
		 WRITE(*,*) ' LE FICHIER DE DONNEE EST: ',DON2
		 WRITE(*,*) ' '
	     PAUSE
	  ELSE ! English
		 WRITE(*,*)' '
		 WRITE(*,*)' Enter the name of the DATA FILE (max 16 charact)'
		 WRITE(*,*)' !! which contains names of the data files co',
     *     'rresponding to the different structure(s) or sub-structures'
		 WRITE(*,*)' '
		 WRITE(*,*)' ATTENTION, enter NAME and TYPE'
		 WRITE(*,*)' *********'
		 WRITE(*,*)' EXAMPLE: MULTI.TXT or MULT.DAT'
		 Read(*,*) DON2
		 WRITE(*,*) ' '
		 WRITE(*,*) ' Your data file is:',DON2
		 WRITE(*,*) ' '
	     PAUSE
	  ENDIF

        OPEN(56,FILE=DON2,STATUS='OLD',ERR=905)  ! Données pour analyse MULTI-STRUCTURES
	                                           ! avec liste des fichiers données
        READ (56,4,END=901) TEXT
        READ (56,*,END=901) NP   ! NP = Nbre de fichiers de données à considérer

        IF(NP.GT.50) THEN													
	    WRITE(*,*) 'NP is too large [NP(max)=50], NP=',NP				
	    WRITE(*,*) 'NP = nbr of structures to analyse'
	    WRITE(29,*) 'NP is too large [NP(max)=50], NP=',NP							!bug
	    WRITE(29,*) 'NP = nbr of structures to analyse'								!bug
	    STOP
	  ENDIF

	ENDIF
c
c 4.0 DEBUT DE LA BOUCLE SUR
c     la lecture du NOM des fichiers de données et résultats
c  ==========================================================
      INP=0   ! compteur du nbre de structures étudiées (max = NP)
  902 CONTINUE  ! début de la boucle sur le nbre de structures à étudier.
      INP=INP+1
	IF(INP.GT.NP) THEN
	  RETURN ! le nbre de structures (NP) à étudier est dépassé.
	ENDIF
 
c 4.1 Lecture du nom du fichier données via le fichier OPEN(56, ..)
      IF((IAUTO==1).OR.(IAUTO==2)) THEN
        READ(56,*,END=901) DON1
	  RES1='Sol-'//DON1
	  RES2='Opt-'//DON1
	  RES3='Sol2-'//DON1
	  RES4='Sol3-'//DON1	!r&d14
        IF(LANGUE==1) THEN !1=French
          WRITE(*,*) 'Fichier donnée  : ',DON1
          WRITE(*,*) 'Fichier résultat: ',RES1
        ELSE !2= English
          WRITE(*,*) 'DATA FILE  : ',DON1
          WRITE(*,*) 'OUTPUT FILE: ',RES1
	  ENDIF
	ENDIF

c 4.2 Entrée du NOM des fichiers données et résultats par l'utilisateur (en direct)
	IF (IBATCH.EQ.1) THEN	! lulu
		NOM1= ''			!Modif Batch
	OPEN(33,FILE=NOM1//'boss.txt',STATUS='OLD',ERR=906) ! Donnée nom fichier	!Modif Batch
		REWIND 33			!Modif Batch
	ENDIF					!Modif Batch

c	IF (IBATCH.NE.0) THEN	!Modif Batch
c		NOM1= ''			!Modif Batch
c	OPEN(33,FILE=NOM1//'boss.txt',STATUS='OLD',ERR=906) ! Donnée nom fichier	!Modif Batch
c		REWIND 33			!Modif Batch
c	ENDIF					!Modif Batch



      IF(IAUTO.EQ.0) THEN  ! IAUTO=0  cas standard (1 structure unique)
 5642 ICO=1
      IF(LANGUE==1) THEN
       WRITE(*,*)' LOGICIEL L.B.R.-5.7 '
       WRITE(*,*)' *******************'
       WRITE(*,*)' '
       WRITE(*,*)' Entrez le NOM du FICHIER DONNEE (max 16 caractères)'
       WRITE(*,*) ' '
       WRITE(*,*) ' ATTENTION, il faut entrer LE NOM AVEC SON TYPE'
       WRITE(*,*) ' *********'
       WRITE(*,*) ' EXEMPLE: DONNEE.DAT OU STRUCTURE.TXT'
	 IF (IBATCH.EQ.0) THEN	!Modif Batch
         Read(*,*) DON1
	 ELSE					!Modif Batch
	   READ(33,*) DON1		!Modif Batch
	 ENDIF					!Modif Batch
       WRITE(*,*) ' '
       WRITE(*,*) ' LE FICHIER DE DONNEE EST-IL BIEN ',DON1,' ?'
       WRITE(*,*) ' '
       WRITE(*,*) ' SI OUI TAPER 0  (par défaut)'
       WRITE(*,*) ' SI NON TAPER 99'
       WRITE(*,*) ' '
       WRITE(*,*) ' SI VOUS VOULEZ STOPPER L''EXECUTION TAPER 100'
       WRITE(*,*) ' '
      ELSE ! English
       WRITE(*,*)' The L.B.R.-5.7  SOFTWARE '
       WRITE(*,*)' *************************'
       WRITE(*,*)' '
       WRITE(*,*)' Enter the name of the DATA FILE (max 16 characters)'
       WRITE(*,*)' '
       WRITE(*,*)' ATTENTION, enter NAME and TYPE'
       WRITE(*,*)' *********'
       WRITE(*,*)' EXAMPLE: DATA.TXT or STRUCTURE.DAT'
       Read(*,*) DON1
       WRITE(*,*) ' '
       WRITE(*,*) ' Is your data file correct ? : ',DON1
       WRITE(*,*) ' '
       WRITE(*,*) ' IF YES ENTER 0  (default value)'
       WRITE(*,*) ' IF NO  ENTER 99'
       WRITE(*,*) ' '
       WRITE(*,*) ' To CANCEL the execution,   ENTER  100'
       WRITE(*,*) ' '
      ENDIF

	IF (IBATCH.EQ.0) THEN	!Modif Batch
        READ(*,*) ICO
	ELSE					!Modif Batch
	  ICO = '0'				!Modif Batch
	ENDIF					!Modif Batch

        IF(ICO.EQ.99)  GOTO 5642  ! recommencer la lecture sur écran
        IF(ICO.EQ.100) STOP

        WRITE(*,*) ' '
	  RES2='Sol-'//DON1
        IF(LANGUE==1) THEN
         WRITE(*,*) ' Entrez le nom du fichier résultat: max 16 caract.'
         WRITE(*,*) ' Par défaut (taper 0): ',RES2
        ELSE
         WRITE(*,*)' Enter the NAME of the OUTPUT FILE: max 16 charact.'
         WRITE(*,*)' Default NAME is: ',RES2
         WRITE(*,*)' To select this name,  ENTER  0'
	  ENDIF

	  IF (IBATCH.EQ.0) THEN	!Modif Batch
          READ(*,*) RES1
	  ELSE					!Modif Batch
		RES1 = '0'			!Modif Batch
	  ENDIF					!Modif Batch

        IF(RES1.EQ.'0') RES1=DON1
 	  RES2='Opt-'//RES1
	  RES3='Sol2-'//RES1
	  RES4='Sol3-'//RES1
	  RES1='Sol-'//RES1
	  WRITE(*,*) ' '
        IF(LANGUE==1) THEN
          WRITE(*,*) 'MERCI, L''EXECUTION COMMENCE, VEUILLEZ PATIENTER'
          WRITE(*,*) 'Fichier résultat: ',RES1
        ELSE
          WRITE(*,*) 'Thanks, The OUTPUT file is: ',RES1
          WRITE(*,*) ' '
          WRITE(*,*) 'EXECUTION IS STARTED, please wait a while.'
	  ENDIF

	ENDIF  !  IF(IAUTO.EQ.0) THEN  

***************************************************************************
C
c 5.0 OUVERTURE DES FICHIERS (OPEN(55, ...
c ==========================================================
      NOM='FichDessin\'

      OPEN(42,STATUS='SCRATCH',FORM='UNFORMATTED') ! Pour le dessin (subr ENT,VISION
      OPEN(43,STATUS='SCRATCH',FORM='UNFORMATTED') ! u,v,w, ..(Subr. SAVE et VISION)
      OPEN(45,STATUS='SCRATCH',FORM='UNFORMATTED') ! Pour le dessin (subr ENT,VISION
      OPEN(46,STATUS='SCRATCH',FORM='UNFORMATTED') ! Pour le dessin (subr ENT,COPY,VISIONN
	                                             ! Données courantes pour Subr.VISIONN (DESSIN) 

      IF(IAUTO.EQ.0) THEN !analyse unique
	  NOM1= '' 
	ELSE ! IAUTO= 1 ou 2 (multi structures)
        NUM=CHAR(96+INP)  ! NUM='a' si INP=1, ...  CHARACTER*1  NUM
	  NOM1= 'A.MULTI.STRUC\STRUC-' //NUM//'\'  ! CHARACTER*22 NOM1
	ENDIF

      OPEN(55,FILE=NOM1//DON1,STATUS='OLD',ERR=903) ! Données modélisation structure
           
C     OPEN(56,FILE=DON2,STATUS='OLD',ERR=905)  voir ci-avant
c        ! contient la liste des "fichiers de données"  (pas utilisé si IAUTO=0)                                  ! Déjà ouvert (si exécution en série (voir ci avant)
         ! (pour une exécution de plusieurs fichiers de donnees)
         ! File 56 est déjà ouvert (voir ci-avant)

c      OPEN(57,FILE=NOM1//'ATableur.txt',STATUS='OLD',ERR=904)      ! Données coût  + !corrosion (mise en commentaire)

      OPEN(66,FILE=NOM1//RES1)    ! Sortie Listing (sortie standard)
c     Un autre Open 66 existe dans la Subr LBR5 (= fichier graphique ECRAN)
        
	OPEN(67,FILE=NOM1//RES3)    ! Sortie Listing (recapitulatif des resultats)


      OPEN(94,STATUS='SCRATCH',FORM='UNFORMATTED') ! Subr PARTICIPATION:forces de bord (ABCD)
												 ! Coeff. de participation
	OPEN(97,STATUS='SCRATCH',FORM='UNFORMATTED') ! Subr BATEAU,BO1:forces de bord (ABCD),
C     File 98:sauvetage pour ITERA=1 ONLY       !
      OPEN(98,STATUS='SCRATCH',FORM='UNFORMATTED') ! Subr. ENT,BO1 (charge de type CHA)
      OPEN(99,STATUS='SCRATCH',FORM='UNFORMATTED') ! Subr. ENT,BO1,BO2
                                                   !
      OPEN(301,STATUS='SCRATCH',FORM='FORMATTED')  ! Données pour CONLIN		!extension neto
      OPEN(302,STATUS='SCRATCH',FORM='UNFORMATTED')! Données fct Objct		!extension neto
      OPEN(303,STATUS='SCRATCH',FORM='UNFORMATTED')! Subr. Contraintes		!extension neto
      OPEN(304,STATUS='SCRATCH',FORM='FORMATTED')  ! Valeurs extrêmes			!extension neto

      OPEN(305,FILE='ALSTOM.txt')   ! Données pour Subr "COST-ALSTOM", mars 2003		!extension neto
	OPEN(306,STATUS='SCRATCH',FORM='UNFORMATTED') !Données pour BO2,février 2004
	OPEN(2218,STATUS='SCRATCH',FORM='FORMATTED')  !novembre 2004	

	OPEN(29,FILE=NOM1//'bug-'//RES1)				!bug  29=année du crash de wall street
	write(29,*)'ITERATION 0'													!bug

C     FILES OPENED LATTER
C -----------------------------
C   - FILE 1,2,3,7, 44 (Subr. COSTMAIN)
c	   OPEN (unit=1, file='DBinput.txt',          status='old',ERR=906) !juillet2003
c	   OPEN (unit=2, file='DBfractionnement.txt', status='old')
c	   OPEN (unit=3, file='DBacces.txt',          status='old',ERR=908) !juillet2003
c        OPEN (unit=7, file='DBbase.txt',           status='old',ERR=904) !juillet2003
c 	   OPEN (unit=44,file='DBsoudures.txt',       status='old',ERR=905) !juillet2003
C   - FILE 20 (Subr. Graphic)
c        OPEN(20,file='TEMPT') fichier temporaire
C   - FILE 21 (Subr. Graphic et Subr. LBR5 --> Subr. QW Drawings)
c        OPEN(21,file='FIG.DAT')
C   - FILE 22 (Subr. Vision, Visionn et Graphic)
c	   OPEN(22,file=NOM//'U.DES') avec NOM = 'FichDessin'
C          à  
C        OPEN(22,file=NOM//'SX.DES'), etc.
c   - FILE 23 (Subr LBR5 --> Subr. QW Drawings)
c       OPEN(23,FILE='INDICE.DAT') --> pour dessin
C       Ecrire  dans Subr MAIN (sauvetage des données)
C       Lecture dans Subr LBR5 (Subr. QW Drawings)
c   - FILE 77
c     Subr. COPY (save updated data)
c       OPEN(77,file='UPDATED-DATA:'Up-//DON1)
c   - FILE 78  (si optimisation)
c     Subr. ENT  (save list of constraint in ENT to be re-used in CONTR)
c       OPEN(78,STATUS='SCRATCH',FORM='UNFORMATTED')
c   - FILE 95 et 96  (si IBUSC=1)
c     Subr. BO1
c       OPEN(95,FILE='BUSM.txt',STATUS='OLD',ERR=659)     
c       OPEN(96,FILE='BUSC.txt',STATUS='OLD',ERR=657)     
C   - Files 101 à 200
c     Tempory files opened in MAIN and used in BO1 and BO2)
c	  OPEN(101,STATUS='SCRATCH',FORM='UNFORMATTED')
C        à
c	  OPEN(100+NETO,STATUS='SCRATCH',FORM='UNFORMATTED')
C   - Files 301 à 400
c     Tempory files opened opened in MAIN and used in BO1 and BO2) only if IOPTI>0
c	  OPEN(301,STATUS='SCRATCH',FORM='UNFORMATTED')
C        à
c	  OPEN(300+NETO,STATUS='SCRATCH',FORM='UNFORMATTED')
c   - File 666 (if IOPTI>0)
c     Opened in Prog. MAIN (Output during Optimization process)
c      OPEN(666,FILE=OPT+NOM.*)		          			
C   - Files 701 à 800
c     Tempory files opened in MAIN and used in MDR2 and BO2 (pour ZSN et DZSN)
c	  OPEN(701,STATUS='SCRATCH',FORM='UNFORMATTED')
C        à
c	  OPEN(700+NETO,STATUS='SCRATCH',FORM='UNFORMATTED')

c	ISTAT=FSTAT(55,STATARRAY)											!sécurité
c	CALL ANNULD(SECUR,7)												!sécurité
c	TOTDAYS=STATARRAY(10)/24/60/60										!sécurité
c	DAYS=0																!sécurité
c	I=1																	!sécurité
c	DO WHILE(DAYS.LT.TOTDAYS)											!sécurité
c	  I=I+1																!sécurité
c	  IF((I-2-((I-2)/4)*4).GE.1) THEN									!sécurité
c	    DAYS=2*365+(I-2)/4*(366+3*365)+366+((I-2-((I-2)/4)*4)-1)*365	!sécurité
c	  ELSE																!sécurité
c	    DAYS=2*365+(I-2)/4*(366+3*365)									!sécurité
c	  ENDIF																!sécurité
c	ENDDO																!sécurité
c	I=I-1																!sécurité
c	SECUR(6)=1970+I														!sécurité
c	IF((I-2-((I-2)/4)*4).GE.1) THEN										!sécurité
c	  DAYS=2*365+(I-2)/4*(366+3*365)+366+((I-2-((I-2)/4)*4)-1)*365		!sécurité
c	ELSE																!sécurité
c	  DAYS=2*365+(I-2)/4*(366+3*365)									!sécurité
c	ENDIF																!sécurité
c	DAYSREM=TOTDAYS-DAYS												!sécurité
c	DAYS=0																!sécurité
c	I=0																	!sécurité
c	DO WHILE(DAYS.LT.DAYSREM)											!sécurité
c	  I=I+1																!sécurité
c	  IF((I.EQ.2).AND.(MOD(SECUR(6),4).EQ.0)) THEN						!sécurité
c	    DAYS=DAYS+YEAR(I)+1												!sécurité
c	  ELSE																!sécurité
c	    DAYS=DAYS+YEAR(I)												!sécurité
c	  ENDIF																!sécurité
c	ENDDO																!sécurité
c	SECUR(5)=I															!sécurité
c	IF((I.EQ.2).AND.(MOD(SECUR(6),4).EQ.0)) THEN						!sécurité
c	  SECUR(4)=DAYSREM-(DAYS-(YEAR(I)+1))+1								!sécurité
c	ELSE																!sécurité
c	  SECUR(4)=DAYSREM-(DAYS-YEAR(I))+1									!sécurité
c	ENDIF																!sécurité
c	SECUR(3)=STATARRAY(10)/60/60-TOTDAYS*24								!sécurité
c	SECUR(2)=STATARRAY(10)/60-TOTDAYS*24*60-SECUR(3)*60					!sécurité
c	SECUR(1)=STATARRAY(10)-TOTDAYS*24*60*60-SECUR(3)*60*60-SECUR(2)*60	!sécurité
c	SECUR(7)=STATARRAY(8)												!sécurité
	
c	TIME=RTC()															!sécurité
c	TOTDAYS=INT(TIME)/24/60/60											!sécurité
c	HOUR=INT(TIME)/60/60-TOTDAYS*24										!sécurité
c	CALL ITIME(TIMEARRAY)												!sécurité
c	SECUR(3)=SECUR(3)+(TIMEARRAY(1)-HOUR)								!sécurité


    
C 6.0 INITIALISATION
c =======================
      CALL ANNULD(HYP   ,360)
      CALL ANNULD(BIDON2,134)
      CALL ANNULD(BIDON6,12)                                
      BIDON7=0.D+00
      CALL ANNULD(BIDON9,20)   ! Subr SAVE (20 = nbre de dessins)                          
      CALL ANNULD(BIDONA,51)
      CALL ANNULD(BIDONB,760)
c     CALL ANNULD(BIDONC,14)  mise à zéro effectuée plus loin (cfr Subr. HULL)


      PI=2.*DASIN(1.D00)

C 7.0 LECTURE DES DONNEES GENERALES (FILE 55)
C ==========================================
      IF ((IAUTO==1).OR.(IAUTO==2)) THEN
       WRITE(66,3) TEXT  ! impression du fichier géneral
       WRITE(66,*) 
       WRITE(66,*) 'Nom du fichier /file name:', DON2
       WRITE(66,*) ' (multi-structures)'
       WRITE(66,*) 
	ENDIF

      READ(55,4) TEXT					!Février 2004 version LBR-5
	WRITE(66,4) TEXT
	WRITE(66,*)
      WRITE(67,4) TEXT
	WRITE(67,*)

	READ(55,*) IANA					!r&d14	!=1:LBR-4 structural analysis
											!=2:beam structural analysis
	READ (55,4) TEXT
      WRITE(66,3) TEXT
      WRITE(67,3) TEXT

      WRITE(66,*) 
      WRITE(66,*) 'Noms des fichiers /file names:'
      WRITE(66,*) ' INPUT - DONNEES    :', DON1
      WRITE(66,*) ' OUTPUT-RESULTAT #1 :', RES1
      WRITE(66,*) '                 #3 :', RES3
      WRITE(66,*) 

      READ(55,*) IMPR,IMPR2,INDAIG,INDRAI,DESSIN,JLPH2,IBUSC,NETO

c Limitation à 25 panneaux
c	IF(NETO.GT.25) THEN
c	  WRITE(*,*) 'Number of panels limited to 25'
c	  STOP
c	ENDIF

	! JLPH2 = Nbre de termes IMPAIR, avec son signe > ou <
	! JLPH  = Nbre de termes total (pair et impair)
	JLPH=2*IABS(JLPH2)-1

      IPRINT=0

      IF(LANGUE==1)THEN  
	 WRITE(66,10) NETO  ! FRENCH
	 WRITE(* ,10) NETO
	ELSE
       WRITE(66, 6) NETO  ! ENGLISH
       WRITE(* , 6) NETO  ! ENGLISH
	ENDIF

	IF (IBATCH.EQ.0) THEN		!Modif Batch
	  IF(IAUTO.EQ.0) PAUSE 'OK?'
	ENDIF						!Modif Batch

	READ(55,*) IMULTI,RHO,W1,W2,W3		!multi obj

      READ(55,*) IOPTI,ITERAM

      IF(IOPTI.NE.1) ITERAM=99
	IF(IOPTI.GE.1) THEN 
	
        OPEN(666,FILE=NOM1//RES2) ! Output File for the Optimization process OPT-NOM.*

c       FILE 78  (save list of struct constraint in ENT to be re-used in CONTR)
        OPEN(78,STATUS='SCRATCH',FORM='UNFORMATTED')

        WRITE(666,  3) TEXT
	  WRITE(666,511)  ! VARIABLES DE CONCEPTION et RESTRICTIONS GEOMETRIQUES 
c        IF(IMPR2.EQ.-3) IMPR2=-2						!15.10.05
        IF(IOPTI.GE.3 ) IPRINT=1
        IF(LANGUE==1) WRITE(66,'(T22,A)')'AVEC OPTIMISATION'
        IF(LANGUE==2) WRITE(66,'(T22,A)')'WITH OPTIMISATION'
	  WRITE (66,400)
	  WRITE(66,*)'                     (iteration nø 1)'
	  WRITE(66,*)'                     (max=',ITERAM,')'
      ELSE
        IF(LANGUE==1) WRITE(66,'(T22,A)')'SANS OPTIMISATION'
        IF(LANGUE==2) WRITE(66,'(T22,A)')'WITHOUT OPTIMISATION'
	  WRITE (66,400)
      ENDIF      

      If(LANGUE==1) THEN
        WRITE (66, 11) JLPH2,IBUSC
        IF(IMPR  .NE.0) WRITE(66,7)
        IF(INDAIG.EQ.0) WRITE(66,9)
        IF(INDRAI.EQ.0) 
     *     WRITE(66,'(/A)')' PAS DE RESULTATS dans les RAIDISSEURS'
        IF(IMPR2 .EQ.1) WRITE(66,8)
	ELSE
        WRITE (66, 14) JLPH2,IBUSC
        IF(IMPR  .NE.0) WRITE(66,'(/A)')' With Intermediate Results'
        IF(INDAIG.EQ.0) 
     *     WRITE(66,'(/A)')' Results in the FRAMES are not printed'
        IF(INDRAI.EQ.0) 
     *     WRITE(66,'(/A)')' Results in the STIFFENERS are not printed'
        IF(IMPR2 .EQ.1) WRITE(66,*)' Final Results + Equilibrium Checks'
	ENDIF

  	WRITE(66,399) Nmax,NVmax,M1max,M2max,Mmax,NGmax,ISmax,IPTmax


c 7.1 Lecture des coûts
C     ------------------
      READ(55,*) ICOUT                ! 18-5-98
C      ICOUT=0 -> Fct Obj. Poids ; ICOUT =1 -> Fct Obj. Coût)
C	 ICOUT=-1 -> Fct Obj. Inertie								!obj inertie
	  WRITE(666,*)'NETO',NETO              				!eugen (17.10.2007)
	  WRITE(666,*)'IANA',IANA              				!eugen (17.10.2007)
	  WRITE(666,*)'ICOUT',ICOUT							!eugen (11.10.2007)
        WRITE(666,*)'ITERAM',ITERAM						!eugen (11.10.2007)
	  WRITE(666,*)'IMULTI',IMULTI						!eugen (11.10.2007)
	  WRITE(666,'(A,F9.4,F9.4,F9.4/)')'WEIGHTS',W1,W2,W3	!eugen (12.10.2007)   
	  
C     Lecture de RENDEMENT, Equivalent Poids de la MdO (=k)                                      
      READ(55,*) REND, EQP         !
      READ(55,*) Dref,DrefX,DrefY  ! Epaisseurs de référence
      READ(55,*) C1,C2,C3,DC1      ! Cout des matériaux
      READ(55,*) DW2,DW3           ! Extra weight
      READ(55,*) P10,DP10          ! MdO bordé
      READ(55,*) P4,P5,DP4,DP5     ! MdO assembl. Membr-bordé
      READ(55,*) P9X,P9Y,DP9X,DP9Y ! MdO constr. Membr.
      READ(55,*) P6,P7,BER,BET     ! MdO Intersect et goussets
      READ(55,*) C8,DC8,IALR,IALT  ! Coût Soudure (Energ + consom.

c 7.2 Lecture des parametres generaux WIDTH, DIS, FAM, IPOIDS, NSOL
C     --------------------------------------------------------------
      READ(55,*) WIDTH            ! 8-5-96
      READ(55,*) (DIS(I),I=1,5)   ! 8-5-96
      READ(55,*) (FAM(I),I=1,6)   ! 8-5-96
	FAM(5)=0.
	FAM(6)=0.
      READ(55,*) IPOIDS

      IF(LANGUE==1) THEN ! Francais
	  WRITE(66,'(/A,F9.4,A)')' LONGUEUR DE LA STRUCTURE =',WIDTH,' m'
	  WRITE(66,12)(DIS(I),I=1,5)
        WRITE(66,13)(FAM(I),I=1,6)
        WRITE(66,*)' '
        IF(IPOIDS.EQ.1) THEN
          WRITE(66,*)' ON TIENT COMPTE DU POIDS PROPRE DES PANNEAUX',
     *               ' (IPOIDS =1)'
        ELSE
          WRITE(66,*)' ON NE TIENT PAS COMPTE DU POIDS PROPRE DES',
     *               ' PANNEAUX (IPOIDS =0)'
        ENDIF
        WRITE(66,'(1X,51(1H-)/)')
	ELSE  ! Anglais
	  WRITE(66,'(/A,F9.4,A)')' LENGTH of the STRUCTURE =',WIDTH,' m'
	  WRITE(66,'(/2A)')' LOCATIONS (X) of 5 SECTIONS where displ.',
     *                   ' and stress are given (in meters)'
	  WRITE(66,'(T10,5(2X,F9.4))')(DIS(I),I=1,5)
        WRITE(66,13)(FAM(I),I=1,6)
        WRITE(66,*)' '
        IF(IPOIDS.EQ.1) THEN
          WRITE(66,*)'DEADWEIGHT OF THE STRUCTURE IS CONSIDERED',
     *               ' (IPOIDS =1)'
        ELSE
          WRITE(66,*)'DEADWEIGHT OF THE STRUCTURE IS NOT CONSIDERED',
     *               ' (IPOIDS =0)'
        ENDIF
        WRITE(66,'(1x,46(1H-)/)')
	ENDIF
      READ(55,*) NSOLM, NSOL,(NNSOL(I),I=1,NSOL)    
c       NSOLM = nbre de cas de sollicitations defini dans les données
c       NSOL  = Nbre de cas de sollicitations qui seront étudiés
c       NNSOL = Liste des numeros des NSOL cas étudies.
      IF(LANGUE==1) WRITE(66,419) NSOLM, NSOL,(NNSOL(I),I=1,NSOL)  
      IF(LANGUE==2) WRITE(66,420) NSOLM, NSOL,(NNSOL(I),I=1,NSOL)  
	DO I=1,NSOLM   !nov2003
	  READ(55,*)   !nov2003
	ENDDO          !nov2003
	IF((NSOL.LT.1).OR.(NSOL.GT.ISmax)) THEN
          WRITE(*,*) 'Le nbre de sollicitation est incorrect (max.= 10)'
          WRITE(*,*) 'The Number of load cases is WRONG      (max.= 10)'
	  PAUSE 'Stop'
	  STOP
	ENDIF

c     Calcul du parametre "NE" = Nbre d'inconnues hyperstatiques de la structure
      NE=NETO*8

C     Vérification de la taille du VECTEUR AL càd DZSN(NE,9*NETO,NSOL)<7200000
	Imax=72*ISmax*Nmax
	IF(Imax.GT.7200000) THEN
        WRITE(*,*) ' Le nbre de panneau est trop élevé pour NSOL=',NSOL
	  WRITE(66,*)' Le nbre de panneau est trop élevé pour NSOL=',NSOL
        WRITE(29,*)'Le nbre de sollicitation est incorrect (max.= 10)'			!bug
	  PAUSE'STOP'
	  STOP
	ENDIF

C ***********************************************************************

C 8.0 DEPART D'UNE NOUVELLE ITERATION (Optimisation via OPTI/CONLIN)
C ===================================================================
      ITERA=0					!13.12.05
	WRITE(29,*)'ITERATION 0'		!13.12.05											!bug
	WRITE(29,*)'<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
     *<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<'
	WRITE(29,*)' '																!bug
	ITER1= ITERAM+1			!13.12.05 pour avoir analyse structurelle du dernier échantillonage

  	IF(IMULTI.EQ.1) THEN			!multi obj
	  CALL ANNULD(FK,3)				!multi obj
	  IF(W1.NE.0) THEN				!multi obj
	    IICOUT=ICOUT				!multi obj
		CPT=1						!multi obj
	  ELSEIF(W2.NE.0) THEN			!multi obj
	    IICOUT=0					!multi obj
		CPT=2						!multi obj
	  ELSEIF(W3.NE.0) THEN			!multi obj
	    IICOUT=-1					!multi obj
		CPT=3						!multi obj
	  ENDIF							!multi obj
	ELSE							!multi obj
	  IICOUT=-5						!multi obj
	ENDIF							!multi obj

c	CALL RANDOM_SEED()
c	CALL RANDOM_NUMBER(W1)
c	CALL RANDOM_NUMBER(W2)
c	W3=1.-W1-W2

  303 CONTINUE

	IF((IMULTI.EQ.1).AND.(ITERA.GT.ITER1)) THEN		!multi obj
	  IF(CPT.EQ.1) THEN								!multi obj
	    IF(W2.NE.0) THEN							!multi obj
		  IICOUT=0									!multi obj
		ELSEIF(W3.NE.0) THEN						!multi obj
		  IICOUT=-1									!multi obj
		  CPT=2										!multi obj
	    ENDIF										!multi obj
	  ELSEIF(CPT.EQ.2) THEN							!multi obj
	    IF(W3.NE.0) THEN							!multi obj
		  IICOUT=-1									!multi obj
		ELSE										!multi obj
	      CPT=3										!multi obj
	      IICOUT=-2									!multi obj
	    ENDIF										!multi obj
	  ELSEIF(CPT.EQ.3) THEN							!multi obj
		IICOUT=-2									!multi obj
	  ELSEIF(CPT.EQ.4) THEN							!multi obj
		GOTO 304									!multi obj
	  ENDIF											!multi obj
	  CPT=CPT+1										!multi obj
	  ITERA=0										!multi obj
        do  NEL=1,NETO								!multi obj
          close(100+NEL)								!multi obj
          if(IOPTI.ge.1) close(400+NEL)				!multi obj		
          close(700+NEL)								!multi obj
        enddo											!multi obj
	ENDIF											!multi obj

	IF((IMULTI.NE.1).AND.(ITERA.GT.ITER1)) GOTO 304		!13.12.05  !multi obj
	
	IF(ITERA.GT.0)THEN				!13.12.05
	  WRITE(29,*)'<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
     *<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<'
	  WRITE(29,*)'ITERATION ',ITERA												!bug
	  WRITE(29,*)'<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
     *<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<'
	  WRITE(29,*)' '															!bug
      ENDIF

      IF(LANGUE==1) WRITE(67,10) NETO
      IF(LANGUE==2) WRITE(67, 6) NETO

	IF (IMPR2.EQ.1) THEN		!+++31.01.06
		IF (ITERA.LT.ITER1)THEN  !juillet04		!+++31.01.06
	    WRITE(66,418)ITERA              !juillet04
	    WRITE(67,418)ITERA              !juillet04
		ELSEIF (ITERA.EQ.ITER1) THEN				    !13.12.05
	    WRITE(66,418)ITERAM     !13.12.05
	    WRITE(67,418)ITERAM     !13.12.05
		ENDIF            !  analyse structurelle finale               !juillet04
	

c	  IF((IMPR2.EQ.1).AND.(ITERA.NE.ITER1)) THEN		!+++31.01.06
c		WRITE(66,418)ITERA								!+++31.01.06
	  ELSE
	  	CLOSE(66)
 	  	CLOSE(67)
          OPEN(66,FILE=NOM1//RES1)                 ! Sortie Listing  --avr04
          OPEN(67,FILE=NOM1//RES3)                 ! Sortie Listing  --avr04
          WRITE(66,3) TEXT
          WRITE(67,3) TEXT
		IF(ITERA.EQ.ITER1) THEN
		WRITE(66,418) ITERAM		!13.12.05
		WRITE(67,418) ITERAM		!13.12.05
		ELSE
          WRITE(66,418)ITERA
          WRITE(67,418)ITERA
		ENDIF			!analyse structurelle finale
          WRITE(66,10) NETO
          WRITE(66,'(T26,A)')'AVEC OPTIMISATION'
          WRITE(66,'(T26,A,I3,A)')'(Iter. max=',ITERAM,')'
          WRITE(66,400)
          WRITE(66,11) JLPH2,IBUSC
          IF(IMPR.NE.0)WRITE(66,7)
          IF(INDAIG.EQ.0)WRITE(66,9)
          IF(IMPR2.EQ.1)WRITE(66,8)
          WRITE(66,12)(DIS(I),I=1,5)
          WRITE(66,*)
          IF(IPOIDS.EQ.1) THEN
           WRITE(29,*)' ON TIENT COMPTE DU POIDS PROPRE DES PANNEAUX',			!bug
     *                 ' (IPOIDS =1)'
          ELSE
            WRITE(66,*)' ON NE TIENT PAS COMPTE DU POIDS PROPRE DES',
     *                 ' PANNEAUX (IPOIDS =0)'	
            WRITE(29,*)' ON NE TIENT PAS COMPTE DU POIDS PROPRE DES',				!bug
     *                 ' PANNEAUX (IPOIDS =0)'
         ENDIF			!poids
          WRITE(66,'(1X,51(1H-)/)')
          WRITE(66,419) NSOLM, NSOL,(NNSOL(I),I=1,NSOL)
        ENDIF			!pour imp2
	if(ICOUT.LT.2) then													!26.05.04	!obj inertie
        WRITE(66,*)'Le cout est calculé par un modèle simplifié '			!26.05.04
        WRITE(67,*)'Le cout est calculé par un modèle simplifié '			!26.05.04
        WRITE(29,*)'Le cout est calculé par un modèle simplifié '						!bug
        if(iopti.ge.1) then
	    WRITE(666,*)'Le cout est calculé par un modèle simplifié '		!26.05.04
	  endif
	else																!26.05.04
        WRITE(66,*)'Le cout est calculé par un modèle détaillé COST CAT'	!26.05.04
        WRITE(67,*)'Le cout est calculé par un modèle détaillé COST CAT'	!26.05.04
	  WRITE(29,*)'Le cout est calculé par un modèle détaillé COST CAT'				!bug
        if(iopti.ge.1) then
	    WRITE(666,*)'Le cout est calculé par modèle détaillé COST CAT'	!26.05.04
	  endif
	endif															!26.05.04

C      IF(LANGUE==1) WRITE(67,10) NETO
C      IF(LANGUE==2) WRITE(67, 6) NETO
C      IF(ITERA.LT.ITER1) THEN	!13.12.05
C	WRITE(67,418)ITERA	
C	ELSE				    !13.12.05
C	    WRITE(67,*)'Résultats après itération',ITERAM     !13.12.05
C	 ENDIF                             !13.12.05

C -------------------------------------------------------------
c 8.2  Impression des parametres de couts
c     -------------------------------------
      IF(IOPTI.GE.1)THEN
	   IF(IMULTI.EQ.1)THEN							!10.10.2007 (eugen)
			WRITE(666,429)	
	   ELSE  										
		   IF(ICOUT.EQ.(-1)) THEN					!obj inertie
c			WRITE(*  ,428)
			WRITE(67 ,428)
			 WRITE(666,428)
		   ENDIF
		   IF(ICOUT.EQ.0)THEN
c			WRITE(*  ,421)
			 WRITE(67 ,421)
			 WRITE(666,421)
		   ENDIF
		   IF(ICOUT.GE.1)THEN
c			WRITE(*  ,422)
			 WRITE(67 ,422)
			 WRITE(666,422)
		   ENDIF
		ENDIF
	ENDIF											!obj inertie
	IF(IMPR2.GE.0) THEN								!15.10.05
	WRITE(67,423) REND,EQP,Dref,DrefX,DrefY,C1,C2,C3,DC1,DW2,DW3
	WRITE(67,424) P10,DP10,P4,DP4,P5,DP5
	WRITE(67,425) P9X,P9Y,DP9X,DP9Y
	WRITE(67,426) P6,P7,BER,BET
	WRITE(67,427) C8,DC8,IALR,IALT
	ENDIF											!15.10.05

c------------------------------------------------------------
C 8.3 Initialisation de IFONCT
C     -------------------------
      IFONCT(1)=1      ! IFONCT(terme impair)=1 pour termes 1,3,7,..
      IFONCT(2)=0      ! IFONCT(terme pair  )=0 pour termes 2,4,6,...
C        Si cas de charges symétriques selon X
C           IFONCT(terme impair)=1 pour termes 1,3,7,..
C           IFONCT(terme pair  )=0 pour termes 2,4,6,...
C        Si cas de charges non symétriques selon X
C           IFONCT =1 pour tous les termes.    


C **********************************************************************
C **********************************************************************

C 9.0 LECTURE DES DONNEES RELATIVES A CHAQUE PANNEAU ET CALCUL DES      
C     GRANDEURS CARACTERISTIQUES   (subr. ENT)                                     
C     ============================================================ 
      IFF  =0        ! IFF=1 Si error dans ENT
      ICHAG=0        ! ICHA  Indice de charges variables selon OX
      POT  =0.
      NTOT =0
      M1TOT=0
      M2TOT=0
	Spoids1 = 0			!05.12.05
      CALL ANNULI(M1TABL,1000)

      IF(LANGUE==1) WRITE(*,*)'LECTURE DES DONNEES (Subr. ENT)'
      IF(LANGUE==2) WRITE(*,*)'Reading the data (Subr. ENT)'
      WRITE(*,*)

      DO 2242 NEL=1,NETO
 
        M1CONT(NEL)=0

	  CALL ENTS(NEL,NETO,WIDTH,FAM,PHILN,QN,IBUSC,DESSIN,ICHA(NEL),
     *  POT,IPOIDS,IMPR,ITERA,IPRINT,NVAR(NEL),ITYPE(NEL),ISECT(NEL),	! février 2004
     *  M1CONT(NEL),M2CONT(NEL),
     *  NXIT(1,NEL),IPTS,YPTS,IPTS2,IPTS3,
     *  NSOLM,NSOL,NNSOL,IFF,
     *  PLOC(NEL),CHA,dCHA,NPT,LM2,TETAQ,TETAS,DELT,DELT2,ANGLE,NOH,	!!!aout04
     *  NSIGN,E,ETA,SIGY,SIGM,SPEC,XICOU,TFA,TFR,MODES,               !!!aout04
     *  XIMIN,XIMAX,TRAV,V,M1TABL,
     *  PART,CORRO,IMPR2,Spoids1,IANA)		!05.12.05		!coefficient de participation  + !corrosion  !15.10.05  !r&d14

     	  IF(ITERA.EQ.0) THEN			!13.12.05

	    OPEN(100+NEL,STATUS='SCRATCH',FORM='UNFORMATTED')
	    OPEN(700+NEL,STATUS='SCRATCH',FORM='UNFORMATTED') ! pour sauver ZSN dans BO1

          IF(IOPTI.GE.1) THEN

          OPEN(400+NEL,STATUS='SCRATCH',FORM='UNFORMATTED')	!extension neto

	    ENDIF
	  ENDIF
        IF(ICHA(NEL).EQ.1) ICHAG=1   ! ICHA indice de charge variable selon OX
 2242 CONTINUE 
 
      IF(IFF.EQ.1)  THEN
	  WRITE(*,*) 'Some errors in your data'
	  WRITE(29,*) 'subroutine Main'												!bug
	  WRITE(29,*) 'Some errors in your data'									!bug
	  PAUSE 'STOP'
	  STOP   
	ENDIF  

  	REWIND 302			!extension neto
	REWIND 99			!r&d14
      IF(IOPTI.GE.1) REWIND 78  ! fichier avec liste des restriction CJmax, LCONT, INV
C ---------------------------------------------------------------------
C 9.1 Impressions des données : Bornes des XI et CJmax des restrictions
C     ================================================================
c	IF((IPRINT.GE.1).AND.(ITERA.EQ.1)) THEN
c        WRITE(*,*)'NTOT=',NTOT,'; M1TOT=',M1TOT,' et M2TOT=',M2TOT
c        WRITE(*,*)'XIMIN < XI < XIMAX'
c        WRITE(*,*)'XIMIN'
c        WRITE(*,'(9E11.4)')(XIMIN(I),I=1,NTOT)
c        WRITE(*,*)'XI' 
c        WRITE(*,'(9E11.4)')(XICOU(I),I=1,NTOT)
c        WRITE(*,*)'XIMAX'
c        WRITE(*,'(9E11.4)')(XIMAX(I),I=1,NTOT)
c        WRITE(*,*)'CJMAX'
c        WRITE(*,'(9E11.4)')(CJMAX(I),I=1,M1TOT)
c        PAUSE 'OK?'
c      ENDIF

      POT=POT*WIDTH+Spoids1				!05.12.05
      IF(LANGUE==1) WRITE(66,405)POT
      IF(LANGUE==1) WRITE(67,405)POT		!05.12.05

      IF(LANGUE==2) WRITE(66,'(/2A,E11.4,2A/)')'TOTAL WEIGHT of the ',
     * 'STRUCTURE = ',POT,' N (If IPA=1, reduced specific weight is ',
     * 'considered due to buoyancy force)'

C10.0 TRANSFORMATIONS DES DONNEES (Subr. MODIF et COORD)
C     ===================================================
c     Subr COORD: Restictions sur Centre de gravité,

	IF(LANGUE==1)  THEN
	 WRITE(*,*)
       WRITE(*,*)'TRANSFORMATIONS DES DONNEES, Subr. PMODIF et PCOORD'
	ENDIF

	CALL MODIF(NETO,NOH,NOEUD,MCOMP,NNO,NC,NSIGN,ITRAV,NCONDI)	!flexion d'axe vert.

      IMG=0  ! Compteur des restr. Struct (IMG= IMG+2 si 2 rest. sur centre de gravité)

	CALL COORD(NETO,PHILN,QN,IMPR,TETAS,DELT,DELT2,
     *           XNEUT,YNEUT,Z,NOEUD,NVAR,NXIT,IMG,
     *           A,ITRAV,ITYPE,ISECT,IMPR2,								!15.10.05 ! A et ITRAV  vecteur de travail dans coord	!février 2004
     *		   NCONDI,NNO,SYMX,SYMY,INERT,IMOD,IXXTOT,OMET,SYM,			!flexion d'axe vert.	!restri inertie	   !restri module	!obj inertie   
     *		   PART,IXXTOTPART,YNEUTPART)								!r&d13  !r&d13_tranchant

      REWIND 42
      REWIND 45


C	RESTRICTION SUR POIDS		!restri poids
C	=====================

	READ(55,4) TEXT				
      READ(55,*) IWEIGHT,WMAX		

	IF((IOPTI.NE.0).AND.(IWEIGHT.NE.0)) THEN
	  WEIGHT=0.
	  CALL ANNULD(DW,NTOT)
	  NCONT=0
	  DO NEL=1,NETO
          NBRXI=NVAR(NEL)
          CALL OBJPD1(NEL,NETO,NTOT,NCONT,NXIT(1,NEL),NBRXI,WEIGHT,DW,
     *		WIDTH,0,SPEC(NEL),ITYPE(NEL),ISECT(NEL),CORRO)		
          NCONT=NCONT+NBRXI
	  ENDDO
	  WRITE(66,'(//A/1x,22(1H*))')
     *    ' RESTRICTION SUR POIDS'
	  WRITE(666,'(//A/1x,22(1H*))')
     *	' RESTRICTION SUR POIDS'
	  WRITE(66,'(A,T17,E14.7,T33,A)') ' Poids actuel = ',WEIGHT,'N'
	  WRITE(666,'(A,T17,E14.7,T33,A)') ' Poids actuel = ',WEIGHT,'N'
	  WRITE(66,'(A,T17,E14.7,T33,A)') ' Poids maximum = ',WMAX,'N'
	  WRITE(666,'(A,T17,E14.7,T33,A)') ' Poids maximum = ',WMAX,'N'
        COM='POIDS < WMAX'
	  C=WEIGHT					
	  CM=WMAX						
	  WRITE(303) C,CM,COM
	  WRITE(303) (DW(I),I=1,NTOT)	
	  IMG=IMG+1						! +1 au compteur des restrictions struc
        IF(IOPTI.GT.2) THEN
		IF(IMPR2.NE.-3) THEN			
	    WRITE(666,*) 
	    WRITE(666,*) 'C,CM,TEXT=',C,CM,'  ',COM
          WRITE(666,*) 'DC après write 303:'	
          WRITE(666,'(9(E10.3,1X))') (DW(I),I=1,NTOT)
		ENDIF			
	  ENDIF
	  REWIND 302
	ENDIF


C	RESTRICTION SUR COUT		!restri cout
C	=====================

	READ(55,4) TEXT				
      READ(55,*) IPRICE,PRICEMAX,ICOST		

	IF((IOPTI.NE.0).AND.(IPRICE.NE.0)) THEN
	  PRICE=0.
	  CALL ANNULD(DPRICE,NTOT)
	  INITCOUT=ICOUT
	  IF(ICOST.EQ.1) THEN
	    ICOUT=ICOST
		NCONT=0
	    DO NEL=1,NETO
            NBRXI=NVAR(NEL)
            CALL OBJCT1(NEL,NETO,NTOT,NCONT,NXIT(1,NEL),NBRXI,PRICE,
     *				  DPRICE,WIDTH,IPRINT,SPEC(NEL),ITYPE(NEL),
     *				  ISECT(NEL),CORRO)		
            NCONT=NCONT+NBRXI
	    ENDDO
	  ENDIF
	  IF(ICOST.GT.1) THEN
	    ICOUT=ICOST
		CALL SENSIBCOUT(DPRICE,NETO,PRICE,NVAR,NTOT,NXIT,SPEC,WIDTH,     
     *                    ISECT,CORRO)
	  ENDIF
	  ICOUT=INITCOUT	  
	  WRITE(66,'(//A/1x,21(1H*))')
     *    ' RESTRICTION SUR COUT'
	  WRITE(666,'(//A/1x,21(1H*))')
     *	' RESTRICTION SUR COUT'
	  WRITE(66,'(A,T17,E14.7,T33,A)')
     *	' Cout actuel = ',PRICE,'Euro ($ or ...)'
	  WRITE(666,'(A,T17,E14.7,T33,A)')
     *	' Cout actuel = ',PRICE,'Euro ($ or ...)'
	  WRITE(66,'(A,T17,E14.7,T33,A)')
     *	' Cout maximum = ',PRICEMAX,'Euro ($ or ...)'
	  WRITE(666,'(A,T17,E14.7,T33,A)')
     *	' Cout maximum = ',PRICEMAX,'Euro ($ or ...)'
        COM='PRICE < PRICEMAX'
	  C=PRICE					
	  CM=PRICEMAX						
	  WRITE(303) C,CM,COM
	  WRITE(303) (DPRICE(I),I=1,NTOT)	
	  IMG=IMG+1									! +1 au compteur des restrictions struc
        IF(IOPTI.GT.2) THEN
		IF(IMPR2.NE.-3) THEN			
	    WRITE(666,*) 
	    WRITE(666,*) 'C,CM,TEXT=',C,CM,'  ',COM
          WRITE(666,*) 'DC après write 303:'	
          WRITE(666,'(9(E10.3,1X))') (DPRICE(I),I=1,NTOT)
		ENDIF			
	  ENDIF
	  REWIND 302
	ENDIF


C11.0 MOMENTS d'EXTREMITES   (Subr. BATEAU)
C     ======================================
      READ(55,4)TEXT
      READ(55,*) IMOM

      IF (IMOM.EQ.0) GOTO 2314
      WRITE(*,*)'Subr. BATEAU (Primary Hull Bending Moments)'
      WRITE(66,5241)TEXT
      READ (55,*) YRED
      WRITE(66,2313)YRED

      ICO=0
	IF(ITERA==0) THEN				!13.12.05
	IF((DABS(1.0-YRED).GT.0.001).or.(JLPH==1)) THEN
       WRITE(*,*)
       WRITE(*,*)'You are applying a reduction factor',
     *           '              at the extremity bending moments.'
       WRITE(*,*)' YRED =',YRED
       WRITE(*,*)
       WRITE(*,*)'Is it correct ?'
       WRITE(*,*)' If YES, type 1 '
       WRITE(*,*)' If NO,  type 0  The application is stopped.'
       WRITE(*,*)'                 You change the factor in date file'
       WRITE(*,*)'                 YRED = 1.0 is recommended'
 	 READ(*,*) ICO
	  IF (ICO.NE.1) THEN
	   STOP
	  ELSE
         WRITE(*,*)'The specified reduction factor is used: YRED =',YRED
         PAUSE 'OK?'
	  ENDIF
	ENDIF
	ENDIF

      IS=0
      DO 733 I=1,NSOLM
        ICC=0
        DO K=1,NSOL
          IF (NNSOL(K).EQ.I) ICC=1   ! ICC=1 cas de charge retenu
	  ENDDO
        IF (ICC.EQ.0) THEN           ! ICC=0 cas de charge pas retenu
          READ(55,*,END=900) TEMP
          GOTO 733
	  ELSE
	    IS=IS+1
	  ENDIF
        READ (55,*) BM1(IS),SF1(IS),BM3(IS),SF3(IS)     !nov2003	!eugen
        IF (NETO.EQ.1) THEN												!flexion d'axe vert.
		WRITE(66,*)'This particular case (Only one panel) is not		
     *treated!'
		WRITE(29,*)'This particular case (Only one panel) is not				!bug	
     *treated!'
          WRITE(*,*)'This particular case (Only one panel) is not		
     *treated!'															!flexion d'axe vert.
		STOP															!flexion d'axe vert.
        ENDIF																!flexion d'axe vert.
	  IF (((BM1(IS).NE.(0.)).AND.(SYMX.NE.0)).
     *      OR.((BM3(IS).NE.(0.)).AND.(SYMY.NE.0))) THEN					!eugen
	    WRITE(66,*)'It is not possible to consider a bending moment
     * around an axis of symmetry!'
          WRITE(*,*)'It is not possible to consider a bending moment
     * around an axis of symmetry!'
	    WRITE(29,*)'It is not possible to consider a bending moment
     * around an axis of symmetry!'												!bug
	    STOP
	  ENDIF
	  WRITE (66,2315) NNSOL(IS),BM1(IS),BM3(IS),BM1(IS),BM3(IS)	        !flexion d'axe vert.	!eugen	  
	  BM1(IS)=BM1(IS)/YRED											    !flexion d'axe vert.
	  BM2(IS)=BM1(IS)/YRED											    !flexion d'axe vert.	!eugen
	  BM3(IS)=BM3(IS)/YRED											    !flexion d'axe vert.
	  BM4(IS)=BM3(IS)/YRED											    !flexion d'axe vert.	!eugen
  733 CONTINUE

	IF(IANA.EQ.2) GOTO 2500												!r&d14

C     Pour utiliser les termes pairs et impairs
      IFONCT(1)=1  
      IFONCT(2)=1  

      CALL BATEAU(BM1,BM2,BM3,BM4,DZSN,NETO,PHILN,QN,NSOL,				!flexion d'axe vert.
     *            TETAS,DELT,XNEUT,YNEUT,Z,SYMX,SYMY,ITYPE,IMPR2)			!15.10.05

C     Sauvetage dans Sbr Bateau des forces de Bord "ABCD" sur File 97 (pour relecture dans BO1)

 2314 CONTINUE



C	MOMENTS D'EXTREMITE en tenant compte des coefficients de participation
c     ======================================================================
	
	DO 25 NEL=1,NETO
	  IF(ITYPE(NEL).EQ.5) GOTO 25														!coefficient de participation
	  IF(PART(NEL).NE.(1.0)) THEN
	    IPART=1
	    GOTO 21
	  ENDIF
   25	CONTINUE
c
	DO IS=1,NSOL
	  IF((BM3(IS).NE.(0.)).OR.(BM4(IS).NE.(0.))) THEN
	    WRITE(66,*) 'The coefficients of participation are not taken
     * taken into account if a bending moment around a vertical axis is
     * considered'
          WRITE(*,*) 'The coefficients of participation are not taken
     * taken into account if a bending moment around a vertical axis is
     * considered'
	    WRITE(29,*) 'The coefficients of participation are not taken					!bug
     * taken into account if a bending moment around a vertical axis is					!bug
     * considered'																		!bug
	    PAUSE
          GOTO 22
	  ENDIF
	ENDDO 

   21	IF((IMOM.NE.0).AND.(IPART.EQ.1)) THEN
        WRITE(*,*)'Subr. PARTICIPATION (Coefficient of participation)'
	  WRITE(66,5242)
	  CALL PARTICIPATION(NETO,ITRAV,PART,PHILN,QN,SYMY,Z,DELT,TETAS,
     *                     YNEUT,BM1,BM2,NSOL,ITYPE,IMPR2)			!15.10.05
	ENDIF																!coefficient de participation


C	REARRANGEMENT DU FICHIER DE TRAVAIL 97
c     =======================================
   22	IF(IMOM.NE.0) THEN
        REWIND 97															!coefficient de participation
	
  	  DO IS=1,NSOL
	    DO 23 NEL=1,NETO
	      IF(ITYPE(NEL).EQ.5) GOTO 23
		  READ(97) ABCD
		  DO I=1,8
		    COEF(NEL,I,IS)=ABCD(I)
	      ENDDO
   23	    CONTINUE
	  ENDDO
	 
	  REWIND 97
	
	  DO 24 NEL=1,NETO
	    IF(ITYPE(NEL).EQ.5) GOTO 24
	    DO IS=1,NSOL
	      DO I=1,8
	        ABCD(I)=COEF(NEL,I,IS)
	      ENDDO
	      WRITE(97) ABCD
	    ENDDO
  24	  CONTINUE
      ENDIF																!coefficient de participation
	    


C12.0 LECTURE DES RESTRICTIONS D'EGALITE (et impression)
C     =================================================
 2500 READ(55,4,END=900)TEXT											!r&d14
      READ(55,*,END=900)NEGALT
      DO 1333 I=1,NEGALT
        READ(55,*,END=900) (MEGA(I,K),K=1,4),EGA(I)
c       WRITE(666,*)       (MEGA(I,K),K=1,4),EGA(I)
 1333 CONTINUE

      IF((IOPTI.GE.1).AND.(ITERA.EQ.0)) THEN		!13.12.05
        WRITE(*,*)'SUBROUTINE SBEGA'

        CALL SBEGA(TEXT,NVAR,NXIT,NETO,
     *             MEGA,EGA,NVS,NXI2,NEGAL,NEGALT,NGmax,IMPR2)		!15.10.05
      ENDIF

C13.0 DONNEES POUR LE CALCUL DE LA RESISTANCE ULTIME DE LA POUTRE NAVIRE
C     ==================================================================
c     Mise à zéro des sections du Deck,Fonds et Sides
      CALL ANNULD(BIDONC,14)

      CALL HULL(ITERA,NETO,SIGY)


C **********************************************************************
C **********************************************************************

	IF(IANA.EQ.2) THEN													!r&d14

	  OPEN(68,FILE=NOM1//RES4)    ! Listing resultats
	  IF (ITERA.LT.ITER1)THEN
	    WRITE(68,'(T40,A,T50,I2/,T40,12(1H=))') 'ITERATION',ITERA
	  ELSEIF (ITERA.EQ.ITER1) THEN
	    WRITE(68,'(T40,A,T50,I2/,T40,12(1H=))') 'ITERATION',ITERAM
	  ENDIF

	  WRITE(68,'(//A)') '!!!STRUCTURAL ANALYSIS USING BEAM THEORY!!!'

C	NEUTRAL AXIS AND MOMENT OF INERTIA (GROSS, NET and PLATE SCANTLING)
C	===================================================================
	
	  WRITE(68,'(///A/,34(1H-)/)')
     *  '/NEUTRAL AXIS AND MOMENT OF INERTIA/'

c	Net Scantling
c	-------------
	  CALL INERTIA(NETO,Z,PHILN,TETAS,ITYPE,PART,DELT,CORRO,SYM,
     *			   NVAR,NXIT,IOPTI,YNEUTNET,dYNEUTNET,INET,dINET,0)

c	Gross Scantling
c	---------------
	  CALL INERTIA(NETO,Z,PHILN,TETAS,ITYPE,PART,DELT,CORRO,SYM,
     *			   NVAR,NXIT,IOPTI,YNEUTGRO,dYNEUTGRO,IGRO,dIGRO,1)

	  WRITE(68,'(T15,A,T33,A/,48(1H-))')
     *  'Net Scantling','Gross Scantling'
	  WRITE(68,'(A,T18,F6.3,T37,F6.3)')'Yneut [m]',YNEUTNET,YNEUTGRO
	  WRITE(68,'(A,T17,F8.3,T36,F8.3)')'Ixx [m4]',INET,IGRO

c	Plate Scantling
c	---------------
	  DO NEL=1,NETO
	    READ(302) KSA,KSR,KSE,EPSA,EPSR,THICKNET(NEL)
	    THICKGRO(NEL)=THICKNET(NEL)+CORRO(NEL,1)
	  ENDDO
	  REWIND 302
c	  Net Scantling	  
	  CALL INERTIA(NETO,Z,PHILN,TETAS,ITYPE,PART,THICKNET,CORRO,
     *			   SYM,NVAR,NXIT,IOPTI,YNEUTPLNET,dYNEUTPLNET,
     *			   IPLNET,dIPLNET,0)
c	  Gross Scantling
	  CALL INERTIA(NETO,Z,PHILN,TETAS,ITYPE,PART,THICKGRO,CORRO,
     *			   SYM,NVAR,NXIT,IOPTI,YNEUTPLGRO,dYNEUTPLGRO,
     *			   IPLGRO,dIPLGRO,0)


C	STRUCTURAL ANALYSIS BASED ON BEAM THEORY							
C	========================================
	  
c	Hull girder shear stresses
c	-------------------------- 

c	  Net Scantling
c	  -------------
c        et=timef()
        CALL SHEAR(NETO,NOEUD,NNO,Z,PHILN,QN,ANGLE,E,ETA,ITYPE,PART,
     *             NVAR,NXIT,IOPTI,THICKNET,XNEUT,YNEUTPLNET,
     *			 dYNEUTPLNET,IPLNET,dIPLNET,A,B,KLI,BLOKA,TRAV,
     *			 TAUNET,dTAUNET)
c	   et1=timef()				
	  
c	  Gross Scantling
c	  ---------------
c        et=timef()
        CALL SHEAR(NETO,NOEUD,NNO,Z,PHILN,QN,ANGLE,E,ETA,ITYPE,PART,
     *             NVAR,NXIT,IOPTI,THICKGRO,XNEUT,YNEUTPLGRO,
     *			 dYNEUTPLGRO,IPLGRO,dIPLGRO,A,B,KLI,BLOKA,TRAV,
     *			 TAUGRO,dTAUGRO)
c	   et1=timef()				

	  
	  WRITE(68,'(///A/,32(1H-))') '/SHEAR STRESS AND NORMAL STRESS/'

	  DO IS=1,NSOL
	    WRITE(68,'(/A,T12,I2/,14(1H*))') '/Load case/',IS
		WRITE(68,'(T42,A,T84,A/,106(1H-))')
     *    'Net Scantling','Gross Scantling'
	    WRITE(68,'(A,T12,A,T33,A,T46,A,T60,A,T75,A,T88,A,T102,A)/')
     *    'Panel','Stress [N/mm2]','Start','Center',
     *    'End','Start','Center','End'

		DO NEL=1,NETO
		  IF(ITYPE(NEL).NE.5) THEN

c	Hull girder bending stresses
c	----------------------------

		    CALL IMPRTAU(TAUNET,TAUGRO,NEL,NETO,SF1(IS))											  						

c	  Net Scantling
c	  -------------
c	        et=timef()
	        CALL BENDING(NEL,NETO,Z,PHILN,TETAS,ITYPE,PART,DELT,
     *				     BM1(IS),NVAR,NXIT,IOPTI,YNEUTNET,dYNEUTNET,
     *				     INET,dINET,SIGNET,dSIGNET)
c	        et1=timef()

c	  Gross Scantling
c	  ---------------
c	        et=timef()
	        CALL BENDING(NEL,NETO,Z,PHILN,TETAS,ITYPE,PART,DELT,
     *				     BM1(IS),NVAR,NXIT,IOPTI,YNEUTGRO,dYNEUTGRO,
     *				     IGRO,dIGRO,SIGGRO,dSIGGRO)
c	        et1=timef()

		    WRITE(68,'(1x,I3,T15,A,T31,F8.3,T57,F8.3,
     *	    T73,F8.3,T99,F8.3)') NEL,'/Sx/',(SIGNET(1)/1.E+06),
     *		(SIGNET(2)/1.E+06),(SIGGRO(1)/1.E+06),(SIGGRO(2)/1.E+06)

c	Stiffener bending stresses
c	--------------------------
			READ(302) KSA,KSR,KSE,EPSA,EPSR,DELTA,HYA,DYA,WYA,TYA,		
     *		          HXR,DXR,WXR,TXR,PHIL,Q,EPAIS,EPSA2

			IF(HXR.GT.0.010) THEN
			  CALL STIFBEND(NEL,NETO,KSR,KSE,EPSA,EPSR,DELTA,
     *					    HXR,DXR,WXR,TXR,EPSA2,IS,TRAV,NSOL,
     *					    IOPTI,SIGSTIF,dSIGSTIF)
		    ELSE
			  SIGSTIF=0.
			  READ(99)
			ENDIF

			WRITE(68,'(1x,I3,T12,A,T44,F8.3)') NEL,'/Sx_stiff/',
     *		(SIGSTIF/1.E+06)
     
			
C	SCANTLING OPTIMIZATION
C	======================

			IF(IOPTI.GE.1) THEN

			  IF(EPSA2.GE.(0.00001)) THEN
			    BACKSPACE(302)
	            READ(302) KSA,KSR,KSE,EPSA,EPSR,DELTA,HYA,DYA,WYA,TYA,		
     *		              HXR,DXR,WXR,TXR,PHIL,Q,EPAIS,EPSA2,
     *				      HYA2,DYA2,WYA2,TYA2
			  ENDIF


c	IACS requirements
c	=================

			  DO I=1,M1TABL(NEL,IS)
			    READ(78) IC,IY,INVV,CMAX
				IF(IC.EQ.10) THEN
				  CM=INVV*CMAX/SIGM(NEL)								!r&d15
				  COM='El.bord.flex'
				  CALL BENDYIELD(NEL,NETO,NVAR,NXIT,SIGGRO,dSIGGRO,
     *							 NTOT,CIACS,dCIACS)
	            ELSEIF(IC.EQ.14) THEN
c		          et=timef()
				  CM=INVV*CMAX
	              COM='Fl.bord.comp'
				  CALL COMPBUCK(NEL,NETO,E(NEL),SIGY(NEL),SIGM(NEL),
     *				            PHILN,NVAR,NXIT,
     *						    EPSA,EPSR,DELTA,HXR,DXR,WXR,TXR,Q,
     *				            EPSA2,WYA2,SIGNET,dSIGNET,TRAV,
     *						    NTOT,CIACS,dCIACS)
c		          et1=timef()
			    ELSEIF(IC.EQ.19) THEN
				  CM=INVV*CMAX/SIGM(NEL)								!r&d15
	              COM='El.bord.cis'
				  CALL SHEARYIELD(NEL,NETO,NVAR,NXIT,TAUGRO,dTAUGRO,
     *							  SF1(IS),NTOT,CIACS,dCIACS)
				ELSEIF(IC.EQ.20) THEN
c		          et=timef()
				  CM=INVV*CMAX
	              COM='Fl.bord.cis'
				  CALL SHEARBUCK(NEL,NETO,E(NEL),SIGY(NEL),Q,PHILN,
     *							 NVAR,NXIT,EPSA,EPSR,DELTA,HXR,EPSA2,
     *							 TAUNET,dTAUNET,SF1(IS),TRAV,NTOT,
     *							 CIACS,dCIACS)
				  CONTINUE
c		          et1=timef()
				ELSEIF(IC.EQ.37) THEN
c		          et=timef()
				  CM=INVV*CMAX
	              COM='Fl.raid.'
				  CALL STIFBUCK(NEL,NETO,E(NEL),SIGY(NEL),SIGM(NEL),
     *							PHILN,NVAR,NXIT,
     *							EPSA,EPSR,DELTA,HXR,DXR,WXR,TXR,Q,
     *							EPSA2,WYA2,SIGNET,dSIGNET,TRAV,
     *							NTOT,CIACS,dCIACS)
c		          et1=timef()
				ELSEIF(IC.EQ.38) THEN
c		          et=timef()
				  CM=INVV*CMAX/SIGM(NEL)								!r&d15
	              COM='El.raid.'
				  CALL STIFYIELD(NEL,NETO,SIGM(NEL),NVAR,NXIT,
     *							 SIGNET,dSIGNET,SIGSTIF,dSIGSTIF,
     *							 NTOT,CIACS,dCIACS)
c		          et1=timef()
				  CONTINUE
				ENDIF
				WRITE(303) CIACS,CM,COM
				WRITE(303) (dCIACS(J),J=1,NTOT)
			  ENDDO
			ENDIF
		  ELSE
		    READ(302)
			READ(99)
		  ENDIF
	    ENDDO
	    REWIND 302
		REWIND 99
	  ENDDO

	  REWIND 78

	  GOTO 2501
	
	ENDIF																!r&d14								

      
C **********************************************************************
C **********************************************************************

C14.0 MISE EN FORME DES EQUATIONS DE CONTINUITE ET RESOLUTION (BO1)
C     ==================================================================
C     BOUCLE EXTERNE SUR LES TERMES DE LA SERIE DE FOURIER (boucle 2201)
C    14.1  BOUCLE INTERNE (BO1) SUR LES PANNEAUX et sur les cas de charges,
C          Structure bi-appyée           
C    14.2  BOUCLE INTERNE (CO1/CO4) pour les forces de bords
C    14.3  Calcul des sensibilités (MDR2)
C           

      IF(LANGUE==1) WRITE(*,407)
      IF(LANGUE==2) WRITE(*,406)


      DO 2201 IJK=1,JLPH  !JLMAX ************

        IF(MOD(IJK,2).EQ.1) THEN  
	    ITERM=1   ! ITERM = 1 si IJK est impair
          IBAT =1   ! 'Pour le terme ',IJK,'IBAT=1'
	  ELSE       
          ITERM=2   ! ITERM = 2 si IJK est pair
	    IBAT=0    ! (terme pair)
          IF(JLPH2.LE.0) THEN  ! JLPH negatif, seul les termes impairs sont considerés
            IFONCT(ITERM)=0  ! skip termes pairs 
	      GOTO 2201
	    ENDIF
          IF((IMOM.NE.0).or.(ICHAG.NE.0).or.(IBUSC.NE.0)) THEN 
		  !IMOM =0  Flexion d'ensemble --> pas besoin de calculer les termes pairs
		  !ICHAG=0  Charge selon X     --> pas besoin de calculer les termes pairs
		  !IBUSC=0  Force de buscage   --> pas besoin de calculer les termes pairs
            GOTO 555  ! il faut utiliser les termes pairs de la série de Fourier
          ENDIF
          IFONCT(ITERM)=0
		GOTO 2201  ! skip termes pairs         
	  ENDIF
 555    CONTINUE

        IFONCT(ITERM)=1 

        REWIND 97
        REWIND 98
        REWIND 99

        CALL LAM(LAMB,IJK,PI,WIDTH)

        IF(LANGUE==1)  WRITE(*,'(A,I3)')' TERME No ',IJK
        IF(LANGUE==2)  WRITE(*,'(A,I3)')' TERM No ',IJK
                       WRITE(*,*)       '----------------'

        DO NEL=1,NETO
c          IF(LANGUE==1) WRITE(*,'(A,I3)')'    Panneau No ',NEL
c          IF(LANGUE==2) WRITE(*,'(A,I3)')'    Panel No ',NEL
          ICH=ICHA(NEL)
          IF(IPOIDS.EQ.1) ICH=1

          CALL BO1 (NEL,ETA(NEL),ITYPE(NEL),WIDTH,IMPR,IJK,NETO,NE,KLI,
     *              ZSN,A,B,SOLT,JLPH,IBAT,ICH,IMOM,IBUSC,
     *              NVAR(NEL),NXIT(1,NEL),IPTS(NEL),YPTS(1,NEL),NSOL,
     *              BLOKA,DZSN,NPT,
     *              ANGLE,NOEUD,MCOMP,NNO,NC,NSIGN,IS)	!!!aout04
        END DO

c ---------------------------------------------------------------------------------
C 14.2 DETERMINATION DES EFFETS DE BORDS (Structure non bi-appuyée)
C      ------------------------------------------------------------
C     IF(JLBORD.EQ.0) GOTO 30
C     REWIND 9
C     DO 2300 NEL=1,NETO
C         DO  I=10,19
C           REWIND I
C         END DO
C         CALL CO1 (NETO,NEL,JLPH,IMPR2,NE,0,ZSN,SOLT,
C    *              UREF,ALLONG,9,PHIL,MT,TETA)
C         DO 2300 IPAN=1,NETO
C         DO 2300 KK=1,8
C             CALL CO1 (NETO,NEL,JLBORD,IMPR2,NE,IPAN,ZSN,
C    *                  SOLT,UREF,ALLONG,KK,PHIL,MT,TETA)
C2300 CONTINUE
C     WRITE(66,5)
C
C     CALL CO4(ALLONG,UREF,UREEL,A,B,BLOKA,KLI,NETO,E(nel),WIDTH,PHILN,
C    *         QN,ABC,ASOL,BSOL,CSOL,DSOL,AMOM,BMOM,CMOM,DMOM,INDIC)
C
C     IF (INDIC.EQ.0) STOP
c 30  CONTINUE
C ---------------------------------------------------------------------------------

	REWIND 99

C14.3 MODULE D'OPTIMISATION - CALCUL DES SENSIBILITES (Subr. MDR2)
C     ------------------------------------------------------------
C      DZSN(NE,9*NETO,10)sauvé dans ALLONG
C      La taille max de DZSN pour NETO = 100 est 7.2 mio, soit 57.6 MB

      IF(IOPTI.GE.1)THEN
        WRITE(*,417)

        CALL MDR2(NETO,NE,A,DZSN,B,B(1,9),ZSN,PHILN,IMPR,NVAR,NXIT,
     *            IPTS,NSOL,
     *            ANGLE,NOEUD,MCOMP,NNO,NC,NSIGN)
      ENDIF

 2201 CONTINUE       ! fin de la boucle sur les termes de Fourier (JLPH)                                                   

      DO NEL=1,NETO
         REWIND(400+NEL)	!extension neto
         REWIND(700+NEL)	
	END DO

C **********************************************************************
C **********************************************************************

C 15.0 DETERMINATION DES CONTRAINTES ET DEPLACEMENTS (Subr. BO2)
C     =========================================================

	IF(LANGUE==1) WRITE(*,408)
      IF(LANGUE==2) WRITE(*,409)
      IF((ITERA.EQ.0).AND.(IOPTI.GE.1))WRITE(666,512)		!13.12.05

c     IMG= Compteur des Restr. Structurelles général (= 1 ou 2 si Rest Sur centre de Gravité)
      DO 2401 IS=1,NSOL
        IF(LANGUE==1 ) THEN ! French
	     WRITE(*,  '(A,I3)') ' *** Cas de Charge n° ',IS
           WRITE(66,2402) IS
c          IF((ITERA.EQ.1).AND.(IOPTI.GE.1)) WRITE(666,2402)IS
           IF(IOPTI.GE.1) WRITE(666,2402)IS
           WRITE(67,2402) IS
	  ELSE  ! English
	     WRITE(*,  '(A,I3)') ' *** LOAD CASE N° ',IS
           WRITE(66,2403) IS
c          IF((ITERA.EQ.1).AND.(IOPTI.GE.1)) WRITE(666,2403)IS
           IF(IOPTI.GE.1) WRITE(666,2403)IS
           WRITE(67,2403) IS
	  ENDIF

c     Read (in File 78) the list of constraints saved in Subr. ENT 
      IM=0
      IF(IOPTI.GE.1) THEN
         DO IP=1,NETO
c          WRITE(666,*) 'nel=',IP
c          WRITE(666,*) 'M1TABL(IP,IS)=',M1TABL(IP,IS)
           DO I=IM+1,IM+M1TABL(IP,IS)
               READ(78) LCONT(1,I),LCONT(2,I),INV(I),CJMAX(I)
c              WRITE(666,*) LCONT(1,I),LCONT(2,I),INV(I),CJMAX(I)
	      ENDDO
	   IM=IM+M1TABL(IP,IS)
	   ENDDO
c        WRITE(666,'(/A,I3/)')'# Struct Constr. of this load case=',IM
	ENDIF
	
      IM=0             ! Compteur des Restr. Structurelles pour le cas de charges en cours
	rewind(306)			!13.05.04		!extension neto
      DO 2301 NEL=1,NETO
        ICH=ICHA(NEL)
        IF(IPOIDS.EQ.1) ICH=1
        REWIND (100+NEL)
c       WRITE(*,'(A,I3)')'    Panneau-Panel No ',NEL

      CALL BO2(NETO,ITYPE,NEL,JLPH,INDAIG,INDRAI,IMPR2,E(NEL),
     *         ETA(NEL),SIGY(NEL),SIGM(NEL),DIS,NE,FAM,DZSN,ZSN,
     *         WIDTH,SOLT,ABC,ASOL,BSOL,CSOL,DSOL,AMOM,BMOM,CMOM,DMOM,
     *         IFONCT,ICH,IMOM,IPTS(NEL),IPRINT,IS,NSOL,IM,IMG,ITERA,
     *         SENS1,DELT,A,I66,I67,PLOC,
     *         TETAQ,
     *         NVAR,M1TABL(1,IS),M2CONT,NXIT,IPTS,YPTS,IPTS2,IPTS3,
     *         LCONT,CJMAX,INV,NXI2,TRAV,ISECT(NEL),					!février 2004  !r&d13
     *		 Z,PHILN,TETAS,PART,YNEUTPART,SYM,SYMX,BM1(IS),CORRO)	!r&d13

        REWIND (100+NEL)
        REWIND (400+NEL)	!extension neto
        REWIND (700+NEL)


C15.1 SAUVETAGE DES DONNEES GRAPHIQUES
C     ---------------------------------
        IF(DESSIN.EQ.0)GOTO 2301
c        WRITE(*,*) 
c	   IF(LANGUE==1) WRITE(*,*) 'SAUVETAGE DES DONNEES GRAPHIQUES'
c        IF(LANGUE==2) WRITE(*,*) 'SAVE the OUTPUT DATA for the GRAPHICS'
c        WRITE(*,*) 

      CALL SAVE(NEL,INDAIG,INDRAI,A(22),ITYPE(NEL),ISECT(NEL))  ! car A(1) à A(21) utilisés dans BO2
										! A(22) = EFF (=vecteur avec resultats Cfr Subr RESUL)
										! février 2004

 2301 CONTINUE
      IMG=IMG+IM   ! compteur general des restr Struct

	IF(IANA.EQ.2) THEN						!r&d14
	  REWIND 99
	  GOTO 2401						
	ENDIF									!r&d14
	
	REWIND 304		!extension neto
	REWIND 2218								!février 2004

C15.2 Impressions de S ultime panneaux (Paik)
C     --------------------------------------
       IF (IMPR2.GE.-2) THEN				!15.10.05
        IF(LANGUE==2 ) THEN ! English
           WRITE(67,'(/A/40(1H-))')' Ultimate Strength of panels (PAIK)'
	     WRITE(67,'(2A)')'NEL,  Sig Ult (N/mm2) > ? >',  !Sult calculé via Subr. Result
     *        ' Mean Compressive Axial stress, (Nx)'       !Nx   calculé dans Subr CONTR
	  ELSE  ! French
          WRITE(67,'(/A/40(1H-))')' Resist Ultime panneau (PAIK)'
	    WRITE(67,'(2A)')'NEL,  Sig Ult (N/mm2) > ? >',
     *        ' Contrainte moyenne axiale en compression (Nx)'
	  ENDIF
	  DO INEL=1,NETO
	    WRITE(67,'(I3,1x,F10.5,18x,F10.5)')INEL, A(I66+INEL-1)/1.E6,
     *                                            -A(I67+INEL-1)/1.E6
        ENDDO
	ENDIF					!15.10.05

C15.3 Analyse des résultats extrêmes
C     -------------------------------

      CALL ANALYS(NETO,INDAIG,INDRAI,IMPR2,ANALY,IANALY,ITYPE,ISECT)		! février 2004

	
      REWIND 304		!extension neto
      REWIND 99
	REWIND 2218						! février 2004

 2401 CONTINUE


C **********************************************************************
C **********************************************************************

C16.0 MODULE D'OPTIMISATION
C     =======================

 2501 IF(IANA.EQ.2) REWIND 99						!r&d14
      IF(IOPTI.EQ.0) GOTO 304						! goto §16.1)	!13.12.05  !multi obj
	IF(ITERA.GE.ITER1) GOTO 350
	IF ((IOPTI.GE.1).AND.(ITERA.EQ.0))THEN		!13.12.05
	ITERA=1										!13.12.05
c	GOTO 357		!13.12.05	
	ENDIF										!13.12.05
      REWIND 303		!extension neto

c	NV=NTOT
c	M1T=M1TOT
c	M2T=M2TOT
c	MM=M1TOT+M2TOT

        I1=1		         !XI (800)  
	  I2=I1+NVmax		 !FI (800)
	  I3=I2+NVmax  	     !CJ (7000)
	  I4=I3+Mmax	     !CJM(7000)
	  I5=I4+Mmax		 !CIJ(800,7000) et S(ICONLIN)
	
C     Dimension maximale pour le vecteur S(1) utile pour la subr.CONLIN
      ICONLIN = 8*NVmax + 3*Mmax + NVmax*Mmax + 2*MAX0(NVmax,Mmax) + 3
C        = 5641.403   si N=800 et M=7000 (NETO=100)

	  I6=I5 +NVmax*Mmax   !DX(800,800)    
	  I7=I6 +NVmax*NVmax  !Z1(800) à la suite de DX(800,800)
	  I8=I7 +NVmax		  !Z2(800)
	  I9=I8 +NVmax	  	  !Z3(800)
	  I10=I9+NVmax		  !FIMULTI(800)							!multi obj
	  I11=I10+NVmax		  !


c Taille max de AL = 6*NVmax + 2*Mmax + (NVmax)**2 + NVmax*Mmax 				!multi obj
c   NETO=100       = 6*800   + 2*7000 + 800*800    + 800*7000   =7058.000

	  K1=1          !VNOM2(Mmax=700) Character*12

      CALL OPTIS(ITERA,ITERAM,NETO,WIDTH,IPRINT,NSOL,
     *           AL(I1),AL(I2),AL(I3),AL(I4),AL(I5),AL(I5),
     *           AL(I6),AL(I7),AL(I8),AL(I9),AL(I10),AT(K1),SPEC,				!multi obj
     *           NVAR,M1TABL,M1CONT,M2CONT,NXIT,NVARR,NXITR,					!dcn
     *           XICOU,XIMIN,XIMAX,
     *           EGA,MEGA,NXI2,NEGAL,NEGALT,ITYPE,ISECT,						! février 2004	
     *		   MODES,PHILN,QN,CORRO,IMPR2,IWEIGHT,IPRICE,INERT,IMOD,		! janvier 2005  + !corrosion  !15.10.05  + !restri poids  +  !restri cout + !restri inertie  +  !restri module
     *		   IXXTOT,OMET,TETAS,Z,DELT,SYMX,SYM,YNEUTPART,PART,			!obj inertie  !r&d13
     *		   IMULTI,FK,IICOUT,RHO,W1,W2,W3)								!multi obj
C avec ITERA = Numéro de l'itération en cours
C	 M1TOT = Nbre de restrictions structurelles (=M1) pour 1 cas de charge
C	 M2TOT = Nbre de restrictions géométriques  (=M2)

C       1 DELTA = nouvelle épaisseur du bordage
C       2 HYA   = nouvelle hauteur de l'âme des tranversaux (aiguilles)
C       2 HYA   = nouvelle épaisseur de l'âme des tranversaux (aiguilles)
C       4 WYA   = nouvelle largeur des semelles des tranversaux (aiguilles)
C       5 EPSA  = nouvelle entredistance entre tranversaux (aiguilles)
C       6 HXR   = nouvelle hauteur de l'âme des raidisseurs
C       6 HXR   = nouvelle épaisseur de l'âme des raidisseurs
C       8 WXR   = nouvelle largeur des semelles des raidisseurs
C       9 EPSR  = nouvelle entredistance entre raidisseurs
	
	
	ICO=0                                     !multi obj

	IF (IBATCH.NE.0) THEN	!Modif Batch
	  IOPTI = 1				!Modif Batch
	ENDIF					!Modif Batch

      IF(IOPTI.EQ.1) GOTO 357                   ! Iter. automatique
       WRITE(*,*)
       WRITE(*,*) ' VOULEZ RELANCER UNE NOUVELLE ITERATION ?'
       WRITE(*,*) ' OUI=0 (défaut),  NON=1'
       READ(*,*) ICO
 357  CONTINUE

c     To save "updated data" after optimization process

      CALL COPY(ITERA,DON1,LM2,NETO,NVAR,MODES,
     *          NXIT,XICOU,TFA,TFR,XIMIN,XIMAX)				

  350 IF(ICO.NE.1) THEN
	   IF (IBATCH.NE.0) THEN	!Modif Batch
	     REWIND 33				!Modif Batch
	   ENDIF					!Modif Batch
         REWIND 42
         REWIND 43
         REWIND 45
         REWIND 46
         REWIND 78 
	   IF(IBUSC.NE.0) REWIND 95
	   IF(IBUSC.NE.0) REWIND 96
         REWIND 97 
C        REWIND 98 (pas nécessaire car sauvetage des données seulement pour ITERA=1)
  	   REWIND 99
  	   REWIND 301		!extension neto
  	   REWIND 302		!extension neto
         REWIND 303		!extension neto
         REWIND 304		!extension neto
	   REWIND 305		!extension neto					!février 2004
	   REWIND 2218										!février 2004

         ITERA=ITERA+1
         REWIND 55
	   READ(55,*)
	   READ(55,*)				!r&d14
	   READ(55,4) TEXT
         READ(55,301) FGH
	   DO I=1,NSOLM       !nov2003
	     READ(55,*)       !nov2003
	   ENDDO              !nov2003
         CLOSE (68)	!r&d14
	   GOTO 303
c        Vers une nouvelle itération (GOTO 303)
      ENDIF


C16.1 FIN des ITERATIONS (Optimisation), FERMETURE des FICHIERS
C     ---------------------------------------------------------
  304 CLOSE(55)
	IF (IBATCH.NE.0) THEN	!Modif Batch
	  CLOSE(33)				!Modif Batch
	ENDIF					!Modif Batch
      CLOSE(97)
      CLOSE(98)
      CLOSE(99)
      DO  NEL=1,NETO
        CLOSE(100+NEL)        
	  IF(IOPTI.GE.1) CLOSE(400+NEL)	!extension neto
        CLOSE(700+NEL)        
      ENDDO
      CLOSE (301)			!extension neto
      CLOSE (302)			!extension neto
      CLOSE (303)			!extension neto
      CLOSE (304)			!extension neto
	CLOSE (305)			!extension neto		!février 2004
	CLOSE (2218)							!février 2004
      IF(IOPTI.GE.1) CLOSE (666)

      CLOSE (29)			!bug
	CLOSE (66)
      CLOSE (67)
	CLOSE (68)	!r&d14
      CLOSE (77)

C17.0 VISUALISATION GRAPHIQUE (Subr. VISION et DESSIN)
C     ================================================
	
	IF (IBATCH.NE.0) THEN	!Modif Batch
	  DESSIN = 0			!Modif Batch
	ENDIF					!Modif Batch

      IF(DESSIN.EQ.0) GOTO 478

         REWIND 42
         REWIND 43
         REWIND 45
         REWIND 46
c     Pour générer les fichiers dessins contenant déformées et contraintes
      CALL VISION(NETO,WIDTH,Z,IMPR,INDAIG,INDRAI,NSOL,
     *            A,AL,IAL)

         REWIND 42  ! (Rewind 43 pas utile)
         REWIND 45
         REWIND 46
c     Pour générer les fichiers dessins relatifs aux données (bordé, cadres,...)
      CALL VISIONN(NETO,WIDTH,Z,IMPR,NSOL,
     *             A,AL,IAL)

C	For QW Drawings
	  OPEN (23,FILE=NOM1//NOM//'INDICE.DAT')
c	  OPEN (23,FILE='INDICE.DAT')
	  WRITE(23,*) INDAIG,INDRAI,NETO,NSOL
	  CLOSE(23)

  478 CONTINUE

      CLOSE (42) ! données pour dessin
      CLOSE (43) ! données pour dessin
      CLOSE (45) ! données pour dessin
      CLOSE (46) ! données pour dessin

C18.0 Si exécution en série de plusieurs fichiers de données (IAUTO= 1 ou 2)
C     ======================================================================
      IF(IAUTO.NE.0) THEN  ! cas de n analyses 

        GOTO 902 ! goto vers la lecture d'un nouveau fichier de données (nouvelle structure)

      ENDIF


  901 RETURN
C ======================================================================
  900 WRITE(*,*) 'Error à la lecture des restrictions d''égalité'
	WRITE(29,*) 'Error à la lecture des restrictions d''égalité'					!bug
      PAUSE 'STOP'
      STOP

  906 WRITE(*,*) 'STOP : The "Boss.txt" File is missing'		!Modif Batch
      WRITE(*,*) '     : (Le fichier "Boss.txt" manque).'		!Modif Batch
  903 WRITE(*,*) 
      WRITE(*,*) 'STOP : Le NOM du Fichier de données est INCORRECT,'
      WRITE(*,*) '     : ou n''existe pas dans la directory.'
      WRITE(*,*) 'Attention: en cas d''analyses multiples, chaque',
     * 'fichier de données doit etre placé dans une directory STRUC-xx.'
      WRITE(*,*) 'cad: respectivement dans STRUC-a,STRUC-b, ..etc.'
      WRITE(*,*) 
      WRITE(*,*) 'STOP : The DATA FILE is not valid or doesn''t exist'
      WRITE(*,*) '       in the right directory'
 	WRITE(29,*) 'STOP : Le NOM du Fichier de données est INCORRECT,'			!bug
      STOP  
  904 WRITE(*,*) 'STOP : The "Atableur.txt" File is missing'
      WRITE(*,*) '     : (Le fichier "Atableur.txt" manque).'
      WRITE(*,*) '       It contains the corrosion thicknesses'
  	WRITE(29,*) 'STOP : The "Atableur.txt" File is missing'						!bug
      STOP  
  905 WRITE(*,*) 'STOP : Le fichier avec la liste des données manque.' 
      WRITE(*,*) '       Cas d''analyse MULTI-STRUCTURES'
	WRITE(29,*) 'STOP : Le fichier avec la liste des données manque.'			!bug
      STOP  
C ======================================================================

C19.0 LES FORMATS
C     ============
    3 FORMAT(1H1,1X,70(1H*)/,
     *       2X,1H*,4X,15A4,4X,1H*/,
     *       2X,1H*,4X,60(1H+),4X,1H*/,
     *       2X,1H*,68X,1H*/,
     *       2X,70(1H*)//)
    4 FORMAT(15A4)
    5 FORMAT(1H1)
    6 FORMAT(///20X,' L.B.R.-5.7  SOFTWARE'/20X,
     * 28(1H=)//' University of LIEGE, 1 Chemin des Chevreuils, B52/3',
     * ' 4000 Liege,Belgium'/' Contact:',
     * ' Dr. Ph. RIGO, Tel:+32-(0)43.66.93.66, Fax:+32-(0)43.66.91.33'/
     * T30,'Email: ph.rigo@ulg.ac.be'////
     * 16X,'ANALYSIS OF A STRUCTURE COMPOSED OF ',I3,' PANEL(S)'/
     * 15X,49(1H+)/)
    7 FORMAT(/' AVEC RESULTATS INTERMEDIAIRES')
    8 FORMAT(/' AVEC RESULTATS FINAUX + VERIFICATIONS')
    9 FORMAT(/' PAS DE RESULTATS dans les TRANSVERSAUX (CADRES).')
   10 FORMAT(///20X,'LOGICIEL DES BORDAGES RAIDIS (L.B.R.-5.6)'/20X,
     * 41(1H=)//' Université de LIEGE, 1 Chemin des Chevreuils, B52/3',
     * ' 4000 Liege,Belgique'/' Contact:',
     * ' Dr. Ph. RIGO, Tel:+32-(0)43.66.93.66, Fax:+32-(0)43.66.91.33'/
     * T30,'Email: ph.rigo@ulg.ac.be'////
     *,16X,'ANALYSE D''UNE STRUCTURE A ',I3,' PANNEAU(X)'/15X,45(1H+)/)
   11 FORMAT(///' UNITES EN METRES ET NEWTONS',//,' NOMBRE DE TERMES',
     * ' DE LA SERIE DE FOURIER POUR LES CHARGES ',I3//
     * ' Efforts de bord Nb et Mb (buscage) --> File: BUSC.txt'/
     * '  IBUSC =',I2,'  ( 0:sans Nb et Mb  et 1 : avec Nb et Mb)'/)
   12 FORMAT(/' COORDONNEES (X) DES 5 SECTIONS OU LES RESULTATS SONT',
     *        ' FOURNIS (en metre)'/T10,5(2X,F9.4))
   13 FORMAT(/' COEFFICIENTS DE CISAILLEMENT DES SEMELLES',
     *          ' DES TRANSVERSAUX',2(F6.3,2X)/
     *      42X,' DES RAIDISSEURS ',2(F6.3,2X)/
     *      42X,' DES TRAVERSES   ',2(F6.3,2X)//)
   14 FORMAT(///' UNITS: METRE, NEWTON and Equivalent',
     *          ' water height (m) for lateral pressure'//
     * ' Number of Terms of the Fourier Series Expansion',
     * ' for Loads (JLPH) =',I3//
     * ' With End forces Nb & End moments Mb defined in File: BUSC.txt'/
     * '  IBUSC =',I2,'  ( 0:without Nb & Mb  et 1 : with Nb & Mb)'/)
  301 FORMAT(17(/),A1)  ! saut de 20 lignes lors de la relecture du fichier de données	!07.06.06   !multi obj
  399 FORMAT(/'LIMITATIONS'/12(1H*)/,
     *  I5,' = Nbre max de panneau (N)'/
     *  I5,' = Nbre max de variables de conception pour',
     *          ' la structure (NV)'/
     *  I5,' = Nbre max  rest. Struct. par panneau (M1)'/
     *  I5,' = Nbre max  rest. géom. par panneau (M2)'/
     *  I5,' = Nbre max  rest. (struct. + Géom/) pour la ',
     *           'structure et par tous les cas de charges (MM) !!'/
     *     '        = SOMME(M1+M2) !!'/
     *  I5,' = Nbre max  rest. égalité (pour la struct.) (NG)'/
     *  I5,' = Nbre max de cas de charges utilisés (IS)'/
     *  5x,'   (Mais il peut y avoir jusqu à 20 cas de charges',
     *     ' dans le jeu de données dont 10 peuvent être utilisés)'/
     *  I5,' = Nbre max de pts de calcul des sensibilités',
     *              ' par panneau (IPT)'/) 
  400 FORMAT(21X,20(1H*))

  405 FORMAT(/' LE POIDS TOTAL DE LA STRUCTURE EST = ',E14.7,' N.',/
     *		'Ce poids reprend la structure principale, secondaire,' !05.12.05
     *		 'les traverses et la corrosion.'/						!05.12.05
     *		 'Avant itération d''optimisation.'/						!13.12.05
     *        ' (!!1/2 structure si symétrique et structure déjaugée'	  !05.12.05
     *		   'si IPA= 1)'/)										 !05.12.05				
  406 FORMAT(/'ASSEMBLING OF THE RIGIDITY MATRIX (Subr. BO1)'/43(1H=))
  407 FORMAT(/'MISE EN FORME DES EQUATIONS DE CONTINUITE ET RESOLUTION',
     *        ' (Subr. BO1)'/50(1H=))
  408 FORMAT(/'DETERMINATION DES CONTRAINTES ET DEPLACEMENTS'/46(1H=))
  409 FORMAT(/'CALCULATION OF THE DISPLACEMENTS & STRESSES'/46(1H=))
  417 FORMAT(/'MODULE OPTIMISATION (Subr. MDR2)')
  418 FORMAT(//30X,'ITERATION nø',I2/30X,20(1H=)/)
  419 FORMAT( ' Cas de charge considérés:'/1x,25(1H=)/ 
     *        ' - Nbre de cas de charge disponibles = ',I2/
     *        ' - Nbre de cas de charge analysés    = ',I2/
     *        ' - Numeros des cas analysés = ',10I3//)
  420 FORMAT( ' Considered load cases:'/1x,22(1H=)/ 
     *        ' - Available  load cases = ',I2/
     *        ' - Considered load cases = ',I2/
     *        ' - Nbr of the considered cases = ',10I3//)
  421 Format(/' La fonction objectif est le POIDS'/1x,33(1H-))
  422 Format(/' La fonction objectif est le COUT' /1x,32(1H-))
  428 Format(/' La fonction objectif est INERTIE' /1x,32(1H-))			!obj inertie
  429 Format(/' La fonction objectif est MULTI' /1x,32(1H-))				!obj multi (10.10.2007 - eugen)
  423 Format(3x,'Rendement global du chantier         =',F8.3/
     *       3x,'Equivalent poids de la main d''oeuvre =',F8.3/
     *       3x,'Epaisseur de référence BORDE (Dref)  =',F8.3,' m'/
     *       3x,'Epaisseur de référence LONG. (DrefX) =',F8.3,' m'/
     *       3x,'Epaisseur de référence CADRE (DrefY) =',F8.3,' m'/
     *      3x,'Coût du matériau au kg, BORDE (C1)   =',F8.3,' Euro/kg'/
     *      3x,'Coût du matériau au kg, LONG. (C2)   =',F8.3,' Euro/kg'/
     *      3x,'Coût du matériau au kg, CADRE (C3)   =',F8.3,' Euro/kg'/
     *       3x,'Variation de C1 par mm de tôle (DC1) =',F8.3/
     *       3x,'Poids compl.pour Membr.longitud.(DW2)=',F8.3/
     *       3x,'Poids compl.pour Membr. Transv. (DW3)=',F8.3/)
  424 Format(3x,'MdO construction du bordé (P10)      =',F8.3,' h-h/m2'/
     *       3x,'Variat. de P10 par mm de bordé (DP10)=',F8.3/
     *       3x,'MdO soudage Long. sur bordé (P4)     =',F8.3,' h-h/m'/
     *       3x,'Variat. de P4 par mm d''âme (DP4)     =',F8.3/
     *       3x,'MdO soudage Transv. sur bordé (P5)   =',F8.3,' h-h/m'/
     *       3x,'Variat. de P5 par mm d''âme (DP5)     =',F8.3/)
  425 Format(3x,'MdO construct. membrures LONG..(P9X) =',F8.3,' h-h/m'/
     *       3x,'MdO construct. membrures CADRE (P9Y) =',F8.3,' h-h/m'/
     *       3x,'Variat. de P9X par mm d''âme (DP9X)   =',F8.3/
     *       3x,'Variat. de P9Y par mm d''âme (DP9Y)   =',F8.3/)
  426 Format(3x,'MdO pour Intersect. Long/Trans (P6)  =',F8.3,
     *                                                 ' h-h/unité'/
     *       3x,'MdO pour Gousset pour Long/Trans (P7)=',F8.3,
     *                                                 ' h-h/unité'/
     *       3x,'Fréq. relat.gousset sur Long. (BétaR)=',F8.3/
     *       3x,'Fréq. relat.gousset sur Trans.(BétaT)=',F8.3/)
  427 Format(3x,'Coût Energie+Consommable(soudure)(C8)=',F8.3,' Euro/m'/
     *       3x,'Var. de C8 par mm de la tôle soudée  =',F8.3/
     *       3x,'Longitudinaux reconstitués (Alpha R) =',I4,' oui=0'/
     *       3x,'Transversaux  reconstitués (Alpha T) =',I4,' non=1'//)

  511 FORMAT('VARIABLES DE CONCEPTION et RESTRICTIONS GEOMETRIQUES'/
     *        52(1H=))
  512 FORMAT(/'RESTRICTIONS'/14(1H=))

 2313 FORMAT(/'Le coefficient de réduction est ',F6.4)
 2315 FORMAT('Cas de charge nø',I2' LES MOMENTS D''EXTREMITES',
     *  ' (non réduits) SONT:'/
     *'A GAUCHE BMX1= ',E14.7,' N.M'/,'         BMY1= ',E14.7,' N.M'/,
     *'A DROITE BMX2= ',E14.7,' N.M'/,'         BMY2= ',E14.7,' N.M'/)			!flexion d'axe vert.
 2402 FORMAT(//30(1H*)/' *** CAS DE CHARGE Nø',I2,' ***'/30(1H*)/)
 2403 FORMAT(//25(1H*)/' *** LOAD CASE No',I2,'  ***'/25(1H*)/)
 5241 FORMAT(///' DONNEES RELATIVES AUX MOMENTS D''EXTREMITES'/
     *  T2,50(1H*)/15A4)
 5242 FORMAT(///' DONNEES RELATIVES AUX COEFFICIENTS DE PARTICIPATION'/
     *  T2,50(1H*)/15A4)
      END


      SUBROUTINE LAM(LAMV,IJK,PI,WIDTH)
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*8 LAMV
      DIMENSION LAMV(8)
      LAMV(1)=IJK*PI/WIDTH
      LAMV(2)=LAMV(1)*LAMV(1)
      LAMV(3)=LAMV(2)*LAMV(1)
      LAMV(4)=LAMV(2)*LAMV(2)
      LAMV(5)=LAMV(3)*LAMV(2)
      LAMV(6)=LAMV(3)*LAMV(3)
      LAMV(7)=LAMV(4)*LAMV(3)
      LAMV(8)=LAMV(4)*LAMV(4)
      RETURN                                                            
      END
