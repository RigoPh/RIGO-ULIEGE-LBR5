VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cCostCAtMain"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
#Const DebugMode = 0
#If DebugMode Then
    Private mlClassDebugID As Long
#End If
Public Enum ID_PANNEAU
    ID_None = 0
    NappePlane = 1
    CarlingueSerreHiloire = 2
    Bouchain = 3
    Virtual = 4
    Epontille = 5
End Enum
Public Enum IT_PANNEAU
    IT_None = 0
    DoubleCoqueInterieure = 1
    DoubleCoqueExterieure = 2
    BordeSimple = 3
End Enum
Public Enum IP_PANNEAU
    IP_None = 0
    InterieurNappe = 1
    ExterieurNappe = 2
End Enum
Public Enum TypeTapes
    TypeTapes_None = 0
    NonEtanchesRecouvrement = 1
    NonEtanchesEncastrees = 2
    EtanchesRecouvrement = 3
    EtanchesEncastrees = 4
End Enum
Public Enum SectionLisses
    SectionLisses_None = 0
    Profiles = 1
    TSynthetiques = 2
End Enum
Public Enum PositionAboutsLisses
    PA_None = 0
    SoudureAPlat = 1
    SoudureVerticale = 2
    SoudureHorizontale = 3
End Enum
Public Enum HabillageCarlingues
    Habillage_None = 0
    Habillage_UnCote = 1
    Habillage_DeuxCotes = 2
End Enum
Public Enum DimensionHabillage
    DimensionPlusPetite1 = 0
    DimensionPlusGrande1 = 1
End Enum
Public Enum SoudureContinueDiscontinue
    SoudureDiscontinue = 0
    SoudureContinue = 1
End Enum

' === Global Parameters ===
' Options
Private mvarbReadFractionnement As Integer
Private mvarbReadAccesibilite As Integer
Private mvarbReadAtelier As Integer
' Overall Parameters
Private mvariNAM As Integer '(nombre des abouts de montage)
Private mvarlPMB As Long '(cout total de preparer/metre/balancer (heures))
Private mvarlDiversTempsTolier As Long
Private mvarlDiversTempsSoudeur As Long
' === Parameters on Panels ===

Private mvarID_PANNEAU As ID_PANNEAU
Private mvarIT_PANNEAU As IT_PANNEAU
Private mvarIP_PANNEAU As IP_PANNEAU
Private mvarPositionAboutsLisses As PositionAboutsLisses
Private mvarcolCostCAtOperations As colCostCAtOperations

Private mvarbIsPartOfNappe As Boolean
' Voiles
Private mvarProfilesSurVoiles As Integer
Private mvarSoudureProfilesSurVoiles As SoudureContinueDiscontinue
Private mvarGoussetsProfilesVoiles As Integer
Private mvarPlatsEnBute As Integer
Private mvarAccostagesVoiles As Integer
' CarlinguesSerresHiloires
Private mvariNCI As Integer '(nombre de couples intermediaires)
Private mvarHabillageCarlingues As HabillageCarlingues
Private mvarDimensionHabillage As DimensionHabillage
' Pré (nappe plane)
Private mvarAccostagesNappes As Integer
Private mvariNANP As Integer '(nombre abouts de la nappe)
Private mvarSoudureLissesNappes As SoudureContinueDiscontinue
Private mvarSectionLisses As SectionLisses

'Pré (assemblage)
Private mvarTypeTapes As TypeTapes
Private mvarSectionTapes As SectionLisses
Private mvarCouplesCarlinguesSurNappes As Integer
Private mvarAccostagesToleBouchain As Integer
Private mvarNoIndicesDCoque As Integer

'Pré-montage - montage
Private mvarContactsBarrotsCloisons As Integer
Private mvarJonctionsBarrotHiloire As Integer

'Coefficients Accessibilité et Atelier groupés
Private mvarCoefAccPrePreVoile As Double
Private mvarCoefAccPrePreNappe As Double
Private mvarCoefAccPreAssInner As Double
Private mvarCoefAccPreAssOuter As Double
Private mvarCoefAccMontage As Double
Private mvarCoefAccPonts As Double
Private mvarCoefAccUsinage As Double

Private mvarCoefAtPrePreVoile As Double
Private mvarCoefAtPrePreNappe As Double
Private mvarCoefAtPreAssInner As Double
Private mvarCoefAtPreAssOuter As Double
Private mvarCoefAtMontage As Double
Private mvarCoefAtPonts As Double
Private mvarCoefAtUsinage As Double

Public Function GetHabillageCarlingues() As String
    On Error GoTo GetHabillageCarlinguesErr
    Select Case HabillageCarlingues
        Case 1
            GetHabillageCarlingues = "Simple"
        Case 2
            GetHabillageCarlingues = "Double"
    End Select
    Exit Function
GetHabillageCarlinguesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: Function GetHabillageCarlingues")
End Function

Public Function SetHabillageCarlingues(ByRef s As String) As HabillageCarlingues
    On Error GoTo SetHabillageCarlinguesErr
    s = Trim(s)
    Select Case s
        Case "Simple"
            SetHabillageCarlingues = Habillage_UnCote
        Case "Double"
            SetHabillageCarlingues = Habillage_DeuxCotes
        Case Else
            SetHabillageCarlingues = mvarHabillageCarlingues
    End Select
    Exit Function
SetHabillageCarlinguesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: Function SetHabillageCarlingues")
End Function

Public Function GetDimensionHabillage() As String
    On Error GoTo GetDimensionHabillageErr
    Select Case DimensionHabillage
        Case 0
            GetDimensionHabillage = "< 1"
        Case 1
            GetDimensionHabillage = "> 1"
    End Select
    Exit Function
GetDimensionHabillageErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: Function GetDimensionHabillage")
End Function

Public Function SetDimensionHabillage(ByRef s As String) As DimensionHabillage
    On Error GoTo SetDimensionHabillageErr
    s = Trim(s)
    Select Case s
        Case "< 1"
            SetDimensionHabillage = DimensionPlusPetite1
        Case "> 1"
            SetDimensionHabillage = DimensionPlusGrande1
        Case Else
            SetDimensionHabillage = mvarDimensionHabillage
    End Select
    Exit Function
SetDimensionHabillageErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: Function SetDimensionHabillage")
End Function

'Options
Public Property Let bReadFractionnement(ByVal vData As Integer)
    On Error GoTo bReadFractionnementLetErr
    mvarbReadFractionnement = vData
    Exit Property
bReadFractionnementLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: bReadFractionnement Property Let")
End Property

Public Property Get bReadFractionnement() As Integer
    On Error GoTo bReadFractionnementGetErr
    bReadFractionnement = mvarbReadFractionnement
    Exit Property
bReadFractionnementGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: bReadFractionnement Property Get")
End Property

Public Property Let bReadAccesibilite(ByVal vData As Integer)
    On Error GoTo bReadAccesibiliteLetErr
    mvarbReadAccesibilite = vData
    Exit Property
bReadAccesibiliteLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: bReadAccesibilite Property Let")
End Property

Public Property Get bReadAccesibilite() As Integer
    On Error GoTo bReadAccesibiliteGetErr
    bReadAccesibilite = mvarbReadAccesibilite
    Exit Property
bReadAccesibiliteGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: bReadAccesibilite Property Get")
End Property

Public Property Let bReadAtelier(ByVal vData As Integer)
    On Error GoTo bReadAtelierLetErr
    mvarbReadAtelier = vData
    Exit Property
bReadAtelierLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: bReadAtelier Property Let")
End Property

Public Property Get bReadAtelier() As Integer
    On Error GoTo bReadAtelierGetErr
    bReadAtelier = mvarbReadAtelier
    Exit Property
bReadAtelierGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: bReadAtelier Property Get")
End Property

Public Property Let iNAM(ByVal vData As Integer)
    On Error GoTo iNAMLetErr
    mvariNAM = vData
    Exit Property
iNAMLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: iNAM Property Let")
End Property

Public Property Get iNAM() As Integer
    On Error GoTo iNAMGetErr
    iNAM = mvariNAM
    Exit Property
iNAMGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: iNAM Property Get")
End Property

Public Property Let lPMB(ByVal vData As Long)
    On Error GoTo lPMBLetErr
    mvarlPMB = vData
    Exit Property
lPMBLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: lPMB Property Let")
End Property

Public Property Get lPMB() As Long
    On Error GoTo lPMBGetErr
    lPMB = mvarlPMB
    Exit Property
lPMBGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: lPMB Property Get")
End Property

Public Property Let lDiversTempsTolier(ByVal vData As Long)
    On Error GoTo lDiversTempsTolierLetErr
    mvarlDiversTempsTolier = vData
    Exit Property
lDiversTempsTolierLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: lDiversTempsTolier Property Let")
End Property

Public Property Get lDiversTempsTolier() As Long
    On Error GoTo lDiversTempsTolierGetErr
    lDiversTempsTolier = mvarlDiversTempsTolier
    Exit Property
lDiversTempsTolierGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: lDiversTempsTolier Property Get")
End Property

Public Property Let lDiversTempsSoudeur(ByVal vData As Long)
    On Error GoTo lDiversTempsSoudeurLetErr
    mvarlDiversTempsSoudeur = vData
    Exit Property
lDiversTempsSoudeurLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: lDiversTempsSoudeur Property Let")
End Property

Public Property Get lDiversTempsSoudeur() As Long
    On Error GoTo lDiversTempsSoudeurGetErr
    lDiversTempsSoudeur = mvarlDiversTempsSoudeur
    Exit Property
lDiversTempsSoudeurGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: lDiversTempsSoudeur Property Get")
End Property

Public Property Let iNCI(ByVal vData As Integer)
    On Error GoTo iNCILetErr
    mvariNCI = vData
    Exit Property
iNCILetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: iNCI Property Let")
End Property

Public Property Get iNCI() As Integer
    On Error GoTo iNCIGetErr
    iNCI = mvariNCI
    Exit Property
iNCIGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: iNCI Property Get")
End Property

Public Property Let iNANP(ByVal vData As Integer)
    On Error GoTo iNANPLetErr
    mvariNANP = vData
    Exit Property
iNANPLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: iNANP Property Let")
End Property

Public Property Get iNANP() As Integer
    On Error GoTo iNANPGetErr
    iNANP = mvariNANP
    Exit Property
iNANPGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: iNANP Property Get")
End Property

Public Property Let ID_PANNEAU(ByVal vData As ID_PANNEAU)
    On Error GoTo ID_PANNEAULetErr
    mvarID_PANNEAU = vData
    Exit Property
ID_PANNEAULetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: ID_PANNEAU Property Let")
End Property

Public Property Get ID_PANNEAU() As ID_PANNEAU
    On Error GoTo ID_PANNEAUGetErr
    ID_PANNEAU = mvarID_PANNEAU
    Exit Property
ID_PANNEAUGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: ID_PANNEAU Property Get")
End Property

Public Property Let IT_PANNEAU(ByVal vData As IT_PANNEAU)
    On Error GoTo IT_PANNEAULetErr
    mvarIT_PANNEAU = vData
    Exit Property
IT_PANNEAULetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: IT_PANNEAU Property Let")
End Property

Public Property Get IT_PANNEAU() As IT_PANNEAU
    On Error GoTo IT_PANNEAUGetErr
    IT_PANNEAU = mvarIT_PANNEAU
    Exit Property
IT_PANNEAUGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: IT_PANNEAU Property Get")
End Property

Public Property Let IP_PANNEAU(ByVal vData As IP_PANNEAU)
    On Error GoTo IP_PANNEAULetErr
    mvarIP_PANNEAU = vData
    Exit Property
IP_PANNEAULetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: IP_PANNEAU Property Let")
End Property

Public Property Get IP_PANNEAU() As IP_PANNEAU
    On Error GoTo IP_PANNEAUGetErr
    IP_PANNEAU = mvarIP_PANNEAU
    Exit Property
IP_PANNEAUGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: IP_PANNEAU Property Get")
End Property

Public Property Let TypeTapes(ByVal vData As TypeTapes)
    On Error GoTo TypeTapesLetErr
    mvarTypeTapes = vData
    Exit Property
TypeTapesLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: TypeTapes Property Let")
End Property

Public Property Get TypeTapes() As TypeTapes
    On Error GoTo TypeTapesGetErr
    TypeTapes = mvarTypeTapes
    Exit Property
TypeTapesGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: TypeTapes Property Get")
End Property

Public Property Let SectionTapes(ByVal vData As SectionLisses)
    On Error GoTo SectionTapesLetErr
    mvarSectionTapes = vData
    Exit Property
SectionTapesLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: SectionTapes Property Let")
End Property

Public Property Get SectionTapes() As SectionLisses
    On Error GoTo SectionTapesGetErr
    SectionTapes = mvarSectionTapes
    Exit Property
SectionTapesGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: SectionTapes Property Get")
End Property

Public Property Let SectionLisses(ByVal vData As SectionLisses)
    On Error GoTo SectionLissesLetErr
    mvarSectionLisses = vData
    Exit Property
SectionLissesLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: SectionLisses Property Let")
End Property

Public Property Get SectionLisses() As SectionLisses
    On Error GoTo SectionLissesGetErr
    SectionLisses = mvarSectionLisses
    Exit Property
SectionLissesGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: SectionLisses Property Get")
End Property

Public Property Let PositionAboutsLisses(ByVal vData As PositionAboutsLisses)
    On Error GoTo PositionAboutsLissesLetErr
    mvarPositionAboutsLisses = vData
    Exit Property
PositionAboutsLissesLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: PositionAboutsLisses Property Let")
End Property

Public Property Get PositionAboutsLisses() As PositionAboutsLisses
    On Error GoTo PositionAboutsLissesGetErr
    PositionAboutsLisses = mvarPositionAboutsLisses
    Exit Property
PositionAboutsLissesGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: PositionAboutsLisses Property Get")
End Property

Public Property Get colCostCAtOperations() As colCostCAtOperations
    On Error GoTo colCostCAtOperationsGetErr
    If mvarcolCostCAtOperations Is Nothing Then
        Set mvarcolCostCAtOperations = New colCostCAtOperations
    End If
    Set colCostCAtOperations = mvarcolCostCAtOperations
    Exit Property
colCostCAtOperationsGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: colCostCAtOperations Property Get")
End Property

Public Property Set colCostCAtOperations(vData As colCostCAtOperations)
    On Error GoTo colCostCAtOperationsSetErr
    Set mvarcolCostCAtOperations = vData
    Exit Property
colCostCAtOperationsSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: colCostCAtOperations Property Set")
End Property

Public Property Let bIsPartOfNappe(ByVal vData As Boolean)
    On Error GoTo bIsPartOfNappeLetErr
    mvarbIsPartOfNappe = vData
    Exit Property
bIsPartOfNappeLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: bIsPartOfNappe Property Let")
End Property

Public Property Get bIsPartOfNappe() As Boolean
    On Error GoTo bIsPartOfNappeGetErr
    bIsPartOfNappe = mvarbIsPartOfNappe
    Exit Property
bIsPartOfNappeGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: bIsPartOfNappe Property Get")
End Property

Public Property Let ProfilesSurVoiles(ByVal vData As Integer)
    On Error GoTo ProfilesSurVoilesLetErr
    mvarProfilesSurVoiles = vData
    Exit Property
ProfilesSurVoilesLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: ProfilesSurVoiles Property Let")
End Property

Public Property Get ProfilesSurVoiles() As Integer
    On Error GoTo ProfilesSurVoilesGetErr
    ProfilesSurVoiles = mvarProfilesSurVoiles
    Exit Property
ProfilesSurVoilesGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: ProfilesSurVoiles Property Get")
End Property

Public Property Let SoudureProfilesSurVoiles(ByVal vData As SoudureContinueDiscontinue)
    On Error GoTo SoudureProfilesSurVoilesLetErr
    mvarSoudureProfilesSurVoiles = vData
    Exit Property
SoudureProfilesSurVoilesLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: SoudureProfilesSurVoiles Property Let")
End Property

Public Property Get SoudureProfilesSurVoiles() As SoudureContinueDiscontinue
    On Error GoTo SoudureProfilesSurVoilesGetErr
    SoudureProfilesSurVoiles = mvarSoudureProfilesSurVoiles
    Exit Property
SoudureProfilesSurVoilesGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: SoudureProfilesSurVoiles Property Get")
End Property

Public Property Let SoudureLissesNappes(ByVal vData As SoudureContinueDiscontinue)
    On Error GoTo SoudureLissesNappesLetErr
    mvarSoudureLissesNappes = vData
    Exit Property
SoudureLissesNappesLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: SoudureLissesNappes Property Let")
End Property

Public Property Get SoudureLissesNappes() As SoudureContinueDiscontinue
    On Error GoTo SoudureLissesNappesGetErr
    SoudureLissesNappes = mvarSoudureLissesNappes
    Exit Property
SoudureLissesNappesGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: SoudureLissesNappes Property Get")
End Property

Public Property Let GoussetsProfilesVoiles(ByVal vData As Integer)
    On Error GoTo GoussetsProfilesVoilesLetErr
    mvarGoussetsProfilesVoiles = vData
    Exit Property
GoussetsProfilesVoilesLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: GoussetsProfilesVoiles Property Let")
End Property

Public Property Get GoussetsProfilesVoiles() As Integer
    On Error GoTo GoussetsProfilesVoilesGetErr
    GoussetsProfilesVoiles = mvarGoussetsProfilesVoiles
    Exit Property
GoussetsProfilesVoilesGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: GoussetsProfilesVoiles Property Get")
End Property

Public Property Let PlatsEnBute(ByVal vData As Integer)
    On Error GoTo PlatsEnButeLetErr
    mvarPlatsEnBute = vData
    Exit Property
PlatsEnButeLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: PlatsEnBute Property Let")
End Property

Public Property Get PlatsEnBute() As Integer
    On Error GoTo PlatsEnButeGetErr
    PlatsEnBute = mvarPlatsEnBute
    Exit Property
PlatsEnButeGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: PlatsEnBute Property Get")
End Property

Public Property Let AccostagesVoiles(ByVal vData As Integer)
    On Error GoTo AccostagesVoilesLetErr
    mvarAccostagesVoiles = vData
    Exit Property
AccostagesVoilesLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: AccostagesVoiles Property Let")
End Property

Public Property Get AccostagesVoiles() As Integer
    On Error GoTo AccostagesVoilesGetErr
    AccostagesVoiles = mvarAccostagesVoiles
    Exit Property
AccostagesVoilesGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: AccostagesVoiles Property Get")
End Property

Public Property Let AccostagesNappes(ByVal vData As Integer)
    On Error GoTo AccostagesNappesLetErr
    mvarAccostagesNappes = vData
    Exit Property
AccostagesNappesLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: AccostagesNappes Property Let")
End Property

Public Property Get AccostagesNappes() As Integer
    On Error GoTo AccostagesNappesGetErr
    AccostagesNappes = mvarAccostagesNappes
    Exit Property
AccostagesNappesGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: AccostagesNappes Property Get")
End Property

Public Property Let AccostagesToleBouchain(ByVal vData As Integer)
    On Error GoTo AccostagesToleBouchainLetErr
    mvarAccostagesToleBouchain = vData
    Exit Property
AccostagesToleBouchainLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: AccostagesToleBouchain Property Let")
End Property

Public Property Get AccostagesToleBouchain() As Integer
    On Error GoTo AccostagesToleBouchainGetErr
    AccostagesToleBouchain = mvarAccostagesToleBouchain
    Exit Property
AccostagesToleBouchainGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: AccostagesToleBouchain Property Get")
End Property

Public Property Let NoIndicesDCoque(ByVal vData As Integer)
    On Error GoTo NoIndicesDCoqueLetErr
    mvarNoIndicesDCoque = vData
    Exit Property
NoIndicesDCoqueLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: NoIndicesDCoque Property Let")
End Property

Public Property Get NoIndicesDCoque() As Integer
    On Error GoTo NoIndicesDCoqueGetErr
    NoIndicesDCoque = mvarNoIndicesDCoque
    Exit Property
NoIndicesDCoqueGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: NoIndicesDCoque Property Get")
End Property

Public Property Let CouplesCarlinguesSurNappes(ByVal vData As Integer)
    On Error GoTo CouplesCarlinguesSurNappesLetErr
    mvarCouplesCarlinguesSurNappes = vData
    Exit Property
CouplesCarlinguesSurNappesLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: CouplesCarlinguesSurNappes Property Let")
End Property

Public Property Get CouplesCarlinguesSurNappes() As Integer
    On Error GoTo CouplesCarlinguesSurNappesGetErr
    CouplesCarlinguesSurNappes = mvarCouplesCarlinguesSurNappes
    Exit Property
CouplesCarlinguesSurNappesGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: CouplesCarlinguesSurNappes Property Get")
End Property

Public Property Let ContactsBarrotsCloisons(ByVal vData As Integer)
    On Error GoTo ContactsBarrotsCloisonsLetErr
    mvarContactsBarrotsCloisons = vData
    Exit Property
ContactsBarrotsCloisonsLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: ContactsBarrotsCloisons Property Let")
End Property

Public Property Get ContactsBarrotsCloisons() As Integer
    On Error GoTo ContactsBarrotsCloisonsGetErr
    ContactsBarrotsCloisons = mvarContactsBarrotsCloisons
    Exit Property
ContactsBarrotsCloisonsGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: ContactsBarrotsCloisons Property Get")
End Property

Public Property Let JonctionsBarrotHiloire(ByVal vData As Integer)
    On Error GoTo JonctionsBarrotHiloireLetErr
    mvarJonctionsBarrotHiloire = vData
    Exit Property
JonctionsBarrotHiloireLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: JonctionsBarrotHiloire Property Let")
End Property

Public Property Get JonctionsBarrotHiloire() As Integer
    On Error GoTo JonctionsBarrotHiloireGetErr
    JonctionsBarrotHiloire = mvarJonctionsBarrotHiloire
    Exit Property
JonctionsBarrotHiloireGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: JonctionsBarrotHiloire Property Get")
End Property

Public Property Let HabillageCarlingues(ByVal vData As HabillageCarlingues)
    On Error GoTo HabillageCarlinguesLetErr
    mvarHabillageCarlingues = vData
    Exit Property
HabillageCarlinguesLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: HabillageCarlingues Property Let")
End Property

Public Property Get HabillageCarlingues() As HabillageCarlingues
    On Error GoTo HabillageCarlinguesGetErr
    HabillageCarlingues = mvarHabillageCarlingues
    Exit Property
HabillageCarlinguesGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: HabillageCarlingues Property Get")
End Property

Public Property Let DimensionHabillage(ByVal vData As DimensionHabillage)
    On Error GoTo DimensionHabillageLetErr
    mvarDimensionHabillage = vData
    Exit Property
DimensionHabillageLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: DimensionHabillage Property Let")
End Property

Public Property Get DimensionHabillage() As DimensionHabillage
    On Error GoTo DimensionHabillageGetErr
    DimensionHabillage = mvarDimensionHabillage
    Exit Property
DimensionHabillageGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: DimensionHabillage Property Get")
End Property

Public Property Let CoefAccPrePreVoile(ByVal vData As Double)
    On Error GoTo CoefAccPrePreVoileLetErr
    mvarCoefAccPrePreVoile = vData
    Exit Property
CoefAccPrePreVoileLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: CoefAccPrePreVoile Property Let")
End Property

Public Property Get CoefAccPrePreVoile() As Double
    On Error GoTo CoefAccPrePreVoileGetErr
    CoefAccPrePreVoile = mvarCoefAccPrePreVoile
    Exit Property
CoefAccPrePreVoileGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: CoefAccPrePreVoile Property Get")
End Property

Public Property Let CoefAccPrePreNappe(ByVal vData As Double)
    On Error GoTo CoefAccPrePreNappeLetErr
    mvarCoefAccPrePreNappe = vData
    Exit Property
CoefAccPrePreNappeLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: CoefAccPrePreNappe Property Let")
End Property

Public Property Get CoefAccPrePreNappe() As Double
    On Error GoTo CoefAccPrePreNappeGetErr
    CoefAccPrePreNappe = mvarCoefAccPrePreNappe
    Exit Property
CoefAccPrePreNappeGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: CoefAccPrePreNappe Property Get")
End Property

Public Property Let CoefAccPreAssInner(ByVal vData As Double)
    On Error GoTo CoefAccPreAssInnerLetErr
    mvarCoefAccPreAssInner = vData
    Exit Property
CoefAccPreAssInnerLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: CoefAccPreAssInner Property Let")
End Property

Public Property Get CoefAccPreAssInner() As Double
    On Error GoTo CoefAccPreAssInnerGetErr
    CoefAccPreAssInner = mvarCoefAccPreAssInner
    Exit Property
CoefAccPreAssInnerGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: CoefAccPreAssInner Property Get")
End Property

Public Property Let CoefAccPreAssOuter(ByVal vData As Double)
    On Error GoTo CoefAccPreAssOuterLetErr
    mvarCoefAccPreAssOuter = vData
    Exit Property
CoefAccPreAssOuterLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: CoefAccPreAssOuter Property Let")
End Property

Public Property Get CoefAccPreAssOuter() As Double
    On Error GoTo CoefAccPreAssOuterGetErr
    CoefAccPreAssOuter = mvarCoefAccPreAssOuter
    Exit Property
CoefAccPreAssOuterGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: CoefAccPreAssOuter Property Get")
End Property

Public Property Let CoefAccMontage(ByVal vData As Double)
    On Error GoTo CoefAccMontageLetErr
    mvarCoefAccMontage = vData
    Exit Property
CoefAccMontageLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: CoefAccMontage Property Let")
End Property

Public Property Get CoefAccMontage() As Double
    On Error GoTo CoefAccMontageGetErr
    CoefAccMontage = mvarCoefAccMontage
    Exit Property
CoefAccMontageGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: CoefAccMontage Property Get")
End Property

Public Property Let CoefAccPonts(ByVal vData As Double)
    On Error GoTo CoefAccPontsLetErr
    mvarCoefAccPonts = vData
    Exit Property
CoefAccPontsLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: CoefAccPonts Property Let")
End Property

Public Property Get CoefAccPonts() As Double
    On Error GoTo CoefAccPontsGetErr
    CoefAccPonts = mvarCoefAccPonts
    Exit Property
CoefAccPontsGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: CoefAccPonts Property Get")
End Property

Public Property Let CoefAccUsinage(ByVal vData As Double)
    On Error GoTo CoefAccUsinageLetErr
    mvarCoefAccUsinage = vData
    Exit Property
CoefAccUsinageLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: CoefAccUsinage Property Let")
End Property

Public Property Get CoefAccUsinage() As Double
    On Error GoTo CoefAccUsinageGetErr
    CoefAccUsinage = mvarCoefAccUsinage
    Exit Property
CoefAccUsinageGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: CoefAccUsinage Property Get")
End Property

Public Property Let CoefAtPrePreVoile(ByVal vData As Double)
    On Error GoTo CoefAtPrePreVoileLetErr
    mvarCoefAtPrePreVoile = vData
    Exit Property
CoefAtPrePreVoileLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: CoefAtPrePreVoile Property Let")
End Property

Public Property Get CoefAtPrePreVoile() As Double
    On Error GoTo CoefAtPrePreVoileGetErr
    CoefAtPrePreVoile = mvarCoefAtPrePreVoile
    Exit Property
CoefAtPrePreVoileGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: CoefAtPrePreVoile Property Get")
End Property

Public Property Let CoefAtPrePreNappe(ByVal vData As Double)
    On Error GoTo CoefAtPrePreNappeLetErr
    mvarCoefAtPrePreNappe = vData
    Exit Property
CoefAtPrePreNappeLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: CoefAtPrePreNappe Property Let")
End Property

Public Property Get CoefAtPrePreNappe() As Double
    On Error GoTo CoefAtPrePreNappeGetErr
    CoefAtPrePreNappe = mvarCoefAtPrePreNappe
    Exit Property
CoefAtPrePreNappeGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: CoefAtPrePreNappe Property Get")
End Property

Public Property Let CoefAtPreAssInner(ByVal vData As Double)
    On Error GoTo CoefAtPreAssInnerLetErr
    mvarCoefAtPreAssInner = vData
    Exit Property
CoefAtPreAssInnerLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: CoefAtPreAssInner Property Let")
End Property

Public Property Get CoefAtPreAssInner() As Double
    On Error GoTo CoefAtPreAssInnerGetErr
    CoefAtPreAssInner = mvarCoefAtPreAssInner
    Exit Property
CoefAtPreAssInnerGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: CoefAtPreAssInner Property Get")
End Property

Public Property Let CoefAtPreAssOuter(ByVal vData As Double)
    On Error GoTo CoefAtPreAssOuterLetErr
    mvarCoefAtPreAssOuter = vData
    Exit Property
CoefAtPreAssOuterLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: CoefAtPreAssOuter Property Let")
End Property

Public Property Get CoefAtPreAssOuter() As Double
    On Error GoTo CoefAtPreAssOuterGetErr
    CoefAtPreAssOuter = mvarCoefAtPreAssOuter
    Exit Property
CoefAtPreAssOuterGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: CoefAtPreAssOuter Property Get")
End Property

Public Property Let CoefAtMontage(ByVal vData As Double)
    On Error GoTo CoefAtMontageLetErr
    mvarCoefAtMontage = vData
    Exit Property
CoefAtMontageLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: CoefAtMontage Property Let")
End Property

Public Property Get CoefAtMontage() As Double
    On Error GoTo CoefAtMontageGetErr
    CoefAtMontage = mvarCoefAtMontage
    Exit Property
CoefAtMontageGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: CoefAtMontage Property Get")
End Property

Public Property Let CoefAtPonts(ByVal vData As Double)
    On Error GoTo CoefAtPontsLetErr
    mvarCoefAtPonts = vData
    Exit Property
CoefAtPontsLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: CoefAtPonts Property Let")
End Property

Public Property Get CoefAtPonts() As Double
    On Error GoTo CoefAtPontsGetErr
    CoefAtPonts = mvarCoefAtPonts
    Exit Property
CoefAtPontsGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: CoefAtPonts Property Get")
End Property

Public Property Let CoefAtUsinage(ByVal vData As Double)
    On Error GoTo CoefAtUsinageLetErr
    mvarCoefAtUsinage = vData
    Exit Property
CoefAtUsinageLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: CoefAtUsinage Property Let")
End Property

Public Property Get CoefAtUsinage() As Double
    On Error GoTo CoefAtUsinageGetErr
    CoefAtUsinage = mvarCoefAtUsinage
    Exit Property
CoefAtUsinageGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: CoefAtUsinage Property Get")
End Property

Public Function Clone() As cCostCAtMain
    On Error GoTo CloneErr
    Set Clone = New cCostCAtMain
    With Clone
        .bReadFractionnement = Me.bReadAccesibilite
        .bReadAccesibilite = Me.bReadAccesibilite
        .bReadAtelier = Me.bReadAtelier
        .iNAM = Me.iNAM
        .lPMB = Me.lPMB
        .lDiversTempsTolier = Me.lDiversTempsTolier
        .lDiversTempsSoudeur = Me.lDiversTempsSoudeur
        .iNCI = Me.iNCI
        .iNANP = Me.iNANP
        .ID_PANNEAU = Me.ID_PANNEAU
        .IT_PANNEAU = Me.IT_PANNEAU
        .IP_PANNEAU = Me.IP_PANNEAU
        .TypeTapes = Me.TypeTapes
        .SectionTapes = Me.SectionTapes
        .SectionLisses = Me.SectionLisses
        .PositionAboutsLisses = Me.PositionAboutsLisses
        Set .colCostCAtOperations = Me.colCostCAtOperations.Clone
        .bIsPartOfNappe = Me.bIsPartOfNappe
        .ProfilesSurVoiles = Me.ProfilesSurVoiles
        .SoudureProfilesSurVoiles = Me.SoudureProfilesSurVoiles
        .GoussetsProfilesVoiles = Me.GoussetsProfilesVoiles
        .PlatsEnBute = Me.PlatsEnBute
        .AccostagesVoiles = Me.AccostagesVoiles
        .HabillageCarlingues = Me.HabillageCarlingues
        .DimensionHabillage = Me.DimensionHabillage
        .AccostagesNappes = Me.AccostagesNappes
        .AccostagesToleBouchain = Me.AccostagesToleBouchain
        .SoudureLissesNappes = Me.SoudureLissesNappes
        .CouplesCarlinguesSurNappes = Me.CouplesCarlinguesSurNappes
        .NoIndicesDCoque = Me.NoIndicesDCoque
        .ContactsBarrotsCloisons = Me.ContactsBarrotsCloisons
        .JonctionsBarrotHiloire = Me.JonctionsBarrotHiloire
        .AccostagesNappes = Me.AccostagesNappes
        'Coefficients Accessibilité et Atelier groupés
        .CoefAccPrePreVoile = Me.CoefAccPrePreVoile
        .CoefAccPrePreNappe = Me.CoefAccPrePreNappe
        .CoefAccPreAssInner = Me.CoefAccPreAssInner
        .CoefAccPreAssOuter = Me.CoefAccPreAssOuter
        .CoefAccMontage = Me.CoefAccMontage
        .CoefAccPonts = Me.CoefAccPonts
        .CoefAccUsinage = Me.CoefAccUsinage
        
        .CoefAtPrePreVoile = Me.CoefAtPrePreVoile
        .CoefAtPrePreNappe = Me.CoefAtPrePreNappe
        .CoefAtPreAssInner = Me.CoefAtPreAssInner
        .CoefAtPreAssOuter = Me.CoefAtPreAssOuter
        .CoefAtMontage = Me.CoefAtMontage
        .CoefAtPonts = Me.CoefAtPonts
        .CoefAtUsinage = Me.CoefAtUsinage
        
    End With
    Exit Function
CloneErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: Function Clone")
End Function

Public Function SetDefaultGlobalData()
    mvarbReadFractionnement = 1
    mvarbReadAccesibilite = 1
    mvarbReadAtelier = 1
    mvariNAM = 2
    mvarlPMB = 0
    mvarlDiversTempsTolier = 0
    mvarlDiversTempsSoudeur = 0
    mvarbIsPartOfNappe = False
    mvarNoIndicesDCoque = 0
    
    'Coefficients Accesibilité et Atelier
    mvarCoefAccPrePreVoile = 1
    mvarCoefAccPrePreNappe = 1
    mvarCoefAccPreAssInner = 1.12
    mvarCoefAccPreAssOuter = 1.22
    mvarCoefAccMontage = 1.27
    mvarCoefAccPonts = 1
    mvarCoefAccUsinage = 1
    
    mvarCoefAtPrePreVoile = 1.25
    mvarCoefAtPrePreNappe = 0.45
    mvarCoefAtPreAssInner = 1.32
    mvarCoefAtPreAssOuter = 1.32
    mvarCoefAtMontage = 2.4
    mvarCoefAtPonts = 1.32
    mvarCoefAtUsinage = 0.41
End Function

Public Function setBilgeID_Soudure()
    On Error GoTo setBilgeID_SoudureErr
    Me.colCostCAtOperations.Item(21).Soudures = 0
    Me.colCostCAtOperations.Item(22).Soudures = 0
    Me.colCostCAtOperations.Item(23).Soudures = 0
    Me.colCostCAtOperations.Item(36).Soudures = 5
    Me.colCostCAtOperations.Item(37).Soudures = 0
    Me.colCostCAtOperations.Item(47).Soudures = 50
    Me.colCostCAtOperations.Item(48).Soudures = 50
    Me.colCostCAtOperations.Item(51).Soudures = 0
    Exit Function
setBilgeID_SoudureErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: Function setBilgeID_Soudure")
End Function

Public Function setGirderID_Soudure(ByVal PanelIndex As Integer, ByVal ProjectIndex As Integer)
    On Error GoTo setGirderID_SoudureErr
    Me.colCostCAtOperations.Item(21).Soudures = 0
    Me.colCostCAtOperations.Item(22).Soudures = 2
    Me.colCostCAtOperations.Item(23).Soudures = 4
    Me.colCostCAtOperations.Item(36).Soudures = 0
    Me.colCostCAtOperations.Item(37).Soudures = 2
    Me.colCostCAtOperations.Item(47).Soudures = 0
    Me.colCostCAtOperations.Item(48).Soudures = 16
    Select Case IsPanelOnSymmAxis(PanelIndex, ProjectIndex)
        Case Is = True
            Me.colCostCAtOperations.Item(51).Soudures = 16
        Case Is = False
            Me.colCostCAtOperations.Item(51).Soudures = 0
    End Select
    Exit Function
setGirderID_SoudureErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmCostCAt: Function setGirderID_Soudure")
End Function

Public Function setShellID_Soudure(ByVal PanelIndex As Integer, ByVal ProjectIndex As Integer)
    On Error GoTo setShellID_SoudureErr
    Me.colCostCAtOperations.Item(21).Soudures = 0
    Me.colCostCAtOperations.Item(22).Soudures = 0
    Me.colCostCAtOperations.Item(23).Soudures = 0
    Me.colCostCAtOperations.Item(36).Soudures = 0
    Me.colCostCAtOperations.Item(37).Soudures = 0
    Select Case IsPanelHorizontal(PanelIndex, ProjectIndex)
        Case Is = True
            Me.colCostCAtOperations.Item(47).Soudures = 14
        Case Is = False
            Me.colCostCAtOperations.Item(47).Soudures = 15
    End Select
    Select Case IsPanelInsideStructure(PanelIndex, ProjectIndex)
        Case Is = True
            Me.colCostCAtOperations.Item(48).Soudures = 16
        Case Is = False
            Me.colCostCAtOperations.Item(48).Soudures = 51
    End Select
    Me.colCostCAtOperations.Item(51).Soudures = 0
    Exit Function
setShellID_SoudureErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmCostCAt: Function setShellID_Soudure")
End Function

Public Function setVirtualID_Soudure()
    On Error GoTo setVirtualID_SoudureErr
    Me.colCostCAtOperations.Item(21).Soudures = 0
    Me.colCostCAtOperations.Item(22).Soudures = 0
    Me.colCostCAtOperations.Item(23).Soudures = 0
    Me.colCostCAtOperations.Item(36).Soudures = 0
    Me.colCostCAtOperations.Item(37).Soudures = 0
    Me.colCostCAtOperations.Item(47).Soudures = 0
    Me.colCostCAtOperations.Item(48).Soudures = 0
    Me.colCostCAtOperations.Item(51).Soudures = 0
    Exit Function
setVirtualID_SoudureErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmCostCAt: Function setVirtualID_Soudure")
End Function

Public Function SetDefaultPanelData(ByRef Panel As cPanel, Optional ByVal ProjectIndex As Integer)
    On Error GoTo SetDefaultPanelDataErr
    'sets default data on a new panel type defined in CostCAt Wizard
    'Set Pré-pré data
    
    Select Case Panel.pType
        Case Plate, DoubleHull
            Select Case Panel.cCostCAtMain.ID_PANNEAU '= CarlingueSerreHiloire: = Bouchain: = Epontille
                Case NappePlane
                    Me.ProfilesSurVoiles = Panel.cScantlings.cPrimaryStiffeners.GetNoOfStiffeners(Panel)
                    Me.SoudureProfilesSurVoiles = SoudureContinue
                    Me.GoussetsProfilesVoiles = 0
                    Me.PlatsEnBute = 0
                    Me.AccostagesVoiles = 0
                    Me.PositionAboutsLisses = SoudureVerticale
                    Select Case Panel.cCostCAtMain.IT_PANNEAU
                        Case BordeSimple
                            setShellID_Soudure Panel.pNumber, ProjectIndex
'                        Case DoubleCoqueInterieure
'                        Case DoubleCoqueExterieure
                    End Select
                Case CarlingueSerreHiloire
                    Me.iNCI = 2
                    Me.HabillageCarlingues = Habillage_DeuxCotes
                    Me.DimensionHabillage = DimensionPlusPetite1
                    Me.PositionAboutsLisses = PA_None
                    Me.setGirderID_Soudure Panel.pNumber, ProjectIndex
                Case Bouchain
                    Me.ProfilesSurVoiles = Panel.cScantlings.cPrimaryStiffeners.GetNoOfStiffeners(Panel)
                    Me.SoudureProfilesSurVoiles = SoudureContinue
                    Me.GoussetsProfilesVoiles = 0
                    Me.PlatsEnBute = 0
                    Me.AccostagesVoiles = 0
                    Me.PositionAboutsLisses = SoudureVerticale
                    Me.setBilgeID_Soudure
                Case Epontille
                Case Virtual
                    Me.setVirtualID_Soudure
                    Panel.cCostCAtMain.IP_PANNEAU = IP_None
                    Panel.cCostCAtMain.IT_PANNEAU = IT_None
                    Panel.cCostCAtMain.TypeTapes = TypeTapes_None
                    Panel.cCostCAtMain.PositionAboutsLisses = PA_None
            End Select
    End Select
    'Set Pré (assemblage) data
    Select Case Panel.pType
        Case Plate, DoubleHull
            Select Case Panel.cCostCAtMain.ID_PANNEAU '= CarlingueSerreHiloire: = Bouchain: = Epontille
                Case NappePlane
                    Me.TypeTapes = NonEtanchesRecouvrement
                    Me.SectionTapes = Profiles
                    Me.CouplesCarlinguesSurNappes = 0
                Case CarlingueSerreHiloire
'                    Me.TypeTapes = TypeTapes_None
'                    Me.SectionTapes = SectionTapes_None
                    Me.CouplesCarlinguesSurNappes = 0
                Case Bouchain
                    Me.TypeTapes = NonEtanchesRecouvrement
                    Me.SectionTapes = Profiles
                    Me.CouplesCarlinguesSurNappes = 0
                    Me.AccostagesToleBouchain = 0
                Case Epontille
            End Select
    End Select
    Exit Function
SetDefaultPanelDataErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: Function SetDefaultPanelData")
End Function

Public Function SetFirstPanelData(ByVal PanelIndex As Integer, ByVal ProjectIndex As Integer)
    'On Error GoTo  SetFirstPanelDataErr
    Dim Panel As cPanel
    Set Panel = Project.Item(ProjectIndex).colPanel.Item(PanelIndex)
    'Sets default data on a new added LBR5 pannel
    'Set ID_PANNEAU, IT_PANNEAU
    Select Case Panel.pType
        Case Plate
            Me.ID_PANNEAU = NappePlane
            Me.IT_PANNEAU = BordeSimple
            Me.IP_PANNEAU = ExterieurNappe
            Me.PositionAboutsLisses = SoudureVerticale
        Case Beam
            Me.ID_PANNEAU = Epontille
            Me.IT_PANNEAU = IT_None
            Me.IP_PANNEAU = IP_None
            Me.PositionAboutsLisses = PA_None
        Case DoubleHull
            Me.ID_PANNEAU = NappePlane
            Me.IT_PANNEAU = BordeSimple
            Me.IP_PANNEAU = ExterieurNappe
            Me.PositionAboutsLisses = SoudureVerticale
    End Select
    'Set Pré-pré data
    Select Case Panel.pType
        Case Plate, DoubleHull
            Me.ProfilesSurVoiles = Panel.cScantlings.cPrimaryStiffeners.GetNoOfStiffeners(Panel)
            Me.SoudureProfilesSurVoiles = SoudureContinue
            Me.GoussetsProfilesVoiles = 0
            Me.PlatsEnBute = 0
            Me.AccostagesVoiles = 0
            Me.SectionLisses = Profiles
    End Select
    
    Select Case Panel.pType
        Case Plate
            Select Case Panel.cGeometry.PanelWidth
                Case Is <= 0.002
                    Panel.cCostCAtMain.ID_PANNEAU = Virtual
                    Panel.cCostCAtMain.IP_PANNEAU = IP_None
                    Panel.cCostCAtMain.IT_PANNEAU = IT_None
                    Panel.cCostCAtMain.TypeTapes = TypeTapes_None
                    Panel.cCostCAtMain.PositionAboutsLisses = PA_None
            End Select
    End Select
    'Set Pré data
    Select Case Panel.pType
        Case Plate, DoubleHull
            If Panel.cCostCAtMain.ID_PANNEAU <> Virtual Then
                Me.iNANP = 0
                Me.AccostagesNappes = 0
                Me.SoudureLissesNappes = SoudureContinue
                Me.TypeTapes = NonEtanchesRecouvrement
                Me.SectionTapes = Profiles
                Me.CouplesCarlinguesSurNappes = 0
            End If
    End Select
    Dim i As Integer
    For i = 1 To 10 'PrePreVoile
        Me.colCostCAtOperations.Item(i).Accesibilite = 1
        Me.colCostCAtOperations.Item(i).Atelier = 1.25
    Next i
    For i = 11 To 20 'PrePreNappe
        Me.colCostCAtOperations.Item(i).Accesibilite = 1
        Me.colCostCAtOperations.Item(i).Atelier = 0.45
    Next i
    For i = 21 To 34 'PreAssInner
        Me.colCostCAtOperations.Item(i).Accesibilite = 1.12
        Me.colCostCAtOperations.Item(i).Atelier = 1.32
    Next i
    For i = 35 To 45 'PreAssOuter
        Me.colCostCAtOperations.Item(i).Accesibilite = 1.22
        Me.colCostCAtOperations.Item(i).Atelier = 1.32
    Next i
    For i = 46 To 52 'Montage
        Me.colCostCAtOperations.Item(i).Accesibilite = 1.27
        Me.colCostCAtOperations.Item(i).Atelier = 2.4
    Next i
    Me.colCostCAtOperations.Item(59).Accesibilite = 1.27 'Montage
    Me.colCostCAtOperations.Item(59).Atelier = 2.4
    For i = 53 To 58 'Ponts
        Me.colCostCAtOperations.Item(i).Accesibilite = 1
        Me.colCostCAtOperations.Item(i).Atelier = 1.32
    Next i
    Me.colCostCAtOperations.Item(60).Accesibilite = 1 'Usinage
    Me.colCostCAtOperations.Item(60).Atelier = 0.41
    
    'ID Soudures
    'operations:  47, 48 --> les autres op sont pour des types de panneaux qui ne sont
    '                        pas encore definis
    'Joints des panneaux
    If Panel.pType <> Beam Then 'exclude beams
        If Panel.cCostCAtMain.ID_PANNEAU <> Virtual Then 'exclude virtual panels
            Select Case IsPanelHorizontal(Panel.pNumber, ProjectIndex)
                Case Is = True
                    Me.colCostCAtOperations.Item(47).Soudures = 14
                    Me.colCostCAtOperations.Item(48).Soudures = 14
                Case Is = False
                    Me.colCostCAtOperations.Item(47).Soudures = 15
                    Select Case IsPanelInsideStructure(Panel.pNumber, ProjectIndex)
                        Case Is = True
                            Me.colCostCAtOperations.Item(48).Soudures = 16
                        Case Is = False
                            Me.colCostCAtOperations.Item(48).Soudures = 51
                    End Select
            End Select
        End If
    End If
    Set Panel = Nothing
    Exit Function
SetFirstPanelDataErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: Function SetFirstPanelData")
End Function

Public Function GetTypeTapes() As String
    On Error GoTo GetTypeTapesErr
    Select Case TypeTapes
        Case 0
            GetTypeTapes = "TypeTapes_None"
        Case 1
            GetTypeTapes = "Non-Etanches - Recouvrement"
        Case 2
            GetTypeTapes = "Non-Etanches - Encastrées"
        Case 3
            GetTypeTapes = "Etanches - Recouvrement"
        Case 4
            GetTypeTapes = "Etanches - Encastrées"
    End Select
    Exit Function
GetTypeTapesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: Function GetTypeTapes")
End Function

Public Function SetTypeTapes(ByRef s As String) As TypeTapes
    On Error GoTo SetTypeTapesErr
    s = Trim(s)
    Select Case s
'        Case "TypeTapes_None"
'            SetTypeTapes = TypeTapes_None
        Case "Non-Etanches - Recouvrement"
            SetTypeTapes = NonEtanchesRecouvrement
        Case "Non-Etanches - Encastrées"
            SetTypeTapes = NonEtanchesEncastrees
        Case "Etanches - Recouvrement"
            SetTypeTapes = EtanchesRecouvrement
        Case "Etanches - Encastrées"
            SetTypeTapes = EtanchesEncastrees
        Case Else
            SetTypeTapes = mvarTypeTapes
    End Select
    Exit Function
SetTypeTapesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: Function SetTypeTapes")
End Function

Public Function GetSectionTapes() As String
    On Error GoTo GetSectionTapesErr
    Select Case SectionTapes
        Case 0
            GetSectionTapes = "SectionTapes_None"
        Case 1
            GetSectionTapes = "Profilés"
        Case 2
            GetSectionTapes = "T Synthétiques"
    End Select
    Exit Function
GetSectionTapesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: Function GetSectionTapes")
End Function

Public Function GetSectionLisses() As String
    On Error GoTo GetSectionLissesErr
    Select Case SectionLisses
'        Case 0
'            GetSectionLisses = "SectionTapes_None"
        Case 1
            GetSectionLisses = "Profilés"
        Case 2
            GetSectionLisses = "T Synthétiques"
    End Select
    Exit Function
GetSectionLissesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: Function GetSectionLisses")
End Function

Public Function SetSectionLisses(ByRef s As String) As SectionLisses
    On Error GoTo SetSectionLissesErr
    s = Trim(s)
    Select Case s
'        Case "SectionTapes_None"
'            SetSectionLisses = SectionTapes_None
        Case "Profilés"
            SetSectionLisses = Profiles
        Case "T Synthétiques"
            SetSectionLisses = TSynthetiques
        Case Else
            SetSectionLisses = mvarSectionLisses
    End Select
    Exit Function
SetSectionLissesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: Function SetSectionLisses")
End Function

Public Function SetSectionTapes(ByRef s As String) As SectionLisses
    On Error GoTo SetSectionTapesErr
    s = Trim(s)
    Select Case s
'        Case "SectionTapes_None"
'            SetSectionTapes = SectionTapes_None
        Case "Profilés"
            SetSectionTapes = Profiles
        Case "T Synthétiques"
            SetSectionTapes = TSynthetiques
        Case Else
            SetSectionTapes = mvarSectionTapes
    End Select
    Exit Function
SetSectionTapesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: Function SetSectionTapes")
End Function

Public Function GetSoudureProfilesSurVoiles() As String
    On Error GoTo GetSoudureProfilesSurVoilesErr
    Select Case SoudureProfilesSurVoiles
        Case 0
            GetSoudureProfilesSurVoiles = "Discontinue"
        Case 1
            GetSoudureProfilesSurVoiles = "Continue"
    End Select
    Exit Function
GetSoudureProfilesSurVoilesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: Function GetSoudureProfilesSurVoiles")
End Function

Public Function GetSoudureLissesNappes() As String
    On Error GoTo GetSoudureLissesNappesErr
    Select Case SoudureLissesNappes
        Case 0
            GetSoudureLissesNappes = "Discontinue"
        Case 1
            GetSoudureLissesNappes = "Continue"
    End Select
    Exit Function
GetSoudureLissesNappesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: Function GetSoudureLissesNappes")
End Function

Public Function SetSoudureContinueDiscontinue(ByRef s As String) As SoudureContinueDiscontinue
    On Error GoTo SetSoudureContinueDiscontinueErr
    s = Trim(s)
    Select Case s
        Case "Discontinue"
            SetSoudureContinueDiscontinue = SoudureDiscontinue
        Case "Continue"
            SetSoudureContinueDiscontinue = SoudureContinue
    End Select
    Exit Function
SetSoudureContinueDiscontinueErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: Function SetSoudureContinueDiscontinue")
End Function

Public Function SetSoudureProfilesSurVoiles(ByRef s As String) As SoudureContinueDiscontinue
    On Error GoTo SetSoudureProfilesSurVoilesErr
    s = Trim(s)
    Select Case s
        Case "Discontinue"
            SetSoudureProfilesSurVoiles = SoudureDiscontinue
        Case "Continue"
            SetSoudureProfilesSurVoiles = SoudureContinue
        Case Else
            SetSoudureProfilesSurVoiles = mvarSoudureProfilesSurVoiles
    End Select
    Exit Function
SetSoudureProfilesSurVoilesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: Function SetSoudureProfilesSurVoiles")
End Function

Public Function SetSoudureLissesNappes(ByRef s As String) As SoudureContinueDiscontinue
    On Error GoTo SetSoudureLissesNappesErr
    s = Trim(s)
    Select Case s
        Case "Discontinue"
            SetSoudureLissesNappes = SoudureDiscontinue
        Case "Continue"
            SetSoudureLissesNappes = SoudureContinue
        Case Else
            SetSoudureLissesNappes = mvarSoudureLissesNappes
    End Select
    Exit Function
SetSoudureLissesNappesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: Function SetSoudureLissesNappes")
End Function

Private Sub Class_Initialize()
    Set mvarcolCostCAtOperations = New colCostCAtOperations
    Dim i As Integer
    Dim m As cCostCAtOperations
    For i = 1 To NO_OPERATIONS
        Set m = New cCostCAtOperations
        m.index = i
        mvarcolCostCAtOperations.Add m, i
        Set m = Nothing
    Next i
    #If DebugMode Then
        mlClassDebugID = GetNextClassDebugID()
        Debug.Print "'" & TypeName(Me) & "' instance " & mlClassDebugID & " created"
    #End If
End Sub

Public Function WriteGlobalParamASCII(ByVal iNoOfFile As Integer)
    On Error GoTo WriteGlobalParamASCIIErr
    With Me
        Put #iNoOfFile, , .bReadFractionnement
        Put #iNoOfFile, , .bReadAccesibilite
        Put #iNoOfFile, , .bReadAtelier
        Put #iNoOfFile, , .iNAM
        Put #iNoOfFile, , .lPMB
        Put #iNoOfFile, , .lDiversTempsTolier
        Put #iNoOfFile, , .lDiversTempsSoudeur
        Put #iNoOfFile, , .NoIndicesDCoque
        'Coefficients Accessibilité et Atelier groupés
        Put #iNoOfFile, , .CoefAccPrePreVoile
        Put #iNoOfFile, , .CoefAccPrePreNappe
        Put #iNoOfFile, , .CoefAccPreAssInner
        Put #iNoOfFile, , .CoefAccPreAssOuter
        Put #iNoOfFile, , .CoefAccMontage
        Put #iNoOfFile, , .CoefAccPonts
        Put #iNoOfFile, , .CoefAccUsinage
        Put #iNoOfFile, , .CoefAtPrePreVoile
        Put #iNoOfFile, , .CoefAtPrePreNappe
        Put #iNoOfFile, , .CoefAtPreAssInner
        Put #iNoOfFile, , .CoefAtPreAssOuter
        Put #iNoOfFile, , .CoefAtMontage
        Put #iNoOfFile, , .CoefAtPonts
        Put #iNoOfFile, , .CoefAtUsinage
    End With
    Exit Function
WriteGlobalParamASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: Function WriteGlobalParamASCII")
End Function

Public Function WritePanelsASCII(ByVal iNoOfFile As Integer)
    On Error GoTo WritePanelsASCIIErr
    With Me
        Put #iNoOfFile, , CInt(.ID_PANNEAU)
        Put #iNoOfFile, , CInt(.IT_PANNEAU)
        Put #iNoOfFile, , CInt(.IP_PANNEAU)
        Put #iNoOfFile, , CInt(.PositionAboutsLisses)
        .colCostCAtOperations.WriteASCII iNoOfFile
        Put #iNoOfFile, , .bIsPartOfNappe
        'Voiles
        Put #iNoOfFile, , .ProfilesSurVoiles
        Put #iNoOfFile, , CInt(.SoudureProfilesSurVoiles)
        Put #iNoOfFile, , .GoussetsProfilesVoiles
        Put #iNoOfFile, , .PlatsEnBute
        Put #iNoOfFile, , .AccostagesVoiles
        ' CarlinguesSerresHiloires
        Put #iNoOfFile, , .iNCI
        Put #iNoOfFile, , CInt(.HabillageCarlingues)
        Put #iNoOfFile, , CInt(.DimensionHabillage)
        ' Pré (nappe plane)
        Put #iNoOfFile, , .AccostagesNappes
        Put #iNoOfFile, , .iNANP
        Put #iNoOfFile, , CInt(.SoudureLissesNappes)
        Put #iNoOfFile, , CInt(.SectionLisses)
        'Pré (assemblage)
        Put #iNoOfFile, , CInt(.TypeTapes)
        Put #iNoOfFile, , CInt(.SectionTapes)
        Put #iNoOfFile, , .CouplesCarlinguesSurNappes
        Put #iNoOfFile, , .AccostagesToleBouchain
        'Pré-montage - montage
        Put #iNoOfFile, , .ContactsBarrotsCloisons
        Put #iNoOfFile, , .JonctionsBarrotHiloire
'        'Coefficients Accessibilité et Atelier groupés
'        Put #iNoOfFile, , .CoefAccPrePreVoile
'        Put #iNoOfFile, , .CoefAccPrePreNappe
'        Put #iNoOfFile, , .CoefAccPreAssInner
'        Put #iNoOfFile, , .CoefAccPreAssOuter
'        Put #iNoOfFile, , .CoefAccMontage
'        Put #iNoOfFile, , .CoefAccPonts
'        Put #iNoOfFile, , .CoefAccUsinage
'        Put #iNoOfFile, , .CoefAtPrePreVoile
'        Put #iNoOfFile, , .CoefAtPrePreNappe
'        Put #iNoOfFile, , .CoefAtPreAssInner
'        Put #iNoOfFile, , .CoefAtPreAssOuter
'        Put #iNoOfFile, , .CoefAtMontage
'        Put #iNoOfFile, , .CoefAtPonts
'        Put #iNoOfFile, , .CoefAtUsinage
    End With
    Exit Function
WritePanelsASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: Function WritePanelsASCII")
End Function

Private Sub Class_terminate()
    Set mvarcolCostCAtOperations = Nothing
    #If DebugMode Then
        Debug.Print "'" & TypeName(Me) & "' instance " & CStr(mlClassDebugID) & " is terminating"
    #End If
End Sub

#If DebugMode Then
    Public Property Get ClassDebugID()
        ClassDebugID = mlClassDebugID
    End Property
#End If
