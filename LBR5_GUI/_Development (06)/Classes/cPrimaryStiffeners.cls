VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cPrimaryStiffeners"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit
#Const DebugMode = 0
#If DebugMode Then
    Private mlClassDebugID As Long
#End If
Private mvarIndex As Integer
Private mvarSpacing As Double
Private mvarFlangeThickness As Double
Private mvarFlangeWidth As Double
Private mvarWebHeight As Double
Private mvarWebThickness As Double
Private mvarSpacingVariation As Double
Private mvarFlangeThicknessVariation As Double
Private mvarFlangeWidthVariation As Double
Private mvarWebHeightVariation As Double
Private mvarWebThicknessVariation As Double
Private mvarCorrosionThickness As Double
Private mvarDistributionMode As String
Private mvarSide As Side
Private DistributionModeLabelTable As Object
Private mvarDistance As Double
Private mvarMaterialNumber As Integer
Private mvarEfficiency As Double
Private mvarProfile As Profile
Private mvarGrossSectionModulus As Double
Private mvarGrossSectionModulusVariation As Double
Private mvarNumberOfStiffeners As Double
Public Enum Profile
    profileFlat = 1
    profileBulb = 2
    profileAngle = 3
    profileTBar = 4
    profileNull = 5
End Enum

Public Property Let index(ByVal vData As Integer)
    On Error GoTo IndexLetErr
    mvarIndex = vData
    Exit Property
IndexLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryStiffeners: Index Property Let")
End Property

Public Property Get index() As Integer
    On Error GoTo IndexGetErr
    index = mvarIndex
    Exit Property
IndexGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryStiffeners: Index Property Get")
End Property

Public Property Let Side(ByVal vData As Side)
    On Error GoTo SideLetErr
    Select Case vData
        Case SideLeft, SideRight, SideNone
        Case Else
            Exit Property
    End Select
    mvarSide = vData
    Exit Property
SideLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryStiffeners: Side Property Let")
End Property

Public Property Get Side() As Side
    On Error GoTo SideGetErr
    Side = mvarSide
    Exit Property
SideGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryStiffeners: Side Property Get")
End Property

Public Property Let Spacing(ByVal vData As Double)
    On Error GoTo SpacingLetErr
    mvarSpacing = vData
    Exit Property
SpacingLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryStiffeners: Spacing Property Let")
End Property

Public Property Get Spacing() As Double
    On Error GoTo SpacingGetErr
    Spacing = mvarSpacing
    Exit Property
SpacingGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryStiffeners: Spacing Property Get")
End Property

Public Property Let FlangeThickness(ByVal vData As Double)
    On Error GoTo FlangeThicknessLetErr
    mvarFlangeThickness = vData
    Exit Property
FlangeThicknessLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryStiffeners: FlangeThickness Property Let")
End Property

Public Property Get FlangeThickness() As Double
    On Error GoTo FlangeThicknessGetErr
    FlangeThickness = mvarFlangeThickness
    Exit Property
FlangeThicknessGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryStiffeners: FlangeThickness Property Get")
End Property

Public Property Let FlangeWidth(ByVal vData As Double)
    On Error GoTo FlangeWidthLetErr
    mvarFlangeWidth = vData
    Exit Property
FlangeWidthLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryStiffeners: FlangeWidth Property Let")
End Property

Public Property Get FlangeWidth() As Double
    On Error GoTo FlangeWidthGetErr
    FlangeWidth = mvarFlangeWidth
    Exit Property
FlangeWidthGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryStiffeners: FlangeWidth Property Get")
End Property

Public Property Let WebThickness(ByVal vData As Double)
    On Error GoTo WebThicknessLetErr
    mvarWebThickness = vData
    Exit Property
WebThicknessLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryStiffeners: WebThickness Property Let")
End Property

Public Property Get WebThickness() As Double
    On Error GoTo WebThicknessGetErr
    WebThickness = mvarWebThickness
    Exit Property
WebThicknessGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryStiffeners: WebThickness Property Get")
End Property

Public Property Let WebHeight(ByVal vData As Double)
    On Error GoTo WebHeightLetErr
    mvarWebHeight = vData
    Exit Property
WebHeightLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryStiffeners: WebHeight Property Let")
End Property

Public Property Get WebHeight() As Double
    On Error GoTo WebHeightGetErr
    WebHeight = mvarWebHeight
    Exit Property
WebHeightGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryStiffeners: WebHeight Property Get")
End Property

'------------------
Public Property Let SpacingVariation(ByVal vData As Double)
    On Error GoTo SpacingVariationLetErr
    mvarSpacingVariation = vData
    Exit Property
SpacingVariationLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryStiffeners: SpacingVariation Property Let")
End Property

Public Property Get SpacingVariation() As Double
    On Error GoTo SpacingVariationGetErr
    SpacingVariation = mvarSpacingVariation
    Exit Property
SpacingVariationGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryStiffeners: SpacingVariation Property Get")
End Property

Public Property Let FlangeThicknessVariation(ByVal vData As Double)
    On Error GoTo FlangeThicknessVariationLetErr
    mvarFlangeThicknessVariation = vData
    Exit Property
FlangeThicknessVariationLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryStiffeners: FlangeThicknessVariation Property Let")
End Property

Public Property Get FlangeThicknessVariation() As Double
    On Error GoTo FlangeThicknessVariationGetErr
    FlangeThicknessVariation = mvarFlangeThicknessVariation
    Exit Property
FlangeThicknessVariationGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryStiffeners: FlangeThicknessVariation Property Get")
End Property

Public Property Let FlangeWidthVariation(ByVal vData As Double)
    On Error GoTo FlangeWidthVariationLetErr
    mvarFlangeWidthVariation = vData
    Exit Property
FlangeWidthVariationLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryStiffeners: FlangeWidthVariation Property Let")
End Property

Public Property Get FlangeWidthVariation() As Double
    On Error GoTo FlangeWidthVariationGetErr
    FlangeWidthVariation = mvarFlangeWidthVariation
    Exit Property
FlangeWidthVariationGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryStiffeners: FlangeWidthVariation Property Get")
End Property

Public Property Let WebThicknessVariation(ByVal vData As Double)
    On Error GoTo WebThicknessVariationLetErr
    mvarWebThicknessVariation = vData
    Exit Property
WebThicknessVariationLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryStiffeners: WebThicknessVariation Property Let")
End Property

Public Property Get WebThicknessVariation() As Double
    On Error GoTo WebThicknessVariationGetErr
    WebThicknessVariation = mvarWebThicknessVariation
    Exit Property
WebThicknessVariationGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryStiffeners: WebThicknessVariation Property Get")
End Property

Public Property Let WebHeightVariation(ByVal vData As Double)
    On Error GoTo WebHeightVariationLetErr
    mvarWebHeightVariation = vData
    Exit Property
WebHeightVariationLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryStiffeners: WebHeightVariation Property Let")
End Property

Public Property Get WebHeightVariation() As Double
    On Error GoTo WebHeightVariationGetErr
    WebHeightVariation = mvarWebHeightVariation
    Exit Property
WebHeightVariationGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryStiffeners: WebHeightVariation Property Get")
End Property
'------------------
Public Property Let CorrosionThickness(ByVal vData As Double)
    On Error GoTo CorrosionThicknessLetErr
    mvarCorrosionThickness = vData
    Exit Property
CorrosionThicknessLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryStiffeners: CorrosionThickness Property Let")
End Property

Public Property Get CorrosionThickness() As Double
    On Error GoTo CorrosionThicknessGetErr
    CorrosionThickness = mvarCorrosionThickness
    Exit Property
CorrosionThicknessGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryStiffeners: CorrosionThickness Property Get")
End Property

Public Property Let DistributionMode(ByVal vData As String)
    On Error GoTo DistributionModeLetErr
    Select Case vData
        Case "EE1", "EE2"
        Case Else
            Exit Property
    End Select
    mvarDistributionMode = vData
    Exit Property
DistributionModeLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryStiffeners, DistributionMode Property Let")
End Property

Public Property Get DistributionMode() As String
    On Error GoTo DistributionModeGetErr
    DistributionMode = mvarDistributionMode
    Exit Property
DistributionModeGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryStiffeners, DistributionMode Property Get")
End Property

Public Property Let Distance(ByVal vData As Double)
    On Error GoTo DistanceLetErr
    mvarDistance = vData
    Exit Property
DistanceLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryStiffeners: Distance Property Let")
End Property

Public Property Get Distance() As Double
    On Error GoTo DistanceGetErr
    Distance = mvarDistance
    Exit Property
DistanceGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryStiffeners: Distance Property Get")
End Property

Public Property Let Efficiency(ByVal vData As Double)
    On Error GoTo EfficiencyLetErr
    mvarEfficiency = vData
    Exit Property
EfficiencyLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryStiffeners: Efficiency Property Let")
End Property

Public Property Get Efficiency() As Double
    On Error GoTo EfficiencyGetErr
    Efficiency = mvarEfficiency
    Exit Property
EfficiencyGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryStiffeners: Efficiency Property Get")
End Property

Public Property Let GrossSectionModulus(ByVal vData As Double)
    On Error GoTo GrossSectionModulusLetErr
    mvarGrossSectionModulus = vData
    Exit Property
GrossSectionModulusLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryStiffeners: GrossSectionModulus Property Let")
End Property

Public Property Get GrossSectionModulus() As Double
    On Error GoTo GrossSectionModulusGetErr
    GrossSectionModulus = mvarGrossSectionModulus
    Exit Property
GrossSectionModulusGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryStiffeners: GrossSectionModulus Property Get")
End Property

Public Property Let GrossSectionModulusVariation(ByVal vData As Double)
    On Error GoTo GrossSectionModulusVariationLetErr
    mvarGrossSectionModulusVariation = vData
    Exit Property
GrossSectionModulusVariationLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryStiffeners: GrossSectionModulusVariation Property Let")
End Property

Public Property Get GrossSectionModulusVariation() As Double
    On Error GoTo GrossSectionModulusVariationGetErr
    GrossSectionModulusVariation = mvarGrossSectionModulusVariation
    Exit Property
GrossSectionModulusVariationGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryStiffeners: GrossSectionModulusVariation Property Get")
End Property

Public Property Let MaterialNumber(ByVal vData As Integer)
    On Error GoTo MaterialNumberLetErr
    mvarMaterialNumber = vData
    Exit Property
MaterialNumberLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryStiffeners: MaterialNumber Property Let")
End Property

Public Property Get MaterialNumber() As Integer
    On Error GoTo MaterialNumberGetErr
    MaterialNumber = mvarMaterialNumber
    Exit Property
MaterialNumberGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryStiffeners: MaterialNumber Property Get")
End Property

Public Property Let Profile(ByVal vData As Profile)
    On Error GoTo ProfileLetErr
    mvarProfile = vData
    Exit Property
ProfileLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSecondaryFrames: Profile Property Let")
End Property

Public Property Get Profile() As Profile
    On Error GoTo ProfileGetErr
    Profile = mvarProfile
    Exit Property
ProfileGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSecondaryFrames: Profile Property Get")
End Property

Public Property Let NumberOfStiffeners(ByVal vData As Double)
    On Error GoTo NumberOfStiffenersLetErr
    mvarNumberOfStiffeners = vData
    Exit Property
NumberOfStiffenersLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSecondaryFrames: NumberOfStiffeners Property Let")
End Property

Public Property Get NumberOfStiffeners() As Double
    On Error GoTo NumberOfStiffenersGetErr
    NumberOfStiffeners = mvarNumberOfStiffeners
    Exit Property
NumberOfStiffenersGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSecondaryFrames: NumberOfStiffeners Property Get")
End Property



Public Function ReadMARS_LBR5TransfertFile(ts As TextStream)
    On Error GoTo ReadMARS_LBR5TransfertFileErr
    Dim sLine As String
    Dim v() As Variant
    sLine = ReadLn(ts)
    GetValues 11, sLine, v
    Me.index = Val_(v(1))
    Select Case v(2)
    Case "F"
        Me.Profile = profileFlat
    Case "B"
        Me.Profile = profileBulb
    Case "A"
        Me.Profile = profileAngle
    Case "T"
        Me.Profile = profileTBar
    Case "N"
        Me.Profile = profileNull
    End Select
    Me.Distance = Val_(v(3))
    'Me.DistributionMode = "EE2"
    Me.WebHeight = Val_(v(4)) / 1000
    Me.WebThickness = Val_(v(5)) / 1000
    Me.FlangeWidth = Val_(v(6)) / 1000
    Me.FlangeThickness = Val_(v(7)) / 1000
    If Me.Profile = profileBulb Then
        GetBVRuleBulbEquivalentSection mvarWebHeight, mvarWebThickness, mvarFlangeWidth, mvarFlangeThickness
    End If
    Me.CorrosionThickness = Val_(v(8)) / 1000
    Select Case Val_(v(9))
        Case 1
            Me.Side = SideRight
        Case 2
            Me.Side = SideLeft
    End Select
    'Me.Side = Val_(V(9))
    Me.MaterialNumber = Val_(v(10))
    Me.Efficiency = Val_(v(11))
    Exit Function
ReadMARS_LBR5TransfertFileErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryFrames: Function ReadMARS_LBR5TransfertFileErr")
End Function

Public Function Clone() As cPrimaryStiffeners
    On Error GoTo CloneErr
    Set Clone = New cPrimaryStiffeners
    Clone.index = Me.index
    Clone.Spacing = Me.Spacing
    Clone.WebHeight = Me.WebHeight
    Clone.WebThickness = Me.WebThickness
    Clone.FlangeWidth = Me.FlangeWidth
    Clone.FlangeThickness = Me.FlangeThickness
    Clone.SpacingVariation = Me.SpacingVariation
    Clone.WebHeightVariation = Me.WebHeightVariation
    Clone.WebThicknessVariation = Me.WebThicknessVariation
    Clone.FlangeWidthVariation = Me.FlangeWidthVariation
    Clone.FlangeThicknessVariation = Me.FlangeThicknessVariation
    Clone.CorrosionThickness = Me.CorrosionThickness
    Clone.Side = Me.Side
    Clone.DistributionMode = Me.DistributionMode
    Clone.Distance = Me.Distance
    Clone.Efficiency = Me.Efficiency
    Clone.MaterialNumber = Me.MaterialNumber
    Clone.GrossSectionModulus = Me.GrossSectionModulus
    Clone.GrossSectionModulusVariation = Me.GrossSectionModulusVariation
    Clone.NumberOfStiffeners = Me.NumberOfStiffeners
    Exit Function
CloneErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryStiffeners: Function Clone")
End Function

Public Function WriteASCII(ByVal iNoOfFile As Integer)
    On Error GoTo WriteASCIIErr
    Dim ss As String * 3
    With Me
        Put #iNoOfFile, , .Spacing
        Put #iNoOfFile, , .WebHeight
        Put #iNoOfFile, , .WebThickness
        Put #iNoOfFile, , .FlangeWidth
        Put #iNoOfFile, , .FlangeThickness
        Put #iNoOfFile, , .CorrosionThickness
        ss = .DistributionMode
        Put #iNoOfFile, , ss
        Put #iNoOfFile, , CInt(.Side)
        Put #iNoOfFile, , .SpacingVariation
        Put #iNoOfFile, , .WebHeightVariation
        Put #iNoOfFile, , .WebThicknessVariation
        Put #iNoOfFile, , .FlangeWidthVariation
        Put #iNoOfFile, , .FlangeThicknessVariation
    End With
    Exit Function
WriteASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryStiffeners: Function WriteASCII")
End Function

Public Function ReadASCII(ByVal iNoOfFile As Integer)
    On Error GoTo ReadASCIIErr
    Dim ii As Integer, dd As Double, ss As String * 3
    With Me
        Get #iNoOfFile, , dd: .Spacing = dd
        Get #iNoOfFile, , dd: .WebHeight = dd
        Get #iNoOfFile, , dd: .WebThickness = dd
        Get #iNoOfFile, , dd: .FlangeWidth = dd
        Get #iNoOfFile, , dd: .FlangeThickness = dd
        Get #iNoOfFile, , dd: .CorrosionThickness = dd
        Get #iNoOfFile, , ss: .DistributionMode = ss
        Get #iNoOfFile, , ii: .Side = ii
    End With
    Exit Function
ReadASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryStiffeners: Function ReadASCII")
End Function

Public Function GetBVRuleBulbEquivalentSection(ByRef HW As Double, ByRef Tw As Double, ByRef Bf As Double, ByRef Tf As Double)
    'According to Bureau Veritas 2003 specifications
    On Error GoTo GetBVRuleBulbEquivalentSectionErr
    Dim Hw_b As Double, Tw_b As Double
    Dim multi As Integer
    multi = 1000
    HW = HW * multi
    Tw = Tw * multi
    Hw_b = HW
    Tw_b = Tw
    Dim alfa As Double
    HW = Hw_b - Hw_b / 9.2 + 2
    Tw = Tw_b
    Select Case Hw_b
        Case Is <= 120
            alfa = 1.1 + ((120 - Hw_b) ^ 2) / 3000
        Case Is > 120
            alfa = 1
    End Select
    Bf = alfa * (Tw_b + Hw_b / 6.7 - 2)
    Tf = Hw_b / 9.2 - 2
    HW = CInt(HW) / multi
    Tw = CInt(Tw) / multi
    Bf = CInt(Bf) / multi
    Tf = CInt(Tf) / multi
    Exit Function
GetBVRuleBulbEquivalentSectionErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryStiffeners: Function GetBVRuleBulbEquivalentSection")
End Function

Public Function GetNoOfStiffeners(ByRef Panel As cPanel) As Integer
    On Error GoTo GetNoOfStiffenersErr
    Select Case Me.DistributionMode
        Case "EE1"
            GetNoOfStiffeners = CLng(Divide(Panel.cGeometry.PanelWidth, Panel.cScantlings.cPrimaryStiffeners.Spacing)) - 1
        Case "EE2"
            GetNoOfStiffeners = CLng(Divide(Panel.cGeometry.PanelWidth, Panel.cScantlings.cPrimaryStiffeners.Spacing))
    End Select
    Exit Function
GetNoOfStiffenersErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryStiffeners: Function GetNoOfStiffeners")
End Function

Private Sub Class_Initialize()
    #If DebugMode Then
        mlClassDebugID = GetNextClassDebugID()
        Debug.Print "'" & TypeName(Me) & "' instance " & mlClassDebugID & " created"
    #End If
End Sub

Private Sub Class_terminate()
    #If DebugMode Then
        Debug.Print "'" & TypeName(Me) & "' instance " & CStr(mlClassDebugID) & " is terminating"
    #End If
End Sub

#If DebugMode Then
    Public Property Get ClassDebugID()
        ClassDebugID = mlClassDebugID
    End Property
#End If
