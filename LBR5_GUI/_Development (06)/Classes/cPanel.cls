VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cPanel"
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
Private mvarpLabel As String
Private mvarSelected As SelectionState
Private mvarpType As PanelType
Private mvarRelatedDoubleHullPanel As Integer
Private mvarRegion As Long
Private mvarpNumber As Integer
Private mvarcGeometry As cGeometry
Private mvarcScantlings As cScantlings
Private mvarcMaterial As cMaterial
Private mvarcCostCAtMain As cCostCAtMain
Private mvarIsBuoyancyForce As YesNo
Private mvarLocalizedPressure As Double
Private mvarUniformLateralPressureVariation As LateralPressureVariation
Private mvarIsWiseLateralPressure As YesNo
Private mvarIsStructuralConstraints As YesNo
Private mvarIsDesignVariables As YesNo
Private mvarBeamSection As BeamSection
Private mvarBucklingLength As Double
Private mvarLeftCompartment As Integer
Private mvarRightCompartment As Integer
Private mvarLateralPressureSide As Side
Private mvarNoOfGeomConstr As Integer
Private mvarFramesFlangeThicknessUpdate As FlangeThicknessUpdate
Private mvarStiffenersFlangeThicknessUpdate As FlangeThicknessUpdate
Private mvarHighLighted As YesNo
'Class Collections
Private mvarcolConnections As Collection
Private mvarcolDesignVariables As colDesignVariables
Private mvarcolLoadCase As colLoadCase
Private mvarcolStrakes As colStrakes
Private mvarcolSecondaryFrames As colSecondaryFrames
Private mvarcolPrimaryStiffeners As colPrimaryStiffeners
Private mvarcolMarsLoadCase As colMarsLoadCase
'Private mvarcolGirder As colGirder
Private mvarcolAssessmentPoints As colAssessmentPoints
Private mvarcolGeometricalConstraints As colGeometricalConstraints
Private mvarcolBoundaryConditions As colBoundaryConditions
Private mvarcolUpdatedScantlings As colUpdatedScantlings
Private mvarVisible As Boolean


Private Const PANELCONNECTIONSTRINGKEY As String = "PanelConnection"
'Class Enumerations
Public Enum PanelType
    Plate
    Beam
    Shell_
    DoubleHull
End Enum
Public Enum BeamSection
    bsCircle
    bsSquare
    bsDoubleT
End Enum
Public Enum SelectionState
    IsUnselected = 0
    isSelected = 1
End Enum
Public Enum LateralPressureVariation
    IsImplicit = 0
    IsExplicit = 1
End Enum
Public Enum FlangeThicknessUpdate
    NoUpdate = 0
    UpdateLshape = 1
    UpdateTshape = 2
End Enum

'Public Enum PositionCode
'    KeelPlate = 1
'    bottom = 2
'    InnerBottom = 3
'    Bilge = 4
'    SideShell_BelowFreeboardDeck = 5
'    SideShell_AboveFreeboardDeck = 6
'    InnerHull = 7
'    ...
'End Enum

' Class member properties
Public Property Let IsWiseLateralPressure(ByVal vData As YesNo)
    On Error GoTo IsWiseLateralPressureLetErr
    mvarIsWiseLateralPressure = vData
    Exit Property
IsWiseLateralPressureLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: IsWiseLateralPressure Property Let")
End Property

Public Property Get IsWiseLateralPressure() As YesNo
    On Error GoTo IsWiseLateralPressureGetErr
    IsWiseLateralPressure = mvarIsWiseLateralPressure
    Exit Property
IsWiseLateralPressureGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: IsWiseLateralPressure Property Get")
End Property

Public Property Let IsDesignVariables(ByVal vData As YesNo)
    On Error GoTo IsDesignVariablesLetErr
    mvarIsDesignVariables = vData
    Exit Property
IsDesignVariablesLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: IsDesignVariables Property Let")
End Property

Public Property Get IsDesignVariables() As YesNo
    On Error GoTo IsDesignVariablesGetErr
    IsDesignVariables = mvarIsDesignVariables
    Exit Property
IsDesignVariablesGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: IsDesignVariables Property Get")
End Property

Public Property Let UniformLateralPressureVariation(ByVal vData As LateralPressureVariation)
    On Error GoTo UniformLateralPressureVariationLetErr
    mvarUniformLateralPressureVariation = vData
    Exit Property
UniformLateralPressureVariationLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: UniformLateralPressureVariation Property Let")
End Property

Public Property Get UniformLateralPressureVariation() As LateralPressureVariation
    On Error GoTo UniformLateralPressureVariationGetErr
    UniformLateralPressureVariation = mvarUniformLateralPressureVariation
    Exit Property
UniformLateralPressureVariationGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: UniformLateralPressureVariation Property Get")
End Property

Public Property Let LocalizedPressure(ByVal vData As Double)
    On Error GoTo LocalizedPressureLetErr
    mvarLocalizedPressure = vData
    Exit Property
LocalizedPressureLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: LocalizedPressure Property Let")
End Property

Public Property Get LocalizedPressure() As Double
    On Error GoTo LocalizedPressureGetErr
    LocalizedPressure = mvarLocalizedPressure
    Exit Property
LocalizedPressureGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: LocalizedPressure Property Get")
End Property

Public Property Let IsBuoyancyForce(ByVal vData As YesNo)
    On Error GoTo IsBuoyancyForceLetErr
    mvarIsBuoyancyForce = vData
    Exit Property
IsBuoyancyForceLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: IsBuoyancyForce Property Let")
End Property

Public Property Get IsBuoyancyForce() As YesNo
    On Error GoTo IsBuoyancyForceGetErr
    IsBuoyancyForce = mvarIsBuoyancyForce
    Exit Property
IsBuoyancyForceGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: IsBuoyancyForce Property Get")
End Property

Public Property Let HighLighted(ByVal vData As YesNo)
    On Error GoTo HighLightedLetErr
    mvarHighLighted = vData
    Exit Property
HighLightedLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: HighLighted Property Let")
End Property

Public Property Get HighLighted() As YesNo
    On Error GoTo HighLightedGetErr
    HighLighted = mvarHighLighted
    Exit Property
HighLightedGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: HighLighted Property Get")
End Property

Public Property Get cMaterial() As cMaterial
    On Error GoTo cMaterialGetErr
    Set cMaterial = mvarcMaterial
    Exit Property
cMaterialGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: cMaterial Property Get")
End Property

Public Property Set cMaterial(vData As cMaterial)
    On Error GoTo cMaterialSetErr
    Set mvarcMaterial = vData
    Exit Property
cMaterialSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: cMaterial Property Set")
End Property

Public Property Get cScantlings() As cScantlings
    On Error GoTo cScantlingsGetErr
    Set cScantlings = mvarcScantlings
    Exit Property
cScantlingsGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: cScantlings Property Get")
End Property

Public Property Set cScantlings(vData As cScantlings)
    On Error GoTo cScantlingsSetErr
    Set mvarcScantlings = vData
    Exit Property
cScantlingsSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: cScantlings Property Set")
End Property

Public Property Get cGeometry() As cGeometry
    On Error GoTo cGeometryGetErr
    Set cGeometry = mvarcGeometry
    Exit Property
cGeometryGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: cGeometry Property Get")
End Property

Public Property Set cGeometry(vData As cGeometry)
    On Error GoTo cGeometrySetErr
    Set mvarcGeometry = vData
    Exit Property
cGeometrySetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: cGeometry Property Set")
End Property

Public Property Get cCostCAtMain() As cCostCAtMain
    On Error GoTo cCostCAtMainGetErr
    Set cCostCAtMain = mvarcCostCAtMain
    Exit Property
cCostCAtMainGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: cCostCAtMain Property Get")
End Property

Public Property Set cCostCAtMain(vData As cCostCAtMain)
    On Error GoTo cCostCAtMainSetErr
    Set mvarcCostCAtMain = vData
    Exit Property
cCostCAtMainSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: cCostCAtMain Property Set")
End Property

Public Property Let pNumber(ByVal vData As Integer)
    On Error GoTo pNumberLetErr
    mvarpNumber = vData
    Exit Property
pNumberLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: pNumber Property Let")
End Property

Public Property Get pNumber() As Integer
    On Error GoTo pNumberGetErr
    pNumber = mvarpNumber
    Exit Property
pNumberGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: pNumber Property Get")
End Property

Public Property Let Region(ByVal vData As Long)
    On Error GoTo RegionLetErr
    mvarRegion = vData
    Exit Property
RegionLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: Region Property Let")
End Property

Public Property Get Region() As Long
    On Error GoTo RegionGetErr
    Region = mvarRegion
    Exit Property
RegionGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: Region Property Get")
End Property

Public Property Let pType(ByVal vData As PanelType)
    On Error GoTo pTypeLetErr
    mvarpType = vData
    Exit Property
pTypeLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: pType Property Let")
End Property

Public Property Get pType() As PanelType
    On Error GoTo pTypeGetErr
    pType = mvarpType
    Exit Property
pTypeGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: pType Property Get")
End Property

Public Property Let RelatedDoubleHullPanel(ByVal vData As Integer)
    On Error GoTo RelatedDoubleHullPanelLetErr
    mvarRelatedDoubleHullPanel = vData
    Exit Property
RelatedDoubleHullPanelLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: RelatedDoubleHullPanel Property Let")
End Property

Public Property Get RelatedDoubleHullPanel() As Integer
    On Error GoTo RelatedDoubleHullPanelGetErr
    RelatedDoubleHullPanel = mvarRelatedDoubleHullPanel
    Exit Property
RelatedDoubleHullPanelGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: RelatedDoubleHullPanel Property Get")
End Property

Public Property Let Selected(ByVal vData As SelectionState)
    On Error GoTo SelectedLetErr
    mvarSelected = vData
    Exit Property
SelectedLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: Selected Property Let")
End Property

Public Property Get Selected() As SelectionState
    On Error GoTo SelectedGetErr
    Selected = mvarSelected
    Exit Property
SelectedGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: Selected Property Get")
End Property

Public Property Let index(ByVal vData As Integer)
    On Error GoTo IndexLetErr
    mvarIndex = vData
    Exit Property
IndexLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: Index Property Let")
End Property

Public Property Get index() As Integer
    On Error GoTo IndexGetErr
    index = mvarIndex
    Exit Property
IndexGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: Index Property Get")
End Property

Public Property Let pLabel(ByVal vData As String)
    On Error GoTo pLabelLetErr
    mvarpLabel = vData
    Exit Property
pLabelLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: pLabel Property Let")
End Property

Public Property Get pLabel() As String
    On Error GoTo pLabelGetErr
    pLabel = mvarpLabel
    Exit Property
pLabelGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: pLabel Property Get")
End Property

Public Property Let IsStructuralConstraints(ByVal vData As YesNo)
    On Error GoTo IsStructuralConstraintsLetErr
    mvarIsStructuralConstraints = vData
    Exit Property
IsStructuralConstraintsLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: IsStructuralConstraints Property Let")
End Property

Public Property Get IsStructuralConstraints() As YesNo
    On Error GoTo IsStructuralConstraintsGetErr
    IsStructuralConstraints = mvarIsStructuralConstraints
    Exit Property
IsStructuralConstraintsGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: IsStructuralConstraints Property Get")
End Property

Public Property Let LeftCompartment(ByVal vData As Integer)
    On Error GoTo LeftCompartmentLetErr
    mvarLeftCompartment = vData
    Exit Property
LeftCompartmentLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: LeftCompartment Property Let")
End Property

Public Property Get LeftCompartment() As Integer
    On Error GoTo LeftCompartmentGetErr
    LeftCompartment = mvarLeftCompartment
    Exit Property
LeftCompartmentGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: LeftCompartment Property Get")
End Property

Public Property Let RightCompartment(ByVal vData As Integer)
    On Error GoTo RightCompartmentLetErr
    mvarRightCompartment = vData
    Exit Property
RightCompartmentLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: RightCompartment Property Let")
End Property

Public Property Get RightCompartment() As Integer
    On Error GoTo RightCompartmentGetErr
    RightCompartment = mvarRightCompartment
    Exit Property
RightCompartmentGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: RightCompartment Property Get")
End Property

Public Property Get LateralPressureSide() As Side
    On Error GoTo LateralPressureSideGetErr
    LateralPressureSide = mvarLateralPressureSide
    Exit Property
LateralPressureSideGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: LateralPressureSide Property Get")
End Property

Public Property Let LateralPressureSide(ByVal vData As Side)
    On Error GoTo LateralPressureSideLetErr
    Select Case vData
        Case SideNone, SideLeft, SideRight
        Case Else
            Exit Property
    End Select
    mvarLateralPressureSide = vData
    Exit Property
LateralPressureSideLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: LateralPressureSide Property Let")
End Property

Public Property Get NoOfGeomConstr() As Integer
    On Error GoTo NoOfGeomConstrGetErr
    NoOfGeomConstr = mvarNoOfGeomConstr
    Exit Property
NoOfGeomConstrGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: NoOfGeomConstr Property Get")
End Property

Public Property Let NoOfGeomConstr(ByVal vData As Integer)
    On Error GoTo NoOfGeomConstrLetErr
    mvarNoOfGeomConstr = vData
    Exit Property
NoOfGeomConstrLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: NoOfGeomConstr Property Let")
End Property

Public Property Let FramesFlangeThicknessUpdate(ByVal vData As FlangeThicknessUpdate)
    On Error GoTo FramesFlangeThicknessUpdateLetErr
    mvarFramesFlangeThicknessUpdate = vData
    Exit Property
FramesFlangeThicknessUpdateLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGeometricalConstraints: FramesFlangeThicknessUpdate Property Let")
End Property

Public Property Get FramesFlangeThicknessUpdate() As FlangeThicknessUpdate
    On Error GoTo FramesFlangeThicknessUpdateGetErr
    FramesFlangeThicknessUpdate = mvarFramesFlangeThicknessUpdate
    Exit Property
FramesFlangeThicknessUpdateGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGeometricalConstraints: FramesFlangeThicknessUpdate Property Get")
End Property

Public Property Let StiffenersFlangeThicknessUpdate(ByVal vData As FlangeThicknessUpdate)
    On Error GoTo StiffenersFlangeThicknessUpdateLetErr
    mvarStiffenersFlangeThicknessUpdate = vData
    Exit Property
StiffenersFlangeThicknessUpdateLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGeometricalConstraints: StiffenersFlangeThicknessUpdate Property Let")
End Property

Public Property Get StiffenersFlangeThicknessUpdate() As FlangeThicknessUpdate
    On Error GoTo StiffenersFlangeThicknessUpdateGetErr
    StiffenersFlangeThicknessUpdate = mvarStiffenersFlangeThicknessUpdate
    Exit Property
StiffenersFlangeThicknessUpdateGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGeometricalConstraints: StiffenersFlangeThicknessUpdate Property Get")
End Property

Public Property Let Visible(ByVal vData As Boolean)
    On Error GoTo VisibleLetErr
    mvarVisible = vData
    Exit Property
VisibleLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: Visible Property Let")
End Property

Public Property Get Visible() As Boolean
    On Error GoTo VisibleGetErr
    Visible = mvarVisible
    Exit Property
VisibleGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: Visible Property Get")
End Property

'Class collections properties
Public Property Get colDesignVariables() As colDesignVariables
    On Error GoTo colDesignVariablesGetErr
    If mvarcolDesignVariables Is Nothing Then
        Set mvarcolDesignVariables = New colDesignVariables
    End If
    Set colDesignVariables = mvarcolDesignVariables
    Exit Property
colDesignVariablesGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: colDesignVariables Property Get")
End Property

Public Property Set colDesignVariables(vData As colDesignVariables)
    On Error GoTo colDesignVariablesSetErr
    Set mvarcolDesignVariables = vData
    Exit Property
colDesignVariablesSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: colDesignVariables Property Set")
End Property

Public Property Get colLoadCase() As colLoadCase
    On Error GoTo colLoadCaseGetErr
    If mvarcolLoadCase Is Nothing Then
        Set mvarcolLoadCase = New colLoadCase
    End If
    Set colLoadCase = mvarcolLoadCase
    Exit Property
colLoadCaseGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: colLoadCase Property Get")
End Property

Public Property Set colLoadCase(vData As colLoadCase)
    On Error GoTo colLoadCaseSetErr
    Set mvarcolLoadCase = vData
    Exit Property
colLoadCaseSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: colLoadCase Property Set")
End Property

Public Property Get colStrakes() As colStrakes
    On Error GoTo colStrakesGetErr
    If mvarcolStrakes Is Nothing Then
        Set mvarcolStrakes = New colStrakes
    End If
    Set colStrakes = mvarcolStrakes
    Exit Property
colStrakesGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: colStrakes Property Get")
End Property

Public Property Set colStrakes(vData As colStrakes)
    On Error GoTo colStrakesSetErr
    Set mvarcolStrakes = vData
    Exit Property
colStrakesSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: colStrakes Property Set")
End Property

Public Property Get colSecondaryFrames() As colSecondaryFrames
    On Error GoTo colSecondaryFramesGetErr
    If mvarcolSecondaryFrames Is Nothing Then
        Set mvarcolSecondaryFrames = New colSecondaryFrames
    End If
    Set colSecondaryFrames = mvarcolSecondaryFrames
    Exit Property
colSecondaryFramesGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: colSecondaryFrames Property Get")
End Property

Public Property Set colSecondaryFrames(vData As colSecondaryFrames)
    On Error GoTo colSecondaryFramesSetErr
    Set mvarcolSecondaryFrames = vData
    Exit Property
colSecondaryFramesSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: colSecondaryFrames Property Set")
End Property

Public Property Get colPrimaryStiffeners() As colPrimaryStiffeners
    On Error GoTo colPrimaryStiffenersGetErr
    If mvarcolPrimaryStiffeners Is Nothing Then
        Set mvarcolPrimaryStiffeners = New colPrimaryStiffeners
    End If
    Set colPrimaryStiffeners = mvarcolPrimaryStiffeners
    Exit Property
colPrimaryStiffenersGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: colPrimaryStiffeners Property Get")
End Property

Public Property Set colPrimaryStiffeners(vData As colPrimaryStiffeners)
    On Error GoTo colPrimaryStiffenersSetErr
    Set mvarcolPrimaryStiffeners = vData
    Exit Property
colPrimaryStiffenersSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: colPrimaryStiffeners Property Set")
End Property

Public Property Get colMarsLoadCase() As colMarsLoadCase
    On Error GoTo colMarsLoadCaseGetErr
    If mvarcolMarsLoadCase Is Nothing Then
        Set mvarcolMarsLoadCase = New colMarsLoadCase
    End If
    Set colMarsLoadCase = mvarcolMarsLoadCase
    Exit Property
colMarsLoadCaseGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: colMarsLoadCase Property Get")
End Property

Public Property Set colMarsLoadCase(vData As colMarsLoadCase)
    On Error GoTo colMarsLoadCaseSetErr
    Set mvarcolMarsLoadCase = vData
    Exit Property
colMarsLoadCaseSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: colMarsLoadCase Property Set")
End Property

Public Property Get colAssessmentPoints() As colAssessmentPoints
    On Error GoTo colAssessmentPointsGetErr
    If mvarcolAssessmentPoints Is Nothing Then
        Set mvarcolAssessmentPoints = New Collection
    End If
    Set colAssessmentPoints = mvarcolAssessmentPoints
    Exit Property
colAssessmentPointsGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: colAssessmentPoints Property Get")
End Property

Public Property Set colAssessmentPoints(vData As colAssessmentPoints)
    On Error GoTo colAssessmentPointsSetErr
    Set mvarcolAssessmentPoints = vData
    Exit Property
colAssessmentPointsSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: colAssessmentPoints Property Set")
End Property

'Public Function AddAssessmentPoint(ByVal p As Double, i As Integer)
'    On Error GoTo  AddAssessmentPointErr
'    colAssessmentPoints.Add p
'    Exit Function
'AddAssessmentPointErr:
'    Call RaiseError(MyUnhandledError,Err.Description & "." & vbCrLf &  "cPanel: Function AddAssessmentPoint")
'End Function

Public Property Get colConnections() As Collection
    On Error GoTo colConnectionsGetErr
    If mvarcolConnections Is Nothing Then
        Set mvarcolConnections = New Collection
    End If
    Set colConnections = mvarcolConnections
    Exit Property
colConnectionsGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: colConnections Property Get")
End Property

Public Property Set colConnections(vData As Collection)
    On Error GoTo colConnectionsSetErr
    Set mvarcolConnections = vData
    Exit Property
colConnectionsSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: colConnections Property Set")
End Property

Public Property Get colGeometricalConstraints() As colGeometricalConstraints
    On Error GoTo colGeometricalConstraintsGetErr
    If mvarcolGeometricalConstraints Is Nothing Then
        Set mvarcolGeometricalConstraints = New colGeometricalConstraints
    End If
    Set colGeometricalConstraints = mvarcolGeometricalConstraints
    Exit Property
colGeometricalConstraintsGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: colGeometricalConstraints Property Get")
End Property

Public Property Set colGeometricalConstraints(vData As colGeometricalConstraints)
    On Error GoTo colGeometricalConstraintsSetErr
    Set mvarcolGeometricalConstraints = vData
    Exit Property
colGeometricalConstraintsSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: colGeometricalConstraints Property Set")
End Property

Public Property Get colBoundaryConditions() As colBoundaryConditions
    On Error GoTo colBoundaryConditionsGetErr
    If mvarcolBoundaryConditions Is Nothing Then
        Set mvarcolBoundaryConditions = New colBoundaryConditions
    End If
    Set colBoundaryConditions = mvarcolBoundaryConditions
    Exit Property
colBoundaryConditionsGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: colBoundaryConditions Property Get")
End Property

Public Property Set colBoundaryConditions(vData As colBoundaryConditions)
    On Error GoTo colBoundaryConditionsSetErr
    Set mvarcolBoundaryConditions = vData
    Exit Property
colBoundaryConditionsSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: colBoundaryConditions Property Set")
End Property

Public Property Get colUpdatedScantlings() As colUpdatedScantlings
    On Error GoTo colUpdatedScantlingsGetErr
    If mvarcolUpdatedScantlings Is Nothing Then
        Set mvarcolUpdatedScantlings = New colUpdatedScantlings
    End If
    Set colUpdatedScantlings = mvarcolUpdatedScantlings
    Exit Property
colUpdatedScantlingsGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: colUpdatedScantlings Property Get")
End Property

Public Property Set colUpdatedScantlings(vData As colUpdatedScantlings)
    On Error GoTo colUpdatedScantlingsSetErr
    Set mvarcolUpdatedScantlings = vData
    Exit Property
colUpdatedScantlingsSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: colUpdatedScantlings Property Set")
End Property

Public Function ReadLBR5txtFile(ts As TextStream)
    'On Error GoTo  ReadLBR5txtFileErr
    Dim sLine As String
    Dim v() As Variant
    Dim i As Integer
    Dim j As Integer
    Dim OBJ As Object
    Dim col As Collection
NextLine:
        sLine = ReadLn(ts)
        sLine = LTrim(sLine)
        Select Case Left(UCase(sLine), 5)
            Case "PLAQU"
                Me.pType = Plate
            Case "EPONT"
                Me.pType = Beam
            Case "COQUE"
                Err.Description = "SHELL element not implemented."
                Err.Raise 9999, , Err.Description
            Case "DCOQU"
                Me.pType = DoubleHull
            Case Else
            GoTo NextLine
        End Select
        sLine = ReadLn(ts)
        GetValues 2, sLine, v
        Me.cGeometry.PanelWidth = Abs(Val_(v(1)))
        Me.cScantlings.NetThickness = Val_(v(2))
        Select Case Me.pType
            Case Plate, DoubleHull
                sLine = ReadLn(ts)
                GetValues 5, sLine, v
                Me.cScantlings.cPrimaryFrames.Spacing = Val_(v(1))
                Me.cScantlings.cPrimaryStiffeners.Spacing = Val_(v(2))
                Me.cScantlings.cSecondaryFrames.Spacing = Val_(v(3))
                Me.cScantlings.cSecondaryStiffeners.Spacing = Val_(v(4))
                Me.cScantlings.CorrosionThickness = Val_(v(5))
                Me.cScantlings.GrossThickness = Me.cScantlings.NetThickness + Me.cScantlings.CorrosionThickness
                sLine = ReadLn(ts)
                GetValues 5, sLine, v
                Me.cScantlings.cPrimaryFrames.WebHeight = Val_(v(1))
                Me.cScantlings.cPrimaryFrames.WebThickness = Val_(v(2))
                Me.cScantlings.cPrimaryFrames.FlangeWidth = Val_(v(3))
                Me.cScantlings.cPrimaryFrames.FlangeThickness = Val_(v(4))
                Me.cScantlings.cPrimaryFrames.CorrosionThickness = Val_(v(5))
                sLine = ReadLn(ts)
                GetValues 5, sLine, v
                Me.cScantlings.cPrimaryStiffeners.WebHeight = Val_(v(1))
                Me.cScantlings.cPrimaryStiffeners.WebThickness = Val_(v(2))
                Me.cScantlings.cPrimaryStiffeners.FlangeWidth = Val_(v(3))
                Me.cScantlings.cPrimaryStiffeners.FlangeThickness = Val_(v(4))
                Me.cScantlings.cPrimaryStiffeners.CorrosionThickness = Val_(v(5))
                Me.cScantlings.cSecondaryFrames.Side = SideLeft
                Me.cScantlings.cSecondaryStiffeners.Side = SideLeft
                If Me.cScantlings.cSecondaryFrames.Spacing > 0 Or Me.cScantlings.cSecondaryStiffeners.Spacing > 0 Then
                    sLine = ReadLn(ts)
                    GetValues 5, sLine, v
                    Me.cScantlings.cSecondaryFrames.WebHeight = Val_(v(1))
                    Me.cScantlings.cSecondaryFrames.WebThickness = Val_(v(2))
                    Me.cScantlings.cSecondaryFrames.FlangeWidth = Val_(v(3))
                    Me.cScantlings.cSecondaryFrames.FlangeThickness = Val_(v(4))
                    Me.cScantlings.cSecondaryFrames.Side = Val_(v(5))
                    sLine = ReadLn(ts)
                    GetValues 5, sLine, v
                    Me.cScantlings.cSecondaryStiffeners.WebHeight = Val_(v(1))
                    Me.cScantlings.cSecondaryStiffeners.WebThickness = Val_(v(2))
                    Me.cScantlings.cSecondaryStiffeners.FlangeWidth = Val_(v(3))
                    Me.cScantlings.cSecondaryStiffeners.FlangeThickness = Val_(v(4))
                    Me.cScantlings.cSecondaryStiffeners.Side = Val_(v(5))
                End If
                sLine = ReadLn(ts)
                GetValues 1, sLine, v
                Me.cScantlings.cPrimaryStiffeners.DistributionMode = v(1)
                If Me.pType = DoubleHull Then
                    sLine = ReadLn(ts)
                    GetValues 1, sLine, v
                    Me.RelatedDoubleHullPanel = Val_(v(1))
                End If
                sLine = ReadLn(ts)
                GetValues 1, sLine, v
                Me.cGeometry.Participation = Val_(v(1))
            Case Beam
                sLine = ReadLn(ts)
                GetValues 1, sLine, v
                Select Case UCase(v(1))
                    Case "CERCLE"
                        Me.cScantlings.BeamSection = bsCircle
                    Case "CARRE"
                        Me.cScantlings.BeamSection = bsSquare
                    Case "DOUBLET"
                        Me.cScantlings.BeamSection = bsDoubleT
                    Case Else
                    Err.Description = "BEAM Section " & "'" & v(1) & "'" & " not recognized."
                    Err.Raise 9999, , Err.Description
                End Select
                sLine = ReadLn(ts)
                GetValues 2, sLine, v
                Me.cScantlings.cPrimaryFrames.Spacing = Val_(v(1))
                Me.cGeometry.BucklingLength = Val_(v(2))
                Select Case Me.cScantlings.BeamSection
                    Case bsCircle
                        sLine = ReadLn(ts)
                        GetValues 2, sLine, v
                        Me.cScantlings.cPrimaryFrames.WebHeight = Val_(v(1))
                        Me.cScantlings.cPrimaryFrames.CorrosionThickness = Val_(v(2))
                    Case bsSquare
                        sLine = ReadLn(ts)
                        GetValues 2, sLine, v
                        Me.cScantlings.cPrimaryFrames.WebHeight = Val_(v(1))
                        Me.cScantlings.cPrimaryFrames.CorrosionThickness = Val_(v(2))
                    Case bsDoubleT
                        sLine = ReadLn(ts)
                        GetValues 5, sLine, v
                        Me.cScantlings.cPrimaryFrames.WebHeight = Val_(v(1)) * 2 ' to modify when Judith modifies in LBR5
                        Me.cScantlings.cPrimaryFrames.WebThickness = Val_(v(2))  ' see ReadLBR5txtFile as well
                        Me.cScantlings.cPrimaryFrames.FlangeWidth = Val_(v(3))
                        Me.cScantlings.cPrimaryFrames.FlangeThickness = Val_(v(4))
                        Me.cScantlings.cPrimaryFrames.CorrosionThickness = Val_(v(5))
                        Me.cScantlings.NetThickness = Empty
                End Select
        End Select
        Me.cMaterial.ReadLBR5txtFile ts
        sLine = ReadLn(ts)
        GetValues 7, sLine, v
        Dim NoOfGirders As Integer
        NoOfGirders = Val_(v(1))
        Me.cScantlings.cPrimaryStiffeners.Side = Val(v(2))
        Me.cScantlings.cPrimaryFrames.Side = Val_(v(3))
        Me.cScantlings.GirderSide = Val_(v(4))
        If Val_(v(5)) = 0 Then v(5) = 1
        Me.LateralPressureSide = Val_(v(5))
        Me.IsBuoyancyForce = Val_(v(6))
        Me.UniformLateralPressureVariation = Val_(v(7))
        sLine = ReadLn(ts)
        GetValues 1, sLine, v
        Me.cGeometry.PanelAngle = Val_(v(1))
        For i = 1 To Project.Item(lProjectCount).cHeader.colLoadCase.Count
            Set OBJ = New cLoadCase
            Me.colLoadCase.Add OBJ, i
            Set OBJ = Nothing
        Next i
        For i = 1 To Me.colLoadCase.Count
            sLine = ReadLn(ts)
            GetValues 2, sLine, v
            Me.colLoadCase.Item(i).index = i
            Me.colLoadCase.Item(i).Title = Project.Item(lProjectCount).cHeader.colLoadCase.Item(i).Title
            Me.colLoadCase.Item(i).LateralPressureIn = Val_(v(1))
            Me.colLoadCase.Item(i).LateralPressureOut = Val_(v(2))
        Next i
        sLine = ReadLn(ts)
        GetValues 1, sLine, v
        Me.LocalizedPressure = Val_(v(1))
        sLine = ReadLn(ts)
        GetValues 1, sLine, v
        Me.IsWiseLateralPressure = Val_(v(1))
        If Me.IsWiseLateralPressure = yes Then
            For i = 1 To Me.colLoadCase.Count
                ts.SkipLine
                sLine = ReadLn(ts)
                GetValues 1, sLine, v
                Dim NoOfSteps As Integer
                NoOfSteps = Val_(v(1))
                For j = 1 To NoOfSteps
                    Set OBJ = New cStepWiseLateralPressure
                    Me.colLoadCase.Item(i).colStepWiseLateralPressure.Add OBJ, j
                    Me.colLoadCase.Item(i).colStepWiseLateralPressure.Item(j).index = j
                    Me.colLoadCase.Item(i).colStepWiseLateralPressure.Item(j).ReadLBR5txtFile ts
                    Set OBJ = Nothing
                Next j
            Next i
        End If
        'Panel Connections
        sLine = ReadLn(ts)
        GetValues 10, sLine, v
        Set col = New Collection
        For i = 1 To 10
            'Me.AddConnection Val_(v(i)), i
            col.Add Val_(v(i))
        Next i
        Set colConnections = col
        Set col = Nothing
        For i = 1 To NoOfGirders
            Set OBJ = New cGirder
            OBJ.index = i
            Me.cScantlings.colGirder.Add OBJ, i
            Me.cScantlings.colGirder.Item(i).ReadLBR5txtFile ts
            Set OBJ = Nothing
        Next i
        'Design Variables
        ts.SkipLine
        sLine = ReadLn(ts)
        GetValues 1, sLine, v
        Dim NoOfDesignVariables As Integer
        NoOfDesignVariables = Val_(v(1))
        For i = 1 To NoOfDesignVariables
            Set OBJ = New cDesignVariables
            OBJ.index = i
            Me.colDesignVariables.Add OBJ, i
            Set OBJ = Nothing
        Next i
        If NoOfDesignVariables > 0 Then
            Dim DesVarList() As Double
            Dim DesVarMin() As Double
            Dim DesVarMax() As Double
            ReDim DesVarList(NoOfDesignVariables - 1)
            ReDim DesVarMin(NoOfDesignVariables - 1)
            ReDim DesVarMax(NoOfDesignVariables - 1)
            sLine = ReadLn(ts)
            GetValues NoOfDesignVariables, sLine, v
            For i = 1 To NoOfDesignVariables
                Me.colDesignVariables.Item(i).VariableName = Val_(v(i))
            Next i
            sLine = ReadLn(ts)
            GetValues NoOfDesignVariables, sLine, v
            For i = 1 To NoOfDesignVariables
                Me.colDesignVariables.Item(i).LowerLimit = Val_(v(i))
            Next i
            sLine = ReadLn(ts)
            GetValues NoOfDesignVariables, sLine, v
            For i = 1 To NoOfDesignVariables
                Me.colDesignVariables.Item(i).UpperLimit = Val_(v(i))
            Next i
        End If
        'Structural Constraints
        ts.SkipLine
        sLine = ReadLn(ts)
        GetValues 1, sLine, v
        Me.IsStructuralConstraints = Val_(v(1))
        If Me.IsStructuralConstraints = yes Then
            sLine = ReadLn(ts)
            Dim NoOfPoints As Integer
            GetValues 1, sLine, v
            NoOfPoints = Val_(v(1))
            sLine = ReadLn(ts)
            GetValues NoOfPoints, sLine, v
            Dim r As Double
            For i = 1 To NoOfPoints
'                r = Val_(v(i))
'                Me.AddAssessmentPoint r, i
                Dim assp As New cAssessmentPoint
                assp.index = i
                assp.Value = Val_(v(i))
                Me.colAssessmentPoints.Add assp, i
                Set assp = Nothing
            Next i
            For i = 1 To Me.colLoadCase.Count
                ts.SkipLine
                sLine = ReadLn(ts)
                GetValues 1, sLine, v
                Dim NoOfStrRestr As Integer
                NoOfStrRestr = Val_(v(1))
                If NoOfStrRestr = -1 Then
                    For j = 1 To Me.colLoadCase.Item(1).colStructuralConstraints.Count
                        Set OBJ = New cStructuralConstraints
                        OBJ.index = Me.colLoadCase.Item(1).colStructuralConstraints.Item(j).index
                        OBJ.Reference = Me.colLoadCase.Item(1).colStructuralConstraints.Item(j).Reference
                        OBJ.Value = Me.colLoadCase.Item(1).colStructuralConstraints.Item(j).Value
                        OBJ.Limit = Me.colLoadCase.Item(1).colStructuralConstraints.Item(j).Limit
                        OBJ.AssesmentPoint = Me.colLoadCase.Item(1).colStructuralConstraints.Item(j).AssesmentPoint
                        Me.colLoadCase.Item(i).colStructuralConstraints.Add OBJ, j
                        Set OBJ = Nothing
                    Next j
                    GoTo NextLoadCase
                End If
                For j = 1 To NoOfStrRestr
                    Set OBJ = New cStructuralConstraints
                    Me.colLoadCase.Item(i).colStructuralConstraints.Add OBJ, j
                    Me.colLoadCase.Item(i).colStructuralConstraints.Item(j).ReadLBR5txtFile ts
                    Set OBJ = Nothing
                Next j
NextLoadCase:
            Next i
        End If
        'Geometrical Constraints
        ts.SkipLine
        sLine = ReadLn(ts)
        GetValues 1, sLine, v
        Me.NoOfGeomConstr = Val_(v(1))
        Select Case NoOfGeomConstr
            Case 0
            Case 99
                Set OBJ = New cGeometricalConstraints
                sLine = ReadLn(ts)
                GetValues 1, sLine, v
                OBJ.index = 1
                OBJ.code = Val_(v(1))
                Me.colGeometricalConstraints.Add OBJ, 1
                sLine = ReadLn(ts)
                GetValues 2, sLine, v
                Me.FramesFlangeThicknessUpdate = Val_(v(1))
                Me.StiffenersFlangeThicknessUpdate = Val_(v(2))
                Set OBJ = Nothing
            Case Else
                sLine = ReadLn(ts)
                GetValues NoOfGeomConstr, sLine, v
                For i = 1 To NoOfGeomConstr
                    Set OBJ = New cGeometricalConstraints
                    OBJ.index = i
                    OBJ.code = Val_(v(i))
                    Me.colGeometricalConstraints.Add OBJ, i
                    Set OBJ = Nothing
                Next i
                sLine = ReadLn(ts)
                GetValues 2, sLine, v
                Me.FramesFlangeThicknessUpdate = Val_(v(1))
                Me.StiffenersFlangeThicknessUpdate = Val_(v(2))
        End Select
        'Me.pNumber = Me.pNumber
    Exit Function
ReadLBR5txtFileErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: Function ReadLBR5txtFile; " & Err.Description)
End Function

Public Function ReadMARS_LBR5TransfertFile(ts As TextStream)
    'On Error GoTo  ReadMARS_LBR5TransfertFileErr
    Dim sLine As String
    Dim i As Integer
    Dim Y0 As Double, y1 As Double, Z0 As Double, z1 As Double
    Dim v() As Variant
NextLine0:
    sLine = ReadLn(ts)
    Select Case Left(UCase(sLine), 10)
        Case "<STIPANEL>"
        GetValues 2, sLine, v
        Me.pLabel = v(2)
        Case Else
            GoTo NextLine0
    End Select
    sLine = ReadLn(ts) 'In Node, Out Node
    GetValues 2, sLine, v
    Me.cGeometry.InNode = Val_(v(1))
    Me.cGeometry.OutNode = Val_(v(2))
    Y0 = Project.Item(lProjectCount).colNodes.Item(Me.cGeometry.InNode).y
    Z0 = Project.Item(lProjectCount).colNodes.Item(Me.cGeometry.InNode).z
    y1 = Project.Item(lProjectCount).colNodes.Item(Me.cGeometry.OutNode).y
    z1 = Project.Item(lProjectCount).colNodes.Item(Me.cGeometry.OutNode).z
    GetLengthAngle Y0, Z0, y1, z1, Me
    'Compartments on left and right side
    sLine = ReadLn(ts)
    GetValues 2, sLine, v
    Me.LeftCompartment = Val_(v(1))
    Me.RightCompartment = Val_(v(2))
    'Strakes
NextLine1:
    sLine = ReadLn(ts)
    GetValues 1, sLine, v
    Dim NoOfStrakes As Integer
    NoOfStrakes = Val_(v(1))
    For i = 1 To NoOfStrakes
        Dim cStrake As New cStrake
        Me.colStrakes.Add cStrake, i
        Me.colStrakes.Item(i).ReadMARS_LBR5TransfertFile ts
        Set cStrake = Nothing
    Next i
    Me.cScantlings.GrossThickness = Round(Me.colStrakes.GetEquivalentGrossThickness, 5)
    Me.cScantlings.NetThickness = Round(Me.colStrakes.GetEquivalentNetThickness, 5)
    Me.cScantlings.CorrosionThickness = Round(Me.cScantlings.GrossThickness - Me.cScantlings.NetThickness, 5)
    'Materials (I take only the material of the first strake from the panel)
    With Project.Item(lProjectCount).colMaterials.Item(Me.colStrakes.Item(1).MaterialNumber)
        Me.cMaterial.mNumber = .mNumber
        Me.cMaterial.index = .index
        Me.cMaterial.Name = .Name
        Me.cMaterial.YieldStress = .YieldStress
        Me.cMaterial.AllowableStress = .AllowableStress
        Me.cMaterial.YoungModulus = .YoungModulus
        Me.cMaterial.Poisson = .Poisson
        Select Case .Name
            Case "Steel", "Stainless steel"
                Me.cMaterial.SpecificWeight = 77000
            Case "Aluminium rolled", "Aluminium extruded"
                Me.cMaterial.SpecificWeight = 27200
        End Select
        'Me.cMaterial.SpecificWeight = 77000 ' par default
    End With
    'Primary Transversals Spacing
    sLine = ReadLn(ts)
    GetValues 1, sLine, v
    Me.cScantlings.cPrimaryFrames.Spacing = Val_(v(1))
    'Secondary Transversals
    sLine = ReadLn(ts)
    GetValues 1, sLine, v
    Dim NoOfSecondaryTransversals As Integer
    NoOfSecondaryTransversals = Val_(v(1))
    For i = 1 To NoOfSecondaryTransversals
        Dim cSecondaryFrames As New cSecondaryFrames
        Me.colSecondaryFrames.Add cSecondaryFrames, i
        Me.colSecondaryFrames.Item(i).ReadMARS_LBR5TransfertFile ts
        Set cSecondaryFrames = Nothing
    Next i
    Dim SP As Double, HW As Double, Tw As Double, Bf As Double, Tf As Double, corr As Double
    If Me.colSecondaryFrames.Count > 0 Then
        If Me.colSecondaryFrames.CheckSides = True Then
            MsgBox "Panel " & Me.pNumber & " has transversal secondary stiffeners defined on both sides." & vbCrLf & _
                    "The equivalent transversal secondary member must be checked.", vbExclamation + vbOKOnly
        End If
        Me.colSecondaryFrames.GetEquivalentFrames SP, HW, Tw, Bf, Tf
        Me.cScantlings.cSecondaryFrames.Spacing = SP
        Me.cScantlings.cSecondaryFrames.WebHeight = HW / 1000
        Me.cScantlings.cSecondaryFrames.WebThickness = Tw / 1000
        Me.cScantlings.cSecondaryFrames.FlangeWidth = Bf / 1000
        Me.cScantlings.cSecondaryFrames.FlangeThickness = Tf / 1000
        Me.cScantlings.cSecondaryFrames.Side = Me.colSecondaryFrames.Item(1).Side
    End If
    'Primary Stiffeners
    sLine = ReadLn(ts)
    GetValues 1, sLine, v
    Dim NoOfPrimaryStiffeners As Integer
    NoOfPrimaryStiffeners = Val_(v(1))
    For i = 1 To NoOfPrimaryStiffeners
        Dim cPrimaryStiffeners As New cPrimaryStiffeners
        Me.colPrimaryStiffeners.Add cPrimaryStiffeners, i
        Me.colPrimaryStiffeners.Item(i).ReadMARS_LBR5TransfertFile ts
        Set cPrimaryStiffeners = Nothing
    Next i
    Me.cScantlings.cPrimaryStiffeners.DistributionMode = "EE2"
    Me.cScantlings.cPrimaryStiffeners.Spacing = Me.cGeometry.PanelWidth
    If Me.colPrimaryStiffeners.Count > 0 Then
        Me.colPrimaryStiffeners.GetEquivalentStiffeners Me, Me.cScantlings.colGirder, SP, HW, Tw, Bf, Tf, corr
        Me.cScantlings.cPrimaryStiffeners.Spacing = Round(SP, 3)
        'Me.cScantlings.cPrimaryStiffeners.DistributionMode = "EE2"
        Me.cScantlings.cPrimaryStiffeners.WebHeight = Round(HW, 3)
        Me.cScantlings.cPrimaryStiffeners.WebThickness = Round(Tw, 3)
        Me.cScantlings.cPrimaryStiffeners.FlangeWidth = Round(Bf, 3)
        Me.cScantlings.cPrimaryStiffeners.FlangeThickness = Round(Tf, 3)
        Me.cScantlings.cPrimaryStiffeners.CorrosionThickness = Round(corr, 3)
    End If
    If Me.colPrimaryStiffeners.Count > 0 Then
        Me.cScantlings.cPrimaryStiffeners.Side = Me.colPrimaryStiffeners.Item(1).Side
    End If
    If Me.colPrimaryStiffeners.CheckSides = True Then
        MsgBox "Panel " & Me.pNumber & " has longitudinal stiffeners defined on both sides." & vbCrLf & _
                "The equivalent longitudinal stiffeners must be checked.", vbExclamation + vbOKOnly
    End If
    ' Lateral pressure side, type
    Me.LateralPressureSide = SideRight
    Me.UniformLateralPressureVariation = IsExplicit
    'Panel efficiency
    sLine = ReadLn(ts)
    GetValues 1, sLine, v
    Me.cGeometry.Participation = Val_(v(1))
    Exit Function
ReadMARS_LBR5TransfertFileErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: Function ReadMARS_LBR5TransfertFile")
End Function

Public Function WriteLBR5txtFile(ts As TextStream)
    On Error GoTo WriteLBR5txtFileErr
    Dim sComment As String
    Select Case Me.pType
        Case Plate
            ts.WriteLine "PLAQUE" & "     " & "PANNEAU NO " & Me.pNumber & "          " & "======================================================================================"
        Case Beam
            ts.WriteLine "EPONTILLE" & "     " & "PANNEAU NO " & Me.pNumber & "          " & "======================================================================================"
        Case DoubleHull
            ts.WriteLine "DCOQUE" & "     " & "PANNEAU NO " & Me.pNumber & "          " & "======================================================================================"
    End Select
    sComment = "Hauteur. Epaisseur. (m)"
    ts.WriteLine PVal(-Me.cGeometry.PanelWidth, "0.000000") & "  " & PVal(Me.cScantlings.NetThickness, "0.00000") & getTabs(7) & sComment
    Select Case Me.pType
        Case Plate, DoubleHull
            sComment = "Entr. cadre. et raid. idem raid. compl.; ép. corrosion tôle"
            ts.WriteLine PVal(Me.cScantlings.cPrimaryFrames.Spacing, "0.00000") & "  " & _
                        PVal(Me.cScantlings.cPrimaryStiffeners.Spacing, "0.00000") & "  " & _
                        PVal(Me.cScantlings.cSecondaryFrames.Spacing, "0.00000") & "  " & _
                        PVal(Me.cScantlings.cSecondaryStiffeners.Spacing, "0.00000") & "  " & _
                        PVal(Me.cScantlings.CorrosionThickness, "0.00000") & _
                        getTabs(5) & sComment
            sComment = "Cadres Transversaux (m). Hauteur âme. Epaisseur âme. Largeur semelle. Epaisseur semelle; ép. corrosion cadre"
            ts.WriteLine PVal(Me.cScantlings.cPrimaryFrames.WebHeight, "0.00000") & "  " & _
                        PVal(Me.cScantlings.cPrimaryFrames.WebThickness, "0.00000") & "  " & _
                        PVal(Me.cScantlings.cPrimaryFrames.FlangeWidth, "0.00000") & "  " & _
                        PVal(Me.cScantlings.cPrimaryFrames.FlangeThickness, "0.00000") & "  " & _
                        PVal(Me.cScantlings.cPrimaryFrames.CorrosionThickness, "0.00000") & _
                        getTabs(5) & sComment
            sComment = "Raidisseurs  Long.  (m). Hauteur âme. Epaisseur âme. Largeur semelle. Epaisseur semelle; ép. corrosion raidisseur"
            ts.WriteLine PVal(Me.cScantlings.cPrimaryStiffeners.WebHeight, "0.00000") & "  " & _
                        PVal(Me.cScantlings.cPrimaryStiffeners.WebThickness, "0.00000") & "  " & _
                        PVal(Me.cScantlings.cPrimaryStiffeners.FlangeWidth, "0.00000") & "  " & _
                        PVal(Me.cScantlings.cPrimaryStiffeners.FlangeThickness, "0.00000") & "  " & _
                        PVal(Me.cScantlings.cPrimaryStiffeners.CorrosionThickness, "0.00000") & _
                        getTabs(5) & sComment

            If Me.cScantlings.cSecondaryFrames.Spacing <> 0 Or _
                Me.cScantlings.cSecondaryStiffeners.Spacing <> 0 Then
            sComment = "Cadres Transversaux secondaires(m). Hauteur âme. Epaisseur âme. Largeur semelle. Epaisseur semelle. Side"
            ts.WriteLine PVal(Me.cScantlings.cSecondaryFrames.WebHeight, "0.00000") & "  " & _
                        PVal(Me.cScantlings.cSecondaryFrames.WebThickness, "0.00000") & "  " & _
                        PVal(Me.cScantlings.cSecondaryFrames.FlangeWidth, "0.00000") & "  " & _
                        PVal(Me.cScantlings.cSecondaryFrames.FlangeThickness, "0.00000") & "  " & _
                        Me.cScantlings.cSecondaryFrames.Side & _
                        getTabs(4) & sComment
            sComment = "Raidisseurs  Long. secondaires (m). Hauteur âme. Epaisseur âme. Largeur semelle. Epaisseur semelle. Side"
            ts.WriteLine PVal(Me.cScantlings.cSecondaryStiffeners.WebHeight, "0.00000") & "  " & _
                        PVal(Me.cScantlings.cSecondaryStiffeners.WebThickness, "0.00000") & "  " & _
                        PVal(Me.cScantlings.cSecondaryStiffeners.FlangeWidth, "0.00000") & "  " & _
                        PVal(Me.cScantlings.cSecondaryStiffeners.FlangeThickness, "0.00000") & "  " & _
                        Me.cScantlings.cSecondaryStiffeners.Side & _
                        getTabs(4) & sComment
                
            End If
            sComment = "Repartition raidisseurs longitudinaux"
            ts.WriteLine Me.cScantlings.cPrimaryStiffeners.DistributionMode & getTabs(8) & sComment
            If Me.pType = DoubleHull Then
                sComment = "Panneau associé"
                ts.WriteLine Me.RelatedDoubleHullPanel & getTabs(8) & sComment
            End If
            sComment = "Coefficient de participation. Code de position. Famille. Lot"
            ts.WriteLine PVal(Me.cGeometry.Participation, "0.00") & " " & Me.cGeometry.PositionCode & "  " & Me.cGeometry.FAMI & "  " & Me.cGeometry.LOT & getTabs(7) & sComment
        Case Beam
            Select Case Me.cScantlings.BeamSection
                Case bsCircle
                    ts.WriteLine "CERCLE"
                Case bsSquare
                    ts.WriteLine "CARRE"
                Case bsDoubleT
                    ts.WriteLine "DOUBLET"
            End Select
            sComment = "Ecartement. Longueur effective"
            ts.WriteLine PVal(Me.cScantlings.cPrimaryFrames.Spacing, "0.00000") & " " & _
                        PVal(Me.cGeometry.BucklingLength, "0.00000") & getTabs(7) & sComment
            Select Case Me.cScantlings.BeamSection
                Case bsCircle
                    sComment = "diamètre extérieur; ép. corrosion"
                    ts.WriteLine PVal(Me.cScantlings.cPrimaryFrames.WebHeight, "0.00000") & "  " & _
                    PVal(Me.cScantlings.cPrimaryFrames.CorrosionThickness, "0.00000") & _
                    getTabs(8) & sComment
                Case bsSquare
                    sComment = "Largeur extérieure; ép. corrosion"
                    ts.WriteLine PVal(Me.cScantlings.cPrimaryFrames.WebHeight, "0.00000") & "  " & _
                    PVal(Me.cScantlings.cPrimaryFrames.CorrosionThickness, "0.00000") & _
                    getTabs(8) & sComment
                Case bsDoubleT
                    sComment = "Hauteur âme. Epaisseur âme. Largeur semelle. Epaisseur semelle; ép. corrosion"
                    ts.WriteLine PVal(Me.cScantlings.cPrimaryFrames.WebHeight / 2, "0.00000") & "  " & _
                        PVal(Me.cScantlings.cPrimaryFrames.WebThickness, "0.00000") & "  " & _
                        PVal(Me.cScantlings.cPrimaryFrames.FlangeWidth, "0.00000") & "  " & _
                        PVal(Me.cScantlings.cPrimaryFrames.FlangeThickness, "0.00000") & "  " & _
                        PVal(Me.cScantlings.cPrimaryFrames.CorrosionThickness, "0.00000") & _
                        getTabs(5) & sComment    ' to modify webheight/2 --> webheight when Judith modifies in LBR5
            End Select                           ' see ReadLBR5txtFile as well
    End Select
    Me.cMaterial.WriteLBR5txtFile ts
    sComment = "MT. KSR. KSA. KST. KSE. IPA. IVARIA"
    ts.WriteLine Me.cScantlings.colGirder.Count & "  " & _
                Me.cScantlings.cPrimaryStiffeners.Side & "  " & _
                Me.cScantlings.cPrimaryFrames.Side & "  " & _
                Me.cScantlings.GirderSide & "  " & _
                Me.LateralPressureSide & "  " & _
                Me.IsBuoyancyForce & "  " & _
                Me.UniformLateralPressureVariation & getTabs(6) & sComment
    sComment = "Angle"
    ts.WriteLine PVal(Me.cGeometry.PanelAngle, "0.000000") & getTabs(8) & sComment
    
    Dim LoadCase As cLoadCase
    For Each LoadCase In Me.colLoadCase
        sComment = LoadCase.Title
        ts.WriteLine PVal(LoadCase.LateralPressureIn, "0.00000") & "  " & _
        PVal(LoadCase.LateralPressureOut, "0.00000") & _
                    getTabs(7) & sComment
    Next LoadCase
    
    sComment = "Pression locale"
    ts.WriteLine PVal(Me.LocalizedPressure, "0.00000") & getTabs(8) & sComment
    
    sComment = "Charge variable selon X: 0 = non. 1 = oui"
    GetIsWiseLateralPressure
    ts.WriteLine Me.IsWiseLateralPressure & getTabs(8) & sComment
    If Me.IsWiseLateralPressure = yes Then
        Dim StepwisePressure As cStepWiseLateralPressure
        For Each LoadCase In Me.colLoadCase
            ts.WriteLine LoadCase.Title
            sComment = "Nombre de pas selon OX"
            ts.WriteLine LoadCase.colStepWiseLateralPressure.Count & getTabs(8) & sComment
            sComment = "CHA1. CHA2. CHA3"
            For Each StepwisePressure In LoadCase.colStepWiseLateralPressure
                ts.WriteLine PVal(StepwisePressure.VerticalGravityLoad, "0.00000") & "  " & _
                            PVal(StepwisePressure.LateralPressureIn, "0.00000") & "  " & _
                            PVal(StepwisePressure.LateralPressureOut, "0.00000") & getTabs(6) & sComment
            Next StepwisePressure
        Next LoadCase
    End If
    Dim sConnectionsList As String
    sConnectionsList = ""
    Dim i As Integer
    For i = 1 To 10
        sConnectionsList = sConnectionsList & "  " & Me.colConnections.Item(i)
    Next i
    sConnectionsList = LTrim(sConnectionsList)
    sComment = "Liste des 10 panneaux qui suivent"
    ts.WriteLine sConnectionsList & getTabs(5) & sComment
    Dim Girder As cGirder
    For Each Girder In Me.cScantlings.colGirder
        sComment = "Raidisseurs longitudinaux (m)"
        ts.WriteLine PVal(Girder.WebHeight, "0.00000") & "  " & _
                    PVal(Girder.WebThickness, "0.00000") & "  " & _
                    PVal(Girder.FlangeWidth, "0.00000") & "  " & _
                    PVal(Girder.FlangeThickness, "0.00000") & "  " & _
                    PVal(Girder.Distance, "0.00000") & getTabs(4) & sComment
    Next Girder
    
    'test desvar for beams - active permited only for beam spacing
    If Me.pType = Beam Then
        For i = 1 To 9
            If i <> 5 Then
                Me.colDesignVariables.Item(i).Active = False
                'Me.colDesignVariables.ActiveVariablesCount
            End If
        Next i
    End If

    ts.WriteLine "DESIGN VARIABLES"
    Select Case Me.colDesignVariables.ActiveVariablesCount
        Case 0
            ts.WriteLine "0"
            GoTo write_des_var
        Case Is > 0
            Dim DesignVariables As cDesignVariables
            Dim sDesignVariablesList As String
            sDesignVariablesList = ""
            
            ts.WriteLine Me.colDesignVariables.ActiveVariablesCount
            For Each DesignVariables In Me.colDesignVariables
                If DesignVariables.Active = True Then
                    sDesignVariablesList = sDesignVariablesList & "  " & DesignVariables.VariableName
                    sDesignVariablesList = LTrim(sDesignVariablesList)
                End If
            Next DesignVariables
            ts.WriteLine sDesignVariablesList
write_des_var:
            Dim sDesignMin As String
            sDesignMin = ""
            For Each DesignVariables In Me.colDesignVariables
                sDesignMin = sDesignMin & "  " & PVal(DesignVariables.LowerLimit, "0.00000")
                sDesignMin = LTrim(sDesignMin)
            Next DesignVariables
            ts.WriteLine sDesignMin
            Dim sDesignMax As String
            sDesignMax = ""
            For Each DesignVariables In Me.colDesignVariables
                sDesignMax = sDesignMax & "  " & PVal(DesignVariables.UpperLimit, "0.00000")
                sDesignMax = LTrim(sDesignMax)
            Next DesignVariables
            ts.WriteLine sDesignMax
    End Select
    
    ts.WriteLine "STRUCTURAL CONSTRAINTS"
    GetIsStructuralConstraints
    sComment = "(0 without structural constraints. 1 with structural constraints)"
    Select Case Me.IsStructuralConstraints
        Case no
            ts.WriteLine "0" & getTabs(8) & sComment
        Case yes
            ts.WriteLine "1" & getTabs(8) & sComment
            sComment = "Nbre de pts pour le calcul des sensibilites"
            ts.WriteLine Me.colAssessmentPoints.Count & getTabs(8) & sComment
            Dim sAssessmentPointList As String
            sAssessmentPointList = ""
            For i = 1 To Me.colAssessmentPoints.Count
                sAssessmentPointList = sAssessmentPointList & "  " & PVal(Me.colAssessmentPoints.Item(i).Value, "0.00000")
                sAssessmentPointList = LTrim(sAssessmentPointList)
            Next i
            sComment = "Liste de pts pour le calcul des sensibilites"
            ts.WriteLine sAssessmentPointList & getTabs(6) & sComment
            Dim StructuralConstraint As cStructuralConstraints
            For Each LoadCase In Me.colLoadCase
                ts.WriteLine LoadCase.Title
                sComment = "Nbre de restrictions structurelles"
                ts.WriteLine LoadCase.colStructuralConstraints.Count & getTabs(8) & sComment
                sComment = "(#no  Constraint Type     CJmax   INV   IPT)"
                For Each StructuralConstraint In LoadCase.colStructuralConstraints
                    ts.WriteLine StructuralConstraint.index & "  " & _
                                StructuralConstraint.Reference & "  " & _
                                PVal(StructuralConstraint.Value, "000.00E+00") & "  " & _
                                StructuralConstraint.Limit & "  " & _
                                StructuralConstraint.AssesmentPoint & _
                                getTabs(6) & sComment
                Next StructuralConstraint
            Next LoadCase
    End Select
    ts.WriteLine "GEOMETRICAL CONSTRAINTS"
    sComment = "M2=Nbre de restrictions geometriques"
    Select Case Me.NoOfGeomConstr
        Case 0
            ts.WriteLine "0" & getTabs(8) & sComment
        Case 99
            ts.WriteLine "99" & getTabs(8) & sComment
            ts.WriteLine Me.colGeometricalConstraints.Item(1).code
            sComment = "ISEMA. ISEMR"
            ts.WriteLine Me.FramesFlangeThicknessUpdate & "  " & _
                        Me.StiffenersFlangeThicknessUpdate & getTabs(8) & sComment
        Case Is > 0
            ts.WriteLine Me.NoOfGeomConstr & getTabs(8) & sComment
            Dim GeometricalConstraint As cGeometricalConstraints
            Dim sGeometricalConstraintList As String
            sGeometricalConstraintList = ""
            For Each GeometricalConstraint In Me.colGeometricalConstraints
                sGeometricalConstraintList = sGeometricalConstraintList & "  " & GeometricalConstraint.code
            Next GeometricalConstraint
            sGeometricalConstraintList = LTrim(sGeometricalConstraintList)
            ts.WriteLine sGeometricalConstraintList
            sComment = "ISEMA. ISEMR"
            ts.WriteLine Me.FramesFlangeThicknessUpdate & "  " & _
                        Me.StiffenersFlangeThicknessUpdate & getTabs(8) & sComment
    End Select
    Exit Function
WriteLBR5txtFileErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: Function WriteLBR5txtFile; " & Err.Description)
End Function

Public Function Reverse()
    On Error GoTo ReverseErr
    'Nodes
    Dim iNodeIn As Integer, iNodeOut As Integer
    iNodeIn = Me.cGeometry.OutNode
    iNodeOut = Me.cGeometry.InNode
    Me.cGeometry.InNode = iNodeIn
    Me.cGeometry.OutNode = iNodeOut
    'Angle
    GetLengthAngle Project.Item(ActiveProject).colNodes.Item(iNodeIn).y, _
                    Project.Item(ActiveProject).colNodes.Item(iNodeIn).z, _
                    Project.Item(ActiveProject).colNodes.Item(iNodeOut).y, _
                    Project.Item(ActiveProject).colNodes.Item(iNodeOut).z, Me
    'Pressures
    Select Case Me.LateralPressureSide
        Case SideLeft
            Me.LateralPressureSide = SideRight
        Case SideRight
            Me.LateralPressureSide = SideLeft
    End Select
    Dim dPressIn As Double, dPressOut As Double, cLoadCase As cLoadCase
    For Each cLoadCase In Me.colLoadCase
        dPressIn = cLoadCase.LateralPressureOut
        dPressOut = cLoadCase.LateralPressureIn
        cLoadCase.LateralPressureIn = dPressIn
        cLoadCase.LateralPressureOut = dPressOut
    Next cLoadCase
    'Mars Pressures
    Dim MarsLC As cMarsLoadCase
    Dim MarsLT As cMarsLoadType
    For Each MarsLC In Me.colMarsLoadCase
        For Each MarsLT In MarsLC.colMarsLoadType
            'Left Compartment
            dPressIn = MarsLT.LeftCompPress.Out_Node
            dPressOut = MarsLT.LeftCompPress.In_Node
            MarsLT.LeftCompPress.In_Node = -dPressIn
            MarsLT.LeftCompPress.Out_Node = -dPressOut
            'Right Compartment
            dPressIn = MarsLT.RightCompPress.Out_Node
            dPressOut = MarsLT.RightCompPress.In_Node
            MarsLT.RightCompPress.In_Node = -dPressIn
            MarsLT.RightCompPress.Out_Node = -dPressOut
        Next MarsLT
    Next MarsLC
    'Primary Stiffeners
    Select Case Me.cScantlings.cPrimaryStiffeners.Side
        Case SideLeft
            Me.cScantlings.cPrimaryStiffeners.Side = SideRight
        Case SideRight
            Me.cScantlings.cPrimaryStiffeners.Side = SideLeft
    End Select
    'Secondary Stiffeners
    Select Case Me.cScantlings.cSecondaryStiffeners.Side
        Case SideLeft
            Me.cScantlings.cSecondaryStiffeners.Side = SideRight
        Case SideRight
            Me.cScantlings.cSecondaryStiffeners.Side = SideLeft
    End Select
    'Primary Frames
    Select Case Me.cScantlings.cPrimaryFrames.Side
        Case SideLeft
            Me.cScantlings.cPrimaryFrames.Side = SideRight
        Case SideRight
            Me.cScantlings.cPrimaryFrames.Side = SideLeft
    End Select
    'Secondary Frames
    Select Case Me.cScantlings.cSecondaryFrames.Side
        Case SideLeft
            Me.cScantlings.cSecondaryFrames.Side = SideRight
        Case SideRight
            Me.cScantlings.cSecondaryFrames.Side = SideLeft
    End Select
    'Girders
    Select Case Me.cScantlings.GirderSide
        Case SideLeft
            Me.cScantlings.GirderSide = SideRight
        Case SideRight
            Me.cScantlings.GirderSide = SideLeft
    End Select
    Dim cGirder As cGirder
    For Each cGirder In Me.cScantlings.colGirder
        cGirder.Distance = Me.cGeometry.PanelWidth - cGirder.Distance
    Next cGirder
    'Compartments
    Dim iCompLeft As Integer, iCompRight As Integer
    iCompLeft = Me.RightCompartment
    iCompRight = Me.LeftCompartment
    Me.LeftCompartment = iCompLeft
    Me.RightCompartment = iCompRight
    
    UpdateCoordinates Project.Item(ActiveProject).frmProject.cRectWND
    UpdatePanelConnections ActiveProject
    UpdateBoundary ActiveProject
    Draw ActiveProject
    Exit Function
ReverseErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: Function Reverse")
End Function

Public Function GetIsWiseLateralPressure()
    On Error GoTo GetIsWiseLateralPressureErr
    Dim LoadCase As cLoadCase
    Me.IsWiseLateralPressure = no
    For Each LoadCase In Me.colLoadCase
        If LoadCase.colStepWiseLateralPressure.Count > 0 Then
            Me.IsWiseLateralPressure = yes
            Exit For
        End If
    Next LoadCase
    Exit Function
GetIsWiseLateralPressureErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: Function GetIsWiseLateralPressure")
End Function

Public Function GetIsStructuralConstraints()
    On Error GoTo GetIsStructuralConstraintsErr
    Dim LoadCase As cLoadCase
    Me.IsStructuralConstraints = no
    For Each LoadCase In Me.colLoadCase
        If LoadCase.colStructuralConstraints.Count > 0 Then
            Me.IsStructuralConstraints = yes
            Exit For
        End If
    Next LoadCase
    Exit Function
GetIsStructuralConstraintsErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: Function GetIsStructuralConstraints")
End Function

Public Function GetGradient(ByVal ProjectIndex As Integer) As Double
    On Error GoTo GetGradientErr
    Dim Ymin As Double, ZMin As Double
    Dim Ymax As Double, ZMax As Double
    Select Case Project.Item(ProjectIndex).colNodes.Item(Me.cGeometry.InNode).y
        Case Is >= Project.Item(ProjectIndex).colNodes.Item(Me.cGeometry.OutNode).y
            Ymax = Project.Item(ProjectIndex).colNodes.Item(Me.cGeometry.InNode).y
            Ymin = Project.Item(ProjectIndex).colNodes.Item(Me.cGeometry.OutNode).y
        Case Else
            Ymin = Project.Item(ProjectIndex).colNodes.Item(Me.cGeometry.InNode).y
            Ymax = Project.Item(ProjectIndex).colNodes.Item(Me.cGeometry.OutNode).y
    End Select
    Select Case Project.Item(ProjectIndex).colNodes.Item(Me.cGeometry.InNode).z
        Case Is >= Project.Item(ProjectIndex).colNodes.Item(Me.cGeometry.OutNode).z
            ZMax = Project.Item(ProjectIndex).colNodes.Item(Me.cGeometry.InNode).z
            ZMin = Project.Item(ProjectIndex).colNodes.Item(Me.cGeometry.OutNode).z
        Case Else
            ZMin = Project.Item(ProjectIndex).colNodes.Item(Me.cGeometry.InNode).z
            ZMax = Project.Item(ProjectIndex).colNodes.Item(Me.cGeometry.OutNode).z
    End Select
    'unfinished
    Exit Function
GetGradientErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: Function GetGradient")
End Function

Public Function Clone() As cPanel
    On Error GoTo CloneErr
    Set Clone = New cPanel
    With Clone
        .index = Me.index
        .pLabel = Me.pLabel
        .Selected = Me.Selected
        .pType = Me.pType
        .RelatedDoubleHullPanel = Me.RelatedDoubleHullPanel
        .Region = Me.Region
        .pNumber = Me.pNumber
        Set .cGeometry = Me.cGeometry.Clone
        Set .cScantlings = Me.cScantlings.Clone
        Set .cMaterial = Me.cMaterial.Clone
        Set .cCostCAtMain = Me.cCostCAtMain.Clone
        .IsBuoyancyForce = Me.IsBuoyancyForce
        .LocalizedPressure = Me.LocalizedPressure
        .UniformLateralPressureVariation = Me.UniformLateralPressureVariation
        .IsWiseLateralPressure = Me.IsWiseLateralPressure
        .IsStructuralConstraints = Me.IsStructuralConstraints
        .IsDesignVariables = Me.IsDesignVariables
        .LeftCompartment = Me.LeftCompartment
        .RightCompartment = Me.RightCompartment
        .LateralPressureSide = Me.LateralPressureSide
        .NoOfGeomConstr = Me.NoOfGeomConstr
        .FramesFlangeThicknessUpdate = Me.FramesFlangeThicknessUpdate
        .StiffenersFlangeThicknessUpdate = Me.StiffenersFlangeThicknessUpdate
        .HighLighted = Me.HighLighted
        .Visible = Me.Visible
        'Class Collections
        Dim i As Integer, Connection As Integer
        For i = 1 To colConnections.Count
            Connection = colConnections.Item(i)
            .colConnections.Add Connection
        Next i
        
        Set .colDesignVariables = Me.colDesignVariables.Clone
        Set .colLoadCase = Me.colLoadCase.Clone
        Set .colMarsLoadCase = Me.colMarsLoadCase.Clone
        'for mars
        Set .colStrakes = Me.colStrakes.Clone
        Set .colSecondaryFrames = Me.colSecondaryFrames.Clone
        Set .colPrimaryStiffeners = Me.colPrimaryStiffeners.Clone
        '
        Dim Point_ As Integer
        Set .colAssessmentPoints = Me.colAssessmentPoints.Clone
        Set .colGeometricalConstraints = Me.colGeometricalConstraints.Clone
        Set .colBoundaryConditions = Me.colBoundaryConditions.Clone
        Set .colUpdatedScantlings = Me.colUpdatedScantlings.Clone
    End With
    Exit Function
CloneErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: Function Clone")
End Function

Public Function WritePanelUpdateASCII(ByVal iNoOfFile As Integer)
    On Error GoTo WritePanelUpdateASCIIErr
    With Me
        Put #iNoOfFile, , CInt(.index)
        Put #iNoOfFile, , CInt(.pNumber)
        Put #iNoOfFile, , CLng(.Region)
        .cScantlings.WriteASCII iNoOfFile
        .cGeometry.WriteASCII iNoOfFile
        If .index = 15 Then
            .index = 15
        End If
        
        .colBoundaryConditions.WriteASCII iNoOfFile
    End With
    Exit Function
WritePanelUpdateASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: Function WritePanelUpdateASCII")
End Function

Public Function WriteASCII(ByVal iNoOfFile As Integer)
    On Error GoTo WriteASCIIErr
    Dim ss As String * 100
    With Me
        Put #iNoOfFile, , .index
        ss = .pLabel
        Put #iNoOfFile, , ss
        Put #iNoOfFile, , CInt(.pType)
        Put #iNoOfFile, , .RelatedDoubleHullPanel
        Put #iNoOfFile, , .pNumber
        .cGeometry.WriteASCII iNoOfFile
        .cScantlings.WriteASCII iNoOfFile
        .cMaterial.WriteASCII iNoOfFile
        Put #iNoOfFile, , CInt(.IsBuoyancyForce)
        Put #iNoOfFile, , .LocalizedPressure
        Put #iNoOfFile, , CInt(.UniformLateralPressureVariation)
        Put #iNoOfFile, , CInt(.IsWiseLateralPressure)
        Put #iNoOfFile, , CInt(.IsStructuralConstraints)
        Put #iNoOfFile, , CInt(.IsDesignVariables)
        Put #iNoOfFile, , .LeftCompartment
        Put #iNoOfFile, , .RightCompartment
        Put #iNoOfFile, , CInt(.LateralPressureSide)
        Put #iNoOfFile, , .NoOfGeomConstr
        Put #iNoOfFile, , CInt(.FramesFlangeThicknessUpdate)
        Put #iNoOfFile, , CInt(.StiffenersFlangeThicknessUpdate)
        .colDesignVariables.WriteASCII iNoOfFile
        .colLoadCase.WriteASCII iNoOfFile
        .colMarsLoadCase.WriteASCII iNoOfFile
        .colAssessmentPoints.WriteASCII iNoOfFile
        .colGeometricalConstraints.WriteASCII iNoOfFile
'        If .index = 15 Then
'            .index = 15
'        End If
        .colBoundaryConditions.WriteASCII iNoOfFile
        '.colUpdatedScantlings.WriteASCII iNoOfFile
        .cCostCAtMain.WritePanelsASCII iNoOfFile
    End With
    Exit Function
WriteASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: Function WriteASCII")
End Function

Public Function ReadASCII(ByVal iNoOfFile As Integer)
    On Error GoTo ReadASCIIErr
    Dim ii As Integer, dd As Double, ss As String * 100
    With Me
        Get #iNoOfFile, , ii: .index = ii
        Get #iNoOfFile, , ss: .pLabel = Trim(ss)
        Get #iNoOfFile, , ii: .pType = ii
        Get #iNoOfFile, , ii:  .RelatedDoubleHullPanel = ii
        Get #iNoOfFile, , ii:  .pNumber = ii
        .cGeometry.ReadASCII iNoOfFile
        .cScantlings.ReadASCII iNoOfFile
        .cMaterial.ReadASCII iNoOfFile
        Get #iNoOfFile, , ii: .IsBuoyancyForce = ii
        Get #iNoOfFile, , dd: .LocalizedPressure = dd
        Get #iNoOfFile, , ii: .UniformLateralPressureVariation = ii
        Get #iNoOfFile, , ii: .IsWiseLateralPressure = ii
        Get #iNoOfFile, , ii: .IsStructuralConstraints = ii
        Get #iNoOfFile, , ii: .IsDesignVariables = ii
        Get #iNoOfFile, , ii: .LeftCompartment = ii
        Get #iNoOfFile, , ii: .RightCompartment = ii
        Get #iNoOfFile, , ii: .LateralPressureSide = ii
        Get #iNoOfFile, , ii: .NoOfGeomConstr = ii
        Get #iNoOfFile, , ii: .FramesFlangeThicknessUpdate = ii
        Get #iNoOfFile, , ii: .StiffenersFlangeThicknessUpdate = ii
        
        .colDesignVariables.ReadASCII iNoOfFile
        .colLoadCase.ReadASCII iNoOfFile
        .colAssessmentPoints.ReadASCII iNoOfFile
        .colGeometricalConstraints.ReadASCII iNoOfFile
        .colBoundaryConditions.ReadASCII iNoOfFile
        '.colUpdatedScantlings.ReadASCII iNoOfFile
    End With
    Exit Function
ReadASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: Function ReadASCII")
End Function

Private Sub Class_Initialize()
    #If DebugMode Then
        mlClassDebugID = GetNextClassDebugID()
        Debug.Print "'" & TypeName(Me) & "' instance " & mlClassDebugID & " created"
    #End If
    Set mvarcGeometry = New cGeometry
    Set mvarcScantlings = New cScantlings
    Set mvarcMaterial = New cMaterial
    Set mvarcCostCAtMain = New cCostCAtMain
    Set mvarcolDesignVariables = New colDesignVariables
    Set mvarcolAssessmentPoints = New colAssessmentPoints
    Set mvarcolUpdatedScantlings = New colUpdatedScantlings
End Sub

Private Sub Class_terminate()
    #If DebugMode Then
        Debug.Print "'" & TypeName(Me) & "' instance " & CStr(mlClassDebugID) & " is terminating"
    #End If
    Set mvarcGeometry = Nothing
    Set mvarcScantlings = Nothing
    Set mvarcMaterial = Nothing
    Set mvarcCostCAtMain = Nothing
    Set mvarcolDesignVariables = Nothing
    Set mvarcolAssessmentPoints = Nothing
    Set mvarcolUpdatedScantlings = Nothing
End Sub

#If DebugMode Then
    Public Property Get ClassDebugID()
        ClassDebugID = mlClassDebugID
    End Property
#End If
