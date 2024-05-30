VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cProject"
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
Private mvarYmin As Double
Private mvarYmax As Double
Private mvarZmin As Double
Private mvarZmax As Double
Private mvarIndex As Long
Private mvarsFileName As String 'Path + File Name
'Private mvarsDataFileName As String 'File Name
Private mvarcolPanel As colPanel
Private mvarcolPanelUpdate As colPanel
Private mvarcolNodes As colNodes
Private mvarcolVortex As colNodes
Private mvarCoordOrigin As cNode
Private mvarLastNode As Integer
Private mvarcHeader As cHeader
Private mvarcDisplaySettings As cDisplaySettings
Private mvarcSolution As cSolution
Private mvarScreenMode As ScreenModes
Private mvarFunctionMode As FunctionModes
Private mvarEconomicDraw As Boolean
Private mvarDataChanged As Boolean

'CostCAt
Private mvarcolCostCAtDHull As colCostCAtDHull
Private mvarcolCostCAtNappe As colCostCAtNappe

'Forms
Private mvarfrmProject As frmProject
Private mvarfrmTitle As frmTitle
Private mvarfrmOverallSpan As frmOverallSpan
Private mvarfrmOptimization As frmOptimization
Private mvarfrmDeadweight As frmDeadweight
Private mvarfrmFourierSeries As frmFourierSeries
Private mvarfrmOutput As frmOutput
Private mvarfrmLoadCases As frmLoadCases
'Private mvarfrmPressuresUniformelyDistributed As frmPressuresUniformelyDistributed
'Private mvarfrmOptions As frmOptions
Private mvarfrmCostSimplified As frmCostSimplified
Private mvarfrmMaterialCosts As frmMaterialCosts
Private mvarfrmCostCAt As frmCostCAt
Private mvarfrmCostCAtWorkshop As frmCostCAtWorkshop
Private mvarfrmCostCAtTables As frmCostCAtTables
Private mvarfrmBoundaryConditions As frmBoundaryConditions
Private mvarfrmSolution As frmSolution
Private mvarfrmNodes As frmNodes
Private mvarfrmPlateScantlings As frmPlateScantlings
Private mvarfrmDoubleHullScantlings As frmDoubleHullScantlings
Private mvarfrmBeamScantlings As frmBeamScantlings
Private mvarfrmLoads As frmLoads
Private mvarfrmRestrictions As frmRestrictions
Private mvarfrmDisplayOptions As frmDisplayOptions
Private mvarfrmMoveOrigin As frmMoveOrigin
Private mvarfrmOptiGlobal As frmOptiGlobal
Private mvarfrmSortPanels As frmSortPanels
Private mvarfrmHull_Girder As frmHull_Girder
Private mvarfrmDeletePanel As frmDeletePanel
Private mvarfrmReversePanel As frmReversePanel
Private mvarfrmRuleLength As frmRuleLength
Private mvarfrmGeometricProperties As frmGeometricProperties
Private mvarfrmWrapPressures As frmWrapPressures
Private mvarfrmOpenGL As frmOpenGL
Private mvarfrmChartObjectiveFunction As frmChartObjectiveFunction
Private mvarfrmChartGlobalRestrictions As frmChartGlobalRestrictions
Private mvarfrmChartDesignVariables As frmChartDesignVariables
Private mvarfrmModellingAssistant As frmModellingAssistant
'New Optimization Forms
Private mvarfrmDesignVariables As frmDesignVariables

'Old Optimization Forms
Private mvarold_frmOpti1 As old_FrmOpti1
Private mvarold_frmOpti2 As old_frmOpti2
Private mvarold_frmopti3_1 As old_FrmOpti3_1
Private mvarold_frmEqConstr1 As old_frmEqConstr1
Private mvarold_frmGlobal As old_frmGlobal
Private mvarold_frmGraphGravityVar As old_frmGraphGravityVar
Private mvarold_frmGrphObjFct As old_frmGrphObjFct
Private mvarold_frmSensitivities As old_frmSensitivities

Private mvarcolCompartments As colCompartments
Private mvarcolMaterials As colMaterials

Private mvarbNewProjectFirstSave As Boolean
Private mvarFileVersionNumber As String

Private mvarProjectMaps As colProject


'Enums
Public Enum ScreenModes
    NORMAL_MODE = -1
    SINGLE_SELECTION_MODE = 0
    MULTIPLE_SELECTION_MODE = 1
    ZOOM_IN_MODE = 2
    ZOOM_OUT_MODE = 3
    ZOOM_WIN_MODE = 4
    PAN_MODE = 5
    ZOOM_FULL_MODE = 6
End Enum

Public Enum FunctionModes
    NO_FUNCTION = 10
    ADD_PLATE_FIRST_NODE_FUNCTION = 11
    ADD_PLATE_SECOND_NODE_FUNCTION = 12
    ADD_BEAM_FIRST_NODE_FUNCTION = 13
    ADD_BEAM_SECOND_NODE_FUNCTION = 14
    ADD_DOUBLEHULL_FIRST_PANEL_FUNCTION = 15
    ADD_DOUBLEHULL_SECOND_PANEL_FUNCTION = 16
    ERASE_MODE_FUNCTION = 17
    EXPLODE_DOUBLEHULL_FUNCTION = 18
    REVERSE_PANEL_FUNCTION = 19
    DIVIDE_PANEL_FUNCTION = 20
    GET_DIST_FIRST_NODE_FUNCTION = 21
    GET_DIST_SECOND_NODE_FUNCTION = 22
    MATCH_PROPERTIES_FUNCTION = 23
    PASTE_PROPERTIES_FUNCTION = 24
    PANEL_TYPE_INNERDHULL = 25
    PANEL_TYPE_OUTERDHULL = 26
    PANEL_TYPE_PRIMARY_LONGITUDINAL = 27
    PANEL_TYPE_BILGE = 28
    PANEL_TYPE_SIMPLE_SHELL = 29
    ADD_NAPPE = 30
    PANEL_TYPE_VIRTUAL = 31
    ADD_COUPLE_GIRDER_ON_NAPPE = 32
    ADD_BOUNDARY = 33
    REDISTRIBUTE_LOADS_DH_FIRST_PANEL = 34
    REDISTRIBUTE_LOADS_DH_SECOND_PANEL = 35
End Enum

Public Property Let bNewProjectFirstSave(ByVal vData As Boolean)
    On Error GoTo bNewProjectFirstSaveLetErr
    mvarbNewProjectFirstSave = vData
    Exit Property
bNewProjectFirstSaveLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cNode: bNewProjectFirstSave Property Let")
End Property

Public Property Get bNewProjectFirstSave() As Boolean
    On Error GoTo bNewProjectFirstSaveGetErr
    bNewProjectFirstSave = mvarbNewProjectFirstSave
    Exit Property
bNewProjectFirstSaveGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cNode: bNewProjectFirstSave Property Get")
End Property

Public Property Let FileVersionNumber(ByVal vData As String)
    On Error GoTo FileVersionNumberLetErr
    mvarFileVersionNumber = vData
    Exit Property
FileVersionNumberLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cNode: FileVersionNumber Property Let")
End Property

Public Property Get FileVersionNumber() As String
    On Error GoTo FileVersionNumberGetErr
    FileVersionNumber = mvarFileVersionNumber
    Exit Property
FileVersionNumberGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cNode: FileVersionNumber Property Get")
End Property

Public Property Get index() As Long
    On Error GoTo IndexGetErr
    index = mvarIndex
    Exit Property
IndexGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: Index Property Get")
End Property

Public Property Let index(ByVal vData As Long)
    On Error GoTo IndexLetErr
    mvarIndex = vData
    'sDataFileName = GetFileName(vData)
    Exit Property
IndexLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: Index Property Let")
End Property

Public Property Get Ymin() As Double
    On Error GoTo YminGetErr
    Ymin = mvarYmin
    Exit Property
YminGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: Ymin Property Get")
End Property

Public Property Let Ymin(ByVal vData As Double)
    On Error GoTo YminLetErr
    mvarYmin = vData
    Exit Property
YminLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: Ymin Property Let")
End Property

Public Property Get Ymax() As Double
    On Error GoTo YmaxGetErr
    Ymax = mvarYmax
    Exit Property
YmaxGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: Ymax Property Get")
End Property

Public Property Let Ymax(ByVal vData As Double)
    On Error GoTo YmaxLetErr
    mvarYmax = vData
    Exit Property
YmaxLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: Ymax Property Let")
End Property

Public Property Get ZMin() As Double
    On Error GoTo ZminGetErr
    ZMin = mvarZmin
    Exit Property
ZminGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: Zmin Property Get")
End Property

Public Property Let ZMin(ByVal vData As Double)
    On Error GoTo ZminLetErr
    mvarZmin = vData
    Exit Property
ZminLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: Zmin Property Let")
End Property

Public Property Get ZMax() As Double
    On Error GoTo ZmaxGetErr
    ZMax = mvarZmax
    Exit Property
ZmaxGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: Zmax Property Get")
End Property

Public Property Let ZMax(ByVal vData As Double)
    On Error GoTo ZmaxLetErr
    mvarZmax = vData
    Exit Property
ZmaxLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: Zmax Property Let")
End Property

Public Property Get sFileName() As String
    On Error GoTo sFileNameGetErr
    sFileName = mvarsFileName
    Exit Property
sFileNameGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: sFileName Property Get")
End Property

Public Property Let sFileName(ByVal vData As String)
    On Error GoTo sFileNameLetErr
    mvarsFileName = vData
    'sDataFileName = GetFileName(vData)
    Exit Property
sFileNameLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: sFileName Property Let")
End Property

'Public Property Get sDataFileName() As String
'    On Error GoTo  sDataFileNameGetErr
'    sDataFileName = mvarsDataFileName
'    Exit Property
'sDataFileNameGetErr:
'    Call RaiseError(MyUnhandledError,Err.Description & "." & vbCrLf &  "cProject: sDataFileName Property Get")
'End Property
'
'Public Property Let sDataFileName(ByVal vData As String)
'    On Error GoTo  sDataFileNameLetErr
'    mvarsDataFileName = vData
'    Exit Property
'sDataFileNameLetErr:
'    Call RaiseError(MyUnhandledError,Err.Description & "." & vbCrLf &  "cProject: sDataFileName Property Let")
'End Property

Public Property Get cHeader() As cHeader
    On Error GoTo cHeaderGetErr
    Set cHeader = mvarcHeader
    Exit Property
cHeaderGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: cHeader Property Get")
End Property

Public Property Set cHeader(ByVal vData As cHeader)
    On Error GoTo cHeaderSetErr
    Set mvarcHeader = vData
    Exit Property
cHeaderSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: cHeader Property Set")
End Property

Public Property Get cDisplaySettings() As cDisplaySettings
    On Error GoTo cDisplaySettingsGetErr
    Set cDisplaySettings = mvarcDisplaySettings
    Exit Property
cDisplaySettingsGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: cDisplaySettings Property Get")
End Property

Public Property Set cDisplaySettings(ByVal vData As cDisplaySettings)
    On Error GoTo cDisplaySettingsSetErr
    Set mvarcDisplaySettings = vData
    Exit Property
cDisplaySettingsSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: cDisplaySettings Property Set")
End Property

Public Property Get cSolution() As cSolution
    On Error GoTo cSolutionGetErr
    Set cSolution = mvarcSolution
    Exit Property
cSolutionGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: cSolution Property Get")
End Property

Public Property Set cSolution(ByVal vData As cSolution)
    On Error GoTo cSolutionSetErr
    Set mvarcSolution = vData
    Exit Property
cSolutionSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: cSolution Property Set")
End Property

Public Property Get CoordOrigin() As cNode
    On Error GoTo CoordOriginGetErr
    Set CoordOrigin = mvarCoordOrigin
    Exit Property
CoordOriginGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: CoordOrigin Property Get")
End Property

Public Property Set CoordOrigin(ByVal vData As cNode)
    On Error GoTo CoordOriginSetErr
    Set mvarCoordOrigin = vData
    Exit Property
CoordOriginSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: CoordOrigin Property Set")
End Property

Public Property Get LastNode() As Integer
    On Error GoTo LastNodeGetErr
    LastNode = mvarLastNode
    Exit Property
LastNodeGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: LastNode Property Get")
End Property

Public Property Let LastNode(ByVal vData As Integer)
    On Error GoTo LastNodeSetErr
    mvarLastNode = vData
    Exit Property
LastNodeSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: LastNode Property Set")
End Property

Public Property Get ScreenMode() As ScreenModes
    On Error GoTo ScreenModeGetErr
        ScreenMode = mvarScreenMode
    Exit Property
ScreenModeGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: ScreenMode Property Get")
End Property

Public Property Let ScreenMode(ByVal vData As ScreenModes)
    On Error GoTo ScreenModeLetErr
        mvarScreenMode = vData
    Exit Property
ScreenModeLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: ScreenMode Property Let")
End Property

Public Property Get FunctionMode() As FunctionModes
    On Error GoTo FunctionModeGetErr
        FunctionMode = mvarFunctionMode
    Exit Property
FunctionModeGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: FunctionMode Property Get")
End Property

Public Property Let FunctionMode(ByVal vData As FunctionModes)
    On Error GoTo FunctionModeLetErr
        mvarFunctionMode = vData
    Exit Property
FunctionModeLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: FunctionMode Property Let")
End Property

Public Property Get EconomicDraw() As Boolean
    On Error GoTo EconomicDrawGetErr
        EconomicDraw = mvarEconomicDraw
    Exit Property
EconomicDrawGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: EconomicDraw Property Get")
End Property

Public Property Let EconomicDraw(ByVal vData As Boolean)
    On Error GoTo EconomicDrawLetErr
        mvarEconomicDraw = vData
    Exit Property
EconomicDrawLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: EconomicDraw Property Let")
End Property

Public Property Get DataChanged() As Boolean
    On Error GoTo DataChangedGetErr
        DataChanged = mvarDataChanged
    Exit Property
DataChangedGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: DataChanged Property Get")
End Property

Public Property Let DataChanged(ByVal vData As Boolean)
    On Error GoTo DataChangedLetErr
        CheckModel Me.index
        mvarDataChanged = vData
    Exit Property
DataChangedLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: DataChanged Property Let")
End Property

Public Property Get colPanel() As colPanel
    On Error GoTo colPanelGetErr
    If mvarcolPanel Is Nothing Then
        Set mvarcolPanel = New colPanel
    End If
    Set colPanel = mvarcolPanel
    Exit Property
colPanelGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: colPanel Property Get")
End Property

Public Property Set colPanel(vData As colPanel)
    On Error GoTo colPanelSetErr
    Set mvarcolPanel = vData
    Exit Property
colPanelSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: colPanel Property Set")
End Property

Public Property Get colPanelUpdate() As colPanel
    On Error GoTo colPanelUpdateGetErr
    If mvarcolPanelUpdate Is Nothing Then
        Set mvarcolPanelUpdate = New colPanel
    End If
    Set colPanelUpdate = mvarcolPanelUpdate
    Exit Property
colPanelUpdateGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: colPanelUpdate Property Get")
End Property

Public Property Set colPanelUpdate(vData As colPanel)
    On Error GoTo colPanelUpdateSetErr
    Set mvarcolPanelUpdate = vData
    Exit Property
colPanelUpdateSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: colPanelUpdate Property Set")
End Property

Public Property Get colCostCAtDHull() As colCostCAtDHull
    On Error GoTo colCostCAtDHullGetErr
    If mvarcolCostCAtDHull Is Nothing Then
        Set mvarcolCostCAtDHull = New colCostCAtDHull
    End If
    Set colCostCAtDHull = mvarcolCostCAtDHull
    Exit Property
colCostCAtDHullGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colPanel: colCostCAtDHull Property Get")
End Property

Public Property Set colCostCAtDHull(vData As colCostCAtDHull)
    On Error GoTo colCostCAtDHullSetErr
    Set mvarcolCostCAtDHull = vData
    Exit Property
colCostCAtDHullSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colPanel: colCostCAtDHull Property Set")
End Property

Public Property Get colCostCAtNappe() As colCostCAtNappe
    On Error GoTo colCostCAtNappeGetErr
    If mvarcolCostCAtNappe Is Nothing Then
        Set mvarcolCostCAtNappe = New colCostCAtNappe
    End If
    Set colCostCAtNappe = mvarcolCostCAtNappe
    Exit Property
colCostCAtNappeGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colPanel: colCostCAtNappe Property Get")
End Property

Public Property Set colCostCAtNappe(vData As colCostCAtNappe)
    On Error GoTo colCostCAtNappeSetErr
    Set mvarcolCostCAtNappe = vData
    Exit Property
colCostCAtNappeSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colPanel: colCostCAtNappe Property Set")
End Property

Public Property Get colNodes() As colNodes
    On Error GoTo colNodesGetErr
    If mvarcolNodes Is Nothing Then
        Set mvarcolNodes = New colNodes
    End If
    Set colNodes = mvarcolNodes
    Exit Property
colNodesGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: colNodes Property Get")
End Property

Public Property Set colNodes(vData As colNodes)
    On Error GoTo colNodesSetErr
    Set mvarcolNodes = vData
    Exit Property
colNodesSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: colNodes Property Set")
End Property

Public Property Get colVortex() As colNodes
    On Error GoTo colVortexGetErr
    If mvarcolVortex Is Nothing Then
        Set mvarcolVortex = New colNodes
    End If
    Set colVortex = mvarcolVortex
    Exit Property
colVortexGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: colVortex Property Get")
End Property

Public Property Set colVortex(vData As colNodes)
    On Error GoTo colVortexSetErr
    Set mvarcolVortex = vData
    Exit Property
colVortexSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: colVortex Property Set")
End Property

Public Property Get colCompartments() As colCompartments
    On Error GoTo colCompartmentsGetErr
    If mvarcolCompartments Is Nothing Then
        Set mvarcolCompartments = New colCompartments
    End If
    Set colCompartments = mvarcolCompartments
    Exit Property
colCompartmentsGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: colCompartments Property Get")
End Property

Public Property Set colCompartments(vData As colCompartments)
    On Error GoTo colCompartmentsSetErr
    Set mvarcolCompartments = vData
    Exit Property
colCompartmentsSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: colCompartments Property Set")
End Property

Public Property Get ProjectMaps() As colProject
    On Error GoTo ProjectMapsGetErr
    If mvarProjectMaps Is Nothing Then
        Set mvarProjectMaps = New colProject
    End If
    Set ProjectMaps = mvarProjectMaps
    Exit Property
ProjectMapsGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: ProjectMaps Property Get")
End Property

Public Property Set ProjectMaps(vData As colProject)
    On Error GoTo ProjectMapsSetErr
    Set mvarProjectMaps = vData
    Exit Property
ProjectMapsSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: ProjectMaps Property Set")
End Property

Public Property Get colMaterials() As colMaterials
    On Error GoTo colMaterialsGetErr
    If mvarcolMaterials Is Nothing Then
        Set mvarcolMaterials = New colMaterials
    End If
    Set colMaterials = mvarcolMaterials
    Exit Property
colMaterialsGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: colMaterials Property Get")
End Property

Public Property Set colMaterials(vData As colMaterials)
    On Error GoTo colMaterialsSetErr
    Set mvarcolMaterials = vData
    Exit Property
colMaterialsSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: colMaterials Property Set")
End Property

'Forms
Public Property Get frmProject() As frmProject
    On Error GoTo frmProjectGetErr
    Set frmProject = mvarfrmProject
    Exit Property
frmProjectGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmProject Property Get")
End Property

Public Property Set frmProject(vData As frmProject)
    On Error GoTo frmProjectSetErr
    Set mvarfrmProject = vData
    Exit Property
frmProjectSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmProject Property Set")
End Property

Public Property Get frmTitle() As frmTitle
    On Error GoTo frmTitleGetErr
    Set frmTitle = mvarfrmTitle
    Exit Property
frmTitleGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmTitle Property Get")
End Property

Public Property Set frmTitle(vData As frmTitle)
    On Error GoTo frmTitleSetErr
    Set mvarfrmTitle = vData
    Exit Property
frmTitleSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmTitle Property Set")
End Property

Public Property Get frmOverallSpan() As frmOverallSpan
    On Error GoTo frmOverallSpanGetErr
    Set frmOverallSpan = mvarfrmOverallSpan
    Exit Property
frmOverallSpanGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmOverallSpan Property Get")
End Property

Public Property Set frmOverallSpan(vData As frmOverallSpan)
    On Error GoTo frmOverallSpanSetErr
    Set mvarfrmOverallSpan = vData
    Exit Property
frmOverallSpanSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmOverallSpan Property Set")
End Property

Public Property Get frmOptimization() As frmOptimization
    On Error GoTo frmOptimizationGetErr
    Set frmOptimization = mvarfrmOptimization
    Exit Property
frmOptimizationGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmOptimization Property Get")
End Property

Public Property Set frmOptimization(vData As frmOptimization)
    On Error GoTo frmOptimizationSetErr
    Set mvarfrmOptimization = vData
    Exit Property
frmOptimizationSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmOptimization Property Set")
End Property

Public Property Get frmDeadweight() As frmDeadweight
    On Error GoTo frmDeadweightGetErr
    Set frmDeadweight = mvarfrmDeadweight
    Exit Property
frmDeadweightGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmDeadweight Property Get")
End Property

Public Property Set frmDeadweight(vData As frmDeadweight)
    On Error GoTo frmDeadweightSetErr
    Set mvarfrmDeadweight = vData
    Exit Property
frmDeadweightSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmDeadweight Property Set")
End Property

Public Property Get frmFourierSeries() As frmFourierSeries
    On Error GoTo frmFourierSeriesGetErr
    Set frmFourierSeries = mvarfrmFourierSeries
    Exit Property
frmFourierSeriesGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmFourierSeries Property Get")
End Property

Public Property Set frmFourierSeries(vData As frmFourierSeries)
    On Error GoTo frmFourierSeriesSetErr
    Set mvarfrmFourierSeries = vData
    Exit Property
frmFourierSeriesSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmFourierSeries Property Set")
End Property

Public Property Get frmOutput() As frmOutput
    On Error GoTo frmOutputGetErr
    Set frmOutput = mvarfrmOutput
    Exit Property
frmOutputGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmOutput Property Get")
End Property

Public Property Set frmOutput(vData As frmOutput)
    On Error GoTo frmOutputSetErr
    Set mvarfrmOutput = vData
    Exit Property
frmOutputSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmOutput Property Set")
End Property

Public Property Get frmLoadCases() As frmLoadCases
    On Error GoTo frmLoadCasesGetErr
    Set frmLoadCases = mvarfrmLoadCases
    Exit Property
frmLoadCasesGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmLoadCases Property Get")
End Property

Public Property Set frmLoadCases(vData As frmLoadCases)
    On Error GoTo frmLoadCasesSetErr
    Set mvarfrmLoadCases = vData
    Exit Property
frmLoadCasesSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmLoadCases Property Set")
End Property

'Public Property Get frmOptions() As frmOptions
'    On Error GoTo  frmOptionsGetErr
'    Set frmOptions = mvarfrmOptions
'    Exit Property
'frmOptionsGetErr:
'    Call RaiseError(MyUnhandledError,Err.Description & "." & vbCrLf &  "cProject: frmOptions Property Get")
'End Property
'
'Public Property Set frmOptions(vData As frmOptions)
'    On Error GoTo  frmOptionsSetErr
'    Set mvarfrmOptions = vData
'    Exit Property
'frmOptionsSetErr:
'    Call RaiseError(MyUnhandledError,Err.Description & "." & vbCrLf &  "cProject: frmOptions Property Set")
'End Property

Public Property Get frmCostSimplified() As frmCostSimplified
    On Error GoTo frmCostSimplifiedGetErr
    Set frmCostSimplified = mvarfrmCostSimplified
    Exit Property
frmCostSimplifiedGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmCostSimplified Property Get")
End Property

Public Property Set frmCostSimplified(vData As frmCostSimplified)
    On Error GoTo frmCostSimplifiedSetErr
    Set mvarfrmCostSimplified = vData
    Exit Property
frmCostSimplifiedSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmCostSimplified Property Set")
End Property

Public Property Get frmMaterialCosts() As frmMaterialCosts
    On Error GoTo frmMaterialCostsGetErr
    Set frmMaterialCosts = mvarfrmMaterialCosts
    Exit Property
frmMaterialCostsGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmMaterialCosts Property Get")
End Property

Public Property Set frmMaterialCosts(vData As frmMaterialCosts)
    On Error GoTo frmMaterialCostsSetErr
    Set mvarfrmMaterialCosts = vData
    Exit Property
frmMaterialCostsSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmMaterialCosts Property Set")
End Property

Public Property Get frmCostCAt() As frmCostCAt
    On Error GoTo frmCOSTCAtGetErr
    Set frmCostCAt = mvarfrmCostCAt
    Exit Property
frmCOSTCAtGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmCOSTCAt Property Get")
End Property

Public Property Set frmCostCAt(vData As frmCostCAt)
    On Error GoTo frmCOSTCAtSetErr
    Set mvarfrmCostCAt = vData
    Exit Property
frmCOSTCAtSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmCOSTCAt Property Set")
End Property

Public Property Get frmCostCAtWorkshop() As frmCostCAtWorkshop
    On Error GoTo frmCostCAtWorkshopGetErr
    Set frmCostCAtWorkshop = mvarfrmCostCAtWorkshop
    Exit Property
frmCostCAtWorkshopGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmCostCAtWorkshop Property Get")
End Property

Public Property Set frmCostCAtWorkshop(vData As frmCostCAtWorkshop)
    On Error GoTo frmCostCAtWorkshopSetErr
    Set mvarfrmCostCAtWorkshop = vData
    Exit Property
frmCostCAtWorkshopSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmCostCAtWorkshop Property Set")
End Property

Public Property Get frmCostCAtTables() As frmCostCAtTables
    On Error GoTo frmCostCAtTablesGetErr
    Set frmCostCAtTables = mvarfrmCostCAtTables
    Exit Property
frmCostCAtTablesGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmCostCAtTables Property Get")
End Property

Public Property Set frmCostCAtTables(vData As frmCostCAtTables)
    On Error GoTo frmCostCAtTablesSetErr
    Set mvarfrmCostCAtTables = vData
    Exit Property
frmCostCAtTablesSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmCostCAtTables Property Set")
End Property

Public Property Get frmBoundaryConditions() As frmBoundaryConditions
    On Error GoTo frmBoundaryConditionsGetErr
    Set frmBoundaryConditions = mvarfrmBoundaryConditions
    Exit Property
frmBoundaryConditionsGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmBoundaryConditions Property Get")
End Property

Public Property Set frmBoundaryConditions(vData As frmBoundaryConditions)
    On Error GoTo frmBoundaryConditionsSetErr
    Set mvarfrmBoundaryConditions = vData
    Exit Property
frmBoundaryConditionsSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmBoundaryConditions Property Set")
End Property

Public Property Get frmSolution() As frmSolution
    On Error GoTo frmSolutionGetErr
    Set frmSolution = mvarfrmSolution
    Exit Property
frmSolutionGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmSolution Property Get")
End Property

Public Property Set frmSolution(vData As frmSolution)
    On Error GoTo frmSolutionSetErr
    Set mvarfrmSolution = vData
    Exit Property
frmSolutionSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmSolution Property Set")
End Property

Public Property Get frmNodes() As frmNodes
    On Error GoTo frmNodesGetErr
    Set frmNodes = mvarfrmNodes
    Exit Property
frmNodesGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmNodes Property Get")
End Property

Public Property Set frmNodes(vData As frmNodes)
    On Error GoTo frmNodesSetErr
    Set mvarfrmNodes = vData
    Exit Property
frmNodesSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmNodes Property Set")
End Property

Public Property Get frmPlateScantlings() As frmPlateScantlings
    On Error GoTo frmPlateScantlingsGetErr
    Set frmPlateScantlings = mvarfrmPlateScantlings
    Exit Property
frmPlateScantlingsGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmPlateScantlings Property Get")
End Property

Public Property Set frmPlateScantlings(vData As frmPlateScantlings)
    On Error GoTo frmPlateScantlingsSetErr
    Set mvarfrmPlateScantlings = vData
    Exit Property
frmPlateScantlingsSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmPlateScantlings Property Set")
End Property

Public Property Get frmDoubleHullScantlings() As frmDoubleHullScantlings
    On Error GoTo frmDoubleHullScantlingsGetErr
    Set frmDoubleHullScantlings = mvarfrmDoubleHullScantlings
    Exit Property
frmDoubleHullScantlingsGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmDoubleHullScantlings Property Get")
End Property

Public Property Set frmDoubleHullScantlings(vData As frmDoubleHullScantlings)
    On Error GoTo frmDoubleHullScantlingsSetErr
    Set mvarfrmDoubleHullScantlings = vData
    Exit Property
frmDoubleHullScantlingsSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmDoubleHullScantlings Property Set")
End Property

Public Property Get frmBeamScantlings() As frmBeamScantlings
    On Error GoTo frmBeamScantlingsGetErr
    Set frmBeamScantlings = mvarfrmBeamScantlings
    Exit Property
frmBeamScantlingsGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmBeamScantlings Property Get")
End Property

Public Property Set frmBeamScantlings(vData As frmBeamScantlings)
    On Error GoTo frmBeamScantlingsSetErr
    Set mvarfrmBeamScantlings = vData
    Exit Property
frmBeamScantlingsSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmBeamScantlings Property Set")
End Property

Public Property Get frmLoads() As frmLoads
    On Error GoTo frmLoadsGetErr
    Set frmLoads = mvarfrmLoads
    Exit Property
frmLoadsGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmLoads Property Get")
End Property

Public Property Set frmLoads(vData As frmLoads)
    On Error GoTo frmLoadsSetErr
    Set mvarfrmLoads = vData
    Exit Property
frmLoadsSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmLoads Property Set")
End Property

Public Property Get frmRestrictions() As frmRestrictions
    On Error GoTo frmRestrictionsGetErr
    Set frmRestrictions = mvarfrmRestrictions
    Exit Property
frmRestrictionsGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmRestrictions Property Get")
End Property

Public Property Set frmRestrictions(vData As frmRestrictions)
    On Error GoTo frmRestrictionsSetErr
    Set mvarfrmRestrictions = vData
    Exit Property
frmRestrictionsSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmRestrictions Property Set")
End Property

Public Property Get frmDisplayOptions() As frmDisplayOptions
    On Error GoTo frmDisplayOptionsGetErr
    Set frmDisplayOptions = mvarfrmDisplayOptions
    Exit Property
frmDisplayOptionsGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmDisplayOptions Property Get")
End Property

Public Property Set frmDisplayOptions(vData As frmDisplayOptions)
    On Error GoTo frmDisplayOptionsSetErr
    Set mvarfrmDisplayOptions = vData
    Exit Property
frmDisplayOptionsSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmDisplayOptions Property Set")
End Property

Public Property Get frmMoveOrigin() As frmMoveOrigin
    On Error GoTo frmMoveOriginGetErr
    Set frmMoveOrigin = mvarfrmMoveOrigin
    Exit Property
frmMoveOriginGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmMoveOrigin Property Get")
End Property

Public Property Set frmMoveOrigin(vData As frmMoveOrigin)
    On Error GoTo frmMoveOriginSetErr
    Set mvarfrmMoveOrigin = vData
    Exit Property
frmMoveOriginSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmMoveOrigin Property Set")
End Property

Public Property Get frmOptiGlobal() As frmOptiGlobal
    On Error GoTo frmOptiGlobalGetErr
    Set frmOptiGlobal = mvarfrmOptiGlobal
    Exit Property
frmOptiGlobalGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmOptiGlobal Property Get")
End Property

Public Property Set frmOptiGlobal(vData As frmOptiGlobal)
    On Error GoTo frmOptiGlobalSetErr
    Set mvarfrmOptiGlobal = vData
    Exit Property
frmOptiGlobalSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmOptiGlobal Property Set")
End Property

Public Property Get frmSortPanels() As frmSortPanels
    On Error GoTo frmSortPanelsGetErr
    Set frmSortPanels = mvarfrmSortPanels
    Exit Property
frmSortPanelsGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmSortPanels Property Get")
End Property

Public Property Set frmSortPanels(vData As frmSortPanels)
    On Error GoTo frmSortPanelsSetErr
    Set mvarfrmSortPanels = vData
    Exit Property
frmSortPanelsSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmSortPanels Property Set")
End Property

Public Property Get frmHull_Girder() As frmHull_Girder
    On Error GoTo frmHull_GirderGetErr
    Set frmHull_Girder = mvarfrmHull_Girder
    Exit Property
frmHull_GirderGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmHull_Girder Property Get")
End Property

Public Property Set frmHull_Girder(vData As frmHull_Girder)
    On Error GoTo frmHull_GirderSetErr
    Set mvarfrmHull_Girder = vData
    Exit Property
frmHull_GirderSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmHull_Girder Property Set")
End Property

Public Property Get frmDeletePanel() As frmDeletePanel
    On Error GoTo frmDeletePanelGetErr
    Set frmDeletePanel = mvarfrmDeletePanel
    Exit Property
frmDeletePanelGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmDeletePanel Property Get")
End Property

Public Property Set frmDeletePanel(vData As frmDeletePanel)
    On Error GoTo frmDeletePanelSetErr
    Set mvarfrmDeletePanel = vData
    Exit Property
frmDeletePanelSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmDeletePanel Property Set")
End Property

Public Property Get frmReversePanel() As frmReversePanel
    On Error GoTo frmReversePanelGetErr
    Set frmReversePanel = mvarfrmReversePanel
    Exit Property
frmReversePanelGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmReversePanel Property Get")
End Property

Public Property Set frmReversePanel(vData As frmReversePanel)
    On Error GoTo frmReversePanelSetErr
    Set mvarfrmReversePanel = vData
    Exit Property
frmReversePanelSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmReversePanel Property Set")
End Property

'New Optimization Modules
Public Property Get frmDesignVariables() As frmDesignVariables
    On Error GoTo frmDesignVariablesGetErr
    Set frmDesignVariables = mvarfrmDesignVariables
    Exit Property
frmDesignVariablesGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmDesignVariables Property Get")
End Property

Public Property Set frmDesignVariables(vData As frmDesignVariables)
    On Error GoTo frmDesignVariablesSetErr
    Set mvarfrmDesignVariables = vData
    Exit Property
frmDesignVariablesSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmDesignVariables Property Set")
End Property

'Old Optimization Modules
Public Property Get old_FrmOpti1() As old_FrmOpti1
    On Error GoTo old_frmOpti1GetErr
    Set old_FrmOpti1 = mvarold_frmOpti1
    Exit Property
old_frmOpti1GetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: old_frmOpti1 Property Get")
End Property

Public Property Set old_FrmOpti1(vData As old_FrmOpti1)
    On Error GoTo old_frmOpti1SetErr
    Set mvarold_frmOpti1 = vData
    Exit Property
old_frmOpti1SetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: old_frmOpti1 Property Set")
End Property

Public Property Get old_frmOpti2() As old_frmOpti2
    On Error GoTo old_frmOpti2GetErr
    Set old_frmOpti2 = mvarold_frmOpti2
    Exit Property
old_frmOpti2GetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: old_frmOpti2 Property Get")
End Property

Public Property Set old_frmOpti2(vData As old_frmOpti2)
    On Error GoTo old_frmOpti2SetErr
    Set mvarold_frmOpti2 = vData
    Exit Property
old_frmOpti2SetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: old_frmOpti2 Property Set")
End Property

Public Property Get old_FrmOpti3_1() As old_FrmOpti3_1
    On Error GoTo old_FrmOpti3_1GetErr
    Set old_FrmOpti3_1 = mvarold_frmopti3_1
    Exit Property
old_FrmOpti3_1GetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: old_FrmOpti3_1 Property Get")
End Property

Public Property Set old_FrmOpti3_1(vData As old_FrmOpti3_1)
    On Error GoTo old_FrmOpti3_1SetErr
    Set mvarold_frmopti3_1 = vData
    Exit Property
old_FrmOpti3_1SetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: old_FrmOpti3_1 Property Set")
End Property

Public Property Get old_frmEqConstr1() As old_frmEqConstr1
    On Error GoTo old_frmEqConstr1GetErr
    Set old_frmEqConstr1 = mvarold_frmEqConstr1
    Exit Property
old_frmEqConstr1GetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: old_frmEqConstr1 Property Get")
End Property

Public Property Set old_frmEqConstr1(vData As old_frmEqConstr1)
    On Error GoTo old_frmEqConstr1SetErr
    Set mvarold_frmEqConstr1 = vData
    Exit Property
old_frmEqConstr1SetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: old_frmEqConstr1 Property Set")
End Property

Public Property Get old_frmGlobal() As old_frmGlobal
    On Error GoTo old_frmGlobalGetErr
    Set old_frmGlobal = mvarold_frmGlobal
    Exit Property
old_frmGlobalGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: old_frmGlobal Property Get")
End Property

Public Property Set old_frmGlobal(vData As old_frmGlobal)
    On Error GoTo old_frmGlobalSetErr
    Set mvarold_frmGlobal = vData
    Exit Property
old_frmGlobalSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: old_frmGlobal Property Set")
End Property

Public Property Get old_frmGraphGravityVar() As old_frmGraphGravityVar
    On Error GoTo old_frmGraphGravityVarErr
    Set old_frmGraphGravityVar = mvarold_frmGraphGravityVar
    Exit Property
old_frmGraphGravityVarErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: old_frmGraphGravityVar Property Get")
End Property

Public Property Set old_frmGraphGravityVar(vData As old_frmGraphGravityVar)
    On Error GoTo old_frmGraphGravityVarErr
    Set mvarold_frmGraphGravityVar = vData
    Exit Property
old_frmGraphGravityVarErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: old_frmGraphGravityVar Property Set")
End Property


Public Property Get old_frmGrphObjFct() As old_frmGrphObjFct
    On Error GoTo old_frmGrphObjFctErr
    Set old_frmGrphObjFct = mvarold_frmGrphObjFct
    Exit Property
old_frmGrphObjFctErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: old_frmGrphObjFct Property Get")
End Property

Public Property Set old_frmGrphObjFct(vData As old_frmGrphObjFct)
    On Error GoTo old_frmGrphObjFctErr
    Set mvarold_frmGrphObjFct = vData
    Exit Property
old_frmGrphObjFctErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: old_frmGrphObjFct Property Set")
End Property

Public Property Get old_frmSensitivities() As old_frmSensitivities
    On Error GoTo old_frmSensitivitiesErr
    Set old_frmSensitivities = mvarold_frmSensitivities
    Exit Property
old_frmSensitivitiesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: old_frmSensitivities Property Get")
End Property

Public Property Set old_frmSensitivities(vData As old_frmSensitivities)
    On Error GoTo old_frmSensitivitiesErr
    Set mvarold_frmSensitivities = vData
    Exit Property
old_frmSensitivitiesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: old_frmSensitivities Property Set")
End Property

Public Property Get frmRuleLength() As frmRuleLength
    On Error GoTo frmRuleLengthGetErr
    Set frmRuleLength = mvarfrmRuleLength
    Exit Property
frmRuleLengthGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmRuleLength Property Get")
End Property

Public Property Set frmRuleLength(vData As frmRuleLength)
    On Error GoTo frmRuleLengthSetErr
    Set mvarfrmRuleLength = vData
    Exit Property
frmRuleLengthSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmRuleLength Property Set")
End Property

Public Property Get frmGeometricProperties() As frmGeometricProperties
    On Error GoTo frmGeometricPropertiesGetErr
    Set frmGeometricProperties = mvarfrmGeometricProperties
    Exit Property
frmGeometricPropertiesGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmGeometricProperties Property Get")
End Property

Public Property Set frmGeometricProperties(vData As frmGeometricProperties)
    On Error GoTo frmGeometricPropertiesSetErr
    Set mvarfrmGeometricProperties = vData
    Exit Property
frmGeometricPropertiesSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmGeometricProperties Property Set")
End Property

Public Property Get frmWrapPressures() As frmWrapPressures
    On Error GoTo frmWrapPressuresGetErr
    Set frmWrapPressures = mvarfrmWrapPressures
    Exit Property
frmWrapPressuresGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmWrapPressures Property Get")
End Property

Public Property Set frmWrapPressures(vData As frmWrapPressures)
    On Error GoTo frmWrapPressuresSetErr
    Set mvarfrmWrapPressures = vData
    Exit Property
frmWrapPressuresSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmWrapPressures Property Set")
End Property

Public Property Get frmOpenGL() As frmOpenGL
    On Error GoTo frmOpenGLGetErr
    Set frmOpenGL = mvarfrmOpenGL
    Exit Property
frmOpenGLGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmOpenGL Property Get")
End Property

Public Property Set frmOpenGL(vData As frmOpenGL)
    On Error GoTo frmOpenGLSetErr
    Set mvarfrmOpenGL = vData
    Exit Property
frmOpenGLSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmOpenGL Property Set")
End Property

Public Property Get frmChartObjectiveFunction() As frmChartObjectiveFunction
    On Error GoTo frmChartObjectiveFunctionGetErr
    Set frmChartObjectiveFunction = mvarfrmChartObjectiveFunction
    Exit Property
frmChartObjectiveFunctionGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmChartObjectiveFunction Property Get")
End Property

Public Property Set frmChartObjectiveFunction(vData As frmChartObjectiveFunction)
    On Error GoTo frmChartObjectiveFunctionSetErr
    Set mvarfrmChartObjectiveFunction = vData
    Exit Property
frmChartObjectiveFunctionSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmChartObjectiveFunction Property Set")
End Property

Public Property Get frmChartGlobalRestrictions() As frmChartGlobalRestrictions
    On Error GoTo frmChartGlobalRestrictionsGetErr
    Set frmChartGlobalRestrictions = mvarfrmChartGlobalRestrictions
    Exit Property
frmChartGlobalRestrictionsGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmChartGlobalRestrictions Property Get")
End Property

Public Property Set frmChartGlobalRestrictions(vData As frmChartGlobalRestrictions)
    On Error GoTo frmChartGlobalRestrictionsSetErr
    Set mvarfrmChartGlobalRestrictions = vData
    Exit Property
frmChartGlobalRestrictionsSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmChartGlobalRestrictions Property Set")
End Property

Public Property Get frmChartDesignVariables() As frmChartDesignVariables
    On Error GoTo frmChartDesignVariablesErr
    Set frmChartDesignVariables = mvarfrmChartDesignVariables
    Exit Property
frmChartDesignVariablesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmChartDesignVariables Property Get")
End Property

Public Property Set frmChartDesignVariables(vData As frmChartDesignVariables)
    On Error GoTo frmChartDesignVariablesErr
    Set mvarfrmChartDesignVariables = vData
    Exit Property
frmChartDesignVariablesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmChartDesignVariables Property Set")
End Property

Public Property Get frmModellingAssistant() As frmModellingAssistant
    On Error GoTo frmModellingAssistantErr
    Set frmModellingAssistant = mvarfrmModellingAssistant
    Exit Property
frmModellingAssistantErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmModellingAssistant Property Get")
End Property

Public Property Set frmModellingAssistant(vData As frmModellingAssistant)
    On Error GoTo frmModellingAssistantErr
    Set mvarfrmModellingAssistant = vData
    Exit Property
frmModellingAssistantErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmModellingAssistant Property Set")
End Property
' Methods
Public Function ReadLBR5txtFile(ts As TextStream)
    On Error GoTo ReadLBR5txtFileErr
    Dim sLine As String
    Dim i As Integer
'    sLine =ReadLn(ts)
'    sLine = LTrim(sLine)
'    sLine = RTrim(sLine)
'    If sLine <> VersionNumber Then
'        MsgBox "Bad file version!", vbCritical + vbOKOnly
'        GoTo ReadLBR5txtFileErr
'    End If
    Me.cHeader.ReadGeneralDataLBR5txtFile ts
    For i = 1 To Me.cHeader.NETO
        Dim m As New cPanel
        Me.colPanel.Add m, i
        Me.colPanel.Item(i).pNumber = i
        Me.colPanel.Item(i).index = i
        Me.colPanel.Item(i).ReadLBR5txtFile ts
        'Me.colPanel.Item(i).cCostCAtMain.SetFirstPanelData m
        Set m = Nothing
    Next i
    Me.cHeader.ReadGlobalDataLBR5txtFile ts
    Call NodeGenerator
    UpdateBoundary Me.index
    Dim cPanel As cPanel
    For Each cPanel In colPanel
        cPanel.cCostCAtMain.SetFirstPanelData cPanel.pNumber, Me.index
    Next cPanel
    ComputeFirstFractionnements Me.index
    Exit Function
ReadLBR5txtFileErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: Function ReadLBR5txtFile")
End Function

Public Function ReadMARS_LBR5TransfertFile(sFile As String)
    On Error GoTo ReadMARS_LBR5TransfertFileErr
    Dim sLine As String
    Dim v() As Variant
    Dim i As Integer
    
    Dim fso As New FileSystemObject, fil As file, ts As TextStream
    Set fil = fso.GetFile(sFile)
    Set ts = fil.OpenAsTextStream(ForReading)

    
    
'    OpenMarsFile ts, Me.index
'    ts.Close
    
'    Project.Item(lProjectCount).frmProject.Tag = lProjectCount
'    Project.Item(lProjectCount).frmProject.Caption = GetFileName(sFile)
'    Project.Item(lProjectCount).sFileName = sFile
'    ZoomFull
'    setFunctionMode NO_FUNCTION
'    setScreenMode NORMAL_MODE
    
    
    Exit Function
    
    Me.cHeader.ReadMARS_LBR5TransfertFile ts
NextLine0:
    'Compartments (general)
    sLine = ReadLn(ts)
    Select Case Left(UCase(sLine), 14)
        Case "<COMPARTMENTS>"
            sLine = ReadLn(ts)
            Dim NoOfCompartments As Integer
            GetValues 1, sLine, v
            NoOfCompartments = Val_(v(1))
        Case Else
            GoTo NextLine0
    End Select
    For i = 1 To NoOfCompartments
        Dim cCompartment As New cCompartment
        Me.colCompartments.Add cCompartment, i
        Me.colCompartments.Item(i).ReadMARS_LBR5TransfertFileGeneral ts
        Set cCompartment = Nothing
    Next i
NextLine1:
    'Compartments (dimensions)
    sLine = ReadLn(ts)
    Select Case Left(UCase(sLine), 24)
        Case "<COMPARTMENT DIMENSIONS>"
        Case Else
            GoTo NextLine1
    End Select
    For i = 1 To NoOfCompartments
        Me.colCompartments.Item(i).ReadMARS_LBR5TransfertFileDimensions ts
    Next i
    ' Nodes
NextLine2:
    sLine = ReadLn(ts)
    Select Case Left(UCase(sLine), 7)
        Case "<NODES>"
            sLine = ReadLn(ts)
            GetValues 1, sLine, v
            Dim NoOfNodes As Integer
            NoOfNodes = Val_(v(1))
        Case Else
            GoTo NextLine2
    End Select
    For i = 1 To NoOfNodes
        Dim cNode As New cNode
        Me.colNodes.Add cNode, i
        Me.colNodes.Item(i).ReadMARS_LBR5TransfertFile ts
        Set cNode = Nothing
    Next i
    'Materials
NextLine3:
    sLine = ReadLn(ts)
    Select Case Left(UCase(sLine), 11)
        Case "<MATERIALS>"
            sLine = ReadLn(ts)
            GetValues 1, sLine, v
            Dim NoOfMaterials As Integer
            NoOfMaterials = Val_(v(1))
        Case Else
            GoTo NextLine3
    End Select
    For i = 1 To NoOfMaterials
        Dim cMaterial As New cMaterial
        Me.colMaterials.Add cMaterial, i
        Me.colMaterials.Item(i).ReadMARS_LBR5TransfertFile ts
        Set cMaterial = Nothing
    Next i
    'Panels
NextLine4:
    sLine = ReadLn(ts)
    Select Case Left(UCase(sLine), 15)
        Case "<STIFFENPANELS>"
            sLine = ReadLn(ts)
            GetValues 1, sLine, v
            Me.cHeader.NETO = Val_(v(1))
        Case Else
            GoTo NextLine4
    End Select
    'GetMinMaxCoordinates Y, Z
    For i = 1 To Me.cHeader.NETO
        Dim cPanel As New cPanel
        cPanel.index = i
        cPanel.pNumber = i
        cPanel.pType = Plate
        'cPanel.cCostCAtMain.SetFirstPanelData cPanel
        Me.colPanel.Add cPanel, i
        Me.colPanel.Item(i).ReadMARS_LBR5TransfertFile ts
        'CalculateMarsSymmetryPanels cPanel
        Set cPanel = Nothing
    Next i
    ' Calculate Ymin
    For i = 1 To Me.colNodes.Count
        GetMinMaxCoordinates Me.colNodes.Item(i).y
    Next i
    ' Calculate equivalent panels (superposed to the Symmetry axis)
    For Each cPanel In Me.colPanel
        If Me.colNodes.Item(cPanel.cGeometry.InNode).y = Me.Ymin And Me.colNodes.Item(cPanel.cGeometry.OutNode).y = Me.Ymin Then
            cPanel.cScantlings.GrossThickness = cPanel.cScantlings.GrossThickness / 2
            cPanel.cScantlings.NetThickness = cPanel.cScantlings.NetThickness / 2
            cPanel.cScantlings.CorrosionThickness = cPanel.cScantlings.CorrosionThickness / 2
            cPanel.cScantlings.cSecondaryFrames.WebHeight = cPanel.cScantlings.cSecondaryFrames.WebHeight / 2
            cPanel.cScantlings.cSecondaryFrames.FlangeThickness = cPanel.cScantlings.cSecondaryFrames.FlangeThickness / 2
            cPanel.cScantlings.cSecondaryFrames.CorrosionThickness = cPanel.cScantlings.cSecondaryFrames.CorrosionThickness / 2
            cPanel.cScantlings.cPrimaryStiffeners.WebHeight = cPanel.cScantlings.cPrimaryStiffeners.WebHeight / 2
            cPanel.cScantlings.cPrimaryStiffeners.FlangeThickness = cPanel.cScantlings.cPrimaryStiffeners.FlangeThickness / 2
            cPanel.cScantlings.cPrimaryStiffeners.CorrosionThickness = cPanel.cScantlings.cPrimaryStiffeners.CorrosionThickness / 2
        End If
    Next cPanel
    'Generate boundary conditions in the Symmetry axis
    UpdateBoundary Me.index
    For Each cPanel In Me.colPanel
        If cPanel.colBoundaryConditions.Count = 1 Then
            If Me.colNodes.Item(cPanel.cGeometry.InNode).y = Me.Ymin Or Me.colNodes.Item(cPanel.cGeometry.OutNode).y = Me.Ymin Then
                cPanel.colBoundaryConditions.Item(cPanel.colBoundaryConditions.Count).BoundaryCondition = SymmetryAxis1
            End If
        End If
    Next cPanel
    
    UpdatePanelConnections Me.index
    For Each cPanel In Me.colPanel
        cPanel.cCostCAtMain.SetFirstPanelData cPanel.pNumber, Me.index
    Next cPanel
    ComputeFirstFractionnements Me.index
    Exit Function
ReadMARS_LBR5TransfertFileErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: Function ReadMARS_LBR5TransfertFile")
End Function

Public Function WriteLBR5txtFile(ByVal ts As TextStream)
    On Error GoTo WriteLBR5txtFileErr
    ts.WriteLine VersionNumber
    Me.cHeader.WriteGeneralDataLBR5txtFile ts
    Dim Panel As cPanel
    For Each Panel In Me.colPanel
        Panel.WriteLBR5txtFile ts
    Next Panel
    Me.cHeader.WriteGlobalDataLBR5txtFile ts
    Exit Function
WriteLBR5txtFileErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: Function WriteLBR5txtFile")
End Function

Public Sub GetMinMaxCoordinates(Optional y As Double, Optional z As Double)
    If y < Me.Ymin Then Me.Ymin = y
    If z < Me.ZMin Then Me.ZMin = z
    If y > Me.Ymax Then Me.Ymax = y
    If z > Me.ZMax Then Me.ZMax = z
End Sub

Public Function IsSymmetryAxis() As Boolean
    ' to complete with other symmetry conditions
    Dim cPanel As cPanel, cBoundary As cBoundaryConditions
    IsSymmetryAxis = False
    For Each cPanel In Me.colPanel
        For Each cBoundary In cPanel.colBoundaryConditions
            If cBoundary.BoundaryCondition = SymmetryAxis1 Then
                IsSymmetryAxis = True
                Exit Function
            End If
        Next cBoundary
    Next cPanel
End Function

Public Function GetModelHeight() As Double
    GetModelHeight = ZMax - ZMin
End Function

Public Function GetModelWidth() As Double
    GetModelWidth = Ymax - Ymin
End Function

Public Sub ReadDefaults(ByVal index As Integer)
   
    'Set me.cHeader = Project.Item(index).cme.cHeader
    Dim i As Integer
    ' ===============================
    ' PROJECT TITLE & STRUCTURE WIDTH
    ' ===============================
    'me.cHeader.Ver = "verLBR5.7(24/03/34)"
'    If Licensing.LicenseTag = "2 - Professional" Then
'        Me.cHeader.IANA = 1
'    Else
'        Me.cHeader.IANA = 2
'    End If
    If Licensing.IS_LBR4 = False And Licensing.IS_BEAM_THEORY = True Then
        Me.cHeader.IANA = 2
    End If
    If Licensing.IS_LBR4 = True And Licensing.IS_BEAM_THEORY = False Then
        Me.cHeader.IANA = 1
    End If
    If Licensing.IS_LBR4 = True And Licensing.IS_BEAM_THEORY = True Then
        Me.cHeader.IANA = 2
    End If
    
    Project.Item(index).frmProject.NegociateIANAmenus
    Project.Item(index).frmProject.NegociateLICmenus
    
    Me.cHeader.cMultiOpti.RHO = 1
    Me.cHeader.cMultiOpti.W1 = 0.333
    Me.cHeader.cMultiOpti.W2 = 0.333
    Me.cHeader.cMultiOpti.W3 = 0.333
    
    Me.cHeader.Title = "New Project"
    Me.cHeader.Width = 20
    ' Sections to analyse (by default)
    Me.cHeader.DIS1 = 0
    Me.cHeader.DIS2 = Me.cHeader.Width / 8
    Me.cHeader.DIS3 = Me.cHeader.Width / 4
    Me.cHeader.DIS4 = 3 / 8 * Me.cHeader.Width
    Me.cHeader.DIS5 = Me.cHeader.Width / 2
    ' Specific weight (1 by default)
    Me.cHeader.IPOIDS = 1
    ' OUPUT FILE
    Me.cHeader.IMPR = 0
    Me.cHeader.IMPR2 = -2
    Me.cHeader.INDAIG = 1
    Me.cHeader.INDRAID = 1
    Me.cHeader.DESSIN = 1
    Me.cHeader.JLPH = 5
    Me.cHeader.JLBORD = 0                        ' Must change JLBORD with IBUSC (everywhere)
    ' OVERALL OPTIMISATION PARAMETERS
    Me.cHeader.IOPTI = 0
    Me.cHeader.ITERAM = 10
    ' COST OBJECTIVE FUNCTION
    Me.cHeader.ICOUT = 0
    Me.cHeader.cCostData.REND = 1
    Me.cHeader.cCostData.EQP = 0.105
    Me.cHeader.cCostData.E0 = 0.008
    Me.cHeader.cCostData.E0X = 0.008
    Me.cHeader.cCostData.E0Y = 0.008
    Me.cHeader.cCostData.C1 = 0.357
    Me.cHeader.cCostData.C2 = 0.457
    Me.cHeader.cCostData.C3 = 0.429
    Me.cHeader.cCostData.DC1 = 0
    Me.cHeader.cCostData.DW2 = 0
    Me.cHeader.cCostData.DW3 = 0
    Me.cHeader.cCostData.P10 = 0.092
    Me.cHeader.cCostData.DP10 = 0.04
    Me.cHeader.cCostData.p4 = 0.5
    Me.cHeader.cCostData.P5 = 1.34
    Me.cHeader.cCostData.DP4 = 0.02
    Me.cHeader.cCostData.DP5 = 0.02
    Me.cHeader.cCostData.P9X = 0
    Me.cHeader.cCostData.P9Y = 0
    Me.cHeader.cCostData.DP9X = 0
    Me.cHeader.cCostData.DP9Y = 0
    Me.cHeader.cCostData.P6 = 0.25
    Me.cHeader.cCostData.P7 = 1.22
    Me.cHeader.cCostData.BETA_X = 1
    Me.cHeader.cCostData.BETA_Y = 1
    Me.cHeader.cCostData.C8 = 2.143
    Me.cHeader.cCostData.DC8 = 0.05
    Me.cHeader.cCostData.ALPHA_X = 1
    Me.cHeader.cCostData.ALPHA_Y = 0
    Me.cHeader.YRED = 1
    
    'Default design variables
    ReadDefaultDesignVariables
    
    'Default CostCAt Global Data
    Me.cHeader.cCostCAtMain.SetDefaultGlobalData
End Sub

Private Sub ReadDefaultDesignVariables()
    On Error GoTo ReadDefaultDesignVariablesErr
    Dim oDes As cDesignVariables
    Dim sFile As String, sLine As String, v() As Variant
    Dim fso As New FileSystemObject, fil As file, ts As TextStream
    Dim i As Integer
    sFile = App.Path & "\desvar.dat"
    Set fil = fso.GetFile(sFile)
    Set ts = fil.OpenAsTextStream(ForReading)
    For i = 1 To 2
        If ts.AtEndOfStream = False Then
            ts.SkipLine
        End If
    Next i
    For i = 1 To 9
        If ts.AtEndOfStream = False Then
            Set oDes = New cDesignVariables
            oDes.index = i
            oDes.VariableName = i
            oDes.Active = False
            sLine = ReadLn(ts)
            GetValues 2, sLine, v
            oDes.LowerLimit = Val_(v(1))
            oDes.UpperLimit = Val_(v(2))
            Me.cHeader.colDefaultDesignVariables.Add oDes, i
            Set oDes = Nothing
        End If
    Next i
    ts.Close
    Exit Sub
ReadDefaultDesignVariablesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: Function ReadDefaultDesignVariables")
End Sub

Private Sub Class_Initialize()
    #If DebugMode Then
        mlClassDebugID = GetNextClassDebugID()
        Debug.Print "'" & TypeName(Me) & "' instance " & mlClassDebugID & " created"
    #End If
    Set mvarcHeader = New cHeader
    Set mvarcDisplaySettings = New cDisplaySettings
    Set mvarcSolution = New cSolution
    Set mvarCoordOrigin = New cNode
    Set mvarfrmProject = New frmProject
    Set mvarfrmTitle = New frmTitle
    Set mvarfrmOverallSpan = New frmOverallSpan
    Set mvarfrmOptimization = New frmOptimization
    Set mvarfrmDeadweight = New frmDeadweight
    Set mvarfrmFourierSeries = New frmFourierSeries
    Set mvarfrmOutput = New frmOutput
    Set mvarfrmLoadCases = New frmLoadCases
'    Set mvarfrmPressuresUniformelyDistributed = New frmPressuresUniformelyDistributed
'    Set mvarfrmOptions = New frmOptions
    Set mvarfrmCostSimplified = New frmCostSimplified
    Set mvarfrmMaterialCosts = New frmMaterialCosts
    Set mvarfrmCostCAt = New frmCostCAt
    Set mvarfrmCostCAtWorkshop = New frmCostCAtWorkshop
    Set mvarfrmCostCAtTables = New frmCostCAtTables
    Set mvarfrmBoundaryConditions = New frmBoundaryConditions
    Set mvarfrmSolution = New frmSolution
    Set mvarfrmNodes = New frmNodes
    Set mvarfrmPlateScantlings = New frmPlateScantlings
    Set mvarfrmDoubleHullScantlings = New frmDoubleHullScantlings
    Set mvarfrmBeamScantlings = New frmBeamScantlings
    'Set mvarfrmBeamScantlings = New frmBeamScantlings
    Set mvarfrmLoads = New frmLoads
    Set mvarfrmRestrictions = New frmRestrictions
    Set mvarfrmDisplayOptions = New frmDisplayOptions
    Set mvarfrmMoveOrigin = New frmMoveOrigin
    Set mvarfrmOptiGlobal = New frmOptiGlobal
    Set mvarfrmSortPanels = New frmSortPanels
    Set mvarfrmHull_Girder = New frmHull_Girder
    Set mvarfrmDeletePanel = New frmDeletePanel
    Set mvarfrmReversePanel = New frmReversePanel
    Set mvarfrmRuleLength = New frmRuleLength
    Set mvarfrmGeometricProperties = New frmGeometricProperties
    Set mvarfrmWrapPressures = New frmWrapPressures
    Set mvarfrmOpenGL = New frmOpenGL
    Set mvarfrmChartObjectiveFunction = New frmChartObjectiveFunction
    Set mvarfrmChartGlobalRestrictions = New frmChartGlobalRestrictions
    Set mvarfrmChartDesignVariables = New frmChartDesignVariables
    Set mvarfrmModellingAssistant = New frmModellingAssistant
    'New Optimization Modules
    Set mvarfrmDesignVariables = New frmDesignVariables
    'Old Optimization Modules
    Set mvarold_frmOpti1 = New old_FrmOpti1
    Set mvarold_frmOpti2 = New old_frmOpti2
    Set mvarold_frmopti3_1 = New old_FrmOpti3_1
    Set mvarold_frmEqConstr1 = New old_frmEqConstr1
    Set mvarold_frmGlobal = New old_frmGlobal
    Set mvarold_frmGraphGravityVar = New old_frmGraphGravityVar
    Set mvarold_frmGrphObjFct = New old_frmGrphObjFct
    Set mvarold_frmSensitivities = New old_frmSensitivities
End Sub

Public Function CloseForms(ByVal index As Integer)
    Unload frmTitle
    Unload frmOverallSpan
    Unload frmOptimization
    Unload frmDeadweight
    Unload frmFourierSeries
    Unload frmOutput
    Unload frmLoadCases
    Unload frmMarsLoads
    Unload frmCostSimplified
    Unload frmMaterialCosts
    Unload frmCostCAt
    Unload frmCostCAtWorkshop
    Unload frmCostCAtTables
    Unload frmBoundaryConditions
    Unload frmSolution
    Unload frmNodes
    Unload frmPlateScantlings
    Unload frmDoubleHullScantlings
    Unload frmBeamScantlings
    Unload frmLoads
    Unload frmRestrictions
    Unload frmMoveOrigin
    Unload frmOptiGlobal
    Unload frmSortPanels
    Unload frmMultiSolve
    Unload frmDesignVariables
    Unload old_FrmOpti1
    Unload old_frmOpti2
    Unload old_FrmOpti3_1
    Unload old_frmEqConstr1
    Unload old_frmGlobal
    Unload frmDeletePanel
    Unload frmReversePanel
    Unload frmRuleLength
    Unload frmGeometricProperties
    Unload frmWrapPressures
    Unload frmOpenGL
    If index = 1 Then 'Close only with project, can coexist with other forms
        Unload frmDisplayOptions
        Unload old_frmGraphGravityVar
        Unload old_frmGrphObjFct
        Unload old_frmSensitivities
        Unload frmHull_Girder
        Unload frmChartObjectiveFunction
        Unload frmModellingAssistant
    End If
End Function

Public Function SaveAsLBR5TextFile(ByVal sFile As String)
    On Error GoTo SaveAsErr
    Dim fso As New FileSystemObject
    Dim fil As file, ts As TextStream
    If fso.FileExists(sFile) = False Then
        fso.CreateTextFile (sFile)
    End If
    Set fil = fso.GetFile(sFile)
    Set ts = fil.OpenAsTextStream(ForWriting)
    Me.WriteLBR5txtFile ts
    MsgBox "File '" & sFile & "'" & " succesfully saved.", vbInformation + vbOKOnly
    If Me.sFileName <> sFile Then
        Me.sFileName = sFile
        Me.cSolution.IsSolution = False
    End If
    Me.frmProject.Caption = GetFileName(sFile)
    ts.Close
    
    If fso.FileExists(GetFilePath(sFile) & "\boss.txt") = True Then
        Set fil = fso.GetFile(GetFilePath(sFile) & "\boss.txt")
        Set ts = fil.OpenAsTextStream(ForWriting)
        ts.WriteLine GetFileName(sFile)
        ts.WriteLine GetFileLock(sFile)
    End If
    
    SaveConfigurationFile
    Set fil = Nothing
    Set ts = Nothing
    Me.DataChanged = False
    ChDrive Left(GetFilePath(Me.sFileName), 3)
    ChDir GetFilePath(Me.sFileName)
    Exit Function
SaveAsErr:
        Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: Function SaveAsLBR5TextFile")
End Function

Public Function SaveAsLBR5ASCIIFile(ByVal sFile As String, Optional bNotReturnMessage As Boolean)
    On Error GoTo SaveAsLBR5ASCIIFileErr
    
    sFile = GetFilePath(sFile) & GetFileRoot(sFile) & ".lbr"
    Dim iNoOfFile As Integer
    iNoOfFile = FreeFile
    Dim file As New FileSystemObject
    If file.FileExists(sFile) Then file.DeleteFile (sFile)
    Set file = Nothing
    Open sFile For Binary Access Write As #iNoOfFile
        Me.WriteASCII iNoOfFile
    Close #iNoOfFile
    If bNotReturnMessage = False Then
        MsgBox "File '" & sFile & "'" & " succesfully saved.", vbInformation + vbOKOnly
    End If
    If Me.sFileName <> sFile Then
        Me.sFileName = sFile
        Me.cSolution.IsSolution = False
    End If
    Me.frmProject.Caption = GetFileRoot(sFile) & ".lbr"
    SaveConfigurationFile
    Me.DataChanged = False
    ChDrive Left(GetFilePath(Me.sFileName), 3)
    ChDir GetFilePath(Me.sFileName)
    Exit Function
SaveAsLBR5ASCIIFileErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: Function SaveAsLBR5ASCIIFile")
End Function

Public Sub SolveProject()
    On Error Resume Next
    'On Error GoTo  SolveProjectErr
    Dim fso As New FileSystemObject
    Dim s As String, s1 As String, s2 As String
    s = GetFilePath(Me.sFileName)
    s1 = App.Path & "\LBR5.exe"
    DeleteOldSolution
    If s & "LBR5.exe" <> s1 Then
        FileCopy App.Path & "\LBR5.exe", s & "LBR5.exe"
    End If
    If fso.FolderExists(s & "FichDessin") = False Then
        fso.CreateFolder s & "FichDessin"
    End If
'    If fso.FileExists(s & "ATableur.txt") = False Then
'        If s & "LBR5.exe" <> s1 Then
'            FileCopy App.Path & "\Atableur.txt", s & "ATableur.txt"
'        End If
'    End If
    If fso.FileExists(s & "boss.txt") = False Then
        fso.CreateTextFile (s & "boss.txt")
    End If
    Dim ts As TextStream
    Set ts = fso.OpenTextFile(s & "boss.txt", ForWriting)
    ts.WriteLine GetFileRoot(Me.sFileName) & ".txt"
    ts.WriteLine GetFileLock(GetFileRoot(Me.sFileName) & ".txt")
    ts.Close
    Dim sMsg As String
    Dim a As Boolean, b As Boolean, c As Boolean, d As Boolean
    sMsg = ""
    If Me.cHeader.cGlobalConstraints.IsCost = yes And Me.cHeader.cGlobalConstraints.CostType = MinimalCostDetailed And Me.cHeader.IOPTI = yes Then
        GoTo ConstrCostCat
    End If
    If Me.cHeader.ICOUT = 2 And Me.cHeader.IOPTI = yes Then
ConstrCostCat:
        'WriteCostCAtFiles Me.index
        If fso.FileExists(s & "dbbase.txt") = False Then
            'sMsg = sMsg + Chr(13) + "dbbase.txt"
            s2 = App.Path & "\dbbase.txt"
            If s & "dbbase.txt" <> s2 Then
                FileCopy App.Path & "\dbbase.txt", s & "dbbase.txt"
            End If
        End If
        
        If fso.FileExists(s & "dbinput.txt") = True Then
            sMsg = sMsg + Chr(13) + "dbinput.txt"
            a = True
        End If

        If fso.FileExists(s & "dbacces.txt") = True Then
            sMsg = sMsg + Chr(13) + "dbacces.txt"
            b = True
        End If
        
        If fso.FileExists(s & "dbfractionnement.txt") = True Then
            sMsg = sMsg + Chr(13) + "dbfractionnement.txt"
            c = True
        End If
        
        If fso.FileExists(s & "dbsoudures.txt") = True Then
            sMsg = sMsg + Chr(13) + "dbsoudures.txt"
            d = True
        End If
        Dim msg_res As VbMsgBoxResult
        If Len(sMsg) > 0 Then
            'MsgBox "The following files are missing:" & Chr(13) & sMsg
            msg_res = MsgBox("Overwrite the following files?" & Chr(13) & sMsg, vbYesNoCancel)
            Select Case msg_res
                Case vbYes
                    WriteCostCAtFiles Me.index, 0, 0, 0, 0 'generate all files
                Case vbNo
                    WriteCostCAtFiles Me.index, a, b, c, d 'if a = false => generate file; etc
                Case vbCancel
                    Exit Sub
            End Select
        Else
            WriteCostCAtFiles Me.index, 0, 0, 0, 0 'generate all files
        End If
    End If
    Me.frmProject.StatusBar.Panels(1).Text = "Computing..."
    Me.frmProject.MouseIcon = LoadResPicture("ID_CURSOR_WAIT", 2)
    ExecCmd s & "LBR5.exe"
    If fso.FileExists(s & "LBR5.exe") = True Then
        If s & "LBR5.exe" <> s1 Then
            fso.DeleteFile s & "LBR5.exe"
        End If
    End If
    If fso.FileExists(s & "dbbase.txt") = True Then
        If s & "dbbase.txt" <> s2 Then
            fso.DeleteFile s & "dbbase.txt"
        End If
    End If
    If fso.FolderExists(s & "FichDessin") = True Then
        fso.DeleteFolder s & "FichDessin"
    End If
    
    Me.frmProject.StatusBar.Panels(1).Text = ""
    Me.frmProject.MouseIcon = LoadResPicture("ID_CURSOR_NORMAL", 2)
    'ViewLogFile Me.sFileName
    'Solution Me.index
    Dim i As Integer
'    If Me.cHeader.ITERAM > 0 Then
'        For i = 1 To Me.cHeader.ITERAM
'            Load Me.frmProject.mnuAnalysisOutputScantlings.Item(i)
'            Me.frmProject.mnuAnalysisOutputScantlings.Item(i).Caption = "Iteration " & i
'            Me.frmProject.mnuAnalysisOutputScantlings.Item(i).Enabled = True
'        Next i
'    End If
    fMainForm.SetFocus
    Exit Sub
SolveProjectErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: Sub SolveProject")
End Sub

Private Sub DeleteOldSolution()
    On Error GoTo DeleteOldSolutionErr
    Dim sFile As String, i As Integer
    Dim fso As New FileSystemObject
    Dim s As String
    s = GetFilePath(Project.Item(Me.index).sFileName)
    sFile = Project.Item(Me.index).sFileName
    If fso.FileExists(s & "sol-" & GetFileRoot(sFile) & ".txt") Then fso.DeleteFile (s & "sol-" & GetFileRoot(sFile) & ".txt")
    If fso.FileExists(s & "sol2-" & GetFileRoot(sFile) & ".txt") Then fso.DeleteFile (s & "sol2-" & GetFileRoot(sFile) & ".txt")
    If fso.FileExists(s & "opt-" & GetFileRoot(sFile) & ".txt") Then fso.DeleteFile (s & "opt-" & GetFileRoot(sFile) & ".txt")
    For i = 1 To 26
        If fso.FileExists(s & "Up-" & GetLetterCorrespondingToNumber(i) & "-" & GetFileRoot(sFile) & ".txt") Then
            fso.DeleteFile (s & "Up-" & GetLetterCorrespondingToNumber(i) & "-" & GetFileRoot(sFile) & ".txt")
        End If
    Next i
    Project.Item(Me.index).cSolution.IsSolution = False
    Exit Sub
DeleteOldSolutionErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: Sub DeleteOldSolution")
End Sub

Private Sub ReadUpdatedScantlings()
    On Error GoTo ReadUpdatedScantlingsErr
    Dim sFile As String, s As String, i As Integer
    Dim fso As FileSystemObject, fil As file, ts As TextStream
    Dim cPanel As cPanel, cScantlings As cScantlings
    s = GetFilePath(Project.Item(Me.index).sFileName)
    sFile = Project.Item(Me.index).sFileName
    For i = 1 To 26
        If fso.FileExists(s & "Up-" & GetLetterCorrespondingToNumber(i) & "-" & GetFileRoot(sFile) & ".txt") Then
            Set fil = fso.GetFile(s & "Up-" & GetLetterCorrespondingToNumber(i) & "-" & GetFileRoot(sFile) & ".txt")
            Set ts = fil.OpenAsTextStream(ForReading)
            For Each cPanel In Me.colPanel
                
            Next cPanel
            ts.Close
            Set fil = Nothing
        End If
    Next i
    Exit Sub
ReadUpdatedScantlingsErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: Sub ReadUpdatedScantlings")
End Sub

Public Function LoadDefaultConfiguration()
    On Error GoTo DefaultConfigurationErr
    With Me.cDisplaySettings
        .DrawPlates = yes
        .DrawBeams = yes
        .DrawFleshes = yes
        .DrawPrimaryFrames = yes
        .DrawSecondaryFrames = yes
        .DrawPrimaryStiffeners = yes
        .DrawSecondaryStiffeners = yes
        .DrawNodes = yes
        .DrawGirders = yes
        .DrawGrid = no
        .DrawPressuresUniformlyDistributed = yes
        .DrawBoundaryConditions = yes
        .DrawLocalizedPressure = no
        .DrawParticipation = no
        'Size
        .SizePlates = 1
        .SizeBeams = 1
        .SizePrimaryFrames = 1
        .SizeSecondaryFrames = 1
        .SizePrimaryStiffeners = 1
        .SizeSecondaryStiffeners = 1
        .SizeNodes = 2
        .SizeGirders = 1
        .SizeGrid = 1
        .SizeFleshes = 2
        .SizePressuresUniformlyDistributed = 0.3
        .SizeLocalizedPressure = 1.7
        'Colors
        .ColorPlates = CLng(vbWhite)
        .ColorBeams = CLng(16744576)
        .ColorFleshes = CLng(vbGreen)
        .ColorPrimaryFrames = CLng(8454143)
        .ColorSecondaryFrames = CLng(12910591)
        .ColorPrimaryStiffeners = 1
        .ColorSecondaryStiffeners = 1 'vbBlack + 1
        .ColorNodes = CLng(vbWhite)
        .ColorGirders = CLng(16711935)
        .ColorGrid = CLng(11513775)
        .ColorScreen = CLng(16730698)
        .ColorPressuresUniformlyDistributed = CLng(vbRed)
        .ColorBoundaryConditions = CLng(16777139)
        .ColorLocalizedPressure = CLng(196)
        'Texts
        .TextPressuresUniformlyDistributed = None
        .TextPrimaryFrames = None
        .TextSecondaryFrames = None
        .TextPlates = Numbers
        .TextBeams = Numbers
        .TextPrimaryStiffeners = None
        .TextSecondaryStiffeners = None
        .TextGirders = None
        .TextNodes = None
        .TextBoundaryConditions = None
        .TextLocalizedPressure = None
        .TextParticipation = None
    End With
    With Me.cSolution
        .IsSolution = False
        .ShowSolution = False
        '.CurrentDiagram = 1
        '.CurrentLoadCase = 0
        '.CurrentSection = 5
        '.DiagramScale = 1
        .ShowNeutralAxis = False
        .ShowLegend = False
        .Color1 = 7667829
        .Color2 = 11403438
        .Color3 = 16318713
        .Color4 = 16711680
        .Color5 = 16744448
        .Color6 = 16776960
        .Color7 = 4227072
        .Color8 = 766017
        .Color9 = 2290278
        .Color10 = 11206570
        .Color11 = 65535
        .Color12 = 304635
        .Color13 = 352762
        .Color14 = 255
        .Color15 = 192
        .Color16 = 128
    End With
    Exit Function
DefaultConfigurationErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: Function DefaultConfiguration")
End Function

Public Function ReadConfigFile()
    On Error GoTo ReadConfigFileErr
    Dim sFile As String
    'sFile = App.Path & "\config.cfg"
    
    sFile = GetCommonAppPath & "\LBR-5.7\config.cfg"
    
    'MsgBox sFile
    Dim fso As New FileSystemObject, fil As file, ts As TextStream
    Set fil = fso.GetFile(sFile)
    Set ts = fil.OpenAsTextStream(ForReading)
    'State
    With Me.cDisplaySettings
        .DrawPlates = CInt(ts.ReadLine)
        .DrawBeams = CInt(ts.ReadLine)
        .DrawFleshes = CInt(ts.ReadLine)
        .DrawPrimaryFrames = CInt(ts.ReadLine)
        .DrawSecondaryFrames = CInt(ts.ReadLine)
        .DrawPrimaryStiffeners = CInt(ts.ReadLine)
        .DrawSecondaryStiffeners = CInt(ts.ReadLine)
        .DrawNodes = CInt(ts.ReadLine)
        .DrawGirders = CInt(ts.ReadLine)
        .DrawGrid = CInt(ts.ReadLine)
        .DrawPressuresUniformlyDistributed = CInt(ts.ReadLine)
        .DrawBoundaryConditions = CInt(ts.ReadLine)
        .DrawLocalizedPressure = CInt(ts.ReadLine)
        .DrawParticipation = CInt(ts.ReadLine)
        'Size
        .SizePlates = CDbl(ts.ReadLine)
        .SizeBeams = CDbl(ts.ReadLine)
        .SizePrimaryFrames = CDbl(ts.ReadLine)
        .SizeSecondaryFrames = CDbl(ts.ReadLine)
        .SizePrimaryStiffeners = CDbl(ts.ReadLine)
        .SizeSecondaryStiffeners = CDbl(ts.ReadLine)
        .SizeNodes = CDbl(ts.ReadLine)
        .SizeGirders = CDbl(ts.ReadLine)
        .SizeGrid = CDbl(ts.ReadLine)
        .SizeFleshes = CDbl(ts.ReadLine)
        .SizePressuresUniformlyDistributed = CDbl(ts.ReadLine)
        .SizeLocalizedPressure = CDbl(ts.ReadLine)
        'Colors
        .ColorPlates = CLng(ts.ReadLine)
        .ColorBeams = CLng(ts.ReadLine)
        .ColorFleshes = CLng(ts.ReadLine)
        .ColorPrimaryFrames = CLng(ts.ReadLine)
        .ColorSecondaryFrames = CLng(ts.ReadLine)
        .ColorPrimaryStiffeners = CLng(ts.ReadLine)
        .ColorSecondaryStiffeners = CLng(ts.ReadLine)
        .ColorNodes = CLng(ts.ReadLine)
        .ColorGirders = CLng(ts.ReadLine)
        .ColorGrid = CLng(ts.ReadLine)
        .ColorScreen = CLng(ts.ReadLine)
        .ColorPressuresUniformlyDistributed = CLng(ts.ReadLine)
        .ColorBoundaryConditions = CLng(ts.ReadLine)
        .ColorLocalizedPressure = CLng(ts.ReadLine)
        'Texts
        .TextPressuresUniformlyDistributed = CInt(ts.ReadLine)
        .TextPrimaryFrames = CInt(ts.ReadLine)
        .TextSecondaryFrames = CInt(ts.ReadLine)
        .TextPlates = CInt(ts.ReadLine)
        .TextBeams = CInt(ts.ReadLine)
        .TextPrimaryStiffeners = CInt(ts.ReadLine)
        .TextSecondaryStiffeners = CInt(ts.ReadLine)
        .TextGirders = CInt(ts.ReadLine)
        .TextNodes = CInt(ts.ReadLine)
        .TextBoundaryConditions = CInt(ts.ReadLine)
        .TextLocalizedPressure = CInt(ts.ReadLine)
        .TextParticipation = CInt(ts.ReadLine)
     End With
    With Me.cSolution
        .IsSolution = False: ts.ReadLine
        .ShowSolution = False: ts.ReadLine
        '.CurrentDiagram = ts.ReadLine
        '.CurrentDiagram = 1
        '.CurrentLoadCase = 0
        '.CurrentSection = ts.ReadLine
        '.CurrentSection = 5
        .DiagramScale = CDbl(ts.ReadLine)
        .ShowNeutralAxis = CBool(ts.ReadLine)
        .ShowLegend = CBool(ts.ReadLine)
        .Color1 = CLng(ts.ReadLine)
        .Color2 = CLng(ts.ReadLine)
        .Color3 = CLng(ts.ReadLine)
        .Color4 = CLng(ts.ReadLine)
        .Color5 = CLng(ts.ReadLine)
        .Color6 = CLng(ts.ReadLine)
        .Color7 = CLng(ts.ReadLine)
        .Color8 = CLng(ts.ReadLine)
        .Color9 = CLng(ts.ReadLine)
        .Color10 = CLng(ts.ReadLine)
        .Color11 = CLng(ts.ReadLine)
        .Color12 = CLng(ts.ReadLine)
        .Color13 = CLng(ts.ReadLine)
        .Color14 = CLng(ts.ReadLine)
        .Color15 = CLng(ts.ReadLine)
        .Color16 = CLng(ts.ReadLine)
    End With
    ts.Close
    Exit Function
ReadConfigFileErr:
    MsgBox "File Config.cfg could not be read. Default settings will be loaded.", vbCritical + vbOKOnly
    LoadDefaultConfiguration
    SaveConfigurationFile
End Function

Public Function SaveConfigurationFile()
    On Error GoTo SaveConfigurationFileErr
    Dim sFile As String
    'sFile = App.Path & "\config.cfg"
    sFile = GetCommonAppPath & "\LBR-5.7\config.cfg"
    Dim fil As FileSystemObject, ts As TextStream
    Set fil = CreateObject("Scripting.FileSystemObject")
    If fil.FolderExists(GetCommonAppPath & "\LBR-5.7") = False Then
        fil.CreateFolder GetCommonAppPath & "\LBR-5.7"
    End If
    Set ts = fil.OpenTextFile(sFile, ForWriting, TristateUseDefault)
    'State
    With Me.cDisplaySettings
        ts.WriteLine .DrawPlates
        ts.WriteLine .DrawBeams
        ts.WriteLine .DrawFleshes
        ts.WriteLine .DrawPrimaryFrames
        ts.WriteLine .DrawSecondaryFrames
        ts.WriteLine .DrawPrimaryStiffeners
        ts.WriteLine .DrawSecondaryStiffeners
        ts.WriteLine .DrawNodes
        ts.WriteLine .DrawGirders
        ts.WriteLine .DrawGrid
        ts.WriteLine .DrawPressuresUniformlyDistributed
        ts.WriteLine .DrawBoundaryConditions
        ts.WriteLine .DrawLocalizedPressure
        ts.WriteLine .DrawParticipation
        
        'Size
        ts.WriteLine .SizePlates
        ts.WriteLine .SizeBeams
        ts.WriteLine .SizePrimaryFrames
        ts.WriteLine .SizeSecondaryFrames
        ts.WriteLine .SizePrimaryStiffeners
        ts.WriteLine .SizeSecondaryStiffeners
        ts.WriteLine .SizeNodes
        ts.WriteLine .SizeGirders
        ts.WriteLine .SizeGrid
        ts.WriteLine .SizeFleshes
        ts.WriteLine .SizePressuresUniformlyDistributed
        ts.WriteLine .SizeLocalizedPressure
        
        'Colors
        ts.WriteLine .ColorPlates
        ts.WriteLine .ColorBeams
        ts.WriteLine .ColorFleshes
        ts.WriteLine .ColorPrimaryFrames
        ts.WriteLine .ColorSecondaryFrames
        ts.WriteLine .ColorPrimaryStiffeners
        ts.WriteLine .ColorSecondaryStiffeners
        ts.WriteLine .ColorNodes
        ts.WriteLine .ColorGirders
        ts.WriteLine .ColorGrid
        ts.WriteLine .ColorScreen
        ts.WriteLine .ColorPressuresUniformlyDistributed
        ts.WriteLine .ColorBoundaryConditions
        ts.WriteLine .ColorLocalizedPressure
        'Texts
        ts.WriteLine .TextPressuresUniformlyDistributed
        ts.WriteLine .TextPrimaryFrames
        ts.WriteLine .TextSecondaryFrames
        ts.WriteLine .TextPlates
        ts.WriteLine .TextBeams
        ts.WriteLine .TextPrimaryStiffeners
        ts.WriteLine .TextSecondaryStiffeners
        ts.WriteLine .TextGirders
        ts.WriteLine .TextNodes
        ts.WriteLine .TextBoundaryConditions
        ts.WriteLine .TextLocalizedPressure
        ts.WriteLine .TextParticipation
    End With
    With Me.cSolution
        ts.WriteLine CInt(.IsSolution)
        ts.WriteLine CInt(.ShowSolution)
        'ts.WriteLine .CurrentDiagram
        'ts.WriteLine .CurrentLoadCase
        'ts.WriteLine .CurrentSection
        ts.WriteLine .DiagramScale
        ts.WriteLine CInt(.ShowNeutralAxis)
        ts.WriteLine CInt(.ShowLegend)
        ts.WriteLine .Color1
        ts.WriteLine .Color2
        ts.WriteLine .Color3
        ts.WriteLine .Color4
        ts.WriteLine .Color5
        ts.WriteLine .Color6
        ts.WriteLine .Color7
        ts.WriteLine .Color8
        ts.WriteLine .Color9
        ts.WriteLine .Color10
        ts.WriteLine .Color11
        ts.WriteLine .Color12
        ts.WriteLine .Color13
        ts.WriteLine .Color14
        ts.WriteLine .Color15
        ts.WriteLine .Color16
    End With
    ts.Close
    Exit Function
SaveConfigurationFileErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: Function SaveConfigurationFile")
End Function

Public Function WriteASCII(ByVal iNoOfFile As Integer)
    On Error GoTo WriteASCIIErr
    Dim ss As String * 100
    ss = VersionNumber
    Put #iNoOfFile, , ss
    With Me
        .cHeader.WriteASCII iNoOfFile
        .colNodes.WriteASCII iNoOfFile
        .colPanel.WriteASCII iNoOfFile
        .colPanelUpdate.WritePanelUpdateASCII iNoOfFile
        .cSolution.WriteASCII iNoOfFile
        .colCompartments.WriteASCII iNoOfFile
        .colCostCAtDHull.WriteASCII iNoOfFile
        .colCostCAtNappe.WriteASCII iNoOfFile
    End With
    Exit Function
WriteASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: Function WriteASCII")
End Function

Public Function ReadASCII(ByVal iNoOfFile As Integer)
    On Error GoTo ReadASCIIErr
    Dim ii As Integer, dd As Double, ss As String * 100
    Get #iNoOfFile, , ss
    With Me
        .cHeader.ReadASCII iNoOfFile
        .colNodes.ReadASCII iNoOfFile
        .colPanel.ReadASCII iNoOfFile
        .cSolution.ReadASCII iNoOfFile
        .colCompartments.ReadASCII iNoOfFile
    End With
    UpdatePanelConnections Me.index
    
    'tmp
    Dim cPanel As cPanel
    For Each cPanel In Me.colPanel
        cPanel.cCostCAtMain.SetFirstPanelData cPanel.pNumber, Me.index
    Next cPanel
    ComputeFirstFractionnements Me.index
    Exit Function
ReadASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: Function ReadASCII")
End Function

Public Function Clone() As cProject
    On Error GoTo CloneErr
    Set Clone = New cProject
    With Clone
        .Ymin = Me.Ymin
        .Ymax = Me.Ymax
        .ZMin = Me.ZMin
        .ZMax = Me.ZMax
        .index = Me.index
        .sFileName = Me.sFileName
        Set .colPanel = Me.colPanel.Clone
        Set .colPanelUpdate = Me.colPanelUpdate.Clone
        Set .colNodes = Me.colNodes.Clone
        Set .colVortex = Me.colVortex.Clone
        Set .CoordOrigin = Me.CoordOrigin.Clone
        .LastNode = Me.LastNode
        Set .cHeader = Me.cHeader.Clone
        Set .cDisplaySettings = Me.cDisplaySettings.Clone
        Set .cSolution = Me.cSolution.Clone
        .ScreenMode = Me.ScreenMode
        .FunctionMode = Me.FunctionMode
        .EconomicDraw = Me.EconomicDraw
        .DataChanged = Me.DataChanged
        Set .colCostCAtDHull = Me.colCostCAtDHull.Clone
        Set .colCostCAtNappe = Me.colCostCAtNappe
        Set .colCompartments = Me.colCompartments.Clone
        Set .colMaterials = Me.colMaterials
        .bNewProjectFirstSave = Me.bNewProjectFirstSave
        .FileVersionNumber = Me.FileVersionNumber
        'Set .ProjectMaps = Me.ProjectMaps.Clone
    End With
    Exit Function
CloneErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: Function Clone")
End Function

Private Sub Class_terminate()
    #If DebugMode Then
        Debug.Print "'" & TypeName(Me) & "' instance " & CStr(mlClassDebugID) & " is terminating"
    #End If
    Set mvarfrmProject = Nothing
    Set mvarcDisplaySettings = Nothing
    Set mvarcSolution = Nothing
    Set mvarCoordOrigin = Nothing
    Set mvarfrmTitle = Nothing
    Set mvarfrmOverallSpan = Nothing
    Set mvarfrmOptimization = Nothing
    Set mvarfrmDeadweight = Nothing
    Set mvarfrmFourierSeries = Nothing
    Set mvarfrmOutput = Nothing
    Set mvarfrmLoadCases = Nothing
'    Set mvarfrmPressuresUniformelyDistributed = Nothing
'    Set mvarfrmOptions = Nothing
    Set mvarfrmCostSimplified = Nothing
    Set mvarfrmMaterialCosts = Nothing
    Set mvarfrmCostCAt = Nothing
    Set mvarfrmCostCAtWorkshop = Nothing
    Set mvarfrmCostCAtTables = Nothing
    Set mvarfrmBoundaryConditions = Nothing
    Set mvarfrmSolution = Nothing
    Set mvarfrmNodes = Nothing
    Set mvarfrmPlateScantlings = Nothing
    Set mvarfrmDoubleHullScantlings = Nothing
    Set mvarfrmBeamScantlings = Nothing
    'Set mvarfrmBeamScantlings = Nothing
    Set mvarfrmLoads = Nothing
    Set mvarfrmRestrictions = Nothing
    Set mvarfrmDisplayOptions = Nothing
    Set mvarfrmMoveOrigin = Nothing
    Set mvarfrmOptiGlobal = Nothing
    Set mvarfrmSortPanels = Nothing
    Set mvarfrmHull_Girder = Nothing
    Set mvarfrmDeletePanel = Nothing
    Set mvarfrmReversePanel = Nothing
    Set mvarfrmRuleLength = Nothing
    Set mvarfrmGeometricProperties = Nothing
    Set mvarfrmWrapPressures = Nothing
    Set mvarfrmOpenGL = Nothing
    Set mvarfrmChartObjectiveFunction = Nothing
    Set mvarfrmChartGlobalRestrictions = Nothing
    Set mvarfrmChartDesignVariables = Nothing
    Set mvarfrmModellingAssistant = Nothing
    'New Optimization Modules
    Set mvarfrmDesignVariables = Nothing
    'Old Optimization Modules
    Set mvarold_frmOpti1 = Nothing
    Set mvarold_frmOpti2 = Nothing
    Set mvarold_frmopti3_1 = Nothing
    Set mvarold_frmEqConstr1 = Nothing
    Set mvarold_frmGlobal = Nothing
    Set mvarold_frmGraphGravityVar = Nothing
    Set mvarold_frmGrphObjFct = Nothing
    Set mvarold_frmSensitivities = Nothing
End Sub

#If DebugMode Then
    Public Property Get ClassDebugID()
        ClassDebugID = mlClassDebugID
    End Property
#End If
