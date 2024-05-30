VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cDisplaySettings"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
#Const DebugMode = 0
#If DebugMode Then
    Private mlClassDebugID As Long
#End If
Public Enum DrawCostCAtItems
    DrawCostCAtNone = 0
    DrawCostCAtPanelTypes = 1
    DrawCostCAtNappes = 2
End Enum
'Draw
Private mvarDrawNodes As YesNo
Private mvarDrawPlates As YesNo
Private mvarDrawBeams As YesNo
Private mvarDrawFleshes As YesNo
Private mvarDrawPrimaryFrames As YesNo
Private mvarDrawSecondaryFrames As YesNo
Private mvarDrawPrimaryStiffeners As YesNo
Private mvarDrawSecondaryStiffeners As YesNo
Private mvarDrawGirders As YesNo
Private mvarDrawCoordinateAxis As YesNo
Private mvarDrawPanelNumbers As YesNo
Private mvarDrawBoundaryConditions As YesNo
Private mvarDrawPressuresUniformlyDistributed As YesNo
Private mvarDrawLocalizedPressure As YesNo
Private mvarDrawPlateThickness As YesNo
Private mvarDrawScantlings As YesNo
Private mvarDrawSolution As YesNo
Private mvarDrawNeutralAxis As YesNo
Private mvarDrawCorrosionThickness As YesNo
Private mvarDrawGrid As YesNo
Private mvarDrawParticipation As YesNo
Private mvarDrawCostCAtItems As DrawCostCAtItems

'Size
Private mvarSizeNodes As Double
Private mvarSizeFleshes As Double
Private mvarSizePressuresUniformlyDistributed As Double
Private mvarSizeLocalizedPressure As Double
Private mvarSizeThickness As Double
Private mvarSizePrimaryFrames As Double
Private mvarSizeSecondaryFrames As Double
Private mvarSizePrimaryStiffeners As Double
Private mvarSizeSecondaryStiffeners As Double
Private mvarSizeGirders As Double
Private mvarSizeSolution As Double
Private mvarSizeGrid As Double
Private mvarSizePlates As Double
Private mvarSizeBeams As Double
Private mvarSolutionLoadCase As Integer
Private mvarShrink As YesNo
Private mvarSizeShrink As Double
Private mvarPrecision As Double
Private mvarSplash As YesNo
Private mvarCurrentLoadCase As Integer
'Colors
Private mvarColorPlates As ColorConstants
Private mvarColorBeams As ColorConstants
Private mvarColorFleshes As ColorConstants
Private mvarColorPrimaryFrames As ColorConstants
Private mvarColorSecondaryFrames As ColorConstants
Private mvarColorPrimaryStiffeners As ColorConstants
Private mvarColorSecondaryStiffeners As ColorConstants
Private mvarColorNodes As ColorConstants
Private mvarColorGirders As ColorConstants
Private mvarColorGrid As ColorConstants
Private mvarColorScreen As ColorConstants
Private mvarColorPressuresUniformlyDistributed As ColorConstants
Private mvarColorBoundaryConditions As ColorConstants
Private mvarColorLocalizedPressure As ColorConstants

'Enums
Public Enum TextOptions
    None = 0
    Numbers = 1
    Lenght = 2
    NetThickness = 3
    GrossThickness = 4
    Corrosion = 5
    Pressures = 6
    WebHeight = 7
    WebThickness = 8
    WebGrossThickness = 9
    FlangeWidth = 10
    FlangeThickness = 11
    FlangeGrossThickness = 12
    Spacing = 13
    All = 14
    Coordinates = 15
    DistFromInNode = 16
    Boundaries = 17
    GrossSectionModulus = 18
    Participation = 19
    Materials = 20
End Enum
'Texts
Private mvarTextPressuresUniformlyDistributed As TextOptions
Private mvarTextPrimaryFrames As TextOptions
Private mvarTextSecondaryFrames As TextOptions
Private mvarTextPlates As TextOptions
Private mvarTextBeams As TextOptions
Private mvarTextPrimaryStiffeners As TextOptions
Private mvarTextSecondaryStiffeners As TextOptions
Private mvarTextGirders As TextOptions
Private mvarTextNodes As TextOptions
Private mvarTextBoundaryConditions As TextOptions
Private mvarTextLocalizedPressure As TextOptions
Private mvarTextShowVariations As YesNo
Private mvarShowLegend As YesNo
Private mvarTextParticipation As TextOptions

Public Property Get DrawNodes() As YesNo
    On Error GoTo DrawNodesGetErr
    DrawNodes = mvarDrawNodes
    Exit Property
DrawNodesGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: DrawNodes Property Get")
End Property

Public Property Let DrawNodes(ByVal vDrawNodes As YesNo)
    On Error GoTo DrawNodesLetErr
    mvarDrawNodes = vDrawNodes
    Exit Property
DrawNodesLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: DrawNodes Property Let")
End Property

Public Property Get DrawPlates() As YesNo
    On Error GoTo DrawPlatesGetErr
    DrawPlates = mvarDrawPlates
    Exit Property
DrawPlatesGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: DrawPlates Property Get")
End Property

Public Property Let DrawPlates(ByVal vDrawPlates As YesNo)
    On Error GoTo DrawPlatesLetErr
    mvarDrawPlates = vDrawPlates
    Exit Property
DrawPlatesLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: DrawPlates Property Let")
End Property

Public Property Get DrawBeams() As YesNo
    On Error GoTo DrawBeamsGetErr
    DrawBeams = mvarDrawBeams
    Exit Property
DrawBeamsGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: DrawBeams Property Get")
End Property

Public Property Let DrawBeams(ByVal vDrawBeams As YesNo)
    On Error GoTo DrawBeamsLetErr
    mvarDrawBeams = vDrawBeams
    Exit Property
DrawBeamsLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: DrawBeams Property Let")
End Property

Public Property Get DrawFleshes() As YesNo
    On Error GoTo DrawFleshesGetErr
    DrawFleshes = mvarDrawFleshes
    Exit Property
DrawFleshesGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: DrawFleshes Property Get")
End Property

Public Property Let DrawFleshes(ByVal vDrawFleshes As YesNo)
    On Error GoTo DrawFleshesLetErr
    mvarDrawFleshes = vDrawFleshes
    Exit Property
DrawFleshesLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: DrawFleshes Property Let")
End Property

Public Property Get DrawPrimaryFrames() As YesNo
    On Error GoTo DrawPrimaryFramesGetErr
    DrawPrimaryFrames = mvarDrawPrimaryFrames
    Exit Property
DrawPrimaryFramesGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: DrawPrimaryFrames Property Get")
End Property

Public Property Let DrawPrimaryFrames(ByVal vDrawPrimaryFrames As YesNo)
    On Error GoTo DrawPrimaryFramesLetErr
    mvarDrawPrimaryFrames = vDrawPrimaryFrames
    Exit Property
DrawPrimaryFramesLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: DrawPrimaryFrames Property Let")
End Property

Public Property Get DrawSecondaryFrames() As YesNo
    On Error GoTo DrawSecondaryFramesGetErr
    DrawSecondaryFrames = mvarDrawSecondaryFrames
    Exit Property
DrawSecondaryFramesGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: DrawSecondaryFrames Property Get")
End Property

Public Property Let DrawSecondaryFrames(ByVal vDrawSecondaryFrames As YesNo)
    On Error GoTo DrawSecondaryFramesLetErr
    mvarDrawSecondaryFrames = vDrawSecondaryFrames
    Exit Property
DrawSecondaryFramesLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: DrawSecondaryFrames Property Let")
End Property

Public Property Get DrawPrimaryStiffeners() As YesNo
    On Error GoTo DrawPrimaryStiffenersGetErr
    DrawPrimaryStiffeners = mvarDrawPrimaryStiffeners
    Exit Property
DrawPrimaryStiffenersGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: DrawPrimaryStiffeners Property Get")
End Property

Public Property Let DrawPrimaryStiffeners(ByVal vDrawPrimaryStiffeners As YesNo)
    On Error GoTo DrawPrimaryStiffenersLetErr
    mvarDrawPrimaryStiffeners = vDrawPrimaryStiffeners
    Exit Property
DrawPrimaryStiffenersLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: DrawPrimaryStiffeners Property Let")
End Property

Public Property Get DrawSecondaryStiffeners() As YesNo
    On Error GoTo DrawSecondaryStiffenersGetErr
    DrawSecondaryStiffeners = mvarDrawSecondaryStiffeners
    Exit Property
DrawSecondaryStiffenersGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: DrawSecondaryStiffeners Property Get")
End Property

Public Property Let DrawSecondaryStiffeners(ByVal vDrawSecondaryStiffeners As YesNo)
    On Error GoTo DrawSecondaryStiffenersLetErr
    mvarDrawSecondaryStiffeners = vDrawSecondaryStiffeners
    Exit Property
DrawSecondaryStiffenersLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: DrawSecondaryStiffeners Property Let")
End Property

Public Property Get DrawGirders() As YesNo
    On Error GoTo DrawGirdersGetErr
    DrawGirders = mvarDrawGirders
    Exit Property
DrawGirdersGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: DrawGirders Property Get")
End Property

Public Property Let DrawGirders(ByVal vDrawGirders As YesNo)
    On Error GoTo DrawGirdersLetErr
    mvarDrawGirders = vDrawGirders
    Exit Property
DrawGirdersLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: DrawGirders Property Let")
End Property

Public Property Get DrawCoordinateAxis() As YesNo
    On Error GoTo DrawCoordinateAxisGetErr
    DrawCoordinateAxis = mvarDrawCoordinateAxis
    Exit Property
DrawCoordinateAxisGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: DrawCoordinateAxis Property Get")
End Property

Public Property Let DrawCoordinateAxis(ByVal vDrawCoordinateAxis As YesNo)
    On Error GoTo DrawCoordinateAxisLetErr
    mvarDrawCoordinateAxis = vDrawCoordinateAxis
    Exit Property
DrawCoordinateAxisLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: DrawCoordinateAxis Property Let")
End Property

Public Property Get DrawPanelNumbers() As YesNo
    On Error GoTo DrawPanelNumbersGetErr
    DrawPanelNumbers = mvarDrawPanelNumbers
    Exit Property
DrawPanelNumbersGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: DrawPanelNumbers Property Get")
End Property

Public Property Let DrawPanelNumbers(ByVal vDrawPanelNumbers As YesNo)
    On Error GoTo DrawPanelNumbersLetErr
    mvarDrawPanelNumbers = vDrawPanelNumbers
    Exit Property
DrawPanelNumbersLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: DrawPanelNumbers Property Let")
End Property

Public Property Get DrawBoundaryConditions() As YesNo
    On Error GoTo DrawBoundaryConditionsGetErr
    DrawBoundaryConditions = mvarDrawBoundaryConditions
    Exit Property
DrawBoundaryConditionsGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: DrawBoundaryConditions Property Get")
End Property

Public Property Let DrawBoundaryConditions(ByVal vDrawBoundaryConditions As YesNo)
    On Error GoTo DrawBoundaryConditionsLetErr
    mvarDrawBoundaryConditions = vDrawBoundaryConditions
    Exit Property
DrawBoundaryConditionsLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: DrawBoundaryConditions Property Let")
End Property

Public Property Get DrawPressuresUniformlyDistributed() As YesNo
    On Error GoTo DrawPressuresUniformlyDistributedGetErr
    DrawPressuresUniformlyDistributed = mvarDrawPressuresUniformlyDistributed
    Exit Property
DrawPressuresUniformlyDistributedGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: DrawPressuresUniformlyDistributed Property Get")
End Property

Public Property Let DrawPressuresUniformlyDistributed(ByVal vDrawPressuresUniformlyDistributed As YesNo)
    On Error GoTo DrawPressuresUniformlyDistributedLetErr
    mvarDrawPressuresUniformlyDistributed = vDrawPressuresUniformlyDistributed
    Exit Property
DrawPressuresUniformlyDistributedLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: DrawPressuresUniformlyDistributed Property Let")
End Property

Public Property Get DrawLocalizedPressure() As YesNo
    On Error GoTo DrawLocalizedPressureGetErr
    DrawLocalizedPressure = mvarDrawLocalizedPressure
    Exit Property
DrawLocalizedPressureGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: DrawLocalizedPressure Property Get")
End Property

Public Property Let DrawLocalizedPressure(ByVal vDrawLocalizedPressure As YesNo)
    On Error GoTo DrawLocalizedPressureLetErr
    mvarDrawLocalizedPressure = vDrawLocalizedPressure
    Exit Property
DrawLocalizedPressureLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: DrawLocalizedPressure Property Let")
End Property

Public Property Get DrawPlateThickness() As YesNo
    On Error GoTo DrawPlateThicknessGetErr
    DrawPlateThickness = mvarDrawPlateThickness
    Exit Property
DrawPlateThicknessGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: DrawPlateThickness Property Get")
End Property

Public Property Let DrawPlateThickness(ByVal vDrawPlateThickness As YesNo)
    On Error GoTo DrawPlateThicknessLetErr
    mvarDrawPlateThickness = vDrawPlateThickness
    Exit Property
DrawPlateThicknessLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: DrawPlateThickness Property Let")
End Property

Public Property Get DrawSolution() As YesNo
    On Error GoTo DrawSolutionGetErr
    DrawSolution = mvarDrawSolution
    Exit Property
DrawSolutionGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: DrawSolution Property Get")
End Property

Public Property Let DrawSolution(ByVal vDrawSolution As YesNo)
    On Error GoTo DrawSolutionLetErr
    mvarDrawSolution = vDrawSolution
    Exit Property
DrawSolutionLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: DrawSolution Property Let")
End Property

Public Property Get DrawNeutralAxis() As YesNo
    On Error GoTo DrawNeutralAxisGetErr
    DrawNeutralAxis = mvarDrawNeutralAxis
    Exit Property
DrawNeutralAxisGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: DrawNeutralAxis Property Get")
End Property

Public Property Let DrawNeutralAxis(ByVal vDrawNeutralAxis As YesNo)
    On Error GoTo DrawNeutralAxisLetErr
    mvarDrawNeutralAxis = vDrawNeutralAxis
    Exit Property
DrawNeutralAxisLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: DrawNeutralAxis Property Let")
End Property

Public Property Get DrawCorrosionThickness() As YesNo
    On Error GoTo DrawCorrosionThicknessGetErr
    DrawCorrosionThickness = mvarDrawCorrosionThickness
    Exit Property
DrawCorrosionThicknessGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: DrawCorrosionThickness Property Get")
End Property

Public Property Let DrawCorrosionThickness(ByVal vDrawCorrosionThickness As YesNo)
    On Error GoTo DrawCorrosionThicknessLetErr
    mvarDrawCorrosionThickness = vDrawCorrosionThickness
    Exit Property
DrawCorrosionThicknessLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: DrawCorrosionThickness Property Let")
End Property

Public Property Get DrawGrid() As YesNo
    On Error GoTo DrawGridGetErr
    DrawGrid = mvarDrawGrid
    Exit Property
DrawGridGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: DrawGrid Property Get")
End Property

Public Property Let DrawGrid(ByVal vDrawGrid As YesNo)
    On Error GoTo DrawGridLetErr
    mvarDrawGrid = vDrawGrid
    Exit Property
DrawGridLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: DrawGrid Property Let")
End Property

Public Property Get DrawParticipation() As YesNo
    On Error GoTo DrawParticipationGetErr
    DrawParticipation = mvarDrawParticipation
    Exit Property
DrawParticipationGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: DrawParticipation Property Get")
End Property

Public Property Let DrawParticipation(ByVal vDrawParticipation As YesNo)
    On Error GoTo DrawParticipationLetErr
    mvarDrawParticipation = vDrawParticipation
    Exit Property
DrawParticipationLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: DrawParticipation Property Let")
End Property

Public Property Get DrawCostCAtItems() As DrawCostCAtItems
    On Error GoTo DrawCostCAtItemsGetErr
    DrawCostCAtItems = mvarDrawCostCAtItems
    Exit Property
DrawCostCAtItemsGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: DrawCostCAtItems Property Get")
End Property

Public Property Let DrawCostCAtItems(ByVal vDrawCostCAtItems As DrawCostCAtItems)
    On Error GoTo DrawCostCAtItemsLetErr
    mvarDrawCostCAtItems = vDrawCostCAtItems
    Exit Property
DrawCostCAtItemsLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: DrawCostCAtItems Property Let")
End Property

Public Property Get SizeNodes() As Double
    On Error GoTo SizeNodesGetErr
    SizeNodes = mvarSizeNodes
    Exit Property
SizeNodesGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: SizeNodes Property Get")
End Property

Public Property Let SizeNodes(ByVal vSizeNodes As Double)
    On Error GoTo SizeNodesLetErr
    mvarSizeNodes = vSizeNodes
    Exit Property
SizeNodesLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: SizeNodes Property Let")
End Property

Public Property Get SizeFleshes() As Double
    On Error GoTo SizeFleshesGetErr
    SizeFleshes = mvarSizeFleshes
    Exit Property
SizeFleshesGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: SizeFleshes Property Get")
End Property

Public Property Let SizeFleshes(ByVal vSizeFleshes As Double)
    On Error GoTo SizeFleshesLetErr
    mvarSizeFleshes = vSizeFleshes
    Exit Property
SizeFleshesLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: SizeFleshes Property Let")
End Property

Public Property Get SizePressuresUniformlyDistributed() As Double
    On Error GoTo SizePressuresUniformlyDistributedGetErr
    SizePressuresUniformlyDistributed = mvarSizePressuresUniformlyDistributed
    Exit Property
SizePressuresUniformlyDistributedGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: SizePressuresUniformlyDistributed Property Get")
End Property

Public Property Let SizePressuresUniformlyDistributed(ByVal vSizePressuresUniformlyDistributed As Double)
    On Error GoTo SizePressuresUniformlyDistributedLetErr
    mvarSizePressuresUniformlyDistributed = vSizePressuresUniformlyDistributed
    Exit Property
SizePressuresUniformlyDistributedLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: SizePressuresUniformlyDistributed Property Let")
End Property

Public Property Get SizeLocalizedPressure() As Double
    On Error GoTo SizeLocalizedPressureGetErr
    SizeLocalizedPressure = mvarSizeLocalizedPressure
    Exit Property
SizeLocalizedPressureGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: SizeLocalizedPressure Property Get")
End Property

Public Property Let SizeLocalizedPressure(ByVal vSizeLocalizedPressure As Double)
    On Error GoTo SizeLocalizedPressureLetErr
    mvarSizeLocalizedPressure = vSizeLocalizedPressure
    Exit Property
SizeLocalizedPressureLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: SizeLocalizedPressure Property Let")
End Property

Public Property Get SizeThickness() As Double
    On Error GoTo SizeThicknessGetErr
    SizeThickness = mvarSizeThickness
    Exit Property
SizeThicknessGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: SizeThickness Property Get")
End Property

Public Property Let SizeThickness(ByVal vSizeThickness As Double)
    On Error GoTo SizeThicknessLetErr
    mvarSizeThickness = vSizeThickness
    Exit Property
SizeThicknessLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: SizeThickness Property Let")
End Property

Public Property Get SolutionLoadCase() As Integer
    On Error GoTo SolutionLoadCaseGetErr
    SolutionLoadCase = mvarSolutionLoadCase
    Exit Property
SolutionLoadCaseGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: SolutionLoadCase Property Get")
End Property

Public Property Let SolutionLoadCase(ByVal vSolutionLoadCase As Integer)
    On Error GoTo SolutionLoadCaseLetErr
    mvarSolutionLoadCase = vSolutionLoadCase
    Exit Property
SolutionLoadCaseLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: SolutionLoadCase Property Let")
End Property

Public Property Get SizePrimaryFrames() As Double
    On Error GoTo SizePrimaryFramesGetErr
    SizePrimaryFrames = mvarSizePrimaryFrames
    Exit Property
SizePrimaryFramesGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: SizePrimaryFrames Property Get")
End Property

Public Property Let SizePrimaryFrames(ByVal vSizePrimaryFrames As Double)
    On Error GoTo SizePrimaryFramesLetErr
    mvarSizePrimaryFrames = vSizePrimaryFrames
    Exit Property
SizePrimaryFramesLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: SizePrimaryFrames Property Let")
End Property

Public Property Get SizeSecondaryFrames() As Double
    On Error GoTo SizeSecondaryFramesGetErr
    SizeSecondaryFrames = mvarSizeSecondaryFrames
    Exit Property
SizeSecondaryFramesGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: SizeSecondaryFrames Property Get")
End Property

Public Property Let SizeSecondaryFrames(ByVal vSizeSecondaryFrames As Double)
    On Error GoTo SizeSecondaryFramesLetErr
    mvarSizeSecondaryFrames = vSizeSecondaryFrames
    Exit Property
SizeSecondaryFramesLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: SizeSecondaryFrames Property Let")
End Property

Public Property Get SizePrimaryStiffeners() As Double
    On Error GoTo SizePrimaryStiffenersGetErr
    SizePrimaryStiffeners = mvarSizePrimaryStiffeners
    Exit Property
SizePrimaryStiffenersGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: SizePrimaryStiffeners Property Get")
End Property

Public Property Let SizePrimaryStiffeners(ByVal vSizePrimaryStiffeners As Double)
    On Error GoTo SizePrimaryStiffenersLetErr
    mvarSizePrimaryStiffeners = vSizePrimaryStiffeners
    Exit Property
SizePrimaryStiffenersLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: SizePrimaryStiffeners Property Let")
End Property

Public Property Get SizeSecondaryStiffeners() As Double
    On Error GoTo SizeSecondaryStiffenersGetErr
    SizeSecondaryStiffeners = mvarSizeSecondaryStiffeners
    Exit Property
SizeSecondaryStiffenersGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: SizeSecondaryStiffeners Property Get")
End Property

Public Property Let SizeSecondaryStiffeners(ByVal vSizeSecondaryStiffeners As Double)
    On Error GoTo SizeSecondaryStiffenersLetErr
    mvarSizeSecondaryStiffeners = vSizeSecondaryStiffeners
    Exit Property
SizeSecondaryStiffenersLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: SizeSecondaryStiffeners Property Let")
End Property

Public Property Get SizeGirders() As Double
    On Error GoTo SizeGirdersGetErr
    SizeGirders = mvarSizeGirders
    Exit Property
SizeGirdersGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: SizeGirders Property Get")
End Property

Public Property Let SizeGirders(ByVal vSizeGirders As Double)
    On Error GoTo SizeGirdersLetErr
    mvarSizeGirders = vSizeGirders
    Exit Property
SizeGirdersLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: SizeGirders Property Let")
End Property

Public Property Get SizeSolution() As Double
    On Error GoTo SizeSolutionGetErr
    SizeSolution = mvarSizeSolution
    Exit Property
SizeSolutionGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: SizeSolution Property Get")
End Property

Public Property Let SizeSolution(ByVal vSizeSolution As Double)
    On Error GoTo SizeSolutionLetErr
    mvarSizeSolution = vSizeSolution
    Exit Property
SizeSolutionLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: SizeSolution Property Let")
End Property

Public Property Get SizePlates() As Double
    On Error GoTo SizePlatesGetErr
    SizePlates = mvarSizePlates
    Exit Property
SizePlatesGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: SizePlates Property Get")
End Property

Public Property Let SizePlates(ByVal vSizePlates As Double)
    On Error GoTo SizePlatesLetErr
    mvarSizePlates = vSizePlates
    Exit Property
SizePlatesLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: SizePlates Property Let")
End Property

Public Property Get SizeBeams() As Double
    On Error GoTo SizeBeamsGetErr
    SizeBeams = mvarSizeBeams
    Exit Property
SizeBeamsGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: SizeBeams Property Get")
End Property

Public Property Let SizeBeams(ByVal vSizeBeams As Double)
    On Error GoTo SizeBeamsLetErr
    mvarSizeBeams = vSizeBeams
    Exit Property
SizeBeamsLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: SizeBeams Property Let")
End Property

Public Property Get SizeGrid() As Double
    On Error GoTo SizeGridGetErr
    SizeGrid = mvarSizeGrid
    Exit Property
SizeGridGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: SizeGrid Property Get")
End Property

Public Property Let SizeGrid(ByVal vSizeGrid As Double)
    On Error GoTo SizeGridLetErr
    mvarSizeGrid = vSizeGrid
    Exit Property
SizeGridLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: SizeGrid Property Let")
End Property

Public Property Get DrawScantlings() As YesNo
    On Error GoTo DrawScantlingsGetErr
    DrawScantlings = mvarDrawScantlings
    Exit Property
DrawScantlingsGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: DrawScantlings Property Get")
End Property

Public Property Let DrawScantlings(ByVal vDrawScantlings As YesNo)
    On Error GoTo DrawScantlingsLetErr
    mvarDrawScantlings = vDrawScantlings
    Exit Property
DrawScantlingsLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: DrawScantlings Property Let")
End Property

Public Property Get Shrink() As YesNo
    On Error GoTo ShrinkGetErr
    Shrink = mvarShrink
    Exit Property
ShrinkGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: Shrink Property Get")
End Property

Public Property Let Shrink(ByVal vShrink As YesNo)
    On Error GoTo ShrinkLetErr
    mvarShrink = vShrink
    Exit Property
ShrinkLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: Shrink Property Let")
End Property

Public Property Get SizeShrink() As Double
    On Error GoTo SizeShrinkGetErr
    SizeShrink = mvarSizeShrink
    Exit Property
SizeShrinkGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: SizeShrink Property Get")
End Property

Public Property Let SizeShrink(ByVal vSizeShrink As Double)
    On Error GoTo SizeShrinkLetErr
    mvarSizeShrink = vSizeShrink
    Exit Property
SizeShrinkLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: SizeShrink Property Let")
End Property

Public Property Get Precision() As Double
    On Error GoTo PrecisionGetErr
    Precision = mvarPrecision
    Exit Property
PrecisionGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: Precision Property Get")
End Property

Public Property Let Precision(ByVal vPrecision As Double)
    On Error GoTo PrecisionLetErr
    mvarPrecision = vPrecision
    Exit Property
PrecisionLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: Precision Property Let")
End Property

Public Property Get Splash() As YesNo
    On Error GoTo SplashGetErr
    Splash = mvarSplash
    Exit Property
SplashGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: Splash Property Get")
End Property

Public Property Let Splash(ByVal Value As YesNo)
    On Error GoTo SplashLetErr
    mvarSplash = Value
    Exit Property
SplashLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: Splash Property Let")
End Property

Public Property Get CurrentLoadCase() As Integer
    On Error GoTo CurrentLoadCaseGetErr
    CurrentLoadCase = mvarCurrentLoadCase
    Exit Property
CurrentLoadCaseGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: CurrentLoadCase Property Get")
End Property

Public Property Let CurrentLoadCase(ByVal Value As Integer)
    On Error GoTo CurrentLoadCaseLetErr
    mvarCurrentLoadCase = Value
    Exit Property
CurrentLoadCaseLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: CurrentLoadCase Property Let")
End Property

Public Property Get ColorPlates() As ColorConstants
    On Error GoTo ColorPlatesGetErr
    ColorPlates = mvarColorPlates
    Exit Property
ColorPlatesGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: ColorPlates Property Get")
End Property

Public Property Let ColorPlates(ByVal Value As ColorConstants)
    On Error GoTo ColorPlatesLetErr
    mvarColorPlates = Value
    Exit Property
ColorPlatesLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: ColorPlates Property Let")
End Property

Public Property Get ColorBeams() As ColorConstants
    On Error GoTo ColorBeamsGetErr
    ColorBeams = mvarColorBeams
    Exit Property
ColorBeamsGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: ColorBeams Property Get")
End Property

Public Property Let ColorBeams(ByVal Value As ColorConstants)
    On Error GoTo ColorBeamsLetErr
    mvarColorBeams = Value
    Exit Property
ColorBeamsLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: ColorBeams Property Let")
End Property

Public Property Get ColorFleshes() As ColorConstants
    On Error GoTo ColorFleshesGetErr
    ColorFleshes = mvarColorFleshes
    Exit Property
ColorFleshesGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: ColorFleshes Property Get")
End Property

Public Property Let ColorFleshes(ByVal Value As ColorConstants)
    On Error GoTo ColorFleshesLetErr
    mvarColorFleshes = Value
    Exit Property
ColorFleshesLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: ColorFleshes Property Let")
End Property

Public Property Get ColorPrimaryFrames() As ColorConstants
    On Error GoTo ColorPrimaryFramesGetErr
    ColorPrimaryFrames = mvarColorPrimaryFrames
    Exit Property
ColorPrimaryFramesGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: ColorPrimaryFrames Property Get")
End Property

Public Property Let ColorPrimaryFrames(ByVal Value As ColorConstants)
    On Error GoTo ColorPrimaryFramesLetErr
    mvarColorPrimaryFrames = Value
    Exit Property
ColorPrimaryFramesLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: ColorPrimaryFrames Property Let")
End Property

Public Property Get ColorSecondaryFrames() As ColorConstants
    On Error GoTo ColorSecondaryFramesGetErr
    ColorSecondaryFrames = mvarColorSecondaryFrames
    Exit Property
ColorSecondaryFramesGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: ColorSecondaryFrames Property Get")
End Property

Public Property Let ColorSecondaryFrames(ByVal Value As ColorConstants)
    On Error GoTo ColorSecondaryFramesLetErr
    mvarColorSecondaryFrames = Value
    Exit Property
ColorSecondaryFramesLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: ColorSecondaryFrames Property Let")
End Property

Public Property Get ColorPrimaryStiffeners() As ColorConstants
    On Error GoTo ColorPrimaryStiffenersGetErr
    ColorPrimaryStiffeners = mvarColorPrimaryStiffeners
    Exit Property
ColorPrimaryStiffenersGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: ColorPrimaryStiffeners Property Get")
End Property

Public Property Let ColorPrimaryStiffeners(ByVal Value As ColorConstants)
    On Error GoTo ColorPrimaryStiffenersLetErr
    mvarColorPrimaryStiffeners = Value
    Exit Property
ColorPrimaryStiffenersLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: ColorPrimaryStiffeners Property Let")
End Property

Public Property Get ColorSecondaryStiffeners() As ColorConstants
    On Error GoTo ColorSecondaryStiffenersGetErr
    ColorSecondaryStiffeners = mvarColorSecondaryStiffeners
    Exit Property
ColorSecondaryStiffenersGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: ColorSecondaryStiffeners Property Get")
End Property

Public Property Let ColorSecondaryStiffeners(ByVal Value As ColorConstants)
    On Error GoTo ColorSecondaryStiffenersLetErr
    mvarColorSecondaryStiffeners = Value
    Exit Property
ColorSecondaryStiffenersLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: ColorSecondaryStiffeners Property Let")
End Property

Public Property Get ColorNodes() As ColorConstants
    On Error GoTo ColorNodesGetErr
    ColorNodes = mvarColorNodes
    Exit Property
ColorNodesGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: ColorNodes Property Get")
End Property

Public Property Let ColorNodes(ByVal Value As ColorConstants)
    On Error GoTo ColorNodesLetErr
    mvarColorNodes = Value
    Exit Property
ColorNodesLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: ColorNodes Property Let")
End Property

Public Property Get ColorGirders() As ColorConstants
    On Error GoTo ColorGirdersGetErr
    ColorGirders = mvarColorGirders
    Exit Property
ColorGirdersGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: ColorGirders Property Get")
End Property

Public Property Let ColorGirders(ByVal Value As ColorConstants)
    On Error GoTo ColorGirdersLetErr
    mvarColorGirders = Value
    Exit Property
ColorGirdersLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: ColorGirders Property Let")
End Property

Public Property Get ColorGrid() As ColorConstants
    On Error GoTo ColorGridGetErr
    ColorGrid = mvarColorGrid
    Exit Property
ColorGridGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: ColorGrid Property Get")
End Property

Public Property Let ColorGrid(ByVal Value As ColorConstants)
    On Error GoTo ColorGridLetErr
    mvarColorGrid = Value
    Exit Property
ColorGridLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: ColorGrid Property Let")
End Property

Public Property Get ColorScreen() As ColorConstants
    On Error GoTo ColorScreenGetErr
    ColorScreen = mvarColorScreen
    Exit Property
ColorScreenGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: ColorScreen Property Get")
End Property

Public Property Let ColorScreen(ByVal Value As ColorConstants)
    On Error GoTo ColorScreenLetErr
    mvarColorScreen = Value
    Exit Property
ColorScreenLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: ColorScreen Property Let")
End Property

Public Property Get ColorPressuresUniformlyDistributed() As ColorConstants
    On Error GoTo ColorPressuresUniformlyDistributedGetErr
    ColorPressuresUniformlyDistributed = mvarColorPressuresUniformlyDistributed
    Exit Property
ColorPressuresUniformlyDistributedGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: ColorPressuresUniformlyDistributed Property Get")
End Property

Public Property Let ColorPressuresUniformlyDistributed(ByVal Value As ColorConstants)
    On Error GoTo ColorPressuresUniformlyDistributedLetErr
    mvarColorPressuresUniformlyDistributed = Value
    Exit Property
ColorPressuresUniformlyDistributedLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: ColorPressuresUniformlyDistributed Property Let")
End Property

Public Property Get ColorBoundaryConditions() As ColorConstants
    On Error GoTo ColorBoundaryConditionsGetErr
    ColorBoundaryConditions = mvarColorBoundaryConditions
    Exit Property
ColorBoundaryConditionsGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: ColorBoundaryConditions Property Get")
End Property

Public Property Let ColorBoundaryConditions(ByVal Value As ColorConstants)
    On Error GoTo ColorBoundaryConditionsLetErr
    mvarColorBoundaryConditions = Value
    Exit Property
ColorBoundaryConditionsLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: ColorBoundaryConditions Property Let")
End Property

Public Property Get ColorLocalizedPressure() As ColorConstants
    On Error GoTo ColorLocalizedPressureGetErr
    ColorLocalizedPressure = mvarColorLocalizedPressure
    Exit Property
ColorLocalizedPressureGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: ColorLocalizedPressure Property Get")
End Property

Public Property Let ColorLocalizedPressure(ByVal Value As ColorConstants)
    On Error GoTo ColorLocalizedPressureLetErr
    mvarColorLocalizedPressure = Value
    Exit Property
ColorLocalizedPressureLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: ColorLocalizedPressure Property Let")
End Property

Public Property Get TextPressuresUniformlyDistributed() As TextOptions
    On Error GoTo TextPressuresUniformlyDistributedGetErr
    TextPressuresUniformlyDistributed = mvarTextPressuresUniformlyDistributed
    Exit Property
TextPressuresUniformlyDistributedGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: TextPressuresUniformlyDistributed Property Get")
End Property


Public Property Let TextPressuresUniformlyDistributed(ByVal Value As TextOptions)
    On Error GoTo TextPressuresUniformlyDistributedLetErr
    mvarTextPressuresUniformlyDistributed = Value
    Exit Property
TextPressuresUniformlyDistributedLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: TextPressuresUniformlyDistributed Property Let")
End Property

Public Property Get TextPrimaryFrames() As TextOptions
    On Error GoTo TextPrimaryFramesGetErr
    TextPrimaryFrames = mvarTextPrimaryFrames
    Exit Property
TextPrimaryFramesGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: TextPrimaryFrames Property Get")
End Property

Public Property Let TextPrimaryFrames(ByVal Value As TextOptions)
    On Error GoTo TextPrimaryFramesLetErr
    mvarTextPrimaryFrames = Value
    Exit Property
TextPrimaryFramesLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: TextPrimaryFrames Property Let")
End Property

Public Property Get TextSecondaryFrames() As TextOptions
    On Error GoTo TextSecondaryFramesGetErr
    TextSecondaryFrames = mvarTextSecondaryFrames
    Exit Property
TextSecondaryFramesGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: TextSecondaryFrames Property Get")
End Property

Public Property Let TextSecondaryFrames(ByVal Value As TextOptions)
    On Error GoTo TextSecondaryFramesLetErr
    mvarTextSecondaryFrames = Value
    Exit Property
TextSecondaryFramesLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: TextSecondaryFrames Property Let")
End Property

Public Property Get TextPlates() As TextOptions
    On Error GoTo TextPlatesGetErr
    TextPlates = mvarTextPlates
    Exit Property
TextPlatesGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: TextPlates Property Get")
End Property

Public Property Let TextPlates(ByVal Value As TextOptions)
    On Error GoTo TextPlatesLetErr
    mvarTextPlates = Value
    Exit Property
TextPlatesLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: TextPlates Property Let")
End Property

Public Property Get TextBeams() As TextOptions
    On Error GoTo TextBeamsGetErr
    TextBeams = mvarTextBeams
    Exit Property
TextBeamsGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: TextBeams Property Get")
End Property

Public Property Let TextBeams(ByVal Value As TextOptions)
    On Error GoTo TextBeamsLetErr
    mvarTextBeams = Value
    Exit Property
TextBeamsLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: TextBeams Property Let")
End Property

Public Property Get TextPrimaryStiffeners() As TextOptions
    On Error GoTo TextPrimaryStiffenersGetErr
    TextPrimaryStiffeners = mvarTextPrimaryStiffeners
    Exit Property
TextPrimaryStiffenersGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: TextPrimaryStiffeners Property Get")
End Property

Public Property Let TextPrimaryStiffeners(ByVal Value As TextOptions)
    On Error GoTo TextPrimaryStiffenersLetErr
    mvarTextPrimaryStiffeners = Value
    Exit Property
TextPrimaryStiffenersLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: TextPrimaryStiffeners Property Let")
End Property

Public Property Get TextSecondaryStiffeners() As TextOptions
    On Error GoTo TextSecondaryStiffenersGetErr
    TextSecondaryStiffeners = mvarTextSecondaryStiffeners
    Exit Property
TextSecondaryStiffenersGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: TextSecondaryStiffeners Property Get")
End Property

Public Property Let TextSecondaryStiffeners(ByVal Value As TextOptions)
    On Error GoTo TextSecondaryStiffenersLetErr
    mvarTextSecondaryStiffeners = Value
    Exit Property
TextSecondaryStiffenersLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: TextSecondaryStiffeners Property Let")
End Property

Public Property Get TextGirders() As TextOptions
    On Error GoTo TextGirdersGetErr
    TextGirders = mvarTextGirders
    Exit Property
TextGirdersGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: TextGirders Property Get")
End Property

Public Property Let TextGirders(ByVal Value As TextOptions)
    On Error GoTo TextGirdersLetErr
    mvarTextGirders = Value
    Exit Property
TextGirdersLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: TextGirders Property Let")
End Property

Public Property Get TextNodes() As TextOptions
    On Error GoTo TextNodesGetErr
    TextNodes = mvarTextNodes
    Exit Property
TextNodesGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: TextNodes Property Get")
End Property

Public Property Let TextNodes(ByVal Value As TextOptions)
    On Error GoTo TextNodesLetErr
    mvarTextNodes = Value
    Exit Property
TextNodesLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: TextNodes Property Let")
End Property

Public Property Get TextBoundaryConditions() As TextOptions
    On Error GoTo TextBoundaryConditionsGetErr
    TextBoundaryConditions = mvarTextBoundaryConditions
    Exit Property
TextBoundaryConditionsGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: TextBoundaryConditions Property Get")
End Property

Public Property Let TextBoundaryConditions(ByVal Value As TextOptions)
    On Error GoTo TextBoundaryConditionsLetErr
    mvarTextBoundaryConditions = Value
    Exit Property
TextBoundaryConditionsLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: TextBoundaryConditions Property Let")
End Property

Public Property Get TextLocalizedPressure() As TextOptions
    On Error GoTo TextLocalizedPressureGetErr
    TextLocalizedPressure = mvarTextLocalizedPressure
    Exit Property
TextLocalizedPressureGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: TextLocalizedPressure Property Get")
End Property

Public Property Let TextLocalizedPressure(ByVal Value As TextOptions)
    On Error GoTo TextLocalizedPressureLetErr
    mvarTextLocalizedPressure = Value
    Exit Property
TextLocalizedPressureLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: TextLocalizedPressure Property Let")
End Property

Public Property Get TextParticipation() As TextOptions
    On Error GoTo TextParticipationGetErr
    TextParticipation = mvarTextParticipation
    Exit Property
TextParticipationGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: TextParticipation Property Get")
End Property

Public Property Let TextParticipation(ByVal Value As TextOptions)
    On Error GoTo TextParticipationLetErr
    mvarTextParticipation = Value
    Exit Property
TextParticipationLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: TextParticipation Property Let")
End Property

Public Property Get TextShowVariations() As YesNo
    On Error GoTo TextShowVariationsGetErr
    TextShowVariations = mvarTextShowVariations
    Exit Property
TextShowVariationsGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: TextShowVariations Property Get")
End Property

Public Property Let TextShowVariations(ByVal vTextShowVariations As YesNo)
    On Error GoTo TextShowVariationsLetErr
    mvarTextShowVariations = vTextShowVariations
    Exit Property
TextShowVariationsLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: TextShowVariations Property Let")
End Property

Public Property Get ShowLegend() As YesNo
    On Error GoTo ShowLegendGetErr
    ShowLegend = mvarShowLegend
    Exit Property
ShowLegendGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: ShowLegend Property Get")
End Property

Public Property Let ShowLegend(ByVal vShowLegend As YesNo)
    On Error GoTo ShowLegendLetErr
    mvarShowLegend = vShowLegend
    Exit Property
ShowLegendLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: ShowLegend Property Let")
End Property

Public Function getItemState(ByVal index As Integer) As String
    On Error GoTo getItemStateErr
    Select Case index
        Case 0
            getItemState = "Off"
        Case 1
            getItemState = "On"
    End Select
    Exit Function
getItemStateErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: Function getItemState")
End Function

Public Function setItemState(ByVal str As String) As Integer
    On Error GoTo setItemStateErr
    Select Case str
        Case "Off"
            setItemState = 0
        Case "On"
            setItemState = 1
    End Select
    Exit Function
setItemStateErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: Function setItemState")
End Function

'Public Function getTextOptionsString(ByVal index As Integer) As String
'    On Error GoTo getTextOptionsStringErr
'    Select Case index
'        Case 0
'            getTextOptionsString = "None"
'        Case 1
'            getTextOptionsString = "Numbers"
'        Case 2
'            getTextOptionsString = "Lenght - m"
'        Case 3
'            getTextOptionsString = "Net Thickness - mm"
'        Case 4
'            getTextOptionsString = "Gross Thickness - mm"
'        Case 5
'            getTextOptionsString = "Corrosion - mm"
'        Case 6
'            getTextOptionsString = "Pressures - mH20"
'        Case 7
'            getTextOptionsString = "Web Height - mm"
'        Case 8
'             getTextOptionsString = "Web Thickness - mm"
'        Case 9
'            getTextOptionsString = "Web Gross Thickness - mm"
'        Case 10
'            getTextOptionsString = "Flange Width - mm"
'        Case 11
'            getTextOptionsString = "Flange Thickness - mm"
'        Case 12
'            getTextOptionsString = "Flange Gross Thickness - mm"
'        Case 13
'            getTextOptionsString = "Spacing - mm"
'        Case 14
'            getTextOptionsString = "All"
'        Case 15
'            getTextOptionsString = "Coordinates - m"
'        Case 16
'            getTextOptionsString = "Distance From In Node - m"
'        Case 17
'            getTextOptionsString = "Boundaries"
'        Case 18
'            getTextOptionsString = "Gross Section Modulus - cm3"
'    End Select
'    Exit Function
'getTextOptionsStringErr:
'    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: Function getTextOptionsString")
'End Function

Public Function getTextOptionsString(ByVal index As Integer) As String
    On Error GoTo getTextOptionsStringErr
    Select Case index
        Case 0
            getTextOptionsString = "None"
        Case 1
            getTextOptionsString = "Numbers"
        Case 2
            getTextOptionsString = "Lenght"
        Case 3
            getTextOptionsString = "Net Thickness"
        Case 4
            getTextOptionsString = "Gross Thickness"
        Case 5
            getTextOptionsString = "Corrosion"
        Case 6
            getTextOptionsString = "Pressures"
        Case 7
            getTextOptionsString = "Web Height"
        Case 8
             getTextOptionsString = "Web Thickness"
        Case 9
            getTextOptionsString = "Web Gross Thickness"
        Case 10
            getTextOptionsString = "Flange Width"
        Case 11
            getTextOptionsString = "Flange Thickness"
        Case 12
            getTextOptionsString = "Flange Gross Thickness"
        Case 13
            getTextOptionsString = "Spacing"
        Case 14
            getTextOptionsString = "All"
        Case 15
            getTextOptionsString = "Coordinates"
        Case 16
            getTextOptionsString = "Distance From In Node"
        Case 17
            getTextOptionsString = "Boundaries"
        Case 18
            getTextOptionsString = "Gross Section Modulus"
        Case 19
            getTextOptionsString = "Participation"
        Case 20
            getTextOptionsString = "Materials"
    End Select
    Exit Function
getTextOptionsStringErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: Function getTextOptionsString")
End Function

Public Function setTextOptionsString(ByVal str As String) As Integer
    On Error GoTo setTextOptionsStringErr
        Select Case str
            Case "None"
                setTextOptionsString = 0
            Case "Numbers"
                setTextOptionsString = 1
            Case "Lenght"
                setTextOptionsString = 2
            Case "Net Thickness"
                setTextOptionsString = 3
            Case "Gross Thickness"
                setTextOptionsString = 4
            Case "Corrosion"
                setTextOptionsString = 5
            Case "Pressures"
                setTextOptionsString = 6
            Case "Web Height"
                setTextOptionsString = 7
            Case "Web Thickness"
                 setTextOptionsString = 8
            Case "Web Gross Thickness"
                setTextOptionsString = 9
            Case "Flange Width"
                setTextOptionsString = 10
            Case "Flange Thickness"
                setTextOptionsString = 11
            Case "Flange Gross Thickness"
                setTextOptionsString = 12
            Case "Spacing"
                setTextOptionsString = 13
            Case "All"
                setTextOptionsString = 14
            Case "Coordinates"
                setTextOptionsString = 15
            Case "Distance From In Node"
                setTextOptionsString = 16
            Case "Boundaries"
                setTextOptionsString = 17
            Case "Gross Section Modulus"
                setTextOptionsString = 18
            Case "Participation"
                setTextOptionsString = 19
            Case "Materials"
                setTextOptionsString = 20
        End Select
    Exit Function
setTextOptionsStringErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: Function setTextOptionsString")
End Function

Public Function Clone() As cDisplaySettings
    On Error GoTo CloneErr
    Set Clone = New cDisplaySettings
    Clone.DrawNodes = Me.DrawNodes
    Clone.DrawPlates = Me.DrawPlates
    Clone.DrawBeams = Me.DrawBeams
    Clone.DrawFleshes = Me.DrawFleshes
    Clone.DrawPrimaryFrames = Me.DrawPrimaryFrames
    Clone.DrawSecondaryFrames = Me.DrawSecondaryFrames
    Clone.DrawPrimaryStiffeners = Me.DrawPrimaryStiffeners
    Clone.DrawSecondaryStiffeners = Me.DrawSecondaryStiffeners
    Clone.DrawGirders = Me.DrawGirders
    Clone.DrawCoordinateAxis = Me.DrawCoordinateAxis
    Clone.DrawPanelNumbers = Me.DrawPanelNumbers
    Clone.DrawBoundaryConditions = Me.DrawBoundaryConditions
    Clone.DrawPressuresUniformlyDistributed = Me.DrawPressuresUniformlyDistributed
    Clone.DrawLocalizedPressure = Me.DrawLocalizedPressure
    Clone.DrawPlateThickness = Me.DrawPlateThickness
    Clone.DrawScantlings = Me.DrawScantlings
    Clone.DrawSolution = Me.DrawSolution
    Clone.DrawNeutralAxis = Me.DrawNeutralAxis
    Clone.DrawCorrosionThickness = Me.DrawCorrosionThickness
    Clone.DrawCostCAtItems = Me.DrawCostCAtItems
    Clone.DrawGrid = Me.DrawGrid
    Clone.DrawParticipation = Me.DrawParticipation
    Clone.SizeNodes = Me.SizeNodes
    Clone.SizePlates = Me.SizePlates
    Clone.SizeBeams = Me.SizeBeams
    Clone.SizeFleshes = Me.SizeFleshes
    Clone.SizePressuresUniformlyDistributed = Me.SizePressuresUniformlyDistributed
    Clone.SizeLocalizedPressure = Me.SizeLocalizedPressure
    Clone.SizeThickness = Me.SizeThickness
    Clone.SizePrimaryFrames = Me.SizePrimaryFrames
    Clone.SizeSecondaryFrames = Me.SizeSecondaryFrames
    Clone.SizePrimaryStiffeners = Me.SizePrimaryStiffeners
    Clone.SizeSecondaryStiffeners = Me.SizeSecondaryStiffeners
    Clone.SizeGirders = Me.SizeGirders
    Clone.SizeSolution = Me.SizeSolution
    Clone.SizeGrid = Me.SizeGrid
    Clone.SolutionLoadCase = Me.SolutionLoadCase
    Clone.Shrink = Me.Shrink
    Clone.SizeShrink = Me.SizeShrink
    Clone.Precision = Me.Precision
    Clone.Splash = Me.Splash
    Clone.ShowLegend = Me.ShowLegend
    'Colors
    Clone.ColorPlates = Me.ColorPlates
    Clone.ColorBeams = Me.ColorBeams
    Clone.ColorFleshes = Me.ColorFleshes
    Clone.ColorPrimaryFrames = Me.ColorPrimaryFrames
    Clone.ColorSecondaryFrames = Me.ColorSecondaryFrames
    Clone.ColorPrimaryStiffeners = Me.ColorPrimaryStiffeners
    Clone.ColorSecondaryStiffeners = Me.ColorSecondaryStiffeners
    Clone.ColorNodes = Me.ColorNodes
    Clone.ColorGirders = Me.ColorGirders
    Clone.ColorGrid = Me.ColorGrid
    Clone.ColorScreen = Me.ColorScreen
    Clone.ColorPressuresUniformlyDistributed = Me.ColorPressuresUniformlyDistributed
    Clone.ColorBoundaryConditions = Me.ColorBoundaryConditions
    Clone.ColorLocalizedPressure = Me.ColorLocalizedPressure
    'Texts
    Clone.TextPressuresUniformlyDistributed = Me.TextPressuresUniformlyDistributed
    Clone.TextPrimaryFrames = Me.TextPrimaryFrames
    Clone.TextSecondaryFrames = Me.TextSecondaryFrames
    Clone.TextPlates = Me.TextPlates
    Clone.TextBeams = Me.TextBeams
    Clone.TextPrimaryStiffeners = Me.TextPrimaryStiffeners
    Clone.TextSecondaryStiffeners = Me.TextSecondaryStiffeners
    Clone.TextGirders = Me.TextGirders
    Clone.TextNodes = Me.TextNodes
    Clone.TextBoundaryConditions = Me.TextBoundaryConditions
    Clone.TextLocalizedPressure = Me.TextLocalizedPressure
    Clone.TextShowVariations = Me.TextShowVariations
    Clone.TextParticipation = Me.TextParticipation
    Exit Function
CloneErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDisplaySettings: Function Clone")
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

