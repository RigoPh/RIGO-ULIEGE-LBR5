VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "COMDLG32.OCX"
Object = "{831FDD16-0C5C-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCTL.OCX"
Begin VB.Form frmProject 
   AutoRedraw      =   -1  'True
   Caption         =   "Project"
   ClientHeight    =   7950
   ClientLeft      =   5460
   ClientTop       =   2805
   ClientWidth     =   14115
   DrawMode        =   2  'Blackness
   Icon            =   "frmProject.frx":0000
   MDIChild        =   -1  'True
   MousePointer    =   99  'Custom
   ScaleHeight     =   530
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   941
   WindowState     =   2  'Maximized
   Begin MSComctlLib.ProgressBar ProgressBar 
      Height          =   255
      Left            =   120
      Negotiate       =   -1  'True
      TabIndex        =   4
      Top             =   6480
      Visible         =   0   'False
      Width           =   1455
      _ExtentX        =   2566
      _ExtentY        =   450
      _Version        =   393216
      Appearance      =   0
      Scrolling       =   1
   End
   Begin VB.ComboBox Combo 
      Appearance      =   0  'Flat
      BackColor       =   &H8000000F&
      Height          =   315
      Left            =   120
      Style           =   2  'Dropdown List
      TabIndex        =   2
      Top             =   6000
      Visible         =   0   'False
      Width           =   975
   End
   Begin VB.PictureBox picScreen 
      Appearance      =   0  'Flat
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      BackColor       =   &H80000005&
      BorderStyle     =   0  'None
      ForeColor       =   &H80000008&
      Height          =   4095
      Left            =   240
      MousePointer    =   99  'Custom
      Negotiate       =   -1  'True
      ScaleHeight     =   273
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   407
      TabIndex        =   0
      Top             =   120
      Width           =   6105
      Begin VB.PictureBox pic1 
         Appearance      =   0  'Flat
         AutoRedraw      =   -1  'True
         BackColor       =   &H80000005&
         ClipControls    =   0   'False
         DrawStyle       =   5  'Transparent
         FillColor       =   &H00FFFFFF&
         FillStyle       =   5  'Downward Diagonal
         ForeColor       =   &H80000008&
         Height          =   1215
         Left            =   120
         ScaleHeight     =   79
         ScaleMode       =   3  'Pixel
         ScaleWidth      =   95
         TabIndex        =   3
         Top             =   2880
         Visible         =   0   'False
         Width           =   1455
      End
      Begin MSComDlg.CommonDialog dlgCommonDialog 
         Left            =   240
         Top             =   1200
         _ExtentX        =   847
         _ExtentY        =   847
         _Version        =   393216
      End
      Begin VB.Line Line1 
         BorderColor     =   &H000000FF&
         BorderStyle     =   6  'Inside Solid
         BorderWidth     =   2
         Visible         =   0   'False
         X1              =   16
         X2              =   48
         Y1              =   160
         Y2              =   128
      End
      Begin VB.Shape Border 
         BorderColor     =   &H00000000&
         BorderStyle     =   3  'Dot
         Height          =   750
         Left            =   240
         Top             =   240
         Visible         =   0   'False
         Width           =   750
      End
   End
   Begin MSComctlLib.StatusBar StatusBar 
      Align           =   2  'Align Bottom
      Height          =   300
      Left            =   0
      TabIndex        =   1
      Top             =   7650
      Visible         =   0   'False
      Width           =   14115
      _ExtentX        =   24897
      _ExtentY        =   529
      _Version        =   393216
      BeginProperty Panels {8E3867A5-8586-11D1-B16A-00C0F0283628} 
         NumPanels       =   5
         BeginProperty Panel1 {8E3867AB-8586-11D1-B16A-00C0F0283628} 
            Object.Width           =   2646
            MinWidth        =   2646
         EndProperty
         BeginProperty Panel2 {8E3867AB-8586-11D1-B16A-00C0F0283628} 
            AutoSize        =   2
            Object.Width           =   4101
            MinWidth        =   4101
            Key             =   "Mode"
            Object.ToolTipText     =   "Current Screen Mode"
         EndProperty
         BeginProperty Panel3 {8E3867AB-8586-11D1-B16A-00C0F0283628} 
            AutoSize        =   2
            Key             =   "LoadCase"
            Object.ToolTipText     =   "Current Load Case"
         EndProperty
         BeginProperty Panel4 {8E3867AB-8586-11D1-B16A-00C0F0283628} 
            Key             =   "Solver"
            Object.ToolTipText     =   "Current Solver"
         EndProperty
         BeginProperty Panel5 {8E3867AB-8586-11D1-B16A-00C0F0283628} 
            Object.Width           =   3969
            MinWidth        =   3969
            Key             =   "Assistant"
            Object.ToolTipText     =   "Modelling Assistant"
         EndProperty
      EndProperty
   End
   Begin VB.Label lbAssistant 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BorderStyle     =   1  'Fixed Single
      Caption         =   "0 Problems Found"
      ForeColor       =   &H80000008&
      Height          =   195
      Left            =   6840
      TabIndex        =   5
      Top             =   7320
      Width           =   1275
   End
   Begin VB.Menu mnuFile 
      Caption         =   "&File"
      Begin VB.Menu mnuFileNew 
         Caption         =   "&New"
         Shortcut        =   ^N
      End
      Begin VB.Menu mnuFileOpen 
         Caption         =   "&Open..."
         Shortcut        =   ^O
      End
      Begin VB.Menu mnuFileClose 
         Caption         =   "&Close"
      End
      Begin VB.Menu mnuFileBar0 
         Caption         =   "-"
      End
      Begin VB.Menu mnuFileSave 
         Caption         =   "&Save"
         Shortcut        =   ^S
      End
      Begin VB.Menu mnuFileSaveAs 
         Caption         =   "Save &As..."
      End
      Begin VB.Menu mnuFileSaveAll 
         Caption         =   "Save A&ll"
         Visible         =   0   'False
      End
      Begin VB.Menu mnuFileBar1 
         Caption         =   "-"
      End
      Begin VB.Menu mnuFileImportFrom 
         Caption         =   "Import From..."
         Begin VB.Menu mnuFileImportFromMarsLbr5TransfertFile 
            Caption         =   "MARS-LBR5 Transfert File"
         End
         Begin VB.Menu mnuFileImportFromAvproLbr5TransfertFile 
            Caption         =   "AVPRO-LBR5 Transfert File"
         End
      End
      Begin VB.Menu mnuFileBar2 
         Caption         =   "-"
      End
      Begin VB.Menu mnuFileProperties 
         Caption         =   "Propert&ies"
      End
      Begin VB.Menu mnuFileOpenTargetFolder 
         Caption         =   "Open &Folder"
      End
      Begin VB.Menu mnuFileBar3 
         Caption         =   "-"
      End
      Begin VB.Menu mnuFilePageSetup 
         Caption         =   "Page Set&up"
         Visible         =   0   'False
      End
      Begin VB.Menu mnuFilePrintPreview 
         Caption         =   "Print Pre&view"
         Visible         =   0   'False
      End
      Begin VB.Menu mnuFilePrint 
         Caption         =   "&Print"
         Shortcut        =   ^P
      End
      Begin VB.Menu mnuFileBar4 
         Caption         =   "-"
      End
      Begin VB.Menu mnuFileSend 
         Caption         =   "Sen&d..."
      End
      Begin VB.Menu mnuFileBar5 
         Caption         =   "-"
      End
      Begin VB.Menu mnuFileMRU 
         Caption         =   ""
         Index           =   1
         Visible         =   0   'False
      End
      Begin VB.Menu mnuFileMRU 
         Caption         =   ""
         Index           =   2
         Visible         =   0   'False
      End
      Begin VB.Menu mnuFileMRU 
         Caption         =   ""
         Index           =   3
         Visible         =   0   'False
      End
      Begin VB.Menu mnuFileMRU 
         Caption         =   ""
         Index           =   4
         Visible         =   0   'False
      End
      Begin VB.Menu mnuFileMRU 
         Caption         =   ""
         Index           =   5
         Visible         =   0   'False
      End
      Begin VB.Menu mnuFileMRU 
         Caption         =   ""
         Index           =   6
         Visible         =   0   'False
      End
      Begin VB.Menu mnuFileMRU 
         Caption         =   ""
         Index           =   7
         Visible         =   0   'False
      End
      Begin VB.Menu mnuFileMRU 
         Caption         =   ""
         Index           =   8
         Visible         =   0   'False
      End
      Begin VB.Menu mnuFileMRU 
         Caption         =   ""
         Index           =   9
         Visible         =   0   'False
      End
      Begin VB.Menu mnuFileMRU 
         Caption         =   ""
         Index           =   10
         Visible         =   0   'False
      End
      Begin VB.Menu mnuFileBar6 
         Caption         =   "-"
         Visible         =   0   'False
      End
      Begin VB.Menu mnuFileExit 
         Caption         =   "E&xit"
      End
   End
   Begin VB.Menu mnuEdit 
      Caption         =   "&Edit"
      Visible         =   0   'False
      Begin VB.Menu mnuEditUndo 
         Caption         =   "&Undo"
         Enabled         =   0   'False
      End
      Begin VB.Menu mnuEditBar0 
         Caption         =   "-"
      End
      Begin VB.Menu mnuEditCut 
         Caption         =   "Cu&t"
         Enabled         =   0   'False
         Shortcut        =   ^X
      End
      Begin VB.Menu mnuEditCopy 
         Caption         =   "&Copy"
         Enabled         =   0   'False
         Shortcut        =   ^C
      End
      Begin VB.Menu mnuEditPaste 
         Caption         =   "&Paste"
         Enabled         =   0   'False
         Shortcut        =   ^V
      End
      Begin VB.Menu mnuEditPasteSpecial 
         Caption         =   "Paste &Special"
         Enabled         =   0   'False
      End
   End
   Begin VB.Menu mnuView 
      Caption         =   "&View"
      Begin VB.Menu mnuViewRefresh 
         Caption         =   "&Refresh"
      End
      Begin VB.Menu mnuViewOptions 
         Caption         =   "&Options..."
         Visible         =   0   'False
      End
      Begin VB.Menu mnuViewDisplayOptions 
         Caption         =   "&Display Options"
      End
      Begin VB.Menu mnuViewWebBrowser 
         Caption         =   "&Web Browser"
      End
      Begin VB.Menu mnuViewBar0 
         Caption         =   "-"
      End
      Begin VB.Menu mnuViewToolbar 
         Caption         =   "&Toolbars"
         Begin VB.Menu mnuViewToolbarsStandard 
            Caption         =   "&Standard"
         End
         Begin VB.Menu mnuViewToolbarsView 
            Caption         =   "&View"
         End
         Begin VB.Menu mnuViewToolbarsGeometry 
            Caption         =   "&Geometry"
         End
         Begin VB.Menu mnuViewToolbarsSolver 
            Caption         =   "Sol&ver"
         End
         Begin VB.Menu mnuViewToolbarsTools 
            Caption         =   "T&ools"
         End
      End
      Begin VB.Menu mnuViewStatusBar 
         Caption         =   "Status &Bar"
         Checked         =   -1  'True
      End
   End
   Begin VB.Menu mnuProject 
      Caption         =   "&Project"
      Begin VB.Menu mnuProjectTitle 
         Caption         =   "&Title"
      End
      Begin VB.Menu mnuProjectOptions 
         Caption         =   "&Options"
         Begin VB.Menu mnuProjectOptionsOverallSpan 
            Caption         =   "Overall &Span"
         End
         Begin VB.Menu mnuProjectOptionsRuleLength 
            Caption         =   "Rule &Length"
         End
         Begin VB.Menu mnuProjectOptionsDeadweight 
            Caption         =   "&Deadweight"
         End
         Begin VB.Menu mnuProjectOptionsFourierSeries 
            Caption         =   "&Fourier Series"
         End
         Begin VB.Menu mnuProjectOptionsOutput 
            Caption         =   "Out&put"
         End
      End
   End
   Begin VB.Menu mnuModel 
      Caption         =   "&Model"
      Begin VB.Menu mnuModelNodes 
         Caption         =   "&Nodes"
      End
      Begin VB.Menu mnuModesBar0 
         Caption         =   "-"
      End
      Begin VB.Menu mnuModelAddPlates 
         Caption         =   "Add &Plate(s)"
      End
      Begin VB.Menu mnuModelAddBeams 
         Caption         =   "Add &Beam(s)"
      End
      Begin VB.Menu mnuModelAddDoubleHulls 
         Caption         =   "Add &Double Hull(s)"
         Enabled         =   0   'False
      End
      Begin VB.Menu mnuModesBar1 
         Caption         =   "-"
      End
      Begin VB.Menu mnuModelDividePanel 
         Caption         =   "&Divide Panel"
      End
      Begin VB.Menu mnuModelReversePanels 
         Caption         =   "&Reverse Panel(s)"
      End
      Begin VB.Menu mnuModelMatchProperties 
         Caption         =   "&Match Properties"
      End
      Begin VB.Menu mnuModelSortPanels 
         Caption         =   "&Sort Panels"
         Visible         =   0   'False
      End
      Begin VB.Menu mnuModesBar2 
         Caption         =   "-"
      End
      Begin VB.Menu mnuModelDeletePanel 
         Caption         =   "Dele&te Panel"
      End
      Begin VB.Menu mnuModelExplodeDoubleHull 
         Caption         =   "&Explode Double Hull"
         Enabled         =   0   'False
      End
      Begin VB.Menu mnuModesBar3 
         Caption         =   "-"
      End
      Begin VB.Menu mnuModelMoveOrigin 
         Caption         =   "Move &Origin"
      End
   End
   Begin VB.Menu mnuPanels 
      Caption         =   "Pa&nels"
      Begin VB.Menu mnuPanelsPlateScantlings 
         Caption         =   "&Plate Scantlings"
      End
      Begin VB.Menu mnuPanelsBeamScantlings 
         Caption         =   "&Beam Scantlings"
      End
      Begin VB.Menu mnuPanelsDoubleHullScantlings 
         Caption         =   "&Double Hull Scantlings"
         Enabled         =   0   'False
      End
      Begin VB.Menu mnuPanelsBar0 
         Caption         =   "-"
      End
      Begin VB.Menu mnuPanelsBoundaryConditions 
         Caption         =   "Boundary &Conditions"
      End
      Begin VB.Menu mnuPanelsAddSymmBC 
         Caption         =   "Add Symmetry Axis #1 BC"
      End
      Begin VB.Menu mnuPanelsBar1 
         Caption         =   "-"
         Visible         =   0   'False
      End
   End
   Begin VB.Menu mnuLoads 
      Caption         =   "&Loads"
      Begin VB.Menu mnuLoadsLoadCases 
         Caption         =   "Load &Cases"
      End
      Begin VB.Menu mnuLoadsLoads 
         Caption         =   "&Loads"
      End
      Begin VB.Menu mnuLoadsLateralPressuers 
         Caption         =   "Lateral &Pressures"
         Visible         =   0   'False
         Begin VB.Menu mnuLoadsUniformelyDistributed 
            Caption         =   "&Uniformely Distributed"
         End
         Begin VB.Menu mnuLoadsStepwiseDistributed 
            Caption         =   "&Stepwise Distributed"
         End
      End
      Begin VB.Menu mnuLoadsLocalizedPressures 
         Caption         =   "Locali&zed Pressures"
         Visible         =   0   'False
      End
      Begin VB.Menu mnuLoadsBendingMoments 
         Caption         =   "Bending &Moments"
         Visible         =   0   'False
      End
      Begin VB.Menu mnuLoadsBar1 
         Caption         =   "-"
      End
      Begin VB.Menu mnuLoadsWrapPressures 
         Caption         =   "&Wrap Pressures"
      End
      Begin VB.Menu mnuLoadsRedistributeLoadsDH 
         Caption         =   "&Redistribute Loads (Double Hull)"
      End
   End
   Begin VB.Menu mnuOptimization 
      Caption         =   "&Optimization"
      Begin VB.Menu mnuProjectOptionsOptimization 
         Caption         =   "&Objective Function"
      End
      Begin VB.Menu mnuOptimizationBar2 
         Caption         =   "-"
      End
      Begin VB.Menu mnuOptimizationCost 
         Caption         =   "&Cost"
         Begin VB.Menu mnuOptimizationCostSimplified 
            Caption         =   "&Simplified"
         End
         Begin VB.Menu mnuOptimizationCostDevelopped 
            Caption         =   "&CostCAT"
            Begin VB.Menu mnuOptimizationCostDeveloppedCostCAtWizard 
               Caption         =   "Wizard"
            End
            Begin VB.Menu mnuOptimizationCostDeveloppedCostCAtAccesibility 
               Caption         =   "Workshop Coefficients"
            End
            Begin VB.Menu mnuOptimizationCostDeveloppedCostCAtTables 
               Caption         =   "Tables"
            End
         End
         Begin VB.Menu mnuOptimizationCostBar1 
            Caption         =   "-"
         End
         Begin VB.Menu mnuOptimizationCostMaterialCosts 
            Caption         =   "&Material Costs"
         End
      End
      Begin VB.Menu mnuOptimizationPlateRestrictions 
         Caption         =   "&Plate Restrictions"
         Visible         =   0   'False
      End
      Begin VB.Menu mnuOptimizationBeamRestrictions 
         Caption         =   "&Beam Restrictions"
         Visible         =   0   'False
      End
      Begin VB.Menu mnuOptimizationDoubleHullRestrictions 
         Caption         =   "&Double Hull Restrictions"
         Visible         =   0   'False
      End
      Begin VB.Menu mnuOptimizationBar0 
         Caption         =   "-"
         Visible         =   0   'False
      End
      Begin VB.Menu mnuOptimizationOldOptimizationModules 
         Caption         =   "&Optimization"
         Visible         =   0   'False
      End
      Begin VB.Menu mnuOptimizationBar1 
         Caption         =   "-"
      End
      Begin VB.Menu mnuOptimizationDesignVariables 
         Caption         =   "Design Variables"
      End
      Begin VB.Menu mnuOptimizationOldOptimizationModulesStructuralConstraints 
         Caption         =   "Structural Constraints"
      End
      Begin VB.Menu mnuOptimizationOldOptimizationModulesGeometricalConstraints 
         Caption         =   "Geometrical Constraints"
      End
      Begin VB.Menu mnuOptimizationOldOptimizationModulesEqualityRestrictions 
         Caption         =   "Equality Restrictions"
      End
      Begin VB.Menu mnuOptimizationGlobalConstraints 
         Caption         =   "Global Constraints"
      End
   End
   Begin VB.Menu mnuTools 
      Caption         =   "&Tools"
      Begin VB.Menu mnuToolsDistBtnNodes 
         Caption         =   "&Ruler"
      End
      Begin VB.Menu mnuToolsBar0 
         Caption         =   "-"
      End
      Begin VB.Menu mnuToolsCheckModel 
         Caption         =   "&Check Model"
         Visible         =   0   'False
      End
      Begin VB.Menu mnuModelStatistics 
         Caption         =   "&Model Statistics "
      End
      Begin VB.Menu mnuToolsSnapshot 
         Caption         =   "&Snapshot"
      End
      Begin VB.Menu mnuToolsBar1 
         Caption         =   "-"
      End
      Begin VB.Menu mnuToolsHullGirder 
         Caption         =   "&Hull Girder"
      End
      Begin VB.Menu mnuToolsWeightGravityCenter 
         Caption         =   "&Geometric Properties"
      End
      Begin VB.Menu mnuToolsBar2 
         Caption         =   "-"
      End
      Begin VB.Menu mnuTools3DView 
         Caption         =   "3D View"
      End
      Begin VB.Menu mnuToolsBar3 
         Caption         =   "-"
      End
      Begin VB.Menu mnuToolsImageViewer 
         Caption         =   "Image &Viewer"
         Visible         =   0   'False
      End
      Begin VB.Menu mnuToolsCalculator 
         Caption         =   "&Calculator"
      End
      Begin VB.Menu mnuToolsBar4 
         Caption         =   "-"
      End
      Begin VB.Menu mnuToolsOptions 
         Caption         =   "&Options..."
         Enabled         =   0   'False
      End
   End
   Begin VB.Menu mnuAnalysis 
      Caption         =   "&Analysis"
      Begin VB.Menu mnuAnalysisSolver 
         Caption         =   "Sol&ver"
         Begin VB.Menu mnuAnalysisSolverLBR4 
            Caption         =   "LBR-4"
         End
         Begin VB.Menu mnuAnalysisSolverBeamTheory 
            Caption         =   "Beam Theory"
         End
      End
      Begin VB.Menu mnuAnalysisBar0 
         Caption         =   "-"
      End
      Begin VB.Menu mnuAnalysisSolve 
         Caption         =   "&Solve"
      End
      Begin VB.Menu mnuAnalysisMultiSolver 
         Caption         =   "Multiple Solve"
      End
      Begin VB.Menu mnuAnalysisBar1 
         Caption         =   "-"
      End
      Begin VB.Menu mnuAnalysisBuildSolution 
         Caption         =   "&Build Solution"
         Visible         =   0   'False
      End
      Begin VB.Menu mnuAnalysisBar2 
         Caption         =   "-"
         Visible         =   0   'False
      End
      Begin VB.Menu mnuAnalysisOutputFiles 
         Caption         =   "Output &Files"
         Begin VB.Menu mnuAnalysisOutputFilesSolution1 
            Caption         =   "Solution &1"
         End
         Begin VB.Menu mnuAnalysisOutputFilesSolution2 
            Caption         =   "Solution &2"
         End
         Begin VB.Menu mnuAnalysisOutputFilesSolution3 
            Caption         =   "Solution &3"
         End
         Begin VB.Menu mnuAnalysisOutputFilesOptimization 
            Caption         =   "&Optimization"
         End
         Begin VB.Menu mnuAnalysisOutputFilesBar1 
            Caption         =   "-"
         End
         Begin VB.Menu mnuAnalysisInitialScantlings 
            Caption         =   "Initial Scantlings"
         End
         Begin VB.Menu mnuAnalysisFinalScantlings 
            Caption         =   "Final Scantlings"
         End
      End
      Begin VB.Menu mnuAnalysisOutputGraphs 
         Caption         =   "Output &Graphs"
         Begin VB.Menu mnuAnalysisOutputGraphsGeneralSolution 
            Caption         =   "General &Solution"
         End
         Begin VB.Menu mnuAnalysisOutputGraphsBar0 
            Caption         =   "-"
         End
         Begin VB.Menu mnuAnalysisOutputGraphsObjectiveFunctionVariation 
            Caption         =   "&Objective Function Variation"
         End
         Begin VB.Menu mnuAnalysisOutputGraphsGravityCenterVariation 
            Caption         =   "&Gravity Center Variation"
         End
         Begin VB.Menu mnuAnalysisOutputGraphsDesignVariablesVariation 
            Caption         =   "&Design Variables Variation"
         End
      End
      Begin VB.Menu mnuAnalysisOutputScantlings 
         Caption         =   "Output Scantlings"
         Enabled         =   0   'False
         Index           =   0
         Visible         =   0   'False
      End
      Begin VB.Menu mnuAnalysisBar3 
         Caption         =   "-"
      End
      Begin VB.Menu mnuAnalysisDeleteSolution 
         Caption         =   "&Delete Solution"
      End
   End
   Begin VB.Menu mnuWindow 
      Caption         =   "&Window"
      WindowList      =   -1  'True
      Begin VB.Menu mnuWindowNewWindow 
         Caption         =   "&New Window"
         Visible         =   0   'False
      End
      Begin VB.Menu mnuWindowBar0 
         Caption         =   "-"
         Visible         =   0   'False
      End
      Begin VB.Menu mnuWindowCascade 
         Caption         =   "&Cascade"
      End
      Begin VB.Menu mnuWindowTileHorizontal 
         Caption         =   "Tile &Horizontal"
      End
      Begin VB.Menu mnuWindowTileVertical 
         Caption         =   "Tile &Vertical"
      End
      Begin VB.Menu mnuWindowArrangeIcons 
         Caption         =   "&Arrange Icons"
      End
   End
   Begin VB.Menu mnuHelp 
      Caption         =   "&Help"
      Begin VB.Menu mnuHelpContents 
         Caption         =   "&Contents"
      End
      Begin VB.Menu mnuHelpTutorial 
         Caption         =   "&Tutorial"
      End
      Begin VB.Menu mnuHelpSearchForHelpOn 
         Caption         =   "&Search For Help On..."
         Visible         =   0   'False
      End
      Begin VB.Menu mnuHelpBar0 
         Caption         =   "-"
      End
      Begin VB.Menu mnuHelpAbout 
         Caption         =   "&About"
      End
   End
   Begin VB.Menu mnuPopup 
      Caption         =   "Popup"
      Visible         =   0   'False
      Begin VB.Menu mnuPopupProperties 
         Caption         =   "Properties"
         Begin VB.Menu mnuPopupPropertiesProperties 
            Caption         =   "Properties"
         End
      End
   End
End
Attribute VB_Name = "frmProject"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Dim X_BUFFER As Single, Y_BUFFER As Single
Dim Border_Height_Buffer As Long, Border_Width_Buffer As Long
Dim Border_Left_Buffer As Long, Border_Top_Buffer As Long
Private mvarcRectWND As cRectWND
Private WithEvents clsMouseWheel As mswhl.CMouseWheel
Attribute clsMouseWheel.VB_VarHelpID = -1

'Dim wheel As New MouseWheel.CMouseWheel

Public Property Get cRectWND() As cRectWND
    On Error GoTo cRectWNDGetErr
    If mvarcRectWND Is Nothing Then
        Set mvarcRectWND = New cRectWND
    End If
    Set cRectWND = mvarcRectWND
    Exit Property
cRectWNDGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmProject: cRectWND Property Get")
End Property

Public Property Set cRectWND(vData As cRectWND)
    On Error GoTo cRectWNDSetErr
    Set mvarcRectWND = vData
    Exit Property
cRectWNDSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmProject: cRectWND Property Set")
End Property

Private Sub Combo_Click()
    Select Case Combo.Tag
        Case "Mode"
            Combo.Visible = False
            Select Case Combo.Text
                Case "MODE= NORMAL"
                    setScreenMode NORMAL_MODE
                Case "MODE= SINGLE SELECTION"
                    setScreenMode SINGLE_SELECTION_MODE
                Case "MODE= ZOOM IN"
                    setScreenMode ZOOM_IN_MODE
                Case "MODE= ZOOM OUT"
                    setScreenMode ZOOM_OUT_MODE
                Case "MODE= ZOOM WIN"
                    setScreenMode ZOOM_WIN_MODE
                Case "MODE= PAN"
                    setScreenMode PAN_MODE
            End Select
            StatusBar.Panels.Item(2).Width = Combo.Width
        Case "LoadCase"
            StatusBar.Panels("LoadCase").Text = "LOAD CASE: " & Combo.Text
            Combo.Visible = False
            Project.Item(Me.Tag).cDisplaySettings.CurrentLoadCase = Combo.ListIndex ' + 1
            Draw ActiveProject
        Case "Solver"
            Project.Item(Me.Tag).CloseForms 0
            StatusBar.Panels("Solver").Text = Combo.Text
            Combo.Visible = False
            StatusBar.Panels.Item(2).Width = Combo.Width '+ 50
            Select Case Combo.Text
                Case "Solver = LBR-4"
                    Project.Item(Me.Tag).cHeader.IANA = 1
                    mnuAnalysisSolverLBR4.Checked = True
                    mnuAnalysisSolverBeamTheory.Checked = False
                Case "Solver = Beam Theory"
                    Project.Item(Me.Tag).cHeader.IANA = 2
                    mnuAnalysisSolverLBR4.Checked = False
                    mnuAnalysisSolverBeamTheory.Checked = True
            End Select
            NegociateIANAmenus
            
    End Select
End Sub

Private Sub Form_Deactivate()
    On Error Resume Next
   clsMouseWheel.SubClassUnHookForm
   Set clsMouseWheel.Form = Nothing
   Set clsMouseWheel = Nothing
End Sub

Private Sub Form_GotFocus()
    ActiveProject = CInt(Me.Tag)
End Sub

Private Sub Form_Load()
'    Set clsMouseWheel = New mswhl.CMouseWheel
'    Set clsMouseWheel.Form = Me
'    clsMouseWheel.SubClassHookForm
StatusBar.Visible = True
End Sub

Private Sub clsMouseWheel_MouseWheel(Cancel As Integer)
    Cancel = False
    ZoomWheel clsMouseWheel.Rotation, clsMouseWheel.XPos, clsMouseWheel.YPos
End Sub

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
    'Project.Item(Me.Tag).CloseForms 0
End Sub

Private Sub Form_Unload(Cancel As Integer)
    On Error GoTo 1
    Dim MSG As VbMsgBoxResult
    If Project.Item(Me.Tag).DataChanged = True And Licensing.LicenseLevel <> 7 Then
        MSG = MsgBox("Save changes on " & "'" & Project.Item(Me.Tag).sFileName & "'" & "?", vbYesNoCancel)
        Select Case MSG
            Case vbYes
                ActiveProject = Me.Tag
                ChDrive Left(GetFilePath(Project.Item(Me.Tag).sFileName), 3)
                ChDir GetFilePath(Project.Item(Me.Tag).sFileName)
                fMainForm.mnuFileSave_Click
                'Set Project.Item(Me.Tag) = Nothing
                
                Project.Remove (CInt(Me.Tag))
            Case vbNo
                'Set Project.Item(Me.Tag) = Nothing
                Project.Remove (CInt(Me.Tag))
            Case vbCancel
                Cancel = 1
                ActiveProject = Me.Tag
        End Select
    Else
        'Set Project.Item(CInt(Me.Tag)) = Nothing
        Project.Remove (CInt(Me.Tag))
    End If
    If Project.Count = 0 Then Toolbars_Initial
   
'   clsMouseWheel.SubClassUnHookForm
'   Set clsMouseWheel.Form = Nothing
'   Set clsMouseWheel = Nothing
Exit Sub
1:
MsgBox "Error!", vbCritical + vbOKOnly
End Sub

Private Sub mnuAnalysisBuildSolution_Click()
    Solution Me.Tag
End Sub

Private Sub mnuAnalysisDeleteSolution_Click()
    Project.Item(Me.Tag).cSolution.Solution = Empty
    Project.Item(Me.Tag).cSolution.IsSolution = False
    Draw Me.Tag
End Sub

Private Sub mnuAnalysisFinalScantlings_Click()
    If Project.Item(Me.Tag).cSolution.IsSolution = False Then
        MsgBox "'General Solution' must be performed to have acces to the final scantlings.", vbInformation + vbOKOnly
    Else
        ViewFinalScantlings Me.Tag
    End If
End Sub

Private Sub mnuAnalysisInitialScantlings_Click()
    ViewInitialScantlings Me.Tag
End Sub

Private Sub mnuAnalysisOutputFilesOptimization_Click()
    ViewOptimization Project.Item(Me.Tag).sFileName
End Sub

Private Sub mnuAnalysisOutputFilesSolution1_Click()
    ViewSolution1 Project.Item(Me.Tag).sFileName
End Sub

Private Sub mnuAnalysisOutputFilesSolution2_Click()
    ViewSolution2 Project.Item(Me.Tag).sFileName
End Sub

Private Sub mnuAnalysisOutputFilesSolution3_Click()
    ViewSolution3 Project.Item(Me.Tag).sFileName
End Sub

Public Sub mnuAnalysisOutputGraphsDesignVariablesVariation_Click()
    Project.Item(Me.Tag).old_frmSensitivities.Show vbModeless, fMainForm
End Sub

Public Sub mnuAnalysisOutputGraphsGeneralSolution_Click()
    Project.Item(Me.Tag).CloseForms 0
    Dim MSG As VbMsgBoxResult
    If Project.Item(Me.Tag).colPanel.Count = 0 Then
        MsgBox "No Panels Defined.", vbInformation + vbOKOnly
        Exit Sub
    End If
    If Project.Item(Me.Tag).cHeader.colLoadCase.Count = 0 Then
        MsgBox "No Load Cases Defined.", vbInformation + vbOKOnly
        Exit Sub
    End If

    If Project.Item(Me.Tag).cSolution.IsSolution = False Then
        'MsgBox "Solution not available.", vbInformation + vbOKOnly
        MSG = MsgBox("Build Solution?", vbQuestion + vbYesNo)
        Select Case MSG
            Case vbYes
                Solution Me.Tag
            Case vbNo
                Exit Sub
        End Select
    End If
    Project.Item(Me.Tag).frmSolution.Show vbModeless, fMainForm
End Sub

Public Sub mnuAnalysisOutputGraphsGravityCenterVariation_Click()
    On Error Resume Next
'    Project.Item(Me.Tag).old_frmGraphGravityVar.Show vbModeless, fMainForm
    Project.Item(ActiveProject).frmChartGlobalRestrictions.Show , fMainForm
End Sub

Public Sub mnuAnalysisOutputGraphsObjectiveFunctionVariation_Click()
    On Error Resume Next
    'Project.Item(Me.Tag).old_frmGrphObjFct.Show vbModeless, fMainForm
    Project.Item(Me.Tag).frmChartObjectiveFunction.Show vbModeless, fMainForm
End Sub

Public Sub mnuAnalysisSolve_Click()
    Project.Item(Me.Tag).CloseForms 0
    Dim MSG As VbMsgBoxResult
    Dim sPath As String
    UpdatePanelConnections Me.Tag
    If IsDemo(ActiveProject) = True Then Exit Sub
    If CheckModel(Me.Tag) = True Then
        MsgBox "The model is not valid. Check the Modelling Assistant."
        Project.Item(Me.Tag).frmModellingAssistant.Show vbModeless, fMainForm
        Exit Sub
    End If
    Select Case Project.Item(Me.Tag).cHeader.IANA
        Case 1
            MSG = MsgBox("Save model and perform analysis (Solver = LBR-4)?", vbQuestion + vbYesNo)
        Case 2
            MSG = MsgBox("Save model and perform analysis (Solver = Beam Theory)?", vbQuestion + vbYesNo)
    End Select
    Select Case MSG
        Case vbYes
            Dim sFile As String
'            If Project.Item(ActiveProject).bNewProjectFirstSave = True Then
'                With dlgCommonDialog
'                    .DialogTitle = "Save As"
'                    .CancelError = True
'                    'ToDo: set the flags and attributes of the common dialog control
'                    .Filter = "LBR-5 Project Files (*.lbr)|*.lbr|LBR-5 Data Files (*.txt)|*.txt"
'                    .FilterIndex = 1
'                    .FileName = GetFileRoot(Project.Item(ActiveProject).sFileName)
'                    .flags = &H2
'                    .InitDir = Project.Item(ActiveProject).sFileName
'
'                    .ShowSave
'                    If Len(.FileName) = 0 Then
'                        Exit Sub
'                    End If
'                    Project.Item(Me.Tag).sFileName = GetFilePath(.FileName) & GetFileRoot(.FileName) & ".lbr"
'                End With
'            End If
'
'            sPath = GetFilePath(Project.Item(Me.Tag).sFileName)
'            ChDrive Left(sPath, 3)
'            ChDir sPath
            
            sFile = GetFileRoot(Project.Item(Me.Tag).sFileName) & ".txt"
            sFile = GetFilePath(Project.Item(Me.Tag).sFileName) & sFile
            Dim fil, ts As TextStream
            Set fil = CreateObject("Scripting.FileSystemObject")
            Set ts = fil.OpenTextFile(sFile, ForWriting, TristateUseDefault)

            Project.Item(Me.Tag).WriteLBR5txtFile ts
            ts.Close
            Set fil = Nothing
            Set ts = Nothing
            Project.Item(Me.Tag).SaveAsLBR5ASCIIFile Project.Item(ActiveProject).sFileName, True
            
'            Project.Item(Me.Tag).sFileName = sFile
'            Project.Item(Me.Tag).frmProject.Caption = GetFileName(sFile)
            
            Project.Item(Me.Tag).cSolution.IsSolution = False
            Project.Item(Me.Tag).DataChanged = False
            Project.Item(Me.Tag).SolveProject
        Case vbNo
    End Select
End Sub

Public Sub mnuAnalysisMultiSolver_Click()
    Project.Item(Me.Tag).CloseForms 0
    Dim MSG As VbMsgBoxResult
    MSG = MsgBox("Save models and perform multiple analysis?", vbQuestion + vbYesNo)
    Select Case MSG
        Case vbYes
            frmMultiSolve.Show vbModeless, fMainForm
        Case vbNo
    End Select
End Sub

Private Sub mnuAnalysisSolverBeamTheory_Click()
    mnuAnalysisSolverLBR4.Checked = False
    mnuAnalysisSolverBeamTheory.Checked = True
    Project.Item(Me.Tag).cHeader.IANA = 2
    NegociateIANAmenus
    Me.StatusBar.Panels("Solver").Text = "Solver = Beam Theory"
End Sub

Private Sub mnuAnalysisSolverLBR4_Click()
    mnuAnalysisSolverLBR4.Checked = True
    mnuAnalysisSolverBeamTheory.Checked = False
    Project.Item(Me.Tag).cHeader.IANA = 1
    NegociateIANAmenus
    Me.StatusBar.Panels("Solver").Text = "Solver = LBR-4"
End Sub

Private Sub mnuFileImportFromAvproLbr5TransfertFile_Click()
    fMainForm.mnuFileImportFromAvproLbr5TransfertFile_Click
End Sub

Private Sub mnuFileImportFromMarsLbr5TransfertFile_Click()
    fMainForm.mnuFileImportFromMarsLbr5TransfertFile_Click
End Sub

Private Sub mnuFileMRU_Click(index As Integer)
    fMainForm.mnuFileMRU_Click index
End Sub

Private Sub mnuHelpTutorial_Click()
    fMainForm.mnuHelpTutorial_Click
End Sub

Private Sub mnuLoadsLoadCases_Click()
    Project.Item(Me.Tag).CloseForms 0
    Project.Item(Me.Tag).frmLoadCases.Show vbModeless, fMainForm
End Sub

Public Sub mnuLoadsLoads_Click()
    Project.Item(Me.Tag).CloseForms 0
    If Project.Item(Me.Tag).cHeader.colLoadCase.Count = 0 Then
        MsgBox "No Load Cases Defined.", vbInformation + vbOKOnly
        Exit Sub
    End If
    If Project.Item(Me.Tag).colPanel.Count = 0 Then
        MsgBox "No Panels Defined.", vbInformation + vbOKOnly
        Exit Sub
    End If
    Project.Item(Me.Tag).frmLoads.Show vbModeless, fMainForm
End Sub

Private Sub mnuLoadsRedistributeLoadsDH_Click()
    If Project.Item(Me.Tag).colPanel.GetNoOfPlates >= 2 Then
        NegotiateModes
        setFunctionMode REDISTRIBUTE_LOADS_DH_FIRST_PANEL
    Else
        MsgBox "At least two plates are needed to redistribute pressures.", vbInformation + vbOKOnly
    End If
End Sub

Private Sub mnuLoadsWrapPressures_Click()
    Project.Item(Me.Tag).CloseForms 0
    If Project.Item(Me.Tag).cHeader.colLoadCase.Count < 2 Then
        MsgBox "At least two load cases are needed to wrap pressures.", vbInformation + vbOKOnly
        Exit Sub
    Else
        Project.Item(Me.Tag).frmWrapPressures.Show vbModeless, fMainForm
    End If
End Sub

Public Sub mnuModelAddBeams_Click()
    Project.Item(Me.Tag).CloseForms 0
    If Project.Item(Me.Tag).colPanel.Count >= Licensing.MAX_PANELS Then
        MsgBox "Maximum number of panels is " & Licensing.MAX_PANELS, vbCritical + vbOKOnly, "LBR-5 License Limitation"
        Exit Sub
    End If
    If Project.Item(Me.Tag).colNodes.Count >= 2 Then
        NegotiateModes
        setFunctionMode ADD_BEAM_FIRST_NODE_FUNCTION
    Else
        MsgBox "At least two nodes are needed to create a beam.", vbInformation + vbOKOnly
    End If
End Sub

Public Sub mnuModelAddDoubleHulls_Click()
    Project.Item(Me.Tag).CloseForms 0
    If Project.Item(Me.Tag).colPanel.GetNoOfPlates >= 2 Then
        NegotiateModes
        setFunctionMode ADD_DOUBLEHULL_FIRST_PANEL_FUNCTION
    Else
        MsgBox "At least two plates are needed to create a double hull.", vbInformation + vbOKOnly
    End If

End Sub

Public Sub mnuModelDeletePanel_Click()
    
    Project.Item(Me.Tag).CloseForms 0
    setScreenMode NORMAL_MODE
    If Project.Item(Me.Tag).colPanel.Count = 0 Then
        MsgBox "No Panels Defined.", vbInformation + vbOKOnly
        Exit Sub
    End If
    Project.Item(Me.Tag).frmDeletePanel.Show vbModeless, fMainForm
End Sub

Public Sub mnuModelDividePanel_Click()
    Project.Item(Me.Tag).CloseForms 0
    Select Case Project.Item(Me.Tag).colPanel.Count
        Case 0
            MsgBox "No Panels Defined.", vbInformation + vbOKOnly
        Case Is >= Licensing.MAX_PANELS
            MsgBox "Maximal number of panels is " & Licensing.MAX_PANELS, vbCritical + vbOKOnly, "LBR-5 License Limitation"
        Case Else
            NegotiateModes
            setFunctionMode DIVIDE_PANEL_FUNCTION
    End Select
End Sub

Public Sub mnuModelExplodeDoubleHull_Click()
    Project.Item(Me.Tag).CloseForms 0
    If Project.Item(Me.Tag).colPanel.GetNoOfDoubleHulls = 0 Then
        MsgBox "No Double Hulls defined.", vbInformation + vbOKOnly
        Exit Sub
    End If
    NegotiateModes
    setFunctionMode EXPLODE_DOUBLEHULL_FUNCTION
End Sub

Public Sub mnuModelMatchProperties_Click()
    Project.Item(Me.Tag).CloseForms 0
    Select Case Project.Item(Me.Tag).colPanel.Count
        Case Is < 2
            MsgBox "At least two panels must be defined.", vbInformation + vbOKOnly
        Case Else
            NegotiateModes
            setFunctionMode MATCH_PROPERTIES_FUNCTION
    End Select
End Sub

Private Sub mnuModelMoveOrigin_Click()
    Project.Item(Me.Tag).CloseForms 0
    Project.Item(Me.Tag).frmMoveOrigin.Show vbModeless, fMainForm
End Sub

Public Sub mnuModelNodes_Click()
    Project.Item(Me.Tag).CloseForms 0
    Project.Item(Me.Tag).frmNodes.Show vbModeless, fMainForm
End Sub

Public Sub mnuModelAddPlates_Click()
    Project.Item(Me.Tag).CloseForms 0
    If Project.Item(Me.Tag).colPanel.Count >= Licensing.MAX_PANELS Then
        MsgBox "Maximum number of panels is " & Licensing.MAX_PANELS, vbCritical + vbOKOnly, "LBR-5 License Limitation"
        Exit Sub
    End If
    If Project.Item(Me.Tag).colNodes.Count >= 2 Then
        NegotiateModes
        setFunctionMode ADD_PLATE_FIRST_NODE_FUNCTION
    Else
        MsgBox "At least two nodes are needed to create a plate.", vbInformation + vbOKOnly
    End If
End Sub

Public Sub mnuModelSortPanels_Click()
    Project.Item(Me.Tag).CloseForms 0
    If Project.Item(Me.Tag).colPanel.Count > 1 Then
        Project.Item(Me.Tag).frmSortPanels.Show vbModeless, fMainForm
    Else
        MsgBox "At least two panels must be defined.", vbInformation + vbOKOnly
    End If
End Sub

Private Sub mnuModelStatistics_Click()
    Dim s As String
    Dim iplates As Integer, ibeams As Integer, ivirtuals As Integer
    GetNoOfPlates CInt(Me.Tag), iplates, ibeams, ivirtuals
    s = "Number of plates: " & iplates & vbCrLf
    s = s + "Number of beams: " & ibeams & vbCrLf
    s = s + "Number of virtual panels: " & ivirtuals & vbCrLf
    s = s + "Total number of panels: " & iplates + ibeams + ivirtuals & vbCrLf
    s = s + "Number of design variables: " & GetTotalNumberOfDesVar(CInt(Me.Tag)) & vbCrLf
    s = s + "Number of structural constraints: " & GetTotalNumberOfStrConstr(CInt(Me.Tag)) & vbCrLf
    s = s + "Number of geometrical constraints: " & GetTotalNumberOfGeoConstr(CInt(Me.Tag)) & vbCrLf
    s = s + "Number of equality restrictions: " & GetTotalNumberOfEqRestr(CInt(Me.Tag)) & vbCrLf
    
    MsgBox s, vbInformation + vbOKOnly, "Model Statistics - [" & GetFileName(Project.Item(Me.Tag).sFileName) & "]"
End Sub

Private Sub mnuOptimizationCostDeveloppedCostCAtAccesibility_Click()
    Project.Item(Me.Tag).CloseForms 0
    Project.Item(Me.Tag).frmCostCAtWorkshop.Show vbModeless, fMainForm
End Sub

Private Sub mnuOptimizationCostDeveloppedCostCAtTables_Click()
    Project.Item(Me.Tag).CloseForms 0
    If Project.Item(Me.Tag).colPanel.GetNoOfPlates > 0 Then
    Project.Item(Me.Tag).frmCostCAtTables.Show 'vbModeless, fMainForm
    Else
        MsgBox "No Plate Panels Defined.", vbInformation + vbOKOnly
    End If
End Sub

Private Sub mnuOptimizationCostDeveloppedCostCAtWizard_Click()
    Project.Item(Me.Tag).CloseForms 0
    If Project.Item(Me.Tag).colPanel.GetNoOfPlates > 0 Then
        Project.Item(Me.Tag).frmCostCAt.Show vbModeless, fMainForm
    Else
        MsgBox "No Plate Panels Defined.", vbInformation + vbOKOnly
    End If
End Sub

Private Sub mnuOptimizationCostMaterialCosts_Click()
    Project.Item(Me.Tag).CloseForms 0
    Project.Item(Me.Tag).frmMaterialCosts.Show vbModeless, fMainForm
End Sub

Private Sub mnuOptimizationCostSimplified_Click()
    Project.Item(Me.Tag).CloseForms 0
    Project.Item(Me.Tag).frmCostSimplified.Show vbModeless, fMainForm
End Sub

Private Sub mnuOptimizationDesignVariables_Click()
    Project.Item(Me.Tag).CloseForms 0
    If Project.Item(Me.Tag).colPanel.Count = 0 Then
        MsgBox "No Panels Defined.", vbInformation + vbOKOnly
        Exit Sub
    End If
    Project.Item(Me.Tag).frmDesignVariables.Show vbModeless, fMainForm
End Sub

'Private Sub mnuOptimizationOldOptimizationModulesDesignVariables_Click()
'    Project.Item(Me.Tag).CloseForms 0
'    If Project.Item(Me.Tag).colPanel.Count = 0 Then
'        MsgBox "No Panels Defined.", vbInformation + vbOKOnly
'        Exit Sub
'    End If
'    Project.Item(Me.Tag).old_FrmOpti1.Show vbModeless, fMainForm
'End Sub

Private Sub mnuOptimizationOldOptimizationModulesEqualityRestrictions_Click()
    Project.Item(Me.Tag).CloseForms 0
    If Project.Item(Me.Tag).colPanel.Count = 0 Then
        MsgBox "No Panels Defined.", vbInformation + vbOKOnly
        Exit Sub
    End If
    Project.Item(Me.Tag).old_frmEqConstr1.Show vbModeless, fMainForm
End Sub

Private Sub mnuOptimizationOldOptimizationModulesGeometricalConstraints_Click()
    Project.Item(Me.Tag).CloseForms 0
    If Project.Item(Me.Tag).colPanel.Count = 0 Then
        MsgBox "No Panels Defined.", vbInformation + vbOKOnly
        Exit Sub
    End If
    Project.Item(Me.Tag).old_FrmOpti3_1.Show vbModeless, fMainForm
End Sub

Private Sub mnuOptimizationGlobalConstraints_Click()
    Project.Item(Me.Tag).CloseForms 0
    If Project.Item(Me.Tag).colPanel.Count = 0 Then
        MsgBox "No Panels Defined.", vbInformation + vbOKOnly
        Exit Sub
    End If
    Project.Item(Me.Tag).frmOptiGlobal.Show vbModeless, fMainForm
    'Project.Item(Me.Tag).old_frmGlobal.Show vbModeless, fMainForm
End Sub

'Private Sub mnuOptimizationOldOptimizationModulesGravityCenter_Click()
'    Project.Item(Me.Tag).CloseForms 0
'    If Project.Item(Me.Tag).colPanel.Count = 0 Then
'        MsgBox "No Panels Defined.", vbInformation + vbOKOnly
'        Exit Sub
'    End If
'    Project.Item(Me.Tag).frmOptiGlobal.Show vbModeless, fMainForm
'    'Project.Item(Me.Tag).old_frmGlobal.Show vbModeless, fMainForm
'End Sub

Private Sub mnuOptimizationOldOptimizationModulesStructuralConstraints_Click()
    Project.Item(Me.Tag).CloseForms 0
    If Project.Item(Me.Tag).colPanel.Count = 0 Then
        MsgBox "No Panels Defined.", vbInformation + vbOKOnly
        Exit Sub
    End If
    If Project.Item(Me.Tag).cHeader.colLoadCase.Count = 0 Then
        MsgBox "No Load Cases Defined.", vbInformation + vbOKOnly
        Exit Sub
    End If
    Project.Item(Me.Tag).old_frmOpti2.Show vbModeless, fMainForm
End Sub

Private Sub mnuOptimizationPlateRestrictions_Click()
    If Project.Item(Me.Tag).colPanel.GetNoOfPlates > 0 Then
        If Project.Item(Me.Tag).cHeader.colLoadCase.Count > 0 Then
            Project.Item(Me.Tag).frmRestrictions.Show vbModeless, fMainForm
        Else
            MsgBox "No Load Cases Defined.", vbInformation + vbOKOnly
        End If
    Else
        MsgBox "No Plates Defined.", vbInformation + vbOKOnly
    End If
End Sub

Private Sub mnuPanelsAddSymmBC_Click()
    setFunctionMode ADD_BOUNDARY
End Sub

Public Sub mnuPanelsBeamScantlings_Click()
    Project.Item(Me.Tag).CloseForms 0
    If Project.Item(Me.Tag).colPanel.GetNoOfBeams > 0 Then
        Project.Item(Me.Tag).frmBeamScantlings.Show vbModeless, fMainForm
    Else
        MsgBox "No Beams Defined.", vbInformation + vbOKOnly
    End If
End Sub

Public Sub mnuPanelsBoundaryConditions_Click()
    Project.Item(Me.Tag).CloseForms 0
    If IsBoundaries = True Then
        Project.Item(Me.Tag).frmBoundaryConditions.Show vbModeless, fMainForm
    Else
        MsgBox "Boundary conditions may be defined only on uncoupled panels.", vbInformation + vbOKOnly
    End If
End Sub

Public Sub mnuPanelsDoubleHullScantlings_Click()
    Project.Item(Me.Tag).CloseForms 0
    If Project.Item(Me.Tag).colPanel.GetNoOfDoubleHulls > 0 Then
        Project.Item(Me.Tag).frmDoubleHullScantlings.Show vbModeless, fMainForm
    Else
        MsgBox "No Double Hull Panels Defined.", vbInformation + vbOKOnly
    End If
End Sub

Public Sub mnuPanelsPlateScantlings_Click()
    Project.Item(Me.Tag).CloseForms 0
    If Project.Item(Me.Tag).colPanel.GetNoOfPlates > 0 Then
        Project.Item(Me.Tag).frmPlateScantlings.Show vbModeless, fMainForm
    Else
        MsgBox "No Plates Defined.", vbInformation + vbOKOnly
    End If
End Sub

Public Sub mnuModelReversePanels_Click()
    Project.Item(Me.Tag).CloseForms 0
    Dim cPanel As cPanel, bSelected As Boolean
    If Project.Item(ActiveProject).colPanel.Count = 0 Then
        MsgBox "No Panels Defined.", vbInformation + vbOKOnly
        Exit Sub
    End If
    Project.Item(Me.Tag).frmReversePanel.Show vbModeless, fMainForm
    
'    bSelected = False
'    For Each cPanel In Project.Item(Me.Tag).colPanel
'        If cPanel.Selected = isSelected Then
'            Select Case cPanel.pType
'                Case DoubleHull
'                    MsgBox "Double Hull Panels cannot be reversed.", vbCritical + vbOKOnly
'                Case Else
'                    bSelected = True
'                    cPanel.Reverse
'                    Project.Item(Me.Tag).DataChanged = True
'                End Select
'        End If
'    Next cPanel
'    If bSelected = False Then
'        NegotiateModes
'        setFunctionMode REVERSE_PANEL_FUNCTION
'    Else
'        setScreenMode NORMAL_MODE
'    End If
End Sub

Private Sub mnuPopupPropertiesProperties_Click()
    frmDisplayOptions.Show vbModeless, fMainForm
End Sub

Private Sub mnuProjectOptionsDeadweight_Click()
    Project.Item(Me.Tag).CloseForms 0
    Project.Item(Me.Tag).frmDeadweight.Show vbModeless, fMainForm
End Sub

Private Sub mnuProjectOptionsFourierSeries_Click()
    Project.Item(Me.Tag).CloseForms 0
    Project.Item(Me.Tag).frmFourierSeries.Show vbModeless, fMainForm
End Sub

Private Sub mnuProjectOptionsOptimization_Click()
    Project.Item(Me.Tag).CloseForms 0
    Project.Item(Me.Tag).frmOptimization.Show vbModeless, fMainForm
End Sub

Private Sub mnuProjectOptionsOutput_Click()
    Project.Item(Me.Tag).CloseForms 0
    Project.Item(Me.Tag).frmOutput.Show vbModeless, fMainForm
End Sub

Private Sub mnuProjectOptionsOverallSpan_Click()
    Project.Item(Me.Tag).CloseForms 0
    Project.Item(Me.Tag).frmOverallSpan.Show vbModeless, fMainForm
End Sub

Private Sub mnuProjectOptionsRuleLength_Click()
    Project.Item(Me.Tag).CloseForms 0
    Project.Item(Me.Tag).frmRuleLength.Show vbModeless, fMainForm
End Sub

Private Sub mnuProjectTitle_Click()
    Project.Item(Me.Tag).CloseForms 0
    Project.Item(Me.Tag).frmTitle.Show vbModeless, fMainForm
End Sub

Private Sub mnuTools3DView_Click()
    On Error Resume Next
    Dim i As Integer
    For i = 1 To Project.Count
        Unload Project.Item(i).frmOpenGL
    Next i
    Project.Item(Me.Tag).frmOpenGL.Show
End Sub

Private Sub mnuToolsCalculator_Click()
    Dim sFile As String
    Dim ShellCalculator As Long
    
    ShellCalculator = ShellExecute(0, "open", "calc.exe", "", "", SW_SHOWNORMAL)

End Sub

Private Sub mnuToolsCheckModel_Click()
    UpdatePanelConnections Me.Tag
    If CheckModel(Me.Tag) = False Then MsgBox "All tests passed.", vbInformation + vbOKOnly
End Sub

Private Sub mnuToolsDistBtnNodes_Click()
    If Project.Item(Me.Tag).colNodes.Count < 2 Then
        MsgBox "Minimum 2 Nodes Required.", vbInformation + vbOKOnly
        Exit Sub
    End If
    NegotiateModes
    setFunctionMode GET_DIST_FIRST_NODE_FUNCTION
End Sub

Private Sub mnuToolsHullGirder_Click()
    Project.Item(Me.Tag).CloseForms 0
    If Project.Item(Me.Tag).colPanel.Count = 0 Then
        MsgBox "No Panels Defined.", vbInformation + vbOKOnly
        Exit Sub
    End If
        If Project.Item(Me.Tag).cHeader.colLoadCase.Count = 0 Then
        MsgBox "No Load Cases Defined.", vbInformation + vbOKOnly
        Exit Sub
    End If
    Project.Item(Me.Tag).frmHull_Girder.Show vbModeless, fMainForm
End Sub

Private Sub mnuToolsImageViewer_Click()
    frmImages.Show vbModeless, fMainForm
End Sub

Private Sub mnuToolsSnapshot_Click()
    On Error Resume Next
    With dlgCommonDialog
        .DialogTitle = "Snapshot"
        .CancelError = True
        .Filter = "Picture Files (*.bmp)|*.bmp"
        .FileName = ""
        .ShowSave
        If Len(.FileName) = 0 Then
            Exit Sub
        End If
        SavePicture Me.picScreen.Image, .FileName
    End With
End Sub

Private Sub mnuHelpAbout_Click()
    fMainForm.mnuHelpAbout_Click
End Sub

Private Sub mnuHelpSearchForHelpOn_Click()
    fMainForm.mnuHelpSearchForHelpOn_Click
End Sub

Private Sub mnuHelpContents_Click()
    fMainForm.mnuHelpContents_Click
End Sub

Private Sub mnuToolsWeightGravityCenter_Click()
    Project.Item(ActiveProject).frmGeometricProperties.Show vbModeless, fMainForm
End Sub

Private Sub mnuViewDisplayOptions_Click()
    Project.Item(ActiveProject).frmDisplayOptions.Show vbModeless, fMainForm
End Sub

Private Sub mnuViewToolbarsGeometry_Click()
    tb_Geometry.FDPane1.PaneVisible = Not tb_Geometry.FDPane1.PaneVisible
End Sub

Public Sub mnuViewToolbarsSolver_Click()
    tb_Solver.FDPane1.PaneVisible = Not tb_Solver.FDPane1.PaneVisible
End Sub

Private Sub mnuViewToolbarsStandard_Click()
    tb_Standard.FDPane1.PaneVisible = Not tb_Standard.FDPane1.PaneVisible
End Sub

Private Sub mnuViewToolbarsTools_Click()
    tb_Tools.FDPane1.PaneVisible = Not tb_Tools.FDPane1.PaneVisible
End Sub

Private Sub mnuViewToolbarsView_Click()
    tb_View.FDPane1.PaneVisible = Not tb_View.FDPane1.PaneVisible
End Sub

Private Sub mnuWindowArrangeIcons_Click()
    fMainForm.Arrange vbArrangeIcons
End Sub

Private Sub mnuWindowTileVertical_Click()
    fMainForm.Arrange vbTileVertical
End Sub

Private Sub mnuWindowTileHorizontal_Click()
    fMainForm.Arrange vbTileHorizontal
End Sub

Private Sub mnuWindowCascade_Click()
    fMainForm.Arrange vbCascade
End Sub

Private Sub mnuToolsOptions_Click()
    'ToDo: Add 'mnuToolsOptions_Click' code.
    MsgBox "Add 'mnuToolsOptions_Click' code."
End Sub

Private Sub mnuViewWebBrowser_Click()
    fMainForm.mnuViewWebBrowser_Click
End Sub

Private Sub mnuViewOptions_Click()
    'fMainForm.mnuViewOptions_Click
    'frmOptions.Show vbModeless, fMainForm
    'Project.Item(Me.Tag).frmOptions.Show vbModeless, fMainForm
End Sub

Private Sub mnuViewRefresh_Click()
    'ToDo: Add 'mnuViewRefresh_Click' code.
    'MsgBox "Add 'mnuViewRefresh_Click' code."
    Draw ActiveProject
End Sub

Private Sub mnuViewStatusBar_Click()
    mnuViewStatusBar.Checked = Not mnuViewStatusBar.Checked
    'StatusBar.Visible = mnuViewStatusBar.Checked
    StatusBar.Visible = Not StatusBar.Visible
    Form_Resize
End Sub

Private Sub mnuEditPasteSpecial_Click()
    'ToDo: Add 'mnuEditPasteSpecial_Click' code.
    MsgBox "Add 'mnuEditPasteSpecial_Click' code."
End Sub

Private Sub mnuEditPaste_Click()
    On Error Resume Next
'    ActiveForm.rtfText.SelRTF = Clipboard.GetText
End Sub

Private Sub mnuEditCopy_Click()
    On Error Resume Next
'    Clipboard.SetText ActiveForm.rtfText.SelRTF

End Sub

Private Sub mnuEditCut_Click()
    On Error Resume Next
'    Clipboard.SetText ActiveForm.rtfText.SelRTF
'    ActiveForm.rtfText.SelText = vbNullString

End Sub

Private Sub mnuEditUndo_Click()
    'ToDo: Add 'mnuEditUndo_Click' code.
    MsgBox "Add 'mnuEditUndo_Click' code."
End Sub


Private Sub mnuFileExit_Click()
    'unload the form
    Unload fMainForm
End Sub

Private Sub mnuFileSend_Click()
    'ToDo: Add 'mnuFileSend_Click' code.
    'MsgBox "Add 'mnuFileSend_Click' code."
        On Error GoTo 1
    Dim conSwNormal As Long
    Dim sFile As String
    sFile = Project.Item(ActiveProject).sFileName
    ShellExecute hwnd, "open", "mailto:eugen.pircalabu@ulg.ac.be", vbNullString, vbNullString, conSwNormal
    Exit Sub
1
    MsgBox "Connection failed.", vbCritical + vbOKOnly
End Sub

Private Sub mnuFilePrint_Click()
    Dim backcolor As ColorConstants
    backcolor = Project.Item(Me.Tag).cDisplaySettings.ColorScreen
    Project.Item(Me.Tag).cDisplaySettings.ColorScreen = vbWhite
    Draw Me.Tag
    Print_ picScreen
    Project.Item(Me.Tag).cDisplaySettings.ColorScreen = backcolor
    
    Draw Me.Tag
    
End Sub

Private Sub mnuFilePrintPreview_Click()
    'ToDo: Add 'mnuFilePrintPreview_Click' code.
    MsgBox "Add 'mnuFilePrintPreview_Click' code."
End Sub

Private Sub mnuFilePageSetup_Click()
    On Error Resume Next
    With dlgCommonDialog
        .DialogTitle = "Page Setup"
        .CancelError = True
        .flags = &H40
        .ShowPrinter
    End With
End Sub

Private Sub mnuFileProperties_Click()
    Dim fso As New FileSystemObject
    Dim sFile As String
    Dim MSG As String
    Dim FileType As String
    sFile = Project.Item(ActiveProject).sFileName
    Dim FileName As String, FileVersion As String, ParentFolderName As String
    FileName = fso.GetFileName(sFile)
    'FileVersion = fso.GetFileVersion(sFile)
    ParentFolderName = fso.GetParentFolderName(sFile)
    Select Case fso.GetExtensionName(sFile)
        Case "txt"
            FileType = "LBR5 Text File"
        Case "lbr"
            FileType = "LBR5 Project File"
        Case "mlt"
            FileType = "MARS-LBR5 Transfert File"
        Case "xml"
            FileType = "XML File"
    End Select
    MSG = MsgBox("Name: " & vbTab & FileName & vbCrLf & _
            "Version: " & vbTab & Project.Item(ActiveProject).FileVersionNumber & vbCrLf & _
            "Path: " & vbTab & ParentFolderName & vbCrLf & _
            "Type: " & vbTab & FileType, vbInformation + vbOKOnly, "File Properties")
End Sub

Private Sub mnuFileOpenTargetFolder_Click()
    Dim sFilePath As String
    sFilePath = GetFilePath(Project.Item(ActiveProject).sFileName)
    Dim ShellFile As Long
    ShellFile = Shell(Windows_Path & "explorer.exe" & " " & sFilePath, vbNormalFocus)
End Sub

Private Sub mnuFileSaveAll_Click()
    fMainForm.mnuFileSaveAll_Click
End Sub

Private Sub mnuFileSaveAs_Click()
    fMainForm.mnuFileSaveAs_Click
End Sub

Private Sub mnuFileSave_Click()
    fMainForm.mnuFileSave_Click
End Sub

Private Sub mnuFileClose_Click()
    On Error Resume Next
    Unload Me
End Sub

Private Sub mnuFileOpen_Click()
    fMainForm.mnuFileOpen_Click
End Sub

Private Sub mnuFileNew_Click()
    fMainForm.mnuFileNew_Click
End Sub

Public Sub Form_Activate()
    ActiveProject = CInt(Me.Tag)
    ChDrive Left(Project.Item(Me.Tag).sFileName, 3)
    ChDir GetFilePath(Project.Item(Me.Tag).sFileName)
    'StatusBar.Panels("LoadCase").Text = "LOAD CASE: None"
    StatusBar.Panels("Solver").Width = 120
    If IsEmpty(Project.Item(Me.Tag).cSolution.Solution) = False Then
        POL = Project.Item(Me.Tag).cSolution.Solution
    End If
    Draw Me.Tag
    NegociateIANAmenus
    NegociateLICmenus
    Form_Resize
'    Set clsMouseWheel = New mswhl.CMouseWheel
'    Set clsMouseWheel.Form = Me
'    clsMouseWheel.SubClassHookForm
'    With StatusBar.Panels("Assistant")
'        .Text = "1000 Problems Found"
'
'    End With
    CheckModel (Me.Tag)
    
End Sub

Private Sub Form_Initialize()
'    On Error Resume Next
'    'Me.Icon = LoadResPicture("ID_ICON_LOGO", 1)
'    'Me.MousePointer = 99
'    MsgBox "Form_Initialize 1"
'    Me.MouseIcon = LoadResPicture("ID_CURSOR_NORMAL", 2)
'    MsgBox "Form_Initialize 2"
'    Me.MousePointer = vbCustom
'    MsgBox "Form_Initialize 3"
'    img.Picture = LoadPicture("C:\Documents and Settings\Eugen PIRCALABU\Bureau\Photo.JPG")
'    img.Picture = LoadPicture(App.Path & "\background.jpg")
End Sub

Private Sub Form_Resize()
    On Error Resume Next
    Combo.Visible = False
    With picScreen
        .Left = Me.ScaleLeft
        .Width = Me.ScaleWidth
        .Top = Me.ScaleTop
        If StatusBar.Visible = True Then
            .Height = Me.ScaleHeight - StatusBar.Height
        Else
            .Height = Me.ScaleHeight
        End If
    End With
'    img.Visible = True
'    img.Left = picScreen.ScaleLeft
'    img.Width = picScreen.ScaleWidth
'    img.Top = picScreen.ScaleTop
'    img.Height = picScreen.ScaleHeight
'    img.Stretch = True
'    img.Refresh
'    picScreen.PaintPicture img.Picture, img.Left, img.Top, img.Width, img.Height  ', _
    img.Visible = False
    'picScreen.Picture = img.Picture
    With ProgressBar
        .Left = Me.ScaleLeft + 2
        .Width = StatusBar.Panels(1).Width - 3
        .Top = Me.ScaleHeight - StatusBar.Height + 3
        .Height = StatusBar.Height - 5
    End With
    
    With lbAssistant
        .Left = StatusBar.Panels("Assistant").Left + 2
        .Width = StatusBar.Panels("Assistant").Width - 3
        .Top = Me.ScaleHeight - StatusBar.Height + 3
        .Height = StatusBar.Height - 5
    End With
    
End Sub

Private Sub NegotiateModes()
    Select Case Project.Item(Me.Tag).ScreenMode
        Case NORMAL_MODE
            Select Case Project.Item(Me.Tag).FunctionMode
                Case NO_FUNCTION
                    setScreenMode NORMAL_MODE
                Case Else
                    setFunctionMode NO_FUNCTION
                    setScreenMode NORMAL_MODE
            End Select
        Case Else
            Select Case Project.Item(Me.Tag).FunctionMode
                Case NO_FUNCTION
                    setScreenMode NORMAL_MODE
                Case Else
                    setScreenMode NORMAL_MODE
                    setFunctionMode Project.Item(Me.Tag).FunctionMode
            End Select
    End Select
End Sub


Private Sub Pic_Click()

End Sub

Private Sub picScreen_DblClick()
    Dim lpp As POINTAPI
    GetCursorPos lpp
    lpp.z = lpp.z - 70 'frmMain.CoolBar.Top
    Dim oPan As cPanel
    If Project.Item(Me.Tag).FunctionMode = NO_FUNCTION And Project.Item(Me.Tag).ScreenMode = NORMAL_MODE Then
        'Panel Properties
        For Each oPan In Project.Item(Me.Tag).colPanel
            If PtInRegion(oPan.Region, lpp.y, lpp.z) = 1 Then
                Select Case oPan.pType
                    Case Plate
                        'mnuPanelsPlateScantlings_Click
                        Project.Item(Me.Tag).frmPlateScantlings.Show vbModeless, fMainForm
                        Project.Item(Me.Tag).frmPlateScantlings.EntryWindow (oPan.index)
                    Case Beam
                        'mnuPanelsBeamScantlings_Click
                        Project.Item(Me.Tag).frmBeamScantlings.Show vbModeless, fMainForm
                        Project.Item(Me.Tag).frmBeamScantlings.EntryWindow (oPan.index)
                End Select
            End If
        Next oPan
    End If
   
End Sub

Private Sub picScreen_KeyDown(KeyCode As Integer, Shift As Integer)
    On Error Resume Next
    
    Select Case KeyCode
        Case 13 'Enter
        
        Case 27 'Escape
            'setScreenMode NORMAL_MODE
            Select Case Project.Item(Me.Tag).FunctionMode
                Case PANEL_TYPE_INNERDHULL, PANEL_TYPE_OUTERDHULL
                    Project.Item(Me.Tag).frmCostCAt.Show
                    AddPanelTypeDHull 0, 0, 4 ' 4 = Cancel
                    setFunctionMode NO_FUNCTION
                    setScreenMode NORMAL_MODE
                Case PANEL_TYPE_SIMPLE_SHELL
                    Project.Item(Me.Tag).frmCostCAt.Show
                    AddPanelTypeSimpleShell 0, 0, 2
                    setFunctionMode NO_FUNCTION
                    setScreenMode NORMAL_MODE
                Case PANEL_TYPE_BILGE
                    Project.Item(Me.Tag).frmCostCAt.Show
                    AddPanelTypeBilge 0, 0, 2
                    setFunctionMode NO_FUNCTION
                    setScreenMode NORMAL_MODE
                Case PANEL_TYPE_PRIMARY_LONGITUDINAL
                    Project.Item(Me.Tag).frmCostCAt.Show
                    AddPanelTypeGirder 0, 0, 2
                    setFunctionMode NO_FUNCTION
                    setScreenMode NORMAL_MODE
                Case ADD_NAPPE
                    Project.Item(Me.Tag).frmCostCAt.Show
                    AddNappe 0, 0, 2
                    setFunctionMode NO_FUNCTION
                    setScreenMode NORMAL_MODE
                Case PANEL_TYPE_VIRTUAL
                    Project.Item(Me.Tag).frmCostCAt.Show
                    AddPanelTypeVirtual 0, 0, 2
                    setFunctionMode NO_FUNCTION
                    setScreenMode NORMAL_MODE
            End Select
            NegotiateModes
        Case 17 'Control
            setScreenMode MULTIPLE_SELECTION_MODE
        Case 46 'Delete
            If Licensing.IS_DELETE_PANEL = False Then Exit Sub
            DeleteSelectedPanels Me.Tag
        Case 69 'E
            If Licensing.IS_DELETE_PANEL = False Then Exit Sub
            setScreenMode NORMAL_MODE
            setFunctionMode ERASE_MODE_FUNCTION
        Case 88 'X
            'setScreenMode NORMAL_MODE:
            ZoomFull
        Case 187 '+
            setScreenMode ZOOM_IN_MODE
        Case 189 '-
            setScreenMode ZOOM_OUT_MODE
        Case 87 'W
            setScreenMode ZOOM_WIN_MODE
        Case 80 'P
            setScreenMode PAN_MODE
        Case 84 'T (tests)
'            NegotiateModes
'            setFunctionMode REDISTRIBUTE_LOADS_DH_FIRST_PANEL
            'setFunctionMode ADD_BOUNDARY
            'frmRestrictions.Show
            'ChangeMaps Me.Tag
            'frmImages.Show vbModeless, fMainForm
                    
            'Project.Item(ActiveProject).frm3D.Show
            'Project.Item(ActiveProject).frmOpenGL.Show 'vbModeless, fMainForm
            'main1 CInt(ActiveProject)
            'Project.Item(ActiveProject).frmRestrictions.Show vbModeless, fMainForm
            'Project.Item(ActiveProject).frmChartObjectiveFunction.Show , fMainForm
             Project.Item(ActiveProject).frmChartDesignVariables.Show , fMainForm
        Case 115 ' F4 (properties)
            Project.Item(ActiveProject).frmDisplayOptions.Show vbModeless, fMainForm
        Case 77 'M (match properties)
            mnuModelMatchProperties_Click
    End Select
End Sub

Private Sub picScreen_KeyUp(KeyCode As Integer, Shift As Integer)
    Select Case KeyCode
        Case 17 'Control
            setScreenMode SINGLE_SELECTION_MODE
        Case 27 'Escape
            Line1.Visible = False
    End Select
End Sub

Private Sub picScreen_MouseDown(Button As Integer, Shift As Integer, x As Single, y As Single)
    
    Select Case Button
        Case 1
            Select Case Project.Item(Me.Tag).ScreenMode
                Case NORMAL_MODE
                Case SINGLE_SELECTION_MODE
                    FindSelectedPanel x, y
                Case MULTIPLE_SELECTION_MODE
                    FindSelectedPanels x, y
                Case ZOOM_IN_MODE
                    ZoomIn x, y
                Case ZOOM_OUT_MODE
                    ZoomOut x, y
                Case ZOOM_WIN_MODE
                    X_BUFFER = x
                    Y_BUFFER = y
                    Border_Height_Buffer = cRectWND.Height
                    Border_Width_Buffer = cRectWND.Width
                    Border_Left_Buffer = cRectWND.Left
                    Border_Top_Buffer = cRectWND.Top
                Case ZOOM_FULL_MODE
                    ZoomFull
                Case PAN_MODE
'                    Project.Item(Me.Tag).EconomicDraw = True
                    X_BUFFER = x
                    Y_BUFFER = y
            End Select
            If Project.Item(Me.Tag).ScreenMode = NORMAL_MODE Then
            Select Case Project.Item(Me.Tag).FunctionMode
                Case ERASE_MODE_FUNCTION
                    'FindSelectedPanel X, Y
                    DeletePanel Me.Tag, x, y
                Case ADD_PLATE_FIRST_NODE_FUNCTION
                    X_BUFFER = x
                    Y_BUFFER = y
                    FindSelectedNode x, y
                    AddPanel "Plate"
                Case ADD_PLATE_SECOND_NODE_FUNCTION
                    FindSelectedNode x, y
                    AddPanel "Plate"
                Case ADD_BEAM_FIRST_NODE_FUNCTION
                    X_BUFFER = x
                    Y_BUFFER = y
                    FindSelectedNode x, y
                    AddPanel "Beam"
                Case ADD_BEAM_SECOND_NODE_FUNCTION
                    FindSelectedNode x, y
                    AddPanel "Beam"
                Case ADD_DOUBLEHULL_FIRST_PANEL_FUNCTION
                    FindSelectedPanel x, y
                    AddPanel "DoubleHull"
                Case ADD_DOUBLEHULL_SECOND_PANEL_FUNCTION
                    FindSelectedPanel x, y
                    AddPanel "DoubleHull"
                Case EXPLODE_DOUBLEHULL_FUNCTION
                    ExplodeDoubleHull x, y
                Case REVERSE_PANEL_FUNCTION
                    ReversePanel x, y
                Case GET_DIST_FIRST_NODE_FUNCTION
                    X_BUFFER = x
                    Y_BUFFER = y
                    GetDistBtnNodes x, y
                Case GET_DIST_SECOND_NODE_FUNCTION
                    GetDistBtnNodes x, y
                Case DIVIDE_PANEL_FUNCTION
                    DividePanel x, y
                Case MATCH_PROPERTIES_FUNCTION
                    MatchProperties x, y
                Case PASTE_PROPERTIES_FUNCTION
                    MatchProperties x, y
                Case PANEL_TYPE_INNERDHULL
                    AddPanelTypeDHull x, y, 0
                Case PANEL_TYPE_OUTERDHULL
                    AddPanelTypeDHull x, y, 1
                Case PANEL_TYPE_PRIMARY_LONGITUDINAL
                    AddPanelTypeGirder x, y, 0
                Case PANEL_TYPE_BILGE
                    AddPanelTypeBilge x, y, 0
                Case PANEL_TYPE_SIMPLE_SHELL
                    AddPanelTypeSimpleShell x, y, 0
                Case ADD_NAPPE
                    AddNappe x, y, 0
                Case PANEL_TYPE_VIRTUAL
                    AddPanelTypeVirtual x, y, 0
                Case ADD_BOUNDARY
                    FindSelectedNode x, y
                    AddBoundary
                Case REDISTRIBUTE_LOADS_DH_FIRST_PANEL
                    FindSelectedPanel x, y
                    RedistributePressuresDH
                Case REDISTRIBUTE_LOADS_DH_SECOND_PANEL
                    FindSelectedPanel x, y
                    RedistributePressuresDH
            End Select
            End If
        Case 2
            If Project.Item(Me.Tag).ScreenMode = NORMAL_MODE Then
                Select Case Project.Item(Me.Tag).FunctionMode
                    Case PANEL_TYPE_INNERDHULL
                        setFunctionMode PANEL_TYPE_OUTERDHULL
                        Exit Sub
                    Case PANEL_TYPE_OUTERDHULL
                        AddPanelTypeDHull 0, 0, 3
                        setFunctionMode NO_FUNCTION
                        setScreenMode NORMAL_MODE
                        Exit Sub
                    Case PANEL_TYPE_SIMPLE_SHELL
                        AddPanelTypeSimpleShell 0, 0, 1
                        setFunctionMode NO_FUNCTION
                        setScreenMode NORMAL_MODE
                    Case PANEL_TYPE_BILGE
                        AddPanelTypeBilge 0, 0, 1
                        setFunctionMode NO_FUNCTION
                        setScreenMode NORMAL_MODE
                    Case PANEL_TYPE_PRIMARY_LONGITUDINAL
                        AddPanelTypeGirder 0, 0, 1
                        setFunctionMode NO_FUNCTION
                        setScreenMode NORMAL_MODE
                    Case ADD_NAPPE
                        AddNappe 0, 0, 1
                        setFunctionMode NO_FUNCTION
                        setScreenMode NORMAL_MODE
                    Case PANEL_TYPE_VIRTUAL
                        AddPanelTypeVirtual 0, 0, 1
                        setFunctionMode NO_FUNCTION
                        setScreenMode NORMAL_MODE
                End Select
                NegotiateModes
            Else
                setFunctionMode Project.Item(Me.Tag).FunctionMode
                If Project.Item(Me.Tag).FunctionMode = NO_FUNCTION Then
                    setScreenMode NORMAL_MODE
                End If
                Project.Item(Me.Tag).ScreenMode = NORMAL_MODE
            End If
        Case 3
    End Select
    Draw Me.Tag
End Sub

Private Sub picScreen_MouseMove(Button As Integer, Shift As Integer, x As Single, y As Single)
    
    'StatusBar.Panels(1).Text = "Y = " & x / getScale(Me.Tag) + Project.Item(Me.Tag).cHeader.YAxisOrigin & "; Z = " & y
    
    
    If Button = 3 Then 'both left & right
        ZoomUpDn x, y
    End If
    Line1.Visible = False
    Select Case Project.Item(Me.Tag).ScreenMode
        Case NORMAL_MODE
            HighlightPassedOverItem x, y
        Case PAN_MODE
            If Button = 1 Then pan x, y, X_BUFFER, Y_BUFFER
            X_BUFFER = x
            Y_BUFFER = y
        Case ZOOM_WIN_MODE
            If Button = 1 Then
                ResizeBorder x, y, X_BUFFER, Y_BUFFER
            End If
    End Select
    Select Case Project.Item(Me.Tag).ScreenMode
        Case NORMAL_MODE
            Select Case Project.Item(Me.Tag).FunctionMode
                Case ADD_PLATE_SECOND_NODE_FUNCTION, ADD_BEAM_SECOND_NODE_FUNCTION, GET_DIST_SECOND_NODE_FUNCTION
                    Dim cNode As cNode
                    Line1.x1 = Project.Item(Me.Tag).colNodes.Item(Project.Item(Me.Tag).LastNode).Y_Screen
                    Line1.y1 = Project.Item(Me.Tag).colNodes.Item(Project.Item(Me.Tag).LastNode).Z_Screen
                    Line1.X2 = x
                    Line1.y2 = y
                    Line1.Visible = True
            End Select
        Case Else
    End Select
End Sub

Private Sub picScreen_MouseUp(Button As Integer, Shift As Integer, x As Single, y As Single)
    Combo.Visible = False
'    Project.Item(Me.Tag).EconomicDraw = False
'    Draw
    Select Case Project.Item(Me.Tag).ScreenMode
        Case ZOOM_WIN_MODE
            If Button = 1 Then
                ZoomWin Border_Width_Buffer, Border_Height_Buffer, Border_Left_Buffer, Border_Top_Buffer
            End If
         Case NORMAL_MODE
            Line1.Visible = False
        Case ADD_PLATE_FIRST_NODE_FUNCTION, ADD_BEAM_FIRST_NODE_FUNCTION, GET_DIST_FIRST_NODE_FUNCTION
            Line1.Visible = False
    End Select
End Sub

Public Sub StatusBar_PanelDblClick(ByVal Panel As MSComctlLib.Panel)
    SetStatusBar Panel
    If Combo.ListCount > 0 Then dropme Combo
End Sub

Public Sub SetStatusBar(ByVal Panel As MSComctlLib.Panel)
    Dim LoadCase As cLoadCase
    Combo.Clear
    
    Combo.Left = Panel.Left
    Combo.Width = Panel.Width
    Combo.Top = StatusBar.Top
    
    Select Case Panel.KEY
        Case "Mode"
            Combo.Tag = "Mode"
            Combo.Visible = True
            Combo.AddItem "MODE= NORMAL"
            Combo.AddItem "MODE= SINGLE SELECTION"
            Combo.AddItem "MODE= ZOOM IN"
            Combo.AddItem "MODE= ZOOM OUT"
            Combo.AddItem "MODE= ZOOM WIN"
            Combo.AddItem "MODE= PAN"
            Combo.Width = 155
            Panel.Width = Combo.Width
            'dropme Combo
        Case "LoadCase"
            Combo.Tag = "LoadCase"
            If Project.Item(Me.Tag).cHeader.colLoadCase.Count = 0 Then Exit Sub
            For Each LoadCase In Project.Item(Me.Tag).cHeader.colLoadCase
                If Len(LoadCase.Title) * 5.5 > Combo.Width Then Combo.Width = Len(LoadCase.Title) * 5.5
            Next LoadCase
            Combo.Visible = True
            Combo.AddItem "None"
            For Each LoadCase In Project.Item(Me.Tag).cHeader.colLoadCase
                Combo.AddItem LoadCase.Title
            Next LoadCase
            Combo = Combo.List(Project.Item(Me.Tag).cDisplaySettings.CurrentLoadCase)
            'dropme Combo
        Case "Solver"
            Combo.Tag = "Solver"
            Combo.Visible = True
            If Licensing.IS_LBR4 = True Then Combo.AddItem "Solver = LBR-4"
            
            If Licensing.IS_BEAM_THEORY = True Then Combo.AddItem "Solver = Beam Theory"
            Panel.Width = 120
            'If Licensing.IS_LBR4 = True And Licensing.IS_BEAM_THEORY = True Then Combo = Combo.List(Project.Item(Me.Tag).cHeader.IANA - 1)
        Case "Assistant"
            Project.Item(Me.Tag).frmModellingAssistant.Show vbModeless, fMainForm
    End Select

End Sub

Public Function NegociateIANAmenus()
    Select Case Project.Item(Me.Tag).cHeader.IANA
        Case 1
            'mnuProjectOptionsOverallSpan.Visible = True
            mnuProjectOptionsDeadweight.Visible = True
            mnuProjectOptionsFourierSeries.Visible = True
            mnuProjectOptionsOutput.Visible = True
            mnuToolsHullGirder.Visible = True
            mnuToolsBar1.Visible = True
            Me.StatusBar.Panels("Solver").Text = "Solver = LBR-4"
            Me.mnuAnalysisSolverLBR4.Checked = True
            Me.mnuAnalysisSolverBeamTheory.Checked = False
            Me.mnuAnalysisOutputFilesSolution1.Visible = True
            Me.mnuAnalysisOutputFilesSolution2.Visible = True
            Me.mnuAnalysisOutputFilesSolution3.Visible = False
            Me.mnuProjectOptionsRuleLength.Visible = True
        Case 2
            'mnuProjectOptionsOverallSpan.Visible = False
            mnuProjectOptionsFourierSeries.Visible = False
            mnuProjectOptionsDeadweight.Visible = False
            mnuProjectOptionsOutput.Visible = False
            mnuToolsHullGirder.Visible = False
            mnuToolsBar1.Visible = False
            Me.StatusBar.Panels("Solver").Text = "Solver = Beam Theory"
            Me.mnuAnalysisSolverLBR4.Checked = False
            Me.mnuAnalysisSolverBeamTheory.Checked = True
            Me.mnuAnalysisOutputFilesSolution1.Visible = False
            Me.mnuAnalysisOutputFilesSolution2.Visible = False
            Me.mnuAnalysisOutputFilesSolution3.Visible = True
            Me.mnuProjectOptionsRuleLength.Visible = True
    End Select
    
'    Select Case IS_COSTCAT
'        Case Is = True
'            Me.mnuOptimizationCostDevelopped.Enabled = True
'        Case Is = False
'            Me.mnuOptimizationCostDevelopped.Enabled = False
'    End Select
End Function

Public Function NegociateLICmenus()

    'LBR4 - stiff plate theory
    Me.mnuAnalysisSolverLBR4.Enabled = Licensing.IS_LBR4
    Me.mnuAnalysisSolverBeamTheory.Enabled = Licensing.IS_BEAM_THEORY
    'COST-CAT AKER
    Me.mnuOptimizationCostDevelopped.Enabled = Licensing.IS_COSTCAT
    
    'Import MARS
    Me.mnuFileImportFromMarsLbr5TransfertFile.Enabled = Licensing.IS_IMPORT_MARS
    'Import AVPRO
    Me.mnuFileImportFromAvproLbr5TransfertFile.Enabled = Licensing.IS_IMPORT_AVPRO
    
    'Add Panel
    Me.mnuModelAddBeams.Enabled = Licensing.IS_ADD_PANEL
    Me.mnuModelAddPlates.Enabled = Licensing.IS_ADD_PANEL
    
    'Divide Panel
    Me.mnuModelDividePanel.Enabled = Licensing.IS_DIVIDE_PANEL
    
    'Delete Panel(s)
    Me.mnuModelDeletePanel.Enabled = Licensing.IS_DELETE_PANEL
    
    'Multiple Solve
    Me.mnuAnalysisMultiSolver.Enabled = Licensing.IS_MULTIPLE_SOLVE
    
    'Demo version
    If Licensing.LicenseLevel = 7 Then
        fMainForm.mnuFileNew.Enabled = False
        Me.mnuFileNew.Enabled = False
        'fMainForm.mnuFileOpen.Enabled = False
        'Me.mnuFileOpen.Enabled = False
        fMainForm.mnuFileImportFrom.Enabled = False
        Me.mnuFileImportFrom.Enabled = False
        fMainForm.mnuFileSaveAs.Enabled = False
        Me.mnuFileSaveAs.Enabled = False
        Me.mnuPanelsAddSymmBC.Enabled = False
        Me.mnuLoadsRedistributeLoadsDH.Enabled = False
        Dim i As Integer
        For i = 1 To Me.mnuFileMRU.Count
            Me.mnuFileMRU.Item(i).Enabled = False
        Next i
        For i = 1 To fMainForm.mnuFileMRU.Count
            fMainForm.mnuFileMRU.Item(i).Enabled = False
        Next i
    End If
End Function



