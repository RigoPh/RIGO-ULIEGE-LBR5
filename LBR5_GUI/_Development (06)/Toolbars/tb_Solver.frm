VERSION 5.00
Object = "{714D09E3-B193-11D3-A192-00A0CC26207F}#1.0#0"; "dftlbV1.dll"
Object = "{831FDD16-0C5C-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCTL.OCX"
Begin VB.Form tb_Solver 
   Caption         =   "Solver"
   ClientHeight    =   1125
   ClientLeft      =   2625
   ClientTop       =   1905
   ClientWidth     =   2250
   LinkTopic       =   "Form1"
   ScaleHeight     =   1125
   ScaleWidth      =   2250
   Begin MSComctlLib.Toolbar Toolbar1 
      Align           =   1  'Align Top
      Height          =   330
      Left            =   0
      TabIndex        =   1
      Top             =   0
      Width           =   2250
      _ExtentX        =   3969
      _ExtentY        =   582
      ButtonWidth     =   609
      ButtonHeight    =   582
      Style           =   1
      ImageList       =   "ImageList1"
      _Version        =   393216
      BeginProperty Buttons {66833FE8-8583-11D1-B16A-00C0F0283628} 
         NumButtons      =   7
         BeginProperty Button1 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Key             =   "Solve"
            Object.ToolTipText     =   "Solve"
            ImageIndex      =   1
         EndProperty
         BeginProperty Button2 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Key             =   "MultiSolve"
            Object.ToolTipText     =   "Multiple Solve"
            ImageIndex      =   2
         EndProperty
         BeginProperty Button3 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Style           =   3
         EndProperty
         BeginProperty Button4 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Key             =   "Solution"
            Object.ToolTipText     =   "Solution"
            ImageIndex      =   3
         EndProperty
         BeginProperty Button5 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Key             =   "ObjectiveFunction"
            Object.ToolTipText     =   "Objective Function Variation"
            ImageIndex      =   4
         EndProperty
         BeginProperty Button6 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Key             =   "GravityCenter"
            Object.ToolTipText     =   "Gravity Center Variation"
            ImageIndex      =   5
         EndProperty
         BeginProperty Button7 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Key             =   "DesignVariables"
            Object.ToolTipText     =   "Design Variables Variation"
            ImageIndex      =   6
         EndProperty
      EndProperty
   End
   Begin MSComctlLib.ImageList ImageList1 
      Left            =   600
      Top             =   360
      _ExtentX        =   1005
      _ExtentY        =   1005
      BackColor       =   -2147483643
      ImageWidth      =   16
      ImageHeight     =   16
      MaskColor       =   12632256
      _Version        =   393216
      BeginProperty Images {2C247F25-8591-11D1-B16A-00C0F0283628} 
         NumListImages   =   6
         BeginProperty ListImage1 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "tb_Solver.frx":0000
            Key             =   ""
         EndProperty
         BeginProperty ListImage2 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "tb_Solver.frx":0112
            Key             =   ""
         EndProperty
         BeginProperty ListImage3 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "tb_Solver.frx":0224
            Key             =   ""
         EndProperty
         BeginProperty ListImage4 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "tb_Solver.frx":0336
            Key             =   ""
         EndProperty
         BeginProperty ListImage5 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "tb_Solver.frx":0448
            Key             =   ""
         EndProperty
         BeginProperty ListImage6 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "tb_Solver.frx":055A
            Key             =   ""
         EndProperty
      EndProperty
   End
   Begin XDOCKFLOATLibCtl.FDPane FDPane1 
      Height          =   420
      Left            =   0
      TabIndex        =   0
      Top             =   360
      Visible         =   0   'False
      Width           =   420
      _cx             =   2010776293
      _cy             =   2010776293
      DockType        =   1
      PaneVisible     =   -1  'True
      DockStyle       =   1
      CanDockLeft     =   -1  'True
      CanDockTop      =   -1  'True
      CanDockRight    =   -1  'True
      CanDockBottom   =   -1  'True
      AutoHide        =   1
      InitDockHW      =   150
      InitFloatLeft   =   200
      InitFloatTop    =   200
      InitFloatWidth  =   200
      InitFloatHeight =   200
   End
End
Attribute VB_Name = "tb_Solver"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Dim cProject As cProject

Private Sub FDPane1_OnHidden()
    For Each cProject In Project
        cProject.frmProject.mnuViewToolbarsSolver.Checked = False
    Next cProject
    ' fMainForm.mnuViewToolbarsSolver.Checked = False
End Sub

Private Sub FDPane1_OnShown()
    For Each cProject In Project
        cProject.frmProject.mnuViewToolbarsSolver.Checked = True
    Next cProject
'    fMainForm.mnuViewToolbarsSolver.Checked = True
End Sub

Private Sub Form_Load()
    Toolbar1.Buttons("MultiSolve").Enabled = Licensing.IS_MULTIPLE_SOLVE
End Sub

Private Sub Toolbar1_ButtonClick(ByVal Button As MSComctlLib.Button)
    On Error Resume Next
    If Project.Count = 0 Then Exit Sub
    Select Case Button.KEY
        Case "Solve"
            Project.Item(ActiveProject).frmProject.mnuAnalysisSolve_Click
        Case "MultiSolve"
            Project.Item(ActiveProject).frmProject.mnuAnalysisMultiSolver_Click
        Case "Solution"
            Project.Item(ActiveProject).frmProject.mnuAnalysisOutputGraphsGeneralSolution_Click
        Case "ObjectiveFunction"
            Project.Item(ActiveProject).frmProject.mnuAnalysisOutputGraphsObjectiveFunctionVariation_Click
        Case "GravityCenter"
            Project.Item(ActiveProject).frmProject.mnuAnalysisOutputGraphsGravityCenterVariation_Click
        Case "DesignVariables"
            Project.Item(ActiveProject).frmProject.mnuAnalysisOutputGraphsDesignVariablesVariation_Click
    End Select
End Sub
