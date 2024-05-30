VERSION 5.00
Object = "{714D09E3-B193-11D3-A192-00A0CC26207F}#1.0#0"; "dftlbV1.dll"
Object = "{831FDD16-0C5C-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCTL.OCX"
Begin VB.Form tb_Geometry 
   Caption         =   "Geometry"
   ClientHeight    =   1170
   ClientLeft      =   5580
   ClientTop       =   2655
   ClientWidth     =   5700
   LinkTopic       =   "Form1"
   ScaleHeight     =   1170
   ScaleWidth      =   5700
   Begin MSComctlLib.Toolbar Toolbar1 
      Align           =   1  'Align Top
      Height          =   330
      Left            =   0
      TabIndex        =   1
      Top             =   0
      Width           =   5700
      _ExtentX        =   10054
      _ExtentY        =   582
      ButtonWidth     =   609
      ButtonHeight    =   582
      Style           =   1
      ImageList       =   "ImageList1"
      _Version        =   393216
      BeginProperty Buttons {66833FE8-8583-11D1-B16A-00C0F0283628} 
         NumButtons      =   19
         BeginProperty Button1 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Key             =   "Nodes"
            Object.ToolTipText     =   "Nodes"
            ImageIndex      =   1
         EndProperty
         BeginProperty Button2 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Key             =   "AddPlate"
            Object.ToolTipText     =   "Add Plate"
            ImageIndex      =   2
         EndProperty
         BeginProperty Button3 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Key             =   "AddBeam"
            Object.ToolTipText     =   "Add Beam"
            ImageIndex      =   3
         EndProperty
         BeginProperty Button4 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Enabled         =   0   'False
            Key             =   "AddDHull"
            Object.ToolTipText     =   "Add Double Hull"
            ImageIndex      =   4
         EndProperty
         BeginProperty Button5 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Style           =   3
         EndProperty
         BeginProperty Button6 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Key             =   "PlateScantlings"
            Object.ToolTipText     =   "Plate Scantlings"
            ImageIndex      =   5
         EndProperty
         BeginProperty Button7 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Key             =   "BeamScantlings"
            Object.ToolTipText     =   "Beam Scantlings"
            ImageIndex      =   6
         EndProperty
         BeginProperty Button8 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Enabled         =   0   'False
            Key             =   "DHullScantlings"
            Object.ToolTipText     =   "Double Hull Scantlings"
            ImageIndex      =   7
         EndProperty
         BeginProperty Button9 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Style           =   3
         EndProperty
         BeginProperty Button10 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Key             =   "DeletePanel"
            Object.ToolTipText     =   "Delete Panel"
            ImageIndex      =   8
         EndProperty
         BeginProperty Button11 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Key             =   "ExplodeDHull"
            Object.ToolTipText     =   "Explode Double Hull"
            ImageIndex      =   9
         EndProperty
         BeginProperty Button12 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Style           =   3
         EndProperty
         BeginProperty Button13 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Key             =   "Loads"
            Object.ToolTipText     =   "Loads"
            ImageIndex      =   10
         EndProperty
         BeginProperty Button14 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Key             =   "BoundaryConditions"
            Object.ToolTipText     =   "Boundary Conditions"
            ImageIndex      =   11
         EndProperty
         BeginProperty Button15 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Style           =   3
         EndProperty
         BeginProperty Button16 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Key             =   "DividePanel"
            Object.ToolTipText     =   "Divide Panel"
            ImageIndex      =   12
         EndProperty
         BeginProperty Button17 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Key             =   "ReversePanel"
            Object.ToolTipText     =   "Reverse Panel"
            ImageIndex      =   13
         EndProperty
         BeginProperty Button18 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Object.Visible         =   0   'False
            Key             =   "SortPanels"
            Object.ToolTipText     =   "Sort Panels"
            ImageIndex      =   14
         EndProperty
         BeginProperty Button19 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Key             =   "MatchProperties"
            Object.ToolTipText     =   "Match Panel Properties"
            ImageIndex      =   15
         EndProperty
      EndProperty
   End
   Begin MSComctlLib.ImageList ImageList1 
      Left            =   1800
      Top             =   480
      _ExtentX        =   1005
      _ExtentY        =   1005
      BackColor       =   -2147483643
      ImageWidth      =   16
      ImageHeight     =   16
      MaskColor       =   12632256
      _Version        =   393216
      BeginProperty Images {2C247F25-8591-11D1-B16A-00C0F0283628} 
         NumListImages   =   15
         BeginProperty ListImage1 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "tb_Geometry.frx":0000
            Key             =   ""
         EndProperty
         BeginProperty ListImage2 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "tb_Geometry.frx":0112
            Key             =   ""
         EndProperty
         BeginProperty ListImage3 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "tb_Geometry.frx":0224
            Key             =   ""
         EndProperty
         BeginProperty ListImage4 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "tb_Geometry.frx":0336
            Key             =   ""
         EndProperty
         BeginProperty ListImage5 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "tb_Geometry.frx":0448
            Key             =   ""
         EndProperty
         BeginProperty ListImage6 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "tb_Geometry.frx":055A
            Key             =   ""
         EndProperty
         BeginProperty ListImage7 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "tb_Geometry.frx":066C
            Key             =   ""
         EndProperty
         BeginProperty ListImage8 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "tb_Geometry.frx":077E
            Key             =   ""
         EndProperty
         BeginProperty ListImage9 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "tb_Geometry.frx":0890
            Key             =   ""
         EndProperty
         BeginProperty ListImage10 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "tb_Geometry.frx":09A2
            Key             =   ""
         EndProperty
         BeginProperty ListImage11 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "tb_Geometry.frx":0AB4
            Key             =   ""
         EndProperty
         BeginProperty ListImage12 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "tb_Geometry.frx":0BC6
            Key             =   ""
         EndProperty
         BeginProperty ListImage13 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "tb_Geometry.frx":0CD8
            Key             =   ""
         EndProperty
         BeginProperty ListImage14 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "tb_Geometry.frx":0DEA
            Key             =   ""
         EndProperty
         BeginProperty ListImage15 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "tb_Geometry.frx":0EFC
            Key             =   ""
         EndProperty
      EndProperty
   End
   Begin XDOCKFLOATLibCtl.FDPane FDPane1 
      Height          =   420
      Left            =   0
      TabIndex        =   0
      Top             =   480
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
Attribute VB_Name = "tb_Geometry"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Dim cProject As cProject

Private Sub FDPane1_OnHidden()
    For Each cProject In Project
        cProject.frmProject.mnuViewToolbarsGeometry.Checked = False
    Next cProject
End Sub

Private Sub FDPane1_OnShown()
    For Each cProject In Project
        cProject.frmProject.mnuViewToolbarsGeometry.Checked = True
    Next cProject
End Sub

Private Sub Form_Load()
    With Toolbar1
        .Buttons("AddBeam").Enabled = Licensing.IS_ADD_PANEL
        .Buttons("AddPlate").Enabled = Licensing.IS_ADD_PANEL
        .Buttons("DeletePanel").Enabled = Licensing.IS_DELETE_PANEL
        .Buttons("DividePanel").Enabled = Licensing.IS_DIVIDE_PANEL
        .Buttons("ExplodeDHull").Enabled = False
    End With
End Sub

Private Sub Toolbar1_ButtonClick(ByVal Button As MSComctlLib.Button)
    On Error Resume Next
    If Project.Count = 0 Then Exit Sub
    Select Case Button.KEY
        Case "Nodes"
            Project.Item(ActiveProject).frmProject.mnuModelNodes_Click
        Case "AddPlate"
            Project.Item(ActiveProject).frmProject.mnuModelAddPlates_Click
        Case "AddBeam"
            Project.Item(ActiveProject).frmProject.mnuModelAddBeams_Click
        Case "AddDHull"
            Project.Item(ActiveProject).frmProject.mnuModelAddDoubleHulls_Click
            '|'
        Case "PlateScantlings"
            Project.Item(ActiveProject).frmProject.mnuPanelsPlateScantlings_Click
        Case "BeamScantlings"
            Project.Item(ActiveProject).frmProject.mnuPanelsBeamScantlings_Click
        Case "DHullScantlings"
            Project.Item(ActiveProject).frmProject.mnuPanelsDoubleHullScantlings_Click
            '|'
        Case "DeletePanel"
            'Project.Item(ActiveProject).frmProject.mnuModelDeletePanel_Click
            setFunctionMode ERASE_MODE_FUNCTION
        Case "ExplodeDHull"
            Project.Item(ActiveProject).frmProject.mnuModelExplodeDoubleHull_Click
            '|'
        Case "Loads"
            Project.Item(ActiveProject).frmProject.mnuLoadsLoads_Click
        Case "BoundaryConditions"
            Project.Item(ActiveProject).frmProject.mnuPanelsBoundaryConditions_Click
            '|'
        Case "DividePanel"
            Project.Item(ActiveProject).frmProject.mnuModelDividePanel_Click
        Case "ReversePanel"
            'Project.Item(ActiveProject).frmProject.mnuModelReversePanels_Click
            setFunctionMode REVERSE_PANEL_FUNCTION
        Case "SortPanels"
            Project.Item(ActiveProject).frmProject.mnuModelSortPanels_Click
        Case "MatchProperties"
            Project.Item(ActiveProject).frmProject.mnuModelMatchProperties_Click
    End Select
End Sub

