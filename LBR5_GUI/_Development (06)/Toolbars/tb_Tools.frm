VERSION 5.00
Object = "{714D09E3-B193-11D3-A192-00A0CC26207F}#1.0#0"; "dftlbV1.dll"
Object = "{831FDD16-0C5C-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCTL.OCX"
Begin VB.Form tb_Tools 
   Caption         =   "Tools"
   ClientHeight    =   1065
   ClientLeft      =   2340
   ClientTop       =   1845
   ClientWidth     =   2355
   LinkTopic       =   "Form1"
   ScaleHeight     =   1065
   ScaleWidth      =   2355
   Begin MSComctlLib.Toolbar Toolbar1 
      Align           =   1  'Align Top
      Height          =   330
      Left            =   0
      Negotiate       =   -1  'True
      TabIndex        =   0
      Top             =   0
      Width           =   2355
      _ExtentX        =   4154
      _ExtentY        =   582
      ButtonWidth     =   609
      ButtonHeight    =   582
      Style           =   1
      ImageList       =   "imlToolbarIcons"
      _Version        =   393216
      BeginProperty Buttons {66833FE8-8583-11D1-B16A-00C0F0283628} 
         NumButtons      =   1
         BeginProperty Button1 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Key             =   "Ruler"
            Object.ToolTipText     =   "Ruler"
            ImageIndex      =   1
         EndProperty
      EndProperty
      OLEDropMode     =   1
   End
   Begin MSComctlLib.ImageList imlToolbarIcons 
      Left            =   840
      Top             =   360
      _ExtentX        =   1005
      _ExtentY        =   1005
      BackColor       =   -2147483643
      ImageWidth      =   16
      ImageHeight     =   16
      MaskColor       =   12632256
      _Version        =   393216
      BeginProperty Images {2C247F25-8591-11D1-B16A-00C0F0283628} 
         NumListImages   =   1
         BeginProperty ListImage1 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "tb_Tools.frx":0000
            Key             =   ""
         EndProperty
      EndProperty
   End
   Begin XDOCKFLOATLibCtl.FDPane FDPane1 
      Height          =   420
      Left            =   0
      TabIndex        =   1
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
Attribute VB_Name = "tb_Tools"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Dim cProject As cProject

Private Sub FDPane1_OnHidden()
    For Each cProject In Project
        cProject.frmProject.mnuViewToolbarsTools.Checked = False
    Next cProject
End Sub

Private Sub FDPane1_OnShown()
    For Each cProject In Project
        cProject.frmProject.mnuViewToolbarsTools.Checked = True
    Next cProject
End Sub

Private Sub Toolbar1_ButtonClick(ByVal Button As MSComctlLib.Button)
    On Error Resume Next
    If Project.Count = 0 Then Exit Sub
    Select Case Button.KEY
        Case "Ruler"
            setFunctionMode GET_DIST_FIRST_NODE_FUNCTION
    End Select
End Sub

