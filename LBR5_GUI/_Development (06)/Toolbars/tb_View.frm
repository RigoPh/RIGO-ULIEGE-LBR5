VERSION 5.00
Object = "{714D09E3-B193-11D3-A192-00A0CC26207F}#1.0#0"; "dftlbV1.dll"
Object = "{831FDD16-0C5C-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCTL.OCX"
Begin VB.Form tb_View 
   Caption         =   "View"
   ClientHeight    =   990
   ClientLeft      =   10965
   ClientTop       =   1575
   ClientWidth     =   2160
   LinkTopic       =   "Form1"
   ScaleHeight     =   990
   ScaleWidth      =   2160
   Begin MSComctlLib.Toolbar Toolbar1 
      Align           =   1  'Align Top
      Height          =   330
      Left            =   0
      Negotiate       =   -1  'True
      TabIndex        =   1
      Top             =   0
      Width           =   2160
      _ExtentX        =   3810
      _ExtentY        =   582
      ButtonWidth     =   609
      ButtonHeight    =   582
      Style           =   1
      ImageList       =   "imlToolbarIcons"
      _Version        =   393216
      BeginProperty Buttons {66833FE8-8583-11D1-B16A-00C0F0283628} 
         NumButtons      =   5
         BeginProperty Button1 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Key             =   "ZoomIn"
            Object.ToolTipText     =   "Zoom In"
            ImageIndex      =   1
         EndProperty
         BeginProperty Button2 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Key             =   "ZoomOut"
            Object.ToolTipText     =   "Zoom Out"
            ImageIndex      =   2
         EndProperty
         BeginProperty Button3 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Key             =   "ZoomWin"
            Object.ToolTipText     =   "Zoom Window"
            ImageIndex      =   3
         EndProperty
         BeginProperty Button4 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Key             =   "ZoomAll"
            Object.ToolTipText     =   "Zoom All"
            ImageIndex      =   4
         EndProperty
         BeginProperty Button5 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Key             =   "Pan"
            Object.ToolTipText     =   "Pan"
            ImageIndex      =   5
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
         NumListImages   =   5
         BeginProperty ListImage1 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "tb_View.frx":0000
            Key             =   ""
         EndProperty
         BeginProperty ListImage2 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "tb_View.frx":0112
            Key             =   ""
         EndProperty
         BeginProperty ListImage3 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "tb_View.frx":0224
            Key             =   ""
         EndProperty
         BeginProperty ListImage4 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "tb_View.frx":0336
            Key             =   ""
         EndProperty
         BeginProperty ListImage5 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "tb_View.frx":0448
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
Attribute VB_Name = "tb_View"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Dim cProject As cProject

Private Sub FDPane1_OnHidden()
    For Each cProject In Project
        cProject.frmProject.mnuViewToolbarsView.Checked = False
    Next cProject
End Sub

Private Sub FDPane1_OnShown()
    For Each cProject In Project
        cProject.frmProject.mnuViewToolbarsView.Checked = True
    Next cProject
End Sub

Private Sub Toolbar1_ButtonClick(ByVal Button As MSComctlLib.Button)
    On Error Resume Next
    If Project.Count = 0 Then Exit Sub
    Select Case Button.KEY
        Case "ZoomIn"
            setScreenMode ZOOM_IN_MODE
        Case "ZoomOut"
            setScreenMode ZOOM_OUT_MODE
        Case "ZoomWin"
            setScreenMode ZOOM_WIN_MODE
        Case "ZoomAll"
            'setScreenMode NORMAL_MODE
            ZoomFull
        Case "Pan"
            setScreenMode PAN_MODE
    End Select
End Sub
