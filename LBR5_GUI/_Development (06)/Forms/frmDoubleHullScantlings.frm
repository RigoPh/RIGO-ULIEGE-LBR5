VERSION 5.00
Object = "{831FDD16-0C5C-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCTL.OCX"
Object = "{5E9E78A0-531B-11CF-91F6-C2863C385E30}#1.0#0"; "MSFLXGRD.OCX"
Object = "{0D452EE1-E08F-101A-852E-02608C4D0BB4}#2.0#0"; "FM20.DLL"
Begin VB.Form frmDoubleHullScantlings 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Double Hull Scantlings"
   ClientHeight    =   4980
   ClientLeft      =   2310
   ClientTop       =   2220
   ClientWidth     =   8580
   Icon            =   "frmDoubleHullScantlings.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   4980
   ScaleWidth      =   8580
   ShowInTaskbar   =   0   'False
   Begin VB.TextBox TxtEditGirders 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   285
      Left            =   480
      TabIndex        =   10
      Text            =   "Girders"
      Top             =   2760
      Visible         =   0   'False
      Width           =   975
   End
   Begin VB.PictureBox picGirders 
      Appearance      =   0  'Flat
      ForeColor       =   &H80000008&
      Height          =   3105
      Left            =   5220
      ScaleHeight     =   3075
      ScaleWidth      =   1785
      TabIndex        =   5
      Top             =   720
      Width           =   1815
      Begin VB.ListBox lstPanels 
         Appearance      =   0  'Flat
         Height          =   1980
         Left            =   -10
         TabIndex        =   9
         Top             =   670
         Width           =   1820
      End
      Begin VB.ComboBox cbSide 
         Height          =   315
         Left            =   720
         Style           =   2  'Dropdown List
         TabIndex        =   8
         Top             =   2720
         Width           =   975
      End
      Begin VB.Label lblSide 
         Caption         =   "Side:"
         Height          =   255
         Left            =   120
         TabIndex        =   7
         Top             =   2760
         Width           =   615
      End
      Begin VB.Label lblPanelLength 
         Alignment       =   2  'Center
         Caption         =   "Panel Length: "
         Height          =   495
         Left            =   120
         TabIndex        =   6
         Top             =   120
         Width           =   1575
         WordWrap        =   -1  'True
      End
   End
   Begin VB.TextBox TxtEdit 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   285
      Left            =   480
      TabIndex        =   2
      Text            =   "Text1"
      Top             =   2280
      Visible         =   0   'False
      Width           =   975
   End
   Begin VB.ComboBox Combo 
      Appearance      =   0  'Flat
      Height          =   315
      ItemData        =   "frmDoubleHullScantlings.frx":000C
      Left            =   1560
      List            =   "frmDoubleHullScantlings.frx":000E
      Style           =   2  'Dropdown List
      TabIndex        =   1
      Top             =   2280
      Visible         =   0   'False
      Width           =   975
   End
   Begin MSFlexGridLib.MSFlexGrid MSH1 
      Height          =   3375
      Left            =   120
      TabIndex        =   0
      Top             =   720
      Width           =   8295
      _ExtentX        =   14631
      _ExtentY        =   5953
      _Version        =   393216
      Cols            =   11
      GridColor       =   0
      WordWrap        =   -1  'True
      ScrollTrack     =   -1  'True
      GridLinesFixed  =   1
      Appearance      =   0
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
   End
   Begin MSComctlLib.TabStrip TabStrip 
      Height          =   4335
      Left            =   0
      TabIndex        =   11
      Top             =   0
      Width           =   8535
      _ExtentX        =   15055
      _ExtentY        =   7646
      MultiRow        =   -1  'True
      HotTracking     =   -1  'True
      _Version        =   393216
      BeginProperty Tabs {1EFB6598-857C-11D1-B16A-00C0F0283628} 
         NumTabs         =   2
         BeginProperty Tab1 {1EFB659A-857C-11D1-B16A-00C0F0283628} 
            Caption         =   "Tab1"
            ImageVarType    =   2
         EndProperty
         BeginProperty Tab2 {1EFB659A-857C-11D1-B16A-00C0F0283628} 
            Caption         =   "Tab2"
            ImageVarType    =   2
         EndProperty
      EndProperty
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
   End
   Begin MSForms.CommandButton cmdCancel 
      Cancel          =   -1  'True
      Height          =   375
      Left            =   7440
      TabIndex        =   4
      Top             =   4440
      Width           =   1095
      Caption         =   "Cancel"
      PicturePosition =   327683
      Size            =   "1931;661"
      FontHeight      =   165
      FontCharSet     =   0
      FontPitchAndFamily=   2
      ParagraphAlign  =   3
   End
   Begin MSForms.CommandButton cmdOK 
      Default         =   -1  'True
      Height          =   375
      Left            =   6240
      TabIndex        =   3
      Top             =   4440
      Width           =   1095
      Caption         =   "OK"
      PicturePosition =   327683
      Size            =   "1931;661"
      FontHeight      =   165
      FontCharSet     =   0
      FontPitchAndFamily=   2
      ParagraphAlign  =   3
   End
   Begin VB.Menu mnuEdit 
      Caption         =   "Edit"
      Begin VB.Menu mnuEditCopy 
         Caption         =   "Copy"
         Shortcut        =   ^C
      End
      Begin VB.Menu mnuEditPaste 
         Caption         =   "Paste"
         Shortcut        =   ^V
      End
   End
End
Attribute VB_Name = "frmDoubleHullScantlings"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Dim ProjectIndex As Integer
Private dBuffer As String
Dim Panel As cPanel, colPanel As colPanel
Dim PanelListIndex As Integer
Dim colGeometry As Collection
Dim colScantlings As Collection
Dim colMaterials As Collection
Dim colParticipation As Collection
Dim colRelatedPanels As Collection
Dim Node As cNode, colNodes As colNodes

Private Sub cbSide_Click()
    Select Case cbSide.Text
        Case "Left"
            colScantlings.Item(PanelListIndex).GirderSide = SideLeft
        Case "Right"
            colScantlings.Item(PanelListIndex).GirderSide = SideRight
    End Select
End Sub

Private Sub Form_Load()
    mnuEdit.Visible = False
    ProjectIndex = ActiveProject
    Me.Move (Screen.Width - Me.Width) / 2, (Screen.Height - Me.Height) / 2
    cmdOK.Picture = LoadResPicture("ID_BITMAP_OK", 0)
    cmdCancel.Picture = LoadResPicture("ID_BITMAP_CANCEL", 0)
    Me.Caption = "Double Hull Scantlings - [" & GetFileName(Project.Item(ProjectIndex).sFileName) & "]"
    GetData
    PopulatePanelList
    lstPanels.ListIndex = 0
    TabStripProperties
    FlexGrid
    FillGrid
End Sub

Private Sub Form_Resize()
    Me.Height = cmdOK.Top + cmdOK.Height + 600
End Sub

Private Sub MSH1_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
    If Button = 2 Then
        PopupMenu mnuEdit
    End If
End Sub

Private Sub mnuEditCopy_Click()
    FlexCopy MSH1
End Sub

Private Sub mnuEditPaste_Click()
    FlexPaste MSH1
    SetData
End Sub

Private Sub FillGrid()
    Dim i As Integer, j As Integer
    i = 0: j = 0
    Select Case TabStrip.SelectedItem.Key
        Case "Geometry"
            For Each Panel In colPanel
                If Panel.pType = DoubleHull Then
                    If Panel.RelatedDoubleHullPanel > Panel.pNumber Then
                        i = i + 1
                        MSH1.TextMatrix(i, 0) = i
                        MSH1.TextMatrix(i, 1) = Panel.pNumber & "; " & Panel.RelatedDoubleHullPanel
                        MSH1.TextMatrix(i, 2) = Panel.cGeometry.PanelWidth
                        MSH1.TextMatrix(i, 3) = Panel.cScantlings.cPrimaryFrames.WebHeight
                        MSH1.TextMatrix(i, 4) = Panel.cGeometry.PanelAngle
                        MSH1.TextMatrix(i, 5) = Panel.pNumber
                        MSH1.TextMatrix(i, 6) = Panel.RelatedDoubleHullPanel
                    End If
                End If
            Next Panel
        Case "Thickness"
            For Each Panel In colPanel
                If Panel.pType = DoubleHull Then
                    If Panel.RelatedDoubleHullPanel > Panel.pNumber Then
                        i = i + 1: j = j + 1
                        MSH1.TextMatrix(i, 0) = j
                        MSH1.TextMatrix(i, 1) = Panel.pNumber
                        'MSH1.Row = i: MSH1.col = 1: MSH1.CellForeColor = &H80000011
                        MSH1.TextMatrix(i, 2) = colScantlings.Item(Panel.pNumber).NetThickness * 1000
                        MSH1.TextMatrix(i, 3) = colScantlings.Item(Panel.pNumber).CorrosionThickness * 1000
                        i = i + 1
                        MSH1.TextMatrix(i, 0) = ""
                        MSH1.TextMatrix(i, 1) = Panel.RelatedDoubleHullPanel
                        'MSH1.Row = i: MSH1.col = 1: MSH1.CellForeColor = &H80000011
                        MSH1.TextMatrix(i, 2) = colScantlings.Item(Panel.RelatedDoubleHullPanel).NetThickness * 1000
                        MSH1.TextMatrix(i, 3) = colScantlings.Item(Panel.RelatedDoubleHullPanel).CorrosionThickness * 1000
                    End If
                End If
            Next Panel
        Case "StiffenersSet1"
            For Each Panel In colPanel
                If Panel.pType = DoubleHull Then
                    If Panel.RelatedDoubleHullPanel > Panel.pNumber Then
                        i = i + 1: j = j + 1
                        MSH1.TextMatrix(i, 0) = j
                        MSH1.TextMatrix(i, 1) = Panel.pNumber
                        MSH1.TextMatrix(i, 2) = colScantlings.Item(Panel.pNumber).cPrimaryStiffeners.DistributionMode
                        MSH1.TextMatrix(i, 3) = colScantlings.Item(Panel.pNumber).cPrimaryStiffeners.WebHeight * 1000
                        MSH1.TextMatrix(i, 4) = colScantlings.Item(Panel.pNumber).cPrimaryStiffeners.WebThickness * 1000
                        MSH1.TextMatrix(i, 5) = colScantlings.Item(Panel.pNumber).cPrimaryStiffeners.FlangeWidth * 1000
                        MSH1.TextMatrix(i, 6) = colScantlings.Item(Panel.pNumber).cPrimaryStiffeners.FlangeThickness * 1000
                        MSH1.TextMatrix(i, 7) = colScantlings.Item(Panel.pNumber).cPrimaryStiffeners.Spacing * 1000
                        MSH1.TextMatrix(i, 8) = colScantlings.Item(Panel.pNumber).cPrimaryStiffeners.CorrosionThickness * 1000
                        Select Case colScantlings.Item(Panel.pNumber).cPrimaryStiffeners.Side
                            Case SideNone
                                MSH1.TextMatrix(i, 9) = "None"
                            Case SideLeft
                                MSH1.TextMatrix(i, 9) = "Left"
                            Case SideRight
                                MSH1.TextMatrix(i, 9) = "Right"
                        End Select
                        i = i + 1
                        MSH1.TextMatrix(i, 0) = ""
                        MSH1.TextMatrix(i, 1) = Panel.RelatedDoubleHullPanel
                        MSH1.TextMatrix(i, 2) = colScantlings.Item(Panel.RelatedDoubleHullPanel).cPrimaryStiffeners.DistributionMode
                        MSH1.TextMatrix(i, 3) = colScantlings.Item(Panel.RelatedDoubleHullPanel).cPrimaryStiffeners.WebHeight * 1000
                        MSH1.TextMatrix(i, 4) = colScantlings.Item(Panel.RelatedDoubleHullPanel).cPrimaryStiffeners.WebThickness * 1000
                        MSH1.TextMatrix(i, 5) = colScantlings.Item(Panel.RelatedDoubleHullPanel).cPrimaryStiffeners.FlangeWidth * 1000
                        MSH1.TextMatrix(i, 6) = colScantlings.Item(Panel.RelatedDoubleHullPanel).cPrimaryStiffeners.FlangeThickness * 1000
                        MSH1.TextMatrix(i, 7) = colScantlings.Item(Panel.RelatedDoubleHullPanel).cPrimaryStiffeners.Spacing * 1000
                        MSH1.TextMatrix(i, 8) = colScantlings.Item(Panel.RelatedDoubleHullPanel).cPrimaryStiffeners.CorrosionThickness * 1000
                        Select Case colScantlings.Item(Panel.RelatedDoubleHullPanel).cPrimaryStiffeners.Side
                            Case SideNone
                                MSH1.TextMatrix(i, 9) = "None"
                            Case SideLeft
                                MSH1.TextMatrix(i, 9) = "Left"
                            Case SideRight
                                MSH1.TextMatrix(i, 9) = "Right"
                        End Select

                    End If
                End If
            Next Panel
        Case "FramesSet1"
            For Each Panel In colPanel
                If Panel.pType = DoubleHull Then
                    If Panel.RelatedDoubleHullPanel > Panel.pNumber Then
                        i = i + 1
                        MSH1.TextMatrix(i, 0) = i
                        MSH1.TextMatrix(i, 1) = Panel.pNumber & "; " & Panel.RelatedDoubleHullPanel
                        MSH1.TextMatrix(i, 2) = colScantlings.Item(Panel.pNumber).cPrimaryFrames.WebHeight * 1000
                        MSH1.Row = i: MSH1.col = 2: MSH1.CellForeColor = &H80000011
                        MSH1.TextMatrix(i, 3) = colScantlings.Item(Panel.pNumber).cPrimaryFrames.WebThickness * 1000
                        MSH1.TextMatrix(i, 4) = colScantlings.Item(Panel.pNumber).cPrimaryFrames.Spacing * 1000
                        MSH1.TextMatrix(i, 5) = colScantlings.Item(Panel.pNumber).cPrimaryFrames.CorrosionThickness * 1000
                    End If
                End If
            Next Panel
        Case "FramesSet2"
            For Each Panel In colPanel
                If Panel.pType = DoubleHull Then
                    If Panel.RelatedDoubleHullPanel > Panel.pNumber Then
                        i = i + 1: j = j + 1
                        MSH1.TextMatrix(i, 0) = j
                        MSH1.TextMatrix(i, 1) = Panel.pNumber
                        MSH1.TextMatrix(i, 2) = colScantlings.Item(Panel.pNumber).cSecondaryFrames.WebHeight * 1000
                        MSH1.TextMatrix(i, 3) = colScantlings.Item(Panel.pNumber).cSecondaryFrames.WebThickness * 1000
                        MSH1.TextMatrix(i, 4) = colScantlings.Item(Panel.pNumber).cSecondaryFrames.FlangeWidth * 1000
                        MSH1.TextMatrix(i, 5) = colScantlings.Item(Panel.pNumber).cSecondaryFrames.FlangeThickness * 1000
                        MSH1.TextMatrix(i, 6) = colScantlings.Item(Panel.pNumber).cSecondaryFrames.Spacing * 1000
                        Select Case colScantlings.Item(Panel.pNumber).cSecondaryFrames.Side
                            Case SideNone
                                MSH1.TextMatrix(i, 7) = "None"
                            Case SideLeft
                                MSH1.TextMatrix(i, 7) = "Left"
                            Case SideRight
                                MSH1.TextMatrix(i, 7) = "Right"
                        End Select
                        i = i + 1
                        MSH1.TextMatrix(i, 0) = ""
                        MSH1.TextMatrix(i, 1) = Panel.RelatedDoubleHullPanel
                        MSH1.TextMatrix(i, 2) = colScantlings.Item(Panel.RelatedDoubleHullPanel).cSecondaryFrames.WebHeight * 1000
                        MSH1.TextMatrix(i, 3) = colScantlings.Item(Panel.RelatedDoubleHullPanel).cSecondaryFrames.WebThickness * 1000
                        MSH1.TextMatrix(i, 4) = colScantlings.Item(Panel.RelatedDoubleHullPanel).cSecondaryFrames.FlangeWidth * 1000
                        MSH1.TextMatrix(i, 5) = colScantlings.Item(Panel.RelatedDoubleHullPanel).cSecondaryFrames.FlangeThickness * 1000
                        MSH1.TextMatrix(i, 6) = colScantlings.Item(Panel.RelatedDoubleHullPanel).cSecondaryFrames.Spacing * 1000
                        Select Case colScantlings.Item(Panel.RelatedDoubleHullPanel).cSecondaryFrames.Side
                            Case SideNone
                                MSH1.TextMatrix(i, 7) = "None"
                            Case SideLeft
                                MSH1.TextMatrix(i, 7) = "Left"
                            Case SideRight
                                MSH1.TextMatrix(i, 7) = "Right"
                        End Select
                    End If
                End If
            Next Panel
        Case "StiffenersSet2"
            For Each Panel In colPanel
                If Panel.pType = DoubleHull Then
                    If Panel.RelatedDoubleHullPanel > Panel.pNumber Then
                        i = i + 1: j = j + 1
                        MSH1.TextMatrix(i, 0) = j
                        MSH1.TextMatrix(i, 1) = Panel.pNumber
                        MSH1.TextMatrix(i, 2) = colScantlings.Item(Panel.pNumber).cSecondaryStiffeners.WebHeight * 1000
                        MSH1.TextMatrix(i, 3) = colScantlings.Item(Panel.pNumber).cSecondaryStiffeners.WebThickness * 1000
                        MSH1.TextMatrix(i, 4) = colScantlings.Item(Panel.pNumber).cSecondaryStiffeners.FlangeWidth * 1000
                        MSH1.TextMatrix(i, 5) = colScantlings.Item(Panel.pNumber).cSecondaryStiffeners.FlangeThickness * 1000
                        MSH1.TextMatrix(i, 6) = colScantlings.Item(Panel.pNumber).cSecondaryStiffeners.Spacing * 1000
                        Select Case colScantlings.Item(Panel.pNumber).cSecondaryStiffeners.Side
                            Case SideNone
                                MSH1.TextMatrix(i, 7) = "None"
                            Case SideLeft
                                MSH1.TextMatrix(i, 7) = "Left"
                            Case SideRight
                                MSH1.TextMatrix(i, 7) = "Right"
                        End Select
                        i = i + 1
                        MSH1.TextMatrix(i, 0) = ""
                        MSH1.TextMatrix(i, 1) = Panel.RelatedDoubleHullPanel
                        MSH1.TextMatrix(i, 2) = colScantlings.Item(Panel.RelatedDoubleHullPanel).cSecondaryStiffeners.WebHeight * 1000
                        MSH1.TextMatrix(i, 3) = colScantlings.Item(Panel.RelatedDoubleHullPanel).cSecondaryStiffeners.WebThickness * 1000
                        MSH1.TextMatrix(i, 4) = colScantlings.Item(Panel.RelatedDoubleHullPanel).cSecondaryStiffeners.FlangeWidth * 1000
                        MSH1.TextMatrix(i, 5) = colScantlings.Item(Panel.RelatedDoubleHullPanel).cSecondaryStiffeners.FlangeThickness * 1000
                        MSH1.TextMatrix(i, 6) = colScantlings.Item(Panel.RelatedDoubleHullPanel).cSecondaryStiffeners.Spacing * 1000
                        Select Case colScantlings.Item(Panel.RelatedDoubleHullPanel).cSecondaryStiffeners.Side
                            Case SideNone
                                MSH1.TextMatrix(i, 7) = "None"
                            Case SideLeft
                                MSH1.TextMatrix(i, 7) = "Left"
                            Case SideRight
                                MSH1.TextMatrix(i, 7) = "Right"
                        End Select
                    End If
                End If
            Next Panel
        Case "Girders"
            For i = 1 To 10
                MSH1.TextMatrix(i, 0) = i
            Next i
            If lstPanels.ListIndex > -1 Then
                For i = 1 To colScantlings.Item(PanelListIndex).colGirder.Count
                    MSH1.TextMatrix(i, 1) = colScantlings.Item(PanelListIndex).colGirder.Item(i).Distance
                    MSH1.TextMatrix(i, 2) = colScantlings.Item(PanelListIndex).colGirder.Item(i).WebHeight * 1000
                    MSH1.TextMatrix(i, 3) = colScantlings.Item(PanelListIndex).colGirder.Item(i).WebThickness * 1000
                    MSH1.TextMatrix(i, 4) = colScantlings.Item(PanelListIndex).colGirder.Item(i).FlangeWidth * 1000
                    MSH1.TextMatrix(i, 5) = colScantlings.Item(PanelListIndex).colGirder.Item(i).FlangeThickness * 1000
                Next i
            End If
            lblPanelLength.Caption = "Panel Length :" & vbCrLf & colPanel.Item(PanelListIndex).cGeometry.PanelWidth & " [m]"
            Select Case colScantlings.Item(PanelListIndex).GirderSide
                Case SideLeft
                    cbSide.Text = "Left"
                Case SideRight
                    cbSide.Text = "Right"
                Case SideNone
                    cbSide.Text = "Right"
                Case Else
            End Select
        Case "Materials"
            For Each Panel In colPanel
                If Panel.pType = DoubleHull Then
                    If Panel.RelatedDoubleHullPanel > Panel.pNumber Then
                        i = i + 1: j = j + 1
                        MSH1.TextMatrix(i, 0) = j
                        MSH1.TextMatrix(i, 1) = Panel.pNumber
                        MSH1.TextMatrix(i, 2) = colMaterials.Item(Panel.pNumber).YoungModulus * 0.000001
                        MSH1.TextMatrix(i, 3) = colMaterials.Item(Panel.pNumber).Poisson
                        MSH1.TextMatrix(i, 4) = Round(colMaterials.Item(Panel.pNumber).YieldStress * 0.000001, 1)
                        MSH1.TextMatrix(i, 5) = Round(colMaterials.Item(Panel.pNumber).AllowableStress * 0.000001, 1)
                        MSH1.TextMatrix(i, 6) = colMaterials.Item(Panel.pNumber).SpecificWeight * 0.0001
                        i = i + 1
                        MSH1.TextMatrix(i, 0) = ""
                        MSH1.TextMatrix(i, 1) = Panel.RelatedDoubleHullPanel
                        MSH1.TextMatrix(i, 2) = colMaterials.Item(Panel.RelatedDoubleHullPanel).YoungModulus * 0.000001
                        MSH1.TextMatrix(i, 3) = colMaterials.Item(Panel.RelatedDoubleHullPanel).Poisson
                        MSH1.TextMatrix(i, 4) = Round(colMaterials.Item(Panel.RelatedDoubleHullPanel).YieldStress * 0.000001, 1)
                        MSH1.TextMatrix(i, 5) = Round(colMaterials.Item(Panel.RelatedDoubleHullPanel).AllowableStress * 0.000001, 1)
                        MSH1.TextMatrix(i, 6) = colMaterials.Item(Panel.RelatedDoubleHullPanel).SpecificWeight * 0.0001
                    End If
                End If
            Next Panel
        Case "Participation"
            For Each Panel In colPanel
                If Panel.pType = DoubleHull Then
                    If Panel.RelatedDoubleHullPanel > Panel.pNumber Then
                        i = i + 1: j = j + 1
                        MSH1.TextMatrix(i, 0) = j
                        MSH1.TextMatrix(i, 1) = Panel.pNumber
                        MSH1.TextMatrix(i, 2) = colGeometry.Item(Panel.pNumber).Participation
                        i = i + 1
                        MSH1.TextMatrix(i, 0) = ""
                        MSH1.TextMatrix(i, 1) = Panel.RelatedDoubleHullPanel
                        MSH1.TextMatrix(i, 2) = colGeometry.Item(Panel.RelatedDoubleHullPanel).Participation
                    End If
                End If
            Next Panel
    End Select
End Sub

Private Sub GetData()
    Set colPanel = Project.Item(ProjectIndex).colPanel
    Set colNodes = Project.Item(ProjectIndex).colNodes
    Set colGeometry = New Collection
    Set colScantlings = New Collection
    Set colMaterials = New Collection
    Set colRelatedPanels = New Collection
    For Each Panel In colPanel
        colRelatedPanels.Add Panel.RelatedDoubleHullPanel
        colGeometry.Add Panel.cGeometry.Clone
        colScantlings.Add Panel.cScantlings.Clone
        colMaterials.Add Panel.cMaterial.Clone
    Next Panel
End Sub

Private Sub SetData()
    On Error GoTo SetDataErr
    Dim i As Integer
    i = 0
    Select Case TabStrip.SelectedItem.Key
        Case "Thickness"
            For Each Panel In colPanel
                If Panel.pType = DoubleHull Then
                    If Panel.RelatedDoubleHullPanel > Panel.pNumber Then
                        i = i + 1
                        colScantlings.Item(Panel.pNumber).NetThickness = MSH1.TextMatrix(i, 2) / 1000
                        colScantlings.Item(Panel.pNumber).CorrosionThickness = MSH1.TextMatrix(i, 3) / 1000
                        i = i + 1
                        colScantlings.Item(Panel.RelatedDoubleHullPanel).NetThickness = MSH1.TextMatrix(i, 2) / 1000
                        colScantlings.Item(Panel.RelatedDoubleHullPanel).CorrosionThickness = MSH1.TextMatrix(i, 3) / 1000
                    End If
                End If
            Next Panel
        Case "StiffenersSet1"
            For Each Panel In colPanel
                If Panel.pType = DoubleHull Then
                    If Panel.RelatedDoubleHullPanel > Panel.pNumber Then
                        i = i + 1
                        colScantlings.Item(Panel.pNumber).cPrimaryStiffeners.DistributionMode = MSH1.TextMatrix(i, 2)
                        colScantlings.Item(Panel.pNumber).cPrimaryStiffeners.WebHeight = MSH1.TextMatrix(i, 3) / 1000
                        colScantlings.Item(Panel.pNumber).cPrimaryStiffeners.WebThickness = MSH1.TextMatrix(i, 4) / 1000
                        colScantlings.Item(Panel.pNumber).cPrimaryStiffeners.FlangeWidth = MSH1.TextMatrix(i, 5) / 1000
                        colScantlings.Item(Panel.pNumber).cPrimaryStiffeners.FlangeThickness = MSH1.TextMatrix(i, 6) / 1000
                        colScantlings.Item(Panel.pNumber).cPrimaryStiffeners.Spacing = MSH1.TextMatrix(i, 7) / 1000
                        colScantlings.Item(Panel.pNumber).cPrimaryStiffeners.CorrosionThickness = MSH1.TextMatrix(i, 8) / 1000
                        Select Case MSH1.TextMatrix(i, 9)
                            Case "None"
                                colScantlings.Item(Panel.pNumber).cPrimaryStiffeners.Side = SideNone
                            Case "Left"
                                colScantlings.Item(Panel.pNumber).cPrimaryStiffeners.Side = SideLeft
                            Case "Right"
                                colScantlings.Item(Panel.pNumber).cPrimaryStiffeners.Side = SideRight
                        End Select
                        i = i + 1
                        colScantlings.Item(Panel.RelatedDoubleHullPanel).cPrimaryStiffeners.DistributionMode = MSH1.TextMatrix(i, 2)
                        colScantlings.Item(Panel.RelatedDoubleHullPanel).cPrimaryStiffeners.WebHeight = MSH1.TextMatrix(i, 3) / 1000
                        colScantlings.Item(Panel.RelatedDoubleHullPanel).cPrimaryStiffeners.WebThickness = MSH1.TextMatrix(i, 4) / 1000
                        colScantlings.Item(Panel.RelatedDoubleHullPanel).cPrimaryStiffeners.FlangeWidth = MSH1.TextMatrix(i, 5) / 1000
                        colScantlings.Item(Panel.RelatedDoubleHullPanel).cPrimaryStiffeners.FlangeThickness = MSH1.TextMatrix(i, 6) / 1000
                        colScantlings.Item(Panel.RelatedDoubleHullPanel).cPrimaryStiffeners.Spacing = MSH1.TextMatrix(i, 7) / 1000
                        colScantlings.Item(Panel.RelatedDoubleHullPanel).cPrimaryStiffeners.CorrosionThickness = MSH1.TextMatrix(i, 8) / 1000
                        Select Case MSH1.TextMatrix(i, 9)
                            Case "None"
                                colScantlings.Item(Panel.RelatedDoubleHullPanel).cPrimaryStiffeners.Side = SideNone
                            Case "Left"
                                colScantlings.Item(Panel.RelatedDoubleHullPanel).cPrimaryStiffeners.Side = SideLeft
                            Case "Right"
                                colScantlings.Item(Panel.RelatedDoubleHullPanel).cPrimaryStiffeners.Side = SideRight
                        End Select
                    End If
                End If
            Next Panel
        Case "FramesSet1"
            For Each Panel In colPanel
                If Panel.pType = DoubleHull Then
                    If Panel.RelatedDoubleHullPanel > Panel.pNumber Then
                        i = i + 1
                        colScantlings.Item(Panel.pNumber).cPrimaryFrames.WebHeight = MSH1.TextMatrix(i, 2) / 1000
                        colScantlings.Item(Panel.pNumber).cPrimaryFrames.WebThickness = MSH1.TextMatrix(i, 3) / 1000
                        colScantlings.Item(Panel.pNumber).cPrimaryFrames.Spacing = MSH1.TextMatrix(i, 4) / 1000
                        colScantlings.Item(Panel.pNumber).cPrimaryFrames.CorrosionThickness = MSH1.TextMatrix(i, 5) / 1000
                        colScantlings.Item(Panel.RelatedDoubleHullPanel).cPrimaryFrames.WebHeight = MSH1.TextMatrix(i, 2) / 1000
                        colScantlings.Item(Panel.RelatedDoubleHullPanel).cPrimaryFrames.WebThickness = MSH1.TextMatrix(i, 3) / 1000
                        colScantlings.Item(Panel.RelatedDoubleHullPanel).cPrimaryFrames.Spacing = MSH1.TextMatrix(i, 4) / 1000
                        colScantlings.Item(Panel.RelatedDoubleHullPanel).cPrimaryFrames.CorrosionThickness = MSH1.TextMatrix(i, 5) / 1000
                    End If
                End If
            Next Panel
        Case "FramesSet2"
            For Each Panel In colPanel
                If Panel.pType = DoubleHull Then
                    If Panel.RelatedDoubleHullPanel > Panel.pNumber Then
                        i = i + 1
                        colScantlings.Item(Panel.pNumber).cSecondaryFrames.WebHeight = MSH1.TextMatrix(i, 2) / 1000
                        colScantlings.Item(Panel.pNumber).cSecondaryFrames.WebThickness = MSH1.TextMatrix(i, 3) / 1000
                        colScantlings.Item(Panel.pNumber).cSecondaryFrames.FlangeWidth = MSH1.TextMatrix(i, 4) / 1000
                        colScantlings.Item(Panel.pNumber).cSecondaryFrames.FlangeThickness = MSH1.TextMatrix(i, 5) / 1000
                        colScantlings.Item(Panel.pNumber).cSecondaryFrames.Spacing = MSH1.TextMatrix(i, 6) / 1000
                        Select Case MSH1.TextMatrix(i, 7)
                            Case "None"
                                colScantlings.Item(Panel.pNumber).cSecondaryFrames.Side = SideNone
                            Case "Left"
                                colScantlings.Item(Panel.pNumber).cSecondaryFrames.Side = SideLeft
                            Case "Right"
                                colScantlings.Item(Panel.pNumber).cSecondaryFrames.Side = SideRight
                        End Select
                        i = i + 1
                        colScantlings.Item(Panel.RelatedDoubleHullPanel).cSecondaryFrames.WebHeight = MSH1.TextMatrix(i, 2) / 1000
                        colScantlings.Item(Panel.RelatedDoubleHullPanel).cSecondaryFrames.WebThickness = MSH1.TextMatrix(i, 3) / 1000
                        colScantlings.Item(Panel.RelatedDoubleHullPanel).cSecondaryFrames.FlangeWidth = MSH1.TextMatrix(i, 4) / 1000
                        colScantlings.Item(Panel.RelatedDoubleHullPanel).cSecondaryFrames.FlangeThickness = MSH1.TextMatrix(i, 5) / 1000
                        colScantlings.Item(Panel.RelatedDoubleHullPanel).cSecondaryFrames.Spacing = MSH1.TextMatrix(i, 6) / 1000
                        Select Case MSH1.TextMatrix(i, 7)
                            Case "None"
                                colScantlings.Item(Panel.RelatedDoubleHullPanel).cSecondaryFrames.Side = SideNone
                            Case "Left"
                                colScantlings.Item(Panel.RelatedDoubleHullPanel).cSecondaryFrames.Side = SideLeft
                            Case "Right"
                                colScantlings.Item(Panel.RelatedDoubleHullPanel).cSecondaryFrames.Side = SideRight
                        End Select
                    End If
                End If
            Next Panel
        Case "StiffenersSet2"
            For Each Panel In colPanel
                If Panel.pType = DoubleHull Then
                    If Panel.RelatedDoubleHullPanel > Panel.pNumber Then
                        i = i + 1
                        colScantlings.Item(Panel.pNumber).cSecondaryStiffeners.WebHeight = MSH1.TextMatrix(i, 2) / 1000
                        colScantlings.Item(Panel.pNumber).cSecondaryStiffeners.WebThickness = MSH1.TextMatrix(i, 3) / 1000
                        colScantlings.Item(Panel.pNumber).cSecondaryStiffeners.FlangeWidth = MSH1.TextMatrix(i, 4) / 1000
                        colScantlings.Item(Panel.pNumber).cSecondaryStiffeners.FlangeThickness = MSH1.TextMatrix(i, 5) / 1000
                        colScantlings.Item(Panel.pNumber).cSecondaryStiffeners.Spacing = MSH1.TextMatrix(i, 6) / 1000
                        Select Case MSH1.TextMatrix(i, 7)
                            Case "None"
                                colScantlings.Item(Panel.pNumber).cSecondaryStiffeners.Side = SideNone
                            Case "Left"
                                colScantlings.Item(Panel.pNumber).cSecondaryStiffeners.Side = SideLeft
                            Case "Right"
                                colScantlings.Item(Panel.pNumber).cSecondaryStiffeners.Side = SideRight
                        End Select
                        i = i + 1
                        colScantlings.Item(Panel.RelatedDoubleHullPanel).cSecondaryStiffeners.WebHeight = MSH1.TextMatrix(i, 2) / 1000
                        colScantlings.Item(Panel.RelatedDoubleHullPanel).cSecondaryStiffeners.WebThickness = MSH1.TextMatrix(i, 3) / 1000
                        colScantlings.Item(Panel.RelatedDoubleHullPanel).cSecondaryStiffeners.FlangeWidth = MSH1.TextMatrix(i, 4) / 1000
                        colScantlings.Item(Panel.RelatedDoubleHullPanel).cSecondaryStiffeners.FlangeThickness = MSH1.TextMatrix(i, 5) / 1000
                        colScantlings.Item(Panel.RelatedDoubleHullPanel).cSecondaryStiffeners.Spacing = MSH1.TextMatrix(i, 6) / 1000
                        Select Case MSH1.TextMatrix(i, 7)
                            Case "None"
                                colScantlings.Item(Panel.RelatedDoubleHullPanel).cSecondaryStiffeners.Side = SideNone
                            Case "Left"
                                colScantlings.Item(Panel.RelatedDoubleHullPanel).cSecondaryStiffeners.Side = SideLeft
                            Case "Right"
                                colScantlings.Item(Panel.RelatedDoubleHullPanel).cSecondaryStiffeners.Side = SideRight
                        End Select
                    End If
                End If
            Next Panel
        Case "Girders"
            UpdateGirders
            'PopulatePanelList
        Case "Materials"
            For Each Panel In colPanel
                If Panel.pType = DoubleHull Then
                    If Panel.RelatedDoubleHullPanel > Panel.pNumber Then
                        i = i + 1
                        colMaterials.Item(Panel.pNumber).YoungModulus = MSH1.TextMatrix(i, 2) / 0.000001
                        colMaterials.Item(Panel.pNumber).Poisson = MSH1.TextMatrix(i, 3)
                        colMaterials.Item(Panel.pNumber).YieldStress = MSH1.TextMatrix(i, 4) / 0.000001
                        colMaterials.Item(Panel.pNumber).AllowableStress = MSH1.TextMatrix(i, 5) / 0.000001
                        colMaterials.Item(Panel.pNumber).SpecificWeight = MSH1.TextMatrix(i, 6) / 0.0001
                        i = i + 1
                        colMaterials.Item(Panel.RelatedDoubleHullPanel).YoungModulus = MSH1.TextMatrix(i, 2) / 0.000001
                        colMaterials.Item(Panel.RelatedDoubleHullPanel).Poisson = MSH1.TextMatrix(i, 3)
                        colMaterials.Item(Panel.RelatedDoubleHullPanel).YieldStress = MSH1.TextMatrix(i, 4) / 0.000001
                        colMaterials.Item(Panel.RelatedDoubleHullPanel).AllowableStress = MSH1.TextMatrix(i, 5) / 0.000001
                        colMaterials.Item(Panel.RelatedDoubleHullPanel).SpecificWeight = MSH1.TextMatrix(i, 6) / 0.0001
                    End If
                End If
            Next Panel
        Case "Participation"
            For Each Panel In colPanel
                If Panel.pType = DoubleHull Then
                    If Panel.RelatedDoubleHullPanel > Panel.pNumber Then
                        i = i + 1
                        colGeometry.Item(Panel.pNumber).Participation = MSH1.TextMatrix(i, 2)
                        i = i + 1
                        colGeometry.Item(Panel.RelatedDoubleHullPanel).Participation = MSH1.TextMatrix(i, 2)
                    End If
                End If
            Next Panel
    End Select
    FillGrid
    Exit Sub
SetDataErr:
    MsgBox "Invalid format", vbCritical + vbOKOnly, "Error"
    FillGrid
End Sub

Private Sub FlexGrid()
    Dim i As Integer
    MSH1.Clear
    MSH1.Rows = colPanel.GetNoOfDoubleHulls * 2 + 1
    'MSH1.Rows = colPanel.GetNoOfPlates + 1
    MSH1.RowHeight(0) = 690
    MSH1.ForeColor = vbBlack
    MSH1.FixedCols = 2
    picGirders.Visible = False
    Select Case TabStrip.SelectedItem.Key
        Case "Geometry"
            MSH1.ForeColor = &H80000011
            MSH1.Rows = colPanel.GetNoOfDoubleHulls + 1
            MSH1.Cols = 7
            MSH1.FormatString = "^|^|^|^|^|^|^"
            MSH1.ColWidth(1) = 900
            MSH1.ColWidth(2) = 900
            MSH1.ColWidth(3) = 900
            MSH1.ColWidth(4) = 900
            MSH1.ColWidth(5) = 900
            MSH1.ColWidth(6) = 900
            MSH1.TextMatrix(0, 0) = "Double Hull"
            MSH1.TextMatrix(0, 1) = "Panels"
            MSH1.TextMatrix(0, 2) = "Width" & vbCrLf & "[m]"
            MSH1.TextMatrix(0, 3) = "Height" & vbCrLf & "[m]"
            MSH1.TextMatrix(0, 4) = "Angle "
            MSH1.TextMatrix(0, 5) = "First" & vbCrLf & "Panel"
            MSH1.TextMatrix(0, 6) = "Second" & vbCrLf & "Panel"
        Case "Thickness"
            MSH1.Cols = 3
            MSH1.FormatString = "^|^|^|^"
            MSH1.ColWidth(1) = 750
            MSH1.ColWidth(2) = 900
            MSH1.ColWidth(3) = 900
            MSH1.TextMatrix(0, 0) = "Double Hull"
            MSH1.TextMatrix(0, 1) = "Panel"
            MSH1.TextMatrix(0, 2) = "Net" & vbCrLf & "Thickness" & vbCrLf & "[mm]"
            MSH1.TextMatrix(0, 3) = "Corrosion" & vbCrLf & "Thickness" & vbCrLf & "[mm]"
        Case "StiffenersSet1"
            MSH1.Cols = 9
            MSH1.FormatString = "^|^|^|^|^|^|^|^|^|^"
            MSH1.ColWidth(1) = 750
            MSH1.ColWidth(2) = 850
            MSH1.ColWidth(3) = 850
            MSH1.ColWidth(4) = 850
            MSH1.ColWidth(5) = 850
            MSH1.ColWidth(6) = 850
            MSH1.ColWidth(7) = 850
            MSH1.ColWidth(8) = 850
            MSH1.ColWidth(9) = 600
            MSH1.TextMatrix(0, 0) = "Double Hull"
            MSH1.TextMatrix(0, 1) = "Panel"
            MSH1.TextMatrix(0, 2) = "Distribution" & vbCrLf & "Mode"
            MSH1.TextMatrix(0, 3) = "Web" & vbCrLf & "Height" & vbCrLf & "[mm]"
            MSH1.TextMatrix(0, 4) = "Web" & vbCrLf & "Thickness" & vbCrLf & "[mm]"
            MSH1.TextMatrix(0, 5) = "Flange" & vbCrLf & "Width" & vbCrLf & "[mm]"
            MSH1.TextMatrix(0, 6) = "Flange" & vbCrLf & "Thickness" & vbCrLf & "[mm]"
            MSH1.TextMatrix(0, 7) = "Spacing" & vbCrLf & "[mm]"
            MSH1.TextMatrix(0, 8) = "Corrosion" & vbCrLf & "Thickness" & vbCrLf & "[mm]"
            MSH1.TextMatrix(0, 9) = "Side"
        Case "FramesSet1"
            MSH1.Cols = 5
            MSH1.Rows = colPanel.GetNoOfDoubleHulls + 1
            MSH1.FormatString = "^|^|^|^|^|^"
            MSH1.ColWidth(1) = 900
            MSH1.ColWidth(2) = 900
            MSH1.ColWidth(3) = 900
            MSH1.ColWidth(4) = 900
            MSH1.ColWidth(5) = 900
            MSH1.TextMatrix(0, 0) = "Double Hull"
            MSH1.TextMatrix(0, 1) = "Panels"
            MSH1.TextMatrix(0, 2) = "Floor" & vbCrLf & "Height" & vbCrLf & "[mm]"
            MSH1.TextMatrix(0, 3) = "Floor" & vbCrLf & "Thickness" & vbCrLf & "[mm]"
            MSH1.TextMatrix(0, 4) = "Spacing" & vbCrLf & "[mm]"
            MSH1.TextMatrix(0, 5) = "Corrosion" & vbCrLf & "Thickness" & vbCrLf & "[mm]"
        Case "FramesSet2", "StiffenersSet2"
            MSH1.Cols = 7
            MSH1.FormatString = "^|^|^|^|^|^|^|^"
            MSH1.ColWidth(1) = 900
            MSH1.ColWidth(2) = 900
            MSH1.ColWidth(3) = 900
            MSH1.ColWidth(4) = 900
            MSH1.ColWidth(5) = 900
            MSH1.ColWidth(6) = 900
            MSH1.ColWidth(7) = 600
            MSH1.TextMatrix(0, 0) = "Double Hull"
            MSH1.TextMatrix(0, 1) = "Panel"
            MSH1.TextMatrix(0, 2) = "Web" & vbCrLf & "Height" & vbCrLf & "[mm]"
            MSH1.TextMatrix(0, 3) = "Web" & vbCrLf & "Thickness" & vbCrLf & "[mm]"
            MSH1.TextMatrix(0, 4) = "Flange" & vbCrLf & "Width" & vbCrLf & "[mm]"
            MSH1.TextMatrix(0, 5) = "Flange" & vbCrLf & "Thickness" & vbCrLf & "[mm]"
            MSH1.TextMatrix(0, 6) = "Spacing" & vbCrLf & "[mm]"
            MSH1.TextMatrix(0, 7) = "Side"
        Case "Girders"
            MSH1.FixedCols = 1
            MSH1.Cols = 5
            MSH1.Rows = 11
            MSH1.FormatString = "^|^|^|^|^|^"
            MSH1.ColWidth(1) = 900
            MSH1.ColWidth(2) = 900
            MSH1.ColWidth(3) = 900
            MSH1.ColWidth(4) = 900
            MSH1.ColWidth(5) = 900
            MSH1.TextMatrix(0, 0) = "Girder"
            MSH1.TextMatrix(0, 1) = "Distance" & vbCrLf & "From In Node [m]"
            MSH1.TextMatrix(0, 2) = "Web" & vbCrLf & "Height" & vbCrLf & "[mm]"
            MSH1.TextMatrix(0, 3) = "Web" & vbCrLf & "Thickness" & vbCrLf & "[mm]"
            MSH1.TextMatrix(0, 4) = "Flange" & vbCrLf & "Width" & vbCrLf & "[mm]"
            MSH1.TextMatrix(0, 5) = "Flange" & vbCrLf & "Thickness" & vbCrLf & "[mm]"
            picGirders.Visible = True
            PopulateComboSide
        Case "Materials"
            MSH1.Cols = 6
            MSH1.FormatString = "^|^|^|^|^|^|^"
            MSH1.ColWidth(1) = 900
            MSH1.ColWidth(2) = 900
            MSH1.ColWidth(3) = 900
            MSH1.ColWidth(4) = 900
            MSH1.ColWidth(5) = 900
            MSH1.ColWidth(6) = 900
            MSH1.TextMatrix(0, 0) = "Double Hull"
            MSH1.TextMatrix(0, 1) = "Panel"
            MSH1.TextMatrix(0, 2) = "Young" & vbCrLf & "Modulus" & vbCrLf & "[N/mm2]"
            MSH1.TextMatrix(0, 3) = "Poisson" & vbCrLf & "Coefficient"
            MSH1.TextMatrix(0, 4) = "Yield" & vbCrLf & "Stress" & vbCrLf & "[N/mm2]"
            MSH1.TextMatrix(0, 5) = "Allowable" & vbCrLf & "Stress" & vbCrLf & "[N/mm2]"
            MSH1.TextMatrix(0, 6) = "Specific" & vbCrLf & "Weight" & vbCrLf & "[t/m3]"
        Case "Participation"
            MSH1.Cols = 3
            MSH1.FormatString = "^|^|^"
            MSH1.ColWidth(1) = 900
            MSH1.ColWidth(2) = 1000
            MSH1.TextMatrix(0, 0) = "Double Hull"
            MSH1.TextMatrix(0, 1) = "Panel"
            MSH1.TextMatrix(0, 2) = "Participation" & vbCrLf & "Coefficient"
    End Select
    MSH1.ColWidth(0) = 600
End Sub

Private Sub TabStripProperties()
    TabStrip.Tabs.Clear
    TabStrip.Tabs.Add 1, "Geometry", "Geometry"
    TabStrip.Tabs.Add 2, "Thickness", "Thickness"
    TabStrip.Tabs.Add 3, "StiffenersSet1", "Stiffeners Set 1"
    TabStrip.Tabs.Add 4, "FramesSet1", "Floors"
    TabStrip.Tabs.Add 5, "StiffenersSet2", "Stiffeners Set 2"
    TabStrip.Tabs.Add 6, "FramesSet2", "Frames Set 2"
    TabStrip.Tabs.Add 7, "Girders", "Girders"
    TabStrip.Tabs.Add 8, "Materials", "Materials"
    TabStrip.Tabs.Add 9, "Participation", "Participation"
    
End Sub

Private Sub PopulatePanelList()
    Dim index As Integer
    index = lstPanels.ListIndex
    lstPanels.Clear
    For Each Panel In colPanel
        If Panel.pType = DoubleHull Or Panel.pType = DoubleHull Then
            If colScantlings.Item(Panel.pNumber).colGirder.Count > 0 Then
                lstPanels.AddItem "Panel " & Panel.pNumber _
                    & " - " & colScantlings.Item(Panel.pNumber).colGirder.Count & " girder(s)"
            Else
                lstPanels.AddItem "Panel " & Panel.pNumber
            End If
        End If
    Next Panel
    lstPanels.ListIndex = index
End Sub

Private Sub PopulateCombo()
    Dim i As Integer
    Combo.Clear
    Select Case TabStrip.SelectedItem.Key
        Case "StiffenersSet1"
            Select Case MSH1.col
                Case 2
                    Combo.AddItem "EE1"
                    Combo.AddItem "EE2"
                Case 9
                    Combo.AddItem "None"
                    Combo.AddItem "Left"
                    Combo.AddItem "Right"
                Case Else
            End Select
        Case "StiffenersSet2", "FramesSet2"
            Select Case MSH1.col
                Case 7
                    Combo.AddItem "None"
                    Combo.AddItem "Left"
                    Combo.AddItem "Right"
                Case Else
            End Select
        Case Else
    End Select
End Sub

Private Sub PopulateComboSide()
    cbSide.Clear
    cbSide.AddItem "Left"
    cbSide.AddItem "Right"
End Sub

Private Sub cmdCancel_Click()
    Unload Me
End Sub

Private Sub cmdOK_Click()
    Dim y1 As Double, z1 As Double, y2 As Double, z2 As Double
    Select Case TabStrip.SelectedItem.Key
        Case "Girders"
            UpdateGirders
    End Select
    For Each Panel In colPanel
        Set Panel.cGeometry = colGeometry.Item(Panel.pNumber)
        y1 = colNodes.Item(Panel.cGeometry.InNode).Y
        z1 = colNodes.Item(Panel.cGeometry.InNode).z
        y2 = colNodes.Item(Panel.cGeometry.OutNode).Y
        z2 = colNodes.Item(Panel.cGeometry.OutNode).z
        GetLengthAngle y1, z1, y2, z2, Panel
        Set Panel.cScantlings = colScantlings(Panel.pNumber)
        Panel.cScantlings.cPrimaryStiffeners.GrossSectionModulus = GetSectionModulus(Panel.cScantlings.cPrimaryStiffeners.WebHeight, _
            Panel.cScantlings.cPrimaryStiffeners.WebThickness + Panel.cScantlings.cPrimaryStiffeners.CorrosionThickness, _
            Panel.cScantlings.cPrimaryStiffeners.FlangeWidth, _
            Panel.cScantlings.cPrimaryStiffeners.FlangeThickness + Panel.cScantlings.cPrimaryStiffeners.CorrosionThickness, _
            Panel.cScantlings.cPrimaryStiffeners.Spacing, _
            Panel.cScantlings.cPrimaryFrames.Spacing, _
            Panel.cScantlings.NetThickness + Panel.cScantlings.CorrosionThickness)

        Set Panel.cMaterial = colMaterials(Panel.pNumber)
    Next Panel
    UpdateBoundary ProjectIndex
    Draw ProjectIndex
    Project.Item(ProjectIndex).DataChanged = True
    Unload Me
End Sub

Private Function UpdateGirders()
    Dim i As Integer
    Dim j As Integer
    Dim index As Integer
    Dim Girder As cGirder, Girder1 As cGirder
    Dim dDistance As Double
    Dim colGirder1 As New colGirder
    If PanelListIndex <= 0 Then Exit Function
    For Each Girder In colScantlings.Item(PanelListIndex).colGirder
        colScantlings.Item(PanelListIndex).colGirder.Remove Girder.index
    Next Girder
    
    For i = 1 To MSH1.Rows - 1
        index = 0
        For j = 1 To MSH1.Cols - 1
            If IsNumeric(Val_(MSH1.TextMatrix(i, j))) = True And MSH1.TextMatrix(i, j) <> "" Then
                index = index + 1
            End If
        Next j
        If index = 5 Then
            Set Girder = New cGirder
            Girder.index = colScantlings.Item(PanelListIndex).colGirder.Count + 1
            Girder.Distance = MSH1.TextMatrix(i, 1)
            Girder.WebHeight = MSH1.TextMatrix(i, 2) / 1000
            Girder.WebThickness = MSH1.TextMatrix(i, 3) / 1000
            Girder.FlangeWidth = MSH1.TextMatrix(i, 4) / 1000
            Girder.FlangeThickness = MSH1.TextMatrix(i, 5) / 1000
            colScantlings.Item(PanelListIndex).colGirder.Add Girder, Girder.index
            Set Girder = Nothing
        End If
    Next i
    ' test if girders superposed
RewindTest:
    index = 0
    For Each Girder In colScantlings.Item(PanelListIndex).colGirder
        index = index + 1
        Girder.index = index
        colGirder1.Add Girder, index
    Next Girder
    Set colScantlings.Item(PanelListIndex).colGirder = colGirder1
    For Each Girder In colScantlings.Item(PanelListIndex).colGirder
        'dDistance = Girder.Distance
        For Each Girder1 In colScantlings.Item(PanelListIndex).colGirder
            If Girder.index < Girder1.index Then
                If Girder.Distance = Girder1.Distance Then
                    colScantlings.Item(PanelListIndex).colGirder.Remove Girder1.index
                    Set colGirder1 = Nothing
                    GoTo RewindTest
                End If
            End If
        Next Girder1
    Next Girder
    Set colGirder1 = Nothing
    'PopulatePanelList
End Function

' =============
' FLEXGRID EDIT
' =============
Function grd_index(r As Integer, c As Integer)
    grd_index = c + MSH1.Cols * r
End Function

Private Sub Form_Unload(Cancel As Integer)
    fMainForm.SetFocus
End Sub

Private Sub lstPanels_Click()
    Dim v() As Variant
    Dim sData As String
    Select Case TabStrip.SelectedItem.Key
        Case "Girders"
            UpdateGirders
    End Select
    sData = lstPanels.Text
    GetValues 2, sData, v
    PanelListIndex = Val_(v(2))
    MSH1.Clear
    FlexGrid
    FillGrid
    If lstPanels.Visible = True Then lstPanels.SetFocus
End Sub

Private Sub lstPanels_GotFocus()
    'UpdateGirders
End Sub

Private Sub MSH1_KeyDown(KeyCode As Integer, Shift As Integer)
    Dim i As Integer
    'If Len(MSH1) > 0 Then dBuffer = (MSH1)
    Select Case TabStrip.SelectedItem.Key
        Case "Girders"
            If KeyCode = 46 Then
                For i = 1 To MSH1.Cols - 1
                    MSH1.TextMatrix(MSH1.Row, i) = ""
                Next i
                UpdateGirders
                Exit Sub
            End If
            TxtEditGirders_KeyDown KeyCode, Shift
        Case Else
            TxtEdit_KeyDown KeyCode, Shift
    End Select
End Sub

Private Sub MSH1_KeyPress(KeyAscii As Integer)
    Select Case KeyAscii
        Case 3 'CTRL + C
            FlexCopy MSH1
            Exit Sub
        Case 22 'CTRL + V
            FlexPaste MSH1: SetData
            Exit Sub
        Case Else
    End Select
        
    Select Case MSH1.CellForeColor
            Case &H80000011
                Exit Sub
        End Select
        Select Case TabStrip.SelectedItem.Key
        Case "Geometry"
        Case "Thickness"
            MSHFlexGridEdit MSH1, TxtEdit, KeyAscii
        Case "StiffenersSet1"
            Select Case MSH1.col
                Case 2
                    PopulateCombo
                    MSHFlexGridEdit MSH1, Combo, KeyAscii
                Case 9
                    PopulateCombo
                    MSHFlexGridEdit MSH1, Combo, KeyAscii
                Case Else
                    MSHFlexGridEdit MSH1, TxtEdit, KeyAscii
            End Select
        Case "FramesSet1"
                    MSHFlexGridEdit MSH1, TxtEdit, KeyAscii
        Case "StiffenersSet2", "FramesSet2"
            Select Case MSH1.col
                Case 7
                    PopulateCombo
                    MSHFlexGridEdit MSH1, Combo, KeyAscii
                Case Else
                    MSHFlexGridEdit MSH1, TxtEdit, KeyAscii
            End Select
        Case "Materials", "Participation"
            MSHFlexGridEdit MSH1, TxtEdit, KeyAscii
        Case "Girders"
            MSHFlexGridEdit MSH1, TxtEditGirders, KeyAscii
    End Select

End Sub

Private Sub MSH1_DblClick()
    Select Case MSH1.CellForeColor
        Case &H80000011
            Exit Sub
    End Select
    Select Case TabStrip.SelectedItem.Key
        Case "Thickness"
            MSHFlexGridEdit MSH1, TxtEdit, 32
        Case "StiffenersSet1"
            Select Case MSH1.col
                Case 2
                    PopulateCombo
                    MSHFlexGridEdit MSH1, Combo, 32
                Case 9
                    PopulateCombo
                    MSHFlexGridEdit MSH1, Combo, 32
                Case Else
                    MSHFlexGridEdit MSH1, TxtEdit, 32
            End Select
        Case "FramesSet1"
            MSHFlexGridEdit MSH1, TxtEdit, 32
        Case "StiffenersSet2", "FramesSet2"
            Select Case MSH1.col
                Case 7
                    PopulateCombo
                    MSHFlexGridEdit MSH1, Combo, 32
                Case Else
                    MSHFlexGridEdit MSH1, TxtEdit, 32
            End Select
        Case "Materials", "Participation"
            MSHFlexGridEdit MSH1, TxtEdit, 32
        Case "Girders"
            MSHFlexGridEdit MSH1, TxtEditGirders, 32
    End Select
End Sub

Sub MSHFlexGridEdit(MSHFlexgrid As Control, edt As Control, KeyAscii As Integer)
    On Error GoTo MSHFlexGridEditErr
    Select Case edt.Name
        Case "Combo"
            Select Case KeyAscii 'tasta apasata
                Case 0 To 32
                Case Else
            End Select
            edt.Move MSHFlexgrid.Left + MSHFlexgrid.CellLeft - 20, _
                MSHFlexgrid.Top + MSHFlexgrid.CellTop - 50, _
                MSHFlexgrid.CellWidth + 7
            edt.Visible = True
            edt.Text = MSH1
            edt.SetFocus
            dropme Combo
        Case "TxtEdit", "TxtEditGirders"
            Select Case KeyAscii 'tasta apasata
                Case 0 To 32
                    edt = MSHFlexgrid
                    edt.SelStart = 1000
                Case Else
                    edt = Chr(KeyAscii)
                    edt.SelStart = 1
            End Select
            edt.Move MSHFlexgrid.Left + MSHFlexgrid.CellLeft, _
                MSHFlexgrid.Top + MSHFlexgrid.CellTop, _
                MSHFlexgrid.CellWidth - 8, _
                MSHFlexgrid.CellHeight - 8
            edt.Visible = True
            edt.SetFocus
    End Select
    Exit Sub
MSHFlexGridEditErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmDoubleHullScantlings: Sub MSHFlexGridEdit")
End Sub

Private Sub MSH1_Scroll()
    TxtEdit.Visible = False
    TxtEditGirders.Visible = False
    Combo.Visible = False
End Sub

Private Sub MSH1_Validate(Cancel As Boolean)
    Select Case TabStrip.SelectedItem.Key
        Case "Girders"
            UpdateGirders
    End Select
End Sub

Private Sub TabStrip_Change()
'    TxtEdit.Visible = False
'    TxtEditGirders.Visible = False
'    Combo.Visible = False
'    MSH1.Row = 1
'    MSH1.col = 1
'    FlexGrid
'    FillGrid
End Sub

Private Sub TabStrip_BeforeClick(Cancel As Integer)
    If TxtEditGirders.Visible = True Then
        Cancel = -1
        MSH1 = TxtEditGirders
        UpdateGirders
   End If
    If TxtEdit.Visible = True Then
       Cancel = -1
    End If
    If Combo.Visible = True Then
        Cancel = -1
    End If
End Sub

Private Sub TabStrip_Click()
    TxtEdit.Visible = False
    TxtEditGirders.Visible = False
    Combo.Visible = False
    MSH1.Row = 1
    MSH1.col = 1
    FlexGrid
    FillGrid
End Sub

Private Sub TxtEdit_KeyPress(KeyAscii As Integer)
    If KeyAscii = Asc(vbCr) Then KeyAscii = 0
End Sub

Private Sub TxtEditGirders_KeyPress(KeyAscii As Integer)
    If KeyAscii = Asc(vbCr) Then KeyAscii = 0
End Sub

Private Sub Combo_KeyPress(KeyAscii As Integer)
    If KeyAscii = Asc(vbCr) Then KeyAscii = 0
End Sub

Private Sub TxtEdit_KeyDown(KeyCode As Integer, Shift As Integer)
    If Len(MSH1) > 0 Then dBuffer = (MSH1)
    cmdOK.Default = False
    EditKeyCode MSH1, TxtEdit, KeyCode, Shift
End Sub

Private Sub TxtEditGirders_KeyDown(KeyCode As Integer, Shift As Integer)
    If Len(MSH1) > 0 Then
        dBuffer = (MSH1)
    Else
        dBuffer = ""
    End If
    cmdOK.Default = False
    EditKeyCode MSH1, TxtEditGirders, KeyCode, Shift
End Sub

Private Sub Combo_KeyDown(KeyCode As Integer, Shift As Integer)
    EditKeyCode MSH1, Combo, KeyCode, Shift
End Sub

Sub EditKeyCode(MSHFlexgrid As Control, edt As Control, KeyCode As Integer, Shift As Integer)
    Select Case KeyCode
    Case 27 ' esc
        edt.Visible = False
        MSHFlexgrid.SetFocus
    Case 13 'enter
        If TxtEdit.Visible = True Then
            TxtEdit_Validate False
        ElseIf TxtEditGirders.Visible = True Then
            TxtEditGirders_Validate False
        End If
        MSHFlexgrid.SetFocus
    Case 38 'up
        If TxtEdit.Visible = True Then
            TxtEdit_Validate False
        ElseIf TxtEditGirders.Visible = True Then
            TxtEditGirders_Validate False
        End If
        MSHFlexgrid.SetFocus
        'DoEvents
        If MSHFlexgrid.Row > MSHFlexgrid.FixedRows Then
           MSHFlexgrid.Row = MSHFlexgrid.Row - 1
        End If
    Case 40 'down
        If TxtEdit.Visible = True Then
            TxtEdit_Validate False
        ElseIf TxtEditGirders.Visible = True Then
            TxtEditGirders_Validate False
        End If
        MSHFlexgrid.SetFocus
        'DoEvents
        If MSHFlexgrid.Row < MSHFlexgrid.FixedRows - 1 Then
           MSHFlexgrid.Row = MSHFlexgrid.Row + 1
        End If
    End Select
End Sub

Private Sub MSH1_GotFocus()
    If TxtEdit.Visible = False Then GoTo 1 ' Exit Sub
    MSH1 = TxtEdit
    TxtEdit.Visible = False
1:
    If TxtEditGirders.Visible = False Then GoTo 2
    MSH1 = TxtEditGirders
    TxtEditGirders.Visible = False
2:
    If Combo.Visible = False Then Exit Sub
    MSH1 = Combo.List(Combo.ListIndex)
    Combo.Visible = False
End Sub

Private Sub MSH1_LeaveCell()
    If TxtEdit.Visible = False Then GoTo 1 'Exit Sub
    MSH1 = TxtEdit
    TxtEdit.Visible = False
1:
    If TxtEditGirders.Visible = False Then GoTo 2
    MSH1 = TxtEditGirders
    TxtEditGirders.Visible = False
2:
    If Combo.Visible = False Then Exit Sub
    MSH1 = Combo.List(Combo.ListIndex)
    Combo.Visible = False
End Sub

Private Sub TxtEdit_LostFocus()
    MSH1_GotFocus
    SetData
    cmdOK.Default = True
End Sub

Private Sub Combo_LostFocus()
    MSH1_GotFocus
    SetData
End Sub

Private Sub TxtEditGirders_LostFocus()
    MSH1_GotFocus
    cmdOK.Default = True
End Sub

Private Sub TxtEdit_Validate(Cancel As Boolean)
    ValidateNumeric TxtEdit, Cancel
    If Cancel = True Then TxtEdit = dBuffer
End Sub

Private Sub TxtEditGirders_Validate(Cancel As Boolean)
    Select Case MSH1.col
        Case 1
            ValidateNumericPozitiveOrNone TxtEditGirders, Cancel
            If Cancel = False Then
                If Val_(TxtEditGirders) > colGeometry.Item(PanelListIndex).PanelWidth Then
                    MsgBox "Distance greater than panel width.", vbCritical + vbOKOnly
                    Cancel = True
                    TxtEditGirders = dBuffer
                    Exit Sub
                End If
            End If
        Case Else
            ValidateNumericNoneNullPozitiveOrNone TxtEditGirders, Cancel
        End Select
    If Cancel = True Then TxtEditGirders = dBuffer
End Sub
