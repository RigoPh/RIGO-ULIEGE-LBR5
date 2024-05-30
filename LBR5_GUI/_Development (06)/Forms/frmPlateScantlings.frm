VERSION 5.00
Object = "{831FDD16-0C5C-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCTL.OCX"
Object = "{5E9E78A0-531B-11CF-91F6-C2863C385E30}#1.0#0"; "MSFLXGRD.OCX"
Object = "{0D452EE1-E08F-101A-852E-02608C4D0BB4}#2.0#0"; "FM20.DLL"
Begin VB.Form frmPlateScantlings 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Plate Scantlings"
   ClientHeight    =   4995
   ClientLeft      =   9885
   ClientTop       =   2685
   ClientWidth     =   8805
   Icon            =   "frmPlateScantlings.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   4995
   ScaleWidth      =   8805
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
      ItemData        =   "frmPlateScantlings.frx":000C
      Left            =   1560
      List            =   "frmPlateScantlings.frx":000E
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
      Width           =   8535
      _ExtentX        =   15055
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
      Width           =   8775
      _ExtentX        =   15478
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
   Begin MSForms.CommandButton cmdApply 
      Default         =   -1  'True
      Height          =   375
      Left            =   5160
      TabIndex        =   12
      Top             =   4440
      Width           =   1095
      Caption         =   "Apply"
      PicturePosition =   327683
      Size            =   "1931;661"
      Accelerator     =   65
      FontHeight      =   165
      FontCharSet     =   0
      FontPitchAndFamily=   2
      ParagraphAlign  =   3
   End
   Begin MSForms.CommandButton cmdCancel 
      Cancel          =   -1  'True
      Height          =   375
      Left            =   7560
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
      Height          =   375
      Left            =   6360
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
Attribute VB_Name = "frmPlateScantlings"
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
Dim Node As cNode, colNodes As colNodes
Dim irow As Integer, icol As Integer
Private irowclicked As Integer

Private Sub cbSide_Click()
    Select Case cbSide.Text
        Case "Left"
            colScantlings.Item(PanelListIndex).GirderSide = SideLeft
        Case "Right"
            colScantlings.Item(PanelListIndex).GirderSide = SideRight
    End Select
End Sub

Private Sub cmdApply_Click()
    Dim y1 As Double, z1 As Double, y2 As Double, z2 As Double
    Select Case TabStrip.SelectedItem.KEY
        Case "Girders"
            UpdateGirders
    End Select
    
    'Update Panel Nodes
    For Each Panel In colPanel
        Set Panel.cGeometry = colGeometry.Item(Panel.pNumber)
        y1 = colNodes.Item(Panel.cGeometry.InNode).y
        z1 = colNodes.Item(Panel.cGeometry.InNode).z
        y2 = colNodes.Item(Panel.cGeometry.OutNode).y
        z2 = colNodes.Item(Panel.cGeometry.OutNode).z
        GetLengthAngle y1, z1, y2, z2, Panel
        Set Panel.cScantlings = colScantlings(Panel.pNumber)
        Panel.cScantlings.cPrimaryStiffeners.GrossSectionModulus = GetSectionModulus(Panel.cScantlings.cPrimaryStiffeners.WebHeight, _
            Panel.cScantlings.cPrimaryStiffeners.WebThickness + Panel.cScantlings.cPrimaryStiffeners.CorrosionThickness, _
            Panel.cScantlings.cPrimaryStiffeners.FlangeWidth, _
            Panel.cScantlings.cPrimaryStiffeners.FlangeThickness + Panel.cScantlings.cPrimaryStiffeners.CorrosionThickness, _
            Panel.cScantlings.cPrimaryStiffeners.Spacing, _
            Panel.cScantlings.cPrimaryFrames.Spacing, _
            Panel.cScantlings.GrossThickness)
        Set Panel.cMaterial = colMaterials(Panel.pNumber)
    Next Panel
    
    'Remove exceding girders
    Dim cGirder As cGirder
    For Each Panel In colPanel
        For Each cGirder In Panel.cScantlings.colGirder
            If cGirder.Distance > Panel.cGeometry.PanelWidth Then
                Panel.cScantlings.colGirder.Remove cGirder.index
            End If
        Next cGirder
    Next Panel
    UpdateBoundary ProjectIndex
    UpdatePanelConnections ProjectIndex
    Draw ProjectIndex
    TestDesignVar ProjectIndex
    Project.Item(ProjectIndex).DataChanged = True
    'Unload Me
End Sub

Public Function TestDesignVar(ByVal pindex As Integer) As Boolean
    On Error GoTo TestDesignVarErr
'    PlateThickness = 1
'    FrameWebHeight = 2
'    FrameWebThickness = 3
'    FrameFlangeWidth = 4
'    FrameSpacing = 5
'    StiffenerWebHeight = 6
'    StiffenerWebThickness = 7
'    StiffenerFlangeWidth = 8
'    StiffenerSpacing = 9
    Dim oPanel As cPanel
    Dim oDes As cDesignVariables
    For Each oPanel In Project.Item(ProjectIndex).colPanel
        For Each oDes In oPanel.colDesignVariables
            Select Case oDes.VariableName
                Case 1
                    If oDes.LowerLimit > oPanel.cScantlings.NetThickness Then oDes.LowerLimit = oPanel.cScantlings.NetThickness
                    If oDes.UpperLimit < oPanel.cScantlings.NetThickness Then oDes.UpperLimit = oPanel.cScantlings.NetThickness
                Case 2
                    If oDes.LowerLimit > oPanel.cScantlings.cPrimaryFrames.WebHeight Then oDes.LowerLimit = oPanel.cScantlings.cPrimaryFrames.WebHeight
                    If oDes.UpperLimit < oPanel.cScantlings.cPrimaryFrames.WebHeight Then oDes.UpperLimit = oPanel.cScantlings.cPrimaryFrames.WebHeight
                Case 3
                    If oDes.LowerLimit > oPanel.cScantlings.cPrimaryFrames.WebThickness Then oDes.LowerLimit = oPanel.cScantlings.cPrimaryFrames.WebThickness
                    If oDes.UpperLimit < oPanel.cScantlings.cPrimaryFrames.WebThickness Then oDes.UpperLimit = oPanel.cScantlings.cPrimaryFrames.WebThickness
                Case 4
                    If oDes.LowerLimit > oPanel.cScantlings.cPrimaryFrames.FlangeWidth Then oDes.LowerLimit = oPanel.cScantlings.cPrimaryFrames.FlangeWidth
                    If oDes.UpperLimit < oPanel.cScantlings.cPrimaryFrames.FlangeWidth Then oDes.UpperLimit = oPanel.cScantlings.cPrimaryFrames.FlangeWidth
                Case 5
                    If oDes.LowerLimit > oPanel.cScantlings.cPrimaryFrames.Spacing Then oDes.LowerLimit = oPanel.cScantlings.cPrimaryFrames.Spacing
                    If oDes.UpperLimit < oPanel.cScantlings.cPrimaryFrames.Spacing Then oDes.UpperLimit = oPanel.cScantlings.cPrimaryFrames.Spacing
                Case 6
                    If oDes.LowerLimit > oPanel.cScantlings.cPrimaryStiffeners.WebHeight Then oDes.LowerLimit = oPanel.cScantlings.cPrimaryStiffeners.WebHeight
                    If oDes.UpperLimit < oPanel.cScantlings.cPrimaryStiffeners.WebHeight Then oDes.UpperLimit = oPanel.cScantlings.cPrimaryStiffeners.WebHeight
                Case 7
                    If oDes.LowerLimit > oPanel.cScantlings.cPrimaryStiffeners.WebThickness Then oDes.LowerLimit = oPanel.cScantlings.cPrimaryStiffeners.WebThickness
                    If oDes.UpperLimit < oPanel.cScantlings.cPrimaryStiffeners.WebThickness Then oDes.UpperLimit = oPanel.cScantlings.cPrimaryStiffeners.WebThickness
                Case 8
                    If oDes.LowerLimit > oPanel.cScantlings.cPrimaryStiffeners.FlangeWidth Then oDes.LowerLimit = oPanel.cScantlings.cPrimaryStiffeners.FlangeWidth
                    If oDes.UpperLimit < oPanel.cScantlings.cPrimaryStiffeners.FlangeWidth Then oDes.UpperLimit = oPanel.cScantlings.cPrimaryStiffeners.FlangeWidth
                Case 9
                    If oDes.LowerLimit > oPanel.cScantlings.cPrimaryStiffeners.Spacing Then oDes.LowerLimit = oPanel.cScantlings.cPrimaryStiffeners.Spacing
                    If oDes.UpperLimit < oPanel.cScantlings.cPrimaryStiffeners.Spacing Then oDes.UpperLimit = oPanel.cScantlings.cPrimaryStiffeners.Spacing
            End Select
        Next oDes
    Next oPanel
    Exit Function
TestDesignVarErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmPlateScantlings: Function TestDesignVar")
End Function

Private Sub cmdApply_DblClick(Cancel As MSForms.ReturnBoolean)
    Cancel = True
End Sub

Private Sub Combo_GotFocus()
    dBuffer = MSH1
    irow = MSH1.Row
    icol = MSH1.col
End Sub

Private Sub Combo_Validate(Cancel As Boolean)
   Select Case TabStrip.SelectedItem.KEY
    Case "Geometry"
        If MSH1.col = 3 Or MSH1.col = 4 Then
            If CheckPanelCollisions(irow) = True Then
                Cancel = True
                Combo.Text = dBuffer
            End If
        End If
        If MSH1.col = 5 Then
            If Cancel = True Then
                MsgBox "dude"
            End If

            Exit Sub
        End If
    End Select
End Sub

Private Sub Form_Load()
    mnuEdit.Visible = False
    ProjectIndex = ActiveProject
    Me.Move (Screen.Width - Me.Width) / 2, (Screen.Height - Me.Height) / 2
    cmdOK.Picture = LoadResPicture("ID_BITMAP_OK", 0)
    cmdCancel.Picture = LoadResPicture("ID_BITMAP_CANCEL", 0)
    Me.Caption = "Plate Scantlings - [" & GetFileName(Project.Item(ProjectIndex).sFileName) & "]"
    GetData
    PopulatePanelList
    lstPanels.ListIndex = 0
    TabStripProperties
    FlexGrid
    FillGrid
    'If irowclicked <> 0 Then lstPanels.ListIndex = irowclicked - 1
End Sub

Public Sub EntryWindow(ByVal index As Integer)
    Dim i As Integer, j As Integer
    Dim row1 As Integer, col1 As Integer 'buffer
    row1 = MSH1.Row
    col1 = MSH1.col
    If index = 0 Then Exit Sub
    irowclicked = index
    For i = 1 To MSH1.Rows - 1
        If Val(MSH1.TextMatrix(i, 0)) = index Then
            If Project.Item(ProjectIndex).colPanel.GetNoOfPlates > 11 Then MSH1.TopRow = i
            MSH1.Row = i
            MSH1.col = 0
            For j = 0 To MSH1.Cols - 1
            'MSH1.ColSel = MSH1.Cols - 1
               MSH1.col = j
               MSH1.CellBackColor = vbGreen
            Next j
        Exit For
        End If
    Next i
    MSH1.Row = row1
    MSH1.col = col1
End Sub

Private Sub Form_Resize()
    Me.Height = cmdOK.Top + cmdOK.Height + 600
End Sub

Private Sub MSH1_MouseDown(Button As Integer, Shift As Integer, x As Single, y As Single)
    If Button = 2 Then
        PopupMenu mnuEdit
    End If
End Sub

Private Sub mnuEditCopy_Click()
    FlexCopy MSH1
End Sub

Private Sub mnuEditPaste_Click()
    Select Case TabStrip.SelectedItem.KEY
        Case "Geometry"
            Exit Sub
    End Select
    FlexPaste MSH1
    'Combo_LostFocus
    SetData
End Sub

Private Function GetPositionCode(ByVal index As Integer) As String
    Select Case index
        Case 0
            GetPositionCode = "No Code"
        Case 1
            GetPositionCode = "1 Keel Plate"
        Case 2
            GetPositionCode = "2 Bottom"
        Case 3
            GetPositionCode = "3 Inner Bottom"
        Case 4
            GetPositionCode = "4 Bilge"
        Case 5
            GetPositionCode = "5 Side Shell - below freeboard deck"
        Case 6
            GetPositionCode = "6 Side Shell - above freeboard deck"
        Case 7
            GetPositionCode = "7 Inner Hull"
        Case 8
            GetPositionCode = "8 Upper Strength Deck"
        Case 9
            GetPositionCode = "9 Lower Deck"
        Case 10
            GetPositionCode = "10 Double hull Girder, tank & watertight bulkhead"
    End Select
End Function

Private Function GetPositionCodeIndex(ByRef code As String) As Integer
    Select Case code
        Case "No Code"
            GetPositionCodeIndex = 0
        Case "1 Keel Plate"
            GetPositionCodeIndex = 1
        Case "2 Bottom"
            GetPositionCodeIndex = 2
        Case "3 Inner Bottom"
            GetPositionCodeIndex = 3
        Case "4 Bilge"
            GetPositionCodeIndex = 4
        Case "5 Side Shell - below freeboard deck"
            GetPositionCodeIndex = 5
        Case "6 Side Shell - above freeboard deck"
            GetPositionCodeIndex = 6
        Case "7 Inner Hull"
            GetPositionCodeIndex = 7
        Case "8 Upper Strength Deck"
            GetPositionCodeIndex = 8
        Case "9 Lower Deck"
            GetPositionCodeIndex = 9
        Case "10 Double hull Girder, tank & watertight bulkhead"
            GetPositionCodeIndex = 10
    End Select
End Function

Private Sub FillGrid()
    Dim i As Integer, j As Integer
    i = 0
    
    Select Case TabStrip.SelectedItem.KEY
        Case "Geometry"
            For Each Panel In colPanel
                If Panel.pType = Plate Then
                    i = i + 1
                    MSH1.TextMatrix(i, 0) = Panel.pNumber
                    MSH1.TextMatrix(i, 1) = Round(colGeometry.Item(Panel.pNumber).PanelWidth, 6)
                    MSH1.Row = i: MSH1.col = 1: MSH1.CellForeColor = &H80000011
                    MSH1.TextMatrix(i, 2) = Round(colGeometry.Item(Panel.pNumber).PanelAngle, 6)
                    MSH1.Row = i: MSH1.col = 2: MSH1.CellForeColor = &H80000011
                    MSH1.TextMatrix(i, 3) = colGeometry.Item(Panel.pNumber).InNode
                    MSH1.TextMatrix(i, 4) = colGeometry.Item(Panel.pNumber).OutNode
                    'If Project.Item(ProjectIndex).cHeader.IANA = 2 Then
                        MSH1.TextMatrix(i, 5) = GetPositionCode(colGeometry.Item(Panel.pNumber).PositionCode)
                    'End If
                    
                End If
            Next Panel
        Case "Thickness"
            For Each Panel In colPanel
                If Panel.pType = Plate Then
                    i = i + 1
                    MSH1.TextMatrix(i, 0) = Panel.pNumber
                    MSH1.TextMatrix(i, 1) = colScantlings.Item(Panel.pNumber).NetThickness * 1000
                    MSH1.TextMatrix(i, 2) = colScantlings.Item(Panel.pNumber).CorrosionThickness * 1000
                End If
            Next Panel
        Case "StiffenersSet1"
            For Each Panel In colPanel
                If Panel.pType = Plate Then
                    i = i + 1
                    MSH1.TextMatrix(i, 0) = Panel.pNumber
                    MSH1.TextMatrix(i, 1) = colScantlings.Item(Panel.pNumber).cPrimaryStiffeners.DistributionMode
                    MSH1.TextMatrix(i, 2) = colScantlings.Item(Panel.pNumber).cPrimaryStiffeners.WebHeight * 1000
                    MSH1.TextMatrix(i, 3) = colScantlings.Item(Panel.pNumber).cPrimaryStiffeners.WebThickness * 1000
                    MSH1.TextMatrix(i, 4) = colScantlings.Item(Panel.pNumber).cPrimaryStiffeners.FlangeWidth * 1000
                    MSH1.TextMatrix(i, 5) = colScantlings.Item(Panel.pNumber).cPrimaryStiffeners.FlangeThickness * 1000
                    MSH1.TextMatrix(i, 6) = colScantlings.Item(Panel.pNumber).cPrimaryStiffeners.Spacing * 1000
                    MSH1.TextMatrix(i, 7) = colScantlings.Item(Panel.pNumber).cPrimaryStiffeners.CorrosionThickness * 1000
                    Select Case colScantlings.Item(Panel.pNumber).cPrimaryStiffeners.Side
                        Case SideNone
                            MSH1.TextMatrix(i, 8) = "None"
                        Case SideLeft
                            MSH1.TextMatrix(i, 8) = "Left"
                        Case SideRight
                            MSH1.TextMatrix(i, 8) = "Right"
                    End Select
                End If
            Next Panel
        Case "FramesSet1"
            For Each Panel In colPanel
                If Panel.pType = Plate Then
                    i = i + 1
                    MSH1.TextMatrix(i, 0) = Panel.pNumber
                    MSH1.TextMatrix(i, 1) = colScantlings.Item(Panel.pNumber).cPrimaryFrames.WebHeight * 1000
                    MSH1.TextMatrix(i, 2) = colScantlings.Item(Panel.pNumber).cPrimaryFrames.WebThickness * 1000
                    MSH1.TextMatrix(i, 3) = colScantlings.Item(Panel.pNumber).cPrimaryFrames.FlangeWidth * 1000
                    MSH1.TextMatrix(i, 4) = colScantlings.Item(Panel.pNumber).cPrimaryFrames.FlangeThickness * 1000
                    MSH1.TextMatrix(i, 5) = colScantlings.Item(Panel.pNumber).cPrimaryFrames.Spacing * 1000
                    MSH1.TextMatrix(i, 6) = colScantlings.Item(Panel.pNumber).cPrimaryFrames.CorrosionThickness * 1000
                    Select Case colScantlings.Item(Panel.pNumber).cPrimaryFrames.Side
                        Case SideNone
                            MSH1.TextMatrix(i, 7) = "None"
                        Case SideLeft
                            MSH1.TextMatrix(i, 7) = "Left"
                        Case SideRight
                            MSH1.TextMatrix(i, 7) = "Right"
                    End Select
                End If
            Next Panel
        Case "FramesSet2"
            For Each Panel In colPanel
                If Panel.pType = Plate Then
                    i = i + 1
                    MSH1.TextMatrix(i, 0) = Panel.pNumber
                    MSH1.TextMatrix(i, 1) = colScantlings.Item(Panel.pNumber).cSecondaryFrames.WebHeight * 1000
                    MSH1.TextMatrix(i, 2) = colScantlings.Item(Panel.pNumber).cSecondaryFrames.WebThickness * 1000
                    MSH1.TextMatrix(i, 3) = colScantlings.Item(Panel.pNumber).cSecondaryFrames.FlangeWidth * 1000
                    MSH1.TextMatrix(i, 4) = colScantlings.Item(Panel.pNumber).cSecondaryFrames.FlangeThickness * 1000
                    MSH1.TextMatrix(i, 5) = colScantlings.Item(Panel.pNumber).cSecondaryFrames.Spacing * 1000
                    Select Case colScantlings.Item(Panel.pNumber).cSecondaryFrames.Side
                        Case SideNone
                            MSH1.TextMatrix(i, 6) = "None"
                        Case SideLeft
                            MSH1.TextMatrix(i, 6) = "Left"
                        Case SideRight
                            MSH1.TextMatrix(i, 6) = "Right"
                    End Select
                End If
            Next Panel
        Case "StiffenersSet2"
            For Each Panel In colPanel
                If Panel.pType = Plate Then
                    i = i + 1
                    MSH1.TextMatrix(i, 0) = Panel.pNumber
                    MSH1.TextMatrix(i, 1) = colScantlings.Item(Panel.pNumber).cSecondaryStiffeners.WebHeight * 1000
                    MSH1.TextMatrix(i, 2) = colScantlings.Item(Panel.pNumber).cSecondaryStiffeners.WebThickness * 1000
                    MSH1.TextMatrix(i, 3) = colScantlings.Item(Panel.pNumber).cSecondaryStiffeners.FlangeWidth * 1000
                    MSH1.TextMatrix(i, 4) = colScantlings.Item(Panel.pNumber).cSecondaryStiffeners.FlangeThickness * 1000
                    MSH1.TextMatrix(i, 5) = colScantlings.Item(Panel.pNumber).cSecondaryStiffeners.Spacing * 1000
                    Select Case colScantlings.Item(Panel.pNumber).cSecondaryStiffeners.Side
                        Case SideNone
                            MSH1.TextMatrix(i, 6) = "None"
                        Case SideLeft
                            MSH1.TextMatrix(i, 6) = "Left"
                        Case SideRight
                            MSH1.TextMatrix(i, 6) = "Right"
                    End Select
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
            lblPanelLength.Caption = "Panel Width:" & vbCrLf & colPanel.Item(PanelListIndex).cGeometry.PanelWidth & " [m]"
            Select Case colScantlings.Item(PanelListIndex).GirderSide
                Case SideLeft
                    cbSide.Text = "Left"
                Case SideRight
                    cbSide.Text = "Right"
                Case SideNone
                    cbSide.Text = "Right"
                Case Else
            End Select
            'If irowclicked <> 0 Then lstPanels.ListIndex = irowclicked - 1
        Case "Materials"
            For Each Panel In colPanel
                If Panel.pType = Plate Then
                    i = i + 1
                    MSH1.TextMatrix(i, 0) = Panel.pNumber
                    MSH1.TextMatrix(i, 1) = colMaterials.Item(Panel.pNumber).YoungModulus * 0.000001
                    MSH1.TextMatrix(i, 2) = colMaterials.Item(Panel.pNumber).Poisson
                    MSH1.TextMatrix(i, 3) = Round(colMaterials.Item(Panel.pNumber).YieldStress * 0.000001, 1)
                    If Project.Item(ProjectIndex).cHeader.IANA = 1 Then
                        MSH1.TextMatrix(i, 4) = Round(colMaterials.Item(Panel.pNumber).AllowableStress * 0.000001, 1)
                    ElseIf Project.Item(ProjectIndex).cHeader.IANA = 2 Then
                        MSH1.TextMatrix(i, 4) = Round(colMaterials.Item(Panel.pNumber).MaterialCoeff, 2)
                    End If
                    MSH1.TextMatrix(i, 5) = colMaterials.Item(Panel.pNumber).SpecificWeight * 0.0001
                End If
            Next Panel
        Case "Participation"
            For Each Panel In colPanel
                If Panel.pType = Plate Then
                    i = i + 1
                    MSH1.TextMatrix(i, 0) = Panel.pNumber
                    MSH1.TextMatrix(i, 1) = colGeometry.Item(Panel.pNumber).Participation
                End If
            Next Panel
        Case "Straightening"
            For Each Panel In colPanel
                If Panel.pType = Plate Then
                    i = i + 1
                    MSH1.TextMatrix(i, 0) = Panel.pNumber
                    MSH1.TextMatrix(i, 1) = colGeometry.Item(Panel.pNumber).FAMI
                    MSH1.TextMatrix(i, 2) = colGeometry.Item(Panel.pNumber).LOT
                End If
            Next Panel
    End Select
    
'    'reset cell back color
'    For i = 1 To MSH1.Rows - 1
'        MSH1.Row = i
'        For j = 0 To MSH1.Cols - 1
'            MSH1.col = j
'            MSH1.CellBackColor = vbWhite
'        Next j
'    Next i
    'set color for the double clicked panel
    If TabStrip.SelectedItem.KEY <> "Girders" Then EntryWindow irowclicked
End Sub

Private Sub GetData()
    Set colPanel = Project.Item(ProjectIndex).colPanel
    Set colNodes = Project.Item(ProjectIndex).colNodes
    Set colGeometry = New Collection
    Set colScantlings = New Collection
    Set colMaterials = New Collection
    For Each Panel In colPanel
        colGeometry.Add Panel.cGeometry.Clone
        colScantlings.Add Panel.cScantlings.Clone
        colMaterials.Add Panel.cMaterial.Clone
    Next Panel
End Sub

Private Sub SetData()
    On Error GoTo SetDataErr
    Dim i As Integer
    i = 0
    Select Case TabStrip.SelectedItem.KEY
        Case "Geometry"
            For Each Panel In colPanel
                If Panel.pType = Plate Then
                    i = i + 1
                    colGeometry.Item(Panel.pNumber).InNode = MSH1.TextMatrix(i, 3)
                    colGeometry.Item(Panel.pNumber).OutNode = MSH1.TextMatrix(i, 4)
                    'If Project.Item(ProjectIndex).cHeader.IANA = 2 Then
                        colGeometry.Item(Panel.pNumber).PositionCode = GetPositionCodeIndex(MSH1.TextMatrix(i, 5))
                    'End If
                End If
            Next Panel
        Case "Thickness"
            For Each Panel In colPanel
                If Panel.pType = Plate Then
                    i = i + 1
                    colScantlings.Item(Panel.pNumber).NetThickness = MSH1.TextMatrix(i, 1) / 1000
                    colScantlings.Item(Panel.pNumber).CorrosionThickness = MSH1.TextMatrix(i, 2) / 1000
                    colScantlings.Item(Panel.pNumber).GrossThickness = colScantlings.Item(Panel.pNumber).NetThickness + colScantlings.Item(Panel.pNumber).CorrosionThickness
                End If
            Next Panel
        Case "StiffenersSet1"
            For Each Panel In colPanel
                If Panel.pType = Plate Then
                    i = i + 1
                    colScantlings.Item(Panel.pNumber).cPrimaryStiffeners.DistributionMode = MSH1.TextMatrix(i, 1)
                    colScantlings.Item(Panel.pNumber).cPrimaryStiffeners.WebHeight = MSH1.TextMatrix(i, 2) / 1000
                    colScantlings.Item(Panel.pNumber).cPrimaryStiffeners.WebThickness = MSH1.TextMatrix(i, 3) / 1000
                    colScantlings.Item(Panel.pNumber).cPrimaryStiffeners.FlangeWidth = MSH1.TextMatrix(i, 4) / 1000
                    colScantlings.Item(Panel.pNumber).cPrimaryStiffeners.FlangeThickness = MSH1.TextMatrix(i, 5) / 1000
                    colScantlings.Item(Panel.pNumber).cPrimaryStiffeners.Spacing = MSH1.TextMatrix(i, 6) / 1000
                    colScantlings.Item(Panel.pNumber).cPrimaryStiffeners.CorrosionThickness = MSH1.TextMatrix(i, 7) / 1000
                    Select Case MSH1.TextMatrix(i, 8)
                        Case "None"
                            colScantlings.Item(Panel.pNumber).cPrimaryStiffeners.Side = SideNone
                        Case "Left"
                            colScantlings.Item(Panel.pNumber).cPrimaryStiffeners.Side = SideLeft
                        Case "Right"
                            colScantlings.Item(Panel.pNumber).cPrimaryStiffeners.Side = SideRight
                    End Select
                End If
            Next Panel
        Case "FramesSet1"
            For Each Panel In colPanel
                If Panel.pType = Plate Then
                    i = i + 1
                    colScantlings.Item(Panel.pNumber).cPrimaryFrames.WebHeight = MSH1.TextMatrix(i, 1) / 1000
                    colScantlings.Item(Panel.pNumber).cPrimaryFrames.WebThickness = MSH1.TextMatrix(i, 2) / 1000
                    colScantlings.Item(Panel.pNumber).cPrimaryFrames.FlangeWidth = MSH1.TextMatrix(i, 3) / 1000
                    colScantlings.Item(Panel.pNumber).cPrimaryFrames.FlangeThickness = MSH1.TextMatrix(i, 4) / 1000
                    colScantlings.Item(Panel.pNumber).cPrimaryFrames.Spacing = MSH1.TextMatrix(i, 5) / 1000
                    colScantlings.Item(Panel.pNumber).cPrimaryFrames.CorrosionThickness = MSH1.TextMatrix(i, 6) / 1000
                    Select Case MSH1.TextMatrix(i, 7)
                        Case "None"
                            colScantlings.Item(Panel.pNumber).cPrimaryFrames.Side = SideNone
                        Case "Left"
                            colScantlings.Item(Panel.pNumber).cPrimaryFrames.Side = SideLeft
                        Case "Right"
                            colScantlings.Item(Panel.pNumber).cPrimaryFrames.Side = SideRight
                    End Select
                End If
            Next Panel
        Case "FramesSet2"
            For Each Panel In colPanel
                If Panel.pType = Plate Then
                    i = i + 1
                    colScantlings.Item(Panel.pNumber).cSecondaryFrames.WebHeight = MSH1.TextMatrix(i, 1) / 1000
                    colScantlings.Item(Panel.pNumber).cSecondaryFrames.WebThickness = MSH1.TextMatrix(i, 2) / 1000
                    colScantlings.Item(Panel.pNumber).cSecondaryFrames.FlangeWidth = MSH1.TextMatrix(i, 3) / 1000
                    colScantlings.Item(Panel.pNumber).cSecondaryFrames.FlangeThickness = MSH1.TextMatrix(i, 4) / 1000
                    colScantlings.Item(Panel.pNumber).cSecondaryFrames.Spacing = MSH1.TextMatrix(i, 5) / 1000
                    Select Case MSH1.TextMatrix(i, 6)
                        Case "None"
                            colScantlings.Item(Panel.pNumber).cSecondaryFrames.Side = SideNone
                        Case "Left"
                            colScantlings.Item(Panel.pNumber).cSecondaryFrames.Side = SideLeft
                        Case "Right"
                            colScantlings.Item(Panel.pNumber).cSecondaryFrames.Side = SideRight
                    End Select
                End If
            Next Panel
        Case "StiffenersSet2"
            For Each Panel In colPanel
                If Panel.pType = Plate Then
                    i = i + 1
                    colScantlings.Item(Panel.pNumber).cSecondaryStiffeners.WebHeight = MSH1.TextMatrix(i, 1) / 1000
                    colScantlings.Item(Panel.pNumber).cSecondaryStiffeners.WebThickness = MSH1.TextMatrix(i, 2) / 1000
                    colScantlings.Item(Panel.pNumber).cSecondaryStiffeners.FlangeWidth = MSH1.TextMatrix(i, 3) / 1000
                    colScantlings.Item(Panel.pNumber).cSecondaryStiffeners.FlangeThickness = MSH1.TextMatrix(i, 4) / 1000
                    colScantlings.Item(Panel.pNumber).cSecondaryStiffeners.Spacing = MSH1.TextMatrix(i, 5) / 1000
                    Select Case MSH1.TextMatrix(i, 6)
                        Case "None"
                            colScantlings.Item(Panel.pNumber).cSecondaryStiffeners.Side = SideNone
                        Case "Left"
                            colScantlings.Item(Panel.pNumber).cSecondaryStiffeners.Side = SideLeft
                        Case "Right"
                            colScantlings.Item(Panel.pNumber).cSecondaryStiffeners.Side = SideRight
                    End Select
                End If
            Next Panel
        Case "Girders"
            UpdateGirders
            'PopulatePanelList
        Case "Materials"
            For Each Panel In colPanel
                If Panel.pType = Plate Then
                    i = i + 1
                    colMaterials.Item(Panel.pNumber).YoungModulus = MSH1.TextMatrix(i, 1) / 0.000001
                    colMaterials.Item(Panel.pNumber).Poisson = MSH1.TextMatrix(i, 2)
                    colMaterials.Item(Panel.pNumber).YieldStress = MSH1.TextMatrix(i, 3) / 0.000001
                    If Project.Item(ProjectIndex).cHeader.IANA = 1 Then
                        colMaterials.Item(Panel.pNumber).AllowableStress = MSH1.TextMatrix(i, 4) / 0.000001
                    ElseIf Project.Item(ProjectIndex).cHeader.IANA = 2 Then
                        colMaterials.Item(Panel.pNumber).MaterialCoeff = MSH1.TextMatrix(i, 4)
                    End If
                    colMaterials.Item(Panel.pNumber).SpecificWeight = MSH1.TextMatrix(i, 5) / 0.0001
                End If
            Next Panel
        Case "Participation"
            For Each Panel In colPanel
                If Panel.pType = Plate Then
                    i = i + 1
                    colGeometry.Item(Panel.pNumber).Participation = MSH1.TextMatrix(i, 1)
                End If
            Next Panel
        Case "Straightening"
            For Each Panel In colPanel
                If Panel.pType = Plate Then
                    i = i + 1
                    colGeometry.Item(Panel.pNumber).FAMI = MSH1.TextMatrix(i, 1)
                    colGeometry.Item(Panel.pNumber).LOT = MSH1.TextMatrix(i, 2)
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
    MSH1.Rows = colPanel.GetNoOfPlates + 1
    MSH1.RowHeight(0) = 690
    picGirders.Visible = False
    Select Case TabStrip.SelectedItem.KEY
        Case "Geometry"
'            If Project.Item(ProjectIndex).cHeader.IANA = 1 Then
'                MSH1.Cols = 5
'                MSH1.FormatString = "|^|^|^|^"
'            ElseIf Project.Item(ProjectIndex).cHeader.IANA = 2 Then
                MSH1.Cols = 6
                MSH1.FormatString = "|^|^|^|^|^"
                MSH1.ColWidth(5) = 3800
                MSH1.TextMatrix(0, 5) = "Position Code"
'            End If
            
            MSH1.ColWidth(1) = 900
            MSH1.ColWidth(2) = 1000
            MSH1.ColWidth(3) = 900
            MSH1.ColWidth(4) = 900
            MSH1.TextMatrix(0, 0) = "Panel"
            MSH1.TextMatrix(0, 1) = "Width [m]"
            MSH1.TextMatrix(0, 2) = "Angle"
            MSH1.TextMatrix(0, 3) = "In Node"
            MSH1.TextMatrix(0, 4) = "Out Node"
            
        Case "Thickness"
            MSH1.Cols = 2
            MSH1.FormatString = "|^|^"
            MSH1.ColWidth(1) = 900
            MSH1.ColWidth(2) = 900
            MSH1.TextMatrix(0, 0) = "Panel"
            MSH1.TextMatrix(0, 1) = "Net" & vbCrLf & "Thickness" & vbCrLf & "[mm]"
            MSH1.TextMatrix(0, 2) = "Corrosion" & vbCrLf & "Thickness" & vbCrLf & "[mm]"
        Case "StiffenersSet1"
            MSH1.Cols = 8
            MSH1.FormatString = "|^|^|^|^|^|^|^|^"
            MSH1.ColWidth(1) = 850
            MSH1.ColWidth(2) = 850
            MSH1.ColWidth(3) = 850
            MSH1.ColWidth(4) = 850
            MSH1.ColWidth(5) = 850
            MSH1.ColWidth(6) = 850
            MSH1.ColWidth(7) = 850
            MSH1.ColWidth(8) = 600
            MSH1.TextMatrix(0, 0) = "Panel"
            MSH1.TextMatrix(0, 1) = "Distribution" & vbCrLf & "Mode"
            MSH1.TextMatrix(0, 2) = "Web" & vbCrLf & "Height" & vbCrLf & "[mm]"
            MSH1.TextMatrix(0, 3) = "Web" & vbCrLf & "Thickness" & vbCrLf & "[mm]"
            MSH1.TextMatrix(0, 4) = "Flange" & vbCrLf & "Width" & vbCrLf & "[mm]"
            MSH1.TextMatrix(0, 5) = "Flange" & vbCrLf & "Thickness" & vbCrLf & "[mm]"
            MSH1.TextMatrix(0, 6) = "Spacing" & vbCrLf & "[mm]"
            MSH1.TextMatrix(0, 7) = "Corrosion" & vbCrLf & "Thickness" & vbCrLf & "[mm]"
            MSH1.TextMatrix(0, 8) = "Side"
        Case "FramesSet1"
            MSH1.Cols = 7
            MSH1.FormatString = "|^|^|^|^|^|^|^"
            MSH1.ColWidth(1) = 900
            MSH1.ColWidth(2) = 900
            MSH1.ColWidth(3) = 900
            MSH1.ColWidth(4) = 900
            MSH1.ColWidth(5) = 900
            MSH1.ColWidth(6) = 900
            MSH1.ColWidth(7) = 600
            MSH1.TextMatrix(0, 0) = "Panel"
            MSH1.TextMatrix(0, 1) = "Web" & vbCrLf & "Height" & vbCrLf & "[mm]"
            MSH1.TextMatrix(0, 2) = "Web" & vbCrLf & "Thickness" & vbCrLf & "[mm]"
            MSH1.TextMatrix(0, 3) = "Flange" & vbCrLf & "Width" & vbCrLf & "[mm]"
            MSH1.TextMatrix(0, 4) = "Flange" & vbCrLf & "Thickness" & vbCrLf & "[mm]"
            MSH1.TextMatrix(0, 5) = "Spacing" & vbCrLf & "[mm]"
            MSH1.TextMatrix(0, 6) = "Corrosion" & vbCrLf & "Thickness" & vbCrLf & "[mm]"
            MSH1.TextMatrix(0, 7) = "Side"
        Case "FramesSet2", "StiffenersSet2"
            MSH1.Cols = 6
            MSH1.FormatString = "|^|^|^|^|^|^"
            MSH1.ColWidth(1) = 900
            MSH1.ColWidth(2) = 900
            MSH1.ColWidth(3) = 900
            MSH1.ColWidth(4) = 900
            MSH1.ColWidth(5) = 900
            MSH1.ColWidth(6) = 600
            MSH1.TextMatrix(0, 0) = "Panel"
            MSH1.TextMatrix(0, 1) = "Web" & vbCrLf & "Height" & vbCrLf & "[mm]"
            MSH1.TextMatrix(0, 2) = "Web" & vbCrLf & "Thickness" & vbCrLf & "[mm]"
            MSH1.TextMatrix(0, 3) = "Flange" & vbCrLf & "Width" & vbCrLf & "[mm]"
            MSH1.TextMatrix(0, 4) = "Flange" & vbCrLf & "Thickness" & vbCrLf & "[mm]"
            MSH1.TextMatrix(0, 5) = "Spacing" & vbCrLf & "[mm]"
            MSH1.TextMatrix(0, 6) = "Side"
        Case "Girders"
            MSH1.Cols = 5
            MSH1.Rows = 11
            MSH1.FormatString = "|^|^|^|^|^"
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
            MSH1.Cols = 5
            MSH1.FormatString = "|^|^|^|^|^"
            MSH1.ColWidth(1) = 900
            MSH1.ColWidth(2) = 900
            MSH1.ColWidth(3) = 900
            MSH1.ColWidth(4) = 900
            MSH1.ColWidth(5) = 900
            MSH1.TextMatrix(0, 0) = "Panel"
            MSH1.TextMatrix(0, 1) = "Young" & vbCrLf & "Modulus" & vbCrLf & "[N/mm2]"
            MSH1.TextMatrix(0, 2) = "Poisson" & vbCrLf & "Coefficient"
            MSH1.TextMatrix(0, 3) = "Yield" & vbCrLf & "Stress" & vbCrLf & "[N/mm2]"
            If Project.Item(ProjectIndex).cHeader.IANA = 1 Then
                MSH1.TextMatrix(0, 4) = "Allowable" & vbCrLf & "Stress" & vbCrLf & "[N/mm2]"
            Else
                MSH1.TextMatrix(0, 4) = "Material" & vbCrLf & "Coefficient" & vbCrLf & "[-]"
            End If
            MSH1.TextMatrix(0, 5) = "Specific" & vbCrLf & "Weight" & vbCrLf & "[t/m3]"
        Case "Participation"
            MSH1.Cols = 1
            MSH1.FormatString = "|^"
            MSH1.ColWidth(1) = 1000
            MSH1.TextMatrix(0, 0) = "Panel"
            MSH1.TextMatrix(0, 1) = "Participation" & vbCrLf & "Coefficient"
        Case "Straightening"
            MSH1.Cols = 3
            MSH1.FormatString = "|^|^"
            MSH1.ColWidth(1) = 900
            MSH1.ColWidth(2) = 900
            MSH1.TextMatrix(0, 0) = "Panel"
            MSH1.TextMatrix(0, 1) = "Family"
            MSH1.TextMatrix(0, 2) = "Lot"
    End Select
    MSH1.ColWidth(0) = 600
End Sub

Private Sub TabStripProperties()
    TabStrip.Tabs.Clear
    TabStrip.Tabs.Add 1, "Geometry", "Geometry"
    TabStrip.Tabs.Add 2, "Thickness", "Thickness"
    TabStrip.Tabs.Add 3, "StiffenersSet1", "Stiffeners Set 1"
    TabStrip.Tabs.Add 4, "FramesSet1", "Frames Set 1"
    TabStrip.Tabs.Add 5, "StiffenersSet2", "Stiffeners Set 2"
    TabStrip.Tabs.Add 6, "FramesSet2", "Frames Set 2"
    TabStrip.Tabs.Add 7, "Girders", "Girders"
    TabStrip.Tabs.Add 8, "Materials", "Materials"
    TabStrip.Tabs.Add 9, "Participation", "Participation"
    TabStrip.Tabs.Add 10, "Straightening", "Straightening"
    
End Sub

Private Sub PopulatePanelList()
    Dim index As Integer
    index = lstPanels.ListIndex
    lstPanels.Clear
    For Each Panel In colPanel
        If Panel.pType = Plate Or Panel.pType = DoubleHull Then
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
    Select Case TabStrip.SelectedItem.KEY
        Case "Geometry"
            Select Case MSH1.col
                Case 3, 4
                    For i = 1 To colNodes.Count
                        Combo.AddItem i
                    Next i
                Case 5
                    Combo.AddItem "No Code"
                    Combo.AddItem "1 Keel Plate"
                    Combo.AddItem "2 Bottom"
                    Combo.AddItem "3 Inner Bottom"
                    Combo.AddItem "4 Bilge"
                    Combo.AddItem "5 Side Shell - below freeboard deck"
                    Combo.AddItem "6 Side Shell - above freeboard deck"
                    Combo.AddItem "7 Inner Hull"
                    Combo.AddItem "8 Upper Strength Deck"
                    Combo.AddItem "9 Lower Deck"
                    Combo.AddItem "10 Double hull Girder, tank & watertight bulkhead"
            'End Select

                
                
                Case Else
            End Select
        Case "StiffenersSet1"
            Select Case MSH1.col
                Case 1
                    Combo.AddItem "EE1"
                    Combo.AddItem "EE2"
                Case 8
                    Combo.AddItem "None"
                    Combo.AddItem "Left"
                    Combo.AddItem "Right"
                Case Else
            End Select
        Case "FramesSet1"
            Select Case MSH1.col
                Case 7
                    Combo.AddItem "None"
                    Combo.AddItem "Left"
                    Combo.AddItem "Right"
                Case Else
            End Select
        Case "StiffenersSet2", "FramesSet2"
            Select Case MSH1.col
                Case 6
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
    cmdApply_Click
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
    irowclicked = 0
    fMainForm.SetFocus
End Sub

Private Sub lstPanels_Click()
    Dim v() As Variant
    Dim sData As String
    Select Case TabStrip.SelectedItem.KEY
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
    Select Case TabStrip.SelectedItem.KEY
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
            Select Case TabStrip.SelectedItem.KEY
                Case "Geometry"
                If MSH1.col <> 5 Then
                    Exit Sub
                End If
            End Select
            FlexPaste MSH1: SetData
            Exit Sub
        Case Else
    End Select
        
    Select Case TabStrip.SelectedItem.KEY
        Case "Geometry"
            Select Case MSH1.col
                Case 3, 4
                    If Licensing.IS_MODIFY_PANEL_NODES = False Then Exit Sub
                    PopulateCombo
                    MSHFlexGridEdit MSH1, Combo, KeyAscii
                Case 5
                    PopulateCombo
                    MSHFlexGridEdit MSH1, Combo, KeyAscii
                Case Else
            End Select
        Case "Thickness"
            MSHFlexGridEdit MSH1, TxtEdit, KeyAscii
        Case "StiffenersSet1"
            Select Case MSH1.col
                Case 1
                    PopulateCombo
                    MSHFlexGridEdit MSH1, Combo, KeyAscii
                Case 8
                    PopulateCombo
                    MSHFlexGridEdit MSH1, Combo, KeyAscii
                Case Else
                    MSHFlexGridEdit MSH1, TxtEdit, KeyAscii
            End Select
        Case "FramesSet1"
            Select Case MSH1.col
                Case 7
                    PopulateCombo
                    MSHFlexGridEdit MSH1, Combo, KeyAscii
                Case Else
                    MSHFlexGridEdit MSH1, TxtEdit, KeyAscii
            End Select
        Case "StiffenersSet2", "FramesSet2"
            Select Case MSH1.col
                Case 6
                    PopulateCombo
                    MSHFlexGridEdit MSH1, Combo, KeyAscii
                Case Else
                    MSHFlexGridEdit MSH1, TxtEdit, KeyAscii
            End Select
        Case "Materials", "Participation"
            MSHFlexGridEdit MSH1, TxtEdit, KeyAscii
        Case "Girders"
            MSHFlexGridEdit MSH1, TxtEditGirders, KeyAscii
        Case "Straightening"
            MSHFlexGridEdit MSH1, TxtEdit, KeyAscii
    End Select
End Sub

Private Sub MSH1_DblClick()
    Select Case TabStrip.SelectedItem.KEY
        Case "Geometry"
            Select Case MSH1.col
                Case 3, 4
                    If Licensing.IS_MODIFY_PANEL_NODES = False Then Exit Sub
                    PopulateCombo
                    MSHFlexGridEdit MSH1, Combo, 32
                Case 5
                    PopulateCombo
                    MSHFlexGridEdit MSH1, Combo, 32
                Case Else
            End Select
        Case "Thickness"
            MSHFlexGridEdit MSH1, TxtEdit, 32
        Case "StiffenersSet1"
            Select Case MSH1.col
                Case 1
                    PopulateCombo
                    MSHFlexGridEdit MSH1, Combo, 32
                Case 8
                    PopulateCombo
                    MSHFlexGridEdit MSH1, Combo, 32
                Case Else
                    MSHFlexGridEdit MSH1, TxtEdit, 32
            End Select
        Case "FramesSet1"
            Select Case MSH1.col
                Case 7
                    PopulateCombo
                    MSHFlexGridEdit MSH1, Combo, 32
                Case Else
                    MSHFlexGridEdit MSH1, TxtEdit, 32
            End Select
        Case "StiffenersSet2", "FramesSet2"
            Select Case MSH1.col
                Case 6
                    PopulateCombo
                    MSHFlexGridEdit MSH1, Combo, 32
                Case Else
                    MSHFlexGridEdit MSH1, TxtEdit, 32
            End Select
        Case "Materials", "Participation"
            MSHFlexGridEdit MSH1, TxtEdit, 32
        Case "Girders"
            MSHFlexGridEdit MSH1, TxtEditGirders, 32
        Case "Straightening"
            MSHFlexGridEdit MSH1, TxtEdit, 32
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
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmPlateScantlings: Sub MSHFlexGridEdit")
End Sub

Private Sub MSH1_Scroll()
    TxtEdit.Visible = False
    TxtEditGirders.Visible = False
    Combo.Visible = False
End Sub

Private Sub MSH1_Validate(Cancel As Boolean)
    Select Case TabStrip.SelectedItem.KEY
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
'    MSH1.Row = 1
'    MSH1.col = 1
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
    'dBuffer = MSH1
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
        ElseIf Combo.Visible = True Then
            Combo_Validate False
        End If
        MSHFlexgrid.SetFocus
    Case 38 'up
        If TxtEdit.Visible = True Then
            TxtEdit_Validate False
        ElseIf TxtEditGirders.Visible = True Then
            TxtEditGirders_Validate False
        ElseIf Combo.Visible = True Then
            Combo_Validate False
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
        ElseIf Combo.Visible = True Then
            Combo_Validate False
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

Private Function CheckPanelCollisions(ByVal PanelIndex As Integer) As Boolean
    Dim y1 As Double, z1 As Double, y2 As Double, z2 As Double
    Dim y3 As Double, z3 As Double, y4 As Double, z4 As Double
    Dim angle As Double

    Select Case icol
        Case 3
            y1 = colNodes.Item(Combo.Text).y
            z1 = colNodes.Item(Combo.Text).z
            y2 = colNodes.Item(MSH1.TextMatrix(irow, 4)).y
            z2 = colNodes.Item(MSH1.TextMatrix(irow, 4)).z
        Case 4
            y1 = colNodes.Item(MSH1.TextMatrix(irow, 3)).y
            z1 = colNodes.Item(MSH1.TextMatrix(irow, 3)).z
            y2 = colNodes.Item(Combo.Text).y
            z2 = colNodes.Item(Combo.Text).z
    End Select
    Dim i As Integer
    For i = 1 To colGeometry.Count
        If i <> PanelIndex Then
            y3 = colNodes.Item(colGeometry.Item(i).InNode).y
            z3 = colNodes.Item(colGeometry.Item(i).InNode).z
            y4 = colNodes.Item(colGeometry.Item(i).OutNode).y
            z4 = colNodes.Item(colGeometry.Item(i).OutNode).z
            angle = colGeometry.Item(i).PanelAngle
            If DetectIntersection(y1, z1, y2, z2, y3, z3, y4, z4, angle) = True Then
                MsgBox "Intersection with another panel. ( " & i & " & " & PanelIndex & ")", vbCritical + vbOKOnly
                MSH1.TextMatrix(irow, icol) = dBuffer
                CheckPanelCollisions = True
                Exit Function
            End If
        End If
    Next i
    
End Function

Private Sub TxtEditGirders_LostFocus()
    MSH1_GotFocus
    cmdOK.Default = True
End Sub

Private Sub TxtEdit_Validate(Cancel As Boolean)
    ValidateNumeric TxtEdit, Cancel
    'ValidateNonNullPozitive txtEdit, Cancel
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
