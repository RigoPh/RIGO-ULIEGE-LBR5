VERSION 5.00
Object = "{831FDD16-0C5C-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCTL.OCX"
Object = "{5E9E78A0-531B-11CF-91F6-C2863C385E30}#1.0#0"; "MSFLXGRD.OCX"
Object = "{0D452EE1-E08F-101A-852E-02608C4D0BB4}#2.0#0"; "FM20.DLL"
Begin VB.Form frmLoads 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Loads"
   ClientHeight    =   4785
   ClientLeft      =   6660
   ClientTop       =   3060
   ClientWidth     =   8925
   Icon            =   "frmLoads.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   4785
   ScaleWidth      =   8925
   ShowInTaskbar   =   0   'False
   Begin VB.TextBox TxtEditStepwise 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   285
      Left            =   600
      TabIndex        =   7
      Text            =   "Stepwise"
      Top             =   3000
      Visible         =   0   'False
      Width           =   975
   End
   Begin VB.ListBox lstPanels 
      Appearance      =   0  'Flat
      Height          =   1980
      ItemData        =   "frmLoads.frx":000C
      Left            =   120
      List            =   "frmLoads.frx":0013
      TabIndex        =   6
      Top             =   360
      Width           =   1820
   End
   Begin VB.ListBox lstLoadCases 
      Appearance      =   0  'Flat
      Height          =   1005
      ItemData        =   "frmLoads.frx":0022
      Left            =   120
      List            =   "frmLoads.frx":0029
      TabIndex        =   5
      Top             =   360
      Width           =   3135
   End
   Begin VB.ComboBox Combo 
      Appearance      =   0  'Flat
      Height          =   315
      ItemData        =   "frmLoads.frx":003B
      Left            =   1560
      List            =   "frmLoads.frx":003D
      Style           =   2  'Dropdown List
      TabIndex        =   1
      Top             =   2160
      Visible         =   0   'False
      Width           =   975
   End
   Begin VB.TextBox TxtEdit 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   285
      Left            =   600
      TabIndex        =   0
      Text            =   "Text1"
      Top             =   2640
      Visible         =   0   'False
      Width           =   975
   End
   Begin MSFlexGridLib.MSFlexGrid MSH1 
      Height          =   3615
      Left            =   240
      TabIndex        =   2
      Top             =   360
      Width           =   8535
      _ExtentX        =   15055
      _ExtentY        =   6376
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
      Height          =   4095
      Left            =   0
      TabIndex        =   8
      Top             =   0
      Width           =   8895
      _ExtentX        =   15690
      _ExtentY        =   7223
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
      Left            =   5400
      TabIndex        =   9
      Top             =   4200
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
   Begin MSForms.CommandButton cmdOK 
      Height          =   375
      Left            =   6600
      TabIndex        =   4
      Top             =   4200
      Width           =   1095
      Caption         =   "OK"
      PicturePosition =   327683
      Size            =   "1931;661"
      FontHeight      =   165
      FontCharSet     =   0
      FontPitchAndFamily=   2
      ParagraphAlign  =   3
   End
   Begin MSForms.CommandButton cmdCancel 
      Cancel          =   -1  'True
      Height          =   375
      Left            =   7800
      TabIndex        =   3
      Top             =   4200
      Width           =   1095
      Caption         =   "Cancel"
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
Attribute VB_Name = "frmLoads"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Dim ProjectIndex As Integer
Private dBuffer As String
Dim Panel As cPanel, colPanel As colPanel
Dim LoadCase As cLoadCase, colLoadCase As colLoadCase
Dim colLateralPressures As Collection, colSide As Collection
Dim colLocalizedPressure As Collection
Dim PanelListIndex As Integer
Dim LoadCaseIndex As Integer
Dim StepWiseLateralPressure As cStepWiseLateralPressure

Private Sub cmdApply_Click()
Select Case TabStrip.SelectedItem.KEY
        Case "LateralStepwisePressures"
            UpdateStepwise
    End Select
    For Each Panel In colPanel
        Set Panel.colLoadCase = colLateralPressures.Item(Panel.pNumber)
        Panel.LateralPressureSide = colSide.Item(Panel.pNumber)
        Panel.LocalizedPressure = colLocalizedPressure.Item(Panel.pNumber)
    Next Panel
    Set Project.Item(ProjectIndex).cHeader.colLoadCase = colLoadCase
    Project.Item(ProjectIndex).DataChanged = True
    Draw ProjectIndex
End Sub

Private Sub cmdCancel_Click()
    Unload Me
End Sub

Private Sub cmdOK_Click()
    Select Case TabStrip.SelectedItem.KEY
        Case "LateralStepwisePressures"
            UpdateStepwise
    End Select
    For Each Panel In colPanel
        Set Panel.colLoadCase = colLateralPressures.Item(Panel.pNumber)
        Panel.LateralPressureSide = colSide.Item(Panel.pNumber)
        Panel.LocalizedPressure = colLocalizedPressure.Item(Panel.pNumber)
    Next Panel
    Set Project.Item(ProjectIndex).cHeader.colLoadCase = colLoadCase
    Project.Item(ProjectIndex).DataChanged = True
    Unload Me
    Draw ProjectIndex
End Sub


Private Sub Form_Load()
    mnuEdit.Visible = False
    ProjectIndex = ActiveProject
    Me.Move (Screen.Width - Me.Width) / 2, (Screen.Height - Me.Height) / 2
    cmdOK.Picture = LoadResPicture("ID_BITMAP_OK", 0)
    cmdCancel.Picture = LoadResPicture("ID_BITMAP_CANCEL", 0)
    Me.Caption = "Loads - [" & GetFileName(Project.Item(ProjectIndex).sFileName) & "]"
    lstLoadCases.Height = MSH1.Height
    GetData
    PopulateListLoadCases
    PopulateListPanels
    lstPanels.ListIndex = 0
    TabStripProperties
    FlexGrid
    FillGrid
End Sub

Private Sub GetData()
    Set colPanel = Project.Item(ProjectIndex).colPanel
    Set colLoadCase = Project.Item(ProjectIndex).cHeader.colLoadCase.Clone
    Set colLateralPressures = New Collection
    Set colSide = New Collection
    Set colLocalizedPressure = New Collection
    For Each Panel In colPanel
        colLateralPressures.Add Panel.colLoadCase.Clone
        colSide.Add Panel.LateralPressureSide
        colLocalizedPressure.Add Panel.LocalizedPressure
    Next Panel
End Sub

Private Sub SetData()
    On Error GoTo SetDataErr
    Dim i As Integer
    Dim tmpCol As New Collection
    i = 0
    Select Case TabStrip.SelectedItem.KEY
        Case "LateralUniformPressures"
            For Each Panel In colPanel
                If Panel.pType = Plate Or Panel.pType = DoubleHull Then
                    i = i + 1
                    colLateralPressures.Item(Panel.pNumber).Item(LoadCaseIndex).LateralPressureIn = MSH1.TextMatrix(i, 1)
                    colLateralPressures.Item(Panel.pNumber).Item(LoadCaseIndex).LateralPressureOut = MSH1.TextMatrix(i, 2)
                    Select Case MSH1.TextMatrix(i, 3)
                        Case "Left"
                            tmpCol.Add SideLeft
                        Case "Right"
                            tmpCol.Add SideRight
                        Case Else
                            GoTo SetDataErr
                    End Select
                Else
                    tmpCol.Add colSide.Item(Panel.pNumber)
                End If
            Next Panel
            Set colSide = tmpCol
            Set tmpCol = Nothing
        Case "LocalizedPressures"
            Dim tmpColLocPress As New Collection
            For Each Panel In colPanel
                If Panel.pType = Plate Or Panel.pType = DoubleHull Then
                    i = i + 1
                    tmpColLocPress.Add MSH1.TextMatrix(i, 1)
                Else
                    tmpColLocPress.Add colLocalizedPressure.Item(Panel.pNumber)
                End If
            Next Panel
            Set colLocalizedPressure = tmpColLocPress
            Set tmpColLocPress = Nothing
        Case "GlobalLoads"
            For Each LoadCase In colLoadCase
                Select Case MSH1.TextMatrix(LoadCase.Index, 3)
                    Case "HOGGING"
                        LoadCase.VerticalBendingMomentFore = Abs(MSH1.TextMatrix(LoadCase.Index, 1)) * 1000
                        'LoadCase.VerticalBendingMomentAft = Abs(MSH1.TextMatrix(LoadCase.Index, 2)) * 1000
                        LoadCase.VerticalBendingMomentAft = Abs(MSH1.TextMatrix(LoadCase.Index, 1)) * 1000
                    Case "SAGGING"
                        LoadCase.VerticalBendingMomentFore = -Abs(MSH1.TextMatrix(LoadCase.Index, 1)) * 1000
                        LoadCase.VerticalBendingMomentAft = -Abs(MSH1.TextMatrix(LoadCase.Index, 2)) * 1000
                End Select
                LoadCase.HorizontalBendingMomentFore = Abs(MSH1.TextMatrix(LoadCase.Index, 4)) * 1000
                'LoadCase.HorizontalBendingMomentAft = Abs(MSH1.TextMatrix(LoadCase.Index, 5)) * 1000
                LoadCase.HorizontalBendingMomentAft = Abs(MSH1.TextMatrix(LoadCase.Index, 4)) * 1000
                LoadCase.VerticalShear = Abs(MSH1.TextMatrix(LoadCase.Index, 5)) * 1000
                LoadCase.HorizontalShear = Abs(MSH1.TextMatrix(LoadCase.Index, 6)) * 1000
            Next LoadCase
        Case "LateralStepwisePressures"
            UpdateStepwise
    End Select
    FillGrid
    Exit Sub
SetDataErr:
    MsgBox "Invalid format", vbCritical + vbOKOnly, "Error"
    FillGrid
End Sub

Private Sub TabStripProperties()
    Dim Index As Integer
    Index = 0
    TabStrip.Tabs.Clear
    Index = Index + 1
    If Project.Item(ProjectIndex).cHeader.IANA = 2 Then
        TabStrip.Tabs.Add Index, "LateralUniformPressures", "Lateral Pressures"
    End If
    If Project.Item(ProjectIndex).cHeader.IANA = 1 Then
        TabStrip.Tabs.Add Index, "LateralUniformPressures", "Lateral Uniform Distributed Pressures"
        Index = Index + 1
        TabStrip.Tabs.Add Index, "LocalizedPressures", "Local Pressures"
        Index = Index + 1
        TabStrip.Tabs.Add Index, "LateralStepwisePressures", "Lateral Stepwise Distributed Pressures"
    End If
    Index = Index + 1
    TabStrip.Tabs.Add Index, "GlobalLoads", "Global Loads"
End Sub

Private Sub FlexGrid()
    Dim i As Integer
    MSH1.Clear
    'MSH1.Rows = colPanel.GetNoOfPlates + 1
    MSH1.Rows = colPanel.GetNoOfPlates + colPanel.GetNoOfDoubleHulls * 2 + 1
    MSH1.RowHeight(0) = 690
    'lstLoadCases.Visible = False
    'lstPanels.Visible = False
    lstLoadCases.Left = 120
    lstPanels.Height = lstLoadCases.Height
    MSH1.Height = lstLoadCases.Height
    Select Case TabStrip.SelectedItem.KEY
        Case "LateralUniformPressures"
            lstLoadCases.Visible = True
            lstPanels.Visible = False
            If lstLoadCases.ListCount > 0 Then
                If lstLoadCases.ListIndex = -1 Then
                    lstLoadCases.ListIndex = 0
                End If
            End If
            MSH1.Left = lstLoadCases.Left + lstLoadCases.Width - 10
            MSH1.Width = 9135 - lstLoadCases.Left - lstLoadCases.Width - 360
            MSH1.Cols = 3
            MSH1.FormatString = "|^|^|^"
            MSH1.ColWidth(0) = 600
            MSH1.ColWidth(1) = 900
            MSH1.ColWidth(2) = 900
            MSH1.ColWidth(3) = 800
            MSH1.TextMatrix(0, 0) = "Panel"
            MSH1.TextMatrix(0, 1) = "In Node" & vbCrLf & "Pressure" & vbCrLf & "[m H2O]"
            MSH1.TextMatrix(0, 2) = "Out Node" & vbCrLf & "Pressure" & vbCrLf & "[m H2O]"
            MSH1.TextMatrix(0, 3) = "Side"
        Case "LocalizedPressures"
            lstLoadCases.Visible = False
            lstPanels.Visible = False
            MSH1.Left = 120
            MSH1.Width = 9135 - TabStrip.Left - 500
            'MSH1.top = 840
            'MSH1.height = 4335
            MSH1.Cols = 1
            MSH1.FormatString = "|^"
            MSH1.ColWidth(0) = 600
            MSH1.ColWidth(1) = 900
            MSH1.TextMatrix(0, 0) = "Panel"
            MSH1.TextMatrix(0, 1) = "Local Pressure" & vbCrLf & "[m H2O]"
        Case "GlobalLoads"
            lstLoadCases.Visible = False
            lstPanels.Visible = False
            MSH1.Left = 120
            MSH1.Width = 9135 - TabStrip.Left - 500
            'MSH1.top = 840
            'MSH1.height = 4335
            MSH1.Cols = 7
            MSH1.Rows = Project.Item(ProjectIndex).cHeader.colLoadCase.Count + 1
            MSH1.FormatString = "^|^|^|^|^|^|^"
            MSH1.ColWidth(0) = 3135
            MSH1.ColWidth(1) = 1100
            MSH1.ColWidth(2) = 0
            MSH1.ColWidth(3) = 900
            MSH1.ColWidth(4) = 1100
            MSH1.ColWidth(5) = 1100
            MSH1.ColWidth(6) = 1100
            If Project.Item(ProjectIndex).cHeader.IANA = 2 Then
                MSH1.ColWidth(4) = 0
                MSH1.ColWidth(6) = 0
            End If
            If Project.Item(ProjectIndex).cHeader.IANA = 1 Then
                MSH1.ColWidth(5) = 0
                MSH1.ColWidth(6) = 0
            End If
            
            MSH1.TextMatrix(0, 0) = "Load Case"
            MSH1.TextMatrix(0, 1) = "Vertical" & vbCrLf & "Moment" & vbCrLf & "[kNm]"
            MSH1.TextMatrix(0, 3) = "Vertical" & vbCrLf & "Moment" & vbCrLf & "Type"
            MSH1.TextMatrix(0, 4) = "Horizontal" & vbCrLf & "Moment" & vbCrLf & "[kNm]"
            MSH1.TextMatrix(0, 5) = "Vertical" & vbCrLf & "Shear" & vbCrLf & "[kN]"
            MSH1.TextMatrix(0, 6) = "Horizontal" & vbCrLf & "Shear" & vbCrLf & "[kN]"
        Case "LateralStepwisePressures"
            If lstLoadCases.Visible = False Then lstLoadCases.Visible = True
            If lstPanels.Visible = False Then lstPanels.Visible = True
            If lstPanels.ListCount > 0 Then
                If lstPanels.ListIndex = -1 Then
                    lstPanels.ListIndex = 0
                End If
            End If
            lstLoadCases.Left = lstPanels.Left + lstPanels.Width - 20 '+ 10
            MSH1.Left = lstLoadCases.Left + lstLoadCases.Width - 40  '+ 10
            
            MSH1.Cols = 4
            MSH1.Rows = 21
            For i = 1 To 20
                MSH1.TextMatrix(i, 0) = i
            Next i
            MSH1.Width = 3720
            MSH1.FormatString = "^|^|^|^"
            MSH1.ColWidth(0) = 500
            MSH1.ColWidth(1) = 900
            MSH1.ColWidth(2) = 900
            MSH1.ColWidth(3) = 900
            MSH1.TextMatrix(0, 0) = "Step"
            MSH1.TextMatrix(0, 1) = "Gravity" & vbCrLf & "Pressure" & vbCrLf & "[t/m]"
            MSH1.TextMatrix(0, 2) = "In Node" & vbCrLf & "Pressure" & vbCrLf & "[m H2O]"
            MSH1.TextMatrix(0, 3) = "Out Node" & vbCrLf & "Pressure" & vbCrLf & "[m H2O]"
    End Select
End Sub

Private Sub FillGrid()
    Dim i As Integer
    i = 0
    Select Case TabStrip.SelectedItem.KEY
        Case "LateralUniformPressures"
            For Each Panel In colPanel
                If Panel.pType = Plate Or Panel.pType = DoubleHull Then
                    i = i + 1
                    MSH1.TextMatrix(i, 0) = Panel.pNumber
                    MSH1.TextMatrix(i, 1) = colLateralPressures.Item(Panel.pNumber).Item(LoadCaseIndex).LateralPressureIn
                    MSH1.TextMatrix(i, 2) = colLateralPressures.Item(Panel.pNumber).Item(LoadCaseIndex).LateralPressureOut
                    Select Case colSide.Item(Panel.pNumber)
                        Case 1
                            MSH1.TextMatrix(i, 3) = "Left"
                        Case 2
                            MSH1.TextMatrix(i, 3) = "Right"
                    End Select
                End If
            Next Panel
        Case "LocalizedPressures"
            For Each Panel In colPanel
                If Panel.pType = Plate Or Panel.pType = DoubleHull Then
                    i = i + 1
                    MSH1.TextMatrix(i, 0) = Panel.pNumber
                    MSH1.TextMatrix(i, 1) = colLocalizedPressure.Item(Panel.pNumber)
                End If
            Next Panel
        Case "GlobalLoads"
            For Each LoadCase In colLoadCase
                i = i + 1
                MSH1.TextMatrix(i, 0) = LoadCase.Title
                MSH1.TextMatrix(i, 1) = Abs(LoadCase.VerticalBendingMomentFore) / 1000
                MSH1.TextMatrix(i, 2) = Abs(LoadCase.VerticalBendingMomentAft) / 1000
                Select Case LoadCase.VerticalBendingMomentFore
                    Case Is < 0
                        MSH1.TextMatrix(i, 3) = "SAGGING"
                    Case Is >= 0
                        MSH1.TextMatrix(i, 3) = "HOGGING"
                End Select
                MSH1.TextMatrix(i, 4) = Abs(LoadCase.HorizontalBendingMomentFore) / 1000
                MSH1.TextMatrix(i, 5) = (LoadCase.VerticalShear) / 1000
                MSH1.TextMatrix(i, 6) = (LoadCase.HorizontalShear) / 1000
            Next LoadCase
        Case "LateralStepwisePressures"
            For Each StepWiseLateralPressure In colLateralPressures.Item(PanelListIndex).Item(LoadCaseIndex).colStepWiseLateralPressure
                i = i + 1
                MSH1.TextMatrix(i, 1) = StepWiseLateralPressure.VerticalGravityLoad / 10000
                MSH1.TextMatrix(i, 2) = StepWiseLateralPressure.LateralPressureIn
                MSH1.TextMatrix(i, 3) = StepWiseLateralPressure.LateralPressureOut
            Next StepWiseLateralPressure
    End Select
End Sub

Private Sub PopulateListLoadCases()
    lstLoadCases.Clear
    For Each LoadCase In Project.Item(ProjectIndex).cHeader.colLoadCase
        lstLoadCases.AddItem LoadCase.Title
    Next LoadCase
End Sub

Private Sub PopulateListPanels()
    lstPanels.Clear
    Dim bIsStepwise As Boolean
    
    For Each Panel In Project.Item(ProjectIndex).colPanel
        bIsStepwise = False
        If Panel.pType = Plate Or Panel.pType = DoubleHull Then
            'If colLateralPressures.Item(PanelListIndex).Item(LoadCaseIndex).colStepWiseLateralPressure.Count > 0 Then
            For Each LoadCase In colLateralPressures.Item(Panel.pNumber)
                If LoadCase.colStepWiseLateralPressure.Count > 0 Then
                    bIsStepwise = True
                End If
            Next LoadCase
            If bIsStepwise = False Then
                lstPanels.AddItem "Panel " & Panel.pNumber
            Else
                lstPanels.AddItem "Panel " & Panel.pNumber & " *"
            End If
        End If
    Next Panel
    If PanelListIndex > 0 Then lstPanels.ListIndex = PanelListIndex - 1
End Sub

Private Sub Form_Resize()
    Me.Height = cmdOK.Top + cmdOK.Height + 600
End Sub

Private Sub Form_Unload(Cancel As Integer)
    Set colPanel = Nothing
    Set colLoadCase = Nothing
    Set colLateralPressures = Nothing
    Set colSide = Nothing
    fMainForm.SetFocus
End Sub

Private Sub lstLoadCases_Click()
    Select Case TabStrip.SelectedItem.KEY
        Case "LateralStepwisePressures"
            UpdateStepwise
    End Select
    LoadCaseIndex = lstLoadCases.ListIndex + 1
    FlexGrid
    FillGrid
End Sub

Private Sub PopulateCombo()
    Combo.Clear
    Select Case TabStrip.SelectedItem.KEY
        Case "LateralUniformPressures"
            Combo.AddItem "Left"
            Combo.AddItem "Right"
        Case "GlobalLoads"
            Combo.AddItem "HOGGING"
            Combo.AddItem "SAGGING"
    End Select
End Sub

Private Function UpdateStepwise()
    Dim i As Integer, j As Integer
    Dim Index As Integer
    Dim StepWiseLateralPressure As cStepWiseLateralPressure
    If PanelListIndex <= 0 Then Exit Function
    If LoadCaseIndex <= 0 Then Exit Function
    For Each StepWiseLateralPressure In colLateralPressures.Item(PanelListIndex).Item(LoadCaseIndex).colStepWiseLateralPressure
        colLateralPressures.Item(PanelListIndex).Item(LoadCaseIndex).colStepWiseLateralPressure.Remove StepWiseLateralPressure.Index
    Next StepWiseLateralPressure
    For i = 1 To MSH1.Rows - 1
        Index = 0
        For j = 1 To MSH1.Cols - 1
            If IsNumeric(Val_(MSH1.TextMatrix(i, j))) = True And MSH1.TextMatrix(i, j) <> "" Then
                Index = Index + 1
            End If
        Next j
        If Index = 3 Then
            Set StepWiseLateralPressure = New cStepWiseLateralPressure
            StepWiseLateralPressure.Index = colLateralPressures.Item(PanelListIndex).Item(LoadCaseIndex).colStepWiseLateralPressure.Count + 1
            StepWiseLateralPressure.VerticalGravityLoad = MSH1.TextMatrix(i, 1) * 10000
            StepWiseLateralPressure.LateralPressureIn = MSH1.TextMatrix(i, 2)
            StepWiseLateralPressure.LateralPressureOut = MSH1.TextMatrix(i, 3)
            colLateralPressures.Item(PanelListIndex).Item(LoadCaseIndex).colStepWiseLateralPressure.Add StepWiseLateralPressure, StepWiseLateralPressure.Index
            Set StepWiseLateralPressure = Nothing
        End If
    Next i
    'PopulateListPanels
End Function

' =============
' FLEXGRID EDIT
' =============
Function grd_index(r As Integer, c As Integer)
    grd_index = c + MSH1.Cols * r
End Function

Private Sub lstPanels_Click()
    Dim v() As Variant
    Dim sData As String
    Select Case TabStrip.SelectedItem.KEY
        Case "LateralStepwisePressures"
            UpdateStepwise
    End Select
    sData = lstPanels.Text
    GetValues 2, sData, v
    PanelListIndex = Val_(v(2))
    MSH1.Clear
    FlexGrid
    FillGrid
    If lstPanels.Visible = True Then lstPanels.SetFocus
End Sub


Private Sub MSH1_KeyDown(KeyCode As Integer, Shift As Integer)
    Dim i As Integer
    'If Len(MSH1) > 0 Then dBuffer = CDbl(MSH1)
    Select Case TabStrip.SelectedItem.KEY
        Case "LateralStepwisePressures"
            If KeyCode = 46 Then ' delete
                For i = 1 To MSH1.Cols - 1
                    MSH1.TextMatrix(MSH1.Row, i) = ""
                Next i
            End If
            TxtEditStepwise_KeyDown KeyCode, Shift
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
    
    Select Case TabStrip.SelectedItem.KEY
        Case "LateralUniformPressures"
            Select Case MSH1.col
                Case 1, 2
                    MSHFlexGridEdit MSH1, TxtEdit, KeyAscii
                Case 3
                    PopulateCombo
                    MSHFlexGridEdit MSH1, Combo, KeyAscii
                Case Else
            End Select
        Case "LocalizedPressures"
            MSHFlexGridEdit MSH1, TxtEdit, KeyAscii
        Case "LateralStepwisePressures"
            MSHFlexGridEdit MSH1, TxtEditStepwise, KeyAscii
        Case "GlobalLoads"
            Select Case MSH1.col
                Case 3
                    PopulateCombo
                    MSHFlexGridEdit MSH1, Combo, KeyAscii
                Case Else
                    MSHFlexGridEdit MSH1, TxtEdit, KeyAscii
            End Select
    End Select
End Sub

Private Sub MSH1_DblClick()
    Select Case TabStrip.SelectedItem.KEY
        Case "LateralUniformPressures"
            Select Case MSH1.col
                Case 1, 2
                    MSHFlexGridEdit MSH1, TxtEdit, 32
                Case 3
                    PopulateCombo
                    MSHFlexGridEdit MSH1, Combo, 32
                Case Else
            End Select
        Case "LocalizedPressures"
            MSHFlexGridEdit MSH1, TxtEdit, 32
        Case "LateralStepwisePressures"
            MSHFlexGridEdit MSH1, TxtEditStepwise, 32
        Case "GlobalLoads"
            Select Case MSH1.col
                Case 3
                    PopulateCombo
                    MSHFlexGridEdit MSH1, Combo, 32
                Case Else
                    MSHFlexGridEdit MSH1, TxtEdit, 32
            End Select
    End Select
End Sub

Sub MSHFlexGridEdit(MSHFlexgrid As Control, edt As Control, KeyAscii As Integer)
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
        Case "TxtEdit", "TxtEditStepwise"
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
End Sub

Private Sub MSH1_Scroll()
    TxtEdit.Visible = False
    TxtEditStepwise.Visible = False
    Combo.Visible = False
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
    FlexPaste MSH1
    SetData
End Sub

Private Sub MSH1_Validate(Cancel As Boolean)
    Select Case TabStrip.SelectedItem.KEY
        Case "LateralStepwisePressures"
            UpdateStepwise
    End Select
End Sub

Private Sub TabStrip_Change()
'    TxtEdit.Visible = False
'    TxtEditStepwise.Visible = False
'    Combo.Visible = False
'    MSH1.Row = 1
'    MSH1.col = 1
'    FlexGrid
'    FillGrid
End Sub

Private Sub TabStrip_BeforeClick(Cancel As Integer)
    If TxtEditStepwise.Visible = True Then
        Cancel = -1
        MSH1 = TxtEditStepwise
        UpdateStepwise
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
    TxtEditStepwise.Visible = False
    Combo.Visible = False
    MSH1.Row = 1
    MSH1.col = 1
    FlexGrid
    FillGrid
End Sub

Private Sub TxtEdit_KeyPress(KeyAscii As Integer)
    If KeyAscii = Asc(vbCr) Then KeyAscii = 0
End Sub

Private Sub TxtEditStepwise_KeyPress(KeyAscii As Integer)
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

Private Sub TxtEditStepwise_KeyDown(KeyCode As Integer, Shift As Integer)
    If Len(MSH1) > 0 Then
        dBuffer = (MSH1)
    Else
        dBuffer = ""
    End If
    cmdOK.Default = False
    EditKeyCode MSH1, TxtEditStepwise, KeyCode, Shift
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
        ElseIf TxtEditStepwise.Visible = True Then
            TxtEditStepwise_Validate False
        End If
        MSHFlexgrid.SetFocus
    Case 38 'up
        If TxtEdit.Visible = True Then
            TxtEdit_Validate False
        ElseIf TxtEditStepwise.Visible = True Then
            TxtEditStepwise_Validate False
        End If
        MSHFlexgrid.SetFocus
        'DoEvents
        If MSHFlexgrid.Row > MSHFlexgrid.FixedRows Then
           MSHFlexgrid.Row = MSHFlexgrid.Row - 1
        End If
    Case 40 'down
        If TxtEdit.Visible = True Then
            TxtEdit_Validate False
        ElseIf TxtEditStepwise.Visible = True Then
            TxtEditStepwise_Validate False
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
    If TxtEditStepwise.Visible = False Then GoTo 2
    MSH1 = TxtEditStepwise
    TxtEditStepwise.Visible = False
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
    If TxtEditStepwise.Visible = False Then GoTo 2
    MSH1 = TxtEditStepwise
    TxtEditStepwise.Visible = False
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

Private Sub TxtEditStepwise_LostFocus()
    MSH1_GotFocus
    cmdOK.Default = True
End Sub

Private Sub Combo_LostFocus()
    MSH1_GotFocus
    SetData
End Sub

Private Sub TxtEdit_Validate(Cancel As Boolean)
    ValidateNumeric TxtEdit, Cancel
    If Cancel = True Then TxtEdit = dBuffer
End Sub

Private Sub TxtEditStepwise_Validate(Cancel As Boolean)
    MSH1.col = MSH1.col
    Select Case MSH1.col
        Case 1
            ValidateNumericPozitiveOrNone TxtEditStepwise, Cancel
        Case 2, 3
            ValidateNumericOrNone TxtEditStepwise, Cancel
    End Select
    If Cancel = True Then TxtEditStepwise = dBuffer
End Sub
