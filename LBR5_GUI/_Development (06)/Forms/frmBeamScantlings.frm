VERSION 5.00
Object = "{831FDD16-0C5C-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCTL.OCX"
Object = "{5E9E78A0-531B-11CF-91F6-C2863C385E30}#1.0#0"; "MSFLXGRD.OCX"
Object = "{0D452EE1-E08F-101A-852E-02608C4D0BB4}#2.0#0"; "FM20.DLL"
Begin VB.Form frmBeamScantlings 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Beam Scantlings"
   ClientHeight    =   4665
   ClientLeft      =   10710
   ClientTop       =   3420
   ClientWidth     =   8985
   Icon            =   "frmBeamScantlings.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   4665
   ScaleWidth      =   8985
   ShowInTaskbar   =   0   'False
   Begin VB.ComboBox Combo 
      Appearance      =   0  'Flat
      Height          =   315
      ItemData        =   "frmBeamScantlings.frx":000C
      Left            =   1560
      List            =   "frmBeamScantlings.frx":000E
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
      Left            =   480
      TabIndex        =   0
      Text            =   "Text1"
      Top             =   2160
      Visible         =   0   'False
      Width           =   975
   End
   Begin MSFlexGridLib.MSFlexGrid MSH1 
      Height          =   3375
      Left            =   120
      TabIndex        =   2
      Top             =   480
      Width           =   8655
      _ExtentX        =   15266
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
      Height          =   3975
      Left            =   0
      TabIndex        =   5
      Top             =   0
      Width           =   8895
      _ExtentX        =   15690
      _ExtentY        =   7011
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
   Begin MSForms.CommandButton cmdOK 
      Default         =   -1  'True
      Height          =   375
      Left            =   6600
      TabIndex        =   4
      Top             =   4080
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
      Top             =   4080
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
Attribute VB_Name = "frmBeamScantlings"
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
Private irowclicked As Integer

Private Sub Form_Load()
    mnuEdit.Visible = False
    ProjectIndex = ActiveProject
    Me.Move (Screen.Width - Me.Width) / 2, (Screen.Height - Me.Height) / 2
    cmdOK.Picture = LoadResPicture("ID_BITMAP_OK", 0)
    cmdCancel.Picture = LoadResPicture("ID_BITMAP_CANCEL", 0)
    Me.Caption = "Beam Scantlings - [" & GetFileName(Project.Item(ProjectIndex).sFileName) & "]"
    GetData
    TabStripProperties
    FlexGrid
    FillGrid
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
            If Project.Item(ProjectIndex).colPanel.GetNoOfBeams > 11 Then MSH1.TopRow = i
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
            If MSH1.col = 2 And MSH1.ColSel Then
                Else
                Exit Sub
            End If
    End Select
    FlexPaste MSH1
    Combo_LostFocus
    SetData
End Sub

Private Sub FillGrid()
    Dim i As Integer
    i = 0
    Select Case TabStrip.SelectedItem.KEY
        Case "Geometry"
            For Each Panel In colPanel
                If Panel.pType = Beam Then
                    i = i + 1
                    MSH1.TextMatrix(i, 0) = Panel.pNumber
                    MSH1.TextMatrix(i, 1) = Round(colGeometry.Item(Panel.pNumber).PanelWidth, 6)
                    MSH1.Row = i: MSH1.col = 1: MSH1.CellForeColor = &H80000011
                    MSH1.TextMatrix(i, 2) = colGeometry.Item(Panel.pNumber).BucklingLength
                    MSH1.TextMatrix(i, 3) = Round(colGeometry.Item(Panel.pNumber).PanelAngle, 6)
                    MSH1.Row = i: MSH1.col = 3: MSH1.CellForeColor = &H80000011
                    MSH1.TextMatrix(i, 4) = colGeometry.Item(Panel.pNumber).InNode
                    MSH1.TextMatrix(i, 5) = colGeometry.Item(Panel.pNumber).OutNode
                End If
            Next Panel
        Case "Sections"
            For Each Panel In colPanel
                If Panel.pType = Beam Then
                    i = i + 1
                    MSH1.TextMatrix(i, 0) = Panel.pNumber
                    Select Case colScantlings.Item(Panel.pNumber).BeamSection
                        Case bsSquare
                            MSH1.TextMatrix(i, 1) = "Square"
                        Case bsCircle
                            MSH1.TextMatrix(i, 1) = "Circle"
                        Case bsDoubleT
                            MSH1.TextMatrix(i, 1) = "Double T"
                    End Select
                    Select Case colScantlings.Item(Panel.pNumber).BeamSection
                        Case bsSquare, bsCircle
                            Select Case MSH1.TextMatrix(i, 2)
                                Case "-------"
                                Case Else
                                    MSH1.TextMatrix(i, 2) = colScantlings.Item(Panel.pNumber).NetThickness * 1000
                            End Select
                            MSH1.TextMatrix(i, 3) = colScantlings.Item(Panel.pNumber).cPrimaryFrames.WebHeight * 1000
                            MSH1.TextMatrix(i, 4) = "-------"
                            MSH1.TextMatrix(i, 5) = "-------"
                            MSH1.TextMatrix(i, 6) = "-------"
                            MSH1.TextMatrix(i, 7) = "-------"
                        Case bsDoubleT
                            MSH1.TextMatrix(i, 2) = "-------"
                            MSH1.TextMatrix(i, 3) = "-------"
                            Select Case MSH1.TextMatrix(i, 4)
                                Case "-------"
                                Case Else
                                MSH1.TextMatrix(i, 4) = colScantlings.Item(Panel.pNumber).cPrimaryFrames.WebHeight * 1000
                                MSH1.TextMatrix(i, 5) = colScantlings.Item(Panel.pNumber).cPrimaryFrames.WebThickness * 1000
                                MSH1.TextMatrix(i, 6) = colScantlings.Item(Panel.pNumber).cPrimaryFrames.FlangeWidth * 1000
                                MSH1.TextMatrix(i, 7) = colScantlings.Item(Panel.pNumber).cPrimaryFrames.FlangeThickness * 1000
                            End Select
'                            MSH1.TextMatrix(i, 3) = colScantlings.Item(Panel.pNumber).cPrimaryFrames.WebHeight * 1000
'                            MSH1.TextMatrix(i, 4) = colScantlings.Item(Panel.pNumber).cPrimaryFrames.WebThickness * 1000
'                            MSH1.TextMatrix(i, 5) = colScantlings.Item(Panel.pNumber).cPrimaryFrames.FlangeWidth * 1000
'                            MSH1.TextMatrix(i, 6) = colScantlings.Item(Panel.pNumber).cPrimaryFrames.FlangeThickness * 1000
                    End Select
                    MSH1.TextMatrix(i, 8) = colScantlings.Item(Panel.pNumber).cPrimaryFrames.CorrosionThickness * 1000
                    MSH1.TextMatrix(i, 9) = colScantlings.Item(Panel.pNumber).cPrimaryFrames.Spacing * 1000
                End If
            Next Panel
        Case "Materials"
            For Each Panel In colPanel
                If Panel.pType = Beam Then
                    i = i + 1
                    MSH1.TextMatrix(i, 0) = Panel.pNumber
                    MSH1.TextMatrix(i, 1) = colMaterials.Item(Panel.pNumber).YoungModulus * 0.000001
                    MSH1.TextMatrix(i, 2) = Round(colMaterials.Item(Panel.pNumber).YieldStress * 0.000001, 1)
                    MSH1.TextMatrix(i, 3) = Round(colMaterials.Item(Panel.pNumber).AllowableStress * 0.000001, 1)
                    MSH1.TextMatrix(i, 4) = colMaterials.Item(Panel.pNumber).SpecificWeight * 0.0001
                End If
            Next Panel
    End Select
    EntryWindow irowclicked
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
                If Panel.pType = Beam Then
                    i = i + 1
                    colGeometry.Item(Panel.pNumber).BucklingLength = MSH1.TextMatrix(i, 2)
                    colGeometry.Item(Panel.pNumber).InNode = MSH1.TextMatrix(i, 4)
                    colGeometry.Item(Panel.pNumber).OutNode = MSH1.TextMatrix(i, 5)
                End If
            Next Panel
        Case "Sections"
            For Each Panel In colPanel
                If Panel.pType = Beam Then
                    i = i + 1
                    Select Case MSH1.TextMatrix(i, 1)
                        Case "Square"
                            colScantlings.Item(Panel.pNumber).BeamSection = bsSquare
                        Case "Circle"
                            colScantlings.Item(Panel.pNumber).BeamSection = bsCircle
                        Case "Double T"
                            colScantlings.Item(Panel.pNumber).BeamSection = bsDoubleT
                    End Select
                    Select Case MSH1.TextMatrix(i, 1)
                        Case "Square", "Circle"
                            Select Case MSH1.TextMatrix(i, 2)
                                Case "-------", ""
                                Case Else
                                    colScantlings.Item(Panel.pNumber).NetThickness = MSH1.TextMatrix(i, 2) / 1000
                                    colScantlings.Item(Panel.pNumber).cPrimaryFrames.WebHeight = MSH1.TextMatrix(i, 3) / 1000
                            End Select
                        Case "Double T"
                            Select Case MSH1.TextMatrix(i, 4)
                                Case "-------"
'                                    colScantlings.Item(Panel.pNumber).cPrimaryFrames.WebHeight = 0
'                                    colScantlings.Item(Panel.pNumber).cPrimaryFrames.WebThickness = 0
'                                    colScantlings.Item(Panel.pNumber).cPrimaryFrames.FlangeWidth = 0
'                                    colScantlings.Item(Panel.pNumber).cPrimaryFrames.FlangeThickness = 0
                                Case Else
                                    colScantlings.Item(Panel.pNumber).cPrimaryFrames.WebHeight = MSH1.TextMatrix(i, 4) / 1000
                                    colScantlings.Item(Panel.pNumber).cPrimaryFrames.WebThickness = MSH1.TextMatrix(i, 5) / 1000
                                    colScantlings.Item(Panel.pNumber).cPrimaryFrames.FlangeWidth = MSH1.TextMatrix(i, 6) / 1000
                                    colScantlings.Item(Panel.pNumber).cPrimaryFrames.FlangeThickness = MSH1.TextMatrix(i, 7) / 1000
                            End Select
                    End Select
                    colScantlings.Item(Panel.pNumber).cPrimaryFrames.CorrosionThickness = MSH1.TextMatrix(i, 8) / 1000
                    colScantlings.Item(Panel.pNumber).cPrimaryFrames.Spacing = MSH1.TextMatrix(i, 9) / 1000
                End If
            Next Panel
        Case "Materials"
            For Each Panel In colPanel
                If Panel.pType = Beam Then
                    i = i + 1
                    colMaterials.Item(Panel.pNumber).YoungModulus = MSH1.TextMatrix(i, 1) / 0.000001
                    colMaterials.Item(Panel.pNumber).YieldStress = MSH1.TextMatrix(i, 2) / 0.000001
                    colMaterials.Item(Panel.pNumber).AllowableStress = MSH1.TextMatrix(i, 3) / 0.000001
                    colMaterials.Item(Panel.pNumber).SpecificWeight = MSH1.TextMatrix(i, 4) / 0.0001
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
    MSH1.Rows = colPanel.GetNoOfBeams + 1
    MSH1.RowHeight(0) = 690
    Select Case TabStrip.SelectedItem.KEY
        Case "Geometry"
            MSH1.Cols = 6
            MSH1.FormatString = "|^|^|^|^|^"
            MSH1.ColWidth(1) = 900
            MSH1.ColWidth(2) = 900
            MSH1.ColWidth(3) = 1000
            MSH1.ColWidth(4) = 900
            MSH1.ColWidth(5) = 900
            MSH1.TextMatrix(0, 0) = "Panel"
            MSH1.TextMatrix(0, 1) = "Width [m]"
            MSH1.TextMatrix(0, 2) = "Effective" & vbCrLf & "Width [m]"
            MSH1.TextMatrix(0, 3) = "Angle"
            MSH1.TextMatrix(0, 4) = "In Node"
            MSH1.TextMatrix(0, 5) = "Out Node"
        Case "Sections"
            MSH1.Cols = 10
            MSH1.FormatString = "|^|^|^|^|^|^|^|^|^"
            MSH1.ColWidth(1) = 750
            MSH1.ColWidth(2) = 850
            MSH1.ColWidth(3) = 1150
            MSH1.ColWidth(4) = 800
            MSH1.ColWidth(5) = 850
            MSH1.ColWidth(6) = 800
            MSH1.ColWidth(7) = 850
            MSH1.ColWidth(8) = 850
            MSH1.ColWidth(9) = 800
            MSH1.TextMatrix(0, 0) = "Panel"
            MSH1.TextMatrix(0, 1) = "Section"
            MSH1.TextMatrix(0, 2) = "Net" & vbCrLf & "Thickness" & vbCrLf & "[mm]"
            MSH1.TextMatrix(0, 3) = "Circle Diameter" & vbCrLf & "Square Width" & vbCrLf & "[mm]"
            MSH1.TextMatrix(0, 4) = "Web" & vbCrLf & "Height" & vbCrLf & "[mm]"
            MSH1.TextMatrix(0, 5) = "Web" & vbCrLf & "Thickness" & vbCrLf & "[mm]"
            MSH1.TextMatrix(0, 6) = "Flange" & vbCrLf & "Width" & vbCrLf & "[mm]"
            MSH1.TextMatrix(0, 7) = "Flange" & vbCrLf & "Thickness" & vbCrLf & "[mm]"
            MSH1.TextMatrix(0, 8) = "Corrosion" & vbCrLf & "Thickness" & vbCrLf & "[mm]"
            MSH1.TextMatrix(0, 9) = "Spacing" & vbCrLf & "[mm]"
        Case "Materials"
            MSH1.Cols = 4
            MSH1.FormatString = "|^|^|^|^"
            MSH1.ColWidth(1) = 900
            MSH1.ColWidth(2) = 900
            MSH1.ColWidth(3) = 900
            MSH1.ColWidth(4) = 900
            MSH1.TextMatrix(0, 0) = "Panel"
            MSH1.TextMatrix(0, 1) = "Young" & vbCrLf & "Modulus" & vbCrLf & "[N/mm2]"
            MSH1.TextMatrix(0, 2) = "Yield" & vbCrLf & "Stress" & vbCrLf & "[N/mm2]"
            MSH1.TextMatrix(0, 3) = "Allowable" & vbCrLf & "Stress" & vbCrLf & "[N/mm2]"
            MSH1.TextMatrix(0, 4) = "Specific" & vbCrLf & "Weight" & vbCrLf & "[t/m3]"
    End Select
    MSH1.ColWidth(0) = 600
End Sub

Private Sub TabStripProperties()
    TabStrip.Tabs.Clear
    TabStrip.Tabs.Add 1, "Geometry", "Geometry"
    TabStrip.Tabs.Add 2, "Sections", "Sections"
    TabStrip.Tabs.Add 3, "Materials", "Materials"
End Sub

Private Sub PopulateCombo()
    Dim i As Integer
    Combo.Clear
    Select Case TabStrip.SelectedItem.KEY
        Case "Geometry"
            Select Case MSH1.col
                Case 4, 5
                    For i = 1 To colNodes.Count
                        Combo.AddItem i
                    Next i
                Case Else
            End Select
        Case "Sections"
            Select Case MSH1.col
                Case 1
                    Combo.AddItem "Circle"
                    Combo.AddItem "Square"
                    Combo.AddItem "Double T"
            End Select
        Case Else
    End Select
End Sub

Private Sub cmdCancel_Click()
    Unload Me
End Sub

Private Sub cmdOK_Click()
    Dim y1 As Double, z1 As Double, y2 As Double, z2 As Double
    For Each Panel In colPanel
        Set Panel.cGeometry = colGeometry.Item(Panel.pNumber)
        y1 = colNodes.Item(Panel.cGeometry.InNode).y
        z1 = colNodes.Item(Panel.cGeometry.InNode).z
        y2 = colNodes.Item(Panel.cGeometry.OutNode).y
        z2 = colNodes.Item(Panel.cGeometry.OutNode).z
        GetLengthAngle y1, z1, y2, z2, Panel
        Set Panel.cScantlings = colScantlings(Panel.pNumber)
        Set Panel.cMaterial = colMaterials(Panel.pNumber)
    Next Panel
    UpdateBoundary ProjectIndex
    Draw ProjectIndex
    Project.Item(ProjectIndex).DataChanged = True
    Unload Me
End Sub

' =============
' FLEXGRID EDIT
' =============
Function grd_index(r As Integer, c As Integer)
    grd_index = c + MSH1.Cols * r
End Function

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
    Set colGeometry = Nothing
    Set colScantlings = Nothing
    Set colMaterials = Nothing
End Sub

Private Sub Form_Unload(Cancel As Integer)
    irowclicked = 0
    fMainForm.SetFocus
End Sub

Private Sub MSH1_KeyDown(KeyCode As Integer, Shift As Integer)
    'If Len(MSH1) > 0 Then dBuffer = (MSH1)
    TxtEdit_KeyDown KeyCode, Shift
End Sub

Private Sub MSH1_KeyPress(KeyAscii As Integer)
    Select Case KeyAscii
        Case 3 'CTRL + C
            FlexCopy MSH1
            Exit Sub
        Case 22 'CTRL + V
            Select Case TabStrip.SelectedItem.KEY
                Case "Geometry"
                Exit Sub
            End Select
            FlexPaste MSH1: SetData
            Exit Sub
        Case Else
    End Select

    Select Case TabStrip.SelectedItem.KEY
        Case "Geometry"
            Select Case MSH1.col
                Case 2
                    MSHFlexGridEdit MSH1, TxtEdit, KeyAscii
                Case 4, 5
                    If Licensing.IS_MODIFY_PANEL_NODES = False Then Exit Sub
                    PopulateCombo
                    MSHFlexGridEdit MSH1, Combo, KeyAscii
                Case Else
            End Select
        Case "Sections"
            Select Case MSH1.col
                Case 1
                    PopulateCombo
                    MSHFlexGridEdit MSH1, Combo, KeyAscii
                Case Else
                    Select Case MSH1.Text
                        Case "-------"
                        Case Else
                            MSHFlexGridEdit MSH1, TxtEdit, KeyAscii
                    End Select
            End Select
        Case "Materials"
            MSHFlexGridEdit MSH1, TxtEdit, KeyAscii
    End Select

End Sub

Private Sub MSH1_DblClick()
    Select Case TabStrip.SelectedItem.KEY
        Case "Geometry"
            Select Case MSH1.col
                Case 2
                    MSHFlexGridEdit MSH1, TxtEdit, 32
                Case 4, 5
                    If Licensing.IS_MODIFY_PANEL_NODES = False Then Exit Sub
                    PopulateCombo
                    MSHFlexGridEdit MSH1, Combo, 32
                Case Else
            End Select
        Case "Sections"
            Select Case MSH1.col
                Case 1
                    PopulateCombo
                    MSHFlexGridEdit MSH1, Combo, 32
                Case Else
                    Select Case MSH1.Text
                        Case "-------"
                        Case Else
                            MSHFlexGridEdit MSH1, TxtEdit, 32
                    End Select
            End Select
        Case "Materials"
            MSHFlexGridEdit MSH1, TxtEdit, 32
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
        Case "TxtEdit"
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
    Combo.Visible = False
End Sub

Private Sub TabStrip_Change()
    TxtEdit.Visible = False
    Combo.Visible = False
    MSH1.Row = 1
    MSH1.col = 1
    FlexGrid
    FillGrid
End Sub

Private Sub TabStrip_BeforeClick(Cancel As Integer)
    If TxtEdit.Visible = True Then
       Cancel = -1
    End If
    If Combo.Visible = True Then
        Cancel = -1
    End If
End Sub

Private Sub TabStrip_Click()
    TxtEdit.Visible = False
    Combo.Visible = False
    MSH1.Row = 1
    MSH1.col = 1
    FlexGrid
    FillGrid
End Sub

Private Sub TxtEdit_KeyPress(KeyAscii As Integer)
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

Private Sub Combo_KeyDown(KeyCode As Integer, Shift As Integer)
    EditKeyCode MSH1, Combo, KeyCode, Shift
End Sub

Sub EditKeyCode(MSHFlexgrid As Control, edt As Control, KeyCode As Integer, Shift As Integer)
    Select Case KeyCode
    Case 27 ' esc
        edt.Visible = False
        MSHFlexgrid.SetFocus
    Case 13 'enter
        TxtEdit_Validate False
        MSHFlexgrid.SetFocus
    Case 38 'up
        TxtEdit_Validate False
        MSHFlexgrid.SetFocus
        'DoEvents
        If MSHFlexgrid.Row > MSHFlexgrid.FixedRows Then
           MSHFlexgrid.Row = MSHFlexgrid.Row - 1
        End If
    Case 40 'down
        TxtEdit_Validate False
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
    If Combo.Visible = False Then Exit Sub
    MSH1 = Combo.List(Combo.ListIndex)
    Combo.Visible = False
End Sub

Private Sub MSH1_LeaveCell()
    If TxtEdit.Visible = False Then GoTo 1 'Exit Sub
    MSH1 = TxtEdit
    TxtEdit.Visible = False
1:
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
    Select Case TabStrip.SelectedItem.KEY
        Case "Sections"
            FlexGrid
            FillGrid
    End Select

End Sub

Private Sub TxtEdit_Validate(Cancel As Boolean)
    ValidateNumeric TxtEdit, Cancel
    If Cancel = True Then TxtEdit = dBuffer
End Sub


