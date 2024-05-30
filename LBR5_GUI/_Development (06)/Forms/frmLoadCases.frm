VERSION 5.00
Object = "{5E9E78A0-531B-11CF-91F6-C2863C385E30}#1.0#0"; "MSFLXGRD.OCX"
Object = "{0D452EE1-E08F-101A-852E-02608C4D0BB4}#2.0#0"; "FM20.DLL"
Begin VB.Form frmLoadCases 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Load Cases"
   ClientHeight    =   3990
   ClientLeft      =   6255
   ClientTop       =   3225
   ClientWidth     =   7110
   Icon            =   "frmLoadCases.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   3990
   ScaleWidth      =   7110
   ShowInTaskbar   =   0   'False
   Begin VB.TextBox TxtEdit 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   285
      Left            =   360
      TabIndex        =   0
      Text            =   "Text1"
      Top             =   720
      Visible         =   0   'False
      Width           =   975
   End
   Begin MSFlexGridLib.MSFlexGrid MSH1 
      Height          =   3135
      Left            =   0
      TabIndex        =   1
      Top             =   120
      Width           =   7095
      _ExtentX        =   12515
      _ExtentY        =   5530
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
   Begin MSForms.CommandButton cmdBuildMarsLoadCase 
      Height          =   375
      Left            =   2400
      TabIndex        =   6
      Top             =   3480
      Width           =   1815
      Caption         =   "Build Mars Load Case"
      PicturePosition =   327683
      Size            =   "3201;661"
      FontHeight      =   165
      FontCharSet     =   0
      FontPitchAndFamily=   2
      ParagraphAlign  =   3
   End
   Begin MSForms.CommandButton cmdRemove 
      Height          =   375
      Left            =   1200
      TabIndex        =   3
      Top             =   3480
      Width           =   975
      Caption         =   "Remove"
      PicturePosition =   327683
      Size            =   "1720;661"
      FontHeight      =   165
      FontCharSet     =   0
      FontPitchAndFamily=   2
      ParagraphAlign  =   3
   End
   Begin MSForms.CommandButton cmdAdd 
      Height          =   375
      Left            =   120
      TabIndex        =   2
      Top             =   3480
      Width           =   975
      Caption         =   "Add"
      PicturePosition =   327683
      Size            =   "1720;661"
      FontHeight      =   165
      FontCharSet     =   0
      FontPitchAndFamily=   2
      ParagraphAlign  =   3
   End
   Begin MSForms.CommandButton cmdOK 
      Default         =   -1  'True
      Height          =   375
      Left            =   4800
      TabIndex        =   4
      Top             =   3480
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
      Left            =   6000
      TabIndex        =   5
      Top             =   3480
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
Attribute VB_Name = "frmLoadCases"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Dim ProjectIndex As Integer
Dim dBuffer As String
Private Panel As cPanel
Private LoadCase As cLoadCase
Private colP As colPanel
Private colLC As colLoadCase
Private col As New Collection
Private p As Object
Private mvarfrmMarsLoads As frmMarsLoads
Private colPanelSide As New Collection

Private Property Get frmMarsLoads() As frmMarsLoads
    On Error GoTo frmMarsLoadsGetErr
    If mvarfrmMarsLoads Is Nothing Then
        Set mvarfrmMarsLoads = New frmMarsLoads
    End If
    Set frmMarsLoads = mvarfrmMarsLoads
    Exit Property
frmMarsLoadsGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmMarsLoads Property Get")
End Property

Private Property Set frmMarsLoads(vData As frmMarsLoads)
    On Error GoTo frmMarsLoadsSetErr
    Set mvarfrmMarsLoads = vData
    Exit Property
frmMarsLoadsSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: frmMarsLoads Property Set")
End Property

Private Sub cmdBuildMarsLoadCase_Click()
    Dim index As Integer
    If Project.Item(ProjectIndex).cHeader.cMarsLoads.colMarsLoadCase.Count = 0 Then
        MsgBox "Mars Loads not available.", vbInformation + vbOKOnly
        Exit Sub
    End If
    If colLC.Count = 0 Then Exit Sub
    index = MSH1.TextMatrix(MSH1.Row, 0)
    If mvarfrmMarsLoads Is Nothing Then
        Set mvarfrmMarsLoads = New frmMarsLoads
    End If

    mvarfrmMarsLoads.OpenForm colLC, col, colPanelSide, index, ProjectIndex
    mvarfrmMarsLoads.Show vbModeless, fMainForm
    Unload Me
    'Project.Item(ProjectIndex).CloseForms 0
    frmMarsLoads.SetFocus
    
End Sub

Private Sub cmdBuildMarsLoadCase_DblClick(Cancel As MSForms.ReturnBoolean)
    Cancel = True
End Sub

Private Sub Form_Initialize()
    Set frmMarsLoads = New frmMarsLoads
End Sub

Private Sub Form_Load()
    mnuEdit.Visible = False
    If ProjectIndex = 0 Then ProjectIndex = ActiveProject
    Me.Move (Screen.Width - Me.Width) / 2, (Screen.Height - Me.Height) / 2
    cmdOK.Picture = LoadResPicture("ID_BITMAP_OK", 0)
    cmdCancel.Picture = LoadResPicture("ID_BITMAP_CANCEL", 0)
    cmdAdd.Picture = LoadResPicture("ID_BITMAP_PLUS", 0)
    cmdRemove.Picture = LoadResPicture("ID_BITMAP_MINUS", 0)
    Me.Caption = "Load Cases - [" & GetFileName(Project.Item(ProjectIndex).sFileName) & "]"
    GetData
End Sub

Private Sub Form_Resize()
    Me.Height = cmdOK.Top + cmdOK.Height + 600
End Sub

Private Sub Form_Terminate()
    Set frmMarsLoads = Nothing
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

Private Sub Form_Unload(Cancel As Integer)
    'On Error Resume Next
    Set col = Nothing
    Set colLC = Nothing
    fMainForm.SetFocus
End Sub

Private Sub GetData()
    Dim i As Integer
    Set colLC = New colLoadCase
    Set colP = New colPanel
    Set colLC = Project.Item(ProjectIndex).cHeader.colLoadCase.Clone
    For Each Panel In Project.Item(ProjectIndex).colPanel
        col.Add Panel.colLoadCase.Clone
        colPanelSide.Add Panel.LateralPressureSide
    Next Panel
    FlexGrid
End Sub

Private Sub FlexGrid()
    Dim i As Integer
    Dim OBJ As cLoadCase
    MSH1.Clear
    MSH1.Cols = 3
    MSH1.Rows = colLC.Count + 1
    MSH1.RowHeight(0) = 690
    MSH1.FormatString = "|^|^"
    MSH1.ColWidth(0) = 600
    MSH1.ColWidth(1) = 5000
    MSH1.ColWidth(2) = 600
    MSH1.TextMatrix(0, 0) = "Case"
    MSH1.TextMatrix(0, 1) = "Name"
    MSH1.TextMatrix(0, 2) = "State"
    i = 0
    For Each OBJ In colLC
        i = i + 1
        MSH1.TextMatrix(i, 0) = OBJ.index
        MSH1.TextMatrix(i, 1) = OBJ.Title
        Select Case OBJ.state
            Case IsOff
                MSH1.TextMatrix(i, 2) = "OFF"
            Case IsOn
                MSH1.TextMatrix(i, 2) = "ON"
        End Select
    Next OBJ
End Sub

Public Sub OpenForm(ByRef colLC1 As colLoadCase, ByRef colPanels As Collection, ByRef Col1 As Collection)
    Set mvarfrmMarsLoads = Nothing
    MSH1.Clear
    Set colLC = colLC1.Clone
   
    Set col = Nothing
    Set colPanelSide = Nothing
    For Each Panel In Project.Item(ProjectIndex).colPanel
        col.Add Col1.Item(Panel.index)
        colPanelSide.Add colPanels.Item(Panel.index)
    Next Panel
    
    FlexGrid
End Sub

Private Sub cmdAdd_Click()
    Dim index As Integer
    Dim OBJ As cLoadCase
    index = GetFreeIndex
    If colLC.Count = 20 Then
        MsgBox "Maximum twenty load cases can be defined", vbExclamation + vbOKOnly, "Warning"
        Exit Sub
    End If
    Set OBJ = New cLoadCase
    OBJ.index = index
    OBJ.Title = "Load Case " & index
    If colLC.Count = 0 Then
        OBJ.state = IsOn
    Else
        OBJ.state = IsOff
    End If
    MSH1.Rows = MSH1.Rows + 1
    MSH1.Row = MSH1.Rows - 1
    MSH1.col = 1
    MSH1.SetFocus
    MSH1.TextMatrix(MSH1.Rows - 1, 0) = OBJ.index
    MSH1.TextMatrix(MSH1.Rows - 1, 1) = OBJ.Title
    If colLC.Count = 0 Then
        MSH1.TextMatrix(MSH1.Rows - 1, 2) = "ON"
    Else
        MSH1.TextMatrix(MSH1.Rows - 1, 2) = "OFF"
    End If
    colLC.Add OBJ, index
    For Each p In col
        Set OBJ = New cLoadCase
        OBJ.index = index
        OBJ.Title = "Load Case " & index
        OBJ.state = IsOff
        p.Add OBJ, index
    Next p
End Sub

Private Sub cmdRemove_Click()
    Dim index As Integer
    If colLC.Count = 0 Then Exit Sub
    index = MSH1.TextMatrix(MSH1.Row, 0)
'    If collc.Count = 1 Then
'        MsgBox "Minimum one load case required", vbExclamation + vbOKOnly, "Warning"
'        Exit Sub
'    End If
    colLC.Remove index
    For Each p In col
        p.Remove index
    Next p
    'test if there is at least one case ON left
    Dim bOn As Boolean, lc As cLoadCase
    bOn = False
    For Each lc In colLC
        If lc.state = IsOn Then
            bOn = True
        End If
    Next lc
    ' since there isn't, set the first case ON
    If bOn = False Then
        For Each lc In colLC
            lc.state = IsOn
            Exit For
        Next lc
        For Each p In col
            For Each LoadCase In p
                LoadCase.state = IsOn
                Exit For
            Next LoadCase
        Next p
    End If
    FlexGrid
End Sub

Private Sub SetData()
    On Error GoTo SetDataErr
    Dim i As Integer
    Dim OBJ As cLoadCase
    i = 0
    For Each OBJ In colLC
        i = i + 1
        OBJ.index = MSH1.TextMatrix(i, 0)
        OBJ.Title = MSH1.TextMatrix(i, 1)
        If MSH1.TextMatrix(i, 2) = "ON" Then
            OBJ.state = IsOn
        ElseIf MSH1.TextMatrix(i, 2) = "OFF" Then
            OBJ.state = IsOff
        End If
    Next OBJ
    For Each p In col
        i = 0
        For Each OBJ In p
            i = i + 1
            OBJ.index = MSH1.TextMatrix(i, 0)
            OBJ.Title = MSH1.TextMatrix(i, 1)
            If MSH1.TextMatrix(i, 2) = "ON" Then
                OBJ.state = IsOn
            ElseIf MSH1.TextMatrix(i, 2) = "OFF" Then
                OBJ.state = IsOff
            End If
        Next OBJ
    Next p
    FlexGrid
    Exit Sub
SetDataErr:
    MsgBox "Invalid format", vbCritical + vbOKOnly, "Error"
    FlexGrid
End Sub

Private Sub getOnMin(b As Boolean)
    Dim i As Integer
    Dim index As Integer
    index = 0
    For i = 1 To MSH1.Rows - 1
        If MSH1.TextMatrix(i, 2) = "ON" Then
            index = index + 1
        End If
    Next i
    If index <= 1 Then
        b = True
    Else
        b = False
    End If
End Sub

Private Sub getOnMax(b As Boolean)
    Dim i As Integer
    Dim index As Integer
    index = 0
    For i = 1 To MSH1.Rows - 1
        If MSH1.TextMatrix(i, 2) = "ON" Then
            index = index + 1
        End If
    Next i
    If index >= 10 Then
        b = True
    Else
        b = False
    End If
End Sub

Private Function GetFreeIndex() As Integer
    Dim OBJ As cLoadCase
    GetFreeIndex = 0
    For Each OBJ In colLC
        If OBJ.index > GetFreeIndex Then
            GetFreeIndex = OBJ.index
        End If
    Next OBJ
    GetFreeIndex = GetFreeIndex + 1
End Function

Private Sub cmdAdd_DblClick(Cancel As MSForms.ReturnBoolean)
    Cancel = True
End Sub

Private Sub cmdCancel_Click()
    Set col = Nothing
    Set colLC = Nothing

    Unload Me
End Sub

Private Sub cmdCancel_DblClick(Cancel As MSForms.ReturnBoolean)
    Cancel = True
End Sub

Private Sub cmdOK_Click()
    'Validate load case titles
    Dim Cancel As Boolean, i As Integer, j As Integer
    For i = 1 To MSH1.Rows - 1
        txtEdit.Text = MSH1.TextMatrix(i, 1)
        ValidateText txtEdit, Cancel
        If Cancel = True Then Exit Sub
    Next i

    Dim ilc As Integer
    For i = 1 To colLC.Count
        If colLC.Item(i).state = IsOn Then
            ilc = ilc + 1
        End If
    Next i
    If ilc > Licensing.MAX_ACTIVE_LOAD_CASES Then
        MsgBox "The maximum number of active load cases is restricted to " & Licensing.MAX_ACTIVE_LOAD_CASES & ".", vbCritical + vbOKOnly, "LBR-5 License Limitation"
        Exit Sub
    End If
    
    'renum loadcase indexes in cHeader
    Dim colRenum As colLoadCase
    Set colRenum = New colLoadCase
    i = 0
    For Each LoadCase In colLC
        i = i + 1
        LoadCase.index = i
        colRenum.Add LoadCase, LoadCase.index
    Next LoadCase
    Set colLC = colRenum
    Set colRenum = Nothing
    Set Project.Item(ProjectIndex).cHeader.colLoadCase = colLC
    'renum loadcase indexes in Panels
    j = 0
    For Each p In col
        Set colRenum = New colLoadCase
        i = 0
        j = j + 1
        For Each LoadCase In p
            i = i + 1
            LoadCase.index = i
            colRenum.Add LoadCase, LoadCase.index
        Next LoadCase
        Set Project.Item(ProjectIndex).colPanel.Item(j).colLoadCase = colRenum
        Set colRenum = Nothing
    Next p
    'the "i" indexation works here because the colPanel Keys are always sorted
    ' (by the ReNumPanels Function)
'    i = 0
'    For Each p In col
'        i = i + 1
'        Set Project.Item(ProjectIndex).colPanel.Item(i).colLoadCase = p
'    Next p
    Dim bCurrentLC As Boolean
    bCurrentLC = False
    For Each LoadCase In colLC
        If LoadCase.index = Project.Item(ProjectIndex).cDisplaySettings.CurrentLoadCase Then
            bCurrentLC = True
            Exit For
        End If
    Next LoadCase
    
    If bCurrentLC = False Then
        Project.Item(ProjectIndex).cDisplaySettings.CurrentLoadCase = 0
        Project.Item(ProjectIndex).frmProject.StatusBar.Panels(3).Text = "LOAD CASE: None"
    End If

    Set col = Nothing
    Set colLC = Nothing
    Project.Item(ProjectIndex).DataChanged = True
    
    'PanelSide
    Dim oPanel As cPanel
    For Each oPanel In Project.Item(ProjectIndex).colPanel
        oPanel.LateralPressureSide = colPanelSide.Item(oPanel.index)
    Next oPanel
    
    Dim pan As MSComctlLib.Panel
    Set pan = Project.Item(ProjectIndex).frmProject.StatusBar.Panels.Item(3)
    Project.Item(ProjectIndex).frmProject.SetStatusBar pan
    Unload Me
    Draw ProjectIndex
End Sub

Private Sub cmdOK_DblClick(Cancel As MSForms.ReturnBoolean)
    Cancel = True
End Sub

Private Sub cmdRemove_DblClick(Cancel As MSForms.ReturnBoolean)
    Cancel = True
End Sub

' =============
' FLEXGRID EDIT
' =============

Private Sub MSH1_KeyDown(KeyCode As Integer, Shift As Integer)
'    Select Case MSH1.col
'        Case Is <> 2
'            If Len(MSH1) > 0 Then dBuffer = (MSH1)
'    End Select
    TxtEdit_KeyDown KeyCode, Shift
End Sub

Private Sub MSH1_Scroll()
    txtEdit.Visible = False
End Sub

Function grd_index(r As Integer, c As Integer)
    grd_index = c + MSH1.Cols * r
End Function

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
    Set col = Nothing
    Set colLC = Nothing
    Set colPanelSide = Nothing
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
    
    If MSH1.Row = 0 Then Exit Sub
    MSHFlexGridEdit MSH1, txtEdit, KeyAscii
End Sub

Private Sub MSH1_DblClick()
    Dim i As Integer
    Dim bOnMax As Boolean
    Dim bOnMin As Boolean
    getOnMin bOnMin
    getOnMax bOnMax
    Dim Contor As Integer
    Contor = 0
    If MSH1.Row = 0 Then Exit Sub
    Select Case MSH1.col
        Case 1
            MSHFlexGridEdit MSH1, txtEdit, 32
        Case 2
            If MSH1.TextMatrix(MSH1.Row, MSH1.col) = "ON" Then
                If bOnMin = True Then
                    MsgBox "At least one active load case required", vbExclamation + vbOKOnly, "Warning"
                    Exit Sub
                Else
                    MSH1.TextMatrix(MSH1.Row, MSH1.col) = "OFF"
                End If
            ElseIf MSH1.TextMatrix(MSH1.Row, MSH1.col) = "OFF" Then
                If bOnMax = True Then
                    MsgBox "Maximum ten active load cases allowed", vbExclamation + vbOKOnly, "Warning"
                    Exit Sub
                Else
                    MSH1.TextMatrix(MSH1.Row, MSH1.col) = "ON"
                End If
            End If
            SetData
    End Select
    'SetData
End Sub

Sub MSHFlexGridEdit(MSHFlexgrid As Control, edt As Control, KeyAscii As Integer)
If MSH1.col = 1 Then
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
End If
End Sub

Private Sub TxtEdit_KeyPress(KeyAscii As Integer)
    If KeyAscii = Asc(vbCr) Then KeyAscii = 0
End Sub

Private Sub TxtEdit_KeyDown(KeyCode As Integer, Shift As Integer)
    If MSH1.col <> 2 Then dBuffer = (MSH1)
    cmdOK.Default = False
    EditKeyCode MSH1, txtEdit, KeyCode, Shift
End Sub

Sub EditKeyCode(MSHFlexgrid As Control, edt As Control, KeyCode As Integer, Shift As Integer)
    Dim Cancel As Boolean
    Select Case KeyCode
    Case 27 ' esc
        edt.Visible = False
        MSHFlexgrid.SetFocus
    Case 13 'enter
        'ValidateText TxtEdit, Cancel
        TxtEdit_Validate False
        'If Cancel = True Then Exit Sub
        MSHFlexgrid.SetFocus
    Case 38 'up
        'ValidateText TxtEdit, Cancel
        TxtEdit_Validate False
        'If Cancel = True Then Exit Sub
        MSHFlexgrid.SetFocus
        'DoEvents
        If MSHFlexgrid.Row > MSHFlexgrid.FixedRows Then
           MSHFlexgrid.Row = MSHFlexgrid.Row - 1
        End If
    Case 40 'down
        MSHFlexgrid.SetFocus
        'ValidateText TxtEdit, Cancel
        TxtEdit_Validate False
        'If Cancel = True Then Exit Sub
        'DoEvents
        If MSHFlexgrid.Row < MSHFlexgrid.FixedRows - 1 Then
           MSHFlexgrid.Row = MSHFlexgrid.Row + 1
        End If
    End Select
End Sub

Private Sub MSH1_GotFocus()
    If txtEdit.Visible = False Then Exit Sub
    MSH1 = txtEdit
    txtEdit.Visible = False
    SetData
End Sub

Private Sub MSH1_LeaveCell()
    If txtEdit.Visible = False Then Exit Sub
    MSH1 = txtEdit
    SetData
    txtEdit.Visible = False
End Sub

Private Sub TxtEdit_LostFocus()
    MSH1_GotFocus
    cmdOK.Default = True
End Sub

Private Sub TxtEdit_Validate(Cancel As Boolean)
    ValidateText txtEdit, Cancel
    If Cancel = True Then txtEdit = dBuffer
End Sub

