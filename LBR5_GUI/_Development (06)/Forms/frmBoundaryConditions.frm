VERSION 5.00
Object = "{5E9E78A0-531B-11CF-91F6-C2863C385E30}#1.0#0"; "MSFLXGRD.OCX"
Object = "{0D452EE1-E08F-101A-852E-02608C4D0BB4}#2.0#0"; "FM20.DLL"
Begin VB.Form frmBoundaryConditions 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Boundary Conditions"
   ClientHeight    =   3825
   ClientLeft      =   2715
   ClientTop       =   2580
   ClientWidth     =   3000
   Icon            =   "frmBoundaryConditions.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   3825
   ScaleWidth      =   3000
   ShowInTaskbar   =   0   'False
   Begin VB.ComboBox Combo 
      Appearance      =   0  'Flat
      Height          =   315
      ItemData        =   "frmBoundaryConditions.frx":000C
      Left            =   1560
      List            =   "frmBoundaryConditions.frx":000E
      Style           =   2  'Dropdown List
      TabIndex        =   2
      Top             =   720
      Visible         =   0   'False
      Width           =   975
   End
   Begin MSFlexGridLib.MSFlexGrid MSH1 
      Height          =   3015
      Left            =   120
      TabIndex        =   3
      Top             =   120
      Width           =   2775
      _ExtentX        =   4895
      _ExtentY        =   5318
      _Version        =   393216
      Cols            =   9
      GridColor       =   0
      ScrollTrack     =   -1  'True
      GridLinesFixed  =   1
      Appearance      =   0
   End
   Begin MSForms.CommandButton cmdCancel 
      Cancel          =   -1  'True
      Height          =   375
      Left            =   1800
      TabIndex        =   1
      Top             =   3240
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
      Left            =   600
      TabIndex        =   0
      Top             =   3240
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
Attribute VB_Name = "frmBoundaryConditions"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Dim ProjectIndex As Integer
Dim Panel As New cPanel
Dim colPanel As colPanel
Dim col As New Collection
Dim OBJ As Object
Dim Boundary As New cBoundaryConditions

Private Sub cmdCancel_Click()
    Set col = Nothing
    Set Boundary = Nothing
    Set Panel = Nothing
    Unload Me
End Sub

Private Sub cmdCancel_DblClick(Cancel As MSForms.ReturnBoolean)
    Cancel = True
End Sub

Private Sub Form_Load()
    mnuEdit.Visible = False
    ProjectIndex = ActiveProject
    Me.Move (Screen.Width - Me.Width) / 2, (Screen.Height - Me.Height) / 2
    cmdOK.Picture = LoadResPicture("ID_BITMAP_OK", 0)
    cmdCancel.Picture = LoadResPicture("ID_BITMAP_CANCEL", 0)
    Me.Caption = "Boundary Conditions - [" & GetFileName(Project.Item(ProjectIndex).sFileName) & "]"
    PopulateCombo
    GetData
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
    MSH1.Cols = 2
    MSH1.ColWidth(0) = 700
    MSH1.ColWidth(1) = 1700
    MSH1.TextMatrix(0, 0) = "Panels"
    MSH1.TextMatrix(0, 1) = "Boundary Conditions"
    Dim index As Integer
    Dim i As Integer
    Dim j As Integer
    index = 0
    For i = 1 To col.Count
        For j = 1 To col.Item(i).Count
            index = index + 1
            MSH1.Rows = index + 1
            MSH1.TextMatrix(index, 0) = i
            MSH1.TextMatrix(index, 1) = Boundary.GetBoundaryName(col.Item(i).Item(j).BoundaryCondition)
        Next j
    Next i
End Sub

'Private Sub FillGrid()
'End Sub

Private Sub GetData()
    For Each Panel In Project.Item(ProjectIndex).colPanel
        col.Add Panel.colBoundaryConditions.Clone
    Next Panel
End Sub

Private Sub SetData()
    'On Error GoTo  SetDataErr
    Dim i As Integer, j As Integer
    Dim index As Integer
    Call IsBoundaries(index)
    For i = 1 To index
        If col.Item(CInt(MSH1.TextMatrix(i, 0))).Count = 1 Then
            col.Item(CInt(MSH1.TextMatrix(i, 0))).Item(1).BoundaryCondition = Boundary.SetBoundaryName(MSH1.TextMatrix(i, 1))
        ElseIf col.Item(CInt(MSH1.TextMatrix(i, 0))).Count = 2 Then
            col.Item(CInt(MSH1.TextMatrix(i, 0))).Item(1).BoundaryCondition = Boundary.SetBoundaryName(MSH1.TextMatrix(i, 1))
            i = i + 1
            col.Item(CInt(MSH1.TextMatrix(i, 0))).Item(2).BoundaryCondition = Boundary.SetBoundaryName(MSH1.TextMatrix(i, 1))
        End If
    Next i
    'Exit Sub
'SetDataErr:
'    MsgBox "Invalid format", vbCritical + vbOKOnly, "Error"
'    Set col = Nothing
'    FillGrid
End Sub

Private Sub cmdOK_Click()
    For Each Panel In Project.Item(ProjectIndex).colPanel
        Set Panel.colBoundaryConditions = col.Item(Panel.index)
    Next Panel
    Set col = Nothing
    Set Boundary = Nothing
    Set Panel = Nothing
    Draw ProjectIndex
    Project.Item(ProjectIndex).DataChanged = True
    Unload Me
End Sub

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
    Set col = Nothing
    Set Boundary = Nothing
    Set Panel = Nothing
End Sub

Private Sub PopulateCombo()
    Combo.AddItem "Free Edge"
    Combo.AddItem "Simply supported 2"
    Combo.AddItem "Simply supported 3"
    Combo.AddItem "Simply supported 4"
    Combo.AddItem "Simply supported 5"
    Combo.AddItem "Clamped"
    Combo.AddItem "Symmetry Axis 1"
    Combo.AddItem "Simply supported 6"
    Combo.AddItem "Simply supported 7"
    Combo.AddItem "Simply supported 8"
    Combo.AddItem "Symmetry Axis 2"
    Combo.AddItem "Double Symmetry"
End Sub

' =============
' FLEXGRID EDIT
' =============
Function grd_index(r As Integer, c As Integer)
    grd_index = c + MSH1.Cols * r
End Function


Private Sub Form_Unload(Cancel As Integer)
    fMainForm.SetFocus
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
    
    Select Case MSH1.col
        Case 1
            MSHFlexGridEdit MSH1, Combo, KeyAscii
    End Select
End Sub

Private Sub MSH1_DblClick()
    On Error Resume Next
    Dim i As Integer
    Dim Contor As Integer
    Contor = 0
    Select Case MSH1.col
        Case 1
            MSHFlexGridEdit MSH1, Combo, 32
    End Select
End Sub

Sub MSHFlexGridEdit(MSHFlexgrid As Control, edt As Control, KeyAscii As Integer)
    Select Case MSH1.col
        Case 1
            edt.Move MSHFlexgrid.Left + MSHFlexgrid.CellLeft - 20, _
                MSHFlexgrid.Top + MSHFlexgrid.CellTop - 50, _
                MSHFlexgrid.CellWidth + 7
            edt.Visible = True
            edt.Text = MSH1
            edt.SetFocus
            dropme Combo
    End Select
End Sub

Private Sub MSH1_Scroll()
    Combo.Visible = False
End Sub

Private Sub Combo_KeyPress(KeyAscii As Integer)
    If KeyAscii = Asc(vbCr) Then KeyAscii = 0
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
        MSHFlexgrid.SetFocus
    Case 38 'up
        MSHFlexgrid.SetFocus
        DoEvents
        If MSHFlexgrid.Row > MSHFlexgrid.FixedRows Then
           MSHFlexgrid.Row = MSHFlexgrid.Row - 1
        End If
    Case 40 'down
        MSHFlexgrid.SetFocus
        DoEvents
        If MSHFlexgrid.Row < MSHFlexgrid.FixedRows - 1 Then
           MSHFlexgrid.Row = MSHFlexgrid.Row + 1
        End If
    End Select
End Sub

Private Sub MSH1_GotFocus()
    Select Case MSH1.col
        Case 1
            If Combo.Visible = False Then Exit Sub
            MSH1 = Combo.List(Combo.ListIndex)
            Combo.Visible = False
    End Select
End Sub

Private Sub MSH1_LeaveCell()
    Select Case MSH1.col
        Case 1
            If Combo.Visible = False Then Exit Sub
            MSH1 = Combo.List(Combo.ListIndex)
            Combo.Visible = False
    End Select
End Sub

Private Sub Combo_LostFocus()
    MSH1_GotFocus
    SetData
End Sub
