VERSION 5.00
Object = "{0D452EE1-E08F-101A-852E-02608C4D0BB4}#2.0#0"; "FM20.DLL"
Object = "{5E9E78A0-531B-11CF-91F6-C2863C385E30}#1.0#0"; "MSFLXGRD.OCX"
Begin VB.Form frmPressuresUniformelyDistributed 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Lateral Uniformely Distributed Pressures"
   ClientHeight    =   5640
   ClientLeft      =   3765
   ClientTop       =   2310
   ClientWidth     =   5175
   Icon            =   "frmPressuresUniformelyDistributed.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   5640
   ScaleWidth      =   5175
   ShowInTaskbar   =   0   'False
   Begin VB.ComboBox Combo 
      Appearance      =   0  'Flat
      Height          =   315
      ItemData        =   "frmPressuresUniformelyDistributed.frx":000C
      Left            =   3720
      List            =   "frmPressuresUniformelyDistributed.frx":000E
      Style           =   2  'Dropdown List
      TabIndex        =   6
      Top             =   1560
      Visible         =   0   'False
      Width           =   975
   End
   Begin VB.TextBox TxtEdit 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   285
      Left            =   360
      TabIndex        =   3
      Text            =   "Text1"
      Top             =   1440
      Visible         =   0   'False
      Width           =   975
   End
   Begin VB.ComboBox cbLoadCases 
      Height          =   315
      ItemData        =   "frmPressuresUniformelyDistributed.frx":0010
      Left            =   1080
      List            =   "frmPressuresUniformelyDistributed.frx":0012
      Style           =   2  'Dropdown List
      TabIndex        =   0
      Top             =   240
      Width           =   3975
   End
   Begin MSFlexGridLib.MSFlexGrid MSH1 
      Height          =   4095
      Left            =   120
      TabIndex        =   1
      Top             =   720
      Width           =   4935
      _ExtentX        =   8705
      _ExtentY        =   7223
      _Version        =   393216
      Cols            =   9
      FixedCols       =   2
      GridColor       =   0
      ScrollTrack     =   -1  'True
      GridLinesFixed  =   1
      Appearance      =   0
      FormatString    =   "||||^"
   End
   Begin MSForms.CommandButton cmdOK 
      Default         =   -1  'True
      Height          =   375
      Left            =   2760
      TabIndex        =   5
      Top             =   5040
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
      Left            =   3960
      TabIndex        =   4
      Top             =   5040
      Width           =   1095
      Caption         =   "Cancel"
      PicturePosition =   327683
      Size            =   "1931;661"
      FontHeight      =   165
      FontCharSet     =   0
      FontPitchAndFamily=   2
      ParagraphAlign  =   3
   End
   Begin VB.Label lblLoadCases 
      AutoSize        =   -1  'True
      Caption         =   "Load Case: "
      Height          =   195
      Left            =   120
      TabIndex        =   2
      Top             =   250
      Width           =   855
   End
End
Attribute VB_Name = "frmPressuresUniformelyDistributed"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Panel As cPanel
Private LoadCase As cLoadCase
Private iLoadCase As Integer
Private col As New Collection
Private obj As Object
Private colSide As New Collection
Dim dBuffer As Double

Private Sub cbLoadCases_Click()
    iLoadCase = cbLoadCases.ListIndex + 1
    FlexGrid
End Sub

Private Function Validate() As Boolean
    Dim i As Integer, j As Integer, Cancel As Boolean
    For i = 1 To MSH1.Rows - 1
        For j = 2 To 3
            TxtEdit = MSH1.TextMatrix(i, j)
            ValidateNumeric TxtEdit, Cancel
            Validate = Cancel
            If Validate = True Then Exit Function
        Next j
    Next i
End Function

Private Sub cmdCancel_DblClick(Cancel As MSForms.ReturnBoolean)
    Cancel = True
End Sub

Private Sub cmdOK_DblClick(Cancel As MSForms.ReturnBoolean)
    Cancel = True
End Sub

Private Sub Form_Load()
    Me.Move (Screen.WIDTH - Me.WIDTH) / 2, (Screen.height - Me.height) / 2
    
    cmdOK.Picture = LoadResPicture("ID_BITMAP_OK", 0)
    cmdCancel.Picture = LoadResPicture("ID_BITMAP_CANCEL", 0)
    Me.Caption = "Uniformely Distributed Pressures - [" & Project.Item(ActiveProject).sDataFileName & "]"
    GetData
End Sub

Private Sub Form_Unload(Cancel As Integer)
    fMainForm.SetFocus
End Sub

Private Sub GetData()
    On Error Resume Next
    For Each Panel In Project.Item(ActiveProject).colPanel
        col.Add Panel.colLoadCase.Clone
        colSide.Add Panel.LateralPressureSide
    Next Panel
    For Each LoadCase In Project.Item(ActiveProject).cHeader.colLoadCase
        cbLoadCases.AddItem LoadCase.Title
    Next LoadCase
    cbLoadCases.ListIndex = 0
    Combo.AddItem "Left"
    Combo.AddItem "Right"
    Combo.AddItem "None"
    FlexGrid
End Sub

Private Sub SetData()
    Dim i As Integer
    Dim colTmp As New Collection
    'On Error Resume Next
    i = 0
    For Each obj In col
        i = i + 1
        For Each LoadCase In obj
            Select Case LoadCase.Title
                Case cbLoadCases.Text
                    obj.Item(LoadCase.Index).LateralPressureIn = MSH1.TextMatrix(i, 2)
                    obj.Item(LoadCase.Index).LateralPressureOut = MSH1.TextMatrix(i, 3)
                    Select Case MSH1.TextMatrix(i, 4)
                        Case "Left"
                            colTmp.Add SideLeft
                        Case "Right"
                            colTmp.Add SideRight
                        Case "None"
                            colTmp.Add SideNone
                    End Select
                    Exit For
            End Select
        Next LoadCase
    Next obj
    Set colSide = colTmp
    Set colTmp = Nothing
End Sub

Private Sub FlexGrid()
    Dim i As Integer
    'On Error GoTo 1
    MSH1.Cols = 5
    MSH1.Rows = col.Count + 1
    MSH1.ColWidth(0) = 0
    MSH1.ColWidth(1) = 650
    MSH1.ColWidth(2) = 1500
    MSH1.ColWidth(3) = 1500
    MSH1.ColWidth(4) = 950
    MSH1.TextMatrix(0, 1) = "Panel"
    MSH1.TextMatrix(0, 2) = "Start Node (m H2O)"
    MSH1.TextMatrix(0, 3) = "End Node (m H2O)"
    MSH1.TextMatrix(0, 4) = "Side"
    i = 0
    For Each obj In col
        i = i + 1
        For Each LoadCase In obj
            Select Case LoadCase.Title
                Case cbLoadCases.Text
                    MSH1.TextMatrix(i, 1) = i
                    MSH1.TextMatrix(i, 2) = obj.Item(LoadCase.Index).LateralPressureIn
                    MSH1.TextMatrix(i, 3) = obj.Item(LoadCase.Index).LateralPressureOut
                    Select Case colSide(i)
                        Case SideLeft
                            MSH1.TextMatrix(i, 4) = "Left"
                        Case SideRight
                            MSH1.TextMatrix(i, 4) = "Right"
                        Case SideNone
                            MSH1.TextMatrix(i, 4) = "None"
                    End Select
                    Exit For
            End Select
        Next LoadCase
    Next obj
1
End Sub

' =============
' FLEXGRID EDIT
' =============
Function grd_index(r As Integer, c As Integer)
    grd_index = c + MSH1.Cols * r
End Function

Private Sub cmdCancel_Click()
    Set colSide = Nothing
    Set col = Nothing
    Unload Me
End Sub

Private Sub cmdOK_Click()
    Dim i As Integer
    For Each Panel In Project.Item(ActiveProject).colPanel
        Set Panel.colLoadCase = col.Item(Panel.Index)
        Panel.LateralPressureSide = colSide(Panel.Index)
    Next Panel
    Set col = Nothing
    Set colSide = Nothing
    Draw
    Unload Me
End Sub

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
    Set col = Nothing
    Set colSide = Nothing
End Sub

Private Sub MSH1_KeyPress(KeyAscii As Integer)
    If MSH1.Row = 0 Then Exit Sub
    Select Case MSH1.col
        Case 1 To 3
            MSHFlexGridEdit MSH1, TxtEdit, KeyAscii
        Case 4
            MSHFlexGridEdit MSH1, Combo, KeyAscii
    End Select
End Sub

Private Sub MSH1_DblClick()
    If MSH1.Row = 0 Then Exit Sub
    Dim i As Integer
    Dim Contor As Integer
    Contor = 0
    Select Case MSH1.col
        Case 1 To 3
            MSHFlexGridEdit MSH1, TxtEdit, 32
        Case 4
            MSHFlexGridEdit MSH1, Combo, 32
   End Select
End Sub

Sub MSHFlexGridEdit(MSHFlexgrid As Control, edt As Control, KeyAscii As Integer)
    Select Case MSH1.col
        Case 1 To 3
            Select Case KeyAscii 'tasta apasata
            Case 0 To 32
                edt = MSHFlexgrid
                edt.SelStart = 1000
            Case Else
                edt = Chr(KeyAscii)
                edt.SelStart = 1
            End Select
            edt.Move MSHFlexgrid.left + MSHFlexgrid.CellLeft, _
                MSHFlexgrid.top + MSHFlexgrid.CellTop, _
                MSHFlexgrid.CellWidth - 8, _
                MSHFlexgrid.CellHeight - 8
            edt.Visible = True
            edt.SetFocus
        Case 4
            'If MSH1.ColSel = 1 And MSH1.Row > 0 Then
                Select Case KeyAscii 'tasta apasata
                Case 0 To 32
                Case Else
                End Select
                edt.Move MSHFlexgrid.left + MSHFlexgrid.CellLeft - 20, _
                    MSHFlexgrid.top + MSHFlexgrid.CellTop - 50, _
                    MSHFlexgrid.CellWidth + 7 ', _
                    MSHFlexgrid.CellHeight -8
                edt.Visible = True
                edt.Text = MSH1
                edt.SetFocus
                dropme Combo
            'End If
    End Select
End Sub

Private Sub MSH1_Scroll()
    TxtEdit.Visible = False
    Combo.Visible = False
End Sub

Private Sub TxtEdit_KeyPress(KeyAscii As Integer)
    If KeyAscii = Asc(vbCr) Then KeyAscii = 0
End Sub

Private Sub Combo_KeyPress(KeyAscii As Integer)
    If KeyAscii = Asc(vbCr) Then KeyAscii = 0
End Sub

Private Sub TxtEdit_KeyDown(KeyCode As Integer, Shift As Integer)
    dBuffer = CDbl(MSH1)
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
        Case 1 To 3
            If TxtEdit.Visible = False Then Exit Sub
            MSH1 = TxtEdit
            TxtEdit.Visible = False
        Case 4
            If Combo.Visible = False Then Exit Sub
            MSH1 = Combo.List(Combo.ListIndex)
            Combo.Visible = False
    End Select
End Sub

Private Sub MSH1_LeaveCell()
    Select Case MSH1.col
        Case 1 To 3
            If TxtEdit.Visible = False Then Exit Sub
            MSH1 = TxtEdit
            TxtEdit.Visible = False
        Case 4
            If Combo.Visible = False Then Exit Sub
            MSH1 = Combo.List(Combo.ListIndex)
            Combo.Visible = False
    End Select
End Sub

Private Sub TxtEdit_LostFocus()
    MSH1_GotFocus
    SetData
End Sub

Private Sub Combo_LostFocus()
    MSH1_GotFocus
    SetData
End Sub

Private Sub TxtEdit_Validate(Cancel As Boolean)
    ValidateNumeric TxtEdit, Cancel
    If Cancel = True Then TxtEdit = dBuffer
End Sub
