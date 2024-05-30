VERSION 5.00
Object = "{5E9E78A0-531B-11CF-91F6-C2863C385E30}#1.0#0"; "MSFLXGRD.OCX"
Object = "{0D452EE1-E08F-101A-852E-02608C4D0BB4}#2.0#0"; "FM20.DLL"
Begin VB.Form frmMaterialCosts 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Material Costs"
   ClientHeight    =   2925
   ClientLeft      =   2805
   ClientTop       =   5610
   ClientWidth     =   5445
   Icon            =   "frmMaterialCosts.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   2925
   ScaleWidth      =   5445
   ShowInTaskbar   =   0   'False
   Begin VB.TextBox txtEQP 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   0
      TabIndex        =   4
      Top             =   2520
      Width           =   735
   End
   Begin VB.TextBox TxtEdit 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   285
      Left            =   480
      TabIndex        =   2
      Text            =   "Text1"
      Top             =   720
      Visible         =   0   'False
      Width           =   975
   End
   Begin MSFlexGridLib.MSFlexGrid MSH1 
      Height          =   2175
      Left            =   0
      TabIndex        =   3
      Top             =   0
      Width           =   5415
      _ExtentX        =   9551
      _ExtentY        =   3836
      _Version        =   393216
      Cols            =   9
      GridColor       =   0
      WordWrap        =   -1  'True
      ScrollTrack     =   -1  'True
      GridLinesFixed  =   1
      ScrollBars      =   2
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
   Begin VB.Label Label2 
      Caption         =   "[ton/man hour]"
      Height          =   255
      Left            =   840
      TabIndex        =   6
      Top             =   2520
      Width           =   1095
   End
   Begin VB.Label Label1 
      Caption         =   "Weight equivalent parameter:"
      Height          =   255
      Left            =   0
      TabIndex        =   5
      Top             =   2280
      Width           =   2175
   End
   Begin MSForms.CommandButton cmdCancel 
      Cancel          =   -1  'True
      Height          =   375
      Left            =   4320
      TabIndex        =   1
      Top             =   2400
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
      Left            =   3120
      TabIndex        =   0
      Top             =   2400
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
Attribute VB_Name = "frmMaterialCosts"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Dim ProjectIndex As Integer
Dim CostData As cCostData
Dim dBuffer As String

Private Sub cmdCancel_Click()
    Unload Me
End Sub

Private Sub cmdCancel_DblClick(Cancel As MSForms.ReturnBoolean)
    Cancel = True
End Sub

Private Sub cmdOK_Click()
    CostData.EQP = txtEQP.Text
    Set Project.Item(ProjectIndex).cHeader.cCostData = CostData
    Project.Item(ProjectIndex).DataChanged = True
    Unload Me
End Sub

Private Sub cmdOK_DblClick(Cancel As MSForms.ReturnBoolean)
    Cancel = True
End Sub

Private Sub Form_Load()
    mnuEdit.Visible = False
    ProjectIndex = ActiveProject
    Me.Move (Screen.Width - Me.Width) / 2, (Screen.Height - Me.Height) / 2
    cmdOK.Picture = LoadResPicture("ID_BITMAP_OK", 0)
    cmdCancel.Picture = LoadResPicture("ID_BITMAP_CANCEL", 0)
    Me.Caption = "Material Costs - [" & GetFileName(Project.Item(ProjectIndex).sFileName) & "]"
    Set CostData = Project.Item(ProjectIndex).cHeader.cCostData.Clone
    FlexGrid
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

Private Sub FlexGrid()
    Dim i As Integer
    MSH1.Clear
            
    MSH1.Rows = 5
    MSH1.Cols = 5
    MSH1.RowHeight(0) = 460
    MSH1.RowHeight(1) = 460
    MSH1.RowHeight(2) = 460
    MSH1.RowHeight(3) = 230
    MSH1.RowHeight(4) = 230
    
    MSH1.ColWidth(0) = 1300
    MSH1.ColWidth(1) = 1000
    MSH1.ColWidth(2) = 1000
    MSH1.ColWidth(3) = 1000
    MSH1.ColWidth(4) = 1000
    
    MSH1.TextMatrix(1, 0) = "Reference thickness"
    MSH1.TextMatrix(2, 0) = "Associated cost per kilogram"
    MSH1.TextMatrix(3, 0) = "Cost variation"
    MSH1.TextMatrix(4, 0) = "Extra Weight"
    MSH1.TextMatrix(0, 1) = "Plates"
    MSH1.TextMatrix(0, 2) = "Longitudinal members"
    MSH1.TextMatrix(0, 3) = "Transversal members"
    MSH1.TextMatrix(0, 4) = "Units"
    MSH1.TextMatrix(1, 4) = "m"
    MSH1.TextMatrix(2, 4) = "Currency/kg"
    MSH1.TextMatrix(3, 4) = "%/mm"
    MSH1.TextMatrix(4, 4) = "%"
    For i = 1 To MSH1.Rows - 1
        MSH1.Row = i: MSH1.col = 4
        MSH1.CellBackColor = &H8000000F
    Next i
    For i = 2 To 3
        MSH1.Row = 3: MSH1.col = i
        MSH1.CellBackColor = &H8000000F
    Next i
    MSH1.Row = 4: MSH1.col = 1
    MSH1.CellBackColor = &H8000000F
    GetData
End Sub

Private Sub GetData()
    MSH1.TextMatrix(1, 1) = CostData.E0
    MSH1.TextMatrix(1, 2) = CostData.E0X
    MSH1.TextMatrix(1, 3) = CostData.E0Y
    MSH1.TextMatrix(2, 1) = CostData.C1
    MSH1.TextMatrix(2, 2) = CostData.C2
    MSH1.TextMatrix(2, 3) = CostData.C3
    MSH1.TextMatrix(3, 1) = CostData.DC1 * 100
    MSH1.TextMatrix(4, 2) = CostData.DW2
    MSH1.TextMatrix(4, 3) = CostData.DW3
    txtEQP.Text = CostData.EQP
End Sub

Private Sub SetData()
    On Error GoTo SetDataErr
    CostData.E0 = MSH1.TextMatrix(1, 1)
    CostData.E0X = MSH1.TextMatrix(1, 2)
    CostData.E0Y = MSH1.TextMatrix(1, 3)
    CostData.C1 = MSH1.TextMatrix(2, 1)
    CostData.C2 = MSH1.TextMatrix(2, 2)
    CostData.C3 = MSH1.TextMatrix(2, 3)
    CostData.DC1 = MSH1.TextMatrix(3, 1) / 100
    CostData.DW2 = MSH1.TextMatrix(4, 2)
    CostData.DW3 = MSH1.TextMatrix(4, 3)
    Exit Sub
SetDataErr:
    MsgBox "Invalid format", vbCritical + vbOKOnly, "Error"
    GetData
End Sub
Private Sub Form_Unload(Cancel As Integer)
    Set CostData = Nothing
    fMainForm.SetFocus
End Sub

' =============
' FLEXGRID EDIT
' =============

Private Sub MSH1_KeyDown(KeyCode As Integer, Shift As Integer)
    TxtEdit_KeyDown KeyCode, Shift
End Sub

Function grd_index(r As Integer, c As Integer)
    grd_index = c + MSH1.Cols * r
End Function

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
    Select Case MSH1.CellBackColor
        Case &H8000000F
        Case Else
            MSHFlexGridEdit MSH1, TxtEdit, KeyAscii
    End Select
End Sub

Private Sub MSH1_DblClick()
    Select Case MSH1.CellBackColor
        Case &H8000000F
        Case Else
            MSHFlexGridEdit MSH1, TxtEdit, 32
    End Select
End Sub

Sub MSHFlexGridEdit(MSHFlexgrid As Control, edt As Control, KeyAscii As Integer)
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
End Sub

Private Sub MSH1_Scroll()
    TxtEdit.Visible = False
End Sub

Private Sub TxtEdit_KeyPress(KeyAscii As Integer)
    If KeyAscii = Asc(vbCr) Then KeyAscii = 0
End Sub

Private Sub TxtEdit_KeyDown(KeyCode As Integer, Shift As Integer)
    dBuffer = (MSH1)
    cmdOK.Default = False
    EditKeyCode MSH1, TxtEdit, KeyCode, Shift
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
        MSHFlexgrid.SetFocus
        TxtEdit_Validate False
        'DoEvents
        If MSHFlexgrid.Row < MSHFlexgrid.FixedRows - 1 Then
           MSHFlexgrid.Row = MSHFlexgrid.Row + 1
        End If
    End Select
End Sub

Private Sub MSH1_GotFocus()
    If TxtEdit.Visible = False Then Exit Sub
    MSH1 = TxtEdit
    TxtEdit.Visible = False
End Sub

Private Sub MSH1_LeaveCell()
    If TxtEdit.Visible = False Then Exit Sub
    MSH1 = TxtEdit
    TxtEdit.Visible = False
End Sub

Private Sub TxtEdit_LostFocus()
    MSH1_GotFocus
    SetData
    cmdOK.Default = True
End Sub

Private Sub TxtEdit_Validate(Cancel As Boolean)
    ValidateNumeric TxtEdit, Cancel
    If Cancel = True Then TxtEdit = dBuffer
End Sub

Private Sub txtEQP_GotFocus()
    txtEQP.SelStart = 0
    txtEQP.SelLength = Len(txtEQP.Text)
    dBuffer = Val_(txtEQP.Text)
End Sub

Private Sub txtEQP_Validate(Cancel As Boolean)
    ValidateNonNullPozitive txtEQP, Cancel
    If Cancel = True Then txtEQP = dBuffer
End Sub
