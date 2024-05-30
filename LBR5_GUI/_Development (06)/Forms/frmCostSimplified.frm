VERSION 5.00
Object = "{831FDD16-0C5C-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCTL.OCX"
Object = "{5E9E78A0-531B-11CF-91F6-C2863C385E30}#1.0#0"; "MSFLXGRD.OCX"
Object = "{0D452EE1-E08F-101A-852E-02608C4D0BB4}#2.0#0"; "FM20.DLL"
Begin VB.Form frmCostSimplified 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Cost Simplified"
   ClientHeight    =   4920
   ClientLeft      =   2415
   ClientTop       =   1980
   ClientWidth     =   6900
   Icon            =   "frmCostSimplified.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   4920
   ScaleWidth      =   6900
   ShowInTaskbar   =   0   'False
   Begin VB.TextBox TxtEdit 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   285
      Left            =   600
      TabIndex        =   3
      Text            =   "Text1"
      Top             =   1200
      Visible         =   0   'False
      Width           =   975
   End
   Begin MSFlexGridLib.MSFlexGrid MSH1 
      Height          =   3615
      Left            =   120
      TabIndex        =   0
      Top             =   480
      Width           =   6615
      _ExtentX        =   11668
      _ExtentY        =   6376
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
   Begin MSComctlLib.TabStrip TabStrip 
      Height          =   4215
      Left            =   0
      TabIndex        =   4
      Top             =   0
      Width           =   6855
      _ExtentX        =   12091
      _ExtentY        =   7435
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
      Left            =   4560
      TabIndex        =   1
      Top             =   4320
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
      Left            =   5760
      TabIndex        =   2
      Top             =   4320
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
Attribute VB_Name = "frmCostSimplified"
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
    Me.Caption = "Cost (simplified) - [" & GetFileName(Project.Item(ProjectIndex).sFileName) & "]"
    Set CostData = Project.Item(ProjectIndex).cHeader.cCostData.Clone
    TabStripProperties
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

Private Sub TabStripProperties()
    TabStrip.Tabs.Clear
    TabStrip.Tabs.Add 1, "ShipyardEfficiency", "Shipyard Efficiency"
    'TabStrip.Tabs.Add 2, "MaterialCosts", "Material Costs"
    TabStrip.Tabs.Add 2, "WorkingLoads", "Working Loads"
    TabStrip.Tabs.Add 3, "Consumables", "Consumables"
End Sub

Private Sub FlexGrid()
    Dim i As Integer
    MSH1.Clear
    Select Case TabStrip.SelectedItem.Key
        Case "ShipyardEfficiency"
            'MSH1.Rows = 3
            MSH1.Rows = 2
            'MSH1.Cols = 3
            MSH1.Cols = 2
            MSH1.RowHeight(0) = 230
            MSH1.RowHeight(1) = 460
            'MSH1.RowHeight(2) = 460
            MSH1.ColWidth(0) = 2000
            MSH1.ColWidth(1) = 1000
            'MSH1.ColWidth(2) = 1100
            MSH1.TextMatrix(1, 0) = "Yard production efficiency parameter (0.00 .. 1.00)"
            'MSH1.TextMatrix(2, 0) = "Weight equivalent parameter"
            MSH1.TextMatrix(0, 1) = "Value"
            'MSH1.TextMatrix(0, 2) = "Units"
            'MSH1.TextMatrix(2, 2) = "ton/man hour"
'            For i = 1 To MSH1.Rows - 1
'                MSH1.Row = i: MSH1.col = 2
'                MSH1.CellBackColor = &H8000000F
'            Next i
            
        Case "MaterialCosts"
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
        Case "WorkingLoads"
            MSH1.Rows = 10
            MSH1.Cols = 5
            MSH1.RowHeight(0) = 460
            MSH1.RowHeight(1) = 240
            MSH1.RowHeight(2) = 460
            MSH1.RowHeight(3) = 460
            MSH1.RowHeight(4) = 720
            MSH1.RowHeight(5) = 720
            MSH1.RowHeight(6) = 720
            MSH1.RowHeight(7) = 960
            MSH1.RowHeight(8) = 720
            MSH1.RowHeight(9) = 720
            
            MSH1.ColWidth(0) = 2000
            MSH1.ColWidth(1) = 1000
            MSH1.ColWidth(2) = 1450
            MSH1.ColWidth(3) = 1000
            MSH1.ColWidth(4) = 800
            
            MSH1.TextMatrix(1, 0) = "Plating and Straightening"
            MSH1.TextMatrix(2, 0) = "Welding Longitudinal Stiffeners on the Plating"
            MSH1.TextMatrix(3, 0) = "Welding Transverse Frames on the Plating"
            MSH1.TextMatrix(4, 0) = "Assembling longitudinal members from standard plates + additional works"
            MSH1.TextMatrix(5, 0) = "Assembling transversal members from standard plates + additional works"
            MSH1.TextMatrix(6, 0) = "Cutting a slot for intersect and join a longitudinal and a  transversal frame"
            MSH1.TextMatrix(7, 0) = "Fixing brackets at intersection between a longitudinal and a  transversal frame"
            MSH1.TextMatrix(8, 0) = "Bracket frequency along longitudinal stiffeners    (0.00 ... 1.00)"
            MSH1.TextMatrix(9, 0) = "Bracket frequency along transverse frames       (0.00 ... 1.00)"
            
            MSH1.TextMatrix(0, 1) = "Working load"
            MSH1.TextMatrix(0, 2) = "Units"
            MSH1.TextMatrix(0, 3) = "Relative variation"
            MSH1.TextMatrix(0, 4) = "Units"
            
            MSH1.TextMatrix(1, 2) = "man-h/m*m"
            MSH1.TextMatrix(2, 2) = "man-h/m"
            MSH1.TextMatrix(3, 2) = "man-h/m"
            MSH1.TextMatrix(4, 2) = "man-h/m"
            MSH1.TextMatrix(5, 2) = "man-h/m"
            MSH1.TextMatrix(6, 2) = "man-h/intersection"
            MSH1.TextMatrix(7, 2) = "man-h/intersection"
            MSH1.TextMatrix(1, 4) = "%/mm"
            MSH1.TextMatrix(2, 4) = "%/mm"
            MSH1.TextMatrix(3, 4) = "%/mm"
            MSH1.TextMatrix(4, 4) = "%/mm"
            MSH1.TextMatrix(5, 4) = "%/mm"
            For i = 1 To MSH1.Rows - 1
                MSH1.Row = i: MSH1.col = 2
                MSH1.CellBackColor = &H8000000F
                MSH1.Row = i: MSH1.col = 4
                MSH1.CellBackColor = &H8000000F
            Next i
            For i = 6 To MSH1.Rows - 1
                MSH1.Row = i: MSH1.col = 3
                MSH1.CellBackColor = &H8000000F
            Next i
        Case "Consumables"
            MSH1.Rows = 5
            MSH1.Cols = 3
            
            MSH1.RowHeight(1) = 720
            MSH1.RowHeight(2) = 240
            MSH1.RowHeight(3) = 720
            MSH1.RowHeight(4) = 720
            
            MSH1.ColWidth(0) = 2000
            MSH1.ColWidth(1) = 1000
            MSH1.ColWidth(2) = 1000
            
            MSH1.TextMatrix(1, 0) = "Cost (energy, electrodes, provisions for equipment depreciation)"
            MSH1.TextMatrix(2, 0) = "Cost relative variation"
            MSH1.TextMatrix(3, 0) = "Cost coefficient if standard  (=1) or manufactured (=0)  longitudinal members"
            MSH1.TextMatrix(4, 0) = "Cost coefficient if standard (=1) or manufactured (=0) transversal members"
            
            MSH1.TextMatrix(0, 1) = "Value"
            MSH1.TextMatrix(0, 2) = "Units"
            MSH1.TextMatrix(1, 2) = "Currency/m"
            MSH1.TextMatrix(2, 2) = "%/mm"
            
            For i = 1 To MSH1.Rows - 1
                MSH1.Row = i: MSH1.col = 2
                MSH1.CellBackColor = &H8000000F
            Next i
    End Select
    GetData
End Sub

Private Sub GetData()
    Select Case TabStrip.SelectedItem.Key
        Case "ShipyardEfficiency"
            MSH1.TextMatrix(1, 1) = CostData.REND
            'MSH1.TextMatrix(2, 1) = CostData.EQP
        Case "MaterialCosts"
            MSH1.TextMatrix(1, 1) = CostData.E0
            MSH1.TextMatrix(1, 2) = CostData.E0X
            MSH1.TextMatrix(1, 3) = CostData.E0Y
            MSH1.TextMatrix(2, 1) = CostData.C1
            MSH1.TextMatrix(2, 2) = CostData.C2
            MSH1.TextMatrix(2, 3) = CostData.C3
            MSH1.TextMatrix(3, 1) = CostData.DC1
            MSH1.TextMatrix(4, 2) = CostData.DW2
            MSH1.TextMatrix(4, 3) = CostData.DW3
        Case "WorkingLoads"
            MSH1.TextMatrix(1, 1) = CostData.P10
            MSH1.TextMatrix(1, 3) = CostData.DP10 * 100
            MSH1.TextMatrix(2, 1) = CostData.p4
            MSH1.TextMatrix(2, 3) = CostData.DP4 * 100
            MSH1.TextMatrix(3, 1) = CostData.P5
            MSH1.TextMatrix(3, 3) = CostData.DP5 * 100
            MSH1.TextMatrix(4, 1) = CostData.P9X
            MSH1.TextMatrix(4, 3) = CostData.DP9X * 100
            MSH1.TextMatrix(5, 1) = CostData.P9Y
            MSH1.TextMatrix(5, 3) = CostData.DP9Y * 100
            MSH1.TextMatrix(6, 1) = CostData.P6
            MSH1.TextMatrix(7, 1) = CostData.P7
            MSH1.TextMatrix(8, 1) = CostData.BETA_X
            MSH1.TextMatrix(9, 1) = CostData.BETA_Y
        Case "Consumables"
            MSH1.TextMatrix(1, 1) = CostData.C8
            MSH1.TextMatrix(2, 1) = CostData.DC8 * 100
            MSH1.TextMatrix(3, 1) = CostData.ALPHA_X
            MSH1.TextMatrix(4, 1) = CostData.ALPHA_Y
    End Select
End Sub

Private Sub SetData()
    On Error GoTo SetDataErr
    Select Case TabStrip.SelectedItem.Key
        Case "ShipyardEfficiency"
            CostData.REND = MSH1.TextMatrix(1, 1)
            'CostData.EQP = MSH1.TextMatrix(2, 1)
        Case "MaterialCosts"
            CostData.E0 = MSH1.TextMatrix(1, 1)
            CostData.E0X = MSH1.TextMatrix(1, 2)
            CostData.E0Y = MSH1.TextMatrix(1, 3)
            CostData.C1 = MSH1.TextMatrix(2, 1)
            CostData.C2 = MSH1.TextMatrix(2, 2)
            CostData.C3 = MSH1.TextMatrix(2, 3)
            CostData.DC1 = MSH1.TextMatrix(3, 1)
            CostData.DW2 = MSH1.TextMatrix(4, 2)
            CostData.DW3 = MSH1.TextMatrix(4, 3)
        Case "WorkingLoads"
            CostData.P10 = MSH1.TextMatrix(1, 1)
            CostData.DP10 = MSH1.TextMatrix(1, 3) / 100
            CostData.p4 = MSH1.TextMatrix(2, 1)
            CostData.DP4 = MSH1.TextMatrix(2, 3) / 100
            CostData.P5 = MSH1.TextMatrix(3, 1)
            CostData.DP5 = MSH1.TextMatrix(3, 3) / 100
            CostData.P9X = MSH1.TextMatrix(4, 1)
            CostData.DP9X = MSH1.TextMatrix(4, 3) / 100
            CostData.P9Y = MSH1.TextMatrix(5, 1)
            CostData.DP9Y = MSH1.TextMatrix(5, 3) / 100
            CostData.P6 = MSH1.TextMatrix(6, 1)
            CostData.P7 = MSH1.TextMatrix(7, 1)
            CostData.BETA_X = MSH1.TextMatrix(8, 1)
            CostData.BETA_Y = MSH1.TextMatrix(9, 1)
        Case "Consumables"
            CostData.C8 = MSH1.TextMatrix(1, 1)
            CostData.DC8 = MSH1.TextMatrix(2, 1) / 100
            CostData.ALPHA_X = MSH1.TextMatrix(3, 1)
            CostData.ALPHA_Y = MSH1.TextMatrix(4, 1)
    End Select
    Exit Sub
SetDataErr:
    MsgBox "Invalid format", vbCritical + vbOKOnly, "Error"
    GetData
End Sub
Private Sub Form_Unload(Cancel As Integer)
    Set CostData = Nothing
    fMainForm.SetFocus
End Sub


Private Sub TabStrip_Change()
    FlexGrid
End Sub

' =============
' FLEXGRID EDIT
' =============

Private Sub MSH1_KeyDown(KeyCode As Integer, Shift As Integer)
    'If Len(MSH1) > 0 Then dBuffer = CDbl(MSH1)
    
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

Private Sub TabStrip_BeforeClick(Cancel As Integer)
    If TxtEdit.Visible = True Then
       Cancel = -1
    End If
End Sub

Private Sub TabStrip_Click()
    TxtEdit.Visible = False
    MSH1.Row = 1
    MSH1.col = 1
    FlexGrid
    SetData
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
