VERSION 5.00
Object = "{5E9E78A0-531B-11CF-91F6-C2863C385E30}#1.0#0"; "MSFLXGRD.OCX"
Object = "{0D452EE1-E08F-101A-852E-02608C4D0BB4}#2.0#0"; "FM20.DLL"
Begin VB.Form frmCostCAtWorkshop 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Coefficients Accessibilité - Atelier"
   ClientHeight    =   2970
   ClientLeft      =   5985
   ClientTop       =   5370
   ClientWidth     =   5610
   Icon            =   "frmCostCAtWorkshop.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   2970
   ScaleWidth      =   5610
   ShowInTaskbar   =   0   'False
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
      Left            =   120
      TabIndex        =   3
      Top             =   120
      Width           =   5415
      _ExtentX        =   9551
      _ExtentY        =   3836
      _Version        =   393216
      Cols            =   9
      GridColor       =   0
      WordWrap        =   -1  'True
      ScrollTrack     =   -1  'True
      GridLinesFixed  =   1
      ScrollBars      =   0
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
   Begin MSForms.CommandButton cmdCancel 
      Cancel          =   -1  'True
      Height          =   375
      Left            =   4440
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
      Left            =   3240
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
End
Attribute VB_Name = "frmCostCAtWorkshop"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Dim ProjectIndex As Integer
Dim dBuffer As String
'Dim Header As New cHeader
'Dim Header1 As New cHeader
Dim Cost As cCostCAtMain

Private Sub cmdCancel_Click()
    Set Cost = Nothing
    Unload Me
End Sub

Private Sub cmdOK_Click()
    SetData
    Set Project.Item(ProjectIndex).cHeader.cCostCAtMain = Cost
    FillWorkshopCoeff
    Set Cost = Nothing
    Project.Item(ProjectIndex).DataChanged = True
    Unload Me
End Sub

Private Sub Form_Load()
    ProjectIndex = ActiveProject
    Me.Move (Screen.Width - Me.Width) / 2, (Screen.Height - Me.Height) / 2
    cmdOK.Picture = LoadResPicture("ID_BITMAP_OK", 0)
    cmdCancel.Picture = LoadResPicture("ID_BITMAP_CANCEL", 0)
    Me.Caption = "Coefficients Accessibilité - Atelier - [" & GetFileName(Project.Item(ProjectIndex).sFileName) & "]"
    GetData
    FlexGrid
End Sub

Private Sub FillWorkshopCoeff()
    Dim i As Integer
    Dim cPanel As cPanel
    For Each cPanel In Project.Item(ProjectIndex).colPanel
        For i = 1 To 10 'PrePreVoile
            cPanel.cCostCAtMain.colCostCAtOperations.Item(i).Accesibilite = Cost.CoefAccPrePreVoile
            cPanel.cCostCAtMain.colCostCAtOperations.Item(i).Atelier = Cost.CoefAtPrePreVoile
        Next i
        For i = 11 To 20 'PrePreNappe
            cPanel.cCostCAtMain.colCostCAtOperations.Item(i).Accesibilite = Cost.CoefAccPrePreNappe
            cPanel.cCostCAtMain.colCostCAtOperations.Item(i).Atelier = Cost.CoefAtPrePreNappe
        Next i
        For i = 21 To 34 'PreAssInner
            cPanel.cCostCAtMain.colCostCAtOperations.Item(i).Accesibilite = Cost.CoefAccPreAssInner
            cPanel.cCostCAtMain.colCostCAtOperations.Item(i).Atelier = Cost.CoefAtPreAssInner
        Next i
        For i = 35 To 45 'PreAssOuter
            cPanel.cCostCAtMain.colCostCAtOperations.Item(i).Accesibilite = Cost.CoefAccPreAssOuter
            cPanel.cCostCAtMain.colCostCAtOperations.Item(i).Atelier = Cost.CoefAtPreAssOuter
        Next i
        For i = 46 To 52 'Montage
            cPanel.cCostCAtMain.colCostCAtOperations.Item(i).Accesibilite = Cost.CoefAccMontage
            cPanel.cCostCAtMain.colCostCAtOperations.Item(i).Atelier = Cost.CoefAtMontage
        Next i
        cPanel.cCostCAtMain.colCostCAtOperations.Item(59).Accesibilite = Cost.CoefAccMontage 'Montage
        cPanel.cCostCAtMain.colCostCAtOperations.Item(59).Atelier = Cost.CoefAtMontage
        For i = 53 To 58 'Ponts
            cPanel.cCostCAtMain.colCostCAtOperations.Item(i).Accesibilite = Cost.CoefAccPonts
            cPanel.cCostCAtMain.colCostCAtOperations.Item(i).Atelier = Cost.CoefAtPonts
        Next i
        cPanel.cCostCAtMain.colCostCAtOperations.Item(60).Accesibilite = Cost.CoefAccUsinage 'Usinage
        cPanel.cCostCAtMain.colCostCAtOperations.Item(60).Atelier = Cost.CoefAtUsinage
    Next cPanel
End Sub

Private Sub GetData()
    On Error GoTo GetDataErr
    Set Cost = Project.Item(ProjectIndex).cHeader.cCostCAtMain.Clone
    Exit Sub
GetDataErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmCostCAtWorkshop: Sub GetData")
End Sub

Private Sub SetData()
    On Error GoTo SetDataErr
    'Accessibilité
    Cost.CoefAccPrePreVoile = Val_(MSH1.TextMatrix(1, 1))
    Cost.CoefAccPrePreNappe = Val_(MSH1.TextMatrix(2, 1))
    Cost.CoefAccPreAssInner = Val_(MSH1.TextMatrix(3, 1))
    Cost.CoefAccPreAssOuter = Val_(MSH1.TextMatrix(4, 1))
    Cost.CoefAccMontage = Val_(MSH1.TextMatrix(5, 1))
    Cost.CoefAccPonts = Val_(MSH1.TextMatrix(6, 1))
    Cost.CoefAccUsinage = Val_(MSH1.TextMatrix(7, 1))
    'Atelier
    Cost.CoefAtPrePreVoile = Val_(MSH1.TextMatrix(1, 2))
    Cost.CoefAtPrePreNappe = Val_(MSH1.TextMatrix(2, 2))
    Cost.CoefAtPreAssInner = Val_(MSH1.TextMatrix(3, 2))
    Cost.CoefAtPreAssOuter = Val_(MSH1.TextMatrix(4, 2))
    Cost.CoefAtMontage = Val_(MSH1.TextMatrix(5, 2))
    Cost.CoefAtPonts = Val_(MSH1.TextMatrix(6, 2))
    Cost.CoefAtUsinage = Val_(MSH1.TextMatrix(7, 2))
    Exit Sub
SetDataErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmCostCAtWorkshop: Sub SetData")
End Sub

Private Sub FlexGrid()
    On Error GoTo FlexGridErr
    Dim i As Integer
    With MSH1
        .Clear
        .Visible = True
        .Rows = 8
        .Cols = 3
        .FormatString = "^|^|^"

        .ColWidth(0) = 3300 '.Width / 5 + 300
        .ColWidth(1) = 1025 '.Width / 5
        .ColWidth(2) = 1025 '.Width / 4
        .RowHeight(0) = 230 * 2
        .TextMatrix(0, 0) = "Atelier"
        .TextMatrix(0, 1) = "Coefficients Accessibilité"
        .TextMatrix(0, 2) = "Coefficients Atelier"
    End With
    
    For i = 1 To MSH1.Rows - 1
        MSH1.RowHeight(i) = 240
    Next i
    MSH1.TextMatrix(1, 0) = "Pré-pré (cadre et voile)"
    MSH1.TextMatrix(2, 0) = "Pré-pré (nappe plane)"
    MSH1.TextMatrix(3, 0) = "Pré-assemblage (INNER, Bouchain, Bordé)"
    MSH1.TextMatrix(4, 0) = "Pré-assemblage (OUTER)"
    MSH1.TextMatrix(5, 0) = "Montage"
    MSH1.TextMatrix(6, 0) = "Ponts"
    MSH1.TextMatrix(7, 0) = "Usinage"
    'Accessibilité
    MSH1.TextMatrix(1, 1) = Cost.CoefAccPrePreVoile
    MSH1.TextMatrix(2, 1) = Cost.CoefAccPrePreNappe
    MSH1.TextMatrix(3, 1) = Cost.CoefAccPreAssInner
    MSH1.TextMatrix(4, 1) = Cost.CoefAccPreAssOuter
    MSH1.TextMatrix(5, 1) = Cost.CoefAccMontage
    MSH1.TextMatrix(6, 1) = Cost.CoefAccPonts
    MSH1.TextMatrix(7, 1) = Cost.CoefAccUsinage
    'Atelier
    MSH1.TextMatrix(1, 2) = Cost.CoefAtPrePreVoile
    MSH1.TextMatrix(2, 2) = Cost.CoefAtPrePreNappe
    MSH1.TextMatrix(3, 2) = Cost.CoefAtPreAssInner
    MSH1.TextMatrix(4, 2) = Cost.CoefAtPreAssOuter
    MSH1.TextMatrix(5, 2) = Cost.CoefAtMontage
    MSH1.TextMatrix(6, 2) = Cost.CoefAtPonts
    MSH1.TextMatrix(7, 2) = Cost.CoefAtUsinage
    Exit Sub
FlexGridErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmCostCAt: Sub FlexGrid")
End Sub

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
    Set Cost = Nothing
End Sub

Private Sub Form_Resize()
    MSH1.Height = 2200 '2160
    Me.Height = cmdOK.Top + cmdOK.Height + 600
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
