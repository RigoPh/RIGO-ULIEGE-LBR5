VERSION 5.00
Object = "{831FDD16-0C5C-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCTL.OCX"
Object = "{5E9E78A0-531B-11CF-91F6-C2863C385E30}#1.0#0"; "MSFLXGRD.OCX"
Object = "{0D452EE1-E08F-101A-852E-02608C4D0BB4}#2.0#0"; "FM20.DLL"
Begin VB.Form frmCostCAtTables 
   Caption         =   "Tableaux CostCAt"
   ClientHeight    =   4905
   ClientLeft      =   4125
   ClientTop       =   2790
   ClientWidth     =   6900
   Icon            =   "frmCostCAtTables.frx":0000
   LinkTopic       =   "Form1"
   MDIChild        =   -1  'True
   ScaleHeight     =   4905
   ScaleWidth      =   6900
   Begin VB.TextBox TxtEdit 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   285
      Left            =   960
      TabIndex        =   1
      Text            =   "Text1"
      Top             =   1440
      Visible         =   0   'False
      Width           =   975
   End
   Begin MSFlexGridLib.MSFlexGrid MSH1 
      Height          =   3615
      Left            =   120
      TabIndex        =   2
      Top             =   360
      Width           =   6615
      _ExtentX        =   11668
      _ExtentY        =   6376
      _Version        =   393216
      Cols            =   9
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
      Height          =   4215
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   6855
      _ExtentX        =   12091
      _ExtentY        =   7435
      MultiRow        =   -1  'True
      _Version        =   393216
      BeginProperty Tabs {1EFB6598-857C-11D1-B16A-00C0F0283628} 
         NumTabs         =   1
         BeginProperty Tab1 {1EFB659A-857C-11D1-B16A-00C0F0283628} 
            ImageVarType    =   2
         EndProperty
      EndProperty
   End
   Begin MSForms.CommandButton cmdWriteFiles 
      Default         =   -1  'True
      Height          =   375
      Left            =   120
      TabIndex        =   5
      Top             =   4320
      Width           =   1605
      VariousPropertyBits=   268435483
      Caption         =   "Write CostCAt Files"
      PicturePosition =   327683
      Size            =   "2831;661"
      FontHeight      =   165
      FontCharSet     =   0
      FontPitchAndFamily=   2
      ParagraphAlign  =   3
   End
   Begin MSForms.CommandButton cmdCancel 
      Cancel          =   -1  'True
      Height          =   375
      Left            =   5640
      TabIndex        =   4
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
   Begin MSForms.CommandButton cmdOK 
      Height          =   375
      Left            =   4440
      TabIndex        =   3
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
   Begin VB.Menu mnuEdit 
      Caption         =   "Edit"
      Begin VB.Menu mnuEditCopy 
         Caption         =   "Copy Ctrl+C"
      End
      Begin VB.Menu mnuEditPaste 
         Caption         =   "Paste Ctrl+V"
      End
   End
End
Attribute VB_Name = "frmCostCAtTables"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Dim ProjectIndex As Integer
Dim dBuffer As String
Dim colMain As Collection

Private Sub cmdCancel_Click()
    Unload Me
End Sub

Private Sub cmdCancel_DblClick(Cancel As MSForms.ReturnBoolean)
    Cancel = True
End Sub

Private Sub GetData()
    On Error GoTo GetDataErr
    Dim cPanel As cPanel
    Set colMain = New Collection
    For Each cPanel In Project.Item(ProjectIndex).colPanel
        colMain.Add cPanel.cCostCAtMain.Clone
    Next cPanel
    Exit Sub
GetDataErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmCostCAtTables: Sub GetData")
End Sub

Private Sub cmdOK_Click()
    Dim cM As cCostCAtMain
    Dim colM As Collection
    Dim cPanel As cPanel
    For Each cPanel In Project.Item(ProjectIndex).colPanel
        Set cPanel.cCostCAtMain = colMain.Item(cPanel.pNumber)
    Next cPanel
    Project.Item(ProjectIndex).DataChanged = True
    Unload Me
End Sub

Private Sub cmdOK_DblClick(Cancel As MSForms.ReturnBoolean)
    Cancel = True
End Sub

Private Sub cmdWriteFiles_Click()
    WriteCostCAtFiles ProjectIndex, 0, 0, 0, 0
End Sub

Private Sub Form_Load()
    ProjectIndex = ActiveProject
    Me.Move (Screen.Width - Me.Width) / 2, (Screen.Height - Me.Height) / 2
    cmdOK.Picture = LoadResPicture("ID_BITMAP_OK", 0)
    cmdCancel.Picture = LoadResPicture("ID_BITMAP_CANCEL", 0)
    Me.Caption = "Tableaux CostCAt - [" & GetFileName(Project.Item(ProjectIndex).sFileName) & "]"
    TabStripProperties
    Set txtEdit.Container = MSH1.Container
    GetData
    FlexGrid
End Sub

Private Sub TabStripProperties()
    TabStrip.Tabs.Clear
    TabStrip.Tabs.Add 1, "Input", "Input"
    TabStrip.Tabs.Add 2, "Fractionnement", "Fractionnement"
    TabStrip.Tabs.Add 3, "Accessibilité", "Accessibilité"
    TabStrip.Tabs.Add 4, "Atelier", "Atelier"
    TabStrip.Tabs.Add 5, "IDSoudures", "ID Soudures"
    TabStrip.Tabs.Add 6, "GorgesDeSoudure", "Gorges de Soudure"
End Sub

Private Sub SetData()
    On Error GoTo SetDataErr
    Dim i As Integer
    Dim Operation As cCostCAtOperations
    Dim cM As cCostCAtMain
    Select Case TabStrip.SelectedItem.Key
        Case "Input"
            For i = 1 To MSH1.Cols - 1
                Set cM = colMain.Item(i)
                cM.iNCI = MSH1.TextMatrix(1, i)
                cM.iNANP = MSH1.TextMatrix(2, i)
                cM.ID_PANNEAU = MSH1.TextMatrix(3, i)
                cM.IP_PANNEAU = MSH1.TextMatrix(4, i)
                cM.IT_PANNEAU = MSH1.TextMatrix(5, i)
                Select Case MSH1.TextMatrix(6, i)
                    Case 0
                        'cM.TypeTapes = 0
                    Case 1
                        Select Case cM.TypeTapes
                            Case 1, 2
                                cM.TypeTapes = 1
                            Case 3, 4
                                cM.TypeTapes = 3
                        End Select
                    Case 2
                        Select Case cM.TypeTapes
                            Case 1, 2
                                cM.TypeTapes = 2
                            Case 3, 4
                                cM.TypeTapes = 4
                        End Select
                End Select
                'cM.TypeTapes = MSH1.TextMatrix(6, i)
                cM.PositionAboutsLisses = MSH1.TextMatrix(7, i)
                Set cM = Nothing
            Next i
        Case "Fractionnement"
            For i = 1 To MSH1.Cols - 1
                Set cM = colMain.Item(i)
                For Each Operation In cM.colCostCAtOperations
                    Operation.Fractionnement = MSH1.TextMatrix(Operation.index, i)
                Next Operation
                Set cM = Nothing
            Next i
        Case "Accessibilité"
            For i = 1 To MSH1.Cols - 1
                Set cM = colMain.Item(i)
                For Each Operation In cM.colCostCAtOperations
                    Operation.Accesibilite = MSH1.TextMatrix(Operation.index, i)
                Next Operation
                Set cM = Nothing
            Next i
        Case "Atelier"
            For i = 1 To MSH1.Cols - 1
                Set cM = colMain.Item(i)
                For Each Operation In cM.colCostCAtOperations
                    Operation.Atelier = MSH1.TextMatrix(Operation.index, i)
                Next Operation
                Set cM = Nothing
            Next i
        Case "IDSoudures"
            For i = 1 To MSH1.Cols - 1
                Set cM = colMain.Item(i)
                cM.colCostCAtOperations.Item(21).Soudures = MSH1.TextMatrix(1, i)
                cM.colCostCAtOperations.Item(22).Soudures = MSH1.TextMatrix(2, i)
                cM.colCostCAtOperations.Item(23).Soudures = MSH1.TextMatrix(3, i)
                cM.colCostCAtOperations.Item(36).Soudures = MSH1.TextMatrix(4, i)
                cM.colCostCAtOperations.Item(37).Soudures = MSH1.TextMatrix(5, i)
                cM.colCostCAtOperations.Item(47).Soudures = MSH1.TextMatrix(6, i)
                cM.colCostCAtOperations.Item(48).Soudures = MSH1.TextMatrix(7, i)
                cM.colCostCAtOperations.Item(51).Soudures = MSH1.TextMatrix(8, i)
                Set cM = Nothing
            Next i
        Case "GorgesDeSoudure"
            For i = 1 To MSH1.Cols - 1
                Set cM = colMain.Item(i)
                cM.colCostCAtOperations.Item(21).Gorges = MSH1.TextMatrix(1, i)
                cM.colCostCAtOperations.Item(22).Gorges = MSH1.TextMatrix(2, i)
                cM.colCostCAtOperations.Item(23).Gorges = MSH1.TextMatrix(3, i)
                cM.colCostCAtOperations.Item(36).Gorges = MSH1.TextMatrix(4, i)
                cM.colCostCAtOperations.Item(37).Gorges = MSH1.TextMatrix(5, i)
                cM.colCostCAtOperations.Item(47).Gorges = MSH1.TextMatrix(6, i)
                cM.colCostCAtOperations.Item(48).Gorges = MSH1.TextMatrix(7, i)
                cM.colCostCAtOperations.Item(51).Gorges = MSH1.TextMatrix(8, i)
                Set cM = Nothing
            Next i
    End Select
    FlexGrid
    Exit Sub
SetDataErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmCostCAtTables: Sub SetData")
End Sub

Private Sub FlexGrid()
    Dim i As Integer
    Dim Operation As cCostCAtOperations
    Dim cM As cCostCAtMain
    Dim Rows As Integer, Cols As Integer
    Rows = NO_OPERATIONS + 1
    Cols = Project.Item(ProjectIndex).colPanel.Count + 1
    MSH1.Clear
    MSH1.Visible = True
    MSH1.Cols = Cols
    For i = 1 To Cols - 1
        MSH1.ColWidth(i) = 600
    Next i
    MSH1.FormatString = "<"
    MSH1.ColWidth(0) = 2800
    Select Case TabStrip.SelectedItem.Key
        Case "Input"
            MSH1.Rows = 8
            MSH1.TextMatrix(0, 0) = "Paramètres / Panneaux"
            MSH1.TextMatrix(1, 0) = "Nombre Couples Intermédiaires"
            MSH1.TextMatrix(2, 0) = "Nombre Abouts de la Nappe"
            MSH1.TextMatrix(3, 0) = "ID Panneau"
            MSH1.TextMatrix(4, 0) = "IP Panneau"
            MSH1.TextMatrix(5, 0) = "IT panneau"
            MSH1.TextMatrix(6, 0) = "Types Tapes"
            MSH1.TextMatrix(7, 0) = "Position Abouts"
            For i = 1 To Cols - 1
                Set cM = colMain.Item(i)
                MSH1.TextMatrix(0, i) = i
                MSH1.TextMatrix(1, i) = cM.iNCI
                MSH1.TextMatrix(2, i) = cM.iNANP
                MSH1.TextMatrix(3, i) = cM.ID_PANNEAU
                MSH1.TextMatrix(4, i) = cM.IP_PANNEAU
                MSH1.TextMatrix(5, i) = cM.IT_PANNEAU
                Select Case cM.TypeTapes
                    Case 0
                        MSH1.TextMatrix(6, i) = 0
                    Case 1, 3
                        MSH1.TextMatrix(6, i) = 1
                    Case 2, 4
                        MSH1.TextMatrix(6, i) = 2
                End Select
                'MSH1.TextMatrix(6, i) = cM.TypeTapes
                MSH1.TextMatrix(7, i) = cM.PositionAboutsLisses
                Set cM = Nothing
            Next i
        Case "Fractionnement"
            MSH1.Rows = Rows
            MSH1.TextMatrix(0, 0) = "Opérations / Panneaux"
            For i = 1 To NO_OPERATIONS
                MSH1.TextMatrix(i, 0) = GetOpName(i)
            Next i
            For i = 1 To Cols - 1
                Set cM = colMain.Item(i)
                For Each Operation In cM.colCostCAtOperations
                    MSH1.TextMatrix(0, i) = i
                    MSH1.TextMatrix(Operation.index, i) = Format(Operation.Fractionnement, "0.000")
                Next Operation
                Set cM = Nothing
            Next i
        Case "Accessibilité"
            MSH1.Rows = Rows
            MSH1.TextMatrix(0, 0) = "Opérations / Panneaux"
            For i = 1 To NO_OPERATIONS
                MSH1.TextMatrix(i, 0) = GetOpName(i)
            Next i
            For i = 1 To Cols - 1
                Set cM = colMain.Item(i)
                For Each Operation In cM.colCostCAtOperations
                    MSH1.TextMatrix(0, i) = i
                    MSH1.TextMatrix(Operation.index, i) = Format(Operation.Accesibilite, "0.000")
                Next Operation
                Set cM = Nothing
            Next i
        Case "Atelier"
            MSH1.Rows = Rows
            MSH1.TextMatrix(0, 0) = "Opérations / Panneaux"
            For i = 1 To NO_OPERATIONS
                MSH1.TextMatrix(i, 0) = GetOpName(i)
            Next i
            For i = 1 To Cols - 1
                Set cM = colMain.Item(i)
                For Each Operation In cM.colCostCAtOperations
                    MSH1.TextMatrix(0, i) = i
                    MSH1.TextMatrix(Operation.index, i) = Format(Operation.Atelier, "0.000")
                Next Operation
                Set cM = Nothing
            Next i '
        Case "IDSoudures"
            MSH1.Rows = 9
            For i = 1 To Cols - 1
                MSH1.TextMatrix(0, i) = i
            Next i
            MSH1.TextMatrix(0, 0) = "Opérations / Panneaux"
            MSH1.TextMatrix(1, 0) = GetOpName(21)
            MSH1.TextMatrix(2, 0) = GetOpName(22)
            MSH1.TextMatrix(3, 0) = GetOpName(23)
            MSH1.TextMatrix(4, 0) = GetOpName(36)
            MSH1.TextMatrix(5, 0) = GetOpName(37)
            MSH1.TextMatrix(6, 0) = GetOpName(47)
            MSH1.TextMatrix(7, 0) = GetOpName(48)
            MSH1.TextMatrix(8, 0) = GetOpName(51)
            For i = 1 To Cols - 1
                Set cM = colMain.Item(i)
                MSH1.TextMatrix(1, i) = cM.colCostCAtOperations.Item(21).Soudures
                MSH1.TextMatrix(2, i) = cM.colCostCAtOperations.Item(22).Soudures
                MSH1.TextMatrix(3, i) = cM.colCostCAtOperations.Item(23).Soudures
                MSH1.TextMatrix(4, i) = cM.colCostCAtOperations.Item(36).Soudures
                MSH1.TextMatrix(5, i) = cM.colCostCAtOperations.Item(37).Soudures
                MSH1.TextMatrix(6, i) = cM.colCostCAtOperations.Item(47).Soudures
                MSH1.TextMatrix(7, i) = cM.colCostCAtOperations.Item(48).Soudures
                MSH1.TextMatrix(8, i) = cM.colCostCAtOperations.Item(51).Soudures
                Set cM = Nothing
            Next i
        Case "GorgesDeSoudure"
            MSH1.Rows = 9
            For i = 1 To Cols - 1
                MSH1.TextMatrix(0, i) = i
            Next i
            MSH1.TextMatrix(0, 0) = "Opérations / Panneaux"
            MSH1.TextMatrix(1, 0) = GetOpName(21)
            MSH1.TextMatrix(2, 0) = GetOpName(22)
            MSH1.TextMatrix(3, 0) = GetOpName(23)
            MSH1.TextMatrix(4, 0) = GetOpName(36)
            MSH1.TextMatrix(5, 0) = GetOpName(37)
            MSH1.TextMatrix(6, 0) = GetOpName(47)
            MSH1.TextMatrix(7, 0) = GetOpName(48)
            MSH1.TextMatrix(8, 0) = GetOpName(51)
            For i = 1 To Cols - 1
                Set cM = colMain.Item(i)
                MSH1.TextMatrix(1, i) = Format(cM.colCostCAtOperations.Item(21).Gorges, "0.000")
                MSH1.TextMatrix(2, i) = Format(cM.colCostCAtOperations.Item(22).Gorges, "0.000")
                MSH1.TextMatrix(3, i) = Format(cM.colCostCAtOperations.Item(23).Gorges, "0.000")
                MSH1.TextMatrix(4, i) = Format(cM.colCostCAtOperations.Item(36).Gorges, "0.000")
                MSH1.TextMatrix(5, i) = Format(cM.colCostCAtOperations.Item(37).Gorges, "0.000")
                MSH1.TextMatrix(6, i) = Format(cM.colCostCAtOperations.Item(47).Gorges, "0.000")
                MSH1.TextMatrix(7, i) = Format(cM.colCostCAtOperations.Item(48).Gorges, "0.000")
                MSH1.TextMatrix(8, i) = Format(cM.colCostCAtOperations.Item(51).Gorges, "0.000")
                Set cM = Nothing
            Next i
    End Select
End Sub

Private Sub Form_Resize()
    On Error Resume Next
    With TabStrip
        .Top = Me.ScaleTop
        .Left = Me.ScaleLeft
        .Width = Me.ScaleWidth
        .Height = Me.ScaleHeight - 600
    End With
    With MSH1
        .Top = Me.ScaleTop + 370
        .Left = Me.ScaleLeft + 50
        .Width = Me.ScaleWidth - 100
        .Height = Me.ScaleHeight - 600 - 350 - 100
    End With
    With cmdCancel
        .Left = Me.Width - .Width - 150
        .Top = Me.Height - .Height - 500
    End With
    With cmdOK
        .Left = cmdCancel.Left - .Width - 100
        .Top = Me.Height - .Height - 500
    End With
    With cmdWriteFiles
        .Left = 100
        .Top = Me.Height - .Height - 500
    End With
End Sub

Private Function GetOpName(ByVal index As Integer) As String
    Select Case index
        Case 1
            GetOpName = "1 Point de départ"
        Case 2
            GetOpName = "2 Traçages"
        Case 3
            GetOpName = "3 Soudure continue profilés"
        Case 4
            GetOpName = "4 Soudure continue (h<1m)"
        Case 5
            GetOpName = "5 Soudure continue (h>1m)"
        Case 6
            GetOpName = "6 Accostage tôles"
        Case 7
            GetOpName = "7 Plat en buté"
        Case 8
            GetOpName = "8 Gousset sur semelle"
        Case 9
            GetOpName = "9 Soudure tôles accostées"
        Case 10
            GetOpName = "10 Soudure discontinue profilés"
        Case 11
            GetOpName = "11 Accostage tôles"
        Case 12
            GetOpName = "12 Réglage Joints"
        Case 13
            GetOpName = "13 Réglage Abouts"
        Case 14
            GetOpName = "14 Traçages"
        Case 15
            GetOpName = "15 Oxycoupage"
        Case 16
            GetOpName = "16 Réglage profilés"
        Case 17
            GetOpName = "17 Soudure Joints"
        Case 18
            GetOpName = "18 Soudure Abouts"
        Case 19
            GetOpName = "19 Soudure continue des profilés"
        Case 20
            GetOpName = "20 Soudure discontinue des profilés"
        Case 21
            GetOpName = "21 Voiles sur nappe DC"
        Case 22
            GetOpName = "22 Carlingues sur nappe DC"
        Case 23
            GetOpName = "23 Voiles sur carlingues DC"
        Case 24
            GetOpName = "24 Entailles DC"
        Case 25
            GetOpName = "25 Tapes non-étanches profilés DC"
        Case 26
            GetOpName = "26 Tapes étanches profilés DC"
        Case 27
            GetOpName = "27 Lisses pré-pré sur lisses DC"
        Case 28
            GetOpName = "28 Contacts verticaux DC"
        Case 29
            GetOpName = "29 Couples intérm sur DC"
        Case 30
            GetOpName = "30 Tapes non-étanches T synt DC"
        Case 31
            GetOpName = "31 Tapes étanches T synt DC"
        Case 32
            GetOpName = "32 Embarquement lisses ind"
        Case 33
            GetOpName = "33 Accostage tôles ind"
        Case 34
            GetOpName = "34 Soudage lisses ind sur tôles"
        Case 35
            GetOpName = "35 Accostage indices"
        Case 36
            GetOpName = "36 Voiles sur nappe bordé"
        Case 37
            GetOpName = "37 Carlingues sur nappe bordé"
        Case 38
            GetOpName = "38 Entailles borde"
        Case 39
            GetOpName = "39 Tapes non-étanches profilés bordé"
        Case 40
            GetOpName = "40 Tapes étanches profilés bordé"
        Case 41
            GetOpName = "41 Lisses pré-pré sur lisses bordé"
        Case 42
            GetOpName = "42 Contacts verticaux bordé"
        Case 43
            GetOpName = "43 Couples interm sur bordé"
        Case 44
            GetOpName = "44 Tapes non-étanches T synt bordé"
        Case 45
            GetOpName = "45 Tapes étanches T synt bordé"
        Case 46
            GetOpName = "46 Préparer , Mettre, Balancer"
        Case 47
            GetOpName = "47 Joints Panneaux"
        Case 48
            GetOpName = "48 Abouts Panneaux"
        Case 49
            GetOpName = "49 Abouts des lisses HP"
        Case 50
            GetOpName = "50 Abouts des lisses T synt"
        Case 51
            GetOpName = "51 Voiles sur carlingues (AX)"
        Case 52
            GetOpName = "52 DIVERS"
        Case 53
            GetOpName = "53 Barrots sur nappe"
        Case 54
            GetOpName = "54 Hiloires sur nappe"
        Case 55
            GetOpName = "55 Contacts barrots sur cloison"
        Case 56
            GetOpName = "56 About Barrot / hiloire"
        Case 57
            GetOpName = "57 Contact Barrot / hiloire"
        Case 58
            GetOpName = "58 Jonction épontille"
        Case 59
            GetOpName = "59 2-ieme jonction épontille"
        Case 60
            GetOpName = "60 fabrication T"
    End Select
End Function

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
            MSHFlexGridEdit MSH1, txtEdit, KeyAscii
    End Select
End Sub

Private Sub MSH1_DblClick()
    Select Case MSH1.CellBackColor
        Case &H8000000F
        Case Else
            MSHFlexGridEdit MSH1, txtEdit, 32
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
    txtEdit.Visible = False
End Sub

Private Sub TabStrip_BeforeClick(Cancel As Integer)
    If txtEdit.Visible = True Then
       Cancel = -1
    End If
End Sub

Private Sub TabStrip_Click()
    txtEdit.Visible = False
'    MSH1.Row = 1
'    MSH1.col = 1
    FlexGrid
    SetData
End Sub

Private Sub TxtEdit_KeyPress(KeyAscii As Integer)
    If KeyAscii = Asc(vbCr) Then KeyAscii = 0
End Sub

Private Sub TxtEdit_KeyDown(KeyCode As Integer, Shift As Integer)
    dBuffer = (MSH1)
    cmdOK.Default = False
    EditKeyCode MSH1, txtEdit, KeyCode, Shift
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
    If txtEdit.Visible = False Then Exit Sub
    MSH1 = txtEdit
    txtEdit.Visible = False
End Sub

Private Sub MSH1_LeaveCell()
    If txtEdit.Visible = False Then Exit Sub
    MSH1 = txtEdit
    txtEdit.Visible = False
End Sub

Private Sub TxtEdit_LostFocus()
    MSH1_GotFocus
    SetData
    cmdOK.Default = True
End Sub

Public Function ValidateID_Soudure1(ByVal sIndex As String) As Boolean
    On Error GoTo ValidateID_Soudure1Err
    'If IsNumeric(sIndex) = False Then ValidateID_Soudure1 = False: Exit Function
    Select Case Val(sIndex)
        Case 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 50, 51, 52
            ValidateID_Soudure1 = True
        Case Else
            ValidateID_Soudure1 = False
    End Select
    Exit Function
ValidateID_Soudure1Err:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modStrings: Function ValidateID_Soudure1")
End Function

Private Sub TxtEdit_Validate(Cancel As Boolean)
    Select Case TabStrip.SelectedItem.Key
        Case "IDSoudures"
             If ValidateID_Soudure1(txtEdit) = False Then
                MsgBox "ID de Soudure inconu.", vbCritical + vbOKOnly
                txtEdit = dBuffer
            End If
        Case Else
            ValidateNumeric txtEdit, Cancel
            If Cancel = True Then txtEdit = dBuffer
    End Select
    MSH1_GotFocus
End Sub
