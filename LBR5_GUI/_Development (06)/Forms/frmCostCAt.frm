VERSION 5.00
Object = "{5E9E78A0-531B-11CF-91F6-C2863C385E30}#1.0#0"; "MSFLXGRD.OCX"
Object = "{0D452EE1-E08F-101A-852E-02608C4D0BB4}#2.0#0"; "FM20.DLL"
Begin VB.Form frmCostCAt 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Assistant CostCAt"
   ClientHeight    =   6780
   ClientLeft      =   4335
   ClientTop       =   2250
   ClientWidth     =   7470
   Icon            =   "frmCostCAt.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   6780
   ScaleWidth      =   7470
   ShowInTaskbar   =   0   'False
   Begin VB.Frame frStep 
      Appearance      =   0  'Flat
      Caption         =   "Etape 8: ID Soudures d'asseblage et de montage"
      ForeColor       =   &H80000008&
      Height          =   5055
      Index           =   9
      Left            =   1200
      TabIndex        =   35
      Top             =   1080
      Width           =   6135
   End
   Begin VB.Frame frStep 
      Appearance      =   0  'Flat
      Caption         =   "Etape 7: Opérations de Pré-Montage - Montage"
      ForeColor       =   &H80000008&
      Height          =   5055
      Index           =   8
      Left            =   1080
      TabIndex        =   32
      Top             =   960
      Width           =   6135
   End
   Begin VB.Frame frStep 
      Appearance      =   0  'Flat
      Caption         =   "Etape 6: Opérations de Pré (assemblage)"
      ForeColor       =   &H80000008&
      Height          =   5055
      Index           =   7
      Left            =   960
      TabIndex        =   29
      Top             =   840
      Width           =   6135
   End
   Begin VB.Frame frStep 
      Appearance      =   0  'Flat
      Caption         =   "Etape 5: Opérations de Pré (fabrication de la nappe plane)"
      ForeColor       =   &H80000008&
      Height          =   5055
      Index           =   6
      Left            =   840
      TabIndex        =   28
      Top             =   720
      Width           =   6135
   End
   Begin VB.ComboBox Combo 
      Appearance      =   0  'Flat
      Height          =   315
      ItemData        =   "frmCostCAt.frx":000C
      Left            =   3600
      List            =   "frmCostCAt.frx":000E
      Style           =   2  'Dropdown List
      TabIndex        =   27
      Top             =   1680
      Visible         =   0   'False
      Width           =   975
   End
   Begin VB.TextBox TxtEdit 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   285
      Left            =   3600
      TabIndex        =   26
      Text            =   "Text1"
      Top             =   2160
      Visible         =   0   'False
      Width           =   975
   End
   Begin MSFlexGridLib.MSFlexGrid MSH1 
      Height          =   3855
      Left            =   2160
      TabIndex        =   9
      Top             =   1080
      Width           =   3375
      _ExtentX        =   5953
      _ExtentY        =   6800
      _Version        =   393216
      Cols            =   9
      GridColor       =   0
      WordWrap        =   -1  'True
      ScrollTrack     =   -1  'True
      GridLinesFixed  =   1
      AllowUserResizing=   1
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
   Begin VB.Frame frStep 
      Appearance      =   0  'Flat
      Caption         =   "Etape 4: Opérations de Pré-Pré (serres, carlingues, hiloires)"
      ForeColor       =   &H80000008&
      Height          =   5055
      Index           =   5
      Left            =   720
      TabIndex        =   25
      Top             =   600
      Width           =   6135
   End
   Begin VB.Frame frStep 
      Appearance      =   0  'Flat
      Caption         =   "Etape 4: Opérations de Pré-Pré (voiles, cadres)"
      ForeColor       =   &H80000008&
      Height          =   5055
      Index           =   4
      Left            =   600
      TabIndex        =   24
      Top             =   480
      Width           =   6135
   End
   Begin VB.Frame frStep 
      Appearance      =   0  'Flat
      Caption         =   "Etape 3: Définition Nappes Planes"
      ForeColor       =   &H80000008&
      Height          =   5055
      Index           =   3
      Left            =   480
      TabIndex        =   11
      Top             =   360
      Width           =   6135
      Begin MSForms.CommandButton cmdfr3AjouterNappe 
         Height          =   375
         Left            =   120
         TabIndex        =   21
         Top             =   4560
         Width           =   1455
         Caption         =   "Ajouter Nappe"
         PicturePosition =   327683
         Size            =   "2566;661"
         TakeFocusOnClick=   0   'False
         FontHeight      =   165
         FontCharSet     =   0
         FontPitchAndFamily=   2
         ParagraphAlign  =   3
      End
   End
   Begin VB.Frame frStep 
      Appearance      =   0  'Flat
      Caption         =   "Etape 2: Définition Types Panneaux"
      ForeColor       =   &H80000008&
      Height          =   5055
      Index           =   2
      Left            =   360
      TabIndex        =   22
      Top             =   240
      Width           =   6135
      Begin MSForms.CommandButton cmdfr3AjouterTypePanneau 
         Height          =   375
         Left            =   120
         TabIndex        =   23
         Top             =   4560
         Width           =   1455
         Caption         =   "Ajouter Panneau"
         PicturePosition =   327683
         Size            =   "2566;661"
         TakeFocusOnClick=   0   'False
         FontHeight      =   165
         FontCharSet     =   0
         FontPitchAndFamily=   2
         ParagraphAlign  =   3
      End
   End
   Begin VB.Frame frStep 
      Appearance      =   0  'Flat
      Caption         =   "Etape 1: Paramètres Globaux"
      ForeColor       =   &H80000008&
      Height          =   5055
      Index           =   1
      Left            =   240
      TabIndex        =   10
      Top             =   120
      Width           =   7095
      Begin VB.TextBox txtNoIndicesDC 
         Appearance      =   0  'Flat
         Height          =   285
         Left            =   4800
         TabIndex        =   4
         Top             =   3480
         Width           =   735
      End
      Begin VB.TextBox txtDIVsoud 
         Appearance      =   0  'Flat
         Height          =   285
         Left            =   4800
         TabIndex        =   3
         Top             =   3120
         Width           =   735
      End
      Begin VB.TextBox txtDIVtol 
         Appearance      =   0  'Flat
         Height          =   285
         Left            =   4800
         TabIndex        =   2
         Top             =   2760
         Width           =   735
      End
      Begin VB.TextBox txtPMB 
         Appearance      =   0  'Flat
         Height          =   285
         Left            =   4800
         TabIndex        =   1
         Top             =   1800
         Width           =   735
      End
      Begin VB.TextBox txtNAM 
         Appearance      =   0  'Flat
         Height          =   285
         Left            =   4800
         TabIndex        =   0
         Top             =   1440
         Width           =   735
      End
      Begin VB.Label Label11 
         AutoSize        =   -1  'True
         Caption         =   "[unités]"
         Height          =   195
         Left            =   5880
         TabIndex        =   31
         Top             =   3480
         Width           =   510
      End
      Begin VB.Label Label10 
         AutoSize        =   -1  'True
         Caption         =   "Nombre indices de double coque :"
         Height          =   195
         Left            =   2160
         TabIndex        =   30
         Top             =   3480
         Width           =   2430
      End
      Begin VB.Label Label9 
         AutoSize        =   -1  'True
         Caption         =   "[heures]"
         Height          =   195
         Left            =   5880
         TabIndex        =   20
         Top             =   2760
         Width           =   570
      End
      Begin VB.Label Label8 
         AutoSize        =   -1  'True
         Caption         =   "[heures]"
         Height          =   195
         Left            =   5880
         TabIndex        =   19
         Top             =   3120
         Width           =   570
      End
      Begin VB.Label Label7 
         AutoSize        =   -1  'True
         Caption         =   "[heures]"
         Height          =   195
         Left            =   5880
         TabIndex        =   18
         Top             =   1800
         Width           =   570
      End
      Begin VB.Label Label6 
         AutoSize        =   -1  'True
         Caption         =   "[unités]"
         Height          =   195
         Left            =   5880
         TabIndex        =   17
         Top             =   1440
         Width           =   510
      End
      Begin VB.Label Label5 
         AutoSize        =   -1  'True
         Caption         =   "Temps soudeur :"
         Height          =   195
         Left            =   3360
         TabIndex        =   16
         Top             =   3120
         Width           =   1185
      End
      Begin VB.Label Label4 
         AutoSize        =   -1  'True
         Caption         =   "Temps tôlier :"
         Height          =   195
         Left            =   3600
         TabIndex        =   15
         Top             =   2760
         Width           =   945
      End
      Begin VB.Label Label3 
         AutoSize        =   -1  'True
         Caption         =   "Opération 'DIVERS' :"
         Height          =   195
         Left            =   2640
         TabIndex        =   14
         Top             =   2400
         Width           =   1485
      End
      Begin VB.Label Label2 
         AutoSize        =   -1  'True
         Caption         =   "Coût total de PMB :"
         Height          =   195
         Left            =   3120
         TabIndex        =   13
         Top             =   1800
         Width           =   1380
      End
      Begin VB.Label Label1 
         AutoSize        =   -1  'True
         Caption         =   "Nombre abouts montage :"
         Height          =   195
         Left            =   2640
         TabIndex        =   12
         Top             =   1440
         Width           =   1830
      End
   End
   Begin MSForms.CommandButton cmdEnregistrer 
      Height          =   375
      Left            =   1080
      TabIndex        =   36
      Top             =   6240
      Width           =   1335
      Caption         =   "Appliquer"
      Size            =   "2355;661"
      FontHeight      =   165
      FontCharSet     =   0
      FontPitchAndFamily=   2
      ParagraphAlign  =   3
   End
   Begin VB.Label lblPage 
      AutoSize        =   -1  'True
      Caption         =   "page"
      ForeColor       =   &H8000000D&
      Height          =   195
      Left            =   240
      TabIndex        =   34
      Top             =   6000
      Width           =   360
   End
   Begin MSForms.CommandButton cmdFinish 
      Height          =   375
      Left            =   6240
      TabIndex        =   33
      Top             =   6240
      Visible         =   0   'False
      Width           =   1095
      Caption         =   "Terminer"
      PicturePosition =   327683
      Size            =   "1931;661"
      TakeFocusOnClick=   0   'False
      FontHeight      =   165
      FontCharSet     =   0
      FontPitchAndFamily=   2
      ParagraphAlign  =   3
   End
   Begin MSForms.CommandButton cmdCancel 
      Cancel          =   -1  'True
      Height          =   375
      Left            =   5040
      TabIndex        =   6
      Top             =   6240
      Width           =   1095
      Caption         =   "Annuler"
      PicturePosition =   327683
      Size            =   "1931;661"
      TakeFocusOnClick=   0   'False
      FontHeight      =   165
      FontCharSet     =   0
      FontPitchAndFamily=   2
      ParagraphAlign  =   3
   End
   Begin MSForms.CommandButton cmdNext 
      Height          =   375
      Left            =   3840
      TabIndex        =   5
      Top             =   6240
      Width           =   1095
      Caption         =   "Avant >"
      PicturePosition =   327683
      Size            =   "1931;661"
      TakeFocusOnClick=   0   'False
      FontHeight      =   165
      FontCharSet     =   0
      FontPitchAndFamily=   2
      ParagraphAlign  =   3
   End
   Begin MSForms.CommandButton cmdBack 
      Height          =   375
      Left            =   2760
      TabIndex        =   7
      Top             =   6240
      Width           =   1095
      Caption         =   "< Arriere"
      PicturePosition =   327683
      Size            =   "1931;661"
      TakeFocusOnClick=   0   'False
      FontHeight      =   165
      FontCharSet     =   0
      FontPitchAndFamily=   2
      ParagraphAlign  =   3
   End
   Begin MSForms.CommandButton cmdOK 
      Default         =   -1  'True
      Height          =   375
      Left            =   5040
      TabIndex        =   8
      Top             =   6240
      Visible         =   0   'False
      Width           =   1095
      Caption         =   "OK"
      PicturePosition =   327683
      Size            =   "1931;661"
      TakeFocusOnClick=   0   'False
      FontHeight      =   165
      FontCharSet     =   0
      FontPitchAndFamily=   2
      ParagraphAlign  =   3
   End
   Begin VB.Menu mnuAjouterPanneau 
      Caption         =   "AjouterPanneau"
      Begin VB.Menu mnuAjouterPanneauBordeSimple 
         Caption         =   "Bordé Simple"
      End
      Begin VB.Menu mnuAjouterPanneauBouchain 
         Caption         =   "Bouchain"
      End
      Begin VB.Menu mnuAjouterPanneauCarlingueSerreHiloire 
         Caption         =   "Carlingue, Serre, Hiloire"
      End
      Begin VB.Menu mnuAjouterPanneauDoubleCoque 
         Caption         =   "Double Coque"
      End
      Begin VB.Menu mnuAjouterPanneauFictif 
         Caption         =   "Fictif"
      End
   End
   Begin VB.Menu mnuAjouterSoudure 
      Caption         =   "Ajouter Soudure"
      Begin VB.Menu mnuAjouterSoudureID_1 
         Caption         =   "  1 Soudure à plat avec CUTO (nappe de double-fond et voiles)"
      End
      Begin VB.Menu mnuAjouterSoudureID_2 
         Caption         =   "  2 Soudure à plat sans CUTO (nappe de double-fond et carlingue)"
      End
      Begin VB.Menu mnuAjouterSoudureID_3 
         Caption         =   "  3 Soudure verticale avec CUTO"
      End
      Begin VB.Menu mnuAjouterSoudureID_4 
         Caption         =   "  4 Soudure verticale sans CUTO"
      End
      Begin VB.Menu mnuAjouterSoudureID_5 
         Caption         =   "  5 Soudure à plat, avec gueuse, avec CUTO"
      End
      Begin VB.Menu mnuAjouterSoudureID_6 
         Caption         =   "  6 Soudure à plat, avec gueuse, sans CUTO"
      End
      Begin VB.Menu mnuAjouterSoudureID_7 
         Caption         =   "  7 Soudure à plat, sans gueuse, avec CUTO"
      End
      Begin VB.Menu mnuAjouterSoudureID_8 
         Caption         =   "  8 Soudure à plat, sans gueuse, sans CUTO"
      End
      Begin VB.Menu mnuAjouterSoudureID_9 
         Caption         =   "  9 Soudure en tête, avec gueuse, avec CUTO"
      End
      Begin VB.Menu mnuAjouterSoudureID_10 
         Caption         =   "10 Soudure en tête, avec gueuse, sans CUTO"
      End
      Begin VB.Menu mnuAjouterSoudureID_11 
         Caption         =   "11 Soudure en tête, sans gueuse, avec CUTO"
      End
      Begin VB.Menu mnuAjouterSoudureID_12 
         Caption         =   "12 Soudure en tête, sans gueuse, sans CUTO"
      End
      Begin VB.Menu mnuAjouterSoudureID_13 
         Caption         =   "13 Soudure verticale avec peigne"
      End
      Begin VB.Menu mnuAjouterSoudureID_14 
         Caption         =   "14 Soudure de joint de tôle (manuelle) + Soudure des abouts de carlingues et serres; Position: à plat"
      End
      Begin VB.Menu mnuAjouterSoudureID_15 
         Caption         =   "15 Soudure de joint de tôle (manuelle) + Soudure des abouts de carlingues et serres; Position: horizontale"
      End
      Begin VB.Menu mnuAjouterSoudureID_16 
         Caption         =   "16 Soudure de joint de tôle (manuelle) + Soudure des abouts de carlingues et serres; Position: verticale"
      End
      Begin VB.Menu mnuAjouterSoudureID_50 
         Caption         =   "50 Soudure automatique: Picomax - BUGO"
      End
      Begin VB.Menu mnuAjouterSoudureID_51 
         Caption         =   "51 Soudure automatique: SEGARC"
      End
      Begin VB.Menu mnuAjouterSoudureID_52 
         Caption         =   "52 Soudure automatique: ARC SUBMERGE"
      End
   End
   Begin VB.Menu mnuViewFile 
      Caption         =   "View File"
      Begin VB.Menu dbinput 
         Caption         =   "View dbinput"
      End
      Begin VB.Menu dbfractionnement 
         Caption         =   "View dbfractionnement"
      End
      Begin VB.Menu dbacces 
         Caption         =   "View dbacces"
      End
   End
   Begin VB.Menu mnuAjouterCouple 
      Caption         =   "AjouterCouple"
      Visible         =   0   'False
      Begin VB.Menu mnuAjouterCoupleCarlingueSurNappe 
         Caption         =   "AjouterCoupleCarlingueSurNappe"
      End
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
Attribute VB_Name = "frmCostCAt"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Dim sBuffer As String
Dim irow As Integer, icol As Integer
Dim ProjectIndex As Integer
Dim frStepIndex As Integer
Dim cHeader As cHeader
Dim cCostCAtDHull As cCostCAtDHull
Dim cCostCAtNappe As cCostCAtNappe
Dim colPanel As Collection
Dim colGeometry As Collection
Dim colScantlings As Collection
Dim colCostCAtMain As Collection
Dim colCostCAtDHull As Collection
Dim colCostCAtNappe As Collection

Private Sub cmdBack_Click()
    Dim i As Integer
1:
    Combo_LostFocus
    TxtEdit_LostFocus
    For i = 1 To frStep.Count '- 1
        If frStep(i).Visible = True Then
            frStep(i).Visible = False
            frStep(i - 1).Visible = True
            cmdNext.Enabled = True
            If i - 1 = frStep.LBound Then cmdBack.Enabled = False
            cmdFinish.Visible = False
            Exit For
        End If
    Next i
    ActivateFrame i - 1
    If MSH1.Rows <= 1 Then GoTo 1
End Sub

Private Sub cmdBack_DblClick(Cancel As MSForms.ReturnBoolean)
    Cancel = True
End Sub

Private Sub cmdCancel_Click()
    Unload Me
End Sub

Private Sub cmdfr2Ajouter_DblClick(Cancel As MSForms.ReturnBoolean)
    Cancel = True
End Sub

Private Sub cmdEnregistrer_Click()
    Dim i As Integer
    Set Project.Item(ProjectIndex).cHeader = cHeader
    Dim cPanel As cPanel
    Dim cP As cPanel, colP As New colPanel
    
    For i = 1 To colPanel.Count
        Set cP = colPanel.Item(i)
        colP.Add cP, i
        Set cP = Nothing
    Next i
    Set Project.Item(ProjectIndex).colPanel = colP
    Set colP = Nothing
    
    Dim cDH As cCostCAtDHull, colDH As New colCostCAtDHull
    For i = 1 To colCostCAtDHull.Count
        Set cDH = colCostCAtDHull.Item(i)
        colDH.Add cDH, i
        Set cDH = Nothing
    Next i
    Set Project.Item(ProjectIndex).colCostCAtDHull = colDH
    Set colDH = Nothing
    
    Dim cN As cCostCAtNappe, colN As New colCostCAtNappe
    For i = 1 To colCostCAtNappe.Count
        Set cN = colCostCAtNappe.Item(i)
        colN.Add cN, i
        Set cN = Nothing
    Next i
    Set Project.Item(ProjectIndex).colCostCAtNappe = colN
    Set colN = Nothing
    
    Project.Item(ProjectIndex).DataChanged = True

End Sub

Private Sub cmdFinish_Click()
    Dim i As Integer
    Set Project.Item(ProjectIndex).cHeader = cHeader
    Dim cPanel As cPanel
    Dim cP As cPanel, colP As New colPanel
    
    For i = 1 To colPanel.Count
        Set cP = colPanel.Item(i)
        colP.Add cP, i
        Set cP = Nothing
    Next i
    Set Project.Item(ProjectIndex).colPanel = colP
    Set colP = Nothing
    
    Dim cDH As cCostCAtDHull, colDH As New colCostCAtDHull
    For i = 1 To colCostCAtDHull.Count
        Set cDH = colCostCAtDHull.Item(i)
        colDH.Add cDH, i
        Set cDH = Nothing
    Next i
    Set Project.Item(ProjectIndex).colCostCAtDHull = colDH
    Set colDH = Nothing
    
    Dim cN As cCostCAtNappe, colN As New colCostCAtNappe
    For i = 1 To colCostCAtNappe.Count
        Set cN = colCostCAtNappe.Item(i)
        colN.Add cN, i
        Set cN = Nothing
    Next i
    Set Project.Item(ProjectIndex).colCostCAtNappe = colN
    Set colN = Nothing
    
    Project.Item(ProjectIndex).DataChanged = True
    
    Unload Me
    Project.Item(ProjectIndex).frmCostCAtTables.Show
End Sub

Private Sub cmdFinish_DblClick(Cancel As MSForms.ReturnBoolean)
    Cancel = True
End Sub

Private Sub cmdfr3AjouterNappe_Click()
    Me.Hide
    setFunctionMode ADD_NAPPE
End Sub

Private Sub cmdfr3AjouterNappe_DblClick(Cancel As MSForms.ReturnBoolean)
    Cancel = True
End Sub

Private Sub cmdfr3AjouterTypePanneau_Click()
    PopupMenu mnuAjouterPanneau
End Sub

Private Sub cmdfr3AjouterTypePanneau_DblClick(Cancel As MSForms.ReturnBoolean)
    Cancel = True
End Sub

Private Sub cmdNext_Click()
    On Error GoTo 2
    Dim i As Integer
1:
    Combo_LostFocus
    TxtEdit_LostFocus
    For i = 1 To frStep.Count '- 1
        If frStep(i).Visible = True Then
            frStep(i).Visible = False
            frStep(i + 1).Visible = True
            cmdBack.Enabled = True
            If i + 1 = frStep.UBound Then cmdNext.Enabled = False
            Exit For
        End If
    Next i
    ActivateFrame i + 1
    If MSH1.Rows <= 1 Then GoTo 1
    If (i + 1) = frStep.Count Then
        cmdFinish.Visible = True
    Else
        cmdFinish.Visible = False
    End If
2:
End Sub

Private Sub cmdNext_DblClick(Cancel As MSForms.ReturnBoolean)
    Cancel = True
End Sub

Private Sub cmdOK_Click()
    Unload Me
End Sub

Private Sub Combo_GotFocus()
    sBuffer = MSH1
    irow = MSH1.Row
    icol = MSH1.col
End Sub

Private Sub Combo_KeyDown(KeyCode As Integer, Shift As Integer)
    EditKeyCode MSH1, Combo, KeyCode, Shift
End Sub

Private Sub Combo_KeyPress(KeyAscii As Integer)
    If KeyAscii = Asc(vbCr) Then KeyAscii = 0
End Sub

Private Sub Combo_LostFocus()
    MSH1_GotFocus
    SetData
End Sub

Private Sub dbacces_Click()
    'ViewDBAcces
End Sub

Private Sub dbfractionnement_Click()
    'reset matrix
    Dim i As Integer
    Dim cPanel As cPanel
    For Each cPanel In colPanel
        For i = 1 To NO_OPERATIONS
            cPanel.cCostCAtMain.colCostCAtOperations.Item(i).Fractionnement = 0
        Next i
    Next cPanel
    
    ComputePrePre
    ComputeNappePlane
    ComputePreAssemblage
    ComputeMontage
    'ViewDBFractionnement
End Sub

Private Sub dbinput_Click()
    'ViewDBInput
End Sub

Private Sub Form_Load()
    ProjectIndex = ActiveProject
    Form_Resize
    Me.Move (Screen.Width - Me.Width) / 2, (Screen.Height - Me.Height) / 2
    cmdOK.Picture = LoadResPicture("ID_BITMAP_OK", 0)
    cmdCancel.Picture = LoadResPicture("ID_BITMAP_CANCEL", 0)
    Me.Caption = "Assistant CostCAt - [" & GetFileName(Project.Item(ProjectIndex).sFileName) & "]"
    mnuAjouterPanneau.Visible = False
    mnuViewFile.Visible = False
    mnuAjouterSoudure.Visible = False
    mnuEdit.Visible = False
    cmdBack.Enabled = False
    SetFramesInvisible
    frStepIndex = 1
    frStep(1).Visible = True
    GetData
    ActivateFrame frStepIndex
End Sub

Private Sub GetData()
    On Error GoTo GetDataErr
    Dim cPanel As cPanel
    Dim cCostCAtDHull As cCostCAtDHull
    Dim cCostCAtNappe As cCostCAtNappe
    Set cHeader = Project.Item(ProjectIndex).cHeader.Clone
    Set colPanel = New Collection
    Set colCostCAtDHull = New Collection
    Set colCostCAtNappe = New Collection
    For Each cPanel In Project.Item(ProjectIndex).colPanel
        colPanel.Add cPanel.Clone
    Next cPanel
    For Each cCostCAtDHull In Project.Item(ProjectIndex).colCostCAtDHull
        colCostCAtDHull.Add cCostCAtDHull.Clone
    Next cCostCAtDHull
    For Each cCostCAtNappe In Project.Item(ProjectIndex).colCostCAtNappe
        colCostCAtNappe.Add cCostCAtNappe.Clone
    Next cCostCAtNappe
    Exit Sub
GetDataErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmCostCAt: Sub GetData")
End Sub

Public Sub ComputeFractionnements()
    Dim i As Integer
    Dim cPanel As cPanel
    For Each cPanel In colPanel
        For i = 1 To NO_OPERATIONS
            cPanel.cCostCAtMain.colCostCAtOperations.Item(i).Fractionnement = 0
        Next i
    Next cPanel
    ComputePrePre
    ComputeNappePlane
    ComputePreAssemblage
    ComputeMontage
End Sub

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
    Set cHeader = Nothing
    Set colPanel = Nothing
    Set colCostCAtDHull = Nothing
    Set colCostCAtNappe = Nothing
End Sub

Private Sub mnuAjouterSoudureID_1_Click()
    MSH1 = 1
End Sub

Private Sub mnuAjouterSoudureID_10_Click()
    MSH1 = 10
End Sub

Private Sub mnuAjouterSoudureID_11_Click()
    MSH1 = 11
End Sub

Private Sub mnuAjouterSoudureID_12_Click()
    MSH1 = 12
End Sub

Private Sub mnuAjouterSoudureID_13_Click()
    MSH1 = 13
End Sub

Private Sub mnuAjouterSoudureID_14_Click()
    MSH1 = 14
End Sub

Private Sub mnuAjouterSoudureID_15_Click()
    MSH1 = 15
End Sub

Private Sub mnuAjouterSoudureID_16_Click()
    MSH1 = 16
End Sub

Private Sub mnuAjouterSoudureID_2_Click()
    MSH1 = 2
End Sub

Private Sub mnuAjouterSoudureID_3_Click()
    MSH1 = 3
End Sub

Private Sub mnuAjouterSoudureID_4_Click()
    MSH1 = 4
End Sub

Private Sub mnuAjouterSoudureID_5_Click()
    MSH1 = 5
End Sub

Private Sub mnuAjouterSoudureID_50_Click()
    MSH1 = 50
End Sub

Private Sub mnuAjouterSoudureID_51_Click()
    MSH1 = 51
End Sub

Private Sub mnuAjouterSoudureID_52_Click()
    MSH1 = 52
End Sub

Private Sub mnuAjouterSoudureID_6_Click()
    MSH1 = 6
End Sub

Private Sub mnuAjouterSoudureID_7_Click()
    MSH1 = 7
End Sub

Private Sub mnuAjouterSoudureID_8_Click()
    MSH1 = 8
End Sub

Private Sub mnuAjouterSoudureID_9_Click()
    MSH1 = 9
End Sub

Private Sub mnuEditCopy_Click()
    FlexCopy MSH1
End Sub

Private Sub mnuEditPaste_Click()
    FlexPaste MSH1
    SetData
End Sub

Private Sub MSH1_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
    If Button = 2 Then
        PopupMenu mnuEdit
    End If
End Sub

Private Sub txtDIVsoud_GotFocus()
    txtDIVsoud.SelStart = 0
    txtDIVsoud.SelLength = Len(txtDIVsoud.Text)
End Sub

Private Sub txtDIVtol_GotFocus()
    txtDIVtol.SelStart = 0
    txtDIVtol.SelLength = Len(txtDIVtol.Text)
End Sub

Public Function OpenFormAddDHull(ByVal sInner As String, ByVal iInner As Integer, ByVal sOuter As String, ByVal iOuter As Integer)
    On Error GoTo OpenFormAddDHullErr
    If iInner = 0 And iOuter = 0 Then
        Me.Show
        Exit Function
    End If
    Dim v_inner() As Variant, v_outer() As Variant
    Dim i As Integer
    Dim SI As String, sO As String
    SI = sInner: sO = sOuter
    GetValues iInner, SI, v_inner
    GetValues iOuter, sO, v_outer
    If iInner > 1 Then
        v_inner = SortAscending(v_inner)
    End If
    If iOuter > 1 Then
        v_outer = SortAscending(v_outer)
    End If
    For i = 1 To iInner
        colPanel.Item(Val(v_inner(i))).cCostCAtMain.ID_PANNEAU = NappePlane
        colPanel.Item(Val(v_inner(i))).cCostCAtMain.IT_PANNEAU = DoubleCoqueInterieure
        colPanel.Item(Val(v_inner(i))).cCostCAtMain.PositionAboutsLisses = SoudureVerticale
    Next i
    For i = 1 To iOuter
        colPanel.Item(Val(v_outer(i))).cCostCAtMain.ID_PANNEAU = NappePlane
        colPanel.Item(Val(v_outer(i))).cCostCAtMain.IT_PANNEAU = DoubleCoqueExterieure
        colPanel.Item(Val(v_outer(i))).cCostCAtMain.PositionAboutsLisses = SoudureVerticale
    Next i
    
    Dim cCostCAtDHull As cCostCAtDHull
    Set cCostCAtDHull = New cCostCAtDHull
    Dim cIndex As cIndex
    
    With cCostCAtDHull
        .index = colCostCAtDHull.Count + 1
        For i = 1 To iInner
            Set cIndex = New cIndex
            cIndex.index = i
            cIndex.Number = v_inner(i)
            cCostCAtDHull.InnerShell.Add cIndex, i
            Set cIndex = Nothing
        Next i
        For i = 1 To iOuter
            Set cIndex = New cIndex
            cIndex.index = i
            cIndex.Number = v_outer(i)
            cCostCAtDHull.OuterShell.Add cIndex, i
            Set cIndex = Nothing
        Next i
    End With
    Dim j As Integer, k As Integer
    
    
    For i = 1 To cCostCAtDHull.InnerShell.Count
        For j = 1 To colCostCAtDHull.Count
1:
            For k = 1 To colCostCAtDHull.Item(j).InnerShell.Count
                If cCostCAtDHull.InnerShell.Item(i).Number = colCostCAtDHull.Item(j).InnerShell.Item(k).Number Then
                    colCostCAtDHull.Item(j).InnerShell.Remove k: colCostCAtDHull.Item(j).InnerShell.Renum
                    colCostCAtDHull.Item(j).SetDefaultDHData colPanel
                    GoTo 1
                End If
            Next k
2:
            For k = 1 To colCostCAtDHull.Item(j).OuterShell.Count
                If cCostCAtDHull.InnerShell.Item(i).Number = colCostCAtDHull.Item(j).OuterShell.Item(k).Number Then
                    colCostCAtDHull.Item(j).OuterShell.Remove k: colCostCAtDHull.Item(j).OuterShell.Renum
                    colCostCAtDHull.Item(j).SetDefaultDHData colPanel
                    GoTo 2
                End If
            Next k
        Next j
    Next i
    For i = 1 To cCostCAtDHull.OuterShell.Count
        For j = 1 To colCostCAtDHull.Count
3:
            For k = 1 To colCostCAtDHull.Item(j).InnerShell.Count
                If cCostCAtDHull.OuterShell.Item(i).Number = colCostCAtDHull.Item(j).InnerShell.Item(k).Number Then
                    colCostCAtDHull.Item(j).InnerShell.Remove k: colCostCAtDHull.Item(j).InnerShell.Renum
                    colCostCAtDHull.Item(j).SetDefaultDHData colPanel
                    GoTo 3
                End If
            Next k
4:
            For k = 1 To colCostCAtDHull.Item(j).OuterShell.Count
                If cCostCAtDHull.OuterShell.Item(i).Number = colCostCAtDHull.Item(j).OuterShell.Item(k).Number Then
                    colCostCAtDHull.Item(j).OuterShell.Remove k: colCostCAtDHull.Item(j).OuterShell.Renum
                    colCostCAtDHull.Item(j).SetDefaultDHData colPanel
                    GoTo 4
                End If
            Next k
        Next j
    Next i
    Dim cIndex1 As cIndex
    Dim cDH As cCostCAtDHull
5:
    For Each cDH In colCostCAtDHull
        If cDH.InnerShell.Count = 0 And cDH.OuterShell.Count = 0 Then
            colCostCAtDHull.Remove cDH.index: colDHRenum     '  : colCostCAtDHull.Renum
            GoTo 5
        End If
    Next cDH
    
    cCostCAtDHull.index = colCostCAtDHull.Count + 1
    cCostCAtDHull.SetDefaultDHData colPanel
    'setDHullID_Soudure cCostCAtDHull
    'cCostCAtDHull.setDHullID_Soudure colPanel
    colCostCAtDHull.Add cCostCAtDHull
    Set cCostCAtDHull = Nothing
    MSH1_Step2
    Me.Show
    Exit Function
OpenFormAddDHullErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmCostCAt: Function OpenFormAddDHull")
End Function


Public Function OpenFormAddNappe(ByVal sNappe As String, iNappe As Integer)
    On Error GoTo OpenFormAddNappeErr
    If iNappe = 0 Then
        Me.Show
        Exit Function
    End If
    Dim v_nappe() As Variant
    Dim i As Integer, j As Integer, k As Integer
    Dim ss As String
    ss = sNappe

    GetValues iNappe, ss, v_nappe
    If iNappe > 1 Then
        v_nappe = SortAscending(v_nappe)
    ElseIf iNappe = 1 Then
        v_nappe(1) = Val(v_nappe(1))
    End If
    
    Dim cCostCAtNappe As cCostCAtNappe
    Set cCostCAtNappe = New cCostCAtNappe
    Dim cIndex As cIndex
    With cCostCAtNappe
        .index = colCostCAtNappe.Count + 1
        For i = 1 To iNappe
            Set cIndex = New cIndex
            cIndex.index = i
            cIndex.Number = v_nappe(i)
            colPanel.Item(cIndex.Number).cCostCAtMain.ID_PANNEAU = NappePlane
            colPanel.Item(cIndex.Number).cCostCAtMain.IP_PANNEAU = ExterieurNappe
            colPanel.Item(cIndex.Number).cCostCAtMain.bIsPartOfNappe = True
            cCostCAtNappe.Nappe.Add cIndex, i
            Set cIndex = Nothing
        Next i
    End With
    
    For i = 1 To iNappe
        For j = 1 To colCostCAtNappe.Count
1:
            For k = 1 To colCostCAtNappe.Item(j).Nappe.Count
                If Val(v_nappe(i)) = colCostCAtNappe.Item(j).Nappe.Item(k).Number Then
                    colCostCAtNappe.Item(j).Nappe.Remove k: colCostCAtNappe.Item(j).Nappe.Renum
                    GoTo 1
                End If
            Next k
        Next j
    Next i

    Dim cN As cCostCAtNappe
2:
    For Each cN In colCostCAtNappe
        If cN.Nappe.Count = 0 Then
            colCostCAtNappe.Remove cN.index: colNRenum
            GoTo 2
        End If
    Next cN
    
    cCostCAtNappe.index = colCostCAtNappe.Count + 1
    cCostCAtNappe.AccostagesNappes = 0
    cCostCAtNappe.iNANP = 0
    cCostCAtNappe.SoudureLissesNappes = SoudureContinue
    colCostCAtNappe.Add cCostCAtNappe
    Set cCostCAtNappe = Nothing
3:
    For Each cN In colCostCAtNappe
        If cN.Nappe.Count = 1 Then
            colPanel.Item(cN.Nappe.Item(1).Number).cCostCAtMain.ID_PANNEAU = NappePlane
            colPanel.Item(cN.Nappe.Item(1).Number).cCostCAtMain.bIsPartOfNappe = False
            colCostCAtNappe.Remove cN.index: colNRenum
            GoTo 3
        End If
    Next cN
    
    'IP Nappes
    If iNappe > 2 Then
        SetIPNappe v_nappe
    End If

    For Each cN In colCostCAtNappe
        resetNappeInterieureID_Soudure cN
    Next cN
    
    MSH1_Step3
    Me.Show

    Exit Function
OpenFormAddNappeErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmCostCAt: Function OpenFormAddNappe")
End Function

Public Function SetIPNappe(ByRef v As Variant)
    On Error GoTo SetIPNappeErr
    'Project.Item(1).colPanel.Item(1).cCostCAtMain.IP_PANNEAU = ExterieurNappe:=InterieurNappe
    Dim i As Integer, j As Integer
    Dim v_nodes() As Integer
    i = 0
    Dim cN As cCostCAtNappe
    Dim cIndex As cIndex
    Dim CI As cIndex
    Dim colIndex As colIndex

    For Each cN In colCostCAtNappe

        If cN.Nappe.Count > 2 Then
        Set colIndex = New colIndex
            For Each cIndex In cN.Nappe
                Set CI = New cIndex
                CI.Number = Project.Item(ProjectIndex).colPanel.Item(cIndex.Number).cGeometry.InNode
                i = i + 1
                colIndex.Add CI, i
                Set CI = Nothing
                Set CI = New cIndex
                CI.Number = Project.Item(ProjectIndex).colPanel.Item(cIndex.Number).cGeometry.OutNode
                i = i + 1
                colIndex.Add CI, i
                Set CI = Nothing
            Next cIndex

            i = 0
            j = 0
            'ReDim v_nodes(1 To 1)
            For Each cIndex In colIndex
                For Each CI In colIndex
                    If CI.Number = cIndex.Number Then
                        i = i + 1
                    End If
                Next CI
                If i = 2 Then
                    j = j + 1
                    ReDim Preserve v_nodes(1 To j) 'As Integer
                    v_nodes(j) = cIndex.Number
                End If
                i = 0
            Next cIndex
            Set colIndex = Nothing
            Dim indiceIn As Boolean, indiceOut As Boolean
            
            For Each cIndex In cN.Nappe
                indiceIn = False: indiceOut = False
                For i = 1 To UBound(v_nodes)
                    If Project.Item(ProjectIndex).colPanel.Item(cIndex.Number).cGeometry.InNode = v_nodes(i) Then
                        indiceIn = True
                    End If
                    If Project.Item(ProjectIndex).colPanel.Item(cIndex.Number).cGeometry.OutNode = v_nodes(i) Then
                        indiceOut = True
                    End If
                Next i
                If indiceIn = True And indiceOut = True Then
                    'MsgBox "panel " & cIndex.Number & " is interiour to the nappe."
                    colPanel.Item(cIndex.Number).cCostCAtMain.IP_PANNEAU = InterieurNappe
                End If
            Next cIndex
        End If
    Next cN

    Exit Function
SetIPNappeErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmCostCAt: Function SetIPNappe")
End Function

Public Function OpenFormAddVirtual(ByVal sVirtual As String, iVirtual As Integer)
    On Error GoTo OpenFormAddVirtualErr
    If iVirtual = 0 Then
        Me.Show
        Exit Function
    End If
    Dim v_shell() As Variant
    Dim i As Integer, j As Integer, k As Integer
    Dim ss As String
    ss = sVirtual
    GetValues iVirtual, ss, v_shell
    Dim cPanel As cPanel
   
    For i = 1 To iVirtual
        colPanel.Item(Val(v_shell(i))).cCostCAtMain.ID_PANNEAU = Virtual
        colPanel.Item(Val(v_shell(i))).cCostCAtMain.IT_PANNEAU = IT_None
        colPanel.Item(Val(v_shell(i))).cCostCAtMain.PositionAboutsLisses = SoudureVerticale
        colPanel.Item(Val(v_shell(i))).cCostCAtMain.SetDefaultPanelData _
        (colPanel.Item(Val(v_shell(i))))
        'setVirtualID_Soudure Val(v_shell(i))
    Next i
    For i = 1 To iVirtual
        For j = 1 To colCostCAtDHull.Count
1:
            For k = 1 To colCostCAtDHull.Item(j).InnerShell.Count
                If Val(v_shell(i)) = colCostCAtDHull.Item(j).InnerShell.Item(k).Number Then
                    colCostCAtDHull.Item(j).InnerShell.Remove k: colCostCAtDHull.Item(j).InnerShell.Renum
                    GoTo 1
                End If
            Next k
2:
            For k = 1 To colCostCAtDHull.Item(j).OuterShell.Count
                If Val(v_shell(i)) = colCostCAtDHull.Item(j).OuterShell.Item(k).Number Then
                    colCostCAtDHull.Item(j).OuterShell.Remove k: colCostCAtDHull.Item(j).OuterShell.Renum
                    GoTo 2
                End If
            Next k
        Next j
    Next i
    
    Dim cDH As cCostCAtDHull
3:
    For Each cDH In colCostCAtDHull
        If cDH.InnerShell.Count = 0 And cDH.OuterShell.Count = 0 Then
            colCostCAtDHull.Remove cDH.index: colDHRenum
            GoTo 3
        End If
    Next cDH
    
    MSH1_Step2
    Me.Show

    Exit Function
OpenFormAddVirtualErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmCostCAt: Function OpenFormAddVirtual")
End Function

Public Function OpenFormAddShell(ByVal sShell As String, iShell As Integer)
    On Error GoTo OpenFormAddShellErr
    If iShell = 0 Then
        Me.Show
        Exit Function
    End If
    Dim v_shell() As Variant
    Dim i As Integer, j As Integer, k As Integer
    Dim ss As String
    ss = sShell
    GetValues iShell, ss, v_shell
    For i = 1 To iShell
        colPanel.Item(Val(v_shell(i))).cCostCAtMain.ID_PANNEAU = NappePlane
        colPanel.Item(Val(v_shell(i))).cCostCAtMain.IT_PANNEAU = BordeSimple
        colPanel.Item(Val(v_shell(i))).cCostCAtMain.PositionAboutsLisses = SoudureVerticale
        colPanel.Item(Val(v_shell(i))).cCostCAtMain.SetDefaultPanelData _
        (colPanel.Item(Val(v_shell(i)))), ProjectIndex
        'setShellID_Soudure Val(v_shell(i))
    Next i
    For i = 1 To iShell
        For j = 1 To colCostCAtDHull.Count
1:
            For k = 1 To colCostCAtDHull.Item(j).InnerShell.Count
                If Val(v_shell(i)) = colCostCAtDHull.Item(j).InnerShell.Item(k).Number Then
                    colCostCAtDHull.Item(j).InnerShell.Remove k: colCostCAtDHull.Item(j).InnerShell.Renum
                    GoTo 1
                End If
            Next k
2:
            For k = 1 To colCostCAtDHull.Item(j).OuterShell.Count
                If Val(v_shell(i)) = colCostCAtDHull.Item(j).OuterShell.Item(k).Number Then
                    colCostCAtDHull.Item(j).OuterShell.Remove k: colCostCAtDHull.Item(j).OuterShell.Renum
                    GoTo 2
                End If
            Next k
        Next j
    Next i
    
    Dim cDH As cCostCAtDHull
3:
    For Each cDH In colCostCAtDHull
        If cDH.InnerShell.Count = 0 And cDH.OuterShell.Count = 0 Then
            colCostCAtDHull.Remove cDH.index: colDHRenum
            GoTo 3
        End If
    Next cDH
    
    MSH1_Step2
    Me.Show

    Exit Function
OpenFormAddShellErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmCostCAt: Function OpenFormAddShell")
End Function

Public Function OpenFormAddBilge(ByVal sBilge As String, iBilge As Integer)
    On Error GoTo OpenFormAddBilgeErr
    If iBilge = 0 Then
        Me.Show
        Exit Function
    End If
    Dim v_bilge() As Variant
    Dim i As Integer, j As Integer, k As Integer
    Dim sB As String
    sB = sBilge
    GetValues iBilge, sB, v_bilge
    For i = 1 To iBilge
        colPanel.Item(Val(v_bilge(i))).cCostCAtMain.ID_PANNEAU = Bouchain
        colPanel.Item(Val(v_bilge(i))).cCostCAtMain.IT_PANNEAU = IT_None
        colPanel.Item(Val(v_bilge(i))).cCostCAtMain.IP_PANNEAU = IP_None
        'colPanel.Item(Val(v_bilge(i))).cCostCAtMain.PositionAboutsLisses = SoudureVerticale
        colPanel.Item(Val(v_bilge(i))).cCostCAtMain.SetDefaultPanelData (colPanel.Item(Val(v_bilge(i))))
        'setBilgeID_Soudure Val(v_bilge(i))
    Next i
    For i = 1 To iBilge
        For j = 1 To colCostCAtDHull.Count
1:
            For k = 1 To colCostCAtDHull.Item(j).InnerShell.Count
                If Val(v_bilge(i)) = colCostCAtDHull.Item(j).InnerShell.Item(k).Number Then
                    colCostCAtDHull.Item(j).InnerShell.Remove k: colCostCAtDHull.Item(j).InnerShell.Renum
                    GoTo 1
                End If
            Next k
2:
            For k = 1 To colCostCAtDHull.Item(j).OuterShell.Count
                If Val(v_bilge(i)) = colCostCAtDHull.Item(j).OuterShell.Item(k).Number Then
                    colCostCAtDHull.Item(j).OuterShell.Remove k: colCostCAtDHull.Item(j).OuterShell.Renum
                    GoTo 2
                End If
            Next k
        Next j
    Next i
    
    Dim cDH As cCostCAtDHull
3:
    For Each cDH In colCostCAtDHull
        If cDH.InnerShell.Count = 0 And cDH.OuterShell.Count = 0 Then
            colCostCAtDHull.Remove cDH.index: colDHRenum
            GoTo 3
        End If
    Next cDH
    
    MSH1_Step2
    Me.Show

    Exit Function
OpenFormAddBilgeErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmCostCAt: Function OpenFormAddBilge")
End Function

Public Function OpenFormAddGirder(ByVal sGirder As String, iGirder As Integer)
    On Error GoTo OpenFormAddGirderErr
    If iGirder = 0 Then
        Me.Show
        Exit Function
    End If
    Dim v_girder() As Variant
    Dim i As Integer, j As Integer, k As Integer
    Dim sG As String
    sG = sGirder
    GetValues iGirder, sG, v_girder
    For i = 1 To iGirder
        colPanel.Item(Val(v_girder(i))).cCostCAtMain.ID_PANNEAU = CarlingueSerreHiloire
        colPanel.Item(Val(v_girder(i))).cCostCAtMain.IT_PANNEAU = IT_None
        colPanel.Item(Val(v_girder(i))).cCostCAtMain.IP_PANNEAU = IP_None
        'colPanel.Item(Val(v_girder(i))).cCostCAtMain.PositionAboutsLisses = PA_None
        colPanel.Item(Val(v_girder(i))).cCostCAtMain.SetDefaultPanelData (colPanel.Item(Val(v_girder(i)))), ProjectIndex
        'setGirderID_Soudure Val(v_girder(i))
    Next i
    For i = 1 To iGirder
        For j = 1 To colCostCAtDHull.Count
1:
            For k = 1 To colCostCAtDHull.Item(j).InnerShell.Count
                If Val(v_girder(i)) = colCostCAtDHull.Item(j).InnerShell.Item(k).Number Then
                    colCostCAtDHull.Item(j).InnerShell.Remove k: colCostCAtDHull.Item(j).InnerShell.Renum
                    GoTo 1
                End If
            Next k
2:
            For k = 1 To colCostCAtDHull.Item(j).OuterShell.Count
                If Val(v_girder(i)) = colCostCAtDHull.Item(j).OuterShell.Item(k).Number Then
                    colCostCAtDHull.Item(j).OuterShell.Remove k: colCostCAtDHull.Item(j).OuterShell.Renum
                    GoTo 2
                End If
            Next k
        Next j
    Next i
    
    Dim cDH As cCostCAtDHull
3:
    For Each cDH In colCostCAtDHull
        If cDH.InnerShell.Count = 0 And cDH.OuterShell.Count = 0 Then
            colCostCAtDHull.Remove cDH.index: colDHRenum
            GoTo 3
        End If
    Next cDH
    
    MSH1_Step2
    Me.Show
    Exit Function
OpenFormAddGirderErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmCostCAt: Function OpenFormAddGirder")
End Function

'Public Function OpenFormAddCoupleGirderOnNappe(ByVal sPanel As String, iPanel As Integer)
'    On Error GoTo  OpenFormAddCoupleGirderOnNappeErr
'
'    Exit Function
'OpenFormAddCoupleGirderOnNappeErr:
'    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmCostCAt: Function OpenFormAddCoupleGirderOnNappe")
'End Function

'Private Function setDHullID_Soudure(ByRef cDH As cCostCAtDHull)
'    On Error GoTo  setDHullID_SoudureErr
'    Dim cIndex As cIndex, cPanel As cPanel
'    'Inner
'    For Each cIndex In cDH.InnerShell
'        Set cPanel = colPanel.Item(cIndex.Number)
'        cPanel.cCostCAtMain.colCostCAtOperations.Item(21).Soudures = 1
'        cPanel.cCostCAtMain.colCostCAtOperations.Item(22).Soudures = 0
'        cPanel.cCostCAtMain.colCostCAtOperations.Item(23).Soudures = 0
'        cPanel.cCostCAtMain.colCostCAtOperations.Item(36).Soudures = 0
'        cPanel.cCostCAtMain.colCostCAtOperations.Item(37).Soudures = 0
'        cPanel.cCostCAtMain.colCostCAtOperations.Item(47).Soudures = 52 'après avoir defini les nappes, il faut revoir
'        cPanel.cCostCAtMain.colCostCAtOperations.Item(48).Soudures = 52
'        cPanel.cCostCAtMain.colCostCAtOperations.Item(51).Soudures = 0
'        Set cPanel = Nothing
'    Next cIndex
'    'Outer
'    For Each cIndex In cDH.OuterShell
'        Set cPanel = colPanel.Item(cIndex.Number)
'        cPanel.cCostCAtMain.colCostCAtOperations.Item(21).Soudures = 0
'        cPanel.cCostCAtMain.colCostCAtOperations.Item(22).Soudures = 0
'        cPanel.cCostCAtMain.colCostCAtOperations.Item(23).Soudures = 0
'        cPanel.cCostCAtMain.colCostCAtOperations.Item(36).Soudures = 5
'        cPanel.cCostCAtMain.colCostCAtOperations.Item(37).Soudures = 0
'        cPanel.cCostCAtMain.colCostCAtOperations.Item(47).Soudures = 50 'après avoir defini les nappes, il faut revoir
'        cPanel.cCostCAtMain.colCostCAtOperations.Item(48).Soudures = 50
'        cPanel.cCostCAtMain.colCostCAtOperations.Item(51).Soudures = 0
'        Set cPanel = Nothing
'    Next cIndex
'    Exit Function
'setDHullID_SoudureErr:
'    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmCostCAt: Function setDHullID_Soudure")
'End Function

'Private Function setBilgeID_Soudure(ByVal PanelIndex As Integer)
'    On Error GoTo  setBilgeID_SoudureErr
'    Dim cPanel As cPanel
'    Set cPanel = colPanel.Item(PanelIndex)
'    cPanel.cCostCAtMain.colCostCAtOperations.Item(21).Soudures = 0
'    cPanel.cCostCAtMain.colCostCAtOperations.Item(22).Soudures = 0
'    cPanel.cCostCAtMain.colCostCAtOperations.Item(23).Soudures = 0
'    cPanel.cCostCAtMain.colCostCAtOperations.Item(36).Soudures = 5
'    cPanel.cCostCAtMain.colCostCAtOperations.Item(37).Soudures = 0
'    cPanel.cCostCAtMain.colCostCAtOperations.Item(47).Soudures = 50
'    cPanel.cCostCAtMain.colCostCAtOperations.Item(48).Soudures = 50
'    cPanel.cCostCAtMain.colCostCAtOperations.Item(51).Soudures = 0
'    Set cPanel = Nothing
'    Exit Function
'setBilgeID_SoudureErr:
'    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmCostCAt: Function setBilgeID_Soudure")
'End Function

'Private Function setGirderID_Soudure(ByVal PanelIndex As Integer)
'    On Error GoTo  setGirderID_SoudureErr
'    Dim cPanel As cPanel
'    Set cPanel = colPanel.Item(PanelIndex)
'    cPanel.cCostCAtMain.colCostCAtOperations.Item(21).Soudures = 0
'    cPanel.cCostCAtMain.colCostCAtOperations.Item(22).Soudures = 2
'    cPanel.cCostCAtMain.colCostCAtOperations.Item(23).Soudures = 4
'    cPanel.cCostCAtMain.colCostCAtOperations.Item(36).Soudures = 0
'    cPanel.cCostCAtMain.colCostCAtOperations.Item(37).Soudures = 2
'    cPanel.cCostCAtMain.colCostCAtOperations.Item(47).Soudures = 0
'    cPanel.cCostCAtMain.colCostCAtOperations.Item(48).Soudures = 16
'    Select Case IsPanelOnSymmAxis(PanelIndex, ProjectIndex)
'        Case Is = True
'            cPanel.cCostCAtMain.colCostCAtOperations.Item(51).Soudures = 16
'        Case Is = False
'            cPanel.cCostCAtMain.colCostCAtOperations.Item(51).Soudures = 0
'    End Select
'    Set cPanel = Nothing
'    Exit Function
'setGirderID_SoudureErr:
'    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmCostCAt: Function setGirderID_Soudure")
'End Function

'Private Function setShellID_Soudure(ByVal PanelIndex As Integer)
'    On Error GoTo  setShellID_SoudureErr
'    Dim cPanel As cPanel
'    Set cPanel = colPanel.Item(PanelIndex)
'    cPanel.cCostCAtMain.colCostCAtOperations.Item(21).Soudures = 0
'    cPanel.cCostCAtMain.colCostCAtOperations.Item(22).Soudures = 0
'    cPanel.cCostCAtMain.colCostCAtOperations.Item(23).Soudures = 0
'    cPanel.cCostCAtMain.colCostCAtOperations.Item(36).Soudures = 0
'    cPanel.cCostCAtMain.colCostCAtOperations.Item(37).Soudures = 0
'    Select Case IsPanelHorizontal(PanelIndex, ProjectIndex)
'        Case Is = True
'            cPanel.cCostCAtMain.colCostCAtOperations.Item(47).Soudures = 14
'        Case Is = False
'            cPanel.cCostCAtMain.colCostCAtOperations.Item(47).Soudures = 15
'    End Select
'    Select Case IsPanelInsideStructure(PanelIndex, ProjectIndex)
'        Case Is = True
'            cPanel.cCostCAtMain.colCostCAtOperations.Item(48).Soudures = 16
'        Case Is = False
'            cPanel.cCostCAtMain.colCostCAtOperations.Item(48).Soudures = 51
'    End Select
'    cPanel.cCostCAtMain.colCostCAtOperations.Item(51).Soudures = 0
'    Set cPanel = Nothing
'    Exit Function
'setShellID_SoudureErr:
'    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmCostCAt: Function setShellID_Soudure")
'End Function

'Private Function setVirtualID_Soudure(ByVal PanelIndex As Integer)
'    On Error GoTo  setVirtualID_SoudureErr
'    Dim cPanel As cPanel
'    Set cPanel = colPanel.Item(PanelIndex)
'    cPanel.cCostCAtMain.colCostCAtOperations.Item(21).Soudures = 0
'    cPanel.cCostCAtMain.colCostCAtOperations.Item(22).Soudures = 0
'    cPanel.cCostCAtMain.colCostCAtOperations.Item(23).Soudures = 0
'    cPanel.cCostCAtMain.colCostCAtOperations.Item(36).Soudures = 0
'    cPanel.cCostCAtMain.colCostCAtOperations.Item(37).Soudures = 0
'    cPanel.cCostCAtMain.colCostCAtOperations.Item(47).Soudures = 0
'    cPanel.cCostCAtMain.colCostCAtOperations.Item(48).Soudures = 0
'    cPanel.cCostCAtMain.colCostCAtOperations.Item(51).Soudures = 0
'    Set cPanel = Nothing
'    Exit Function
'setVirtualID_SoudureErr:
'    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmCostCAt: Function setVirtualID_Soudure")
'End Function

Private Function resetNappeInterieureID_Soudure(ByRef cNappe As cCostCAtNappe)
    On Error GoTo resetNappeInterieureID_SoudureErr
    Dim cPanel As cPanel, cIndex As cIndex
    For Each cIndex In cNappe.Nappe
        Set cPanel = colPanel.Item(cIndex.Number)
        Select Case cPanel.cCostCAtMain.IP_PANNEAU
            Case InterieurNappe
                cPanel.cCostCAtMain.colCostCAtOperations.Item(47).Soudures = 0
            Case ExterieurNappe
        End Select
        Set cPanel = Nothing
    Next cIndex
    Exit Function
resetNappeInterieureID_SoudureErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmCostCAt: Function resetNappeInterieureID_Soudure")
End Function

Private Function colNRenum()
    On Error GoTo colNRenumErr
    Dim col As New Collection
    Dim cN As cCostCAtNappe
    Dim i As Integer
    i = 0
    For Each cN In colCostCAtNappe
        i = i + 1
        cN.index = i
        col.Add cN
    Next cN
    Set colCostCAtNappe = col
    Exit Function
colNRenumErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmCostCAt: Function colNRenum")
End Function

Private Function colDHRenum()
    On Error GoTo colDHRenumErr
    Dim col As New Collection
    Dim cDH As cCostCAtDHull
    Dim i As Integer
    i = 0
    For Each cDH In colCostCAtDHull
        i = i + 1
        cDH.index = i
        col.Add cDH
    Next cDH
    Set colCostCAtDHull = col
    Exit Function
colDHRenumErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmCostCAt: Function colDHRenum")
End Function

Private Sub ActivateFrame(ByVal index As Integer)
    frStepIndex = index
    MSH1.Visible = False
    'TxtEdit.Visible = False
    'Combo.Visible = False
    lblPage.Caption = "page " & index & " sur " & frStep.Count
    Select Case index
        Case 1 'Paramètres globaux
            txtNAM.Text = cHeader.cCostCAtMain.iNAM
            txtPMB.Text = cHeader.cCostCAtMain.lPMB
            txtDIVtol.Text = cHeader.cCostCAtMain.lDiversTempsTolier
            txtDIVsoud.Text = cHeader.cCostCAtMain.lDiversTempsSoudeur
            txtNoIndicesDC.Text = cHeader.cCostCAtMain.NoIndicesDCoque
            Project.Item(ProjectIndex).colPanel.SetPanelsVisible
            Draw ProjectIndex
        Case 2 'Types de panneaux
            Set MSH1.Container = frStep.Item(2)
            Set TxtEdit.Container = frStep.Item(2)
            Project.Item(ProjectIndex).colPanel.SetPanelsVisible
            Project.Item(ProjectIndex).cDisplaySettings.DrawCostCAtItems = DrawCostCAtPanelTypes
            Draw ProjectIndex
            MSH1_Step2
        Case 3 'Nappes planes
            Set MSH1.Container = frStep.Item(3)
            Set TxtEdit.Container = frStep.Item(3)
            VerifyStep3DataIntegrity
            OnlyNappesVisible
            Project.Item(ProjectIndex).cDisplaySettings.DrawCostCAtItems = DrawCostCAtNappes
            Draw ProjectIndex
            MSH1_Step3
        Case 4 'Opérations de pré-pré (voiles)
            Set MSH1.Container = frStep.Item(4)
            Set TxtEdit.Container = frStep.Item(4)
            Set Combo.Container = frStep.Item(4)
            Project.Item(ProjectIndex).colPanel.SetPanelsVisible
            Draw ProjectIndex
            MSH1_Step4
        Case 5 'Opérations de pré-pré (carlingues, serres, hiloires)
            'SetStep4Fract
            Set MSH1.Container = frStep.Item(5)
            Set TxtEdit.Container = frStep.Item(5)
            Set Combo.Container = frStep.Item(5)
            Project.Item(ProjectIndex).colPanel.SetPanelsVisible
            Draw ProjectIndex
            MSH1_Step5
        Case 6 'Opérations de pré (fabrication de la nappe plane)
            Set MSH1.Container = frStep.Item(6)
            Set TxtEdit.Container = frStep(6)
            Set Combo.Container = frStep(6)
            Project.Item(ProjectIndex).colPanel.SetPanelsVisible
            Draw ProjectIndex
            MSH1_Step6
        Case 7 'Opérations de pré (assemblage)
            Set MSH1.Container = frStep.Item(7)
            Set TxtEdit.Container = frStep(7)
            Set Combo.Container = frStep(7)
            Project.Item(ProjectIndex).colPanel.SetPanelsVisible
            Draw ProjectIndex
            MSH1_Step7
        Case 8 'Opérations de pré-montage - montage
            Set MSH1.Container = frStep.Item(8)
            Set TxtEdit.Container = frStep(8)
            Set Combo.Container = frStep(8)
            Project.Item(ProjectIndex).colPanel.SetPanelsVisible
            Draw ProjectIndex
            MSH1_Step8
        Case 9 'ID Soudures
            Set MSH1.Container = frStep.Item(9)
            Set TxtEdit.Container = frStep(9)
            Set Combo.Container = frStep(9)
            Project.Item(ProjectIndex).colPanel.SetPanelsVisible
            Draw ProjectIndex
            ComputeFractionnements
            MSH1_Step9
        Case Else
    End Select
End Sub

'Private Sub ComputeIDSoudures()
'    On Error GoTo  ComputeIDSouduresErr
'    Dim cPanel As cPanel
'    For Each cPanel In colPanel
'        Select Case cPanel.cCostCAtMain.ID_PANNEAU
'        ' = NappePlane: = CarlingueSerreHiloire: = Bouchain: = Epontille: = Virtual
'            Case NappePlane
'                Select Case cPanel.cCostCAtMain.IP_PANNEAU
'                ' = InterieurNappe: = ExterieurNappe
'                    Case InterieurNappe
'                        Select Case cPanel.cCostCAtMain.IT_PANNEAU
'                        ' = BordeSimple: = DoubleCoqueInterieure: = DoubleCoqueExterieure
'                            Case BordeSimple
'                                cPanel.cCostCAtMain.colCostCAtOperations.Item(21).Soudures = 0
'                                cPanel.cCostCAtMain.colCostCAtOperations.Item(22).Soudures = 0
'                                cPanel.cCostCAtMain.colCostCAtOperations.Item(23).Soudures = 0
'                                cPanel.cCostCAtMain.colCostCAtOperations.Item(36).Soudures = 0
'                                cPanel.cCostCAtMain.colCostCAtOperations.Item(37).Soudures = 0
'                                cPanel.cCostCAtMain.colCostCAtOperations.Item(47).Soudures = 0
'                                cPanel.cCostCAtMain.colCostCAtOperations.Item(48).Soudures = getID_op48
'                                cPanel.cCostCAtMain.colCostCAtOperations.Item(51).Soudures = 0
'                            Case DoubleCoqueInterieure
'
'                            Case DoubleCoqueExterieure
'
'                        End Select
'                    Case ExterieurNappe
'                        Select Case cPanel.cCostCAtMain.IT_PANNEAU
'                        ' = BordeSimple: = DoubleCoqueInterieure: = DoubleCoqueExterieure
'                            Case BordeSimple
'
'                            Case DoubleCoqueInterieure
'
'                            Case DoubleCoqueExterieure
'
'                        End Select
'                End Select
'            Case CarlingueSerreHiloire
'
'            Case Bouchain
'
'            Case Epontille
'
'            Case Virtual
'
'        End Select
'    Next cPanel
'    Exit Sub
'ComputeIDSouduresErr:
'    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmCostCAt: Sub ComputeIDSoudures")
'End Sub

Private Sub ComputeNappePlane()
    On Error GoTo ComputeNappePlaneErr
    Dim cPanel As cPanel, cCostCAtNappe As cCostCAtNappe, Panneau_de_Nappe As cIndex
    'Nappes (un panneau)
    For Each cPanel In colPanel
        cPanel.cCostCAtMain.colCostCAtOperations.Item(11).Fractionnement = FractOp11NappesSimples(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(12).Fractionnement = _
            cPanel.cCostCAtMain.colCostCAtOperations.Item(11).Fractionnement 'same as op. 11
        cPanel.cCostCAtMain.colCostCAtOperations.Item(13).Fractionnement = _
            cPanel.cCostCAtMain.colCostCAtOperations.Item(11).Fractionnement 'same as op. 11
        cPanel.cCostCAtMain.colCostCAtOperations.Item(14).Fractionnement = FractOp14NappesSimples(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(15).Fractionnement = _
            cPanel.cCostCAtMain.colCostCAtOperations.Item(14).Fractionnement 'same as op. 14
        cPanel.cCostCAtMain.colCostCAtOperations.Item(16).Fractionnement = _
            cPanel.cCostCAtMain.colCostCAtOperations.Item(14).Fractionnement 'same as op. 14
        cPanel.cCostCAtMain.colCostCAtOperations.Item(17).Fractionnement = _
            cPanel.cCostCAtMain.colCostCAtOperations.Item(11).Fractionnement 'same as op. 11
        cPanel.cCostCAtMain.colCostCAtOperations.Item(18).Fractionnement = _
            cPanel.cCostCAtMain.colCostCAtOperations.Item(11).Fractionnement 'same as op. 11
        cPanel.cCostCAtMain.colCostCAtOperations.Item(19).Fractionnement = FractOp19NappesSimples(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(20).Fractionnement = FractOp20NappesSimples(cPanel)
    Next cPanel
    'Nappes (collection panneaux)
    For Each cCostCAtNappe In colCostCAtNappe
        Dim fractOp11Nappes As Double, fractOp14Nappes As Double, fractOp15Nappes As Double, _
            fractOp16Nappes As Double, fractOp19Nappes As Double, fractOp20Nappes As Double
        fractOp11Nappes = FractOp11NappesComposees(cCostCAtNappe, ProjectIndex)
        fractOp14Nappes = FractOp14NappesComposees(cCostCAtNappe, ProjectIndex)
        fractOp19Nappes = FractOp19NappesComposees(cCostCAtNappe, ProjectIndex)
        fractOp20Nappes = FractOp20NappesComposees(cCostCAtNappe, ProjectIndex)
        For Each Panneau_de_Nappe In cCostCAtNappe.Nappe
            colPanel.Item(Panneau_de_Nappe.Number).cCostCAtMain.colCostCAtOperations.Item(11).Fractionnement = fractOp11Nappes
            colPanel.Item(Panneau_de_Nappe.Number).cCostCAtMain.colCostCAtOperations.Item(12).Fractionnement = fractOp11Nappes 'same as op. 11
            colPanel.Item(Panneau_de_Nappe.Number).cCostCAtMain.colCostCAtOperations.Item(13).Fractionnement = fractOp11Nappes 'same as op. 11
            colPanel.Item(Panneau_de_Nappe.Number).cCostCAtMain.colCostCAtOperations.Item(14).Fractionnement = fractOp14Nappes
            colPanel.Item(Panneau_de_Nappe.Number).cCostCAtMain.colCostCAtOperations.Item(15).Fractionnement = fractOp14Nappes 'same as op. 14
            colPanel.Item(Panneau_de_Nappe.Number).cCostCAtMain.colCostCAtOperations.Item(16).Fractionnement = fractOp14Nappes 'same as op. 14
            colPanel.Item(Panneau_de_Nappe.Number).cCostCAtMain.colCostCAtOperations.Item(17).Fractionnement = fractOp11Nappes 'same as op. 11
            colPanel.Item(Panneau_de_Nappe.Number).cCostCAtMain.colCostCAtOperations.Item(18).Fractionnement = fractOp11Nappes 'same as op. 11
            colPanel.Item(Panneau_de_Nappe.Number).cCostCAtMain.colCostCAtOperations.Item(19).Fractionnement = fractOp19Nappes
            colPanel.Item(Panneau_de_Nappe.Number).cCostCAtMain.colCostCAtOperations.Item(20).Fractionnement = fractOp20Nappes
        Next Panneau_de_Nappe
    Next cCostCAtNappe
    Exit Sub
ComputeNappePlaneErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmCostCAt: Sub ComputeNappePlane")
End Sub

Private Sub ComputePreAssemblage()
    On Error GoTo ComputePreAssemblageErr
    Dim cPanel As cPanel, cDoubleHull As cCostCAtDHull
    
    'Plates
    For Each cPanel In colPanel
        cPanel.cCostCAtMain.colCostCAtOperations.Item(21).Fractionnement = FractOp21BordesSimples(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(22).Fractionnement = FractOp22Carlingues(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(23).Fractionnement = FractOp23Carlingues(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(24).Fractionnement = FractOp24BordesSimples(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(25).Fractionnement = FractOp25BordesSimples(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(26).Fractionnement = FractOp26BordesSimples(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(27).Fractionnement = FractOp27BordesSimples(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(28).Fractionnement = FractOp28BordesSimples(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(29).Fractionnement = FractOp28BordesSimples(cPanel) 'same as op. 28
        cPanel.cCostCAtMain.colCostCAtOperations.Item(30).Fractionnement = FractOp30BordesSimples(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(31).Fractionnement = FractOp31BordesSimples(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(32).Fractionnement = FractOp32Bouchain(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(33).Fractionnement = FractOp33Bouchain(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(34).Fractionnement = FractOp34Bouchain(cPanel)
        '35 --> computed for 'Double Hulls'
        cPanel.cCostCAtMain.colCostCAtOperations.Item(36).Fractionnement = FractOp36BordesSimples(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(37).Fractionnement = FractOp22Carlingues(cPanel) 'same as op. 22
        cPanel.cCostCAtMain.colCostCAtOperations.Item(38).Fractionnement = FractOp38BordesSimples(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(39).Fractionnement = FractOp39BordesSimples(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(40).Fractionnement = FractOp40BordesSimples(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(41).Fractionnement = 0
        cPanel.cCostCAtMain.colCostCAtOperations.Item(42).Fractionnement = FractOp42BordesSimples(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(43).Fractionnement = FractOp42BordesSimples(cPanel) 'same as op. 42
        cPanel.cCostCAtMain.colCostCAtOperations.Item(44).Fractionnement = FractOp44BordesSimples(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(45).Fractionnement = FractOp45BordesSimples(cPanel)
    Next cPanel
    
    'Double Hulls
    Dim cInner As cIndex, cOuter As cIndex
    For Each cDoubleHull In colCostCAtDHull
        Dim fractOp21DQ As Double, fractOp27DQ As Double, fractOp35DQ As Double, _
        fractOp41DQ As Double
        
        'fractOp21DQ = FractOp21DoubleCoques(cDoubleHull, ProjectIndex)
        fractOp27DQ = FractOp27DoubleCoques(cDoubleHull, ProjectIndex)
        fractOp35DQ = FractOp35DoubleCoques(colCostCAtDHull, cHeader)
        fractOp41DQ = FractOp41DoubleCoques(cDoubleHull, ProjectIndex)
        
        For Each cInner In cDoubleHull.InnerShell
            colPanel.Item(cInner.Number).cCostCAtMain.colCostCAtOperations.Item(21).Fractionnement = 1
            colPanel.Item(cInner.Number).cCostCAtMain.colCostCAtOperations.Item(22).Fractionnement = 0
            colPanel.Item(cInner.Number).cCostCAtMain.colCostCAtOperations.Item(23).Fractionnement = 0
            colPanel.Item(cInner.Number).cCostCAtMain.colCostCAtOperations.Item(24).Fractionnement = 1
            '25, 26 --> computed with 'bordés simples'
            colPanel.Item(cInner.Number).cCostCAtMain.colCostCAtOperations.Item(27).Fractionnement = fractOp27DQ
            '28, 29, 30, 31 --> computed with 'bordés simples'
            colPanel.Item(cInner.Number).cCostCAtMain.colCostCAtOperations.Item(32).Fractionnement = 0
            colPanel.Item(cInner.Number).cCostCAtMain.colCostCAtOperations.Item(34).Fractionnement = 0
            colPanel.Item(cInner.Number).cCostCAtMain.colCostCAtOperations.Item(35).Fractionnement = fractOp35DQ
            colPanel.Item(cInner.Number).cCostCAtMain.colCostCAtOperations.Item(36).Fractionnement = 0
            colPanel.Item(cInner.Number).cCostCAtMain.colCostCAtOperations.Item(37).Fractionnement = 0
            colPanel.Item(cInner.Number).cCostCAtMain.colCostCAtOperations.Item(38).Fractionnement = 0
            '39, 40 --> computed with 'bordés simples'
            colPanel.Item(cInner.Number).cCostCAtMain.colCostCAtOperations.Item(41).Fractionnement = 0
            '42, 43, 44, 45 --> computed with 'bordés simples'
        Next cInner
        For Each cOuter In cDoubleHull.OuterShell
            colPanel.Item(cOuter.Number).cCostCAtMain.colCostCAtOperations.Item(21).Fractionnement = 0
            colPanel.Item(cOuter.Number).cCostCAtMain.colCostCAtOperations.Item(22).Fractionnement = 0
            colPanel.Item(cOuter.Number).cCostCAtMain.colCostCAtOperations.Item(23).Fractionnement = 0
            colPanel.Item(cOuter.Number).cCostCAtMain.colCostCAtOperations.Item(24).Fractionnement = 0
            '25, 26 --> computed with 'bordés simples'
            colPanel.Item(cOuter.Number).cCostCAtMain.colCostCAtOperations.Item(27).Fractionnement = 0
            '28, 29, 30, 31 --> computed with 'bordés simples'
            colPanel.Item(cOuter.Number).cCostCAtMain.colCostCAtOperations.Item(32).Fractionnement = 0
            colPanel.Item(cOuter.Number).cCostCAtMain.colCostCAtOperations.Item(34).Fractionnement = 0
            colPanel.Item(cOuter.Number).cCostCAtMain.colCostCAtOperations.Item(35).Fractionnement = fractOp35DQ
            colPanel.Item(cOuter.Number).cCostCAtMain.colCostCAtOperations.Item(36).Fractionnement = 1
            colPanel.Item(cOuter.Number).cCostCAtMain.colCostCAtOperations.Item(37).Fractionnement = 0
            colPanel.Item(cOuter.Number).cCostCAtMain.colCostCAtOperations.Item(38).Fractionnement = 1
            '39, 40 --> computed with 'bordés simples'
            colPanel.Item(cOuter.Number).cCostCAtMain.colCostCAtOperations.Item(41).Fractionnement = fractOp41DQ
            '42, 43, 44, 45 --> computed with 'bordés simples'
        Next cOuter
    Next cDoubleHull
    'reset Op35 if all double hulls deleted
    If colCostCAtDHull.Count = 0 Then
        cHeader.cCostCAtMain.NoIndicesDCoque = 0
    End If
    Exit Sub
ComputePreAssemblageErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmCostCAt: Sub ComputePreAssemblage")
End Sub

Private Sub ComputeMontage()
    On Error GoTo ComputeMontageErr
    Dim cPanel As cPanel, cDoubleHull As cCostCAtDHull
    'Plates
    
    FractOp54BordesSimples colPanel, ProjectIndex 'out of 'for each ... next' sequence because values can change
                                                  'more than one time ( k for a panel can change when computing
                                                  'another panel
    FractOp56BordesSimples colPanel, ProjectIndex
    FractOp47Nappes colPanel, colCostCAtNappe, ProjectIndex
    
    For Each cPanel In colPanel
        cPanel.cCostCAtMain.colCostCAtOperations.Item(46).Fractionnement = 0
        cPanel.cCostCAtMain.colCostCAtOperations.Item(48).Fractionnement = FractOp48BordesSimples(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(49).Fractionnement = FractOp49BordesSimples(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(50).Fractionnement = FractOp50BordesSimples(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(51).Fractionnement = FractOp51Carlingues(cPanel, ProjectIndex)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(52).Fractionnement = 0
        cPanel.cCostCAtMain.colCostCAtOperations.Item(53).Fractionnement = FractOp53BordesSimples(cPanel)
        '54 ---> out of sequence (see begining of the Sub)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(55).Fractionnement = FractOp55BordesSimples(cPanel)
'        cPanel.cCostCAtMain.colCostCAtOperations.Item(56).Fractionnement = _
'        cPanel.cCostCAtMain.colCostCAtOperations.Item(54).Fractionnement * 2 'always double of op 54
        cPanel.cCostCAtMain.colCostCAtOperations.Item(57).Fractionnement = FractOp57BordesSimples(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(58).Fractionnement = FractOp58Epontilles(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(59).Fractionnement = FractOp59Epontilles(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(60).Fractionnement = FractOp60BordesSimples(cPanel)
    Next cPanel
    'Double Hulls
    Dim cInner As cIndex, cOuter As cIndex
    For Each cDoubleHull In colCostCAtDHull
        Dim fractOp49DQ As Double, fractOp50DQ As Double
        fractOp49DQ = FractOp49DoubleCoques(cDoubleHull, ProjectIndex)
        fractOp50DQ = FractOp50DoubleCoques(cDoubleHull, ProjectIndex)
        For Each cInner In cDoubleHull.InnerShell
            colPanel.Item(cInner.Number).cCostCAtMain.colCostCAtOperations.Item(49).Fractionnement = fractOp49DQ
            colPanel.Item(cInner.Number).cCostCAtMain.colCostCAtOperations.Item(50).Fractionnement = fractOp50DQ
        Next cInner
        For Each cOuter In cDoubleHull.OuterShell
            colPanel.Item(cOuter.Number).cCostCAtMain.colCostCAtOperations.Item(49).Fractionnement = fractOp49DQ
            colPanel.Item(cOuter.Number).cCostCAtMain.colCostCAtOperations.Item(50).Fractionnement = fractOp50DQ
        Next cOuter
    Next cDoubleHull
    Exit Sub
ComputeMontageErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmCostCAt: Sub ComputeMontage")
End Sub

Private Sub ComputePrePre()
    On Error GoTo SetStep4FracErr
    Dim cPanel As cPanel, cDoubleHull As cCostCAtDHull
    'Plates
    'Dim iProfilesSurVoiles As Integer, iPlatsEnBute As Integer, iStiff As Integer
    For Each cPanel In colPanel
        cPanel.cCostCAtMain.colCostCAtOperations.Item(1).Fractionnement = FractOp1BordesSimples(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(2).Fractionnement = FractOp2BordesSimples(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(3).Fractionnement = FractOp3BordesSimples(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(4).Fractionnement = FractOp4Carlingues(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(5).Fractionnement = FractOp5Carlingues(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(6).Fractionnement = FractOp6BordesSimples(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(7).Fractionnement = FractOp7BordesSimples(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(8).Fractionnement = FractOp8BordesSimples(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(9).Fractionnement = _
            cPanel.cCostCAtMain.colCostCAtOperations.Item(6).Fractionnement 'Mêmes fractionnements
        cPanel.cCostCAtMain.colCostCAtOperations.Item(10).Fractionnement = FractOp10BordesSimples(cPanel)
    Next cPanel
    ' Double Hulls
    Dim cInner As cIndex, cOuter As cIndex
    For Each cDoubleHull In colCostCAtDHull
        Dim fractOp1DQ As Double, fractOp2DQ As Double, fractOp3DQ As Double, _
        fractOp6DQ As Double, fractOp7DQ As Double, fractOp8DQ As Double, _
        fractOp10DQ As Double
        
        fractOp1DQ = FractOp1DoubleCoques(cDoubleHull, ProjectIndex)
        fractOp2DQ = FractOp2DoubleCoques(cDoubleHull, ProjectIndex)
        fractOp3DQ = FractOp3DoubleCoques(cDoubleHull, ProjectIndex)
        fractOp6DQ = FractOp6DoubleCoques(cDoubleHull, ProjectIndex)
        fractOp7DQ = FractOp7DoubleCoques(cDoubleHull, ProjectIndex)
        fractOp8DQ = FractOp8DoubleCoques(cDoubleHull, ProjectIndex)
        fractOp10DQ = FractOp10DoubleCoques(cDoubleHull, ProjectIndex)
        For Each cInner In cDoubleHull.InnerShell
            colPanel.Item(cInner.Number).cCostCAtMain.colCostCAtOperations.Item(1).Fractionnement = fractOp1DQ
            colPanel.Item(cInner.Number).cCostCAtMain.colCostCAtOperations.Item(2).Fractionnement = fractOp2DQ
            colPanel.Item(cInner.Number).cCostCAtMain.colCostCAtOperations.Item(3).Fractionnement = fractOp3DQ
            colPanel.Item(cInner.Number).cCostCAtMain.colCostCAtOperations.Item(6).Fractionnement = fractOp6DQ
            colPanel.Item(cInner.Number).cCostCAtMain.colCostCAtOperations.Item(7).Fractionnement = fractOp7DQ
            colPanel.Item(cInner.Number).cCostCAtMain.colCostCAtOperations.Item(8).Fractionnement = fractOp8DQ
            colPanel.Item(cInner.Number).cCostCAtMain.colCostCAtOperations.Item(9).Fractionnement = fractOp6DQ 'même que 6
            colPanel.Item(cInner.Number).cCostCAtMain.colCostCAtOperations.Item(10).Fractionnement = fractOp10DQ
        Next cInner
        For Each cOuter In cDoubleHull.OuterShell
            colPanel.Item(cOuter.Number).cCostCAtMain.colCostCAtOperations.Item(1).Fractionnement = fractOp1DQ
            colPanel.Item(cOuter.Number).cCostCAtMain.colCostCAtOperations.Item(2).Fractionnement = fractOp2DQ
            colPanel.Item(cOuter.Number).cCostCAtMain.colCostCAtOperations.Item(3).Fractionnement = fractOp3DQ
            colPanel.Item(cOuter.Number).cCostCAtMain.colCostCAtOperations.Item(6).Fractionnement = fractOp6DQ
            colPanel.Item(cOuter.Number).cCostCAtMain.colCostCAtOperations.Item(7).Fractionnement = fractOp7DQ
            colPanel.Item(cOuter.Number).cCostCAtMain.colCostCAtOperations.Item(8).Fractionnement = fractOp8DQ
            colPanel.Item(cOuter.Number).cCostCAtMain.colCostCAtOperations.Item(9).Fractionnement = fractOp6DQ 'même que 6
            colPanel.Item(cOuter.Number).cCostCAtMain.colCostCAtOperations.Item(10).Fractionnement = fractOp10DQ
        Next cOuter
    Next cDoubleHull
    Exit Sub
SetStep4FracErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmCostCAt: Sub ComputePrePre")
End Sub

Private Sub DrawPanelTypes(ByVal Panel As cPanel)
    On Error GoTo DrawPanelTypesErr:
    Select Case Panel.pType
        Case Plate, DoubleHull
        Case Else
            Exit Sub
    End Select
    Dim Pic As Long
    Pic = Project.Item(ProjectIndex).frmProject.picScreen.hdc
    If Panel.Visible = False Then Exit Sub
    Dim DY As Long, dZ As Long
    Dim hBrush As Long
    Dim angle As Double, Thickness As Double
    Dim Y_IN As Long, Z_IN As Long, Y_OUT As Long, Z_OUT As Long
    Y_IN = Project.Item(ProjectIndex).colNodes.Item(Panel.cGeometry.InNode).Y_Screen  'Panel.Z_IN_Screen
    Z_IN = Project.Item(ProjectIndex).colNodes.Item(Panel.cGeometry.InNode).Z_Screen 'Panel.Z_IN_Screen
    Y_OUT = Project.Item(ProjectIndex).colNodes.Item(Panel.cGeometry.OutNode).Y_Screen 'Panel.Z_OUT_Screen
    Z_OUT = Project.Item(ProjectIndex).colNodes.Item(Panel.cGeometry.OutNode).Z_Screen 'Panel.Z_OUT_Screen
    
    angle = Panel.cGeometry.PanelAngle
    Thickness = 5
    DY = Thickness * Cos(PI * (angle - 90) / 180)
    dZ = Thickness * Sin(PI * (angle - 90) / 180)
    ReDim lp(4)
    Dim selSizeY As Long, selSizeZ As Long
    selSizeY = 5 * Cos(PI * (angle - 90) / 180)
    selSizeZ = 5 * Sin(PI * (angle - 90) / 180)
    If Abs(selSizeY) < Abs(DY) Then selSizeY = DY
    If Abs(selSizeZ) < Abs(dZ) Then selSizeZ = dZ
    lp(0).Y = Y_IN - selSizeY: lp(0).z = Z_IN - selSizeZ
    lp(1).Y = Y_OUT - selSizeY: lp(1).z = Z_OUT - selSizeZ
    lp(2).Y = Y_OUT + selSizeY: lp(2).z = Z_OUT + selSizeZ
    lp(3).Y = Y_IN + selSizeY: lp(3).z = Z_IN + selSizeZ
    lp(4).Y = Y_IN - selSizeY: lp(4).z = Z_IN - selSizeZ
    FormRegions Panel, Project.Item(ProjectIndex).cDisplaySettings.ColorPlates
    
    If Panel.HighLighted = yes Then
        DY = selSizeY
        dZ = selSizeZ
    End If

    lp(0).Y = Y_IN - DY: lp(0).z = Z_IN - dZ
    lp(1).Y = Y_OUT - DY: lp(1).z = Z_OUT - dZ
    lp(2).Y = Y_OUT + DY: lp(2).z = Z_OUT + dZ
    lp(3).Y = Y_IN + DY: lp(3).z = Z_IN + dZ
    lp(4).Y = Y_IN - DY: lp(4).z = Z_IN - dZ
    Dim rgn As Long
    rgn = CreatePolygonRgn(lp(0), 5, 2)
    hBrush = apiCreateSolidBrush(Project.Item(ProjectIndex).cDisplaySettings.ColorPlates)
    FillRgn Pic, rgn, hBrush
    Polyline Pic, lp(0), 5
    DeleteObject hBrush
    DeleteObject rgn
    Exit Sub
DrawPanelTypesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmCostCAt: Sub DrawPanelTypes")
End Sub
Private Function VerifyStep3DataIntegrity()
    On Error GoTo VerifyStep3DataIntegrityErr
    Dim cPanel As cPanel
    Dim cCostCAtNappe As cCostCAtNappe
    Dim cIndex As cIndex
    
    For Each cPanel In colPanel
        Select Case cPanel.cCostCAtMain.ID_PANNEAU
            Case NappePlane
            Case Else
            For Each cCostCAtNappe In colCostCAtNappe
1:
                For Each cIndex In cCostCAtNappe.Nappe
                    If cPanel.pNumber = cIndex.Number Then
                        cCostCAtNappe.Nappe.Remove cIndex.index
                        cCostCAtNappe.Nappe.Renum
                        GoTo 1
                    End If
                Next cIndex
            Next cCostCAtNappe
        End Select
    Next cPanel
2:
    For Each cCostCAtNappe In colCostCAtNappe
        If cCostCAtNappe.Nappe.Count = 0 Then
            colCostCAtNappe.Remove cCostCAtNappe.index: colNRenum
            GoTo 2
        End If
    Next cCostCAtNappe
3:
    For Each cCostCAtNappe In colCostCAtNappe
        If cCostCAtNappe.Nappe.Count = 1 Then
            colPanel.Item(cCostCAtNappe.Nappe.Item(1).Number).cCostCAtMain.ID_PANNEAU = NappePlane
            colPanel.Item(cCostCAtNappe.Nappe.Item(1).Number).cCostCAtMain.bIsPartOfNappe = False
            cCostCAtNappe.Nappe.Remove 1
            colCostCAtNappe.Remove cCostCAtNappe.index: colNRenum
            GoTo 3
        End If
    Next cCostCAtNappe

    Exit Function
VerifyStep3DataIntegrityErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmCostCAt: Function VerifyStep3DataIntegrity")
End Function

Private Function OnlyNappesVisible()
    On Error GoTo OnlyNappesVisibleErr
    Dim cPanel As cPanel
    For Each cPanel In colPanel
        If cPanel.cCostCAtMain.ID_PANNEAU = NappePlane Then
            Project.Item(ProjectIndex).colPanel.Item(cPanel.pNumber).Visible = True
        Else
            Project.Item(ProjectIndex).colPanel.Item(cPanel.pNumber).Visible = False
        End If
    Next cPanel
    Exit Function
OnlyNappesVisibleErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmCostCAt: Function OnlyNappesVisible")
End Function

Private Function CountNappes() As Integer
    Dim i As Integer
    For i = 1 To colPanel.Count
        If colPanel.Item(i).pType = Plate Or colPanel.Item(i).pType = DoubleHull Then
            If colPanel.Item(i).cCostCAtMain.ID_PANNEAU = NappePlane Then
                If colPanel.Item(i).cCostCAtMain.bIsPartOfNappe = False Then
                    CountNappes = CountNappes + 1
                End If
            End If
        End If
    Next i
    CountNappes = CountNappes + colCostCAtNappe.Count
End Function

Private Sub MSH1_Step9()
    On Error GoTo MSH1_Step9Err
    With MSH1
        .Clear
        .Visible = True
        .Rows = GetMSHStep9Rows + 1
        .Cols = 7
        .FormatString = "^|^|^|^|^|^|^"
        .ColWidth(0) = 2800 '.Width / 5 + 300
        .ColWidth(1) = 1000
        .ColWidth(2) = 1000
        .ColWidth(3) = 1000
        .ColWidth(4) = 1000
        .ColWidth(5) = 1000
        .ColWidth(6) = 1130
        .RowHeight(0) = 230 * 4  ' 5 + 70
        .TextMatrix(0, 0) = "Panneau"
        .TextMatrix(0, 1) = "Voiles sur nappes, carlingues" & vbCrLf & " [ID]"
        .TextMatrix(0, 2) = "Carlingues sur napppes Inner" & vbCrLf & " [ID]"
        .TextMatrix(0, 3) = "Carlingues sur napppes Outer" & vbCrLf & " [ID]"
        .TextMatrix(0, 4) = "Joints des panneaux" & vbCrLf & " [ID]"
        .TextMatrix(0, 5) = "Abouts des panneaux" & vbCrLf & " [ID]"
        .TextMatrix(0, 6) = "Voiles sur carlingues (ax)" & vbCrLf & " [ID]"
        
    End With
    Dim RowIndex As Integer, i As Integer
    For i = 1 To MSH1.Rows - 1
        MSH1.RowHeight(i) = 240
    Next i
    Dim cPanel As cPanel
    RowIndex = 0
    
'    ' double coque extérieure
'    For Each cPanel In colPanel
'        Select Case cPanel.cCostCAtMain.ID_PANNEAU
'            Case NappePlane
'                Select Case cPanel.cCostCAtMain.IT_PANNEAU
'                    Case DoubleCoqueExterieure
'                        RowIndex = RowIndex + 1
'                        MSH1.TextMatrix(RowIndex, 0) = "Panneau" & " " & cPanel.pNumber & " (OUTER)"
'                End Select
'        End Select
'    Next cPanel
'    ' double coque intérieure
'    For Each cPanel In colPanel
'        Select Case cPanel.cCostCAtMain.ID_PANNEAU
'            Case NappePlane
'                Select Case cPanel.cCostCAtMain.IT_PANNEAU
'                    Case DoubleCoqueInterieure
'                        RowIndex = RowIndex + 1
'                        MSH1.TextMatrix(RowIndex, 0) = "Panneau" & " " & cPanel.pNumber & " (INNER)"
'                End Select
'        End Select
'    Next cPanel
'    ' bouchain
'    For Each cPanel In colPanel
'        Select Case cPanel.cCostCAtMain.ID_PANNEAU
'            Case Bouchain
'                RowIndex = RowIndex + 1
'                MSH1.TextMatrix(RowIndex, 0) = "Panneau" & " " & cPanel.pNumber & " (Bouchain)"
'        End Select
'    Next cPanel
'    ' carlingue serre hiloire
'    For Each cPanel In colPanel
'        Select Case cPanel.cCostCAtMain.ID_PANNEAU
'            Case CarlingueSerreHiloire
'                RowIndex = RowIndex + 1
'                MSH1.TextMatrix(RowIndex, 0) = "Panneau" & " " & cPanel.pNumber & " (Carlingue-Serre-Hiloire)"
'        End Select
'    Next cPanel
'    ' bordé simple
'    For Each cPanel In colPanel
'        Select Case cPanel.cCostCAtMain.ID_PANNEAU
'            Case NappePlane
'                Select Case cPanel.cCostCAtMain.IT_PANNEAU
'                    Case BordeSimple
'                        RowIndex = RowIndex + 1
'                        MSH1.TextMatrix(RowIndex, 0) = "Panneau" & " " & cPanel.pNumber & " (bordé simple)"
'                End Select
'        End Select
'    Next cPanel



    For Each cPanel In colPanel
        Select Case cPanel.cCostCAtMain.ID_PANNEAU '= Epontille: = Virtual
            Case Epontille, Virtual
                'MsgBox cPanel.cCostCAtMain.ID_PANNEAU
            Case NappePlane
                RowIndex = RowIndex + 1
                Select Case cPanel.cCostCAtMain.IT_PANNEAU
                    Case BordeSimple
                        MSH1.TextMatrix(RowIndex, 0) = "Panneau" & " " & cPanel.pNumber & " (Bordé Simple)"
                        MSH1.TextMatrix(RowIndex, 1) = "-------"
                        MSH1.TextMatrix(RowIndex, 2) = "-------"
                        MSH1.TextMatrix(RowIndex, 3) = "-------"
                        If cPanel.cCostCAtMain.colCostCAtOperations.Item(47).Fractionnement = 0 Then
                            MSH1.TextMatrix(RowIndex, 4) = "-------"
                        Else
                            MSH1.TextMatrix(RowIndex, 4) = cPanel.cCostCAtMain.colCostCAtOperations.Item(47).Soudures
                        End If
                        MSH1.TextMatrix(RowIndex, 5) = cPanel.cCostCAtMain.colCostCAtOperations.Item(48).Soudures
                        MSH1.TextMatrix(RowIndex, 6) = "-------"
                    Case DoubleCoqueInterieure
                        If cPanel.pNumber = 6 Then
                            cPanel.pNumber = cPanel.pNumber
                        End If
                        MSH1.TextMatrix(RowIndex, 0) = "Panneau" & " " & cPanel.pNumber & " (Inner)"
                        MSH1.TextMatrix(RowIndex, 1) = cPanel.cCostCAtMain.colCostCAtOperations.Item(21).Soudures
                        MSH1.TextMatrix(RowIndex, 2) = "-------"
                        MSH1.TextMatrix(RowIndex, 3) = "-------"
                        If cPanel.cCostCAtMain.colCostCAtOperations.Item(47).Fractionnement = 0 Then
                            MSH1.TextMatrix(RowIndex, 4) = "-------"
                        Else
                            MSH1.TextMatrix(RowIndex, 4) = cPanel.cCostCAtMain.colCostCAtOperations.Item(47).Soudures
                        End If
                        MSH1.TextMatrix(RowIndex, 5) = cPanel.cCostCAtMain.colCostCAtOperations.Item(48).Soudures
                        MSH1.TextMatrix(RowIndex, 6) = "-------"
                    Case DoubleCoqueExterieure
                        MSH1.TextMatrix(RowIndex, 0) = "Panneau" & " " & cPanel.pNumber & " (Outer)"
                        MSH1.TextMatrix(RowIndex, 1) = cPanel.cCostCAtMain.colCostCAtOperations.Item(36).Soudures
                        MSH1.TextMatrix(RowIndex, 2) = "-------"
                        MSH1.TextMatrix(RowIndex, 3) = "-------"
                        If cPanel.cCostCAtMain.colCostCAtOperations.Item(47).Fractionnement = 0 Then
                            MSH1.TextMatrix(RowIndex, 4) = "-------"
                        Else
                            MSH1.TextMatrix(RowIndex, 4) = cPanel.cCostCAtMain.colCostCAtOperations.Item(47).Soudures
                        End If
                        MSH1.TextMatrix(RowIndex, 5) = cPanel.cCostCAtMain.colCostCAtOperations.Item(48).Soudures
                        MSH1.TextMatrix(RowIndex, 6) = "-------"
                End Select
            Case Bouchain
                RowIndex = RowIndex + 1
                MSH1.TextMatrix(RowIndex, 0) = "Panneau" & " " & cPanel.pNumber & " (Bouchain)"
                MSH1.TextMatrix(RowIndex, 1) = cPanel.cCostCAtMain.colCostCAtOperations.Item(36).Soudures
                MSH1.TextMatrix(RowIndex, 2) = "-------"
                MSH1.TextMatrix(RowIndex, 3) = "-------"
                If cPanel.cCostCAtMain.colCostCAtOperations.Item(47).Fractionnement = 0 Then
                    MSH1.TextMatrix(RowIndex, 4) = "-------"
                Else
                    MSH1.TextMatrix(RowIndex, 4) = cPanel.cCostCAtMain.colCostCAtOperations.Item(47).Soudures
                End If
                MSH1.TextMatrix(RowIndex, 5) = cPanel.cCostCAtMain.colCostCAtOperations.Item(48).Soudures
                MSH1.TextMatrix(RowIndex, 6) = "-------"
            Case CarlingueSerreHiloire
                RowIndex = RowIndex + 1
                MSH1.TextMatrix(RowIndex, 0) = "Panneau" & " " & cPanel.pNumber & " (Carlingue-Serre-Hiloire)"
                MSH1.TextMatrix(RowIndex, 1) = cPanel.cCostCAtMain.colCostCAtOperations.Item(23).Soudures
                MSH1.TextMatrix(RowIndex, 2) = cPanel.cCostCAtMain.colCostCAtOperations.Item(22).Soudures
                MSH1.TextMatrix(RowIndex, 3) = cPanel.cCostCAtMain.colCostCAtOperations.Item(37).Soudures
                MSH1.TextMatrix(RowIndex, 4) = "-------"
                MSH1.TextMatrix(RowIndex, 5) = cPanel.cCostCAtMain.colCostCAtOperations.Item(48).Soudures
                If cPanel.cCostCAtMain.colCostCAtOperations.Item(51).Fractionnement = 0 Then
                    MSH1.TextMatrix(RowIndex, 6) = "-------"
                Else
                    MSH1.TextMatrix(RowIndex, 6) = cPanel.cCostCAtMain.colCostCAtOperations.Item(51).Soudures
                End If
                
        End Select
    Next cPanel
    Exit Sub
MSH1_Step9Err:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmCostCAt: Sub MSH1_Step9")
End Sub

Private Sub MSH1_Step8()
    On Error GoTo MSH1_Step8Err
    With MSH1
        .Clear
        .Visible = True
        .Rows = GetMSHStep8Rows + 1
        .Cols = 3
        .FormatString = "^|^|^"
        .ColWidth(0) = 2000 '.Width / 5 + 300
        .ColWidth(1) = 1500 '.Width / 4 '+ .Width / 2
        .ColWidth(2) = 1500
        .RowHeight(0) = 230 * 3  ' 5 + 70
        .TextMatrix(0, 0) = "Type" & vbCrLf & "Panneau"
        .TextMatrix(0, 1) = "Contacts des barrots sur cloisons" & vbCrLf & "[unités]"
        .TextMatrix(0, 2) = "Jonctions" & vbCrLf & "barrot / hiloire" & vbCrLf & "[unités]"
    End With
    Dim RowIndex As Integer, i As Integer
    For i = 1 To MSH1.Rows - 1
        MSH1.RowHeight(i) = 240
    Next i
    Dim cPanel As cPanel
    RowIndex = 0
    For Each cPanel In colPanel
    'For i = 1 To colPanel.Count
        Select Case cPanel.cCostCAtMain.IT_PANNEAU
            Case BordeSimple, DoubleCoqueInterieure, DoubleCoqueExterieure
                RowIndex = RowIndex + 1
                MSH1.TextMatrix(RowIndex, 0) = "Panneau de nappe" & " [" & cPanel.pNumber & "]"
                MSH1.TextMatrix(RowIndex, 1) = cPanel.cCostCAtMain.ContactsBarrotsCloisons
                MSH1.TextMatrix(RowIndex, 2) = cPanel.cCostCAtMain.JonctionsBarrotHiloire
        End Select
    'Next i
    Next cPanel
    Exit Sub
MSH1_Step8Err:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmCostCAt: Sub MSH1_Step8")
End Sub

Private Sub MSH1_Step7()
    On Error GoTo MSH1_Step7Err
    With MSH1
        .Clear
        .Visible = True
        .Rows = GetMSHStep7Rows + 1
        .Cols = 5
        .FormatString = "^|^|^|^|^"
        .ColWidth(0) = 2000 '.Width / 5 + 300
        .ColWidth(1) = 1100 '.Width / 4 '+ .Width / 2
        .ColWidth(2) = 2400
        .ColWidth(3) = 1200
        .ColWidth(4) = 1100
        .RowHeight(0) = 230 * 5 + 70
        .TextMatrix(0, 0) = "Type" & vbCrLf & "Panneau"
        .TextMatrix(0, 1) = "Couples intermediaires carlingues sur panneaux de nappe" & vbCrLf & "[unités]"
        .TextMatrix(0, 2) = "Type tapes" & vbCrLf & "[type]"
        .TextMatrix(0, 3) = "Sections tapes" & vbCrLf & "[type]"
        .TextMatrix(0, 4) = "Accostages des tôles individuelles de bouchain" & vbCrLf & "[unités]"
    End With
    Dim RowIndex As Integer, i As Integer
    For i = 1 To MSH1.Rows - 1
        MSH1.RowHeight(i) = 240
    Next i

    Dim cIndex As cIndex
    Dim sInner As String, sOuter As String
    RowIndex = 0
    For i = 1 To colPanel.Count
        Select Case colPanel.Item(i).cCostCAtMain.ID_PANNEAU
            Case Bouchain
                RowIndex = RowIndex + 1
                MSH1.TextMatrix(RowIndex, 0) = "Bouchain" & " [" & colPanel.Item(i).pNumber & "]"
                MSH1.TextMatrix(RowIndex, 1) = "-------"
                MSH1.TextMatrix(RowIndex, 2) = colPanel.Item(i).cCostCAtMain.GetTypeTapes
                MSH1.TextMatrix(RowIndex, 3) = colPanel.Item(i).cCostCAtMain.GetSectionTapes
                MSH1.TextMatrix(RowIndex, 4) = colPanel.Item(i).cCostCAtMain.AccostagesToleBouchain
        End Select
    Next i
    For i = 1 To colPanel.Count
        Select Case colPanel.Item(i).cCostCAtMain.IT_PANNEAU
            Case BordeSimple, DoubleCoqueInterieure, DoubleCoqueExterieure
                RowIndex = RowIndex + 1
                MSH1.TextMatrix(RowIndex, 0) = "Panneau de nappe" & " [" & colPanel.Item(i).pNumber & "]"
                MSH1.TextMatrix(RowIndex, 1) = colPanel.Item(i).cCostCAtMain.CouplesCarlinguesSurNappes
                MSH1.TextMatrix(RowIndex, 2) = colPanel.Item(i).cCostCAtMain.GetTypeTapes
                MSH1.TextMatrix(RowIndex, 3) = colPanel.Item(i).cCostCAtMain.GetSectionTapes
                MSH1.TextMatrix(RowIndex, 4) = "-------"
        End Select
    Next i
    Exit Sub
MSH1_Step7Err:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmCostCAt: Sub MSH1_Step7")
End Sub

Private Sub MSH1_Step6()
    On Error GoTo MSH1_Step6Err
    With MSH1
        .Clear
        .Visible = True
        .Rows = CountNappes + 1 'colPanel.Count + 1
        .Cols = 4
        .FormatString = "^|^|^|^"
        .ColWidth(0) = 2000 '.Width / 5 + 300
        .ColWidth(1) = 1100 '.Width / 4 '+ .Width / 2
        .ColWidth(2) = 1100
        .ColWidth(3) = 1100
        '.ColWidth(4) = 1300
        .RowHeight(0) = 230 * 3
        .TextMatrix(0, 0) = "Nappe" & vbCrLf & "Plane"
        .TextMatrix(0, 1) = "Nombre d'accostages [unités]"
        .TextMatrix(0, 2) = "Nombre abouts nappe [unités]"
        .TextMatrix(0, 3) = "Soudure lisses" & vbCrLf & "[type]"
        '.TextMatrix(0, 4) = "Sections lisses" & vbCrLf & "[type]"
    End With

    Dim RowIndex As Integer, i As Integer
    For i = 1 To MSH1.Rows - 1
        MSH1.RowHeight(i) = 240
    Next i

    Dim cIndex As cIndex
    Dim sNappe As String
    RowIndex = 0
    For i = 1 To colCostCAtNappe.Count
        RowIndex = RowIndex + 1
        For Each cIndex In colCostCAtNappe.Item(i).Nappe
            sNappe = sNappe & cIndex.Number & " "
        Next cIndex
        sNappe = Trim(sNappe)
        MSH1.TextMatrix(RowIndex, 0) = "Nappe " & "[" & sNappe & "]"
        MSH1.TextMatrix(RowIndex, 1) = colCostCAtNappe.Item(i).AccostagesNappes
        MSH1.TextMatrix(RowIndex, 2) = colCostCAtNappe.Item(i).iNANP
        MSH1.TextMatrix(RowIndex, 3) = colCostCAtNappe.Item(i).GetSoudureLissesNappes
        'MSH1.TextMatrix(RowIndex, 4) = colCostCAtNappe.Item(i).GetSectionLisses
        sNappe = ""
    Next i
    For i = 1 To colPanel.Count
        If colPanel.Item(i).pType = Plate Or colPanel.Item(i).pType = DoubleHull Then
            If colPanel.Item(i).cCostCAtMain.ID_PANNEAU = NappePlane And colPanel.Item(i).cCostCAtMain.bIsPartOfNappe = False Then
                RowIndex = RowIndex + 1
                MSH1.TextMatrix(RowIndex, 0) = "Nappe" & " [" & i & "]"
                MSH1.TextMatrix(RowIndex, 1) = colPanel.Item(i).cCostCAtMain.AccostagesNappes
                MSH1.TextMatrix(RowIndex, 2) = colPanel.Item(i).cCostCAtMain.iNANP
                MSH1.TextMatrix(RowIndex, 3) = colPanel.Item(i).cCostCAtMain.GetSoudureLissesNappes
                'MSH1.TextMatrix(RowIndex, 4) = colPanel.Item(i).cCostCAtMain.GetSectionLisses
            End If
        End If
    Next i
    Exit Sub
MSH1_Step6Err:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmCostCAt: Sub MSH1_Step6")
End Sub

Private Sub MSH1_Step5()
    On Error GoTo MSH1_Step5Err
    With MSH1
        .Clear
        .Visible = True
        .Rows = GetMSHStep5Rows + 1
        .Cols = 4
        .FormatString = "^|^|^|^"
        .ColWidth(0) = 2200 '.Width / 5 '+ 300
        .ColWidth(1) = 1100 '.Width / 10
        .ColWidth(2) = 1100 '.Width / 10
        .ColWidth(3) = 1100 '.Width / 10
        .RowHeight(0) = 230 * 4
        .TextMatrix(0, 0) = "Type" & vbCrLf & "Panneau"
        .TextMatrix(0, 1) = "Nombre des couples intermédiaires" & vbCrLf & "[unités]"
        .TextMatrix(0, 2) = "Habillage des carlingues" & vbCrLf & "[type]"
        .TextMatrix(0, 3) = "Dimension pièce d'habillage" & vbCrLf & "[longueur, m]"
    End With
    Dim RowIndex As Integer, i As Integer
    For i = 1 To MSH1.Rows - 1
        MSH1.RowHeight(i) = 240
    Next i

    Dim cIndex As cIndex
    Dim sInner As String, sOuter As String
    RowIndex = 0
    For i = 1 To colPanel.Count
        Select Case colPanel.Item(i).cCostCAtMain.ID_PANNEAU
            Case CarlingueSerreHiloire
                RowIndex = RowIndex + 1
                MSH1.TextMatrix(RowIndex, 0) = "Carlingue-Serre-Hiloire" & " [" & colPanel.Item(i).pNumber & "]"
                MSH1.TextMatrix(RowIndex, 1) = colPanel.Item(i).cCostCAtMain.iNCI
                MSH1.TextMatrix(RowIndex, 2) = colPanel.Item(i).cCostCAtMain.GetHabillageCarlingues
                MSH1.TextMatrix(RowIndex, 3) = colPanel.Item(i).cCostCAtMain.GetDimensionHabillage
            Case Bouchain
            Case Epontille
        End Select
    Next i
    Exit Sub
MSH1_Step5Err:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmCostCAt: Sub MSH1_Step5")
End Sub

Private Sub SetData()
    Dim i As Integer
    Dim cPanel As cPanel
    Dim cDoubleHull As cCostCAtDHull
    Dim RowIndex As Integer
    RowIndex = 0
    Select Case frStepIndex
        Case 1 'Paramètres globaux
            cHeader.cCostCAtMain.iNAM = txtNAM.Text
            cHeader.cCostCAtMain.lPMB = txtPMB.Text
            cHeader.cCostCAtMain.lDiversTempsTolier = txtDIVtol.Text
            cHeader.cCostCAtMain.lDiversTempsSoudeur = txtDIVsoud.Text
            cHeader.cCostCAtMain.NoIndicesDCoque = txtNoIndicesDC.Text
        Case 2 'Types de panneaux
        Case 3 'Nappes planes
        Case 4 'Opérations de pré-pré (voiles)
            For i = 1 To colCostCAtDHull.Count
                RowIndex = RowIndex + 1
                colCostCAtDHull.Item(i).ProfilesSurVoiles = MSH1.TextMatrix(RowIndex, 1)
                colCostCAtDHull.Item(i).SoudureProfilesSurVoiles = colCostCAtDHull.Item(i).SetSoudureProfilesSurVoiles(MSH1.TextMatrix(RowIndex, 2))
                colCostCAtDHull.Item(i).GoussetsProfilesVoiles = MSH1.TextMatrix(RowIndex, 3)
                colCostCAtDHull.Item(i).PlatsEnBute = MSH1.TextMatrix(RowIndex, 4)
                colCostCAtDHull.Item(i).AccostagesVoiles = MSH1.TextMatrix(RowIndex, 5)
                colCostCAtDHull.Item(i).SectionLisses = colCostCAtDHull.Item(i).SetSectionLisses(MSH1.TextMatrix(RowIndex, 6))
            Next i
            For i = 1 To colPanel.Count
                Select Case colPanel.Item(i).cCostCAtMain.ID_PANNEAU
                    Case CarlingueSerreHiloire
                    Case Bouchain
                        RowIndex = RowIndex + 1
                        colPanel.Item(i).cCostCAtMain.ProfilesSurVoiles = MSH1.TextMatrix(RowIndex, 1)
                        colPanel.Item(i).cCostCAtMain.SoudureProfilesSurVoiles = colPanel.Item(i).cCostCAtMain.SetSoudureProfilesSurVoiles(MSH1.TextMatrix(RowIndex, 2))
                        colPanel.Item(i).cCostCAtMain.GoussetsProfilesVoiles = MSH1.TextMatrix(RowIndex, 3)
                        colPanel.Item(i).cCostCAtMain.PlatsEnBute = MSH1.TextMatrix(RowIndex, 4)
                        colPanel.Item(i).cCostCAtMain.AccostagesVoiles = MSH1.TextMatrix(RowIndex, 5)
                        colPanel.Item(i).cCostCAtMain.SectionLisses = colPanel.Item(i).cCostCAtMain.SetSectionLisses(MSH1.TextMatrix(RowIndex, 6))
                    Case Epontille
                End Select
            Next i
            For i = 1 To colPanel.Count
                Select Case colPanel.Item(i).cCostCAtMain.ID_PANNEAU
                    Case NappePlane
                        Select Case colPanel.Item(i).cCostCAtMain.IT_PANNEAU
                            Case BordeSimple
                                RowIndex = RowIndex + 1
                                colPanel.Item(i).cCostCAtMain.ProfilesSurVoiles = MSH1.TextMatrix(RowIndex, 1)
                                colPanel.Item(i).cCostCAtMain.SoudureProfilesSurVoiles = colPanel.Item(i).cCostCAtMain.SetSoudureProfilesSurVoiles(MSH1.TextMatrix(RowIndex, 2))
                                colPanel.Item(i).cCostCAtMain.GoussetsProfilesVoiles = MSH1.TextMatrix(RowIndex, 3)
                                colPanel.Item(i).cCostCAtMain.PlatsEnBute = MSH1.TextMatrix(RowIndex, 4)
                                colPanel.Item(i).cCostCAtMain.AccostagesVoiles = MSH1.TextMatrix(RowIndex, 5)
                                colPanel.Item(i).cCostCAtMain.SectionLisses = colPanel.Item(i).cCostCAtMain.SetSectionLisses(MSH1.TextMatrix(RowIndex, 6))
                        End Select
                End Select
            Next i
            MSH1_Step4
        Case 5 'Opérations de pré-pré (carlingues, serres, hiloires)
            RowIndex = 0
            For i = 1 To colPanel.Count
                Select Case colPanel.Item(i).cCostCAtMain.ID_PANNEAU
                    Case CarlingueSerreHiloire
                        RowIndex = RowIndex + 1
                        colPanel.Item(i).cCostCAtMain.iNCI = MSH1.TextMatrix(RowIndex, 1)
                        colPanel.Item(i).cCostCAtMain.HabillageCarlingues = colPanel.Item(i).cCostCAtMain.SetHabillageCarlingues(MSH1.TextMatrix(RowIndex, 2))
                        colPanel.Item(i).cCostCAtMain.DimensionHabillage = colPanel.Item(i).cCostCAtMain.SetDimensionHabillage(MSH1.TextMatrix(RowIndex, 3))
                    Case Bouchain
                    Case Epontille
                End Select
            Next i
            MSH1_Step5
        Case 6 'Opérations de fabrication de la nappe plane
            RowIndex = 0
            For i = 1 To colCostCAtNappe.Count
                RowIndex = RowIndex + 1
                colCostCAtNappe.Item(i).AccostagesNappes = MSH1.TextMatrix(RowIndex, 1)
                colCostCAtNappe.Item(i).iNANP = MSH1.TextMatrix(RowIndex, 2)
                colCostCAtNappe.Item(i).SoudureLissesNappes = colCostCAtNappe.Item(i).SetSoudureLissesNappes(MSH1.TextMatrix(RowIndex, 3))
                'colCostCAtNappe.Item(i).SectionLisses = colCostCAtNappe.Item(i).SetSectionLisses(MSH1.TextMatrix(RowIndex, 4))
            Next i
            For i = 1 To colPanel.Count
                If colPanel.Item(i).pType = Plate Or colPanel.Item(i).pType = DoubleHull Then
                    If colPanel.Item(i).cCostCAtMain.ID_PANNEAU = NappePlane And colPanel.Item(i).cCostCAtMain.bIsPartOfNappe = False Then
                        RowIndex = RowIndex + 1
                        colPanel.Item(i).cCostCAtMain.AccostagesNappes = MSH1.TextMatrix(RowIndex, 1)
                        colPanel.Item(i).cCostCAtMain.iNANP = MSH1.TextMatrix(RowIndex, 2)
                        colPanel.Item(i).cCostCAtMain.SoudureLissesNappes = colPanel.Item(i).cCostCAtMain.SetSoudureLissesNappes(MSH1.TextMatrix(RowIndex, 3))
                        'colPanel.Item(i).cCostCAtMain.SectionLisses = colPanel.Item(i).cCostCAtMain.SetSectionLisses(MSH1.TextMatrix(RowIndex, 4))
                    End If
                End If
            Next i
            MSH1_Step6
        Case 7
            RowIndex = 0
            For i = 1 To colPanel.Count
                Select Case colPanel.Item(i).cCostCAtMain.ID_PANNEAU
                    Case Bouchain
                        RowIndex = RowIndex + 1
                        colPanel.Item(i).cCostCAtMain.TypeTapes = colPanel.Item(i).cCostCAtMain.SetTypeTapes(MSH1.TextMatrix(RowIndex, 2))
                        colPanel.Item(i).cCostCAtMain.SectionTapes = colPanel.Item(i).cCostCAtMain.SetSectionTapes(MSH1.TextMatrix(RowIndex, 3))
                        colPanel.Item(i).cCostCAtMain.AccostagesToleBouchain = MSH1.TextMatrix(RowIndex, 4)
                End Select
            Next i
            For i = 1 To colPanel.Count
                Select Case colPanel.Item(i).cCostCAtMain.IT_PANNEAU
                    Case BordeSimple, DoubleCoqueInterieure, DoubleCoqueExterieure
                        RowIndex = RowIndex + 1
                        colPanel.Item(i).cCostCAtMain.CouplesCarlinguesSurNappes = MSH1.TextMatrix(RowIndex, 1)
                        colPanel.Item(i).cCostCAtMain.TypeTapes = colPanel.Item(i).cCostCAtMain.SetTypeTapes(MSH1.TextMatrix(RowIndex, 2))
                        colPanel.Item(i).cCostCAtMain.SectionTapes = colPanel.Item(i).cCostCAtMain.SetSectionTapes(MSH1.TextMatrix(RowIndex, 3))
                End Select
            Next i
            MSH1_Step7
        Case 8
            RowIndex = 0
            For i = 1 To colPanel.Count
                Select Case colPanel.Item(i).cCostCAtMain.IT_PANNEAU
                    Case BordeSimple, DoubleCoqueInterieure, DoubleCoqueExterieure
                        RowIndex = RowIndex + 1
                        colPanel.Item(i).cCostCAtMain.ContactsBarrotsCloisons = MSH1.TextMatrix(RowIndex, 1)
                        colPanel.Item(i).cCostCAtMain.JonctionsBarrotHiloire = MSH1.TextMatrix(RowIndex, 2)
                End Select
            Next i
            MSH1_Step8
        Case 9
            RowIndex = 0
            For i = 1 To colPanel.Count
                Select Case colPanel.Item(i).cCostCAtMain.ID_PANNEAU
                    Case NappePlane
                        RowIndex = RowIndex + 1
                        Select Case colPanel.Item(i).cCostCAtMain.IT_PANNEAU
                            Case BordeSimple
                                If ValidateID_Soudure(MSH1.TextMatrix(RowIndex, 4)) = True Then
                                    colPanel.Item(i).cCostCAtMain.colCostCAtOperations.Item(47).Soudures = MSH1.TextMatrix(RowIndex, 4)
                                End If
                                If ValidateID_Soudure(MSH1.TextMatrix(RowIndex, 5)) = True Then
                                    colPanel.Item(i).cCostCAtMain.colCostCAtOperations.Item(48).Soudures = MSH1.TextMatrix(RowIndex, 5)
                                End If
                            Case DoubleCoqueInterieure
                                If ValidateID_Soudure(MSH1.TextMatrix(RowIndex, 1)) = True Then
                                    colPanel.Item(i).cCostCAtMain.colCostCAtOperations.Item(21).Soudures = MSH1.TextMatrix(RowIndex, 1)
                                End If
                                If ValidateID_Soudure(MSH1.TextMatrix(RowIndex, 4)) = True Then
                                    colPanel.Item(i).cCostCAtMain.colCostCAtOperations.Item(47).Soudures = MSH1.TextMatrix(RowIndex, 4)
                                End If
                                If ValidateID_Soudure(MSH1.TextMatrix(RowIndex, 5)) = True Then
                                    colPanel.Item(i).cCostCAtMain.colCostCAtOperations.Item(48).Soudures = MSH1.TextMatrix(RowIndex, 5)
                                End If
                            Case DoubleCoqueExterieure
                                If ValidateID_Soudure(MSH1.TextMatrix(RowIndex, 1)) = True Then
                                    colPanel.Item(i).cCostCAtMain.colCostCAtOperations.Item(36).Soudures = MSH1.TextMatrix(RowIndex, 1)
                                End If
                                If ValidateID_Soudure(MSH1.TextMatrix(RowIndex, 4)) = True Then
                                    colPanel.Item(i).cCostCAtMain.colCostCAtOperations.Item(47).Soudures = MSH1.TextMatrix(RowIndex, 4)
                                End If
                                If ValidateID_Soudure(MSH1.TextMatrix(RowIndex, 5)) = True Then
                                    colPanel.Item(i).cCostCAtMain.colCostCAtOperations.Item(48).Soudures = MSH1.TextMatrix(RowIndex, 5)
                                End If
                        End Select
                    Case CarlingueSerreHiloire
                        RowIndex = RowIndex + 1
                        If ValidateID_Soudure(MSH1.TextMatrix(RowIndex, 1)) = True Then
                            colPanel.Item(i).cCostCAtMain.colCostCAtOperations.Item(23).Soudures = MSH1.TextMatrix(RowIndex, 1)
                        End If
                        If ValidateID_Soudure(MSH1.TextMatrix(RowIndex, 2)) = True Then
                            colPanel.Item(i).cCostCAtMain.colCostCAtOperations.Item(22).Soudures = MSH1.TextMatrix(RowIndex, 2)
                        End If
                        If ValidateID_Soudure(MSH1.TextMatrix(RowIndex, 3)) = True Then
                            colPanel.Item(i).cCostCAtMain.colCostCAtOperations.Item(37).Soudures = MSH1.TextMatrix(RowIndex, 3)
                        End If
                        If ValidateID_Soudure(MSH1.TextMatrix(RowIndex, 6)) = True Then
                            colPanel.Item(i).cCostCAtMain.colCostCAtOperations.Item(51).Soudures = MSH1.TextMatrix(RowIndex, 6)
                        End If
                    Case Bouchain
                        RowIndex = RowIndex + 1
                        If ValidateID_Soudure(MSH1.TextMatrix(RowIndex, 1)) = True Then
                            colPanel.Item(i).cCostCAtMain.colCostCAtOperations.Item(36).Soudures = MSH1.TextMatrix(RowIndex, 1)
                        End If
                        If ValidateID_Soudure(MSH1.TextMatrix(RowIndex, 4)) = True Then
                            colPanel.Item(i).cCostCAtMain.colCostCAtOperations.Item(47).Soudures = MSH1.TextMatrix(RowIndex, 4)
                        End If
                        If ValidateID_Soudure(MSH1.TextMatrix(RowIndex, 5)) = True Then
                            colPanel.Item(i).cCostCAtMain.colCostCAtOperations.Item(48).Soudures = MSH1.TextMatrix(RowIndex, 5)
                        End If
                    Case Epontille, Virtual
                    Case Else
                End Select
            Next i
            MSH1_Step9
    End Select
End Sub

Private Sub MSH1_Step4()
    On Error GoTo MSH1_Step4Err
    With MSH1
        .Clear
        .Visible = True
        .Rows = GetMSHStep4Rows + 1
        .Cols = 7
        .FormatString = "^|^|^|^|^|^|^"
        .ColWidth(0) = 1700 '.Width / 5 '+ 300
        .ColWidth(1) = 950 '.Width / 10
        .ColWidth(2) = 1000 '.Width / 10
        .ColWidth(3) = 1000 '.Width / 10
        .ColWidth(4) = 900 '.Width / 10
        .ColWidth(5) = 1000 '.Width / 10
        .ColWidth(6) = 1300 '.Width / 10
        .RowHeight(0) = 230 * 4
        .TextMatrix(0, 0) = "Type" & vbCrLf & "Panneau"
        .TextMatrix(0, 1) = "Profilés sur âmes voiles" & vbCrLf & "[unités]"
        .TextMatrix(0, 2) = "Soudure profilés sur âmes voiles" & vbCrLf & "[type]"
        .TextMatrix(0, 3) = "Goussets sur semelles de profilés" & vbCrLf & "[unités]"
        .TextMatrix(0, 4) = "Plats" & vbCrLf & "en buté" & vbCrLf & "[unités]"
        .TextMatrix(0, 5) = "Accostages de voiles" & vbCrLf & "[unités]"
        .TextMatrix(0, 6) = "Sections lisses" & vbCrLf & "[type]"
    End With
    
    Dim RowIndex As Integer, i As Integer
    For i = 1 To MSH1.Rows - 1
        MSH1.RowHeight(i) = 240
    Next i

    Dim cIndex As cIndex
    Dim sInner As String, sOuter As String
    RowIndex = 0

    For i = 1 To colCostCAtDHull.Count
        RowIndex = RowIndex + 1
        For Each cIndex In colCostCAtDHull.Item(i).InnerShell
            sInner = sInner & cIndex.Number & " "
        Next cIndex
        For Each cIndex In colCostCAtDHull.Item(i).OuterShell
            sOuter = sOuter & cIndex.Number & " "
        Next cIndex
        sInner = Trim(sInner)
        sOuter = Trim(sOuter)
        MSH1.RowHeight(RowIndex) = 230 '* 2
        MSH1.TextMatrix(RowIndex, 0) = "Double Coque" & " [" & sInner & "; " & sOuter & "]"
        MSH1.TextMatrix(RowIndex, 1) = colCostCAtDHull.Item(i).ProfilesSurVoiles
        MSH1.TextMatrix(RowIndex, 2) = colCostCAtDHull.Item(i).GetSoudureProfilesSurVoiles
        MSH1.TextMatrix(RowIndex, 3) = colCostCAtDHull.Item(i).GoussetsProfilesVoiles
        MSH1.TextMatrix(RowIndex, 4) = colCostCAtDHull.Item(i).PlatsEnBute
        MSH1.TextMatrix(RowIndex, 5) = colCostCAtDHull.Item(i).AccostagesVoiles
        MSH1.TextMatrix(RowIndex, 6) = colCostCAtDHull.Item(i).GetSectionLisses
        sInner = ""
        sOuter = ""
    Next i

    For i = 1 To colPanel.Count
        Select Case colPanel.Item(i).cCostCAtMain.ID_PANNEAU
            Case CarlingueSerreHiloire
            Case Bouchain
                        RowIndex = RowIndex + 1
                        MSH1.TextMatrix(RowIndex, 0) = "Bouchain" & " [" & colPanel.Item(i).pNumber & "]"
                        MSH1.TextMatrix(RowIndex, 1) = colPanel.Item(i).cCostCAtMain.ProfilesSurVoiles
                        MSH1.TextMatrix(RowIndex, 2) = colPanel.Item(i).cCostCAtMain.GetSoudureProfilesSurVoiles
                        MSH1.TextMatrix(RowIndex, 3) = colPanel.Item(i).cCostCAtMain.GoussetsProfilesVoiles
                        MSH1.TextMatrix(RowIndex, 4) = colPanel.Item(i).cCostCAtMain.PlatsEnBute
                        MSH1.TextMatrix(RowIndex, 5) = colPanel.Item(i).cCostCAtMain.AccostagesVoiles
                        MSH1.TextMatrix(RowIndex, 6) = colPanel.Item(i).cCostCAtMain.GetSectionLisses
            Case Epontille
        End Select
    Next i
    For i = 1 To colPanel.Count
        Select Case colPanel.Item(i).cCostCAtMain.ID_PANNEAU
            Case NappePlane
                Select Case colPanel.Item(i).cCostCAtMain.IT_PANNEAU
                    Case BordeSimple
                        RowIndex = RowIndex + 1
                        MSH1.TextMatrix(RowIndex, 0) = "Bordé Simple" & " [" & colPanel.Item(i).pNumber & "]"
                        MSH1.TextMatrix(RowIndex, 1) = colPanel.Item(i).cCostCAtMain.ProfilesSurVoiles
                        MSH1.TextMatrix(RowIndex, 2) = colPanel.Item(i).cCostCAtMain.GetSoudureProfilesSurVoiles
                        MSH1.TextMatrix(RowIndex, 3) = colPanel.Item(i).cCostCAtMain.GoussetsProfilesVoiles
                        MSH1.TextMatrix(RowIndex, 4) = colPanel.Item(i).cCostCAtMain.PlatsEnBute
                        MSH1.TextMatrix(RowIndex, 5) = colPanel.Item(i).cCostCAtMain.AccostagesVoiles
                        MSH1.TextMatrix(RowIndex, 6) = colPanel.Item(i).cCostCAtMain.GetSectionLisses
                End Select
        End Select
    Next i
    Exit Sub
MSH1_Step4Err:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmCostCAt: Sub MSH1_Step4")
End Sub

Private Sub MSH1_Step3()
    On Error GoTo MSH1_Step3Err
    With MSH1
        .Clear
        .Visible = True
        .Rows = CountNappes + 1 'colPanel.Count + 1
        .Cols = 1
        .FormatString = "^|^"
        .ColWidth(0) = 1700 '.Width / 5 + 300
        .ColWidth(1) = 1500 '.Width / 4 '+ .Width / 2
        .RowHeight(0) = 230 * 3
        .TextMatrix(0, 0) = "Nappe" & vbCrLf & "Plane"
        .TextMatrix(0, 1) = "Panneaux"
    End With

    Dim RowIndex As Integer, i As Integer
    For i = 1 To MSH1.Rows - 1
        MSH1.RowHeight(i) = 240
    Next i

    Dim cIndex As cIndex
    Dim sNappe As String
    RowIndex = 0
    For i = 1 To colCostCAtNappe.Count
        RowIndex = RowIndex + 1
        MSH1.TextMatrix(RowIndex, 0) = "Nappe " '& i
        MSH1.Row = RowIndex: MSH1.col = 1
        MSH1.CellBackColor = &H8000000E 'white
        For Each cIndex In colCostCAtNappe.Item(i).Nappe
            sNappe = sNappe & cIndex.Number & " "
        Next cIndex
        MSH1.TextMatrix(RowIndex, 1) = RTrim(sNappe)
        sNappe = ""
    Next i
    For i = 1 To colPanel.Count
        If colPanel.Item(i).pType = Plate Or colPanel.Item(i).pType = DoubleHull Then
            If colPanel.Item(i).cCostCAtMain.ID_PANNEAU = NappePlane And colPanel.Item(i).cCostCAtMain.bIsPartOfNappe = False Then
                RowIndex = RowIndex + 1
                MSH1.TextMatrix(RowIndex, 0) = "Nappe " '& i
                MSH1.TextMatrix(RowIndex, 1) = colPanel.Item(i).pNumber
            End If
        End If
    Next i
    Exit Sub
MSH1_Step3Err:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmCostCAt: Sub MSH1_Step3")
End Sub

Private Sub MSH1_Step2()
    On Error GoTo MSH1_Step2Err
    With MSH1
        .Clear
        .Visible = True
        .Rows = GetMSHStep2Rows + 1
        .Cols = 4
        .FormatString = "^|^|^|^"

        .ColWidth(0) = 1700 '.Width / 5 + 300
        .ColWidth(1) = 1500 '.Width / 5
        .ColWidth(2) = 1500 '.Width / 4
        .ColWidth(3) = 1500 '.Width / 4
        .RowHeight(0) = 230 * 3
        .TextMatrix(0, 0) = "Type" & vbCrLf & "Panneau"
        .TextMatrix(0, 1) = "Panneau" & vbCrLf & "Simple"
        .TextMatrix(0, 2) = "Panneaux" & vbCrLf & "Intérieurs Double Coque [Inner]"
        .TextMatrix(0, 3) = "Panneaux" & vbCrLf & "Extérieurs Double Coque [Outer]"
    End With
    Dim RowIndex As Integer, i As Integer
    For i = 1 To MSH1.Rows - 1
        MSH1.RowHeight(i) = 240
    Next i

    Dim cIndex As cIndex
    Dim sInner As String, sOuter As String
    RowIndex = 0
    For i = 1 To colCostCAtDHull.Count
        RowIndex = RowIndex + 1
        MSH1.TextMatrix(RowIndex, 0) = "Double Coque " & i
'        MSH1.Row = RowIndex: MSH1.col = 1
'        MSH1.CellBackColor = &H8000000F 'grey
'        MSH1.Row = RowIndex: MSH1.col = 2
'        MSH1.CellBackColor = &H8000000E 'white
'        MSH1.Row = RowIndex: MSH1.col = 3
'        MSH1.CellBackColor = &H8000000E 'white
        MSH1.TextMatrix(RowIndex, 1) = "-------"
        For Each cIndex In colCostCAtDHull.Item(i).InnerShell
            sInner = sInner & cIndex.Number & " "
        Next cIndex
        For Each cIndex In colCostCAtDHull.Item(i).OuterShell
            sOuter = sOuter & cIndex.Number & " "
        Next cIndex
        If sInner = "" Then sInner = "-------"
        If sOuter = "" Then sOuter = "-------"
        MSH1.TextMatrix(RowIndex, 2) = RTrim(sInner)
        MSH1.TextMatrix(RowIndex, 3) = RTrim(sOuter)
        sInner = ""
        sOuter = ""
    Next i

    For i = 1 To colPanel.Count
        Select Case colPanel.Item(i).cCostCAtMain.ID_PANNEAU
            Case CarlingueSerreHiloire
                        RowIndex = RowIndex + 1
                        MSH1.TextMatrix(RowIndex, 0) = "Carlingue-Serre-Hiloire"
                        MSH1.TextMatrix(RowIndex, 1) = i
                        MSH1.TextMatrix(RowIndex, 2) = "-------"
                        MSH1.TextMatrix(RowIndex, 3) = "-------"
'                        MSH1.Row = RowIndex: MSH1.col = 1
'                        MSH1.CellBackColor = &H8000000E 'grey
'                        MSH1.Row = RowIndex: MSH1.col = 2
'                        MSH1.CellBackColor = &H8000000F
'                        MSH1.Row = RowIndex: MSH1.col = 3
'                        MSH1.CellBackColor = &H8000000F
            Case Bouchain
                        RowIndex = RowIndex + 1
                        MSH1.TextMatrix(RowIndex, 0) = "Bouchain"
                        MSH1.TextMatrix(RowIndex, 1) = i
                        MSH1.TextMatrix(RowIndex, 2) = "-------"
                        MSH1.TextMatrix(RowIndex, 3) = "-------"
'                        MSH1.Row = RowIndex: MSH1.col = 1
'                        MSH1.CellBackColor = &H8000000E 'grey
'                        MSH1.Row = RowIndex: MSH1.col = 2
'                        MSH1.CellBackColor = &H8000000F
'                        MSH1.Row = RowIndex: MSH1.col = 3
'                        MSH1.CellBackColor = &H8000000F
            Case Epontille
                        RowIndex = RowIndex + 1
                        MSH1.TextMatrix(RowIndex, 0) = "Epontille"
                        MSH1.TextMatrix(RowIndex, 1) = i
                        MSH1.TextMatrix(RowIndex, 2) = "-------"
                        MSH1.TextMatrix(RowIndex, 3) = "-------"
'                        MSH1.Row = RowIndex: MSH1.col = 1
'                        MSH1.CellBackColor = &H8000000E 'grey
'                        MSH1.Row = RowIndex: MSH1.col = 2
'                        MSH1.CellBackColor = &H8000000F
'                        MSH1.Row = RowIndex: MSH1.col = 3
'                        MSH1.CellBackColor = &H8000000F
        End Select
    Next i
    For i = 1 To colPanel.Count
        Select Case colPanel.Item(i).cCostCAtMain.ID_PANNEAU
            Case NappePlane
                Select Case colPanel.Item(i).cCostCAtMain.IT_PANNEAU
                    Case BordeSimple
                        RowIndex = RowIndex + 1
                        MSH1.TextMatrix(RowIndex, 0) = "Bordé Simple" ' " & colPanel.Item(i).pNumber
'                        MSH1.Row = RowIndex: MSH1.col = 1
'                        MSH1.CellBackColor = &H8000000E 'grey
                        MSH1.TextMatrix(RowIndex, 1) = i
                        MSH1.TextMatrix(RowIndex, 2) = "-------"
                        MSH1.TextMatrix(RowIndex, 3) = "-------"
'                        MSH1.Row = RowIndex: MSH1.col = 2
'                        MSH1.CellBackColor = &H8000000F
'                        MSH1.Row = RowIndex: MSH1.col = 3
'                        MSH1.CellBackColor = &H8000000F
                End Select
        End Select
    Next i
    For i = 1 To colPanel.Count
        Select Case colPanel.Item(i).cCostCAtMain.ID_PANNEAU
            Case Virtual
                RowIndex = RowIndex + 1
                MSH1.TextMatrix(RowIndex, 0) = "Panneau Fictif" ' " & colPanel.Item(i).pNumber
'                MSH1.Row = RowIndex: MSH1.col = 1
'                MSH1.CellBackColor = &H8000000E 'grey
                MSH1.TextMatrix(RowIndex, 1) = i
                MSH1.TextMatrix(RowIndex, 2) = "-------"
                MSH1.TextMatrix(RowIndex, 3) = "-------"
'                MSH1.Row = RowIndex: MSH1.col = 2
'                MSH1.CellBackColor = &H8000000F
'                MSH1.Row = RowIndex: MSH1.col = 3
'                MSH1.CellBackColor = &H8000000F
        End Select
    Next i

    Exit Sub
MSH1_Step2Err:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmCostCAt: Sub MSH1_Step2")
End Sub

Private Function GetMSHStep2Rows() As Integer
    On Error GoTo GetMSHStep2RowsErr
    Dim i As Integer
    GetMSHStep2Rows = 0
    For i = 1 To colPanel.Count
        Select Case colPanel.Item(i).cCostCAtMain.ID_PANNEAU
            Case NappePlane ' = 1
                Select Case colPanel.Item(i).cCostCAtMain.IT_PANNEAU
                    Case BordeSimple ' = 3
                        GetMSHStep2Rows = GetMSHStep2Rows + 1
                End Select
            Case CarlingueSerreHiloire ' = 2
                GetMSHStep2Rows = GetMSHStep2Rows + 1
            Case Bouchain ' = 3
                GetMSHStep2Rows = GetMSHStep2Rows + 1
            Case Virtual ' = 4
                GetMSHStep2Rows = GetMSHStep2Rows + 1
            Case Epontille  ' = 5
                GetMSHStep2Rows = GetMSHStep2Rows + 1
            Case Else
        End Select
    Next i
    'Add DoubleHulls
    GetMSHStep2Rows = GetMSHStep2Rows + colCostCAtDHull.Count
    Exit Function
GetMSHStep2RowsErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmCostCAt: Function GetMSHStep2Rows")
End Function

Private Function GetMSHStep5Rows() As Integer
    On Error GoTo GetMSHStep5RowsErr
    Dim i As Integer
    GetMSHStep5Rows = 0
    For i = 1 To colPanel.Count
        Select Case colPanel.Item(i).cCostCAtMain.ID_PANNEAU
            Case CarlingueSerreHiloire ' = 2
                GetMSHStep5Rows = GetMSHStep5Rows + 1
            Case Else
        End Select
    Next i
    Exit Function
GetMSHStep5RowsErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmCostCAt: Function GetMSHStep5Rows")
End Function

Private Function GetMSHStep9Rows() As Integer
    On Error GoTo GetMSHStep9RowsErr
    Dim i As Integer
    GetMSHStep9Rows = 0
    For i = 1 To colPanel.Count
        Select Case colPanel.Item(i).cCostCAtMain.ID_PANNEAU
            Case NappePlane ' = 1
                GetMSHStep9Rows = GetMSHStep9Rows + 1
            Case CarlingueSerreHiloire
                GetMSHStep9Rows = GetMSHStep9Rows + 1
            Case Bouchain ' = 3
                GetMSHStep9Rows = GetMSHStep9Rows + 1
            Case Epontille  ' = 5
            Case Else
        End Select
    Next i
    Exit Function
GetMSHStep9RowsErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmCostCAt: Function GetMSHStep9Rows")
End Function

Private Function GetMSHStep8Rows() As Integer
    On Error GoTo GetMSHStep8RowsErr
    Dim i As Integer
    GetMSHStep8Rows = 0
    For i = 1 To colPanel.Count
        Select Case colPanel.Item(i).cCostCAtMain.ID_PANNEAU
            Case NappePlane ' = 1
                GetMSHStep8Rows = GetMSHStep8Rows + 1
            Case CarlingueSerreHiloire
                'GetMSHStep8Rows = GetMSHStep8Rows + 1
            Case Bouchain ' = 3
            Case Epontille  ' = 5
            Case Else
        End Select
    Next i
    Exit Function
GetMSHStep8RowsErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmCostCAt: Function GetMSHStep8Rows")
End Function

Private Function GetMSHStep7Rows() As Integer
    On Error GoTo GetMSHStep7RowsErr
    Dim i As Integer
    GetMSHStep7Rows = 0
    For i = 1 To colPanel.Count
        Select Case colPanel.Item(i).cCostCAtMain.ID_PANNEAU
            Case NappePlane ' = 1
                GetMSHStep7Rows = GetMSHStep7Rows + 1
            Case CarlingueSerreHiloire
                'GetMSHStep7Rows = GetMSHStep7Rows + 1
            Case Bouchain ' = 3
                GetMSHStep7Rows = GetMSHStep7Rows + 1
            Case Epontille  ' = 5
            Case Else
        End Select
    Next i
    Exit Function
GetMSHStep7RowsErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmCostCAt: Function GetMSHStep7Rows")
End Function

Private Function GetMSHStep4Rows() As Integer
    On Error GoTo GetMSHStep4RowsErr
    Dim i As Integer
    GetMSHStep4Rows = 0
    For i = 1 To colPanel.Count
        Select Case colPanel.Item(i).cCostCAtMain.ID_PANNEAU
            Case NappePlane ' = 1
                Select Case colPanel.Item(i).cCostCAtMain.IT_PANNEAU
                    Case BordeSimple ' = 3
                        GetMSHStep4Rows = GetMSHStep4Rows + 1
                End Select
            Case Bouchain ' = 3
                GetMSHStep4Rows = GetMSHStep4Rows + 1
            Case Epontille  ' = 5
            Case Else
        End Select
    Next i
    'Add DoubleHulls
    GetMSHStep4Rows = GetMSHStep4Rows + colCostCAtDHull.Count
    Exit Function
GetMSHStep4RowsErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmCostCAt: Function GetMSHStep4Rows")
End Function

Private Sub SetFramesInvisible()
    Dim i As Integer
    For i = 1 To frStep.Count
        frStep(i).Visible = False
    Next i
End Sub

Private Sub Form_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
    If Button = 2 Then
        'PopupMenu mnuViewFile
    End If
End Sub

Private Sub Form_Resize()
    Dim i As Integer
    Me.Width = 6315 * 1.4 + 1000 '* 1.71 '* 1.8 '* 1.73 '* 1.18
    Me.Height = 6255
    cmdCancel.Left = Me.Width - cmdCancel.Width - 150
    cmdCancel.Top = Me.Height - cmdCancel.Height - 500
    cmdNext.Left = cmdCancel.Left - cmdNext.Width - 100
    cmdNext.Top = Me.Height - cmdNext.Height - 500
    cmdBack.Left = cmdNext.Left - cmdBack.Width
    cmdBack.Top = Me.Height - cmdBack.Height - 500
    cmdFinish.Left = cmdCancel.Left - cmdNext.Width - 100
    cmdFinish.Top = Me.Height - cmdNext.Height - 500
    cmdEnregistrer.Left = cmdBack.Left - cmdEnregistrer.Width - 150
    cmdEnregistrer.Top = Me.Height - cmdEnregistrer.Height - 500

    For i = 1 To frStep.Count
        With frStep(i)
            .Left = 100
            .Top = 100
            .Width = Me.Width - 300 '6015
            .Height = 5055
        End With
    Next i
    With MSH1
        .Left = frStep(1).Left + 50
        .Top = frStep(1).Top + 150
        .Width = frStep(1).Width - 300
        .Height = frStep(1).Height - 1000
    End With
    cmdfr3AjouterTypePanneau.Left = MSH1.Left
    cmdfr3AjouterTypePanneau.Width = 1700 'MSH1.Width / 5 + 300
    cmdfr3AjouterNappe.Left = MSH1.Left
    cmdfr3AjouterNappe.Width = 1700 'MSH1.Width / 5 + 300
    With lblPage
        .Left = frStep(1).Left
        .Top = Me.Height - .Height * 2 - 500
    End With
End Sub

Private Sub Form_Unload(Cancel As Integer)
    Project.Item(ProjectIndex).colPanel.SetPanelsVisible
    Draw ProjectIndex
End Sub

Private Sub mnuAjouterPanneauBordeSimple_Click()
    Me.Hide
    setFunctionMode PANEL_TYPE_SIMPLE_SHELL
End Sub

Private Sub mnuAjouterPanneauBouchain_Click()
    Me.Hide
    setFunctionMode PANEL_TYPE_BILGE
End Sub

Private Sub mnuAjouterPanneauCarlingueSerreHiloire_Click()
    Me.Hide
    setFunctionMode PANEL_TYPE_PRIMARY_LONGITUDINAL
End Sub

Private Sub mnuAjouterPanneauDoubleCoque_Click()
    Me.Hide
    setFunctionMode PANEL_TYPE_INNERDHULL
End Sub

'Private Sub ViewDBInput()
'    On Error GoTo  ViewDBInputErr
'    Dim fso, f
'    Dim sFile As String, i As Integer
'    Set fso = CreateObject("Scripting.FileSystemObject")
'    sFile = Project.Item(ProjectIndex).sFileName
'    sFile = GetFilePath(sFile) & "dbinput.txt"
'    Set f = fso.OpenTextFile(sFile, ForWriting, TristateUseDefault)
'    Dim cPanel As cPanel
'    f.WriteLine colPanel.Count
'    f.WriteLine Project.Item(ProjectIndex).cHeader.cCostCAtMain.bReadFractionnement
'    f.WriteLine Project.Item(ProjectIndex).cHeader.cCostCAtMain.bReadAccesibilite
'    f.WriteLine Project.Item(ProjectIndex).cHeader.cCostCAtMain.bReadAtelier
'    f.WriteLine Project.Item(ProjectIndex).cHeader.cCostCAtMain.iNAM
'    f.WriteLine Project.Item(ProjectIndex).cHeader.cCostCAtMain.lPMB
'    f.WriteLine Project.Item(ProjectIndex).cHeader.cCostCAtMain.lDiversTempsTolier & _
'    vbTab & Project.Item(ProjectIndex).cHeader.cCostCAtMain.lDiversTempsSoudeur
'    Dim sNCI As String, sNANP As String, sID As String, _
'    sIT As String, sIP As String, sTypeTapes As String, _
'    sPAbouts As String
'    sNCI = "": sNANP = "": sID = "": sIT = "": sIP = ""
'    sTypeTapes = "": sPAbouts = ""
'    For Each cPanel In colPanel
'        sNCI = sNCI & cPanel.cCostCAtMain.iNCI & vbTab
'        sNANP = sNANP & cPanel.cCostCAtMain.iNANP & vbTab
'        sID = sID & cPanel.cCostCAtMain.ID_PANNEAU & vbTab
'        sIP = sIP & cPanel.cCostCAtMain.IP_PANNEAU & vbTab
'        sIT = sIT & cPanel.cCostCAtMain.IT_PANNEAU & vbTab
'        sTypeTapes = sTypeTapes & cPanel.cCostCAtMain.TypeTapes & vbTab
'        sPAbouts = sPAbouts & cPanel.cCostCAtMain.PositionAboutsLisses & vbTab
'    Next cPanel
'    f.WriteLine sNCI
'    f.WriteLine sNANP
'    f.WriteLine sID
'    f.WriteLine sIP
'    f.WriteLine sIT
'    f.WriteLine sTypeTapes
'    f.WriteLine sPAbouts
'    f.Close
'    Dim ShellFile As Long
'    If fso.FileExists(sFile) = True Then
'        ShellFile = Shell(Windows_Path & "notepad.exe" & " " & sFile, vbNormalFocus)
'    End If
'    Exit Sub
'ViewDBInputErr:
'    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmCostCAt: Sub ViewBDInput")
'End Sub
'
'Private Sub ViewDBAcces()
'On Error GoTo  ViewdbAccesErr
'    Dim fso, f
'    Dim sFile As String, i As Integer
'    Set fso = CreateObject("Scripting.FileSystemObject")
'    sFile = Project.Item(ProjectIndex).sFileName
'    sFile = GetFilePath(sFile) & "dbacces.txt"
'    Set f = fso.OpenTextFile(sFile, ForWriting, TristateUseDefault)
'    Dim cPanel As cPanel
'    f.WriteLine colPanel.Count
'    f.WriteLine NO_OPERATIONS
'    Dim s As String
'    s = ""
'    For i = 1 To NO_OPERATIONS
'        For Each cPanel In colPanel
'            s = s & (Format(Val_(cPanel.cCostCAtMain.colCostCAtOperations.Item(i).Accesibilite), "0.000")) & vbTab
'        Next cPanel
'        f.WriteLine s
'        s = ""
'    Next i
'    s = ""
'    For i = 1 To NO_OPERATIONS
'        For Each cPanel In colPanel
'            s = s & (Format(Val_(cPanel.cCostCAtMain.colCostCAtOperations.Item(i).Atelier), "0.000")) & vbTab
'        Next cPanel
'        f.WriteLine s
'        s = ""
'    Next i
'    f.Close
'    Dim ShellFile As Long
'    If fso.FileExists(sFile) = True Then
'        ShellFile = Shell(Windows_Path & "notepad.exe" & " " & sFile, vbNormalFocus)
'    End If
'    Exit Sub
'ViewdbAccesErr:
'    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmCostCAt: Sub ViewBDInput")
'End Sub
'
'Private Sub ViewDBFractionnement()
'    On Error GoTo  ViewDBFractionnementErr
'    Dim fso, f
'    Dim sFile As String, i As Integer
'    Set fso = CreateObject("Scripting.FileSystemObject")
'    sFile = Project.Item(ProjectIndex).sFileName
'    sFile = GetFilePath(sFile) & "dbfractionnement.txt"
'    Set f = fso.OpenTextFile(sFile, ForWriting, TristateUseDefault)
'    Dim cPanel As cPanel
'    f.WriteLine colPanel.Count
'    f.WriteLine NO_OPERATIONS
'    Dim sOperation As String
'    '----temp----
'    Dim sPan As String
'    sPan = vbTab
'    For i = 1 To NO_OPERATIONS
'        For Each cPanel In colPanel
'            sOperation = sOperation & (Format(Val_(cPanel.cCostCAtMain.colCostCAtOperations.Item(i).Fractionnement), "0.000")) & vbTab
'        Next cPanel
'        f.WriteLine sOperation
'        sOperation = ""
'    Next i
'    f.Close
'    Dim ShellFile As Long
'    If fso.FileExists(sFile) = True Then
'        ShellFile = Shell(Windows_Path & "notepad.exe" & " " & sFile, vbNormalFocus)
'    End If
'    Exit Sub
'ViewDBFractionnementErr:
'    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmCostCAt: Sub ViewBDInput")
'End Sub
'
'Private Sub ViewDBSoudures()
'    On Error GoTo  ViewDBSouduresErr
'    Dim fso, f
'    Dim sFile As String, i As Integer
'    Set fso = CreateObject("Scripting.FileSystemObject")
'    sFile = Project.Item(ProjectIndex).sFileName
'    sFile = GetFilePath(sFile) & "dbsoudures.txt"
'    Set f = fso.OpenTextFile(sFile, ForWriting, TristateUseDefault)
'    Dim cPanel As cPanel
'    f.WriteLine colPanel.Count
'    f.WriteLine NO_OPERATIONS_SOUDURE
'    Dim sOperation As String
'    '----temp----
'    Dim sPan As String
'    sPan = vbTab
'    For i = 1 To NO_OPERATIONS_SOUDURE
'        For Each cPanel In colPanel
'            sOperation = sOperation & Val_(cPanel.cCostCAtMain.colCostCAtOperations.Item(i).Soudures) & vbTab
'        Next cPanel
'        f.WriteLine sOperation
'        sOperation = ""
'    Next i
'    For i = 1 To NO_OPERATIONS_SOUDURE
'        For Each cPanel In colPanel
'            sOperation = sOperation & Format(Val_(cPanel.cCostCAtMain.colCostCAtOperations.Item(i).Gorges), "0.000") & vbTab
'        Next cPanel
'        f.WriteLine sOperation
'        sOperation = ""
'    Next i
'    f.Close
'    Dim ShellFile As Long
'    If fso.FileExists(sFile) = True Then
'        ShellFile = Shell(Windows_Path & "notepad.exe" & " " & sFile, vbNormalFocus)
'    End If
'
'    Exit Sub
'ViewDBSouduresErr:
'    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmCostCAt: Sub ViewDBSoudures")
'End Sub

Private Sub mnuAjouterPanneauFictif_Click()
    Me.Hide
    setFunctionMode PANEL_TYPE_VIRTUAL
End Sub

Private Sub mnuAjouterCoupleCarlingueSurNappe_Click()
    Me.Hide
    setFunctionMode ADD_COUPLE_GIRDER_ON_NAPPE
End Sub


' =============
' FLEXGRID EDIT
' =============

Private Sub MSH1_KeyDown(KeyCode As Integer, Shift As Integer)
    'If Len(MSH1) > 0 Then sBuffer = CDbl(MSH1)
    
    TxtEdit_KeyDown KeyCode, Shift
End Sub

Function grd_index(r As Integer, c As Integer)
    grd_index = c + MSH1.Cols * r
End Function

Private Sub MSH1_KeyPress(KeyAscii As Integer)
    If MSH1.Rows <= 1 Then Exit Sub
    If MSH1 = "-------" Then Exit Sub
    Select Case KeyAscii
        Case 3 'CTRL + C
            FlexCopy MSH1
            Exit Sub
        Case 22 'CTRL + V
            Select Case frStepIndex
                Case 1
                    Exit Sub
                Case 2
                    Exit Sub
                Case 3
                    Exit Sub
                Case 4
                Case 5
                Case 6
            End Select
            FlexPaste MSH1: SetData
            Exit Sub
        Case Else
    End Select
    
    Select Case frStepIndex
        Case 1
        Case 2
        Case 3
        Case 4
        Select Case MSH1.col
            Case 1, 3, 4, 5
                MSHFlexGridEdit MSH1, TxtEdit, KeyAscii
            Case 2, 6
                PopulateComboEdit
                MSHFlexGridEdit MSH1, Combo, KeyAscii
        End Select
        Case 5
            Select Case MSH1.col
                Case 1
                    MSHFlexGridEdit MSH1, TxtEdit, KeyAscii
                Case 2, 3
                    PopulateComboEdit
                    MSHFlexGridEdit MSH1, Combo, KeyAscii
            End Select
        Case 6
            Select Case MSH1.col
                Case 1, 2
                    MSHFlexGridEdit MSH1, TxtEdit, KeyAscii
                Case 3
                    PopulateComboEdit
                    MSHFlexGridEdit MSH1, Combo, KeyAscii
            End Select
        Case 7
            Select Case MSH1.col
                    Case 1, 4
                        MSHFlexGridEdit MSH1, TxtEdit, KeyAscii
                    Case 2, 3
                        PopulateComboEdit
                        MSHFlexGridEdit MSH1, Combo, KeyAscii
            End Select
        Case 8
            MSHFlexGridEdit MSH1, TxtEdit, KeyAscii
        Case 9
            MSHFlexGridEdit MSH1, TxtEdit, KeyAscii
    End Select
End Sub

Private Sub MSH1_DblClick()
    If MSH1.Rows <= 1 Then Exit Sub
    If MSH1 = "-------" Then Exit Sub
    Select Case frStepIndex
        Case 1
        Case 2
        Case 3
        Case 4
        Select Case MSH1.col
            Case 1, 3, 4, 5
                MSHFlexGridEdit MSH1, TxtEdit, 32
            Case 2, 6
                PopulateComboEdit
                MSHFlexGridEdit MSH1, Combo, 32
        End Select
        Case 5
            Select Case MSH1.col
                Case 1
                    MSHFlexGridEdit MSH1, TxtEdit, 32
                Case 2, 3
                    PopulateComboEdit
                    MSHFlexGridEdit MSH1, Combo, 32
            End Select
        Case 6
            Select Case MSH1.col
                Case 1, 2
                    MSHFlexGridEdit MSH1, TxtEdit, 32
                Case 3
                    PopulateComboEdit
                    MSHFlexGridEdit MSH1, Combo, 32
            End Select
        Case 7
            Select Case MSH1.col
                Case 1, 4
                    MSHFlexGridEdit MSH1, TxtEdit, 32
                Case 2, 3
                PopulateComboEdit
                    MSHFlexGridEdit MSH1, Combo, 32
            End Select
        Case 8
            MSHFlexGridEdit MSH1, TxtEdit, 32
        Case 9
            PopupMenu mnuAjouterSoudure
    End Select
End Sub

Private Sub PopulateComboEdit()
    Combo.Clear
    Select Case frStepIndex
        Case 1
        Case 2
        Case 3
        Case 4
            Select Case MSH1.col
                Case 2
                    Combo.AddItem "Discontinue"
                    Combo.AddItem "Continue"
                Case 6
                    Combo.AddItem "Profilés"
                    Combo.AddItem "T Synthétiques"
            End Select
        Case 5
            Select Case MSH1.col
                Case 2
                    Combo.AddItem "Simple"
                    Combo.AddItem "Double"
                Case 3
                    Combo.AddItem "< 1"
                    Combo.AddItem "> 1"
            End Select
        Case 6
            Select Case MSH1.col
                Case 3
                    Combo.AddItem "Discontinue"
                    Combo.AddItem "Continue"
            End Select
        Case 7
            Select Case MSH1.col
                Case 2
                    Combo.AddItem "Non-Etanches - Recouvrement"
                    Combo.AddItem "Non-Etanches - Encastrées"
                    Combo.AddItem "Etanches - Recouvrement"
                    Combo.AddItem "Etanches - Encastrées"
                Case 3
                    Combo.AddItem "Profilés"
                    Combo.AddItem "T Synthétiques"
            End Select
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
'            edt.Move MSHFlexgrid.Left + MSHFlexgrid.CellLeft + 100, _
'                MSHFlexgrid.Top + MSHFlexgrid.CellTop + 50, _
'                MSHFlexgrid.CellWidth + 8
            edt.Move MSHFlexgrid.Left + MSHFlexgrid.CellLeft, _
                MSHFlexgrid.Top + MSHFlexgrid.CellTop - 30, _
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
    Exit Sub
MSHFlexGridEditErr:
End Sub

Private Sub MSH1_Scroll()
    TxtEdit.Visible = False
    Combo.Visible = False
End Sub

Private Sub TxtEdit_GotFocus()
    sBuffer = MSH1
    irow = MSH1.Row
    icol = MSH1.col
End Sub

Private Sub TxtEdit_KeyPress(KeyAscii As Integer)
    If KeyAscii = Asc(vbCr) Then KeyAscii = 0
End Sub

Private Sub TxtEdit_KeyDown(KeyCode As Integer, Shift As Integer)
    If Len(MSH1) > 0 Then sBuffer = (MSH1)
    cmdOK.Default = False
    EditKeyCode MSH1, TxtEdit, KeyCode, Shift
End Sub

Sub EditKeyCode(MSHFlexgrid As Control, edt As Control, KeyCode As Integer, Shift As Integer)
    Select Case KeyCode
    Case 27 ' esc
        edt.Visible = False
        MSHFlexgrid.SetFocus
    Case 13 'enter
        If TxtEdit.Visible = True Then
            TxtEdit_Validate False
        ElseIf Combo.Visible = True Then
            'Combo_Validate False
        End If
        MSHFlexgrid.SetFocus
    Case 38 'up
        If TxtEdit.Visible = True Then
            TxtEdit_Validate False
        ElseIf Combo.Visible = True Then
            'Combo_Validate False
        End If
        MSHFlexgrid.SetFocus
        'DoEvents
        If MSHFlexgrid.Row > MSHFlexgrid.FixedRows Then
           MSHFlexgrid.Row = MSHFlexgrid.Row - 1
        End If
    Case 40 'down
        If TxtEdit.Visible = True Then
            TxtEdit_Validate False
        ElseIf Combo.Visible = True Then
            'Combo_Validate False
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

Private Sub TxtEdit_Validate(Cancel As Boolean)
    Select Case frStepIndex
        Case 9 'ID Soudures
            If ValidateID_Soudure(TxtEdit) = False Then
                MsgBox "ID de Soudure inconu.", vbCritical + vbOKOnly
                TxtEdit = sBuffer
            End If
        Case Else
            ValidateNumeric TxtEdit, Cancel
            If Cancel = True Then TxtEdit = sBuffer
    End Select
End Sub

Private Sub txtNAM_GotFocus()
    txtNAM.SelStart = 0
    txtNAM.SelLength = Len(txtNAM.Text)
End Sub

Private Sub txtNoIndicesDC_GotFocus()
    txtNoIndicesDC.SelStart = 0
    txtNoIndicesDC.SelLength = Len(txtNoIndicesDC.Text)
End Sub

Private Sub txtPMB_GotFocus()
    txtPMB.SelStart = 0
    txtPMB.SelLength = Len(txtPMB.Text)
End Sub
