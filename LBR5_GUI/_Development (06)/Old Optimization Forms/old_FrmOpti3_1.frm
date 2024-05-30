VERSION 5.00
Object = "{831FDD16-0C5C-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCTL.OCX"
Begin VB.Form old_FrmOpti3_1 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Geometrical Constraints"
   ClientHeight    =   9030
   ClientLeft      =   6225
   ClientTop       =   1785
   ClientWidth     =   6810
   ForeColor       =   &H00404040&
   HasDC           =   0   'False
   Icon            =   "old_FrmOpti3_1.frx":0000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form2"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   9030
   ScaleWidth      =   6810
   ShowInTaskbar   =   0   'False
   Begin VB.ListBox ReferenceList 
      Appearance      =   0  'Flat
      Height          =   1785
      Left            =   120
      TabIndex        =   3
      Top             =   6240
      Width           =   6495
   End
   Begin VB.CommandButton cmdExpand 
      Caption         =   "Expand List >>"
      Height          =   345
      Left            =   120
      TabIndex        =   7
      Top             =   8160
      Width           =   1455
   End
   Begin VB.CommandButton CmdReferenceList 
      Caption         =   "References >>"
      Height          =   345
      Left            =   120
      TabIndex        =   6
      Top             =   5520
      Width           =   1455
   End
   Begin VB.ListBox AvailableList 
      Appearance      =   0  'Flat
      ForeColor       =   &H00C00000&
      Height          =   3630
      ItemData        =   "old_FrmOpti3_1.frx":000C
      Left            =   1920
      List            =   "old_FrmOpti3_1.frx":000E
      Sorted          =   -1  'True
      Style           =   1  'Checkbox
      TabIndex        =   5
      Top             =   960
      Width           =   4695
   End
   Begin MSComctlLib.TabStrip TabStrip 
      Height          =   4335
      Left            =   1680
      TabIndex        =   4
      Top             =   480
      Width           =   5055
      _ExtentX        =   8916
      _ExtentY        =   7646
      MultiRow        =   -1  'True
      HotTracking     =   -1  'True
      _Version        =   393216
      BeginProperty Tabs {1EFB6598-857C-11D1-B16A-00C0F0283628} 
         NumTabs         =   1
         BeginProperty Tab1 {1EFB659A-857C-11D1-B16A-00C0F0283628} 
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
   Begin VB.CommandButton cmdClose 
      Caption         =   "&Close"
      Height          =   345
      Left            =   5520
      TabIndex        =   2
      Top             =   5520
      Width           =   1125
   End
   Begin VB.CommandButton cmdApply 
      Caption         =   "&Apply"
      Default         =   -1  'True
      Height          =   345
      Left            =   4320
      TabIndex        =   1
      Top             =   5520
      Width           =   1125
   End
   Begin VB.ListBox PasteList 
      Appearance      =   0  'Flat
      ForeColor       =   &H00C00000&
      Height          =   4320
      Left            =   120
      MultiSelect     =   2  'Extended
      TabIndex        =   8
      Top             =   480
      Width           =   1455
   End
   Begin VB.ListBox PanelList 
      Appearance      =   0  'Flat
      Height          =   4320
      Left            =   120
      TabIndex        =   0
      Top             =   480
      Width           =   1455
   End
   Begin VB.CommandButton Command1 
      Caption         =   "Show Figure"
      Height          =   345
      Left            =   1680
      TabIndex        =   10
      Top             =   5520
      Width           =   1335
   End
   Begin VB.Frame Frame1 
      Appearance      =   0  'Flat
      ForeColor       =   &H80000008&
      Height          =   615
      Left            =   120
      TabIndex        =   11
      Top             =   4800
      Width           =   6615
      Begin VB.ComboBox Combo2 
         Height          =   315
         Left            =   5280
         Style           =   2  'Dropdown List
         TabIndex        =   14
         Top             =   240
         Width           =   1215
      End
      Begin VB.ComboBox Combo1 
         Height          =   315
         Left            =   2880
         Style           =   2  'Dropdown List
         TabIndex        =   13
         Top             =   240
         Width           =   1215
      End
      Begin VB.Label Label7 
         AutoSize        =   -1  'True
         Caption         =   "Frames: "
         Height          =   195
         Left            =   2160
         TabIndex        =   16
         Top             =   240
         Width           =   600
      End
      Begin VB.Label Label8 
         AutoSize        =   -1  'True
         Caption         =   "Stiffeners: "
         Height          =   195
         Left            =   4440
         TabIndex        =   15
         Top             =   240
         Width           =   750
      End
      Begin VB.Label Label2 
         Caption         =   "Automatic update of the flange thickness: "
         Height          =   375
         Left            =   240
         TabIndex        =   12
         Top             =   120
         Width           =   1650
      End
   End
   Begin VB.Label Label1 
      AutoSize        =   -1  'True
      Caption         =   "Label1"
      Height          =   195
      Left            =   120
      TabIndex        =   9
      Top             =   120
      Width           =   480
   End
   Begin VB.Menu mnuCopy 
      Caption         =   "Copy"
      Begin VB.Menu CopySel 
         Caption         =   "Copy"
      End
   End
   Begin VB.Menu mnuPaste 
      Caption         =   "Paste"
      Begin VB.Menu PasteSel 
         Caption         =   "Paste"
      End
   End
End
Attribute VB_Name = "old_FrmOpti3_1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
'Option Base 1
Public v As String
Dim vM2() As Integer
Dim vGeom() As Variant
Dim vGeom0() As Integer
Dim vGeom1() As Integer
Dim vGeom2() As Integer
Dim vGeom3() As Integer
Dim Index0 As Integer
Dim Index1 As Integer
Dim Index2 As Integer
Dim Index3 As Integer
Dim Index99 As Integer
Dim vGeomtmp() As Integer
Dim vGeomSet() As Integer
Dim vISEMA() As Integer
Dim vISEMR() As Integer
Dim vRefNumber() As Variant
Dim vToPaste() As Variant
Dim PanelIndex As Integer
Dim REFstate As Boolean
Dim ExpandState As Boolean
Dim MsgCount As Integer
Dim ProjectIndex As Integer
Dim NETO As Integer

Private Sub AvailableList_ItemCheck(Item As Integer)
Dim i As Integer, index As Integer
index = 0
'    Select Case TabStrip.SelectedItem
'        Case "Stiffeners"
'            For i = 1 To AvailableList.ListCount
'                If AvailableList.Selected(i - 1) = False Then
'                    index = index + 1
'                End If
'            Next i
'            If index = AvailableList.ListCount Then
'                Combo1.Text = "0: Fixed"
'                Combo1.Enabled = False
'            Else
'                Combo1.Enabled = True
'            End If
'        Case "Frames"
'            For i = 1 To AvailableList.ListCount
'                If AvailableList.Selected(i - 1) = False Then
'                    index = index + 1
'                End If
'            Next i
'            If index = AvailableList.ListCount Then
'                Combo2.Text = "0: Fixed"
'                Combo2.Enabled = False
'            Else
'                Combo2.Enabled = True
'            End If
'        Case "Stiffeners - Frames Interaction"
'        Case "Sets"
'            For i = 1 To AvailableList.ListCount
'                If AvailableList.Selected(i - 1) = True Then
'                    index = index + 1
'                End If
'            Next i
'            If index > 0 Then
'                'Combo1.Text = "0: Fixed"
'                Combo1.Enabled = False
'                'Combo2.Text = "0: Fixed"
'                Combo2.Enabled = False
'            End If
'    End Select
    
    Select Case TabStrip.SelectedItem
        Case "Sets"
            If Item > -1 Then
                For i = 1 To 5
                    AvailableList.ItemData(i - 1) = 0
                Next i
                AvailableList.ItemData(Item) = 1
                For i = 1 To 5
                    If AvailableList.ItemData(i - 1) = 1 Then
                        If Item = i - 1 Then
                            If PanelIndex > 0 Then
                                vM2(PanelIndex) = 0
                            End If
                        Else
                            AvailableList.Selected(i - 1) = True
                        End If
                    ElseIf AvailableList.ItemData(i - 1) = 0 Then
                        AvailableList.Selected(i - 1) = False
                    End If
                Next i
            End If
        Case Else
    End Select
    If Item > -1 Then
        If AvailableList.Selected(Item) = True Then
            AvailableList.ItemData(Item) = 1
        Else
            AvailableList.ItemData(Item) = 0
        End If
    End If
    
    'Compute
    PopulateCombo
End Sub

Private Sub AvailableList_MouseDown(Button As Integer, Shift As Integer, x As Single, y As Single)
    If Button = 2 Then
        If PanelIndex > 0 Then
            PopupMenu mnuCopy
        End If
    End If
End Sub

Private Sub cmdApply_Click()
Compute
Dim i As Integer
Dim j As Integer

Dim GetTotalNumberOfGeoConstr As Integer
For i = 1 To NETO
    GetTotalNumberOfGeoConstr = 0
    If vM2(i) = 99 Then
        GetTotalNumberOfGeoConstr = GetTotalNumberOfGeoConstr + 1
    Else
        GetTotalNumberOfGeoConstr = GetTotalNumberOfGeoConstr + vM2(i)
    End If
    If vISEMA(i) > 0 Then
        GetTotalNumberOfGeoConstr = GetTotalNumberOfGeoConstr + 1
    End If
    If vISEMR(i) > 0 Then
        GetTotalNumberOfGeoConstr = GetTotalNumberOfGeoConstr + 1
    End If
    If GetTotalNumberOfGeoConstr > Licensing.MAX_GEOMETRICAL_CONSTRAINTS_ON_PANEL Then
        MsgBox "The maximum number of geometrical constraints is restricted to " & Licensing.MAX_GEOMETRICAL_CONSTRAINTS_ON_PANEL & " on each panel.", vbCritical + vbOKOnly, "LBR-5 License Limitation"
        Exit Sub
    End If
Next i
Dim cGeometricalConstraints As cGeometricalConstraints
    For i = 1 To NETO
        Project.Item(ProjectIndex).colPanel.Item(i).NoOfGeomConstr = vM2(i)
        Set Project.Item(ProjectIndex).colPanel.Item(i).colGeometricalConstraints = Nothing
        If vM2(i) = 99 Then
            Set cGeometricalConstraints = New cGeometricalConstraints
            cGeometricalConstraints.index = 1
            cGeometricalConstraints.code = vGeomSet(i)
            Project.Item(ProjectIndex).colPanel.Item(i).colGeometricalConstraints.Add cGeometricalConstraints, 1
            Set cGeometricalConstraints = Nothing
        ElseIf vM2(i) > 0 Then
            For j = 1 To vM2(i)
                Set cGeometricalConstraints = New cGeometricalConstraints
                cGeometricalConstraints.index = j
                cGeometricalConstraints.code = vGeom(i)(j)
                Project.Item(ProjectIndex).colPanel.Item(i).colGeometricalConstraints.Add cGeometricalConstraints, j
                Set cGeometricalConstraints = Nothing
            Next j
        End If
        Project.Item(ProjectIndex).colPanel.Item(i).FramesFlangeThicknessUpdate = vISEMA(i)
        Project.Item(ProjectIndex).colPanel.Item(i).StiffenersFlangeThicknessUpdate = vISEMR(i)
    Next i
'    For i = 1 To NETO
'        Panel(i).M2 = vM2(i)
'        Panel(i).GeomConstr = vGeom(i)
'        Panel(i).GeomSet = vGeomSet(i)
'        Panel(i).ISEMA = vISEMA(i)
'        Panel(i).ISEMR = vISEMR(i)
'    Next i
Project.Item(ProjectIndex).DataChanged = True
End Sub

Private Sub cmdExpand_Click()
    If ExpandState = True Then
        cmdExpand.Caption = "Expand List >>"
        ReferenceList.Top = 6240
        ReferenceList.Height = 1815
        ExpandState = False
        Me.Width = 6930 '6420
        ReferenceList.Width = 6495 '6015
        Label1.Visible = True
    Else
        cmdExpand.Caption = "<< Restore List"
        ReferenceList.Top = PanelList.Top
        ReferenceList.Height = 6240 + 1230
        ExpandState = True
        Me.Width = 10800
        ReferenceList.Width = 10800 - 450
        Label1.Visible = False
    End If
End Sub

Private Sub CmdReferenceList_Click()
    If REFstate = True Then
        CmdReferenceList.Caption = "References >>"
        Me.Height = 6585
        REFstate = False
    Else
        CmdReferenceList.Caption = "<< References"
        Me.Height = 9240
        REFstate = True
    End If
End Sub

Private Sub Combo1_Click()
    Combo1_LostFocus
End Sub

Private Sub Combo1_LostFocus()
If PanelIndex > 0 Then
    If vM2(PanelIndex) > 0 Then
        vISEMA(PanelIndex) = Combo1.ListIndex
    End If
End If

End Sub

Private Sub Combo2_Click()
    Combo2_LostFocus
End Sub

Private Sub Combo2_LostFocus()
If PanelIndex > 0 Then
    If vM2(PanelIndex) > 0 Then
        vISEMR(PanelIndex) = Combo2.ListIndex
    End If
End If
End Sub

Private Sub Command1_Click()
Load old_frmGeomConstrFig
old_frmGeomConstrFig.Show vbModeless, Me
End Sub

Private Sub CopySel_Click()
Compute
    PasteList.Visible = True
Label1.Visible = True
Label1.Caption = "Panel " & PanelIndex & " selected as source; Press Escape to cancel."
'    Dim i As Integer
'    Dim Index As Integer
'    Index = 0
'    For i = 1 To AvailableList.ListCount
'        If AvailableList.ItemData(i - 1) = 1 Then
'            Index = Index + 1
'            ReDim Preserve vToPaste(1 To Index)
'            V = AvailableList.List(i - 1)
'            GetRefNumber V
'            vToPaste(Index) = val_(V)
'            MsgBox vToPaste(Index)
'        End If
'    Next i
End Sub

Private Sub Form_KeyPress(KeyAscii As Integer)
    If KeyAscii = vbKeyEscape Then
        PasteList.Visible = False
        Label1.Caption = "Panels:"
    End If
End Sub

Private Sub TabProperties()
    TabStrip.Tabs.Remove 1
    If Project.Item(ProjectIndex).cHeader.IANA = 1 Then
        TabStrip.Tabs.Add 1, "Stiffeners", "Stiffeners"
        TabStrip.Tabs.Add 2, "Frames", "Frames"
        TabStrip.Tabs.Add 3, "Stiffeners_Frames", "Stiffeners - Frames Interaction"
        TabStrip.Tabs.Add 4, "Sets", "Sets"
    Else
        TabStrip.Tabs.Add 1, "Stiffeners", "Stiffeners"
    End If
End Sub

Private Sub Form_Load()
    If Licensing.IS_MODIFY_GEOMETRICAL_CONSTRAINTS = False Then
        cmdApply.Enabled = False
    End If
    ProjectIndex = ActiveProject
    Me.Move (Screen.Width - Me.Width) / 2, (Screen.Height - Me.Height) / 2
    TabProperties
    If Project.Item(ProjectIndex).cHeader.IANA = 2 Then
        Combo1.Visible = False
        Label7.Visible = False
    End If
'    cmdOK.Picture = LoadResPicture("ID_BITMAP_OK", 0)
'    cmdCancel.Picture = LoadResPicture("ID_BITMAP_CANCEL", 0)
    Me.Caption = "Geometrical Constraints - [" & GetFileName(Project.Item(ProjectIndex).sFileName) & "]"
    NETO = Project.Item(ProjectIndex).colPanel.Count
    Dim i As Integer

    REFstate = False
    ExpandState = False
    Me.Height = 6585
    ReferenceList.Top = 6240
    ReferenceList.Height = 1815
    RefList
    PopulatePanelList
    PopulatePasteList
    Label1.Caption = "Panels:"
    PasteList.Visible = False
    GetData
    RefList
    PopulateAvailableList
    AvailableList.ListIndex = -1
    'PanelList.ListIndex = 0
    PanelIndex = PanelList.ListIndex + 1

    Combo1.Enabled = False
    Combo2.Enabled = False
    PopulateCombo
    mnuCopy.Visible = False
    mnuPaste.Visible = False
    

'    Compute
'
    PanelList.ListIndex = 0
    

End Sub


Private Sub GetData()
    Dim i As Integer
    Dim j As Integer
    ReDim vM2(1 To NETO) As Integer
    ReDim vGeom(1 To NETO)
    ReDim vGeomSet(1 To NETO) As Integer
    ReDim vISEMA(1 To NETO)
    ReDim vISEMR(1 To NETO)
    Dim vGeomtmp() As Variant
    
    For i = 1 To NETO
        vM2(i) = Project.Item(ProjectIndex).colPanel.Item(i).NoOfGeomConstr
        If vM2(i) = 99 Then
            vGeomSet(i) = Project.Item(ProjectIndex).colPanel.Item(i).colGeometricalConstraints.Item(1).code
        ElseIf vM2(i) > 0 Then
            vGeomSet(i) = 0
            ReDim vGeomtmp(1 To vM2(i))
            For j = 1 To vM2(i)
                vGeomtmp(j) = Project.Item(ProjectIndex).colPanel.Item(i).colGeometricalConstraints.Item(j).code
            Next j
            vGeom(i) = vGeomtmp
        End If
        vISEMA(i) = Project.Item(ProjectIndex).colPanel.Item(i).FramesFlangeThicknessUpdate
        vISEMR(i) = Project.Item(ProjectIndex).colPanel.Item(i).StiffenersFlangeThicknessUpdate
    Next i
    
'        For i = 1 To NETO
'            vM2(i) = Panel(i).M2
'            vGeom(i) = Panel(i).GeomConstr
'            vGeomSet(i) = Panel(i).GeomSet
'            vISEMA(i) = Panel(i).ISEMA
'            vISEMR(i) = Panel(i).ISEMR
'        Next i
    ReDim vGeom0(1 To 1)
        For i = 1 To NETO
            If IsEmpty(vGeom(i)) Then
                vGeom(i) = vGeom0
            End If
        Next i
End Sub

Private Sub cmdClose_Click()
    Unload Me
End Sub

Private Sub PopulatePanelList()
    PanelList.Clear
    Dim i As Integer
    For i = 1 To NETO
        PanelList.AddItem "Panel " & i
    Next i
End Sub

Private Sub PopulatePasteList()
    PasteList.Clear
    Dim i As Integer
    For i = 1 To NETO
        PasteList.AddItem "Panel " & i
    Next i
End Sub
Private Sub PopulateAvailableList()

    Select Case TabStrip.SelectedItem
        Case "Stiffeners"
            StiffenersSet
        Case "Frames"
            FramesSet
        Case "Stiffeners - Frames Interaction"
            StiffenersFramesInteractionSet
        Case "Sets"
            SetsSet
    End Select
End Sub

Private Sub StiffenersSet()
    AvailableList.Clear
    'AvailableList.AddItem "206. Tf <= 2 * Tw"
    'AvailableList.AddItem "207. 8 * Tf <= Df"
    'AvailableList.AddItem "208. Df <= 32 * Tf"
    AvailableList.AddItem "204. d <= 2 * Tw"
    AvailableList.AddItem "212. Tw <= 2 * d"
    AvailableList.AddItem "201. Df <= Dw"
    AvailableList.AddItem "202. Dw <= 2 * Df"
    AvailableList.AddItem "205. 3 * d <= Dw"
    AvailableList.AddItem "209. 0.625 * Df <= Dw"
    AvailableList.AddItem "210. Dw <= 2.5 * Df"
    AvailableList.AddItem "215. 1.25 * Df <= Dw"
    AvailableList.AddItem "216. Dw <= 5 * Df"
    AvailableList.AddItem "203. Dw <= 40 * Tw"
    AvailableList.AddItem "206. Dmin (position code - Mars2000)"
    AvailableList.AddItem "211. Dw <= 36 * Tw"
    AvailableList.AddItem "213. Dw <= 120 * Tw "
    AvailableList.AddItem "214. 0.0065 + Dw / 170 <= Tw"
    'Restrictions DNV
    AvailableList.AddItem "220. 8 <= Dw / Tw"
    AvailableList.AddItem "221. 50 >= Dw / Tw"
    'Restrictions DNV for Girders
    AvailableList.AddItem "230. 0.2 <= Df / Dw (only for girders)"
    AvailableList.AddItem "231. 0.5 >= Df / Dw (only for girders)"
    AvailableList.AddItem "232. 28 <= Dw / Tw (only for girders)"
    AvailableList.AddItem "233. 90 >= Dw / Tw (only for girders)"
    AvailableList.AddItem "234. 8 <= Tw (only for girders)"
    AvailableList.AddItem "235. 20 >= Tw (only for girders)"
    AvailableList.AddItem "236. 0.1 <= Df / Dw (only for girders)"
    AvailableList.AddItem "237. 130 >= Dw / Tw (only for girders)"
    'Restrictions DNV for Girders located in the symmetry axis
    AvailableList.AddItem "240. 0.1 <= Df / Dw (only for girders on the symmetry axis)"
    AvailableList.AddItem "241. 0.25 >= Df / Dw (only for girders on the symmetry axis)"
    AvailableList.AddItem "242. 56 <= Dw / Tw (only for girders on the symmetry axis)"
    AvailableList.AddItem "243. 180 >= Dw / Tw (only for girders on the symmetry axis)"
    AvailableList.AddItem "244. 4 <= Tw (only for girders on the symmetry axis)"
    AvailableList.AddItem "245. 10 >= Tw (only for girders on the symmetry axis)"
    AvailableList.AddItem "246. 0.05 <= Df / Dw (only for girders on the symmetry axis)"
    AvailableList.AddItem "247. 260 >= Dw / Tw (only for girders on the symmetry axis)"
End Sub

Private Sub FramesSet()
    AvailableList.Clear
    'AvailableList.AddItem "106. Tf <= 2 * Tw"
    'AvailableList.AddItem "107. 8 * Tf <= Df"
    'AvailableList.AddItem "108. Df <= 32 * Tf"
    AvailableList.AddItem "104. d <= 2 * Tw"
    AvailableList.AddItem "112. Tw <= 2 * d"
    AvailableList.AddItem "105. 3 * d <= Dw"
    AvailableList.AddItem "101. Df <= Dw"
    AvailableList.AddItem "102. Dw <= 2 * Df "
    AvailableList.AddItem "109. 0.625 * Df <= Dw"
    AvailableList.AddItem "110. Dw <= 2.5 * Df"
    AvailableList.AddItem "115. 1.25 * Df <= Dw"
    AvailableList.AddItem "116. Dw <= 5 * Df"
    AvailableList.AddItem "111. Dw <= 120 * Tw"
    AvailableList.AddItem "103. 0.0065 + Dw / 170 <= Tw"
    AvailableList.AddItem "113. Dw <= 36 * Tw"
    AvailableList.AddItem "114. Dw <= 40 * Tw"
    'Restrictions DNV
    AvailableList.AddItem "120. 0.2 <= Df / Dw"
    AvailableList.AddItem "121. 0.5 >= Df / Dw"
    AvailableList.AddItem "122. 28 <= Dw / Tw"
    AvailableList.AddItem "123. 90 >= Dw / Tw"
    
    
End Sub

Private Sub StiffenersFramesInteractionSet()
    AvailableList.Clear
    AvailableList.AddItem "301. Dw(st) <= Dw(fr)"
    AvailableList.AddItem "304. Dw(fr) <= Dw(st)"
    AvailableList.AddItem "302. Tw(st) <= 4 * Tw(fr)"
    AvailableList.AddItem "303. Tw(fr) <= 4 * Tw(st)"
    ' Restrictions DNV
    AvailableList.AddItem "320. 2 <= Dw(fr) / Dw(st)"
    
    
End Sub

Private Sub SetsSet()
    AvailableList.Clear
    AvailableList.AddItem "1. Set of Hughes (for T members)"
    AvailableList.AddItem "2. Set of Hughes (extended)"
    AvailableList.AddItem "3. Set of Hughes (for L members)"
    AvailableList.AddItem "4. Set of Rahman (barges, inland navigation)"
    AvailableList.AddItem "5. Set of Rahman (extended)"
End Sub

Private Sub RefList()
    ReferenceList.Clear
    If Project.Item(ProjectIndex).cHeader.IANA = 1 Then
        ReferenceList.AddItem "    1. Set of Hughes (for T members); 109, 110, 111, 112 (frames); 209, 210, 211, 212 (stiffeners);" & _
        " 301, 302, 303 (jonction frames-stiffeners)"
        ReferenceList.AddItem "    2. Set of Hughes (extended); Set 1 + 104, 105, 204, 205"
        ReferenceList.AddItem "    3. Set of Hughes (for L members); 115, 116, 111, 112 (frames); 215, 216, 211, 212 (stiffeners);" & _
        " 301, 302, 303 (jonction frames-stiffeners)"
        ReferenceList.AddItem "    4. Set of Rahman (barge, inland navigation); 101, 102, 103 (frames); 201, 202, 203 (stiffeners)"
        ReferenceList.AddItem "    5. Set of Rahman (extended); Set 4 + 104, 105, 204, 205"
        ReferenceList.AddItem "106. Flange of frames: Tf - 2 * Tw <= 0"
        ReferenceList.AddItem "107. Flange of frames: 8 * Tf - Df <= 0"
        ReferenceList.AddItem "108. Flange of frames: Df - 16 * Tf <= 0 (L profiles), Df - 32 * Tf <= 0 (T profile)"
        ReferenceList.AddItem "206. Flange of stiffeners (longitudinals): Tf - 2 * Tw <= 0"
        ReferenceList.AddItem "207. Flange of stiffeners (longitudinals): 8 * Tf - Df <= 0"
        ReferenceList.AddItem "208. Flange of stiffeners (longitudinals): Df - 16 * Tf <= 0 (L profiles), Df - 32 * Tf <= 0 (T profile)"
        ReferenceList.AddItem "104. Frames; plate thickness / web thickness ratio: d - 2 * Tw <= 0"
        ReferenceList.AddItem "112. Frames; plate thickness / web thickness ratio: Tw - 2 * d <= 0"
        ReferenceList.AddItem "105. Frames; minimal web height (minimal frame stiffness): 3 * d - Dw <= 0"
        ReferenceList.AddItem "101. Frames; flange width / web height ratio: Df - Dw <= 0 (Rahman set)"
        ReferenceList.AddItem "102. Frames; flange width / web height ratio: Dw - 2 * Df <= 0 (Rahman set)"
        ReferenceList.AddItem "109. Frames; flange width / web height ratio: 0.625 * Df - Dw <= 0 (for T shape member - Hughes set)"
        ReferenceList.AddItem "110. Frames; flange width / web height ratio: Dw - 2.5 * Df <= 0 (for T shape member - Hughes set)"
        ReferenceList.AddItem "115. Frames; flange width / web height ratio: 1.25 * Df - Dw <= 0 (for L shape member - Hughes set)"
        ReferenceList.AddItem "116. Frames; flange width / web height ratio: Dw - 5 * Df <= 0 (for L shape member - Hughes set)"
        ReferenceList.AddItem "111. Frames; web slenderness: Dw - 120 * Tw <= 0 (for web supported by smaller stiffeners - Hughes)"
        ReferenceList.AddItem "103. Frames; web slenderness: 0.0065 + Dw / 170 - Tw <= 0 (for barges - Lloyds Register Rules, Rahman), not recommended"
        ReferenceList.AddItem "113. Frames; web slenderness: Dw - 36 * Tw <= 0 (for stiffener height > frame height)"
        ReferenceList.AddItem "114. Frames; web slenderness: Dw - 40 * Tw <= 0 (for stiffener height > frame height)"
        
        ' ---DNV---
        ReferenceList.AddItem "120. Frames; flange width / web height ratio: 0.2 <= Df / Dw"
        ReferenceList.AddItem "121. Frames; flange width / web height ratio: 0.5 >= Df / Dw"
        ReferenceList.AddItem "122. Frames; web slenderness: 28 <= Dw / Tw"
        ReferenceList.AddItem "123. Frames; web slenderness: 90 >= Dw / Tw"
        ' -----\----
        
        ReferenceList.AddItem "204. Stiffeners; plate thickness / web thickness ratio: d - 2 * Tw <= 0"
        ReferenceList.AddItem "212. Stiffeners; plate thickness / web thickness ratio: Tw - 2 * d <= 0"
        ReferenceList.AddItem "205. Stiffeners; minimal web height (minimal frame stiffness): 3 * d - Dw <= 0"
        ReferenceList.AddItem "201. Stiffeners; flange width / web height ratio: Df - Dw <= 0 (Rahman set)"
        ReferenceList.AddItem "202. Stiffeners; flange width / web height ratio: Dw - 2 * Df <= 0 (Rahman set)"
        ReferenceList.AddItem "209. Stiffeners; flange width / web height ratio: 0.625 * Df - Dw <= 0 (for T shape member - Hughes set)"
        ReferenceList.AddItem "210. Stiffeners; flange width / web height ratio: Dw - 2.5 * Df <= 0 (for T shape member - Hughes set)"
        ReferenceList.AddItem "215. Stiffeners; flange width / web height ratio: 1.25 * Df - Dw <= 0 (for L shape member - Hughes set)"
        ReferenceList.AddItem "216. Stiffeners; flange width / web height ratio: Dw - 5 * Df <= 0 (for L shape member - Hughes set)"
        ReferenceList.AddItem "203. Stiffeners; web slenderness: Dw - 40 * Tw <= 0 (frames taller than stiffeners - stiffener web not supported, Rahman)"
        ReferenceList.AddItem "211. Stiffeners; web slenderness: Dw - 36 * Tw <= 0 (frames taller than stiffeners - stiffener web not supported, Rahman)"
        ReferenceList.AddItem "213. Stiffeners; web slenderness: Dw - 120 * Tw <= 0 (stiffeners taller than frames- stiffener web not supported, Rahman)"
        ReferenceList.AddItem "214. Stiffeners; web slenderness: 0.0065 + Dw / 170 - Tw <= 0 (frames taller than stiffeners - stiffener web not supported, Rahman)"
        
        ' ---DNV---
        ReferenceList.AddItem "220. Stiffeners; web slenderness: 8 <= Dw / Tw"
        ReferenceList.AddItem "221. Stiffeners; web slenderness: 50 >= Dw / Tw"
        ReferenceList.AddItem "230. Girders; flange width / web height ratio: 0.2 <= Df / Dw (to use with stiffener emulating a girder)"
        ReferenceList.AddItem "231. Girders; flange width / web height ratio: 0.5 >= Df / Dw (to use with stiffener emulating a girder)"
        ReferenceList.AddItem "232. Girders; web slenderness: 28 <= Dw / Tw (to use with stiffener emulating a girder)"
        ReferenceList.AddItem "233. Girders; web slenderness: 90 >= Dw / Tw (to use with stiffener emulating a girder)"
        ReferenceList.AddItem "234. Girders; web slenderness: 8 <= Tw (to use with stiffener emulating a girder)"
        ReferenceList.AddItem "235. Girders; web slenderness: 20 >= Tw (to use with stiffener emulating a girder)"
        ReferenceList.AddItem "236. Girders; flange width / web height ratio: 0.1 <= Df / Dw (to use with stiffener emulating a girder)"
        ReferenceList.AddItem "237. Girders; web slenderness: 130 >= Dw / Tw (to use with stiffener emulating a girder)"
        
        'Restrictions DNV for Girders located in the symmetry axis
        ReferenceList.AddItem "240. Girders; flange width / web height ratio: 0.1 <= Df / Dw (to use with stiffener emulating a girder on the symmetry axis)"
        ReferenceList.AddItem "241. Girders; flange width / web height ratio: 0.25 >= Df / Dw (to use with stiffener emulating a girder on the symmetry axis)"
        ReferenceList.AddItem "242. Girders; web slenderness: 56 <= Dw / Tw (to use with stiffener emulating a girder on the symmetry axis)"
        ReferenceList.AddItem "243. Girders; web slenderness: 180 >= Dw / Tw (to use with stiffener emulating a girder on the symmetry axis)"
        ReferenceList.AddItem "244. Girders; web slenderness: 4 <= Tw (to use with stiffener emulating a girder on the symmetry axis)"
        ReferenceList.AddItem "245. Girders; web slenderness: 10 >= Tw (to use with stiffener emulating a girder on the symmetry axis)"
        ReferenceList.AddItem "246. Girders; flange width / web height ratio: 0.05 <= Df / Dw (to use with stiffener emulating a girder on the symmetry axis)"
        ReferenceList.AddItem "247. Girders; web slenderness: 260 >= Dw / Tw (to use with stiffener emulating a girder on the symmetry axis)"
    
        ' -----\----
        
        ReferenceList.AddItem "301. Interaction stiffeners - frames; Dw(stiffener) - Dw(frame) <= 0 (frames taller than stiffeners)"
        ReferenceList.AddItem "304. Interaction stiffeners - frames; Dw(frame) - Dw(stiffener) <= 0 (stiffeners taller than frames)"
        ReferenceList.AddItem "302. Interaction stiffeners - frames; Tw(stiffener) - 4 * Tw(frame) <= 0"
        ReferenceList.AddItem "303. Interaction stiffeners - frames; Tw(frame) - 4 * Tw(stiffener) <= 0"
        
        ' ---DNV---
        ReferenceList.AddItem "320. Interaction stiffeners - frames; 2 <= Dw(fr) / Dw(st)"
        ' -----\----
    Else
        ReferenceList.AddItem "204. Stiffeners; plate thickness / web thickness ratio: d - 2 * Tw <= 0"
        ReferenceList.AddItem "212. Stiffeners; plate thickness / web thickness ratio: Tw - 2 * d <= 0"
        ReferenceList.AddItem "205. Stiffeners; minimal web height (minimal frame stiffness): 3 * d - Dw <= 0"
        ReferenceList.AddItem "206. Minimum plate thickness according to the position code (Mars 2000)"
        ReferenceList.AddItem "201. Stiffeners; flange width / web height ratio: Df - Dw <= 0 (Rahman set)"
        ReferenceList.AddItem "202. Stiffeners; flange width / web height ratio: Dw - 2 * Df <= 0 (Rahman set)"
        ReferenceList.AddItem "209. Stiffeners; flange width / web height ratio: 0.625 * Df - Dw <= 0 (for T shape member - Hughes set)"
        ReferenceList.AddItem "210. Stiffeners; flange width / web height ratio: Dw - 2.5 * Df <= 0 (for T shape member - Hughes set)"
        ReferenceList.AddItem "215. Stiffeners; flange width / web height ratio: 1.25 * Df - Dw <= 0 (for L shape member - Hughes set)"
        ReferenceList.AddItem "216. Stiffeners; flange width / web height ratio: Dw - 5 * Df <= 0 (for L shape member - Hughes set)"
        ReferenceList.AddItem "203. Stiffeners; web slenderness: Dw - 40 * Tw <= 0 (frames taller than stiffeners - stiffener web not supported, Rahman)"
        ReferenceList.AddItem "211. Stiffeners; web slenderness: Dw - 36 * Tw <= 0 (frames taller than stiffeners - stiffener web not supported, Rahman)"
        ReferenceList.AddItem "213. Stiffeners; web slenderness: Dw - 120 * Tw <= 0 (stiffeners taller than frames- stiffener web not supported, Rahman)"
        ReferenceList.AddItem "214. Stiffeners; web slenderness: 0.0065 + Dw / 170 - Tw <= 0 (frames taller than stiffeners - stiffener web not supported, Rahman)"
        
        ' ---DNV---
        ReferenceList.AddItem "220. Stiffeners; web slenderness: 8 <= Dw / Tw"
        ReferenceList.AddItem "221. Stiffeners; web slenderness: 50 >= Dw / Tw"
        ReferenceList.AddItem "230. Girders; flange width / web height ratio: 0.2 <= Df / Dw (to use with stiffener emulating a girder)"
        ReferenceList.AddItem "231. Girders; flange width / web height ratio: 0.5 >= Df / Dw (to use with stiffener emulating a girder)"
        ReferenceList.AddItem "232. Girders; web slenderness: 28 <= Dw / Tw (to use with stiffener emulating a girder)"
        ReferenceList.AddItem "233. Girders; web slenderness: 90 >= Dw / Tw (to use with stiffener emulating a girder)"
        ReferenceList.AddItem "234. Girders; web slenderness: 8 <= Tw (to use with stiffener emulating a girder)"
        ReferenceList.AddItem "235. Girders; web slenderness: 20 >= Tw (to use with stiffener emulating a girder)"
        ReferenceList.AddItem "236. Girders; flange width / web height ratio: 0.1 <= Df / Dw (to use with stiffener emulating a girder)"
        ReferenceList.AddItem "237. Girders; web slenderness: 130 >= Dw / Tw (to use with stiffener emulating a girder)"
        
        'Restrictions DNV for Girders located in the symmetry axis
        ReferenceList.AddItem "240. Girders; flange width / web height ratio: 0.1 <= Df / Dw (to use with stiffener emulating a girder on the symmetry axis)"
        ReferenceList.AddItem "241. Girders; flange width / web height ratio: 0.25 >= Df / Dw (to use with stiffener emulating a girder on the symmetry axis)"
        ReferenceList.AddItem "242. Girders; web slenderness: 56 <= Dw / Tw (to use with stiffener emulating a girder on the symmetry axis)"
        ReferenceList.AddItem "243. Girders; web slenderness: 180 >= Dw / Tw (to use with stiffener emulating a girder on the symmetry axis)"
        ReferenceList.AddItem "244. Girders; web slenderness: 4 <= Tw (to use with stiffener emulating a girder on the symmetry axis)"
        ReferenceList.AddItem "245. Girders; web slenderness: 10 >= Tw (to use with stiffener emulating a girder on the symmetry axis)"
        ReferenceList.AddItem "246. Girders; flange width / web height ratio: 0.05 <= Df / Dw (to use with stiffener emulating a girder on the symmetry axis)"
        ReferenceList.AddItem "247. Girders; web slenderness: 260 >= Dw / Tw (to use with stiffener emulating a girder on the symmetry axis)"
    
    End If
End Sub

Private Sub PanelList_Click()
    Compute
    PanelIndex = PanelList.ListIndex + 1
    PopulateCombo
    PopulateAvailableList
    SelectionInAvailableList
    TabStripsHighlight
End Sub

Public Sub TabStripsHighlight()
    Dim i As Integer
    'On Error Resume Next
    For i = 1 To TabStrip.Tabs.Count
        TabStrip.Tabs(i).HighLighted = False
    Next i
    If PanelIndex > 0 Then
        'If vGeom(PanelIndex)(1) <> 0 Then
        If vM2(PanelIndex) <> 99 Then
            For i = 1 To vM2(PanelIndex)
                Select Case vGeom(PanelIndex)(i)
                    Case 100 To 199
                        If Project.Item(ProjectIndex).cHeader.IANA = 1 Then TabStrip.Tabs(2).HighLighted = True ' p'aici am un bug
                    Case 200 To 299
                        TabStrip.Tabs(1).HighLighted = True
                    Case 300 To 399
                        If Project.Item(ProjectIndex).cHeader.IANA = 1 Then TabStrip.Tabs(3).HighLighted = True
                End Select
            Next i
        'ElseIf vGeomSet(PanelIndex) <> 0 Then
        ElseIf vM2(PanelIndex) = 99 Then
            TabStrip.Tabs(4).HighLighted = True
        End If
    End If

End Sub

Private Sub PasteList_MouseDown(Button As Integer, Shift As Integer, x As Single, y As Single)
    If Button = 2 Then
        PopupMenu mnuPaste
    End If
End Sub

Private Sub PasteSel_Click()
    Dim i As Integer
    For i = 1 To PasteList.ListCount
        If PasteList.Selected(i - 1) = True Then
            vM2(i) = vM2(PanelIndex)
            vGeom(i) = vGeom(PanelIndex)
            vGeomSet(i) = vGeomSet(PanelIndex)
            vISEMA(i) = vISEMA(PanelIndex)
            vISEMR(i) = vISEMR(PanelIndex)
        End If
    Next i
    
'        Panel(i).M2 = vM2(i)
'        Panel(i).GeomConstr = vGeom(i)
'        Panel(i).GeomSet = vGeomSet(i)
'        Panel(i).ISEMA = vISEMA(i)
'        Panel(i).ISEMR = vISEMR(i)
    For i = 1 To PasteList.ListCount
        PasteList.Selected(i - 1) = False
    Next i
    PasteList.Visible = False
    Label1.Caption = "Panels:"
End Sub

Private Sub TabStrip_BeforeClick(Cancel As Integer)
If PanelIndex > 0 Then
    Compute
    TabStripsHighlight
End If
End Sub

Private Sub TabStrip_Click()
    PopulateAvailableList
    SelectionInAvailableList
End Sub

Private Sub SelectionInAvailableList()
    Dim i As Integer
    Dim j As Integer
    'On Error Resume Next
    ReDim vRefNumber(1 To AvailableList.ListCount)
    If PanelIndex > 0 Then
        For i = 1 To AvailableList.ListCount
            v = AvailableList.List(i - 1)
            GetRefNumber v
            vRefNumber(i) = Val_(v)
        Next i
        For i = 1 To AvailableList.ListCount
            'If vGeom(PanelIndex)(1) > 0 And vGeomSet(PanelIndex) = 0 Then
            If vM2(PanelIndex) <> 99 And vM2(PanelIndex) > 0 Then
                For j = 1 To vM2(PanelIndex)
                    If vRefNumber(i) = vGeom(PanelIndex)(j) Then
                        AvailableList.Selected(i - 1) = True
                    End If
                Next j
            'ElseIf vGeom(PanelIndex)(1) = 0 And vGeomSet(PanelIndex) > 0 Then
            ElseIf vM2(PanelIndex) = 99 Then
                If vRefNumber(i) = vGeomSet(PanelIndex) Then
                    AvailableList.Selected(i - 1) = True
                End If
            End If
        Next i
    End If
    AvailableList.ListIndex = -1
End Sub

Public Sub Compute()
    Dim i As Integer
    If PanelIndex > 0 Then
        Index1 = 0
        For i = 1 To UBound(vGeom(PanelIndex))
            If Len(vGeom(PanelIndex)(i)) = 3 And Left(vGeom(PanelIndex)(i), 1) = 2 Then
                Index1 = Index1 + 1
                ReDim Preserve vGeom1(1 To Index1)
                vGeom1(Index1) = vGeom(PanelIndex)(i)
            End If
        Next i
        Index2 = 0
        For i = 1 To UBound(vGeom(PanelIndex))
            If Len(vGeom(PanelIndex)(i)) = 3 And Left(vGeom(PanelIndex)(i), 1) = 1 Then
                Index2 = Index2 + 1
                ReDim Preserve vGeom2(1 To Index2)
                vGeom2(Index2) = vGeom(PanelIndex)(i)
            End If
        Next i
        Index3 = 0
        For i = 1 To UBound(vGeom(PanelIndex))
            If Len(vGeom(PanelIndex)(i)) = 3 And Left(vGeom(PanelIndex)(i), 1) = 3 Then
                Index3 = Index3 + 1
                ReDim Preserve vGeom3(1 To Index3)
                vGeom3(Index3) = vGeom(PanelIndex)(i)
            End If
        Next i
        Index99 = 0
'        For i = 1 To 5
'            If Left(vGeomSet(PanelIndex), 1) = i Then
'                Index99 = 99
'
'            End If
'        Next i
        
    AvailableList_ItemCheck AvailableList.ListIndex
    
    
     Select Case TabStrip.SelectedItem
            Case "Stiffeners"
                Index1 = 0
                For i = 1 To AvailableList.ListCount
                    If AvailableList.ItemData(i - 1) = 1 Then
                        'Index99 = 0
                        Index1 = Index1 + 1
                        ReDim Preserve vGeom1(1 To Index1)
                        vGeom1(Index1) = Left(AvailableList.List(i - 1), 3)
                    End If
                Next i
            Case "Frames"
                Index2 = 0
                For i = 1 To AvailableList.ListCount
                    If AvailableList.ItemData(i - 1) = 1 Then
                        'Index99 = 0
                        Index2 = Index2 + 1
                        ReDim Preserve vGeom2(1 To Index2)
                        vGeom2(Index2) = Left(AvailableList.List(i - 1), 3)
                    End If
                Next i
            Case "Stiffeners - Frames Interaction"
                Index3 = 0
                For i = 1 To AvailableList.ListCount
                    If AvailableList.ItemData(i - 1) = 1 Then
                        'Index99 = 0
                        Index3 = Index3 + 1
                        ReDim Preserve vGeom3(1 To Index3)
                        vGeom3(Index3) = Left(AvailableList.List(i - 1), 3)
                    End If
                Next i
            Case "Sets"
                Index99 = 0
                For i = 1 To AvailableList.ListCount
                    If AvailableList.ItemData(i - 1) = 1 Then
                        Index99 = 99
                        vGeomSet(PanelIndex) = Left(AvailableList.List(i - 1), 1)
                        vGeom(PanelIndex) = vGeom0
                        'Index1 = 0
                        'Index2 = 0
                        'Index3 = 0
                    End If
                Next i
        End Select
        Index0 = Index1 + Index2 + Index3
        If Index0 > 0 And Index99 <> 99 Then
            ReDim vGeomtmp(1 To Index0)
            For i = 1 To Index1
                vGeomtmp(i) = vGeom1(i)
            Next i
            For i = 1 To Index2
                vGeomtmp(Index1 + i) = vGeom2(i)
            Next i
            For i = 1 To Index3
                vGeomtmp(Index1 + Index2 + i) = vGeom3(i)
            Next i
        Else
            ReDim vGeomtmp(1 To 1)
            vGeomtmp(1) = 0
            vGeom(PanelIndex) = vGeomtmp
        End If
        If Index99 = 99 Then
            vM2(PanelIndex) = 99
            ReDim vGeomtmp(1 To 1)
            vGeomtmp(1) = 0
            vGeom(PanelIndex) = vGeomtmp
        ElseIf Index0 > 0 And Index99 <> 99 Then
            vGeom(PanelIndex) = vGeomtmp
            vM2(PanelIndex) = UBound(vGeom(PanelIndex))
            vGeomSet(PanelIndex) = 0
        'End If

        ElseIf vGeom(PanelIndex)(1) = 0 And vGeomSet(PanelIndex) = 0 Then
           vM2(PanelIndex) = 0

        End If
           PopulateCombo
'        If Index0 = 0 And Index99 = 0 Then
'            vM2(PanelIndex) = 0
'        End If
    End If ' PanelIndex > 0
End Sub

Private Sub PopulateCombo()
Combo1.Clear
Combo2.Clear
On Error Resume Next
If PanelIndex > 0 Then
    If vM2(PanelIndex) = 0 Then
        Combo1.Enabled = False
        Combo2.Enabled = False
        Exit Sub
    Else
        Combo1.Enabled = True
        Combo2.Enabled = True
    End If

    Combo1.AddItem "0: Fixed"
    Combo1.AddItem "1: Adjusted"
    Combo1.AddItem "2: Adjusted"
    Combo1.AddItem "3: Adjusted"
    Combo2.AddItem "0: Fixed"
    Combo2.AddItem "1: Adjusted"
    Combo2.AddItem "2: Adjusted"
    Combo2.AddItem "3: Adjusted"
    Combo2.AddItem "4: Adjusted"
    Combo2.AddItem "5: Adjusted"
    
    If vM2(PanelIndex) > 0 Then
        Select Case vISEMA(PanelIndex)
            Case 0
                Combo1.ListIndex = 0
            Case 1
                Combo1.ListIndex = 1
            Case 2
                Combo1.ListIndex = 2
            Case 3
                Combo1.ListIndex = 3
            Case 4
                Combo1.ListIndex = 4
        End Select
        Select Case vISEMR(PanelIndex)
            Case 0
                Combo2.ListIndex = 0
            Case 1
                Combo2.ListIndex = 1
            Case 2
                Combo2.ListIndex = 2
            Case 3
                Combo2.ListIndex = 3
            Case 4
                Combo2.ListIndex = 4
            Case 5
                Combo2.ListIndex = 5
        End Select
        Combo1.Enabled = True
        Combo2.Enabled = True
    Else
        Combo1.Enabled = False
        Combo2.Enabled = False
    End If
End If
End Sub

Public Function GetRefNumber(v As String)
1:
    If Left(v, 1) = " " Then
        v = right(v, Len(v) - 1)
        GoTo 1
    End If
    v = Left(v, InStr(1, v, ".") - 1)
End Function
