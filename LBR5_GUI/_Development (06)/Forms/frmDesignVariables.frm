VERSION 5.00
Object = "{5E9E78A0-531B-11CF-91F6-C2863C385E30}#1.0#0"; "MSFLXGRD.OCX"
Object = "{0D452EE1-E08F-101A-852E-02608C4D0BB4}#2.0#0"; "FM20.DLL"
Begin VB.Form frmDesignVariables 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Design Variables"
   ClientHeight    =   3795
   ClientLeft      =   6315
   ClientTop       =   3810
   ClientWidth     =   6750
   Icon            =   "frmDesignVariables.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   3795
   ScaleWidth      =   6750
   ShowInTaskbar   =   0   'False
   Begin VB.ListBox lstMultiPanels 
      Appearance      =   0  'Flat
      ForeColor       =   &H00FF0000&
      Height          =   2955
      ItemData        =   "frmDesignVariables.frx":000C
      Left            =   0
      List            =   "frmDesignVariables.frx":0013
      MultiSelect     =   2  'Extended
      TabIndex        =   6
      Top             =   120
      Visible         =   0   'False
      Width           =   1695
   End
   Begin VB.CheckBox Chk 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      ForeColor       =   &H80000008&
      Height          =   195
      Index           =   0
      Left            =   6480
      TabIndex        =   5
      Top             =   240
      Width           =   200
   End
   Begin VB.TextBox TxtEdit 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   285
      Left            =   1800
      TabIndex        =   4
      Text            =   "Text1"
      Top             =   600
      Visible         =   0   'False
      Width           =   975
   End
   Begin VB.ListBox lstPanels 
      Appearance      =   0  'Flat
      Height          =   2955
      ItemData        =   "frmDesignVariables.frx":0022
      Left            =   0
      List            =   "frmDesignVariables.frx":0029
      TabIndex        =   1
      Top             =   120
      Width           =   1695
   End
   Begin MSFlexGridLib.MSFlexGrid MSH1 
      Height          =   2955
      Left            =   1680
      TabIndex        =   0
      Top             =   120
      Width           =   5055
      _ExtentX        =   8916
      _ExtentY        =   5212
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
   Begin MSForms.CommandButton cmdOK 
      Default         =   -1  'True
      Height          =   375
      Left            =   4440
      TabIndex        =   3
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
   Begin MSForms.CommandButton cmdCancel 
      Height          =   375
      Left            =   5640
      TabIndex        =   2
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
   Begin VB.Menu mnuEdit 
      Caption         =   "Edit"
      Begin VB.Menu mnuEditCopy 
         Caption         =   "Copy"
         Shortcut        =   ^C
      End
      Begin VB.Menu mnuPaste 
         Caption         =   "Paste"
         Shortcut        =   ^V
      End
      Begin VB.Menu mnuEditSeparator 
         Caption         =   "-"
      End
      Begin VB.Menu mnuEditCopyMultiple 
         Caption         =   "Copy (On Multiple Panels)"
      End
   End
   Begin VB.Menu mnuMultiPaste 
      Caption         =   "MultiPaste"
      Begin VB.Menu mnuMultiPastePaste 
         Caption         =   "Paste"
      End
   End
End
Attribute VB_Name = "frmDesignVariables"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Dim ProjectIndex As Integer
Dim PanelIndex As Integer
Dim dBuffer As String
Dim colDes As Collection

Private Sub Chk_Click(index As Integer)
    Select Case Project.Item(ProjectIndex).colPanel.Item(PanelIndex).pType
        Case Plate
            colDes.Item(PanelIndex).Item(index).Active = Chk(index).Value
        Case Beam
            colDes.Item(PanelIndex).Item(5).Active = Chk(index).Value
    End Select
    MSH1.TextMatrix(index, 4) = Chk(index).Value
End Sub

Private Sub cmdCancel_Click()
    Unload Me
End Sub

Private Sub cmdOK_Click()
    Dim i As Integer
    Dim oPan As cPanel, oDes As cDesignVariables
    Dim GetTotalNumberOfDesVar As Integer
    
    'Demo check
    GetTotalNumberOfDesVar = 0
    'For Each oPan In Project.Item(index).colPanel
    For i = 1 To colDes.Count
        For Each oDes In colDes(i)
            If oDes.Active = True Then
                GetTotalNumberOfDesVar = GetTotalNumberOfDesVar + 1
                If GetTotalNumberOfDesVar > Licensing.MAX_DESIGN_VARIABLES Then
                    MsgBox "The maximum number of design variables is restricted to " & Licensing.MAX_DESIGN_VARIABLES & ".", vbCritical + vbOKOnly, "LBR-5 License Limitation"
                    Exit Sub
                End If
            End If
        Next oDes
    'Next oPan
    Next i

    For Each oPan In Project.Item(ProjectIndex).colPanel
        Set oPan.colDesignVariables = colDes.Item(oPan.index)
    Next oPan
    Unload Me
End Sub

Private Sub Form_KeyDown(KeyCode As Integer, Shift As Integer)
    Dim i As Integer
    Select Case KeyCode
        Case 27 'Escape
            If lstMultiPanels.Visible = True Then
                lstMultiPanels.Visible = False
                MSH1.Enabled = True
                For i = 1 To Chk.Count - 1
                    Chk(i).Enabled = True
                Next i
            Else
                cmdCancel_Click
            End If
    End Select
End Sub

Private Sub Form_Load()
    If Licensing.IS_MODIFY_DESIGN_VARIABLES = False Then
        cmdOK.Enabled = False
    End If

    mnuEdit.Visible = False
    mnuMultiPaste.Visible = False
    ProjectIndex = ActiveProject
    Me.Move (Screen.Width - Me.Width) / 2, (Screen.Height - Me.Height) / 2
    cmdOK.Picture = LoadResPicture("ID_BITMAP_OK", 0)
    cmdCancel.Picture = LoadResPicture("ID_BITMAP_CANCEL", 0)
    Me.Caption = "Design Variables - [" & GetFileName(Project.Item(ProjectIndex).sFileName) & "]"
    Chk(0).Visible = False
    GetData
    PopulatePanelList
    PopulateMultiPanelList
    lstPanels.ListIndex = 0
    lstMultiPanels.ListIndex = 0
    FlexGrid
    FillGrid
    LoadChk
End Sub

Private Sub GetData()
    Dim Panel As cPanel
    Set colDes = New Collection
    For Each Panel In Project.Item(ProjectIndex).colPanel
        colDes.Add Panel.colDesignVariables.Clone
    Next Panel
End Sub

Private Sub LoadChk()
    Dim i As Integer
    Dim h As Integer
    Chk(0).Left = MSH1.Left + MSH1.Width - 390 + 68
    Chk(0).Top = MSH1.Top + 720
    h = Chk(0).Top
    For i = 1 To Chk.UBound
        Unload Chk(i)
    Next i
    Select Case Project.Item(ProjectIndex).colPanel.Item(PanelIndex).pType
        Case Plate
            For i = 1 To 9
                Load Chk(i)
                If MSH1.RowHeight(i) > 0 Then 'if IANA = 1
                    Chk(i).Top = h
                    Chk(i).Visible = True
                    Chk(i).ZOrder 0
                    h = h + 240
                End If
                If colDes.Item(PanelIndex).Item(i).Active = True Then
                    Chk(i).Value = vbChecked
                Else
                    Chk(i).Value = vbUnchecked
                End If
            Next i
        Case Beam
            Load Chk(1)
            If MSH1.RowHeight(1) > 0 Then 'if IANA = 1
                Chk(1).Top = h
                Chk(1).Visible = True
                Chk(1).ZOrder 0
            End If
            If colDes.Item(PanelIndex).Item(5).Active = True Then
                Chk(1).Value = vbChecked
            Else
                Chk(1).Value = vbUnchecked
            End If
    End Select
End Sub

Private Sub FlexGrid()
    Dim i As Integer
    MSH1.Clear
    Select Case Project.Item(ProjectIndex).colPanel.Item(PanelIndex).pType
        Case Plate
            MSH1.RowHeight(0) = 690
            MSH1.Rows = 10
            MSH1.Cols = 5
            MSH1.FormatString = "|^|^|^"
            MSH1.ColWidth(0) = 2000
            MSH1.ColWidth(1) = 900
            MSH1.ColWidth(2) = 900
            MSH1.ColWidth(3) = 900
            MSH1.ColWidth(4) = 250
            MSH1.TextMatrix(0, 0) = "Design Variable"
            MSH1.TextMatrix(0, 1) = "Lower Limit [mm]"
            MSH1.TextMatrix(0, 2) = "Current Value [mm]"
            MSH1.TextMatrix(0, 3) = "Upper Limit [mm]"
            MSH1.TextMatrix(1, 0) = "Plate Thickness"
            MSH1.TextMatrix(2, 0) = "Frames Web Height"
            MSH1.TextMatrix(3, 0) = "Frames Web Thickness"
            MSH1.TextMatrix(4, 0) = "Frames Flange Width"
            MSH1.TextMatrix(5, 0) = "Frames Spacing"
            MSH1.TextMatrix(6, 0) = "Stiffeners Web Height"
            MSH1.TextMatrix(7, 0) = "Stiffeners Web Thickness"
            MSH1.TextMatrix(8, 0) = "Stiffeners Flange Width"
            MSH1.TextMatrix(9, 0) = "Stiffeners Spacing"
            For i = 1 To MSH1.Rows - 1
                MSH1.Row = i: MSH1.col = 2
                MSH1.CellBackColor = &H8000000F
                MSH1.CellFontBold = True
            Next i
            If Project.Item(ProjectIndex).cHeader.IANA = 2 Then
                MSH1.RowHeight(2) = 0
                MSH1.RowHeight(3) = 0
                MSH1.RowHeight(4) = 0
                MSH1.TextMatrix(5, 0) = "Stiffeners Span"
            End If
        Case Beam
            MSH1.RowHeight(0) = 690
            MSH1.Rows = 2
            MSH1.Cols = 5
            MSH1.FormatString = "|^|^|^"
            MSH1.ColWidth(0) = 2000
            MSH1.ColWidth(1) = 900
            MSH1.ColWidth(2) = 900
            MSH1.ColWidth(3) = 900
            MSH1.ColWidth(4) = 250
            MSH1.TextMatrix(0, 0) = "Design Variable"
            MSH1.TextMatrix(0, 1) = "Lower Limit [mm]"
            MSH1.TextMatrix(0, 2) = "Current Value [mm]"
            MSH1.TextMatrix(0, 3) = "Upper Limit [mm]"
            MSH1.TextMatrix(1, 0) = "Beam Spacing"
            MSH1.Row = 1: MSH1.col = 2
            MSH1.CellBackColor = &H8000000F
            MSH1.CellFontBold = True
            If Project.Item(ProjectIndex).cHeader.IANA = 2 Then
                MSH1.RowHeight(1) = 0
            End If
    End Select
End Sub

Private Sub FillGrid()
    Dim cPanel As cPanel
    Dim cDesignVariable As cDesignVariables
    Dim i As Integer
    Select Case Project.Item(ProjectIndex).colPanel.Item(PanelIndex).pType
        Case Plate
            For Each cDesignVariable In colDes.Item(PanelIndex)
                MSH1.TextMatrix(cDesignVariable.VariableName, 1) = cDesignVariable.LowerLimit * 1000
                MSH1.TextMatrix(cDesignVariable.VariableName, 3) = cDesignVariable.UpperLimit * 1000
                If cDesignVariable.Active = True Then
                    MSH1.TextMatrix(cDesignVariable.VariableName, 4) = 1
                Else
                    MSH1.TextMatrix(cDesignVariable.VariableName, 4) = 0
                End If
            Next cDesignVariable
            'Scantlings
            MSH1.TextMatrix(1, 2) = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.NetThickness * 1000
            MSH1.TextMatrix(2, 2) = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryFrames.WebHeight * 1000
            MSH1.TextMatrix(3, 2) = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryFrames.WebThickness * 1000
            MSH1.TextMatrix(4, 2) = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryFrames.FlangeWidth * 1000
            MSH1.TextMatrix(5, 2) = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryFrames.Spacing * 1000
            MSH1.TextMatrix(6, 2) = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryStiffeners.WebHeight * 1000
            MSH1.TextMatrix(7, 2) = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryStiffeners.WebThickness * 1000
            MSH1.TextMatrix(8, 2) = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryStiffeners.FlangeWidth * 1000
            MSH1.TextMatrix(9, 2) = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryStiffeners.Spacing * 1000
        Case Beam
            MSH1.TextMatrix(1, 1) = colDes.Item(PanelIndex).Item(5).LowerLimit * 1000
            MSH1.TextMatrix(1, 3) = colDes.Item(PanelIndex).Item(5).UpperLimit * 1000
            If colDes.Item(PanelIndex).Item(5).Active = True Then
                MSH1.TextMatrix(1, 4) = 1
            Else
                MSH1.TextMatrix(1, 4) = 0
            End If
            MSH1.TextMatrix(1, 2) = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryFrames.Spacing * 1000
        Case Else
    End Select
End Sub

Private Sub PopulateMultiPanelList()
    lstMultiPanels.Clear
    Dim i As Integer
    Dim oPan As cPanel
    For Each oPan In Project.Item(ProjectIndex).colPanel
        Select Case oPan.pType
            Case Plate
                If oPan.cGeometry.PanelWidth > 0.002 Then
                    lstMultiPanels.AddItem "Panel " & oPan.index & " (Plate)"
                End If
            Case Beam
                lstMultiPanels.AddItem "Panel " & oPan.index & " (Beam)"
            Case Else
        End Select
    Next oPan
End Sub

Private Sub PopulatePanelList()
    lstPanels.Clear
    Dim i As Integer
    Dim oPan As cPanel
    For Each oPan In Project.Item(ProjectIndex).colPanel
        Select Case oPan.pType
            Case Plate
                If oPan.cGeometry.PanelWidth > 0.002 Then
                    lstPanels.AddItem "Panel " & oPan.index & " (Plate)"
                End If
            Case Beam
                lstPanels.AddItem "Panel " & oPan.index & " (Beam)"
            Case Else
        End Select
    Next oPan
End Sub

Private Sub SetData()
    On Error GoTo SetDataErr
    Dim cPanel As cPanel
    Dim cDesignVariable As cDesignVariables
    Dim i As Integer
    Select Case Project.Item(ProjectIndex).colPanel.Item(PanelIndex).pType
        Case Plate
            For Each cDesignVariable In colDes.Item(PanelIndex)
                cDesignVariable.LowerLimit = MSH1.TextMatrix(cDesignVariable.VariableName, 1) / 1000
                cDesignVariable.UpperLimit = MSH1.TextMatrix(cDesignVariable.VariableName, 3) / 1000
                If MSH1.TextMatrix(cDesignVariable.VariableName, 4) = 1 Then
                    cDesignVariable.Active = True
                Else
                    cDesignVariable.Active = False
                End If
            Next cDesignVariable
            'Scantlings
            MSH1.TextMatrix(1, 2) = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.NetThickness * 1000
            MSH1.TextMatrix(2, 2) = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryFrames.WebHeight * 1000
            MSH1.TextMatrix(3, 2) = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryFrames.WebThickness * 1000
            MSH1.TextMatrix(4, 2) = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryFrames.FlangeWidth * 1000
            MSH1.TextMatrix(5, 2) = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryFrames.Spacing * 1000
            MSH1.TextMatrix(6, 2) = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryStiffeners.WebHeight * 1000
            MSH1.TextMatrix(7, 2) = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryStiffeners.WebThickness * 1000
            MSH1.TextMatrix(8, 2) = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryStiffeners.FlangeWidth * 1000
            MSH1.TextMatrix(9, 2) = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryStiffeners.Spacing * 1000
        Case Beam
            colDes.Item(PanelIndex).Item(5).LowerLimit = MSH1.TextMatrix(1, 1) / 1000
            colDes.Item(PanelIndex).Item(5).UpperLimit = MSH1.TextMatrix(1, 3) / 1000
            MSH1.TextMatrix(1, 2) = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryFrames.Spacing * 1000
            If MSH1.TextMatrix(1, 4) = 1 Then
                colDes.Item(PanelIndex).Item(5).Active = True
            Else
                colDes.Item(PanelIndex).Item(5).Active = False
            End If
        Case Else
    End Select
    LoadChk
    Exit Sub
SetDataErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmDesignVariables: Sub SetData")
End Sub

' =============
' FLEXGRID EDIT
' =============

Function grd_index(r As Integer, c As Integer)
    grd_index = c + MSH1.Cols * r
End Function

Private Sub Form_Resize()
    Me.Height = cmdOK.Top + cmdOK.Height + 600
End Sub

Private Sub Form_Unload(Cancel As Integer)
    Set colDes = Nothing
End Sub

Private Sub lstMultiPanels_KeyPress(KeyAscii As Integer)
    Dim i As Integer
    Select Case KeyAscii
        Case 27
            If lstMultiPanels.Visible = True Then
                lstMultiPanels.Visible = False
                MSH1.Enabled = True
                For i = 1 To Chk.Count - 1
                    Chk(i).Enabled = True
                Next i
            Else
                cmdCancel_Click
            End If
    End Select
End Sub

Private Sub lstMultiPanels_MouseDown(Button As Integer, Shift As Integer, x As Single, y As Single)
    If Button = 2 Then
        PopupMenu mnuMultiPaste
    End If
End Sub

Private Sub lstPanels_Click()
    Dim v() As Variant
    Dim sData As String
    sData = lstPanels.Text
    GetValues 2, sData, v
    PanelIndex = Val_(v(2))
    MSH1.Clear
    FlexGrid
    FillGrid
    LoadChk
End Sub

Private Sub mnuEditCopy_Click()
    DesFlexCopy MSH1
End Sub

Private Sub mnuEditCopyMultiple_Click()
    Dim i As Integer
    lstMultiPanels.Visible = True
    MSH1.Enabled = False
    For i = 1 To Chk.Count - 1
        Chk(i).Enabled = False
    Next i
End Sub

Private Sub mnuMultiPastePaste_Click()
    Dim i As Integer
    Dim v() As Variant
    Dim sData As String
    Dim index As Integer
    For i = 0 To lstPanels.ListCount - 1
        If lstMultiPanels.Selected(i) = True Then
            sData = lstMultiPanels.List(i)
            GetValues 2, sData, v
            index = Val_(v(2))
            DesPasteMulti index
        End If
    Next i
    lstMultiPanels.Visible = False
    MSH1.Enabled = True
    For i = 1 To Chk.Count - 1
        Chk(i).Enabled = True
    Next i
End Sub

Private Sub mnuPaste_Click()
    DesFlexPaste MSH1
    SetData
End Sub

Private Sub MSH1_KeyDown(KeyCode As Integer, Shift As Integer)
    'If Len(MSH1) > 0 Then dBuffer = (MSH1)
    TxtEdit_KeyDown KeyCode, Shift
End Sub

Private Sub MSH1_KeyPress(KeyAscii As Integer)
    Dim i As Integer
    Select Case KeyAscii
        Case 3 'CTRL + C
            DesFlexCopy MSH1
            Exit Sub
        Case 22 'CTRL + V
            DesFlexPaste MSH1: SetData
            Exit Sub
        Case 27 'Esc
            If lstMultiPanels.Visible = True Then
                lstMultiPanels.Visible = False
                MSH1.Enabled = True
                For i = 1 To Chk.Count - 1
                    Chk(i).Enabled = True
                Next i
                Exit Sub
            Else
                cmdCancel_Click
            End If
    End Select
    Select Case MSH1.col
        Case 1, 3
            MSHFlexGridEdit MSH1, TxtEdit, KeyAscii
        Case Else
    End Select
End Sub

Private Sub MSH1_DblClick()
    Select Case MSH1.col
        Case 1, 3
            MSHFlexGridEdit MSH1, TxtEdit, 32
        Case Else
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

Private Sub MSH1_MouseDown(Button As Integer, Shift As Integer, x As Single, y As Single)
    If Button = 2 Then
        PopupMenu mnuEdit
    End If
End Sub

Private Sub MSH1_Scroll()
    TxtEdit.Visible = False
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
'    If Combo.Visible = False Then Exit Sub
'    MSH1 = Combo.List(Combo.ListIndex)
'    Combo.Visible = False
End Sub

Private Sub MSH1_LeaveCell()
    If TxtEdit.Visible = False Then GoTo 1 'Exit Sub
    MSH1 = TxtEdit
    TxtEdit.Visible = False
1:
'    If Combo.Visible = False Then Exit Sub
'    MSH1 = Combo.List(Combo.ListIndex)
'    Combo.Visible = False
End Sub

Private Sub TxtEdit_LostFocus()
    MSH1_GotFocus
    SetData
    cmdOK.Default = True
End Sub

Private Sub Combo_LostFocus()
    MSH1_GotFocus
    SetData
End Sub

Private Sub TxtEdit_Validate(Cancel As Boolean)
    ValidateNonNullPozitive TxtEdit, Cancel
    If Cancel = True Then TxtEdit = dBuffer: Exit Sub
    ValidateLimits Cancel
    If Cancel = True Then TxtEdit = dBuffer
End Sub

Private Sub ValidateLimits(Cancel As Boolean)
    Select Case MSH1.col
        Case 1
            If Val_(TxtEdit.Text) > Val_(MSH1.TextMatrix(MSH1.Row, 2)) Then
                Cancel = True
            End If
        Case 3
            If Val_(TxtEdit.Text) < Val_(MSH1.TextMatrix(MSH1.Row, 2)) Then
                Cancel = True
            End If
    End Select
    If Cancel = True Then
        MsgBox "Limit not compatible with scantlings", vbCritical + vbOKOnly
    End If
End Sub

Private Function ValidateLimits1(irow As Integer, icol As Integer) As Boolean
    Select Case icol
        Case 1
            If Val_(MSH1.TextMatrix(irow, 1)) > Val_(MSH1.TextMatrix(irow, 2)) Then
                ValidateLimits1 = True
            End If
        Case 2
            ValidateLimits1 = True
        Case 3
            If Val_(MSH1.TextMatrix(irow, 3)) < Val_(MSH1.TextMatrix(irow, 2)) Then
                ValidateLimits1 = True
            End If
    End Select
    If ValidateLimits1 = True Then
        MsgBox "Limit not compatible with scantlings", vbCritical + vbOKOnly
    End If
End Function

Private Sub DesFlexCopy(MSH1 As MSFlexGrid)
    Dim i As Integer
    Dim j As Integer
    Dim Indexi As Integer
    Dim Indexj As Integer
    Dim Row1 As Integer, Row2 As Integer, Col1 As Integer, Col2 As Integer
    Indexi = 0
    Indexj = 0
    If MSH1.ColSel >= MSH1.col Then
        Col1 = MSH1.col
        Col2 = MSH1.ColSel
    Else
        Col1 = MSH1.ColSel
        Col2 = MSH1.col
    End If
    If MSH1.RowSel >= MSH1.Row Then
        Row1 = MSH1.Row
        Row2 = MSH1.RowSel
    Else
        Row1 = MSH1.RowSel
        Row2 = MSH1.Row
    End If
    ReDim ArrayCopy(Row2 - Row1 + 1, Col2 - Col1 + 1)
    For i = Row1 To Row2
        Indexi = Indexi + 1
        Indexj = 0
        For j = Col1 To Col2
            Indexj = Indexj + 1
            ArrayCopy(Indexi, Indexj) = MSH1.TextMatrix(i, j)
        Next j
    Next i
End Sub

Private Sub DesPasteMulti(ByVal index As Integer)
    On Error GoTo 1
    Dim i As Integer, j As Integer
    Dim Row1 As Integer, Row2 As Integer, Col1 As Integer, Col2 As Integer
    Dim cSc As cScantlings
    Dim v() As Variant
    Set cSc = Project.Item(ProjectIndex).colPanel.Item(index).cScantlings
    GetScantlings cSc, v
    If MSH1.ColSel >= MSH1.col Then
        Col1 = MSH1.col
        Col2 = MSH1.ColSel
    Else
        Col1 = MSH1.ColSel
        Col2 = MSH1.col
    End If
    If MSH1.RowSel >= MSH1.Row Then
        Row1 = MSH1.Row
        Row2 = MSH1.RowSel
    Else
        Row1 = MSH1.RowSel
        Row2 = MSH1.Row
    End If
    For i = Row1 To Row2
        For j = Col1 To Col2
            Select Case j
                Case 1
                    If colDes.Item(PanelIndex).Item(i).LowerLimit <= Val_(v(i)) Then
                        colDes.Item(index).Item(i).LowerLimit = colDes.Item(PanelIndex).Item(i).LowerLimit
                    End If
                Case 2
                Case 3
                    If colDes.Item(PanelIndex).Item(i).UpperLimit >= Val_(v(i)) Then
                        colDes.Item(index).Item(i).UpperLimit = colDes.Item(PanelIndex).Item(i).UpperLimit
                    End If
                Case 4
                    If Project.Item(ProjectIndex).colPanel.Item(index).pType = Beam Then
                        If i = 5 Then
                            colDes.Item(index).Item(i).Active = colDes.Item(PanelIndex).Item(i).Active
                        Else
                            colDes.Item(index).Item(i).Active = False
                        End If
                    Else
                        colDes.Item(index).Item(i).Active = colDes.Item(PanelIndex).Item(i).Active
                    End If
            End Select
        Next j
    Next i
    Set cSc = Nothing
    Exit Sub
1:
    MsgBox "err."
End Sub

Private Sub DesFlexPaste(MSH1 As MSFlexGrid)
    On Error GoTo 1
    Dim i As Integer
    Dim j As Integer
    Dim Indexi As Integer
    Dim Indexj As Integer
    Dim Row1 As Integer, Row2 As Integer, Col1 As Integer, Col2 As Integer
    Indexi = 0
    Indexj = 0
    If MSH1.ColSel >= MSH1.col Then
        Col1 = MSH1.col
        Col2 = MSH1.ColSel
    Else
        Col1 = MSH1.ColSel
        Col2 = MSH1.col
    End If
    If MSH1.RowSel >= MSH1.Row Then
        Row1 = MSH1.Row
        Row2 = MSH1.RowSel
    Else
        Row1 = MSH1.RowSel
        Row2 = MSH1.Row
    End If
    ReDim ArrayPaste(Row2 - Row1 + 1, Col2 - Col1 + 1)
    If UBound(ArrayCopy, 1) = 1 And UBound(ArrayCopy, 2) = 1 Then
        For i = 1 To UBound(ArrayPaste, 1)
            For j = 1 To UBound(ArrayPaste, 2)
                ArrayPaste(i, j) = ArrayCopy(1, 1)
            Next j
        Next i
    End If
    If UBound(ArrayCopy, 1) > 1 And UBound(ArrayCopy, 2) > 1 Then
        Row2 = Row1 + UBound(ArrayCopy, 1) - 1
        MSH1.RowSel = Row1 + UBound(ArrayCopy, 1) - 1
        Col2 = Col1 + UBound(ArrayCopy, 2) - 1
        MSH1.ColSel = Col1 + UBound(ArrayCopy, 2) - 1
        ReDim ArrayPaste(Row2 - Row1 + 1, Col2 - Col1 + 1)
        ArrayPaste = ArrayCopy
    End If
    If UBound(ArrayCopy, 1) = 1 And UBound(ArrayCopy, 2) > 1 Then
        ReDim ArrayPaste(UBound(ArrayPaste, 1), UBound(ArrayCopy, 2))
        Col2 = Col1 + UBound(ArrayCopy, 2) - 1
        MSH1.ColSel = Col1 + UBound(ArrayCopy, 2) - 1
        For i = 1 To UBound(ArrayPaste, 1)
            For j = 1 To UBound(ArrayCopy, 2)
                ArrayPaste(i, j) = ArrayCopy(1, j)
            Next j
        Next i
    End If
    If UBound(ArrayCopy, 1) > 1 And UBound(ArrayCopy, 2) = 1 Then
        ReDim ArrayPaste(UBound(ArrayCopy, 1), UBound(ArrayPaste, 2))
        Row2 = Row1 + UBound(ArrayCopy, 1) - 1
        MSH1.RowSel = Row1 + UBound(ArrayCopy, 1) - 1
        For i = 1 To UBound(ArrayCopy, 1)
            For j = 1 To UBound(ArrayPaste, 2)
                ArrayPaste(i, j) = ArrayCopy(i, 1)
            Next j
        Next i
    End If
    For i = Row1 To Row2
        Indexi = Indexi + 1
        Indexj = 0
        For j = Col1 To Col2
            Indexj = Indexj + 1
            Select Case j
                Case 1
                    If Val_(ArrayCopy(Indexi, Indexj)) <= Val_(MSH1.TextMatrix(i, 2)) Then
                        MSH1.TextMatrix(i, j) = Val_(ArrayPaste(Indexi, Indexj))
                    End If
                Case 3
                    If Val_(ArrayCopy(Indexi, Indexj)) >= Val_(MSH1.TextMatrix(i, 2)) Then
                        MSH1.TextMatrix(i, j) = Val_(ArrayPaste(Indexi, Indexj))
                    End If
                Case 4
                    MSH1.TextMatrix(i, j) = Val_(ArrayPaste(Indexi, Indexj))
            End Select
        Next j
    Next i
Exit Sub
1

MsgBox "Copied area exceeds the grid borders. Data could not be pasted.", vbCritical + vbOKOnly

End Sub

