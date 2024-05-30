VERSION 5.00
Object = "{831FDD16-0C5C-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCTL.OCX"
Object = "{5E9E78A0-531B-11CF-91F6-C2863C385E30}#1.0#0"; "MSFLXGRD.OCX"
Object = "{0D452EE1-E08F-101A-852E-02608C4D0BB4}#2.0#0"; "FM20.DLL"
Begin VB.Form frmRestrictions 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Restrictions"
   ClientHeight    =   5265
   ClientLeft      =   2580
   ClientTop       =   2160
   ClientWidth     =   8340
   FillColor       =   &H000000FF&
   Icon            =   "frmRestrictions.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   5265
   ScaleWidth      =   8340
   ShowInTaskbar   =   0   'False
   Begin VB.PictureBox picAssessmentPoints 
      Appearance      =   0  'Flat
      AutoRedraw      =   -1  'True
      ForeColor       =   &H80000008&
      Height          =   615
      Left            =   1920
      ScaleHeight     =   39
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   415
      TabIndex        =   8
      Top             =   3870
      Width           =   6255
      Begin VB.CommandButton cmdRemAssP 
         Appearance      =   0  'Flat
         Caption         =   "-"
         Height          =   255
         Left            =   5760
         TabIndex        =   10
         Top             =   180
         Width           =   375
      End
      Begin VB.CommandButton cmdAddAssP 
         Appearance      =   0  'Flat
         Caption         =   "+"
         Height          =   255
         Left            =   5400
         TabIndex        =   9
         Top             =   180
         Width           =   375
      End
   End
   Begin MSComctlLib.ImageList ImageList1 
      Left            =   120
      Top             =   4680
      _ExtentX        =   1005
      _ExtentY        =   1005
      BackColor       =   -2147483643
      ImageWidth      =   11
      ImageHeight     =   11
      MaskColor       =   12632256
      _Version        =   393216
      BeginProperty Images {2C247F25-8591-11D1-B16A-00C0F0283628} 
         NumListImages   =   4
         BeginProperty ListImage1 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmRestrictions.frx":000C
            Key             =   "plus"
         EndProperty
         BeginProperty ListImage2 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmRestrictions.frx":00B3
            Key             =   "flesh"
         EndProperty
         BeginProperty ListImage3 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmRestrictions.frx":0196
            Key             =   "end"
         EndProperty
         BeginProperty ListImage4 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmRestrictions.frx":023A
            Key             =   "minus"
         EndProperty
      EndProperty
   End
   Begin MSComctlLib.TreeView Tree 
      Height          =   3525
      Left            =   1920
      TabIndex        =   7
      Top             =   360
      Width           =   6255
      _ExtentX        =   11033
      _ExtentY        =   6218
      _Version        =   393217
      Indentation     =   0
      LabelEdit       =   1
      LineStyle       =   1
      Style           =   7
      FullRowSelect   =   -1  'True
      HotTracking     =   -1  'True
      BorderStyle     =   1
      Appearance      =   0
      OLEDropMode     =   1
   End
   Begin VB.ComboBox Combo 
      Appearance      =   0  'Flat
      Height          =   315
      ItemData        =   "frmRestrictions.frx":02DD
      Left            =   2160
      List            =   "frmRestrictions.frx":02DF
      Style           =   2  'Dropdown List
      TabIndex        =   6
      Top             =   1560
      Visible         =   0   'False
      Width           =   975
   End
   Begin VB.TextBox TxtEdit 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   285
      Left            =   2160
      TabIndex        =   5
      Text            =   "Text1"
      Top             =   1200
      Visible         =   0   'False
      Width           =   975
   End
   Begin MSFlexGridLib.MSFlexGrid MSH1 
      Height          =   4125
      Left            =   1920
      TabIndex        =   4
      Top             =   360
      Width           =   6255
      _ExtentX        =   11033
      _ExtentY        =   7276
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
   Begin VB.ListBox lstPanels 
      Appearance      =   0  'Flat
      Height          =   4125
      ItemData        =   "frmRestrictions.frx":02E1
      Left            =   120
      List            =   "frmRestrictions.frx":02E8
      TabIndex        =   3
      Top             =   360
      Width           =   1820
   End
   Begin MSComctlLib.TabStrip TabStrip 
      Height          =   4575
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   8295
      _ExtentX        =   14631
      _ExtentY        =   8070
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
   Begin VB.Shape shpAssP 
      BackColor       =   &H00C0C0C0&
      BorderStyle     =   6  'Inside Solid
      FillColor       =   &H00E0E0E0&
      FillStyle       =   0  'Solid
      Height          =   240
      Index           =   0
      Left            =   0
      Shape           =   4  'Rounded Rectangle
      Top             =   0
      Visible         =   0   'False
      Width           =   135
   End
   Begin VB.Shape shpAssScale 
      BackColor       =   &H00C0C0C0&
      BackStyle       =   1  'Opaque
      BorderStyle     =   6  'Inside Solid
      Height          =   300
      Left            =   960
      Top             =   4800
      Visible         =   0   'False
      Width           =   4920
   End
   Begin MSForms.CommandButton cmdCancel 
      Cancel          =   -1  'True
      Height          =   375
      Left            =   7200
      TabIndex        =   2
      Top             =   4680
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
      Left            =   6000
      TabIndex        =   1
      Top             =   4680
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
Attribute VB_Name = "frmRestrictions"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Dim ProjectIndex As Integer
Dim dBuffer As String
Dim Panel As cPanel, LoadCase As cLoadCase, header As cHeader
Dim colDesignVariables As New Collection
Dim colSC As New Collection
Dim colAssPoints As New Collection
Dim colGeometricalConstraints As New Collection
Dim GravityCenter As New cGlobalConstraints
Dim PanelIndex As Integer
Dim n As Node
Dim sUnit As String
Dim sCaption As String
Dim sOldVal As String
Dim lShpIndex As Integer
Dim lShpMinIndex As Integer
Dim lShpMaxIndex As Integer
Dim reg() As Long

Private Sub cmdAddAssP_Click()
    Dim cAssP As New cAssessmentPoint
    cAssP.index = colAssPoints.Item(PanelIndex).Count + 1
    cAssP.Value = 0.7
    colAssPoints.Item(PanelIndex).Add cAssP, cAssP.index
    Set cAssP = Nothing
    DrawAssessmentPoints
End Sub

Private Sub cmdCancel_Click()
    Unload Me
End Sub

Private Sub cmdOK_Click()
    Project.Item(ProjectIndex).DataChanged = True
    Unload Me
End Sub

Private Sub Form_Load()
    
    ProjectIndex = ActiveProject
    Me.Move (Screen.Width - Me.Width) / 2, (Screen.Height - Me.Height) / 2
    cmdOK.Picture = LoadResPicture("ID_BITMAP_OK", 0)
    cmdCancel.Picture = LoadResPicture("ID_BITMAP_CANCEL", 0)
    Me.Caption = "Plate Restrictions - [" & GetFileName(Project.Item(ProjectIndex).sFileName) & "]"
    GetData
    PopulatePanelList
    lstPanels.ListIndex = 0
    TabStripProperties
    TabStrip_Click
    FlexGrid
    FillGrid
    FillTree
    
End Sub

Private Sub DrawAssessmentPoints()
    Dim lp() As POINTAPI
    Dim lp1() As POINTAPI
    ReDim lp(0 To 1)
    picAssessmentPoints.Refresh
    lp(0).Y = 10
    lp(0).z = picAssessmentPoints.ScaleHeight / 2 - 10
    lp(1).Y = picAssessmentPoints.ScaleWidth - 70
    lp(1).z = picAssessmentPoints.ScaleHeight / 2 + 10

    Dim lrect As RECT
    lrect.Top = lp(0).z
    lrect.bottom = lp(1).z
    lrect.Left = lp(0).Y
    lrect.right = lp(1).Y
    Dim hBrush As Long
    hBrush = apiCreateSolidBrush(&HC0C0C0)
    
    FillRect picAssessmentPoints.hdc, lrect, hBrush
    Rectangle picAssessmentPoints.hdc, lp(0).Y, lp(0).z, lp(1).Y, lp(1).z
    DeleteObject hBrush
    Dim cAssP As cAssessmentPoint
    
    For Each cAssP In colAssPoints.Item(PanelIndex)
        ReDim lp1(0 To 1)
        lp1(0).Y = lp(0).Y - 5 + (lp(1).Y - lp(0).Y) * cAssP.Value
        lp1(0).z = lp(0).Y + 2
        lp1(1).Y = lp(0).Y + 5 + (lp(1).Y - lp(0).Y) * cAssP.Value
        lp1(1).z = lp(1).z - 2
        lrect.Top = lp1(0).z
        lrect.bottom = lp1(1).z
        lrect.Left = lp1(0).Y
        lrect.right = lp1(1).Y
        hBrush = apiCreateSolidBrush(&HE0E0E0)
        FillRect picAssessmentPoints.hdc, lrect, hBrush
        Rectangle picAssessmentPoints.hdc, lp1(0).Y, lp1(0).z, lp1(1).Y, lp1(1).z
        DeleteObject hBrush
    Next cAssP


'    Polyline picAssessmentPoints.hdc, lp(0), 4
'    Dim cAssP As cAssessmentPoint
'
'    Dim i As Integer
'    For i = shpAssP.UBound To shpAssP.LBound + 1 Step -1
'        Unload shpAssP.Item(i)
'    Next i
'    For Each cAssP In colAssPoints.Item(PanelIndex)
'        Load shpAssP.Item(cAssP.Index)
'        shpAssP.Item(cAssP.Index).left = shpAssScale.left + shpAssScale.WIDTH * cAssP.Value - shpAssP.Item(cAssP.Index).WIDTH / 2
'        shpAssP.Item(cAssP.Index).Visible = True
'        shpAssP.Item(cAssP.Index).ZOrder 0
'    Next cAssP
End Sub

Private Sub TabStripProperties()
    TabStrip.Tabs.Clear
    TabStrip.Tabs.Add 1, "Technological", "Technological"
    TabStrip.Tabs.Add 2, "Structural", "Structural"
    TabStrip.Tabs.Add 3, "Geometrical", "Geometrical"
    TabStrip.Tabs.Add 4, "Equality", "Equality"
    TabStrip.Tabs.Add 5, "GravityCenter", "Gravity Center"
End Sub

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
    Set colDesignVariables = Nothing
    Set colSC = Nothing
    Set colGeometricalConstraints = Nothing
    Set GravityCenter = Nothing
End Sub

Private Sub Form_Unload(Cancel As Integer)
    Set colDesignVariables = Nothing
    Set colSC = Nothing
    Set colAssPoints = Nothing
    Set colGeometricalConstraints = Nothing

    fMainForm.SetFocus
End Sub

Private Sub FlexGrid()
    Dim i As Integer
    MSH1.Clear
    MSH1.RowHeight(0) = 690
    Select Case TabStrip.SelectedItem.Key
        Case "Technological"
            MSH1.Rows = 10
            MSH1.Cols = 4
            MSH1.FormatString = "|^|^|^"
            MSH1.ColWidth(0) = 2000
            MSH1.ColWidth(1) = 900
            MSH1.ColWidth(2) = 900
            MSH1.ColWidth(3) = 900
            MSH1.TextMatrix(0, 0) = "Design Variable"
            MSH1.TextMatrix(0, 1) = "Lower Limit [mm]"
            MSH1.TextMatrix(0, 2) = "Current Value [mm]"
            MSH1.TextMatrix(0, 3) = "Upper Limit [mm]"
            MSH1.TextMatrix(1, 0) = "Plate Thickness"
            MSH1.TextMatrix(2, 0) = "Frames Web Height"
            MSH1.TextMatrix(3, 0) = "Frames Web Thickness"
            MSH1.TextMatrix(4, 0) = "Frames Flange Width"
            MSH1.TextMatrix(5, 0) = "Frames Spacing "
            MSH1.TextMatrix(6, 0) = "Stiffeners Web Height"
            MSH1.TextMatrix(7, 0) = "Stiffeners Web Thickness"
            MSH1.TextMatrix(8, 0) = "Stiffeners Flange Width"
            MSH1.TextMatrix(9, 0) = "Stiffeners Spacing"
            For i = 1 To MSH1.Rows - 1
                MSH1.Row = i: MSH1.col = 2
                MSH1.CellBackColor = &H8000000F
            Next i

        Case "Structural"
        Case "Geometrical"
        Case "Equality"
        Case "GravityCenter"
    End Select
End Sub

Private Sub FillGrid()
    Dim cPanel As cPanel
    Dim cDesignVariable As cDesignVariables
    Dim i As Integer
    Select Case TabStrip.SelectedItem.Key
        Case "Technological"
            For Each cDesignVariable In colDesignVariables.Item(PanelIndex)
                MSH1.TextMatrix(cDesignVariable.VariableName, 1) = cDesignVariable.LowerLimit * 1000
                MSH1.TextMatrix(cDesignVariable.VariableName, 3) = cDesignVariable.UpperLimit * 1000
            Next cDesignVariable
            MSH1.TextMatrix(1, 2) = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.NetThickness * 1000
            MSH1.TextMatrix(2, 2) = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryFrames.WebHeight * 1000
            MSH1.TextMatrix(3, 2) = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryFrames.WebThickness * 1000
            MSH1.TextMatrix(4, 2) = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryFrames.FlangeWidth * 1000
            MSH1.TextMatrix(5, 2) = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryFrames.Spacing * 1000
            MSH1.TextMatrix(6, 2) = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryStiffeners.WebHeight * 1000
            MSH1.TextMatrix(7, 2) = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryStiffeners.WebThickness * 1000
            MSH1.TextMatrix(8, 2) = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryStiffeners.FlangeWidth * 1000
            MSH1.TextMatrix(9, 2) = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryStiffeners.Spacing * 1000
        Case "Structural"
        Case "Geometrical"
        Case "Equality"
        Case "GravityCenter"
    End Select
End Sub

Private Sub FillTree()
    Dim lc As cLoadCase
    Dim sc As cStructuralConstraints
    Tree.Nodes.Clear
    With Tree.Nodes
        For Each lc In colSC.Item(PanelIndex)
            Set n = .Add(, , "n" & " " & PanelIndex & " " & lc.index, lc.Title)
                'Displacements
                Set n = .Add("n" & " " & PanelIndex & " " & lc.index, 4, "n" & " " & PanelIndex & " " & lc.index & " " & "disp", "Displacements")
                    'Absolute displacements
                    Set n = .Add("n" & " " & PanelIndex & " " & lc.index & " " & "disp", 4, "ref" & " " & PanelIndex & " " & lc.index & " " & "2", "U Absolute Displacement")
                    Set n = .Add("n" & " " & PanelIndex & " " & lc.index & " " & "disp", 4, "ref" & " " & PanelIndex & " " & lc.index & " " & "1", "V Absolute Displacement")
                    Set n = .Add("n" & " " & PanelIndex & " " & lc.index & " " & "disp", 4, "ref" & " " & PanelIndex & " " & lc.index & " " & "3", "W Absolute Displacement")
                    For Each sc In lc.colStructuralConstraints
                        Select Case sc.Reference
                            Case 1
                            Set n = .Add("ref" & " " & PanelIndex & " " & lc.index & " " & "1", 4, "restr" & " " & PanelIndex & " " & lc.index & " " & "1" & " " & sc.index, "Restriction")
                                Set n = .Add("restr" & " " & PanelIndex & " " & lc.index & " " & "1" & " " & sc.index, 4, "val" & " " & PanelIndex & " " & lc.index & " " & "1" & " " & sc.index & " " & "mm", "Value: " & sc.Value * 1000 & " mm")
                                Set n = .Add("restr" & " " & PanelIndex & " " & lc.index & " " & "1" & " " & sc.index, 4, "ass" & " " & PanelIndex & " " & lc.index & " " & "1" & " " & sc.index, "Point: " & colAssPoints.Item(PanelIndex).Item(sc.AssesmentPoint).Value)
                            Case 2
                            Set n = .Add("ref" & " " & PanelIndex & " " & lc.index & " " & "2", 4, "val" & " " & PanelIndex & " " & lc.index & " " & "1" & " " & sc.index & " " & "mm", sc.Value * 1000 & " mm")
                            Case 3
                            Set n = .Add("ref" & " " & PanelIndex & " " & lc.index & " " & "3", 4, "val" & " " & PanelIndex & " " & lc.index & " " & "1" & " " & sc.index & " " & "mm", sc.Value * 1000 & " mm")
                        End Select
                      Next sc
                    'relative displacements
        Next lc
    End With
'    For Each n In Tree.Nodes
'        If n.Children = 0 Then
'        Else
'            n.Image = "plus"
'        End If
'        n.ExpandedImage = "minus"
'    Next n
DrawAssessmentPoints
End Sub

Private Function getAssList() As String
    Dim ap As cAssessmentPoint
    getAssList = ""
    For Each ap In colAssPoints.Item(PanelIndex)
        If getAssList <> "" Then
            getAssList = getAssList & "; " & ap.Value
        Else
            getAssList = ap.Value
        End If
    Next ap
End Function

Private Sub setValues(ByVal Value As String)
        Dim sLine As String
        Dim v() As Variant
        Dim cSc As cStructuralConstraints
        sLine = Tree.SelectedItem.Key
        GetValues 5, sLine, v
        ' v(1) = val key; v(2) = panel index; v(3) = load case index; v(4) = reference index; v(5) = structural constraint index; v(6) = unit
        Select Case v(4) 'reference
            Case v(4)
            For Each cSc In colSC.Item(CInt(v(2))).Item(CInt(v(3))).colStructuralConstraints
                If cSc.Reference = CInt(v(4)) Then
                    cSc.Value = CDbl(Value) / 1000
                    'cSC.Point = CDbl(Value)
                    'setAssessmentPoint AssP
                    Exit For
                End If
            Next cSc
        End Select
End Sub

Private Sub setAssPoint(ByVal Value As String)
        Dim sLine As String
        Dim v() As Variant
        Dim bAssP As Boolean
        Dim cSc As cStructuralConstraints
        Dim cAP As cAssessmentPoint
        sLine = Tree.SelectedItem.Key
        GetValues 5, sLine, v
        ' v(1) = val key; v(2) = panel index; v(3) = load case index; v(4) = reference index; v(5) = structural constraint index; v(6) = unit
        Select Case v(4) 'reference
            Case v(4)
            For Each cSc In colSC.Item(CInt(v(2))).Item(CInt(v(3))).colStructuralConstraints
                If cSc.Reference = CInt(v(4)) Then
                    bAssP = False
                    For Each cAP In colAssPoints.Item(PanelIndex)
                        If cAP.Value = CDbl(Value) Then
                            cSc.AssesmentPoint = cAP.index
                            bAssP = True
                            Exit For
                        End If
                        If bAssP = False Then
                            If colAssPoints.Item(PanelIndex).Count = 4 Then
                                MsgBox "error. too many ass points", vbCritical + vbOKCancel
                                Exit Sub
                            Else
                                colAssPoints.Item(PanelIndex).Add cAP, colAssPoints.Item(PanelIndex).Count + 1
                            End If
                            
                        End If
                    Next cAP

                    'checkAssessmentPoints value, Cancel
                    'cSC.Point = CDbl(value)
                    Exit For
                End If
            Next cSc
        End Select
End Sub

Private Sub checkAssessmentPoints(ByVal Value As String, ByVal Cancel As Boolean)
    Dim cAP As cAssessmentPoint
    For Each cAP In colAssPoints.Item(PanelIndex)
        If cAP.Value = CDbl(Value) Then
        End If
    Next cAP
End Sub

Private Sub picAssessmentPoints_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
    Dim i As Integer
    If Button = 1 Then
        For i = shpAssP.LBound + 1 To shpAssP.UBound
            If colAssPoints.Item(PanelIndex).Item(i).Value = 0 Then
                lShpMinIndex = i
            ElseIf colAssPoints.Item(PanelIndex).Item(i).Value = 1 Then
                lShpMaxIndex = i
            ElseIf X >= shpAssP.Item(i).Left And X <= shpAssP.Item(i).Left + shpAssP.Item(i).Width Then '
                lShpIndex = i
            End If
        Next i
    End If
End Sub

Private Sub picAssessmentPoints_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
    If Button = 1 Then
        If X + shpAssP.Item(lShpIndex).Width / 2 <= shpAssP.Item(lShpMaxIndex).Left Then
            If X - shpAssP.Item(lShpIndex).Width / 2 >= shpAssP.Item(lShpMinIndex).Left + shpAssP.Item(lShpMinIndex).Width Then
                shpAssP.Item(lShpIndex).Move (X) - shpAssP.Item(lShpIndex).Width / 2
            End If
        End If
    End If
End Sub

Private Sub picAssessmentPoints_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
    lShpIndex = 0
End Sub

Private Sub Tree_AfterLabelEdit(Cancel As Integer, NewString As String)
    Dim sLine As String
    Dim v() As Variant
    Dim bCancel As Boolean
    GetValues 1, NewString, v
    NewString = v(1)
    txtEdit.Text = NewString
    ValidateNumeric txtEdit, bCancel
    Cancel = 1
    If bCancel = False Then
        Tree.Nodes(Tree.SelectedItem.Key).Text = sCaption & "  " & NewString & "  " & sUnit
        Select Case Left(Tree.SelectedItem.Key, 3)
            Case "val"
                setValues NewString
            Case "ass"
                setAssPoint NewString
        End Select
    ElseIf bCancel = True Then
        Tree.Nodes(Tree.SelectedItem.Key).Text = sCaption & "  " & sOldVal & "  " & sUnit
    End If
    
End Sub

Private Sub Tree_BeforeLabelEdit(Cancel As Integer)
    Tree.Nodes(Tree.SelectedItem.Key).Text = sCaption & "  " & sOldVal & "  " & sUnit
End Sub

Private Sub Tree_Click()
    'Tree.LabelEdit = tvwManual
End Sub

Private Sub Tree_DblClick()
    Dim v() As Variant
    Select Case Left(Tree.SelectedItem.Key, 3)
        Case "val", "ass"
            GetValues 3, Tree.SelectedItem.Text, v
            Tree.SelectedItem.Text = v(2)
            sCaption = v(1)
            sOldVal = v(2)
            sUnit = v(3)
            Tree.LabelEdit = tvwAutomatic
            Tree.StartLabelEdit
        Case Else
            Tree.LabelEdit = tvwManual
    End Select
End Sub

Private Sub PopulateCombo()

End Sub

Private Sub PopulatePanelList()
    lstPanels.Clear
    Dim i As Integer
    For Each Panel In Project.Item(ProjectIndex).colPanel
        If Panel.pType = Plate Then
            lstPanels.AddItem "Panel " & Panel.index
        End If
    Next Panel
End Sub

Private Sub GetData()
    For Each Panel In Project.Item(ProjectIndex).colPanel
        colDesignVariables.Add Panel.colDesignVariables.Clone
        colSC.Add Panel.colLoadCase.Clone
        colAssPoints.Add Panel.colAssessmentPoints.Clone
        colGeometricalConstraints.Add Panel.colGeometricalConstraints.Clone
    Next Panel
    Set GravityCenter = Project.Item(ProjectIndex).cHeader.cGlobalConstraints.Clone
End Sub

Private Sub SetData()

End Sub

' =============
' FLEXGRID EDIT
' =============

Function grd_index(r As Integer, c As Integer)
    grd_index = c + MSH1.Cols * r
End Function

Private Sub lstPanels_Click()
    Dim v() As Variant
    Dim sData As String
    'UpdateGirders
    sData = lstPanels.Text
    GetValues 2, sData, v
    PanelIndex = Val_(v(2))
    MSH1.Clear
    FlexGrid
    FillGrid
    FillTree
End Sub

Private Sub MSH1_KeyDown(KeyCode As Integer, Shift As Integer)
    'If Len(MSH1) > 0 Then dBuffer = (MSH1)
    TxtEdit_KeyDown KeyCode, Shift
End Sub

Private Sub MSH1_KeyPress(KeyAscii As Integer)
        Select Case TabStrip.SelectedItem.Key
        Case "Geometry"
            Select Case MSH1.col
                Case 2
                    MSHFlexGridEdit MSH1, txtEdit, KeyAscii
                Case 4, 5
                    PopulateCombo
                    MSHFlexGridEdit MSH1, Combo, KeyAscii
                Case Else
            End Select
        Case "Sections"
            Select Case MSH1.col
                Case 1
                    PopulateCombo
                    MSHFlexGridEdit MSH1, Combo, KeyAscii
                Case Else
                    Select Case MSH1.Text
                        Case "-------"
                        Case Else
                            MSHFlexGridEdit MSH1, txtEdit, KeyAscii
                    End Select
            End Select
        Case "Materials"
            MSHFlexGridEdit MSH1, txtEdit, KeyAscii
    End Select

End Sub

Private Sub MSH1_DblClick()
    Select Case TabStrip.SelectedItem.Key
        Case "Geometry"
            Select Case MSH1.col
                Case 2
                    MSHFlexGridEdit MSH1, txtEdit, 32
                Case 4, 5
                    PopulateCombo
                    MSHFlexGridEdit MSH1, Combo, 32
                Case Else
            End Select
        Case "Sections"
            Select Case MSH1.col
                Case 1
                    PopulateCombo
                    MSHFlexGridEdit MSH1, Combo, 32
                Case Else
                    Select Case MSH1.Text
                        Case "-------"
                        Case Else
                            MSHFlexGridEdit MSH1, txtEdit, 32
                    End Select
            End Select
        Case "Materials"
            MSHFlexGridEdit MSH1, txtEdit, 32
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
End Sub

Private Sub MSH1_Scroll()
    txtEdit.Visible = False
    Combo.Visible = False
End Sub

Private Sub TabStrip_Change()
    txtEdit.Visible = False
    Combo.Visible = False
    MSH1.Row = 1
    MSH1.col = 1
    FlexGrid
    FillGrid
End Sub

Private Sub TabStrip_BeforeClick(Cancel As Integer)
    If txtEdit.Visible = True Then
       Cancel = -1
    End If
    If Combo.Visible = True Then
       Cancel = -1
    End If
End Sub

Private Sub TabStrip_Click()
    MSH1.Visible = False
    Tree.Visible = False
    Select Case TabStrip.SelectedItem
        Case "Technological"
            MSH1.Visible = True
            txtEdit.Visible = False
            Combo.Visible = False
            picAssessmentPoints.Visible = False
            MSH1.Row = 1
            MSH1.col = 1
            FlexGrid
            FillGrid
        Case "Structural"
            Tree.Visible = True
            picAssessmentPoints.Visible = True
            DrawAssessmentPoints
            FillTree
    End Select
End Sub

Private Sub Tree_KeyPress(KeyAscii As Integer)
    '    Dim v() As Variant
    '    Dim sLine As String
    '    GetValues 1, Tree.SelectedItem.Key, v
    '    If v(1) = "val" Then
    '        Tree.Nodes(Tree.SelectedItem.Key).Text = Chr(KeyAscii)
    '        Tree_DblClick
    '    End If
    Select Case KeyAscii
        Case 116    '"T"
            frmInputStructRestr.Show 1, Me
    End Select
End Sub

Private Sub Tree_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
'picAssPct.left = x
'picAssPct.top = y
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
    EditKeyCode MSH1, txtEdit, KeyCode, Shift
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
    If txtEdit.Visible = False Then GoTo 1 ' Exit Sub
    MSH1 = txtEdit
    txtEdit.Visible = False
1:
    If Combo.Visible = False Then Exit Sub
    MSH1 = Combo.List(Combo.ListIndex)
    Combo.Visible = False
End Sub

Private Sub MSH1_LeaveCell()
    If txtEdit.Visible = False Then GoTo 1 'Exit Sub
    MSH1 = txtEdit
    txtEdit.Visible = False
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

Private Sub Combo_LostFocus()
    MSH1_GotFocus
    SetData
'    Select Case TabStrip.SelectedItem.Key
'        Case "Sections"
'            FlexGrid
'            FillGrid
'    End Select
End Sub

Private Sub TxtEdit_Validate(Cancel As Boolean)
    ValidateNumeric txtEdit, Cancel
    If Cancel = True Then txtEdit = dBuffer
End Sub

