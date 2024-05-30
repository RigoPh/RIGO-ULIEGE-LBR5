VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "comdlg32.ocx"
Object = "{0D452EE1-E08F-101A-852E-02608C4D0BB4}#2.0#0"; "FM20.DLL"
Object = "{65E121D4-0C60-11D2-A9FC-0000F8754DA1}#2.0#0"; "mschrt20.ocx"
Begin VB.Form frmChartObjectiveFunction 
   BorderStyle     =   5  'Sizable ToolWindow
   Caption         =   "Objective Function"
   ClientHeight    =   6990
   ClientLeft      =   1740
   ClientTop       =   3165
   ClientWidth     =   9375
   Icon            =   "frmObjectiveFunction.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   6990
   ScaleWidth      =   9375
   ShowInTaskbar   =   0   'False
   Begin VB.PictureBox picSelectView 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      ForeColor       =   &H80000008&
      Height          =   855
      Left            =   6360
      ScaleHeight     =   825
      ScaleWidth      =   945
      TabIndex        =   2
      Top             =   1080
      Visible         =   0   'False
      Width           =   975
      Begin MSForms.CheckBox chkInertia 
         Height          =   345
         Left            =   120
         TabIndex        =   5
         Top             =   480
         Width           =   840
         VariousPropertyBits=   1015023635
         BackColor       =   -2147483633
         ForeColor       =   -2147483630
         DisplayStyle    =   4
         Size            =   "1482;609"
         Value           =   "1"
         Caption         =   "Inertia"
         FontHeight      =   165
         FontCharSet     =   0
         FontPitchAndFamily=   2
      End
      Begin MSForms.CheckBox chkWeight 
         Height          =   345
         Left            =   120
         TabIndex        =   4
         Top             =   240
         Width           =   915
         VariousPropertyBits=   1015023635
         BackColor       =   -2147483633
         ForeColor       =   -2147483630
         DisplayStyle    =   4
         Size            =   "1614;609"
         Value           =   "1"
         Caption         =   "Weight"
         FontHeight      =   165
         FontCharSet     =   0
         FontPitchAndFamily=   2
      End
      Begin MSForms.CheckBox chkCost 
         Height          =   345
         Left            =   120
         TabIndex        =   3
         Top             =   0
         Width           =   720
         VariousPropertyBits=   1015023635
         BackColor       =   -2147483633
         ForeColor       =   -2147483630
         DisplayStyle    =   4
         Size            =   "1270;609"
         Value           =   "1"
         Caption         =   "Cost"
         FontHeight      =   165
         FontCharSet     =   0
         FontPitchAndFamily=   2
      End
   End
   Begin VB.PictureBox Picture1 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      ForeColor       =   &H80000008&
      Height          =   1695
      Left            =   1800
      ScaleHeight     =   1665
      ScaleWidth      =   1785
      TabIndex        =   1
      Top             =   2400
      Visible         =   0   'False
      Width           =   1815
   End
   Begin MSComDlg.CommonDialog CommonDialog1 
      Left            =   360
      Top             =   1800
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
   End
   Begin MSChart20Lib.MSChart MSChart1 
      Height          =   1575
      Left            =   -120
      OleObjectBlob   =   "frmObjectiveFunction.frx":000C
      TabIndex        =   0
      Top             =   0
      Width           =   2295
   End
   Begin VB.Image Image1 
      Appearance      =   0  'Flat
      Height          =   1695
      Left            =   2880
      Top             =   360
      Visible         =   0   'False
      Width           =   2535
   End
   Begin VB.Menu mnuSnapshop 
      Caption         =   "&Snapshot"
   End
   Begin VB.Menu mnuPrint 
      Caption         =   "&Print"
   End
End
Attribute VB_Name = "frmChartObjectiveFunction"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Dim fso As New FileSystemObject, fil As file, ts As TextStream, sFile As String
Dim ProjectIndex As Integer
Dim aObjFct() As Variant
Dim sOptiType As String
Dim sOptiLegend As String
Dim iNumCols As Integer
Dim ICOUT As Integer, ITERAM As Integer, IMULTI As Integer
Dim bSelectView(0 To 3) As Boolean
Dim w(2) As Double

Private Sub chkCost_Click()
        bSelectView(0) = chkCost.Value
        bSelectView(1) = chkWeight.Value
        bSelectView(2) = chkInertia.Value
        GraphDrawMulti
End Sub

Private Sub chkInertia_Click()
        bSelectView(0) = chkCost.Value
        bSelectView(1) = chkWeight.Value
        bSelectView(2) = chkInertia.Value
        GraphDrawMulti
End Sub

Private Sub chkWeight_Click()
        bSelectView(0) = chkCost.Value
        bSelectView(1) = chkWeight.Value
        bSelectView(2) = chkInertia.Value
        GraphDrawMulti
End Sub

Private Sub Form_Load()
    ProjectIndex = ActiveProject
    Me.Move (Screen.Width - Me.Width) / 2, (Screen.Height - Me.Height) / 2
    Me.Caption = "Objective Function Variation - [" & GetFileName(Project.Item(ProjectIndex).sFileName) & "]"
    If ReadFile = False Then
       Unload Me
       Exit Sub
    End If
    GraphSetup
    If IMULTI = 0 Then
        If GraphDrawUni = False Then
            Unload Me
            Exit Sub
        End If
    ElseIf IMULTI = 1 Then
        picSelectView.Visible = True
        bSelectView(0) = chkCost.Value
        bSelectView(1) = chkWeight.Value
        bSelectView(2) = chkInertia.Value
        If GraphDrawMulti = False Then
            Unload Me
            Exit Sub
        End If
    End If
End Sub

Private Sub Form_Resize()
    With MSChart1
        .Left = ScaleLeft
        .Top = ScaleTop
        .Width = ScaleWidth
        .Height = ScaleHeight
    End With
    With Picture1
        .Left = ScaleLeft
        .Top = ScaleTop
        .Width = ScaleWidth
        .Height = ScaleHeight
    End With
    With picSelectView
        .Left = ScaleLeft + ScaleWidth - .Width - 60
        .Top = ScaleTop + 60
    End With
End Sub

Private Function GraphDrawMulti() As Boolean
    On Error GoTo GraphDrawMultiErr
    Dim min As Double, max As Double
    Dim i As Integer, j As Integer
    Dim index As Integer
    'compute scale
    min = 1000000000
    max = -1000000000
    For i = 0 To ITERAM
        For j = 0 To 2
            If w(j) > 0 Then
                If bSelectView(j) = True Then
                    If aObjFct(i, j) < min Then
                        min = aObjFct(i, j)
                    End If
                    If aObjFct(i, j) > max Then
                        max = aObjFct(i, j)
                    End If
                End If
            End If
        Next j
    Next i

    With MSChart1
        .Plot.Axis(VtChAxisIdY).ValueScale.Auto = False
        .Plot.Axis(VtChAxisIdY).ValueScale.Minimum = min - 5 / 100 * min
        .Plot.Axis(VtChAxisIdY).ValueScale.Maximum = max + 5 / 100 * max
        .Plot.Axis(VtChAxisIdY).ValueScale.MajorDivision = 0 ' (max - min) / 10
    End With
    
    
    'draw series
    With MSChart1
        For i = 0 To ITERAM
            .Row = i + 1
            .RowLabel = i
            For j = 0 To 2
                .Column = j + 1
                If bSelectView(j) = True Or w(j) = 0 Then
                    .Data = aObjFct(i, j)
                Else
                    .Data = ""
                End If
            Next j
        Next i
    End With
    
    For i = 1 To 3
        With MSChart1.Plot.SeriesCollection(i).DataPoints(-1).DataPointLabel
            If bSelectView(i - 1) = True Then
                '.Custom = False
                .Component = VtChLabelComponentValue
                .LocationType = VtChLabelLocationTypeAbovePoint
            Else
                '.Custom = False
                .LocationType = VtChLabelLocationTypeNone
            End If
        End With
    Next i
    
    'markers
    If bSelectView(0) = True Then
        MSChart1.Plot.SeriesCollection(1).SeriesMarker.Auto = False
        With MSChart1.Plot.SeriesCollection(1).DataPoints(-1).Marker
            .Visible = True
            .Style = VtMarkerStyleCircle
            .size = 110
            .Pen.Width = 1
            .Pen.VtColor.Set 255, 0, 0
        End With
    Else
        With MSChart1.Plot.SeriesCollection(1).DataPoints(-1).Marker
           .Visible = False
        End With
    End If
    
    If bSelectView(1) = True Then
        MSChart1.Plot.SeriesCollection(2).SeriesMarker.Auto = False
        With MSChart1.Plot.SeriesCollection(2).DataPoints(-1).Marker
            .Visible = True
            .Style = VtMarkerStyleDownTriangle
            .size = 110
            .Pen.Width = 1
            .Pen.VtColor.Set 255, 0, 0
        End With
    Else
        With MSChart1.Plot.SeriesCollection(2).DataPoints(-1).Marker
           .Visible = False
        End With
    End If
    
    If bSelectView(2) = True Then
        MSChart1.Plot.SeriesCollection(3).SeriesMarker.Auto = False
        With MSChart1.Plot.SeriesCollection(3).DataPoints(-1).Marker
            .Visible = True
            .Style = VtMarkerStyleSquare
            .size = 110
            .Pen.Width = 1
            .Pen.VtColor.Set 255, 0, 0
        End With
    Else
        With MSChart1.Plot.SeriesCollection(3).DataPoints(-1).Marker
           .Visible = False
        End With
    End If
    
    
'    For i = 1 To 3
'        If bSelectView(i - 1) = True Then
'            MSChart1.Plot.SeriesCollection(i).SeriesMarker.Auto = False
'            With MSChart1.Plot.SeriesCollection(i).DataPoints(-1).Marker
'                .Visible = True
'                .Style = VtMarkerStyleCircle
'                .size = 110
'                .Pen.Width = 1
'            End With
'        Else
'            With MSChart1.Plot.SeriesCollection(i).DataPoints(-1).Marker
'               .Visible = True
'            End With
'        End If
'    Next i
    
'    With MSChart1
'        Select Case ICOUT
'            Case -1
'                .FootnoteText = "Gain: " & -Round((aObjFct(0, 0) - aObjFct(ITERAM, 0)) / aObjFct(0, 0) * 100, 3) & "%"
'            Case Else
'                .FootnoteText = "Gain: " & Round((aObjFct(0, 0) - aObjFct(ITERAM, 0)) / aObjFct(0, 0) * 100, 3) & "%"
'        End Select
'    End With

    Dim gain(2) As String
    
    If w(0) > 0 Then gain(0) = "Cost = " & Round((aObjFct(0, 0) - aObjFct(ITERAM, 0)) / aObjFct(0, 0) * 100, 3) & "%; "
    If w(1) > 0 Then gain(1) = "Weight = " & Round((aObjFct(0, 1) - aObjFct(ITERAM, 1)) / aObjFct(0, 1) * 100, 3) & "%; "
    If w(2) > 0 Then gain(2) = "Inertia = " & Round(-(aObjFct(0, 2) - aObjFct(ITERAM, 2)) / aObjFct(0, 2) * 100, 3) & "%; "
    
    MSChart1.FootnoteText = "Gain: " & gain(0) & gain(1) & gain(2)

    GraphDrawMulti = True
    Exit Function
GraphDrawMultiErr:
    GraphDrawMulti = False
End Function

Private Function GraphDrawUni() As Boolean
    On Error GoTo GraphDrawUniErr
    Dim min As Double, max As Double
    Dim i As Integer
    
    'compute scale
    min = 1000000000
    max = -1000000000
    For i = 0 To ITERAM
        If aObjFct(i, 0) < min Then
            min = aObjFct(i, 0)
        End If
        If aObjFct(i, 0) > max Then
            max = aObjFct(i, 0)
        End If
    Next i
    
    With MSChart1
        .Plot.Axis(VtChAxisIdY).ValueScale.Auto = False
        .Plot.Axis(VtChAxisIdY).ValueScale.Minimum = min - 5 / 100 * min
        .Plot.Axis(VtChAxisIdY).ValueScale.Maximum = max + 5 / 100 * max
        .Plot.Axis(VtChAxisIdY).ValueScale.MajorDivision = 0 ' (max - min) / 10
    End With
    
    'draw series
    With MSChart1
       For i = 0 To ITERAM
           .Row = i + 1
           .RowLabel = i
           .Column = 1
           .Data = aObjFct(i, 0)
       Next i
    End With
     
    With MSChart1.Plot.SeriesCollection(1).DataPoints(-1).DataPointLabel
        .Custom = False
        .Component = VtChLabelComponentValue
        .LocationType = VtChLabelLocationTypeAbovePoint
    End With

    MSChart1.Plot.SeriesCollection(1).SeriesMarker.Auto = False
    With MSChart1.Plot.SeriesCollection(1).DataPoints(-1).Marker
        .Visible = True
        .Style = VtMarkerStyleCircle
        .size = 110
        .Pen.Width = 1
    End With
    
    'footnote - gain
    With MSChart1
        Select Case ICOUT
            Case -1
                .FootnoteText = "Gain: " & -Round((aObjFct(0, 0) - aObjFct(ITERAM, 0)) / aObjFct(0, 0) * 100, 3) & "%"
            Case Else
                .FootnoteText = "Gain: " & Round((aObjFct(0, 0) - aObjFct(ITERAM, 0)) / aObjFct(0, 0) * 100, 3) & "%"
        End Select
    End With
    GraphDrawUni = True
    Exit Function
GraphDrawUniErr:
    GraphDrawUni = False
End Function

Private Function GraphSetup() As Boolean
    
    MSChart1.ShowLegend = True
    MSChart1.AllowSelections = False
    If IMULTI = 0 Then
        MSChart1.ColumnCount = 1
    ElseIf IMULTI = 1 Then
        MSChart1.ColumnCount = 3
    End If
    MSChart1.RowCount = ITERAM + 1
    
    MSChart1.chartType = VtChChartType2dLine
    
    'Title
    Select Case ICOUT
        Case -1
            sOptiType = "Maximum Inertia"
            sOptiLegend = "Inertia [m4]"
        Case 0
            sOptiType = "Minimum Weight"
            sOptiLegend = "Weight [T]"
        Case 1
            sOptiType = "Minimum Cost"
            sOptiLegend = "Cost [€]"
    End Select
    If IMULTI = 1 Then sOptiType = "Multi Criteria"
    
    MSChart1.TitleText = "Objective - " & sOptiType ' "Objective Function - Weight"
    With MSChart1.Title.VtFont
        .Name = "MS Sans Serif"
        .Style = VtFontStyle.VtFontStyleItalic
        .size = 12
        .Effect = VtFontEffectUnderline
    End With
    
    'Texte de la légende
    With MSChart1.Legend
        .VtFont.Name = "MS Sans Serif"
        .VtFont.Style = VtFontStyle.VtFontStyleItalic
        .VtFont.size = 8
        .Location.LocationType = VtChLocationTypeBottom              ' La légende sera au-dessus
        .TextLayout.HorzAlignment = VtHorizontalAlignmentCenter    ' Alignement centré
        .VtFont.VtColor.Set 0, 0, 0                            ' couleur texte.
        .Backdrop.Fill.Style = VtFillStyleBrush                    ' Style de fond
        .Backdrop.Fill.Brush.Style = VtBrushStyleSolid 'VtBrushStyleHatched           ' Style de remplissage
        .Backdrop.Fill.Brush.FillColor.Set 255, 255, 255             ' Couleur de remplissage du fond de la
    End With
    
    With MSChart1.Plot
        If IMULTI = 0 Then
            .SeriesCollection(1).LegendText = sOptiLegend
        ElseIf IMULTI = 1 Then
        
            .SeriesCollection(1).LegendText = "Cost [€]"
            .SeriesCollection(2).LegendText = "Weight [T]"
            .SeriesCollection(3).LegendText = "Inertia [m4]"
            '.SeriesCollection(4).LegendText = "Combined [-]"
        End If
    End With
        
                   
    ' Titre pour l'axe des X
    With MSChart1.Plot.Axis(0, 1)
         .AxisTitle.VtFont.size = 10
         .AxisTitle.Visible = True
         .AxisTitle.Text = "Iterations"
    End With
    
     'Changer la couleur du graphe
    With MSChart1.Plot.SeriesCollection(1)
        .DataPoints(-1).Brush.FillColor.Set 0, 128, 255      ' couleur RGB
    End With
    
    ' Changer la couleur du fond du graphe
    With MSChart1.Backdrop.Fill
        .Style = VtFillStyleBrush
        .Brush.FillColor.Set 255, 255, 255   'fond blanc
    End With

End Function

'Private Function SetGraph() As Boolean
'    Dim i As Integer
'    Dim min As Double, max As Double
'    With grphObjectiveFunction
'        'background
'        '.Backdrop.Fill.Style = VtFillStyleBrush
'        '.Backdrop.Fill.Brush.Style = VtBrushStyleHatched
'        '.Backdrop.Fill.Brush.FillColor.Set 159, 0, 255
'
'        'title
'        .TitleText = "Objective Function - Weight"
'        .Title.VtFont.size = 15
'        .Title.VtFont.Effect = VtFontEffectUnderline
'
'        'y axis title
'        .Plot.Axis(VtChAxisIdY2).AxisTitle.VtFont.size = 10
'        .Plot.Axis(VtChAxisIdY2).AxisTitle.Visible = True
'        .Plot.Axis(VtChAxisIdY2).AxisTitle.Text = "Objective Function"
'        .Plot.Axis(VtChAxisIdY2).AxisTitle.TextLayout.Orientation = VtOrientationUp ' VtOrientationVertical  ' '.VertAlignment = VtVerticalAlignmentCenter
'
'        .AllowSelections = False
'
'        'legend
'        .chartType = VtChChartType2dLine
'        .ShowLegend = True
'        .Legend.TextLayout.HorzAlignment = VtHorizontalAlignmentCenter
'        .Legend.Location.LocationType = VtChLocationTypeBottom
'
'        'axis
'        .Plot.Axis(2, 1).AxisTitle.Visible = True
'        '.Plot.Axis(2, 1).AxisTitle.Text = "[T]"
'        .Plot.Axis(2, 1).AxisTitle.TextLayout.Orientation = VtOrientationHorizontal
'        .RowCount = NoOfIte '+ 1
'        .ColumnCount = 1
'        For i = 0 To NoOfIte - 1 '- 1
'            .Row = i + 1
'            .Column = 1
'            .Data = aObjFct(i, 1)
'
'        Next i
'        .Plot.SeriesCollection(1).LegendText = "Weight [T]"
'
'        'show values
'        .Plot.SeriesCollection(1).DataPoints(-1).DataPointLabel.Custom = False
'        .Plot.SeriesCollection(1).DataPoints(-1).DataPointLabel.Component = VtChLabelComponentValue
'        .Plot.SeriesCollection(1).DataPoints(-1).DataPointLabel.LocationType = VtChLabelLocationTypeAbovePoint
'
'        ' compute scale
'        min = 1000000000
'        max = -1000000000
'        For i = 0 To NoOfIte - 1
'            If aObjFct(i, 1) < min Then
'                min = aObjFct(i, 1)
'            End If
'            If aObjFct(i, 1) > max Then
'                max = aObjFct(i, 1)
'            End If
'        Next i
'        .Plot.Axis(VtChAxisIdY).ValueScale.Auto = False
'        .Plot.Axis(VtChAxisIdY).ValueScale.Minimum = min - 1 / 100 * min
'        .Plot.Axis(VtChAxisIdY).ValueScale.Maximum = max + 1 / 100 * max
'
'    End With
'
'
'
'End Function

Private Function ReadFile() As Boolean
    On Error GoTo ReadFileErr
    
    Dim sPrefix As String
    Dim sLine As String, find As String
    Dim v() As Variant
    
    'get file name
    sFile = Project.Item(ProjectIndex).sFileName
    sPrefix = "opt-"
    sFile = GetFilePath(sFile) & "opt-" & GetFileRoot(sFile) & ".txt"
    
    If Not fso.FileExists(sFile) Then
        MsgBox "File '" & sFile & "' not found.", vbCritical + vbOKOnly
        Exit Function
    End If
    Set fil = fso.GetFile(sFile)
    Set ts = fil.OpenAsTextStream(ForReading)
    
    'ICOUT
    find = ""
    Do Until find = "icout" Or ts.AtEndOfStream = True
        sLine = ReadLn(ts)
        If ts.AtEndOfStream = True Then
            MsgBox "File not read succesfully (ICOUT)!", vbCritical + vbOKOnly
        End If
        find = LCase(Left(sLine, 5))
    Loop
    GetValues 2, sLine, v
    ICOUT = Val_(v(2))
    
    'ITERAM
    find = ""
    Do Until find = "iteram" Or ts.AtEndOfStream = True
        sLine = ReadLn(ts)
        If ts.AtEndOfStream = True Then
            MsgBox "File not read succesfully (ITERAM)!", vbCritical + vbOKOnly
        End If
        find = LCase(Left(sLine, 6))
    Loop
    GetValues 2, sLine, v
    ITERAM = Val_(v(2))
    
    ReDim aObjFct(0 To ITERAM, 0 To 3)
    
    'IMULTI
    find = ""
    Do Until find = "imulti" Or ts.AtEndOfStream = True
        sLine = ReadLn(ts)
        If ts.AtEndOfStream = True Then
            MsgBox "File not read succesfully (IMULTI)!", vbCritical + vbOKOnly
        End If
        find = LCase(Left(sLine, 6))
    Loop
    GetValues 2, sLine, v
    IMULTI = Val_(v(2))
    
    'WEIGHTS
    find = ""
    Do Until find = "weights" Or ts.AtEndOfStream = True
        sLine = ReadLn(ts)
        If ts.AtEndOfStream = True Then
            MsgBox "File not read succesfully (IMULTI)!", vbCritical + vbOKOnly
        End If
        find = LCase(Left(sLine, 7))
    Loop
    GetValues 4, sLine, v
    w(0) = Val_(v(2)) 'cost
    w(1) = Val_(v(3)) 'weight
    w(2) = Val_(v(4)) 'inertia
    
    If w(0) = 0 Then
        chkCost.Value = 0
        chkCost.Enabled = False
    End If
    If w(1) = 0 Then
        chkWeight.Value = 0
        chkWeight.Enabled = False
    End If
    If w(2) = 0 Then
        chkInertia.Value = 0
        chkInertia.Enabled = False
    End If
    
    If IMULTI = 0 Then
        ReadUniObj ts
    ElseIf IMULTI = 1 Then
        ReadMultiObj ts
    End If
    
    ts.Close
    Set ts = Nothing
    ReadFile = True
    Exit Function
ReadFileErr:
    ReadFile = False
End Function

Private Function ReadMultiObj(ts As TextStream) As Boolean
    On Error GoTo ReadUniObjErr
    Dim find As String, FirstOcc As String, sLine As String
    Dim v() As Variant
    Dim sKey As String
    Dim i As Integer

    
    'READ COST
    If w(0) > 0 Then
        ts.Close
        Set ts = fil.OpenAsTextStream(ForReading)
        'initial values
        sKey = "COUT-COST"
        Do Until find = sKey Or ts.AtEndOfStream = True
            sLine = ReadLn(ts)
            sLine = RemoveBlanksTabs(sLine)
            find = Left(sLine, Len(sKey))
        Loop
        FirstOcc = InStr(1, sLine, "=")
        sLine = Mid(sLine, FirstOcc + 1)
        GetValues 1, sLine, v
        aObjFct(0, 0) = Round((Val_(v(1))), 3)
        
        'skip ITERAM times
        sKey = "fctobjectifcout(recalculée)"
        For i = 1 To ITERAM
            find = ""
            Do Until find = sKey Or ts.AtEndOfStream = True
                sLine = ReadLn(ts)
                sLine = RemoveBlanksTabs(sLine)
                sLine = LCase(sLine)
                find = Left(sLine, Len(sKey))
                If ts.AtEndOfStream = True Then Exit Do
            Loop
        Next i

        'read cost objective function
        For i = 1 To ITERAM
            find = ""
            Do Until find = sKey Or ts.AtEndOfStream = True
                sLine = ReadLn(ts)
                sLine = RemoveBlanksTabs(sLine)
                sLine = LCase(sLine)
                find = Left(sLine, Len(sKey))
                If ts.AtEndOfStream = True Then Exit Do
            Loop
            FirstOcc = InStr(1, sLine, "=")
            sLine = Mid(sLine, FirstOcc + 1)
            GetValues 1, sLine, v
            aObjFct(i, 0) = Round((Val_(v(1))), 3)
        Next i
        
    End If

    'READ WEIGHT
    If w(1) > 0 Then
        ts.Close
        Set ts = fil.OpenAsTextStream(ForReading)
        'initial values
        sKey = "POIDS-WEIGHT"
        Do Until find = sKey Or ts.AtEndOfStream = True
            sLine = ReadLn(ts)
            sLine = RemoveBlanksTabs(sLine)
            find = Left(sLine, Len(sKey))
        Loop
        FirstOcc = InStr(1, sLine, "=")
        sLine = Mid(sLine, FirstOcc + 1)
        GetValues 1, sLine, v
        aObjFct(0, 1) = Round((Val_(v(1))), 3) / 10000
        
        'skip ITERAM times
        sKey = "fctobjectifpoids(recalculée)"
        For i = 1 To ITERAM
            find = ""
            Do Until find = sKey Or ts.AtEndOfStream = True
                sLine = ReadLn(ts)
                sLine = RemoveBlanksTabs(sLine)
                sLine = LCase(sLine)
                find = Left(sLine, Len(sKey))
                If ts.AtEndOfStream = True Then Exit Do
            Loop
        Next i

        'read cost objective function
        For i = 1 To ITERAM
            find = ""
            Do Until find = sKey Or ts.AtEndOfStream = True
                sLine = ReadLn(ts)
                sLine = RemoveBlanksTabs(sLine)
                sLine = LCase(sLine)
                find = Left(sLine, Len(sKey))
                If ts.AtEndOfStream = True Then Exit Do
            Loop
            FirstOcc = InStr(1, sLine, "=")
            sLine = Mid(sLine, FirstOcc + 1)
            GetValues 1, sLine, v
            aObjFct(i, 1) = Round((Val_(v(1))), 3) / 10000
        Next i
    End If
    
    'READ INERTIA
    If w(2) > 0 Then
        ts.Close
        Set ts = fil.OpenAsTextStream(ForReading)
        'initial values
        sKey = "INERTIE-INERTIAIYY"
        Do Until find = sKey Or ts.AtEndOfStream = True
            sLine = ReadLn(ts)
            sLine = RemoveBlanksTabs(sLine)
            find = Left(sLine, Len(sKey))
        Loop
        FirstOcc = InStr(1, sLine, "=")
        sLine = Mid(sLine, FirstOcc + 1)
        GetValues 1, sLine, v
        aObjFct(0, 2) = Round((Val_(v(1))), 3)
        
        'skip ITERAM times
        sKey = "fctobjectifinertie(recalculée)"
        For i = 1 To ITERAM
            find = ""
            Do Until find = sKey Or ts.AtEndOfStream = True
                sLine = ReadLn(ts)
                sLine = RemoveBlanksTabs(sLine)
                sLine = LCase(sLine)
                find = Left(sLine, Len(sKey))
                If ts.AtEndOfStream = True Then Exit Do
            Loop
        Next i

        'read cost objective function
        For i = 1 To ITERAM
            find = ""
            Do Until find = sKey Or ts.AtEndOfStream = True
                sLine = ReadLn(ts)
                sLine = RemoveBlanksTabs(sLine)
                sLine = LCase(sLine)
                find = Left(sLine, Len(sKey))
                If ts.AtEndOfStream = True Then Exit Do
            Loop
            FirstOcc = InStr(1, sLine, "=")
            sLine = Mid(sLine, FirstOcc + 1)
            GetValues 1, sLine, v
            aObjFct(i, 2) = Round((Val_(v(1))), 3)
        Next i
    End If
    
    ReadMultiObj = True
    Exit Function
ReadUniObjErr:
    ReadMultiObj = False
End Function


'Private Function ReadMultiObj(ts As TextStream) As Boolean
'    On Error GoTo ReadUniObjErr
'    Dim find As String, FirstOcc As String, sLine As String
'    Dim v() As Variant
'
'    'Initial Values
'    Dim sKey As String, iUnit As Integer
'
'    'cost
'    sKey = "COUT-COST"
'    iUnit = 1
'    Do Until find = sKey Or ts.AtEndOfStream = True
'        sLine = ReadLn(ts)
'        sLine = RemoveBlanksTabs(sLine)
'        find = Left(sLine, Len(sKey))
'    Loop
'    FirstOcc = InStr(1, sLine, "=")
'    sLine = Mid(sLine, FirstOcc + 1)
'    GetValues 1, sLine, v
'    aObjFct(0, 0) = Round((Val_(v(1))), 3) / iUnit
'
'    'weight
'    sKey = "POIDS-WEIGHT"
'    iUnit = 10000
'    Do Until find = sKey Or ts.AtEndOfStream = True
'        sLine = ReadLn(ts)
'        sLine = RemoveBlanksTabs(sLine)
'        find = Left(sLine, Len(sKey))
'    Loop
'    FirstOcc = InStr(1, sLine, "=")
'    sLine = Mid(sLine, FirstOcc + 1)
'    GetValues 1, sLine, v
'    aObjFct(0, 1) = Round((Val_(v(1))), 3) / iUnit
'
'    'inertia
'    sKey = "INERTIE-INERTIAIYY"
'    iUnit = 1
'    Do Until find = sKey Or ts.AtEndOfStream = True
'        sLine = ReadLn(ts)
'        sLine = RemoveBlanksTabs(sLine)
'        find = Left(sLine, Len(sKey))
'    Loop
'    FirstOcc = InStr(1, sLine, "=")
'    sLine = Mid(sLine, FirstOcc + 1)
'    GetValues 1, sLine, v
'    aObjFct(0, 2) = Round((Val_(v(1))), 3) / iUnit
'
'    'jump over first inertia optimization cycle
'    sKey = "fctobjectifinertie(recalculée)"
'    Dim i As Integer
'    For i = 1 To ITERAM
'        find = ""
'        Do Until find = sKey Or ts.AtEndOfStream = True
'            sLine = ReadLn(ts)
'            sLine = RemoveBlanksTabs(sLine)
'            sLine = LCase(sLine)
'            find = Left(sLine, Len(sKey))
'            If ts.AtEndOfStream = True Then Exit Do
'        Loop
'    Next i
'
'    'read the three objective functions
'    For i = 1 To ITERAM
'        'inertia
'        sKey = "fctobjectifinertie(recalculée)"
'        iUnit = 1
'        find = ""
'        Do Until find = sKey Or ts.AtEndOfStream = True
'            sLine = ReadLn(ts)
'            sLine = RemoveBlanksTabs(sLine)
'            sLine = LCase(sLine)
'            find = Left(sLine, Len(sKey))
'            If ts.AtEndOfStream = True Then Exit Do
'        Loop
'        FirstOcc = InStr(1, sLine, "=")
'        sLine = Mid(sLine, FirstOcc + 1)
'        GetValues 1, sLine, v
'        aObjFct(i, 2) = Round((Val_(v(1))), 3) / iUnit
'
'        'weight
'        sKey = "fctobjectifpoids(recalculée)"
'        iUnit = 10000
'        find = ""
'        Do Until find = sKey Or ts.AtEndOfStream = True
'            sLine = ReadLn(ts)
'            sLine = RemoveBlanksTabs(sLine)
'            sLine = LCase(sLine)
'            find = Left(sLine, Len(sKey))
'            If ts.AtEndOfStream = True Then Exit Do
'        Loop
'        FirstOcc = InStr(1, sLine, "=")
'        sLine = Mid(sLine, FirstOcc + 1)
'        GetValues 1, sLine, v
'        aObjFct(i, 1) = Round((Val_(v(1))), 3) / iUnit
'
'        'cost
'        sKey = "fctobjectifcout(recalculée)"
'        iUnit = 1
'        find = ""
'        Do Until find = sKey Or ts.AtEndOfStream = True
'            sLine = ReadLn(ts)
'            sLine = RemoveBlanksTabs(sLine)
'            sLine = LCase(sLine)
'            find = Left(sLine, Len(sKey))
'            If ts.AtEndOfStream = True Then Exit Do
'        Loop
'        FirstOcc = InStr(1, sLine, "=")
'        sLine = Mid(sLine, FirstOcc + 1)
'        GetValues 1, sLine, v
'        aObjFct(i, 0) = Round((Val_(v(1))), 3) / iUnit
'    Next i
'
'
'    ReadMultiObj = True
'    Exit Function
'ReadUniObjErr:
'    ReadMultiObj = False
'End Function

Private Function ReadUniObj(ts As TextStream) As Boolean
    On Error GoTo ReadUniObjErr
    Dim find As String, FirstOcc As String, sLine As String
    Dim v() As Variant
    
    'Initial Values
    Dim sKey As String, iUnit As Integer
    Select Case ICOUT
        Case -1
            sKey = "INERTIE-INERTIAIYY"
            iUnit = 1
        Case 0
            sKey = "POIDS-WEIGHT"
            iUnit = 10000
        Case 1
            sKey = "COUT-COST"
            iUnit = 1
    End Select
    
    find = ""
    Do Until find = sKey Or ts.AtEndOfStream = True
        sLine = ReadLn(ts)
        sLine = RemoveBlanksTabs(sLine)
        find = Left(sLine, Len(sKey))
    Loop
    FirstOcc = InStr(1, sLine, "=")
    sLine = Mid(sLine, FirstOcc + 1)
    GetValues 1, sLine, v
    aObjFct(0, 0) = Round((Val_(v(1))), 3) / iUnit
    
    'iterations
    Dim i As Integer
    
    Select Case ICOUT
        Case -1
            sKey = "fctobjectifinertie(recalculée)"
            iUnit = 1
        Case 0
            sKey = "fctobjectifpoids(recalculée)"
            iUnit = 10000
        Case 1
            sKey = "fctobjectifcout(recalculée)"
            iUnit = 1
    End Select

    For i = 1 To ITERAM
        find = ""
        Do Until find = sKey Or ts.AtEndOfStream = True
            sLine = ReadLn(ts)
            sLine = RemoveBlanksTabs(sLine)
            sLine = LCase(sLine)
            find = Left(sLine, Len(sKey))
            If ts.AtEndOfStream = True Then Exit Do
        Loop
        FirstOcc = InStr(1, sLine, "=")
        sLine = Mid(sLine, FirstOcc + 1)
        GetValues 1, sLine, v
        aObjFct(i, 0) = Round((Val_(v(1))), 3) / iUnit
    Next i
    
    ReadUniObj = True
    Exit Function
ReadUniObjErr:
    ReadUniObj = False
End Function

Private Sub mnuPrint_Click()
    On Error GoTo PrintErr
    MSChart1.EditCopy
    Picture1.Picture = Clipboard.GetData(vbCFBitmap)
    
    Printer.Print
    Printer.PaintPicture Picture1.Image, 0, 0
    Printer.EndDoc
    Exit Sub
PrintErr:
    Call RaiseError(MyUnhandledError, Err.Description)
End Sub

Private Sub mnuSnapshop_Click()
    On Error Resume Next
    MSChart1.EditCopy
    Picture1.Picture = Clipboard.GetData(vbCFBitmap)
    With CommonDialog1
        .DialogTitle = "Snapshot"
        .CancelError = True
        .Filter = "Picture Files (*.bmp)|*.bmp"
        .FileName = ""
        .ShowSave
        If Len(.FileName) = 0 Then
            Exit Sub
        End If
        SavePicture Picture1.Image, .FileName
    End With

End Sub
