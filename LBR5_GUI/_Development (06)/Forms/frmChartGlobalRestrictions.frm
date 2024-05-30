VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "comdlg32.ocx"
Object = "{65E121D4-0C60-11D2-A9FC-0000F8754DA1}#2.0#0"; "mschrt20.ocx"
Begin VB.Form frmChartGlobalRestrictions 
   BorderStyle     =   5  'Sizable ToolWindow
   Caption         =   "Global Restrictions"
   ClientHeight    =   6930
   ClientLeft      =   885
   ClientTop       =   2640
   ClientWidth     =   9360
   Icon            =   "frmChartGlobalRestrictions.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   6930
   ScaleWidth      =   9360
   ShowInTaskbar   =   0   'False
   Begin VB.PictureBox Picture1 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      ForeColor       =   &H80000008&
      Height          =   1695
      Left            =   1440
      ScaleHeight     =   1665
      ScaleWidth      =   1785
      TabIndex        =   1
      Top             =   2040
      Visible         =   0   'False
      Width           =   1815
   End
   Begin MSChart20Lib.MSChart MSChart1 
      Height          =   1575
      Left            =   0
      OleObjectBlob   =   "frmChartGlobalRestrictions.frx":000C
      TabIndex        =   0
      Top             =   0
      Width           =   2295
   End
   Begin MSComDlg.CommonDialog CommonDialog1 
      Left            =   120
      Top             =   2040
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
   End
   Begin VB.Menu mnuSnapshot 
      Caption         =   "&Snapshot"
   End
   Begin VB.Menu mnuPrint 
      Caption         =   "&Print"
   End
End
Attribute VB_Name = "frmChartGlobalRestrictions"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private ProjectIndex As Integer
Dim ITERAM As Integer, IGRAV As Integer, KGmin As Double, KGmax As Double
Dim fso As New FileSystemObject, fil As file, ts As TextStream, sFile As String
Dim aObjFct() As Variant

Private Sub Form_Load()
    ProjectIndex = ActiveProject
    Me.Move (Screen.Width - Me.Width) / 2, (Screen.Height - Me.Height) / 2
    Me.Caption = "Gravity Center Variation - [" & GetFileName(Project.Item(ProjectIndex).sFileName) & "]"
    If ReadFile = False Then
       Unload Me
       Exit Sub
    End If
    GraphSetup
    GraphDraw
End Sub

Private Function GraphDraw() As Boolean
    On Error GoTo GraphDrawErr
    Dim min As Double, max As Double
    Dim i As Integer
        
    'draw limits
    'min = IGRAV =
     Select Case IGRAV
        Case 0 '"NoLimit"
            MSChart1.ColumnCount = 1
            min = 10000000
            For i = 0 To ITERAM
                If min > aObjFct(i, 0) Then
                    min = aObjFct(i, 0)
                End If
            Next i
            max = -10000000
            For i = 0 To ITERAM
                If max < aObjFct(i, 0) Then
                    max = aObjFct(i, 0)
                End If
            Next i
        Case 1 '"LowerLimit"
            MSChart1.ColumnCount = 2
             With MSChart1.Plot
                .SeriesCollection(2).LegendText = "Lower Limit [m]"
            End With
            MSChart1.Plot.SeriesCollection(2).Pen.Width = 50
            min = KGmin
            max = -10000000
            For i = 0 To ITERAM
                If max < aObjFct(i, 0) Then
                    max = aObjFct(i, 0)
                End If
            Next i
        Case 2 '"UpperLimit"
             With MSChart1.Plot
                .SeriesCollection(2).LegendText = "Upper Limit [m]"
            End With
            MSChart1.ColumnCount = 2
            MSChart1.Plot.SeriesCollection(2).Pen.Width = 50
            max = KGmax
            min = 10000000
            For i = 0 To ITERAM
                If min > aObjFct(i, 0) Then
                    min = aObjFct(i, 0)
                End If
            Next i
        Case 3 '"BothLimits"
            MSChart1.ColumnCount = 3
             With MSChart1.Plot
                .SeriesCollection(2).LegendText = "Lower Limit [m]"
            End With
             With MSChart1.Plot
                .SeriesCollection(3).LegendText = "Upper Limit [m]"
            End With
            MSChart1.Plot.SeriesCollection(2).Pen.Width = 50
            MSChart1.Plot.SeriesCollection(3).Pen.Width = 50
            min = KGmin
            max = KGmax
    End Select
    
    With MSChart1
        .Plot.Axis(VtChAxisIdY).ValueScale.Auto = False
        .Plot.Axis(VtChAxisIdY).ValueScale.Minimum = min '- 1 / 100 * min
        .Plot.Axis(VtChAxisIdY).ValueScale.Maximum = max '+ 1 / 100 * max
        .Plot.Axis(VtChAxisIdY).ValueScale.MajorDivision = 0 ' (max - min) / 10
    End With
    
    

    Select Case IGRAV
        Case 0  '"NoLimit"
            With MSChart1
               For i = 0 To ITERAM
                   .Row = i + 1
                   .RowLabel = i
                   .Column = 1
                   .Data = aObjFct(i, 0)
               Next i
            End With
        Case 1 '"LowerLimit"
            With MSChart1
               For i = 0 To ITERAM
                   .Row = i + 1
                   .RowLabel = i
                   .Column = 1
                   .Data = aObjFct(i, 0)
               Next i
               For i = 0 To ITERAM
                   .Row = i + 1
                   .RowLabel = i
                   .Column = 2
                   .Data = min
               Next i
            End With
        Case 2 '"UpperLimit"
            With MSChart1
               For i = 0 To ITERAM
                   .Row = i + 1
                   .RowLabel = i
                   .Column = 1
                   .Data = aObjFct(i, 0)
               Next i
               For i = 0 To ITERAM
                   .Row = i + 1
                   .RowLabel = i
                   .Column = 2
                   .Data = max
               Next i
            End With
        Case 3 '"BothLimits"
            With MSChart1
                For i = 0 To ITERAM
                   .Row = i + 1
                   .RowLabel = i
                   .Column = 1
                   .Data = aObjFct(i, 0)
                Next i
                For i = 0 To ITERAM
                   .Row = i + 1
                   .RowLabel = i
                   .Column = 2
                   .Data = min
                Next i
                For i = 0 To ITERAM
                   .Row = i + 1
                   .RowLabel = i
                   .Column = 3
                   .Data = max
                Next i
            End With
    End Select
    
    
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
    
   
    GraphDraw = True
    Exit Function
GraphDrawErr:
    GraphDraw = False
End Function

Private Function ReadFile() As Boolean
    On Error GoTo ReadFileErr
    Dim sPrefix As String
    Dim sLine As String, find As String, sKey As String, FirstOcc As Integer
    Dim v() As Variant
    Dim i As Integer
    
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
    ReDim aObjFct(ITERAM, 0)
    
    'IGRAV
    find = ""
    Do Until find = "igrav" Or ts.AtEndOfStream = True
        sLine = ReadLn(ts)
        If ts.AtEndOfStream = True Then
            MsgBox "File not read succesfully (ITERAM)!", vbCritical + vbOKOnly
        End If
        find = LCase(Left(sLine, 5))
    Loop
    GetValues 2, sLine, v
    IGRAV = Val_(v(2))
    
    sKey = "kg actuel"
    For i = 0 To ITERAM
        find = ""
        Do Until find = sKey Or ts.AtEndOfStream = True
            sLine = ReadLn(ts)
            If ts.AtEndOfStream = True Then
                MsgBox "File not read succesfully (" & sKey & ")!", vbCritical + vbOKOnly
                Exit Function
            End If
            find = LCase(Left(sLine, Len(sKey)))
        Loop
        FirstOcc = InStr(1, sLine, "=")
        sLine = Mid(sLine, FirstOcc + 1)
        sLine = CleanLines(sLine)
        
        aObjFct(i, 0) = Round(Val_(Left(sLine, 13)), 3)
    Next i
    
    'limits
    Select Case IGRAV
        Case 0
        Case 1
            find = ""
            sKey = "kg minimum"
            Do Until find = sKey Or ts.AtEndOfStream = True
                sLine = ReadLn(ts)
                If ts.AtEndOfStream = True Then
                    MsgBox "File not read succesfully (" & sKey & ")!", vbCritical + vbOKOnly
                    Exit Function
                End If
                find = LCase(Left(sLine, Len(sKey)))
            Loop
            FirstOcc = InStr(1, sLine, "=")
            sLine = Mid(sLine, FirstOcc + 1)
            sLine = CleanLines(sLine)
            KGmin = Round(Val_(Left(sLine, 13)), 3)
        Case 2
            find = ""
            sKey = "kg maximum"
            Do Until find = sKey Or ts.AtEndOfStream = True
                sLine = ReadLn(ts)
                If ts.AtEndOfStream = True Then
                    MsgBox "File not read succesfully (" & sKey & ")!", vbCritical + vbOKOnly
                    Exit Function
                End If
                find = LCase(Left(sLine, Len(sKey)))
            Loop
            FirstOcc = InStr(1, sLine, "=")
            sLine = Mid(sLine, FirstOcc + 1)
            sLine = CleanLines(sLine)
            KGmax = Round(Val_(Left(sLine, 13)), 3)
        Case 3
            find = ""
            sKey = "kg maximum"
            Do Until find = sKey Or ts.AtEndOfStream = True
                sLine = ReadLn(ts)
                If ts.AtEndOfStream = True Then
                    MsgBox "File not read succesfully (" & sKey & ")!", vbCritical + vbOKOnly
                    Exit Function
                End If
                find = LCase(Left(sLine, Len(sKey)))
            Loop
            FirstOcc = InStr(1, sLine, "=")
            sLine = Mid(sLine, FirstOcc + 1)
            sLine = CleanLines(sLine)
            KGmax = Round(Val_(Left(sLine, 13)), 3)
            find = ""
            sKey = "kg minimum"
            Do Until find = sKey Or ts.AtEndOfStream = True
                sLine = ReadLn(ts)
                If ts.AtEndOfStream = True Then
                    MsgBox "File not read succesfully (" & sKey & ")!", vbCritical + vbOKOnly
                    Exit Function
                End If
                find = LCase(Left(sLine, Len(sKey)))
            Loop
            FirstOcc = InStr(1, sLine, "=")
            sLine = Mid(sLine, FirstOcc + 1)
            sLine = CleanLines(sLine)
            KGmin = Round(Val_(Left(sLine, 13)), 3)
    End Select
    ts.Close
    Set ts = Nothing
    ReadFile = True
    Exit Function
ReadFileErr:
    ReadFile = False
End Function

Private Function GraphSetup() As Boolean
    MSChart1.ShowLegend = True
    MSChart1.AllowSelections = False
    
    
    
    MSChart1.RowCount = ITERAM + 1
    
    MSChart1.chartType = VtChChartType2dLine
    
   
    MSChart1.TitleText = "Gravity Center Variation"
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
        .SeriesCollection(1).LegendText = "Gravity center [m]"
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
End Sub

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

Private Sub mnuSnapshot_Click()
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
