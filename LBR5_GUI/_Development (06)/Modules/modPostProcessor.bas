Attribute VB_Name = "modPostProcessor"
Option Explicit
Option Base 1
Dim POP() As Double
Dim POX() As Variant
Dim POG() As Variant
Dim pos() As Variant
Public POL() As Variant
Dim ValMin As Double
Dim ValMax As Double
Dim ValAbs As Double
Dim ValScale As Double
Dim VECTORdes() As Double
Dim Vector() As Double
Dim angle() As Double
Dim Length() As Double
Dim y1() As Double
Dim y2() As Double
Dim z1() As Double
Dim z2() As Double
Dim NeutralAxis As Double
' **************************
Dim col As colPanel
Dim colNodes As colNodes
Dim Panel As cPanel
Dim LoadCase As cLoadCase
Dim KeyDiagPLATE() As String, KeyDiagBEAM() As String
Dim NLOADCASES As Integer
Dim sFile As String, sPath As String, sName As String, sDocName As String, _
    sLine As String, find As String, strInStr As String
Dim fso As New FileSystemObject, fil As file, ts As TextStream
Dim ProjectIndex As Integer
Dim IANA As Integer

Public Sub Solution(ByVal index As Integer)
    ProjectIndex = index
    IANA = Project.Item(ProjectIndex).cHeader.IANA
    sFile = Project.Item(ProjectIndex).sFileName
    sPath = GetFilePath(sFile)
    sName = GetFileName(sFile)
    ChDrive Left(sPath, 3)
    ChDir sPath
    If IANA = 1 Then
        sDocName = GetFileRoot(sPath & "sol-" & sName) & ".txt"
    ElseIf IANA = 2 Then
        sDocName = GetFileRoot(sPath & "sol3-" & sName) & ".txt"
    End If
    If fso.FileExists(sDocName) = False Then
        MsgBox "Solution File not found.", vbCritical + vbOKOnly
        Exit Sub
    End If
    Project.Item(ProjectIndex).frmProject.MouseIcon = LoadResPicture("ID_CURSOR_WAIT", 2)
    Set fil = fso.GetFile(sDocName)
    Set ts = fil.OpenAsTextStream(ForReading)
    Set colNodes = Project.Item(ProjectIndex).colNodes
    Set col = Project.Item(ProjectIndex).colPanel
    ReDim angle(col.Count)
    ReDim Length(col.Count)
    ReDim y1(col.Count)
    ReDim y2(col.Count)
    ReDim z1(col.Count)
    ReDim z2(col.Count)
    ValScale = 0
    
    For Each Panel In col
        angle(Panel.pNumber) = Panel.cGeometry.PanelAngle
        Length(Panel.pNumber) = Panel.cGeometry.PanelWidth
        y1(Panel.pNumber) = colNodes.Item(Panel.cGeometry.InNode).y 'Panel.X_IN
        z1(Panel.pNumber) = colNodes.Item(Panel.cGeometry.InNode).z 'Panel.Y_IN
        y2(Panel.pNumber) = colNodes.Item(Panel.cGeometry.OutNode).y 'Panel.X_OUT
        z2(Panel.pNumber) = colNodes.Item(Panel.cGeometry.OutNode).z 'Panel.Y_OUT
        ValScale = ValScale + Length(Panel.pNumber)
    Next Panel
    ValScale = ValScale / col.Count / 2.5
    SolMatrix ProjectIndex
    If IANA = 1 Then
        If ReadSolutionLBR4 = False Then
            Project.Item(ProjectIndex).frmProject.MouseIcon = LoadResPicture("ID_CURSOR_NORMAL", 2)
            Exit Sub
        End If
    ElseIf IANA = 2 Then
        If ReadSolutionBeamTheory = False Then
            Project.Item(ProjectIndex).frmProject.MouseIcon = LoadResPicture("ID_CURSOR_NORMAL", 2)
            Exit Sub
        End If
    End If
    FindMinMaxAbs
    FillMatrix
    ReadUpdatedScantlings ProjectIndex
    Project.Item(ProjectIndex).frmProject.MouseIcon = LoadResPicture("ID_CURSOR_NORMAL", 2)
    If IANA = 1 Then
        Project.Item(ProjectIndex).cSolution.CurrentDiagram = 19 'none
    ElseIf IANA = 2 Then
        Project.Item(ProjectIndex).cSolution.CurrentDiagram = 4 'none
    End If
    Project.Item(ProjectIndex).DataChanged = True
End Sub

Public Sub ReadUpdatedScantlings(index As Integer)
    'On Error GoTo  ReadUpdatedScantlingsErr
    On Error GoTo 1
    Dim cPanel As cPanel, cUpdatedPanel As cPanel
    Dim cScantlings As cScantlings
    Dim colPanel As New colPanel
    Dim fso As New FileSystemObject, fil As file, ts As TextStream
    sFile = GetFilePath(Project.Item(index).sFileName) & "up-" & GetLetterCorrespondingToNumber(Project.Item(index).cHeader.ITERAM) & "-" & GetFileRoot(Project.Item(index).sFileName) & ".txt"
    Set fil = fso.GetFile(sFile)
    Set ts = fil.OpenAsTextStream(ForReading)
    Dim sLine As String ', sFile As String
    Dim v() As Variant
    ts.SkipLine
    ts.SkipLine
    For Each cPanel In Project.Item(index).colPanelUpdate
        Project.Item(index).colPanelUpdate.Remove cPanel.index
    Next cPanel
    'Project.Item(index).colPanelUpdate = Nothing
    For Each cPanel In Project.Item(index).colPanel
        Set cUpdatedPanel = New cPanel
        cUpdatedPanel.index = cPanel.index
        cUpdatedPanel.pNumber = cPanel.pNumber
        cUpdatedPanel.Region = cPanel.Region
        Set cUpdatedPanel.cScantlings = cPanel.cScantlings.Clone
        Set cUpdatedPanel.cGeometry = cPanel.cGeometry.Clone
        Set cUpdatedPanel.colBoundaryConditions = cPanel.colBoundaryConditions.Clone
NextLine:
        sLine = ReadLn(ts)
        sLine = LTrim(sLine)
        Select Case Left(UCase(sLine), 5)
            Case "PLAQU"
                cUpdatedPanel.pType = Plate
            Case "EPONT"
                cUpdatedPanel.pType = Beam
            Case "COQUE"
                Err.Description = "SHELL element not implemented."
                Err.Raise 9999, , Err.Description
            Case "DCOQU"
                cUpdatedPanel.pType = DoubleHull
            Case Else
            GoTo NextLine
        End Select
        sLine = ReadLn(ts)
        GetValues 2, sLine, v
        'cUpdatedPanel.cGeometry.PanelWidth = Abs(Val_(v(1)))
        cUpdatedPanel.cScantlings.NetThickness = Val_(v(2))
        cUpdatedPanel.cScantlings.GrossThickness = cUpdatedPanel.cScantlings.NetThickness
        'cUpdatedPanel.cScantlings.ThicknessVariation = (cPanel.cScantlings.NetThickness - cUpdatedPanel.cScantlings.NetThickness) / cPanel.cScantlings.NetThickness * 100
        cUpdatedPanel.cScantlings.ThicknessVariation = Percent(cPanel.cScantlings.NetThickness, cUpdatedPanel.cScantlings.NetThickness, 2)
        Select Case cUpdatedPanel.pType
            Case Plate, DoubleHull
                sLine = ReadLn(ts)
                GetValues 4, sLine, v
                cUpdatedPanel.cScantlings.cPrimaryFrames.Spacing = Val_(v(1))
                cUpdatedPanel.cScantlings.cPrimaryFrames.SpacingVariation = -Percent(cPanel.cScantlings.cPrimaryFrames.Spacing, cUpdatedPanel.cScantlings.cPrimaryFrames.Spacing, 2)
                cUpdatedPanel.cScantlings.cPrimaryStiffeners.Spacing = Val_(v(2))
                cUpdatedPanel.cScantlings.cPrimaryStiffeners.SpacingVariation = -Percent(cPanel.cScantlings.cPrimaryStiffeners.Spacing, cUpdatedPanel.cScantlings.cPrimaryStiffeners.Spacing, 2)
                cUpdatedPanel.cScantlings.cSecondaryFrames.Spacing = Val_(v(3))
                cUpdatedPanel.cScantlings.cSecondaryStiffeners.Spacing = Val_(v(4))
                sLine = ReadLn(ts)
                GetValues 4, sLine, v
                cUpdatedPanel.cScantlings.cPrimaryFrames.WebHeight = Val_(v(1))
                cUpdatedPanel.cScantlings.cPrimaryFrames.WebHeightVariation = Percent(cPanel.cScantlings.cPrimaryFrames.WebHeight, cUpdatedPanel.cScantlings.cPrimaryFrames.WebHeight, 2)
                cUpdatedPanel.cScantlings.cPrimaryFrames.WebThickness = Val_(v(2))
                cUpdatedPanel.cScantlings.cPrimaryFrames.WebThicknessVariation = Percent(cPanel.cScantlings.cPrimaryFrames.WebThickness, cUpdatedPanel.cScantlings.cPrimaryFrames.WebThickness, 2)
                cUpdatedPanel.cScantlings.cPrimaryFrames.FlangeWidth = Val_(v(3))
                cUpdatedPanel.cScantlings.cPrimaryFrames.FlangeWidthVariation = Percent(cPanel.cScantlings.cPrimaryFrames.FlangeWidth, cUpdatedPanel.cScantlings.cPrimaryFrames.FlangeWidth, 2)
                cUpdatedPanel.cScantlings.cPrimaryFrames.FlangeThickness = Val_(v(4))
                cUpdatedPanel.cScantlings.cPrimaryFrames.FlangeThicknessVariation = Percent(cPanel.cScantlings.cPrimaryFrames.FlangeThickness, cUpdatedPanel.cScantlings.cPrimaryFrames.FlangeThickness, 2)
                sLine = ReadLn(ts)
                GetValues 4, sLine, v
                cUpdatedPanel.cScantlings.cPrimaryStiffeners.WebHeight = Val_(v(1))
                cUpdatedPanel.cScantlings.cPrimaryStiffeners.WebHeightVariation = Percent(cPanel.cScantlings.cPrimaryStiffeners.WebHeight, cUpdatedPanel.cScantlings.cPrimaryStiffeners.WebHeight, 2)
                cUpdatedPanel.cScantlings.cPrimaryStiffeners.WebThickness = Val_(v(2))
                cUpdatedPanel.cScantlings.cPrimaryStiffeners.WebThicknessVariation = Percent(cPanel.cScantlings.cPrimaryStiffeners.WebThickness, cUpdatedPanel.cScantlings.cPrimaryStiffeners.WebThickness, 2)
                cUpdatedPanel.cScantlings.cPrimaryStiffeners.FlangeWidth = Val_(v(3))
                cUpdatedPanel.cScantlings.cPrimaryStiffeners.FlangeWidthVariation = Percent(cPanel.cScantlings.cPrimaryStiffeners.FlangeWidth, cUpdatedPanel.cScantlings.cPrimaryStiffeners.FlangeWidth, 2)
                cUpdatedPanel.cScantlings.cPrimaryStiffeners.FlangeThickness = Val_(v(4))
                cUpdatedPanel.cScantlings.cPrimaryStiffeners.FlangeThicknessVariation = Percent(cPanel.cScantlings.cPrimaryStiffeners.FlangeThickness, cUpdatedPanel.cScantlings.cPrimaryStiffeners.FlangeThickness, 2)
                cUpdatedPanel.cScantlings.cSecondaryFrames.Side = SideLeft
                cUpdatedPanel.cScantlings.cSecondaryStiffeners.Side = SideLeft
                If cUpdatedPanel.cScantlings.cSecondaryFrames.Spacing > 0 Or cUpdatedPanel.cScantlings.cSecondaryStiffeners.Spacing > 0 Then
                    sLine = ReadLn(ts)
                    GetValues 5, sLine, v
                    cUpdatedPanel.cScantlings.cSecondaryFrames.WebHeight = Val_(v(1))
                    cUpdatedPanel.cScantlings.cSecondaryFrames.WebThickness = Val_(v(2))
                    cUpdatedPanel.cScantlings.cSecondaryFrames.FlangeWidth = Val_(v(3))
                    cUpdatedPanel.cScantlings.cSecondaryFrames.FlangeThickness = Val_(v(4))
                    cUpdatedPanel.cScantlings.cSecondaryFrames.Side = Val_(v(5))
                    sLine = ReadLn(ts)
                    GetValues 5, sLine, v
                    cUpdatedPanel.cScantlings.cSecondaryStiffeners.WebHeight = Val_(v(1))
                    cUpdatedPanel.cScantlings.cSecondaryStiffeners.WebThickness = Val_(v(2))
                    cUpdatedPanel.cScantlings.cSecondaryStiffeners.FlangeWidth = Val_(v(3))
                    cUpdatedPanel.cScantlings.cSecondaryStiffeners.FlangeThickness = Val_(v(4))
                    cUpdatedPanel.cScantlings.cSecondaryStiffeners.Side = Val_(v(5))
                End If
                
                sLine = ReadLn(ts)
                GetValues 1, sLine, v
                cUpdatedPanel.cScantlings.cPrimaryStiffeners.DistributionMode = v(1)
                cUpdatedPanel.cScantlings.cPrimaryStiffeners.GrossSectionModulus = GetSectionModulus(cUpdatedPanel.cScantlings.cPrimaryStiffeners.WebHeight, _
                    cUpdatedPanel.cScantlings.cPrimaryStiffeners.WebThickness + cUpdatedPanel.cScantlings.cPrimaryStiffeners.CorrosionThickness, _
                    cUpdatedPanel.cScantlings.cPrimaryStiffeners.FlangeWidth, _
                    cUpdatedPanel.cScantlings.cPrimaryStiffeners.FlangeThickness + cUpdatedPanel.cScantlings.cPrimaryStiffeners.CorrosionThickness, _
                    cUpdatedPanel.cScantlings.cPrimaryStiffeners.Spacing, _
                    cUpdatedPanel.cScantlings.cPrimaryFrames.Spacing, _
                    cUpdatedPanel.cScantlings.GrossThickness)
                cUpdatedPanel.cScantlings.cPrimaryStiffeners.GrossSectionModulusVariation = Percent(cPanel.cScantlings.cPrimaryStiffeners.GrossSectionModulus, cUpdatedPanel.cScantlings.cPrimaryStiffeners.GrossSectionModulus, 2)
                If cUpdatedPanel.pType = DoubleHull Then
                    sLine = ReadLn(ts)
                    GetValues 1, sLine, v
                    cUpdatedPanel.RelatedDoubleHullPanel = Val_(v(1))
                End If
                sLine = ReadLn(ts)
                GetValues 1, sLine, v
                cUpdatedPanel.cGeometry.Participation = Val_(v(1))
            Case Beam
                sLine = ReadLn(ts)
                GetValues 1, sLine, v
                Select Case UCase(v(1))
                    Case "CERCLE"
                        cUpdatedPanel.cScantlings.BeamSection = bsCircle
                    Case "CARRE"
                        cUpdatedPanel.cScantlings.BeamSection = bsSquare
                    Case "DOUBLET"
                        cUpdatedPanel.cScantlings.BeamSection = bsDoubleT
                    Case Else
                    Err.Description = "BEAM Section " & "'" & v(1) & "'" & " not recognized."
                    Err.Raise 9999, , Err.Description
                End Select
                sLine = ReadLn(ts)
                GetValues 2, sLine, v
                cUpdatedPanel.cScantlings.cPrimaryFrames.Spacing = Val_(v(1))
                cUpdatedPanel.cGeometry.BucklingLength = Val_(v(2))
                Select Case cUpdatedPanel.cScantlings.BeamSection
                    Case bsCircle
                        sLine = ReadLn(ts)
                        GetValues 1, sLine, v
                        cUpdatedPanel.cScantlings.cPrimaryFrames.WebHeight = Val_(v(1))
                    Case bsSquare
                        sLine = ReadLn(ts)
                        GetValues 1, sLine, v
                        cUpdatedPanel.cScantlings.cPrimaryFrames.WebHeight = Val_(v(1))
                    Case bsDoubleT
                        sLine = ReadLn(ts)
                        GetValues 4, sLine, v
                        cUpdatedPanel.cScantlings.cPrimaryFrames.WebHeight = Val_(v(1)) * 2 ' to modify when Judith modifies in LBR5
                        cUpdatedPanel.cScantlings.cPrimaryFrames.WebThickness = Val_(v(2))
                        cUpdatedPanel.cScantlings.cPrimaryFrames.FlangeWidth = Val_(v(3))
                        cUpdatedPanel.cScantlings.cPrimaryFrames.FlangeThickness = Val_(v(4))
                        cUpdatedPanel.cScantlings.NetThickness = Empty
                End Select
        End Select



        Project.Item(index).colPanelUpdate.Add cUpdatedPanel, cPanel.index
        Set cUpdatedPanel = Nothing
        
    Next cPanel
    Exit Sub
ReadUpdatedScantlingsErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modPostProcessor: Sub ReadUpdatedScantlings")
1:
End Sub

Public Sub SolMatrix(ByVal index As Integer)
    Dim i As Integer
    IANA = Project.Item(index).cHeader.IANA
    NLOADCASES = Project.Item(index).cHeader.colLoadCase.GetActiveLoadCasesNo
    If IANA = 1 Then
        ReDim POP(31, 6) As Double ' -> 1..4 - coordinates; 5 - real value of vector; 6 - scaled value of vector
        ReDim POX(5) ' number of transversal sections
        ReDim POG(18) ' number of diagrams
        ReDim pos(Project.Item(index).colPanel.Count) ' number of panels
        ReDim POL(NLOADCASES) ' number of active load cases
    ElseIf IANA = 2 Then
        ReDim POP(3, 6) As Double ' -> 1..4 - coordinates; 5 - real value of vector; 6 - scaled value of vector
        ReDim POX(2) ' number of transversal sections
        ReDim POG(3) ' number of diagrams
        ReDim pos(Project.Item(index).colPanel.Count) ' number of panels
        ReDim POL(NLOADCASES) ' number of active load cases
    End If
    
    For i = 1 To UBound(POX)
        POX(i) = POP
    Next i
    For i = 1 To UBound(POG)
        POG(i) = POX
    Next i
    For i = 1 To Project.Item(index).colPanel.Count
        pos(i) = POG
    Next i
    For i = 1 To NLOADCASES
        POL(i) = pos
    Next i
End Sub

Private Function ReadSolutionBeamTheory() As Boolean
    On Error GoTo ReadSolutionBeamTheoryErr
    Dim i As Integer, j As Integer, k As Integer, m As Integer, o As Integer
    Dim v As Variant
    ReDim Vector(31, 5) As Double
    Project.Item(ProjectIndex).frmProject.ProgressBar.Visible = _
    Project.Item(ProjectIndex).frmProject.StatusBar.Visible
    Project.Item(ProjectIndex).frmProject.ProgressBar.Value = 0
    'Project.Item(ProjectIndex).frmProject.ProgressBar.Max = NLOADCASES * colcount * 3
    Project.Item(ProjectIndex).frmProject.ProgressBar.Max = 2 * (col.Count * NLOADCASES) + 3 * NLOADCASES
    'Project.Item(ProjectIndex).frmProject.ProgressBar.Max = 2 * (col.Count * NLOADCASES) + 18 * NLOADCASES
    find = ""
    Do Until find = "/neutral axis and moment of inertia/" Or ts.AtEndOfStream = True
        sLine = ReadLn(ts)
        find = LCase(Left(sLine, 36))
    Loop
    ts.SkipLine
    ts.SkipLine
    ts.SkipLine
    ts.SkipLine
    sLine = ReadLn(ts)
    GetValues 4, sLine, v
    NeutralAxis = Val_(v(3))
    Project.Item(ProjectIndex).cSolution.NeutralAxis = NeutralAxis
    NeutralAxis = Val_(v(4))
    Project.Item(ProjectIndex).cSolution.NeutralAxisGross = NeutralAxis
    
    sLine = ReadLn(ts)
    GetValues 4, sLine, v
    Project.Item(ProjectIndex).cSolution.Iyy = Val_(v(3))
    Project.Item(ProjectIndex).cSolution.IyyGross = Val_(v(4))
    
    sLine = ReadLn(ts)
    GetValues 4, sLine, v
    
    For i = 1 To NLOADCASES 'loop on load cases
        find = ""
        Do Until find = "/load case/" Or ts.AtEndOfStream = True
            sLine = ReadLn(ts)
            find = LCase(Left(sLine, 11))
        Loop
        
        ts.SkipLine
        ts.SkipLine
        ts.SkipLine
        ts.SkipLine
        
        For j = 1 To col.Count 'loop on panels
            If col.Item(j).pType <> Plate Then
                GoTo nextpanel
            End If
            For k = 1 To 3 'loop on diagrams
                sLine = ReadLn(ts)
                Select Case k
                    Case 1 'Txy
                        GetValues 8, sLine, v
                        For o = 1 To 2 ' net, gross
                            POL(i)(j)(k)(o)(1, 5) = Val_(v(3 + 3 * (o - 1)))
                            POL(i)(j)(k)(o)(2, 5) = Val_(v(4 + 3 * (o - 1)))
                            POL(i)(j)(k)(o)(3, 5) = Val_(v(5 + 3 * (o - 1)))
                        Next o
                    Case 2 'Sx
                        GetValues 6, sLine, v
                        For o = 1 To 2
                            POL(i)(j)(k)(o)(1, 5) = Val_(v(3 + 2 * (o - 1)))
                            POL(i)(j)(k)(o)(3, 5) = Val_(v(4 + 2 * (o - 1)))
                            POL(i)(j)(k)(o)(2, 5) = (POL(i)(j)(k)(o)(1, 5) + POL(i)(j)(k)(o)(3, 5)) / 2
                        Next o
                    Case 3 'Sx_stiff
                        GetValues 3, sLine, v
                        For o = 1 To 2
                            POL(i)(j)(k)(o)(1, 5) = Val_(v(3))
                            POL(i)(j)(k)(o)(2, 5) = POL(i)(j)(k)(o)(1, 5)
                            POL(i)(j)(k)(o)(3, 5) = POL(i)(j)(k)(o)(1, 5)
                        Next o
                        
                End Select
                
            Next k
nextpanel:
Project.Item(ProjectIndex).frmProject.ProgressBar.Value = Project.Item(ProjectIndex).frmProject.ProgressBar.Value + 1
        Next j
    Next i
    
    ts.Close
    ReadSolutionBeamTheory = True
    Exit Function
ReadSolutionBeamTheoryErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modPostProcessor: Function ReadSolutionBeamTheory")
    ReadSolutionBeamTheory = False
End Function

Private Function ReadSolutionLBR4() As Boolean
    On Error GoTo ReadSolutionLBR4Err
    Dim i As Integer, j As Integer, k As Integer, m As Integer, o As Integer
    Dim v As Variant
    ReDim Vector(31, 5) As Double
    GetKeyDiag 'Get KeyWords
    Project.Item(ProjectIndex).frmProject.ProgressBar.Visible = _
    Project.Item(ProjectIndex).frmProject.StatusBar.Visible
    Project.Item(ProjectIndex).frmProject.ProgressBar.Value = 0
    Project.Item(ProjectIndex).frmProject.ProgressBar.Max = 2 * (col.Count * NLOADCASES) + 18 * NLOADCASES
    
    If Project.Item(ProjectIndex).cHeader.IOPTI = yes Then
NextIteration:
        find = ""
        Do Until find = "iteration nø" Or find = "(iteration n" 'Or ts.AtEndOfStream = True
            If ts.AtEndOfStream = True Then
                MsgBox "Error Reading Solution.", vbCritical + vbOKOnly
                Exit Function
            End If
            sLine = ReadLn(ts)
            sLine = LTrim(sLine)
            sLine = LTrim(sLine)
            find = LCase(Left(sLine, 12))
        Loop
        If Project.Item(ProjectIndex).cHeader.ITERAM > 1 Then
            If Val(right(sLine, Len(sLine) - Len(find))) = Project.Item(ProjectIndex).cHeader.ITERAM Then
            Else
                GoTo NextIteration
            End If
        End If
    End If
    find = ""
    Do Until find = "1- Inertie flexionnelle"
        sLine = ReadLn(ts)
        sLine = LTrim(sLine)
        find = Left(sLine, 23)
    Loop
    sLine = ReadLn(ts)
    sLine = ReadLn(ts)
    Select Case LCase(Left(sLine, 5))
        Case "coord"
            ts.SkipLine
            sLine = ReadLn(ts)
            GetValues 4, sLine, v
            NeutralAxis = Val_(v(2))
            Project.Item(ProjectIndex).cSolution.NeutralAxis = NeutralAxis
        Case "a uti"
            ts.SkipLine
            ts.SkipLine
            ts.SkipLine
            ts.SkipLine
            sLine = ReadLn(ts)
            GetValues 3, sLine, v
            NeutralAxis = Val_(v(3))
            Project.Item(ProjectIndex).cSolution.NeutralAxis = NeutralAxis
    End Select
    
    'read Iyy
    ts.SkipLine
    ts.SkipLine
    sLine = ReadLn(ts)
    GetValues 3, sLine, v
    Project.Item(ProjectIndex).cSolution.Iyy = Val_(v(3))
    
    For i = 1 To NLOADCASES
        For j = 1 To col.Count
            For k = 1 To 18
            find = " "
            Select Case Project.Item(ProjectIndex).cHeader.IMPR2
                Case -3
                    Select Case k
                        Case 4 To 18
                            GoTo NextDiag
                    End Select
                Case -2, -1
                    Select Case Project.Item(ProjectIndex).cHeader.INDAIG
                        Case 0
                            Select Case k
                                Case 4 To 10, 15 To 18
                                    GoTo NextDiag
                            End Select
                        Case 1
                            Select Case k
                                Case 4 To 10
                                    GoTo NextDiag
                            End Select
                    End Select
                Case 0
                    Select Case Project.Item(ProjectIndex).cHeader.INDAIG
                        Case 0
                            Select Case k
                                Case 15 To 18
                                    GoTo NextDiag
                            End Select
                        Case 1
                    End Select
                Case 1
                    Select Case Project.Item(ProjectIndex).cHeader.INDAIG
                        Case 0
                            Select Case k
                                Case 15 To 18
                                    GoTo NextDiag
                            End Select
                        Case 1
                    End Select
            End Select
            Select Case col.Item(j).pType
                Case Plate, DoubleHull
                    Do Until find = KeyDiagPLATE(k)
                    sLine = ReadLn(ts)
                    sLine = LTrim(sLine)
                    find = Left(sLine, 13)
                Loop
                Case Beam
                    Select Case KeyDiagBEAM(k)
                        Case "NONE"
                            GoTo NextDiag
                    End Select
                    Do Until find = KeyDiagBEAM(k)
                    sLine = ReadLn(ts)
                    sLine = LTrim(sLine)
                    find = Left(sLine, 13)
                Loop
            End Select
                    ts.SkipLine
                    ts.SkipLine
                    ts.SkipLine
                    ts.SkipLine
                    For m = 1 To 31
                        sLine = ReadLn(ts)
                        strInStr = InStr(1, sLine, "m")
                        sLine = Mid(sLine, strInStr + 1)
                        GetValues 5, sLine, v
                        For o = 1 To 5
                            POL(i)(j)(k)(o)(m, 5) = Val_(v(o))
                        Next o
                    Next m
NextDiag:
            Next k

Project.Item(ProjectIndex).frmProject.ProgressBar.Value = Project.Item(ProjectIndex).frmProject.ProgressBar.Value + 1
nextpanel:
        Next j
    Next i
    
    ts.Close
    ReadSolutionLBR4 = True
    Exit Function
ReadSolutionLBR4Err:
    'MsgBox "Error in Sub ReadSolutionLBR4.", vbCritical + vbOKOnly
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modPostProcessor: Function ReadSolutionLBR4")
    ReadSolutionLBR4 = False
End Function

Private Sub GetKeyDiag()
    ReDim KeyDiagPLATE(18) As String
    KeyDiagPLATE(1) = "V (m) - DEPLA"
    KeyDiagPLATE(2) = "U (m) - DEPLA"
    KeyDiagPLATE(3) = "W (m) - FLECH"
    KeyDiagPLATE(4) = "Nx (N/m) - Ef"
    KeyDiagPLATE(5) = "Ny (N/m) - Ef"
    KeyDiagPLATE(6) = "Nxy (N/m) - E"
    KeyDiagPLATE(7) = "Mx (N.m/m)- M"
    KeyDiagPLATE(8) = "My (N.m/m)- M"
    KeyDiagPLATE(9) = "Ry (N/m) - Ki"
    KeyDiagPLATE(10) = "Rx (N/m) - Ki"
    KeyDiagPLATE(11) = "Sy (N/m2) - C"
    KeyDiagPLATE(12) = "Sx (N/m2) - C"
    KeyDiagPLATE(13) = "Txy (N/m2) - "
    KeyDiagPLATE(14) = "Scomp (N/m2) "
    KeyDiagPLATE(15) = "Sy Cadre (JAS"
    KeyDiagPLATE(16) = "Tyz Cadre (JA"
    KeyDiagPLATE(17) = "Sc Cadre (JAS"
    KeyDiagPLATE(18) = "Sc Cadre (JAB"
    
    ReDim KeyDiagBEAM(18) As String
    KeyDiagBEAM(1) = "V (m) - DEPLA"
    KeyDiagBEAM(2) = "NONE"
    KeyDiagBEAM(3) = "W (m) - FLECH"
    KeyDiagBEAM(4) = "NONE"
    KeyDiagBEAM(5) = "Ny (N/m) - Ef"
    KeyDiagBEAM(6) = "NONE"
    KeyDiagBEAM(7) = "NONE"
    KeyDiagBEAM(8) = "My (N.m/m)- M"
    KeyDiagBEAM(9) = "Ry (N/m) - Ki"
    KeyDiagBEAM(10) = "NONE"
    KeyDiagBEAM(11) = "NONE"
    KeyDiagBEAM(12) = "NONE"
    KeyDiagBEAM(13) = "NONE"
    KeyDiagBEAM(14) = "NONE"
    KeyDiagBEAM(15) = "Sy Ep.+ - Con"
    KeyDiagBEAM(16) = "Tyz Ep. (a.n."
    KeyDiagBEAM(17) = "Sc VM.Ep.+ - "
    KeyDiagBEAM(18) = "Sc VM.Ep. (a."
End Sub

Private Sub FindMinMaxAbs()
    Dim i As Integer
    Dim j As Integer
    Dim k As Integer
    Dim m As Integer
    Dim o As Integer

    For i = 1 To NLOADCASES
        For k = 1 To UBound(POG) '18
            For o = 1 To UBound(POX) '5
                ValMin = 0          ' daca vreau diagrame cu linii variabile pe fiecare sectiune,
                ValMax = 0          ' pun cele 3 Val = 0 inainte de o = 1 to 5
                ValAbs = 0
                For j = 1 To col.Count
                    For m = 1 To UBound(POP) '31
                        If POL(i)(j)(k)(o)(m, 5) > ValMax Then
                            ValMax = POL(i)(j)(k)(o)(m, 5)
                        End If
                    Next m
                    For m = 1 To UBound(POP) '31
                        If POL(i)(j)(k)(o)(m, 5) < ValMin Then
                            ValMin = POL(i)(j)(k)(o)(m, 5)
                        End If
                    Next m
                Next j
                If Abs(ValMax) > Abs(ValMin) Then
                    ValAbs = Abs(ValMax)
                End If
                If Abs(ValMax) <= Abs(ValMin) Then
                    ValAbs = Abs(ValMin)
                End If
                For j = 1 To col.Count
                    For m = 1 To UBound(POP) '31
                        
                        If Round(ValAbs, 5) <> 0 Then
                            POL(i)(j)(k)(o)(m, 6) = POL(i)(j)(k)(o)(m, 5) * ValScale / ValAbs
                            
                        Else
                            POL(i)(j)(k)(o)(m, 6) = 0
                        End If
                    Next m
                Next j
            Next o
            Project.Item(ProjectIndex).frmProject.ProgressBar.Value = Project.Item(ProjectIndex).frmProject.ProgressBar.Value + 1
        Next k
    Next i
End Sub

Private Sub FillMatrix()
    Dim i As Integer
    Dim j As Integer
    Dim k As Integer
    Dim m As Integer
    Dim o As Integer
    Dim Step As Double
    Dim DiagAngle() As Double
    ReDim DiagAngle(col.Count)
    For i = 1 To NLOADCASES
        For j = 1 To col.Count
            For k = 1 To UBound(POG) '18
                If IANA = 1 Then
                    Select Case k
                        Case 2, 4, 7, 12
                            DiagAngle(j) = -45
                        Case Else
                            DiagAngle(j) = angle(j) + 90
                    End Select
                ElseIf IANA = 2 Then
                    Select Case k
                        Case 2
                            DiagAngle(j) = -45
                        Case Else
                            DiagAngle(j) = angle(j) + 90
                    End Select
                End If
                POP(1, 1) = y1(j)
                POP(1, 2) = z1(j)
                POP(UBound(POP), 1) = y2(j)
                POP(UBound(POP), 2) = z2(j)
                Step = Length(j) / (UBound(POP) - 1) 'length(j) / 30
                For m = 1 To UBound(POP) - 2 '29
                    POP(m + 1, 1) = POP(1, 1) + Step * m * Cos(angle(j) * PI / 180)
                    POP(m + 1, 2) = POP(1, 2) + Step * m * Sin(angle(j) * PI / 180)
                Next m
                For m = 1 To UBound(POP) '31
                    For o = 1 To UBound(POX) '5
                        POL(i)(j)(k)(o)(m, 1) = POP(m, 1)
                        POL(i)(j)(k)(o)(m, 2) = POP(m, 2)
                        POL(i)(j)(k)(o)(m, 3) = POP(m, 1) + POL(i)(j)(k)(o)(m, 6) * Cos((DiagAngle(j)) * PI / 180)
                        POL(i)(j)(k)(o)(m, 4) = POP(m, 2) + POL(i)(j)(k)(o)(m, 6) * Sin((DiagAngle(j)) * PI / 180)
                    Next o
                Next m
            Next k
        Project.Item(ProjectIndex).frmProject.ProgressBar.Value = Project.Item(ProjectIndex).frmProject.ProgressBar.Value + 1
     Next j
    Next i
    'SendToClasses
    MsgBox "Solution Ready.", vbInformation + vbOKOnly
    Project.Item(ProjectIndex).frmProject.ProgressBar.Visible = False
    Set col = Nothing
    Project.Item(ProjectIndex).cSolution.IsSolution = True
    Project.Item(ProjectIndex).cSolution.Solution = POL
    fMainForm.SetFocus
    Exit Sub
1
    Project.Item(ProjectIndex).frmProject.ProgressBar.Visible = False
    MsgBox "Error!", vbCritical + vbOKOnly
    Project.Item(ProjectIndex).cSolution.IsSolution = False
End Sub

