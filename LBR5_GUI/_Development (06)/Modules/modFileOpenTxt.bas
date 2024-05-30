Attribute VB_Name = "modFileOpenTxt"
Option Explicit
Dim ProjectIndex As Integer
Dim sFileVer As String, sSubVer As String

Public Function OpenTxtFile(ts As TextStream, ByVal index As Integer)
    On Error GoTo OpenTxtFileErr
    Dim sLine As String
    Dim v() As Variant
    ProjectIndex = index
    sLine = ReadLn(ts)
    sLine = Trim(sLine)
    GetValues 1, sLine, v
    sFileVer = v(1)
    Project.Item(ProjectIndex).FileVersionNumber = sFileVer
    sSubVer = right(sFileVer, 1)
    Select Case sFileVer
        Case "verLBR5.7(24/03/04)"
            OpenTxtVer10 ts
        Case Else
            Select Case Left(sFileVer, 3)
                Case "1.1"
                    OpenTxtVer11 ts
                Case Else
            End Select
    End Select
    Select Case sFileVer
        Case VersionNumber
        Case Else
            MsgBox "This project was last saved in a previous file version: '" & sFileVer & "'" & vbCrLf & _
            "Next save command will update it to the last file version: '" & VersionNumber & _
            "'.", vbInformation + vbOKOnly
            Project.Item(ProjectIndex).frmProject.Form_Activate
    End Select
    Exit Function
OpenTxtFileErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modFileOpenMars: Function OpenTxtFile")
End Function

'version 1.0.0 (verLBR5.7(24/03/04))
Private Function OpenTxtVer10(ts As TextStream)
    On Error GoTo OpenTxtVer10Err
    Dim sLine As String
    Dim v() As Variant
    Dim i As Integer, j As Integer, k As Integer
    'HEADER
    'General Data
    Dim oHeader As cHeader
    Set oHeader = Project.Item(ProjectIndex).cHeader
    oHeader.IANA = 1
    sLine = ReadLn(ts)
    sLine = Trim(sLine)
    oHeader.Title = sLine
    'General parameters
    sLine = ReadLn(ts)
    GetValues 9, sLine, v
    oHeader.IMPR = Val_(v(1))
    oHeader.IMPR2 = Val_(v(2))
    oHeader.INDAIG = Val_(v(3))
    oHeader.INDRAID = Val_(v(4))
    oHeader.DESSIN = Val_(v(5))
    oHeader.JLPH = Val_(v(6))
    oHeader.JLBORD = Val_(v(7))
    oHeader.NETO = Val_(v(8))
    If IsNumeric(v(9)) = True Then oHeader.NoOfDoubleHulls = Val_(v(9))
    sLine = ReadLn(ts)
    GetValues 2, sLine, v
    oHeader.IOPTI = Val_(v(1))
    oHeader.ITERAM = Val_(v(2))
    sLine = ReadLn(ts)
    GetValues 1, sLine, v
    oHeader.ICOUT = Val_(v(1))
    'Cost Data
    Dim oCost As cCostData
    Set oCost = oHeader.cCostData
    sLine = ReadLn(ts)
    GetValues 2, sLine, v
    oCost.REND = Val_(v(1))
    oCost.EQP = Val_(v(2))
    sLine = ReadLn(ts)
    GetValues 3, sLine, v
    oCost.E0 = Val_(v(1))
    oCost.E0X = Val_(v(2))
    oCost.E0Y = Val_(v(3))
    sLine = ReadLn(ts)
    GetValues 4, sLine, v
    oCost.C1 = Val_(v(1))
    oCost.C2 = Val_(v(2))
    oCost.C3 = Val_(v(3))
    oCost.DC1 = Val_(v(4))
    sLine = ReadLn(ts)
    GetValues 2, sLine, v
    oCost.DW2 = Val_(v(1))
    oCost.DW3 = Val_(v(2))
    sLine = ReadLn(ts)
    GetValues 2, sLine, v
    oCost.P10 = Val_(v(1))
    oCost.DP10 = Val_(v(2))
    sLine = ReadLn(ts)
    GetValues 4, sLine, v
    oCost.p4 = Val_(v(1))
    oCost.P5 = Val_(v(2))
    oCost.DP4 = Val_(v(3))
    oCost.DP5 = Val_(v(4))
    sLine = ReadLn(ts)
    GetValues 4, sLine, v
    oCost.P9X = Val_(v(1))
    oCost.P9Y = Val_(v(2))
    oCost.DP9X = Val_(v(3))
    oCost.DP9Y = Val_(v(4))
    sLine = ReadLn(ts)
    GetValues 4, sLine, v
    oCost.P6 = Val_(v(1))
    oCost.P7 = Val_(v(2))
    oCost.BETA_X = Val_(v(3))
    oCost.BETA_Y = Val_(v(4))
    sLine = ReadLn(ts)
    GetValues 4, sLine, v
    oCost.C8 = Val_(v(1))
    oCost.DC8 = Val_(v(2))
    oCost.ALPHA_X = Val_(v(3))
    oCost.ALPHA_Y = Val_(v(4))
    '
    sLine = ReadLn(ts)
    GetValues 1, sLine, v
    oHeader.Width = Val_(v(1))
    sLine = ReadLn(ts)
    GetValues 5, sLine, v
    oHeader.DIS1 = Val_(v(1))
    oHeader.DIS2 = Val_(v(2))
    oHeader.DIS3 = Val_(v(3))
    oHeader.DIS4 = Val_(v(4))
    oHeader.DIS5 = Val_(v(5))
    sLine = ReadLn(ts)
    GetValues 6, sLine, v
    oHeader.FAM1 = Val_(v(1))
    oHeader.FAM2 = Val_(v(2))
    oHeader.FAM3 = Val_(v(3))
    oHeader.FAM4 = Val_(v(4))
    oHeader.FAM5 = Val_(v(5))
    oHeader.FAM6 = Val_(v(6))
    sLine = ReadLn(ts)
    GetValues 1, sLine, v
    oHeader.IPOIDS = Val_(v(1))
    'Load Cases
    sLine = ReadLn(ts)
    GetValues 2, sLine, v
    For i = 1 To Val_(v(1))
        Dim m As New cLoadCase
        m.index = i
        oHeader.colLoadCase.Add m, i
        Set m = Nothing
    Next i
    GetValues Val_(v(2)), sLine, v
    For i = 1 To UBound(v)
        For j = 1 To oHeader.colLoadCase.Count
            If oHeader.colLoadCase.Item(j).index = Val_(v(i)) Then
                oHeader.colLoadCase.Item(j).state = IsOn
            End If
        Next j
    Next i
    For i = 1 To oHeader.colLoadCase.Count
        sLine = ReadLn(ts)
        sLine = LTrim(sLine)
        sLine = RTrim(sLine)
        oHeader.colLoadCase.Item(i).Title = sLine
    Next i
    'PANELS
    Dim oPan As cPanel
    Dim colPan As colPanel
    Set colPan = Project.Item(ProjectIndex).colPanel
    For i = 1 To oHeader.NETO
        Set oPan = New cPanel
        oPan.index = i
        oPan.pNumber = i
NextLine:
        sLine = ReadLn(ts)
        sLine = LTrim(sLine)
        Select Case Left(UCase(sLine), 5)
            Case "PLAQU"
                oPan.pType = Plate
            Case "EPONT"
                oPan.pType = Beam
            Case "COQUE"
                Err.Description = "SHELL element not implemented."
                Err.Raise 9999, , Err.Description
            Case "DCOQU"
                oPan.pType = DoubleHull
            Case Else
            GoTo NextLine
        End Select
        sLine = ReadLn(ts)
        GetValues 2, sLine, v
        oPan.cGeometry.PanelWidth = Abs(Val_(v(1)))
        oPan.cScantlings.NetThickness = Val_(v(2))
        Select Case oPan.pType
            Case Plate, DoubleHull
                sLine = ReadLn(ts)
                GetValues 5, sLine, v
                oPan.cScantlings.cPrimaryFrames.Spacing = Val_(v(1))
                oPan.cScantlings.cPrimaryStiffeners.Spacing = Val_(v(2))
                oPan.cScantlings.cSecondaryFrames.Spacing = Val_(v(3))
                oPan.cScantlings.cSecondaryStiffeners.Spacing = Val_(v(4))
                oPan.cScantlings.CorrosionThickness = Val_(v(5))
                oPan.cScantlings.GrossThickness = oPan.cScantlings.NetThickness + oPan.cScantlings.CorrosionThickness
                sLine = ReadLn(ts)
                GetValues 5, sLine, v
                oPan.cScantlings.cPrimaryFrames.WebHeight = Val_(v(1))
                oPan.cScantlings.cPrimaryFrames.WebThickness = Val_(v(2))
                oPan.cScantlings.cPrimaryFrames.FlangeWidth = Val_(v(3))
                oPan.cScantlings.cPrimaryFrames.FlangeThickness = Val_(v(4))
                oPan.cScantlings.cPrimaryFrames.CorrosionThickness = Val_(v(5))
                sLine = ReadLn(ts)
                GetValues 5, sLine, v
                oPan.cScantlings.cPrimaryStiffeners.WebHeight = Val_(v(1))
                oPan.cScantlings.cPrimaryStiffeners.WebThickness = Val_(v(2))
                oPan.cScantlings.cPrimaryStiffeners.FlangeWidth = Val_(v(3))
                oPan.cScantlings.cPrimaryStiffeners.FlangeThickness = Val_(v(4))
                oPan.cScantlings.cPrimaryStiffeners.CorrosionThickness = Val_(v(5))
                oPan.cScantlings.cSecondaryFrames.Side = SideLeft
                oPan.cScantlings.cSecondaryStiffeners.Side = SideLeft
                If oPan.cScantlings.cSecondaryFrames.Spacing > 0 Or oPan.cScantlings.cSecondaryStiffeners.Spacing > 0 Then
                    sLine = ReadLn(ts)
                    GetValues 5, sLine, v
                    oPan.cScantlings.cSecondaryFrames.WebHeight = Val_(v(1))
                    oPan.cScantlings.cSecondaryFrames.WebThickness = Val_(v(2))
                    oPan.cScantlings.cSecondaryFrames.FlangeWidth = Val_(v(3))
                    oPan.cScantlings.cSecondaryFrames.FlangeThickness = Val_(v(4))
                    oPan.cScantlings.cSecondaryFrames.Side = Val_(v(5))
                    sLine = ReadLn(ts)
                    GetValues 5, sLine, v
                    oPan.cScantlings.cSecondaryStiffeners.WebHeight = Val_(v(1))
                    oPan.cScantlings.cSecondaryStiffeners.WebThickness = Val_(v(2))
                    oPan.cScantlings.cSecondaryStiffeners.FlangeWidth = Val_(v(3))
                    oPan.cScantlings.cSecondaryStiffeners.FlangeThickness = Val_(v(4))
                    oPan.cScantlings.cSecondaryStiffeners.Side = Val_(v(5))
                End If
                sLine = ReadLn(ts)
                GetValues 1, sLine, v
                oPan.cScantlings.cPrimaryStiffeners.DistributionMode = v(1)
                If oPan.pType = DoubleHull Then
                    sLine = ReadLn(ts)
                    GetValues 1, sLine, v
                    oPan.RelatedDoubleHullPanel = Val_(v(1))
                End If
                sLine = ReadLn(ts)
                GetValues 1, sLine, v
                oPan.cGeometry.Participation = Val_(v(1))
            Case Beam
                sLine = ReadLn(ts)
                GetValues 1, sLine, v
                Select Case UCase(v(1))
                    Case "CERCLE"
                        oPan.cScantlings.BeamSection = bsCircle
                    Case "CARRE"
                        oPan.cScantlings.BeamSection = bsSquare
                    Case "DOUBLET"
                        oPan.cScantlings.BeamSection = bsDoubleT
                    Case Else
                    Err.Description = "BEAM Section " & "'" & v(1) & "'" & " not recognized."
                    Err.Raise 9999, , Err.Description
                End Select
                sLine = ReadLn(ts)
                GetValues 2, sLine, v
                oPan.cScantlings.cPrimaryFrames.Spacing = Val_(v(1))
                oPan.cGeometry.BucklingLength = Val_(v(2))
                Select Case oPan.cScantlings.BeamSection
                    Case bsCircle
                        sLine = ReadLn(ts)
                        GetValues 2, sLine, v
                        oPan.cScantlings.cPrimaryFrames.WebHeight = Val_(v(1))
                        oPan.cScantlings.cPrimaryFrames.CorrosionThickness = Val_(v(2))
                    Case bsSquare
                        sLine = ReadLn(ts)
                        GetValues 2, sLine, v
                        oPan.cScantlings.cPrimaryFrames.WebHeight = Val_(v(1))
                        oPan.cScantlings.cPrimaryFrames.CorrosionThickness = Val_(v(2))
                    Case bsDoubleT
                        sLine = ReadLn(ts)
                        GetValues 5, sLine, v
                        oPan.cScantlings.cPrimaryFrames.WebHeight = Val_(v(1)) * 2 ' to modify when Judith modifies in LBR5
                        oPan.cScantlings.cPrimaryFrames.WebThickness = Val_(v(2))  ' see ReadLBR5txtFile as well
                        oPan.cScantlings.cPrimaryFrames.FlangeWidth = Val_(v(3))
                        oPan.cScantlings.cPrimaryFrames.FlangeThickness = Val_(v(4))
                        oPan.cScantlings.cPrimaryFrames.CorrosionThickness = Val_(v(5))
                        oPan.cScantlings.NetThickness = Empty
                End Select
        End Select
        'Materials
        Dim oMat As cMaterial
        Set oMat = oPan.cMaterial
        sLine = ReadLn(ts)
        GetValues 5, sLine, v
        oMat.YoungModulus = Val_(v(1))
        oMat.Poisson = Val_(v(2))
        oMat.YieldStress = Val_(v(3))
        oMat.AllowableStress = Val_(v(4))
        oMat.SpecificWeight = Val_(v(5))
                
        Dim NoOfGirders As Integer
        sLine = ReadLn(ts)
        GetValues 7, sLine, v
        NoOfGirders = Val_(v(1))
        oPan.cScantlings.cPrimaryStiffeners.Side = Val(v(2))
        oPan.cScantlings.cPrimaryFrames.Side = Val_(v(3))
        oPan.cScantlings.GirderSide = Val_(v(4))
        If Val_(v(5)) = 0 Then v(5) = 1
        oPan.LateralPressureSide = Val_(v(5))
        oPan.IsBuoyancyForce = Val_(v(6))
        oPan.UniformLateralPressureVariation = Val_(v(7))
        sLine = ReadLn(ts)
        GetValues 1, sLine, v
        oPan.cGeometry.PanelAngle = Val_(v(1))
        
        For j = 1 To Project.Item(ProjectIndex).cHeader.colLoadCase.Count
            Set m = New cLoadCase
            oPan.colLoadCase.Add m, j
            Set m = Nothing
        Next j
        For j = 1 To oPan.colLoadCase.Count
            sLine = ReadLn(ts)
            GetValues 2, sLine, v
            oPan.colLoadCase.Item(j).index = j
            oPan.colLoadCase.Item(j).Title = Project.Item(ProjectIndex).cHeader.colLoadCase.Item(j).Title
            oPan.colLoadCase.Item(j).LateralPressureIn = Val_(v(1))
            oPan.colLoadCase.Item(j).LateralPressureOut = Val_(v(2))
        Next j
        sLine = ReadLn(ts)
        GetValues 1, sLine, v
        oPan.LocalizedPressure = Val_(v(1))
        sLine = ReadLn(ts)
        GetValues 1, sLine, v
        oPan.IsWiseLateralPressure = Val_(v(1))
        If oPan.IsWiseLateralPressure = yes Then
            For j = 1 To oPan.colLoadCase.Count
                ts.SkipLine
                sLine = ReadLn(ts)
                GetValues 1, sLine, v
                Dim iStep As Integer
                Dim oStep As cStepWiseLateralPressure
                Dim colStep As colStepWiseLateralPressure
                Set colStep = oPan.colLoadCase.Item(j).colStepWiseLateralPressure
                iStep = Val_(v(1))
                For k = 1 To iStep
                    Set oStep = New cStepWiseLateralPressure
                    sLine = ReadLn(ts)
                    GetValues 3, sLine, v
                    oStep.index = k
                    oStep.VerticalGravityLoad = Val_(v(1))
                    oStep.LateralPressureIn = Val_(v(2))
                    oStep.LateralPressureOut = Val_(v(3))
                    colStep.Add oStep, oStep.index
                    Set oStep = Nothing
                Next k
            Next j
        End If
        
        'Panel Connections
        Dim col As Collection
        sLine = ReadLn(ts)
        GetValues 10, sLine, v
        Set col = New Collection
        For j = 1 To 10
            col.Add Val_(v(j))
        Next j
        Set oPan.colConnections = col
        Set col = Nothing
        'Girders
        Dim oGirder As cGirder
        Dim colGirder As colGirder
        Set colGirder = oPan.cScantlings.colGirder
        For j = 1 To NoOfGirders
            Set oGirder = New cGirder
            oGirder.index = j
            sLine = ReadLn(ts)
            GetValues 5, sLine, v
            oGirder.WebHeight = Val_(v(1))
            oGirder.WebThickness = Val_(v(2))
            oGirder.FlangeWidth = Val_(v(3))
            oGirder.FlangeThickness = Val_(v(4))
            oGirder.Distance = Val_(v(5))
            colGirder.Add oGirder, oGirder.index
            Set oGirder = Nothing
        Next j
        'Design Variables
        ts.SkipLine
        sLine = ReadLn(ts)
        GetValues 1, sLine, v
        Dim iDes As Integer
        Dim oDes As cDesignVariables
        Dim colDes As colDesignVariables
        Set colDes = oPan.colDesignVariables
        iDes = Val_(v(1))
        ''Default Design Variables
        ''------------------------
        'Dim v() As Variant
        GetScantlings oPan.cScantlings, v
        For j = 1 To 9
            Set oDes = New cDesignVariables
            oDes.index = j
            oDes.Active = False
            oDes.VariableName = j
            Select Case oHeader.colDefaultDesignVariables.Item(j).LowerLimit
                Case Is < v(j)
                    oDes.LowerLimit = oHeader.colDefaultDesignVariables.Item(j).LowerLimit
                Case Else
                    oDes.LowerLimit = v(j)
            End Select
            Select Case oHeader.colDefaultDesignVariables.Item(j).UpperLimit
                Case Is > v(j)
                    oDes.UpperLimit = oHeader.colDefaultDesignVariables.Item(j).UpperLimit
                Case Else
                    oDes.UpperLimit = v(j)
            End Select
            colDes.Add oDes, j
        Next j
        ''------------------------

        Dim listvar() As Variant
        Dim varinf() As Variant
        Dim varsup() As Variant
        Dim index_listvar As Integer
        Dim a As Integer
        a = oPan.index
        index_listvar = 1
        Select Case iDes
            Case Is = 0
            Case Is > 0
                sLine = ReadLn(ts)
                GetValues iDes, sLine, listvar
                sLine = ReadLn(ts)
                GetValues iDes, sLine, varinf
                sLine = ReadLn(ts)
                GetValues iDes, sLine, varsup
                For j = 1 To 9
                    If index_listvar > iDes Then Exit For
                    If j = Val_(listvar(index_listvar)) Then
                        colDes.Item(j).Active = True
                        colDes.Item(j).VariableName = j
                        colDes.Item(j).LowerLimit = Val_(varinf(index_listvar))
                        colDes.Item(j).UpperLimit = Val_(varsup(index_listvar))
                        index_listvar = index_listvar + 1
                    End If
                Next j
        End Select
        
        'Structural Constraints
        Dim oStru As cStructuralConstraints
        ts.SkipLine
        sLine = ReadLn(ts)
        GetValues 1, sLine, v
        oPan.IsStructuralConstraints = Val_(v(1))
        If oPan.IsStructuralConstraints = yes Then
            sLine = ReadLn(ts)
            Dim NoOfPoints As Integer
            GetValues 1, sLine, v
            NoOfPoints = Val_(v(1))
            sLine = ReadLn(ts)
            GetValues NoOfPoints, sLine, v
            Dim r As Double
            For j = 1 To NoOfPoints
                Dim assp As New cAssessmentPoint
                assp.index = j
                assp.Value = Val_(v(j))
                oPan.colAssessmentPoints.Add assp, j
                Set assp = Nothing
            Next j
            For j = 1 To oPan.colLoadCase.Count
                ts.SkipLine
                sLine = ReadLn(ts)
                GetValues 1, sLine, v
                Dim NoOfStrRestr As Integer
                NoOfStrRestr = Val_(v(1))
                If NoOfStrRestr < 0 Then
                    NoOfStrRestr = -NoOfStrRestr
                    For k = 1 To oPan.colLoadCase.Item(NoOfStrRestr).colStructuralConstraints.Count
                        Set oStru = New cStructuralConstraints
                        oStru.index = oPan.colLoadCase.Item(NoOfStrRestr).colStructuralConstraints.Item(k).index
                        oStru.Reference = oPan.colLoadCase.Item(NoOfStrRestr).colStructuralConstraints.Item(k).Reference
                        oStru.Value = oPan.colLoadCase.Item(NoOfStrRestr).colStructuralConstraints.Item(k).Value
                        oStru.Limit = oPan.colLoadCase.Item(NoOfStrRestr).colStructuralConstraints.Item(k).Limit
                        oStru.AssesmentPoint = oPan.colLoadCase.Item(NoOfStrRestr).colStructuralConstraints.Item(k).AssesmentPoint
                        oPan.colLoadCase.Item(j).colStructuralConstraints.Add oStru, k
                        Set oStru = Nothing
                    Next k
                    GoTo NextLoadCase
                End If
                For k = 1 To NoOfStrRestr
                    Set oStru = New cStructuralConstraints
                    sLine = ReadLn(ts)
                    GetValues 5, sLine, v
                    oStru.index = Val_(v(1))
                    oStru.Reference = Val_(v(2))
                    oStru.Value = Val_(v(3))
                    oStru.Limit = Val_(v(4))
                    oStru.AssesmentPoint = Val_(v(5))
                    oPan.colLoadCase.Item(j).colStructuralConstraints.Add oStru, k
                    Set oStru = Nothing
                Next k
NextLoadCase:
            Next j
        End If
        'Geometrical Constraints
        Dim oGeo As cGeometricalConstraints
        ts.SkipLine
        sLine = ReadLn(ts)
        GetValues 1, sLine, v
        oPan.NoOfGeomConstr = Val_(v(1))
        Select Case oPan.NoOfGeomConstr
            Case 0
            Case 99
                Set oGeo = New cGeometricalConstraints
                sLine = ReadLn(ts)
                GetValues 1, sLine, v
                oGeo.index = 1
                oGeo.code = Val_(v(1))
                oPan.colGeometricalConstraints.Add oGeo, 1
                sLine = ReadLn(ts)
                GetValues 2, sLine, v
                oPan.FramesFlangeThicknessUpdate = Val_(v(1))
                oPan.StiffenersFlangeThicknessUpdate = Val_(v(2))
                Set oGeo = Nothing
            Case Else
                sLine = ReadLn(ts)
                GetValues oPan.NoOfGeomConstr, sLine, v
                For j = 1 To oPan.NoOfGeomConstr
                    Set oGeo = New cGeometricalConstraints
                    oGeo.index = j
                    oGeo.code = Val_(v(j))
                    oPan.colGeometricalConstraints.Add oGeo, j
                    Set oGeo = Nothing
                Next j
                sLine = ReadLn(ts)
                GetValues 2, sLine, v
                oPan.FramesFlangeThicknessUpdate = Val_(v(1))
                oPan.StiffenersFlangeThicknessUpdate = Val_(v(2))
        End Select
        colPan.Add oPan, oPan.index
        Set oPan = Nothing
    Next i
    
    'GLOBAL DATA
    'Boundary Conditions
    Dim oBound As cBoundaryConditions
    ts.SkipLine
    sLine = ReadLn(ts)
    GetValues 1, sLine, v
    Dim NoOfBoundaryCond As Integer
    NoOfBoundaryCond = Val_(v(1))
    For i = 1 To NoOfBoundaryCond
        sLine = ReadLn(ts)
        GetValues 2, sLine, v
        Select Case Project.Item(ProjectIndex).colPanel.Count
            Case 1
                Set oBound = New cBoundaryConditions
                oBound.index = i
                oBound.BoundaryCondition = Val_(v(2))
                If i = 1 Then
                    oBound.Edge = InEdge
                ElseIf i = 2 Then
                    oBound.Edge = OutEdge
                End If
                Project.Item(ProjectIndex).colPanel.Item(1).colBoundaryConditions.Add oBound, oBound.index
                Set oBound = Nothing
            Case Is > 1
                Set oBound = New cBoundaryConditions
                oBound.index = 1
                oBound.BoundaryCondition = Val_(v(2))
                Project.Item(ProjectIndex).colPanel.Item(Val_(v(1))).colBoundaryConditions.Add oBound, oBound.index
                Set oBound = Nothing
        End Select
    Next i
    'Gravity Center
    ts.SkipLine
    sLine = ReadLn(ts)
    GetValues 2, sLine, v
    oHeader.YAxisOrigin = Val_(v(1))
    oHeader.ZAxisOrigin = Val_(v(2))
    Dim oGrav As cGlobalConstraints
    Set oGrav = oHeader.cGlobalConstraints
    'oHeader.cGlobalConstraints.ReadLBR5txtFile ts
    sLine = ReadLn(ts)
    GetValues 3, sLine, v
    oGrav.GravityLimitRestriction = Val_(v(1))
    oGrav.MinGravityCenter = Val_(v(2))
    oGrav.MaxGravityCenter = Val_(v(3))

    'Bending Moments
    ts.SkipLine
    sLine = ReadLn(ts)
    GetValues 1, sLine, v
    oHeader.IsBendingMoments = Val_(v(1))
    Select Case oHeader.IsBendingMoments
        Case no
        Case yes
            sLine = ReadLn(ts)
            GetValues 1, sLine, v
            oHeader.YRED = Val_(v(1))
            For i = 1 To oHeader.colLoadCase.Count
                sLine = ReadLn(ts)
                GetValues 4, sLine, v
                oHeader.colLoadCase.Item(i).VerticalBendingMomentFore = Round(Val_(v(1)), 6)
                'oHeader.colLoadCase.Item(i).VerticalBendingMomentAft = Round(Val_(v(2)), 6)
                oHeader.colLoadCase.Item(i).VerticalShear = Round(Val_(v(2)), 6)
                oHeader.colLoadCase.Item(i).HorizontalBendingMomentFore = Val_(v(3))
                'oHeader.colLoadCase.Item(i).HorizontalBendingMomentAft = Val_(v(4))
                oHeader.colLoadCase.Item(i).HorizontalShear = Round(Val_(v(4)), 6)
            Next i
    End Select
    'Copy bending moments in panel's load cases collections
    For i = 1 To oHeader.NETO
        For j = 1 To oHeader.colLoadCase.Count
            Project.Item(ProjectIndex).colPanel.Item(i).colLoadCase.Item(j).VerticalBendingMomentFore = oHeader.colLoadCase.Item(j).VerticalBendingMomentFore
            'Project.Item(ProjectIndex).colPanel.Item(i).colLoadCase.Item(j).VerticalBendingMomentAft = oHeader.colLoadCase.Item(j).VerticalBendingMomentAft
            Project.Item(ProjectIndex).colPanel.Item(i).colLoadCase.Item(j).VerticalShear = oHeader.colLoadCase.Item(j).VerticalShear
            Project.Item(ProjectIndex).colPanel.Item(i).colLoadCase.Item(j).HorizontalBendingMomentFore = oHeader.colLoadCase.Item(j).HorizontalBendingMomentFore
            'Project.Item(ProjectIndex).colPanel.Item(i).colLoadCase.Item(j).HorizontalBendingMomentAft = oHeader.colLoadCase.Item(j).HorizontalBendingMomentAft
            Project.Item(ProjectIndex).colPanel.Item(i).colLoadCase.Item(j).HorizontalShear = oHeader.colLoadCase.Item(j).HorizontalShear
        Next j
    Next i
    'Set Me.colLoadCase = Nothing
    ' Equality constraints
    ts.SkipLine
    sLine = ReadLn(ts)
    GetValues 1, sLine, v
    Dim NoOfEqRestr As Integer
    Dim oEq As cEqualityRestrictions
    NoOfEqRestr = Val_(v(1))
    For i = 1 To NoOfEqRestr
        Set oEq = New cEqualityRestrictions
        oEq.index = i
        'Me.colEqualityRestrictions.Add oEq, i
        'Me.colEqualityRestrictions.Item(i).ReadLBR5txtFile ts
        sLine = ReadLn(ts)
        GetValues 5, sLine, v
        oEq.DependingDesignVariable = Val_(v(1))
        oEq.DependingPanel = Val_(v(2))
        oEq.LeadingDesignVariable = Val_(v(3))
        oEq.LeadingPanel = Val_(v(4))
        oEq.Ratio = Val_(v(5))
        oHeader.colEqualityRestrictions.Add oEq, oEq.index
        Set oEq = Nothing
    Next i
    'Ultimate strenght
    ts.SkipLine
    sLine = ReadLn(ts)
    GetValues 1, sLine, v
    oHeader.IRESTR = Val_(v(1))
    sLine = ReadLn(ts)
    GetValues 1, sLine, v
    oHeader.IULT = Val_(v(1))
          
    Call NodeGenerator
    UpdateBoundary ProjectIndex
    Dim cPanel As cPanel
    For Each cPanel In Project.Item(ProjectIndex).colPanel
        cPanel.cCostCAtMain.SetFirstPanelData cPanel.pNumber, ProjectIndex
    Next cPanel
    ComputeFirstFractionnements ProjectIndex

    Exit Function
OpenTxtVer10Err:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "OpenTxtVer10: Function OpenTxtVer10")
    ts.Close
End Function

'version 1.1...
Private Function OpenTxtVer11(ts As TextStream)
    On Error GoTo OpenTxtVer11Err
    Dim sLine As String
    Dim v() As Variant
    Dim i As Integer, j As Integer, k As Integer
    'HEADER
    'General Data
    Dim oHeader As cHeader
    Set oHeader = Project.Item(ProjectIndex).cHeader
    If Val_(right(sFileVer, 1)) >= 2 Then
        sLine = ReadLn(ts)
        GetValues 1, sLine, v
        oHeader.IANA = CInt(v(1))
    Else
        oHeader.IANA = 1
    End If
    'verif iana licensing
    If Licensing.IS_LBR4 = False Then Project.Item(ActiveProject).cHeader.IANA = 2
    If Licensing.IS_BEAM_THEORY = False Then Project.Item(ActiveProject).cHeader.IANA = 1
    
    sLine = ReadLn(ts)
    sLine = Trim(sLine)
    oHeader.Title = sLine
    'General parameters
    sLine = ReadLn(ts)
    GetValues 9, sLine, v
    oHeader.IMPR = Val_(v(1))
    oHeader.IMPR2 = Val_(v(2))
    oHeader.INDAIG = Val_(v(3))
    oHeader.INDRAID = Val_(v(4))
    oHeader.DESSIN = Val_(v(5))
    oHeader.JLPH = Val_(v(6))
    oHeader.JLBORD = Val_(v(7))
    oHeader.NETO = Val_(v(8))
    If IsNumeric(v(9)) = True Then oHeader.NoOfDoubleHulls = Val_(v(9))
    'Multi Opti
    sLine = ReadLn(ts)
    GetValues 5, sLine, v
    oHeader.cMultiOpti.IMULTI = Val_(v(1))
    oHeader.cMultiOpti.RHO = Val_(v(2))
    oHeader.cMultiOpti.W1 = Val_(v(3))
    oHeader.cMultiOpti.W2 = Val_(v(4))
    oHeader.cMultiOpti.W3 = Val_(v(5))
    
    sLine = ReadLn(ts)
    GetValues 2, sLine, v
    oHeader.IOPTI = Val_(v(1))
    oHeader.ITERAM = Val_(v(2))
    
    sLine = ReadLn(ts)
    GetValues 1, sLine, v
    oHeader.ICOUT = Val_(v(1))
    'Cost Data
    Dim oCost As cCostData
    Set oCost = oHeader.cCostData
    sLine = ReadLn(ts)
    GetValues 2, sLine, v
    oCost.REND = Val_(v(1))
    oCost.EQP = Val_(v(2))
    sLine = ReadLn(ts)
    GetValues 3, sLine, v
    oCost.E0 = Val_(v(1))
    oCost.E0X = Val_(v(2))
    oCost.E0Y = Val_(v(3))
    sLine = ReadLn(ts)
    GetValues 4, sLine, v
    oCost.C1 = Val_(v(1))
    oCost.C2 = Val_(v(2))
    oCost.C3 = Val_(v(3))
    oCost.DC1 = Val_(v(4))
    sLine = ReadLn(ts)
    GetValues 2, sLine, v
    oCost.DW2 = Val_(v(1))
    oCost.DW3 = Val_(v(2))
    sLine = ReadLn(ts)
    GetValues 2, sLine, v
    oCost.P10 = Val_(v(1))
    oCost.DP10 = Val_(v(2))
    sLine = ReadLn(ts)
    GetValues 4, sLine, v
    oCost.p4 = Val_(v(1))
    oCost.P5 = Val_(v(2))
    oCost.DP4 = Val_(v(3))
    oCost.DP5 = Val_(v(4))
    sLine = ReadLn(ts)
    GetValues 4, sLine, v
    oCost.P9X = Val_(v(1))
    oCost.P9Y = Val_(v(2))
    oCost.DP9X = Val_(v(3))
    oCost.DP9Y = Val_(v(4))
    sLine = ReadLn(ts)
    GetValues 4, sLine, v
    oCost.P6 = Val_(v(1))
    oCost.P7 = Val_(v(2))
    oCost.BETA_X = Val_(v(3))
    oCost.BETA_Y = Val_(v(4))
    sLine = ReadLn(ts)
    GetValues 4, sLine, v
    oCost.C8 = Val_(v(1))
    oCost.DC8 = Val_(v(2))
    oCost.ALPHA_X = Val_(v(3))
    oCost.ALPHA_Y = Val_(v(4))
    '
    sLine = ReadLn(ts)
    If Val_(right(sFileVer, 1)) < 4 Then
        GetValues 1, sLine, v
        oHeader.Width = Val_(v(1))
    Else
        GetValues 2, sLine, v
        oHeader.Width = Val_(v(1))
        oHeader.LongRegl = Val_(v(2))
    End If
    
    sLine = ReadLn(ts)
    GetValues 5, sLine, v
    oHeader.DIS1 = Val_(v(1))
    oHeader.DIS2 = Val_(v(2))
    oHeader.DIS3 = Val_(v(3))
    oHeader.DIS4 = Val_(v(4))
    oHeader.DIS5 = Val_(v(5))
    sLine = ReadLn(ts)
    GetValues 6, sLine, v
    oHeader.FAM1 = Val_(v(1))
    oHeader.FAM2 = Val_(v(2))
    oHeader.FAM3 = Val_(v(3))
    oHeader.FAM4 = Val_(v(4))
    oHeader.FAM5 = Val_(v(5))
    oHeader.FAM6 = Val_(v(6))
    sLine = ReadLn(ts)
    GetValues 1, sLine, v
    oHeader.IPOIDS = Val_(v(1))
    'Load Cases
    sLine = ReadLn(ts)
    GetValues 2, sLine, v
    For i = 1 To Val_(v(1))
        Dim m As New cLoadCase
        m.index = i
        oHeader.colLoadCase.Add m, i
        Set m = Nothing
    Next i
    GetValues Val_(v(2)), sLine, v
    For i = 1 To UBound(v)
        For j = 1 To oHeader.colLoadCase.Count
            If oHeader.colLoadCase.Item(j).index = Val_(v(i)) Then
                oHeader.colLoadCase.Item(j).state = IsOn
            End If
        Next j
    Next i
    For i = 1 To oHeader.colLoadCase.Count
        sLine = ReadLn(ts)
        sLine = LTrim(sLine)
        sLine = RTrim(sLine)
        oHeader.colLoadCase.Item(i).Title = sLine
    Next i
    'PANELS
    Dim oPan As cPanel
    Dim colPan As colPanel
    Set colPan = Project.Item(ProjectIndex).colPanel
    For i = 1 To oHeader.NETO
        Set oPan = New cPanel
        oPan.index = i
        oPan.pNumber = i

NextLine:
        sLine = ReadLn(ts)
        sLine = LTrim(sLine)
        Select Case Left(UCase(sLine), 5)
            Case "PLAQU"
                oPan.pType = Plate
            Case "EPONT"
                oPan.pType = Beam
            Case "COQUE"
                Err.Description = "SHELL element not implemented."
                Err.Raise 9999, , Err.Description
            Case "DCOQU"
                oPan.pType = DoubleHull
            Case Else
            GoTo NextLine
        End Select
        sLine = ReadLn(ts)
        GetValues 2, sLine, v
        oPan.cGeometry.PanelWidth = Abs(Val_(v(1)))
        oPan.cScantlings.NetThickness = Val_(v(2))
        Select Case oPan.pType
            Case Plate, DoubleHull
                sLine = ReadLn(ts)
                GetValues 5, sLine, v
                oPan.cScantlings.cPrimaryFrames.Spacing = Val_(v(1))
                oPan.cScantlings.cPrimaryStiffeners.Spacing = Val_(v(2))
                oPan.cScantlings.cSecondaryFrames.Spacing = Val_(v(3))
                oPan.cScantlings.cSecondaryStiffeners.Spacing = Val_(v(4))
                oPan.cScantlings.CorrosionThickness = Val_(v(5))
                oPan.cScantlings.GrossThickness = oPan.cScantlings.NetThickness + oPan.cScantlings.CorrosionThickness
                sLine = ReadLn(ts)
                GetValues 5, sLine, v
                oPan.cScantlings.cPrimaryFrames.WebHeight = Val_(v(1))
                oPan.cScantlings.cPrimaryFrames.WebThickness = Val_(v(2))
                oPan.cScantlings.cPrimaryFrames.FlangeWidth = Val_(v(3))
                oPan.cScantlings.cPrimaryFrames.FlangeThickness = Val_(v(4))
                oPan.cScantlings.cPrimaryFrames.CorrosionThickness = Val_(v(5))
                sLine = ReadLn(ts)
                GetValues 5, sLine, v
                oPan.cScantlings.cPrimaryStiffeners.WebHeight = Val_(v(1))
                oPan.cScantlings.cPrimaryStiffeners.WebThickness = Val_(v(2))
                oPan.cScantlings.cPrimaryStiffeners.FlangeWidth = Val_(v(3))
                oPan.cScantlings.cPrimaryStiffeners.FlangeThickness = Val_(v(4))
                oPan.cScantlings.cPrimaryStiffeners.CorrosionThickness = Val_(v(5))
                oPan.cScantlings.cSecondaryFrames.Side = SideLeft
                oPan.cScantlings.cSecondaryStiffeners.Side = SideLeft
                If oPan.cScantlings.cSecondaryFrames.Spacing > 0 Or oPan.cScantlings.cSecondaryStiffeners.Spacing > 0 Then
                    sLine = ReadLn(ts)
                    GetValues 5, sLine, v
                    oPan.cScantlings.cSecondaryFrames.WebHeight = Val_(v(1))
                    oPan.cScantlings.cSecondaryFrames.WebThickness = Val_(v(2))
                    oPan.cScantlings.cSecondaryFrames.FlangeWidth = Val_(v(3))
                    oPan.cScantlings.cSecondaryFrames.FlangeThickness = Val_(v(4))
                    oPan.cScantlings.cSecondaryFrames.Side = Val_(v(5))
                    sLine = ReadLn(ts)
                    GetValues 5, sLine, v
                    oPan.cScantlings.cSecondaryStiffeners.WebHeight = Val_(v(1))
                    oPan.cScantlings.cSecondaryStiffeners.WebThickness = Val_(v(2))
                    oPan.cScantlings.cSecondaryStiffeners.FlangeWidth = Val_(v(3))
                    oPan.cScantlings.cSecondaryStiffeners.FlangeThickness = Val_(v(4))
                    oPan.cScantlings.cSecondaryStiffeners.Side = Val_(v(5))
                End If
                sLine = ReadLn(ts)
                GetValues 1, sLine, v
                oPan.cScantlings.cPrimaryStiffeners.DistributionMode = v(1)
                If oPan.pType = DoubleHull Then
                    sLine = ReadLn(ts)
                    GetValues 1, sLine, v
                    oPan.RelatedDoubleHullPanel = Val_(v(1))
                End If
                sLine = ReadLn(ts)
                If Val_(right(sFileVer, 1)) < 4 Then
                    GetValues 3, sLine, v
                    oPan.cGeometry.Participation = Val_(v(1))
                    oPan.cGeometry.FAMI = Val_(v(2))
                    oPan.cGeometry.LOT = Val_(v(3))
                Else
                    GetValues 4, sLine, v
                    oPan.cGeometry.Participation = Val_(v(1))
                    oPan.cGeometry.PositionCode = Val_(v(2))
                    oPan.cGeometry.FAMI = Val_(v(3))
                    oPan.cGeometry.LOT = Val_(v(4))
                End If
                
                oPan.cScantlings.cPrimaryStiffeners.GrossSectionModulus = GetSectionModulus(oPan.cScantlings.cPrimaryStiffeners.WebHeight, _
                oPan.cScantlings.cPrimaryStiffeners.WebThickness + oPan.cScantlings.cPrimaryStiffeners.CorrosionThickness, _
                oPan.cScantlings.cPrimaryStiffeners.FlangeWidth, _
                oPan.cScantlings.cPrimaryStiffeners.FlangeThickness + oPan.cScantlings.cPrimaryStiffeners.CorrosionThickness, _
                oPan.cScantlings.cPrimaryStiffeners.Spacing, _
                oPan.cScantlings.cPrimaryFrames.Spacing, _
                oPan.cScantlings.GrossThickness)
                
            Case Beam
                sLine = ReadLn(ts)
                GetValues 1, sLine, v
                Select Case UCase(v(1))
                    Case "CERCLE"
                        oPan.cScantlings.BeamSection = bsCircle
                    Case "CARRE"
                        oPan.cScantlings.BeamSection = bsSquare
                    Case "DOUBLET"
                        oPan.cScantlings.BeamSection = bsDoubleT
                    Case Else
                    Err.Description = "BEAM Section " & "'" & v(1) & "'" & " not recognized."
                    Err.Raise 9999, , Err.Description
                End Select
                sLine = ReadLn(ts)
                GetValues 2, sLine, v
                oPan.cScantlings.cPrimaryFrames.Spacing = Val_(v(1))
                oPan.cGeometry.BucklingLength = Val_(v(2))
                Select Case oPan.cScantlings.BeamSection
                    Case bsCircle
                        sLine = ReadLn(ts)
                        GetValues 2, sLine, v
                        oPan.cScantlings.cPrimaryFrames.WebHeight = Val_(v(1))
                        oPan.cScantlings.cPrimaryFrames.CorrosionThickness = Val_(v(2))
                    Case bsSquare
                        sLine = ReadLn(ts)
                        GetValues 2, sLine, v
                        oPan.cScantlings.cPrimaryFrames.WebHeight = Val_(v(1))
                        oPan.cScantlings.cPrimaryFrames.CorrosionThickness = Val_(v(2))
                    Case bsDoubleT
                        sLine = ReadLn(ts)
                        GetValues 5, sLine, v
                        oPan.cScantlings.cPrimaryFrames.WebHeight = Val_(v(1)) * 2 ' to modify when Judith modifies in LBR5
                        oPan.cScantlings.cPrimaryFrames.WebThickness = Val_(v(2))  ' see ReadLBR5txtFile as well
                        oPan.cScantlings.cPrimaryFrames.FlangeWidth = Val_(v(3))
                        oPan.cScantlings.cPrimaryFrames.FlangeThickness = Val_(v(4))
                        oPan.cScantlings.cPrimaryFrames.CorrosionThickness = Val_(v(5))
                        oPan.cScantlings.NetThickness = Empty
                End Select
        End Select
        'Materials
        Dim oMat As cMaterial
        Set oMat = oPan.cMaterial
        sLine = ReadLn(ts)
        If Val_(right(sFileVer, 1)) < 4 Then
            GetValues 5, sLine, v
            oMat.YoungModulus = Val_(v(1))
            oMat.Poisson = Val_(v(2))
            oMat.YieldStress = Val_(v(3))
            oMat.AllowableStress = Val_(v(4))
            oMat.SpecificWeight = Val_(v(5))
        Else
            GetValues 5, sLine, v
            oMat.YoungModulus = Val_(v(1))
            oMat.Poisson = Val_(v(2))
            oMat.YieldStress = Val_(v(3))
            oMat.AllowableStress = Val_(v(4))
            If oHeader.IANA = 1 Then
                oMat.AllowableStress = Val_(v(4))
            ElseIf oHeader.IANA = 2 Then
                oMat.MaterialCoeff = Val_(v(4))
            End If
            oMat.SpecificWeight = Val_(v(5))
        End If
        
        Dim NoOfGirders As Integer
        sLine = ReadLn(ts)
        GetValues 7, sLine, v
        NoOfGirders = Val_(v(1))
        oPan.cScantlings.cPrimaryStiffeners.Side = Val(v(2))
        oPan.cScantlings.cPrimaryFrames.Side = Val_(v(3))
        oPan.cScantlings.GirderSide = Val_(v(4))
        If Val_(v(5)) = 0 Then v(5) = 1
        oPan.LateralPressureSide = Val_(v(5))
        oPan.IsBuoyancyForce = Val_(v(6))
        oPan.UniformLateralPressureVariation = Val_(v(7))
        sLine = ReadLn(ts)
        GetValues 1, sLine, v
        oPan.cGeometry.PanelAngle = Val_(v(1))
        
        For j = 1 To Project.Item(ProjectIndex).cHeader.colLoadCase.Count
            Set m = New cLoadCase
            oPan.colLoadCase.Add m, j
            Set m = Nothing
        Next j
        For j = 1 To oPan.colLoadCase.Count
            sLine = ReadLn(ts)
            GetValues 2, sLine, v
            oPan.colLoadCase.Item(j).index = j
            oPan.colLoadCase.Item(j).Title = Project.Item(ProjectIndex).cHeader.colLoadCase.Item(j).Title
            oPan.colLoadCase.Item(j).LateralPressureIn = Val_(v(1))
            oPan.colLoadCase.Item(j).LateralPressureOut = Val_(v(2))
        Next j
        sLine = ReadLn(ts)
        GetValues 1, sLine, v
        oPan.LocalizedPressure = Val_(v(1))
        sLine = ReadLn(ts)
        GetValues 1, sLine, v
        oPan.IsWiseLateralPressure = Val_(v(1))
        If oPan.IsWiseLateralPressure = yes Then
            For j = 1 To oPan.colLoadCase.Count
                ts.SkipLine
                sLine = ReadLn(ts)
                GetValues 1, sLine, v
                Dim iStep As Integer
                Dim oStep As cStepWiseLateralPressure
                Dim colStep As colStepWiseLateralPressure
                Set colStep = oPan.colLoadCase.Item(j).colStepWiseLateralPressure
                iStep = Val_(v(1))
                For k = 1 To iStep
                    Set oStep = New cStepWiseLateralPressure
                    sLine = ReadLn(ts)
                    GetValues 3, sLine, v
                    oStep.index = k
                    oStep.VerticalGravityLoad = Val_(v(1))
                    oStep.LateralPressureIn = Val_(v(2))
                    oStep.LateralPressureOut = Val_(v(3))
                    colStep.Add oStep, oStep.index
                    Set oStep = Nothing
                Next k
            Next j
        End If
        
        'Panel Connections
        Dim col As Collection
        sLine = ReadLn(ts)
        GetValues 10, sLine, v
        Set col = New Collection
        For j = 1 To 10
            col.Add Val_(v(j))
        Next j
        Set oPan.colConnections = col
        Set col = Nothing
        'Girders
        Dim oGirder As cGirder
        Dim colGirder As colGirder
        Set colGirder = oPan.cScantlings.colGirder
        For j = 1 To NoOfGirders
            Set oGirder = New cGirder
            oGirder.index = j
            sLine = ReadLn(ts)
            GetValues 5, sLine, v
            oGirder.WebHeight = Val_(v(1))
            oGirder.WebThickness = Val_(v(2))
            oGirder.FlangeWidth = Val_(v(3))
            oGirder.FlangeThickness = Val_(v(4))
            oGirder.Distance = Val_(v(5))
            colGirder.Add oGirder, oGirder.index
            Set oGirder = Nothing
        Next j
        'Design Variables
        ts.SkipLine
        sLine = ReadLn(ts)
        GetValues 1, sLine, v
        Dim iDes As Integer
        Dim oDes As cDesignVariables
        Dim colDes As colDesignVariables
        Set colDes = oPan.colDesignVariables
        iDes = Val_(v(1))
'        For j = 1 To 9
'            Set oDes = New cDesignVariables
'            oDes.index = j
'            oDes.Active = False
'            oDes.VariableName = j
'            colDes.Add oDes, j
'        Next j
        ''Default Design Variables
        ''------------------------
        'Dim v() As Variant
        GetScantlings oPan.cScantlings, v
        For j = 1 To 9
            Set oDes = New cDesignVariables
            oDes.index = j
            oDes.Active = False
            oDes.VariableName = j
            Select Case oHeader.colDefaultDesignVariables.Item(j).LowerLimit
                Case Is < v(j)
                    oDes.LowerLimit = oHeader.colDefaultDesignVariables.Item(j).LowerLimit
                Case Else
                    oDes.LowerLimit = v(j)
            End Select
            Select Case oHeader.colDefaultDesignVariables.Item(j).UpperLimit
                Case Is > v(j)
                    oDes.UpperLimit = oHeader.colDefaultDesignVariables.Item(j).UpperLimit
                Case Else
                    oDes.UpperLimit = v(j)
            End Select
            colDes.Add oDes, j
        Next j
        ''------------------------

        Dim listvar() As Variant
        Dim varinf() As Variant
        Dim varsup() As Variant
        Dim index_listvar As Integer
        index_listvar = 1
        Select Case Val_(right(sFileVer, 1))
            Case Is = 0
                Select Case iDes
                    Case Is = 0
                    Case Is > 0
                        sLine = ReadLn(ts)
                        GetValues iDes, sLine, listvar
                        sLine = ReadLn(ts)
                        GetValues iDes, sLine, varinf
                        sLine = ReadLn(ts)
                        GetValues iDes, sLine, varsup
                        For j = 1 To 9
                            If index_listvar > iDes Then Exit For
                            If j = Val_(listvar(index_listvar)) Then
                                colDes.Item(j).Active = True
                                colDes.Item(j).VariableName = j
                                colDes.Item(j).LowerLimit = Val_(varinf(index_listvar))
                                colDes.Item(j).UpperLimit = Val_(varsup(index_listvar))
                                index_listvar = index_listvar + 1
                            End If
                        Next j
                End Select
            Case Is >= 1
                Select Case iDes
                    Case Is = 0
                        sLine = ReadLn(ts)
                        GetValues 9, sLine, varinf
                        sLine = ReadLn(ts)
                        GetValues 9, sLine, varsup
                        For j = 1 To 9
                            colDes.Item(j).VariableName = j
                            colDes.Item(j).LowerLimit = Val_(varinf(j))
                            colDes.Item(j).UpperLimit = Val_(varsup(j))
                        Next j
                    Case Is > 0
                        sLine = ReadLn(ts)
                        GetValues iDes, sLine, listvar
                        sLine = ReadLn(ts)
                        GetValues 9, sLine, varinf
                        sLine = ReadLn(ts)
                        GetValues 9, sLine, varsup
                        For j = 1 To 9
                            colDes.Item(j).VariableName = j
                            colDes.Item(j).LowerLimit = Val_(varinf(j))
                            colDes.Item(j).UpperLimit = Val_(varsup(j))
                        Next j
                        For j = 1 To iDes
                            colDes.Item(Val_(listvar(j))).Active = True
                        Next j
                End Select
        End Select
        'Structural Constraints
        Dim oStru As cStructuralConstraints
        ts.SkipLine
        sLine = ReadLn(ts)
        GetValues 1, sLine, v
        oPan.IsStructuralConstraints = Val_(v(1))
        If oPan.IsStructuralConstraints = yes Then
            sLine = ReadLn(ts)
            Dim NoOfPoints As Integer
            GetValues 1, sLine, v
            NoOfPoints = Val_(v(1))
            sLine = ReadLn(ts)
            GetValues NoOfPoints, sLine, v
            Dim r As Double
            For j = 1 To NoOfPoints
                Dim assp As New cAssessmentPoint
                assp.index = j
                assp.Value = Val_(v(j))
                oPan.colAssessmentPoints.Add assp, j
                Set assp = Nothing
            Next j
            For j = 1 To oPan.colLoadCase.Count
                ts.SkipLine
                sLine = ReadLn(ts)
                GetValues 1, sLine, v
                Dim NoOfStrRestr As Integer
                NoOfStrRestr = Val_(v(1))
                If NoOfStrRestr < 0 Then
                    NoOfStrRestr = -NoOfStrRestr
                    For k = 1 To oPan.colLoadCase.Item(NoOfStrRestr).colStructuralConstraints.Count
                        Set oStru = New cStructuralConstraints
                        oStru.index = oPan.colLoadCase.Item(NoOfStrRestr).colStructuralConstraints.Item(k).index
                        oStru.Reference = oPan.colLoadCase.Item(NoOfStrRestr).colStructuralConstraints.Item(k).Reference
                        oStru.Value = oPan.colLoadCase.Item(NoOfStrRestr).colStructuralConstraints.Item(k).Value
                        oStru.Limit = oPan.colLoadCase.Item(NoOfStrRestr).colStructuralConstraints.Item(k).Limit
                        oStru.AssesmentPoint = oPan.colLoadCase.Item(NoOfStrRestr).colStructuralConstraints.Item(k).AssesmentPoint
                        oPan.colLoadCase.Item(j).colStructuralConstraints.Add oStru, k
                        Set oStru = Nothing
                    Next k
                    GoTo NextLoadCase
                End If
                For k = 1 To NoOfStrRestr
                    Set oStru = New cStructuralConstraints
                    sLine = ReadLn(ts)
                    GetValues 5, sLine, v
                    oStru.index = Val_(v(1))
                    oStru.Reference = Val_(v(2))
                    oStru.Value = Val_(v(3))
                    oStru.Limit = Val_(v(4))
                    oStru.AssesmentPoint = Val_(v(5))
                    oPan.colLoadCase.Item(j).colStructuralConstraints.Add oStru, k
                    Set oStru = Nothing
                Next k
NextLoadCase:
            Next j
        End If
        'Geometrical Constraints
        Dim oGeo As cGeometricalConstraints
        ts.SkipLine
        sLine = ReadLn(ts)
        GetValues 1, sLine, v
        oPan.NoOfGeomConstr = Val_(v(1))
        Select Case oPan.NoOfGeomConstr
            Case 0
            Case 99
                Set oGeo = New cGeometricalConstraints
                sLine = ReadLn(ts)
                GetValues 1, sLine, v
                oGeo.index = 1
                oGeo.code = Val_(v(1))
                oPan.colGeometricalConstraints.Add oGeo, 1
                sLine = ReadLn(ts)
                GetValues 2, sLine, v
                oPan.FramesFlangeThicknessUpdate = Val_(v(1))
                oPan.StiffenersFlangeThicknessUpdate = Val_(v(2))
                Set oGeo = Nothing
            Case Else
                sLine = ReadLn(ts)
                GetValues oPan.NoOfGeomConstr, sLine, v
                For j = 1 To oPan.NoOfGeomConstr
                    Set oGeo = New cGeometricalConstraints
                    oGeo.index = j
                    oGeo.code = Val_(v(j))
                    oPan.colGeometricalConstraints.Add oGeo, j
                    Set oGeo = Nothing
                Next j
                sLine = ReadLn(ts)
                GetValues 2, sLine, v
                oPan.FramesFlangeThicknessUpdate = Val_(v(1))
                oPan.StiffenersFlangeThicknessUpdate = Val_(v(2))
        End Select
        colPan.Add oPan, oPan.index
        Set oPan = Nothing
    Next i
   
    'GLOBAL DATA
    'Boundary Conditions
    Dim oBound As cBoundaryConditions
    ts.SkipLine
    sLine = ReadLn(ts)
    GetValues 1, sLine, v
    Dim NoOfBoundaryCond As Integer
    NoOfBoundaryCond = Val_(v(1))
    For i = 1 To NoOfBoundaryCond
        sLine = ReadLn(ts)
        GetValues 2, sLine, v
        Select Case Project.Item(ProjectIndex).colPanel.Count
            Case 1
                Set oBound = New cBoundaryConditions
                oBound.index = i
                oBound.BoundaryCondition = Val_(v(2))
                If i = 1 Then
                    oBound.Edge = InEdge
                ElseIf i = 2 Then
                    oBound.Edge = OutEdge
                End If
                Project.Item(ProjectIndex).colPanel.Item(1).colBoundaryConditions.Add oBound, oBound.index
                Set oBound = Nothing
            Case Is > 1
                Set oBound = New cBoundaryConditions
                oBound.index = 1
                oBound.BoundaryCondition = Val_(v(2))
                Project.Item(ProjectIndex).colPanel.Item(Val_(v(1))).colBoundaryConditions.Add oBound, oBound.index
                Set oBound = Nothing
        End Select
    Next i
    'Gravity Center
    ts.SkipLine
    sLine = ReadLn(ts)
    GetValues 2, sLine, v
    oHeader.YAxisOrigin = Val_(v(1))
    oHeader.ZAxisOrigin = Val_(v(2))
    Dim oGlob As cGlobalConstraints
    Set oGlob = oHeader.cGlobalConstraints
    'Global Restrictions
    'Gravity Center
    sLine = ReadLn(ts)
    GetValues 3, sLine, v
    oGlob.GravityLimitRestriction = Val_(v(1))
    oGlob.MinGravityCenter = Val_(v(2))
    oGlob.MaxGravityCenter = Val_(v(3))
    'Inertia Constraint
    ts.SkipLine
    sLine = ReadLn(ts)
    GetValues 2, sLine, v
    oGlob.IsInertia = Val_(v(1))
    oGlob.Inertia = Val_(v(2))
    'Section Modulus Constraint
    ts.SkipLine
    sLine = ReadLn(ts)
    GetValues 3, sLine, v
    oGlob.IsSectionModulus = Val_(v(1))
    oGlob.SectionModulus = Val_(v(2))
    oGlob.ZPanel = Val_(v(3))
    'Weight Constraint
    ts.SkipLine
    sLine = ReadLn(ts)
    GetValues 2, sLine, v
    oGlob.IsWeight = Val_(v(1))
    oGlob.Weight = Val_(v(2))
    'Cost Constraint
    ts.SkipLine
    sLine = ReadLn(ts)
    GetValues 3, sLine, v
    oGlob.IsCost = Val_(v(1))
    oGlob.Cost = Val_(v(2))
    oGlob.CostType = Val_(v(3))
    'Bending Moments
    ts.SkipLine
    sLine = ReadLn(ts)
    GetValues 1, sLine, v
    oHeader.IsBendingMoments = Val_(v(1))
    Select Case oHeader.IsBendingMoments
        Case no
        Case yes
            sLine = ReadLn(ts)
            GetValues 1, sLine, v
            oHeader.YRED = Val_(v(1))
            For i = 1 To oHeader.colLoadCase.Count
                sLine = ReadLn(ts)
                GetValues 4, sLine, v
                oHeader.colLoadCase.Item(i).VerticalBendingMomentFore = Round(Val_(v(1)), 6)
                'oHeader.colLoadCase.Item(i).VerticalBendingMomentAft = Round(Val_(v(2)), 6)
                oHeader.colLoadCase.Item(i).VerticalShear = Round(Val_(v(2)), 6)
                oHeader.colLoadCase.Item(i).HorizontalBendingMomentFore = Round(Val_(v(3)), 6)
                'oHeader.colLoadCase.Item(i).HorizontalBendingMomentAft = Val_(v(4))
                oHeader.colLoadCase.Item(i).HorizontalShear = Round(Val_(v(4)), 6)
            Next i
    End Select
    'Copy bending moments in panel's load cases collections
    For i = 1 To oHeader.NETO
        For j = 1 To oHeader.colLoadCase.Count
            Project.Item(ProjectIndex).colPanel.Item(i).colLoadCase.Item(j).VerticalBendingMomentFore = oHeader.colLoadCase.Item(j).VerticalBendingMomentFore
            'Project.Item(ProjectIndex).colPanel.Item(i).colLoadCase.Item(j).VerticalBendingMomentAft = oHeader.colLoadCase.Item(j).VerticalBendingMomentAft
            Project.Item(ProjectIndex).colPanel.Item(i).colLoadCase.Item(j).VerticalShear = oHeader.colLoadCase.Item(j).VerticalShear
            Project.Item(ProjectIndex).colPanel.Item(i).colLoadCase.Item(j).HorizontalBendingMomentFore = oHeader.colLoadCase.Item(j).HorizontalBendingMomentFore
            'Project.Item(ProjectIndex).colPanel.Item(i).colLoadCase.Item(j).HorizontalBendingMomentAft = oHeader.colLoadCase.Item(j).HorizontalBendingMomentAft
            Project.Item(ProjectIndex).colPanel.Item(i).colLoadCase.Item(j).HorizontalShear = oHeader.colLoadCase.Item(j).HorizontalShear
        Next j
    Next i
    'Set Me.colLoadCase = Nothing
    ' Equality constraints
    ts.SkipLine
    sLine = ReadLn(ts)
    GetValues 1, sLine, v
    Dim NoOfEqRestr As Integer
    Dim oEq As cEqualityRestrictions
    NoOfEqRestr = Val_(v(1))
    For i = 1 To NoOfEqRestr
        Set oEq = New cEqualityRestrictions
        oEq.index = i
        'Me.colEqualityRestrictions.Add oEq, i
        'Me.colEqualityRestrictions.Item(i).ReadLBR5txtFile ts
        sLine = ReadLn(ts)
        GetValues 5, sLine, v
        oEq.DependingDesignVariable = Val_(v(1))
        oEq.DependingPanel = Val_(v(2))
        oEq.LeadingDesignVariable = Val_(v(3))
        oEq.LeadingPanel = Val_(v(4))
        oEq.Ratio = Val_(v(5))
        oHeader.colEqualityRestrictions.Add oEq, oEq.index
        Set oEq = Nothing
    Next i
    'Ultimate strenght
    ts.SkipLine
    sLine = ReadLn(ts)
    GetValues 1, sLine, v
    oHeader.IRESTR = Val_(v(1))
    sLine = ReadLn(ts)
    GetValues 1, sLine, v
    oHeader.IULT = Val_(v(1))
          
    Call NodeGenerator
    UpdateBoundary ProjectIndex
    Dim cPanel As cPanel
    For Each cPanel In Project.Item(ProjectIndex).colPanel
        cPanel.cCostCAtMain.SetFirstPanelData cPanel.pNumber, ProjectIndex
    Next cPanel
    ComputeFirstFractionnements ProjectIndex

    Exit Function
OpenTxtVer11Err:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modFileOpenTxt: Function OpenTxtVer11")
    ts.Close
End Function


