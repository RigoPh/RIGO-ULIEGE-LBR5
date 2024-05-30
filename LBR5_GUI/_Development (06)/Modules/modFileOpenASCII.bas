Attribute VB_Name = "modFileOpenASCII"
Option Explicit
Dim ProjectIndex As Integer
Dim sFileVer As String, sSubVer As String

Public Function OpenAsciiFile(ByVal iNoOfFile As Integer, ByVal index As Integer)
    On Error GoTo OpenAsciiFileErr
    ProjectIndex = index
    Dim ss As String * 100
    Get #iNoOfFile, , ss
    sFileVer = Trim(ss)
    sSubVer = right(sFileVer, 1)
    Project.Item(ProjectIndex).FileVersionNumber = sFileVer
    Select Case sFileVer
        Case "verLBR5.7(24/03/04)" 'before advanced version management
            OpenASCIIVer10 iNoOfFile
        Case Else
            Select Case Left(sFileVer, 3)
                Case "1.1" '1.1.0 ... 1.1.9
                    OpenASCIIVer11 iNoOfFile
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
OpenAsciiFileErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modFileOpenASCII: Function OpenAsciiFile")
End Function

'version 1.0.0 (verLBR5.7(24/03/04))
Public Function OpenASCIIVer10(ByVal iNoOfFile As Integer)
    On Error GoTo OpenASCIIVer10Err
    Dim i As Integer, j As Integer, k As Integer
    Dim ii As Integer, dd As Double, ss As String * 100, BB As Boolean
    'HEADER
    Dim oHeader As cHeader
    Set oHeader = Project.Item(ProjectIndex).cHeader
    With oHeader
        oHeader.IANA = 1
        Get #iNoOfFile, , ss: .Title = Trim(ss)
        Get #iNoOfFile, , ii: .IMPR = ii
        Get #iNoOfFile, , ii: .IMPR2 = ii
        Get #iNoOfFile, , ii: .INDAIG = ii
        Get #iNoOfFile, , ii: .INDRAID = ii
        Get #iNoOfFile, , ii: .DESSIN = ii
        Get #iNoOfFile, , ii: .JLPH = ii
        Get #iNoOfFile, , ii: .JLBORD = ii
        Get #iNoOfFile, , ii: .NETO = ii
        Get #iNoOfFile, , ii: .IOPTI = ii
        Get #iNoOfFile, , ii: .ITERAM = ii
        Get #iNoOfFile, , ii: .ICOUT = ii
        Get #iNoOfFile, , dd: .Width = dd
        Get #iNoOfFile, , dd: .DIS1 = dd
        Get #iNoOfFile, , dd: .DIS2 = dd
        Get #iNoOfFile, , dd: .DIS3 = dd
        Get #iNoOfFile, , dd: .DIS4 = dd
        Get #iNoOfFile, , dd: .DIS5 = dd
        Get #iNoOfFile, , dd: .FAM1 = dd
        Get #iNoOfFile, , dd: .FAM2 = dd
        Get #iNoOfFile, , dd: .FAM3 = dd
        Get #iNoOfFile, , dd: .FAM4 = dd
        Get #iNoOfFile, , dd: .FAM5 = dd
        Get #iNoOfFile, , dd: .FAM6 = dd
        Get #iNoOfFile, , ii: .IPOIDS = ii
    End With
        'Cost Data
    With oHeader.cCostData
        Get #iNoOfFile, , dd: .REND = dd
        Get #iNoOfFile, , dd: .EQP = dd
        Get #iNoOfFile, , dd: .E0 = dd
        Get #iNoOfFile, , dd: .E0X = dd
        Get #iNoOfFile, , dd: .E0Y = dd
        Get #iNoOfFile, , dd: .C1 = dd
        Get #iNoOfFile, , dd: .C2 = dd
        Get #iNoOfFile, , dd: .C3 = dd
        Get #iNoOfFile, , dd: .DC1 = dd
        Get #iNoOfFile, , dd: .DW2 = dd
        Get #iNoOfFile, , dd: .DW3 = dd
        Get #iNoOfFile, , dd: .P10 = dd
        Get #iNoOfFile, , dd: .DP10 = dd
        Get #iNoOfFile, , dd: .p4 = dd
        Get #iNoOfFile, , dd: .P5 = dd
        Get #iNoOfFile, , dd: .DP4 = dd
        Get #iNoOfFile, , dd: .DP5 = dd
        Get #iNoOfFile, , dd: .P9X = dd
        Get #iNoOfFile, , dd: .P9Y = dd
        Get #iNoOfFile, , dd: .DP9X = dd
        Get #iNoOfFile, , dd: .DP9Y = dd
        Get #iNoOfFile, , dd: .P6 = dd
        Get #iNoOfFile, , dd: .P7 = dd
        Get #iNoOfFile, , dd: .BETA_X = dd
        Get #iNoOfFile, , dd: .BETA_Y = dd
        Get #iNoOfFile, , dd: .C8 = dd
        Get #iNoOfFile, , dd: .DC8 = dd
        Get #iNoOfFile, , dd: .ALPHA_X = dd
        Get #iNoOfFile, , dd: .ALPHA_Y = dd
    End With
    'Load Cases
    Dim iLoadCase As Integer
    Dim oLoadCase As cLoadCase
    Dim colLoadCase As colLoadCase
    Set colLoadCase = Project.Item(ProjectIndex).cHeader.colLoadCase
    Get #iNoOfFile, , ii
    iLoadCase = ii
    For i = 1 To iLoadCase
        Set oLoadCase = New cLoadCase
        With oLoadCase
            Get #iNoOfFile, , ii: .index = ii
            Get #iNoOfFile, , ss: .Title = Trim(ss)
            Get #iNoOfFile, , ii: .state = ii
            Get #iNoOfFile, , dd: .LateralPressureIn = dd
            Get #iNoOfFile, , dd: .LateralPressureOut = dd
            Get #iNoOfFile, , dd: .VerticalBendingMomentFore = dd
            'Get #iNoOfFile, , dd: .VerticalBendingMomentAft = dd
            Get #iNoOfFile, , dd: .VerticalShear = dd
            Get #iNoOfFile, , dd: .HorizontalBendingMomentFore = dd
            'Get #iNoOfFile, , dd: .HorizontalBendingMomentAft = dd
            Get #iNoOfFile, , dd: .HorizontalShear = dd
        End With
        colLoadCase.Add oLoadCase, i
        Set oLoadCase = Nothing
        Get #iNoOfFile, , ii 'Stepwise lateral pressures
        Get #iNoOfFile, , ii 'Structural constraints
    Next i
    'Global Data
    'Origin
    With oHeader
        Get #iNoOfFile, , dd: .YAxisOrigin = dd
        Get #iNoOfFile, , dd: .ZAxisOrigin = dd
    End With
    'Gravity Center Constraints
    With oHeader.cGlobalConstraints
        Get #iNoOfFile, , ii: .GravityLimitRestriction = ii
        Get #iNoOfFile, , dd: .MinGravityCenter = dd
        Get #iNoOfFile, , dd: .MaxGravityCenter = dd
    End With
    '
    With oHeader
        Get #iNoOfFile, , dd: .YRED = dd
        Get #iNoOfFile, , ii: .IsBendingMoments = ii
    End With
    'Equality Restrictions
    Dim iEq As Integer
    Dim oEq As cEqualityRestrictions
    Dim colEq As colEqualityRestrictions
    Set colEq = oHeader.colEqualityRestrictions
    Get #iNoOfFile, , ii
    iEq = ii
    For i = 1 To iEq
        Set oEq = New cEqualityRestrictions
        Get #iNoOfFile, , ii: oEq.index = ii
        Get #iNoOfFile, , ii: oEq.LeadingPanel = ii
        Get #iNoOfFile, , ii: oEq.DependingPanel = ii
        Get #iNoOfFile, , ii: oEq.DependingDesignVariable = ii
        Get #iNoOfFile, , ii: oEq.LeadingDesignVariable = ii
        Get #iNoOfFile, , dd: oEq.Ratio = dd
        colEq.Add oEq, i
        Set oEq = Nothing
    Next i
    With oHeader
        Get #iNoOfFile, , ii: .IRESTR = ii
        Get #iNoOfFile, , ii: .IULT = ii
        Get #iNoOfFile, , ii: .NoOfDoubleHulls = ii
    End With
    'Nodes
    Dim iNode As Integer
    Dim oNode As cNode
    Dim colNode As colNodes
    Set colNode = Project.Item(ProjectIndex).colNodes
    Get #iNoOfFile, , ii
    iNode = ii
    For i = 1 To ii
        Set oNode = New cNode
        Get #iNoOfFile, , ii: oNode.index = ii
        Get #iNoOfFile, , ii: oNode.nNumber = ii
        Get #iNoOfFile, , dd: oNode.y = dd
        Get #iNoOfFile, , dd: oNode.z = dd
        colNode.Add oNode, i
        Set oNode = Nothing
    Next i
    'PANELS
    Dim iPan As Integer
    Dim oPan As cPanel
    Dim colPan As colPanel
    Set colPan = Project.Item(ProjectIndex).colPanel
    'iPan = Project.Item(ProjectIndex).cHeader.NETO
    Get #iNoOfFile, , ii
    iPan = ii
    For i = 1 To iPan
        Set oPan = New cPanel
        Get #iNoOfFile, , ii: oPan.index = ii
        Get #iNoOfFile, , ss: oPan.pLabel = Trim(ss)
        Get #iNoOfFile, , ii: oPan.pType = ii
        Get #iNoOfFile, , ii: oPan.RelatedDoubleHullPanel = ii
        Get #iNoOfFile, , ii: oPan.pNumber = ii
        'Geometry
        With oPan.cGeometry
            Get #iNoOfFile, , dd: .Q = dd
            Get #iNoOfFile, , dd: .PHIL = dd
            Get #iNoOfFile, , ii: .InNode = ii
            Get #iNoOfFile, , ii: .OutNode = ii
            Get #iNoOfFile, , dd: .PanelWidth = dd
            Get #iNoOfFile, , dd: .PanelAngle = dd
            Get #iNoOfFile, , dd: .Participation = dd
            Get #iNoOfFile, , dd: .BucklingLength = dd
        End With
        'Scantlings
        With oPan.cScantlings.cPrimaryFrames
            Get #iNoOfFile, , dd: .Spacing = dd
            Get #iNoOfFile, , dd: .WebHeight = dd
            Get #iNoOfFile, , dd: .WebThickness = dd
            Get #iNoOfFile, , dd: .FlangeWidth = dd
            Get #iNoOfFile, , dd: .FlangeThickness = dd
            Get #iNoOfFile, , dd: .CorrosionThickness = dd
            Get #iNoOfFile, , ii: .Side = ii
        End With
        Dim ss2 As String * 3
        With oPan.cScantlings.cPrimaryStiffeners
            Get #iNoOfFile, , dd: .Spacing = dd
            Get #iNoOfFile, , dd: .WebHeight = dd
            Get #iNoOfFile, , dd: .WebThickness = dd
            Get #iNoOfFile, , dd: .FlangeWidth = dd
            Get #iNoOfFile, , dd: .FlangeThickness = dd
            Get #iNoOfFile, , dd: .CorrosionThickness = dd
            Get #iNoOfFile, , ss2: .DistributionMode = ss2
            Get #iNoOfFile, , ii: .Side = ii
        End With
        With oPan.cScantlings.cSecondaryFrames
            Get #iNoOfFile, , dd: .Spacing = dd
            Get #iNoOfFile, , dd: .WebHeight = dd
            Get #iNoOfFile, , dd: .WebThickness = dd
            Get #iNoOfFile, , dd: .FlangeWidth = dd
            Get #iNoOfFile, , dd: .FlangeThickness = dd
            Get #iNoOfFile, , dd: .CorrosionThickness = dd
            Get #iNoOfFile, , ii: .Side = ii
        End With
        With oPan.cScantlings.cSecondaryStiffeners
            Get #iNoOfFile, , dd: .Spacing = dd
            Get #iNoOfFile, , dd: .WebHeight = dd
            Get #iNoOfFile, , dd: .WebThickness = dd
            Get #iNoOfFile, , dd: .FlangeWidth = dd
            Get #iNoOfFile, , dd: .FlangeThickness = dd
            Get #iNoOfFile, , ii: .Side = ii
        End With
        Dim iGirder As Integer
        Dim oGirder As cGirder
        Dim colGirder As colGirder
        Set colGirder = oPan.cScantlings.colGirder
        Get #iNoOfFile, , ii
        iGirder = ii
        For j = 1 To iGirder
            Set oGirder = New cGirder
            Get #iNoOfFile, , ii: oGirder.index = ii
            Get #iNoOfFile, , dd: oGirder.FlangeThickness = dd
            Get #iNoOfFile, , dd: oGirder.FlangeWidth = dd
            Get #iNoOfFile, , dd: oGirder.WebHeight = dd
            Get #iNoOfFile, , dd: oGirder.WebThickness = dd
            Get #iNoOfFile, , dd: oGirder.Distance = dd
            colGirder.Add oGirder, j
            Set oGirder = Nothing
        Next j
        With oPan.cScantlings
            Get #iNoOfFile, , ii: .GirderSide = ii
            Get #iNoOfFile, , dd: .NetThickness = dd
            Get #iNoOfFile, , dd: .CorrosionThickness = dd
            Get #iNoOfFile, , dd: .GrossThickness = dd
            Get #iNoOfFile, , ii: .BeamSection = ii
        End With
        'Materials
        With oPan.cMaterial
            Get #iNoOfFile, , ii: .index = ii
            Get #iNoOfFile, , ii: .mNumber = ii
            Get #iNoOfFile, , ss: .Name = ss
            Get #iNoOfFile, , dd: .YoungModulus = dd
            Get #iNoOfFile, , dd: .Poisson = dd
            Get #iNoOfFile, , dd: .YieldStress = dd
            Get #iNoOfFile, , dd: .AllowableStress = dd
            Get #iNoOfFile, , dd: .SpecificWeight = dd
        End With
        With oPan
            Get #iNoOfFile, , ii: .IsBuoyancyForce = ii
            Get #iNoOfFile, , dd: .LocalizedPressure = dd
            Get #iNoOfFile, , ii: .UniformLateralPressureVariation = ii
            Get #iNoOfFile, , ii: .IsWiseLateralPressure = ii
            Get #iNoOfFile, , ii: .IsStructuralConstraints = ii
            Get #iNoOfFile, , ii: .IsDesignVariables = ii
            Get #iNoOfFile, , ii: .LeftCompartment = ii
            Get #iNoOfFile, , ii: .RightCompartment = ii
            Get #iNoOfFile, , ii: .LateralPressureSide = ii
            Get #iNoOfFile, , ii: .NoOfGeomConstr = ii
            Get #iNoOfFile, , ii: .FramesFlangeThicknessUpdate = ii
            Get #iNoOfFile, , ii: .StiffenersFlangeThicknessUpdate = ii
        End With
        'Design Variables
        Dim iDes As Integer
        Dim oDes As cDesignVariables
        Dim colDes As colDesignVariables
        Set colDes = oPan.colDesignVariables
        Get #iNoOfFile, , ii
        iDes = ii
        
        ''Default Design Variables
        ''------------------------
        Dim v() As Variant
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
        
        For j = 1 To iDes
            Get #iNoOfFile, , ii: 'oDes.Index = ii
            Get #iNoOfFile, , ii: k = ii
            colDes.Item(k).Active = True
            Get #iNoOfFile, , dd: colDes.Item(k).UpperLimit = dd
            Get #iNoOfFile, , dd: colDes.Item(k).LowerLimit = dd
        Next j
        'Load Cases
        Set colLoadCase = oPan.colLoadCase
        Get #iNoOfFile, , ii
        iLoadCase = ii
        For j = 1 To iLoadCase
            Set oLoadCase = New cLoadCase
            Get #iNoOfFile, , ii: oLoadCase.index = ii
            Get #iNoOfFile, , ss: oLoadCase.Title = Trim(ss)
            Get #iNoOfFile, , ii: oLoadCase.state = ii
            Get #iNoOfFile, , dd: oLoadCase.LateralPressureIn = dd
            Get #iNoOfFile, , dd: oLoadCase.LateralPressureOut = dd
            Get #iNoOfFile, , dd: oLoadCase.VerticalBendingMomentFore = dd
            Get #iNoOfFile, , dd: oLoadCase.VerticalBendingMomentAft = dd
            Get #iNoOfFile, , dd: oLoadCase.HorizontalBendingMomentFore = dd
            Get #iNoOfFile, , dd: oLoadCase.HorizontalBendingMomentAft = dd
            'Stepwise Pressures
            Dim iStep As Integer
            Dim oStep As cStepWiseLateralPressure
            Dim colStep As colStepWiseLateralPressure
            Set colStep = oLoadCase.colStepWiseLateralPressure
            Get #iNoOfFile, , ii
            iStep = ii
            For k = 1 To iStep
                Set oStep = New cStepWiseLateralPressure
                Get #iNoOfFile, , ii: oStep.index = ii
                Get #iNoOfFile, , dd: oStep.VerticalGravityLoad = dd
                Get #iNoOfFile, , dd: oStep.LateralPressureIn = dd
                Get #iNoOfFile, , dd: oStep.LateralPressureOut = dd
                colStep.Add oStep, k
                Set oStep = Nothing
            Next k
            'Structural Constraints
            Dim iStru As Integer
            Dim oStru As cStructuralConstraints
            Dim colStru As colStructuralConstraints
            Set colStru = oLoadCase.colStructuralConstraints
            Get #iNoOfFile, , ii
            iStru = ii
            For k = 1 To iStru
                Set oStru = New cStructuralConstraints
                Get #iNoOfFile, , ii: oStru.index = ii
                Get #iNoOfFile, , ii: oStru.Reference = ii
                Get #iNoOfFile, , dd: oStru.Value = dd
                Get #iNoOfFile, , ii: oStru.Limit = ii
                Get #iNoOfFile, , ii: oStru.AssesmentPoint = ii
                Get #iNoOfFile, , dd: oStru.Point = dd
                colStru.Add oStru, k
                Set oStru = Nothing
            Next k
            colLoadCase.Add oLoadCase, j
            Set oLoadCase = Nothing
        Next j
        'Assessment Points (for Structural Constraints)
        Dim iAss As Integer
        Dim oAss As cAssessmentPoint
        Dim colAss As colAssessmentPoints
        Set colAss = oPan.colAssessmentPoints
        Get #iNoOfFile, , ii
        iAss = ii
        For j = 1 To iAss
            Set oAss = New cAssessmentPoint
            Get #iNoOfFile, , ii: oAss.index = ii
            Get #iNoOfFile, , dd: oAss.Value = dd
            colAss.Add oAss, j
            Set oAss = Nothing
        Next j
        'Geometrical Constraints
        Dim iGeo As Integer
        Dim oGeo As cGeometricalConstraints
        Dim colGeo As colGeometricalConstraints
        Set colGeo = oPan.colGeometricalConstraints
        Get #iNoOfFile, , ii
        iGeo = ii
        Select Case iGeo
            Case 0
            Case Is = 99
                Set oGeo = New cGeometricalConstraints
                Get #iNoOfFile, , ii: oGeo.index = ii
                Get #iNoOfFile, , ii: oGeo.code = ii
                colGeo.Add oGeo, oGeo.index
                Set oGeo = Nothing
            Case Is > 0
                For j = 1 To iGeo
                Set oGeo = New cGeometricalConstraints
                Get #iNoOfFile, , ii: oGeo.index = ii
                Get #iNoOfFile, , ii: oGeo.code = ii
                colGeo.Add oGeo, oGeo.index
                Set oGeo = Nothing
                Next j
        End Select
        'Boundary Conditions
        Dim iBound As Integer
        Dim oBound As cBoundaryConditions
        Dim colBound As colBoundaryConditions
        Set colBound = oPan.colBoundaryConditions
        Get #iNoOfFile, , ii
        iBound = ii
        For k = 1 To iBound
            Set oBound = New cBoundaryConditions
            Get #iNoOfFile, , ii: oBound.index = ii
            Get #iNoOfFile, , ii: oBound.BoundaryCondition = ii
            Get #iNoOfFile, , ii: oBound.Edge = ii
            colBound.Add oBound, oBound.index
            Set oBound = Nothing
        Next k
        colPan.Add oPan, i
        Set oPan = Nothing
    Next i
    'SOLUTION
    Dim vv() As Variant
    Dim oSol As cSolution
    Set oSol = Project.Item(ProjectIndex).cSolution
    With oSol
        Get #iNoOfFile, , BB: .IsSolution = BB
        If .IsSolution = True Then
            SolMatrix ProjectIndex
            Get #iNoOfFile, , POL: .Solution = POL
        End If
    End With
    'COMPARTMENTS
    Dim iComp As Integer
    Dim oComp As cCompartment
    Dim colComp As colCompartments
    Set colComp = Project.Item(ProjectIndex).colCompartments
    Get #iNoOfFile, , ii
    iComp = ii
    For i = 1 To iComp
        Set oComp = New cCompartment
        With oComp
            'General
            Get #iNoOfFile, , ii: .index = ii
            Get #iNoOfFile, , ss: .Name = Trim(ss)
            Get #iNoOfFile, , ii: .MainDestination = ii
            Get #iNoOfFile, , ii: .CompartmentType = ii
            Get #iNoOfFile, , ii: .CompartmentLoad = ii
            'Dimensions
            Get #iNoOfFile, , dd: .Length = dd
            Get #iNoOfFile, , dd: .Breadth = dd
            Get #iNoOfFile, , dd: .Height = dd
            Get #iNoOfFile, , dd: .XStartFromAPP = dd
        End With
        colComp.Add oComp, oComp.index
        Set oComp = Nothing
    Next i
    UpdatePanelConnections ProjectIndex

    'tmp
    Dim cPanel As cPanel
    For Each cPanel In Project.Item(ProjectIndex).colPanel
        cPanel.cCostCAtMain.SetFirstPanelData cPanel.pNumber, ProjectIndex
    Next cPanel
    ComputeFirstFractionnements ProjectIndex

    Exit Function
OpenASCIIVer10Err:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modFileOpenASCII: Function OpenASCIIVer10")
    Close #iNoOfFile
End Function

'version 1.1.0 (verLBR5.7(24/03/04))
Public Function OpenASCIIVer11(ByVal iNoOfFile As Integer)
    '==============================
    'version 1.1.0 [...]
    'version 1.1.1
        '- changement dans les variables de conception (proprieté Active = false, true)
        '- les coefficients d'attelier et d'accessibilité ne sont plus definis par panneau
        'mais dans cHeader.cCostCAtMain (globalement)
    '==============================
    On Error GoTo OpenASCIIVer11Err
    Dim i As Integer, j As Integer, k As Integer
    Dim ii As Integer, dd As Double, ss As String * 100, _
    LL As Long, BB As Boolean
    'HEADER
    Dim oHeader As cHeader
    Set oHeader = Project.Item(ProjectIndex).cHeader
    With oHeader
        If Val_(right(sFileVer, 1)) >= 2 Then
            Get #iNoOfFile, , ii: .IANA = ii
            
        Else
            .IANA = 1
        End If
        'verif iana licensing
        If Licensing.IS_LBR4 = False Then Project.Item(ActiveProject).cHeader.IANA = 2
        If Licensing.IS_BEAM_THEORY = False Then Project.Item(ActiveProject).cHeader.IANA = 1
        
        Get #iNoOfFile, , ss: .Title = Trim(ss)
        Get #iNoOfFile, , ii: .IMPR = ii
        Get #iNoOfFile, , ii: .IMPR2 = ii
        Get #iNoOfFile, , ii: .INDAIG = ii
        Get #iNoOfFile, , ii: .INDRAID = ii
        Get #iNoOfFile, , ii: .DESSIN = ii
        Get #iNoOfFile, , ii: .JLPH = ii
        Get #iNoOfFile, , ii: .JLBORD = ii
        Get #iNoOfFile, , ii: .NETO = ii
        Get #iNoOfFile, , ii: .IOPTI = ii
        Get #iNoOfFile, , ii: .ITERAM = ii
        Get #iNoOfFile, , ii: .cMultiOpti.IMULTI = CInt(ii)
        Get #iNoOfFile, , dd: .cMultiOpti.RHO = 1 '.cMultiOpti.RHO = dd
        Get #iNoOfFile, , dd: .cMultiOpti.W1 = dd
        Get #iNoOfFile, , dd: .cMultiOpti.W2 = dd
        Get #iNoOfFile, , dd: .cMultiOpti.W3 = dd
        Get #iNoOfFile, , ii: .ICOUT = ii
        Get #iNoOfFile, , dd: .Width = dd
        If Val_(right(sFileVer, 1)) >= 4 Then
            Get #iNoOfFile, , dd: .LongRegl = dd
        End If
        Get #iNoOfFile, , dd: .DIS1 = dd
        Get #iNoOfFile, , dd: .DIS2 = dd
        Get #iNoOfFile, , dd: .DIS3 = dd
        Get #iNoOfFile, , dd: .DIS4 = dd
        Get #iNoOfFile, , dd: .DIS5 = dd
        Get #iNoOfFile, , dd: .FAM1 = dd
        Get #iNoOfFile, , dd: .FAM2 = dd
        Get #iNoOfFile, , dd: .FAM3 = dd
        Get #iNoOfFile, , dd: .FAM4 = dd
        Get #iNoOfFile, , dd: .FAM5 = dd
        Get #iNoOfFile, , dd: .FAM6 = dd
        Get #iNoOfFile, , ii: .IPOIDS = ii
    End With
    'Cost Data
    With oHeader.cCostData
        Get #iNoOfFile, , dd: .REND = dd
        Get #iNoOfFile, , dd: .EQP = dd
        Get #iNoOfFile, , dd: .E0 = dd
        Get #iNoOfFile, , dd: .E0X = dd
        Get #iNoOfFile, , dd: .E0Y = dd
        Get #iNoOfFile, , dd: .C1 = dd
        Get #iNoOfFile, , dd: .C2 = dd
        Get #iNoOfFile, , dd: .C3 = dd
        Get #iNoOfFile, , dd: .DC1 = dd
        Get #iNoOfFile, , dd: .DW2 = dd
        Get #iNoOfFile, , dd: .DW3 = dd
        Get #iNoOfFile, , dd: .P10 = dd
        Get #iNoOfFile, , dd: .DP10 = dd
        Get #iNoOfFile, , dd: .p4 = dd
        Get #iNoOfFile, , dd: .P5 = dd
        Get #iNoOfFile, , dd: .DP4 = dd
        Get #iNoOfFile, , dd: .DP5 = dd
        Get #iNoOfFile, , dd: .P9X = dd
        Get #iNoOfFile, , dd: .P9Y = dd
        Get #iNoOfFile, , dd: .DP9X = dd
        Get #iNoOfFile, , dd: .DP9Y = dd
        Get #iNoOfFile, , dd: .P6 = dd
        Get #iNoOfFile, , dd: .P7 = dd
        Get #iNoOfFile, , dd: .BETA_X = dd
        Get #iNoOfFile, , dd: .BETA_Y = dd
        Get #iNoOfFile, , dd: .C8 = dd
        Get #iNoOfFile, , dd: .DC8 = dd
        Get #iNoOfFile, , dd: .ALPHA_X = dd
        Get #iNoOfFile, , dd: .ALPHA_Y = dd
    End With
    'cCostCAtMain
    With oHeader.cCostCAtMain
        Get #iNoOfFile, , ii: .bReadFractionnement = ii
        Get #iNoOfFile, , ii: .bReadAccesibilite = ii
        Get #iNoOfFile, , ii: .bReadAtelier = ii
        Get #iNoOfFile, , ii: .iNAM = ii
        Get #iNoOfFile, , LL: .lPMB = LL
        Get #iNoOfFile, , LL: .lDiversTempsTolier = LL
        Get #iNoOfFile, , LL: .lDiversTempsSoudeur = LL
        Get #iNoOfFile, , ii: .NoIndicesDCoque = ii
        Select Case Val_(right(sFileVer, 1))
            Case Is >= 1
                'Coefficients Accessibilité et Atelier groupés
                Get #iNoOfFile, , dd: .CoefAccPrePreVoile = dd
                Get #iNoOfFile, , dd: .CoefAccPrePreNappe = dd
                Get #iNoOfFile, , dd: .CoefAccPreAssInner = dd
                Get #iNoOfFile, , dd: .CoefAccPreAssOuter = dd
                Get #iNoOfFile, , dd: .CoefAccMontage = dd
                Get #iNoOfFile, , dd: .CoefAccPonts = dd
                Get #iNoOfFile, , dd: .CoefAccUsinage = dd
                Get #iNoOfFile, , dd: .CoefAtPrePreVoile = dd
                Get #iNoOfFile, , dd: .CoefAtPrePreNappe = dd
                Get #iNoOfFile, , dd: .CoefAtPreAssInner = dd
                Get #iNoOfFile, , dd: .CoefAtPreAssOuter = dd
                Get #iNoOfFile, , dd: .CoefAtMontage = dd
                Get #iNoOfFile, , dd: .CoefAtPonts = dd
                Get #iNoOfFile, , dd: .CoefAtUsinage = dd
        End Select

    End With
    'Load Cases
    Dim iLoadCase As Integer
    Dim oLoadCase As cLoadCase
    Dim colLoadCase As colLoadCase
    Set colLoadCase = Project.Item(ProjectIndex).cHeader.colLoadCase
    Get #iNoOfFile, , ii
    iLoadCase = ii
    For i = 1 To iLoadCase
        Set oLoadCase = New cLoadCase
        With oLoadCase
            Get #iNoOfFile, , ii: .index = ii
            Get #iNoOfFile, , ss: .Title = Trim(ss)
            Get #iNoOfFile, , ii: .state = ii
            Get #iNoOfFile, , dd: .LateralPressureIn = dd
            Get #iNoOfFile, , dd: .LateralPressureOut = dd
            Get #iNoOfFile, , dd: .VerticalBendingMomentFore = dd
            'Get #iNoOfFile, , dd: .VerticalBendingMomentAft = dd
            Get #iNoOfFile, , dd: .VerticalShear = dd
            Get #iNoOfFile, , dd: .HorizontalBendingMomentFore = dd
            'Get #iNoOfFile, , dd: .HorizontalBendingMomentAft = dd
            Get #iNoOfFile, , dd: .HorizontalShear = dd
        End With
        colLoadCase.Add oLoadCase, i
        Set oLoadCase = Nothing
        Get #iNoOfFile, , ii 'Stepwise lateral pressures
        Get #iNoOfFile, , ii 'Structural constraints
    Next i
    'Mars Loads
    'colMarsLoadCase
    Dim iMlc As Integer
    Dim oMlc As cMarsLoadCase
    Dim colMlc As colMarsLoadCase
    Set colMlc = oHeader.cMarsLoads.colMarsLoadCase
    Dim ss10 As String * 10
    Get #iNoOfFile, , ii: iMlc = ii
    For i = 1 To iMlc
        Set oMlc = New cMarsLoadCase
        Get #iNoOfFile, , ii: oMlc.index = ii
        Get #iNoOfFile, , ss10: oMlc.Label = Trim(ss10)
        Get #iNoOfFile, , dd: oMlc.CF_VerticalWaveBendingMoment_Shear = dd
        Get #iNoOfFile, , dd: oMlc.CF_HorizontalWaveBendingMoment = dd
        Dim iType As Integer
        Dim oType As cMarsLoadType
        Dim colType As colMarsLoadType
        Set colType = oMlc.colMarsLoadType
        Get #iNoOfFile, , ii: iType = ii
        For j = 1 To iType
            Set oType = New cMarsLoadType
            Get #iNoOfFile, , ii: oType.index = ii
            Get #iNoOfFile, , ss10: oType.Label = Trim(ss10)
            'Press on left comp
            Get #iNoOfFile, , dd: oType.LeftCompPress.In_Node = dd
            Get #iNoOfFile, , dd: oType.RightCompPress.Out_Node = dd
            'Press on right comp
            Get #iNoOfFile, , dd: oType.LeftCompPress.In_Node = dd
            Get #iNoOfFile, , dd: oType.RightCompPress.Out_Node = dd
            'Related Draught
            Get #iNoOfFile, , ii: oType.RelatedDraught = ii
            colType.Add oType, oType.index
            Set oType = Nothing
        Next j
        colMlc.Add oMlc, oMlc.index
        Set oMlc = Nothing
    Next i
    'colMarsLoadType
    Set colType = oHeader.cMarsLoads.colMarsLoadType
    Get #iNoOfFile, , ii: iType = ii
    For i = 1 To iType
        Set oType = New cMarsLoadType
        Get #iNoOfFile, , ii: oType.index = ii
        Get #iNoOfFile, , ss10: oType.Label = Trim(ss10)
        'Press on left comp
        Get #iNoOfFile, , dd: oType.LeftCompPress.In_Node = dd
        Get #iNoOfFile, , dd: oType.LeftCompPress.Out_Node = dd
        'Press on right comp
        Get #iNoOfFile, , dd: oType.RightCompPress.In_Node = dd
        Get #iNoOfFile, , dd: oType.RightCompPress.Out_Node = dd
        'Related Draught
        Get #iNoOfFile, , ii: oType.RelatedDraught = ii
        colType.Add oType, oType.index
        Set oType = Nothing
    Next i
    'colDraught
    Dim iDra As Long
    Dim oDra As cDouble
    Dim colDra As colDouble
    Set colDra = oHeader.cMarsLoads.colDraught
    Get #iNoOfFile, , ii: iDra = ii
    For i = 1 To iDra
        Set oDra = New cDouble
        Get #iNoOfFile, , LL: oDra.index = LL
        Get #iNoOfFile, , ss10: oDra.Label = Trim(ss10)
        Get #iNoOfFile, , dd: oDra.Value = dd
        colDra.Add oDra, oDra.index
        Set oDra = Nothing
    Next i
    Get #iNoOfFile, , dd: oHeader.cMarsLoads.StillWaterBendingMomentHogg = dd
    Get #iNoOfFile, , dd: oHeader.cMarsLoads.StillWaterBendingMomentSagg = dd
    Get #iNoOfFile, , dd: oHeader.cMarsLoads.VerticalWaveBendingMomentHogg = dd
    Get #iNoOfFile, , dd: oHeader.cMarsLoads.VerticalWaveBendingMomentSagg = dd
    Get #iNoOfFile, , dd: oHeader.cMarsLoads.HorizontalWaveBendingMoment = dd
    If Val_(right(sFileVer, 1)) >= 3 Then
        Get #iNoOfFile, , dd: oHeader.cMarsLoads.VerticalStillShear = dd
    End If
    Get #iNoOfFile, , dd: oHeader.cMarsLoads.VerticalWaveShearPositive = dd
    Get #iNoOfFile, , dd: oHeader.cMarsLoads.VerticalWaveShearNegative = dd
    Get #iNoOfFile, , dd: oHeader.cMarsLoads.GammaStillGlobal = dd
    Get #iNoOfFile, , dd: oHeader.cMarsLoads.GammaWaveGlobal = dd
    Get #iNoOfFile, , dd: oHeader.cMarsLoads.GammaStillLocal = dd
    Get #iNoOfFile, , dd: oHeader.cMarsLoads.GammaWaveLocal = dd
    
    'Global Data
    'Origin
    With oHeader
        Get #iNoOfFile, , dd: .YAxisOrigin = dd
        Get #iNoOfFile, , dd: .ZAxisOrigin = dd
    End With
    'Gravity Center Constraints
    With oHeader.cGlobalConstraints
        Get #iNoOfFile, , ii: .GravityLimitRestriction = ii
        Get #iNoOfFile, , dd: .MinGravityCenter = dd
        Get #iNoOfFile, , dd: .MaxGravityCenter = dd
        Get #iNoOfFile, , ii: .IsInertia = ii
        Get #iNoOfFile, , dd: .Inertia = dd
        Get #iNoOfFile, , ii: .IsSectionModulus = ii
        Get #iNoOfFile, , dd: .SectionModulus = dd
        Get #iNoOfFile, , ii: .ZPanel = ii
        Get #iNoOfFile, , ii: .IsWeight = ii
        Get #iNoOfFile, , dd: .Weight = dd
        Get #iNoOfFile, , ii: .IsCost = ii
        Get #iNoOfFile, , dd: .Cost = dd
        Get #iNoOfFile, , ii: .CostType = ii
    End With
    '
    With oHeader
        Get #iNoOfFile, , dd: .YRED = dd
        Get #iNoOfFile, , ii: .IsBendingMoments = ii
    End With
    'Equality Restrictions
    Dim iEq As Integer
    Dim oEq As cEqualityRestrictions
    Dim colEq As colEqualityRestrictions
    Set colEq = oHeader.colEqualityRestrictions
    Get #iNoOfFile, , ii
    iEq = ii
    For i = 1 To iEq
        Set oEq = New cEqualityRestrictions
        Get #iNoOfFile, , ii: oEq.index = ii
        Get #iNoOfFile, , ii: oEq.LeadingPanel = ii
        Get #iNoOfFile, , ii: oEq.DependingPanel = ii
        Get #iNoOfFile, , ii: oEq.DependingDesignVariable = ii
        Get #iNoOfFile, , ii: oEq.LeadingDesignVariable = ii
        Get #iNoOfFile, , dd: oEq.Ratio = dd
        colEq.Add oEq, i
        Set oEq = Nothing
    Next i
    With oHeader
        Get #iNoOfFile, , ii: .IRESTR = ii
        Get #iNoOfFile, , ii: .IULT = ii
        Get #iNoOfFile, , ii: .NoOfDoubleHulls = ii
        Get #iNoOfFile, , BB: .MarsSymm = BB
    End With
    'Nodes
    Dim iNode As Integer
    Dim oNode As cNode
    Dim colNode As colNodes
    Set colNode = Project.Item(ProjectIndex).colNodes
    Get #iNoOfFile, , ii
    iNode = ii
    For i = 1 To ii
        Set oNode = New cNode
        Get #iNoOfFile, , ii: oNode.index = ii
        Get #iNoOfFile, , ii: oNode.nNumber = ii
        Get #iNoOfFile, , dd: oNode.y = dd
        Get #iNoOfFile, , dd: oNode.z = dd
        colNode.Add oNode, i
        Set oNode = Nothing
    Next i
    'PANELS
    Dim iPan As Integer
    Dim oPan As cPanel
    Dim colPan As colPanel
    Set colPan = Project.Item(ProjectIndex).colPanel
    Get #iNoOfFile, , ii
    iPan = ii
    For i = 1 To iPan
        Set oPan = New cPanel
        Get #iNoOfFile, , ii: oPan.index = ii
        Get #iNoOfFile, , ss: oPan.pLabel = Trim(ss)
        Get #iNoOfFile, , ii: oPan.pType = ii
        Get #iNoOfFile, , ii: oPan.RelatedDoubleHullPanel = ii
        Get #iNoOfFile, , ii: oPan.pNumber = ii
        'Geometry
        With oPan.cGeometry
            Get #iNoOfFile, , dd: .Q = dd
            Get #iNoOfFile, , dd: .PHIL = dd
            Get #iNoOfFile, , ii: .InNode = ii
            Get #iNoOfFile, , ii: .OutNode = ii
            Get #iNoOfFile, , dd: .PanelWidth = dd
            Get #iNoOfFile, , dd: .PanelAngle = dd
            Get #iNoOfFile, , dd: .Participation = dd
            Get #iNoOfFile, , dd: .BucklingLength = dd
            If Val_(right(sFileVer, 1)) >= 4 Then
                Get #iNoOfFile, , ii: .PositionCode = ii
            End If
            Get #iNoOfFile, , ii: .FAMI = ii
            Get #iNoOfFile, , ii: .LOT = ii
        End With
        'Straightening
'        Get #iNoOfFile, , ii: oPan.FAMI = ii
'        Get #iNoOfFile, , ii: oPan.LOT = ii
        'Scantlings
        With oPan.cScantlings.cPrimaryFrames
            Get #iNoOfFile, , dd: .Spacing = dd
            Get #iNoOfFile, , dd: .WebHeight = dd
            Get #iNoOfFile, , dd: .WebThickness = dd
            Get #iNoOfFile, , dd: .FlangeWidth = dd
            Get #iNoOfFile, , dd: .FlangeThickness = dd
            Get #iNoOfFile, , dd: .CorrosionThickness = dd
            Get #iNoOfFile, , ii: .Side = ii
            Get #iNoOfFile, , dd: .SpacingVariation = dd
            Get #iNoOfFile, , dd: .WebHeightVariation = dd
            Get #iNoOfFile, , dd: .WebThicknessVariation = dd
            Get #iNoOfFile, , dd: .FlangeWidthVariation = dd
            Get #iNoOfFile, , dd: .FlangeThicknessVariation = dd
        End With
        Dim ss2 As String * 3
        With oPan.cScantlings.cPrimaryStiffeners
            Get #iNoOfFile, , dd: .Spacing = dd
            Get #iNoOfFile, , dd: .WebHeight = dd
            Get #iNoOfFile, , dd: .WebThickness = dd
            Get #iNoOfFile, , dd: .FlangeWidth = dd
            Get #iNoOfFile, , dd: .FlangeThickness = dd
            Get #iNoOfFile, , dd: .CorrosionThickness = dd
            Get #iNoOfFile, , ss2: .DistributionMode = ss2
            Get #iNoOfFile, , ii: .Side = ii
            Get #iNoOfFile, , dd: .SpacingVariation = dd
            Get #iNoOfFile, , dd: .WebHeightVariation = dd
            Get #iNoOfFile, , dd: .WebThicknessVariation = dd
            Get #iNoOfFile, , dd: .FlangeWidthVariation = dd
            Get #iNoOfFile, , dd: .FlangeThicknessVariation = dd
        End With
        With oPan.cScantlings.cSecondaryFrames
            Get #iNoOfFile, , dd: .Spacing = dd
            Get #iNoOfFile, , dd: .WebHeight = dd
            Get #iNoOfFile, , dd: .WebThickness = dd
            Get #iNoOfFile, , dd: .FlangeWidth = dd
            Get #iNoOfFile, , dd: .FlangeThickness = dd
            Get #iNoOfFile, , dd: .CorrosionThickness = dd
            Get #iNoOfFile, , ii: .Side = ii
        End With
        With oPan.cScantlings.cSecondaryStiffeners
            Get #iNoOfFile, , dd: .Spacing = dd
            Get #iNoOfFile, , dd: .WebHeight = dd
            Get #iNoOfFile, , dd: .WebThickness = dd
            Get #iNoOfFile, , dd: .FlangeWidth = dd
            Get #iNoOfFile, , dd: .FlangeThickness = dd
            Get #iNoOfFile, , ii: .Side = ii
        End With
        Dim iGirder As Integer
        Dim oGirder As cGirder
        Dim colGirder As colGirder
        Set colGirder = oPan.cScantlings.colGirder
        Get #iNoOfFile, , ii
        iGirder = ii
        For j = 1 To iGirder
            Set oGirder = New cGirder
            Get #iNoOfFile, , ii: oGirder.index = ii
            Get #iNoOfFile, , dd: oGirder.FlangeThickness = dd
            Get #iNoOfFile, , dd: oGirder.FlangeWidth = dd
            Get #iNoOfFile, , dd: oGirder.WebHeight = dd
            Get #iNoOfFile, , dd: oGirder.WebThickness = dd
            Get #iNoOfFile, , dd: oGirder.Distance = dd
            colGirder.Add oGirder, j
            Set oGirder = Nothing
        Next j
        With oPan.cScantlings
            Get #iNoOfFile, , ii: .GirderSide = ii
            Get #iNoOfFile, , dd: .NetThickness = dd
            Get #iNoOfFile, , dd: .CorrosionThickness = dd
            Get #iNoOfFile, , dd: .GrossThickness = dd
            Get #iNoOfFile, , ii: .BeamSection = ii
            Get #iNoOfFile, , dd: .ThicknessVariation = dd
        End With
        oPan.cScantlings.cPrimaryStiffeners.GrossSectionModulus = GetSectionModulus(oPan.cScantlings.cPrimaryStiffeners.WebHeight, _
                oPan.cScantlings.cPrimaryStiffeners.WebThickness + oPan.cScantlings.cPrimaryStiffeners.CorrosionThickness, _
                oPan.cScantlings.cPrimaryStiffeners.FlangeWidth, _
                oPan.cScantlings.cPrimaryStiffeners.FlangeThickness + oPan.cScantlings.cPrimaryStiffeners.CorrosionThickness, _
                oPan.cScantlings.cPrimaryStiffeners.Spacing, _
                oPan.cScantlings.cPrimaryFrames.Spacing, _
                oPan.cScantlings.GrossThickness)

        'Materials
        With oPan.cMaterial
            Get #iNoOfFile, , ii: .index = ii
            Get #iNoOfFile, , ii: .mNumber = ii
            Get #iNoOfFile, , ss: .Name = ss
            Get #iNoOfFile, , dd: .YoungModulus = dd
            Get #iNoOfFile, , dd: .Poisson = dd
            Get #iNoOfFile, , dd: .YieldStress = dd
            Get #iNoOfFile, , dd: .AllowableStress = dd
            If Val_(right(sFileVer, 1)) >= 4 Then
                Get #iNoOfFile, , dd: .MaterialCoeff = dd
            End If
            Get #iNoOfFile, , dd: .SpecificWeight = dd
        End With
        With oPan
            Get #iNoOfFile, , ii: .IsBuoyancyForce = ii
            Get #iNoOfFile, , dd: .LocalizedPressure = dd
            Get #iNoOfFile, , ii: .UniformLateralPressureVariation = ii
            Get #iNoOfFile, , ii: .IsWiseLateralPressure = ii
            Get #iNoOfFile, , ii: .IsStructuralConstraints = ii
            Get #iNoOfFile, , ii: .IsDesignVariables = ii
            Get #iNoOfFile, , ii: .LeftCompartment = ii
            Get #iNoOfFile, , ii: .RightCompartment = ii
            Get #iNoOfFile, , ii: .LateralPressureSide = ii
            Get #iNoOfFile, , ii: .NoOfGeomConstr = ii
            Get #iNoOfFile, , ii: .FramesFlangeThicknessUpdate = ii
            Get #iNoOfFile, , ii: .StiffenersFlangeThicknessUpdate = ii
        End With
        'Design Variables
        Dim iDes As Integer
        Dim oDes As cDesignVariables
        Dim colDes As colDesignVariables
        Set colDes = oPan.colDesignVariables
        Get #iNoOfFile, , ii
        iDes = ii
        Select Case Val_(right(sFileVer, 1))
            Case 0
                ''Default Design Variables
                ''------------------------
                Dim v() As Variant
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

                For j = 1 To iDes
                    Get #iNoOfFile, , ii: 'oDes.Index = ii
                    Get #iNoOfFile, , ii: k = ii
                    colDes.Item(k).Active = True
                    Get #iNoOfFile, , dd: colDes.Item(k).UpperLimit = dd
                    Get #iNoOfFile, , dd: colDes.Item(k).LowerLimit = dd
                Next j
            Case Is >= 1
                For j = 1 To 9
                    Set oDes = New cDesignVariables
                    Get #iNoOfFile, , ii: oDes.index = ii
                    Get #iNoOfFile, , BB: oDes.Active = BB
                    Get #iNoOfFile, , ii: oDes.VariableName = ii
                    Get #iNoOfFile, , dd: oDes.UpperLimit = dd
                    Get #iNoOfFile, , dd: oDes.LowerLimit = dd
                    colDes.Add oDes, j
                    Set oDes = Nothing
                Next j
        End Select
        'Load Cases
        Set colLoadCase = oPan.colLoadCase
        Get #iNoOfFile, , ii
        iLoadCase = ii
        For j = 1 To iLoadCase
            Set oLoadCase = New cLoadCase
            Get #iNoOfFile, , ii: oLoadCase.index = ii
            Get #iNoOfFile, , ss: oLoadCase.Title = Trim(ss)
            Get #iNoOfFile, , ii: oLoadCase.state = ii
            Get #iNoOfFile, , dd: oLoadCase.LateralPressureIn = dd
            Get #iNoOfFile, , dd: oLoadCase.LateralPressureOut = dd
            Get #iNoOfFile, , dd: oLoadCase.VerticalBendingMomentFore = dd
            Get #iNoOfFile, , dd: oLoadCase.VerticalBendingMomentAft = dd
            Get #iNoOfFile, , dd: oLoadCase.HorizontalBendingMomentFore = dd
            Get #iNoOfFile, , dd: oLoadCase.HorizontalBendingMomentAft = dd
            'Stepwise Pressures
            Dim iStep As Integer
            Dim oStep As cStepWiseLateralPressure
            Dim colStep As colStepWiseLateralPressure
            Set colStep = oLoadCase.colStepWiseLateralPressure
            Get #iNoOfFile, , ii
            iStep = ii
            For k = 1 To iStep
                Set oStep = New cStepWiseLateralPressure
                Get #iNoOfFile, , ii: oStep.index = ii
                Get #iNoOfFile, , dd: oStep.VerticalGravityLoad = dd
                Get #iNoOfFile, , dd: oStep.LateralPressureIn = dd
                Get #iNoOfFile, , dd: oStep.LateralPressureOut = dd
                colStep.Add oStep, k
                Set oStep = Nothing
            Next k
            'Structural Constraints
            Dim iStru As Integer
            Dim oStru As cStructuralConstraints
            Dim colStru As colStructuralConstraints
            Set colStru = oLoadCase.colStructuralConstraints
            Get #iNoOfFile, , ii
            iStru = ii
            For k = 1 To iStru
                Set oStru = New cStructuralConstraints
                Get #iNoOfFile, , ii: oStru.index = ii
                Get #iNoOfFile, , ii: oStru.Reference = ii
                Get #iNoOfFile, , dd: oStru.Value = dd
                Get #iNoOfFile, , ii: oStru.Limit = ii
                Get #iNoOfFile, , ii: oStru.AssesmentPoint = ii
                Get #iNoOfFile, , dd: oStru.Point = dd
                colStru.Add oStru, k
                Set oStru = Nothing
            Next k
            colLoadCase.Add oLoadCase, j
            Set oLoadCase = Nothing
        Next j
        'Mars Loads
        'colMarsLoadCase
        Set colMlc = oPan.colMarsLoadCase
        Get #iNoOfFile, , ii: iMlc = ii
        For j = 1 To iMlc
            Set oMlc = New cMarsLoadCase
            Get #iNoOfFile, , ii: oMlc.index = ii
            Get #iNoOfFile, , ss10: oMlc.Label = Trim(ss10)
            Get #iNoOfFile, , dd: oMlc.CF_VerticalWaveBendingMoment_Shear = dd
            Get #iNoOfFile, , dd: oMlc.CF_HorizontalWaveBendingMoment = dd
            Set colType = oMlc.colMarsLoadType
            Get #iNoOfFile, , ii: iType = ii
            For k = 1 To iType
                Set oType = New cMarsLoadType
                Get #iNoOfFile, , ii: oType.index = ii
                Get #iNoOfFile, , ss10: oType.Label = Trim(ss10)
                'Press on left comp
                Get #iNoOfFile, , dd: oType.LeftCompPress.In_Node = dd
                Get #iNoOfFile, , dd: oType.LeftCompPress.Out_Node = dd
                'Press on right comp
                Get #iNoOfFile, , dd: oType.RightCompPress.In_Node = dd
                Get #iNoOfFile, , dd: oType.RightCompPress.Out_Node = dd
                'Related Draught
                Get #iNoOfFile, , ii: oType.RelatedDraught = ii
                colType.Add oType, oType.index
                Set oType = Nothing
            Next k
            colMlc.Add oMlc, oMlc.index
            Set oMlc = Nothing
        Next j
        'Assessment Points (for Structural Constraints)
        Dim iAss As Integer
        Dim oAss As cAssessmentPoint
        Dim colAss As colAssessmentPoints
        Set colAss = oPan.colAssessmentPoints
        Get #iNoOfFile, , ii
        iAss = ii
        For j = 1 To iAss
            Set oAss = New cAssessmentPoint
            Get #iNoOfFile, , ii: oAss.index = ii
            Get #iNoOfFile, , dd: oAss.Value = dd
            colAss.Add oAss, j
            Set oAss = Nothing
        Next j
        'Geometrical Constraints
        Dim iGeo As Integer
        Dim oGeo As cGeometricalConstraints
        Dim colGeo As colGeometricalConstraints
        Set colGeo = oPan.colGeometricalConstraints
        Get #iNoOfFile, , ii
        iGeo = ii
        Select Case iGeo
            Case 0
            Case Is = 99
                Set oGeo = New cGeometricalConstraints
                Get #iNoOfFile, , ii: oGeo.index = ii
                Get #iNoOfFile, , ii: oGeo.code = ii
                colGeo.Add oGeo, oGeo.index
                Set oGeo = Nothing
            Case Is > 0
                For j = 1 To iGeo
                Set oGeo = New cGeometricalConstraints
                Get #iNoOfFile, , ii: oGeo.index = ii
                Get #iNoOfFile, , ii: oGeo.code = ii
                colGeo.Add oGeo, oGeo.index
                Set oGeo = Nothing
                Next j
        End Select
        'Boundary Conditions
        Dim iBound As Integer
        Dim oBound As cBoundaryConditions
        Dim colBound As colBoundaryConditions
        Set colBound = oPan.colBoundaryConditions
        Get #iNoOfFile, , ii
        iBound = ii
        For k = 1 To iBound
            Set oBound = New cBoundaryConditions
            Get #iNoOfFile, , ii: oBound.index = ii
            Get #iNoOfFile, , ii: oBound.BoundaryCondition = CInt(ii)
            Get #iNoOfFile, , ii: oBound.Edge = CInt(ii)
            colBound.Add oBound, oBound.index
            Set oBound = Nothing
        Next k
        'CostCAt
        With oPan.cCostCAtMain
            Get #iNoOfFile, , ii: .ID_PANNEAU = CInt(ii)
            Get #iNoOfFile, , ii: .IT_PANNEAU = CInt(ii)
            Get #iNoOfFile, , ii: .IP_PANNEAU = CInt(ii)
            Get #iNoOfFile, , ii: .PositionAboutsLisses = CInt(ii)
            Dim iOp As Integer
            Dim cOp As cCostCAtOperations
            Dim colOp As colCostCAtOperations
            Set colOp = New colCostCAtOperations '.colCostCAtOperations
            Set .colCostCAtOperations = Nothing
            Get #iNoOfFile, , ii: iOp = ii
            For j = 1 To iOp
                Set cOp = New cCostCAtOperations
                Get #iNoOfFile, , ii: cOp.index = ii
                Get #iNoOfFile, , dd: cOp.Fractionnement = dd
                Get #iNoOfFile, , dd: cOp.Accesibilite = dd
                Get #iNoOfFile, , dd: cOp.Atelier = dd
                Get #iNoOfFile, , ii: cOp.Soudures = ii
                Get #iNoOfFile, , dd: cOp.Gorges = dd
                colOp.Add cOp, cOp.index
                Set cOp = Nothing
            Next j
            Set .colCostCAtOperations = colOp.Clone
            Get #iNoOfFile, , BB: .bIsPartOfNappe = BB
            'Voiles
            Get #iNoOfFile, , ii: .ProfilesSurVoiles = ii
            Get #iNoOfFile, , ii: .SoudureProfilesSurVoiles = CInt(ii)
            Get #iNoOfFile, , ii: .GoussetsProfilesVoiles = ii
            Get #iNoOfFile, , ii: .PlatsEnBute = ii
            Get #iNoOfFile, , ii: .AccostagesVoiles = ii
            ' CarlinguesSerresHiloires
            Get #iNoOfFile, , ii: .iNCI = ii
            Get #iNoOfFile, , ii: .HabillageCarlingues = CInt(ii)
            Get #iNoOfFile, , ii: .DimensionHabillage = CInt(ii)
            ' Pré (nappe plane)
            Get #iNoOfFile, , ii: .AccostagesNappes = ii
            Get #iNoOfFile, , ii: .iNANP = ii
            Get #iNoOfFile, , ii: .SoudureLissesNappes = CInt(ii)
            Get #iNoOfFile, , ii: .SectionLisses = CInt(ii)
            'Pré (assemblage)
            Get #iNoOfFile, , ii: .TypeTapes = CInt(ii)
            Get #iNoOfFile, , ii: .SectionTapes = CInt(ii)
            Get #iNoOfFile, , ii: .CouplesCarlinguesSurNappes = ii
            Get #iNoOfFile, , ii: .AccostagesToleBouchain = ii
            'Pré-montage - montage
            Get #iNoOfFile, , ii: .ContactsBarrotsCloisons = ii
            Get #iNoOfFile, , ii: .JonctionsBarrotHiloire = ii
            Select Case Val_(right(sFileVer, 1))
                Case 0
                    'Coefficients Accessibilité et Atelier groupés
                    Get #iNoOfFile, , dd: .CoefAccPrePreVoile = dd
                    Get #iNoOfFile, , dd: .CoefAccPrePreNappe = dd
                    Get #iNoOfFile, , dd: .CoefAccPreAssInner = dd
                    Get #iNoOfFile, , dd: .CoefAccPreAssOuter = dd
                    Get #iNoOfFile, , dd: .CoefAccMontage = dd
                    Get #iNoOfFile, , dd: .CoefAccPonts = dd
                    Get #iNoOfFile, , dd: .CoefAccUsinage = dd
                    Get #iNoOfFile, , dd: .CoefAtPrePreVoile = dd
                    Get #iNoOfFile, , dd: .CoefAtPrePreNappe = dd
                    Get #iNoOfFile, , dd: .CoefAtPreAssInner = dd
                    Get #iNoOfFile, , dd: .CoefAtPreAssOuter = dd
                    Get #iNoOfFile, , dd: .CoefAtMontage = dd
                    Get #iNoOfFile, , dd: .CoefAtPonts = dd
                    Get #iNoOfFile, , dd: .CoefAtUsinage = dd
            End Select
        End With
        colPan.Add oPan, i
        Set oPan = Nothing
    Next i
    'PANEL UPDATED SCANTLINGS
    Dim btmp As Boolean
    btmp = False
    Dim iPanU As Integer
    Dim oPanU As cPanel
    Dim colPanU As colPanel
    Set colPanU = Project.Item(ProjectIndex).colPanelUpdate
    Get #iNoOfFile, , ii: iPanU = ii
    For i = 1 To iPanU
        Set oPanU = New cPanel
        oPanU.pType = Project.Item(ProjectIndex).colPanel.Item(i).pType
        With oPanU
        
        
        'working

        Get #iNoOfFile, , ii: .index = ii
        
'        Select Case i
'            Case 16, 19, 25, 28, 32, 35, 40, 44, 48, 52, 84, 85
'            Get #iNoOfFile, , dd
'            .index = i
'        End Select



'            If ii = 0 Then
'            Get #iNoOfFile, , ii: .index = ii
'            End If
            
            
            
            Get #iNoOfFile, , ii: .pNumber = ii
            Get #iNoOfFile, , LL: .Region = LL
        End With
        'Scantlings
        'Primary Frames
        With oPanU.cScantlings.cPrimaryFrames
            Get #iNoOfFile, , dd: .Spacing = dd
            Get #iNoOfFile, , dd: .WebHeight = dd
            Get #iNoOfFile, , dd: .WebThickness = dd
            Get #iNoOfFile, , dd: .FlangeWidth = dd
            Get #iNoOfFile, , dd: .FlangeThickness = dd
            Get #iNoOfFile, , dd: .CorrosionThickness = dd
            Get #iNoOfFile, , ii: .Side = ii
            Get #iNoOfFile, , dd: .SpacingVariation = dd
            Get #iNoOfFile, , dd: .WebHeightVariation = dd
            Get #iNoOfFile, , dd: .WebThicknessVariation = dd
            Get #iNoOfFile, , dd: .FlangeWidthVariation = dd
            Get #iNoOfFile, , dd: .FlangeThicknessVariation = dd
        End With
        'Primary Stiffeners
        With oPanU.cScantlings.cPrimaryStiffeners
            Get #iNoOfFile, , dd: .Spacing = dd
            Get #iNoOfFile, , dd: .WebHeight = dd
            Get #iNoOfFile, , dd: .WebThickness = dd
            Get #iNoOfFile, , dd: .FlangeWidth = dd
            Get #iNoOfFile, , dd: .FlangeThickness = dd
            Get #iNoOfFile, , dd: .CorrosionThickness = dd
            Get #iNoOfFile, , ss2: .DistributionMode = Trim(ss2)
            Get #iNoOfFile, , ii: .Side = ii
            Get #iNoOfFile, , dd: .SpacingVariation = dd
            Get #iNoOfFile, , dd: .WebHeightVariation = dd
            Get #iNoOfFile, , dd: .WebThicknessVariation = dd
            Get #iNoOfFile, , dd: .FlangeWidthVariation = dd
            Get #iNoOfFile, , dd: .FlangeThicknessVariation = dd
        End With
        'Secondary Frames
        With oPanU.cScantlings.cSecondaryFrames
            Get #iNoOfFile, , dd: .Spacing = dd
            Get #iNoOfFile, , dd: .WebHeight = dd
            Get #iNoOfFile, , dd: .WebThickness = dd
            Get #iNoOfFile, , dd: .FlangeWidth = dd
            Get #iNoOfFile, , dd: .FlangeThickness = dd
            Get #iNoOfFile, , dd: .CorrosionThickness = dd
            Get #iNoOfFile, , ii: .Side = ii
        End With
        'Secondary Stiffeners
        With oPanU.cScantlings.cSecondaryStiffeners
            Get #iNoOfFile, , dd: .Spacing = dd
            Get #iNoOfFile, , dd: .WebHeight = dd
            Get #iNoOfFile, , dd: .WebThickness = dd
            Get #iNoOfFile, , dd: .FlangeWidth = dd
            Get #iNoOfFile, , dd: .FlangeThickness = dd
            Get #iNoOfFile, , ii: .Side = ii
        End With
        'Girders
        Dim iGirU As Integer
        Dim oGirU As cGirder
        Dim colGirU As colGirder
        Set colGirU = oPanU.cScantlings.colGirder
        Get #iNoOfFile, , ii: iGirU = ii
        If ii > 0 Then
            ii = ii
            btmp = True
        Else
            btmp = False
        End If
        For j = 1 To iGirU
            Set oGirU = New cGirder
            With oGirU
                Get #iNoOfFile, , ii: .index = ii
                Get #iNoOfFile, , dd: .FlangeThickness = dd
                Get #iNoOfFile, , dd: .FlangeWidth = dd
                Get #iNoOfFile, , dd: .WebHeight = dd
                Get #iNoOfFile, , dd: .WebThickness = dd
                Get #iNoOfFile, , dd: .Distance = dd
            End With
            colGirU.Add oGirU, oGirU.index
            Set oGirU = Nothing
        Next j
        With oPanU.cScantlings
            Get #iNoOfFile, , ii: .GirderSide = ii
            Get #iNoOfFile, , dd: .NetThickness = dd
            Get #iNoOfFile, , dd: .CorrosionThickness = dd
            Get #iNoOfFile, , dd: .GrossThickness = dd
            Get #iNoOfFile, , ii: .BeamSection = ii
            Get #iNoOfFile, , dd: .ThicknessVariation = dd
        End With
        'Geometry
        With oPanU.cGeometry
            Get #iNoOfFile, , dd: .Q = dd
            Get #iNoOfFile, , dd: .PHIL = dd
            Get #iNoOfFile, , ii: .InNode = ii
            Get #iNoOfFile, , ii: .OutNode = ii
            Get #iNoOfFile, , dd: .PanelWidth = dd
            Get #iNoOfFile, , dd: .PanelAngle = dd
            Get #iNoOfFile, , dd: .Participation = dd
            Get #iNoOfFile, , dd: .BucklingLength = dd
            
            If Val_(right(sFileVer, 1)) >= 4 Then
                Get #iNoOfFile, , ii: .PositionCode = ii
            End If
            
            Get #iNoOfFile, , ii: .FAMI = ii
            Get #iNoOfFile, , ii: .LOT = ii

        End With
        Dim iBouU As Integer
        Dim oBouU As cBoundaryConditions
        Dim colBouU As colBoundaryConditions
        Set colBouU = oPanU.colBoundaryConditions
        Get #iNoOfFile, , ii: iBouU = ii
        If ii <> 0 Then
            ii = ii
        End If
        For j = 1 To iBouU
            Set oBouU = New cBoundaryConditions
            With oBouU
                Get #iNoOfFile, , ii: .index = ii
                Get #iNoOfFile, , ii: .BoundaryCondition = ii
                Get #iNoOfFile, , ii: .Edge = ii
            End With
            colBouU.Add oBouU, oBouU.index
            Set oBouU = Nothing
        Next j
        colPanU.Add oPanU, oPanU.index
        Set oPanU = Nothing
    Next i
    'SOLUTION
    Dim vv() As Variant
    Dim oSol As cSolution
    Set oSol = Project.Item(ProjectIndex).cSolution
    With oSol
        Get #iNoOfFile, , BB: .IsSolution = BB
        If .IsSolution = True Then
            SolMatrix ProjectIndex
            Get #iNoOfFile, , POL: .Solution = POL
            Get #iNoOfFile, , dd: .NeutralAxis = dd
            If Val_(right(sFileVer, 1)) >= 2 Then
                Get #iNoOfFile, , dd: .NeutralAxisGross = dd
                Get #iNoOfFile, , dd: .Iyy = dd
                Get #iNoOfFile, , dd: .IyyGross = dd
            End If
        End If
        
    End With
    'COMPARTMENTS
    Dim iComp As Integer
    Dim oComp As cCompartment
    Dim colComp As colCompartments
    Set colComp = Project.Item(ProjectIndex).colCompartments
    Get #iNoOfFile, , ii
    iComp = ii
    For i = 1 To iComp
        Set oComp = New cCompartment
        With oComp
            'General
            Get #iNoOfFile, , ii: .index = ii
            Get #iNoOfFile, , ss: .Name = Trim(ss)
            Get #iNoOfFile, , ii: .MainDestination = ii
            Get #iNoOfFile, , ii: .CompartmentType = ii
            Get #iNoOfFile, , ii: .CompartmentLoad = ii
            'Dimensions
            Get #iNoOfFile, , dd: .Length = dd
            Get #iNoOfFile, , dd: .Breadth = dd
            Get #iNoOfFile, , dd: .Height = dd
            Get #iNoOfFile, , dd: .XStartFromAPP = dd
        End With
        colComp.Add oComp, oComp.index
        Set oComp = Nothing
    Next i
    'Double Hulls
    Dim iDH As Integer
    Dim cDH As cCostCAtDHull
    Dim colDH As colCostCAtDHull
    Set colDH = Project.Item(ProjectIndex).colCostCAtDHull
    Get #iNoOfFile, , ii: iDH = ii
    For i = 1 To iDH
        Set cDH = New cCostCAtDHull
        Get #iNoOfFile, , ii: cDH.index = ii
        Dim iIndex As Integer
        Dim cIndex As cIndex
        Dim colIndex As colIndex
        'Inner Shell
        Set colIndex = cDH.InnerShell
        Get #iNoOfFile, , ii: iIndex = ii
        For j = 1 To iIndex
            Set cIndex = New cIndex
            Get #iNoOfFile, , LL: cIndex.index = LL
            Get #iNoOfFile, , LL: cIndex.Number = LL
            colIndex.Add cIndex, cIndex.index
            Set cIndex = Nothing
        Next j
        'Outer Shell
        Set colIndex = cDH.OuterShell
        Get #iNoOfFile, , ii: iIndex = ii
        For j = 1 To iIndex
            Set cIndex = New cIndex
            Get #iNoOfFile, , LL: cIndex.index = LL
            Get #iNoOfFile, , LL: cIndex.Number = LL
            colIndex.Add cIndex, cIndex.index
            Set cIndex = Nothing
        Next j
        '------
        With cDH
        Get #iNoOfFile, , ii: .ProfilesSurVoiles = ii
        Get #iNoOfFile, , ii: .SoudureProfilesSurVoiles = CInt(ii)
        Get #iNoOfFile, , ii: .GoussetsProfilesVoiles = ii
        Get #iNoOfFile, , ii: .PlatsEnBute = ii
        Get #iNoOfFile, , ii: .AccostagesVoiles = ii
        Get #iNoOfFile, , ii: .TypeTapes = CInt(ii)
        Get #iNoOfFile, , ii: .SectionTapes = CInt(ii)
        Get #iNoOfFile, , ii: .SectionLisses = CInt(ii)
        End With
        colDH.Add cDH, cDH.index
        Set cDH = Nothing
    Next i
    'Nappes
    Dim iNap As Integer
    Dim cNap As cCostCAtNappe
    Dim colNap As colCostCAtNappe
    Set colNap = Project.Item(ProjectIndex).colCostCAtNappe
    Get #iNoOfFile, , ii: iNap = ii
    For i = 1 To iNap
        Set cNap = New cCostCAtNappe
        Get #iNoOfFile, , ii: cNap.index = ii
        Dim colIndex1 As colIndex
        Set colIndex1 = cNap.Nappe
        Get #iNoOfFile, , ii: iIndex = ii
        For j = 1 To iIndex
            Set cIndex = New cIndex
            Get #iNoOfFile, , LL: cIndex.index = LL
            Get #iNoOfFile, , LL: cIndex.Number = LL
            colIndex1.Add cIndex, cIndex.index
            Set cIndex = Nothing
        Next j
        With cNap
            Get #iNoOfFile, , ii: .AccostagesNappes = ii
            Get #iNoOfFile, , ii: .iNANP = ii
            Get #iNoOfFile, , ii: .SoudureLissesNappes = CInt(ii)
        End With
        colNap.Add cNap, cNap.index
        Set cNap = Nothing
    Next i
    
    
    UpdatePanelConnections ProjectIndex
    
    'tmp
'    Dim cPanel As cPanel
'    For Each cPanel In Project.Item(ProjectIndex).colPanel
'        cPanel.cCostCAtMain.SetFirstPanelData cPanel.pNumber, ProjectIndex
'    Next cPanel
'    ComputeFirstFractionnements ProjectIndex

    Exit Function
OpenASCIIVer11Err:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modFileOpenASCII: Function OpenASCIIVer11")
    Close #iNoOfFile
End Function


