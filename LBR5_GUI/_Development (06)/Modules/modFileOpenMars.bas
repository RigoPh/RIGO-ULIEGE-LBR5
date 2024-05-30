Attribute VB_Name = "modFileOpenMars"
Option Explicit
Dim ProjectIndex As Integer
Dim sFileVer As String, sSubVer As String

Public Function OpenMarsFile(ts As TextStream, ByVal index As Integer)
    On Error GoTo OpenMarsFileErr
    Dim sLine As String
    
    Dim v() As Variant
    ProjectIndex = index
    Do Until Left(UCase(sLine), 9) = "<VERSION>"
        sLine = ReadLn(ts)
    Loop
    sLine = ReadLn(ts)
    GetValues 1, sLine, v
    sFileVer = v(1)
    Project.Item(ProjectIndex).FileVersionNumber = sFileVer
    sSubVer = right(sFileVer, 1)
    Select Case sFileVer
        Case "1.0.0"
            OpenMarsVer100 ts
        Case Else
            MsgBox "Bad Version Number: " & "'" & sFileVer & "'.", vbCritical + vbOKOnly
        End Select
    Exit Function
OpenMarsFileErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modFileOpenMars: Function OpenMarsFile")
End Function

'version 1.0.0
Private Function OpenMarsVer100(ts As TextStream)
    On Error GoTo OpenMarsVer100Err
    Dim sLine As String
    Dim v() As Variant
    Dim i As Integer, j As Integer
    'READ GEOMETRY
    'HEADER DATA
    Dim oHeader As cHeader
    Set oHeader = Project.Item(ProjectIndex).cHeader
    'Title
    Do Until Left(UCase(sLine), 7) = "<TITLE>"
        sLine = ReadLn(ts)
    Loop
    'oHeader.IANA = 2
    If Licensing.IS_LBR4 = False And Licensing.IS_BEAM_THEORY = True Then
        Project.Item(ProjectIndex).cHeader.IANA = 2
    End If
    If Licensing.IS_LBR4 = True And Licensing.IS_BEAM_THEORY = False Then
        Project.Item(ProjectIndex).cHeader.IANA = 1
    End If
    If Licensing.IS_LBR4 = True And Licensing.IS_BEAM_THEORY = True Then
        Project.Item(ProjectIndex).cHeader.IANA = 2
    End If
    
    Project.Item(ProjectIndex).frmProject.NegociateIANAmenus
    'Project.Item(ProjectIndex).frmProject.NegociateLICmenus
    
    sLine = ReadLn(ts)
    GetValues 1, sLine, v
    oHeader.Title = v(1)
    'X Section
    Do Until Left(UCase(sLine), 9) = "<SECTION>"
        sLine = ReadLn(ts)
    Loop
    sLine = ReadLn(ts)
    GetValues 1, sLine, v
    oHeader.XSection = Val_(v(1))
    'Symmetry
    Do Until Left(UCase(sLine), 10) = "<SYMMETRY>"
        sLine = ReadLn(ts)
    Loop
    sLine = ReadLn(ts)
    GetValues 1, sLine, v
    Select Case Val_(v(1))
        Case 0
            oHeader.MarsSymm = False
        Case 1
            oHeader.MarsSymm = True
    End Select
    'Compartment Description
    Dim iComp As Integer
    Do Until Left(UCase(sLine), 14) = "<COMPARTMENTS>"
        sLine = ReadLn(ts)
    Loop
    sLine = ReadLn(ts)
    GetValues 1, sLine, v
    iComp = Val_(v(1))
    For i = 1 To iComp
        Dim oComp As New cCompartment
        sLine = ReadLn(ts)
        GetValues 4, sLine, v
        oComp.index = Val_(v(1))
        oComp.MainDestination = Val_(v(2))
        oComp.CompartmentType = Val_(v(3))
        oComp.Name = Val_(v(4))
        Project.Item(ProjectIndex).colCompartments.Add oComp, i
        Set oComp = Nothing
    Next i
    'Compartment Dimensions
    Dim colComp As colCompartments
    Set colComp = Project.Item(ProjectIndex).colCompartments
    Do Until Left(UCase(sLine), 24) = "<COMPARTMENT DIMENSIONS>"
        sLine = ReadLn(ts)
    Loop
    For i = 1 To iComp
        sLine = ReadLn(ts)
        GetValues 5, sLine, v
        colComp.Item(i).Length = Val_(v(2))
        colComp.Item(i).Breadth = Val_(v(3))
        colComp.Item(i).Height = Val_(v(4))
        colComp.Item(i).XStartFromAPP = Val_(v(5))
    Next i
    'Nodes
    Dim iNodes As Integer
    Dim oNode As cNode
    Dim colNodes As colNodes
    Set colNodes = Project.Item(ProjectIndex).colNodes
    Do Until Left(UCase(sLine), 7) = "<NODES>"
        sLine = ReadLn(ts)
    Loop
    sLine = ReadLn(ts)
    GetValues 1, sLine, v
    iNodes = Val_(v(1))
    For i = 1 To iNodes
        Set oNode = New cNode
        sLine = ReadLn(ts)
        GetValues 3, sLine, v
        oNode.index = Val_(v(1))
        oNode.nNumber = Val_(v(1))
        oNode.y = Val_(v(2))
        oNode.z = -Val_(v(3))
        colNodes.Add oNode, Val_(v(1))
        Set oNode = Nothing
    Next i
    'Materials
    Dim iMat As Integer
    Dim oMat As cMaterial
    Dim colMat As colMaterials
    Set colMat = Project.Item(ProjectIndex).colMaterials
    Do Until Left(UCase(sLine), 11) = "<MATERIALS>"
        sLine = ReadLn(ts)
    Loop
    sLine = ReadLn(ts)
    GetValues 1, sLine, v
    iMat = Val_(v(1))
    Dim k As Integer
    For i = 1 To iMat
        Set oMat = New cMaterial
        sLine = ReadLn(ts)
        GetValues 4, sLine, v
        oMat.index = Val_(v(1))
        oMat.mNumber = Val_(v(1))
        oMat.Name = v(2)
        oMat.YieldStress = Val_(v(3)) * 1000000
        oMat.YoungModulus = Val_(v(4)) * 1000000
        oMat.Poisson = 0.3
        Select Case oMat.YieldStress
            Case Is = 235 * 1000000
                oMat.AllowableStress = 175 * 1000000
            Case Is = 355 * 1000000
                oMat.AllowableStress = Int(175 * 1000000 / 0.72)
            Case Else
                oMat.AllowableStress = 175 * 1000000
        End Select
        colMat.Add oMat, i
    Next i
    'Panels
    Dim iPanels As Integer
    Dim oPanel As cPanel
    Dim Y0 As Double, Z0 As Double, y1 As Double, z1 As Double
    Dim colPanel As colPanel
    Dim iStrakes As Integer
    Dim oStrake As cStrake
    Dim colStrakes As colStrakes
    Set colPanel = Project.Item(ProjectIndex).colPanel
    Do Until Left(UCase(sLine), 15) = "<STIFFENPANELS>"
        sLine = ReadLn(ts)
    Loop
    sLine = ReadLn(ts)
    GetValues 1, sLine, v
    iPanels = Val_(v(1))
    Project.Item(ProjectIndex).cHeader.NETO = iPanels
    For i = 1 To iPanels
        Set oPanel = New cPanel
        oPanel.index = i
        oPanel.pNumber = i
        oPanel.pType = Plate
        Do Until Left(UCase(sLine), 10) = "<STIPANEL>"
            sLine = ReadLn(ts)
        Loop
        'Label
        GetValues 2, sLine, v
        oPanel.pLabel = v(2)
        sLine = ReadLn(ts)
        GetValues 2, sLine, v
        oPanel.cGeometry.InNode = Val_(v(1))
        oPanel.cGeometry.OutNode = Val_(v(2))
        Y0 = colNodes.Item(Val_(v(1))).y
        Z0 = colNodes.Item(Val_(v(1))).z
        y1 = colNodes.Item(Val_(v(2))).y
        z1 = colNodes.Item(Val_(v(2))).z
        GetLengthAngle Y0, Z0, y1, z1, oPanel
        'Comps on Left and Right
        sLine = ReadLn(ts)
        GetValues 2, sLine, v
        oPanel.LeftCompartment = Val_(v(1))
        oPanel.RightCompartment = Val_(v(2))
        'Strakes
        Set colStrakes = oPanel.colStrakes
        sLine = ReadLn(ts)
        GetValues 1, sLine, v
        iStrakes = Val_(v(1))
        For j = 1 To iStrakes
            Set oStrake = New cStrake
            sLine = ReadLn(ts)
            GetValues 6, sLine, v
            oStrake.index = Val_(v(1))
            oStrake.WidthRatio = Val_(v(2))
            oStrake.GrossThickness = Val_(v(3)) / 1000
            oStrake.NetThickness = Val_(v(4)) / 1000
            'oStrake.OpeningCoefficient = Val_(V(5))
            oStrake.MaterialNumber = Val_(v(6))
            colStrakes.Add oStrake, j
            Set oStrake = Nothing
        Next j
        oPanel.cScantlings.GrossThickness = Round(colStrakes.GetEquivalentGrossThickness, 5)
        oPanel.cScantlings.NetThickness = Round(colStrakes.GetEquivalentNetThickness, 5)
        oPanel.cScantlings.CorrosionThickness = Round(oPanel.cScantlings.GrossThickness - oPanel.cScantlings.NetThickness, 5)
        'Materials (I take only the material of the first strake from the panel)
        With colMat.Item(colStrakes.Item(1).MaterialNumber)
            oPanel.cMaterial.mNumber = .mNumber
            oPanel.cMaterial.index = .index
            oPanel.cMaterial.Name = .Name
            oPanel.cMaterial.YieldStress = .YieldStress
            oPanel.cMaterial.AllowableStress = .AllowableStress
            oPanel.cMaterial.YoungModulus = .YoungModulus
            oPanel.cMaterial.Poisson = .Poisson
            Select Case .Name
                Case "Steel", "Stainless steel"
                    oPanel.cMaterial.SpecificWeight = 77000
                Case "Aluminium rolled", "Aluminium extruded"
                    oPanel.cMaterial.SpecificWeight = 27200
            End Select
            'oPanel.cMaterial.SpecificWeight = 77000 ' par default
        End With
        'Primary Transversal Spacing
        sLine = ReadLn(ts)
        GetValues 1, sLine, v
        oPanel.cScantlings.cPrimaryFrames.Spacing = Val_(v(1))
        'Primary Transversal Default Values
        With oPanel.cScantlings.cPrimaryFrames
            .WebHeight = 1 / 1000
            .WebThickness = 1 / 1000
            .FlangeWidth = 1 / 1000
            .FlangeThickness = 1 / 1000
            .Side = SideLeft
        End With
        'Secondary Transversals
        Dim SP As Double, HW As Double, Tw As Double, _
        Bf As Double, Tf As Double, corr As Double
        Dim iSecTrans As Integer
        Dim oSecTrans As cSecondaryFrames
        Dim colSecTrans As colSecondaryFrames
        Set colSecTrans = oPanel.colSecondaryFrames
        sLine = ReadLn(ts)
        GetValues 1, sLine, v
        iSecTrans = Val_(v(1))
        For j = 1 To iSecTrans
            Set oSecTrans = New cSecondaryFrames
            sLine = ReadLn(ts)
            GetValues 11, sLine, v
            oSecTrans.index = Val_(v(1))
            Select Case v(2)
            Case "F"
                oSecTrans.Profile = profileFlat
            Case "B"
                oSecTrans.Profile = profileBulb
            Case "A"
                oSecTrans.Profile = profileAngle
            Case "T"
                oSecTrans.Profile = profileTBar
            Case "N"
                oSecTrans.Profile = profileNull
            End Select
            oSecTrans.CorrosionThickness = Val_(v(9))
            oSecTrans.WidthRatio = Val_(v(3))
            oSecTrans.Spacing = Val_(v(4))
            oSecTrans.WebHeight = Val_(v(5))
            oSecTrans.WebThickness = Val_(v(6)) '- Val_(v(9))
            oSecTrans.FlangeWidth = Val_(v(7))
            oSecTrans.FlangeThickness = Val_(v(8)) '- Val_(v(9))
            If oSecTrans.Profile = profileBulb Then
                HW = oSecTrans.WebHeight
                Tw = oSecTrans.WebThickness
                Bf = 0
                Tf = 0
                oSecTrans.GetTEquivalentSection HW, Tw, Bf, Tf
                oSecTrans.WebHeight = HW
                oSecTrans.WebThickness = Tw
                oSecTrans.FlangeWidth = Bf
                oSecTrans.FlangeThickness = Tf
            End If
            
            Select Case Val_(v(10))
                Case 1
                    oSecTrans.Side = SideRight
                Case 2
                    oSecTrans.Side = SideLeft
                Case Else
                    oSecTrans.Side = SideLeft
            End Select
            oSecTrans.MaterialNumber = Val_(v(11))
            colSecTrans.Add oSecTrans, j
            Set oSecTrans = Nothing
        Next j
        If oPanel.colSecondaryFrames.Count > 0 Then
            If colSecTrans.CheckSides = True Then
                MsgBox "Panel " & oPanel.pNumber & " has transversal secondary stiffeners defined on both sides." & vbCrLf & _
                        "The equivalent transversal secondary member must be checked.", vbExclamation + vbOKOnly
            End If
            colSecTrans.GetEquivalentFrames SP, HW, Tw, Bf, Tf
            oPanel.cScantlings.cSecondaryFrames.Spacing = SP
            If HW = 0 Then HW = 1
            If Tw = 0 Then Tw = 1
            If Bf = 0 Then Bf = 1
            If Tf = 0 Then Tf = 1
            oPanel.cScantlings.cSecondaryFrames.WebHeight = HW / 1000
            oPanel.cScantlings.cSecondaryFrames.WebThickness = Tw / 1000
            oPanel.cScantlings.cSecondaryFrames.FlangeWidth = Bf / 1000
            oPanel.cScantlings.cSecondaryFrames.FlangeThickness = Tf / 1000
            oPanel.cScantlings.cSecondaryFrames.Side = oPanel.colSecondaryFrames.Item(1).Side
        End If
        If oPanel.cScantlings.cSecondaryFrames.Side = SideNone Then oPanel.cScantlings.cSecondaryFrames.Side = SideLeft
        'Primary Stiffeners
        Dim iPrimSt As Integer
        Dim oPrimSt As cPrimaryStiffeners
        Dim colPrimSt As colPrimaryStiffeners
        Set colPrimSt = oPanel.colPrimaryStiffeners
        sLine = ReadLn(ts)
        GetValues 1, sLine, v
        iPrimSt = Val_(v(1))
        For j = 1 To iPrimSt
            Set oPrimSt = New cPrimaryStiffeners
            sLine = ReadLn(ts)
            GetValues 11, sLine, v
            oPrimSt.index = Val_(v(1))
            Select Case v(2)
            Case "F"
                oPrimSt.Profile = profileFlat
            Case "B"
                oPrimSt.Profile = profileBulb
            Case "A"
                oPrimSt.Profile = profileAngle
            Case "T"
                oPrimSt.Profile = profileTBar
            Case "N"
                oPrimSt.Profile = profileNull
            End Select
            oPrimSt.Distance = Val_(v(3))
            'Me.DistributionMode = "EE2"
            oPrimSt.CorrosionThickness = Val_(v(8)) / 1000
            oPrimSt.WebHeight = Val_(v(4)) / 1000
            oPrimSt.WebThickness = Val_(v(5)) / 1000 - oPrimSt.CorrosionThickness
            oPrimSt.FlangeWidth = Val_(v(6)) / 1000
            oPrimSt.FlangeThickness = Val_(v(7)) / 1000 - oPrimSt.CorrosionThickness
            If oPrimSt.Profile = profileBulb Then
                HW = oPrimSt.WebHeight
                Tw = oPrimSt.WebThickness
                Bf = 0
                Tf = 0
                oPrimSt.GetBVRuleBulbEquivalentSection HW, Tw, Bf, Tf
                oPrimSt.WebHeight = HW
                oPrimSt.WebThickness = Tw
                oPrimSt.FlangeWidth = Bf
                oPrimSt.FlangeThickness = Tf
            End If
            
            Select Case Val_(v(9))
                Case 1
                    oPrimSt.Side = SideRight
                Case 2
                    oPrimSt.Side = SideLeft
                Case Else
                    oPrimSt.Side = SideLeft
            End Select
            oPrimSt.MaterialNumber = Val_(v(10))
            oPrimSt.Efficiency = Val_(v(11))
            colPrimSt.Add oPrimSt, j
            Set oPrimSt = Nothing
        Next j
        oPanel.cScantlings.cPrimaryStiffeners.DistributionMode = "EE1"
        'oPanel.cScantlings.cPrimaryStiffeners.DistributionMode = "EE2"
        oPanel.cScantlings.cPrimaryStiffeners.Spacing = oPanel.cGeometry.PanelWidth
        If colPrimSt.Count > 0 Then
            colPrimSt.GetEquivalentStiffeners oPanel, oPanel.cScantlings.colGirder, SP, HW, Tw, Bf, Tf, corr
            oPanel.cScantlings.cPrimaryStiffeners.Spacing = Round(SP, 3)
            'Me.cScantlings.cPrimaryStiffeners.DistributionMode = "EE2"
            oPanel.cScantlings.cPrimaryStiffeners.WebHeight = Round(HW, 3)
            oPanel.cScantlings.cPrimaryStiffeners.WebThickness = Round(Tw, 3)
            oPanel.cScantlings.cPrimaryStiffeners.FlangeWidth = Round(Bf, 3)
            oPanel.cScantlings.cPrimaryStiffeners.FlangeThickness = Round(Tf, 3)
            oPanel.cScantlings.cPrimaryStiffeners.CorrosionThickness = Round(corr, 3)
        End If
        With oPanel.cScantlings.cPrimaryStiffeners
            If .WebHeight = 0 Then .WebHeight = 1 / 1000
            If .WebThickness = 0 Then .WebThickness = 1 / 1000
            If .FlangeWidth = 0 Then .FlangeWidth = 1 / 1000
            If .FlangeThickness = 0 Then .FlangeThickness = 1 / 1000
        End With
        If colPrimSt.Count > 0 Then
            oPanel.cScantlings.cPrimaryStiffeners.Side = colPrimSt.Item(1).Side
        Else
            oPanel.cScantlings.cPrimaryStiffeners.Side = SideLeft
        End If
        If colPrimSt.CheckSides = True Then
            MsgBox "Panel " & oPanel.pNumber & " has longitudinal stiffeners defined on both sides." & vbCrLf & _
                    "The equivalent longitudinal stiffeners must be checked.", vbExclamation + vbOKOnly
        End If
        'Secondary Stiffeners Default
        oPanel.cScantlings.cSecondaryStiffeners.Side = SideLeft
        ' Lateral pressure side, type
        oPanel.LateralPressureSide = SideRight
        oPanel.UniformLateralPressureVariation = IsExplicit
        'Panel efficiency
        sLine = ReadLn(ts)
        GetValues 1, sLine, v
        oPanel.cGeometry.Participation = Val_(v(1))
        ' Calculate equivalent panels (superposed to the Symmetry axis)
        'If Me.colNodes.Item(oPanel.cGeometry.InNode).Y = Me.Ymin And Me.colNodes.Item(oPanel.cGeometry.OutNode).Y = Me.Ymin Then
        If colNodes.Item(oPanel.cGeometry.InNode).y = 0 And _
           colNodes.Item(oPanel.cGeometry.OutNode).y = 0 And oHeader.MarsSymm = True Then
            oPanel.cScantlings.GrossThickness = oPanel.cScantlings.GrossThickness / 2
            oPanel.cScantlings.NetThickness = oPanel.cScantlings.NetThickness / 2
            oPanel.cScantlings.CorrosionThickness = oPanel.cScantlings.CorrosionThickness / 2
            oPanel.cScantlings.cSecondaryFrames.WebHeight = oPanel.cScantlings.cSecondaryFrames.WebHeight / 2
            oPanel.cScantlings.cSecondaryFrames.FlangeThickness = oPanel.cScantlings.cSecondaryFrames.FlangeThickness / 2
            oPanel.cScantlings.cSecondaryFrames.CorrosionThickness = oPanel.cScantlings.cSecondaryFrames.CorrosionThickness / 2
            oPanel.cScantlings.cPrimaryStiffeners.WebHeight = oPanel.cScantlings.cPrimaryStiffeners.WebHeight / 2
            oPanel.cScantlings.cPrimaryStiffeners.FlangeThickness = oPanel.cScantlings.cPrimaryStiffeners.FlangeThickness / 2
            oPanel.cScantlings.cPrimaryStiffeners.CorrosionThickness = oPanel.cScantlings.cPrimaryStiffeners.CorrosionThickness / 2
        End If
        oPanel.cScantlings.cPrimaryStiffeners.GrossSectionModulus = GetSectionModulus(oPanel.cScantlings.cPrimaryStiffeners.WebHeight, _
                oPanel.cScantlings.cPrimaryStiffeners.WebThickness + oPanel.cScantlings.cPrimaryStiffeners.CorrosionThickness, _
                oPanel.cScantlings.cPrimaryStiffeners.FlangeWidth, _
                oPanel.cScantlings.cPrimaryStiffeners.FlangeThickness + oPanel.cScantlings.cPrimaryStiffeners.CorrosionThickness, _
                oPanel.cScantlings.cPrimaryStiffeners.Spacing, _
                oPanel.cScantlings.cPrimaryFrames.Spacing, _
                oPanel.cScantlings.GrossThickness)
        
        'Design Variables
        Dim oDes As cDesignVariables
        Dim colDes As colDesignVariables
        Set colDes = oPanel.colDesignVariables
'        For j = 1 To 9
'            Set oDes = New cDesignVariables
'            oDes.index = j
'            oDes.VariableName = j
'            colDes.Add oDes, j
'        Next j
        ''Default Design Variables
        ''------------------------
'        Dim v() As Variant
        GetScantlings oPanel.cScantlings, v
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

        colPanel.Add oPanel, i
        Set oPanel = Nothing
    Next i
    
    UpdateBoundary ProjectIndex
    'Generate boundary conditions in the Symmetry axis
    For Each oPanel In colPanel
        If oPanel.colBoundaryConditions.Count = 1 Then
            If oHeader.MarsSymm = True Then
                If colNodes.Item(oPanel.cGeometry.InNode).y = 0 Or colNodes.Item(oPanel.cGeometry.OutNode).y = 0 Then
                    oPanel.colBoundaryConditions.Item(oPanel.colBoundaryConditions.Count).BoundaryCondition = SymmetryAxis1
                End If
            End If
        End If
    Next oPanel
    UpdatePanelConnections ProjectIndex
    For Each oPanel In colPanel
        oPanel.cCostCAtMain.SetFirstPanelData oPanel.pNumber, ProjectIndex
    Next oPanel
    ComputeFirstFractionnements ProjectIndex
    
    
    'READ LOADS
    'Load Cases
    Do Until Left(UCase(sLine), 12) = "<LOAD CASES>"
        sLine = ReadLn(ts)
    Loop
    Dim oLoadCase As cMarsLoadCase
    Dim iLoadCases As Integer
    
    Set oLoadCase = New cMarsLoadCase
    oLoadCase.index = 0
    oLoadCase.Label = "Ps"
    Project.Item(ProjectIndex).cHeader.cMarsLoads.colMarsLoadCase.Add oLoadCase, oLoadCase.index
    Set oLoadCase = Nothing

    sLine = ReadLn(ts)
    GetValues 1, sLine, v
    iLoadCases = v(1)
    For i = 1 To iLoadCases
        sLine = ReadLn(ts)
        GetValues 2, sLine, v
        Set oLoadCase = New cMarsLoadCase
        oLoadCase.index = v(1)
        oLoadCase.Label = v(2)
        Project.Item(ProjectIndex).cHeader.cMarsLoads.colMarsLoadCase.Add oLoadCase, oLoadCase.index
        Set oLoadCase = Nothing
    Next i
    'Draughts
    Do Until Left(UCase(sLine), 10) = "<DRAUGHTS>"
        sLine = ReadLn(ts)
    Loop
    Dim iDraughts As Integer
    sLine = ReadLn(ts)
    GetValues 1, sLine, v
    iDraughts = v(1)
    Dim oDraught As cDouble
    For i = 1 To iDraughts
        sLine = ReadLn(ts)
        GetValues 3, sLine, v
        Set oDraught = New cDouble
        oDraught.index = v(1)
        oDraught.Label = v(2)
        oDraught.Value = Val_(v(3))
        Project.Item(ProjectIndex).cHeader.cMarsLoads.colDraught.Add oDraught, oDraught.index
        Set oDraught = Nothing
    Next i
    'Load Types
    Do Until Left(UCase(sLine), 7) = "<LOADS>"
        sLine = ReadLn(ts)
    Loop
    Dim iLoadTypes As Integer
    sLine = ReadLn(ts)
    GetValues 1, sLine, v
    iLoadTypes = v(1)
    Dim oLoadType As cMarsLoadType
    Dim ocolLoadType As New colMarsLoadType
    For i = 1 To iLoadTypes
        sLine = ReadLn(ts)
        GetValues 3, sLine, v
        Set oLoadType = New cMarsLoadType
        oLoadType.index = v(1)
        oLoadType.Label = v(2)
        oLoadType.RelatedDraught = v(3)
        ocolLoadType.Add oLoadType.Clone, oLoadType.index
        Project.Item(ProjectIndex).cHeader.cMarsLoads.colMarsLoadType.Add oLoadType.Clone, oLoadType.index
        Set oLoadType = Nothing
    Next i
    For Each oLoadCase In Project.Item(ProjectIndex).cHeader.cMarsLoads.colMarsLoadCase
        Set oLoadCase.colMarsLoadType = ocolLoadType.Clone
    Next oLoadCase
    
    
    'Bending Moment and Shear Force
    Do Until Left(UCase(sLine), 34) = "<BENDING MOMENTS AND SHEAR FORCES>"
        sLine = ReadLn(ts)
    Loop
    sLine = ReadLn(ts)
    GetValues 2, sLine, v
    Project.Item(ProjectIndex).cHeader.cMarsLoads.StillWaterBendingMomentHogg = Val_(v(1))
    Project.Item(ProjectIndex).cHeader.cMarsLoads.VerticalWaveBendingMomentHogg = Val_(v(2))
    sLine = ReadLn(ts)
    GetValues 2, sLine, v
    Project.Item(ProjectIndex).cHeader.cMarsLoads.StillWaterBendingMomentSagg = Val_(v(1))
    Project.Item(ProjectIndex).cHeader.cMarsLoads.VerticalWaveBendingMomentSagg = Val_(v(2))
    sLine = ReadLn(ts)
    GetValues 1, sLine, v
    Project.Item(ProjectIndex).cHeader.cMarsLoads.HorizontalWaveBendingMoment = Val_(v(1))
    sLine = ReadLn(ts)
    GetValues 3, sLine, v
    Project.Item(ProjectIndex).cHeader.cMarsLoads.VerticalStillShear = Val_(v(1))
    Project.Item(ProjectIndex).cHeader.cMarsLoads.VerticalWaveShearPositive = Val_(v(2))
    Project.Item(ProjectIndex).cHeader.cMarsLoads.VerticalWaveShearNegative = Val_(v(3))
    'Combination Factors
    Do Until Left(UCase(sLine), 21) = "<COMBINATION FACTORS>"
        sLine = ReadLn(ts)
    Loop
    For Each oLoadCase In Project.Item(ProjectIndex).cHeader.cMarsLoads.colMarsLoadCase
        If oLoadCase.index > 0 Then
            sLine = ReadLn(ts)
            GetValues 2, sLine, v
            oLoadCase.CF_VerticalWaveBendingMoment_Shear = Val_(v(1))
            oLoadCase.CF_HorizontalWaveBendingMoment = Val_(v(2))
        End If
    Next oLoadCase
    'Partial Safety Factors
    Do Until Left(UCase(sLine), 24) = "<PARTIAL SAFETY FACTORS>"
        sLine = ReadLn(ts)
    Loop
    sLine = ReadLn(ts)
    GetValues 2, sLine, v
    Project.Item(ProjectIndex).cHeader.cMarsLoads.GammaStillGlobal = Val_(v(1))
    Project.Item(ProjectIndex).cHeader.cMarsLoads.GammaWaveGlobal = Val_(v(2))
    sLine = ReadLn(ts)
    GetValues 2, sLine, v
    Project.Item(ProjectIndex).cHeader.cMarsLoads.GammaStillLocal = Val_(v(1))
    Project.Item(ProjectIndex).cHeader.cMarsLoads.GammaWaveLocal = Val_(v(2))
    'Panel Data
    Dim CompIndex As Integer, LoadTypeIndex As Integer
    For Each oPanel In Project.Item(ProjectIndex).colPanel
        Set oPanel.colMarsLoadCase = Project.Item(ProjectIndex).cHeader.cMarsLoads.colMarsLoadCase.Clone
    Next oPanel
    For Each oPanel In Project.Item(ProjectIndex).colPanel
        Do Until Left(UCase(sLine), 10) = "<STIPANEL>"
            sLine = ReadLn(ts)
        Loop
        sLine = ReadLn(ts)
        sLine = ReadLn(ts)
        GetValues 1, sLine, v
        iLoadTypes = v(1)
        For i = 1 To iLoadTypes
            sLine = ReadLn(ts)
            GetValues 2, sLine, v
            CompIndex = v(1)
            LoadTypeIndex = v(2)
            Select Case CompIndex
                Case Is = oPanel.LeftCompartment
                    sLine = ReadLn(ts)
                    GetValues oPanel.colMarsLoadCase.Count, sLine, v
                    For Each oLoadCase In Project.Item(ProjectIndex).colPanel.Item(oPanel.index).colMarsLoadCase
                        oLoadCase.colMarsLoadType.Item(LoadTypeIndex).LeftCompPress.In_Node = Val_(v(oLoadCase.index + 1))
                    Next oLoadCase
                    sLine = ReadLn(ts)
                    GetValues oPanel.colMarsLoadCase.Count, sLine, v
                    For Each oLoadCase In Project.Item(ProjectIndex).colPanel.Item(oPanel.index).colMarsLoadCase
                        oLoadCase.colMarsLoadType.Item(LoadTypeIndex).LeftCompPress.Out_Node = Val_(v(oLoadCase.index + 1))
                    Next oLoadCase
                Case Is = oPanel.RightCompartment
                    sLine = ReadLn(ts)
                    GetValues oPanel.colMarsLoadCase.Count, sLine, v
                    For Each oLoadCase In Project.Item(ProjectIndex).colPanel.Item(oPanel.index).colMarsLoadCase
                        oLoadCase.colMarsLoadType.Item(LoadTypeIndex).RightCompPress.In_Node = Val_(v(oLoadCase.index + 1))
                    Next oLoadCase
                    sLine = ReadLn(ts)
                    GetValues oPanel.colMarsLoadCase.Count, sLine, v
                    For Each oLoadCase In Project.Item(ProjectIndex).colPanel.Item(oPanel.index).colMarsLoadCase
                        oLoadCase.colMarsLoadType.Item(LoadTypeIndex).RightCompPress.Out_Node = Val_(v(oLoadCase.index + 1))
                    Next oLoadCase
            End Select
        Next i
    Next oPanel
    Exit Function
OpenMarsVer100Err:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modFileOpenMars: Function OpenMarsVer100")
    ts.Close
End Function
