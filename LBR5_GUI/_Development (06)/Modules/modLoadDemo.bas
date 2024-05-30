Attribute VB_Name = "modLoadDemo"
Option Explicit

Public Function LoadDemo() As Boolean
    Dim oNode As cNode
    Dim oPan As cPanel
    Dim oLoadCase As cLoadCase
    Dim oBound As cBoundaryConditions
    Dim oDes As cDesignVariables
    Dim oStr As cStructuralConstraints
    Dim oAssPt As cAssessmentPoint
    Dim oGeo As cGeometricalConstraints
    Dim oEqu As cEqualityRestrictions
    
    Dim i As Integer, j As Integer, k As Integer
    Dim fso As New FileSystemObject
    If fso.FolderExists(App.Path & "\demo") = False Then
        fso.CreateFolder App.Path & "\demo"
    End If
    Set fso = Nothing
    
    Project.OpenProject
    With Project.Item(lProjectCount)
    
        .frmProject.Caption = "demo_sample.lbr"
        .sFileName = App.Path & "\demo\demo_sample" & ".lbr"
        .FileVersionNumber = VersionNumber
        .bNewProjectFirstSave = True
        AddtoMRU (.sFileName)
        UpdateRecentList
        
        'model length
        .cHeader.Width = 29
        .cHeader.DIS1 = 0
        .cHeader.DIS2 = .cHeader.Width / 8
        .cHeader.DIS3 = .cHeader.Width / 4
        .cHeader.DIS4 = 3 * .cHeader.Width / 8
        .cHeader.DIS5 = .cHeader.Width / 2
                
        'opti
        .cHeader.IOPTI = yes
        .cHeader.ITERAM = 10
        .cHeader.ICOUT = MinimalWeight
        .cHeader.IANA = 1
        .cHeader.JLPH = -5
        
        'header load cases
        Set oLoadCase = New cLoadCase
        oLoadCase.index = 1
        oLoadCase.Title = "HOGGING"
        oLoadCase.state = IsOn
        oLoadCase.VerticalBendingMomentFore = 75000 * 1000 'N * m
        .cHeader.colLoadCase.Add oLoadCase.Clone, 1
        Set oLoadCase = Nothing
        Set oLoadCase = New cLoadCase
        oLoadCase.index = 2
        oLoadCase.Title = "SAGGING"
        oLoadCase.state = IsOn
        oLoadCase.VerticalBendingMomentFore = -45000 * 1000 'N * m
        .cHeader.colLoadCase.Add oLoadCase.Clone, 2
        Set oLoadCase = Nothing
        'nodes
        Dim coord(1 To 2, 1 To 17) As Integer
        coord(1, 1) = 0: coord(2, 1) = 0
        coord(1, 2) = 2.4: coord(2, 2) = 0
        coord(1, 3) = 5.7: coord(2, 3) = 0
        coord(1, 4) = 7.5: coord(2, 4) = -1.8
        coord(1, 5) = 7.5: coord(2, 5) = -3.4
        coord(1, 6) = 2.4: coord(2, 6) = -3.4
        coord(1, 7) = 0: coord(2, 7) = -3.4
        coord(1, 8) = 7.5: coord(2, 8) = -6.55
        coord(1, 9) = 2.4: coord(2, 9) = -6.55
        coord(1, 10) = 0: coord(2, 10) = -6.55
        coord(1, 11) = 7.5: coord(2, 11) = -9.2
        coord(1, 12) = 6.8: coord(2, 12) = -9.2
        coord(1, 13) = 2.4: coord(2, 13) = -9.2
        coord(1, 14) = 0: coord(2, 14) = -9.2
        coord(1, 15) = 6.8: coord(2, 15) = -11.8
        coord(1, 16) = 2.4: coord(2, 16) = -11.8
        coord(1, 17) = 0: coord(2, 17) = -11.8
        
        For i = 1 To 17
            Set oNode = New cNode
            oNode.index = i
            oNode.nNumber = oNode.index
            oNode.y = coord(1, i)
            oNode.z = coord(2, i)
            .colNodes.Add oNode.Clone, oNode.index
            Set oNode = Nothing
        Next i
        
        'panels
        Dim npan(1 To 2, 1 To 20)
        npan(1, 1) = 1: npan(2, 1) = 2
        npan(1, 2) = 2: npan(2, 2) = 3
        npan(1, 3) = 3: npan(2, 3) = 4
        npan(1, 4) = 4: npan(2, 4) = 5
        npan(1, 5) = 5: npan(2, 5) = 8
        npan(1, 6) = 8: npan(2, 6) = 11
        npan(1, 7) = 11: npan(2, 7) = 12
        npan(1, 8) = 12: npan(2, 8) = 15
        npan(1, 9) = 15: npan(2, 9) = 16
        npan(1, 10) = 16: npan(2, 10) = 17
        npan(1, 11) = 5: npan(2, 11) = 6
        npan(1, 12) = 6: npan(2, 12) = 7
        npan(1, 13) = 8: npan(2, 13) = 9
        npan(1, 14) = 9: npan(2, 14) = 10
        npan(1, 15) = 12: npan(2, 15) = 13
        npan(1, 16) = 13: npan(2, 16) = 14
        npan(1, 17) = 2: npan(2, 17) = 6
        npan(1, 18) = 6: npan(2, 18) = 9
        npan(1, 19) = 9: npan(2, 19) = 13
        npan(1, 20) = 13: npan(2, 20) = 16
        
        'plates
        For i = 1 To 17
            Set oPan = New cPanel
            oPan.index = i
            oPan.pNumber = oPan.index
            oPan.pType = Plate
            oPan.cGeometry.InNode = npan(1, i)
            oPan.cGeometry.OutNode = npan(2, i)
            NewPanel oPan, "Plate"
            .colPanel.Add oPan.Clone, oPan.pNumber
            Set oPan = Nothing
        Next i
        
        
        'plate scantlings
        For i = 1 To 4 'all the rest 0.005 by default
            .colPanel.Item(i).cScantlings.NetThickness = 0.007
        Next i
        
        'stiffeners & frames scantlings
        For i = 1 To 17
            If i < 5 Then
                .colPanel.Item(i).cScantlings.cPrimaryStiffeners.Side = SideLeft
                .colPanel.Item(i).cScantlings.cPrimaryStiffeners.WebHeight = 0.09
                .colPanel.Item(i).cScantlings.cPrimaryStiffeners.WebThickness = 0.006
                .colPanel.Item(i).cScantlings.cPrimaryStiffeners.FlangeWidth = 0.03
                .colPanel.Item(i).cScantlings.cPrimaryStiffeners.FlangeThickness = 0.01
                .colPanel.Item(i).cScantlings.cPrimaryFrames.Side = SideLeft
                .colPanel.Item(i).cScantlings.cPrimaryFrames.WebHeight = 0.4
                .colPanel.Item(i).cScantlings.cPrimaryFrames.WebThickness = 0.006
                .colPanel.Item(i).cScantlings.cPrimaryFrames.FlangeWidth = 0.1
                .colPanel.Item(i).cScantlings.cPrimaryFrames.FlangeThickness = 0.01
                
            Else
                .colPanel.Item(i).cScantlings.cPrimaryStiffeners.Side = SideLeft
                .colPanel.Item(i).cScantlings.cPrimaryStiffeners.WebHeight = 0.05
                .colPanel.Item(i).cScantlings.cPrimaryStiffeners.WebThickness = 0.005
                .colPanel.Item(i).cScantlings.cPrimaryStiffeners.FlangeWidth = 0.02
                .colPanel.Item(i).cScantlings.cPrimaryStiffeners.FlangeThickness = 0.01
                .colPanel.Item(i).cScantlings.cPrimaryFrames.Side = SideLeft
                .colPanel.Item(i).cScantlings.cPrimaryFrames.WebHeight = 0.2
                .colPanel.Item(i).cScantlings.cPrimaryFrames.WebThickness = 0.006
                .colPanel.Item(i).cScantlings.cPrimaryFrames.FlangeWidth = 0.1
                .colPanel.Item(i).cScantlings.cPrimaryFrames.FlangeThickness = 0.01
            End If
            .colPanel.Item(i).cScantlings.cPrimaryStiffeners.Spacing = 0.3
            .colPanel.Item(i).cScantlings.cPrimaryFrames.Spacing = 1.2
        Next i
        
        'beams
        For i = 18 To 20
            Set oPan = New cPanel
            oPan.index = i
            oPan.pNumber = oPan.index
            oPan.pType = Beam
            oPan.cScantlings.BeamSection = bsSquare
            oPan.cGeometry.InNode = npan(1, i)
            oPan.cGeometry.OutNode = npan(2, i)
            NewPanel oPan, "Beam"
            .colPanel.Add oPan.Clone, oPan.pNumber
            Set oPan = Nothing
        Next i
        
        'beam scantlings
        For i = 18 To 20
            .colPanel.Item(i).cScantlings.NetThickness = 0.01
            .colPanel.Item(i).cScantlings.cPrimaryFrames.WebHeight = 0.2
            .colPanel.Item(i).cScantlings.cPrimaryFrames.Spacing = 4.8333
        Next i
        
        'materials
        For i = 1 To 20
            .colPanel.Item(i).cMaterial.YoungModulus = 70000 * 1000000#
            If .colPanel.Item(i).pType <> Beam Then
                .colPanel.Item(i).cMaterial.Poisson = 0.33
            End If
            .colPanel.Item(i).cMaterial.YieldStress = 100 * 1000000#
            .colPanel.Item(i).cMaterial.AllowableStress = 80 * 1000000#
            .colPanel.Item(i).cMaterial.SpecificWeight = 2.75 * 10000#
        Next i
        
        'pressures
        For i = 1 To .cHeader.colLoadCase.Count
            .colPanel.Item(1).colLoadCase.Item(i).LateralPressureIn = 3#
            .colPanel.Item(1).colLoadCase.Item(i).LateralPressureOut = 3#
            .colPanel.Item(2).colLoadCase.Item(i).LateralPressureIn = 3#
            .colPanel.Item(2).colLoadCase.Item(i).LateralPressureOut = 3#
            .colPanel.Item(3).colLoadCase.Item(i).LateralPressureIn = 3#
            .colPanel.Item(3).colLoadCase.Item(i).LateralPressureOut = 2
            .colPanel.Item(4).colLoadCase.Item(i).LateralPressureIn = 2
            .colPanel.Item(4).colLoadCase.Item(i).LateralPressureOut = 0
            .colPanel.Item(11).colLoadCase.Item(i).LateralPressureIn = 0.5
            .colPanel.Item(11).colLoadCase.Item(i).LateralPressureOut = 0.5
            .colPanel.Item(12).colLoadCase.Item(i).LateralPressureIn = 0.5
            .colPanel.Item(12).colLoadCase.Item(i).LateralPressureOut = 0.5
            
            .colPanel.Item(13).colLoadCase.Item(i).LateralPressureIn = 0.35
            .colPanel.Item(13).colLoadCase.Item(i).LateralPressureOut = 0.35
            .colPanel.Item(14).colLoadCase.Item(i).LateralPressureIn = 0.35
            .colPanel.Item(14).colLoadCase.Item(i).LateralPressureOut = 0.35
            .colPanel.Item(15).colLoadCase.Item(i).LateralPressureIn = 0.35
            .colPanel.Item(15).colLoadCase.Item(i).LateralPressureOut = 0.35
            .colPanel.Item(16).colLoadCase.Item(i).LateralPressureIn = 0.35
            .colPanel.Item(16).colLoadCase.Item(i).LateralPressureOut = 0.35
            .colPanel.Item(9).colLoadCase.Item(i).LateralPressureIn = 0.35
            .colPanel.Item(9).colLoadCase.Item(i).LateralPressureOut = 0.35
            .colPanel.Item(10).colLoadCase.Item(i).LateralPressureIn = 0.35
            .colPanel.Item(10).colLoadCase.Item(i).LateralPressureOut = 0.35
        Next i
        
        .colPanel.Item(1).colLoadCase.Item(2).LateralPressureIn = 2
        .colPanel.Item(1).colLoadCase.Item(2).LateralPressureOut = 2
        .colPanel.Item(2).colLoadCase.Item(2).LateralPressureIn = 2
        .colPanel.Item(2).colLoadCase.Item(2).LateralPressureOut = 2
        .colPanel.Item(3).colLoadCase.Item(2).LateralPressureIn = 2
        .colPanel.Item(3).colLoadCase.Item(2).LateralPressureOut = 0
        .colPanel.Item(4).colLoadCase.Item(2).LateralPressureIn = 0
        .colPanel.Item(4).colLoadCase.Item(2).LateralPressureOut = 0
        
        'boundary conditions
        Set oBound = New cBoundaryConditions
        oBound.BoundaryCondition = SymmetryAxis1
        oBound.index = 1
        .colPanel.Item(1).colBoundaryConditions.Add oBound.Clone, 1
        .colPanel.Item(12).colBoundaryConditions.Add oBound.Clone, 1
        .colPanel.Item(14).colBoundaryConditions.Add oBound.Clone, 1
        .colPanel.Item(16).colBoundaryConditions.Add oBound.Clone, 1
        .colPanel.Item(10).colBoundaryConditions.Add oBound.Clone, 1
        Set oBound = Nothing
    
        'optimization
        
        'design variables
        For i = 1 To 17
            'plate thickness
            .colPanel.Item(i).colDesignVariables.Item(1).LowerLimit = 0.005
            .colPanel.Item(i).colDesignVariables.Item(1).UpperLimit = 0.03
            .colPanel.Item(i).colDesignVariables.Item(1).Active = True
            'frames web height
            .colPanel.Item(i).colDesignVariables.Item(2).LowerLimit = 0.1
            .colPanel.Item(i).colDesignVariables.Item(2).UpperLimit = 1#
            .colPanel.Item(i).colDesignVariables.Item(2).Active = True
            'frames web thickness
            .colPanel.Item(i).colDesignVariables.Item(3).LowerLimit = 0.005
            .colPanel.Item(i).colDesignVariables.Item(3).UpperLimit = 0.02
            .colPanel.Item(i).colDesignVariables.Item(3).Active = True
            'frames flange width
            .colPanel.Item(i).colDesignVariables.Item(4).LowerLimit = 0.05
            .colPanel.Item(i).colDesignVariables.Item(4).UpperLimit = 0.5
            .colPanel.Item(i).colDesignVariables.Item(4).Active = True
            'frames spacing
            .colPanel.Item(i).colDesignVariables.Item(5).LowerLimit = 0.5
            .colPanel.Item(i).colDesignVariables.Item(5).UpperLimit = 2#
            .colPanel.Item(i).colDesignVariables.Item(5).Active = True
            'stiff web height
            .colPanel.Item(i).colDesignVariables.Item(6).LowerLimit = 0.05
            .colPanel.Item(i).colDesignVariables.Item(6).UpperLimit = 0.3
            .colPanel.Item(i).colDesignVariables.Item(6).Active = True
            'stiff web thickness
            .colPanel.Item(i).colDesignVariables.Item(7).LowerLimit = 0.005
            .colPanel.Item(i).colDesignVariables.Item(7).UpperLimit = 0.02
            .colPanel.Item(i).colDesignVariables.Item(7).Active = True
            'stiff flange width
            .colPanel.Item(i).colDesignVariables.Item(8).LowerLimit = 0.01
            .colPanel.Item(i).colDesignVariables.Item(8).UpperLimit = 0.25
            .colPanel.Item(i).colDesignVariables.Item(8).Active = True
            'stiff flange width
            .colPanel.Item(i).colDesignVariables.Item(9).LowerLimit = 0.1
            .colPanel.Item(i).colDesignVariables.Item(9).UpperLimit = 0.5
            .colPanel.Item(i).colDesignVariables.Item(9).Active = True
        Next i
    
        'structural constraints (default for all panels)
        For i = 1 To 17
            .colPanel.Item(i).IsStructuralConstraints = yes
            Set oAssPt = New cAssessmentPoint
            oAssPt.index = 1
            oAssPt.Value = 0
            .colPanel.Item(i).colAssessmentPoints.Add oAssPt.Clone, oAssPt.index
            Set oAssPt = Nothing
            Set oAssPt = New cAssessmentPoint
            oAssPt.index = 2
            oAssPt.Value = 0.5
            .colPanel.Item(i).colAssessmentPoints.Add oAssPt.Clone, oAssPt.index
            Set oAssPt = Nothing
            Set oAssPt = New cAssessmentPoint
            oAssPt.index = 3
            oAssPt.Value = 1
            .colPanel.Item(i).colAssessmentPoints.Add oAssPt.Clone, oAssPt.index
            Set oAssPt = Nothing
            
            For j = 1 To .cHeader.colLoadCase.Count
                
                Set oStr = New cStructuralConstraints
                oStr.index = 1
                oStr.AssesmentPoint = 2
                'oStr.Point = 0.5
                oStr.Reference = 11
                oStr.Limit = 1
                oStr.Value = 80 * 1000000#
                .colPanel.Item(i).colLoadCase.Item(j).colStructuralConstraints.Add oStr.Clone, oStr.index
                Set oStr = Nothing
                
                Set oStr = New cStructuralConstraints
                oStr.index = 2
                oStr.AssesmentPoint = 2
                'oStr.Point = 0.5
                oStr.Reference = 22
                oStr.Limit = 1
                oStr.Value = 80 * 1000000#
                .colPanel.Item(i).colLoadCase.Item(j).colStructuralConstraints.Add oStr.Clone, oStr.index
                Set oStr = Nothing
                
                Set oStr = New cStructuralConstraints
                oStr.index = 3
                oStr.AssesmentPoint = 2
                'oStr.Point = 0.5
                oStr.Reference = 24
                oStr.Limit = 1
                oStr.Value = 80 * 1000000#
                .colPanel.Item(i).colLoadCase.Item(j).colStructuralConstraints.Add oStr.Clone, oStr.index
                Set oStr = Nothing
                
                Set oStr = New cStructuralConstraints
                oStr.index = 4
                oStr.AssesmentPoint = 2
                'oStr.Point = 0.5
                oStr.Reference = 4
                oStr.Limit = 1
                oStr.Value = 0
                .colPanel.Item(i).colLoadCase.Item(j).colStructuralConstraints.Add oStr.Clone, oStr.index
                Set oStr = Nothing
                
                Set oStr = New cStructuralConstraints
                oStr.index = 5
                oStr.AssesmentPoint = 2
                'oStr.Point = 0.5
                oStr.Reference = 15
                oStr.Limit = 1
                oStr.Value = 0.55
                .colPanel.Item(i).colLoadCase.Item(j).colStructuralConstraints.Add oStr.Clone, oStr.index
                Set oStr = Nothing
                
            Next j
        Next i
        
        'structural constraints (different for some panels)
        'restr 22
        For i = 1 To .cHeader.colLoadCase.Count
            .colPanel.Item(1).colLoadCase.Item(i).colStructuralConstraints.Item(2).AssesmentPoint = 3
            
            .colPanel.Item(3).colLoadCase.Item(i).colStructuralConstraints.Item(2).AssesmentPoint = 3
            .colPanel.Item(4).colLoadCase.Item(i).colStructuralConstraints.Item(2).AssesmentPoint = 1
            .colPanel.Item(6).colLoadCase.Item(i).colStructuralConstraints.Item(2).AssesmentPoint = 3
            .colPanel.Item(7).colLoadCase.Item(i).colStructuralConstraints.Item(2).AssesmentPoint = 1
            .colPanel.Item(9).colLoadCase.Item(i).colStructuralConstraints.Item(2).AssesmentPoint = 3
            .colPanel.Item(11).colLoadCase.Item(i).colStructuralConstraints.Item(2).AssesmentPoint = 1 ' 3
            .colPanel.Item(13).colLoadCase.Item(i).colStructuralConstraints.Item(2).AssesmentPoint = 1 '3
            .colPanel.Item(15).colLoadCase.Item(i).colStructuralConstraints.Item(2).AssesmentPoint = 3
            
            
        Next i
        'restr 24
        For i = 1 To .cHeader.colLoadCase.Count
            .colPanel.Item(1).colLoadCase.Item(i).colStructuralConstraints.Item(3).AssesmentPoint = 3
            .colPanel.Item(2).colLoadCase.Item(i).colStructuralConstraints.Item(3).AssesmentPoint = 1
        Next i
        
        ' restr w max
'        For i = 1 To .cHeader.colLoadCase.Count
'            .colPanel.Item(9).colLoadCase.Item(i).colStructuralConstraints.Item(1).Reference = 3
'            .colPanel.Item(9).colLoadCase.Item(i).colStructuralConstraints.Item(1).Value = 17 / 1000
'            .colPanel.Item(9).colLoadCase.Item(i).colStructuralConstraints.Item(1).AssesmentPoint = 3
'
'            .colPanel.Item(10).colLoadCase.Item(i).colStructuralConstraints.Item(1).Reference = 3
'            .colPanel.Item(10).colLoadCase.Item(i).colStructuralConstraints.Item(1).Value = 17 / 1000
'            .colPanel.Item(10).colLoadCase.Item(i).colStructuralConstraints.Item(1).AssesmentPoint = 3
'
'            .colPanel.Item(12).colLoadCase.Item(i).colStructuralConstraints.Item(1).Reference = 3
'            .colPanel.Item(12).colLoadCase.Item(i).colStructuralConstraints.Item(1).Value = 18 / 1000
'            .colPanel.Item(12).colLoadCase.Item(i).colStructuralConstraints.Item(1).AssesmentPoint = 3
'
'            .colPanel.Item(1).colLoadCase.Item(i).colStructuralConstraints.Item(1).Reference = 3
'            .colPanel.Item(1).colLoadCase.Item(i).colStructuralConstraints.Item(1).Value = -15 / 1000
'            .colPanel.Item(1).colLoadCase.Item(i).colStructuralConstraints.Item(1).Limit = -1
'            .colPanel.Item(1).colLoadCase.Item(i).colStructuralConstraints.Item(1).AssesmentPoint = 3
'
'        Next i
        
        'geometrical constraints
        For i = 1 To 17
            .colPanel.Item(i).NoOfGeomConstr = 99
            Set oGeo = New cGeometricalConstraints
            oGeo.index = 1
            oGeo.code = 1
            .colPanel.Item(i).colGeometricalConstraints.Add oGeo.Clone, 1
            Set oGeo = Nothing
        Next i
        
        'For Each cPanel In Project.Item(lProjectCount).colPanel
        For i = 1 To 20
            .colPanel.Item(i).cCostCAtMain.SetFirstPanelData i, lProjectCount
        Next i
        'Next cPanel
        ComputeFirstFractionnements lProjectCount
    
        'equality restrictions
        .cHeader.colEqualityRestrictions.AddFromString ("1  2  1  1  1")
        .cHeader.colEqualityRestrictions.AddFromString ("2  2  2  1  1")
        .cHeader.colEqualityRestrictions.AddFromString ("3  2  3  1  1")
        .cHeader.colEqualityRestrictions.AddFromString ("4  2  4  1  1")
        .cHeader.colEqualityRestrictions.AddFromString ("5  2  5  1  1")
        .cHeader.colEqualityRestrictions.AddFromString ("6  2  6  1  1")
        .cHeader.colEqualityRestrictions.AddFromString ("7  2  7  1  1")
        .cHeader.colEqualityRestrictions.AddFromString ("8  2  8  1  1")
        .cHeader.colEqualityRestrictions.AddFromString ("9  2  9  1  1")
        .cHeader.colEqualityRestrictions.AddFromString ("5  3  5  1  1")
        .cHeader.colEqualityRestrictions.AddFromString ("1  4  1  3  1")
        .cHeader.colEqualityRestrictions.AddFromString ("2  4  2  3  1")
        .cHeader.colEqualityRestrictions.AddFromString ("3  4  3  3  1")
        .cHeader.colEqualityRestrictions.AddFromString ("4  4  4  3  1")
        .cHeader.colEqualityRestrictions.AddFromString ("5  4  5  1  1")
        .cHeader.colEqualityRestrictions.AddFromString ("6  4  6  3  1")
        .cHeader.colEqualityRestrictions.AddFromString ("7  4  7  3  1")
        .cHeader.colEqualityRestrictions.AddFromString ("8  4  8  3  1")
        .cHeader.colEqualityRestrictions.AddFromString ("9  4  9  3  1")
        .cHeader.colEqualityRestrictions.AddFromString ("5  5  5  1  1")
        .cHeader.colEqualityRestrictions.AddFromString ("1  6  1  5  1")
        .cHeader.colEqualityRestrictions.AddFromString ("2  6  2  5  1")
        .cHeader.colEqualityRestrictions.AddFromString ("3  6  3  5  1")
        .cHeader.colEqualityRestrictions.AddFromString ("4  6  4  5  1")
        .cHeader.colEqualityRestrictions.AddFromString ("5  6  5  1  1")
        .cHeader.colEqualityRestrictions.AddFromString ("6  6  6  5  1")
        .cHeader.colEqualityRestrictions.AddFromString ("7  6  7  5  1")
        .cHeader.colEqualityRestrictions.AddFromString ("8  6  8  5  1")
        .cHeader.colEqualityRestrictions.AddFromString ("9  6  9  5  1")
        .cHeader.colEqualityRestrictions.AddFromString ("5  7  5  1  1")
        .cHeader.colEqualityRestrictions.AddFromString ("5  8  5  1  1")
        .cHeader.colEqualityRestrictions.AddFromString ("5  9  5  1  1")
        .cHeader.colEqualityRestrictions.AddFromString ("1  10  1  9  1")
        .cHeader.colEqualityRestrictions.AddFromString ("2  10  2  9  1")
        .cHeader.colEqualityRestrictions.AddFromString ("3  10  3  9  1")
        .cHeader.colEqualityRestrictions.AddFromString ("4  10  4  9  1")
        .cHeader.colEqualityRestrictions.AddFromString ("5  10  5  1  1")
        .cHeader.colEqualityRestrictions.AddFromString ("6  10  6  9  1")
        .cHeader.colEqualityRestrictions.AddFromString ("7  10  7  9  1")
        .cHeader.colEqualityRestrictions.AddFromString ("8  10  8  9  1")
        .cHeader.colEqualityRestrictions.AddFromString ("9  10  9  9  1")
        .cHeader.colEqualityRestrictions.AddFromString ("5  11  5  1  1")
        .cHeader.colEqualityRestrictions.AddFromString ("1  12  1  11  1")
        .cHeader.colEqualityRestrictions.AddFromString ("2  12  2  11  1")
        .cHeader.colEqualityRestrictions.AddFromString ("3  12  3  11  1")
        .cHeader.colEqualityRestrictions.AddFromString ("4  12  4  11  1")
        .cHeader.colEqualityRestrictions.AddFromString ("5  12  5  1  1")
        .cHeader.colEqualityRestrictions.AddFromString ("6  12  6  11  1")
        .cHeader.colEqualityRestrictions.AddFromString ("7  12  7  11  1")
        .cHeader.colEqualityRestrictions.AddFromString ("8  12  8  11  1")
        .cHeader.colEqualityRestrictions.AddFromString ("9  12  9  11  1")
        .cHeader.colEqualityRestrictions.AddFromString ("5  13  5  1  1")
        .cHeader.colEqualityRestrictions.AddFromString ("1  14  1  13  1")
        .cHeader.colEqualityRestrictions.AddFromString ("2  14  2  13  1")
        .cHeader.colEqualityRestrictions.AddFromString ("3  14  3  13  1")
        .cHeader.colEqualityRestrictions.AddFromString ("4  14  4  13  1")
        .cHeader.colEqualityRestrictions.AddFromString ("5  14  5  1  1")
        .cHeader.colEqualityRestrictions.AddFromString ("6  14  6  13  1")
        .cHeader.colEqualityRestrictions.AddFromString ("7  14  7  13  1")
        .cHeader.colEqualityRestrictions.AddFromString ("8  14  8  13  1")
        .cHeader.colEqualityRestrictions.AddFromString ("9  14  9  13  1")
        .cHeader.colEqualityRestrictions.AddFromString ("5  15  5  1  1")
        .cHeader.colEqualityRestrictions.AddFromString ("1  16  1  15  1")
        .cHeader.colEqualityRestrictions.AddFromString ("2  16  2  15  1")
        .cHeader.colEqualityRestrictions.AddFromString ("3  16  3  15  1")
        .cHeader.colEqualityRestrictions.AddFromString ("4  16  4  15  1")
        .cHeader.colEqualityRestrictions.AddFromString ("5  16  5  1  1")
        .cHeader.colEqualityRestrictions.AddFromString ("6  16  6  15  1")
        .cHeader.colEqualityRestrictions.AddFromString ("7  16  7  15  1")
        .cHeader.colEqualityRestrictions.AddFromString ("8  16  8  15  1")
        .cHeader.colEqualityRestrictions.AddFromString ("9  16  9  15  1")
        .cHeader.colEqualityRestrictions.AddFromString ("5  17  5  1  1")
        
    End With
    
    
    
    UpdateBoundary lProjectCount
    ZoomFull
    
End Function
