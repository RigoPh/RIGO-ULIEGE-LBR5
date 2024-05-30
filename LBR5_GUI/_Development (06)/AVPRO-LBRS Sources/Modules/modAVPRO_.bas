Attribute VB_Name = "modAVPRO_"
Option Explicit
Dim header As cHeader
Dim cLoadCase As cLoadCase, oLoadCase As cAVPROLoadCase
Dim cPanel As cPanel, oPanel As cAVPROPanel
Dim sFileVer As String

Public Sub Read_AVPRO(oProject As cAVPROProject)
    Dim i As Integer, j As Integer
    Set header = Project.Item(lProjectCount).cHeader
    'header.IANA = 2
    header.Title = oProject.Name
    header.Width = oProject.GeneralData.Length
    sFileVer = "1.0.0"
    Project.Item(lProjectCount).FileVersionNumber = sFileVer
    ' Sections to analyse (by default)
    header.DIS1 = 0
    header.DIS2 = header.Width / 8
    header.DIS3 = header.Width / 4
    header.DIS4 = 3 / 8 * header.Width
    header.DIS5 = header.Width / 2

    ' Specific weight (1 by default)
    header.IPOIDS = 1
        
    ' Load Cases
    For i = 1 To oProject.GeneralData.NoOfLoadCases
        Set cLoadCase = New cLoadCase
        Set oLoadCase = oProject.GeneralData.m_LoadCases.Item(i)
        cLoadCase.index = oLoadCase.index
        cLoadCase.Title = oLoadCase.Name
        cLoadCase.state = IsOn
        cLoadCase.VerticalBendingMomentAft = oLoadCase.AftBendingMoment
        cLoadCase.VerticalBendingMomentFore = oLoadCase.ForeBendingMoment
        header.colLoadCase.Add cLoadCase, cLoadCase.index
        Set cLoadCase = Nothing
        Set oLoadCase = Nothing
    Next i
    header.IsBendingMoments = yes
    
    ' Panels
    Dim y1 As Double, z1 As Double, y2 As Double, z2 As Double
    Dim angle As Double, Width As Double
    
    For i = 1 To oProject.NoOfPanels
         Set cPanel = New cPanel
         Set oPanel = oProject.m_Panels.Item(i)
         cPanel.pType = Plate
         'cPanel.cCostCAtMain.SetFirstPanelData cPanel
         cPanel.pNumber = i
         cPanel.index = i
         cPanel.pLabel = oPanel.Surface
         ' Panel Geometry
         y1 = oPanel.StartPoint.y
         z1 = -oPanel.StartPoint.z
         y2 = oPanel.EndPoint.y
         z2 = -oPanel.EndPoint.z
         GetLengthAngle y1, z1, y2, z2, cPanel
         ' Panel Conections
         Dim col As New Collection
         For j = 1 To oPanel.NoOfConnectedPanels
         col.Add oPanel.getConnectedPanel(j)
         Next j
         For j = oPanel.NoOfConnectedPanels + 1 To 10
         col.Add 0
         Next j
         Set cPanel.colConnections = col
         Set col = Nothing
         ' Boundary conditions
         For j = 1 To oPanel.NoOfBoundaryConditions
             Dim cBoundary As New cBoundaryConditions
             cBoundary.index = j
             cBoundary.BoundaryCondition = oPanel.Boundary(j)
             cPanel.colBoundaryConditions.Add cBoundary, j
             Set cBoundary = Nothing
         Next j
        ' Panel Scantlings
        cPanel.cScantlings.NetThickness = oPanel.Thickness
        ' Frame Scantlings
        With cPanel.cScantlings.cPrimaryFrames
            Select Case oPanel.SideFrame
                Case 0
                    .Side = SideLeft
                Case 1
                    .Side = SideRight
            End Select
            .Spacing = oPanel.Frame.Spacing
            .WebHeight = oPanel.Frame.Scantling.HWeb
            .WebThickness = oPanel.Frame.Scantling.TWeb
            .FlangeWidth = oPanel.Frame.Scantling.HFla
            .FlangeThickness = oPanel.Frame.Scantling.TFla
        End With
        ' Stiffener Scantlings
        With cPanel.cScantlings.cPrimaryStiffeners
            Select Case oPanel.SideStiffener
                Case 0
                    .Side = SideLeft
                Case 1
                    .Side = SideRight
            End Select
            Select Case oPanel.RepartitionMode
                Case 1
                    .DistributionMode = "EE1"
                Case 2
                    .DistributionMode = "EE2"
            End Select
            .Spacing = oPanel.Stiffener.Spacing
            .WebHeight = oPanel.Stiffener.Scantling.HWeb
            .WebThickness = oPanel.Stiffener.Scantling.TWeb
            .FlangeWidth = oPanel.Stiffener.Scantling.HFla
            .FlangeThickness = oPanel.Stiffener.Scantling.TFla
        End With
        ' Girder Scantlings
        For j = 1 To oPanel.NoOfGirders
            Dim cGirder As New cGirder
            Dim oGirder As New cAVPROGirder
            Set oGirder = oPanel.m_LongiGirders.Item(j)
            cGirder.Distance = oGirder.Position
            Select Case oPanel.SideGirder
                Case 0
                    cPanel.cScantlings.GirderSide = SideLeft
                Case 1
                    cPanel.cScantlings.GirderSide = SideRight
            End Select
            cGirder.WebHeight = oGirder.Scantling.HWeb
            cGirder.WebThickness = oGirder.Scantling.TWeb
            cGirder.FlangeWidth = oGirder.Scantling.HFla
            cGirder.FlangeThickness = oGirder.Scantling.TFla
            cPanel.cScantlings.colGirder.Add cGirder, j
            Set cGirder = Nothing
            Set oGirder = Nothing
        Next j
        ' Materials
        With cPanel.cMaterial
            .YoungModulus = oPanel.Material.YoungModulus
            .Poisson = oPanel.Material.Poisson
            .AllowableStress = oPanel.Material.SigAdm
            .YieldStress = oPanel.Material.SigMax
            .SpecificWeight = oPanel.Material.Density * 9.81
            .Name = oPanel.Material.Name
        End With
        'Uniform Lateral Pressures
        Select Case oPanel.SidePressure
            Case 0
                cPanel.LateralPressureSide = SideLeft
            Case 1
                cPanel.LateralPressureSide = SideRight
        End Select
        cPanel.UniformLateralPressureVariation = IsExplicit
        For j = 1 To oProject.GeneralData.NoOfLoadCases
            Set cLoadCase = New cLoadCase
            Dim oPressures As New cAVPROPressureLoad
            Set oPressures = oPanel.m_PressureLoads.Item(j)
            cLoadCase.index = j
            cLoadCase.Title = header.colLoadCase.Item(j).Title
            cLoadCase.LateralPressureIn = oPressures.StartPointPressure
            cLoadCase.LateralPressureOut = oPressures.EndPointPressure
            cPanel.colLoadCase.Add cLoadCase, j
            Set cLoadCase = Nothing
            Set oPressures = Nothing
        Next j
        'Participation coefficient
        cPanel.cGeometry.Participation = 1
        
        'Design Variables
        Dim oDes As cDesignVariables
        Dim colDes As colDesignVariables
        Set colDes = cPanel.colDesignVariables
        ''Default Design Variables
        ''------------------------
        Dim v() As Variant
        GetScantlings cPanel.cScantlings, v
        For j = 1 To 9
            Set oDes = New cDesignVariables
            oDes.index = j
            oDes.Active = False
            oDes.VariableName = j
            Select Case header.colDefaultDesignVariables.Item(j).LowerLimit
                Case Is < v(j)
                    oDes.LowerLimit = header.colDefaultDesignVariables.Item(j).LowerLimit
                Case Else
                    oDes.LowerLimit = v(j)
            End Select
            Select Case header.colDefaultDesignVariables.Item(j).UpperLimit
                Case Is > v(j)
                    oDes.UpperLimit = header.colDefaultDesignVariables.Item(j).UpperLimit
                Case Else
                    oDes.UpperLimit = v(j)
            End Select
            colDes.Add oDes, j
        Next j
        ''------------------------

        Project.Item(lProjectCount).colPanel.Add cPanel, i
        Set cPanel = Nothing
        Set oPanel = Nothing
    Next i
    NodeGenerator
    UpdateBoundary lProjectCount
    UpdatePanelConnections lProjectCount
    ByDefaultParameters header
    For Each cPanel In Project.Item(lProjectCount).colPanel
        cPanel.cCostCAtMain.SetFirstPanelData cPanel.pNumber, lProjectCount
    Next cPanel
    ComputeFirstFractionnements lProjectCount
End Sub

Public Sub ByDefaultParameters(ByVal header As cHeader)

    ' ========================
    ' ==== HEADER SECTION ====
    ' ========================

    ' OUPUT FILE
    header.IMPR = 0
    header.IMPR2 = -2
    header.INDAIG = 1
    header.INDRAID = 1
    header.DESSIN = 1
    header.JLPH = 5
    header.JLBORD = 0                        ' Must change JLBORD with IBUSC (everywhere)

    ' OVERALL OPTIMISATION PARAMETERS

    header.IOPTI = 0
    header.ITERAM = 10

    ' COST OBJECTIVE FUNCTION
    header.ICOUT = 0

    header.cCostData.REND = 1
    header.cCostData.EQP = 0.105

    header.cCostData.E0 = 0.012
    header.cCostData.E0X = 0.012
    header.cCostData.E0Y = 0.012

    header.cCostData.C1 = 2.5
    header.cCostData.C2 = 3.2
    header.cCostData.C3 = 3
    header.cCostData.DC1 = 0

    header.cCostData.DW2 = 0
    header.cCostData.DW3 = 0

    header.cCostData.P10 = 1.025
    header.cCostData.DP10 = 0.04

    header.cCostData.p4 = 0.79
    header.cCostData.P5 = 2.77
    header.cCostData.DP4 = 0
    header.cCostData.DP5 = 0.02

    header.cCostData.P9X = 0
    header.cCostData.P9Y = 0
    header.cCostData.DP9X = 0
    header.cCostData.DP9Y = 0

    header.cCostData.P6 = 0.1
    header.cCostData.P7 = 2
    header.cCostData.BETA_X = 1
    header.cCostData.BETA_Y = 1

    header.cCostData.C8 = 15
    header.cCostData.DC8 = 0.05
    header.cCostData.ALPHA_X = 1
    header.cCostData.ALPHA_Y = 1

    header.YRED = 1
End Sub

