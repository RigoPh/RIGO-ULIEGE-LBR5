Attribute VB_Name = "modHullGirder"
Option Explicit
Public Const NO_SECTIONS = 501
Type Hull_Girder
    STRUCTURE_DEADWEIGHT(0 To NO_SECTIONS) As Double
    UNIFORM_DN(0 To NO_SECTIONS) As Double
    UNIFORM_UP(0 To NO_SECTIONS) As Double
    STEPWISE_DN(0 To NO_SECTIONS) As Double
    STEPWISE_UP(0 To NO_SECTIONS) As Double
    STEPWISE_DEADWEIGHT(0 To NO_SECTIONS) As Double
    SHEAR_FORCE(0 To NO_SECTIONS) As Double
    BENDING_MOMENT(0 To NO_SECTIONS) As Double
End Type
Private Hull_Girder As Hull_Girder

Public Function HullGirder(ByVal ProjectIndex As Integer, ByVal LoadCase As Integer) As Hull_Girder
    On Error GoTo HullGirderErr
    Dim dDeadWeight As Double
    Dim dUpwardPress As Double, dDownwardPress As Double, dPress As Double
    Dim dStepwisePress() As Variant
    Dim Panel As cPanel
    Dim i As Integer
    'Dim Hull_Girder As Hull_Girder
    For i = 0 To NO_SECTIONS
        Hull_Girder.STRUCTURE_DEADWEIGHT(i) = 0
        Hull_Girder.UNIFORM_DN(i) = 0
        Hull_Girder.UNIFORM_UP(i) = 0
        Hull_Girder.STEPWISE_DN(i) = 0
        Hull_Girder.STEPWISE_UP(i) = 0
        Hull_Girder.STEPWISE_DEADWEIGHT(i) = 0
        Hull_Girder.SHEAR_FORCE(i) = 0
        Hull_Girder.BENDING_MOMENT(i) = 0
    Next i
    Dim l As Double
    l = Project.Item(ProjectIndex).cHeader.Width
    For Each Panel In Project.Item(ProjectIndex).colPanel
        dDeadWeight = dDeadWeight - GetPanelWeight(ProjectIndex, Panel) '[N/m]
        'get uniform pressures
        dPress = (GetResultantUniformPressure(ProjectIndex, LoadCase, Panel)) * 10000 '[N/m]
        Select Case dPress
            Case Is < 0
                dDownwardPress = dDownwardPress + dPress
            Case Is > 0
                dUpwardPress = dUpwardPress + dPress
        End Select
        'get stepwise pressures
        If Panel.colLoadCase.Item(LoadCase).colStepWiseLateralPressure.Count > 0 Then
            Dim X As Double, iStepNo As Integer
            For i = 0 To NO_SECTIONS
                X = i * l / NO_SECTIONS
                iStepNo = GetStep(X, Panel.colLoadCase.Item(LoadCase).colStepWiseLateralPressure.Count, l)
                dStepwisePress = GetResultantStepwisePressure(ProjectIndex, LoadCase, Panel, iStepNo)
                dStepwisePress(1) = dStepwisePress(1) * 10000 '[N/m]
                Select Case dStepwisePress(1)
                    Case Is < 0
                        Hull_Girder.STEPWISE_DN(i) = Hull_Girder.STEPWISE_DN(i) + dStepwisePress(1)
                    Case Is > 0
                        Hull_Girder.STEPWISE_UP(i) = Hull_Girder.STEPWISE_UP(i) + dStepwisePress(1)
                End Select
                Hull_Girder.STEPWISE_DEADWEIGHT(i) = Hull_Girder.STEPWISE_DEADWEIGHT(i) - dStepwisePress(0)
            Next i
        End If
    Next Panel
    For i = 0 To NO_SECTIONS
        Hull_Girder.STRUCTURE_DEADWEIGHT(i) = dDeadWeight
        Hull_Girder.UNIFORM_DN(i) = dDownwardPress
        Hull_Girder.UNIFORM_UP(i) = dUpwardPress
    Next i
    If IsSymmAxis(ProjectIndex) = True Then
        'If half section, the pressures will be multiplied by 2 (to obtain full section shear)
        For i = 0 To NO_SECTIONS
            Hull_Girder.STRUCTURE_DEADWEIGHT(i) = dDeadWeight * 2
            Hull_Girder.UNIFORM_DN(i) = dDownwardPress * 2
            Hull_Girder.UNIFORM_UP(i) = dUpwardPress * 2
        Next i
    Else
        For i = 0 To NO_SECTIONS
            Hull_Girder.STRUCTURE_DEADWEIGHT(i) = dDeadWeight
            Hull_Girder.UNIFORM_DN(i) = dDownwardPress
            Hull_Girder.UNIFORM_UP(i) = dUpwardPress
        Next i
    End If
    
    Dim Diagram() As Variant
    GetDiagram Hull_Girder, l, ProjectIndex, LoadCase
    HullGirder = Hull_Girder
'    Dim s As Double
'    s = dDeadWeight * Project.Item(ProjectIndex).cHeader.Width
    
    Exit Function
HullGirderErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modHullGirder: Function Hull Girder")
End Function

Private Function GetDiagram(ByRef Hull_Girder As Hull_Girder, ByVal l As Double, ByVal ProjectIndex As Integer, ByVal LoadCase As Integer) As Variant
    On Error GoTo GetDiagramErr
    Dim sumV As Double, sumM As Double, sumForceArmM0 As Double
    Dim i As Integer
    Dim dUnit As Double
    Dim str_dead As Double, unif_up As Double, unif_dn As Double, step_dead As Double, step_up As Double, step_dn As Double
    dUnit = l / NO_SECTIONS
    For i = 1 To NO_SECTIONS
        str_dead = (Hull_Girder.STRUCTURE_DEADWEIGHT(i) / 2 + Hull_Girder.STRUCTURE_DEADWEIGHT(i - 1) / 2) * dUnit '[N]
        unif_up = (Hull_Girder.UNIFORM_UP(i) / 2 + Hull_Girder.UNIFORM_UP(i - 1) / 2) * dUnit
        unif_dn = (Hull_Girder.UNIFORM_DN(i) / 2 + Hull_Girder.UNIFORM_DN(i - 1) / 2) * dUnit
        step_dead = (Hull_Girder.STEPWISE_DEADWEIGHT(i) / 2 + Hull_Girder.STEPWISE_DEADWEIGHT(i - 1) / 2) * dUnit
        step_up = (Hull_Girder.STEPWISE_UP(i) / 2 + Hull_Girder.STEPWISE_UP(i - 1) / 2) * dUnit
        step_dn = (Hull_Girder.STEPWISE_DN(i) / 2 + Hull_Girder.STEPWISE_DN(i - 1) / 2) * dUnit
        'Sum of reactions
        sumV = sumV + str_dead + unif_up + unif_dn + step_dead + step_up + step_dn
        'sum of moments in 0
        sumForceArmM0 = sumForceArmM0 + (str_dead + unif_up + unif_dn + step_dead + step_up + step_dn) * (i * dUnit - dUnit / 2)
    Next i
    Dim V1 As Double, V2 As Double
    Dim M1 As Double, M2 As Double
    M1 = Project.Item(ProjectIndex).cHeader.colLoadCase.Item(LoadCase).VerticalBendingMomentFore
    M2 = Project.Item(ProjectIndex).cHeader.colLoadCase.Item(LoadCase).VerticalBendingMomentFore
    V2 = (M1 - M2 + sumForceArmM0) / l
    V1 = -sumV + V2
    Hull_Girder.BENDING_MOMENT(0) = M1
    Hull_Girder.SHEAR_FORCE(0) = V1
    For i = 1 To NO_SECTIONS
        str_dead = (Hull_Girder.STRUCTURE_DEADWEIGHT(i) / 2 + Hull_Girder.STRUCTURE_DEADWEIGHT(i - 1) / 2) * dUnit '[N]
        unif_up = (Hull_Girder.UNIFORM_UP(i) / 2 + Hull_Girder.UNIFORM_UP(i - 1) / 2) * dUnit
        unif_dn = (Hull_Girder.UNIFORM_DN(i) / 2 + Hull_Girder.UNIFORM_DN(i - 1) / 2) * dUnit
        step_dead = (Hull_Girder.STEPWISE_DEADWEIGHT(i) / 2 + Hull_Girder.STEPWISE_DEADWEIGHT(i - 1) / 2) * dUnit
        step_up = (Hull_Girder.STEPWISE_UP(i) / 2 + Hull_Girder.STEPWISE_UP(i - 1) / 2) * dUnit
        step_dn = (Hull_Girder.STEPWISE_DN(i) / 2 + Hull_Girder.STEPWISE_DN(i - 1) / 2) * dUnit

        Hull_Girder.SHEAR_FORCE(i) = Hull_Girder.SHEAR_FORCE(i - 1) + (str_dead + unif_up + unif_dn + step_dead + step_up + step_dn)
        Hull_Girder.BENDING_MOMENT(i) = Hull_Girder.BENDING_MOMENT(i - 1) - (Hull_Girder.SHEAR_FORCE(i) + Hull_Girder.SHEAR_FORCE(i - 1)) / 2 * dUnit
    Next i
    Exit Function
GetDiagramErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modHullGirder: Function GetDiagram")
End Function

Private Function GetStep(ByVal X As Double, ByVal NoOfSteps As Integer, ByVal l As Double) As Integer
    On Error GoTo GetStepErr
    Dim dDivision As Double
    Dim i As Integer
    'case x = L
    If X = l Then
        GetStep = NoOfSteps
        Exit Function
    End If
    'case  0 <= x < L
    dDivision = l / NoOfSteps
    For i = 1 To NoOfSteps
        If X >= dDivision * (i - 1) And X < dDivision * i Then
            GetStep = i
            Exit Function
        End If
    Next i
    Exit Function
GetStepErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modHullGirder: Function GetStep")
End Function

Public Function GetPanelWeight(ByVal ProjectIndex As Integer, ByVal Panel As cPanel) As Double
    On Error GoTo GetPanelWeightErr
    Dim dPlateWeight As Double, dPrimaryStiffWeight As Double, dPrimaryFrameWeight As Double
    Dim dSecondaryStiffWeight As Double, dSecondaryFrameWeight As Double, dGirderWeight As Double
    Dim dNoOfPrimaryStiff As Double, dNoOfPrimaryFrames As Double, dNoOfSecondaryStiff As Double, dNoOfSecondaryFrames As Double
    
    If Project.Item(ProjectIndex).cHeader.IPOIDS = no Then
        GetPanelWeight = 0
        Exit Function
    End If
    Select Case Panel.pType
        Case Plate, DoubleHull
            'Plate
            dPlateWeight = Project.Item(ProjectIndex).cHeader.Width * Panel.cScantlings.GrossThickness * Panel.cGeometry.PanelWidth * Panel.cMaterial.SpecificWeight
            'Primary Stiffeners
            Select Case Panel.cScantlings.cPrimaryStiffeners.DistributionMode
                Case "EE1"
                    dNoOfPrimaryStiff = Divide(Panel.cGeometry.PanelWidth, Panel.cScantlings.cPrimaryStiffeners.Spacing) - 1
                Case "EE2"
                    dNoOfPrimaryStiff = Divide(Panel.cGeometry.PanelWidth, Panel.cScantlings.cPrimaryStiffeners.Spacing)
            End Select
            With Panel.cScantlings.cPrimaryStiffeners
                dPrimaryStiffWeight = Project.Item(ProjectIndex).cHeader.Width * Panel.cMaterial.SpecificWeight * dNoOfPrimaryStiff * (.WebHeight * (.WebThickness + .CorrosionThickness) + .FlangeWidth * (.FlangeThickness + .CorrosionThickness))
            End With
            'Primary Frames
            dNoOfPrimaryFrames = Divide(Project.Item(ProjectIndex).cHeader.Width, Panel.cScantlings.cPrimaryFrames.Spacing)
            With Panel.cScantlings.cPrimaryFrames
                dPrimaryFrameWeight = Panel.cGeometry.PanelWidth * Panel.cMaterial.SpecificWeight * dNoOfPrimaryFrames * (.WebHeight * (.WebThickness + .CorrosionThickness) + .FlangeWidth * (.FlangeThickness + .CorrosionThickness))
            End With
            'Secondary Stiffeners
            dNoOfSecondaryStiff = Divide(Panel.cGeometry.PanelWidth, Panel.cScantlings.cSecondaryStiffeners.Spacing)
            With Panel.cScantlings.cSecondaryStiffeners
                dSecondaryStiffWeight = Project.Item(ProjectIndex).cHeader.Width * Panel.cMaterial.SpecificWeight * dNoOfSecondaryStiff * (.WebHeight * .WebThickness + .FlangeWidth * .FlangeThickness)
            End With
            'Secondary Frames
            dNoOfSecondaryFrames = Divide(Project.Item(ProjectIndex).cHeader.Width, Panel.cScantlings.cSecondaryFrames.Spacing)
            With Panel.cScantlings.cSecondaryFrames
                dSecondaryFrameWeight = Panel.cGeometry.PanelWidth * Panel.cMaterial.SpecificWeight * dNoOfSecondaryFrames * (.WebHeight * .WebThickness + .FlangeWidth * .FlangeThickness)
            End With
            'Girders
            Dim Girder As cGirder
            For Each Girder In Panel.cScantlings.colGirder
                dGirderWeight = dGirderWeight + Project.Item(ProjectIndex).cHeader.Width * Panel.cMaterial.SpecificWeight * (Girder.WebHeight * Girder.WebThickness + Girder.FlangeWidth * Girder.FlangeThickness)
            Next Girder
        Case Beam
            Dim dNoOfBeams As Double, dBeamSurface As Double, dBeamWeight As Double
            dNoOfBeams = Divide(Project.Item(ProjectIndex).cHeader.Width, Panel.cScantlings.cPrimaryFrames.Spacing)
            Select Case Panel.cScantlings.BeamSection
                Case bsCircle
                    dBeamSurface = PI * (Panel.cScantlings.cPrimaryFrames.WebHeight / 2) ^ 2 - PI * (Panel.cScantlings.cPrimaryFrames.WebHeight / 2 - (Panel.cScantlings.NetThickness + Panel.cScantlings.cPrimaryFrames.CorrosionThickness)) ^ 2
                Case bsSquare
                    dBeamSurface = Panel.cScantlings.cPrimaryFrames.WebHeight ^ 2 - (Panel.cScantlings.cPrimaryFrames.WebHeight - 2 * (Panel.cScantlings.NetThickness + Panel.cScantlings.cPrimaryFrames.CorrosionThickness)) ^ 2
                Case bsDoubleT
                    With Panel.cScantlings.cPrimaryFrames
                       dBeamSurface = (.WebHeight * (.WebThickness + .CorrosionThickness) + 2 * .FlangeWidth * (.FlangeThickness + .CorrosionThickness))
                    End With
            End Select
            dBeamWeight = Panel.cGeometry.PanelWidth * Panel.cMaterial.SpecificWeight * dBeamSurface * dNoOfBeams
    End Select
    'Total Weight [N/m]
    GetPanelWeight = (dPlateWeight + dBeamWeight + dPrimaryStiffWeight + dPrimaryFrameWeight + dSecondaryStiffWeight + dSecondaryFrameWeight + dGirderWeight) / Project.Item(ProjectIndex).cHeader.Width
    Exit Function
GetPanelWeightErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modHullGirder: Function GetPanelWeigh")
End Function

Private Function GetResultantUniformPressure(ByVal ProjectIndex As Integer, ByVal LoadCase As Integer, ByVal Panel As cPanel) As Double
    On Error GoTo GetResultantUniformPressureErr
    'Needed values are those of the pressure vertical projection
    Dim dPressSign As Integer, dPressSide As Integer, dPressValue As Double
    Dim dPressIn As Double, dPressOut As Double
    dPressIn = Panel.colLoadCase.Item(LoadCase).LateralPressureIn
    dPressOut = Panel.colLoadCase.Item(LoadCase).LateralPressureOut
    GetResultantUniformPressure = (dPressIn / 2 + dPressOut / 2) * Panel.cGeometry.PanelWidth
    dPressSign = 1
    Select Case Panel.LateralPressureSide
        Case SideLeft
            dPressSide = -1
        Case SideRight
            dPressSide = 1
        Case SideNone
            dPressSide = 0
    End Select
    Dim dPressAngle As Double
    dPressAngle = (Panel.cGeometry.PanelAngle + 90) * dPressSign * -dPressSide
    Dim dSin As Double
    dSin = -Sin(dPressAngle * PI / 180)
    GetResultantUniformPressure = GetResultantUniformPressure * dSin
    Exit Function
GetResultantUniformPressureErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modHullGirder: Function GetResultantUniformPressure")
End Function

Private Function GetResultantStepwisePressure(ByVal ProjectIndex As Integer, ByVal LoadCase As Integer, ByVal Panel As cPanel, ByVal step_no As Integer) As Variant
    On Error GoTo GetResultantStepwisePressureErr
    'Needed values are those of the pressure vertical projection
    Dim dPressSign As Integer, dPressSide As Integer, dPressValue As Double
    Dim dPressIn As Double, dPressOut As Double, dPressDead As Double
    dPressIn = Panel.colLoadCase.Item(LoadCase).colStepWiseLateralPressure.Item(step_no).LateralPressureIn
    dPressOut = Panel.colLoadCase.Item(LoadCase).colStepWiseLateralPressure.Item(step_no).LateralPressureOut
    dPressDead = Panel.colLoadCase.Item(LoadCase).colStepWiseLateralPressure.Item(step_no).VerticalGravityLoad * Panel.cGeometry.PanelWidth
    GetResultantStepwisePressure = (dPressIn / 2 + dPressOut / 2) * Panel.cGeometry.PanelWidth
    dPressSign = 1
    Select Case Panel.LateralPressureSide
        Case SideLeft
            dPressSide = -1
        Case SideRight
            dPressSide = 1
        Case SideNone
            dPressSide = 0
    End Select
    Dim dPressAngle As Double
    dPressAngle = (Panel.cGeometry.PanelAngle + 90) * dPressSign * -dPressSide
    Dim dSin As Double
    dSin = -Sin(dPressAngle * PI / 180)
    GetResultantStepwisePressure = Array(dPressDead, GetResultantStepwisePressure * dSin)
    Exit Function
GetResultantStepwisePressureErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modHullGirder: Function GetResultantStepwisePressure")
End Function

