Attribute VB_Name = "modPanelFunctions"
Option Explicit

Public Function AddBoundary()
    On Error GoTo AddBoundaryErr
    Dim oNode1 As cNode, oNode2 As cNode
    Dim iNode1 As Integer, iNode2 As Integer
    'Find selected Node
    For Each oNode1 In Project.Item(ActiveProject).colNodes
        If oNode1.Selected = isSelected Then
            iNode1 = oNode1.index
            Exit For
        End If
    Next oNode1
    'Exit if no node selected
    If iNode1 = 0 Then Exit Function
    'Detect existing virtual panels in node
    Dim oPan As cPanel
    For Each oPan In Project.Item(ActiveProject).colPanel
        If oPan.cGeometry.InNode = iNode1 Or oPan.cGeometry.OutNode = iNode1 Then
            If oPan.cGeometry.PanelWidth <= 0.002 Then
                MsgBox "Virtual panel already defined in this node.", vbInformation + vbOKOnly
                Project.Item(ActiveProject).colNodes.Item(iNode1).Selected = IsUnselected
                Exit Function
            End If
        End If
    Next oPan
    Set oNode1 = Project.Item(ActiveProject).colNodes.Item(iNode1)
    'Add new node
    Set oNode2 = New cNode
    iNode2 = Project.Item(ActiveProject).colNodes.Count + 1
    oNode2.index = iNode2
    oNode2.nNumber = iNode2
    oNode2.z = oNode1.z
    oNode2.y = oNode1.y - 0.002
    Project.Item(ActiveProject).colNodes.Add oNode2, iNode2
    
    'Test intersection with other panels
    Dim y1 As Double, z1 As Double, y2 As Double, z2 As Double, Angle2 As Double
    Dim y3 As Double, z3 As Double, y4 As Double, z4 As Double
    Dim iNodeTmp As Integer
    y1 = Project.Item(ActiveProject).colNodes.Item(iNode1).y
    z1 = Project.Item(ActiveProject).colNodes.Item(iNode1).z
    y2 = Project.Item(ActiveProject).colNodes.Item(iNode2).y
    z2 = Project.Item(ActiveProject).colNodes.Item(iNode2).z
    For Each oPan In Project.Item(ActiveProject).colPanel
        y3 = Project.Item(ActiveProject).colNodes.Item(oPan.cGeometry.InNode).y
        z3 = Project.Item(ActiveProject).colNodes.Item(oPan.cGeometry.InNode).z
        y4 = Project.Item(ActiveProject).colNodes.Item(oPan.cGeometry.OutNode).y
        z4 = Project.Item(ActiveProject).colNodes.Item(oPan.cGeometry.OutNode).z
        Angle2 = oPan.cGeometry.PanelAngle
        If DetectIntersection(y1, z1, y2, z2, y3, z3, y4, z4, Angle2) = True Then
'            If oPan.cGeometry.PanelWidth <= 0.002 Then
'                Project.Item(ActiveProject).colNodes.Remove iNode2
'                Exit Function
'            Else
                Project.Item(ActiveProject).colNodes.Item(iNode2).y = Project.Item(ActiveProject).colNodes.Item(iNode1).y + 0.002
'            End If
        End If
    Next oPan

    
    
    'GENERATE VIRTUAL PANEL
    'Select second node
    'Project.Item(ActiveProject).colNodes.Item(iNode1).Selected = isSelected
    setFunctionMode ADD_PLATE_FIRST_NODE_FUNCTION
    AddPanel "Plate"

    'Unselect nodes
    Project.Item(ActiveProject).colNodes.Item(iNode1).Selected = IsUnselected
    'Project.Item(ActiveProject).colNodes.Item(iNode2).Selected = IsUnselected
    'Select second node
    Project.Item(ActiveProject).colNodes.Item(iNode2).Selected = isSelected
    setFunctionMode ADD_PLATE_SECOND_NODE_FUNCTION
    AddPanel "Plate"
    'Unselect second node
    Project.Item(ActiveProject).colNodes.Item(iNode2).Selected = IsUnselected
    'kill temp nodes
    Set oNode1 = Nothing
    Set oNode2 = Nothing
    
    'Set compatible sens for the panel
    Dim iEnter As Integer, iExit As Integer
    iEnter = 0: iExit = 0
    For Each oPan In Project.Item(ActiveProject).colPanel
        If oPan.cGeometry.InNode = iNode1 Then
            iEnter = iEnter + 1
        End If
        If oPan.cGeometry.OutNode = iNode1 Then
            iExit = iExit + 1
        End If
    Next oPan
    If iEnter = 0 Or iExit = 0 Then
        Project.Item(ActiveProject).colPanel.Item(Project.Item(ActiveProject).colPanel.Count).Reverse
        'MsgBox "reversed."
    End If

    setFunctionMode ADD_BOUNDARY
    'Set Boundary Condition (symm axis #1)
    Project.Item(ActiveProject).colPanel.Item(Project.Item(ActiveProject).colPanel.Count).colBoundaryConditions.Item(1).BoundaryCondition = SymmetryAxis1
    Project.Item(ActiveProject).colPanel.Item(Project.Item(ActiveProject).colPanel.Count).colBoundaryConditions.Item(1).Edge = OutEdge
    
    'Update Boundaries
    UpdateBoundary ActiveProject
    'Redraw
    UpdateCoordinates Project.Item(ActiveProject).frmProject.cRectWND
    Draw ActiveProject
    
    Exit Function
AddBoundaryErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modPanelFunctions: Function AddBoundary")
End Function

Public Function AddPanel(ByVal PanelType As String)
    Dim Node As cNode, Panel As cPanel
    Dim colPanel As colPanel
    Dim SelectedNode As Integer
    Static FirstNode As Integer, SecondNode As Integer
    Static FirstPanel As Integer, SecondPanel As Integer
    Static y1 As Double
    Static z1 As Double
    Dim y2 As Double, z2 As Double, Angle2 As Double
    Dim y3 As Double, z3 As Double, y4 As Double, z4 As Double
    If Project.Item(ActiveProject).ScreenMode <> NORMAL_MODE Then Exit Function
    Select Case Project.Item(ActiveProject).FunctionMode
        Case ADD_PLATE_FIRST_NODE_FUNCTION, ADD_BEAM_FIRST_NODE_FUNCTION
            For Each Node In Project.Item(ActiveProject).colNodes
                If Node.Selected = isSelected Then
                    SelectedNode = Node.nNumber
                    FirstNode = Node.nNumber
                    y1 = Node.y
                    z1 = Node.z
                    Select Case PanelType
                        Case "Plate"
                            setFunctionMode ADD_PLATE_SECOND_NODE_FUNCTION
                        Case "Beam"
                            setFunctionMode ADD_BEAM_SECOND_NODE_FUNCTION
                    End Select
                    Exit For
                End If
            Next Node
        Case ADD_PLATE_SECOND_NODE_FUNCTION, ADD_BEAM_SECOND_NODE_FUNCTION
            For Each Node In Project.Item(ActiveProject).colNodes
                If Node.Selected = isSelected Then
                    SelectedNode = Node.nNumber
                    SecondNode = Node.nNumber
                    y2 = Node.y
                    z2 = Node.z
                    Set colPanel = Project.Item(ActiveProject).colPanel
                    For Each Panel In colPanel
                        y3 = Project.Item(ActiveProject).colNodes.Item(Panel.cGeometry.InNode).y
                        z3 = Project.Item(ActiveProject).colNodes.Item(Panel.cGeometry.InNode).z
                        y4 = Project.Item(ActiveProject).colNodes.Item(Panel.cGeometry.OutNode).y
                        z4 = Project.Item(ActiveProject).colNodes.Item(Panel.cGeometry.OutNode).z
                        Angle2 = Panel.cGeometry.PanelAngle
                        If DetectIntersection(y1, z1, y2, z2, y3, z3, y4, z4, Angle2) = True Then
                            MsgBox "Intersection with another panel.", vbCritical + vbOKOnly
                            Node.Selected = IsUnselected
                            setFunctionMode NO_FUNCTION
                            setScreenMode NORMAL_MODE
                            Exit Function
                        End If
                    Next Panel
                    Set Panel = New cPanel
                    Panel.index = colPanel.Count + 1
                    Panel.pNumber = Panel.index
                    Panel.cGeometry.InNode = FirstNode
                    Panel.cGeometry.OutNode = SecondNode
                    'Panel.pType = Plate
                    NewPanel Panel, PanelType
                    colPanel.Add Panel, Panel.index
                    'UpdatePanelConnections
                    colPanel.Item(colPanel.Count).cCostCAtMain.SetFirstPanelData colPanel.Count, ActiveProject
                    UpdateBoundary ActiveProject
                    Set Panel = Nothing
                    Set colPanel = Nothing
                    Node.Selected = IsUnselected
                    If Project.Item(ActiveProject).colPanel.Count >= Licensing.MAX_PANELS Then
                        MsgBox "Maximum number of panels reached.", vbInformation + vbOKOnly
                        setFunctionMode NO_FUNCTION
                        setScreenMode NORMAL_MODE
                        Exit Function
                    End If

                    Select Case PanelType
                        Case "Plate"
                            setFunctionMode ADD_PLATE_FIRST_NODE_FUNCTION
                        Case "Beam"
                            setFunctionMode ADD_BEAM_FIRST_NODE_FUNCTION
                    End Select
                    Exit For
                End If
            Next Node
            Case ADD_DOUBLEHULL_FIRST_PANEL_FUNCTION
                For Each Panel In Project.Item(ActiveProject).colPanel
                    If Panel.Selected = isSelected Then
                        If Panel.pType <> Plate Then
                            MsgBox "Selected panel is not a plate.", vbCritical + vbOKOnly
                            Panel.Selected = IsUnselected
                            FirstPanel = 0
                            Exit Function
                        End If
                        FirstPanel = Panel.pNumber
                        setFunctionMode ADD_DOUBLEHULL_SECOND_PANEL_FUNCTION
                        Exit For
                    End If
                Next Panel
            Case ADD_DOUBLEHULL_SECOND_PANEL_FUNCTION
                For Each Panel In Project.Item(ActiveProject).colPanel
                    If Panel.Selected = isSelected Then
                        If Panel.pType <> Plate Then
                            MsgBox "Selected panel is not a plate.", vbCritical + vbOKOnly
                            Panel.Selected = IsUnselected
                            SecondPanel = 0
                            Exit Function
                        End If
                        SecondPanel = Panel.pNumber
                        Exit For
                    End If
                Next Panel
                If FirstPanel > 0 And SecondPanel > 0 Then
                    CreateDoubleHull FirstPanel, SecondPanel
                    Project.Item(ActiveProject).colPanel.Item(FirstPanel).Selected = IsUnselected
                    Project.Item(ActiveProject).colPanel.Item(SecondPanel).Selected = IsUnselected
                    FirstPanel = 0
                    SecondPanel = 0
                    setFunctionMode ADD_DOUBLEHULL_FIRST_PANEL_FUNCTION
                End If
    End Select
    UpdateBoundary ActiveProject
    Project.Item(ActiveProject).DataChanged = True
    
End Function

Private Function CreateDoubleHull(ByVal FirstPanel As Integer, ByVal SecondPanel As Integer)
    Dim colPanel As colPanel
    Set colPanel = Project.Item(ActiveProject).colPanel
    'Check Widths
    If colPanel.Item(FirstPanel).cGeometry.PanelWidth <> colPanel.Item(SecondPanel).cGeometry.PanelWidth Then
        MsgBox "Panel Widths must be Equal.", vbCritical + vbOKOnly
        Exit Function
    End If
    'Check Angles
    If colPanel.Item(FirstPanel).cGeometry.PanelAngle <> colPanel.Item(SecondPanel).cGeometry.PanelAngle Then
        MsgBox "Panel Angles must be Equal.", vbCritical + vbOKOnly
        Exit Function
    End If
    'Check Double Hull Height
    Dim y1 As Double, z1 As Double, y2 As Double, z2 As Double, _
        y3 As Double, z3 As Double, y4 As Double, z4 As Double
    Dim Yr2 As Double, Zr2 As Double, Yr1 As Double, Zr1 As Double, _
        Yr3 As Double, Zr3 As Double, Yr4 As Double, Zr4 As Double
    Dim angle As Double
    Dim DoubleHullHeight As Double
    Dim colNodes As colNodes
    Set colNodes = Project.Item(ActiveProject).colNodes
    y1 = colNodes.Item(colPanel.Item(FirstPanel).cGeometry.InNode).y
    z1 = colNodes.Item(colPanel.Item(FirstPanel).cGeometry.InNode).z
    y2 = colNodes.Item(colPanel.Item(FirstPanel).cGeometry.OutNode).y
    z2 = colNodes.Item(colPanel.Item(FirstPanel).cGeometry.OutNode).z
    y3 = colNodes.Item(colPanel.Item(SecondPanel).cGeometry.InNode).y
    z3 = colNodes.Item(colPanel.Item(SecondPanel).cGeometry.InNode).z
    y4 = colNodes.Item(colPanel.Item(SecondPanel).cGeometry.OutNode).y
    z4 = colNodes.Item(colPanel.Item(SecondPanel).cGeometry.OutNode).z
    angle = colPanel.Item(FirstPanel).cGeometry.PanelAngle
    If angle < 0 Then angle = angle + 360
    y1 = y1 - y3
    z1 = -z1 + z3
    y2 = y2 - y3
    z2 = -z2 + z3
    y4 = y4 - y3
    z4 = -z4 + z3
    y3 = 0
    z3 = 0
    Call RotateNode(y1, z1, Yr1, Zr1, angle)
    Call RotateNode(y2, z2, Yr2, Zr2, angle)
    'Call RotateNode(Y3, Z3, Yr3, Zr3, Angle)
    Call RotateNode(y4, z4, Yr4, Zr4, angle)
    DoubleHullHeight = Round(Abs(Zr3 - Zr1), 4)
    If DoubleHullHeight = 0 Then
        MsgBox "Double Hull Height cannot be Null.", vbCritical + vbOKOnly
        Exit Function
    End If
    'Check Start Points
    If Round(Yr3, 4) <> Round(Yr1, 4) Or Round(Yr2, 4) <> Round(Yr4, 4) Then
        MsgBox "Double Hull is distorted.", vbCritical + vbOKOnly
        Exit Function
    End If
    'Check Frame & Stiffener Side
    If Yr1 > Yr2 Then
        If Zr1 > Zr3 Then
            colPanel.Item(FirstPanel).cScantlings.cPrimaryFrames.Side = SideLeft
            colPanel.Item(SecondPanel).cScantlings.cPrimaryFrames.Side = SideRight
            colPanel.Item(FirstPanel).cScantlings.cPrimaryStiffeners.Side = SideLeft
            colPanel.Item(SecondPanel).cScantlings.cPrimaryStiffeners.Side = SideRight
        ElseIf Zr1 < Zr3 Then
            colPanel.Item(FirstPanel).cScantlings.cPrimaryFrames.Side = SideRight
            colPanel.Item(SecondPanel).cScantlings.cPrimaryFrames.Side = SideLeft
            colPanel.Item(FirstPanel).cScantlings.cPrimaryStiffeners.Side = SideRight
            colPanel.Item(SecondPanel).cScantlings.cPrimaryStiffeners.Side = SideLeft
        End If
    ElseIf Yr1 < Yr2 Then
        If Zr1 < Zr3 Then
            colPanel.Item(FirstPanel).cScantlings.cPrimaryFrames.Side = SideLeft
            colPanel.Item(SecondPanel).cScantlings.cPrimaryFrames.Side = SideRight
            colPanel.Item(FirstPanel).cScantlings.cPrimaryStiffeners.Side = SideLeft
            colPanel.Item(SecondPanel).cScantlings.cPrimaryStiffeners.Side = SideRight
        ElseIf Zr1 > Zr3 Then
            colPanel.Item(FirstPanel).cScantlings.cPrimaryFrames.Side = SideRight
            colPanel.Item(SecondPanel).cScantlings.cPrimaryFrames.Side = SideLeft
            colPanel.Item(FirstPanel).cScantlings.cPrimaryStiffeners.Side = SideRight
            colPanel.Item(SecondPanel).cScantlings.cPrimaryStiffeners.Side = SideLeft
        End If
    End If
    colPanel.Item(FirstPanel).pType = DoubleHull
    colPanel.Item(FirstPanel).cScantlings.cPrimaryFrames.WebHeight = DoubleHullHeight
    colPanel.Item(FirstPanel).RelatedDoubleHullPanel = SecondPanel
    colPanel.Item(SecondPanel).pType = DoubleHull
    colPanel.Item(SecondPanel).cScantlings.cPrimaryFrames.WebHeight = DoubleHullHeight
    colPanel.Item(SecondPanel).RelatedDoubleHullPanel = FirstPanel
    
    FirstPanel = 0: SecondPanel = 0
    Project.Item(ActiveProject).DataChanged = True
End Function

Public Function DeleteSelectedPanels(ProjectIndex As Integer)
    On Error GoTo DeleteSelectedPanelsErr
    Dim Panel As cPanel
    Dim msgSingleOcc As Boolean, MSG As VbMsgBoxResult
    msgSingleOcc = False
    
    For Each Panel In Project.Item(ProjectIndex).colPanel
        UpdateCostCAtDHulls Panel.pNumber, ProjectIndex
    Next Panel
    
    For Each Panel In Project.Item(ProjectIndex).colPanel
        If Panel.Selected = isSelected Then
            If Panel.pType = DoubleHull Then
                MsgBox "Double Hull Panels must be exploded before deleting.", vbCritical + vbOKOnly
                Exit For
            End If

            If msgSingleOcc = False Then
                MSG = MsgBox("Delete selected panels?", vbYesNo + vbQuestion)
                msgSingleOcc = True
            End If
            If MSG = vbYes Then
                Project.Item(ProjectIndex).colPanel.Remove Panel.pNumber
                If Project.Item(ProjectIndex).colPanelUpdate.Count >= Panel.pNumber Then Project.Item(ProjectIndex).colPanelUpdate.Remove Panel.pNumber
            Else
                Exit Function
            End If
        End If
    Next Panel
    ReNumPanels
    Project.Item(ProjectIndex).cHeader.NETO = Project.Item(ProjectIndex).colPanel.Count
    UpdateBoundary ProjectIndex
    Draw ProjectIndex
    Project.Item(ProjectIndex).DataChanged = True
    Exit Function
DeleteSelectedPanelsErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modPanelFunctions: Function DeleteSelectedPanels")
End Function

Public Function DeletePanel(ByVal ProjectIndex As Integer, ByVal y As Integer, ByVal z As Integer)
    On Error GoTo DeletePanelErr
    Dim Panel As cPanel
    Dim MSG As VbMsgBoxResult
    Dim bAnySelected As Boolean
    If Project.Item(ProjectIndex).ScreenMode <> NORMAL_MODE Then Exit Function
    For Each Panel In Project.Item(ProjectIndex).colPanel
        If PtInRegion(Panel.Region, y, z) = 1 Then
            If Panel.pType = DoubleHull Then
                MsgBox "Double Hull Panels must be exploded before deleting.", vbCritical + vbOKOnly
                Exit Function
            End If
            MSG = MsgBox("Delete Panel " & Panel.index & "?", vbYesNo + vbQuestion)
           
            If MSG = vbYes Then
                UpdateCostCAtDHulls Panel.pNumber, ProjectIndex
                UpdateCostCAtNappes Panel.pNumber, ProjectIndex
                Project.Item(ProjectIndex).colPanel.Remove Panel.pNumber
                If Project.Item(ProjectIndex).colPanelUpdate.Count >= Panel.pNumber Then Project.Item(ProjectIndex).colPanelUpdate.Remove Panel.pNumber
                Project.Item(ProjectIndex).frmProject.picScreen.Cls
                ReNumPanels
                Project.Item(ProjectIndex).cHeader.NETO = Project.Item(ProjectIndex).colPanel.Count
                UpdateBoundary ProjectIndex
                Draw ProjectIndex
                Project.Item(ProjectIndex).DataChanged = True
            End If
            Exit Function
        End If
    Next Panel
    Exit Function
DeletePanelErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modPanelFunctions: DeletePanelFromMenu")
End Function

Public Function DeletePanelFromMenu(ByVal ProjectIndex As Integer, ByVal PanelIndex As Integer)
    On Error GoTo DeletePanelFromMenuErr
    Dim Panel As cPanel
    Dim MSG As VbMsgBoxResult
    Dim bAnySelected As Boolean
    If Project.Item(ProjectIndex).ScreenMode <> NORMAL_MODE Then Exit Function
    'For Each Panel In Project.Item(ProjectIndex).colPanel
        'If PtInRegion(Panel.Region, Y, z) = 1 Then
            Set Panel = Project.Item(ProjectIndex).colPanel.Item(PanelIndex)
            If Panel.pType = DoubleHull Then
                MsgBox "Double Hull Panels must be exploded before deleting.", vbCritical + vbOKOnly
                Exit Function
            End If
            'MSG = MsgBox("Delete Panel " & Panel.index & "?", vbYesNo + vbQuestion)
           
            'If MSG = vbYes Then
                UpdateCostCAtDHulls Panel.pNumber, ProjectIndex
                UpdateCostCAtNappes Panel.pNumber, ProjectIndex
                Project.Item(ProjectIndex).colPanel.Remove Panel.pNumber
                Project.Item(ProjectIndex).frmProject.picScreen.Cls
                ReNumPanels
                Project.Item(ProjectIndex).cHeader.NETO = Project.Item(ProjectIndex).colPanel.Count
                UpdateBoundary ProjectIndex
                Draw ProjectIndex
                Project.Item(ProjectIndex).DataChanged = True
            
            'End If
            'Exit Function
        'End If
   ' Next Panel
    Exit Function
DeletePanelFromMenuErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modPanelFunctions: DeletePanelFromMenu")
End Function

Public Function UpdateCostCAtNappes(ByVal PanelIndex As Integer, ByVal ProjectIndex As Integer)
    On Error GoTo UpdateCostCAtNappesErr
    Dim cNappe As cCostCAtNappe, cIndex As cIndex, cIndex1 As cIndex
    For Each cNappe In Project.Item(ProjectIndex).colCostCAtNappe
        For Each cIndex In cNappe.Nappe
            Select Case cIndex.Number
                Case Is = PanelIndex
                    For Each cIndex1 In cNappe.Nappe
                        Project.Item(ProjectIndex).colPanel.Item(cIndex1.Number).cCostCAtMain.bIsPartOfNappe = False
                        Project.Item(ProjectIndex).colPanel.Item(cIndex1.Number).cCostCAtMain.IP_PANNEAU = ExterieurNappe
                        UpdateCostCAtNappeIDSoudure cIndex1.Number, ProjectIndex
                    Next cIndex1
                    Project.Item(ProjectIndex).colCostCAtNappe.Remove cNappe.index
            End Select
        Next cIndex
    Next cNappe
    Exit Function
UpdateCostCAtNappesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modPanelFunctions: UpdateCostCAtNappes")
End Function

Public Function UpdateCostCAtNappeIDSoudure(ByVal PanelIndex As Integer, ProjectIndex As Integer)
    On Error GoTo UpdateCostCAtNappeIDSoudureErr
    'update 'ID Soudure' for the broken 'Nappes', in the pannels that were in the interiour of the nappe (ID_Soudure = 0)
    Select Case Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cCostCAtMain.ID_PANNEAU
        Case NappePlane
            If Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cCostCAtMain.colCostCAtOperations.Item(47).Soudures = 0 Then
                Select Case Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cCostCAtMain.IT_PANNEAU
                    Case BordeSimple
                        Select Case IsPanelHorizontal(PanelIndex, ProjectIndex)
                            Case Is = True
                                Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cCostCAtMain.colCostCAtOperations.Item(47).Soudures = 14
                            Case Is = False
                                Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cCostCAtMain.colCostCAtOperations.Item(47).Soudures = 15
                        End Select
                    Case DoubleCoqueInterieure
                        Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cCostCAtMain.colCostCAtOperations.Item(47).Soudures = 52
                    Case DoubleCoqueExterieure
                        Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cCostCAtMain.colCostCAtOperations.Item(47).Soudures = 50
                End Select
            End If
        End Select
    Exit Function
UpdateCostCAtNappeIDSoudureErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modPanelFunctions: UpdateCostCAtNappeIDSoudure")
End Function

Public Function UpdateCostCAtDHulls(ByVal PanelIndex As Integer, ByVal ProjectIndex As Integer)
    On Error GoTo UpdateCostCAtDHullsErr
    Dim cDHull As cCostCAtDHull, cIndex As cIndex
    For Each cDHull In Project.Item(ProjectIndex).colCostCAtDHull
        For Each cIndex In cDHull.InnerShell
            Select Case cIndex.Number
                Case Is = PanelIndex
                    cDHull.InnerShell.Remove cIndex.index
                    cDHull.InnerShell.Renum
                Case Is > PanelIndex
                    cIndex.Number = cIndex.Number - 1
            End Select
        Next cIndex
        For Each cIndex In cDHull.OuterShell
            Select Case cIndex.Number
                Case Is = PanelIndex
                    cDHull.OuterShell.Remove cIndex.index
                    cDHull.OuterShell.Renum
                Case Is > PanelIndex
                    cIndex.Number = cIndex.Number - 1
            End Select
        Next cIndex
    Next cDHull
    For Each cDHull In Project.Item(ProjectIndex).colCostCAtDHull
        If cDHull.InnerShell.Count = 0 And cDHull.OuterShell.Count = 0 Then
            Project.Item(ProjectIndex).colCostCAtDHull.Remove cDHull.index
            Project.Item(ProjectIndex).colCostCAtDHull.Renum
        End If
    Next cDHull
    Exit Function
UpdateCostCAtDHullsErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modPanelFunctions: UpdateCostCAtDHulls")
End Function

'Public Function RenumColCostCAtDHull(ByVal ProjectIndex As Integer)
'    On Error GoTo  RenumColCostCAtDHullErr
'    Dim cDH As cCostCAtDHull, colDH As New Collection
'    Dim i As Integer
'    i = 0
'    For Each cDH In Project.Item(ProjectIndex).colCostCAtDHull
'        i = i + 1
'        cDH.Index = i
'        colDH.Add cDH
'    Next cDH
'    Set Project.Item(ProjectIndex).colCostCAtDHull = colDH
'    Exit Function
'RenumColCostCAtDHullErr:
'    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modPanelFunctions: RenumColCostCAtDHullErr")
'End Function

Public Sub NewPanel(ByVal Panel As cPanel, ByVal PanelType As String)
    Dim y1 As Double, z1 As Double, y2 As Double, z2 As Double
    Dim Node As cNode
    y1 = Project.Item(ActiveProject).colNodes.Item(Panel.cGeometry.InNode).y
    z1 = Project.Item(ActiveProject).colNodes.Item(Panel.cGeometry.InNode).z
    y2 = Project.Item(ActiveProject).colNodes.Item(Panel.cGeometry.OutNode).y
    z2 = Project.Item(ActiveProject).colNodes.Item(Panel.cGeometry.OutNode).z
    GetLengthAngle y1, z1, y2, z2, Panel
    'Panel.Visible = True
    Dim LoadCase As cLoadCase
    For Each LoadCase In Project.Item(ActiveProject).cHeader.colLoadCase
        Panel.colLoadCase.Add LoadCase, LoadCase.index
    Next LoadCase
    
    Dim MarsLoadCase As cMarsLoadCase
    For Each MarsLoadCase In Project.Item(ActiveProject).cHeader.cMarsLoads.colMarsLoadCase
        Panel.colMarsLoadCase.Add MarsLoadCase, MarsLoadCase.index '+ 1
    Next MarsLoadCase
    
    Panel.LateralPressureSide = SideRight
    Panel.UniformLateralPressureVariation = IsExplicit
    ' Panel Thickness
    Panel.cScantlings.NetThickness = 0.005
    Panel.cScantlings.GrossThickness = 0.005
    ' Panel Material
    With Panel.cMaterial
        .YoungModulus = 210000000000#
        .YieldStress = 235000000
        Select Case .YieldStress
            Case Is = 235000000
                .AllowableStress = 175000000
            Case Is = 355000000
                .AllowableStress = 175000000 / 0.72
            Case Else
                .AllowableStress = 175000000
        End Select
        '.AllowableStress = 192000000
        .SpecificWeight = 78500
    End With
    
    'Design Variables
    Dim oDes As cDesignVariables
    Dim colDes As colDesignVariables
    Dim oHeader As cHeader
    Set colDes = Panel.colDesignVariables
    Set oHeader = Project.Item(ActiveProject).cHeader
    Dim i As Integer, j As Integer
'    For i = 1 To 9
'        Set oDes = New cDesignVariables
'        oDes.Index = i
'        oDes.VariableName = i
'        colDes.Add oDes, i
'        Set oDes = Nothing
'    Next i
    
    Select Case PanelType
        Case "Plate"
            Panel.pType = Plate
            ' Panel Primary Frames
            With Panel.cScantlings.cPrimaryFrames
                .Side = SideRight
                .Spacing = Project.Item(ActiveProject).cHeader.Width / 10
                .WebHeight = 0.001
                .WebThickness = 0.001
                .FlangeWidth = 0.001
                .FlangeThickness = 0.001
            End With
            ' Panel Secondary Frames
            Panel.cScantlings.cSecondaryFrames.Side = SideLeft
            ' Panel Primary Stiffeners
            With Panel.cScantlings.cPrimaryStiffeners
                .Side = SideRight
                .Spacing = Panel.cGeometry.PanelWidth / 5
                .DistributionMode = "EE2"
                .WebHeight = 0.001
                .WebThickness = 0.001
                .FlangeWidth = 0.001
                .FlangeThickness = 0.001
            End With
            ' Panel Secondary Stiffeners
            Panel.cScantlings.cSecondaryStiffeners.Side = SideLeft
            ' Poisson Coefficient
            Panel.cMaterial.Poisson = 0.3
            ' Participation Coefficient
            Panel.cGeometry.Participation = 1
            Panel.cScantlings.cPrimaryStiffeners.GrossSectionModulus = GetSectionModulus(Panel.cScantlings.cPrimaryStiffeners.WebHeight, _
                Panel.cScantlings.cPrimaryStiffeners.WebThickness + Panel.cScantlings.cPrimaryStiffeners.CorrosionThickness, _
                Panel.cScantlings.cPrimaryStiffeners.FlangeWidth, _
                Panel.cScantlings.cPrimaryStiffeners.FlangeThickness + Panel.cScantlings.cPrimaryStiffeners.CorrosionThickness, _
                Panel.cScantlings.cPrimaryStiffeners.Spacing, _
                Panel.cScantlings.cPrimaryFrames.Spacing, _
                Panel.cScantlings.GrossThickness)

        Case "Beam"
            Panel.pType = Beam
            Panel.cScantlings.BeamSection = bsSquare
            Panel.cGeometry.BucklingLength = Panel.cGeometry.PanelWidth
            Panel.cScantlings.cPrimaryFrames.Spacing = Project.Item(ActiveProject).cHeader.Width / 10
            Panel.cScantlings.cPrimaryFrames.WebHeight = 0.1 ' Square side
    End Select
    'Default Design Variables
    '------------------------
    Dim v() As Variant
    GetScantlings Panel.cScantlings, v
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
    
    'Panel.cCostCAtMain.SetFirstPanelData Panel.pNumber, ActiveProject
End Sub

Public Function ReNumPanels()
    'Performs panel renumerotation + panel collection renumerotation
    Dim index As Integer
    Dim Panel As cPanel
    Dim col As New colPanel
    index = 0
    For Each Panel In Project.Item(ActiveProject).colPanel
        index = index + 1
        Panel.pNumber = index
        Panel.index = index
        col.Add Panel, Panel.pNumber
    Next Panel
    Set Project.Item(ActiveProject).colPanel = col
    Set col = Nothing
    
    Set col = New colPanel
    index = 0
    For Each Panel In Project.Item(ActiveProject).colPanelUpdate
        index = index + 1
        Panel.pNumber = index
        Panel.index = index
        col.Add Panel, Panel.pNumber
    Next Panel
    Set Project.Item(ActiveProject).colPanelUpdate = col
    Set col = Nothing
    
End Function

Public Function ReNumProjects()
    Dim index As Integer
    Dim oPro As cProject
    Dim colPro As New colProject
    index = 0
    For Each oPro In Project
        index = index + 1
        oPro.index = index
        colPro.Add oPro, oPro.index
    Next oPro
    Set Project = colPro
    
    Set colPro = Nothing
End Function

Public Function UpdatePanelConnections(ProjectIndex As Integer)
    On Error GoTo UpdatePanelConnectionsErr
    Dim cNode As cNode, colNodes As colNodes
    Dim CheckedPanel As cPanel, Panel As cPanel
    Dim index As Integer, i As Integer
    Dim col As Collection
    Set colNodes = Project.Item(ProjectIndex).colNodes
    For Each Panel In Project.Item(ProjectIndex).colPanel
        Set col = New Collection
        index = 0
        For Each CheckedPanel In Project.Item(ProjectIndex).colPanel
            If CheckedPanel.cGeometry.InNode = Panel.cGeometry.OutNode Then
                index = index + 1
                col.Add CheckedPanel.pNumber
            End If
        Next CheckedPanel
        For i = index + 1 To 10
            col.Add 0
        Next i
        Set Panel.colConnections = col
        Set col = Nothing
    Next Panel
    Set colNodes = Nothing
    Exit Function
UpdatePanelConnectionsErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modPanelFunctions: Function UpdatePanelConnections")
End Function

'Public Function UpdateNodeCoordinates()
'    On Error GoTo  UpdateNodeCoordinatesErr
'    Dim Panel As cPanel
'    Dim X_Translation As Double
'    Dim Y_Translation As Double
'    If Project.Item(ProjectIndex).colPanel.Count = 0 Then Exit Function
'    X_Translation = Project.Item(ProjectIndex).colPanel.Item(1).X_OUT
'    Y_Translation = Project.Item(ProjectIndex).colPanel.Item(1).Y_OUT
'    For Each Panel In Project.Item(ProjectIndex).colPanel
'        Panel.X_IN = Panel.X_IN - X_Translation
'        Panel.Y_IN = Panel.Y_IN - Y_Translation
'        Panel.X_OUT = Panel.X_OUT - X_Translation
'        Panel.Y_OUT = Panel.Y_OUT - Y_Translation
'    Next Panel
'    Exit Function
'UpdateNodeCoordinatesErr:
'    Call RaiseError(MyUnhandledError,Err.Description & "." & vbCrLf &  "modPanelFunctions: Function UpdateNodeCoordinates")
'End Function

Public Function UpdateBoundary(ByVal ProjectIndex As Integer)
    Dim i As Integer
    Dim Panel As cPanel
    Dim panel1 As cPanel
    Dim col As colPanel
    Dim Boundary As New cBoundaryConditions, Boundary1 As New cBoundaryConditions
    Dim colBoundary As colBoundaryConditions
    Dim colConnections As Collection
    Dim bConnectedIn As Boolean
    Dim bConnectedOut As Boolean
    Dim colNodes As colNodes
    Set colNodes = Project.Item(ProjectIndex).colNodes
    Set col = Project.Item(ProjectIndex).colPanel
    Select Case col.Count
        Case 1
            Set colBoundary = col.Item(1).colBoundaryConditions
            Select Case colBoundary.Count
                Case 0
                    ' No boundary condition defined
                    Boundary.index = 1
                    Boundary.BoundaryCondition = FreeEdge
                    Boundary.Edge = InEdge
                    colBoundary.Add Boundary, 1
                    Boundary.index = 2
                    Boundary.BoundaryCondition = FreeEdge
                    Boundary.Edge = OutEdge
                    colBoundary.Add Boundary, 2
                Case 1
                    ' One boundary condition defined
                    Select Case colBoundary.Item(1).Edge
                        Case InEdge
                            Boundary.index = 2
                            Boundary.BoundaryCondition = FreeEdge
                            Boundary.Edge = OutEdge
                            colBoundary.Add Boundary, 2
                        Case OutEdge
                            Boundary1.BoundaryCondition = colBoundary.Item(1).BoundaryCondition
                            colBoundary.Remove 1
                            Boundary.index = 1
                            Boundary.BoundaryCondition = FreeEdge
                            Boundary.Edge = InEdge
                            colBoundary.Add Boundary, 1
                            Boundary.index = 2
                            Boundary.BoundaryCondition = Boundary1.BoundaryCondition
                            Boundary.Edge = OutEdge
                            colBoundary.Add Boundary, 2
                            Set Boundary1 = Nothing
                    End Select
                Case 2
            End Select
            Set colBoundary = Nothing
        Case Is > 1
            For Each Panel In col
                bConnectedIn = False
                bConnectedOut = False
                For Each panel1 In col
                    If Panel.index <> panel1.index Then
                        If Panel.cGeometry.InNode = panel1.cGeometry.InNode Then bConnectedIn = True
                        If Panel.cGeometry.InNode = panel1.cGeometry.OutNode Then bConnectedIn = True
                        If Panel.cGeometry.OutNode = panel1.cGeometry.InNode Then bConnectedOut = True
                        If Panel.cGeometry.OutNode = panel1.cGeometry.OutNode Then bConnectedOut = True
                    End If
                Next panel1
                ' remove boundaries if panel has no more free edges
                If bConnectedIn = True And bConnectedOut = True Then
                    Set colBoundary = Panel.colBoundaryConditions
                    For Each Boundary In colBoundary
                        colBoundary.Remove Boundary.index
                    Next Boundary
                    Set colBoundary = Nothing
                End If
                
                If bConnectedIn = False And bConnectedOut = True Then
                    Set colBoundary = Panel.colBoundaryConditions
                    If colBoundary.Count = 0 Then
                        Set Boundary = New cBoundaryConditions
                            Boundary.index = 1
                            Boundary.BoundaryCondition = FreeEdge
                            Boundary.Edge = InEdge
                            colBoundary.Add Boundary, Boundary.index
                        Set Boundary = Nothing
                    ElseIf colBoundary.Count = 1 Then
                        Boundary.Edge = InEdge
                        colBoundary.Item(1).Edge = InEdge
                    ElseIf colBoundary.Count = 2 Then
                        For Each Boundary In colBoundary
                            If Boundary.Edge = OutEdge Then
                                colBoundary.Remove Boundary.index
                            End If
                        Next Boundary
                    End If
                    Set colBoundary = Nothing
                End If
                If bConnectedOut = False And bConnectedIn = True Then
                    Set colBoundary = Panel.colBoundaryConditions
                    If colBoundary.Count = 0 Then
                        Set Boundary = New cBoundaryConditions
                            Boundary.index = 1
                            Boundary.BoundaryCondition = FreeEdge
                            Boundary.Edge = OutEdge
                            colBoundary.Add Boundary, Boundary.index
                        Set Boundary = Nothing
                    ElseIf colBoundary.Count = 1 Then
                        Boundary.Edge = OutEdge
                        colBoundary.Item(1).Edge = OutEdge
                    ElseIf colBoundary.Count = 2 Then
                        For Each Boundary In colBoundary
                            If Boundary.Edge = OutEdge Then
                                colBoundary.Remove Boundary.index
                            End If
                        Next Boundary
                    End If
                    Set colBoundary = Nothing
                End If
                ' case both edges uncoupled
                If bConnectedIn = False And bConnectedOut = False Then
                    Set colBoundary = col.Item(Panel.index).colBoundaryConditions
                    Select Case colBoundary.Count
                        Case 0
                            ' No boundary condition defined
                            Boundary.index = 1
                            Boundary.BoundaryCondition = FreeEdge
                            Boundary.Edge = InEdge
                            colBoundary.Add Boundary, 1
                            Boundary.index = 2
                            Boundary.BoundaryCondition = FreeEdge
                            Boundary.Edge = OutEdge
                            colBoundary.Add Boundary, 2
                        Case 1
                            ' One boundary condition defined
                            Select Case colBoundary.Item(1).Edge
                                Case InEdge
                                    Boundary.index = 2
                                    Boundary.BoundaryCondition = FreeEdge
                                    Boundary.Edge = OutEdge
                                    colBoundary.Add Boundary, 2
                                Case OutEdge
                                    Boundary1.BoundaryCondition = colBoundary.Item(1).BoundaryCondition
                                    colBoundary.Remove 1
                                    Boundary.index = 1
                                    Boundary.BoundaryCondition = FreeEdge
                                    Boundary.Edge = InEdge
                                    colBoundary.Add Boundary, 1
                                    Boundary.index = 2
                                    Boundary.BoundaryCondition = Boundary1.BoundaryCondition
                                    Boundary.Edge = OutEdge
                                    colBoundary.Add Boundary, 2
                                    Set Boundary1 = Nothing
                            End Select
                        Case 2
                    End Select
                    Set colBoundary = Nothing
                End If
            Next Panel
    End Select
    Set col = Nothing
    Set colNodes = Nothing
End Function

Public Function IsBoundaries(Optional index As Integer) As Boolean
    Dim Panel As cPanel
    Dim col As colPanel
    index = 0
    Set col = Project.Item(ActiveProject).colPanel
    IsBoundaries = False
    For Each Panel In col
        If Panel.colBoundaryConditions.Count > 0 Then
            IsBoundaries = True
            index = index + Panel.colBoundaryConditions.Count
            'Set col = Nothing
            'Exit Function
        End If
    Next Panel
    Set col = Nothing
End Function

Public Sub GetLengthAngle(ByVal Y0 As Double, ByVal Z0 As Double, _
    ByVal y1 As Double, ByVal z1 As Double, ByVal Panel As cPanel)
    Dim Length As Double, angle As Double
    'Y0, Z0 in origine
    Dim DY As Double, dZ As Double
    'Dim Length As double
    Y0 = Round(Y0, 6)
    Z0 = Round(Z0, 6)
    y1 = Round(y1, 6)
    z1 = Round(z1, 6)
    DY = y1 - Y0
    dZ = z1 - Z0
    Length = Sqr(DY * DY + dZ * dZ)
'    Length = Round(Length, 6)
    If Z0 = z1 And y1 > Y0 Then
        'Angle = 0
        angle = 0
    ElseIf y1 > Y0 And z1 > Z0 Then    'cadran1; Y1>0, Z1>0
        angle = ArcSin(dZ / Length)
    ElseIf Y0 = y1 And z1 > Z0 Then
        angle = 90
    ElseIf y1 < Y0 And z1 > Z0 Then    'cadran2; Y1<0, Z1>0
        angle = 180 - ArcSin(dZ / Length)
    ElseIf Z0 = z1 And y1 < Y0 Then
        'Angle = 180
        angle = 180
    ElseIf y1 < Y0 And z1 < Z0 Then    'cadran3; Y1<0, Z1<0
        angle = 180 - ArcSin(dZ / Length) ''
    ElseIf Y0 = y1 And z1 < Z0 Then
        angle = 270
    ElseIf y1 > Y0 And z1 < Z0 Then    'cadran4; Y1>0, Z1<0
        angle = 360 + ArcSin(dZ / Length)
    End If
    Panel.cGeometry.PanelAngle = Round(angle, 6)
    Panel.cGeometry.PanelWidth = Round(Length, 6)
'    Panel.cGeometry.PanelAngle = Angle
'    Panel.cGeometry.PanelWidth = Length

End Sub

Public Function DetectIntersection( _
                ByVal y1 As Double, ByVal z1 As Double, _
                ByVal y2 As Double, ByVal z2 As Double, _
                ByVal y3 As Double, ByVal z3 As Double, _
                ByVal y4 As Double, ByVal z4 As Double, _
                ByVal Angle2 As Double) As Boolean
    On Error Resume Next
   'Detect intersection
    Dim Ua As Double, Ub As Double
    Dim dDenominator As Double
    dDenominator = (z4 - z3) * (y2 - y1) - (y4 - y3) * (z2 - z1)
    Ua = Divide(((y4 - y3) * (z1 - z3) - (z4 - z3) * (y1 - y3)), (dDenominator))
    Ub = Divide(((y2 - y1) * (z1 - z3) - (z2 - z1) * (y1 - y3)), (dDenominator))
    If Ua > 0 And Ua < 1 And Ub > 0 And Ub < 1 Then
        DetectIntersection = True
    End If
    'Detect if duplicate node definition
    If y1 = y2 And z1 = z2 Then DetectIntersection = True
    If y3 = y4 And z3 = z4 Then DetectIntersection = True
    'Detect coliniarity
    If y1 = y3 And z1 = z3 And y2 = y4 And z2 = z4 Then ' Lines superposed
        DetectIntersection = True
    End If
     If y1 = y4 And z1 = z4 And y2 = y3 And z2 = z3 Then ' Lines superposed
        DetectIntersection = True
    End If
    If dDenominator = 0 And Ua = 0 And Ub = 0 Then 'Lines Paraleles
        Dim Yr2 As Double, Zr2 As Double, Yr1 As Double, Zr1 As Double, Yr4 As Double, Zr4 As Double
        y1 = y1 - y3
        z1 = -z1 + z3
        y2 = y2 - y3
        z2 = -z2 + z3
        y4 = y4 - y3
        z4 = -z4 + z3
        y3 = 0
        z3 = 0
        If Angle2 < 0 Then Angle2 = Angle2 + 360
            Call RotateNode(y1, z1, Yr1, Zr1, Angle2)
            Call RotateNode(y2, z2, Yr2, Zr2, Angle2)
            Call RotateNode(y4, z4, Yr4, Zr4, Angle2)
            If Round(Zr1, 4) = Round(Zr4, 4) Then 'Colinearity
                If y3 > Yr1 And y3 < Yr2 Then DetectIntersection = True
                If y3 > Yr2 And y3 < Yr1 Then DetectIntersection = True
                If Yr4 > Yr1 And Yr4 < Yr2 Then DetectIntersection = True
                If Yr4 > Yr2 And Yr4 < Yr1 Then DetectIntersection = True
                If Yr1 > y3 And Yr1 < Yr4 Then DetectIntersection = True
                If Yr1 > Yr4 And Yr1 < y3 Then DetectIntersection = True
                If Yr2 > y3 And Yr2 < Yr4 Then DetectIntersection = True
                If Yr2 > Yr4 And Yr2 < y3 Then DetectIntersection = True
            End If
    End If
    
End Function

Public Sub RotateNode(x1 As Double, y1 As Double, XR As Double, yr As Double, angle As Double)
    On Error GoTo RotatePointErr
    'This Sub works for panels with In Node in 0,0
    XR = x1 * Cos(angle * PI / 180) - y1 * Sin(angle * PI / 180)
    yr = x1 * Sin(angle * PI / 180) + y1 * Cos(angle * PI / 180)
    Exit Sub
RotatePointErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modPanelFunctions: Sub RotatePoint")
End Sub

Public Function ExplodeDoubleHull(ByVal y As Integer, ByVal z As Integer)
    'On Error GoTo  ExplodeDoubleHullErr
    Dim Panel As cPanel
    If Project.Item(ActiveProject).ScreenMode <> NORMAL_MODE Then Exit Function
    For Each Panel In Project.Item(ActiveProject).colPanel
        If Panel.pType = DoubleHull And Panel.RelatedDoubleHullPanel > Panel.pNumber Then
            If PtInRegion(Panel.cScantlings.cPrimaryFrames.DoubleHullRgn, y, z) = 1 Then
                Panel.pType = Plate
                Project.Item(ActiveProject).colPanel.Item(Panel.RelatedDoubleHullPanel).pType = Plate
                
                Project.Item(ActiveProject).colPanel.Item(Panel.RelatedDoubleHullPanel).RelatedDoubleHullPanel = 0
                Panel.RelatedDoubleHullPanel = 0
                Draw ActiveProject
                Exit Function
            End If
        End If
    Next Panel
    Project.Item(ActiveProject).DataChanged = True
    Exit Function
ExplodeDoubleHullErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modPanelFunctions: Sub ExplodeDoubleHull")
End Function

Public Function ReversePanel(ByVal y As Integer, ByVal z As Integer)
    On Error GoTo ReversePanelErr
    Dim cPanel As cPanel
    If Project.Item(ActiveProject).ScreenMode <> NORMAL_MODE Then Exit Function
    For Each cPanel In Project.Item(ActiveProject).colPanel
        If PtInRegion(cPanel.Region, y, z) = 1 Then
            Select Case cPanel.pType
                Case DoubleHull
                    MsgBox "Double Hull Panels cannot be reversed.", vbCritical + vbOKOnly
                Case Else
                    cPanel.Reverse
                    Project.Item(ActiveProject).DataChanged = True
            End Select
            Exit Function
        End If
    Next cPanel
    Project.Item(ActiveProject).DataChanged = True
    Exit Function
ReversePanelErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modPanelFunctions: Function ReversePanel")
End Function

Public Function MatchProperties(ByVal y As Integer, ByVal z As Integer)
    'On Error GoTo  MatchPropertiesErr
    Static iOriginalPanel As Integer
    Static ePanelType As PanelType
    Dim cPanel As cPanel
    Dim cGirder As cGirder
    If Project.Item(ActiveProject).ScreenMode <> NORMAL_MODE Then Exit Function
    Select Case Project.Item(ActiveProject).FunctionMode
        Case MATCH_PROPERTIES_FUNCTION
            For Each cPanel In Project.Item(ActiveProject).colPanel
                If PtInRegion(cPanel.Region, y, z) = 1 Then
                    iOriginalPanel = cPanel.pNumber
                    ePanelType = cPanel.pType
                    Exit For
                End If
            Next cPanel
            setFunctionMode PASTE_PROPERTIES_FUNCTION
        Case PASTE_PROPERTIES_FUNCTION
            For Each cPanel In Project.Item(ActiveProject).colPanel
                If PtInRegion(cPanel.Region, y, z) = 1 Then
                    Select Case cPanel.pType
                        Case ePanelType
                            With Project.Item(ActiveProject).colPanel.Item(iOriginalPanel)
                                Set cPanel.cScantlings = .cScantlings.Clone
                                Set cPanel.colLoadCase = .colLoadCase.Clone
                                Set cPanel.colAssessmentPoints = .colAssessmentPoints.Clone
                                cPanel.LateralPressureSide = .LateralPressureSide
                                cPanel.LocalizedPressure = .LocalizedPressure
                                Set cPanel.cMaterial = .cMaterial.Clone
                                Set cPanel.colDesignVariables = .colDesignVariables.Clone
                                Set cPanel.colGeometricalConstraints = .colGeometricalConstraints.Clone
                            End With
                        Case Else
                            MsgBox "Matched panels must be of the same type.", vbCritical + vbOKOnly
'                        Case DoubleHull
'                            With Project.Item(ActiveProject).colPanel.Item(iOriginalPanel).cScantlings
'                                cPanel.cScantlings.BeamSection = .BeamSection
'                                cpanel.cScantlings.
'                            End With
'                            Set cPanel.colLoadCase = Project.Item(ActiveProject).colPanel.Item(iOriginalPanel).colLoadCase.Clone
'                            cPanel.LateralPressureSide = Project.Item(ActiveProject).colPanel.Item(iOriginalPanel).LateralPressureSide
'                            Set cPanel.colGeometricalConstraints = Project.Item(ActiveProject).colPanel.Item(iOriginalPanel).colGeometricalConstraints.Clone
                    End Select
                    For Each cGirder In cPanel.cScantlings.colGirder
                        If cGirder.Distance > cPanel.cGeometry.PanelWidth Then
                            cPanel.cScantlings.colGirder.Remove cGirder.index
                        ElseIf cGirder.Distance = cPanel.cGeometry.PanelWidth Then
                            cGirder.Distance = cGirder.Distance - 0.001
                        End If
                    Next cGirder
                    Project.Item(ActiveProject).DataChanged = True
                End If
            Next cPanel
            
    End Select
    Exit Function
MatchPropertiesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modPanelFunctions: Function MatchProperties")
End Function

Public Function DividePanel(ByVal y As Integer, ByVal z As Integer)
    On Error GoTo DividePanelErr
    Dim cPanel As cPanel
    If Project.Item(ActiveProject).ScreenMode <> NORMAL_MODE Then Exit Function
    For Each cPanel In Project.Item(ActiveProject).colPanel
        If PtInRegion(cPanel.Region, y, z) = 1 Then
            If cPanel.pType = DoubleHull Then
                MsgBox "Double Hull Panels cannot be divided.", vbCritical + vbOKOnly
                Exit Function
            End If
            frmDividePanel.OpenForm cPanel.pNumber
            If Project.Item(ActiveProject).colPanel.Count >= Licensing.MAX_PANELS Then
                MsgBox "Maximum number of panels reached.", vbInformation + vbOKOnly
                setFunctionMode NO_FUNCTION
                setScreenMode NORMAL_MODE
                Exit Function
            End If
            Exit Function
        End If
    Next cPanel
    Exit Function
DividePanelErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modPanelFunctions: Function DividePanel")
End Function

Public Function AddNappe(ByVal y As Integer, ByVal z As Integer, ByVal index As Integer)
    On Error GoTo AddNappeErr
    ' index = 0 for add nappe; = 1 to finish operation, = 2 to cancel
    Static sNappe As String, iNappe As Integer
    Static cPanel As cPanel
    ' index = 0 for simple shell; = 1 to finish operation; = 2 to cancel
    If index = 0 Then
        For Each cPanel In Project.Item(ActiveProject).colPanel
            If cPanel.Visible = True Then
                If PtInRegion(cPanel.Region, y, z) = 1 Then
                    Select Case cPanel.pType
                        Case Plate, DoubleHull
                            If cPanel.Selected = IsUnselected Then
                                cPanel.Selected = isSelected
                                sNappe = sNappe & cPanel.pNumber & " "
                                iNappe = iNappe + 1
                                Exit Function
                            End If
                        Case Beam
                            MsgBox "Beam Panels cannot be treated as shells.", vbCritical + vbOKOnly
                            cPanel.Selected = IsUnselected
                    End Select
                End If
            End If
        Next cPanel
    End If
    If index = 1 Then
        sNappe = RTrim(sNappe)
        If TestNappe(sNappe, iNappe) = False Then
            sNappe = ""
            iNappe = 0
        End If
        Project.Item(ActiveProject).frmCostCAt.OpenFormAddNappe sNappe, iNappe
        sNappe = ""
        iNappe = 0
        For Each cPanel In Project.Item(ActiveProject).colPanel
            cPanel.Selected = IsUnselected
        Next cPanel
    End If
1:
    If index = 2 Then
        sNappe = ""
        iNappe = 0
        For Each cPanel In Project.Item(ActiveProject).colPanel
            cPanel.Selected = IsUnselected
        Next cPanel
    End If

    Exit Function
AddNappeErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modPanelFunctions: Function AddNAppe")
End Function

Public Function IsPanelInsideStructure(ByVal PanelIndex As Integer, ByVal ProjectIndex As Integer) As Boolean
    'On Error GoTo  IsPanelInsideStructureErr
    Dim cPanel As cPanel, Panel As cPanel
    Dim index As Integer
    Set cPanel = Project.Item(ProjectIndex).colPanel.Item(PanelIndex)
    'set symmetry axis
    index = -1
    Dim lp() As POINTAPI
    Dim bSymm As Boolean
    bSymm = False
    If Project.Item(ProjectIndex).colPanel.Count < 2 Then Exit Function
    For Each Panel In Project.Item(ProjectIndex).colPanel
        If Panel.colBoundaryConditions.Count > 0 Then
            If Panel.colBoundaryConditions.Item(1).BoundaryCondition = SymmetryAxis1 Then
                bSymm = True
                index = index + 1
                ReDim Preserve lp(index)
                Select Case Panel.colBoundaryConditions.Item(1).Edge
                    Case InEdge
                        lp(index).y = Project.Item(ProjectIndex).colNodes.Item(Panel.cGeometry.InNode).y
                        lp(index).z = Project.Item(ProjectIndex).colNodes.Item(Panel.cGeometry.InNode).z
                    Case OutEdge
                        lp(index).y = Project.Item(ProjectIndex).colNodes.Item(Panel.cGeometry.OutNode).y
                        lp(index).z = Project.Item(ProjectIndex).colNodes.Item(Panel.cGeometry.OutNode).z
                End Select
            End If
        End If
    Next Panel
    'Reorder after Z
    If bSymm = False Then Exit Function
    Dim i As Integer, j As Integer
    Dim lp1(0) As POINTAPI
    
    If UBound(lp) > 0 Then
        For i = 0 To UBound(lp)
            For j = i To UBound(lp) - 1
                If lp(i).z > lp(j + 1).z Then
                    lp1(0) = lp(j + 1)
                    lp(j + 1) = lp(i)
                    lp(i) = lp1(0)
                End If
            Next j
        Next i
    End If
    
    'begin test
    Dim y1 As Double, z1 As Double, y2 As Double, z2 As Double, _
        y3 As Double, z3 As Double, y4 As Double, z4 As Double, _
        Angle2 As Double
    Dim node1y As Double, node1z As Double, node2y As Double, node2z As Double
    
    node1y = Project.Item(ProjectIndex).colNodes.Item(cPanel.cGeometry.InNode).y
    node1z = Project.Item(ProjectIndex).colNodes.Item(cPanel.cGeometry.InNode).z
    node2y = Project.Item(ProjectIndex).colNodes.Item(cPanel.cGeometry.OutNode).y
    node2z = Project.Item(ProjectIndex).colNodes.Item(cPanel.cGeometry.OutNode).z
    
    'virtual line to check intersections
    y1 = (node1y + node2y) / 2
    z1 = (node1z + node2z) / 2
    

    Dim occ_left As Integer, occ_right As Integer
    For Each Panel In Project.Item(ProjectIndex).colPanel
        If Panel.pNumber <> cPanel.pNumber Then

            y3 = Project.Item(ProjectIndex).colNodes.Item(Panel.cGeometry.InNode).y
            z3 = Project.Item(ProjectIndex).colNodes.Item(Panel.cGeometry.InNode).z
            y4 = Project.Item(ProjectIndex).colNodes.Item(Panel.cGeometry.OutNode).y
            z4 = Project.Item(ProjectIndex).colNodes.Item(Panel.cGeometry.OutNode).z
            Angle2 = Panel.cGeometry.PanelAngle
            
            y2 = y1 + 100 * Cos(PI * (cPanel.cGeometry.PanelAngle - 90) / 180)
            z2 = z1 + 100 * Sin(PI * (cPanel.cGeometry.PanelAngle - 90) / 180)
            If DetectIntersection(y1, z1, y2, z2, y3, z3, y4, z4, Angle2) = True Then
                occ_left = occ_left + 1
            End If
            
            y2 = y1 + 100 * Cos(PI * (cPanel.cGeometry.PanelAngle + 90) / 180)
            z2 = z1 + 100 * Sin(PI * (cPanel.cGeometry.PanelAngle + 90) / 180)
            If DetectIntersection(y1, z1, y2, z2, y3, z3, y4, z4, Angle2) = True Then
                occ_right = occ_right + 1
            End If
        End If
    Next Panel
    If UBound(lp) > 0 Then
        For i = 0 To UBound(lp) - 1
            y3 = lp(i).y
            z3 = lp(i).z
            y4 = lp(i + 1).y
            z4 = lp(i + 1).z
            y2 = y1 + 100 * Cos(PI * (cPanel.cGeometry.PanelAngle - 90) / 180)
            z2 = z1 + 100 * Sin(PI * (cPanel.cGeometry.PanelAngle - 90) / 180)
            If DetectIntersection(y1, z1, y2, z2, y3, z3, y4, z4, Angle2) = True Then
                occ_left = occ_left + 1
            End If
            y2 = y1 + 100 * Cos(PI * (cPanel.cGeometry.PanelAngle + 90) / 180)
            z2 = z1 + 100 * Sin(PI * (cPanel.cGeometry.PanelAngle + 90) / 180)
            If DetectIntersection(y1, z1, y2, z2, y3, z3, y4, z4, Angle2) = True Then
                occ_right = occ_right + 1
            End If
        Next i
    End If
    If occ_left > 0 And occ_right > 0 Then
        IsPanelInsideStructure = True
    Else
        IsPanelInsideStructure = False
    End If
    Set cPanel = Nothing
    Exit Function
IsPanelInsideStructureErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modPanelFunctions: Function IsPanelInsideStructure")
End Function

Public Function DetectParalelism(ByRef panel1 As cPanel, ByRef panel2 As cPanel, ByVal ProjectIndex As Integer) As Boolean
    On Error GoTo DetectColinearityErr
    Dim y1 As Double, z1 As Double, y2 As Double, z2 As Double, _
    y3 As Double, z3 As Double, y4 As Double, z4 As Double
    y1 = Project.Item(ProjectIndex).colNodes.Item(panel1.cGeometry.InNode).y
    z1 = Project.Item(ProjectIndex).colNodes.Item(panel1.cGeometry.InNode).z
    y2 = Project.Item(ProjectIndex).colNodes.Item(panel1.cGeometry.OutNode).y
    z2 = Project.Item(ProjectIndex).colNodes.Item(panel1.cGeometry.OutNode).z
    y3 = Project.Item(ProjectIndex).colNodes.Item(panel2.cGeometry.InNode).y
    z3 = Project.Item(ProjectIndex).colNodes.Item(panel2.cGeometry.InNode).z
    y4 = Project.Item(ProjectIndex).colNodes.Item(panel2.cGeometry.OutNode).y
    z4 = Project.Item(ProjectIndex).colNodes.Item(panel2.cGeometry.OutNode).z
    Dim Ua As Double, Ub As Double
    Dim dDenominator As Double
    dDenominator = Round((z4 - z3) * (y2 - y1) - (y4 - y3) * (z2 - z1), 1)
    Ua = Divide(((y4 - y3) * (z1 - z3) - (z4 - z3) * (y1 - y3)), (dDenominator))
    Ub = Divide(((y2 - y1) * (z1 - z3) - (z2 - z1) * (y1 - y3)), (dDenominator))
    If Round(dDenominator, 1) = 0 And Round(Ua, 1) = 0 And Round(Ub, 1) = 0 Then 'Lines Paraleles
        DetectParalelism = True
    Else
        DetectParalelism = False
    End If
    Exit Function
DetectColinearityErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modPanelFunctions: Function DetectColinearity")
End Function

Public Function TestNappe(ByRef sNappe As String, ByRef iNappe As Integer) As Boolean
    On Error GoTo TestNappeErr
    Dim v() As Variant
    Dim s As String
    TestNappe = True
    s = sNappe
    Dim i As Integer, j As Integer
    If iNappe < 2 Then
        TestNappe = True
        Exit Function
    End If
    GetValues iNappe, s, v
    For i = 1 To iNappe
        v(i) = Val(v(i))
    Next i
    'Test 1 : panels must have the same gradient
    Dim bTest1 As Boolean
    bTest1 = True

    Dim y1 As Double, z1 As Double, _
    y2 As Double, z2 As Double, _
    y3 As Double, z3 As Double, _
    y4 As Double, z4 As Double
                
    Dim Ua As Double, Ub As Double
    Dim dDenominator As Double
    
    y1 = Project.Item(ActiveProject).colNodes.Item(Project.Item(ActiveProject).colPanel.Item(CInt(v(1))).cGeometry.InNode).y
    z1 = Project.Item(ActiveProject).colNodes.Item(Project.Item(ActiveProject).colPanel.Item(CInt(v(1))).cGeometry.InNode).z
    y2 = Project.Item(ActiveProject).colNodes.Item(Project.Item(ActiveProject).colPanel.Item(CInt(v(1))).cGeometry.OutNode).y
    z2 = Project.Item(ActiveProject).colNodes.Item(Project.Item(ActiveProject).colPanel.Item(CInt(v(1))).cGeometry.OutNode).z
    
    For i = 2 To iNappe
        y3 = Project.Item(ActiveProject).colNodes.Item(Project.Item(ActiveProject).colPanel.Item(CInt(v(i))).cGeometry.InNode).y
        z3 = Project.Item(ActiveProject).colNodes.Item(Project.Item(ActiveProject).colPanel.Item(CInt(v(i))).cGeometry.InNode).z
        y4 = Project.Item(ActiveProject).colNodes.Item(Project.Item(ActiveProject).colPanel.Item(CInt(v(i))).cGeometry.OutNode).y
        z4 = Project.Item(ActiveProject).colNodes.Item(Project.Item(ActiveProject).colPanel.Item(CInt(v(i))).cGeometry.OutNode).z
        dDenominator = Round((z4 - z3) * (y2 - y1) - (y4 - y3) * (z2 - z1), 1)
        Ua = Divide(((y4 - y3) * (z1 - z3) - (z4 - z3) * (y1 - y3)), (dDenominator))
        Ub = Divide(((y2 - y1) * (z1 - z3) - (z2 - z1) * (y1 - y3)), (dDenominator))
        If Round(dDenominator, 1) = 0 And Round(Ua, 1) = 0 And Round(Ub, 1) = 0 Then 'Lines Paraleles
        Else
            bTest1 = False
            TestNappe = False
        End If
    Next i

'    For i = 2 To iNappe
'        Select Case Round(Project.Item(ActiveProject).colPanel.Item(CInt(v(1))).cGeometry.PanelAngle, 1)
'            Case Is = Round(Project.Item(ActiveProject).colPanel.Item(CInt(v(i))).cGeometry.PanelAngle, 1)
'            Case Is = Round(Project.Item(ActiveProject).colPanel.Item(CInt(v(i))).cGeometry.PanelAngle + 180, 1)
'            Case Else
'                bTest1 = False
'                TestNappe = False
'        End Select
'    Next i
    
    ' Test continuity
    Dim bTest2 As Boolean
    bTest2 = True
    Dim cNode As cNode
    Dim cPanel As cPanel
    Dim index As Integer
    index = 0
    For Each cNode In Project.Item(ActiveProject).colNodes
        For Each cPanel In Project.Item(ActiveProject).colPanel
        'For i = 1 To UBound(v)
            If cPanel.Selected = isSelected Then
'                If Project.Item(ActiveProject).colPanel.Item(CInt(v(i))).cGeometry.InNode = cNode.nNumber Then
'                    Index = Index + 1
'                    Exit For
'                End If
'                If Project.Item(ActiveProject).colPanel.Item(CInt(v(i))).cGeometry.OutNode = cNode.nNumber Then
'                    Index = Index + 1
'                    Exit For
'                End If
                If cPanel.cGeometry.InNode = cNode.nNumber Then
                    index = index + 1
                    Exit For
                End If
                If cPanel.cGeometry.OutNode = cNode.nNumber Then
                    index = index + 1
                    Exit For
                End If
            End If
'        Next i
        Next cPanel
    Next cNode
    Select Case index
        Case iNappe + 1
        Case Is >= iNappe + 2
            bTest2 = False
            TestNappe = False
        Case Else
            'MsgBox "Unexpected Case!", vbOKOnly + vbCritical
    End Select
    
    'Messages
    If bTest1 = False Then
        MsgBox "Panels must have equal gradients.", vbOKOnly + vbCritical
    End If
    If bTest2 = False Then
        MsgBox "Nappes must have continuity.", vbOKOnly + vbCritical
    End If
    Exit Function
TestNappeErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modPanelFunctions: Function TestNappe")
End Function

Public Function AddPanelTypeSimpleShell(ByVal y As Integer, ByVal z As Integer, ByVal index As Integer)
    On Error GoTo AddPanelTypeSimpleShellErr
    Static sShell As String, iShell As Integer
    Static cPanel As cPanel
    ' index = 0 for simple shell; = 1 to finish operation; = 2 to cancel
    If index = 0 Then
        For Each cPanel In Project.Item(ActiveProject).colPanel
            If PtInRegion(cPanel.Region, y, z) = 1 Then
                Select Case cPanel.pType
                    Case Plate, DoubleHull
                        cPanel.Selected = isSelected
                        sShell = sShell & cPanel.pNumber & " "
                        iShell = iShell + 1
                        Exit Function
                    Case Beam
                        MsgBox "Beam Panels cannot be treated as shells.", vbCritical + vbOKOnly
                        cPanel.Selected = IsUnselected
                End Select
            End If
        Next cPanel
    End If
    If index = 1 Then
        sShell = RTrim(sShell)
        Project.Item(ActiveProject).frmCostCAt.OpenFormAddShell sShell, iShell
        sShell = ""
        iShell = 0
        For Each cPanel In Project.Item(ActiveProject).colPanel
            cPanel.Selected = IsUnselected
        Next cPanel
    End If
    If index = 2 Then
        sShell = ""
        iShell = 0
        For Each cPanel In Project.Item(ActiveProject).colPanel
            cPanel.Selected = IsUnselected
        Next cPanel
    End If
    Exit Function
AddPanelTypeSimpleShellErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modPanelFunctions: Function AddPanelTypeSimpleShell")
End Function

Public Function AddPanelTypeVirtual(ByVal y As Integer, ByVal z As Integer, ByVal index As Integer)
    On Error GoTo AddPanelTypeVirtualErr
    Static sVirtual As String, iVirtual As Integer
    Static cPanel As cPanel
    ' index = 0 for simple shell; = 1 to finish operation; = 2 to cancel
    If index = 0 Then
        For Each cPanel In Project.Item(ActiveProject).colPanel
            If PtInRegion(cPanel.Region, y, z) = 1 Then
                Select Case cPanel.pType
                    Case Plate, DoubleHull
                        cPanel.Selected = isSelected
                        sVirtual = sVirtual & cPanel.pNumber & " "
                        iVirtual = iVirtual + 1
                        Exit Function
                    Case Beam
                        MsgBox "Beam Panels cannot be treated as shells.", vbCritical + vbOKOnly
                        cPanel.Selected = IsUnselected
                End Select
            End If
        Next cPanel
    End If
    If index = 1 Then
        sVirtual = RTrim(sVirtual)
        Project.Item(ActiveProject).frmCostCAt.OpenFormAddVirtual sVirtual, iVirtual
        sVirtual = ""
        iVirtual = 0
        For Each cPanel In Project.Item(ActiveProject).colPanel
            cPanel.Selected = IsUnselected
        Next cPanel
    End If
    If index = 2 Then
        sVirtual = ""
        iVirtual = 0
        For Each cPanel In Project.Item(ActiveProject).colPanel
            cPanel.Selected = IsUnselected
        Next cPanel
    End If
    Exit Function
AddPanelTypeVirtualErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modPanelFunctions: Function AddPanelTypeVirtual")
End Function

Public Function AddPanelTypeBilge(ByVal y As Integer, ByVal z As Integer, ByVal index As Integer)
    On Error GoTo AddPanelTypeBilgeErr
    Static sBilge As String, iBilge As Integer
    Static cPanel As cPanel
    ' index = 0 for bilge; = 1 to finish operation; = 2 to cancel
    If index = 0 Then
        For Each cPanel In Project.Item(ActiveProject).colPanel
            If PtInRegion(cPanel.Region, y, z) = 1 Then
                Select Case cPanel.pType
                    Case Plate, DoubleHull
                        cPanel.Selected = isSelected
                        sBilge = sBilge & cPanel.pNumber & " "
                        iBilge = iBilge + 1
                        Exit Function
                    Case Beam
                        MsgBox "Beam Panels cannot be treated as bilges.", vbCritical + vbOKOnly
                        cPanel.Selected = IsUnselected
                End Select
            End If
        Next cPanel
    End If
    If index = 1 Then
        sBilge = RTrim(sBilge)
        Project.Item(ActiveProject).frmCostCAt.OpenFormAddBilge sBilge, iBilge
        sBilge = ""
        iBilge = 0
        For Each cPanel In Project.Item(ActiveProject).colPanel
            cPanel.Selected = IsUnselected
        Next cPanel
    End If
    If index = 2 Then
        sBilge = ""
        iBilge = 0
        For Each cPanel In Project.Item(ActiveProject).colPanel
            cPanel.Selected = IsUnselected
        Next cPanel
    End If
    Exit Function
AddPanelTypeBilgeErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modPanelFunctions: Function AddPanelTypeBilge")
End Function

Public Function AddPanelTypeGirder(ByVal y As Integer, ByVal z As Integer, ByVal index As Integer)
    On Error GoTo AddPanelTypeGirderErr
    Static sGirder As String, iGirder As Integer
    Static cPanel As cPanel
    ' index = 0 for girder; = 1 to finish operation; = 2 to cancel
    If index = 0 Then
        For Each cPanel In Project.Item(ActiveProject).colPanel
            If PtInRegion(cPanel.Region, y, z) = 1 Then
                Select Case cPanel.pType
                    Case Plate, DoubleHull
                        cPanel.Selected = isSelected
                        sGirder = sGirder & cPanel.pNumber & " "
                        iGirder = iGirder + 1
                        Exit Function
                    Case Beam
                        MsgBox "Beam Panels cannot be treated as girders.", vbCritical + vbOKOnly
                        cPanel.Selected = IsUnselected
                End Select
            End If
        Next cPanel
    End If
    If index = 1 Then
        sGirder = RTrim(sGirder)
        Project.Item(ActiveProject).frmCostCAt.OpenFormAddGirder sGirder, iGirder
        sGirder = ""
        iGirder = 0
        For Each cPanel In Project.Item(ActiveProject).colPanel
            cPanel.Selected = IsUnselected
        Next cPanel
    End If
    If index = 2 Then
        sGirder = ""
        iGirder = 0
        For Each cPanel In Project.Item(ActiveProject).colPanel
            cPanel.Selected = IsUnselected
        Next cPanel
    End If
    Exit Function
AddPanelTypeGirderErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modPanelFunctions: Function AddPanelTypeGirder")
End Function

Public Function AddPanelTypeDHull(ByVal y As Integer, ByVal z As Integer, ByVal index As Integer)
    On Error GoTo AddPanelTypeDHullErr
    ' index = 0 for inner hull; = 1 for outer hull; = 3 to finish operation; 4 to cancel
    Static sInner As String, sOuter As String
    Static iInner As Integer, iOuter As Integer
    Dim cPanel As cPanel
    If index <> 3 Then
        For Each cPanel In Project.Item(ActiveProject).colPanel
            If PtInRegion(cPanel.Region, y, z) = 1 Then
                Select Case cPanel.pType
                    Case Plate, DoubleHull
                        If index = 0 Then
                            If cPanel.Selected = IsUnselected Then
                                cPanel.Selected = isSelected
                                sInner = sInner & cPanel.pNumber & " "
                                iInner = iInner + 1
                                Exit Function
                            End If
                        ElseIf index = 1 Then
                            If cPanel.Selected = IsUnselected Then
                                cPanel.Selected = isSelected
                                sOuter = sOuter & cPanel.pNumber & " "
                                iOuter = iOuter + 1
                                Exit Function
                            End If
                        End If
                    Case Beam
                        MsgBox "Beam Panels cannot be treated as double hulls.", vbCritical + vbOKOnly
                        cPanel.Selected = IsUnselected
                End Select
            End If
        Next cPanel
    End If
    
    If index = 4 Then
        sInner = ""
        iInner = 0
        sOuter = ""
        iOuter = 0
        For Each cPanel In Project.Item(ActiveProject).colPanel
            cPanel.Selected = IsUnselected
        Next cPanel
    End If
    If index = 3 Then
        sInner = RTrim(sInner)
        sOuter = RTrim(sOuter)
        Project.Item(ActiveProject).frmCostCAt.OpenFormAddDHull sInner, iInner, sOuter, iOuter
        sInner = ""
        sOuter = ""
        iInner = 0
        iOuter = 0
        For Each cPanel In Project.Item(ActiveProject).colPanel
            cPanel.Selected = IsUnselected
        Next cPanel
    End If
    Exit Function
AddPanelTypeDHullErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modPanelFunctions: Function AddPanelTypeDHull")
End Function

Public Function IsSymmAxis(ByVal ProjectIndex As Integer) As Boolean
    On Error GoTo IsSymmAxisErr
    Dim oPanel As cPanel
    For Each oPanel In Project.Item(ProjectIndex).colPanel
        If oPanel.colBoundaryConditions.Count > 0 Then
            If oPanel.colBoundaryConditions.Item(1).BoundaryCondition = SymmetryAxis1 Then
                IsSymmAxis = True
                Exit Function
            End If
        End If
    Next oPanel
    Exit Function
IsSymmAxisErr:
End Function

Public Function IsPanelOnSymmAxis(ByVal PanelIndex As Integer, ByVal ProjectIndex As Integer) As Boolean
    On Error GoTo IsPanelOnSymmAxisErr
    Dim iSymm As Integer
    iSymm = 0
    Dim n_in As Integer, n_out As Integer
    Dim cPanel As cPanel, Panel As cPanel
    Set cPanel = Project.Item(ProjectIndex).colPanel.Item(PanelIndex)
    
    n_in = cPanel.cGeometry.InNode
    n_out = cPanel.cGeometry.OutNode
    For Each Panel In Project.Item(ProjectIndex).colPanel
    If Panel.colBoundaryConditions.Count > 0 Then
        If Panel.cGeometry.InNode = n_in And Panel.colBoundaryConditions.Item(1).BoundaryCondition = SymmetryAxis1 Then
            iSymm = iSymm + 1
        End If
        If Panel.cGeometry.OutNode = n_in And Panel.colBoundaryConditions.Item(1).BoundaryCondition = SymmetryAxis1 Then
            iSymm = iSymm + 1
        End If
        If Panel.cGeometry.InNode = n_out And Panel.colBoundaryConditions.Item(1).BoundaryCondition = SymmetryAxis1 Then
            iSymm = iSymm + 1
        End If
        If Panel.cGeometry.OutNode = n_out And Panel.colBoundaryConditions.Item(1).BoundaryCondition = SymmetryAxis1 Then
            iSymm = iSymm + 1
        End If
    End If
    Next Panel
    If iSymm = 2 Then
        IsPanelOnSymmAxis = True
    Else
        IsPanelOnSymmAxis = False
    End If
    Set Panel = Nothing
    Exit Function
IsPanelOnSymmAxisErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modPanelFunctions: Function IsPanelOnSymmAxis")
End Function

Public Function IsPanelHorizontal(ByVal PanelIndex As Integer, ByVal ProjectIndex As Integer) As Boolean
    On Error GoTo IsPanelHorizontalErr
    Dim cPanel As cPanel
    Set cPanel = Project.Item(ProjectIndex).colPanel.Item(PanelIndex)
    If Round(Project.Item(ProjectIndex).colNodes.Item(cPanel.cGeometry.InNode).z, 1) = _
    Round(Project.Item(ProjectIndex).colNodes.Item(cPanel.cGeometry.OutNode).z, 1) Then
        IsPanelHorizontal = True
    Else
        IsPanelHorizontal = False
    End If
    Exit Function
IsPanelHorizontalErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modPanelFunctions: Function IsPanelHorizontal")
End Function

Public Function GetSectionModulus(ByVal HW As Double, ByVal Tw As Double, _
                                    ByVal lf As Double, ByVal Tf As Double, _
                                    ByVal Ds As Double, ByVal DF As Double, _
                                    ByVal tp As Double)
    On Error GoTo GetSectionModulusErr
    'Params:
    'hw = web height
    'tw = web thickness
    'lf = flange width
    'tf = flange thickness
    'Ds = section attached plate width
    'Df = section length
    
    'Attached width
    Dim lp As Double
    If Ds < 0.2 * DF Then
        lp = Ds
    Else
        lp = 0.2 * DF
    End If
    'Areas
    Dim s1 As Double, s2 As Double, s3 As Double
    s1 = lp * tp
    s2 = HW * Tw
    s3 = lf * Tf
    'Gravity centers
    Dim y1 As Double, y2 As Double, y3 As Double, yg As Double
    y1 = tp / 2
    y2 = tp + HW / 2
    y3 = tp + HW + Tf / 2
    yg = Divide((s1 * y1 + s2 * y2 + s3 * y3), (s1 + s2 + s3))
    'Moments of Inertia
    Dim I1 As Double, I2 As Double, I3 As Double, Iy As Double
    I1 = lp * tp ^ 3 / 12 + lp * tp * (yg - tp / 2) ^ 2
    I2 = Tw * HW ^ 3 / 12 + Tw * HW * (HW / 2 + tp - yg) ^ 2
    I3 = lf * Tf ^ 3 / 12 + lf * Tf * (Tf / 2 + HW + tp - yg) ^ 2
    Iy = I1 + I2 + I3
    'Section modulus
    Dim w As Double
    Select Case yg
        Case Is > tp + HW + Tf - yg
        Case Else
            yg = tp + HW + Tf - yg
    End Select
    w = Divide(Iy, yg)
    'Convert in centimeters
    Iy = Iy * 100000000#
    w = w * 1000000#
    GetSectionModulus = Round(w, 2) '[cm3]
    Exit Function
GetSectionModulusErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modPanelFunctions: Function GetSectionModulus")
End Function

Public Function GetTotalNumberOfDesVar(ByVal index As Integer) As Long
    Dim oPan As cPanel
    Dim oDes As cDesignVariables
    GetTotalNumberOfDesVar = 0
    For Each oPan In Project.Item(index).colPanel
        For Each oDes In oPan.colDesignVariables
            If oDes.Active = True Then
                GetTotalNumberOfDesVar = GetTotalNumberOfDesVar + 1
            End If
        Next oDes
    Next oPan
End Function

Public Function GetTotalNumberOfStrConstr(ByVal index As Integer) As Long
    Dim oPan As cPanel
    Dim oStr As cStructuralConstraints
    Dim oLoadC As cLoadCase
    GetTotalNumberOfStrConstr = 0
    For Each oPan In Project.Item(index).colPanel
        For Each oLoadC In oPan.colLoadCase
            For Each oStr In oLoadC.colStructuralConstraints
                GetTotalNumberOfStrConstr = GetTotalNumberOfStrConstr + 1
            Next oStr
        Next oLoadC
    Next oPan
End Function

Public Function GetTotalNumberOfGeoConstr(ByVal index As Integer) As Long
    Dim oPan As cPanel
    Dim oGeo As cGeometricalConstraints
    GetTotalNumberOfGeoConstr = 0
    For Each oPan In Project.Item(index).colPanel
        For Each oGeo In oPan.colGeometricalConstraints
            GetTotalNumberOfGeoConstr = GetTotalNumberOfGeoConstr + 1
        Next oGeo
    Next oPan
End Function

Public Function GetTotalNumberOfEqRestr(ByVal index As Integer) As Long
    Dim oEq As cEqualityRestrictions
    For Each oEq In Project.Item(index).cHeader.colEqualityRestrictions
        GetTotalNumberOfEqRestr = GetTotalNumberOfEqRestr + 1
    Next oEq
End Function

Public Function GetNoOfPlates(ByVal index As Integer, iplates As Integer, ibeams As Integer, ivirtuals As Integer)
    Dim oPan As cPanel
    iplates = 0
    ibeams = 0
    ivirtuals = 0
    For Each oPan In Project.Item(index).colPanel
        If oPan.pType = Plate Then
            If oPan.cGeometry.PanelWidth <= 0.002 Then
                ivirtuals = ivirtuals + 1
            Else
                iplates = iplates + 1
            End If
        End If
        If oPan.pType = Beam Then
            ibeams = ibeams + 1
        End If
    Next oPan
End Function

Public Function RedistributePressuresDH()
    On Error GoTo RedistributePressuresDHErr
    Static panel1 As Integer
    Dim panel2 As Integer
    Dim oPan As cPanel
    Dim oLC1 As cLoadCase, oLC2 As cLoadCase
    Dim pin As Double, pout As Double
    Static side1 As Integer
    Dim side2 As Integer
    
    For Each oPan In Project.Item(ActiveProject).colPanel
        If oPan.Selected = isSelected Then
            Select Case Project.Item(ActiveProject).FunctionMode
                Case REDISTRIBUTE_LOADS_DH_FIRST_PANEL
                    panel1 = oPan.pNumber
                    side1 = oPan.LateralPressureSide
                    setFunctionMode REDISTRIBUTE_LOADS_DH_SECOND_PANEL
                Case REDISTRIBUTE_LOADS_DH_SECOND_PANEL
                    panel2 = oPan.pNumber
                    side2 = oPan.LateralPressureSide
                    If panel1 = panel2 Then
                        Exit Function
                    End If
                    For Each oLC1 In Project.Item(ActiveProject).colPanel.Item(panel1).colLoadCase
                        Set oLC2 = Project.Item(ActiveProject).colPanel.Item(panel2).colLoadCase.Item(oLC1.index)
                        If side1 = side2 Then
                            pin = (oLC1.LateralPressureIn + oLC2.LateralPressureIn) / 2
                            pout = (oLC1.LateralPressureOut + oLC2.LateralPressureOut) / 2
                            oLC1.LateralPressureIn = pin
                            oLC1.LateralPressureOut = pout
                            oLC2.LateralPressureIn = pin
                            oLC2.LateralPressureOut = pout
                        Else
                            pin = (oLC1.LateralPressureIn + oLC2.LateralPressureOut) / 2
                            pout = (oLC1.LateralPressureOut + oLC2.LateralPressureIn) / 2
                            oLC1.LateralPressureIn = pin
                            oLC1.LateralPressureOut = pout
                            oLC2.LateralPressureIn = pout
                            oLC2.LateralPressureOut = pin
                        End If
                        
                    Next oLC1
                    setFunctionMode REDISTRIBUTE_LOADS_DH_FIRST_PANEL
                    panel1 = 0
                    Project.Item(ActiveProject).DataChanged = True
                Case Else
            End Select
            oPan.Selected = IsUnselected
            Exit For
        End If
    Next oPan
    Exit Function
RedistributePressuresDHErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modPanelFunctions: Function RedistributePressuresDH")
End Function

Public Function GetScantlings(ByVal cSc As cScantlings, v As Variant)
    On Error GoTo GetScantlingsErr
    ReDim v(1 To 9)
    v(1) = cSc.NetThickness
    v(2) = cSc.cPrimaryFrames.WebHeight
    v(3) = cSc.cPrimaryFrames.WebThickness
    v(4) = cSc.cPrimaryFrames.FlangeWidth
    v(5) = cSc.cPrimaryFrames.Spacing
    v(6) = cSc.cPrimaryStiffeners.WebHeight
    v(7) = cSc.cPrimaryStiffeners.WebThickness
    v(8) = cSc.cPrimaryStiffeners.FlangeWidth
    v(9) = cSc.cPrimaryStiffeners.Spacing
    Exit Function
GetScantlingsErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modPanelFunctions: Function GetScantlings")
End Function
'Public Function AddCoupleGirderOnNappe(ByVal Y As Integer, ByVal Z As Integer, ByVal Index As Integer)
'    On Error GoTo  AddCoupleGirderOnNappeErr
'    Static sPanel As String, iPanel As Integer
'    Static cPanel As cPanel
'    ' index = 0 for add; = 1 to finish operation; = 2 to cancel
'    If Index = 0 Then
'        For Each cPanel In Project.Item(ActiveProject).colPanel
'            If PtInRegion(cPanel.Region, Y, Z) = 1 Then
'                Select Case cPanel.pType
'                    Case Plate, DoubleHull
'                        If cPanel.Selected = IsUnselected Then
'                            cPanel.Selected = isSelected
'                            sPanel = sPanel & cPanel.pNumber & " "
'                            iPanel = iPanel + 1
'                        End If
'                        Exit Function
'                    Case Beam
'                        MsgBox "Beams cannot be selected.", vbCritical + vbOKOnly
'                        cPanel.Selected = IsUnselected
'                End Select
'            End If
'        Next cPanel
'    End If
'    If Index = 1 Then
'        sPanel = RTrim(sPanel)
'        Project.Item(ActiveProject).frmCostCAt.OpenFormAddCoupleGirderOnNappe sPanel, iPanel
'        sPanel = ""
'        iPanel = 0
'        For Each cPanel In Project.Item(ActiveProject).colPanel
'            cPanel.Selected = IsUnselected
'        Next cPanel
'    End If
'    If Index = 2 Then
'        sPanel = ""
'        iPanel = 0
'        For Each cPanel In Project.Item(ActiveProject).colPanel
'            cPanel.Selected = IsUnselected
'        Next cPanel
'    End If
'    Exit Function
'AddCoupleGirderOnNappeErr:
'    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modPanelFunctions: Function AddCoupleGirderOnNappe")
'End Function
