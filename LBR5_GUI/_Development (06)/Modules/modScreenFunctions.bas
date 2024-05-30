Attribute VB_Name = "modScreenFunctions"
Option Explicit
Const ZOOM_FACTOR As Long = 10
'Dim ZOOM_FACTOR As Long
Private Ymin As Double, ZMin As Double
Private Ymax As Double, ZMax As Double
Private modelDiag As Double

Private Sub ModelSize()
    Dim Node As cNode
    With Project.Item(ActiveProject)
        .Ymin = 10000
        .Ymax = -10000
        .ZMin = 10000
        .ZMax = -10000
    End With
     
    For Each Node In Project.Item(ActiveProject).colNodes
        Project.Item(ActiveProject).GetMinMaxCoordinates Node.y, Node.z
    Next Node
    
    With Project.Item(ActiveProject)
        Ymin = .Ymin
        Ymax = .Ymax
        ZMin = .ZMin
        ZMax = .ZMax
    End With
    modelDiag = Sqr((Ymax - Ymin) ^ 2 + (ZMax - ZMin) ^ 2)

End Sub

Public Sub UpdateCoordinates(ByVal objShape As cRectWND)
    'On Error GoTo  UpdateCoordinatesErr
    On Error Resume Next ' Bypass division by 0
    Dim Oy As Double: Dim Oz As Double
'    Dim Ymax As Double: Dim Zmax As Double
'    Dim Ymin As Double: Dim Zmin As Double
'    Dim X As Double: Dim Y As Double
    Dim ScaleY As Double: Dim ScaleZ As Double
    Dim CoeffY As Double: Dim CoeffZ As Double
    Project.Item(ActiveProject).frmProject.picScreen.Cls
    Dim Panel As cPanel, Node As cNode
    Dim wndWidth As Double: Dim wndHeight As Double
    Dim wndTop As Double: Dim wndLeft As Double
    wndWidth = objShape.Width
    wndHeight = objShape.Height
    wndTop = objShape.Top
    wndLeft = objShape.Left
'    ScaleY = wndWidth / (Ymax - Ymin)
'    ScaleZ = wndHeight / (Zmax - Zmin)
    ModelSize
    ScaleY = Divide(wndWidth, (Ymax - Ymin))
    ScaleZ = Divide(wndHeight, (ZMax - ZMin))
    
    If ScaleY = 0 Or ScaleZ = 0 Then
        If Divide(wndWidth, ((Ymax - Ymin))) > Divide(wndHeight, ((ZMax - ZMin))) Then
            ScaleZ = ScaleY
        Else
            ScaleY = ScaleZ
        End If
    Else
        If Divide(wndWidth, ((Ymax - Ymin))) < Divide(wndHeight, ((ZMax - ZMin))) Then
            ScaleZ = ScaleY
        Else
            ScaleY = ScaleZ
        End If
    End If

'    If Divide(wndWidth, ((Ymax - Ymin))) < Divide(wndHeight, ((Zmax - Zmin))) Then
'        ScaleZ = ScaleY
'    Else
'        ScaleY = ScaleZ
'    End If
    
'    If (wndWidth / ((Ymax - Ymin))) < (wndHeight / ((Zmax - Zmin))) Then
'        ScaleY = wndWidth / (Ymax - Ymin)
'    Else
'        ScaleZ = wndHeight / (Zmax - Zmin)
'    End If

    Oy = (Divide(wndWidth, ScaleY)) / 2 - (Ymin + Ymax) / 2
    Oz = (Divide(wndHeight, ScaleZ)) / 2 - (ZMin + ZMax) / 2

'    Oy = (wndWidth / ScaleY) / 2 - (Ymin + Ymax) / 2
'    Oz = (wndHeight / ScaleZ) / 2 - (Zmin + Zmax) / 2

'    Dim X_IN As Double: Dim X_OUT As Double
'    Dim Y_IN As Double: Dim Y_OUT As Double
    Dim Y_NODE As Double, Z_NODE As Double
    For Each Node In Project.Item(ActiveProject).colNodes
        Y_NODE = Node.y
        Z_NODE = Node.z
        If Project.Item(ActiveProject).colNodes.Count = 1 Then
            Node.Y_Screen = wndLeft + wndWidth / 2
            Node.Z_Screen = wndTop + wndHeight / 2
        Else
            Node.Y_Screen = wndLeft + (Oy + Y_NODE) * ScaleY
            Node.Z_Screen = wndTop + (Oz + Z_NODE) * ScaleZ
        End If
    Next Node
    
    Y_NODE = Project.Item(ActiveProject).CoordOrigin.y
    Z_NODE = Project.Item(ActiveProject).CoordOrigin.z
    If Project.Item(ActiveProject).colNodes.Count = 1 Then
        Project.Item(ActiveProject).CoordOrigin.Y_Screen = wndLeft + wndWidth / 2
        Project.Item(ActiveProject).CoordOrigin.Z_Screen = wndTop + wndHeight / 2
    Else
        Project.Item(ActiveProject).CoordOrigin.Y_Screen = wndLeft + (Oy + Y_NODE) * ScaleY
        Project.Item(ActiveProject).CoordOrigin.Z_Screen = wndTop + (Oz + Z_NODE) * ScaleZ
    End If
    
'    For Each Panel In Project.Item(ActiveProject).colPanel
'        X_IN = Panel.X_IN
'        Y_IN = Panel.Y_IN
'        X_OUT = Panel.X_OUT
'        Y_OUT = Panel.Y_OUT
'        Panel.X_IN_Screen = wndLeft + (Oy + X_IN) * ScaleY
'        Panel.Y_IN_Screen = wndTop + (Oz + Y_IN) * ScaleZ
'        Panel.X_OUT_Screen = wndLeft + (Oy + X_OUT) * ScaleY
'        Panel.Y_OUT_Screen = wndTop + (Oz + Y_OUT) * ScaleZ
'    Next Panel
    Draw ActiveProject
    Exit Sub
UpdateCoordinatesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modScreenFunctions: Sub UpdateCoordinates")
End Sub

Public Sub ZoomFull()
    On Error GoTo ZoomFullErr:
    Dim objShape As New cRectWND
    Set objShape = Project.Item(ActiveProject).frmProject.cRectWND
    Dim lSize As Long
    lSize = 90
    
    With objShape
        If Project.Item(ActiveProject).frmProject.StatusBar.Visible = True Then
            .Left = Abs(Project.Item(ActiveProject).frmProject.ScaleLeft + lSize)
            .Width = Abs(Project.Item(ActiveProject).frmProject.ScaleWidth - 2 * lSize)
            .Top = Abs(Project.Item(ActiveProject).frmProject.ScaleTop + lSize)
            .Height = Abs(Project.Item(ActiveProject).frmProject.ScaleHeight - 2 * lSize - Project.Item(ActiveProject).frmProject.StatusBar.Height)
        Else
            .Left = Abs(Project.Item(ActiveProject).frmProject.ScaleLeft + lSize)
            .Width = Abs(Project.Item(ActiveProject).frmProject.ScaleWidth - 2 * lSize)
            .Top = Abs(Project.Item(ActiveProject).frmProject.ScaleTop + lSize)
            .Height = Abs(Project.Item(ActiveProject).frmProject.ScaleHeight - 2 * lSize)
        End If
    End With
    
    UpdateCoordinates objShape
    Set objShape = Nothing
    Exit Sub
ZoomFullErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modScreenFunctions: Sub ZoomFull")
End Sub

Public Sub ZoomUpDn(ByVal y As Long, ByVal z As Long)
'
'    On Error Resume Next
'    Dim iSign As Single
'    Static z1 As Long
'    'If z1 = 0 Then z1 = Z
'    If z1 > z Then
'        iSign = 1
'    ElseIf z1 < z Then
'        iSign = -1
'    ElseIf z1 = z Then
'        Exit Sub
'    End If
'    iSign = iSign * 0.2
'
'    Dim objShape As New cRectWND
'    Dim Y_Dir As Long, Z_Dir As Long
'    Set objShape = Project.Item(ActiveProject).frmProject.cRectWND
'    Y_Dir = (Project.Item(ActiveProject).frmProject.picScreen.ScaleWidth / 2 - y)
'    Z_Dir = (Project.Item(ActiveProject).frmProject.picScreen.ScaleHeight / 2 - z)
'    With objShape
'        .Width = .Width + y * iSign
'        .Height = .Height + z * iSign
'        .Left = .Left - iSign * (y / 2 - Y_Dir)
'        .Top = .Top - iSign * (z / 2 - Z_Dir)
'    End With
'    UpdateCoordinates objShape
'    Set objShape = Nothing
'    z1 = z
End Sub

Public Sub ZoomWheel(ByVal lValue As Long, ByVal y As Long, z As Long)
    Dim OBJ As New cRectWND
    Set OBJ = Project.Item(ActiveProject).frmProject.cRectWND
    Dim sc As Long, ZoomValue As Long
    sc = getScale(ActiveProject)
    ModelSize
    ZoomValue = modelDiag * sc
    
    Dim modelwidth As Long
    Dim modelheight As Long
    modelwidth = CLng(Ymax - Ymin)
    modelheight = CLng(ZMax - ZMin)
    
    'TwipsToPixelsX modelwidth
    
    
End Sub

'Public Sub ZoomWheel(ByVal lValue As Long, ByVal y As Long, z As Long)
'    'On Error GoTo  ZoomWheelErr
'    On Error Resume Next
'    Dim objShape As New cRectWND
'    Set objShape = Project.Item(ActiveProject).frmProject.cRectWND
'    Dim Y_Dir As Long, Z_Dir As Long
'    Dim ZoomValue As Long
'    Dim sc As Long
'    sc = getScale(ActiveProject)
'
'    ModelSize
'
'    If y > Ymax Then
''        y = Ymax
'    End If
'    If y < Ymin Then
' '       y = Ymin
'    End If
'
'    ZoomValue = ZOOM_FACTOR * modelDiag * sc / 200 '+ sc
'
'    Y_Dir = (Project.Item(ActiveProject).frmProject.picScreen.ScaleWidth / 2 - y) * 0.85 '/ 2
'    Z_Dir = (Project.Item(ActiveProject).frmProject.picScreen.ScaleHeight / 2 - z) * 0.85 '/ 2
'    Select Case lValue
'        Case Is < 0 ' zoom out
'            With objShape
'                If .Width > ZoomValue And .Height > ZoomValue Then
'                    .Width = (.Width - ZoomValue)
'                    .Height = (.Height - ZoomValue)
'                    .Left = .Left + CLng(ZoomValue / 2) - Y_Dir
'                    .Top = .Top + CLng(ZoomValue / 2) - Z_Dir
'                End If
'            End With
'        Case Is > 0 ' zoom in
'            With objShape
'                .Width = .Width + CLng(ZoomValue)
'                .Height = .Height + CLng(ZoomValue)
'                .Left = .Left - CLng(ZoomValue / 2) + Y_Dir
'                .Top = .Top - CLng(ZoomValue / 2) + Z_Dir
'            End With
'    End Select
'    Dim diag As Long
'    diag = modelDiag
'
'    UpdateCoordinates objShape
'    Set objShape = Nothing
'    Exit Sub
'ZoomWheelErr:
'    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modScreenFunctions: Sub ZoomWheel")
'End Sub

Public Sub ZoomIn(ByVal y As Single, z As Single)
    'On Error GoTo  ZoomInErr
    On Error Resume Next
    Dim objShape As New cRectWND
    Set objShape = Project.Item(ActiveProject).frmProject.cRectWND
    Dim Y_Dir As Long, Z_Dir As Long
    Dim ZoomValue As Long
    ModelSize
    ZoomValue = ZOOM_FACTOR * modelDiag
    'ZoomValue = getScale(ActiveProject) + Project.Item(ActiveProject).GetModelHeight * Project.Item(ActiveProject).GetModelWidth / 2
    Y_Dir = Project.Item(ActiveProject).frmProject.picScreen.ScaleWidth / 2 - y
    Z_Dir = Project.Item(ActiveProject).frmProject.picScreen.ScaleHeight / 2 - z
    With objShape
        .Width = .Width + ZoomValue
        .Height = .Height + ZoomValue
        .Left = .Left - ZoomValue / 2 + Y_Dir
        .Top = .Top - ZoomValue / 2 + Z_Dir
    End With
    'MsgBox getScale(ActiveProject)
    UpdateCoordinates objShape
    Set objShape = Nothing
    Exit Sub
ZoomInErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modScreenFunctions: Sub ZoomIn")
End Sub

Public Sub ZoomOut(ByVal y As Single, ByVal z As Single)
    'On Error GoTo  ZoomOutErr
    On Error Resume Next
    Dim objShape As New cRectWND
    Set objShape = Project.Item(ActiveProject).frmProject.cRectWND
    Dim Y_Dir As Long, Z_Dir As Long
'    Dim sc As Double
    Dim ZoomValue As Long
    ModelSize
    ZoomValue = ZOOM_FACTOR * modelDiag

 '   sc = getScale(ActiveProject)
 '   ZoomValue = getScale(ActiveProject) + Project.Item(ActiveProject).GetModelHeight * Project.Item(ActiveProject).GetModelWidth / 2
    Y_Dir = Project.Item(ActiveProject).frmProject.picScreen.ScaleWidth / 2 - y
    Z_Dir = Project.Item(ActiveProject).frmProject.picScreen.ScaleHeight / 2 - z
    With objShape
        If .Width > ZoomValue And .Height > ZoomValue Then
            .Width = (.Width - ZoomValue)
            .Height = (.Height - ZoomValue)
            .Left = .Left + ZoomValue / 2 - Y_Dir '+ Y_Dir
            .Top = .Top + ZoomValue / 2 - Z_Dir '+ Z_Dir
        End If
    End With
    UpdateCoordinates objShape
    Set objShape = Nothing
    Exit Sub
ZoomOutErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modScreenFunctions: Sub ZoomOut")
End Sub

Public Sub ZoomUpDown(ByVal y As Single, ByVal z As Single, ByVal Y0 As Single, ByVal Z0 As Single)
    On Error Resume Next
    Dim objShape As New cRectWND
    Set objShape = Project.Item(ActiveProject).frmProject.cRectWND
    Dim sUpDown As Single
'    Static Z1 As Single
'    If Z1 > Z Then
'        Z = 0
    
    sUpDown = z - Z0
    
    With objShape
        If .Width > 0 And .Height > 0 Then
            .Width = .Width + sUpDown
            .Height = .Height + sUpDown
        Else
            .Width = 1
            .Height = 1
        End If
        '.Left = .Left - sUpDown / 2
        '.Top = .Top - sUpDown / 2
        '.Left = Project.Item(ActiveProject).frmProject.picScreen.ScaleLeft / 2
        
    End With
    UpdateCoordinates objShape
    Set objShape = Nothing
    'Z1 = Z
End Sub

Public Sub ZoomWin(ByVal lWidth As Long, lHeight As Long, ByVal lLeft As Long, ByVal lTop As Long)
    On Error GoTo ZoomWinErr
    Dim objShape As New cRectWND
    Dim lScaleWidth As Double, lScaleHeight As Double
    Dim LL As Double, TT As Double, ZOOM As Double
    lScaleWidth = Project.Item(ActiveProject).frmProject.picScreen.ScaleWidth
    lScaleHeight = Project.Item(ActiveProject).frmProject.picScreen.ScaleHeight
    Set objShape = Project.Item(ActiveProject).frmProject.cRectWND
    If (lScaleWidth / objShape.Width) > (lScaleHeight / objShape.Height) Then
        ZOOM = (lScaleHeight / objShape.Height)
    Else
        ZOOM = (lScaleWidth / objShape.Width)
    End If
    
    If ZOOM * lWidth > 1500000 Then
        ZOOM = 1500000 / lWidth
    End If
    If ZOOM * lHeight > 1500000 Then
        ZOOM = 1500000 / lHeight
    End If

    If lScaleWidth > lScaleHeight And objShape.Width > objShape.Height Then
        If lScaleWidth / objShape.Width > lScaleHeight / objShape.Height Then
            LL = (lScaleWidth - objShape.Width * ZOOM) / 2
        Else
            TT = (lScaleHeight - objShape.Height * ZOOM) / 2
        End If
    End If
    If lScaleWidth > lScaleHeight And objShape.Width <= objShape.Height Then
        If lScaleWidth / objShape.Width > lScaleHeight / objShape.Height Then
            LL = (lScaleWidth - objShape.Width * ZOOM) / 2
        Else
            TT = (lScaleHeight - objShape.Height * ZOOM) / 2
        End If
    End If
    If lScaleWidth <= lScaleHeight And objShape.Width > objShape.Height Then
        If lScaleWidth / objShape.Width > lScaleHeight / objShape.Height Then
            LL = (lScaleWidth - objShape.Width * ZOOM) / 2
        Else
            TT = (lScaleHeight - objShape.Height * ZOOM) / 2
        End If
    End If
    If lScaleWidth <= lScaleHeight And objShape.Width <= objShape.Height Then
        If lScaleWidth / objShape.Width > lScaleHeight / objShape.Height Then
            LL = (lScaleWidth - objShape.Width * ZOOM) / 2
        Else
            TT = (lScaleHeight - objShape.Height * ZOOM) / 2
        End If
    End If
    With objShape
        .Height = lHeight * ZOOM
        .Width = lWidth * ZOOM
        .Left = -(.Left - lLeft) * ZOOM + LL
        .Top = -(.Top - lTop) * ZOOM + TT
    End With
    UpdateCoordinates objShape
    Project.Item(ActiveProject).frmProject.Border.Visible = False
    Exit Sub
ZoomWinErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "mosScreenFunctions: Sub ZoomWin")
End Sub

Public Sub pan(ByVal y As Single, ByVal z As Single, ByVal Y_BUFFER As Single, ByVal Z_BUFFER As Single)
    'On Error GoTo  PanErr
    On Error Resume Next
    Dim objShape As New cRectWND
    Set objShape = Project.Item(ActiveProject).frmProject.cRectWND
    Dim Y_Dir As Long, Z_Dir As Long
    Y_Dir = Project.Item(ActiveProject).frmProject.picScreen.ScaleWidth / 2 - Y_BUFFER
    Z_Dir = Project.Item(ActiveProject).frmProject.picScreen.ScaleHeight / 2 - Z_BUFFER
    With objShape
        .Left = .Left + y - Y_BUFFER
        .Top = .Top + z - Z_BUFFER
    End With
    UpdateCoordinates objShape
    Set objShape = Nothing
    Exit Sub
PanErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modScreenFunctions: Sub Pan")
End Sub

Public Sub FindSelectedPanel(ByVal y As Integer, ByVal z As Integer)
    On Error GoTo FindSelectedPanelErr
    Dim Panel As cPanel
    For Each Panel In Project.Item(ActiveProject).colPanel
        Panel.Selected = IsUnselected
    Next Panel
    For Each Panel In Project.Item(ActiveProject).colPanel
        Panel.Selected = PtInRegion(Panel.Region, y, z)
        If Panel.Selected = isSelected Then Exit For
    Next Panel
    Exit Sub
FindSelectedPanelErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modScreenFunctions: Sub FindSelectedPanel")
End Sub

Public Sub FindSelectedPanels(ByVal y As Integer, ByVal z As Integer)
    On Error GoTo FindSelectedPanelsErr
    Dim Panel As cPanel
    For Each Panel In Project.Item(ActiveProject).colPanel
        If Panel.Selected = IsUnselected And PtInRegion(Panel.Region, y, z) Then
            Panel.Selected = isSelected
            Exit For
        End If
        If Panel.Selected = isSelected And PtInRegion(Panel.Region, y, z) Then
            Panel.Selected = IsUnselected
            Exit For
        End If
    Next Panel
    Exit Sub
FindSelectedPanelsErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modScreenFunctions: Sub FindSelectedPanels")
End Sub

Public Sub FindSelectedNode(ByVal y As Integer, ByVal z As Integer)
    On Error GoTo FindSelectedPanelErr
    Dim Node As cNode
    For Each Node In Project.Item(ActiveProject).colNodes
        Node.Selected = IsUnselected
    Next Node
    For Each Node In Project.Item(ActiveProject).colNodes
        Node.Selected = PtInRegion(Node.Region, y, z)
            If Node.Selected = isSelected Then
                Project.Item(ActiveProject).LastNode = Node.nNumber
                Exit For
            End If
    Next Node
    Exit Sub
FindSelectedPanelErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modScreenFunctions: Sub FindSelectedPanel")
End Sub

Public Sub setScreenMode(eData As ScreenModes)
    Dim sMode As String
    Project.Item(ActiveProject).ScreenMode = eData
    sMode = "SCREEN MODE= "
    Select Case eData
        Case -1
            sMode = sMode + "NORMAL"
            Project.Item(ActiveProject).frmProject.MouseIcon = LoadResPicture("ID_CURSOR_NORMAL", 2)
            UnselectAll
        Case 0
            sMode = sMode + "SINGLE SELECTION"
            Project.Item(ActiveProject).frmProject.MouseIcon = LoadResPicture("ID_CURSOR_SELECT", 2)
        Case 1
            sMode = sMode + "MULTIPLE SELECTION"
            Project.Item(ActiveProject).frmProject.MouseIcon = LoadResPicture("ID_CURSOR_SELECT_MULTIPLE", 2)
        Case 2
            sMode = sMode + "ZOOM IN"
            Project.Item(ActiveProject).frmProject.MouseIcon = LoadResPicture("ID_CURSOR_ZOOM_IN", 2)
        Case 3
            sMode = sMode + "ZOOM OUT"
            Project.Item(ActiveProject).frmProject.MouseIcon = LoadResPicture("ID_CURSOR_ZOOM_OUT", 2)
        Case 4
            sMode = sMode + "ZOOM WIN"
            Project.Item(ActiveProject).frmProject.MouseIcon = LoadResPicture("ID_CURSOR_CROSS", 2)
        Case 5
            sMode = sMode + "PAN"
            Project.Item(ActiveProject).frmProject.MouseIcon = LoadResPicture("ID_CURSOR_PAN", 2)
        Case 6
            sMode = sMode + "ZOOM FULL"
        Case 7
            sMode = sMode + "ZOOM UP DOWN"
        Project.Item(ActiveProject).frmProject.MouseIcon = LoadResPicture("ID_CURSOR_ZOOM_IN", 2)
    End Select
    Project.Item(ActiveProject).frmProject.StatusBar.Panels(2).Text = sMode
End Sub

Public Sub setFunctionMode(eData As FunctionModes)
    Dim sMode As String
    Project.Item(ActiveProject).FunctionMode = eData
    sMode = "FUNCTION MODE= "
    Select Case eData
        Case 10
            sMode = sMode + "NONE"
        Case 11
            sMode = sMode + "Add Plate: First Node (Press Escape to cancel)"
            Project.Item(ActiveProject).frmProject.MouseIcon = LoadResPicture("ID_CURSOR_ADD_FIRST_NODE", 2)
        Case 12
            sMode = sMode + "Add Plate: Second Node (Press Escape to cancel)"
            Project.Item(ActiveProject).frmProject.MouseIcon = LoadResPicture("ID_CURSOR_ADD_SECOND_NODE", 2)
        Case 13
            sMode = sMode + "Add Beam: First Node (Press Escape to cancel)"
            Project.Item(ActiveProject).frmProject.MouseIcon = LoadResPicture("ID_CURSOR_ADD_FIRST_NODE", 2)
        Case 14
            sMode = sMode + "Add Beam: Second Node (Press Escape to cancel)"
            Project.Item(ActiveProject).frmProject.MouseIcon = LoadResPicture("ID_CURSOR_ADD_SECOND_NODE", 2)
        Case 15
            sMode = sMode + "Add Double Hull: First Plate (Press Escape to cancel)"
            Project.Item(ActiveProject).frmProject.MouseIcon = LoadResPicture("ID_CURSOR_SELECT", 2)
        Case 16
            sMode = sMode + "Add Double Hull: Second Plate (Press Escape to cancel)"
            Project.Item(ActiveProject).frmProject.MouseIcon = LoadResPicture("ID_CURSOR_SELECT", 2)
        Case 17
            sMode = sMode + "ERASE"
            Project.Item(ActiveProject).frmProject.MouseIcon = LoadResPicture("ID_CURSOR_ERASER", 2)
        Case 18
            sMode = sMode + "Explode Double Hull: Select Double Hull (Press Escape to cancel)"
            Project.Item(ActiveProject).frmProject.MouseIcon = LoadResPicture("ID_CURSOR_SELECT", 2)
        Case 19
            sMode = sMode + "Reverse Panel: Select Panel (Press Escape to cancel)"
            Project.Item(ActiveProject).frmProject.MouseIcon = LoadResPicture("ID_CURSOR_SELECT", 2)
        Case 20
            sMode = sMode + "Divide Panel: Select Panel (Press Escape to cancel)"
            Project.Item(ActiveProject).frmProject.MouseIcon = LoadResPicture("ID_CURSOR_SELECT", 2)
        Case 21
            sMode = sMode + "Get Distance: Select First Node (Press Escape to cancel)"
            Project.Item(ActiveProject).frmProject.MouseIcon = LoadResPicture("ID_CURSOR_SELECT", 2)
        Case 22
            sMode = sMode + "Get Distance: Select Second Node (Press Escape to cancel)"
            Project.Item(ActiveProject).frmProject.MouseIcon = LoadResPicture("ID_CURSOR_SELECT", 2)
        Case 23
            sMode = sMode + "Match Properties: Select Original Panel (Press Escape to cancel)"
            Project.Item(ActiveProject).frmProject.MouseIcon = LoadResPicture("ID_CURSOR_SELECT", 2)
        Case 24
            sMode = sMode + "Match Properties: Select Panel(s) to inherit properties (Press Escape to cancel)"
            Project.Item(ActiveProject).frmProject.MouseIcon = LoadResPicture("ID_MATCH_PROPERTIES", 2)
        Case 25 ' Panel type: Inner Double Hull
            sMode = sMode + "Select Inner Side Panels (Right Click to Apply; Escape to cancel)"
            Project.Item(ActiveProject).frmProject.MouseIcon = LoadResPicture("ID_CURSOR_SELECT", 2)
        Case 26 ' Panel type: Outer Double Hull
            sMode = sMode + "Select Outer Side Panels (Right Click to Apply; Escape to cancel)"
            Project.Item(ActiveProject).frmProject.MouseIcon = LoadResPicture("ID_CURSOR_SELECT", 2)
        Case 27 ' Panel type: Carlingue / Serre / Hiloire
            sMode = sMode + "Select Primary Longitudinal Members (Right Click to Apply; Escape to cancel)"
            Project.Item(ActiveProject).frmProject.MouseIcon = LoadResPicture("ID_CURSOR_SELECT", 2)
        Case 28 ' Panel type : Bouchain
            sMode = sMode + "Select Bilge Panels (Right Click to Apply; Escape to cancel)"
            Project.Item(ActiveProject).frmProject.MouseIcon = LoadResPicture("ID_CURSOR_SELECT", 2)
        Case 29 ' Panel type : Bordé simple
            sMode = sMode + "Select Simple Hull Panels (Right Click to Apply; Escape to cancel)"
            Project.Item(ActiveProject).frmProject.MouseIcon = LoadResPicture("ID_CURSOR_SELECT", 2)
        Case 30 ' Add Nappe
            sMode = sMode + "Select Panels to form Plane Panel - Nappe (Right Click to Apply; Escape to cancel)"
            Project.Item(ActiveProject).frmProject.MouseIcon = LoadResPicture("ID_CURSOR_SELECT", 2)
        Case 31 'Add Virtual Panels
            sMode = sMode + "Select Virtual Panels (Right Click to Apply; Escape to cancel)"
            Project.Item(ActiveProject).frmProject.MouseIcon = LoadResPicture("ID_CURSOR_SELECT", 2)
        Case 32 'Add girder intermediary frame on plate panel
            sMode = sMode + "Select Plate Panels - Nappes (Right Click to Apply; Escape to cancel)"
            Project.Item(ActiveProject).frmProject.MouseIcon = LoadResPicture("ID_CURSOR_SELECT", 2)
        Case 33 'Select node
            sMode = sMode + "Add Boundary: Select Node (Press Escape to cancel)"
            Project.Item(ActiveProject).frmProject.MouseIcon = LoadResPicture("ID_CURSOR_SELECT_NODE", 2)
        Case 34
            sMode = sMode + "Redistribute Loads: Select First Plate  (Press Escape to cancel)"
             Project.Item(ActiveProject).frmProject.MouseIcon = LoadResPicture("ID_CURSOR_SELECT", 2)
        Case 35
            sMode = sMode + "Redistribute Loads: Select Second Plate  (Press Escape to cancel)"
             Project.Item(ActiveProject).frmProject.MouseIcon = LoadResPicture("ID_CURSOR_SELECT", 2)
    End Select
    Project.Item(ActiveProject).frmProject.StatusBar.Panels(2).Text = sMode
End Sub

Public Sub UnselectAll()
    Dim Panel As cPanel
    Dim colPanel As colPanel
    Set colPanel = Project.Item(ActiveProject).colPanel
    For Each Panel In colPanel
        Panel.Selected = IsUnselected
    Next Panel
    Set colPanel = Nothing
    Dim Node As cNode
    Dim colNodes As colNodes
    Set colNodes = Project.Item(ActiveProject).colNodes
    For Each Node In colNodes
        If Node.Selected = isSelected Then
            Node.Selected = IsUnselected
            Exit For
        End If
    Next Node
    Set colNodes = Nothing
    Draw ActiveProject
End Sub

Public Sub ResizeBorder(ByVal y As Single, ByVal z As Single, ByVal Y_BUFFER As Single, ByVal Z_BUFFER As Single)
    On Error Resume Next
    'Dim objShape As New cRectWND
    Dim objShape As Shape
    'Set objShape = Project.Item(ActiveProject).frmProject.cRectWND
    Set objShape = Project.Item(ActiveProject).frmProject.Border
    With objShape
        Select Case y
            Case Is <= Y_BUFFER
                .Left = y
                .Width = Y_BUFFER - y
            Case Is > Y_BUFFER
                .Left = Y_BUFFER
                .Width = y - Y_BUFFER
        End Select
        Select Case z
            Case Is <= Z_BUFFER
                .Top = z
                .Height = Z_BUFFER - z
            Case Is > Z_BUFFER
                .Top = Z_BUFFER
                .Height = z - Z_BUFFER
        End Select
    End With
    With Project.Item(ActiveProject).frmProject.cRectWND
        .Height = objShape.Height
        .Top = objShape.Top
        .Left = objShape.Left
        .Width = objShape.Width
    End With
    Project.Item(ActiveProject).frmProject.Border.Visible = True
End Sub

Public Function getScale(ByVal index As Integer) As Double
    'On Error GoTo  getScaleErr
    Dim colNodes As colNodes, Node1 As cNode, Node2 As cNode
    Set colNodes = Project.Item(index).colNodes
    If colNodes.Count < 2 Then Exit Function
    Set Node1 = colNodes.Item(1)
    Set Node2 = colNodes.Item(2)
    getScale = Divide(Sqr((Node2.Y_Screen - Node1.Y_Screen) ^ 2 + _
                (Node2.Z_Screen - Node1.Z_Screen) ^ 2), Sqr((Node2.y - _
                Node1.y) ^ 2 + (Node2.z - Node1.z) ^ 2))
    Set Node1 = Nothing
    Set Node2 = Nothing
    Set colNodes = Nothing
    Exit Function
getScaleErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modDraw: Function getScale")
End Function

Public Function HighlightPassedOverItem(ByVal y As Integer, ByVal z As Integer)
'    On Error GoTo  HighlightPassedOverItemErr
'    Dim cPanel As cPanel
'    Dim cNode As cNode
'    For Each cPanel In Project.Item(ActiveProject).colPanel
'        cPanel.HighLighted = No
'    Next cPanel
'    For Each cNode In Project.Item(ActiveProject).colNodes
'        cNode.HighLighted = No
'    Next cNode
'    For Each cNode In Project.Item(ActiveProject).colNodes
'        If PtInRegion(cNode.Region, Y, Z) = 1 Then
'            cNode.HighLighted = Yes
'            Draw
'            Exit Function
'        End If
'    Next cNode
'    For Each cPanel In Project.Item(ActiveProject).colPanel
'        If PtInRegion(cPanel.Region, Y, Z) = 1 Then
'            cPanel.HighLighted = Yes
'            Exit For
'        End If
'    Next cPanel
'    Draw
'    Exit Function
'HighlightPassedOverItemErr:
'    Call RaiseError(MyUnhandledError,Err.Description & "." & vbCrLf &  "modScreenFunctions: Function HighlightPassedOverItem")
End Function
