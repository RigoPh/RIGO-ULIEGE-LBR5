Attribute VB_Name = "modDraw"
Option Explicit
Dim Panel As cPanel
Dim Node As cNode, colNodes As colNodes
Dim colVortex As colNodes
Public lp() As POINTAPI, lrect As RECT
Dim size As Single
Dim Pic As Long
Dim CurrentScale As Double
Dim ProjectIndex As Integer
Dim IANA As Integer

Public Sub Draw(ByVal index As Integer)
    'On Error GoTo  DrawErr:
    Dim i As Long

    ProjectIndex = index
    IANA = Project.Item(ProjectIndex).cHeader.IANA
    
'    If frmOpenGLVisible = True Then
'        Draw3D ProjectIndex
'        'DrawGLScene
'        ReSizeGLScene Project.Item(ProjectIndex).frmOpenGL.ScaleWidth, Project.Item(ProjectIndex).frmOpenGL.ScaleHeight
'    End If

'    StartTime Timer
'    For i = 1 To 500
    'Project.Item(ProjectIndex).frmProject.picScreen.AutoRedraw = False
    
        Dim col As New colPanel
        Select Case Project.Item(ProjectIndex).cSolution.ShowUpdatedScantling
            Case False
                Set col = Project.Item(ProjectIndex).colPanel
            Case True
                Set col = Project.Item(ProjectIndex).colPanelUpdate
        End Select
        'Set col = Project.Item(ProjectIndex).colPanel
        Set colNodes = Project.Item(ProjectIndex).colNodes
        DrawScreen
        If ProjectIndex = 0 Then Exit Sub
        Pic = Project.Item(ProjectIndex).frmProject.picScreen.hdc
        If col.Count > 0 Then CurrentScale = getScale(ProjectIndex)
        DrawGrid
        For Each Panel In col
            DrawPrimaryFrames Panel
        Next Panel
        For Each Panel In col
            DrawSecondaryFrames Panel
        Next Panel
        For Each Panel In col
            DrawDoubleHullFrames Panel
        Next Panel
'        For Each Panel In col
'            DrawPressuresUniformelyDistributed Panel
'        Next Panel
'        For Each Panel In col
'            DrawLocalPressures Panel
'        Next Panel
        For Each Panel In col
            DrawPlates Panel
        Next Panel
        For Each Panel In col
            DrawBeams Panel
        Next Panel
        For Each Panel In col
            DrawPressuresUniformelyDistributed Panel
        Next Panel
        For Each Panel In col
            DrawLocalPressures Panel
        Next Panel

'        If Project.Item(ProjectIndex).EconomicDraw = False Then
            For Each Panel In col
                DrawPrimaryStiffeners Panel
            Next Panel
            For Each Panel In col
                DrawSecondaryStiffeners Panel
            Next Panel
            For Each Panel In col
                DrawBoundaryConditions Panel
            Next Panel
            For Each Panel In col
                DrawGirders Panel
            Next Panel
            For Each Node In colNodes
                DrawNodes Node
            Next Node
            For Each Panel In col
                DrawSelections Panel
            Next Panel
            For Each Panel In col
                DrawFleshes Panel
            Next Panel
            For Each Panel In col
                DrawParticipation Panel
            Next Panel
'            If Project.Item(ProjectIndex).cSolution.ShowSolution = True Then
'                DrawSolution
'            End If
            For Each Panel In col
                TextPrimaryFrames Panel
            Next Panel
            For Each Panel In col
                TextSecondaryFrames Panel
            Next Panel
            For Each Panel In col
                TextPlates Panel
            Next Panel
            For Each Panel In col
                TextBeams Panel
            Next Panel
            For Each Panel In col
                TextPrimaryStiffeners Panel
            Next Panel
            For Each Panel In col
                TextSecondaryStiffeners Panel
            Next Panel
            For Each Panel In col
                TextPressuresUniformelyDistributed Panel
            Next Panel
            For Each Panel In col
                TextLocalizedPressures Panel
            Next Panel
            For Each Panel In col
                TextGirders Panel
            Next Panel
            For Each Node In colNodes
                TextNodes Node
            Next Node
            If Project.Item(ProjectIndex).cSolution.ShowSolution = True Then
                DrawSolution
            End If
            For Each Panel In col
                TextSolutionSelected Panel
            Next Panel
'        End If
        'TEMP
        'Project.Item(ProjectIndex).frmProject.StatusBar.Panels(1).Text = "SCALE= " & Round(CurrentScale, 1)
        DrawCoordOrigin
        DrawNeutralAxis
        DrawLegendItems
        Project.Item(ProjectIndex).frmProject.picScreen.Refresh
        Project.Item(ProjectIndex).frmProject.picScreen.Visible = True
        
        Set col = Nothing
        Set colNodes = Nothing
        
'    Next i
'    ElapsedTime Timer, 500
    Exit Sub
DrawErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modDraw: Sub Draw")
End Sub


Private Sub DrawLegendItems()
    On Error Resume Next
    If Project.Item(ProjectIndex).cDisplaySettings.ShowLegend = no Then Exit Sub
    Dim lTop As Long, lLeft As Long, hBrush As Long, s As String
    Dim iItem As Integer, sItemName As String
    lTop = 10
    lLeft = Project.Item(ProjectIndex).frmProject.picScreen.ScaleWidth - 260 '- 250
    
    With Project.Item(ProjectIndex).cDisplaySettings
        'Nodes
        If .DrawNodes = yes Then
            iItem = .TextNodes: hBrush = .ColorNodes: sItemName = "Nodes"
            DrawItem lTop, lLeft, iItem, sItemName, hBrush
            lTop = lTop + 15
        End If
        'Plates
        If .DrawPlates = yes Then
            iItem = .TextPlates: hBrush = .ColorPlates: sItemName = "Plates"
            DrawItem lTop, lLeft, iItem, sItemName, hBrush
            lTop = lTop + 15
        End If
        'Beams
        If .DrawBeams = yes Then
            iItem = .TextBeams: hBrush = .ColorBeams: sItemName = "Beams"
            DrawItem lTop, lLeft, iItem, sItemName, hBrush
            lTop = lTop + 15
        End If
        'Primary Frames
        If .DrawPrimaryFrames = yes Then
            iItem = .TextPrimaryFrames: hBrush = .ColorPrimaryFrames: sItemName = "Primary Frames"
            DrawItem lTop, lLeft, iItem, sItemName, hBrush
            lTop = lTop + 15
        End If
        'Primary Stiffeners
        If .DrawPrimaryStiffeners = yes Then
            iItem = .TextPrimaryStiffeners: hBrush = .ColorPrimaryStiffeners: sItemName = "Primary Stiffeners"
            DrawItem lTop, lLeft, iItem, sItemName, hBrush
            lTop = lTop + 15
        End If
        'Secondary Frames
        If .DrawSecondaryFrames = yes Then
            iItem = .TextSecondaryFrames: hBrush = .ColorSecondaryFrames: sItemName = "Secondary Frames"
            DrawItem lTop, lLeft, iItem, sItemName, hBrush
            lTop = lTop + 15
        End If
        'Secondary Stiffeners
        If .DrawSecondaryStiffeners = yes Then
            iItem = .TextSecondaryStiffeners: hBrush = .ColorSecondaryStiffeners: sItemName = "Secondary Stiffeners"
            DrawItem lTop, lLeft, iItem, sItemName, hBrush
            lTop = lTop + 15
        End If
        'Girders
        If .DrawGirders = yes Then
            iItem = .TextGirders: hBrush = .ColorGirders: sItemName = "Girders"
            DrawItem lTop, lLeft, iItem, sItemName, hBrush
            lTop = lTop + 15
        End If
        'Lateral Pressures
        If .DrawPressuresUniformlyDistributed = yes Then
            If .CurrentLoadCase <> 0 Then
                iItem = .TextPressuresUniformlyDistributed: hBrush = .ColorPressuresUniformlyDistributed: sItemName = "Lateral Pressures"
                DrawItem lTop, lLeft, iItem, sItemName, hBrush
                lTop = lTop + 15
            End If
        End If
        'Boundary Conditions
        If .DrawBoundaryConditions = yes Then
            iItem = .TextBoundaryConditions: hBrush = .ColorBoundaryConditions: sItemName = "Boundary Conditions"
            DrawItem lTop, lLeft, iItem, sItemName, hBrush
            lTop = lTop + 15
        End If
        'Panel Flow
        If .DrawFleshes = yes Then
            iItem = 0: hBrush = .ColorFleshes: sItemName = "Panel Flow"
            DrawItem lTop, lLeft, iItem, sItemName, hBrush
            lTop = lTop + 15
        End If
        ' Bending Moments
        lTop = lTop + 15
        TextBendingMoments lTop, lLeft
    End With
End Sub

Private Sub DrawItem(ByVal lTop As Long, ByVal lLeft As Long, ByVal iItem As Integer, ByVal sItemName As String, _
                    hBrush As Long)
    On Error GoTo DrawLegendErr
    Dim lSquare As Long
    Dim rgn As Long
    Dim lrect As RECT
    lSquare = 13
    ReDim lp(4)
        lp(0).y = lLeft: lp(0).z = lTop
        lp(1).y = lp(0).y + lSquare: lp(1).z = lTop
        lp(2).y = lp(0).y + lSquare: lp(2).z = lTop + lSquare
        lp(3).y = lp(0).y: lp(3).z = lTop + lSquare
        lp(4).y = lLeft: lp(4).z = lTop
        hBrush = apiCreateSolidBrush(hBrush)
        rgn = CreatePolygonRgn(lp(0), 4, 1)
        FillRgn Project.Item(ProjectIndex).frmProject.picScreen.hdc, rgn, hBrush
        Polyline Project.Item(ProjectIndex).frmProject.picScreen.hdc, lp(0), 5
        DeleteObject hBrush
        DeleteObject rgn
        lrect.bottom = lp(2).z
        lrect.Top = lp(0).z
        lrect.Left = lp(1).y + 10
        lrect.right = lp(1).y + 250
        Dim s As String
        s = Project.Item(ProjectIndex).cDisplaySettings.getTextOptionsString(iItem)
        If s <> "None" Then
            s = sItemName & " [" & s & "]"
        Else
            s = sItemName
        End If
        DrawText Project.Item(ProjectIndex).frmProject.picScreen.hdc, s, -1, lrect, 4
    Exit Sub
DrawLegendErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modDraw: Sub DrawItem")
End Sub

Private Sub DrawNeutralAxis()
    If Project.Item(ProjectIndex).cSolution.IsSolution = False Then Exit Sub
    If Project.Item(ProjectIndex).cSolution.ShowNeutralAxis = False Then Exit Sub
    'If Project.Item(ProjectIndex).colPanel.Count = 1 Then Exit Sub
    ReDim lp(1)
'    lp(0).Y = Project.Item(ProjectIndex).Ymin * getScale(ProjectIndex)
'    lp(1).Y = Project.Item(ProjectIndex).Ymax * getScale(ProjectIndex)
    lp(0).y = Project.Item(ProjectIndex).frmProject.cRectWND.Left
    lp(1).y = Project.Item(ProjectIndex).frmProject.cRectWND.Left + Project.Item(ProjectIndex).frmProject.cRectWND.Width
    'Project.Item(ProjectIndex).colNodes.Item(Project.Item(ProjectIndex).colPanel.Item(1).cGeometry.OutNode).Z
    Dim a As Double, b As Double, c As Double, d As Double
    a = Project.Item(ProjectIndex).colNodes.Item(Project.Item(ProjectIndex).colPanel.Item(1).cGeometry.OutNode).Z_Screen
    If IANA = 1 Then
        b = (Project.Item(ProjectIndex).cSolution.NeutralAxis * getScale(ProjectIndex))
    ElseIf IANA = 2 Then
        If Project.Item(ProjectIndex).cSolution.Thickness = Net Then
            b = (Project.Item(ProjectIndex).cSolution.NeutralAxis * getScale(ProjectIndex))
        ElseIf Project.Item(ProjectIndex).cSolution.Thickness = Gross Then
            b = (Project.Item(ProjectIndex).cSolution.NeutralAxisGross * getScale(ProjectIndex))
        End If
    End If
    c = Project.Item(ProjectIndex).colNodes.Item(Project.Item(ProjectIndex).colPanel.Item(1).cGeometry.OutNode).z * getScale(ProjectIndex)
    lp(0).z = a - b - c
    lp(1).z = lp(0).z
    Project.Item(ProjectIndex).frmProject.picScreen.DrawStyle = 3
    DrawPolyline 2, vbRed
    Project.Item(ProjectIndex).frmProject.picScreen.DrawStyle = 0
    Dim s As String
    If IANA = 1 Then
        s = "zn = " & Format(Project.Item(ProjectIndex).cSolution.NeutralAxis, "0.000") & " [m]"
    ElseIf IANA = 2 Then
        If Project.Item(ProjectIndex).cSolution.Thickness = Net Then
            s = "zn = " & Format(Project.Item(ProjectIndex).cSolution.NeutralAxis, "0.000") & " [m]"
        ElseIf Project.Item(ProjectIndex).cSolution.Thickness = Gross Then
            s = "zn = " & Format(Project.Item(ProjectIndex).cSolution.NeutralAxisGross, "0.000") & " [m]"
        End If
    End If
    
    lrect.Top = lp(0).z - 18
    lrect.bottom = lp(0).z - 3
    lrect.Left = lp(0).y
    lrect.right = lp(0).y + Len(s) * 6 '(6 + 5 / 2)
    DrawTxt s, vbYellow, vbRed
    
    'Text Inertia
    If IANA = 1 Then
        s = "Iyy = " & Format(Project.Item(ProjectIndex).cSolution.Iyy, "0.000") & " [m4]"
    ElseIf IANA = 2 Then
        If Project.Item(ProjectIndex).cSolution.Thickness = Net Then
            s = "Iyy = " & Format(Project.Item(ProjectIndex).cSolution.Iyy, "0.000") & " [m4]"
        ElseIf Project.Item(ProjectIndex).cSolution.Thickness = Gross Then
            s = "Iyy = " & Format(Project.Item(ProjectIndex).cSolution.IyyGross, "0.000") & " [m4]"
        End If
    End If
    lrect.Top = lp(0).z + 3
    lrect.bottom = lp(0).z + 18
    lrect.Left = lp(0).y
    lrect.right = lp(0).y + Len(s) * 6 '(6 + 5 / 2)
    DrawTxt s, vbYellow, vbRed
   
End Sub

Private Sub TextBendingMoments(ByVal lTop As Long, ByVal lLeft As Long)
    On Error GoTo TextBendingMomentsErr
    Dim sBendingMoments As String
    Dim iLoadCase As Integer
    Dim sMomentType As String
    iLoadCase = Project.Item(ProjectIndex).cDisplaySettings.CurrentLoadCase
    If iLoadCase = 0 Then Exit Sub
    lrect.Top = lTop
    lrect.bottom = lTop + 92
    lrect.Left = lLeft
    lrect.right = lLeft + 140
    
    Select Case Project.Item(ProjectIndex).cHeader.colLoadCase.Item(iLoadCase).VerticalBendingMomentFore
        Case Is > 0
            sMomentType = "(HOGGING)"
        Case Is < 0
            sMomentType = "(SAGGING)"
        Case Is = 0
            sMomentType = ""
    End Select
    Dim s As String, s1 As String, s2 As String
    If Project.Item(ProjectIndex).cHeader.colLoadCase.Item(iLoadCase).VerticalBendingMomentFore <> 0 Or _
            Project.Item(ProjectIndex).cHeader.colLoadCase.Item(iLoadCase).VerticalBendingMomentAft <> 0 Then
        s1 = "Vertical " & sMomentType & ": " & vbCr & _
                            GetSpaceFormat(Round(Abs(Project.Item(ProjectIndex).cHeader.colLoadCase.Item(iLoadCase).VerticalBendingMomentFore) / 1000, 6)) & " " & "[kNm]" & vbCr
'                            "Fore: " & GetSpaceFormat(Round(Abs(Project.Item(ProjectIndex).cHeader.colLoadCase.Item(iLoadCase).VerticalBendingMomentFore) / 1000, 6)) & " " & "[kNm]" & vbCr & _
'                            "Aft: " & GetSpaceFormat(Round(Abs(Project.Item(ProjectIndex).cHeader.colLoadCase.Item(iLoadCase).VerticalBendingMomentAft) / 1000, 6)) & " " & "[kNm]" & vbCr
    
    End If
    If Project.Item(ProjectIndex).cHeader.colLoadCase.Item(iLoadCase).HorizontalBendingMomentFore <> 0 Or _
            Project.Item(ProjectIndex).cHeader.colLoadCase.Item(iLoadCase).HorizontalBendingMomentAft <> 0 Then
        s2 = "Horizontal: " & vbCr & _
                            GetSpaceFormat(Round(Abs(Project.Item(ProjectIndex).cHeader.colLoadCase.Item(iLoadCase).HorizontalBendingMomentFore) / 1000, 6)) & " " & "[kNm]"
'                            "Fore: " & GetSpaceFormat(Round(Abs(Project.Item(ProjectIndex).cHeader.colLoadCase.Item(iLoadCase).HorizontalBendingMomentFore) / 1000, 6)) & " " & "[kNm]" & vbCr & _
'                            "Aft: " & GetSpaceFormat(Round(Abs(Project.Item(ProjectIndex).cHeader.colLoadCase.Item(iLoadCase).HorizontalBendingMomentAft) / 1000, 6)) & " " & "[kNm]"
    End If
    
    If s1 <> "" Or s2 <> "" Then
        s = "Bending Moments:" + vbCrLf + s1 + s2
    End If
    Select Case iLoadCase
        Case 0
        Case Else
            sBendingMoments = s
    End Select
    DrawText Pic, sBendingMoments, -1, lrect, 0

    'DrawTxt sBendingMoments, Project.Item(ProjectIndex).cDisplaySettings.ColorScreen, vbBlack 'Project.Item(ProjectIndex).cDisplaySettings.ColorPressuresUniformlyDistributed
    Exit Sub
TextBendingMomentsErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modDraw: Sub TextBendingMoments")
End Sub

Private Sub DrawCoordOrigin()
    Dim y1 As Long, z1 As Long, y2 As Long, z2 As Long, y3 As Long, z3 As Long
    If Project.Item(ProjectIndex).colNodes.Count = 0 Then Exit Sub
    y1 = Project.Item(ProjectIndex).CoordOrigin.Y_Screen
    z1 = Project.Item(ProjectIndex).CoordOrigin.Z_Screen
    Dim rgn As Long
    Dim hBrush As Long
    
    'Z Axis
    ReDim lp(3)
    lp(0).y = y1
    lp(0).z = z1
    lp(1).y = y1 + 10
    lp(1).z = z1
    lp(2).y = y1
    lp(2).z = z1 - 50
    lp(3).y = lp(0).y
    lp(3).z = lp(0).z
    
    lrect.Top = lp(2).z - 18
    lrect.bottom = lp(2).z - 3
    lrect.Left = lp(0).y
    lrect.right = lp(0).y + (6 + 5 / 2)
    Project.Item(ProjectIndex).frmProject.picScreen.ForeColor = vbRed
    DrawText Pic, "Z", 1, lrect, 1
    
    rgn = CreatePolygonRgn(lp(0), 3, 1)
    hBrush = vbRed
    hBrush = apiCreateSolidBrush(hBrush)
    FillRgn Pic, rgn, hBrush
    DrawPolyline 4, vbBlack
    DeleteObject rgn
    DeleteObject hBrush

    'Y Axis
    lp(0).y = y1
    lp(0).z = z1
    lp(1).y = y1 + 50
    lp(1).z = z1
    lp(2).y = y1
    lp(2).z = z1 - 10
    lp(3).y = lp(0).y
    lp(3).z = lp(0).z
    
    lrect.Top = lp(0).z - 18
    lrect.bottom = lp(0).z - 3
    lrect.Left = lp(1).y
    lrect.right = lp(1).y + (6 + 5 / 2)
    DrawText Pic, "Y", 1, lrect, 1

    rgn = CreatePolygonRgn(lp(0), 3, 1)
    hBrush = vbBlack
    hBrush = apiCreateSolidBrush(hBrush)
    FillRgn Pic, rgn, hBrush
    DrawPolyline 4, vbBlack
    DeleteObject rgn
    DeleteObject hBrush
    
End Sub

Private Sub DrawBoundaryConditions(ByVal Panel As cPanel)
    On Error GoTo DrawBoundaryConditionsErr
    If Project.Item(ProjectIndex).cDisplaySettings.DrawBoundaryConditions = no Then Exit Sub
    If Panel.Visible = False Then Exit Sub
    Dim hBrush As Long
    Dim y As Long, z As Long
    hBrush = vbRed
    Dim sBoundaryName As String
    Dim i As Integer
    Select Case Project.Item(ProjectIndex).colPanel.Count
        Case 1
'            DiscernSupports Panel.X_IN_Screen, Panel.Y_IN_Screen, Panel.Angle, Panel.colBoundaryConditions.Item(1).BoundaryCondition
'            DiscernSupports Panel.X_OUT_Screen, Panel.Y_OUT_Screen, Panel.Angle, Panel.colBoundaryConditions.Item(2).BoundaryCondition
            
            DiscernSupports colNodes.Item(Panel.cGeometry.InNode).Y_Screen, colNodes.Item(Panel.cGeometry.InNode).Z_Screen, _
                Panel.cGeometry.PanelAngle, Panel.colBoundaryConditions.Item(1).BoundaryCondition
            DiscernSupports colNodes.Item(Panel.cGeometry.OutNode).Y_Screen, colNodes.Item(Panel.cGeometry.OutNode).Z_Screen, _
                Panel.cGeometry.PanelAngle, Panel.colBoundaryConditions.Item(2).BoundaryCondition
        Case Is > 1
            If Panel.colBoundaryConditions.Count = 1 Then
                Select Case Panel.colBoundaryConditions.Item(1).Edge
                    Case InEdge
'                        Y = Panel.X_IN_Screen
'                        Z = Panel.Y_IN_Screen
                        y = colNodes.Item(Panel.cGeometry.InNode).Y_Screen
                        z = colNodes.Item(Panel.cGeometry.InNode).Z_Screen
                    Case OutEdge
'                        Y = Panel.X_OUT_Screen
'                        Z = Panel.Y_OUT_Screen
                        y = colNodes.Item(Panel.cGeometry.OutNode).Y_Screen
                        z = colNodes.Item(Panel.cGeometry.OutNode).Z_Screen
                End Select
                DiscernSupports y, z, Panel.cGeometry.PanelAngle, Panel.colBoundaryConditions.Item(1).BoundaryCondition
            End If
        If Panel.colBoundaryConditions.Count = 2 Then
            For i = 1 To 2
                Select Case Panel.colBoundaryConditions.Item(i).Edge
                    Case InEdge
'                        Y = Panel.X_IN_Screen
'                        Z = Panel.Y_IN_Screen
                        y = colNodes.Item(Panel.cGeometry.InNode).Y_Screen
                        z = colNodes.Item(Panel.cGeometry.InNode).Z_Screen
                
                    Case OutEdge
'                        Y = Panel.X_OUT_Screen
'                        Z = Panel.Y_OUT_Screen
                        y = colNodes.Item(Panel.cGeometry.OutNode).Y_Screen
                        z = colNodes.Item(Panel.cGeometry.OutNode).Z_Screen
                End Select
                DiscernSupports y, z, Panel.cGeometry.PanelAngle, Panel.colBoundaryConditions.Item(i).BoundaryCondition
            Next i
        End If
    End Select
    
    Exit Sub
DrawBoundaryConditionsErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modDraw: Sub DrawBoundaryConditions")
End Sub

Private Sub DiscernSupports(ByVal y As Long, ByVal z As Long, ByVal angle As Double, ByVal index As Integer)
    Select Case index
        Case 1 'Free Edge
            ' Nothing to draw
            DrawSupport y, z, angle + 180
            TextSupport y, z, "Free Edge"
        Case 2 'Simply Supported 2 (w = 0)
            DrawSupport y, z, angle + 180
            TextSupport y, z, "Simply Supported 2"
        Case 3 ' Simply Supported 3 (v = w = 0)
            DrawSupport y, z, angle + 180
            'DrawSupport y, z, Angle - 90
            TextSupport y, z, "Simply Supported 3"
        Case 4 ' Simply Supported 4 (u = w = 0)
            DrawSupport y, z, 45
            'DrawSupport y, z, Angle - 90
            TextSupport y, z, "Simply Supported 4"
        Case 5 ' Simply Supported 5 (u = v = w = 0)
            DrawSupport y, z, 45
            'DrawSupport y, z, Angle + 180
            'DrawSupport y, z, Angle - 90
            TextSupport y, z, "Simply Supported 5"
        Case 6 ' Clamped (u = v = w = dw/dv = 0)
            DrawSupport y, z, 45
            'DrawSupport y, z, Angle + 180
            'DrawSupport y, z, Angle - 90
            TextSupport y, z, "Clamped"
            'DrawArc Y, Z, Angle
            'DrawSupport Y - 5, Z + 10, Angle - 135
        Case 7 ' Symmetry Axis (v = dw/dv = 0)
            DrawSupport y, z, angle + 180
            TextSupport y, z, "Symmetry Axis"
        Case 8 ' Simply Supported 6 (v = 0)
            DrawSupport y, z, angle + 180
            TextSupport y, z, "Simply Supported 6"
        Case 9 ' Simply Supported 7 (u = 0)
            DrawSupport y, z, 45
            TextSupport y, z, "Simply Supported 7"
        Case 10 ' Simply Supported 8 (u = v = 0)
            DrawSupport y, z, 45
            'DrawSupport y, z, Angle + 180
            TextSupport y, z, "Simply Supported 8"
        Case 11 ' Symmetry Axis 2 (w = dw/dv = 0)
            DrawSupport y, z, angle - 90
            TextSupport y, z, "Symmetry Axis 2"
        Case 12 ' Double Symmetry (v = w = dw/dv = 0)
            DrawSupport y, z, angle + 180
            'DrawSupport y, z, Angle - 90
            TextSupport y, z, "Double Symmetry"
    End Select
End Sub

Private Sub TextSupport(ByVal y As Long, ByVal z As Long, s As String)
    If Project.Item(ProjectIndex).cDisplaySettings.TextBoundaryConditions = None Then Exit Sub
    lrect.Top = z - 9
    lrect.bottom = z + 6
    lrect.Left = y - Len(s) * 5 / 2
    lrect.right = y + Len(s) * 6
    DrawTxt s, Project.Item(ProjectIndex).cDisplaySettings.ColorBoundaryConditions, vbBlack
End Sub

Private Sub DrawSupport(ByVal y As Long, ByVal z As Long, ByVal angle As Double)
    size = 2
    Dim y1 As Long, z1 As Long
    Dim y2 As Long, z2 As Long
    Dim y3 As Long, z3 As Long
'    Call RotatePoint(0 * size, 4 * size, Y1, Z1, Angle)
'    Call RotatePoint(-2 * size, -2 * size, Y2, Z2, Angle)
'    Call RotatePoint(2 * size, -2 * size, Y3, Z3, Angle)
    Call RotatePoint(0 * size, 4 * size, y1, z1, 180)
    Call RotatePoint(-2 * size, -2 * size, y2, z2, 180)
    Call RotatePoint(2 * size, -2 * size, y3, z3, 180)

    ReDim lp(3)
    lp(0).y = y: lp(0).z = z
    lp(1).y = y - y1 + y2: lp(1).z = z - z1 + z2
    lp(2).y = y - y1 + y3: lp(2).z = z - z1 + z3
    lp(3).y = y: lp(3).z = z
    Dim rgn As Long
    rgn = CreatePolygonRgn(lp(0), 4, 2)
    Dim hBrush As ColorConstants
    hBrush = Project.Item(ProjectIndex).cDisplaySettings.ColorBoundaryConditions
    hBrush = apiCreateSolidBrush(hBrush)
    FillRgn Pic, rgn, hBrush
    DrawPolyline 4, vbBlack
    DeleteObject rgn
    DeleteObject hBrush
    
    
        
End Sub

'Private Sub DrawArc(ByVal Y As Long, ByVal Z As Long, Angle As Double)
'    size = 2
'    Dim Y1 As Long, Z1 As Long
'    Dim Y2 As Long, Z2 As Long
'    Dim X3 As Long, Y3 As Long
''    Call RotatePoint(0 * size, 4 * size, Y1, Z1, Angle)
''    Call RotatePoint(-2 * size, -2 * size, Y2, Z2, Angle)
''    Call RotatePoint(2 * size, -2 * size, X3, Y3, Angle)
''
''    Arc pic, Y - Y2 - 10, Z - Z2, Y + X3 + 10, Z + Y3 + 20, (Y + Y2) / 2, (Z + Z2) / 2, (Y + X3) / 2, (Z + Y3) / 2
'End Sub
Private Sub DrawPressuresUniformelyDistributed(ByVal Panel As cPanel)
    'On Error GoTo  DrawPressuresUniformelyDistributedErr
    'On Error Resume Next
    If Panel.Visible = False Then Exit Sub
    If Project.Item(ProjectIndex).cDisplaySettings.CurrentLoadCase = 0 Then Exit Sub
    If Project.Item(ProjectIndex).cDisplaySettings.DrawPressuresUniformlyDistributed = no Then Exit Sub
    size = Project.Item(ProjectIndex).cDisplaySettings.SizePressuresUniformlyDistributed
    Dim iLoadCase As Integer, i As Integer
    Dim iStep As Long
    Dim py1 As Long, pz1 As Long, py2 As Long, pz2 As Long
    Dim angle As Double, pWidth As Double
    Dim pin As Double, pout As Double
    Dim pSide As Integer, pSign As Integer
    py1 = colNodes.Item(Panel.cGeometry.InNode).Y_Screen 'Panel.Y_IN_Screen
    pz1 = colNodes.Item(Panel.cGeometry.InNode).Z_Screen 'Panel.Z_IN_Screen
    py2 = colNodes.Item(Panel.cGeometry.OutNode).Y_Screen 'Panel.Y_OUT_Screen
    pz2 = colNodes.Item(Panel.cGeometry.OutNode).Z_Screen 'Panel.Z_OUT_Screen
    iLoadCase = Project.Item(ProjectIndex).cDisplaySettings.CurrentLoadCase
'    pIN = size * CurrentScale * Panel.colLoadCase.Item(iLoadCase).LateralPressureIn
'    pOut = size * CurrentScale * Panel.colLoadCase.Item(iLoadCase).LateralPressureOut
    pin = size * CurrentScale * Project.Item(ProjectIndex).colPanel.Item(Panel.index).colLoadCase.Item(iLoadCase).LateralPressureIn
    pout = size * CurrentScale * Project.Item(ProjectIndex).colPanel.Item(Panel.index).colLoadCase.Item(iLoadCase).LateralPressureOut
    If pin = 0 And pout = 0 Then Exit Sub
    'Select Case Panel.LateralPressureSide
     Select Case Project.Item(ProjectIndex).colPanel.Item(Panel.index).LateralPressureSide
        Case SideLeft
            pSide = -1
        Case SideRight
            pSide = 1
        Case SideNone
            pSide = 0
    End Select
    Select Case pin
        Case Is < 0
            pSign = -1
        Case Is >= 0
            pSign = 1
    End Select
    
    angle = Panel.cGeometry.PanelAngle
    Dim iNoOfLines As Integer
    pWidth = (Sqr((py1 - py2) ^ 2 + (pz1 - pz2) ^ 2))
    iNoOfLines = CInt(3 * pWidth / getScale(ProjectIndex))
    iStep = CLng(pWidth / (iNoOfLines + 1))
    Dim hBrush As Long
    Dim plate_th_Y As Double, plate_th_Z As Double
    plate_th_Y = pSign * pSide * getScale(ProjectIndex) * Panel.cScantlings.NetThickness * Project.Item(ProjectIndex).cDisplaySettings.SizePlates * Cos((angle + 90) * PI / 180)
    plate_th_Z = pSign * pSide * getScale(ProjectIndex) * Panel.cScantlings.NetThickness * Project.Item(ProjectIndex).cDisplaySettings.SizePlates * Sin((angle + 90) * PI / 180)
    hBrush = Project.Item(ProjectIndex).cDisplaySettings.ColorPressuresUniformlyDistributed
    ReDim lp(3)
    Dim y11 As Long, z11 As Long, y22 As Long, z22 As Long
    Dim y1 As Long, z1 As Long, y2 As Long, z2 As Long
    Dim y3 As Long, z3 As Long, y4 As Long, z4 As Long
    Dim y5 As Long, z5 As Long
    Dim vct As Long
    Dim size1 As Integer
    Dim hBrush1 As Long, rgn As Long
    size1 = 0.015 * getScale(ProjectIndex)
    
    'First vector line
    Select Case pin
        Case Is < 0
            lp(0).y = py1 - plate_th_Y
            lp(0).z = pz1 - plate_th_Z
            lp(1).y = py1 + pSide * pSign * pin * Cos((angle + 90) * PI / 180) - plate_th_Y
            lp(1).z = pz1 + pSide * pSign * pin * Sin((angle + 90) * PI / 180) - plate_th_Z
        Case Is >= 0
            lp(0).y = py1 + plate_th_Y
            lp(0).z = pz1 + plate_th_Z
            lp(1).y = py1 + pSide * pSign * pin * Cos((angle + 90) * PI / 180) + plate_th_Y
            lp(1).z = pz1 + pSide * pSign * pin * Sin((angle + 90) * PI / 180) + plate_th_Z
    End Select
    DrawPolyline 2, hBrush
    'first vector flesh
    y11 = lp(0).y
    z11 = lp(0).z
    y22 = lp(1).y
    z22 = lp(1).z
    Select Case pin
        Case Is < 0
            Call RotatePoint(0 * pSign * -pSide * size1, 0 * -pSide * pSign * size1, y3, z3, (angle))
            Call RotatePoint(-2 * pSign * -pSide * size1, -6 * -pSide * pSign * size1, y4, z4, (angle))
            Call RotatePoint(2 * pSign * -pSide * size1, -6 * -pSide * pSign * size1, y5, z5, (angle))

            lp(0).y = y22 + y3: lp(0).z = z22 + z3
            lp(1).y = y22 + y4: lp(1).z = z22 + z4
            lp(2).y = y22 + y5: lp(2).z = z22 + z5
            lp(3).y = y22 + y3: lp(3).z = z22 + z3
        Case Is >= 0
            Call RotatePoint(0 * pSign * -pSide * size1, 0 * -pSide * pSign * size1, y3, z3, (angle))
            Call RotatePoint(-2 * pSign * -pSide * size1, -6 * -pSide * pSign * size1, y4, z4, (angle))
            Call RotatePoint(2 * pSign * -pSide * size1, -6 * -pSide * pSign * size1, y5, z5, (angle))
            
            lp(0).y = y11 + y3: lp(0).z = z11 + z3
            lp(1).y = y11 + y4: lp(1).z = z11 + z4
            lp(2).y = y11 + y5: lp(2).z = z11 + z5
            lp(3).y = y11 + y3: lp(3).z = z11 + z3
    End Select
    If pin <> 0 Then
        hBrush1 = apiCreateSolidBrush(hBrush)
        rgn = CreatePolygonRgn(lp(0), 4, 2)
        FillRgn Pic, rgn, hBrush1
        DeleteObject rgn
        DeleteObject hBrush1
        DeleteObject hBrush
    End If
    
    Select Case pin
        Case Is < 0
            lp(0).y = py2 - plate_th_Y
            lp(0).z = pz2 - plate_th_Z
            lp(1).y = py2 + pSide * pSign * pout * Cos((angle + 90) * PI / 180) - plate_th_Y
            lp(1).z = pz2 + pSide * pSign * pout * Sin((angle + 90) * PI / 180) - plate_th_Z
        Case Is >= 0
            lp(0).y = py2 + plate_th_Y
            lp(0).z = pz2 + plate_th_Z
            lp(1).y = py2 + pSide * pSign * pout * Cos((angle + 90) * PI / 180) + plate_th_Y
            lp(1).z = pz2 + pSide * pSign * pout * Sin((angle + 90) * PI / 180) + plate_th_Z
    End Select

    DrawPolyline 2, hBrush

    y11 = lp(0).y
    z11 = lp(0).z
    y22 = lp(1).y
    z22 = lp(1).z
    Select Case pin
        Case Is < 0
            Call RotatePoint(0 * pSign * -pSide * size1, 0 * -pSide * pSign * size1, y3, z3, (angle))
            Call RotatePoint(-2 * pSign * -pSide * size1, -6 * -pSide * pSign * size1, y4, z4, (angle))
            Call RotatePoint(2 * pSign * -pSide * size1, -6 * -pSide * pSign * size1, y5, z5, (angle))

            lp(0).y = y22 + y3: lp(0).z = z22 + z3
            lp(1).y = y22 + y4: lp(1).z = z22 + z4
            lp(2).y = y22 + y5: lp(2).z = z22 + z5
            lp(3).y = y22 + y3: lp(3).z = z22 + z3
        Case Is >= 0
            Call RotatePoint(0 * pSign * -pSide * size1, 0 * -pSide * pSign * size1, y3, z3, (angle))
            Call RotatePoint(-2 * pSign * -pSide * size1, -6 * -pSide * pSign * size1, y4, z4, (angle))
            Call RotatePoint(2 * pSign * -pSide * size1, -6 * -pSide * pSign * size1, y5, z5, (angle))
            
            lp(0).y = y11 + y3: lp(0).z = z11 + z3
            lp(1).y = y11 + y4: lp(1).z = z11 + z4
            lp(2).y = y11 + y5: lp(2).z = z11 + z5
            lp(3).y = y11 + y3: lp(3).z = z11 + z3
    End Select
    If pout <> 0 Then
        hBrush1 = apiCreateSolidBrush(hBrush)
        rgn = CreatePolygonRgn(lp(0), 4, 2)
        FillRgn Pic, rgn, hBrush1
        DeleteObject rgn
        DeleteObject hBrush1
        DeleteObject hBrush
    End If
    
    Select Case pin
        Case Is < 0
            lp(0).y = py1 + pSign * pSide * pin * Cos((angle + 90) * PI / 180) - plate_th_Y
            lp(0).z = pz1 + pSign * pSide * pin * Sin((angle + 90) * PI / 180) - plate_th_Z
            lp(1).y = py2 + pSign * pSide * pout * Cos((angle + 90) * PI / 180) - plate_th_Y
            lp(1).z = pz2 + pSign * pSide * pout * Sin((angle + 90) * PI / 180) - plate_th_Z
        Case Is >= 0
            lp(0).y = py1 + pSign * pSide * pin * Cos((angle + 90) * PI / 180) + plate_th_Y
            lp(0).z = pz1 + pSign * pSide * pin * Sin((angle + 90) * PI / 180) + plate_th_Z
            lp(1).y = py2 + pSign * pSide * pout * Cos((angle + 90) * PI / 180) + plate_th_Y
            lp(1).z = pz2 + pSign * pSide * pout * Sin((angle + 90) * PI / 180) + plate_th_Z
    End Select
    
'    lp(0).Y = py1 + pSign * pSide * pIN * Cos((angle + 90) * PI / 180) '+ plate_th_Y
'    lp(0).Z = pz1 + pSign * pSide * pIN * Sin((angle + 90) * PI / 180) '+ plate_th_Z
'    lp(1).Y = py2 + pSign * pSide * pOut * Cos((angle + 90) * PI / 180) '+ plate_th_Y
'    lp(1).Z = pz2 + pSign * pSide * pOut * Sin((angle + 90) * PI / 180) '+ plate_th_Z
    DrawPolyline 2, hBrush
    Dim lDist As Long
    'Dim size As Integer
    lDist = 0
    'size = 5
    'Dim lp1() As POINTAPI
    For i = 1 To iNoOfLines
        vct = pin + (iStep * i) * Divide((pout - pin), (pWidth))
        If pin < 0 Then
            y1 = py1 + iStep * i * Cos((angle) * PI / 180) - plate_th_Y
            z1 = pz1 + iStep * i * Sin((angle) * PI / 180) - plate_th_Z
            y2 = y1 + vct * pSign * pSide * Cos((angle + 90) * PI / 180) '- plate_th_Y
            z2 = z1 + vct * pSign * pSide * Sin((angle + 90) * PI / 180) '- plate_th_Z
        
            lp(0).y = y1
            lp(0).z = z1
            lp(1).y = y2 '+ plate_th_Y
            lp(1).z = z2 '+ plate_th_Z

        Else
            y1 = py1 + iStep * i * Cos((angle) * PI / 180) + plate_th_Y
            z1 = pz1 + iStep * i * Sin((angle) * PI / 180) + plate_th_Z
            y2 = y1 + vct * pSign * pSide * Cos((angle + 90) * PI / 180) '+ plate_th_Y
            z2 = z1 + vct * pSign * pSide * Sin((angle + 90) * PI / 180) '+ plate_th_Z
        
            lp(0).y = y1
            lp(0).z = z1
            lp(1).y = y2 '- plate_th_Y
            lp(1).z = z2 '- plate_th_Z

        End If
'        lp(0).Y = y1
'        lp(0).Z = z1
'        lp(1).Y = y2 - plate_th_Y
'        lp(1).Z = z2 - plate_th_Z
        DrawPolyline 2, hBrush
        'ReDim lp(3)
        Select Case pin
            Case Is < 0
                Call RotatePoint(0 * pSign * -pSide * size1, 0 * -pSide * pSign * size1, y3, z3, (angle))
                Call RotatePoint(-2 * pSign * -pSide * size1, -6 * -pSide * pSign * size1, y4, z4, (angle))
                Call RotatePoint(2 * pSign * -pSide * size1, -6 * -pSide * pSign * size1, y5, z5, (angle))

                lp(0).y = y2 + y3: lp(0).z = z2 + z3
                lp(1).y = y2 + y4: lp(1).z = z2 + z4
                lp(2).y = y2 + y5: lp(2).z = z2 + z5
                lp(3).y = y2 + y3: lp(3).z = z2 + z3
            Case Is >= 0
                Call RotatePoint(0 * pSign * -pSide * size1, 0 * -pSide * pSign * size1, y3, z3, (angle))
                Call RotatePoint(-2 * pSign * -pSide * size1, -6 * -pSide * pSign * size1, y4, z4, (angle))
                Call RotatePoint(2 * pSign * -pSide * size1, -6 * -pSide * pSign * size1, y5, z5, (angle))
                
                lp(0).y = y1 + y3: lp(0).z = z1 + z3
                lp(1).y = y1 + y4: lp(1).z = z1 + z4
                lp(2).y = y1 + y5: lp(2).z = z1 + z5
                lp(3).y = y1 + y3: lp(3).z = z1 + z3
        End Select
        hBrush1 = apiCreateSolidBrush(hBrush)
        rgn = CreatePolygonRgn(lp(0), 4, 2)
        FillRgn Pic, rgn, hBrush1
        DeleteObject rgn
        DeleteObject hBrush1
        DeleteObject hBrush
    Next i
    Exit Sub
DrawPressuresUniformelyDistributedErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modDraw: Sub DrawPressuresUniformelyDistributed")
End Sub

Private Sub DrawLocalPressures(ByVal Panel As cPanel)
    If Project.Item(ProjectIndex).cDisplaySettings.DrawLocalizedPressure = no Then Exit Sub
    If Panel.Visible = False Then Exit Sub
    size = Project.Item(ProjectIndex).cDisplaySettings.SizeLocalizedPressure
    Dim y1 As Long, z1 As Long
    Dim y2 As Long, z2 As Long
    Dim y3 As Long, z3 As Long
    Dim py1 As Long, pz1 As Long, py2 As Long, pz2 As Long
    Dim yy1 As Long, zz1 As Long
    Dim Node As cNode
    Dim colNodes As colNodes
    Set colNodes = Project.Item(ProjectIndex).colNodes
    py1 = colNodes.Item(Panel.cGeometry.InNode).Y_Screen
    pz1 = colNodes.Item(Panel.cGeometry.InNode).Z_Screen
    py2 = colNodes.Item(Panel.cGeometry.OutNode).Y_Screen
    pz2 = colNodes.Item(Panel.cGeometry.OutNode).Z_Screen
    
    Dim angle As Double
    Dim size1 As Integer
    size1 = 0.015 * getScale(ProjectIndex)

    angle = 0
    Dim plate_th_Y As Double, plate_th_Z As Double
    plate_th_Y = getScale(ProjectIndex) * Panel.cScantlings.NetThickness * Project.Item(ProjectIndex).cDisplaySettings.SizePlates * Cos((angle + 90) * PI / 180)
    plate_th_Z = getScale(ProjectIndex) * Panel.cScantlings.NetThickness * Project.Item(ProjectIndex).cDisplaySettings.SizePlates * Sin((angle + 90) * PI / 180)
    
    'If Panel.LocalizedPressure <> 0 Then
    If Panel.LocalizedPressure <> 0 Then
        yy1 = (py1 + py2) / 2
        zz1 = (pz1 + pz2) / 2
        
        Call RotatePoint(0 * size1, 0 * size1, y1, z1, 0)
        Call RotatePoint(-2 * size1, -6 * size1, y2, z2, 0)
        Call RotatePoint(2 * size1, -6 * size1, y3, z3, 0)
        
'        Call RotatePoint(0 * -pSide * size1, 0 * -pSide * size1, y3, z3, (angle))
'        Call RotatePoint(-2 * -pSide * size1, -6 * -pSide * size1, y4, z4, (angle))
'        Call RotatePoint(2 * -pSide * size1, -6 * -pSide * size1, y5, z5, (angle))

        ReDim lp(1)
        lp(0).y = yy1 - plate_th_Y
        lp(0).z = zz1 - plate_th_Z
        lp(1).y = yy1 + (y2 + y3) / 2 - plate_th_Y
        lp(1).z = zz1 + (z2 + z3) * size * 2 - plate_th_Z
        'DrawPolyline 2, vbRed
        'lp(1).y = yy1 - 1 + (y2 + y3) / 2
        DrawPolyline 2, Project.Item(ProjectIndex).cDisplaySettings.ColorLocalizedPressure
        'lp(1).y = yy1 + 1 + (y2 + y3) / 2
        'DrawPolyline 2, vbBlack
        
        ReDim lp(3)
        lp(0).y = yy1 - plate_th_Y: lp(0).z = zz1 - plate_th_Z
        lp(1).y = yy1 - y1 + y2 - plate_th_Y: lp(1).z = zz1 - z1 + z2 - plate_th_Z
        lp(2).y = yy1 - y1 + y3 - plate_th_Y: lp(2).z = zz1 - z1 + z3 - plate_th_Z
        lp(3).y = yy1 - plate_th_Y: lp(3).z = zz1 - plate_th_Z
    
        Dim rgn As Long
        rgn = CreatePolygonRgn(lp(0), 4, 2)
        Dim hBrush As Long
        hBrush = Project.Item(ProjectIndex).cDisplaySettings.ColorLocalizedPressure
        hBrush = apiCreateSolidBrush(hBrush)
        FillRgn Pic, rgn, hBrush
        DrawPolyline 4, vbBlack
        DeleteObject rgn
        DeleteObject hBrush
    End If
End Sub

Private Sub TextLocalizedPressures(ByVal Panel As cPanel)
    Dim py1 As Long, pz1 As Long, py2 As Long, pz2 As Long
    Dim yy1 As Long, zz1 As Long, size As Double
    size = Project.Item(ProjectIndex).cDisplaySettings.SizeLocalizedPressure
    If Project.Item(ProjectIndex).cDisplaySettings.TextLocalizedPressure = None Then Exit Sub
    If Panel.Visible = False Then Exit Sub
    Dim colNodes As colNodes
    Dim hBrush As Long
    Set colNodes = Project.Item(ProjectIndex).colNodes
    Dim y1 As Long, z1 As Long
    Dim y2 As Long, z2 As Long
    Dim y3 As Long, z3 As Long
    'If Panel.LocalizedPressure > 0 Then
    If Project.Item(ProjectIndex).colPanel.Item(Panel.index).LocalizedPressure <> 0 Then
        py1 = colNodes.Item(Panel.cGeometry.InNode).Y_Screen
        pz1 = colNodes.Item(Panel.cGeometry.InNode).Z_Screen
        py2 = colNodes.Item(Panel.cGeometry.OutNode).Y_Screen
        pz2 = colNodes.Item(Panel.cGeometry.OutNode).Z_Screen
        yy1 = (py1 + py2) / 2
        zz1 = (pz1 + pz2) / 2

    
        Call RotatePoint(0 * size, 4 * size, y1, z1, 0)
        Call RotatePoint(-2 * size, -2 * size, y2, z2, 0)
        Call RotatePoint(2 * size, -2 * size, y3, z3, 0)
    
        ReDim lp(3)
        lrect.Top = zz1 - 9 - 16
        lrect.bottom = zz1 + 6 - 16
        lrect.Left = yy1 - 12 + 16
        lrect.right = yy1 + 18 + 16
        Dim rgn As Long
        rgn = CreatePolygonRgn(lp(0), 4, 2)
        hBrush = Project.Item(ProjectIndex).cDisplaySettings.ColorLocalizedPressure
        FillRgn Pic, rgn, hBrush
        'DrawTxt CStr(Round(Panel.LocalizedPressure, 2)), vbWhite, hBrush
        DrawTxt CStr(Round(Project.Item(ProjectIndex).colPanel.Item(Panel.index).LocalizedPressure, 2)), vbWhite, hBrush
        
        DrawPolyline 4, vbBlack
        DeleteObject rgn
    End If
End Sub

Private Sub TextPressuresUniformelyDistributed(ByVal Panel As cPanel)
    'On Error GoTo  TextPressuresUniformelyDistributedErr
    'On Error Resume Next
    If Project.Item(ProjectIndex).cDisplaySettings.DrawPressuresUniformlyDistributed = no Then Exit Sub
    If Panel.Visible = False Then Exit Sub
    Select Case Project.Item(ProjectIndex).cDisplaySettings.TextPressuresUniformlyDistributed
    Case None
    Case Pressures
        size = Project.Item(ProjectIndex).cDisplaySettings.SizePressuresUniformlyDistributed
        If Project.Item(ProjectIndex).cDisplaySettings.CurrentLoadCase = 0 Then Exit Sub
        Dim iLoadCase As Integer, i As Integer
        Dim sngStep As Single
        Dim py1 As Long, pz1 As Long, py2 As Long, pz2 As Long
        Dim angle As Double, pWidth As Double
        Dim pin As Long, pout As Long
        Dim pSide As Integer
        py1 = colNodes.Item(Panel.cGeometry.InNode).Y_Screen 'Panel.Z_IN_Screen
        pz1 = colNodes.Item(Panel.cGeometry.InNode).Z_Screen 'Panel.Z_IN_Screen
        py2 = colNodes.Item(Panel.cGeometry.OutNode).Y_Screen 'Panel.Z_OUT_Screen
        pz2 = colNodes.Item(Panel.cGeometry.OutNode).Z_Screen 'Panel.Z_OUT_Screen

        iLoadCase = Project.Item(ProjectIndex).cDisplaySettings.CurrentLoadCase
'        pIN = size * CurrentScale * Panel.colLoadCase.Item(iLoadCase).LateralPressureIn
'        pOut = size * CurrentScale * Panel.colLoadCase.Item(iLoadCase).LateralPressureOut
        pin = size * CurrentScale * Project.Item(ProjectIndex).colPanel.Item(Panel.index).colLoadCase.Item(iLoadCase).LateralPressureIn
        pout = size * CurrentScale * Project.Item(ProjectIndex).colPanel.Item(Panel.index).colLoadCase.Item(iLoadCase).LateralPressureOut
        If pin = 0 And pout = 0 Then Exit Sub
        'Select Case Panel.LateralPressureSide
        Select Case Project.Item(ProjectIndex).colPanel.Item(Panel.index).LateralPressureSide
            Case SideLeft
                pSide = -1
            Case SideRight
                pSide = 1
            Case SideNone
                pSide = 0
        End Select

        Dim pSign As Integer
        Select Case Project.Item(ProjectIndex).colPanel.Item(Panel.index).colLoadCase.Item(iLoadCase).LateralPressureIn
            Case Is < 0
                pSign = -1
            Case Is >= 0
                pSign = 1
        End Select

        angle = Panel.cGeometry.PanelAngle
        ReDim lp(1)
        lp(0).y = py2 + pout * pSide * pSign * Cos((angle + 90) * PI / 180)
        lp(0).z = pz2 + pout * pSide * pSign * Sin((angle + 90) * PI / 180)
        lp(1).y = py1 + pin * pSide * pSign * Cos((angle + 90) * PI / 180)
        lp(1).z = pz1 + pin * pSide * pSign * Sin((angle + 90) * PI / 180)
        Dim hBrush As Long
        hBrush = Project.Item(ProjectIndex).cDisplaySettings.ColorPressuresUniformlyDistributed
        lrect.Top = lp(0).z - 9
        lrect.bottom = lp(0).z + 6
        lrect.Left = lp(0).y - 15
        lrect.right = lp(0).y + 21
        DrawTxt Abs(Round(Project.Item(ProjectIndex).colPanel.Item(Panel.index).colLoadCase.Item(iLoadCase).LateralPressureOut, 3)), vbYellow, hBrush
        'DrawTxt (Round(Project.Item(ProjectIndex).colPanel.Item(Panel.Index).colLoadCase.Item(iLoadCase).LateralPressureOut, 2)), vbYellow, hBrush
        lrect.Top = lp(1).z - 9
        lrect.bottom = lp(1).z + 6
        lrect.Left = lp(1).y - 15
        lrect.right = lp(1).y + 21
        DrawTxt Abs(Round(Project.Item(ProjectIndex).colPanel.Item(Panel.index).colLoadCase.Item(iLoadCase).LateralPressureIn, 3)), vbYellow, hBrush
        'DrawTxt (Round(Project.Item(ProjectIndex).colPanel.Item(Panel.Index).colLoadCase.Item(iLoadCase).LateralPressureIn, 2)), vbYellow, hBrush
    End Select
    Exit Sub
TextPressuresUniformelyDistributedErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modDraw: Sub TextPressuresUniformelyDistributed")
End Sub

Private Sub DrawScreen()
    On Error GoTo DrawScreenErr
    Dim hBrush As Long
    hBrush = Project.Item(ProjectIndex).cDisplaySettings.ColorScreen
    Project.Item(ProjectIndex).frmProject.picScreen.BackColor = hBrush
    'Background
'    With Project.Item(ProjectIndex).frmProject.img
'        '.Visible = True
'        .Left = Project.Item(ProjectIndex).frmProject.picScreen.ScaleLeft
'        .Width = Project.Item(ProjectIndex).frmProject.picScreen.ScaleWidth
'        .Top = Project.Item(ProjectIndex).frmProject.picScreen.ScaleTop
'        .Height = Project.Item(ProjectIndex).frmProject.picScreen.ScaleHeight
'        .Stretch = True
'        .Refresh
'        Project.Item(ProjectIndex).frmProject.picScreen.PaintPicture .Picture, .Left, .Top, .Width, .Height
'        .Visible = False
'    End With
    Exit Sub
DrawScreenErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modDraw: Sub DrawScreen")
End Sub

Private Sub DrawSelections(ByVal Panel As cPanel)
    If Project.Item(ProjectIndex).cDisplaySettings.DrawPlates = no Then Exit Sub
    If Panel.Visible = False Then Exit Sub
    If Panel.Selected = IsUnselected Then Exit Sub
    Dim angle As Double, Thickness As Double
    Dim DY As Long, dZ As Long
    Dim Y_IN As Long, Z_IN As Long, Y_OUT As Long, Z_OUT As Long
    size = Project.Item(ProjectIndex).cDisplaySettings.SizePlates
    
    Y_IN = colNodes.Item(Panel.cGeometry.InNode).Y_Screen 'Panel.Z_IN_Screen
    Z_IN = colNodes.Item(Panel.cGeometry.InNode).Z_Screen 'Panel.Z_IN_Screen
    Y_OUT = colNodes.Item(Panel.cGeometry.OutNode).Y_Screen 'Panel.Z_OUT_Screen
    Z_OUT = colNodes.Item(Panel.cGeometry.OutNode).Z_Screen 'Panel.Z_OUT_Screen

    angle = Panel.cGeometry.PanelAngle
    Select Case Panel.pType
        Case Plate
            Thickness = Panel.cScantlings.NetThickness * CurrentScale
        Case Beam
            Thickness = Abs(Panel.cScantlings.cPrimaryFrames.WebHeight / 2 * CurrentScale - 1)
    End Select
    'Thickness = Panel.cScantlings.NetThickness * CurrentScale
    DY = size * (Thickness + 1) * Cos(PI * (angle - 90) / 180)
    dZ = size * (Thickness + 1) * Sin(PI * (angle - 90) / 180)


    ReDim lp(4)
    lp(0).y = Y_IN - DY: lp(0).z = Z_IN - dZ
    lp(1).y = Y_OUT - DY: lp(1).z = Z_OUT - dZ
    lp(2).y = Y_OUT + DY: lp(2).z = Z_OUT + dZ
    lp(3).y = Y_IN + DY: lp(3).z = Z_IN + dZ
    lp(4).y = Y_IN - DY: lp(4).z = Z_IN - dZ
    Dim rgn As Long, hBrush As Long
    rgn = CreatePolygonRgn(lp(0), 5, 2)
    hBrush = apiCreateSolidBrush(vbRed)
    FillRgn Pic, rgn, hBrush
    DrawPolyline 5, vbBlack
    Project.Item(ProjectIndex).frmProject.picScreen.ForeColor = vbBlack
    
    'Nodes
'    lrect.Left = Y_IN - size * (Thickness + 3)
'    lrect.bottom = Z_IN - size * (Thickness + 3)
'    lrect.right = Y_IN + size * (Thickness + 3)
'    lrect.Top = Z_IN + size * (Thickness + 3)
'    FillRectangle vbRed
'    DrawRectangle Y_IN - size * (Thickness + 3), Z_IN - size * (Thickness + 3), Y_IN + size * (Thickness + 3), Z_IN + size * (Thickness + 3)
'    lrect.Left = Y_OUT - size * (Thickness + 3)
'    lrect.bottom = Z_OUT - size * (Thickness + 3)
'    lrect.right = Y_OUT + size * (Thickness + 3)
'    lrect.Top = Z_OUT + size * (Thickness + 3)
'    FillRectangle vbRed
'    DrawRectangle Y_OUT - size * (Thickness + 3), Z_OUT - size * (Thickness + 3), Y_OUT + size * (Thickness + 3), Z_OUT + size * (Thickness + 3)
    Dim size_nodes As Single
    size_nodes = Project.Item(ProjectIndex).cDisplaySettings.SizeNodes + 1
    lrect.Left = Y_IN - size_nodes
    lrect.bottom = Z_IN - size_nodes
    lrect.right = Y_IN + size_nodes
    lrect.Top = Z_IN + size_nodes
    FillRectangle vbRed
    DrawRectangle Y_IN - size_nodes, Z_IN - size_nodes, Y_IN + size_nodes, Z_IN + size_nodes
    lrect.Left = Y_OUT - size_nodes
    lrect.bottom = Z_OUT - size_nodes
    lrect.right = Y_OUT + size_nodes
    lrect.Top = Z_OUT + size_nodes
    FillRectangle vbRed
    DrawRectangle Y_OUT - size_nodes, Z_OUT - size_nodes, Y_OUT + size_nodes, Z_OUT + size_nodes

    DeleteObject hBrush
    DeleteObject rgn
  
End Sub

Private Sub DrawParticipation(ByVal Panel As cPanel)
    If Panel.pType = Beam Then Exit Sub
    If Project.Item(ProjectIndex).cDisplaySettings.DrawParticipation = no Then Exit Sub
    Dim rgn As Long
    Dim DY As Long, dZ As Long
    Dim hBrush As Long
    Dim angle As Double, Thickness As Double
    Dim Y_IN As Long, Z_IN As Long, Y_OUT As Long, Z_OUT As Long
    size = 1 'Project.Item(ProjectIndex).cDisplaySettings.SizePlates
    Y_IN = colNodes.Item(Panel.cGeometry.InNode).Y_Screen 'Panel.Z_IN_Screen
    Z_IN = colNodes.Item(Panel.cGeometry.InNode).Z_Screen 'Panel.Z_IN_Screen
    Y_OUT = colNodes.Item(Panel.cGeometry.OutNode).Y_Screen 'Panel.Z_OUT_Screen
    Z_OUT = colNodes.Item(Panel.cGeometry.OutNode).Z_Screen 'Panel.Z_OUT_Screen
    angle = Panel.cGeometry.PanelAngle
    Thickness = 0.2 * CurrentScale
    'Thickness = Panel.cScantlings.NetThickness * CurrentScale
    DY = size * Thickness * Cos(PI * (angle - 90) / 180)
    dZ = size * Thickness * Sin(PI * (angle - 90) / 180)
    ReDim lp(4)
    Dim selSizeY As Long, selSizeZ As Long
    selSizeY = 5 * Cos(PI * (angle - 90) / 180)
    selSizeZ = 5 * Sin(PI * (angle - 90) / 180)
    If Abs(selSizeY) < Abs(DY) Then selSizeY = DY
    If Abs(selSizeZ) < Abs(dZ) Then selSizeZ = dZ
    lp(0).y = Y_IN - DY: lp(0).z = Z_IN - dZ
    lp(1).y = Y_OUT - DY: lp(1).z = Z_OUT - dZ
    lp(2).y = Y_OUT + DY: lp(2).z = Z_OUT + dZ
    lp(3).y = Y_IN + DY: lp(3).z = Z_IN + dZ
    lp(4).y = Y_IN - DY: lp(4).z = Z_IN - dZ
    rgn = CreatePolygonRgn(lp(0), 5, 2)
'    Dim a As ColorConstants
'    a = RGB(Panel.cGeometry.Participation * 255, Panel.cGeometry.Participation * 50, Panel.cGeometry.Participation * 50)
'    hBrush = apiCreateSolidBrush(a)
    hBrush = apiCreateSolidBrush(((Panel.cGeometry.Participation) ^ 1) * 150000)
    
    
    FillRgn Pic, rgn, hBrush
    DrawPolyline 5, vbBlack
    DeleteObject hBrush
    DeleteObject rgn
End Sub

Private Sub DrawPlates(ByVal Panel As cPanel)
    On Error GoTo DrawPlatesErr:
    Dim rgn As Long

    'If Panel.pType <> Plate Or Panel.pType <> DoubleHull Then Exit Sub
    Select Case Panel.pType
        Case Plate, DoubleHull
        Case Else
            Exit Sub
    End Select
    If Project.Item(ProjectIndex).cDisplaySettings.DrawPlates = no Then Exit Sub
    If Panel.Visible = False Then Exit Sub
    Dim DY As Long, dZ As Long
    Dim hBrush As Long
    Dim angle As Double, Thickness As Double
    Dim Y_IN As Long, Z_IN As Long, Y_OUT As Long, Z_OUT As Long
    size = Project.Item(ProjectIndex).cDisplaySettings.SizePlates
    Y_IN = colNodes.Item(Panel.cGeometry.InNode).Y_Screen 'Panel.Z_IN_Screen
    Z_IN = colNodes.Item(Panel.cGeometry.InNode).Z_Screen 'Panel.Z_IN_Screen
    Y_OUT = colNodes.Item(Panel.cGeometry.OutNode).Y_Screen 'Panel.Z_OUT_Screen
    Z_OUT = colNodes.Item(Panel.cGeometry.OutNode).Z_Screen 'Panel.Z_OUT_Screen
    
    angle = Panel.cGeometry.PanelAngle
    Thickness = Panel.cScantlings.NetThickness * CurrentScale
    DY = size * Thickness * Cos(PI * (angle - 90) / 180)
    dZ = size * Thickness * Sin(PI * (angle - 90) / 180)
    ReDim lp(4)
    Dim selSizeY As Long, selSizeZ As Long
    selSizeY = 5 * Cos(PI * (angle - 90) / 180)
    selSizeZ = 5 * Sin(PI * (angle - 90) / 180)
    If Abs(selSizeY) < Abs(DY) Then selSizeY = DY
    If Abs(selSizeZ) < Abs(dZ) Then selSizeZ = dZ
    lp(0).y = Y_IN - selSizeY: lp(0).z = Z_IN - selSizeZ
    lp(1).y = Y_OUT - selSizeY: lp(1).z = Z_OUT - selSizeZ
    lp(2).y = Y_OUT + selSizeY: lp(2).z = Z_OUT + selSizeZ
    lp(3).y = Y_IN + selSizeY: lp(3).z = Z_IN + selSizeZ
    lp(4).y = Y_IN - selSizeY: lp(4).z = Z_IN - selSizeZ
    FormRegions Panel, Project.Item(ProjectIndex).cDisplaySettings.ColorPlates
    
    If Panel.HighLighted = yes Then
        DY = selSizeY
        dZ = selSizeZ
    End If

    lp(0).y = Y_IN - DY: lp(0).z = Z_IN - dZ
    lp(1).y = Y_OUT - DY: lp(1).z = Z_OUT - dZ
    lp(2).y = Y_OUT + DY: lp(2).z = Z_OUT + dZ
    lp(3).y = Y_IN + DY: lp(3).z = Z_IN + dZ
    lp(4).y = Y_IN - DY: lp(4).z = Z_IN - dZ
    rgn = CreatePolygonRgn(lp(0), 5, 2)
'    If Panel.Selected = isSelected Then
'        hBrush = apiCreateSolidBrush(vbRed)
'        FillRgn Pic, rgn, hBrush
'        DrawPolyline 5, vbRed 'vbBlack
'    Else
        hBrush = apiCreateSolidBrush(Project.Item(ProjectIndex).cDisplaySettings.ColorPlates)
        FillRgn Pic, rgn, hBrush
        DrawPolyline 5, vbBlack
'    End If
    DeleteObject hBrush
    DeleteObject rgn
    Exit Sub
DrawPlatesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modDraw: Sub DrawPlates")
End Sub

Private Sub DrawBeams(ByVal Panel As cPanel)
    On Error GoTo DrawBeamsErr:
    If Project.Item(ProjectIndex).cDisplaySettings.DrawBeams = no Then Exit Sub
    If Panel.Visible = False Then Exit Sub
    If Panel.pType <> Beam Then Exit Sub
    Dim DY As Long, dZ As Long
    Dim Y_IN As Long, Z_IN As Long, Y_OUT As Long, Z_OUT As Long
    Dim angle As Double, Thickness As Double, WebHeight As Double, FlangeThickness As Double
    
    Dim hBrush As Long
    size = Project.Item(ProjectIndex).cDisplaySettings.SizeBeams
    Y_IN = colNodes.Item(Panel.cGeometry.InNode).Y_Screen 'Panel.Z_IN_Screen
    Z_IN = colNodes.Item(Panel.cGeometry.InNode).Z_Screen 'Panel.Z_IN_Screen
    Y_OUT = colNodes.Item(Panel.cGeometry.OutNode).Y_Screen 'Panel.Z_OUT_Screen
    Z_OUT = colNodes.Item(Panel.cGeometry.OutNode).Z_Screen 'Panel.Z_OUT_Screen

    angle = Panel.cGeometry.PanelAngle
    'Thickness = Panel.cScantlings.NetThickness * CurrentScale
    WebHeight = Panel.cScantlings.cPrimaryFrames.WebHeight * CurrentScale
    'FlangeThickness = Panel.cScantlings.cPrimaryFrames.FlangeThickness * CurrentScale
    FlangeThickness = Panel.cScantlings.NetThickness * CurrentScale
    
    Select Case Panel.cScantlings.BeamSection
        Case bsCircle, bsSquare
            FlangeThickness = Panel.cScantlings.NetThickness * CurrentScale
        Case bsDoubleT
            FlangeThickness = Panel.cScantlings.cPrimaryFrames.FlangeThickness * CurrentScale
    End Select
    WebHeight = Panel.cScantlings.cPrimaryFrames.WebHeight * CurrentScale
    
'    dy = size * Thickness * Cos(PI * (Angle - 90) / 180)
'    dz = size * Thickness * Sin(PI * (Angle - 90) / 180)
    DY = size * WebHeight * Cos(PI * (angle - 90) / 180)
    dZ = size * WebHeight * Sin(PI * (angle - 90) / 180)
    Dim DY1 As Long, dz1 As Long
    DY1 = size * FlangeThickness * Cos(PI * (angle - 90) / 180)
    dz1 = size * FlangeThickness * Sin(PI * (angle - 90) / 180)
    ReDim lp(4)
    Dim selSizeY As Integer, selSizeZ As Integer
    selSizeY = 5 * size * Cos(PI * (angle - 90) / 180)
    selSizeZ = 5 * size * Sin(PI * (angle - 90) / 180)
    If Abs(selSizeY) < Abs(DY) Then selSizeY = DY
    If Abs(selSizeZ) < Abs(dZ) Then selSizeZ = dZ
    
    lp(0).y = Y_IN - selSizeY: lp(0).z = Z_IN - selSizeZ
    lp(1).y = Y_OUT - selSizeY: lp(1).z = Z_OUT - selSizeZ
    lp(2).y = Y_OUT + selSizeY: lp(2).z = Z_OUT + selSizeZ
    lp(3).y = Y_IN + selSizeY: lp(3).z = Z_IN + selSizeZ
    lp(4).y = Y_IN - selSizeY: lp(4).z = Z_IN - selSizeZ
    FormRegions Panel, Project.Item(ProjectIndex).cDisplaySettings.ColorBeams
    
    If Panel.HighLighted = yes Then
        DY = selSizeY * 2
        dZ = selSizeZ * 2
    End If

    lp(0).y = Y_IN - DY / 2: lp(0).z = Z_IN - dZ / 2
    lp(1).y = Y_OUT - DY / 2: lp(1).z = Z_OUT - dZ / 2
    lp(2).y = Y_OUT + DY / 2: lp(2).z = Z_OUT + dZ / 2
    lp(3).y = Y_IN + DY / 2: lp(3).z = Z_IN + dZ / 2
    lp(4).y = Y_IN - DY / 2: lp(4).z = Z_IN - dZ / 2
    Dim rgn As Long
    rgn = CreatePolygonRgn(lp(0), 5, 2)
    hBrush = apiCreateSolidBrush(Project.Item(ProjectIndex).cDisplaySettings.ColorBeams)
    FillRgn Pic, rgn, hBrush
    DeleteObject hBrush
    hBrush = CreateHatchBrush(HS_FDIAGONAL, vbBlack) ' Project.Item(ProjectIndex).cDisplaySettings.ColorBeams)
    FillRgn Pic, rgn, hBrush
    DrawPolyline 5, vbBlack
    'Flanges
    lp(0).y = Y_IN - DY / 2: lp(0).z = Z_IN - dZ / 2
    lp(1).y = Y_OUT - DY / 2: lp(1).z = Z_OUT - dZ / 2
    lp(2).y = Y_OUT - DY / 2 - DY1: lp(2).z = Z_OUT - dZ / 2 - dz1
    lp(3).y = Y_IN - DY / 2 - DY1: lp(3).z = Z_IN - dZ / 2 - dz1
    lp(4).y = Y_IN - DY / 2: lp(4).z = Z_IN - dZ / 2
    DrawPolyline 5, vbBlack
    lp(0).y = Y_IN + DY / 2: lp(0).z = Z_IN + dZ / 2
    lp(1).y = Y_OUT + DY / 2: lp(1).z = Z_OUT + dZ / 2
    lp(2).y = Y_OUT + DY / 2 + DY1: lp(2).z = Z_OUT + dZ / 2 + dz1
    lp(3).y = Y_IN + DY / 2 + DY1: lp(3).z = Z_IN + dZ / 2 + dz1
    lp(4).y = Y_IN + DY / 2: lp(4).z = Z_IN + dZ / 2
    DrawPolyline 5, vbBlack
    DeleteObject hBrush
    DeleteObject rgn
    Exit Sub
DrawBeamsErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modDraw: Sub DrawBeams")
End Sub

Private Sub DrawNodes(ByVal Node As cNode)
    On Error GoTo DrawNodesErr
    Dim y As Long, z As Long
    Dim hBrush As ColorConstants
    If Project.Item(ProjectIndex).cDisplaySettings.DrawNodes = no Then Exit Sub
    size = Project.Item(ProjectIndex).cDisplaySettings.SizeNodes
    hBrush = Project.Item(ProjectIndex).cDisplaySettings.ColorNodes
    y = Node.Y_Screen
    z = Node.Z_Screen
    If Node.HighLighted = yes Then size = size * 2
    lrect.Left = y - size - 1
    lrect.bottom = z - size - 1
    lrect.right = y + size + 1
    lrect.Top = z + size + 1
    ReDim lp(4)
    lp(0).y = y - size - 3
    lp(0).z = z - size - 3
    lp(1).y = y + size + 3
    lp(1).z = z - size - 3
    lp(2).y = y + size + 3
    lp(2).z = z + size + 3
    lp(3).y = y - size - 3
    lp(3).z = z + size + 3
    DeleteObject Node.Region
    Node.Region = Empty
    Node.Region = CreatePolygonRgn&(lp(0), 4, 2)
    If Node.Selected = isSelected Then hBrush = vbRed
    FillRectangle hBrush
    Project.Item(ProjectIndex).frmProject.picScreen.ForeColor = vbBlack
    DrawRectangle y - size - 1, z - size - 1, y + size + 1, z + size + 1

    
    
'    Dim nHeight As Long, nWidth As Long, hScrDC As Long
'    nHeight = Project.Item(ProjectIndex).frmProject.pic.height
'    nWidth = Project.Item(ProjectIndex).frmProject.pic.WIDTH
'    hScrDC = Project.Item(ProjectIndex).frmProject.pic.hdc
'    'hScrDC = Project.Item(ProjectIndex).frmProject.pic.picture.handle
'    BitBlt pic, X - nWidth / 2, Y, nWidth, nHeight, hScrDC, 0, 0, SRCAND
    
    Exit Sub
DrawNodesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modDraw: Sub DrawNodes")
End Sub


Private Sub TextNodes(ByVal Node As cNode)
    'On Error GoTo  TextNodesErr
    If Project.Item(ProjectIndex).cDisplaySettings.TextNodes = None Then Exit Sub
    Dim y As Long, z As Long
    Dim sValue As String, lenTxt As Integer
    Dim hBrush As ColorConstants
    hBrush = Project.Item(ProjectIndex).cDisplaySettings.ColorNodes
    y = Node.Y_Screen
    z = Node.Z_Screen
    Select Case Project.Item(ProjectIndex).cDisplaySettings.TextNodes
        Case Numbers
            sValue = " " & Node.nNumber & " "
        Case Coordinates
            sValue = CStr(Round(Node.y, 3)) & "; " & -CStr(Round(Node.z, 3))
        Case All
    End Select
    
    lenTxt = Len(sValue)
    lrect.Top = z + 5
    lrect.bottom = z + 5 + 15
    lrect.Left = y - lenTxt * 3
    lrect.right = y + lenTxt * 3
    DrawTxt (sValue), hBrush, vbBlack
    
    Exit Sub
TextNodesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modDraw: Sub TextNodes")
End Sub

Private Sub TextPlates(Panel As cPanel)
    On Error GoTo TextPlatesErr
    Dim DY As Long, dZ As Long
    Dim Y_IN As Long, Z_IN As Long, Y_OUT As Long, Z_OUT As Long
    Dim angle As Double
    If Panel.Visible = False Then Exit Sub
    If Panel.pType <> Plate And Panel.pType <> DoubleHull Then Exit Sub
    Y_IN = colNodes.Item(Panel.cGeometry.InNode).Y_Screen 'Panel.Z_IN_Screen
    Z_IN = colNodes.Item(Panel.cGeometry.InNode).Z_Screen 'Panel.Z_IN_Screen
    Y_OUT = colNodes.Item(Panel.cGeometry.OutNode).Y_Screen 'Panel.Z_OUT_Screen
    Z_OUT = colNodes.Item(Panel.cGeometry.OutNode).Z_Screen 'Panel.Z_OUT_Screen

    angle = Panel.cGeometry.PanelAngle
    size = 2
    If Project.Item(ProjectIndex).cDisplaySettings.DrawFleshes = yes Then
        DY = 6 * size * Cos(PI * (angle - 90) / 180) + (Y_IN + Y_OUT) / 2
        dZ = 6 * size * Sin(PI * (angle - 90) / 180) + (Z_IN + Z_OUT) / 2
    Else
        DY = (Y_IN + Y_OUT) / 2
        dZ = (Z_IN + Z_OUT) / 2
    End If
    Dim lenTxt As Integer, dValue As String
    Select Case Project.Item(ProjectIndex).cDisplaySettings.TextPlates
        Case None
            Exit Sub
        Case Numbers
            dValue = " " & CStr(Panel.pNumber) & " "
        Case Lenght
            dValue = " " & CStr(Panel.cGeometry.PanelWidth) & " "
        Case NetThickness
            If Project.Item(ProjectIndex).cSolution.ShowVariation = True Then
                dValue = " " & CStr(Round(Panel.cScantlings.ThicknessVariation, 2)) & "%"
            Else
                dValue = " " & CStr(Panel.cScantlings.NetThickness * 1000) & " "
            End If
        Case GrossThickness
            If Project.Item(ProjectIndex).cSolution.ShowVariation = True Then
                dValue = " " & CStr(Round(Panel.cScantlings.ThicknessVariation, 2)) & "%"
            Else
                dValue = " " & CStr((Panel.cScantlings.NetThickness + Panel.cScantlings.CorrosionThickness) * 1000) & " "
            End If
        Case Corrosion
            dValue = " " & CStr(Panel.cScantlings.CorrosionThickness * 1000) & " "
        Case Participation
            dValue = CStr(Format(Round(Panel.cGeometry.Participation, 3), "0.00")) 'CStr(Round(Node.y, 3))
        Case All
    End Select
    lenTxt = Len(dValue)
    lrect.Top = dZ - 8
    lrect.bottom = dZ + 6
    lrect.Left = DY - lenTxt * 3
    lrect.right = DY + lenTxt * 3
    DrawTxt (dValue), vbWhite, vbBlack
    
    Exit Sub
TextPlatesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modDraw: Sub TextPlates")
End Sub

Private Sub TextBeams(Panel As cPanel)
    On Error GoTo TextBeamsErr
    Dim DY As Long, dZ As Long
    Dim Y_IN As Long, Z_IN As Long, Y_OUT As Long, Z_OUT As Long
    Dim angle As Double
    If Panel.Visible = False Then Exit Sub
    If Panel.pType <> Beam Then Exit Sub
    Y_IN = colNodes.Item(Panel.cGeometry.InNode).Y_Screen 'Panel.Z_IN_Screen
    Z_IN = colNodes.Item(Panel.cGeometry.InNode).Z_Screen 'Panel.Z_IN_Screen
    Y_OUT = colNodes.Item(Panel.cGeometry.OutNode).Y_Screen 'Panel.Z_OUT_Screen
    Z_OUT = colNodes.Item(Panel.cGeometry.OutNode).Z_Screen 'Panel.Z_OUT_Screen

    angle = Panel.cGeometry.PanelAngle
    size = 2
    If Project.Item(ProjectIndex).cDisplaySettings.DrawFleshes = yes Then
        DY = 6 * size * Cos(PI * (angle - 90) / 180) + (Y_IN + Y_OUT) / 2
        dZ = 6 * size * Sin(PI * (angle - 90) / 180) + (Z_IN + Z_OUT) / 2
    Else
        DY = (Y_IN + Y_OUT) / 2
        dZ = (Z_IN + Z_OUT) / 2
    End If
    Dim lenTxt As Integer, dValue As String
    Select Case Project.Item(ProjectIndex).cDisplaySettings.TextBeams
        Case None
            Exit Sub
        Case Numbers
            dValue = " " & CStr(Panel.pNumber) & " "
        Case Lenght
            dValue = " " & CStr(Panel.cGeometry.PanelWidth) & " "
        Case NetThickness
            dValue = " " & CStr(Panel.cScantlings.NetThickness * 1000) & " "
        Case GrossThickness
            dValue = " " & CStr((Panel.cScantlings.NetThickness + Panel.cScantlings.CorrosionThickness) * 1000) & " "
        Case Corrosion
            dValue = " " & CStr(Panel.cScantlings.CorrosionThickness * 1000) & " "
        Case All
    End Select
    lenTxt = Len(dValue)
    lrect.Top = dZ - 8
    lrect.bottom = dZ + 6
    lrect.Left = DY - lenTxt * 3
    lrect.right = DY + lenTxt * 3
    DrawTxt (dValue), vbWhite, vbBlack
    
    Exit Sub
TextBeamsErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modDraw: Sub TextBeams")
End Sub
Public Sub DrawFleshes(ByVal Panel As cPanel)
    On Error GoTo DrawFleshesErr
    If Project.Item(ProjectIndex).cDisplaySettings.DrawFleshes = no Then Exit Sub
    If Panel.Visible = False Then Exit Sub
    size = Project.Item(ProjectIndex).cDisplaySettings.SizeFleshes
    Dim y As Long, z As Long
    Dim y1 As Long, z1 As Long
    Dim y2 As Long, z2 As Long
    Dim y3 As Long, z3 As Long
    Dim angle As Double
    angle = Panel.cGeometry.PanelAngle
'    Y = (Panel.X_IN_Screen + Panel.X_OUT_Screen) / 2
'    Z = (Panel.Y_IN_Screen + Panel.Y_OUT_Screen) / 2
    y = (colNodes.Item(Panel.cGeometry.InNode).Y_Screen + colNodes.Item(Panel.cGeometry.OutNode).Y_Screen) / 2
    z = (colNodes.Item(Panel.cGeometry.InNode).Z_Screen + colNodes.Item(Panel.cGeometry.OutNode).Z_Screen) / 2
    Call RotatePoint(0 * size, 4 * size, y1, z1, (angle - 90))
    Call RotatePoint(-2 * size, -2 * size, y2, z2, (angle - 90))
    Call RotatePoint(2 * size, -2 * size, y3, z3, (angle - 90))
    ReDim lp(3)
    lp(0).y = y + y1: lp(0).z = z + z1
    lp(1).y = y + y2: lp(1).z = z + z2
    lp(2).y = y + y3: lp(2).z = z + z3
    lp(3).y = y + y1: lp(3).z = z + z1
    Dim rgn As Long
    rgn = CreatePolygonRgn(lp(0), 4, 2)
    Dim hBrush As Long
    hBrush = Project.Item(ProjectIndex).cDisplaySettings.ColorFleshes
    hBrush = apiCreateSolidBrush(hBrush)
    FillRgn Pic, rgn, hBrush
    DrawPolyline 4, vbBlack
    DeleteObject rgn
    DeleteObject hBrush
    'Dim d As ColorConstants
    Exit Sub
DrawFleshesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modDraw: Sub DrawFleshes")
End Sub

Private Sub DrawPrimaryStiffeners(ByVal Panel As cPanel)
'    On Error GoTo  DrawPrimaryStiffenersErr
'    On Error Resume Next 'Bypass div by 0
    If Project.Item(ProjectIndex).cDisplaySettings.DrawPrimaryStiffeners = no Then Exit Sub
    If Panel.Visible = False Then Exit Sub
    size = Project.Item(ProjectIndex).cDisplaySettings.SizePrimaryStiffeners
    Dim NoOfStiffeners As Long, Interval As Double, i As Integer
    Dim py1 As Long, pz1 As Long, py2 As Long, pz2 As Long
    Dim y As Long, z As Long, y1 As Long, z1 As Long
    Dim y2 As Long, z2 As Long, y3 As Long, z3 As Long
    Dim angle As Double, WebHeight As Double, WebThickness As Double
    Dim FlangeWidth As Double, FlangeThickness As Double
    Dim Spacing As Double, Side As Side, pWidth As Double
    py1 = colNodes.Item(Panel.cGeometry.InNode).Y_Screen 'Panel.Z_IN_Screen
    pz1 = colNodes.Item(Panel.cGeometry.InNode).Z_Screen 'Panel.Z_IN_Screen
    py2 = colNodes.Item(Panel.cGeometry.OutNode).Y_Screen 'Panel.Z_OUT_Screen
    pz2 = colNodes.Item(Panel.cGeometry.OutNode).Z_Screen 'Panel.Z_OUT_Screen

    angle = Panel.cGeometry.PanelAngle
    pWidth = Sqr((py1 - py2) ^ 2 + (pz1 - pz2) ^ 2)
    WebHeight = Panel.cScantlings.cPrimaryStiffeners.WebHeight
    WebThickness = Panel.cScantlings.cPrimaryStiffeners.WebThickness
    FlangeWidth = Panel.cScantlings.cPrimaryStiffeners.FlangeWidth
    FlangeThickness = Panel.cScantlings.cPrimaryStiffeners.FlangeThickness
    Side = Panel.cScantlings.cPrimaryStiffeners.Side
    Select Case Panel.cScantlings.cPrimaryStiffeners.DistributionMode
        Case "EE1"
            NoOfStiffeners = CLng(Divide(Panel.cGeometry.PanelWidth, Panel.cScantlings.cPrimaryStiffeners.Spacing)) - 1
            Panel.cScantlings.cPrimaryStiffeners.NumberOfStiffeners = Divide(Panel.cGeometry.PanelWidth, Panel.cScantlings.cPrimaryStiffeners.Spacing) - 1
            Spacing = Divide(pWidth, (NoOfStiffeners + 1))
            Interval = Spacing
        Case "EE2"
            NoOfStiffeners = CLng(Divide(Panel.cGeometry.PanelWidth, Panel.cScantlings.cPrimaryStiffeners.Spacing))
            Panel.cScantlings.cPrimaryStiffeners.NumberOfStiffeners = Divide(Panel.cGeometry.PanelWidth, Panel.cScantlings.cPrimaryStiffeners.Spacing)
            Spacing = Divide(pWidth, NoOfStiffeners)
            Interval = Spacing / 2
    End Select
    For i = 1 To NoOfStiffeners
        y = py1 + Divide(Interval * (py2 - py1), Sqr(((py2 - py1) ^ 2) + ((pz2 - pz1) ^ 2)))
        z = pz1 + Divide(Interval * (pz2 - pz1), Sqr(((py2 - py1) ^ 2) + ((pz2 - pz1) ^ 2)))
'        X = pZ1 + ((Interval * (pZ2 - pZ1)) / Sqr(((pZ2 - pZ1) ^ 2) + ((pZ2 - pZ1) ^ 2)))
'        Y = pZ1 + ((Interval * (pZ2 - pZ1)) / Sqr(((pZ2 - pZ1) ^ 2) + ((pZ2 - pZ1) ^ 2)))
        Select Case Panel.cScantlings.cPrimaryStiffeners.Side
           Case SideLeft
               Call RotatePoint(size * WebHeight * CurrentScale, 0, y1, z1, -90 + angle)
               Call RotatePoint(size * WebHeight * CurrentScale, (size * FlangeWidth * CurrentScale / 2), y2, z2, -90 + angle)
               Call RotatePoint(size * WebHeight * CurrentScale, (size * -FlangeWidth * CurrentScale / 2), y3, z3, -90 + angle)
           Case SideRight
               Call RotatePoint(size * WebHeight * CurrentScale, 0, y1, z1, 90 + angle)
               Call RotatePoint(size * WebHeight * CurrentScale, (size * FlangeWidth * CurrentScale / 2), y2, z2, 90 + angle)
               Call RotatePoint(size * WebHeight * CurrentScale, (size * -FlangeWidth * CurrentScale / 2), y3, z3, 90 + angle)
            Case SideNone
                Exit Sub
        End Select
        y1 = y + y1: y2 = y + y2: y3 = y + y3
        z1 = z + z1: z2 = z + z2: z3 = z + z3
        Interval = Interval + Spacing
        ReDim lp(1)
        Dim hBrush As Long
        hBrush = Project.Item(ProjectIndex).cDisplaySettings.ColorPrimaryStiffeners
        lp(0).y = y: lp(0).z = z
        lp(1).y = y1: lp(1).z = z1
        DrawPolyline 2, hBrush
        lp(0).y = y2: lp(0).z = z2
        lp(1).y = y3: lp(1).z = z3
        DrawPolyline 2, hBrush
    Next i
    Exit Sub
DrawPrimaryStiffenersErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modDraw: Sub DrawPrimaryStiffeners")
End Sub

Private Sub DrawSecondaryStiffeners(ByVal Panel As cPanel)
    'On Error GoTo  DrawSecondaryStiffenersErr
    On Error Resume Next 'Bypass div by 0
    If Project.Item(ProjectIndex).cDisplaySettings.DrawSecondaryStiffeners = no Then Exit Sub
    If Panel.Visible = False Then Exit Sub
    size = Project.Item(ProjectIndex).cDisplaySettings.SizeSecondaryStiffeners
    Dim NoOfStiffeners As Long, Interval As Long, i As Integer
    Dim py1 As Long, pz1 As Long, py2 As Long, pz2 As Long
    Dim y As Long, z As Long, y1 As Long, z1 As Long
    Dim y2 As Long, z2 As Long, y3 As Long, z3 As Long
    Dim angle As Double, WebHeight As Double, WebThickness As Double
    Dim FlangeWidth As Double, FlangeThickness As Double
    Dim Spacing As Long, Side As Side, pWidth As Long
    If Panel.cScantlings.cSecondaryStiffeners.Spacing = 0 Then Exit Sub
    
    py1 = colNodes.Item(Panel.cGeometry.InNode).Y_Screen 'Panel.Z_IN_Screen
    pz1 = colNodes.Item(Panel.cGeometry.InNode).Z_Screen 'Panel.Z_IN_Screen
    py2 = colNodes.Item(Panel.cGeometry.OutNode).Y_Screen 'Panel.Z_OUT_Screen
    pz2 = colNodes.Item(Panel.cGeometry.OutNode).Z_Screen 'Panel.Z_OUT_Screen
    
    angle = Panel.cGeometry.PanelAngle
    pWidth = CLng(Sqr((py1 - py2) ^ 2 + (pz1 - pz2) ^ 2))
    WebHeight = Panel.cScantlings.cSecondaryStiffeners.WebHeight
    WebThickness = Panel.cScantlings.cSecondaryStiffeners.WebThickness
    FlangeWidth = Panel.cScantlings.cSecondaryStiffeners.FlangeWidth
    FlangeThickness = Panel.cScantlings.cSecondaryStiffeners.FlangeThickness
    Side = Panel.cScantlings.cSecondaryStiffeners.Side
    NoOfStiffeners = CLng(Panel.cGeometry.PanelWidth / Panel.cScantlings.cSecondaryStiffeners.Spacing)
    Spacing = pWidth / NoOfStiffeners
    Interval = Spacing / 2
    For i = 1 To NoOfStiffeners
        y = py1 + ((Interval * (py2 - py1)) / Sqr(((py2 - py1) ^ 2) + ((pz2 - pz1) ^ 2)))
        z = pz1 + ((Interval * (pz2 - pz1)) / Sqr(((py2 - py1) ^ 2) + ((pz2 - pz1) ^ 2)))
        Select Case Panel.cScantlings.cSecondaryStiffeners.Side
           Case SideLeft
               Call RotatePoint(size * WebHeight * CurrentScale, 0, y1, z1, -90 + angle)
               Call RotatePoint(size * WebHeight * CurrentScale, (size * FlangeWidth * CurrentScale / 2), y2, z2, -90 + angle)
               Call RotatePoint(size * WebHeight * CurrentScale, (size * -FlangeWidth * CurrentScale / 2), y3, z3, -90 + angle)
           Case SideRight
               Call RotatePoint(size * WebHeight * CurrentScale, 0, y1, z1, 90 + angle)
               Call RotatePoint(size * WebHeight * CurrentScale, (size * FlangeWidth * CurrentScale / 2), y2, z2, 90 + angle)
               Call RotatePoint(size * WebHeight * CurrentScale, (size * -FlangeWidth * CurrentScale / 2), y3, z3, 90 + angle)
            Case SideNone
                Exit Sub
        End Select
        y1 = y + y1: y2 = y + y2: y3 = y + y3
        z1 = z + z1: z2 = z + z2: z3 = z + z3
        Interval = Interval + Spacing
        ReDim lp(1)
        Dim hBrush As Long
        hBrush = Project.Item(ProjectIndex).cDisplaySettings.ColorSecondaryStiffeners
        lp(0).y = y: lp(0).z = z
        lp(1).y = y1: lp(1).z = z1
        DrawPolyline 2, hBrush
        lp(0).y = y2: lp(0).z = z2
        lp(1).y = y3: lp(1).z = z3
        DrawPolyline 2, hBrush
    Next i
    Exit Sub
DrawSecondaryStiffenersErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modDraw: Sub DrawSecondaryStiffeners")
End Sub

Private Sub DrawPrimaryFrames(ByVal Panel As cPanel)
    On Error GoTo DrawPrimaryFramesErr
    If Project.Item(ProjectIndex).cDisplaySettings.DrawPrimaryFrames = no Then Exit Sub
    If Panel.Visible = False Then Exit Sub
    'If Panel.pType <> Plate And Panel.pType <> DoubleHull Then Exit Sub
    Select Case Panel.pType
        Case Plate
        Case Else
            Exit Sub
    End Select
    size = Project.Item(ProjectIndex).cDisplaySettings.SizePrimaryFrames
    Dim DY As Long, dZ As Long
    Dim y1 As Long, z1 As Long, y2 As Long, z2 As Long
    Dim y3 As Long, z3 As Long, y4 As Long, z4 As Long
    Dim WebHeight As Long
    Dim angle As Double
    WebHeight = size * CurrentScale * Panel.cScantlings.cPrimaryFrames.WebHeight
    angle = Panel.cGeometry.PanelAngle
    
'    Dim sizepl As Double, dypl As Double, dzpl As Double
'    sizepl = Project.Item(ProjectIndex).cDisplaySettings.SizePlates
'    dypl = Panel.cScantlings.NetThickness * CurrentScale * sizepl * (Cos(PI * (angle + 90) / 180)) * IIf(Panel.cScantlings.cPrimaryFrames.Side = SideLeft, -1, 1)
'    dzpl = Panel.cScantlings.NetThickness * CurrentScale * sizepl * (Sin(PI * (angle + 90) / 180)) * IIf(Panel.cScantlings.cPrimaryFrames.Side = SideLeft, -1, 1)

    DY = (WebHeight) * (Cos(PI * (angle + 90) / 180)) '- dypl
    dZ = (WebHeight) * (Sin(PI * (angle + 90) / 180)) '- dzpl
    y1 = colNodes.Item(Panel.cGeometry.InNode).Y_Screen '+ dypl 'Panel.Z_IN_Screen
    z1 = colNodes.Item(Panel.cGeometry.InNode).Z_Screen '+ dzpl 'Panel.Z_IN_Screen
    y2 = colNodes.Item(Panel.cGeometry.OutNode).Y_Screen '+ dypl 'Panel.Z_OUT_Screen
    z2 = colNodes.Item(Panel.cGeometry.OutNode).Z_Screen '+ dzpl 'Panel.Z_OUT_Screen
    
    Select Case Panel.cScantlings.cPrimaryFrames.Side
        Case SideLeft
            y3 = y2 - DY: z3 = z2 - dZ
            y4 = y1 - DY: z4 = z1 - dZ
        Case SideRight
            y3 = y2 + DY: z3 = z2 + dZ
            y4 = y1 + DY: z4 = z1 + dZ
        Case SideNone
            Exit Sub
    End Select
    Dim rgn As Long
    ReDim lp(5)
    lp(0).y = y1: lp(0).z = z1
    lp(1).y = y2: lp(1).z = z2
    lp(2).y = y3: lp(2).z = z3
    lp(3).y = y4: lp(3).z = z4
    lp(4).y = y1: lp(4).z = z1
    rgn = CreatePolygonRgn(lp(0), 5, 1)
    Dim hBrush As Long
    hBrush = Project.Item(ProjectIndex).cDisplaySettings.ColorPrimaryFrames
    hBrush = apiCreateSolidBrush(hBrush)
    FillRgn Pic, rgn, hBrush
    DrawPolyline 5, vbBlack
    DeleteObject rgn
    DeleteObject hBrush
    Exit Sub
DrawPrimaryFramesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modDraw: Sub DrawPrimaryFrames")
End Sub

Private Sub DrawDoubleHullFrames(ByVal Panel As cPanel)
    On Error GoTo DrawDoubleHullFrames
    If Panel.pType <> DoubleHull Then Exit Sub
    If Panel.RelatedDoubleHullPanel < Panel.pNumber Then Exit Sub
    If Project.Item(ProjectIndex).cDisplaySettings.DrawPrimaryFrames = no Then Exit Sub
    If Panel.Visible = False Then Exit Sub
    size = Project.Item(ProjectIndex).cDisplaySettings.SizePrimaryFrames
    Dim DY As Long, dZ As Long
    Dim y1 As Long, z1 As Long, y2 As Long, z2 As Long
    Dim y3 As Long, z3 As Long, y4 As Long, z4 As Long
    Dim WebHeight As Long
    Dim angle As Double
    WebHeight = size * CurrentScale * Panel.cScantlings.cPrimaryFrames.WebHeight
    angle = Panel.cGeometry.PanelAngle
    DY = WebHeight * (Cos(PI * (angle + 90) / 180))
    dZ = WebHeight * (Sin(PI * (angle + 90) / 180))
    y1 = colNodes.Item(Panel.cGeometry.InNode).Y_Screen 'Panel.Z_IN_Screen
    z1 = colNodes.Item(Panel.cGeometry.InNode).Z_Screen 'Panel.Z_IN_Screen
    y2 = colNodes.Item(Panel.cGeometry.OutNode).Y_Screen 'Panel.Z_OUT_Screen
    z2 = colNodes.Item(Panel.cGeometry.OutNode).Z_Screen 'Panel.Z_OUT_Screen

    Select Case Panel.cScantlings.cPrimaryFrames.Side
        Case SideLeft
            y3 = y2 - DY: z3 = z2 - dZ
            y4 = y1 - DY: z4 = z1 - dZ
        Case SideRight
            y3 = y2 + DY: z3 = z2 + dZ
            y4 = y1 + DY: z4 = z1 + dZ
        Case SideNone
            Exit Sub
    End Select
    'Dim rgn As Long
    ReDim lp(5)
    lp(0).y = y1: lp(0).z = z1
    lp(1).y = y2: lp(1).z = z2
    lp(2).y = y3: lp(2).z = z3
    lp(3).y = y4: lp(3).z = z4
    lp(4).y = y1: lp(4).z = z1
    DeleteObject Panel.cScantlings.cPrimaryFrames.DoubleHullRgn
    Panel.cScantlings.cPrimaryFrames.DoubleHullRgn = Empty
    Panel.cScantlings.cPrimaryFrames.DoubleHullRgn = CreatePolygonRgn(lp(0), 5, 1)
    Dim hBrush As Long
    hBrush = Project.Item(ProjectIndex).cDisplaySettings.ColorPrimaryFrames
    hBrush = apiCreateSolidBrush(hBrush)
    FillRgn Pic, Panel.cScantlings.cPrimaryFrames.DoubleHullRgn, hBrush
    DeleteObject hBrush
    hBrush = CreateHatchBrush(HS_FDIAGONAL, vbBlack) ' Project.Item(ProjectIndex).cDisplaySettings.ColorBeams)
    FillRgn Pic, Panel.cScantlings.cPrimaryFrames.DoubleHullRgn, hBrush
    DrawPolyline 5, vbBlack
    DeleteObject hBrush
    Exit Sub
DrawDoubleHullFrames:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modDraw: Sub DrawDoubleHullFrames")
End Sub

Private Sub DrawSolution()
    On Error GoTo 1
    Dim i As Integer, j As Integer
    Dim ValMin As Double, ValMax As Double, DeltaVect As Double, Division() As Double
    ReDim Division(1 To 15)
    Dim Sol As cSolution
    Dim col As colPanel
    Dim LoadCaseIndex As Integer, DiagramIndex As Integer, SectionIndex As Integer
    Dim BooMin As Boolean, BooMax As Boolean
    Set Sol = Project.Item(ProjectIndex).cSolution
    If Sol.IsSolution = False Then Exit Sub
    Set col = Project.Item(ProjectIndex).colPanel
    LoadCaseIndex = Sol.CurrentLoadCase
    DiagramIndex = Sol.CurrentDiagram
    SectionIndex = Sol.CurrentSection
    ValMin = 100000000000# ' in case there are no negative values
    ValMax = -100000000000#
    DeltaVect = 0
    BooMin = False
    BooMax = False
    
'    For Each Panel In col
'        For j = 1 To 31
'            POL(LoadCaseIndex)(Panel.pNumber)(DiagramIndex)(SectionIndex)(j, 1) = Panel.colLoadCase.Item(LoadCaseIndex).colDiagram.Item(DiagramIndex).colResultsXSection.Item(SectionIndex).colResultsYSection.Item(j).Y1
'            POL(LoadCaseIndex)(Panel.pNumber)(DiagramIndex)(SectionIndex)(j, 2) = Panel.colLoadCase.Item(LoadCaseIndex).colDiagram.Item(DiagramIndex).colResultsXSection.Item(SectionIndex).colResultsYSection.Item(j).Z1
'            POL(LoadCaseIndex)(Panel.pNumber)(DiagramIndex)(SectionIndex)(j, 3) = Panel.colLoadCase.Item(LoadCaseIndex).colDiagram.Item(DiagramIndex).colResultsXSection.Item(SectionIndex).colResultsYSection.Item(j).Y2
'            POL(LoadCaseIndex)(Panel.pNumber)(DiagramIndex)(SectionIndex)(j, 4) = Panel.colLoadCase.Item(LoadCaseIndex).colDiagram.Item(DiagramIndex).colResultsXSection.Item(SectionIndex).colResultsYSection.Item(j).Z2
'            POL(LoadCaseIndex)(Panel.pNumber)(DiagramIndex)(SectionIndex)(j, 5) = Panel.colLoadCase.Item(LoadCaseIndex).colDiagram.Item(DiagramIndex).colResultsXSection.Item(SectionIndex).colResultsYSection.Item(j).Value
'            POL(LoadCaseIndex)(Panel.pNumber)(DiagramIndex)(SectionIndex)(j, 6) = Panel.colLoadCase.Item(LoadCaseIndex).colDiagram.Item(DiagramIndex).colResultsXSection.Item(SectionIndex).colResultsYSection.Item(j).ScaledValue
'        Next j
'    Next Panel
    Dim popmax  As Integer
    Dim IANA As Integer
    IANA = Project.Item(ProjectIndex).cHeader.IANA
    If IANA = 1 Then
        popmax = 31
    ElseIf IANA = 2 Then
        popmax = 3
    End If
    
    For Each Panel In col
        For j = 1 To popmax '31
'            If Panel.colLoadCase.Item(LoadCaseIndex).colDiagram.Item(DiagramIndex).colResultsXSection.Item(SectionIndex).colResultsYSection.Item(j).Value > ValMax Then
'                ValMax = Panel.colLoadCase.Item(LoadCaseIndex).colDiagram.Item(DiagramIndex).colResultsXSection.Item(SectionIndex).colResultsYSection.Item(j).Value
'            End If
'            If Panel.colLoadCase.Item(LoadCaseIndex).colDiagram.Item(DiagramIndex).colResultsXSection.Item(SectionIndex).colResultsYSection.Item(j).Value < ValMin Then
'                ValMin = Panel.colLoadCase.Item(LoadCaseIndex).colDiagram.Item(DiagramIndex).colResultsXSection.Item(SectionIndex).colResultsYSection.Item(j).Value
'            End If
            If POL(LoadCaseIndex)(Panel.pNumber)(DiagramIndex)(SectionIndex)(j, 5) > ValMax Then
                ValMax = POL(LoadCaseIndex)(Panel.pNumber)(DiagramIndex)(SectionIndex)(j, 5)
            End If
            If POL(LoadCaseIndex)(Panel.pNumber)(DiagramIndex)(SectionIndex)(j, 5) < ValMin Then
                ValMin = POL(LoadCaseIndex)(Panel.pNumber)(DiagramIndex)(SectionIndex)(j, 5)
            End If
        Next j
    Next Panel
    DeltaVect = ValMax - ValMin
    DeltaVect = DeltaVect / 14
    Division(1) = ValMin
    Division(15) = ValMax
    For i = 2 To 14
        Division(i) = Division(i - 1) + DeltaVect
    Next i
'**************************************************************************************
    Dim yy1() As Long, yy2() As Long, zz1() As Long, zz2() As Long
    Dim a As Double, b As Double
    ReDim DiagAngle(1 To col.Count)
    If IANA = 1 Then
        ReDim yy1(1 To 31): ReDim yy2(1 To 31)
        ReDim zz1(1 To 31): ReDim zz2(1 To 31)
    ElseIf IANA = 2 Then
        ReDim yy1(1 To 3): ReDim yy2(1 To 3)
        ReDim zz1(1 To 3): ReDim zz2(1 To 3)
    End If
    Dim TransformUnit As Double
    Dim MesureUnit As String
    Dim lBrush As ColorConstants
    Dim fBrush(1 To 16) As ColorConstants
    Dim sValue(0 To 16) As String
    Dim pNumber As Integer, pAngle As Double
    Dim DiagramScale As Double
    DiagramScale = Sol.DiagramScale
    For Each Panel In col
        If Panel.Visible = False Then GoTo nextpanel
        pNumber = Panel.pNumber
        pAngle = Panel.cGeometry.PanelAngle
        If IANA = 1 Then
            Select Case DiagramIndex
                Case 2, 4, 7, 12
                    DiagAngle(pNumber) = -45
                Case Else
                    DiagAngle(pNumber) = pAngle + 90
            End Select
        
            Select Case DiagramIndex
                Case 1, 2, 3
                    TransformUnit = 1000
                    MesureUnit = "mm"
                Case 4, 5, 6, 9, 10
                    TransformUnit = 1 / 1000
                    MesureUnit = "N/mm"
                Case 7, 8
                    TransformUnit = 1 / 1000
                    MesureUnit = "kNm/m"
                Case 11, 12, 13, 14, 15, 16, 17, 18
                    TransformUnit = 1 / 1000000
                    MesureUnit = "N/mm2"
                Case Else
                    TransformUnit = 1
            End Select
        ElseIf IANA = 2 Then
            Select Case DiagramIndex
                Case 2
                    DiagAngle(pNumber) = -45
                Case Else
                    DiagAngle(pNumber) = pAngle + 90
            End Select
        
            Select Case DiagramIndex
                Case 1, 2, 3
                    TransformUnit = 1 '1000
                    MesureUnit = "N/mm2"
                Case Else
                    TransformUnit = 1
            End Select
        End If
        
        POL(LoadCaseIndex)(pNumber)(DiagramIndex)(SectionIndex)(1, 1) = colNodes.Item(Panel.cGeometry.InNode).Y_Screen
        POL(LoadCaseIndex)(pNumber)(DiagramIndex)(SectionIndex)(1, 2) = colNodes.Item(Panel.cGeometry.InNode).Z_Screen
        POL(LoadCaseIndex)(pNumber)(DiagramIndex)(SectionIndex)(popmax, 1) = colNodes.Item(Panel.cGeometry.OutNode).Y_Screen
        POL(LoadCaseIndex)(pNumber)(DiagramIndex)(SectionIndex)(popmax, 2) = colNodes.Item(Panel.cGeometry.OutNode).Z_Screen
'        Panel.colLoadCase.Item(LoadCaseIndex).colDiagram.Item(DiagramIndex).colResultsXSection.Item(SectionIndex).colResultsYSection.Item(1).Y1 = colNodes.Item(Panel.cGeometry.InNode).Y_Screen
'        Panel.colLoadCase.Item(LoadCaseIndex).colDiagram.Item(DiagramIndex).colResultsXSection.Item(SectionIndex).colResultsYSection.Item(1).Z1 = colNodes.Item(Panel.cGeometry.InNode).Z_Screen
'        Panel.colLoadCase.Item(LoadCaseIndex).colDiagram.Item(DiagramIndex).colResultsXSection.Item(SectionIndex).colResultsYSection.Item(31).Y1 = colNodes.Item(Panel.cGeometry.OutNode).Y_Screen
'        Panel.colLoadCase.Item(LoadCaseIndex).colDiagram.Item(DiagramIndex).colResultsXSection.Item(SectionIndex).colResultsYSection.Item(31).Z1 = colNodes.Item(Panel.cGeometry.OutNode).Z_Screen
        Dim Step As Double
        Step = Panel.cGeometry.PanelWidth * CurrentScale / (popmax - 1) '30
        Dim m As Integer
        For m = 1 To popmax - 2 '29
            POL(LoadCaseIndex)(pNumber)(DiagramIndex)(SectionIndex)(m + 1, 1) = POL(LoadCaseIndex)(pNumber)(DiagramIndex)(SectionIndex)(1, 1) + Step * m * Cos(pAngle * PI / 180)
            POL(LoadCaseIndex)(pNumber)(DiagramIndex)(SectionIndex)(m + 1, 2) = POL(LoadCaseIndex)(pNumber)(DiagramIndex)(SectionIndex)(1, 2) + Step * m * Sin(pAngle * PI / 180)
'            Panel.colLoadCase.Item(LoadCaseIndex).colDiagram.Item(DiagramIndex).colResultsXSection.Item(SectionIndex).colResultsYSection.Item(m + 1).Y1 = Panel.colLoadCase.Item(LoadCaseIndex).colDiagram.Item(DiagramIndex).colResultsXSection.Item(SectionIndex).colResultsYSection.Item(1).Y1 + Step * m * Cos(pAngle * PI / 180)
'            Panel.colLoadCase.Item(LoadCaseIndex).colDiagram.Item(DiagramIndex).colResultsXSection.Item(SectionIndex).colResultsYSection.Item(m + 1).Z1 = Panel.colLoadCase.Item(LoadCaseIndex).colDiagram.Item(DiagramIndex).colResultsXSection.Item(SectionIndex).colResultsYSection.Item(1).Z1 + Step * m * Sin(pAngle * PI / 180)
        Next m
        
         For i = 1 To popmax '31
             POL(LoadCaseIndex)(pNumber)(DiagramIndex)(SectionIndex)(i, 3) = _
             POL(LoadCaseIndex)(pNumber)(DiagramIndex)(SectionIndex)(i, 1) + _
             POL(LoadCaseIndex)(pNumber)(DiagramIndex)(SectionIndex)(i, 6) * _
             Cos((DiagAngle(pNumber)) * PI / 180) * DiagramScale * CurrentScale
'            Panel.colLoadCase.Item(LoadCaseIndex).colDiagram.Item(DiagramIndex).colResultsXSection.Item(SectionIndex).colResultsYSection.Item(i).Y2 = _
'            Panel.colLoadCase.Item(LoadCaseIndex).colDiagram.Item(DiagramIndex).colResultsXSection.Item(SectionIndex).colResultsYSection.Item(i).Y1 + _
'            Panel.colLoadCase.Item(LoadCaseIndex).colDiagram.Item(DiagramIndex).colResultsXSection.Item(SectionIndex).colResultsYSection.Item(i).ScaledValue * _
'            Cos((DiagAngle(pNumber)) * PI / 180) * DiagramScale * CurrentScale
            
             POL(LoadCaseIndex)(pNumber)(DiagramIndex)(SectionIndex)(i, 4) = _
             POL(LoadCaseIndex)(pNumber)(DiagramIndex)(SectionIndex)(i, 2) + _
             POL(LoadCaseIndex)(pNumber)(DiagramIndex)(SectionIndex)(i, 6) * _
             Sin((DiagAngle(pNumber)) * PI / 180) * DiagramScale * CurrentScale
'            Panel.colLoadCase.Item(LoadCaseIndex).colDiagram.Item(DiagramIndex).colResultsXSection.Item(SectionIndex).colResultsYSection.Item(i).Z2 = _
'            Panel.colLoadCase.Item(LoadCaseIndex).colDiagram.Item(DiagramIndex).colResultsXSection.Item(SectionIndex).colResultsYSection.Item(i).Z1 + _
'            Panel.colLoadCase.Item(LoadCaseIndex).colDiagram.Item(DiagramIndex).colResultsXSection.Item(SectionIndex).colResultsYSection.Item(i).ScaledValue * _
'            Sin((DiagAngle(pNumber)) * PI / 180) * DiagramScale * CurrentScale

         Next i
        For i = 1 To popmax '31
             yy1(i) = POL(LoadCaseIndex)(pNumber)(DiagramIndex)(SectionIndex)(i, 1)
             zz1(i) = POL(LoadCaseIndex)(pNumber)(DiagramIndex)(SectionIndex)(i, 2)
             yy2(i) = POL(LoadCaseIndex)(pNumber)(DiagramIndex)(SectionIndex)(i, 3)
             zz2(i) = POL(LoadCaseIndex)(pNumber)(DiagramIndex)(SectionIndex)(i, 4)
'            yy1(i) = Panel.colLoadCase.Item(LoadCaseIndex).colDiagram.Item(DiagramIndex).colResultsXSection.Item(SectionIndex).colResultsYSection.Item(i).Y1
'            zz1(i) = Panel.colLoadCase.Item(LoadCaseIndex).colDiagram.Item(DiagramIndex).colResultsXSection.Item(SectionIndex).colResultsYSection.Item(i).Z1
'            yy2(i) = Panel.colLoadCase.Item(LoadCaseIndex).colDiagram.Item(DiagramIndex).colResultsXSection.Item(SectionIndex).colResultsYSection.Item(i).Y2
'            zz2(i) = Panel.colLoadCase.Item(LoadCaseIndex).colDiagram.Item(DiagramIndex).colResultsXSection.Item(SectionIndex).colResultsYSection.Item(i).Z2
        Next i
        For i = 1 To popmax '31
             a = Val_(POL(LoadCaseIndex)(pNumber)(DiagramIndex)(SectionIndex)(i, 5))
             'A = Val_(Panel.colLoadCase.Item(LoadCaseIndex).colDiagram.Item(DiagramIndex).colResultsXSection.Item(SectionIndex).colResultsYSection.Item(i).Value)
             b = TransformUnit
             If a = Division(1) Then
                 lBrush = Sol.Color2
                 sValue(1) = Round(a * b, 2)
             End If
             If a > Division(1) And a <= Division(2) Then
                 lBrush = Sol.Color2
                 sValue(2) = Round((Division(1) * b), 1) & " ... " & Round((Division(2) * b), 1)
             End If
             If a > Division(2) And a <= Division(3) Then
                 lBrush = Sol.Color3
                 sValue(3) = Round((Division(2) * b), 1) & " ... " & Round((Division(3) * b), 1)
             End If
             If a > Division(3) And a <= Division(4) Then
                 lBrush = Sol.Color4
                 sValue(4) = Round((Division(3) * b), 1) & " ... " & Round((Division(4) * b), 1)
             End If
             If a > Division(4) And a <= Division(5) Then
                 lBrush = Sol.Color5
                 sValue(5) = Round((Division(4) * b), 1) & " ... " & Round((Division(5) * b), 1)
             End If
             If a > Division(5) And a <= Division(6) Then
                 lBrush = Sol.Color6
                 sValue(6) = Round((Division(5) * b), 1) & " ... " & Round((Division(6) * b), 1)
             End If
             If a > Division(6) And a <= Division(7) Then
                 lBrush = Sol.Color7
                 sValue(7) = Round((Division(6) * b), 1) & " ... " & Round((Division(7) * b), 1)
             End If
             If a > Division(7) And a <= Division(8) Then
                 lBrush = Sol.Color8
                 sValue(8) = Round((Division(7) * b), 1) & " ... " & Round((Division(8) * b), 1)
             End If
             If a > Division(8) And a <= Division(9) Then
                 lBrush = Sol.Color9
                 sValue(9) = Round((Division(8) * b), 1) & " ... " & Round((Division(9) * b), 1)
             End If
             If a > Division(9) And a <= Division(10) Then
                 lBrush = Sol.Color10
                 sValue(10) = Round((Division(9) * b), 1) & " ... " & Round((Division(10) * b), 1)
             End If
             If a > Division(10) And a <= Division(11) Then
                 lBrush = Sol.Color11
                 sValue(11) = Round((Division(10) * b), 1) & " ... " & Round((Division(11) * b), 1)
             End If
             If a > Division(11) And a <= Division(12) Then
                 lBrush = Sol.Color12
                 sValue(12) = Round((Division(11) * b), 1) & " ... " & Round((Division(12) * b), 1)
             End If
             If a > Division(12) And a <= Division(13) Then
                 lBrush = Sol.Color13
                 sValue(13) = Round((Division(12) * b), 1) & " ... " & Round((Division(13) * b), 1)
             End If
             If a > Division(13) And a <= Division(14) Then
                 lBrush = Sol.Color14
                 sValue(14) = Round((Division(13) * b), 1) & " ... " & Round((Division(14) * b), 1)
             End If
             If a > Division(14) And a < Division(15) Then
                 lBrush = Sol.Color15
                 sValue(15) = Round((Division(14) * b), 1) & " ... " & Round((Division(15) * b), 1)
            End If
            If a = Division(15) Then
                lBrush = Sol.Color16
                sValue(16) = Round(a * b, 2)
            End If
            
            Dim rgn As Long, hBrush As Long
            If i < popmax Then
                ReDim lp(4)
                lp(0).y = yy1(i): lp(0).z = zz1(i)
                lp(1).y = yy2(i): lp(1).z = zz2(i)
                lp(2).y = yy2(i + 1): lp(2).z = zz2(i + 1)
                lp(3).y = yy1(i + 1): lp(3).z = zz1(i + 1)
                lp(4).y = yy1(i): lp(4).z = zz1(i)
                rgn = CreatePolygonRgn(lp(0), 5, 2)
                hBrush = apiCreateSolidBrush(lBrush)
                'Project.Item(ProjectIndex).frmProject.picScreen.DrawStyle = 1
                FillRgn& Pic, rgn, hBrush
                'Project.Item(ProjectIndex).frmProject.picScreen.DrawStyle = 0
                DeleteObject hBrush
                DeleteObject rgn
            End If
            
            ReDim lp(1)
'            lp(0).y = yy1(i): lp(0).Z = zz1(i)
'            lp(1).y = yy2(i): lp(1).Z = zz2(i)
'            DrawPolyline 2, vbBlack

'-------- temp ------
    If i = 1 Or i = popmax Then
        lp(0).y = yy1(i): lp(0).z = zz1(i)
        lp(1).y = yy2(i): lp(1).z = zz2(i)
        DrawPolyline 2, vbBlack
    End If
'--------------------


            If i < popmax Then
                lp(0).y = yy2(i): lp(0).z = zz2(i)
                lp(1).y = yy2(i + 1): lp(1).z = zz2(i + 1)
                DrawPolyline 2, vbBlack
            End If
        Next i
        
        Dim yy_Min As Long, zz_Min As Long, yy_Max As Long, zz_Max As Long
        For i = 1 To popmax '31
           a = Val_(POL(LoadCaseIndex)(pNumber)(DiagramIndex)(SectionIndex)(i, 5))
           'A = Panel.colLoadCase.Item(LoadCaseIndex).colDiagram.Item(DiagramIndex).colResultsXSection.Item(SectionIndex).colResultsYSection.Item(i).Value
           If a = Division(1) Then
                sValue(1) = Round(a * b, 2)
                If BooMin = False Then
                    yy_Min = yy2(i): zz_Min = zz2(i)
                    BooMin = True
               End If
           End If
        
           If a = Division(15) Then
               sValue(16) = Round(a * b, 2)
               If BooMax = False Then
                    yy_Max = yy2(i): zz_Max = zz2(i)
                    BooMax = True
               End If
           End If
        Next i

       
      
'        If Panel.Selected = isSelected Then
'            For i = 1 To popmax '31
'                lrect.Left = yy2(i): lrect.Top = zz2(i): lrect.right = lrect.Left + 30: lrect.bottom = lrect.Top + 15
'                DrawTxt Round(Val_(POL(LoadCaseIndex)(pNumber) _
'                (DiagramIndex)(SectionIndex)(i, 5)) * TransformUnit, 2), vbRed, vbYellow
''                DrawTxt Round(Val_(Panel.colLoadCase.Item(LoadCaseIndex).colDiagram.Item(DiagramIndex).colResultsXSection.Item(SectionIndex).colResultsYSection.Item(i).Value * TransformUnit, 2)), vbRed, vbYellow
'            Next i
'        End If
        
'        Dim lrect1 As RECT, lrect2 As RECT, lrect3 As RECT
'        Dim index As Integer
'        Dim str As String, lenstr As Long
'        If Panel.Selected = isSelected Then
'
'            index = 1
'            str = Format(Round(Val_(POL(LoadCaseIndex)(pNumber) _
'                (DiagramIndex)(SectionIndex)(index, 5)) * TransformUnit, 2), "0.00")
'            lenstr = Len(str) * 6
'            lrect.Left = yy2(index) - lenstr: lrect.Top = zz2(index): lrect.right = lrect.Left + lenstr: lrect.bottom = lrect.Top + 15
'            DrawTxt str, vbRed, vbYellow
'
'            For i = index To popmax - 1
'
'                lrect1.Left = yy2(index) - lenstr: lrect1.Top = zz2(index): lrect1.right = lrect1.Left + lenstr: lrect1.bottom = lrect1.Top + 15
'                lrect2.Left = yy2(i + 1) - lenstr: lrect2.Top = zz2(i + 1): lrect2.right = lrect2.Left + lenstr: lrect2.bottom = lrect2.Top + 15
'
'                If IntersectRect(lrect3, lrect1, lrect2) = 0 Then
'                    index = i + 1
'                    lrect = lrect2
''                    DrawTxt Format(Round(Val_(POL(LoadCaseIndex)(pNumber) _
''                        (DiagramIndex)(SectionIndex)(index, 5)) * TransformUnit, 2), "0.00"), vbRed, vbYellow
'                str = Format(Round(Val_(POL(LoadCaseIndex)(pNumber) _
'                (DiagramIndex)(SectionIndex)(index, 5)) * TransformUnit, 2), "0.00")
'                lenstr = Len(str) * 6
'                DrawTxt str, vbRed, vbYellow
'                End If
'
'            Next i
'
'        End If
        
nextpanel:
    Next Panel
    
    lrect.Left = yy_Min: lrect.Top = zz_Min: lrect.right = lrect.Left + 30: lrect.bottom = lrect.Top + 15
    DrawTxt "MIN", vbWhite, vbBlack
    lrect.Left = yy_Max: lrect.Top = zz_Max: lrect.right = lrect.Left + 30: lrect.bottom = lrect.Top + 15
    DrawTxt "MAX", vbWhite, vbBlack
    
    If Sol.ShowLegend = True Then
        lrect.Left = 5: lrect.right = 120: lrect.Top = 5: lrect.bottom = 20
        sValue(0) = GetDiagramHeading(DiagramIndex, MesureUnit)
        DrawTxt sValue(0), vbWhite, vbBlack
        For i = 1 To 16
            If sValue(i) <> "" Then
            lrect.Top = lrect.Top + 14: lrect.bottom = lrect.Top + 15
            DrawTxt sValue(i), Sol.GetColorMatrix(i), vbBlack
            End If
        Next i
    End If
    
    If Sol.ShowNeutralAxis = True Then
'        Dim header As cHeader
'        Dim XMIN As Double, XMAX As Double
'        XMIN = Project.Item(ProjectIndex).XMIN * CurrentScale
'        XMAX = Project.Item(ProjectIndex).XMAX * CurrentScale
'        Set header = Project.Item(ProjectIndex).cHeader
'        ReDim lp(1)
'        If Sol.NeutralAxis <> 0 Then
'            Select Case header.ZAxisOrigin
'                Case Is <= 0
'                    lp(0).X = XMIN
'                    lp(0).Y = Abs(header.ZAxisOrigin) * CurrentScale - Sol.NeutralAxis * CurrentScale
'                    lp(1).X = XMAX
'                    lp(1).Y = lp(0).Y
'                    DrawPolyline 2, vbGreen
'                Case Is > 0
'                    lp(0).X = XMIN
'                    lp(0).Y = -header.ZAxisOrigin * CurrentScale - Sol.NeutralAxis * CurrentScale
'                    lp(1).X = XMAX
'                    lp(1).Y = lp(0).Y
'                    DrawPolyline 2, vbGreen
'            End Select
'        End If
'        Set header = Nothing
    End If
    Set Sol = Nothing: Set col = Nothing
1
End Sub

Private Function TextSolutionSelected(ByVal Panel As cPanel)
    If Panel.Selected = isSelected Then
        Dim i As Integer, popmax As Integer
        popmax = 31
        Dim Sol As cSolution
        Dim LoadCaseIndex As Integer, DiagramIndex As Integer, SectionIndex As Integer
        Dim TransformUnit As Double
        Set Sol = Project.Item(ProjectIndex).cSolution
        If Sol.IsSolution = False Then Exit Function
        Dim pNumber As Integer
        Dim lrect1 As RECT, lrect2 As RECT, lrect3 As RECT
        Dim index As Integer
        Dim str As String, lenstr As Long
        
        LoadCaseIndex = Sol.CurrentLoadCase
        DiagramIndex = Sol.CurrentDiagram
        SectionIndex = Sol.CurrentSection
        If LoadCaseIndex = 0 Or DiagramIndex = 0 Or SectionIndex = 0 Then
            Exit Function
        End If
        pNumber = Panel.pNumber
        
        Dim yy1() As Long, yy2() As Long, zz1() As Long, zz2() As Long
        
        Dim IANA As Integer
        IANA = Project.Item(ProjectIndex).cHeader.IANA
        If IANA = 1 Then
            ReDim yy1(1 To 31): ReDim yy2(1 To 31)
            ReDim zz1(1 To 31): ReDim zz2(1 To 31)
        ElseIf IANA = 2 Then
            ReDim yy1(1 To 3): ReDim yy2(1 To 3)
            ReDim zz1(1 To 3): ReDim zz2(1 To 3)
        End If
        
        For i = 1 To popmax '31
             yy1(i) = POL(LoadCaseIndex)(pNumber)(DiagramIndex)(SectionIndex)(i, 1)
             zz1(i) = POL(LoadCaseIndex)(pNumber)(DiagramIndex)(SectionIndex)(i, 2)
             yy2(i) = POL(LoadCaseIndex)(pNumber)(DiagramIndex)(SectionIndex)(i, 3)
             zz2(i) = POL(LoadCaseIndex)(pNumber)(DiagramIndex)(SectionIndex)(i, 4)
        Next i
    
        Select Case DiagramIndex
            Case 1, 2, 3
                TransformUnit = 1000
                'MesureUnit = "mm"
            Case 4, 5, 6, 9, 10
                TransformUnit = 1 / 1000
                'MesureUnit = "N/mm"
            Case 7, 8
                TransformUnit = 1 / 1000
                'MesureUnit = "kNm/m"
            Case 11, 12, 13, 14, 15, 16, 17, 18
                TransformUnit = 1 / 1000000
                'MesureUnit = "N/mm2"
            Case Else
                TransformUnit = 1
        End Select

        index = 1
        str = Format(Round(Val_(POL(LoadCaseIndex)(pNumber) _
            (DiagramIndex)(SectionIndex)(index, 5)) * TransformUnit, 2), "0.00")
        lenstr = Len(str) * 6
        lrect.Left = yy2(index) - lenstr: lrect.Top = zz2(index): lrect.right = lrect.Left + lenstr: lrect.bottom = lrect.Top + 15
        DrawTxt str, vbRed, vbYellow

        For i = index To popmax - 1

            lrect1.Left = yy2(index) - lenstr: lrect1.Top = zz2(index): lrect1.right = lrect1.Left + lenstr: lrect1.bottom = lrect1.Top + 15
            lrect2.Left = yy2(i + 1) - lenstr: lrect2.Top = zz2(i + 1): lrect2.right = lrect2.Left + lenstr: lrect2.bottom = lrect2.Top + 15

            If IntersectRect(lrect3, lrect1, lrect2) = 0 Then
                index = i + 1
                lrect = lrect2
                str = Format(Round(Val_(POL(LoadCaseIndex)(pNumber) _
                        (DiagramIndex)(SectionIndex)(index, 5)) * TransformUnit, 2), "0.00")
                lenstr = Len(str) * 6
                lrect.Left = yy2(index) - lenstr: lrect.Top = zz2(index): lrect.right = lrect.Left + lenstr: lrect.bottom = lrect.Top + 15
                DrawTxt str, vbRed, vbYellow
            End If

        Next i

    End If
End Function

Private Function GetDiagramHeading(ByVal index As Integer, ByVal MesureUnit As String) As String
    If Project.Item(ProjectIndex).cHeader.IANA = 1 Then
        Select Case index
            Case 1
                GetDiagramHeading = "V " & "[" & MesureUnit & "]"
            Case 2
                GetDiagramHeading = "U " & "[" & MesureUnit & "]"
            Case 3
                GetDiagramHeading = "W " & "[" & MesureUnit & "]"
            Case 4
                GetDiagramHeading = "Nx " & "[" & MesureUnit & "]"
            Case 5
                GetDiagramHeading = "Ny " & "[" & MesureUnit & "]"
            Case 6
                GetDiagramHeading = "Nxy " & "[" & MesureUnit & "]"
            Case 7
                GetDiagramHeading = "Mx " & "[" & MesureUnit & "]"
            Case 8
                GetDiagramHeading = "My " & "[" & MesureUnit & "]"
            Case 9
                GetDiagramHeading = "Ry " & "[" & MesureUnit & "]"
            Case 10
                GetDiagramHeading = "Rx " & "[" & MesureUnit & "]"
            Case 11
                GetDiagramHeading = "Sy " & "[" & MesureUnit & "]"
            Case 12
                GetDiagramHeading = "Sx " & "[" & MesureUnit & "]"
            Case 13
                GetDiagramHeading = "Txy " & "[" & MesureUnit & "]"
            Case 14
                GetDiagramHeading = "Sc Plates " & "[" & MesureUnit & "]"
            Case 15
                GetDiagramHeading = "Sy Frames " & "[" & MesureUnit & "]"
            Case 16
                GetDiagramHeading = "Tyz Frames " & "[" & MesureUnit & "]"
            Case 17
                GetDiagramHeading = "Sc WFJ Fr." & "[" & MesureUnit & "]"
            Case 18
                GetDiagramHeading = "Sc WPJ Fr." & "[" & MesureUnit & "]"
        End Select
    ElseIf Project.Item(ProjectIndex).cHeader.IANA = 2 Then
        Select Case index
            Case 1
                GetDiagramHeading = "Txy " & "[" & MesureUnit & "]"
            Case 2
                GetDiagramHeading = "Sx " & "[" & MesureUnit & "]"
            Case 3
                GetDiagramHeading = "Sx_stiff " & "[" & MesureUnit & "]"
        End Select
    End If
End Function

Private Sub TextPrimaryFrames(ByVal Panel As cPanel)
    On Error GoTo TextPrimaryFramesErr
    Dim dx As Long, DY As Long
    Dim Y_IN As Long, Z_IN As Long, Y_OUT As Long, Z_OUT As Long
    Dim angle As Double
    Dim iSide As Integer
    Dim dValue As String
    If Panel.Visible = False Then Exit Sub
    If Panel.pType <> Plate And Panel.pType <> DoubleHull Then Exit Sub
    Y_IN = colNodes.Item(Panel.cGeometry.InNode).Y_Screen 'Panel.Z_IN_Screen
    Z_IN = colNodes.Item(Panel.cGeometry.InNode).Z_Screen 'Panel.Z_IN_Screen
    Y_OUT = colNodes.Item(Panel.cGeometry.OutNode).Y_Screen 'Panel.Z_OUT_Screen
    Z_OUT = colNodes.Item(Panel.cGeometry.OutNode).Z_Screen 'Panel.Z_OUT_Screen

    angle = Panel.cGeometry.PanelAngle
    Select Case Panel.cScantlings.cPrimaryFrames.Side
        Case SideLeft
            iSide = -1
        Case SideRight
            iSide = 1
        Case SideNone
            Exit Sub
    End Select
    size = Project.Item(ProjectIndex).cDisplaySettings.SizePrimaryFrames
    Dim offset As Long
    offset = CurrentScale * size * iSide * Panel.cScantlings.cPrimaryFrames.WebHeight / 2
    dx = offset * Cos(PI * (angle + 90) / 180)
    DY = offset * Sin(PI * (angle + 90) / 180)
    Dim lenTxt As Integer
    Select Case Project.Item(ProjectIndex).cDisplaySettings.TextPrimaryFrames
        Case None
            Exit Sub
        Case WebHeight
            If Project.Item(ProjectIndex).cSolution.ShowVariation = True Then
                dValue = " " & CStr(Panel.cScantlings.cPrimaryFrames.WebHeightVariation) & "%" & " "
            Else
                dValue = " " & CStr(Panel.cScantlings.cPrimaryFrames.WebHeight * 1000) & " "
            End If
        Case WebThickness
            If Project.Item(ProjectIndex).cSolution.ShowVariation = True Then
                dValue = " " & CStr(Panel.cScantlings.cPrimaryFrames.WebThicknessVariation) & "%" & " "
            Else
                dValue = " " & CStr(Panel.cScantlings.cPrimaryFrames.WebThickness * 1000) & " "
            End If
        Case WebGrossThickness
            If Project.Item(ProjectIndex).cSolution.ShowVariation = True Then
                dValue = " " & CStr(Panel.cScantlings.cPrimaryFrames.WebThicknessVariation) & "%" & " "
            Else
                dValue = " " & CStr((Panel.cScantlings.cPrimaryFrames.WebThickness + Panel.cScantlings.cPrimaryFrames.CorrosionThickness) * 1000) & " "
            End If
        Case FlangeWidth
            If Project.Item(ProjectIndex).cSolution.ShowVariation = True Then
                dValue = " " & CStr(Panel.cScantlings.cPrimaryFrames.FlangeWidthVariation) & "%" & " "
            Else
                dValue = " " & CStr(Panel.cScantlings.cPrimaryFrames.FlangeWidth * 1000) & " "
            End If
        Case FlangeThickness
            If Project.Item(ProjectIndex).cSolution.ShowVariation = True Then
                dValue = " " & CStr(Panel.cScantlings.cPrimaryFrames.FlangeThicknessVariation) & "%" & " "
            Else
                dValue = " " & CStr(Panel.cScantlings.cPrimaryFrames.FlangeThickness * 1000) & " "
            End If
        Case FlangeGrossThickness
            If Project.Item(ProjectIndex).cSolution.ShowVariation = True Then
                dValue = " " & CStr(Panel.cScantlings.cPrimaryFrames.FlangeThicknessVariation) & "%" & " "
            Else
                dValue = " " & CStr((Panel.cScantlings.cPrimaryFrames.FlangeThickness + Panel.cScantlings.cPrimaryFrames.CorrosionThickness) * 1000) & " "
            End If
        Case Spacing
            If Project.Item(ProjectIndex).cSolution.ShowVariation = True Then
                dValue = " " & CStr(Panel.cScantlings.cPrimaryFrames.SpacingVariation) & "%" & " "
            Else
                dValue = " " & CStr(Panel.cScantlings.cPrimaryFrames.Spacing * 1000) & " "
            End If
        Case All
    End Select
    lenTxt = Len(CStr(dValue))
    lrect.Top = DY + (Z_IN + Z_OUT) / 2 - 8
    lrect.bottom = DY + (Z_IN + Z_OUT) / 2 + 6
    lrect.Left = dx + (Y_IN + Y_OUT) / 2 - lenTxt * 3
    lrect.right = dx + (Y_IN + Y_OUT) / 2 + lenTxt * 3
    DrawTxt (dValue), Project.Item(ProjectIndex).cDisplaySettings.ColorPrimaryFrames, vbBlack
    Exit Sub
TextPrimaryFramesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modDraw: Sub TextPrimaryFrames")
End Sub

Private Sub TextPrimaryStiffeners(ByVal Panel As cPanel)
    On Error GoTo TextPrimaryStiffenersErr
    If Panel.Visible = False Then Exit Sub
    Dim DY As Long, dZ As Long
    Dim Y_IN As Long, Z_IN As Long, Y_OUT As Long, Z_OUT As Long
    Dim angle As Double
    Dim iSide As Integer
    Dim dValue As String
    If Panel.pType <> Plate And Panel.pType <> DoubleHull Then Exit Sub
    Y_IN = colNodes.Item(Panel.cGeometry.InNode).Y_Screen 'Panel.Z_IN_Screen
    Z_IN = colNodes.Item(Panel.cGeometry.InNode).Z_Screen 'Panel.Z_IN_Screen
    Y_OUT = colNodes.Item(Panel.cGeometry.OutNode).Y_Screen 'Panel.Z_OUT_Screen
    Z_OUT = colNodes.Item(Panel.cGeometry.OutNode).Z_Screen 'Panel.Z_OUT_Screen

    angle = Panel.cGeometry.PanelAngle
    Select Case Panel.cScantlings.cPrimaryStiffeners.Side
        Case SideLeft
            iSide = -1
        Case SideRight
            iSide = 1
        Case SideNone
            Exit Sub
    End Select
    
    size = Project.Item(ProjectIndex).cDisplaySettings.SizePrimaryStiffeners
    Dim offset As Long
    offset = CurrentScale * size * iSide * Panel.cScantlings.cPrimaryStiffeners.WebHeight
    DY = offset * Cos(PI * (angle + 90) / 180)
    dZ = offset * Sin(PI * (angle + 90) / 180)
    Dim lenTxt As Integer
    Select Case Project.Item(ProjectIndex).cDisplaySettings.TextPrimaryStiffeners
        Case None
            Exit Sub
        Case WebHeight
            If Project.Item(ProjectIndex).cSolution.ShowVariation = True Then
                dValue = " " & CStr(Panel.cScantlings.cPrimaryStiffeners.WebHeightVariation) & "%" & " "
            Else
                dValue = " " & CStr(Panel.cScantlings.cPrimaryStiffeners.WebHeight * 1000) & " "
            End If
        Case WebThickness
            If Project.Item(ProjectIndex).cSolution.ShowVariation = True Then
                dValue = " " & CStr(Panel.cScantlings.cPrimaryStiffeners.WebThicknessVariation) & "%" & " "
            Else
                dValue = " " & CStr(Panel.cScantlings.cPrimaryStiffeners.WebThickness * 1000) & " "
            End If
        Case WebGrossThickness
            If Project.Item(ProjectIndex).cSolution.ShowVariation = True Then
                dValue = " " & CStr(Panel.cScantlings.cPrimaryStiffeners.WebThicknessVariation) & "%" & " "
            Else
                dValue = " " & CStr((Panel.cScantlings.cPrimaryStiffeners.WebThickness + Panel.cScantlings.cPrimaryStiffeners.CorrosionThickness) * 1000) & " "
            End If
        Case FlangeWidth
            If Project.Item(ProjectIndex).cSolution.ShowVariation = True Then
                dValue = " " & CStr(Panel.cScantlings.cPrimaryStiffeners.FlangeWidthVariation) & "%" & " "
            Else
                dValue = " " & CStr(Panel.cScantlings.cPrimaryStiffeners.FlangeWidth * 1000) & " "
            End If
        Case FlangeThickness
            If Project.Item(ProjectIndex).cSolution.ShowVariation = True Then
                dValue = " " & CStr(Panel.cScantlings.cPrimaryStiffeners.FlangeThicknessVariation) & "%" & " "
            Else
                dValue = " " & CStr(Panel.cScantlings.cPrimaryStiffeners.FlangeThickness * 1000) & " "
            End If
        Case FlangeGrossThickness
            If Project.Item(ProjectIndex).cSolution.ShowVariation = True Then
                dValue = " " & CStr(Panel.cScantlings.cPrimaryStiffeners.FlangeThicknessVariation) & "%" & " "
            Else
                dValue = " " & CStr((Panel.cScantlings.cPrimaryStiffeners.FlangeThickness + Panel.cScantlings.cPrimaryStiffeners.CorrosionThickness) * 1000) & " "
            End If
        Case Spacing
            If Project.Item(ProjectIndex).cSolution.ShowVariation = True Then
                dValue = " " & CStr(Panel.cScantlings.cPrimaryStiffeners.SpacingVariation) & "%" & " "
            Else
                dValue = " " & CStr(Panel.cScantlings.cPrimaryStiffeners.Spacing * 1000) & " "
            End If
        Case GrossSectionModulus
            If Project.Item(ProjectIndex).cSolution.ShowVariation = True Then
                dValue = " " & CStr(Panel.cScantlings.cPrimaryStiffeners.GrossSectionModulusVariation) & "%" & " "
            Else
'                dValue = " " & CStr(GetSectionModulus(Panel.cScantlings.cPrimaryStiffeners.WebHeight, _
'                Panel.cScantlings.cPrimaryStiffeners.WebThickness + Panel.cScantlings.cPrimaryStiffeners.CorrosionThickness, _
'                Panel.cScantlings.cPrimaryStiffeners.FlangeWidth, _
'                Panel.cScantlings.cPrimaryStiffeners.FlangeThickness + Panel.cScantlings.cPrimaryStiffeners.CorrosionThickness, _
'                Panel.cScantlings.cPrimaryStiffeners.Spacing, _
'                Panel.cScantlings.cPrimaryFrames.Spacing, _
'                Panel.cScantlings.GrossThickness)) & " "
                dValue = " " & CStr(Round(Panel.cScantlings.cPrimaryStiffeners.GrossSectionModulus, 2)) & " "
                '" " & CStr(Panel.cScantlings.cPrimaryStiffeners.Spacing * 1000) & " "
            End If
        Case All
        
'        dValue = CStr(CStr(Panel.cScantlings.cPrimaryStiffeners.WebHeight * 1000) & " x " & CStr(Panel.cScantlings.cPrimaryStiffeners.WebThickness * 1000) & vbCrLf & _
'                CStr(Panel.cScantlings.cPrimaryStiffeners.FlangeWidth * 1000) & " x " & CStr(Panel.cScantlings.cPrimaryStiffeners.FlangeThickness * 1000) & vbCrLf & _
'                CStr(Panel.cScantlings.cPrimaryStiffeners.Spacing * 1000))
    End Select
    lenTxt = Len(CStr(dValue))
    lrect.Top = dZ + (Z_IN + Z_OUT) / 2 + 4
    lrect.bottom = dZ + (Z_IN + Z_OUT) / 2 + 18
    lrect.Left = DY + (Y_IN + Y_OUT) / 2 - lenTxt * 3
    lrect.right = DY + (Y_IN + Y_OUT) / 2 + lenTxt * 3
    DrawTxt (dValue), vbWhite, Project.Item(ProjectIndex).cDisplaySettings.ColorPrimaryStiffeners
    Exit Sub
TextPrimaryStiffenersErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modDraw: Sub TextPrimaryStiffeners")
End Sub

Private Sub TextSecondaryStiffeners(ByVal Panel As cPanel)
    On Error GoTo TextSecondaryStiffenersErr
    If Panel.Visible = False Then Exit Sub
    Dim DY As Long, dZ As Long
    Dim Y_IN As Long, Z_IN As Long, Y_OUT As Long, Z_OUT As Long
    Dim angle As Double
    Dim iSide As Integer
    Dim dValue As String
    Y_IN = colNodes.Item(Panel.cGeometry.InNode).Y_Screen 'Panel.Z_IN_Screen
    Z_IN = colNodes.Item(Panel.cGeometry.InNode).Z_Screen 'Panel.Z_IN_Screen
    Y_OUT = colNodes.Item(Panel.cGeometry.OutNode).Y_Screen 'Panel.Z_OUT_Screen
    Z_OUT = colNodes.Item(Panel.cGeometry.OutNode).Z_Screen 'Panel.Z_OUT_Screen

    angle = Panel.cGeometry.PanelAngle
    Select Case Panel.cScantlings.cSecondaryStiffeners.Side
        Case SideLeft
            iSide = -1
        Case SideRight
            iSide = 1
        Case SideNone
            Exit Sub
    End Select
    size = Project.Item(ProjectIndex).cDisplaySettings.SizeSecondaryStiffeners
    Dim offset As Long
    offset = CurrentScale * size * iSide * Panel.cScantlings.cSecondaryStiffeners.WebHeight / 2
    DY = offset * Cos(PI * (angle + 90) / 180)
    dZ = offset * Sin(PI * (angle + 90) / 180)
    Dim lenTxt As Integer
    Select Case Project.Item(ProjectIndex).cDisplaySettings.TextSecondaryStiffeners
        Case None
            Exit Sub
        Case WebHeight
            dValue = CStr(Panel.cScantlings.cSecondaryStiffeners.WebHeight * 1000)
        Case WebThickness
            dValue = CStr(Panel.cScantlings.cSecondaryStiffeners.WebThickness * 1000)
        Case FlangeWidth
            dValue = CStr(Panel.cScantlings.cSecondaryStiffeners.FlangeWidth * 1000)
        Case FlangeThickness
            dValue = CStr(Panel.cScantlings.cSecondaryStiffeners.FlangeThickness * 1000)
        Case Spacing
            dValue = CStr(Panel.cScantlings.cSecondaryStiffeners.Spacing * 1000)
        Case All
    End Select
    lenTxt = Len(str(dValue))
    lrect.Top = dZ + (Z_IN + Z_OUT) / 2 - 8
    lrect.bottom = dZ + (Z_IN + Z_OUT) / 2 + 6
    lrect.Left = DY + (Y_IN + Y_OUT) / 2 - lenTxt * 3
    lrect.right = DY + (Y_IN + Y_OUT) / 2 + lenTxt * 3
    DrawTxt (dValue), vbWhite, vbBlack
    Exit Sub
TextSecondaryStiffenersErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modDraw: Sub TextSecondaryStiffeners")
End Sub

Private Sub DrawSecondaryFrames(ByVal Panel As cPanel)
    On Error GoTo DrawSecondaryFramesErr
    If Panel.Visible = False Then Exit Sub
    If Project.Item(ProjectIndex).cDisplaySettings.DrawSecondaryFrames = no Then Exit Sub
    size = Project.Item(ProjectIndex).cDisplaySettings.SizeSecondaryFrames
    Dim DY As Long, dZ As Long
    Dim y1 As Long, z1 As Long, y2 As Long, z2 As Long
    Dim y3 As Long, z3 As Long, y4 As Long, z4 As Long
    Dim WebHeight As Long
    Dim angle As Double
    WebHeight = size * CurrentScale * Panel.cScantlings.cSecondaryFrames.WebHeight
    angle = Panel.cGeometry.PanelAngle
    DY = WebHeight * (Cos(PI * (angle + 90) / 180))
    dZ = WebHeight * (Sin(PI * (angle + 90) / 180))
    y1 = colNodes.Item(Panel.cGeometry.InNode).Y_Screen 'Panel.Z_IN_Screen
    z1 = colNodes.Item(Panel.cGeometry.InNode).Z_Screen 'Panel.Z_IN_Screen
    y2 = colNodes.Item(Panel.cGeometry.OutNode).Y_Screen 'Panel.Z_OUT_Screen
    z2 = colNodes.Item(Panel.cGeometry.OutNode).Z_Screen 'Panel.Z_OUT_Screen

    Select Case Panel.cScantlings.cSecondaryFrames.Side
        Case SideLeft
            y3 = y2 - DY: z3 = z2 - dZ
            y4 = y1 - DY: z4 = z1 - dZ
        Case SideRight
            y3 = y2 + DY: z3 = z2 + dZ
            y4 = y1 + DY: z4 = z1 + dZ
        Case SideNone
            Exit Sub
    End Select
    Dim rgn As Long
    ReDim lp(5)
    lp(0).y = y1: lp(0).z = z1
    lp(1).y = y2: lp(1).z = z2
    lp(2).y = y3: lp(2).z = z3
    lp(3).y = y4: lp(3).z = z4
    lp(4).y = y1: lp(4).z = z1
    rgn = CreatePolygonRgn(lp(0), 5, 1)
    Dim hBrush As Long
    hBrush = Project.Item(ProjectIndex).cDisplaySettings.ColorSecondaryFrames
    hBrush = apiCreateSolidBrush(hBrush)
    FillRgn Pic, rgn, hBrush
    DrawPolyline 5, vbBlack
    DeleteObject rgn
    DeleteObject hBrush
    Exit Sub
DrawSecondaryFramesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modDraw: Sub DrawSecondaryFrames")
End Sub

Private Sub TextSecondaryFrames(ByVal Panel As cPanel)
    On Error GoTo TextSecondaryFramesErr
    If Panel.Visible = False Then Exit Sub
    Dim DY As Long, dZ As Long
    Dim Y_IN As Long, Z_IN As Long, Y_OUT As Long, Z_OUT As Long
    Dim angle As Double
    Dim iSide As Integer
    Dim dValue As String
    Y_IN = colNodes.Item(Panel.cGeometry.InNode).Y_Screen 'Panel.Z_IN_Screen
    Z_IN = colNodes.Item(Panel.cGeometry.InNode).Z_Screen 'Panel.Z_IN_Screen
    Y_OUT = colNodes.Item(Panel.cGeometry.OutNode).Y_Screen 'Panel.Z_OUT_Screen
    Z_OUT = colNodes.Item(Panel.cGeometry.OutNode).Z_Screen 'Panel.Z_OUT_Screen

    angle = Panel.cGeometry.PanelAngle
    Select Case Panel.cScantlings.cSecondaryFrames.Side
        Case SideLeft
            iSide = -1
        Case SideRight
            iSide = 1
        Case SideNone
            Exit Sub
    End Select
    size = Project.Item(ProjectIndex).cDisplaySettings.SizeSecondaryFrames
    Dim offset As Long
    offset = CurrentScale * size * iSide * Panel.cScantlings.cSecondaryFrames.WebHeight / 2
    DY = offset * Cos(PI * (angle + 90) / 180)
    dZ = offset * Sin(PI * (angle + 90) / 180)
    Dim lenTxt As Integer
    Select Case Project.Item(ProjectIndex).cDisplaySettings.TextSecondaryFrames
        Case None
            Exit Sub
        Case WebHeight
            dValue = CStr(Panel.cScantlings.cSecondaryFrames.WebHeight * 1000)
        Case WebThickness
            dValue = CStr(Panel.cScantlings.cSecondaryFrames.WebThickness * 1000)
        Case FlangeWidth
            dValue = CStr(Panel.cScantlings.cSecondaryFrames.FlangeWidth * 1000)
        Case FlangeThickness
            dValue = CStr(Panel.cScantlings.cSecondaryFrames.FlangeThickness * 1000)
        Case Spacing
            dValue = CStr(Panel.cScantlings.cSecondaryFrames.Spacing * 1000)
        Case All
    End Select
    If dValue = 0 Then Exit Sub
    lenTxt = Len(str(dValue))
    lrect.Top = dZ + (Z_IN + Z_OUT) / 2 - 8
    lrect.bottom = dZ + (Z_IN + Z_OUT) / 2 + 6
    lrect.Left = DY + (Y_IN + Y_OUT) / 2 - lenTxt * 3
    lrect.right = DY + (Y_IN + Y_OUT) / 2 + lenTxt * 3
    DrawTxt (dValue), Project.Item(ProjectIndex).cDisplaySettings.ColorSecondaryFrames, vbBlack
    Exit Sub
TextSecondaryFramesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modDraw: Sub TextSecondaryFrames")
End Sub

Private Sub DrawGirders(ByVal Panel As cPanel)
    On Error GoTo DrawGirdersErr
    If Panel.Visible = False Then Exit Sub
    If Project.Item(ProjectIndex).cDisplaySettings.DrawGirders = no Then Exit Sub
    size = Project.Item(ProjectIndex).cDisplaySettings.SizeGirders
    Dim i As Integer
    Dim Girder As cGirder
    Dim WebHeight As Long, FlangeWidth As Long, Distance As Long
    Dim hBrush As Long
    Dim y1 As Long, z1 As Long
    Dim y2 As Long, z2 As Long
    Dim y3 As Long, z3 As Long
    Dim y4 As Long, z4 As Long
    Dim Y_IN As Long, Z_IN As Long
    Dim angle As Double
    angle = Panel.cGeometry.PanelAngle
    Y_IN = colNodes.Item(Panel.cGeometry.InNode).Y_Screen 'Panel.Z_IN_Screen
    Z_IN = colNodes.Item(Panel.cGeometry.InNode).Z_Screen 'Panel.Z_IN_Screen

    hBrush = Project.Item(ProjectIndex).cDisplaySettings.ColorGirders
    For Each Girder In Panel.cScantlings.colGirder.col
        WebHeight = Girder.WebHeight * CurrentScale
        FlangeWidth = Girder.FlangeWidth * CurrentScale
        Distance = Girder.Distance * CurrentScale
        y1 = Y_IN + Distance * Cos(PI * angle / 180)
        z1 = Z_IN + Distance * Sin(PI * angle / 180)
        Select Case Panel.cScantlings.GirderSide
            Case SideLeft
                Call RotatePoint(size * WebHeight, 0, y2, z2, -90 + angle)
                Call RotatePoint(size * WebHeight, size * -FlangeWidth / 2, y3, z3, -90 + angle)
                Call RotatePoint(size * WebHeight, size * FlangeWidth / 2, y4, z4, -90 + angle)
            Case SideRight
                Call RotatePoint(size * WebHeight, 0, y2, z2, 90 + angle)
                Call RotatePoint(size * WebHeight, size * -FlangeWidth / 2, y3, z3, 90 + angle)
                Call RotatePoint(size * WebHeight, size * FlangeWidth / 2, y4, z4, 90 + angle)
            Case SideNone
                Exit Sub
        End Select
        y2 = y1 + y2: z2 = z1 + z2
        y3 = y1 + y3: z3 = z1 + z3
        y4 = y1 + y4: z4 = z1 + z4
        ReDim lp(1)
        lp(0).y = y1: lp(0).z = z1
        lp(1).y = y2: lp(1).z = z2
        DrawPolyline 2, hBrush
        lp(0).y = y3: lp(0).z = z3
        lp(1).y = y4: lp(1).z = z4
        DrawPolyline 2, hBrush
    Next Girder
    Exit Sub
DrawGirdersErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modDraw: Sub DrawGirders")
End Sub

Private Sub TextGirders(ByVal Panel As cPanel)
    On Error GoTo TextGirdersErr
    If Panel.Visible = False Then Exit Sub
    Dim Web_Height As Long, Distance As Long
    Dim Girder As cGirder
    Dim dValue As String, lenTxt As Integer
    Dim angle As Double
    Dim y1 As Long, z1 As Long
    Dim y2 As Long, z2 As Long
    Dim Y_IN As Long, Z_IN As Long
    
    Y_IN = colNodes.Item(Panel.cGeometry.InNode).Y_Screen 'Panel.Z_IN_Screen
    Z_IN = colNodes.Item(Panel.cGeometry.InNode).Z_Screen 'Panel.Z_IN_Screen

    angle = Panel.cGeometry.PanelAngle
    size = Project.Item(ProjectIndex).cDisplaySettings.SizeGirders
    For Each Girder In Panel.cScantlings.colGirder
        Web_Height = size * Girder.WebHeight * CurrentScale
        Distance = Girder.Distance * CurrentScale
        y1 = Y_IN + Distance * Cos(PI * angle / 180)
        z1 = Z_IN + Distance * Sin(PI * angle / 180)
        Select Case Panel.cScantlings.GirderSide
            Case SideLeft
                y2 = y1 + Web_Height * Cos(PI * (angle - 90) / 180)
                z2 = z1 + Web_Height * Sin(PI * (angle - 90) / 180)
            Case SideRight
                y2 = y1 + Web_Height * Cos(PI * (angle + 90) / 180)
                z2 = z1 + Web_Height * Sin(PI * (angle + 90) / 180)
            Case SideNone
                Resume Next
        End Select
        Select Case Project.Item(ProjectIndex).cDisplaySettings.TextGirders
            Case None
                Exit Sub
            Case WebHeight
                dValue = CStr(Girder.WebHeight * 1000)
            Case WebThickness
                dValue = CStr(Girder.WebThickness * 1000)
            Case FlangeWidth
                dValue = CStr(Girder.FlangeWidth * 1000)
            Case FlangeThickness
                dValue = CStr(Girder.FlangeThickness * 1000)
            Case DistFromInNode
                dValue = CStr(Girder.Distance)
            Case All
                dValue = CStr(Girder.WebHeight * 1000) & "x" & CStr(Girder.WebThickness * 1000) & vbCrLf & _
                        CStr(Girder.FlangeWidth * 1000) & "x" & CStr(Girder.FlangeThickness * 1000)
            Case GrossSectionModulus
                dValue = CStr(Round(Girder.GrossSectionModulus, 2))
        End Select
        lenTxt = Len(str(dValue))
        lrect.Top = z2 + 3
        lrect.bottom = z2 + 17
        lrect.Left = y2 - lenTxt * 3
        lrect.right = y2 + lenTxt * 3
        DrawTxt (dValue), vbWhite, vbBlack
    Next Girder
    Exit Sub
TextGirdersErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modDraw: Sub TextGirders")
End Sub

Private Sub DrawGrid()
'    On Error GoTo  DrawDrawGridErr
    On Error Resume Next
    If Project.Item(ProjectIndex).cDisplaySettings.DrawGrid = no Then Exit Sub
    size = Project.Item(ProjectIndex).cDisplaySettings.SizeGrid
    Dim i As Integer
    Dim NoOfHorizontalLines As Integer
    Dim NoOfVerticalLines As Integer
    Dim lScaleWidth As Long
    Dim lScaleHeight As Long
    Dim hBrush As Long
    hBrush = Project.Item(ProjectIndex).cDisplaySettings.ColorGrid
    lScaleWidth = Project.Item(ProjectIndex).frmProject.Width
    lScaleHeight = Project.Item(ProjectIndex).frmProject.Height
    NoOfHorizontalLines = Divide(lScaleWidth, CurrentScale) / size
    NoOfVerticalLines = Divide(lScaleHeight, CurrentScale) / size
    ReDim lp(1)
    For i = 1 To NoOfHorizontalLines
        lp(0).y = 0: lp(0).z = CurrentScale * i * size
        lp(1).y = lScaleWidth: lp(1).z = CurrentScale * i * size
        DrawPolyline 2, hBrush
    Next i
    For i = 1 To NoOfVerticalLines
        lp(0).y = CurrentScale * i * size: lp(0).z = 0
        lp(1).y = CurrentScale * i * size: lp(1).z = lScaleHeight
        DrawPolyline 2, hBrush
    Next i
    Exit Sub
DrawDrawGridErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modDraw: Sub DrawDrawGrid")
End Sub
'Drawing Functions
Private Sub DrawPolyline(ByVal nPoints As Long, ByVal cColor As ColorConstants)
    On Error GoTo DrawPolylineErr
    Project.Item(ProjectIndex).frmProject.picScreen.ForeColor = cColor
    Polyline Pic, lp(0), nPoints
    Exit Sub
DrawPolylineErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modDraw: Sub DrawPolyline")
End Sub

Private Sub DrawTxt(ByVal txt As String, cColorFill As ColorConstants, cColorChar As ColorConstants)
    On Error GoTo DrawTextErr
    Dim hBrush As Long
    hBrush = apiCreateSolidBrush(cColorFill)
    FillRect Pic, lrect, hBrush
    DeleteObject hBrush
    Project.Item(ProjectIndex).frmProject.picScreen.ForeColor = cColorChar
    Rectangle Pic, lrect.right, lrect.Top, lrect.Left, lrect.bottom
    DrawText Pic, txt, -1, lrect, 5
   
    Project.Item(ProjectIndex).frmProject.picScreen.ForeColor = vbBlack
    Exit Sub
DrawTextErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modDraw Sub DrawText")
End Sub

Private Sub DrawRectangle(ByVal Left As Long, ByVal Top As Long, ByVal Width As Long, ByVal Height As Long)
    On Error GoTo DrawRectangleErr
    Rectangle Pic, Left, Top, Width, Height
    Exit Sub
DrawRectangleErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modDraw: Sub DrawRectangle")
End Sub

Private Sub FillRectangle(ByVal lColor As Long)
    On Error GoTo FillRectangleErr
    Dim hBrush As Long
    hBrush = apiCreateSolidBrush(lColor)
    FillRect Pic, lrect, hBrush
    DeleteObject hBrush
    Exit Sub
FillRectangleErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modDraw: Sub FillRectangle")
End Sub

Public Sub FormRegions(ByVal Panel As cPanel, ByVal hBrush As ColorConstants)
    On Error GoTo FormRegionsErr
    DeleteObject Panel.Region
    Panel.Region = Empty
    Panel.Region = CreatePolygonRgn(lp(0), 5, 2)
    Exit Sub
FormRegionsErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modDraw: Sub FormRegions")
End Sub

Public Sub RotatePoint(x1 As Long, y1 As Long, XR As Long, yr As Long, angle As Double)
    On Error GoTo RotatePointErr
    XR = x1 * Cos(angle * PI / 180) - y1 * Sin(angle * PI / 180)
    yr = x1 * Sin(angle * PI / 180) + y1 * Cos(angle * PI / 180)
    Exit Sub
RotatePointErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modDraw: Sub RotatePoint")
End Sub

Public Function GetSupInteger(ByVal dData As Double) As Long
    On Error GoTo GetSupIntegerErr
    Dim dDecimal As Single
    dDecimal = Round(dData - Int(dData), 1)
    dData = dData - dDecimal
    Select Case dDecimal
        Case 0
            GetSupInteger = Int(dData)
        Case Is > 0
            GetSupInteger = Int(dData) + 1
        Case Is < 0
            GetSupInteger = Int(dData) - 1
    End Select
    Exit Function
GetSupIntegerErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modDraw: Function GetSupInteger")
End Function

Public Function Divide(numer, denom) As Variant
    Dim MSG As String
    Const mnErrDivByZero = 11, mnErrOverFlow = 6
    Const mnErrBadCall = 5
    On Error GoTo MathHandler
    Divide = numer / denom
    Exit Function
MathHandler:
    If Err.Number = mnErrDivByZero Or Err.Number = mnErrOverFlow _
                    Or Err.Number = mnErrBadCall Then
        Divide = 0
    Else
        MSG = "Unanticipated error " & Err.Number
        MSG = MSG & ": " & Err.Description
    End If
    Resume Next
End Function


