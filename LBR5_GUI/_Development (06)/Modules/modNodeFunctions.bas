Attribute VB_Name = "modNodeFunctions"
Option Explicit

'Public Function DeleteNode(ByVal iNodeNumber As Integer, colNodes As colNodes)
'    Dim Panel As cPanel, Node As cNode
'    For Each Panel In colPanel
'        If Panel.InNode = iNodeNumber Or Panel.OutNode = iNodeNumber Then
'            MsgBox "Node is linked to a panel and cannot be deleted.", vbCritical + vbOKOnly
'            Exit Function
'        End If
'    Next Panel
'    For Each Node In colNodes
'        If Node.nNumber = iNodeNumber Then
'            colNodes.Remove Node.Index
'            Exit For
'        End If
'    Next Node
'    RenumNodes colNodes
'    Set colPanel = Nothing
'End Function
'
'Public Function RenumNodes(ByVal colNodes As colNodes)
'    Dim Node As cNode
'    Dim i As Integer
'    Dim colNodes1 As New colNodes
'    i = 0
'    For Each Node In colNodes
'        i = i + 1
'        Node.Index = i
'        colNodes1.Add Node, i
'    Next Node
'    Set colNodes = colNodes1
'    Set colNodes1 = Nothing
'End Function

Public Sub NodeGenerator()
    On Error GoTo NodeGeneratorErr
    Dim NETO As Integer
    Dim Panel As cPanel
    Dim i As Integer
    Dim j As Integer
    Dim l As Integer
    NETO = Project.Item(ActiveProject).colPanel.Count
    For Each Panel In Project.Item(ActiveProject).colPanel
        Panel.cGeometry.PanelWidth = -Panel.cGeometry.PanelWidth
    Next Panel
    Dim x2arc As Double, y2arc As Double    'shell end
    Dim x_cen As Double, y_cen As Double    'shell center
    
    'Generate nodes
    Dim KK As Integer, LL As Integer
    Dim LI As Variant, Nc As Integer, NTN As Integer
    Dim ii As Integer, jj As Integer, k As Integer
    Dim j2 As Integer, N2 As Integer, KT As Integer
    Dim N6 As Integer, NDD As Integer, NAA As Integer
    Dim NN As Integer, x As Integer, NQ As Integer
    ReDim Node(NETO, 2) As Integer
    Dim cNode As New cNode
    Dim colNodes As colNodes
    Set colNodes = Project.Item(ActiveProject).colNodes
    If NETO <> 1 Then
          GoTo 51
    Else
          Nc = 0
          Node(1, 1) = 1
          Node(1, 2) = 2
          NTN = 2
          GoTo 801
    End If
51  KK = 1
110
    For i = 1 To NETO
        If i = NETO Then GoTo 135
        If Project.Item(ActiveProject).colPanel.Item(i).colConnections.Item(1) = 0 Then GoTo 121
        k = i + 1
        For l = k To NETO
            If Project.Item(ActiveProject).colPanel.Item(l).colConnections.Item(1) <> _
                Project.Item(ActiveProject).colPanel.Item(i).colConnections.Item(1) Then
                GoTo 130
            End If
            If (Node(l, 2) = 0) Then Node(l, 2) = KK
130         Next l
135         For LL = 1 To 10
            LI = Project.Item(ActiveProject).colPanel.Item(i).colConnections.Item(LL)
            If LI = 0 Then GoTo 121
            If (Node(LI, 1) = 0) Then Node(LI, 1) = KK
140         Next LL
121         If (Node(i, 2) <> 0) Then GoTo 120
        Node(i, 2) = KK
        KK = KK + 1
120 Next i
        For jj = 1 To NETO
            If (Node(jj, 1) <> 0) Then GoTo 150
            Node(jj, 1) = KK
            KK = KK + 1
150   Next jj
      NTN = KK - 1 ' Number of nodes
801
    
'generate coordinates
    ReDim NCH(NETO)
    ReDim PHILM(NETO) As Double
    ReDim PHILN(NETO) As Double
    ReDim z(NETO, 4)
    ReDim N1(NETO + 1)
    ReDim AONO(NETO + 1, 2) As Double
    ReDim tetas(NETO) As Double
    ReDim QN(NETO)
    For i = 1 To NETO
        NCH(i) = 0
        PHILN(i) = -PHILM(i)
        tetas(i) = Project.Item(ActiveProject).colPanel.Item(i).cGeometry.PanelAngle
        QN(i) = 1
        PHILN(i) = Project.Item(ActiveProject).colPanel.Item(i).cGeometry.PanelWidth
        For j = 1 To 4
            z(i, j) = 0#
2       Next j
    Next i
    For j2 = 1 To NTN
        N1(j2) = 0
    Next j2
    N2 = 1
    N1(N2) = 1
    For KT = 1 To NETO + 1 ' NETO+1 = Nbre theorique max de nodes
        If (KT = N2) Then GoTo 118
        N6 = KT + 1
        For jj = N6 To N2
            For KK = 1 To NETO
                If (NCH(KK) = 1) Then GoTo 500
                NDD = Node(KK, 1)
                NAA = Node(KK, 2)
                If N1(KT) = NDD And N1(jj) = NAA Then NCH(KK) = 1
                If N1(KT) = NAA And N1(jj) = NDD Then NCH(KK) = 1
            Next KK
        Next jj
500
118     NN = N1(KT)
        For j = 1 To NETO ' NETO = Nbre total de panneaux
            If NCH(j) = 1 Then GoTo 200
            NDD = Node(j, 1)
            NAA = Node(j, 2)
            x = 1#
            NQ = NAA
            If (NDD = NN) Then GoTo 190
            If (NAA = NN) Then GoTo 180
            GoTo 200
180         x = -1
            NQ = NDD
190
            AONO(NQ, 1) = AONO(NN, 1) - x * Project.Item(ActiveProject).colPanel.Item(j).cGeometry.PanelWidth * _
                        Cos(Project.Item(ActiveProject).colPanel.Item(j).cGeometry.PanelAngle * PI / 180)
            AONO(NQ, 2) = AONO(NN, 2) - x * Project.Item(ActiveProject).colPanel.Item(j).cGeometry.PanelWidth * _
                        Sin(Project.Item(ActiveProject).colPanel.Item(j).cGeometry.PanelAngle * PI / 180)
            N2 = N2 + 1
            N1(N2) = NQ
            NCH(j) = 1
200     Next j
    Next KT
    For j = 1 To NETO
        Project.Item(ActiveProject).colPanel.Item(j).cGeometry.InNode = Node(j, 1)
        Project.Item(ActiveProject).colPanel.Item(j).cGeometry.OutNode = Node(j, 2)
    Next j
'    For Each Panel In Project.Item(ActiveProject).colPanel
'        Panel.cGeometry.PanelWidth = -Panel.cGeometry.PanelWidth
'    Next Panel
    For i = 1 To NTN
        cNode.index = i
        cNode.nNumber = i
        cNode.y = Round(AONO(i, 1), 6)
        cNode.z = Round(AONO(i, 2), 6)
'        cNode.Y = AONO(i, 1)
'        cNode.Z = AONO(i, 2)
        colNodes.Add cNode, i
    Next i
        'Dim cNode As cNode
    
    For Each cNode In Project.Item(lProjectCount).colNodes
        'If Project.Item(ActiveProject).colPanel.Count > 1 Then
            cNode.y = cNode.y - Project.Item(lProjectCount).cHeader.YAxisOrigin
            cNode.z = cNode.z + Project.Item(lProjectCount).cHeader.ZAxisOrigin
        'Else
'            cNode.Y = cNode.Y - Project.Item(lProjectCount).cHeader.YAxisOrigin
'            cNode.Z = cNode.Z + Project.Item(lProjectCount).cHeader.ZAxisOrigin
        'End If
    Next cNode

    Dim y1 As Double, z1 As Double, y2 As Double, z2 As Double
    For Each Panel In Project.Item(ActiveProject).colPanel
        y1 = colNodes.Item(Panel.cGeometry.InNode).y
        z1 = colNodes.Item(Panel.cGeometry.InNode).z
        y2 = colNodes.Item(Panel.cGeometry.OutNode).y
        z2 = colNodes.Item(Panel.cGeometry.OutNode).z
        GetLengthAngle y1, z1, y2, z2, Panel
    Next Panel
    
    Exit Sub
NodeGeneratorErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modNodeGenerator: Sub NodeGenerator")
End Sub

Public Function GetDistBtnNodes(ByVal y As Integer, ByVal z As Integer)
    On Error GoTo GetDistBtnNodesErr
    Dim cNode As cNode
    Static iNode1 As Integer
    Static iNode2 As Integer
    Dim DY As Double, dZ As Double
    For Each cNode In Project.Item(ActiveProject).colNodes
        cNode.Selected = PtInRegion(cNode.Region, y, z)
        If cNode.Selected = isSelected Then
            If Project.Item(ActiveProject).FunctionMode = GET_DIST_FIRST_NODE_FUNCTION Then
                Project.Item(ActiveProject).LastNode = cNode.nNumber
                'Project.Item(ActiveProject).frmProject.Line1.Visible = True
                iNode1 = cNode.nNumber
                setFunctionMode GET_DIST_SECOND_NODE_FUNCTION
                cNode.Selected = IsUnselected
                Exit Function
            ElseIf Project.Item(ActiveProject).FunctionMode = GET_DIST_SECOND_NODE_FUNCTION Then
                iNode2 = cNode.nNumber
                setFunctionMode GET_DIST_FIRST_NODE_FUNCTION
                'setScreenMode NORMAL_MODE
                DY = Project.Item(ActiveProject).colNodes.Item(iNode2).y - Project.Item(ActiveProject).colNodes.Item(iNode1).y
                dZ = Project.Item(ActiveProject).colNodes.Item(iNode2).z - Project.Item(ActiveProject).colNodes.Item(iNode1).z
                MsgBox "Nodes " & iNode1 & " and " & iNode2 & ": " & vbCrLf & vbCrLf & _
                "Distance: " & Abs(Round(Sqr(DY * DY + dZ * dZ), 4)) & vbTab & " m" & vbCrLf & _
                "OY: " & vbTab & Abs(Round(DY, 4)) & vbTab & " m" & vbCrLf & _
                "OZ: " & vbTab & Abs(Round(dZ, 4)) & vbTab & " m"
                cNode.Selected = IsUnselected
                Project.Item(ActiveProject).frmProject.Line1.Visible = False
                
                Exit Function
            End If
            Exit Function
        End If
    Next cNode
    Exit Function
GetDistBtnNodesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modNodeFunctions: Function GetDistBtnNodes")
End Function

Public Function UpdateOrigin(ByVal ProjectIndex As Integer, Optional y As Double, Optional z As Double)
    On Error GoTo UpdateOriginErr
    If Project.Item(ProjectIndex).colPanel.Count = 0 Then
        Project.Item(ProjectIndex).cHeader.YAxisOrigin = y
        Project.Item(ProjectIndex).cHeader.ZAxisOrigin = z
    End If
    If Project.Item(ProjectIndex).colPanel.Count = 1 Then
        Project.Item(ProjectIndex).cHeader.YAxisOrigin = -Project.Item(ProjectIndex).colNodes.Item(Project.Item(ProjectIndex).colPanel.Item(1).cGeometry.InNode).y
        Project.Item(ProjectIndex).cHeader.ZAxisOrigin = Project.Item(ProjectIndex).colNodes.Item(Project.Item(ProjectIndex).colPanel.Item(1).cGeometry.InNode).z
    End If
    If Project.Item(ProjectIndex).colPanel.Count > 1 Then
        Project.Item(ProjectIndex).cHeader.YAxisOrigin = -Project.Item(ProjectIndex).colNodes.Item(Project.Item(ProjectIndex).colPanel.Item(1).cGeometry.OutNode).y
        Project.Item(ProjectIndex).cHeader.ZAxisOrigin = Project.Item(ProjectIndex).colNodes.Item(Project.Item(ProjectIndex).colPanel.Item(1).cGeometry.OutNode).z
    End If
    Exit Function
UpdateOriginErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmMoveOrigin: Function UpdateOrigin")
End Function

