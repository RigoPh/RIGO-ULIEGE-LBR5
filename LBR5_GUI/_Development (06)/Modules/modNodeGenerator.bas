Attribute VB_Name = "modNodeGenerator"
Option Base 1

Public Sub NodeGenerator()
    On Error GoTo NodeGeneratorErr
    Dim NETO As Integer
    Dim Panel As cPanel
    Dim i As Integer
    Dim j As Integer
    Dim l As Integer
    NETO = Project.Item(ActiveProject).colPanel.Count
    For Each Panel In Project.Item(ActiveProject).colPanel.Col
        Panel.pWidth = -Panel.pWidth
    Next Panel
    Dim x2arc As Double, y2arc As Double    'shell end
    Dim x_cen As Double, y_cen As Double    'shell center
    '===========================================================
    'if SHELLS, nodes will be computed
    'considerring the shell equivalent to a plate having HIGHT equal
    'to the the chord length;
    'the angle has to be transformed into the chord angle
    'at the end of this subroutine, the angles have to be revesed
    'to the original value
    
    ' CODE FOR SHELL PANEL
    'For i = 1 To Project.Item(ActiveProject).colPanel.Count
    '        If Panel(i).PanelType = "SHELL" Then
    '            Call nod2_arc(0, 0, x2arc, y2arc, x_cen, y_cen, Panel(i).Angle, Panel(i).PHIL, Panel(i).Q)
    '            Panel(i).HIGHT = Sqr(x2arc * x2arc + y2arc * y2arc)
    '            Panel(i).HIGHT = -Panel(i).HIGHT
    '        End If
    '        If Panel(i).PanelType = "SHELL" And Panel(i).PHIL <= 0 Then
    '            'tetas(i) = Panel(i).Angle - (Abs(Panel(i).PHIL) / 2)
    '            Panel(i).Angle = Panel(i).Angle - (Abs(Panel(i).PHIL) / 2)
    '        End If
    '        If Panel(i).PanelType = "SHELL" And Panel(i).PHIL > 0 Then
    '            'tetas(i) = Panel(i).Angle + (Abs(Panel(i).PHIL) / 2)
    '            Panel(i).Angle = Panel(i).Angle + (Abs(Panel(i).PHIL) / 2)
    '        End If
    'Next i
    
    'Generate nodes
    Dim KK As Integer
    Dim LI As Variant
    ReDim Node(NETO, 2) As Integer
    If NETO <> 1 Then
          GoTo 51
    Else
          NC = 0
          Node(1, 1) = 1
          Node(1, 2) = 2
          NTN = 2
          GoTo 801
    End If
51  KK = 1
    For ii = 1 To NETO
110 Next ii
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
      NTN = KK - 1
801
    
'generate coordinates
    ReDim NCH(NETO)
    ReDim PHILM(NETO) As Double
    ReDim PHILN(NETO) As Double
    ReDim z(NETO, 4)
    ReDim n1(NETO + 1)
    ReDim AONO(NETO + 1, 2) As Double
    ReDim tetas(NETO) As Double
    ReDim QN(NETO)
    For i = 1 To NETO
        NCH(i) = 0
        PHILN(i) = -PHILM(i)
        tetas(i) = Project.Item(ActiveProject).colPanel.Item(i).Angle
        QN(i) = 1
        PHILN(i) = Project.Item(ActiveProject).colPanel.Item(i).pWidth
        For j = 1 To 4
            z(i, j) = 0#
2       Next j
    Next i
    For j2 = 1 To NTN
        n1(j2) = 0
    Next j2
    N2 = 1
    n1(N2) = 1
    For KT = 1 To NETO + 1 ' NETO+1 = Nbre theorique max de nodes
        If (KT = N2) Then GoTo 118
        N6 = KT + 1
        For jj = N6 To N2
            For KK = 1 To NETO
                If (NCH(KK) = 1) Then GoTo 500
                NDD = Node(KK, 1)
                NAA = Node(KK, 2)
                If n1(KT) = NDD And n1(jj) = NAA Then NCH(KK) = 1
                If n1(KT) = NAA And n1(jj) = NDD Then NCH(KK) = 1
            Next KK
        Next jj
500
118     NN = n1(KT)
        For j = 1 To NETO ' NETO = Nbre total de panneaux
            If NCH(j) = 1 Then GoTo 200
            NDD = Node(j, 1)
            NAA = Node(j, 2)
            X = 1#
            NQ = NAA
            If (NDD = NN) Then GoTo 190
            If (NAA = NN) Then GoTo 180
            GoTo 200
180         X = -1#
            NQ = NDD
190         'SPH2 = VSIN(PHILN(j) / 2#, 0#)
            'S2 = VSIN(tetas(j), PHILN(j) / 2#)
            'C2 = VCOS(tetas(j), PHILN(j) / 2#)
            AONO(NQ, 1) = AONO(NN, 1) - X * Project.Item(ActiveProject).colPanel.Item(j).pWidth * _
                        Cos(Project.Item(ActiveProject).colPanel.Item(j).Angle * PI / 180)
            AONO(NQ, 2) = AONO(NN, 2) - X * Project.Item(ActiveProject).colPanel.Item(j).pWidth * _
                        Sin(Project.Item(ActiveProject).colPanel.Item(j).Angle * PI / 180)
            N2 = N2 + 1
            n1(N2) = NQ
            NCH(j) = 1
200     Next j
    Next KT
    For j = 1 To NETO
        Project.Item(ActiveProject).colPanel.Item(j).X_IN = AONO(Node(j, 1), 1)
        Project.Item(ActiveProject).colPanel.Item(j).Y_IN = AONO(Node(j, 1), 2)
        Project.Item(ActiveProject).colPanel.Item(j).X_OUT = AONO(Node(j, 2), 1)
        Project.Item(ActiveProject).colPanel.Item(j).Y_OUT = AONO(Node(j, 2), 2)
    Next j
    For Each Panel In Project.Item(ActiveProject).colPanel.Col
        Panel.pWidth = -Panel.pWidth
    Next Panel

    'put back the original angles values for shells
    'For i = 1 To header.NETO
    '        If Panel(i).PanelType = "SHELL" And Panel(i).PHIL <= 0 Then
    '            'tetas(i) = Panel(i).Angle - (Abs(Panel(i).PHIL) / 2)
    '            Panel(i).Angle = Panel(i).Angle + (Abs(Panel(i).PHIL) / 2)
    '        End If
    '        If Panel(i).PanelType = "SHELL" And Panel(i).PHIL > 0 Then
    '            'tetas(i) = Panel(i).Angle + (Abs(Panel(i).PHIL) / 2)
    '            Panel(i).Angle = Panel(i).Angle - (Abs(Panel(i).PHIL) / 2)
    '        End If
    'Next i
    Exit Sub
NodeGeneratorErr:
    Call RaiseError(MyUnhandledError, "modNodeGenerator: Sub NodeGenerator")
End Sub

Public Function VSIN(A, b)
      X = A
      Y = b
      If (Abs(Y) <= 0.5) Then GoTo 1
      X = X + Y
      Y = 0#
1
3     If (X >= 0#) Then GoTo 2
      X = X + 360#
      GoTo 3
2
      NN = 1
      n = 0
5     If (X < 180#) Then GoTo 4
      X = X - 180#
      n = n + 1
      GoTo 5
4     If (n <> 0) Then NN = (-1) ^ n
      If ((X + Y) < 90#) Then GoTo 6
      X = 180# - X
      Y = -Y
6     VSIN = NN * Sin((X + Y) * PI / 180#)
      If ((X <= 90#) And (X > 89.99)) Then VSIN = NN * Cos(Y * PI / 180#)
End Function

Public Function VCOS(A, b)
      X = A
      Y = b
      If (Abs(Y) <= 0.5) Then GoTo 1
      X = X + Y
      Y = 0#
1
3     If (X >= 0#) Then GoTo 2
      X = X + 360#
      GoTo 3
2
      NN = 1
      n = 0
5     If (X < 180#) Then GoTo 4
      X = X - 180#
      n = n + 1
      GoTo 5
4     If (n <> 0) Then NN = (-1) ^ n
      If ((X + Y) >= 90#) Then
      X = 180# - X
      Y = -Y
      VCOS = -NN * Cos((X + Y) * PI / 180#)
      Else:
      VCOS = NN * Cos((X + Y) * PI / 180#)
      End If
      If ((X <= 90#) And (X > 89.99)) Then VCOS = NN * Sin(Y * PI / 180#)
End Function
