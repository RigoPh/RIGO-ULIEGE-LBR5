Attribute VB_Name = "modCheckModel"
Option Explicit
Dim fs, F
Public sList() As String

Public Function AddToList(s As String)
    ReDim Preserve sList(0 To UBound(sList) + 1)
    sList(0) = UBound(sList) & " Problem(s) Found"
    sList(UBound(sList)) = s
End Function

Public Function CheckModel(ByVal index As Integer) As Boolean
    Dim sFile As String
    ReDim sList(0)
    sList(0) = "0 Problem(s) Found"
    UpdatePanelConnections index
'    sFile = GetFilePath(Project.Item(index).sFileName) & "Err_" & GetFileRoot(Project.Item(index).sFileName) & ".log"
'
'    Set fs = CreateObject("Scripting.FileSystemObject")
'    Set F = fs.OpenTextFile(sFile, ForWriting, TristateUseDefault)
    
    If CheckLoadCases(index) = True Then
        CheckModel = True
        'MsgBox Project.Item(index).sFileName & " cannot be saved. At least one load case must be defined.", vbCritical + vbOKOnly
        AddToList "No load case defined"
    End If
    If CheckNoOfPanels(index) = True Then
        CheckModel = True
        'MsgBox Project.Item(index).sFileName & " cannot be saved. No panels defined.", vbCritical + vbOKOnly
        AddToList "No panels defined"
    End If
    Dim panel_no As Integer
    If CheckContinuity(index, panel_no) = True Then
        CheckModel = True
        'MsgBox Project.Item(index).sFileName & " cannot be saved. Continuity test failed.", vbCritical + vbOKOnly
        AddToList "Continuity test failed" '. (check panel " & panel_no & ")"
    End If
    If CheckGravityLimits(index) = True Then
        CheckModel = True
    End If
    If CheckFourierTerms(index) = True Then
        CheckModel = True
    End If
    If CheckBendingMoments(index) = True Then
        CheckModel = True
    End If
    If CheckGirderABTR(index) = True Then
        CheckModel = True
    End If
    If CheckDesignVariablesLimits(index) = True Then
        CheckModel = True
    End If
    
    If CheckFlowRule(index) = True Then
        CheckModel = True
    End If
    '--------------
'    If CheckModel = True Then
'        MsgBox "The current model is not valid. A list of the detected problems will be generated.", vbCritical + vbOKOnly
'    End If
'    F.Close
'    Dim ShellFile As Long
'    If CheckModel = True Then
'        ShellFile = Shell(Windows_Path & "notepad.exe" & " " & sFile, vbNormalFocus)
'    Else
'        If fs.FileExists(sFile) Then fs.DeleteFile (sFile)
'    End If
    Project.Item(index).frmProject.StatusBar.Panels("Assistant").Text = sList(0)
    If Project.Item(index).frmModellingAssistant.Visible = True Then
        Project.Item(index).frmModellingAssistant.FillList
    End If
End Function

Public Function CheckDesignVariablesLimits(ByVal index As Integer) As Boolean
    Dim oPan As cPanel
    Dim var As Double, min As Double, max As Double
    For Each oPan In Project.Item(index).colPanel
        'plate thickness
        If oPan.colDesignVariables.Item(1).Active = True Then
            var = oPan.cScantlings.NetThickness
            min = oPan.colDesignVariables.Item(1).LowerLimit
            max = oPan.colDesignVariables.Item(1).UpperLimit
            If var < min Then
                CheckDesignVariablesLimits = True
                AddToList "Design Variables: " & "Panel " & oPan.index & _
                    "; " & "variable 'plate thickness' not compatible with the lower limit"
            End If
            If var > max Then
                CheckDesignVariablesLimits = True
                AddToList "Design Variables: " & "Panel " & oPan.index & _
                    "; " & "variable 'plate thickness' not compatible with the upper limit"
            End If
        End If
        'frames web height
        If oPan.colDesignVariables.Item(2).Active = True Then
            var = oPan.cScantlings.cPrimaryFrames.WebHeight
            min = oPan.colDesignVariables.Item(2).LowerLimit
            max = oPan.colDesignVariables.Item(2).UpperLimit
            If var < min Then
                CheckDesignVariablesLimits = True
                AddToList "Design Variables: " & "Panel " & oPan.index & _
                    "; " & "variable 'Frame Web Height' not compatible with the lower limit"
            End If
            If var > max Then
                CheckDesignVariablesLimits = True
                AddToList "Design Variables: " & "Panel " & oPan.index & _
                    "; " & "variable 'Frame Web Height' not compatible with the upper limit"
            End If
        End If
        'frames web thickness
        If oPan.colDesignVariables.Item(3).Active = True Then
            var = oPan.cScantlings.cPrimaryFrames.WebThickness
            min = oPan.colDesignVariables.Item(3).LowerLimit
            max = oPan.colDesignVariables.Item(3).UpperLimit
            If var < min Then
                CheckDesignVariablesLimits = True
                AddToList "Design Variables: " & "Panel " & oPan.index & _
                    "; " & "variable 'Frame Web Thickness' not compatible with the lower limit"
            End If
            If var > max Then
                CheckDesignVariablesLimits = True
                AddToList "Design Variables: " & "Panel " & oPan.index & _
                    "; " & "variable 'Frame Web Thickness' not compatible with the upper limit"
            End If
        End If
        'frames flange width
        If oPan.colDesignVariables.Item(4).Active = True Then
            var = oPan.cScantlings.cPrimaryFrames.FlangeWidth
            min = oPan.colDesignVariables.Item(4).LowerLimit
            max = oPan.colDesignVariables.Item(4).UpperLimit
            If var < min Then
                CheckDesignVariablesLimits = True
                AddToList "Design Variables: " & "Panel " & oPan.index & _
                    "; " & "variable 'Frame Flange Width' not compatible with the lower limit"
            End If
            If var > max Then
                CheckDesignVariablesLimits = True
                AddToList "Design Variables: " & "Panel " & oPan.index & _
                    "; " & "variable 'Frame Flange Width' not compatible with the upper limit"
            End If
        End If
        'frames (beams) spacing
        Dim s As String
        Select Case oPan.pType
            Case Plate
                s = "Frame Spacing"
            Case Beam
                s = "Beam Spacing"
        End Select
        If oPan.colDesignVariables.Item(5).Active = True Then
            var = oPan.cScantlings.cPrimaryFrames.Spacing
            min = oPan.colDesignVariables.Item(5).LowerLimit
            max = oPan.colDesignVariables.Item(5).UpperLimit
            If var < min Then
                CheckDesignVariablesLimits = True
                AddToList "Design Variables: " & "Panel " & oPan.index & _
                    "; " & "variable '" & s & "' not compatible with the lower limit"
            End If
            If var > max Then
                CheckDesignVariablesLimits = True
                AddToList "Design Variables: " & "Panel " & oPan.index & _
                    "; " & "variable '" & s & "' not compatible with the upper limit"
            End If
        End If
        
        'stiffener web height
        If oPan.colDesignVariables.Item(6).Active = True Then
            var = oPan.cScantlings.cPrimaryStiffeners.WebHeight
            min = oPan.colDesignVariables.Item(6).LowerLimit
            max = oPan.colDesignVariables.Item(6).UpperLimit
            If var < min Then
                CheckDesignVariablesLimits = True
                AddToList "Design Variables: " & "Panel " & oPan.index & _
                    "; " & "variable 'Stiffener Web Height' not compatible with the lower limit"
            End If
            If var > max Then
                CheckDesignVariablesLimits = True
                AddToList "Design Variables: " & "Panel " & oPan.index & _
                    "; " & "variable 'Stiffener Web Height' not compatible with the upper limit"
            End If
        End If
        'frames web thickness
        If oPan.colDesignVariables.Item(7).Active = True Then
            var = oPan.cScantlings.cPrimaryStiffeners.WebThickness
            min = oPan.colDesignVariables.Item(7).LowerLimit
            max = oPan.colDesignVariables.Item(7).UpperLimit
            If var < min Then
                CheckDesignVariablesLimits = True
                AddToList "Design Variables: " & "Panel " & oPan.index & _
                    "; " & "variable 'Stiffener Web Thickness' not compatible with the lower limit"
            End If
            If var > max Then
                CheckDesignVariablesLimits = True
                AddToList "Design Variables: " & "Panel " & oPan.index & _
                    "; " & "variable 'Stiffener Web Thickness' not compatible with the upper limit"
            End If
        End If
        'frames flange width
        If oPan.colDesignVariables.Item(8).Active = True Then
            var = oPan.cScantlings.cPrimaryStiffeners.FlangeWidth
            min = oPan.colDesignVariables.Item(8).LowerLimit
            max = oPan.colDesignVariables.Item(8).UpperLimit
            If var < min Then
                CheckDesignVariablesLimits = True
                AddToList "Design Variables: " & "Panel " & oPan.index & _
                    "; " & "variable 'Stiffener Flange Width' not compatible with the lower limit"
            End If
            If var > max Then
                CheckDesignVariablesLimits = True
                AddToList "Design Variables: " & "Panel " & oPan.index & _
                    "; " & "variable 'Stiffener Flange Width' not compatible with the upper limit"
            End If
        End If
        'frames (beams) spacing
        If oPan.colDesignVariables.Item(9).Active = True Then
            var = oPan.cScantlings.cPrimaryStiffeners.Spacing
            min = oPan.colDesignVariables.Item(9).LowerLimit
            max = oPan.colDesignVariables.Item(9).UpperLimit
            If var < min Then
                CheckDesignVariablesLimits = True
                AddToList "Design Variables: " & "Panel " & oPan.index & _
                    "; " & "variable 'Stiffener Spacing' not compatible with the lower limit"
            End If
            If var > max Then
                CheckDesignVariablesLimits = True
                AddToList "Design Variables: " & "Panel " & oPan.index & _
                    "; " & "variable 'Stiffener Spacing' not compatible with the upper limit"
            End If
        End If
    Next oPan
End Function

Public Function CheckGirderABTR(ByVal index As Integer) As Boolean
    Dim cPanel As cPanel, cGirder As cGirder
    For Each cPanel In Project.Item(index).colPanel
        For Each cGirder In cPanel.cScantlings.colGirder
            If cGirder.Distance > cPanel.cGeometry.PanelWidth Then
                CheckGirderABTR = True
                AddToList "Girder " & cGirder.index & " on panel " & cPanel.pNumber & " is out of panel's limits"
            End If
        Next cGirder
    Next cPanel
End Function

Public Function CheckNoOfPanels(ByVal index As Integer) As Boolean
    If Project.Item(index).colPanel.Count = 0 Then CheckNoOfPanels = True
End Function

Public Function CheckLoadCases(ByVal index As Integer) As Boolean
    If Project.Item(index).cHeader.colLoadCase.Count = 0 Then CheckLoadCases = True
End Function

Public Function CheckContinuity(ByVal index As Integer, Optional panel_no As Integer) As Boolean
    Dim i As Integer, j As Integer, k As Integer
    Dim Panel As cPanel
    Dim colPanels As colPanel
    Set colPanels = Project.Item(index).colPanel
    If colPanels.Count <= 1 Then
        CheckContinuity = False
        Exit Function
    End If
    Dim bFlag() As Boolean, bFlag_old() As Boolean
    ReDim bFlag(1 To colPanels.Count)
    ReDim bFlag_old(1 To colPanels.Count)
    bFlag(1) = True
    Dim iter As Integer
    Const iter_max As Integer = 50
    Do
        iter = iter + 1
        bFlag_old = bFlag
        For Each Panel In colPanels
            If bFlag(Panel.pNumber) = True Then
                'Outgoing panels
                For j = 1 To 10
                    If Panel.colConnections.Item(j) <> 0 Then
                        bFlag(Panel.colConnections.Item(j)) = True
                    End If
                Next j
                'Entering panels
                For j = 1 To colPanels.Count
                    If Panel.pNumber <> j And bFlag(j) = False Then
                        For k = 1 To 10
                            If colPanels.Item(j).colConnections.Item(k) <> 0 And _
                                colPanels.Item(j).colConnections.Item(k) = Panel.pNumber Then
                                bFlag(j) = True
                            End If
                        Next k
                    End If
                Next j
            End If
        Next Panel
        CheckContinuity = False
        For Each Panel In colPanels
            If bFlag(Panel.pNumber) <> bFlag_old(Panel.pNumber) Then
                CheckContinuity = True
                Exit For
            End If
        Next Panel
        If CheckContinuity = False Then Exit Do
    Loop
    CheckContinuity = False
    For Each Panel In colPanels
        If bFlag(Panel.pNumber) = False Or iter = iter_max Then
            CheckContinuity = True
            panel_no = Panel.pNumber
            Exit For
        End If
    Next Panel
    
'    Dim Node As cNode, colNodes As colNodes
'    'Dim Panel As cPanel, colPanels As colPanel
'    Set colNodes = Project.Item(index).colNodes
'    'Set colPanels = Project.Item(index).colPanel
'    Dim InNode As Integer, OutNode As Integer
'    For Each Node In colNodes
'        InNode = 0
'        OutNode = 0
'        For Each Panel In colPanels
'            If Panel.cGeometry.InNode = Node.nNumber Then
'                InNode = InNode + 1
'                'Resume Next
'            End If
'            If Panel.cGeometry.OutNode = Node.nNumber Then
'                OutNode = OutNode + 1
'            End If
'        Next Panel
'        If InNode > 1 And OutNode = 0 Then
'            CheckContinuity = True
'            Exit Function
'        End If
'        If OutNode > 1 And InNode = 0 Then
'            CheckContinuity = True
'            Exit Function
'        End If
'    Next Node
'    Set colNodes = Nothing
'    Set colPanels = Nothing

    
    'Set colPanels = Nothing
End Function

Public Function CheckFlowRule(ByVal index As Integer) As Boolean
    Dim Node As cNode, colNodes As colNodes
    Dim Panel As cPanel, colPanels As colPanel
    Set colNodes = Project.Item(index).colNodes
    Set colPanels = Project.Item(index).colPanel
    Dim InNode As Integer, OutNode As Integer
    Dim snodes As String
    
    For Each Node In colNodes
        InNode = 0
        OutNode = 0
        For Each Panel In colPanels
            If Panel.cGeometry.InNode = Node.nNumber Then
                InNode = InNode + 1
                'Resume Next
                
            End If
            If Panel.cGeometry.OutNode = Node.nNumber Then
                OutNode = OutNode + 1
            End If
        Next Panel
        If InNode > 1 And OutNode = 0 Then
            CheckFlowRule = True
            snodes = snodes & Node.index & " "
            'Exit Function
        End If
        If OutNode > 1 And InNode = 0 Then
            CheckFlowRule = True
            snodes = snodes & Node.index & " "
            'Exit Function
        End If
    Next Node
    If CheckFlowRule = True Then
        AddToList "Flow Rule test failed in node(s):" & snodes
    End If
    Set colNodes = Nothing
    Set colPanels = Nothing
End Function

Public Function CheckGravityLimits(ByVal ProjectIndex As Integer) As Boolean
    On Error GoTo CheckGravityLimitsErr
    Dim index As Integer
    Dim dLowerLimit As Double, dUpperLimit As Double
    Dim ZMin As Double, ZMax As Double
    ZMin = -Project.Item(ProjectIndex).ZMax
    ZMax = -Project.Item(ProjectIndex).ZMin
    index = Project.Item(ProjectIndex).cHeader.cGlobalConstraints.GravityLimitRestriction
    dLowerLimit = Project.Item(ProjectIndex).cHeader.cGlobalConstraints.MinGravityCenter
    dUpperLimit = Project.Item(ProjectIndex).cHeader.cGlobalConstraints.MaxGravityCenter
    Select Case index
        Case 0
        Case 1
            If dLowerLimit < ZMin Or dLowerLimit > ZMax Then
                'MsgBox "Gravity Center limits exceed model limits.", vbCritical + vbOKOnly
                AddToList "Gravity Center limits exceed model limits"
                CheckGravityLimits = True
                If CheckGravityLimits = True Then Exit Function
            End If
        Case 2
            If dUpperLimit < ZMin Or dUpperLimit > ZMax Then
                'MsgBox "Gravity Center limits exceed model limits.", vbCritical + vbOKOnly
                AddToList "Gravity Center limits exceed model limits"
                CheckGravityLimits = True
                If CheckGravityLimits = True Then Exit Function
            End If
        Case 3
            If dLowerLimit < ZMin Or dLowerLimit > ZMax Then
                'MsgBox "Gravity Center limits exceed model limits.", vbCritical + vbOKOnly
                AddToList "Gravity Center limits exceed model limits"
                CheckGravityLimits = True
                If CheckGravityLimits = True Then Exit Function
            End If
            If dUpperLimit < ZMin Or dUpperLimit > ZMax Then
                'MsgBox "Gravity Center limits exceed model limits.", vbCritical + vbOKOnly
                AddToList "Gravity Center limits exceed model limits"
                CheckGravityLimits = True
                If CheckGravityLimits = True Then Exit Function
            End If
            If dLowerLimit > dUpperLimit Then
                'MsgBox "Gravity Center upper limit is below the lower limit.", vbCritical + vbOKOnly
                AddToList "Gravity Center upper limit is below the lower limit"
                CheckGravityLimits = True
                If CheckGravityLimits = True Then Exit Function
            End If
    End Select
    Exit Function
CheckGravityLimitsErr:
   Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCheckModel: Function CheckGravityLimits")
End Function

Public Function CheckFourierTerms(ByVal ProjectIndex As Integer) As Boolean
    On Error GoTo CheckFourierTermsErr
    Dim cLoadCase As cLoadCase
    'Check Moments
'    Dim bInegalMoments As Boolean
'    bInegalMoments = False
'    For Each cLoadCase In Project.Item(ProjectIndex).cHeader.colLoadCase
'        If cLoadCase.VerticalBendingMomentFore <> cLoadCase.VerticalBendingMomentAft Then
'            bInegalMoments = True
'        End If
'        If cLoadCase.HorizontalBendingMomentFore <> cLoadCase.HorizontalBendingMomentAft Then
'            bInegalMoments = True
'        End If
'    Next cLoadCase
'    If bInegalMoments = True Then
'        If Project.Item(ProjectIndex).cHeader.JLPH < 5 Then
'            addtolist "When the bending moments in X = 0 and X = L are different, " & vbCrLf & _
'            "   the number of terms in Fourier series must be pozitive and greater than 5"
'            CheckFourierTerms = True
'        End If
'    End If
    'Check Stepwise Lateral Pressures
    Dim iNoofSteps As Integer
    Dim cPanel As cPanel
    Dim i As Integer
    Dim sMsg As String
    
    Dim bNonSimetricPressures As Boolean
    bNonSimetricPressures = False
    If Project.Item(ProjectIndex).cHeader.JLPH < 0 Then
        For Each cPanel In Project.Item(ProjectIndex).colPanel
            For Each cLoadCase In cPanel.colLoadCase
                iNoofSteps = cLoadCase.colStepWiseLateralPressure.Count
                If iNoofSteps = 0 Then GoTo NextLoadCase
                For i = 1 To CInt(iNoofSteps / 2)
                    If cLoadCase.colStepWiseLateralPressure.Item(i).LateralPressureIn <> cLoadCase.colStepWiseLateralPressure.Item(iNoofSteps - i + 1).LateralPressureIn Then
                        bNonSimetricPressures = True
                    End If
                    If cLoadCase.colStepWiseLateralPressure.Item(i).LateralPressureOut <> cLoadCase.colStepWiseLateralPressure.Item(iNoofSteps - i + 1).LateralPressureOut Then
                        bNonSimetricPressures = True
                    End If
                    If cLoadCase.colStepWiseLateralPressure.Item(i).VerticalGravityLoad <> cLoadCase.colStepWiseLateralPressure.Item(iNoofSteps - i + 1).VerticalGravityLoad Then
                        bNonSimetricPressures = True
                    End If
                Next i
                If bNonSimetricPressures = True Then
                    AddToList "Panel " & cPanel.index & ", Load Case " & "'" & cLoadCase.Title & "'" & "; " & "Non symmetric stepwise lateral pressures -> the number of terms in the Fourier series must be pozitive"
                    CheckFourierTerms = True
                End If
NextLoadCase:
            bNonSimetricPressures = False
            Next cLoadCase
        Next cPanel
    End If
    If bNonSimetricPressures = True Then
        CheckFourierTerms = True
    End If
    Exit Function
CheckFourierTermsErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCheckModel: Function CheckFourierTerms")
End Function

Public Function CheckBendingMoments(ByVal ProjectIndex As Integer) As Boolean
    On Error GoTo CheckBendingMomentsErr
    Dim cLoadCase As cLoadCase
    Dim bParticularCase As Boolean
    'Particular case: 1 panel model
    If Project.Item(ProjectIndex).colPanel.Count = 1 Then
        For Each cLoadCase In Project.Item(ProjectIndex).cHeader.colLoadCase
            If cLoadCase.HorizontalBendingMomentFore <> 0 Then
                bParticularCase = True
            End If
            If cLoadCase.HorizontalBendingMomentAft <> 0 Then
                bParticularCase = True
            End If
            If Round(cLoadCase.VerticalBendingMomentFore, 6) <> 0 Then
                bParticularCase = True
            End If
            If Round(cLoadCase.VerticalBendingMomentAft, 6) <> 0 Then
                bParticularCase = True
            End If
            If bParticularCase = True Then
                CheckBendingMoments = True
                AddToList "Bending moments cannot be assigned to one panel models"
            End If
            Exit For
        Next cLoadCase
    End If
    'Horizontal bending moments in symmetric models
    Dim bSymmetry As Boolean
    Dim cPanel As cPanel, cBoundary As cBoundaryConditions
    For Each cPanel In Project.Item(ProjectIndex).colPanel
        For Each cBoundary In cPanel.colBoundaryConditions
        Select Case cBoundary.BoundaryCondition
            Case 7, 11, 12
                bSymmetry = True
                GoTo ExitLoop
        End Select
        Next cBoundary
    Next cPanel
ExitLoop:
    Dim bHzMoments As Boolean
    bHzMoments = False
    If bSymmetry = True Then
        For Each cLoadCase In Project.Item(ProjectIndex).cHeader.colLoadCase
            If cLoadCase.HorizontalBendingMomentAft <> 0 Then
                bHzMoments = True
            End If
            If cLoadCase.HorizontalBendingMomentFore <> 0 Then
                bHzMoments = True
            End If
        Next cLoadCase
    End If
    If bHzMoments = True Then
        CheckBendingMoments = True
        AddToList "Horizontal bending moments cannot be assigned to a half section model"
    End If
    Exit Function
CheckBendingMomentsErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCheckModel: Function CheckBendingMoments")
End Function
