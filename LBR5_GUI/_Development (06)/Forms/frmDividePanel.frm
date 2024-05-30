VERSION 5.00
Object = "{0D452EE1-E08F-101A-852E-02608C4D0BB4}#2.0#0"; "FM20.DLL"
Begin VB.Form frmDividePanel 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Divide Panel"
   ClientHeight    =   1470
   ClientLeft      =   2430
   ClientTop       =   2175
   ClientWidth     =   3360
   Icon            =   "frmDividePanel.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   1470
   ScaleWidth      =   3360
   ShowInTaskbar   =   0   'False
   Begin VB.TextBox txtFirstWidth 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   1560
      TabIndex        =   0
      Top             =   480
      Width           =   975
   End
   Begin VB.Label lblUnit 
      AutoSize        =   -1  'True
      Caption         =   "[m]"
      Height          =   195
      Left            =   2640
      TabIndex        =   5
      Top             =   480
      Width           =   210
   End
   Begin VB.Label lblFirstWidth 
      AutoSize        =   -1  'True
      Caption         =   "Enter First Width: "
      Height          =   195
      Left            =   240
      TabIndex        =   4
      Top             =   480
      Width           =   1260
   End
   Begin VB.Label lblPanelToDivide 
      AutoSize        =   -1  'True
      Caption         =   "Panel To Divide"
      Height          =   195
      Left            =   240
      TabIndex        =   3
      Top             =   120
      Width           =   1140
   End
   Begin MSForms.CommandButton cmdCancel 
      Cancel          =   -1  'True
      Height          =   375
      Left            =   2160
      TabIndex        =   2
      Top             =   960
      Width           =   1095
      Caption         =   "Cancel"
      PicturePosition =   327683
      Size            =   "1931;661"
      FontHeight      =   165
      FontCharSet     =   0
      FontPitchAndFamily=   2
      ParagraphAlign  =   3
   End
   Begin MSForms.CommandButton cmdOK 
      Default         =   -1  'True
      Height          =   375
      Left            =   960
      TabIndex        =   1
      Top             =   960
      Width           =   1095
      Caption         =   "OK"
      PicturePosition =   327683
      Size            =   "1931;661"
      FontHeight      =   165
      FontCharSet     =   0
      FontPitchAndFamily=   2
      ParagraphAlign  =   3
   End
End
Attribute VB_Name = "frmDividePanel"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Dim ProjectIndex As Integer
Dim cPanel As cPanel
Dim cNewPanel As cPanel
Dim cNode As cNode
Dim dFirstWidth As Double
Dim iNoOfGirders As Integer
Dim tmpNode As Integer

Private Sub cmdCancel_Click()
    Unload Me
End Sub

Private Sub cmdOK_Click()
    dFirstWidth = CDbl(txtFirstWidth.Text)
    DivideSelectedPanel
'    If cPanel.pType = DoubleHull Then
'        Set cPanel = Project.Item(ProjectIndex).colPanel.Item(cPanel.RelatedDoubleHullPanel)
'        DivideSelectedPanel
'        Project.Item(ProjectIndex).colPanel.Item(Project.Item(ProjectIndex).colPanel.Count - 1).RelatedDoubleHullPanel = Project.Item(ProjectIndex).colPanel.Item(Project.Item(ProjectIndex).colPanel.Count).RelatedDoubleHullPanel
'        Project.Item(ProjectIndex).colPanel.Item(Project.Item(ProjectIndex).colPanel.Count).RelatedDoubleHullPanel = Project.Item(ProjectIndex).colPanel.Item(Project.Item(ProjectIndex).colPanel.Count - 1).RelatedDoubleHullPanel
'        CreateDoubleHull Project.Item(ProjectIndex).colPanel.Item(Project.Item(ProjectIndex).colPanel.Count - 1).pNumber, Project.Item(ProjectIndex).colPanel.Item(Project.Item(ProjectIndex).colPanel.Count).pNumber
'        Set cPanel = Nothing
'    End If
    
    Project.Item(ProjectIndex).DataChanged = True
    Unload Me
End Sub

Private Function UpdateCostCAtDHullDivide(ByRef cPanel As cPanel, cNewPanel As cPanel)
    On Error GoTo UpdateCostCAtDHullDivide
    Dim cDH As cCostCAtDHull, cIndex As cIndex, cNewIndex As cIndex
    For Each cDH In Project.Item(ProjectIndex).colCostCAtDHull
        For Each cIndex In cDH.InnerShell
            If cIndex.Number = cPanel.pNumber Then
                Set cNewIndex = New cIndex
                cNewIndex.index = cDH.InnerShell.Count + 1
                cNewIndex.Number = Project.Item(ProjectIndex).colPanel.Count + 1 'cNewPanel.pNumber
                cDH.InnerShell.Add cNewIndex, cNewIndex.index
            End If
        Next cIndex
        For Each cIndex In cDH.OuterShell
            If cIndex.Number = cPanel.pNumber Then
                Set cNewIndex = New cIndex
                cNewIndex.index = cDH.OuterShell.Count + 1
                cNewIndex.Number = Project.Item(ProjectIndex).colPanel.Count + 1 'cNewPanel.pNumber
                cDH.OuterShell.Add cNewIndex, cNewIndex.index
            End If
        Next cIndex
    Next cDH
    Exit Function
UpdateCostCAtDHullDivide:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmDividePanel: Function UpdateCostCAtDHullDivide")
End Function

Public Function UpdateCostCAtNappesDivideDivide(ByVal PanelIndex As Integer)
    On Error GoTo UpdateCostCAtNappesDivideErr
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
UpdateCostCAtNappesDivideErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "UpdateCostCAtNappesDivide: DeletePanel")
End Function

Private Function DivideSelectedPanel()
    Dim y1 As Double, z1 As Double, y2 As Double, z2 As Double, Y As Double, z As Double
    iNoOfGirders = cPanel.cScantlings.colGirder.Count
    y1 = Project.Item(ProjectIndex).colNodes.Item(cPanel.cGeometry.InNode).Y
    z1 = Project.Item(ProjectIndex).colNodes.Item(cPanel.cGeometry.InNode).z
    y2 = Project.Item(ProjectIndex).colNodes.Item(cPanel.cGeometry.OutNode).Y
    z2 = Project.Item(ProjectIndex).colNodes.Item(cPanel.cGeometry.OutNode).z
    Y = y1 + dFirstWidth * Cos(PI * cPanel.cGeometry.PanelAngle / 180)
    z = z1 + dFirstWidth * Sin(PI * cPanel.cGeometry.PanelAngle / 180)

    Set cNode = New cNode
    cNode.Y = Y
    cNode.z = z
    cNode.index = Project.Item(ProjectIndex).colNodes.Count + 1
    cNode.nNumber = Project.Item(ProjectIndex).colNodes.Count + 1
    Project.Item(ProjectIndex).colNodes.Add cNode, Project.Item(ProjectIndex).colNodes.Count + 1
    Set cNode = Nothing
    tmpNode = cPanel.cGeometry.OutNode
    cPanel.cGeometry.OutNode = Project.Item(ProjectIndex).colNodes.Count
    Set cNewPanel = New cPanel
    Set cNewPanel = cPanel.Clone
    'cNewPanel.Index = Project.Item(ProjectIndex).colPanel.Count + 1
    UpdateCostCAtDHullDivide cPanel, cNewPanel
    UpdateCostCAtNappesDivideDivide cPanel.pNumber
    GetLengthAngle y1, z1, Y, z, cPanel
    GetLengthAngle Y, z, y2, z2, cNewPanel
'    cNewPanel.cGeometry.PanelWidth = (cPanel.cGeometry.PanelWidth - dFirstWidth)
'    cPanel.cGeometry.PanelWidth = dFirstWidth
    
    'Update Panel Properties
    UpdateSpacingsPrimaryStiffeners    ' Update Spacings to preserve the overall number of longitudinals
    UpdateSpacingsSecondaryStiffeners
    UpdateGirders
    UpdateUniformPressures
    UpdateStepwisePressures
    UpdateMarsPressures
    UpdateNodes
    
    
'-----------------------------------------------
'    Dim nr1 As Integer
'    nr1 = nr + 1
'    Mesh.Add Item:=Panel(header.NETO)
'    i = 0
'    Panel(header.NETO).Pno = header.NETO
'    Dim cc As CLoads1
'    Call coordonata2(l1, 0, unghi, X, Y)
'    X = X + X1
'    Y = Y + y1
'    Mesh.Item(nr).Px2 = X
'    Mesh.Item(nr).py2 = Y
'    Mesh.Item(header.NETO).Px1 = X
'    Mesh.Item(header.NETO).py1 = Y
'    Mesh.Item(header.NETO).Px2 = X2
'    Mesh.Item(header.NETO).py2 = y2
'    Mesh.Item(nr).HIGHT = l1: Mesh.Item(header.NETO).HIGHT = l2
'    Mesh.Item(header.NETO).Angle = Mesh.Item(nr).Angle
'    Copy1 nr, header.NETO
 
'    Call GenNoh
'    Call Desen(frmGraphics.poza)
'    frmGraphics.Redraw Hscroll_Temp, Vscroll_Temp
'    Combo1.Clear
'    For i = 1 To Mesh.Count
'        Combo1.AddItem (Mesh.Item(i).Pno)
'    Next i
'    Combo1.ListIndex = 0
'-----------------------------------------------
    
    Project.Item(ProjectIndex).colPanel.Add cNewPanel, Project.Item(ProjectIndex).colPanel.Count + 1
    UpdateCoordinates Project.Item(ProjectIndex).frmProject.cRectWND
    UpdatePanelConnections ProjectIndex
    UpdateBoundary ProjectIndex
    Draw ProjectIndex
End Function

Private Sub UpdateSpacingsPrimaryStiffeners()
    Dim iInitNoOfStiff As Integer
    Dim iNoOfStiff1 As Integer
    Dim iNoOfStiff2 As Integer
    Dim dSpacing1 As Double
    Dim dSpacing2 As Double
    
    Select Case cPanel.cScantlings.cPrimaryStiffeners.DistributionMode
        Case "EE1"
            iInitNoOfStiff = Divide((cPanel.cGeometry.PanelWidth + cNewPanel.cGeometry.PanelWidth), cPanel.cScantlings.cPrimaryStiffeners.Spacing) - 1
        Case "EE2"
            iInitNoOfStiff = Divide((cPanel.cGeometry.PanelWidth + cNewPanel.cGeometry.PanelWidth), cPanel.cScantlings.cPrimaryStiffeners.Spacing)
    End Select
    iNoOfStiff1 = CInt(iInitNoOfStiff * cPanel.cGeometry.PanelWidth / (cPanel.cGeometry.PanelWidth + cNewPanel.cGeometry.PanelWidth))
    iNoOfStiff2 = iInitNoOfStiff - iNoOfStiff1
    Select Case cPanel.cScantlings.cPrimaryStiffeners.DistributionMode
        Case "EE1"
            dSpacing1 = Divide(cPanel.cGeometry.PanelWidth, (iNoOfStiff1 + 1))
            dSpacing2 = Divide(cNewPanel.cGeometry.PanelWidth, (iNoOfStiff2 + 1))
        Case "EE2"
            dSpacing1 = Divide(cPanel.cGeometry.PanelWidth, iNoOfStiff1)
            dSpacing2 = Divide(cNewPanel.cGeometry.PanelWidth, iNoOfStiff2)
    End Select
    If dSpacing1 = 0 Then
        dSpacing1 = cPanel.cGeometry.PanelWidth
        With cPanel.cScantlings.cPrimaryStiffeners
            .WebHeight = 0.001
            .WebThickness = 0.001
            .FlangeWidth = 0.001
            .FlangeThickness = 0.001
        End With
    End If
    If dSpacing2 = 0 Then
        dSpacing2 = cNewPanel.cGeometry.PanelWidth
        With cNewPanel.cScantlings.cPrimaryStiffeners
            .WebHeight = 0.001
            .WebThickness = 0.001
            .FlangeWidth = 0.001
            .FlangeThickness = 0.001
        End With
    End If
    cPanel.cScantlings.cPrimaryStiffeners.Spacing = Round(dSpacing1, 4)
    cNewPanel.cScantlings.cPrimaryStiffeners.Spacing = Round(dSpacing2, 4)
End Sub

Private Sub UpdateSpacingsSecondaryStiffeners()
    Dim iInitNoOfStiff As Integer
    Dim iNoOfStiff1 As Integer
    Dim iNoOfStiff2 As Integer
    Dim dSpacing1 As Double
    Dim dSpacing2 As Double
    
    iInitNoOfStiff = Divide((cPanel.cGeometry.PanelWidth + cNewPanel.cGeometry.PanelWidth), cPanel.cScantlings.cSecondaryStiffeners.Spacing)
    iNoOfStiff1 = CInt(iInitNoOfStiff * cPanel.cGeometry.PanelWidth / (cPanel.cGeometry.PanelWidth + cNewPanel.cGeometry.PanelWidth))
    iNoOfStiff2 = iInitNoOfStiff - iNoOfStiff1
    dSpacing1 = Divide(cPanel.cGeometry.PanelWidth, iNoOfStiff1)
    dSpacing2 = Divide(cNewPanel.cGeometry.PanelWidth, iNoOfStiff2)
    If dSpacing1 = 0 Then
        dSpacing1 = cPanel.cGeometry.PanelWidth
        With cPanel.cScantlings.cSecondaryStiffeners
            .WebHeight = 0.001
            .WebThickness = 0.001
            .FlangeWidth = 0.001
            .FlangeThickness = 0.001
        End With
    End If
    If dSpacing2 = 0 Then
        dSpacing2 = cNewPanel.cGeometry.PanelWidth
        With cNewPanel.cScantlings.cSecondaryStiffeners
            .WebHeight = 0.001
            .WebThickness = 0.001
            .FlangeWidth = 0.001
            .FlangeThickness = 0.001
        End With
    End If
    cPanel.cScantlings.cSecondaryStiffeners.Spacing = Round(dSpacing1, 4)
    cNewPanel.cScantlings.cSecondaryStiffeners.Spacing = Round(dSpacing2, 4)
End Sub

Private Sub UpdateGirders()
    Dim cGirder As cGirder
    Dim cNewGirder As cGirder
    Dim i As Integer
    i = 0
    If iNoOfGirders > 0 Then
        For Each cGirder In cNewPanel.cScantlings.colGirder
            cNewPanel.cScantlings.colGirder.Remove cGirder.index
        Next cGirder
        For Each cGirder In cPanel.cScantlings.colGirder
            If cGirder.Distance > dFirstWidth Then
                i = i + 1
                Set cNewGirder = cGirder.Clone
                cNewGirder.index = i
                cNewGirder.Distance = cGirder.Distance - dFirstWidth
                cNewPanel.cScantlings.colGirder.Add cNewGirder, i
                cPanel.cScantlings.colGirder.Remove cGirder.index
            End If
        Next cGirder
    End If
End Sub

Private Sub UpdateMarsPressures()
    On Error GoTo UpdateMarsPressuresErr
    Dim dStartPress As Double, dEndPress As Double, dMidPress As Double
    Dim L1 As Double, L2 As Double
    L1 = cPanel.cGeometry.PanelWidth
    L2 = cNewPanel.cGeometry.PanelWidth
    Dim MarsLC As cMarsLoadCase
    Dim MarsLType As cMarsLoadType
    For Each MarsLC In cPanel.colMarsLoadCase
        For Each MarsLType In MarsLC.colMarsLoadType
            'left compartment
            dStartPress = MarsLType.LeftCompPress.In_Node
            dEndPress = MarsLType.LeftCompPress.Out_Node
            Select Case dStartPress
                Case Is <= dEndPress
                    dMidPress = dStartPress + (dEndPress - dStartPress) * (L1 / (L1 + L2))
                Case Is > dEndPress
                    dMidPress = dEndPress + (dStartPress - dEndPress) * (L2 / (L1 + L2))
            End Select
            MarsLType.LeftCompPress.Out_Node = dMidPress
            cNewPanel.colMarsLoadCase.Item(MarsLC.index).colMarsLoadType.Item(MarsLType.index).LeftCompPress.In_Node = dMidPress
            
            'right compartment
            dStartPress = MarsLType.RightCompPress.In_Node
            dEndPress = MarsLType.RightCompPress.Out_Node
            Select Case dStartPress
                Case Is <= dEndPress
                    dMidPress = dStartPress + (dEndPress - dStartPress) * (L1 / (L1 + L2))
                Case Is > dEndPress
                    dMidPress = dEndPress + (dStartPress - dEndPress) * (L2 / (L1 + L2))
            End Select
            MarsLType.RightCompPress.Out_Node = dMidPress
            cNewPanel.colMarsLoadCase.Item(MarsLC.index).colMarsLoadType.Item(MarsLType.index).RightCompPress.In_Node = dMidPress
        Next MarsLType
    Next MarsLC
    Exit Sub
UpdateMarsPressuresErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmDividePanel: Sub UpdateMarsPressures")
End Sub

Private Sub UpdateUniformPressures()
    Dim dStartPress As Double, dEndPress As Double, dMidPress As Double
    Dim cLoadCase As cLoadCase
    Dim L1 As Double, L2 As Double
    L1 = cPanel.cGeometry.PanelWidth
    L2 = cNewPanel.cGeometry.PanelWidth
    For Each cLoadCase In cPanel.colLoadCase
        dStartPress = cLoadCase.LateralPressureIn
        dEndPress = cLoadCase.LateralPressureOut
        Select Case dStartPress
            Case Is <= dEndPress
                dMidPress = dStartPress + (dEndPress - dStartPress) * (L1 / (L1 + L2))
            Case Is > dEndPress
                dMidPress = dEndPress + (dStartPress - dEndPress) * (L2 / (L1 + L2))
        End Select
        cLoadCase.LateralPressureOut = Round(dMidPress, 4)
        cNewPanel.colLoadCase.Item(cLoadCase.index).LateralPressureIn = Round(dMidPress, 4)
    Next cLoadCase
End Sub

Private Sub UpdateStepwisePressures()
    Dim dStartPress As Double, dEndPress As Double, dMidPress As Double
    Dim cLoadCase As cLoadCase, cStepWiseLateralPressure As cStepWiseLateralPressure
    Dim L1 As Double, L2 As Double
    L1 = cPanel.cGeometry.PanelWidth
    L2 = cNewPanel.cGeometry.PanelWidth
    For Each cLoadCase In cPanel.colLoadCase
        For Each cStepWiseLateralPressure In cLoadCase.colStepWiseLateralPressure
            dStartPress = cStepWiseLateralPressure.LateralPressureIn
            dEndPress = cStepWiseLateralPressure.LateralPressureOut
            Select Case dStartPress
                Case Is <= dEndPress
                    dMidPress = dStartPress + (dEndPress - dStartPress) * (L1 / (L1 + L2))
                Case Is > dEndPress
                    dMidPress = dEndPress + (dStartPress - dEndPress) * (L2 / (L1 + L2))
            End Select
        cStepWiseLateralPressure.LateralPressureOut = dMidPress
        cNewPanel.colLoadCase.Item(cLoadCase.index).colStepWiseLateralPressure.Item(cStepWiseLateralPressure.index).LateralPressureIn = dMidPress
        Next cStepWiseLateralPressure
    Next cLoadCase
End Sub

Private Sub UpdateNodes()
    cNewPanel.cGeometry.InNode = Project.Item(ProjectIndex).colNodes.Count
    cNewPanel.cGeometry.OutNode = tmpNode
    cNewPanel.index = Project.Item(ProjectIndex).colPanel.Count + 1
    cNewPanel.pNumber = Project.Item(ProjectIndex).colPanel.Count + 1
End Sub

Private Sub Form_Load()
    Me.Move (Screen.Width - Me.Width) / 2, (Screen.Height - Me.Height) / 2
    cmdOK.Picture = LoadResPicture("ID_BITMAP_OK", 0)
    cmdCancel.Picture = LoadResPicture("ID_BITMAP_CANCEL", 0)
    Me.Caption = "Divide Panel - [" & GetFileName(Project.Item(ProjectIndex).sFileName) & "]"
End Sub

Public Function OpenForm(ByVal index As Integer)
    ProjectIndex = ActiveProject
    Set cPanel = Project.Item(ProjectIndex).colPanel.Item(index)
    lblPanelToDivide.Caption = "Panel To Divide: no°" & index & " (width = " & Round(cPanel.cGeometry.PanelWidth, 4) & " m)"
    txtFirstWidth.Text = Round(cPanel.cGeometry.PanelWidth / 2, 4)
    txtFirstWidth.SelStart = 0
    txtFirstWidth.SelLength = Len(txtFirstWidth.Text)
    Me.Show vbModal
End Function

Private Sub Form_Resize()
    Me.Height = cmdOK.Top + cmdOK.Height + 600
End Sub

Private Sub Form_Unload(Cancel As Integer)
    Set cPanel = Nothing
    Set cNewPanel = Nothing
End Sub

Private Sub txtFirstWidth_GotFocus()
    txtFirstWidth.SelStart = 0
    txtFirstWidth.SelLength = Len(txtFirstWidth.Text)
End Sub

Private Sub txtFirstWidth_Validate(Cancel As Boolean)
    ValidateNumeric txtFirstWidth, Cancel
    txtFirstWidth_GotFocus
    If Cancel = True Then Exit Sub
    ValidateNonNullPozitive txtFirstWidth, Cancel
    txtFirstWidth_GotFocus
    If Cancel = True Then Exit Sub
    If Round(CDbl(txtFirstWidth), 4) >= cPanel.cGeometry.PanelWidth Then
        MsgBox "First Witdth should be smaller than the panel width.", vbCritical + vbOKOnly
        Cancel = True
    End If
End Sub
