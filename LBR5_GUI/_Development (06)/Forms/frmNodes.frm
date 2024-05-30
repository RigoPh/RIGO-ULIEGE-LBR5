VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "comdlg32.ocx"
Object = "{0D452EE1-E08F-101A-852E-02608C4D0BB4}#2.0#0"; "FM20.DLL"
Begin VB.Form frmNodes 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Nodes"
   ClientHeight    =   3870
   ClientLeft      =   2865
   ClientTop       =   1830
   ClientWidth     =   4155
   Icon            =   "frmNodes.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   3870
   ScaleWidth      =   4155
   ShowInTaskbar   =   0   'False
   Begin MSComDlg.CommonDialog dlgCommonDialog 
      Left            =   480
      Top             =   2280
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
   End
   Begin VB.ListBox lstNodes 
      Height          =   2205
      Left            =   360
      TabIndex        =   8
      TabStop         =   0   'False
      Top             =   600
      Width           =   2535
   End
   Begin VB.TextBox txtZ 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   2040
      TabIndex        =   1
      Top             =   120
      Width           =   975
   End
   Begin VB.TextBox txtY 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   480
      TabIndex        =   0
      Top             =   120
      Width           =   975
   End
   Begin MSForms.CommandButton cmdWrite 
      Height          =   375
      Left            =   3000
      TabIndex        =   13
      TabStop         =   0   'False
      Top             =   2520
      Width           =   975
      Caption         =   "Write"
      PicturePosition =   327683
      Size            =   "1720;661"
      Accelerator     =   119
      FontHeight      =   165
      FontCharSet     =   0
      FontPitchAndFamily=   2
      ParagraphAlign  =   3
   End
   Begin VB.Label lblNoOfNodes 
      AutoSize        =   -1  'True
      Caption         =   "lblNoOfNodes"
      Height          =   195
      Left            =   3120
      TabIndex        =   12
      Top             =   120
      Width           =   990
   End
   Begin MSForms.CommandButton cmdRead 
      Height          =   375
      Left            =   3000
      TabIndex        =   11
      TabStop         =   0   'False
      Top             =   2040
      Width           =   975
      Caption         =   "Read"
      PicturePosition =   327683
      Size            =   "1720;661"
      Accelerator     =   100
      FontHeight      =   165
      FontCharSet     =   0
      FontPitchAndFamily=   2
      ParagraphAlign  =   3
   End
   Begin MSForms.CommandButton cmdModify 
      Height          =   375
      Left            =   3000
      TabIndex        =   6
      TabStop         =   0   'False
      Top             =   1560
      Width           =   975
      VariousPropertyBits=   25
      Caption         =   "Modify"
      PicturePosition =   327683
      Size            =   "1720;661"
      Accelerator     =   77
      FontEffects     =   1073750016
      FontHeight      =   165
      FontCharSet     =   0
      FontPitchAndFamily=   2
      ParagraphAlign  =   3
   End
   Begin VB.Label lblZ 
      AutoSize        =   -1  'True
      Caption         =   "Z [m]:"
      Height          =   195
      Left            =   1560
      TabIndex        =   10
      Top             =   120
      Width           =   405
   End
   Begin VB.Label lblY 
      AutoSize        =   -1  'True
      Caption         =   "Y [m]:"
      Height          =   195
      Left            =   0
      TabIndex        =   9
      Top             =   120
      Width           =   405
   End
   Begin MSForms.CommandButton cmdAdd 
      Height          =   375
      Left            =   3000
      TabIndex        =   2
      Top             =   600
      Width           =   975
      Caption         =   "Add"
      PicturePosition =   327683
      Size            =   "1720;661"
      Accelerator     =   65
      FontHeight      =   165
      FontCharSet     =   0
      FontPitchAndFamily=   2
      ParagraphAlign  =   3
   End
   Begin MSForms.CommandButton cmdRemove 
      Height          =   375
      Left            =   3000
      TabIndex        =   7
      TabStop         =   0   'False
      Top             =   1080
      Width           =   975
      Caption         =   "Remove"
      PicturePosition =   327683
      Size            =   "1720;661"
      Accelerator     =   82
      FontHeight      =   165
      FontCharSet     =   0
      FontPitchAndFamily=   2
      ParagraphAlign  =   3
   End
   Begin MSForms.CommandButton cmdOK 
      Default         =   -1  'True
      Height          =   375
      Left            =   1680
      TabIndex        =   4
      TabStop         =   0   'False
      Top             =   3000
      Width           =   1095
      Caption         =   "OK"
      PicturePosition =   327683
      Size            =   "1931;661"
      FontHeight      =   165
      FontCharSet     =   0
      FontPitchAndFamily=   2
      ParagraphAlign  =   3
   End
   Begin MSForms.CommandButton cmdCancel 
      Cancel          =   -1  'True
      Height          =   375
      Left            =   2880
      TabIndex        =   3
      TabStop         =   0   'False
      Top             =   3000
      Width           =   1095
      Caption         =   "Cancel"
      PicturePosition =   327683
      Size            =   "1931;661"
      FontHeight      =   165
      FontCharSet     =   0
      FontPitchAndFamily=   2
      ParagraphAlign  =   3
   End
   Begin MSForms.CommandButton cmdApply 
      Height          =   375
      Left            =   480
      TabIndex        =   5
      TabStop         =   0   'False
      Top             =   3000
      Width           =   1095
      Caption         =   "Apply"
      PicturePosition =   327683
      Size            =   "1931;661"
      FontHeight      =   165
      FontCharSet     =   0
      FontPitchAndFamily=   2
      ParagraphAlign  =   3
   End
End
Attribute VB_Name = "frmNodes"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Dim ProjectIndex As Integer
Dim Panel As cPanel
Dim colPanel As colPanel
Dim Node As cNode, colNodes As colNodes
Dim NodesIndex As Integer
Dim iPanelInNode() As Integer
Dim iPanelOutNode() As Integer

Private Sub cmdAdd_Click()
    Dim Cancel As Boolean
    Cancel = False
    ValidateNumeric txtY, Cancel
    If Cancel = True Then Exit Sub
    ValidateNumeric txtZ, Cancel
    If Cancel = True Then Exit Sub
    If CheckIfNodeExists = True Then Exit Sub
    If Cancel = True Then Exit Sub
    Set Node = New cNode
    Node.y = txtY
    Node.z = -txtZ
    Node.index = colNodes.Count + 1
    Node.nNumber = colNodes.Count + 1
    colNodes.Add Node, colNodes.Count + 1
    Set Node = Nothing
    PopulateList
    txtY = ""
    txtZ = ""
    txtY.SetFocus
    lblNoOfNodes.Caption = colNodes.Count & " node(s)"
End Sub

Private Function CheckIfNodeExists() As Boolean
    Dim y As Double, z As Double
    y = CDbl(txtY)
    z = CDbl(txtZ)
    For Each Node In colNodes
        If Node.y = y And Node.z = -z Then
            CheckIfNodeExists = True
            MsgBox "Node Already Exists."
            Exit Function
        End If
    Next Node
End Function

Private Sub cmdAdd_DblClick(Cancel As MSForms.ReturnBoolean)
    Cancel = True
End Sub

Private Sub cmdApply_Click()
    SetData
    GetData
    PopulateList
    UpdateCoordinates Project.Item(ProjectIndex).frmProject.cRectWND
    ZoomFull
    Draw ProjectIndex
    Project.Item(ProjectIndex).DataChanged = True
End Sub

Private Sub cmdApply_DblClick(Cancel As MSForms.ReturnBoolean)
    Cancel = True
End Sub

Private Sub cmdCancel_Click()
    Unload Me
End Sub

Private Sub cmdModify_Click()
    Dim Cancel As Boolean
    Cancel = False
    Dim tmpY As Double, tmpZ As Double
    tmpY = colNodes.Item(NodesIndex).y
    tmpZ = colNodes.Item(NodesIndex).z
    colNodes.Item(NodesIndex).y = txtY
    colNodes.Item(NodesIndex).z = -txtZ

    ValidateNumeric txtY, Cancel
    If Cancel = True Then Exit Sub
    ValidateNumeric txtZ, Cancel
    If Cancel = True Then Exit Sub
    cmdModify.Enabled = False
    If CheckPanelIntersection = True Then
        colNodes.Item(NodesIndex).y = tmpY
        colNodes.Item(NodesIndex).z = tmpZ
        Exit Sub
    End If
    PopulateList
End Sub

Private Function CheckPanelIntersection() As Boolean
    On Error GoTo CheckPanelIntersectionErr
    Dim y1 As Double, z1 As Double, y2 As Double, z2 As Double
    Dim y3 As Double, z3 As Double, y4 As Double, z4 As Double
    Dim angle As Double
    Dim panel2 As cPanel
    
    

    For Each Panel In colPanel
        If Panel.cGeometry.InNode = NodesIndex Then
            y1 = CDbl(txtY)
            z1 = -CDbl(txtZ)
            y2 = colNodes.Item(Panel.cGeometry.OutNode).y
            z2 = colNodes.Item(Panel.cGeometry.OutNode).z
        End If
        If Panel.cGeometry.OutNode = NodesIndex Then
            y1 = colNodes.Item(Panel.cGeometry.InNode).y
            z1 = colNodes.Item(Panel.cGeometry.InNode).z
            y2 = CDbl(txtY)
            z2 = -CDbl(txtZ)
        End If
        If Panel.cGeometry.InNode = NodesIndex Or Panel.cGeometry.OutNode = NodesIndex Then
            For Each panel2 In colPanel
                If panel2.index <> Panel.index Then
                    y3 = colNodes.Item(panel2.cGeometry.InNode).y
                    z3 = colNodes.Item(panel2.cGeometry.InNode).z
                    y4 = colNodes.Item(panel2.cGeometry.OutNode).y
                    z4 = colNodes.Item(panel2.cGeometry.OutNode).z
                    angle = panel2.cGeometry.PanelAngle
                    If DetectIntersection(y1, z1, y2, z2, y3, z3, y4, z4, angle) = True Then
                        MsgBox "Intersection with another panel.", vbCritical + vbOKOnly
                        CheckPanelIntersection = True
                        Exit Function
                    End If
                End If
            Next panel2
        End If
    Next Panel
    Exit Function
CheckPanelIntersectionErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmNodes: Function CheckPanelIntersection")
End Function

Private Sub cmdModify_DblClick(Cancel As MSForms.ReturnBoolean)
    Cancel = True
End Sub

Private Sub cmdOK_Click()
    SetData
    UpdateCoordinates Project.Item(ProjectIndex).frmProject.cRectWND
    ZoomFull
    Draw ProjectIndex
    Project.Item(ProjectIndex).DataChanged = True
    Unload Me
End Sub

Private Sub cmdRead_Click()
    On Error GoTo CancelSelected
    Dim sFile As String
    Dim Cancel As Boolean
    With dlgCommonDialog
        .DialogTitle = "Read Nodes"
        .CancelError = True
        'ToDo: set the flags and attributes of the common dialog control
        .Filter = "Nodes (*.txt)|*.txt"
        .InitDir = App.Path & "\_Data"
        .ShowOpen
        If Len(.FileName) = 0 Then
            Exit Sub
        End If
        sFile = .FileName
    End With
    Dim fso As New FileSystemObject, fil As file, ts As TextStream
    Set fil = fso.GetFile(sFile)
    Set ts = fil.OpenAsTextStream(ForReading)
    
    Dim NoOfNodes As Long, i As Integer
    Dim Counter As Integer
    Counter = 0
    Dim sLine As String
    Dim v() As Variant
    sLine = ReadLn(ts)
    GetValues 1, sLine, v
    NoOfNodes = Val_(v(1))
    Dim Node1 As cNode
    
    For i = 1 To NoOfNodes
        '
        Set Node = New cNode
        Node.index = colNodes.Count + 1
        Node.nNumber = colNodes.Count + 1
        sLine = ReadLn(ts)
        GetValues 3, sLine, v
        Node.y = Val_(v(2))
        Node.z = -Val_(v(3))
        For Each Node1 In colNodes ' Check if node already exists
            If Node1.y = Node.y And Node1.z = Node.z Then
                Set Node = Nothing
                Counter = Counter + 1
                GoTo NextNode
            End If
        Next Node1
        colNodes.Add Node, Node.index
        Set Node = Nothing
NextNode:
    Next i
    If Counter > 0 Then MsgBox Counter & " duplicate node(s) found and ignored.", vbInformation + vbOKOnly
    PopulateList
    lblNoOfNodes.Caption = colNodes.Count & " node(s)"
    Exit Sub
CancelSelected:
End Sub

Private Sub cmdWrite_Click()
    On Error GoTo CancelSelected
    Dim sFile As String
    Dim Cancel As Boolean
    With dlgCommonDialog
        .DialogTitle = "Write Nodes"
        .CancelError = True
        'ToDo: set the flags and attributes of the common dialog control
        .Filter = "Nodes (*.txt)|*.txt"
        '.InitDir = App.Path & "\_Data"
        .flags = &H2
        .ShowSave
        If Len(.FileName) = 0 Then
            Exit Sub
        End If
        sFile = .FileName
    End With
    Dim fil, ts As TextStream
    Set fil = CreateObject("Scripting.FileSystemObject")
    Set ts = fil.OpenTextFile(sFile, ForWriting, TristateUseDefault)
    
    Dim cNode As cNode
    ts.WriteLine colNodes.Count
    For Each cNode In colNodes
        ts.WriteLine cNode.nNumber & vbTab & Round(cNode.y, 3) & vbTab & Round(-cNode.z, 3)
    Next cNode
    ts.Close
    MsgBox "File '" & sFile & "'" & " succesfully saved.", vbInformation + vbOKOnly
Exit Sub
CancelSelected:

End Sub
Private Sub cmdRead_DblClick(Cancel As MSForms.ReturnBoolean)
    Cancel = True
End Sub

Private Sub cmdRemove_Click()
    Dim iNodeNumber As Integer, i As Integer
    If lstNodes.ListIndex < 0 Then Exit Sub
    
    iNodeNumber = lstNodes.ListIndex + 1
    DeleteNode iNodeNumber
'    For i = 1 To lstNodes.ListCount
'        If lstNodes.Selected(i - 1) = True Then
'            DeleteNode i
'        End If
'    Next i
    
    PopulateList
    lstNodes.ListIndex = iNodeNumber - 2
    lblNoOfNodes.Caption = colNodes.Count & " node(s)"
End Sub

Private Sub DeleteNode(ByVal iNodeNumber As Integer)
    Dim i As Integer
'    Dim v() As Variant
'    Dim sList As String
    
'    sList = lstNodes.Text
'    getValues 1, sList, v
'    iNodeNumber = CInt(v(1))
    
    If iNodeNumber = 0 Then Exit Sub 'DeleteNode NodesIndex, colNodes
    For i = 1 To colPanel.Count
        If iPanelInNode(i) = iNodeNumber Or iPanelOutNode(i) = iNodeNumber Then
            MsgBox "Node is linked to a panel and cannot be deleted.", vbCritical + vbOKOnly
            Exit Sub
        End If
    Next i
    For Each Node In colNodes
        If Node.index = iNodeNumber Then
            colNodes.Remove Node.index
            Exit For
        End If
    Next Node
    Dim colNodes1 As New colNodes
    i = 0
    For Each Node In colNodes ' renum nodes
        i = i + 1
        Node.index = i
        'Node.nNumber = i
        colNodes1.Add Node, i
    Next Node
    Set colNodes = colNodes1
    Set colNodes1 = Nothing
    For Each Panel In colPanel 'renum panel links
        If iPanelInNode(Panel.pNumber) > iNodeNumber Then
            iPanelInNode(Panel.pNumber) = iPanelInNode(Panel.pNumber) - 1
        End If
        If iPanelOutNode(Panel.pNumber) > iNodeNumber Then
            iPanelOutNode(Panel.pNumber) = iPanelOutNode(Panel.pNumber) - 1
        End If
    Next Panel
End Sub

Private Sub cmdRemove_DblClick(Cancel As MSForms.ReturnBoolean)
    Cancel = True
End Sub

Private Sub Form_Load()
    ProjectIndex = ActiveProject
    Me.Move (Screen.Width - Me.Width) / 2, (Screen.Height - Me.Height) / 2
    cmdOK.Picture = LoadResPicture("ID_BITMAP_OK", 0)
    cmdCancel.Picture = LoadResPicture("ID_BITMAP_CANCEL", 0)
    cmdAdd.Picture = LoadResPicture("ID_BITMAP_PLUS", 0)
    cmdRemove.Picture = LoadResPicture("ID_BITMAP_MINUS", 0)
    Me.Caption = "Nodes - [" & GetFileName(Project.Item(ProjectIndex).sFileName) & "]"
    GetData
    lblNoOfNodes.Caption = colNodes.Count & " node(s)"
    PopulateList
    If Licensing.IS_EDIT_NODES = False Then
        cmdApply.Enabled = False
        cmdOK.Enabled = False
    End If
End Sub

Private Sub GetData()
    Set colNodes = Project.Item(ProjectIndex).colNodes.Clone
    Set colPanel = Project.Item(ProjectIndex).colPanel.Clone
    If colPanel.Count > 0 Then
        ReDim iPanelInNode(1 To colPanel.Count)
        ReDim iPanelOutNode(1 To colPanel.Count)
        For Each Panel In colPanel
            iPanelInNode(Panel.pNumber) = Panel.cGeometry.InNode
            iPanelOutNode(Panel.pNumber) = Panel.cGeometry.OutNode
        Next Panel
    End If
End Sub

Private Sub SetData()
    Dim y1 As Double, z1 As Double, y2 As Double, z2 As Double
    For Each Node In colNodes
        Node.nNumber = Node.index
    Next Node
    Set Project.Item(ProjectIndex).colNodes = colNodes
    For Each Panel In Project.Item(ProjectIndex).colPanel
        Panel.cGeometry.InNode = iPanelInNode(Panel.pNumber)
        Panel.cGeometry.OutNode = iPanelOutNode(Panel.pNumber)
        y1 = colNodes.Item(Panel.cGeometry.InNode).y
        z1 = colNodes.Item(Panel.cGeometry.InNode).z
        y2 = colNodes.Item(Panel.cGeometry.OutNode).y
        z2 = colNodes.Item(Panel.cGeometry.OutNode).z
        GetLengthAngle y1, z1, y2, z2, Panel
    Next Panel
End Sub

Private Sub PopulateList()
    lstNodes.Clear
    For Each Node In colNodes
        lstNodes.AddItem Node.nNumber & " : " & Round(Node.y, 3) & "; " & Round(-Node.z, 3)
    Next Node
End Sub

Private Sub Form_Resize()
    Me.Height = cmdOK.Top + cmdOK.Height + 600
End Sub

Private Sub Form_Unload(Cancel As Integer)
    fMainForm.SetFocus
End Sub

Private Sub lstNodes_Click()
    NodesIndex = lstNodes.ListIndex + 1
    cmdModify.Enabled = False
    txtY = ""
    txtZ = ""
End Sub

Private Sub lstNodes_DblClick()
    NodesIndex = lstNodes.ListIndex + 1
    txtY = colNodes.Item(NodesIndex).y
    txtZ = -colNodes.Item(NodesIndex).z
    cmdModify.Enabled = True
    txtY.SetFocus
End Sub

Private Sub txtY_GotFocus()
    txtY.SelStart = 0
    txtY.SelLength = Len(txtY.Text)
End Sub

Private Sub txtY_Validate(Cancel As Boolean)
    'ValidateNumeric txtY, Cancel
End Sub

Private Sub txtZ_GotFocus()
    txtZ.SelStart = 0
    txtZ.SelLength = Len(txtZ.Text)
End Sub

Private Sub txtZ_Validate(Cancel As Boolean)
    'ValidateNumeric txtZ, Cancel
End Sub
