VERSION 5.00
Object = "{0D452EE1-E08F-101A-852E-02608C4D0BB4}#2.0#0"; "FM20.DLL"
Begin VB.Form frmMultiSolve 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "MultiSolve"
   ClientHeight    =   4200
   ClientLeft      =   885
   ClientTop       =   3120
   ClientWidth     =   8430
   Icon            =   "frmMultiSolve.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   4200
   ScaleWidth      =   8430
   ShowInTaskbar   =   0   'False
   Begin VB.PictureBox picChageOrder 
      Height          =   435
      Left            =   120
      ScaleHeight     =   375
      ScaleWidth      =   3075
      TabIndex        =   4
      Top             =   3600
      Width           =   3135
      Begin VB.Label lblChangeOrder 
         Caption         =   "Change Priority of selected project"
         Height          =   255
         Left            =   480
         TabIndex        =   6
         Top             =   70
         UseMnemonic     =   0   'False
         Width           =   2415
      End
      Begin MSForms.SpinButton btnSpin 
         Height          =   375
         Left            =   0
         TabIndex        =   5
         Top             =   0
         Width           =   375
         Size            =   "661;661"
         Orientation     =   0
      End
   End
   Begin VB.ListBox lstProjectList 
      Appearance      =   0  'Flat
      Height          =   3540
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   8415
   End
   Begin MSForms.CommandButton cmdClose 
      Default         =   -1  'True
      Height          =   435
      Left            =   7320
      TabIndex        =   3
      Top             =   3600
      Width           =   1005
      Caption         =   "Close"
      PicturePosition =   327683
      Size            =   "1773;767"
      FontHeight      =   165
      FontCharSet     =   0
      FontPitchAndFamily=   2
      ParagraphAlign  =   3
   End
   Begin MSForms.CommandButton cmdSolve 
      Height          =   435
      Left            =   4920
      TabIndex        =   2
      Top             =   3600
      Width           =   1860
      VariousPropertyBits=   268435483
      Caption         =   "Solve all listed projects"
      PicturePosition =   327683
      Size            =   "3281;767"
      FontHeight      =   165
      FontCharSet     =   0
      FontPitchAndFamily=   2
      ParagraphAlign  =   3
   End
   Begin MSForms.CommandButton cmdRemove 
      Height          =   435
      Left            =   3360
      TabIndex        =   1
      Top             =   3600
      Width           =   1425
      VariousPropertyBits=   268435483
      Caption         =   "Remove from list"
      PicturePosition =   327683
      Size            =   "2514;767"
      FontHeight      =   165
      FontCharSet     =   0
      FontPitchAndFamily=   2
      ParagraphAlign  =   3
   End
End
Attribute VB_Name = "frmMultiSolve"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Dim cProject As cProject
Dim sSelectedStr As String
Dim sUpperStr As String
Dim sLowerStr As String

Private Sub btnSpin_SpinDown()
    If lstProjectList.ListIndex < 0 Then Exit Sub
    If lstProjectList.ListIndex < lstProjectList.ListCount - 1 Then
        lstProjectList.List(lstProjectList.ListIndex) = sUpperStr
        lstProjectList.List(lstProjectList.ListIndex + 1) = sSelectedStr
        lstProjectList.Selected(lstProjectList.ListIndex + 1) = True
        lstProjectList_Click
    End If
End Sub

Private Sub btnSpin_SpinUp()
    If lstProjectList.ListIndex < 0 Then Exit Sub
    If lstProjectList.ListIndex > 0 Then
        lstProjectList.List(lstProjectList.ListIndex) = sLowerStr
        lstProjectList.List(lstProjectList.ListIndex - 1) = sSelectedStr
        lstProjectList.Selected(lstProjectList.ListIndex - 1) = True
        lstProjectList_Click
    End If
End Sub

Private Sub cmdClose_Click()
    Unload Me
End Sub

Private Sub cmdRemove_Click()
    Dim i As Integer
Rewind:
    For i = 1 To lstProjectList.ListCount
        If lstProjectList.Selected(i - 1) = True Then
            lstProjectList.RemoveItem i - 1
            GoTo Rewind
        End If
    Next i
    lstProjectList_Click
End Sub

Private Sub cmdRemove_DblClick(Cancel As MSForms.ReturnBoolean)
    Cancel = True
End Sub

Private Sub cmdSolve_Click()
    Dim i As Integer
    For i = 1 To lstProjectList.ListCount
        'If IsDemo(ActiveProject) = True Then Exit Sub
        For Each cProject In Project
            If GetFilePath(cProject.sFileName) & GetFileRoot(cProject.sFileName) = lstProjectList.List(i - 1) Then
                cProject.SolveProject
                Me.Refresh
                GoTo jumpover
            End If
        Next cProject
jumpover:
    Next i
    ChDir GetFilePath(Project.Item(ActiveProject).sFileName)
    Unload Me
End Sub

Private Sub Form_Load()
    PopulateProjectList
End Sub

Private Sub PopulateProjectList()
    Dim lActiveProject As String
    lActiveProject = ActiveProject
    For Each cProject In Project
        If IsDemo(cProject.index) = True Then GoTo 1 'Exit Sub
        UpdatePanelConnections cProject.index
        If CheckModel(cProject.index) = False Then
            lstProjectList.AddItem GetFilePath(cProject.sFileName) & GetFileRoot(cProject.sFileName)
            ActiveProject = cProject.index
            SaveProject cProject.index
            Project.Item(cProject.index).SaveAsLBR5ASCIIFile Project.Item(ActiveProject).sFileName, True
            'Project.Item(cProject.index).frmProject.Caption = GetFileRoot(cProject.sFileName)
        End If
1:
    Next cProject
    ChDrive Left(GetFilePath(Project.Item(ActiveProject).sFileName), 3)
    ChDir GetFilePath(Project.Item(ActiveProject).sFileName)
    ActiveProject = lActiveProject
End Sub

Private Function SaveProject(ByVal index As Integer)
    Dim sFile As String
    
    ChDrive Left(GetFilePath(Project.Item(index).sFileName), 3)
    ChDir GetFilePath(Project.Item(index).sFileName)
    
    sFile = GetFileRoot(Project.Item(index).sFileName) & ".txt"
    sFile = GetFilePath(Project.Item(index).sFileName) & sFile
    Dim fil, ts As TextStream
    Set fil = CreateObject("Scripting.FileSystemObject")
    Set ts = fil.OpenTextFile(sFile, ForWriting, TristateUseDefault)

    Project.Item(index).WriteLBR5txtFile ts
    Set fil = Nothing
    Set ts = Nothing
'    Project.Item(index).sFileName = sFile
'    Project.Item(index).frmProject.Caption = GetFileName(sFile)
    
    Project.Item(index).cSolution.IsSolution = False
    Project.Item(index).DataChanged = False
End Function

Private Sub Form_Resize()
    'Me.Height = cmdClose.Top + cmdClose.Height + 600
    With lstProjectList
        .Left = Me.ScaleLeft
        .Width = Me.ScaleWidth
        .Top = Me.ScaleTop
        .Height = Me.ScaleHeight - 500
    End With
    
End Sub

Private Sub SpinButton1_Change()

End Sub

Private Sub Form_Unload(Cancel As Integer)
    'fMainForm.SetFocus
End Sub

Private Sub Picture1_Click()

End Sub

Private Sub lstProjectList_Click()
    sSelectedStr = lstProjectList.Text
    If lstProjectList.ListIndex > 0 Then
        sLowerStr = lstProjectList.List(lstProjectList.ListIndex - 1)
    End If
    If lstProjectList.ListIndex < lstProjectList.ListCount - 1 Then
        sUpperStr = lstProjectList.List(lstProjectList.ListIndex + 1)
    End If
End Sub
