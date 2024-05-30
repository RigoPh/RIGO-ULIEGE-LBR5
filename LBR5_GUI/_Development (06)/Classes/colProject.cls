VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "colProject"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
Private mCol As Collection
Private Const PROJECTSTRINGKEY As String = "Project"

Public Function Add(m As cProject, i As Integer) As Boolean
    On Error GoTo AddErr
    mCol.Add m, PROJECTSTRINGKEY & i
    Set m = Nothing
    Add = True
    Exit Function
AddErr:
    Add = False
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colProject: Function Add")
End Function

Public Function Remove(i As Integer) As Boolean
    On Error GoTo RemoveErr
    Project.Item(i).CloseForms 1
    mCol.Remove (PROJECTSTRINGKEY & i)
    Remove = True
    'ReNumProjects
    lProjectCount = lProjectCount - 1
    Exit Function
RemoveErr:
    Remove = False
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colProject: Function Remove")
End Function

Public Property Get Item(i As Integer) As cProject
    On Error GoTo ItemGetErr
    Set Item = mCol(PROJECTSTRINGKEY & i)
    Exit Property
ItemGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colProject: Item Property Get")
End Property

Public Property Get Count() As Long
    On Error GoTo CountGetErr
    Count = mCol.Count
    Exit Property
CountGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colProject: Count Property Get")
End Property

Public Property Get NewEnum() As IUnknown
Attribute NewEnum.VB_UserMemId = -4
Attribute NewEnum.VB_MemberFlags = "40"
    On Error GoTo NewEnumGetErr
    Set NewEnum = mCol.[_NewEnum]
    Exit Property
NewEnumGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colProject: NewEnum Property Get")
End Property

Public Function RemoveProject(ByVal index As Integer)
    Dim cProject As cProject
    For Each cProject In mCol
        If cProject.index = index Then
            Remove cProject.index
            Exit Function
        End If
    Next cProject
End Function

Public Sub OpenProject()
    lProjectCount = lProjectCount + 1
    Dim p As cProject
    Set p = New cProject
    p.index = lProjectCount
    Project.Add p, lProjectCount
    'Config lProjectCount
    Project.Item(lProjectCount).ReadConfigFile
    Project.Item(lProjectCount).frmProject.Tag = lProjectCount
    Project.Item(lProjectCount).frmProject.Caption = "Project" & lProjectCount & ".lbr"
    Project.Item(lProjectCount).sFileName = App.Path & "\Project" & lProjectCount & ".lbr"
    Project.Item(lProjectCount).frmProject.Show
    Project.Item(lProjectCount).frmProject.StatusBar.Panels("LoadCase").Text = "LOAD CASE: None"
    ActiveProject = lProjectCount
'    ReadConfig
    Project.Item(lProjectCount).ReadDefaults lProjectCount
    setFunctionMode NO_FUNCTION
    setScreenMode NORMAL_MODE
    Toolbars

End Sub
'je suis ici
Public Function OpenAvproLbr5TransfertFile(ByVal sFile As String)
    On Error GoTo OpenAvproLbr5TransfertFileErr
    Dim oProject As New cAVPROProject
    
    Dim iNoOfFile As Long
    
    iNoOfFile = FreeFile(0)
    Open sFile For Input As iNoOfFile
    oProject.readXML (iNoOfFile)
    Close #iNoOfFile

    Project.OpenProject

    Project.Item(lProjectCount).frmProject.Tag = lProjectCount
    Project.Item(lProjectCount).frmProject.Caption = GetFileName(sFile)
    Project.Item(lProjectCount).sFileName = sFile
    Read_AVPRO oProject

    ZoomFull
    setFunctionMode NO_FUNCTION
    setScreenMode NORMAL_MODE
    Exit Function
OpenAvproLbr5TransfertFileErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cohhlProject: Function OpenAvproLbr5TransfertFile")
End Function

Public Function OpenMARS_LBR5TransfertFile(ByVal sFile As String)
    On Error GoTo OpenMARS_LBR5TransfertFileErr
    Dim fso As New FileSystemObject, fil As file, ts As TextStream
    Set fil = fso.GetFile(sFile)
    Set ts = fil.OpenAsTextStream(ForReading)
    Me.OpenProject
    OpenMarsFile ts, lProjectCount
    ts.Close
    Me.Item(lProjectCount).frmProject.Tag = lProjectCount
    Me.Item(lProjectCount).frmProject.Caption = GetFileName(sFile)
    Me.Item(lProjectCount).sFileName = sFile
    ZoomFull
    setFunctionMode NO_FUNCTION
    setScreenMode NORMAL_MODE
    frmMarsImportMessage.Show vbModeless, fMainForm
    Exit Function
OpenMARS_LBR5TransfertFileErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cohhlProject: Function OpenMARS_LBR5TransfertFile")
End Function

Public Function OpenLBR5TextFile(ByVal sFile As String)
    On Error GoTo OpenLBR5TextFileErr
    Dim fso As New FileSystemObject, fil As file, ts As TextStream
    Set fil = fso.GetFile(sFile)
    Set ts = fil.OpenAsTextStream(ForReading)
    Me.OpenProject
    OpenTxtFile ts, lProjectCount
    ts.Close
    Me.Item(lProjectCount).frmProject.Tag = lProjectCount
    Me.Item(lProjectCount).frmProject.Caption = GetFileName(sFile)
    Me.Item(lProjectCount).sFileName = sFile
    ZoomFull
    setFunctionMode NO_FUNCTION
    setScreenMode NORMAL_MODE
    Exit Function
OpenLBR5TextFileErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cohhlProject: Function OpenLBR5TextFile")
End Function

Public Function OpenLBR5ASCIIFile(ByVal sFile As String)
    On Error GoTo OpenLBR5ASCIIFileErr
    'Verify Version
    Dim iNoOfFile As Integer, ss As String * 100
    sFile = GetFilePath(sFile) & GetFileRoot(sFile) & ".lbr"
    iNoOfFile = FreeFile
'    Open sFile For Binary As #iNoOfFile
'    Get #iNoOfFile, , ss
'    If Trim(ss) <> VersionNumber Then
'        MsgBox "Bad file version!", vbCritical + vbOKOnly
'        Exit Function
'    End If
'    Close #iNoOfFile
    'Read File
    Me.OpenProject
    iNoOfFile = FreeFile
    Open sFile For Binary As #iNoOfFile
'        Me.Item(lProjectCount).ReadASCII iNoOfFile
        OpenAsciiFile iNoOfFile, lProjectCount
    Close #iNoOfFile
    
    Me.Item(lProjectCount).frmProject.Tag = lProjectCount
    Me.Item(lProjectCount).frmProject.Caption = GetFileName(sFile)
    Me.Item(lProjectCount).sFileName = sFile
    ChDrive Left(GetFilePath(Me.Item(lProjectCount).sFileName), 3)
    ChDir GetFilePath(Me.Item(lProjectCount).sFileName)
    ZoomFull
    setFunctionMode NO_FUNCTION
    setScreenMode NORMAL_MODE
    Exit Function
OpenLBR5ASCIIFileErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colProject: Function OpenLBR5ASCIIFile")
End Function

Public Function SaveAllLBR5TextFiles()
    On Error GoTo SaveAllLBR5TextFilesErr
    Dim cProject As cProject
    Dim lActiveProject As Long
    lActiveProject = Me.Item(ActiveProject).frmProject.Tag
    For Each cProject In Me
        ChDrive Left(GetFilePath(Me.Item(cProject.index).sFileName), 3)
        ChDir GetFilePath(Me.Item(cProject.index).sFileName)
        ActiveProject = cProject.index
        cProject.SaveAsLBR5TextFile cProject.sFileName
    Next cProject
    ActiveProject = lActiveProject
    ChDrive Left(GetFilePath(Me.Item(ActiveProject).sFileName), 3)
    ChDir GetFilePath(Me.Item(ActiveProject).sFileName)
    Exit Function
SaveAllLBR5TextFilesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colProject: Function SaveAllLBR5TextFiles")
End Function

Public Function Clone() As colProject
    On Error GoTo CloneErr
    Dim OBJ As cProject
    Set Clone = New colProject
    For Each OBJ In mCol
        Clone.Add OBJ.Clone, OBJ.Clone.index
    Next OBJ
    Exit Function
CloneErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colProject: Function Clone")
End Function

Private Sub Class_Initialize()
    Set mCol = New Collection
End Sub

Private Sub Class_terminate()
    Set mCol = Nothing
End Sub


