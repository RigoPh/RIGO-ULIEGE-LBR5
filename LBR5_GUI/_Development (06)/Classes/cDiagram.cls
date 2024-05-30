VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cDiagram"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
#Const DebugMode = 0
#If DebugMode Then
    Private mlClassDebugID As Long
#End If

Private mvarIndex As Integer
Private mvarName As String
Private mvarcolResultsXSection As colResultsXSection

Public Property Let Index(ByVal vData As Integer)
    On Error GoTo IndexLetErr
    mvarIndex = vData
    Exit Property
IndexLetErr:
    Call RaiseError(MyUnhandledError, "cDiagram: Index Property Let")
End Property

Public Property Get Index() As Integer
    On Error GoTo IndexGetErr
    Index = mvarIndex
    Exit Property
IndexGetErr:
    Call RaiseError(MyUnhandledError, "cDiagram: Index Property Get")
End Property

Public Property Let Name(ByVal vData As String)
    On Error GoTo NameLetErr
    mvarName = vData
    Exit Property
NameLetErr:
    Call RaiseError(MyUnhandledError, "cDiagram: Name Property Let")
End Property

Public Property Get Name() As String
    On Error GoTo NameGetErr
    Name = mvarName
    Exit Property
NameGetErr:
    Call RaiseError(MyUnhandledError, "cDiagram: Name Property Get")
End Property

Public Property Get colResultsXSection() As colResultsXSection
    On Error GoTo colResultsXSectionGetErr
    If mvarcolResultsXSection Is Nothing Then
        Set mvarcolResultsXSection = New colResultsXSection
    End If
    Set colResultsXSection = mvarcolResultsXSection
    Exit Property
colResultsXSectionGetErr:
    Call RaiseError(MyUnhandledError, "cDiagram: colResultsXSection Property Get")
End Property

Public Property Set colResultsXSection(vData As colResultsXSection)
    On Error GoTo colResultsXSectionSetErr
    Set mvarcolResultsXSection = vData
    Exit Property
colResultsXSectionSetErr:
    Call RaiseError(MyUnhandledError, "cDiagram: colResultsXSection Property Set")
End Property

Public Function Clone() As cDiagram
    On Error GoTo CloneErr
    Set Clone = New cDiagram
    With Clone
        .Index = Me.Index
        .Name = Me.Name
        Set .colResultsXSection = Me.colResultsXSection.Clone
    End With
    Exit Function
CloneErr:
    Call RaiseError(MyUnhandledError, "cStrake: Function Clone")
End Function

Private Sub Class_Initialize()
    #If DebugMode Then
        mlClassDebugID = GetNextClassDebugID()
        Debug.Print "'" & TypeName(Me) & "' instance " & mlClassDebugID & " created"
    #End If
End Sub

Private Sub Class_Terminate()
    #If DebugMode Then
        Debug.Print "'" & TypeName(Me) & "' instance " & CStr(mlClassDebugID) & " is terminating"
    #End If
End Sub

#If DebugMode Then
    Public Property Get ClassDebugID()
        ClassDebugID = mlClassDebugID
    End Property
#End If

