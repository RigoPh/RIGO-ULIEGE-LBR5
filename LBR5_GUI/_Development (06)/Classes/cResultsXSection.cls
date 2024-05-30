VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cResultsXSection"
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
Private mvarSection As Double
Private mvarcolResultsYSection As colResultsYSection

Public Property Let index(ByVal vData As Integer)
    On Error GoTo IndexLetErr
    mvarIndex = vData
    Exit Property
IndexLetErr:
    Call RaiseError(MyUnhandledError, "cResultsXSection: Index Property Let")
End Property

Public Property Get index() As Integer
    On Error GoTo IndexGetErr
    index = mvarIndex
    Exit Property
IndexGetErr:
    Call RaiseError(MyUnhandledError, "cResultsXSection: Index Property Get")
End Property

Public Property Let Section(ByVal vData As Double)
    On Error GoTo SectionLetErr
    mvarSection = vData
    Exit Property
SectionLetErr:
    Call RaiseError(MyUnhandledError, "cResultsXSection: Section Property Let")
End Property

Public Property Get Section() As Double
    On Error GoTo SectionGetErr
    Section = mvarSection
    Exit Property
SectionGetErr:
    Call RaiseError(MyUnhandledError, "cResultsXSection: Section Property Get")
End Property

Public Property Get colResultsYSection() As colResultsYSection
    On Error GoTo colResultsYSectionGetErr
    If mvarcolResultsYSection Is Nothing Then
        Set mvarcolResultsYSection = New colResultsYSection
    End If
    Set colResultsYSection = mvarcolResultsYSection
    Exit Property
colResultsYSectionGetErr:
    Call RaiseError(MyUnhandledError, "cResultsXSection: colResultsYSection Property Get")
End Property

Public Property Set colResultsYSection(vData As colResultsYSection)
    On Error GoTo colResultsYSectionSetErr
    Set mvarcolResultsYSection = vData
    Exit Property
colResultsYSectionSetErr:
    Call RaiseError(MyUnhandledError, "cResultsXSection: colResultsYSection Property Set")
End Property

Public Function Clone() As cResultsXSection
    On Error GoTo CloneErr
    Set Clone = New cResultsXSection
    With Clone
        .index = Me.index
        .Section = Me.Section
        Set .colResultsYSection = Me.colResultsYSection.Clone
    End With
    Exit Function
CloneErr:
    Call RaiseError(MyUnhandledError, "cStrake: Function Clone")
End Function

Private Sub Class_Initialize()
    #If DebugMode Then
        mlClassDebugID = GetNextClassDebugID()
        Debug.Print "'" & TypeSection(Me) & "' instance " & mlClassDebugID & " created"
    #End If
End Sub

Private Sub Class_Terminate()
    #If DebugMode Then
        Debug.Print "'" & TypeSection(Me) & "' instance " & CStr(mlClassDebugID) & " is terminating"
    #End If
    Set mvarcolResultsYSection = Nothing
End Sub

#If DebugMode Then
    Public Property Get ClassDebugID()
        ClassDebugID = mlClassDebugID
    End Property
#End If


