VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "colResultsXSection"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
Private mCol As Collection
Private Const RESULTSXSECTIONSTRINGKEY As String = "ResultsXSection"

Public Function Add(m As cResultsXSection, i As Integer) As Boolean
    On Error GoTo AddErr
    mCol.Add m, RESULTSXSECTIONSTRINGKEY & i
    Set m = Nothing
    Add = True
    Exit Function
AddErr:
    Add = False
    Call RaiseError(MyUnhandledError, "colResultsXSection: Function Add")
End Function

Public Function Remove(i As Integer) As Boolean
    On Error GoTo RemoveErr
    mCol.Remove (RESULTSXSECTIONSTRINGKEY & i)
    Remove = True
    Exit Function
RemoveErr:
    Remove = False
    Call RaiseError(MyUnhandledError, "colResultsXSection: Function Remove")
End Function

Public Property Get Item(i As Integer) As cResultsXSection
    On Error GoTo ItemGetErr
    Set Item = mCol(RESULTSXSECTIONSTRINGKEY & i)
    Exit Property
ItemGetErr:
    Call RaiseError(MyUnhandledError, "colResultsXSection: Item Property Get")
End Property

Public Property Get Count() As Long
    On Error GoTo CountGetErr
    Count = mCol.Count
    Exit Property
CountGetErr:
    Call RaiseError(MyUnhandledError, "colResultsXSection: Count Property Get")
End Property

Public Property Get NewEnum() As IUnknown
    On Error GoTo NewEnumGetErr
    Set NewEnum = mCol.[_NewEnum]
    Exit Property
NewEnumGetErr:
    Call RaiseError(MyUnhandledError, "colResultsXSection: NewEnum Property Get")
End Property

Public Function Clone() As colResultsXSection
    On Error GoTo CloneErr
    Dim obj As cResultsXSection
    Set Clone = New colResultsXSection
    For Each obj In mCol
        Clone.Add obj.Clone, obj.Clone.Index
    Next obj
    Exit Function
CloneErr:
    Call RaiseError(MyUnhandledError, "colResultsXSection: Function Clone")
End Function

Private Sub Class_Initialize()
    Set mCol = New Collection
End Sub

Private Sub Class_Terminate()
    Set mCol = Nothing
End Sub

