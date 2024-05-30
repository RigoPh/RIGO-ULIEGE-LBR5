VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "colResultsYSection"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
Private mCol As Collection
Private Const RESULTSYSECTIONSTRINGKEY As String = "ResultsYSection"

Public Function Add(m As cResultsYSection, i As Integer) As Boolean
    On Error GoTo AddErr
    mCol.Add m, RESULTSYSECTIONSTRINGKEY & i
    Set m = Nothing
    Add = True
    Exit Function
AddErr:
    Add = False
    Call RaiseError(MyUnhandledError, "colResultsYSection: Function Add")
End Function

Public Function Remove(i As Integer) As Boolean
    On Error GoTo RemoveErr
    mCol.Remove (RESULTSYSECTIONSTRINGKEY & i)
    Remove = True
    Exit Function
RemoveErr:
    Remove = False
    Call RaiseError(MyUnhandledError, "colResultsYSection: Function Remove")
End Function

Public Property Get Item(i As Integer) As cResultsYSection
    On Error GoTo ItemGetErr
    Set Item = mCol(RESULTSYSECTIONSTRINGKEY & i)
    Exit Property
ItemGetErr:
    Call RaiseError(MyUnhandledError, "colResultsYSection: Item Property Get")
End Property

Public Property Get Count() As Long
    On Error GoTo CountGetErr
    Count = mCol.Count
    Exit Property
CountGetErr:
    Call RaiseError(MyUnhandledError, "colResultsYSection: Count Property Get")
End Property

Public Property Get NewEnum() As IUnknown
    On Error GoTo NewEnumGetErr
    Set NewEnum = mCol.[_NewEnum]
    Exit Property
NewEnumGetErr:
    Call RaiseError(MyUnhandledError, "colResultsYSection: NewEnum Property Get")
End Property

Public Function Clone() As colResultsYSection
    On Error GoTo CloneErr
    Dim obj As cResultsYSection
    Set Clone = New colResultsYSection
    For Each obj In mCol
        Clone.Add obj.Clone, obj.Clone.Index
    Next obj
    Exit Function
CloneErr:
    Call RaiseError(MyUnhandledError, "colResultsYSection: Function Clone")
End Function

Private Sub Class_Initialize()
    Set mCol = New Collection
End Sub

Private Sub Class_Terminate()
    Set mCol = Nothing
End Sub


