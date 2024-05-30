VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "colUpdatedScantlings"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
Private mCol As Collection
Private Const SCANTLINGSSTRINGKEY As String = "Scantlings"

Public Property Get col() As Collection
    On Error GoTo ColGetErr
    Set col = mCol
    Exit Property
ColGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colUpdatedScantlings: Col Property Get")
End Property

Public Function Add(m As cScantlings, i As Integer) As Boolean
    On Error GoTo AddErr
    mCol.Add m, SCANTLINGSSTRINGKEY & i
    Set m = Nothing
    Add = True
    Exit Function
AddErr:
    Add = False
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colUpdatedScantlings: Function Add")
End Function

Public Function Remove(i As Integer) As Boolean
    On Error GoTo RemoveErr
    mCol.Remove (SCANTLINGSSTRINGKEY & i)
    Remove = True
    Exit Function
RemoveErr:
    Remove = False
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colUpdatedScantlings: Function Remove")
End Function

Public Property Get Item(i As Integer) As cScantlings
    On Error GoTo ItemGetErr
    Set Item = mCol(SCANTLINGSSTRINGKEY & i)
    Exit Property
ItemGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colUpdatedScantlings: Item Property Get")
End Property

Public Property Get Count() As Long
    On Error GoTo CountGetErr
    Count = mCol.Count
    Exit Property
CountGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colUpdatedScantlings: Count Property Get")
End Property

Public Property Get NewEnum() As IUnknown
    On Error GoTo NewEnumGetErr
    Set NewEnum = mCol.[_NewEnum]
    Exit Property
NewEnumGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colUpdatedScantlings: NewEnum Property Get")
End Property

Public Function Clone() As colUpdatedScantlings
    On Error GoTo CloneErr
    Dim OBJ As cScantlings
    Set Clone = New colUpdatedScantlings
    For Each OBJ In mCol
        Clone.Add OBJ.Clone, OBJ.Clone.index
    Next OBJ
    Exit Function
CloneErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colUpdatedScantlings: Function Clone")
End Function

Public Function WriteASCII(ByVal iNoOfFile As Integer)
    On Error GoTo WriteASCIIErr
    Dim cScantlings As cScantlings
    With Me
        Put #iNoOfFile, , CInt(.Count)
        For Each cScantlings In Me
            cScantlings.WriteASCII iNoOfFile
        Next cScantlings
    End With
    Exit Function
WriteASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colUpdatedScantlings: Function WriteASCII")
End Function

Public Function ReadASCII(ByVal iNoOfFile As Integer)
    On Error GoTo ReadASCIIErr
    Dim i As Integer, ii As Integer
    Dim cScantlings As cScantlings
    With Me
        Get #iNoOfFile, , ii
        For i = 1 To ii
            Set cScantlings = New cScantlings
            cScantlings.ReadASCII iNoOfFile
            Me.Add cScantlings, cScantlings.index
            Set cScantlings = Nothing
        Next i
    End With
    Exit Function
ReadASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colUpdatedScantlings: Function ReadASCII")
End Function

Private Sub Class_Initialize()
    Set mCol = New Collection
End Sub

Private Sub Class_terminate()
    Set mCol = Nothing
End Sub

