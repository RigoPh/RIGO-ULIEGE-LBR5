VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "colIndex"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
Private mCol As Collection
Private Const INDEXSTRINGKEY As String = "Index"

Public Property Get col() As Collection
    On Error GoTo ColGetErr
    Set col = mCol
    Exit Property
ColGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colIndex: Col Property Get")
End Property

Public Function Add(m As cIndex, i As Integer) As Boolean
    On Error GoTo AddErr
    mCol.Add m, INDEXSTRINGKEY & i
    Set m = Nothing
    Add = True
    Exit Function
AddErr:
    Add = False
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colIndex: Function Add")
End Function

Public Function Remove(i As Integer) As Boolean
    On Error GoTo RemoveErr
    mCol.Remove (INDEXSTRINGKEY & i)
    Remove = True
    Exit Function
RemoveErr:
    Remove = False
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colIndex: Function Remove")
End Function

Public Property Get Item(i As Integer) As cIndex
    On Error GoTo ItemGetErr
    Set Item = mCol(INDEXSTRINGKEY & i)
    Exit Property
ItemGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colIndex: Item Property Get")
End Property

Public Property Get Count() As Long
    On Error GoTo CountGetErr
    Count = mCol.Count
    Exit Property
CountGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colIndex: Count Property Get")
End Property

Public Property Get NewEnum() As IUnknown
Attribute NewEnum.VB_UserMemId = -4
    On Error GoTo NewEnumGetErr
    Set NewEnum = mCol.[_NewEnum]
    Exit Property
NewEnumGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colIndex: NewEnum Property Get")
End Property

Public Function Renum() As Boolean
    On Error GoTo RenumErr
    Dim mCol1 As New Collection
    Dim OBJ As cIndex
    Dim i As Integer
    i = 0
    For Each OBJ In mCol
        i = i + 1
        OBJ.index = i
        mCol1.Add OBJ, INDEXSTRINGKEY & i
    Next OBJ
    Set mCol = mCol1
    Exit Function
RenumErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colIndex: Function Renum")
End Function

Public Function WriteASCII(ByVal iNoOfFile As Integer)
    On Error GoTo WriteASCIIErr
    Dim cIndex As cIndex
    With Me
        Put #iNoOfFile, , CInt(.Count)
        For Each cIndex In Me
            cIndex.WriteASCII iNoOfFile
        Next cIndex
    End With
    Exit Function
WriteASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colIndex: Function WriteASCII")
End Function

Public Function Clone() As colIndex
    On Error GoTo CloneErr
    Dim OBJ As cIndex
    Set Clone = New colIndex
    For Each OBJ In mCol
        Clone.Add OBJ.Clone, OBJ.Clone.index
    Next OBJ
    Exit Function
CloneErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colIndex: Function Clone")
End Function

Private Sub Class_Initialize()
    Set mCol = New Collection
End Sub

Private Sub Class_terminate()
    Set mCol = Nothing
End Sub
