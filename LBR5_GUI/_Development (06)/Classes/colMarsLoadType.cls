VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "colMarsLoadType"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
Private mCol As Collection
Private Const MARSLOADTYPESTRINGKEY As String = "MarsLoadType"

Public Function Add(m As cMarsLoadType, i As Integer) As Boolean
    On Error GoTo AddErr
    mCol.Add m, MARSLOADTYPESTRINGKEY & i
    Set m = Nothing
    Add = True
    Exit Function
AddErr:
    Add = False
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colMarsLoadType: Function Add")
End Function

Public Function Remove(i As Integer) As Boolean
    On Error GoTo RemoveErr
    mCol.Remove (MARSLOADTYPESTRINGKEY & i)
    Remove = True
    Exit Function
RemoveErr:
    Remove = False
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colMarsLoadType: Function Remove")
End Function

Public Property Get Item(i As Integer) As cMarsLoadType
    On Error GoTo ItemGetErr
    Set Item = mCol(MARSLOADTYPESTRINGKEY & i)
    Exit Property
ItemGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colMarsLoadType: Item Property Get")
End Property

Public Property Get Count() As Long
    On Error GoTo CountGetErr
    Count = mCol.Count
    Exit Property
CountGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colMarsLoadType: Count Property Get")
End Property

Public Property Get NewEnum() As IUnknown
Attribute NewEnum.VB_UserMemId = -4
    On Error GoTo NewEnumGetErr
    Set NewEnum = mCol.[_NewEnum]
    Exit Property
NewEnumGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colMarsLoadType: NewEnum Property Get")
End Property

Public Function Clone() As colMarsLoadType
    On Error GoTo CloneErr
    Dim OBJ As cMarsLoadType
    Set Clone = New colMarsLoadType
    For Each OBJ In mCol
        Clone.Add OBJ.Clone, OBJ.Clone.index
    Next OBJ
    Exit Function
CloneErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colMarsLoadType: Function Clone")
End Function

Public Function WriteASCII(ByVal iNoOfFile As Integer)
    On Error GoTo WriteASCIIErr
    Dim cMarsLoadType As cMarsLoadType
    With Me
        Put #iNoOfFile, , CInt(.Count)
        For Each cMarsLoadType In Me
            cMarsLoadType.WriteASCII iNoOfFile
        Next cMarsLoadType
    End With
    Exit Function
WriteASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colMarsLoadType: Function WriteASCII")
End Function

Private Sub Class_Initialize()
    Set mCol = New Collection
End Sub

Private Sub Class_terminate()
    Set mCol = Nothing
End Sub
