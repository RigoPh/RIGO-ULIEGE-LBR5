VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "colStructuralConstraints"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
Private mCol As Collection
Private Const STRUCTURALCONSTRAINTSSTRINGKEY As String = "StructuralConstraint"

Public Function Add(m As cStructuralConstraints, i As Integer) As Boolean
    On Error GoTo AddErr
    mCol.Add m, STRUCTURALCONSTRAINTSSTRINGKEY & i
    Set m = Nothing
    Add = True
    Exit Function
AddErr:
    Add = False
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colStructuralConstraints: Function Add")
End Function

Public Function Remove(i As Integer) As Boolean
    On Error GoTo RemoveErr
    mCol.Remove (STRUCTURALCONSTRAINTSSTRINGKEY & i)
    Remove = True
    Exit Function
RemoveErr:
    Remove = False
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colStructuralConstraints: Function Remove")
End Function

Public Property Get Item(i As Integer) As cStructuralConstraints
    On Error GoTo ItemGetErr
    Set Item = mCol(STRUCTURALCONSTRAINTSSTRINGKEY & i)
    Exit Property
ItemGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colStructuralConstraints: Item Property Get")
End Property

Public Property Get Count() As Long
    On Error GoTo CountGetErr
    Count = mCol.Count
    Exit Property
CountGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colStructuralConstraints: Count Property Get")
End Property

Public Property Get NewEnum() As IUnknown
Attribute NewEnum.VB_UserMemId = -4
Attribute NewEnum.VB_MemberFlags = "40"
    On Error GoTo NewEnumGetErr
    Set NewEnum = mCol.[_NewEnum]
    Exit Property
NewEnumGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colStructuralConstraints: NewEnum Property Get")
End Property

Public Function Clone() As colStructuralConstraints
    On Error GoTo CloneErr
    Dim OBJ As cStructuralConstraints
    Set Clone = New colStructuralConstraints
    For Each OBJ In mCol
        Clone.Add OBJ.Clone, OBJ.Clone.index
    Next OBJ
    Exit Function
CloneErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colStructuralConstraints: Function Clone")
End Function

Public Function WriteASCII(ByVal iNoOfFile As Integer)
    On Error GoTo WriteASCIIErr
    Dim cStructuralConstraints As cStructuralConstraints
    With Me
        Put #iNoOfFile, , CInt(.Count)
        For Each cStructuralConstraints In Me
            cStructuralConstraints.WriteASCII iNoOfFile
        Next cStructuralConstraints
    End With
    Exit Function
WriteASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colStructuralConstraints: Function WriteASCII")
End Function

Public Function ReadASCII(ByVal iNoOfFile As Integer)
    On Error GoTo ReadASCIIErr
    Dim i As Integer, ii As Integer
    Dim cStructuralConstraints As cStructuralConstraints
    With Me
        Get #iNoOfFile, , ii
        For i = 1 To ii
            Set cStructuralConstraints = New cStructuralConstraints
            cStructuralConstraints.ReadASCII iNoOfFile
            Me.Add cStructuralConstraints, cStructuralConstraints.index
            Set cStructuralConstraints = Nothing
        Next i
    End With
    Exit Function
ReadASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colStructuralConstraints: Function ReadASCII")
End Function

Private Sub Class_Initialize()
    Set mCol = New Collection
End Sub

Private Sub Class_terminate()
    Set mCol = Nothing
End Sub
