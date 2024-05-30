VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "colEqualityRestrictions"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
Private mCol As Collection
Private Const EQUALITYRESTRICTIONSTRINGKEY As String = "EqualityRestriction"

Public Function Add(m As cEqualityRestrictions, i As Integer) As Boolean
    On Error GoTo AddErr
    mCol.Add m, EQUALITYRESTRICTIONSTRINGKEY & i
    Set m = Nothing
    Add = True
    Exit Function
AddErr:
    Add = False
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colEqualityRestrictions: Function Add")
End Function

Public Function AddFromString(ByRef s As String)
    On Error GoTo AddFromStringErr
    Dim oEqu As New cEqualityRestrictions
    oEqu.index = Me.Count + 1
    oEqu.GetFromString (s)
    mCol.Add oEqu, EQUALITYRESTRICTIONSTRINGKEY & oEqu.index
    Set oEqu = Nothing
    Exit Function
AddFromStringErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colEqualityRestrictions: Function AddFromString")
End Function

Public Function Remove(i As Integer) As Boolean
    On Error GoTo RemoveErr
    mCol.Remove (EQUALITYRESTRICTIONSTRINGKEY & i)
    Remove = True
    Exit Function
RemoveErr:
    Remove = False
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colEqualityRestrictions: Function Remove")
End Function

Public Property Get Item(i As Integer) As cEqualityRestrictions
    On Error GoTo ItemGetErr
    Set Item = mCol(EQUALITYRESTRICTIONSTRINGKEY & i)
    Exit Property
ItemGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colEqualityRestrictions: Item Property Get")
End Property

Public Property Get Count() As Long
    On Error GoTo CountGetErr
    Count = mCol.Count
    Exit Property
CountGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colEqualityRestrictions: Count Property Get")
End Property

Public Property Get NewEnum() As IUnknown
Attribute NewEnum.VB_UserMemId = -4
Attribute NewEnum.VB_MemberFlags = "40"
    On Error GoTo NewEnumGetErr
    Set NewEnum = mCol.[_NewEnum]
    Exit Property
NewEnumGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colEqualityRestrictions: NewEnum Property Get")
End Property

Public Function Clone() As colEqualityRestrictions
    On Error GoTo CloneErr
    Dim OBJ As cEqualityRestrictions
    Set Clone = New colEqualityRestrictions
    For Each OBJ In mCol
        Clone.Add OBJ.Clone, OBJ.Clone.index
    Next OBJ
    Exit Function
CloneErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colEqualityRestrictions: Function Clone")
End Function

Public Function WriteASCII(ByVal iNoOfFile As Integer)
    On Error GoTo WriteASCIIErr
    Dim cEqualityRestrictions As cEqualityRestrictions
    With Me
        Put #iNoOfFile, , CInt(.Count)
        For Each cEqualityRestrictions In Me
            cEqualityRestrictions.WriteASCII iNoOfFile
        Next cEqualityRestrictions
    End With
    Exit Function
WriteASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colEqualityRestrictions: Function WriteASCII")
End Function

Public Function ReadASCII(ByVal iNoOfFile As Integer)
    On Error GoTo ReadASCIIErr
    Dim i As Integer, ii As Integer
    Dim cEqualityRestrictions As cEqualityRestrictions
    With Me
        Get #iNoOfFile, , ii
        For i = 1 To ii
            Set cEqualityRestrictions = New cEqualityRestrictions
            cEqualityRestrictions.ReadASCII iNoOfFile
            Me.Add cEqualityRestrictions, cEqualityRestrictions.index
            Set cEqualityRestrictions = Nothing
        Next i
    End With
    Exit Function
ReadASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colEqualityRestrictions: Function ReadASCII")
End Function

Private Sub Class_Initialize()
    Set mCol = New Collection
End Sub

Private Sub Class_terminate()
    Set mCol = Nothing
End Sub
