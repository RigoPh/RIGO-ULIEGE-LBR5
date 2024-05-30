VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "colCostCAtDHull"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
Private mCol As Collection
Private Const COSTCATDHULLSTRINGKEY As String = "CostCAtDHull"

Public Function Add(m As cCostCAtDHull, i As Integer) As Boolean
    On Error GoTo AddErr
    mCol.Add m, COSTCATDHULLSTRINGKEY & i
    Set m = Nothing
    Add = True
    Exit Function
AddErr:
    Add = False
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colCostCAtDHull: Function Add")
End Function

Public Function Remove(i As Integer) As Boolean
    On Error GoTo RemoveErr
    mCol.Remove (COSTCATDHULLSTRINGKEY & i)
    Remove = True
    Exit Function
RemoveErr:
    Remove = False
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colCostCAtDHull: Function Remove")
End Function

Public Property Get Item(i As Integer) As cCostCAtDHull
    On Error GoTo ItemGetErr
    Set Item = mCol(COSTCATDHULLSTRINGKEY & i)
    Exit Property
ItemGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colCostCAtDHull: Item Property Get")
End Property

Public Property Get Count() As Long
    On Error GoTo CountGetErr
    Count = mCol.Count
    Exit Property
CountGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colCostCAtDHull: Count Property Get")
End Property

Public Property Get NewEnum() As IUnknown
Attribute NewEnum.VB_UserMemId = -4
    On Error GoTo NewEnumGetErr
    Set NewEnum = mCol.[_NewEnum]
    Exit Property
NewEnumGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colCostCAtDHull: NewEnum Property Get")
End Property

Public Function Renum() As Boolean
    On Error GoTo RenumErr
    Dim mCol1 As New Collection
    Dim OBJ As cCostCAtDHull
    Dim i As Integer
    i = 0
    For Each OBJ In mCol
        i = i + 1
        OBJ.index = i
        mCol1.Add OBJ, COSTCATDHULLSTRINGKEY & i
    Next OBJ
    Set mCol = mCol1
    Exit Function
RenumErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colCostCAtDHull: Function Renum")
End Function

Public Function Clone() As colCostCAtDHull
    On Error GoTo CloneErr
    Dim OBJ As cCostCAtDHull
    Set Clone = New colCostCAtDHull
    For Each OBJ In mCol
        Clone.Add OBJ.Clone, OBJ.Clone.index
    Next OBJ
    Exit Function
CloneErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colCostCAtDHull: Function Clone")
End Function

Public Function WriteASCII(ByVal iNoOfFile As Integer)
    On Error GoTo WriteASCIIErr
    Dim cCostCAtDHull As cCostCAtDHull
    With Me
        Put #iNoOfFile, , CInt(.Count)
        For Each cCostCAtDHull In Me
            cCostCAtDHull.WriteASCII iNoOfFile
        Next cCostCAtDHull
    End With
    Exit Function
WriteASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colCostCAtDHull: Function WriteASCII")
End Function

Private Sub Class_Initialize()
    Set mCol = New Collection
End Sub

Private Sub Class_terminate()
    Set mCol = Nothing
End Sub

