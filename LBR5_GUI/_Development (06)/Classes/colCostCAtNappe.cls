VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "colCostCAtNappe"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
Private mCol As Collection
Private Const COSTCATNAPPESTRINGKEY As String = "CostCAtNappe"

Public Function Add(m As cCostCAtNappe, i As Integer) As Boolean
    On Error GoTo AddErr
    mCol.Add m, COSTCATNAPPESTRINGKEY & i
    Set m = Nothing
    Add = True
    Exit Function
AddErr:
    Add = False
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colCostCAtNappe: Function Add")
End Function

Public Function Remove(i As Integer) As Boolean
    On Error GoTo RemoveErr
    mCol.Remove (COSTCATNAPPESTRINGKEY & i)
    Remove = True
    Exit Function
RemoveErr:
    Remove = False
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colCostCAtNappe: Function Remove")
End Function

Public Property Get Item(i As Integer) As cCostCAtNappe
    On Error GoTo ItemGetErr
    Set Item = mCol(COSTCATNAPPESTRINGKEY & i)
    Exit Property
ItemGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colCostCAtNappe: Item Property Get")
End Property

Public Property Get Count() As Long
    On Error GoTo CountGetErr
    Count = mCol.Count
    Exit Property
CountGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colCostCAtNappe: Count Property Get")
End Property

Public Property Get NewEnum() As IUnknown
Attribute NewEnum.VB_UserMemId = -4
    On Error GoTo NewEnumGetErr
    Set NewEnum = mCol.[_NewEnum]
    Exit Property
NewEnumGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colCostCAtNappe: NewEnum Property Get")
End Property

Public Function Renum() As Boolean
    On Error GoTo RenumErr
    Dim mCol1 As New Collection
    Dim OBJ As cCostCAtNappe
    Dim i As Integer
    i = 0
    For Each OBJ In mCol
        i = i + 1
        OBJ.index = i
        mCol1.Add OBJ, COSTCATNAPPESTRINGKEY & i
    Next OBJ
    Set mCol = mCol1
    Exit Function
RenumErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colCostCAtNappe: Function Renum")
End Function

Public Function Clone() As colCostCAtNappe
    On Error GoTo CloneErr
    Dim OBJ As cCostCAtNappe
    Set Clone = New colCostCAtNappe
    For Each OBJ In mCol
        Clone.Add OBJ.Clone, OBJ.Clone.index
    Next OBJ
    Exit Function
CloneErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colCostCAtNappe: Function Clone")
End Function

Public Function WriteASCII(ByVal iNoOfFile As Integer)
    On Error GoTo WriteASCIIErr
    Dim cCostCAtNappe As cCostCAtNappe
    With Me
        Put #iNoOfFile, , CInt(.Count)
        For Each cCostCAtNappe In Me
            cCostCAtNappe.WriteASCII iNoOfFile
        Next cCostCAtNappe
    End With
    Exit Function
WriteASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colCostCAtNappe: Function WriteASCII")
End Function

Private Sub Class_Initialize()
    Set mCol = New Collection
End Sub

Private Sub Class_terminate()
    Set mCol = Nothing
End Sub
