VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "colStrakes"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
Private mCol As Collection
Private Const STRAKESTRINGKEY As String = "Strake"

Public Property Get col() As Collection
    On Error GoTo ColGetErr
    Set col = mCol
    Exit Property
ColGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colStrakes: Col Property Get")
End Property

Public Function Add(m As cStrake, i As Integer) As Boolean
    On Error GoTo AddErr
    mCol.Add m, STRAKESTRINGKEY & i
    Set m = Nothing
    Add = True
    Exit Function
AddErr:
    Add = False
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colStrakes: Function Add")
End Function

Public Function Remove(i As Integer) As Boolean
    On Error GoTo RemoveErr
    mCol.Remove (STRAKESTRINGKEY & i)
    Remove = True
    Exit Function
RemoveErr:
    Remove = False
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colStrakes: Function Remove")
End Function

Public Property Get Item(i As Integer) As cStrake
    On Error GoTo ItemGetErr
    Set Item = mCol(STRAKESTRINGKEY & i)
    Exit Property
ItemGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colStrakes: Item Property Get")
End Property

Public Property Get Count() As Long
    On Error GoTo CountGetErr
    Count = mCol.Count
    Exit Property
CountGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colStrakes: Count Property Get")
End Property

Public Property Get NewEnum() As IUnknown
Attribute NewEnum.VB_UserMemId = -4
    On Error GoTo NewEnumGetErr
    Set NewEnum = mCol.[_NewEnum]
    Exit Property
NewEnumGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colStrakes: NewEnum Property Get")
End Property

Public Function GetEquivalentGrossThickness() As Double
    On Error GoTo GetEquivalentGrossThicknessErr
    Dim cStrake As cStrake
    GetEquivalentGrossThickness = 0
    For Each cStrake In Me
        GetEquivalentGrossThickness = GetEquivalentGrossThickness + cStrake.GrossThickness * cStrake.WidthRatio
    Next cStrake
    Exit Function
GetEquivalentGrossThicknessErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cStrake: Function GetEquivalentGrossThickness")
End Function

Public Function GetEquivalentNetThickness() As Double
    On Error GoTo GetEquivalentNetThicknessErr
    Dim cStrake As cStrake
    GetEquivalentNetThickness = 0
    For Each cStrake In Me
        GetEquivalentNetThickness = GetEquivalentNetThickness + cStrake.NetThickness * cStrake.WidthRatio
    Next cStrake
    Exit Function
GetEquivalentNetThicknessErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cStrake: Function GetEquivalentNetThickness")
End Function

Public Function Clone() As colStrakes
    On Error GoTo CloneErr
    Dim OBJ As cStrake
    Set Clone = New colStrakes
    For Each OBJ In mCol
        Clone.Add OBJ.Clone, OBJ.Clone.index
    Next OBJ
    Exit Function
CloneErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colStrakes: Function Clone")
End Function

Private Sub Class_Initialize()
    Set mCol = New Collection
End Sub

Private Sub Class_terminate()
    Set mCol = Nothing
End Sub
