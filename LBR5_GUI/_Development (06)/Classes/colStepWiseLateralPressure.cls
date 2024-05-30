VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "colStepWiseLateralPressure"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
Private mCol As Collection
Private Const STEPWISESTRINGKEY As String = "StepWiseLateralPressure"

Public Function Add(m As cStepWiseLateralPressure, i As Integer) As Boolean
    On Error GoTo AddErr
    mCol.Add m, STEPWISESTRINGKEY & i
    Set m = Nothing
    Add = True
    Exit Function
AddErr:
    Add = False
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colStepWiseLateralPressure: Function Add")
End Function

Public Function Remove(i As Integer) As Boolean
    On Error GoTo RemoveErr
    mCol.Remove (STEPWISESTRINGKEY & i)
    Remove = True
    Exit Function
RemoveErr:
    Remove = False
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colStepWiseLateralPressure: Function Remove")
End Function

Public Property Get Item(i As Integer) As cStepWiseLateralPressure
    On Error GoTo ItemGetErr
    Set Item = mCol(STEPWISESTRINGKEY & i)
    Exit Property
ItemGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colStepWiseLateralPressure: Item Property Get")
End Property

Public Property Get Count() As Long
    On Error GoTo CountGetErr
    Count = mCol.Count
    Exit Property
CountGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colStepWiseLateralPressure: Count Property Get")
End Property

Public Property Get NewEnum() As IUnknown
Attribute NewEnum.VB_UserMemId = -4
Attribute NewEnum.VB_MemberFlags = "40"
    On Error GoTo NewEnumGetErr
    Set NewEnum = mCol.[_NewEnum]
    Exit Property
NewEnumGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colStepWiseLateralPressure: NewEnum Property Get")
End Property

Public Function Clone() As colStepWiseLateralPressure
    On Error GoTo CloneErr
    Dim OBJ As cStepWiseLateralPressure
    Set Clone = New colStepWiseLateralPressure
    For Each OBJ In mCol
        Clone.Add OBJ.Clone, OBJ.Clone.index
    Next OBJ
    Exit Function
CloneErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colStepWiseLateralPressure: Function Clone")
End Function

Public Function WriteASCII(ByVal iNoOfFile As Integer)
    On Error GoTo WriteASCIIErr
    Dim cStepWiseLateralPressure As cStepWiseLateralPressure
    With Me
        Put #iNoOfFile, , CInt(.Count)
        For Each cStepWiseLateralPressure In Me
            cStepWiseLateralPressure.WriteASCII iNoOfFile
        Next cStepWiseLateralPressure
    End With
    Exit Function
WriteASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colStepWiseLateralPressure: Function WriteASCII")
End Function

Public Function ReadASCII(ByVal iNoOfFile As Integer)
    On Error GoTo ReadASCIIErr
    Dim i As Integer, ii As Integer
    Dim cStepWiseLateralPressure As cStepWiseLateralPressure
    With Me
        Get #iNoOfFile, , ii
        For i = 1 To ii
            Set cStepWiseLateralPressure = New cStepWiseLateralPressure
            cStepWiseLateralPressure.ReadASCII iNoOfFile
            Me.Add cStepWiseLateralPressure, cStepWiseLateralPressure.index
            Set cStepWiseLateralPressure = Nothing
        Next i
    End With
    Exit Function
ReadASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colStepWiseLateralPressure: Function ReadASCII")
End Function

Private Sub Class_Initialize()
    Set mCol = New Collection
End Sub

Private Sub Class_terminate()
    Set mCol = Nothing
End Sub



