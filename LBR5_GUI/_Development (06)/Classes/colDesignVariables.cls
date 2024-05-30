VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "colDesignVariables"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
Private mCol As Collection
Private Const DESIGNVARIABLESTRINGKEY As String = "DesignVariable"

Public Function Add(m As cDesignVariables, i As Integer) As Boolean
    On Error GoTo AddErr
    mCol.Add m, DESIGNVARIABLESTRINGKEY & i
    Set m = Nothing
    Add = True
    Exit Function
AddErr:
    Add = False
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colDesignVariables: Function Add")
End Function

Public Function Remove(i As Integer) As Boolean
    On Error GoTo RemoveErr
    mCol.Remove (DESIGNVARIABLESTRINGKEY & i)
    Remove = True
    Exit Function
RemoveErr:
    Remove = False
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colDesignVariables: Function Remove")
End Function

Public Property Get Item(i As Integer) As cDesignVariables
    On Error GoTo ItemGetErr
    Set Item = mCol(DESIGNVARIABLESTRINGKEY & i)
    Exit Property
ItemGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colDesignVariables: Item Property Get")
End Property

Public Property Get Count() As Long
    On Error GoTo CountGetErr
    Count = mCol.Count
    Exit Property
CountGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colDesignVariables: Count Property Get")
End Property

Public Property Get NewEnum() As IUnknown
Attribute NewEnum.VB_UserMemId = -4
Attribute NewEnum.VB_MemberFlags = "40"
    On Error GoTo NewEnumGetErr
    Set NewEnum = mCol.[_NewEnum]
    Exit Property
NewEnumGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colDesignVariables: NewEnum Property Get")
End Property

Public Function Clone() As colDesignVariables
    On Error GoTo CloneErr
    Dim OBJ As cDesignVariables
    Set Clone = New colDesignVariables
    For Each OBJ In mCol
        Clone.Add OBJ.Clone, OBJ.Clone.index
    Next OBJ
    Exit Function
CloneErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colDesignVariables: Function Clone")
End Function

Public Function WriteASCII(ByVal iNoOfFile As Integer)
    On Error GoTo WriteASCIIErr
    Dim cDesignVariables As cDesignVariables
    With Me
        Put #iNoOfFile, , CInt(.Count)
        For Each cDesignVariables In Me
            cDesignVariables.WriteASCII iNoOfFile
        Next cDesignVariables
    End With
    Exit Function
WriteASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colDesignVariables: Function WriteASCII")
End Function

Public Function ReadASCII(ByVal iNoOfFile As Integer)
    On Error GoTo ReadASCIIErr
    Dim i As Integer, ii As Integer
    Dim cDesignVariables As cDesignVariables
    With Me
        Get #iNoOfFile, , ii
        For i = 1 To ii
            Set cDesignVariables = New cDesignVariables
            cDesignVariables.ReadASCII iNoOfFile
            Me.Add cDesignVariables, cDesignVariables.index
            Set cDesignVariables = Nothing
        Next i
    End With
    Exit Function
ReadASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colDesignVariables: Function ReadASCII")
End Function

Public Function ActiveVariablesCount() As Integer
    On Error GoTo ActiveVariablesCountErr
    Dim oDes As cDesignVariables
    ActiveVariablesCount = 0
    For Each oDes In Me
        If oDes.Active = True Then
            ActiveVariablesCount = ActiveVariablesCount + 1
        End If
    Next oDes
    Exit Function
ActiveVariablesCountErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colDesignVariables: Function ActiveVariablesCount")
End Function

Private Sub Class_Initialize()
    Set mCol = New Collection
End Sub

Private Sub Class_terminate()
    Set mCol = Nothing
End Sub
