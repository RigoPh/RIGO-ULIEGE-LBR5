VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "colLoadCase"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
Private mCol As Collection
Private Const LOADCASESTRINGKEY As String = "LoadCase"

Public Function Add(m As cLoadCase, i As Integer) As Boolean
    On Error GoTo AddErr
    mCol.Add m, LOADCASESTRINGKEY & i
    Set m = Nothing
    Add = True
    Exit Function
AddErr:
    Add = False
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colLoadCase: Function Add")
End Function

Public Function Remove(i As Integer) As Boolean
    On Error GoTo RemoveErr
    mCol.Remove (LOADCASESTRINGKEY & i)
    Remove = True
    Exit Function
RemoveErr:
    Remove = False
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colLoadCase: Function Remove")
End Function

Public Property Get Item(i As Integer) As cLoadCase
    On Error GoTo ItemGetErr
    Set Item = mCol(LOADCASESTRINGKEY & i)
    Exit Property
ItemGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colLoadCase: Item Property Get")
End Property

Public Property Get Count() As Long
    On Error GoTo CountGetErr
    Count = mCol.Count
    Exit Property
CountGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colLoadCase: Count Property Get")
End Property

Public Property Get NewEnum() As IUnknown
Attribute NewEnum.VB_UserMemId = -4
Attribute NewEnum.VB_MemberFlags = "40"
    On Error GoTo NewEnumGetErr
    Set NewEnum = mCol.[_NewEnum]
    Exit Property
NewEnumGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colLoadCase: NewEnum Property Get")
End Property

Public Function GetActiveLoadCasesNo() As Integer
    Dim m As cLoadCase
    GetActiveLoadCasesNo = 0
    For Each m In mCol
        If m.State = IsOn Then
            GetActiveLoadCasesNo = GetActiveLoadCasesNo + 1
        End If
    Next m
End Function

Public Function Clone() As colLoadCase
    On Error GoTo CloneErr
    Dim OBJ As cLoadCase
    Set Clone = New colLoadCase
    For Each OBJ In mCol
        Clone.Add OBJ.Clone, OBJ.Clone.index
    Next OBJ
    Exit Function
CloneErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colLoadCase: Function Clone")
End Function

Public Function WriteASCII(ByVal iNoOfFile As Integer)
    On Error GoTo WriteASCIIErr
    Dim cLoadCase As cLoadCase
    With Me
        Put #iNoOfFile, , CInt(.Count)
        For Each cLoadCase In Me
            cLoadCase.WriteASCII iNoOfFile
        Next cLoadCase
    End With
    Exit Function
WriteASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colLoadCase: Function WriteASCII")
End Function

Public Function ReadASCII(ByVal iNoOfFile As Integer)
    On Error GoTo ReadASCIIErr
    Dim i As Integer, ii As Integer
    Dim cLoadCase As cLoadCase
    With Me
        Get #iNoOfFile, , ii
        For i = 1 To ii
            Set cLoadCase = New cLoadCase
            cLoadCase.ReadASCII iNoOfFile
            Me.Add cLoadCase, cLoadCase.index
            Set cLoadCase = Nothing
        Next i
    End With
    Exit Function
ReadASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colLoadCase: Function ReadASCII")
End Function

Private Sub Class_Initialize()
    Set mCol = New Collection
End Sub

Private Sub Class_terminate()
    Set mCol = Nothing
End Sub

