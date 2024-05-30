VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cGeometricalConstraints"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
#Const DebugMode = 0
#If DebugMode Then
    Private mlClassDebugID As Long
#End If
Private mvarIndex As Integer
Private mvarCode As Integer

Public Property Let index(ByVal vData As Integer)
    On Error GoTo IndexLetErr
    mvarIndex = vData
    Exit Property
IndexLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGeometricalConstraints: Index Property Let")
End Property

Public Property Get index() As Integer
    On Error GoTo IndexGetErr
    index = mvarIndex
    Exit Property
IndexGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGeometricalConstraints: Index Property Get")
End Property

Public Property Let code(ByVal vData As Integer)
    On Error GoTo CodeLetErr
    mvarCode = vData
    Exit Property
CodeLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGeometricalConstraints: Code Property Let")
End Property

Public Property Get code() As Integer
    On Error GoTo CodeGetErr
    code = mvarCode
    Exit Property
CodeGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGeometricalConstraints: Code Property Get")
End Property

Public Function Clone() As cGeometricalConstraints
    On Error GoTo CloneErr
    Set Clone = New cGeometricalConstraints
    Clone.index = Me.index
    Clone.code = Me.code
    Exit Function
CloneErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGeometricalConstraints: Function Clone")
End Function

Public Function WriteASCII(ByVal iNoOfFile As Integer)
    On Error GoTo WriteASCIIErr
    With Me
        Put #iNoOfFile, , .index
        Put #iNoOfFile, , .code
    End With
    Exit Function
WriteASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGeometricalConstraints: Function WriteASCII")
End Function

Public Function ReadASCII(ByVal iNoOfFile As Integer)
    On Error GoTo ReadASCIIErr
    Dim ii As Integer, dd As Double
    With Me
        Get #iNoOfFile, , ii: .index = ii
        Get #iNoOfFile, , ii: .code = ii
    End With
    Exit Function
ReadASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGeometricalConstraints: Function ReadASCII")
End Function

Private Sub Class_Initialize()
    #If DebugMode Then
        mlClassDebugID = GetNextClassDebugID()
        Debug.Print "'" & TypeName(Me) & "' instance " & mlClassDebugID & " created"
    #End If
End Sub

Private Sub Class_terminate()
    #If DebugMode Then
        Debug.Print "'" & TypeName(Me) & "' instance " & CStr(mlClassDebugID) & " is terminating"
    #End If
End Sub

#If DebugMode Then
    Public Property Get ClassDebugID()
        ClassDebugID = mlClassDebugID
    End Property
#End If

