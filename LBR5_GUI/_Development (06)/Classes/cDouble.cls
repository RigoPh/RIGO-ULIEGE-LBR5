VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cDouble"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
#Const DebugMode = 0
#If DebugMode Then
    Private mlClassDebugID As Long
#End If
' Class Members
Private mvarIndex As Long
Private mvarLabel As String
Private mvarValue As Double

Public Property Let index(ByVal vData As Long)
    On Error GoTo IndexLetErr
    mvarIndex = vData
    Exit Property
IndexLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDouble: Index Property Let")
End Property

Public Property Get index() As Long
    On Error GoTo IndexGetErr
    index = mvarIndex
    Exit Property
IndexGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDouble: Index Property Get")
End Property

Public Property Get Label() As String
    On Error GoTo LabelGetErr
    Label = mvarLabel
    Exit Property
LabelGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDouble: Label Property Get")
End Property

Public Property Let Label(ByVal vData As String)
    On Error GoTo LabelSetErr
    mvarLabel = vData
    Exit Property
LabelSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDouble: Label Property Set")
End Property

Public Property Let Value(ByVal vData As Double)
    On Error GoTo ValueLetErr
    mvarValue = vData
    Exit Property
ValueLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDouble: Value Property Let")
End Property

Public Property Get Value() As Double
    On Error GoTo ValueGetErr
    Value = mvarValue
    Exit Property
ValueGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDouble: Value Property Get")
End Property

Public Function Clone() As cDouble
    On Error GoTo CloneErr
    Set Clone = New cDouble
    With Clone
        .index = Me.index
        .Label = Me.Label
        .Value = Me.Value
    End With
    Exit Function
CloneErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDouble: Function Clone")
End Function

Public Function WriteASCII(ByVal iNoOfFile As Integer)
    On Error GoTo WriteASCIIErr
    Dim ss As String * 10
    With Me
        Put #iNoOfFile, , .index
        ss = .Label
        Put #iNoOfFile, , ss
        Put #iNoOfFile, , .Value
    End With
    Exit Function
WriteASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDouble: Function WriteASCII")
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


