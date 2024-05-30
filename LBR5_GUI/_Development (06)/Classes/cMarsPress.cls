VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cMarsPress"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
#Const DebugMode = 0
#If DebugMode Then
    Private mlClassDebugID As Long
#End If

Private mvarIn_Node As Double
Private mvarOut_Node As Double

Public Property Let In_Node(ByVal vData As Double)
    On Error GoTo In_NodeLetErr
    mvarIn_Node = vData
    Exit Property
In_NodeLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMarsPress: In_Node Property Let")
End Property

Public Property Get In_Node() As Double
    On Error GoTo In_NodeGetErr
    In_Node = mvarIn_Node
    Exit Property
In_NodeGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMarsPress: In_Node Property Get")
End Property

Public Property Let Out_Node(ByVal vData As Double)
    On Error GoTo Out_NodeLetErr
    mvarOut_Node = vData
    Exit Property
Out_NodeLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMarsPress: Out_Node Property Let")
End Property

Public Property Get Out_Node() As Double
    On Error GoTo Out_NodeGetErr
    Out_Node = mvarOut_Node
    Exit Property
Out_NodeGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMarsPress: Out_Node Property Get")
End Property

Public Function Clone() As cMarsPress
    On Error GoTo CloneErr
    Set Clone = New cMarsPress
    With Clone
        .In_Node = Me.In_Node
        .Out_Node = Me.Out_Node
    End With
    Exit Function
CloneErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMarsPress: Function Clone")
End Function

Public Function WriteASCII(ByVal iNoOfFile As Integer)
    On Error GoTo WriteASCIIErr
    With Me
        Put #iNoOfFile, , .In_Node
        Put #iNoOfFile, , .Out_Node
    End With
    Exit Function
WriteASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMarsPress: Function WriteASCII")
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
