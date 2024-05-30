VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cIndex"
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
Private mvarNumber As Long

Public Property Let index(ByVal vData As Long)
    On Error GoTo IndexLetErr
    mvarIndex = vData
    Exit Property
IndexLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cIndex: Index Property Let")
End Property

Public Property Get index() As Long
    On Error GoTo IndexGetErr
    index = mvarIndex
    Exit Property
IndexGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cIndex: Index Property Get")
End Property

Public Property Let Number(ByVal vData As Long)
    On Error GoTo NumberLetErr
    mvarNumber = vData
    Exit Property
NumberLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cNumber: Number Property Let")
End Property

Public Property Get Number() As Long
    On Error GoTo NumberGetErr
    Number = mvarNumber
    Exit Property
NumberGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cNumber: Number Property Get")
End Property

Public Function Clone() As cIndex
    On Error GoTo CloneErr
    Set Clone = New cIndex
    Clone.index = Me.index
    Clone.Number = Me.Number
    Exit Function
CloneErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cIndex: Function Clone")
End Function

Public Function WriteASCII(ByVal iNoOfFile As Integer)
    On Error GoTo WriteASCIIErr
    With Me
        Put #iNoOfFile, , .index
        Put #iNoOfFile, , .Number
    End With
    Exit Function
WriteASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cIndex: Function WriteASCII")
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

