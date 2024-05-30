VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cStepWiseLateralPressure"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit
#Const DebugMode = 0
#If DebugMode Then
    Private mlClassDebugID As Long
#End If
Private mvarIndex As Integer
Private mvarVerticalGravityLoad As Double
Private mvarLateralPressureIn As Double
Private mvarLateralPressureOut As Double

Public Property Let LateralPressureOut(ByVal vData As Double)
    On Error GoTo LateralPressureOutLetErr
    mvarLateralPressureOut = vData
    Exit Property
LateralPressureOutLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cStepWiseLateralPressure: LateralPressureOut Property Let")
End Property

Public Property Get LateralPressureOut() As Double
    On Error GoTo LateralPressureOutGetErr
    LateralPressureOut = mvarLateralPressureOut
    Exit Property
LateralPressureOutGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cStepWiseLateralPressure: LateralPressureOut Property Get")
End Property

Public Property Let LateralPressureIn(ByVal vData As Double)
    On Error GoTo LateralPressureInLetErr:
    mvarLateralPressureIn = vData
    Exit Property
LateralPressureInLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cStepWiseLateralPressure: LateralPressureIn Property Let")
End Property

Public Property Get LateralPressureIn() As Double
    On Error GoTo LateralPressureInGetErr
    LateralPressureIn = mvarLateralPressureIn
    Exit Property
LateralPressureInGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cStepWiseLateralPressure: LateralPressureIn Property Get")
End Property

Public Property Let VerticalGravityLoad(ByVal vData As Double)
    On Error GoTo VerticalGravityLoadLetErr
    mvarVerticalGravityLoad = vData
    Exit Property
VerticalGravityLoadLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cStepWiseLateralPressure: VerticalGravityLoad Property Let")
End Property

Public Property Get VerticalGravityLoad() As Double
    On Error GoTo VerticalGravityLoadGetErr
    VerticalGravityLoad = mvarVerticalGravityLoad
    Exit Property
VerticalGravityLoadGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cStepWiseLateralPressure: VerticalGravityLoad Property Get")
End Property

Public Property Let index(ByVal vData As Integer)
    On Error GoTo IndexLetErr
    mvarIndex = vData
    Exit Property
IndexLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cStepWiseLateralPressure: Index Property Let")
End Property

Public Property Get index() As Integer
    On Error GoTo IndexGetErr
    index = mvarIndex
    Exit Property
IndexGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cStepWiseLateralPressure: Index Property Get")
End Property

Public Function ReadLBR5txtFile(ts As TextStream)
    On Error GoTo ReadLBR5txtFileErr
    Dim sLine As String
    Dim v() As Variant
    sLine = ReadLn(ts)
    GetValues 3, sLine, v
    Me.VerticalGravityLoad = Val_(v(1))
    Me.LateralPressureIn = Val_(v(2))
    Me.LateralPressureOut = Val_(v(3))
    Exit Function
ReadLBR5txtFileErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cStepWiseLateralPressure: Function ReadLBR5txtFile")
End Function

Public Function Clone() As cStepWiseLateralPressure
    On Error GoTo CloneErr
    Set Clone = New cStepWiseLateralPressure
    Clone.index = Me.index
    Clone.VerticalGravityLoad = Me.VerticalGravityLoad
    Clone.LateralPressureIn = Me.LateralPressureIn
    Clone.LateralPressureOut = Me.LateralPressureOut
    Exit Function
CloneErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cStepWiseLateralPressure: Function Clone")
End Function

Public Function WriteASCII(ByVal iNoOfFile As Integer)
    On Error GoTo WriteASCIIErr
    With Me
        Put #iNoOfFile, , .index
        Put #iNoOfFile, , .VerticalGravityLoad
        Put #iNoOfFile, , .LateralPressureIn
        Put #iNoOfFile, , .LateralPressureOut
    End With
    Exit Function
WriteASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cStepWiseLateralPressure: Function WriteASCII")
End Function

Public Function ReadASCII(ByVal iNoOfFile As Integer)
    On Error GoTo ReadASCIIErr
    Dim ii As Integer, dd As Double
    With Me
        Get #iNoOfFile, , ii: .index = ii
        Get #iNoOfFile, , dd: .VerticalGravityLoad = dd
        Get #iNoOfFile, , dd: .LateralPressureIn = dd
        Get #iNoOfFile, , dd: .LateralPressureOut = dd
    End With
    Exit Function
ReadASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cStepWiseLateralPressure: Function ReadASCII")
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

