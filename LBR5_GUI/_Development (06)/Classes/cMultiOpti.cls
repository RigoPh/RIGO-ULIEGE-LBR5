VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cMultiOpti"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
#Const DebugMode = 0
#If DebugMode Then
    Private mlClassDebugID As Long
#End If

Private mvarIMULTI As YesNo
Private mvarRHO As Double
Private mvarW1 As Double
Private mvarW2 As Double
Private mvarW3 As Double
Private mvarF0Cost As Double
Private mvarF0Weight As Double
Private mvarF0Inertia As Double

Public Property Get IMULTI() As YesNo
    On Error GoTo IMULTIGetErr
    IMULTI = mvarIMULTI
    Exit Property
IMULTIGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMultiOpti: IMULTI Property Get")
End Property

Public Property Let IMULTI(ByVal vData As YesNo)
    On Error GoTo IMULTILetErr
    mvarIMULTI = vData
    Exit Property
IMULTILetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMultiOpti: IMULTI Property Let")
End Property

Public Property Get RHO() As Double
    On Error GoTo RHOGetErr
    RHO = mvarRHO
    Exit Property
RHOGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMultiOpti: RHO Property Get")
End Property

Public Property Let RHO(ByVal vData As Double)
    On Error GoTo RHOLetErr
    mvarRHO = vData
    Exit Property
RHOLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMultiOpti: RHO Property Let")
End Property

Public Property Get W1() As Double
    On Error GoTo W1GetErr
    W1 = mvarW1
    Exit Property
W1GetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMultiOpti: W1 Property Get")
End Property

Public Property Let W1(ByVal vData As Double)
    On Error GoTo W1LetErr
    mvarW1 = vData
    Exit Property
W1LetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMultiOpti: W1 Property Let")
End Property

Public Property Get W2() As Double
    On Error GoTo W2GetErr
    W2 = mvarW2
    Exit Property
W2GetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMultiOpti: W2 Property Get")
End Property

Public Property Let W2(ByVal vData As Double)
    On Error GoTo W2LetErr
    mvarW2 = vData
    Exit Property
W2LetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMultiOpti: W2 Property Let")
End Property

Public Property Get W3() As Double
    On Error GoTo W3GetErr
    W3 = mvarW3
    Exit Property
W3GetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMultiOpti: W3 Property Get")
End Property

Public Property Let W3(ByVal vData As Double)
    On Error GoTo W3LetErr
    mvarW3 = vData
    Exit Property
W3LetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMultiOpti: W3 Property Let")
End Property

Public Property Get F0Cost() As Double
    On Error GoTo F0CostGetErr
    F0Cost = mvarF0Cost
    Exit Property
F0CostGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMultiOpti: F0Cost Property Get")
End Property

Public Property Let F0Cost(ByVal vData As Double)
    On Error GoTo F0CostLetErr
    mvarF0Cost = vData
    Exit Property
F0CostLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMultiOpti: F0Cost Property Let")
End Property

Public Property Get F0Weight() As Double
    On Error GoTo F0WeightGetErr
    F0Weight = mvarF0Weight
    Exit Property
F0WeightGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMultiOpti: F0Weight Property Get")
End Property

Public Property Let F0Weight(ByVal vData As Double)
    On Error GoTo F0WeightLetErr
    mvarF0Weight = vData
    Exit Property
F0WeightLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMultiOpti: F0Weight Property Let")
End Property

Public Property Get F0Inertia() As Double
    On Error GoTo F0InertiaGetErr
    F0Inertia = mvarF0Inertia
    Exit Property
F0InertiaGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMultiOpti: F0Inertia Property Get")
End Property

Public Property Let F0Inertia(ByVal vData As Double)
    On Error GoTo F0InertiaLetErr
    mvarF0Inertia = vData
    Exit Property
F0InertiaLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMultiOpti: F0Inertia Property Let")
End Property

Public Function Clone() As cMultiOpti
    On Error GoTo CloneErr
    Set Clone = New cMultiOpti
    With Clone
        .IMULTI = Me.IMULTI
        .RHO = Me.RHO
        .W1 = Me.W1
        .W2 = Me.W2
        .W3 = Me.W3
        .F0Cost = Me.F0Cost
        .F0Weight = Me.F0Weight
        .F0Inertia = Me.F0Inertia
    End With
    Exit Function
CloneErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMultiOpti: Function Clone")
End Function

Public Function WriteASCII(ByVal iNoOfFile As Integer)
    On Error GoTo WriteASCIIErr
    With Me
        Put #iNoOfFile, , CInt(.IMULTI)
        Put #iNoOfFile, , .RHO
        Put #iNoOfFile, , .W1
        Put #iNoOfFile, , .W2
        Put #iNoOfFile, , .W3
    End With
    Exit Function
WriteASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMultiOpti: Function WriteASCII")
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

