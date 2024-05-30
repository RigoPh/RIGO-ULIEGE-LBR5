VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cMarsLoads"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
#Const DebugMode = 0
#If DebugMode Then
    Private mlClassDebugID As Long
#End If

Private mvarcolMarsLoadCase As colMarsLoadCase
Private mvarcolMarsLoadType As colMarsLoadType
Private mvarcolDraught As colDouble
Private mvarStillWaterBendingMomentHogg As Double
Private mvarStillWaterBendingMomentSagg As Double
Private mvarVerticalWaveBendingMomentHogg As Double
Private mvarVerticalWaveBendingMomentSagg As Double
Private mvarHorizontalWaveBendingMoment As Double
Private mvarVerticalStillShear As Double
Private mvarVerticalWaveShearPositive As Double
Private mvarVerticalWaveShearNegative As Double
Private mvarGammaStillGlobal As Double ' gs1
Private mvarGammaWaveGlobal As Double 'gw1
Private mvarGammaStillLocal As Double 'gs2
Private mvarGammaWaveLocal As Double 'gw2

Public Property Let GammaWaveLocal(ByVal vData As Double)
    On Error GoTo GammaWaveLocalLetErr
    mvarGammaWaveLocal = vData
    Exit Property
GammaWaveLocalLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: GammaWaveLocal Property Let")
End Property

Public Property Get GammaWaveLocal() As Double
    On Error GoTo GammaWaveLocalGetErr
    GammaWaveLocal = mvarGammaWaveLocal
    Exit Property
GammaWaveLocalGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: GammaWaveLocal Property Get")
End Property

Public Property Let GammaStillLocal(ByVal vData As Double)
    On Error GoTo GammaStillLocalLetErr
    mvarGammaStillLocal = vData
    Exit Property
GammaStillLocalLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: GammaStillLocal Property Let")
End Property

Public Property Get GammaStillLocal() As Double
    On Error GoTo GammaStillLocalGetErr
    GammaStillLocal = mvarGammaStillLocal
    Exit Property
GammaStillLocalGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: GammaStillLocal Property Get")
End Property

Public Property Let GammaWaveGlobal(ByVal vData As Double)
    On Error GoTo GammaWaveGlobalLetErr
    mvarGammaWaveGlobal = vData
    Exit Property
GammaWaveGlobalLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: GammaWaveGlobal Property Let")
End Property

Public Property Get GammaWaveGlobal() As Double
    On Error GoTo GammaWaveGlobalGetErr
    GammaWaveGlobal = mvarGammaWaveGlobal
    Exit Property
GammaWaveGlobalGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: GammaWaveGlobal Property Get")
End Property

Public Property Let GammaStillGlobal(ByVal vData As Double)
    On Error GoTo GammaStillGlobalLetErr
    mvarGammaStillGlobal = vData
    Exit Property
GammaStillGlobalLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: GammaStillGlobal Property Let")
End Property

Public Property Get GammaStillGlobal() As Double
    On Error GoTo GammaStillGlobalGetErr
    GammaStillGlobal = mvarGammaStillGlobal
    Exit Property
GammaStillGlobalGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: GammaStillGlobal Property Get")
End Property

Public Property Let VerticalWaveShearPositive(ByVal vData As Double)
    On Error GoTo VerticalWaveShearPositiveLetErr
    mvarVerticalWaveShearPositive = vData
    Exit Property
VerticalWaveShearPositiveLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: VerticalWaveShearPositive Property Let")
End Property

Public Property Get VerticalWaveShearPositive() As Double
    On Error GoTo VerticalWaveShearPositiveGetErr
    VerticalWaveShearPositive = mvarVerticalWaveShearPositive
    Exit Property
VerticalWaveShearPositiveGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: VerticalWaveShearPositive Property Get")
End Property

Public Property Let VerticalWaveShearNegative(ByVal vData As Double)
    On Error GoTo VerticalWaveShearNegativeLetErr
    mvarVerticalWaveShearNegative = vData
    Exit Property
VerticalWaveShearNegativeLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: VerticalWaveShearNegative Property Let")
End Property

Public Property Get VerticalWaveShearNegative() As Double
    On Error GoTo VerticalWaveShearNegativeGetErr
    VerticalWaveShearNegative = mvarVerticalWaveShearNegative
    Exit Property
VerticalWaveShearNegativeGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: VerticalWaveShearNegative Property Get")
End Property

Public Property Let VerticalStillShear(ByVal vData As Double)
    On Error GoTo VerticalStillShearLetErr
    mvarVerticalStillShear = vData
    Exit Property
VerticalStillShearLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: VerticalStillShear Property Let")
End Property

Public Property Get VerticalStillShear() As Double
    On Error GoTo VerticalStillShearGetErr
    VerticalStillShear = mvarVerticalStillShear
    Exit Property
VerticalStillShearGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: VerticalStillShear Property Get")
End Property

Public Property Let HorizontalWaveBendingMoment(ByVal vData As Double)
    On Error GoTo HorizontalWaveBendingMomentLetErr
    mvarHorizontalWaveBendingMoment = vData
    Exit Property
HorizontalWaveBendingMomentLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: HorizontalWaveBendingMoment Property Let")
End Property

Public Property Get HorizontalWaveBendingMoment() As Double
    On Error GoTo HorizontalWaveBendingMomentGetErr
    HorizontalWaveBendingMoment = mvarHorizontalWaveBendingMoment
    Exit Property
HorizontalWaveBendingMomentGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: HorizontalWaveBendingMoment Property Get")
End Property

Public Property Let VerticalWaveBendingMomentSagg(ByVal vData As Double)
    On Error GoTo VerticalWaveBendingMomentSaggLetErr
    mvarVerticalWaveBendingMomentSagg = vData
    Exit Property
VerticalWaveBendingMomentSaggLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: VerticalWaveBendingMomentSagg Property Let")
End Property

Public Property Get VerticalWaveBendingMomentSagg() As Double
    On Error GoTo VerticalWaveBendingMomentSaggGetErr
    VerticalWaveBendingMomentSagg = mvarVerticalWaveBendingMomentSagg
    Exit Property
VerticalWaveBendingMomentSaggGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: VerticalWaveBendingMomentSagg Property Get")
End Property

Public Property Let VerticalWaveBendingMomentHogg(ByVal vData As Double)
    On Error GoTo VerticalWaveBendingMomentHoggLetErr
    mvarVerticalWaveBendingMomentHogg = vData
    Exit Property
VerticalWaveBendingMomentHoggLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: VerticalWaveBendingMomentHogg Property Let")
End Property

Public Property Get VerticalWaveBendingMomentHogg() As Double
    On Error GoTo VerticalWaveBendingMomentHoggGetErr
    VerticalWaveBendingMomentHogg = mvarVerticalWaveBendingMomentHogg
    Exit Property
VerticalWaveBendingMomentHoggGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: VerticalWaveBendingMomentHogg Property Get")
End Property

Public Property Let StillWaterBendingMomentSagg(ByVal vData As Double)
    On Error GoTo StillWaterBendingMomentSaggLetErr
    mvarStillWaterBendingMomentSagg = vData
    Exit Property
StillWaterBendingMomentSaggLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: StillWaterBendingMomentSagg Property Let")
End Property

Public Property Get StillWaterBendingMomentSagg() As Double
    On Error GoTo StillWaterBendingMomentSaggGetErr
    StillWaterBendingMomentSagg = mvarStillWaterBendingMomentSagg
    Exit Property
StillWaterBendingMomentSaggGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: StillWaterBendingMomentSagg Property Get")
End Property

Public Property Let StillWaterBendingMomentHogg(ByVal vData As Double)
    On Error GoTo StillWaterBendingMomentHoggLetErr
    mvarStillWaterBendingMomentHogg = vData
    Exit Property
StillWaterBendingMomentHoggLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: StillWaterBendingMomentHogg Property Let")
End Property

Public Property Get StillWaterBendingMomentHogg() As Double
    On Error GoTo StillWaterBendingMomentHoggGetErr
    StillWaterBendingMomentHogg = mvarStillWaterBendingMomentHogg
    Exit Property
StillWaterBendingMomentHoggGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: StillWaterBendingMomentHogg Property Get")
End Property

Public Property Get colMarsLoadCase() As colMarsLoadCase
    On Error GoTo colMarsLoadCaseGetErr
    If mvarcolMarsLoadCase Is Nothing Then
        Set mvarcolMarsLoadCase = New colMarsLoadCase
    End If
    Set colMarsLoadCase = mvarcolMarsLoadCase
    Exit Property
colMarsLoadCaseGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMarsLoads: colMarsLoadCase Property Get")
End Property

Public Property Set colMarsLoadCase(vData As colMarsLoadCase)
    On Error GoTo colMarsLoadCaseSetErr
    Set mvarcolMarsLoadCase = vData
    Exit Property
colMarsLoadCaseSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMarsLoads: colMarsLoadCase Property Set")
End Property

Public Property Get colDraught() As colDouble
    On Error GoTo colDraughtGetErr
    If mvarcolDraught Is Nothing Then
        Set mvarcolDraught = New colDouble
    End If
    Set colDraught = mvarcolDraught
    Exit Property
colDraughtGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMarsLoads: colDraught Property Get")
End Property

Public Property Set colDraught(vData As colDouble)
    On Error GoTo colDraughtSetErr
    Set mvarcolDraught = vData
    Exit Property
colDraughtSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMarsLoads: colDraught Property Set")
End Property

Public Property Get colMarsLoadType() As colMarsLoadType
    On Error GoTo colMarsLoadTypeGetErr
    If mvarcolMarsLoadType Is Nothing Then
        Set mvarcolMarsLoadType = New colMarsLoadType
    End If
    Set colMarsLoadType = mvarcolMarsLoadType
    Exit Property
colMarsLoadTypeGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMarsLoads: colMarsLoadType Property Get")
End Property

Public Property Set colMarsLoadType(vData As colMarsLoadType)
    On Error GoTo colMarsLoadTypeSetErr
    Set mvarcolMarsLoadType = vData
    Exit Property
colMarsLoadTypeSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMarsLoads: colMarsLoadType Property Set")
End Property

Public Function Clone() As cMarsLoads
    On Error GoTo CloneErr
    Set Clone = New cMarsLoads
    With Clone
        Set .colMarsLoadCase = Me.colMarsLoadCase.Clone
        Set .colMarsLoadType = Me.colMarsLoadType.Clone
        Set .colDraught = Me.colDraught.Clone
        .StillWaterBendingMomentHogg = Me.StillWaterBendingMomentHogg
        .StillWaterBendingMomentSagg = Me.StillWaterBendingMomentSagg
        .VerticalWaveBendingMomentHogg = Me.VerticalWaveBendingMomentHogg
        .VerticalWaveBendingMomentSagg = Me.VerticalWaveBendingMomentSagg
        .HorizontalWaveBendingMoment = Me.HorizontalWaveBendingMoment
        .VerticalWaveShearNegative = Me.VerticalWaveShearNegative
        .VerticalWaveShearPositive = Me.VerticalWaveShearPositive
        .GammaStillGlobal = Me.GammaStillGlobal
        .GammaWaveGlobal = Me.GammaWaveGlobal
        .GammaStillLocal = Me.GammaStillLocal
        .GammaWaveLocal = Me.GammaWaveLocal
        .VerticalStillShear = Me.VerticalStillShear
        ' ! To add vertical still shear (new version)
    End With
    Exit Function
CloneErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMarsLoads: Function Clone")
End Function

Public Function WriteASCII(ByVal iNoOfFile As Integer)
    On Error GoTo WriteASCIIErr
    With Me
        colMarsLoadCase.WriteASCII iNoOfFile
        colMarsLoadType.WriteASCII iNoOfFile
        colDraught.WriteASCII iNoOfFile
        Put #iNoOfFile, , .StillWaterBendingMomentHogg
        Put #iNoOfFile, , .StillWaterBendingMomentSagg
        Put #iNoOfFile, , .VerticalWaveBendingMomentHogg
        Put #iNoOfFile, , .VerticalWaveBendingMomentSagg
        Put #iNoOfFile, , .HorizontalWaveBendingMoment
        Put #iNoOfFile, , .VerticalStillShear
        Put #iNoOfFile, , .VerticalWaveShearPositive
        Put #iNoOfFile, , .VerticalWaveShearNegative
        Put #iNoOfFile, , .GammaStillGlobal
        Put #iNoOfFile, , .GammaWaveGlobal
        Put #iNoOfFile, , .GammaStillLocal
        Put #iNoOfFile, , .GammaWaveLocal
    End With
    Exit Function
WriteASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMarsLoads: Function WriteASCII")
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

