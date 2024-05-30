VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cLoadCase"
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
Private mvarTitle As String
Private mvarState As LoadCaseState
Private mvarLateralPressureIn As Double
Private mvarLateralPressureOut As Double
Private mvarVerticalBendingMomentFore As Double
Private mvarVerticalBendingMomentAft As Double
Private mvarHorizontalBendingMomentFore As Double
Private mvarHorizontalBendingMomentAft As Double
Private mvarcolStepwiseLateralPressure As colStepWiseLateralPressure
Private mvarcolStructuralConstraints As colStructuralConstraints
Private mvarVerticalShear As Double
Private mvarHorizontalShear As Double
'Private mvarcolDiagram As colDiagram
Public Enum LoadCaseState
    IsOff = 0
    IsOn = 1
End Enum

Public Property Let LateralPressureOut(ByVal vData As Double)
    On Error GoTo LateralPressureOutLetErr
    mvarLateralPressureOut = vData
    Exit Property
LateralPressureOutLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cLoadCase: LateralPressureOut Property Let")
End Property

Public Property Get LateralPressureOut() As Double
    On Error GoTo LateralPressureOutGetErr
    LateralPressureOut = mvarLateralPressureOut
    Exit Property
LateralPressureOutGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cLoadcase: LateralPressureOut Property Get")
End Property

Public Property Let LateralPressureIn(ByVal vData As Double)
    On Error GoTo LateralPressureInLetErr
    mvarLateralPressureIn = vData
    Exit Property
LateralPressureInLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cLoadCase: LateralPressureIn Property Let")
End Property

Public Property Get LateralPressureIn() As Double
    On Error GoTo LateralPressureInGetErr
    LateralPressureIn = mvarLateralPressureIn
    Exit Property
LateralPressureInGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cLoadCase: LateralPressureIn Property Get")
End Property

Public Property Let Title(ByVal vData As String)
    On Error GoTo TitleLetErr
    mvarTitle = vData
    Exit Property
TitleLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cLoadCase: Title Property Let")
End Property

Public Property Get Title() As String
    On Error GoTo TitleGetErr
    Title = mvarTitle
    Exit Property
TitleGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cLoadCase: Title Property Get")
End Property

Public Property Let State(ByVal vData As LoadCaseState)
    On Error GoTo StateLetErr
    Select Case vData
        Case IsOff, IsOn
        Case Else
            Exit Property
    End Select
    mvarState = vData
    Exit Property
StateLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cLoadCase: State Property Let")
End Property

Public Property Get State() As LoadCaseState
    On Error GoTo StateGetErr
    State = mvarState
    Exit Property
StateGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cLoadCase: State Property Get")
End Property

Public Property Let index(ByVal vData As Integer)
    On Error GoTo IndexLetErr
    mvarIndex = vData
    Exit Property
IndexLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cLoadCase: Index Property Let")
End Property

Public Property Get index() As Integer
    On Error GoTo IndexGetErr
    index = mvarIndex
    Exit Property
IndexGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cLoadCase: Index Propetry Get")
End Property

Public Property Let VerticalBendingMomentFore(ByVal vData As Double)
    On Error GoTo VerticalBendingMomentForeLetErr
    mvarVerticalBendingMomentFore = vData
    Exit Property
VerticalBendingMomentForeLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cLoadCase: VerticalBendingMomentFore Property Let")
End Property

Public Property Get VerticalBendingMomentFore() As Double
    On Error GoTo VerticalBendingMomentForeGetErr
    VerticalBendingMomentFore = mvarVerticalBendingMomentFore
    Exit Property
VerticalBendingMomentForeGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cLoadCase: VerticalBendingMomentFore Propetry Get")
End Property

Public Property Let VerticalBendingMomentAft(ByVal vData As Double)
    On Error GoTo VerticalBendingMomentAftLetErr
    mvarVerticalBendingMomentAft = vData
    Exit Property
VerticalBendingMomentAftLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cLoadCase: VerticalBendingMomentAft Property Let")
End Property

Public Property Get VerticalBendingMomentAft() As Double
    On Error GoTo VerticalBendingMomentAftGetErr
    VerticalBendingMomentAft = mvarVerticalBendingMomentAft
    Exit Property
VerticalBendingMomentAftGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cLoadCase: VerticalBendingMomentAft Propetry Get")
End Property

Public Property Let HorizontalBendingMomentFore(ByVal vData As Double)
    On Error GoTo HorizontalBendingMomentForeLetErr
    mvarHorizontalBendingMomentFore = vData
    Exit Property
HorizontalBendingMomentForeLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cLoadCase: HorizontalBendingMomentFore Property Let")
End Property

Public Property Get HorizontalBendingMomentFore() As Double
    On Error GoTo HorizontalBendingMomentForeGetErr
    HorizontalBendingMomentFore = mvarHorizontalBendingMomentFore
    Exit Property
HorizontalBendingMomentForeGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cLoadCase: HorizontalBendingMomentFore Propetry Get")
End Property

Public Property Let HorizontalBendingMomentAft(ByVal vData As Double)
    On Error GoTo HorizontalBendingMomentAftLetErr
    mvarHorizontalBendingMomentAft = vData
    Exit Property
HorizontalBendingMomentAftLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cLoadCase: HorizontalBendingMomentAft Property Let")
End Property

Public Property Get HorizontalBendingMomentAft() As Double
    On Error GoTo HorizontalBendingMomentAftGetErr
    HorizontalBendingMomentAft = mvarHorizontalBendingMomentAft
    Exit Property
HorizontalBendingMomentAftGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cLoadCase: HorizontalBendingMomentAft Propetry Get")
End Property

Public Property Let VerticalShear(ByVal vData As Double)
    On Error GoTo VerticalShearLetErr
    mvarVerticalShear = vData
    Exit Property
VerticalShearLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cLoadCase: VerticalShear Property Let")
End Property

Public Property Get VerticalShear() As Double
    On Error GoTo VerticalShearGetErr
    VerticalShear = mvarVerticalShear
    Exit Property
VerticalShearGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cLoadCase: VerticalShear Propetry Get")
End Property

Public Property Let HorizontalShear(ByVal vData As Double)
    On Error GoTo HorizontalShearLetErr
    mvarHorizontalShear = vData
    Exit Property
HorizontalShearLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cLoadCase: HorizontalShear Property Let")
End Property

Public Property Get HorizontalShear() As Double
    On Error GoTo HorizontalShearGetErr
    HorizontalShear = mvarHorizontalShear
    Exit Property
HorizontalShearGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cLoadCase: HorizontalShear Propetry Get")
End Property

Public Property Get colStepWiseLateralPressure() As colStepWiseLateralPressure
    On Error GoTo colStepWiseLateralPressureGetErr
    If mvarcolStepwiseLateralPressure Is Nothing Then
        Set mvarcolStepwiseLateralPressure = New colStepWiseLateralPressure
    End If
    Set colStepWiseLateralPressure = mvarcolStepwiseLateralPressure
    Exit Property
colStepWiseLateralPressureGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cLoadCase: colStepWiseLateralPressure Property Get")
End Property

Public Property Set colStepWiseLateralPressure(vData As colStepWiseLateralPressure)
    On Error GoTo colStepWiseLateralPressureSetErr
    Set mvarcolStepwiseLateralPressure = vData
    Exit Property
colStepWiseLateralPressureSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cLoadCase: colStepWiseLateralPressure Property Set")
End Property

Public Property Get colStructuralConstraints() As colStructuralConstraints
    On Error GoTo colStructuralConstraintsGetErr
    If mvarcolStructuralConstraints Is Nothing Then
        Set mvarcolStructuralConstraints = New colStructuralConstraints
    End If
    Set colStructuralConstraints = mvarcolStructuralConstraints
    Exit Property
colStructuralConstraintsGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cLoadCase: colStructuralConstraints Property Get")
End Property

Public Property Set colStructuralConstraints(vData As colStructuralConstraints)
    On Error GoTo colStructuralConstraintsSetErr
    Set mvarcolStructuralConstraints = vData
    Exit Property
colStructuralConstraintsSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cLoadCase: colStructuralConstraints Property Set")
End Property

'Public Property Get colDiagram() As colDiagram
'    On Error GoTo  colDiagramGetErr
'    If mvarcolDiagram Is Nothing Then
'        Set mvarcolDiagram = New colDiagram
'    End If
'    Set colDiagram = mvarcolDiagram
'    Exit Property
'colDiagramGetErr:
'    Call RaiseError(MyUnhandledError,Err.Description & "." & vbCrLf &  "cLoadCase: colDiagram Property Get")
'End Property
'
'Public Property Set colDiagram(ByVal vData As colDiagram)
'    On Error GoTo  colDiagramSetErr
'    Set mvarcolDiagram = vData
'    Exit Property
'colDiagramSetErr:
'    Call RaiseError(MyUnhandledError,Err.Description & "." & vbCrLf &  "cLoadCase: colDiagram Property Set")
'End Property

Public Function Clone() As cLoadCase
    On Error GoTo CloneErr
    Set Clone = New cLoadCase
    With Clone
        .index = Me.index
        .Title = Me.Title
        .State = Me.State
        .LateralPressureIn = Me.LateralPressureIn
        .LateralPressureOut = Me.LateralPressureOut
        .VerticalBendingMomentFore = Me.VerticalBendingMomentFore
        .VerticalBendingMomentAft = Me.VerticalBendingMomentAft
        .HorizontalBendingMomentFore = Me.HorizontalBendingMomentFore
        .HorizontalBendingMomentAft = Me.HorizontalBendingMomentAft
        .VerticalShear = Me.VerticalShear
        .HorizontalShear = Me.HorizontalShear
        Set .colStepWiseLateralPressure = Me.colStepWiseLateralPressure.Clone
        Set .colStructuralConstraints = Me.colStructuralConstraints.Clone
    End With
    Exit Function
CloneErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cLoadCase: Function Clone")
End Function

Public Function WriteASCII(ByVal iNoOfFile As Integer)
    On Error GoTo WriteASCIIErr
    Dim ss As String * 100
    With Me
        Put #iNoOfFile, , .index
        ss = .Title
        Put #iNoOfFile, , ss
        Put #iNoOfFile, , CInt(.State)
        Put #iNoOfFile, , .LateralPressureIn
        Put #iNoOfFile, , .LateralPressureOut
        Put #iNoOfFile, , .VerticalBendingMomentFore
        'Put #iNoOfFile, , .VerticalBendingMomentAft
        Put #iNoOfFile, , .VerticalShear
        Put #iNoOfFile, , .HorizontalBendingMomentFore
        'Put #iNoOfFile, , .HorizontalBendingMomentAft
        'Put #iNoOfFile, , .HorizontalBendingMomentFore
        Put #iNoOfFile, , .HorizontalShear
        .colStepWiseLateralPressure.WriteASCII iNoOfFile
        .colStructuralConstraints.WriteASCII iNoOfFile
    End With
    Exit Function
WriteASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cLoadCase: Function WriteASCII")
End Function

Public Function ReadASCII(ByVal iNoOfFile As Integer)
    On Error GoTo ReadASCIIErr
    Dim ii As Integer, dd As Double, ss As String * 100
    With Me
        Get #iNoOfFile, , ii: .index = ii
        Get #iNoOfFile, , ss: .Title = Trim(ss)
        Get #iNoOfFile, , ii: .State = ii
        Get #iNoOfFile, , dd: .LateralPressureIn = dd
        Get #iNoOfFile, , dd: .LateralPressureOut = dd
        Get #iNoOfFile, , dd: .VerticalBendingMomentFore = dd
        Get #iNoOfFile, , dd: .VerticalBendingMomentAft = dd
        Get #iNoOfFile, , dd: .HorizontalBendingMomentFore = dd
        Get #iNoOfFile, , dd: .HorizontalBendingMomentAft = dd
        .colStepWiseLateralPressure.ReadASCII iNoOfFile
        .colStructuralConstraints.ReadASCII iNoOfFile
    End With
    Exit Function
ReadASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cLoadCase: Function ReadASCII")
End Function

Private Sub Class_Initialize()
    #If DebugMode Then
        mlClassDebugID = GetNextClassDebugID()
        Debug.Print "'" & TypeName(Me) & "' instance " & mlClassDebugID & " created"
    #End If
    'Set mvarcolStepwiseLateralPressure = New colStepWiseLateralPressure
End Sub

Private Sub Class_terminate()
    #If DebugMode Then
        Debug.Print "'" & TypeName(Me) & "' instance " & CStr(mlClassDebugID) & " is terminating"
    #End If
    Set mvarcolStepwiseLateralPressure = Nothing
    'Set mvarcolDiagram = Nothing
End Sub

#If DebugMode Then
    Public Property Get ClassDebugID()
        ClassDebugID = mlClassDebugID
    End Property
#End If

