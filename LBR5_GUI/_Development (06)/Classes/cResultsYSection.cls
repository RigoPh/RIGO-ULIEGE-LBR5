VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cResultsYSection"
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
Private mvarSection As Double
Private mvarValue As Double
Private mvarScaledValue As Double
Private mvarY1 As Double
Private mvarZ1 As Double
Private mvarY2 As Double
Private mvarZ2 As Double

Public Property Let index(ByVal vData As Integer)
    On Error GoTo IndexLetErr
    mvarIndex = vData
    Exit Property
IndexLetErr:
    Call RaiseError(MyUnhandledError, "cResultsYSection: Index Property Let")
End Property

Public Property Get index() As Integer
    On Error GoTo IndexGetErr
    index = mvarIndex
    Exit Property
IndexGetErr:
    Call RaiseError(MyUnhandledError, "cResultsYSection: Index Property Get")
End Property

Public Property Let Section(ByVal vData As Double)
    On Error GoTo SectionLetErr
    mvarSection = vData
    Exit Property
SectionLetErr:
    Call RaiseError(MyUnhandledError, "cResultsYSection: Section Property Let")
End Property

Public Property Get Section() As Double
    On Error GoTo SectionGetErr
    Section = mvarSection
    Exit Property
SectionGetErr:
    Call RaiseError(MyUnhandledError, "cResultsYSection: Section Property Get")
End Property

Public Property Let Value(ByVal vData As Double)
    On Error GoTo ValueLetErr
    mvarValue = vData
    Exit Property
ValueLetErr:
    Call RaiseError(MyUnhandledError, "cResultsYValue: Value Property Let")
End Property

Public Property Get Value() As Double
    On Error GoTo ValueGetErr
    Value = mvarValue
    Exit Property
ValueGetErr:
    Call RaiseError(MyUnhandledError, "cResultsYValue: Value Property Get")
End Property

Public Property Let ScaledValue(ByVal vData As Double)
    On Error GoTo ScaledValueLetErr
    mvarScaledValue = vData
    Exit Property
ScaledValueLetErr:
    Call RaiseError(MyUnhandledError, "cResultsYScaledValue: ScaledValue Property Let")
End Property

Public Property Get ScaledValue() As Double
    On Error GoTo ScaledValueGetErr
    ScaledValue = mvarScaledValue
    Exit Property
ScaledValueGetErr:
    Call RaiseError(MyUnhandledError, "cResultsYScaledValue: ScaledValue Property Get")
End Property

Public Property Let Y1(ByVal vData As Double)
    On Error GoTo Y1LetErr
    mvarY1 = vData
    Exit Property
Y1LetErr:
    Call RaiseError(MyUnhandledError, "cResultsYY1: Y1 Property Let")
End Property

Public Property Get Y1() As Double
    On Error GoTo Y1GetErr
    Y1 = mvarY1
    Exit Property
Y1GetErr:
    Call RaiseError(MyUnhandledError, "cResultsYY1: Y1 Property Get")
End Property

Public Property Let Y2(ByVal vData As Double)
    On Error GoTo Y2LetErr
    mvarY2 = vData
    Exit Property
Y2LetErr:
    Call RaiseError(MyUnhandledError, "cResultsYY2: Y2 Property Let")
End Property

Public Property Get Y2() As Double
    On Error GoTo Y2GetErr
    Y2 = mvarY2
    Exit Property
Y2GetErr:
    Call RaiseError(MyUnhandledError, "cResultsYY2: Y2 Property Get")
End Property

Public Property Let Z1(ByVal vData As Double)
    On Error GoTo Z1LetErr
    mvarZ1 = vData
    Exit Property
Z1LetErr:
    Call RaiseError(MyUnhandledError, "cResultsYZ1: Z1 Property Let")
End Property

Public Property Get Z1() As Double
    On Error GoTo Z1GetErr
    Z1 = mvarZ1
    Exit Property
Z1GetErr:
    Call RaiseError(MyUnhandledError, "cResultsYZ1: Z1 Property Get")
End Property

Public Property Let Z2(ByVal vData As Double)
    On Error GoTo Z2LetErr
    mvarZ2 = vData
    Exit Property
Z2LetErr:
    Call RaiseError(MyUnhandledError, "cResultsYZ2: Z2 Property Let")
End Property

Public Property Get Z2() As Double
    On Error GoTo Z2GetErr
    Z2 = mvarZ2
    Exit Property
Z2GetErr:
    Call RaiseError(MyUnhandledError, "cResultsYZ2: Z2 Property Get")
End Property

Public Function Clone() As cResultsYSection
    On Error GoTo CloneErr
    Set Clone = New cResultsYSection
    With Clone
        .index = Me.index
        .Section = Me.Section
        .Value = Me.Value
        .ScaledValue = Me.Value
        .Y1 = Me.Y1
        .Z1 = Me.Z1
        .Z2 = Me.Z2
        .Y2 = Me.Y2
    End With
    Exit Function
CloneErr:
    Call RaiseError(MyUnhandledError, "cStrake: Function Clone")
End Function

Private Sub Class_Initialize()
    #If DebugMode Then
        mlClassDebugID = GetNextClassDebugID()
        Debug.Print "'" & TypeSection(Me) & "' instance " & mlClassDebugID & " created"
    #End If
End Sub

Private Sub Class_Terminate()
    #If DebugMode Then
        Debug.Print "'" & TypeSection(Me) & "' instance " & CStr(mlClassDebugID) & " is terminating"
    #End If
End Sub

#If DebugMode Then
    Public Property Get ClassDebugID()
        ClassDebugID = mlClassDebugID
    End Property
#End If



