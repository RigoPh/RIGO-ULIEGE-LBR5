VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cGeometry"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
#Const DebugMode = 0
#If DebugMode Then
    Private mlClassDebugID As Long
#End If

Private mvarPanelWidth As Double
Private mvarPanelAngle As Double
Private mvarInNode As Integer
Private mvarOutNode As Integer
Private mvarParticipation As Double
Private mvarQ As Double
Private mvarPHIL As Double
Private mvarBucklingLength As Double
Private mvarFAMI As Integer
Private mvarLOT As Integer
Private mvarPositionCode As Integer

Public Property Let PanelAngle(ByVal vData As Double)
    On Error GoTo PanelAngleLetErr
    mvarPanelAngle = vData
    Exit Property
PanelAngleLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGeometry: PanelAngle Property Let")
End Property

Public Property Get PanelAngle() As Double
    On Error GoTo PanelAngleGetErr
    PanelAngle = mvarPanelAngle
    Exit Property
PanelAngleGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGeometry: PanelAngle Property Get")
End Property

Public Property Let PanelWidth(ByVal vData As Double)
    On Error GoTo PanelWidthLetErr
    mvarPanelWidth = vData
    Exit Property
PanelWidthLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGeometry: PanelWidth Property Let")
End Property

Public Property Get PanelWidth() As Double
    On Error GoTo PanelWidthGetErr
    PanelWidth = mvarPanelWidth
    Exit Property
PanelWidthGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGeometry: PanelWidth Property Get")
End Property

Public Property Let OutNode(ByVal vData As Integer)
    On Error GoTo OutNodeLetErr
    mvarOutNode = vData
    Exit Property
OutNodeLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGeometry: OutNode Property Let")
End Property

Public Property Get OutNode() As Integer
    On Error GoTo OutNodeGetErr
    OutNode = mvarOutNode
    Exit Property
OutNodeGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGeometry: OutNode Property Get")
End Property

Public Property Let InNode(ByVal vData As Integer)
    On Error GoTo InNodeLetErr
    mvarInNode = vData
    Exit Property
InNodeLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGeometry: InNode Property Let")
End Property

Public Property Let PHIL(ByVal vData As Double)
    On Error GoTo PHILLetErr
    mvarPHIL = vData
    Exit Property
PHILLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGeometry: PHIL Property Let")
End Property

Public Property Get PHIL() As Double
    On Error GoTo PHILGetErr
    PHIL = mvarPHIL
    Exit Property
PHILGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGeometry: PHIL Property Get")
End Property

Public Property Let Q(ByVal vData As Double)
    On Error GoTo QLetErr
    mvarQ = vData
    Exit Property
QLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGeometry: Q Property Let")
End Property

Public Property Get Q() As Double
    On Error GoTo QGetErr
    Q = mvarQ
    Exit Property
QGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGeometry: Q Property Get")
End Property

Public Property Get InNode() As Integer
    On Error GoTo InNodeGetErr
    InNode = mvarInNode
    Exit Property
InNodeGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGeometry: InNode Property Get")
End Property

Public Property Let Participation(ByVal vData As Double)
    On Error GoTo ParticipationLetErr
    mvarParticipation = vData
    Exit Property
ParticipationLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGeometry: Participation Property Let")
End Property

Public Property Get Participation() As Double
    On Error GoTo ParticipationGetErr
    Participation = mvarParticipation
    Exit Property
ParticipationGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGeometry: Participation Property Get")
End Property

Public Property Let BucklingLength(ByVal vData As Double)
    On Error GoTo BucklingLengthErr
    mvarBucklingLength = vData
    Exit Property
BucklingLengthErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGeometry: BucklingLength Property Let")
End Property

Public Property Get BucklingLength() As Double
    On Error GoTo BucklingLengthErr
    BucklingLength = mvarBucklingLength
    Exit Property
BucklingLengthErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGeometry: BucklingLength Property Get")
End Property

Public Property Get FAMI() As Integer
    On Error GoTo FAMIGetErr
    FAMI = mvarFAMI
    Exit Property
FAMIGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: FAMI Property Get")
End Property

Public Property Let FAMI(ByVal vData As Integer)
    On Error GoTo FAMILetErr
    mvarFAMI = vData
    Exit Property
FAMILetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: FAMI Property Let")
End Property

Public Property Get LOT() As Integer
    On Error GoTo LOTGetErr
    LOT = mvarLOT
    Exit Property
LOTGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: LOT Property Get")
End Property

Public Property Let LOT(ByVal vData As Integer)
    On Error GoTo LOTLetErr
    mvarLOT = vData
    Exit Property
LOTLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: LOT Property Let")
End Property

Public Property Get PositionCode() As Integer
    On Error GoTo PositionCodeGetErr
    PositionCode = mvarPositionCode
    Exit Property
PositionCodeGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: PositionCode Property Get")
End Property

Public Property Let PositionCode(ByVal vData As Integer)
    On Error GoTo PositionCodeLetErr
    mvarPositionCode = vData
    Exit Property
PositionCodeLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: PositionCode Property Let")
End Property

Public Function Clone() As cGeometry
    On Error GoTo CloneErr
    Set Clone = New cGeometry
    Clone.Q = Me.Q
    Clone.PHIL = Me.PHIL
    Clone.InNode = Me.InNode
    Clone.OutNode = Me.OutNode
    Clone.PanelWidth = Me.PanelWidth
    Clone.PanelAngle = Me.PanelAngle
    Clone.Participation = Me.Participation
    Clone.BucklingLength = Me.BucklingLength
    Clone.FAMI = Me.FAMI
    Clone.LOT = Me.LOT
    Clone.PositionCode = Me.PositionCode
    Exit Function
CloneErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGeometry: Function Clone")
End Function

Public Function WriteASCII(ByVal iNoOfFile As Integer)
    On Error GoTo WriteASCIIErr
    With Me
        Put #iNoOfFile, , .Q
        Put #iNoOfFile, , .PHIL
        Put #iNoOfFile, , CInt(.InNode)
        Put #iNoOfFile, , CInt(.OutNode)
        Put #iNoOfFile, , .PanelWidth
        Put #iNoOfFile, , .PanelAngle
        Put #iNoOfFile, , .Participation
        Put #iNoOfFile, , .BucklingLength
        Put #iNoOfFile, , .PositionCode
        Put #iNoOfFile, , .FAMI
        Put #iNoOfFile, , .LOT
        
    End With
    Exit Function
WriteASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGeometry: Function WriteASCII")
End Function

Public Function ReadASCII(ByVal iNoOfFile As Integer)
    On Error GoTo ReadASCIIErr
    Dim ii As Integer, dd As Double
    With Me
        Get #iNoOfFile, , dd: .Q = dd
        Get #iNoOfFile, , dd: .PHIL = dd
        Get #iNoOfFile, , ii: .InNode = ii
        Get #iNoOfFile, , ii: .OutNode = ii
        Get #iNoOfFile, , dd: .PanelWidth = dd
        Get #iNoOfFile, , dd: .PanelAngle = dd
        Get #iNoOfFile, , dd: .Participation = dd
        Get #iNoOfFile, , dd: .BucklingLength = dd
    End With
    Exit Function
ReadASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGeometry: Function ReadASCII")
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


