VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cMarsLoadType"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
#Const DebugMode = 0
#If DebugMode Then
    Private mlClassDebugID As Long
#End If

Dim mvarIndex As Integer
Dim mvarLabel As String
Dim mvarLeftCompPress As cMarsPress
Dim mvarRightCompPress As cMarsPress
Dim mvarRelatedDraught As Integer

Public Property Let index(ByVal vData As Integer)
    On Error GoTo IndexLetErr
    mvarIndex = vData
    Exit Property
IndexLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMarsLoadType: Index Property Let")
End Property

Public Property Get index() As Integer
    On Error GoTo IndexGetErr
    index = mvarIndex
    Exit Property
IndexGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMarsLoadType: Index Property Get")
End Property

Public Property Get Label() As String
    On Error GoTo LabelGetErr
    Label = mvarLabel
    Exit Property
LabelGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMarsLoadType: Label Property Get")
End Property

Public Property Let Label(ByVal vData As String)
    On Error GoTo LabelSetErr
    mvarLabel = vData
    Exit Property
LabelSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMarsLoadType: Label Property Set")
End Property

Public Property Get LeftCompPress() As cMarsPress
    On Error GoTo LeftCompPressGetErr
    Set LeftCompPress = mvarLeftCompPress
    Exit Property
LeftCompPressGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMarsLoadType: LeftCompPress Property Get")
End Property

Public Property Set LeftCompPress(ByVal vData As cMarsPress)
    On Error GoTo LeftCompPressSetErr
    Set mvarLeftCompPress = vData
    Exit Property
LeftCompPressSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMarsLoadType: LeftCompPress Property Set")
End Property

Public Property Get RightCompPress() As cMarsPress
    On Error GoTo RightCompPressGetErr
    Set RightCompPress = mvarRightCompPress
    Exit Property
RightCompPressGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMarsLoadType: RightCompPress Property Get")
End Property

Public Property Set RightCompPress(ByVal vData As cMarsPress)
    On Error GoTo RightCompPressSetErr
    Set mvarRightCompPress = vData
    Exit Property
RightCompPressSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMarsLoadType: RightCompPress Property Set")
End Property

Public Property Let RelatedDraught(ByVal vData As Integer)
    On Error GoTo RelatedDraughtLetErr
    mvarRelatedDraught = vData
    Exit Property
RelatedDraughtLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMarsLoadType: RelatedDraught Property Let")
End Property

Public Property Get RelatedDraught() As Integer
    On Error GoTo RelatedDraughtGetErr
    RelatedDraught = mvarRelatedDraught
    Exit Property
RelatedDraughtGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMarsLoadType: RelatedDraught Property Get")
End Property

Public Function Clone() As cMarsLoadType
    On Error GoTo CloneErr
    Set Clone = New cMarsLoadType
    With Clone
        .index = Me.index
        .Label = Me.Label
        Set .LeftCompPress = Me.LeftCompPress.Clone
        Set .RightCompPress = Me.RightCompPress.Clone
        .RelatedDraught = Me.RelatedDraught
    End With
    Exit Function
CloneErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMarsLoadType: Function Clone")
End Function

Public Function WriteASCII(ByVal iNoOfFile As Integer)
    On Error GoTo WriteASCIIErr
    Dim ss As String * 10
    With Me
        Put #iNoOfFile, , .index
        ss = .Label
        Put #iNoOfFile, , ss
        .LeftCompPress.WriteASCII iNoOfFile
        .RightCompPress.WriteASCII iNoOfFile
        Put #iNoOfFile, , .RelatedDraught
    End With
    Exit Function
WriteASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMarsLoadType: Function WriteASCII")
End Function

Private Sub Class_Initialize()
    #If DebugMode Then
        mlClassDebugID = GetNextClassDebugID()
        Debug.Print "'" & TypeName(Me) & "' instance " & mlClassDebugID & " created"
    #End If
    Set mvarLeftCompPress = New cMarsPress
    Set mvarRightCompPress = New cMarsPress
End Sub

Private Sub Class_terminate()
    #If DebugMode Then
        Debug.Print "'" & TypeName(Me) & "' instance " & CStr(mlClassDebugID) & " is terminating"
    #End If
    Set mvarLeftCompPress = Nothing
    Set mvarRightCompPress = Nothing
End Sub

#If DebugMode Then
    Public Property Get ClassDebugID()
        ClassDebugID = mlClassDebugID
    End Property
#End If
