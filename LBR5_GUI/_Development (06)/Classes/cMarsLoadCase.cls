VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cMarsLoadCase"
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
Dim mvarCF_VerticalWaveBendingMoment_Shear As Double
Dim mvarCF_HorizontalWaveBendingMoment As Double
Dim mvarcolMarsLoadType As colMarsLoadType

Public Property Let index(ByVal vData As Integer)
    On Error GoTo IndexLetErr
    mvarIndex = vData
    Exit Property
IndexLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMarsLoadCase: Index Property Let")
End Property

Public Property Get index() As Integer
    On Error GoTo IndexGetErr
    index = mvarIndex
    Exit Property
IndexGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMarsLoadCase: Index Property Get")
End Property

Public Property Get Label() As String
    On Error GoTo LabelGetErr
    Label = mvarLabel
    Exit Property
LabelGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMarsLoadCase: Label Property Get")
End Property

Public Property Let Label(ByVal vData As String)
    On Error GoTo LabelSetErr
    mvarLabel = vData
    Exit Property
LabelSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMarsLoadCase: Label Property Set")
End Property

Public Property Let CF_VerticalWaveBendingMoment_Shear(ByVal vData As Double)
    On Error GoTo CF_VerticalWaveBendingMoment_ShearLetErr
    mvarCF_VerticalWaveBendingMoment_Shear = vData
    Exit Property
CF_VerticalWaveBendingMoment_ShearLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMarsLoadCase: CF_VerticalWaveBendingMoment_Shear Property Let")
End Property

Public Property Get CF_VerticalWaveBendingMoment_Shear() As Double
    On Error GoTo CF_VerticalWaveBendingMoment_ShearGetErr
    CF_VerticalWaveBendingMoment_Shear = mvarCF_VerticalWaveBendingMoment_Shear
    Exit Property
CF_VerticalWaveBendingMoment_ShearGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMarsLoadCase: CF_VerticalWaveBendingMoment_Shear Property Get")
End Property

Public Property Let CF_HorizontalWaveBendingMoment(ByVal vData As Double)
    On Error GoTo CF_HorizontalWaveBendingMomentLetErr
    mvarCF_HorizontalWaveBendingMoment = vData
    Exit Property
CF_HorizontalWaveBendingMomentLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMarsLoadCase: CF_HorizontalWaveBendingMoment Property Let")
End Property

Public Property Get CF_HorizontalWaveBendingMoment() As Double
    On Error GoTo CF_HorizontalWaveBendingMomentGetErr
    CF_HorizontalWaveBendingMoment = mvarCF_HorizontalWaveBendingMoment
    Exit Property
CF_HorizontalWaveBendingMomentGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMarsLoadCase: CF_HorizontalWaveBendingMoment Property Get")
End Property

Public Property Get colMarsLoadType() As colMarsLoadType
    On Error GoTo colMarsLoadTypeGetErr
    If mvarcolMarsLoadType Is Nothing Then
        Set mvarcolMarsLoadType = New colMarsLoadType
    End If
    Set colMarsLoadType = mvarcolMarsLoadType
    Exit Property
colMarsLoadTypeGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMarsLoadCase: colMarsLoadType Property Get")
End Property

Public Property Set colMarsLoadType(vData As colMarsLoadType)
    On Error GoTo colMarsLoadTypeSetErr
    Set mvarcolMarsLoadType = vData
    Exit Property
colMarsLoadTypeSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMarsLoadCase: colMarsLoadType Property Set")
End Property

Public Function Clone() As cMarsLoadCase
    On Error GoTo CloneErr
    Set Clone = New cMarsLoadCase
    With Clone
        .index = Me.index
        .Label = Me.Label
        .CF_VerticalWaveBendingMoment_Shear = Me.CF_VerticalWaveBendingMoment_Shear
        .CF_HorizontalWaveBendingMoment = Me.CF_HorizontalWaveBendingMoment
        Set .colMarsLoadType = Me.colMarsLoadType.Clone
    End With
    Exit Function
CloneErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMarsLoadCase: Function Clone")
End Function

Public Function WriteASCII(ByVal iNoOfFile As Integer)
    On Error GoTo WriteASCIIErr
    Dim ss As String * 10
    With Me
        Put #iNoOfFile, , .index
        ss = .Label
        Put #iNoOfFile, , ss
        Put #iNoOfFile, , .CF_VerticalWaveBendingMoment_Shear
        Put #iNoOfFile, , .CF_HorizontalWaveBendingMoment
        .colMarsLoadType.WriteASCII iNoOfFile
    End With
    Exit Function
WriteASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMarsLoadCase: Function WriteASCII")
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
