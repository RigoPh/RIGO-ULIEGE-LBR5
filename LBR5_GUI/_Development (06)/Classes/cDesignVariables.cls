VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cDesignVariables"
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
Private mvarActive As Boolean
Private mvarVariableName As DesignVariableName
Private mvarUpperLimit As Double
Private mvarLowerLimit As Double
Public Enum DesignVariableName
    PlateThickness = 1
    FrameWebHeight = 2
    FrameWebThickness = 3
    FrameFlangeWidth = 4
    FrameSpacing = 5
    StiffenerWebHeight = 6
    StiffenerWebThickness = 7
    StiffenerFlangeWidth = 8
    StiffenerSpacing = 9
End Enum

Public Property Let index(ByVal vData As Integer)
    On Error GoTo IndexLetErr
    mvarIndex = vData
    Exit Property
IndexLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDesignVariables: Index Property Let")
End Property

Public Property Get index() As Integer
    On Error GoTo IndexGetErr
    index = mvarIndex
    Exit Property
IndexGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDesignVariables: Index Property Get")
End Property

Public Property Let Active(ByVal vData As Boolean)
    On Error GoTo ActiveLetErr
    mvarActive = vData
    Exit Property
ActiveLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDesignVariables: Active Property Let")
End Property

Public Property Get Active() As Boolean
    On Error GoTo ActiveGetErr
    Active = mvarActive
    Exit Property
ActiveGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDesignVariables: Active Property Get")
End Property

Public Property Let LowerLimit(ByVal vData As Double)
    On Error GoTo LowerLimitLetErr
    mvarLowerLimit = vData
    Exit Property
LowerLimitLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDesignVariables: LowerLimit Property Let")
End Property

Public Property Get LowerLimit() As Double
    On Error GoTo LowerLimitGetErr
    LowerLimit = mvarLowerLimit
    Exit Property
LowerLimitGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDesignVariables: LowerLimit Property Get")
End Property

Public Property Let UpperLimit(ByVal vData As Double)
    On Error GoTo UpperLimitLetErr
    mvarUpperLimit = vData
    Exit Property
UpperLimitLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDesignVariables: UpperLimit Property Let")
End Property

Public Property Get UpperLimit() As Double
    On Error GoTo UpperLimitGetErr
    UpperLimit = mvarUpperLimit
    Exit Property
UpperLimitGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDesignVariables: UpperLimit Property Get")
End Property

Public Property Let VariableName(ByVal vData As DesignVariableName)
    On Error GoTo VariableNameLetErr
    mvarVariableName = vData
    Exit Property
VariableNameLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDesignVariables: VariableName Property Let")
End Property

Public Property Get VariableName() As DesignVariableName
    On Error GoTo VariableNameGetErr
    VariableName = mvarVariableName
    Exit Property
VariableNameGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDesignVariables: VariableName Property Get")
End Property

Public Function Clone() As cDesignVariables
    On Error GoTo CloneErr
    Set Clone = New cDesignVariables
    Clone.index = Me.index
    Clone.Active = Me.Active
    Clone.VariableName = Me.VariableName
    Clone.UpperLimit = Me.UpperLimit
    Clone.LowerLimit = Me.LowerLimit
    Exit Function
CloneErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDesignVariables: Function Clone")
End Function

Public Function WriteASCII(ByVal iNoOfFile As Integer)
    On Error GoTo WriteASCIIErr
    With Me
        Put #iNoOfFile, , .index
        Put #iNoOfFile, , .Active
        Put #iNoOfFile, , CInt(.VariableName)
        Put #iNoOfFile, , .UpperLimit
        Put #iNoOfFile, , .LowerLimit
    End With
    Exit Function
WriteASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDesignVariables: Function WriteASCII")
End Function

Public Function ReadASCII(ByVal iNoOfFile As Integer)
    On Error GoTo ReadASCIIErr
    Dim ii As Integer, dd As Double
    With Me
        Get #iNoOfFile, , ii: .index = ii
        Get #iNoOfFile, , ii: .VariableName = ii
        Get #iNoOfFile, , dd: .UpperLimit = dd
        Get #iNoOfFile, , dd: .LowerLimit = dd
    End With
    Exit Function
ReadASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cDesignVariables: Function ReadASCII")
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

