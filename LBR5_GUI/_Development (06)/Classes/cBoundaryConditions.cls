VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cBoundaryConditions"
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
Private mvarBoundaryCondition As BoundaryCondition
Private mvarEdge As Edge
Public Enum BoundaryCondition
    FreeEdge = 1
    SimplySupported2 = 2
    SimplySupported3 = 3
    SimplySupported4 = 4
    SimplySupported5 = 5
    Clamped = 6
    SymmetryAxis1 = 7
    SimplySupported6 = 8
    SimplySupported7 = 9
    SimplySupported8 = 10
    SymmetryAxis2 = 11
    DoubleSymmetry = 12
End Enum
Public Enum Edge
    InEdge = 1
    OutEdge = 2
End Enum

Public Property Let index(ByVal vData As Integer)
    On Error GoTo IndexLetErr
    mvarIndex = vData
    Exit Property
IndexLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cBondaryConditions: Index Property Let")
End Property

Public Property Get index() As Integer
    On Error GoTo IndexGetErr
    index = mvarIndex
    Exit Property
IndexGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cBondaryConditions: Index Property Get")
End Property

Public Property Let BoundaryCondition(ByVal vData As BoundaryCondition)
    On Error GoTo BoundaryConditionLetErr
    Select Case vData
        Case 1 To 12
        Case Else
            Exit Property
    End Select
    mvarBoundaryCondition = vData
    Exit Property
BoundaryConditionLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cBondaryConditions: BoundaryCondition Property Let")
End Property

Public Property Get BoundaryCondition() As BoundaryCondition
    On Error GoTo BoundaryConditionGetErr
    BoundaryCondition = mvarBoundaryCondition
    Exit Property
BoundaryConditionGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cBondaryConditions: BoundaryCondition Property Get")
End Property

Public Property Let Edge(ByVal vData As Edge)
    On Error GoTo EdgeLetErr
    mvarEdge = vData
    Exit Property
EdgeLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGeometricalConstraints: Edge Property Let")
End Property

Public Property Get Edge() As Edge
    On Error GoTo EdgeGetErr
    Edge = mvarEdge
    Exit Property
EdgeGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGeometricalConstraints: Edge Property Get")
End Property

Public Function Clone() As cBoundaryConditions
    On Error GoTo CloneErr
    Set Clone = New cBoundaryConditions
    Clone.index = Me.index
    Clone.BoundaryCondition = Me.BoundaryCondition
    Clone.Edge = Me.Edge
    Exit Function
CloneErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cBoundaryConditions: Function Clone")
End Function

Public Function WriteASCII(ByVal iNoOfFile As Integer)
    On Error GoTo WriteASCIIErr
    With Me
        Put #iNoOfFile, , CInt(.index)
        Put #iNoOfFile, , CInt(.BoundaryCondition)
        Put #iNoOfFile, , CInt(.Edge)
    End With
    Exit Function
WriteASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cBoundaryConditions: Function WriteASCII")
End Function

Public Function ReadASCII(ByVal iNoOfFile As Integer)
    On Error GoTo ReadASCIIErr
    Dim ii As Integer
    With Me
        Get #iNoOfFile, , ii: .index = ii
        Get #iNoOfFile, , ii: .BoundaryCondition = ii
        Get #iNoOfFile, , ii: .Edge = ii
    End With
    Exit Function
ReadASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cBoundaryConditions: Function ReadASCII")
End Function

Public Function GetBoundaryName(ByVal i As Integer) As String
    On Error GoTo GetBoundaryNameErr
    Dim nameTable(1 To 12) As String
    nameTable(1) = "Free Edge"
    nameTable(2) = "Simply supported 2"
    nameTable(3) = "Simply supported 3"
    nameTable(4) = "Simply supported 4"
    nameTable(5) = "Simply supported 5"
    nameTable(6) = "Clamped"
    nameTable(7) = "Symmetry Axis 1"
    nameTable(8) = "Simply supported 6"
    nameTable(9) = "Simply supported 7"
    nameTable(10) = "Simply supported 8"
    nameTable(11) = "Symmetry Axis 2"
    nameTable(12) = "Double Symmetry"
    GetBoundaryName = nameTable(i)
    Exit Function
GetBoundaryNameErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cBoundaryConditions: Function GetBoundaryName")
    GetBoundaryName = "Free Edge"
End Function

Public Function SetBoundaryName(ByVal s As String) As Integer
    Select Case s
        Case "Free Edge"
            SetBoundaryName = 1
        Case "Simply supported 2"
            SetBoundaryName = 2
        Case "Simply supported 3"
            SetBoundaryName = 3
        Case "Simply supported 4"
            SetBoundaryName = 4
        Case "Simply supported 5"
            SetBoundaryName = 5
        Case "Clamped"
            SetBoundaryName = 6
        Case "Symmetry Axis 1"
            SetBoundaryName = 7
        Case "Simply supported 6"
            SetBoundaryName = 8
        Case "Simply supported 7"
            SetBoundaryName = 9
        Case "Simply supported 8"
            SetBoundaryName = 10
        Case "Symmetry Axis 2"
            SetBoundaryName = 11
        Case "Double Symmetry"
            SetBoundaryName = 12
        Case Else
            'SetBoundaryName = 1
    End Select
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

