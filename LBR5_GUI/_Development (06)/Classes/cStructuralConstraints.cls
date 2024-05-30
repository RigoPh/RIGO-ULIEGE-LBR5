VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cStructuralConstraints"
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
Private mvarReference As Integer
Private mvarValue As Double
Private mvarLimit As Limit
Private mvarAssesmentPoint As Integer
Private mvarPoint As Double ' assessment point value
Public Enum Limit
    LowerLimit = 0
    UpperLimit = 1
End Enum

'Class members properties
Public Property Let index(ByVal vData As Integer)
    On Error GoTo IndexLetErr
    mvarIndex = vData
    Exit Property
IndexLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cStructuralConstraints: Index Property Let")
End Property

Public Property Get index() As Integer
    On Error GoTo IndexGetErr
    index = mvarIndex
    Exit Property
IndexGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cStructuralConstraints: Index Property Get")
End Property

Public Property Let Reference(vData As Integer)
    On Error GoTo ReferenceLetErr
    mvarReference = vData
    Exit Property
ReferenceLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cStructuralConstraints: Reference Property Let")
End Property

Public Property Get Reference() As Integer
    On Error GoTo ReferenceGetErr
    Reference = mvarReference
    Exit Property
ReferenceGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cStructuralConstraints: Reference Property Get")
End Property

Public Property Let Value(vData As Double)
    On Error GoTo ValueLetErr
    mvarValue = vData
    Exit Property
ValueLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cStructuralConstraints: Value Property Let")
End Property

Public Property Get Value() As Double
    On Error GoTo ValueGetErr
    Value = mvarValue
    Exit Property
ValueGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cStructuralConstraints: Value Property Get")
End Property

Public Property Let Limit(ByVal vData As Limit)
    On Error GoTo LimitLetErr
    mvarLimit = vData
    Exit Property
LimitLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cStructuralConstraints: Limit Property Let")
End Property

Public Property Get Limit() As Limit
    On Error GoTo LimitGetErr
    Limit = mvarLimit
    Exit Property
LimitGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cStructuralConstraints: Limit Property Get")
End Property

Public Property Let AssesmentPoint(ByVal vData As Integer)
    On Error GoTo AssesmentPointLetErr
    mvarAssesmentPoint = vData
    Exit Property
AssesmentPointLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cStructuralConstraints: AssesmentPoint Property Let")
End Property

Public Property Get AssesmentPoint() As Integer
    On Error GoTo AssesmentPointGetErr
    AssesmentPoint = mvarAssesmentPoint
    Exit Property
AssesmentPointGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cStructuralConstraints: AssesmentPoint Property Get")
End Property

Public Property Let Point(vData As Double)
    On Error GoTo PointLetErr
    mvarPoint = vData
    Exit Property
PointLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cStructuralConstraints: Point Property Let")
End Property

Public Property Get Point() As Double
    On Error GoTo PointGetErr
    Point = mvarPoint
    Exit Property
PointGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cStructuralConstraints: Point Property Get")
End Property

Public Function ReadLBR5txtFile(ts As TextStream)
    On Error GoTo ReadLBR5txtFileErr
    Dim sLine As String
    Dim v() As Variant
    sLine = ReadLn(ts)
    GetValues 5, sLine, v
    Me.index = Val_(v(1))
    Me.Reference = Val_(v(2))
    Me.Value = Val_(v(3))
    Me.Limit = Val_(v(4))
    Me.AssesmentPoint = Val_(v(5))
    Exit Function
ReadLBR5txtFileErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cStructuralConstraints: Function ReadLBR5txtFile")
End Function

Public Function Clone() As cStructuralConstraints
    On Error GoTo CloneErr
    Set Clone = New cStructuralConstraints
    Clone.index = Me.index
    Clone.Reference = Me.Reference
    Clone.Value = Me.Value
    Clone.Limit = Me.Limit
    Clone.AssesmentPoint = Me.AssesmentPoint
    Clone.Point = Me.Point
    Exit Function
CloneErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cStructuralConstraints: Function Clone")
End Function

Public Function WriteASCII(ByVal iNoOfFile As Integer)
    On Error GoTo WriteASCIIErr
    With Me
        Put #iNoOfFile, , .index
        Put #iNoOfFile, , .Reference
        Put #iNoOfFile, , .Value
        Put #iNoOfFile, , CInt(.Limit)
        Put #iNoOfFile, , .AssesmentPoint
        Put #iNoOfFile, , .Point
    End With
    Exit Function
WriteASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cStructuralConstraints: Function WriteASCII")
End Function

Public Function ReadASCII(ByVal iNoOfFile As Integer)
    On Error GoTo ReadASCIIErr
    Dim ii As Integer, dd As Double
    With Me
        Get #iNoOfFile, , ii: .index = ii
        Get #iNoOfFile, , ii: .Reference = ii
        Get #iNoOfFile, , dd: .Value = dd
        Get #iNoOfFile, , ii: .Limit = ii
        Get #iNoOfFile, , ii: .AssesmentPoint = ii
        Get #iNoOfFile, , dd: .Point = dd
    End With
    Exit Function
ReadASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cStructuralConstraints: Function ReadASCII")
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

