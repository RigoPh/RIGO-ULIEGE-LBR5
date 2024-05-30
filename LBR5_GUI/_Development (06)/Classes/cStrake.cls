VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cStrake"
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
Private mvarWidthRatio As Double
Private mvarGrossThickness As Double
Private mvarNetThickness As Double
Private mvarOpeningCoefficient As Double
Private mvarMaterialNumber As Integer

Public Property Let index(ByVal vData As Integer)
    On Error GoTo IndexLetErr
    mvarIndex = vData
    Exit Property
IndexLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cStrake: Index Property Let")
End Property

Public Property Get index() As Integer
    On Error GoTo IndexGetErr
    index = mvarIndex
    Exit Property
IndexGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cStrake: Index Property Get")
End Property

Public Property Let WidthRatio(ByVal vData As Double)
    On Error GoTo WidthRatioLetErr
    mvarWidthRatio = vData
    Exit Property
WidthRatioLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cStrake: WidthRatio Property Let")
End Property

Public Property Get WidthRatio() As Double
    On Error GoTo WidthRatioGetErr
    WidthRatio = mvarWidthRatio
    Exit Property
WidthRatioGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cStrake: WidthRatio Property Get")
End Property

Public Property Let GrossThickness(ByVal vData As Double)
    On Error GoTo GrossThicknessLetErr
    mvarGrossThickness = vData
    Exit Property
GrossThicknessLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cStrake: GrossThickness Property Let")
End Property

Public Property Get GrossThickness() As Double
    On Error GoTo GrossThicknessGetErr
    GrossThickness = mvarGrossThickness
    Exit Property
GrossThicknessGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cStrake: GrossThickness Property Get")
End Property

Public Property Let NetThickness(ByVal vData As Double)
    On Error GoTo NetThicknessLetErr
    mvarNetThickness = vData
    Exit Property
NetThicknessLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cStrake: NetThickness Property Let")
End Property

Public Property Get NetThickness() As Double
    On Error GoTo NetThicknessGetErr
    NetThickness = mvarNetThickness
    Exit Property
NetThicknessGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cStrake: NetThickness Property Get")
End Property

Public Property Let OpeningCoefficient(ByVal vData As Double)
    On Error GoTo OpeningCoefficientLetErr
    mvarOpeningCoefficient = vData
    Exit Property
OpeningCoefficientLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cStrake: OpeningCoefficient Property Let")
End Property

Public Property Get OpeningCoefficient() As Double
    On Error GoTo OpeningCoefficientGetErr
    OpeningCoefficient = mvarOpeningCoefficient
    Exit Property
OpeningCoefficientGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cStrake: OpeningCoefficient Property Get")
End Property

Public Property Let MaterialNumber(ByVal vData As Integer)
    On Error GoTo MaterialNumberLetErr
    mvarMaterialNumber = vData
    Exit Property
MaterialNumberLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cStrake: MaterialNumber Property Let")
End Property

Public Property Get MaterialNumber() As Integer
    On Error GoTo MaterialNumberGetErr
    MaterialNumber = mvarMaterialNumber
    Exit Property
MaterialNumberGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cStrake: MaterialNumber Property Get")
End Property

Public Function ReadMARS_LBR5TransfertFile(ts As TextStream)
    On Error GoTo ReadMARS_LBR5TransfertFileErr
    Dim sLine As String
    Dim v() As Variant
    sLine = ReadLn(ts)
    GetValues 6, sLine, v
    Me.index = Val_(v(1))
    Me.WidthRatio = Val_(v(2))
    Me.GrossThickness = Val_(v(3)) / 1000
    Me.NetThickness = Val_(v(4)) / 1000
    'Me.OpeningCoefficient = Val_(V(5))
    Me.MaterialNumber = Val_(v(6))
    Exit Function
ReadMARS_LBR5TransfertFileErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cStrake: Function ReadMARS_LBR5TransfertFile")
End Function

Public Function Clone() As cStrake
    On Error GoTo CloneErr
    Set Clone = New cStrake
    With Clone
        .index = Me.index
        .WidthRatio = Me.WidthRatio
        .GrossThickness = Me.GrossThickness
        .NetThickness = Me.NetThickness
        .OpeningCoefficient = Me.OpeningCoefficient
        .MaterialNumber = Me.MaterialNumber
    End With
    Exit Function
CloneErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cStrake: Function Clone")
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
