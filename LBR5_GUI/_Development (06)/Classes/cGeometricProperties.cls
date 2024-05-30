VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cGeometricProperties"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
#Const DebugMode = 0
#If DebugMode Then
    Private mlClassDebugID As Long
#End If
' Class Members
Private mvarCrossSectionNet As Double
Private mvarCrossSectionGross As Double
Private mvarGravityCenterNet As Double
Private mvarGravityCenterGross As Double
Private mvarNeutralAxisNet As Double
Private mvarNeutralAxisGross As Double
Private mvarInertiaYYNet As Double
Private mvarInertiaYYGross As Double
Private mvarSectionModulusNet As Double
Private mvarSectionModulusGross As Double

Public Property Let CrossSectionNet(ByVal vData As Double)
    On Error GoTo CrossSectionNetLetErr
    mvarCrossSectionNet = vData
    Exit Property
CrossSectionNetLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGeometricProperties: CrossSectionNet Property Let")
End Property

Public Property Get CrossSectionNet() As Double
    On Error GoTo CrossSectionNetGetErr
    CrossSectionNet = mvarCrossSectionNet
    Exit Property
CrossSectionNetGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGeometricProperties: CrossSectionNet Property Get")
End Property

Public Property Let CrossSectionGross(ByVal vData As Double)
    On Error GoTo CrossSectionGrossLetErr
    mvarCrossSectionGross = vData
    Exit Property
CrossSectionGrossLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGeometricProperties: CrossSectionGross Property Let")
End Property

Public Property Get CrossSectionGross() As Double
    On Error GoTo CrossSectionGrossGetErr
    CrossSectionGross = mvarCrossSectionGross
    Exit Property
CrossSectionGrossGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGeometricProperties: CrossSectionGross Property Get")
End Property

Public Property Let NeutralAxisNet(ByVal vData As Double)
    On Error GoTo NeutralAxisNetLetErr
    mvarNeutralAxisNet = vData
    Exit Property
NeutralAxisNetLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGeometricProperties: NeutralAxisNet Property Let")
End Property

Public Property Get NeutralAxisNet() As Double
    On Error GoTo NeutralAxisNetGetErr
    NeutralAxisNet = mvarNeutralAxisNet
    Exit Property
NeutralAxisNetGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGeometricProperties: NeutralAxisNet Property Get")
End Property

Public Property Let NeutralAxisGross(ByVal vData As Double)
    On Error GoTo NeutralAxisGrossLetErr
    mvarNeutralAxisGross = vData
    Exit Property
NeutralAxisGrossLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGeometricProperties: NeutralAxisGross Property Let")
End Property

Public Property Get NeutralAxisGross() As Double
    On Error GoTo NeutralAxisGrossGetErr
    NeutralAxisGross = mvarNeutralAxisGross
    Exit Property
NeutralAxisGrossGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGeometricProperties: NeutralAxisGross Property Get")
End Property

Public Property Let GravityCenterNet(ByVal vData As Double)
    On Error GoTo GravityCenterNetLetErr
    mvarGravityCenterNet = vData
    Exit Property
GravityCenterNetLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGeometricProperties: GravityCenterNet Property Let")
End Property

Public Property Get GravityCenterNet() As Double
    On Error GoTo GravityCenterNetGetErr
    GravityCenterNet = mvarGravityCenterNet
    Exit Property
GravityCenterNetGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGeometricProperties: GravityCenterNet Property Get")
End Property

Public Property Let GravityCenterGross(ByVal vData As Double)
    On Error GoTo GravityCenterGrossLetErr
    mvarGravityCenterGross = vData
    Exit Property
GravityCenterGrossLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGeometricProperties: GravityCenterGross Property Let")
End Property

Public Property Get GravityCenterGross() As Double
    On Error GoTo GravityCenterGrossGetErr
    GravityCenterGross = mvarGravityCenterGross
    Exit Property
GravityCenterGrossGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGeometricProperties: GravityCenterGross Property Get")
End Property

Public Property Let InertiaYYNet(ByVal vData As Double)
    On Error GoTo InertiaYYNetLetErr
    mvarInertiaYYNet = vData
    Exit Property
InertiaYYNetLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGeometricProperties: InertiaYYNet Property Let")
End Property

Public Property Get InertiaYYNet() As Double
    On Error GoTo InertiaYYNetGetErr
    InertiaYYNet = mvarInertiaYYNet
    Exit Property
InertiaYYNetGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGeometricProperties: InertiaYYNet Property Get")
End Property

Public Property Let InertiaYYGross(ByVal vData As Double)
    On Error GoTo InertiaYYGrossLetErr
    mvarInertiaYYGross = vData
    Exit Property
InertiaYYGrossLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGeometricProperties: InertiaYYGross Property Let")
End Property

Public Property Get InertiaYYGross() As Double
    On Error GoTo InertiaYYGrossGetErr
    InertiaYYGross = mvarInertiaYYGross
    Exit Property
InertiaYYGrossGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGeometricProperties: InertiaYYGross Property Get")
End Property

Public Property Let SectionModulusNet(ByVal vData As Double)
    On Error GoTo SectionModulusNetLetErr
    mvarSectionModulusNet = vData
    Exit Property
SectionModulusNetLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGeometricProperties: SectionModulusNet Property Let")
End Property

Public Property Get SectionModulusNet() As Double
    On Error GoTo SectionModulusNetGetErr
    SectionModulusNet = mvarSectionModulusNet
    Exit Property
SectionModulusNetGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGeometricProperties: SectionModulusNet Property Get")
End Property

Public Property Let SectionModulusGross(ByVal vData As Double)
    On Error GoTo SectionModulusGrossLetErr
    mvarSectionModulusGross = vData
    Exit Property
SectionModulusGrossLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGeometricProperties: SectionModulusGross Property Let")
End Property

Public Property Get SectionModulusGross() As Double
    On Error GoTo SectionModulusGrossGetErr
    SectionModulusGross = mvarSectionModulusGross
    Exit Property
SectionModulusGrossGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGeometricProperties: SectionModulusGross Property Get")
End Property

Public Function SetGeometricProperties(ByVal ProjectIndex As Integer) As Boolean
    On Error GoTo SetGeometricPropertiesErr
    Dim oPanel As cPanel, oGirder As cGirder
    Dim sectst1 As Double, sectst2 As Double, sectfr1 As Double, sectfr2 As Double, sectgr As Double
    Dim spst1 As Double, spst2 As Double, spfr1 As Double, spfr2 As Double, larg As Double
    Dim nrst1 As Double, nrst2 As Double, nrfr1 As Double, nrfr2 As Double
    Dim plth As Double, eqplth As Double
    Dim icorrth As Integer 'corrosion thickness (zero if not considered)
    Dim AiDENSi As Double, ZiAiDENSi As Double
    Dim Zi As Double, AI As Double, AiZi As Double, SumAi As Double
    Dim z1 As Double, z2 As Double, Zg As Double, ZZg As Double
    
    For Each oPanel In Project.Item(ProjectIndex).colPanel
    
    Next oPanel
    
    Exit Function
SetGeometricPropertiesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGeometricProperties: Function SetGeometricProperties")
End Function

Public Function Clone() As cGeometricProperties
    On Error GoTo CloneErr
    Set Clone = New cGeometricProperties
    With Clone
        .CrossSectionNet = Me.CrossSectionNet
        .CrossSectionGross = Me.CrossSectionGross
        .GravityCenterNet = Me.GravityCenterNet
        .GravityCenterGross = Me.GravityCenterGross
        .NeutralAxisNet = Me.NeutralAxisNet
        .NeutralAxisGross = Me.NeutralAxisGross
        .InertiaYYNet = Me.InertiaYYNet
        .InertiaYYGross = Me.InertiaYYGross
        .SectionModulusNet = Me.SectionModulusNet
        .SectionModulusGross = Me.SectionModulusGross
    End With
    Exit Function
CloneErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGeometricProperties: Function Clone")
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




