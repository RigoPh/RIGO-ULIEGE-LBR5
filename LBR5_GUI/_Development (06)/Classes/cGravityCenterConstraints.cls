VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cGlobalConstraints"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

#Const DebugMode = 0
#If DebugMode Then
    Private mlClassDebugID As Long
#End If
Private mvarGravityLimitRestriction As GravityLimitRestriction
Private mvarMinGravityCenter As Double
Private mvarMaxGravityCenter As Double
Private mvarIsInertia As YesNo
Private mvarInertia As Double
Private mvarIsSectionModulus As YesNo
Private mvarSectionModulus As Double
Private mvarZPanel As Integer
Private mvarIsWeight As YesNo
Private mvarWeight As Double
Private mvarIsCost As YesNo
Private mvarCost As Double
Private mvarCostType As OptimizationType ' Never 0
Public Enum GravityLimitRestriction
    NoLimit = 0
    LowerLimit = 1
    UpperLimit = 2
    BothLimits = 3
End Enum

Public Property Let GravityLimitRestriction(ByVal vData As GravityLimitRestriction)
    On Error GoTo GravityLimitRestrictionLetErr
    mvarGravityLimitRestriction = vData
    Exit Property
GravityLimitRestrictionLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGlobalConstraints: GravityLimitRestriction Property Let")
End Property

Public Property Get GravityLimitRestriction() As GravityLimitRestriction
    On Error GoTo GravityLimitRestrictionGetErr
    GravityLimitRestriction = mvarGravityLimitRestriction
    Exit Property
GravityLimitRestrictionGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGlobalConstraints: GravityLimitRestriction Property Get")
End Property

Public Property Let MinGravityCenter(ByVal vData As Double)
    On Error GoTo MinGravityCenterLetErr
    mvarMinGravityCenter = vData
    Exit Property
MinGravityCenterLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGlobalConstraints: MinGravityCenter Property Let")
End Property

Public Property Get MinGravityCenter() As Double
    On Error GoTo MinGravityCenterGetErr
    MinGravityCenter = mvarMinGravityCenter
    Exit Property
MinGravityCenterGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGlobalConstraints: MinGravityCenter Property Get")
End Property

Public Property Let MaxGravityCenter(ByVal vData As Double)
    On Error GoTo MaxGravityCenterLetErr
    mvarMaxGravityCenter = vData
    Exit Property
MaxGravityCenterLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGlobalConstraints: MaxGravityCenter Property Let")
End Property

Public Property Get MaxGravityCenter() As Double
    On Error GoTo MaxGravityCenterGetErr
    MaxGravityCenter = mvarMaxGravityCenter
    Exit Property
MaxGravityCenterGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGlobalConstraints: MaxGravityCenter Property Get")
End Property

Public Property Let IsInertia(ByVal vData As YesNo)
    On Error GoTo IsInertiaLetErr
    mvarIsInertia = vData
    Exit Property
IsInertiaLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGlobalConstraints: IsInertia Property Let")
End Property

Public Property Get IsInertia() As YesNo
    On Error GoTo IsInertiaGetErr
    IsInertia = mvarIsInertia
    Exit Property
IsInertiaGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGlobalConstraints: IsInertia Property Get")
End Property

Public Property Let IsSectionModulus(ByVal vData As YesNo)
    On Error GoTo IsSectionModulusLetErr
    mvarIsSectionModulus = vData
    Exit Property
IsSectionModulusLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGlobalConstraints: IsSectionModulus Property Let")
End Property

Public Property Get IsSectionModulus() As YesNo
    On Error GoTo IsSectionModulusGetErr
    IsSectionModulus = mvarIsSectionModulus
    Exit Property
IsSectionModulusGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGlobalConstraints: IsSectionModulus Property Get")
End Property

Public Property Let IsWeight(ByVal vData As YesNo)
    On Error GoTo IsWeightLetErr
    mvarIsWeight = vData
    Exit Property
IsWeightLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGlobalConstraints: IsWeight Property Let")
End Property

Public Property Get IsWeight() As YesNo
    On Error GoTo IsWeightGetErr
    IsWeight = mvarIsWeight
    Exit Property
IsWeightGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGlobalConstraints: IsWeight Property Get")
End Property

Public Property Let IsCost(ByVal vData As YesNo)
    On Error GoTo IsCostLetErr
    mvarIsCost = vData
    Exit Property
IsCostLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGlobalConstraints: IsCost Property Let")
End Property

Public Property Get IsCost() As YesNo
    On Error GoTo IsCostGetErr
    IsCost = mvarIsCost
    Exit Property
IsCostGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGlobalConstraints: IsCost Property Get")
End Property

Public Property Let Inertia(ByVal vData As Double)
    On Error GoTo InertiaLetErr
    mvarInertia = vData
    Exit Property
InertiaLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGlobalConstraints: Inertia Property Let")
End Property

Public Property Get Inertia() As Double
    On Error GoTo InertiaGetErr
    Inertia = mvarInertia
    Exit Property
InertiaGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGlobalConstraints: Inertia Property Get")
End Property

Public Property Let SectionModulus(ByVal vData As Double)
    On Error GoTo SectionModulusLetErr
    mvarSectionModulus = vData
    Exit Property
SectionModulusLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGlobalConstraints: SectionModulus Property Let")
End Property

Public Property Get SectionModulus() As Double
    On Error GoTo SectionModulusGetErr
    SectionModulus = mvarSectionModulus
    Exit Property
SectionModulusGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGlobalConstraints: SectionModulus Property Get")
End Property

Public Property Let Weight(ByVal vData As Double)
    On Error GoTo WeightLetErr
    mvarWeight = vData
    Exit Property
WeightLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGlobalConstraints: Weight Property Let")
End Property

Public Property Get Weight() As Double
    On Error GoTo WeightGetErr
    Weight = mvarWeight
    Exit Property
WeightGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGlobalConstraints: Weight Property Get")
End Property

Public Property Let Cost(ByVal vData As Double)
    On Error GoTo CostLetErr
    mvarCost = vData
    Exit Property
CostLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGlobalConstraints: Cost Property Let")
End Property

Public Property Get Cost() As Double
    On Error GoTo CostGetErr
    Cost = mvarCost
    Exit Property
CostGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGlobalConstraints: Cost Property Get")
End Property

Public Property Let ZPanel(ByVal vData As Integer)
    On Error GoTo ZPanelLetErr
    mvarZPanel = vData
    Exit Property
ZPanelLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGlobalConstraints: ZPanel Property Let")
End Property

Public Property Get ZPanel() As Integer
    On Error GoTo ZPanelGetErr
    ZPanel = mvarZPanel
    Exit Property
ZPanelGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGlobalConstraints: ZPanel Property Get")
End Property

Public Property Let CostType(ByVal vData As OptimizationType)
    On Error GoTo CostTypeLetErr
    mvarCostType = vData
    Exit Property
CostTypeLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGlobalConstraints: CostType Property Let")
End Property

Public Property Get CostType() As OptimizationType
    On Error GoTo CostTypeGetErr
    CostType = mvarCostType
    Exit Property
CostTypeGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGlobalConstraints: CostType Property Get")
End Property

Public Function ReadLBR5txtFile(ts As TextStream)
    On Error GoTo ReadGlobalDataLBR5txtFileErr
    Dim sLine As String
    Dim v() As Variant
    sLine = ReadLn(ts)
    GetValues 3, sLine, v
    Me.GravityLimitRestriction = Val_(v(1))
    Me.MinGravityCenter = Val_(v(2))
    Me.MaxGravityCenter = Val_(v(3))
    Exit Function
ReadGlobalDataLBR5txtFileErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGlobalConstraints: Function ReadLBR5txtFile")
End Function

Public Function WriteLBR5txtFile(ts As TextStream)
    On Error GoTo ReadGlobalDataLBR5txtFileErr
    Dim sComment As String
    sComment = "IGRAV(NO:0;Min:1;MAX:2;Min et max:3).KGMIN.KGMAX"
    ts.WriteLine Me.GravityLimitRestriction & "  " & _
                PVal(Me.MinGravityCenter, "0.0000") & "  " & _
                PVal(Me.MaxGravityCenter, "0.0000") & getTabs(6) & sComment
    sComment = ""
    ts.WriteLine "INERTIA CONSTRAINT"
    ts.WriteLine Me.IsInertia & "  " & Me.Inertia
    ts.WriteLine "SECTION MODULUS CONSTRAINT"
    ts.WriteLine Me.IsSectionModulus & "  " & Me.SectionModulus & "  " & Me.ZPanel
    ts.WriteLine "WEIGHT CONSTRAINT"
    ts.WriteLine Me.IsWeight & "  " & Me.Weight
    ts.WriteLine "COST CONSTRAINT"
    ts.WriteLine Me.IsCost & "  " & Me.Cost & "  " & Me.CostType
    Exit Function
ReadGlobalDataLBR5txtFileErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGlobalConstraints: Function WriteLBR5txtFile")
End Function

Public Function Clone() As cGlobalConstraints
    On Error GoTo CloneErr
    Set Clone = New cGlobalConstraints
    With Clone
        .GravityLimitRestriction = Me.GravityLimitRestriction
        .MinGravityCenter = Me.MinGravityCenter
        .MaxGravityCenter = Me.MaxGravityCenter
        .IsInertia = Me.IsInertia
        .Inertia = Me.Inertia
        .IsSectionModulus = Me.IsSectionModulus
        .SectionModulus = Me.SectionModulus
        .ZPanel = Me.ZPanel
        .IsWeight = Me.IsWeight
        .Weight = Me.Weight
        .IsCost = Me.IsCost
        .Cost = Me.Cost
        .CostType = Me.CostType
    End With
    Exit Function
CloneErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGlobalConstraints: Function Clone")
End Function

Public Function WriteASCII(ByVal iNoOfFile As Integer)
    On Error GoTo WriteASCIIErr
    With Me
        Put #iNoOfFile, , CInt(.GravityLimitRestriction)
        Put #iNoOfFile, , .MinGravityCenter
        Put #iNoOfFile, , .MaxGravityCenter
        Put #iNoOfFile, , CInt(.IsInertia)
        Put #iNoOfFile, , .Inertia
        Put #iNoOfFile, , CInt(.IsSectionModulus)
        Put #iNoOfFile, , .SectionModulus
        Put #iNoOfFile, , .ZPanel
        Put #iNoOfFile, , CInt(.IsWeight)
        Put #iNoOfFile, , .Weight
        Put #iNoOfFile, , CInt(.IsCost)
        Put #iNoOfFile, , .Cost
        Put #iNoOfFile, , CInt(.CostType)
    End With
    Exit Function
WriteASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGlobalConstraints: Function WriteASCII")
End Function

Public Function ReadASCII(ByVal iNoOfFile As Integer)
    On Error GoTo ReadASCIIErr
    Dim ii As Integer, dd As Double
    With Me
        Get #iNoOfFile, , ii: .GravityLimitRestriction = ii
        Get #iNoOfFile, , dd: .MinGravityCenter = dd
        Get #iNoOfFile, , dd: .MaxGravityCenter = dd
    End With
    Exit Function
ReadASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGlobalConstraints: Function ReadASCII")
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
