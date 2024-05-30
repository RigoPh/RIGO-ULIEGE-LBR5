VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cMaterial"
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
Private mvarmNumber As Integer
Private mvarName As String
Private mvarYoungModulus As Double
Private mvarPoisson As Double
Private mvarYieldStress As Double
Private mvarAllowableStress As Double
Private mvarSpecificWeight As Double
Private mvarMaterialCoeff As Double

Public Property Let Index(ByVal vData As Integer)
    On Error GoTo IndexLetErr
    mvarIndex = vData
    Exit Property
IndexLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMaterial: Index Property Let")
End Property

Public Property Get Index() As Integer
    On Error GoTo IndexGetErr
    Index = mvarIndex
    Exit Property
IndexGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMaterial: Index Property Get")
End Property

Public Property Let mNumber(ByVal vData As Integer)
    On Error GoTo mNumberLetErr
    mvarmNumber = vData
    Exit Property
mNumberLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMaterial: mNumber Property Let")
End Property

Public Property Get mNumber() As Integer
    On Error GoTo mNumberGetErr
    mNumber = mvarmNumber
    Exit Property
mNumberGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMaterial: mNumber Property Get")
End Property

Public Property Let Name(ByVal vData As String)
    On Error GoTo NameLetErr
    mvarName = vData
    Exit Property
NameLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMaterial: Name Property Let")
End Property

Public Property Get Name() As String
    On Error GoTo NameGetErr
    Name = mvarName
    Exit Property
NameGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMaterial: Name Property Get")
End Property

Public Property Let SpecificWeight(ByVal vData As Double)
    On Error GoTo SpecificWeightLetErr
    mvarSpecificWeight = vData
    Exit Property
SpecificWeightLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMaterial: SpecificWeight Property Let")
End Property

Public Property Get SpecificWeight() As Double
    On Error GoTo SpecificWeightGetErr
    SpecificWeight = mvarSpecificWeight
    Exit Property
SpecificWeightGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMaterial: SpecificWeight Property Get")
End Property

Public Property Let MaterialCoeff(ByVal vData As Double)
    On Error GoTo MaterialCoeffLetErr
    mvarMaterialCoeff = vData
    Exit Property
MaterialCoeffLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMaterial: MaterialCoeff Property Let")
End Property

Public Property Get MaterialCoeff() As Double
    On Error GoTo MaterialCoeffGetErr
    MaterialCoeff = mvarMaterialCoeff
    Exit Property
MaterialCoeffGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMaterial: MaterialCoeff Property Get")
End Property

Public Property Let AllowableStress(ByVal vData As Double)
    On Error GoTo AllowableStressLetErr
    mvarAllowableStress = vData
    Exit Property
AllowableStressLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMaterial: AllowableStress Property Let")
End Property

Public Property Get AllowableStress() As Double
    On Error GoTo AllowableStressGetErr
    AllowableStress = mvarAllowableStress
    Exit Property
AllowableStressGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMaterial: AllowableStress Property Get")
End Property

Public Property Let YieldStress(ByVal vData As Double)
    On Error GoTo YieldStressLetErr
    mvarYieldStress = vData
    Exit Property
YieldStressLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMaterial: YieldStress Property Let")
End Property

Public Property Get YieldStress() As Double
    On Error GoTo YieldStressGetErr
    YieldStress = mvarYieldStress
    Exit Property
YieldStressGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMaterial: YieldStress Property Get")
End Property

Public Property Let Poisson(ByVal vData As Double)
    On Error GoTo PoissonLetErr
    mvarPoisson = vData
    Exit Property
PoissonLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMaterial: Poisson Property Let")
End Property

Public Property Get Poisson() As Double
    On Error GoTo PoissonGetErr
    Poisson = mvarPoisson
    Exit Property
PoissonGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMaterial: Poisson Property Get")
End Property

Public Property Let YoungModulus(ByVal vData As Double)
    On Error GoTo YoungModulusLetErr
    mvarYoungModulus = vData
    Exit Property
YoungModulusLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMaterial: YoungModulus Property Let")
End Property

Public Property Get YoungModulus() As Double
    On Error GoTo YoungModulusGetErr
    YoungModulus = mvarYoungModulus
    Exit Property
YoungModulusGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMaterial: YoungModulus Property Get")
End Property

Public Function ReadLBR5txtFile(ts As TextStream)
    On Error GoTo ReadLBR5txtFileErr
    Dim sLine As String
    Dim v() As Variant
    sLine = ReadLn(ts)
    GetValues 5, sLine, v
    Me.YoungModulus = Val_(v(1))
    Me.Poisson = Val_(v(2))
    Me.YieldStress = Val_(v(3))
    Me.AllowableStress = Val_(v(4))
    Me.SpecificWeight = Val_(v(5))
    Exit Function
ReadLBR5txtFileErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMaterial: Function ReadLBR5txtFile")
End Function

Public Function ReadMARS_LBR5TransfertFile(ts As TextStream)
    On Error GoTo ReadMARS_LBR5TransfertFileErr
    Dim sLine As String
    Dim v() As Variant
    sLine = ReadLn(ts)
    GetValues 4, sLine, v
    Me.Index = Val_(v(1))
    Me.mNumber = Val_(v(1))
    Me.Name = v(2)
    Me.YieldStress = Val_(v(3)) * 1000000
    Me.YoungModulus = Val_(v(4)) * 1000000
    Me.Poisson = 0.3
    Me.AllowableStress = 175 * 1000000
    Exit Function
ReadMARS_LBR5TransfertFileErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMaterial: ReadMARS_LBR5TransfertFile")
End Function

Public Function WriteLBR5txtFile(ts As TextStream)
    On Error GoTo WriteLBR5txtFileErr
    Dim sComment As String
    If Project.Item(ActiveProject).cHeader.IANA = 1 Then
        sComment = "E. Poisson. Sy. Sadm. Poids Specifique"
        ts.WriteLine Me.YoungModulus & "  " & PVal(Me.Poisson, "0.00") & "  " & _
                Me.YieldStress & "  " & Me.AllowableStress & "  " & _
                Me.SpecificWeight & getTabs(3) & sComment
    ElseIf Project.Item(ActiveProject).cHeader.IANA = 2 Then
        sComment = "E. Poisson. Sy. k. Poids Specifique"
        ts.WriteLine Me.YoungModulus & "  " & PVal(Me.Poisson, "0.00") & "  " & _
                Me.YieldStress & "  " & Me.MaterialCoeff & "  " & _
                Me.SpecificWeight & getTabs(3) & sComment
    End If

    Exit Function
WriteLBR5txtFileErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMaterial: Function WriteLBR5txtFile")
End Function

Public Function Clone() As cMaterial
    On Error GoTo CloneErr
    Set Clone = New cMaterial
    Clone.Index = Me.Index
    Clone.mNumber = Me.mNumber
    Clone.Name = Me.Name
    Clone.YoungModulus = Me.YoungModulus
    Clone.Poisson = Me.Poisson
    Clone.YieldStress = Me.YieldStress
    Clone.AllowableStress = Me.AllowableStress
    Clone.SpecificWeight = Me.SpecificWeight
    Clone.MaterialCoeff = Me.MaterialCoeff
    Exit Function
CloneErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMaterial: Function Clone")
End Function

Public Function WriteASCII(ByVal iNoOfFile As Integer)
    On Error GoTo WriteASCIIErr
    Dim ss As String * 100
    With Me
        Put #iNoOfFile, , .Index
        Put #iNoOfFile, , .mNumber
        ss = .Name
        Put #iNoOfFile, , ss
        Put #iNoOfFile, , .YoungModulus
        Put #iNoOfFile, , .Poisson
        Put #iNoOfFile, , .YieldStress
        Put #iNoOfFile, , .AllowableStress
        Put #iNoOfFile, , .MaterialCoeff
        Put #iNoOfFile, , .SpecificWeight
    End With
    Exit Function
WriteASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMaterial: Function WriteASCII")
End Function

Public Function ReadASCII(ByVal iNoOfFile As Integer)
    On Error GoTo ReadASCIIErr
    Dim ii As Integer, dd As Double, ss As String * 100
    With Me
        Get #iNoOfFile, , ii: .Index = ii
        Get #iNoOfFile, , ii: .mNumber = ii
        Get #iNoOfFile, , ss: .Name = ss
        Get #iNoOfFile, , dd: .YoungModulus = dd
        Get #iNoOfFile, , dd: .Poisson = dd
        Get #iNoOfFile, , dd: .YieldStress = dd
        Get #iNoOfFile, , dd: .AllowableStress = dd
        Get #iNoOfFile, , dd: .SpecificWeight = dd
    End With
    Exit Function
ReadASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cMaterial: Function ReadASCII")
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

