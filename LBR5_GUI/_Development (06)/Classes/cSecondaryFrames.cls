VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cSecondaryFrames"
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
Private mvarSpacing As Double
Private mvarWebHeight As Double
Private mvarWebThickness As Double
Private mvarFlangeWidth As Double
Private mvarFlangeThickness As Double
Private mvarSide As Side
Private mvarProfile As Profile
Private mvarWidthRatio As Double
Private mvarCorrosionThickness As Double
Private mvarMaterialNumber As Integer

Public Property Let index(ByVal vData As Integer)
    On Error GoTo IndexLetErr
    mvarIndex = vData
    Exit Property
IndexLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSecondaryFrames: Side Property Let")
End Property

Public Property Get index() As Integer
    On Error GoTo IndexGetErr
    index = mvarIndex
    Exit Property
IndexGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSecondaryFrames: Index Property Get")
End Property

Public Property Let Side(ByVal vData As Side)
    On Error GoTo SideLetErr
    Select Case vData
        Case SideLeft, SideRight, SideNone
        Case Else
            Exit Property
    End Select
    mvarSide = vData
    Exit Property
SideLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSecondaryFrames: Side Property Let")
End Property

Public Property Get Side() As Side
    On Error GoTo SideGetErr
    Side = mvarSide
    Exit Property
SideGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSecondaryFrames: Side Property Get")
End Property

Public Property Let FlangeThickness(ByVal vData As Double)
    On Error GoTo FlangeThicknessLetErr
    mvarFlangeThickness = vData
    Exit Property
FlangeThicknessLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSecondaryFrames: Side Property Let")
End Property

Public Property Get FlangeThickness() As Double
    On Error GoTo FlangeThicknessGetErr
    FlangeThickness = mvarFlangeThickness
    Exit Property
FlangeThicknessGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSecondaryFrames: FlangeThickness Property Get")
End Property

Public Property Let FlangeWidth(ByVal vData As Double)
    On Error GoTo FlangeWidthLetErr
    mvarFlangeWidth = vData
    Exit Property
FlangeWidthLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSecondaryFrames: FlangeWidth Property Let")
End Property

Public Property Get FlangeWidth() As Double
    On Error GoTo FlangeWidthGetErr
    FlangeWidth = mvarFlangeWidth
    Exit Property
FlangeWidthGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSecondaryFrames: FlangeWidth Property Get")
End Property

Public Property Let WebThickness(ByVal vData As Double)
    On Error GoTo WebThicknessLetErr
    mvarWebThickness = vData
    Exit Property
WebThicknessLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSecondaryFrames: WebThickness Property Let")
End Property

Public Property Get WebThickness() As Double
    On Error GoTo WebThicknessGetErr
    WebThickness = mvarWebThickness
    Exit Property
WebThicknessGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSecondaryFrames: WebThickness Property Get")
End Property

Public Property Let WebHeight(ByVal vData As Double)
    On Error GoTo WebHeightLetErr
    mvarWebHeight = vData
    Exit Property
WebHeightLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSecondaryFrames: WebHeight Property Let")
End Property

Public Property Get WebHeight() As Double
    On Error GoTo WebHeightGetErr
    WebHeight = mvarWebHeight
    Exit Property
WebHeightGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSecondaryFrames: WebHeight Property Get")
End Property

Public Property Let Spacing(ByVal vData As Double)
    On Error GoTo SpacingLetErr
    mvarSpacing = vData
    Exit Property
SpacingLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSecondaryFrames: Spacing Property Let")
End Property

Public Property Get Spacing() As Double
    On Error GoTo SpacingGetErr
    Spacing = mvarSpacing
    Exit Property
SpacingGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSecondaryFrames: Spacing Property Get")
End Property

Public Property Let Profile(ByVal vData As Profile)
    On Error GoTo ProfileLetErr
    mvarProfile = vData
    Exit Property
ProfileLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSecondaryFrames: Profile Property Let")
End Property

Public Property Get Profile() As Profile
    On Error GoTo ProfileGetErr
    Profile = mvarProfile
    Exit Property
ProfileGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSecondaryFrames: Profile Property Get")
End Property

Public Property Let WidthRatio(ByVal vData As Double)
    On Error GoTo WidthRatioLetErr
    mvarWidthRatio = vData
    Exit Property
WidthRatioLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSecondaryFrames: WidthRatio Property Let")
End Property

Public Property Get WidthRatio() As Double
    On Error GoTo WidthRatioGetErr
    WidthRatio = mvarWidthRatio
    Exit Property
WidthRatioGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSecondaryFrames: WidthRatio Property Get")
End Property

Public Property Let CorrosionThickness(ByVal vData As Double)
    On Error GoTo CorrosionThicknessLetErr
    mvarCorrosionThickness = vData
    Exit Property
CorrosionThicknessLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSecondaryFrames: CorrosionThickness Property Let")
End Property

Public Property Get CorrosionThickness() As Double
    On Error GoTo CorrosionThicknessGetErr
    CorrosionThickness = mvarCorrosionThickness
    Exit Property
CorrosionThicknessGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSecondaryFrames: CorrosionThickness Property Get")
End Property

Public Property Let MaterialNumber(ByVal vData As Integer)
    On Error GoTo MaterialNumberLetErr
    mvarMaterialNumber = vData
    Exit Property
MaterialNumberLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSecondaryFrames: MaterialNumber Property Let")
End Property

Public Property Get MaterialNumber() As Integer
    On Error GoTo MaterialNumberGetErr
    MaterialNumber = mvarMaterialNumber
    Exit Property
MaterialNumberGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSecondaryFrames: MaterialNumber Property Get")
End Property

Public Function ReadMARS_LBR5TransfertFile(ts As TextStream)
    On Error GoTo ReadMARS_LBR5TransfertFileErr
    Dim sLine As String
    Dim v() As Variant
    sLine = ReadLn(ts)
    GetValues 11, sLine, v
    Me.index = Val_(v(1))
    Select Case v(2)
    Case "F"
        Me.Profile = profileFlat
    Case "B"
        Me.Profile = profileBulb
    Case "A"
        Me.Profile = profileAngle
    Case "T"
        Me.Profile = profileTBar
    Case "N"
        Me.Profile = profileNull
    End Select
    Me.WidthRatio = Val_(v(3))
    Me.Spacing = Val_(v(4))
    Me.WebHeight = Val_(v(5))
    Me.WebThickness = Val_(v(6))
    Me.FlangeWidth = Val_(v(7))
    Me.FlangeThickness = Val_(v(8))
    If Me.Profile = profileBulb Then
        GetTEquivalentSection mvarWebHeight, mvarWebThickness, mvarFlangeWidth, mvarFlangeThickness
    End If
    Me.CorrosionThickness = Val_(v(9))
    Select Case Val_(v(10))
        Case 1
            Me.Side = SideRight
        Case 2
            Me.Side = SideLeft
    End Select
    'Me.Side = Val_(V(10))
    Me.MaterialNumber = Val_(v(11))
    Exit Function
ReadMARS_LBR5TransfertFileErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSecondaryFrames: ReadMARS_LBR5TransfertFile")
End Function

Public Function Clone() As cSecondaryFrames
    On Error GoTo CloneErr
    Set Clone = New cSecondaryFrames
    Clone.index = Me.index
    Clone.Spacing = Me.Spacing
    Clone.WebHeight = Me.WebHeight
    Clone.WebThickness = Me.WebThickness
    Clone.FlangeWidth = Me.FlangeWidth
    Clone.FlangeThickness = Me.FlangeThickness
    Clone.Side = Me.Side
    Clone.Profile = Me.Profile
    Clone.WidthRatio = Me.WidthRatio
    Clone.CorrosionThickness = Me.CorrosionThickness
    Clone.MaterialNumber = Me.MaterialNumber
    Exit Function
CloneErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSecondaryFrames: Function Clone")
End Function

Public Function WriteASCII(ByVal iNoOfFile As Integer)
    On Error GoTo WriteASCIIErr
    With Me
        Put #iNoOfFile, , .Spacing
        Put #iNoOfFile, , .WebHeight
        Put #iNoOfFile, , .WebThickness
        Put #iNoOfFile, , .FlangeWidth
        Put #iNoOfFile, , .FlangeThickness
        Put #iNoOfFile, , .CorrosionThickness
        Put #iNoOfFile, , CInt(.Side)
    End With
    Exit Function
WriteASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSecondaryFrames: Function WriteASCII")
End Function

Public Function ReadASCII(ByVal iNoOfFile As Integer)
    On Error GoTo ReadASCIIErr
    Dim ii As Integer, dd As Double
    With Me
        Get #iNoOfFile, , dd: .Spacing = dd
        Get #iNoOfFile, , dd: .WebHeight = dd
        Get #iNoOfFile, , dd: .WebThickness = dd
        Get #iNoOfFile, , dd: .FlangeWidth = dd
        Get #iNoOfFile, , dd: .FlangeThickness = dd
        Get #iNoOfFile, , dd: .CorrosionThickness = dd
        Get #iNoOfFile, , ii: .Side = ii
    End With
    Exit Function
ReadASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSecondaryFrames: Function ReadASCII")
End Function

Public Function GetTEquivalentSection(ByRef HW As Double, ByRef Tw As Double, ByRef Bf As Double, ByRef Tf As Double)
    'According to Bureau Veritas 2003 specifications
    On Error GoTo GetTEquivalentSectionErr
    Dim Hw_b As Double, Tw_b As Double
    Hw_b = HW
    Tw_b = Tw
    Dim alfa As Double
    HW = Hw_b - Hw_b / 9.2 + 2
    Tw = Tw_b
    Select Case Hw_b
        Case Is <= 120
            alfa = 1.1 + ((120 - Hw_b) ^ 2) / 3000
        Case Is > 120
            alfa = 1
    End Select
    Bf = alfa * (Tw_b + Hw_b / 6.7 - 2)
    Tf = Hw_b / 9.2 - 2
    HW = CInt(HW)
    Tw = CInt(Tw)
    Bf = CInt(Bf)
    Tf = CInt(Tf)
    Exit Function
GetTEquivalentSectionErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSecondaryFrames: Function GetTEquivalentSection")
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
