VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 1  'vbSimpleBound
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cHeader"
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
'General Data
Private mvarTitle As String
Private mvarIMPR As Integer
Private mvarIMPR2 As Integer
Private mvarINDAIG As Integer
Private mvarINDRAID As Integer
Private mvarDESSIN As Integer
Private mvarJLPH As Integer
Private mvarJLBORD As Integer
Private mvarNETO As Integer
Private mvarNoOfDoubleHulls As Integer
Private mvarIOPTI As YesNo
Private mvarITERAM As Integer
Private mvarICOUT As OptimizationType
Private mvarWIDTH As Double
Private mvarLongRegl As Double
Private mvarDIS1 As Double
Private mvarDIS2 As Double
Private mvarDIS3 As Double
Private mvarDIS4 As Double
Private mvarDIS5 As Double
Private mvarFAM1 As Double
Private mvarFAM2 As Double
Private mvarFAM3 As Double
Private mvarFAM4 As Double
Private mvarFAM5 As Double
Private mvarFAM6 As Double
Private mvarIPOIDS As YesNo
Private mvarcCostData As cCostData
Private mvarcCostCAtMain As cCostCAtMain
Private mvarcolLoadCase As colLoadCase
Private mvarcMarsLoads As cMarsLoads
Private mvarcMultiOpti As cMultiOpti
'Global Data
Private mvarYAxisOrigin As Double
Private mvarZAxisOrigin As Double
'Private mvarGravityLimitRestriction As GravityLimitRestriction
'Private mvarMinGravityCenter As Double
'Private mvarMaxGravityCenter As Double
Private mvarcGlobalConstraints As cGlobalConstraints
Private mvarYRED As Double
Private mvarIsBendingMoments As YesNo
Private mvarcolEqualityRestrictions As colEqualityRestrictions
Private mvarcolDefaultDesignVariables As colDesignVariables
Private mvarIRESTR As YesNo
Private mvarIULT As YesNo

'MARS_LBR5
Private mvarXSection As Double
Private mvarMarsSymm As Boolean

Private mvarIANA As Integer

Public Enum OptimizationType
    MaximalIntertia = -1
    MinimalWeight = 0
    MinimalCostSimple = 1
    MinimalCostDetailed = 2
End Enum

Public Property Get cCostData() As cCostData
    On Error GoTo cCostDataGetErr
    Set cCostData = mvarcCostData
    Exit Property
cCostDataGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: cCostData Property Get")
End Property

Public Property Set cCostData(ByVal vData As cCostData)
    On Error GoTo cCostDataSetErr
    Set mvarcCostData = vData
    Exit Property
cCostDataSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: cCostData Property Set")
End Property

Public Property Get cMarsLoads() As cMarsLoads
    On Error GoTo cMarsLoadsGetErr
    Set cMarsLoads = mvarcMarsLoads
    Exit Property
cMarsLoadsGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: cMarsLoads Property Get")
End Property

Public Property Set cMultiOpti(ByVal vData As cMultiOpti)
    On Error GoTo cMultiOptiSetErr
    Set mvarcMultiOpti = vData
    Exit Property
cMultiOptiSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: cMultiOpti Property Set")
End Property

Public Property Get cMultiOpti() As cMultiOpti
    On Error GoTo cMultiOptiGetErr
    Set cMultiOpti = mvarcMultiOpti
    Exit Property
cMultiOptiGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: cMultiOpti Property Get")
End Property

Public Property Set cMarsLoads(ByVal vData As cMarsLoads)
    On Error GoTo cMarsLoadsSetErr
    Set mvarcMarsLoads = vData
    Exit Property
cMarsLoadsSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: cMarsLoads Property Set")
End Property

Public Property Get cCostCAtMain() As cCostCAtMain
    On Error GoTo cCostCAtMainGetErr
    Set cCostCAtMain = mvarcCostCAtMain
    Exit Property
cCostCAtMainGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: cCostCAtMain Property Get")
End Property

Public Property Set cCostCAtMain(vData As cCostCAtMain)
    On Error GoTo cCostCAtMainSetErr
    Set mvarcCostCAtMain = vData
    Exit Property
cCostCAtMainSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: cCostCAtMain Property Set")
End Property

Public Property Get colLoadCase() As colLoadCase
    On Error GoTo colLoadCaseGetErr
    If mvarcolLoadCase Is Nothing Then
        Set mvarcolLoadCase = New colLoadCase
    End If
    Set colLoadCase = mvarcolLoadCase
    Exit Property
colLoadCaseGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: colLoadCase Property Get")
End Property

Public Property Set colLoadCase(vData As colLoadCase)
    On Error GoTo colLoadCaseSetErr
    Set mvarcolLoadCase = vData
    Exit Property
colLoadCaseSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: colLoadCase Property Set")
End Property

Public Property Get Title() As String
    On Error GoTo TitleGetErr
    Title = mvarTitle
    Exit Property
TitleGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: Title Property Get")
End Property

Public Property Let Title(ByVal vData As String)
    On Error GoTo TitleLetErr
    mvarTitle = vData
    Exit Property
TitleLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: Title Property Let")
End Property

Public Property Get IMPR() As Integer
    On Error GoTo IMPRGetErr
    IMPR = mvarIMPR
    Exit Property
IMPRGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: IMPR Property Get")
End Property

Public Property Let IMPR(ByVal vData As Integer)
    On Error GoTo IMPRLetErr
    mvarIMPR = vData
    Exit Property
IMPRLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: IMPR Property Let")
End Property

Public Property Get IMPR2() As Integer
    On Error GoTo IMPR2GetErr
    IMPR2 = mvarIMPR2
    Exit Property
IMPR2GetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: IMPR2 Property Get")
End Property

Public Property Let IMPR2(ByVal vData As Integer)
    On Error GoTo IMPR2LetErr
    mvarIMPR2 = vData
    Exit Property
IMPR2LetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: IMPR2 Property Let")
End Property

Public Property Get INDAIG() As Integer
    On Error GoTo INDAIGGetErr
    INDAIG = mvarINDAIG
    Exit Property
INDAIGGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: INDAIG Property Get")
End Property

Public Property Let INDAIG(ByVal vData As Integer)
    On Error GoTo INDAIGLetErr
    mvarINDAIG = vData
    Exit Property
INDAIGLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: INDAIG Property Let")
End Property

Public Property Get INDRAID() As Integer
    On Error GoTo INDRAIDGetErr
    INDRAID = mvarINDRAID
    Exit Property
INDRAIDGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: INDRAID Property Get")
End Property

Public Property Let INDRAID(ByVal vData As Integer)
    On Error GoTo INDRAIDLetErr
    mvarINDRAID = vData
    Exit Property
INDRAIDLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: INDRAID Property Let")
End Property

Public Property Get DESSIN() As Integer
    On Error GoTo DESSINGetErr
    DESSIN = mvarDESSIN
    Exit Property
DESSINGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: DESSIN Property Get")
End Property

Public Property Let DESSIN(ByVal vData As Integer)
    On Error GoTo DESSINLetErr
    mvarDESSIN = vData
    Exit Property
DESSINLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: DESSIN Property Let")
End Property

Public Property Get JLPH() As Integer
    On Error GoTo JLPHGetErr
    JLPH = mvarJLPH
    Exit Property
JLPHGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: JLPH Property Get")
End Property

Public Property Let JLPH(ByVal vData As Integer)
    On Error GoTo JLPHLetErr
    mvarJLPH = vData
    Exit Property
JLPHLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: JLPH Property Let")
End Property

Public Property Get JLBORD() As Integer
    On Error GoTo JLBORDGetErr
    JLBORD = mvarJLBORD
    Exit Property
JLBORDGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: JLBORD Property Get")
End Property

Public Property Let JLBORD(ByVal vData As Integer)
    On Error GoTo JLBORDLetErr
    mvarJLBORD = vData
    Exit Property
JLBORDLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: JLBORD Property Let")
End Property

Public Property Get NETO() As Integer
    On Error GoTo NETOGetErr
    NETO = mvarNETO
    Exit Property
NETOGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: NETO Property Get")
End Property

Public Property Let NETO(ByVal vData As Integer)
    On Error GoTo NETOLetErr
    mvarNETO = vData
    Exit Property
NETOLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: NETO Property Let")
End Property

Public Property Get NoOfDoubleHulls() As Integer
    On Error GoTo NoOfDoubleHullsGetErr
    NoOfDoubleHulls = mvarNoOfDoubleHulls
    Exit Property
NoOfDoubleHullsGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: NoOfDoubleHulls Property Get")
End Property

Public Property Let NoOfDoubleHulls(ByVal vData As Integer)
    On Error GoTo NoOfDoubleHullsLetErr
    mvarNoOfDoubleHulls = vData
    Exit Property
NoOfDoubleHullsLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: NoOfDoubleHulls Property Let")
End Property

Public Property Get IOPTI() As YesNo
    On Error GoTo IOPTIGetErr
    IOPTI = mvarIOPTI
    Exit Property
IOPTIGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: IOPTI Property Get")
End Property

Public Property Let IOPTI(ByVal vData As YesNo)
    On Error GoTo IOPTILetErr
    mvarIOPTI = vData
    Exit Property
IOPTILetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: IOPTI Property Let")
End Property

Public Property Get ITERAM() As Integer
    On Error GoTo ITERAMGetErr
    ITERAM = mvarITERAM
    Exit Property
ITERAMGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: ITERAM Property Get")
End Property

Public Property Let ITERAM(ByVal vData As Integer)
    On Error GoTo ITERAMLetErr
    mvarITERAM = vData
    Exit Property
ITERAMLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: ITERAM Property Let")
End Property

Public Property Get ICOUT() As OptimizationType
    On Error GoTo ICOUTGetErr
    ICOUT = mvarICOUT
    Exit Property
ICOUTGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: ICOUT Property Get")
End Property

Public Property Let ICOUT(ByVal vData As OptimizationType)
    On Error GoTo ICOUTLetErr
    mvarICOUT = vData
    Exit Property
ICOUTLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: ICOUT Property Let")
End Property

Public Property Get Width() As Double
    On Error GoTo WIDTHGetErr
    Width = mvarWIDTH
    Exit Property
WIDTHGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: WIDTH Property Get")
End Property

Public Property Let Width(ByVal vData As Double)
    On Error GoTo WIDTHLetErr
    mvarWIDTH = vData
    Exit Property
WIDTHLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: WIDTH Property Let")
End Property

Public Property Get LongRegl() As Double
    On Error GoTo LongReglGetErr
    LongRegl = mvarLongRegl
    Exit Property
LongReglGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: LongRegl Property Get")
End Property

Public Property Let LongRegl(ByVal vData As Double)
    On Error GoTo LongReglLetErr
    mvarLongRegl = vData
    Exit Property
LongReglLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: LongRegl Property Let")
End Property

Public Property Get DIS1() As Double
    On Error GoTo DIS1GetErr
    DIS1 = mvarDIS1
    Exit Property
DIS1GetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: DIS1 Property Get")
End Property

Public Property Let DIS1(ByVal vData As Double)
    On Error GoTo DIS1LetErr
    mvarDIS1 = vData
    Exit Property
DIS1LetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: DIS1 Property Let")
End Property

Public Property Get DIS2() As Double
    On Error GoTo DIS2GetErr
    DIS2 = mvarDIS2
    Exit Property
DIS2GetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: DIS2 Property Get")
End Property

Public Property Let DIS2(ByVal vData As Double)
    On Error GoTo DIS2LetErr
    mvarDIS2 = vData
    Exit Property
DIS2LetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: DIS2 Property Let")
End Property

Public Property Get DIS3() As Double
    On Error GoTo DIS3GetErr
    DIS3 = mvarDIS3
    Exit Property
DIS3GetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: DIS3 Property Get")
End Property

Public Property Let DIS3(ByVal vData As Double)
    On Error GoTo DIS3LetErr
    mvarDIS3 = vData
    Exit Property
DIS3LetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: DIS3 Property Let")
End Property

Public Property Get DIS4() As Double
    On Error GoTo DIS4GetErr
    DIS4 = mvarDIS4
    Exit Property
DIS4GetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: DIS4 Property Get")
End Property

Public Property Let DIS4(ByVal vData As Double)
    On Error GoTo DIS4LetErr
    mvarDIS4 = vData
    Exit Property
DIS4LetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: DIS4 Property Let")
End Property

Public Property Get DIS5() As Double
    On Error GoTo DIS5GetErr
    DIS5 = mvarDIS5
    Exit Property
DIS5GetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: DIS5 Property Get")
End Property

Public Property Let DIS5(ByVal vData As Double)
    On Error GoTo DIS5LetErr
    mvarDIS5 = vData
    Exit Property
DIS5LetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: DIS5 Property Let")
End Property

Public Property Get FAM1() As Double
    On Error GoTo FAM1GetErr
    FAM1 = mvarFAM1
    Exit Property
FAM1GetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: FAM1 Property Get")
End Property

Public Property Let FAM1(ByVal vData As Double)
    On Error GoTo FAM1LetErr
    mvarFAM1 = vData
    Exit Property
FAM1LetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: FAM1 Property Let")
End Property

Public Property Get FAM2() As Double
    On Error GoTo FAM2GetErr
    FAM2 = mvarFAM2
    Exit Property
FAM2GetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: FAM2 Property Get")
End Property

Public Property Let FAM2(ByVal vData As Double)
    On Error GoTo FAM2LetErr
    mvarFAM2 = vData
    Exit Property
FAM2LetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: FAM2 Property Let")
End Property

Public Property Get FAM3() As Double
    On Error GoTo FAM3GetErr
    FAM3 = mvarFAM3
    Exit Property
FAM3GetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: FAM3 Property Get")
End Property

Public Property Let FAM3(ByVal vData As Double)
    On Error GoTo FAM3LetErr
    mvarFAM3 = vData
    Exit Property
FAM3LetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: FAM3 Property Let")
End Property

Public Property Get FAM4() As Double
    On Error GoTo FAM4GetErr
    FAM4 = mvarFAM4
    Exit Property
FAM4GetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: FAM4 Property Get")
End Property

Public Property Let FAM4(ByVal vData As Double)
    On Error GoTo FAM4LetErr
    mvarFAM4 = vData
    Exit Property
FAM4LetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: FAM4 Property Let")
End Property

Public Property Get FAM5() As Double
    On Error GoTo FAM5GetErr
    FAM5 = mvarFAM5
    Exit Property
FAM5GetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: FAM5 Property Get")
End Property

Public Property Let FAM5(ByVal vData As Double)
    On Error GoTo FAM5LetErr
    mvarFAM5 = vData
    Exit Property
FAM5LetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: FAM5 Property Let")
End Property

Public Property Get FAM6() As Double
    On Error GoTo FAM6GetErr
    FAM6 = mvarFAM6
    Exit Property
FAM6GetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: FAM6 Property Get")
End Property

Public Property Let FAM6(ByVal vData As Double)
    On Error GoTo FAM6LetErr
    mvarFAM6 = vData
    Exit Property
FAM6LetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: FAM6 Property Let")
End Property

Public Property Get IPOIDS() As YesNo
    On Error GoTo IPOIDSGetErr
    IPOIDS = mvarIPOIDS
    Exit Property
IPOIDSGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: IPOIDS Property Get")
End Property

Public Property Let IPOIDS(ByVal vData As YesNo)
    On Error GoTo IPOIDSLetErr
    mvarIPOIDS = vData
    Exit Property
IPOIDSLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: IPOIDS Property Let")
End Property

Public Property Get YAxisOrigin() As Double
    On Error GoTo YAxisOriginGetErr
    YAxisOrigin = mvarYAxisOrigin
    Exit Property
YAxisOriginGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: YAxisOrigin Property Get")
End Property

Public Property Let YAxisOrigin(ByVal vData As Double)
    On Error GoTo YAxisOriginLetErr
    mvarYAxisOrigin = vData
    Exit Property
YAxisOriginLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: YAxisOrigin Property Let")
End Property

Public Property Get ZAxisOrigin() As Double
    On Error GoTo ZAxisOriginGetErr
    ZAxisOrigin = mvarZAxisOrigin
    Exit Property
ZAxisOriginGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: ZAxisOrigin Property Get")
End Property

Public Property Let ZAxisOrigin(ByVal vData As Double)
    On Error GoTo ZAxisOriginLetErr
    mvarZAxisOrigin = vData
    Exit Property
ZAxisOriginLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: ZAxisOrigin Property Let")
End Property

'Public Property Get GravityLimitRestriction() As GravityLimitRestriction
'    On Error GoTo  GravityLimitRestrictionGetErr
'    GravityLimitRestriction = mvarGravityLimitRestriction
'    Exit Property
'GravityLimitRestrictionGetErr:
'    Call RaiseError(MyUnhandledError,Err.Description & "." & vbCrLf &  "cHeader: GravityLimitRestriction Property Get")
'End Property
'
'Public Property Let GravityLimitRestriction(ByVal vData As GravityLimitRestriction)
'    On Error GoTo  GravityLimitRestrictionLetErr
'    mvarGravityLimitRestriction = vData
'    Exit Property
'GravityLimitRestrictionLetErr:
'    Call RaiseError(MyUnhandledError,Err.Description & "." & vbCrLf &  "cHeader: GravityLimitRestriction Property Let")
'End Property
'
'Public Property Get MinGravityCenter() As Double
'    On Error GoTo  MinGravityCenterGetErr
'    MinGravityCenter = mvarMinGravityCenter
'    Exit Property
'MinGravityCenterGetErr:
'    Call RaiseError(MyUnhandledError,Err.Description & "." & vbCrLf &  "cHeader: MinGravityCenter Property Get")
'End Property
'
'Public Property Let MinGravityCenter(ByVal vData As Double)
'    On Error GoTo  MinGravityCenterLetErr
'    mvarMinGravityCenter = vData
'    Exit Property
'MinGravityCenterLetErr:
'    Call RaiseError(MyUnhandledError,Err.Description & "." & vbCrLf &  "cHeader: MinGravityCenter Property Let")
'End Property
'
'Public Property Get MaxGravityCenter() As Double
'    On Error GoTo  MaxGravityCenterGetErr
'    MaxGravityCenter = mvarMaxGravityCenter
'    Exit Property
'MaxGravityCenterGetErr:
'    Call RaiseError(MyUnhandledError,Err.Description & "." & vbCrLf &  "cHeader: MaxGravityCenter Property Get")
'End Property
'
'Public Property Let MaxGravityCenter(ByVal vData As Double)
'    On Error GoTo  MaxGravityCenterLetErr
'    mvarMaxGravityCenter = vData
'    Exit Property
'MaxGravityCenterLetErr:
'    Call RaiseError(MyUnhandledError,Err.Description & "." & vbCrLf &  "cHeader: MaxGravityCenter Property Let")
'End Property

Public Property Get cGlobalConstraints() As cGlobalConstraints
    On Error GoTo cGlobalConstraintsGetErr
    Set cGlobalConstraints = mvarcGlobalConstraints
    Exit Property
cGlobalConstraintsGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: cGlobalConstraints Property Get")
End Property

Public Property Set cGlobalConstraints(ByVal vData As cGlobalConstraints)
    On Error GoTo cGlobalConstraintsSetErr
    Set mvarcGlobalConstraints = vData
    Exit Property
cGlobalConstraintsSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cProject: cGlobalConstraints Property Set")
End Property

Public Property Get YRED() As Double
    On Error GoTo YREDGetErr
    YRED = mvarYRED
    Exit Property
YREDGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: YRED Property Get")
End Property

Public Property Let YRED(ByVal vData As Double)
    On Error GoTo YREDLetErr
    mvarYRED = vData
    Exit Property
YREDLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: YRED Property Let")
End Property

Public Property Get IsBendingMoments() As YesNo
    On Error GoTo IsBendingMomentsGetErr
    IsBendingMoments = mvarIsBendingMoments
    Exit Property
IsBendingMomentsGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: IsBendingMoments Property Get")
End Property

Public Property Let IsBendingMoments(ByVal vData As YesNo)
    On Error GoTo IsBendingMomentsLetErr
    mvarIsBendingMoments = vData
    Exit Property
IsBendingMomentsLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: IsBendingMoments Property Let")
End Property

Public Property Get IRESTR() As YesNo
    On Error GoTo IRESTRGetErr
    IRESTR = mvarIRESTR
    Exit Property
IRESTRGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: IRESTR Property Get")
End Property

Public Property Let IRESTR(ByVal vData As YesNo)
    On Error GoTo IRESTRLetErr
    mvarIRESTR = vData
    Exit Property
IRESTRLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: IRESTR Property Let")
End Property

Public Property Get IULT() As YesNo
    On Error GoTo IULTGetErr
    IULT = mvarIULT
    Exit Property
IULTGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: IULT Property Get")
End Property

Public Property Let IULT(ByVal vData As YesNo)
    On Error GoTo IULTLetErr
    mvarIULT = vData
    Exit Property
IULTLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: IULT Property Let")
End Property

Public Property Get colEqualityRestrictions() As colEqualityRestrictions
    On Error GoTo colEqualityRestrictionsGetErr
    If mvarcolEqualityRestrictions Is Nothing Then
        Set mvarcolEqualityRestrictions = New colEqualityRestrictions
    End If
    Set colEqualityRestrictions = mvarcolEqualityRestrictions
    Exit Property
colEqualityRestrictionsGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: colEqualityRestrictions Property Get")
End Property

Public Property Set colEqualityRestrictions(vData As colEqualityRestrictions)
    On Error GoTo colEqualityRestrictionsSetErr
    Set mvarcolEqualityRestrictions = vData
    Exit Property
colEqualityRestrictionsSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: colEqualityRestrictions Property Set")
End Property

Public Property Get colDefaultDesignVariables() As colDesignVariables
    On Error GoTo colDefaultDesignVariablesGetErr
    If mvarcolDefaultDesignVariables Is Nothing Then
        Set mvarcolDefaultDesignVariables = New colDesignVariables
    End If
    Set colDefaultDesignVariables = mvarcolDefaultDesignVariables
    Exit Property
colDefaultDesignVariablesGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: colDefaultDesignVariables Property Get")
End Property

Public Property Set colDefaultDesignVariables(vData As colDesignVariables)
    On Error GoTo colDefaultDesignVariablesSetErr
    Set mvarcolDefaultDesignVariables = vData
    Exit Property
colDefaultDesignVariablesSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: colDefaultDesignVariables Property Set")
End Property

Public Property Get XSection() As Double
    On Error GoTo XSectionGetErr
    XSection = mvarXSection
    Exit Property
XSectionGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: XSection Property Get")
End Property

Public Property Let XSection(ByVal vData As Double)
    On Error GoTo XSectionLetErr
    mvarXSection = vData
    Exit Property
XSectionLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: XSection Property Let")
End Property

Public Property Get MarsSymm() As Boolean
    On Error GoTo MarsSymmGetErr
    MarsSymm = mvarMarsSymm
    Exit Property
MarsSymmGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: MarsSymm Property Get")
End Property

Public Property Let MarsSymm(ByVal vData As Boolean)
    On Error GoTo MarsSymmLetErr
    mvarMarsSymm = vData
    Exit Property
MarsSymmLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: MarsSymm Property Let")
End Property

Public Property Get IANA() As Integer
    On Error GoTo IANAGetErr
    IANA = mvarIANA
    Exit Property
IANAGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: IANA Property Get")
End Property

Public Property Let IANA(ByVal vData As Integer)
    On Error GoTo IANASetErr
    mvarIANA = vData
    Exit Property
IANASetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: IANA Property Set")
End Property

'Methods
Public Function ReadGeneralDataLBR5txtFile(ts As TextStream)
    On Error GoTo ReadGeneralDataLBR5txtFileErr
        Dim sLine As String
        Dim v() As Variant
        Dim i As Integer
        Dim j As Integer
        'sLine =ReadLn(ts)
        sLine = ReadLn(ts)
        sLine = LTrim(sLine)
        sLine = RTrim(sLine)
        Me.Title = sLine
        'General Parameters
        sLine = ReadLn(ts)
        GetValues 9, sLine, v
        Me.IMPR = Val_(v(1))
        Me.IMPR2 = Val_(v(2))
        Me.INDAIG = Val_(v(3))
        Me.INDRAID = Val_(v(4))
        Me.DESSIN = Val_(v(5))
        Me.JLPH = Val_(v(6))
        Me.JLBORD = Val_(v(7))
        Me.NETO = Val_(v(8))
        If IsNumeric(v(9)) = True Then NoOfDoubleHulls = Val_(v(9))
        'Opti
        sLine = ReadLn(ts)
        GetValues 2, sLine, v
        Me.IOPTI = Val_(v(1))
        Me.ITERAM = Val_(v(2))
        
        sLine = ReadLn(ts)
        GetValues 1, sLine, v
        Me.ICOUT = Val_(v(1))
        Me.cCostData.ReadLBR5txtFile ts
        sLine = ReadLn(ts)
        GetValues 1, sLine, v
        Me.Width = Val_(v(1))
        sLine = ReadLn(ts)
        GetValues 5, sLine, v
        Me.DIS1 = Val_(v(1))
        Me.DIS2 = Val_(v(2))
        Me.DIS3 = Val_(v(3))
        Me.DIS4 = Val_(v(4))
        Me.DIS5 = Val_(v(5))
        sLine = ReadLn(ts)
        GetValues 6, sLine, v
        Me.FAM1 = Val_(v(1))
        Me.FAM2 = Val_(v(2))
        Me.FAM3 = Val_(v(3))
        Me.FAM4 = Val_(v(4))
        Me.FAM5 = Val_(v(5))
        Me.FAM6 = Val_(v(6))
        sLine = ReadLn(ts)
        GetValues 1, sLine, v
        Me.IPOIDS = Val_(v(1))
        sLine = ReadLn(ts)
        GetValues 2, sLine, v
        For i = 1 To Val_(v(1))
            Dim m As New cLoadCase
            m.index = i
            Me.colLoadCase.Add m, i
            Set m = Nothing
        Next i
        GetValues Val_(v(2)), sLine, v
        For i = 1 To UBound(v)
            For j = 1 To Me.colLoadCase.Count
                If Me.colLoadCase.Item(j).index = Val_(v(i)) Then
                    Me.colLoadCase.Item(j).state = IsOn
                End If
            Next j
        Next i
        For i = 1 To Me.colLoadCase.Count
            sLine = ReadLn(ts)
            sLine = LTrim(sLine)
            sLine = RTrim(sLine)
            Me.colLoadCase.Item(i).Title = sLine
        Next i
    Exit Function
ReadGeneralDataLBR5txtFileErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: Function ReadGeneralDataLBR5txtFile")
End Function

Public Function WriteGeneralDataLBR5txtFile(ByVal ts As TextStream)
    On Error GoTo WriteGeneralDataLBR5txtFileErr
    Dim sComment As String
    sComment = "IANA = 1 (LBR-4); IANA = 2 (calcul réglementaire)"
    ts.WriteLine Me.IANA & getTabs(8) & sComment
    ts.WriteLine Me.Title
    Me.NETO = Project.Item(ActiveProject).colPanel.Count
    sComment = "IMPR, IMPR2, INDAIG, INDRAID ,DESSIN ,JLPH ,JLBORD ,NETO ,NDQ"
    ts.WriteLine Me.IMPR & "  " & Me.IMPR2 & "  " & Me.INDAIG & "  " & _
                Me.INDRAID & "  " & Me.DESSIN & "  " & Me.JLPH & "  " & _
                Me.JLBORD & "  " & Me.NETO & "  " & Me.NoOfDoubleHulls & _
                getTabs(5) & sComment
    
    sComment = "" ' later add comment : IMULTI  RHO  W1  W2  W3
    ts.WriteLine Me.cMultiOpti.IMULTI & "  " & PVal(Me.cMultiOpti.RHO, "0.000") & "  " & PVal(Me.cMultiOpti.W1, "0.000") & "  " & _
                 PVal(Me.cMultiOpti.W2, "0.000") & "  " & PVal(Me.cMultiOpti.W3, "0.000") & "  " & PVal(Me.cMultiOpti.F0Cost, "0.000000") & _
                 "  " & PVal(Me.cMultiOpti.F0Weight, "0.000000") & "  " & PVal(Me.cMultiOpti.F0Inertia, "0.000000")
                
    
    sComment = "IOPTI (1 oui. 0 non) et ITERAM = nbre d'itération"
    ts.WriteLine Me.IOPTI & "  " & Me.ITERAM & getTabs(8) & sComment
    
    sComment = "ICOUT"
    ts.WriteLine Me.ICOUT & getTabs(8) & sComment
    
    Me.cCostData.WriteLBR5txtFile ts
    
    sComment = "Longueur du tronçon (WIDTH), Longueur reglementaire du navire"
    ts.WriteLine PVal(Me.Width, "0.0000") & "  " & PVal(Me.LongRegl, "0.0000") & getTabs(7) & sComment
    
    sComment = "Section de calcul (DIS)"
    ts.WriteLine PVal(Me.DIS1, "0.0000") & "  " & PVal(Me.DIS2, "0.0000") & "  " & PVal(Me.DIS3, "0.0000") & "  " & PVal(Me.DIS4, "0.0000") & "  " & PVal(Me.DIS5, "0.0000") & getTabs(3) & sComment
    
    sComment = "Cisaillement des semelles (FAM)"
    ts.WriteLine PVal(Me.FAM1, "0.0000") & "  " & PVal(Me.FAM2, "0.0000") & "  " & PVal(Me.FAM3, "0.0000") & "  " & PVal(Me.FAM4, "0.0000") & "  " & PVal(Me.FAM5, "0.0000") & "  " & PVal(Me.FAM6, "0.0000") & getTabs(3) & sComment
    
    sComment = "IPOIDS ( 1 OUI. 0 NON)"
    ts.WriteLine Me.IPOIDS & getTabs(8) & sComment
    
    Dim LoadCase As cLoadCase
    Dim sLoadCaseList As String
    For Each LoadCase In Me.colLoadCase
        If LoadCase.state = IsOn Then
            sLoadCaseList = sLoadCaseList & "  " & LoadCase.index
        End If
    Next LoadCase
    sLoadCaseList = LTrim(sLoadCaseList)
    ts.WriteLine Me.colLoadCase.Count & "  " & Me.colLoadCase.GetActiveLoadCasesNo & "  " & sLoadCaseList
    For Each LoadCase In Me.colLoadCase
        ts.WriteLine LoadCase.Title
    Next LoadCase
    Exit Function
WriteGeneralDataLBR5txtFileErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: Function WriteGeneralDataLBR5txtFile")
End Function

Public Function ReadGlobalDataLBR5txtFile(ts As TextStream)
    On Error GoTo ReadGlobalDataLBR5txtFileErr
    Dim sLine As String
    Dim v() As Variant
    Dim i As Integer
    Dim j As Integer
    Dim OBJ As Object
    'Boundary Conditions
    ts.SkipLine
    sLine = ReadLn(ts)
    GetValues 1, sLine, v
    Dim NoOfBoundaryCond As Integer
    NoOfBoundaryCond = Val_(v(1))
    For i = 1 To NoOfBoundaryCond
        sLine = ReadLn(ts)
        GetValues 2, sLine, v
        Select Case Project.Item(lProjectCount).colPanel.Count
            Case 1
                Set OBJ = New cBoundaryConditions
                OBJ.index = i
                OBJ.BoundaryCondition = Val_(v(2))
                If i = 1 Then
                    OBJ.Edge = InEdge
                ElseIf i = 2 Then
                    OBJ.Edge = OutEdge
                End If
                Project.Item(lProjectCount).colPanel.Item(1).colBoundaryConditions.Add OBJ, OBJ.index
                Set OBJ = Nothing
            Case Is > 1
                Set OBJ = New cBoundaryConditions
                OBJ.index = 1
                OBJ.BoundaryCondition = Val_(v(2))
                Project.Item(lProjectCount).colPanel.Item(Val_(v(1))).colBoundaryConditions.Add OBJ, OBJ.index
                Set OBJ = Nothing
        End Select
    Next i
    'Gravity Center
    ts.SkipLine
    sLine = ReadLn(ts)
    GetValues 2, sLine, v
    Me.YAxisOrigin = Val_(v(1))
    Me.ZAxisOrigin = Val_(v(2))
    Me.cGlobalConstraints.ReadLBR5txtFile ts
    'Bending Moments
    ts.SkipLine
    sLine = ReadLn(ts)
    GetValues 1, sLine, v
    Me.IsBendingMoments = Val_(v(1))
    Select Case Me.IsBendingMoments
        Case no
        Case yes
            sLine = ReadLn(ts)
            GetValues 1, sLine, v
            Me.YRED = Val_(v(1))
            For i = 1 To Me.colLoadCase.Count
                sLine = ReadLn(ts)
                GetValues 4, sLine, v
                Me.colLoadCase.Item(i).VerticalBendingMomentFore = Round(Val_(v(1)), 6)
                Me.colLoadCase.Item(i).VerticalBendingMomentAft = Round(Val_(v(2)), 6)
                Me.colLoadCase.Item(i).HorizontalBendingMomentFore = Val_(v(3))
                Me.colLoadCase.Item(i).HorizontalBendingMomentAft = Val_(v(4))
                
                 
            Next i
    End Select
    'Copy bending moments in panel's load cases collections
    For i = 1 To Me.NETO
        For j = 1 To Me.colLoadCase.Count
            Project.Item(lProjectCount).colPanel.Item(i).colLoadCase.Item(j).VerticalBendingMomentFore = Me.colLoadCase.Item(j).VerticalBendingMomentFore
            Project.Item(lProjectCount).colPanel.Item(i).colLoadCase.Item(j).VerticalBendingMomentAft = Me.colLoadCase.Item(j).VerticalBendingMomentAft
            Project.Item(lProjectCount).colPanel.Item(i).colLoadCase.Item(j).HorizontalBendingMomentFore = Me.colLoadCase.Item(j).HorizontalBendingMomentFore
            Project.Item(lProjectCount).colPanel.Item(i).colLoadCase.Item(j).HorizontalBendingMomentAft = Me.colLoadCase.Item(j).HorizontalBendingMomentAft
        Next j
    Next i
    'Set Me.colLoadCase = Nothing
    ' Equality constraints
    ts.SkipLine
    sLine = ReadLn(ts)
    GetValues 1, sLine, v
    Dim NoOfEqRestr As Integer
    NoOfEqRestr = Val_(v(1))
    For i = 1 To NoOfEqRestr
        Set OBJ = New cEqualityRestrictions
        OBJ.index = i
        Me.colEqualityRestrictions.Add OBJ, i
        Me.colEqualityRestrictions.Item(i).ReadLBR5txtFile ts
        Set OBJ = Nothing
    Next i
    'Ultimate strenght
    ts.SkipLine
    sLine = ReadLn(ts)
    GetValues 1, sLine, v
    Me.IRESTR = Val_(v(1))
    sLine = ReadLn(ts)
    GetValues 1, sLine, v
    Me.IULT = Val_(v(1))
    Exit Function
ReadGlobalDataLBR5txtFileErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: Function ReadGlobalDataLBR5txtFile")
End Function

Public Function ReadMARS_LBR5TransfertFile(ts As TextStream)
    On Error GoTo ReadMARS_LBR5TransfertFileErr
    Dim sLine As String
    Dim v() As Variant
NextLine:
    'Title
    sLine = ReadLn(ts)
    Select Case Left(UCase(sLine), 7)
        Case "<TITLE>"
            sLine = ReadLn(ts)
            Me.Title = sLine
        Case Else
            GoTo NextLine
    End Select
    'X Section
NextLine1:
    sLine = ReadLn(ts)
    Select Case Left(UCase(sLine), 9)
        Case "<SECTION>"
            sLine = ReadLn(ts)
            GetValues 1, sLine, v
            Me.XSection = Val_(v(1))
        Case Else
            GoTo NextLine1
    End Select
NextLine2:
    sLine = ReadLn(ts)
    Select Case Left(UCase(sLine), 9)
        Case "<SYMETRY>"
            sLine = ReadLn(ts)
        Case Else
            GoTo NextLine2
    End Select
    Exit Function
ReadMARS_LBR5TransfertFileErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: Function ReadMARS_LBR5TransfertFile")
End Function

Public Function WriteGlobalDataLBR5txtFile(ts As TextStream)
    On Error GoTo WriteGlobalDataLBR5txtFileErr
    Dim sComment As String
    ts.WriteLine "BOUNDARY CONDITIONS"
    sComment = "Nbre de conditions de bords"
    ts.WriteLine GetNoOfBoundaryConditions & getTabs(8) & sComment
    Dim Panel As cPanel
    Dim BoundaryCondition As cBoundaryConditions
    sComment = "nbre panneau. nbre condition"
    If Project.Item(ActiveProject).colPanel.Count > 1 Then
        For Each Panel In Project.Item(ActiveProject).colPanel
            For Each BoundaryCondition In Panel.colBoundaryConditions
                ts.WriteLine Panel.pNumber & "  " & _
                            BoundaryCondition.BoundaryCondition & getTabs(8) & sComment
            Next BoundaryCondition
        Next Panel
    ElseIf Project.Item(ActiveProject).colPanel.Count = 1 Then
        For Each BoundaryCondition In Project.Item(ActiveProject).colPanel.Item(1).colBoundaryConditions
            ts.WriteLine BoundaryCondition.index & "  " & _
                        BoundaryCondition.BoundaryCondition & getTabs(8) & sComment
        Next BoundaryCondition
    End If
    ts.WriteLine "GRAVITY CENTER LOCATION - REPERE UTILISATEUR + RESTRICTION SUR LE CENTRE DE GRAVITE"
    UpdateOrigin ActiveProject
    sComment = "(XK.YK) Coord du centre du repere utilisateur"
    ts.WriteLine PVal(Me.YAxisOrigin, "0.0000") & "  " & _
                PVal(Me.ZAxisOrigin, "0.0000") & getTabs(6) & sComment
    Me.cGlobalConstraints.WriteLBR5txtFile ts
    ts.WriteLine "MOMENTS"
    sComment = "= 0:pas de moment. = 1:avec moment"
    Dim LoadCase As cLoadCase
    Me.IsBendingMoments = no
    For Each LoadCase In Me.colLoadCase
        If LoadCase.HorizontalBendingMomentAft > 10 Then
            Me.IsBendingMoments = yes
        End If
        If LoadCase.HorizontalBendingMomentFore > 10 Then
            Me.IsBendingMoments = yes
        End If
        If LoadCase.VerticalBendingMomentAft > 10 Then
            Me.IsBendingMoments = yes
        End If
        If LoadCase.VerticalBendingMomentFore > 10 Then
            Me.IsBendingMoments = yes
        End If
    Next LoadCase
    
    
        
    
    If Project.Item(ActiveProject).colPanel.Count = 1 Then Me.IsBendingMoments = no
    Select Case Me.IsBendingMoments
        Case no
            ts.WriteLine "0" & getTabs(8) & sComment
        Case yes
            ts.WriteLine "1" & getTabs(8) & sComment
            sComment = "Coeff. de réduction 1.24 pour Sig. et 1.04 pour W"
            ts.WriteLine Me.YRED & getTabs(8) & sComment
            
            For Each LoadCase In Me.colLoadCase
                'If Project.Item(ActiveProject).IsSymmetryAxis = False Then
                    Dim vBMa As Double, vBMf As Double
                    If LoadCase.VerticalBendingMomentFore = 0 Then
                        vBMf = 0.0000001
                    Else
                        vBMf = LoadCase.VerticalBendingMomentFore
                    End If
                    
                    If LoadCase.VerticalBendingMomentAft = 0 Then
                        vBMa = 0.0000001
                    Else
                        vBMa = LoadCase.VerticalBendingMomentAft
                    End If
                'End If
                sComment = LoadCase.Title
                
                If Project.Item(ActiveProject).cHeader.IANA = 1 Then
                    LoadCase.VerticalShear = 0
                    LoadCase.HorizontalShear = 0
                End If
'                ts.WriteLine PVal(vBMf, "0.0000E+00") & "  " & _
'                            PVal(vBMa, "0.0000E+00") & "  " & _
'                            PVal(LoadCase.HorizontalBendingMomentFore, "0.0000E+00") & "  " & _
'                            PVal(LoadCase.HorizontalBendingMomentAft, "0.0000E+00") & getTabs(3) & sComment
'                 ts.WriteLine PVal(vBMf, "0.0000E+00") & "  " & _
'                            PVal(vBMf, "0.0000E+00") & "  " & _
'                            PVal(LoadCase.HorizontalBendingMomentFore, "0.0000E+00") & "  " & _
'                            PVal(LoadCase.HorizontalBendingMomentFore, "0.0000E+00") & getTabs(3) & sComment

                ts.WriteLine PVal(vBMf, "0.0000E+00") & "  " & _
                            PVal(LoadCase.VerticalShear, "0.0000E+00") & "  " & _
                            PVal(LoadCase.HorizontalBendingMomentFore, "0.0000E+00") & "  " & _
                            PVal(LoadCase.HorizontalShear, "0.0000E+00") & getTabs(3) & sComment

            Next LoadCase
    End Select
    ts.WriteLine "EQUALITY CONSTRAINTS"
    sComment = "Nbre de restrictions d'égalité"
    ts.WriteLine Me.colEqualityRestrictions.Count & getTabs(8) & sComment
    Dim EqualityRestriction As cEqualityRestrictions
    
    For Each EqualityRestriction In Me.colEqualityRestrictions
        ts.WriteLine EqualityRestriction.DependingDesignVariable & "  " & _
                    EqualityRestriction.DependingPanel & "  " & _
                    EqualityRestriction.LeadingDesignVariable & "  " & _
                    EqualityRestriction.LeadingPanel & "  " & _
                    PVal(EqualityRestriction.Ratio, "0.000")
    Next EqualityRestriction
    
    ts.WriteLine "ULTIMATE STRENGTH - SHIP GIRDER"
    sComment = "IRESTR: utilisation comme restriction ; 0 = NON;1 = OUI"
    ts.WriteLine "0" & getTabs(8) & sComment
    sComment = "IULT =0 No analysis. = 1 for PAIK/Caldwell. = 2 for PROCOL(Smith). = 3  for..."
    ts.WriteLine "0" & getTabs(8) & sComment
    Exit Function
WriteGlobalDataLBR5txtFileErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: Function WriteGlobalDataLBR5txtFile")
End Function

Public Function GetNoOfBoundaryConditions() As Integer
    On Error GoTo GetNoOfBoundaryConditionsErr
    GetNoOfBoundaryConditions = 0
    Dim Panel As cPanel
    For Each Panel In Project.Item(ActiveProject).colPanel
        GetNoOfBoundaryConditions = GetNoOfBoundaryConditions + Panel.colBoundaryConditions.Count
    Next Panel
    Exit Function
GetNoOfBoundaryConditionsErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: Function GetNoOfBoundaryConditions")
End Function

Public Function Clone() As cHeader
    On Error GoTo CloneErr
    Set Clone = New cHeader
    'General Data
    With Clone
        .Title = Me.Title
        .IMPR = Me.IMPR
        .IMPR2 = Me.IMPR2
        .INDAIG = Me.INDAIG
        .INDRAID = Me.INDRAID
        .DESSIN = Me.DESSIN
        .JLPH = Me.JLPH
        .JLBORD = Me.JLBORD
        .NETO = Me.NETO
        .IOPTI = Me.IOPTI
        .ITERAM = Me.ITERAM
        .ICOUT = Me.ICOUT
        .Width = Me.Width
        .LongRegl = Me.LongRegl
        .DIS1 = Me.DIS1
        .DIS2 = Me.DIS2
        .DIS3 = Me.DIS3
        .DIS4 = Me.DIS4
        .DIS5 = Me.DIS5
        .FAM1 = Me.FAM1
        .FAM2 = Me.FAM2
        .FAM3 = Me.FAM3
        .FAM4 = Me.FAM4
        .FAM5 = Me.FAM5
        .FAM6 = Me.FAM6
        .IPOIDS = Me.IPOIDS
        Set .cCostData = Me.cCostData.Clone
        Set .cCostCAtMain = Me.cCostCAtMain.Clone
        Set .cMarsLoads = Me.cMarsLoads.Clone
        Set .colLoadCase = Me.colLoadCase.Clone
        Set .cMultiOpti = Me.cMultiOpti.Clone
        'Global Data
        .YAxisOrigin = Me.YAxisOrigin
        .ZAxisOrigin = Me.ZAxisOrigin
        Set .cGlobalConstraints = Me.cGlobalConstraints.Clone
        .YRED = Me.YRED
        .IsBendingMoments = Me.IsBendingMoments
        Set .colEqualityRestrictions = Me.colEqualityRestrictions.Clone
        .IRESTR = Me.IRESTR
        .IULT = Me.IULT
        .NoOfDoubleHulls = Me.NoOfDoubleHulls
        .XSection = Me.XSection
        .MarsSymm = Me.MarsSymm
        .IANA = Me.IANA
    End With
    Exit Function
CloneErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: Function Clone")
End Function

Public Function WriteASCII(ByVal iNoOfFile As Integer)
    On Error GoTo WriteASCIIErr
    Dim ss As String * 100
    With Me
        Put #iNoOfFile, , .IANA
        ss = .Title
        Put #iNoOfFile, , ss
        Put #iNoOfFile, , .IMPR
        Put #iNoOfFile, , .IMPR2
        Put #iNoOfFile, , .INDAIG
        Put #iNoOfFile, , .INDRAID
        Put #iNoOfFile, , .DESSIN
        Put #iNoOfFile, , .JLPH
        Put #iNoOfFile, , .JLBORD
        Put #iNoOfFile, , .NETO
        Put #iNoOfFile, , CInt(.IOPTI)
        Put #iNoOfFile, , .ITERAM
        .cMultiOpti.WriteASCII iNoOfFile
        Put #iNoOfFile, , CInt(.ICOUT)
        Put #iNoOfFile, , .Width
        Put #iNoOfFile, , .LongRegl
        Put #iNoOfFile, , .DIS1
        Put #iNoOfFile, , .DIS2
        Put #iNoOfFile, , .DIS3
        Put #iNoOfFile, , .DIS4
        Put #iNoOfFile, , .DIS5
        Put #iNoOfFile, , .FAM1
        Put #iNoOfFile, , .FAM2
        Put #iNoOfFile, , .FAM3
        Put #iNoOfFile, , .FAM4
        Put #iNoOfFile, , .FAM5
        Put #iNoOfFile, , .FAM6
        Put #iNoOfFile, , CInt(.IPOIDS)
        .cCostData.WriteASCII iNoOfFile
        .cCostCAtMain.WriteGlobalParamASCII iNoOfFile
        .colLoadCase.WriteASCII iNoOfFile
        .cMarsLoads.WriteASCII iNoOfFile
        'Global Data
        Put #iNoOfFile, , .YAxisOrigin
        Put #iNoOfFile, , .ZAxisOrigin
        .cGlobalConstraints.WriteASCII iNoOfFile
        Put #iNoOfFile, , .YRED
        Put #iNoOfFile, , CInt(.IsBendingMoments)
        .colEqualityRestrictions.WriteASCII iNoOfFile
        Put #iNoOfFile, , CInt(.IRESTR)
        Put #iNoOfFile, , CInt(.IULT)
        Put #iNoOfFile, , .NoOfDoubleHulls
        Put #iNoOfFile, , .MarsSymm
    End With
    Exit Function
WriteASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: Function WriteASCII")
End Function

Public Function ReadASCII(ByVal iNoOfFile As Integer)
    On Error GoTo ReadASCIIErr
    Dim ii As Integer, dd As Double, ss As String * 100
    With Me
        Get #iNoOfFile, , ss: .Title = Trim(ss)
        Get #iNoOfFile, , ii: .IMPR = ii
        Get #iNoOfFile, , ii: .IMPR2 = ii
        Get #iNoOfFile, , ii: .INDAIG = ii
        Get #iNoOfFile, , ii: .INDRAID = ii
        Get #iNoOfFile, , ii: .DESSIN = ii
        Get #iNoOfFile, , ii: .JLPH = ii
        Get #iNoOfFile, , ii: .JLBORD = ii
        Get #iNoOfFile, , ii: .NETO = ii
        Get #iNoOfFile, , ii: .IOPTI = ii
        Get #iNoOfFile, , ii: .ITERAM = ii
        Get #iNoOfFile, , ii: .ICOUT = ii
        Get #iNoOfFile, , dd: .Width = dd
        Get #iNoOfFile, , dd: .DIS1 = dd
        Get #iNoOfFile, , dd: .DIS2 = dd
        Get #iNoOfFile, , dd: .DIS3 = dd
        Get #iNoOfFile, , dd: .DIS4 = dd
        Get #iNoOfFile, , dd: .DIS5 = dd
        Get #iNoOfFile, , dd: .FAM1 = dd
        Get #iNoOfFile, , dd: .FAM2 = dd
        Get #iNoOfFile, , dd: .FAM3 = dd
        Get #iNoOfFile, , dd: .FAM4 = dd
        Get #iNoOfFile, , dd: .FAM5 = dd
        Get #iNoOfFile, , dd: .FAM6 = dd
        Get #iNoOfFile, , ii: .IPOIDS = ii
        .cCostData.ReadASCII iNoOfFile
        .colLoadCase.ReadASCII iNoOfFile
    'Global Data
        Get #iNoOfFile, , dd: .YAxisOrigin = dd
        Get #iNoOfFile, , dd: .ZAxisOrigin = dd
        .cGlobalConstraints.ReadASCII iNoOfFile
        Get #iNoOfFile, , dd: .YRED = dd
        Get #iNoOfFile, , ii: .IsBendingMoments = ii
        .colEqualityRestrictions.ReadASCII iNoOfFile
        Get #iNoOfFile, , ii: .IRESTR = ii
        Get #iNoOfFile, , ii: .IULT = ii
        Get #iNoOfFile, , ii: .NoOfDoubleHulls = ii
    End With
    Exit Function
ReadASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cHeader: Function ReadASCII")
End Function

Private Sub Class_Initialize()
    #If DebugMode Then
        mlClassDebugID = GetNextClassDebugID()
        Debug.Print "'" & TypeName(Me) & "' instance " & mlClassDebugID & " created"
    #End If
    Set mvarcCostData = New cCostData
    Set mvarcCostCAtMain = New cCostCAtMain
    Set mvarcGlobalConstraints = New cGlobalConstraints
    Set mvarcMarsLoads = New cMarsLoads
    Set mvarcMultiOpti = New cMultiOpti
End Sub

Private Sub Class_terminate()
    #If DebugMode Then
        Debug.Print "'" & TypeName(Me) & "' instance " & CStr(mlClassDebugID) & " is terminating"
    #End If
    Set mvarcCostData = Nothing
    Set mvarcCostCAtMain = Nothing
    Set mvarcGlobalConstraints = Nothing
    Set mvarcMarsLoads = Nothing
    Set mvarcMultiOpti = Nothing
End Sub

#If DebugMode Then
    Public Property Get ClassDebugID()
        ClassDebugID = mlClassDebugID
    End Property
#End If

