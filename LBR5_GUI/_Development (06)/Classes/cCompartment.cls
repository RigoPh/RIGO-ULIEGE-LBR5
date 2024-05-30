VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cCompartment"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
#Const DebugMode = 0
#If DebugMode Then
    Private mlClassDebugID As Long
#End If

Public Enum Compartment_MainDestination
    destBallast = 1
    destCargoTankOrFuelOil = 2
    destCargoTankOfGasCarrier = 3
    destDryBulk = 4
    destSpoil = 5
    destAccommodationSpace = 6
    destCofferdamGasCarrier = 7
    destOtherDestination = 8
End Enum
Public Enum Compartment_Type
    typeTank = 1
    typeDoubleBottom_Skin = 2
    typeDryCompartment = 3
    typeEngineRoom = 4
    typeBoilerCompartment = 5
    typeTunnel = 6
    typeHopperWell1 = 7
    typeHopperWell2 = 8
End Enum
Public Enum Compartment_Load
    loadNotLoaded = 1
    loadBallast = 2
    loadLiquidCargo = 3
    loadLiquidGas = 4
    loadBulk = 5
    loadBulkAndBallast = 6
    loadSpoil = 7
End Enum

Private mvarIndex As Integer
Private mvarName As String
Private mvarMainDestination As Compartment_MainDestination
Private mvarCompartmentType As Compartment_Type
Private mvarCompartmentLoad As Compartment_Load
Private mvarLength As Double
Private mvarBreadth As Double
Private mvarHeight As Double
Private mvarXStartFromAPP As Double

Public Property Let index(ByVal vData As Integer)
    On Error GoTo IndexLetErr
    mvarIndex = vData
    Exit Property
IndexLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCompartment: Index Property Let")
End Property

Public Property Get index() As Integer
    On Error GoTo IndexGetErr
    index = mvarIndex
    Exit Property
IndexGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCompartment: Index Property Get")
End Property

Public Property Let Name(ByVal vData As String)
    On Error GoTo NameLetErr
    mvarName = vData
    Exit Property
NameLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCompartment: Name Property Let")
End Property

Public Property Get Name() As String
    On Error GoTo NameGetErr
    Name = mvarName
    Exit Property
NameGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCompartment: Name Property Get")
End Property

Public Property Let MainDestination(ByVal vData As Compartment_MainDestination)
    On Error GoTo MainDestinationLetErr
    mvarMainDestination = vData
    Exit Property
MainDestinationLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCompartment: MainDestination Property Let")
End Property

Public Property Get MainDestination() As Compartment_MainDestination
    On Error GoTo MainDestinationGetErr
    MainDestination = mvarMainDestination
    Exit Property
MainDestinationGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCompartment: MainDestination Property Get")
End Property

Public Property Let CompartmentType(ByVal vData As Compartment_Type)
    On Error GoTo CompartmentTypeLetErr
    mvarCompartmentType = vData
    Exit Property
CompartmentTypeLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCompartment: CompartmentType Property Let")
End Property

Public Property Get CompartmentType() As Compartment_Type
    On Error GoTo CompartmentTypeGetErr
    CompartmentType = mvarCompartmentType
    Exit Property
CompartmentTypeGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCompartment: CompartmentType Property Get")
End Property

Public Property Let CompartmentLoad(ByVal vData As Compartment_Type)
    On Error GoTo CompartmentLoadLetErr
    mvarCompartmentLoad = vData
    Exit Property
CompartmentLoadLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCompartment: CompartmentLoad Property Let")
End Property

Public Property Get CompartmentLoad() As Compartment_Type
    On Error GoTo CompartmentLoadGetErr
    CompartmentLoad = mvarCompartmentLoad
    Exit Property
CompartmentLoadGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCompartment: CompartmentLoad Property Get")
End Property

Public Property Let length(ByVal vData As Double)
    On Error GoTo LengthLetErr
    mvarLength = vData
    Exit Property
LengthLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCompartment: Length Property Let")
End Property

Public Property Get length() As Double
    On Error GoTo LengthGetErr
    length = mvarLength
    Exit Property
LengthGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCompartment: Length Property Get")
End Property

Public Property Let Breadth(ByVal vData As Double)
    On Error GoTo BreadthLetErr
    mvarBreadth = vData
    Exit Property
BreadthLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCompartment: Breadth Property Let")
End Property

Public Property Get Breadth() As Double
    On Error GoTo BreadthGetErr
    Breadth = mvarBreadth
    Exit Property
BreadthGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCompartment: Breadth Property Get")
End Property

Public Property Let Height(ByVal vData As Double)
    On Error GoTo HeightLetErr
    mvarHeight = vData
    Exit Property
HeightLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCompartment: Height Property Let")
End Property

Public Property Get Height() As Double
    On Error GoTo HeightGetErr
    Height = mvarHeight
    Exit Property
HeightGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCompartment: Height Property Get")
End Property

Public Property Let XStartFromAPP(ByVal vData As Double)
    On Error GoTo XStartFromAPPLetErr
    mvarXStartFromAPP = vData
    Exit Property
XStartFromAPPLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCompartment: XStartFromAPP Property Let")
End Property

Public Property Get XStartFromAPP() As Double
    On Error GoTo XStartFromAPPGetErr
    XStartFromAPP = mvarXStartFromAPP
    Exit Property
XStartFromAPPGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCompartment: XStartFromAPP Property Get")
End Property

Public Function ReadMARS_LBR5TransfertFileGeneral(ts As TextStream)
    On Error GoTo ReadMARS_LBR5TransfertFileErr
    Dim sLine As String
    Dim v() As Variant
    sLine = ReadLn(ts)
    GetValues 5, sLine, v
    Me.index = Val_(v(1))
    Me.MainDestination = Val_(v(2))
    Me.CompartmentType = Val_(v(3))
    Me.CompartmentLoad = Val_(v(4))
    Me.Name = (v(5))
    Exit Function
ReadMARS_LBR5TransfertFileErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCompartment: Function ReadMARS_LBR5TransfertFile")
End Function

Public Function ReadMARS_LBR5TransfertFileDimensions(ts As TextStream)
    On Error GoTo ReadMARS_LBR5TransfertFileDimensionsErr
    Dim sLine As String
    Dim v() As Variant
    sLine = ReadLn(ts)
    GetValues 5, sLine, v
    Me.length = Val_(v(2))
    Me.Breadth = Val_(v(3))
    Me.Height = Val_(v(4))
    Me.XStartFromAPP = Val_(v(5))
    Exit Function
ReadMARS_LBR5TransfertFileDimensionsErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCompartment: Function ReadMARS_LBR5TransfertFileDimensions")
End Function

Public Function Clone() As cCompartment
    On Error GoTo CloneErr
    Set Clone = New cCompartment
    'General
    Clone.index = Me.index
    Clone.Name = Me.Name
    Clone.MainDestination = Me.MainDestination
    Clone.CompartmentType = Me.CompartmentType
    Clone.CompartmentLoad = Me.CompartmentLoad
    'Dimensions
    Clone.length = Me.length
    Clone.Breadth = Me.Breadth
    Clone.Height = Me.Height
    Clone.XStartFromAPP = Me.XStartFromAPP
    Exit Function
CloneErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCompartment: Function Clone")
End Function

Public Function WriteASCII(ByVal iNoOfFile As Integer)
    On Error GoTo WriteASCIIErr
    Dim ss As String * 100
    With Me
        'General
        Put #iNoOfFile, , .index
        ss = .Name
        Put #iNoOfFile, , ss
        Put #iNoOfFile, , CInt(.MainDestination)
        Put #iNoOfFile, , CInt(.CompartmentType)
        Put #iNoOfFile, , CInt(.CompartmentLoad)
        'Dimensions
        Put #iNoOfFile, , .length
        Put #iNoOfFile, , .Breadth
        Put #iNoOfFile, , .Height
        Put #iNoOfFile, , .XStartFromAPP
    End With
    Exit Function
WriteASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCompartment: Function WriteASCII")
End Function

Public Function ReadASCII(ByVal iNoOfFile As Integer)
    On Error GoTo ReadASCIIErr
    Dim ii As Integer, dd As Double, ss As String * 100
    With Me
        'General
        Get #iNoOfFile, , ii: .index = ii
        Get #iNoOfFile, , ss: .Name = Trim(ss)
        Get #iNoOfFile, , ii: .MainDestination = ii
        Get #iNoOfFile, , ii: .CompartmentType = ii
        Get #iNoOfFile, , ii: .CompartmentLoad = ii
        'Dimensions
        Get #iNoOfFile, , dd: .length = dd
        Get #iNoOfFile, , dd: .Breadth = dd
        Get #iNoOfFile, , dd: .Height = dd
        Get #iNoOfFile, , dd: .XStartFromAPP = dd
    End With
    Exit Function
ReadASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCompartment: Function ReadASCII")
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

