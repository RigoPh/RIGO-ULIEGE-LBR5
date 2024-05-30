VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cScantlings"
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
Private mvarNetThickness As Double
Private mvarCorrosionThickness As Double
Private mvarGrossThickness As Double
Private mvarcPrimaryFrames As cPrimaryFrames
Private mvarcPrimaryStiffeners As cPrimaryStiffeners
Private mvarcSecondaryFrames As cSecondaryFrames
Private mvarcSecondaryStiffeners As cSecondaryStiffeners
Private mvarcolGirder As colGirder
Private mvarGirderSide As Side
Private mvarBeamSection As BeamSection
Private mvarThicknessVariation As Double

Public Property Let index(ByVal vData As Integer)
    On Error GoTo IndexLetErr
    mvarIndex = vData
    Exit Property
IndexLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cScantlings: Index Property Let")
End Property

Public Property Get index() As Integer
    On Error GoTo IndexGetErr
    index = mvarIndex
    Exit Property
IndexGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cScantlings: Index Property Get")
End Property

Public Property Let NetThickness(ByVal vData As Double)
    On Error GoTo NetThicknessErr
    mvarNetThickness = vData
    Exit Property
NetThicknessErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cScantlings: NetThickness Property Let")
End Property

Public Property Get NetThickness() As Double
    On Error GoTo NetThicknessErr
    NetThickness = mvarNetThickness
    Exit Property
NetThicknessErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cScantlings: NetThickness Property Get")
End Property

Public Property Let ThicknessVariation(ByVal vData As Double)
    On Error GoTo ThicknessVariationErr
    mvarThicknessVariation = vData
    Exit Property
ThicknessVariationErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cScantlings: ThicknessVariation Property Let")
End Property

Public Property Get ThicknessVariation() As Double
    On Error GoTo ThicknessVariationErr
    ThicknessVariation = mvarThicknessVariation
    Exit Property
ThicknessVariationErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cScantlings: ThicknessVariation Property Get")
End Property

Public Property Let CorrosionThickness(ByVal vData As Double)
    On Error GoTo CorrosionThicknessErr
    mvarCorrosionThickness = vData
    Exit Property
CorrosionThicknessErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cScantlings: CorrosionThickness Property Let")
End Property

Public Property Get CorrosionThickness() As Double
    On Error GoTo CorrosionThicknessErr
    CorrosionThickness = mvarCorrosionThickness
    Exit Property
CorrosionThicknessErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cScantlings: CorrosionThickness Property Get")
End Property

Public Property Let GrossThickness(ByVal vData As Double)
    On Error GoTo GrossThicknessErr
    mvarGrossThickness = vData
    Exit Property
GrossThicknessErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cScantlings: GrossThickness Property Let")
End Property

Public Property Get GrossThickness() As Double
    On Error GoTo GrossThicknessErr
    GrossThickness = mvarGrossThickness 'mvarNetThickness + mvarCorrosionThickness 'mvarGrossThickness
    Exit Property
GrossThicknessErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cScantlings: GrossThickness Property Get")
End Property

Public Property Get cPrimaryFrames() As cPrimaryFrames
    On Error GoTo cPrimaryFramesGetErr
    Set cPrimaryFrames = mvarcPrimaryFrames
    Exit Property
cPrimaryFramesGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cScantlings: cPrimaryFrames Property Get")
End Property

Public Property Set cPrimaryFrames(vData As cPrimaryFrames)
    On Error GoTo cPrimaryFramesSetErr
    Set mvarcPrimaryFrames = vData
    Exit Property
cPrimaryFramesSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cScantlings: cPrimaryFrames Property Set")
End Property

Public Property Get cPrimaryStiffeners() As cPrimaryStiffeners
    On Error GoTo cPrimaryStiffenersGetErr
    Set cPrimaryStiffeners = mvarcPrimaryStiffeners
    Exit Property
cPrimaryStiffenersGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cScantlings: cPrimaryStiffeners Property Get")
End Property

Public Property Set cPrimaryStiffeners(vData As cPrimaryStiffeners)
    On Error GoTo cPrimaryStiffenersSetErr
    Set mvarcPrimaryStiffeners = vData
    Exit Property
cPrimaryStiffenersSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cScantlings: cPrimaryStiffeners Property Set")
End Property

Public Property Get cSecondaryFrames() As cSecondaryFrames
    On Error GoTo cSecondaryFramesGetErr
    Set cSecondaryFrames = mvarcSecondaryFrames
    Exit Property
cSecondaryFramesGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cScantlings: cSecondaryFrames Property Get")
End Property

Public Property Set cSecondaryFrames(vData As cSecondaryFrames)
    On Error GoTo cSecondaryFramesSetErr
    Set mvarcSecondaryFrames = vData
    Exit Property
cSecondaryFramesSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cScantlings: cSecondaryFrames Property Set")
End Property

Public Property Get cSecondaryStiffeners() As cSecondaryStiffeners
    On Error GoTo cSecondaryStiffenersGetErr
    Set cSecondaryStiffeners = mvarcSecondaryStiffeners
    Exit Property
cSecondaryStiffenersGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cScantlings: cSecondaryStiffeners Property Get")
End Property

Public Property Set cSecondaryStiffeners(vData As cSecondaryStiffeners)
    On Error GoTo cSecondaryStiffenersSetErr
    Set mvarcSecondaryStiffeners = vData
    Exit Property
cSecondaryStiffenersSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cScantlings: cSecondaryStiffeners Property Set")
End Property

Public Property Get colGirder() As colGirder
    On Error GoTo colGirderGetErr
    If mvarcolGirder Is Nothing Then
        Set mvarcolGirder = New colGirder
    End If
    Set colGirder = mvarcolGirder
    Exit Property
colGirderGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cScantlings: colGirder Property Get")
End Property

Public Property Set colGirder(vData As colGirder)
    On Error GoTo colGirderSetErr
    Set mvarcolGirder = vData
    Exit Property
colGirderSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cScantlings: colGirder Property Set")
End Property

Public Property Get GirderSide() As Side
    On Error GoTo GirderSideGetErr
    GirderSide = mvarGirderSide
    Exit Property
GirderSideGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cScantlings: GirderSide Property Get")
End Property

Public Property Let GirderSide(ByVal vData As Side)
    On Error GoTo GirderSideLetErr
    mvarGirderSide = vData
    Exit Property
GirderSideLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cScantlings: GirderSide Property Let")
End Property

Public Property Let BeamSection(ByVal vData As BeamSection)
    On Error GoTo BeamSectionLetErr
    mvarBeamSection = vData
    Exit Property
BeamSectionLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cScantlings: BeamSection Property Let")
End Property

Public Property Get BeamSection() As BeamSection
    On Error GoTo BeamSectionGetErr
    BeamSection = mvarBeamSection
    Exit Property
BeamSectionGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cScantlings: BeamSection Property Get")
End Property

Public Function Clone() As cScantlings
    On Error GoTo CloneErr
    Set Clone = New cScantlings
    Set Clone.cPrimaryFrames = Me.cPrimaryFrames.Clone
    Set Clone.cPrimaryStiffeners = Me.cPrimaryStiffeners.Clone
    Set Clone.cSecondaryFrames = Me.cSecondaryFrames.Clone
    Set Clone.cSecondaryStiffeners = Me.cSecondaryStiffeners.Clone
    Set Clone.colGirder = Me.colGirder.Clone
    Clone.GirderSide = Me.GirderSide
    Clone.NetThickness = Me.NetThickness
    Clone.CorrosionThickness = Me.CorrosionThickness
    Clone.GrossThickness = Me.GrossThickness
    Clone.BeamSection = Me.BeamSection
    Exit Function
CloneErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cScantlings: Function Clone")
End Function

Public Function WriteASCII(ByVal iNoOfFile As Integer)
    On Error GoTo WriteASCIIErr
    With Me
        .cPrimaryFrames.WriteASCII iNoOfFile
        .cPrimaryStiffeners.WriteASCII iNoOfFile
        .cSecondaryFrames.WriteASCII iNoOfFile
        .cSecondaryStiffeners.WriteASCII iNoOfFile
        .colGirder.WriteASCII iNoOfFile
        Put #iNoOfFile, , CInt(.GirderSide)
        Put #iNoOfFile, , .NetThickness
        Put #iNoOfFile, , .CorrosionThickness
        Put #iNoOfFile, , .GrossThickness
        Put #iNoOfFile, , CInt(.BeamSection)
        Put #iNoOfFile, , .ThicknessVariation
    End With
    Exit Function
WriteASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cScantlings: Function WriteASCII")
End Function

Public Function ReadASCII(ByVal iNoOfFile As Integer)
    On Error GoTo ReadASCIIErr
    Dim ii As Integer, dd As Double
    With Me
        .cPrimaryFrames.ReadASCII iNoOfFile
        .cPrimaryStiffeners.ReadASCII iNoOfFile
        .cSecondaryFrames.ReadASCII iNoOfFile
        .cSecondaryStiffeners.ReadASCII iNoOfFile
        .colGirder.ReadASCII iNoOfFile
        Get #iNoOfFile, , ii: .GirderSide = ii
        Get #iNoOfFile, , dd: .NetThickness = dd
        Get #iNoOfFile, , dd: .CorrosionThickness = dd
        
        Get #iNoOfFile, , dd: .GrossThickness = dd
        Get #iNoOfFile, , ii: .BeamSection = ii
    End With
    Exit Function
ReadASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cScantlings: Function ReadASCII")
End Function


Private Sub Class_Initialize()
    #If DebugMode Then
        mlClassDebugID = GetNextClassDebugID()
        Debug.Print "'" & TypeName(Me) & "' instance " & mlClassDebugID & " created"
    #End If
    Set mvarcPrimaryStiffeners = New cPrimaryStiffeners
    Set mvarcSecondaryStiffeners = New cSecondaryStiffeners
    Set mvarcPrimaryFrames = New cPrimaryFrames
    Set mvarcSecondaryFrames = New cSecondaryFrames
    Set mvarcolGirder = New colGirder
End Sub

Private Sub Class_terminate()
    #If DebugMode Then
        Debug.Print "'" & TypeName(Me) & "' instance " & CStr(mlClassDebugID) & " is terminating"
    #End If
    Set mvarcPrimaryStiffeners = Nothing
    Set mvarcSecondaryStiffeners = Nothing
    Set mvarcPrimaryFrames = Nothing
    Set mvarcSecondaryFrames = Nothing
    Set mvarcolGirder = Nothing
End Sub

#If DebugMode Then
    Public Property Get ClassDebugID()
        ClassDebugID = mlClassDebugID
    End Property
#End If
