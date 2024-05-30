VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cPrimaryFrames"
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
Private mvarSpacing As Double
Private mvarWebHeight As Double
Private mvarWebThickness As Double
Private mvarFlangeWidth As Double
Private mvarFlangeThickness As Double
Private mvarCorrosionThickness As Double
Private mvarSide As Side
Private mvarDoubleHullRgn As Long
Private mvarSpacingVariation As Double
Private mvarWebHeightVariation As Double
Private mvarWebThicknessVariation As Double
Private mvarFlangeWidthVariation As Double
Private mvarFlangeThicknessVariation As Double

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
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryFrames: Side Property Let")
End Property

Public Property Get Side() As Side
    On Error GoTo SideGetErr
    Side = mvarSide
    Exit Property
SideGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryFrames: Side Property Get")
End Property

Public Property Let Spacing(ByVal vData As Double)
    On Error GoTo SpacingLetErr
    mvarSpacing = vData
    Exit Property
SpacingLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryFrames: Spacing Property Let")
End Property

Public Property Get Spacing() As Double
    On Error GoTo SpacingGetErr
    Spacing = mvarSpacing
    Exit Property
SpacingGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryFrames: Spacing Property Get")
End Property

Public Property Let FlangeThickness(ByVal vData As Double)
    On Error GoTo FlangeThicknessLetErr
    mvarFlangeThickness = vData
    Exit Property
FlangeThicknessLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryFrames: FlangeThickness Property Let")
End Property

Public Property Get FlangeThickness() As Double
    On Error GoTo FlangeThicknessGetErr
    FlangeThickness = mvarFlangeThickness
    Exit Property
FlangeThicknessGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryFrames: FlangeThickness Property Get")
End Property

Public Property Let FlangeWidth(ByVal vData As Double)
    On Error GoTo FlangeWidthLetErr
    mvarFlangeWidth = vData
    Exit Property
FlangeWidthLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryFrames: FlangeWidth Property Let")
End Property

Public Property Get FlangeWidth() As Double
    On Error GoTo FlangeWidthGetErr
    FlangeWidth = mvarFlangeWidth
    Exit Property
FlangeWidthGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryFrames: FlangeWidth Property Get")
End Property

Public Property Let WebThickness(ByVal vData As Double)
    On Error GoTo WebThicknessLetErr
    mvarWebThickness = vData
    Exit Property
WebThicknessLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryFrames: WebThickness Property Let")
End Property

Public Property Get WebThickness() As Double
    On Error GoTo WebThicknessGetErr
    WebThickness = mvarWebThickness
    Exit Property
WebThicknessGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cprimaryFrames: WebThickness Property Get")
End Property

Public Property Let WebHeight(ByVal vData As Double)
    On Error GoTo WebHeightLetErr
    mvarWebHeight = vData
    Exit Property
WebHeightLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryFrames: WebHeight Property Let")
End Property

Public Property Get WebHeight() As Double
    On Error GoTo WebHeightGetErr
    WebHeight = mvarWebHeight
    Exit Property
WebHeightGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryFrames: WebHeight Property Get")
End Property

'---------------------
Public Property Let SpacingVariation(ByVal vData As Double)
    On Error GoTo SpacingVariationLetErr
    mvarSpacingVariation = vData
    Exit Property
SpacingVariationLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryFrames: SpacingVariation Property Let")
End Property

Public Property Get SpacingVariation() As Double
    On Error GoTo SpacingVariationGetErr
    SpacingVariation = mvarSpacingVariation
    Exit Property
SpacingVariationGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryFrames: SpacingVariation Property Get")
End Property

Public Property Let FlangeThicknessVariation(ByVal vData As Double)
    On Error GoTo FlangeThicknessVariationLetErr
    mvarFlangeThicknessVariation = vData
    Exit Property
FlangeThicknessVariationLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryFrames: FlangeThicknessVariation Property Let")
End Property

Public Property Get FlangeThicknessVariation() As Double
    On Error GoTo FlangeThicknessVariationGetErr
    FlangeThicknessVariation = mvarFlangeThicknessVariation
    Exit Property
FlangeThicknessVariationGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryFrames: FlangeThicknessVariation Property Get")
End Property

Public Property Let FlangeWidthVariation(ByVal vData As Double)
    On Error GoTo FlangeWidthVariationLetErr
    mvarFlangeWidthVariation = vData
    Exit Property
FlangeWidthVariationLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryFrames: FlangeWidthVariation Property Let")
End Property

Public Property Get FlangeWidthVariation() As Double
    On Error GoTo FlangeWidthVariationGetErr
    FlangeWidthVariation = mvarFlangeWidthVariation
    Exit Property
FlangeWidthVariationGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryFrames: FlangeWidthVariation Property Get")
End Property

Public Property Let WebThicknessVariation(ByVal vData As Double)
    On Error GoTo WebThicknessVariationLetErr
    mvarWebThicknessVariation = vData
    Exit Property
WebThicknessVariationLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryFrames: WebThicknessVariation Property Let")
End Property

Public Property Get WebThicknessVariation() As Double
    On Error GoTo WebThicknessVariationGetErr
    WebThicknessVariation = mvarWebThicknessVariation
    Exit Property
WebThicknessVariationGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cprimaryFrames: WebThicknessVariation Property Get")
End Property

Public Property Let WebHeightVariation(ByVal vData As Double)
    On Error GoTo WebHeightVariationLetErr
    mvarWebHeightVariation = vData
    Exit Property
WebHeightVariationLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryFrames: WebHeightVariation Property Let")
End Property

Public Property Get WebHeightVariation() As Double
    On Error GoTo WebHeightVariationGetErr
    WebHeightVariation = mvarWebHeightVariation
    Exit Property
WebHeightVariationGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryFrames: WebHeightVariation Property Get")
End Property

'---------------------

Public Property Let CorrosionThickness(ByVal vData As Double)
    On Error GoTo CorrosionThicknessLetErr
    mvarCorrosionThickness = vData
    Exit Property
CorrosionThicknessLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryFrames: CorrosionThickness Property Let")
End Property

Public Property Get CorrosionThickness() As Double
    On Error GoTo CorrosionThicknessGetErr
    CorrosionThickness = mvarCorrosionThickness
    Exit Property
CorrosionThicknessGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryFrames: CorrosionThickness Property Get")
End Property

Public Property Let DoubleHullRgn(ByVal vData As Long)
    On Error GoTo DoubleHullRgnLetErr
    mvarDoubleHullRgn = vData
    Exit Property
DoubleHullRgnLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryFrames: DoubleHullRgn Property Let")
End Property

Public Property Get DoubleHullRgn() As Long
    On Error GoTo DoubleHullRgnGetErr
    DoubleHullRgn = mvarDoubleHullRgn
    Exit Property
DoubleHullRgnGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryFrames: DoubleHullRgn Property Get")
End Property

Public Function Clone() As cPrimaryFrames
    On Error GoTo CloneErr
    Set Clone = New cPrimaryFrames
    Clone.Spacing = Me.Spacing
    Clone.WebHeight = Me.WebHeight
    Clone.WebThickness = Me.WebThickness
    Clone.FlangeWidth = Me.FlangeWidth
    Clone.FlangeThickness = Me.FlangeThickness
    Clone.SpacingVariation = Me.SpacingVariation
    Clone.WebHeightVariation = Me.WebHeightVariation
    Clone.WebThicknessVariation = Me.WebThicknessVariation
    Clone.FlangeWidthVariation = Me.FlangeWidthVariation
    Clone.FlangeThicknessVariation = Me.FlangeThicknessVariation
    Clone.CorrosionThickness = Me.CorrosionThickness
    Clone.Side = Me.Side
    Clone.DoubleHullRgn = Me.DoubleHullRgn
    Exit Function
CloneErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryFrames: Function Clone")
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
        Put #iNoOfFile, , .SpacingVariation
        Put #iNoOfFile, , .WebHeightVariation
        Put #iNoOfFile, , .WebThicknessVariation
        Put #iNoOfFile, , .FlangeWidthVariation
        Put #iNoOfFile, , .FlangeThicknessVariation
    End With
    Exit Function
WriteASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryFrames: Function WriteASCII")
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
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPrimaryFrames: Function ReadASCII")
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

