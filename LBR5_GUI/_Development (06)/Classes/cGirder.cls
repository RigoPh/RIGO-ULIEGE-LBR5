VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cGirder"
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
Private mvarFlangeThickness As Double
Private mvarFlangeWidth As Double
Private mvarWebHeight As Double
Private mvarWebThickness As Double
Private mvarDistance As Double
Private mvarGrossSectionModulus As Double

Public Property Let index(ByVal vData As Integer)
    On Error GoTo IndexLetErr
    mvarIndex = vData
    Exit Property
IndexLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGirder: Index Property Let")
End Property

Public Property Get index() As Integer
    On Error GoTo IndexGetErr
    index = mvarIndex
    Exit Property
IndexGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGirder: Index Property Get")
End Property

Public Property Let Distance(ByVal vData As Double)
    On Error GoTo DistanceLetError
    mvarDistance = vData
    Exit Property
DistanceLetError:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGirder: Distance Property Let")
End Property

Public Property Get Distance() As Double
    On Error GoTo DistanceGetError
    Distance = mvarDistance
    Exit Property
DistanceGetError:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGirder: Distance Property Get")
End Property

Public Property Let WebThickness(ByVal vData As Double)
    On Error GoTo WebThicknessLetErr
    mvarWebThickness = vData
    Exit Property
WebThicknessLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGirder: WebThickness Property Let")
End Property

Public Property Get WebThickness() As Double
    On Error GoTo WebThicknessGetErr
    WebThickness = mvarWebThickness
    Exit Property
WebThicknessGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGirder: WebThickness Property Let")
End Property

Public Property Let WebHeight(ByVal vData As Double)
    On Error GoTo WebHeightLetErr
    mvarWebHeight = vData
    Exit Property
WebHeightLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGirder: WebHeight Property Let")
End Property

Public Property Get WebHeight() As Double
    On Error GoTo WebHeightGetErr
    WebHeight = mvarWebHeight
    Exit Property
WebHeightGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGirder: WebHeight Property Get")
End Property

Public Property Let FlangeWidth(ByVal vData As Double)
    On Error GoTo FlangeWidthLetErr
    mvarFlangeWidth = vData
    Exit Property
FlangeWidthLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGirder: FlangeWidth Property Let")
End Property

Public Property Get FlangeWidth() As Double
    On Error GoTo FlangeWidthGetErr
    FlangeWidth = mvarFlangeWidth
    Exit Property
FlangeWidthGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGirder: FlangeWidth Property Get")
End Property

Public Property Let FlangeThickness(ByVal vData As Double)
    On Error GoTo FlangeThicknessLetErr
    mvarFlangeThickness = vData
    Exit Property
FlangeThicknessLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGirder: FlangeThickness Property Let")
End Property

Public Property Get FlangeThickness() As Double
    On Error GoTo FlangeThicknessGetErr
    FlangeThickness = mvarFlangeThickness
    Exit Property
FlangeThicknessGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGirder: FlangeThickness Property Get")
End Property

Public Property Let GrossSectionModulus(ByVal vData As Double)
    On Error GoTo GrossSectionModulusLetErr
    mvarGrossSectionModulus = vData
    Exit Property
GrossSectionModulusLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGirder: GrossSectionModulus Property Let")
End Property

Public Property Get GrossSectionModulus() As Double
    On Error GoTo GrossSectionModulusGetErr
    GrossSectionModulus = mvarGrossSectionModulus
    Exit Property
GrossSectionModulusGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGirder: GrossSectionModulus Property Get")
End Property

Public Function ReadLBR5txtFile(ts As TextStream)
    On Error GoTo ReadLBR5txtFileErr
    Dim sLine As String
    Dim v() As Variant
    sLine = ReadLn(ts)
    GetValues 5, sLine, v
    Me.WebHeight = Val_(v(1))
    Me.WebThickness = Val_(v(2))
    Me.FlangeWidth = Val_(v(3))
    Me.FlangeThickness = Val_(v(4))
    Me.Distance = Val_(v(5))
    Exit Function
ReadLBR5txtFileErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGirder: Function ReadLBR5txtFile")
End Function

Public Function Clone() As cGirder
    On Error GoTo CloneErr
    Set Clone = New cGirder
    Clone.index = Me.index
    Clone.FlangeThickness = Me.FlangeThickness
    Clone.FlangeWidth = Me.FlangeWidth
    Clone.WebHeight = Me.WebHeight
    Clone.WebThickness = Me.WebThickness
    Clone.Distance = Me.Distance
    Clone.GrossSectionModulus = Me.GrossSectionModulus
    Exit Function
CloneErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGirder: Function Clone")
End Function

Public Function WriteASCII(ByVal iNoOfFile As Integer)
    On Error GoTo WriteASCIIErr
    With Me
        Put #iNoOfFile, , CInt(.index)
        Put #iNoOfFile, , .FlangeThickness
        Put #iNoOfFile, , .FlangeWidth
        Put #iNoOfFile, , .WebHeight
        Put #iNoOfFile, , .WebThickness
        Put #iNoOfFile, , .Distance
    End With
    Exit Function
WriteASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGirder: Function WriteASCII")
End Function

Public Function ReadASCII(ByVal iNoOfFile As Integer)
    On Error GoTo ReadASCIIErr
    Dim ii As Integer, dd As Double
    With Me
        Get #iNoOfFile, , ii: .index = ii
        Get #iNoOfFile, , dd: .FlangeThickness = dd
        Get #iNoOfFile, , dd: .FlangeWidth = dd
        Get #iNoOfFile, , dd: .WebHeight = dd
        Get #iNoOfFile, , dd: .WebThickness = dd
        Get #iNoOfFile, , dd: .Distance = dd
    End With
    Exit Function
ReadASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cGirder: Function ReadASCII")
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

