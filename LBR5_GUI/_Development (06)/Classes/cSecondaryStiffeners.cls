VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cSecondaryStiffeners"
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
Private mvarSide As Side

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
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSecondaryStiffeners: Side Property Let")
End Property

Public Property Get Side() As Side
    On Error GoTo SideGetErr
    Side = mvarSide
    Exit Property
SideGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSecondaryStiffeners: Side Property Get")
End Property

Public Property Let FlangeThickness(ByVal vData As Double)
    On Error GoTo FlangeThicknessLetErr
    mvarFlangeThickness = vData
    Exit Property
FlangeThicknessLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSecondaryStiffeners: Side Property Let")
End Property

Public Property Get FlangeThickness() As Double
    On Error GoTo FlangeThicknessGetErr
    FlangeThickness = mvarFlangeThickness
    Exit Property
FlangeThicknessGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSecondaryStiffeners: FlangeThickness Property Get")
End Property

Public Property Let FlangeWidth(ByVal vData As Double)
    On Error GoTo FlangeWidthLetErr
    mvarFlangeWidth = vData
    Exit Property
FlangeWidthLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSecondaryStiffeners: FlangeWidth Property Let")
End Property

Public Property Get FlangeWidth() As Double
    On Error GoTo FlangeWidthGetErr
    FlangeWidth = mvarFlangeWidth
    Exit Property
FlangeWidthGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSecondaryStiffeners: FlangeWidth Property Get")
End Property

Public Property Let WebThickness(ByVal vData As Double)
    On Error GoTo WebThicknessLetErr
    mvarWebThickness = vData
    Exit Property
WebThicknessLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSecondaryStiffeners: WebThickness Property Let")
End Property

Public Property Get WebThickness() As Double
    On Error GoTo WebThicknessGetErr
    WebThickness = mvarWebThickness
    Exit Property
WebThicknessGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSecondaryStiffeners: WebThickness Property Get")
End Property

Public Property Let WebHeight(ByVal vData As Double)
    On Error GoTo WebHeightLetErr
    mvarWebHeight = vData
    Exit Property
WebHeightLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSecondaryStiffeners: WebHeight Property Let")
End Property

Public Property Get WebHeight() As Double
    On Error GoTo WebHeightGetErr
    WebHeight = mvarWebHeight
    Exit Property
WebHeightGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSecondaryStiffeners: WebHeight Property Get")
End Property

Public Property Let Spacing(ByVal vData As Double)
    On Error GoTo SpacingLetErr
    mvarSpacing = vData
    Exit Property
SpacingLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSecondaryStiffeners: Spacing Property Let")
End Property

Public Property Get Spacing() As Double
    On Error GoTo SpacingGetErr
    Spacing = mvarSpacing
    Exit Property
SpacingGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSecondaryStiffeners: Spacing Property Get")
End Property

Public Function Clone() As cSecondaryStiffeners
    On Error GoTo CloneErr
    Set Clone = New cSecondaryStiffeners
    Clone.Spacing = Me.Spacing
    Clone.WebHeight = Me.WebHeight
    Clone.WebThickness = Me.WebThickness
    Clone.FlangeWidth = Me.FlangeWidth
    Clone.FlangeThickness = Me.FlangeThickness
    Clone.Side = Me.Side
    Exit Function
CloneErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSecondaryStiffeners: Function Clone")
End Function

Public Function WriteASCII(ByVal iNoOfFile As Integer)
    On Error GoTo WriteASCIIErr
    With Me
        Put #iNoOfFile, , .Spacing
        Put #iNoOfFile, , .WebHeight
        Put #iNoOfFile, , .WebThickness
        Put #iNoOfFile, , .FlangeWidth
        Put #iNoOfFile, , .FlangeThickness
        Put #iNoOfFile, , CInt(.Side)
    End With
    Exit Function
WriteASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSecondaryStiffeners: Function WriteASCII")
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
        Get #iNoOfFile, , ii: .Side = ii
    End With
    Exit Function
ReadASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cSecondaryStiffeners: Function ReadASCII")
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

