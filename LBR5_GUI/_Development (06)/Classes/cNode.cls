VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cNode"
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
Private mvarnNumber As Integer
Private mvarY As Double
Private mvarZ As Double
Private mvarY_Screen As Long
Private mvarZ_Screen As Long
Private mvarRegion As Long
Private mvarSelected As SelectionState
Private mvarHighLighted As YesNo

Public Property Let index(ByVal vData As Integer)
    On Error GoTo IndexLetErr
    mvarIndex = vData
    Exit Property
IndexLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cNode: Index Property Let")
End Property

Public Property Get index() As Integer
    On Error GoTo IndexGetErr
    index = mvarIndex
    Exit Property
IndexGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cNode: Index Property Get")
End Property

Public Property Let nNumber(ByVal vData As Integer)
    On Error GoTo nNumberLetErr
    mvarnNumber = vData
    Exit Property
nNumberLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cNode: nNumber Property Let")
End Property

Public Property Get nNumber() As Integer
    On Error GoTo nNumberGetErr
    nNumber = mvarnNumber
    Exit Property
nNumberGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cNode: nNumber Property Get")
End Property

Public Property Let Y(ByVal vData As Double)
    On Error GoTo YLetErr
    mvarY = vData
    Exit Property
YLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cNode: Y Property Let")
End Property

Public Property Get Y() As Double
    On Error GoTo YGetErr
    Y = mvarY
    Exit Property
YGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cNode: Y Property Get")
End Property

Public Property Let z(ByVal vData As Double)
    On Error GoTo ZLetErr
    mvarZ = vData
    Exit Property
ZLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cNode: Z Property Let")
End Property

Public Property Get z() As Double
    On Error GoTo ZGetErr
    z = mvarZ
    Exit Property
ZGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cNode: Z Property Get")
End Property

Public Property Let Y_Screen(ByVal vData As Long)
    On Error GoTo Y_ScreenLetErr
    mvarY_Screen = vData
    Exit Property
Y_ScreenLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cNode: Y_Screen Property Let")
End Property

Public Property Get Y_Screen() As Long
    On Error GoTo Y_ScreenGetErr
    Y_Screen = mvarY_Screen
    Exit Property
Y_ScreenGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cNode: Y_Screen Property Get")
End Property

Public Property Let Z_Screen(ByVal vData As Long)
    On Error GoTo Z_ScreenLetErr
    mvarZ_Screen = vData
    Exit Property
Z_ScreenLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cNode: Z_Screen Property Let")
End Property

Public Property Get Z_Screen() As Long
    On Error GoTo Z_ScreenGetErr
    Z_Screen = mvarZ_Screen
    Exit Property
Z_ScreenGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cNode: Z_Screen Property Get")
End Property

Public Property Let Region(ByVal vData As Long)
    On Error GoTo RegionLetErr
    mvarRegion = vData
    Exit Property
RegionLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cNode: Region Property Let")
End Property

Public Property Get Region() As Long
    On Error GoTo RegionGetErr
    Region = mvarRegion
    Exit Property
RegionGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cNode: Region Property Get")
End Property

Public Property Let Selected(ByVal vData As SelectionState)
    On Error GoTo SelectedLetErr
    mvarSelected = vData
    Exit Property
SelectedLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cNode: Selected Property Let")
End Property

Public Property Get Selected() As SelectionState
    On Error GoTo SelectedGetErr
    Selected = mvarSelected
    Exit Property
SelectedGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cNode: Selected Property Get")
End Property

Public Property Let HighLighted(ByVal vData As YesNo)
    On Error GoTo HighLightedLetErr
    mvarHighLighted = vData
    Exit Property
HighLightedLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: HighLighted Property Let")
End Property

Public Property Get HighLighted() As YesNo
    On Error GoTo HighLightedGetErr
    HighLighted = mvarHighLighted
    Exit Property
HighLightedGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cPanel: HighLighted Property Get")
End Property

Public Function ReadMARS_LBR5TransfertFile(ts As TextStream)
    On Error GoTo ReadMARS_LBR5TransfertFileErr
    Dim sLine As String
    Dim v() As Variant
    sLine = ReadLn(ts)
    GetValues 3, sLine, v
    Me.index = Val_(v(1))
    Me.nNumber = Val_(v(1))
    Me.Y = Val_(v(2))
    Me.z = -Val_(v(3))
    Exit Function
ReadMARS_LBR5TransfertFileErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cNode: Function ReadMARS_LBR5TransfertFile")
End Function

Public Function Clone() As cNode
    On Error GoTo CloneErr
    Set Clone = New cNode
    With Clone
        .index = Me.index
        .nNumber = Me.nNumber
        .Y = Me.Y
        .z = Me.z
        .Y_Screen = Me.Y_Screen
        .Z_Screen = Me.Z_Screen
        .Region = Me.Region
        .Selected = Me.Selected
        .HighLighted = Me.HighLighted
    End With
    Exit Function
CloneErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cNode: Function Clone")
End Function

Public Function WriteASCII(ByVal iNoOfFile As Integer)
    On Error GoTo WriteASCIIErr
    With Me
        Put #iNoOfFile, , .index
        Put #iNoOfFile, , .nNumber
        Put #iNoOfFile, , .Y
        Put #iNoOfFile, , .z
    End With
    Exit Function
WriteASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cNode: Function WriteASCII")
End Function

Public Function ReadASCII(ByVal iNoOfFile As Integer)
    On Error GoTo ReadASCIIErr
    Dim ii As Integer, dd As Double
    With Me
        Get #iNoOfFile, , ii: .index = ii
        Get #iNoOfFile, , ii: .nNumber = ii
        Get #iNoOfFile, , dd: .Y = dd
        Get #iNoOfFile, , dd: .z = dd
    End With
    Exit Function
ReadASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cNode: Function ReadASCII")
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

