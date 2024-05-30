VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "colSecondaryFrames"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
Private mCol As Collection
Private Const SECONDARYFRAMESTRINGKEY As String = "SecondaryFrame"

Public Property Get col() As Collection
    On Error GoTo ColGetErr
    Set col = mCol
    Exit Property
ColGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colSecondaryFrames: Col Property Get")
End Property

Public Function Add(m As cSecondaryFrames, i As Integer) As Boolean
    On Error GoTo AddErr
    mCol.Add m, SECONDARYFRAMESTRINGKEY & i
    Set m = Nothing
    Add = True
    Exit Function
AddErr:
    Add = False
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colSecondaryFrames: Function Add")
End Function

Public Function Remove(i As Integer) As Boolean
    On Error GoTo RemoveErr
    mCol.Remove (SECONDARYFRAMESTRINGKEY & i)
    Remove = True
    Exit Function
RemoveErr:
    Remove = False
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colSecondaryFrames: Function Remove")
End Function

Public Property Get Item(i As Integer) As cSecondaryFrames
    On Error GoTo ItemGetErr
    Set Item = mCol(SECONDARYFRAMESTRINGKEY & i)
    Exit Property
ItemGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colSecondaryFrames: Item Property Get")
End Property

Public Property Get Count() As Long
    On Error GoTo CountGetErr
    Count = mCol.Count
    Exit Property
CountGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colSecondaryFrames: Count Property Get")
End Property

Public Property Get NewEnum() As IUnknown
Attribute NewEnum.VB_UserMemId = -4
    On Error GoTo NewEnumGetErr
    Set NewEnum = mCol.[_NewEnum]
    Exit Property
NewEnumGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colSecondaryFrames: NewEnum Property Get")
End Property

Public Function CheckSides() As Boolean
    On Error GoTo CheckSidesErr
    Dim cSecondaryFrames As cSecondaryFrames
    Dim bLeft As Boolean, bRight As Boolean
    bLeft = False
    bRight = False
    For Each cSecondaryFrames In Me
        Select Case cSecondaryFrames.Side
            Case SideLeft
                bLeft = True
            Case SideRight
                bRight = True
        End Select
    Next cSecondaryFrames
    If bLeft = True And bRight = True Then
        CheckSides = True
    End If
    Exit Function
CheckSidesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colSecondaryFrames: Function CheckSides")
End Function

Public Function GetEquivalentFrames(ByRef SP As Double, ByRef HW As Double, _
                                    ByRef Tw As Double, ByRef Bf As Double, _
                                    ByRef Tf As Double)
    On Error GoTo GetEquivalentFramesErr
    Dim cSecondaryFrames As cSecondaryFrames
    SP = 0
    HW = 0
    Tw = 0
    Bf = 0
    Tf = 0
    For Each cSecondaryFrames In Me
        SP = SP + cSecondaryFrames.Spacing * cSecondaryFrames.WidthRatio
        HW = HW + cSecondaryFrames.WebHeight * cSecondaryFrames.WidthRatio
        Tw = Tw + cSecondaryFrames.WebThickness * cSecondaryFrames.WidthRatio
        Bf = Bf + cSecondaryFrames.FlangeWidth * cSecondaryFrames.WidthRatio
        Tf = Tf + cSecondaryFrames.FlangeThickness * cSecondaryFrames.WidthRatio
    Next cSecondaryFrames
    Exit Function
GetEquivalentFramesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colSecondaryFrames: Function GetEquivalentFrames")
End Function

Public Function Clone() As colSecondaryFrames
    On Error GoTo CloneErr
    Dim OBJ As cSecondaryFrames
    Set Clone = New colSecondaryFrames
    For Each OBJ In mCol
        Clone.Add OBJ.Clone, OBJ.Clone.index
    Next OBJ
    Exit Function
CloneErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colSecondaryFrames: Function Clone")
End Function

Private Sub Class_Initialize()
    Set mCol = New Collection
End Sub

Private Sub Class_terminate()
    Set mCol = Nothing
End Sub

