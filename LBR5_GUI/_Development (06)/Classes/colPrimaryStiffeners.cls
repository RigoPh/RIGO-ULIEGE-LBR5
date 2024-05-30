VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "colPrimaryStiffeners"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
Private mCol As Collection
Private Const PRIMARYSTIFFENERSTRINGKEY As String = "PrimaryStiffener"

Public Property Get col() As Collection
    On Error GoTo ColGetErr
    Set col = mCol
    Exit Property
ColGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colPrimaryStiffeners: Col Property Get")
End Property

Public Property Set col(vData As Collection)
    On Error GoTo ColSetErr
    Set mCol = vData
    Exit Property
ColSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colPrimaryStiffeners: Col Property Set")
End Property

Public Function Add(m As cPrimaryStiffeners, i As Integer) As Boolean
    On Error GoTo AddErr
    mCol.Add m, PRIMARYSTIFFENERSTRINGKEY & i
    Set m = Nothing
    Add = True
    Exit Function
AddErr:
    Add = False
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colPrimaryStiffeners: Function Add")
End Function

Public Function Remove(i As Integer) As Boolean
    On Error GoTo RemoveErr
    mCol.Remove (PRIMARYSTIFFENERSTRINGKEY & i)
    Remove = True
    Exit Function
RemoveErr:
    Remove = False
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colPrimaryStiffeners: Function Remove")
End Function

Public Property Get Item(i As Integer) As cPrimaryStiffeners
    On Error GoTo ItemGetErr
    Set Item = mCol(PRIMARYSTIFFENERSTRINGKEY & i)
    Exit Property
ItemGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colPrimaryStiffeners: Item Property Get")
End Property

Public Property Get Count() As Long
    On Error GoTo CountGetErr
    Count = mCol.Count
    Exit Property
CountGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colPrimaryStiffeners: Count Property Get")
End Property

Public Property Get NewEnum() As IUnknown
Attribute NewEnum.VB_UserMemId = -4
    On Error GoTo NewEnumGetErr
    Set NewEnum = mCol.[_NewEnum]
    Exit Property
NewEnumGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colPrimaryStiffeners: NewEnum Property Get")
End Property

Public Function CheckSides() As Boolean
    On Error GoTo CheckSidesErr
    Dim cPrimaryStiffeners As cPrimaryStiffeners
    Dim bLeft As Boolean, bRight As Boolean
    bLeft = False
    bRight = False
    For Each cPrimaryStiffeners In Me
        Select Case cPrimaryStiffeners.Side
            Case SideLeft
                bLeft = True
            Case SideRight
                bRight = True
        End Select
    Next cPrimaryStiffeners
    If bLeft = True And bRight = True Then
        CheckSides = True
    End If
    Exit Function
CheckSidesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colPrimaryStiffeners: Function CheckSides")
End Function

Public Function GetEquivalentStiffeners(ByRef cPanel As cPanel, ByRef colGirder As colGirder, ByRef SP As Double, ByRef HW As Double, _
                                    ByRef Tw As Double, ByRef Bf As Double, _
                                    ByRef Tf As Double, corr As Double)
    On Error GoTo GetEquivalentStiffenersErr
    Dim cPrimaryStiffener As cPrimaryStiffeners
    Dim cGirder As cGirder
    Dim WebHeight As Double
    Dim Spacing As Double
    Dim i As Integer, j As Integer
    WebHeight = 0
    i = 0
    j = 0
    
    For Each cPrimaryStiffener In mCol
        If cPrimaryStiffener.Profile <> profileNull Then
            i = i + 1
            'WebHeight = WebHeight + cPrimaryStiffener.WebHeight
        End If
    Next cPrimaryStiffener
    
    If i > 0 Then
        Spacing = mCol.Item(1).Distance
        For j = 2 To i
        'For j = 2 + 1 To i - 1
            If mCol.Item(j - 1).Distance = 0 Or mCol.Item(j).Distance = cPanel.cGeometry.PanelWidth Then
            Else
                If mCol.Item(j).Distance - mCol.Item(j - 1).Distance > Spacing Then
                    Spacing = mCol.Item(j).Distance - mCol.Item(j - 1).Distance
                End If
            End If
        Next j
    End If
'    WebHeight = Divide(WebHeight, i)  'average web height
Rewind:
'Discern Girders on basis of Web Height
'    For Each cPrimaryStiffener In mCol
'        If cPrimaryStiffener.Distance = 0 Or cPrimaryStiffener.Distance = cPanel.cGeometry.PanelWidth Then
'            GoTo ChangeIntoGirder
'        End If
'    Next cPrimaryStiffener
'    For Each cPrimaryStiffener In mCol
'        If cPrimaryStiffener.Distance = 0 Or cPrimaryStiffener.Distance = cPanel.cGeometry.PanelWidth Then
'            GoTo ChangeIntoGirder
'        End If
'        If mCol.Count = 1 Then
'            GoTo ChangeIntoGirder
'        End If
'        If cPrimaryStiffener.WebHeight > Divide(WebHeight, 0.7) Then ' 30% larger web height
'ChangeIntoGirder:
'            Set cGirder = New cGirder
'            cGirder.Index = colGirder.Count + 1
'            cGirder.Distance = cPrimaryStiffener.Distance
'            If cGirder.Distance = cPanel.cGeometry.PanelWidth Then
'                cGirder.Distance = cGirder.Distance - 0.001
'            End If
'            cGirder.FlangeWidth = cPrimaryStiffener.FlangeWidth
'            cGirder.FlangeThickness = cPrimaryStiffener.FlangeThickness
'            cGirder.WebHeight = cPrimaryStiffener.WebHeight
'            cGirder.WebThickness = cPrimaryStiffener.WebThickness
'            cPanel.cScantlings.GirderSide = cPrimaryStiffener.Side
'            colGirder.Add cGirder, cGirder.Index
'            Set cGirder = Nothing
'            mCol.Remove cPrimaryStiffener.Index
'            i = i - 1
'            Renum
'            GoTo Rewind
'        End If
'    Next cPrimaryStiffener
    'Discern Girders on basis of the profile type
'Rewind:
    For Each cPrimaryStiffener In mCol
        If cPrimaryStiffener.Profile <> profileBulb Then
ChangeIntoGirder:
            Set cGirder = New cGirder
            cGirder.index = colGirder.Count + 1
            cGirder.Distance = cPrimaryStiffener.Distance
            If cGirder.Distance = cPanel.cGeometry.PanelWidth Then
                cGirder.Distance = cGirder.Distance - 0.001
            End If
            cGirder.FlangeWidth = cPrimaryStiffener.FlangeWidth
            cGirder.FlangeThickness = cPrimaryStiffener.FlangeThickness - cPrimaryStiffener.CorrosionThickness
            cGirder.WebHeight = cPrimaryStiffener.WebHeight
            cGirder.WebThickness = cPrimaryStiffener.WebThickness - cPrimaryStiffener.CorrosionThickness
            cPanel.cScantlings.GirderSide = cPrimaryStiffener.Side
            colGirder.Add cGirder, cGirder.index
            Set cGirder = Nothing
            mCol.Remove cPrimaryStiffener.index
            i = i - 1
            Renum
            GoTo Rewind
        End If
    Next cPrimaryStiffener
    
    If i > 0 Then
        SP = Spacing 'Divide(cPanel.cGeometry.PanelWidth, (i))  'EE2
        HW = 0
        Tw = 0
        Bf = 0
        Tf = 0
        corr = 0
    Else
        SP = cPanel.cGeometry.PanelWidth
    End If
    For Each cPrimaryStiffener In mCol
        If cPrimaryStiffener.Profile <> profileNull Then
            HW = HW + cPrimaryStiffener.WebHeight
            Tw = Tw + cPrimaryStiffener.WebThickness
            Bf = Bf + cPrimaryStiffener.FlangeWidth
            Tf = Tf + cPrimaryStiffener.FlangeThickness
            corr = corr + cPrimaryStiffener.CorrosionThickness
        End If
    Next cPrimaryStiffener
    
    HW = Divide(HW, i)
    Tw = Divide(Tw, i)
    Bf = Divide(Bf, i)
    Tf = Divide(Tf, i)
    corr = Divide(corr, i)
    Exit Function
GetEquivalentStiffenersErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colPrimaryStiffeners: Function GetEquivalentStiffeners")
End Function

Public Function Renum()
    On Error GoTo RenumErr
    Dim i As Integer
    i = 0
    Dim cPrimaryStiffener As cPrimaryStiffeners
    Dim colTmp As New Collection
    For Each cPrimaryStiffener In mCol
        i = i + 1
        cPrimaryStiffener.index = i
        colTmp.Add cPrimaryStiffener, PRIMARYSTIFFENERSTRINGKEY & i
    Next cPrimaryStiffener
    Set mCol = colTmp
    Set colTmp = Nothing
    Exit Function
RenumErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colPrimaryStiffeners: Function Renum")
End Function

Public Function Clone() As colPrimaryStiffeners
    On Error GoTo CloneErr
    Dim OBJ As cPrimaryStiffeners
    Set Clone = New colPrimaryStiffeners
    For Each OBJ In mCol
        Clone.Add OBJ.Clone, OBJ.Clone.index
    Next OBJ
    Exit Function
CloneErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colPrimaryStiffeners: Function Clone")
End Function

Private Sub Class_Initialize()
    Set mCol = New Collection
End Sub

Private Sub Class_terminate()
    Set mCol = Nothing
End Sub
