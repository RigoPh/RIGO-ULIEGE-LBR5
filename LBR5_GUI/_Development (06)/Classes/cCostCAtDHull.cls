VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cCostCAtDHull"
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
Private mvarInnerShell As colIndex
Private mvarOuterShell As colIndex
Private mvarProfilesSurVoiles As Integer
Private mvarSoudureProfilesSurVoiles As SoudureContinueDiscontinue
Private mvarGoussetsProfilesVoiles As Integer
Private mvarPlatsEnBute As Integer
Private mvarAccostagesVoiles As Integer
Private mvarTypeTapes As TypeTapes
Private mvarSectionTapes As SectionLisses
Private mvarSectionLisses As SectionLisses

Public Property Let index(ByVal vData As Integer)
    On Error GoTo IndexLetErr
    mvarIndex = vData
    Exit Property
IndexLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtDHull: Index Property Let")
End Property

Public Property Get index() As Integer
    On Error GoTo IndexGetErr
    index = mvarIndex
    Exit Property
IndexGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtDHull: Index Property Get")
End Property

Public Property Get InnerShell() As colIndex
    On Error GoTo InnerShellGetErr
    If mvarInnerShell Is Nothing Then
        Set mvarInnerShell = New colIndex
    End If
    Set InnerShell = mvarInnerShell
    Exit Property
InnerShellGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtDHull: InnerShell Property Get")
End Property

Public Property Set InnerShell(vData As colIndex)
    On Error GoTo colGirderSetErr
    Set mvarInnerShell = vData
    Exit Property
colGirderSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtDHull: colGirder Property Set")
End Property

Public Property Get OuterShell() As colIndex
    On Error GoTo OuterShellGetErr
    If mvarOuterShell Is Nothing Then
        Set mvarOuterShell = New colIndex
    End If
    Set OuterShell = mvarOuterShell
    Exit Property
OuterShellGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtDHull: OuterShell Property Get")
End Property

Public Property Set OuterShell(vData As colIndex)
    On Error GoTo colGirderSetErr
    Set mvarOuterShell = vData
    Exit Property
colGirderSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtDHull: colGirder Property Set")
End Property

Public Property Let ProfilesSurVoiles(ByVal vData As Integer)
    On Error GoTo ProfilesSurVoilesLetErr
    mvarProfilesSurVoiles = vData
    Exit Property
ProfilesSurVoilesLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtDHull: ProfilesSurVoiles Property Let")
End Property

Public Property Get ProfilesSurVoiles() As Integer
    On Error GoTo ProfilesSurVoilesGetErr
    ProfilesSurVoiles = mvarProfilesSurVoiles
    Exit Property
ProfilesSurVoilesGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtDHull: ProfilesSurVoiles Property Get")
End Property

Public Property Let SoudureProfilesSurVoiles(ByVal vData As SoudureContinueDiscontinue)
    On Error GoTo SoudureProfilesSurVoilesLetErr
    mvarSoudureProfilesSurVoiles = vData
    Exit Property
SoudureProfilesSurVoilesLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtDHull: SoudureProfilesSurVoiles Property Let")
End Property

Public Property Get SoudureProfilesSurVoiles() As SoudureContinueDiscontinue
    On Error GoTo SoudureProfilesSurVoilesGetErr
    SoudureProfilesSurVoiles = mvarSoudureProfilesSurVoiles
    Exit Property
SoudureProfilesSurVoilesGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtDHull: SoudureProfilesSurVoiles Property Get")
End Property

Public Property Let GoussetsProfilesVoiles(ByVal vData As Integer)
    On Error GoTo GoussetsProfilesVoilesLetErr
    mvarGoussetsProfilesVoiles = vData
    Exit Property
GoussetsProfilesVoilesLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtDHull: GoussetsProfilesVoiles Property Let")
End Property

Public Property Get GoussetsProfilesVoiles() As Integer
    On Error GoTo GoussetsProfilesVoilesGetErr
    GoussetsProfilesVoiles = mvarGoussetsProfilesVoiles
    Exit Property
GoussetsProfilesVoilesGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtDHull: GoussetsProfilesVoiles Property Get")
End Property

Public Property Let PlatsEnBute(ByVal vData As Integer)
    On Error GoTo PlatsEnButeLetErr
    mvarPlatsEnBute = vData
    Exit Property
PlatsEnButeLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtDHull: PlatsEnBute Property Let")
End Property

Public Property Get PlatsEnBute() As Integer
    On Error GoTo PlatsEnButeGetErr
    PlatsEnBute = mvarPlatsEnBute
    Exit Property
PlatsEnButeGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtDHull: PlatsEnBute Property Get")
End Property

Public Property Let AccostagesVoiles(ByVal vData As Integer)
    On Error GoTo AccostagesVoilesLetErr
    mvarAccostagesVoiles = vData
    Exit Property
AccostagesVoilesLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtDHull: AccostagesVoiles Property Let")
End Property

Public Property Get AccostagesVoiles() As Integer
    On Error GoTo AccostagesVoilesGetErr
    AccostagesVoiles = mvarAccostagesVoiles
    Exit Property
AccostagesVoilesGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtDHull: AccostagesVoiles Property Get")
End Property

Public Property Let TypeTapes(ByVal vData As TypeTapes)
    On Error GoTo TypeTapesLetErr
    mvarTypeTapes = vData
    Exit Property
TypeTapesLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtDHull: TypeTapes Property Let")
End Property

Public Property Get TypeTapes() As TypeTapes
    On Error GoTo TypeTapesGetErr
    TypeTapes = mvarTypeTapes
    Exit Property
TypeTapesGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtDHull: TypeTapes Property Get")
End Property

Public Property Let SectionTapes(ByVal vData As SectionLisses)
    On Error GoTo SectionTapesLetErr
    mvarSectionTapes = vData
    Exit Property
SectionTapesLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtDHull: SectionTapes Property Let")
End Property

Public Property Get SectionTapes() As SectionLisses
    On Error GoTo SectionTapesGetErr
    SectionTapes = mvarSectionTapes
    Exit Property
SectionTapesGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtDHull: SectionTapes Property Get")
End Property

Public Property Let SectionLisses(ByVal vData As SectionLisses)
    On Error GoTo SectionLissesLetErr
    mvarSectionLisses = vData
    Exit Property
SectionLissesLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtDHull: SectionLisses Property Let")
End Property

Public Property Get SectionLisses() As SectionLisses
    On Error GoTo SectionLissesGetErr
    SectionLisses = mvarSectionLisses
    Exit Property
SectionLissesGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtDHull: SectionLisses Property Get")
End Property

Public Function GetSectionLisses() As String
    On Error GoTo GetSectionLissesErr
    Select Case SectionLisses
'        Case 0
'            GetSectionLisses = "SectionTapes_None"
        Case 1
            GetSectionLisses = "Profilés"
        Case 2
            GetSectionLisses = "T Synthétiques"
    End Select
    Exit Function
GetSectionLissesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtDHull: Function GetSectionLisses")
End Function

Public Function SetSectionLisses(ByRef s As String) As SectionLisses
    On Error GoTo SetSectionLissesErr
    s = Trim(s)
    Select Case s
'        Case "SectionTapes_None"
'            SetSectionLisses = SectionTapes_None
        Case "Profilés"
            SetSectionLisses = Profiles
        Case "T Synthétiques"
            SetSectionLisses = TSynthetiques
        Case Else
            SetSectionLisses = mvarSectionLisses
    End Select
    Exit Function
SetSectionLissesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtDHull: Function SetSectionLisses")
End Function

Public Function Clone() As cCostCAtDHull
    On Error GoTo CloneErr
    Set Clone = New cCostCAtDHull
    With Clone
        .index = Me.index
        Set .InnerShell = Me.InnerShell.Clone
        Set .OuterShell = Me.OuterShell.Clone
        .ProfilesSurVoiles = Me.ProfilesSurVoiles
        .SoudureProfilesSurVoiles = Me.SoudureProfilesSurVoiles
        .GoussetsProfilesVoiles = Me.GoussetsProfilesVoiles
        .PlatsEnBute = Me.PlatsEnBute
        .AccostagesVoiles = Me.AccostagesVoiles
        .TypeTapes = Me.TypeTapes
        .SectionTapes = Me.SectionTapes
        .SectionLisses = Me.SectionLisses
    End With
    Exit Function
CloneErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtDHull: Function Clone")
End Function

Public Function setDHullID_Soudure(ByRef colPanel As Collection)
    On Error GoTo setDHullID_SoudureErr
    Dim cIndex As cIndex, cPanel As cPanel
    'Inner
    For Each cIndex In Me.InnerShell
        Set cPanel = colPanel.Item(cIndex.Number)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(21).Soudures = 1
        cPanel.cCostCAtMain.colCostCAtOperations.Item(22).Soudures = 0
        cPanel.cCostCAtMain.colCostCAtOperations.Item(23).Soudures = 0
        cPanel.cCostCAtMain.colCostCAtOperations.Item(36).Soudures = 0
        cPanel.cCostCAtMain.colCostCAtOperations.Item(37).Soudures = 0
        cPanel.cCostCAtMain.colCostCAtOperations.Item(47).Soudures = 52 'après avoir defini les nappes, il faut revoir
        cPanel.cCostCAtMain.colCostCAtOperations.Item(48).Soudures = 52
        cPanel.cCostCAtMain.colCostCAtOperations.Item(51).Soudures = 0
        Set cPanel = Nothing
    Next cIndex
    'Outer
    For Each cIndex In Me.OuterShell
        Set cPanel = colPanel.Item(cIndex.Number)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(21).Soudures = 0
        cPanel.cCostCAtMain.colCostCAtOperations.Item(22).Soudures = 0
        cPanel.cCostCAtMain.colCostCAtOperations.Item(23).Soudures = 0
        cPanel.cCostCAtMain.colCostCAtOperations.Item(36).Soudures = 5
        cPanel.cCostCAtMain.colCostCAtOperations.Item(37).Soudures = 0
        cPanel.cCostCAtMain.colCostCAtOperations.Item(47).Soudures = 50 'après avoir defini les nappes, il faut revoir
        cPanel.cCostCAtMain.colCostCAtOperations.Item(48).Soudures = 50
        cPanel.cCostCAtMain.colCostCAtOperations.Item(51).Soudures = 0
        Set cPanel = Nothing
    Next cIndex
    Exit Function
setDHullID_SoudureErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtDHull: Function setDHullID_Soudure")
End Function

Public Function SetDefaultDHData(ByRef colPanel As Collection)
    On Error GoTo SetDefaultDHDataErr
    Me.ProfilesSurVoiles = GetDefaultProfilesSurVoiles
    Me.SoudureProfilesSurVoiles = SoudureContinue
    Me.GoussetsProfilesVoiles = 0
    Me.PlatsEnBute = 0
    Me.AccostagesVoiles = 0
    Me.TypeTapes = NonEtanchesRecouvrement
    Me.SectionTapes = Profiles
    Me.SectionLisses = Profiles
    Me.setDHullID_Soudure colPanel
    Exit Function
SetDefaultDHDataErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtDHull: Function SetDefaultDHData")
End Function

Private Function GetDefaultProfilesSurVoiles() As Integer
    On Error GoTo GetDefaultProfilesSurVoilesErr
    Dim cInner As cIndex, cOuter As cIndex
    Dim iNoOfStiffInner As Integer, iNoOfStiffOuter As Integer
    iNoOfStiffInner = 0
    iNoOfStiffOuter = 0
    For Each cInner In Me.InnerShell
        iNoOfStiffInner = iNoOfStiffInner + _
        Project.Item(ActiveProject).colPanel.Item(cInner.Number) _
        .cScantlings.cPrimaryStiffeners.GetNoOfStiffeners(Project. _
        Item(ActiveProject).colPanel.Item(cInner.Number))
    Next cInner
    For Each cOuter In Me.OuterShell
        iNoOfStiffOuter = iNoOfStiffOuter + _
        Project.Item(ActiveProject).colPanel.Item(cOuter.Number). _
        cScantlings.cPrimaryStiffeners.GetNoOfStiffeners(Project. _
        Item(ActiveProject).colPanel.Item(cOuter.Number))
    Next cOuter
    Select Case iNoOfStiffInner
        Case Is > iNoOfStiffOuter
            GetDefaultProfilesSurVoiles = iNoOfStiffInner
        Case Is <= iNoOfStiffOuter
            GetDefaultProfilesSurVoiles = iNoOfStiffOuter
    End Select
    Exit Function
GetDefaultProfilesSurVoilesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtDHull: Function GetDefaultProfilesSurVoiles")
End Function

Public Function GetSoudureProfilesSurVoiles() As String
    On Error GoTo GetSoudureProfilesSurVoilesErr
    Select Case SoudureProfilesSurVoiles
        Case 0
            GetSoudureProfilesSurVoiles = "Discontinue"
        Case 1
            GetSoudureProfilesSurVoiles = "Continue"
    End Select
    Exit Function
GetSoudureProfilesSurVoilesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtDHull: Function GetSoudureProfilesSurVoiles")
End Function

Public Function SetSoudureContinueDiscontinue(ByRef s As String) As SoudureContinueDiscontinue
    On Error GoTo SetSoudureContinueDiscontinueErr
    s = Trim(s)
    Select Case s
        Case "Discontinue"
            SetSoudureContinueDiscontinue = SoudureDiscontinue
        Case "Continue"
            SetSoudureContinueDiscontinue = SoudureContinue
    End Select
    Exit Function
SetSoudureContinueDiscontinueErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtDHull: Function SetSoudureContinueDiscontinue")
End Function

Public Function SetSoudureProfilesSurVoiles(ByRef s As String) As SoudureContinueDiscontinue
    On Error GoTo SetSoudureProfilesSurVoilesErr
    s = Trim(s)
    Select Case s
        Case "Discontinue"
            SetSoudureProfilesSurVoiles = SoudureDiscontinue
        Case "Continue"
            SetSoudureProfilesSurVoiles = SoudureContinue
        Case Else
            SetSoudureProfilesSurVoiles = mvarSoudureProfilesSurVoiles
    End Select
    Exit Function
SetSoudureProfilesSurVoilesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtDHull: Function SetSoudureProfilesSurVoiles")
End Function

Public Function GetTypeTapes() As String
    On Error GoTo GetTypeTapesErr
    Select Case TypeTapes
        Case 0
            GetTypeTapes = "TypeTapes_None"
        Case 1
            GetTypeTapes = "Non-Etanches - Recouvrement"
        Case 2
            GetTypeTapes = "Non-Etanches - Encastrées"
        Case 3
            GetTypeTapes = "Etanches - Recouvrement"
        Case 4
            GetTypeTapes = "Etanches - Encastrées"
    End Select
    Exit Function
GetTypeTapesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: Function GetTypeTapes")
End Function

Public Function SetTypeTapes(ByRef s As String) As TypeTapes
    On Error GoTo SetTypeTapesErr
    s = Trim(s)
    Select Case s
'        Case "TypeTapes_None"
'            SetTypeTapes = TypeTapes_None
        Case "Non-Etanches - Recouvrement"
            SetTypeTapes = NonEtanchesRecouvrement
        Case "Non-Etanches - Encastrées"
            SetTypeTapes = NonEtanchesEncastrees
        Case "Etanches - Recouvrement"
            SetTypeTapes = EtanchesRecouvrement
        Case "Etanches - Encastrées"
            SetTypeTapes = EtanchesEncastrees
        Case Else
            SetTypeTapes = mvarTypeTapes
    End Select
    Exit Function
SetTypeTapesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: Function SetTypeTapes")
End Function

Public Function GetSectionTapes() As String
    On Error GoTo GetSectionTapesErr
    Select Case SectionTapes
        Case 0
            GetSectionTapes = "SectionTapes_None"
        Case 1
            GetSectionTapes = "Profilés"
        Case 2
            GetSectionTapes = "T Synthétiques"
    End Select
    Exit Function
GetSectionTapesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: Function GetSectionTapes")
End Function

Public Function SetSectionTapes(ByRef s As String) As SectionLisses
    On Error GoTo SetSectionTapesErr
    s = Trim(s)
    Select Case s
'        Case "SectionTapes_None"
'            SetSectionTapes = SectionTapes_None
        Case "Profilés"
            SetSectionTapes = Profiles
        Case "T Synthétiques"
            SetSectionTapes = TSynthetiques
        Case Else
            SetSectionTapes = mvarSectionTapes
    End Select
    Exit Function
SetSectionTapesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: Function SetSectionTapes")
End Function

Public Function WriteASCII(ByVal iNoOfFile As Integer)
    On Error GoTo WriteASCIIErr
    With Me
        Put #iNoOfFile, , .index
        .InnerShell.WriteASCII iNoOfFile
        .OuterShell.WriteASCII iNoOfFile
        Put #iNoOfFile, , .ProfilesSurVoiles
        Put #iNoOfFile, , CInt(.SoudureProfilesSurVoiles)
        Put #iNoOfFile, , .GoussetsProfilesVoiles
        Put #iNoOfFile, , .PlatsEnBute
        Put #iNoOfFile, , .AccostagesVoiles
        Put #iNoOfFile, , CInt(.TypeTapes)
        Put #iNoOfFile, , CInt(.SectionTapes)
        Put #iNoOfFile, , CInt(.SectionLisses)
    End With
    Exit Function
WriteASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: Function WriteASCII")
End Function

Private Sub Class_Initialize()
    Set mvarInnerShell = New colIndex
    Set mvarOuterShell = New colIndex
    #If DebugMode Then
        mlClassDebugID = GetNextClassDebugID()
        Debug.Print "'" & TypeName(Me) & "' instance " & mlClassDebugID & " created"
    #End If
End Sub

Private Sub Class_terminate()
    Set mvarInnerShell = Nothing
    Set mvarOuterShell = Nothing
    #If DebugMode Then
        Debug.Print "'" & TypeName(Me) & "' instance " & CStr(mlClassDebugID) & " is terminating"
    #End If
End Sub

#If DebugMode Then
    Public Property Get ClassDebugID()
        ClassDebugID = mlClassDebugID
    End Property
#End If
