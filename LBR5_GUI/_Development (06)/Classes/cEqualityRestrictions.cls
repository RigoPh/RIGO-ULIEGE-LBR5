VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cEqualityRestrictions"
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
Private mvarLeadingPanel As Integer
Private mvarDependingPanel As Integer
Private mvarDependingDesignVariable As DesignVariableName
Private mvarLeadingDesignVariable As DesignVariableName
Private mvarRatio As Double

Public Property Let index(ByVal vData As Integer)
    On Error GoTo IndexLetErr
    mvarIndex = vData
    Exit Property
IndexLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cEqualityRestrictions: Index Property Let")
End Property

Public Property Get index() As Integer
    On Error GoTo IndexGetErr
    index = mvarIndex
    Exit Property
IndexGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cEqualityRestrictions: Index Property Get")
End Property

Public Property Let LeadingPanel(ByVal vData As Integer)
    On Error GoTo LeadingPanelLetErr
    mvarLeadingPanel = vData
    Exit Property
LeadingPanelLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cEqualityRestrictions: LeadingPanel Property Let")
End Property

Public Property Get LeadingPanel() As Integer
    On Error GoTo LeadingPanelGetErr
    LeadingPanel = mvarLeadingPanel
    Exit Property
LeadingPanelGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cEqualityRestrictions: LeadingPanel Property Get")
End Property

Public Property Let DependingPanel(ByVal vData As Integer)
    On Error GoTo DependingPanelLetErr
    mvarDependingPanel = vData
    Exit Property
DependingPanelLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cEqualityRestrictions: DependingPanel Property Let")
End Property

Public Property Get DependingPanel() As Integer
    On Error GoTo DependingPanelGetErr
    DependingPanel = mvarDependingPanel
    Exit Property
DependingPanelGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cEqualityRestrictions: DependingPanel Property Get")
End Property

Public Property Let DependingDesignVariable(ByVal vData As DesignVariableName)
    On Error GoTo DependingDesignVariableLetErr
    mvarDependingDesignVariable = vData
    Exit Property
DependingDesignVariableLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cEqualityRestrictions: DependingDesignVariable Property Let")
End Property

Public Property Get DependingDesignVariable() As DesignVariableName
    On Error GoTo DependingDesignVariableGetErr
    DependingDesignVariable = mvarDependingDesignVariable
    Exit Property
DependingDesignVariableGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cEqualityRestrictions: DependingDesignVariable Property Get")
End Property

Public Property Let LeadingDesignVariable(ByVal vData As DesignVariableName)
    On Error GoTo LeadingDesignVariableLetErr
    mvarLeadingDesignVariable = vData
    Exit Property
LeadingDesignVariableLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cEqualityRestrictions: LeadingDesignVariable Property Let")
End Property

Public Property Get LeadingDesignVariable() As DesignVariableName
    On Error GoTo LeadingDesignVariableGetErr
    LeadingDesignVariable = mvarLeadingDesignVariable
    Exit Property
LeadingDesignVariableGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cEqualityRestrictions: LeadingDesignVariable Property Get")
End Property

Public Property Let Ratio(ByVal vData As Double)
    On Error GoTo RatioLetErr
    mvarRatio = vData
    Exit Property
RatioLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cEqualityRestrictions: Ratio Property Let")
End Property

Public Property Get Ratio() As Double
    On Error GoTo RatioGetErr
    Ratio = mvarRatio
    Exit Property
RatioGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cEqualityRestrictions: Ratio Property Get")
End Property

Public Function GetFromString(ByRef s As String)
    On Error GoTo GetFromStringErr
    Dim v() As Variant
    GetValues 5, s, v
    mvarDependingDesignVariable = Val_(v(1))
    mvarDependingPanel = Val_(v(2))
    mvarLeadingDesignVariable = Val_(v(3))
    mvarLeadingPanel = Val_(v(4))
    mvarRatio = Val_(v(5))
    Exit Function
GetFromStringErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cEqualityRestrictions: Function GetFromString")
End Function

Public Function ReadLBR5txtFile(ts As TextStream)
    On Error GoTo ReadLBR5txtFileErr
    Dim sLine As String
    Dim v() As Variant
    sLine = ReadLn(ts)
    GetValues 5, sLine, v
    Me.DependingDesignVariable = Val_(v(1))
    Me.DependingPanel = Val_(v(2))
    Me.LeadingDesignVariable = Val_(v(3))
    Me.LeadingPanel = Val_(v(4))
    Me.Ratio = Val_(v(5))
    Exit Function
ReadLBR5txtFileErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cEqualityRestrictions: Function ReadLBR5txtFile")
End Function

Public Function Clone() As cEqualityRestrictions
    On Error GoTo CloneErr
    Set Clone = New cEqualityRestrictions
    Clone.index = Me.index
    Clone.LeadingPanel = Me.LeadingPanel
    Clone.DependingPanel = Me.DependingPanel
    Clone.DependingDesignVariable = Me.DependingDesignVariable
    Clone.LeadingDesignVariable = Me.LeadingDesignVariable
    Clone.Ratio = Me.Ratio
    Exit Function
CloneErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cEqualityRestrictions: Function Clone")
End Function

Public Function WriteASCII(ByVal iNoOfFile As Integer)
    On Error GoTo WriteASCIIErr
    With Me
        Put #iNoOfFile, , .index
        Put #iNoOfFile, , .LeadingPanel
        Put #iNoOfFile, , .DependingPanel
        Put #iNoOfFile, , CInt(.DependingDesignVariable)
        Put #iNoOfFile, , CInt(.LeadingDesignVariable)
        Put #iNoOfFile, , .Ratio
    End With
    Exit Function
WriteASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cEqualityRestrictions: Function WriteASCII")
End Function

Public Function ReadASCII(ByVal iNoOfFile As Integer)
    On Error GoTo ReadASCIIErr
    Dim ii As Integer, dd As Double
    With Me
        Get #iNoOfFile, , ii: .index = ii
        Get #iNoOfFile, , ii: .LeadingPanel = ii
        Get #iNoOfFile, , ii: .DependingPanel = ii
        Get #iNoOfFile, , ii: .DependingDesignVariable = ii
        Get #iNoOfFile, , ii: .LeadingDesignVariable = ii
        Get #iNoOfFile, , dd: .Ratio = dd
    End With
    Exit Function
ReadASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cEqualityRestrictions: Function ReadASCII")
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
