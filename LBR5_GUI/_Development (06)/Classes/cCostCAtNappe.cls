VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cCostCAtNappe"
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
Private mvarNappe As colIndex
' Pré
Private mvarAccostagesNappes As Integer
Private mvariNANP As Integer '(nombre abouts de la nappe)
Private mvarSoudureLissesNappes As SoudureContinueDiscontinue

Public Property Let index(ByVal vData As Integer)
    On Error GoTo IndexLetErr
    mvarIndex = vData
    Exit Property
IndexLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtNappe: Index Property Let")
End Property

Public Property Get index() As Integer
    On Error GoTo IndexGetErr
    index = mvarIndex
    Exit Property
IndexGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtNappe: Index Property Get")
End Property

Public Property Get Nappe() As colIndex
    On Error GoTo NappeGetErr
    If mvarNappe Is Nothing Then
        Set mvarNappe = New colIndex
    End If
    Set Nappe = mvarNappe
    Exit Property
NappeGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtNappe: Nappe Property Get")
End Property

Public Property Set Nappe(vData As colIndex)
    On Error GoTo colGirderSetErr
    Set mvarNappe = vData
    Exit Property
colGirderSetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtNappe: colGirder Property Set")
End Property

Public Property Let AccostagesNappes(ByVal vData As Integer)
    On Error GoTo AccostagesNappesLetErr
    mvarAccostagesNappes = vData
    Exit Property
AccostagesNappesLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtNappe: AccostagesNappes Property Let")
End Property

Public Property Get AccostagesNappes() As Integer
    On Error GoTo AccostagesNappesGetErr
    AccostagesNappes = mvarAccostagesNappes
    Exit Property
AccostagesNappesGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtNappe: AccostagesNappes Property Get")
End Property

Public Property Let iNANP(ByVal vData As Integer)
    On Error GoTo iNANPLetErr
    mvariNANP = vData
    Exit Property
iNANPLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtNappe: iNANP Property Let")
End Property

Public Property Get iNANP() As Integer
    On Error GoTo iNANPGetErr
    iNANP = mvariNANP
    Exit Property
iNANPGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtNappe: iNANP Property Get")
End Property

Public Property Let SoudureLissesNappes(ByVal vData As SoudureContinueDiscontinue)
    On Error GoTo SoudureLissesNappesLetErr
    mvarSoudureLissesNappes = vData
    Exit Property
SoudureLissesNappesLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtNappe: SoudureLissesNappes Property Let")
End Property

Public Property Get SoudureLissesNappes() As SoudureContinueDiscontinue
    On Error GoTo SoudureLissesNappesGetErr
    SoudureLissesNappes = mvarSoudureLissesNappes
    Exit Property
SoudureLissesNappesGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtNappe: SoudureLissesNappes Property Get")
End Property

Public Function GetSoudureLissesNappes() As String
    On Error GoTo GetSoudureLissesNappesErr
    Select Case SoudureLissesNappes
        Case 0
            GetSoudureLissesNappes = "Discontinue"
        Case 1
            GetSoudureLissesNappes = "Continue"
    End Select
    Exit Function
GetSoudureLissesNappesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtNappe: Function GetSoudureLissesNappes")
End Function

Public Function SetSoudureLissesNappes(ByRef s As String) As SoudureContinueDiscontinue
    On Error GoTo SetSoudureContinueDiscontinueErr
    s = Trim(s)
    Select Case s
        Case "Discontinue"
            SetSoudureLissesNappes = SoudureDiscontinue
        Case "Continue"
            SetSoudureLissesNappes = SoudureContinue
        Case Else
            SetSoudureLissesNappes = mvarSoudureLissesNappes
    End Select
    Exit Function
SetSoudureContinueDiscontinueErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtNappe: Function SetSoudureContinueDiscontinue")
End Function

Public Function Clone() As cCostCAtNappe
    On Error GoTo CloneErr
    Set Clone = New cCostCAtNappe
    With Clone
        .index = Me.index
        Set .Nappe = Me.Nappe
        .AccostagesNappes = Me.AccostagesNappes
        .iNANP = Me.iNANP
        .SoudureLissesNappes = Me.SoudureLissesNappes
    End With
    Exit Function
CloneErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtNappe: Function Clone")
End Function

Public Function WriteASCII(ByVal iNoOfFile As Integer)
    On Error GoTo WriteASCIIErr
    With Me
        Put #iNoOfFile, , .index
        .Nappe.WriteASCII iNoOfFile
        ' Pré
        Put #iNoOfFile, , .AccostagesNappes
        Put #iNoOfFile, , .iNANP
        Put #iNoOfFile, , CInt(.SoudureLissesNappes)
    End With
    Exit Function
WriteASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtMain: Function WriteASCII")
End Function

Private Sub Class_Initialize()
    Set mvarNappe = New colIndex
    #If DebugMode Then
        mlClassDebugID = GetNextClassDebugID()
        Debug.Print "'" & TypeName(Me) & "' instance " & mlClassDebugID & " created"
    #End If
End Sub

Private Sub Class_terminate()
    Set mvarNappe = Nothing
    #If DebugMode Then
        Debug.Print "'" & TypeName(Me) & "' instance " & CStr(mlClassDebugID) & " is terminating"
    #End If
End Sub

#If DebugMode Then
    Public Property Get ClassDebugID()
        ClassDebugID = mlClassDebugID
    End Property
#End If
