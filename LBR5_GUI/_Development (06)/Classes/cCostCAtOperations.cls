VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cCostCAtOperations"
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
Private mvarFractionnement As Double
Private mvarAccesibilite As Double
Private mvarAtelier As Double
Private mvarSoudures As Integer
Private mvarGorges As Double

Public Property Let index(ByVal vData As Integer)
    On Error GoTo IndexLetErr
    mvarIndex = vData
    Exit Property
IndexLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtOperations: Index Property Let")
End Property

Public Property Get index() As Integer
    On Error GoTo IndexGetErr
    index = mvarIndex
    Exit Property
IndexGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtOperations: Index Property Get")
End Property

Public Property Let Fractionnement(ByVal vData As Double)
    On Error GoTo FractionnementLetErr
    mvarFractionnement = vData
    Exit Property
FractionnementLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtOperations: Fractionnement Property Let")
End Property

Public Property Get Fractionnement() As Double
    On Error GoTo FractionnementGetErr
    Fractionnement = mvarFractionnement
    Exit Property
FractionnementGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtOperations: Fractionnement Property Get")
End Property

Public Property Let Accesibilite(ByVal vData As Double)
    On Error GoTo AccesibiliteLetErr
    mvarAccesibilite = vData
    Exit Property
AccesibiliteLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtOperations: Accesibilite Property Let")
End Property

Public Property Get Accesibilite() As Double
    On Error GoTo AccesibiliteGetErr
    Accesibilite = mvarAccesibilite
    Exit Property
AccesibiliteGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtOperations: Accesibilite Property Get")
End Property

Public Property Let Atelier(ByVal vData As Double)
    On Error GoTo AtelierLetErr
    mvarAtelier = vData
    Exit Property
AtelierLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtOperations: Atelier Property Let")
End Property

Public Property Get Atelier() As Double
    On Error GoTo AtelierGetErr
    Atelier = mvarAtelier
    Exit Property
AtelierGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtOperations: Atelier Property Get")
End Property

Public Property Let Soudures(ByVal vData As Integer)
    On Error GoTo SouduresLetErr
    mvarSoudures = vData
    Exit Property
SouduresLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtOperations: Soudures Property Let")
End Property

Public Property Get Soudures() As Integer
    On Error GoTo SouduresGetErr
    Soudures = mvarSoudures
    Exit Property
SouduresGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtOperations: Soudures Property Get")
End Property

Public Property Let Gorges(ByVal vData As Double)
    On Error GoTo GorgesLetErr
    mvarGorges = vData
    Exit Property
GorgesLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtOperations: Gorges Property Let")
End Property

Public Property Get Gorges() As Double
    On Error GoTo GorgesGetErr
    Gorges = mvarGorges
    Exit Property
GorgesGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtOperations: Gorges Property Get")
End Property

Public Function Clone() As cCostCAtOperations
    On Error GoTo CloneErr
    Set Clone = New cCostCAtOperations
    Clone.index = Me.index
    Clone.Fractionnement = Me.Fractionnement
    Clone.Accesibilite = Me.Accesibilite
    Clone.Atelier = Me.Atelier
    Clone.Soudures = Me.Soudures
    Clone.Gorges = Me.Gorges
    Exit Function
CloneErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtOperations: Function Clone")
End Function

Public Function WriteASCII(ByVal iNoOfFile As Integer)
    On Error GoTo WriteASCIIErr
    With Me
        Put #iNoOfFile, , .index
        Put #iNoOfFile, , .Fractionnement
        Put #iNoOfFile, , .Accesibilite
        Put #iNoOfFile, , .Atelier
        Put #iNoOfFile, , .Soudures
        Put #iNoOfFile, , .Gorges
    End With
    Exit Function
WriteASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostCAtOperations: Function WriteASCII")
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
