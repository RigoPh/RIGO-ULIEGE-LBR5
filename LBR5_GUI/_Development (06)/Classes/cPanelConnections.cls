VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cPanelConnections"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit
#Const DebugMode = 1
#If DebugMode Then
    Private mlClassDebugID As Long
#End If
Private mvarConnectedPanel As Integer

Public Property Let ConnectedPanel(ByVal vData As Integer)
    On Error GoTo ConnectedPanelLetErr
    mvarConnectedPanel = vData
    Exit Property
ConnectedPanelLetErr:
    Call RaiseError(MyUnhandledError, "cPanelConnections: ConnectedPanel Property Let")
End Property

Public Property Get ConnectedPanel() As Integer
    On Error GoTo ConnectedPanelGetErr
    ConnectedPanel = mvarConnectedPanel
    Exit Property
ConnectedPanelGetErr:
    Call RaiseError(MyUnhandledError, "cPanelConnections: ConnectedPanel Property Get")
End Property

Private Sub Class_Initialize()
    #If DebugMode Then
        mlClassDebugID = GetNextClassDebugID()
        Debug.Print "'" & TypeName(Me) & "' instance " & mlClassDebugID & " created"
    #End If
End Sub

Private Sub Class_Terminate()
    #If DebugMode Then
        Debug.Print "'" & TypeName(Me) & "' instance " & CStr(mlClassDebugID) & " is terminating"
    #End If
End Sub

#If DebugMode Then
    Public Property Get ClassDebugID()
        ClassDebugID = mlClassDebugID
    End Property
#End If

