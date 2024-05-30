VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cRGB"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
#Const DebugMode = 0
#If DebugMode Then
    Private mlClassDebugID As Long
#End If
Private mvarbtRed As Byte
Private mvarbtGreen As Byte
Private mvarbtBlue As Byte

Public Property Get btRed() As Byte
    On Error GoTo btRedGetErr
    btRed = mvarbtRed
    Exit Property
btRedGetErr:
    Call RaiseError(MyUnhandledError, "cRGB: btRed Property Get")
End Property

Public Property Let btRed(ByVal vData As Byte)
    On Error GoTo btRedLetErr
    mvarbtRed = vData
    Exit Property
btRedLetErr:
    Call RaiseError(MyUnhandledError, "cRGB: btRed Property Let")
End Property

Public Property Get btGreen() As Byte
    On Error GoTo btGreenGetErr
    btGreen = mvarbtGreen
    Exit Property
btGreenGetErr:
    Call RaiseError(MyUnhandledError, "cRGB: btGreen Property Get")
End Property

Public Property Let btGreen(ByVal vData As Byte)
    On Error GoTo btGreenLetErr
    mvarbtGreen = vData
    Exit Property
btGreenLetErr:
    Call RaiseError(MyUnhandledError, "cRGB: btGreen Property Let")
End Property

Public Property Get btBlue() As Byte
    On Error GoTo btBlueGetErr
    btBlue = mvarbtBlue
    Exit Property
btBlueGetErr:
    Call RaiseError(MyUnhandledError, "cRGB: btBlue Property Get")
End Property

Public Property Let btBlue(ByVal vData As Byte)
    On Error GoTo btBlueLetErr
    mvarbtBlue = vData
    Exit Property
btBlueLetErr:
    Call RaiseError(MyUnhandledError, "cRGB: btBlue Property Let")
End Property

Public Function SetRGB(ByVal r As Byte, ByVal g As Byte, ByVal b As Byte)
    mvarbtRed = r
    mvarbtGreen = g
    mvarbtBlue = b
End Function

Public Function GetRGB() As Variant
    Dim r As Byte, g As Byte, b As Byte
    r = mvarbtRed
    g = mvarbtGreen
    b = mvarbtBlue
    GetRGB = Array(r, g, b)
End Function

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


