VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cRectWND"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Private mvarLeft As Double
Private mvarWIDTH As Double
Private mvarTop As Double
Private mvarHeight As Double

Public Property Let Left(ByVal vData As Double)
    On Error GoTo LeftLetErr
    mvarLeft = vData
    Exit Property
LeftLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cNode: Left Property Let")
End Property

Public Property Get Left() As Double
    On Error GoTo LeftGetErr
    Left = mvarLeft
    Exit Property
LeftGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cNode: Left Property Get")
End Property

Public Property Let Width(ByVal vData As Double)
    On Error GoTo WIDTHLetErr
    mvarWIDTH = vData
    Exit Property
WIDTHLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cNode: Width Property Let")
End Property

Public Property Get Width() As Double
    On Error GoTo WIDTHGetErr
    Width = mvarWIDTH
    Exit Property
WIDTHGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cNode: Width Property Get")
End Property

Public Property Let Top(ByVal vData As Double)
    On Error GoTo TopLetErr
    mvarTop = vData
    Exit Property
TopLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cNode: Top Property Let")
End Property

Public Property Get Top() As Double
    On Error GoTo TopGetErr
    Top = mvarTop
    Exit Property
TopGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cNode: Top Property Get")
End Property

Public Property Let Height(ByVal vData As Double)
    On Error GoTo HeightLetErr
    mvarHeight = vData
    Exit Property
HeightLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cNode: Height Property Let")
End Property

Public Property Get Height() As Double
    On Error GoTo HeightGetErr
    Height = mvarHeight
    Exit Property
HeightGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cNode: Height Property Get")
End Property

