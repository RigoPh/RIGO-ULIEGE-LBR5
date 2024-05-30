VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "CMouseWheel"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Compare Text
Option Explicit

Private frm As Object
Private intCancel As Integer
Public Event MouseWheel(Cancel As Integer)
Private mvarRotation As Long
Private mvarXPos As Long
Private mvarYpos As Long

Public Property Let Rotation(ByVal vData As Long)
    On Error GoTo RotationLetErr
    mvarRotation = vData
    Exit Property
RotationLetErr:
    MsgBox Err.Description & " : CMouseWheel: Rotation Property Let"
End Property

Public Property Get Rotation() As Long
    On Error GoTo RotationGetErr
    Rotation = mvarRotation
    Exit Property
RotationGetErr:
    MsgBox Err.Description & ": CMouseWheel: Rotation Property Get"
End Property

Public Property Let XPos(ByVal vData As Long)
    On Error GoTo XPosLetErr
    mvarXPos = vData
    Exit Property
XPosLetErr:
    MsgBox Err.Description & " : CMouseWheel: XPos Property Let"
End Property

Public Property Get XPos() As Long
    On Error GoTo XPosGetErr
    XPos = mvarXPos
    Exit Property
XPosGetErr:
    MsgBox Err.Description & ": CMouseWheel: XPos Property Get"
End Property

Public Property Let YPos(ByVal vData As Long)
    On Error GoTo YPosLetErr
    mvarYpos = vData
    Exit Property
YPosLetErr:
    MsgBox Err.Description & " : CMouseWheel: YPos Property Let"
End Property

Public Property Get YPos() As Long
    On Error GoTo YPosGetErr
    YPos = mvarYpos
    Exit Property
YPosGetErr:
    MsgBox Err.Description & ": CMouseWheel: YPos Property Get"
End Property

Public Property Set Form(frmIn As Object)
    Set frm = frmIn
End Property

Public Property Get MouseWheelCancel() As Integer
    MouseWheelCancel = intCancel
End Property

Public Sub SubClassHookForm()
    lpPrevWndProc = SetWindowLong(frm.hwnd, GWL_WNDPROC, _
                                    AddressOf WindowProc)
      Set CMouse = Me
   End Sub

Public Sub SubClassUnHookForm()
    Call SetWindowLong(frm.hwnd, GWL_WNDPROC, lpPrevWndProc)
End Sub

Public Sub FireMouseWheel()
    RaiseEvent MouseWheel(intCancel)
End Sub
