Attribute VB_Name = "basSubClassWindow"
Option Compare Text
Option Explicit

Public CMouse As CMouseWheel
Declare Function SetWindowLong Lib "user32" Alias "SetWindowLongA" _
    (ByVal hwnd As Long, _
    ByVal nIndex As Long, _
    ByVal dwNewLong As Long) As Long

Public Declare Function CallWindowProc Lib "user32" Alias "CallWindowProcA" _
    (ByVal lpPrevWndFunc As Long, _
     ByVal hwnd As Long, _
     ByVal msg As Long, _
     ByVal wParam As Long, _
     ByVal lParam As Long) As Long

Public Const GWL_WNDPROC = -4
Public Const WM_MouseWheel = &H20A
Public lpPrevWndProc As Long

Public Function WindowProc(ByVal hwnd As Long, _
    ByVal uMsg As Long, _
    ByVal wParam As Long, _
    ByVal lParam As Long) As Long
    Select Case uMsg
        Case WM_MouseWheel
            CMouse.XPos = lParam And 65535
            CMouse.YPos = lParam / 65536
            Select Case wParam / 65536
                Case Is < 0
                    CMouse.Rotation = -1
                Case Is > 0
                    CMouse.Rotation = 1
            End Select
            CMouse.FireMouseWheel
            If CMouse.MouseWheelCancel = False Then
                WindowProc = CallWindowProc(lpPrevWndProc, hwnd, uMsg, wParam, lParam)
            End If
        Case Else
           WindowProc = CallWindowProc(lpPrevWndProc, hwnd, uMsg, wParam, lParam)
    End Select
End Function
