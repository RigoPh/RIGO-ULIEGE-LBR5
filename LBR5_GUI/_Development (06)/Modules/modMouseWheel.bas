Attribute VB_Name = "modMouseWheel"
'Private Declare Function CallWindowProc Lib "user32.dll" Alias "CallWindowProcA" ( _
'ByVal lpPrevWndFunc As Long, _
'ByVal hwnd As Long, _
'ByVal msg As Long, _
'ByVal wParam As Long, _
'ByVal lParam As Long) As Long
'
'Private Declare Function SetWindowLong Lib "user32.dll" Alias "SetWindowLongA" ( _
'ByVal hwnd As Long, _
'ByVal nIndex As Long, _
'ByVal dwNewLong As Long) As Long
'
'Public Const MK_CONTROL = &H8
'Public Const MK_LBUTTON = &H1
'Public Const MK_RBUTTON = &H2
'Public Const MK_MBUTTON = &H10
'Public Const MK_SHIFT = &H4
'Private Const GWL_WNDPROC = -4
'Private Const WM_MOUSEWHEEL = &H20A
'
'Dim LocalHwnd As Long
'Dim LocalPrevWndProc As Long
'Dim obj As PictureBox
'
'Private Function WindowProc(ByVal Lwnd As Long, ByVal Lmsg As Long, ByVal wParam As Long, ByVal lParam As Long) As Long
'
'    Dim MouseKeys As Long
'    Dim Rotation As Long
'    Dim Xpos As Long
'    Dim Ypos As Long
'
'    If Lmsg = WM_MOUSEWHEEL Then
'        MouseKeys = wParam And 65535
'        Rotation = wParam / 65536
'        Xpos = lParam And 65535
'        Ypos = lParam / 65536
'        MouseWheel MouseKeys, Rotation, Xpos, Ypos
'    End If
'    If LocalPrevWndProc <> 0 Then
'        WindowProc = CallWindowProc(LocalPrevWndProc, Lwnd, Lmsg, wParam, lParam)
'    End If
'End Function
'
'Public Sub WheelHook(PassedObj As PictureBox)
'    On Error Resume Next
'    Set obj = PassedObj
'    LocalHwnd = PassedObj.hwnd
'    LocalPrevWndProc = SetWindowLong(LocalHwnd, GWL_WNDPROC, AddressOf WindowProc)
'End Sub
'
'Public Sub WheelUnHook()
'    Dim WorkFlag As Long
'    On Error Resume Next
'    WorkFlag = SetWindowLong(LocalHwnd, GWL_WNDPROC, LocalPrevWndProc)
'    Set obj = Nothing
'End Sub
'
''To activate the hook into the Windows message stream that detects the mouse wheel "event" you should call the WheelHook() Sub from the relevant Form Activate event. You should also remember to call the WheelUnHook() Sub from the Deactivate event. This cleans up by deactivating the hook into the relevant message stream but also means that you can apply this technique to multiple forms in the same application.
'
''You will note that the WindowProc() function calls a routine on the form passed to the WheelHook() Sub as an argument. This routine is (arbitrarily) called MouseWheel() and has a number of arguments. You have to provide this Sub but there are two sample ones you might like to make use of below.
'
''The first is intended to work with an MSFlexgrid control:
'
'Public Sub MouseWheel(ByVal MouseKeys As Long, ByVal Rotation As Long, ByVal Xpos As Long, ByVal Ypos As Long)
'    Dim NewValue As Long
'    Dim Lstep As Single
'    Select Case Rotation
'        Case Is < 0
'            Rotation = -1
'        Case Is > 0
'            Rotation = 1
'    End Select
'    ZoomWheel Rotation, Xpos, Ypos
'End Sub
'
