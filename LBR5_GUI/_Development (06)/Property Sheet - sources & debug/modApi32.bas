Attribute VB_Name = "modApi32"
'<CSCC>
'--------------------------------------------------------------------------------
'    Component  : modApi32
'    Project    : PropertySheet
'    Created By : Project Administrator
'    Description: Several api calls and wrapper functions
'
'    Modified   : 16/3/2004 19:26:01
'--------------------------------------------------------------------------------
'</CSCC>
Option Explicit

Private Const SM_CXBORDER = 5
Private Const SM_CXVSCROLL = 2
Private Const WS_VSCROLL = &H200000
Private Const GWL_STYLE = (-16)

Private Declare Sub CopyMemory Lib "kernel32" Alias "RtlMoveMemory" (pDest As Any, pSrc As Any, ByVal ByteLen As Long)
Private Declare Function GetSystemMetrics Lib "user32" (ByVal nIndex As Long) As Long
'Public Declare Function GetWindowLong Lib "user32" Alias "GetWindowLongA" (ByVal hwnd As Long, ByVal nIndex As Long) As Long
Public Declare Function LockWindowUpdate Lib "user32" (ByVal hWndLock As Long) As Long
Private Declare Function ApiSetCurrentDirectory Lib "kernel32" Alias "SetCurrentDirectoryA" (ByVal lpPathName As String) As Long

Public Declare Function GetTextMetrics Lib "gdi32.dll" Alias "GetTextMetricsA" (ByVal hdc As Long, ByRef lpMetrics As TEXTMETRIC) As Long

Public Type TEXTMETRIC
    tmHeight As Long
    tmAscent As Long
    tmDescent As Long
    tmInternalLeading As Long
    tmExternalLeading As Long
    tmAveCharWidth As Long
    tmMaxCharWidth As Long
    tmWeight As Long
    tmOverhang As Long
    tmDigitizedAspectX As Long
    tmDigitizedAspectY As Long
    tmFirstChar As Byte
    tmLastChar As Byte
    tmDefaultChar As Byte
    tmBreakChar As Byte
    tmItalic As Byte
    tmUnderlined As Byte
    tmStruckOut As Byte
    tmPitchAndFamily As Byte
    tmCharSet As Byte
End Type

Public Const BDR_INNER As Long = &HC
Public Const BDR_OUTER As Long = &H3
Public Const BDR_RAISED As Long = &H5
Public Const BDR_RAISEDINNER As Long = &H4
Public Const BDR_RAISEDOUTER As Long = &H1
Public Const BDR_SUNKEN As Long = &HA
Public Const BDR_SUNKENINNER As Long = &H8
Public Const BDR_SUNKENOUTER As Long = &H2
Public Const BF_LEFT As Long = &H1
Public Const BF_RIGHT As Long = &H4
Public Const BF_TOP As Long = &H2
Public Const BF_BOTTOM As Long = &H8
Public Const BF_SOFT As Long = &H1000
Public Const BF_FLAT As Long = &H4000

Public Const EDGE_RAISED = (BDR_RAISEDOUTER Or BDR_RAISEDINNER)
Public Const EDGE_SUNKEN = (BDR_SUNKENOUTER Or BDR_SUNKENINNER)
Public Const EDGE_ETCHED = (BDR_SUNKENOUTER Or BDR_RAISEDINNER)
Public Const EDGE_BUMP = (BDR_RAISEDOUTER Or BDR_SUNKENINNER)
Public Const BF_RECT As Long = (BF_LEFT Or BF_TOP Or BF_RIGHT Or BF_BOTTOM)

Public Declare Function DrawEdge Lib "user32.dll" (ByVal hdc As Long, qrc As RECT, ByVal edge As Long, ByVal grfFlags As Long) As Long

Public Declare Function GetWindowRect Lib "user32" (ByVal hwnd As Long, lpRect As RECT) As Long
Public Declare Function ClientToScreen Lib "user32" (ByVal hwnd As Long, lpPoint As POINTAPI) As Long
Public Declare Function GetClientRect Lib "user32" (ByVal hwnd As Long, lpRect As RECT) As Long
Public Declare Function GetDesktopWindow Lib "user32.dll" () As Long
Public Declare Function SetFocus Lib "user32.dll" (ByVal hwnd As Long) As Long

Public Type POINTAPI
    x As Long
    y As Long
End Type

Public Declare Function SetWindowLong Lib "user32.dll" Alias "SetWindowLongA" (ByVal hwnd As Long, ByVal nIndex As Long, ByVal dwNewLong As Long) As Long
Public Declare Function GetWindowLong Lib "user32.dll" Alias "GetWindowLongA" (ByVal hwnd As Long, ByVal nIndex As Long) As Long
Public Const GWL_EXSTYLE As Long = -20
Public Const WS_EX_TOOLWINDOW As Long = &H80&

Private Declare Function GetTickCount Lib "kernel32" () As Long

Public Function ObjectFromPtr(ByVal lPtr As Long) As Object
    Dim oThis As Object

    ' Turn the pointer into an illegal, uncounted interface
    CopyMemory oThis, lPtr, 4
    ' Do NOT hit the End button here! You will crash!
    ' Assign to legal reference
    Set ObjectFromPtr = oThis
    ' Still do NOT hit the End button here! You will still crash!
    ' Destroy the illegal reference
    CopyMemory oThis, 0&, 4
    ' OK, hit the End button if you must--you'll probably still crash,
    ' but this will be your code rather than the uncounted reference!
End Function

Public Function PtrFromObject(ByRef oThis) As Long
    ' Return the pointer to this object:
    PtrFromObject = ObjPtr(oThis)
End Function

Function TrimNull(item As String) As String
    Dim pos As Integer

    'double check there is a chr$(0) in the string
    pos = InStr(item, Chr$(0))
    If pos Then
        TrimNull = Left$(item, pos - 1)
    Else
        TrimNull = item
    End If
End Function

Function ScrollBarVisible(hWndA As Long) As Integer
   Dim StyleFlag As Long
   
   StyleFlag = GetWindowLong(hWndA, GWL_STYLE)
   If StyleFlag And WS_VSCROLL Then
      ScrollBarVisible = True
   Else
      ScrollBarVisible = False
   End If
End Function

' ******************************************************************************
' Routine       : SetCurrentDirectory
' Created by    : Marclei V Silva
' Machine       : ZEUS
' Date-Time     : 01/02/01 2:43:48
' Description   : Set windows current directory
' Inputs        :
' Outputs       :
' Credits       :
' Modifications :
' Remarks       :
' ******************************************************************************
Public Function SetCurrentDirectory(Path As Variant) As Boolean
    Dim strPath As String
    Dim lngRetVal As Long
    
    strPath = Path
    lngRetVal = ApiSetCurrentDirectory(strPath)
    SetCurrentDirectory = (lngRetVal <> 0)
End Function

Public Function Wait(ByVal TimeToWait As Long) 'Time In seconds
    Dim EndTime As Long
    
    EndTime = GetTickCount + TimeToWait * 1000 '* 1000 Cause u give seconds and GetTickCount uses Milliseconds
    Do Until GetTickCount > EndTime
        DoEvents
    Loop
End Function
'-- end code
