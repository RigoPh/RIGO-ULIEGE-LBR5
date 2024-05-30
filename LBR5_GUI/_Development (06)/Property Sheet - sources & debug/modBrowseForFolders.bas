Attribute VB_Name = "modBrowseForFolders"
Option Explicit

'Description: Calls the "Open File Dialog" without need for an OCX
'Be careful when dealing with this and the "Save File Dialog", the
'Type and examples are the same. It can be confusing...

Private Type BrowseInfo
     hWndOwner As Long
     pIDLRoot As Long
     pszDisplayName As Long
     lpszTitle As Long
     ulFlags As Long
     lpfnCallback As Long
     lParam As Long
     iImage As Long
End Type

Private Const BIF_RETURNONLYFSDIRS = 1
Private Const MAX_PATH = 260
Private Const EM_SETSEL = &HB1

Private Const BIF_STATUSTEXT = &H4&
Private Const BIF_DONTGOBELOWDOMAIN = 2

Private Const WM_USER = &H400
Private Const BFFM_INITIALIZED = 1
Private Const BFFM_SELCHANGED = 2
Private Const BFFM_SETSTATUSTEXT = (WM_USER + 100)
Private Const BFFM_SETSELECTION = (WM_USER + 102)

' the current directory
Private m_CurrentDirectory As String

'Enter each the following declarations on a single line:
Private Declare Function SHBrowseForFolder Lib "shell32" (lpbi As BrowseInfo) As Long
Private Declare Function SHGetPathFromIDList Lib "shell32" (ByVal pidList As Long, ByVal lpBuffer As String) As Long
Private Declare Function lstrcat Lib "kernel32" Alias "lstrcatA" (ByVal lpString1 As String, ByVal lpString2 As String) As Long

' public declares
'Public Declare Function SendMessageByString Lib "user32" Alias "SendMessageA" (ByVal hwnd As Long, ByVal wMsg As Long, ByVal wParam As Long, lParam As Any) As Long
Public Declare Function SendMessageByString Lib "user32" Alias "SendMessageA" (ByVal hwnd As Long, ByVal wMsg As Long, ByVal wParam As Long, ByVal lParam As String) As Long
Public Declare Sub CoTaskMemFree Lib "ole32.dll" (ByVal hMem As Long)

Public Function BrowseForFolder(ByVal hWndOwner As Long, ByVal Title As String, Path As String) As Boolean
    ' Opens a Treeview control that displays the directories in a computer
    Dim lpIDList As Long
    Dim szTitle As String
    Dim sBuffer As String
    Dim tBrowseInfo As BrowseInfo
    Dim iNull As Integer
    
    m_CurrentDirectory = Path & vbNullChar
    szTitle = Title
    With tBrowseInfo
        .hWndOwner = hWndOwner
        .lpszTitle = lstrcat(szTitle, "")
        .ulFlags = BIF_RETURNONLYFSDIRS + BIF_DONTGOBELOWDOMAIN + BIF_STATUSTEXT
        .lpfnCallback = GetAddressofFunction(AddressOf BrowseCallbackProc)  'get address of function.
    End With
    lpIDList = SHBrowseForFolder(tBrowseInfo)
    If (lpIDList) Then
        sBuffer = String(MAX_PATH, 0)
        SHGetPathFromIDList lpIDList, sBuffer
        Call CoTaskMemFree(lpIDList)
        iNull = InStr(sBuffer, vbNullChar)
        If iNull Then
            sBuffer = Left(sBuffer, iNull - 1)
        End If
        Path = sBuffer
        BrowseForFolder = True
    Else
        BrowseForFolder = False
    End If
End Function
 
Private Function BrowseCallbackProc(ByVal hwnd As Long, ByVal uMsg As Long, ByVal lp As Long, ByVal pData As Long) As Long
    Dim lpIDList As Long
    Dim ret As Long
    Dim sBuffer As String
  
    On Error Resume Next  'Sugested by MS to prevent an error from
    ' propagating back into the calling process.
    Select Case uMsg
        Case BFFM_INITIALIZED
            Call SendMessageByString(hwnd, BFFM_SETSELECTION, 1, m_CurrentDirectory)
        Case BFFM_SELCHANGED
            sBuffer = String(MAX_PATH, 0)
            ret = SHGetPathFromIDList(lp, sBuffer)
            Call CoTaskMemFree(ret)
            If ret = 1 Then
                Call SendMessageByString(hwnd, BFFM_SETSTATUSTEXT, 0, sBuffer)
            End If
    End Select
    BrowseCallbackProc = 0
End Function

' This function allows you to assign a function pointer to a vaiable.
Private Function GetAddressofFunction(Add As Long) As Long
    GetAddressofFunction = Add
End Function
