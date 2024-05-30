Attribute VB_Name = "modExeCMD"
Private Type STARTUPINFO
    CB As Long
    lpReserved As String
    lpDesktop As String
    lpTitle As String
    dwX As Long
    dwY As Long
    dwXSize As Long
    dwYSize As Long
    dwXCountChars As Long
    dwYCountChars As Long
    dwFillAttribute As Long
    dwFlags As Long
    wShowWindow As Integer
    cbReserved2 As Integer
    lpReserved2 As Long
    hStdInput As Long
    hStdOutput As Long
    hStdError As Long
End Type

Private Type PROCESS_INFORMATION
    hProcess As Long
    hThread As Long
    dwProcessID As Long
    dwThreadID As Long
End Type

Private Declare Function WaitForSingleObject Lib "kernel32" (ByVal hHandle As Long, ByVal dwMilliseconds As Long) As Long

Declare Function CreateProcessA Lib "kernel32" ( _
    ByVal lpApplicationName As Long, _
    ByVal lpCommandLine As String, _
    ByVal lpProcessAttributes As Long, _
    ByVal lpThreadAttributes As Long, _
    ByVal bInheritHandles As Long, _
    ByVal dwCreationFlags As Long, _
    ByVal lpEnvironment As Long, _
    ByVal lpCurrentDirectory As Long, _
    lpStartupInfo As STARTUPINFO, _
    lpProcessInformation As PROCESS_INFORMATION) As Long

Declare Function CloseHandle Lib "kernel32" (ByVal hObject As Long) As Long

Private Const NORMAL_PRIORITY_CLASS = &H20&
Private Const INFINITE = -1&

Public Sub ExecCmd(CmdLine As String) 'cmdline is the name of the application and any command line directions
On Error Resume Next
Dim proc As PROCESS_INFORMATION
Dim Start As STARTUPINFO
Dim ret As Long

' Initialize the STARTUPINFO structure:
Start.CB = Len(Start)

' Shelled application is invisible
'Start.dwFlags = 1
'Start.wShowWindow = SW_HIDE

' Start the shelled application:
Dim sPath As String
sPath = GetFilePath(CmdLine)
ChDrive Left(CmdLine, 3)
ChDir sPath

ret = CreateProcessA(0&, CmdLine, 0&, 0&, 1&, NORMAL_PRIORITY_CLASS, 0&, 0&, Start, proc)

' Wait for the shelled application to finish:
ret = WaitForSingleObject(proc.hProcess, INFINITE)
ret = CloseHandle(proc.hProcess)

'return focus
fMainForm.SetFocus
End Sub
