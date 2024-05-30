Attribute VB_Name = "modFileLock"
Const FILE_SHARE_READ = &O1
Const FILE_SHARE_WRITE = &O2
Const GENERIC_ALL = &H10000000
Const GENERIC_READ = &H80000000
Const GENERIC_WRITE = &H40000000
Const OPEN_EXISTING = 3
Const INVALID_HANDLE_VALUE = -1

Private Type SYSTEMTIME
     wYear As Integer
     wMonth As Integer
     wDayOfWeek As Integer
     wDay As Integer
     wHour As Integer
     wMinute As Integer
     wSecond As Integer
     wMilliseconds As Integer
End Type

Private Declare Function GetFileTime& Lib "kernel32" (ByVal hFile As Long, _
lpCreationTime As Currency, lpLastAccessTime As Currency, lpLastWriteTime As _
Currency)

Private Declare Function GetFileSize& Lib "kernel32" (ByVal hFile As Long, _
lpFileSizeHigh As Long)

Private Declare Function CreateFile Lib "kernel32" Alias "CreateFileA" (ByVal lpFileName As String, ByVal dwDesiredAccess As Long, ByVal dwSharedMode As Long, ByVal lpSecurityAttributes As Long, ByVal dwCreationDisposition As Long, ByVal dwFlagsAndAttributes As Long, ByVal hTemplateFile As Long) As Long
Private Declare Function CloseHandle Lib "kernel32" (ByVal hObject As Long) As Long

Public Function GetFileLock(ByRef FileName As String) As String

Dim fil As file
Dim hFile As Long
Dim filesize As Long
hFile = CreateFile(FileName, GENERIC_READ, FILE_SHARE_READ Or FILE_SHARE_WRITE, ByVal 0&, OPEN_EXISTING, 0, 0)

Dim last_write As Currency
Dim creation_time As Currency
Dim last_access As Currency
GetFileTime hFile, creation_time, last_access, last_write
If hFile = INVALID_HANDLE_VALUE Then
    MsgBox "Error Reading File: " & FileName, vbCritical + vbOKOnly
    Exit Function
End If

filesize = GetFileSize(hFile, 0)
Dim dt As Date
dt = (last_write / 1000 - 9435312000#) / 86400
CloseHandle (hFile)

seed = getSommeTime(dt) + filesize
Dim firstseed As Long
firstseed = seed

Dim y As Long

Dim alphanumeric As String
alphanumeric = "0123456789abcdefghijklmnoprstxyuvwzABCDEFGHIJKLMNOPRSTXYUVWZ0123456789abcdefghijklmnoprstxyuvwzABCDEFGHIJKLMNOPRSTXYUVWZ0123456789abcdefghijklmnoprstxyuvwzABCDEFGHIJKLMNOPRSTXYUVWZ0123456789abcdefghijklmnoprstxyuvwzABCDEFGHIJKLMNOPRSTXYUVWZ0123456789abcde" 'len = 255

Dim codex As String
For i = 1 To 16
    y = (57 * ((57 * seed + 1) Mod 256) + 1) Mod 256
    If y = 0 Then y = 1
    codex = codex + Mid(alphanumeric, y, 1)
    seed = y
Next i

GetFileLock = codex

'    Dim ts As Object
'    open_file "secu.txt", 1, ts
'    ts.WriteLine "------------------------------"
'    ts.WriteLine "file size:" & vbTab & filesize
'    ts.WriteLine "first seed:" & vbTab & firstseed
'
'
'    ts.WriteLine "------------------------------"
'
'    ts.Close

End Function

Function getSommeTime(ByVal dt As Date) As Long
    
    Dim i As Integer
    Dim s As String
    s = Trim(str(dt))
    getSommeTime = 0
    
    Dim stime As String, sdate As String
    Dim pos As Integer

    pos = InStr(1, s, " ")
    sdate = Left(s, pos - 1)
    stime = Mid(s, pos + 1)

    Dim v_time(0, 5) As String

    'time
    pos = InStrRev(stime, ":", Len(stime))
    v_time(0, 0) = Mid(stime, pos + 1) 'sec
    stime = Left(stime, pos - 1)
    pos = InStrRev(stime, ":", Len(stime))
    v_time(0, 1) = Mid(stime, pos + 1) 'min
    stime = Left(stime, pos - 1)
    v_time(0, 2) = stime 'hour

    'date
    pos = InStrRev(sdate, "/", Len(sdate))
    v_time(0, 5) = Mid(sdate, pos + 1) 'year
    sdate = Left(sdate, pos - 1)
    pos = InStrRev(sdate, "/", Len(sdate))
    v_time(0, 4) = Mid(sdate, pos + 1) 'month
    sdate = Left(sdate, pos - 1)
    v_time(0, 3) = sdate 'day

    For i = 0 To 5
        getSommeTime = getSommeTime + CInt(v_time(0, i))
    Next i
    
'    Dim ts As Object
'    open_file "secu.txt", 0, ts
'    ts.WriteLine "seconds:" & vbTab & v_time(0, 0)
'    ts.WriteLine "minutes:" & vbTab & v_time(0, 1)
'    ts.WriteLine "hours:" & vbTab & v_time(0, 2)
'    ts.WriteLine "day:" & vbTab & v_time(0, 3)
'    ts.WriteLine "month:" & vbTab & v_time(0, 4)
'    ts.WriteLine "year:" & vbTab & v_time(0, 5)
'    ts.WriteLine "------------------------------"
'    ts.WriteLine "time sum:" & vbTab & getSommeTime
'
'    ts.Close
    
End Function
