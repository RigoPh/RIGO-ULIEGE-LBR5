Attribute VB_Name = "modFileAssociation"
' ============================================================================
' Rutina care porneste aplicatia pornind de la dublu click pe fisierul de date
' ============================================================================

Option Explicit

Private Declare Function RegCreateKey Lib "advapi32.dll" Alias "RegCreateKeyA" (ByVal hKey As Long, ByVal lpSubKey As String, phkResult As Long) As Long
Private Declare Function RegSetValue Lib "advapi32.dll" Alias "RegSetValueA" (ByVal hKey As Long, ByVal lpSubKey As String, ByVal dwType As Long, ByVal lpData As String, ByVal cbData As Long) As Long
Private Declare Function RegEnumKey Lib "advapi32.dll" Alias "RegEnumKeyA" (ByVal hKey As Long, ByVal dwIndex As Long, ByVal lpName As String, ByVal cbName As Long) As Long
Private Declare Function RegDeleteKey Lib "advapi32.dll" Alias "RegDeleteKeyA" (ByVal hKey As Long, ByVal lpSubKey As String) As Long
Private Declare Function RegOpenKeyEx Lib "advapi32.dll" Alias "RegOpenKeyExA" (ByVal hKey As Long, ByVal lpSubKey As String, ByVal Reserved As Long, ByVal samDesired As Long, phkResult As Long) As Long
Private Declare Function RegCloseKey Lib "advapi32.dll" (ByVal hKey As Long) As Long
Private Declare Function RegDeleteValue Lib "advapi32.dll" Alias "RegDeleteValueA" (ByVal hKey As Long, ByVal lpValueName As String) As Long
Private Declare Function RegQueryValueEx Lib "advapi32.dll" Alias _
   "RegQueryValueExA" (ByVal hKey As Long, ByVal lpValueName As _
   String, ByVal lpReserved As Long, lpType As Long, ByVal lpData _
   As String, lpcbData As Long) As Long

' Return codes from Registration functions.
Const ERROR_SUCCESS As Long = 0
Const ERROR_BADDB As Long = 1
Const ERROR_BADKEY As Long = 2
Const ERROR_CANTOPEN As Long = 3
Const ERROR_CANTREAD As Long = 4
Const ERROR_CANTWRITE As Long = 5
Const ERROR_OUTOFMEMORY As Long = 6
Const ERROR_INVALID_PARAMETER As Long = 7
Const ERROR_ACCESS_DENIED As Long = 8
      
Private Const HKEY_CLASSES_ROOT = &H80000000
Private Const MAX_PATH As Long = 260
Private Const MAX_VALUE_NAME As Long = 260
Private Const REG_SZ = 1

Const HKEY_CURRENT_USER = &H80000001
Const REG_OPTION_BACKUP_RESTORE = 4     ' open for backup or restore
Const REG_OPTION_VOLATILE = 1           ' Key is not preserved when system is rebooted
Const REG_OPTION_NON_VOLATILE = 0       ' Key is preserved when system is rebooted
Const STANDARD_RIGHTS_ALL = &H1F0000
Const SYNCHRONIZE = &H100000
Const READ_CONTROL = &H20000
Const STANDARD_RIGHTS_READ = (READ_CONTROL)
Const STANDARD_RIGHTS_WRITE = (READ_CONTROL)
Const KEY_CREATE_LINK = &H20
Const KEY_CREATE_SUB_KEY = &H4
Const KEY_ENUMERATE_SUB_KEYS = &H8
Const KEY_NOTIFY = &H10
Const KEY_QUERY_VALUE = &H1
Const KEY_SET_VALUE = &H2
Const KEY_READ = ((STANDARD_RIGHTS_READ Or KEY_QUERY_VALUE Or KEY_ENUMERATE_SUB_KEYS Or KEY_NOTIFY) And (Not SYNCHRONIZE))
Const KEY_WRITE = ((STANDARD_RIGHTS_WRITE Or KEY_SET_VALUE Or KEY_CREATE_SUB_KEY) And (Not SYNCHRONIZE))
Const KEY_EXECUTE = (KEY_READ)
Const KEY_ALL_ACCESS = ((STANDARD_RIGHTS_ALL Or KEY_QUERY_VALUE Or KEY_SET_VALUE Or KEY_CREATE_SUB_KEY Or KEY_ENUMERATE_SUB_KEYS Or KEY_NOTIFY Or KEY_CREATE_LINK) And (Not SYNCHRONIZE))

Public Const GW_HWNDNEXT = 2
Public Const GW_HWNDPREV = 3
Public Const GW_CHILD = 5
Public Const WM_SETTEXT = &HC
Public Const WM_GETTEXTLENGTH = &HE

Declare Function GetWindowText Lib "user32" Alias "GetWindowTextA" _
        (ByVal hwnd As Long, ByVal lpString As String, ByVal cch As Long) As Long
Declare Function OpenIcon Lib "user32" (ByVal hwnd As Long) As Long
Declare Function FindWindow Lib "user32" Alias "FindWindowA" _
        (ByVal lpClassName As String, ByVal lpWindowName As String) As Long
Declare Function GetWindow Lib "user32" (ByVal hwnd As Long, ByVal wCmd As Long) As Long
Declare Function SetForegroundWindow Lib "user32" (ByVal hwnd As Long) As Long
Declare Function SendMessageText Lib "user32" Alias "SendMessageA" _
        (ByVal hwnd As Long, ByVal wMsg As Long, ByVal wParam As Long, ByVal strText As String) As Long
Declare Function GetClassName Lib "user32" Alias "GetClassNameA" _
        (ByVal hwnd As Long, ByVal lpClassName As String, ByVal nMaxCount As Long) As Long

Sub ActivatePrevInstance()
    Dim OldTitle As String
    Dim PrevHndl As Long
    Dim Result As Long
    Dim length As Long
    Dim ChildHwnd As Long
    Dim ChildClassName As String
    Dim CmdText As String

    'Save the title of the application.
    OldTitle = App.Title

    'Rename the title of this application so FindWindow
    'will not find this application instance.
    App.Title = "Unwanted instance"

    'Attempt to get window handle using VB6 class name
    PrevHndl = FindWindow("ThunderRT6Main", OldTitle)

    'Check if found
    If PrevHndl = 0 Then
        'No previous instance found.
        Exit Sub
    End If

    'Get handle to previous window.
    PrevHndl = GetWindow(PrevHndl, GW_HWNDPREV)

    'Restore the program.
    Result = OpenIcon(PrevHndl)

    'Activate the application.
    Result = SetForegroundWindow(PrevHndl)

    'Try to find the textbox on form
    ChildHwnd = GetWindow(PrevHndl, GW_CHILD)
    Do While (ChildHwnd <> 0)
        ' Get the child's class name.
        ChildClassName = Space(1024)
        length = GetClassName(ChildHwnd, ChildClassName, Len(ChildClassName))
        ChildClassName = Left(ChildClassName, length)
        ' See if this is the target.
        If Trim(ChildClassName) = "ThunderRT6TextBox" Then
            Exit Do
        End If
        ChildHwnd = GetWindow(ChildHwnd, GW_HWNDNEXT)
    Loop
    
    'Check if found
    If (ChildHwnd <> 0) Then
        CmdText = Command()
        Call SendMessageText(ChildHwnd, WM_SETTEXT, 0, CmdText)
    End If
         
End Sub

Public Sub AssociateFileExtension(Extension As String, PathToExecute As String, ApplicationName As String)
 
'   ApplicationName is what shows in the list (Registered file types) when you click
'   My Computer, View, Folder Options, File Types
 
'   Extension is three letters without the "."

    On Error GoTo AssociateErrorHandler
    
    Dim sKeyName As String   'Holds Key Name in registry.
    Dim sKeyValue As String  'Holds Key Value in registry.
    Dim ret As Long          'Holds error status if any from API calls.
    Dim lphKey As Long       'Holds created key handle from RegCreateKey.
      
'   This creates a Root entry called "MyApp".
    sKeyName = ApplicationName
    sKeyValue = ApplicationName
    ret = RegCreateKey(HKEY_CLASSES_ROOT, sKeyName, lphKey)
    ret = RegSetValue(lphKey, "", REG_SZ, sKeyValue, 0&)
      
'   This creates a Root entry for the extension to be associated with "MyApp".
    sKeyName = "." & Extension
    sKeyValue = ApplicationName
    ret = RegCreateKey(HKEY_CLASSES_ROOT, sKeyName, lphKey)
    ret = RegSetValue(lphKey, "", REG_SZ, sKeyValue, 0&)
      
'   This sets the command line for "MyApp".
    sKeyName = ApplicationName
    sKeyValue = Chr(34) & PathToExecute & Chr(34) & " %1"
    ret = RegCreateKey(HKEY_CLASSES_ROOT, sKeyName, lphKey)
    ret = RegSetValue(lphKey, "shell\open\command", REG_SZ, sKeyValue, MAX_PATH)

'   This sets the default icon
    sKeyName = ApplicationName
    sKeyValue = PathToExecute & ",0"
    ret = RegCreateKey(HKEY_CLASSES_ROOT, sKeyName, lphKey)
    ret = RegSetValue(lphKey, "DefaultIcon", REG_SZ, sKeyValue, MAX_PATH)
Exit Sub

AssociateErrorHandler:
    MsgBox "Associate Error " & Err.Number & ": " & Err.Description, vbCritical
End Sub

Public Sub DissociateFileExtension(Extension As String, PathToExecute As String, ApplicationName As String)
 
'   ApplicationName is what shows in the list (Registered file types) when you click
'   My Computer, View, Folder Options, File Types
 
'   Extension is three letters without the "."

    On Error GoTo DissociateErrorHandler
    
    Dim sKeyName As String   'Holds Key Name in registry.
    Dim sKeyValue As String
    Dim lphKey As Long       'Holds created key handle from RegCreateKey.
    Dim ret As Long          'Holds error status if any from API calls.

    Dim lpcbData As Long
    Dim lpType As Long
    Dim szValue As String

    ' Determine the size of data to be read
    sKeyName = ApplicationName
    sKeyValue = ApplicationName
    ret = RegOpenKeyEx(HKEY_CLASSES_ROOT, sKeyName, 0, KEY_ALL_ACCESS, lphKey)
    ret = RegQueryValueEx(lphKey, vbNullString, 0&, lpType, vbNullString, lpcbData)
    If ret <> ERROR_SUCCESS Then
        If (lphKey <> 0) Then
            ret = RegCloseKey(lphKey)
        End If
        Exit Sub
    End If

    szValue = String(lpcbData, 0)
    ret = RegQueryValueEx(lphKey, vbNullString, 0&, REG_SZ, szValue, lpcbData)
    If ret = ERROR_SUCCESS Then
        szValue = Left(szValue, lpcbData - 1)
    Else
        If (lphKey <> 0) Then
            ret = RegCloseKey(lphKey)
        End If
        Exit Sub
    End If
    
    If (lphKey <> 0) Then
        ret = RegCloseKey(lphKey)
    End If
    
    If (szValue = ApplicationName) Then
    '   This deletes the default icon
        sKeyName = ApplicationName & "\DefaultIcon"
        ret = RegOpenKeyEx(HKEY_CLASSES_ROOT, sKeyName, 0, KEY_ALL_ACCESS, lphKey)
        
        If lphKey <> 0 Then
            ret = RegDeleteValue(lphKey, "")
            ret = RegDeleteKey(lphKey, "")
            ret = RegCloseKey(lphKey)
        End If
    
    '   This deletes the command line for "MyApp".
        sKeyName = ApplicationName & "\shell\open\command"
        ret = RegOpenKeyEx(HKEY_CLASSES_ROOT, sKeyName, 0, KEY_ALL_ACCESS, lphKey)
        
        If lphKey <> 0 Then
            ret = RegDeleteValue(lphKey, "")
            ret = RegDeleteKey(lphKey, "")
            ret = RegCloseKey(lphKey)
        End If
        
    '   This deletes the command line for "MyApp".
        sKeyName = ApplicationName & "\shell\open"
        ret = RegOpenKeyEx(HKEY_CLASSES_ROOT, sKeyName, 0, KEY_ALL_ACCESS, lphKey)
        
        If lphKey <> 0 Then
            ret = RegDeleteValue(lphKey, "")
            ret = RegDeleteKey(lphKey, "")
            ret = RegCloseKey(lphKey)
        End If
        
    '   This deletes the command line for "MyApp".
        sKeyName = ApplicationName & "\shell"
        ret = RegOpenKeyEx(HKEY_CLASSES_ROOT, sKeyName, 0, KEY_ALL_ACCESS, lphKey)
        
        If lphKey <> 0 Then
            ret = RegDeleteValue(lphKey, "")
            ret = RegDeleteKey(lphKey, "")
            ret = RegCloseKey(lphKey)
        End If
        
    '   This deletes the command line for "MyApp".
        sKeyName = ApplicationName
        ret = RegOpenKeyEx(HKEY_CLASSES_ROOT, sKeyName, 0, KEY_ALL_ACCESS, lphKey)
        
        If lphKey <> 0 Then
            ret = RegDeleteValue(lphKey, "")
            ret = RegDeleteKey(lphKey, "")
            ret = RegCloseKey(lphKey)
        End If
    
    '   This deletes the Root entry for the extension to be associated with "MyApp".
        sKeyName = "." & Extension
        ret = RegOpenKeyEx(HKEY_CLASSES_ROOT, sKeyName, 0, KEY_ALL_ACCESS, lphKey)
        
        If lphKey <> 0 Then
            ret = RegDeleteValue(lphKey, "")
            ret = RegDeleteKey(lphKey, "")
            ret = RegCloseKey(lphKey)
        End If
    End If
Exit Sub

DissociateErrorHandler:
    MsgBox "Dissociate Error " & Err.Number & ": " & Err.Description, vbCritical
End Sub

Function GetCommandLine(sFile As String) As Boolean
    Dim i As Integer
    Dim CmdLine As String
    Dim CmdLen As Integer
    Dim PathLength As Integer
    Dim FileName As String
    Dim FileTitle As String
    CmdLine = Command()                          'Get command line arguments.
    CmdLen = Len(CmdLine)
    
    If (CmdLine <> "") Then
        GetCommandLine = True
        i = CmdLen
        Do While (PathLength = 0)
            PathLength = InStr(i, CmdLine, "\")
            i = i - 1
        Loop
        'FileName = CmdLine 'Mid(CmdLine, 1, PathLength) 'asa era inainte
        FileName = Mid(CmdLine, 2, PathLength - 1)
        'FileTitle = Mid(CmdLine, PathLength + 1) ', CmdLen - PathLength - 4)  'asa era inainte
        FileTitle = Mid(CmdLine, PathLength + 1, CmdLen - PathLength - 1)
    Else
        GetCommandLine = False
    End If
    sFile = FileName & FileTitle
End Function
