Attribute VB_Name = "modDeclarations"
Option Explicit
Public Const VersionNumber As String = "1.1.4"
Public Const PI As Double = 3.14159265358979
Public Const NO_OPERATIONS As Integer = 60
Public Const NO_OPERATIONS_SOUDURE = 8
Public Project As colProject
Public ActiveProject As Integer
Public Enum Side
    SideNone = 0
    SideLeft = 1
    SideRight = 2
End Enum
Public Enum YesNo
    no = 0
    yes = 1
End Enum
Public lProjectCount As Integer
Declare Function GetWindowsDirectory& Lib "kernel32" Alias _
"GetWindowsDirectoryA" (ByVal lpBuffer As String, ByVal nSize As Long)
Declare Function SendMessage Lib "user32" Alias "SendMessageA" (ByVal hwnd As Long, ByVal wMsg As Long, ByVal wParam As Long, lParam As Any) As Long
Declare Function ShellExecute Lib "shell32.dll" Alias "ShellExecuteA" (ByVal hwnd As Long, ByVal lpOperation As String, ByVal lpFile As String, ByVal lpParameters As String, ByVal lpDirectory As String, ByVal nShowCmd As Long) As Long
Declare Function BitBlt Lib "gdi32" (ByVal hDestDC As Long, ByVal x As Long, ByVal y As Long, ByVal nWidth As Long, ByVal nHeight As Long, ByVal hSrcDC As Long, ByVal xSrc As Long, ByVal ySrc As Long, ByVal dwRop As Long) As Long
Declare Function Polyline& Lib "gdi32" (ByVal hdc As Long, lpPoint As POINTAPI, ByVal nCount As Long)
Declare Function DrawTextEx& Lib "user32" Alias "DrawTextExA" (ByVal hdc As Long, ByVal lpsz As String, ByVal N As Long, lpRect As RECT, ByVal un As Long, lpDrawTextParams As DRAWTEXTPARAMS)
Declare Function DrawText& Lib "user32" Alias "DrawTextA" (ByVal hdc As Long, ByVal lpStr As String, ByVal nCount As Long, lpRect As RECT, ByVal wFormat As Long)
Declare Function Rectangle& Lib "gdi32" (ByVal hdc As Long, ByVal x1 As Long, ByVal y1 As Long, ByVal X2 As Long, ByVal y2 As Long)
Declare Function Arc& Lib "gdi32" (ByVal hdc As Long, ByVal x1 As Long, ByVal _
y1 As Long, ByVal X2 As Long, ByVal y2 As Long, ByVal X3 As Long, ByVal y3 As _
Long, ByVal X4 As Long, ByVal y4 As Long)
Declare Function FillRect& Lib "user32" (ByVal hdc As Long, lpRect As RECT, ByVal hBrush As Long)
Declare Function apiCreateSolidBrush& Lib "gdi32" Alias "CreateSolidBrush" (ByVal crColor As Long)
Declare Function CreateHatchBrush& Lib "gdi32" (ByVal nIndex As Long, ByVal crColor As Long)
Public Declare Function PaintRgn& Lib "gdi32" (ByVal hdc As Long, ByVal hRgn As Long)
Public Declare Function CreatePolygonRgn& Lib "gdi32" (lpPoint As POINTAPI, ByVal nCount As Long, ByVal nPolyFillMode As Long)
Public Declare Function SelectClipRgn& Lib "gdi32" (ByVal hdc As Long, ByVal hRgn As Long)
Public Declare Function PtInRegion& Lib "gdi32" (ByVal hRgn As Long, ByVal x As Long, ByVal y As Long)
Declare Function FillRgn& Lib "gdi32" (ByVal hdc As Long, ByVal hRgn As Long, ByVal hBrush As Long)
Public Declare Function DeleteObject& Lib "gdi32" (ByVal hObject As Long)
Declare Function GetCursorPos& Lib "user32" (lpPoint As POINTAPI)
Declare Function CreateDIBPatternBrush& Lib "gdi32" (ByVal hPackedDIB As Long, ByVal wUsage As Long)
Declare Function CombineRgn& Lib "gdi32" (ByVal hDestRgn As Long, ByVal _
hSrcRgn1 As Long, ByVal hSrcRgn2 As Long, ByVal nCombineMode As Long)
Public Declare Function GetMessage Lib "user32" Alias "GetMessageA" (lpMsg As MSG, ByVal hwnd As Long, ByVal wMsgFilterMin As Long, ByVal wMsgFilterMax As Long) As Long
Declare Function GetTextMetrics& Lib "gdi32" Alias "GetTextMetricsA" (ByVal hdc _
As Long, lpMetrics As TEXTMETRIC)
Declare Function SHGetFolderPath Lib "shfolder.dll" _
   Alias "SHGetFolderPathA" _
  (ByVal hwndOwner As Long, _
   ByVal nFolder As Long, _
   ByVal hToken As Long, _
   ByVal dwReserved As Long, _
   ByVal lpszPath As String) As Long
Declare Function lstrlenW Lib "kernel32" _
  (ByVal lpString As Long) As Long

Declare Function IntersectRect Lib "user32.dll" (lpDestRect _
As RECT, lpSrc1Rect As RECT, lpSrc2Rect As RECT) As Long
Declare Sub SHAddToRecentDocs Lib "shell32.dll" ( _
                    ByVal uFlags As Long, ByVal pv As String)
                    
Public Const SRCCOPY = &HCC0020    ' (DWORD) dest = source
Public Const SRCPAINT = &HEE0086   ' (DWORD) dest = source OR dest
Public Const SRCAND = &H8800C6     ' (DWORD) dest = source AND dest
Public Const SRCINVERT = &H660046  ' (DWORD) dest = source XOR dest
Public Const SRCERASE = &H440328   ' (DWORD) dest = source AND (NOT dest )
Public Const NOTSRCCOPY = &H330008 ' (DWORD) dest = (NOT source)
Public Const NOTSRCERASE = &H1100A6 ' (DWORD) dest = (NOT src) AND (NOT dest)
Public Const MERGECOPY = &HC000CA  ' (DWORD) dest = (source AND pattern)
Public Const MERGEPAINT = &HBB0226 ' (DWORD) dest = (NOT source) OR dest
Public Const PATCOPY = &HF00021    ' (DWORD) dest = pattern
Public Const PATPAINT = &HFB0A09   ' (DWORD) dest = (Not source) or pattern or dest
Public Const PATINVERT = &H5A0049  ' (DWORD) dest = pattern XOR dest
Public Const DSTINVERT = &H550009  ' (DWORD) dest = (NOT dest)
Public Const BLACKNESS = &H42&     ' (DWORD) dest = BLACK
Public Const WHITENESS = &HFF0062  ' (DWORD) dest = WHITE

Public Enum CSIDL_VALUES
    CSIDL_DESKTOP = &H0
    CSIDL_INTERNET = &H1
    CSIDL_PROGRAMS = &H2
    CSIDL_CONTROLS = &H3
    CSIDL_PRINTERS = &H4
    CSIDL_PERSONAL = &H5
    CSIDL_FAVORITES = &H6
    CSIDL_STARTUP = &H7
    CSIDL_RECENT = &H8
    CSIDL_SENDTO = &H9
    CSIDL_BITBUCKET = &HA
    CSIDL_STARTMENU = &HB
    CSIDL_MYDOCUMENTS = &HC
    CSIDL_MYMUSIC = &HD
    CSIDL_MYVIDEO = &HE
    CSIDL_DESKTOPDIRECTORY = &H10
    CSIDL_DRIVES = &H11
    CSIDL_NETWORK = &H12
    CSIDL_NETHOOD = &H13
    CSIDL_FONTS = &H14
    CSIDL_TEMPLATES = &H15
    CSIDL_COMMON_STARTMENU = &H16
    CSIDL_COMMON_PROGRAMS = &H17
    CSIDL_COMMON_STARTUP = &H18
    CSIDL_COMMON_DESKTOPDIRECTORY = &H19
    CSIDL_APPDATA = &H1A
    CSIDL_PRINTHOOD = &H1B
    CSIDL_LOCAL_APPDATA = &H1C
    CSIDL_ALTSTARTUP = &H1D
    CSIDL_COMMON_ALTSTARTUP = &H1E
    CSIDL_COMMON_FAVORITES = &H1F
    CSIDL_INTERNET_CACHE = &H20
    CSIDL_COOKIES = &H21
    CSIDL_HISTORY = &H22
    CSIDL_COMMON_APPDATA = &H23
    CSIDL_WINDOWS = &H24
    CSIDL_SYSTEM = &H25
    CSIDL_PROGRAM_FILES = &H26
    CSIDL_MYPICTURES = &H27
    CSIDL_PROFILE = &H28
    CSIDL_SYSTEMX86 = &H29
    CSIDL_PROGRAM_FILESX86 = &H2A
    CSIDL_PROGRAM_FILES_COMMON = &H2B
    CSIDL_PROGRAM_FILES_COMMONX86 = &H2C
    CSIDL_COMMON_TEMPLATES = &H2D
    CSIDL_COMMON_DOCUMENTS = &H2E
    CSIDL_COMMON_ADMINTOOLS = &H2F
    CSIDL_ADMINTOOLS = &H30
    CSIDL_CONNECTIONS = &H31
    CSIDL_COMMON_MUSIC = &H35
    CSIDL_COMMON_PICTURES = &H36
    CSIDL_COMMON_VIDEO = &H37
    CSIDL_RESOURCES = &H38
    CSIDL_RESOURCES_LOCALIZED = &H39
    CSIDL_COMMON_OEM_LINKS = &H3A
    CSIDL_CDBURN_AREA = &H3B
    CSIDL_COMPUTERSNEARME = &H3D
    CSIDL_FLAG_PER_USER_INIT = &H800
    CSIDL_FLAG_NO_ALIAS = &H1000
    CSIDL_FLAG_DONT_VERIFY = &H4000
    CSIDL_FLAG_CREATE = &H8000
    CSIDL_FLAG_MASK = &HFF00
End Enum
' Hatch Styles
Public Const HS_HORIZONTAL = 0
Public Const HS_FDIAGONAL = 3
Public Const HS_BDIAGONAL = 2
Public Const HS_VERTICAL = 1
Public Const HS_CROSS = 4
Public Const HS_DIAGCROSS = 5

Public Type POINTAPI
    y As Long
    z As Long
End Type

Public Type lines
    Points() As POINTAPI
End Type

Public Type RECT   '  16  Bytes
     Left As Long
     Top As Long
     right As Long
     bottom As Long
End Type

Type DRAWTEXTPARAMS  '  2Ø Bytes
        cbSize As Long
        iTabLength As Long
        iLeftMargin As Long
        iRightMargin As Long
        uiLengthDraw As Long
End Type

Public Type MSG
    hwnd As Long
    message As Long
    wParam As Long
    lParam As Long
    time As Long
    pt As POINTAPI
End Type

Type TEXTMETRIC   '  53 Bytes
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

'    Public Const DT_BOTTOM = 'DT_SINGLE must also be specified. Aligns text with the bottom of the formatting rectangle.
'    Public Const DT_CALCRECT =  'Calculates the formatting rectangle as follows: On multiline drawing, the bottom of the rectangle is extended as needed to hold the text. On single line drawing the right of the rectangle is extended. No text is drawn. The rectangle specified by the lpRect parameter is loaded with the calculated values.
'    Public Const DT_CENTER =   'Text is centered horizontally.
'    Public Const DT_EXPANDTABS =   'Tabs are expanded when text is drawn. The default tab spacing is eight characters; however, this may be changed using the DT_TABSTOP flag.
'    Public Const DT_EXTERNALLEADING =  'Use the external leading attribute of the current font when calculating the line height.
'    Public Const DT_LEFT = 0  'Text is left-aligned.
'    Public Const DT_NOCLIP =   'Draws without clipping to the specified rectangle.
'    Public Const DT_NOPREFIX =  'Normally, this function assumes that the & character indicates that the next character should be underlined. This flag turns off this behavior.
'    Public Const DT_RIGHT = 6   'Text is right-aligned.
'    Public Const DT_SINGLELINE =   'Draws a single line only.
'    Public Const DT_TABSTOP =  'Specifies the new tab spacing in the high eight bits of this integer.
'    Public Const DT_TOP =  'DT_SINGLE must also be specified. Aligns text with the top of the formatting rectangle.
'    Public Const DT_VCENTER =  'DT_SINGLE must also be specified. Aligns text at the center of the formatting rectangle.
'    Public Const DT_WORDBREAK =    'Performs word wrapping. Starts a new line whenever a word would exceed the rectangle boundary or a carriage return linefeed sequence is met. Has no effect if the TA_UPDATECP flag has been set using the SetTextAlign function.

Public Function dropme(CB As Object)
   Const CB_SHOWDROPDOWN = &H14F
   Dim Tmp As String
   Tmp = SendMessage(CB.hwnd, CB_SHOWDROPDOWN, 1, ByVal 0&)
End Function

Public Function ArcSin(x As Double) As Double
On Error Resume Next
    'grade
    ArcSin = Atn(x / Sqr(-x * x + 1)) * 180 / PI
End Function
