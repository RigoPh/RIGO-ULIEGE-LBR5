Attribute VB_Name = "modImageList"
Option Explicit

Public Declare Function Rectangle Lib "gdi32" (ByVal hdc As Long, ByVal X1 As Long, ByVal Y1 As Long, ByVal X2 As Long, ByVal Y2 As Long) As Long
Public Declare Function CreateDCAsNull Lib "gdi32" Alias "CreateDCA" ( _
   ByVal lpDriverName As String, lpDeviceName As Any, lpOutput As Any, lpInitData As Any) As Long
Public Declare Function SetROP2 Lib "gdi32" (ByVal hdc As Long, ByVal nDrawMode As Long) As Long
     Public Const R2_BLACK = 1 ' 0
     Public Const R2_COPYPEN = 13 ' P
     Public Const R2_LAST = 16
     Public Const R2_MASKNOTPEN = 3 ' DPna
     Public Const R2_MASKPEN = 9 ' DPa
     Public Const R2_MASKPENNOT = 5 ' PDna
     Public Const R2_MERGENOTPEN = 12    ' DPno
     Public Const R2_MERGEPEN = 15 ' DPo
     Public Const R2_MERGEPENNOT = 14    ' PDno
     Public Const R2_NOP = 11    ' D
     Public Const R2_NOT = 6 ' Dn
     Public Const R2_NOTCOPYPEN = 4 ' PN
     Public Const R2_NOTMASKPEN = 8 ' DPan
     Public Const R2_NOTMERGEPEN = 2 ' DPon
     Public Const R2_NOTXORPEN = 10 ' DPxn
     Public Const R2_WHITE = 16 ' 1
     Public Const R2_XORPEN = 7 ' DPx
Public Declare Function DeleteDC Lib "gdi32" (ByVal hdc As Long) As Long

' Colour functions:
Declare Function SetTextColor Lib "gdi32" (ByVal hdc As Long, ByVal crColor As Long) As Long
Declare Function SetBkColor Lib "gdi32" (ByVal hdc As Long, ByVal crColor As Long) As Long
Declare Function SetBkMode Lib "gdi32" (ByVal hdc As Long, ByVal nBkMode As Long) As Long
    Public Const OPAQUE = 2
    Public Const TRANSPARENT = 1
Declare Function GetSysColor Lib "user32" (ByVal nIndex As Long) As Long
    Public Const COLOR_ACTIVEBORDER = 10
    Public Const COLOR_ACTIVECAPTION = 2
    Public Const COLOR_ADJ_MAX = 100
    Public Const COLOR_ADJ_MIN = -100
    Public Const COLOR_APPWORKSPACE = 12
    Public Const COLOR_BACKGROUND = 1
    Public Const COLOR_BTNFACE = 15
    Public Const COLOR_BTNHIGHLIGHT = 20
    Public Const COLOR_BTNSHADOW = 16
    Public Const COLOR_BTNTEXT = 18
    Public Const COLOR_CAPTIONTEXT = 9
    Public Const COLOR_GRAYTEXT = 17
    Public Const COLOR_HIGHLIGHT = 13
    Public Const COLOR_HIGHLIGHTTEXT = 14
    Public Const COLOR_INACTIVEBORDER = 11
    Public Const COLOR_INACTIVECAPTION = 3
    Public Const COLOR_INACTIVECAPTIONTEXT = 19
    Public Const COLOR_MENU = 4
    Public Const COLOR_MENUTEXT = 7
    Public Const COLOR_SCROLLBAR = 0
    Public Const COLOR_WINDOW = 5
    Public Const COLOR_WINDOWFRAME = 6
    Public Const COLOR_WINDOWTEXT = 8
    Public Const COLORONCOLOR = 3

Declare Function OleTranslateColor Lib "olepro32.dll" (ByVal OLE_COLOR As Long, ByVal HPALETTE As Long, pccolorref As Long) As Long
Public Const CLR_INVALID = -1

Public Type RECT
    Left As Long
    Top As Long
    Right As Long
    Bottom As Long
End Type

' Image list functions:
Public Declare Function ImageList_GetBkColor Lib "COMCTL32" (ByVal hImageList As Long) As Long
Public Declare Function ImageList_ReplaceIcon Lib "COMCTL32" (ByVal hImageList As Long, ByVal i As Long, ByVal hIcon As Long) As Long
Public Declare Function ImageList_Convert Lib "COMCTL32" Alias "ImageList_Draw" (ByVal hImageList As Long, ByVal ImgIndex As Long, ByVal hDCDest As Long, ByVal x As Long, ByVal y As Long, ByVal flags As Long) As Long
Public Declare Function ImageList_Create Lib "COMCTL32" (ByVal MinCx As Long, ByVal MinCy As Long, ByVal flags As Long, ByVal cInitial As Long, ByVal cGrow As Long) As Long
Public Declare Function ImageList_AddMasked Lib "COMCTL32" (ByVal hImageList As Long, ByVal hbmImage As Long, ByVal crMask As Long) As Long
Public Declare Function ImageList_Replace Lib "COMCTL32" (ByVal hImageList As Long, ByVal ImgIndex As Long, ByVal hbmImage As Long, ByVal hBmMask As Long) As Long
Public Declare Function ImageList_Add Lib "COMCTL32" (ByVal hImageList As Long, ByVal hbmImage As Long, hBmMask As Long) As Long
Public Declare Function ImageList_Remove Lib "COMCTL32" (ByVal hImageList As Long, ByVal ImgIndex As Long) As Long
Public Type IMAGEINFO
    hBitmapImage As Long
    hBitmapMask As Long
    cPlanes As Long
    cBitsPerPixel As Long
    rcImage As RECT
End Type
Public Declare Function ImageList_GetImageInfo Lib "COMCTL32.DLL" ( _
        ByVal hIml As Long, _
        ByVal i As Long, _
        pImageInfo As IMAGEINFO _
    ) As Long
Public Declare Function ImageList_AddIcon Lib "COMCTL32" (ByVal hIml As Long, ByVal hIcon As Long) As Long
Public Declare Function ImageList_GetIcon Lib "COMCTL32" (ByVal hImageList As Long, ByVal ImgIndex As Long, ByVal fuFlags As Long) As Long
Public Declare Function ImageList_SetImageCount Lib "COMCTL32" (ByVal hImageList As Long, uNewCount As Long)
Public Declare Function ImageList_GetImageCount Lib "COMCTL32" (ByVal hImageList As Long) As Long
Public Declare Function ImageList_Destroy Lib "COMCTL32" (ByVal hImageList As Long) As Long
Public Declare Function ImageList_GetIconSize Lib "COMCTL32" (ByVal hImageList As Long, cx As Long, cy As Long) As Long
Public Declare Function ImageList_SetIconSize Lib "COMCTL32" (ByVal hImageList As Long, cx As Long, cy As Long) As Long

' ImageList functions:
' Draw:
Public Declare Function ImageList_Draw Lib "COMCTL32.DLL" ( _
        ByVal hIml As Long, _
        ByVal i As Long, _
        ByVal hdcDst As Long, _
        ByVal x As Long, _
        ByVal y As Long, _
        ByVal fStyle As Long _
    ) As Long
Public Const ILD_NORMAL = 0&
Public Const ILD_TRANSPARENT = 1&
Public Const ILD_BLEND25 = 2&
Public Const ILD_SELECTED = 4&
Public Const ILD_FOCUS = 4&
Public Const ILD_MASK = &H10&
Public Const ILD_IMAGE = &H20&
Public Const ILD_ROP = &H40&
Public Const ILD_OVERLAYMASK = 3840&
Public Declare Function ImageList_GetImageRect Lib "COMCTL32.DLL" ( _
        ByVal hIml As Long, _
        ByVal i As Long, _
        prcImage As RECT _
    ) As Long
' Messages:
Public Declare Function ImageList_DrawEx Lib "COMCTL32" (ByVal hIml As Long, ByVal i As Long, ByVal hdcDst As Long, ByVal x As Long, ByVal y As Long, ByVal dx As Long, ByVal dy As Long, ByVal rgbBk As Long, ByVal rgbFg As Long, ByVal fStyle As Long) As Long
Public Declare Function ImageList_LoadImage Lib "COMCTL32" Alias "ImageList_LoadImageA" (ByVal hInst As Long, ByVal lpbmp As String, ByVal cx As Long, ByVal cGrow As Long, ByVal crMask As Long, ByVal uType As Long, ByVal uFlags As Long)
Public Declare Function ImageList_SetBkColor Lib "COMCTL32" (ByVal hImageList As Long, ByVal clrBk As Long) As Long

Public Const ILC_MASK = &H1&
 
Public Const CLR_DEFAULT = -16777216
Public Const CLR_HILIGHT = -16777216
Public Const CLR_NONE = -1

Public Const ILCF_MOVE = &H0&
Public Const ILCF_SWAP = &H1&
Public Declare Function ImageList_Copy Lib "COMCTL32" (ByVal himlDst As Long, ByVal iDst As Long, ByVal himlSrc As Long, ByVal iSrc As Long, ByVal uFlags As Long) As Long

Private Declare Function GetTempFileName Lib "kernel32" Alias "GetTempFileNameA" (ByVal lpszPath As String, ByVal lpPrefixString As String, ByVal wUnique As Long, ByVal lpTempFileName As String) As Long
Private Declare Function GetTempPath Lib "kernel32" Alias "GetTempPathA" (ByVal nBufferLength As Long, ByVal lpBuffer As String) As Long
Private Const MAX_PATH = 260
Private Declare Function lstrlen Lib "kernel32" Alias "lstrlenA" (ByVal lpString As String) As Long

Private Type PictDesc
    cbSizeofStruct As Long
    picType As Long
    hImage As Long
    xExt As Long
    yExt As Long
End Type
Private Type Guid
    Data1 As Long
    Data2 As Integer
    Data3 As Integer
    Data4(0 To 7) As Byte
End Type
Private Declare Function OleCreatePictureIndirect Lib "olepro32.dll" (lpPictDesc As PictDesc, riid As Guid, ByVal fPictureOwnsHandle As Long, ipic As IPicture) As Long
Public Declare Function GetDC Lib "user32" (ByVal hwnd As Long) As Long

Function TranslateColor(ByVal clr As OLE_COLOR, _
                        Optional hpal As Long = 0) As Long
    If OleTranslateColor(clr, hpal, TranslateColor) Then
        TranslateColor = CLR_INVALID
    End If
End Function

