Attribute VB_Name = "modMain"
' *******************************************************
' Module       : modMain.bas
' Written By   : Marclei V Silva (MVS)
' Programmer   : Marclei V Silva (MVS) [Spnorte Consultoria de Informática]
' Date Writen  : 06/16/2000 -- 08:43:12
' Inputs       : N/A
' Outputs      : N/A
' Description  : This module contains several
'              : constants definitions, basic
'              : routines and API declares
' Called By    :
' *******************************************************
Option Explicit

' Keep up with the errors
Const g_ErrConstant As Long = vbObjectError + 1000
Const m_constClassName = "modMain"

'Public Const CLR_INVALID = -1

Private m_lngErrNum As Long
Private m_strErrStr As String
Private m_strErrSource As String

Public Const PROJECT_STR = "PROJECT"
Public Const FOLDER_STR = "FOLDER"
Public Const ITEM_STR = "ITEM"

' several constants definitions
    '
    ' Strip the path from the file name, and just return the FileName
    ' Wraps the SeparatePathAndFileName from DWTools
    '
Global Const CB_ERR = -1
Global Const CB_FINDSTRING = &H14C

Public Const SM_CXSIZE = 30
Public Const SM_CXBORDER = 5
Public Const SM_CXFRAME = 32
Public Const SM_CYFRAME = 33
Public Const SM_CYSIZE = 31
Public Const SM_CYCAPTION = 4
Public Const SM_CYBORDER = 6
Public Const SM_CYMENU = 15
Public Const SM_CXVSCROLL = 2

Public Const WS_VSCROLL = &H200000
Public Const GWL_STYLE = (-16)
Public Const LB_FINDSTRING = &H18F

' Set TopMost Constants
Public Const HWND_TOPMOST = -1
Public Const HWND_NOTOPMOST = -2
Public Const SWP_NOMOVE = &H2
Public Const SWP_NOSIZE = &H1
Public Const TOPMOST_Flags = SWP_NOMOVE Or SWP_NOSIZE

' Api declares
Public Declare Function SendMessage Lib "user32" Alias "SendMessageA" (ByVal hwnd As Long, ByVal wMsg As Long, ByVal wParam As Long, lParam As Any) As Long
Public Declare Function SetParent Lib "user32" (ByVal hWndChild As Long, ByVal hWndNewParent As Long) As Long
Public Declare Function GetSystemMetrics Lib "user32" (ByVal nIndex As Integer) As Integer
Public Declare Function SetWindowPos Lib "user32" (ByVal hwnd As Long, ByVal hWndInsertAfter As Long, ByVal x As Long, y, ByVal cx As Long, ByVal cy As Long, ByVal wFlags As Long) As Long

' ******************************************************************************
' Routine       : (Function) FindComboStr
' Created by    : Marclei V Silva
' Company Name  : Spnorte Consultoria
' Machine       : ZEUS
' Date-Time     : 16/06/2000 - 8:49:20
' Inputs        : hWnd : Long
'               : sSearchText : String
' Outputs       : Numeric (long)
' Modifications : N/A
' Description   : Find a string in a combo box
' ******************************************************************************
Public Function FindComboStr(hwnd As Long, sSearchText As String) As Long
    Dim lReturn As Long

    lReturn = SendMessage(hwnd, CB_FINDSTRING, -1, ByVal sSearchText)
    If lReturn <> CB_ERR Then
        FindComboStr = lReturn
    Else
        FindComboStr = lReturn
    End If
End Function

' ******************************************************************************
' Routine       : (Function) IsVarEmpty
' Created by    : Marclei V Silva
' Company Name  : Spnorte Consultoria
' Machine       : ZEUS
' Date-Time     : 16/06/2000 - 8:48:07
' Inputs        : varName : Variant
' Outputs       : True/False
' Modifications : N/A
' Description   : Check for a empty value
' ******************************************************************************
Public Function IsVarEmpty(varName As Variant) As Boolean
    If IsObject(varName) Then
        IsVarEmpty = varName Is Nothing
    ElseIf IsArray(varName) Then
        IsVarEmpty = (UBound(varName) = 0)
    Else
        If IsNull(varName) Or IsEmpty(varName) Then
            IsVarEmpty = True
        ElseIf varName = "" Then
            IsVarEmpty = True
        Else
            IsVarEmpty = False
        End If
    End If
End Function

' ******************************************************************************
' Routine       : (Sub) StayOnTop
' Created by    : Marclei V Silva
' Company Name  : Spnorte Consultoria
' Machine       : ZEUS
' Date-Time     : 16/06/2000 - 8:48:44
' Inputs        : hWndA : Long
' Outputs       : N/A
' Modifications : N/A
' Description   : Sets a specific window handler to top
' ******************************************************************************
Public Sub StayOnTop(hWndA As Long)
    SetWindowPos hWndA, HWND_TOPMOST, 0, 0, 0, 0, TOPMOST_Flags
End Sub

Public Function ZHex(lHex As Long, iZeros As Integer) As String
'returns a HEX string of specified length (pad zeros on left)
    ZHex = Right$(String$(iZeros - 1, "0") & Hex$(lHex), iZeros)
End Function

Public Function MakeHex(R As Long, g As Long, b As Long) As String
    MakeHex = ZHex(R, 2) & ZHex(g, 2) & ZHex(b, 2)
End Function

Public Function RGBRed(RGBCol As Long) As Integer
'Return the Red component from an RGB Color
    RGBRed = RGBCol And &HFF
End Function

Public Function RGBGreen(RGBCol As Long) As Integer
'Return the Green component from an RGB Color
    RGBGreen = ((RGBCol And &H100FF00) / &H100)
End Function

Public Function RGBBlue(RGBCol As Long) As Integer
'Return the Blue component from an RGB Color
    RGBBlue = (RGBCol And &HFF0000) / &H10000
End Function

Private Function iMax(a As Integer, b As Integer) _
    As Integer
'Return the Larger of two values
    iMax = IIf(a > b, a, b)
End Function

Private Function iMin(a As Integer, b As Integer) _
    As Integer
'Return the smaller of two values
    iMin = IIf(a < b, a, b)
End Function

Public Function RGBToVB(RGBCol As Long) As String
    RGBToVB = "&H" & Hex$(RGBCol) & "H&"
End Function

Public Function RGBToHTML(RGBCol As Long) As String
    Dim R As Long
    Dim g As Long
    Dim b As Long
    
    R = RGBRed(RGBCol)
    g = RGBGreen(RGBCol)
    b = RGBBlue(RGBCol)
    RGBToHTML = MakeHex(R, g, b)
End Function

Function ObjectExists(pColl As Object, sMemName) As Boolean
    Dim pObj As Object
  
    On Error Resume Next
    Err = 0
    Set pObj = pColl(sMemName)
    ObjectExists = (Err = 0)
End Function

Function ConvertValue(Value As Variant, ValueType As psPropertyType) As Variant
On Error GoTo Err_ConvertValue
    Const constSource As String = m_constClassName & ".ConvertValue"
    
    Select Case ValueType
        Case psInteger
            ConvertValue = CInt(Val(Value))
        Case psLong
            ConvertValue = CLng(Val(Value))
        Case psSingle
            ConvertValue = CSng(Val(Value))
        Case psDouble
            ConvertValue = CDbl(Val(Value))
        Case psCurrency
            ConvertValue = CCur(Val(Value))
        Case psDate
            ConvertValue = CDate(Value)
        Case psString
            ConvertValue = CStr(Value)
        Case psBoolean
            ConvertValue = CBool(Value)
        Case psDecimal
            ConvertValue = Format(Val(Value), "##,##0.00")
        Case psByte
            ConvertValue = CByte(Val(Value))
        Case Else
            ConvertValue = Value
    End Select

Exit Function
Err_ConvertValue:
    ConvertValue = Null
End Function
'-- end code

Function StripBkLinefeed(ByVal Text As String) As String
    Dim NewValue As Variant
    NewValue = Text
    If Right(NewValue, 2) = vbCrLf Then
        NewValue = Left(NewValue, Len(NewValue) - 2)
    End If
    StripBkLinefeed = NewValue
End Function

Function FormatColor(ByVal lValue As Long, ByVal sFormat As String) As String
    
    If sFormat = "CustomDisplay" Then
        FormatColor = CStr(lValue)
        Exit Function
    End If
    
    Dim sDisplayStr As String
    Dim sTemp As String
    Dim i As Integer
    
    sDisplayStr = ""
    For i = 1 To Len(sFormat)
        sTemp = Mid(sFormat, i, 1)
        Select Case sTemp
        Case "e"
            sDisplayStr = sDisplayStr & Hex$(lValue)
        Case "m"
            sDisplayStr = sDisplayStr & RGBToHTML(lValue)
        Case "r"
            sDisplayStr = sDisplayStr & RGBRed(lValue)
        Case "g"
            sDisplayStr = sDisplayStr & RGBGreen(lValue)
        Case "b"
            sDisplayStr = sDisplayStr & RGBBlue(lValue)
        Case Else
            sDisplayStr = sDisplayStr & sTemp
        End Select
    Next
    FormatColor = sDisplayStr
End Function

Function FormatFont(ByVal ObjFont As StdFont, ByVal sFormat As String) As String
    
    If sFormat = "CustomDisplay" Then
        FormatFont = ObjFont.Name
        Exit Function
    End If
    
    Dim sDisplayStr As String
    Dim sTemp As String
    Dim i As Integer
    
    sDisplayStr = ""
    For i = 1 To Len(sFormat)
        sTemp = Mid(sFormat, i, 1)
        Select Case sTemp
        Case "n"
            sDisplayStr = sDisplayStr & Round(ObjFont.Size, 0)
        Case "c"
            sDisplayStr = sDisplayStr & ObjFont.Name
        Case "b"
            If ObjFont.Bold = True Then
                sDisplayStr = sDisplayStr & "bold"
            End If
        Case "i"
            If ObjFont.Italic = True Then
                sDisplayStr = sDisplayStr & "italic"
            End If
        Case "u"
            If ObjFont.Underline = True Then
                sDisplayStr = sDisplayStr & "underline"
            End If
        Case Else
            sDisplayStr = sDisplayStr & sTemp
        End Select
    Next
    FormatFont = sDisplayStr
End Function

' ******************************************************************************
' Routine       : HiByte
' Created by    : Marclei V Silva
' Machine       : ZEUS
' Date-Time     : 20/03/01 7:21:50
' Description   : Gets the high byte from a integer value
' Inputs        :
' Outputs       :
' Credits       :
' Modifications :
' Remarks       :
' ******************************************************************************
Function HiByte(ByVal w As Integer) As Byte
    If w And &H8000 Then
        HiByte = &H80 Or ((w And &H7FFF) \ &HFF)
    Else
        HiByte = w \ 256
    End If
End Function

' ******************************************************************************
' Routine       : LoByte
' Created by    : Marclei V Silva
' Machine       : ZEUS
' Date-Time     : 20/03/01 7:22:11
' Description   : Get the low byte from an integer value
' Inputs        :
' Outputs       :
' Credits       :
' Modifications :
' Remarks       :
' ******************************************************************************
Function LoByte(w As Integer) As Byte
    LoByte = w And &HFF
End Function

' ******************************************************************************
' Routine       : MakeWord
' Created by    : Marclei V Silva
' Machine       : ZEUS
' Date-Time     : 20/03/01 7:22:27
' Inputs        : wHi : Integer
'               : wLo : Integer
' Outputs       : Long
' Modifications : N/A
' Description   : Sets the high and low byte into a integer value
' ******************************************************************************
Function MakeWord(ByVal bHi As Byte, ByVal bLo As Byte) As Integer
    If bHi And &H80 Then
        MakeWord = (((bHi And &H7F) * 256) + bLo) Or &H8000
    Else
        MakeWord = (bHi * 256) + bLo
    End If
End Function

' ******************************************************************************
' Routine       : (Function) MakeDWord
' Created by    : Marclei V Silva
' Company Name  : Spnorte Consultoria
' Machine       : ZEUS
' Date-Time     : 16/06/2000 - 8:46:18
' Inputs        : wHi : Integer
'               : wLo : Integer
' Outputs       : Long
' Modifications : N/A
' Description   : Sets the high and low byte into a long value
' ******************************************************************************
Function MakeDWord(ByVal wHi As Integer, _
       ByVal wLo As Integer) As Long
    If wHi And &H8000 Then
        MakeDWord = (((wHi And &H7FFF) * 65536) Or _
           (wLo And &HFFFF)) Or &H80000000
        Else: MakeDWord = (wHi * 65536) + wLo
    End If
End Function

' ******************************************************************************
' Routine       : (Function) HiWord
' Created by    : Marclei V Silva
' Company Name  : Spnorte Consultoria
' Machine       : ZEUS
' Date-Time     : 16/06/2000 - 8:45:32
' Inputs        : dw : Long
' Outputs       : Integer
' Modifications : N/A
' Description   : Retrieves the High byte (word)
' ******************************************************************************
Function HiWord(ByVal dw As Long) As Integer
    If dw And &H80000000 Then
        HiWord = (dw \ 65535) - 1
        Else: HiWord = dw \ 65535
    End If
End Function

' ******************************************************************************
' Routine       : (Function) LoWord
' Created by    : Marclei V Silva
' Company Name  : Spnorte Consultoria
' Machine       : ZEUS
' Date-Time     : 16/06/2000 - 8:44:48
' Inputs        : dw : Long
' Outputs       : Integer
' Modifications : N/A
' Description   : Retrieve Low byte (word)
' ******************************************************************************
Function LoWord(ByVal dw As Long) As Integer
    If dw And &H8000& Then
        LoWord = &H8000 Or (dw And &H7FFF&)
        Else: LoWord = dw And &HFFFF&
    End If
End Function

Public Function GetDefaultValue(ValueType As psPropertyType) As Variant
    Select Case ValueType
    Case psInteger: GetDefaultValue = 0
    Case psLong: GetDefaultValue = 0
    Case psSingle: GetDefaultValue = 0
    Case psDouble: GetDefaultValue = 0
    Case psCurrency: GetDefaultValue = 0
    Case psDate: GetDefaultValue = Date
    Case psString: GetDefaultValue = ""
    Case psObject: GetDefaultValue = Nothing
    Case psBoolean: GetDefaultValue = False
    Case psDecimal: GetDefaultValue = 0
    Case psByte: GetDefaultValue = 0
    Case psFont: GetDefaultValue = Nothing
    Case psPicture: GetDefaultValue = Nothing
    Case psFile: GetDefaultValue = ""
    Case psColor: GetDefaultValue = 0
    Case psDropDownList: GetDefaultValue = 0
    Case psCombo: GetDefaultValue = 0
    Case psTime: GetDefaultValue = Time
    Case psLongText: GetDefaultValue = ""
    Case psFolder: GetDefaultValue = ""
    Case psDropDownCheckList: GetDefaultValue = ""
    End Select
End Function

Public Function GetValueType(Value As Variant) As psPropertyType
    Select Case VarType(Value)
        Case vbBoolean
            GetValueType = psBoolean
        Case vbLong
            GetValueType = psLong
        Case vbString
            GetValueType = psString
        Case vbDouble
            GetValueType = psDouble
        Case vbCurrency
            GetValueType = psCurrency
        Case vbString
            GetValueType = psString
        Case vbInteger
            GetValueType = psInteger
        Case vbSingle
            GetValueType = psSingle
        Case vbObject
            GetValueType = psObject
        Case Else
            GetValueType = psCustom
    End Select
End Function

