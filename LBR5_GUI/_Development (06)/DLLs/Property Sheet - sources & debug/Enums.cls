VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "TEnums"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
' *******************************************************
' Routine Name : TEnums.cls
' Written By   : Marclei V Silva (MVS)
' Programmer   : Marclei V Silva (MVS) [Spnorte Consultoria de Informática]
' Date Writen  : 06/16/2000 -- 08:40:24
' Inputs       : N/A
' Outputs      : N/A
' Description  : Several Enumerations and constants for
'              : the project
'              :
'              :
' *******************************************************
Option Explicit

Public Enum clColorEnum
    clAliceBlue = &HFFF8F0
    clAntiqueWhite = &HD7EBFA
    clAqua = &HFFFF00
    clAquamarine = &HD4FF7F
    clAzure = &HFFFFF0
    clBeige = &HDCF5F5
    clBisque = &HC4E4FF
    clBlack = &H0
    clBlanchedalmond = &HCDEBFF
    clBlue = &HFF0000
    clBlueViolet = &HE22B8A
    clBrown = &H2A2AA5
    clBurlyWood = &H87B8DE
    clCadetBlue = &HA09E5F
    clChartreuse = &HFF7F
    clChocolate = &H1E69D2
    clCoral = &H507FFF
    clCornflower = &HED9564
    clCornsilk = &HDCF8FF
    clCrimson = &H3C14DC
    clCyan = &HFFFF00
    clDarkblue = &H8B0000
    clDarkcyan = &H8B8B00
    clDarkgoldenrod = &HB86B8
    clDarkgray = &HA9A9A9
    clDarkgreen = &H6400
    clDarkkhaki = &H6BB7BD
    clDarkmagenta = &H8B008B
    clDarkolivegreen = &H2F6B55
    clDarkorange = &H8CFF
    clDarkorchid = &HCC3299
    clDarkred = &H8B
    clDarksalmon = &H7A96E9
    clDarkseagreen = &H8BBC8F
    clDarkslateblue = &H8B3D48
    clDarkslategray = &H4F4F2F
    clDarkturquoise = &HD1CE00
    clDarkviolet = &HD30094
    clDeeppink = &H9314FF
    clDeepskyblue = &HFFBF00
    clDimgray = &H696969
    clDodgerblue = &HFF901E
    clFirebrick = &H2222B2
    clFloralwhite = &HF0FAFF
    clForestgreen = &H228B22
    clFuchsia = &HFF00FF
    clGainsboro = &HDCDCDC
    clGhostwhite = &HFFF8F8
    clGold = &HD7FF
    clGoldenrod = &H20A5DA
    clGray = &H808080
    clGreen = &H8000
    clGreenyellow = &H2FFFAD
    clHoneydew = &HF0FFF0
    clHotpink = &HB469FF
    clIndianred = &H5C5CCD
    clIndigo = &H82004B
    clIvory = &HF0FFFF
    clKhaki = &H8CE6F0
    clLavender = &HFAE6E6
    clLavenderblush = &HF5F0FF
    clLawngreen = &HFC7C
    clLemonchiffon = &HCDFAFF
    clLightblue = &HE6D8AD
    clLightcoral = &H8080F0
    clLightcyan = &HFFFFE0
    clLightgoldenrodyellow = &HD2FAFA
    clLightgreen = &H90EE90
    clLightgrey = &HD3D3D3
    clLightpink = &HC1B6FF
    clLightsalmon = &H7AA0FF
    clLightseagreen = &HAAB220
    clLightskyblue = &HFACE87
    clLightslategray = &H998877
    clLightsteelblue = &HDEC4B0
    clLightyellow = &HE0FFFF
    clLime = &HFF00
    clLimegreen = &H32CD32
    clLinen = &HE6F0FA
    clMagenta = &HFF00FF
    clMaroon = &H80
    clMediumaquamarine = &HAACD66
    clMediumblue = &HCD0000
    clMediumorchid = &HD355BA
    clMediumpurple = &HDB7093
    clMediumseagreen = &H71B33C
    clMediumslateblue = &HEE687B
    clMediumspringgreen = &H9AFA00
    clMediumturquoise = &HCCD148
    clMediumvioletred = &H8515C7
    clMidnightblue = &H701919
    clMintcream = &HFAFFF5
    clMistyrose = &HE1E4FF
    clMoccasin = &HB5E4FF
    clNavajowhite = &HADDEFF
    clNavy = &H800000
    clOldlace = &HE6F5FD
    clOlive = &H8080
    clOlivedrab = &H238E6B
    clOrange = &HA5FF
    clOrangered = &H45FF
    clOrchid = &HD670DA
    clPalegoldenrod = &HAAE8EE
    clPalegreen = &H98FB98
    clPaleturquoise = &HEEEEAF
    clPalevioletred = &H9370DB
    clPapayawhip = &HD5EFFF
    clPeachpuff = &HB9DAFF
    clPeru = &H3F85CD
    clPink = &HCBC0FF
    clPlum = &HDDA0DD
    clPowderblue = &HE6E0B0
    clPurple = &H800080
    clRed = &HFF
    clRosybrown = &H8F8FBC
    clRoyalblue = &HE16941
    clSaddlebrown = &H13458B
    clSalmon = &H7280FA
    clSandybrown = &H60A4F4
    clSeagreen = &H578B2E
    clSeashell = &HEEF5FF
    clSienna = &H2D52A0
    clSilver = &HC0C0C0
    clSkyblue = &HEBCE87
    clSlateblue = &HCD5A6A
    clSlategray = &H908070
    clSnow = &HFAFAFF
    clSpringgreen = &H7FFF00
    clSteelblue = &HB48246
    clTan = &H8CB4D2
    clTeal = &H808000
    clThistle = &HD8BFD8
    clTomato = &H4763FF
    clTurquoise = &HD0E040
    clViolet = &HEE82EE
    clWheat = &HB3DEF5
    clWhite = &HFFFFFF
    clWhitesmoke = &HF5F5F5
    clYellow = &HFFFF
    clYellowgreen = &H32CD9A
End Enum

' PropertySheet enumarations, constants and definitions
Public Enum psAppearanceSettings
    psFlat = 0
    ps3D = 1
End Enum

Public Enum psBorderStyle
    psBorderNone = 0
    psBorderSingle = 1
End Enum

Public Enum psMessages
    PLM_GETTOOLTIP = 4096 ' Returns the tooltip window handle
    PLM_GETEDITWND = 4097 ' Returns the edit window handle.
End Enum

'Property types.
Public Enum psPropertyType
    psNone = -1
    psCustom = 0
    psInteger = 2
    psLong = 3
    psSingle = 4
    psDouble = 5
    psCurrency = 6
    psDate = 7
    psString = 8
    psObject = 9
    psBoolean = 11
    psDecimal = 14
    psByte = 17
    psFont = 240
    psPicture = 241
    psFile = 242
    psColor = 243
    psDropDownList = 244  ' Like ComboBox DropDown List style
    psCombo = 245         ' Like ComboBox DropDown Combo style
    psTime = 246
    psLongText = 247      ' Long text like memo text
    psFolder = 248
    psDropDownCheckList = 249
End Enum

Public Enum psGridLinesSettings
    psGridNone = 0    ' There are no lines between cells. In the MSFlexGrid, this is the default for GridLines.
    psGridFlat = 1    ' The line style between cells is set to normal, flat, lines. In the MSHFlexGrid, this is the default for GridLines.
    psGridInset = 2   ' The line style between cells is set to inset lines. In the MSFlexGrid, this is the default for GridLinesFixed.
    psGridRaised = 3  ' The line style between cells is set to raised lines. In the MSHFlexGrid, this is the default for GridLinesFixed.
End Enum

Public Enum psEffectStyle
    psNormal = 0
    psSmooth = 1
End Enum
'-- end code
