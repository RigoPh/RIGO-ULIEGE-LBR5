Attribute VB_Name = "modRegionalSettings"
Option Explicit

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' Copyright ©1996-2007 VBnet, Randy Birch, All Rights Reserved.
' Some pages may also contain other copyrights by the author.
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' Distribution: You can freely use this code in your own
'               applications, but you may not reproduce
'               or publish this code on any web site,
'               online service, or distribute as source
'               on any media without express permission.
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

''USE
'Dim LCID As Long
'LCID = GetSystemDefaultLCID()
''language id
'Text0.Text = GetUserLocaleInfo(LCID, LOCALE_ILANGUAGE)
   
Public Const LOCALE_ILANGUAGE             As Long = &H1     'language id
Public Const LOCALE_SLANGUAGE             As Long = &H2     'localized name of language
Public Const LOCALE_SENGLANGUAGE          As Long = &H1001  'English name of language
Public Const LOCALE_SABBREVLANGNAME       As Long = &H3     'abbreviated language name
Public Const LOCALE_SNATIVELANGNAME       As Long = &H4     'native name of language

Public Const LOCALE_ICOUNTRY              As Long = &H5     'country code
Public Const LOCALE_SCOUNTRY              As Long = &H6     'localized name of country
Public Const LOCALE_SENGCOUNTRY           As Long = &H1002  'English name of country
Public Const LOCALE_SABBREVCTRYNAME       As Long = &H7     'abbreviated country name
Public Const LOCALE_SNATIVECTRYNAME       As Long = &H8     'native name of country

Public Const LOCALE_IDEFAULTLANGUAGE      As Long = &H9     'default language id
Public Const LOCALE_IDEFAULTCOUNTRY       As Long = &HA     'default country code
Public Const LOCALE_IDEFAULTCODEPAGE      As Long = &HB     'default oem code page
Public Const LOCALE_IDEFAULTANSICODEPAGE  As Long = &H1004  'default ansi code page
Public Const LOCALE_IDEFAULTMACCODEPAGE   As Long = &H1011  'default mac code page

Public Const LOCALE_SLIST                 As Long = &HC     'list item separator
Public Const LOCALE_IMEASURE              As Long = &HD     '0 = metric, 1 = US

Public Const LOCALE_SDECIMAL              As Long = &HE     'decimal separator
Public Const LOCALE_STHOUSAND             As Long = &HF     'thousand separator
Public Const LOCALE_SGROUPING             As Long = &H10    'digit grouping
Public Const LOCALE_IDIGITS               As Long = &H11    'number of fractional digits
Public Const LOCALE_ILZERO                As Long = &H12    'leading zeros for decimal
Public Const LOCALE_INEGNUMBER            As Long = &H1010  'negative number mode
Public Const LOCALE_SNATIVEDIGITS         As Long = &H13    'native ascii 0-9

Public Const LOCALE_SCURRENCY             As Long = &H14    'local monetary symbol
Public Const LOCALE_SINTLSYMBOL           As Long = &H15    'intl monetary symbol
Public Const LOCALE_SMONDECIMALSEP        As Long = &H16    'monetary decimal separator
Public Const LOCALE_SMONTHOUSANDSEP       As Long = &H17    'monetary thousand separator
Public Const LOCALE_SMONGROUPING          As Long = &H18    'monetary grouping
Public Const LOCALE_ICURRDIGITS           As Long = &H19    '# local monetary digits
Public Const LOCALE_IINTLCURRDIGITS       As Long = &H1A    '# intl monetary digits
Public Const LOCALE_ICURRENCY             As Long = &H1B    'positive currency mode
Public Const LOCALE_INEGCURR              As Long = &H1C    'negative currency mode

Public Const LOCALE_SDATE                 As Long = &H1D    'date separator
Public Const LOCALE_STIME                 As Long = &H1E    'time separator
Public Const LOCALE_SSHORTDATE            As Long = &H1F    'short date format string
Public Const LOCALE_SLONGDATE             As Long = &H20    'long date format string
Public Const LOCALE_STIMEFORMAT           As Long = &H1003  'time format string
Public Const LOCALE_IDATE                 As Long = &H21    'short date format ordering
Public Const LOCALE_ILDATE                As Long = &H22    'long date format ordering
Public Const LOCALE_ITIME                 As Long = &H23    'time format specifier
Public Const LOCALE_ITIMEMARKPOSN         As Long = &H1005  'time marker position
Public Const LOCALE_ICENTURY              As Long = &H24    'century format specifier (short date)
Public Const LOCALE_ITLZERO               As Long = &H25    'leading zeros in time field
Public Const LOCALE_IDAYLZERO             As Long = &H26    'leading zeros in day field (short date)
Public Const LOCALE_IMONLZERO             As Long = &H27    'leading zeros in month field (short date)
Public Const LOCALE_S1159                 As Long = &H28    'AM designator
Public Const LOCALE_S2359                 As Long = &H29    'PM designator

Public Const LOCALE_ICALENDARTYPE         As Long = &H1009  'type of calendar specifier
Public Const LOCALE_IOPTIONALCALENDAR     As Long = &H100B  'additional calendar types specifier
Public Const LOCALE_IFIRSTDAYOFWEEK       As Long = &H100C  'first day of week specifier
Public Const LOCALE_IFIRSTWEEKOFYEAR      As Long = &H100D  'first week of year specifier

Public Const LOCALE_SDAYNAME1             As Long = &H2A    'long name for Monday
Public Const LOCALE_SDAYNAME2             As Long = &H2B    'long name for Tuesday
Public Const LOCALE_SDAYNAME3             As Long = &H2C    'long name for Wednesday
Public Const LOCALE_SDAYNAME4             As Long = &H2D    'long name for Thursday
Public Const LOCALE_SDAYNAME5             As Long = &H2E    'long name for Friday
Public Const LOCALE_SDAYNAME6             As Long = &H2F    'long name for Saturday
Public Const LOCALE_SDAYNAME7             As Long = &H30    'long name for Sunday
Public Const LOCALE_SABBREVDAYNAME1       As Long = &H31    'abbreviated name for Monday
Public Const LOCALE_SABBREVDAYNAME2       As Long = &H32    'abbreviated name for Tuesday
Public Const LOCALE_SABBREVDAYNAME3       As Long = &H33    'abbreviated name for Wednesday
Public Const LOCALE_SABBREVDAYNAME4       As Long = &H34    'abbreviated name for Thursday
Public Const LOCALE_SABBREVDAYNAME5       As Long = &H35    'abbreviated name for Friday
Public Const LOCALE_SABBREVDAYNAME6       As Long = &H36    'abbreviated name for Saturday
Public Const LOCALE_SABBREVDAYNAME7       As Long = &H37    'abbreviated name for Sunday
Public Const LOCALE_SMONTHNAME1           As Long = &H38    'long name for January
Public Const LOCALE_SMONTHNAME2           As Long = &H39    'long name for February
Public Const LOCALE_SMONTHNAME3           As Long = &H3A    'long name for March
Public Const LOCALE_SMONTHNAME4           As Long = &H3B    'long name for April
Public Const LOCALE_SMONTHNAME5           As Long = &H3C    'long name for May
Public Const LOCALE_SMONTHNAME6           As Long = &H3D    'long name for June
Public Const LOCALE_SMONTHNAME7           As Long = &H3E    'long name for July
Public Const LOCALE_SMONTHNAME8           As Long = &H3F    'long name for August
Public Const LOCALE_SMONTHNAME9           As Long = &H40    'long name for September
Public Const LOCALE_SMONTHNAME10          As Long = &H41    'long name for October
Public Const LOCALE_SMONTHNAME11          As Long = &H42    'long name for November
Public Const LOCALE_SMONTHNAME12          As Long = &H43    'long name for December
Public Const LOCALE_SMONTHNAME13          As Long = &H100E  'long name for 13th month (if exists)
Public Const LOCALE_SABBREVMONTHNAME1     As Long = &H44    'abbreviated name for January
Public Const LOCALE_SABBREVMONTHNAME2     As Long = &H45    'abbreviated name for February
Public Const LOCALE_SABBREVMONTHNAME3     As Long = &H46    'abbreviated name for March
Public Const LOCALE_SABBREVMONTHNAME4     As Long = &H47    'abbreviated name for April
Public Const LOCALE_SABBREVMONTHNAME5     As Long = &H48    'abbreviated name for May
Public Const LOCALE_SABBREVMONTHNAME6     As Long = &H49    'abbreviated name for June
Public Const LOCALE_SABBREVMONTHNAME7     As Long = &H4A    'abbreviated name for July
Public Const LOCALE_SABBREVMONTHNAME8     As Long = &H4B    'abbreviated name for August
Public Const LOCALE_SABBREVMONTHNAME9     As Long = &H4C    'abbreviated name for September
Public Const LOCALE_SABBREVMONTHNAME10    As Long = &H4D    'abbreviated name for October
Public Const LOCALE_SABBREVMONTHNAME11    As Long = &H4E    'abbreviated name for November
Public Const LOCALE_SABBREVMONTHNAME12    As Long = &H4F    'abbreviated name for December
Public Const LOCALE_SABBREVMONTHNAME13    As Long = &H100F  'abbreviated name for 13th month (if exists)

Public Const LOCALE_SPOSITIVESIGN         As Long = &H50    'positive sign
Public Const LOCALE_SNEGATIVESIGN         As Long = &H51    'negative sign
Public Const LOCALE_IPOSSIGNPOSN          As Long = &H52    'positive sign position
Public Const LOCALE_INEGSIGNPOSN          As Long = &H53    'negative sign position
Public Const LOCALE_IPOSSYMPRECEDES       As Long = &H54    'mon sym precedes pos amt
Public Const LOCALE_IPOSSEPBYSPACE        As Long = &H55    'mon sym sep by space from pos amt
Public Const LOCALE_INEGSYMPRECEDES       As Long = &H56    'mon sym precedes neg amt
Public Const LOCALE_INEGSEPBYSPACE        As Long = &H57    'mon sym sep by space from neg amt

'#if(WINVER >= &H0400)
Public Const LOCALE_FONTSIGNATURE         As Long = &H58    'font signature
Public Const LOCALE_SISO639LANGNAME       As Long = &H59    'ISO abbreviated language name
Public Const LOCALE_SISO3166CTRYNAME      As Long = &H5A    'ISO abbreviated country name
'#endif /* WINVER >= &H0400 */

'#if(WINVER >= &H0500)
Public Const LOCALE_IDEFAULTEBCDICCODEPAGE As Long = &H1012 'default ebcdic code page
Public Const LOCALE_IPAPERSIZE            As Long = &H100A  '0 = letter, 1 = a4, 2 = legal, 3 = a3
Public Const LOCALE_SENGCURRNAME          As Long = &H1007  'english name of currency
Public Const LOCALE_SNATIVECURRNAME       As Long = &H1008  'native name of currency
Public Const LOCALE_SYEARMONTH            As Long = &H1006  'year month format string
Public Const LOCALE_SSORTNAME             As Long = &H1013  'sort name
Public Const LOCALE_IDIGITSUBSTITUTION    As Long = &H1014  '0 = none, 1 = context, 2 = native digit
'#endif /* WINVER >=  &H0500 */

Public Declare Function GetThreadLocale Lib "kernel32" () As Long

Public Declare Function GetSystemDefaultLCID Lib "kernel32" () As Long

Public Declare Function GetLocaleInfo Lib "kernel32" _
   Alias "GetLocaleInfoA" _
  (ByVal Locale As Long, _
   ByVal LCType As Long, _
   ByVal lpLCData As String, _
   ByVal cchData As Long) As Long


Public Function GetUserLocaleInfo(ByVal dwLocaleID As Long, ByVal dwLCType As Long) As String

   Dim sReturn As String
   Dim r As Long

  'call the function passing the Locale type
  'variable to retrieve the required size of
  'the string buffer needed
   r = GetLocaleInfo(dwLocaleID, dwLCType, sReturn, Len(sReturn))
    
  'if successful..
   If r Then
    
     'pad the buffer with spaces
      sReturn = Space$(r)
       
     'and call again passing the buffer
      r = GetLocaleInfo(dwLocaleID, dwLCType, sReturn, Len(sReturn))
     
     'if successful (r > 0)
      If r Then
      
        'r holds the size of the string
        'including the terminating null
         GetUserLocaleInfo = Left$(sReturn, r - 1)
      
      End If
   
   End If
    
End Function



