VERSION 5.00
Object = "{8E27C92E-1264-101C-8A2F-040224009C02}#7.0#0"; "MSCAL.OCX"
Begin VB.Form frmLic_Gen 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "lic_gen LBR5V1"
   ClientHeight    =   7335
   ClientLeft      =   2250
   ClientTop       =   1650
   ClientWidth     =   3390
   Icon            =   "frmLic_Gen.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   7335
   ScaleWidth      =   3390
   Begin VB.TextBox txtDetails 
      Appearance      =   0  'Flat
      Height          =   2895
      Left            =   120
      Locked          =   -1  'True
      MultiLine       =   -1  'True
      TabIndex        =   10
      Top             =   1080
      Width           =   3135
   End
   Begin MSACAL.Calendar Calendar 
      Height          =   2295
      Left            =   0
      TabIndex        =   8
      Top             =   4320
      Width           =   3375
      _Version        =   524288
      _ExtentX        =   5953
      _ExtentY        =   4048
      _StockProps     =   1
      BackColor       =   -2147483633
      Year            =   2007
      Month           =   3
      Day             =   20
      DayLength       =   1
      MonthLength     =   2
      DayFontColor    =   0
      FirstDay        =   1
      GridCellEffect  =   0
      GridFontColor   =   10485760
      GridLinesColor  =   -2147483632
      ShowDateSelectors=   -1  'True
      ShowDays        =   -1  'True
      ShowHorizontalGrid=   -1  'True
      ShowTitle       =   0   'False
      ShowVerticalGrid=   -1  'True
      TitleFontColor  =   10485760
      ValueIsNull     =   0   'False
      BeginProperty DayFont {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty GridFont {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty TitleFont {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial"
         Size            =   12
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
   End
   Begin VB.CommandButton cmdGenerateLicense 
      Caption         =   "Generate License"
      Height          =   375
      Left            =   1800
      TabIndex        =   6
      Top             =   6720
      Width           =   1455
   End
   Begin VB.TextBox txtKey 
      Appearance      =   0  'Flat
      Enabled         =   0   'False
      Height          =   285
      Left            =   1320
      TabIndex        =   5
      Top             =   6840
      Visible         =   0   'False
      Width           =   1935
   End
   Begin VB.ComboBox cbLicenseLevel 
      Height          =   315
      Left            =   1320
      Style           =   2  'Dropdown List
      TabIndex        =   3
      Top             =   480
      Width           =   1935
   End
   Begin VB.TextBox txtMAC 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   1320
      TabIndex        =   1
      Top             =   120
      Width           =   1935
   End
   Begin VB.Label lblDetails 
      AutoSize        =   -1  'True
      Caption         =   "Details:"
      Height          =   195
      Left            =   120
      TabIndex        =   9
      Top             =   840
      Width           =   525
   End
   Begin VB.Label Label4 
      AutoSize        =   -1  'True
      Caption         =   "Expiration Date:"
      Height          =   195
      Left            =   120
      TabIndex        =   7
      Top             =   4080
      Width           =   1125
   End
   Begin VB.Label Label3 
      AutoSize        =   -1  'True
      Caption         =   "Encryption Key:"
      Height          =   195
      Left            =   120
      TabIndex        =   4
      Top             =   6840
      Visible         =   0   'False
      Width           =   1110
   End
   Begin VB.Label Label2 
      AutoSize        =   -1  'True
      Caption         =   "License Level:"
      Height          =   195
      Left            =   120
      TabIndex        =   2
      Top             =   480
      Width           =   1035
   End
   Begin VB.Label Label1 
      AutoSize        =   -1  'True
      Caption         =   "MAC Address:"
      Height          =   195
      Left            =   120
      TabIndex        =   0
      Top             =   120
      Width           =   1005
   End
End
Attribute VB_Name = "frmLic_Gen"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Dim sMAC As String
Dim sLevel As String
Dim sKey As String
Dim sUncrypted As String
Dim sCrypted As String

Private Sub cbLicenseLevel_Click()
    ShowDetails
End Sub

Private Sub cmdGenerateLicense_Click()
    Dim aes As New CRijndael
    Dim sFile As String
    Dim sDate As String
    Dim sExpiration As String
    If txtMAC = "" Then
        MsgBox "Must insert a MAC address you do.", vbCritical + vbOKOnly
        Exit Sub
    End If
    sExpiration = Format(Calendar, "dd/mm/yyyy")
    sDate = Format(Date, "dd/mm/yyyy")
    
    If Calendar.Day = "0" Then
        MsgBox "You must chose a day. You must...", vbInformation + vbOKOnly
        Exit Sub
    End If
    If CheckPastDate() = False Then Exit Sub
    If sDate = sExpiration Then
        MsgBox "License cannot expire today. In fact it could, but it wouldn't make any sense...", vbInformation + vbOKOnly
        Exit Sub
    End If
    
    Dim tim As String
    tim = Date
    sMAC = Trim(UCase(Replace(txtMAC, "-", "")))
    sMAC = Replace(sMAC, ":", "")
    sLevel = Left(cbLicenseLevel, 1)
    sKey = Trim(txtKey)
    sUncrypted = sMAC + sLevel + sExpiration
    sCrypted = aes.JustCrypter(sUncrypted, sKey)
    
    sFile = App.Path & "\license.lic"
    Open sFile For Output As #1
    Print #1, sCrypted
    'Close (1)
    
    Print #1, "LBR-5 license generated at: " & Date
    Print #1, "License Level: " & cbLicenseLevel.List(sLevel)
    Print #1, "Expires at: " & sExpiration
    Close (1)
    MsgBox "License file created." & vbCrLf & _
    "License Level: " & cbLicenseLevel.List(sLevel) & vbCrLf & _
    "Expires at: " & sExpiration
    End
End Sub

Private Sub Form_Load()
    On Error Resume Next
    popCb
    Calendar = Date
    txtMAC = "00-00-00-00-00-00"
    txtKey = "FH7GDG5U8OVCZ31O"
    Dim sMACfile As String
    sMACfile = App.Path & "\mac_address.txt"
    Open sMACfile For Input As #3
    Line Input #3, sMAC
    txtMAC = sMAC
    Close (3)
        
End Sub

Sub popCb()
    cbLicenseLevel.AddItem "0 Professional licence"
    cbLicenseLevel.AddItem "1 Professional licence"
    cbLicenseLevel.AddItem "2 Professional licence"
    cbLicenseLevel.AddItem "3 Professional licence"
    cbLicenseLevel.AddItem "4 Academic licence"
    cbLicenseLevel.AddItem "5 Academic licence"
    cbLicenseLevel.AddItem "6 Student licence"
    cbLicenseLevel.AddItem "7 Demo licence"
    cbLicenseLevel.ListIndex = 0
End Sub

Function CheckPastDate() As Boolean
    Dim sDay As String
    Dim sMonth As String
    Dim sYear As String
    Dim sDate As String
    Dim pos As Integer
    CheckPastDate = True 'valid
    sDate = Date
    
    pos = InStr(1, sDate, "/")
    sDay = Left(sDate, pos - 1)
    sDate = Mid(sDate, pos + 1)
    pos = InStr(1, sDate, "/")
    sMonth = Left(sDate, pos - 1)
    sDate = Mid(sDate, pos + 1)
    sYear = sDate
    
    If CInt(Calendar.Year) < CInt(sYear) Then
        CheckPastDate = False
    End If
    If CInt(Calendar.Year) = CInt(sYear) And CInt(Calendar.Month) < CInt(sMonth) Then
        CheckPastDate = False
    End If
    If CInt(Calendar.Year) = CInt(sYear) And CInt(Calendar.Month) = CInt(sMonth) And CInt(Calendar.Day) < CInt(sDay) Then
        CheckPastDate = False
    End If
    
    If CheckPastDate = False Then
        MsgBox "Why would you set the expiration date in the past?", vbQuestion + vbOKOnly
    End If
End Function

Sub ShowDetails()
        Select Case cbLicenseLevel.ListIndex
            Case 0
                txtDetails = "AKER License (0)" & vbCrLf & _
                    "LBR4 Solver:" & vbTab & vbTab & "YES" & vbCrLf & _
                    "BEAM THEORY Solver:" & vbTab & "YES" & vbCrLf & _
                    "COSTCAT:" & vbTab & vbTab & "YES" & vbCrLf & _
                    "MAX PANELS:" & vbTab & vbTab & "150" & vbCrLf & _
                    "MAX ACTIVE LOAD CASES:" & vbTab & " 10" & vbCrLf & _
                    "MAX STR CONSTR / PANEL:" & "200" & vbCrLf & _
                    "MAX GEO CONSTR / PANEL:" & " 20 " & vbCrLf & _
                    "MAX EQ RESTR:" & vbTab & vbTab & "1350" & vbCrLf & _
                    "IMPORT MARS:" & vbTab & vbTab & "YES" & vbCrLf & _
                    "IMPORT AVPRO:" & vbTab & vbTab & "YES"
            Case 1
                txtDetails = "Professional License (1)" & vbCrLf & _
                    "LBR4 Solver:" & vbTab & vbTab & "YES" & vbCrLf & _
                    "BEAM THEORY Solver:" & vbTab & "YES" & vbCrLf & _
                    "COSTCAT:" & vbTab & vbTab & "NO" & vbCrLf & _
                    "MAX PANELS:" & vbTab & vbTab & "150" & vbCrLf & _
                    "MAX ACTIVE LOAD CASES:" & vbTab & " 10" & vbCrLf & _
                    "MAX STR CONSTR / PANEL:" & "200" & vbCrLf & _
                    "MAX GEO CONSTR / PANEL:" & " 20 " & vbCrLf & _
                    "MAX EQ RESTR:" & vbTab & vbTab & "1350" & vbCrLf & _
                    "IMPORT MARS:" & vbTab & vbTab & "YES" & vbCrLf & _
                    "IMPORT AVPRO:" & vbTab & vbTab & "YES"

            Case 2
                txtDetails = "Professional License (2)" & vbCrLf & _
                    "LBR4 Solver:" & vbTab & vbTab & "NO" & vbCrLf & _
                    "BEAM THEORY Solver:" & vbTab & "YES" & vbCrLf & _
                    "COSTCAT:" & vbTab & vbTab & "NO" & vbCrLf & _
                    "MAX PANELS:" & vbTab & vbTab & "150" & vbCrLf & _
                    "MAX ACTIVE LOAD CASES:" & vbTab & " 10" & vbCrLf & _
                    "MAX STR CONSTR / PANEL:" & "200" & vbCrLf & _
                    "MAX GEO CONSTR / PANEL:" & " 20 " & vbCrLf & _
                    "MAX EQ RESTR:" & vbTab & vbTab & "1350" & vbCrLf & _
                    "IMPORT MARS:" & vbTab & vbTab & "YES" & vbCrLf & _
                    "IMPORT AVPRO:" & vbTab & vbTab & "YES"
            Case 3
                txtDetails = "Professional License (3)" & vbCrLf & _
                    "LBR4 Solver:" & vbTab & vbTab & "YES" & vbCrLf & _
                    "BEAM THEORY Solver:" & vbTab & "NO" & vbCrLf & _
                    "COSTCAT:" & vbTab & vbTab & "NO" & vbCrLf & _
                    "MAX PANELS:" & vbTab & vbTab & "150" & vbCrLf & _
                    "MAX ACTIVE LOAD CASES:" & vbTab & " 10" & vbCrLf & _
                    "MAX STR CONSTR / PANEL:" & "200" & vbCrLf & _
                    "MAX GEO CONSTR / PANEL:" & " 20 " & vbCrLf & _
                    "MAX EQ RESTR:" & vbTab & vbTab & "1350" & vbCrLf & _
                    "IMPORT MARS:" & vbTab & vbTab & "YES" & vbCrLf & _
                    "IMPORT AVPRO:" & vbTab & vbTab & "YES"

            Case 4
                txtDetails = "Academic License (4)" & vbCrLf & _
                    "LBR4 Solver:" & vbTab & vbTab & "YES" & vbCrLf & _
                    "BEAM THEORY Solver:" & vbTab & "YES" & vbCrLf & _
                    "COSTCAT:" & vbTab & vbTab & "NO" & vbCrLf & _
                    "MAX PANELS:" & vbTab & vbTab & "150" & vbCrLf & _
                    "MAX ACTIVE LOAD CASES:" & vbTab & " 10" & vbCrLf & _
                    "MAX STR CONSTR / PANEL:" & "200" & vbCrLf & _
                    "MAX GEO CONSTR / PANEL:" & " 20 " & vbCrLf & _
                    "MAX EQ RESTR:" & vbTab & vbTab & "1350" & vbCrLf & _
                    "IMPORT MARS:" & vbTab & vbTab & "YES" & vbCrLf & _
                    "IMPORT AVPRO:" & vbTab & vbTab & "YES"

            Case 5
                txtDetails = "Academic License (5)" & vbCrLf & _
                    "LBR4 Solver:" & vbTab & vbTab & "NO" & vbCrLf & _
                    "BEAM THEORY Solver:" & vbTab & "YES" & vbCrLf & _
                    "COSTCAT:" & vbTab & vbTab & "NO" & vbCrLf & _
                    "MAX PANELS:" & vbTab & vbTab & "150" & vbCrLf & _
                    "MAX ACTIVE LOAD CASES:" & vbTab & " 10" & vbCrLf & _
                    "MAX STR CONSTR / PANEL:" & "200" & vbCrLf & _
                    "MAX GEO CONSTR / PANEL:" & " 20 " & vbCrLf & _
                    "MAX EQ RESTR:" & vbTab & vbTab & "1350" & vbCrLf & _
                    "IMPORT MARS:" & vbTab & vbTab & "YES" & vbCrLf & _
                    "IMPORT AVPRO:" & vbTab & vbTab & "YES"

            Case 6
                txtDetails = "Student License (6)" & vbCrLf & _
                    "LBR4 Solver:" & vbTab & vbTab & "NO" & vbCrLf & _
                    "BEAM THEORY Solver:" & vbTab & "YES" & vbCrLf & _
                    "COSTCAT:" & vbTab & vbTab & "NO" & vbCrLf & _
                    "MAX PANELS:" & vbTab & vbTab & " 10" & vbCrLf & _
                    "MAX ACTIVE LOAD CASES:" & vbTab & " 2" & vbCrLf & _
                    "MAX STR CONSTR / PANEL:" & "10" & vbCrLf & _
                    "MAX GEO CONSTR / PANEL:" & " 10 " & vbCrLf & _
                    "MAX EQ RESTR:" & vbTab & vbTab & "90" & vbCrLf & _
                    "IMPORT MARS:" & vbTab & vbTab & "YES" & vbCrLf & _
                    "IMPORT AVPRO:" & vbTab & vbTab & "NO"
            Case 7
                txtDetails = "Demo Version (7)" & vbCrLf & _
                    "LBR4 Solver:" & vbTab & vbTab & "NO" & vbCrLf & _
                    "BEAM THEORY Solver:" & vbTab & "YES" & vbCrLf & _
                    "COSTCAT:" & vbTab & vbTab & "NO" & vbCrLf & _
                    "MAX PANELS:" & vbTab & vbTab & " 5" & vbCrLf & _
                    "MAX ACTIVE LOAD CASES:" & vbTab & " 1" & vbCrLf & _
                    "MAX STR CONSTR / PANEL:" & "  5" & vbCrLf & _
                    "MAX GEO CONSTR / PANEL:" & " 5 " & vbCrLf & _
                    "MAX EQ RESTR:" & vbTab & vbTab & "45" & vbCrLf & _
                    "IMPORT MARS:" & vbTab & vbTab & "NO" & vbCrLf & _
                    "IMPORT AVPRO:" & vbTab & vbTab & "NO" & vbCrLf & _
                    "GEOMETRY: " & vbTab & vbTab & "LOCKED"

            Case Else
        End Select
End Sub
