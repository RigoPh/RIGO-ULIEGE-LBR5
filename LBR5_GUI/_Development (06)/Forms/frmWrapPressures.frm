VERSION 5.00
Object = "{5E9E78A0-531B-11CF-91F6-C2863C385E30}#1.0#0"; "MSFLXGRD.OCX"
Object = "{0D452EE1-E08F-101A-852E-02608C4D0BB4}#2.0#0"; "FM20.DLL"
Begin VB.Form frmWrapPressures 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Wrap Pressures"
   ClientHeight    =   3660
   ClientLeft      =   9015
   ClientTop       =   2595
   ClientWidth     =   6015
   Icon            =   "frmWrapPressures.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   3660
   ScaleWidth      =   6015
   ShowInTaskbar   =   0   'False
   Begin VB.CheckBox Chk 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      ForeColor       =   &H80000008&
      Height          =   195
      Index           =   0
      Left            =   5400
      TabIndex        =   1
      Top             =   240
      Width           =   200
   End
   Begin MSFlexGridLib.MSFlexGrid MSH1 
      Height          =   2955
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   6015
      _ExtentX        =   10610
      _ExtentY        =   5212
      _Version        =   393216
      Cols            =   11
      GridColor       =   0
      WordWrap        =   -1  'True
      ScrollTrack     =   -1  'True
      GridLinesFixed  =   1
      Appearance      =   0
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
   End
   Begin MSForms.CommandButton cmdCancel 
      Cancel          =   -1  'True
      Height          =   375
      Left            =   4800
      TabIndex        =   3
      Top             =   3120
      Width           =   1095
      Caption         =   "Cancel"
      PicturePosition =   327683
      Size            =   "1931;661"
      FontHeight      =   165
      FontCharSet     =   0
      FontPitchAndFamily=   2
      ParagraphAlign  =   3
   End
   Begin MSForms.CommandButton cmdOK 
      Default         =   -1  'True
      Height          =   375
      Left            =   3600
      TabIndex        =   2
      Top             =   3120
      Width           =   1095
      Caption         =   "OK"
      PicturePosition =   327683
      Size            =   "1931;661"
      FontHeight      =   165
      FontCharSet     =   0
      FontPitchAndFamily=   2
      ParagraphAlign  =   3
   End
End
Attribute VB_Name = "frmWrapPressures"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Dim ProjectIndex As Integer

Private Sub cmdCancel_Click()
    Unload Me
End Sub

Private Sub cmdOK_Click()
    Dim i As Integer
    Dim iChk As Integer
    For i = 1 To Chk.Count - 1
        If Chk(i) = vbChecked Then
            iChk = iChk + 1
        End If
    Next i
    If iChk < 2 Then
        MsgBox "At list two load cases must be checked to wrap pressures.", vbInformation + vbOKOnly
        Exit Sub
    End If
    If Project.Item(ActiveProject).cHeader.colLoadCase.Count >= 20 Then
        MsgBox "Maximum twenty load cases can be defined", vbExclamation + vbOKOnly, "Warning"
        Exit Sub
    End If
    WrapPressures
    Unload Me
End Sub

Private Sub Form_Load()
    ProjectIndex = ActiveProject
    Me.Move (Screen.Width - Me.Width) / 2, (Screen.Height - Me.Height) / 2
    cmdOK.Picture = LoadResPicture("ID_BITMAP_OK", 0)
    cmdCancel.Picture = LoadResPicture("ID_BITMAP_CANCEL", 0)
    Me.Caption = "Wrap Pressures - [" & GetFileName(Project.Item(ProjectIndex).sFileName) & "]"
    Chk(0).Visible = False
    FlexGrid
    LoadChk
End Sub

Private Sub FlexGrid()
    Dim i As Integer
    MSH1.Cols = 3
    MSH1.FormatString = "|^|^"
    MSH1.Rows = Project.Item(ActiveProject).cHeader.colLoadCase.Count + 1
    MSH1.ColWidth(0) = 450
    MSH1.ColWidth(1) = 5000
    MSH1.ColWidth(2) = 250
    
    MSH1.TextMatrix(0, 0) = "Case"
    MSH1.TextMatrix(0, 1) = "Name"
    
    For i = 1 To Project.Item(ActiveProject).cHeader.colLoadCase.Count
        MSH1.TextMatrix(i, 0) = i
        MSH1.TextMatrix(i, 1) = Project.Item(ActiveProject).cHeader.colLoadCase.Item(i).Title
    Next i
    
End Sub

Private Sub LoadChk()
    Dim i As Integer
    Dim h As Integer
    Chk(0).Left = MSH1.Left + MSH1.Width - 525
    Chk(0).Top = MSH1.Top + 250 + 25
    h = Chk(0).Top
    For i = 1 To Chk.UBound
        Unload Chk(i)
    Next i
    
    For i = 1 To Project.Item(ProjectIndex).cHeader.colLoadCase.Count
        Load Chk(i)
        Chk(i).Top = h
        Chk(i).Visible = True
        Chk(i).ZOrder 0
        h = h + 240
        'select active cases par default for wrapping
        If Project.Item(ProjectIndex).cHeader.colLoadCase.Item(i).state = IsOn Then Chk(i) = vbChecked
    Next i
End Sub

Private Sub WrapPressures()
    Dim i As Integer
    Dim oPanel As cPanel
    Dim oLoadCase As cLoadCase
    Dim newLoadCase As cLoadCase
    Set newLoadCase = New cLoadCase
    Dim pin As Double, pout As Double
    Dim strLoadCase As String
    Dim state As LoadCaseState, iActive As Integer
    
    For i = 1 To Chk.Count - 1
        If Chk(i) = vbChecked Then
            strLoadCase = strLoadCase & i & ", "
        End If
    Next i
    
    For Each oLoadCase In Project.Item(ProjectIndex).cHeader.colLoadCase
        If oLoadCase.state = IsOn Then
            iActive = iActive + 1
        End If
    Next oLoadCase
    If iActive >= Licensing.MAX_ACTIVE_LOAD_CASES Then
        state = IsOff
        MsgBox "The maximum number of active load cases is restricted to " & Licensing.MAX_ACTIVE_LOAD_CASES & "." & _
        vbCrLf & "The wrapped pressures load case will be set as inactive.", vbInformation + vbOKOnly, "LBR-5 License Limitation"
    Else
        state = IsOn
    End If
    
    strLoadCase = Trim(strLoadCase)
    strLoadCase = Left(strLoadCase, Len(strLoadCase) - 1)
    strLoadCase = "Wrapped Pressures (Load Cases: " & strLoadCase & ")"
    newLoadCase.index = Project.Item(ProjectIndex).cHeader.colLoadCase.Count + 1
    newLoadCase.Title = strLoadCase
    newLoadCase.state = state
    Project.Item(ProjectIndex).cHeader.colLoadCase.Add newLoadCase, newLoadCase.index
    Set newLoadCase = Nothing
    
    For Each oPanel In Project.Item(ProjectIndex).colPanel
        pin = 0
        pout = 0
        For Each oLoadCase In oPanel.colLoadCase
            If Abs(oLoadCase.LateralPressureIn) > Abs(pin) Then
                pin = oLoadCase.LateralPressureIn
            End If
            If Abs(oLoadCase.LateralPressureOut) > Abs(pout) Then
                pout = oLoadCase.LateralPressureOut
            End If
        Next oLoadCase
        Set newLoadCase = New cLoadCase
        newLoadCase.index = Project.Item(ProjectIndex).cHeader.colLoadCase.Count
        newLoadCase.Title = strLoadCase
        newLoadCase.state = state
        newLoadCase.LateralPressureIn = pin
        newLoadCase.LateralPressureOut = pout
        oPanel.colLoadCase.Add newLoadCase, newLoadCase.index
        Set newLoadCase = Nothing
    Next oPanel
    

    
End Sub

















