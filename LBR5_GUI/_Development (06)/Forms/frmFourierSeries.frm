VERSION 5.00
Object = "{0D452EE1-E08F-101A-852E-02608C4D0BB4}#2.0#0"; "FM20.DLL"
Begin VB.Form frmFourierSeries 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Fourier Series"
   ClientHeight    =   1350
   ClientLeft      =   3525
   ClientTop       =   4650
   ClientWidth     =   3645
   Icon            =   "frmFourierSeries.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   1350
   ScaleWidth      =   3645
   ShowInTaskbar   =   0   'False
   Begin VB.ComboBox cbFourierSeries 
      Appearance      =   0  'Flat
      Height          =   315
      Left            =   2520
      Style           =   2  'Dropdown List
      TabIndex        =   0
      Top             =   240
      Width           =   735
   End
   Begin VB.Label lbFourierSeries 
      Alignment       =   2  'Center
      AutoSize        =   -1  'True
      Caption         =   "Number of odd terms in Fourier series expansion:"
      Height          =   390
      Left            =   120
      TabIndex        =   3
      Top             =   240
      Width           =   2100
      WordWrap        =   -1  'True
   End
   Begin MSForms.CommandButton cmdOK 
      Default         =   -1  'True
      Height          =   375
      Left            =   1200
      TabIndex        =   1
      Top             =   840
      Width           =   1095
      Caption         =   "OK"
      PicturePosition =   327683
      Size            =   "1931;661"
      FontHeight      =   165
      FontCharSet     =   0
      FontPitchAndFamily=   2
      ParagraphAlign  =   3
   End
   Begin MSForms.CommandButton cmdCancel 
      Cancel          =   -1  'True
      Height          =   375
      Left            =   2400
      TabIndex        =   2
      Top             =   840
      Width           =   1095
      Caption         =   "Cancel"
      PicturePosition =   327683
      Size            =   "1931;661"
      FontHeight      =   165
      FontCharSet     =   0
      FontPitchAndFamily=   2
      ParagraphAlign  =   3
   End
End
Attribute VB_Name = "frmFourierSeries"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Dim ProjectIndex As Integer

Private Sub cmdCancel_Click()
    Unload Me
End Sub

Private Sub cmdCancel_DblClick(Cancel As MSForms.ReturnBoolean)
    Cancel = True
End Sub

Private Sub cmdOK_Click()
    Project.Item(ProjectIndex).cHeader.JLPH = cbFourierSeries.Text
    Project.Item(ProjectIndex).DataChanged = True
    Unload Me
End Sub

Private Sub cmdOK_DblClick(Cancel As MSForms.ReturnBoolean)
    Cancel = True
End Sub

Private Sub Form_Load()
    ProjectIndex = ActiveProject
    Me.Move (Screen.Width - Me.Width) / 2, (Screen.Height - Me.Height) / 2
    cmdOK.Picture = LoadResPicture("ID_BITMAP_OK", 0)
    cmdCancel.Picture = LoadResPicture("ID_BITMAP_CANCEL", 0)
    Me.Caption = "Fourier Series - [" & GetFileName(Project.Item(ProjectIndex).sFileName) & "]"
    PopulateCb
    If Project.Item(ProjectIndex).cHeader.JLPH = 0 Then Project.Item(ProjectIndex).cHeader.JLPH = 1
    cbFourierSeries.Text = Project.Item(ProjectIndex).cHeader.JLPH
    If Licensing.LicenseLevel = 7 Then
        'cbFourierSeries.Enabled = False
    End If
End Sub

Private Sub Form_Resize()
    Me.Height = cmdOK.Top + cmdOK.Height + 600
End Sub

Private Sub Form_Unload(Cancel As Integer)
    fMainForm.SetFocus
End Sub

Private Sub PopulateCb()
    Dim i As Integer
    For i = -100 To -1
        cbFourierSeries.AddItem i
    Next i
    For i = 1 To 100
        cbFourierSeries.AddItem i
    Next i
End Sub


