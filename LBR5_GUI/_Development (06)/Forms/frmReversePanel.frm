VERSION 5.00
Object = "{0D452EE1-E08F-101A-852E-02608C4D0BB4}#2.0#0"; "FM20.DLL"
Begin VB.Form frmReversePanel 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Reverse Panel"
   ClientHeight    =   1305
   ClientLeft      =   4260
   ClientTop       =   2130
   ClientWidth     =   3975
   Icon            =   "frmReversePanel.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   1305
   ScaleWidth      =   3975
   ShowInTaskbar   =   0   'False
   Begin VB.ComboBox cbPanels 
      Height          =   315
      Left            =   120
      Style           =   2  'Dropdown List
      TabIndex        =   0
      Top             =   240
      Width           =   1815
   End
   Begin MSForms.CommandButton cmdApply 
      Default         =   -1  'True
      Height          =   375
      Left            =   1560
      TabIndex        =   1
      Top             =   720
      Width           =   1095
      Caption         =   "Apply"
      PicturePosition =   327683
      Size            =   "1931;661"
      Accelerator     =   65
      FontHeight      =   165
      FontCharSet     =   0
      FontPitchAndFamily=   2
      ParagraphAlign  =   3
   End
   Begin MSForms.CommandButton cmdCancel 
      Cancel          =   -1  'True
      Height          =   375
      Left            =   2760
      TabIndex        =   3
      Top             =   720
      Width           =   1095
      Caption         =   "Close"
      PicturePosition =   327683
      Size            =   "1931;661"
      FontHeight      =   165
      FontCharSet     =   0
      FontPitchAndFamily=   2
      ParagraphAlign  =   3
   End
   Begin MSForms.CommandButton cmdOK 
      Height          =   375
      Left            =   1560
      TabIndex        =   2
      Top             =   720
      Visible         =   0   'False
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
Attribute VB_Name = "frmReversePanel"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Dim ProjectIndex As Integer

Private Sub cmdApply_Click()
    Project.Item(ProjectIndex).colPanel.Item(cbPanels.ListIndex + 1).Reverse
    'GetData
End Sub

Private Sub cmdApply_DblClick(Cancel As MSForms.ReturnBoolean)
    Cancel = True
End Sub

Private Sub cmdCancel_Click()
    Unload Me
End Sub

Private Sub cmdOK_Click()
    Project.Item(ProjectIndex).colPanel.Item(cbPanels.ListIndex + 1).Reverse
    Unload Me
End Sub

Private Sub Form_Load()
    ProjectIndex = ActiveProject
    Me.Move (Screen.Width - Me.Width) / 2, (Screen.Height - Me.Height) / 2
    cmdOK.Picture = LoadResPicture("ID_BITMAP_OK", 0)
    cmdCancel.Picture = LoadResPicture("ID_BITMAP_CANCEL", 0)
    Me.Caption = "Reverse Panel - [" & GetFileName(Project.Item(ProjectIndex).sFileName) & "]"
    GetData
End Sub

Private Sub GetData()
    Dim i As Integer
    cbPanels.Clear
    For i = 1 To Project.Item(ProjectIndex).colPanel.Count
        cbPanels.AddItem "Panel " & i
    Next i
    cbPanels.ListIndex = 0
End Sub
