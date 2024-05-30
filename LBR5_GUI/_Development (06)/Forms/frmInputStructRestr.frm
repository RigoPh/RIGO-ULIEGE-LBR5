VERSION 5.00
Object = "{0D452EE1-E08F-101A-852E-02608C4D0BB4}#2.0#0"; "FM20.DLL"
Begin VB.Form frmInputStructRestr 
   BorderStyle     =   4  'Fixed ToolWindow
   Caption         =   "Form1"
   ClientHeight    =   1335
   ClientLeft      =   4995
   ClientTop       =   1830
   ClientWidth     =   3015
   Icon            =   "frmInputStructRestr.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   1335
   ScaleWidth      =   3015
   ShowInTaskbar   =   0   'False
   Begin VB.TextBox txtAssessmentPoint 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   1560
      TabIndex        =   3
      Text            =   "Text1"
      Top             =   360
      Width           =   1335
   End
   Begin VB.TextBox txtValue 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   120
      TabIndex        =   2
      Text            =   "Text1"
      Top             =   360
      Width           =   1335
   End
   Begin VB.Label lblAssessmentPoint 
      AutoSize        =   -1  'True
      Caption         =   "Assessment Point:"
      Height          =   195
      Left            =   1560
      TabIndex        =   5
      Top             =   120
      Width           =   1290
   End
   Begin VB.Label lblValue 
      AutoSize        =   -1  'True
      Caption         =   "Value:"
      Height          =   195
      Left            =   120
      TabIndex        =   4
      Top             =   120
      Width           =   450
   End
   Begin MSForms.CommandButton cmdOK 
      Default         =   -1  'True
      Height          =   375
      Left            =   360
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
      Left            =   1560
      TabIndex        =   0
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
   Begin VB.Shape Shape1 
      Height          =   1335
      Left            =   0
      Top             =   0
      Width           =   3015
   End
End
Attribute VB_Name = "frmInputStructRestr"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

Private Sub cmdCancel_Click()
    Unload Me
End Sub

Private Sub cmdOK_Click()
    Unload Me
End Sub

Private Sub Form_Load()
    Me.Move (Screen.WIDTH - Me.WIDTH) / 2, (Screen.Height - Me.Height) / 2
    cmdOK.Picture = LoadResPicture("ID_BITMAP_OK", 0)
    cmdCancel.Picture = LoadResPicture("ID_BITMAP_CANCEL", 0)

End Sub

