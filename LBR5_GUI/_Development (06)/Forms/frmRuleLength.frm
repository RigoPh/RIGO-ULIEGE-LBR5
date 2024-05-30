VERSION 5.00
Object = "{0D452EE1-E08F-101A-852E-02608C4D0BB4}#2.0#0"; "FM20.DLL"
Begin VB.Form frmRuleLength 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Rule Length"
   ClientHeight    =   1545
   ClientLeft      =   2355
   ClientTop       =   4230
   ClientWidth     =   3855
   Icon            =   "frmRuleLength.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   1545
   ScaleWidth      =   3855
   ShowInTaskbar   =   0   'False
   Begin VB.TextBox txtRuleLength 
      Appearance      =   0  'Flat
      BeginProperty DataFormat 
         Type            =   1
         Format          =   "0"
         HaveTrueFalseNull=   0
         FirstDayOfWeek  =   0
         FirstWeekOfYear =   0
         LCID            =   1033
         SubFormatType   =   1
      EndProperty
      Height          =   285
      Left            =   1935
      TabIndex        =   0
      Text            =   "txtRuleLength"
      Top             =   240
      Width           =   1335
   End
   Begin MSForms.CommandButton cmdCancel 
      Cancel          =   -1  'True
      Height          =   375
      Left            =   2640
      TabIndex        =   4
      Top             =   960
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
      Left            =   1440
      TabIndex        =   3
      Top             =   960
      Width           =   1095
      Caption         =   "OK"
      PicturePosition =   327683
      Size            =   "1931;661"
      FontHeight      =   165
      FontCharSet     =   0
      FontPitchAndFamily=   2
      ParagraphAlign  =   3
   End
   Begin VB.Label lbUnit1 
      AutoSize        =   -1  'True
      Caption         =   "[ m ]"
      Height          =   195
      Left            =   3375
      TabIndex        =   2
      Top             =   240
      Width           =   300
   End
   Begin VB.Label lbNoOfOpti 
      AutoSize        =   -1  'True
      Caption         =   "Rule Length:"
      Height          =   195
      Left            =   165
      TabIndex        =   1
      Top             =   240
      Width           =   1365
      WordWrap        =   -1  'True
   End
End
Attribute VB_Name = "frmRuleLength"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Dim ProjectIndex As Integer
Private OBJ As New cHeader

Private Sub cmdCancel_Click()
    Unload Me
End Sub

Private Sub cmdOK_Click()
    SetData
    Unload Me
End Sub

Private Sub Form_Load()
    ProjectIndex = ActiveProject
    Me.Move (Screen.Width - Me.Width) / 2, (Screen.Height - Me.Height) / 2
    cmdOK.Picture = LoadResPicture("ID_BITMAP_OK", 0)
    cmdCancel.Picture = LoadResPicture("ID_BITMAP_CANCEL", 0)
    Me.Caption = "Rule Length- [" & GetFileName(Project.Item(ProjectIndex).sFileName) & "]"
    GetData
End Sub

Private Sub GetData()
    Set OBJ = Project.Item(ProjectIndex).cHeader
    txtRuleLength = OBJ.LongRegl
End Sub

Private Sub SetData()
    OBJ.LongRegl = Val(txtRuleLength)
End Sub

Private Sub Form_Unload(Cancel As Integer)
    fMainForm.SetFocus
End Sub

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
    Set OBJ = Nothing
End Sub

Private Sub txtRuleLength_GotFocus()
    txtRuleLength.SelStart = 0
    txtRuleLength.SelLength = Len(txtRuleLength.Text)
End Sub

Private Sub txtRuleLength_Validate(Cancel As Boolean)
    ValidateNumeric txtRuleLength, Cancel
    ValidateNullOrPozitive txtRuleLength, Cancel
End Sub
