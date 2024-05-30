VERSION 5.00
Object = "{0D452EE1-E08F-101A-852E-02608C4D0BB4}#2.0#0"; "FM20.DLL"
Begin VB.Form frmMoveOrigin 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Move Origin"
   ClientHeight    =   1470
   ClientLeft      =   5790
   ClientTop       =   2040
   ClientWidth     =   2775
   Icon            =   "frmMoveOrigin.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   1470
   ScaleWidth      =   2775
   ShowInTaskbar   =   0   'False
   Begin VB.TextBox txtZOffset 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   1200
      TabIndex        =   1
      Top             =   480
      Width           =   975
   End
   Begin VB.TextBox txtYOffset 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   1200
      TabIndex        =   0
      Top             =   120
      Width           =   975
   End
   Begin VB.Label lblUnit2 
      AutoSize        =   -1  'True
      Caption         =   "[m]"
      Height          =   195
      Left            =   2400
      TabIndex        =   7
      Top             =   480
      Width           =   210
   End
   Begin VB.Label lblUnit1 
      AutoSize        =   -1  'True
      Caption         =   "[m]"
      Height          =   195
      Left            =   2400
      TabIndex        =   6
      Top             =   120
      Width           =   210
   End
   Begin VB.Label lblZOffset 
      AutoSize        =   -1  'True
      Caption         =   "OZ Offset:"
      Height          =   195
      Left            =   240
      TabIndex        =   5
      Top             =   480
      Width           =   735
   End
   Begin VB.Label lblYOffset 
      AutoSize        =   -1  'True
      Caption         =   "OY Offset:"
      Height          =   195
      Left            =   240
      TabIndex        =   4
      Top             =   120
      Width           =   735
   End
   Begin MSForms.CommandButton cmdCancel 
      Cancel          =   -1  'True
      Height          =   375
      Left            =   1560
      TabIndex        =   3
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
      Left            =   360
      TabIndex        =   2
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
End
Attribute VB_Name = "frmMoveOrigin"
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
    Dim Cancel As Boolean
    Cancel = False
    ValidateNumeric txtYOffset, Cancel
    If Cancel = True Then Exit Sub
    ValidateNumeric txtZOffset, Cancel
    If Cancel = True Then Exit Sub
    Dim cNode As cNode
    For Each cNode In Project.Item(ProjectIndex).colNodes
        cNode.Y = cNode.Y - CDbl(txtYOffset.Text)
        cNode.z = cNode.z + CDbl(txtZOffset.Text)
    Next cNode
    
    UpdateOrigin ProjectIndex, txtYOffset.Text, txtZOffset.Text
    
    Project.Item(ProjectIndex).cHeader.cGlobalConstraints.MinGravityCenter = Project.Item(ProjectIndex).cHeader.cGlobalConstraints.MinGravityCenter - CDbl(txtZOffset.Text)
    Project.Item(ProjectIndex).cHeader.cGlobalConstraints.MaxGravityCenter = Project.Item(ProjectIndex).cHeader.cGlobalConstraints.MaxGravityCenter - CDbl(txtZOffset.Text)

    Project.Item(ProjectIndex).cSolution.NeutralAxis = Project.Item(ProjectIndex).cSolution.NeutralAxis - CDbl(txtZOffset.Text)
    UpdateCoordinates Project.Item(ProjectIndex).frmProject.cRectWND
    Draw ProjectIndex
    Project.Item(ProjectIndex).DataChanged = True
    Unload Me
End Sub

Private Sub Form_Load()
    ProjectIndex = ActiveProject
    Me.Move (Screen.Width - Me.Width) / 2, (Screen.Height - Me.Height) / 2
    cmdOK.Picture = LoadResPicture("ID_BITMAP_OK", 0)
    cmdCancel.Picture = LoadResPicture("ID_BITMAP_CANCEL", 0)
    Me.Caption = "Move Origin - [" & GetFileName(Project.Item(ProjectIndex).sFileName) & "]"
    'txtYOffset.Text = Project.Item(ProjectIndex).CoordOrigin.Y
    'txtZOffset.Text = Project.Item(ProjectIndex).CoordOrigin.Z
    txtYOffset.Text = 0
    txtZOffset.Text = 0
End Sub

Private Sub Form_Resize()
    Me.Height = cmdOK.Top + cmdOK.Height + 600
End Sub

Private Sub txtYOffset_GotFocus()
    txtYOffset.SelStart = 0
    txtYOffset.SelLength = Len(txtYOffset.Text)
End Sub

Private Sub txtZOffset_GotFocus()
    txtZOffset.SelStart = 0
    txtZOffset.SelLength = Len(txtZOffset.Text)
End Sub
