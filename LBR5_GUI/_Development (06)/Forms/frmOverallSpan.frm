VERSION 5.00
Object = "{0D452EE1-E08F-101A-852E-02608C4D0BB4}#2.0#0"; "FM20.DLL"
Begin VB.Form frmOverallSpan 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Overall Span"
   ClientHeight    =   2130
   ClientLeft      =   6840
   ClientTop       =   5595
   ClientWidth     =   4845
   Icon            =   "frmOverallSpan.frx":0000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   2130
   ScaleWidth      =   4845
   ShowInTaskbar   =   0   'False
   Begin VB.TextBox txtCrossSect 
      Appearance      =   0  'Flat
      Height          =   285
      Index           =   5
      Left            =   3600
      TabIndex        =   5
      Text            =   "txtCrossSect"
      Top             =   1080
      Width           =   735
   End
   Begin VB.TextBox txtCrossSect 
      Appearance      =   0  'Flat
      Height          =   285
      Index           =   4
      Left            =   2760
      TabIndex        =   4
      Text            =   "txtCrossSect"
      Top             =   1080
      Width           =   735
   End
   Begin VB.TextBox txtCrossSect 
      Appearance      =   0  'Flat
      Height          =   285
      Index           =   3
      Left            =   1920
      TabIndex        =   3
      Text            =   "txtCrossSect"
      Top             =   1080
      Width           =   735
   End
   Begin VB.TextBox txtCrossSect 
      Appearance      =   0  'Flat
      Height          =   285
      Index           =   2
      Left            =   1080
      TabIndex        =   2
      Text            =   "txtCrossSect"
      Top             =   1080
      Width           =   735
   End
   Begin VB.TextBox txtCrossSect 
      Appearance      =   0  'Flat
      Height          =   285
      Index           =   1
      Left            =   240
      TabIndex        =   1
      Text            =   "txtCrossSect"
      Top             =   1080
      Width           =   735
   End
   Begin VB.TextBox txtSpan 
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
      Left            =   3000
      TabIndex        =   0
      Text            =   "txtSpan"
      Top             =   240
      Width           =   1335
   End
   Begin MSForms.CommandButton cmdRecommended 
      Height          =   375
      Left            =   240
      TabIndex        =   6
      Top             =   1560
      Width           =   1935
      Caption         =   "Recommended Sections"
      Size            =   "3413;661"
      FontHeight      =   165
      FontCharSet     =   0
      FontPitchAndFamily=   2
      ParagraphAlign  =   3
   End
   Begin VB.Label lbUnit2 
      AutoSize        =   -1  'True
      Caption         =   "[ m ]"
      Height          =   195
      Left            =   4440
      TabIndex        =   12
      Top             =   1080
      Width           =   300
   End
   Begin VB.Label lbCrossSections 
      Alignment       =   2  'Center
      AutoSize        =   -1  'True
      Caption         =   "Cross Sections to Compute:"
      Height          =   255
      Left            =   225
      TabIndex        =   11
      Top             =   720
      Width           =   1950
      WordWrap        =   -1  'True
   End
   Begin VB.Label lbNoOfOpti 
      Alignment       =   2  'Center
      AutoSize        =   -1  'True
      Caption         =   "Overall Span of the Structure:"
      Height          =   255
      Left            =   225
      TabIndex        =   10
      Top             =   240
      Width           =   2175
      WordWrap        =   -1  'True
   End
   Begin VB.Label lbUnit1 
      AutoSize        =   -1  'True
      Caption         =   "[ m ]"
      Height          =   195
      Left            =   4440
      TabIndex        =   9
      Top             =   240
      Width           =   300
   End
   Begin MSForms.CommandButton cmdOK 
      Default         =   -1  'True
      Height          =   375
      Left            =   2400
      TabIndex        =   7
      Top             =   1560
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
      Left            =   3600
      TabIndex        =   8
      Top             =   1560
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
Attribute VB_Name = "frmOverallSpan"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Dim ProjectIndex As Integer
Private OBJ As New cHeader
Private IANA As Integer

Private Sub cmdCancel_Click()
    Unload Me
End Sub

Private Sub cmdCancel_DblClick(Cancel As MSForms.ReturnBoolean)
    Cancel = True
End Sub

Private Sub cmdOK_Click()
    Dim Cancel As Boolean
    Dim i As Integer
    txtSpan_Validate Cancel
    If Cancel = True Then Exit Sub
    For i = 1 To 5
        txtCrossSect_Validate i, Cancel
        If Cancel = True Then Exit Sub
    Next i
    If Project.Item(ProjectIndex).cHeader.IOPTI = yes Then
        If Val_(txtCrossSect(5).Text) > Val_(txtSpan.Text / 2) Then
            MsgBox "Last section exceeds half span", vbCritical + vbOKOnly
            Exit Sub
        End If
    End If
    OBJ.Width = txtSpan.Text
    OBJ.DIS1 = txtCrossSect(1).Text
    OBJ.DIS2 = txtCrossSect(2).Text
    OBJ.DIS3 = txtCrossSect(3).Text
    OBJ.DIS4 = txtCrossSect(4).Text
    OBJ.DIS5 = txtCrossSect(5).Text
    If OBJ.IOPTI = yes Then
        OBJ.DIS1 = 0
        'obj.DIS1 = False
        OBJ.DIS5 = OBJ.Width / 2
        'obj.DIS5 = False
    End If
'    Project.Item(ProjectIndex).cHeader.Width = obj.Width
'    Project.Item(ProjectIndex).cHeader.DIS1 = obj.DIS1
'    Project.Item(ProjectIndex).cHeader.DIS2 = obj.DIS2
'    Project.Item(ProjectIndex).cHeader.DIS3 = obj.DIS3
'    Project.Item(ProjectIndex).cHeader.DIS4 = obj.DIS4
'    Project.Item(ProjectIndex).cHeader.DIS5 = obj.DIS5
    'Set Project.Item(ProjectIndex).cHeader = OBJ
    
    With Project.Item(ProjectIndex).cHeader
        .Width = OBJ.Width
        .DIS1 = OBJ.DIS1
        .DIS2 = OBJ.DIS2
        .DIS3 = OBJ.DIS3
        .DIS4 = OBJ.DIS4
        .DIS5 = OBJ.DIS5
    End With
    
    Set OBJ = Nothing
    Project.Item(ProjectIndex).DataChanged = True
    Unload Me
End Sub

Private Sub cmdOK_DblClick(Cancel As MSForms.ReturnBoolean)
    Cancel = True
End Sub

Private Sub cmdRecommended_Click()
    OBJ.Width = txtSpan.Text
    OBJ.DIS1 = 0
    OBJ.DIS2 = OBJ.Width / 8
    OBJ.DIS3 = OBJ.Width / 4
    OBJ.DIS4 = OBJ.Width * 3 / 8
    OBJ.DIS5 = OBJ.Width / 2
    If OBJ.IOPTI = yes Then
        OBJ.DIS1 = 0
        OBJ.DIS1 = False
        OBJ.DIS5 = OBJ.Width / 2
        OBJ.DIS5 = False
    End If
    SetData
End Sub

Private Sub cmdRecommended_DblClick(Cancel As MSForms.ReturnBoolean)
    Cancel = True
End Sub

Private Sub Form_Load()
    ProjectIndex = ActiveProject
    IANA = Project.Item(ProjectIndex).cHeader.IANA
    Me.Move (Screen.Width - Me.Width) / 2, (Screen.Height - Me.Height) / 2
    cmdOK.Picture = LoadResPicture("ID_BITMAP_OK", 0)
    cmdCancel.Picture = LoadResPicture("ID_BITMAP_CANCEL", 0)
    Me.Caption = "Overall Span - [" & GetFileName(Project.Item(ProjectIndex).sFileName) & "]"
    GetData
End Sub

Private Sub Form_Resize()
    Dim i As Integer
    Me.Height = cmdOK.Top + cmdOK.Height + 600
    If IANA = 2 Then
        cmdOK.Top = txtSpan.Top + txtSpan.Height + 300
        cmdCancel.Top = cmdOK.Top
        Me.Height = cmdOK.Top + cmdOK.Height + 600
        'Me.Height = cmdOK.Top + cmdOK.Height + 200
        lbCrossSections.Visible = False
        For i = 1 To 5
            txtCrossSect(i).Visible = False
        Next i
        cmdRecommended.Visible = False
        lbUnit2.Visible = False
    End If
End Sub

Private Sub Form_Unload(Cancel As Integer)
    fMainForm.SetFocus
End Sub

Private Sub GetData()
    Set OBJ = Project.Item(ProjectIndex).cHeader.Clone
'    obj.WIDTH = Project.Item(ProjectIndex).cHeader.WIDTH
'    obj.DIS1 = Project.Item(ProjectIndex).cHeader.DIS1
'    obj.DIS2 = Project.Item(ProjectIndex).cHeader.DIS2
'    obj.DIS3 = Project.Item(ProjectIndex).cHeader.DIS3
'    obj.DIS4 = Project.Item(ProjectIndex).cHeader.DIS4
'    obj.DIS5 = Project.Item(ProjectIndex).cHeader.DIS5
'    obj.IOPTI = Project.Item(ProjectIndex).cHeader.IOPTI
    SetData
End Sub

Private Sub SetData()
    txtSpan.Text = OBJ.Width
    txtCrossSect(1).Text = OBJ.DIS1
    txtCrossSect(2).Text = OBJ.DIS2
    txtCrossSect(3).Text = OBJ.DIS3
    txtCrossSect(4).Text = OBJ.DIS4
    txtCrossSect(5).Text = OBJ.DIS5
    If OBJ.IOPTI = yes Then
        txtCrossSect(1).Text = 0
        txtCrossSect(1).Enabled = False
        txtCrossSect(5).Text = OBJ.Width / 2
        txtCrossSect(5).Enabled = False
    End If
End Sub

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
    Set OBJ = Nothing
End Sub

Private Sub txtCrossSect_GotFocus(index As Integer)
    txtCrossSect(index).SelStart = 0
    txtCrossSect(index).SelLength = Len(txtCrossSect(index).Text)
End Sub

Private Sub txtCrossSect_Validate(index As Integer, Cancel As Boolean)
    ValidateNumeric txtCrossSect(index), Cancel
    ValidateNullOrPozitive txtCrossSect(index), Cancel
    Dim i As Integer
    For i = 1 To 4
        If Val_(txtCrossSect(i + 1).Text) <= Val_(txtCrossSect(i).Text) Then
            Cancel = True
            MsgBox "Values not in the right order", vbCritical + vbOKOnly
        End If
    Next i
End Sub

Private Sub txtSpan_GotFocus()
    txtSpan.SelStart = 0
    txtSpan.SelLength = Len(txtSpan.Text)
End Sub

Private Sub txtSpan_Validate(Cancel As Boolean)
    ValidateNumeric txtSpan, Cancel
    ValidateNonNullPozitive txtSpan, Cancel
End Sub
