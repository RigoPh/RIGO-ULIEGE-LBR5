VERSION 5.00
Object = "{831FDD16-0C5C-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCTL.OCX"
Object = "{0D452EE1-E08F-101A-852E-02608C4D0BB4}#2.0#0"; "FM20.DLL"
Begin VB.Form frmOutput 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Output"
   ClientHeight    =   3960
   ClientLeft      =   4890
   ClientTop       =   3630
   ClientWidth     =   4140
   Icon            =   "frmOutput.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   3960
   ScaleWidth      =   4140
   ShowInTaskbar   =   0   'False
   Begin VB.Frame frOutputLevel 
      Appearance      =   0  'Flat
      Caption         =   "Output Level"
      ForeColor       =   &H80000008&
      Height          =   3135
      Left            =   120
      TabIndex        =   6
      Top             =   120
      Width           =   3855
      Begin VB.CheckBox ckStressesInFrames 
         Caption         =   "Stresses in the frames"
         Height          =   255
         Left            =   120
         TabIndex        =   1
         Top             =   1920
         Width           =   2295
      End
      Begin VB.CheckBox ckStressesInLongitudinals 
         Caption         =   "Stresses in the longitudinals"
         Height          =   255
         Left            =   120
         TabIndex        =   2
         Top             =   2280
         Width           =   2655
      End
      Begin VB.CheckBox ckAdditionalDebugging 
         Caption         =   "Additional debugging parameters"
         Height          =   255
         Left            =   120
         TabIndex        =   3
         Top             =   2640
         Width           =   2655
      End
      Begin MSComctlLib.Slider slOutputLevel 
         Height          =   495
         Left            =   120
         TabIndex        =   0
         Top             =   360
         Width           =   3495
         _ExtentX        =   6165
         _ExtentY        =   873
         _Version        =   393216
         LargeChange     =   1
         Min             =   1
         Max             =   5
         SelStart        =   1
         Value           =   1
         TextPosition    =   1
      End
      Begin VB.Label lbOutputLevelDescr 
         Appearance      =   0  'Flat
         Caption         =   "lbOutputLevelDescr"
         ForeColor       =   &H80000008&
         Height          =   855
         Left            =   240
         TabIndex        =   7
         Top             =   960
         Width           =   3375
      End
   End
   Begin MSForms.CommandButton cmdCancel 
      Cancel          =   -1  'True
      Height          =   375
      Left            =   2880
      TabIndex        =   5
      Top             =   3360
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
      Left            =   1680
      TabIndex        =   4
      Top             =   3360
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
Attribute VB_Name = "frmOutput"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Dim ProjectIndex As Integer
Private OBJ As cHeader

Private Sub cmdCancel_Click()
    Unload Me
End Sub

Private Sub cmdCancel_DblClick(Cancel As MSForms.ReturnBoolean)
    Cancel = True
End Sub

Private Sub cmdOK_Click()
    OBJ.IMPR2 = slOutputLevel.Value - 4
    OBJ.IMPR = ckAdditionalDebugging.Value
    OBJ.INDAIG = ckStressesInFrames.Value
    OBJ.INDRAID = ckStressesInLongitudinals.Value
    Set Project.Item(ProjectIndex).cHeader = OBJ
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
    Me.Caption = "Output - [" & GetFileName(Project.Item(ProjectIndex).sFileName) & "]"
    GetData
    slOutputLevel_Change
End Sub

Private Sub Form_Resize()
    Me.Height = cmdOK.Top + cmdOK.Height + 600
End Sub

Private Sub Form_Unload(Cancel As Integer)
    fMainForm.SetFocus
End Sub

Private Sub GetData()
    Set OBJ = Project.Item(ProjectIndex).cHeader
    slOutputLevel.Value = OBJ.IMPR2 + 4
    ckAdditionalDebugging.Value = OBJ.IMPR
    ckStressesInLongitudinals.Value = OBJ.INDRAID
    ckStressesInFrames.Value = OBJ.INDAIG
End Sub

Private Sub slOutputLevel_Change()
    Select Case slOutputLevel.Value
        Case 1
            lbOutputLevelDescr.Caption = "Displacements only."
        Case 2
            lbOutputLevelDescr.Caption = "Displacements, rotations and stresses at mid-plate."
        Case 3
            lbOutputLevelDescr.Caption = "Displacements, rotations, stresses at mid-plate" & _
            " and stresses in plate upper/lower string."
        Case 4
            lbOutputLevelDescr.Caption = "Displacements, rotations, stresses at mid-plate" & _
            ", stresses in plate upper/lower string, unitary forces and moments."
        Case 5
            lbOutputLevelDescr.Caption = "Displacements, rotations, stresses at mid-plate" & _
            ", stresses in plate upper/lower string, unitary forces and moments," & _
            "check of forces equilibrium, additional outputs."
    End Select
End Sub

