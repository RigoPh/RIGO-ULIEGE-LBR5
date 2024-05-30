VERSION 5.00
Begin VB.Form old_frmGlobal 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Gravity Center"
   ClientHeight    =   2670
   ClientLeft      =   6825
   ClientTop       =   3975
   ClientWidth     =   4665
   Icon            =   "old_frmGlobal.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   2670
   ScaleWidth      =   4665
   ShowInTaskbar   =   0   'False
   Begin VB.ComboBox Combo1 
      Appearance      =   0  'Flat
      Height          =   315
      Left            =   2760
      Style           =   2  'Dropdown List
      TabIndex        =   6
      Top             =   600
      Width           =   735
   End
   Begin VB.CommandButton Command2 
      Cancel          =   -1  'True
      Caption         =   "&Close"
      Height          =   345
      Left            =   3480
      TabIndex        =   5
      Top             =   2160
      Width           =   1125
   End
   Begin VB.CommandButton Command1 
      Caption         =   "&Apply"
      Default         =   -1  'True
      Height          =   345
      Left            =   2280
      TabIndex        =   4
      Top             =   2160
      Width           =   1125
   End
   Begin VB.TextBox Text4 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   2760
      TabIndex        =   3
      Text            =   "Text3"
      Top             =   1440
      Width           =   1455
   End
   Begin VB.TextBox Text3 
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   2760
      TabIndex        =   2
      Text            =   "Text3"
      Top             =   1080
      Width           =   1455
   End
   Begin VB.TextBox Text2 
      Appearance      =   0  'Flat
      Enabled         =   0   'False
      Height          =   285
      Left            =   720
      TabIndex        =   1
      Text            =   "Text2"
      Top             =   1200
      Width           =   1095
   End
   Begin VB.TextBox Text1 
      Appearance      =   0  'Flat
      Enabled         =   0   'False
      Height          =   285
      Left            =   720
      TabIndex        =   0
      Text            =   "Text1"
      Top             =   840
      Width           =   1095
   End
   Begin VB.Frame Frame1 
      Caption         =   "Global Coordinates Origin"
      Height          =   1695
      Left            =   120
      TabIndex        =   7
      ToolTipText     =   "Coordinates with regard to the end node of Panel 1"
      Top             =   240
      Width           =   2295
      Begin VB.Label Label4 
         Caption         =   "Zg = "
         Height          =   255
         Left            =   120
         TabIndex        =   12
         Top             =   960
         Width           =   495
      End
      Begin VB.Label Label3 
         Caption         =   "Yg = "
         Height          =   255
         Left            =   120
         TabIndex        =   11
         Top             =   600
         Width           =   495
      End
      Begin VB.Label Label2 
         AutoSize        =   -1  'True
         Caption         =   "(m)"
         Height          =   195
         Left            =   1800
         TabIndex        =   10
         Top             =   960
         Width           =   210
      End
      Begin VB.Label Label1 
         AutoSize        =   -1  'True
         Caption         =   "(m)"
         Height          =   195
         Left            =   1800
         TabIndex        =   9
         Top             =   600
         Width           =   210
      End
   End
   Begin VB.Frame Frame2 
      Caption         =   "Alowed Limits"
      Height          =   1695
      Left            =   2640
      TabIndex        =   8
      ToolTipText     =   "Min. and Max. alowed limits for the gravity center defined in KXY user frame"
      Top             =   240
      Width           =   1935
   End
End
Attribute VB_Name = "old_frmGlobal"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
'FIXIT: Option Base 1 is not supported in Visual Basic .NET                                FixIT90210ae-R9148-H1984
Option Base 1
Option Explicit
Dim i As Integer
Dim ProjectIndex As Integer

Private Sub Form_Load()

    ProjectIndex = ActiveProject
    Me.Move (Screen.Width - Me.Width) / 2, (Screen.Height - Me.Height) / 2
'    cmdOK.Picture = LoadResPicture("ID_BITMAP_OK", 0)
'    cmdCancel.Picture = LoadResPicture("ID_BITMAP_CANCEL", 0)
    Me.Caption = "Global Axis Origin & Gravity Center - [" & GetFileName(Project.Item(ProjectIndex).sFileName) & "]"

'Text1.Text = Format(header.XK, "##0.###0")
'Text2.Text = Format(header.YK, "##0.###0")
Text1.Text = Format(Project.Item(ProjectIndex).cHeader.YAxisOrigin, "##0.###0")
Text2.Text = Format(Project.Item(ProjectIndex).cHeader.ZAxisOrigin, "##0.###0")

'Project.Item(ProjectIndex).cHeader.YAxisOrigin
For i = 0 To 3
    Combo1.AddItem (i)
Next i

Select Case Project.Item(ProjectIndex).cHeader.cGlobalConstraints.GravityLimitRestriction  'header.IGRAV
    Case 0
        Combo1.ListIndex = 0
    Case 1
        Combo1.ListIndex = 1
    Case 2
        Combo1.ListIndex = 2
    Case 3
        Combo1.ListIndex = 3
    Case Else
        MsgBox "ERROR in input file; only 0,1,2,3 accepted; 0 value automaticaly chosen"
End Select

'Text3.Text = Format(header.KGMIN, "##0.###0")
'Text4.Text = Format(header.KGMAX, "##0.###0")

Text3.Text = Format(Project.Item(ProjectIndex).cHeader.cGlobalConstraints.MinGravityCenter, "##0.###0")
Text4.Text = Format(Project.Item(ProjectIndex).cHeader.cGlobalConstraints.MaxGravityCenter, "##0.###0")

End Sub

Private Sub Command1_Click()
'header.XK = Val_(Text1.Text)
'header.YK = Val_(Text2.Text)

Project.Item(ProjectIndex).cHeader.YAxisOrigin = Val_(Text1.Text)
Project.Item(ProjectIndex).cHeader.ZAxisOrigin = Val_(Text2.Text)

Select Case Combo1.ListIndex
    Case 0
        Project.Item(ProjectIndex).cHeader.cGlobalConstraints.GravityLimitRestriction = 0
    Case 1
        Project.Item(ProjectIndex).cHeader.cGlobalConstraints.GravityLimitRestriction = 1
    Case 2
        Project.Item(ProjectIndex).cHeader.cGlobalConstraints.GravityLimitRestriction = 2
    Case 3
        Project.Item(ProjectIndex).cHeader.cGlobalConstraints.GravityLimitRestriction = 3
End Select

'header.KGMIN = Val_(Text3.Text)
'header.KGMAX = Val_(Text4.Text)
Project.Item(ProjectIndex).cHeader.cGlobalConstraints.MinGravityCenter = Val_(Text3.Text)
Project.Item(ProjectIndex).cHeader.cGlobalConstraints.MaxGravityCenter = Val_(Text4.Text)
Project.Item(ProjectIndex).DataChanged = True
'frmGraphics.Redraw Hscroll_Temp, Vscroll_Temp
'Unload Me
End Sub

Private Sub Command2_Click()
Unload Me
End Sub

Private Sub Text1_Change()
If Not IsNumeric(Text1.Text) Then
   SendKeys "{BackSpace}"
End If

End Sub

Private Sub Text2_Change()
If Not IsNumeric(Text2.Text) Then
   SendKeys "{BackSpace}"
End If

End Sub

Private Sub Text3_Change()
If Not IsNumeric(Text3.Text) Then
   SendKeys "{BackSpace}"
End If

End Sub

Private Sub Text4_Change()
If Not IsNumeric(Text4.Text) Then
   SendKeys "{BackSpace}"
End If

End Sub
