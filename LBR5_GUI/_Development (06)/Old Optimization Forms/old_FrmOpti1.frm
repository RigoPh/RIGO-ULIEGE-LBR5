VERSION 5.00
Begin VB.Form old_FrmOpti1 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Design Variables"
   ClientHeight    =   4830
   ClientLeft      =   14115
   ClientTop       =   5520
   ClientWidth     =   6735
   Icon            =   "old_FrmOpti1.frx":0000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form2"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   4830
   ScaleWidth      =   6735
   ShowInTaskbar   =   0   'False
   Begin VB.ListBox List2 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      ForeColor       =   &H00C00000&
      Height          =   3345
      ItemData        =   "old_FrmOpti1.frx":000C
      Left            =   120
      List            =   "old_FrmOpti1.frx":000E
      MultiSelect     =   2  'Extended
      TabIndex        =   1
      Top             =   720
      Visible         =   0   'False
      Width           =   1335
   End
   Begin VB.Frame Frame1 
      Appearance      =   0  'Flat
      ForeColor       =   &H80000008&
      Height          =   3495
      Left            =   1560
      TabIndex        =   26
      Top             =   600
      Width           =   5055
      Begin VB.TextBox XIMAX_txt 
         Alignment       =   2  'Center
         Appearance      =   0  'Flat
         Height          =   285
         Index           =   9
         Left            =   3840
         TabIndex        =   19
         Top             =   3120
         Width           =   735
      End
      Begin VB.TextBox XIMAX_txt 
         Alignment       =   2  'Center
         Appearance      =   0  'Flat
         Height          =   285
         Index           =   8
         Left            =   3840
         TabIndex        =   17
         Top             =   2760
         Width           =   735
      End
      Begin VB.TextBox XIMAX_txt 
         Alignment       =   2  'Center
         Appearance      =   0  'Flat
         Height          =   285
         Index           =   7
         Left            =   3840
         TabIndex        =   15
         Top             =   2400
         Width           =   735
      End
      Begin VB.TextBox XIMAX_txt 
         Alignment       =   2  'Center
         Appearance      =   0  'Flat
         Height          =   285
         Index           =   6
         Left            =   3840
         TabIndex        =   13
         Top             =   2040
         Width           =   735
      End
      Begin VB.TextBox XIMAX_txt 
         Alignment       =   2  'Center
         Appearance      =   0  'Flat
         Height          =   285
         Index           =   5
         Left            =   3840
         TabIndex        =   11
         Top             =   1680
         Width           =   735
      End
      Begin VB.TextBox XIMAX_txt 
         Alignment       =   2  'Center
         Appearance      =   0  'Flat
         Height          =   285
         Index           =   4
         Left            =   3840
         TabIndex        =   9
         Top             =   1320
         Width           =   735
      End
      Begin VB.TextBox XIMAX_txt 
         Alignment       =   2  'Center
         Appearance      =   0  'Flat
         Height          =   285
         Index           =   3
         Left            =   3840
         TabIndex        =   7
         Top             =   960
         Width           =   735
      End
      Begin VB.TextBox XIMAX_txt 
         Alignment       =   2  'Center
         Appearance      =   0  'Flat
         Height          =   285
         Index           =   2
         Left            =   3840
         TabIndex        =   5
         Top             =   600
         Width           =   735
      End
      Begin VB.TextBox XIMAX_txt 
         Alignment       =   2  'Center
         Appearance      =   0  'Flat
         ForeColor       =   &H80000007&
         Height          =   285
         Index           =   1
         Left            =   3840
         TabIndex        =   3
         Top             =   240
         Width           =   735
      End
      Begin VB.TextBox XIMIN_txt 
         Alignment       =   2  'Center
         Appearance      =   0  'Flat
         Height          =   285
         Index           =   8
         Left            =   2160
         TabIndex        =   16
         Top             =   2760
         Width           =   735
      End
      Begin VB.TextBox XIMIN_txt 
         Alignment       =   2  'Center
         Appearance      =   0  'Flat
         Height          =   285
         Index           =   7
         Left            =   2160
         TabIndex        =   14
         Top             =   2400
         Width           =   735
      End
      Begin VB.TextBox XIMIN_txt 
         Alignment       =   2  'Center
         Appearance      =   0  'Flat
         Height          =   285
         Index           =   6
         Left            =   2160
         TabIndex        =   12
         Top             =   2040
         Width           =   735
      End
      Begin VB.TextBox XIMIN_txt 
         Alignment       =   2  'Center
         Appearance      =   0  'Flat
         Height          =   285
         Index           =   5
         Left            =   2160
         TabIndex        =   10
         Top             =   1680
         Width           =   735
      End
      Begin VB.TextBox XIMIN_txt 
         Alignment       =   2  'Center
         Appearance      =   0  'Flat
         Height          =   285
         Index           =   4
         Left            =   2160
         TabIndex        =   8
         Top             =   1320
         Width           =   735
      End
      Begin VB.TextBox XIMIN_txt 
         Alignment       =   2  'Center
         Appearance      =   0  'Flat
         Height          =   285
         Index           =   3
         Left            =   2160
         TabIndex        =   6
         Top             =   960
         Width           =   735
      End
      Begin VB.TextBox XIMIN_txt 
         Alignment       =   2  'Center
         Appearance      =   0  'Flat
         Height          =   285
         Index           =   2
         Left            =   2160
         TabIndex        =   4
         Top             =   600
         Width           =   735
      End
      Begin VB.TextBox XIMIN_txt 
         Alignment       =   2  'Center
         Appearance      =   0  'Flat
         Height          =   285
         Index           =   1
         Left            =   2160
         TabIndex        =   2
         Top             =   240
         Width           =   735
      End
      Begin VB.TextBox XIMIN_txt 
         Alignment       =   2  'Center
         Appearance      =   0  'Flat
         Height          =   285
         Index           =   9
         Left            =   2160
         TabIndex        =   18
         Top             =   3120
         Width           =   735
      End
      Begin VB.Label Label1 
         Alignment       =   2  'Center
         Appearance      =   0  'Flat
         BackColor       =   &H80000005&
         BackStyle       =   0  'Transparent
         BorderStyle     =   1  'Fixed Single
         Caption         =   "Label1"
         ForeColor       =   &H80000008&
         Height          =   285
         Index           =   9
         Left            =   3000
         TabIndex        =   46
         Top             =   3120
         Width           =   735
      End
      Begin VB.Label Label1 
         Alignment       =   2  'Center
         Appearance      =   0  'Flat
         BackColor       =   &H80000005&
         BackStyle       =   0  'Transparent
         BorderStyle     =   1  'Fixed Single
         Caption         =   "Label1"
         ForeColor       =   &H80000008&
         Height          =   285
         Index           =   8
         Left            =   3000
         TabIndex        =   45
         Top             =   2760
         Width           =   750
      End
      Begin VB.Label Label1 
         Alignment       =   2  'Center
         Appearance      =   0  'Flat
         BackColor       =   &H80000005&
         BackStyle       =   0  'Transparent
         BorderStyle     =   1  'Fixed Single
         Caption         =   "Label1"
         ForeColor       =   &H80000008&
         Height          =   285
         Index           =   7
         Left            =   3000
         TabIndex        =   44
         Top             =   2400
         Width           =   735
      End
      Begin VB.Label Label1 
         Alignment       =   2  'Center
         Appearance      =   0  'Flat
         BackColor       =   &H80000005&
         BackStyle       =   0  'Transparent
         BorderStyle     =   1  'Fixed Single
         Caption         =   "Label1"
         ForeColor       =   &H80000008&
         Height          =   285
         Index           =   6
         Left            =   3000
         TabIndex        =   43
         Top             =   2040
         Width           =   735
      End
      Begin VB.Label Label1 
         Alignment       =   2  'Center
         Appearance      =   0  'Flat
         BackColor       =   &H80000005&
         BackStyle       =   0  'Transparent
         BorderStyle     =   1  'Fixed Single
         Caption         =   "Label1"
         ForeColor       =   &H80000008&
         Height          =   285
         Index           =   5
         Left            =   3000
         TabIndex        =   42
         Top             =   1680
         Width           =   735
      End
      Begin VB.Label Label1 
         Alignment       =   2  'Center
         Appearance      =   0  'Flat
         BackColor       =   &H80000005&
         BackStyle       =   0  'Transparent
         BorderStyle     =   1  'Fixed Single
         Caption         =   "Label1"
         ForeColor       =   &H80000008&
         Height          =   285
         Index           =   4
         Left            =   3000
         TabIndex        =   41
         Top             =   1320
         Width           =   735
      End
      Begin VB.Label Label1 
         Alignment       =   2  'Center
         Appearance      =   0  'Flat
         BackColor       =   &H80000005&
         BackStyle       =   0  'Transparent
         BorderStyle     =   1  'Fixed Single
         Caption         =   "Label1"
         ForeColor       =   &H80000008&
         Height          =   285
         Index           =   3
         Left            =   3000
         TabIndex        =   40
         Top             =   960
         Width           =   735
      End
      Begin VB.Label Label1 
         Alignment       =   2  'Center
         Appearance      =   0  'Flat
         BackColor       =   &H80000005&
         BackStyle       =   0  'Transparent
         BorderStyle     =   1  'Fixed Single
         Caption         =   "Label1"
         ForeColor       =   &H80000008&
         Height          =   285
         Index           =   2
         Left            =   3000
         TabIndex        =   39
         Top             =   600
         Width           =   735
      End
      Begin VB.Label Label1 
         Alignment       =   2  'Center
         Appearance      =   0  'Flat
         BackColor       =   &H80000005&
         BackStyle       =   0  'Transparent
         BorderStyle     =   1  'Fixed Single
         Caption         =   "Label1"
         ForeColor       =   &H80000008&
         Height          =   285
         Index           =   1
         Left            =   3000
         TabIndex        =   38
         Top             =   240
         Width           =   735
      End
      Begin VB.Image SignErr 
         Height          =   255
         Index           =   1
         Left            =   4680
         Picture         =   "old_FrmOpti1.frx":0010
         Stretch         =   -1  'True
         Top             =   240
         Width           =   255
      End
      Begin VB.Image SignErr 
         Height          =   255
         Index           =   2
         Left            =   4680
         Picture         =   "old_FrmOpti1.frx":0452
         Stretch         =   -1  'True
         Top             =   600
         Width           =   255
      End
      Begin VB.Image SignErr 
         Height          =   255
         Index           =   3
         Left            =   4680
         Picture         =   "old_FrmOpti1.frx":0894
         Stretch         =   -1  'True
         Top             =   960
         Width           =   255
      End
      Begin VB.Image SignErr 
         Height          =   255
         Index           =   4
         Left            =   4680
         Picture         =   "old_FrmOpti1.frx":0CD6
         Stretch         =   -1  'True
         Top             =   1320
         Width           =   255
      End
      Begin VB.Image SignErr 
         Height          =   255
         Index           =   5
         Left            =   4680
         Picture         =   "old_FrmOpti1.frx":1118
         Stretch         =   -1  'True
         Top             =   1680
         Width           =   255
      End
      Begin VB.Image SignErr 
         Height          =   255
         Index           =   6
         Left            =   4680
         Picture         =   "old_FrmOpti1.frx":155A
         Stretch         =   -1  'True
         Top             =   2040
         Width           =   255
      End
      Begin VB.Image SignErr 
         Height          =   255
         Index           =   7
         Left            =   4680
         Picture         =   "old_FrmOpti1.frx":199C
         Stretch         =   -1  'True
         Top             =   2400
         Width           =   255
      End
      Begin VB.Image SignErr 
         Height          =   255
         Index           =   8
         Left            =   4680
         Picture         =   "old_FrmOpti1.frx":1DDE
         Stretch         =   -1  'True
         Top             =   2760
         Width           =   255
      End
      Begin VB.Image SignErr 
         Height          =   255
         Index           =   9
         Left            =   4680
         Picture         =   "old_FrmOpti1.frx":2220
         Stretch         =   -1  'True
         Top             =   3120
         Width           =   255
      End
      Begin VB.Label Title 
         AutoSize        =   -1  'True
         Caption         =   "2. Frames Web Height"
         Height          =   195
         Index           =   2
         Left            =   120
         TabIndex        =   34
         Top             =   600
         Width           =   1590
      End
      Begin VB.Label Title 
         AutoSize        =   -1  'True
         Caption         =   "3. Frames Web Thickness"
         Height          =   195
         Index           =   3
         Left            =   120
         TabIndex        =   33
         Top             =   960
         Width           =   1860
      End
      Begin VB.Label Title 
         AutoSize        =   -1  'True
         Caption         =   "4. Frames Flange Wide"
         Height          =   195
         Index           =   4
         Left            =   120
         TabIndex        =   32
         Top             =   1320
         Width           =   1635
      End
      Begin VB.Label Title 
         AutoSize        =   -1  'True
         Caption         =   "5. Frames Spacing"
         Height          =   195
         Index           =   5
         Left            =   120
         TabIndex        =   31
         Top             =   1680
         Width           =   1320
      End
      Begin VB.Label Title 
         AutoSize        =   -1  'True
         Caption         =   "6. Stiffeners Web Height"
         Height          =   195
         Index           =   6
         Left            =   120
         TabIndex        =   30
         Top             =   2040
         Width           =   1740
      End
      Begin VB.Label Title 
         AutoSize        =   -1  'True
         Caption         =   "7. Stiffeners Web Thickness"
         Height          =   195
         Index           =   7
         Left            =   120
         TabIndex        =   29
         Top             =   2400
         Width           =   2010
      End
      Begin VB.Label Title 
         AutoSize        =   -1  'True
         Caption         =   "8. Stiffeners Flange Wide"
         Height          =   195
         Index           =   8
         Left            =   120
         TabIndex        =   28
         Top             =   2760
         Width           =   1785
      End
      Begin VB.Image SignOk 
         Height          =   240
         Index           =   1
         Left            =   4680
         Picture         =   "old_FrmOpti1.frx":2662
         Stretch         =   -1  'True
         Top             =   240
         Width           =   240
      End
      Begin VB.Image SignOk 
         Height          =   240
         Index           =   2
         Left            =   4680
         Picture         =   "old_FrmOpti1.frx":2AA4
         Stretch         =   -1  'True
         Top             =   600
         Width           =   240
      End
      Begin VB.Image SignOk 
         Height          =   240
         Index           =   3
         Left            =   4680
         Picture         =   "old_FrmOpti1.frx":2EE6
         Stretch         =   -1  'True
         Top             =   960
         Width           =   240
      End
      Begin VB.Image SignOk 
         Height          =   240
         Index           =   4
         Left            =   4680
         Picture         =   "old_FrmOpti1.frx":3328
         Stretch         =   -1  'True
         Top             =   1320
         Width           =   240
      End
      Begin VB.Image SignOk 
         Height          =   240
         Index           =   5
         Left            =   4680
         Picture         =   "old_FrmOpti1.frx":376A
         Stretch         =   -1  'True
         Top             =   1680
         Width           =   240
      End
      Begin VB.Image SignOk 
         Height          =   240
         Index           =   6
         Left            =   4680
         Picture         =   "old_FrmOpti1.frx":3BAC
         Stretch         =   -1  'True
         Top             =   2040
         Width           =   240
      End
      Begin VB.Image SignOk 
         Height          =   240
         Index           =   7
         Left            =   4680
         Picture         =   "old_FrmOpti1.frx":3FEE
         Stretch         =   -1  'True
         Top             =   2400
         Width           =   240
      End
      Begin VB.Image SignOk 
         Height          =   240
         Index           =   8
         Left            =   4680
         Picture         =   "old_FrmOpti1.frx":4430
         Stretch         =   -1  'True
         Top             =   2760
         Width           =   240
      End
      Begin VB.Image SignOk 
         Height          =   240
         Index           =   9
         Left            =   4680
         Picture         =   "old_FrmOpti1.frx":4872
         Stretch         =   -1  'True
         Top             =   3120
         Width           =   240
      End
      Begin VB.Label Title 
         AutoSize        =   -1  'True
         Caption         =   "9. Stiffeners Spacing"
         Height          =   195
         Index           =   9
         Left            =   120
         TabIndex        =   27
         Top             =   3120
         Width           =   1470
      End
      Begin VB.Label Title 
         AutoSize        =   -1  'True
         Caption         =   "1. Plate Thickness"
         Height          =   195
         Index           =   1
         Left            =   120
         TabIndex        =   35
         Top             =   240
         Width           =   1320
      End
   End
   Begin VB.CommandButton cmdClose 
      Caption         =   "&Close"
      Height          =   345
      Left            =   5520
      TabIndex        =   21
      Top             =   4320
      Width           =   1125
   End
   Begin VB.CommandButton cmdApply 
      Caption         =   "&Apply"
      Default         =   -1  'True
      Height          =   345
      Left            =   4320
      TabIndex        =   20
      Top             =   4320
      Width           =   1125
   End
   Begin VB.ListBox List1 
      Height          =   3375
      ItemData        =   "old_FrmOpti1.frx":4CB4
      Left            =   120
      List            =   "old_FrmOpti1.frx":4CB6
      TabIndex        =   0
      Top             =   720
      Width           =   1335
   End
   Begin VB.Label Label9 
      Alignment       =   2  'Center
      AutoSize        =   -1  'True
      Caption         =   "(mm)"
      Height          =   195
      Left            =   5400
      TabIndex        =   49
      Top             =   360
      Width           =   735
   End
   Begin VB.Label Label8 
      Alignment       =   2  'Center
      AutoSize        =   -1  'True
      Caption         =   "(mm)"
      Height          =   195
      Left            =   4560
      TabIndex        =   48
      Top             =   360
      Width           =   735
   End
   Begin VB.Label Label3 
      Alignment       =   2  'Center
      AutoSize        =   -1  'True
      Caption         =   "(mm)"
      Height          =   195
      Left            =   3720
      TabIndex        =   47
      Top             =   360
      Width           =   735
   End
   Begin VB.Label Label2 
      Alignment       =   2  'Center
      AutoSize        =   -1  'True
      Caption         =   "<= X(i) <="
      Height          =   195
      Left            =   4560
      TabIndex        =   37
      Top             =   120
      Width           =   735
   End
   Begin VB.Label Label17 
      Height          =   495
      Left            =   240
      TabIndex        =   36
      Top             =   4200
      Width           =   2655
   End
   Begin VB.Label Label7 
      AutoSize        =   -1  'True
      Caption         =   "Panels: "
      Height          =   195
      Left            =   120
      TabIndex        =   25
      Top             =   360
      Width           =   570
   End
   Begin VB.Label Label6 
      Alignment       =   2  'Center
      AutoSize        =   -1  'True
      Caption         =   "XIMAX(i)"
      Height          =   195
      Left            =   5400
      TabIndex        =   24
      Top             =   120
      Width           =   735
   End
   Begin VB.Label Label5 
      Alignment       =   2  'Center
      AutoSize        =   -1  'True
      Caption         =   "XIMIN(i)"
      Height          =   195
      Left            =   3720
      TabIndex        =   23
      Top             =   120
      Width           =   735
   End
   Begin VB.Label Label4 
      AutoSize        =   -1  'True
      Caption         =   "NXI(i)"
      Height          =   195
      Left            =   2280
      TabIndex        =   22
      Top             =   120
      Width           =   390
   End
   Begin VB.Menu mnuCopy 
      Caption         =   "Copy"
      Begin VB.Menu CopySelection 
         Caption         =   "Copy Selection"
      End
   End
   Begin VB.Menu mnuPaste 
      Caption         =   "Paste"
      Begin VB.Menu PasteSelection 
         Caption         =   "Paste Selection"
      End
   End
End
Attribute VB_Name = "old_FrmOpti1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Base 1
Dim PanelIndex As Integer
Dim vctNBRXI() As Variant
Dim vctNXI() As Variant
Dim vctXIMIN() As Variant
Dim vctXIMAX() As Variant
Dim vctNBRXI1(1) As Variant
Dim vctNXI1(1) As Variant
Dim vctXIMIN1(1) As Variant
Dim vctXIMAX1(1) As Variant
Dim vctNBRXI2() As Variant
Dim vctNXI2() As Variant
Dim vctXIMIN2() As Variant
Dim vctXIMAX2() As Variant
Dim LeastSel As Boolean
Dim ProjectIndex As Integer
Dim NETO As Integer

Private Sub cmdApply_Click()
    Dim i As Integer
    Dim j As Integer
    Dim Count As Integer
    Dim Contor As Integer
    Count = 0
    Dim Cond1 As Boolean
    Dim Cond2 As Boolean
    Dim MSG As String
    'List1_Click
    XIMIN_txt_LostFocus 0
    XIMAX_txt_LostFocus 0
    CountRestrictions
    ' +++++++++++++++ verific sa nu am casute goale nepereche
    For i = 1 To 9
        If XIMIN_txt(i).Text <> "" And XIMAX_txt(i) = "" Then
            Cond1 = True
            SignErr(i).Visible = True
        End If
        If XIMIN_txt(i).Text = "" And XIMAX_txt(i) <> "" Then
            Cond2 = True
            SignErr(i).Visible = True
        End If
        If XIMIN_txt(i).Text <> "" And XIMAX_txt(i) <> "" Then
            SignErr(i).Visible = False
        End If
        If XIMIN_txt(i).Text = "" And XIMAX_txt(i) = "" Then
            SignErr(i).Visible = False
        End If
    Next i
    If Cond1 = True Or Cond2 = True Then
        MSG = MsgBox("Insufficient Data", vbCritical, "Error!")
        Exit Sub
    Exit Sub
    End If
    Cond1 = False: Cond2 = False
    ' -------------------------------------------------------
    Dim vctNXI3() As Variant
    Dim vctXIMIN3() As Variant
    Dim vctXIMAX3() As Variant
    Dim cDesignVariable As cDesignVariables
    For i = 1 To NETO
        If vctNBRXI(i) > 0 Then
            ReDim vctNXI3(1 To vctNBRXI(i))
            ReDim vctXIMIN3(1 To vctNBRXI(i))
            ReDim vctXIMAX3(1 To vctNBRXI(i))
            For Each cDesignVariable In Project.Item(ProjectIndex).colPanel.Item(i).colDesignVariables
                Project.Item(ProjectIndex).colPanel.Item(i).colDesignVariables.Remove cDesignVariable.index
            Next cDesignVariable
            
                Contor = 0
                For j = 1 To 9
                    If vctXIMIN(i)(j) = "" And vctXIMAX(i)(j) = "" Then
                        vctNXI(i)(j) = ""
                    End If
                Next j
                For j = 1 To 9
                    If vctNXI(i)(j) <> "" Then
                        Contor = Contor + 1
                        
                        Set cDesignVariable = New cDesignVariables
                        cDesignVariable.index = Contor
                        cDesignVariable.VariableName = j
                        cDesignVariable.LowerLimit = vctXIMIN(i)(j) / 1000
                        cDesignVariable.UpperLimit = vctXIMAX(i)(j) / 1000
                        Project.Item(ProjectIndex).colPanel.Item(i).colDesignVariables.Add cDesignVariable, cDesignVariable.index
'                        vctNXI3(Contor) = vctNXI(i)(j)
'                        vctXIMIN3(Contor) = vctXIMIN(i)(j) / 1000
'                        vctXIMAX3(Contor) = vctXIMAX(i)(j) / 1000
                    End If
                Next j
'                    Panel(i).NrNXI = vctNBRXI(i)
'                    Panel(i).NXI = vctNXI3
'                    Panel(i).XMIN = vctXIMIN3
'                    Panel(i).XMAX = vctXIMAX3
        Else
'                    Panel(i).NrNXI = 0
'                    Panel(i).NXI = Empty
'                    Panel(i).XMIN = Empty
'                    Panel(i).XMAX = Empty
                    For Each cDesignVariable In Project.Item(ProjectIndex).colPanel.Item(i).colDesignVariables
                        Project.Item(ProjectIndex).colPanel.Item(i).colDesignVariables.Remove cDesignVariable.index
                    Next cDesignVariable
        End If
    Next i
    Project.Item(ProjectIndex).DataChanged = True
End Sub

Private Sub cmdClose_Click()
    Unload Me
End Sub

Private Sub CopySelection_Click()
    Dim i As Integer
    List2.Clear
    For i = 1 To NETO
        List2.AddItem "Panel " & i
    Next i
    List2.Visible = True
    Label17.Caption = "Selection mode active; select panels to paste data or press Esc to exit."
    Label7.Caption = "Panel " & PanelIndex & " selected as source"
End Sub

Private Sub Form_KeyPress(KeyAscii As Integer)
    Dim i As Integer
    If KeyAscii = vbKeyEscape Then
        For i = 1 To 9
            Title(i).FontUnderline = False
        Next i
        List2.Visible = False
        Label7.Caption = "Panels:"
        Label17.Caption = ""
    End If
End Sub

Private Function GetDesignVariableValue(ByVal PanelIndex As Integer, ByVal VariableIndex As Integer) As Double
    With Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings
        Select Case VariableIndex
            Case 1
                GetDesignVariableValue = .NetThickness * 1000
            Case 2
                GetDesignVariableValue = .cPrimaryFrames.WebHeight * 1000
            Case 3
                GetDesignVariableValue = .cPrimaryFrames.WebThickness * 1000
            Case 4
                GetDesignVariableValue = .cPrimaryFrames.FlangeWidth * 1000
            Case 5
                GetDesignVariableValue = .cPrimaryFrames.Spacing * 1000
            Case 6
                GetDesignVariableValue = .cPrimaryStiffeners.WebHeight * 1000
            Case 7
                GetDesignVariableValue = .cPrimaryStiffeners.WebThickness * 1000
            Case 8
                GetDesignVariableValue = .cPrimaryStiffeners.FlangeWidth * 1000
            Case 9
                GetDesignVariableValue = .cPrimaryStiffeners.Spacing * 1000
        End Select
    End With
End Function

Private Sub PasteSelection_Click()
    Dim i As Integer
    Dim j As Integer
    Dim index As Integer
    Dim OldNBRXI As Integer

    Dim desvar As Double, bMsg As Boolean
    bMsg = False
    For i = 0 To NETO - 1
        index = 0
        OldNBRXI = vctNBRXI(i + 1)
        If List2.Selected(i) = True Then
            For j = 1 To 9
                If Title(j).FontUnderline = True Then
                    desvar = GetDesignVariableValue(i + 1, j)
                    If vctXIMIN(PanelIndex)(j) = "" Then
                        vctXIMIN(i + 1)(j) = ""
                        vctXIMAX(i + 1)(j) = ""
                    Else
                        If vctXIMIN(PanelIndex)(j) <= desvar And vctXIMAX(PanelIndex)(j) >= desvar Then
                            vctXIMIN(i + 1)(j) = vctXIMIN(PanelIndex)(j)
                            vctXIMAX(i + 1)(j) = vctXIMAX(PanelIndex)(j)
                        End If
                    End If
                    If vctXIMIN(PanelIndex)(j) <> "" Then
                        vctNXI(i + 1)(j) = j
                    Else
                        vctNXI(i + 1)(j) = ""
                    End If
                End If
            Next j
            For j = 1 To 9
                If vctNXI(i + 1)(j) <> "" Then
                    index = index + 1
                End If
                vctNBRXI(i + 1) = index
            Next j
        End If
    Next i
    For i = 1 To 9
        Title(i).FontUnderline = False
    Next i
    List2.Visible = False
    Label17.Caption = ""
    Label7.Caption = "Panels:"
    If bMsg = True Then MsgBox "!!!"
End Sub

Private Sub Form_Load()
    Dim i As Integer
    Dim j As Integer
    Dim k As Integer
    
    
    ProjectIndex = ActiveProject
    Me.Move (Screen.Width - Me.Width) / 2, (Screen.Height - Me.Height) / 2
'    cmdOK.Picture = LoadResPicture("ID_BITMAP_OK", 0)
'    cmdCancel.Picture = LoadResPicture("ID_BITMAP_CANCEL", 0)
    Me.Caption = "Design Variables - [" & GetFileName(Project.Item(ProjectIndex).sFileName) & "]"
    NETO = Project.Item(ProjectIndex).colPanel.Count
    Me.Height = 5415 '- 255
    mnuCopy.Visible = False
    mnuPaste.Visible = False
    ' signs
    For i = 1 To 9
        SignOk(i).Visible = False
        SignErr(i).Visible = False
    Next i
    For i = 1 To NETO
        'If Project.Item(ProjectIndex).colPanel.Item(i).pType = Plate Then
            List1.AddItem "Panel " & i
        'End If
    Next i
    ReDim vctNBRXI(1 To NETO)
    ReDim vctNXI(1 To NETO)
    ReDim vctXIMIN(1 To NETO)
    ReDim vctXIMAX(1 To NETO)
    ' Bag membrii in vectori
    Dim vctNXItmp() As Variant
    Dim vctXIMINtmp() As Variant
    Dim vctXIMAXtmp() As Variant
    For i = 1 To NETO
        vctNBRXI(i) = Project.Item(ProjectIndex).colPanel.Item(i).colDesignVariables.Count
        If Project.Item(ProjectIndex).colPanel.Item(i).colDesignVariables.Count > 0 Then
            ReDim vctNXItmp(1 To Project.Item(ProjectIndex).colPanel.Item(i).colDesignVariables.Count)
            ReDim vctXIMINtmp(1 To Project.Item(ProjectIndex).colPanel.Item(i).colDesignVariables.Count)
            ReDim vctXIMAXtmp(1 To Project.Item(ProjectIndex).colPanel.Item(i).colDesignVariables.Count)
            For j = 1 To Project.Item(ProjectIndex).colPanel.Item(i).colDesignVariables.Count
                vctNXItmp(j) = Project.Item(ProjectIndex).colPanel.Item(i).colDesignVariables.Item(j).VariableName
                vctXIMINtmp(j) = Project.Item(ProjectIndex).colPanel.Item(i).colDesignVariables.Item(j).LowerLimit
                vctXIMAXtmp(j) = Project.Item(ProjectIndex).colPanel.Item(i).colDesignVariables.Item(j).UpperLimit
            Next j
        Else
            ReDim vctNXItmp(1 To 1)
            ReDim vctXIMINtmp(1 To 1)
            ReDim vctXIMAXtmp(1 To 1)
            vctNXItmp(1) = 0
            vctXIMINtmp(1) = 0
            vctXIMAXtmp(1) = 0
        End If
        'vctNBRXI(i) = Panel(i).NrNXI
        vctNXI(i) = vctNXItmp
        vctXIMIN(i) = vctXIMINtmp
        vctXIMAX(i) = vctXIMAXtmp
    Next i
    ' vectori de 1 ramura cu valoarea 0 (pt a pastra dimensiunea vectorului principal)
    vctNXI1(1) = 0
    vctXIMIN1(1) = 0
    vctXIMAX1(1) = 0
    For i = 1 To NETO
        If IsEmpty(vctNXI(i)) Then
            vctNXI(i) = vctNXI1
        End If
        If IsEmpty(vctXIMIN(i)) Then
            vctXIMIN(i) = vctXIMIN1
        End If
        If IsEmpty(vctXIMAX(i)) Then
            vctXIMAX(i) = vctXIMAX1
        End If
    Next i
    ReDim vctNXI2(1 To 9)
    ReDim vctXIMIN2(1 To 9)
    ReDim vctXIMAX2(1 To 9)
    
    For i = 1 To NETO
        If Project.Item(ProjectIndex).colPanel.Item(i).colDesignVariables.Count > 0 Then
            For j = 1 To Project.Item(ProjectIndex).colPanel.Item(i).colDesignVariables.Count
                ReDim Preserve vctNXI2(1 To 9)
                ReDim Preserve vctXIMIN2(1 To 9)
                ReDim Preserve vctXIMAX2(1 To 9)
                vctNXI2(Project.Item(ProjectIndex).colPanel.Item(i).colDesignVariables.Item(j).VariableName) = Project.Item(ProjectIndex).colPanel.Item(i).colDesignVariables.Item(j).VariableName
                vctXIMIN2(Project.Item(ProjectIndex).colPanel.Item(i).colDesignVariables.Item(j).VariableName) = Project.Item(ProjectIndex).colPanel.Item(i).colDesignVariables.Item(j).LowerLimit
                vctXIMAX2(Project.Item(ProjectIndex).colPanel.Item(i).colDesignVariables.Item(j).VariableName) = Project.Item(ProjectIndex).colPanel.Item(i).colDesignVariables.Item(j).UpperLimit
                For k = 1 To 9
                    If IsEmpty(vctNXI2(k)) Then  ' Or vctNXI2(k) = 0
                        vctNXI2(k) = ""
                        vctXIMIN2(k) = ""
                        vctXIMAX2(k) = ""
                    End If
                Next k
            Next j
        Else
            ReDim Preserve vctNXI2(1 To 9)
            ReDim Preserve vctXIMIN2(1 To 9)
            ReDim Preserve vctXIMAX2(1 To 9)
            For j = 1 To 9
                vctNXI2(j) = ""
                vctXIMIN2(j) = ""
                vctXIMAX2(j) = ""
            Next j
        End If
        vctNXI(i) = vctNXI2
        vctXIMIN(i) = vctXIMIN2
        vctXIMAX(i) = vctXIMAX2
        ReDim vctNXI2(1 To 9)
        ReDim vctXIMIN2(1 To 9)
        ReDim vctXIMAX2(1 To 9)

    Next i

    For i = 1 To NETO
        For j = 1 To UBound(vctNXI(i))
            If vctXIMIN(i)(j) <> "" And vctXIMAX(i)(j) <> "" Then
                vctXIMIN(i)(j) = vctXIMIN(i)(j) * 1000
                vctXIMAX(i)(j) = vctXIMAX(i)(j) * 1000
            End If
        Next j
    Next i
    
    List1.ListIndex = 0
    
    
End Sub

Private Sub Frame1_MouseDown(Button As Integer, Shift As Integer, x As Single, y As Single)
    Dim i As Integer
    If Button = 2 Then
        CopySelection.Enabled = False
        For i = 1 To 9
            If Title(i).FontUnderline = True Then
                CopySelection.Enabled = True
            End If
        Next i
        PopupMenu mnuCopy
    End If
End Sub

Private Sub Title_MouseDown(index As Integer, Button As Integer, Shift As Integer, x As Single, y As Single)
    Dim i As Integer
    If Button = 1 Then
        'If XIMIN_txt(Index) <> "" And XIMAX_txt(Index) <> "" Then
            If Title(index).FontUnderline = True Then
                Title(index).FontUnderline = False
            Else
                Title(index).FontUnderline = True
            End If
        'End If
    End If
    If Button = 2 Then
        CopySelection.Enabled = False
        For i = 1 To 9
            If Title(i).FontUnderline = True Then
                CopySelection.Enabled = True
            End If
        Next i
        PopupMenu mnuCopy
    End If
End Sub

Private Sub Title_MouseUp(index As Integer, Button As Integer, Shift As Integer, x As Single, y As Single)
    Dim i As Integer
End Sub

Private Sub List1_Click()
    Dim i As Integer
    Dim Cond1 As Boolean
    Dim Cond2 As Boolean
    For i = 1 To 9
        If XIMIN_txt(i).Text <> "" And XIMAX_txt(i) = "" Then
            Cond1 = True
            SignErr(i).Visible = True
            SignOk(i).Visible = False
        End If
        If XIMIN_txt(i).Text = "" And XIMAX_txt(i) <> "" Then
            Cond2 = True
            SignErr(i).Visible = True
            SignOk(i).Visible = False
        End If
        If XIMIN_txt(i).Text <> "" And XIMAX_txt(i) <> "" Then
            SignErr(i).Visible = False
            SignOk(i).Visible = True
        End If
        If XIMIN_txt(i).Text = "" And XIMAX_txt(i) = "" Then
            SignErr(i).Visible = False
            SignOk(i).Visible = False
        End If
    Next i
    If Cond1 = True Or Cond2 = True Then
        'MsgBox "Insuficient Data", vbCritical, vbOKOnly
    List1.ListIndex = PanelIndex - 1
    Exit Sub
    End If
    Cond1 = False: Cond2 = False
    
    PanelIndex = List1.ListIndex + 1
    Select Case PanelIndex
        Case PanelIndex
            For i = 1 To 9
                XIMIN_txt(i).Text = vctXIMIN(PanelIndex)(i)
                XIMAX_txt(i).Text = vctXIMAX(PanelIndex)(i)
            Next i
    End Select
    
    For i = 1 To 9
        If XIMIN_txt(i).Text <> "" And XIMAX_txt(i) = "" Then
            SignErr(i).Visible = True
            SignOk(i).Visible = False
        End If
        If XIMIN_txt(i).Text = "" And XIMAX_txt(i) <> "" Then
            SignErr(i).Visible = True
            SignOk(i).Visible = False
        End If
        If XIMIN_txt(i).Text <> "" And XIMAX_txt(i) <> "" Then
            SignErr(i).Visible = False
            SignOk(i).Visible = True
        End If
        If XIMIN_txt(i).Text = "" And XIMAX_txt(i) = "" Then
            SignErr(i).Visible = False
            SignOk(i).Visible = False
        End If
    Next i
    For i = 1 To 9
        Title(i).FontUnderline = False
    Next i
    InitCheck PanelIndex
    Label1(1).Caption = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.NetThickness * 1000
    Label1(2).Caption = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryFrames.WebHeight * 1000
    Label1(3).Caption = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryFrames.WebThickness * 1000
    Label1(4).Caption = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryFrames.FlangeWidth * 1000
    Label1(5).Caption = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryFrames.Spacing * 1000
    Label1(6).Caption = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryStiffeners.WebHeight * 1000
    Label1(7).Caption = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryStiffeners.WebThickness * 1000
    Label1(8).Caption = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryStiffeners.FlangeWidth * 1000
    Label1(9).Caption = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryStiffeners.Spacing * 1000
    LimitTouched
End Sub

Private Sub LimitTouched()
    ' warning if limit touched
    Dim i As Integer
    For i = 1 To 9
        If Val_(Label1(i).Caption) = Val_(XIMIN_txt(i).Text) Then
            XIMIN_txt(i).ForeColor = vbRed
        Else
            XIMIN_txt(i).ForeColor = vbBlack
        End If
        If Val_(Label1(i).Caption) = Val_(XIMAX_txt(i).Text) Then
            XIMAX_txt(i).ForeColor = vbRed
        Else
            XIMAX_txt(i).ForeColor = vbBlack
        End If
    Next i
End Sub

Private Sub InitCheck(ByVal PanelIndex As Integer)
    Dim i As Integer
    Dim j As Integer
    ' reinitializez checkurile si texturile
    For i = 1 To 9
        XIMIN_txt(i).Text = ""
        XIMAX_txt(i).Text = ""
    Next i
    For i = 1 To 9
        XIMIN_txt(i).Text = vctXIMIN(PanelIndex)(i)
        XIMAX_txt(i).Text = vctXIMAX(PanelIndex)(i)
    Next i
End Sub

Private Sub List2_MouseDown(Button As Integer, Shift As Integer, x As Single, y As Single)
    If Button = 2 Then
        PopupMenu mnuPaste
    End If
End Sub

Private Sub XIMAX_txt_Change(index As Integer)
    If Not IsNumeric(XIMAX_txt(index).Text) Then
       SendKeys "{BackSpace}"
    End If
End Sub

Private Sub XIMIN_txt_Change(index As Integer)
    If Not IsNumeric(XIMIN_txt(index).Text) Then
       SendKeys "{BackSpace}"
    End If
End Sub

Private Sub XIMIN_txt_LostFocus(index As Integer)
    Dim i As Integer
    For i = 1 To 9
        If Val(XIMIN_txt(i)) > Val(Label1(i).Caption) And XIMIN_txt(i) <> "" Then
            MsgBox "Lower limit must be inferiour to the design variable value", vbCritical + vbOKOnly
            XIMIN_txt(i).Text = vctXIMIN(PanelIndex)(i)
        Else
            vctXIMIN(PanelIndex)(i) = XIMIN_txt(i).Text
        End If
    Next i
    For i = 1 To 9
        If vctXIMIN(PanelIndex)(i) = "" Then
            vctNXI(PanelIndex)(i) = ""
        Else
            vctNXI(PanelIndex)(i) = i
        End If
    Next i
    ' Sign Err & OK
    For i = 1 To 9
        If XIMIN_txt(i).Text <> "" And XIMAX_txt(i) = "" Then
            SignErr(i).Visible = False
            SignOk(i).Visible = False
        End If
        If XIMIN_txt(i).Text = "" And XIMAX_txt(i) <> "" Then
            SignErr(i).Visible = False
            SignOk(i).Visible = False
        End If
        If XIMIN_txt(i).Text <> "" And XIMAX_txt(i) <> "" Then
            SignErr(i).Visible = False
            SignOk(i).Visible = True
        End If
        If XIMIN_txt(i).Text = "" And XIMAX_txt(i) = "" Then
            SignErr(i).Visible = False
            SignOk(i).Visible = False
        End If
    Next i
    CountRestrictions
    LimitTouched
End Sub

Private Sub XIMAX_txt_LostFocus(index As Integer)
    Dim i As Integer
    For i = 1 To 9
        If Val(XIMAX_txt(i)) < Val(Label1(i).Caption) And XIMAX_txt(i) <> "" Then
            MsgBox "Upper limit must be superiour to the design variable value", vbCritical + vbOKOnly
            XIMAX_txt(i).Text = vctXIMAX(PanelIndex)(i)
        Else
            vctXIMAX(PanelIndex)(i) = XIMAX_txt(i).Text
        End If
    Next i
    For i = 1 To 9
        If vctXIMAX(PanelIndex)(i) = "" Then
            vctNXI(PanelIndex)(i) = ""
        Else
            vctNXI(PanelIndex)(i) = i
        End If
    Next i
    ' Sign Err
    For i = 1 To 9
        If XIMIN_txt(i).Text <> "" And XIMAX_txt(i) = "" Then
            SignErr(i).Visible = False
            SignOk(i).Visible = False
        End If
        If XIMIN_txt(i).Text = "" And XIMAX_txt(i) <> "" Then
            SignErr(i).Visible = False
            SignOk(i).Visible = False
        End If
        If XIMIN_txt(i).Text <> "" And XIMAX_txt(i) <> "" Then
            SignErr(i).Visible = False
            SignOk(i).Visible = True
        End If
        If XIMIN_txt(i).Text = "" And XIMAX_txt(i) = "" Then
            SignErr(i).Visible = False
            SignOk(i).Visible = False
        End If
    Next i
    CountRestrictions
    LimitTouched
End Sub

Private Sub CountRestrictions()
    Dim i As Integer
    Dim Cont As Integer
    Cont = 0
    For i = 1 To 9
        If XIMIN_txt(i).Text <> "" And XIMAX_txt(i) <> "" Then
            Cont = Cont + 1
        End If
    Next i
    vctNBRXI(PanelIndex) = Cont
End Sub
