VERSION 5.00
Object = "{0D452EE1-E08F-101A-852E-02608C4D0BB4}#2.0#0"; "FM20.DLL"
Begin VB.Form frmOptiGlobal 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Global Constraints"
   ClientHeight    =   5760
   ClientLeft      =   9315
   ClientTop       =   1920
   ClientWidth     =   4545
   Icon            =   "frmOptiGlobal.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   5760
   ScaleWidth      =   4545
   ShowInTaskbar   =   0   'False
   Begin VB.Frame frWeight 
      Appearance      =   0  'Flat
      BackColor       =   &H80000004&
      Caption         =   "Weight Constraint"
      ForeColor       =   &H80000008&
      Height          =   1215
      Left            =   2520
      TabIndex        =   29
      Top             =   1440
      Width           =   1935
      Begin VB.CheckBox chkWeight 
         Caption         =   "Maximum Weight"
         Height          =   195
         Left            =   120
         TabIndex        =   31
         Top             =   360
         Width           =   1695
      End
      Begin VB.TextBox txtWeight 
         Appearance      =   0  'Flat
         Height          =   285
         Left            =   120
         TabIndex        =   30
         Top             =   720
         Width           =   1095
      End
      Begin VB.Label lblWeight 
         AutoSize        =   -1  'True
         Caption         =   "[T]"
         Height          =   195
         Left            =   1320
         TabIndex        =   32
         Top             =   720
         Width           =   195
      End
   End
   Begin VB.Frame frCost 
      Appearance      =   0  'Flat
      BackColor       =   &H80000004&
      Caption         =   "Cost Constraint"
      ForeColor       =   &H80000008&
      Height          =   2175
      Left            =   2520
      TabIndex        =   22
      Top             =   2760
      Width           =   1935
      Begin VB.ComboBox cbCostModule 
         Height          =   315
         Left            =   120
         Style           =   2  'Dropdown List
         TabIndex        =   27
         Top             =   1680
         Width           =   1335
      End
      Begin VB.CheckBox chkCost 
         Caption         =   "Maximum Cost"
         Height          =   195
         Left            =   120
         TabIndex        =   24
         Top             =   360
         Width           =   1695
      End
      Begin VB.TextBox txtCost 
         Appearance      =   0  'Flat
         Height          =   285
         Left            =   120
         TabIndex        =   23
         Top             =   720
         Width           =   1095
      End
      Begin VB.Label lblCostModule 
         AutoSize        =   -1  'True
         Caption         =   "Cost Module:"
         Height          =   195
         Left            =   120
         TabIndex        =   28
         Top             =   1440
         Width           =   930
      End
      Begin VB.Label lblCost 
         AutoSize        =   -1  'True
         Caption         =   "[euros]"
         Height          =   195
         Left            =   1320
         TabIndex        =   25
         Top             =   720
         Width           =   480
      End
   End
   Begin VB.Frame frModulus 
      Appearance      =   0  'Flat
      BackColor       =   &H80000004&
      Caption         =   "Section Modulus Constraint"
      ForeColor       =   &H80000008&
      Height          =   2175
      Left            =   120
      TabIndex        =   17
      Top             =   2760
      Width           =   2295
      Begin VB.ComboBox cbPanel 
         Height          =   315
         Left            =   120
         Style           =   2  'Dropdown List
         TabIndex        =   21
         Top             =   1680
         Width           =   1095
      End
      Begin VB.CheckBox chkModulus 
         Caption         =   " Minimum Section Modulus"
         Height          =   375
         Left            =   120
         TabIndex        =   19
         Top             =   240
         Width           =   1695
      End
      Begin VB.TextBox txtModulus 
         Appearance      =   0  'Flat
         Height          =   285
         Left            =   120
         TabIndex        =   18
         Top             =   720
         Width           =   1095
      End
      Begin VB.Label lblZPanel 
         Caption         =   "Vertical Distance From Section Centroid:"
         Height          =   435
         Left            =   120
         TabIndex        =   26
         Top             =   1200
         Width           =   1635
      End
      Begin VB.Label lblModulus 
         AutoSize        =   -1  'True
         Caption         =   "[m3]"
         Height          =   195
         Left            =   1320
         TabIndex        =   20
         Top             =   720
         Width           =   300
      End
   End
   Begin VB.Frame frInertia 
      Appearance      =   0  'Flat
      BackColor       =   &H80000004&
      Caption         =   "Inertia Constraint"
      ForeColor       =   &H80000008&
      Height          =   1215
      Left            =   2520
      TabIndex        =   13
      Top             =   120
      Width           =   1935
      Begin VB.TextBox txtInertia 
         Appearance      =   0  'Flat
         Height          =   285
         Left            =   120
         TabIndex        =   15
         Top             =   720
         Width           =   1095
      End
      Begin VB.CheckBox chkInertia 
         Caption         =   " Minimum Inertia"
         Height          =   195
         Left            =   120
         TabIndex        =   14
         Top             =   360
         Width           =   1455
      End
      Begin VB.Label lblInertia 
         AutoSize        =   -1  'True
         Caption         =   "[m4]"
         Height          =   195
         Left            =   1320
         TabIndex        =   16
         Top             =   720
         Width           =   300
      End
   End
   Begin VB.Frame frGravityLimits 
      Appearance      =   0  'Flat
      Caption         =   "Gravity Center Limits"
      ForeColor       =   &H80000008&
      Height          =   2535
      Left            =   120
      TabIndex        =   6
      Top             =   120
      Width           =   2295
      Begin VB.TextBox txtUpperLimit 
         Appearance      =   0  'Flat
         Height          =   285
         Left            =   1080
         TabIndex        =   8
         Top             =   2040
         Width           =   735
      End
      Begin VB.TextBox txtLowerLimit 
         Appearance      =   0  'Flat
         Height          =   285
         Left            =   1080
         TabIndex        =   7
         Top             =   1680
         Width           =   735
      End
      Begin VB.OptionButton obLimits 
         Appearance      =   0  'Flat
         Caption         =   "Both Limits Active"
         ForeColor       =   &H80000008&
         Height          =   195
         Index           =   3
         Left            =   120
         TabIndex        =   3
         Top             =   1080
         Width           =   1935
      End
      Begin VB.OptionButton obLimits 
         Appearance      =   0  'Flat
         Caption         =   "No Active Limits"
         ForeColor       =   &H80000008&
         Height          =   195
         Index           =   0
         Left            =   120
         TabIndex        =   0
         Top             =   360
         Width           =   1935
      End
      Begin VB.OptionButton obLimits 
         Appearance      =   0  'Flat
         Caption         =   "Lower Limit Active"
         ForeColor       =   &H80000008&
         Height          =   195
         Index           =   1
         Left            =   120
         TabIndex        =   1
         Top             =   600
         Width           =   1935
      End
      Begin VB.OptionButton obLimits 
         Appearance      =   0  'Flat
         Caption         =   "Upper Limit Active"
         ForeColor       =   &H80000008&
         Height          =   195
         Index           =   2
         Left            =   120
         TabIndex        =   2
         Top             =   840
         Width           =   1935
      End
      Begin VB.Label lblLowerLimit 
         AutoSize        =   -1  'True
         Caption         =   "Lower Limit:"
         Height          =   255
         Left            =   120
         TabIndex        =   12
         Top             =   1680
         Width           =   840
      End
      Begin VB.Label lblUpperLimit 
         AutoSize        =   -1  'True
         Caption         =   "Upper Limit:"
         Height          =   195
         Left            =   120
         TabIndex        =   11
         Top             =   2040
         Width           =   840
      End
      Begin VB.Label lblUnit2 
         AutoSize        =   -1  'True
         Caption         =   "[m]"
         Height          =   195
         Left            =   1920
         TabIndex        =   10
         Top             =   2040
         Width           =   210
      End
      Begin VB.Label lblUnit1 
         AutoSize        =   -1  'True
         Caption         =   "[m]"
         Height          =   195
         Left            =   1920
         TabIndex        =   9
         Top             =   1680
         Width           =   210
      End
   End
   Begin MSForms.CommandButton cmdCancel 
      Cancel          =   -1  'True
      Height          =   375
      Left            =   3360
      TabIndex        =   5
      TabStop         =   0   'False
      Top             =   5280
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
      Left            =   2160
      TabIndex        =   4
      TabStop         =   0   'False
      Top             =   5280
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
Attribute VB_Name = "frmOptiGlobal"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Dim ProjectIndex As Integer
Dim dBuffer As Double

Private Sub chkCost_Click()
        Select Case chkCost
            Case vbUnchecked
                txtCost.Enabled = False
                lblCost.Enabled = False
                lblCostModule.Enabled = False
                cbCostModule.Enabled = False
            Case vbChecked
                txtCost.Enabled = True
                lblCost.Enabled = True
                lblCostModule.Enabled = True
                cbCostModule.Enabled = True
        End Select
End Sub

Private Sub chkInertia_Click()
    Select Case chkInertia
        Case vbUnchecked
            txtInertia.Enabled = False
            lblInertia.Enabled = False
        Case vbChecked
            txtInertia.Enabled = True
            lblInertia.Enabled = True
    End Select
End Sub

Private Sub chkModulus_Click()
    Select Case chkModulus
        Case vbUnchecked
            txtModulus.Enabled = False
            lblModulus.Enabled = False
            lblZPanel.Enabled = False
            cbPanel.Enabled = False
        Case vbChecked
            txtModulus.Enabled = True
            lblModulus.Enabled = True
            lblZPanel.Enabled = True
            cbPanel.Enabled = True
    End Select
End Sub

Private Sub chkWeight_Click()
    Select Case chkWeight
        Case vbUnchecked
            txtWeight.Enabled = False
            lblWeight.Enabled = False
        Case vbChecked
            txtWeight.Enabled = True
            lblWeight.Enabled = True
    End Select
End Sub

Private Sub cmdCancel_Click()
    Unload Me
End Sub

Private Sub cmdOK_Click()
    'MsgBox Project.Item(ProjectIndex).Zmin & " - " & Project.Item(ProjectIndex).Zmax
    If SetData = True Then Exit Sub
    Project.Item(ProjectIndex).DataChanged = True
    Unload Me
End Sub

Private Sub Form_Load()
    ProjectIndex = ActiveProject
    Me.Move (Screen.Width - Me.Width) / 2, (Screen.Height - Me.Height) / 2
    cmdOK.Picture = LoadResPicture("ID_BITMAP_OK", 0)
    cmdCancel.Picture = LoadResPicture("ID_BITMAP_CANCEL", 0)
    Me.Caption = "Global Constraints - [" & GetFileName(Project.Item(ProjectIndex).sFileName) & "]"
    PopulateCombo
    GetData
End Sub

Private Sub PopulateCombo()
    Dim i As Integer
    For i = 1 To Project.Item(ProjectIndex).colPanel.Count
        cbPanel.AddItem "Panel " & i
    Next i
    cbCostModule.AddItem "Simplified"
    If Licensing.IS_COSTCAT = True Then
        cbCostModule.AddItem "Developped"
    End If
End Sub

Private Sub GetData()
    With Project.Item(ProjectIndex).cHeader.cGlobalConstraints
        Select Case .GravityLimitRestriction
            Case .GravityLimitRestriction
                obLimits.Item(Project.Item(ProjectIndex).cHeader.cGlobalConstraints.GravityLimitRestriction) = True
        End Select
        txtLowerLimit.Text = .MinGravityCenter
        txtUpperLimit.Text = .MaxGravityCenter
        'Inertia Constraint
        Select Case .IsInertia
            Case no
                chkInertia = vbUnchecked
                txtInertia.Enabled = False
                lblInertia.Enabled = False
            Case yes
                chkInertia = vbChecked
                txtInertia.Enabled = True
                lblInertia.Enabled = True
        End Select
        txtInertia.Text = .Inertia
        'Weight Constraint
        Select Case .IsWeight
            Case no
                chkWeight = vbUnchecked
                txtWeight.Enabled = False
                lblWeight.Enabled = False
            Case yes
                chkWeight = vbChecked
                txtWeight.Enabled = True
                lblWeight.Enabled = True
        End Select
        txtWeight.Text = .Weight / 10000
        'Section Modulus Constraint
        Select Case .IsSectionModulus
            Case no
                chkModulus = vbUnchecked
                txtModulus.Enabled = False
                lblModulus.Enabled = False
                lblZPanel.Enabled = False
                cbPanel.Enabled = False
            Case yes
                chkModulus = vbChecked
                txtModulus.Enabled = True
                lblModulus.Enabled = True
                lblZPanel.Enabled = True
                cbPanel.Enabled = True
        End Select
        txtModulus = .SectionModulus
        If .ZPanel <= 0 Then .ZPanel = 1
        cbPanel.Text = "Panel " & .ZPanel
        'Cost Constraint
        Select Case .IsCost
            Case no
                chkCost = vbUnchecked
                txtCost.Enabled = False
                lblCost.Enabled = False
                cbCostModule.Enabled = False
                lblCostModule.Enabled = False
            Case yes
                chkCost = vbChecked
                txtCost.Enabled = True
                lblCost.Enabled = True
                cbCostModule.Enabled = True
                lblCostModule.Enabled = True
        End Select
        txtCost = .Cost
        Select Case .CostType
            Case 1
                cbCostModule.Text = "Simplified"
            Case 2
                cbCostModule.Text = "Developped"
            Case Else
                cbCostModule.Text = "Simplified"
        End Select
    End With
End Sub

Private Function SetData() As Boolean
    Dim i As Integer, index As Integer
    Dim v() As Variant, sLine As String
    For i = 0 To obLimits.Count - 1
        If obLimits.Item(i) = True Then
            index = i
            Exit For
        End If
    Next i
    Dim ZMin As Double, ZMax As Double
    ZMin = -Project.Item(ProjectIndex).ZMax
    ZMax = -Project.Item(ProjectIndex).ZMin
    ValidateNumeric txtLowerLimit, SetData
    ValidateNumeric txtUpperLimit, SetData
    If SetData = True Then Exit Function
    Select Case index
        Case 0
        Case 1
            If CDbl(txtLowerLimit.Text) < ZMin Or CDbl(txtLowerLimit.Text) > ZMax Then
                MsgBox "Gravity Center limits exceed model limits.", vbCritical + vbOKOnly
                SetData = True
                If SetData = True Then Exit Function
            End If
        Case 2
            If CDbl(txtUpperLimit.Text) < ZMin Or CDbl(txtUpperLimit.Text) > ZMax Then
                MsgBox "Gravity Center limits exceed model limits.", vbCritical + vbOKOnly
                SetData = True
                If SetData = True Then Exit Function
            End If
        Case 3
            If CDbl(txtLowerLimit.Text) < ZMin Or CDbl(txtLowerLimit.Text) > ZMax Then
                MsgBox "Gravity Center limits exceed model limits.", vbCritical + vbOKOnly
                SetData = True
                If SetData = True Then Exit Function
            End If
            If CDbl(txtUpperLimit.Text) < ZMin Or CDbl(txtUpperLimit.Text) > ZMax Then
                MsgBox "Gravity Center limits exceed model limits.", vbCritical + vbOKOnly
                SetData = True
                If SetData = True Then Exit Function
            End If
            If CDbl(txtLowerLimit.Text) > CDbl(txtUpperLimit.Text) Then
                MsgBox "Invalid Data.", vbCritical + vbOKOnly
                SetData = True
                Exit Function
            End If
    End Select
    
    
    If SetData = True Then Exit Function
    With Project.Item(ProjectIndex).cHeader.cGlobalConstraints
        .GravityLimitRestriction = index
        .MinGravityCenter = CDbl(txtLowerLimit.Text)
        .MaxGravityCenter = CDbl(txtUpperLimit.Text)
        'Inertia
        Select Case chkInertia
            Case vbUnchecked
                .IsInertia = no
            Case vbChecked
                .IsInertia = yes
        End Select
        .Inertia = CDbl(txtInertia.Text)
        Select Case chkWeight
            Case vbUnchecked
                .IsWeight = no
            Case vbChecked
                .IsWeight = yes
        End Select
        .Weight = CDbl(txtWeight.Text) * 10000
        'Section Modulus
        Select Case chkModulus
            Case vbUnchecked
                .IsSectionModulus = no
            Case vbChecked
                .IsSectionModulus = yes
        End Select
        .SectionModulus = CDbl(txtModulus.Text)
        sLine = cbPanel.Text
        GetValues 2, sLine, v
        .ZPanel = CInt(v(2))
        'Cost
        Select Case chkCost
            Case vbUnchecked
                .IsCost = no
            Case vbChecked
                .IsCost = yes
        End Select
        .Cost = CDbl(txtCost.Text)
        Select Case cbCostModule.Text
            Case "Simplified"
                .CostType = 1
            Case "Developped"
                .CostType = 2
        End Select
    End With
End Function

Private Sub Form_Resize()
    Me.Height = cmdOK.Top + cmdOK.Height + 600
End Sub

Private Sub obLimits_Click(index As Integer)
    Select Case index
        Case 0
            txtLowerLimit.Enabled = False
            txtUpperLimit.Enabled = False
            lblLowerLimit.Enabled = False
            lblUpperLimit.Enabled = False
            lblUnit1.Enabled = False
            lblUnit2.Enabled = False
        Case 1
            txtLowerLimit.Enabled = True
            txtUpperLimit.Enabled = False
            lblLowerLimit.Enabled = True
            lblUpperLimit.Enabled = False
            lblUnit1.Enabled = True
            lblUnit2.Enabled = False
        Case 2
            txtLowerLimit.Enabled = False
            txtUpperLimit.Enabled = True
            lblLowerLimit.Enabled = False
            lblUpperLimit.Enabled = True
            lblUnit1.Enabled = False
            lblUnit2.Enabled = True
        Case 3
            txtLowerLimit.Enabled = True
            txtUpperLimit.Enabled = True
            lblLowerLimit.Enabled = True
            lblUpperLimit.Enabled = True
            lblUnit1.Enabled = True
            lblUnit2.Enabled = True
    End Select
End Sub

Private Sub txtCost_GotFocus()
    txtCost.SelStart = 0
    txtCost.SelLength = Len(txtCost.Text)
    dBuffer = CDbl(txtCost.Text)
End Sub

Private Sub txtCost_Validate(Cancel As Boolean)
    ValidateNullOrPozitive txtCost, Cancel
    If Cancel = True Then txtCost = dBuffer
End Sub

Private Sub txtInertia_GotFocus()
    txtInertia.SelStart = 0
    txtInertia.SelLength = Len(txtInertia.Text)
    dBuffer = CDbl(txtInertia.Text)
End Sub

Private Sub txtInertia_Validate(Cancel As Boolean)
    ValidateNullOrPozitive txtInertia, Cancel
    If Cancel = True Then txtInertia = dBuffer
End Sub

Private Sub txtLowerLimit_GotFocus()
    txtLowerLimit.SelStart = 0
    txtLowerLimit.SelLength = Len(txtLowerLimit.Text)
    dBuffer = CDbl(txtLowerLimit.Text)
End Sub

Private Sub txtLowerLimit_Validate(Cancel As Boolean)
    ValidateNumeric txtLowerLimit, Cancel
    If Cancel = True Then txtLowerLimit = dBuffer
End Sub

Private Sub txtModulus_GotFocus()
    txtModulus.SelStart = 0
    txtModulus.SelLength = Len(txtModulus.Text)
    dBuffer = CDbl(txtModulus.Text)
End Sub

Private Sub txtModulus_Validate(Cancel As Boolean)
    ValidateNullOrPozitive txtModulus, Cancel
    If Cancel = True Then txtModulus = dBuffer
End Sub

Private Sub txtUpperLimit_GotFocus()
    txtUpperLimit.SelStart = 0
    txtUpperLimit.SelLength = Len(txtUpperLimit.Text)
    dBuffer = CDbl(txtUpperLimit.Text)
End Sub

Private Sub txtUpperLimit_Validate(Cancel As Boolean)
    ValidateNumeric txtUpperLimit, Cancel
    If Cancel = True Then txtUpperLimit = dBuffer
End Sub

Private Sub txtWeight_GotFocus()
    txtWeight.SelStart = 0
    txtWeight.SelLength = Len(txtWeight.Text)
    dBuffer = CDbl(txtWeight.Text)
End Sub

Private Sub txtWeight_Validate(Cancel As Boolean)
    ValidateNullOrPozitive txtWeight, Cancel
    If Cancel = True Then txtWeight = dBuffer
End Sub
