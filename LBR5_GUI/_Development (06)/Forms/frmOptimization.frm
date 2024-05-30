VERSION 5.00
Object = "{0D452EE1-E08F-101A-852E-02608C4D0BB4}#2.0#0"; "FM20.DLL"
Begin VB.Form frmOptimization 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Optimization"
   ClientHeight    =   4305
   ClientLeft      =   5565
   ClientTop       =   3225
   ClientWidth     =   3225
   Icon            =   "frmOptimization.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   4305
   ScaleWidth      =   3225
   ShowInTaskbar   =   0   'False
   Begin VB.Frame frMultiobjective 
      Appearance      =   0  'Flat
      Caption         =   "Multiobjective"
      ForeColor       =   &H80000008&
      Height          =   2055
      Left            =   120
      TabIndex        =   10
      Top             =   1440
      Width           =   3015
      Begin VB.OptionButton opMultiObjCost 
         Appearance      =   0  'Flat
         Caption         =   "Developped"
         ForeColor       =   &H80000008&
         Height          =   195
         Index           =   1
         Left            =   1080
         TabIndex        =   20
         Top             =   1760
         Width           =   1215
      End
      Begin VB.OptionButton opMultiObjCost 
         Appearance      =   0  'Flat
         Caption         =   "Simplified"
         ForeColor       =   &H80000008&
         Height          =   195
         Index           =   0
         Left            =   1080
         TabIndex        =   19
         Top             =   1440
         Width           =   1215
      End
      Begin VB.TextBox txtInertiaWeight 
         Appearance      =   0  'Flat
         Height          =   285
         Left            =   2040
         TabIndex        =   14
         Top             =   960
         Width           =   855
      End
      Begin VB.TextBox txtWeightWeight 
         Appearance      =   0  'Flat
         Height          =   285
         Left            =   1080
         TabIndex        =   13
         Top             =   960
         Width           =   855
      End
      Begin VB.TextBox txtCostWeight 
         Appearance      =   0  'Flat
         Height          =   285
         Left            =   120
         TabIndex        =   11
         Top             =   960
         Width           =   855
      End
      Begin VB.Label Label5 
         AutoSize        =   -1  'True
         Caption         =   "Cost Type:"
         Height          =   195
         Left            =   120
         TabIndex        =   18
         Top             =   1440
         Width           =   765
      End
      Begin VB.Label Label4 
         AutoSize        =   -1  'True
         Caption         =   "Inertia"
         Height          =   255
         Left            =   2040
         TabIndex        =   17
         Top             =   720
         Width           =   435
      End
      Begin VB.Label Label3 
         AutoSize        =   -1  'True
         Caption         =   "Weight"
         Height          =   195
         Left            =   1080
         TabIndex        =   16
         Top             =   720
         Width           =   510
      End
      Begin VB.Label Label2 
         AutoSize        =   -1  'True
         Caption         =   "Cost"
         Height          =   195
         Left            =   120
         TabIndex        =   15
         Top             =   720
         Width           =   315
      End
      Begin VB.Label Label1 
         AutoSize        =   -1  'True
         Caption         =   "Objective Functions Weights:"
         Height          =   195
         Left            =   120
         TabIndex        =   12
         Top             =   360
         Width           =   2085
      End
   End
   Begin VB.CheckBox chkMultiobjective 
      Caption         =   "Perform Multiobjective Optimization"
      Height          =   255
      Left            =   120
      TabIndex        =   21
      Top             =   480
      Visible         =   0   'False
      Width           =   2775
   End
   Begin VB.ComboBox cbNoOfIter 
      Height          =   315
      Left            =   2400
      Style           =   2  'Dropdown List
      TabIndex        =   2
      Top             =   960
      Width           =   735
   End
   Begin VB.CheckBox ckPerformOpti 
      Caption         =   "Perform Optimization"
      Height          =   255
      Left            =   120
      TabIndex        =   1
      Top             =   120
      Width           =   1815
   End
   Begin VB.Frame frOptimizationType 
      Appearance      =   0  'Flat
      Caption         =   "Optimization Type"
      ForeColor       =   &H80000008&
      Height          =   2055
      Left            =   120
      TabIndex        =   0
      Top             =   1440
      Width           =   3015
      Begin VB.OptionButton obOptimizationType 
         Appearance      =   0  'Flat
         Caption         =   "Maximal Inertia"
         ForeColor       =   &H80000008&
         Height          =   255
         Index           =   0
         Left            =   240
         TabIndex        =   9
         Top             =   360
         Width           =   1455
      End
      Begin VB.OptionButton obOptimizationType 
         Appearance      =   0  'Flat
         Caption         =   "Minimal Cost (Developped)"
         ForeColor       =   &H80000008&
         Height          =   255
         Index           =   3
         Left            =   240
         TabIndex        =   5
         Top             =   1440
         Width           =   2295
      End
      Begin VB.OptionButton obOptimizationType 
         Appearance      =   0  'Flat
         Caption         =   "Minimal Cost (Simplified)"
         ForeColor       =   &H80000008&
         Height          =   255
         Index           =   2
         Left            =   240
         TabIndex        =   4
         Top             =   1080
         Width           =   2055
      End
      Begin VB.OptionButton obOptimizationType 
         Appearance      =   0  'Flat
         Caption         =   "Minimal Weight"
         ForeColor       =   &H80000008&
         Height          =   255
         Index           =   1
         Left            =   240
         TabIndex        =   3
         Top             =   720
         Width           =   1815
      End
   End
   Begin VB.Label lbNoOfOpti 
      Alignment       =   2  'Center
      AutoSize        =   -1  'True
      Caption         =   "Number of iterations allowed for the optimization process:"
      Height          =   390
      Left            =   45
      TabIndex        =   8
      Top             =   885
      Width           =   2205
      WordWrap        =   -1  'True
   End
   Begin MSForms.CommandButton cmdOK 
      Default         =   -1  'True
      Height          =   375
      Left            =   840
      TabIndex        =   6
      Top             =   3720
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
      Left            =   2040
      TabIndex        =   7
      Top             =   3720
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
Attribute VB_Name = "frmOptimization"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Dim ProjectIndex As Integer
Private OBJ As New cHeader

Private Sub cbNoOfIter_Click()
    OBJ.ITERAM = CInt(cbNoOfIter.Text)
End Sub

Private Sub chkMultiobjective_Click()
    If chkMultiobjective.Value = vbChecked Then
        frOptimizationType.Visible = False
        frMultiobjective.Visible = True
    Else
        frOptimizationType.Visible = True
        frMultiobjective.Visible = False
    End If
    If chkMultiobjective.Value = vbChecked Then
        Label1.Enabled = True
        Label2.Enabled = True
        Label3.Enabled = True
        Label4.Enabled = True
        Label5.Enabled = True
        opMultiObjCost(0).Enabled = True
        opMultiObjCost(1).Enabled = Licensing.IS_COSTCAT
        txtCostWeight.Enabled = True
        txtWeightWeight.Enabled = True
        txtInertiaWeight.Enabled = True
    Else
        Label1.Enabled = False
        Label2.Enabled = False
        Label3.Enabled = False
        Label4.Enabled = False
        Label5.Enabled = False
        opMultiObjCost(0).Enabled = False
        opMultiObjCost(1).Enabled = False
        txtCostWeight.Enabled = False
        txtWeightWeight.Enabled = False
        txtInertiaWeight.Enabled = False
    End If
End Sub

Private Sub ckPerformOpti_Click()
    Dim i As Integer
'    If ckPerformOpti.Value = vbChecked Then
'        If chkMultiobjective.Value = vbChecked Then
'            frOptimizationType.Visible = False
'            frMultiobjective.Visible = True
'        Else
'            frOptimizationType.Visible = True
'            frMultiobjective.Visible = False
'        End If
'    End If
    If ckPerformOpti.Value = vbChecked Then
        lbNoOfOpti.Enabled = True
        cbNoOfIter.Enabled = True
        'frOptimizationType.Enabled = True
        lbNoOfOpti.Enabled = True
        For i = 0 To 3
            obOptimizationType(i).Enabled = True
        Next i
        If Licensing.LicenseLevel = 7 Then
            obOptimizationType(3).Enabled = False
        End If
        
        OBJ.IOPTI = yes
        OBJ.DIS1 = 0
        OBJ.DIS2 = OBJ.Width / 8
        OBJ.DIS3 = OBJ.Width / 4
        OBJ.DIS4 = OBJ.Width * 3 / 8
        OBJ.DIS5 = OBJ.Width / 2
        frMultiobjective.Enabled = True
    Else
        lbNoOfOpti.Enabled = False
        cbNoOfIter.Enabled = False
        'frOptimizationType.Enabled = False
        lbNoOfOpti.Enabled = False
        For i = 0 To 3
            obOptimizationType(i).Enabled = False
        Next i
        OBJ.IOPTI = no
        frMultiobjective.Enabled = False
    End If
    
    If ckPerformOpti.Value = vbChecked Then
        chkMultiobjective.Enabled = True
        Label1.Enabled = True
        Label2.Enabled = True
        Label3.Enabled = True
        Label4.Enabled = True
        Label5.Enabled = True
        opMultiObjCost(0).Enabled = True
        opMultiObjCost(1).Enabled = Licensing.IS_COSTCAT
        txtCostWeight.Enabled = True
        txtWeightWeight.Enabled = True
        txtInertiaWeight.Enabled = True
    Else
        chkMultiobjective.Enabled = False
        Label1.Enabled = False
        Label2.Enabled = False
        Label3.Enabled = False
        Label4.Enabled = False
        Label5.Enabled = False
        opMultiObjCost(0).Enabled = False
        opMultiObjCost(1).Enabled = False
        txtCostWeight.Enabled = False
        txtWeightWeight.Enabled = False
        txtInertiaWeight.Enabled = False
    End If
    'tmp
    'obOptimizationType(3).Enabled = False
End Sub

Private Sub cmdCancel_Click()
    Set OBJ = Nothing
    Unload Me
End Sub

Private Sub cmdCancel_DblClick(Cancel As MSForms.ReturnBoolean)
    Cancel = True
End Sub

Private Sub cmdOK_Click()

    'validate weights ...
    'sum = 1
    If chkMultiobjective = vbChecked And ckPerformOpti = vbChecked Then
        If Val(txtCostWeight) + Val(txtWeightWeight) + Val(txtInertiaWeight) < 0.999 Or _
            Val(txtCostWeight) + Val(txtWeightWeight) + Val(txtInertiaWeight) > 1.001 Then
           MsgBox "The sum of the objective functions weights must be 1.", vbCritical + vbOKOnly
           Exit Sub
        End If
    End If
    'no single weight (= 1.0)
    Dim index As Integer
    index = 0
    If Val(txtCostWeight) > 0.001 Then index = index + 1
    If Val(txtWeightWeight) > 0.001 Then index = index + 1
    If Val(txtInertiaWeight) > 0.001 Then index = index + 1
    If index < 2 Then
        MsgBox "Set at least two non-null weights.", vbCritical + vbOKOnly
        Exit Sub
    End If
    
    If chkMultiobjective.Value = vbChecked Then
        OBJ.cMultiOpti.IMULTI = yes
    Else
        OBJ.cMultiOpti.IMULTI = no
    End If
    
    OBJ.cMultiOpti.W1 = Val(txtCostWeight)
    OBJ.cMultiOpti.W2 = Val(txtWeightWeight)
    OBJ.cMultiOpti.W3 = Val(txtInertiaWeight)
    
    Project.Item(ProjectIndex).cHeader.IOPTI = OBJ.IOPTI
    Project.Item(ProjectIndex).cHeader.ITERAM = OBJ.ITERAM
    Project.Item(ProjectIndex).cHeader.ICOUT = OBJ.ICOUT
    If chkMultiobjective = vbChecked Then
        If opMultiObjCost(0).Value = True Then
            Project.Item(ProjectIndex).cHeader.ICOUT = MinimalCostSimple
        ElseIf opMultiObjCost(1).Value = True Then
            Project.Item(ProjectIndex).cHeader.ICOUT = MinimalCostDetailed
        End If
    End If
    Set Project.Item(ProjectIndex).cHeader.cMultiOpti = OBJ.cMultiOpti.Clone
    Set OBJ = Nothing
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
    Me.Caption = "Optimization - [" & GetFileName(Project.Item(ProjectIndex).sFileName) & "]"
    
    PopulateCb
    GetData
    'tmp
    'obOptimizationType(3).Enabled = False
End Sub

Private Sub Form_Resize()
    Me.Height = cmdOK.Top + cmdOK.Height + 600
End Sub

Private Sub Form_Unload(Cancel As Integer)
    fMainForm.SetFocus
End Sub

Private Sub GetData()
    On Error GoTo GetDataErr
    Dim i As Integer
    OBJ.IOPTI = Project.Item(ProjectIndex).cHeader.IOPTI
    OBJ.ITERAM = Project.Item(ProjectIndex).cHeader.ITERAM
    OBJ.ICOUT = Project.Item(ProjectIndex).cHeader.ICOUT
    Set OBJ.cMultiOpti = Project.Item(ProjectIndex).cHeader.cMultiOpti.Clone
    If OBJ.cMultiOpti.IMULTI = yes Then
        chkMultiobjective.Value = vbChecked
    Else
        chkMultiobjective.Value = vbUnchecked
    End If
    
    Select Case OBJ.IOPTI
        Case no
            ckPerformOpti.Value = vbUnchecked
            cbNoOfIter.Enabled = False
            'frOptimizationType.Enabled = False
            lbNoOfOpti.Enabled = False
            For i = 0 To 3
                obOptimizationType(i).Enabled = False
            Next i
        Case yes
            ckPerformOpti.Value = vbChecked
            cbNoOfIter.Enabled = True
            'frOptimizationType.Enabled = True
            lbNoOfOpti.Enabled = True
            For i = 0 To 3
                obOptimizationType(i).Enabled = True
            Next i
    End Select
    If Licensing.IS_COSTCAT = False Then
        obOptimizationType(3).Enabled = False
        opMultiObjCost(1).Enabled = False
    End If
    If OBJ.ITERAM = 0 Then OBJ.ITERAM = 1
    cbNoOfIter.Text = OBJ.ITERAM
    obOptimizationType(OBJ.ICOUT + 1).Value = True
    
    If OBJ.ICOUT = MinimalCostSimple Then
        opMultiObjCost(0) = True
    ElseIf OBJ.ICOUT = MinimalCostDetailed Then
        opMultiObjCost(1) = True
    Else
        opMultiObjCost(0) = True
    End If
    
    txtCostWeight = Format(OBJ.cMultiOpti.W1, "0.000")
    txtWeightWeight = Format(OBJ.cMultiOpti.W2, "0.000")
    txtInertiaWeight = Format(OBJ.cMultiOpti.W3, "0.000")
    
    'If ckPerformOpti.Value = vbChecked Then
        If chkMultiobjective.Value = vbChecked Then
            frMultiobjective.Visible = True
            frOptimizationType.Visible = False
        Else
            frMultiobjective.Visible = False
            frOptimizationType.Visible = True
        End If
'    Else
'
'        frMultiobjective.Visible = False
'        frOptimizationType.Visible = True
'    End If
    If ckPerformOpti.Value = vbChecked Then
        chkMultiobjective.Enabled = True
        Select Case OBJ.cMultiOpti.IMULTI
            Case no
                chkMultiobjective.Value = vbUnchecked
                Label1.Enabled = False
                Label2.Enabled = False
                Label3.Enabled = False
                Label4.Enabled = False
                Label5.Enabled = False
                opMultiObjCost(0).Enabled = False
                opMultiObjCost(1).Enabled = False
                txtCostWeight.Enabled = False
                txtWeightWeight.Enabled = False
                txtInertiaWeight.Enabled = False
            Case yes
                chkMultiobjective.Value = vbChecked
                Label1.Enabled = True
                Label2.Enabled = True
                Label3.Enabled = True
                Label4.Enabled = True
                Label5.Enabled = True
                opMultiObjCost(0).Enabled = True
                opMultiObjCost(1).Enabled = Licensing.IS_COSTCAT
                txtCostWeight.Enabled = True
                txtWeightWeight.Enabled = True
                txtInertiaWeight.Enabled = True
        End Select
    Else
        chkMultiobjective.Enabled = False
        Label1.Enabled = False
        Label2.Enabled = False
        Label3.Enabled = False
        Label4.Enabled = False
        Label5.Enabled = False
        opMultiObjCost(0).Enabled = False
        opMultiObjCost(1).Enabled = False
        txtCostWeight.Enabled = False
        txtWeightWeight.Enabled = False
        txtInertiaWeight.Enabled = False
    End If
    Exit Sub
GetDataErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmOptimization: Sub GetData")
End Sub

Private Sub PopulateCb()
    Dim i As Integer
    For i = 1 To 20
        cbNoOfIter.AddItem i
    Next i
End Sub

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
    Set OBJ = Nothing
End Sub

Private Sub obOptimizationType_Click(index As Integer)
    OBJ.ICOUT = index - 1
End Sub

Private Sub txtCostWeight_GotFocus()
    txtCostWeight.SelStart = 0
    txtCostWeight.SelLength = Len(txtCostWeight.Text)
End Sub

Private Sub txtCostWeight_Validate(Cancel As Boolean)
    ValidateSubunitary txtCostWeight, Cancel
End Sub

Private Sub txtInertiaWeight_GotFocus()
    txtInertiaWeight.SelStart = 0
    txtInertiaWeight.SelLength = Len(txtInertiaWeight.Text)
End Sub


Private Sub txtInertiaWeight_Validate(Cancel As Boolean)
    ValidateSubunitary txtInertiaWeight, Cancel
End Sub

Private Sub txtWeightWeight_GotFocus()
    txtWeightWeight.SelStart = 0
    txtWeightWeight.SelLength = Len(txtWeightWeight.Text)
End Sub

Private Sub txtWeightWeight_Validate(Cancel As Boolean)
    ValidateSubunitary txtWeightWeight, Cancel
End Sub
