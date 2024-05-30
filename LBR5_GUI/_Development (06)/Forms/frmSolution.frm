VERSION 5.00
Object = "{0D452EE1-E08F-101A-852E-02608C4D0BB4}#2.0#0"; "FM20.DLL"
Begin VB.Form frmSolution 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Solution"
   ClientHeight    =   4965
   ClientLeft      =   6510
   ClientTop       =   3195
   ClientWidth     =   4575
   Icon            =   "frmSolution.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   4965
   ScaleWidth      =   4575
   ShowInTaskbar   =   0   'False
   Begin VB.ListBox lstLoadCases 
      Height          =   1035
      Left            =   120
      TabIndex        =   7
      Top             =   360
      Width           =   3375
   End
   Begin VB.CheckBox chkShowVariations 
      Caption         =   "Show Gain"
      Height          =   255
      Left            =   3360
      TabIndex        =   14
      Top             =   3480
      Width           =   1095
   End
   Begin VB.CheckBox chkShowLegend 
      Caption         =   "Show Legend"
      Height          =   255
      Left            =   120
      TabIndex        =   12
      Top             =   3960
      Width           =   1335
   End
   Begin VB.CheckBox chkNeutralAxis 
      Caption         =   "Neutral Axis"
      Height          =   255
      Left            =   120
      TabIndex        =   10
      Top             =   3480
      Width           =   1215
   End
   Begin VB.ListBox lstDiagram 
      Height          =   1620
      Left            =   120
      TabIndex        =   3
      Top             =   1680
      Width           =   4335
   End
   Begin VB.TextBox txtScale 
      Height          =   315
      Left            =   3840
      Locked          =   -1  'True
      TabIndex        =   2
      Text            =   "1"
      Top             =   3960
      Width           =   375
   End
   Begin VB.CheckBox chkFinalScantlings 
      Caption         =   "Final Scantlings"
      Height          =   255
      Left            =   1800
      TabIndex        =   13
      Top             =   3480
      Width           =   1455
   End
   Begin VB.Frame frThickness 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   1800
      TabIndex        =   15
      Top             =   4000
      Width           =   1455
      Begin VB.OptionButton opNet 
         Caption         =   "Net"
         Height          =   195
         Left            =   0
         TabIndex        =   17
         Top             =   0
         Width           =   615
      End
      Begin VB.OptionButton opGross 
         Caption         =   "Gross"
         Height          =   195
         Left            =   720
         TabIndex        =   16
         Top             =   0
         Width           =   735
      End
   End
   Begin VB.ListBox lstSections 
      Height          =   1035
      Left            =   3600
      TabIndex        =   8
      Top             =   360
      Width           =   855
   End
   Begin MSForms.SpinButton SpinButton1 
      Height          =   315
      Left            =   4200
      TabIndex        =   18
      Top             =   3960
      Width           =   255
      Size            =   "450;556"
   End
   Begin VB.Line Line1 
      BorderColor     =   &H00FFFFFF&
      X1              =   120
      X2              =   4440
      Y1              =   3840
      Y2              =   3840
   End
   Begin VB.Line Line2 
      BorderWidth     =   2
      X1              =   120
      X2              =   4430
      Y1              =   3840
      Y2              =   3840
   End
   Begin MSForms.CommandButton cmdApply 
      Height          =   375
      Left            =   960
      TabIndex        =   11
      Top             =   4440
      Visible         =   0   'False
      Width           =   1095
      Caption         =   "Apply"
      PicturePosition =   327683
      Size            =   "1931;661"
      FontHeight      =   165
      FontCharSet     =   0
      FontPitchAndFamily=   2
      ParagraphAlign  =   3
   End
   Begin VB.Label Label1 
      AutoSize        =   -1  'True
      Caption         =   "Load cases :"
      Height          =   195
      Index           =   1
      Left            =   120
      TabIndex        =   9
      Top             =   120
      Width           =   915
   End
   Begin VB.Label Label1 
      AutoSize        =   -1  'True
      Caption         =   "Section (m) :"
      Height          =   195
      Index           =   0
      Left            =   3600
      TabIndex        =   6
      Top             =   120
      Width           =   885
   End
   Begin VB.Label Label2 
      AutoSize        =   -1  'True
      Caption         =   "Results:"
      Height          =   195
      Left            =   120
      TabIndex        =   5
      Top             =   1440
      Width           =   570
   End
   Begin VB.Label Label3 
      AutoSize        =   -1  'True
      Caption         =   "Scale:"
      Height          =   195
      Left            =   3360
      TabIndex        =   4
      Top             =   4005
      Width           =   450
   End
   Begin MSForms.CommandButton cmdOK 
      Default         =   -1  'True
      Height          =   375
      Left            =   2160
      TabIndex        =   1
      Top             =   4440
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
      Left            =   3360
      TabIndex        =   0
      Top             =   4440
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
Attribute VB_Name = "frmSolution"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Dim ProjectIndex As Integer
Dim LoadCaseIndex As Integer, DiagramIndex As Integer, SectionIndex As Integer
Dim header As cHeader, LoadCase As cLoadCase
Dim IANA As Integer

Private Sub chkFinalScantlings_Click()
    If chkFinalScantlings = vbChecked Then
        chkShowVariations.Enabled = True
    Else
        chkShowVariations.Enabled = False
        chkShowVariations = vbUnchecked
        Project.Item(ProjectIndex).cSolution.ShowVariation = False
    End If
    If chkFinalScantlings = vbChecked Then
        Project.Item(ProjectIndex).cSolution.ShowUpdatedScantling = True
    Else
        Project.Item(ProjectIndex).cSolution.ShowUpdatedScantling = False
    End If
    Draw ProjectIndex
End Sub

Private Sub chkNeutralAxis_Click()
    If chkNeutralAxis = vbChecked Then
        Project.Item(ProjectIndex).cSolution.ShowNeutralAxis = True
    Else
        Project.Item(ProjectIndex).cSolution.ShowNeutralAxis = False
    End If
    Draw ProjectIndex
End Sub

Private Sub chkShowLegend_Click()
    If chkShowLegend = vbChecked Then
        Project.Item(ProjectIndex).cSolution.ShowLegend = True
    Else
        Project.Item(ProjectIndex).cSolution.ShowLegend = False
    End If
    Draw ProjectIndex
End Sub

Private Sub chkShowVariations_Click()
    If chkShowVariations = vbChecked Then
        Project.Item(ProjectIndex).cSolution.ShowVariation = True
    Else
        Project.Item(ProjectIndex).cSolution.ShowVariation = False
    End If
    Draw ProjectIndex
End Sub

Private Sub cmdApply_Click()
    SetData
    Draw ProjectIndex
End Sub

Private Sub cmdApply_DblClick(Cancel As MSForms.ReturnBoolean)
    Cancel = True
End Sub

Private Sub cmdCancel_Click()
    Unload Me
End Sub

Private Sub cmdOK_Click()
    SetData
    Draw ProjectIndex
    Unload Me
End Sub

Private Sub Form_Load()
    ProjectIndex = ActiveProject
    IANA = Project.Item(ProjectIndex).cHeader.IANA
    Me.Move (Screen.Width - Me.Width) / 2, (Screen.Height - Me.Height) / 2
    cmdOK.Picture = LoadResPicture("ID_BITMAP_OK", 0)
    cmdCancel.Picture = LoadResPicture("ID_BITMAP_CANCEL", 0)
    Me.Caption = "Solution - [" & GetFileName(Project.Item(ProjectIndex).sFileName) & "]"
    If Project.Item(ProjectIndex).cSolution.ShowUpdatedScantling = True Then chkFinalScantlings = vbChecked
    If Project.Item(ProjectIndex).cSolution.ShowLegend = True Then chkShowLegend = vbChecked
    If Project.Item(ProjectIndex).cSolution.ShowNeutralAxis = True Then chkNeutralAxis = vbChecked
    If IANA = 2 Then
        If Project.Item(ProjectIndex).cSolution.Thickness = Net Then
            opNet = True
        ElseIf Project.Item(ProjectIndex).cSolution.Thickness = Gross Then
            opGross = True
        End If
    End If
    Select Case Project.Item(ProjectIndex).cHeader.IANA
        Case 1
            opNet.Visible = False
            opGross.Visible = False
            Label1(0).Visible = True
            lstLoadCases.Width = 3375
        Case 2
            opNet.Visible = True
            opGross.Visible = True
            Label1(0).Visible = False
            lstLoadCases.Width = lstDiagram.Width
    End Select
    GetData
End Sub

Private Sub GetData()
    Set header = Project.Item(ProjectIndex).cHeader
    For Each LoadCase In header.colLoadCase
        If LoadCase.state = IsOn Then
            lstLoadCases.AddItem LoadCase.Title
        End If
    Next LoadCase
    lstSections.AddItem header.DIS1
    lstSections.AddItem header.DIS2
    lstSections.AddItem header.DIS3
    lstSections.AddItem header.DIS4
    lstSections.AddItem header.DIS5

    If Project.Item(ProjectIndex).cHeader.IANA = 1 Then
        lstDiagram.AddItem "V - Transversal Displacement"
        lstDiagram.AddItem "U - Longitudinal Displacement"
        lstDiagram.AddItem "W - Transversal Displacement"
        lstDiagram.AddItem "Nx - Unitary Longitudinal Forces"
        lstDiagram.AddItem "Ny - Unitary Transversal Forces"
        lstDiagram.AddItem "Nxy - Unitary In-Plane Shear Forces"
        lstDiagram.AddItem "Mx - Unitary Longitudinal Bending Moment"
        lstDiagram.AddItem "My - Unitary Transversal Bending Moment"
        lstDiagram.AddItem "Ry - Unitary Reaction in Frames"
        lstDiagram.AddItem "Rx - Unitary Reaction in Stiffeners"
        lstDiagram.AddItem "Sy - Transversal Stress in Plates"
        lstDiagram.AddItem "Sx - Longitudinal Stress in Plates"
        lstDiagram.AddItem "Txy - Shear Stress in Plates"
        lstDiagram.AddItem "Sc - Von Mises Stress in Plates"
        lstDiagram.AddItem "Sy Frames - Transversal Stress (Web-Flange Junction)"
        lstDiagram.AddItem "Tyz Frames - Shear Stress (Web-Plate Junction)"
        lstDiagram.AddItem "Sc Frames - Von Mises Stress (Web-Flange Junction)"
        lstDiagram.AddItem "Sc Frames - Von Mises Stress (Web-Plate Junction)"
        lstDiagram.AddItem "None"
    Else
        lstDiagram.AddItem "Txy    - Shear Stress"
        lstDiagram.AddItem "Sx      - Longitudinal Global Stress"
        lstDiagram.AddItem "Sx_st - Longitudinal Local Stress in Stiffeners"
        lstDiagram.AddItem "None"
    End If
    txtScale = Project.Item(ProjectIndex).cSolution.DiagramScale
    If Project.Item(ProjectIndex).cSolution.ShowNeutralAxis = True Then
        chkNeutralAxis = vbChecked
    Else
        chkNeutralAxis = vbUnchecked
    End If
    
    If Project.Item(ProjectIndex).cHeader.colLoadCase.Count > 0 Then
        lstLoadCases.ListIndex = Project.Item(ProjectIndex).cSolution.CurrentLoadCase - 1
    End If
    
    lstSections.ListIndex = Project.Item(ProjectIndex).cSolution.CurrentSection - 1
    SectionIndex = Project.Item(ProjectIndex).cSolution.CurrentSection
    
    If IANA = 1 Then
        lstDiagram.ListIndex = Project.Item(ProjectIndex).cSolution.CurrentDiagram - 1
    End If
    If IANA = 2 And Project.Item(ProjectIndex).cSolution.CurrentDiagram > 3 Then
        lstDiagram.ListIndex = 3
    ElseIf IANA = 2 Then
        lstDiagram.ListIndex = Project.Item(ProjectIndex).cSolution.CurrentDiagram - 1
    End If
    If Project.Item(ProjectIndex).cSolution.ShowLegend = True Then
        chkShowLegend = vbChecked
    Else
        chkShowLegend = vbUnchecked
    End If
    
    If Project.Item(ProjectIndex).colPanelUpdate.Count = 0 Then
        Project.Item(ProjectIndex).cSolution.ShowUpdatedScantling = False
        chkFinalScantlings.Enabled = False
    Else
        chkFinalScantlings.Enabled = True
    End If
    
    If Project.Item(ProjectIndex).cSolution.ShowUpdatedScantling = True Then
        chkFinalScantlings = vbChecked
    Else
        chkFinalScantlings = vbUnchecked
    End If
    
    
    If chkFinalScantlings = vbChecked Then
        chkShowVariations.Enabled = True
    Else
        chkShowVariations.Enabled = False
        chkShowVariations = vbUnchecked
    End If
    If Project.Item(ProjectIndex).cSolution.ShowVariation = True Then
        chkShowVariations = vbChecked
    Else
        chkShowVariations = vbUnchecked
    End If
End Sub

Private Sub SetData()
    With Project.Item(ProjectIndex).cSolution
        .CurrentLoadCase = LoadCaseIndex
        .CurrentSection = SectionIndex
        .CurrentDiagram = DiagramIndex
        
        If DiagramIndex = 19 And IANA = 1 Then ' None
            .ShowSolution = False
        Else
            .ShowSolution = True
        End If
        If DiagramIndex = 4 And IANA = 2 Then ' None
            .ShowSolution = False
        Else
            .ShowSolution = True
        End If
        .DiagramScale = txtScale
    End With
End Sub

Private Sub Form_Resize()
    Me.Height = cmdOK.Top + cmdOK.Height + 600
End Sub

Private Sub Form_Unload(Cancel As Integer)
    fMainForm.SetFocus
End Sub

Private Sub lstDiagram_Click()
    DiagramIndex = lstDiagram.ListIndex + 1
    cmdApply_Click
End Sub

Private Sub lstLoadCases_Click()
    LoadCaseIndex = lstLoadCases.ListIndex + 1
    cmdApply_Click
End Sub

Private Sub lstSections_Click()
    SectionIndex = lstSections.ListIndex + 1
    cmdApply_Click
End Sub

Private Sub opGross_Click()
    Project.Item(ProjectIndex).cSolution.Thickness = Gross
    SectionIndex = 2
    cmdApply_Click
End Sub

Private Sub opNet_Click()
    Project.Item(ProjectIndex).cSolution.Thickness = Net
    SectionIndex = 1
    cmdApply_Click
End Sub

Private Sub SpinButton1_SpinDown()
    If SpinButton1.Value > 0 Then
        txtScale.Text = CInt(txtScale.Text) - 1
        SpinButton1.Value = txtScale.Text
        cmdApply_Click
    End If
End Sub

Private Sub SpinButton1_SpinUp()
    If SpinButton1.Value < 21 Then
        txtScale.Text = CInt(txtScale.Text) + 1
        SpinButton1.Value = txtScale.Text
        cmdApply_Click
    End If
End Sub

Private Sub txtScale_GotFocus()
    txtScale.SelStart = 0
    txtScale.SelLength = Len(txtScale.Text)
End Sub

Private Sub txtScale_KeyDown(KeyCode As Integer, Shift As Integer)
    On Error Resume Next
    Dim Cancel As Boolean
    Select Case KeyCode
        Case 40 'Arrow down
            If txtScale.Text > 1 Then
                txtScale.Text = CDbl(txtScale.Text) - 1
            End If
        Case 38 'Arrow Up
            txtScale.Text = CDbl(txtScale.Text) + 1
    End Select
    txtScale_Validate Cancel
    If Cancel = False Then cmdApply_Click
End Sub

Private Sub txtScale_Validate(Cancel As Boolean)
    ValidateNonNullPozitive txtScale, Cancel
    ValidateNumeric txtScale, Cancel
    If Cancel = False Then cmdApply_Click
End Sub
