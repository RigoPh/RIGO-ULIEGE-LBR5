VERSION 5.00
Object = "{0D452EE1-E08F-101A-852E-02608C4D0BB4}#2.0#0"; "FM20.DLL"
Object = "{65E121D4-0C60-11D2-A9FC-0000F8754DA1}#2.0#0"; "mschrt20.ocx"
Begin VB.Form frmHull_Girder 
   BorderStyle     =   4  'Fixed ToolWindow
   Caption         =   "Hull Girder"
   ClientHeight    =   9810
   ClientLeft      =   8205
   ClientTop       =   2130
   ClientWidth     =   11655
   Icon            =   "frmHull_Girder.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   9810
   ScaleWidth      =   11655
   ShowInTaskbar   =   0   'False
   Begin VB.TextBox txtSubstitute 
      Appearance      =   0  'Flat
      BackColor       =   &H8000000F&
      BorderStyle     =   0  'None
      Height          =   855
      Left            =   9000
      Locked          =   -1  'True
      TabIndex        =   8
      Top             =   5280
      Visible         =   0   'False
      Width           =   1815
   End
   Begin MSChart20Lib.MSChart grphBending 
      Height          =   1335
      Left            =   120
      OleObjectBlob   =   "frmHull_Girder.frx":000C
      TabIndex        =   3
      TabStop         =   0   'False
      Top             =   3360
      Width           =   1815
   End
   Begin MSChart20Lib.MSChart grphShear 
      Height          =   1335
      Left            =   120
      OleObjectBlob   =   "frmHull_Girder.frx":24C4
      TabIndex        =   2
      TabStop         =   0   'False
      Top             =   1920
      Width           =   1815
   End
   Begin MSChart20Lib.MSChart grphLoading 
      Height          =   1335
      Left            =   120
      OleObjectBlob   =   "frmHull_Girder.frx":497C
      TabIndex        =   0
      TabStop         =   0   'False
      Top             =   480
      Width           =   1815
   End
   Begin VB.ListBox lstLoadCase 
      Appearance      =   0  'Flat
      Height          =   3345
      Left            =   9000
      TabIndex        =   1
      Top             =   480
      Width           =   2535
   End
   Begin VB.Label lblNote 
      Caption         =   "Note:"
      ForeColor       =   &H00404000&
      Height          =   795
      Left            =   9120
      TabIndex        =   7
      Top             =   4320
      Width           =   2415
   End
   Begin VB.Label lblLoadCases 
      AutoSize        =   -1  'True
      Caption         =   "Load Cases:"
      Height          =   195
      Left            =   9000
      TabIndex        =   6
      Top             =   120
      Width           =   885
   End
   Begin MSForms.CommandButton cmdSubstitute 
      Height          =   375
      Left            =   9120
      TabIndex        =   5
      Top             =   6240
      Visible         =   0   'False
      Width           =   1335
      Caption         =   "Substitute"
      Size            =   "2355;661"
      Accelerator     =   83
      FontHeight      =   165
      FontCharSet     =   0
      FontPitchAndFamily=   2
      ParagraphAlign  =   3
   End
   Begin VB.Label lblSummary 
      AutoSize        =   -1  'True
      Caption         =   "Summary"
      Height          =   195
      Left            =   9120
      TabIndex        =   4
      Top             =   3960
      Width           =   645
   End
End
Attribute VB_Name = "frmHull_Girder"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Dim ProjectIndex As Integer
Dim i As Integer
Dim Hull_Girder As Hull_Girder
Dim dsgMomFore As Double, dsgMomAft As Double

Private Sub cmdSubstitute_Click()
    With Project.Item(ProjectIndex).cHeader.colLoadCase.Item(lstLoadCase.ListIndex + 1)
        .VerticalBendingMomentFore = dsgMomFore * 1000
        .VerticalBendingMomentAft = dsgMomAft * 1000
    End With
    Hull_Girder = HullGirder(ProjectIndex, lstLoadCase.ListIndex + 1)
    GraphShear
    GraphBending
    SetSummary
End Sub

Private Sub Form_Load()
    ProjectIndex = ActiveProject
    Me.Move (Screen.Width - Me.Width) / 2, (Screen.Height - Me.Height) / 2
    Me.Caption = "Hull Girder - [" & GetFileName(Project.Item(ProjectIndex).sFileName) & "]"
    Hull_Girder = HullGirder(ProjectIndex, 1)
    PopulateLoadCaseList
    lstLoadCase.ListIndex = 0
    GraphLoading
    GraphShear
    GraphBending
    SetSummary
End Sub

Private Sub SetSummary()
    Dim dWeight As Double, dReactFore As Double, dReactAft As Double, _
        dMomFore As Double, dMomAft As Double, dMomMid As Double
    'Structure Weight
    Dim i As Integer
    Dim dUnit As Double
    Dim s As String
    dUnit = Project.Item(ProjectIndex).cHeader.Width / NO_SECTIONS
    For i = 0 To NO_SECTIONS - 1
        dWeight = dWeight - Hull_Girder.STRUCTURE_DEADWEIGHT(i) * dUnit / 1000
    Next i
    s = "Structure Weight:" & vbCrLf
    s = s + "G = " & GetSpaceFormat(Round(dWeight, 3)) & " kN" & vbCrLf & vbCrLf
    'Shear Force
    dReactFore = Hull_Girder.SHEAR_FORCE(0) / 1000
    dReactAft = Hull_Girder.SHEAR_FORCE(NO_SECTIONS) / 1000
    s = s + "Shear Force:" & vbCrLf & _
        "T for = " & GetSpaceFormat(Round(dReactFore, 3)) & " kN" & vbCrLf & _
        "T aft = " & GetSpaceFormat(Round(dReactAft, 3)) & " kN" & vbCrLf & vbCrLf
    'Bending Moment
    dMomFore = Hull_Girder.BENDING_MOMENT(0) / 1000
    dMomAft = Hull_Girder.BENDING_MOMENT(NO_SECTIONS) / 1000
    dMomMid = Hull_Girder.BENDING_MOMENT(CInt(NO_SECTIONS / 2)) / 1000
    s = s + "Bending Moment:" & vbCrLf & _
        "M for = " & GetSpaceFormat(Round(dMomFore, 3)) & " kNm" & vbCrLf & _
        "M aft = " & GetSpaceFormat(Round(dMomAft, 3)) & " kNm" & vbCrLf & _
        "M mid = " & GetSpaceFormat(Round(dMomMid, 3)) & " kNm" & vbCrLf & vbCrLf
    'suggested substitute bending moments
    dsgMomFore = dMomFore + (dMomFore - dMomMid)
    dsgMomAft = dMomAft + (dMomAft - dMomMid)
'    s = s + "Substitution Bending Moment: " & vbCrLf & _
'        "M for = " & GetSpaceFormat(Round(dsgMomFore, 3)) & " kNm" & vbCrLf & _
'        "M aft = " & GetSpaceFormat(Round(dsgMomAft, 3)) & " kNm" & vbCrLf
    s = s + "Substitution Bending Moment: "
    lblSummary.Caption = s
    txtSubstitute.Text = Round(Abs(dsgMomFore), 3) & " [kNm]"
    txtSubstitute.Visible = True
    lblNote.Caption = "Note: All values correspond to the full section."
End Sub

Private Sub PopulateLoadCaseList()
    Dim LoadCase As cLoadCase
    For Each LoadCase In Project.Item(ProjectIndex).cHeader.colLoadCase
        lstLoadCase.AddItem LoadCase.Title
    Next LoadCase
End Sub

Private Sub GraphLoading()
     With grphLoading
        .AllowSelections = False
        '.AllowSeriesSelection = True
        
        'legend
        .ShowLegend = True
        .Legend.TextLayout.HorzAlignment = VtHorizontalAlignmentCenter
        .Legend.Location.LocationType = VtChLocationTypeBottom
        'axis
        .Plot.Axis(2, 1).AxisTitle.Visible = True
        .Plot.Axis(2, 1).AxisTitle.Text = "[kN/m]"
        .Plot.Axis(2, 1).AxisTitle.TextLayout.Orientation = VtOrientationHorizontal
        .RowCount = NO_SECTIONS + 1
        .ColumnCount = 3
        For i = 0 To NO_SECTIONS
            .Row = i + 1
            .Column = 1
            .Data = (Hull_Girder.STEPWISE_DEADWEIGHT(i) + Hull_Girder.STEPWISE_DN(i) + Hull_Girder.STEPWISE_UP(i)) / 1000
        Next i
        For i = 0 To NO_SECTIONS
            .Row = i + 1
            .Column = 2
            .Data = Hull_Girder.STRUCTURE_DEADWEIGHT(i) / 1000
        Next i
        For i = 0 To NO_SECTIONS
            .Row = i + 1
            .Column = 3
            .Data = Format((Hull_Girder.UNIFORM_UP(i) + Hull_Girder.UNIFORM_DN(i)) / 1000, "0.000")
        Next i
        .Plot.SeriesCollection(1).LegendText = "Stepwise Pressure"
        .Plot.SeriesCollection(2).LegendText = "Deadweight"
        .Plot.SeriesCollection(3).LegendText = "Uniform Pressure"
        
    End With
End Sub

Private Sub GraphShear()
     With grphShear
        .AllowSelections = False
        .RowCount = NO_SECTIONS + 1
        .ColumnCount = 1
        .chartType = VtChChartType2dBar
        'legend
        .ShowLegend = True
        .Legend.Location.LocationType = VtChLocationTypeBottom
        .Plot.SeriesCollection(1).LegendText = "Shear Force"
        'axis
        .Plot.Axis(2, 1).AxisTitle.Visible = True
        .Plot.Axis(2, 1).AxisTitle.Text = "[kN]"
        .Plot.Axis(2, 1).AxisTitle.TextLayout.Orientation = VtOrientationHorizontal
        For i = 0 To NO_SECTIONS
            .Row = i + 1
            .Column = 1
            .Data = Format(Hull_Girder.SHEAR_FORCE(i) / 1000, "0.000")
        Next i
        
    End With

End Sub

Private Sub GraphBending()
     With grphBending
        .AllowSelections = False
        
        .RowCount = NO_SECTIONS + 1
        .ColumnCount = 1
        .chartType = VtChChartType2dBar
        'legend
        .ShowLegend = True
        .Legend.Location.LocationType = VtChLocationTypeBottom
        'axis
        .Plot.Axis(2, 1).AxisTitle.Visible = True
        .Plot.Axis(2, 1).AxisTitle.Text = "[kNm]"
        .Plot.Axis(2, 1).AxisTitle.TextLayout.Orientation = VtOrientationHorizontal
        For i = 0 To NO_SECTIONS
            .Row = i + 1
            .Column = 1
            .Data = Format(Hull_Girder.BENDING_MOMENT(i) / 1000, "0.000")
        Next i
        .Plot.SeriesCollection(1).LegendText = "Bending Moment"
    End With
End Sub

Private Sub Form_Resize()
    On Error Resume Next
    lblLoadCases.Top = Me.ScaleTop + 10
    lblLoadCases.Left = Me.ScaleWidth - lstLoadCase.Width - 200
    With lstLoadCase
        lstLoadCase.Top = lblLoadCases.Top + lblLoadCases.Height + 30
        lstLoadCase.Left = Me.ScaleWidth - lstLoadCase.Width - 200
        lstLoadCase.Height = 2000
    End With
    With grphLoading
         .Top = Me.ScaleTop '+ 10
        .Left = Me.ScaleLeft + 10
        .Height = 1 / 3 * Me.ScaleHeight '- 200
        .Width = IIf(lstLoadCase.Left - 400 > 0, lstLoadCase.Left - 10, 1)
    End With
    With grphShear
        .Top = grphLoading.Top + grphLoading.Height '+ 20
        .Left = Me.ScaleLeft + 10
        .Height = 1 / 3 * Me.ScaleHeight '- 200
        .Width = IIf(lstLoadCase.Left - 400 > 0, lstLoadCase.Left - 10, 1)
    End With
    With grphBending
        .Top = grphShear.Top + grphShear.Height '+ 20
        .Left = Me.ScaleLeft + 10
        .Height = 1 / 3 * Me.ScaleHeight '- 200
        .Width = IIf(lstLoadCase.Left - 400 > 0, lstLoadCase.Left - 10, 1)
    End With
    lblSummary.Left = lstLoadCase.Left
    lblSummary.Top = lstLoadCase.Top + lstLoadCase.Height + 500
    
    txtSubstitute.Left = lblSummary.Left
    txtSubstitute.Top = lblSummary.Top + lblSummary.Height + 100
    
    cmdSubstitute.Left = lblSummary.Left
    cmdSubstitute.Top = lblSummary.Top + lblSummary.Height + 50
    
    lblNote.Left = cmdSubstitute.Left
    lblNote.Top = cmdSubstitute.Top + cmdSubstitute.Height + 2250
    
End Sub

Private Sub Label1_Click()

End Sub

Private Sub lstLoadCase_Click()
    Hull_Girder = HullGirder(ProjectIndex, lstLoadCase.ListIndex + 1)
    GraphLoading
    GraphShear
    GraphBending
    SetSummary
End Sub
