VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "COMDLG32.OCX"
Object = "{5E9E78A0-531B-11CF-91F6-C2863C385E30}#1.0#0"; "MSFLXGRD.OCX"
Object = "{0D452EE1-E08F-101A-852E-02608C4D0BB4}#2.0#0"; "FM20.DLL"
Begin VB.Form frmOptions 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Options"
   ClientHeight    =   3825
   ClientLeft      =   1500
   ClientTop       =   1590
   ClientWidth     =   5070
   Icon            =   "frmOptions.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   3825
   ScaleWidth      =   5070
   ShowInTaskbar   =   0   'False
   Begin MSComDlg.CommonDialog cdlgColors 
      Left            =   2760
      Top             =   840
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
   End
   Begin VB.ComboBox Combo 
      Appearance      =   0  'Flat
      Height          =   315
      ItemData        =   "frmOptions.frx":000C
      Left            =   1680
      List            =   "frmOptions.frx":000E
      Style           =   2  'Dropdown List
      TabIndex        =   4
      Top             =   840
      Visible         =   0   'False
      Width           =   975
   End
   Begin VB.TextBox TxtEdit 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   285
      Left            =   600
      TabIndex        =   0
      Text            =   "Text1"
      Top             =   840
      Visible         =   0   'False
      Width           =   975
   End
   Begin MSFlexGridLib.MSFlexGrid MSH1 
      Height          =   3015
      Left            =   120
      TabIndex        =   1
      Top             =   120
      Width           =   4815
      _ExtentX        =   8493
      _ExtentY        =   5318
      _Version        =   393216
      Cols            =   9
      GridColor       =   0
      ScrollTrack     =   -1  'True
      GridLinesFixed  =   1
      Appearance      =   0
      FormatString    =   "|^|^|^|^|^|"
   End
   Begin MSForms.CommandButton cmdApply 
      Height          =   375
      Left            =   1440
      TabIndex        =   5
      Top             =   3240
      Width           =   1095
      Caption         =   "Apply"
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
      Left            =   3840
      TabIndex        =   3
      Top             =   3240
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
      Left            =   2640
      TabIndex        =   2
      Top             =   3240
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
Attribute VB_Name = "frmOptions"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Dim ProjectIndex As Integer
Dim dBuffer As String

Private Sub CmdApply_Click()
    SetData
    Project.Item(ProjectIndex).frmProject.picScreen.Cls
    Draw ProjectIndex
    Project.Item(ProjectIndex).DataChanged = True
End Sub

Private Sub cmdApply_DblClick(Cancel As MSForms.ReturnBoolean)
    Cancel = True
End Sub

Private Sub cmdCancel_Click()
    Unload Me
End Sub

Private Sub cmdCancel_DblClick(Cancel As MSForms.ReturnBoolean)
    Cancel = True
End Sub

Private Sub cmdOK_DblClick(Cancel As MSForms.ReturnBoolean)
    Cancel = True
End Sub

Private Sub Combo_Click()
    'Combo.Visible = Not Combo.Visible
End Sub

Private Sub Form_Load()
    ProjectIndex = ActiveProject
    Me.Move (Screen.Width - Me.Width) / 2, (Screen.Height - Me.Height) / 2
    cmdOK.Picture = LoadResPicture("ID_BITMAP_OK", 0)
    cmdCancel.Picture = LoadResPicture("ID_BITMAP_CANCEL", 0)
    Me.Caption = "Options - [" & GetFileName(Project.Item(ProjectIndex).sFileName) & "]"
    FlexGrid
End Sub

Private Sub FlexGrid()
    MSH1.Cols = 5
    MSH1.Rows = 1
    MSH1.TextMatrix(0, 0) = "Item"
    MSH1.TextMatrix(0, 1) = "State"
    MSH1.TextMatrix(0, 2) = "Size"
    MSH1.TextMatrix(0, 3) = "Color"
    MSH1.TextMatrix(0, 4) = "Text"
    MSH1.ColWidth(0) = 1500
    MSH1.ColWidth(1) = 500
    MSH1.ColWidth(2) = 500
    MSH1.ColWidth(3) = 500
    MSH1.ColWidth(4) = 1530
    MSH1.AddItem "Beams"
    MSH1.AddItem "Fleshes"
    MSH1.AddItem "Frames 1"
    MSH1.AddItem "Frames 2"
    MSH1.AddItem "Girders"
    MSH1.AddItem "Grid"
    MSH1.AddItem "Nodes"
    MSH1.AddItem "Panels"
    MSH1.AddItem "Uniform Pressures"
    MSH1.AddItem "Screen"
    MSH1.AddItem "Stiffeners 1"
    MSH1.AddItem "Stiffeners 2"
    GetData
End Sub

Private Sub GetData()
    Dim index As Integer
    index = 0
    'Beams
    index = index + 1
    MSH1.Row = index: MSH1.col = 3
    MSH1.CellBackColor = Project.Item(ProjectIndex).cDisplaySettings.ColorBeams
    MSH1.TextMatrix(index, 1) = "---------" '9
    MSH1.TextMatrix(index, 2) = "---------" '9
    MSH1.TextMatrix(index, 4) = "---------" '9
    'Fleshes
    index = index + 1
    Select Case Project.Item(ProjectIndex).cDisplaySettings.DrawFleshes
        Case No
            MSH1.TextMatrix(index, 1) = "OFF"
        Case Yes
            MSH1.TextMatrix(index, 1) = "ON"
    End Select
    MSH1.Row = index: MSH1.col = 3
    MSH1.CellBackColor = Project.Item(ProjectIndex).cDisplaySettings.ColorFleshes
    MSH1.TextMatrix(index, 2) = "---------" '9
    MSH1.TextMatrix(index, 4) = "---------" '9
    'Primary Frames
    index = index + 1
    Select Case Project.Item(ProjectIndex).cDisplaySettings.DrawPrimaryFrames
        Case No
            MSH1.TextMatrix(index, 1) = "OFF"
        Case Yes
            MSH1.TextMatrix(index, 1) = "ON"
    End Select
    MSH1.TextMatrix(index, 2) = Project.Item(ProjectIndex).cDisplaySettings.SizePrimaryFrames
    MSH1.Row = index: MSH1.col = 3
    MSH1.CellBackColor = Project.Item(ProjectIndex).cDisplaySettings.ColorPrimaryFrames
    Select Case Project.Item(ProjectIndex).cDisplaySettings.TextPrimaryFrames
        Case None
            MSH1.TextMatrix(index, 4) = "None"
        Case WebHeight
            MSH1.TextMatrix(index, 4) = "Web Height"
        Case WebThickness
            MSH1.TextMatrix(index, 4) = "Web Thickness"
        Case WebGrossThickness
            MSH1.TextMatrix(index, 4) = "Web Gross Thickness"
        Case FlangeWidth
            MSH1.TextMatrix(index, 4) = "Flange Width"
        Case FlangeThickness
            MSH1.TextMatrix(index, 4) = "Flange Thickness"
        Case FlangeGrossThickness
            MSH1.TextMatrix(index, 4) = "Flange Gross Thickness"
        Case Spacing
            MSH1.TextMatrix(index, 4) = "Spacing"
        Case All
            MSH1.TextMatrix(index, 4) = "All"
    End Select
    'Secondary Frames
    index = index + 1
    Select Case Project.Item(ProjectIndex).cDisplaySettings.DrawSecondaryFrames
        Case No
            MSH1.TextMatrix(index, 1) = "OFF"
        Case Yes
            MSH1.TextMatrix(index, 1) = "ON"
    End Select
    MSH1.TextMatrix(index, 2) = Project.Item(ProjectIndex).cDisplaySettings.SizeSecondaryFrames
    MSH1.Row = index: MSH1.col = 3
    MSH1.CellBackColor = Project.Item(ProjectIndex).cDisplaySettings.ColorSecondaryFrames
    Select Case Project.Item(ProjectIndex).cDisplaySettings.TextSecondaryFrames
        Case None
            MSH1.TextMatrix(index, 4) = "None"
        Case WebHeight
            MSH1.TextMatrix(index, 4) = "Web Height"
        Case WebThickness
            MSH1.TextMatrix(index, 4) = "Web Thickness"
        Case FlangeWidth
            MSH1.TextMatrix(index, 4) = "Flange Width"
        Case FlangeThickness
            MSH1.TextMatrix(index, 4) = "Flange Thickness"
        Case Spacing
            MSH1.TextMatrix(index, 4) = "Spacing"
        Case All
            MSH1.TextMatrix(index, 4) = "All"
    End Select
    'Girders
    index = index + 1
    Select Case Project.Item(ProjectIndex).cDisplaySettings.DrawGirders
        Case No
            MSH1.TextMatrix(index, 1) = "OFF"
        Case Yes
            MSH1.TextMatrix(index, 1) = "ON"
    End Select
    MSH1.TextMatrix(index, 2) = Project.Item(ProjectIndex).cDisplaySettings.SizeGirders
    MSH1.Row = index: MSH1.col = 3
    MSH1.CellBackColor = Project.Item(ProjectIndex).cDisplaySettings.ColorGirders
    Select Case Project.Item(ProjectIndex).cDisplaySettings.TextGirders
        Case None
            MSH1.TextMatrix(index, 4) = "None"
        Case WebHeight
            MSH1.TextMatrix(index, 4) = "Web Height"
        Case WebThickness
            MSH1.TextMatrix(index, 4) = "Web Thickness"
        Case FlangeWidth
            MSH1.TextMatrix(index, 4) = "Flange Width"
        Case FlangeThickness
            MSH1.TextMatrix(index, 4) = "Flange Thickness"
        Case All
            MSH1.TextMatrix(index, 4) = "All"
    End Select
    'Grid
    index = index + 1
    Select Case Project.Item(ProjectIndex).cDisplaySettings.DrawGrid
        Case No
            MSH1.TextMatrix(index, 1) = "OFF"
        Case Yes
            MSH1.TextMatrix(index, 1) = "ON"
    End Select
    MSH1.TextMatrix(index, 2) = Project.Item(ProjectIndex).cDisplaySettings.SizeGrid
    MSH1.Row = index: MSH1.col = 3
    MSH1.CellBackColor = Project.Item(ProjectIndex).cDisplaySettings.ColorGrid
    MSH1.TextMatrix(index, 4) = "---------" '9
    'Nodes
    index = index + 1
    Select Case Project.Item(ProjectIndex).cDisplaySettings.DrawNodes
        Case No
            MSH1.TextMatrix(index, 1) = "OFF"
        Case Yes
            MSH1.TextMatrix(index, 1) = "ON"
    End Select
    MSH1.TextMatrix(index, 2) = Project.Item(ProjectIndex).cDisplaySettings.SizeNodes
    MSH1.Row = index: MSH1.col = 3
    MSH1.CellBackColor = Project.Item(ProjectIndex).cDisplaySettings.ColorNodes
    'MSH1.TextMatrix(Index, 4) = "---------" '9
    Select Case Project.Item(ProjectIndex).cDisplaySettings.TextNodes
        Case None
            MSH1.TextMatrix(index, 4) = "None"
        Case Numbers
            MSH1.TextMatrix(index, 4) = "Numbers"
        Case Coordinates
            MSH1.TextMatrix(index, 4) = "Coordinates"
        Case All
            MSH1.TextMatrix(index, 4) = "All"
    End Select

    'Panels
    index = index + 1
    Select Case Project.Item(ProjectIndex).cDisplaySettings.DrawPlates
        Case No
            MSH1.TextMatrix(index, 1) = "OFF"
        Case Yes
            MSH1.TextMatrix(index, 1) = "ON"
    End Select
    MSH1.Row = index: MSH1.col = 3
    MSH1.CellBackColor = Project.Item(ProjectIndex).cDisplaySettings.ColorPlates
    MSH1.TextMatrix(index, 2) = Project.Item(ProjectIndex).cDisplaySettings.SizePlates
    Select Case Project.Item(ProjectIndex).cDisplaySettings.TextPlates
        Case None
            MSH1.TextMatrix(index, 4) = "None"
        Case Numbers
            MSH1.TextMatrix(index, 4) = "Numbers"
        Case Lenght
            MSH1.TextMatrix(index, 4) = "Lenght"
        Case NetThickness
            MSH1.TextMatrix(index, 4) = "Net Thickness"
        Case GrossThickness
            MSH1.TextMatrix(index, 4) = "Gross Thickness"
        Case Corrosion
            MSH1.TextMatrix(index, 4) = "Corrosion"
    End Select
    'Uniform Pressures
    index = index + 1
    Select Case Project.Item(ProjectIndex).cDisplaySettings.DrawPressuresUniformlyDistributed
        Case No
            MSH1.TextMatrix(index, 1) = "OFF"
        Case Yes
            MSH1.TextMatrix(index, 1) = "ON"
    End Select
    MSH1.Row = index: MSH1.col = 3
    MSH1.CellBackColor = Project.Item(ProjectIndex).cDisplaySettings.ColorPressuresUniformlyDistributed
    MSH1.TextMatrix(index, 2) = Project.Item(ProjectIndex).cDisplaySettings.SizePressuresUniformlyDistributed
    Select Case Project.Item(ProjectIndex).cDisplaySettings.TextPressuresUniformlyDistributed
        Case None
            MSH1.TextMatrix(index, 4) = "None"
        Case Pressures
            MSH1.TextMatrix(index, 4) = "Pressures"
    End Select
    'Screen
    index = index + 1
    MSH1.Row = index: MSH1.col = 3
    MSH1.CellBackColor = Project.Item(ProjectIndex).cDisplaySettings.ColorScreen
    MSH1.TextMatrix(index, 1) = "---------" '9
    MSH1.TextMatrix(index, 2) = "---------" '9
    MSH1.TextMatrix(index, 4) = "---------" '9
    'Primary Stiffeners
    index = index + 1
    Select Case Project.Item(ProjectIndex).cDisplaySettings.DrawPrimaryStiffeners
        Case No
            MSH1.TextMatrix(index, 1) = "OFF"
        Case Yes
            MSH1.TextMatrix(index, 1) = "ON"
    End Select
    MSH1.TextMatrix(index, 2) = Project.Item(ProjectIndex).cDisplaySettings.SizePrimaryStiffeners
    MSH1.Row = index: MSH1.col = 3
    MSH1.CellBackColor = Project.Item(ProjectIndex).cDisplaySettings.ColorPrimaryStiffeners
    Select Case Project.Item(ProjectIndex).cDisplaySettings.TextPrimaryStiffeners
        Case None
            MSH1.TextMatrix(index, 4) = "None"
        Case WebHeight
            MSH1.TextMatrix(index, 4) = "Web Height"
        Case WebThickness
            MSH1.TextMatrix(index, 4) = "Web Thickness"
        Case WebGrossThickness
            MSH1.TextMatrix(index, 4) = "Web Gross Thickness"
        Case FlangeWidth
            MSH1.TextMatrix(index, 4) = "Flange Width"
        Case FlangeThickness
            MSH1.TextMatrix(index, 4) = "Flange Thickness"
        Case FlangeGrossThickness
            MSH1.TextMatrix(index, 4) = "Flange Gross Thickness"
        Case Spacing
            MSH1.TextMatrix(index, 4) = "Spacing"
        Case All
            MSH1.TextMatrix(index, 4) = "All"
    End Select
    'Secondary Stiffeners
    index = index + 1
    Select Case Project.Item(ProjectIndex).cDisplaySettings.DrawSecondaryStiffeners
        Case No
            MSH1.TextMatrix(index, 1) = "OFF"
        Case Yes
            MSH1.TextMatrix(index, 1) = "ON"
    End Select
    MSH1.TextMatrix(index, 2) = Project.Item(ProjectIndex).cDisplaySettings.SizeSecondaryStiffeners
    MSH1.Row = index: MSH1.col = 3
    MSH1.CellBackColor = Project.Item(ProjectIndex).cDisplaySettings.ColorSecondaryStiffeners
    Select Case Project.Item(ProjectIndex).cDisplaySettings.TextSecondaryStiffeners
      Case None
          MSH1.TextMatrix(index, 4) = "None"
      Case WebHeight
          MSH1.TextMatrix(index, 4) = "Web Height"
      Case WebThickness
          MSH1.TextMatrix(index, 4) = "Web Thickness"
      Case FlangeWidth
          MSH1.TextMatrix(index, 4) = "Flange Width"
      Case FlangeThickness
          MSH1.TextMatrix(index, 4) = "Flange Thickness"
      Case Spacing
          MSH1.TextMatrix(index, 4) = "Spacing"
      Case All
          MSH1.TextMatrix(index, 4) = "All"
    End Select

End Sub

Private Sub SetData()
    Dim index As Integer
    With Project.Item(ProjectIndex).cDisplaySettings
        'Beams
        index = index + 1
        MSH1.Row = index: MSH1.col = 3
        .ColorBeams = MSH1.CellBackColor
        'Fleshes
        index = index + 1
        If MSH1.TextMatrix(index, 1) = "ON" Then
            .DrawFleshes = Yes
        Else
            .DrawFleshes = No
        End If
        MSH1.Row = index: MSH1.col = 3
        .ColorFleshes = MSH1.CellBackColor
        'Primary Frames
        index = index + 1
        If MSH1.TextMatrix(index, 1) = "ON" Then
            .DrawPrimaryFrames = Yes
        Else
            .DrawPrimaryFrames = No
        End If
        .SizePrimaryFrames = MSH1.TextMatrix(index, 2)
        MSH1.Row = index: MSH1.col = 3
        .ColorPrimaryFrames = MSH1.CellBackColor
        With Project.Item(ProjectIndex).cDisplaySettings
            Select Case MSH1.TextMatrix(index, 4)
                Case "None"
                    .TextPrimaryFrames = None
                Case "Web Height"
                    .TextPrimaryFrames = WebHeight
                Case "Web Net Thickness"
                    .TextPrimaryFrames = WebThickness
                Case "Web Gross Thickness"
                    .TextPrimaryFrames = WebGrossThickness
                Case "Flange Width"
                    .TextPrimaryFrames = FlangeWidth
                Case "Flange Net Thickness"
                    .TextPrimaryFrames = FlangeThickness
                Case "Flange Gross Thickness"
                    .TextPrimaryFrames = FlangeGrossThickness
                Case "Spacing"
                    .TextPrimaryFrames = Spacing
                Case "All"
                    .TextPrimaryFrames = All
            End Select
        End With
        'Secondary Frames
        index = index + 1
        If MSH1.TextMatrix(index, 1) = "ON" Then
            .DrawSecondaryFrames = Yes
        Else
            .DrawSecondaryFrames = No
        End If
        .SizeSecondaryFrames = MSH1.TextMatrix(index, 2)
        MSH1.Row = index: MSH1.col = 3
        .ColorSecondaryFrames = MSH1.CellBackColor
        With Project.Item(ProjectIndex).cDisplaySettings
            Select Case MSH1.TextMatrix(index, 4)
                Case "None"
                    .TextSecondaryFrames = None
                Case "Web Height"
                    .TextSecondaryFrames = WebHeight
                Case "Web Thickness"
                    .TextSecondaryFrames = WebThickness
                Case "Flange Width"
                    .TextSecondaryFrames = FlangeWidth
                Case "Flange Thickness"
                    .TextSecondaryFrames = FlangeThickness
                Case "Spacing"
                    .TextSecondaryFrames = Spacing
                Case "All"
                    .TextSecondaryFrames = All
            End Select
        End With
        'Girders
        index = index + 1
        If MSH1.TextMatrix(index, 1) = "ON" Then
            .DrawGirders = Yes
        Else
            .DrawGirders = No
        End If
        .SizeGirders = MSH1.TextMatrix(index, 2)
        MSH1.Row = index: MSH1.col = 3
        .ColorGirders = MSH1.CellBackColor
        With Project.Item(ProjectIndex).cDisplaySettings
            Select Case MSH1.TextMatrix(index, 4)
                Case "None"
                    .TextGirders = None
                Case "Web Height"
                    .TextGirders = WebHeight
                Case "Web Thickness"
                    .TextGirders = WebThickness
                Case "Flange Width"
                    .TextGirders = FlangeWidth
                Case "Flange Thickness"
                    .TextGirders = FlangeThickness
                Case "All"
                    .TextGirders = All
            End Select
        End With
        'Grid
        index = index + 1
        If MSH1.TextMatrix(index, 1) = "ON" Then
            .DrawGrid = Yes
        Else
            .DrawGrid = No
        End If
        .SizeGrid = MSH1.TextMatrix(index, 2)
        MSH1.Row = index: MSH1.col = 3
        .ColorGrid = MSH1.CellBackColor
        'Nodes
        index = index + 1
        If MSH1.TextMatrix(index, 1) = "ON" Then
            .DrawNodes = Yes
        Else
            .DrawNodes = No
        End If
        .SizeNodes = MSH1.TextMatrix(index, 2)
        MSH1.Row = index: MSH1.col = 3
        .ColorNodes = MSH1.CellBackColor
        With Project.Item(ProjectIndex).cDisplaySettings
            Select Case MSH1.TextMatrix(index, 4)
                Case "None"
                    .TextNodes = None
                Case "Numbers"
                    .TextNodes = Numbers
                Case "Coordinates"
                    .TextNodes = Coordinates
                Case "All"
                    .TextNodes = All
            End Select
        End With
        
        'Panels
        index = index + 1
        If MSH1.TextMatrix(index, 1) = "ON" Then
            .DrawPlates = Yes
        Else
            .DrawPlates = No
        End If
        MSH1.Row = index: MSH1.col = 3
        .ColorPlates = MSH1.CellBackColor
        .SizePlates = MSH1.TextMatrix(index, 2)
        Select Case MSH1.TextMatrix(index, 4)
            Case "None"
                .TextPlates = None
            Case "Numbers"
                .TextPlates = Numbers
            Case "Lenght"
                .TextPlates = Lenght
            Case "Net Thickness"
                .TextPlates = NetThickness
            Case "Gross Thickness"
                .TextPlates = GrossThickness
            Case "Corrosion"
                .TextPlates = Corrosion
        End Select
        'Uniform Pressures
        index = index + 1
        If MSH1.TextMatrix(index, 1) = "ON" Then
            .DrawPressuresUniformlyDistributed = Yes
        Else
            .DrawPressuresUniformlyDistributed = No
        End If
        MSH1.Row = index: MSH1.col = 3
        .ColorPressuresUniformlyDistributed = MSH1.CellBackColor
        .SizePressuresUniformlyDistributed = MSH1.TextMatrix(index, 2)
        Select Case MSH1.TextMatrix(index, 4)
            Case "None"
                .TextPressuresUniformlyDistributed = None
            Case "Pressures"
                .TextPressuresUniformlyDistributed = Pressures
        End Select
        'Screen
        index = index + 1
        MSH1.Row = index: MSH1.col = 3
        .ColorScreen = MSH1.CellBackColor
        'Primary Stiffeners
        index = index + 1
        If MSH1.TextMatrix(index, 1) = "ON" Then
            .DrawPrimaryStiffeners = Yes
        Else
            .DrawPrimaryStiffeners = No
        End If
        .SizePrimaryStiffeners = MSH1.TextMatrix(index, 2)
        MSH1.Row = index: MSH1.col = 3
        .ColorPrimaryStiffeners = MSH1.CellBackColor
        With Project.Item(ProjectIndex).cDisplaySettings
            Select Case MSH1.TextMatrix(index, 4)
                Case "None"
                    .TextPrimaryStiffeners = None
                Case "Web Height"
                    .TextPrimaryStiffeners = WebHeight
                Case "Web Net Thickness"
                    .TextPrimaryStiffeners = WebThickness
                Case "Web Gross Thickness"
                    .TextPrimaryStiffeners = WebGrossThickness
                Case "Flange Width"
                    .TextPrimaryStiffeners = FlangeWidth
                Case "Flange Net Thickness"
                    .TextPrimaryStiffeners = FlangeThickness
                Case "Flange Gross Thickness"
                    .TextPrimaryStiffeners = FlangeGrossThickness
                Case "Spacing"
                    .TextPrimaryStiffeners = Spacing
                Case "All"
                    .TextPrimaryStiffeners = All
            End Select
        End With
        'Secondary Stiffeners
        index = index + 1
        If MSH1.TextMatrix(index, 1) = "ON" Then
            .DrawSecondaryStiffeners = Yes
        Else
            .DrawSecondaryStiffeners = No
        End If
        .SizeSecondaryStiffeners = MSH1.TextMatrix(index, 2)
        MSH1.Row = index: MSH1.col = 3
        .ColorSecondaryStiffeners = MSH1.CellBackColor
        With Project.Item(ProjectIndex).cDisplaySettings
            Select Case MSH1.TextMatrix(index, 4)
                Case "None"
                    .TextSecondaryStiffeners = None
                Case "Web Height"
                    .TextSecondaryStiffeners = WebHeight
                Case "Web Thickness"
                    .TextSecondaryStiffeners = WebThickness
                Case "Flange Width"
                    .TextSecondaryStiffeners = FlangeWidth
                Case "Flange Thickness"
                    .TextSecondaryStiffeners = FlangeThickness
                Case "Spacing"
                    .TextSecondaryStiffeners = Spacing
                Case "All"
                    .TextSecondaryStiffeners = All
            End Select
        End With
    End With
End Sub

Private Sub cmdOK_Click()
    SetData
    Project.Item(ProjectIndex).frmProject.picScreen.Cls
    Draw ProjectIndex
    Project.Item(ProjectIndex).DataChanged = True
    Unload Me
End Sub

Private Sub PopulateCombo()
    Combo.Clear
    Select Case MSH1.TextMatrix(MSH1.Row, 0)
        Case "Frames 1", "Stiffeners 1"
            Combo.AddItem "None"
            Combo.AddItem "Web Height"
            Combo.AddItem "Web Net Thickness"
            Combo.AddItem "Web Gross Thickness"
            Combo.AddItem "Flange Width"
            Combo.AddItem "Flange Net Thickness"
            Combo.AddItem "Flange Gross Thickness"
            Combo.AddItem "Spacing"
            'Combo.AddItem "All"
        Case "Frames 2", "Stiffeners 2"
            Combo.AddItem "None"
            Combo.AddItem "Web Height"
            Combo.AddItem "Web Thickness"
            Combo.AddItem "Flange Width"
            Combo.AddItem "Flange Thickness"
            Combo.AddItem "Spacing"
            'Combo.AddItem "All"

        Case "Girders"
            Combo.AddItem "None"
            Combo.AddItem "Web Height"
            Combo.AddItem "Web Thickness"
            Combo.AddItem "Flange Width"
            Combo.AddItem "Flange Thickness"
            'Combo.AddItem "All"
        Case "Panels"
            Combo.AddItem "None"
            Combo.AddItem "Numbers"
            Combo.AddItem "Lenght"
            Combo.AddItem "Net Thickness"
            Combo.AddItem "Gross Thickness"
            Combo.AddItem "Corrosion"
            'Combo.AddItem "All"
        Case "Uniform Pressures"
            Combo.AddItem "None"
            Combo.AddItem "Pressures"
        Case "Nodes"
            Combo.AddItem "None"
            Combo.AddItem "Numbers"
            Combo.AddItem "Coordinates"
    End Select
End Sub

' =============
' FLEXGRID EDIT
' =============
Function grd_index(r As Integer, c As Integer)
    grd_index = c + MSH1.Cols * r
End Function


Private Sub Form_Unload(Cancel As Integer)
    fMainForm.SetFocus
End Sub

Private Sub MSH1_KeyDown(KeyCode As Integer, Shift As Integer)
'    If Len(MSH1) > 0 Then dBuffer = CDbl(MSH1)
    TxtEdit_KeyDown KeyCode, Shift
End Sub

Private Sub MSH1_KeyPress(KeyAscii As Integer)
    If MSH1 = "---------" Then Exit Sub
    If MSH1.col <> 2 Then Exit Sub
    Select Case MSH1.col
        Case 1 To 2
            MSHFlexGridEdit MSH1, TxtEdit, KeyAscii
        Case 3
            
        Case 4
            MSHFlexGridEdit MSH1, Combo, KeyAscii
    End Select
End Sub

Private Sub MSH1_DblClick()
    On Error Resume Next
    Dim i As Integer
    Dim Contor As Integer
    Contor = 0
    Select Case MSH1.col
        Case 1
            Select Case MSH1
                Case "ON"
                    MSH1 = "OFF"
                Case "OFF"
                    MSH1 = "ON"
                Case "---------"
            End Select
        Case 2
            Select Case MSH1
                Case "---------"
                Case Else
                    MSHFlexGridEdit MSH1, TxtEdit, 32
            End Select
        Case 3
            With cdlgColors
                .Color = MSH1.CellBackColor
                .Flags = cdlCCRGBInit
                .ShowColor
                .CancelError = True
                If .Color = 0 Then .Color = 1
                MSH1.CellBackColor = .Color
            End With
        Case 4
            Select Case MSH1
                Case "---------"
                Case Else
                    PopulateCombo
                    MSHFlexGridEdit MSH1, Combo, 32
            End Select
   End Select
End Sub

Sub MSHFlexGridEdit(MSHFlexgrid As Control, edt As Control, KeyAscii As Integer)
    Select Case MSH1.col
        Case 1 To 2
            Select Case KeyAscii 'tasta apasata
            Case 0 To 32
                edt = MSHFlexgrid
                edt.SelStart = 1000
            Case Else
                edt = Chr(KeyAscii)
                edt.SelStart = 1
            End Select
            edt.Move MSHFlexgrid.left + MSHFlexgrid.CellLeft, _
                MSHFlexgrid.top + MSHFlexgrid.CellTop, _
                MSHFlexgrid.CellWidth - 8, _
                MSHFlexgrid.CellHeight - 8
            edt.Visible = True
            edt.SetFocus
        Case 3
        Case 4
                Select Case KeyAscii 'tasta apasata
                Case 0 To 32
                Case Else
                End Select
                edt.Move MSHFlexgrid.left + MSHFlexgrid.CellLeft - 20, _
                    MSHFlexgrid.top + MSHFlexgrid.CellTop - 50, _
                    MSHFlexgrid.CellWidth + 7
                edt.Visible = True
                edt.Text = MSH1
                edt.SetFocus
                dropme Combo
    End Select
End Sub

Private Sub MSH1_Scroll()
    TxtEdit.Visible = False
    Combo.Visible = False
End Sub

Private Sub TxtEdit_KeyPress(KeyAscii As Integer)
    If KeyAscii = Asc(vbCr) Then KeyAscii = 0
End Sub

Private Sub Combo_KeyPress(KeyAscii As Integer)
    If KeyAscii = Asc(vbCr) Then KeyAscii = 0
End Sub

Private Sub TxtEdit_KeyDown(KeyCode As Integer, Shift As Integer)
    dBuffer = (MSH1)
    cmdOK.Default = False
    EditKeyCode MSH1, TxtEdit, KeyCode, Shift
End Sub

Private Sub Combo_KeyDown(KeyCode As Integer, Shift As Integer)
    EditKeyCode MSH1, Combo, KeyCode, Shift
End Sub

Sub EditKeyCode(MSHFlexgrid As Control, edt As Control, KeyCode As Integer, Shift As Integer)
    Select Case KeyCode
    Case 27 ' esc
        edt.Visible = False
        MSHFlexgrid.SetFocus
    Case 13 'enter
        TxtEdit_Validate False
        MSHFlexgrid.SetFocus
    Case 38 'up
        TxtEdit_Validate False
        MSHFlexgrid.SetFocus
        'DoEvents
        If MSHFlexgrid.Row > MSHFlexgrid.FixedRows Then
           MSHFlexgrid.Row = MSHFlexgrid.Row - 1
        End If
    Case 40 'down
        MSHFlexgrid.SetFocus
        TxtEdit_Validate False
        'DoEvents
        If MSHFlexgrid.Row < MSHFlexgrid.FixedRows - 1 Then
           MSHFlexgrid.Row = MSHFlexgrid.Row + 1
        End If
    End Select
End Sub

Private Sub MSH1_GotFocus()
    Select Case MSH1.col
        Case 1 To 2
            If TxtEdit.Visible = False Then Exit Sub
            MSH1 = TxtEdit
            TxtEdit.Visible = False
        Case 3
        Case 4
            If Combo.Visible = False Then Exit Sub
            MSH1 = Combo.List(Combo.ListIndex)
            Combo.Visible = False
    End Select
End Sub

Private Sub MSH1_LeaveCell()
    Select Case MSH1.col
        Case 1 To 2
            If TxtEdit.Visible = False Then Exit Sub
            MSH1 = TxtEdit
            TxtEdit.Visible = False
        Case 3
        Case 4
            If Combo.Visible = False Then Exit Sub
            MSH1 = Combo.List(Combo.ListIndex)
            Combo.Visible = False
    End Select
End Sub

Private Sub TxtEdit_LostFocus()
    MSH1_GotFocus
    cmdOK.Enabled = True
End Sub

Private Sub Combo_LostFocus()
    MSH1_GotFocus
End Sub

Private Sub TxtEdit_Validate(Cancel As Boolean)
    ValidateNonNullPozitive TxtEdit, Cancel
    If Cancel = True Then TxtEdit = dBuffer
End Sub
