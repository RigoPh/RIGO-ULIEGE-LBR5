VERSION 5.00
Object = "{831FDD16-0C5C-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCTL.OCX"
Object = "{80F7E9EA-292E-4054-B3DB-CB675BD6FA04}#1.0#0"; "CSPSHEET.OCX"
Begin VB.Form frmDisplayOptions 
   BorderStyle     =   5  'Sizable ToolWindow
   Caption         =   "Display Options"
   ClientHeight    =   5895
   ClientLeft      =   1050
   ClientTop       =   2325
   ClientWidth     =   4695
   Icon            =   "frmDisplayOptions.frx":0000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   5895
   ScaleWidth      =   4695
   ShowInTaskbar   =   0   'False
   Begin VB.CheckBox chkShowLegend 
      Caption         =   "Show Legend On Screen"
      Height          =   375
      Left            =   120
      TabIndex        =   7
      Top             =   5400
      Width           =   2175
   End
   Begin PropertySheet.TPropertySheet PS 
      Height          =   5295
      Left            =   0
      TabIndex        =   6
      Top             =   0
      Width           =   4695
      _ExtentX        =   8281
      _ExtentY        =   9340
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      AllowEmptyValues=   0   'False
      CatBackColor    =   -2147483646
      BeginProperty CatFont {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Appearance      =   0
      BorderStyle     =   0
      ShowDescription =   0   'False
      ShowToolbar     =   0   'False
   End
   Begin MSComctlLib.ImageList ImgLst 
      Left            =   2760
      Top             =   5280
      _ExtentX        =   794
      _ExtentY        =   794
      BackColor       =   16777215
      ImageWidth      =   16
      ImageHeight     =   16
      MaskColor       =   12632256
      _Version        =   393216
      BeginProperty Images {2C247F25-8591-11D1-B16A-00C0F0283628} 
         NumListImages   =   25
         BeginProperty ListImage1 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmDisplayOptions.frx":000C
            Key             =   ""
         EndProperty
         BeginProperty ListImage2 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmDisplayOptions.frx":011E
            Key             =   ""
         EndProperty
         BeginProperty ListImage3 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmDisplayOptions.frx":0230
            Key             =   ""
         EndProperty
         BeginProperty ListImage4 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmDisplayOptions.frx":0342
            Key             =   ""
         EndProperty
         BeginProperty ListImage5 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmDisplayOptions.frx":0454
            Key             =   ""
         EndProperty
         BeginProperty ListImage6 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmDisplayOptions.frx":0566
            Key             =   ""
         EndProperty
         BeginProperty ListImage7 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmDisplayOptions.frx":0678
            Key             =   ""
         EndProperty
         BeginProperty ListImage8 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmDisplayOptions.frx":078A
            Key             =   ""
         EndProperty
         BeginProperty ListImage9 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmDisplayOptions.frx":089C
            Key             =   ""
         EndProperty
         BeginProperty ListImage10 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmDisplayOptions.frx":09AE
            Key             =   ""
         EndProperty
         BeginProperty ListImage11 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmDisplayOptions.frx":0AC0
            Key             =   ""
         EndProperty
         BeginProperty ListImage12 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmDisplayOptions.frx":0BD2
            Key             =   ""
         EndProperty
         BeginProperty ListImage13 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmDisplayOptions.frx":0CE4
            Key             =   ""
         EndProperty
         BeginProperty ListImage14 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmDisplayOptions.frx":0DF6
            Key             =   ""
         EndProperty
         BeginProperty ListImage15 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmDisplayOptions.frx":0F08
            Key             =   ""
         EndProperty
         BeginProperty ListImage16 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmDisplayOptions.frx":101A
            Key             =   ""
         EndProperty
         BeginProperty ListImage17 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmDisplayOptions.frx":112C
            Key             =   ""
         EndProperty
         BeginProperty ListImage18 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmDisplayOptions.frx":123E
            Key             =   ""
         EndProperty
         BeginProperty ListImage19 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmDisplayOptions.frx":1350
            Key             =   ""
         EndProperty
         BeginProperty ListImage20 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmDisplayOptions.frx":1462
            Key             =   ""
         EndProperty
         BeginProperty ListImage21 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmDisplayOptions.frx":1574
            Key             =   ""
         EndProperty
         BeginProperty ListImage22 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmDisplayOptions.frx":1686
            Key             =   ""
         EndProperty
         BeginProperty ListImage23 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmDisplayOptions.frx":1798
            Key             =   ""
         EndProperty
         BeginProperty ListImage24 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmDisplayOptions.frx":18AA
            Key             =   ""
         EndProperty
         BeginProperty ListImage25 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmDisplayOptions.frx":19BC
            Key             =   ""
         EndProperty
      EndProperty
   End
   Begin VB.PictureBox picOptions 
      BorderStyle     =   0  'None
      Height          =   3780
      Index           =   3
      Left            =   -20000
      ScaleHeight     =   3780
      ScaleWidth      =   5685
      TabIndex        =   2
      TabStop         =   0   'False
      Top             =   480
      Width           =   5685
      Begin VB.Frame fraSample4 
         Caption         =   "Sample 4"
         Height          =   1785
         Left            =   2100
         TabIndex        =   5
         Top             =   840
         Width           =   2055
      End
   End
   Begin VB.PictureBox picOptions 
      BorderStyle     =   0  'None
      Height          =   3780
      Index           =   2
      Left            =   -20000
      ScaleHeight     =   3780
      ScaleWidth      =   5685
      TabIndex        =   1
      TabStop         =   0   'False
      Top             =   480
      Width           =   5685
      Begin VB.Frame fraSample3 
         Caption         =   "Sample 3"
         Height          =   1785
         Left            =   1545
         TabIndex        =   4
         Top             =   675
         Width           =   2055
      End
   End
   Begin VB.PictureBox picOptions 
      BorderStyle     =   0  'None
      Height          =   3780
      Index           =   1
      Left            =   -20000
      ScaleHeight     =   3780
      ScaleWidth      =   5685
      TabIndex        =   0
      TabStop         =   0   'False
      Top             =   480
      Width           =   5685
      Begin VB.Frame fraSample2 
         Caption         =   "Sample 2"
         Height          =   1785
         Left            =   645
         TabIndex        =   3
         Top             =   300
         Width           =   2055
      End
   End
End
Attribute VB_Name = "frmDisplayOptions"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Dim ProjectIndex As Integer

Private Sub SetProperties(ByVal Prop As PropertySheet.TProperty, NewValue As Variant, Cancel As Boolean)
    With Project.Item(ProjectIndex).cDisplaySettings
        Select Case Prop.Caption
            'Screen
            Case "Screen Color"
                .ColorScreen = NewValue
            'Nodes
            Case "Node Color"
                .ColorNodes = NewValue
            Case "Node Caption"
                .TextNodes = Project.Item(ProjectIndex).cDisplaySettings.setTextOptionsString(NewValue)
            Case "Node Visibility"
                .DrawNodes = Project.Item(ProjectIndex).cDisplaySettings.setItemState(NewValue)
            'Plates
            Case "Plate Color"
                .ColorPlates = NewValue
            Case "Plate Scale"
                .SizePlates = NewValue
            Case "Plate Caption"
                .TextPlates = Project.Item(ProjectIndex).cDisplaySettings.setTextOptionsString(NewValue)
            Case "Plate Visibility"
                .DrawPlates = Project.Item(ProjectIndex).cDisplaySettings.setItemState(NewValue)
            Case "Plate Participation Visibility"
                .DrawParticipation = Project.Item(ProjectIndex).cDisplaySettings.setItemState(NewValue)
            'Beams
            Case "Beam Color"
                .ColorBeams = NewValue
            Case "Beam Scale"
                .SizeBeams = NewValue
            Case "Beam Caption"
                .TextBeams = Project.Item(ProjectIndex).cDisplaySettings.setTextOptionsString(NewValue)
            Case "Beam Visibility"
                .DrawBeams = Project.Item(ProjectIndex).cDisplaySettings.setItemState(NewValue)
            'Primary Frames
            Case "Primary Frames Color"
                .ColorPrimaryFrames = NewValue
            Case "Primary Frames Scale"
                .SizePrimaryFrames = NewValue
            Case "Primary Frames Caption"
                .TextPrimaryFrames = Project.Item(ProjectIndex).cDisplaySettings.setTextOptionsString(NewValue)
            Case "Primary Frames Visibility"
                .DrawPrimaryFrames = Project.Item(ProjectIndex).cDisplaySettings.setItemState(NewValue)
            'Primary Stiffeners
            Case "Primary Stiffeners Color"
                .ColorPrimaryStiffeners = NewValue
            Case "Primary Stiffeners Scale"
                .SizePrimaryStiffeners = NewValue
            Case "Primary Stiffeners Caption"
                .TextPrimaryStiffeners = Project.Item(ProjectIndex).cDisplaySettings.setTextOptionsString(NewValue)
            Case "Primary Stiffeners Visibility"
                .DrawPrimaryStiffeners = Project.Item(ProjectIndex).cDisplaySettings.setItemState(NewValue)
                
            'Secondary Frames
            Case "Secondary Frames Color"
                .ColorSecondaryFrames = NewValue
            Case "Secondary Frames Scale"
                .SizeSecondaryFrames = NewValue
            Case "Secondary Frames Caption"
                .TextSecondaryFrames = Project.Item(ProjectIndex).cDisplaySettings.setTextOptionsString(NewValue)
            Case "Secondary Frames Visibility"
                .DrawSecondaryFrames = Project.Item(ProjectIndex).cDisplaySettings.setItemState(NewValue)
            'Secondary Stiffeners
            Case "Secondary Stiffeners Color"
                .ColorSecondaryStiffeners = NewValue
            Case "Secondary Stiffeners Scale"
                .SizeSecondaryStiffeners = NewValue
            Case "Secondary Stiffeners Caption"
                .TextSecondaryStiffeners = Project.Item(ProjectIndex).cDisplaySettings.setTextOptionsString(NewValue)
            Case "Secondary Stiffeners Visibility"
                .DrawSecondaryStiffeners = Project.Item(ProjectIndex).cDisplaySettings.setItemState(NewValue)
            'Girders
            Case "Girders Color"
                .ColorGirders = NewValue
            Case "Girders Scale"
                .SizeGirders = NewValue
            Case "Girders Caption"
                .TextGirders = Project.Item(ProjectIndex).cDisplaySettings.setTextOptionsString(NewValue)
            Case "Girders Visibility"
                .DrawGirders = Project.Item(ProjectIndex).cDisplaySettings.setItemState(NewValue)
            'Grid
            Case "Grid Color"
                .ColorGrid = NewValue
            Case "Grid Scale"
                .SizeGrid = NewValue
            Case "Grid Visibility"
                .DrawGrid = Project.Item(ProjectIndex).cDisplaySettings.setItemState(NewValue)
            'Uniform Pressures
            Case "Uniform Pressures Color"
                .ColorPressuresUniformlyDistributed = NewValue
            Case "Uniform Pressures Scale"
                .SizePressuresUniformlyDistributed = NewValue
            Case "Uniform Pressures Caption"
                .TextPressuresUniformlyDistributed = Project.Item(ProjectIndex).cDisplaySettings.setTextOptionsString(NewValue)
            Case "Uniform Pressures Visibility"
                .DrawPressuresUniformlyDistributed = Project.Item(ProjectIndex).cDisplaySettings.setItemState(NewValue)
            'Localized Pressures
            Case "Localized Pressures Color"
                .ColorLocalizedPressure = NewValue
            Case "Localized Pressures Scale"
                .SizeLocalizedPressure = NewValue
            Case "Localized Pressures Caption"
                .TextLocalizedPressure = Project.Item(ProjectIndex).cDisplaySettings.setTextOptionsString(NewValue)
            Case "Localized Pressures Visibility"
                .DrawLocalizedPressure = Project.Item(ProjectIndex).cDisplaySettings.setItemState(NewValue)
            'Boundary Conditions
            Case "Boundary Conditions Color"
                .ColorBoundaryConditions = NewValue
            Case "Boundary Conditions Caption"
                .TextBoundaryConditions = Project.Item(ProjectIndex).cDisplaySettings.setTextOptionsString(NewValue)
            Case "Boundary Conditions Visibility"
                .DrawBoundaryConditions = Project.Item(ProjectIndex).cDisplaySettings.setItemState(NewValue)
            'Flow Fleshes
            Case "Flesh Color"
                .ColorFleshes = NewValue
            Case "Flesh Scale"
                .SizeFleshes = NewValue
            Case "Flesh Visibility"
                .DrawFleshes = Project.Item(ProjectIndex).cDisplaySettings.setItemState(NewValue)
        End Select
    End With
    Draw ProjectIndex
'    DisplayList
End Sub

Private Sub GetProperties()
    
    With PS
        .Redraw = False
        .ImageList = ImgLst
        With .Categories
            'Screen
            With .Add("Screen", , , False).Properties
                .Add "Screen Color", Project.Item(ProjectIndex).cDisplaySettings.ColorScreen, psColor, , , , "Returns/sets the back color of the Screen"
            End With
            'Nodes
            With .Add("Nodes", , , False).Properties
                .Add "Node Color", Project.Item(ProjectIndex).cDisplaySettings.ColorNodes, psColor, , , , "Returns/sets the back color of the Nodes"
                With .Add("Node Caption", Project.Item(ProjectIndex).cDisplaySettings.getTextOptionsString(Project.Item(ProjectIndex).cDisplaySettings.TextNodes), psDropDownList).ListValues
                    .Add "None", "None"
                    .Add "Numbers", "Numbers"
                    .Add "Coordinates", "Coordinates"
                End With
                With .Add("Node Visibility", Project.Item(ProjectIndex).cDisplaySettings.getItemState(Project.Item(ProjectIndex).cDisplaySettings.DrawNodes), psDropDownList).ListValues
                    .Add "On", "On"
                    .Add "Off", "Off"
                End With
            End With
            'Plates
            With .Add("Plates", , , False).Properties
                .Add "Plate Color", Project.Item(ProjectIndex).cDisplaySettings.ColorPlates, psColor, , , "Plate Color", "Returns/sets the color of the Plate panels"
                With .Add("Plate Scale", Project.Item(ProjectIndex).cDisplaySettings.SizePlates, psDouble, , , "Plate Scale", "Returns/sets the scale of the Plate panels")
                    .UpDownIncrement = 1
                End With
                With .Add("Plate Caption", Project.Item(ProjectIndex).cDisplaySettings.getTextOptionsString(Project.Item(ProjectIndex).cDisplaySettings.TextPlates), psDropDownList).ListValues
                    .Add "None", "None"
                    .Add "Numbers", "Numbers"
                    .Add "Lenght", "Lenght"
                    .Add "Net Thickness", "Net Thickness"
                    .Add "Gross Thickness", "Gross Thickness"
                    .Add "Corrosion", "Corrosion"
                    .Add "Participation", "Participation"
                End With
                With .Add("Plate Visibility", Project.Item(ProjectIndex).cDisplaySettings.getItemState(Project.Item(ProjectIndex).cDisplaySettings.DrawPlates), psDropDownList).ListValues
                    .Add "On", "On"
                    .Add "Off", "Off"
                End With
                With .Add("Plate Participation Visibility", Project.Item(ProjectIndex).cDisplaySettings.getItemState(Project.Item(ProjectIndex).cDisplaySettings.DrawParticipation), psDropDownList).ListValues
                    .Add "On", "On"
                    .Add "Off", "Off"
                End With
            End With
            'Beams
            With .Add("Beams", , , False).Properties
                .Add "Beam Color", Project.Item(ProjectIndex).cDisplaySettings.ColorBeams, psColor, , , "Beam Color", "Returns/sets the color of the Beam panels"
                With .Add("Beam Scale", Project.Item(ProjectIndex).cDisplaySettings.SizeBeams, psDouble, , , "Beam Scale", "Returns/sets the scale of the Beam panels")
                    .UpDownIncrement = 1
                End With
                With .Add("Beam Caption", Project.Item(ProjectIndex).cDisplaySettings.getTextOptionsString(Project.Item(ProjectIndex).cDisplaySettings.TextBeams), psDropDownList).ListValues
                    .Add "None", "None"
                    .Add "Numbers", "Numbers"
                    .Add "Lenght", "Lenght"
                    .Add "Net Thickness", "Net Thickness"
                    .Add "Gross Thickness", "Gross Thickness"
                    .Add "Corrosion", "Corrosion"
                End With
                With .Add("Beam Visibility", Project.Item(ProjectIndex).cDisplaySettings.getItemState(Project.Item(ProjectIndex).cDisplaySettings.DrawBeams), psDropDownList).ListValues
                    .Add "On", "On"
                    .Add "Off", "Off"
                End With
            End With
            'Primary Frames
            With .Add("Primary Frames", , , False).Properties
                .Add "Primary Frames Color", Project.Item(ProjectIndex).cDisplaySettings.ColorPrimaryFrames, psColor, , , "Primary Frames Color", "Returns/sets the color of the Primary Frames"
                With .Add("Primary Frames Scale", Project.Item(ProjectIndex).cDisplaySettings.SizePrimaryFrames, psDouble, , , "Primary Frames scale", "Returns/sets the scale of the Primary Frames")
                    .UpDownIncrement = 1
                End With
                With .Add("Primary Frames Caption", Project.Item(ProjectIndex).cDisplaySettings.getTextOptionsString(Project.Item(ProjectIndex).cDisplaySettings.TextPrimaryFrames), psDropDownList).ListValues
                    .Add "None", "None"
                    .Add "Web Height", "Web Height"
                    .Add "Web Thickness", "Web Thickness"
                    .Add "Web Gross Thickness", "Web Gross Thickness"
                    .Add "Flange Width", "Flange Width"
                    .Add "Flange Thickness", "Flange Thickness"
                    .Add "Flange Gross Thickness", "Flange Gross Thickness"
                    .Add "Spacing", "Spacing"
                End With
                With .Add("Primary Frames Visibility", Project.Item(ProjectIndex).cDisplaySettings.getItemState(Project.Item(ProjectIndex).cDisplaySettings.DrawPrimaryFrames), psDropDownList).ListValues
                    .Add "On", "On"
                    .Add "Off", "Off"
                End With
            End With
            'Primary Stiffeners
            With .Add("Primary Stiffeners", , , False).Properties
                .Add "Primary Stiffeners Color", Project.Item(ProjectIndex).cDisplaySettings.ColorPrimaryStiffeners, psColor, , , "Primary Stiffeners Color", "Returns/sets the color of the Primary Stiffeners"
                With .Add("Primary Stiffeners Scale", Project.Item(ProjectIndex).cDisplaySettings.SizePrimaryStiffeners, psDouble, , , "Primary Stiffeners scale", "Returns/sets the scale of the Primary Stiffeners")
                    .UpDownIncrement = 1
                End With
                With .Add("Primary Stiffeners Caption", Project.Item(ProjectIndex).cDisplaySettings.getTextOptionsString(Project.Item(ProjectIndex).cDisplaySettings.TextPrimaryStiffeners), psDropDownList).ListValues
                    .Add "None", "None"
                    .Add "Web Height", "Web Height"
                    .Add "Web Thickness", "Web Thickness"
                    .Add "Web Gross Thickness", "Web Gross Thickness"
                    .Add "Flange Width", "Flange Width"
                    .Add "Flange Thickness", "Flange Thickness"
                    .Add "Flange Gross Thickness", "Flange Gross Thickness"
                    .Add "Spacing", "Spacing"
                    .Add "Gross Section Modulus", "Gross Section Modulus"
                    '.Add "All", "All"
                End With
                With .Add("Primary Stiffeners Visibility", Project.Item(ProjectIndex).cDisplaySettings.getItemState(Project.Item(ProjectIndex).cDisplaySettings.DrawPrimaryStiffeners), psDropDownList).ListValues
                    .Add "On", "On"
                    .Add "Off", "Off"
                End With
            End With
            
            'Secondary Frames
            With .Add("Secondary Frames", , , False).Properties
                .Add "Secondary Frames Color", Project.Item(ProjectIndex).cDisplaySettings.ColorSecondaryFrames, psColor, , , "Secondary Frames Color", "Returns/sets the color of the Secondary Frames"
                With .Add("Secondary Frames Scale", Project.Item(ProjectIndex).cDisplaySettings.SizeSecondaryFrames, psDouble, , , "Secondary Frames scale", "Returns/sets the scale of the Secondary Frames")
                    .UpDownIncrement = 1
                End With
                With .Add("Secondary Frames Caption", Project.Item(ProjectIndex).cDisplaySettings.getTextOptionsString(Project.Item(ProjectIndex).cDisplaySettings.TextSecondaryFrames), psDropDownList).ListValues
                    .Add "None", "None"
                    .Add "Web Height", "Web Height"
                    .Add "Web Thickness", "Web Thickness"
                    '.Add "Web Gross Thickness", "Web Gross Thickness"
                    .Add "Flange Width", "Flange Width"
                    .Add "Flange Thickness", "Flange Thickness"
                    '.Add "Flange Gross Thickness", "Flange Gross Thickness"
                    .Add "Spacing", "Spacing"
                End With
                With .Add("Secondary Frames Visibility", Project.Item(ProjectIndex).cDisplaySettings.getItemState(Project.Item(ProjectIndex).cDisplaySettings.DrawSecondaryFrames), psDropDownList).ListValues
                    .Add "On", "On"
                    .Add "Off", "Off"
                End With
            End With
            'Secondary Stiffeners
            With .Add("Secondary Stiffeners", , , False).Properties
                .Add "Secondary Stiffeners Color", Project.Item(ProjectIndex).cDisplaySettings.ColorSecondaryStiffeners, psColor, , , "Secondary Stiffeners Color", "Returns/sets the color of the Secondary Stiffeners"
                With .Add("Secondary Stiffeners Scale", Project.Item(ProjectIndex).cDisplaySettings.SizeSecondaryStiffeners, psDouble, , , "Secondary Stiffeners scale", "Returns/sets the scale of the Secondary Stiffeners")
                    .UpDownIncrement = 1
                End With
                With .Add("Secondary Stiffeners Caption", Project.Item(ProjectIndex).cDisplaySettings.getTextOptionsString(Project.Item(ProjectIndex).cDisplaySettings.TextSecondaryStiffeners), psDropDownList).ListValues
                    .Add "None", "None"
                    .Add "Web Height", "Web Height"
                    '.Add "Web Net Thickness", "Web Net Thickness"
                    .Add "Web Thickness", "Web Thickness"
                    .Add "Flange Width", "Flange Width"
                    '.Add "Flange Net Thickness", "Flange Net Thickness"
                    .Add "Flange Thickness", "Flange Thickness"
                    .Add "Spacing", "Spacing"
                End With
                With .Add("Secondary Stiffeners Visibility", Project.Item(ProjectIndex).cDisplaySettings.getItemState(Project.Item(ProjectIndex).cDisplaySettings.DrawSecondaryStiffeners), psDropDownList).ListValues
                    .Add "On", "On"
                    .Add "Off", "Off"
                End With
            End With
            'Girders
            With .Add("Girders", , , False).Properties
                .Add "Girders Color", Project.Item(ProjectIndex).cDisplaySettings.ColorGirders, psColor, , , "Girders Color", "Returns/sets the color of the Girders"
                With .Add("Girders Scale", Project.Item(ProjectIndex).cDisplaySettings.SizeGirders, psDouble, , , "Girders scale", "Returns/sets the scale of the Girders")
                    .UpDownIncrement = 1
                End With
                With .Add("Girders Caption", Project.Item(ProjectIndex).cDisplaySettings.getTextOptionsString(Project.Item(ProjectIndex).cDisplaySettings.TextGirders), psDropDownList).ListValues
                    .Add "None", "None"
                    .Add "Web Height", "Web Height"
                    '.Add "Web Net Thickness", "Web Net Thickness"
                    .Add "Web Thickness", "Web Thickness"
                    .Add "Flange Width", "Flange Width"
                    '.Add "Flange Net Thickness", "Flange Net Thickness"
                    .Add "Flange Thickness", "Flange Thickness"
                    .Add "Distance From In Node", "Distance From In Node"
                    '.Add "Gross Section Modulus", "Gross Section Modulus"
                    '.Add "All", "All"
                End With
                With .Add("Girders Visibility", Project.Item(ProjectIndex).cDisplaySettings.getItemState(Project.Item(ProjectIndex).cDisplaySettings.DrawGirders), psDropDownList).ListValues
                    .Add "On", "On"
                    .Add "Off", "Off"
                End With
            End With
            'Lateral Uniform Pressures
            With .Add("Lateral Uniform Pressures", , , False).Properties
                .Add "Uniform Pressures Color", Project.Item(ProjectIndex).cDisplaySettings.ColorPressuresUniformlyDistributed, psColor, , , "Pressures Color", "Returns/sets the color of the uniform pressures"
                With .Add("Uniform Pressures Scale", Project.Item(ProjectIndex).cDisplaySettings.SizePressuresUniformlyDistributed, psDouble, , , "Pressures Color", "Returns/sets the scale of the uniform pressures")
                    .UpDownIncrement = 0.1
                End With
                With .Add("Uniform Pressures Caption", Project.Item(ProjectIndex).cDisplaySettings.getTextOptionsString(Project.Item(ProjectIndex).cDisplaySettings.TextPressuresUniformlyDistributed), psDropDownList).ListValues
                    .Add "None", "None"
                    .Add "Pressures", "Pressures"
                End With
                With .Add("Uniform Pressures Visibility", Project.Item(ProjectIndex).cDisplaySettings.getItemState(Project.Item(ProjectIndex).cDisplaySettings.DrawPressuresUniformlyDistributed), psDropDownList).ListValues
                    .Add "On", "On"
                    .Add "Off", "Off"
                End With
            End With
            'Localized Pressures
            With .Add("Localized Pressures", , , False).Properties
                .Add "Localized Pressures Color", Project.Item(ProjectIndex).cDisplaySettings.ColorLocalizedPressure, psColor, , , "Localized Pressures Color", "Returns/sets the color of the localized pressures"
                With .Add("Localized Pressures Scale", Project.Item(ProjectIndex).cDisplaySettings.SizeLocalizedPressure, psDouble, , , "Localized Pressures Color", "Returns/sets the scale of the localized pressures")
                    .UpDownIncrement = 0.3
                End With
                With .Add("Localized Pressures Caption", Project.Item(ProjectIndex).cDisplaySettings.getTextOptionsString(Project.Item(ProjectIndex).cDisplaySettings.TextLocalizedPressure), psDropDownList).ListValues
                    .Add "None", "None"
                    .Add "Pressures", "Pressures"
                End With
                With .Add("Localized Pressures Visibility", Project.Item(ProjectIndex).cDisplaySettings.getItemState(Project.Item(ProjectIndex).cDisplaySettings.DrawLocalizedPressure), psDropDownList).ListValues
                    .Add "On", "On"
                    .Add "Off", "Off"
                End With
            End With
            'Boundary Conditions
            With .Add("Boundary Conditions", , , False).Properties
                .Add "Boundary Conditions Color", Project.Item(ProjectIndex).cDisplaySettings.ColorBoundaryConditions, psColor, , , "Pressures Color", "Returns/sets the color of the boundary conditions"
                With .Add("Boundary Conditions Caption", Project.Item(ProjectIndex).cDisplaySettings.getTextOptionsString(Project.Item(ProjectIndex).cDisplaySettings.TextBoundaryConditions), psDropDownList).ListValues
                    .Add "None", "None"
                    .Add "Boundaries", "Boundaries"
                End With
                With .Add("Boundary Conditions Visibility", Project.Item(ProjectIndex).cDisplaySettings.getItemState(Project.Item(ProjectIndex).cDisplaySettings.DrawBoundaryConditions), psDropDownList).ListValues
                    .Add "On", "On"
                    .Add "Off", "Off"
                End With
            End With

            'End With
            'Grid
            With .Add("Grid", , , False).Properties
                .Add "Grid Color", Project.Item(ProjectIndex).cDisplaySettings.ColorGrid, psColor, , , "Grid Color", "Returns/sets the color of the grid"
                With .Add("Grid Scale", Project.Item(ProjectIndex).cDisplaySettings.SizeGrid, psDouble, , , "Grid Scale", "Returns/sets the scale of the grid")
                    .UpDownIncrement = 1
                End With
                With .Add("Grid Visibility", Project.Item(ProjectIndex).cDisplaySettings.getItemState(Project.Item(ProjectIndex).cDisplaySettings.DrawGrid), psDropDownList).ListValues
                    .Add "On", "On"
                    .Add "Off", "Off"
                End With
            End With
            'Fleshes
            With .Add("Panel Flow", , , False).Properties
                .Add "Flesh Color", Project.Item(ProjectIndex).cDisplaySettings.ColorFleshes, psColor, , , "Grid Color", "Returns/sets the color of the fleshes"
                With .Add("Flesh Scale", Project.Item(ProjectIndex).cDisplaySettings.SizeFleshes, psDouble, , , "Flesh Scale", "Returns/sets the scale of the flesh")
                    .UpDownIncrement = 1
                End With
                With .Add("Flesh Visibility", Project.Item(ProjectIndex).cDisplaySettings.getItemState(Project.Item(ProjectIndex).cDisplaySettings.DrawFleshes), psDropDownList).ListValues
                    .Add "On", "On"
                    .Add "Off", "Off"
                End With
            End With
        End With
        .Redraw = True
        '.Refresh
    End With


'    With PS2
''<Added by: Project Administrator at: 1/4/2004-19:59:14 on machine: ZEUS>
'        .Redraw = False
''</Added by: Project Administrator at: 1/4/2004-19:59:14 on machine: ZEUS>
'        .ImageList = ImageList1
'        .ShowToolTips = True
'        With .Categories
'            With .Add("Appearance", , "@Use these properties to change the" & vbCrLf & "PropertyList2 appearance").Properties
'                .Add "BackColor", PS2.BackColor, psColor, , psImgBackColor, , "Returns/sets the background color of the object"
'                .Add "CatBackColor", PS2.CatBackColor, psColor, , psImgBackColor, , "Returns/sets catagory cell background color"
'                .Add "SelBackColor", PS2.SelBackColor, psColor, , psImgBackColor, , "Returns/sets selection background color"
'                .Add "CatForeColor", PS2.CatForeColor, psColor, , psImgFontColor, , "Returns/sets category foreground color used to display text and graphics of an object"
'                .Add "ForeColor", PS2.ForeColor, psColor, , psImgFontColor, , "Returns/sets foreground color used to display text and graphics of an object"
'                .Add "SelForeColor", PS2.SelForeColor, psColor, , psImgFontColor, , "Returns/sets selection foreground color used to display text and graphics of an object"
'                .Add "GridColor", PS2.GridColor, psColor, , psImgLineColor, , "Returns/sets object grid color"
'                With .Add("BorderStyle", 0, psDropDownList)
'                    .ListValues.Add 0, "0 - psBorderNone"
'                    .ListValues.Add 1, "1 - psBorderSingle"
'                    .Value = PS2.BorderStyle
'                    .Description = "Returns/sets the border style for the object"
'                End With
'                With .Add("Appearance", 0, psDropDownList)
'                    .ListValues.Add 0, "0 - psFlat"
'                    .ListValues.Add 1, "1 - ps3D"
'                    .Value = PS2.Appearance
'                End With
'                .Add("CatFont", PS2.CatFont, psFont).Format = "n (c)"
'                .Add "Font", PS2.Font, psFont
'                .Add "NameWidth", PS2.NameWidth
'                With .Add("ShowCategories", PS2.ShowCategories)
'                    With .ListValues
'                        .Item(1).Caption = "No"
'                        .Item(2).Caption = "Yes"
'                    End With
'                End With
'                With .Add("Tooltips", PS2.ShowToolTips, , , psImgPicture2)
'                    With .ListValues
'                        .Item(1).Caption = "Hide"
'                        .Item(2).Caption = "Show"
'                    End With
'                End With
'                .Add "ShowToolbar", True
'                .Add "ShowDescription", False
''<Added by: Project Administrator at: 31/3/2004-21:16:10 on machine: ZEUS>
'                With .Add("EffectStyle", psNormal, psDropDownList)
'                    With .ListValues
'                        .Add psNormal, "psNormal"
'                        .Add psSmooth, "psSmooth"
'                    End With
'                End With
''</Added by: Project Administrator at: 31/3/2004-21:16:10 on machine: ZEUS>
'                With .Add("DescriptionHeight", PS2.DescriptionHeight, psInteger)
'                    .UpDownIncrement = 50
'                End With
'            End With
'            With .Add("Behavior", , "@Set the control behavior").Properties
'                .Add "AllowEmptyValues", PS2.AllowEmptyValues
'                .Add "AutoSelect", PS2.AutoSelect
'                .Add "Expandable Categories", PS2.ExpandableCategories
'                .Add "RequiresEnter", PS2.RequiresEnter
'                .Add "Visible", True
'            End With
'            With .Add("Misc", , "@Other properties")
'                With .Properties
'                    .Add("(About)", "", psCustom, , psImgNotes, "Click the button for information about this control", "Show propertysheet about box").ForeColor = vbBlue
'                    .Add("(Revisions)", "", psCustom, , psImgWebPage, "Click the button for revision file", "Open the revision file for PropertySheet control").ForeColor = vbBlue
'                    .Add("(Readme)", "", psCustom, , psImgFile, "Click the button for read-me file", "Open the readme file for PropertySheet control").ForeColor = vbBlue
'                End With
'            End With
'            With .Add("Position", , "@Fields in red are read-only")
'                With .Properties
'                    With .Add("Left", PS2.Left, , True)
'                        .ForeColor = vbRed
'                    End With
'                    .Add("Width", PS2.Width, , , psImgWidth).SetRange 2100, 2790
'                    .Add("Height", PS2.Height, , , psImgHeight).SetRange 100, 3300
'                    .Add("Top", PS2.Top, , True).ForeColor = vbRed
'                End With
'            End With
'            With .Add("Formats").Properties
'                With .Add("ColorFormat", "RRGGBB", psCombo).ListValues
'                    .Add "&HeH&", "VB"
'                    .Add "$e", "Delphi"
'                    .Add "#m", "HTML"
'                    .Add "r g b", "Red Green Blue"
'                End With
'                With .Add("Date Format", "dd-MMM-yyyy", psCombo).ListValues
'                    .Add "Long Date"
'                    .Add "Medium Date"
'                    .Add "Short Date"
'                    .Add """Today is"" dddd dd "", a really nice day.""", "Really Long Date"
'                End With
'                With .Add("Boolean Format", 0, psBoolean).ListValues
'                    .Item(1).Caption = "Like combobox"
'                    .Item(2).Caption = "Like checkbox"
'                End With
'            End With
'        End With
''<Added by: Project Administrator at: 1/4/2004-19:59:24 on machine: ZEUS>
'        .Redraw = True
''</Added by: Project Administrator at: 1/4/2004-19:59:24 on machine: ZEUS>
'    End With
End Sub

Private Sub chkShowLegend_Click()
    If chkShowLegend = vbChecked Then
        Project.Item(ProjectIndex).cDisplaySettings.ShowLegend = yes
    Else
        Project.Item(ProjectIndex).cDisplaySettings.ShowLegend = no
    End If
    Draw ProjectIndex
End Sub

Private Sub Form_Load()
    ProjectIndex = ActiveProject
    Me.Move 0, (Screen.Height - Me.Height) / 2
    Me.Caption = "Display Settings - [" & GetFileName(Project.Item(ProjectIndex).sFileName) & "]"
    GetProperties
    If Project.Item(ProjectIndex).cDisplaySettings.ShowLegend = yes Then
        chkShowLegend = vbChecked
    Else
        chkShowLegend = vbUnchecked
    End If
End Sub

Private Sub Form_Resize()
    PS.Left = Me.ScaleLeft
    PS.Width = Me.ScaleWidth
    PS.Top = Me.ScaleTop
    PS.Height = IIf(Me.ScaleHeight - 500 > 0, Me.ScaleHeight - 500, 0)
    PS.NameWidth = PS.Width / 2
    chkShowLegend.Left = PS.Left
    chkShowLegend.Top = PS.Top + PS.Height + 100
End Sub

Private Sub Form_Unload(Cancel As Integer)
    fMainForm.SetFocus
End Sub

Private Sub PS_CategoryExpanded(Cancel As Boolean)
PS.Refresh
'    Dim i As Integer
'
'    For i = 1 To PS.Categories.Count
'        If PS.Categories.Item(i).Expanded = True Then
'            MsgBox PS.Categories(i).Caption
'        End If
'    Next i
End Sub

Private Sub PS_PropertyChanged(ByVal Prop As PropertySheet.TProperty, NewValue As Variant, Cancel As Boolean)
    On Error GoTo 1
    SetProperties Prop, NewValue, Cancel
    Project.Item(ProjectIndex).DataChanged = True
    Exit Sub
1
    Cancel = True
End Sub

