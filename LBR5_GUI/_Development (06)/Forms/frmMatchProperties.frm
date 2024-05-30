VERSION 5.00
Begin VB.Form frmMatchProperties 
   AutoRedraw      =   -1  'True
   Caption         =   "Match Properties"
   ClientHeight    =   8505
   ClientLeft      =   2400
   ClientTop       =   1965
   ClientWidth     =   9195
   LinkTopic       =   "Form1"
   ScaleHeight     =   8505
   ScaleWidth      =   9195
   Begin VB.Frame frScantlings 
      Caption         =   "Scantlings"
      Height          =   6855
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   3135
      Begin VB.CheckBox ChkScantlingsCheckAll 
         Caption         =   "Check All Scantlings"
         Height          =   195
         Left            =   120
         TabIndex        =   23
         Top             =   6480
         Width           =   1935
      End
      Begin VB.CheckBox Check10 
         Caption         =   "Secondary Frames (All Scantlings)"
         Height          =   195
         Left            =   120
         TabIndex        =   22
         Top             =   5640
         Width           =   2775
      End
      Begin VB.CheckBox Check9 
         Caption         =   "Primary Frames (All Scantlings)"
         Height          =   195
         Left            =   120
         TabIndex        =   21
         Top             =   4320
         Width           =   2535
      End
      Begin VB.CheckBox ChkSecFrAll 
         Caption         =   "Secondary Frames (All Scantlings)"
         Height          =   195
         Left            =   120
         TabIndex        =   20
         Top             =   3000
         Width           =   2775
      End
      Begin VB.CheckBox ChkPrFrAll 
         Caption         =   "Primary Frames (All Scantlings)"
         Height          =   195
         Left            =   120
         TabIndex        =   19
         Top             =   1680
         Width           =   2535
      End
      Begin VB.CheckBox chkGirders 
         Caption         =   "Girders"
         Height          =   195
         Left            =   120
         TabIndex        =   18
         Top             =   6000
         Width           =   975
      End
      Begin VB.CheckBox Check8 
         Caption         =   "Secondary Frames Spacing"
         Height          =   195
         Left            =   120
         TabIndex        =   17
         Top             =   5400
         Width           =   2535
      End
      Begin VB.CheckBox Check7 
         Caption         =   "Secondary Frames Flange Width"
         Height          =   195
         Left            =   120
         TabIndex        =   16
         Top             =   5160
         Width           =   2655
      End
      Begin VB.CheckBox Check6 
         Caption         =   "Secondary Frames Web Thickness"
         Height          =   195
         Left            =   120
         TabIndex        =   15
         Top             =   4920
         Width           =   2895
      End
      Begin VB.CheckBox Check5 
         Caption         =   "Secondary Frames Web Height"
         Height          =   195
         Left            =   120
         TabIndex        =   14
         Top             =   4680
         Width           =   2535
      End
      Begin VB.CheckBox Check4 
         Caption         =   "Primary Frames Spacing"
         Height          =   195
         Left            =   120
         TabIndex        =   13
         Top             =   4080
         Width           =   2535
      End
      Begin VB.CheckBox Check3 
         Caption         =   "Primary Frames Flange Width"
         Height          =   195
         Left            =   120
         TabIndex        =   12
         Top             =   3840
         Width           =   2535
      End
      Begin VB.CheckBox Check2 
         Caption         =   "Primary Frames Web Thickness"
         Height          =   195
         Left            =   120
         TabIndex        =   11
         Top             =   3600
         Width           =   2655
      End
      Begin VB.CheckBox Check1 
         Caption         =   "Primary Frames Web Height"
         Height          =   195
         Left            =   120
         TabIndex        =   10
         Top             =   3360
         Width           =   2415
      End
      Begin VB.CheckBox ChkSecFrSpacing 
         Caption         =   "Secondary Frames Spacing"
         Height          =   195
         Left            =   120
         TabIndex        =   9
         Top             =   2760
         Width           =   2535
      End
      Begin VB.CheckBox ChkSecFrFlangeWidth 
         Caption         =   "Secondary Frames Flange Width"
         Height          =   195
         Left            =   120
         TabIndex        =   8
         Top             =   2520
         Width           =   2655
      End
      Begin VB.CheckBox ChkSecFrWebThickness 
         Caption         =   "Secondary Frames Web Thickness"
         Height          =   195
         Left            =   120
         TabIndex        =   7
         Top             =   2280
         Width           =   2895
      End
      Begin VB.CheckBox ChkSecFrWebHeight 
         Caption         =   "Secondary Frames Web Height"
         Height          =   195
         Left            =   120
         TabIndex        =   6
         Top             =   2040
         Width           =   2535
      End
      Begin VB.CheckBox ChkPrFrSpacing 
         Caption         =   "Primary Frames Spacing"
         Height          =   195
         Left            =   120
         TabIndex        =   5
         Top             =   1440
         Width           =   2535
      End
      Begin VB.CheckBox ChkPrFrFlangeWidth 
         Caption         =   "Primary Frames Flange Width"
         Height          =   195
         Left            =   120
         TabIndex        =   4
         Top             =   1200
         Width           =   2535
      End
      Begin VB.CheckBox ChkPrFrWebThickness 
         Caption         =   "Primary Frames Web Thickness"
         Height          =   195
         Left            =   120
         TabIndex        =   3
         Top             =   960
         Width           =   2655
      End
      Begin VB.CheckBox ChkPrFrWebHeight 
         Caption         =   "Primary Frames Web Height"
         Height          =   195
         Left            =   120
         TabIndex        =   2
         Top             =   720
         Width           =   2415
      End
      Begin VB.CheckBox ChkPlateThickness 
         Caption         =   "Plate Thickness"
         Height          =   195
         Left            =   120
         TabIndex        =   1
         Top             =   360
         Width           =   1575
      End
   End
End
Attribute VB_Name = "frmMatchProperties"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
