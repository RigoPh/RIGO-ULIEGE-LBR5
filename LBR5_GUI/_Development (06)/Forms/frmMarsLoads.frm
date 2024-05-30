VERSION 5.00
Object = "{0D452EE1-E08F-101A-852E-02608C4D0BB4}#2.0#0"; "FM20.DLL"
Begin VB.Form frmMarsLoads 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Mars Loads"
   ClientHeight    =   6435
   ClientLeft      =   2475
   ClientTop       =   1830
   ClientWidth     =   9885
   Icon            =   "frmMarsLoads.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   6435
   ScaleWidth      =   9885
   ShowInTaskbar   =   0   'False
   Begin VB.PictureBox Picture5 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      ForeColor       =   &H80000008&
      Height          =   1935
      Left            =   5520
      ScaleHeight     =   1935
      ScaleWidth      =   2655
      TabIndex        =   41
      Top             =   3360
      Width           =   2655
      Begin VB.TextBox txtNegativeWaveShear 
         Appearance      =   0  'Flat
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   285
         Left            =   1080
         TabIndex        =   45
         Top             =   1080
         Width           =   975
      End
      Begin VB.TextBox txtPositiveWaveShear 
         Appearance      =   0  'Flat
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   285
         Left            =   1080
         TabIndex        =   44
         Top             =   720
         Width           =   975
      End
      Begin VB.OptionButton opWaveShear 
         Caption         =   "Negative"
         Height          =   195
         Index           =   1
         Left            =   0
         TabIndex        =   43
         Top             =   1080
         Width           =   975
      End
      Begin VB.OptionButton opWaveShear 
         Caption         =   "Positive"
         Height          =   195
         Index           =   0
         Left            =   0
         TabIndex        =   42
         Top             =   720
         Width           =   975
      End
      Begin VB.Line Line6 
         X1              =   0
         X2              =   2640
         Y1              =   1440
         Y2              =   1440
      End
      Begin VB.Line Line5 
         X1              =   0
         X2              =   2640
         Y1              =   600
         Y2              =   600
      End
      Begin VB.Label Label20 
         AutoSize        =   -1  'True
         Caption         =   "Wave Shear Force:"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   -1  'True
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H8000000D&
         Height          =   195
         Left            =   360
         TabIndex        =   48
         Top             =   0
         Width           =   1395
      End
      Begin VB.Label Label8 
         AutoSize        =   -1  'True
         Caption         =   "[kN]"
         Height          =   195
         Left            =   2160
         TabIndex        =   47
         Top             =   720
         Width           =   300
      End
      Begin VB.Label Label7 
         AutoSize        =   -1  'True
         Caption         =   "[kN]"
         Height          =   195
         Left            =   2160
         TabIndex        =   46
         Top             =   1080
         Width           =   300
      End
   End
   Begin VB.PictureBox Picture10 
      BorderStyle     =   0  'None
      Height          =   735
      Left            =   120
      ScaleHeight     =   735
      ScaleWidth      =   1455
      TabIndex        =   67
      Top             =   4560
      Width           =   1455
      Begin VB.TextBox txtgw1 
         Appearance      =   0  'Flat
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   285
         Left            =   360
         TabIndex        =   68
         Top             =   360
         Width           =   735
      End
      Begin VB.Label Label31 
         AutoSize        =   -1  'True
         Caption         =   "gw1:"
         Height          =   195
         Left            =   0
         TabIndex        =   70
         Top             =   360
         Width           =   345
      End
      Begin VB.Label Label29 
         AutoSize        =   -1  'True
         Caption         =   "Safety Coefficient:"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   -1  'True
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H8000000D&
         Height          =   195
         Left            =   0
         TabIndex        =   69
         Top             =   0
         Width           =   1290
      End
   End
   Begin VB.PictureBox Picture9 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      ForeColor       =   &H80000008&
      Height          =   2295
      Left            =   8160
      ScaleHeight     =   2295
      ScaleWidth      =   1695
      TabIndex        =   60
      Top             =   3360
      Width           =   1695
      Begin VB.TextBox txtCFTorsion 
         Appearance      =   0  'Flat
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   285
         Left            =   360
         TabIndex        =   74
         Top             =   1920
         Width           =   735
      End
      Begin VB.TextBox txtCFVerticalBending 
         Appearance      =   0  'Flat
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   285
         Left            =   360
         TabIndex        =   63
         Top             =   840
         Width           =   735
      End
      Begin VB.TextBox txtCFHorizontalBending 
         Appearance      =   0  'Flat
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   285
         Left            =   360
         TabIndex        =   62
         Top             =   1560
         Width           =   735
      End
      Begin VB.Label Label30 
         AutoSize        =   -1  'True
         Caption         =   "x "
         Height          =   195
         Left            =   120
         TabIndex        =   75
         Top             =   1920
         Width           =   120
      End
      Begin VB.Line Line4 
         X1              =   0
         X2              =   1680
         Y1              =   1440
         Y2              =   1440
      End
      Begin VB.Line Line3 
         X1              =   0
         X2              =   1680
         Y1              =   600
         Y2              =   600
      End
      Begin VB.Label Label25 
         AutoSize        =   -1  'True
         Caption         =   "x "
         Height          =   195
         Left            =   120
         TabIndex        =   65
         Top             =   1560
         Width           =   120
      End
      Begin VB.Label Label9 
         AutoSize        =   -1  'True
         Caption         =   "x "
         Height          =   195
         Left            =   120
         TabIndex        =   64
         Top             =   840
         Width           =   120
      End
      Begin VB.Label Label13 
         Caption         =   "Combination Factors:"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   -1  'True
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H8000000D&
         Height          =   435
         Left            =   0
         TabIndex        =   61
         Top             =   0
         Width           =   1485
      End
   End
   Begin VB.PictureBox Picture8 
      BorderStyle     =   0  'None
      Height          =   1095
      Left            =   8160
      ScaleHeight     =   1095
      ScaleWidth      =   1695
      TabIndex        =   53
      Top             =   480
      Width           =   1695
      Begin VB.TextBox txtgw2 
         Appearance      =   0  'Flat
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   285
         Left            =   360
         TabIndex        =   56
         Top             =   720
         Width           =   735
      End
      Begin VB.TextBox txtgs2 
         Appearance      =   0  'Flat
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   285
         Left            =   360
         TabIndex        =   55
         Top             =   360
         Width           =   735
      End
      Begin VB.Label Label24 
         AutoSize        =   -1  'True
         Caption         =   "gw2:"
         Height          =   195
         Left            =   0
         TabIndex        =   58
         Top             =   720
         Width           =   345
      End
      Begin VB.Label Label11 
         AutoSize        =   -1  'True
         Caption         =   "gs2:"
         Height          =   195
         Left            =   0
         TabIndex        =   57
         Top             =   360
         Width           =   300
      End
      Begin VB.Label Label26 
         AutoSize        =   -1  'True
         Caption         =   "Safety Coefficients:"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   -1  'True
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H8000000D&
         Height          =   195
         Left            =   0
         TabIndex        =   54
         Top             =   0
         Width           =   1365
      End
   End
   Begin VB.PictureBox Picture7 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      ForeColor       =   &H80000008&
      Height          =   735
      Left            =   5520
      ScaleHeight     =   735
      ScaleWidth      =   2535
      TabIndex        =   49
      Top             =   2160
      Width           =   2535
      Begin VB.TextBox txtStillShear 
         Appearance      =   0  'Flat
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   285
         Left            =   1080
         TabIndex        =   50
         Top             =   360
         Width           =   975
      End
      Begin VB.Label Label22 
         AutoSize        =   -1  'True
         Caption         =   "[kN]"
         Height          =   195
         Left            =   2160
         TabIndex        =   52
         Top             =   360
         Width           =   300
      End
      Begin VB.Label Label21 
         AutoSize        =   -1  'True
         Caption         =   "Still Water Shear Force:"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   -1  'True
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H8000000D&
         Height          =   195
         Left            =   360
         TabIndex        =   51
         Top             =   0
         Width           =   1680
      End
   End
   Begin VB.ListBox lstLoadCases 
      Appearance      =   0  'Flat
      Height          =   2565
      Left            =   1320
      TabIndex        =   40
      Top             =   480
      Width           =   975
   End
   Begin VB.ListBox lstInternalLoads 
      Appearance      =   0  'Flat
      Height          =   1005
      Left            =   120
      MultiSelect     =   1  'Simple
      TabIndex        =   39
      Top             =   2040
      Width           =   975
   End
   Begin VB.ListBox lstSeaLoads 
      Appearance      =   0  'Flat
      Height          =   1005
      Left            =   120
      TabIndex        =   38
      Top             =   480
      Width           =   975
   End
   Begin VB.PictureBox Picture1 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      ForeColor       =   &H80000008&
      Height          =   975
      Left            =   2520
      ScaleHeight     =   975
      ScaleWidth      =   1695
      TabIndex        =   30
      Top             =   480
      Width           =   1695
      Begin VB.OptionButton OptLocalLoads 
         Caption         =   "Still Water + Wave"
         Height          =   195
         Index           =   1
         Left            =   0
         TabIndex        =   32
         Top             =   720
         Width           =   1695
      End
      Begin VB.OptionButton OptLocalLoads 
         Caption         =   "Still Water"
         Height          =   195
         Index           =   0
         Left            =   0
         TabIndex        =   31
         Top             =   360
         Width           =   1095
      End
      Begin VB.Label Label4 
         AutoSize        =   -1  'True
         Caption         =   "Lateral Pressures:"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   -1  'True
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H8000000D&
         Height          =   195
         Left            =   0
         TabIndex        =   33
         Top             =   0
         Width           =   1260
      End
   End
   Begin VB.PictureBox Picture4 
      BorderStyle     =   0  'None
      Height          =   735
      Left            =   8160
      ScaleHeight     =   735
      ScaleWidth      =   1695
      TabIndex        =   14
      Top             =   2160
      Width           =   1695
      Begin VB.TextBox txtgs1 
         Appearance      =   0  'Flat
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   285
         Left            =   360
         TabIndex        =   15
         Top             =   360
         Width           =   735
      End
      Begin VB.Label Label12 
         AutoSize        =   -1  'True
         Caption         =   "Safety Coefficient:"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   -1  'True
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H8000000D&
         Height          =   195
         Left            =   0
         TabIndex        =   17
         Top             =   0
         Width           =   1290
      End
      Begin VB.Label Label10 
         AutoSize        =   -1  'True
         Caption         =   "gs1:"
         Height          =   195
         Left            =   0
         TabIndex        =   16
         Top             =   360
         Width           =   300
      End
   End
   Begin VB.PictureBox Picture6 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      ForeColor       =   &H80000008&
      Height          =   615
      Left            =   120
      ScaleHeight     =   615
      ScaleWidth      =   1695
      TabIndex        =   10
      Top             =   3720
      Width           =   1695
      Begin VB.OptionButton OptWaveProbability 
         Caption         =   "10e-5"
         Height          =   195
         Index           =   0
         Left            =   0
         TabIndex        =   12
         Top             =   360
         Width           =   735
      End
      Begin VB.OptionButton OptWaveProbability 
         Caption         =   "10e-8"
         Height          =   195
         Index           =   1
         Left            =   960
         TabIndex        =   11
         Top             =   360
         Width           =   735
      End
      Begin VB.Label Label19 
         Caption         =   "Wave Probability:"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   -1  'True
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H8000000D&
         Height          =   255
         Left            =   0
         TabIndex        =   13
         Top             =   0
         Width           =   1365
      End
   End
   Begin VB.PictureBox Picture2 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      ForeColor       =   &H80000008&
      Height          =   1095
      Left            =   2520
      ScaleHeight     =   1095
      ScaleWidth      =   2655
      TabIndex        =   2
      Top             =   2160
      Width           =   2655
      Begin VB.OptionButton OptStillBending 
         Caption         =   "Hogging"
         Height          =   195
         Index           =   0
         Left            =   0
         TabIndex        =   6
         Top             =   360
         Width           =   975
      End
      Begin VB.OptionButton OptStillBending 
         Caption         =   "Sagging"
         Height          =   195
         Index           =   1
         Left            =   0
         TabIndex        =   5
         Top             =   720
         Width           =   975
      End
      Begin VB.TextBox txtStillBendingHogg 
         Appearance      =   0  'Flat
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   285
         Left            =   1080
         TabIndex        =   4
         Top             =   360
         Width           =   975
      End
      Begin VB.TextBox txtStillBendingSagg 
         Appearance      =   0  'Flat
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   285
         Left            =   1080
         TabIndex        =   3
         Top             =   720
         Width           =   975
      End
      Begin VB.Label Label18 
         AutoSize        =   -1  'True
         Caption         =   "[kNm]"
         Height          =   195
         Left            =   2160
         TabIndex        =   9
         Top             =   720
         Width           =   420
      End
      Begin VB.Label Label17 
         AutoSize        =   -1  'True
         Caption         =   "[kNm]"
         Height          =   195
         Left            =   2160
         TabIndex        =   8
         Top             =   360
         Width           =   420
      End
      Begin VB.Label Label5 
         AutoSize        =   -1  'True
         Caption         =   "Still Water Bending Moments:"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   -1  'True
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H8000000D&
         Height          =   195
         Left            =   0
         TabIndex        =   7
         Top             =   0
         Width           =   2085
      End
   End
   Begin VB.PictureBox Picture3 
      BorderStyle     =   0  'None
      Height          =   2295
      Left            =   2520
      ScaleHeight     =   2295
      ScaleWidth      =   4215
      TabIndex        =   18
      Top             =   3360
      Width           =   4215
      Begin VB.CheckBox ChkTorsion 
         Caption         =   "Torsion"
         Height          =   195
         Left            =   0
         TabIndex        =   72
         Top             =   1920
         Width           =   975
      End
      Begin VB.TextBox txtWaveTorsion 
         Appearance      =   0  'Flat
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   285
         Left            =   1080
         TabIndex        =   71
         Top             =   1920
         Width           =   975
      End
      Begin VB.TextBox txtWaveBendingHorizontal 
         Appearance      =   0  'Flat
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   285
         Left            =   1080
         TabIndex        =   19
         Top             =   1560
         Width           =   975
      End
      Begin VB.OptionButton OptWaveBending 
         Caption         =   "None"
         Height          =   195
         Index           =   0
         Left            =   0
         TabIndex        =   25
         Top             =   360
         Width           =   975
      End
      Begin VB.OptionButton OptWaveBending 
         Caption         =   "Sagging"
         Height          =   195
         Index           =   2
         Left            =   0
         TabIndex        =   24
         Top             =   1080
         Width           =   975
      End
      Begin VB.OptionButton OptWaveBending 
         Caption         =   "Hogging"
         Height          =   195
         Index           =   1
         Left            =   0
         TabIndex        =   23
         Top             =   720
         Width           =   975
      End
      Begin VB.TextBox txtWaveBendingHogg 
         Appearance      =   0  'Flat
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   285
         Left            =   1080
         TabIndex        =   22
         Top             =   720
         Width           =   975
      End
      Begin VB.TextBox txtWaveBendingSagg 
         Appearance      =   0  'Flat
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   285
         Left            =   1080
         TabIndex        =   21
         Top             =   1080
         Width           =   975
      End
      Begin VB.CheckBox ChkHorizontalBending 
         Caption         =   "Horizontal"
         Height          =   195
         Left            =   0
         TabIndex        =   20
         Top             =   1560
         Width           =   1095
      End
      Begin VB.Label Label23 
         AutoSize        =   -1  'True
         Caption         =   "[kNm]"
         Height          =   195
         Left            =   2160
         TabIndex        =   73
         Top             =   1920
         Width           =   420
      End
      Begin VB.Line Line2 
         X1              =   0
         X2              =   4200
         Y1              =   1440
         Y2              =   1440
      End
      Begin VB.Line Line1 
         X1              =   0
         X2              =   4200
         Y1              =   600
         Y2              =   600
      End
      Begin VB.Label Label16 
         AutoSize        =   -1  'True
         Caption         =   "[kNm]"
         Height          =   195
         Left            =   2160
         TabIndex        =   29
         Top             =   1560
         Width           =   420
      End
      Begin VB.Label Label15 
         AutoSize        =   -1  'True
         Caption         =   "[kNm]"
         Height          =   195
         Left            =   2160
         TabIndex        =   28
         Top             =   1080
         Width           =   420
      End
      Begin VB.Label Label14 
         AutoSize        =   -1  'True
         Caption         =   "[kNm]"
         Height          =   195
         Left            =   2160
         TabIndex        =   27
         Top             =   720
         Width           =   420
      End
      Begin VB.Label Label6 
         AutoSize        =   -1  'True
         Caption         =   "Wave Bending Moments:"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   -1  'True
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H8000000D&
         Height          =   195
         Left            =   0
         TabIndex        =   26
         Top             =   0
         Width           =   1800
      End
   End
   Begin VB.Line Line9 
      X1              =   120
      X2              =   9840
      Y1              =   5760
      Y2              =   5760
   End
   Begin VB.Label Label28 
      Caption         =   "Global Loads:"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   -1  'True
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H8000000D&
      Height          =   255
      Left            =   2520
      TabIndex        =   66
      Top             =   1800
      Width           =   1215
   End
   Begin VB.Line Line8 
      X1              =   2520
      X2              =   9840
      Y1              =   1680
      Y2              =   1680
   End
   Begin VB.Line Line7 
      X1              =   120
      X2              =   2400
      Y1              =   3240
      Y2              =   3240
   End
   Begin VB.Label Label27 
      Caption         =   "Local Loads:"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   -1  'True
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H8000000D&
      Height          =   255
      Left            =   2520
      TabIndex        =   59
      Top             =   120
      Width           =   1215
   End
   Begin VB.Label Label1 
      AutoSize        =   -1  'True
      Caption         =   "Sea Loads:"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   -1  'True
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H8000000D&
      Height          =   195
      Left            =   120
      TabIndex        =   37
      Top             =   120
      Width           =   810
   End
   Begin VB.Label Label2 
      AutoSize        =   -1  'True
      Caption         =   "Internal Loads:"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   -1  'True
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H8000000D&
      Height          =   195
      Left            =   120
      TabIndex        =   36
      Top             =   1680
      Width           =   1050
   End
   Begin VB.Label Label3 
      AutoSize        =   -1  'True
      Caption         =   "Load Cases:"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   -1  'True
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H8000000D&
      Height          =   195
      Left            =   1320
      TabIndex        =   35
      Top             =   120
      Width           =   885
   End
   Begin VB.Label lblDisclaimer 
      Appearance      =   0  'Flat
      Caption         =   "Note"
      ForeColor       =   &H00808000&
      Height          =   375
      Left            =   120
      TabIndex        =   34
      Top             =   5880
      Width           =   7215
   End
   Begin MSForms.CommandButton cmdCancel 
      Cancel          =   -1  'True
      Height          =   375
      Left            =   8760
      TabIndex        =   1
      Top             =   5880
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
      Left            =   7560
      TabIndex        =   0
      Top             =   5880
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
Attribute VB_Name = "frmMarsLoads"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Dim ProjectIndex As Integer
Dim oLBRLoadCase As cLoadCase
Dim colLC1 As colLoadCase
Dim colPanel As New Collection
Dim colPanelSide As New Collection
Dim oMarsLoads As cMarsLoads
Dim SeaLoadsIndex As Integer
Dim InternalLoadsIndex As Integer
Dim LoadCasesIndex As Integer

Public Function OpenForm(ByRef colLC As colLoadCase, ByRef col As Collection, _
                        ByRef colPSide As Collection, ByVal Index As Integer, ByVal PrIndex As Integer)
    ProjectIndex = PrIndex
    Me.Move (Screen.Width - Me.Width) / 2, (Screen.Height - Me.Height) / 2
    Me.Caption = "Load Case: " & colLC.Item(Index).Title & " - [" & GetFileName(Project.Item(ProjectIndex).sFileName) & "]"
    cmdOK.Picture = LoadResPicture("ID_BITMAP_OK", 0)
    cmdCancel.Picture = LoadResPicture("ID_BITMAP_CANCEL", 0)
    lblDisclaimer.Caption = "Note: The pressures are determined based on Bureau Veritas Rules and issued " & _
    "by Mars Software version 1.9a. Bureau Veritas is not responsible for the use made of these pressures."
    Dim i As Integer
    For i = 1 To col.Count
        colPanel.Add col.Item(i)
        colPanelSide.Add colPSide.Item(i)
    Next i
    GetData colLC, Index
End Function

Private Sub GetData(ByRef colLC As colLoadCase, ByVal Index As Integer)
    On Error GoTo GetDataErr
    Set colLC1 = colLC.Clone
    Set oLBRLoadCase = colLC1.Item(Index)
    Set oMarsLoads = Project.Item(ProjectIndex).cHeader.cMarsLoads
    InitializeControls
    Dim SWBMhogg As Double, WVBMhogg As Double
    Dim SWBMsagg As Double, WVBMsagg As Double
    Dim HWBM As Double
    Dim StillShear As Double
    Dim WaveShearPos As Double, WaveShearNeg As Double
    Dim gs1 As Double, gw1 As Double
    Dim gs2 As Double, gw2 As Double
    SWBMhogg = Project.Item(ProjectIndex).cHeader.cMarsLoads.StillWaterBendingMomentHogg
    SWBMsagg = Project.Item(ProjectIndex).cHeader.cMarsLoads.StillWaterBendingMomentSagg
    WVBMhogg = Project.Item(ProjectIndex).cHeader.cMarsLoads.VerticalWaveBendingMomentHogg * 0.625
    WVBMsagg = Project.Item(ProjectIndex).cHeader.cMarsLoads.VerticalWaveBendingMomentSagg * 0.625
    HWBM = Project.Item(ProjectIndex).cHeader.cMarsLoads.HorizontalWaveBendingMoment * 0.625
    StillShear = Project.Item(ProjectIndex).cHeader.cMarsLoads.VerticalStillShear
    WaveShearNeg = Project.Item(ProjectIndex).cHeader.cMarsLoads.VerticalWaveShearNegative * 0.625
    WaveShearPos = Project.Item(ProjectIndex).cHeader.cMarsLoads.VerticalWaveShearPositive * 0.625
    
'    If Index = 0 Then
'        txtWaveBendingHogg = Project.Item(ProjectIndex).cHeader.cMarsLoads.VerticalWaveBendingMomentHogg * 0.625
'        txtWaveBendingSagg = Project.Item(ProjectIndex).cHeader.cMarsLoads.VerticalWaveBendingMomentSagg * 0.625
'        txtWaveBendingHorizontal = Project.Item(ProjectIndex).cHeader.cMarsLoads.HorizontalWaveBendingMoment * 0.625
'        txtNegativeWaveShear = Project.Item(ProjectIndex).cHeader.cMarsLoads.VerticalWaveShearNegative * 0.625
'        txtPositiveWaveShear = Project.Item(ProjectIndex).cHeader.cMarsLoads.VerticalWaveShearPositive * 0.625
'    ElseIf Index = 1 Then
'        txtWaveBendingHogg = Project.Item(ProjectIndex).cHeader.cMarsLoads.VerticalWaveBendingMomentHogg
'        txtWaveBendingSagg = Project.Item(ProjectIndex).cHeader.cMarsLoads.VerticalWaveBendingMomentSagg
'        txtWaveBendingHorizontal = Project.Item(ProjectIndex).cHeader.cMarsLoads.HorizontalWaveBendingMoment
'        txtNegativeWaveShear = Project.Item(ProjectIndex).cHeader.cMarsLoads.VerticalWaveShearNegative
'        txtPositiveWaveShear = Project.Item(ProjectIndex).cHeader.cMarsLoads.VerticalWaveShearPositive
'    End If

    
    gs1 = Project.Item(ProjectIndex).cHeader.cMarsLoads.GammaStillGlobal
    gw1 = Project.Item(ProjectIndex).cHeader.cMarsLoads.GammaWaveGlobal
    gs2 = Project.Item(ProjectIndex).cHeader.cMarsLoads.GammaStillLocal
    gw2 = Project.Item(ProjectIndex).cHeader.cMarsLoads.GammaWaveLocal
    SWBMhogg = SWBMhogg '/ gs1
    SWBMsagg = SWBMsagg '/ gs1
    WVBMhogg = WVBMhogg '/ gw1 / 0.625
    WVBMsagg = WVBMsagg '/ gw1 / 0.625
    HWBM = HWBM '/ gw1 / 0.625
    
    txtStillBendingHogg.Text = Round(SWBMhogg, 0)
    txtStillBendingSagg.Text = Round(SWBMsagg, 0)
    txtWaveBendingHogg.Text = Round(WVBMhogg, 0)
    txtWaveBendingSagg.Text = Round(WVBMsagg, 0)
    txtWaveBendingHorizontal.Text = Round(HWBM, 0)
    txtStillShear.Text = Round(StillShear, 0)
    txtNegativeWaveShear.Text = Round(WaveShearNeg, 0)
    txtPositiveWaveShear.Text = Round(WaveShearPos, 0)
    
    txtgs1.Text = Format(gs1, "0.00")
    txtgw1.Text = Format(gw1, "0.00")
    txtgs2.Text = Format(gs2, "0.00")
    txtgw2.Text = Format(gw2, "0.00")
    
    txtCFVerticalBending.Text = Format(Project.Item(ProjectIndex).cHeader.cMarsLoads.colMarsLoadCase.Item(LoadCasesIndex).CF_VerticalWaveBendingMoment_Shear, "0.0")
    txtCFHorizontalBending.Text = Format(Project.Item(ProjectIndex).cHeader.cMarsLoads.colMarsLoadCase.Item(LoadCasesIndex).CF_HorizontalWaveBendingMoment, "0.0")
    Exit Sub
GetDataErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmMarsLoads: Sub GetData")
End Sub

Private Sub InitializeControls()
    
    PopulateListLoadTypes
    PopulateListLoadCases
    OptLocalLoads.Item(0) = True
    OptStillBending.Item(0) = True
    OptWaveBending.Item(1) = True
    OptWaveProbability.Item(0) = True
    opWaveShear.Item(0) = True
    
    'If Project.Item(ProjectIndex).cHeader.IANA = 1 Then
        If Project.Item(ProjectIndex).cHeader.MarsSymm = True Then
            ChkHorizontalBending.Enabled = False
            ChkTorsion.Enabled = False
            txtWaveBendingHorizontal.Enabled = False
            txtWaveTorsion.Enabled = False
            txtCFHorizontalBending.Enabled = False
            txtCFTorsion.Enabled = False
        Else
            'ChkHorizontalBending.Enabled = True
            'ChkTorsion.Enabled = True
        End If
'    ElseIf Project.Item(ProjectIndex).cHeader.IANA = 2 Then
'        ChkHorizontalBending.Enabled = True
'    End If

    If Project.Item(ProjectIndex).cHeader.IANA = 1 Then
        ChkTorsion.Enabled = False
    End If
    
    
    If Project.Item(ProjectIndex).cHeader.IANA = 1 Then
        txtStillShear.Enabled = False
        txtNegativeWaveShear.Enabled = False
        txtPositiveWaveShear.Enabled = False
        txtWaveBendingHorizontal.Enabled = False
        txtWaveTorsion.Enabled = False
        opWaveShear(0).Enabled = False
        opWaveShear(1).Enabled = False
    End If
    
End Sub

Private Sub PopulateListLoadCases()
    Dim oMarsLoadCase As cMarsLoadCase
    For Each oMarsLoadCase In Project.Item(ProjectIndex).cHeader.cMarsLoads.colMarsLoadCase
        If oMarsLoadCase.Index > 0 Then
            lstLoadCases.AddItem oMarsLoadCase.Label
        End If
    Next oMarsLoadCase
    If lstLoadCases.ListCount > 0 Then
        lstLoadCases.Selected(0) = True
    End If
End Sub

Private Sub PopulateListLoadTypes()
    Dim oLoadType As cMarsLoadType
    For Each oLoadType In Project.Item(ProjectIndex).cHeader.cMarsLoads.colMarsLoadType
        Select Case Left(UCase(oLoadType.Label), 3)
            Case "SEA"
                lstSeaLoads.AddItem oLoadType.Label
            Case Else
                lstInternalLoads.AddItem oLoadType.Label
        End Select
    Next oLoadType
    If lstSeaLoads.ListCount > 0 Then
        lstSeaLoads.Selected(0) = True
    End If
    If lstInternalLoads.ListCount > 0 Then
        lstInternalLoads.Selected(0) = True
    End If
End Sub

Private Sub SetData()
    On Error GoTo SetDataErr
    ComputeLocalLoads
    ComputeGlobalLoads
    oLBRLoadCase.Title = SetLoadCaseName
    Exit Sub
SetDataErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmMarsLoads: Sub SetData")
End Sub

Private Sub ComputeGlobalLoads()
    On Error GoTo ComputeGlobalLoadsErr
    Dim SWBM As Double, VWBM As Double, HWBM As Double
    Dim gs1 As Double, gw1 As Double
    Dim CFVWBM As Double, CFHWBM As Double
    Dim WaveProb As Double
    Dim StillShear As Double
    Dim WaveShear As Double
    
    If OptStillBending(0) Then
        SWBM = Val(txtStillBendingHogg.Text) * 1000
    Else
        SWBM = Val(txtStillBendingSagg.Text) * 1000
    End If
    
    If OptWaveBending(0) Then
        VWBM = 0
    ElseIf OptWaveBending(1) Then
        VWBM = Val(txtWaveBendingHogg.Text) * 1000
    ElseIf OptWaveBending(2) Then
        VWBM = Val(txtWaveBendingSagg.Text) * 1000
    End If
    
    If ChkHorizontalBending Then
        HWBM = Val(txtWaveBendingHorizontal.Text) * 1000
    End If
    
    'Still Shear Force
    StillShear = Val(txtStillShear.Text) * 1000
    
    'Wave Shear Force
    If opWaveShear(0) Then
        WaveShear = Val(txtPositiveWaveShear.Text) * 1000
    ElseIf opWaveShear(1) Then
        WaveShear = Val(txtNegativeWaveShear.Text) * 1000
    End If
    
    'Wave Probability
'    If OptWaveProbability(0) Then
'        VWBM = VWBM * 0.625
'        HWBM = HWBM * 0.625
'        WaveShear = WaveShear * 0.625
'    End If
    
    'Safety Factors
    gs1 = Val(txtgs1.Text)
    gw1 = Val(txtgw1.Text)
    SWBM = SWBM * gs1
    VWBM = VWBM * gw1
    HWBM = HWBM * gw1
    
    StillShear = StillShear * gs1
    WaveShear = WaveShear * gw1
    
    'Combination Factors
    VWBM = VWBM * Val(txtCFVerticalBending.Text)
    HWBM = HWBM * Val(txtCFHorizontalBending.Text)
    WaveShear = WaveShear * Val(txtCFVerticalBending.Text)
    
    If Project.Item(ProjectIndex).cHeader.MarsSymm = True Then
        HWBM = 0
    End If
    
    If Project.Item(ProjectIndex).cHeader.IANA = 1 Then
        StillShear = 0
        WaveShear = 0
    End If
    
    'Totals
    oLBRLoadCase.VerticalBendingMomentFore = Round(SWBM + VWBM, 0)
    oLBRLoadCase.VerticalBendingMomentAft = Round(SWBM + VWBM, 0)
    oLBRLoadCase.HorizontalBendingMomentFore = Round(HWBM, 0)
    oLBRLoadCase.HorizontalBendingMomentAft = Round(HWBM, 0)
    oLBRLoadCase.VerticalShear = Round(StillShear + WaveShear, 0)
    
    Exit Sub
ComputeGlobalLoadsErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmMarsLoads: Sub ComputeGlobalLoads")
End Sub

Private Sub ComputeLocalLoads()
    On Error GoTo ComputeLocalLoadsErr
    Dim i As Integer
    Dim ps_in As Double, pw_in As Double, ps_out As Double, pw_out As Double, gs2 As Double, gw2 As Double
    Dim p_in As Double, p_out As Double ' p = ps + pw
    Dim oPanel As cPanel
    Dim colLoadCase As colLoadCase
    
    For Each oPanel In Project.Item(ProjectIndex).colPanel
        Set colLoadCase = colPanel.Item(oPanel.Index)
        'Sea Loads
        ps_in = oPanel.colMarsLoadCase.Item(0).colMarsLoadType.Item(SeaLoadsIndex).RightCompPress.In_Node - _
                oPanel.colMarsLoadCase.Item(0).colMarsLoadType.Item(SeaLoadsIndex).LeftCompPress.In_Node
        pw_in = oPanel.colMarsLoadCase.Item(LoadCasesIndex).colMarsLoadType.Item(SeaLoadsIndex).RightCompPress.In_Node - _
                oPanel.colMarsLoadCase.Item(LoadCasesIndex).colMarsLoadType.Item(SeaLoadsIndex).LeftCompPress.In_Node
        ps_out = oPanel.colMarsLoadCase.Item(0).colMarsLoadType.Item(SeaLoadsIndex).RightCompPress.Out_Node - _
                 oPanel.colMarsLoadCase.Item(0).colMarsLoadType.Item(SeaLoadsIndex).LeftCompPress.Out_Node
        pw_out = oPanel.colMarsLoadCase.Item(LoadCasesIndex).colMarsLoadType.Item(SeaLoadsIndex).RightCompPress.Out_Node - _
                 oPanel.colMarsLoadCase.Item(LoadCasesIndex).colMarsLoadType.Item(SeaLoadsIndex).LeftCompPress.Out_Node
        'Internal Loads
        For i = 0 To lstInternalLoads.ListCount - 1
            If lstInternalLoads.Selected(i) = True Then
                ps_in = ps_in + _
                        oPanel.colMarsLoadCase.Item(0).colMarsLoadType.Item(lstSeaLoads.ListCount + i + 1).RightCompPress.In_Node - _
                        oPanel.colMarsLoadCase.Item(0).colMarsLoadType.Item(lstSeaLoads.ListCount + i + 1).LeftCompPress.In_Node
                pw_in = pw_in + _
                        oPanel.colMarsLoadCase.Item(LoadCasesIndex).colMarsLoadType.Item(lstSeaLoads.ListCount + i + 1).RightCompPress.In_Node - _
                        oPanel.colMarsLoadCase.Item(LoadCasesIndex).colMarsLoadType.Item(lstSeaLoads.ListCount + i + 1).LeftCompPress.In_Node
                ps_out = ps_out + _
                         oPanel.colMarsLoadCase.Item(0).colMarsLoadType.Item(lstSeaLoads.ListCount + i + 1).RightCompPress.Out_Node - _
                         oPanel.colMarsLoadCase.Item(0).colMarsLoadType.Item(lstSeaLoads.ListCount + i + 1).LeftCompPress.Out_Node
                pw_out = pw_out + _
                         oPanel.colMarsLoadCase.Item(LoadCasesIndex).colMarsLoadType.Item(lstSeaLoads.ListCount + i + 1).RightCompPress.Out_Node - _
                         oPanel.colMarsLoadCase.Item(LoadCasesIndex).colMarsLoadType.Item(lstSeaLoads.ListCount + i + 1).LeftCompPress.Out_Node
            End If
        Next i
        
        Select Case OptLocalLoads(0)
            Case Is = True 'ps
                pw_in = 0
                pw_out = 0
            Case Is = False 'ps + pw
        End Select
        gs2 = Val(txtgs2.Text)
        gw2 = Val(txtgw2.Text)
        
        p_in = ps_in * gs2 + pw_in * gw2
        p_out = ps_out * gs2 + pw_out * gw2
        If oPanel.LateralPressureSide = SideLeft Then
            p_in = -p_in
            p_out = -p_out
        End If

        colLoadCase.Item(oLBRLoadCase.Index).LateralPressureIn = p_in / 10  ' kN/m2 --> mH20
        colLoadCase.Item(oLBRLoadCase.Index).LateralPressureOut = p_out / 10  ' kN/m2 --> mH20
    Next oPanel
    Exit Sub
ComputeLocalLoadsErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmMarsLoads: Sub ComputeLocalLoads")
End Sub

Private Function SetLoadCaseName() As String
    On Error GoTo SetLoadCaseNameErr
    Dim s As String
    s = ""
    'Sea Loads
    s = lstSeaLoads.Text
    'Internal Loads
    For i = 0 To lstInternalLoads.ListCount - 1
        If lstInternalLoads.Selected(i) = True Then
            s = s & " " & lstInternalLoads.List(i)
        End If
    Next i
    'Load Cases
    s = s & " " & lstLoadCases.Text
    If OptLocalLoads(0) Then
        s = s & " " & "ps"
    Else
        s = s & " " & "ps+pw"
    End If
    'Still Water Bending Moment
    If OptStillBending(0) Then
        s = s & " " & "SWBM:Hogg"
    Else
        s = s & " " & "SWBM:Sagg"
    End If
    'Vertical Wave Bending Moment
    If OptWaveBending(0) Then
    ElseIf OptWaveBending(1) Then
        s = s & " " & "VWBM:Hogg"
    ElseIf OptWaveBending(2) Then
        s = s & " " & "VWBM:Sagg"
    End If
    'Wave Probability
    If OptWaveBending(0) = False Then
        If OptWaveProbability(0) Then
            s = s & "(10e-5)"
        Else
            s = s & "(10e-8)"
        End If
    End If
    If ChkHorizontalBending Then
        s = s & " " & "HWBM"
    End If
    SetLoadCaseName = s
    Exit Function
SetLoadCaseNameErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmMarsLoads: Function SetLoadCaseName")
End Function

Private Sub cmdCancel_Click()
    Project.Item(ProjectIndex).frmLoadCases.OpenForm colLC1, colPanelSide, colPanel
    Unload Me
    Project.Item(ProjectIndex).frmLoadCases.Show vbModeless, fMainForm
End Sub

Private Sub cmdOK_Click()
    SetData
    Project.Item(ProjectIndex).frmLoadCases.OpenForm colLC1, colPanelSide, colPanel
    Unload Me
    Project.Item(ProjectIndex).frmLoadCases.Show vbModeless, fMainForm
End Sub

Private Sub Form_Resize()
    Me.Height = cmdOK.Top + cmdOK.Height + 600
End Sub

Private Sub Form_Terminate()
    Set frmMarsLoads = Nothing
End Sub

Private Sub lstInternalLoads_Click()
    InternalLoadsIndex = lstInternalLoads.ListIndex + 1
End Sub

Private Sub lstLoadCases_Click()
    LoadCasesIndex = lstLoadCases.ListIndex + 1
    txtCFVerticalBending.Text = Format(Project.Item(ProjectIndex).cHeader.cMarsLoads.colMarsLoadCase.Item(LoadCasesIndex).CF_VerticalWaveBendingMoment_Shear, "0.0")
    txtCFHorizontalBending.Text = Format(Project.Item(ProjectIndex).cHeader.cMarsLoads.colMarsLoadCase.Item(LoadCasesIndex).CF_HorizontalWaveBendingMoment, "0.0")
    'txtCFTorsion.Text =
End Sub

Private Sub txtLoadCaseName_GotFocus()
    txtLoadCaseName.SelStart = 0
    txtLoadCaseName.SelLength = Len(txtLoadCaseName.Text)
End Sub

Private Sub lstSeaLoads_Click()
    SeaLoadsIndex = lstSeaLoads.ListIndex + 1
End Sub

Private Sub OptWaveProbability_Click(Index As Integer)
    If Index = 0 Then
        txtWaveBendingHogg = Project.Item(ProjectIndex).cHeader.cMarsLoads.VerticalWaveBendingMomentHogg * 0.625
        txtWaveBendingSagg = Project.Item(ProjectIndex).cHeader.cMarsLoads.VerticalWaveBendingMomentSagg * 0.625
        txtWaveBendingHorizontal = Project.Item(ProjectIndex).cHeader.cMarsLoads.HorizontalWaveBendingMoment * 0.625
        txtNegativeWaveShear = Project.Item(ProjectIndex).cHeader.cMarsLoads.VerticalWaveShearNegative * 0.625
        txtPositiveWaveShear = Project.Item(ProjectIndex).cHeader.cMarsLoads.VerticalWaveShearPositive * 0.625
    ElseIf Index = 1 Then
        txtWaveBendingHogg = Project.Item(ProjectIndex).cHeader.cMarsLoads.VerticalWaveBendingMomentHogg
        txtWaveBendingSagg = Project.Item(ProjectIndex).cHeader.cMarsLoads.VerticalWaveBendingMomentSagg
        txtWaveBendingHorizontal = Project.Item(ProjectIndex).cHeader.cMarsLoads.HorizontalWaveBendingMoment
        txtNegativeWaveShear = Project.Item(ProjectIndex).cHeader.cMarsLoads.VerticalWaveShearNegative
        txtPositiveWaveShear = Project.Item(ProjectIndex).cHeader.cMarsLoads.VerticalWaveShearPositive
    End If
End Sub

Private Sub txtCFHorizontalBending_GotFocus()
    txtCFHorizontalBending.SelStart = 0
    txtCFHorizontalBending.SelLength = Len(txtCFHorizontalBending.Text)
End Sub

Private Sub txtCFHorizontalBending_Validate(Cancel As Boolean)
    ValidateNumeric txtCFHorizontalBending, Cancel
End Sub

Private Sub txtCFVerticalBending_GotFocus()
    txtCFVerticalBending.SelStart = 0
    txtCFVerticalBending.SelLength = Len(txtCFVerticalBending.Text)
End Sub

Private Sub txtCFVerticalBending_Validate(Cancel As Boolean)
    ValidateNumeric txtCFVerticalBending, Cancel
End Sub

Private Sub txtgs1_GotFocus()
    txtgs1.SelStart = 0
    txtgs1.SelLength = Len(txtgs1.Text)
End Sub

Private Sub txtgs1_Validate(Cancel As Boolean)
    ValidateNumeric txtgs1, Cancel
End Sub

Private Sub txtgs2_GotFocus()
    txtgs2.SelStart = 0
    txtgs2.SelLength = Len(txtgs2.Text)
End Sub

Private Sub txtgs2_Validate(Cancel As Boolean)
    ValidateNumeric txtgs2, Cancel
End Sub

Private Sub txtgw1_GotFocus()
    txtgw1.SelStart = 0
    txtgw1.SelLength = Len(txtgw1.Text)
End Sub

Private Sub txtgw1_Validate(Cancel As Boolean)
    ValidateNumeric txtgw1, Cancel
End Sub

Private Sub txtgw2_GotFocus()
    txtgw2.SelStart = 0
    txtgw2.SelLength = Len(txtgw2.Text)
End Sub

Private Sub txtgw2_Validate(Cancel As Boolean)
    ValidateNumeric txtgw2, Cancel
End Sub

Private Sub txtStillBendingHogg_GotFocus()
    txtStillBendingHogg.SelStart = 0
    txtStillBendingHogg.SelLength = Len(txtStillBendingHogg.Text)
End Sub

Private Sub txtStillBendingHogg_Validate(Cancel As Boolean)
    ValidateNumeric txtStillBendingHogg, Cancel
End Sub

Private Sub txtStillBendingSagg_GotFocus()
    txtStillBendingSagg.SelStart = 0
    txtStillBendingSagg.SelLength = Len(txtStillBendingSagg.Text)
End Sub

Private Sub txtStillBendingSagg_Validate(Cancel As Boolean)
    ValidateNumeric txtStillBendingSagg, Cancel
End Sub

Private Sub txtWaveBendingHogg_GotFocus()
    txtWaveBendingHogg.SelStart = 0
    txtWaveBendingHogg.SelLength = Len(txtWaveBendingHogg.Text)
End Sub

Private Sub txtWaveBendingHogg_Validate(Cancel As Boolean)
    ValidateNumeric txtWaveBendingHogg, Cancel
End Sub

Private Sub txtWaveBendingHorizontal_GotFocus()
    txtWaveBendingHorizontal.SelStart = 0
    txtWaveBendingHorizontal.SelLength = Len(txtWaveBendingHorizontal.Text)
End Sub

Private Sub txtWaveBendingHorizontal_Validate(Cancel As Boolean)
    ValidateNumeric txtWaveBendingHorizontal, Cancel
End Sub

Private Sub txtWaveBendingSagg_GotFocus()
    txtWaveBendingSagg.SelStart = 0
    txtWaveBendingSagg.SelLength = Len(txtWaveBendingSagg.Text)
End Sub

Private Sub txtWaveBendingSagg_Validate(Cancel As Boolean)
ValidateNumeric txtWaveBendingSagg, Cancel
End Sub
