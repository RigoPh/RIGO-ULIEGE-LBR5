VERSION 5.00
Begin VB.Form Form1 
   Caption         =   "Form1"
   ClientHeight    =   1740
   ClientLeft      =   480
   ClientTop       =   1845
   ClientWidth     =   2370
   Icon            =   "Form1.frx":0000
   LinkTopic       =   "Form1"
   MDIChild        =   -1  'True
   ScaleHeight     =   116
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   158
   Begin VB.PictureBox Picture1 
      Height          =   375
      Left            =   240
      ScaleHeight     =   315
      ScaleWidth      =   435
      TabIndex        =   0
      Top             =   120
      Visible         =   0   'False
      Width           =   495
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

' Note the ScaleMode of this form is set to pixels

Private Sub Form_KeyDown(KeyCode As Integer, Shift As Integer)
    Keys(KeyCode) = True
End Sub

Private Sub Form_KeyUp(KeyCode As Integer, Shift As Integer)
    Keys(KeyCode) = False
End Sub


Private Sub Form_Resize()
    ReSizeGLScene ScaleWidth, ScaleHeight
End Sub

Private Sub Form_Unload(Cancel As Integer)
    Keys(vbKeyEscape) = True
    KillGLWindow
End Sub



