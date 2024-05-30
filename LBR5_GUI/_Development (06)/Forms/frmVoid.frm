VERSION 5.00
Begin VB.Form frmVoid 
   Caption         =   "3D View"
   ClientHeight    =   4035
   ClientLeft      =   945
   ClientTop       =   1950
   ClientWidth     =   6315
   LinkTopic       =   "Form1"
   MDIChild        =   -1  'True
   ScaleHeight     =   269
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   421
   Begin VB.PictureBox Picture1 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1575
      Left            =   0
      ScaleHeight     =   101
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   117
      TabIndex        =   0
      Top             =   0
      Visible         =   0   'False
      Width           =   1815
   End
End
Attribute VB_Name = "frmVoid"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Dim x_old As Double
Dim y_old As Double
Dim xrot_old As Double
Dim yrot_old As Double
' Note the ScaleMode of this form is set to pixels

Private Sub Form_Activate()
    ReSizeGLScene ScaleWidth, ScaleHeight
End Sub

Private Sub Form_KeyDown(KeyCode As Integer, Shift As Integer)
    'Keys(KeyCode) = True
    Select Case KeyCode
        Case vbKeyLeft
            xrot = xrot + 1
        Case vbKeyRight
            xrot = xrot - 1
        Case vbKeyUp
            yrot = yrot - 1
        Case vbKeyDown
            yrot = yrot + 1
        Case vbKeyAdd
            ZOOM = ZOOM - 1
        Case vbKeySubtract
            ZOOM = ZOOM + 1
    End Select
    DrawGLScene
End Sub

Private Sub Form_KeyUp(KeyCode As Integer, Shift As Integer)
    'Keys(KeyCode) = False
End Sub

Private Sub Form_MouseMove(Button As Integer, Shift As Integer, x As Single, y As Single)
    If Button = 1 And Shift = 0 Then
        xrot = xrot_old - (x - x_old)
        yrot = yrot_old + (y - y_old)
    End If
    If Button = 1 And Shift = 1 Then
        x_trans = x_trans - (x - x_old)
        y_trans = y_trans + (y - y_old)
    End If
    If Button = 2 Then
        If ZOOM + (y - y_old) < m_NearPlane Then
            ZOOM = m_NearPlane
        ElseIf ZOOM + (y - y_old) > m_FarPlane Then '- Project.Item(ProjectIndex).cHeader.Width Then
            ZOOM = m_FarPlane '- Project.Item(ProjectIndex).cHeader.Width
        Else
            ZOOM = Abs(ZOOM + (y - y_old))
        End If
    End If
    x_old = x
    y_old = y
    xrot_old = xrot
    yrot_old = yrot
    DrawGLScene
End Sub

Private Sub Form_Paint()
    DrawGLScene
End Sub

Private Sub Form_Resize()
    ReSizeGLScene ScaleWidth, ScaleHeight
End Sub

Private Sub Form_Unload(Cancel As Integer)
    Keys(vbKeyEscape) = True
    KillGLWindow
End Sub



