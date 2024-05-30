VERSION 5.00
Begin VB.Form frmOpenGL 
   Caption         =   "3D View"
   ClientHeight    =   4800
   ClientLeft      =   3855
   ClientTop       =   3795
   ClientWidth     =   6525
   ForeColor       =   &H000000FF&
   Icon            =   "frmOpenGL.frx":0000
   LinkTopic       =   "Form1"
   MDIChild        =   -1  'True
   ScaleHeight     =   320
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   435
   Begin VB.Timer Time 
      Left            =   240
      Top             =   480
   End
   Begin VB.PictureBox Picture1 
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      Height          =   1575
      Left            =   240
      ScaleHeight     =   101
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   117
      TabIndex        =   0
      Top             =   1080
      Visible         =   0   'False
      Width           =   1815
   End
   Begin VB.Menu mnuWindow 
      Caption         =   "&Window"
      WindowList      =   -1  'True
      Begin VB.Menu mnuWindowCascade 
         Caption         =   "&Cascade"
      End
      Begin VB.Menu mnuWindowTileHorizontal 
         Caption         =   "Tile &Horizontal"
      End
      Begin VB.Menu mnuWindowTileVertical 
         Caption         =   "Tile &Vertical"
      End
      Begin VB.Menu mnuWindowArrangeIcons 
         Caption         =   "&Arrange Icons"
      End
   End
End
Attribute VB_Name = "frmOpenGL"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Dim ProjectIndex As Integer
Dim x_old As Double
Dim y_old As Double
Dim xrot_old As Double
Dim yrot_old As Double
Dim SwitchLights As Boolean
Dim SwitchTextures As Boolean
Dim xspeed As Double, yspeed As Double
Dim anim As Boolean

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
        Case vbKeyL
            SwitchLights = Not SwitchLights
            If SwitchLights = True Then
                glEnable glcLighting
                glEnable glcLight0
            Else
                glDisable glcLighting
                glDisable glcLight0
            End If
        Case vbKeyT
'            SwitchTextures = Not SwitchTextures
'            If SwitchTextures = True Then
'                glDisable glcTexture2D
'            Else
'                glEnable glcTexture2D
'            End If
    End Select
    DrawGLScene
End Sub

Private Sub Form_KeyUp(KeyCode As Integer, Shift As Integer)
    'Keys(KeyCode) = False
End Sub

Private Sub Form_Load()
    ProjectIndex = ActiveProject
    main1 ProjectIndex
'    Time.Enabled = True
'    Time.Interval = 1
    Me.Caption = "3D View - [" & GetFileName(Project.Item(ProjectIndex).sFileName) & "]"
End Sub

Private Sub Form_MouseDown(Button As Integer, Shift As Integer, x As Single, y As Single)
    xspeed = 0
    yspeed = 0
    'anim = Not anim
End Sub

Private Sub Form_MouseMove(Button As Integer, Shift As Integer, x As Single, y As Single)
    If Button = 1 And Shift = 0 Then
        xrot = xrot_old - (x - x_old)
        yrot = yrot_old + (y - y_old)
    End If
    If xrot > 360 Then xrot = xrot - 360
    If yrot > 360 Then yrot = yrot - 360
    If xrot < 0 Then xrot = 360 + xrot
    If yrot < 0 Then yrot = 360 + yrot
    
    'test lock mouse movement
    If Button = 1 Then
        anim = False
'        xspeed = (xrot - xrot_old) '* 0.2
'        yspeed = (yrot - yrot_old) '* 0.2
'    Else
'        Do While anim = True
'            DoEvents
'            If xspeed = 0 And yspeed = 0 Then
'                anim = False
'                Exit Do
'            End If
'            xrot = xrot + xspeed
'            yrot = yrot + yspeed
'            DrawGLScene
'        Loop
'    End If
    End If
'    xspeed = (xrot - xrot_old) '* 0.2
'    yspeed = (yrot - yrot_old) '* 0.2

'        xrot = xrot + xspeed
'        yrot = yrot + yspeed
  
        'If Button = 1 Then
'        xrot = xrot + xspeed
'        yrot = yrot + yspeed
        'DrawGLScene
       ' End If

    
    If Button = 1 And Shift = 1 Then
        x_trans = x_trans - (x - x_old) * 0.1
        y_trans = y_trans + (y - y_old) * 0.1
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

Private Sub Form_MouseUp(Button As Integer, Shift As Integer, x As Single, y As Single)
'    Do While anim = True
'        DoEvents
'        If xspeed = 0 And yspeed = 0 Then
'            anim = False
'            Exit Do
'        End If
'        xrot = xrot + xspeed
'        yrot = yrot + yspeed
'        DrawGLScene
'    Loop
'    If anim = False Then
'        xspeed = 0
'        yspeed = 0
'    End If

If xspeed <> 0 And yspeed <> 0 Then
    anim = True
End If
End Sub

Private Sub Form_Paint()
    DrawGLScene
End Sub

Private Sub Form_Resize()
    ReSizeGLScene ScaleWidth, ScaleHeight
End Sub

Private Sub Form_Unload(Cancel As Integer)
    Keys(vbKeyEscape) = True
    SwitchLights = False
    'SwitchTextures = False
    glDisable glcLighting
    glDisable glcLight0
    'glEnable glcTexture2D
    KillGLWindow
End Sub

Private Sub mnuView_Click()

End Sub

Private Sub mnuWindowArrangeIcons_Click()
    fMainForm.Arrange vbArrangeIcons
End Sub

'Private Sub Time_Timer()
'    Static x As Double
'    Static y As Double
'    xspeed = (x - x_old) '* 0.2
'    yspeed = (y - y_old) '* 0.2
'    x = x_old
'    y = y_old
'        xrot = xrot + xspeed
'        yrot = yrot + yspeed
'        DrawGLScene
'End Sub
Private Sub mnuWindowCascade_Click()
    fMainForm.Arrange vbCascade
End Sub

Private Sub mnuWindowTileHorizontal_Click()
    fMainForm.Arrange vbTileHorizontal
End Sub

Private Sub mnuWindowTileVertical_Click()
    fMainForm.Arrange vbTileVertical
End Sub
