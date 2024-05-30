Attribute VB_Name = "modOGLUtils"
Option Explicit
Dim project_index As Integer
Public frmOpenGLVisible As Boolean
' a couple of declares to work around some deficiencies of the type library
Private Declare Function EnumDisplaySettings Lib "user32" Alias "EnumDisplaySettingsA" (ByVal lpszDeviceName As Long, ByVal iModeNum As Long, lpDevMode As Any) As Boolean
Private Declare Function ChangeDisplaySettings Lib "user32" Alias "ChangeDisplaySettingsA" (lpDevMode As Any, ByVal dwFlags As Long) As Long
Private Declare Function CreateIC Lib "gdi32" Alias "CreateICA" (ByVal lpDriverName As String, ByVal lpDeviceName As String, ByVal lpOutput As String, ByVal lpInitData As Long) As Long

Private Const CCDEVICENAME = 32
Private Const CCFORMNAME = 32
Private Const DM_BITSPERPEL = &H40000
Private Const DM_PELSWIDTH = &H80000
Private Const DM_PELSHEIGHT = &H100000

Private Type DEVMODE
    dmDeviceName        As String * CCDEVICENAME
    dmSpecVersion       As Integer
    dmDriverVersion     As Integer
    dmSize              As Integer
    dmDriverExtra       As Integer
    dmFields            As Long
    dmOrientation       As Integer
    dmPaperSize         As Integer
    dmPaperLength       As Integer
    dmPaperWidth        As Integer
    dmScale               As Integer
    dmCopies            As Integer
    dmDefaultSource     As Integer
    dmPrintQuality      As Integer
    dmColor             As Integer
    dmDuplex            As Integer
    dmYResolution       As Integer
    dmTTOption          As Integer
    dmCollate           As Integer
    dmFormName          As String * CCFORMNAME
    dmUnusedPadding     As Integer
    dmBitsPerPel        As Integer
    dmPelsWidth         As Long
    dmPelsHeight        As Long
    dmDisplayFlags      As Long
    dmDisplayFrequency  As Long
End Type

Public Keys(255) As Boolean             ' used to keep track of key_downs
Public Const WORLD_LIST = 10000000

Private hrc As Long
Private hdc As Long
Private fullscreen As Boolean

Private OldWidth As Long
Private OldHeight As Long
Private OldBits As Long
Private OldVertRefresh As Long
Public xrot As GLfloat                                  ' X Rotation ( NEW )
Public yrot As GLfloat                                  ' Y Rotation ( NEW )
Public zrot As GLfloat                                  ' Z Rotation ( NEW )

Public x_trans As Double
Public y_trans As Double
Dim xrot_old As Double
Dim yrot_old As Double
Public x_old As Double
Public y_old As Double
Public ZOOM As Long

' user position (viewport)
Public m_fieldOfView As Double
Public m_NearPlane As Double
Public m_FarPlane As Double
Public m_AspectRatio As Double

Public Texture(0) As GLuint                             ' Storage For One Texture ( NEW )
Private mPointerCount As Integer

Dim frm As Form

Dim mat_specular(3) As GLfloat
Dim mat_shininess As GLfloat
Dim light_position(3) As GLfloat
    
Private Function LoadBMP(ByVal FileName As String, ByRef Texture() As GLuint, ByRef Height As Long, ByRef Width As Long) As Boolean
    frm.Picture1.Picture = LoadPicture(FileName)  ' note the SacleMode of the picture control is set to 3 - Pixel
    CreateMapImage frm.Picture1, Texture(), Height, Width ' Create a texture map array from the picture
    LoadBMP = True
End Function

Private Sub CreateMapImage(pict As PictureBox, ByRef TextureImg() As GLbyte, ByRef Height As Long, ByRef Width As Long)
    ' Create the array as needed for the image.
    pict.ScaleMode = 3                  ' Pixels
    Height = pict.ScaleHeight
    Width = pict.ScaleWidth
    
    ReDim TextureImg(2, Height - 1, Width - 1) ' size our texture array
    
    Dim x As Long
    Dim y As Long
    Dim c As Long
    
    Dim yloc As Long
    For x = 0 To Width - 1                      ' loop through every pixel in the image
        For y = 0 To Height - 1
            c = pict.Point(x, y)                ' Returns in long format.
            yloc = Height - y - 1               ' work out where we are in the Y
            TextureImg(0, x, yloc) = c And &HFF     ' get the lower 8 bits Red
            TextureImg(1, x, yloc) = (c \ 256) And &HFF   ' Get the middle 8 bits Green
            TextureImg(2, x, yloc) = (c \ 65536) And &HFF   'Get the top 8 bits Blue
        Next y
    Next x
    
End Sub


Private Function LoadGLTextures() As Boolean
' Load Bitmaps And Convert To Textures
    Dim Status As Boolean
    Dim h As Long
    Dim w As Long
    Dim TextureImage() As GLbyte
    Status = False                         ' Status Indicator

    If LoadBMP(App.Path & "\blue.bmp", TextureImage(), h, w) Then
        ' Load The Bitmap, Check For Errors, If Bitmap's Not Found Quit
        Status = True                          ' Set The Status To TRUE

        glGenTextures 1, Texture(0)                 ' Create The Texture

        ' Typical Texture Generation Using Data From The Bitmap
        glBindTexture glTexture2D, Texture(0)

        ' Generate The Texture
        glTexImage2D glTexture2D, 0, 3, w, h, 0, GL_RGB, GL_UNSIGNED_BYTE, TextureImage(0, 0, 0)
        glTexParameteri glTexture2D, tpnTextureMinFilter, GL_LINEAR     ' Linear Filtering
        glTexParameteri glTexture2D, tpnTextureMagFilter, GL_LINEAR     ' Linear Filtering
'        glTexParameteri glTexture2D, tpnTextureWrapS, GL_CLAMP '_CLAMP_TO_EDGE
'        glTexParameteri glTexture2D, tpnTextureWrapT, GL_CLAMP '_TO_EDGE
    End If

    Erase TextureImage   ' Free the texture image memory
    LoadGLTextures = Status
End Function



Private Sub HidePointer()
    ' hide the cursor (mouse pointer)
    mPointerCount = ShowCursor(False) + 1
    Do While ShowCursor(False) >= -1
    Loop
    Do While ShowCursor(True) <= -1
    Loop
    ShowCursor False
End Sub

Private Sub ShowPointer()
    ' show the cursor (mouse pointer)
    Do While ShowCursor(False) >= mPointerCount
    Loop
    Do While ShowCursor(True) <= mPointerCount
    Loop
End Sub

Public Sub ReSizeGLScene(ByVal Width As GLsizei, ByVal Height As GLsizei)
    Static w&, h&
    Dim W1&, H1&
    W1 = Width
    H1 = Height
    
    If h = 0 Then h = 1

    m_AspectRatio = W1 / H1
    glViewport 0, 0, W1, H1
    SetViewPort
    
    If W1 <= w And H1 <= h Then DrawGLScene
    w = W1: h = H1
End Sub

Private Sub SetViewPort()
    glMatrixMode mmProjection ' Specifies which matrix is the current matrix
    glLoadIdentity 'replaces the current matrix with the identity matrix
    gluPerspective m_fieldOfView, _
                    m_AspectRatio, _
                    m_NearPlane, _
                    m_FarPlane ' sets up a perspective projection matrix
    glMatrixMode mmModelView
    glLoadIdentity
End Sub

Private Sub setCamera()
    glViewport 0, 0, frm.ScaleWidth, frm.ScaleHeight

    glMatrixMode GL_MODELVIEW

    glLoadIdentity

    glMatrixMode (GL_PROJECTION)

    glLoadIdentity

    gluPerspective m_fieldOfView, _
                    m_AspectRatio, _
                    m_NearPlane, _
                    m_FarPlane ' sets up a perspective projection matrix
                    
    glMatrixMode (GL_MODELVIEW)
End Sub

Public Function DisplayList()
    glPushMatrix 'push the current matrix stack
        glNewList WORLD_LIST, lstCompile 'Create or replace a display list
            glTranslatef -x_trans, -y_trans, -ZOOM
            Draw3D project_index
        glEndList 'Ends creation of a display list
    glPopMatrix 'pop the current matrix stack
End Function

Public Function InitGL() As Boolean
    
'    If Not LoadGLTextures Then          ' Jump To Texture Loading Routine ( NEW )
'        InitGL = False                  ' If Texture Didn't Load Return FALSE ( NEW )
'        Exit Function
'    End If

    'glEnable glcTexture2D               ' Enable Texture Mapping ( NEW )
    glShadeModel smSmooth               ' Enables Smooth Shading
    glBlendFunc GL_SRC_ALPHA, GL_DST_ALPHA
    
    'glEnable glcAutoNormal
    'glEnable glcNormalize
    
    glClearColor 0#, 0#, 0#, 0#         ' Black Background

    SetGLLighting
    
    glClearDepth 1#                     ' Depth Buffer Setup
    glEnable glcDepthTest               ' Enables Depth Testing
    glDepthFunc cfLEqual                ' The Type Of Depth Test To Do
    
    glHint htPerspectiveCorrectionHint, hmNicest    ' Really Nice Perspective Calculations
    
    InitGL = True                       ' Initialization Went OK
End Function


Public Sub KillGLWindow()
    ' Properly Kill The Window
    If fullscreen Then                              ' Are We In Fullscreen Mode?
        ResetDisplayMode                            ' If So Switch Back To The Desktop
        ShowPointer                                 ' Show Mouse Pointer
    End If
    
    If hrc Then                                     ' Do We Have A Rendering Context?
        If wglMakeCurrent(0, 0) = 0 Then             ' Are We Able To Release The DC And RC Contexts?
            MsgBox "Release Of DC And RC Failed.", vbInformation, "SHUTDOWN ERROR"
        End If
    
        If wglDeleteContext(hrc) = 0 Then           ' Are We Able To Delete The RC?
            MsgBox "Release Rendering Context Failed.", vbInformation, "SHUTDOWN ERROR"
        End If
        hrc = 0                                     ' Set RC To NULL
    End If
    
    ' Note
    ' The form owns the device context (hDC) window handle (hWnd) and class (RTThundermain)
    ' so we do not have to do all the extra work
    frmOpenGLVisible = False
End Sub

Private Sub SaveCurrentScreen()
    ' Save the current screen resolution, bits, and Vertical refresh
    Dim ret As Long
    ret = CreateIC("DISPLAY", "", "", 0&)
    OldWidth = GetDeviceCaps(ret, HORZRES)
    OldHeight = GetDeviceCaps(ret, VERTRES)
    OldBits = GetDeviceCaps(ret, BITSPIXEL)
    OldVertRefresh = GetDeviceCaps(ret, VREFRESH)
    ret = DeleteDC(ret)
End Sub

Private Function FindDEVMODE(ByVal Width As Integer, ByVal Height As Integer, ByVal Bits As Integer, Optional ByVal VertRefresh As Long = -1) As DEVMODE
    ' locate a DEVMOVE that matches the passed parameters
    Dim ret As Boolean
    Dim i As Long
    Dim dm As DEVMODE
    i = 0
    Do  ' enumerate the display settings until we find the one we want
        ret = EnumDisplaySettings(0&, i, dm)
        If dm.dmPelsWidth = Width And _
            dm.dmPelsHeight = Height And _
            dm.dmBitsPerPel = Bits And _
            ((dm.dmDisplayFrequency = VertRefresh) Or (VertRefresh = -1)) Then Exit Do ' exit when we have a match
        i = i + 1
    Loop Until (ret = False)
    FindDEVMODE = dm
End Function

Private Sub ResetDisplayMode()
    Dim dm As DEVMODE             ' Device Mode
    
    dm = FindDEVMODE(OldWidth, OldHeight, OldBits, OldVertRefresh)
    dm.dmFields = DM_BITSPERPEL Or DM_PELSWIDTH Or DM_PELSHEIGHT
    If OldVertRefresh <> -1 Then
        dm.dmFields = dm.dmFields Or DM_DISPLAYFREQUENCY
    End If
    ' Try To Set Selected Mode And Get Results.  NOTE: CDS_FULLSCREEN Gets Rid Of Start Bar.
    If (ChangeDisplaySettings(dm, CDS_FULLSCREEN) <> DISP_CHANGE_SUCCESSFUL) Then
    
        ' If The Mode Fails, Offer Two Options.  Quit Or Run In A Window.
        MsgBox "The Requested Mode Is Not Supported By Your Video Card", , "NeHe GL"
    End If

End Sub

Private Sub SetDisplayMode(ByVal Width As Integer, ByVal Height As Integer, ByVal Bits As Integer, ByRef fullscreen As Boolean, Optional VertRefresh As Long = -1)
    Dim dmScreenSettings As DEVMODE             ' Device Mode
    Dim p As Long
    SaveCurrentScreen                           ' save the current screen attributes so we can go back later
    
    dmScreenSettings = FindDEVMODE(Width, Height, Bits, VertRefresh)
    dmScreenSettings.dmBitsPerPel = Bits
    dmScreenSettings.dmPelsWidth = Width
    dmScreenSettings.dmPelsHeight = Height
    dmScreenSettings.dmFields = DM_BITSPERPEL Or DM_PELSWIDTH Or DM_PELSHEIGHT
    If VertRefresh <> -1 Then
        dmScreenSettings.dmDisplayFrequency = VertRefresh
        dmScreenSettings.dmFields = dmScreenSettings.dmFields Or DM_DISPLAYFREQUENCY
    End If
    ' Try To Set Selected Mode And Get Results.  NOTE: CDS_FULLSCREEN Gets Rid Of Start Bar.
    If (ChangeDisplaySettings(dmScreenSettings, CDS_FULLSCREEN) <> DISP_CHANGE_SUCCESSFUL) Then
    
        ' If The Mode Fails, Offer Two Options.  Quit Or Run In A Window.
        If (MsgBox("The Requested Mode Is Not Supported By" & vbCr & "Your Video Card. Use Windowed Mode Instead?", vbYesNo + vbExclamation, "NeHe GL") = vbYes) Then
            fullscreen = False                  ' Select Windowed Mode (Fullscreen=FALSE)
        Else
            ' Pop Up A Message Box Letting User Know The Program Is Closing.
            MsgBox "Program Will Now Close.", vbCritical, "ERROR"
            End                   ' Exit And Return FALSE
        End If
    End If
End Sub

Public Function CreateGLWindow(hdc As Long, Width As Integer, Height As Integer, Bits As Integer, fullscreenflag As Boolean) As Boolean
    Dim PixelFormat As GLuint                       ' Holds The Results After Searching For A Match
    Dim pfd As PIXELFORMATDESCRIPTOR                ' pfd Tells Windows How We Want Things To Be


    fullscreen = fullscreenflag                     ' Set The Global Fullscreen Flag
    
    pfd.cColorBits = Bits
    pfd.cDepthBits = 32
    pfd.dwFlags = PFD_DRAW_TO_WINDOW Or PFD_SUPPORT_OPENGL Or PFD_DOUBLEBUFFER Or PFD_TYPE_RGBA
    pfd.iLayerType = PFD_MAIN_PLANE
    pfd.iPixelType = PFD_TYPE_RGBA
    pfd.nSize = Len(pfd)
    pfd.nVersion = 1
    
    PixelFormat = ChoosePixelFormat(hdc, pfd)
    If PixelFormat = 0 Then                     ' Did Windows Find A Matching Pixel Format?
        KillGLWindow                            ' Reset The Display
        MsgBox "Can't Find A Suitable PixelFormat.", vbExclamation, "ERROR"
        CreateGLWindow = False                  ' Return FALSE
    End If

    If SetPixelFormat(hdc, PixelFormat, pfd) = 0 Then ' Are We Able To Set The Pixel Format?
        KillGLWindow                            ' Reset The Display
        MsgBox "Can't Set The PixelFormat.", vbExclamation, "ERROR"
        CreateGLWindow = False                           ' Return FALSE
    End If
    
    hrc = wglCreateContext(hdc)
    If (hrc = 0) Then                           ' Are We Able To Get A Rendering Context?
        KillGLWindow                            ' Reset The Display
        MsgBox "Can't Create A GL Rendering Context.", vbExclamation, "ERROR"
        CreateGLWindow = False                  ' Return FALSE
    End If

    If wglMakeCurrent(hdc, hrc) = 0 Then    ' Try To Activate The Rendering Context
        KillGLWindow                            ' Reset The Display
        MsgBox "Can't Activate The GL Rendering Context.", vbExclamation, "ERROR"
        CreateGLWindow = False                  ' Return FALSE
    End If
    frm.Show                                    ' Show The Window
    frmOpenGLVisible = True
    SetForegroundWindow frm.hwnd                ' Slightly Higher Priority
    frm.SetFocus                                ' Sets Keyboard Focus To The Window
    ReSizeGLScene frm.ScaleWidth, frm.ScaleHeight ' Set Up Our Perspective GL Screen

    If Not InitGL() Then                        ' Initialize Our Newly Created GL Window
        KillGLWindow                            ' Reset The Display
        MsgBox "Initialization Failed.", vbExclamation, "ERROR"
        CreateGLWindow = False                   ' Return FALSE
    End If

    CreateGLWindow = True                       ' Success

End Function

Public Function SetGLLighting() As Boolean
   
 Dim Lum_Pos1(3) As GLfloat
 Dim Lum_Pos2(3) As GLfloat
 Dim Lum_Spec(3) As GLfloat
 
 Dim MatSpec(3) As GLfloat
 Dim MatDif(3) As GLfloat
 Dim MatAmb(3) As GLfloat
 
 MatSpec(0) = 1
 MatSpec(1) = 1
 MatSpec(2) = 1
 MatSpec(3) = 1

 MatDif(0) = 0.8
 MatDif(1) = 0.5
 MatDif(2) = 1
 MatDif(3) = 1
 
 MatAmb(0) = 1
 MatAmb(1) = 1
 MatAmb(2) = 1
 MatAmb(3) = 1

' Lum_Pos1(0) = 0
' Lum_Pos1(1) = 0
' Lum_Pos1(2) = -1
' Lum_Pos1(3) = 1
 Lum_Pos1(0) = -1
 Lum_Pos1(1) = 1
 Lum_Pos1(2) = 1
 Lum_Pos1(3) = 0
 
 Lum_Pos2(0) = 0.1
 Lum_Pos2(1) = 0.1
 Lum_Pos2(2) = 0.1
 Lum_Pos2(3) = 0.5
 
 Lum_Spec(0) = 0
 Lum_Spec(1) = 1
 Lum_Spec(2) = 1
 Lum_Spec(3) = 1

 'glMaterialfv GL_FRONT_AND_BACK, GL_SPECULAR, MatSpec(0)
 'glMaterialfv GL_FRONT_AND_BACK, GL_DIFFUSE, MatDif(0)
' glMaterialfv GL_FRONT_AND_BACK, GL_AMBIENT, MatAmb(0)

 'glLightfv GL_LIGHT0, lpmSpotDirection, Lum_Pos2(0)
 glLightfv GL_LIGHT0, lpmPosition, Lum_Pos1(0)
 glLightfv GL_LIGHT0, lpmSpecular, Lum_Spec(0)
' glLightf GL_LIGHT0, GL_SPOT_CUTOFF, 60
' glLightfv GL_LIGHT1, lpmSpotDirection, Lum_Pos2(0)
' glLightfv GL_LIGHT1, lpmPosition, Lum_Pos2(0)

' glLightfv GL_LIGHT0, GL_SPECULAR, Light1Spec(0)
' glLightfv GL_LIGHT0, GL_AMBIENT, Light1Amb(0)
'glEnable glcLight1
   
    glLightModelf lmTwoSide, 1
    glLightModelf lmAmbient, 1
    glLightModelf lmLocalViewer, 1
    
    
    glEnable glcColorMaterial              ' Enable Material Coloring
End Function

Public Function DrawGLScene() As Boolean
    Dim rad2deg As Double
    Static Busy As Boolean
    Dim eyex As GLdouble, eyey As GLdouble, eyez As GLdouble
    Dim centerx As GLdouble, centery As GLdouble, centerz As GLdouble
    Dim upx As GLdouble, upy As GLdouble, upz As GLdouble
    
    If Busy Then Exit Function
    If project_index = 0 Then project_index = ActiveProject
    Busy = True
    m_fieldOfView = ZOOM
    
    SetViewPort
    
    'SetGLLighting
    glClear clrColorBufferBit Or clrDepthBufferBit  'Clears buffers to preset values, where mask is a bitwise OR of masks that indicate the buffers to be cleared
    
    rad2deg = PI / 180
    eyex = 150 * (Sin(xrot * rad2deg) * Cos(yrot * rad2deg))
    eyey = 150 * (Sin(yrot * rad2deg))
    eyez = 150 * (Cos(xrot * rad2deg) * Cos(yrot * rad2deg))
    centerx = x_trans
    centery = y_trans
    centerz = Project.Item(project_index).cHeader.Width / 2
 
    If Abs(yrot) <= 90 Then
        upx = 0
        upy = 1
        upz = 0
    ElseIf Abs(yrot) > 90 And Abs(yrot) <= 270 Then
        upx = 0
        upy = -1
        upz = 0
    ElseIf Abs(yrot) > 270 Then
        upx = 0
        upy = 1
        upz = 0
    End If
    
    glPushMatrix
        'gluLookAt eyex, eyey, eyez, centerx, centery, centerz, upx, upy, upz
       
        glTranslatef -x_trans, -y_trans, -150#                         ' Move Into The Screen 5 Units
        
        glRotatef yrot, 1#, 0#, 0#                        ' Rotate On The X Axis
        
        glRotatef -xrot, 0#, 1#, 0#                        ' Rotate On The Y Axis
        
        glCallList WORLD_LIST
    
    glPopMatrix
    
    glFinish 'blocks until all OpenGL execution is complete
        
    SwapBuffers hdc 'Exchanges the front and back buffers if the current pixel format for the window referenced by the specified device context includes a back buffer
    Busy = False
End Function

Public Function DrawGLScene1() As Boolean
' Here's Where We Do All The Drawing
    glClear clrColorBufferBit Or clrDepthBufferBit            ' Clear Screen And Depth Buffer
    glLoadIdentity                            ' Reset The Current Matrix
    glTranslatef 0#, 0#, -5#                          ' Move Into The Screen 5 Units
    glRotatef xrot, 1#, 0#, 0#                        ' Rotate On The X Axis
    glRotatef yrot, 0#, 1#, 0#                        ' Rotate On The Y Axis
    glRotatef zrot, 0#, 0#, 1#                        ' Rotate On The Z Axis

    glBindTexture GL_TEXTURE_2D, Texture(0)           ' Select Our Texture

    glBegin GL_QUADS
        ' Front Face
        glTexCoord2f 0#, 0#: glVertex3f -1#, -1#, 1#             ' Bottom Left Of The Texture and Quad
        glTexCoord2f 1#, 0#: glVertex3f 1#, -1#, 1#              ' Bottom Right Of The Texture and Quad
        glTexCoord2f 1#, 1#: glVertex3f 1#, 1#, 1#               ' Top Right Of The Texture and Quad
        glTexCoord2f 0#, 1#: glVertex3f -1#, 1#, 1#              ' Top Left Of The Texture and Quad
        ' Back Face
        glTexCoord2f 1#, 0#: glVertex3f -1#, -1#, -1#            ' Bottom Right Of The Texture and Quad
        glTexCoord2f 1#, 1#: glVertex3f -1#, 1#, -1#             ' Top Right Of The Texture and Quad
        glTexCoord2f 0#, 1#: glVertex3f 1#, 1#, -1#              ' Top Left Of The Texture and Quad
        glTexCoord2f 0#, 0#: glVertex3f 1#, -1#, -1#             ' Bottom Left Of The Texture and Quad
        ' Top Face
        glTexCoord2f 0#, 1#: glVertex3f -1#, 1#, -1#             ' Top Left Of The Texture and Quad
        glTexCoord2f 0#, 0#: glVertex3f -1#, 1#, 1#              ' Bottom Left Of The Texture and Quad
        glTexCoord2f 1#, 0#: glVertex3f 1#, 1#, 1#               ' Bottom Right Of The Texture and Quad
        glTexCoord2f 1#, 1#: glVertex3f 1#, 1#, -1#              ' Top Right Of The Texture and Quad
        ' Bottom Face
        glTexCoord2f 1#, 1#: glVertex3f -1#, -1#, -1#            ' Top Right Of The Texture and Quad
        glTexCoord2f 0#, 1#: glVertex3f 1#, -1#, -1#             ' Top Left Of The Texture and Quad
        glTexCoord2f 0#, 0#: glVertex3f 1#, -1#, 1#              ' Bottom Left Of The Texture and Quad
        glTexCoord2f 1#, 0#: glVertex3f -1#, -1#, 1#             ' Bottom Right Of The Texture and Quad
        ' Right face
        glTexCoord2f 1#, 0#: glVertex3f 1#, -1#, -1#             ' Bottom Right Of The Texture and Quad
        glTexCoord2f 1#, 1#: glVertex3f 1#, 1#, -1#              ' Top Right Of The Texture and Quad
        glTexCoord2f 0#, 1#: glVertex3f 1#, 1#, 1#               ' Top Left Of The Texture and Quad
        glTexCoord2f 0#, 0#: glVertex3f 1#, -1#, 1#              ' Bottom Left Of The Texture and Quad
        ' Left Face
        glTexCoord2f 0#, 0#: glVertex3f -1#, -1#, -1#            ' Bottom Left Of The Texture and Quad
        glTexCoord2f 1#, 0#: glVertex3f -1#, -1#, 1#             ' Bottom Right Of The Texture and Quad
        glTexCoord2f 1#, 1#: glVertex3f -1#, 1#, 1#              ' Top Right Of The Texture and Quad
        glTexCoord2f 0#, 1#: glVertex3f -1#, 1#, -1#             ' Top Left Of The Texture and Quad
    glEnd

    xrot = xrot + 0.3                             ' X Axis Rotation
    yrot = yrot + 0.2                             ' Y Axis Rotation
    zrot = zrot + 0.4                             ' Z Axis Rotation
    
    DrawGLScene1 = True                                 ' Keep Going

End Function

Sub main1(ByRef index As Integer)
    project_index = index
    xrot = -45
    yrot = -15
    Dim Sign As Integer
    
    Sign = IIf(Project.Item(project_index).Ymax + Project.Item(project_index).Ymin > 0, 1, -1)
    
    
    x_trans = Project.Item(project_index).Ymin + (Project.Item(project_index).Ymax - Project.Item(project_index).Ymin) / 2
    y_trans = -Project.Item(project_index).ZMin - (Project.Item(project_index).ZMax - Project.Item(project_index).ZMin) / 2

    ZOOM = Project.Item(project_index).cHeader.Width / 2
    
    m_AspectRatio = 1
    m_FarPlane = 200
    m_NearPlane = 1
    
    m_fieldOfView = ZOOM * 2
    
    Set frm = Project.Item(project_index).frmOpenGL
    ' Ask The User Which Screen Mode They Prefer
    fullscreen = False 'MsgBox("Would You Like To Run In Fullscreen Mode?", vbYesNo + vbQuestion, "Start FullScreen?") = vbYes
    hdc = frm.hdc
    
    If Not CreateGLWindow(hdc, 640, 480, 32, fullscreen) Then
                                     ' Quit If Window Was Not Created
    End If

    SwapBuffers (hdc)               ' Swap Buffers (Double Buffering)
    
    DisplayList
    
    ZOOM = Project.Item(project_index).GetModelWidth
    If ZOOM < Project.Item(project_index).GetModelHeight Then
        ZOOM = Project.Item(project_index).GetModelHeight
    End If
    x_trans = 0
    y_trans = 0
End Sub



