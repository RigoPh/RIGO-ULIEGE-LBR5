Attribute VB_Name = "modDraw3D"
Dim ProjectIndex As Integer
Dim red As GLfloat, green As GLfloat, blue As GLfloat
Dim oPro As cProject
Dim oPan As cPanel
Dim colNodes As colNodes
Dim cColor As ColorConstants
Dim modelwidth As GLfloat
Dim x1 As GLfloat, y1 As GLfloat, z1 As GLfloat, X2 As GLfloat, y2 As GLfloat, z2 As GLfloat 'panel coordinates
Dim bOutline As Boolean

'Coordinate system:
'(lbr5 between brakes, ogl outside brakes)
' |y(z)
' |
' |  /z(x)
' | /
' |/_________ x(y)
  
Public Sub Draw3D(project_index As Integer)

'    ''sphere test
'    glColor3f 1#, 0, 0
'    glTranslatef x_trans, y_trans, ZOOM
'    glutSolidSphere 1#, 20, 16
'    Exit Sub
    
    ProjectIndex = project_index
    Set oPro = Project.Item(ProjectIndex)
    Set colNodes = oPro.colNodes
    modelwidth = oPro.cHeader.Width
    bOutline = True
    glLineWidth 0.000001
    glPushMatrix
    'Draw Plates
    GetRBGFromOLEColour oPro.cDisplaySettings.ColorPlates
    For Each oPan In oPro.colPanel: DrawPlate oPan: Next oPan
    
    'glLineWidth 3
    'Draw Beams
    GetRBGFromOLEColour oPro.cDisplaySettings.ColorBeams
    For Each oPan In oPro.colPanel: DrawBeam oPan: Next oPan
    
    'glLineWidth 1
    'Draw Primary Frames
    'ConvertToRGB vbYellow 'oPro.cDisplaySettings.ColorPrimaryFrames
    GetRBGFromOLEColour oPro.cDisplaySettings.ColorPrimaryFrames
    For Each oPan In oPro.colPanel: DrawPrimaryFrame oPan: Next oPan
    
    'Draw Primary Stiffeners
    GetRBGFromOLEColour oPro.cDisplaySettings.ColorPrimaryStiffeners
    For Each oPan In oPro.colPanel: DrawPrimaryStiffener oPan: Next oPan
    
    'Draw Secondary Frames
    GetRBGFromOLEColour oPro.cDisplaySettings.ColorSecondaryFrames
    For Each oPan In oPro.colPanel: DrawSecondaryFrame oPan: Next oPan
    
    'Draw Secondary Stiffeners
    GetRBGFromOLEColour oPro.cDisplaySettings.ColorSecondaryStiffeners
    For Each oPan In oPro.colPanel: DrawSecondaryStiffener oPan: Next oPan
    
    'Draw Girders
    GetRBGFromOLEColour oPro.cDisplaySettings.ColorGirders
    For Each oPan In oPro.colPanel: DrawGirder oPan: Next oPan
    glPopMatrix
End Sub

Sub setNormal(ByVal xx1 As GLfloat, ByVal yy1 As GLfloat, ByVal zz1 As GLfloat, _
                ByVal xx2 As GLfloat, ByVal yy2 As GLfloat, ByVal zz2 As GLfloat, _
                ByVal xx3 As GLfloat, ByVal yy3 As GLfloat, ByVal zz3 As GLfloat)
    
    Dim xn As GLfloat, yn As GLfloat, zn As GLfloat
    'compute normal
'    zn = (zz2 - zz1) * (xx3 - xx1) - (zz3 - zz1) * (xx2 - xx1)
'    xn = (xx2 - xx1) * (yy3 - yy1) - (xx3 - xx1) * (yy2 - yy1)
'    yn = (yy2 - yy1) * (zz3 - zz1) - (yy3 - yy1) * (zz2 - zz1)
    xn = (yy2 - yy1) * (zz3 - zz1) - (yy3 - yy1) * (zz2 - zz1)
    yn = (zz2 - zz1) * (xx3 - xx1) - (zz3 - zz1) * (xx2 - xx1)
    zn = (xx2 - xx1) * (yy3 - yy1) - (xx3 - xx1) * (yy2 - yy1)
    'normalize
    Dim len_ As GLfloat
    len_ = Sqr(xn ^ 2 + yn ^ 2 + zn ^ 2)
    If len_ = 0# Then len_ = 1#
    xn = xn / len_
    yn = yn / len_
    zn = zn / len_
    glNormal3f xn, yn, zn

End Sub
                

Sub DrawPlate(ByRef oPan As cPanel)
    If oPan.pType <> Plate Then Exit Sub
    If oPro.cDisplaySettings.DrawPlates = no Then Exit Sub
    y1 = colNodes.Item(oPan.cGeometry.InNode).y
    z1 = -colNodes.Item(oPan.cGeometry.InNode).z
    y2 = colNodes.Item(oPan.cGeometry.OutNode).y
    z2 = -colNodes.Item(oPan.cGeometry.OutNode).z

    '(y2-y1)*(z3-z1)-(y3-y1)*(z2-z1)
    '(z2-z1)*(x3-x1)-(z3-z1)*(x2-x1)
    '(x2-x1)*(y3-y1)-(x3-x1)*(y2-y1)
    
'    'GL = lbr
'    'x = y
'    'y = z
'    'z = x
'    x1 = 0
'    X2 = 0
'    X3 = modelwidth
'    y3 = y2
'    z3 = z2
'
'    Dim xn As GLfloat, yn As GLfloat, zn As GLfloat
'    zn = (z2 - z1) * (X3 - x1) - (z3 - z1) * (X2 - x1)
'    xn = (X2 - x1) * (y3 - y1) - (X3 - x1) * (y2 - y1)
'    yn = (y2 - y1) * (z3 - z1) - (y3 - y1) * (z2 - z1)
    
    glColor3d red, green, blue
    glPolygonMode faceFrontAndBack, pgmFILL
    glBegin (bmQuads)
        'glNormal3f xn, yn, zn
        'setNormal 0, y1, z1, 0, y2, z2, modelwidth, y2, z2
        setNormal y1, z1, 0, y2, z2, 0, y2, z2, modelwidth
        glVertex3f y1, z1, 0
        glVertex3f y2, z2, 0
        glVertex3f y2, z2, modelwidth
        glVertex3f y1, z1, modelwidth
        
'        glTexCoord2f 1#, 0#: glVertex3f y1, z1, 0
'        glTexCoord2f 1#, 1#: glVertex3f y2, z2, 0
'        glTexCoord2f 0#, 1#: glVertex3f y2, z2, modelwidth
'        glTexCoord2f 0#, 0#: glVertex3f y1, z1, modelwidth
    glEnd
     
    If bOutline Then
        glColor3d 0, 0, 0
        glPolygonMode faceFrontAndBack, pgmLine
        glBegin (bmQuads)
            glVertex3f y1, z1, 0
            glVertex3f y2, z2, 0
            glVertex3f y2, z2, modelwidth
            glVertex3f y1, z1, modelwidth
        glEnd
    End If
End Sub

Sub DrawBeam(ByRef oPan As cPanel)
    If oPan.pType <> Beam Then Exit Sub
    If oPro.cDisplaySettings.DrawBeams = no Then Exit Sub
    Dim Spacing As GLfloat, sum_spacing As GLfloat
    Dim WebHeight As GLfloat
    Spacing = oPan.cScantlings.cPrimaryFrames.Spacing
    WebHeight = oPan.cScantlings.cPrimaryFrames.WebHeight
    y1 = colNodes.Item(oPan.cGeometry.InNode).y
    z1 = -colNodes.Item(oPan.cGeometry.InNode).z
    y2 = colNodes.Item(oPan.cGeometry.OutNode).y
    z2 = -colNodes.Item(oPan.cGeometry.OutNode).z
    glColor3d red, green, blue
       
    Dim y3 As GLfloat, z3 As GLfloat, y4 As GLfloat, z4 As GLfloat
    Dim y5 As GLfloat, z5 As GLfloat, y6 As GLfloat, z6 As GLfloat
    Dim a As GLfloat
    Dim i As Integer, alfa As GLfloat
    Dim slices As Integer
    slices = 12
    glLineWidth 1
   
    glPolygonMode faceFrontAndBack, pgmFILL
    
    'temp coords for circle section lighting
    Dim xx1 As GLfloat, yy1 As GLfloat, zz1 As GLfloat, _
        xx2 As GLfloat, yy2 As GLfloat, zz2 As GLfloat, _
        xx3 As GLfloat, yy3 As GLfloat, zz3 As GLfloat
    
    sum_spacing = Spacing / 2
    Do While sum_spacing < modelwidth
        Select Case oPan.cScantlings.BeamSection
            Case bsCircle
                alfa = 0
                a = WebHeight * Cos(alfa * PI / 180)
                y3 = y1 - (a) * Cos(alfa * PI / 180)
                x1 = sum_spacing + (a) * Sin(alfa * PI / 180)
                z3 = z1
                y4 = y2 - (a) * Cos(alfa * PI / 180)
                z4 = z2
                glBegin bmQuadStrip
                    glVertex3f y3, z3, x1
                    glVertex3f y4, z4, x1
                    For i = 1 To slices - 1
                        alfa = alfa + 360 / (slices - 1)
                        
                        y5 = y1 - (a) * Cos(alfa * PI / 180)
                        x1 = sum_spacing + (a) * Sin(alfa * PI / 180)
                        z5 = z1
                        y6 = y2 - (a) * Cos(alfa * PI / 180)
                        z6 = z2
                        
                        
                        glVertex3f y6, z5, x1
                        glVertex3f y5, z6, x1
                    Next i
                    glVertex3f y3, z3, x1
                    glVertex3f y4, z4, x1
                glEnd
            Case bsSquare
                'face 1
                y3 = y1 - WebHeight / 2
                x1 = sum_spacing - WebHeight / 2
                z3 = z1
                y4 = y1 - WebHeight / 2
                z4 = z2
                y5 = y2 + WebHeight / 2
                z5 = z2
                y6 = y2 + WebHeight / 2
                z6 = z1
                glColor3d red, green, blue
                glBegin bmQuads
                    setNormal y3, z3, x1, y4, z4, x1, y5, z5, x1
                    glVertex3f y3, z3, x1
                    glVertex3f y4, z4, x1
                    glVertex3f y5, z5, x1
                    glVertex3f y6, z6, x1
                glEnd
                glColor3d 0, 0, 0
                glBegin bmLines
                    glVertex3f y3, z3, x1
                    glVertex3f y4, z4, x1
                glEnd
                'face 2
                y3 = y1 - WebHeight / 2
                x1 = sum_spacing - WebHeight / 2
                z3 = z1
                y4 = y1 - WebHeight / 2
                z4 = z2
                y5 = y2 - WebHeight / 2
                X2 = sum_spacing + WebHeight / 2
                z5 = z2
                y6 = y2 - WebHeight / 2
                z6 = z1
                glColor3d red, green, blue
                glBegin bmQuads
                    setNormal y3, z3, x1, y4, z4, x1, y5, z5, X2
                    glVertex3f y3, z3, x1
                    glVertex3f y4, z4, x1
                    glVertex3f y5, z5, X2
                    glVertex3f y6, z6, X2
                glEnd
                glColor3d 0, 0, 0
                glBegin bmLines
                    glVertex3f y5, z5, X2
                    glVertex3f y6, z6, X2
                glEnd
                'face 3
                y3 = y1 - WebHeight / 2
                x1 = sum_spacing + WebHeight / 2
                z3 = z1
                y4 = y1 - WebHeight / 2
                z4 = z2
                y5 = y2 + WebHeight / 2
                z5 = z2
                y6 = y2 + WebHeight / 2
                z6 = z1
                glColor3d red, green, blue
                glBegin bmQuads
                    setNormal y3, z3, x1, y4, z4, x1, y5, z5, x1
                    glVertex3f y3, z3, x1
                    glVertex3f y4, z4, x1
                    glVertex3f y5, z5, x1
                    glVertex3f y6, z6, x1
                glEnd
                'face 4
                y3 = y1 + WebHeight / 2
                x1 = sum_spacing - WebHeight / 2
                z3 = z1
                y4 = y1 + WebHeight / 2
                z4 = z2
                y5 = y2 + WebHeight / 2
                X2 = sum_spacing + WebHeight / 2
                z5 = z2
                y6 = y2 + WebHeight / 2
                z6 = z1
                glColor3d red, green, blue
                glBegin bmQuads
                    setNormal y3, z3, x1, y4, z4, x1, y5, z5, X2
                    glVertex3f y3, z3, x1
                    glVertex3f y4, z4, x1
                    glVertex3f y5, z5, X2
                    glVertex3f y6, z6, X2
                glEnd
                glColor3d 0, 0, 0
                glBegin bmLines
                    glVertex3f y3, z3, x1
                    glVertex3f y4, z4, x1
                    glVertex3f y5, z5, X2
                    glVertex3f y6, z6, X2
                glEnd
            Case bsDoubleT
            
            Case Else
        End Select
        sum_spacing = sum_spacing + Spacing
    Loop
    
    
End Sub
   
Sub DrawPrimaryFrame(ByRef oPan As cPanel)
    If oPan.pType <> Plate Then Exit Sub
    If oPro.cDisplaySettings.DrawPrimaryFrames = no Then Exit Sub
    Dim Spacing As GLfloat, sum_spacing As GLfloat
    Dim y3 As GLfloat, z3 As GLfloat, y4 As GLfloat, z4 As GLfloat
    Dim y5 As GLfloat, z5 As GLfloat, y6 As GLfloat, z6 As GLfloat
    Dim DY As GLfloat, dZ As GLfloat, WebHeight As GLfloat, FlangeWidth As GLfloat, angle As GLfloat
    
    Spacing = oPan.cScantlings.cPrimaryFrames.Spacing
    If Spacing = 0 Then Exit Sub
    y1 = colNodes.Item(oPan.cGeometry.InNode).y
    z1 = -colNodes.Item(oPan.cGeometry.InNode).z
    y2 = colNodes.Item(oPan.cGeometry.OutNode).y
    z2 = -colNodes.Item(oPan.cGeometry.OutNode).z
    WebHeight = oPan.cScantlings.cPrimaryFrames.WebHeight
    If WebHeight = 0 Then Exit Sub
    FlangeWidth = oPan.cScantlings.cPrimaryFrames.FlangeWidth
    angle = oPan.cGeometry.PanelAngle
    DY = (WebHeight) * (Cos(PI * (angle + 90) / 180))
    dZ = (WebHeight) * (Sin(PI * (angle + 90) / 180))
    Select Case oPan.cScantlings.cPrimaryFrames.Side
        Case SideLeft
            y3 = y2 - DY
            z3 = z2 + dZ
            y4 = y1 - DY
            z4 = z1 + dZ
        Case SideRight
            y3 = y2 + DY
            z3 = z2 - dZ
            y4 = y1 + DY
            z4 = z1 - dZ
    End Select
    
    glColor3d red, green, blue
    glPolygonMode faceFrontAndBack, pgmFILL
    glBegin (bmQuads)
        sum_spacing = Spacing / 2
        Do While sum_spacing < modelwidth
            setNormal y1, z1, sum_spacing, y2, z2, sum_spacing, y3, z3, sum_spacing
            glVertex3f y1, z1, sum_spacing
            glVertex3f y2, z2, sum_spacing
            glVertex3f y3, z3, sum_spacing
            glVertex3f y4, z4, sum_spacing
            'setNormal sum_spacing - FlangeWidth / 2, y3, z3, sum_spacing + FlangeWidth / 2, y3, z3, sum_spacing + FlangeWidth / 2, y4, z4
            setNormal y3, z3, sum_spacing - FlangeWidth / 2, _
                        y3, z3, sum_spacing + FlangeWidth / 2, _
                        y4, z4, sum_spacing + FlangeWidth / 2
            glVertex3f y3, z3, sum_spacing - FlangeWidth / 2
            glVertex3f y3, z3, sum_spacing + FlangeWidth / 2
            glVertex3f y4, z4, sum_spacing + FlangeWidth / 2
            glVertex3f y4, z4, sum_spacing - FlangeWidth / 2
            sum_spacing = sum_spacing + Spacing
        Loop
    glEnd
    
    If bOutline Then
        glColor3d 0, 0, 0
        glPolygonMode faceFrontAndBack, pgmLine
        glBegin (bmQuads)
            sum_spacing = Spacing / 2
            Do While sum_spacing < modelwidth
                glVertex3f y3, z3, sum_spacing - FlangeWidth / 2
                glVertex3f y3, z3, sum_spacing + FlangeWidth / 2
                glVertex3f y4, z4, sum_spacing + FlangeWidth / 2
                glVertex3f y4, z4, sum_spacing - FlangeWidth / 2
                sum_spacing = sum_spacing + Spacing
            Loop
        glEnd
        
        glBegin bmLines
            sum_spacing = Spacing / 2
            Do While sum_spacing < modelwidth
                glVertex3f y1, z1, sum_spacing
                glVertex3f y4, z4, sum_spacing
                glVertex3f y2, z2, sum_spacing
                glVertex3f y3, z3, sum_spacing
                sum_spacing = sum_spacing + Spacing
            Loop
        glEnd
    End If
End Sub

Sub DrawGirder(ByRef oPan As cPanel)
    If oPan.pType <> Plate Then Exit Sub
    If oPro.cDisplaySettings.DrawGirders = no Then Exit Sub
    Dim oGir As cGirder
    Dim y3 As GLfloat, z3 As GLfloat, y4 As GLfloat, z4 As GLfloat
    Dim y5 As GLfloat, z5 As GLfloat, y6 As GLfloat, z6 As GLfloat
    Dim Dist As GLfloat, angle As GLfloat, WebHeight As GLfloat, FlangeWidth As GLfloat
    y1 = colNodes.Item(oPan.cGeometry.InNode).y
    z1 = -colNodes.Item(oPan.cGeometry.InNode).z
    y2 = colNodes.Item(oPan.cGeometry.OutNode).y
    z2 = -colNodes.Item(oPan.cGeometry.OutNode).z
    angle = oPan.cGeometry.PanelAngle
    glColor3d red, green, blue
    glPolygonMode faceFrontAndBack, pgmFILL
    glBegin (bmQuads)
        For Each oGir In oPan.cScantlings.colGirder
            Dist = oGir.Distance
            WebHeight = oGir.WebHeight
            FlangeWidth = oGir.FlangeWidth
            y3 = y1 + Divide(Dist * (y2 - y1), Sqr(((y2 - y1) ^ 2) + ((z2 - z1) ^ 2)))
            z3 = z1 + Divide(Dist * (z2 - z1), Sqr(((y2 - y1) ^ 2) + ((z2 - z1) ^ 2)))
            Select Case oPan.cScantlings.GirderSide
                Case SideLeft
                    y4 = y3 - (WebHeight) * (Cos(PI * (angle + 90) / 180))
                    z4 = z3 + (WebHeight) * (Sin(PI * (angle + 90) / 180))
                Case SideRight
                    y4 = y3 + (WebHeight) * (Cos(PI * (angle + 90) / 180))
                    z4 = z3 - (WebHeight) * (Sin(PI * (angle + 90) / 180))
            End Select
            y5 = y4 - FlangeWidth / 2 * (Cos(PI * (angle + 180) / 180))
            z5 = z4 - FlangeWidth / 2 * (Sin(PI * (angle + 180) / 180))
            y6 = y4 + FlangeWidth / 2 * (Cos(PI * (angle + 180) / 180))
            z6 = z4 + FlangeWidth / 2 * (Sin(PI * (angle + 180) / 180))
            setNormal y3, z3, 0, y3, z3, modelwidth, y4, z4, modelwidth
            glVertex3f y3, z3, 0
            glVertex3f y3, z3, modelwidth
            glVertex3f y4, z4, modelwidth
            glVertex3f y4, z4, 0
            setNormal y5, z5, 0, y5, z5, modelwidth, y6, z6, modelwidth
            glVertex3f y5, z5, 0
            glVertex3f y5, z5, modelwidth
            glVertex3f y6, z6, modelwidth
            glVertex3f y6, z6, 0
        Next oGir
    glEnd
    If bOutline Then
        glColor3d 0, 0, 0
        glPolygonMode faceFrontAndBack, pgmLine
        glBegin (bmQuads)
            For Each oGir In oPan.cScantlings.colGirder
                Dist = oGir.Distance
                WebHeight = oGir.WebHeight
                FlangeWidth = oGir.FlangeWidth
                y3 = y1 + Divide(Dist * (y2 - y1), Sqr(((y2 - y1) ^ 2) + ((z2 - z1) ^ 2)))
                z3 = z1 + Divide(Dist * (z2 - z1), Sqr(((y2 - y1) ^ 2) + ((z2 - z1) ^ 2)))
                Select Case oPan.cScantlings.GirderSide
                    Case SideLeft
                        y4 = y3 - (WebHeight) * (Cos(PI * (angle + 90) / 180))
                        z4 = z3 + (WebHeight) * (Sin(PI * (angle + 90) / 180))
                    Case SideRight
                        y4 = y3 + (WebHeight) * (Cos(PI * (angle + 90) / 180))
                        z4 = z3 - (WebHeight) * (Sin(PI * (angle + 90) / 180))
                End Select
                y5 = y4 - FlangeWidth / 2 * (Cos(PI * (angle + 180) / 180))
                z5 = z4 - FlangeWidth / 2 * (Sin(PI * (angle + 180) / 180))
                y6 = y4 + FlangeWidth / 2 * (Cos(PI * (angle + 180) / 180))
                z6 = z4 + FlangeWidth / 2 * (Sin(PI * (angle + 180) / 180))
                glVertex3f y5, z5, 0
                glVertex3f y5, z5, modelwidth
                glVertex3f y6, z6, modelwidth
                glVertex3f y6, z6, 0
            Next oGir
        glEnd
        
        glBegin (bmLines)
            For Each oGir In oPan.cScantlings.colGirder
                Dist = oGir.Distance
                WebHeight = oGir.WebHeight
                FlangeWidth = oGir.FlangeWidth
                y3 = y1 + Divide(Dist * (y2 - y1), Sqr(((y2 - y1) ^ 2) + ((z2 - z1) ^ 2)))
                z3 = z1 + Divide(Dist * (z2 - z1), Sqr(((y2 - y1) ^ 2) + ((z2 - z1) ^ 2)))
                Select Case oPan.cScantlings.GirderSide
                    Case SideLeft
                        y4 = y3 - (WebHeight) * (Cos(PI * (angle + 90) / 180))
                        z4 = z3 + (WebHeight) * (Sin(PI * (angle + 90) / 180))
                    Case SideRight
                        y4 = y3 + (WebHeight) * (Cos(PI * (angle + 90) / 180))
                        z4 = z3 - (WebHeight) * (Sin(PI * (angle + 90) / 180))
                End Select
                y5 = y4 - FlangeWidth / 2 * (Cos(PI * (angle + 180) / 180))
                z5 = z4 - FlangeWidth / 2 * (Sin(PI * (angle + 180) / 180))
                y6 = y4 + FlangeWidth / 2 * (Cos(PI * (angle + 180) / 180))
                z6 = z4 + FlangeWidth / 2 * (Sin(PI * (angle + 180) / 180))
                glVertex3f y3, z3, 0
                glVertex3f y4, z4, 0
                glVertex3f y3, z3, modelwidth
                glVertex3f y4, z4, modelwidth
    
            Next oGir
        glEnd
    End If
End Sub

Sub DrawPrimaryStiffener(ByRef oPan As cPanel)
    If oPan.pType <> Plate Then Exit Sub
    If oPro.cDisplaySettings.DrawPrimaryStiffeners = no Then Exit Sub
    Dim Spacing As GLfloat, sum_spacing As GLfloat, no_stiff As Integer
    Dim y3 As GLfloat, z3 As GLfloat, y4 As GLfloat, z4 As GLfloat
    Dim y5 As GLfloat, z5 As GLfloat, y6 As GLfloat, z6 As GLfloat
    Dim DY As GLfloat, dZ As GLfloat, WebHeight As GLfloat, FlangeWidth As GLfloat, angle As GLfloat, Width As GLfloat
    Spacing = oPan.cScantlings.cPrimaryStiffeners.Spacing
    If Spacing = 0 Then Exit Sub
    y1 = colNodes.Item(oPan.cGeometry.InNode).y
    z1 = -colNodes.Item(oPan.cGeometry.InNode).z
    y2 = colNodes.Item(oPan.cGeometry.OutNode).y
    z2 = -colNodes.Item(oPan.cGeometry.OutNode).z
    WebHeight = oPan.cScantlings.cPrimaryStiffeners.WebHeight
    If WebHeight = 0 Then Exit Sub
    FlangeWidth = oPan.cScantlings.cPrimaryStiffeners.FlangeWidth
    angle = oPan.cGeometry.PanelAngle
    Width = oPan.cGeometry.PanelWidth
    DY = (WebHeight) * (Cos(PI * (angle + 90) / 180))
    dZ = (WebHeight) * (Sin(PI * (angle + 90) / 180))
    Select Case oPan.cScantlings.cPrimaryStiffeners.DistributionMode
        Case "EE1"
            no_stiff = CInt(Divide(oPan.cGeometry.PanelWidth, Spacing)) - 1
            sum_spacing = Spacing
        Case "EE2"
            no_stiff = CInt(Divide(oPan.cGeometry.PanelWidth, Spacing))
            sum_spacing = Spacing / 2
    End Select
    
    glColor3d red, green, blue
    glPolygonMode faceFrontAndBack, pgmFILL
    glBegin (bmQuads)
        Do While sum_spacing < Width
            y3 = y1 + Divide(sum_spacing * (y2 - y1), Sqr(((y2 - y1) ^ 2) + ((z2 - z1) ^ 2)))
            z3 = z1 + Divide(sum_spacing * (z2 - z1), Sqr(((y2 - y1) ^ 2) + ((z2 - z1) ^ 2)))
            Select Case oPan.cScantlings.cPrimaryStiffeners.Side
                Case SideLeft
                    y4 = y3 - (WebHeight) * (Cos(PI * (angle + 90) / 180))
                    z4 = z3 + (WebHeight) * (Sin(PI * (angle + 90) / 180))
                Case SideRight
                    y4 = y3 + (WebHeight) * (Cos(PI * (angle + 90) / 180))
                    z4 = z3 - (WebHeight) * (Sin(PI * (angle + 90) / 180))
            End Select
            y5 = y4 - FlangeWidth / 2 * (Cos(PI * (angle + 180) / 180))
            z5 = z4 - FlangeWidth / 2 * (Sin(PI * (angle + 180) / 180))
            y6 = y4 + FlangeWidth / 2 * (Cos(PI * (angle + 180) / 180))
            z6 = z4 + FlangeWidth / 2 * (Sin(PI * (angle + 180) / 180))
            
            setNormal y3, z3, 0, y3, z3, modelwidth, y4, z4, modelwidth
            glVertex3f y3, z3, 0
            glVertex3f y3, z3, modelwidth
            glVertex3f y4, z4, modelwidth
            glVertex3f y4, z4, 0
            
            setNormal y5, z5, 0, y5, z5, modelwidth, y6, z6, modelwidth
            glVertex3f y5, z5, 0
            glVertex3f y5, z5, modelwidth
            glVertex3f y6, z6, modelwidth
            glVertex3f y6, z6, 0
            sum_spacing = sum_spacing + Spacing
        Loop
    glEnd
    If bOutline Then
    Select Case oPan.cScantlings.cPrimaryStiffeners.DistributionMode
        Case "EE1"
            no_stiff = CInt(Divide(oPan.cGeometry.PanelWidth, Spacing)) - 1
            sum_spacing = Spacing
        Case "EE2"
            no_stiff = CInt(Divide(oPan.cGeometry.PanelWidth, Spacing))
            sum_spacing = Spacing / 2
    End Select
    glColor3d 0, 0, 0
    glPolygonMode faceFrontAndBack, pgmLine
    glBegin (bmQuads)
        Do While sum_spacing < Width
            y3 = y1 + Divide(sum_spacing * (y2 - y1), Sqr(((y2 - y1) ^ 2) + ((z2 - z1) ^ 2)))
            z3 = z1 + Divide(sum_spacing * (z2 - z1), Sqr(((y2 - y1) ^ 2) + ((z2 - z1) ^ 2)))
            Select Case oPan.cScantlings.cPrimaryStiffeners.Side
                Case SideLeft
                    y4 = y3 - (WebHeight) * (Cos(PI * (angle + 90) / 180))
                    z4 = z3 + (WebHeight) * (Sin(PI * (angle + 90) / 180))
                Case SideRight
                    y4 = y3 + (WebHeight) * (Cos(PI * (angle + 90) / 180))
                    z4 = z3 - (WebHeight) * (Sin(PI * (angle + 90) / 180))
            End Select
            y5 = y4 - FlangeWidth / 2 * (Cos(PI * (angle + 180) / 180))
            z5 = z4 - FlangeWidth / 2 * (Sin(PI * (angle + 180) / 180))
            y6 = y4 + FlangeWidth / 2 * (Cos(PI * (angle + 180) / 180))
            z6 = z4 + FlangeWidth / 2 * (Sin(PI * (angle + 180) / 180))
            glVertex3f y5, z5, 0
            glVertex3f y5, z5, modelwidth
            glVertex3f y6, z6, modelwidth
            glVertex3f y6, z6, 0
            sum_spacing = sum_spacing + Spacing
        Loop
    glEnd

    Select Case oPan.cScantlings.cPrimaryStiffeners.DistributionMode
        Case "EE1"
            no_stiff = CInt(Divide(oPan.cGeometry.PanelWidth, Spacing)) - 1
            sum_spacing = Spacing
        Case "EE2"
            no_stiff = CInt(Divide(oPan.cGeometry.PanelWidth, Spacing))
            sum_spacing = Spacing / 2
    End Select

    glBegin (bmLines)
        Do While sum_spacing < Width
            y3 = y1 + Divide(sum_spacing * (y2 - y1), Sqr(((y2 - y1) ^ 2) + ((z2 - z1) ^ 2)))
            z3 = z1 + Divide(sum_spacing * (z2 - z1), Sqr(((y2 - y1) ^ 2) + ((z2 - z1) ^ 2)))
            Select Case oPan.cScantlings.cPrimaryStiffeners.Side
                Case SideLeft
                    y4 = y3 - (WebHeight) * (Cos(PI * (angle + 90) / 180))
                    z4 = z3 + (WebHeight) * (Sin(PI * (angle + 90) / 180))
                Case SideRight
                    y4 = y3 + (WebHeight) * (Cos(PI * (angle + 90) / 180))
                    z4 = z3 - (WebHeight) * (Sin(PI * (angle + 90) / 180))
            End Select
            y5 = y4 - FlangeWidth / 2 * (Cos(PI * (angle + 180) / 180))
            z5 = z4 - FlangeWidth / 2 * (Sin(PI * (angle + 180) / 180))
            y6 = y4 + FlangeWidth / 2 * (Cos(PI * (angle + 180) / 180))
            z6 = z4 + FlangeWidth / 2 * (Sin(PI * (angle + 180) / 180))
            glVertex3f y3, z3, 0
            glVertex3f y4, z4, 0
            glVertex3f y3, z3, modelwidth
            glVertex3f y4, z4, modelwidth
 
            sum_spacing = sum_spacing + Spacing
        Loop
        glEnd
    End If
End Sub

Sub DrawSecondaryFrame(ByRef oPan As cPanel)
    If oPan.pType <> Plate Then Exit Sub
    If oPro.cDisplaySettings.DrawSecondaryFrames = no Then Exit Sub
    Dim Spacing As GLfloat, sum_spacing As GLfloat
    Dim y3 As GLfloat, z3 As GLfloat, y4 As GLfloat, z4 As GLfloat
    Dim y5 As GLfloat, z5 As GLfloat, y6 As GLfloat, z6 As GLfloat
    Dim DY As GLfloat, dZ As GLfloat, WebHeight As GLfloat, FlangeWidth As GLfloat, angle As GLfloat
    
    Spacing = oPan.cScantlings.cSecondaryFrames.Spacing
    If Spacing = 0 Then Exit Sub
    y1 = colNodes.Item(oPan.cGeometry.InNode).y
    z1 = -colNodes.Item(oPan.cGeometry.InNode).z
    y2 = colNodes.Item(oPan.cGeometry.OutNode).y
    z2 = -colNodes.Item(oPan.cGeometry.OutNode).z
    WebHeight = oPan.cScantlings.cSecondaryFrames.WebHeight
    If WebHeight = 0 Then Exit Sub
    FlangeWidth = oPan.cScantlings.cSecondaryFrames.FlangeWidth
    angle = oPan.cGeometry.PanelAngle
    DY = (WebHeight) * (Cos(PI * (angle + 90) / 180))
    dZ = (WebHeight) * (Sin(PI * (angle + 90) / 180))
    Select Case oPan.cScantlings.cSecondaryFrames.Side
        Case SideLeft
            y3 = y2 - DY
            z3 = z2 + dZ
            y4 = y1 - DY
            z4 = z1 + dZ
        Case SideRight
            y3 = y2 + DY
            z3 = z2 - dZ
            y4 = y1 + DY
            z4 = z1 - dZ
    End Select
    
    glColor3d red, green, blue
    glPolygonMode faceFrontAndBack, pgmFILL
    glBegin (bmQuads)
        sum_spacing = Spacing / 2
        Do While sum_spacing < modelwidth
            setNormal y1, z1, sum_spacing, y2, z2, sum_spacing, y3, z3, sum_spacing
            glVertex3f y1, z1, sum_spacing
            glVertex3f y2, z2, sum_spacing
            glVertex3f y3, z3, sum_spacing
            glVertex3f y4, z4, sum_spacing
            
            setNormal y3, z3, sum_spacing - FlangeWidth / 2, _
                        y3, z3, sum_spacing + FlangeWidth / 2, _
                        y4, z4, sum_spacing + FlangeWidth / 2
            glVertex3f y3, z3, sum_spacing - FlangeWidth / 2
            glVertex3f y3, z3, sum_spacing + FlangeWidth / 2
            glVertex3f y4, z4, sum_spacing + FlangeWidth / 2
            glVertex3f y4, z4, sum_spacing - FlangeWidth / 2
            sum_spacing = sum_spacing + Spacing
        Loop
    glEnd
    If bOutline Then
        glColor3d 0, 0, 0
        glPolygonMode faceFrontAndBack, pgmLine
        glBegin (bmQuads)
            sum_spacing = Spacing / 2
            Do While sum_spacing < modelwidth
                glVertex3f y3, z3, sum_spacing - FlangeWidth / 2
                glVertex3f y3, z3, sum_spacing + FlangeWidth / 2
                glVertex3f y4, z4, sum_spacing + FlangeWidth / 2
                glVertex3f y4, z4, sum_spacing - FlangeWidth / 2
                sum_spacing = sum_spacing + Spacing
            Loop
        glEnd
        
        glBegin bmLines
            sum_spacing = Spacing / 2
            Do While sum_spacing < modelwidth
                glVertex3f y1, z1, sum_spacing
                glVertex3f y4, z4, sum_spacing
                glVertex3f y2, z2, sum_spacing
                glVertex3f y3, z3, sum_spacing
                sum_spacing = sum_spacing + Spacing
            Loop
        glEnd
    End If
End Sub

Sub DrawSecondaryStiffener(ByRef oPan As cPanel)
    If oPan.pType <> Plate Then Exit Sub
    If oPro.cDisplaySettings.DrawSecondaryStiffeners = no Then Exit Sub
    Dim Spacing As GLfloat, sum_spacing As GLfloat, no_stiff As Integer
    Dim y3 As GLfloat, z3 As GLfloat, y4 As GLfloat, z4 As GLfloat
    Dim y5 As GLfloat, z5 As GLfloat, y6 As GLfloat, z6 As GLfloat
    Dim DY As GLfloat, dZ As GLfloat, WebHeight As GLfloat, FlangeWidth As GLfloat, angle As GLfloat, Width As GLfloat
    Spacing = oPan.cScantlings.cSecondaryStiffeners.Spacing
    If Spacing = 0 Then Exit Sub
    y1 = colNodes.Item(oPan.cGeometry.InNode).y
    z1 = -colNodes.Item(oPan.cGeometry.InNode).z
    y2 = colNodes.Item(oPan.cGeometry.OutNode).y
    z2 = -colNodes.Item(oPan.cGeometry.OutNode).z
    WebHeight = oPan.cScantlings.cSecondaryStiffeners.WebHeight
    If WebHeight = 0 Then Exit Sub
    FlangeWidth = oPan.cScantlings.cSecondaryStiffeners.FlangeWidth
    angle = oPan.cGeometry.PanelAngle
    Width = oPan.cGeometry.PanelWidth
    DY = (WebHeight) * (Cos(PI * (angle + 90) / 180))
    dZ = (WebHeight) * (Sin(PI * (angle + 90) / 180))
    sum_spacing = Spacing / 2
        
    glColor3d red, green, blue
    glPolygonMode faceFrontAndBack, pgmFILL
    glBegin (bmQuads)
        Do While sum_spacing < Width
            y3 = y1 + Divide(sum_spacing * (y2 - y1), Sqr(((y2 - y1) ^ 2) + ((z2 - z1) ^ 2)))
            z3 = z1 + Divide(sum_spacing * (z2 - z1), Sqr(((y2 - y1) ^ 2) + ((z2 - z1) ^ 2)))
            Select Case oPan.cScantlings.cSecondaryStiffeners.Side
                Case SideLeft
                    y4 = y3 - (WebHeight) * (Cos(PI * (angle + 90) / 180))
                    z4 = z3 + (WebHeight) * (Sin(PI * (angle + 90) / 180))
                Case SideRight
                    y4 = y3 + (WebHeight) * (Cos(PI * (angle + 90) / 180))
                    z4 = z3 - (WebHeight) * (Sin(PI * (angle + 90) / 180))
            End Select
            y5 = y4 - FlangeWidth / 2 * (Cos(PI * (angle + 180) / 180))
            z5 = z4 - FlangeWidth / 2 * (Sin(PI * (angle + 180) / 180))
            y6 = y4 + FlangeWidth / 2 * (Cos(PI * (angle + 180) / 180))
            z6 = z4 + FlangeWidth / 2 * (Sin(PI * (angle + 180) / 180))
            setNormal y3, z3, 0, y3, z3, modelwidth, y4, z4, modelwidth
            glVertex3f y3, z3, 0
            glVertex3f y3, z3, modelwidth
            glVertex3f y4, z4, modelwidth
            glVertex3f y4, z4, 0
            
            setNormal y5, z5, 0, y5, z5, modelwidth, y6, z6, modelwidth
            glVertex3f y5, z5, 0
            glVertex3f y5, z5, modelwidth
            glVertex3f y6, z6, modelwidth
            glVertex3f y6, z6, 0
            sum_spacing = sum_spacing + Spacing
        Loop
    glEnd
    
    If bOutline Then
        sum_spacing = Spacing / 2
        glColor3d 0, 0, 0
        glPolygonMode faceFrontAndBack, pgmLine
        glBegin (bmQuads)
            Do While sum_spacing < Width
                y3 = y1 + Divide(sum_spacing * (y2 - y1), Sqr(((y2 - y1) ^ 2) + ((z2 - z1) ^ 2)))
                z3 = z1 + Divide(sum_spacing * (z2 - z1), Sqr(((y2 - y1) ^ 2) + ((z2 - z1) ^ 2)))
                Select Case oPan.cScantlings.cSecondaryStiffeners.Side
                    Case SideLeft
                        y4 = y3 - (WebHeight) * (Cos(PI * (angle + 90) / 180))
                        z4 = z3 + (WebHeight) * (Sin(PI * (angle + 90) / 180))
                    Case SideRight
                        y4 = y3 + (WebHeight) * (Cos(PI * (angle + 90) / 180))
                        z4 = z3 - (WebHeight) * (Sin(PI * (angle + 90) / 180))
                End Select
                y5 = y4 - FlangeWidth / 2 * (Cos(PI * (angle + 180) / 180))
                z5 = z4 - FlangeWidth / 2 * (Sin(PI * (angle + 180) / 180))
                y6 = y4 + FlangeWidth / 2 * (Cos(PI * (angle + 180) / 180))
                z6 = z4 + FlangeWidth / 2 * (Sin(PI * (angle + 180) / 180))
                glVertex3f y5, z5, 0
                glVertex3f y5, z5, modelwidth
                glVertex3f y6, z6, modelwidth
                glVertex3f y6, z6, 0
                sum_spacing = sum_spacing + Spacing
            Loop
        glEnd
    
        sum_spacing = Spacing / 2
        glPolygonMode faceFrontAndBack, pgmLine
        glBegin (bmLines)
            Do While sum_spacing < Width
                y3 = y1 + Divide(sum_spacing * (y2 - y1), Sqr(((y2 - y1) ^ 2) + ((z2 - z1) ^ 2)))
                z3 = z1 + Divide(sum_spacing * (z2 - z1), Sqr(((y2 - y1) ^ 2) + ((z2 - z1) ^ 2)))
                Select Case oPan.cScantlings.cSecondaryStiffeners.Side
                    Case SideLeft
                        y4 = y3 - (WebHeight) * (Cos(PI * (angle + 90) / 180))
                        z4 = z3 + (WebHeight) * (Sin(PI * (angle + 90) / 180))
                    Case SideRight
                        y4 = y3 + (WebHeight) * (Cos(PI * (angle + 90) / 180))
                        z4 = z3 - (WebHeight) * (Sin(PI * (angle + 90) / 180))
                End Select
                y5 = y4 - FlangeWidth / 2 * (Cos(PI * (angle + 180) / 180))
                z5 = z4 - FlangeWidth / 2 * (Sin(PI * (angle + 180) / 180))
                y6 = y4 + FlangeWidth / 2 * (Cos(PI * (angle + 180) / 180))
                z6 = z4 + FlangeWidth / 2 * (Sin(PI * (angle + 180) / 180))
                glVertex3f y3, z3, 0
                glVertex3f y4, z4, 0
                glVertex3f y3, z3, modelwidth
                glVertex3f y4, z4, modelwidth
     
                sum_spacing = sum_spacing + Spacing
            Loop
        glEnd
    End If
End Sub

Private Sub GetRBGFromOLEColour(ByVal dwOleColour As Long)
  'pass a hex colour, return the rgb components
   Dim clrref As Long
   Dim s As String
  'translate OLE color to valid color if passed
   OleTranslateColor dwOleColour, 0, clrref
   blue = (dwOleColour \ 65536) And &HFF
   green = (dwOleColour \ 256) And &HFF
   red = dwOleColour And &HFF
   blue = blue / 255
   green = green / 255
   red = red / 255
End Sub
