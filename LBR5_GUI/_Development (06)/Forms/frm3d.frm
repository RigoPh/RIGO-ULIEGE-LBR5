VERSION 5.00
Object = "{831FDD16-0C5C-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCTL.OCX"
Begin VB.Form frm3D 
   AutoRedraw      =   -1  'True
   Caption         =   "Test 3D avec matrice + API polygon V1.1"
   ClientHeight    =   7620
   ClientLeft      =   12405
   ClientTop       =   2580
   ClientWidth     =   7935
   FillColor       =   &H000000FF&
   FillStyle       =   0  'Solid
   Icon            =   "frm3d.frx":0000
   MDIChild        =   -1  'True
   ScaleHeight     =   508
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   529
   Begin MSComctlLib.StatusBar StatusBar1 
      Align           =   2  'Align Bottom
      Height          =   375
      Left            =   0
      TabIndex        =   24
      Top             =   7245
      Width           =   7935
      _ExtentX        =   13996
      _ExtentY        =   661
      _Version        =   393216
      BeginProperty Panels {8E3867A5-8586-11D1-B16A-00C0F0283628} 
         NumPanels       =   1
         BeginProperty Panel1 {8E3867AB-8586-11D1-B16A-00C0F0283628} 
         EndProperty
      EndProperty
   End
   Begin VB.PictureBox PIC 
      Appearance      =   0  'Flat
      AutoRedraw      =   -1  'True
      BackColor       =   &H80000005&
      BeginProperty Font 
         Name            =   "Small Fonts"
         Size            =   6
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   6825
      Left            =   0
      ScaleHeight     =   453
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   524
      TabIndex        =   0
      Top             =   30
      Width           =   7890
   End
   Begin VB.CommandButton Command6 
      Caption         =   "AJT"
      Height          =   330
      Left            =   4050
      TabIndex        =   22
      Top             =   585
      Width           =   600
   End
   Begin VB.CommandButton Command5 
      Caption         =   "DEL"
      Height          =   330
      Left            =   4050
      TabIndex        =   21
      Top             =   225
      Width           =   600
   End
   Begin VB.CheckBox CAffPoint 
      Caption         =   "Voir points"
      Height          =   195
      Left            =   45
      TabIndex        =   20
      Top             =   720
      Width           =   1275
   End
   Begin VB.CheckBox cCacher 
      Caption         =   "Voir Faces Cachées"
      Height          =   330
      Left            =   45
      TabIndex        =   19
      Top             =   405
      Value           =   1  'Checked
      Width           =   1770
   End
   Begin VB.Frame Frame1 
      Caption         =   "Position Caméra"
      Height          =   735
      Left            =   6255
      TabIndex        =   12
      Top             =   -45
      Width           =   1455
      Begin VB.CommandButton cXp 
         Caption         =   ">"
         Height          =   195
         Left            =   630
         TabIndex        =   18
         Top             =   360
         Width           =   240
      End
      Begin VB.CommandButton cxm 
         Caption         =   "<"
         Height          =   195
         Left            =   90
         TabIndex        =   17
         Top             =   360
         Width           =   240
      End
      Begin VB.CommandButton cym 
         Caption         =   "v"
         Height          =   195
         Left            =   360
         TabIndex        =   16
         Top             =   450
         Width           =   240
      End
      Begin VB.CommandButton cyp 
         Caption         =   "^"
         Height          =   195
         Left            =   360
         TabIndex        =   15
         Top             =   225
         Width           =   240
      End
      Begin VB.CommandButton czm 
         Caption         =   "^"
         Height          =   195
         Left            =   900
         TabIndex        =   14
         Top             =   225
         Width           =   240
      End
      Begin VB.CommandButton czp 
         Caption         =   "v"
         Height          =   195
         Left            =   900
         TabIndex        =   13
         Top             =   450
         Width           =   240
      End
   End
   Begin VB.CommandButton Command2 
      Caption         =   "MATRICE"
      Height          =   330
      Left            =   2835
      TabIndex        =   11
      Top             =   225
      Width           =   915
   End
   Begin VB.CommandButton Command4 
      Caption         =   "PYR."
      Height          =   330
      Left            =   1935
      TabIndex        =   10
      Top             =   585
      Width           =   870
   End
   Begin VB.CommandButton Command3 
      Caption         =   "CYLIND."
      Height          =   330
      Left            =   2835
      TabIndex        =   9
      Top             =   585
      Width           =   915
   End
   Begin VB.CommandButton Command1 
      Caption         =   "CUBE"
      Height          =   330
      Left            =   1935
      TabIndex        =   8
      Top             =   225
      Width           =   870
   End
   Begin VB.HScrollBar HSC 
      Height          =   195
      Left            =   4860
      Max             =   1000
      Min             =   20
      TabIndex        =   2
      Top             =   585
      Value           =   1000
      Width           =   1095
   End
   Begin VB.CheckBox cFil 
      Caption         =   "Remplir les faces"
      Height          =   240
      Left            =   45
      TabIndex        =   1
      Top             =   180
      Value           =   1  'Checked
      Width           =   1500
   End
   Begin VB.Label Label5 
      Caption         =   "Test"
      Height          =   195
      Left            =   4140
      TabIndex        =   23
      Top             =   0
      Width           =   510
   End
   Begin VB.Label COORD 
      Alignment       =   2  'Center
      Caption         =   "COORD"
      Height          =   195
      Left            =   6210
      TabIndex        =   7
      Top             =   720
      Width           =   1455
   End
   Begin VB.Label Label4 
      Caption         =   "CAMERA"
      Height          =   240
      Left            =   4995
      TabIndex        =   6
      Top             =   90
      Width           =   780
   End
   Begin VB.Label Label3 
      Caption         =   "VUE"
      Height          =   240
      Left            =   450
      TabIndex        =   5
      Top             =   0
      Width           =   510
   End
   Begin VB.Label Label2 
      Caption         =   "OBJET"
      Height          =   195
      Left            =   2565
      TabIndex        =   4
      Top             =   0
      Width           =   915
   End
   Begin VB.Label Label1 
      Caption         =   "Focale XY"
      Height          =   240
      Left            =   4950
      TabIndex        =   3
      Top             =   360
      Width           =   960
   End
End
Attribute VB_Name = "frm3D"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
'TEST 3D avec Matrice & API Polygon
Dim ProjectIndex As Integer

Private OBj As tObj3D
Dim MX As Long
Dim Cam As tCamera
Dim zzz As Double
Dim P2D(1000000) As tPoint2D

Dim VX As tPoint3D
Dim VY As tPoint3D
Dim VZ As tPoint3D
Dim VX2D As tPoint2D
Dim VY2D As tPoint2D
Dim VZ2D As tPoint2D
Dim o As tPoint3D
Dim O2D As tPoint2D
Dim m(16) As Double
Dim Pol(16) As POINTAPI1
Dim dist As tPoint3D
Dim tp As tPolygon

Dim oPro As cProject
' MERCI A VBsorcier pour ce bout de code !!! (je l'ai simplifié mais ton idée et là...)
Dim XStart, YStart

Private Sub Form_Unload(Cancel As Integer)
    With OBj
        .NbPoint = 0
        ReDim .Point(0)
        .NbPolygon = 0
        ReDim .Polygon(0)
    End With
End Sub

Private Sub PIC_MouseMove(Button As Integer, Shift As Integer, X As Single, y As Single)
Dim m(16) As Double
If Button = 1 Then
    For P = 1 To OBj.NbPoint
        CreerMatriceRotationY CDbl((X - XStart) / 100), m
        OBj.Point(P) = TransformePoint3D(OBj.Point(P), m)
        CreerMatriceRotationX CDbl((y - YStart) / 100), m
        OBj.Point(P) = TransformePoint3D(OBj.Point(P), m)
    Next P
    Afficher
   
End If

If Button = 2 Then
    If y < YStart Then
        Cam.FocaleX = Cam.FocaleX + 55
        Cam.FocaleY = Cam.FocaleY + 55
    Else
        Cam.FocaleX = Cam.FocaleX - 55
        Cam.FocaleY = Cam.FocaleY - 55
    End If
    Afficher
End If
XStart = X
YStart = y
End Sub


'pour la rotation de l'objet ***************************************************

' AFFICHAGE ************************************************************

Public Sub Afficher()

On Error Resume Next

'camera à l'echelle de la picture
Cam.width = Pic.ScaleWidth
Cam.height = Pic.ScaleHeight

'DESSIN
Pic.Cls

'AXE XYZ
If cFil = 0 Then

    VX.X = 10
    VY.y = 10
    VZ.z = 10
    VX2D = Projection2D(VX, Cam)
    VY2D = Projection2D(VY, Cam)
    VZ2D = Projection2D(VZ, Cam)
    O2D = Projection2D(o, Cam)
    Pic.Line (O2D.X, O2D.y)-(VX2D.X, VX2D.y), vbBlue
    Pic.Line (O2D.X, O2D.y)-(VY2D.X, VY2D.y), vbRed
    Pic.Line (O2D.X, O2D.y)-(VZ2D.X, VZ2D.y), vbGreen
End If
'OBJET 3D

'calcul les distances centres des polygones >>>  camera
For P = 1 To OBj.NbPolygon
    If OBj.Polygon(P).NbPoint > 1 Then OBj.Polygon(P).Centre = CentrePolygon(OBj, OBj.Polygon(P))
    dist.X = Cam.Position.X - OBj.Polygon(P).Centre.X
    dist.y = Cam.Position.y - OBj.Polygon(P).Centre.y
    dist.z = Cam.Position.z - OBj.Polygon(P).Centre.z
    OBj.Polygon(P).DistanceCam = NormeVecteur3D(dist)
    
Next P

'trier les polygones par rapport à la distance de la caméra pour
'un affichage propre les polygones
TriRapide OBj.Polygon(), 1, OBj.NbPolygon

'On projete les points 3D sur le plan de notre Camera
For P = 1 To OBj.NbPoint
    P2D(P) = Projection2D(OBj.Point(P), Cam)
Next P

'affichage des faces avant des polygones
For P = 1 To OBj.NbPolygon
    For pp = 1 To OBj.Polygon(P).NbPoint
        Pol(pp).X = P2D(OBj.Polygon(P).Point(pp)).X
        Pol(pp).y = P2D(OBj.Polygon(P).Point(pp)).y
    Next pp
    If SensHorairePolygone(Pol()) Or cCacher.Value = 1 Then
        If cFil = 0 Then Pic.FillStyle = 1 Else Pic.FillStyle = 0
        Pic.FillColor = OBj.Polygon(P).Couleur
        Polygon Pic.hdc, Pol(1), OBj.Polygon(P).NbPoint
    End If
Next P
    If CAffPoint = 1 Then
        For P = 1 To OBj.NbPoint
            SetPixel Pic.hdc, P2D(P).X, P2D(P).y, vbRed
            Pic.CurrentX = P2D(P).X
            Pic.CurrentY = P2D(P).y - 5
            Pic.Print P
        Next P
    End If
COORD = "X:" & Round(Cam.Position.X, 2) & " Y:" & Round(Cam.Position.y, 2) & " Z:" & Round(Cam.Position.z, 2)
Me.Refresh
End Sub

'cette fonction determine le sens de rotation des 3 premiers points
'ce qui permet de pas afficher la face arrière d'un polygon (backface culling)
Private Function SensHorairePolygone(P() As POINTAPI1) As Boolean
va1 = P(1).X - P(2).X
vb1 = P(1).y - P(2).y
va2 = P(3).X - P(2).X
vb2 = P(3).y - P(2).y
If va2 * vb1 - va1 * vb2 < 0 Then
    SensHorairePolygone = True
Else
    SensHorairePolygone = False
End If
End Function

'algo de tri plus rapide recursif
Private Sub TriRapide(Tbl() As tPolygon, debut As Long, fin As Long)
Dim pivot As Long
Dim gauche As Long
Dim droite As Long
Dim Tpol  As tPolygon
  pivot = debut
  gauche = debut
  droite = fin
  Do
    If Tbl(gauche).DistanceCam <= Tbl(droite).DistanceCam Then
      Tpol = Tbl(gauche)
      Tbl(gauche) = Tbl(droite)
      Tbl(droite) = Tpol
      pivot = gauche + droite - pivot
    End If
    If pivot = gauche Then droite = droite - 1 Else gauche = gauche + 1
  Loop Until gauche = droite
  If debut < gauche - 1 Then TriRapide Tbl(), debut, gauche - 1
  If fin > droite + 1 Then TriRapide Tbl(), droite + 1, fin
End Sub





'CONTROLES ******************************************************

Private Sub CAffPoint_Click()
Afficher
End Sub

Private Sub cCacher_Click()
Afficher
End Sub

Private Sub cFil_Click()
Afficher
End Sub

Private Sub Command1_Click()
CUBE
Afficher
End Sub

Private Sub Command2_Click()
MATRICE
Afficher
End Sub

Private Sub Command3_Click()
CYLINDRE
Afficher
End Sub

Private Sub Command4_Click()
PYRAMIDE
Afficher
End Sub

Private Sub Command5_Click()
Erase OBj.Point
Erase OBj.Polygon
OBj.NbPoint = 0
OBj.NbPolygon = 0
Afficher
End Sub

Private Sub Command6_Click()
'Form2.Show
End Sub

Private Sub cxm_Click()
Cam.Position.X = Cam.Position.X - 1
Afficher
End Sub

Private Sub cXp_Click()
Cam.Position.X = Cam.Position.X + 1
Afficher
End Sub

Private Sub cym_Click()
Cam.Position.y = Cam.Position.y - 1
Afficher
End Sub

Private Sub cyp_Click()
Cam.Position.y = Cam.Position.y + 1
Afficher
End Sub

Private Sub czm_Click()
Cam.Position.z = Cam.Position.z - 1
Afficher
End Sub

Private Sub czp_Click()
Cam.Position.z = Cam.Position.z + 1
Afficher
End Sub

Private Sub Form_Load()
ProjectIndex = ActiveProject
Me.Caption = "3D View - [" & GetFileName(Project.Item(ProjectIndex).sFileName) & "]"
Pic.BackColor = Project.Item(ProjectIndex).cDisplaySettings.ColorScreen
'init camera
'Cam.FocaleX = 1000
'Cam.FocaleY = 1000
'Cam.Position = XYZ(4, 10, 30)

Dim X As Double, y As Double, z As Double
X = Project.Item(ProjectIndex).GetModelWidth
y = Project.Item(ProjectIndex).GetModelHeight
If Project.Item(ProjectIndex).GetModelHeight > Project.Item(ProjectIndex).cHeader.width Then
    z = Project.Item(ProjectIndex).GetModelHeight
Else
    z = Project.Item(ProjectIndex).cHeader.width
End If
Cam.Position = XYZ(0, 0, 2.2 * z)
'fabrique un cube
'CUBE
DrawModel
Form_Resize
Afficher

End Sub

Private Sub Form_Resize()
Pic.Left = 0
Pic.Top = 0
Pic.width = Me.ScaleWidth
Pic.height = Me.ScaleHeight '- 70
Cam.FocaleX = Pic.ScaleWidth
Cam.FocaleY = Pic.ScaleWidth

End Sub

Private Sub HSC_Change()
Cam.FocaleX = HSC.Value
Cam.FocaleY = HSC.Value
Afficher
End Sub

'OBJET 3D ********************************************************************************

Sub CUBE()
With OBj
    .NbPoint = 8
    ReDim .Point(8)
    .Point(1) = XYZ(-2, -2, 2)
    .Point(2) = XYZ(2, -2, 2)
    .Point(3) = XYZ(2, 2, 2)
    .Point(4) = XYZ(-2, 2, 2)
    .Point(5) = XYZ(-2, -2, -2)
    .Point(6) = XYZ(2, -2, -2)
    .Point(7) = XYZ(2, 2, -2)
    .Point(8) = XYZ(-2, 2, -2)
    .NbPolygon = 6
    ReDim .Polygon(6)
    .Polygon(1) = P1234C(1, 2, 3, 4, vbRed)
    .Polygon(2) = P1234C(8, 7, 6, 5, vbYellow)
    .Polygon(3) = P1234C(5, 6, 2, 1, vbGreen)
    .Polygon(4) = P1234C(7, 8, 4, 3, vbBlue)
    .Polygon(5) = P1234C(6, 7, 3, 2, vbCyan)
    .Polygon(6) = P1234C(1, 4, 8, 5, vbMagenta)
    End With
End Sub


Sub PYRAMIDE()
With OBj
    .NbPoint = 4
    ReDim .Point(4)
    .Point(1) = XYZ(-4, -2, 0)
    .Point(2) = XYZ(4, -2, 0)
    .Point(3) = XYZ(0, 4, 0)
    .Point(4) = XYZ(0, 0, 4)
    .NbPolygon = 4
    ReDim .Polygon(4)
    .Polygon(1) = P123C(3, 2, 1, vbRed)
    .Polygon(2) = P123C(1, 2, 4, vbBlue)
    .Polygon(3) = P123C(4, 3, 1, vbYellow)
    .Polygon(4) = P123C(2, 3, 4, vbGreen)
End With
End Sub



Sub CYLINDRE()
Dim T1 As Long
Dim T2 As Long
Dim T3 As Long
    xx = 43
With OBj
    .NbPoint = xx
    ReDim .Point(xx)

For t = 1 To 20
    np = np + 1
    .Point(np) = XYZ(Cos(t / 20 * PI * 2) * 2, Sin(t / 20 * PI * 2) * 2, -2)
    np = np + 1
    .Point(np) = XYZ(Cos(t / 20 * PI * 2) * 2, Sin(t / 20 * PI * 2) * 2, 2)
Next t
    np = np + 1
    .Point(np) = XYZ(0, 0, -2)
        np = np + 1
    .Point(np) = XYZ(0, 0, 2)
    .NbPolygon = 83
    ReDim .Polygon(83)
For T1 = 1 To 38 Step 2
    ff = ff + 1
    .Polygon(ff) = P123C(T1, T1 + 2, T1 + 1, vbBlue)
  ff = ff + 1
    .Polygon(ff) = P123C(T1 + 2, T1 + 3, T1 + 1, vbRed)
Next T1
    ff = ff + 1
    .Polygon(ff) = P123C(T1, 1, T1 + 1, vbBlue)
    ff = ff + 1
    .Polygon(ff) = P123C(1, 2, T1 + 1, vbRed)
For t = 1 To 20
    ff = ff + 1
    .Polygon(ff) = P123C(42, t * 2, t * 2 + 2, vbRed)
    ff = ff + 1
   .Polygon(ff) = P123C(t * 2 + 3, t * 2 + 1, 41, vbBlue)
Next t
    ff = ff + 1
    .Polygon(ff) = P123C(42, 40, 2, vbRed)
    ff = ff + 1
   .Polygon(ff) = P123C(3, 1, 41, vbBlue)
    ff = ff + 1
   .Polygon(ff) = P123C(1, 39, 41, vbBlue)
End With
End Sub



Sub MATRICE()
With OBj
nb = 16
.NbPoint = nb * nb
ReDim .Point(.NbPoint)
For xx = 0 To nb - 1
    For zz = 0 To nb - 1
        np = np + 1
        yy = Sin(xx) / 1.2 * Cos(zz) / 1.2
        .Point(np) = XYZ(xx - (nb / 2), yy, zz - (nb / 2))
    Next zz
Next xx
np = 0
.NbPolygon = (nb * nb) * 2
ReDim .Polygon(.NbPolygon)
Dim P1 As Long
Dim P2 As Long
Dim P3 As Long
Dim P4 As Long
For xx = 0 To nb - 3
    For zz = 0 To nb - 3
        P1 = xx + 1 + ((zz + 1) * nb)
        P2 = xx + 2 + ((zz + 1) * nb)
        P3 = xx + 2 + ((zz + 2) * nb)
        P4 = xx + 1 + ((zz + 2) * nb)
        np = np + 1
        .Polygon(np) = P123C(P1, P2, P4, vbGreen)
        np = np + 1
        .Polygon(np) = P123C(P2, P3, P4, RGB(0, 200, 0))
    Next zz
Next xx
End With
End Sub

'DEBUT D'EDITEUR  (lol vraiment un début)

Sub AjouterPoint(X As Double, y As Double, z As Double)
OBj.NbPoint = OBj.NbPoint + 1
ReDim Preserve OBj.Point(OBj.NbPoint)
OBj.Point(OBj.NbPoint) = XYZ(X, y, z)
End Sub

Sub AjouterPolygone(P1 As Long, P2 As Long, P3 As Long, c As Long)
OBj.NbPolygon = OBj.NbPolygon + 1
ReDim Preserve OBj.Polygon(OBj.NbPolygon)
OBj.Polygon(OBj.NbPolygon) = P123C(P1, P2, P3, c)
OBj.Polygon(OBj.NbPolygon).NbPoint = 3
End Sub



'======================================
'======================================
'======================================
'Draw model

Sub DrawModel()
    Dim i As Integer
    Dim n1 As Double, n2 As Double, n3 As Double, n4 As Double
    
    Dim opan As cPanel
    Dim oNod As cNode
    Set oPro = Project.Item(ProjectIndex)
    Dim color As Long
    Dim width As Double
    width = oPro.cHeader.width
    
    'NODES
    'real nodes
    For Each oNod In oPro.colNodes
        DrawNode oNod.y, oNod.z, 0
    Next oNod
    'doubling nodes
    For Each oNod In oPro.colNodes
        DrawNode oNod.y, oNod.z, width
    Next oNod
    
    'PLATES
    
    color = Project.Item(ProjectIndex).cDisplaySettings.ColorPlates
    For Each opan In oPro.colPanel
        If opan.pType = Plate Then DrawPlate opan, color
    Next opan
    
    'BEAMS
    color = Project.Item(ProjectIndex).cDisplaySettings.ColorBeams
    For Each opan In oPro.colPanel
        If opan.pType = Beam Then DrawBeam opan, width, color
    Next opan
    
    'PRIMARYFRAMES
    color = Project.Item(ProjectIndex).cDisplaySettings.ColorPrimaryFrames
    For Each opan In oPro.colPanel
        If opan.pType = Plate Then DrawPrimaryFrame opan, width, color
    Next opan
    
    
    Set oPro = Nothing
End Sub

Sub DrawNode(ByRef y As Double, ByRef z As Double, ByRef X As Double)
    With OBj
        .NbPoint = .NbPoint + 1
        ReDim Preserve .Point(.NbPoint)
        .Point(.NbPoint) = XYZ(y, -z, X)
    End With
End Sub

Sub DrawPlate(ByRef opan As cPanel, ByRef color As Long)
    Dim n1 As Long, n2 As Long, n3 As Long, n4 As Long
    With OBj
        .NbPolygon = .NbPolygon + 1
        ReDim Preserve .Polygon(.NbPolygon)
        n1 = opan.cGeometry.InNode
        n2 = opan.cGeometry.OutNode
        n3 = opan.cGeometry.OutNode + Project.Item(ProjectIndex).colNodes.Count
        n4 = opan.cGeometry.InNode + Project.Item(ProjectIndex).colNodes.Count
        .Polygon(.NbPolygon) = P1234C(n1, n2, n3, n4, color)
    End With
End Sub


Sub DrawBeam(ByRef opan As cPanel, ByRef width As Double, ByRef color As Long)
    Dim n1 As Long, n2 As Long, n3 As Long, n4 As Long
    Dim spacing As Double, height As Double
    Dim nodein As Integer, nodeout As Integer
    spacing = opan.cScantlings.cPrimaryFrames.spacing
    height = opan.cScantlings.cPrimaryFrames.WebHeight
    nodein = opan.cGeometry.InNode
    nodeout = opan.cGeometry.OutNode
    DrawNode oPro.colNodes.Item(nodein).y - height / 2, oPro.colNodes.Item(nodein).z, spacing / 2
    DrawNode oPro.colNodes.Item(nodein).y + height / 2, oPro.colNodes.Item(nodein).z, spacing / 2
    DrawNode oPro.colNodes.Item(nodeout).y + height / 2, oPro.colNodes.Item(nodeout).z, spacing / 2
    DrawNode oPro.colNodes.Item(nodeout).y - height / 2, oPro.colNodes.Item(nodeout).z, spacing / 2
    
    With OBj
        .NbPolygon = .NbPolygon + 1
        ReDim Preserve .Polygon(.NbPolygon)
        .Polygon(.NbPolygon) = P1234C(.NbPoint - 3, .NbPoint - 2, .NbPoint - 1, .NbPoint, color)
        Dim sum As Double
        sum = spacing / 2 + spacing
        Do While sum < width
            DrawNode oPro.colNodes.Item(nodein).y - height / 2, oPro.colNodes.Item(nodein).z, spacing + .Point(.NbPoint - 3).z
            DrawNode oPro.colNodes.Item(nodein).y + height / 2, oPro.colNodes.Item(nodein).z, spacing + .Point(.NbPoint - 3).z
            DrawNode oPro.colNodes.Item(nodeout).y + height / 2, oPro.colNodes.Item(nodeout).z, spacing + .Point(.NbPoint - 3).z
            DrawNode oPro.colNodes.Item(nodeout).y - height / 2, oPro.colNodes.Item(nodeout).z, spacing + .Point(.NbPoint - 3).z
            .NbPolygon = .NbPolygon + 1
            ReDim Preserve .Polygon(.NbPolygon)
            .Polygon(.NbPolygon) = P1234C(.NbPoint - 3, .NbPoint - 2, .NbPoint - 1, .NbPoint, color)
            sum = sum + spacing
        Loop
   
    End With
End Sub

Sub DrawPrimaryFrame(ByRef opan As cPanel, ByRef width As Double, ByRef color As Long)
    Dim n1 As Long, n2 As Long, n3 As Long, n4 As Long
    Dim spacing As Double, height As Double
    Dim nodein As Integer, nodeout As Integer
    Dim dy As Double, dz As Double, WebHeight As Double, Angle As Double
    spacing = opan.cScantlings.cPrimaryFrames.spacing
    height = opan.cScantlings.cPrimaryFrames.WebHeight
    nodein = opan.cGeometry.InNode
    nodeout = opan.cGeometry.OutNode
    DrawNode oPro.colNodes.Item(nodein).y, oPro.colNodes.Item(nodein).z, spacing / 2
    DrawNode oPro.colNodes.Item(nodeout).y, oPro.colNodes.Item(nodeout).z, spacing / 2
    WebHeight = opan.cScantlings.cPrimaryFrames.WebHeight
    Angle = opan.cGeometry.PanelAngle
    dy = (WebHeight) * (Cos(PI * (Angle + 90) / 180))
    dz = (WebHeight) * (Sin(PI * (Angle + 90) / 180))
    With OBj
        Select Case opan.cScantlings.cPrimaryFrames.Side
            Case SideLeft
                DrawNode oPro.colNodes.Item(nodeout).y - dy, oPro.colNodes.Item(nodeout).z - dz, spacing / 2
                DrawNode oPro.colNodes.Item(nodein).y - dy, oPro.colNodes.Item(nodein).z - dz, spacing / 2
            Case SideRight
                DrawNode oPro.colNodes.Item(nodeout).y + dy, oPro.colNodes.Item(nodeout).z + dz, spacing / 2
                DrawNode oPro.colNodes.Item(nodein).y + dy, oPro.colNodes.Item(nodein).z + dz, spacing / 2
            Case none
                Exit Sub
        End Select
        .NbPolygon = .NbPolygon + 1
        ReDim Preserve .Polygon(.NbPolygon)
        .Polygon(.NbPolygon) = P1234C(.NbPoint - 3, .NbPoint - 2, .NbPoint - 1, .NbPoint, color)
        Dim sum As Double
        sum = spacing / 2 + spacing
        Do While sum < width
            DrawNode oPro.colNodes.Item(nodein).y, oPro.colNodes.Item(nodein).z, sum
            DrawNode oPro.colNodes.Item(nodeout).y, oPro.colNodes.Item(nodeout).z, sum
            Select Case opan.cScantlings.cPrimaryFrames.Side
                Case SideLeft
                    DrawNode oPro.colNodes.Item(nodeout).y - dy, oPro.colNodes.Item(nodeout).z - dz, sum
                    DrawNode oPro.colNodes.Item(nodein).y - dy, oPro.colNodes.Item(nodein).z - dz, sum
                Case SideRight
                    DrawNode oPro.colNodes.Item(nodeout).y + dy, oPro.colNodes.Item(nodeout).z + dz, sum
                    DrawNode oPro.colNodes.Item(nodein).y + dy, oPro.colNodes.Item(nodein).z + dz, sum
                Case none
                    Exit Sub
            End Select
            .NbPolygon = .NbPolygon + 1
            ReDim Preserve .Polygon(.NbPolygon)
            .Polygon(.NbPolygon) = P1234C(.NbPoint - 3, .NbPoint - 2, .NbPoint - 1, .NbPoint, color)
            sum = sum + spacing
        Loop
    End With
End Sub

Sub DrawPrimaryStiffener(ByRef opan As cPanel, ByRef width As Double, ByRef color As Long)
    Dim n1 As Long, n2 As Long, n3 As Long, n4 As Long
    Dim spacing As Double, WebHeight As Double, FlangeWidth As Double
    Dim nodein As Integer, nodeout As Integer
    Dim dy As Double, dz As Double, Angle As Double
    spacing = opan.cScantlings.cPrimaryStiffeners.spacing
    WebHeight = opan.cScantlings.cPrimaryStiffeners.WebHeight
    FlangeWidth = opan.cScantlings.cPrimaryStiffeners.FlangeWidth
    Angle = opan.cGeometry.PanelAngle
    
End Sub



