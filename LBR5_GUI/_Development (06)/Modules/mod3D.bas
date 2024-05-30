Attribute VB_Name = "mod3D"
Public Declare Function Polygon Lib "gdi32" (ByVal hdc As Long, lpPoint As POINTAPI1, ByVal nCount As Long) As Long
Public Declare Function SetPixel Lib "gdi32" (ByVal hdc As Long, ByVal X As Long, ByVal y As Long, ByVal crColor As Long) As Long

'coordonnées 2D long pour  API
Public Type POINTAPI1
    X As Long
    y As Long
End Type

'coordonnées 2D ou vecteur 2D ou angle2D
Public Type tPoint2D
    X As Double
    y As Double
End Type

'coordonnées 3D ou  vecteur 3D ou angle3D
Public Type tPoint3D
    X As Double
    y As Double
    z As Double
End Type

Public Type tPolygon
    NbPoint As Byte
    Point(16) As Long
    Centre As tPoint3D
    Couleur As Long
    DistanceCam As Double
End Type

Public Type tObj3D
    Point() As tPoint3D
    NbPoint As Long
    Polygon() As tPolygon
    NbPolygon As Long
End Type

Public Type tCamera
    width As Long
    height As Long
    FocaleX As Double
    FocaleY As Double
    Position As tPoint3D
    Cible As tPoint3D
End Type

'Public Const PI = 3.14159265

''matrices diverses pour manip
Private m(16) As Double
Private m1(16) As Double
Private m2(16) As Double
Private m3(16) As Double
Private m4(16) As Double
Private m5(16) As Double

'création rapide de point3D
Function XYZ(X, y, z) As tPoint3D
    XYZ.X = X
    XYZ.y = y
    XYZ.z = z
End Function
'création rapide de facette 3P
Function P123C(P1 As Long, P2 As Long, P3 As Long, Couleur As Long) As tPolygon
    P123C.NbPoint = 3
    P123C.Point(1) = P1
    P123C.Point(2) = P2
    P123C.Point(3) = P3
    P123C.Couleur = Couleur
End Function

'création rapide de facette 4P
Function P1234C(P1 As Long, P2 As Long, P3 As Long, P4 As Long, Couleur As Long) As tPolygon
    P1234C.NbPoint = 4
    P1234C.Point(1) = P1
    P1234C.Point(2) = P2
    P1234C.Point(3) = P3
    P1234C.Point(4) = P4
    P1234C.Couleur = Couleur
End Function

'calcul le point centre d'un polygone
Public Function CentrePolygon(OBj As tObj3D, Pol As tPolygon) As tPoint3D
For P = 1 To Pol.NbPoint
    CentrePolygon.X = CentrePolygon.X + OBj.Point(Pol.Point(P)).X
    CentrePolygon.y = CentrePolygon.y + OBj.Point(Pol.Point(P)).y
    CentrePolygon.z = CentrePolygon.z + OBj.Point(Pol.Point(P)).z
Next P
CentrePolygon.X = CentrePolygon.X / Pol.NbPoint
CentrePolygon.y = CentrePolygon.y / Pol.NbPoint
CentrePolygon.z = CentrePolygon.z / Pol.NbPoint
End Function

'Projection d'un point 3D sur un plan 2D par rapport a une "camera"
Public Function Projection2D(P3D As tPoint3D, Cam As tCamera) As tPoint2D

Dim P As tPoint3D
Dim V As tPoint3D
Dim ax As Double
Dim ay As Double

'changement de plan :

V.X = Cam.Cible.X - Cam.Position.X
V.y = Cam.Cible.y - Cam.Position.y
V.z = Cam.Cible.z - Cam.Position.z

'translation
CreerMatriceTranslation V, m1

'+ rotation suivant positionCAM et cible CAM
'(marche pas terrible avec Z négatif à modifier)

ax = ArcSin(-V.y / NormeVecteur3D(V))
ay = ArcSin(V.X / NormeVecteur3D(V))

CreerMatriceRotationY ay, m2
CreerMatriceRotationX ax, m3

MultiplierLesMatrices m4, m2, m1
MultiplierLesMatrices m, m3, m4

'applique la translation et les rotations au point
P = TransformePoint3D(P3D, m)

 'projection
 Select Case P.z
 Case 0
    Projection2D.X = (Cam.FocaleX * P.X) + (Cam.width / 2)
    Projection2D.y = (Cam.height / 2) - (Cam.FocaleY * P.y)
 Case Else
    Projection2D.X = ((Cam.FocaleX * P.X) / -P.z) + (Cam.width / 2)
    Projection2D.y = (Cam.height / 2) - ((Cam.FocaleY * P.y) / -P.z)
 End Select

End Function

'renvoi la norme d'un vecteur
Public Function NormeVecteur3D(V1 As tPoint3D) As Double
    NormeVecteur3D = Sqr(V1.X ^ 2 + V1.y ^ 2 + V1.z ^ 2)
End Function
'renvoi la sommme d'un vecteur
Public Function SommeVecteur3D(V1 As tPoint3D, V2 As tPoint3D) As tPoint3D
With SommeVecteur3D
    .X = V1.X + V2.X
    .y = V1.y + V2.y
    .z = V1.z + V2.z
End With
End Function
'renvoi le scalaire d'un vecteur
Public Function ScalaireVecteur3D(V1 As tPoint3D, V2 As tPoint3D) As Double
    ScalaireVecteur3D = V1.X * V2.X + V1.y * V2.y + V1.z * V2.z
End Function

'renvoi le vectoriel d'un vecteur
Public Function VectorielVecteur3D(V1 As tPoint3D, V2 As tPoint3D) As tPoint3D
With VectorielVecteur3D
    .X = V1.y * V2.z - V2.y * V1.z
    .y = V1.z * V2.X - V2.z * V1.X
    .z = V1.X * V2.y - V2.X * V1.y
End With
End Function

'Public Function ArcSin(x)
'    On Error Resume Next
'    ArcSin = Atn(x / Sqr(-x * x + 1))
'End Function

'Public Function ArcSin(x)
'    On Error Resume Next
'    ArcSin = Atn(x / Sqr(-x * x + 1))
'End Function

'Function ArcCos(x)
'    On Error Resume Next
'    ArcCos = Atn(-x / Sqr(-x * x + 1)) + 2 * Atn(1)
'End Function

'***********************  CALCUL MATRICIEL  *************************

'Format des matrices
'|  1   2   3   4   |
'|  5   6   7   8   |   Matrice(X)
'|  9   10  11  12  |
'|  13  14  15  16  |

'Transforme un point 3D en un autre par rapport à une matrice
Function TransformePoint3D(P3D As tPoint3D, MATRICE() As Double) As tPoint3D
On Error GoTo fin
Dim W As Double
TransformePoint3D.X = MATRICE(1) * P3D.X + MATRICE(2) * P3D.y + MATRICE(3) * P3D.z + MATRICE(4)
TransformePoint3D.y = MATRICE(5) * P3D.X + MATRICE(6) * P3D.y + MATRICE(7) * P3D.z + MATRICE(8)
TransformePoint3D.z = MATRICE(9) * P3D.X + MATRICE(10) * P3D.y + MATRICE(11) * P3D.z + MATRICE(12)
W = MATRICE(13) * P3D.X + MATRICE(14) * P3D.y + MATRICE(15) * P3D.z + MATRICE(16)

If W <> 0 Then
    TransformePoint3D.X = TransformePoint3D.X / W
    TransformePoint3D.y = TransformePoint3D.y / W
    TransformePoint3D.z = TransformePoint3D.z / W
End If

Exit Function
fin:
TransformePoint3D = P3D
End Function
'création d'une matrice identité (pas de transformation)
Sub CreerMatriceIdentite(MATRICE() As Double)
    Erase MATRICE
    MATRICE(1) = 1
    MATRICE(6) = 1
    MATRICE(11) = 1
    MATRICE(16) = 1
End Sub
'création d'une matrice de translation
Sub CreerMatriceTranslation(Vecteur As tPoint3D, MATRICE() As Double)
    CreerMatriceIdentite MATRICE
    MATRICE(4) = Vecteur.X
    MATRICE(8) = Vecteur.y
    MATRICE(12) = Vecteur.z
End Sub
'creation d'une matrice de changement d'échelle
Sub CreerMatriceEchelle(Facteur As tPoint3D, MATRICE() As Double)
    CreerMatriceIdentite MATRICE
    MATRICE(1) = Facteur.X
    MATRICE(6) = Facteur.y
    MATRICE(11) = Facteur.z
End Sub
'creation d'une matrice de rotation X
Sub CreerMatriceRotationX(Angle As Double, MATRICE() As Double)
    CreerMatriceIdentite MATRICE
    MATRICE(6) = Cos(Angle)
    MATRICE(7) = -Sin(Angle)
    MATRICE(10) = Sin(Angle)
    MATRICE(11) = Cos(Angle)
End Sub
'creation d'une matrice de rotation Y
Sub CreerMatriceRotationY(Angle As Double, MATRICE() As Double)
    CreerMatriceIdentite MATRICE
    MATRICE(1) = Cos(Angle)
    MATRICE(9) = -Sin(Angle)
    MATRICE(3) = Sin(Angle)
    MATRICE(11) = Cos(Angle)
End Sub
'creation d'une matrice de rotation Z
Sub CreerMatriceRotationZ(Angle As Double, MATRICE() As Double)
    CreerMatriceIdentite MATRICE
    MATRICE(1) = Cos(Angle)
    MATRICE(2) = -Sin(Angle)
    MATRICE(5) = Sin(Angle)
    MATRICE(6) = Cos(Angle)
End Sub
'Creation d'une matrice resultat de la Multiplication de 2 matrices
'pour enchainer les transformations
Sub MultiplierLesMatrices(MatR() As Double, Mat1() As Double, Mat2() As Double)
     MatR(1) = (Mat1(1) * Mat2(1)) + (Mat1(2) * Mat2(5)) + (Mat1(3) * Mat2(9)) + (Mat1(4) * Mat2(13))
     MatR(5) = (Mat1(5) * Mat2(1)) + (Mat1(6) * Mat2(5)) + (Mat1(7) * Mat2(9)) + (Mat1(8) * Mat2(13))
     MatR(9) = (Mat1(9) * Mat2(1)) + (Mat1(10) * Mat2(5)) + (Mat1(11) * Mat2(9)) + (Mat1(12) * Mat2(13))
     MatR(13) = (Mat1(13) * Mat2(1)) + (Mat1(14) * Mat2(5)) + (Mat1(15) * Mat2(9)) + (Mat1(16) * Mat2(13))
     MatR(2) = (Mat1(1) * Mat2(2)) + (Mat1(2) * Mat2(6)) + (Mat1(3) * Mat2(10)) + (Mat1(4) * Mat2(14))
     MatR(6) = (Mat1(5) * Mat2(2)) + (Mat1(6) * Mat2(6)) + (Mat1(7) * Mat2(10)) + (Mat1(8) * Mat2(14))
     MatR(10) = (Mat1(9) * Mat2(2)) + (Mat1(10) * Mat2(6)) + (Mat1(11) * Mat2(10)) + (Mat1(12) * Mat2(14))
     MatR(14) = (Mat1(13) * Mat2(2)) + (Mat1(14) * Mat2(6)) + (Mat1(15) * Mat2(10)) + (Mat1(16) * Mat2(14))
     MatR(3) = (Mat1(1) * Mat2(3)) + (Mat1(2) * Mat2(7)) + (Mat1(3) * Mat2(11)) + (Mat1(4) * Mat2(15))
     MatR(7) = (Mat1(5) * Mat2(3)) + (Mat1(6) * Mat2(7)) + (Mat1(7) * Mat2(11)) + (Mat1(8) * Mat2(15))
     MatR(11) = (Mat1(9) * Mat2(3)) + (Mat1(10) * Mat2(7)) + (Mat1(11) * Mat2(11)) + (Mat1(12) * Mat2(15))
     MatR(15) = (Mat1(13) * Mat2(3)) + (Mat1(14) * Mat2(7)) + (Mat1(15) * Mat2(11)) + (Mat1(16) * Mat2(15))
     MatR(4) = (Mat1(1) * Mat2(4)) + (Mat1(2) * Mat2(8)) + (Mat1(3) * Mat2(12)) + (Mat1(4) * Mat2(16))
     MatR(8) = (Mat1(5) * Mat2(4)) + (Mat1(6) * Mat2(8)) + (Mat1(7) * Mat2(12)) + (Mat1(8) * Mat2(16))
     MatR(12) = (Mat1(9) * Mat2(4)) + (Mat1(10) * Mat2(8)) + (Mat1(11) * Mat2(12)) + (Mat1(12) * Mat2(16))
     MatR(16) = (Mat1(13) * Mat2(4)) + (Mat1(14) * Mat2(8)) + (Mat1(15) * Mat2(12)) + (Mat1(16) * Mat2(16))
End Sub







