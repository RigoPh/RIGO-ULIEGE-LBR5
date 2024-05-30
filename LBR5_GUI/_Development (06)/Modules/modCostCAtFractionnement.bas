Attribute VB_Name = "modCostCAtFractionnement"
Option Explicit

'======================
'=== Fractionnement ===
'======================

'PRE-PRE
' OPERATION 1 - Points de depart
'Exp:
' bordés simples, bouchain: k = lisses cadres / lisses bordé
' carlingues: k = 0
Public Function FractOp1BordesSimples(ByRef cPanel As cPanel) As Double
    On Error GoTo FractOp1BordesSimplesErr
    Dim iProfilesVoiles As Integer, iPlatsEnBute As Integer, iStiff As Integer
    iProfilesVoiles = cPanel.cCostCAtMain.ProfilesSurVoiles
    iPlatsEnBute = cPanel.cCostCAtMain.PlatsEnBute
    iStiff = cPanel.cScantlings.cPrimaryStiffeners.GetNoOfStiffeners(cPanel)
    Select Case cPanel.cCostCAtMain.ID_PANNEAU
        Case NappePlane
            Select Case cPanel.cCostCAtMain.IT_PANNEAU
                Case BordeSimple
                    FractOp1BordesSimples = Round(Divide(iProfilesVoiles, iStiff), 3)
            End Select
        Case CarlingueSerreHiloire
            FractOp1BordesSimples = 0
        Case Bouchain
            FractOp1BordesSimples = Round(Divide(iProfilesVoiles, iStiff), 3)
    End Select
    Exit Function
FractOp1BordesSimplesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp1BordesSimples")
End Function

'Exp:
' double coques:
'- somme lisses bordés Inner;
'- somme lisses bordés Outer;
'- k par élément de double coque =
'= (lisses voile / la somme la plus grande des lisses bordés) / somme des panneaux participants
Public Function FractOp1DoubleCoques(ByRef cDoubleHull As cCostCAtDHull, ByVal ProjectIndex As Integer) As Double
    On Error GoTo FractOp1DoubleCoquesErr
    Dim iProfilesVoiles As Integer, iPlatsEnBute As Integer, iStiff As Integer
    iProfilesVoiles = cDoubleHull.ProfilesSurVoiles
    iPlatsEnBute = cDoubleHull.PlatsEnBute
    Dim cPanel As cPanel
    Dim iStiffInner As Integer, iStiffOuter As Integer
    iStiffInner = 0: iStiffOuter = 0
    Dim cInner As cIndex, cOuter As cIndex
    For Each cInner In cDoubleHull.InnerShell
        Set cPanel = Project.Item(ProjectIndex).colPanel.Item(cInner.Number)
        iStiffInner = iStiffInner + cPanel.cScantlings.cPrimaryStiffeners.GetNoOfStiffeners(cPanel)
        Set cPanel = Nothing
    Next cInner
    For Each cOuter In cDoubleHull.OuterShell
        Set cPanel = Project.Item(ProjectIndex).colPanel.Item(cOuter.Number)
        iStiffOuter = iStiffOuter + cPanel.cScantlings.cPrimaryStiffeners.GetNoOfStiffeners(cPanel)
        Set cPanel = Nothing
    Next cOuter
    Select Case iStiffInner
        Case Is > iStiffOuter
            iStiff = iStiffInner
        Case Is <= iStiffOuter
            iStiff = iStiffOuter
    End Select
    Dim iPanels As Integer
    iPanels = cDoubleHull.InnerShell.Count + cDoubleHull.OuterShell.Count
    FractOp1DoubleCoques = Divide(iProfilesVoiles, iStiff)
    FractOp1DoubleCoques = Round(Divide(FractOp1DoubleCoques, iPanels), 3)
    Exit Function
FractOp1DoubleCoquesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp1DoubleCoques")
End Function
' ------------------------------

' OPERATION 2 - Traçage
'Exp:
' bordés simples:
'- k = (lisses cadres + plats en buté) / lisses bordé
' carlingues:
'- si pas de habillage k = 0
'- si habillage sur un coté k =0.5
'- si habillage sur deux cotés k = 1
Public Function FractOp2BordesSimples(ByRef cPanel As cPanel) As Double
    On Error GoTo FractOp2BordesSimplesErr
    Dim iProfilesVoiles As Integer, iPlatsEnBute As Integer, iStiff As Integer
    iProfilesVoiles = cPanel.cCostCAtMain.ProfilesSurVoiles
    iPlatsEnBute = cPanel.cCostCAtMain.PlatsEnBute
    iStiff = cPanel.cScantlings.cPrimaryStiffeners.GetNoOfStiffeners(cPanel)
    
    Select Case cPanel.cCostCAtMain.ID_PANNEAU
        Case NappePlane
            Select Case cPanel.cCostCAtMain.IT_PANNEAU
                Case BordeSimple
                    FractOp2BordesSimples = Round(Divide(iProfilesVoiles + iPlatsEnBute, iStiff), 3)
            End Select
        Case CarlingueSerreHiloire
            Select Case cPanel.cCostCAtMain.HabillageCarlingues
            ' =Habillage_UnCote : = Habillage_DeuxCotes: = Habillage_None
                Case Habillage_None
                    FractOp2BordesSimples = 0
                Case Habillage_UnCote
                    FractOp2BordesSimples = 0.5
                Case Habillage_DeuxCotes
                    FractOp2BordesSimples = 1
            End Select
        Case Bouchain
            FractOp2BordesSimples = Round(Divide(iProfilesVoiles + iPlatsEnBute, iStiff), 3)
    End Select
    Exit Function
FractOp2BordesSimplesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp1BordesSimples")
End Function

'Exp:
' double coques:
'- somme lisses bordés Inner;
'- somme lisses bordés Outer;
'- k par élément de double coque =
'= ((lisses voile + plats en buté) / la somme la plus grande des lisses bordés) / somme des panneaux participants
Public Function FractOp2DoubleCoques(ByRef cDoubleHull As cCostCAtDHull, ByVal ProjectIndex As Integer) As Double
    On Error GoTo FractOp2DoubleCoquesErr
    Dim iProfilesVoiles As Integer, iPlatsEnBute As Integer, iStiff As Integer
    iProfilesVoiles = cDoubleHull.ProfilesSurVoiles
    iPlatsEnBute = cDoubleHull.PlatsEnBute
    Dim cPanel As cPanel
    Dim iStiffInner As Integer, iStiffOuter As Integer
    iStiffInner = 0: iStiffOuter = 0
    Dim cInner As cIndex, cOuter As cIndex
    For Each cInner In cDoubleHull.InnerShell
        Set cPanel = Project.Item(ProjectIndex).colPanel.Item(cInner.Number)
        iStiffInner = iStiffInner + cPanel.cScantlings.cPrimaryStiffeners.GetNoOfStiffeners(cPanel)
        Set cPanel = Nothing
    Next cInner
    For Each cOuter In cDoubleHull.OuterShell
        Set cPanel = Project.Item(ProjectIndex).colPanel.Item(cOuter.Number)
        iStiffOuter = iStiffOuter + cPanel.cScantlings.cPrimaryStiffeners.GetNoOfStiffeners(cPanel)
        Set cPanel = Nothing
    Next cOuter
    Select Case iStiffInner
        Case Is > iStiffOuter
            iStiff = iStiffInner
        Case Is <= iStiffOuter
            iStiff = iStiffOuter
    End Select
    Dim iPanels As Integer
    iPanels = cDoubleHull.InnerShell.Count + cDoubleHull.OuterShell.Count
    FractOp2DoubleCoques = Divide(iProfilesVoiles + iPlatsEnBute, iStiff)
    FractOp2DoubleCoques = Round(Divide(FractOp2DoubleCoques, iPanels), 3)
    Exit Function
FractOp2DoubleCoquesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp2DoubleCoques")
End Function
' -------------------------------------------

' OPERATION 3 - Soudure continue des profilés
'Exp:
' bordés simples:
' soudure discontinue: k = 0
' soudure continue:  k = (lisses cadres + plats en buté) / lisses bordé
Public Function FractOp3BordesSimples(ByRef cPanel As cPanel)
    On Error GoTo FractOp3BordesSimplesErr
    Dim iProfilesVoiles As Integer, iPlatsEnBute As Integer, iStiff As Integer
    iProfilesVoiles = cPanel.cCostCAtMain.ProfilesSurVoiles
    iPlatsEnBute = cPanel.cCostCAtMain.PlatsEnBute
    iStiff = cPanel.cScantlings.cPrimaryStiffeners.GetNoOfStiffeners(cPanel)
    Select Case cPanel.cCostCAtMain.SoudureProfilesSurVoiles
        Case SoudureDiscontinue
            FractOp3BordesSimples = 0
        Case SoudureContinue
            Select Case cPanel.cCostCAtMain.ID_PANNEAU
                Case NappePlane, Bouchain
                    FractOp3BordesSimples = Divide(iProfilesVoiles + iPlatsEnBute, iStiff)
                Case CarlingueSerreHiloire
                    FractOp3BordesSimples = 0
            End Select
    End Select
    Exit Function
FractOp3BordesSimplesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp3BordesSimples")
End Function

'Exp:
' double coques:
'- soudure discontinue: k = 0
'- soudure continue:
'- somme lisses bordés Inner;
'- somme lisses bordés Outer;
'- k par élément de double coque =
'= ((lisses voile + plats en buté) / la somme la plus grande des lisses bordés) / somme des panneaux participants
Public Function FractOp3DoubleCoques(ByRef cDoubleHull As cCostCAtDHull, ByVal ProjectIndex As Integer) As Double
    On Error GoTo FractOp3DoubleCoquesErr
    Dim iProfilesVoiles As Integer, iPlatsEnBute As Integer, iStiff As Integer
    iProfilesVoiles = cDoubleHull.ProfilesSurVoiles
    iPlatsEnBute = cDoubleHull.PlatsEnBute
    Dim cPanel As cPanel
    Dim iStiffInner As Integer, iStiffOuter As Integer
    iStiffInner = 0: iStiffOuter = 0
    Dim cInner As cIndex, cOuter As cIndex
    Select Case cDoubleHull.SoudureProfilesSurVoiles
        Case SoudureDiscontinue
            FractOp3DoubleCoques = 0
        Case SoudureContinue
            For Each cInner In cDoubleHull.InnerShell
                Set cPanel = Project.Item(ProjectIndex).colPanel.Item(cInner.Number)
                iStiffInner = iStiffInner + cPanel.cScantlings.cPrimaryStiffeners.GetNoOfStiffeners(cPanel)
                Set cPanel = Nothing
            Next cInner
            For Each cOuter In cDoubleHull.OuterShell
                Set cPanel = Project.Item(ProjectIndex).colPanel.Item(cOuter.Number)
                iStiffOuter = iStiffOuter + cPanel.cScantlings.cPrimaryStiffeners.GetNoOfStiffeners(cPanel)
                Set cPanel = Nothing
            Next cOuter
            Select Case iStiffInner
                Case Is > iStiffOuter
                    iStiff = iStiffInner
                Case Is <= iStiffOuter
                    iStiff = iStiffOuter
            End Select
            Dim iPanels As Integer
            iPanels = cDoubleHull.InnerShell.Count + cDoubleHull.OuterShell.Count
            FractOp3DoubleCoques = Divide(iProfilesVoiles + iPlatsEnBute, iStiff)
            FractOp3DoubleCoques = Round(Divide(FractOp3DoubleCoques, iPanels), 3)
        End Select
    Exit Function
FractOp3DoubleCoquesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp3DoubleCoques")
End Function

' OPERATION 4 - Soudure continue (h < 1m) - Cadres intermédiaires sur carlingues
' Exp:
' carlingues:
' si dimension < 1:
    '- si pas de habillage k = 0
    '- si habillage sur un coté k =0.5
    '- si habillage sur deux cotés k = 1
' si dimension > 1:
    'k = 0
Public Function FractOp4Carlingues(ByRef cPanel As cPanel) As Double
    On Error GoTo FractOp4CarlinguesErr
    Select Case cPanel.cCostCAtMain.ID_PANNEAU
        Case NappePlane
            FractOp4Carlingues = 0
        Case CarlingueSerreHiloire
            Select Case cPanel.cCostCAtMain.DimensionHabillage
                Case DimensionPlusPetite1
                    Select Case cPanel.cCostCAtMain.HabillageCarlingues
                        Case Habillage_None
                            FractOp4Carlingues = 0
                        Case Habillage_UnCote
                            FractOp4Carlingues = 0.5
                        Case Habillage_DeuxCotes
                            FractOp4Carlingues = 1
                    End Select
                Case DimensionPlusGrande1
                    FractOp4Carlingues = 0
            End Select
        Case Bouchain
            FractOp4Carlingues = 0
    End Select
    Exit Function
FractOp4CarlinguesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp4Carlingues")
End Function

' OPERATION 5 - Soudure continue (h > 1m) - Cadres intermédiaires sur carlingues
'Exp:
' carlingues:
' si dimension < 1:
    'k = 0
' si dimension > 1:
    '- si pas de habillage k = 0
    '- si habillage sur un coté k =0.5
    '- si habillage sur deux cotés k = 1
Public Function FractOp5Carlingues(ByRef cPanel As cPanel) As Double
    On Error GoTo FractOp5CarlinguesErr
    Select Case cPanel.cCostCAtMain.ID_PANNEAU
        Case NappePlane
            FractOp5Carlingues = 0
        Case CarlingueSerreHiloire
            Select Case cPanel.cCostCAtMain.DimensionHabillage
                Case DimensionPlusPetite1
                    FractOp5Carlingues = 0
                Case DimensionPlusGrande1
                    Select Case cPanel.cCostCAtMain.HabillageCarlingues
                        Case Habillage_None
                            FractOp5Carlingues = 0
                        Case Habillage_UnCote
                            FractOp5Carlingues = 0.5
                        Case Habillage_DeuxCotes
                            FractOp5Carlingues = 1
                    End Select
            End Select
        Case Bouchain
            FractOp5Carlingues = 0
    End Select
    Exit Function
FractOp5CarlinguesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp5Carlingues")
End Function

'OPERATION 6 - Accostage tôles (voiles)
' Exp:
' bordés simples: k = nombre d'accostages
Public Function FractOp6BordesSimples(ByRef cPanel As cPanel) As Double
    On Error GoTo FractOp6BordesSimplesErr
    Select Case cPanel.cCostCAtMain.ID_PANNEAU
        Case NappePlane
            FractOp6BordesSimples = cPanel.cCostCAtMain.AccostagesVoiles
        Case CarlingueSerreHiloire
            FractOp6BordesSimples = 0
        Case Bouchain
            FractOp6BordesSimples = cPanel.cCostCAtMain.AccostagesVoiles
    End Select
    Exit Function
FractOp6BordesSimplesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp6BordesSimples")
End Function

' Exp:
' double coques:
' k = nombre d'accostages / nombre de panneaux paticipants
Public Function FractOp6DoubleCoques(ByRef cDoubleHull As cCostCAtDHull, ByVal ProjectIndex As Integer) As Double
    On Error GoTo FractOp6DoubleCoquesErr
    Dim iPanels As Integer
    iPanels = cDoubleHull.InnerShell.Count + cDoubleHull.OuterShell.Count
    FractOp6DoubleCoques = cDoubleHull.AccostagesVoiles
    FractOp6DoubleCoques = Round(Divide(FractOp6DoubleCoques, iPanels), 3)
    Exit Function
FractOp6DoubleCoquesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp6DoubleCoques")
End Function

'OPERATION 7 - Plats en buté
' Exp:
' bordés simples:
' k = nombre de plats en buté
Public Function FractOp7BordesSimples(ByRef cPanel As cPanel) As Double
    On Error GoTo FractOp7BordesSimplesErr
    Select Case cPanel.cCostCAtMain.ID_PANNEAU
        Case NappePlane
            FractOp7BordesSimples = cPanel.cCostCAtMain.PlatsEnBute
        Case CarlingueSerreHiloire
            FractOp7BordesSimples = 0
        Case Bouchain
            FractOp7BordesSimples = cPanel.cCostCAtMain.PlatsEnBute
    End Select
    Exit Function
FractOp7BordesSimplesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp7BordesSimples")
End Function

'Exp:
' double coques:
' k = nombre de plats en buté / nombre de panneaux paticipants
Public Function FractOp7DoubleCoques(ByRef cDoubleHull As cCostCAtDHull, ByVal ProjectIndex As Integer) As Double
    On Error GoTo FractOp7DoubleCoquesErr
    Dim iPanels As Integer
    iPanels = cDoubleHull.InnerShell.Count + cDoubleHull.OuterShell.Count
    FractOp7DoubleCoques = cDoubleHull.PlatsEnBute
    FractOp7DoubleCoques = Round(Divide(FractOp7DoubleCoques, iPanels), 3)
    Exit Function
FractOp7DoubleCoquesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp7DoubleCoques")
End Function

'OPERATION 8 - Goussets sur semelle de profilé
' Exp:
' bordés simples:
' k = nombre goussets / nombre lisses bordé
Public Function FractOp8BordesSimples(ByRef cPanel As cPanel) As Double
    On Error GoTo FractOp8BordesSimplesErr
    Dim iProfilesVoiles As Integer, iStiff As Integer, iGoussets As Integer
    iStiff = cPanel.cScantlings.cPrimaryStiffeners.GetNoOfStiffeners(cPanel)
    iGoussets = cPanel.cCostCAtMain.GoussetsProfilesVoiles
    Select Case cPanel.cCostCAtMain.ID_PANNEAU
        Case NappePlane
            Select Case cPanel.cCostCAtMain.IT_PANNEAU
                Case BordeSimple
                    FractOp8BordesSimples = Round(Divide(iGoussets, iStiff), 3)
            End Select
        Case CarlingueSerreHiloire
            FractOp8BordesSimples = 0
        Case Bouchain
            FractOp8BordesSimples = Round(Divide(iGoussets, iStiff), 3)
    End Select
    Exit Function
FractOp8BordesSimplesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp8BordesSimples")
End Function

' Exp:
' double coques:
' - lisses bordé: somme lisses inner + somme lisses outer
' k = nombre goussets / lisses bordé / nombre panneaux paticipants
Public Function FractOp8DoubleCoques(ByRef cDoubleHull As cCostCAtDHull, ByVal ProjectIndex As Integer) As Double
    On Error GoTo FractOp8DoubleCoquesErr
    Dim iStiff As Integer
    Dim cPanel As cPanel
    Dim iStiffInner As Integer, iStiffOuter As Integer
    Dim iGoussets As Integer
    iGoussets = cDoubleHull.GoussetsProfilesVoiles
    iStiffInner = 0: iStiffOuter = 0
    Dim cInner As cIndex, cOuter As cIndex
    For Each cInner In cDoubleHull.InnerShell
        Set cPanel = Project.Item(ProjectIndex).colPanel.Item(cInner.Number)
        iStiffInner = iStiffInner + cPanel.cScantlings.cPrimaryStiffeners.GetNoOfStiffeners(cPanel)
        Set cPanel = Nothing
    Next cInner
    For Each cOuter In cDoubleHull.OuterShell
        Set cPanel = Project.Item(ProjectIndex).colPanel.Item(cOuter.Number)
        iStiffOuter = iStiffOuter + cPanel.cScantlings.cPrimaryStiffeners.GetNoOfStiffeners(cPanel)
        Set cPanel = Nothing
    Next cOuter
    iStiff = iStiffInner + iStiffOuter
    Dim iPanels As Integer
    iPanels = cDoubleHull.InnerShell.Count + cDoubleHull.OuterShell.Count
    FractOp8DoubleCoques = Divide(iGoussets, iStiff)
    FractOp8DoubleCoques = Round(Divide(FractOp8DoubleCoques, iPanels), 3)
    Exit Function
FractOp8DoubleCoquesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp8DoubleCoques")
End Function

'OPERATION 9 - Soudure des tôles accostées (formation des voiles)
'---> mêmes fractionnements que pour l'opération 6

'OPERATION 10 - Soudure discontinue des profilés
'Exp:
' bordés simples:
' soudure discontinue: k = (lisses cadres + plats en buté) / lisses bordé
' soudure continue: k = 0
Public Function FractOp10BordesSimples(ByRef cPanel As cPanel)
    On Error GoTo FractOp10BordesSimplesErr
    Dim iProfilesVoiles As Integer, iPlatsEnBute As Integer, iStiff As Integer
    iProfilesVoiles = cPanel.cCostCAtMain.ProfilesSurVoiles
    iPlatsEnBute = cPanel.cCostCAtMain.PlatsEnBute
    iStiff = cPanel.cScantlings.cPrimaryStiffeners.GetNoOfStiffeners(cPanel)
    Select Case cPanel.cCostCAtMain.SoudureProfilesSurVoiles
        Case SoudureDiscontinue
            Select Case cPanel.cCostCAtMain.ID_PANNEAU
                Case NappePlane, Bouchain
                    FractOp10BordesSimples = Divide(iProfilesVoiles + iPlatsEnBute, iStiff)
                Case CarlingueSerreHiloire
                    FractOp10BordesSimples = 0
            End Select
        Case SoudureContinue
            FractOp10BordesSimples = 0
    End Select
    Exit Function
FractOp10BordesSimplesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp10BordesSimples")
End Function

'Exp:
' double coques:
'- soudure continue: k = 0
'- soudure discontinue:
'- somme lisses bordés Inner;
'- somme lisses bordés Outer;
'- k par élément de double coque =
'= ((lisses voile + plats en buté) / la somme la plus grande des lisses bordés) / somme des panneaux participants
Public Function FractOp10DoubleCoques(ByRef cDoubleHull As cCostCAtDHull, ByVal ProjectIndex As Integer) As Double
    On Error GoTo FractOp10DoubleCoquesErr
    Dim iProfilesVoiles As Integer, iPlatsEnBute As Integer, iStiff As Integer
    iProfilesVoiles = cDoubleHull.ProfilesSurVoiles
    iPlatsEnBute = cDoubleHull.PlatsEnBute
    Dim cPanel As cPanel
    Dim iStiffInner As Integer, iStiffOuter As Integer
    iStiffInner = 0: iStiffOuter = 0
    Dim cInner As cIndex, cOuter As cIndex
    Select Case cDoubleHull.SoudureProfilesSurVoiles
        Case SoudureDiscontinue
            For Each cInner In cDoubleHull.InnerShell
                Set cPanel = Project.Item(ProjectIndex).colPanel.Item(cInner.Number)
                iStiffInner = iStiffInner + cPanel.cScantlings.cPrimaryStiffeners.GetNoOfStiffeners(cPanel)
                Set cPanel = Nothing
            Next cInner
            For Each cOuter In cDoubleHull.OuterShell
                Set cPanel = Project.Item(ProjectIndex).colPanel.Item(cOuter.Number)
                iStiffOuter = iStiffOuter + cPanel.cScantlings.cPrimaryStiffeners.GetNoOfStiffeners(cPanel)
                Set cPanel = Nothing
            Next cOuter
            Select Case iStiffInner
                Case Is > iStiffOuter
                    iStiff = iStiffInner
                Case Is <= iStiffOuter
                    iStiff = iStiffOuter
            End Select
            Dim iPanels As Integer
            iPanels = cDoubleHull.InnerShell.Count + cDoubleHull.OuterShell.Count
            FractOp10DoubleCoques = Divide(iProfilesVoiles + iPlatsEnBute, iStiff)
            FractOp10DoubleCoques = Round(Divide(FractOp10DoubleCoques, iPanels), 3)
        Case SoudureContinue
            FractOp10DoubleCoques = 0
    End Select
    Exit Function
FractOp10DoubleCoquesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp10DoubleCoques")
End Function

'FABRICATION DE LA NAPPE PLANE (PRE)
'OPERATION 11 - Accostages tôles
' Exp:
' Nappes simples: k = nombre accostages x 2
Public Function FractOp11NappesSimples(ByRef cPanel As cPanel) As Double
    On Error GoTo FractOp11NappesSimplesErr
    Dim iAccostagesNappes As Integer
    Select Case cPanel.pType
        Case Plate
            Select Case cPanel.cCostCAtMain.ID_PANNEAU
                Case NappePlane
                    FractOp11NappesSimples = cPanel.cCostCAtMain.AccostagesNappes * 2
            End Select
    End Select
    Exit Function
FractOp11NappesSimplesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp11NappesSimples")
End Function

' Exp:
' Nappes composées : k = nombre accostages / (nombre panneaux participants - 1)
Public Function FractOp11NappesComposees(ByRef cNappe As cCostCAtNappe, ProjectIndex As Integer) As Double
    On Error GoTo FractOp11NappesComposeesErr
    Dim iPanels As Integer
    Dim Panneau_de_Nappe As cIndex
    iPanels = cNappe.Nappe.Count
    FractOp11NappesComposees = Divide(cNappe.AccostagesNappes, (iPanels - 1))
    Exit Function
FractOp11NappesComposeesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp11NappesComposees")
End Function

'OPERATION 12 - Réglage des joints
'---> mêmes fractionnements que pour l'opération 11

'OPERATION 13 - Réglage des abouts
'---> mêmes fractionnements que pour l'opération 11

'OPERATION 14 - Traçage
' Exp:
' nappes simples: k = 1
Public Function FractOp14NappesSimples(ByRef cPanel As cPanel) As Double
    On Error GoTo FractOp14NappesSimplesErr
    Dim iAccostagesNappes As Integer
    Select Case cPanel.pType
        Case Plate
            Select Case cPanel.cCostCAtMain.ID_PANNEAU
                Case NappePlane
                    FractOp14NappesSimples = 1
            End Select
    End Select
    Exit Function
FractOp14NappesSimplesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp14NappesSimples")
End Function

' Exp:
' nappes composées: k = 1
Public Function FractOp14NappesComposees(ByRef cNappe As cCostCAtNappe, ProjectIndex As Integer) As Double
    On Error GoTo FractOp14NappesComposeesErr
    FractOp14NappesComposees = 1
    Exit Function
FractOp14NappesComposeesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp14NappesComposees")
End Function

'OPERATION 15 - Oxycoupage
'---> mêmes fractionnements que pour l'opération 14

'OPERATION 16 - Réglage profilés
'---> mêmes fractionnements que pour l'opération 14

'OPERATION 17 - Soudure des joints
'---> mêmes fractionnements que pour l'opération 11

'OPERATION 18 - Soudure des abouts
'---> mêmes fractionnements que pour l'opération 11

'OPERATION 19 - Soudure continue des profilés
'Exp:
' nappes simples:
'   si soudure discontinue k = 0
'   si soudure continue k = 1
Public Function FractOp19NappesSimples(ByRef cPanel As cPanel) As Double
    On Error GoTo FractOp19NappesSimplesErr
    Dim iAccostagesNappes As Integer
    Select Case cPanel.pType
        Case Plate
            Select Case cPanel.cCostCAtMain.ID_PANNEAU
                Case NappePlane
                    Select Case cPanel.cCostCAtMain.SoudureLissesNappes
                        Case SoudureContinue
                            FractOp19NappesSimples = 1
                        Case SoudureDiscontinue
                            FractOp19NappesSimples = 0
                    End Select
            End Select
    End Select
    Exit Function
FractOp19NappesSimplesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp19NappesSimples")
End Function

'Exp:
' nappes composées:
'   si soudure discontinue k = 0
'   si soudure continue k = 1
Public Function FractOp19NappesComposees(ByRef cNappe As cCostCAtNappe, ProjectIndex As Integer) As Double
    On Error GoTo FractOp19NappesComposeesErr
    Select Case cNappe.SoudureLissesNappes
        Case SoudureContinue
            FractOp19NappesComposees = 1
        Case SoudureDiscontinue
            FractOp19NappesComposees = 0
    End Select
    Exit Function
FractOp19NappesComposeesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp19NappesComposees")
End Function

'OPERATION 20 - Soudure discontinue des profilés
'Exp:
' nappes simples:
'   si soudure discontinue k = 1
'   si soudure continue k = 0
Public Function FractOp20NappesSimples(ByRef cPanel As cPanel) As Double
    On Error GoTo FractOp20NappesSimplesErr
    Dim iAccostagesNappes As Integer
    Select Case cPanel.pType
        Case Plate
            Select Case cPanel.cCostCAtMain.ID_PANNEAU
                Case NappePlane
                    Select Case cPanel.cCostCAtMain.SoudureLissesNappes
                        Case SoudureContinue
                            FractOp20NappesSimples = 0
                        Case SoudureDiscontinue
                            FractOp20NappesSimples = 1
                    End Select
            End Select
    End Select
    Exit Function
FractOp20NappesSimplesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp20NappesSimples")
End Function

'Exp:
' nappes composées:
'   si soudure discontinue k = 1
'   si soudure continue k = 0
Public Function FractOp20NappesComposees(ByRef cNappe As cCostCAtNappe, ProjectIndex As Integer) As Double
    On Error GoTo FractOp20NappesComposeesErr
    Select Case cNappe.SoudureLissesNappes
        Case SoudureContinue
            FractOp20NappesComposees = 0
        Case SoudureDiscontinue
            FractOp20NappesComposees = 1
    End Select
    Exit Function
FractOp20NappesComposeesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp20NappesComposees")
End Function

'ASSEMBLAGE (PRE) - DOUBLE COQUE
'OPERATION 21 - Réglage et soudure des voiles de pré-pré sur la nappe de double coque
'Exp: k = 0
Public Function FractOp21BordesSimples(ByRef cPanel As cPanel) As Double
    On Error GoTo FractOp21BordesSimplesErr
        FractOp21BordesSimples = 0
    Exit Function
FractOp21BordesSimplesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp21BordesSimples")
End Function

' Rem: op. 21 pour double coques - traité dans frmCostCAt: Sub ComputePreAssemblage
' Exp:
' double coques Inner: k = 1
' double coques Outer: k = 0

'OPERATION 22 - Réglage et soudure des carlingues sur la nappe de double coque
' Exp:
' carlingues: k = 1
Public Function FractOp22Carlingues(ByRef cPanel As cPanel) As Double
    On Error GoTo FractOp22CarlinguesErr
    Select Case cPanel.cCostCAtMain.ID_PANNEAU
        Case CarlingueSerreHiloire
            FractOp22Carlingues = 1
        Case Else
            FractOp22Carlingues = 0
        End Select
    Exit Function
FractOp22CarlinguesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp22Carlingues")
End Function

'OPERATION 23 - Réglage et soudure des voiles sur carlingues
' Exp:
' carlingues: k = 1
Public Function FractOp23Carlingues(ByRef cPanel As cPanel) As Double
    On Error GoTo FractOp23CarlinguesErr
    Select Case cPanel.cCostCAtMain.ID_PANNEAU
        Case CarlingueSerreHiloire
            FractOp23Carlingues = 1
        Case Else
            FractOp23Carlingues = 0
        End Select
    Exit Function
FractOp23CarlinguesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp23Carlingues")
End Function

'OPERATION 24 - Entailles nappe de double coque (Soudure entre voiles et lisses)
' (Entailles nappe de bordé - operation distincte)
' Exp:
' bordés simples: k = 1
Public Function FractOp24BordesSimples(ByRef cPanel As cPanel) As Double
    On Error GoTo FractOp24BordesSimplesErr
        Select Case cPanel.pType
            Case Plate, DoubleHull
                Select Case cPanel.cCostCAtMain.ID_PANNEAU
                    Case NappePlane
                        Select Case cPanel.cCostCAtMain.IT_PANNEAU
                            Case BordeSimple
                                FractOp24BordesSimples = 1
                            Case Else
                                FractOp24BordesSimples = 0
                        End Select
                    Case Else
                        FractOp24BordesSimples = 0
                End Select
        End Select
    Exit Function
FractOp24BordesSimplesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp24BordesSimples")
End Function

' Rem: op. 24 pour double coques - traité dans frmCostCAt: Sub ComputePreAssemblage
' Exp:
' double coques Inner: k = 1
' double coques Outer: k = 0

'OPERATION 25 - Mise en place + soudure des tapes non-etanches sur la nappe de double coque
'(profilés)
' Exp:
' bordés simples, double coques Inner:
' si tapes non-etanches, profilés: k = 1
Public Function FractOp25BordesSimples(ByRef cPanel As cPanel) As Double
    On Error GoTo FractOp25BordesSimplesErr
    Select Case cPanel.cCostCAtMain.ID_PANNEAU
        Case NappePlane
            Select Case cPanel.cCostCAtMain.IT_PANNEAU
                Case BordeSimple, DoubleCoqueInterieure
                    Select Case cPanel.cCostCAtMain.SectionTapes '= Profiles: = TSynthetiques
                        Case Profiles
                            Select Case cPanel.cCostCAtMain.TypeTapes
                                Case NonEtanchesRecouvrement
                                    FractOp25BordesSimples = 1
                                Case NonEtanchesEncastrees
                                    FractOp25BordesSimples = 1
                                Case EtanchesRecouvrement
                                    FractOp25BordesSimples = 0
                                Case EtanchesEncastrees
                                    FractOp25BordesSimples = 0
                            End Select
                        Case TSynthetiques
                            FractOp25BordesSimples = 0
                    End Select
                Case Else
                    FractOp25BordesSimples = 0
            End Select
        Case Else
            FractOp25BordesSimples = 0
        End Select
    Exit Function
FractOp25BordesSimplesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp25BordesSimples")
End Function

'OPERATION 26 - Mise en place + soudure des tapes etanches sur la nappe de double coque
'(profilés)
' Exp:
' bordés simples, double coques Inner:
' si tapes etanches, profilés: k = 1
Public Function FractOp26BordesSimples(ByRef cPanel As cPanel) As Double
    On Error GoTo FractOp26BordesSimplesErr
    Select Case cPanel.cCostCAtMain.ID_PANNEAU
        Case NappePlane
            Select Case cPanel.cCostCAtMain.IT_PANNEAU
                Case BordeSimple, DoubleCoqueInterieure
                    Select Case cPanel.cCostCAtMain.SectionTapes
                        Case Profiles
                            Select Case cPanel.cCostCAtMain.TypeTapes
                                Case NonEtanchesRecouvrement
                                    FractOp26BordesSimples = 0
                                Case NonEtanchesEncastrees
                                    FractOp26BordesSimples = 0
                                Case EtanchesRecouvrement
                                    FractOp26BordesSimples = 1
                                Case EtanchesEncastrees
                                    FractOp26BordesSimples = 1
                            End Select
                        Case TSynthetiques
                            FractOp26BordesSimples = 0
                    End Select
                Case Else
                    FractOp26BordesSimples = 0
            End Select
        Case Else
            FractOp26BordesSimples = 0
        End Select
    Exit Function
FractOp26BordesSimplesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp26BordesSimples")
End Function

'OPERATION 27  - Réglage + soudure des raidisseurs de voiles (pré-pré) sur lisses de la
'nappe de double coque
' Exp:
' bordés simples: k = nombre lisses voiles / nombre lisses bordés
Public Function FractOp27BordesSimples(ByRef cPanel As cPanel) As Double
    On Error GoTo FractOp27BordesSimplesErr
    Dim iProfilesVoiles As Integer, iStiff As Integer
    iProfilesVoiles = cPanel.cCostCAtMain.ProfilesSurVoiles
    iStiff = cPanel.cScantlings.cPrimaryStiffeners.GetNoOfStiffeners(cPanel)
    Select Case cPanel.cCostCAtMain.ID_PANNEAU
        Case NappePlane
            Select Case cPanel.cCostCAtMain.IT_PANNEAU
                Case BordeSimple
                    FractOp27BordesSimples = Round(Divide(iProfilesVoiles, iStiff), 3)
            End Select
        Case CarlingueSerreHiloire
            FractOp27BordesSimples = 0
        Case Bouchain
            FractOp27BordesSimples = 0
    End Select
    Exit Function
FractOp27BordesSimplesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp27BordesSimples")
End Function

' Exp:
' double coques Inner
' - somme lisses bordés Inner
' - somme lisses bordés Outer
' si nombre lisses bordés inner > nombre lisses bordés outer
'   k = lisses voiles / lisses bordés inner / nombre panneaux inner participants
' autrement
'   lisses voiles aproximées = lisses bordé inner x lisses voiles / lisses bordé outer
'   k = lisses voiles aproximées / lisses bordés inner / nombre panneaux inner participants
Public Function FractOp27DoubleCoques(ByRef cDoubleHull As cCostCAtDHull, ByVal ProjectIndex As Integer) As Double
    On Error GoTo FractOp27DoubleCoquesErr
    ' only inner hulls
    Dim iProfilesVoiles As Integer, iStiff As Integer
    iProfilesVoiles = cDoubleHull.ProfilesSurVoiles
    Dim cPanel As cPanel
    Dim iStiffInner As Integer, iStiffOuter As Integer
    iStiffInner = 0: iStiffOuter = 0
    Dim cInner As cIndex, cOuter As cIndex
    For Each cInner In cDoubleHull.InnerShell
        Set cPanel = Project.Item(ProjectIndex).colPanel.Item(cInner.Number)
        iStiffInner = iStiffInner + cPanel.cScantlings.cPrimaryStiffeners.GetNoOfStiffeners(cPanel)
        Set cPanel = Nothing
    Next cInner
    For Each cOuter In cDoubleHull.OuterShell
        Set cPanel = Project.Item(ProjectIndex).colPanel.Item(cOuter.Number)
        iStiffOuter = iStiffOuter + cPanel.cScantlings.cPrimaryStiffeners.GetNoOfStiffeners(cPanel)
        Set cPanel = Nothing
    Next cOuter
    Dim iPanels As Integer, iRaid As Integer
    iPanels = cDoubleHull.InnerShell.Count
    Select Case iStiffInner
        Case Is > iStiffOuter
            FractOp27DoubleCoques = Divide(iProfilesVoiles, iStiffInner)
            FractOp27DoubleCoques = Round(Divide(FractOp27DoubleCoques, iPanels), 3)
        Case Is <= iStiffOuter
            'iRaid = iProfilesVoiles - (iStiffOuter - iStiffInner)
            iRaid = iStiffInner * iProfilesVoiles / iStiffOuter
            FractOp27DoubleCoques = Divide(iRaid, iStiffInner)
            FractOp27DoubleCoques = Round(Divide(FractOp27DoubleCoques, iPanels), 3)
    End Select
    If FractOp27DoubleCoques < 0 Then FractOp27DoubleCoques = 0
    Exit Function
FractOp27DoubleCoquesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp27DoubleCoques")
End Function

'OPERATION 28 - Réglage + soudure des contacts verticaux de double coque
' Exp:
' bordés simples, double coques Inner:
' k = nombre couples intermediaires des carlingues sur nappes
Public Function FractOp28BordesSimples(ByRef cPanel As cPanel) As Double
    On Error GoTo FractOp28BordesSimplesErr
        Select Case cPanel.pType
            Case Plate, DoubleHull
                Select Case cPanel.cCostCAtMain.ID_PANNEAU
                    Case NappePlane
                        Select Case cPanel.cCostCAtMain.IT_PANNEAU
                            Case BordeSimple, DoubleCoqueInterieure
                                FractOp28BordesSimples = cPanel.cCostCAtMain.CouplesCarlinguesSurNappes
                            Case Else
                                FractOp28BordesSimples = 0
                        End Select
                    Case Else
                        FractOp28BordesSimples = 0
                End Select
        End Select
    Exit Function
FractOp28BordesSimplesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp28BordesSimples")
End Function

'OPERATION 29 - Réglages + soudure des couples intermediaires sur double coques
'---> mêmes fractionnements que pour l'opération 28

'OPERATION 30 - Mise en place + soudure des tapes non-etanches sur la nappe de double coque
'(T Synthétiques)
' Exp:
' bordés simples, double coques Inner:
' si tapes non-etanches, T Synthétiques: k = 1
Public Function FractOp30BordesSimples(ByRef cPanel As cPanel) As Double
    On Error GoTo FractOp30BordesSimplesErr
    Select Case cPanel.cCostCAtMain.ID_PANNEAU
        Case NappePlane
            Select Case cPanel.cCostCAtMain.IT_PANNEAU
                Case BordeSimple, DoubleCoqueInterieure
                    Select Case cPanel.cCostCAtMain.SectionTapes '= Profiles: = TSynthetiques
                        Case TSynthetiques
                            Select Case cPanel.cCostCAtMain.TypeTapes
                                Case NonEtanchesRecouvrement
                                    FractOp30BordesSimples = 1
                                Case NonEtanchesEncastrees
                                    FractOp30BordesSimples = 1
                                Case EtanchesRecouvrement
                                    FractOp30BordesSimples = 0
                                Case EtanchesEncastrees
                                    FractOp30BordesSimples = 0
                            End Select
                        Case Profiles
                            FractOp30BordesSimples = 0
                    End Select
                Case Else
                    FractOp30BordesSimples = 0
            End Select
        Case Else
            FractOp30BordesSimples = 0
        End Select
    Exit Function
FractOp30BordesSimplesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp30BordesSimples")
End Function

'OPERATION 31 - Mise en place + soudure des tapes etanches sur la nappe de double coque
'(T Synthétiques)
' Exp:
' bordés simples, double coques Inner:
' si tapes etanches, T synthétiques: k = 1
Public Function FractOp31BordesSimples(ByRef cPanel As cPanel) As Double
    On Error GoTo FractOp31BordesSimplesErr
    Select Case cPanel.cCostCAtMain.ID_PANNEAU
        Case NappePlane
            Select Case cPanel.cCostCAtMain.IT_PANNEAU
                Case BordeSimple, DoubleCoqueInterieure
                    Select Case cPanel.cCostCAtMain.SectionTapes
                        Case TSynthetiques
                            Select Case cPanel.cCostCAtMain.TypeTapes
                                Case NonEtanchesRecouvrement
                                    FractOp31BordesSimples = 0
                                Case NonEtanchesEncastrees
                                    FractOp31BordesSimples = 0
                                Case EtanchesRecouvrement
                                    FractOp31BordesSimples = 1
                                Case EtanchesEncastrees
                                    FractOp31BordesSimples = 1
                            End Select
                        Case Profiles
                            FractOp31BordesSimples = 0
                    End Select
                Case Else
                    FractOp31BordesSimples = 0
            End Select
        Case Else
            FractOp31BordesSimples = 0
        End Select
    Exit Function
FractOp31BordesSimplesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp31BordesSimples")
End Function

'ASSEMBLAGE (PRE) - BOUCHAIN
'OPERATION 32 - Embarquement des lisses individuelles dans les entailles du bordé
' Exp:
' bouchains: k = 1
Public Function FractOp32Bouchain(ByRef cPanel As cPanel) As Double
    On Error GoTo FractOp32BouchainErr
        Select Case cPanel.pType
            Case Plate, DoubleHull
                Select Case cPanel.cCostCAtMain.ID_PANNEAU
                    Case Bouchain
                        FractOp32Bouchain = 1
                    Case Else
                        FractOp32Bouchain = 0
                End Select
        End Select
    Exit Function
FractOp32BouchainErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp32Bouchain")
End Function

'OPERATION 33 - Accostage des tôles individuelles de bordé
' Exp:
' bouchains: k = nombre accostages tôles bouchain + 1 (accostage tôle bouchain avec tôle double fond outer automatiquement pris en compte)
Public Function FractOp33Bouchain(ByRef cPanel As cPanel) As Double
    On Error GoTo FractOp33BouchainErr
        Select Case cPanel.pType
            Case Plate, DoubleHull
                Select Case cPanel.cCostCAtMain.ID_PANNEAU
                    Case Bouchain
                        FractOp33Bouchain = cPanel.cCostCAtMain.AccostagesToleBouchain + 1
                        ' (1 --> accostage dans les extremités du bouchain 0.5 + 0.5)
                    Case Else
                        FractOp33Bouchain = 0
                End Select
        End Select
    Exit Function
FractOp33BouchainErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp33Bouchain")
End Function

'OPERATION 34 - Soudage des lisses individuelles sur bordé
' Exp:
' bouchain: k = 1
Public Function FractOp34Bouchain(ByRef cPanel As cPanel) As Double
    On Error GoTo FractOp34BouchainErr
        Select Case cPanel.pType
            Case Plate, DoubleHull
                Select Case cPanel.cCostCAtMain.ID_PANNEAU
                    Case Bouchain
                        FractOp34Bouchain = 1
                    Case Else
                        FractOp34Bouchain = 0
                End Select
        End Select
    Exit Function
FractOp34BouchainErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp34Bouchain")
End Function

'ASSEMBLAGE (PRE) - BORDE
'OPERATION 35 - Accostage des indices
' Exp:
' double coques (inner & outer): k = nombre accostages indices / nombre total de panneaux participants de tous les éléments double coque
Public Function FractOp35DoubleCoques(ByRef colCostCAtDHull As Collection, ByRef cHeader As cHeader) As Double
    On Error GoTo FractOp35DoubleCoquesErr
    Dim i As Integer
    Dim iIndices As Integer
    iIndices = cHeader.cCostCAtMain.NoIndicesDCoque
    Select Case iIndices
        Case Is = 0
            If colCostCAtDHull.Count > 0 Then
                iIndices = 1
                cHeader.cCostCAtMain.NoIndicesDCoque = 1
            End If
    End Select
    Dim cDoubleHull As cCostCAtDHull, iDHPanels As Integer
    iDHPanels = 0
    For i = 1 To colCostCAtDHull.Count
    'For Each cDoubleHull In colCostCAtDHull
        iDHPanels = iDHPanels + colCostCAtDHull.Item(i).InnerShell.Count + colCostCAtDHull.Item(i).OuterShell.Count
    'Next cDoubleHull
    Next i
    FractOp35DoubleCoques = Divide(iIndices, iDHPanels)
    Exit Function
FractOp35DoubleCoquesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp35DoubleCoques")
End Function

'OPERATION 36 - Réglage et soudure des voiles de pré-pré sur la nappe de bordé
' Exp:
' bouchain: k = 1
Public Function FractOp36BordesSimples(ByRef cPanel As cPanel) As Double
    On Error GoTo FractOp36BordesSimplesErr
    Select Case cPanel.cCostCAtMain.ID_PANNEAU
        Case Bouchain
            FractOp36BordesSimples = 1
        Case Else
            FractOp36BordesSimples = 0
    End Select
    Exit Function
FractOp36BordesSimplesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp36BordesSimples")
End Function
' Rem: op. 36 pour double coques - traité dans frmCostCAt: Sub ComputePreAssemblage
' Exp:
' double coques inner: k = 0
' double coques outer: k = 1

'OPERATION 37 - Réglage et soudure des carlingues sur les nappes de bordé
' même que l'operation 22

'OPERATION 38 - Entailles au bordé
' Exp:
' bouchain: k = 1
Public Function FractOp38BordesSimples(ByRef cPanel As cPanel) As Double
    On Error GoTo FractOp38BordesSimplesErr
    Select Case cPanel.cCostCAtMain.ID_PANNEAU
        Case Bouchain
            FractOp38BordesSimples = 1
        Case Else
            FractOp38BordesSimples = 0
    End Select
    Exit Function
FractOp38BordesSimplesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp38BordesSimples")
End Function

'OPERATION 39 - Mise en place + soudure des tapes non-etanches sur la nappe de bordé
'(profilés)
' Exp:
' Double coques exterieures, bouchain:
' si tapes non-etanches, profilés: k = 1
Public Function FractOp39BordesSimples(ByRef cPanel As cPanel) As Double
    On Error GoTo FractOp39BordesSimplesErr
    Select Case cPanel.cCostCAtMain.ID_PANNEAU
        Case NappePlane
            Select Case cPanel.cCostCAtMain.IT_PANNEAU
                Case DoubleCoqueExterieure
                    Select Case cPanel.cCostCAtMain.SectionTapes '= Profiles: = TSynthetiques
                        Case Profiles
                            Select Case cPanel.cCostCAtMain.TypeTapes
                                Case NonEtanchesRecouvrement
                                    FractOp39BordesSimples = 1
                                Case NonEtanchesEncastrees
                                    FractOp39BordesSimples = 1
                                Case EtanchesRecouvrement
                                    FractOp39BordesSimples = 0
                                Case EtanchesEncastrees
                                    FractOp39BordesSimples = 0
                            End Select
                        Case TSynthetiques
                            FractOp39BordesSimples = 0
                    End Select
                Case Else
                    FractOp39BordesSimples = 0
            End Select
        Case Bouchain
            Select Case cPanel.cCostCAtMain.TypeTapes
                Case NonEtanchesRecouvrement
                    FractOp39BordesSimples = 1
                Case NonEtanchesEncastrees
                    FractOp39BordesSimples = 1
                Case EtanchesRecouvrement
                    FractOp39BordesSimples = 0
                Case EtanchesEncastrees
                    FractOp39BordesSimples = 0
            End Select
        Case Else
            FractOp39BordesSimples = 0
        End Select
    Exit Function
FractOp39BordesSimplesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp39BordesSimples")
End Function

'OPERATION 40 - Mise en place + soudure des tapes etanches sur la nappe de bordé
'(profilés)
' Exp:
' Double coques exterieures, bouchain:
' si tapes etanches, profilés: k = 1
Public Function FractOp40BordesSimples(ByRef cPanel As cPanel) As Double
    On Error GoTo FractOp40BordesSimplesErr
    Select Case cPanel.cCostCAtMain.ID_PANNEAU
        Case NappePlane
            Select Case cPanel.cCostCAtMain.IT_PANNEAU
                Case DoubleCoqueExterieure
                    Select Case cPanel.cCostCAtMain.SectionTapes
                        Case Profiles
                            Select Case cPanel.cCostCAtMain.TypeTapes
                                Case NonEtanchesRecouvrement
                                    FractOp40BordesSimples = 0
                                Case NonEtanchesEncastrees
                                    FractOp40BordesSimples = 0
                                Case EtanchesRecouvrement
                                    FractOp40BordesSimples = 1
                                Case EtanchesEncastrees
                                    FractOp40BordesSimples = 1
                            End Select
                        Case TSynthetiques
                            FractOp40BordesSimples = 0
                    End Select
                Case Else
                    FractOp40BordesSimples = 0
            End Select
        Case Bouchain
            Select Case cPanel.cCostCAtMain.TypeTapes
                Case NonEtanchesRecouvrement
                    FractOp40BordesSimples = 0
                Case NonEtanchesEncastrees
                    FractOp40BordesSimples = 0
                Case EtanchesRecouvrement
                    FractOp40BordesSimples = 1
                Case EtanchesEncastrees
                    FractOp40BordesSimples = 1
            End Select
        Case Else
            FractOp40BordesSimples = 0
        End Select
    Exit Function
FractOp40BordesSimplesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp40BordesSimples")
End Function

'OPERATION 41 - Réglage + soudure des raidisseurs de voiles (pré-pré) sur lisses de la
'double coque extérieure (OUTER)
' Exp:
' double coques Outer
' - somme lisses bordés Inner
' - somme lisses bordés Outer
' si nombre lisses bordés outer > nombre lisses bordés inner
'   k = lisses voiles / lisses bordés outer / nombre panneaux outer participants
' autrement
'   lisses voiles aproximées = lisses bordé outer x lisses voiles / lisses bordé inner
'   k = lisses voiles aproximées / lisses bordés inner / nombre panneaux outer participants
Public Function FractOp41DoubleCoques(ByRef cDoubleHull As cCostCAtDHull, ByVal ProjectIndex As Integer) As Double
    On Error GoTo FractOp41DoubleCoquesErr
    ' only outer hulls
    Dim iProfilesVoiles As Integer, iStiff As Integer
    iProfilesVoiles = cDoubleHull.ProfilesSurVoiles
    Dim cPanel As cPanel
    Dim iStiffInner As Integer, iStiffOuter As Integer
    iStiffInner = 0: iStiffOuter = 0
    Dim cInner As cIndex, cOuter As cIndex
    For Each cInner In cDoubleHull.InnerShell
        Set cPanel = Project.Item(ProjectIndex).colPanel.Item(cInner.Number)
        iStiffInner = iStiffInner + cPanel.cScantlings.cPrimaryStiffeners.GetNoOfStiffeners(cPanel)
        Set cPanel = Nothing
    Next cInner
    For Each cOuter In cDoubleHull.OuterShell
        Set cPanel = Project.Item(ProjectIndex).colPanel.Item(cOuter.Number)
        iStiffOuter = iStiffOuter + cPanel.cScantlings.cPrimaryStiffeners.GetNoOfStiffeners(cPanel)
        Set cPanel = Nothing
    Next cOuter
    Dim iPanels As Integer, iRaid As Integer
    iPanels = cDoubleHull.OuterShell.Count
    Select Case iStiffInner
        Case Is > iStiffOuter
            'iRaid = iProfilesVoiles - (iStiffInner - iStiffOuter)
            iRaid = iStiffOuter * iProfilesVoiles / iStiffInner
            FractOp41DoubleCoques = Divide(iRaid, iStiffOuter)
            FractOp41DoubleCoques = Round(Divide(FractOp41DoubleCoques, iPanels), 3)
        Case Is <= iStiffOuter
            FractOp41DoubleCoques = Divide(iProfilesVoiles, iStiffOuter)
            FractOp41DoubleCoques = Round(Divide(FractOp41DoubleCoques, iPanels), 3)
    End Select
    If FractOp41DoubleCoques < 0 Then FractOp41DoubleCoques = 0
    Exit Function
FractOp41DoubleCoquesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp41DoubleCoques")
End Function

'OPERATION 42 - Réglages + soudure des contacts verticaux du bordé
' Exp:
' double coques outer:
' k = nombre couples intermédiaires de carlingues sur nappes
Public Function FractOp42BordesSimples(ByRef cPanel As cPanel) As Double
    On Error GoTo FractOp42BordesSimplesErr
        Select Case cPanel.pType
            Case Plate, DoubleHull
                Select Case cPanel.cCostCAtMain.ID_PANNEAU
                    Case NappePlane
                        Select Case cPanel.cCostCAtMain.IT_PANNEAU
                            Case DoubleCoqueExterieure
                                FractOp42BordesSimples = cPanel.cCostCAtMain.CouplesCarlinguesSurNappes
                            Case Else
                                FractOp42BordesSimples = 0
                        End Select
                    Case Else
                        FractOp42BordesSimples = 0
                End Select
        End Select
    Exit Function
FractOp42BordesSimplesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp42BordesSimples")
End Function

'OPERATION 43 - Réglages + soudure des couples intermédiaires du bordé
'---> mêmes fractionnements que pour l'opération 42

'OPERATION 44 - Mise en place + soudure des tapes non-etanches sur la nappe de bordé
'(T synthétiques)
' Exp:
' si tapes non-étanches, t synthétique: k = 1
Public Function FractOp44BordesSimples(ByRef cPanel As cPanel) As Double
    On Error GoTo FractOp44BordesSimplesErr
    Select Case cPanel.cCostCAtMain.ID_PANNEAU
        Case NappePlane
            Select Case cPanel.cCostCAtMain.IT_PANNEAU
                Case DoubleCoqueExterieure
                    Select Case cPanel.cCostCAtMain.SectionTapes '= Profiles: = TSynthetiques
                        Case TSynthetiques
                            Select Case cPanel.cCostCAtMain.TypeTapes
                                Case NonEtanchesRecouvrement
                                    FractOp44BordesSimples = 1
                                Case NonEtanchesEncastrees
                                    FractOp44BordesSimples = 1
                                Case EtanchesRecouvrement
                                    FractOp44BordesSimples = 0
                                Case EtanchesEncastrees
                                    FractOp44BordesSimples = 0
                            End Select
                        Case Profiles
                            FractOp44BordesSimples = 0
                    End Select
                Case Else
                    FractOp44BordesSimples = 0
            End Select
        Case Else
            FractOp44BordesSimples = 0
        End Select
    Exit Function
FractOp44BordesSimplesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp44BordesSimples")
End Function

'OPERATION 45 - Mise en place + soudure des tapes etanches sur la nappe de bordé
'(T synthétiques)
' Exp:
' si tapes étanches, t synthétique: k = 1
Public Function FractOp45BordesSimples(ByRef cPanel As cPanel) As Double
    On Error GoTo FractOp45BordesSimplesErr
    Select Case cPanel.cCostCAtMain.ID_PANNEAU
        Case NappePlane
            Select Case cPanel.cCostCAtMain.IT_PANNEAU
                Case DoubleCoqueExterieure
                    Select Case cPanel.cCostCAtMain.SectionTapes
                        Case TSynthetiques
                            Select Case cPanel.cCostCAtMain.TypeTapes
                                Case NonEtanchesRecouvrement
                                    FractOp45BordesSimples = 0
                                Case NonEtanchesEncastrees
                                    FractOp45BordesSimples = 0
                                Case EtanchesRecouvrement
                                    FractOp45BordesSimples = 1
                                Case EtanchesEncastrees
                                    FractOp45BordesSimples = 1
                            End Select
                        Case Profiles
                            FractOp45BordesSimples = 0
                    End Select
                Case Else
                    FractOp45BordesSimples = 0
            End Select
        Case Else
            FractOp45BordesSimples = 0
        End Select
    Exit Function
FractOp45BordesSimplesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp45BordesSimples")
End Function

'PRE-MONTAGE - MONTAGE
'OPERATION 46 - Préparer, mettre, balancer
'---> zeros partout (pas traité)
 
'OPERATION 47 - Réglage et soudure des joints de panneaux
' panneaux de nappe:
' boucle sur toutes les noeuds
'   boucle sur toutes les panneaux
'       si noeud exterieur de panneau de nappe exterieur --> panneau avec soudure
'       si nombre panneaux avec soudure dans le noeud = 1 -> k(i) = k(i - 1) + 1
'       si nombre panneaux avec soudure dans le noeud > 1 -> k(i) = k(i - 1) + (nombre panneaux avec soudure - 1) / (nombre panneaux avec soudure)

Public Function FractOp47Nappes(ByRef colPanel As Collection, ByRef colNappe As Collection, ByVal ProjectIndex As Integer)
    On Error GoTo FractOp47NappesErr
    Dim cPanel As cPanel, Panel As cPanel
    Dim cNode As cNode, cNappe As cCostCAtNappe
    Dim cIndex As cIndex, colIndex As colIndex
    Dim i As Integer, j As Integer, k As Integer
    Dim iNappeSimple As Integer, iNappeComposee As Integer
    Dim iPanels As Integer, iSoudure As Double
    Dim iDCext As Integer, iBouchain As Integer
    OpenFile "soudure_nodes"
    For Each cNode In Project.Item(ProjectIndex).colNodes
        iPanels = 0
        'verifier si node entre bouchain et panneau de double fond extérieur
        iDCext = 0
        iBouchain = 0
        For Each cPanel In colPanel
            If cPanel.cGeometry.InNode = cNode.nNumber Or _
            cPanel.cGeometry.OutNode = cNode.nNumber Then  'selectionne panneaux liés au cNode
                If cPanel.cCostCAtMain.IT_PANNEAU = DoubleCoqueExterieure Then
                    iDCext = iDCext + 1
                End If
                If cPanel.cCostCAtMain.ID_PANNEAU = Bouchain Then
                    iBouchain = iBouchain + 1
                End If
            End If
        Next cPanel
        If iBouchain > 0 And iDCext > 0 Then GoTo NextcNode
        
        For Each cPanel In colPanel 'boucle sur nappes simples
            If cPanel.cGeometry.InNode = cNode.nNumber Or _
            cPanel.cGeometry.OutNode = cNode.nNumber Then  'selectionne panneaux liés au cNode
                If cPanel.cCostCAtMain.ID_PANNEAU = NappePlane And _
                cPanel.cCostCAtMain.IP_PANNEAU = ExterieurNappe And _
                cPanel.cCostCAtMain.bIsPartOfNappe = False And _
                cPanel.cCostCAtMain.ID_PANNEAU <> Virtual Then 'selectionne panneaux extérieurs
                                                                'de nappe plane simple non-virtuels
                    iPanels = iPanels + 1
                    cPanel.Selected = isSelected
                End If
                If cPanel.cCostCAtMain.ID_PANNEAU = Bouchain Then
                    iPanels = iPanels + 1
                    cPanel.Selected = isSelected
                End If
            End If
        Next cPanel
        
        For Each cNappe In colNappe 'boucle sur nappes composées
            iNappeComposee = 0
            For Each cIndex In cNappe.Nappe 'boucle sur les panneaux de la nappe
                Set cPanel = colPanel.Item(cIndex.Number)
                'If cPanel.cCostCAtMain.IP_PANNEAU = ExterieurNappe Then
                    If cPanel.cGeometry.InNode = cNode.nNumber Or _
                    cPanel.cGeometry.OutNode = cNode.nNumber Then  'selectionne panneaux liés au cNode
                        iNappeComposee = iNappeComposee + 1
                        'cPanel.Selected = isSelected
                    End If
                'End If
            Next cIndex
            If iNappeComposee > 1 Then
            ElseIf iNappeComposee = 1 Then
                For Each cIndex In cNappe.Nappe 'boucle sur les panneaux de la nappe
                    Set cPanel = colPanel.Item(cIndex.Number)
                    'If cPanel.cCostCAtMain.IP_PANNEAU = ExterieurNappe Then
                        If cPanel.cGeometry.InNode = cNode.nNumber Or _
                        cPanel.cGeometry.OutNode = cNode.nNumber Then  'selectionne panneaux liés au cNode
                            iPanels = iPanels + 1
                            cPanel.Selected = isSelected
                        End If
                    'End If
                Next cIndex

            End If
            Set cPanel = Nothing
        Next cNappe
        
        For Each cPanel In colPanel
            If cPanel.Selected = isSelected Then
                cPanel.pNumber = cPanel.pNumber
                'test symm direct
                If cPanel.colBoundaryConditions.Count > 0 Then
                    If cPanel.colBoundaryConditions.Item(1).BoundaryCondition = SymmetryAxis1 Then
                        cPanel.cCostCAtMain.colCostCAtOperations.Item(47).Fractionnement = _
                        cPanel.cCostCAtMain.colCostCAtOperations.Item(47).Fractionnement + 0.5
                        GoTo NextcPanel
                    End If
                End If
                'test symm indirect
                For Each Panel In colPanel
                    Select Case Panel.cCostCAtMain.ID_PANNEAU
                        Case Virtual
                            If Panel.cGeometry.InNode = cPanel.cGeometry.InNode Or Panel.cGeometry.OutNode = cPanel.cGeometry.InNode Or _
                            Panel.cGeometry.InNode = cPanel.cGeometry.OutNode Or Panel.cGeometry.OutNode = cPanel.cGeometry.OutNode Then
                                If Panel.colBoundaryConditions.Count > 0 Then
                                    If Panel.colBoundaryConditions.Item(1).BoundaryCondition = SymmetryAxis1 Then
                                        cPanel.cCostCAtMain.colCostCAtOperations.Item(47).Fractionnement = _
                                        cPanel.cCostCAtMain.colCostCAtOperations.Item(47).Fractionnement + 0.5
                                        GoTo NextcPanel
                                    End If
                                End If
                            End If
                    End Select
                Next Panel
                If iPanels > 1 Then ' plusieurs panneaux avec soudure dans le noeud
                    cPanel.cCostCAtMain.colCostCAtOperations.Item(47).Fractionnement = _
                    cPanel.cCostCAtMain.colCostCAtOperations.Item(47).Fractionnement + (iPanels - 1) / (iPanels)
                ElseIf iPanels = 1 Then ' un seul panneau avec soudure dans le noeud
                    cPanel.cCostCAtMain.colCostCAtOperations.Item(47).Fractionnement = _
                    cPanel.cCostCAtMain.colCostCAtOperations.Item(47).Fractionnement + 1
                End If
            End If
NextcPanel:
        Next cPanel
        WriteFile "node: " & cNode.nNumber & "; " & "soudures: " & iPanels
        For Each cPanel In colPanel
            cPanel.Selected = IsUnselected
        Next cPanel
NextcNode:
    Next cNode
    CloseFile
    Exit Function
FractOp47NappesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp47Nappes")
End Function

'OPERATION 48 - Réglage et soudure des abouts des panneaux
' Exp:
' tous les panneaux sauf Epontille et Fictif: k = 1
Public Function FractOp48BordesSimples(ByRef cPanel As cPanel) As Double
    On Error GoTo FractOp48BordesSimplesErr
    Select Case cPanel.cCostCAtMain.ID_PANNEAU
        Case Epontille, Virtual
            FractOp48BordesSimples = 0
        Case Else
            FractOp48BordesSimples = 1
    End Select
    Exit Function
FractOp48BordesSimplesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp48BordesSimples")
End Function

'OPERAION 49 - Réglage et soudure des abouts des lisses (HP)
'---> !!!à faire - introduire dans paramatres de la nappe planne (etape 5) le type de lisse (HP ou T)
Public Function FractOp49BordesSimples(ByRef cPanel As cPanel) As Double
    On Error GoTo FractOp49BordesSimplesErr
    Select Case cPanel.cCostCAtMain.ID_PANNEAU
        Case NappePlane, Bouchain
            Select Case cPanel.cCostCAtMain.SectionLisses '= Profiles: = TSynthetiques
                Case Profiles
                    FractOp49BordesSimples = 1
                Case TSynthetiques
                    FractOp49BordesSimples = 0
            End Select
        Case Else
            FractOp49BordesSimples = 0
    End Select
    Exit Function
FractOp49BordesSimplesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp49BordesSimples")
End Function

Public Function FractOp49DoubleCoques(ByRef cDoubleHull As cCostCAtDHull, ByVal ProjectIndex As Integer) As Double
    On Error GoTo FractOp49DoubleCoquesErr
    Select Case cDoubleHull.SectionLisses '= Profiles: = TSynthetiques
        Case Profiles
            FractOp49DoubleCoques = 1
        Case TSynthetiques
            FractOp49DoubleCoques = 0
    End Select
    Exit Function
FractOp49DoubleCoquesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp49DoubleCoques")
End Function

'OPERATION 50 - Réglage et soudure des abouts des lisses (T Synthétiques)
'---> !!!à faire - introduire dans paramatres de la nappe planne (etape 5) le type de lisse (HP ou T)
Public Function FractOp50BordesSimples(ByRef cPanel As cPanel) As Double
    On Error GoTo FractOp50BordesSimplesErr
    Select Case cPanel.cCostCAtMain.ID_PANNEAU
        Case NappePlane, Bouchain
            Select Case cPanel.cCostCAtMain.SectionLisses '= Profiles: = TSynthetiques
                Case Profiles
                    FractOp50BordesSimples = 0
                Case TSynthetiques
                    FractOp50BordesSimples = 1
            End Select
        Case Else
            FractOp50BordesSimples = 0
    End Select
    Exit Function
FractOp50BordesSimplesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp50BordesSimples")
End Function

Public Function FractOp50DoubleCoques(ByRef cDoubleHull As cCostCAtDHull, ByVal ProjectIndex As Integer) As Double
    On Error GoTo FractOp50DoubleCoquesErr
    Select Case cDoubleHull.SectionLisses '= Profiles: = TSynthetiques
        Case Profiles
            FractOp50DoubleCoques = 0
        Case TSynthetiques
            FractOp50DoubleCoques = 1
    End Select
    Exit Function
FractOp50DoubleCoquesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp50DoubleCoques")
End Function

'OPERATION 51 - Réglage et soudure des voiles sur carlingues
'---> '''à faire - trouver une modalité pour depister les carlingues alignées sur l'ax neutre
Public Function FractOp51Carlingues(ByRef cPanel As cPanel, ProjectIndex As Integer) As Double
    On Error GoTo FractOp51CarlinguesErr
    Dim Panel As cPanel
    Select Case cPanel.cCostCAtMain.ID_PANNEAU '= CarlingueSerreHiloire
        Case CarlingueSerreHiloire
            Select Case IsPanelOnSymmAxis(cPanel.pNumber, ProjectIndex)
                Case Is = True
                    FractOp51Carlingues = 1
                Case Is = False
                    FractOp51Carlingues = 0
            End Select
        Case Else
            FractOp51Carlingues = 0
    End Select
    Exit Function
FractOp51CarlinguesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp51Carlingues")
End Function

'OPERATION 52 - DIVERS
'---> zeros partout (pas traité)

'OPERATION 53 - Réglage et soudure des barrots sur nappe
Public Function FractOp53BordesSimples(ByRef cPanel As cPanel) As Double
    On Error GoTo FractOp53BordesSimplesErr
    'tous les panneaux de bordé simple
    Select Case cPanel.cCostCAtMain.ID_PANNEAU
        Case NappePlane
            Select Case cPanel.cCostCAtMain.IT_PANNEAU
                Case BordeSimple
                    FractOp53BordesSimples = 1
                Case Else
                    FractOp53BordesSimples = 0
            End Select
        Case Else
            FractOp53BordesSimples = 0
    End Select
    Exit Function
FractOp53BordesSimplesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp53BordesSimples")
End Function

'OPERATION 54 - Réglage et soudure des hiloires sur nappe
' hiloires peuvent être definies au dessus d'une eppontille ou 'dans l'air'. Si elles sont definies en dessus d'une plaque
' le fractionnement sera erroné
Public Function FractOp54BordesSimples(ByRef colPanel As Collection, ByVal ProjectIndex As Integer) As Double
    On Error GoTo FractOp54BordesSimplesErr
    Dim Index1 As Integer, Index2 As Integer
    Dim cPanel As cPanel
    For Each cPanel In colPanel
        If cPanel.cScantlings.colGirder.Count = 0 Then
            'Exit Function
            GoTo NextcPanel
        End If
        Dim Girder As cGirder, Panel As cPanel
        For Each Girder In cPanel.cScantlings.colGirder
            'test girder & frame on the same side
            If cPanel.cScantlings.GirderSide <> cPanel.cScantlings.cPrimaryFrames.Side Then
                GoTo NextGirder
            End If
            'case girder inside panel 0 < X < L
            If Girder.Distance > 0 And Round(Girder.Distance, 2) < cPanel.cGeometry.PanelWidth Then
                colPanel.Item(cPanel.pNumber).cCostCAtMain.colCostCAtOperations.Item(54).Fractionnement = _
                colPanel.Item(cPanel.pNumber).cCostCAtMain.colCostCAtOperations.Item(54).Fractionnement + 1
            End If
            'case girder on panel start extremity X = 0
                'If Round(Girder.Distance,2) = 0 Then
            Select Case Round(Girder.Distance, 2)
                Case Is = 0
                    'test ax symm on girder's panel
                    If cPanel.colBoundaryConditions.Count > 0 Then
                        If cPanel.colBoundaryConditions.Item(1).Edge = InEdge Then
                            If cPanel.colBoundaryConditions.Item(1).BoundaryCondition = SymmetryAxis1 Then
                                colPanel.Item(cPanel.pNumber).cCostCAtMain.colCostCAtOperations.Item(54).Fractionnement = _
                                colPanel.Item(cPanel.pNumber).cCostCAtMain.colCostCAtOperations.Item(54).Fractionnement + 0.5
                            End If
                            If cPanel.colBoundaryConditions.Item(1).BoundaryCondition = FreeEdge Then
                                colPanel.Item(cPanel.pNumber).cCostCAtMain.colCostCAtOperations.Item(54).Fractionnement = _
                                colPanel.Item(cPanel.pNumber).cCostCAtMain.colCostCAtOperations.Item(54).Fractionnement + 1
                            End If
                        End If
                    End If
                    For Each Panel In Project.Item(ProjectIndex).colPanel
                        If DetectParalelism(cPanel, Panel, ProjectIndex) = True Then
                            If Panel.pNumber = cPanel.pNumber Then GoTo NextPanel1
                            Select Case Panel.cCostCAtMain.ID_PANNEAU
                            '= NappePlane: =CarlingueSerreHiloire: = Bouchain:  = Virtual: = Epontille
                                Case NappePlane, Bouchain
                                    If Panel.cGeometry.InNode = cPanel.cGeometry.InNode Or Panel.cGeometry.OutNode = cPanel.cGeometry.InNode Then
                                        Index1 = Index1 + 1
                                        colPanel.Item(cPanel.pNumber).cCostCAtMain.colCostCAtOperations.Item(54).Fractionnement = _
                                        colPanel.Item(cPanel.pNumber).cCostCAtMain.colCostCAtOperations.Item(54).Fractionnement + 0.5
                                        colPanel.Item(Panel.pNumber).cCostCAtMain.colCostCAtOperations.Item(54).Fractionnement = _
                                        colPanel.Item(Panel.pNumber).cCostCAtMain.colCostCAtOperations.Item(54).Fractionnement + 0.5
                                    End If
                                Case Virtual
                                    'test ax symm on each panel
                                    If Panel.cGeometry.InNode = cPanel.cGeometry.InNode Or Panel.cGeometry.OutNode = cPanel.cGeometry.InNode Then
                                        If Panel.colBoundaryConditions.Count > 0 And Panel.cCostCAtMain.ID_PANNEAU = Virtual Then
                                            If Panel.colBoundaryConditions.Item(1).BoundaryCondition = SymmetryAxis1 Then
                                                Index2 = Index2 + 1
                                                colPanel.Item(cPanel.pNumber).cCostCAtMain.colCostCAtOperations.Item(54).Fractionnement = _
                                                colPanel.Item(cPanel.pNumber).cCostCAtMain.colCostCAtOperations.Item(54).Fractionnement + 0.5
                                            End If
                                        End If
                                    End If
                            End Select
                        End If
NextPanel1:
                    Next Panel
                    If cPanel.colBoundaryConditions.Count = 0 Then ' unselect the free node cPanels
                        If Index1 = 0 Then ' no other panel to share the distribution with
                            If Index2 = 0 Then ' the panel's node is not on symmetry axis
                                colPanel.Item(cPanel.pNumber).cCostCAtMain.colCostCAtOperations.Item(54).Fractionnement = _
                                colPanel.Item(cPanel.pNumber).cCostCAtMain.colCostCAtOperations.Item(54).Fractionnement + 1
                            End If
                        End If
                    End If
                    Index1 = 0: Index2 = 0
                'End If
                Case Is = cPanel.cGeometry.PanelWidth
                    'test ax symm on girder's panel
                    If cPanel.colBoundaryConditions.Count > 0 Then
                        If cPanel.colBoundaryConditions.Item(1).Edge = OutEdge Then
                            If cPanel.colBoundaryConditions.Item(1).BoundaryCondition = SymmetryAxis1 Then
                                colPanel.Item(cPanel.pNumber).cCostCAtMain.colCostCAtOperations.Item(54).Fractionnement = _
                                colPanel.Item(cPanel.pNumber).cCostCAtMain.colCostCAtOperations.Item(54).Fractionnement + 0.5
                            End If
                            If cPanel.colBoundaryConditions.Item(1).BoundaryCondition = FreeEdge Then
                                colPanel.Item(cPanel.pNumber).cCostCAtMain.colCostCAtOperations.Item(54).Fractionnement = _
                                colPanel.Item(cPanel.pNumber).cCostCAtMain.colCostCAtOperations.Item(54).Fractionnement + 1
                            End If
                        End If
                    End If
                    For Each Panel In Project.Item(ProjectIndex).colPanel
                        If DetectParalelism(cPanel, Panel, ProjectIndex) = True Then
                            If Panel.pNumber = cPanel.pNumber Then GoTo NextPanel2
                            Select Case Panel.cCostCAtMain.ID_PANNEAU
                            '= NappePlane: =CarlingueSerreHiloire: = Bouchain:  = Virtual: = Epontille
                                Case NappePlane, Bouchain
                                    If Panel.cGeometry.InNode = cPanel.cGeometry.OutNode Or Panel.cGeometry.OutNode = cPanel.cGeometry.OutNode Then
                                        Index1 = Index1 + 1
                                        colPanel.Item(cPanel.pNumber).cCostCAtMain.colCostCAtOperations.Item(54).Fractionnement = _
                                        colPanel.Item(cPanel.pNumber).cCostCAtMain.colCostCAtOperations.Item(54).Fractionnement + 0.5
                                        colPanel.Item(Panel.pNumber).cCostCAtMain.colCostCAtOperations.Item(54).Fractionnement = _
                                        colPanel.Item(Panel.pNumber).cCostCAtMain.colCostCAtOperations.Item(54).Fractionnement + 0.5
                                    End If
                                Case Virtual
                                    'test ax symm on each panel
                                    If Panel.cGeometry.InNode = cPanel.cGeometry.OutNode Or Panel.cGeometry.OutNode = cPanel.cGeometry.OutNode Then
                                        If Panel.colBoundaryConditions.Count > 0 Then
                                            If Panel.colBoundaryConditions.Item(1).BoundaryCondition = SymmetryAxis1 Then
                                                Index2 = Index2 + 1
                                                colPanel.Item(cPanel.pNumber).cCostCAtMain.colCostCAtOperations.Item(54).Fractionnement = _
                                                colPanel.Item(cPanel.pNumber).cCostCAtMain.colCostCAtOperations.Item(54).Fractionnement + 0.5
                                            End If
                                        End If
                                    End If
                            End Select
                        End If
NextPanel2:
                    Next Panel
                    If cPanel.colBoundaryConditions.Count = 0 Then ' unselect the free node cPanels
                        If Index1 = 0 Then ' no other panel to share the distribution with
                            If Index2 = 0 Then ' the panel's node is not on symmetry axis
                                colPanel.Item(cPanel.pNumber).cCostCAtMain.colCostCAtOperations.Item(54).Fractionnement = _
                                colPanel.Item(cPanel.pNumber).cCostCAtMain.colCostCAtOperations.Item(54).Fractionnement + 1
                            End If
                        End If
                    End If
                    Index1 = 0: Index2 = 0
            End Select
NextGirder:
        Next Girder
NextcPanel:
    Next cPanel
    Exit Function
FractOp54BordesSimplesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp54BordesSimples")
End Function

'OPERATION 55 - Réglage et soudure des contacts des barrots sur cloison
Public Function FractOp55BordesSimples(ByRef cPanel As cPanel) As Double
    On Error GoTo FractOp55BordesSimplesErr
    Select Case cPanel.cCostCAtMain.ID_PANNEAU
        Case NappePlane
            FractOp55BordesSimples = cPanel.cCostCAtMain.ContactsBarrotsCloisons
        Case Else
            FractOp55BordesSimples = 0
    End Select
    Exit Function
FractOp55BordesSimplesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp55BordesSimples")
End Function

'OPERATION 56 - Réglage et soudure des abouts (barrot - hiloire)
' hiloires peuvent être definies au dessus d'une eppontille ou 'dans l'air'. Si elles sont definies en dessus d'une plaque
' le fractionnement sera erroné
Public Function FractOp56BordesSimples(ByRef colPanel As Collection, ByVal ProjectIndex As Integer) As Double
    On Error GoTo FractOp56BordesSimplesErr
    Dim Index1 As Integer, Index2 As Integer
    Dim cPanel As cPanel
    For Each cPanel In colPanel
        If cPanel.cScantlings.colGirder.Count = 0 Then
            'Exit Function
            GoTo NextcPanel
        End If
        Dim Girder As cGirder, Panel As cPanel
        For Each Girder In cPanel.cScantlings.colGirder
            'test girder & frame on the same side
            If cPanel.cScantlings.GirderSide <> cPanel.cScantlings.cPrimaryFrames.Side Then
                GoTo NextGirder
            End If
            'case girder inside panel 0 < X < L
            If Girder.Distance > 0 And Round(Girder.Distance, 2) < cPanel.cGeometry.PanelWidth Then
                colPanel.Item(cPanel.pNumber).cCostCAtMain.colCostCAtOperations.Item(56).Fractionnement = _
                colPanel.Item(cPanel.pNumber).cCostCAtMain.colCostCAtOperations.Item(56).Fractionnement + 1 * 2
            End If
            'case girder on panel start extremity X = 0
                'If Round(Girder.Distance,2) = 0 Then
            Select Case Round(Girder.Distance, 2)
                Case Is = 0
                    'test ax symm on girder's panel
                    If cPanel.colBoundaryConditions.Count > 0 Then
                        If cPanel.colBoundaryConditions.Item(1).Edge = InEdge Then
                            If cPanel.colBoundaryConditions.Item(1).BoundaryCondition = SymmetryAxis1 Then
                                colPanel.Item(cPanel.pNumber).cCostCAtMain.colCostCAtOperations.Item(56).Fractionnement = _
                                colPanel.Item(cPanel.pNumber).cCostCAtMain.colCostCAtOperations.Item(56).Fractionnement + 0.5 * 2
                            End If
                            If cPanel.colBoundaryConditions.Item(1).BoundaryCondition = FreeEdge Then
                                colPanel.Item(cPanel.pNumber).cCostCAtMain.colCostCAtOperations.Item(56).Fractionnement = _
                                colPanel.Item(cPanel.pNumber).cCostCAtMain.colCostCAtOperations.Item(56).Fractionnement + 1
                            End If
                        End If
                    End If
                    For Each Panel In Project.Item(ProjectIndex).colPanel
                        If DetectParalelism(cPanel, Panel, ProjectIndex) = True Then
                            If Panel.pNumber = cPanel.pNumber Then GoTo NextPanel1
                            Select Case Panel.cCostCAtMain.ID_PANNEAU
                            '= NappePlane: =CarlingueSerreHiloire: = Bouchain:  = Virtual: = Epontille
                                Case NappePlane, Bouchain
                                    If Panel.cGeometry.InNode = cPanel.cGeometry.InNode Or Panel.cGeometry.OutNode = cPanel.cGeometry.InNode Then
                                        Index1 = Index1 + 1
                                        colPanel.Item(cPanel.pNumber).cCostCAtMain.colCostCAtOperations.Item(56).Fractionnement = _
                                        colPanel.Item(cPanel.pNumber).cCostCAtMain.colCostCAtOperations.Item(56).Fractionnement + 0.5 * 2
                                        colPanel.Item(Panel.pNumber).cCostCAtMain.colCostCAtOperations.Item(56).Fractionnement = _
                                        colPanel.Item(Panel.pNumber).cCostCAtMain.colCostCAtOperations.Item(56).Fractionnement + 0.5 * 2
                                    End If
                                Case Virtual
                                    'test ax symm on each panel
                                    If Panel.cGeometry.InNode = cPanel.cGeometry.InNode Or Panel.cGeometry.OutNode = cPanel.cGeometry.InNode Then
                                        If Panel.colBoundaryConditions.Count > 0 And Panel.cCostCAtMain.ID_PANNEAU = Virtual Then
                                            If Panel.colBoundaryConditions.Item(1).BoundaryCondition = SymmetryAxis1 Then
                                                Index2 = Index2 + 1
                                                colPanel.Item(cPanel.pNumber).cCostCAtMain.colCostCAtOperations.Item(56).Fractionnement = _
                                                colPanel.Item(cPanel.pNumber).cCostCAtMain.colCostCAtOperations.Item(56).Fractionnement + 0.5 * 2
                                            End If
                                        End If
                                    End If
                            End Select
                        End If
NextPanel1:
                    Next Panel
                    If cPanel.colBoundaryConditions.Count = 0 Then ' unselect the free node cPanels
                        If Index1 = 0 Then ' no other panel to share the distribution with
                            If Index2 = 0 Then ' the panel's node is not on symmetry axis
                                colPanel.Item(cPanel.pNumber).cCostCAtMain.colCostCAtOperations.Item(56).Fractionnement = _
                                colPanel.Item(cPanel.pNumber).cCostCAtMain.colCostCAtOperations.Item(56).Fractionnement + 1
                            End If
                        End If
                    End If
                    Index1 = 0: Index2 = 0
                'End If
                Case Is = cPanel.cGeometry.PanelWidth
                    'test ax symm on girder's panel
                    If cPanel.colBoundaryConditions.Count > 0 Then
                        If cPanel.colBoundaryConditions.Item(1).Edge = OutEdge Then
                            If cPanel.colBoundaryConditions.Item(1).BoundaryCondition = SymmetryAxis1 Then
                                colPanel.Item(cPanel.pNumber).cCostCAtMain.colCostCAtOperations.Item(56).Fractionnement = _
                                colPanel.Item(cPanel.pNumber).cCostCAtMain.colCostCAtOperations.Item(56).Fractionnement + 0.5 * 2
                            End If
                            If cPanel.colBoundaryConditions.Item(1).BoundaryCondition = FreeEdge Then
                                colPanel.Item(cPanel.pNumber).cCostCAtMain.colCostCAtOperations.Item(56).Fractionnement = _
                                colPanel.Item(cPanel.pNumber).cCostCAtMain.colCostCAtOperations.Item(56).Fractionnement + 1
                            End If
                        End If
                    End If
                    For Each Panel In Project.Item(ProjectIndex).colPanel
                        If DetectParalelism(cPanel, Panel, ProjectIndex) = True Then
                            If Panel.pNumber = cPanel.pNumber Then GoTo NextPanel2
                            Select Case Panel.cCostCAtMain.ID_PANNEAU
                            '= NappePlane: =CarlingueSerreHiloire: = Bouchain:  = Virtual: = Epontille
                                Case NappePlane, Bouchain
                                    If Panel.cGeometry.InNode = cPanel.cGeometry.OutNode Or Panel.cGeometry.OutNode = cPanel.cGeometry.OutNode Then
                                        Index1 = Index1 + 1
                                        colPanel.Item(cPanel.pNumber).cCostCAtMain.colCostCAtOperations.Item(56).Fractionnement = _
                                        colPanel.Item(cPanel.pNumber).cCostCAtMain.colCostCAtOperations.Item(56).Fractionnement + 0.5 * 2
                                        colPanel.Item(Panel.pNumber).cCostCAtMain.colCostCAtOperations.Item(56).Fractionnement = _
                                        colPanel.Item(Panel.pNumber).cCostCAtMain.colCostCAtOperations.Item(56).Fractionnement + 0.5 * 2
                                    End If
                                Case Virtual
                                    'test ax symm on each panel
                                    If Panel.cGeometry.InNode = cPanel.cGeometry.OutNode Or Panel.cGeometry.OutNode = cPanel.cGeometry.OutNode Then
                                        If Panel.colBoundaryConditions.Count > 0 Then
                                            If Panel.colBoundaryConditions.Item(1).BoundaryCondition = SymmetryAxis1 Then
                                                Index2 = Index2 + 1
                                                colPanel.Item(cPanel.pNumber).cCostCAtMain.colCostCAtOperations.Item(56).Fractionnement = _
                                                colPanel.Item(cPanel.pNumber).cCostCAtMain.colCostCAtOperations.Item(56).Fractionnement + 0.5 * 2
                                            End If
                                        End If
                                    End If
                            End Select
                        End If
NextPanel2:
                    Next Panel
                    If cPanel.colBoundaryConditions.Count = 0 Then ' unselect the free node cPanels
                        If Index1 = 0 Then ' no other panel to share the distribution with
                            If Index2 = 0 Then ' the panel's node is not on symmetry axis
                                colPanel.Item(cPanel.pNumber).cCostCAtMain.colCostCAtOperations.Item(56).Fractionnement = _
                                colPanel.Item(cPanel.pNumber).cCostCAtMain.colCostCAtOperations.Item(56).Fractionnement + 1
                            End If
                        End If
                    End If
                    Index1 = 0: Index2 = 0
            End Select
NextGirder:
        Next Girder
NextcPanel:
    Next cPanel
    Exit Function
FractOp56BordesSimplesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp56BordesSimples")
End Function

'OPERATION 57 - Réglage et soudure des jonctions barrot / hiloire
Public Function FractOp57BordesSimples(ByRef cPanel As cPanel) As Double
    On Error GoTo FractOp57BordesSimplesErr
    Select Case cPanel.cCostCAtMain.ID_PANNEAU
        Case NappePlane
            FractOp57BordesSimples = cPanel.cCostCAtMain.JonctionsBarrotHiloire
        Case Else
            FractOp57BordesSimples = 0
    End Select
    Exit Function
FractOp57BordesSimplesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp57BordesSimples")
End Function

'OPERATION 58 - Réglage et soudure des jonctions d'épontille (pré)
Public Function FractOp58Epontilles(ByRef cPanel As cPanel) As Double
    On Error GoTo FractOp58EpontillesErr
    Select Case cPanel.cCostCAtMain.ID_PANNEAU
        Case Epontille
            FractOp58Epontilles = 1
        Case Else
            FractOp58Epontilles = 0
    End Select
    Exit Function
FractOp58EpontillesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp58Epontilles")
End Function

'OPERATION 59 - Réglage et soudure des jonctions d'épontille (montage)
Public Function FractOp59Epontilles(ByRef cPanel As cPanel) As Double
    On Error GoTo FractOp59EpontillesErr
    Select Case cPanel.cCostCAtMain.ID_PANNEAU
        Case Epontille
            FractOp59Epontilles = 1
        Case Else
            FractOp59Epontilles = 0
    End Select
    Exit Function
FractOp59EpontillesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp59Epontilles")
End Function

'OPERATION 60 - Fabrication T synthétique
Public Function FractOp60BordesSimples(ByRef cPanel As cPanel) As Double
    On Error GoTo FractOp60BordesSimplesErr
    Select Case cPanel.cCostCAtMain.ID_PANNEAU
        Case NappePlane
            Select Case cPanel.cCostCAtMain.IT_PANNEAU
                Case BordeSimple
                    FractOp60BordesSimples = 1 + cPanel.cCostCAtMain.AccostagesVoiles
                Case Else
                    FractOp60BordesSimples = 0
            End Select
        Case Else
            FractOp60BordesSimples = 0
    End Select
    Exit Function
FractOp60BordesSimplesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function FractOp60BordesSimples")
End Function

Public Function ComputeFirstFractionnements(ByVal ProjectIndex As Integer)
    'On Error GoTo  ComputeFirstFractionnementsErr
    Dim cPanel As cPanel
    For Each cPanel In Project.Item(ProjectIndex).colPanel
        'Pré-Pré
        cPanel.cCostCAtMain.colCostCAtOperations.Item(1).Fractionnement = FractOp1BordesSimples(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(2).Fractionnement = FractOp2BordesSimples(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(3).Fractionnement = FractOp3BordesSimples(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(4).Fractionnement = FractOp4Carlingues(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(5).Fractionnement = FractOp5Carlingues(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(6).Fractionnement = FractOp6BordesSimples(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(7).Fractionnement = FractOp7BordesSimples(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(8).Fractionnement = FractOp8BordesSimples(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(9).Fractionnement = _
            cPanel.cCostCAtMain.colCostCAtOperations.Item(6).Fractionnement 'Mêmes fractionnements
        cPanel.cCostCAtMain.colCostCAtOperations.Item(10).Fractionnement = FractOp10BordesSimples(cPanel)
        'Nappe Plane
        cPanel.cCostCAtMain.colCostCAtOperations.Item(11).Fractionnement = FractOp11NappesSimples(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(12).Fractionnement = _
            cPanel.cCostCAtMain.colCostCAtOperations.Item(11).Fractionnement 'same as op. 11
        cPanel.cCostCAtMain.colCostCAtOperations.Item(13).Fractionnement = _
            cPanel.cCostCAtMain.colCostCAtOperations.Item(11).Fractionnement 'same as op. 11
        cPanel.cCostCAtMain.colCostCAtOperations.Item(14).Fractionnement = FractOp14NappesSimples(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(15).Fractionnement = _
            cPanel.cCostCAtMain.colCostCAtOperations.Item(14).Fractionnement 'same as op. 14
        cPanel.cCostCAtMain.colCostCAtOperations.Item(16).Fractionnement = _
            cPanel.cCostCAtMain.colCostCAtOperations.Item(14).Fractionnement 'same as op. 14
        cPanel.cCostCAtMain.colCostCAtOperations.Item(17).Fractionnement = _
            cPanel.cCostCAtMain.colCostCAtOperations.Item(11).Fractionnement 'same as op. 11
        cPanel.cCostCAtMain.colCostCAtOperations.Item(18).Fractionnement = _
            cPanel.cCostCAtMain.colCostCAtOperations.Item(11).Fractionnement 'same as op. 11
        cPanel.cCostCAtMain.colCostCAtOperations.Item(19).Fractionnement = FractOp19NappesSimples(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(20).Fractionnement = FractOp20NappesSimples(cPanel)
        'Pré-Assemblage
        cPanel.cCostCAtMain.colCostCAtOperations.Item(21).Fractionnement = FractOp21BordesSimples(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(22).Fractionnement = FractOp22Carlingues(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(23).Fractionnement = FractOp23Carlingues(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(24).Fractionnement = FractOp24BordesSimples(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(25).Fractionnement = FractOp25BordesSimples(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(26).Fractionnement = FractOp26BordesSimples(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(27).Fractionnement = FractOp27BordesSimples(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(28).Fractionnement = FractOp28BordesSimples(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(29).Fractionnement = FractOp28BordesSimples(cPanel) 'same as op. 28
        cPanel.cCostCAtMain.colCostCAtOperations.Item(30).Fractionnement = FractOp30BordesSimples(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(31).Fractionnement = FractOp31BordesSimples(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(32).Fractionnement = FractOp32Bouchain(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(33).Fractionnement = FractOp33Bouchain(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(34).Fractionnement = FractOp34Bouchain(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(36).Fractionnement = FractOp36BordesSimples(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(37).Fractionnement = FractOp22Carlingues(cPanel) 'same as op. 22
        cPanel.cCostCAtMain.colCostCAtOperations.Item(38).Fractionnement = FractOp38BordesSimples(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(39).Fractionnement = FractOp39BordesSimples(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(40).Fractionnement = FractOp40BordesSimples(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(41).Fractionnement = 0
        cPanel.cCostCAtMain.colCostCAtOperations.Item(42).Fractionnement = FractOp42BordesSimples(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(43).Fractionnement = FractOp42BordesSimples(cPanel) 'same as op. 42
        cPanel.cCostCAtMain.colCostCAtOperations.Item(44).Fractionnement = FractOp44BordesSimples(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(45).Fractionnement = FractOp45BordesSimples(cPanel)
        'Montage
        cPanel.cCostCAtMain.colCostCAtOperations.Item(46).Fractionnement = 0
        cPanel.cCostCAtMain.colCostCAtOperations.Item(48).Fractionnement = FractOp48BordesSimples(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(49).Fractionnement = FractOp49BordesSimples(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(50).Fractionnement = FractOp50BordesSimples(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(51).Fractionnement = FractOp51Carlingues(cPanel, ProjectIndex)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(52).Fractionnement = 0
        cPanel.cCostCAtMain.colCostCAtOperations.Item(53).Fractionnement = FractOp53BordesSimples(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(55).Fractionnement = FractOp55BordesSimples(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(57).Fractionnement = FractOp57BordesSimples(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(58).Fractionnement = FractOp58Epontilles(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(59).Fractionnement = FractOp59Epontilles(cPanel)
        cPanel.cCostCAtMain.colCostCAtOperations.Item(60).Fractionnement = FractOp60BordesSimples(cPanel)
    Next cPanel

    'Ops computed for all panels at once
    Dim colCostCAtNappe As New Collection
    Dim colPanel As New Collection
    For Each cPanel In Project.Item(ProjectIndex).colPanel
        colPanel.Add cPanel
    Next cPanel
    FractOp54BordesSimples colPanel, ProjectIndex 'out of 'for each ... next' sequence because values can change
                                                  'more than one time ( k for a panel can change when computing
                                                  'another panel
    FractOp56BordesSimples colPanel, ProjectIndex
    FractOp47Nappes colPanel, colCostCAtNappe, ProjectIndex

    Dim colP As New colPanel
    For Each cPanel In Project.Item(ProjectIndex).colPanel
        colP.Add colPanel.Item(cPanel.index), cPanel.index
    Next cPanel
    Set Project.Item(ProjectIndex).colPanel = colP
    Set colP = Nothing
    Set colCostCAtNappe = Nothing
    Set colPanel = Nothing
    Exit Function
ComputeFirstFractionnementsErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modCostCAtFractionnement: Function ComputeFirstFractionnements")
End Function
