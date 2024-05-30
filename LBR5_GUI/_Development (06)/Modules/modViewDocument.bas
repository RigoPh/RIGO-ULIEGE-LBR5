Attribute VB_Name = "modViewDocument"
Option Explicit
Dim fso As FileSystemObject
Dim txtfile

Public Sub open_file(sfile As String, ByVal iWriteAppend, ByRef f)
    'if i = 0 -> write
    'if i = 1 -> append
    
    Dim fso
    Set fso = CreateObject("Scripting.FileSystemObject")
    sfile = GetFilePath(Project.Item(ActiveProject).sFileName) & sfile
    If iWriteAppend = 0 Then
        Set f = fso.OpenTextFile(sfile, ForWriting, TristateUseDefault)
    ElseIf iWriteAppend = 1 Then
        Set f = fso.OpenTextFile(sfile, ForAppending, TristateUseDefault)
    End If
End Sub

Public Sub ViewFinalScantlings(ByVal ProjectIndex As Integer)
    On Error GoTo ViewFinalScantlingsErr
    Dim fso, f
    Dim sfile As String
    Set fso = CreateObject("Scripting.FileSystemObject")
    sfile = Project.Item(ProjectIndex).sFileName
    sfile = GetFilePath(sfile) & "final_scantl_" & GetFileRoot(sfile) & ".txt"
    Set f = fso.OpenTextFile(sfile, ForWriting, TristateUseDefault)
    Dim cPanel As cPanel, s As String, i As Integer
    f.WriteLine "NO" & vbTab & "PT" & vbTab & "SWH" & vbTab & "SWT" & vbTab & "SFB" & _
    vbTab & "SFT" & vbTab & "SS" & vbTab & "FWH" & vbTab & "FWT" & vbTab & "FFB" & vbTab & _
    "FFT" & vbTab & "FS"
    Dim no As Integer, pt As Double, swh As Double, swt As Double, sfb As Double, sft As Double, ss As Double
    Dim fwh As Double, fwt As Double, ffb As Double, fft As Double, fs As Double
    For Each cPanel In Project.Item(ProjectIndex).colPanelUpdate
        no = cPanel.pNumber
        With cPanel.cScantlings
            pt = Round(.NetThickness * 1000 + .CorrosionThickness * 1000, 1)
            swh = Round(.cPrimaryStiffeners.WebHeight * 1000, 1)
            swt = Round(.cPrimaryStiffeners.WebThickness * 1000 + .cPrimaryStiffeners.CorrosionThickness * 1000, 1)
            sfb = Round(.cPrimaryStiffeners.FlangeWidth * 1000, 1)
            sft = Round(.cPrimaryStiffeners.FlangeThickness * 1000 + .cPrimaryStiffeners.CorrosionThickness * 1000, 1)
            ss = Round(.cPrimaryStiffeners.Spacing * 1000, 1)
            
            fwh = Round(.cPrimaryFrames.WebHeight * 1000, 1)
            fwt = Round(.cPrimaryFrames.WebThickness * 1000 + .cPrimaryFrames.CorrosionThickness * 1000, 1)
            ffb = Round(.cPrimaryFrames.FlangeWidth * 1000, 1)
            fft = Round(.cPrimaryFrames.FlangeThickness * 1000 + .cPrimaryFrames.CorrosionThickness * 1000, 1)
            fs = Round(.cPrimaryFrames.Spacing * 1000, 1)
            
            f.WriteLine no & vbTab & pt & vbTab & swh & vbTab & swt & vbTab & sfb & vbTab & sft & vbTab & ss & _
            vbTab & fwh & vbTab & fwt & vbTab & ffb & vbTab & fft & vbTab & fs
        End With
    Next cPanel
    f.Close
    Dim ShellFile As Long
    If fso.FileExists(sfile) = True Then
        ShellFile = Shell(Windows_Path & "notepad.exe" & " " & sfile, vbNormalFocus)
    End If
    Exit Sub
ViewFinalScantlingsErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modViewDocument: Sub ViewFinalScantlings")
End Sub

'Public Sub ViewCostCAtData(ByVal ProjectIndex As Integer)
'    On Error GoTo  ViewCostCAtDataErr
'    Dim fso, f
'    Dim sFile As String
'    Set fso = CreateObject("Scripting.FileSystemObject")
'    sFile = Project.Item(ProjectIndex).sFileName
'    sFile = GetFilePath(sFile) & "CostCAt_Data_" & GetFileRoot(sFile) & ".txt"
'    Set f = fso.OpenTextFile(sFile, ForWriting, TristateUseDefault)
'    Dim cPanel As cPanel, s As String, i As Integer
'    f.WriteLine "I. Identification des panneaux"
'
'    f.WriteLine "NO" & vbTab & "ID" & vbTab & "IT" & vbTab & "IP"
'    Dim no As Integer, id As Integer, it As Integer, ip As Integer
'    For Each cPanel In Project.Item(ProjectIndex).colPanel
'        no = cPanel.pNumber
'        id = cPanel.cCostCAtMain.ID_PANNEAU
'        it = cPanel.cCostCAtMain.IT_PANNEAU
'        ip = cPanel.cCostCAtMain.IP_PANNEAU
'        f.WriteLine no & vbTab & id & vbTab & it & vbTab & ip
'    Next cPanel
'    f.Close
'    Dim ShellFile As Long
'    If fso.FileExists(sFile) = True Then
'        ShellFile = Shell(Windows_Path & "notepad.exe" & " " & sFile, vbNormalFocus)
'    End If
'    Exit Sub
'ViewCostCAtDataErr:
'    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modViewDocument: Sub ViewCostCAtData")
'End Sub



Public Sub ViewInitialScantlings(ByVal ProjectIndex As Integer)
    On Error GoTo ViewInitialScantlingsErr
    Dim fso, f
    Dim sfile As String
    Set fso = CreateObject("Scripting.FileSystemObject")
    sfile = Project.Item(ProjectIndex).sFileName
    sfile = GetFilePath(sfile) & "initial_scantl_" & GetFileRoot(sfile) & ".txt"
    Set f = fso.OpenTextFile(sfile, ForWriting, TristateUseDefault)
    Dim cPanel As cPanel, s As String, i As Integer
    f.WriteLine "NO" & vbTab & "PT" & vbTab & "SWH" & vbTab & "SWT" & vbTab & "SFB" & _
    vbTab & "SFT" & vbTab & "SS" & vbTab & "FWH" & vbTab & "FWT" & vbTab & "FFB" & vbTab & _
    "FFT" & vbTab & "FS"
    Dim no As Integer, pt As Double, swh As Double, swt As Double, sfb As Double, sft As Double, ss As Double
    Dim fwh As Double, fwt As Double, ffb As Double, fft As Double, fs As Double
    For Each cPanel In Project.Item(ProjectIndex).colPanel
        no = cPanel.pNumber
        With cPanel.cScantlings
            pt = Round(.NetThickness * 1000 + .CorrosionThickness * 1000, 1)
            swh = Round(.cPrimaryStiffeners.WebHeight * 1000, 1)
            swt = Round(.cPrimaryStiffeners.WebThickness * 1000 + .cPrimaryStiffeners.CorrosionThickness * 1000, 1)
            sfb = Round(.cPrimaryStiffeners.FlangeWidth * 1000, 1)
            sft = Round(.cPrimaryStiffeners.FlangeThickness * 1000 + .cPrimaryStiffeners.CorrosionThickness * 1000, 1)
            ss = Round(.cPrimaryStiffeners.Spacing * 1000, 1)
            
            fwh = Round(.cPrimaryFrames.WebHeight * 1000, 1)
            fwt = Round(.cPrimaryFrames.WebThickness * 1000 + .cPrimaryFrames.CorrosionThickness * 1000, 1)
            ffb = Round(.cPrimaryFrames.FlangeWidth * 1000, 1)
            fft = Round(.cPrimaryFrames.FlangeThickness * 1000 + .cPrimaryFrames.CorrosionThickness * 1000, 1)
            fs = Round(.cPrimaryFrames.Spacing * 1000, 1)
            
            f.WriteLine no & vbTab & pt & vbTab & swh & vbTab & swt & vbTab & sfb & vbTab & sft & vbTab & ss & _
            vbTab & fwh & vbTab & fwt & vbTab & ffb & vbTab & fft & vbTab & fs
        End With
    Next cPanel
    f.Close
    Dim ShellFile As Long
    If fso.FileExists(sfile) = True Then
        ShellFile = Shell(Windows_Path & "notepad.exe" & " " & sfile, vbNormalFocus)
    End If
    Exit Sub
ViewInitialScantlingsErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modViewDocument: Sub ViewInitialScantlings")
End Sub

Public Sub ViewSolution3(ByVal sfile As String)
    On Error GoTo ViewSolution3Err
    Dim sPrefix As String, sName As String, sPath As String, sDocName As String
    Dim ShellFile As Long
    Set fso = New FileSystemObject
    sPrefix = "sol3-"
    sPath = GetFilePath(sfile)
    sName = GetFileName(sfile)
    sDocName = GetFileRoot(sPath & sPrefix & sName) & ".txt"
    If fso.FileExists(sDocName) = True Then
        ShellFile = Shell(Windows_Path & "notepad.exe" & " " & sDocName, vbNormalFocus)
    Else
        MsgBox "File not found", vbCritical + vbOKOnly
        Exit Sub
    End If
    Exit Sub
ViewSolution3Err:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modViewDocument: Sub ViewSolution3")
End Sub

Public Sub ViewSolution1(ByVal sfile As String)
    On Error GoTo ViewSolution1Err
    Dim sPrefix As String, sName As String, sPath As String, sDocName As String
    Dim ShellFile As Long
    Set fso = New FileSystemObject
    sPrefix = "sol-"
    sPath = GetFilePath(sfile)
    sName = GetFileName(sfile)
    sDocName = GetFileRoot(sPath & sPrefix & sName) & ".txt"
    If fso.FileExists(sDocName) = True Then
        ShellFile = Shell(Windows_Path & "notepad.exe" & " " & sDocName, vbNormalFocus)
    Else
        MsgBox "File not found", vbCritical + vbOKOnly
        Exit Sub
    End If
    Exit Sub
ViewSolution1Err:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modViewDocument: Sub ViewSolution1")
End Sub

Public Sub WebSite()
    Dim ShellSite As Long
    ShellSite = ShellExecute(0, "open", "http://www.anast.ulg.ac.be/main.php?LGID=2&MID=34", "", "", SW_SHOWNORMAL)
End Sub

Public Sub ViewSolution2(ByVal sfile As String)
    On Error GoTo ViewSolution2Err
    Dim sPrefix As String, sName As String, sPath As String, sDocName As String
    Dim ShellFile As Long
    Set fso = New FileSystemObject
    sPrefix = "sol2-"
    sPath = GetFilePath(sfile)
    sName = GetFileName(sfile)
    sDocName = GetFileRoot(sPath & sPrefix & sName) & ".txt"
    If fso.FileExists(sDocName) = True Then
        ShellFile = Shell(Windows_Path & "notepad.exe" & " " & sDocName, vbNormalFocus)
    Else
        MsgBox "File not found", vbCritical + vbOKOnly
        Exit Sub
    End If
    Exit Sub
ViewSolution2Err:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modViewDocument: Sub ViewSolution2")
End Sub

Public Sub ViewOptimization(ByVal sfile As String)
    On Error GoTo ViewOptimizationErr
    Dim sPrefix As String, sName As String, sPath As String, sDocName As String
    Dim ShellFile As Long
    Set fso = New FileSystemObject
    sPrefix = "opt-"
    sPath = GetFilePath(sfile)
    sName = GetFileName(sfile)
    sDocName = GetFileRoot(sPath & sPrefix & sName) & ".txt"
    If fso.FileExists(sDocName) = True Then
        ShellFile = Shell(Windows_Path & "notepad.exe" & " " & sDocName, vbNormalFocus)
    Else
        MsgBox "File not found", vbCritical + vbOKOnly
        Exit Sub
    End If
    Exit Sub
ViewOptimizationErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modViewDocument: Sub ViewOptimization")
End Sub

Public Sub OpenFile(ByRef Name As String)
    On Error GoTo OpenFileErr
    Dim fso
    Dim sfile As String
    Set fso = CreateObject("Scripting.FileSystemObject")
    sfile = Project.Item(ActiveProject).sFileName
    sfile = GetFilePath(sfile) & Name & ".txt"
    Set txtfile = fso.OpenTextFile(sfile, ForWriting, TristateUseDefault)
    Exit Sub
OpenFileErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modViewDocument: Sub OpenFile")
End Sub

Public Sub WriteFile(ByRef str As String)
    txtfile.WriteLine str
End Sub

Public Sub CloseFile()
    txtfile.Close
End Sub

Public Sub ViewLogFile(ByVal sfile As String)
    On Error GoTo ViewOptimizationErr
    Dim sPrefix As String, sName As String, sPath As String, sDocName As String
    Dim ShellFile As Long
    Set fso = New FileSystemObject
    sPrefix = "bug-Sol-"
    sPath = GetFilePath(sfile)
    sName = GetFileName(sfile)
    sDocName = GetFileRoot(sPath & sPrefix & sName) & ".txt"
    If fso.FileExists(sDocName) = True Then
        ShellFile = Shell(Windows_Path & "notepad.exe" & " " & sDocName, vbNormalFocus)
    Else
        MsgBox "File not found", vbCritical + vbOKOnly
        Exit Sub
    End If
    Exit Sub
ViewOptimizationErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modViewDocument: Sub ViewOptimization")
End Sub

'CostCAt Files
Public Sub WriteCostCAtFiles(ByVal ProjectIndex As Integer, ByVal a As Boolean, ByVal b As Boolean, _
                            ByVal c As Boolean, ByVal d As Boolean)
    If a = False Then
        WriteDBInput ProjectIndex
    End If
    If b = False Then
        WriteDBAcces ProjectIndex
    End If
    If c = False Then
        WriteDBFractionnement ProjectIndex
    End If
    If d = False Then
        WriteDBSoudures ProjectIndex
    End If
End Sub

Private Sub WriteDBInput(ByVal ProjectIndex As Integer)
    On Error GoTo WriteDBInputErr
    Dim fso, f
    Dim sfile As String, i As Integer
    Set fso = CreateObject("Scripting.FileSystemObject")
    sfile = Project.Item(ProjectIndex).sFileName
    sfile = GetFilePath(sfile) & "dbinput.txt"
    Set f = fso.OpenTextFile(sfile, ForWriting, TristateUseDefault)
    Dim cPanel As cPanel
    f.WriteLine Project.Item(ProjectIndex).colPanel.Count
    f.WriteLine Project.Item(ProjectIndex).cHeader.cCostCAtMain.bReadFractionnement
    f.WriteLine Project.Item(ProjectIndex).cHeader.cCostCAtMain.bReadAccesibilite
    f.WriteLine Project.Item(ProjectIndex).cHeader.cCostCAtMain.bReadAtelier
    f.WriteLine Project.Item(ProjectIndex).cHeader.cCostCAtMain.iNAM
    f.WriteLine Project.Item(ProjectIndex).cHeader.cCostCAtMain.lPMB
    f.WriteLine Project.Item(ProjectIndex).cHeader.cCostCAtMain.lDiversTempsTolier & _
    vbTab & Project.Item(ProjectIndex).cHeader.cCostCAtMain.lDiversTempsSoudeur

    Dim sNCI As String, sNANP As String, sID As String, _
    sIT As String, sIP As String, sTypeTapes As String, _
    sPAbouts As String
    sNCI = "": sNANP = "": sID = "": sIT = "": sIP = ""
    sTypeTapes = "": sPAbouts = ""
    For Each cPanel In Project.Item(ProjectIndex).colPanel
        sNCI = sNCI & cPanel.cCostCAtMain.iNCI & vbTab
        sNANP = sNANP & cPanel.cCostCAtMain.iNANP & vbTab
        sID = sID & cPanel.cCostCAtMain.ID_PANNEAU & vbTab
        sIP = sIP & cPanel.cCostCAtMain.IP_PANNEAU & vbTab
        sIT = sIT & cPanel.cCostCAtMain.IT_PANNEAU & vbTab
        Select Case cPanel.cCostCAtMain.TypeTapes
            Case 0
                sTypeTapes = sTypeTapes & "0" & vbTab
            Case 1, 3
                sTypeTapes = sTypeTapes & "1" & vbTab
            Case 2, 4
                sTypeTapes = sTypeTapes & "2" & vbTab
        End Select
        'sTypeTapes = sTypeTapes & cPanel.cCostCAtMain.TypeTapes & vbTab
        sPAbouts = sPAbouts & cPanel.cCostCAtMain.PositionAboutsLisses & vbTab
    Next cPanel
    f.WriteLine sNCI
    f.WriteLine sNANP
    f.WriteLine sID
    f.WriteLine sIP
    f.WriteLine sIT
    f.WriteLine sTypeTapes
    f.WriteLine sPAbouts
    f.Close
'    Dim ShellFile As Long
'    If fso.FileExists(sFile) = True Then
'        ShellFile = Shell(Windows_Path & "notepad.exe" & " " & sFile, vbNormalFocus)
'    End If
    Exit Sub
WriteDBInputErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modViewDocument: Sub WriteDBInput")
End Sub

Private Sub WriteDBAcces(ByVal ProjectIndex As Integer)
    On Error GoTo WriteDBAccesErr
    Dim fso, f
    Dim sfile As String, i As Integer
    Set fso = CreateObject("Scripting.FileSystemObject")
    sfile = Project.Item(ProjectIndex).sFileName
    sfile = GetFilePath(sfile) & "dbacces.txt"
    Set f = fso.OpenTextFile(sfile, ForWriting, TristateUseDefault)
    Dim cPanel As cPanel
    f.WriteLine Project.Item(ProjectIndex).colPanel.Count
    f.WriteLine NO_OPERATIONS
    Dim s As String
    s = ""
    For i = 1 To NO_OPERATIONS
        For Each cPanel In Project.Item(ProjectIndex).colPanel
            s = s & (Format(Val_(cPanel.cCostCAtMain.colCostCAtOperations.Item(i).Accesibilite), "0.000")) & vbTab
        Next cPanel
        f.WriteLine s
        s = ""
    Next i
    s = ""
    For i = 1 To NO_OPERATIONS
        For Each cPanel In Project.Item(ProjectIndex).colPanel
            s = s & (Format(Val_(cPanel.cCostCAtMain.colCostCAtOperations.Item(i).Atelier), "0.000")) & vbTab
        Next cPanel
        f.WriteLine s
        s = ""
    Next i
    f.Close
'    Dim ShellFile As Long
'    If fso.FileExists(sFile) = True Then
'        ShellFile = Shell(Windows_Path & "notepad.exe" & " " & sFile, vbNormalFocus)
'    End If
    Exit Sub
WriteDBAccesErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modViewDocument: Sub WriteDBAcces")
End Sub

Private Sub WriteDBFractionnement(ByVal ProjectIndex As Integer)
    On Error GoTo WriteDBFractionnementErr
    Dim fso, f
    Dim sfile As String, i As Integer
    Set fso = CreateObject("Scripting.FileSystemObject")
    sfile = Project.Item(ProjectIndex).sFileName
    sfile = GetFilePath(sfile) & "dbfractionnement.txt"
    Set f = fso.OpenTextFile(sfile, ForWriting, TristateUseDefault)
    Dim cPanel As cPanel
    f.WriteLine Project.Item(ProjectIndex).colPanel.Count
    f.WriteLine NO_OPERATIONS
    Dim sOperation As String
    '----temp----
    Dim sPan As String
    sPan = vbTab
    For i = 1 To NO_OPERATIONS
        For Each cPanel In Project.Item(ProjectIndex).colPanel
            sOperation = sOperation & (Format(Val_(cPanel.cCostCAtMain.colCostCAtOperations.Item(i).Fractionnement), "0.000")) & vbTab
        Next cPanel
        f.WriteLine sOperation
        sOperation = ""
    Next i
    f.Close
'    Dim ShellFile As Long
'    If fso.FileExists(sFile) = True Then
'        ShellFile = Shell(Windows_Path & "notepad.exe" & " " & sFile, vbNormalFocus)
'    End If
    Exit Sub
WriteDBFractionnementErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modViewDocument: Sub WriteDBFractionnement")
End Sub

Private Sub WriteDBSoudures(ByVal ProjectIndex As Integer)
    On Error GoTo WriteDBSouduresErr
    Dim fso, f
    Dim sfile As String, i As Integer
    Set fso = CreateObject("Scripting.FileSystemObject")
    sfile = Project.Item(ProjectIndex).sFileName
    sfile = GetFilePath(sfile) & "dbsoudures.txt"
    Set f = fso.OpenTextFile(sfile, ForWriting, TristateUseDefault)
    Dim cPanel As cPanel
    f.WriteLine Project.Item(ProjectIndex).colPanel.Count
    f.WriteLine NO_OPERATIONS_SOUDURE
    Dim sOperation As String
    '----temp----
    Dim sPan As String
    sPan = vbTab
    Dim v(1 To 8) As Integer
    v(1) = 21
    v(2) = 22
    v(3) = 23
    v(4) = 36
    v(5) = 37
    v(6) = 47
    v(7) = 48
    v(8) = 51
    For i = 1 To NO_OPERATIONS_SOUDURE
        For Each cPanel In Project.Item(ProjectIndex).colPanel
            sOperation = sOperation & Val_(cPanel.cCostCAtMain.colCostCAtOperations.Item(v(i)).Soudures) & vbTab
        Next cPanel
        f.WriteLine sOperation
        sOperation = ""
    Next i
    For i = 1 To NO_OPERATIONS_SOUDURE
        For Each cPanel In Project.Item(ProjectIndex).colPanel
            sOperation = sOperation & Format(Val_(cPanel.cCostCAtMain.colCostCAtOperations.Item(v(i)).Gorges), "0.000") & vbTab
        Next cPanel
        f.WriteLine sOperation
        sOperation = ""
    Next i
    f.Close
'    Dim ShellFile As Long
'    If fso.FileExists(sFile) = True Then
'        ShellFile = Shell(Windows_Path & "notepad.exe" & " " & sFile, vbNormalFocus)
'    End If
    Exit Sub
WriteDBSouduresErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modViewDocument: Sub WriteDBSoudures")
End Sub
