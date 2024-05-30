Attribute VB_Name = "modMain"
Option Explicit
Public fMainForm As frmMain
Public sExpires As String
Dim lic_msg As String

Type T_Licensing
    LicenseLevel As Integer
    LicenseTag As String
    MAX_PANELS As Integer
    MAX_DESIGN_VARIABLES As Integer
    MAX_STRUCTURAL_CONSTRAINTS_ON_PANEL As Integer
    MAX_GEOMETRICAL_CONSTRAINTS_ON_PANEL As Integer
    MAX_EQUALITY_RESTRICTIONS As Integer
    MAX_ACTIVE_LOAD_CASES As Integer
    IS_COSTCAT As Boolean
    IS_LBR4 As Boolean
    IS_BEAM_THEORY As Boolean
    IS_IMPORT_MARS As Boolean
    IS_IMPORT_AVPRO As Boolean
    IS_ADD_PANEL As Boolean
    IS_DIVIDE_PANEL As Boolean
    IS_MODIFY_PANEL_NODES As Boolean
    IS_DELETE_PANEL As Boolean
    IS_MULTIPLE_SOLVE As Boolean
    IS_EDIT_NODES As Boolean
    IS_MODIFY_DESIGN_VARIABLES As Boolean
    IS_MODIFY_STRUCTURAL_CONSTRAINTS As Boolean
    IS_MODIFY_GEOMETRICAL_CONSTRAINTS As Boolean
    IS_MODIFY_EQUALITY_CONSTRAINTS As Boolean
End Type

Public Licensing As T_Licensing

Public Parser As cMathParser

Sub Main()

    getLicenseLevel
'    Licensing.LicenseLevel = 0
    setLicensing
        
    Set fMainForm = New frmMain
    fMainForm.Show
    InitMRU
    Toolbars_Initial
    Set Project = New colProject
    Set Parser = New cMathParser
    
    If Licensing.LicenseLevel = 7 Then 'demo version
        fMainForm.mnuFileNew.Enabled = False
        fMainForm.mnuFileImportFrom.Enabled = False
        fMainForm.mnuFileSaveAs.Enabled = False
        Dim i As Integer
        For i = 1 To fMainForm.mnuFileMRU.Count
            fMainForm.mnuFileMRU.Item(i).Enabled = False
        Next i
        'LoadDemo
        Exit Sub
    End If
    
    
'    Dim a As Double
'    Dim b As Double
'    Dim c As Double
'    Dim f As Double
'    Dim x As Double
'    Parser.StoreExpression ("a + c = 12")
'    Parser.Variable("a") = 5
'    'Parser.Variable("b") = 2
'    Parser.Variable("c") = 2
'    f = Parser.Eval

'    If GetCommandLine(sFile) = True Then
'        StartOpenBlock
'        Call launch
'        ReadSketch sFile
'        EndOpenBlock
'    End If
    Dim sFile As String
    If GetCommandLine(sFile) = True Then
        Project.OpenLBR5ASCIIFile CStr(sFile)
        AddtoMRU sFile
        UpdateRecentList
    End If
End Sub

Sub getLicenseLevel()
    On Error Resume Next
    Dim fso As New FileSystemObject
    Dim fil As file
    Dim ts As TextStream
    Dim sFile As String
    sFile = App.Path & "\license.lic"
    If fso.FileExists(sFile) = False Then
        'MsgBox "License File Not Found.", vbCritical + vbOKOnly
        Licensing.LicenseLevel = 7
        lic_msg = "License File Not Found. LBR-5 will start in demo mode."
        MsgBox lic_msg, vbInformation + vbOKOnly
        Exit Sub
        'End
    End If
    Set fil = fso.GetFile(sFile)
    Set ts = fil.OpenAsTextStream(ForReading)
    
    Dim slic As String
    slic = ts.ReadLine
    
    lic_msg = "License File Not Valid. LBR-5 will start in demo mode."
    If LicCompare(slic) = False Then
        'MsgBox "License File Not Valid.", vbCritical + vbOKOnly
        Licensing.LicenseLevel = 7
        MsgBox lic_msg, vbInformation + vbOKOnly
        Exit Sub
        'End
    End If
    
End Sub

Function LicCompare(ByRef slic As String) As Boolean
    Dim i As Integer
    Dim crypt As New CRijndael
    Dim sKey As String
    Licensing.LicenseLevel = -1
    sKey = crypt.JustDécrypter("D35920D40507DE7CBC10EE49FA167CC17F1CFB89CED49E824533CE2E6462CCEB0888F8237E690A471BF934ADE076078B0F27D2B3814BE25DB4583F50EDC1C821", "clef")
    Dim stime As String, stimecrypted As String
        
    Dim localcode As String
    Dim sMAC As String
    
    sMAC = GetMAC 'GetMACs_IfTable()
    sMAC = Trim(UCase(Replace(sMAC, "-", "")))
    sMAC = Replace(sMAC, ":", "")
    'get expiration
    Dim sExp As String
    sExp = crypt.JustDécrypter(slic, sKey)
'    Dim pos As Integer
'    pos = InStr(sExp, " ")
'    sExp = right(sExp, Len(sExp) - pos)
    sExp = right(sExp, 10)
    
    If CheckExpirationDate(sExp) = False Then
        Licensing.LicenseLevel = 7
        
        Exit Function
        'End
    End If
    
    Dim sLevel() As String
    ReDim sLevel(0 To 7)
    For i = 0 To UBound(sLevel)
        sLevel(i) = crypt.JustCrypter(sMAC + CStr(i) + sExp, sKey)
        If sLevel(i) = slic Then
            Licensing.LicenseLevel = i
            Exit For
        End If
    Next i
    
    If Licensing.LicenseLevel > -1 Then
        LicCompare = True
    Else
        LicCompare = False
    End If

End Function

Function CheckExpirationDate(ByVal sExp As String) As Boolean
    On Error GoTo CheckExpirationDateErr
    Dim sDay As String
    Dim sMonth As String
    Dim sYear As String
    Dim sdate As String
    Dim pos As Integer
    CheckExpirationDate = True 'valid
    sdate = Date
    sExpires = sExp
    
    pos = InStr(1, sdate, "/")
    sDay = Left(sdate, pos - 1)
    sdate = Mid(sdate, pos + 1)
    pos = InStr(1, sdate, "/")
    sMonth = Left(sdate, pos - 1)
    sdate = Mid(sdate, pos + 1)
    sYear = sdate
    
    Dim sEDay As String
    Dim sEMonth As String
    Dim sEYear As String

    pos = InStr(1, sExp, "/")
    sEDay = Left(sExp, pos - 1)
    sExp = Mid(sExp, pos + 1)
    pos = InStr(1, sExp, "/")
    sEMonth = Left(sExp, pos - 1)
    sExp = Mid(sExp, pos + 1)
    sEYear = sExp

    
    If CInt(sEYear) < CInt(sYear) Then
        CheckExpirationDate = False
    End If
    If CInt(sEYear) = CInt(sYear) And CInt(sEMonth) < CInt(sMonth) Then
        CheckExpirationDate = False
    End If
    If CInt(sEYear) = CInt(sYear) And CInt(sEMonth) = CInt(sMonth) And CInt(sEDay) <= CInt(sDay) Then
        CheckExpirationDate = False
    End If
    
    If CheckExpirationDate = False Then lic_msg = "Your license has expired. LBR-5 will start in demo mode."
    
    Exit Function
CheckExpirationDateErr:
    'MsgBox lic_msg, vbInformation + vbOKOnly '"License File Not Valid.", vbCritical + vbOKOnly
    'End
End Function

Sub setLicensing()
    '    MAX_PANELS As Integer
    '    MAX_DESIGN_VARIABLES As Integer
    '    MAX_STRUCTURAL_CONSTRAINTS_ON_PANEL As Integer
    '    MAX_GEOMETRICAL_CONSTRAINTS_ON_PANEL As Integer
    '    MAX_EQUALITY_RESTRICTIONS As Integer
    '    MAX_ACTIVE_LOAD_CASES As Integer
    '    IS_COSTCAT As Boolean
    '    IS_BEAM_THEORY As Boolean
    Licensing.MAX_DESIGN_VARIABLES = 1000
    
    With Licensing
        Select Case .LicenseLevel
            Case 0
                .LicenseTag = "0 - Professional (AKER)"
                .IS_LBR4 = True
                .IS_BEAM_THEORY = True
                .IS_COSTCAT = True
                .MAX_PANELS = 150
                .MAX_ACTIVE_LOAD_CASES = 10
                .MAX_STRUCTURAL_CONSTRAINTS_ON_PANEL = 20 * .MAX_ACTIVE_LOAD_CASES
                .MAX_GEOMETRICAL_CONSTRAINTS_ON_PANEL = 20
                .MAX_EQUALITY_RESTRICTIONS = 9 * .MAX_PANELS
                .IS_IMPORT_MARS = True
                .IS_IMPORT_AVPRO = True
                .IS_ADD_PANEL = True
                .IS_DIVIDE_PANEL = True
                .IS_MODIFY_PANEL_NODES = True
                .IS_DELETE_PANEL = True
                .IS_MULTIPLE_SOLVE = True
                .IS_EDIT_NODES = True
                .IS_MODIFY_DESIGN_VARIABLES = True
                .IS_MODIFY_STRUCTURAL_CONSTRAINTS = True
                .IS_MODIFY_GEOMETRICAL_CONSTRAINTS = True
                .IS_MODIFY_EQUALITY_CONSTRAINTS = True
            Case 1
                .LicenseTag = "1 - Professional"
                .IS_LBR4 = True
                .IS_BEAM_THEORY = True
                .IS_COSTCAT = False
                .MAX_PANELS = 150
                .MAX_ACTIVE_LOAD_CASES = 10
                .MAX_STRUCTURAL_CONSTRAINTS_ON_PANEL = 20 * .MAX_ACTIVE_LOAD_CASES
                .MAX_GEOMETRICAL_CONSTRAINTS_ON_PANEL = 20
                .MAX_EQUALITY_RESTRICTIONS = 9 * .MAX_PANELS
                .IS_IMPORT_MARS = True
                .IS_IMPORT_AVPRO = True
                .IS_ADD_PANEL = True
                .IS_DIVIDE_PANEL = True
                .IS_MODIFY_PANEL_NODES = True
                .IS_DELETE_PANEL = True
                .IS_MULTIPLE_SOLVE = True
                .IS_EDIT_NODES = True
                .IS_MODIFY_DESIGN_VARIABLES = True
                .IS_MODIFY_STRUCTURAL_CONSTRAINTS = True
                .IS_MODIFY_GEOMETRICAL_CONSTRAINTS = True
                .IS_MODIFY_EQUALITY_CONSTRAINTS = True
            Case 2
                .LicenseTag = "2 - Professional"
                .IS_LBR4 = False
                .IS_BEAM_THEORY = True
                .IS_COSTCAT = False
                .MAX_PANELS = 150
                .MAX_ACTIVE_LOAD_CASES = 10
                .MAX_STRUCTURAL_CONSTRAINTS_ON_PANEL = 20 * .MAX_ACTIVE_LOAD_CASES
                .MAX_GEOMETRICAL_CONSTRAINTS_ON_PANEL = 20
                .MAX_EQUALITY_RESTRICTIONS = 9 * .MAX_PANELS
                .IS_IMPORT_MARS = True
                .IS_IMPORT_AVPRO = True
                .IS_ADD_PANEL = True
                .IS_DIVIDE_PANEL = True
                .IS_MODIFY_PANEL_NODES = True
                .IS_DELETE_PANEL = True
                .IS_MULTIPLE_SOLVE = True
                .IS_EDIT_NODES = True
                .IS_MODIFY_DESIGN_VARIABLES = True
                .IS_MODIFY_STRUCTURAL_CONSTRAINTS = True
                .IS_MODIFY_GEOMETRICAL_CONSTRAINTS = True
                .IS_MODIFY_EQUALITY_CONSTRAINTS = True
            Case 3
                .LicenseTag = "3 - Professional"
                .IS_LBR4 = True
                .IS_BEAM_THEORY = False
                .IS_COSTCAT = False
                .MAX_PANELS = 150
                .MAX_ACTIVE_LOAD_CASES = 10
                .MAX_STRUCTURAL_CONSTRAINTS_ON_PANEL = 20 * .MAX_ACTIVE_LOAD_CASES
                .MAX_GEOMETRICAL_CONSTRAINTS_ON_PANEL = 20
                .MAX_EQUALITY_RESTRICTIONS = 9 * .MAX_PANELS
                .IS_IMPORT_MARS = True
                .IS_IMPORT_AVPRO = True
                .IS_ADD_PANEL = True
                .IS_DIVIDE_PANEL = True
                .IS_MODIFY_PANEL_NODES = True
                .IS_DELETE_PANEL = True
                .IS_MULTIPLE_SOLVE = True
                .IS_EDIT_NODES = True
                .IS_MODIFY_DESIGN_VARIABLES = True
                .IS_MODIFY_STRUCTURAL_CONSTRAINTS = True
                .IS_MODIFY_GEOMETRICAL_CONSTRAINTS = True
                .IS_MODIFY_EQUALITY_CONSTRAINTS = True
            Case 4
                .LicenseTag = "4 - Academic"
                .IS_LBR4 = True
                .IS_BEAM_THEORY = True
                .IS_COSTCAT = False
                .MAX_PANELS = 150
                .MAX_ACTIVE_LOAD_CASES = 10
                .MAX_STRUCTURAL_CONSTRAINTS_ON_PANEL = 20 * .MAX_ACTIVE_LOAD_CASES
                .MAX_GEOMETRICAL_CONSTRAINTS_ON_PANEL = 20
                .MAX_EQUALITY_RESTRICTIONS = 9 * .MAX_PANELS
                .IS_IMPORT_MARS = True
                .IS_IMPORT_AVPRO = True
                .IS_ADD_PANEL = True
                .IS_DIVIDE_PANEL = True
                .IS_MODIFY_PANEL_NODES = True
                .IS_DELETE_PANEL = True
                .IS_MULTIPLE_SOLVE = True
                .IS_EDIT_NODES = True
                .IS_MODIFY_DESIGN_VARIABLES = True
                .IS_MODIFY_STRUCTURAL_CONSTRAINTS = True
                .IS_MODIFY_GEOMETRICAL_CONSTRAINTS = True
                .IS_MODIFY_EQUALITY_CONSTRAINTS = True
            Case 5
                .LicenseTag = "5 - Academic"
                .IS_LBR4 = False
                .IS_BEAM_THEORY = True
                .IS_COSTCAT = False
                .MAX_PANELS = 150
                .MAX_ACTIVE_LOAD_CASES = 10
                .MAX_STRUCTURAL_CONSTRAINTS_ON_PANEL = 20 * .MAX_ACTIVE_LOAD_CASES
                .MAX_GEOMETRICAL_CONSTRAINTS_ON_PANEL = 20
                .MAX_EQUALITY_RESTRICTIONS = 9 * .MAX_PANELS
                .IS_IMPORT_MARS = True
                .IS_IMPORT_AVPRO = True
                .IS_ADD_PANEL = True
                .IS_DIVIDE_PANEL = True
                .IS_MODIFY_PANEL_NODES = True
                .IS_DELETE_PANEL = True
                .IS_MULTIPLE_SOLVE = True
                .IS_EDIT_NODES = True
                .IS_MODIFY_DESIGN_VARIABLES = True
                .IS_MODIFY_STRUCTURAL_CONSTRAINTS = True
                .IS_MODIFY_GEOMETRICAL_CONSTRAINTS = True
                .IS_MODIFY_EQUALITY_CONSTRAINTS = True
            Case 6
                .LicenseTag = "6 - Student"
                .IS_LBR4 = False
                .IS_BEAM_THEORY = True
                .IS_COSTCAT = False
                .MAX_PANELS = 10
                .MAX_ACTIVE_LOAD_CASES = 2
                .MAX_STRUCTURAL_CONSTRAINTS_ON_PANEL = 10
                .MAX_GEOMETRICAL_CONSTRAINTS_ON_PANEL = 10
                .MAX_EQUALITY_RESTRICTIONS = 9 * .MAX_PANELS
                .IS_IMPORT_MARS = True
                .IS_IMPORT_AVPRO = False
                .IS_ADD_PANEL = True
                .IS_DIVIDE_PANEL = True
                .IS_MODIFY_PANEL_NODES = True
                .IS_DELETE_PANEL = True
                .IS_MULTIPLE_SOLVE = False
                .IS_EDIT_NODES = True
                .IS_MODIFY_DESIGN_VARIABLES = True
                .IS_MODIFY_STRUCTURAL_CONSTRAINTS = True
                .IS_MODIFY_GEOMETRICAL_CONSTRAINTS = True
                .IS_MODIFY_EQUALITY_CONSTRAINTS = True
            Case 7
                .LicenseTag = "7 - Demo"
                .IS_LBR4 = True
                .IS_BEAM_THEORY = False
                .IS_COSTCAT = False
                .MAX_PANELS = 20
                .MAX_ACTIVE_LOAD_CASES = 2
                .MAX_STRUCTURAL_CONSTRAINTS_ON_PANEL = 10
                .MAX_GEOMETRICAL_CONSTRAINTS_ON_PANEL = 5
                .MAX_EQUALITY_RESTRICTIONS = 9 * .MAX_PANELS
                .IS_IMPORT_MARS = False
                .IS_IMPORT_AVPRO = False
                .IS_ADD_PANEL = False
                .IS_DIVIDE_PANEL = False
                .IS_MODIFY_PANEL_NODES = False
                .IS_DELETE_PANEL = False
                .IS_MULTIPLE_SOLVE = False
                .IS_EDIT_NODES = False
                .IS_MODIFY_DESIGN_VARIABLES = True 'False
                .IS_MODIFY_STRUCTURAL_CONSTRAINTS = True 'False
                .IS_MODIFY_GEOMETRICAL_CONSTRAINTS = True 'False
                .IS_MODIFY_EQUALITY_CONSTRAINTS = True 'False
            Case Else
        End Select
    End With
End Sub

Public Sub Toolbars_Initial()
    Unload tb_Solver
    Unload tb_Geometry
    Unload tb_View
    Unload tb_Standard
    Unload tb_Tools
    tb_Initial.Show
    If tb_Initial.Visible = True Then
        fMainForm.mnuViewToolbarsStandard.Checked = True
    Else
        fMainForm.mnuViewToolbarsStandard.Checked = False
    End If
End Sub

Public Sub Toolbars()
    Unload tb_Initial
    tb_Tools.Show
    tb_Solver.FDPane1.SetLayoutReference tb_Tools.FDPane1
    tb_Solver.Show
    tb_Geometry.FDPane1.SetLayoutReference tb_Solver.FDPane1
    tb_Geometry.Show
    tb_View.FDPane1.SetLayoutReference tb_Geometry.FDPane1
    tb_View.Show
    tb_Standard.FDPane1.SetLayoutReference tb_View.FDPane1
    tb_Standard.Show
    
    Dim cProject As cProject
    For Each cProject In Project
        cProject.frmProject.mnuViewToolbarsSolver.Checked = True
        cProject.frmProject.mnuViewToolbarsGeometry.Checked = True
        cProject.frmProject.mnuViewToolbarsView.Checked = True
        cProject.frmProject.mnuViewToolbarsStandard.Checked = True
        cProject.frmProject.mnuViewToolbarsTools.Checked = True
    Next cProject
End Sub

Sub LoadResStrings(frm As Form)
    'On Error Resume Next
'    On Error GoTo  LoadResStringsErr
'    Dim ctl As Control
'    Dim obj As Object
'    Dim fnt As Object
'    Dim sCtlType As String
'    Dim nVal As Integer
'    'set the form's caption
'    frm.Caption = LoadResString(CInt(frm.Tag))
'    'set the font
'    Set fnt = frm.Font
'    fnt.Name = LoadResString(20)
'    fnt.size = CInt(LoadResString(21))
'    'set the controls' captions using the caption
'    'property for menu items and the Tag property
'    'for all other controls
'    For Each ctl In frm.Controls
'        Set ctl.Font = fnt
'        sCtlType = TypeName(ctl)
'        If sCtlType = "Label" Then
'            ctl.Caption = LoadResString(CInt(ctl.Tag))
'        ElseIf sCtlType = "Menu" Then
'            ctl.Caption = LoadResString(CInt(ctl.Caption))
'        ElseIf sCtlType = "TabStrip" Then
'            For Each obj In ctl.Tabs
'                obj.Caption = LoadResString(CInt(obj.Tag))
'                obj.ToolTipText = LoadResString(CInt(obj.ToolTipText))
'            Next
'        ElseIf sCtlType = "Toolbar" Then
'            For Each obj In ctl.Buttons
'                obj.ToolTipText = LoadResString(CInt(obj.ToolTipText))
'            Next
'        ElseIf sCtlType = "ListView" Then
'            For Each obj In ctl.ColumnHeaders
'                obj.Text = LoadResString(CInt(obj.Tag))
'            Next
'        Else
'            nVal = 0
'            nVal = Val(ctl.Tag)
'            If nVal > 0 Then ctl.Caption = LoadResString(nVal)
'            nVal = 0
'            nVal = Val(ctl.ToolTipText)
'            If nVal > 0 Then ctl.ToolTipText = LoadResString(nVal)
'        End If
'    Next
'Exit Sub
'LoadResStringsErr:
'Call RaiseError(MyUnhandledError,Err.Description & "." & vbCrLf &  "modMain: Sub LoadResStrings")
End Sub

