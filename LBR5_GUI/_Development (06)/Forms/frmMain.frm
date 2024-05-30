VERSION 5.00
Object = "{714D09E3-B193-11D3-A192-00A0CC26207F}#1.0#0"; "dftlbV1.dll"
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "comdlg32.ocx"
Object = "{831FDD16-0C5C-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCTL.OCX"
Object = "{38911DA0-E448-11D0-84A3-00DD01104159}#1.1#0"; "comct332.ocx"
Begin VB.MDIForm frmMain 
   Appearance      =   0  'Flat
   BackColor       =   &H00808080&
   Caption         =   "LBR-5"
   ClientHeight    =   7380
   ClientLeft      =   3870
   ClientTop       =   3090
   ClientWidth     =   9660
   Icon            =   "frmMain.frx":0000
   LinkTopic       =   "MDIForm1"
   WindowState     =   2  'Maximized
   Begin VB.PictureBox picMRU 
      Align           =   1  'Align Top
      Height          =   1095
      Left            =   0
      ScaleHeight     =   1035
      ScaleWidth      =   9600
      TabIndex        =   3
      Top             =   390
      Visible         =   0   'False
      Width           =   9660
      Begin VB.ListBox lstbuffMRU 
         Height          =   1035
         Left            =   4680
         TabIndex        =   5
         Top             =   0
         Width           =   4935
      End
      Begin VB.ListBox lstMRU 
         Height          =   1035
         Left            =   0
         TabIndex        =   4
         Top             =   0
         Width           =   4695
      End
   End
   Begin MSComDlg.CommonDialog dlgCommonDialog 
      Left            =   120
      Top             =   1560
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
   End
   Begin MSComctlLib.ImageList imlToolbarIcons 
      Left            =   120
      Top             =   2040
      _ExtentX        =   1005
      _ExtentY        =   1005
      BackColor       =   -2147483643
      ImageWidth      =   16
      ImageHeight     =   16
      MaskColor       =   12632256
      _Version        =   393216
      BeginProperty Images {2C247F25-8591-11D1-B16A-00C0F0283628} 
         NumListImages   =   8
         BeginProperty ListImage1 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmMain.frx":0CCA
            Key             =   "New"
         EndProperty
         BeginProperty ListImage2 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmMain.frx":0DDC
            Key             =   "Open"
         EndProperty
         BeginProperty ListImage3 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmMain.frx":0EEE
            Key             =   "Save"
         EndProperty
         BeginProperty ListImage4 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmMain.frx":1000
            Key             =   ""
         EndProperty
         BeginProperty ListImage5 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmMain.frx":1112
            Key             =   ""
         EndProperty
         BeginProperty ListImage6 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmMain.frx":1224
            Key             =   ""
         EndProperty
         BeginProperty ListImage7 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmMain.frx":1336
            Key             =   ""
         EndProperty
         BeginProperty ListImage8 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "frmMain.frx":1448
            Key             =   ""
         EndProperty
      EndProperty
   End
   Begin ComCtl3.CoolBar CoolBar 
      Align           =   1  'Align Top
      Height          =   390
      Left            =   0
      Negotiate       =   -1  'True
      TabIndex        =   0
      Top             =   0
      Visible         =   0   'False
      Width           =   9660
      _ExtentX        =   17039
      _ExtentY        =   688
      BandCount       =   2
      EmbossPicture   =   -1  'True
      _CBWidth        =   9660
      _CBHeight       =   390
      _Version        =   "6.7.9782"
      Child1          =   "tbToolBar"
      MinWidth1       =   1095
      MinHeight1      =   330
      Width1          =   1095
      FixedBackground1=   0   'False
      NewRow1         =   0   'False
      Child2          =   "tbToolBarScreen"
      MinWidth2       =   1815
      MinHeight2      =   330
      Width2          =   1815
      FixedBackground2=   0   'False
      NewRow2         =   0   'False
      Begin MSComctlLib.Toolbar tbToolBar 
         Height          =   330
         Left            =   165
         Negotiate       =   -1  'True
         TabIndex        =   2
         Top             =   30
         Width           =   1095
         _ExtentX        =   1931
         _ExtentY        =   582
         ButtonWidth     =   609
         ButtonHeight    =   582
         Style           =   1
         ImageList       =   "imlToolbarIcons"
         _Version        =   393216
         BeginProperty Buttons {66833FE8-8583-11D1-B16A-00C0F0283628} 
            NumButtons      =   3
            BeginProperty Button1 {66833FEA-8583-11D1-B16A-00C0F0283628} 
               Key             =   "New"
               Object.ToolTipText     =   "New"
               ImageIndex      =   1
            EndProperty
            BeginProperty Button2 {66833FEA-8583-11D1-B16A-00C0F0283628} 
               Key             =   "Open"
               Object.ToolTipText     =   "Open"
               ImageIndex      =   2
            EndProperty
            BeginProperty Button3 {66833FEA-8583-11D1-B16A-00C0F0283628} 
               Key             =   "Save"
               Object.ToolTipText     =   "Save"
               ImageIndex      =   3
            EndProperty
         EndProperty
      End
      Begin MSComctlLib.Toolbar tbToolBarScreen 
         Height          =   330
         Left            =   1485
         TabIndex        =   1
         Top             =   30
         Width           =   8085
         _ExtentX        =   14261
         _ExtentY        =   582
         ButtonWidth     =   609
         ButtonHeight    =   582
         Style           =   1
         ImageList       =   "imlToolbarIcons"
         _Version        =   393216
         BeginProperty Buttons {66833FE8-8583-11D1-B16A-00C0F0283628} 
            NumButtons      =   5
            BeginProperty Button1 {66833FEA-8583-11D1-B16A-00C0F0283628} 
               Key             =   "ZoomIn"
               Object.ToolTipText     =   "Zoom In"
               ImageIndex      =   4
            EndProperty
            BeginProperty Button2 {66833FEA-8583-11D1-B16A-00C0F0283628} 
               Key             =   "ZoomOut"
               Object.ToolTipText     =   "Zoom Out"
               ImageIndex      =   5
            EndProperty
            BeginProperty Button3 {66833FEA-8583-11D1-B16A-00C0F0283628} 
               Key             =   "ZoomWin"
               Object.ToolTipText     =   "Zoom Win"
               ImageIndex      =   6
            EndProperty
            BeginProperty Button4 {66833FEA-8583-11D1-B16A-00C0F0283628} 
               Key             =   "ZoomFull"
               Object.ToolTipText     =   "Zoom Full"
               ImageIndex      =   7
            EndProperty
            BeginProperty Button5 {66833FEA-8583-11D1-B16A-00C0F0283628} 
               Key             =   "Pan"
               Object.ToolTipText     =   "Pan"
               ImageIndex      =   8
            EndProperty
         EndProperty
      End
   End
   Begin XDOCKFLOATLibCtl.DockFrame DockFrame1 
      Left            =   360
      Top             =   960
      _cx             =   688
      _cy             =   688
      DragAreaStyle   =   0
      PICTCNT         =   0
      MENUCNT         =   0
   End
   Begin VB.Menu mnuFile 
      Caption         =   "&File"
      Begin VB.Menu mnuFileNew 
         Caption         =   "&New"
         Shortcut        =   ^N
      End
      Begin VB.Menu mnuFileOpen 
         Caption         =   "&Open..."
         Shortcut        =   ^O
      End
      Begin VB.Menu mnuFileClose 
         Caption         =   "&Close"
         Visible         =   0   'False
      End
      Begin VB.Menu mnuFileBar0 
         Caption         =   "-"
      End
      Begin VB.Menu mnuFileSave 
         Caption         =   "&Save"
         Visible         =   0   'False
      End
      Begin VB.Menu mnuFileSaveAs 
         Caption         =   "Save &As..."
         Visible         =   0   'False
      End
      Begin VB.Menu mnuFileSaveAll 
         Caption         =   "Save A&ll"
         Visible         =   0   'False
      End
      Begin VB.Menu mnuFileBar1 
         Caption         =   "-"
         Visible         =   0   'False
      End
      Begin VB.Menu mnuFileImportFrom 
         Caption         =   "Import From..."
         Begin VB.Menu mnuFileImportFromMarsLbr5TransfertFile 
            Caption         =   "MARS-LBR5 Transfert File"
         End
         Begin VB.Menu mnuFileImportFromAvproLbr5TransfertFile 
            Caption         =   "AVPRO-LBR5 Transfert File"
         End
      End
      Begin VB.Menu mnuFileBar2 
         Caption         =   "-"
      End
      Begin VB.Menu mnuFileProperties 
         Caption         =   "Propert&ies"
         Visible         =   0   'False
      End
      Begin VB.Menu mnuFileBar3 
         Caption         =   "-"
         Visible         =   0   'False
      End
      Begin VB.Menu mnuFilePageSetup 
         Caption         =   "Page Set&up"
         Visible         =   0   'False
      End
      Begin VB.Menu mnuFilePrintPreview 
         Caption         =   "Print Pre&view"
         Visible         =   0   'False
      End
      Begin VB.Menu mnuFilePrint 
         Caption         =   "&Print"
         Visible         =   0   'False
      End
      Begin VB.Menu mnuFileBar4 
         Caption         =   "-"
         Visible         =   0   'False
      End
      Begin VB.Menu mnuFileSend 
         Caption         =   "Sen&d..."
         Visible         =   0   'False
      End
      Begin VB.Menu mnuFileBar5 
         Caption         =   "-"
         Visible         =   0   'False
      End
      Begin VB.Menu mnuFileMRU 
         Caption         =   ""
         Index           =   1
         Visible         =   0   'False
      End
      Begin VB.Menu mnuFileMRU 
         Caption         =   ""
         Index           =   2
         Visible         =   0   'False
      End
      Begin VB.Menu mnuFileMRU 
         Caption         =   ""
         Index           =   3
         Visible         =   0   'False
      End
      Begin VB.Menu mnuFileMRU 
         Caption         =   ""
         Index           =   4
         Visible         =   0   'False
      End
      Begin VB.Menu mnuFileMRU 
         Caption         =   ""
         Index           =   5
         Visible         =   0   'False
      End
      Begin VB.Menu mnuFileMRU 
         Caption         =   ""
         Index           =   6
         Visible         =   0   'False
      End
      Begin VB.Menu mnuFileMRU 
         Caption         =   ""
         Index           =   7
         Visible         =   0   'False
      End
      Begin VB.Menu mnuFileMRU 
         Caption         =   ""
         Index           =   8
         Visible         =   0   'False
      End
      Begin VB.Menu mnuFileMRU 
         Caption         =   ""
         Index           =   9
         Visible         =   0   'False
      End
      Begin VB.Menu mnuFileMRU 
         Caption         =   ""
         Index           =   10
         Visible         =   0   'False
      End
      Begin VB.Menu mnuFileBar6 
         Caption         =   "-"
         Visible         =   0   'False
      End
      Begin VB.Menu mnuFileExit 
         Caption         =   "E&xit"
      End
   End
   Begin VB.Menu mnuEdit 
      Caption         =   "&Edit"
      Visible         =   0   'False
      Begin VB.Menu mnuEditUndo 
         Caption         =   "&Undo"
      End
      Begin VB.Menu mnuEditBar0 
         Caption         =   "-"
      End
      Begin VB.Menu mnuEditCut 
         Caption         =   "Cu&t"
         Shortcut        =   ^X
      End
      Begin VB.Menu mnuEditCopy 
         Caption         =   "&Copy"
         Shortcut        =   ^C
      End
      Begin VB.Menu mnuEditPaste 
         Caption         =   "&Paste"
         Shortcut        =   ^V
      End
      Begin VB.Menu mnuEditPasteSpecial 
         Caption         =   "Paste &Special"
      End
   End
   Begin VB.Menu mnuView 
      Caption         =   "&View"
      Begin VB.Menu mnuViewProject 
         Caption         =   "&Project"
         Visible         =   0   'False
         Begin VB.Menu mnuViewProjectSolutionFile1 
            Caption         =   "Solution File1"
         End
         Begin VB.Menu mnuViewProjectSolutionFile2 
            Caption         =   "Solution File2"
         End
         Begin VB.Menu mnuViewProjectOptimizationFile 
            Caption         =   "Optimization File"
         End
      End
      Begin VB.Menu mnuViewRefresh 
         Caption         =   "&Refresh"
         Visible         =   0   'False
      End
      Begin VB.Menu mnuViewOptions 
         Caption         =   "&Options..."
         Visible         =   0   'False
      End
      Begin VB.Menu mnuViewWebBrowser 
         Caption         =   "&Web Browser"
      End
      Begin VB.Menu mnuViewBar0 
         Caption         =   "-"
      End
      Begin VB.Menu mnuViewToolbars 
         Caption         =   "&Toolbars"
         Begin VB.Menu mnuViewToolbarsStandard 
            Caption         =   "&Standard"
         End
      End
      Begin VB.Menu mnuViewStatusBar 
         Caption         =   "Status &Bar"
         Checked         =   -1  'True
         Visible         =   0   'False
      End
   End
   Begin VB.Menu mnuProject 
      Caption         =   "&Project"
      Visible         =   0   'False
      Begin VB.Menu mnuProjectTitle 
         Caption         =   "&Title"
      End
      Begin VB.Menu mnuProjectOptions 
         Caption         =   "&Options"
         Begin VB.Menu mnuProjectOptionsOverallSpan 
            Caption         =   "Overall &Span"
         End
         Begin VB.Menu mnuProjectOptionsOptimization 
            Caption         =   "&Optimization"
         End
         Begin VB.Menu mnuProjectOptionsDeadweight 
            Caption         =   "&Deadweight"
         End
         Begin VB.Menu mnuProjectOptionsFourierSeries 
            Caption         =   "&Fourier Series"
         End
         Begin VB.Menu mnuProjectOptionsOutput 
            Caption         =   "Out&put"
         End
      End
   End
   Begin VB.Menu mnuLoads 
      Caption         =   "&Loads"
      Visible         =   0   'False
      Begin VB.Menu mnuLoadsLoadCases 
         Caption         =   "&Load Cases"
      End
      Begin VB.Menu mnuLoadsLateralPressuers 
         Caption         =   "Lateral &Pressures"
         Begin VB.Menu mnuLoadsUniformelyDistributed 
            Caption         =   "&Uniformely Distributed"
         End
         Begin VB.Menu mnuLoadsStepwiseDistributed 
            Caption         =   "&Stepwise Distributed"
         End
      End
      Begin VB.Menu mnuLoadsLocalizedPressures 
         Caption         =   "Locali&zed Pressures"
      End
      Begin VB.Menu mnuLoadsBendingMoments 
         Caption         =   "Bending &Moments"
      End
   End
   Begin VB.Menu mnuTools 
      Caption         =   "&Tools"
      Visible         =   0   'False
      Begin VB.Menu mnuToolsCheckModel 
         Caption         =   "&Check Model"
      End
      Begin VB.Menu mnuToolsBar0 
         Caption         =   "-"
      End
      Begin VB.Menu mnuToolsOptions 
         Caption         =   "&Options..."
      End
   End
   Begin VB.Menu mnuWindow 
      Caption         =   "&Window"
      Visible         =   0   'False
      WindowList      =   -1  'True
      Begin VB.Menu mnuWindowNewWindow 
         Caption         =   "&New Window"
         Visible         =   0   'False
      End
      Begin VB.Menu mnuWindowBar0 
         Caption         =   "-"
         Visible         =   0   'False
      End
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
   Begin VB.Menu mnuHelp 
      Caption         =   "&Help"
      Begin VB.Menu mnuHelpContents 
         Caption         =   "&Contents"
      End
      Begin VB.Menu mnuHelpTutorial 
         Caption         =   "&Tutorial"
      End
      Begin VB.Menu mnuHelpSearchForHelpOn 
         Caption         =   "&Search For Help On..."
         Visible         =   0   'False
      End
      Begin VB.Menu mnuHelpBar0 
         Caption         =   "-"
      End
      Begin VB.Menu mnuHelpAbout 
         Caption         =   "&About"
      End
   End
End
Attribute VB_Name = "frmMain"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Declare Function SendMessage Lib "user32" Alias "SendMessageA" (ByVal hwnd As Long, ByVal wMsg As Long, ByVal wParam As Long, ByVal lParam As Any) As Long
Const EM_UNDO = &HC7
Private Declare Function OSWinHelp% Lib "user32" Alias "WinHelpA" (ByVal hwnd&, ByVal HelpFile$, ByVal wCommand%, dwData As Any)
'Private mvarcToolbars As New cToolbars
'
'Public Property Set cToolbars(vData As cToolbars)
'    Set mvarcToolbars = cToolbars
'End Property
'
'Public Property Get cToolbars() As cToolbars
'    Set cToolbars = mvarcToolbars
'End Property

Private Sub MDIForm_Initialize()
    Me.Width = 10000
    Me.Height = 8000
    
    'Import MARS
    Me.mnuFileImportFromMarsLbr5TransfertFile.Enabled = Licensing.IS_IMPORT_MARS
    'Import AVPRO
    Me.mnuFileImportFromAvproLbr5TransfertFile.Enabled = Licensing.IS_IMPORT_AVPRO
End Sub

Private Sub MDIForm_Load()
    LoadResStrings Me
    'Me.Icon = LoadResPicture("ID_ICON_LOGO", 1)
    Me.Left = GetSetting(App.Title, "Settings", "MainLeft", 1000)
    Me.Top = GetSetting(App.Title, "Settings", "MainTop", 1000)
    Me.Width = GetSetting(App.Title, "Settings", "MainWidth", 6500)
    Me.Height = GetSetting(App.Title, "Settings", "MainHeight", 6500)
End Sub

'Private Sub LoadNewProject()
'    lProjectCount = lProjectCount + 1
'    ActiveProject = lProjectCount
'    Dim p As cProject
'    Set p = New cProject
'    p.index = lProjectCount
'    Project.Add p, lProjectCount
'    Config lProjectCount
'    Project.Item(lProjectCount).frmProject.Tag = lProjectCount
'    Project.Item(lProjectCount).frmProject.Caption = "Project" & lProjectCount & ".txt"
'    Project.Item(lProjectCount).sFileName = App.Path & "\Project" & lProjectCount & ".txt"
'    Project.Item(lProjectCount).frmProject.Show
'    ReadConfig
'
'End Sub

Private Sub MDIForm_QueryUnload(Cancel As Integer, UnloadMode As Integer)
   
'    Dim msg As VbMsgBoxResult
'    Dim lActiveProject As Integer
'    lActiveProject = ActiveProject
'    Dim cProject As cProject
'    For Each cProject In Project
'        If Project.Item(cProject.index).DataChanged = True Then
'            msg = MsgBox("Save changes on" & vbCrLf & Project.Item(cProject.index).sFileName & "?", vbYesNoCancel)
'            Select Case msg
'                Case vbYes
'                    ActiveProject = cProject.index
'                    fMainForm.mnuFileSave_Click
'                    Project.Remove (CInt(cProject.index))
'                Case vbNo
'                    Project.Remove (CInt(cProject.index))
'                Case vbCancel
'                    Cancel = 1
'            End Select
'        Else
'            Project.Remove (CInt(cProject.index))
'        End If
'    Next cProject
'    ActiveProject = lActiveProject
End Sub

Private Sub MDIForm_Unload(Cancel As Integer)
    If Me.WindowState <> vbMinimized Then
        SaveSetting App.Title, "Settings", "MainLeft", Me.Left
        SaveSetting App.Title, "Settings", "MainTop", Me.Top
        SaveSetting App.Title, "Settings", "MainWidth", Me.Width
        SaveSetting App.Title, "Settings", "MainHeight", Me.Height
    End If
    Unload tb_Initial
    Unload tb_Standard
    Unload tb_View
    Unload tb_Geometry
    Unload tb_Solver
    Unload tb_Tools
    'End
End Sub

Public Sub mnuFileImportFromAvproLbr5TransfertFile_Click()
'    Dim oProject As New cAVPROProject
'    Dim iNoOfFile As Long
'    Dim sFileName As String
    dlgCommonDialog.FileName = ""
        With dlgCommonDialog
            .DialogTitle = "Import AVPRO file"
            .CancelError = False
            .Filter = "XML Files (*.xml)|*.xml"
            .FileName = ""
            .ShowOpen
            If Len(.FileName) = 0 Then
                Exit Sub
            End If
            'sFileName = .FileName
            sFile = .FileName
        End With
        
        Project.OpenAvproLbr5TransfertFile sFile
        
'        iNoOfFile = FreeFile(0)
'        Open sFileName For Input As iNoOfFile
'        oProject.readXML (iNoOfFile)
'        Close #iNoOfFile
'
'        Project.OpenProject
'
'        Project.Item(lProjectCount).frmProject.Tag = lProjectCount
'        Project.Item(lProjectCount).frmProject.Caption = GetFileName(sFileName)
'        Project.Item(lProjectCount).sFileName = sFile
'        Read_AVPRO oProject
'
'        ZoomFull
'        setFunctionMode NO_FUNCTION
'        setScreenMode NORMAL_MODE
        
        AddtoMRU (sFile)
        UpdateRecentList
        Exit Sub

'        Call launch
        
'        Call ByDefaultParameters ' AVPRO_DATA module
'        Call NodeGenerator(header.NETO)
'        Call GenNoh
        

End Sub

Public Sub mnuFileMRU_Click(index As Integer)
'
    Dim i As Integer
    Dim ext As String
    ext = right(lstMRU.List(index - 1), 3)
    
    Dim fso As New FileSystemObject
    If fso.FileExists(lstMRU.List(index - 1)) = False Then
        MsgBox "File '" & lstMRU.List(index - 1) & "'" & "not found.", vbExclamation + vbOKOnly
        'faire disparaitre dans le menu
        RemoveFromMRU lstMRU.List(index - 1)
        Exit Sub
    End If
    
    Select Case ext
        Case "txt"
            Project.OpenLBR5TextFile lstMRU.List(index - 1)
        Case "lbr"
            Project.OpenLBR5ASCIIFile lstMRU.List(index - 1)
        Case "mlt"
            Project.OpenMARS_LBR5TransfertFile lstMRU.List(index - 1)
        Case "xml"
            Project.OpenAvproLbr5TransfertFile lstMRU.List(index - 1)
    End Select
    AddtoMRU lstMRU.List(index - 1)
    UpdateRecentList
    
End Sub

Public Sub mnuHelpTutorial_Click()
    Dim sFile As String
    Dim ShellHelp As Long
    sFile = App.Path & "\" & "Tutorial LBR5.chm"
    ShellHelp = ShellExecute(0, "open", sFile, "", "", SW_SHOWNORMAL)
End Sub

Private Sub mnuLoadsLoadCases_Click()
    Project.Item(ActiveProject).frmLoadCases.Show vbModeless, Me
End Sub

Private Sub mnuProjectOptionsDeadweight_Click()
    Project.Item(ActiveProject).frmDeadweight.Show vbModeless, Me
End Sub

Private Sub mnuProjectOptionsFourierSeries_Click()
    Project.Item(ActiveProject).frmFourierSeries.Show vbModeless, Me
End Sub

Private Sub mnuProjectOptionsOptimization_Click()
    Project.Item(ActiveProject).frmOptimization.Show vbModeless, Me
End Sub

Private Sub mnuProjectOptionsOutput_Click()
    Project.Item(ActiveProject).frmOutput.Show vbModeless, Me
End Sub

Private Sub mnuProjectOptionsOverallSpan_Click()
    Project.Item(ActiveProject).frmOverallSpan.Show vbModeless, Me
End Sub

Private Sub mnuProjectTitle_Click()
    Project.Item(ActiveProject).frmTitle.Show vbModeless, Me
End Sub

Private Sub mnuViewProjectOptimizationFile_Click()
    ViewOptimization Project.Item(ActiveProject).sFileName
End Sub

Private Sub mnuViewProjectSolutionFile1_Click()
    ViewSolution1 Project.Item(ActiveProject).sFileName
End Sub

Private Sub mnuViewProjectSolutionFile2_Click()
    ViewSolution2 Project.Item(ActiveProject).sFileName
End Sub

Private Sub tbToolBar_ButtonClick(ByVal Button As MSComctlLib.Button)
    On Error Resume Next
    Select Case Button.KEY
        Case "New"
            mnuFileNew_Click
        Case "Open"
            mnuFileOpen_Click
        Case "Save"
            If Project.Count = 0 Then Exit Sub
            mnuFileSave_Click
    End Select
End Sub

Private Sub tbToolBarScreen_ButtonClick(ByVal Button As MSComctlLib.Button)
    On Error Resume Next
    If Project.Count = 0 Then Exit Sub
    Select Case Button.KEY
        Case "ZoomIn"
            setScreenMode ZOOM_IN_MODE
        Case "ZoomOut"
            setScreenMode ZOOM_OUT_MODE
        Case "ZoomWin"
            setScreenMode ZOOM_WIN_MODE
        Case "ZoomFull"
            'setScreenMode NORMAL_MODE
            ZoomFull
        Case "Pan"
            setScreenMode PAN_MODE
    End Select
End Sub

Public Sub mnuHelpAbout_Click()
    Dim m As VbMsgBoxStyle
    MsgBox "LBR-5.7" & vbCrLf & "GUI Version: " & App.Major & "." & App.Minor & " Built: " & App.Revision & vbCrLf & _
    "License Level: " & Licensing.LicenseTag & vbCrLf & _
    "License Expiration Date: " & sExpires
End Sub

Public Sub mnuHelpSearchForHelpOn_Click()
    Dim nRet As Integer
    If Len(App.HelpFile) = 0 Then
        MsgBox "Unable to display Help Contents. There is no Help associated with this project.", vbInformation, Me.Caption
    Else
        On Error Resume Next
        nRet = OSWinHelp(Me.hwnd, App.HelpFile, 261, 0)
        If Err Then
            MsgBox Err.Description
        End If
    End If
End Sub

Public Sub mnuHelpContents_Click()
    Dim sFile As String
    Dim ShellHelp As Long
    sFile = App.Path & "\" & "lbr5_gui_help.chm"
    ShellHelp = ShellExecute(0, "open", sFile, "", "", SW_SHOWNORMAL)
End Sub


Public Sub mnuViewToolbarsStandard_Click()
    tb_Initial.FDPane1.PaneVisible = Not tb_Initial.FDPane1.PaneVisible
End Sub

Private Sub mnuWindowArrangeIcons_Click()
    Me.Arrange vbArrangeIcons
End Sub

Private Sub mnuWindowTileVertical_Click()
    Me.Arrange vbTileVertical
End Sub

Private Sub mnuWindowTileHorizontal_Click()
    Me.Arrange vbTileHorizontal
End Sub

Private Sub mnuWindowCascade_Click()
    Me.Arrange vbCascade
End Sub

Private Sub mnuToolsOptions_Click()
    'ToDo: Add 'mnuToolsOptions_Click' code.
    MsgBox "Add 'mnuToolsOptions_Click' code."
End Sub

Public Sub mnuViewWebBrowser_Click()
    'ToDo: Add 'mnuViewWebBrowser_Click' code.
    'MsgBox "Add 'mnuViewWebBrowser_Click' code."
    WebSite
End Sub

Public Sub mnuViewOptions_Click()
    'ToDo: Add 'mnuViewOptions_Click' code.
    MsgBox "Add 'mnuViewOptions_Click' code."
End Sub

Private Sub mnuViewRefresh_Click()
    'ToDo: Add 'mnuViewRefresh_Click' code.
    MsgBox "Add 'mnuViewRefresh_Click' code."
End Sub

Private Sub mnuViewStatusBar_Click()
    mnuViewStatusBar.Checked = Not mnuViewStatusBar.Checked
    'sbStatusBar.Visible = mnuViewStatusBar.Checked
End Sub

Public Sub mnuViewToolbar_Click()
'    mnuViewToolbar.Checked = Not mnuViewToolbar.Checked
    'CoolBar.Visible = mnuViewToolbar.Checked
End Sub

Private Sub mnuEditPasteSpecial_Click()
    'ToDo: Add 'mnuEditPasteSpecial_Click' code.
    MsgBox "Add 'mnuEditPasteSpecial_Click' code."
End Sub

Private Sub mnuEditPaste_Click()
    On Error Resume Next
    'ActiveForm.rtfText.SelRTF = Clipboard.GetText

End Sub

Private Sub mnuEditCopy_Click()
    On Error Resume Next
    'Clipboard.SetText ActiveForm.rtfText.SelRTF

End Sub

Private Sub mnuEditCut_Click()
    On Error Resume Next
    'Clipboard.SetText ActiveForm.rtfText.SelRTF
    'ActiveForm.rtfText.SelText = vbNullString

End Sub

Private Sub mnuEditUndo_Click()
    'ToDo: Add 'mnuEditUndo_Click' code.
    MsgBox "Add 'mnuEditUndo_Click' code."
End Sub


Private Sub mnuFileExit_Click()
    'unload the form
    Unload Me

End Sub

Private Sub mnuFileSend_Click()
    'ToDo: Add 'mnuFileSend_Click' code.
    MsgBox "Add 'mnuFileSend_Click' code."
End Sub

Private Sub mnuFilePrint_Click()
    On Error Resume Next
    If ActiveForm Is Nothing Then Exit Sub
    

    With dlgCommonDialog
        .DialogTitle = "Print"
        .CancelError = True
        .flags = cdlPDReturnDC + cdlPDNoPageNums
        If ActiveForm.rtfText.SelLength = 0 Then
            .flags = .flags + cdlPDAllPages
        Else
            .flags = .flags + cdlPDSelection
        End If
        .ShowPrinter
        If Err <> MSComDlg.cdlCancel Then
            ActiveForm.rtfText.SelPrint .hdc
        End If
    End With

End Sub

Private Sub mnuFilePrintPreview_Click()
    'ToDo: Add 'mnuFilePrintPreview_Click' code.
    MsgBox "Add 'mnuFilePrintPreview_Click' code."
End Sub

Private Sub mnuFilePageSetup_Click()
    On Error Resume Next
    With dlgCommonDialog
        .DialogTitle = "Page Setup"
        .CancelError = True
        .ShowPrinter
    End With

End Sub

Private Sub mnuFileProperties_Click()
    'ToDo: Add 'mnuFileProperties_Click' code.
    MsgBox "Add 'mnuFileProperties_Click' code."
End Sub

Public Sub mnuFileSaveAs_Click()
    On Error GoTo CancelSelected
    UpdatePanelConnections ActiveProject
    If IsDemo(ActiveProject) = True Then Exit Sub
    Dim sFile As String
    With dlgCommonDialog
        .DialogTitle = "Save As"
        .CancelError = True
        'ToDo: set the flags and attributes of the common dialog control
        .Filter = "LBR-5 Project Files (*.lbr)|*.lbr|LBR-5 Data Files (*.txt)|*.txt"
        .FilterIndex = 1
        .FileName = GetFileRoot(Project.Item(ActiveProject).sFileName)
        .flags = &H2
        .InitDir = Project.Item(ActiveProject).sFileName
        
        .ShowSave
        If Len(.FileName) = 0 Then
            Exit Sub
        End If
        sFile = GetFilePath(.FileName) & GetFileRoot(.FileName) & ".txt"
    End With
    'SaveAs ActiveProject, sFile
    Select Case dlgCommonDialog.FilterIndex
        Case 1 'lbr
            Project.Item(ActiveProject).SaveAsLBR5ASCIIFile sFile
        Case 2 'txt
            If CheckModel(ActiveProject) = True Then Exit Sub
            Project.Item(ActiveProject).SaveAsLBR5TextFile sFile
    End Select
    Project.Item(ActiveProject).bNewProjectFirstSave = False
    Project.Item(ActiveProject).FileVersionNumber = VersionNumber
    Exit Sub
CancelSelected:
End Sub

Public Sub mnuFileSaveAll_Click()
    Project.SaveAllLBR5TextFiles
End Sub

Public Sub mnuFileSave_Click()
    UpdatePanelConnections ActiveProject
    If IsDemo(ActiveProject) = True Then Exit Sub
    If Project.Item(ActiveProject).bNewProjectFirstSave = True Then
        fMainForm.mnuFileSaveAs_Click
        Exit Sub
    End If
    GetFileExtension Project.Item(ActiveProject).sFileName
    Select Case GetFileExtension(Project.Item(ActiveProject).sFileName)
        Case "txt"
            If CheckModel(ActiveProject) = True Then Exit Sub
            Project.Item(ActiveProject).SaveAsLBR5TextFile Project.Item(ActiveProject).sFileName
        Case "lbr"
            Project.Item(ActiveProject).SaveAsLBR5ASCIIFile Project.Item(ActiveProject).sFileName
        Case "mlt"
            Project.Item(ActiveProject).SaveAsLBR5ASCIIFile Project.Item(ActiveProject).sFileName
    End Select
    Project.Item(ActiveProject).FileVersionNumber = VersionNumber
End Sub

Private Sub mnuFileClose_Click()
    On Error Resume Next
    Unload ActiveForm
End Sub

Public Sub mnuFileOpen_Click()
    On Error GoTo CancelSelected
    Dim sFile As String
    If Licensing.LicenseLevel = 7 Then
        LoadDemo
        Exit Sub
    End If
    With dlgCommonDialog
        .DialogTitle = "Open"
        .CancelError = True
        .Filter = "LBR-5 Project Files (*.lbr)|*.lbr|LBR-5 Text Files (*.txt)|*.txt"
        .FilterIndex = 1
        .FileName = ""
        '.Filter = "MARS-LBR5 Transfert Files (*.mlt)|*.mlt|MARS-LBR5 Transfert Files (*.txt)|*.txt"
        .flags = &H4
        '.InitDir = Project.Item(ActiveProject).sFileName ' App.Path & "\_Data"
        .ShowOpen
        If Len(.FileName) = 0 Then
            Exit Sub
        End If
        sFile = .FileName
    End With
    'FileOpen sFile
    Select Case dlgCommonDialog.FilterIndex
        Case 1 'lbr
            Project.OpenLBR5ASCIIFile sFile
        Case 2 'txt
            Project.OpenLBR5TextFile sFile
    End Select
    'SHAddToRecentDocs 2, sFile
    AddtoMRU (sFile)
    UpdateRecentList
    Exit Sub
CancelSelected:
End Sub

Public Sub mnuFileOpenLBR5Project_Click()
    On Error GoTo CancelSelected
    Dim sFile As String
    With dlgCommonDialog
        .DialogTitle = "Open"
        .CancelError = True
        'ToDo: set the flags and attributes of the common dialog control
        '.Filter = "LBR-5 Text Files (*.txt)|*.txt|LBR-5 Data Files (*.lbr)|*.lbr"
        .Filter = "LBR-5 Text Files (*.lbr)|*.lbr"
        .flags = &H4
        '.InitDir = App.Path & "\_Data"
        .ShowOpen
        If Len(.FileName) = 0 Then
            Exit Sub
        End If
        sFile = .FileName
    End With
    'RemoveEmptyLines sFile
    Dim fso As New FileSystemObject, fil As file, ts As TextStream
    Set fil = fso.GetFile(sFile)
    Set ts = fil.OpenAsTextStream(ForReading)
    Dim sLine As String
    'sLine =ReadLn(ts)
    sLine = ReadLn(ts)
    sLine = LTrim(sLine)
    sLine = RTrim(sLine)
    If LCase(Left(sLine, 7)) <> "verlbr5" Then
        MsgBox "Data File is corrupted.", vbCritical + vbOKOnly
        Exit Sub
    End If
    If sLine <> VersionNumber Then
        MsgBox "Bad file version!", vbCritical + vbOKOnly
        Exit Sub
    End If

    Project.OpenProject
    Project.Item(lProjectCount).ReadLBR5txtFile ts
    ts.Close
    'Set ts = Nothing
    Project.Item(lProjectCount).frmProject.Tag = lProjectCount
    Project.Item(lProjectCount).frmProject.Caption = GetFileName(sFile)
    Project.Item(lProjectCount).sFileName = sFile
    ZoomFull
    setFunctionMode NO_FUNCTION
    setScreenMode NORMAL_MODE
    Exit Sub
CancelSelected:
End Sub

Public Sub mnuFileImportFromMarsLbr5TransfertFile_Click()
    On Error GoTo CancelSelected
    Dim sFile As String
    With dlgCommonDialog
        .DialogTitle = "Open"
        .CancelError = True
        'ToDo: set the flags and attributes of the common dialog control
        '.Filter = "LBR-5 Text Files (*.txt)|*.txt|LBR-5 Data Files (*.lbr)|*.lbr"
        .Filter = "MARS-LBR5 Transfert Files (*.mlt)|*.mlt|MARS-LBR5 Transfert Files (*.txt)|*.txt"
        .flags = &H4
        .FileName = ""
        '.InitDir = App.Path & "\_Data"
        .ShowOpen
        If Len(.FileName) = 0 Then
            Exit Sub
        End If
        sFile = .FileName
    End With
'    Dim fso As New FileSystemObject, fil As file, ts As TextStream
'    Set fil = fso.GetFile(sFile)
'    Set ts = fil.OpenAsTextStream(ForReading)
'    Dim sLine As String
'
'    Project.OpenProject
    
   
    Project.OpenMARS_LBR5TransfertFile sFile
    AddtoMRU (sFile)
    UpdateRecentList
'    ts.Close
    
    
    Exit Sub
CancelSelected:

End Sub

Public Sub mnuFileNew_Click()
    Project.OpenProject
    Draw ActiveProject
    Project.Item(lProjectCount).bNewProjectFirstSave = True
End Sub

