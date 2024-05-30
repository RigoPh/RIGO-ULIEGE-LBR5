VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "comdlg32.ocx"
Begin VB.Form old_frmGrphObjFct 
   AutoRedraw      =   -1  'True
   BorderStyle     =   5  'Sizable ToolWindow
   Caption         =   "Objective Function"
   ClientHeight    =   4365
   ClientLeft      =   6375
   ClientTop       =   3405
   ClientWidth     =   8055
   Icon            =   "old_frmGraphObjFct.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   291
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   537
   ShowInTaskbar   =   0   'False
   Begin VB.PictureBox Pic 
      Appearance      =   0  'Flat
      AutoRedraw      =   -1  'True
      BackColor       =   &H80000005&
      ForeColor       =   &H80000008&
      Height          =   2175
      Left            =   120
      ScaleHeight     =   143
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   183
      TabIndex        =   0
      Top             =   120
      Width           =   2775
      Begin VB.Shape Shape1 
         BorderWidth     =   3
         Height          =   855
         Left            =   0
         Top             =   0
         Visible         =   0   'False
         Width           =   735
      End
   End
   Begin MSComDlg.CommonDialog dlgCommonDialog 
      Left            =   120
      Top             =   2880
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
   End
   Begin VB.Menu mnuSnapshot 
      Caption         =   "&Snapshot"
   End
   Begin VB.Menu mnuPrint 
      Caption         =   "&Print"
   End
End
Attribute VB_Name = "old_frmGrphObjFct"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Base 1
Option Explicit
Dim ICOUT As Integer
Dim aObjFct(0 To 100, 1) As Variant
Dim brYellow As Long
Dim MinPoint As Double
Dim header As cHeader
Dim ProjectIndex As Integer
Dim NoOfIte As Integer

Private Sub Form_Load()
    ProjectIndex = ActiveProject
    Me.Move (Screen.Width - Me.Width) / 2, (Screen.Height - Me.Height) / 2
    Me.Caption = "Objective Function Variation - [" & GetFileName(Project.Item(ProjectIndex).sFileName) & "]"
    Set header = Project.Item(ProjectIndex).cHeader
    ReadFile
End Sub

Private Sub ReadFile()
    On Error GoTo 1
    Dim i As Integer, j As Integer, find As String, FirstOcc As Integer
    Dim fso As New FileSystemObject, fil As file, ts As TextStream, v() As Variant
    Dim sPrefix As String, sName As String, sPath As String, sDocName As String, sLine As String, sFile As String
    Set fso = New FileSystemObject
    sFile = Project.Item(ProjectIndex).sFileName
    sPrefix = "opt-"
    sPath = GetFilePath(sFile)
    sName = GetFileRoot(sFile) & ".txt"
    sDocName = sPath & sPrefix & sName
    Set fil = fso.GetFile(sDocName)
    Set ts = fil.OpenAsTextStream(ForReading)
    
    
    Do Until find = "La fonction" Or ts.AtEndOfStream = True
        sLine = ReadLn(ts)
        If ts.AtEndOfStream = True Then
            MsgBox "File not read succesfully!", vbCritical + vbOKOnly
        End If
        find = Left(sLine, 11)
    Loop
    sLine = CleanString(sLine)
    Select Case LCase(right(sLine, 4))
        Case "rtie"
            ICOUT = -1
        Case "cout"
            ICOUT = 1
        Case "oids"
            ICOUT = 0
        Case Else
            MsgBox "Error. The Objective function type could not be read.", vbCritical + vbOKOnly
    End Select
    ' First Iteration
    Select Case ICOUT
        Case -1
            find = ""
            Do Until find = "INERTIE-INERTIAIYY" Or ts.AtEndOfStream = True
                sLine = ReadLn(ts)
                sLine = RemoveBlanksTabs(sLine)
                find = Left(sLine, 18)
            Loop
            FirstOcc = InStr(1, sLine, "=")
            sLine = Mid(sLine, FirstOcc + 1)
            GetValues 1, sLine, v
            aObjFct(0, 1) = Round((Val_(v(1))), 3) '/ 10000
        Case 0
            find = ""
            Do Until find = "POIDS-WEIGHT" Or ts.AtEndOfStream = True
                sLine = ReadLn(ts)
                sLine = RemoveBlanksTabs(sLine)
                find = Left(sLine, 12)
            Loop
            FirstOcc = InStr(1, sLine, "=")
            sLine = Mid(sLine, FirstOcc + 1)
            GetValues 1, sLine, v
            aObjFct(0, 1) = CLng(Val_(v(1))) / 10000
        Case 1
            find = ""
            Do Until find = "COUT-COST" Or ts.AtEndOfStream = True
                sLine = ReadLn(ts)
                sLine = RemoveBlanksTabs(sLine)
                find = Left(sLine, 9)
            Loop
            FirstOcc = InStr(1, sLine, "=")
            sLine = Mid(sLine, FirstOcc + 1)
            GetValues 1, sLine, v
            aObjFct(0, 1) = CLng(Val_(v(1)))
    End Select
    'Read objective function (recalculated)
    i = 0
    Select Case ICOUT
        Case Is = -1
            Do While ts.AtEndOfStream <> True
                find = ""
                Do Until find = "fctobjectifinertie(recalculée)" Or ts.AtEndOfStream = True
                    sLine = ReadLn(ts)
                    sLine = RemoveBlanksTabs(sLine)
                    sLine = LCase(sLine)
                    find = Left(sLine, 30)
                    If ts.AtEndOfStream = True Then Exit Do
                Loop
                i = i + 1
                FirstOcc = InStr(1, sLine, "=")
                sLine = Mid(sLine, FirstOcc + 1)
                GetValues 1, sLine, v
                aObjFct(i, 1) = Round((Val_(v(1))), 3)
            Loop
        Case Is = 0
            Do While ts.AtEndOfStream <> True
                find = ""
                Do Until find = "fctobjectifpoids(recalculée)" Or ts.AtEndOfStream = True
                    sLine = ReadLn(ts)
                    sLine = RemoveBlanksTabs(sLine)
                    sLine = LCase(sLine)
                    find = Left(sLine, 28)
                    If ts.AtEndOfStream = True Then Exit Do
                Loop
                i = i + 1
                FirstOcc = InStr(1, sLine, "=")
                sLine = Mid(sLine, FirstOcc + 1)
                GetValues 1, sLine, v
                aObjFct(i, 1) = Round((Val_(v(1))) / 10000, 3)
            Loop
        Case Is = 1
            Do While ts.AtEndOfStream <> True
                find = ""
                Do Until find = "fctobjectifcout(recalculée)" Or ts.AtEndOfStream = True
                    sLine = ReadLn(ts)
                    sLine = RemoveBlanksTabs(sLine)
                    sLine = LCase(sLine)
                    find = Left(sLine, 27)
                    If ts.AtEndOfStream = True Then Exit Do
                Loop
                i = i + 1
                FirstOcc = InStr(1, sLine, "=")
                sLine = Mid(sLine, FirstOcc + 1)
                GetValues 1, sLine, v
                aObjFct(i, 1) = Round((Val_(v(1))), 3)
            Loop
    End Select

'    Read Primal Function value
'    i = 0
'    Do While ts.AtEndOfStream <> True
'        find = ""
'        Do Until find = "Primal Function" Or ts.AtEndOfStream = True  'Or EOF(1) ' cauta val fct obj pe iteratii
'            sLine = ReadLn(ts)
'            find = Left(sLine, 15)
'            If ts.AtEndOfStream = True Then Exit Do
'        Loop
'        GetValues 4, sLine, v
'        i = i + 1
'        Select Case ICOUT
'            Case -1
'                aObjFct(i, 1) = Round((Val_(v(4))), 3)
'            Case 0
'                aObjFct(i, 1) = CLng(Val_(v(4))) / 10000
'            Case 1
'                aObjFct(i, 1) = CLng(Val_(v(4)))
'        End Select
'    Loop
    NoOfIte = i
    ts.Close
    DrawFunction aObjFct
    Exit Sub
1
    MsgBox "File not read succesfully!", vbCritical + vbOKOnly
End Sub

Private Sub Form_Resize()
    On Error Resume Next
    Pic.Cls
    Pic.Top = Me.ScaleTop
    Pic.Left = Me.ScaleLeft + 1
    Pic.Height = Me.ScaleHeight - 1
    Pic.Width = Me.ScaleWidth - 2
    Shape1.Left = Me.ScaleLeft + 40
    Shape1.Width = Me.ScaleWidth - 80
    Shape1.Top = Me.ScaleTop + 40
    Shape1.Height = Abs(Me.ScaleHeight - 80)
    DrawFunction aObjFct
End Sub

Private Sub DrawFunction(ByRef vOf As Variant)
    'On Error Resume Next
    Dim lSlice As Long
    Dim lMin As Double
    Dim lMax As Double
    Dim lXs() As Long
    Dim i As Integer
    Dim shL As Long, shH As Long, shT As Long, shW As Long
    If NoOfIte = 0 Then Exit Sub
    brYellow = apiCreateSolidBrush(vbYellow)
    shW = Shape1.Width
    shL = Shape1.Width - Shape1.Left
    shH = Shape1.Height - Shape1.Top
    shT = Shape1.Top
    
    lSlice = CLng(shW / (NoOfIte - 1))
    ReDim lXs(0 To NoOfIte - 1)
    'lXs(0) = lSlice
    lXs(0) = Shape1.Left
    For i = 1 To NoOfIte - 1
        lXs(i) = lXs(i - 1) + lSlice
    Next i
    
    GetMinMax vOf, lMin, lMax
    Dim a As Long
    Dim b As Long
    Dim c As Long
    Dim multi As Long
    multi = 1000 '000
    'multi = 10 ^ (Len(lMax))
    'multi = 524280 / lMax
    
    a = CLng((lMax - lMin) * multi)
    b = shH + shT
    If a = 0 Then a = 1
    If b = 0 Then b = 1
    c = CLng(a / b) '/ 0.9
    If c = 0 Then c = 1
    Dim lp() As POINTAPI
    ReDim lp(1 To NoOfIte) '+ 1)
    For i = 1 To UBound(lp)
        lp(i).y = (lXs(i - 1))
        lp(i).z = ((lMax * multi - (vOf(i - 1, 1)) * multi) / c)
    Next i
    For i = 1 To UBound(lp)
        lp(i).z = lp(i).z + shT
    Next i
    Pic.DrawStyle = 0
    Pic.DrawWidth = 2
    Pic.ForeColor = vbRed
    Polyline Pic.hdc, lp(1), UBound(lp)
    Pic.DrawStyle = 0

    Pic.DrawWidth = 1
    Pic.ForeColor = vbBlue
    Dim r As RECT
    Dim R1 As RECT
    'Dim br As Long
    Dim strvOf As String
    For i = 1 To UBound(lp)
        strvOf = GetSpaceFormat(CStr(vOf(i - 1, 1)))
        r.Left = lp(i).y - Len(strvOf) * 7 / 2
        r.right = r.Left + Len(strvOf) * 6
        r.Top = lp(i).z + 5
        r.bottom = lp(i).z + 20
        R1 = r
        R1.Left = lp(i).y + 7 / 2
        R1.right = R1.Left + 12 * 7
        R1.Top = r.Top - 22
        FillRect Pic.hdc, r, brYellow
        Rectangle Pic.hdc, lp(i).y - 3, lp(i).z - 3, lp(i).y + 3, lp(i).z + 3
        Rectangle Pic.hdc, r.Left, r.Top, r.right, r.bottom
        DrawText Pic.hdc, strvOf, Len(strvOf), r, 1
        If i = 1 Then
            DrawText Pic.hdc, "Initial", -1, R1, 0
        Else
            DrawText Pic.hdc, i - 1, -1, R1, 0
        End If
    Next i
    R1.Left = Shape1.Left + Shape1.Width - 90
    R1.right = Shape1.Width + Shape1.Left
    R1.Top = Shape1.Top - 30
    R1.bottom = Shape1.Top - 15
    FillRect Pic.hdc, R1, brYellow
    Rectangle Pic.hdc, R1.Left, R1.Top, R1.right, R1.bottom
    Dim dInit As Double, dFin As Double, dGain As Double
    dInit = CDbl(vOf(0, 1))
    dFin = CDbl(vOf(UBound(lp) - 1, 1))
    If ICOUT = -1 Then
        dGain = -(Round((dInit - dFin) / dInit * 100, 3))
    Else
        dGain = Round((dInit - dFin) / dInit * 100, 3)
    End If
    'dGain = Round(CDbl(vOf(UBound(lp) - 1, 1)) / CDbl(vOf(0, 1)) * 100, 3)
    
    DrawText Pic.hdc, "Gain = " & dGain & "%", -1, R1, 1
    Pic.ForeColor = vbBlack
    Select Case ICOUT
        Case -1
            Pic.Print "Inertia Objective Function [m4]"
        Case 0
            Pic.Print "Weight Objective Function [T]"
        Case 1
            Pic.Print "Cost Objective Function [€]"
    End Select
    Refresh
    DeleteObject brYellow

End Sub

Private Sub GetMinMax(ByRef vOf As Variant, lMin As Double, lMax As Double)
    On Error Resume Next
    Dim i As Integer
    lMax = vOf(0, 1)
    lMin = vOf(0, 1)
    For i = 0 To NoOfIte - 2
        If (vOf(i + 1, 1)) > lMax Then
            lMax = (vOf(i + 1, 1))
        End If
    Next i
    For i = 0 To NoOfIte - 2
        If (vOf(i + 1, 1)) <= lMin Then
            lMin = (vOf(i + 1, 1))
        End If
    Next i
    Exit Sub
1
End Sub

Private Sub Form_Unload(Cancel As Integer)
    fMainForm.SetFocus
End Sub

Private Sub mnuPrint_Click()
    Print_ Pic
End Sub

Private Sub mnuSnapshot_Click()
    On Error Resume Next
    With dlgCommonDialog
        .DialogTitle = "Snapshot"
        .CancelError = True
        .Filter = "Picture Files (*.bmp)|*.bmp"
        .FileName = ""
        .ShowSave
        If Len(.FileName) = 0 Then
            Exit Sub
        End If
        SavePicture Me.Pic.Image, .FileName
    End With
End Sub
