VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "comdlg32.ocx"
Begin VB.Form old_frmGraphGravityVar 
   AutoRedraw      =   -1  'True
   BorderStyle     =   5  'Sizable ToolWindow
   Caption         =   "Gravity Center Variation"
   ClientHeight    =   4380
   ClientLeft      =   8745
   ClientTop       =   3375
   ClientWidth     =   8070
   Icon            =   "old_frmGraphGravityVar.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   292
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   538
   ShowInTaskbar   =   0   'False
   Begin MSComDlg.CommonDialog dlgCommonDialog 
      Left            =   960
      Top             =   3600
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
   End
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
   End
   Begin VB.Shape Shape1 
      BorderWidth     =   3
      Height          =   855
      Left            =   0
      Top             =   0
      Visible         =   0   'False
      Width           =   735
   End
   Begin VB.Menu mnuSnapShot 
      Caption         =   "&Snapshot"
   End
   Begin VB.Menu mnuPrint 
      Caption         =   "&Print"
   End
End
Attribute VB_Name = "old_frmGraphGravityVar"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Base 1
Option Explicit

Dim aObjFct(0 To 100, 1) As Variant
Dim brYellow As Long
Dim MinPoint As Double
Dim ProjectIndex As Integer
Dim header As cHeader
Dim NoOfIte As Integer

Private Sub Form_Load()
    ProjectIndex = ActiveProject
    Me.Move (Screen.Width - Me.Width) / 2, (Screen.Height - Me.Height) / 2
    Me.Caption = "Gravity Center Variation - [" & GetFileName(Project.Item(ProjectIndex).sFileName) & "]"
    Set header = Project.Item(ProjectIndex).cHeader
    ReadFile
End Sub

Private Sub ReadFile()
    On Error GoTo 1
    Dim i As Integer, find As String, FirstOcc As Integer, sFile As String
    Dim fso As New FileSystemObject, fil As file, ts As TextStream
    Dim sPrefix As String, sName As String, sPath As String, sDocName As String, sLine As String
    Set fso = New FileSystemObject
    sFile = Project.Item(ProjectIndex).sFileName
    sPrefix = "opt-"
    sPath = GetFilePath(sFile)
    sName = GetFileName(sFile)
    sDocName = sPath & sPrefix & GetFileRoot(sName) & ".txt"
    Set fil = fso.GetFile(sDocName)
    Set ts = fil.OpenAsTextStream(ForReading)
    i = -1
    Do While ts.AtEndOfStream <> True
        find = ""
        Do Until find = "KG actuel" Or ts.AtEndOfStream = True 'Or EOF(fil)   ' cauta prima val fct obj
            sLine = ReadLn(ts)
            find = Left(sLine, 9)
        Loop
        If ts.AtEndOfStream = True Then Exit Do
        FirstOcc = InStr(1, sLine, "=")
        sLine = Mid(sLine, FirstOcc + 1)
        sLine = CleanLines(sLine)
        i = i + 1
        aObjFct(i, 1) = Round(Val_(Left(sLine, 13)), 3)
    Loop
    ts.Close
    NoOfIte = i + 1
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
    Shape1.Height = Abs(Me.ScaleHeight - 50)
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
    lSlice = CLng(Divide(shW, (NoOfIte - 1)))
    ReDim lXs(0 To NoOfIte)
    'lXs(0) = lSlice
    lXs(0) = Shape1.Left
    For i = 1 To NoOfIte
        lXs(i) = lXs(i - 1) + lSlice
    Next i
    GetMinMax vOf, lMin, lMax
    Dim a As Long
    Dim b As Long
    Dim c As Long
    Dim multi As Long
    
    multi = 100000
    a = CLng((lMax - lMin) * multi)
    b = shH
    If a = 0 Then a = 1
    If b = 0 Then b = 1
    c = CLng(a / b) '/ 0.9
    If c = 0 Then c = 1
    Dim lp1() As POINTAPI
    ReDim lp1(1 To NoOfIte)
    For i = 1 To UBound(lp1)
        lp1(i).y = (lXs(i - 1))
        lp1(i).z = ((CLng(lMax * multi) - CLng(vOf(i - 1, 1) * multi)) / c)
    Next i
    For i = 1 To UBound(lp1)
        lp1(i).z = lp1(i).z + shT
    Next i
    Pic.DrawStyle = 0
    Pic.DrawWidth = 2
    Pic.ForeColor = vbRed
    Polyline Pic.hdc, lp1(1), UBound(lp1)
    Pic.DrawStyle = 0
    Pic.DrawWidth = 1
    Pic.ForeColor = vbBlue
    Dim r As RECT
    Dim R1 As RECT
    'Dim br As Long
    For i = 1 To UBound(lp1)
        r.Left = lp1(i).y - Len(vOf(i - 1, 1)) * 7 / 2
        r.right = r.Left + Len(vOf(i - 1, 1)) * 7
        r.Top = lp1(i).z + 5
        r.bottom = lp1(i).z + 20
        R1 = r
        R1.Left = lp1(i).y + 7 / 2
        R1.right = R1.Left + 12 * 7
        R1.Top = r.Top - 22
        FillRect Pic.hdc, r, brYellow
        Rectangle Pic.hdc, lp1(i).y - 3, lp1(i).z - 3, lp1(i).y + 3, lp1(i).z + 3
        Rectangle Pic.hdc, r.Left, r.Top, r.right, r.bottom
        DrawText Pic.hdc, str(vOf(i - 1, 1)), -1, r, 0
        If i = 1 Then
            DrawText Pic.hdc, "Initial", -1, R1, 0
        Else
            DrawText Pic.hdc, i - 1, -1, R1, 0
        End If
    Next i
    Pic.ForeColor = vbBlack
    Pic.Print "Gravity Center Variation [m]"
    DeleteObject brYellow
    Refresh
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
    Set header = Nothing
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
