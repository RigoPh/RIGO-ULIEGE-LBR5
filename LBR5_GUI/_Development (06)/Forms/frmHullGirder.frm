VERSION 5.00
Begin VB.Form frmHullGirder 
   AutoRedraw      =   -1  'True
   BorderStyle     =   5  'Sizable ToolWindow
   Caption         =   "Hull Girder"
   ClientHeight    =   6480
   ClientLeft      =   8190
   ClientTop       =   3045
   ClientWidth     =   9480
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   6480
   ScaleWidth      =   9480
   ShowInTaskbar   =   0   'False
   Begin VB.Frame frCursor 
      Appearance      =   0  'Flat
      BackColor       =   &H80000004&
      ForeColor       =   &H80000008&
      Height          =   2295
      Left            =   7560
      MousePointer    =   9  'Size W E
      TabIndex        =   4
      Top             =   3840
      Width           =   38
   End
   Begin VB.PictureBox picShear 
      AutoRedraw      =   -1  'True
      BackColor       =   &H80000009&
      Height          =   1935
      Left            =   240
      ScaleHeight     =   125
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   421
      TabIndex        =   2
      Top             =   2160
      Width           =   6375
   End
   Begin VB.PictureBox picLoading 
      AutoRedraw      =   -1  'True
      BackColor       =   &H80000009&
      Height          =   1935
      Left            =   240
      ScaleHeight     =   125
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   421
      TabIndex        =   1
      Top             =   120
      Width           =   6375
   End
   Begin VB.ListBox lstLoadCase 
      Appearance      =   0  'Flat
      Height          =   3345
      Left            =   6840
      TabIndex        =   0
      Top             =   120
      Width           =   2535
   End
   Begin VB.PictureBox picBending 
      AutoRedraw      =   -1  'True
      BackColor       =   &H80000009&
      Height          =   1935
      Left            =   240
      ScaleHeight     =   125
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   421
      TabIndex        =   3
      Top             =   4200
      Width           =   6375
   End
End
Attribute VB_Name = "frmHullGirder"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Dim ProjectIndex As Integer
Dim lp() As POINTAPI
Dim Hull_Girder As Hull_Girder

Private Sub Command1_Click()

End Sub

Private Sub Command1_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)

End Sub

Private Sub Form_Load()
    ProjectIndex = ActiveProject
    Me.Move (Screen.Width - Me.Width) / 2, (Screen.Height - Me.Height) / 2
    Me.Caption = "Hull Girder - [" & GetFileName(Project.Item(ProjectIndex).sFileName) & "]"
    PopulateLoadCaseList
    lstLoadCase.ListIndex = 0
    Hull_Girder = HullGirder(ProjectIndex, lstLoadCase.ListIndex + 1)
    InitializeCursor
    DrawDiagrams
End Sub


Private Sub InitializeCursor()
    frCursor.Left = picLoading.Left + picLoading.Width / 2
    frCursor.Top = picLoading.Top - 80
    frCursor.Height = picBending.Top + picBending.Height + 60
End Sub

Private Sub DrawDiagrams()
    DrawLoading
    DrawShearForce
    DrawBendingMoment
End Sub

Private Sub DrawLoading()
    On Error GoTo DrawLoadingErr
    Dim rgn As Long, hBrush As Long
    Dim dUnit As Double
    Dim picWidth As Long, picHeight As Long
    picWidth = picLoading.ScaleWidth * 1
    picHeight = picLoading.ScaleHeight * 1
    Dim i As Integer
    picLoading.Cls
    dUnit = (picWidth) / (NO_SECTIONS - 1)
    'DRAW LOADING
    Dim obj_loading As lines
    ReDim obj_loading.Points(NO_SECTIONS + 3)
    Dim val_min As Double, val_max As Double, val_diff As Double
    val_min = -10000000000#
    val_max = 10000000000#
    Dim p As Double
    For i = 0 To NO_SECTIONS
        p = Hull_Girder.STEPWISE_DEADWEIGHT(i) + Hull_Girder.STEPWISE_DN(i) + Hull_Girder.STEPWISE_UP(i) + _
        Hull_Girder.UNIFORM_DN(i) + Hull_Girder.UNIFORM_UP(i)
        If p <= val_max Then
            val_max = p
        End If
        If p >= val_min Then
            val_min = p
        End If
    Next i
    
    Dim dead As Double, sh As Double
    dead = Hull_Girder.STRUCTURE_DEADWEIGHT(0)
    Dim iSign As Integer
    
    If val_min <= 0 And val_max <= 0 Then
        val_max = -val_max
        val_min = 0
        val_diff = val_max - dead
        iSign = 1
    ElseIf val_min >= 0 And val_max >= 0 Then
        val_max = val_min
        val_min = 0
        val_diff = val_max - dead
        iSign = -1
    ElseIf val_min >= 0 And val_max <= 0 Then
        val_min = val_min
        val_max = -val_max
        val_diff = val_min + val_max - dead
        iSign = -1
    End If
    
    obj_loading.Points(0).Y = 0
    obj_loading.Points(0).Z = Divide(val_max * picHeight, val_diff)
    For i = 0 To NO_SECTIONS '- 1
        obj_loading.Points(i + 1).Y = i * dUnit
        sh = Hull_Girder.STEPWISE_DEADWEIGHT(i) + Hull_Girder.STEPWISE_DN(i) + Hull_Girder.STEPWISE_UP(i) + _
        Hull_Girder.UNIFORM_DN(i) + Hull_Girder.UNIFORM_UP(i)
        sh = (val_max + iSign * sh) * picHeight / val_diff
        obj_loading.Points(i + 1).Z = sh
    Next i
    obj_loading.Points(NO_SECTIONS + 1).Y = picWidth
    obj_loading.Points(NO_SECTIONS + 1).Z = obj_loading.Points(0).Z
    obj_loading.Points(NO_SECTIONS + 2).Y = 0
    obj_loading.Points(NO_SECTIONS + 2).Z = obj_loading.Points(0).Z
    
    rgn = CreatePolygonRgn(obj_loading.Points(0), NO_SECTIONS + 2, 1)
    hBrush = vbYellow
    hBrush = apiCreateSolidBrush(hBrush)
    FillRgn picLoading.hdc, rgn, hBrush
    DeleteObject hBrush
    hBrush = CreateHatchBrush(1, vbBlack)
    FillRgn picLoading.hdc, rgn, hBrush
    Polyline picLoading.hdc, obj_loading.Points(0), NO_SECTIONS + 3
    DeleteObject hBrush
    DeleteObject rgn

    val_min = val_max + val_min
    'DRAW DEADWEIGHT
    Dim obj_dead As lines
    ReDim obj_dead.Points(4)
    obj_dead.Points(0).Y = 0 '+ 50
    obj_dead.Points(0).Z = val_min * picHeight / val_diff
    obj_dead.Points(1).Y = 0 '+ 50
    obj_dead.Points(1).Z = (val_min - dead) * picHeight / val_diff
    obj_dead.Points(2).Y = picWidth '+ 50
    obj_dead.Points(2).Z = (val_min - dead) * picHeight / val_diff
    obj_dead.Points(3).Y = picWidth '+ 50
    obj_dead.Points(3).Z = val_min * picHeight / val_diff
    obj_dead.Points(4).Y = 0 '+ 50
    obj_dead.Points(4).Z = val_min * picHeight / val_diff

    rgn = CreatePolygonRgn(obj_dead.Points(0), 4, 1)
    hBrush = vbGreen
    hBrush = apiCreateSolidBrush(hBrush)
    FillRgn picLoading.hdc, rgn, hBrush
    DeleteObject hBrush
    hBrush = CreateHatchBrush(1, vbBlack)
    FillRgn picLoading.hdc, rgn, hBrush
    Polyline picLoading.hdc, obj_dead.Points(0), 5
    DeleteObject hBrush
    DeleteObject rgn

    Exit Sub
DrawLoadingErr:
    Call RaiseError(MyUnhandledError, "frmHull_Girder: Sub DrawLoading")
End Sub

Private Sub DrawBendingMoment()
    'On Error GoTo DrawBendingMomentErr
    Dim rgn As Long, hBrush As Long
    Dim dUnit As Double
    Dim picWidth As Long, picHeight As Long
    picWidth = picBending.ScaleWidth * 1
    picHeight = picBending.ScaleHeight * 1
    Dim i As Integer
    picBending.Cls
    dUnit = (picWidth) / (NO_SECTIONS - 1)
    'DRAW BENDING
    Dim obj_bending As lines
    ReDim obj_bending.Points(NO_SECTIONS + 3)
    'Get MinMax
    Dim val_min As Double, val_max As Double, val_diff As Double, max_abs As Double
    val_min = 10000000000#
    val_max = -10000000000#
    For i = 0 To NO_SECTIONS
        If Hull_Girder.BENDING_MOMENT(i) > val_max Then val_max = Hull_Girder.BENDING_MOMENT(i)
        If Hull_Girder.BENDING_MOMENT(i) < val_min Then val_min = Hull_Girder.BENDING_MOMENT(i)
    Next i
    
    val_diff = val_max - val_min
    
    '-------------
    If val_max >= 0 And val_min >= 0 Then val_diff = (val_max)
    If val_max >= 0 And val_min < 0 Then val_diff = (val_max - val_min)
    If val_max < 0 And val_min < 0 Then val_diff = (-val_min - val_max)
    '-------------

    obj_bending.Points(0).Y = 0
    obj_bending.Points(0).Z = Divide(val_max * picHeight, val_diff) '+ 59
    If val_max < 0 Then obj_bending.Points(0).Z = 0
    Dim sh As Double
    For i = 0 To NO_SECTIONS '- 1
        obj_bending.Points(i + 1).Y = i * dUnit
        sh = Hull_Girder.BENDING_MOMENT(i)
        sh = Divide((Abs(val_max) - sh) * picHeight, val_diff)
        obj_bending.Points(i + 1).Z = sh
    Next i
    obj_bending.Points(NO_SECTIONS + 1).Y = picWidth
    obj_bending.Points(NO_SECTIONS + 1).Z = obj_bending.Points(0).Z
    obj_bending.Points(NO_SECTIONS + 2).Y = 0
    obj_bending.Points(NO_SECTIONS + 2).Z = obj_bending.Points(0).Z
    
    rgn = CreatePolygonRgn(obj_bending.Points(0), NO_SECTIONS + 2, 1)
    hBrush = vbRed
    hBrush = apiCreateSolidBrush(hBrush)
    FillRgn picBending.hdc, rgn, hBrush
    DeleteObject hBrush
    hBrush = CreateHatchBrush(1, vbBlack)
    FillRgn picBending.hdc, rgn, hBrush
    
    Polyline picBending.hdc, obj_bending.Points(0), NO_SECTIONS + 3
    
    DeleteObject hBrush
    DeleteObject rgn
    Exit Sub
DrawBendingMomentErr:
    Call RaiseError(MyUnhandledError, "frmHull_Girder: Sub DrawBendingMoment")
End Sub

Private Sub DrawShearForce()
    On Error GoTo DrawShearForceErr
    Dim rgn As Long, hBrush As Long
    Dim dUnit As Double
    Dim picWidth As Long, picHeight As Long
    picWidth = picShear.ScaleWidth * 1
    picHeight = picShear.ScaleHeight * 1
    Dim i As Integer
    picShear.Cls
    dUnit = (picWidth) / (NO_SECTIONS - 1)
    'DRAW Shear
    Dim obj_Shear As lines
    ReDim obj_Shear.Points(NO_SECTIONS + 3)
    'Get MinMax
    Dim val_min As Double, val_max As Double, val_diff As Double, max_abs As Double
    val_min = 10000000000#
    val_max = -10000000000#
    For i = 0 To NO_SECTIONS
        If Hull_Girder.SHEAR_FORCE(i) > val_max Then val_max = Hull_Girder.SHEAR_FORCE(i)
        If Hull_Girder.SHEAR_FORCE(i) < val_min Then val_min = Hull_Girder.SHEAR_FORCE(i)
    Next i
    val_diff = val_max - val_min
    If Abs(val_min) > Abs(val_max) Then
        max_abs = val_min
    Else
        max_abs = val_max
    End If
    obj_Shear.Points(0).Y = 0
    obj_Shear.Points(0).Z = val_max * picHeight / val_diff
    Dim sh As Double
    For i = 0 To NO_SECTIONS '- 1
        obj_Shear.Points(i + 1).Y = i * dUnit
        sh = Hull_Girder.SHEAR_FORCE(i)
        sh = (val_max - sh) * picHeight / val_diff
        obj_Shear.Points(i + 1).Z = sh
    Next i
    obj_Shear.Points(NO_SECTIONS + 1).Y = picWidth
    obj_Shear.Points(NO_SECTIONS + 1).Z = obj_Shear.Points(0).Z
    obj_Shear.Points(NO_SECTIONS + 2).Y = 0
    obj_Shear.Points(NO_SECTIONS + 2).Z = obj_Shear.Points(0).Z
    
    rgn = CreatePolygonRgn(obj_Shear.Points(0), NO_SECTIONS + 2, 1)
    hBrush = vbBlue
    
    hBrush = apiCreateSolidBrush(hBrush)
    FillRgn picShear.hdc, rgn, hBrush
    DeleteObject hBrush
    hBrush = CreateHatchBrush(1, vbBlack)
    FillRgn picShear.hdc, rgn, hBrush
    Polyline picShear.hdc, obj_Shear.Points(0), NO_SECTIONS + 3
    
    DeleteObject hBrush
    DeleteObject rgn

    Exit Sub
DrawShearForceErr:
    Call RaiseError(MyUnhandledError, "frmHull_Girder: Sub DrawShearForce")
End Sub

Private Sub PopulateLoadCaseList()
    Dim LoadCase As cLoadCase
    For Each LoadCase In Project.Item(ProjectIndex).cHeader.colLoadCase
        lstLoadCase.AddItem LoadCase.Title
    Next LoadCase
End Sub

Private Sub Form_Resize()
    On Error Resume Next

    lstLoadCase.Left = Me.ScaleWidth - lstLoadCase.Width - 100
    lstLoadCase.Height = 2000

    picLoading.Top = Me.ScaleTop + 10
    picLoading.Height = 1 / 3 * Me.ScaleHeight - 200
    picLoading.Width = IIf(lstLoadCase.Left - 400 > 0, lstLoadCase.Left - 400, 1)
    picShear.Top = picLoading.Top + picLoading.Height + 20
    picShear.Width = picLoading.Width
    picShear.Height = 1 / 3 * Me.ScaleHeight - 200
    picBending.Top = picShear.Top + picShear.Height + 20
    picBending.Height = 1 / 3 * Me.ScaleHeight - 50 - 200
    picBending.Width = picLoading.Width
    
    frCursor.Top = picLoading.Top - 80
    frCursor.Height = picBending.Top + picBending.Height + 60
    
    DrawDiagrams
End Sub

Private Sub frCursor_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
    If Button = 1 Then
        
'        If frCursor.Left <= picLoading.Left And CLng(X) >= 0 Then
'            frCursor.Left = picLoading.Left + CLng(X)
'        ElseIf frCursor.Left >= picLoading.Left + CLng(picLoading.Width) * 1 Then
'            frCursor.Left = picLoading.Left + CLng(picLoading.Width * 1) - CLng(X)
'        ElseIf frCursor.Left > picLoading.Left And frCursor.Left < picLoading.Left + CLng(picLoading.Width * 1) Then
'            frCursor.Left = frCursor.Left + CLng(X)
'        End If
    End If
    frCursor.Refresh
    Me.Refresh
End Sub

Private Sub frCursor_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
    If frCursor.Left < picLoading.Left Then
        frCursor.Left = picLoading.Left
    End If
End Sub

Private Sub lstLoadCase_Click()
    Hull_Girder = HullGirder(ProjectIndex, lstLoadCase.ListIndex + 1)
    DrawDiagrams
End Sub

