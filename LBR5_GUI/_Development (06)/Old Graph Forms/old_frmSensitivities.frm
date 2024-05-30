VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "comdlg32.ocx"
Begin VB.Form old_frmSensitivities 
   BorderStyle     =   5  'Sizable ToolWindow
   Caption         =   "Design Variation"
   ClientHeight    =   4965
   ClientLeft      =   765
   ClientTop       =   3075
   ClientWidth     =   10020
   Icon            =   "old_frmSensitivities.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   331
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   668
   ShowInTaskbar   =   0   'False
   Begin VB.CheckBox chkBox 
      Caption         =   "check box"
      Height          =   255
      Index           =   0
      Left            =   2280
      TabIndex        =   2
      Top             =   480
      Visible         =   0   'False
      Width           =   2655
   End
   Begin VB.ListBox PanelList 
      Height          =   2985
      Left            =   5400
      TabIndex        =   1
      Top             =   1200
      Width           =   1935
   End
   Begin VB.PictureBox Pic 
      AutoRedraw      =   -1  'True
      BackColor       =   &H00FFFFFF&
      Height          =   3135
      Left            =   960
      ScaleHeight     =   205
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   285
      TabIndex        =   0
      Top             =   1200
      Width           =   4335
   End
   Begin MSComDlg.CommonDialog dlgCommonDialog 
      Left            =   0
      Top             =   0
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
   End
   Begin VB.Shape Shape1 
      Height          =   1335
      Left            =   120
      Top             =   360
      Visible         =   0   'False
      Width           =   1455
   End
   Begin VB.Menu mnuSnapshot 
      Caption         =   "&Snapshot"
   End
   Begin VB.Menu mnuPrint 
      Caption         =   "&Print"
   End
End
Attribute VB_Name = "old_frmSensitivities"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Dim nChk As Integer
Dim PanelIndex As Integer
Dim vMin As Double
Dim vMax As Double
Dim lSlice As Long
Dim lXs() As Long
Dim shL As Long, shH As Long, shT As Long, shW As Long
Dim r As RECT
Dim R1 As RECT
Dim brGreen As Long
Dim brYellow As Long
Dim ProjectIndex As Integer
Dim NoOfIte As Integer
Dim Panel As cPanel
'Dim colPanel As Collection

Private Type stBorne
    InfEdge As Double
    CurrentValue As Double
    SupEdge As Double
End Type

Private Type stVariable
    PlateThick As stBorne
    HWebStiff As stBorne
    TWebStiff As stBorne
    WFlaStiff As stBorne
    SpcStiff As stBorne
    HWebFrame As stBorne
    TWebFrame As stBorne
    WFlaFrame As stBorne
    SpcFrame As stBorne
End Type

Private Type stPanel
    PanelNo() As stVariable
End Type
    Dim IT() As stPanel
    Dim IANA As Integer

Private Sub chkBox_Click(index As Integer)
    GetMinMax
    GetItemsToDraw
End Sub

Private Sub Form_Load()
    ProjectIndex = ActiveProject
    IANA = Project.Item(ProjectIndex).cHeader.IANA
    Me.Move (Screen.Width - Me.Width) / 2, (Screen.Height - Me.Height) / 2
    Me.Caption = "Design Variables Variation - [" & GetFileName(Project.Item(ProjectIndex).sFileName) & "]"
    NoOfIte = Project.Item(ProjectIndex).cHeader.ITERAM
    nChk = 10
    GenerateCheckBoxes
    FillPanelList
    PanelList.Width = chkBox(0).Width
    PanelList.Top = 0
    ReadFile
End Sub

Private Sub Form_Resize()
    Dim i As Integer
    On Error Resume Next
    Pic.Cls
    Pic.Top = Me.ScaleTop
    Pic.Left = Me.ScaleLeft
    Pic.Height = Me.ScaleHeight
    Pic.Width = Abs(Me.ScaleWidth - 5 - chkBox(0).Width)
    Shape1.Left = Me.ScaleLeft + 40
    Shape1.Width = Abs(Me.ScaleWidth - 80 - 5 - chkBox(0).Width)
    Shape1.Top = Me.ScaleTop + 40
    Shape1.Height = Abs(Me.ScaleHeight - 40)
    PanelList.Left = Me.ScaleWidth - chkBox(i).Width
    PanelList.Height = Abs(Me.ScaleHeight - 20 * nChk)
    For i = 1 To nChk
        chkBox(i).Left = Me.ScaleWidth - chkBox(i).Width
    Next i
    Dim index As Integer
    index = 0
    For i = 1 To nChk
        If chkBox(i).Visible = True Then
            index = index + 1
        End If
        chkBox(i).Top = PanelList.Height + 20 * (index - 1)
    Next i
    GetMinMax
    GetLeftRight
    GetItemsToDraw
End Sub

Private Sub GetItemsToDraw()
    Dim i As Integer
    Dim FctVal() As Variant
    On Error GoTo 1
    ReDim FctVal(0 To NoOfIte)
    Pic.Cls
    If chkBox(1) Then
        If IT(1).PanelNo(PanelIndex).PlateThick.InfEdge > 0 Then
            FctVal(0) = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.NetThickness
            Pic.ForeColor = vbRed
            For i = 1 To UBound(FctVal)
                FctVal(i) = IT(i).PanelNo(PanelIndex).PlateThick.CurrentValue
            Next i
            DoDraw FctVal(), "PT"
        End If
    End If
    If chkBox(2) Then
        If IT(1).PanelNo(PanelIndex).HWebFrame.InfEdge > 0 Then
            FctVal(0) = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryFrames.WebHeight
            For i = 1 To UBound(FctVal)
                FctVal(i) = IT(i).PanelNo(PanelIndex).HWebFrame.CurrentValue
            Next i
            DoDraw FctVal(), "FWH"
        End If
    End If
    If chkBox(3) Then
        If IT(1).PanelNo(PanelIndex).TWebFrame.InfEdge > 0 Then
            FctVal(0) = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryFrames.WebThickness
            For i = 1 To UBound(FctVal)
                FctVal(i) = IT(i).PanelNo(PanelIndex).TWebFrame.CurrentValue
            Next i
            DoDraw FctVal(), "FWT"
        End If
    End If
    If chkBox(4) Then
        If IT(1).PanelNo(PanelIndex).WFlaFrame.InfEdge > 0 Then
            FctVal(0) = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryFrames.FlangeWidth
            For i = 1 To UBound(FctVal)
                FctVal(i) = IT(i).PanelNo(PanelIndex).WFlaFrame.CurrentValue
            Next i
            DoDraw FctVal(), "FFW"
        End If
    End If
    If chkBox(5) Then
        If IT(1).PanelNo(PanelIndex).SpcFrame.InfEdge > 0 Then
            FctVal(0) = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryFrames.Spacing
            For i = 1 To UBound(FctVal)
                FctVal(i) = IT(i).PanelNo(PanelIndex).SpcFrame.CurrentValue
            Next i
            DoDraw FctVal(), "FS"
        End If
    End If
    If chkBox(6) Then
        If IT(1).PanelNo(PanelIndex).HWebStiff.InfEdge > 0 Then
            FctVal(0) = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryStiffeners.WebHeight
            For i = 1 To UBound(FctVal)
                FctVal(i) = IT(i).PanelNo(PanelIndex).HWebStiff.CurrentValue
            Next i
            DoDraw FctVal(), "SWH"
        End If
    End If
    If chkBox(7) Then
        If IT(1).PanelNo(PanelIndex).TWebStiff.InfEdge > 0 Then
            FctVal(0) = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryStiffeners.WebThickness
            For i = 1 To UBound(FctVal)
                FctVal(i) = IT(i).PanelNo(PanelIndex).TWebStiff.CurrentValue
            Next i
            DoDraw FctVal(), "SWT"
        End If
    End If
    If chkBox(8) Then
        If IT(1).PanelNo(PanelIndex).WFlaStiff.InfEdge > 0 Then
            FctVal(0) = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryStiffeners.FlangeWidth
            For i = 1 To UBound(FctVal)
                FctVal(i) = IT(i).PanelNo(PanelIndex).WFlaStiff.CurrentValue
            Next i
            DoDraw FctVal(), "SFW"
        End If
    End If
    If chkBox(9) Then
        If IT(1).PanelNo(PanelIndex).SpcStiff.InfEdge > 0 Then
            FctVal(0) = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryStiffeners.Spacing
            For i = 1 To UBound(FctVal)
                FctVal(i) = IT(i).PanelNo(PanelIndex).SpcStiff.CurrentValue
            Next i
            DoDraw FctVal(), "SS"
        End If
    End If
    Pic.ForeColor = vbBlack
    Pic.Print "Design Variables Variation [mm]"
    Exit Sub
1
    'MsgBox "File not read succesfully!", vbCritical + vbOKOnly
End Sub

Private Sub DoDraw(FctVal As Variant, ItemName As String)
    Dim i As Integer
    Dim a As Long
    Dim b As Long
    Dim c As Long
    Dim lMax As Long
    Dim lMin As Long
    Dim lFactor As Long
    On Error GoTo 1
    lFactor = 10000000

    lMax = CLng(vMax * lFactor)
    lMin = CLng(vMin * lFactor)
    a = lMax - lMin
    b = shH
    If a = 0 Then a = 1
    If b = 0 Then b = 1
    c = CLng(a / b)
    If c = 0 Then c = 1
    Dim lp() As POINTAPI

    ReDim lp(0 To NoOfIte)
    For i = 0 To UBound(lp)
        lp(i).y = (lXs(i))
        lp(i).z = (lMax - FctVal(i) * lFactor) / c
    Next i
    For i = 0 To UBound(lp)
        lp(i).z = lp(i).z + shT
    Next i
    Pic.DrawStyle = 0
    Pic.DrawWidth = 2
    Pic.ForeColor = vbRed
    Polyline Pic.hdc, lp(0), UBound(lp) + 1
    
    Pic.DrawStyle = 0
    Pic.DrawWidth = 1
    Pic.ForeColor = vbBlue
    

    'br = apiCreateSolidBrush(vbGreen)
    r.Left = 1    'lp(0).Y '- Len(ItemName) * 9
    r.right = r.Left + Len(ItemName) * 9
    r.Top = Abs(lp(0).z - 5)
    r.bottom = Abs(lp(0).z + 10)
    brGreen = apiCreateSolidBrush(vbGreen)
    FillRect Pic.hdc, r, brGreen
    DeleteObject brGreen
    Rectangle Pic.hdc, r.Left - 1, r.Top, r.right, r.bottom
    DrawText Pic.hdc, ItemName, -1, r, 0
    
    For i = 0 To UBound(lp)
        r.Left = lp(i).y - Len(str(FctVal(i) * 1000)) * 6 / 2
        r.right = r.Left + Len(str(FctVal(i) * 1000)) * 6
        r.Top = lp(i).z + 5
        r.bottom = lp(i).z + 20
        R1 = r
        R1.Left = lp(i).y + 7 / 2
        R1.right = R1.Left + 12 * 7
        R1.Top = r.Top - 22
'        r1 = r
'        r1.right = r.right + 13 * 7
'        r1.top = r.top - 22
         ' Nodes
        Rectangle Pic.hdc, lp(i).y - 3, lp(i).z - 3, lp(i).y + 3, lp(i).z + 3
        If chkBox(10) Then
            brYellow = apiCreateSolidBrush(vbYellow)
            FillRect Pic.hdc, r, brYellow
            DeleteObject brYellow
            'Background
            Rectangle Pic.hdc, r.Left, r.Top, r.right, r.bottom

            DrawText Pic.hdc, str(FctVal(i) * 1000), -1, r, 0
            If i = 0 Then
                DrawText Pic.hdc, "Initial", -1, R1, 0
            Else
                DrawText Pic.hdc, i, -1, R1, 0
            End If
        End If
    Next i
        Exit Sub
1
    'MsgBox "File not read succesfully!", vbCritical + vbOKOnly
End Sub

Private Sub GetMinMax()
    On Error GoTo 1
    vMin = 100
    vMax = 0
    Dim i As Integer
    If chkBox(1) Then
        For i = 0 To NoOfIte
            If IT(1).PanelNo(PanelIndex).PlateThick.InfEdge > 0 Then
                If IT(i).PanelNo(PanelIndex).PlateThick.CurrentValue <= vMin Then vMin = IT(i).PanelNo(PanelIndex).PlateThick.CurrentValue
                If IT(i).PanelNo(PanelIndex).PlateThick.CurrentValue >= vMax Then vMax = IT(i).PanelNo(PanelIndex).PlateThick.CurrentValue
                If Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.NetThickness <= vMin Then vMin = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.NetThickness
                If Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.NetThickness >= vMax Then vMax = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.NetThickness
            End If
        Next i
    End If
    If chkBox(2) Then
        For i = 0 To NoOfIte
            If IT(1).PanelNo(PanelIndex).HWebFrame.InfEdge > 0 Then
                If IT(i).PanelNo(PanelIndex).HWebFrame.CurrentValue <= vMin Then vMin = IT(i).PanelNo(PanelIndex).HWebFrame.CurrentValue
                If IT(i).PanelNo(PanelIndex).HWebFrame.CurrentValue >= vMax Then vMax = IT(i).PanelNo(PanelIndex).HWebFrame.CurrentValue
                If Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryFrames.WebHeight <= vMin Then vMin = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryFrames.WebHeight
                If Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryFrames.WebHeight >= vMax Then vMax = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryFrames.WebHeight
            End If
        Next i
    End If
    If chkBox(3) Then
        For i = 0 To NoOfIte
            If IT(1).PanelNo(PanelIndex).TWebFrame.InfEdge > 0 Then
                If IT(i).PanelNo(PanelIndex).TWebFrame.CurrentValue <= vMin Then vMin = IT(i).PanelNo(PanelIndex).TWebFrame.CurrentValue
                If IT(i).PanelNo(PanelIndex).TWebFrame.CurrentValue >= vMax Then vMax = IT(i).PanelNo(PanelIndex).TWebFrame.CurrentValue
                If Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryFrames.WebThickness <= vMin Then vMin = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryFrames.WebThickness
                If Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryFrames.WebThickness >= vMax Then vMax = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryFrames.WebThickness
            End If
        Next i
    End If
    If chkBox(4) Then
        For i = 0 To NoOfIte
            If IT(1).PanelNo(PanelIndex).WFlaFrame.InfEdge > 0 Then
                If IT(i).PanelNo(PanelIndex).WFlaFrame.CurrentValue <= vMin Then vMin = IT(i).PanelNo(PanelIndex).WFlaFrame.CurrentValue
                If IT(i).PanelNo(PanelIndex).WFlaFrame.CurrentValue >= vMax Then vMax = IT(i).PanelNo(PanelIndex).WFlaFrame.CurrentValue
                If Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryFrames.FlangeWidth <= vMin Then vMin = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryFrames.FlangeWidth
                If Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryFrames.FlangeWidth >= vMax Then vMax = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryFrames.FlangeWidth
            End If
        Next i
    End If
    If chkBox(5) Then
        For i = 0 To NoOfIte
            If IT(1).PanelNo(PanelIndex).SpcFrame.InfEdge > 0 Then
                If IT(i).PanelNo(PanelIndex).SpcFrame.CurrentValue <= vMin Then vMin = IT(i).PanelNo(PanelIndex).SpcFrame.CurrentValue
                If IT(i).PanelNo(PanelIndex).SpcFrame.CurrentValue >= vMax Then vMax = IT(i).PanelNo(PanelIndex).SpcFrame.CurrentValue
                If Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryFrames.Spacing <= vMin Then vMin = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryFrames.Spacing
                If Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryFrames.Spacing >= vMax Then vMax = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryFrames.Spacing
            End If
        Next i
    End If
    If chkBox(6) Then
        For i = 0 To NoOfIte
            If IT(1).PanelNo(PanelIndex).HWebStiff.InfEdge > 0 Then
                If IT(i).PanelNo(PanelIndex).HWebStiff.CurrentValue <= vMin Then vMin = IT(i).PanelNo(PanelIndex).HWebStiff.CurrentValue
                If IT(i).PanelNo(PanelIndex).HWebStiff.CurrentValue >= vMax Then vMax = IT(i).PanelNo(PanelIndex).HWebStiff.CurrentValue
                If Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryStiffeners.WebHeight <= vMin Then vMin = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryStiffeners.WebHeight
                If Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryStiffeners.WebHeight >= vMax Then vMax = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryStiffeners.WebHeight
            End If
        Next i
    End If
    If chkBox(7) Then
        For i = 0 To NoOfIte
            If IT(1).PanelNo(PanelIndex).TWebStiff.InfEdge > 0 Then
                If IT(i).PanelNo(PanelIndex).TWebStiff.CurrentValue <= vMin Then vMin = IT(i).PanelNo(PanelIndex).TWebStiff.CurrentValue
                If IT(i).PanelNo(PanelIndex).TWebStiff.CurrentValue >= vMax Then vMax = IT(i).PanelNo(PanelIndex).TWebStiff.CurrentValue
                If Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryStiffeners.WebThickness <= vMin Then vMin = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryStiffeners.WebThickness
                If Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryStiffeners.WebThickness >= vMax Then vMax = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryStiffeners.WebThickness
            End If
        Next i
    End If
    If chkBox(8) Then
        For i = 0 To NoOfIte
            If IT(1).PanelNo(PanelIndex).WFlaStiff.InfEdge > 0 Then
                If IT(i).PanelNo(PanelIndex).WFlaStiff.CurrentValue <= vMin Then vMin = IT(i).PanelNo(PanelIndex).WFlaStiff.CurrentValue
                If IT(i).PanelNo(PanelIndex).WFlaStiff.CurrentValue >= vMax Then vMax = IT(i).PanelNo(PanelIndex).WFlaStiff.CurrentValue
                If Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryStiffeners.FlangeWidth <= vMin Then vMin = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryStiffeners.FlangeWidth
                If Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryStiffeners.FlangeWidth >= vMax Then vMax = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryStiffeners.FlangeWidth
            End If
        Next i
    End If
    If chkBox(9) Then
        For i = 0 To NoOfIte
            If IT(1).PanelNo(PanelIndex).SpcStiff.InfEdge > 0 Then
                If IT(i).PanelNo(PanelIndex).SpcStiff.CurrentValue <= vMin Then vMin = IT(i).PanelNo(PanelIndex).SpcStiff.CurrentValue
                If IT(i).PanelNo(PanelIndex).SpcStiff.CurrentValue >= vMax Then vMax = IT(i).PanelNo(PanelIndex).SpcStiff.CurrentValue
                If Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryStiffeners.Spacing <= vMin Then vMin = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryStiffeners.Spacing
                If Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryStiffeners.Spacing >= vMax Then vMax = Project.Item(ProjectIndex).colPanel.Item(PanelIndex).cScantlings.cPrimaryStiffeners.Spacing
            End If
        Next i
    End If
    Exit Sub
1
    'MsgBox "File not read succesfully!", vbCritical + vbOKOnly
End Sub

Private Sub GetLeftRight()
    On Error GoTo 1
    Dim i As Integer
    shW = Shape1.Width
    shL = Shape1.Width - Shape1.Left
    shH = Shape1.Height - Shape1.Top
    shT = Shape1.Top
    lSlice = CLng(shW / (NoOfIte))
    ReDim lXs(0 To NoOfIte)
    lXs(0) = Shape1.Left
    For i = 1 To NoOfIte
        lXs(i) = lXs(i - 1) + lSlice
    Next i
    Exit Sub
1
    'MsgBox "File not read succesfully!", vbCritical + vbOKOnly

End Sub

Private Sub GenerateCheckBoxes()
    Dim i As Integer
    For i = 1 To nChk
        Load chkBox(i)
        chkBox(i).Visible = True
    Next i
    
    chkBox(1).Caption = "Plate Thickness"
    chkBox(2).Caption = "Frames Web Height"
    chkBox(3).Caption = "Frames Web Thickness"
    chkBox(4).Caption = "Frames Flange Wide"
    chkBox(5).Caption = "Frames Spacing"
    chkBox(6).Caption = "Stiffeners Web Height"
    chkBox(7).Caption = "Stiffeners Web Thickness"
    chkBox(8).Caption = "Stiffeners Flange Wide"
    chkBox(9).Caption = "Stiffeners Spacing"
    chkBox(10).Caption = "Show Text"
    
    chkBox(1).ToolTipText = "PT"
    chkBox(2).ToolTipText = "FWH"
    chkBox(3).ToolTipText = "FWT"
    chkBox(4).ToolTipText = "FFW"
    chkBox(5).ToolTipText = "FS"
    chkBox(6).ToolTipText = "SWH"
    chkBox(7).ToolTipText = "SWT"
    chkBox(8).ToolTipText = "SFW"
    chkBox(9).ToolTipText = "SS"
    chkBox(10).Value = vbChecked
    If IANA = 2 Then
        chkBox(2).Visible = False
        chkBox(3).Visible = False
        chkBox(4).Visible = False
        chkBox(5).Caption = "Stiffeners Span"
    End If
    
End Sub

Private Sub FillPanelList()
    On Error GoTo 1
    Dim i As Integer
    For i = 1 To Project.Item(ProjectIndex).colPanel.Count
        PanelList.AddItem "Panel " & i
    Next i
    PanelList.ListIndex = 0
1
End Sub

Private Sub Form_Unload(Cancel As Integer)
    fMainForm.SetFocus
End Sub

Private Sub mnuPrint_Click()
    On Error GoTo PrintErr
    Printer.Print
    Printer.PaintPicture Pic.Image, 0, 0
    Printer.EndDoc
    Exit Sub
PrintErr:
    Call RaiseError(MyUnhandledError, Err.Description)
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

Private Sub PanelList_Click()
    PanelIndex = PanelList.ListIndex + 1
    GetMinMax
    GetItemsToDraw
End Sub

Public Sub ReadFile()
    Dim i As Integer, j As Integer, k As Integer, l As Integer, find As String
    ReDim IT(0 To NoOfIte)
    On Error GoTo 1
    For i = 0 To UBound(IT)
        ReDim IT(i).PanelNo(1 To Project.Item(ProjectIndex).colPanel.Count)
    Next i
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
    'ITERATION nø
    For i = 0 To NoOfIte
        For Each Panel In Project.Item(ProjectIndex).colPanel
            If Panel.colDesignVariables.ActiveVariablesCount > 0 Then
                find = ""
                Do Until find = "Variables_:" 'Or ts.AtEndOfStream = True
                    sLine = ReadLn(ts)
                    find = Left(sLine, 11)
                Loop
                Dim nNXI As Integer
                Dim mNXI() As Variant
                'nNXI = Panel.colDesignVariables.Count
                nNXI = Panel.colDesignVariables.ActiveVariablesCount
                If nNXI > 0 Then
                    GetValues1 nNXI, sLine, v
'                    For k = 1 To nNXI
'                        For l = 1 To 9
'                            If l = Panel.colDesignVariables.Item(k).VariableName Then
'                                setValue i, Panel.pNumber, k, l, v
'                            End If
'                        Next l
'                    Next k
                    k = 0
                    For l = 1 To 9
                        If Panel.colDesignVariables.Item(l).Active = True Then
                            k = k + 1
                            setValue i, Panel.pNumber, k, l, v
                        End If
                    Next
                End If
            End If
        Next Panel
    Next i
    ts.Close
    Exit Sub
1
    MsgBox "File not read succesfully!", vbCritical + vbOKOnly
End Sub

Private Sub setValue(i As Integer, j As Integer, _
                    k As Integer, l As Integer, v As Variant)
    On Error GoTo 1
    Select Case l
        Case 1
            IT(i).PanelNo(j).PlateThick.CurrentValue = Val_(v(k))
            IT(i).PanelNo(j).PlateThick.InfEdge = Project.Item(ProjectIndex).colPanel.Item(j).colDesignVariables.Item(k).LowerLimit
            IT(i).PanelNo(j).PlateThick.SupEdge = Project.Item(ProjectIndex).colPanel.Item(j).colDesignVariables.Item(k).UpperLimit
        Case 2
            IT(i).PanelNo(j).HWebFrame.CurrentValue = Val_(v(k))
            IT(i).PanelNo(j).HWebFrame.InfEdge = Project.Item(ProjectIndex).colPanel.Item(j).colDesignVariables.Item(k).LowerLimit
            IT(i).PanelNo(j).HWebFrame.SupEdge = Project.Item(ProjectIndex).colPanel.Item(j).colDesignVariables.Item(k).UpperLimit
        Case 3
            IT(i).PanelNo(j).TWebFrame.CurrentValue = Val_(v(k))
            IT(i).PanelNo(j).TWebFrame.InfEdge = Project.Item(ProjectIndex).colPanel.Item(j).colDesignVariables.Item(k).LowerLimit
            IT(i).PanelNo(j).TWebFrame.SupEdge = Project.Item(ProjectIndex).colPanel.Item(j).colDesignVariables.Item(k).UpperLimit
        Case 4
            IT(i).PanelNo(j).WFlaFrame.CurrentValue = Val_(v(k))
            IT(i).PanelNo(j).WFlaFrame.InfEdge = Project.Item(ProjectIndex).colPanel.Item(j).colDesignVariables.Item(k).LowerLimit
            IT(i).PanelNo(j).WFlaFrame.SupEdge = Project.Item(ProjectIndex).colPanel.Item(j).colDesignVariables.Item(k).UpperLimit
        Case 5
            IT(i).PanelNo(j).SpcFrame.CurrentValue = Val_(v(k))
            IT(i).PanelNo(j).SpcFrame.InfEdge = Project.Item(ProjectIndex).colPanel.Item(j).colDesignVariables.Item(k).LowerLimit
            IT(i).PanelNo(j).SpcFrame.SupEdge = Project.Item(ProjectIndex).colPanel.Item(j).colDesignVariables.Item(k).UpperLimit
        Case 6
            IT(i).PanelNo(j).HWebStiff.CurrentValue = Val_(v(k))
            IT(i).PanelNo(j).HWebStiff.InfEdge = Project.Item(ProjectIndex).colPanel.Item(j).colDesignVariables.Item(k).LowerLimit
            IT(i).PanelNo(j).HWebStiff.SupEdge = Project.Item(ProjectIndex).colPanel.Item(j).colDesignVariables.Item(k).UpperLimit
        Case 7
            IT(i).PanelNo(j).TWebStiff.CurrentValue = Val_(v(k))
            IT(i).PanelNo(j).TWebStiff.InfEdge = Project.Item(ProjectIndex).colPanel.Item(j).colDesignVariables.Item(k).LowerLimit
            IT(i).PanelNo(j).TWebStiff.SupEdge = Project.Item(ProjectIndex).colPanel.Item(j).colDesignVariables.Item(k).UpperLimit
        Case 8
            IT(i).PanelNo(j).WFlaStiff.CurrentValue = Val_(v(k))
            IT(i).PanelNo(j).WFlaStiff.InfEdge = Project.Item(ProjectIndex).colPanel.Item(j).colDesignVariables.Item(k).LowerLimit
            IT(i).PanelNo(j).WFlaStiff.SupEdge = Project.Item(ProjectIndex).colPanel.Item(j).colDesignVariables.Item(k).UpperLimit
        Case 9
            IT(i).PanelNo(j).SpcStiff.CurrentValue = Val_(v(k))
            IT(i).PanelNo(j).SpcStiff.InfEdge = Project.Item(ProjectIndex).colPanel.Item(j).colDesignVariables.Item(k).LowerLimit
            IT(i).PanelNo(j).SpcStiff.SupEdge = Project.Item(ProjectIndex).colPanel.Item(j).colDesignVariables.Item(k).UpperLimit
    End Select
    Exit Sub
1
    'MsgBox "File not read succesfully!", vbCritical + vbOKOnly

End Sub
