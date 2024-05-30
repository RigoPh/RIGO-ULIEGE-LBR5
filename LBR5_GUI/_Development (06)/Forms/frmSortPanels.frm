VERSION 5.00
Object = "{0D452EE1-E08F-101A-852E-02608C4D0BB4}#2.0#0"; "FM20.DLL"
Begin VB.Form frmSortPanels 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Sort Panels"
   ClientHeight    =   5085
   ClientLeft      =   6255
   ClientTop       =   2820
   ClientWidth     =   4635
   Icon            =   "frmSortPanels.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   5085
   ScaleWidth      =   4635
   ShowInTaskbar   =   0   'False
   Begin VB.PictureBox picChageOrder 
      Height          =   435
      Left            =   2760
      ScaleHeight     =   375
      ScaleWidth      =   1755
      TabIndex        =   8
      Top             =   3960
      Width           =   1815
      Begin MSForms.SpinButton btnSpin 
         Height          =   375
         Left            =   0
         TabIndex        =   10
         Top             =   0
         Width           =   375
         Size            =   "661;661"
         Orientation     =   0
      End
      Begin VB.Label lblChangeOrder 
         AutoSize        =   -1  'True
         Caption         =   "Change Order"
         Height          =   195
         Left            =   480
         TabIndex        =   9
         Top             =   75
         UseMnemonic     =   0   'False
         Width           =   990
      End
   End
   Begin VB.ListBox LstSort 
      Height          =   3765
      Left            =   2760
      TabIndex        =   3
      Top             =   120
      Width           =   1695
   End
   Begin VB.ListBox lstPanel 
      Height          =   3765
      Left            =   240
      TabIndex        =   2
      Top             =   120
      Width           =   1695
   End
   Begin MSForms.CommandButton cmdMoveAllLeft 
      Height          =   375
      Left            =   2040
      TabIndex        =   7
      Top             =   2280
      Width           =   615
      Caption         =   "<<"
      PicturePosition =   327683
      Size            =   "1085;661"
      FontEffects     =   1073741825
      FontHeight      =   165
      FontCharSet     =   0
      FontPitchAndFamily=   2
      ParagraphAlign  =   3
      FontWeight      =   700
   End
   Begin MSForms.CommandButton cmdMoveAllRight 
      Height          =   375
      Left            =   2040
      TabIndex        =   6
      Top             =   840
      Width           =   615
      Caption         =   ">>"
      PicturePosition =   327683
      Size            =   "1085;661"
      FontEffects     =   1073741825
      FontHeight      =   165
      FontCharSet     =   0
      FontPitchAndFamily=   2
      ParagraphAlign  =   3
      FontWeight      =   700
   End
   Begin MSForms.CommandButton cmdMoveLeft 
      Default         =   -1  'True
      Height          =   375
      Left            =   2040
      TabIndex        =   5
      Top             =   1800
      Width           =   615
      Caption         =   "<"
      PicturePosition =   327683
      Size            =   "1085;661"
      FontEffects     =   1073741825
      FontHeight      =   165
      FontCharSet     =   0
      FontPitchAndFamily=   2
      ParagraphAlign  =   3
      FontWeight      =   700
   End
   Begin MSForms.CommandButton cmdMoveRight 
      Height          =   375
      Left            =   2040
      TabIndex        =   4
      Top             =   1320
      Width           =   615
      Caption         =   ">"
      PicturePosition =   327683
      Size            =   "1085;661"
      FontEffects     =   1073741825
      FontHeight      =   165
      FontCharSet     =   0
      FontPitchAndFamily=   2
      ParagraphAlign  =   3
      FontWeight      =   700
   End
   Begin MSForms.CommandButton cmdOK 
      Height          =   375
      Left            =   2280
      TabIndex        =   1
      Top             =   4560
      Width           =   1095
      Caption         =   "OK"
      PicturePosition =   327683
      Size            =   "1931;661"
      FontHeight      =   165
      FontCharSet     =   0
      FontPitchAndFamily=   2
      ParagraphAlign  =   3
   End
   Begin MSForms.CommandButton cmdCancel 
      Cancel          =   -1  'True
      Height          =   375
      Left            =   3480
      TabIndex        =   0
      Top             =   4560
      Width           =   1095
      Caption         =   "Cancel"
      PicturePosition =   327683
      Size            =   "1931;661"
      FontHeight      =   165
      FontCharSet     =   0
      FontPitchAndFamily=   2
      ParagraphAlign  =   3
   End
End
Attribute VB_Name = "frmSortPanels"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Dim ProjectIndex As Integer
Dim colPanel As New Collection
Dim colSortedPanel As New Collection
Dim colDH As New Collection
Dim sSelectedStr As String
Dim sUpperStr As String
Dim sLowerStr As String

Private Sub cmdCancel_Click()
    Unload Me
End Sub

Private Sub cmdMoveAllLeft_Click()
    Dim i As Integer
    Dim v() As Variant
    Dim sData As String
    For i = 1 To LstSort.ListCount
        sData = LstSort.List(i - 1)
        GetValues 2, sData, v
        lstPanel.AddItem "Panel " & CInt(v(2))
    Next i
    LstSort.Clear
    LstSort_Click

End Sub

Private Sub cmdMoveAllRight_Click()
    Dim i As Integer
    Dim v() As Variant
    Dim sData As String
    For i = 1 To lstPanel.ListCount
        sData = lstPanel.List(i - 1)
        GetValues 2, sData, v
        LstSort.AddItem "Panel " & CInt(v(2))
    Next i
    lstPanel.Clear
    LstSort_Click
End Sub

Private Sub cmdMoveLeft_Click()
    Dim sData As String
    Dim v() As Variant
    If LstSort.ListIndex > -1 Then
        sData = LstSort.Text
        GetValues 2, sData, v
        lstPanel.AddItem "Panel " & CInt(v(2))
        LstSort.RemoveItem LstSort.ListIndex
    End If

End Sub

Private Sub cmdMoveLeft_DblClick(Cancel As MSForms.ReturnBoolean)
    Cancel = True
End Sub

Private Sub cmdMoveRight_Click()
    Dim sData As String
    Dim v() As Variant
    If lstPanel.ListIndex > -1 Then
        sData = lstPanel.Text
        GetValues 2, sData, v
        LstSort.AddItem "Panel " & CInt(v(2))
        lstPanel.RemoveItem lstPanel.ListIndex
    End If
    LstSort_Click
End Sub

Private Sub cmdMoveRight_DblClick(Cancel As MSForms.ReturnBoolean)
    Cancel = True
End Sub

Public Function UpdateCostCAtDHullSort(ByVal iPanel As Integer, ByVal iNewPanel As Integer)
    'On Error GoTo  UpdateCostCAtDHullSortErr
    Dim cDH As cCostCAtDHull, cIndex As cIndex
    For Each cDH In Project.Item(ProjectIndex).colCostCAtDHull
        For Each cIndex In cDH.InnerShell
            If cIndex.Number = iPanel Then
                'cIndex.Number = iNewPanel
                colDH.Item(cDH.index).InnerShell.Item(cIndex.index).Number = iNewPanel
            End If
        Next cIndex
        For Each cIndex In cDH.OuterShell
            If cIndex.Number = iPanel Then
                'cIndex.Number = iNewPanel
                colDH.Item(cDH.index).OuterShell.Item(cIndex.index).Number = iNewPanel
            End If
        Next cIndex
    Next cDH
    Exit Function
UpdateCostCAtDHullSortErr:
Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "frmSortPanels: Function UpdateCostCAtDHullSort")
End Function

Private Sub cmdOK_Click()
    Dim cPanel As cPanel
    Dim i As Integer, j As Integer
    Dim v() As Variant
    Dim sData As String
    Dim index As Integer
    Dim iRelDH As Integer
    If LstSort.ListCount = colPanel.Count Then
        For i = 1 To LstSort.ListCount
            sData = LstSort.List(i - 1)
            GetValues 2, sData, v
            index = CInt(v(2))
            Set cPanel = colPanel.Item(index)
            UpdateCostCAtDHullSort i, CInt(v(2))
            cPanel.index = i
            cPanel.pNumber = i
            If cPanel.pType = DoubleHull Then
                For j = 1 To LstSort.ListCount
                    sData = LstSort.List(j - 1)
                    GetValues 2, sData, v
                    If CInt(v(2)) = cPanel.RelatedDoubleHullPanel Then
                    'If CInt(v(2)) = Project.Item(ProjectIndex).colPanel.Item(cPanel.Index).RelatedDoubleHullPanel Then 'cPanel.RelatedDoubleHullPanel Then
                        cPanel.RelatedDoubleHullPanel = j
                    End If
                Next j
            End If
            colSortedPanel.Add cPanel
            Set cPanel = Nothing
        Next i
    Else
        MsgBox "All panels must be moved on the right side.", vbCritical + vbOKOnly
        Exit Sub
    End If
    
    Set Project.Item(ProjectIndex).colPanel = Nothing
    For Each cPanel In colSortedPanel
        Project.Item(ProjectIndex).colPanel.Add cPanel, cPanel.pNumber
    Next cPanel
    Dim cDH As cCostCAtDHull, colDH1 As New colCostCAtDHull
    For Each cDH In colDH
        colDH1.Add cDH, cDH.index
    Next
    Set Project.Item(ProjectIndex).colCostCAtDHull = colDH1
    Set colDH1 = Nothing

    UpdatePanelConnections ProjectIndex
    UpdateBoundary ProjectIndex
    Draw ProjectIndex
    Project.Item(ProjectIndex).DataChanged = True
    Unload Me
End Sub

Private Sub Form_Load()
    ProjectIndex = ActiveProject
    Me.Move (Screen.Width - Me.Width) / 2, (Screen.Height - Me.Height) / 2
    cmdOK.Picture = LoadResPicture("ID_BITMAP_OK", 0)
    cmdCancel.Picture = LoadResPicture("ID_BITMAP_CANCEL", 0)
    Me.Caption = "Sort Panels - [" & GetFileName(Project.Item(ProjectIndex).sFileName) & "]"
    GetData
End Sub

Private Sub GetData()
    Dim cPanel As cPanel
    For Each cPanel In Project.Item(ProjectIndex).colPanel
        colPanel.Add cPanel.Clone
        lstPanel.AddItem "Panel " & cPanel.pNumber
    Next cPanel
    Dim cDH0 As cCostCAtDHull
    For Each cDH0 In Project.Item(ProjectIndex).colCostCAtDHull
        colDH.Add cDH0.Clone
    Next cDH0
End Sub

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
    Set colPanel = Nothing
    Set colSortedPanel = Nothing
    Set colDH = Nothing
End Sub

Private Sub Form_Resize()
    Me.Height = cmdOK.Top + cmdOK.Height + 600
End Sub

Private Sub lstPanel_DblClick()
    cmdMoveRight_Click
End Sub

Private Sub LstSort_Click()
    sSelectedStr = LstSort.Text
    If LstSort.ListIndex > 0 Then
        sLowerStr = LstSort.List(LstSort.ListIndex - 1)
    End If
    If LstSort.ListIndex < LstSort.ListCount - 1 Then
        sUpperStr = LstSort.List(LstSort.ListIndex + 1)
    End If
End Sub

Private Sub LstSort_DblClick()
    cmdMoveLeft_Click
End Sub

Private Sub btnSpin_SpinDown()
    If LstSort.ListIndex < 0 Then Exit Sub
    If LstSort.ListIndex < LstSort.ListCount - 1 Then
        LstSort.List(LstSort.ListIndex) = sUpperStr
        LstSort.List(LstSort.ListIndex + 1) = sSelectedStr
        LstSort.Selected(LstSort.ListIndex + 1) = True
        LstSort_Click
    End If
End Sub

Private Sub btnSpin_SpinUp()
    If LstSort.ListIndex < 0 Then Exit Sub
    If LstSort.ListIndex > 0 Then
        LstSort.List(LstSort.ListIndex) = sLowerStr
        LstSort.List(LstSort.ListIndex - 1) = sSelectedStr
        LstSort.Selected(LstSort.ListIndex - 1) = True
        LstSort_Click
    End If
End Sub

