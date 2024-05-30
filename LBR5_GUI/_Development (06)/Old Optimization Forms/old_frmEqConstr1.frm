VERSION 5.00
Begin VB.Form old_frmEqConstr1 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Equality Restrictions"
   ClientHeight    =   7170
   ClientLeft      =   5460
   ClientTop       =   2340
   ClientWidth     =   6945
   Icon            =   "old_frmEqConstr1.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   7170
   ScaleWidth      =   6945
   ShowInTaskbar   =   0   'False
   Begin VB.Frame Frame1 
      Height          =   3495
      Left            =   1800
      TabIndex        =   17
      Top             =   240
      Width           =   2175
      Begin VB.Label Title 
         AutoSize        =   -1  'True
         Caption         =   "Title"
         Height          =   195
         Index           =   0
         Left            =   120
         TabIndex        =   18
         Top             =   240
         Visible         =   0   'False
         Width           =   300
      End
   End
   Begin VB.CommandButton cmdRemove 
      Caption         =   "Remove Selected"
      Height          =   375
      Left            =   3720
      TabIndex        =   15
      Top             =   6600
      Width           =   1455
   End
   Begin VB.CommandButton cmdListView 
      Caption         =   "Normal View"
      Height          =   375
      Left            =   1800
      TabIndex        =   10
      Top             =   6600
      Width           =   1455
   End
   Begin VB.ListBox EQAltList 
      Height          =   1815
      ItemData        =   "old_frmEqConstr1.frx":000C
      Left            =   240
      List            =   "old_frmEqConstr1.frx":000E
      MultiSelect     =   2  'Extended
      TabIndex        =   9
      Top             =   4560
      Visible         =   0   'False
      Width           =   6495
   End
   Begin VB.ListBox EQList 
      Height          =   1815
      Left            =   240
      MultiSelect     =   2  'Extended
      TabIndex        =   4
      Top             =   4560
      Width           =   6495
   End
   Begin VB.CommandButton cmdExpand 
      Caption         =   "Expand List >>"
      Height          =   375
      Left            =   240
      TabIndex        =   8
      Top             =   6600
      Width           =   1455
   End
   Begin VB.CommandButton CmdShowList 
      Caption         =   "Show List >>"
      Height          =   375
      Left            =   240
      TabIndex        =   7
      Top             =   3960
      Width           =   1455
   End
   Begin VB.CommandButton cmdAdd 
      Caption         =   "="
      Height          =   375
      Left            =   4200
      TabIndex        =   6
      Top             =   1680
      Width           =   855
   End
   Begin VB.CommandButton cmdMode 
      Caption         =   "View Mode 1"
      Height          =   375
      Left            =   1920
      TabIndex        =   5
      Top             =   3960
      Visible         =   0   'False
      Width           =   1455
   End
   Begin VB.ListBox DepList 
      Height          =   3375
      Left            =   240
      MultiSelect     =   2  'Extended
      TabIndex        =   3
      Top             =   360
      Width           =   1455
   End
   Begin VB.CommandButton cmdApply 
      Caption         =   "&Apply"
      Default         =   -1  'True
      Height          =   345
      Left            =   4440
      TabIndex        =   2
      Top             =   3960
      Width           =   1125
   End
   Begin VB.CommandButton cmdClose 
      Cancel          =   -1  'True
      Caption         =   "&Close"
      Height          =   345
      Left            =   5640
      TabIndex        =   1
      Top             =   3960
      Width           =   1125
   End
   Begin VB.ListBox IndList 
      Height          =   3375
      Left            =   5280
      TabIndex        =   0
      Top             =   360
      Width           =   1455
   End
   Begin VB.TextBox txtRatio 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      Height          =   285
      Left            =   4200
      TabIndex        =   13
      Text            =   "1"
      Top             =   1200
      Width           =   855
   End
   Begin VB.Label CountNegalt 
      Alignment       =   2  'Center
      Caption         =   "Count Restrictions"
      Height          =   255
      Left            =   3960
      TabIndex        =   16
      Top             =   3480
      Width           =   1335
   End
   Begin VB.Label Label2 
      Alignment       =   2  'Center
      AutoSize        =   -1  'True
      Caption         =   "Independent Variables"
      Height          =   195
      Left            =   5160
      TabIndex        =   12
      Top             =   120
      Width           =   1710
   End
   Begin VB.Label Label1 
      AutoSize        =   -1  'True
      Caption         =   "Dependent Variables"
      Height          =   195
      Left            =   240
      TabIndex        =   11
      Top             =   120
      Width           =   1485
   End
   Begin VB.Label Label3 
      Alignment       =   2  'Center
      Caption         =   "Ratio"
      Height          =   255
      Left            =   4200
      TabIndex        =   14
      Top             =   960
      Width           =   855
   End
   Begin VB.Menu mnuEQ 
      Caption         =   "EQ Edit"
      Begin VB.Menu mnuEQEdit 
         Caption         =   "Edit"
      End
      Begin VB.Menu mnuEQRemove 
         Caption         =   "Remove"
      End
   End
End
Attribute VB_Name = "old_frmEqConstr1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Base 1
Dim IndIndex As Integer
Dim DepIndex As Integer
Dim EQIndex As Integer
Dim Vct0() As Variant
Dim Vct1() As Variant
Dim vctIndex() As Variant
Dim strEQList() As Variant
Dim StrBlank As String
Dim StrTrunc() As Variant
Dim StrEQConstr() As Variant
Dim vctRestr() As Variant
Dim intNEGALT As Integer
Dim str1 As String
Dim str2 As String
Dim str3 As String
Dim str4 As String
Dim str5 As String
Dim Comments() As Variant
Dim ShowList As Boolean
Dim ExpandList As Boolean
Dim ListView As Boolean
Dim varRatio As Double
Dim ProjectIndex As Integer
Dim NETO As Integer

Private Sub cmdAdd_Click()
    Dim i As Integer: Dim j As Integer: Dim k As Integer
    Dim StrAdd As String
    For i = 1 To NETO
        If DepList.Selected(i - 1) = True Then
            For j = 1 To 9
                If Title(j).FontUnderline = True Then
                    StrAdd = j & "   " & i & "   " & j & _
                            "   " & IndIndex & "   " & Val_(txtRatio.Text)
                    For k = 1 To intNEGALT
                        Strings k, str1, str2, str3, str4, str5
                        'If StrEQConstr(k) = StrAdd Then
                        If Val_(str1) = j And Val_(str2) = i Then 'And (str5) = txtRatio.Text Then
                            GoTo 1
                        End If
                        If Val_(str1) = j And Val_(str2) = IndIndex Then
                            GoTo 1
                        End If
                        If Val_(str3) = j And Val_(str4) = i Then
                            GoTo 1
                        End If
                    Next k
                    intNEGALT = intNEGALT + 1
                    ReDim Preserve StrEQConstr(1 To intNEGALT)
                    StrEQConstr(UBound(StrEQConstr)) = StrAdd
                    Strings intNEGALT, str1, str2, str3, str4, str5
                    ReDim Preserve vctRestr(1 To intNEGALT)
                    vctRestr(UBound(vctRestr)) = Array(Val_(str1), Val_(str2), Val_(str3), Val_(str4), Val_(str5))
                    'PopulateEQList
                End If
1
            Next j
        End If
    Next i
    
    SortList vctRestr
    PopulateEQList

    txtRatio.Text = "1"
End Sub

Private Sub cmdApply_Click()
    Dim i As Integer
    Dim cEq As cEqualityRestrictions
    Dim v() As Variant
    
    If EQAltList.ListCount > Licensing.MAX_EQUALITY_RESTRICTIONS Then
        MsgBox "The maximum number of equality restrictions is restricted to " & Licensing.MAX_EQUALITY_RESTRICTIONS & ".", vbCritical + vbOKOnly, "LBR-5 License Limitation"
        Exit Sub
    End If
    
    Set Project.Item(ProjectIndex).cHeader.colEqualityRestrictions = Nothing
    If EQAltList.ListCount > 0 Then
        ReDim StrEQConstr(1 To EQAltList.ListCount)
        For i = 1 To EQAltList.ListCount
            StrEQConstr(i) = EQAltList.List(i - 1)
            GetValues 5, CStr(StrEQConstr(i)), v
            Set cEq = New cEqualityRestrictions
            cEq.index = i
            cEq.DependingDesignVariable = Val_(v(1))
            cEq.DependingPanel = Val_(v(2))
            cEq.LeadingDesignVariable = Val_(v(3))
            cEq.LeadingPanel = Val_(v(4))
            cEq.Ratio = Val_(v(5))
            Project.Item(ProjectIndex).cHeader.colEqualityRestrictions.Add cEq, i
            Set cEq = Nothing
        Next i
    End If
    Project.Item(ProjectIndex).DataChanged = True
    
'''    If EQAltList.ListCount > 0 Then
'''        ReDim StrEQConstr(1 To EQAltList.ListCount)
'''        For i = 1 To EQAltList.ListCount '- 1
'''            StrEQConstr(i) = EQAltList.List(i - 1)
'''        Next i
'''        header.EQConstr = StrEQConstr
'''        header.NEGALT = EQAltList.ListCount
'''    Else
'''        header.NEGALT = 0
'''    End If
'''    'Label5.Caption = List5.ListCount

End Sub

Private Sub cmdClose_Click()
    Unload Me
End Sub

Private Sub cmdExpand_Click()
    If ExpandList = False Then
        ExpandList = True
        EQList.Top = 320
        EQList.Height = 6100
        EQAltList.Top = 320
        EQAltList.Height = 6100
        cmdExpand.Caption = "<< Restore List"
    ElseIf ExpandList = True Then
        ExpandList = False
        EQList.Top = 4560
        EQList.Height = 1815
        EQAltList.Top = 4560
        EQAltList.Height = 1815
        cmdExpand.Caption = "Expand List >>"
    End If
End Sub

Private Sub cmdListView_Click()
    Dim i As Integer
    For i = 1 To EQList.ListCount
        EQList.Selected(i - 1) = False
    Next i
    If ListView = False Then
        ListView = True
        EQAltList.Visible = True
        cmdListView.Caption = "Normal View"
    ElseIf ListView = True Then
        ListView = False
        EQAltList.Visible = False
        cmdListView.Caption = "Numeric View"
    End If
End Sub

'Private Sub cmdMode_Click()
'    Unload Me
'    Load old_frmEqConstr1
'    old_frmEqConstr1.Show 1
'End Sub

Private Sub cmdRemove_Click()
    Dim i As Integer
1
    For i = 1 To EQList.ListCount
        If EQList.Selected(i - 1) = True Then
            EQAltList.Selected(i - 1) = True
            EQList.RemoveItem i - 1
            EQAltList.RemoveItem i - 1
            intNEGALT = intNEGALT - 1
            GoTo 1
        End If
    Next i
    If EQList.ListCount > 0 Then
        ReDim StrEQConstr(1 To intNEGALT)
        For i = 1 To EQAltList.ListCount
            StrEQConstr(i) = EQAltList.List(i - 1)
        Next i
        For i = 1 To intNEGALT
            Strings i, str1, str2, str3, str4, str5
            ReDim Preserve vctRestr(1 To intNEGALT)
            vctRestr(i) = Array(Val_(str1), Val_(str2), Val_(str3), Val_(str4), Val_(str5))
        Next i
    End If
    PopulateEQList
    EQList.ListIndex = -1
    EQAltList.ListIndex = -1

End Sub

Private Sub CmdShowList_Click()
    If ShowList = False Then
        ShowList = True
        Me.Height = 7560
        CmdShowList.Caption = "<< Hide List"
    ElseIf ShowList = True Then
        ShowList = False
        Me.Height = 4965
        CmdShowList.Caption = "Show List >>"
    
    End If
End Sub

Private Sub DepList_Click()
    TitlesState
End Sub

Private Sub DepList_MouseDown(Button As Integer, Shift As Integer, x As Single, y As Single)
'Select Case Button
'    Case 2
'        PopupMenu mnuPopUp
'End Select
End Sub

Private Sub DepList_MouseUp(Button As Integer, Shift As Integer, x As Single, y As Single)
    TitlesState
End Sub

Private Sub EQList_DblClick()
EQIndex = EQList.ListIndex + 1
mnuEQRemove_Click
End Sub

Private Sub EQList_MouseDown(Button As Integer, Shift As Integer, x As Single, y As Single)
EQIndex = EQList.ListIndex + 1

'Select Case Button
'    Case 2
'        PopupMenu mnuEQ
'End Select
End Sub

Private Sub LoadTitles()
    Dim i As Integer
    Dim Height As Long
    Height = Title(0).Top
    Dim index As Integer
    index = 0
    For i = 1 To 9
        Load Title(i)
        Title(i).Visible = True
        Select Case i
            Case 1
                Title(i).Caption = "Plate Thickness"
            Case 2
                Title(i).Caption = "Frames Web Height"
            Case 3
                Title(i).Caption = "Frames Web Thickness"
            Case 4
                Title(i).Caption = "Frames Flange Wide"
            Case 5
                Title(i).Caption = "Frames Spacing"
            Case 6
                Title(i).Caption = "Stiffeners Web Height"
            Case 7
                Title(i).Caption = "Stiffeners Web Thickness"
            Case 8
                Title(i).Caption = "Stiffeners Flange Wide"
            Case 9
                Title(i).Caption = "Stiffeners Spacing"
        End Select
        If Project.Item(ProjectIndex).cHeader.IANA = 2 Then
            Select Case i
                Case 2, 3, 4
                    Title(i).Visible = False
                Case 5
                    Title(i).Caption = "Stiffeners Span"
            End Select
        End If
        If Title(i).Visible = True Then
            index = index + 1
            Height = Title(0).Top + 360 * (index - 1)
            Title(i).Top = Height
        End If
    Next i
End Sub

Private Sub Form_Load()
    Dim i As Integer
    If Licensing.IS_MODIFY_EQUALITY_CONSTRAINTS = False Then
        cmdApply.Enabled = False
    End If
    ProjectIndex = ActiveProject
    Me.Move (Screen.Width - Me.Width) / 2, (Screen.Height - Me.Height) / 2
'    cmdOK.Picture = LoadResPicture("ID_BITMAP_OK", 0)
'    cmdCancel.Picture = LoadResPicture("ID_BITMAP_CANCEL", 0)
    Me.Caption = "Equality Restrictions - [" & GetFileName(Project.Item(ProjectIndex).sFileName) & "]"
    NETO = Project.Item(ProjectIndex).colPanel.Count
    
    LoadTitles
    
    ReDim Vct0(1 To 9)
    ReDim Vct1(1 To NETO)
    mnuEQ.Visible = False
    Me.Height = 4965
    EQList.Top = 4560
    EQList.Height = 1815
    
    ShowList = False
    ExpandList = False
    ListView = False
    cmdListView.Caption = "Numeric View"
    Vectors
    PopulateIndList
    PopulateDepList
    IndList.ListIndex = 0
    TitlesState
    
    intNEGALT = Project.Item(ProjectIndex).cHeader.colEqualityRestrictions.Count
    If intNEGALT > 0 Then
        ReDim StrEQConstr(1 To intNEGALT)
        For i = 1 To intNEGALT
            str1 = Project.Item(ProjectIndex).cHeader.colEqualityRestrictions.Item(i).DependingDesignVariable
            str2 = Project.Item(ProjectIndex).cHeader.colEqualityRestrictions.Item(i).DependingPanel
            str3 = Project.Item(ProjectIndex).cHeader.colEqualityRestrictions.Item(i).LeadingDesignVariable
            str4 = Project.Item(ProjectIndex).cHeader.colEqualityRestrictions.Item(i).LeadingPanel
            str5 = Project.Item(ProjectIndex).cHeader.colEqualityRestrictions.Item(i).Ratio
            StrEQConstr(i) = str1 & "   " & str2 & "   " & str3 & "   " & str4 & "   " & str5
        Next i
        For i = 1 To intNEGALT
            Strings i, str1, str2, str3, str4, str5
            ReDim Preserve vctRestr(1 To intNEGALT)
            vctRestr(i) = Array(Val_(str1), Val_(str2), Val_(str3), Val_(str4), Val_(str5))
        Next i
        PopulateEQList
    Else
        CountNegalt.Caption = "0 restrictions"
    End If
    
    ' EQList
'    intNEGALT = header.NEGALT
'    If intNEGALT > 0 Then
'        ReDim StrEQConstr(1 To intNEGALT)
'        For i = 1 To intNEGALT
'            StrEQConstr(i) = header.EQConstr(i)
'        Next i
'        For i = 1 To intNEGALT
'            Strings i, str1, str2, str3, str4, str5
'            ReDim Preserve vctRestr(1 To intNEGALT)
'            vctRestr(i) = Array(Val_(str1), Val_(str2), Val_(str3), Val_(str4), Val_(str5))
'        Next i
'        PopulateEQList
'    Else
'        CountNegalt.Caption = "0 restrictions"
'    End If
End Sub

Private Sub IndList_Click()
    Dim i As Integer
    For i = 1 To 9
        Title(i).Enabled = False
        Title(i).FontUnderline = False
    Next i
    IndIndex = IndList.ListIndex + 1
    TitlesState
    'DepList.ListIndex = 0
    'PopulateDepList
End Sub

Private Sub IndList_GotFocus()
IndList_Click
End Sub

Private Sub mnuEQRemove_Click()
    Dim i As Integer
    Dim IndexDelete As Integer
    If EQList.ListIndex <> -1 Then
    IndexDelete = EQList.ListIndex
    EQAltList.ListIndex = EQList.ListIndex
        Select Case IndexDelete
            Case IndexDelete
                EQList.RemoveItem IndexDelete
                EQAltList.RemoveItem IndexDelete
                intNEGALT = intNEGALT - 1
            Case Else
        End Select
    End If
    If EQList.ListCount > 0 Then
        ReDim StrEQConstr(1 To intNEGALT)
        For i = 1 To EQAltList.ListCount
            StrEQConstr(i) = EQAltList.List(i - 1)
        Next i
        For i = 1 To intNEGALT
            Strings i, str1, str2, str3, str4, str5
            ReDim Preserve vctRestr(1 To intNEGALT)
            vctRestr(i) = Array(Val_(str1), Val_(str2), Val_(str3), Val_(str4), Val_(str5))
        Next i
    End If
    PopulateEQList
    EQList.ListIndex = -1
    EQAltList.ListIndex = -1

End Sub

Private Sub Title_MouseDown(index As Integer, Button As Integer, Shift As Integer, x As Single, y As Single)
    Dim i As Integer
    If Button = 1 Then
        If Title(index).FontUnderline = True Then
            Title(index).FontUnderline = False
        Else
            Title(index).FontUnderline = True
        End If
    End If
End Sub

Private Sub TitlesState()
    Dim i As Integer: Dim j As Integer: Dim k As Integer: Dim index As Integer
    index = 0
    For i = 1 To NETO
        If DepList.Selected(i - 1) = True Then
            index = index + 1
            ReDim Preserve vctIndex(1 To index)
            vctIndex(index) = i
        End If
    Next i
    ReDim Preserve vctIndex(1 To index + 1)
    vctIndex(UBound(vctIndex)) = IndIndex
    For i = 1 To 9
        Title(i).Enabled = True
    Next i
    ' dupa IndList
    For i = 1 To 9
        If Vct1(IndIndex)(i) = i Then
            Title(i).Enabled = True
        Else
            Title(i).Enabled = False
        End If
    Next i
    ' dupa DepList + IndList
        For i = 1 To UBound(vctIndex)
            For j = i To UBound(vctIndex)
                For k = 1 To 9
                    If Vct1(vctIndex(i))(k) = Vct1(vctIndex(j))(k) Then
                        'Title(k).Enabled = True
                    Else
                        Title(k).Enabled = False
                        Title(k).FontUnderline = False
                    End If
                Next k
            Next j
        Next i
    ' daca panou ind si panou dep selectate
    For i = 1 To NETO
        If DepList.Selected(IndList.ListIndex) = True Then
            For j = 1 To 9
                Title(j).Enabled = False
                Title(j).FontUnderline = False
            Next j
        End If
    Next i
End Sub

Private Sub Vectors()
    Dim i As Integer
    Dim j As Integer
    Dim k As Integer
    For i = 1 To 9
        Vct0(i) = Empty
    Next i
    For i = 1 To NETO
        Vct1(i) = Vct0
    Next i
    For i = 1 To NETO
        For j = 1 To 9
            For k = 1 To Project.Item(ProjectIndex).colPanel.Item(i).colDesignVariables.Count
                If Project.Item(ProjectIndex).colPanel.Item(i).colDesignVariables.Item(k).VariableName = j Then
                    Vct1(i)(j) = j
                End If
            Next k
        Next j
    Next i
'    For i = 1 To NETO
'        For j = 1 To 9
'            For k = 1 To Panel(i).NrNXI
'                If Panel(i).NXI(k) = j Then
'                    Vct1(i)(j) = j
'                End If
'            Next k
'        Next j
'    Next i
End Sub

Private Sub Strings(index As Integer, str1 As String, str2 As String, str3 As String, str4 As String, str5 As String)
    Dim CutString As String
    Dim FirstOccurence As Integer
    Dim StringLenght As Integer
    ' 1
    FirstOccurence = InStr(StrEQConstr(index), "   ")
    str1 = Left(StrEQConstr(index), FirstOccurence - 1)
    StringLenght = Len(StrEQConstr(index))
    CutString = right(StrEQConstr(index), StringLenght - FirstOccurence - 2)
    ' 2
    FirstOccurence = InStr(CutString, "   ")
    str2 = Left(CutString, FirstOccurence - 1)
    StringLenght = Len(CutString)
    CutString = right(CutString, StringLenght - FirstOccurence - 2)
    ' 3
    FirstOccurence = InStr(CutString, "   ")
    str3 = Left(CutString, FirstOccurence - 1)
    StringLenght = Len(CutString)
    CutString = right(CutString, StringLenght - FirstOccurence - 2)
    ' 4
    FirstOccurence = InStr(CutString, "   ")
    str4 = Left(CutString, FirstOccurence - 1)
    StringLenght = Len(CutString)
    CutString = right(CutString, StringLenght - FirstOccurence - 2)
    ' 5
    str5 = CutString
End Sub

Private Sub PopulateIndList()
    Dim i As Integer
    For i = 1 To NETO
        IndList.AddItem "Panel " & i
    Next i
End Sub
Private Sub PopulateDepList()
    Dim i As Integer
    DepList.Clear
    For i = 1 To NETO
        DepList.AddItem "Panel " & i
    Next i
End Sub

Private Sub PopulateEQList()
    Dim i As Integer
    EQList.Clear
    EQAltList.Clear
    CommentsLib
    For i = 1 To intNEGALT
        EQAltList.AddItem vctRestr(i)(1) & "   " & vctRestr(i)(2) & "   " & vctRestr(i)(3) & _
        "   " & vctRestr(i)(4) & "   " & vctRestr(i)(5)
        EQList.AddItem i & ". " & Comments(vctRestr(i)(1)) & " on Panel " & vctRestr(i)(2) & _
        " = " & vctRestr(i)(5) & " x " & Comments(vctRestr(i)(3)) & _
        " on Panel " & vctRestr(i)(4)
    Next i
    Select Case intNEGALT
        Case 1
        CountNegalt.Caption = intNEGALT & " restriction"
        Case Else
            CountNegalt.Caption = intNEGALT & " restrictions"
    End Select
End Sub

Private Sub CommentsLib()
    ReDim Comments(1 To 9)
    Comments(1) = "Plate Thickness"
    Comments(2) = "Frames Web Height"
    Comments(3) = "Frames Web Thickness"
    Comments(4) = "Frames Flange Width"
    If Project.Item(ProjectIndex).cHeader.IANA = 1 Then
        Comments(5) = "Frames Spacing"
    Else
        Comments(5) = "Stiffeners Span"
    End If
    Comments(6) = "Stiffeners Web Height"
    Comments(7) = "Stiffeners Web Thickness"
    Comments(8) = "Stiffeners Flange Width"
    Comments(9) = "Stiffeners Spacing"
End Sub

Private Sub txtRatio_Change()
    If Not IsNumeric(txtRatio.Text) Then
       SendKeys "{BackSpace}"
    End If
End Sub

Private Sub SortList(vctRestr)
    Dim i As Integer
    Dim j As Integer
    Dim Buffer() As Variant
    For i = 1 To intNEGALT
        For j = i To intNEGALT - 1
            ' sortare dupa termenul 2
            If Val_(vctRestr(i)(2)) > Val_(vctRestr(j + 1)(2)) Then
                Buffer = vctRestr(j + 1)
                vctRestr(j + 1) = vctRestr(i)
                vctRestr(i) = Buffer
            End If
            ' sortare dupa termenul 1, pentru termeni 2 egali
            If Val_(vctRestr(i)(2)) = Val_(vctRestr(j + 1)(2)) Then
                If vctRestr(i)(1) > vctRestr(j + 1)(1) Then
                    Buffer = vctRestr(j + 1)
                    vctRestr(j + 1) = vctRestr(i)
                    vctRestr(i) = Buffer
                End If
            End If
        Next j
    Next i
End Sub
