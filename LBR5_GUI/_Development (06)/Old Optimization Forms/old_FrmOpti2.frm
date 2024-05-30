VERSION 5.00
Object = "{831FDD16-0C5C-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCTL.OCX"
Object = "{5E9E78A0-531B-11CF-91F6-C2863C385E30}#1.0#0"; "MSFLXGRD.OCX"
Begin VB.Form old_frmOpti2 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Structural Constraints"
   ClientHeight    =   7875
   ClientLeft      =   4935
   ClientTop       =   2250
   ClientWidth     =   8550
   Icon            =   "old_FrmOpti2.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   7875
   ScaleWidth      =   8550
   ShowInTaskbar   =   0   'False
   Begin VB.TextBox txtEdit 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   195
      Left            =   4080
      TabIndex        =   4
      Top             =   2160
      Visible         =   0   'False
      Width           =   975
   End
   Begin VB.ListBox PasteList 
      Appearance      =   0  'Flat
      ForeColor       =   &H00C00000&
      Height          =   4125
      Left            =   120
      MultiSelect     =   2  'Extended
      TabIndex        =   16
      Top             =   360
      Width           =   1455
   End
   Begin VB.CommandButton cmdExpand 
      Caption         =   "Expand List >>"
      Height          =   345
      Left            =   120
      TabIndex        =   15
      Top             =   6960
      Width           =   1455
   End
   Begin VB.ListBox List3 
      Appearance      =   0  'Flat
      Height          =   1395
      Left            =   120
      TabIndex        =   14
      Top             =   5400
      Width           =   8295
   End
   Begin VB.CommandButton CmdReferenceList 
      Caption         =   "References >>"
      Height          =   345
      Left            =   120
      TabIndex        =   13
      Top             =   4680
      Width           =   1455
   End
   Begin VB.ListBox List1 
      Height          =   4155
      Left            =   120
      TabIndex        =   8
      Top             =   360
      Width           =   1455
   End
   Begin VB.ListBox List2 
      Enabled         =   0   'False
      Height          =   840
      Left            =   1800
      TabIndex        =   7
      Top             =   1440
      Width           =   1095
   End
   Begin VB.ListBox List4 
      Appearance      =   0  'Flat
      Height          =   810
      Left            =   3120
      TabIndex        =   6
      Top             =   360
      Width           =   5295
   End
   Begin VB.CommandButton cmdApply 
      Caption         =   "&Apply"
      Default         =   -1  'True
      Height          =   345
      Left            =   6000
      TabIndex        =   3
      Top             =   4680
      Width           =   1125
   End
   Begin VB.CommandButton cmdClose 
      Caption         =   "&Close"
      Height          =   345
      Left            =   7200
      TabIndex        =   2
      Top             =   4680
      Width           =   1125
   End
   Begin VB.CommandButton cmdAdd 
      Caption         =   "Add Restriction"
      Height          =   615
      Left            =   1800
      TabIndex        =   1
      Top             =   3120
      Width           =   1095
   End
   Begin VB.CommandButton cmdRemove 
      Caption         =   "Remove Restriction"
      Height          =   615
      Left            =   1800
      TabIndex        =   0
      Top             =   3840
      Width           =   1095
   End
   Begin MSComctlLib.Slider Slider1 
      Height          =   615
      Left            =   1680
      TabIndex        =   5
      Top             =   2400
      Width           =   1335
      _ExtentX        =   2355
      _ExtentY        =   1085
      _Version        =   393216
      TickStyle       =   1
      TextPosition    =   1
   End
   Begin MSFlexGridLib.MSFlexGrid MSH1 
      Height          =   3135
      Left            =   3120
      TabIndex        =   17
      Top             =   1320
      Width           =   5295
      _ExtentX        =   9340
      _ExtentY        =   5530
      _Version        =   393216
      Cols            =   9
      ScrollTrack     =   -1  'True
      TextStyleFixed  =   3
      Appearance      =   0
   End
   Begin VB.PictureBox Picture1 
      Height          =   3135
      Left            =   3120
      ScaleHeight     =   3075
      ScaleWidth      =   5115
      TabIndex        =   9
      Top             =   1320
      Width           =   5175
   End
   Begin VB.Label Label2 
      Alignment       =   2  'Center
      Caption         =   "Points for constraint assessment:"
      Height          =   615
      Left            =   1680
      TabIndex        =   12
      Top             =   720
      Width           =   1215
   End
   Begin VB.Label Label1 
      AutoSize        =   -1  'True
      Caption         =   "Panels: "
      Height          =   195
      Left            =   120
      TabIndex        =   11
      Top             =   120
      Width           =   570
   End
   Begin VB.Label Label4 
      Caption         =   "Load Cases: "
      Height          =   255
      Left            =   3120
      TabIndex        =   10
      Top             =   120
      Width           =   1695
   End
   Begin VB.Menu mnuCopy 
      Caption         =   "Copy"
      Begin VB.Menu mnuCopy1 
         Caption         =   "Copy"
      End
      Begin VB.Menu mnuPaste1 
         Caption         =   "Paste"
      End
      Begin VB.Menu mnuLine 
         Caption         =   "-"
      End
      Begin VB.Menu mnuCopyAll 
         Caption         =   "Copy All Restrictions To Other Panels"
      End
   End
   Begin VB.Menu mnuPaste 
      Caption         =   "Paste"
      Begin VB.Menu mnuPastePaste 
         Caption         =   "Paste"
      End
   End
   Begin VB.Menu mnuCopyOnCase 
      Caption         =   "Copy Paste"
      Begin VB.Menu mnuCopyOnLoadCases 
         Caption         =   "Copy Paste to all load cases"
      End
   End
End
Attribute VB_Name = "old_frmOpti2"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Base 1
Dim PanelIndex As Integer
Dim LoadIndex As Integer
Dim PointIndex As Integer
Dim vctIM10() As Variant
Dim vctM10() As Variant
Dim vctJ0() As Variant
Dim vctICT0() As Variant
Dim vctCJMAX0() As Variant
Dim vctINV0() As Variant
Dim vctIY0() As Variant
Dim vctJ1() As Variant
Dim vctICT1() As Variant
Dim vctCJMAX1() As Variant
Dim vctINV1() As Variant
Dim vctIY1() As Variant
Dim vctJ2() As Variant
Dim vctICT2() As Variant
Dim vctCJMAX2() As Variant
Dim vctINV2() As Variant
Dim vctIY2() As Variant
Dim vctJ3() As Variant
Dim vctICT3() As Variant
Dim vctCJMAX3() As Variant
Dim vctINV3() As Variant
Dim vctIY3() As Variant

Dim vctIPT() As Variant
Dim vctYPT() As Variant

Dim REFstate As Boolean
Dim ExpandState As Boolean
Dim state As Boolean
Dim Row1 As Integer, Row2 As Integer, Col1 As Integer, Col2 As Integer
Dim NETO As Integer
Dim ProjectIndex As Integer

Dim buff As String

Private Sub cmdAdd_Click()
Dim i As Integer
' ===================================
' Daca initial am restrictii pe panou
' ===================================
If vctIM10(PanelIndex) > 0 Then ' daca initial am restrictii pe panou
    If vctM10(PanelIndex)(LoadIndex) > 0 Then ' daca initial am restrictii pe caz de incarcare
        vctM10(PanelIndex)(LoadIndex) = vctM10(PanelIndex)(LoadIndex) + 1
        ReDim Preserve vctJ1(1 To vctM10(PanelIndex)(LoadIndex))
        ReDim Preserve vctICT1(1 To vctM10(PanelIndex)(LoadIndex))
        ReDim Preserve vctCJMAX1(1 To vctM10(PanelIndex)(LoadIndex))
        ReDim Preserve vctINV1(1 To vctM10(PanelIndex)(LoadIndex))
        ReDim Preserve vctIY1(1 To vctM10(PanelIndex)(LoadIndex))
        For i = 1 To vctM10(PanelIndex)(LoadIndex)
            If IsEmpty(vctJ1(i)) Then
                vctJ1(i) = 0
            End If
            If IsEmpty(vctICT1(i)) Then
                vctICT1(i) = 0
            End If
            If IsEmpty(vctCJMAX1(i)) Then
                vctCJMAX1(i) = 0
            End If
            If IsEmpty(vctINV1(i)) Then
                vctINV1(i) = 0
            End If
            If IsEmpty(vctIY1(i)) Then
                vctIY1(i) = 0
            End If
        Next i
            vctJ0(PanelIndex)(LoadIndex) = vctJ1
            ' Iau randurile vechi de valori din flex
            For i = 1 To vctM10(PanelIndex)(LoadIndex) - 1
                vctICT1(i) = Val_(MSH1.TextMatrix(i, 1))
                vctCJMAX1(i) = Val_(MSH1.TextMatrix(i, 2))
                vctINV1(i) = Val_(MSH1.TextMatrix(i, 3))
                vctIY1(i) = Val_(MSH1.TextMatrix(i, 4))
            Next i
            ' introduc randul nou de valori
            vctICT1(vctM10(PanelIndex)(LoadIndex)) = 0
            vctCJMAX1(vctM10(PanelIndex)(LoadIndex)) = 0
            vctINV1(vctM10(PanelIndex)(LoadIndex)) = 1
            vctIY1(vctM10(PanelIndex)(LoadIndex)) = 2
            ' Umplu vectorii mari
            vctICT0(PanelIndex)(LoadIndex) = vctICT1
            vctCJMAX0(PanelIndex)(LoadIndex) = vctCJMAX1
            vctINV0(PanelIndex)(LoadIndex) = vctINV1
            vctIY0(PanelIndex)(LoadIndex) = vctIY1
    End If ' daca initial am restrictii pe caz de incarcare
End If ' daca initial am restrictii pe panou
' ======================================
' Daca initial nu am restrictii pe panou
' ======================================
If vctIM10(PanelIndex) = 0 Then
' +++++++++++++++++++ Initiez Puncte de calcul
List2.AddItem 0
List2.AddItem 0.5
List2.AddItem 1
vctIPT(PanelIndex) = 3     ' Temporar (pentru moment nu pot schimba nr de puncte)
'FIXIT: Declare 'vctYPT1' with an early-bound data type                                    FixIT90210ae-R1672-R1B8ZE
Dim vctYPT1() As Variant
ReDim vctYPT1(vctIPT(PanelIndex))
For i = 1 To vctIPT(PanelIndex)
    vctYPT1(i) = Val_(List2.List(i - 1))
Next i
vctYPT(PanelIndex) = vctYPT1
' ------------------- Initiez Puncte de calcul
    vctIM10(PanelIndex) = 1
'FIXIT: Non Zero lowerbound arrays are not supported in Visual Basic .NET                  FixIT90210ae-R9815-H1984
 ReDim vctM11(1 To Project.Item(ProjectIndex).cHeader.colLoadCase.Count)
 For i = 1 To Project.Item(ProjectIndex).cHeader.colLoadCase.Count
     vctM11(i) = 0
 Next i
 vctM11(LoadIndex) = 1
 vctM10(PanelIndex) = vctM11
 ReDim Preserve vctJ2(Project.Item(ProjectIndex).cHeader.colLoadCase.Count)
 ReDim Preserve vctICT2(Project.Item(ProjectIndex).cHeader.colLoadCase.Count)
 ReDim Preserve vctCJMAX2(Project.Item(ProjectIndex).cHeader.colLoadCase.Count)
 ReDim Preserve vctINV2(Project.Item(ProjectIndex).cHeader.colLoadCase.Count)
 ReDim Preserve vctIY2(Project.Item(ProjectIndex).cHeader.colLoadCase.Count)
 ' dau valori vectorului mic
 ReDim Preserve vctJ1(1 To vctM10(PanelIndex)(LoadIndex))
 ReDim Preserve vctICT1(1 To vctM10(PanelIndex)(LoadIndex))
 ReDim Preserve vctCJMAX1(1 To vctM10(PanelIndex)(LoadIndex))
 ReDim Preserve vctINV1(1 To vctM10(PanelIndex)(LoadIndex))
 ReDim Preserve vctIY1(1 To vctM10(PanelIndex)(LoadIndex))
 For i = 1 To vctM10(PanelIndex)(LoadIndex)
 If IsEmpty(vctICT1(i)) Then
     vctJ1(i) = i
     vctICT1(i) = 0
     vctCJMAX1(i) = 0
     vctINV1(i) = 1
     vctIY1(i) = 2
End If
     vctJ1(i) = i
     vctICT1(i) = 0
     vctCJMAX1(i) = 0
     vctINV1(i) = 1
     vctIY1(i) = 2
Next i
' fac vectori de 1 ramura plini cu zerouri
ReDim vctJ3(1)
ReDim vctICT3(1)
ReDim vctCJMAX3(1)
ReDim vctINV3(1)
ReDim vctIY3(1)
vctJ3(1) = 0
vctICT3(1) = 0
vctCJMAX3(1) = 0
vctINV3(1) = 0
vctIY3(1) = 0
' umplu vectorii goi de 2 ramuri
For i = 1 To Project.Item(ProjectIndex).cHeader.colLoadCase.Count
    If IsEmpty(vctJ2(i)) Then
        vctJ2(i) = vctJ3
    End If
    If IsEmpty(vctICT2(i)) Then
        vctICT2(i) = vctICT3
    End If
    If IsEmpty(vctCJMAX2(i)) Then
        vctCJMAX2(i) = vctCJMAX3
    End If
    If IsEmpty(vctINV2(i)) Then
        vctINV2(i) = vctINV3
    End If
    If IsEmpty(vctIY2(i)) Then
        vctIY2(i) = vctIY3
    End If
Next i
    vctJ2(LoadIndex) = vctJ1
    vctICT2(LoadIndex) = vctICT1
    vctCJMAX2(LoadIndex) = vctCJMAX1
    vctINV2(LoadIndex) = vctINV1
    vctIY2(LoadIndex) = vctIY1
    ' bag in vectorul mare
    vctJ0(PanelIndex) = vctJ2
    vctICT0(PanelIndex) = vctICT2
    vctCJMAX0(PanelIndex) = vctCJMAX2
    vctINV0(PanelIndex) = vctINV2
    vctIY0(PanelIndex) = vctIY2
Else
End If
' =================================================================
' Daca initial am restrictii pe panou dar nu am pe caz de incarcare
' =================================================================
If vctIM10(PanelIndex) > 0 Then ' daca initial am restrictii pe panou
    If vctM10(PanelIndex)(LoadIndex) = 0 Then ' daca initial nu am restrictii pe caz de incarcare
        vctM10(PanelIndex)(LoadIndex) = 1
        ReDim vctM11(1 To Project.Item(ProjectIndex).cHeader.colLoadCase.Count)
        vctM11 = vctM10(PanelIndex)
        vctM10(PanelIndex) = vctM11
        ReDim Preserve vctJ2(Project.Item(ProjectIndex).cHeader.colLoadCase.Count)
        ReDim Preserve vctICT2(Project.Item(ProjectIndex).cHeader.colLoadCase.Count)
        ReDim Preserve vctCJMAX2(Project.Item(ProjectIndex).cHeader.colLoadCase.Count)
        ReDim Preserve vctINV2(Project.Item(ProjectIndex).cHeader.colLoadCase.Count)
        ReDim Preserve vctIY2(Project.Item(ProjectIndex).cHeader.colLoadCase.Count)
        ReDim vctJ1(1 To vctM10(PanelIndex)(LoadIndex))
        ReDim vctICT1(1 To vctM10(PanelIndex)(LoadIndex))
        ReDim vctCJMAX1(1 To vctM10(PanelIndex)(LoadIndex))
        ReDim vctINV1(1 To vctM10(PanelIndex)(LoadIndex))
        ReDim vctIY1(1 To vctM10(PanelIndex)(LoadIndex))
        For i = 1 To vctM10(PanelIndex)(LoadIndex)
            vctJ1(i) = 1
            vctICT1(i) = 0
            vctCJMAX1(i) = 0
            vctINV1(i) = 1
            vctIY1(i) = 2
        Next i
        vctJ2(LoadIndex) = vctJ1
        vctICT2(LoadIndex) = vctICT1
        vctCJMAX2(LoadIndex) = vctCJMAX1
        vctINV2(LoadIndex) = vctINV1
        vctIY2(LoadIndex) = vctIY1
        ReDim Preserve vctJ3(1 To Project.Item(ProjectIndex).cHeader.colLoadCase.Count)
        ReDim Preserve vctICT3(1 To Project.Item(ProjectIndex).cHeader.colLoadCase.Count)
        ReDim Preserve vctCJMAX3(1 To Project.Item(ProjectIndex).cHeader.colLoadCase.Count)
        ReDim Preserve vctINV3(1 To Project.Item(ProjectIndex).cHeader.colLoadCase.Count)
        ReDim Preserve vctIY3(1 To Project.Item(ProjectIndex).cHeader.colLoadCase.Count)
        vctJ3 = vctJ2(LoadIndex)
        vctICT3 = vctICT2(LoadIndex)
        vctCJMAX3 = vctCJMAX2(LoadIndex)
        vctINV3 = vctINV2(LoadIndex)
        vctIY3 = vctIY2(LoadIndex)
        vctJ0(PanelIndex)(LoadIndex) = vctJ2(LoadIndex)
        vctICT0(PanelIndex)(LoadIndex) = vctICT2(LoadIndex)
        vctCJMAX0(PanelIndex)(LoadIndex) = vctCJMAX2(LoadIndex)
        vctINV0(PanelIndex)(LoadIndex) = vctINV2(LoadIndex)
        vctIY0(PanelIndex)(LoadIndex) = vctIY2(LoadIndex)
    End If
End If
Ierarhic1 PanelIndex, LoadIndex
End Sub

Private Sub cmdClose_Click()
Unload Me
End Sub

Private Sub cmdExpand_Click()
If ExpandState = True Then
    cmdExpand.Caption = "Expand List >>"
    List3.Top = 5400
    List3.Height = 1425
    ExpandState = False
Else
    cmdExpand.Caption = "<< Restore List"
    List3.Top = Label1.Top
    List3.Height = 6825
    ExpandState = True
End If
End Sub

Private Sub CmdReferenceList_Click()
If REFstate = True Then
    CmdReferenceList.Caption = "References >>"
    Me.Height = 5700
    REFstate = False
Else
    CmdReferenceList.Caption = "<< References"
    Me.Height = 7995
    REFstate = True
End If
End Sub

Private Sub cmdRemove_Click()
Dim i As Integer
Dim SelectedRow As Integer
SelectedRow = MSH1.Row
If vctIM10(PanelIndex) > 0 Then
    If vctM10(PanelIndex)(LoadIndex) > 1 Then
        Select Case SelectedRow
            Case 1
                ReDim vctJ1(1 To vctM10(PanelIndex)(LoadIndex) - 1)
                ReDim vctICT1(1 To vctM10(PanelIndex)(LoadIndex) - 1)
                ReDim vctCJMAX1(1 To vctM10(PanelIndex)(LoadIndex) - 1)
                ReDim vctINV1(1 To vctM10(PanelIndex)(LoadIndex) - 1)
                ReDim vctIY1(1 To vctM10(PanelIndex)(LoadIndex) - 1)
                vctM10(PanelIndex)(LoadIndex) = vctM10(PanelIndex)(LoadIndex) - 1
                For i = 1 To vctM10(PanelIndex)(LoadIndex)
                    vctJ1(i) = vctJ0(PanelIndex)(LoadIndex)(i + 1)
                    vctICT1(i) = vctICT0(PanelIndex)(LoadIndex)(i + 1)
                    vctCJMAX1(i) = vctCJMAX0(PanelIndex)(LoadIndex)(i + 1)
                    vctINV1(i) = vctINV0(PanelIndex)(LoadIndex)(i + 1)
                    vctIY1(i) = vctIY0(PanelIndex)(LoadIndex)(i + 1)
                Next i
                vctJ0(PanelIndex)(LoadIndex) = vctJ1
                vctICT0(PanelIndex)(LoadIndex) = vctICT1
                vctCJMAX0(PanelIndex)(LoadIndex) = vctCJMAX1
                vctINV0(PanelIndex)(LoadIndex) = vctINV1
                vctIY0(PanelIndex)(LoadIndex) = vctIY1
            Case vctM10(PanelIndex)(LoadIndex)
                ReDim vctJ1(1 To vctM10(PanelIndex)(LoadIndex) - 1)
                ReDim vctICT1(1 To vctM10(PanelIndex)(LoadIndex) - 1)
                ReDim vctCJMAX1(1 To vctM10(PanelIndex)(LoadIndex) - 1)
                ReDim vctINV1(1 To vctM10(PanelIndex)(LoadIndex) - 1)
                ReDim vctIY1(1 To vctM10(PanelIndex)(LoadIndex) - 1)
                vctM10(PanelIndex)(LoadIndex) = vctM10(PanelIndex)(LoadIndex) - 1
                For i = 1 To vctM10(PanelIndex)(LoadIndex)
                    vctJ1(i) = vctJ0(PanelIndex)(LoadIndex)(i)
                    vctICT1(i) = vctICT0(PanelIndex)(LoadIndex)(i)
                    vctCJMAX1(i) = vctCJMAX0(PanelIndex)(LoadIndex)(i)
                    vctINV1(i) = vctINV0(PanelIndex)(LoadIndex)(i)
                    vctIY1(i) = vctIY0(PanelIndex)(LoadIndex)(i)
                Next i
                vctJ0(PanelIndex)(LoadIndex) = vctJ1
                vctICT0(PanelIndex)(LoadIndex) = vctICT1
                vctCJMAX0(PanelIndex)(LoadIndex) = vctCJMAX1
                vctINV0(PanelIndex)(LoadIndex) = vctINV1
                vctIY0(PanelIndex)(LoadIndex) = vctIY1
            Case Else
                ' Redimensioneaza pana la restrictia selectata exclusiv
                ReDim vctJ1(1 To SelectedRow - 1)
                ReDim vctICT1(1 To SelectedRow - 1)
                ReDim vctCJMAX1(1 To SelectedRow - 1)
                ReDim vctINV1(1 To SelectedRow - 1)
                ReDim vctIY1(1 To SelectedRow - 1)
                For i = 1 To SelectedRow - 1
                    vctJ1(i) = vctJ0(PanelIndex)(LoadIndex)(i)
                    vctICT1(i) = vctICT0(PanelIndex)(LoadIndex)(i)
                    vctCJMAX1(i) = vctCJMAX0(PanelIndex)(LoadIndex)(i)
                    vctINV1(i) = vctINV0(PanelIndex)(LoadIndex)(i)
                    vctIY1(i) = vctIY0(PanelIndex)(LoadIndex)(i)
                Next i
                ' redimensioneaza dupa restrictia selectata exclusiv
                ReDim vctJ21(SelectedRow To vctM10(PanelIndex)(LoadIndex))
                ReDim vctICT21(SelectedRow To vctM10(PanelIndex)(LoadIndex))
                ReDim vctCJMAX21(SelectedRow To vctM10(PanelIndex)(LoadIndex))
                ReDim vctINV21(SelectedRow To vctM10(PanelIndex)(LoadIndex))
                ReDim vctIY21(SelectedRow To vctM10(PanelIndex)(LoadIndex))
                For i = SelectedRow To vctM10(PanelIndex)(LoadIndex)
                    vctJ21(i) = vctJ0(PanelIndex)(LoadIndex)(i)
                    vctICT21(i) = vctICT0(PanelIndex)(LoadIndex)(i)
                    vctCJMAX21(i) = vctCJMAX0(PanelIndex)(LoadIndex)(i)
                    vctINV21(i) = vctINV0(PanelIndex)(LoadIndex)(i)
                    vctIY21(i) = vctIY0(PanelIndex)(LoadIndex)(i)
                Next i
                ' formeaza vectorul mare din cei 2 vectori mici
                ' (introduc un nou vector compus din vectorii mici - nu pot introduce cei doi
                ' vectori direct in vectorul principal deoarece daca folosesc bucle nu se va
                ' redimensiona
                Dim vctJ3() As Variant
                Dim vctICT3() As Variant
                Dim vctCJMAX3() As Variant
                Dim vctINV3() As Variant
                Dim vctIY3() As Variant
                vctM10(PanelIndex)(LoadIndex) = vctM10(PanelIndex)(LoadIndex) - 1
                ReDim vctJ3(1 To vctM10(PanelIndex)(LoadIndex))
                ReDim vctICT3(1 To vctM10(PanelIndex)(LoadIndex))
                ReDim vctCJMAX3(1 To vctM10(PanelIndex)(LoadIndex))
                ReDim vctINV3(1 To vctM10(PanelIndex)(LoadIndex))
                ReDim vctIY3(1 To vctM10(PanelIndex)(LoadIndex))
                For i = 1 To SelectedRow - 1
                    vctJ3(i) = vctJ1(i)
                    vctICT3(i) = vctICT1(i)
                    vctCJMAX3(i) = vctCJMAX1(i)
                    vctINV3(i) = vctINV1(i)
                    vctIY3(i) = vctIY1(i)
                Next i
                For i = SelectedRow To vctM10(PanelIndex)(LoadIndex)
                    vctJ3(i) = vctJ21(i + 1)
                    vctICT3(i) = vctICT21(i + 1)
                    vctCJMAX3(i) = vctCJMAX21(i + 1)
                    vctINV3(i) = vctINV21(i + 1)
                    vctIY3(i) = vctIY21(i + 1)
                Next i
                ' introduc vectorul 3 in vectorul principal
                vctJ0(PanelIndex)(LoadIndex) = vctJ3
                vctICT0(PanelIndex)(LoadIndex) = vctICT3
                vctCJMAX0(PanelIndex)(LoadIndex) = vctCJMAX3
                vctINV0(PanelIndex)(LoadIndex) = vctINV3
                vctIY0(PanelIndex)(LoadIndex) = vctIY3
        End Select
    ElseIf vctM10(PanelIndex)(LoadIndex) = 1 Then
    vctM10(PanelIndex)(LoadIndex) = 0     ' - DUBIOS!!!
    cmdRemove.Enabled = False
    End If
End If
' daca nu mai am nici o restrictie pe nici un caz
Dim index As Integer
index = 0
For i = 1 To Project.Item(ProjectIndex).cHeader.colLoadCase.Count
    If vctM10(PanelIndex)(i) = 0 Then
        index = index + 1
    End If
Next i
If index = Project.Item(ProjectIndex).cHeader.colLoadCase.Count Then
    vctIM10(PanelIndex) = 0
    List2.Clear
End If
Ierarhic1 PanelIndex, LoadIndex
End Sub

Private Sub Form_Load()

If Licensing.IS_MODIFY_STRUCTURAL_CONSTRAINTS = False Then
    cmdApply.Enabled = False
End If
mnuPaste1.Enabled = False
REFstate = False
ExpandState = False
Me.Height = 5700
List3.Top = 5400
List3.Height = 1425
ProjectIndex = ActiveProject
RefList

Me.Move (Screen.Width - Me.Width) / 2, (Screen.Height - Me.Height) / 2
'    cmdOK.Picture = LoadResPicture("ID_BITMAP_OK", 0)
'    cmdCancel.Picture = LoadResPicture("ID_BITMAP_CANCEL", 0)
Me.Caption = "Structural Constraints - [" & GetFileName(Project.Item(ProjectIndex).sFileName) & "]"
NETO = Project.Item(ProjectIndex).colPanel.Count

' Golesc toti vectorii (se pare ca raman valori dupa ce inchid formul
' si daca am facut schimbari nu se mai pupa cu membrii din clasa
ReDim vctIM10(1) As Variant
ReDim vctM10(1) As Variant
ReDim vctJ0(1) As Variant
ReDim vctICT0(1) As Variant
ReDim vctCJMAX0(1) As Variant
ReDim vctINV0(1) As Variant
ReDim vctIY0(1) As Variant
ReDim vctJ1(1) As Variant
ReDim vctICT1(1) As Variant
ReDim vctCJMAX1(1) As Variant
ReDim vctINV1(1) As Variant
ReDim vctIY1(1) As Variant
ReDim vctJ2(1) As Variant
ReDim vctICT2(1) As Variant
ReDim vctCJMAX2(1) As Variant
ReDim vctINV2(1) As Variant
ReDim vctIY2(1) As Variant
ReDim vctJ3(1) As Variant
ReDim vctICT3(1) As Variant
ReDim vctCJMAX3(1) As Variant
ReDim vctINV3(1) As Variant
ReDim vctIY3(1) As Variant
ReDim vctIPT(1) As Variant
ReDim vctYPT(1) As Variant
Dim i As Integer
TakeOverVariables
For i = 1 To NETO
    List1.AddItem "Panel " & i
Next i
For i = 1 To NETO
    PasteList.AddItem "Panel " & i
Next i

For i = 1 To Project.Item(ProjectIndex).cHeader.colLoadCase.Count
    List4.AddItem Project.Item(ProjectIndex).cHeader.colLoadCase.Item(i).Title
Next i
'For i = 1 To Project.Item(ProjectIndex).cHeader.colLoadCase.Count
'    List4.AddItem header.colLoadCases(i).LoadCaseName
'Next i

List1.ListIndex = 0
If List2.ListCount > 0 Then
    List2.ListIndex = 0
End If
List4.ListIndex = 0
Points
Ierarhic1 PanelIndex, LoadIndex
mnuCopy.Visible = False
mnuPaste.Visible = False
PasteList.Visible = False
mnuCopyOnCase.Visible = False
End Sub

Private Sub TakeOverVariables()
Dim i As Integer
Dim j As Integer
Dim k As Integer
ReDim vctIPT(NETO)
ReDim vctYPT(NETO)
Dim YPTtmp() As Variant
For i = 1 To NETO
    If Project.Item(ProjectIndex).colPanel.Item(i).colAssessmentPoints.Count > 0 Then
        vctIPT(i) = Project.Item(ProjectIndex).colPanel.Item(i).colAssessmentPoints.Count
        ReDim YPTtmp(1 To vctIPT(i))
        For j = 1 To vctIPT(i)
            YPTtmp(j) = Project.Item(ProjectIndex).colPanel.Item(i).colAssessmentPoints.Item(j).Value
        Next j
        vctYPT(i) = YPTtmp
    End If
Next i

ReDim vctIM10(1 To NETO) As Variant
ReDim vctM10(1 To NETO) As Variant
ReDim vctJ0(1 To NETO) As Variant
ReDim vctICT0(1 To NETO) As Variant
ReDim vctCJMAX0(1 To NETO) As Variant
ReDim vctINV0(1 To NETO) As Variant
ReDim vctIY0(1 To NETO) As Variant

Dim vctM10tmp() As Variant

Dim vctJ0tmp() As Variant
Dim ICT0tmp() As Variant
Dim vctCJMAXtmp() As Variant
Dim vctINVtmp() As Variant
Dim vctIYtmp() As Variant

Dim vctJ0tmp1() As Variant
Dim ICT0tmp1() As Variant
Dim vctCJMAXtmp1() As Variant
Dim vctINVtmp1() As Variant
Dim vctIYtmp1() As Variant

For i = 1 To NETO
    vctIM10(i) = Project.Item(ProjectIndex).colPanel.Item(i).IsStructuralConstraints
    If vctIM10(i) > 0 Then
        ReDim vctM10tmp(1 To Project.Item(ProjectIndex).colPanel.Item(i).colLoadCase.Count)
        ReDim vctJ0tmp(1 To Project.Item(ProjectIndex).colPanel.Item(i).colLoadCase.Count)
        ReDim ICT0tmp(1 To Project.Item(ProjectIndex).colPanel.Item(i).colLoadCase.Count)
        ReDim vctCJMAXtmp(1 To Project.Item(ProjectIndex).colPanel.Item(i).colLoadCase.Count)
        ReDim vctINVtmp(1 To Project.Item(ProjectIndex).colPanel.Item(i).colLoadCase.Count)
        ReDim vctIYtmp(1 To Project.Item(ProjectIndex).colPanel.Item(i).colLoadCase.Count)
        For j = 1 To Project.Item(ProjectIndex).colPanel.Item(i).colLoadCase.Count
            vctM10tmp(j) = Project.Item(ProjectIndex).colPanel.Item(i).colLoadCase.Item(j).colStructuralConstraints.Count
            If vctM10tmp(j) > 0 Then
                ReDim vctJ0tmp1(1 To vctM10tmp(j))
                ReDim ICT0tmp1(1 To vctM10tmp(j))
                ReDim vctCJMAXtmp1(1 To vctM10tmp(j))
                ReDim vctINVtmp1(1 To vctM10tmp(j))
                ReDim vctIYtmp1(1 To vctM10tmp(j))
                For k = 1 To Project.Item(ProjectIndex).colPanel.Item(i).colLoadCase.Item(j).colStructuralConstraints.Count
                    vctJ0tmp1(k) = Project.Item(ProjectIndex).colPanel.Item(i).colLoadCase.Item(j).colStructuralConstraints.Item(k).index
                    ICT0tmp1(k) = Project.Item(ProjectIndex).colPanel.Item(i).colLoadCase.Item(j).colStructuralConstraints.Item(k).Reference
                    vctCJMAXtmp1(k) = Project.Item(ProjectIndex).colPanel.Item(i).colLoadCase.Item(j).colStructuralConstraints.Item(k).Value
                    vctINVtmp1(k) = Project.Item(ProjectIndex).colPanel.Item(i).colLoadCase.Item(j).colStructuralConstraints.Item(k).Limit
                    vctIYtmp1(k) = Project.Item(ProjectIndex).colPanel.Item(i).colLoadCase.Item(j).colStructuralConstraints.Item(k).AssesmentPoint
                Next k
                vctJ0tmp(j) = vctJ0tmp1
                ICT0tmp(j) = ICT0tmp1
                vctCJMAXtmp(j) = vctCJMAXtmp1
                vctINVtmp(j) = vctINVtmp1
                vctIYtmp(j) = vctIYtmp1
            End If
        Next j
        vctM10(i) = vctM10tmp
        vctJ0(i) = vctJ0tmp
        vctICT0(i) = ICT0tmp
        vctCJMAX0(i) = vctCJMAXtmp
        vctINV0(i) = vctINVtmp
        vctIY0(i) = vctIYtmp
    End If
Next i




' fac vectori de 1 ramura
ReDim vctM1(1 To Project.Item(ProjectIndex).cHeader.colLoadCase.Count)
ReDim vctJ1(1)
ReDim vctICT1(1)
ReDim vctCJMAX1(1)
ReDim vctINV1(1)
ReDim vctIY1(1)
For i = 1 To Project.Item(ProjectIndex).cHeader.colLoadCase.Count
    vctM1(i) = 0
Next i
vctJ1(1) = 0
vctICT1(1) = 0
vctCJMAX1(1) = 0
vctINV1(1) = 0
vctIY1(1) = 0
' fac vectori de 2 ramuri
ReDim vctJ2(1)
ReDim vctICT2(1)
ReDim vctCJMAX2(1)
ReDim vctINV2(1)
ReDim vctIY2(1)
vctJ2(1) = vctJ1
vctICT2(1) = vctICT1
vctCJMAX2(1) = vctCJMAX1
vctINV2(1) = vctINV1
vctIY2(1) = vctIY1
' incercare: vreau sa scap de vectori empty
For i = 1 To NETO
    If IsEmpty(vctM10(i)) Then
        vctM10(i) = vctM1
    End If
    If IsEmpty(vctJ0(i)) Then
        vctJ0(i) = vctJ2
    End If
    If IsEmpty(vctICT0(i)) Then
        vctICT0(i) = vctICT2
    End If
    If IsEmpty(vctCJMAX0(i)) Then
        vctCJMAX0(i) = vctCJMAX2
    End If
    If IsEmpty(vctINV0(i)) Then
        vctINV0(i) = vctINV2
    End If
    If IsEmpty(vctIY0(i)) Then
        vctIY0(i) = vctIY2
    End If
Next i




For i = 1 To NETO
    For j = 1 To UBound(vctM10(i))  ' Project.Item(ProjectIndex).cHeader.colLoadCase.Count
        'If vctM10(i)(1) > 0 Then
        For k = 1 To (vctM10(i)(j))
            Select Case vctICT0(i)(j)(k)
                Case 1, 2, 3, 4, 5, 6, 7, 51, 52, 57, 58 ' (mm)
                    vctCJMAX0(i)(j)(k) = vctCJMAX0(i)(j)(k) * 1000
                Case 15, 14, 37 ' no dimension
                Case Else ' N/mm2
                    vctCJMAX0(i)(j)(k) = vctCJMAX0(i)(j)(k) / 1000000
            End Select
        Next k
        'End If
    Next j
Next i
End Sub

Private Sub cmdApply_Click()

ReturnVariables
Project.Item(ProjectIndex).DataChanged = True
End Sub

Private Sub ReturnVariables()
Dim i As Integer
Dim j As Integer
Dim k As Integer
Dim index As Integer
ReDim Preserve vctIM10(1 To NETO) As Variant
ReDim Preserve vctM10(1 To NETO) As Variant
ReDim Preserve vctJ0(1 To NETO) As Variant
ReDim Preserve vctICT0(1 To NETO) As Variant
ReDim Preserve vctCJMAX0(1 To NETO) As Variant
ReDim Preserve vctINV0(1 To NETO) As Variant
ReDim Preserve vctIY0(1 To NETO) As Variant
Dim vctCJMAX5 As Variant
ReDim vctCJMAX5(1 To NETO) As Variant
vctCJMAX5 = vctCJMAX0
index = 0

Dim GetTotalNumberOfStrConstr As Integer

For i = 1 To NETO
    GetTotalNumberOfStrConstr = 0
    For j = 1 To Project.Item(ProjectIndex).colPanel.Item(i).colLoadCase.Count
        GetTotalNumberOfStrConstr = GetTotalNumberOfStrConstr + vctM10(i)(j)
        If GetTotalNumberOfStrConstr > Licensing.MAX_STRUCTURAL_CONSTRAINTS_ON_PANEL Then
            MsgBox "The maximum number of structural constraints is restricted to " & Licensing.MAX_STRUCTURAL_CONSTRAINTS_ON_PANEL & " on each panel.", vbCritical + vbOKOnly, "LBR-5 License Limitation"
            Exit Sub
        End If
    Next j
Next i
' Verific daca am vreo restrictie pe toata structura
For i = 1 To NETO
    If vctIM10(i) = 0 Then
    index = index + 1
    End If
Next i
If index = NETO Then ' adica daca n-am nici o restrictie structurala pe model
    For i = 1 To NETO
        'Mesh.Item(i).IM1 = 0
        For j = Project.Item(ProjectIndex).colPanel.Item(i).colAssessmentPoints.Count To 1 Step -1
            Project.Item(ProjectIndex).colPanel.Item(i).colAssessmentPoints.Remove j
        Next j
        For j = 1 To Project.Item(ProjectIndex).cHeader.colLoadCase.Count
            Set Project.Item(ProjectIndex).colPanel.Item(i).colLoadCase.Item(j).colStructuralConstraints = Nothing
        Next j

        Project.Item(ProjectIndex).colPanel.Item(i).IsStructuralConstraints = no
    Next i
Else
    For i = 1 To NETO
        For j = Project.Item(ProjectIndex).colPanel.Item(i).colAssessmentPoints.Count To 1 Step -1
            Project.Item(ProjectIndex).colPanel.Item(i).colAssessmentPoints.Remove j
        Next j
        For j = 1 To vctIPT(i)
            'Project.Item(ProjectIndex).colPanel.Item(i).AddAssessmentPoint vctYPT(i)(j), j
            Dim m As New cAssessmentPoint
            m.index = j
            m.Value = vctYPT(i)(j)
            Project.Item(ProjectIndex).colPanel.Item(i).colAssessmentPoints.Add m, j
            Set m = Nothing
        Next j
    Next i

   ' On Error GoTo  1
    For i = 1 To vctM10(PanelIndex)(LoadIndex)
        vctJ0(PanelIndex)(LoadIndex)(i) = Val_(MSH1.TextMatrix(i, 0))
        vctICT0(PanelIndex)(LoadIndex)(i) = Val_(MSH1.TextMatrix(i, 1))
        vctCJMAX0(PanelIndex)(LoadIndex)(i) = Val_(MSH1.TextMatrix(i, 2))
        vctINV0(PanelIndex)(LoadIndex)(i) = Val_(MSH1.TextMatrix(i, 3))
        vctIY0(PanelIndex)(LoadIndex)(i) = Val_(MSH1.TextMatrix(i, 4))
    Next i
'''
'''
    For i = 1 To NETO
        For j = 1 To UBound(vctM10(i))
            For k = 1 To vctM10(i)(j)
                Select Case vctICT0(i)(j)(k)
                    Case 1, 2, 3, 4, 5, 6, 7, 51, 52, 57, 58 ' (mm)
                        vctCJMAX5(i)(j)(k) = vctCJMAX0(i)(j)(k) / 1000
                    Case 15, 14, 37 ' no dimension
                    Case Else ' N/mm2
                        vctCJMAX5(i)(j)(k) = vctCJMAX0(i)(j)(k) * 1000000
                End Select
            Next k
        Next j
    Next i
    
    For i = 1 To NETO
        Project.Item(ProjectIndex).colPanel.Item(i).IsStructuralConstraints = vctIM10(i)
        For j = 1 To Project.Item(ProjectIndex).cHeader.colLoadCase.Count
            Set Project.Item(ProjectIndex).colPanel.Item(i).colLoadCase.Item(j).colStructuralConstraints = Nothing
        Next j
        For j = 1 To Project.Item(ProjectIndex).cHeader.colLoadCase.Count
            For k = 1 To vctM10(i)(j)
                Dim cStructuralConstraint As New cStructuralConstraints
                cStructuralConstraint.index = k
                cStructuralConstraint.Reference = vctICT0(i)(j)(k)
                cStructuralConstraint.Value = vctCJMAX5(i)(j)(k)
                cStructuralConstraint.Limit = vctINV0(i)(j)(k)
                cStructuralConstraint.AssesmentPoint = vctIY0(i)(j)(k)
                Project.Item(ProjectIndex).colPanel.Item(i).colLoadCase.Item(j).colStructuralConstraints.Add cStructuralConstraint, k
                Set cStructuralConstraint = Nothing
            Next k
        Next j
    Next i
    
'''
'''    For i = 1 To NETO
'''        Mesh.Item(i).IM1 = vctIM10(i)
'''        Mesh.Item(i).M1 = vctM10(i)
'''        Mesh.Item(i).j = vctJ0(i)
'''        Mesh.Item(i).ICT = vctICT0(i)
'''        Mesh.Item(i).CJMAX = vctCJMAX5(i)
'''        Mesh.Item(i).INV = vctINV0(i)
'''        Mesh.Item(i).IY = vctIY0(i)
'''    Next i
'''1
End If
End Sub

Private Sub Points()
Dim i As Integer
List2.Clear
Select Case PanelIndex
    Case PanelIndex
        For i = 1 To vctIPT(PanelIndex)
            List2.AddItem vctYPT(PanelIndex)(i)
        Next i
    Case Else
End Select
End Sub

'FIXIT: Form_OLEDragOver event has no Visual Basic .NET equivalent and will not be upgraded.     FixIT90210ae-R7593-R67265
Private Sub Form_OLEDragOver(Data As DataObject, Effect As Long, Button As Integer, Shift As Integer, x As Single, y As Single, state As Integer)
Me.BackColor = vbRed
End Sub

Private Sub List1_Click()
Dim i As Integer
PanelIndex = List1.ListIndex + 1
If LoadIndex > 0 Then ' la prima initializare LoadIndex = 0
    If LoadIndex <= Project.Item(ProjectIndex).cHeader.colLoadCase.Count Then
        Ierarhic1 PanelIndex, LoadIndex
    End If
End If
List2.Clear
Select Case PanelIndex
    Case PanelIndex
        For i = 1 To vctIPT(PanelIndex)
            List2.AddItem vctYPT(PanelIndex)(i)
        Next i
    Case Else
End Select
Slider1.Enabled = True
If vctIM10(PanelIndex) > 0 Then
    Points
Else
    List2.Clear
End If
End Sub

Private Sub List2_Click()
PointIndex = List2.ListIndex + 1
Select Case PointIndex
    Case PointIndex
        Slider1.Value = List2.List(PointIndex - 1) * 10
        Slider1.Text = Slider1.Value / 10
End Select
End Sub

Private Sub List4_Click()
'     MSH1.Row = 2
'     MSH1.Col = 2

LoadIndex = List4.ListIndex + 1
Ierarhic1 PanelIndex, LoadIndex
End Sub

'FIXIT: Declare 'PanelIndex' and 'LoadIndex' with an early-bound data type                 FixIT90210ae-R1672-R1B8ZE
Private Sub Ierarhic1(PanelIndex, LoadIndex)
Dim i As Integer
MSH1.Cols = 5

If MSH1.Rows > 0 Then    ' altfel, daca am selectie pt un caz de incarcare,
'    MSH1.Row = 1         ' se pastreaza cand schimb cazul
End If
If vctIM10(PanelIndex) > 0 Then
    MSH1.Visible = True
    cmdRemove.Enabled = True
'    Dim rs As New ADODB.Recordset
'    rs.ActiveConnection = "provider=msdatashape;data provider=none;"
'    rs.Open "SHAPE APPEND new adInteger as Restriction," & _
'           "New adInteger  as Reference," & _
'           "New adDecimal (4) as Limit," & _
'           "New adInteger as Bound," & _
'           "New adInteger as Point", , adOpenStatic, adLockOptimistic
MSH1.TextMatrix(0, 0) = "Restriction"
MSH1.TextMatrix(0, 1) = "Reference"
MSH1.TextMatrix(0, 2) = "Limit (N/mm2;mm;-)"
MSH1.TextMatrix(0, 3) = "Bound"
MSH1.TextMatrix(0, 4) = "Point"

    Select Case LoadIndex
        Case LoadIndex
            If vctM10(PanelIndex)(LoadIndex) = 0 Then     ' - DUBIOS!!!
                cmdRemove.Enabled = False
            Else: cmdRemove.Enabled = True: End If
            MSH1.Rows = vctM10(PanelIndex)(LoadIndex) + 1
            For i = 1 To vctM10(PanelIndex)(LoadIndex)
'                      rs.AddNew Array("Restriction", "Reference", "Limit", "Bound", "Point"), _
'                      Array(i, vctICT0(PanelIndex)(LoadIndex)(i), _
'                      vctCJMAX0(PanelIndex)(LoadIndex)(i), vctINV0(PanelIndex)(LoadIndex)(i), _
'                      vctIY0(PanelIndex)(LoadIndex)(i))
                MSH1.TextMatrix(i, 0) = i
                MSH1.TextMatrix(i, 1) = vctICT0(PanelIndex)(LoadIndex)(i)
                MSH1.TextMatrix(i, 2) = vctCJMAX0(PanelIndex)(LoadIndex)(i)
                MSH1.TextMatrix(i, 3) = vctINV0(PanelIndex)(LoadIndex)(i)
                MSH1.TextMatrix(i, 4) = vctIY0(PanelIndex)(LoadIndex)(i)
            Next i
'            Set MSH1.DataSource = rs
            MSH1.AllowUserResizing = flexResizeColumns
            'MSH1.ColWidth(0) = 0
            'MSH1.ColWidth(1) = 1900
            MSH1.ColWidth(2) = 1500
            MSH1.ColWidth(3) = 740
            MSH1.ColWidth(4) = 740
'            rs.Close
    End Select
Else
    MSH1.Visible = False
    cmdRemove.Enabled = False
End If
End Sub

Function grd_index(r As Integer, c As Integer)
    grd_index = c + MSH1.Cols * r
End Function

Private Sub List4_MouseDown(Button As Integer, Shift As Integer, x As Single, y As Single)
    If Button = 2 Then
        PopupMenu mnuCopyOnCase
    End If
End Sub

Private Sub mnuCopy1_Click()
FlexCopy MSH1
mnuPaste1.Enabled = True
End Sub

Private Sub mnuCopyAll_Click()
    PasteList.Visible = True
    Label1.Caption = "Panel " & PanelIndex & " selected as source"
End Sub

Private Sub mnuCopyPasteOnCase_Click()
'    Dim i As Integer
'    For i = 1 To Project.Item(ProjectIndex).cHeader.colLoadCase.Count
'        'vctIPT(PanelIndex)(i) = vctIPT(PanelIndex)(LoadIndex)
'        'vctYPT(PanelIndex)(i) = vctYPT(PanelIndex)(LoadIndex)
'        'vctIM10(i) = vctIM10(LoadIndex)
'        'If vctM10(PanelIndex) <> Empty Then
'        If vctM10(PanelIndex)(LoadIndex) <> Empty Then
'        vctM10(PanelIndex)(i) = vctM10(PanelIndex)(LoadIndex)
'        vctJ0(PanelIndex)(i) = vctJ0(PanelIndex)(LoadIndex)
'        vctICT0(PanelIndex)(i) = vctICT0(PanelIndex)(LoadIndex)
'        vctCJMAX0(PanelIndex)(i) = vctCJMAX0(PanelIndex)(LoadIndex)
'        vctINV0(PanelIndex)(i) = vctINV0(PanelIndex)(LoadIndex)
'        vctIY0(PanelIndex)(i) = vctIY0(PanelIndex)(LoadIndex)
'        End If
''        End If
'    Next i
    
End Sub

Private Sub mnuCopyOnLoadCases_Click()
    Dim i As Integer
    For i = 1 To Project.Item(ProjectIndex).cHeader.colLoadCase.Count
        'vctIPT(PanelIndex)(i) = vctIPT(PanelIndex)(LoadIndex)
        'vctYPT(PanelIndex)(i) = vctYPT(PanelIndex)(LoadIndex)
        'vctIM10(i) = vctIM10(LoadIndex)
        'If vctM10(PanelIndex) <> Empty Then
        If vctM10(PanelIndex)(LoadIndex) <> Empty Then
        vctM10(PanelIndex)(i) = vctM10(PanelIndex)(LoadIndex)
        vctJ0(PanelIndex)(i) = vctJ0(PanelIndex)(LoadIndex)
        vctICT0(PanelIndex)(i) = vctICT0(PanelIndex)(LoadIndex)
        vctCJMAX0(PanelIndex)(i) = vctCJMAX0(PanelIndex)(LoadIndex)
        vctINV0(PanelIndex)(i) = vctINV0(PanelIndex)(LoadIndex)
        vctIY0(PanelIndex)(i) = vctIY0(PanelIndex)(LoadIndex)
        End If
'        End If
    Next i

End Sub

Private Sub mnuPaste1_Click()
FlexPaste MSH1
End Sub

Private Sub mnuPastePaste_Click()
    Dim i As Integer
    Dim j As Integer
    For i = 0 To NETO - 1
        If PasteList.Selected(i) = True Then
            vctIPT(i + 1) = vctIPT(PanelIndex)
            vctYPT(i + 1) = vctYPT(PanelIndex)
            vctIM10(i + 1) = vctIM10(PanelIndex)
            vctM10(i + 1) = vctM10(PanelIndex)
            vctJ0(i + 1) = vctJ0(PanelIndex)
            vctICT0(i + 1) = vctICT0(PanelIndex)
            vctCJMAX0(i + 1) = vctCJMAX0(PanelIndex)
            vctINV0(i + 1) = vctINV0(PanelIndex)
            vctIY0(i + 1) = vctIY0(PanelIndex)
        End If
    Next i
    PasteList.Visible = False
    Label1.Caption = "Panels:"
End Sub

Private Sub MSH1_KeyPress(KeyAscii As Integer)
    Select Case KeyAscii
        Case 3 'CTRL + C
            mnuPaste1.Enabled = True
            FlexCopy MSH1
            Exit Sub
        Case 22 'CTRL + V
            FlexPaste MSH1 ': SetData
            Exit Sub
        Case Else
    End Select
    
    MSHFlexGridEdit MSH1, TxtEdit, KeyAscii
End Sub

Private Sub MSH1_DblClick()
    MSHFlexGridEdit MSH1, TxtEdit, 32
End Sub

Sub MSHFlexGridEdit(MSHFlexgrid As Control, edt As Control, KeyAscii As Integer)
If MSH1.Row = 0 Then Exit Sub
    Select Case KeyAscii 'tasta apasata
    Case 0 To 32
        edt = MSHFlexgrid
'FIXIT: 'SelStart' is not a property of the generic 'Control' object in Visual Basic .NET. To access 'SelStart' declare 'edt' using its actual type instead of 'Control'     FixIT90210ae-R1460-RCFE85
        edt.SelStart = 1000
    Case Else
        edt = Chr(KeyAscii)
'FIXIT: 'SelStart' is not a property of the generic 'Control' object in Visual Basic .NET. To access 'SelStart' declare 'edt' using its actual type instead of 'Control'     FixIT90210ae-R1460-RCFE85
        edt.SelStart = 1
    End Select
'FIXIT: 'Move' is not a property of the generic 'Control' object in Visual Basic .NET. To access 'Move' declare 'edt' using its actual type instead of 'Control'     FixIT90210ae-R1460-RCFE85
'FIXIT: 'CellLeft' is not a property of the generic 'Control' object in Visual Basic .NET. To access 'CellLeft' declare 'MSHFlexgrid' using its actual type instead of 'Control'     FixIT90210ae-R1460-RCFE85
'FIXIT: 'CellTop' is not a property of the generic 'Control' object in Visual Basic .NET. To access 'CellTop' declare 'MSHFlexgrid' using its actual type instead of 'Control'     FixIT90210ae-R1460-RCFE85
'FIXIT: 'CellWidth' is not a property of the generic 'Control' object in Visual Basic .NET. To access 'CellWidth' declare 'MSHFlexgrid' using its actual type instead of 'Control'     FixIT90210ae-R1460-RCFE85
'FIXIT: 'CellHeight' is not a property of the generic 'Control' object in Visual Basic .NET. To access 'CellHeight' declare 'MSHFlexgrid' using its actual type instead of 'Control'     FixIT90210ae-R1460-RCFE85
    edt.Move MSHFlexgrid.Left + MSHFlexgrid.CellLeft, _
        MSHFlexgrid.Top + MSHFlexgrid.CellTop, _
        MSHFlexgrid.CellWidth - 8, _
        MSHFlexgrid.CellHeight - 8
    edt.Visible = True
'FIXIT: 'SetFocus' is not a property of the generic 'Control' object in Visual Basic .NET. To access 'SetFocus' declare 'edt' using its actual type instead of 'Control'     FixIT90210ae-R1460-RCFE85
    edt.SetFocus
End Sub

Private Sub MSH1_LostFocus()
Keeper
End Sub

Private Sub MSH1_MouseDown(Button As Integer, Shift As Integer, x As Single, y As Single)
    If Button = 2 Then
'FIXIT: PopupMenu method has no Visual Basic .NET equivalent and will not be upgraded.     FixIT90210ae-R7593-R67265
        PopupMenu mnuCopy
    End If
End Sub

Private Sub PasteList_MouseDown(Button As Integer, Shift As Integer, x As Single, y As Single)
    If Button = 2 Then
'FIXIT: PopupMenu method has no Visual Basic .NET equivalent and will not be upgraded.     FixIT90210ae-R7593-R67265
        PopupMenu mnuPaste
    End If
End Sub

Private Sub Slider1_Click()
'List2.ListIndex = 1
End Sub

Private Sub Slider1_GotFocus()
If vctIM10(PanelIndex) > 0 Then
List2.ListIndex = 1
End If
End Sub

Private Sub Slider1_MouseUp(Button As Integer, Shift As Integer, x As Single, y As Single)
Select Case PointIndex
    Case 1
        If List2.List(1) <= List2.List(0) Then
            List2.List(0) = vctYPT(PanelIndex)(1)
        Else
            vctYPT(PanelIndex)(1) = List2.List(0)
        End If
    Case 2
        If List2.List(0) >= List2.List(1) Or List2.List(2) <= List2.List(1) Then
            List2.List(1) = vctYPT(PanelIndex)(2)
        Else
            vctYPT(PanelIndex)(2) = List2.List(1)
        End If
    Case 3
        If List2.List(1) >= List2.List(2) Then
            List2.List(2) = vctYPT(PanelIndex)(3)
        Else
            vctYPT(PanelIndex)(3) = List2.List(2)
        End If
    Case Else
    MsgBox "Case not covered!", vbInformation, vbOKOnly
End Select
List2_Click ' pentru ca altfel nu face refresh
End Sub

Private Sub Slider1_Scroll()
If vctIM10(PanelIndex) = 0 Then
    Slider1.Enabled = False
Else
    Slider1.Enabled = True
    If List2.ListIndex = -1 Then
        List2.ListIndex = 0
    End If
    PointIndex = List2.ListIndex + 1
    Slider1.Text = Slider1.Value / 10
    If PointIndex = 2 Then  ' if bagat pt ca am citit in user guide ca primul si ultimul punct sunt obligatoriu
    List2.List(PointIndex - 1) = Slider1.Value / 10           ' 0 respectiv lungime panou
    End If
End If
End Sub

Private Sub TxtEdit_Change()
    On Error GoTo 1
    If Not IsNumeric(TxtEdit.Text) Then
       SendKeys "{BackSpace}"
    End If
    Exit Sub
1:
    TxtEdit.Text = buff
End Sub

Private Sub txtEdit_GotFocus()
    buff = TxtEdit.Text
End Sub

Private Sub TxtEdit_KeyPress(KeyAscii As Integer)
    If KeyAscii = Asc(vbCr) Then KeyAscii = 0
End Sub

Private Sub TxtEdit_KeyDown(KeyCode As Integer, Shift As Integer)
    EditKeyCode MSH1, TxtEdit, KeyCode, Shift
End Sub

Sub EditKeyCode(MSHFlexgrid As Control, edt As Control, KeyCode As Integer, Shift As Integer)
    Select Case KeyCode
    Case 27 ' esc
        edt.Visible = False
        MSHFlexgrid.SetFocus
    Case 13 'enter
        MSHFlexgrid.SetFocus
    Case 38 'up
        MSHFlexgrid.SetFocus
        DoEvents
        If MSHFlexgrid.Row > MSHFlexgrid.FixedRows Then
           MSHFlexgrid.Row = MSHFlexgrid.Row - 1
        End If
    Case 40 'down
        MSHFlexgrid.SetFocus
        DoEvents
        If MSHFlexgrid.Row < MSHFlexgrid.FixedRows - 1 Then
           MSHFlexgrid.Row = MSHFlexgrid.Row + 1
        End If
    End Select
End Sub
Private Sub MSH1_GotFocus()
    If TxtEdit.Visible = False Then Exit Sub
    MSH1 = TxtEdit
    TxtEdit.Visible = False
End Sub

Private Sub MSH1_LeaveCell()
If TxtEdit.Visible = False Then Exit Sub
MSH1 = TxtEdit
TxtEdit.Visible = False
End Sub

Private Sub TxtEdit_LostFocus()
MSH1_GotFocus
Keeper
End Sub

Private Sub Keeper()
Dim i As Integer
If vctIM10(PanelIndex) > 0 Then ' daca initial am restrictii pe panou
    If vctM10(PanelIndex)(LoadIndex) > 0 Then ' daca initial am restrictii pe caz de incarcare
        ReDim Preserve vctJ1(1 To vctM10(PanelIndex)(LoadIndex))
        ReDim Preserve vctICT1(1 To vctM10(PanelIndex)(LoadIndex))
        ReDim Preserve vctCJMAX1(1 To vctM10(PanelIndex)(LoadIndex))
        ReDim Preserve vctINV1(1 To vctM10(PanelIndex)(LoadIndex))
        ReDim Preserve vctIY1(1 To vctM10(PanelIndex)(LoadIndex))
        For i = 1 To vctM10(PanelIndex)(LoadIndex)
            If IsEmpty(vctJ1(i)) Then
                vctJ1(i) = 0
            End If
            If IsEmpty(vctICT1(i)) Then
                vctICT1(i) = 0
            End If
            If IsEmpty(vctCJMAX1(i)) Then
                vctCJMAX1(i) = 0
            End If
            If IsEmpty(vctINV1(i)) Then
                vctINV1(i) = 0
            End If
            If IsEmpty(vctIY1(i)) Then
                vctIY1(i) = 0
            End If
        Next i
            vctJ0(PanelIndex)(LoadIndex) = vctJ1
            ' Iau randurile vechi de valori din flex
            For i = 1 To vctM10(PanelIndex)(LoadIndex)
                vctICT1(i) = Val_(MSH1.TextMatrix(i, 1))
                vctCJMAX1(i) = Val_(MSH1.TextMatrix(i, 2))
                vctINV1(i) = Val_(MSH1.TextMatrix(i, 3))
                vctIY1(i) = Val_(MSH1.TextMatrix(i, 4))
            Next i
            ' Umplu vectorii mari
            vctICT0(PanelIndex)(LoadIndex) = vctICT1
            vctCJMAX0(PanelIndex)(LoadIndex) = vctCJMAX1
            vctINV0(PanelIndex)(LoadIndex) = vctINV1
            vctIY0(PanelIndex)(LoadIndex) = vctIY1
    End If ' daca initial am restrictii pe caz de incarcare
End If ' daca initial am restrictii pe panou
End Sub

Private Sub RefList()
If Project.Item(ProjectIndex).cHeader.IANA = 1 Then
    List3.AddItem " 1.  V absolute displacement along OX"
    List3.AddItem " 2.  U absolute displacement along OY"
    List3.AddItem " 3.  W absolute displacement along OZ"
    List3.AddItem " 4.  Minimum plate thickness"
    List3.AddItem " 5.  W1 relative deflection - departure node"
    List3.AddItem " 6.  W2 relative deflection - end node"
    List3.AddItem " 7.  W3 relative deflection with regards to the average W displacements of the 2 ends"
    'List3.AddItem "10.  Bending Strenght (Sx) - IACS"
    List3.AddItem "11.  Sigma von-Mises (Sx, Sy) plate (Z = 0, mid-plate thickness) in x = L/2"
    List3.AddItem "12.  Sigma von-Mises (Sx + Sx_STIFF, Sy) plate (Z = 0, mid-plate thickness) in x = L/2"
    List3.AddItem "13.  Sigma von-Mises (Sx + Sx_STIFF + Sx_PLATE, Sy) plate (Z = 0, mid-plate thickness) in x = L/2"
    List3.AddItem "14.  Plate buckling - Euler"
    List3.AddItem "15.  Nx/Nult (current average axial load / ultimate strenght -Paik)"
    List3.AddItem "16.  Sigma von-Mises (Txy) plate (Z = 0, mid-plate thickness) in x = 0"
    List3.AddItem "17.  Sigma von-Mises (Sx, Sy) plate (Z = +delta/2, upper face) in x = L/2"
    List3.AddItem "18.  Sigma von-Mises (Sx, Sy) plate (Z = -delta/2, lower face) in x = L/2"
    List3.AddItem "21.  Sigma von-Mises (Tweb) frame, at web-flange / web-plate jonctions in x = 0"
    List3.AddItem "22.  Sigma von-Mises (Sy, Txy) frame, at web-flange jonction in x = L/2"
    List3.AddItem "23.  Sigma von-Mises (Tweb) frame, at web-flange / web-plate jonctions as T(web-flange)=T(web-plate) in x = 0"
    List3.AddItem "24.  Sigma von-Mises (Sy, Txy) frame, at web-plate jonction in x = L/2"
    List3.AddItem "25.  Sigma von-Mises (Sy, Txy) frame, in the flange in x = L/2"
    List3.AddItem "31.  Sigma von-Mises (Tweb) stiffener, at web-plate jonction in x = 0"
    List3.AddItem "32.  Sigma von-Mises (Sx) stiffener, in the flange in x = L/2"
    List3.AddItem "33.  Sigma von-Mises (Txy_web_global + Txy_web_local) stiffener, at web-plate jonction in x = 0"
    List3.AddItem "34.  Sigma von-Mises (Sx + Sx_STIFF) stiffener, in the flange in x = L/2"
    List3.AddItem "35.  Sigma von-Mises (Sx + Sx_STIFF(PL^2/12),Txy_web_global + Txy_web_local) stiff, web-plate in x = L/2"
    List3.AddItem "36.  Sigma von-Mises (Sx + Sx_STIFF, Txy_web_global) stiffener, at web-flange jonction in x = L/2"
    List3.AddItem "37.  Stiffener buckling - Johnson"
    List3.AddItem "41.  Sigma von-Mises (Tweb) girder, at web-flange jonction in x = 0"
    List3.AddItem "42.  Sigma von-Mises (Tweb) girder, at web-plate jonction in x = 0"
    List3.AddItem "43.  Sigma von-Mises (Sx) girder, in the flange in x = L/2"
    List3.AddItem "51.  Stiffener deflection with 2 clamped ends"
    List3.AddItem "52.  Stiffener deflection with 2 simply supported ends"
    List3.AddItem "54.  Maximum stress in the stiffener's flange"
    List3.AddItem "55.  Maximum stress in the stiffener attached plate"
    List3.AddItem "56.  Maximum shear stress in then stiffener's flange"
    List3.AddItem "57.  Maximum unstiffened plate deflection (clamped edges)"
    List3.AddItem "58.  Maximum unstiffened plate deflection (simply supported edges)"
Else
    List3.AddItem "10.  Bending Strenght (Sx) - IACS"
    List3.AddItem "14.  Compression Plate Buckling (Sx/Scr) - IACS"
    List3.AddItem "19.  Shearing Strenght (Txy) - IACS"
    List3.AddItem "20.  Shear Plate Buckling (Txy) - IACS"
    List3.AddItem "37.  Stiffener Buckling (Sx/Scr) - IACS"
    List3.AddItem "38.  Stiffener Yielding (Sx + Sx_stiff) - BV"
End If

End Sub

