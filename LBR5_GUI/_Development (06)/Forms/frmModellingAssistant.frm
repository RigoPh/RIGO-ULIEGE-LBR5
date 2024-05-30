VERSION 5.00
Begin VB.Form frmModellingAssistant 
   BorderStyle     =   5  'Sizable ToolWindow
   Caption         =   "Modelling Assistant"
   ClientHeight    =   2595
   ClientLeft      =   5625
   ClientTop       =   4785
   ClientWidth     =   6240
   Icon            =   "frmModellingAssistant.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   2595
   ScaleWidth      =   6240
   ShowInTaskbar   =   0   'False
   Begin VB.ListBox lstAssistant 
      Appearance      =   0  'Flat
      Height          =   810
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   1095
   End
End
Attribute VB_Name = "frmModellingAssistant"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Dim ProjectIndex As Integer

Private Sub Form_Load()
    ProjectIndex = ActiveProject
    Me.Caption = "Modelling Assistant - [" & GetFileName(Project.Item(ProjectIndex).sFileName) & "]"
    Me.Top = fMainForm.Height - Me.Height - 600
    Me.Left = fMainForm.Width - Me.Width - 300
    FillList
End Sub

Public Sub FillList()
    Dim i As Integer
    'CheckModel ProjectIndex
    lstAssistant.Clear
    lstAssistant.AddItem sList(0)
    For i = 1 To UBound(sList)
        lstAssistant.AddItem i & ": " & sList(i)
    Next i
End Sub

Private Sub Form_Resize()
    With lstAssistant
        .Width = ScaleWidth
        .Height = ScaleHeight
    End With
End Sub
