VERSION 5.00
Begin VB.Form frmImages 
   BorderStyle     =   5  'Sizable ToolWindow
   Caption         =   "Image Viewer"
   ClientHeight    =   3390
   ClientLeft      =   105
   ClientTop       =   1485
   ClientWidth     =   7545
   Icon            =   "frmImages.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   3390
   ScaleWidth      =   7545
   ShowInTaskbar   =   0   'False
   Begin VB.FileListBox FileList 
      Appearance      =   0  'Flat
      Height          =   1395
      Left            =   3000
      TabIndex        =   0
      Top             =   0
      Width           =   1095
   End
   Begin VB.Image Images 
      Height          =   1455
      Left            =   0
      Stretch         =   -1  'True
      Top             =   0
      Width           =   3015
   End
End
Attribute VB_Name = "frmImages"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub FileList_Click()
    Dim s As String
    s = FileList.FileName
    Images.Picture = LoadPicture(FileList.Path & "\" & s)
    
End Sub

Private Sub Form_Load()
    'Me.Move 0, Screen.Top '(Screen.Height - Me.Height) / 2
    FileList.Path = App.Path & "\Images"
End Sub

Private Sub Form_Resize()
    With FileList
        
        .Height = Me.ScaleHeight
        .Width = 1500
        .Left = Me.ScaleWidth - .Width
    End With
    With Images
        .Left = Me.ScaleLeft
        .Width = Me.ScaleWidth - FileList.Width
        .Top = Me.ScaleTop
        .Height = FileList.Height 'Me.ScaleHeight
    End With
    'FileList.Left = Images.Left + Images.Width - FileList.Width
End Sub

