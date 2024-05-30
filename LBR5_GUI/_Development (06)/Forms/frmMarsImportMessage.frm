VERSION 5.00
Object = "{0D452EE1-E08F-101A-852E-02608C4D0BB4}#2.0#0"; "FM20.DLL"
Begin VB.Form frmMarsImportMessage 
   BorderStyle     =   4  'Fixed ToolWindow
   Caption         =   "Form1"
   ClientHeight    =   2205
   ClientLeft      =   5430
   ClientTop       =   1815
   ClientWidth     =   6105
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   2205
   ScaleWidth      =   6105
   ShowInTaskbar   =   0   'False
   Begin MSForms.CommandButton cmdOK 
      Height          =   375
      Left            =   4800
      TabIndex        =   1
      Top             =   1680
      Width           =   1095
      Caption         =   "OK"
      PicturePosition =   327683
      Size            =   "1931;661"
      FontHeight      =   165
      FontCharSet     =   0
      FontPitchAndFamily=   2
      ParagraphAlign  =   3
   End
   Begin VB.Label Label1 
      Appearance      =   0  'Flat
      Caption         =   "Label1"
      ForeColor       =   &H00404000&
      Height          =   1395
      Left            =   120
      TabIndex        =   0
      Top             =   120
      Width           =   5880
   End
End
Attribute VB_Name = "frmMarsImportMessage"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub cmdOK_Click()
    Unload Me
End Sub

Private Sub Form_Load()
    Me.Move (Screen.Width - Me.Width) / 2, (Screen.Height - Me.Height) / 2
    cmdOK.Picture = LoadResPicture("ID_BITMAP_OK", 0)
    Me.Caption = "Mars LBR5 Import - [" & GetFileName(Project.Item(ActiveProject).sFileName) & "]"
    
    Label1.Caption = "The functionality 'Import from MARS' allows the use of a data file generated " & _
                     "by the MARS2000 tool developed by Bureau Veritas (http://www.veristar.com). " & _
                     "Bureau Veritas cannot be held responsible for the use of this file made by " & _
                     "the user." & vbCrLf & vbCrLf & _
                     "The users are reminded that scantlings resulting of the LBR5 output results " & _
                     "are to be checked by Bureau Veritas for classification or certification " & _
                     "purpose."
End Sub

