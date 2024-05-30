VERSION 5.00
Begin VB.MDIForm MDIForm1 
   BackColor       =   &H8000000C&
   Caption         =   "MDIForm1"
   ClientHeight    =   8475
   ClientLeft      =   5070
   ClientTop       =   1950
   ClientWidth     =   6585
   LinkTopic       =   "MDIForm1"
   Begin VB.Menu mnuOpen 
      Caption         =   "Open Properties Sheet"
   End
End
Attribute VB_Name = "MDIForm1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub mnuOpen_Click()
    Form1.Show vbModeless, Me
End Sub
