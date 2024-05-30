VERSION 5.00
Object = "*\ApPropertySheet.vbp"
Object = "{831FDD16-0C5C-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCTL.OCX"
Begin VB.Form Form1 
   Caption         =   "Form1"
   ClientHeight    =   7185
   ClientLeft      =   1965
   ClientTop       =   1755
   ClientWidth     =   6030
   LinkTopic       =   "Form1"
   ScaleHeight     =   7185
   ScaleWidth      =   6030
   Begin PropertySheet.TPropertySheet PS1 
      Height          =   7095
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   6015
      _ExtentX        =   10610
      _ExtentY        =   12515
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BeginProperty CatFont {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
   End
   Begin MSComctlLib.ImageList ImageList 
      Left            =   120
      Top             =   6360
      _ExtentX        =   794
      _ExtentY        =   794
      BackColor       =   16777215
      MaskColor       =   12632256
      _Version        =   393216
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub TPropertySheet1_Browse(ByVal Left As Variant, ByVal Top As Variant, ByVal Width As Variant, ByVal Prop As PropertySheet.TProperty)

End Sub

Sub AddPropertiesPS1()
    With PS1
'<Added by: Project Administrator at: 1/4/2004-19:59:14 on machine: ZEUS>
        .Redraw = False
'</Added by: Project Administrator at: 1/4/2004-19:59:14 on machine: ZEUS>
        '.ImageList = ImageList
        .ShowToolTips = True
        With .Categories
            With .Add("Appearance", , "@Use these properties to change the" & vbCrLf & "PropertyList2 appearance").Properties
                .Add "BackColor", PS1.BackColor, psColor, , psImgBackColor, , "Returns/sets the background color of the object"
                .Add "CatBackColor", PS1.CatBackColor, psColor, , psImgBackColor, , "Returns/sets catagory cell background color"
                .Add "SelBackColor", PS1.SelBackColor, psColor, , psImgBackColor, , "Returns/sets selection background color"
                .Add "CatForeColor", PS1.CatForeColor, psColor, , psImgFontColor, , "Returns/sets category foreground color used to display text and graphics of an object"
                .Add "ForeColor", PS1.ForeColor, psColor, , psImgFontColor, , "Returns/sets foreground color used to display text and graphics of an object"
                .Add "SelForeColor", PS1.SelForeColor, psColor, , psImgFontColor, , "Returns/sets selection foreground color used to display text and graphics of an object"
                .Add "GridColor", PS1.GridColor, psColor, , psImgLineColor, , "Returns/sets object grid color"
                With .Add("BorderStyle", 0, psDropDownList)
                    .ListValues.Add 0, "0 - psBorderNone"
                    .ListValues.Add 1, "1 - psBorderSingle"
                    .Value = PS1.BorderStyle
                    .Description = "Returns/sets the border style for the object"
                End With
                With .Add("Appearance", 0, psDropDownList)
                    .ListValues.Add 0, "0 - psFlat"
                    .ListValues.Add 1, "1 - ps3D"
                    .Value = PS1.Appearance
                End With
                .Add("CatFont", PS1.CatFont, psFont).Format = "n (c)"
                .Add "Font", PS1.Font, psFont
                .Add "NameWidth", PS1.NameWidth
                With .Add("ShowCategories", PS1.ShowCategories)
                    With .ListValues
                        .Item(1).Caption = "No"
                        .Item(2).Caption = "Yes"
                    End With
                End With
                With .Add("Tooltips", PS1.ShowToolTips, , , psImgPicture2)
                    With .ListValues
                        .Item(1).Caption = "Hide"
                        .Item(2).Caption = "Show"
                    End With
                End With
                .Add "ShowToolbar", True
                .Add "ShowDescription", False
'<Added by: Project Administrator at: 31/3/2004-21:16:10 on machine: ZEUS>
                With .Add("EffectStyle", psNormal, psDropDownList)
                    With .ListValues
                        .Add psNormal, "psNormal"
                        .Add psSmooth, "psSmooth"
                    End With
                End With
'</Added by: Project Administrator at: 31/3/2004-21:16:10 on machine: ZEUS>
                With .Add("DescriptionHeight", PS1.DescriptionHeight, psInteger)
                    .UpDownIncrement = 50
                End With
            End With
            With .Add("Behavior", , "@Set the control behavior").Properties
                .Add "AllowEmptyValues", PS1.AllowEmptyValues
                .Add "AutoSelect", PS1.AutoSelect
                .Add "Expandable Categories", PS1.ExpandableCategories
                .Add "RequiresEnter", PS1.RequiresEnter
                .Add "Visible", True
            End With
            With .Add("Misc", , "@Other properties")
                With .Properties
                    .Add("(About)", "", psCustom, , psImgNotes, "Click the button for information about this control", "Show propertysheet about box").ForeColor = vbBlue
                    .Add("(Revisions)", "", psCustom, , psImgWebPage, "Click the button for revision file", "Open the revision file for PropertySheet control").ForeColor = vbBlue
                    .Add("(Readme)", "", psCustom, , psImgFile, "Click the button for read-me file", "Open the readme file for PropertySheet control").ForeColor = vbBlue
                End With
            End With
            With .Add("Position", , "@Fields in red are read-only")
                With .Properties
                    With .Add("Left", PS1.Left, , True)
                        .ForeColor = vbRed
                    End With
                    .Add("Width", PS1.Width, , , psImgWidth).SetRange 2100, 2790
                    .Add("Height", PS1.Height, , , psImgHeight).SetRange 100, 3300
                    .Add("Top", PS1.Top, , True).ForeColor = vbRed
                End With
            End With
            With .Add("Formats").Properties
                With .Add("ColorFormat", "RRGGBB", psCombo).ListValues
                    .Add "&HeH&", "VB"
                    .Add "$e", "Delphi"
                    .Add "#m", "HTML"
                    .Add "r g b", "Red Green Blue"
                End With
                With .Add("Date Format", "dd-MMM-yyyy", psCombo).ListValues
                    .Add "Long Date"
                    .Add "Medium Date"
                    .Add "Short Date"
                    .Add """Today is"" dddd dd "", a really nice day.""", "Really Long Date"
                End With
                With .Add("Boolean Format", 0, psBoolean).ListValues
                    .Item(1).Caption = "Like combobox"
                    .Item(2).Caption = "Like checkbox"
                End With
            End With
        End With
'<Added by: Project Administrator at: 1/4/2004-19:59:24 on machine: ZEUS>
        .Redraw = True
'</Added by: Project Administrator at: 1/4/2004-19:59:24 on machine: ZEUS>
    End With
End Sub

Private Sub Form_Load()
AddPropertiesPS1
End Sub

