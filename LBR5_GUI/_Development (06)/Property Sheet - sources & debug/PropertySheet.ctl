VERSION 5.00
Object = "{831FDD16-0C5C-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCTL.OCX"
Object = "{0ECD9B60-23AA-11D0-B351-00A0C9055D8E}#6.0#0"; "MSHFLXGD.OCX"
Object = "{86CF1D34-0C5F-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCT2.OCX"
Begin VB.UserControl TPropertySheet 
   ClientHeight    =   4695
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   6375
   FillStyle       =   0  'Solid
   ScaleHeight     =   4695
   ScaleWidth      =   6375
   ToolboxBitmap   =   "PropertySheet.ctx":0000
   Begin MSComctlLib.ImageList tbrImages 
      Left            =   1140
      Top             =   3240
      _ExtentX        =   1005
      _ExtentY        =   1005
      BackColor       =   -2147483643
      ImageWidth      =   16
      ImageHeight     =   16
      MaskColor       =   12632256
      _Version        =   393216
      BeginProperty Images {2C247F25-8591-11D1-B16A-00C0F0283628} 
         NumListImages   =   2
         BeginProperty ListImage1 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "PropertySheet.ctx":0312
            Key             =   "prop"
         EndProperty
         BeginProperty ListImage2 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "PropertySheet.ctx":046C
            Key             =   "sort"
         EndProperty
      EndProperty
   End
   Begin MSComctlLib.Toolbar tbrSort 
      Align           =   1  'Align Top
      Height          =   330
      Left            =   0
      TabIndex        =   13
      Top             =   0
      Width           =   6375
      _ExtentX        =   11245
      _ExtentY        =   582
      ButtonWidth     =   609
      ButtonHeight    =   582
      Style           =   1
      ImageList       =   "tbrImages"
      _Version        =   393216
      BeginProperty Buttons {66833FE8-8583-11D1-B16A-00C0F0283628} 
         NumButtons      =   2
         BeginProperty Button1 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Key             =   "prop"
            ImageKey        =   "prop"
            Style           =   2
         EndProperty
         BeginProperty Button2 {66833FEA-8583-11D1-B16A-00C0F0283628} 
            Key             =   "sort"
            ImageKey        =   "sort"
            Style           =   2
         EndProperty
      EndProperty
   End
   Begin VB.PictureBox picSplitter 
      Appearance      =   0  'Flat
      BackColor       =   &H80000003&
      BorderStyle     =   0  'None
      FillColor       =   &H00C0FFFF&
      FillStyle       =   0  'Solid
      ForeColor       =   &H80000008&
      Height          =   75
      Left            =   1320
      ScaleHeight     =   75
      ScaleWidth      =   1755
      TabIndex        =   12
      Top             =   4380
      Visible         =   0   'False
      Width           =   1755
   End
   Begin VB.PictureBox picStatus 
      BorderStyle     =   0  'None
      Height          =   915
      Left            =   2520
      ScaleHeight     =   915
      ScaleWidth      =   1575
      TabIndex        =   9
      Top             =   3000
      Width           =   1575
      Begin VB.Label lblStatusHeadLine 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   195
         Left            =   0
         TabIndex        =   11
         Top             =   0
         Width           =   75
      End
      Begin VB.Label lblStatusBody 
         BackStyle       =   0  'Transparent
         Height          =   195
         Left            =   180
         TabIndex        =   10
         Top             =   480
         Width           =   360
      End
   End
   Begin VB.PictureBox picText 
      Height          =   555
      Left            =   420
      ScaleHeight     =   495
      ScaleWidth      =   495
      TabIndex        =   8
      Top             =   3540
      Visible         =   0   'False
      Width           =   555
   End
   Begin MSComctlLib.ImageList StdImages 
      Left            =   1980
      Top             =   3600
      _ExtentX        =   794
      _ExtentY        =   794
      BackColor       =   -2147483643
      ImageWidth      =   14
      ImageHeight     =   14
      MaskColor       =   12632256
      _Version        =   393216
      BeginProperty Images {2C247F25-8591-11D1-B16A-00C0F0283628} 
         NumListImages   =   7
         BeginProperty ListImage1 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "PropertySheet.ctx":05C6
            Key             =   "frame"
         EndProperty
         BeginProperty ListImage2 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "PropertySheet.ctx":06D8
            Key             =   "plus"
         EndProperty
         BeginProperty ListImage3 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "PropertySheet.ctx":0749
            Key             =   "dots"
         EndProperty
         BeginProperty ListImage4 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "PropertySheet.ctx":0843
            Key             =   "drop"
         EndProperty
         BeginProperty ListImage5 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "PropertySheet.ctx":093D
            Key             =   "check_off"
         EndProperty
         BeginProperty ListImage6 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "PropertySheet.ctx":0A4F
            Key             =   "check_on"
         EndProperty
         BeginProperty ListImage7 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "PropertySheet.ctx":0B61
            Key             =   "minus"
         EndProperty
      EndProperty
   End
   Begin VB.ListBox lstCheck 
      Appearance      =   0  'Flat
      Height          =   30
      ItemData        =   "PropertySheet.ctx":0BCF
      Left            =   480
      List            =   "PropertySheet.ctx":0BD6
      Style           =   1  'Checkbox
      TabIndex        =   7
      Top             =   2775
      Width           =   975
   End
   Begin VB.TextBox txtList 
      Appearance      =   0  'Flat
      Height          =   492
      Left            =   840
      MultiLine       =   -1  'True
      ScrollBars      =   2  'Vertical
      TabIndex        =   6
      Top             =   1560
      Width           =   732
   End
   Begin MSComCtl2.MonthView monthView 
      Height          =   2310
      Left            =   1800
      TabIndex        =   5
      Top             =   120
      Visible         =   0   'False
      Width           =   2565
      _ExtentX        =   4524
      _ExtentY        =   4075
      _Version        =   393216
      ForeColor       =   -2147483630
      BackColor       =   -2147483635
      BorderStyle     =   1
      Appearance      =   0
      StartOfWeek     =   25362433
      TitleBackColor  =   -2147483635
      TitleForeColor  =   -2147483634
      CurrentDate     =   36675
   End
   Begin MSComCtl2.UpDown UpDown 
      Height          =   372
      Left            =   600
      TabIndex        =   4
      Top             =   1080
      Visible         =   0   'False
      Width           =   216
      _ExtentX        =   450
      _ExtentY        =   661
      _Version        =   393216
      OrigLeft        =   240
      OrigTop         =   1560
      OrigRight       =   456
      OrigBottom      =   1932
      Enabled         =   -1  'True
   End
   Begin VB.CommandButton cmdBrowse 
      Height          =   372
      Left            =   240
      Picture         =   "PropertySheet.ctx":0BDD
      Style           =   1  'Graphical
      TabIndex        =   3
      TabStop         =   0   'False
      Top             =   1080
      Visible         =   0   'False
      Width           =   372
   End
   Begin VB.ListBox lstBox 
      Appearance      =   0  'Flat
      Height          =   225
      Left            =   1080
      TabIndex        =   2
      Top             =   720
      Visible         =   0   'False
      Width           =   612
   End
   Begin VB.TextBox txtBox 
      Appearance      =   0  'Flat
      BackColor       =   &H8000000E&
      BorderStyle     =   0  'None
      Height          =   252
      Left            =   360
      TabIndex        =   1
      Top             =   720
      Visible         =   0   'False
      Width           =   492
   End
   Begin MSComctlLib.ImageList StdImages2 
      Left            =   300
      Top             =   1560
      _ExtentX        =   794
      _ExtentY        =   794
      BackColor       =   -2147483643
      ImageWidth      =   13
      ImageHeight     =   13
      MaskColor       =   12632256
      _Version        =   393216
      BeginProperty Images {2C247F25-8591-11D1-B16A-00C0F0283628} 
         NumListImages   =   11
         BeginProperty ListImage1 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "PropertySheet.ctx":0CC7
            Key             =   "minus"
         EndProperty
         BeginProperty ListImage2 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "PropertySheet.ctx":0D35
            Key             =   "plus"
         EndProperty
         BeginProperty ListImage3 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "PropertySheet.ctx":0DA6
            Key             =   "dots"
         EndProperty
         BeginProperty ListImage4 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "PropertySheet.ctx":0EA0
            Key             =   "drop"
         EndProperty
         BeginProperty ListImage5 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "PropertySheet.ctx":0F9A
            Key             =   "check_on_sel"
         EndProperty
         BeginProperty ListImage6 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "PropertySheet.ctx":10AC
            Key             =   "check_off_sel"
         EndProperty
         BeginProperty ListImage7 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "PropertySheet.ctx":11BE
            Key             =   "check_on_2"
         EndProperty
         BeginProperty ListImage8 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "PropertySheet.ctx":12D0
            Key             =   "check_off_2"
         EndProperty
         BeginProperty ListImage9 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "PropertySheet.ctx":13E2
            Key             =   "check_off"
         EndProperty
         BeginProperty ListImage10 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "PropertySheet.ctx":14F4
            Key             =   "check_on"
         EndProperty
         BeginProperty ListImage11 {2C247F27-8591-11D1-B16A-00C0F0283628} 
            Picture         =   "PropertySheet.ctx":1606
            Key             =   "frame"
         EndProperty
      EndProperty
   End
   Begin MSHierarchicalFlexGridLib.MSHFlexGrid fGrid 
      Height          =   1950
      Left            =   4380
      TabIndex        =   0
      Top             =   2280
      Width           =   1575
      _ExtentX        =   2778
      _ExtentY        =   3440
      _Version        =   393216
      GridColor       =   12632256
      ScrollTrack     =   -1  'True
      FocusRect       =   2
      GridLinesUnpopulated=   1
      ScrollBars      =   2
      BorderStyle     =   0
      BandDisplay     =   1
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      _NumberOfBands  =   1
      _Band(0).Cols   =   2
      _Band(0).TextStyleBand=   0
   End
End
Attribute VB_Name = "TPropertySheet"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
' *******************************************************
' Control      : TPropertySheet.Ctl
' Written By   : Marclei V Silva (MVS)
' Programmer   : Marclei V Silva (MVS)
' Date Writen  : 06/16/2000 -- 09:08:30
' Description  : PropertySheet control which show property
'              : and values in a spredsheet like
'              : workspace
' *******************************************************
Option Explicit
Option Compare Text
'Public Description As String
'Public Number As Long
'Public Source As String

Private Type CELL_RECT
    Top As Long
    Left As Long
    Width As Long
    Height As Long
    ButtonLeft As Long
    ButtonTop As Long
    ButtonWidth As Long
    ButtonHeight As Long
    WindowLeft As Long
    WindowTop As Long
    WindowWidth As Long
    InterfaceLeft As Long
End Type

Private rc As CELL_RECT

' Keep up with the errors
Const g_ErrConstant As Long = vbObjectError + 1000
Const m_constClassName = "TPropertySheet"
Const GAPY = 80
Const OFFSCREEN = -10000

Private m_lngErrNum As Long
Private m_strErrStr As String
Private m_strErrSource As String

Const ColCount = 4
Private Const flexSortStringNoCaseAsending = 5
Private Const flexSortNumericAscending = 3

' indicates the columns of th grid
Private Enum enPropertyColumns
    colStatus = 0
    colName = 1
    colValue = 2
    colSort = 3
End Enum

Private Const COL_WIDTH = 18
Private Const ROW_HEIGHT = 18

' Default Property Values:
Const m_def_Appearance = 1
Const m_def_BorderStyle = 1
Const m_def_ForeColor = &H80000008
Const m_def_GridColor = &HC0C0C0
Const m_def_BackColor = &HFFFFFF
Const m_def_SelBackColor = &H8000000D
Const m_def_SelForeColor = &H8000000E
Const m_def_CatBackColor = &HFFC080
Const m_def_CatForeColor = &HFFFFFF
Const m_def_ShowToolTips = 0
Const m_def_Enabled = 1
Const m_def_AllowEmptyValues = 1
Const m_def_ExpandableCategories = 1
Const m_def_NameWidth = 0
Const m_def_RequiresEnter = 0
Const m_def_ShowCategories = 1
Const m_def_ItemHeight = 8
Const m_def_ExpandedImage = 0
Const m_def_CollapsedImage = 0
Const m_def_Initializing = 0
Const m_def_AutoSelect = 1
Const m_def_DescriptionHeight = 1000
Const m_def_ShowToolbar = True
Const m_def_ShowDescription = 1
'<Added by: Project Administrator at: 31/3/2004-21:06:35 on machine: ZEUS>
Const m_def_EffectStyle = psNormal
'</Added by: Project Administrator at: 31/3/2004-21:06:35 on machine: ZEUS>

' Property Variables:
Private m_ExpandedImage As Integer
Private m_CollapsedImage As Integer
Private m_CatFont As Font
Private m_ForeColor As OLE_COLOR
Private m_GridColor As OLE_COLOR
Private m_BackColor As OLE_COLOR
Private m_SelBackColor As OLE_COLOR
Private m_SelForeColor As OLE_COLOR
Private m_CatBackColor As OLE_COLOR
Private m_CatForeColor As OLE_COLOR
Private m_ShowToolTips As Boolean
Private m_SelectedItem As Object
Private m_Enabled As Boolean
Private m_Font As Font
Private m_AllowEmptyValues As Boolean
Private m_AutoSelect As Boolean
Private m_ExpandableCategories As Boolean
Private m_NameWidth As Single
Private m_RequiresEnter As Boolean
Private m_ShowCategories As Boolean
Private m_EffectStyle As psEffectStyle

' private declaration
Private m_bEditFlag As Boolean
Private m_EditRow As Integer
Private m_BrowseWnd As Object
Private m_bDataChanged As Boolean
Private m_bBrowseMode As Boolean
Private m_OldValue As Variant
Private m_bDirty As Boolean
Private m_strBuffer As String
Private m_Categories As TCategories
Private m_LastKey As Integer
Private m_ItemHeight As Integer
Private m_SelectedRow As Integer
Private m_lPadding As Single
Private m_Properties As Collection
Private m_strText As String
Private m_bUserMode As Boolean
Private m_hIml As Long
Private m_hImlStd As Long
Private m_lIconSize As Long
Private m_bListDirty As Boolean
Private m_bMonthViewFocus As Boolean

'<Added by: Project Administrator at: 26/3/2004-10:16:45 on machine: ZEUS>
    ' this solve the problem of missing property after running the component
Private m_ShowDescription As Boolean
Private m_ShowToolbar As Boolean
'</Added by: Project Administrator at: 26/3/2004-10:16:45 on machine: ZEUS>

'<Added by: Project Administrator at: 31/3/2004-21:54:32 on machine: ZEUS>
Private m_Version As String
Private m_Redraw As Boolean
'</Added by: Project Administrator at: 31/3/2004-21:54:32 on machine: ZEUS>

' Event Declarations:
Event Browse(ByVal Left, ByVal Top, ByVal Width, ByVal Prop As TProperty)
Event CategoryCollapsed(Cancel As Boolean)
Event CategoryExpanded(Cancel As Boolean)
Event EnterEditMode(ByVal Prop As TProperty, Cancel As Boolean)
Attribute EnterEditMode.VB_Description = "Occurs when the edit control is to be shown allowing the user to edit the property"
Event GetDisplayString(ByVal Prop As TProperty, DisplayString As String, UseDefault As Boolean)
Attribute GetDisplayString.VB_Description = "Occurs when the control needs the display string of a property. This event is called only if the property has the FormatProperty set to ""CustomeDisplay"""
Event ParseString(ByVal Prop As TProperty, ByVal Text As String, UseDefault As Boolean)
Attribute ParseString.VB_Description = "Occurs when the user changes a property that has the format property set to ""CustomDisplay"""
Event PropertyChanged(ByVal Prop As TProperty, NewValue, Cancel As Boolean)
Attribute PropertyChanged.VB_Description = "Occurs when a property value is changed"
Event SelectionChanged(ByVal Prop As TProperty)
Event Click()
Attribute Click.VB_Description = "Occurs when the user presses and then releases a mouse button over an object."
Event DblClick()
Attribute DblClick.VB_Description = "Occurs when the user presses and releases a mouse button and then presses and releases it again over an object."
Event KeyDown(KeyCode As Integer, Shift As Integer)
Attribute KeyDown.VB_Description = "Occurs when the user presses a key while an object has the focus."
Event KeyPress(KeyAscii As Integer)
Attribute KeyPress.VB_Description = "Occurs when the user presses and releases an ANSI key."
Event KeyUp(KeyCode As Integer, Shift As Integer)
Attribute KeyUp.VB_Description = "Occurs when the user releases a key while an object has the focus."
Event MouseDown(Button As Integer, Shift As Integer, x As Single, y As Single)
Attribute MouseDown.VB_Description = "Occurs when the user presses the mouse button while an object has the focus."
Event MouseMove(Button As Integer, Shift As Integer, x As Single, y As Single)
Attribute MouseMove.VB_Description = "Occurs when the user moves the mouse."
Event MouseUp(Button As Integer, Shift As Integer, x As Single, y As Single)
Attribute MouseUp.VB_Description = "Occurs when the user releases the mouse button while an object has the focus."
Event EditError(ErrMessage As String)
Event HideControls()
Event BrowseForFile(ByVal Prop As TProperty, Title As String, ByRef InitDir As String, Filter As String, FilterIndex As Integer, flags As Long)
Event BrowseForFolder(ByVal Prop As TProperty, Title As String, Path As String, Prompt As String)
Event OnClear()

'<Added by: Project Administrator at: 31/3/2004-22:31:20 on machine: ZEUS>
Public Property Get Redraw() As Boolean
    Redraw = m_Redraw
End Property

Public Property Let Redraw(ByVal Value As Boolean)
    m_Redraw = Value
    If m_Redraw = False Then
        LockWindowUpdate hwnd
    Else
        LockWindowUpdate 0
        fGrid.Redraw = True
    End If
End Property

Public Property Get Version() As String
    Version = m_Version
End Property

Public Property Get EffectStyle() As psEffectStyle
    EffectStyle = m_EffectStyle
End Property

Public Property Let EffectStyle(ByVal Value As psEffectStyle)
    m_EffectStyle = Value
    PropertyChanged "EffectStyle"
End Property
'</Added by: Project Administrator at: 31/3/2004-22:31:20 on machine: ZEUS>

Public Property Get Appearance() As psAppearanceSettings
    Appearance = UserControl.Appearance
End Property

Public Property Let Appearance(ByVal New_Appearance As psAppearanceSettings)
    UserControl.Appearance() = New_Appearance
    PropertyChanged "Appearance"
End Property

Public Property Get BorderStyle() As psBorderStyle
    BorderStyle = fGrid.BorderStyle
End Property

Public Property Let BorderStyle(ByVal New_BorderStyle As psBorderStyle)
    HideControls
    fGrid.BorderStyle = New_BorderStyle
    PropertyChanged "BorderStyle"
End Property

Public Property Get Properties(Index As Variant) As TProperty
    Dim Ptr As Long
    
    On Error Resume Next
    ' get a pointer to the property
    Ptr = m_Properties(Index)
    If Ptr <> 0 Then
        ' retrieve property object
        Set Properties = ObjectFromPtr(Ptr)
    End If
End Property

Public Property Get PropertyCount() As Long
    PropertyCount = m_Properties.Count
End Property

Public Property Get Parent() As Object
    Set Parent = Extender.Parent
End Property

Public Property Get DescriptionHeight() As Long
    DescriptionHeight = picStatus.Height
End Property

Public Property Let DescriptionHeight(ByVal New_DescriptionHeight As Long)
    picStatus.Height = New_DescriptionHeight
    PropertyChanged "DescriptionHeight"
    Grid_Resize
End Property

Public Property Get Enabled() As Boolean
    Enabled = UserControl.Enabled
End Property

Public Property Let Enabled(ByVal New_Enabled As Boolean)
    UserControl.Enabled() = New_Enabled
    PropertyChanged "Enabled"
End Property

Public Property Get Font() As Font
Attribute Font.VB_Description = "Returns a Font object."
Attribute Font.VB_UserMemId = -512
    Set Font = m_Font
End Property

Public Property Set Font(ByVal New_Font As Font)
    Set m_Font = New_Font
    Set UserControl.Font = New_Font
    PropertyChanged "Font"
    Grid_Paint
End Property

Public Property Get AllowEmptyValues() As Boolean
    AllowEmptyValues = m_AllowEmptyValues
End Property

Public Property Let AllowEmptyValues(ByVal New_AllowEmptyValues As Boolean)
    m_AllowEmptyValues = New_AllowEmptyValues
    PropertyChanged "AllowEmptyValues"
End Property

Public Property Get AutoSelect() As Boolean
    AutoSelect = m_AutoSelect
End Property

Public Property Let AutoSelect(ByVal New_AutoSelect As Boolean)
    m_AutoSelect = New_AutoSelect
    PropertyChanged "AutoSelect"
End Property

Public Property Get ExpandableCategories() As Boolean
    ExpandableCategories = m_ExpandableCategories
End Property

Public Property Let ExpandableCategories(ByVal New_ExpandableCategories As Boolean)
    m_ExpandableCategories = New_ExpandableCategories
    PropertyChanged "ExpandableCategories"
End Property

Public Property Get NameWidth() As Single
    NameWidth = m_NameWidth
End Property

Public Property Let NameWidth(ByVal New_NameWidth As Single)
    m_NameWidth = New_NameWidth
    PropertyChanged "NameWidth"
    Grid_Resize
End Property

Public Property Get RequiresEnter() As Boolean
    RequiresEnter = m_RequiresEnter
End Property

Public Property Let RequiresEnter(ByVal New_RequiresEnter As Boolean)
    m_RequiresEnter = New_RequiresEnter
    PropertyChanged "RequiresEnter"
End Property

Public Property Get ShowCategories() As Boolean
    ShowCategories = m_ShowCategories
End Property

Public Property Let ShowCategories(ByVal New_ShowCategories As Boolean)
    m_ShowCategories = New_ShowCategories
    PropertyChanged "ShowCategories"
    
    If m_ShowToolbar Then
        If m_ShowCategories Then
            tbrSort.Buttons("prop").Value = 1
        Else
            tbrSort.Buttons("sort").Value = 1
        End If
    End If
    
    Grid_ShowCategories
End Property

Public Property Get ShowToolTips() As Boolean
    ShowToolTips = m_ShowToolTips
End Property

Public Property Let ShowToolTips(ByVal New_ShowToolTips As Boolean)
    m_ShowToolTips = New_ShowToolTips
    PropertyChanged "ShowToolTips"
End Property

Public Property Get ShowDescription() As Boolean
'<Modified by: Project Administrator at 26/3/2004-10:17:48 on machine: ZEUS>
    ' persistent fix for this property
    ShowDescription = m_ShowDescription 'picStatus.Visible
'</Modified by: Project Administrator at 26/3/2004-10:17:48 on machine: ZEUS>
End Property

Public Property Let ShowDescription(ByVal New_ShowDescription As Boolean)
'<Added by: Project Administrator at: 26/3/2004-10:18:19 on machine: ZEUS>
    m_ShowDescription = New_ShowDescription
'</Added by: Project Administrator at: 26/3/2004-10:18:19 on machine: ZEUS>
    picStatus.Visible = New_ShowDescription
    PropertyChanged "ShowDescription"
    Grid_Resize
End Property

Public Property Get ShowToolbar() As Boolean
    ShowToolbar = m_ShowToolbar
End Property

Public Property Let ShowToolbar(ByVal New_ShowToolbar As Boolean)
    m_ShowToolbar = New_ShowToolbar
    tbrSort.Visible = New_ShowToolbar
    PropertyChanged "ShowToolbar"
    Grid_Resize
End Property

Public Property Get CatBackColor() As OLE_COLOR
    CatBackColor = m_CatBackColor
End Property

Public Property Let CatBackColor(ByVal New_CatBackColor As OLE_COLOR)
    Dim i As Integer
    Dim Row As Integer
    
    m_CatBackColor = New_CatBackColor
    PropertyChanged "CatBackColor"
    With fGrid
        .Redraw = False
        For i = 1 To m_Categories.Count
            Row = m_Categories(i).Row
            .Row = Row
            .Col = colName
            .ColSel = colValue
            .CellBackColor = New_CatBackColor
            .Col = colStatus
            .ColSel = colName
            .CellBackColor = New_CatBackColor
        Next
        .Redraw = True
    End With
    Grid_Paint
End Property

Public Property Get CatForeColor() As OLE_COLOR
    CatForeColor = m_CatForeColor
End Property

Public Property Let CatForeColor(ByVal New_CatForeColor As OLE_COLOR)
    Dim i As Integer
    Dim Row As Integer
    
    m_CatForeColor = New_CatForeColor
    PropertyChanged "CatForeColor"
    With fGrid
        .Redraw = False
        For i = 1 To m_Categories.Count
            Row = m_Categories(i).Row
            .Row = Row
            .Col = colName
            .ColSel = colValue
            .CellForeColor = New_CatForeColor
        Next
        .Redraw = True
    End With
End Property

Public Property Get SelBackColor() As OLE_COLOR
    SelBackColor = m_SelBackColor
End Property

Public Property Let SelBackColor(ByVal New_SelBackColor As OLE_COLOR)
    m_SelBackColor = New_SelBackColor
    PropertyChanged "SelBackColor"
    Hilite m_SelectedRow
End Property

Public Property Get SelForeColor() As OLE_COLOR
    SelForeColor = m_SelForeColor
End Property

Public Property Let SelForeColor(ByVal New_SelForeColor As OLE_COLOR)
    m_SelForeColor = New_SelForeColor
    PropertyChanged "SelForeColor"
    Hilite m_SelectedRow
End Property

Public Property Get Categories() As TCategories
    Set Categories = m_Categories
End Property

Public Property Get hwnd() As Long
    hwnd = UserControl.hwnd
End Property

Public Property Get SelectedItem() As Object
    Set SelectedItem = m_SelectedItem
End Property

Public Property Get BackColor() As OLE_COLOR
    BackColor = m_BackColor
End Property

Public Property Let BackColor(ByVal New_BackColor As OLE_COLOR)
    Dim i As Integer
    Dim j As Integer
    Dim Row As Integer
    
    m_BackColor = New_BackColor
    PropertyChanged "BackColor"
    With fGrid
        .BackColor = New_BackColor
        .BackColorBkg = New_BackColor
        .BackColorUnpopulated = New_BackColor
        .GridColorFixed = New_BackColor
        .BackColorFixed = New_BackColor
    End With
    With fGrid
        .Redraw = False
        For i = 1 To m_Categories.Count
            For j = 1 To m_Categories(i).Properties.Count
                Row = m_Categories(i).Properties(j).Row
                .Row = Row
                .Col = colName
                .ColSel = colValue
                .CellBackColor = New_BackColor
                .Col = colStatus
                .ColSel = colStatus
                .CellBackColor = New_BackColor
            Next
        Next
        .Redraw = True
    End With
End Property

Public Property Get ForeColor() As OLE_COLOR
    ForeColor = m_ForeColor
End Property

Public Property Let ForeColor(ByVal New_ForeColor As OLE_COLOR)
    Dim i As Integer
    Dim j As Integer
    
    m_ForeColor = New_ForeColor
    PropertyChanged "ForeColor"
    With fGrid
        .Redraw = False
        For i = 1 To m_Categories.Count
            For j = 1 To m_Categories(i).Properties.Count
                .Row = m_Categories(i).Properties(j).Row
                .Col = colName
                .ColSel = colValue
                .CellForeColor = New_ForeColor
            Next
        Next
        .Redraw = True
    End With
End Property

Public Property Get GridColor() As OLE_COLOR
    GridColor = m_GridColor
End Property

Public Property Let GridColor(ByVal New_GridColor As OLE_COLOR)
    m_GridColor = New_GridColor
    PropertyChanged "GridColor"
    fGrid.GridColor = New_GridColor
End Property

Public Property Get CatFont() As Font
    Set CatFont = m_CatFont
End Property

Public Property Set CatFont(ByVal New_CatFont As Font)
    Dim i As Integer
    Dim Row As Integer
    Dim oldRow As Integer
    
    Set m_CatFont = New_CatFont
    
    With fGrid
        If m_Categories.Count > 0 Then
            .Redraw = False
            oldRow = .Row
            For i = 1 To m_Categories.Count
                Row = m_Categories(i).Row
                .Row = Row
                .Col = colName
                .ColSel = colValue
                .CellFontName = New_CatFont.Name
                .CellFontBold = New_CatFont.Bold
                .CellFontItalic = New_CatFont.Italic
                .CellFontStrikeThrough = New_CatFont.Strikethrough
                .CellFontUnderline = New_CatFont.Underline
                .CellFontSize = New_CatFont.Size
            Next
            .Redraw = True
            .Row = oldRow
        End If
    End With
    
    PropertyChanged "CatFont"
    Grid_Paint
    
End Property

Public Property Get ExpandedImage() As Integer
    ExpandedImage = m_ExpandedImage
End Property

Public Property Let ExpandedImage(ByVal New_ExpandedImage As Integer)
    m_ExpandedImage = New_ExpandedImage
    PropertyChanged "ExpandedImage"
    Grid_Paint
End Property

Public Property Get ItemHeight() As Integer
    ItemHeight = m_ItemHeight
End Property

Public Property Let ItemHeight(ByVal New_ItemHeight As Integer)
    m_ItemHeight = New_ItemHeight
    PropertyChanged "ItemHeight"
End Property

Public Property Get CollapsedImage() As Integer
    CollapsedImage = m_CollapsedImage
End Property

Public Property Let CollapsedImage(ByVal New_CollapsedImage As Integer)
    m_CollapsedImage = New_CollapsedImage
    PropertyChanged "CollapsedImage"
    Grid_Paint
End Property

Public Property Let ImageList(ByRef vImageList As Variant)
    m_hIml = 0
    If (VarType(vImageList) = vbLong) Then
        ' Assume a handle to an image list:
        m_hIml = vImageList
    ElseIf (VarType(vImageList) = vbObject) Then
        ' Assume a VB image list:
        On Error Resume Next
        ' Get the image list initialised..
        vImageList.ListImages(1).Draw 0, 0, 0, 1
        m_hIml = vImageList.hImageList
        If (Err.Number = 0) Then
            ' OK
            m_hIml = PtrFromObject(vImageList)
        Else
            Debug.Print "Failed to Get Image list Handle", "PropertySheet.ImageList"
        End If
        On Error GoTo 0
    End If
    If (m_hIml <> 0) Then
        Dim cx As Long, cy As Long
        If (ImageList_GetIconSize(vImageList.hImageList, cx, cy) <> 0) Then
            m_lIconSize = cy
        End If
    End If
End Property

Private Function Image_List(hIml As Long) As MSComctlLib.ImageList
    Set Image_List = ObjectFromPtr(hIml)
End Function

Public Sub Refresh()
    m_bDirty = True
    Grid_Paint
End Sub

Private Sub cmdBrowse_MouseUp(Button As Integer, Shift As Integer, x As Single, y As Single)
    If m_bMonthViewFocus Then
        m_bMonthViewFocus = False
        cmdBrowse_Click
    End If
End Sub

Private Sub lblStatusBody_MouseMove(Button As Integer, Shift As Integer, x As Single, y As Single)
    If UserControl.MousePointer = 7 Then
        UserControl.MousePointer = 0
    End If
End Sub

Private Sub lblStatusHeadLine_MouseMove(Button As Integer, Shift As Integer, x As Single, y As Single)
    If UserControl.MousePointer = 7 Then
        UserControl.MousePointer = 0
    End If
End Sub

Private Sub lstBox_LostFocus()
    On Error GoTo 1
    lstBox.Top = OFFSCREEN
    SetParent lstBox.hwnd, Extender.Parent.hwnd
1
End Sub

Private Sub lstCheck_ItemCheck(item As Integer)
    If m_bListDirty = True Then Exit Sub
    UpdateCheckList
End Sub

Private Sub lstCheck_LostFocus()
    On Error GoTo 1
    lstCheck.Top = OFFSCREEN
    SetParent lstCheck.hwnd, Extender.Parent.hwnd
1
End Sub


Private Sub monthView_KeyDown(KeyCode As Integer, Shift As Integer)
    Select Case KeyCode
        Case vbKeyReturn
            UpdateProperty monthView.Value, True
            KeyCode = 0
        Case vbKeyEscape
            monthView.Visible = False
            m_bBrowseMode = False
            KeyCode = 0
    End Select
End Sub

Private Sub monthView_LostFocus()
    monthView.Top = OFFSCREEN
    SetParent monthView.hwnd, Extender.Parent.hwnd
    m_bMonthViewFocus = True
End Sub

Private Sub picStatus_MouseMove(Button As Integer, Shift As Integer, x As Single, y As Single)
    If UserControl.MousePointer = 7 Then
        UserControl.MousePointer = 0
    End If
End Sub

Private Sub tbrSort_ButtonClick(ByVal Button As MSComctlLib.Button)
    Select Case Button.Key
        Case "prop"
            ShowCategories = True
        Case "sort"
            ShowCategories = False
    End Select
End Sub

Private Sub txtBox_GotFocus()
    If IsProperty(m_SelectedItem) Then
        If m_SelectedItem.ValueType <> psTime Then
            'SelectText
        End If
    End If
End Sub

Private Sub UserControl_InitProperties()
    ' get default font
    Set m_Font = Ambient.Font
    ' recalc padding
    RecalcPadding
    
    Set m_CatFont = Ambient.Font
    m_Enabled = m_def_Enabled
    m_AllowEmptyValues = m_def_AllowEmptyValues
    m_AutoSelect = m_def_AutoSelect
    m_ExpandableCategories = m_def_ExpandableCategories
    m_NameWidth = m_def_NameWidth
    picStatus.Height = m_def_DescriptionHeight
    m_RequiresEnter = m_def_RequiresEnter
    m_ShowCategories = m_def_ShowCategories
    
    m_ShowToolTips = m_def_ShowToolTips
    m_CatBackColor = m_def_CatBackColor
    m_CatForeColor = m_def_CatForeColor
    m_SelBackColor = m_def_SelBackColor
    m_SelForeColor = m_def_SelForeColor
    m_ItemHeight = m_def_ItemHeight
    m_BackColor = m_def_BackColor
    m_ForeColor = m_def_ForeColor
    m_GridColor = m_def_GridColor
    m_ExpandedImage = m_def_ExpandedImage
    m_CollapsedImage = m_def_CollapsedImage
'<Added by: Project Administrator at: 31/3/2004-21:12:05 on machine: ZEUS>
    m_EffectStyle = m_def_EffectStyle
    m_Redraw = True
'</Added by: Project Administrator at: 31/3/2004-21:12:05 on machine: ZEUS>
    m_bDirty = True
    fGrid.BorderStyle = m_def_BorderStyle
    UserControl.Appearance = m_def_Appearance
    

    
'<Modified by: Project Administrator at 26/3/2004-10:18:53 on machine: ZEUS>
    'picStatus.Visible = m_def_ShowDescription
    m_ShowDescription = m_def_ShowDescription
    
    'tbrSort.Visible = m_def_ShowToolbar
    m_ShowToolbar = m_def_ShowToolbar
    
'</Modified by: Project Administrator at 26/3/2004-10:18:53 on machine: ZEUS>
    
    If m_ShowToolbar Then
        If m_ShowCategories Then
            tbrSort.Buttons("prop").Value = 1
        Else
            tbrSort.Buttons("sort").Value = 1
        End If
    End If

    
    fGrid.Clear
    fGrid.Rows = 0
    fGrid.cols = ColCount
    m_Categories.Clear
    lblStatusHeadLine.Caption = ""
    lblStatusBody.Caption = ""
    Grid_Config     ' config the grid
    Grid_Resize     ' resize the grid
    fGrid.ColWidth(colStatus) = COL_WIDTH * Screen.TwipsPerPixelX
    If (UserControl.Ambient.UserMode = False) Then
        m_bUserMode = False
        Set m_Properties = Nothing
        Set m_Properties = New Collection
        With m_Categories.Add("TPropertySheet")
            .Properties.Add "(Name)", UserControl.Ambient.DisplayName
            With .Properties.Add("Selected", "Value")
                .Selected = True
            End With
        End With
    Else
        m_bUserMode = True
    End If
End Sub

Private Sub UserControl_LostFocus()
    HideControls
End Sub

Private Sub UserControl_MouseDown(Button As Integer, Shift As Integer, x As Single, y As Single)
    picSplitter.Visible = True
End Sub

Private Sub UserControl_MouseMove(Button As Integer, Shift As Integer, x As Single, y As Single)
    If y > fGrid.Top Then
        UserControl.MousePointer = 7
    End If
    If y < Extender.Height - (2 * GAPY) And y > fGrid.Top + (5 * GAPY) Then
        picSplitter.Move 0, y, Extender.Width
    End If
End Sub

Private Sub UserControl_MouseUp(Button As Integer, Shift As Integer, x As Single, y As Single)
    picSplitter.Visible = False
    picStatus.Top = picSplitter.Top
    picStatus.Height = Extender.Height - picSplitter.Top - picSplitter.Height
    Grid_Resize
End Sub

Private Sub UserControl_ReadProperties(PropBag As PropertyBag)
    ' get default Font
    Set m_Font = PropBag.ReadProperty("Font", Ambient.Font)
    ' recalc padding
    RecalcPadding
    Set m_CatFont = PropBag.ReadProperty("CatFont", Ambient.Font)
    m_Enabled = PropBag.ReadProperty("Enabled", m_def_Enabled)
    m_AllowEmptyValues = PropBag.ReadProperty("AllowEmptyValues", m_def_AllowEmptyValues)
    m_AutoSelect = PropBag.ReadProperty("AutoSelect", m_def_AutoSelect)
    m_ExpandableCategories = PropBag.ReadProperty("ExpandableCategories", m_def_ExpandableCategories)
    m_NameWidth = PropBag.ReadProperty("NameWidth", m_def_NameWidth)
    m_RequiresEnter = PropBag.ReadProperty("RequiresEnter", m_def_RequiresEnter)
    m_ShowCategories = PropBag.ReadProperty("ShowCategories", m_def_ShowCategories)
    m_ShowToolTips = PropBag.ReadProperty("ShowToolTips", m_def_ShowToolTips)
    m_CatBackColor = PropBag.ReadProperty("CatBackColor", m_def_CatBackColor)
    m_CatForeColor = PropBag.ReadProperty("CatForeColor", m_def_CatForeColor)
    m_SelBackColor = PropBag.ReadProperty("SelBackColor", m_def_SelBackColor)
    m_SelForeColor = PropBag.ReadProperty("SelForeColor", m_def_SelForeColor)
    m_ItemHeight = PropBag.ReadProperty("ItemHeight", m_def_ItemHeight)
    m_BackColor = PropBag.ReadProperty("BackColor", m_def_BackColor)
    m_ForeColor = PropBag.ReadProperty("ForeColor", m_def_ForeColor)
    m_GridColor = PropBag.ReadProperty("GridColor", m_def_GridColor)
    m_ExpandedImage = PropBag.ReadProperty("ExpandedImage", m_def_ExpandedImage)
    m_CollapsedImage = PropBag.ReadProperty("CollapsedImage", m_def_CollapsedImage)
    UserControl.Appearance = PropBag.ReadProperty("Appearance", m_def_Appearance)
    fGrid.BorderStyle = PropBag.ReadProperty("BorderStyle", m_def_BorderStyle)
    picStatus.Height = PropBag.ReadProperty("DescriptionHeight", m_def_DescriptionHeight)
'<Added by: Project Administrator at: 31/3/2004-21:09:53 on machine: ZEUS>
    m_EffectStyle = PropBag.ReadProperty("EffectStyle", m_def_EffectStyle)
'</Added by: Project Administrator at: 31/3/2004-21:09:53 on machine: ZEUS>

'<Modified by: Project Administrator at 26/3/2004-10:19:07 on machine: ZEUS>
    'picStatus.Visible = PropBag.ReadProperty("ShowDescription", m_def_ShowDescription)
    'tbrSort.Visible = PropBag.ReadProperty("ShowToolbar", m_def_ShowToolbar)
    m_ShowDescription = PropBag.ReadProperty("ShowDescription", m_def_ShowDescription)
    m_ShowToolbar = PropBag.ReadProperty("ShowToolbar", m_def_ShowToolbar)
'</Modified by: Project Administrator at 26/3/2004-10:19:07 on machine: ZEUS>
    
    If m_ShowToolbar Then
        If m_ShowCategories Then
            tbrSort.Buttons("prop").Value = 1
        Else
            tbrSort.Buttons("sort").Value = 1
        End If
    End If

    fGrid.Clear
    fGrid.Rows = 0
    fGrid.cols = ColCount
    m_Categories.Clear
    Grid_Config     ' config the grid
    Grid_Resize     ' resize the grid
    If m_ShowCategories = True Then
        fGrid.ColWidth(colStatus) = COL_WIDTH * Screen.TwipsPerPixelX
    Else
        fGrid.ColWidth(colStatus) = 0
    End If
    If (UserControl.Ambient.UserMode = False) Then
        m_bUserMode = False
        Set m_Properties = Nothing
        Set m_Properties = New Collection
        With m_Categories.Add("TPropertySheet")
            .Properties.Add "(Name)", UserControl.Ambient.DisplayName
            With .Properties.Add("Selected", "Value")
                .Selected = True
            End With
        End With
    Else
        m_bUserMode = True
    End If
End Sub

Private Sub UserControl_Show()
    Static s_bNotFirst As Boolean
    
    If Not (s_bNotFirst) Then
        ' set the parent of this resources
        'SetParent lstCheck.hwnd, Extender.Parent.hwnd
        SetParent lstBox.hwnd, Extender.Parent.hwnd
        SetParent monthView.hwnd, Extender.Parent.hwnd
        SetParent txtList.hwnd, Extender.Parent.hwnd
        ' stay on top
        StayOnTop lstBox.hwnd
        StayOnTop monthView.hwnd
        StayOnTop txtList.hwnd
        s_bNotFirst = True
    End If
End Sub

Private Sub UserControl_WriteProperties(PropBag As PropertyBag)
    Call PropBag.WriteProperty("Enabled", m_Enabled, m_def_Enabled)
    Call PropBag.WriteProperty("Font", m_Font, Ambient.Font)
    Call PropBag.WriteProperty("AllowEmptyValues", m_AllowEmptyValues, m_def_AllowEmptyValues)
    Call PropBag.WriteProperty("AutoSelect", m_AutoSelect, m_def_AutoSelect)
    Call PropBag.WriteProperty("ExpandableCategories", m_ExpandableCategories, m_def_ExpandableCategories)
    Call PropBag.WriteProperty("NameWidth", m_NameWidth, m_def_NameWidth)
    Call PropBag.WriteProperty("RequiresEnter", m_RequiresEnter, m_def_RequiresEnter)
    Call PropBag.WriteProperty("ShowCategories", m_ShowCategories, m_def_ShowCategories)
    Call PropBag.WriteProperty("ShowToolTips", m_ShowToolTips, m_def_ShowToolTips)
    Call PropBag.WriteProperty("CatBackColor", m_CatBackColor, m_def_CatBackColor)
    Call PropBag.WriteProperty("CatForeColor", m_CatForeColor, m_def_CatForeColor)
    Call PropBag.WriteProperty("SelBackColor", m_SelBackColor, m_def_SelBackColor)
    Call PropBag.WriteProperty("SelForeColor", m_SelForeColor, m_def_SelForeColor)
    Call PropBag.WriteProperty("BackColor", m_BackColor, m_def_BackColor)
    Call PropBag.WriteProperty("ForeColor", m_ForeColor, m_def_ForeColor)
    Call PropBag.WriteProperty("GridColor", m_GridColor, m_def_GridColor)
    Call PropBag.WriteProperty("CatFont", m_CatFont, Ambient.Font)
    Call PropBag.WriteProperty("ExpandedImage", m_ExpandedImage, m_def_ExpandedImage)
    Call PropBag.WriteProperty("CollapsedImage", m_CollapsedImage, m_def_CollapsedImage)
    Call PropBag.WriteProperty("Appearance", UserControl.Appearance, m_def_Appearance)
    Call PropBag.WriteProperty("BorderStyle", fGrid.BorderStyle, m_def_BorderStyle)
    Call PropBag.WriteProperty("ItemHeight", m_ItemHeight, m_def_ItemHeight)
    Call PropBag.WriteProperty("DescriptionHeight", picStatus.Height, m_def_DescriptionHeight)

'<Modified by: Project Administrator at 26/3/2004-10:19:17 on machine: ZEUS>
    Call PropBag.WriteProperty("ShowDescription", m_ShowDescription, m_def_ShowDescription)
    Call PropBag.WriteProperty("ShowToolbar", m_ShowToolbar, m_def_ShowToolbar)
'</Modified by: Project Administrator at 26/3/2004-10:19:17 on machine: ZEUS>
'<Added by: Project Administrator at: 31/3/2004-21:10:24 on machine: ZEUS>
    Call PropBag.WriteProperty("EffectStyle", EffectStyle, m_def_EffectStyle)
'</Added by: Project Administrator at: 31/3/2004-21:10:24 on machine: ZEUS>
End Sub

Private Sub UserControl_Initialize()
    m_bDirty = True
    m_NameWidth = 0
    m_hIml = 0
    ' initialize objects
    Set m_Categories = New TCategories
    ' initialize the object
    m_Categories.Init Me
    ' create properties collection
    Set m_Properties = New Collection
    ' initialize grid
    Grid_Initialize
    m_hImlStd = PtrFromObject(StdImages)
    m_EffectStyle = psNormal
    m_Version = App.Major & "." & App.Minor & "." & App.Revision
    m_Redraw = False
End Sub

Private Sub Grid_Initialize()
    ' set grid parameters for the sheet
    With fGrid
        .Redraw = False
        .Left = 0
        .Top = 0
        .FixedRows = 0
        .cols = ColCount
        .FixedCols = 1
        .ColWidth(colSort) = 0
        .GridLines = flexGridFlat
        .GridLinesFixed = flexGridNone
        .SelectionMode = flexSelectionByRow
        .FillStyle = flexFillRepeat
        .FocusRect = flexFocusNone
        .GridLines = flexGridFlat
        .Font.Name = "Verdana"
        .MergeCells = flexMergeFree
        .MergeCol(colStatus) = True
        .Redraw = True
    End With
End Sub

Private Sub UserControl_Paint()
    On Error GoTo Err_UserControl_Paint
    Const constSource As String = m_constClassName & ".UserControl_Paint"
    
    If m_bDirty = False Then Exit Sub
    Grid_Paint
    m_bDirty = False
    
    Exit Sub
Err_UserControl_Paint:
    'Err.Raise Description:=Err.Description, Number:=Err.Number, Source:=constSource
End Sub

Private Sub UserControl_Resize()
    On Error GoTo Err_UserControl_Resize
    Const constSource As String = m_constClassName & ".UserControl_Resize"

    Grid_Resize

    Exit Sub
Err_UserControl_Resize:
    'Err.Raise Description:=Err.Description, Number:=Err.Number, Source:=constSource
End Sub

Private Sub UserControl_Terminate()
    Set m_Categories = Nothing
    Set m_Properties = Nothing
End Sub

Private Sub Grid_Config()
    On Error GoTo Err_Grid_Config
    Const constSource As String = m_constClassName & ".Grid_Config"

    With fGrid
        .Redraw = False
        .GridColor = m_GridColor
        .GridLines = flexGridFlat
        .GridColorFixed = m_BackColor
        .FixedCols = 1
        .GridColorUnpopulated = vbBlue
        .BackColorFixed = m_BackColor
        .BackColorSel = m_SelBackColor
        .BackColorBkg = m_BackColor
        .BackColorUnpopulated = m_BackColor
        .BackColor = m_BackColor
        .ForeColorSel = m_SelForeColor
        .ForeColor = m_ForeColor
        Set .Font = m_Font
        .Redraw = True
    End With

    Exit Sub
Err_Grid_Config:
    'Err.Raise Description:=Err.Description, Number:=Err.Number, Source:=constSource
End Sub

Private Sub Grid_Resize()
    On Error GoTo Err_Grid_Resize
    Const constSource As String = m_constClassName & ".Grid_Resize"

    Dim wid As Single  ' column width - col(3) width
    Dim gw As Single   ' grid line width
    Dim ch As Single   ' cell height
    Dim sb As Single   ' spacer (scrollbar)
    Dim Cell As Integer
    Dim nw As Single
    Dim cols As Single
    Dim h As Double
    Dim edge As Single
    Dim picRect As RECT
    
    ' hide all the controls visible
    HideControls
    
'<Modified by: Project Administrator at 26/3/2004-10:19:24 on machine: ZEUS>
    picStatus.Visible = m_ShowDescription
    tbrSort.Visible = m_ShowToolbar
'</Modified by: Project Administrator at 26/3/2004-10:19:24 on machine: ZEUS>
    picStatus.Left = 0
    picStatus.Top = UserControl.ScaleHeight - picStatus.Height
    picStatus.Width = UserControl.ScaleWidth
    
    lblStatusHeadLine.Top = 30
    lblStatusHeadLine.Left = 30
    lblStatusBody.Left = lblStatusHeadLine.Left
    lblStatusBody.Top = lblStatusHeadLine.Height + lblStatusHeadLine.Top
    lblStatusBody.Height = picStatus.Height
    lblStatusBody.Width = picStatus.Width
    
    'RECT structure for drawing frame
    picRect.Top = 0
    picRect.Left = 0
    picRect.Right = (picStatus.Width / Screen.TwipsPerPixelX)
    picRect.Bottom = (picStatus.Height / Screen.TwipsPerPixelY)
    
    picStatus.Cls
    picStatus.AutoRedraw = True
    DrawEdge picStatus.hdc, picRect, BDR_SUNKENOUTER, BF_RECT
    tbrSort.Top = 0
    
    ' update grid columns
    With fGrid
        ' avoid flickering
        .Redraw = False
        ' save current cell
        Cell = Cell_Save
        ' update grid rect
        .Left = 0
        .Top = ToolbarHeight 'tbrSort.Height + GAPY

        fGrid.Width = UserControl.ScaleWidth
        fGrid.Height = Abs(UserControl.ScaleHeight - DescriptionPanelHeight - ToolbarHeight) 'tbrSort.Height - GAPY
        ' get grid line width in screen resolution
        gw = .GridLineWidth * Screen.TwipsPerPixelX
        ' get the cell height
        ch = .CellHeight
        On Error Resume Next
        wid = 0
        cols = 0
        ' check for ShowCategories property
        If m_ShowCategories Then
            .ColWidth(colStatus) = COL_WIDTH * Screen.TwipsPerPixelX
            wid = wid + .ColWidth(colStatus)
            cols = cols + 1
        Else
            .ColWidth(colStatus) = 0
        End If
        ' detect name width here
        If m_NameWidth = 0 Then
            nw = Cell_NameWidth '+ (colName * gw)
        Else
            nw = m_NameWidth
        End If
        ' update the name column width
        .ColWidth(colName) = nw
        ' get column 2 width
        wid = wid + .ColWidth(colName)
        ' increase columns
        cols = cols + 1
        ' sort column is invisible
        .ColWidth(colSort) = 0
        If ScrollBarVisible(.hwnd) Then
            ' If the contents don't fit in the available outline space,
            ' then we have to compensate for the width of the scrollbar.
            sb = Screen.TwipsPerPixelX * (GetSystemMetrics(SM_CXVSCROLL) + GetSystemMetrics(SM_CXBORDER))
        Else
            ' Otherwise, we don't have a scrollbar.
            sb = 0
        End If
        ' set value column width now
        .ColWidth(colValue) = (fGrid.Width - (wid + sb + ((cols + 1) * gw))) + ((cols - 1) * gw) + 2 * Screen.TwipsPerPixelX
        ' restore cell position
        Cell_Restore Cell
        ' start drawing from here
        StoreCellPosition
        .Redraw = True
    End With
    
    Exit Sub
Err_Grid_Resize:
    'Err.Raise Description:="Unexpected Error: " & Err.Description, Number:=Err.Number, Source:=constSource
End Sub

Private Sub Grid_Clear()
    On Error GoTo Err_Grid_Clear
    Const constSource As String = m_constClassName & ".Grid_Clear"

    With fGrid
        .Redraw = False
        .Clear
        .Rows = 0
        .cols = ColCount
        .Redraw = True
    End With

    Exit Sub
Err_Grid_Clear:
    'Err.Raise Description:=Err.Description, Number:=Err.Number, Source:=constSource
End Sub

Private Sub Grid_Edit(KeyAscii As Integer, bFocus As Boolean)
    On Error GoTo Err_Grid_Edit
    Const constSource As String = m_constClassName & ".Grid_Edit"

    Dim Cancel As Boolean
    If TypeName(m_SelectedItem) <> "TProperty" Then Exit Sub
    If m_SelectedItem.ReadOnly = True Then Exit Sub
    Cancel = False
    RaiseEvent EnterEditMode(m_SelectedItem, Cancel)
    If Cancel = True Then Exit Sub
    ' set last key to nothing
    m_LastKey = 0
    ' give way to windows (good after a event raise)
    DoEvents
    m_EditRow = fGrid.Row
    m_bEditFlag = True
    If IsObject(m_SelectedItem.Value) Then
        Set m_OldValue = m_SelectedItem.Value
    Else
        m_OldValue = m_SelectedItem.Value
    End If
    fGrid.Col = colValue
    ShowTextBox
    If Not IsWindowLess(m_SelectedItem) Then
        UpDown.Visible = False
        ShowBrowseButton
    Else
        cmdBrowse.Visible = False
        If IsIncremental(m_SelectedItem) Then
            ShowUpDown
        Else
            UpDown.Visible = False
        End If
    End If
    m_bDataChanged = False
    If bFocus = True Then
        If txtBox.Visible = False Then ShowTextBox
        txtBox.SetFocus
    End If
    If AutoSelect Then
        Select Case KeyAscii
            Case 0 To Asc(" ")
                txtBox.SelStart = 0
                txtBox.SelLength = Len(txtBox.Text)
            Case Else
                txtBox.Text = Chr(KeyAscii)
                txtBox.SelStart = 1
        End Select
    End If
    Exit Sub
Err_Grid_Edit:
    'Err.Raise Description:=Err.Description, Number:=Err.Number, Source:=constSource
End Sub

Friend Sub AddNewCategory(ByVal objCat As TCategory)
    On Error GoTo Err_AddNewCategory
    Const constSource As String = m_constClassName & ".AddNewCategory"

    Dim CurrRow As Integer
    Dim Ptr As Long
    Dim Index As Long
    Dim Cell As Integer
    
    ' stop flickering
    fGrid.Redraw = False
    ' save row col position
    Cell = Cell_Save
    ' hide controls
    HideControls
    ' add new category
    fGrid.AddItem "  " & vbTab & "" & vbTab & "" & vbTab & "" '& vbTab & ""
    ' set the row to update
    CurrRow = fGrid.Rows - 1
    objCat.Row = CurrRow
    ' get the pointer to the object
    Ptr = objCat.Handle
    ' get the catego
    Index = MakeDWord(objCat.Index, 0)
    ' row data will contain object pointer
    fGrid.RowData(CurrRow) = Ptr
    ' this row will be merged
    fGrid.MergeRow(CurrRow) = True
    Row_Category objCat
    Grid_Index
    ' restore cell position
    Cell_Restore Cell
    ' active drawing
    fGrid.Redraw = True
    ' give way to windows
    DoEvents
    
    Exit Sub
Err_AddNewCategory:
    fGrid.Redraw = True
    'Err.Raise Description:=Err.Description, Number:=Err.Number, Source:=constSource
End Sub

Private Sub Row_Category(objCat As TCategory)
    Dim Row As Integer
    Dim tmpBackColor As OLE_COLOR
    Dim tmpForeColor As OLE_COLOR
    Dim ObjFont As StdFont
    
    Row = objCat.Row
    Set ObjFont = m_CatFont
    With fGrid
        .Row = Row
        .Col = colStatus
        .CellPictureAlignment = flexAlignCenterCenter
        If objCat.Expanded = False Then
            Set .CellPicture = StdImages.ListImages("plus").Picture
        Else
            Set .CellPicture = StdImages.ListImages("minus").Picture
        End If
        If m_ShowCategories = True Then
            .RowHeight(Row) = PropertyHeight(False)
        Else
            .RowHeight(Row) = 0
        End If
        ' configure back color
        GetObjectColors objCat, tmpBackColor, tmpForeColor
        If m_hIml <> 0 Then
            ' set the current state for this category expanded/collapseed
            If objCat.Expanded Then
                objCat.Image = m_ExpandedImage
            Else
                objCat.Image = m_CollapsedImage
            End If
            Cell_DrawPicture Row, colName, objCat.Image
        End If
        .Col = colStatus
        .ColSel = colStatus
        .CellBackColor = tmpBackColor
        
        .Col = colName
        .ColSel = colValue
        .CellBackColor = tmpBackColor
        
        .CellForeColor = tmpForeColor
        .CellAlignment = flexAlignLeftCenter
        If Not ObjFont Is Nothing Then
            .CellFontName = ObjFont.Name
            .CellFontBold = ObjFont.Bold
            .CellFontItalic = ObjFont.Italic
            .CellFontStrikeThrough = ObjFont.Strikethrough
            .CellFontUnderline = ObjFont.Underline
            .CellFontSize = ObjFont.Size
        End If
        .Text = Pad(objCat.Caption)
    End With
End Sub

Private Sub GetObjectColors(obj As Object, Back_Color As OLE_COLOR, Optional Fore_Color As OLE_COLOR)
    If obj.Selected Then
        Back_Color = m_SelBackColor
        Fore_Color = m_SelForeColor
    Else
        If IsProperty(obj) Then
            Back_Color = m_BackColor
            Fore_Color = m_ForeColor
        Else
            Back_Color = m_CatBackColor
            Fore_Color = m_CatForeColor
        End If
        If obj.BackColor <> CLR_INVALID Then
            Back_Color = obj.BackColor
        End If
        If obj.ForeColor <> CLR_INVALID Then
            Fore_Color = obj.ForeColor
        End If
    End If
End Sub

Private Sub AddNewProperty(ByVal objProp As TProperty, ByVal Relative As TCategory)
    On Error GoTo Err_AddNewProperty
    Const constSource As String = m_constClassName & ".AddNewProperty"

    Dim strText As String
    Dim CurrRow As Integer
    Dim Index As Long
    Dim Ptr As Long
    Dim Cell As Integer
    
    ' stop flickering
    fGrid.Redraw = False
    ' save row col position
    Cell = Cell_Save
    ' hide controls
    HideControls
    ' update grid
    With fGrid
        ' add a new item
        .AddItem "" & vbTab & "" & vbTab & "" & vbTab & 0
        ' get property handle
        Ptr = objProp.Handle
        ' create unique index for this property
        Index = MakeDWord(Relative.Index, objProp.Index)
        ' save curr row
        CurrRow = .Rows - 1
        ' save row to the property object
        objProp.Row = CurrRow
        ' set handle to rowData to retrive the object later
        .RowData(CurrRow) = Ptr
        Row_Property objProp, Relative.Expanded
        Grid_Index
    End With
    ' add object pointer to properties collection
    m_Properties.Add Ptr, objProp.Caption
    ' restore cell position
    Cell_Restore Cell
    ' active drawing
    fGrid.Redraw = True
    ' give way to windows
    DoEvents

    Exit Sub
Err_AddNewProperty:
    ' active drawing
    fGrid.Redraw = True
    'Err.Raise Description:=Err.Description, Number:=Err.Number, Source:=constSource
End Sub

Private Sub Row_Property(objProp As TProperty, bExpanded As Boolean)
    Dim strDisplayText As String
    Dim Cell As Integer
    Dim tmpBackColor As OLE_COLOR
    Dim tmpForeColor As OLE_COLOR
    Dim Row As Integer
    Dim strText As String
    
    Row = objProp.Row
    ' get property name caption
    strText = Pad(objProp.Caption)
    ' get display Text
    strDisplayText = GetDisplayString(objProp)
    With fGrid
        .Row = Row
        
        .Col = colStatus
        .ColSel = colStatus
        .CellBackColor = CatBackColor
        
        .Col = colName
        ' if ShowCategories is enabled check row height
        If m_ShowCategories = True Then
            ' if category is not expanded the cell height must be zero
            If bExpanded Then
                .RowHeight(Row) = PropertyHeight(False)
            Else
                .RowHeight(Row) = 0    ' invisible
            End If
        Else
            .RowHeight(Row) = PropertyHeight(False)
        End If
        ' configure back color
        GetObjectColors objProp, tmpBackColor, tmpForeColor
        If m_hIml <> 0 Then
            ' draw picture inside column #1
            If .RowHeight(Row) <> 0 Then
                Cell_DrawPicture Row, colName, objProp.Image
            End If
        End If
        If objProp.BackColor <> CLR_INVALID Then
            .CellBackColor = tmpBackColor
        End If
        If objProp.ForeColor <> CLR_INVALID Then
            .CellForeColor = tmpForeColor
        End If
        .CellAlignment = flexAlignLeftCenter
        .Text = strText
        ' value text
        If HasGraphicInterface(objProp) Then
            DrawGraphicInterface objProp, Row
            strDisplayText = Space(m_lPadding) & strDisplayText
        End If
        .Col = colValue
        If objProp.BackColor <> CLR_INVALID Then
            .CellBackColor = tmpBackColor
        End If
        If objProp.ForeColor <> CLR_INVALID Then
            .CellForeColor = tmpForeColor
        End If
        .CellAlignment = flexAlignLeftCenter
        .Text = strDisplayText
    End With
End Sub

Private Sub fGrid_KeyDown(KeyCode As Integer, Shift As Integer)
    On Error GoTo 1
    If TypeOf m_SelectedItem Is TCategory Then
        Select Case KeyCode
            Case vbKeyLeft
                If m_SelectedItem.Expanded = True Then
                    CollapseCategory m_SelectedItem
                End If
            Case vbKeyRight
                If m_SelectedItem.Expanded = False Then
                    ExpandCategory m_SelectedItem
                End If
            Case Else
                txtBox_KeyDown KeyCode, Shift
        End Select
    End If
    Exit Sub
1
End Sub

Private Sub fGrid_LostFocus()
    If m_bDataChanged Then
        ' update value only if we don't have a browse window active
        If m_bBrowseMode = False Then
            UpdateProperty txtBox.Text
        End If
    End If
End Sub

Private Sub fGrid_Scroll()
    HideControls
End Sub

Private Sub fGrid_RowColChange()
    On Error GoTo Err_fGrid_RowColChange
    Const constSource As String = m_constClassName & ".fGrid_RowColChange"

    Dim CurrRow As Integer
    
    ' get current row
    CurrRow = fGrid.Row
    ' check row changed here
    If m_bDataChanged Then
        If m_RequiresEnter = True Then
            UpdateProperty m_OldValue
        Else
            If IsProperty(m_SelectedItem) Then
                If (m_bBrowseMode = False) Then
'<Added by: Project Administrator at: 1/4/2004-21:03:12 on machine: ZEUS>
                    If m_SelectedItem.ValueType <> psCustom Then
'</Added by: Project Administrator at: 1/4/2004-21:03:12 on machine: ZEUS>
                        If (m_SelectedItem.ListValues.Count = 0) Then
                            UpdateProperty txtBox.Text
                        Else
                            On Error Resume Next
                            If m_SelectedItem.ValueType = psCombo Then
                                UpdateProperty txtBox.Text
                            Else
                                If m_SelectedItem.ListValues.Exists(txtBox.Text) Then
                                    UpdateProperty m_SelectedItem.ListValues(txtBox.Text).Value
                                End If
                            End If
                        End If
'<Added by: Project Administrator at: 1/4/2004-21:03:19 on machine: ZEUS>
                    End If
'</Added by: Project Administrator at: 1/4/2004-21:03:19 on machine: ZEUS>
                End If
            End If
        End If
    End If
    ' highlight current row no matter the type
    Hilite CurrRow
    If IsProperty(m_SelectedItem) Then
        ' hide all controls
        HideBrowseWnd
        ' over a property then fire this event
        UpdateStatus
        RaiseEvent SelectionChanged(m_SelectedItem)
        fGrid_KeyPress Asc(" ")
    Else
        HideControls
    End If
    Exit Sub

Err_fGrid_RowColChange:
    '    'Err.Raise Description:=Err.Description, Number:=Err.Number, Source:=constSource
End Sub

Private Sub fGrid_MouseMove(Button As Integer, Shift As Integer, x As Single, y As Single)
    On Error GoTo Err_fGrid_MouseMove
    Const constSource As String = m_constClassName & ".fGrid_MouseMove"

    If UserControl.MousePointer = 7 Then
        UserControl.MousePointer = 0
    End If
    
    ' if show tips is disabled then exit
    If m_ShowToolTips = False Or m_Categories.Count = 0 Then Exit Sub
    
    Static LastRow As Integer
    Dim Row As Integer
    
    ' get the row at mouse position
    Row = fGrid.MouseRow
    ' if this is the last row then exit
    If LastRow = Row Then Exit Sub
    ' save this row
    LastRow = Row
    Dim objTemp As Object
    ' get the appropriate tip from row's object
    Set objTemp = GetRowObject(Row)
    ' sets the tip
    If Not objTemp Is Nothing Then
        fGrid.TooltipText = objTemp.TooltipText
    End If

    Exit Sub
Err_fGrid_MouseMove:
    'Err.Raise Description:=Err.Description, Number:=Err.Number, Source:=constSource
End Sub

Private Sub fGrid_DblClick()
    On Error GoTo Err_fGrid_DblClick
    Const constSource As String = m_constClassName & ".fGrid_DblClick"

    Dim Row As Integer
    
    If m_bBrowseMode Or m_Categories.Count = 0 Then Exit Sub
    ' get mouse row
    Row = fGrid.MouseRow
    ' if we are in a property cell
    If IsProperty(m_SelectedItem) Then
        If m_SelectedItem.ReadOnly = False Then
            ' check if we have to browse
            If IsBrowsable(m_SelectedItem) Then
                ' browse the property
                BrowseProperty
            Else
                ' get next avail row value
                GetNextVisibleRowValue
            End If
        Else
            ' hide the text box
            If txtBox.Visible Then
                txtBox.Visible = False
            End If
        End If
        RaiseEvent DblClick
    Else
        ' toggle category state expanded/collapsed
        ToggleCategoryState
    End If
    If txtBox.Visible Then
        txtBox.SetFocus
    End If
    Exit Sub

Err_fGrid_DblClick:
    '    'Err.Raise Description:=Err.Description, Number:=Err.Number, Source:=constSource
End Sub

Private Sub fGrid_Click()
    On Error GoTo Err_fGrid_Click
    Const constSource As String = m_constClassName & ".fGrid_Click"
    
    If m_bBrowseMode Or m_Categories.Count = 0 Then Exit Sub
    
    Dim Col As Integer
    
    ' get mouse coordinates in row/col
    Col = fGrid.MouseCol
    ' if its a category and the column is 0 then
    ' promote collapse/expand
    If m_SelectedItem Is Nothing Then
        Hilite fGrid.MouseRow
    End If
    If Not IsProperty(m_SelectedItem) Then
        If Col = 0 Then
            ToggleCategoryState
        End If
    Else
        If m_SelectedItem.ReadOnly = True Then
            HideControls
        End If
    End If

    Exit Sub
Err_fGrid_Click:
    'Err.Raise Description:=Err.Description, Number:=Err.Number, Source:=constSource
End Sub

Private Sub fGrid_KeyPress(KeyAscii As Integer)
    On Error GoTo Err_txtBox_KeyPress
    Const constSource As String = m_constClassName & ".txtBox_KeyPress"
    
    Dim FindString As String
    Dim i As Integer
    Dim n As Integer
    
    If m_Categories.Count = 0 Then Exit Sub
    ' update last key value
    m_LastKey = KeyAscii
    If (m_SelectedItem.ValueType = psBoolean Or _
       m_SelectedItem.ValueType = psDropDownList) And _
       KeyAscii > 32 Then
        FindString = Chr(KeyAscii)
        n = Len(FindString)
        For i = 1 To m_SelectedItem.ListValues.Count
            If Left(m_SelectedItem.ListValues(i).Caption, n) = FindString Then
                Exit For
            End If
        Next
        If i <= m_SelectedItem.ListValues.Count Then
            txtBox.Text = m_SelectedItem.ListValues(i).Caption
            If AutoSelect Then
                txtBox.SetFocus
                txtBox.SelStart = 0
                txtBox.SelLength = Len(m_SelectedItem.ListValues(i).Caption)
            End If
        End If
        KeyAscii = 0
    Else
        Grid_Edit KeyAscii, True
    End If

    Exit Sub
Err_txtBox_KeyPress:
    'Err.Raise Description:=Err.Description, Number:=Err.Number, Source:=constSource
End Sub

Private Sub txtBox_Click()
    If m_SelectedItem.ValueType = psDropDownList Or m_SelectedItem.ValueType = psLongText Or m_SelectedItem.ValueType = psDropDownCheckList Then
        If m_bBrowseMode = False Then
            BrowseProperty
        End If
    End If
End Sub

Private Sub txtBox_KeyPress(KeyAscii As Integer)
    On Error GoTo Err_txtBox_KeyPress
    Const constSource As String = m_constClassName & ".txtBox_KeyPress"

    Dim FindString As String
    Dim i As Integer
    Dim n As Integer
    
    ' update last key
    m_LastKey = KeyAscii
    
    If (m_SelectedItem.ValueType = psBoolean Or _
       m_SelectedItem.ValueType = psDropDownList) Then
        FindString = Chr(KeyAscii)
        n = Len(FindString)
        For i = 1 To m_SelectedItem.ListValues.Count
            If Left(m_SelectedItem.ListValues(i).Caption, n) = FindString Then
                Exit For
            End If
        Next
        If i <= m_SelectedItem.ListValues.Count Then
            txtBox.Text = m_SelectedItem.ListValues(i).Caption
            If AutoSelect Then
                txtBox.SetFocus
                txtBox.SelStart = 0
                txtBox.SelLength = Len(m_SelectedItem.ListValues(i).Caption)
            End If
        End If
        KeyAscii = 0
    Else
        If KeyAscii = vbKeyReturn Then KeyAscii = 0
    End If

    Exit Sub
Err_txtBox_KeyPress:
    'Err.Raise Description:=Err.Description, Number:=Err.Number, Source:=constSource
End Sub

Private Sub txtBox_LostFocus()
    If m_bBrowseMode = False Then
        UpdateProperty txtBox.Text
    End If
End Sub

Private Sub txtBox_DblClick()
    On Error GoTo Err_txtBox_DblClick
    Const constSource As String = m_constClassName & ".txtBox_DblClick"

    GetNextVisibleRowValue

    Exit Sub
Err_txtBox_DblClick:
    'Err.Raise Description:=Err.Description, Number:=Err.Number, Source:=constSource
End Sub

Private Sub txtBox_Change()
    On Error GoTo Err_txtBox_Change
    Const constSource As String = m_constClassName & ".txtBox_Change"

    ' text has changed
    m_bDataChanged = True
    
    Exit Sub
Err_txtBox_Change:
    'Err.Raise Description:=Err.Description, Number:=Err.Number, Source:=constSource
End Sub

Private Sub txtBox_KeyDown(KeyCode As Integer, Shift As Integer)
    On Error GoTo Err_txtBox_KeyDown
    Const constSource As String = m_constClassName & ".txtBox_KeyDown"

    Dim NewRow As Integer
    Dim AltDown As Boolean
    
    ' alt key was pressed?
    AltDown = (Shift And vbAltMask) > 0
    Select Case KeyCode
        Case vbKeyDelete
            ' if its an object or image clean it up
            If m_SelectedItem.ValueType = psObject Or _
               m_SelectedItem.ValueType = psPicture Then
                UpdateProperty Nothing, True
            ElseIf m_SelectedItem.ValueType = psLongText Then
                UpdateProperty "", True
            End If
        
        Case vbKeyEscape
            m_bDataChanged = False
            KeyCode = 0
            fGrid.SetFocus
        
        Case vbKeyReturn
            If IsWindowLess(m_SelectedItem) Or m_SelectedItem.ValueType = psCombo Then
                UpdateProperty txtBox.Text
            End If
            fGrid_RowColChange
        
        Case vbKeyDown
            If AltDown Then
                ' check if we have to browse here
                If m_SelectedItem.ValueType = psDropDownList Or _
                   m_SelectedItem.ValueType = psBoolean Or _
                   m_SelectedItem.ValueType = psDropDownCheckList Or _
                   m_SelectedItem.ValueType = psLongText Or _
                   IsArray(m_SelectedItem.Value) Or _
                   m_SelectedItem.ValueType = psCombo Or m_SelectedItem.ValueType = psDate Then
                    BrowseProperty
                ElseIf IsIncremental(m_SelectedItem) Then
                    UpdateUpDown -m_SelectedItem.UpDownIncrement
                End If
            Else
                If (m_RequiresEnter = True And m_bDataChanged = False) Or m_RequiresEnter = False Then
                    NewRow = GetNextVisibleRow
                    If NewRow <> -1 Then
                        fGrid.SetFocus
                        fGrid.Row = NewRow
                        fGrid.Refresh
                        fGrid_RowColChange
                    End If
                Else
                    Beep
                    KeyCode = 0
                End If
            End If
        Case vbKeyUp
            If AltDown Then
                If IsIncremental(m_SelectedItem) Then
                    UpdateUpDown m_SelectedItem.UpDownIncrement
                End If
            Else
                If (m_RequiresEnter = True And m_bDataChanged = False) Or m_RequiresEnter = False Then
                    NewRow = GetPreviousVisibleRow
                    If NewRow <> -1 Then
                        fGrid.SetFocus
                        fGrid.Row = NewRow
                        fGrid.Refresh
                        fGrid_RowColChange
                    End If
                Else
                    Beep
                    KeyCode = 0
                End If
            End If
    End Select

    Exit Sub
Err_txtBox_KeyDown:
    'Err.Raise Description:=Err.Description, Number:=Err.Number, Source:=constSource
End Sub

Private Sub txtList_Change()
    m_bDataChanged = True
End Sub

Private Sub txtList_KeyPress(KeyAscii As Integer)
    If KeyAscii = vbKeyReturn Then KeyAscii = 0
End Sub

Private Sub txtList_LostFocus()
    If m_bDataChanged Then
        Dim tmpValue As String
        tmpValue = StripBkLinefeed(txtList.Text)
        If IsArray(m_SelectedItem.Value) Then
            UpdateProperty Split(tmpValue, vbCrLf)
        Else
            UpdateProperty tmpValue
        End If
    End If
    txtList.Top = OFFSCREEN
    SetParent txtList.hwnd, Extender.Parent.hwnd
End Sub

Private Sub txtList_KeyDown(KeyCode As Integer, Shift As Integer)
    On Error GoTo Err_txtList_KeyDown
    Const constSource As String = m_constClassName & ".txtList_KeyDown"

    Dim CtrlDown As Boolean
    
    ' alt key was pressed?
    CtrlDown = (Shift And vbCtrlMask) > 0
    Select Case KeyCode
        Case vbKeyEscape
            m_bDataChanged = False
            fGrid_RowColChange
        Case vbKeyReturn
            If CtrlDown = False Then
                txtList_LostFocus
                fGrid_RowColChange
            End If
    End Select
    
    Exit Sub
Err_txtList_KeyDown:
    'Err.Raise Description:=Err.Description, Number:=Err.Number, Source:=constSource
End Sub

Private Sub lstBox_KeyDown(KeyCode As Integer, Shift As Integer)
    On Error GoTo Err_lstBox_KeyDown
    Const constSource As String = m_constClassName & ".lstBox_KeyDown"

    Dim Index As Integer
    
    Select Case KeyCode
        Case vbKeyReturn
            ' get list box index to access listvalues value
            Index = lstBox.ListIndex + 1
            If Index > 0 Then
                UpdateProperty m_SelectedItem.ListValues(Index).Value
            End If
        Case vbKeyEscape
            lstBox.Visible = False
            m_bBrowseMode = False
        Case vbKeyUp
            Index = lstBox.ListIndex - 1
            If Index >= 0 Then
                txtBox.Text = lstBox.List(Index)
            End If
        Case vbKeyDown
            Index = lstBox.ListIndex + 1
            If Index <= lstBox.ListCount Then
                txtBox.Text = lstBox.List(Index)
            End If
    End Select

    Exit Sub
Err_lstBox_KeyDown:
    'Err.Raise Description:=Err.Description, Number:=Err.Number, Source:=constSource
End Sub

Private Sub lstBox_MouseUp(Button As Integer, Shift As Integer, x As Single, y As Single)
    On Error GoTo Err_lstBox_MouseUp
    Const constSource As String = m_constClassName & ".lstBox_MouseUp"
    
    Dim Index As Integer
    
    ' get list box index to access listvalues value
    Index = lstBox.ListIndex + 1
    If Index > 0 Then
        UpdateProperty m_SelectedItem.ListValues(Index).Value, True
    End If
    
    Exit Sub
Err_lstBox_MouseUp:
    'Err.Raise Description:=Err.Description, Number:=Err.Number, Source:=constSource
End Sub

Private Sub lstCheck_KeyDown(KeyCode As Integer, Shift As Integer)
    On Error GoTo Err_lstCheck_KeyDown
    Const constSource As String = m_constClassName & ".lstCheck_KeyDown"

    Dim Index As Integer
    
    Select Case KeyCode
        Case 32
            UpdateCheckList
        Case vbKeyReturn
            fGrid.SetFocus
        Case vbKeyEscape
            lstCheck.Visible = False
            m_bBrowseMode = False
    End Select

    Exit Sub
Err_lstCheck_KeyDown:
    'Err.Raise Description:=Err.Description, Number:=Err.Number, Source:=constSource
End Sub

Private Sub UpdateCheckList()
    On Error GoTo Err_UpdateCheckList
    Const constSource As String = m_constClassName & ".UpdateCheckList"
    
    Dim Value As String
    Dim i As Integer
    
    For i = 0 To lstCheck.ListCount - 1
        If lstCheck.Selected(i) Then
            If Value = "" Then
                Value = m_SelectedItem.ListValues(i + 1).Value
            Else
                Value = Value & Chr(0) & m_SelectedItem.ListValues(i + 1).Value
            End If
        End If
    Next
    UpdateProperty Value, True
    
    Exit Sub
Err_UpdateCheckList:
    'Err.Raise Description:=Err.Description, Number:=Err.Number, Source:=constSource
End Sub

Private Function GetNextVisibleRowValue()
    On Error GoTo Err_GetNextVisibleRowValue
    Const constSource As String = m_constClassName & ".GetNextVisibleRowValue"

    Dim strCaption As String
    Dim i As Integer
    Dim n As Integer
    
    ' exit if there's no item value
    If m_SelectedItem.ListValues.Count = 0 Then Exit Function
    n = 1
    ' loop the item values to find the next candidate
    For i = 1 To m_SelectedItem.ListValues.Count
        strCaption = txtBox.Text
        If m_SelectedItem.ListValues(i).Caption = strCaption Then
            n = i + 1
            If n > m_SelectedItem.ListValues.Count Then
                n = 1
            End If
            Exit For
        End If
    Next
    ' update property
    UpdateProperty m_SelectedItem.ListValues(n).Value, True
    Exit Function

Err_GetNextVisibleRowValue:
    'Err.Raise Description:=Err.Description, Number:=Err.Number, Source:=constSource
End Function

Private Sub UpDown_DownClick()
    On Error GoTo Err_UpDown_DownClick
    Const constSource As String = m_constClassName & ".UpDown_DownClick"
    
    ' update with decreasing increment value
    UpdateUpDown -m_SelectedItem.UpDownIncrement
    
    Exit Sub
Err_UpDown_DownClick:
    'Err.Raise Description:=Err.Description, Number:=Err.Number, Source:=constSource
End Sub

Private Sub UpDown_UpClick()
    On Error GoTo Err_UpDown_UpClick
    Const constSource As String = m_constClassName & ".UpDown_UpClick"
    
    ' update with increasing increment
    UpdateUpDown m_SelectedItem.UpDownIncrement
    
    Exit Sub
Err_UpDown_UpClick:
    'Err.Raise Description:=Err.Description, Number:=Err.Number, Source:=constSource
End Sub

Private Sub UpdateUpDown(Increment As Variant)
    Dim Value As Variant
    Dim StartPos As Integer
    
    Value = m_SelectedItem.Value
    If m_SelectedItem.ValueType = psTime Then
        Dim Interval As String
        If txtBox.SelStart >= 0 And txtBox.SelStart <= 2 Then
            StartPos = 0
            Interval = "h"
        ElseIf txtBox.SelStart >= 3 And txtBox.SelStart <= 5 Then
            StartPos = 3
            Interval = "n"
        Else
            StartPos = 6
            Interval = "s"
        End If
        Value = DateAdd(Interval, Increment, Value)
    Else
        Dim MinValue As Variant
        Dim MaxValue As Variant
        m_SelectedItem.GetRange MinValue, MaxValue
        Value = Value + Increment
        If Not IsEmpty(MinValue) Then
            If Value < MinValue Then
                Value = MinValue
            End If
        End If
    End If
    UpdateProperty Value, True
    If m_SelectedItem.ValueType = psTime Then
        On Error Resume Next
        If AutoSelect Then
            txtBox.SetFocus
            txtBox.SelStart = StartPos
            txtBox.SelLength = 2
        End If
    Else
        SelectText
    End If
End Sub

Private Sub cmdBrowse_Click()
    On Error GoTo Err_cmdBrowse_Click
    Const constSource As String = m_constClassName & ".cmdBrowse_Click"

    m_bMonthViewFocus = False
    BrowseProperty

    Exit Sub
Err_cmdBrowse_Click:
    'Err.Raise Description:=Err.Description, Number:=Err.Number, Source:=constSource
End Sub

Private Sub HideControls()
    Dim i As Integer
    
    On Error Resume Next
    RaiseEvent HideControls
    For i = 0 To UserControl.Controls.Count - 1
        If UserControl.Controls(i).Name <> "fGrid" And UserControl.Controls(i).Name <> "lblStatusHeadline" _
           And UserControl.Controls(i).Name <> "picStatus" And UserControl.Controls(i).Name <> "lblStatusBody" _
           And UserControl.Controls(i).Name <> "tbrSort" _
           Then
            UserControl.Controls(i).Visible = False
        End If
    Next
    m_bEditFlag = False
    m_bBrowseMode = False
End Sub

Public Sub UpdateProperty(ByVal NewValue As Variant, Optional bForceUpdate As Boolean = False)
    On Error GoTo Err_UpdateProperty
    Const constSource As String = m_constClassName & ".UpdateProperty"
    
    Dim Cancel As Boolean
    Dim tmpValue As Variant
    
    ' If there is nothing to update then exit
    If (m_SelectedItem Is Nothing) Or (m_bDataChanged = False And bForceUpdate = False) Then
        Exit Sub
    End If
    ' check for AllowEmptyValues
    If m_AllowEmptyValues = False And IsVarEmpty(NewValue) Then
        RaiseEvent EditError("Value for '" & m_SelectedItem.Caption & "' cannot be empty")
        Exit Sub
    End If
    ' cancel is false
    Cancel = False
    ' get permission to change the property
    RaiseEvent PropertyChanged(m_SelectedItem, NewValue, Cancel)
    ' permission denied get out
    If Cancel = True Then
        fGrid.SetFocus
        Exit Sub
    End If
    ' disable redrawing
    fGrid.Redraw = False
    ' data changed is false now
    m_bDataChanged = False
    ' check for a passed object here
    If IsObject(NewValue) Then
        Set m_SelectedItem.Value = NewValue
    Else
        tmpValue = ConvertValue(NewValue, m_SelectedItem.ValueType)
        If Not IsNull(tmpValue) Then
            If IsIncremental(m_SelectedItem) Then
                Dim MinValue
                Dim MaxValue
                m_SelectedItem.GetRange MinValue, MaxValue
                If Not IsEmpty(MinValue) Or Not IsEmpty(MaxValue) Then
                    If Not IsEmpty(MinValue) Then
                        If tmpValue < MinValue Then tmpValue = MinValue
                    End If
                    If Not IsEmpty(MaxValue) Then
                        If tmpValue > MaxValue Then
                            tmpValue = MaxValue
                        End If
                    End If
                End If
            End If
            m_SelectedItem.Value = tmpValue
        Else
            RaiseEvent EditError("Can't update. " & m_SelectedItem.Caption & " property has invalid data for its type")
        End If
    End If
    ' update textbox text value
    If m_SelectedItem.ValueType <> psDropDownCheckList Then
        UpdateTextBox m_SelectedItem
    End If
    fGrid.Redraw = True
    
    Exit Sub
Err_UpdateProperty:
    fGrid.Redraw = True
    'Err.Raise Description:=Err.Description, Number:=Err.Number, Source:=constSource
End Sub

Private Sub BrowseProperty()
    On Error GoTo Err_BrowseProperty
    Const constSource As String = m_constClassName & ".BrowseProperty"

    ' Are we in browse mode? (window is open)
    If m_bBrowseMode Then
        If IsObject(m_BrowseWnd) Then
            On Error Resume Next
            m_BrowseWnd.Visible = False
        End If
        m_bBrowseMode = False
    Else
        ' Now we are in browse mode
        ' a specific window is open for editting purposes
        m_bBrowseMode = True
        ' raise event indicating we are browsing now
'<Modified by: Project Administrator at 1/4/2004-21:03:51 on machine: ZEUS>
        RaiseEvent Browse(rc.WindowLeft + Extender.Parent.Left + Extender.Left + (2 * Screen.TwipsPerPixelX), rc.WindowTop + Extender.Parent.Top + Extender.Top + (2 * Screen.TwipsPerPixelY), rc.WindowWidth, m_SelectedItem)
'</Modified by: Project Administrator at 1/4/2004-21:03:51 on machine: ZEUS>
'        UpdateProperty m_SelectedItem.Value, True
        ' give way to windows
        DoEvents
        ' select the edit method based on ValueType property
        Select Case m_SelectedItem.ValueType
            Case psCombo, psDropDownList, psBoolean: EditCombo
            Case psDropDownCheckList: EditCheckList
            Case psDate: EditDate
            Case psPicture: EditPicture
            Case psFont: EditFont
            Case psFile: EditFile
            Case psFolder: EditFolder
            Case psLongText: EditLongText
            Case psColor: EditColor
            Case psCustom
                If IsArray(m_SelectedItem.Value) Then
                    EditLongText
                End If
        End Select
        If txtBox.Visible = False Then
            ShowTextBox
        End If
    End If

    Exit Sub
Err_BrowseProperty:
    'Err.Raise Description:=Err.Description, Number:=Err.Number, Source:=constSource
End Sub

Private Sub EditLongText()
    On Error GoTo Err_EditLongText
    Const constSource As String = m_constClassName & ".EditLongText"

    Dim strBuffer As String
        
    SetControlFont txtList
    txtList.ZOrder
    If IsArray(m_SelectedItem.Value) Then
        strBuffer = Join(m_SelectedItem.Value, vbCrLf)
    Else
        strBuffer = m_SelectedItem.Value
    End If
    txtList.Left = rc.WindowLeft
    txtList.Width = rc.WindowWidth
    txtList.Height = m_ItemHeight * TextHeight("A")
    txtList.Top = FixTopPos(txtList.Height)
    txtList.Text = strBuffer
    m_bDataChanged = False
    Set m_BrowseWnd = txtList
    
    RepositionOnClient txtList
    txtList.Visible = True
    txtList.SetFocus

    Exit Sub
Err_EditLongText:
    'Err.Raise Description:=Err.Description, Number:=Err.Number, Source:=constSource
End Sub

Private Sub EditCombo()
    On Error GoTo Err_EditCombo
    Const constSource As String = m_constClassName & ".EditCombo"

    Dim i As Integer
    Dim h As Integer
    
    lstBox.Clear
    lstBox.ZOrder
    SetControlFont lstBox
    For i = 1 To m_SelectedItem.ListValues.Count
        lstBox.AddItem m_SelectedItem.ListValues(i).Caption
        If m_SelectedItem.ListValues(i).Value = m_SelectedItem.Value Then
            lstBox.ListIndex = lstBox.NewIndex
        End If
    Next
    If m_SelectedItem.ListValues.Count > m_ItemHeight Then
        h = m_ItemHeight * TextHeight("A")
    Else
        h = (m_SelectedItem.ListValues.Count + 1) * TextHeight("A")
    End If
    lstBox.Left = rc.WindowLeft
    lstBox.Width = rc.WindowWidth
    lstBox.Height = h
    lstBox.Top = FixTopPos(lstBox.Height)
    Set m_BrowseWnd = lstBox
    
    RepositionOnClient lstBox
    
    lstBox.Visible = True
    m_bDataChanged = False
    
    Exit Sub
Err_EditCombo:
    'Err.Raise Description:=Err.Description, Number:=Err.Number, Source:=constSource
End Sub

Private Sub EditCheckList()
    On Error GoTo Err_EditCheckList
    Const constSource As String = m_constClassName & ".EditCheckList"

    Dim vArray As Variant
    Dim h As Integer
    Dim i As Integer
    Dim j As Integer
    Dim Value As String
    Dim t As Single
    Dim Header As Single

    m_bListDirty = True
    lstCheck.Clear
    lstCheck.ZOrder
    SetControlFont lstCheck
    Value = m_SelectedItem.Value
    vArray = Split(Value, Chr(0))
    For i = 1 To m_SelectedItem.ListValues.Count
        lstCheck.AddItem m_SelectedItem.ListValues(i).Caption
        If Trim(m_SelectedItem.Value) <> "" Then
            If Not IsNull(vArray) Then
                For j = LBound(vArray) To UBound(vArray)
                    If StrComp(m_SelectedItem.ListValues(i).Value, vArray(j), vbTextCompare) = 0 Then
                        lstCheck.Selected(lstCheck.NewIndex) = True
                    End If
                Next
            End If
        End If
    Next
    lstCheck.ListIndex = -1
    lstCheck.Width = rc.WindowWidth
    If m_SelectedItem.ListValues.Count > m_ItemHeight Then
        h = m_ItemHeight * TextHeight("A")
    Else
        h = (m_SelectedItem.ListValues.Count + 1) * TextHeight("A")
    End If
    
    lstCheck.Height = h
    t = FixTopPos(lstCheck.Height)
    ' list with check box has a header so we have to skip this
    ' height off the position so that it will fit right on the
    ' screen
    Header = ((GetSystemMetrics(SM_CYCAPTION) + (GetSystemMetrics(SM_CYBORDER) * 3)) * Screen.TwipsPerPixelY)
    lstCheck.Top = t
    lstCheck.Left = rc.WindowLeft - (2 * Screen.TwipsPerPixelX)
    Set m_BrowseWnd = lstCheck
    RepositionOnClient lstCheck
    lstCheck.Visible = True
    lstCheck.SetFocus
    m_bDataChanged = False
    m_bListDirty = False
    
    Exit Sub
Err_EditCheckList:
    'Err.Raise Description:=Err.Description, Number:=Err.Number, Source:=constSource
End Sub

Private Sub UpdateTextBox(objProp As TProperty)
    On Error GoTo Err_UpdateTextBox
    Const constSource As String = m_constClassName & ".UpdateTextBox"

    Dim strDisplayStr As String
    ' get the display string for cell
    strDisplayStr = GetDisplayString(objProp)
    txtBox.Text = strDisplayStr
    m_bDataChanged = False
    Cell_ValueChanged objProp, m_EditRow
    Grid_Edit 32, False
    
    Exit Sub
    
Err_UpdateTextBox:
    'Err.Raise Description:=Err.Description, Number:=Err.Number, Source:=constSource
End Sub

Private Sub EditFont()
    On Error GoTo Err_EditFont
    Const constSource As String = m_constClassName & ".EditFont"

    Dim ObjFont As StdFont
    Dim dlgCMD As New cCommonDialog
    
    ' check if Font was specified
    If TypeOf m_SelectedItem.Value Is StdFont Then
        Set ObjFont = m_SelectedItem.Value
    Else
        ' create a new Font
        Set ObjFont = New StdFont
    End If
    With dlgCMD
        .DialogTitle = "Font"
        .FontName = ObjFont.Name
        .FontBold = ObjFont.Bold
        .FontItalic = ObjFont.Italic
        .FontSize = ObjFont.Size
        .FontStrikethru = ObjFont.Strikethrough
        .FontUnderline = ObjFont.Underline
        .flags = CF_BOTH + CF_EFFECTS
'<Added by: Project Administrator at: 26/3/2004-09:47:43 on machine: ZEUS>
        .hwnd = Me.hwnd ' this avoid font dialog in taskbar
'</Added by: Project Administrator at: 26/3/2004-09:47:43 on machine: ZEUS>
        .ShowFont
        ObjFont.Name = .FontName
        ObjFont.Size = .FontSize
        ObjFont.Bold = .FontBold
        ObjFont.Italic = .FontItalic
        ObjFont.Underline = .FontUnderline
        ObjFont.Strikethrough = .FontStrikethru
    End With
    ' update property
    UpdateProperty ObjFont, True
    ' clean up
    Set dlgCMD = Nothing
    Exit Sub
    
Err_EditFont:
    Set dlgCMD = Nothing
    m_bBrowseMode = False
End Sub

Private Sub EditPicture()
    On Error GoTo Err_EditPicture
    Const constSource As String = m_constClassName & ".EditPicture"
    
    Dim strFileName As String
    Dim sTitle As String
    Dim sFilter As String
    Dim iFilterIndex As Integer
    Dim lFlags As Long
    Dim dlgCMD As New cCommonDialog
    Dim Pict As StdPicture
    Dim InitDir As String
    
    ' check if file already exists
    On Error Resume Next
    ' get filename
    strFileName = m_SelectedItem.Value
    ' check if file exist
    Dir strFileName
    ' update file name
    If Err.Number <> 0 Then
        If Len(strFileName) = 3 Then
            strFileName = strFileName & "*.*"
        Else
            strFileName = ""
        End If
    End If
    ' these vars will be passed to the user define
    sTitle = "Open Picture"
    sFilter = "Picture Files|*.bmp;*.gif;*.jpg;*.jpeg;*.wmf;*.ico;.png|All Files (*.*)|*.*"
    iFilterIndex = 1
    lFlags = OFN_FILEMUSTEXIST
    InitDir = CurDir
    ' call the event for user definition
    RaiseEvent BrowseForFile(m_SelectedItem, sTitle, InitDir, sFilter, iFilterIndex, lFlags)
    With dlgCMD
        .InitDir = InitDir
        .Filter = sFilter
        .DialogTitle = sTitle
        .FilterIndex = iFilterIndex
        .Filename = strFileName
        .flags = lFlags
        .hwnd = hwnd
        .ShowOpen
        If Len(.Filename) > 0 Then
            On Error Resume Next
            Set Pict = LoadPicture(.Filename)
            If Err = 0 Then
                UpdateProperty Pict, True
            End If
        End If
    End With
    Set dlgCMD = Nothing
    m_bBrowseMode = False

    Exit Sub
Err_EditPicture:
    Set dlgCMD = Nothing
    m_bBrowseMode = False
End Sub

Private Sub EditDate()
    On Error GoTo Err_EditDate
    Const constSource As String = m_constClassName & ".EditDate"

    Dim tmpValue As Date
    Dim ctrlTop As Single
    
    If IsDate(m_SelectedItem.Value) Then
        tmpValue = CDate(m_SelectedItem.Value)
    Else
        tmpValue = Date
    End If
    With monthView
        .Left = rc.WindowLeft
        ctrlTop = rc.WindowTop
        ctrlTop = FixTopPos(.Height)
        .Top = -10000
        .Day = Day(tmpValue)
        .Month = Month(tmpValue)
        .Year = Year(tmpValue)
        .ZOrder
        .Visible = True
    End With
    
    RepositionOnClient monthView, ctrlTop
    
    monthView.SetFocus
    Set m_BrowseWnd = monthView

    Exit Sub
Err_EditDate:
    'Err.Raise Description:=Err.Description, Number:=Err.Number, Source:=constSource
End Sub

Private Sub monthView_DateClick(ByVal DateClicked As Date)
    UpdateProperty DateClicked, True
End Sub

Private Sub EditFile()
    On Error GoTo Err_EditFile
    Const constSource As String = m_constClassName & ".EditFile"

    Dim strFileName As String
    Dim sTitle As String
    Dim sFilter As String
    Dim iFilterIndex As Integer
    Dim lFlags As Long
    Dim dlgCMD As New cCommonDialog
    Dim InitDir As String
    
    ' check if file already exists
    On Error Resume Next
    ' get filename
    strFileName = m_SelectedItem.Value
    ' check if file exist
    Dir strFileName
    ' update file name
    If Err.Number <> 0 Then
        If Len(strFileName) = 3 Then
            strFileName = strFileName & "*.*"
        Else
            strFileName = ""
        End If
    End If
    ' these vars will be passed to the user define
    sTitle = "Open"
    sFilter = "All files (*.*)|*.*"
    iFilterIndex = 1
    lFlags = OFN_FILEMUSTEXIST
    InitDir = CurDir
    ' call the event for user defined vars
    RaiseEvent BrowseForFile( _
       m_SelectedItem, _
       sTitle, _
       InitDir, _
       sFilter, _
       iFilterIndex, _
       lFlags)
    ' update dialog properties
    With dlgCMD
        .InitDir = InitDir
        .Filter = sFilter
        .DialogTitle = sTitle
        .FilterIndex = iFilterIndex
        .Filename = strFileName
        .CancelError = True
        .flags = lFlags
        .hwnd = hwnd
        .ShowOpen
        strFileName = .Filename
    End With
    If Len(strFileName) > 0 Then
        ' update property
        UpdateProperty strFileName, True
    End If
    ' clean dialog object
    Set dlgCMD = Nothing
    
    Exit Sub
Err_EditFile:
    Set dlgCMD = Nothing
    m_bBrowseMode = False
End Sub

Private Sub EditFolder()
    On Error GoTo Err_EditFolder
    Const constSource As String = m_constClassName & ".EditFolder"

    Dim sPath As String
    Dim sPrompt As String
    Dim sTitle As String
    
    ' set default properties
    sPath = m_SelectedItem.Value
    sPrompt = "Select destination path"
    sTitle = "Browse for folders"
    ' raise this event for user cutomizations
    RaiseEvent BrowseForFolder(m_SelectedItem, sTitle, sPath, sPrompt)
    ' browse for folder
    If BrowseForFolder(Extender.Parent.hwnd, sPrompt, sPath) Then
        ' update property
        UpdateProperty sPath, True
    End If
    
    Exit Sub
Err_EditFolder:
    m_bBrowseMode = False
End Sub

Private Sub EditColor()
    On Error GoTo Err_EditColor
    Const constSource As String = m_constClassName & ".EditColor"
    
    Dim CurrColor As Long
    Dim dlgCMD As New cCommonDialog
    
    CurrColor = Val(m_SelectedItem.Value)
    With dlgCMD
        .DialogTitle = m_SelectedItem.Caption
        .CancelError = True
        .flags = CC_AnyColor Or CC_FullOpen
        .Color = CurrColor
        .hwnd = hwnd
        .ShowColor
        UpdateProperty .Color, True
    End With
    ' clean it up
    Set dlgCMD = Nothing
    
    Exit Sub
Err_EditColor:
    Set dlgCMD = Nothing
    m_bBrowseMode = False
End Sub

Private Sub ShowTextBox()
    On Error GoTo Err_ShowTextBox
    Const constSource As String = m_constClassName & ".ShowTextBox"
    
    Dim strDisplayStr As String
    Dim MinValue As Variant
    Dim MaxValue As Variant
    
    txtBox.Visible = False
    ' use correct Font
    SetControlFont txtBox
    ' update dimensions
    txtBox.Left = rc.Left + (1 * Screen.TwipsPerPixelX)
    txtBox.Top = rc.Top + 1 * Screen.TwipsPerPixelY
    txtBox.Width = rc.Width - (5 * Screen.TwipsPerPixelX)
    txtBox.Height = rc.Height - (3 * Screen.TwipsPerPixelY)
    If HasGraphicInterface(m_SelectedItem) Then
        txtBox.Left = rc.InterfaceLeft + (Screen.TwipsPerPixelX)
        txtBox.Width = rc.Width - (rc.InterfaceLeft - rc.Left) - (4 * Screen.TwipsPerPixelX)
    End If
    ' check for max length specification
    m_SelectedItem.GetRange MinValue, MaxValue
    ' if max value is numeric then we have a length restriction
    If IsNumeric(MaxValue) Then
        txtBox.MaxLength = MaxValue
    Else
        txtBox.MaxLength = 255
    End If
    If m_SelectedItem.Format = "Password" Then
        txtBox.PasswordChar = "*"
        strDisplayStr = m_SelectedItem.Value
    Else
        txtBox.PasswordChar = ""
        strDisplayStr = GetDisplayString(m_SelectedItem)
    End If
    txtBox.Text = strDisplayStr
    If IsReadOnly(m_SelectedItem) Then
        txtBox.Locked = True
    Else
        txtBox.Locked = False
    End If
    txtBox.Enabled = True
    txtBox.Visible = True

    Exit Sub
Err_ShowTextBox:
    ' cant show the text box within the current cell
    txtBox.Visible = False
End Sub

'<CSCM>
'--------------------------------------------------------------------------------
' Project      :       PropertySheet
' Procedure    :       ShowBrowseButton
' Description  :       Show cell browse button
' Created by   :       Project Administrator
' Machine      :       ZEUS
' Date-Time    :       23/3/2004-19:20:19
'
' Parameters   :
' Return Values:
'--------------------------------------------------------------------------------
'</CSCM>
Private Sub ShowBrowseButton()
    On Error GoTo Err_ShowBrowseButton
    Const constSource As String = m_constClassName & ".ShowBrowseButton"
    ' configure button dimensions
    With cmdBrowse
        .Top = rc.ButtonTop
        .Width = rc.ButtonWidth
        .Left = rc.ButtonLeft - Screen.TwipsPerPixelX
        .Height = rc.ButtonHeight
    End With
    ' update button image
    If HasBrowseButton(m_SelectedItem.ValueType) Then
        cmdBrowse.Picture = StdImages.ListImages("dots").Picture
    Else
        cmdBrowse.Picture = StdImages.ListImages("drop").Picture
    End If
    cmdBrowse.Visible = True
    txtBox.Width = (txtBox.Width - cmdBrowse.Width) + Screen.TwipsPerPixelX

    Exit Sub
Err_ShowBrowseButton:
    txtBox.Visible = False
End Sub

'<CSCM>
'--------------------------------------------------------------------------------
' Project      :       PropertySheet
' Procedure    :       HasBrowseButton
' Description  :       Check if a value type has browse button
' Created by   :       Project Administrator
' Machine      :       ZEUS
' Date-Time    :       23/3/2004-19:21:25
'
' Parameters   :       ValueType (psPropertyType)
' Return Values:
'--------------------------------------------------------------------------------
'</CSCM>
Private Function HasBrowseButton(ValueType As psPropertyType) As Boolean
    HasBrowseButton = ValueType = psFile Or _
                      ValueType = psFolder Or _
                      ValueType = psColor Or _
                      ValueType = psPicture Or _
                      ValueType = psCustom Or _
                      ValueType = psFont
End Function

Private Sub ShowUpDown()
    On Error GoTo Err_ShowBrowseButton
    Const constSource As String = m_constClassName & ".ShowUpDown"

    Dim MinValue As Variant
    Dim MaxValue As Variant
    Dim Increment As Variant
    
    ' get property range
    m_SelectedItem.GetRange MinValue, MaxValue
    ' set default values
    If IsEmpty(MinValue) Then MinValue = -999999
    If IsEmpty(MaxValue) Then MaxValue = 999999
    ' update min/max values
    UpDown.Min = MinValue
    UpDown.Max = MaxValue
    ' add increment
    Increment = m_SelectedItem.UpDownIncrement
    ' check for a numeric value here
    If IsNumeric(Increment) Then
        If Increment = 0 Then Increment = 1
    Else
        Increment = 1
    End If
    ' configure updown dimensions
    With UpDown
        .Increment = Increment
        .Top = rc.ButtonTop
        .Width = rc.ButtonWidth
        .Left = rc.ButtonLeft - Screen.TwipsPerPixelX
        .Height = rc.ButtonHeight
        .Visible = True
    End With
    txtBox.Width = (txtBox.Width - UpDown.Width) + Screen.TwipsPerPixelX
    
    Exit Sub
Err_ShowBrowseButton:
    txtBox.Visible = False
End Sub

Friend Function GetRowObject(ByVal Row As Integer) As Object
    On Error GoTo Err_GetRowObject
    Const constSource As String = m_constClassName & ".GetRowObject"

    Dim Ptr As Long
    
    If fGrid.Rows = 0 Then Exit Function
    Ptr = fGrid.RowData(Row)
    Set GetRowObject = ObjectFromPtr(Ptr)
    Exit Function

Err_GetRowObject:
    '    'Err.Raise Description:=Err.Description, Number:=Err.Number, Source:=constSource
End Function

Private Sub CollapseCategory(objCat As TCategory)
    On Error GoTo Err_CollapseCategory
    Const constSource As String = m_constClassName & ".CollapseCategory"

    Dim Cancel As Boolean
    Dim i As Integer
    Dim Row As Integer
    Dim Cell As Integer
'<Added by: Project Administrator at: 31/3/2004-21:47:18 on machine: ZEUS>
    Dim j As Integer
    Dim h As Integer
    Dim tmpRow As Integer
'</Added by: Project Administrator at: 31/3/2004-21:47:18 on machine: ZEUS>
    
    If ((objCat.Expanded = False) Or (m_ExpandableCategories = False)) Then Exit Sub
    Cancel = False
    RaiseEvent CategoryCollapsed(Cancel)
    If Cancel = True Then Exit Sub
    Row = m_SelectedRow
    If m_EffectStyle = psNormal Then StopFlicker hwnd
    Cell = Cell_Save
    With fGrid
        For i = objCat.Properties.Count To 1 Step -1
            tmpRow = objCat.Properties(i).Row
'<Added by: Project Administrator at: 31/3/2004-21:47:27 on machine: ZEUS>
            If EffectStyle = psNormal Then
                ' nothing
                .RowHeight(tmpRow) = 0
            Else
                h = .RowHeight(tmpRow)
                For j = h To 0 Step -15
                    .RowHeight(tmpRow) = j
                    Wait 0.00001
                Next
            End If
            .Row = tmpRow
            If m_hIml <> 0 Then
                Cell_ClearPicture tmpRow, colName
                Cell_ClearPicture tmpRow, colValue
            End If
        Next
    End With
'</Added by: Project Administrator at: 31/3/2004-21:47:27 on machine: ZEUS>
    SetState Row, colStatus, False
    objCat.Expanded = False
    ' set the apropriate icon
    objCat.Image = m_CollapsedImage
    Cell_DrawPicture Row, colName, m_CollapsedImage
    Grid_Resize
    Cell_Restore Cell
'<Modified by: Project Administrator at 31/3/2004-21:46:54 on machine: ZEUS>
    If m_EffectStyle = psNormal Then Release
'</Modified by: Project Administrator at 31/3/2004-21:46:54 on machine: ZEUS>
    
    Exit Sub
Err_CollapseCategory:
    Release
    'Err.Raise Description:=Err.Description, Number:=Err.Number, Source:=constSource
End Sub

Private Sub ExpandCategory(objCat As TCategory)
    On Error GoTo Err_ExpandCategory
    Const constSource As String = m_constClassName & ".ExpandCategory"

    Dim Cancel As Boolean
    Dim i As Integer
    Dim Row As Integer
    Dim objProp As TProperty
    Dim j As Integer
    Dim h As Integer
    Dim tmpRow As Integer
    
    If ((objCat.Expanded = True) Or (m_ExpandableCategories = False)) Then Exit Sub
    Cancel = False
    RaiseEvent CategoryExpanded(Cancel)
    If Cancel = True Then Exit Sub
    Row = m_SelectedRow
'<Modified by: Project Administrator at 31/3/2004-21:46:39 on machine: ZEUS>
    If m_EffectStyle = psNormal Then StopFlicker hwnd
'</Modified by: Project Administrator at 31/3/2004-21:46:39 on machine: ZEUS>
    h = PropertyHeight(True)
    For i = 1 To objCat.Properties.Count
'<Modified by: Project Administrator at 31/3/2004-21:46:35 on machine: ZEUS>
        tmpRow = objCat.Properties(i).Row
        If EffectStyle = psNormal Then
            ' nothing
            fGrid.RowHeight(tmpRow) = PropertyHeight(True)
        Else
            For j = 0 To h Step 15
                fGrid.RowHeight(tmpRow) = j
                Wait 0.00001
            Next
        End If
'</Modified by: Project Administrator at 31/3/2004-21:46:35 on machine: ZEUS>
        If m_hIml <> 0 Then
            Cell_DrawPicture tmpRow, colName, objCat.Properties(i).Image
        End If
        If HasGraphicInterface(objCat.Properties(i)) Then
            DrawGraphicInterface objCat.Properties(i), tmpRow
        End If
    Next
    SetState Row, colStatus, True
    objCat.Expanded = True
    ' set the apropriate icon
    objCat.Image = m_ExpandedImage
    Cell_DrawPicture Row, colName, m_ExpandedImage
    Grid_Resize
'<Modified by: Project Administrator at 31/3/2004-21:46:46 on machine: ZEUS>
    If m_EffectStyle = psNormal Then Release
'</Modified by: Project Administrator at 31/3/2004-21:46:46 on machine: ZEUS>
    
    Exit Sub
Err_ExpandCategory:
    Release
    'Err.Raise Description:=Err.Description, Number:=Err.Number, Source:=constSource
End Sub

Private Sub SetState(Row As Integer, Col As Integer, bExpanded As Boolean)
    On Error GoTo Err_SetState
    Const constSource As String = m_constClassName & ".SetState"
    
    Dim Cell As Integer
    
    With fGrid
        .Redraw = False
        Cell = Cell_Save
        .Row = Row
        .Col = Col
        .CellPictureAlignment = flexAlignCenterCenter
        If bExpanded = False Then
            Set .CellPicture = StdImages.ListImages("plus").Picture
        Else
            Set .CellPicture = StdImages.ListImages("minus").Picture
        End If
        Cell_Restore Cell
        .Redraw = True
    End With
    
    Exit Sub
Err_SetState:
    'Err.Raise Description:=Err.Description, Number:=Err.Number, Source:=constSource
End Sub

Private Function IsWindowLess(objProp As Object) As Boolean
    On Error GoTo Err_IsWindowLess
    Const constSource As String = m_constClassName & ".IsWindowLess"

    Select Case objProp.ValueType
        Case 0, psFont To psCombo, psLongText To psDropDownCheckList, psObject, psDate, psBoolean, psCustom
            If objProp.ValueType = psBoolean And objProp.Format = "checkbox" Then
                IsWindowLess = True
            Else
                IsWindowLess = False
            End If
        Case Else
            IsWindowLess = True
    End Select
    Exit Function

    Exit Function
Err_IsWindowLess:
    IsWindowLess = True
End Function

Private Function GetNextVisibleRow() As Integer
    On Error GoTo Err_GetNextVisibleRow
    Const constSource As String = m_constClassName & ".GetNextVisibleRow"

    Dim Row As Integer
    Dim obj As Object
    
    ' get the next row
    Row = fGrid.Row + 1
    ' loop to get a property row
    Do While fGrid.RowHeight(Row) = 0 And Row < fGrid.Rows + 1
        Row = Row + 1
    Loop
    ' return property row
    If Row <= fGrid.Rows Then
        GetNextVisibleRow = Row
    Else
        GetNextVisibleRow = -1
    End If

    Exit Function
Err_GetNextVisibleRow:
    GetNextVisibleRow = -1
End Function

Private Function GetPreviousVisibleRow() As Integer
    On Error GoTo Err_GetPreviousVisibleRow
    Const constSource As String = m_constClassName & ".GetPreviousVisibleRow"

    Dim Row As Integer
    
    ' get previous row
    Row = fGrid.Row - 1
    ' loop for a property
    Do While fGrid.RowHeight(Row) = 0 And Row > -1
        Row = Row - 1
    Loop
    ' return new row
    GetPreviousVisibleRow = Row

    Exit Function
Err_GetPreviousVisibleRow:
    GetPreviousVisibleRow = -1
End Function

Friend Sub DoSort(RowStart As Integer, RowEnd As Integer, Optional Col As Integer, Optional SortMethod As Integer = 1)
    On Error GoTo Err_DoSort
    Const constSource As String = m_constClassName & ".DoSort"
    
    Dim Cell As Integer
    
    With fGrid
        .Redraw = False
        Cell = Cell_Save
        .Row = RowStart
        .Col = Col
        .RowSel = RowEnd
        .ColSel = Col
        .Sort = SortMethod
        Cell_Restore Cell
        .Redraw = True
    End With
end_sort:

    Exit Sub
Err_DoSort:
End Sub

Private Sub Hilite(ByVal Row As Integer)
    On Error GoTo Err_Hilite
    Const constSource As String = m_constClassName & ".Hilite"
    
    Dim tmpBackColor As OLE_COLOR
    Dim tmpForeColor As OLE_COLOR
    
    ' dehilite current row
    DeHilite
    ' save selected row
    m_SelectedRow = Row
    ' get the object associated with this row
    Set m_SelectedItem = GetRowObject(Row)
    ' nothing found then exit
    If m_SelectedItem Is Nothing Then Exit Sub
    ' activate selection
    m_SelectedItem.Selected = True
    With fGrid
        .Redraw = False
        .Row = Row                      ' set grid row
        .Col = colName                        ' set grid color col #2
        .CellBackColor = m_SelBackColor
        .CellForeColor = m_SelForeColor
        
        tmpBackColor = m_BackColor
        tmpForeColor = vbBlack
        If IsProperty(m_SelectedItem) Then
            If m_SelectedItem.ReadOnly = True Then
                tmpForeColor = m_SelectedItem.ForeColor
            End If
        End If
        .Col = colValue                        ' set grid color col #2
        .CellBackColor = tmpBackColor
        .CellForeColor = tmpForeColor
        ' save cell dimensions
        StoreCellPosition
        Cell_DrawPicture Row, colName, m_SelectedItem.Image
        .Redraw = True
    End With
    Exit Sub
Err_Hilite:
    fGrid.Redraw = True
    'Err.Raise Description:=Err.Description, Number:=Err.Number, Source:=constSource
End Sub

Private Sub DeHilite()
    On Error GoTo Err_DeHilite
    Const constSource As String = m_constClassName & ".DeHilite"

    Dim obj As Object
    Dim tmpBackColor As OLE_COLOR
    Dim tmpForeColor As OLE_COLOR
    
    ' finds the row associate with the selected object
    Set obj = GetRowObject(m_SelectedRow)
    ' row not found then exit
    If obj Is Nothing Then Exit Sub
    With fGrid
        .Redraw = False
        .Row = m_SelectedRow            ' set grid row
        .Col = colName                        ' set grid col
        .ColSel = colValue
        obj.Selected = False
        GetObjectColors obj, tmpBackColor, tmpForeColor
        .CellBackColor = tmpBackColor
        .CellForeColor = tmpForeColor
        Cell_DrawPicture m_SelectedRow, colName, obj.Image
        .Redraw = True
    End With
    Exit Sub

Err_DeHilite:
    fGrid.Redraw = True
    'Err.Raise Description:=Err.Description, Number:=Err.Number, Source:=constSource
End Sub

Friend Function FindGridRow(obj As Object) As Integer
    On Error GoTo Err_FindGridRow
    Const constSource As String = m_constClassName & ".FindGridRow"

    Dim i As Integer
    Dim Ptr As Long
    
    Ptr = obj.Handle
    For i = 0 To fGrid.Rows - 1
        If fGrid.RowData(i) = Ptr Then
            FindGridRow = i
            Exit Function
        End If
    Next
    FindGridRow = -1
    Exit Function

Err_FindGridRow:
    FindGridRow = -1
End Function

Private Sub Cell_DrawPicture(Row As Integer, Col As Integer, Image As Variant)
    On Error GoTo Err_Cell_DrawPicture
    Const constSource As String = m_constClassName & ".Cell_DrawPicture"
    
    Dim BkColor As OLE_COLOR
    Dim obj As Object
    
    Set obj = GetRowObject(Row)
    If obj Is Nothing Then Exit Sub
    GetObjectColors obj, BkColor
    Cell_DrawPictureEx Row, Col, Image, m_hIml, BkColor
    Exit Sub

Err_Cell_DrawPicture:
    'Err.Raise Description:=Err.Description, Number:=Err.Number, Source:=constSource
End Sub

Private Sub Cell_ClearPicture(Row As Integer, Col As Integer)
    On Error GoTo Err_Cell_ClearPicture
    Const constSource As String = m_constClassName & ".Cell_ClearPicture"
    
    Dim obj As Object
    Dim tmpBackColor As OLE_COLOR
    Dim Cell As Integer
        
    Set obj = GetRowObject(Row)
    If obj Is Nothing Then Exit Sub
    With fGrid
        .Redraw = False
        Cell = Cell_Save
        .Row = Row
        .Col = Col
        Set .CellPicture = Nothing
        Cell_Restore Cell
        .Redraw = True
    End With

    Exit Sub
Err_Cell_ClearPicture:
    'Err.Raise Description:=Err.Description, Number:=Err.Number, Source:=constSource
End Sub

Private Function IsProperty(ByVal obj As Object) As Boolean
    IsProperty = TypeName(obj) = "TProperty"
End Function

Private Function GetDisplayString(objProp As TProperty) As String
    Dim strFormat As String
    Dim bUseDefault As Boolean
    Dim strDisplayStr As String
    
    strFormat = objProp.Format
    If (objProp.ValueType = psCustom) Or (strFormat = "CustomDisplay") Then
        bUseDefault = False
        RaiseEvent GetDisplayString(objProp, strDisplayStr, bUseDefault)
    Else
        bUseDefault = True
    End If
    If bUseDefault = True Then
        strDisplayStr = GetDefaultDisplayString(objProp)
    End If
    If objProp.Format = "Password" Then
        strDisplayStr = String(Len(strDisplayStr), "*")
    End If
    GetDisplayString = strDisplayStr
End Function

Private Function GetDefaultDisplayString(objProp As TProperty) As String
    On Error GoTo Err_GetDefaultDisplayString
    Const constSource As String = m_constClassName & ".GetDefaultDisplayString"

    Dim strDisplayStr As String
    Dim i As Integer
    Dim strTemp As String
    Dim lsValue As TListValue
    
    Select Case objProp.ValueType
        Case psDropDownCheckList
            strDisplayStr = "(List)"
        Case psFont
            If IsObject(objProp.Value) Then
                If Not objProp.Value Is Nothing Then
                    If objProp.Format <> "" Then
                        strDisplayStr = FormatFont(objProp.Value, objProp.Format)
                    Else
                        strDisplayStr = objProp.Value.Name
                    End If
                Else
                    strDisplayStr = "(None)"
                End If
            End If
        Case psObject
            strDisplayStr = "(Object)"
        Case psLongText
            strDisplayStr = "(Text)"
        Case psPicture
            strDisplayStr = "(Picture)"
        Case psTime
            strDisplayStr = Format(objProp.Value, "hh:mm:ss")
        Case psColor
            If objProp.Format <> "" Then
                strDisplayStr = FormatColor(objProp.Value, objProp.Format)
            Else
                strDisplayStr = objProp.Value
            End If
        
        Case psCombo
            For Each lsValue In objProp.ListValues
                If objProp.Value = lsValue.Value Then
                    strDisplayStr = lsValue.Caption
                    GoTo Exit_GetDefaultDisplayString
                End If
            Next
            ' not found then set the default value
            strDisplayStr = objProp.Value
        
        Case psDropDownList
            
            For i = 1 To objProp.ListValues.Count
                Set lsValue = objProp.ListValues(i)
                If lsValue.Value = objProp.Value Then
                    strDisplayStr = lsValue.Caption
                    GoTo Exit_GetDefaultDisplayString
                End If
            Next
            
        Case Else
            If IsArray(objProp.Value) Then
                strDisplayStr = "(Array)"
            Else
                If objProp.Format <> "checkbox" And objProp.Format <> "Password" And objProp.Format <> "" And objProp.Format <> "CustomDisplay" Then
                    strDisplayStr = Format(objProp.Value, objProp.Format)
                Else
                    If objProp.ListValues.Count > 0 Then
                        For i = 1 To objProp.ListValues.Count
                            If objProp.Value = objProp.ListValues(i).Value Then
                                strDisplayStr = objProp.ListValues(i).Caption
                                GoTo Exit_GetDefaultDisplayString
                            End If
                        Next
                    End If
                    strDisplayStr = objProp.Value
                End If
            End If
    End Select

Exit_GetDefaultDisplayString:
    GetDefaultDisplayString = strDisplayStr

    Exit Function
Err_GetDefaultDisplayString:
    'Err.Raise Description:=Err.Description, Number:=Err.Number, Source:=constSource
End Function

Private Sub ToggleCategoryState()
    If m_SelectedItem.Expanded = True Then
        CollapseCategory m_SelectedItem
    Else
        ExpandCategory m_SelectedItem
    End If
End Sub

Private Function Cell_Save() As Integer
    Cell_Save = MakeWord(fGrid.Row, fGrid.Col)
End Function

Private Function Cell_Restore(Info As Integer)
    On Error Resume Next
    fGrid.Row = HiByte(Info)
    fGrid.Col = LoByte(Info)
End Function

Private Function IsIncremental(Prop As TProperty) As Boolean
    Dim MinValue As Variant
    Dim MaxValue As Variant
    
    Prop.GetRange MinValue, MaxValue
    IsIncremental = _
       (Not IsEmpty(MinValue) Or _
       Not IsEmpty(MaxValue) Or _
       Prop.UpDownIncrement > 0) And _
       (IsWindowLess(Prop) And _
       Prop.ValueType <> psString)
End Function

Private Sub StoreCellPosition()
    On Error Resume Next
    Dim edgeX As Single
    Dim edgeY As Single
    Dim buttonEdgeX As Single
    
    If BorderStyle = psBorderSingle Then
        edgeX = 1 * Screen.TwipsPerPixelX
        edgeY = 0 * Screen.TwipsPerPixelY
        buttonEdgeX = 2 * Screen.TwipsPerPixelX
    Else
        edgeX = 2 * Screen.TwipsPerPixelX
        edgeY = 1 * Screen.TwipsPerPixelY
        buttonEdgeX = 0 * Screen.TwipsPerPixelX
    End If
    With fGrid
        ' set column to the value column
        .Col = colValue
        ' get cell rect
        rc.Left = .CellLeft + edgeX
        rc.Top = .CellTop + edgeY + ToolbarHeight
        rc.Height = .CellHeight
        rc.Width = .CellWidth
        ' button properties
        rc.ButtonTop = rc.Top - Screen.TwipsPerPixelY
        rc.ButtonWidth = GetSystemMetrics(SM_CXVSCROLL) * Screen.TwipsPerPixelX
        rc.ButtonLeft = (rc.Left + rc.Width) - (rc.ButtonWidth) - buttonEdgeX
        rc.ButtonHeight = rc.Height
        ' get browse window initial rect
        rc.WindowLeft = (Extender.Left + .CellLeft) - buttonEdgeX
        rc.WindowTop = Extender.Top + .CellTop + .CellHeight + edgeY + ToolbarHeight
        rc.WindowWidth = rc.Width
        rc.InterfaceLeft = rc.Left + (UserControl.TextWidth(Space(m_lPadding)))
    End With
End Sub

Private Sub Grid_ShowCategories()
    On Error GoTo Err_Grid_ShowCategories
    Const constSource As String = m_constClassName & ".Grid_ShowCategories"

    Dim Row As Integer
    Dim Cell As Integer
    Dim objCat As TCategory
    Dim i As Integer
    Dim j As Integer
    Dim Prop As TProperty
    Dim objTemp As Object
    Dim obj As Object
    Dim Handle As Long
    
    StopFlicker hwnd
    ' get the object related with this row
    Set objTemp = GetRowObject(m_SelectedRow)
    ' hide all the controls
    HideControls
    ' save cell position
    Cell = Cell_Save
    ' if it is to disable categories...
    If m_ShowCategories = False Then
        ' set column #0 width to 0
        fGrid.ColWidth(colStatus) = 0
        If m_Categories.Count > 0 Then
            For i = 1 To m_Categories.Count
                Row = m_Categories(i).Row
                fGrid.RowHeight(Row) = 0
                ' clear minus/plus picture
                Cell_ClearPicture Row, colStatus
                For j = 1 To m_Categories(i).Properties.Count
                    Row = m_Categories(i).Properties(j).Row
                    fGrid.RowHeight(Row) = PropertyHeight(False)
                Next
            Next
            ' sort entire row count
            DoSort 0, fGrid.Rows - 1, colName, flexSortStringNoCaseAsending
        End If
    Else
        ' set column #0 width to 0
        fGrid.ColWidth(colStatus) = COL_WIDTH * Screen.TwipsPerPixelX
        For i = 1 To m_Categories.Count
            Row = m_Categories(i).Row
            fGrid.RowHeight(Row) = PropertyHeight(False)
            SetState Row, colStatus, m_Categories(i).Expanded
        Next
        ' sort entire row count
        DoSort 0, fGrid.Rows - 1, colSort, flexSortNumericAscending
    End If
    Grid_Reindex
    ' select the object row
    If Not objTemp Is Nothing Then
        m_SelectedRow = objTemp.Row
    Else
        If fGrid.Rows > 0 Then
            m_SelectedRow = 0
        End If
    End If
    ' restore position
    Cell_Restore Cell
    ' resize the grid
    Grid_Resize
    Release
    
    Exit Sub
Err_Grid_ShowCategories:
    Release
    'Err.Raise Description:="Unexpected Error: " & Err.Description, _
       Number:=Err.Number, _
       Source:=constSource
End Sub

Private Sub Grid_Reindex()
    Dim Row As Integer
    Dim obj As Object
    
    For Row = 0 To fGrid.Rows - 1
        Set obj = GetRowObject(Row)
        If Not obj Is Nothing Then
            obj.Row = Row
        End If
    Next
End Sub

Private Sub Grid_Index()
    Dim Handle As Long
    Dim Row As Integer
    Dim i As Integer
    Dim j As Integer
    
    If m_Categories.Count > 0 Then
        For i = 1 To m_Categories.Count
            Row = m_Categories(i).Row
            Handle = MakeDWord(m_Categories(i).Index, 0)
            fGrid.TextMatrix(Row, colSort) = Handle
            For j = 1 To m_Categories(i).Properties.Count
                Row = m_Categories(i).Properties(j).Row
                Handle = MakeDWord(m_Categories(i).Index, m_Categories(i).Properties(j).Index)
                fGrid.TextMatrix(Row, colSort) = Handle
            Next
        Next
    End If
End Sub

Private Sub SetControlFont(Ctl As Control)
    Ctl.FontName = m_Font.Name
    Ctl.FontSize = m_Font.Size
End Sub

Private Function FixTopPos(lHeight) As Long
    lHeight = CLng(lHeight)
    If rc.WindowTop + lHeight + 300 > UserControl.Extender.Parent.ScaleHeight Then
        FixTopPos = rc.WindowTop - (rc.Height + lHeight)
    Else
        FixTopPos = rc.WindowTop
    End If
End Function

Private Sub HideBrowseWnd()
    If Not m_BrowseWnd Is Nothing Then
        If m_BrowseWnd.Visible = True Then
            m_BrowseWnd.Visible = False
        End If
    End If
    m_bBrowseMode = False
End Sub

Private Sub DrawCheckBox(Row As Integer, objProp As TProperty)
    Dim Image As String
    Dim BkColor As OLE_COLOR
    
    If objProp.Value = True Then
        Image = "check_on"
    Else
        Image = "check_off"
    End If
    If objProp.Selected Then
        BkColor = vbWhite
    Else
        If objProp.BackColor = CLR_INVALID Then
            BkColor = m_BackColor
        Else
            BkColor = objProp.BackColor
        End If
    End If
    Cell_DrawPictureEx Row, colValue, Image, m_hImlStd, BkColor
End Sub

Private Sub DrawColorBox(Row As Integer, objProp As TProperty)
    Dim Image As String
    Dim BkColor As OLE_COLOR
    
    Image = "frame"
    BkColor = objProp.Value
    Cell_DrawPictureEx Row, colValue, StdImages.ListImages(Image).Index, m_hImlStd, BkColor
End Sub

Private Sub Cell_DrawPictureEx(ByVal Row As Integer, ByVal Col As Integer, ByVal Image As Variant, hIml As Long, Optional BkColor As OLE_COLOR = 0)
    On Error GoTo Err_Cell_DrawPictureEx
    Const constSource As String = m_constClassName & ".Cell_DrawPictureEx"
    
    Dim obj As Object
    
    If fGrid.RowHeight(Row) = 0 Or hIml = 0 Then Exit Sub
    With fGrid
        .Redraw = False
        .Row = Row
        .Col = Col
        .CellPictureAlignment = flexAlignLeftCenter
        If BkColor <> -1 Then
            Image_List(hIml).BackColor = BkColor
        End If
        On Error Resume Next
        Set .CellPicture = Image_List(hIml).Overlay(Image, Image)
        .Redraw = True
    End With
    
    Exit Sub
Err_Cell_DrawPictureEx:
    ''Err.Raise Description:=Err.Description, Number:=Err.Number, Source:=constSource
End Sub

Private Sub RecalcPadding()
    Dim Twips As Long
    Dim w As Single
    
    If Not m_Font Is Nothing Then
        Twips = 17 * Screen.TwipsPerPixelX
        Set UserControl.Font = m_Font
        w = Twips / UserControl.TextWidth(" ")
        m_lPadding = w
    End If
    Grid_Resize
End Sub

Private Function HasGraphicInterface(objProp As Object) As Boolean
    If objProp Is Nothing Or IsProperty(objProp) = False Then
        HasGraphicInterface = False
        Exit Function
    End If
    HasGraphicInterface = (objProp.ValueType = psBoolean And objProp.Format = "checkbox") Or objProp.ValueType = psColor
End Function

Private Sub DrawGraphicInterface(objProp As TProperty, Row As Integer)
    If (objProp.ValueType = psBoolean And objProp.Format = "checkbox") Then
        DrawCheckBox Row, objProp
    ElseIf objProp.ValueType = psColor Then
        DrawColorBox Row, objProp
    End If
End Sub

Private Sub SelectText()
    On Error Resume Next
    If AutoSelect Then
        txtBox.SetFocus
        txtBox.SelStart = 0
        txtBox.SelLength = Len(txtBox.Text)
    End If
End Sub

Public Sub LoadFromFile(ByVal Filename As String, ByVal Section As String)
    On Error GoTo Err_LoadFromFile
    Const constSource As String = m_constClassName & ".LoadFromFile"

    'StopFlicker hwnd
    Dim Col As Collection
    Set Col = EnumSections(Filename, Section)
    If Col Is Nothing Then
        'Release
        Exit Sub
    End If
    On Error Resume Next
    Set Font = ReadProperty(Col("Font"), Ambient.Font)
    Set CatFont = ReadProperty(Col("CatFont"), Ambient.Font)
    AllowEmptyValues = ReadProperty(Col("AllowEmptyValues"), m_def_AllowEmptyValues)
    ExpandableCategories = ReadProperty(Col("ExpandableCategories"), m_def_ExpandableCategories)
    NameWidth = ReadProperty(Col("NameWidth"), m_def_NameWidth)
    RequiresEnter = ReadProperty(Col("RequiresEnter"), m_def_RequiresEnter)
    ShowToolTips = ReadProperty(Col("ShowToolTips"), m_def_ShowToolTips)
    CatBackColor = ReadProperty(Col("CatBackColor"), m_def_CatBackColor)
    CatForeColor = ReadProperty(Col("CatForeColor"), m_def_CatForeColor)
    SelBackColor = ReadProperty(Col("SelBackColor"), m_def_SelBackColor)
    SelForeColor = ReadProperty(Col("SelForeColor"), m_def_SelForeColor)
    BackColor = ReadProperty(Col("BackColor"), m_def_BackColor)
    ForeColor = ReadProperty(Col("ForeColor"), m_def_ForeColor)
    GridColor = ReadProperty(Col("GridColor"), m_def_GridColor)
    BorderStyle = ReadProperty(Col("BorderStyle"), 1)
    Appearance = ReadProperty(Col("Appearance"), 1)
    'Release
    Exit Sub
Err_LoadFromFile:
    'Release
    'Err.Raise Description:="Unexpected Error: " & Err.Description, Number:=Err.Number, Source:=constSource
End Sub

Private Function ReadProperty(Prop As Variant, DefProp As Variant) As Variant
    Dim objTemp As Object
    If IsVarEmpty(Prop) Then
        If IsObject(DefProp) Then
            Set ReadProperty = DefProp
        Else
            ReadProperty = DefProp
        End If
    Else
        If IsObject(DefProp) Then
            If TypeOf DefProp Is StdFont Then
                Set objTemp = FontFromStr(Prop)
                Set ReadProperty = objTemp
            Else
                Set ReadProperty = Prop
            End If
        Else
            ReadProperty = Prop
        End If
    End If
End Function

Public Sub SaveToFile(ByVal Filename As String, ByVal Section As String)
    On Error GoTo Err_SaveFile
    Const constSource As String = m_constClassName & ".SaveFile"

    Dim i As Integer
    Dim j As Integer
    Dim hFile As Integer
    
    'StopFlicker hwnd
    m_strText = "[" & Section & "]" & vbCrLf
    Call WriteProperty("Font", StrFromFont(m_Font))
    Call WriteProperty("CatFont", StrFromFont(m_CatFont))
    Call WriteProperty("AllowEmptyValues", m_AllowEmptyValues)
    Call WriteProperty("ExpandableCategories", m_ExpandableCategories)
    Call WriteProperty("NameWidth", m_NameWidth)
    Call WriteProperty("RequiresEnter", m_RequiresEnter)
    Call WriteProperty("ShowCategories", m_ShowCategories)
    Call WriteProperty("ShowToolTips", m_ShowToolTips)
    Call WriteProperty("CatBackColor", m_CatBackColor)
    Call WriteProperty("CatForeColor", m_CatForeColor)
    Call WriteProperty("SelBackColor", m_SelBackColor)
    Call WriteProperty("SelForeColor", m_SelForeColor)
    Call WriteProperty("BackColor", m_BackColor)
    Call WriteProperty("ForeColor", m_ForeColor)
    Call WriteProperty("GridColor", m_GridColor)
    Call WriteProperty("BorderStyle", BorderStyle)
    Call WriteProperty("Appearance", Appearance)
    Call WriteProperty("ExpandedImage", m_ExpandedImage)
    Call WriteProperty("CollapsedImage", m_CollapsedImage)
    hFile = FreeFile
    Open Filename For Output As #hFile
    Print #hFile, m_strText
    Close #hFile
    'Release

    Exit Sub
Err_SaveFile:
    Close #hFile
    'Release
    'Err.Raise Description:=Err.Description, _
       Number:=Err.Number, _
       Source:=constSource
End Sub

Private Sub WriteProperty(ByVal Prop As String, ByVal Value As Variant)
    m_strText = m_strText & Prop & "=" & Value & vbCrLf
End Sub

Private Function DefaultHeight() As Long
    ' return height in pixels
    DefaultHeight = 18 * Screen.TwipsPerPixelY
End Function

Private Function PropertyHeight(bType As Boolean) As Long
    ' return height in pixels
    
    If bType Then
        Set picText.Font = Me.Font
    Else
        Set picText.Font = Me.CatFont
    End If
       
    PropertyHeight = picText.TextHeight(" ") + (4 * Screen.TwipsPerPixelY)
    
End Function

'<CSCM>
'--------------------------------------------------------------------------------
' Project      :       PropertySheet
' Procedure    :       IsReadOnly
' Description  :       Returns true if the given property is readonly or has
'                      readonly effect
' Created by   :       Project Administrator
' Machine      :       ZEUS
' Date-Time    :       26/3/2004-09:54:44
'
' Parameters   :       Prop (TProperty)
' Return Values:
'--------------------------------------------------------------------------------
'</CSCM>
Private Function IsReadOnly(Prop As TProperty) As Boolean
    With Prop
        IsReadOnly = _
           .ValueType = psLongText Or _
           .ValueType = psPicture Or _
           .ValueType = psDropDownCheckList Or _
           .ValueType = psDropDownList Or _
           .ValueType = psBoolean Or _
           .ValueType = psFont Or _
           .ReadOnly Or _
           IsArray(.Value)
    End With
End Function

'<CSCM>
'--------------------------------------------------------------------------------
' Project      :       PropertySheet
' Procedure    :       IsBrowsable
' Description  :       Returns true if property has browsable value
' Created by   :       Project Administrator
' Machine      :       ZEUS
' Date-Time    :       26/3/2004-09:55:32
'
' Parameters   :       Prop (TProperty)
' Return Values:
'--------------------------------------------------------------------------------
'</CSCM>
Private Function IsBrowsable(Prop As TProperty) As Boolean
    With m_SelectedItem
        IsBrowsable = _
           .ValueType = psCustom Or _
           .ValueType = psColor Or _
           .ValueType = psFile Or _
           .ValueType = psFolder Or _
           .ValueType = psPicture Or _
           .ValueType = psFont Or _
           .ValueType = psDate
    End With
End Function

Private Function Pad(ByVal Text As String) As String
    If m_hIml = 0 Then
        Pad = Text
    Else
        Pad = Space(m_lPadding) & Text
    End If
End Function

Private Function AvgTextWidth() As Single
    Dim avgWidth As Single
    ' Get the average character width of the current list box Font
    ' (in pixels) using the form's TextWidth width method.
    avgWidth = UserControl.TextWidth("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
    avgWidth = avgWidth / 52
    ' Set the white space you want between columns.
    AvgTextWidth = avgWidth
End Function

Private Function EvaluateTextWidth(ByVal s As String) As Single
    Dim avgWidth As Single
    
    avgWidth = AvgTextWidth
    EvaluateTextWidth = Len(Trim(s)) * avgWidth + (m_lPadding * UserControl.TextWidth(" "))
End Function

Private Function Cell_NameWidth() As Single
    Dim tW As Single
    Dim nw As Single
    Dim Index As Integer
    
    nw = 0
    ' calculate the automatic name width
    For Index = 1 To m_Properties.Count
        tW = EvaluateTextWidth(Properties(Index).Caption)
        If tW > nw Then
            nw = tW
        End If
    Next Index
    Cell_NameWidth = nw
End Function

Private Sub Grid_Paint()
    On Error GoTo Err_Grid_Paint
    Const constSource As String = m_constClassName & ".Grid_Paint"
    
    Dim Cat As Integer
    Dim Prop As Integer
    Dim RowStart As Integer
    Dim RowSel As Integer
    Dim Cell As Integer
    Dim Row As Integer
    Dim obj As Object
   
    Dim propVal As TProperty
    Dim catVal As TCategory
    Dim i As Integer
    Dim j As Integer
    Dim propHeight As Long
    Dim oldRow As Integer

    StopFlicker fGrid.hwnd
    RecalcPadding
    ' check for Categories. If no categories
    ' is found then clear grid and exit
    If m_Categories.Count = 0 Then
        Grid_Clear
'<Added by: Project Administrator at: 31/3/2004-22:56:01 on machine: ZEUS>
        Release
'</Added by: Project Administrator at: 31/3/2004-22:56:01 on machine: ZEUS>
        Exit Sub
    End If
    ' hide all visible control
    HideControls
    
    propHeight = PropertyHeight(True)
    oldRow = fGrid.Row
    'adjust each row height
    For j = 1 To m_Categories.Count
        Set catVal = m_Categories(j)
        Row_Category catVal
        If m_Categories(j).Expanded = True Then
            For i = 1 To m_Categories(j).Properties.Count
                Set propVal = m_Categories(j).Properties(i)
                Row_Property propVal, True
                fGrid.RowHeight(propVal.Row) = propHeight
            Next i
        End If
    Next j
    fGrid.Row = oldRow
    ' update category order
    Grid_ShowCategories
    ' draw grid cells
    With fGrid
        ' to avoid flickering
        .Redraw = False
        ' save current cell
        Cell = Cell_Save
        .GridColor = m_GridColor
        .GridColorFixed = m_BackColor
        .BackColorFixed = m_BackColor
        .BackColorSel = m_SelBackColor
        .BackColorBkg = m_BackColor
        .BackColorUnpopulated = m_BackColor
        .BackColor = m_BackColor
        .ForeColorSel = m_SelForeColor
        .ForeColor = m_ForeColor
        Set .Font = m_Font
        ' restore cell position
        Cell_Restore Cell
        .Redraw = True
    End With

    Exit Sub
    Release
Err_Grid_Paint:
    Release
    'Err.Raise Description:=Err.Description, Number:=Err.Number, Source:=constSource
End Sub

Friend Sub TriggerEvent(ByVal RaisedEvent As String, ParamArray aParams())
    Select Case RaisedEvent
        Case "CaptionChanged"
            Cell_CaptionChanged aParams(0), aParams(1), aParams(2)
        Case "ValueChanged"
            Cell_ValueChanged aParams(0), aParams(1)
        Case "AddNewCategory"
            AddNewCategory aParams(0)
        Case "AddNewProperty"
            AddNewProperty aParams(0), aParams(1)
        Case "SelectedChanged"
            'DeHilite
            'Hilite aParams(1)
        Case "ForeColorChanged"
            Cell_ChangeForeColor aParams(1), aParams(2)
        Case "BackColorChanged"
            Cell_ChangeBackColor aParams(1), aParams(2)
        Case "Clear"
            Set m_Properties = Nothing
            Set m_Properties = New Collection
    End Select
End Sub

Private Sub Cell_ChangeForeColor(ByVal Row As Integer, ByVal New_Color As OLE_COLOR)
    Dim Cell As Integer
    Cell = Cell_Save
    With fGrid
        .Row = Row
        .Col = colName
        .ColSel = colValue
        If m_SelectedRow <> Row Then
            fGrid.CellForeColor = New_Color
        Else
            fGrid.CellForeColor = m_SelForeColor
        End If
    End With
    Cell_Restore Cell
End Sub

Private Sub Cell_ChangeBackColor(ByVal Row As Integer, ByVal New_Color As OLE_COLOR)
    Dim Cell As Integer
    Cell = Cell_Save
    With fGrid
        .Row = Row
        .Col = colName
        .ColSel = colValue
        If m_SelectedRow <> Row Then
            fGrid.CellBackColor = New_Color
        Else
            fGrid.CellBackColor = m_SelBackColor
        End If
    End With
    Cell_Restore Cell
End Sub

Private Sub Cell_ValueChanged(ByVal PropObj As TProperty, ByVal Row As Integer)
    On Error GoTo Err_ValueChanged
    Const constSource As String = m_constClassName & ".ValueChanged"

    Dim Cell As Integer
    Dim tmpBackColor As OLE_COLOR
    Dim tmpForeColor As OLE_COLOR
    Dim strValue As String
    
    ' save cell pos
    Cell = Cell_Save
    ' check for the value type
    If (PropObj.ValueType <> psDropDownCheckList) Then
        HideBrowseWnd
        txtBox.Visible = False
        If UpDown.Visible = False And cmdBrowse.Visible = False Then
            HideControls
        End If
    End If
    ' configure back color
    GetObjectColors PropObj, tmpBackColor, tmpForeColor
    If PropObj.ReadOnly Then
        tmpForeColor = PropObj.ForeColor
    End If
    strValue = GetDisplayString(PropObj)
    With fGrid
        .Row = Row
        .Col = colValue
        If PropObj.BackColor <> CLR_INVALID Then
            .CellBackColor = tmpBackColor
        End If
        If PropObj.ForeColor <> CLR_INVALID Then
            .CellForeColor = tmpForeColor
        End If
        .CellAlignment = flexAlignLeftCenter
        Set .CellPicture = Nothing
        If HasGraphicInterface(PropObj) Then
            DrawGraphicInterface PropObj, Row
            strValue = Space(m_lPadding) & strValue
        End If
        .Text = strValue
    End With
    ' restore cell properties
    Cell_Restore Cell
    Exit Sub
Err_ValueChanged:
    'Err.Raise Description:="Unexpected Error: " & Err.Description, Number:=Err.Number, Source:=constSource
End Sub

Private Sub Cell_CaptionChanged(ByVal PropObj As TProperty, ByVal Row As Integer, ByVal NewCaption As String)
    On Error GoTo Err_ValueChanged
    Const constSource As String = m_constClassName & ".ValueChanged"

    Dim Cell As Integer
    Dim tmpBackColor As OLE_COLOR
    Dim tmpForeColor As OLE_COLOR

    ' disable drawing
    fGrid.Redraw = False
    ' save cell pos
    Cell = Cell_Save
    ' check for the value type
    If (PropObj.ValueType <> psDropDownCheckList) Then
        HideBrowseWnd
        txtBox.Visible = False
        If UpDown.Visible = False And cmdBrowse.Visible = False Then
            HideControls
        End If
    End If
    ' configure back color
    GetObjectColors PropObj, tmpBackColor, tmpForeColor
    With fGrid
        .Row = Row
        .Col = colName
        .CellBackColor = tmpBackColor
        .CellForeColor = tmpForeColor
        .CellAlignment = flexAlignLeftCenter
        If m_hIml <> 0 Then
            If PropObj.Image <> -1 Then
                Cell_DrawPicture Row, colName, PropObj.Image
            End If
        End If
        .Text = Pad(PropObj.Caption)
    End With
    ' restore cell properties
    Cell_Restore Cell
    ' enable drawing
    fGrid.Redraw = True

    Exit Sub
Err_ValueChanged:
    fGrid.Redraw = True
    'Err.Raise Description:="Unexpected Error: " & Err.Description, Number:=Err.Number, Source:=constSource
End Sub

Private Sub UpdateStatus()
    Dim strValue As String
    
    If Not IsObject(m_SelectedItem.Value) Then
        If IsArray(m_SelectedItem.Value) Then
            strValue = Join(m_SelectedItem.Value, ", ")
        Else
            strValue = m_SelectedItem.Value
        End If
        lblStatusHeadLine.Caption = m_SelectedItem.Caption
        lblStatusBody.Caption = m_SelectedItem.Description
    Else
        lblStatusHeadLine.Caption = m_SelectedItem.Caption
        lblStatusBody.Caption = m_SelectedItem.Description
    End If

End Sub

Private Function ToolbarHeight() As Long
    ToolbarHeight = 0
    
    If m_ShowToolbar Then
        ToolbarHeight = tbrSort.Height + (GAPY / 2)
    End If
End Function

Private Function DescriptionPanelHeight() As Long
    DescriptionPanelHeight = 0
    
'<Modified by: Project Administrator at 26/3/2004-10:19:39 on machine: ZEUS>
    'If picStatus.Visible Then
'</Modified by: Project Administrator at 26/3/2004-10:19:39 on machine: ZEUS>
    If m_ShowDescription Then
        DescriptionPanelHeight = picStatus.Height + (GAPY)
    End If
    
End Function

Private Sub RepositionOnClient(ctrl As Object, Optional ctrlTop As Single = -1)
    Dim lp As POINTAPI
    Dim ret As Long
    
    modApi32.SetFocus ctrl.hwnd

    'to avoid creation of TaskBar button
    ret = GetWindowLong(ctrl.hwnd, GWL_EXSTYLE)
    ret = ret Or WS_EX_TOOLWINDOW
    ret = SetWindowLong(ctrl.hwnd, GWL_EXSTYLE, ret)

    lp.x = ctrl.Left / Screen.TwipsPerPixelX
    
    If ctrlTop = -1 Then
        lp.y = ctrl.Top / Screen.TwipsPerPixelY
    Else
        lp.y = ctrlTop / Screen.TwipsPerPixelY
    End If
    
    ClientToScreen Extender.Parent.hwnd, lp
    
    SetParent ctrl.hwnd, 0
    
    ctrl.Left = lp.x * Screen.TwipsPerPixelX
    ctrl.Top = lp.y * Screen.TwipsPerPixelY
    
End Sub

'<Added by: Project Administrator at: 31/3/2004-22:54:31 on machine: ZEUS>
'<CSCM>
'--------------------------------------------------------------------------------
' Project      :       PropertySheet
' Procedure    :       Clear
' Description  :       Clear property sheet
' Created by   :       Project Administrator
' Machine      :       ZEUS
' Date-Time    :       31/3/2004-22:55:19
'
' Parameters   :
' Return Values:
'--------------------------------------------------------------------------------
'</CSCM>
Public Sub Clear()
    m_Categories.Clear
    Grid_Paint
    RaiseEvent OnClear
End Sub

'<CSCM>
'--------------------------------------------------------------------------------
' Project      :       PropertySheet
' Procedure    :       Exists
' Description  :       Returns true if the given properti caption exists
' Created by   :       Project Administrator
' Machine      :       ZEUS
' Date-Time    :       31/3/2004-22:54:45
'
' Parameters   :       Caption (String)
' Return Values:       True if property exists
'--------------------------------------------------------------------------------
'</CSCM>
Public Function Exists(ByVal Caption As String) As Boolean
    Dim item As TProperty
    
    Set item = Properties(Caption)
    Exists = Not item Is Nothing
End Function
'</Added by: Project Administrator at: 31/3/2004-22:54:31 on machine: ZEUS>

'<Modified by: Project Administrator at 31/3/2004-22:54:36 on machine: ZEUS>
Private Sub StopFlicker(ByVal lHwnd As Long)
    Dim lRet As Long
    
    If m_Redraw = False Then Exit Sub
    ' object will not flicker - just be blank
    lRet = LockWindowUpdate(lHwnd)
End Sub

Private Sub Release()
    Dim lRet As Long
    
    If m_Redraw = False Then Exit Sub
    lRet = LockWindowUpdate(0)
End Sub
'</Modified by: Project Administrator at 31/3/2004-22:54:36 on machine: ZEUS>
'-- end code
