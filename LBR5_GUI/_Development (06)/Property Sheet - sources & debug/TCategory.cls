VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "TCategory"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
' *******************************************************
' Class        : TCategory.cls
' Written By   : Marclei V Silva (MVS)
' Programmer   : Marclei V Silva (MVS) [Spnorte Consultoria de Informática]
' Date Writen  : 06/16/2000 -- 08:41:37
' Inputs       : N/A
' Outputs      : N/A
' Description  : Describe categories for the PropertySheet
'              : Object.
'              :
' *******************************************************
Option Explicit
Option Compare Text

Private m_BackColor As OLE_COLOR
Private m_BorderStyle As psBorderStyle
Private m_Caption As String
Private m_Expanded As Boolean
Private m_ForeColor As OLE_COLOR
Private m_Index As Long
Private m_Key As String
Private m_MaskColor As OLE_COLOR
Private m_Image As Variant
Private m_Properties As TProperties
Attribute m_Properties.VB_VarHelpID = -1
Private m_SelBorderStyle As psBorderStyle
Private m_Selected As Boolean
Private m_TooltipText As String
Private m_ParentPtr As Long
Private m_Handle As Long
Private m_Row As Integer

Public Sub Init(ByVal Object As TPropertySheet)
    m_ParentPtr = PtrFromObject(Object)
End Sub

Public Property Get Parent() As TPropertySheet
    Set Parent = ObjectFromPtr(m_ParentPtr)
End Property

Public Property Get Handle() As Long
    Handle = m_Handle
End Property

Public Property Let Row(ByVal vData As Integer)
    m_Row = vData
End Property

Public Property Get Row() As Integer
    Row = m_Row
End Property

Public Property Let TooltipText(ByVal vData As String)
    m_TooltipText = vData
End Property

Public Property Get TooltipText() As String
    TooltipText = m_TooltipText
End Property

Public Property Let Selected(ByVal vData As Boolean)
    m_Selected = vData
End Property

Public Property Get Selected() As Boolean
    Selected = m_Selected
End Property

Public Property Let SelBorderStyle(ByVal vData As psBorderStyle)
    m_SelBorderStyle = vData
End Property

Public Property Get SelBorderStyle() As psBorderStyle
    SelBorderStyle = m_SelBorderStyle
End Property

Public Property Set Properties(ByVal vData As TProperties)
    Set m_Properties = vData
End Property

Public Property Get Properties() As TProperties
    Set Properties = m_Properties
End Property

Public Property Let Image(ByVal vData As Variant)
    m_Image = vData
End Property

Public Property Get Image() As Variant
    Image = m_Image
End Property

Public Property Let MaskColor(ByVal vData As OLE_COLOR)
    m_MaskColor = vData
End Property

Public Property Get MaskColor() As OLE_COLOR
    MaskColor = m_MaskColor
End Property

Public Property Let Key(ByVal vData As String)
    m_Key = vData
End Property

Public Property Get Key() As String
    Key = m_Key
End Property

Public Property Let Index(ByVal vData As Long)
    m_Index = vData
End Property

Public Property Get Index() As Long
    Index = m_Index
End Property

Public Property Let ForeColor(ByVal vData As OLE_COLOR)
    m_ForeColor = vData
End Property

Public Property Get ForeColor() As OLE_COLOR
    ForeColor = m_ForeColor
End Property

Public Property Let Expanded(ByVal vData As Boolean)
    m_Expanded = vData
End Property

Public Property Get Expanded() As Boolean
    Expanded = m_Expanded
End Property

Public Property Let Caption(ByVal vData As String)
    m_Caption = vData
End Property

Public Property Get Caption() As String
    Caption = m_Caption
End Property

Public Property Let BorderStyle(ByVal vData As psBorderStyle)
    m_BorderStyle = vData
End Property

Public Property Get BorderStyle() As psBorderStyle
    BorderStyle = m_BorderStyle
End Property

Public Property Let BackColor(ByVal vData As OLE_COLOR)
    m_BackColor = vData
End Property

Public Property Get BackColor() As OLE_COLOR
    BackColor = m_BackColor
End Property

Public Property Get Object() As TCategory
    Set Object = Me
End Property

Private Sub Class_Initialize()
    m_Handle = PtrFromObject(Me)
    Set Properties = New TProperties
    m_Properties.Init Me
End Sub

Private Sub Class_Terminate()
    Set m_Properties = Nothing
End Sub

Public Sub EnsureVisible()
End Sub
'-- end code
