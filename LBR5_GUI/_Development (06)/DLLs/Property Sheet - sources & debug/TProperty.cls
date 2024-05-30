VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "TProperty"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
' *******************************************************
' Class        : TProperty.cls
' Written By   : Marclei V Silva (MVS)
' Programmer   : Marclei V Silva (MVS) [Spnorte Consultoria de Informática]
' Date Writen  : 06/16/2000 -- 08:27:55
' Inputs       : N/A
' Outputs      : N/A
' Description  : Set editable properties for PropertySheet
'              :
'              :
' *******************************************************
Option Explicit

Private m_BackColor As OLE_COLOR
Private m_BorderStyle As psBorderStyle
Private m_Caption As String
Private m_Expanded As Boolean
Private m_ForeColor As OLE_COLOR
Private m_Index As Long
Private m_Key As String
Private m_MaskColor As OLE_COLOR
Private m_Image As Variant
Private m_Format As String
Private m_ListValues As TListValues
Private m_UpDownIncrement As Variant
Private m_ReadOnly As Boolean
Private m_Selected As Boolean
Private m_SelBorderStyle As psBorderStyle
Private m_TooltipText As String
Private m_Value As Variant
Private m_ValueType As psPropertyType
Private m_MinValue As Variant
Private m_MaxValue As Variant
Private m_ParentPtr As Long
Private m_Handle As Long
Private m_Row As Integer
Private m_Tag As Variant
Private m_Description As String

Public Sub Init(ByVal Object As TCategory)
    m_ParentPtr = PtrFromObject(Object)
End Sub

Public Property Get Parent() As TCategory
    Set Parent = ObjectFromPtr(m_ParentPtr)
End Property

Public Property Get Description() As String
    Description = m_Description
End Property

Public Property Let Description(ByVal Value As String)
    m_Description = Value
End Property

Public Property Get Tag() As Variant
    Tag = m_Tag
End Property

Public Property Let Tag(ByVal Value As Variant)
    m_Tag = Value
End Property

Public Property Let Row(ByVal vData As Integer)
    m_Row = vData
End Property

Public Property Get Row() As Integer
    Row = m_Row
End Property

Public Property Get Handle() As Long
    Handle = m_Handle
End Property

Public Property Get Object() As TProperty
    Set Object = Me
End Property

Public Sub SetRange(Optional ByVal MinValue As Variant, Optional ByVal MaxValue As Variant)
Attribute SetRange.VB_Description = "Sets the property maximun and minum values. This property is used only for numeric properties and pbString properties. pbString properties uses maxvalue to set the maximum string length"
    If IsMissing(MinValue) Then MinValue = Empty
    If IsMissing(MaxValue) Then MaxValue = Empty
    m_MinValue = MinValue
    m_MaxValue = MaxValue
    If m_UpDownIncrement = 0 Then
        m_UpDownIncrement = 1
    End If
End Sub

Public Sub GetRange(MinValue As Variant, MaxValue As Variant)
Attribute GetRange.VB_Description = "Returns the property minimun and maximun values"
    MinValue = m_MinValue
    MaxValue = m_MaxValue
End Sub

Public Property Let ValueType(ByVal vData As psPropertyType)
    m_ValueType = vData
End Property

Public Property Get ValueType() As psPropertyType
    ValueType = m_ValueType
End Property

Public Property Let Value(ByVal vData As Variant)
    m_Value = vData
    If m_ParentPtr <> 0 Then
        ' sends a message to parent to change the value
        Parent.Parent.TriggerEvent "ValueChanged", Object, m_Row, vData
    End If
End Property

Public Property Set Value(ByVal vData As Variant)
    Set m_Value = vData
    If m_ParentPtr <> 0 Then
        Parent.Parent.TriggerEvent "ValueChanged", Object, m_Row, vData
    End If
End Property

Public Property Get Value() As Variant
    If IsObject(m_Value) Then
        Set Value = m_Value
    Else
        Value = m_Value
    End If
End Property

Public Property Let TooltipText(ByVal vData As String)
    m_TooltipText = vData
End Property

Public Property Get TooltipText() As String
    TooltipText = m_TooltipText
End Property

Public Property Let SelBorderStyle(ByVal vData As psBorderStyle)
    m_SelBorderStyle = vData
End Property

Public Property Get SelBorderStyle() As psBorderStyle
    SelBorderStyle = m_SelBorderStyle
End Property

Public Property Let Selected(ByVal vData As Boolean)
    m_Selected = vData
    If m_ParentPtr <> 0 Then
        Parent.Parent.TriggerEvent "SelectedChanged", Object, m_Row
    End If
End Property

Public Property Get Selected() As Boolean
    Selected = m_Selected
End Property

Public Property Let ReadOnly(ByVal vData As Boolean)
    m_ReadOnly = vData
End Property

Public Property Get ReadOnly() As Boolean
    ReadOnly = m_ReadOnly
End Property

Public Property Let UpDownIncrement(ByVal vData As Variant)
    m_UpDownIncrement = vData
End Property

Public Property Get UpDownIncrement() As Variant
    UpDownIncrement = m_UpDownIncrement
End Property

Public Property Set ListValues(ByVal vData As TListValues)
    Set m_ListValues = vData
    If Not m_ListValues Is Nothing Then
        m_ListValues.Init Me
    End If
End Property

Public Property Get ListValues() As TListValues
    Set ListValues = m_ListValues
End Property

Public Property Let Format(ByVal vData As String)
    m_Format = vData
    If m_ParentPtr <> 0 Then
        Parent.Parent.TriggerEvent "ValueChanged", Object, m_Row, m_Value
    End If
End Property

Public Property Get Format() As String
    Format = m_Format
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
    If m_ParentPtr <> 0 Then
        Parent.Parent.TriggerEvent "ForeColorChanged", Object, m_Row, vData
    End If
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
    If m_ParentPtr <> 0 Then
        Parent.Parent.TriggerEvent "CaptionChanged", Object, m_Row, vData
    End If
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
    If m_ParentPtr <> 0 Then
        Parent.Parent.TriggerEvent "BackColorChanged", Object, m_Row, vData
    End If
End Property

Public Property Get BackColor() As OLE_COLOR
    BackColor = m_BackColor
End Property

Public Sub EnsureVisible()
End Sub

Private Sub Class_Initialize()
    m_Handle = PtrFromObject(Me)
    m_MinValue = Empty
    m_MaxValue = Empty
    m_UpDownIncrement = 0
    m_Image = -1
    m_Tag = ""
    Set ListValues = New TListValues
    m_ListValues.Init Me
End Sub

Private Sub Class_Terminate()
    m_Handle = 0
    Set ListValues = Nothing
End Sub
