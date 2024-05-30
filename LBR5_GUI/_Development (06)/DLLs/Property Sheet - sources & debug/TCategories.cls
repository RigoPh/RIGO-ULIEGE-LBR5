VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "TCategories"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Collection" ,"TCategory"
Attribute VB_Ext_KEY = "Member0" ,"TCategory"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
' *******************************************************
' Class        : TCategories.cls
' Written By   : Marclei V Silva (MVS)
' Programmer   : Marclei V Silva (MVS) [Spnorte Consultoria de Informática]
' Date Writen  : 06/16/2000 -- 08:41:37
' Inputs       : N/A
' Outputs      : N/A
' Description  : Collection of category
'              :
'              :
' *******************************************************
Option Explicit
Option Compare Text

' local variable to hold collection
Private mCol As Collection
Private m_ParentPtr As Long

Public Sub Init(Object As TPropertySheet)
    m_ParentPtr = PtrFromObject(Object)
End Sub

Public Property Get Parent() As TPropertySheet
    Set Parent = ObjectFromPtr(m_ParentPtr)
End Property

Public Function Add( _
    Caption As String, _
    Optional Image As Variant, _
    Optional TooltipText As String, _
    Optional Expanded As Boolean = True _
    ) As TCategory
    
    ' create a new object
    Dim objNewMember As TCategory
        
    Set objNewMember = New TCategory
    ' set the properties passed into the method
    objNewMember.Caption = Caption
    objNewMember.Expanded = Expanded
    objNewMember.Index = mCol.Count + 1
    objNewMember.Key = Caption
    objNewMember.Image = Image
    objNewMember.TooltipText = TooltipText
    objNewMember.BackColor = CLR_INVALID
    objNewMember.ForeColor = CLR_INVALID
    objNewMember.MaskColor = -1
    objNewMember.Selected = False
    objNewMember.BorderStyle = psBorderSingle
    objNewMember.SelBorderStyle = psBorderSingle
    ' add object to the collection
    If Len(Caption) = 0 Then
        mCol.Add objNewMember
    Else
        mCol.Add objNewMember, Caption
    End If
    ' add new category
    Parent.TriggerEvent "AddNewCategory", objNewMember
    ' initialize object
    objNewMember.Init Parent
    ' return the object created
    Set Add = objNewMember
    Set objNewMember = Nothing
End Function

Public Property Get item(Index As Variant) As TCategory
Attribute item.VB_UserMemId = 0
    Set item = mCol(Index)
End Property

Public Property Get Count() As Long
    Count = mCol.Count
End Property

Public Sub Remove(Index As Variant)
    mCol.Remove Index
End Sub

Public Property Get NewEnum() As IUnknown
Attribute NewEnum.VB_UserMemId = -4
Attribute NewEnum.VB_MemberFlags = "40"
    'this property allows you to enumerate
    'this collection with the For...Each syntax
    Set NewEnum = mCol.[_NewEnum]
End Property

Private Sub Class_Initialize()
    'creates the collection when this class is created
    Set mCol = New Collection
End Sub

Private Sub Class_Terminate()
    'destroys collection when this class is terminated
    Set mCol = Nothing
End Sub

' ******************************************************************************
' Routine       : (Sub) Clear
' Created by    : Marclei V Silva
' Company Name  : Spnorte Consultoria
' Machine       : ZEUS
' Date-Time     : 17/06/2000 - 9:33:23
' Inputs        : N/A
' Outputs       : N/A
' Modifications : N/A
' Description   : Clears the Category collection
' ******************************************************************************
Public Sub Clear()
    Set mCol = Nothing
    Set mCol = New Collection
    Parent.TriggerEvent "Clear"
End Sub
