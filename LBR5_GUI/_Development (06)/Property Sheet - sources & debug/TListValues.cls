VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "TListValues"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Collection" ,"TListValue"
Attribute VB_Ext_KEY = "Member0" ,"TListValue"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
' *******************************************************
' Class        : TListValues.cls
' Written By   : Marclei V Silva (MVS)
' Programmer   : Marclei V Silva (MVS) [Spnorte Consultoria de Informática]
' Date Writen  : 06/16/2000 -- 08:33:11
' Inputs       : N/A
' Outputs      : N/A
' Description  : Collection of ListValue
'              :
'              :
' *******************************************************
Option Explicit

' local variable to hold collection
Private mCol As Collection
Private m_Sorted As Boolean

Private m_ParentPtr As Long

Friend Sub Init(Object As TProperty)
    m_ParentPtr = PtrFromObject(Object)
    ' if there are itens set the we have to initializa it all
    If mCol.Count > 0 Then
        Dim Item As TListValue
        For Each Item In mCol
            Item.Init Object
        Next
    End If
End Sub

Public Property Get Parent() As TProperty
    Set Parent = ObjectFromPtr(m_ParentPtr)
End Property

Public Property Let Sorted(ByVal vData As Boolean)
    m_Sorted = vData
    If vData = True And mCol.Count > 0 Then
        Sort
    End If
End Property

Public Property Get Sorted() As Boolean
    Sorted = m_Sorted
End Property

Public Function Add( _
    Optional Value As Variant, _
    Optional Caption As String, _
    Optional Key As String) As TListValue
    
    ' create a new object
    Dim objNewMember As TListValue
    Dim idx As Integer
    Dim myKey As Variant
    
    Set objNewMember = New TListValue
    ' set the properties passed into the method
    If Len(Caption) = 0 Then
        Caption = CStr(Value)
    End If
    objNewMember.Caption = Caption
    If IsObject(Value) Then
        Set objNewMember.Value = Value
    Else
        objNewMember.Value = Value
    End If
    idx = mCol.Count + 1
    If m_Sorted = True Then
        For idx = 1 To mCol.Count
            If Caption < mCol(idx).Caption Then
                Exit For
            End If
        Next
    End If
    If Len(Key) > 0 Then
        If idx <= mCol.Count Then
            mCol.Add objNewMember, Key, idx
        Else
            mCol.Add objNewMember, Key
        End If
    Else
        If idx <= mCol.Count Then
            mCol.Add objNewMember, , idx
        Else
            mCol.Add objNewMember
        End If
    End If
    ' initialize object
    objNewMember.Init Parent
    ' return the object created
    Set Add = objNewMember
    Set objNewMember = Nothing
End Function

Public Property Get Item(Index As Variant) As TListValue
Attribute Item.VB_UserMemId = 0
    Set Item = mCol(Index)
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
    Set NewEnum = mCol.[_NewEnum]
End Property

Private Sub Class_Initialize()
    Set mCol = New Collection
    m_Sorted = True
End Sub

Private Sub Class_Terminate()
    'destroys collection when this class is terminated
    Set mCol = Nothing
End Sub

Public Sub Clear()
    Dim i As Integer
    
    For i = 1 To mCol.Count
        mCol.Remove 1
    Next
End Sub

Public Sub Append(Item As TListValue)
    If Item.Caption = "" Then
        Item.Caption = CStr(Item.Value)
    End If
    mCol.Add Item, Item.Caption
End Sub

Private Sub Sort()
    Dim t As TListValue
    Dim i As Long
    Dim j As Long
    Dim Lb As Long
    Dim Ub As Long
    Lb = 1
    Ub = mCol.Count
    ' sort A[Lb..Ub]
    For i = Lb + 1 To Ub
        Set t = mCol(i)
        ' shift elements down until insertion point found
        For j = i - 1 To Lb Step -1
            If mCol(j).Caption <= t.Caption Then Exit For
            Set mCol(j + 1) = mCol(j)
        Next j
        ' insert
        Set mCol(j + 1) = t
    Next i
End Sub

Public Function Clone() As TListValues
    Dim Item As TListValue
    Dim NewList As New TListValues
    Dim NewItem As TListValue
    
    NewList.Sorted = Me.Sorted
    For Each Item In mCol
        Set NewItem = New TListValue
        NewItem.Caption = Item.Caption
        NewItem.Value = Item.Value
        NewList.Append NewItem
    Next
    Set Clone = NewList
End Function

Function Exists(Index As Variant) As Boolean
    Dim objTest As TListValue
    
    Set objTest = Nothing
    If Count > 0 Then
        On Error Resume Next
        Set objTest = Item(Index)
        On Error GoTo 0
    End If
    Exists = Not (objTest Is Nothing)
End Function
'--end code
