VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "TListValue"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
' *******************************************************
' Class        : TListValue.cls
' Written By   : Marclei V Silva (MVS)
' Programmer   : Marclei V Silva (MVS) [Spnorte Consultoria de Informática]
' Date Writen  : 06/16/2000 -- 08:33:11
' Inputs       : N/A
' Outputs      : N/A
' Description  : Simple list value information
'              :
'              :
' *******************************************************
Option Explicit

Private m_Caption As String
Private m_Value As Variant
Private m_ParentPtr As Long

Public Sub Init(ByVal Object As TProperty)
    m_ParentPtr = PtrFromObject(Object)
End Sub

Public Property Get Parent() As TProperty
    Set Parent = ObjectFromPtr(m_ParentPtr)
End Property

Public Property Let Value(ByVal vData As Variant)
    m_Value = vData
    If m_ParentPtr <> 0 Then
        Parent.Parent.Parent.TriggerEvent "ValueChanged", Parent, Parent.Row, vData
    End If
End Property

Public Property Set Value(ByVal vData As Variant)
    Set m_Value = vData
    If m_ParentPtr <> 0 Then
        Parent.Parent.Parent.TriggerEvent "ValueChanged", Parent, Parent.Row, vData
    End If
End Property

Public Property Get Value() As Variant
    If IsObject(m_Value) Then
        On Error Resume Next
        Set Value = m_Value
    Else
        Value = m_Value
    End If
End Property

Public Property Let Caption(ByVal vData As String)
    m_Caption = vData
    If m_ParentPtr <> 0 Then
        Parent.Parent.Parent.TriggerEvent "ValueChanged", Parent, Parent.Row, vData
    End If
End Property

Public Property Get Caption() As String
    Caption = m_Caption
End Property
