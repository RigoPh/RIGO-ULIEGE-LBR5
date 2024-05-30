VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "TProperties"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Collection" ,"TProperty"
Attribute VB_Ext_KEY = "Member0" ,"TProperty"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
' *******************************************************
' Class        : TProperties.cls
' Written By   : Marclei V Silva (MVS)
' Programmer   : Marclei V Silva (MVS) [Spnorte Consultoria de Informática]
' Date Writen  : 06/16/2000 -- 08:27:55
' Inputs       : N/A
' Outputs      : N/A
' Description  : Collection of properties
'              :
'              :
'              :
' *******************************************************
Option Explicit
Option Compare Text

' local variable to hold collection
Private mCol As Collection
Private m_ParentPtr As Long

Friend Sub Init(Object As TCategory)
    m_ParentPtr = PtrFromObject(Object)
End Sub

Public Property Get Parent() As TCategory
    Set Parent = ObjectFromPtr(m_ParentPtr)
End Property

Public Function Add( _
    Caption As String, _
    Optional Value As Variant, _
    Optional ValueType As psPropertyType = psNone, _
    Optional ReadOnly As Boolean, _
    Optional Image As Variant = 0, _
    Optional TooltipText As String, _
    Optional Description As String _
    ) As TProperty
    
    Dim objNewMember As TProperty
    Dim Index As Integer
    Dim i As Integer
    
    ' create a new object
    Set objNewMember = New TProperty
    ' check for no given value type
    If ValueType = psNone Then
        ValueType = GetValueType(Value)
    End If
    ' set the properties passed into the method
    objNewMember.Caption = Caption
    objNewMember.Image = Image
    objNewMember.ReadOnly = ReadOnly
    objNewMember.TooltipText = TooltipText
    objNewMember.ValueType = ValueType
    If IsMissing(Value) Then
        Value = GetDefaultValue(ValueType)
    Else
        If IsObject(Value) Then
            Set objNewMember.Value = Value
        Else
'<Modified by: Project Administrator at 26/3/2004-09:49:15 on machine: ZEUS>
            ' if a not valid value is passed it will be converted to a valid data
            ' based on value type value
            objNewMember.Value = ConvertValue(Value, ValueType)
'</Modified by: Project Administrator at 26/3/2004-09:49:15 on machine: ZEUS>
        End If
    End If
    ' if it is a boolean value then add items
    ' to ListValues collection
    If ValueType = psBoolean Then
        objNewMember.ListValues.Add False, "False", "False"
        objNewMember.ListValues.Add True, "True", "True"
    ElseIf ValueType = psTime Then
        objNewMember.UpDownIncrement = 1
        objNewMember.Format = "hh:mm:ss"
    ElseIf ValueType = psCurrency Then
        ' value type is currency then add a format
        objNewMember.Format = "$ #,##0.00"
    End If
    ' set the default properties
    objNewMember.BackColor = CLR_INVALID
    objNewMember.ForeColor = CLR_INVALID
    objNewMember.MaskColor = CLR_INVALID
    objNewMember.BorderStyle = psBorderSingle
    objNewMember.Expanded = False
    objNewMember.Key = Caption
    objNewMember.Selected = False
    objNewMember.SelBorderStyle = psBorderSingle
    objNewMember.Description = Description
    Index = mCol.Count + 1
    For Index = 1 To mCol.Count
        If LCase(Caption) < LCase(mCol(Index).Caption) Then
            Exit For
        End If
    Next
    objNewMember.Index = Index
    For i = Index To mCol.Count
        mCol(i).Index = i + 1
    Next
    ' the key property is the property caption
    If Index <= mCol.Count Then
        mCol.Add objNewMember, Caption, Index
    Else
        mCol.Add objNewMember, Caption
    End If
    ' add new property
    Parent.Parent.TriggerEvent "AddNewProperty", objNewMember, Parent
    ' add parent reference
    objNewMember.Init Parent
    ' return the object created
    Set Add = objNewMember
    ' clean it up
    Set objNewMember = Nothing
    Parent.Parent.Refresh
End Function

Public Property Get Item(Index As Variant) As TProperty
Attribute Item.VB_UserMemId = 0
    Set Item = mCol(Index)
End Property

'<CSCM>
'--------------------------------------------------------------------------------
' Project      :       PropertySheet
' Procedure    :       Exists
' Description  :       Returns true if the given property value exists
' Created by   :       Project Administrator
' Machine      :       ZEUS
' Date-Time    :       24/3/2004-18:20:19
'
' Parameters   :       Caption (String)
' Return Values:
'--------------------------------------------------------------------------------
'</CSCM>
Public Function Exists(ByVal Caption As String) As Boolean
    Dim objTest As TProperty
    
    Set objTest = Nothing
    If Count > 0 Then
        On Error Resume Next
        Set objTest = Item(Caption)
        On Error GoTo 0
    End If
    Exists = Not (objTest Is Nothing)
End Function

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
End Sub

Private Sub Class_Terminate()
    Set mCol = Nothing
End Sub
'-- end code
