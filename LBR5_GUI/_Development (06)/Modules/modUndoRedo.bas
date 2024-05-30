Attribute VB_Name = "modUndoRedo"
Option Explicit
Public Const iMaps As Integer = 10
Dim cMap As cProject
Dim colMap As New Collection
Dim vMap() As Variant

Public Function ChangeMaps(ByVal ProjectIndex As Integer)
    On Error GoTo ChangeMapsErr:
    Static iInd As Integer
    'Set colMap = Project.Item(ProjectIndex).ProjectMaps
    'Set colMap = New colProject
    'For Each cMap In Project.Item(ProjectIndex).ProjectMaps
        iInd = iInd + 1
        colMap.Add Project.Item(ProjectIndex) '.Clone
    'Next cMap
    Dim index_max As Integer
    index_max = colMap.Count
'    Select Case colMap.Count
'        Case Is < iMaps
'            colMap.Add Project.Item(ProjectIndex), index_max + 1
'        Case Is = iMaps
'        Case Else
'    End Select
    
    
'    Set Project.Item(ProjectIndex).ProjectMaps = Nothing
'    For Each cMap In colMap
'        iInd = iInd + 1
'        Project.Item(ProjectIndex).ProjectMaps.Add cMap, iInd
'    Next cMap
'    Set colMap = Nothing
    Exit Function
ChangeMapsErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modUndoRedo: Function ChangeMaps")
End Function
