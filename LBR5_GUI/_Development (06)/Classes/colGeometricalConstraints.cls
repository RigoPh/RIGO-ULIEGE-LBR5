VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "colGeometricalConstraints"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
Private mCol As Collection
Private Const GEOMETRICALCONSTRAINTSTRINGKEY As String = "GeometricalConstraint"

Public Function Add(m As cGeometricalConstraints, i As Integer) As Boolean
    On Error GoTo AddErr
    mCol.Add m, GEOMETRICALCONSTRAINTSTRINGKEY & i
    Set m = Nothing
    Add = True
    Exit Function
AddErr:
    Add = False
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colGeometricalConstraints: Function Add")
End Function

Public Function Remove(i As Integer) As Boolean
    On Error GoTo RemoveErr
    mCol.Remove (GEOMETRICALCONSTRAINTSTRINGKEY & i)
    Remove = True
    Exit Function
RemoveErr:
    Remove = False
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colGeometricalConstraints: Function Remove")
End Function

Public Property Get Item(i As Integer) As cGeometricalConstraints
    On Error GoTo ItemGetErr
    Set Item = mCol(GEOMETRICALCONSTRAINTSTRINGKEY & i)
    Exit Property
ItemGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colGeometricalConstraints: Item Property Get")
End Property

Public Property Get Count() As Long
    On Error GoTo CountGetErr
    Count = mCol.Count
    Exit Property
CountGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colGeometricalConstraints: Count Property Get")
End Property

Public Property Get NewEnum() As IUnknown
Attribute NewEnum.VB_UserMemId = -4
Attribute NewEnum.VB_MemberFlags = "40"
    On Error GoTo NewEnumGetErr
    Set NewEnum = mCol.[_NewEnum]
    Exit Property
NewEnumGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colGeometricalConstraints: NewEnum Property Get")
End Property

Public Function Clone() As colGeometricalConstraints
    On Error GoTo CloneErr
    Dim OBJ As cGeometricalConstraints
    Set Clone = New colGeometricalConstraints
    For Each OBJ In mCol
        Clone.Add OBJ.Clone, OBJ.Clone.index
    Next OBJ
    Exit Function
CloneErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colGeometricalConstraints: Function Clone")
End Function

Public Function WriteASCII(ByVal iNoOfFile As Integer)
    On Error GoTo WriteASCIIErr
    Dim cGeometricalConstraints As cGeometricalConstraints
    Dim ii As Integer
    With Me
        Select Case .Count
            Case Is = 0
                ii = 0
            Case Is >= 1
                If .Item(1).code = 99 Then
                    ii = 99
                Else
                    ii = CInt(.Count)
                End If
        End Select
        Put #iNoOfFile, , ii
        For Each cGeometricalConstraints In Me
            cGeometricalConstraints.WriteASCII iNoOfFile
        Next cGeometricalConstraints
    End With
    Exit Function
WriteASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colGeometricalConstraints: Function WriteASCII")
End Function

Public Function ReadASCII(ByVal iNoOfFile As Integer)
    On Error GoTo ReadASCIIErr
    Dim i As Integer, ii As Integer
    Dim cGeometricalConstraints As cGeometricalConstraints
    With Me
        Get #iNoOfFile, , ii
        Select Case ii
            Case 0
            Case Is = 99
                Set cGeometricalConstraints = New cGeometricalConstraints
                cGeometricalConstraints.ReadASCII iNoOfFile
                Me.Add cGeometricalConstraints, cGeometricalConstraints.index
                Set cGeometricalConstraints = Nothing
            Case Is > 0
                For i = 1 To ii
                    Set cGeometricalConstraints = New cGeometricalConstraints
                    cGeometricalConstraints.ReadASCII iNoOfFile
                    Me.Add cGeometricalConstraints, cGeometricalConstraints.index
                    Set cGeometricalConstraints = Nothing
                Next i
            End Select
    End With
    Exit Function
ReadASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colGeometricalConstraints: Function ReadASCII")
End Function

Private Sub Class_Initialize()
    Set mCol = New Collection
End Sub

Private Sub Class_terminate()
    Set mCol = Nothing
End Sub




