VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "colAssessmentPoints"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
Private mCol As Collection
Private Const ASSESSMENTPOINTSTRINGKEY As String = "Girder"

Public Property Get col() As Collection
    On Error GoTo ColGetErr
    Set col = mCol
    Exit Property
ColGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colAssessmentPoints: Col Property Get")
End Property

Public Function Add(m As cAssessmentPoint, i As Integer) As Boolean
    On Error GoTo AddErr
    mCol.Add m, ASSESSMENTPOINTSTRINGKEY & i
    Set m = Nothing
    Add = True
    Exit Function
AddErr:
    Add = False
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colAssessmentPoints: Function Add")
End Function

Public Function Remove(i As Integer) As Boolean
    On Error GoTo RemoveErr
    mCol.Remove (ASSESSMENTPOINTSTRINGKEY & i)
    Remove = True
    Exit Function
RemoveErr:
    Remove = False
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colAssessmentPoints: Function Remove")
End Function

Public Property Get Item(i As Integer) As cAssessmentPoint
    On Error GoTo ItemGetErr
    Set Item = mCol(ASSESSMENTPOINTSTRINGKEY & i)
    Exit Property
ItemGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colAssessmentPoints: Item Property Get")
End Property

Public Property Get Count() As Long
    On Error GoTo CountGetErr
    Count = mCol.Count
    Exit Property
CountGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colAssessmentPoints: Count Property Get")
End Property

Public Property Get NewEnum() As IUnknown
Attribute NewEnum.VB_UserMemId = -4
    On Error GoTo NewEnumGetErr
    Set NewEnum = mCol.[_NewEnum]
    Exit Property
NewEnumGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colAssessmentPoints: NewEnum Property Get")
End Property

Public Function Clone() As colAssessmentPoints
    On Error GoTo CloneErr
    Dim OBJ As cAssessmentPoint
    Set Clone = New colAssessmentPoints
    For Each OBJ In mCol
        Clone.Add OBJ.Clone, OBJ.Clone.index
    Next OBJ
    Exit Function
CloneErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colAssessmentPoints: Function Clone")
End Function

Public Function WriteASCII(ByVal iNoOfFile As Integer)
    On Error GoTo WriteASCIIErr
    Dim cAssessmentPoint As cAssessmentPoint
    With Me
        Put #iNoOfFile, , CInt(.Count)
        For Each cAssessmentPoint In Me
            cAssessmentPoint.WriteASCII iNoOfFile
        Next cAssessmentPoint
    End With
    Exit Function
WriteASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colAssessmentPoints: Function WriteASCII")
End Function

Public Function ReadASCII(ByVal iNoOfFile As Integer)
    On Error GoTo ReadASCIIErr
    Dim i As Integer, ii As Integer
    Dim cAssessmentPoint As cAssessmentPoint
    With Me
        Get #iNoOfFile, , ii
        For i = 1 To ii
            Set cAssessmentPoint = New cAssessmentPoint
            cAssessmentPoint.ReadASCII iNoOfFile
            Me.Add cAssessmentPoint, cAssessmentPoint.index
            Set cAssessmentPoint = Nothing
        Next i
    End With
    Exit Function
ReadASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colAssessmentPoints: Function ReadASCII")
End Function

Private Sub Class_Initialize()
    Set mCol = New Collection
End Sub

Private Sub Class_terminate()
    Set mCol = Nothing
End Sub
