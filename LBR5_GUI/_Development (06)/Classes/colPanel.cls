VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "colPanel"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
Private mCol As Collection
Private Const PANELSTRINGKEY As String = "Panel"

Public Function Add(m As cPanel, i As Integer) As Boolean
    On Error GoTo AddErr
    m.Visible = True
    mCol.Add m, PANELSTRINGKEY & i
    Set m = Nothing
    Add = True
    Exit Function
AddErr:
    Add = False
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colPanel: Function Add")
End Function

Public Function Remove(i As Integer) As Boolean
    On Error GoTo RemoveErr
    mCol.Remove (PANELSTRINGKEY & i)
    Remove = True
    Exit Function
RemoveErr:
    Remove = False
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colPanel: Function Remove")
End Function

Public Property Get Item(i As Integer) As cPanel
    On Error GoTo ItemGetErr
    Set Item = mCol(PANELSTRINGKEY & i)
    Exit Property
ItemGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colPanel: Item Property Get")
End Property

Public Property Get Count() As Long
    On Error GoTo CountGetErr
    Count = mCol.Count
    Exit Property
CountGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colPanel: Count Property Get")
End Property

Public Property Get NewEnum() As IUnknown
Attribute NewEnum.VB_UserMemId = -4
Attribute NewEnum.VB_MemberFlags = "40"
    On Error GoTo NewEnumGetErr
    Set NewEnum = mCol.[_NewEnum]
    Exit Property
NewEnumGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colPanel: NewEnum Property Get")
End Property

Public Function Clone() As colPanel
    On Error GoTo CloneErr
    Dim OBJ As cPanel
    Set Clone = New colPanel
    For Each OBJ In mCol
        Clone.Add OBJ.Clone, OBJ.Clone.index
    Next OBJ
    Exit Function
CloneErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colPanel: Function Clone")
End Function

Public Function WritePanelUpdateASCII(ByVal iNoOfFile As Integer)
    On Error GoTo WritePanelUpdateASCIIErr
    Dim cPanel As cPanel
    With Me
        Put #iNoOfFile, , CInt(.Count)
        For Each cPanel In Me
            cPanel.WritePanelUpdateASCII iNoOfFile
        Next cPanel
    End With
    Exit Function
WritePanelUpdateASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colPanel: Function WritePanelUpdateASCII")
End Function

Public Function WriteASCII(ByVal iNoOfFile As Integer)
    On Error GoTo WriteASCIIErr
    Dim cPanel As cPanel
    With Me
        Put #iNoOfFile, , CInt(.Count)
        For Each cPanel In Me
            cPanel.WriteASCII iNoOfFile
        Next cPanel
    End With
    Exit Function
WriteASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colPanel: Function WriteASCII")
End Function

Public Function ReadASCII(ByVal iNoOfFile As Integer)
    On Error GoTo ReadASCIIErr
    Dim i As Integer, ii As Integer
    Dim cPanel As cPanel
    With Me
        Get #iNoOfFile, , ii
        For i = 1 To ii
            Set cPanel = New cPanel
            cPanel.ReadASCII iNoOfFile
            'cPanel.cCostCAtMain.SetFirstPanelData cPanel 'temp (will have to read/write CostCatData
            Me.Add cPanel, cPanel.pNumber                  'in .lbr project")
            Set cPanel = Nothing
        Next i
    End With
    Exit Function
ReadASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "colPanel: Function ReadASCII")
End Function

Public Function GetNoOfPlates() As Integer
    Dim Panel As cPanel
    GetNoOfPlates = 0
    For Each Panel In mCol
        If Panel.pType = Plate Then
            GetNoOfPlates = GetNoOfPlates + 1
        End If
    Next Panel
End Function

Public Function GetNoOfDoubleHulls() As Integer
    Dim Panel As cPanel
    GetNoOfDoubleHulls = 0
    For Each Panel In mCol
        If Panel.pType = DoubleHull Then
            If Panel.RelatedDoubleHullPanel > Panel.pNumber Then
                GetNoOfDoubleHulls = GetNoOfDoubleHulls + 1
            End If
        End If
    Next Panel
End Function

Public Function GetNoOfBeams() As Integer
    Dim Panel As cPanel
    GetNoOfBeams = 0
    For Each Panel In mCol
        If Panel.pType = Beam Then
            GetNoOfBeams = GetNoOfBeams + 1
        End If
    Next Panel
End Function

Public Function SetPanelsVisible()
    On Error GoTo SetPanelsVisibleErr
    Dim cPanel As cPanel
    For Each cPanel In mCol
        cPanel.Visible = True
    Next cPanel
    Exit Function
SetPanelsVisibleErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "modPanelFunctions: Function SetPanelsVisible")
End Function

Private Sub Class_Initialize()
    Set mCol = New Collection
End Sub

Private Sub Class_terminate()
    Set mCol = Nothing
End Sub
