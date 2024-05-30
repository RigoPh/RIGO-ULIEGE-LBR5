VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cToolbars"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Private mvartbStandard As YesNo
Private mvartbView As YesNo
Private mvartbGeometry As YesNo
Private mvartbSolver As YesNo

Public Property Let tbStandard(ByVal vData As YesNo)
    On Error GoTo tbStandardLetErr
    mvartbStandard = vData
    Exit Property
tbStandardLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cToolbars: tbStandard Property Let")
End Property

Public Property Get tbStandard() As YesNo
    On Error GoTo tbStandardGetErr
    tbStandard = mvartbStandard
    Exit Property
tbStandardGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cToolbars: tbStandard Property Get")
End Property

Public Property Let tbView(ByVal vData As YesNo)
    On Error GoTo tbViewLetErr
    mvartbView = vData
    Exit Property
tbViewLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cToolbars: tbView Property Let")
End Property

Public Property Get tbView() As YesNo
    On Error GoTo tbViewGetErr
    tbView = mvartbView
    Exit Property
tbViewGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cToolbars: tbView Property Get")
End Property

Public Property Let tbGeometry(ByVal vData As YesNo)
    On Error GoTo tbGeometryLetErr
    mvartbGeometry = vData
    Exit Property
tbGeometryLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cToolbars: tbGeometry Property Let")
End Property

Public Property Get tbGeometry() As YesNo
    On Error GoTo tbGeometryGetErr
    tbGeometry = mvartbGeometry
    Exit Property
tbGeometryGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cToolbars: tbGeometry Property Get")
End Property

Public Property Let tbSolver(ByVal vData As YesNo)
    On Error GoTo tbSolverLetErr
    mvartbSolver = vData
    Exit Property
tbSolverLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cToolbars: tbSolver Property Let")
End Property

Public Property Get tbSolver() As YesNo
    On Error GoTo tbSolverGetErr
    tbSolver = mvartbSolver
    Exit Property
tbSolverGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cToolbars: tbSolver Property Get")
End Property
