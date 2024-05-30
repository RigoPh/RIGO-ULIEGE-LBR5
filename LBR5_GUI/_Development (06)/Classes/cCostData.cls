VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "cCostData"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
#Const DebugMode = 0
#If DebugMode Then
    Private mlClassDebugID As Long
#End If
Private mvarREND As Double
Private mvarEQP As Double
Private mvarE0 As Double
Private mvarE0X As Double
Private mvarE0Y As Double
Private mvarC1 As Double
Private mvarC2 As Double
Private mvarC3 As Double
Private mvarDC1 As Double
Private mvarDW2 As Double
Private mvarDW3 As Double
Private mvarP10 As Double
Private mvarDP10 As Double
Private mvarP4 As Double
Private mvarP5 As Double
Private mvarDP4 As Double
Private mvarDP5 As Double
Private mvarP9X As Double
Private mvarP9Y As Double
Private mvarDP9X As Double
Private mvarDP9Y As Double
Private mvarP6 As Double
Private mvarP7 As Double
Private mvarBETA_X As Double
Private mvarBETA_Y As Double
Private mvarC8 As Double
Private mvarDC8 As Double
Private mvarALPHA_X As Double
Private mvarALPHA_Y As Double

Public Property Get REND() As Double
    On Error GoTo RENDGetErr
    REND = mvarREND
    Exit Property
RENDGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: REND Property Get")
End Property

Public Property Let REND(ByVal vData As Double)
    On Error GoTo RENDLetErr
    mvarREND = vData
    Exit Property
RENDLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: REND Property Let")
End Property

Public Property Get EQP() As Double
    On Error GoTo EQPGetErr
    EQP = mvarEQP
    Exit Property
EQPGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: EQP Property Get")
End Property

Public Property Let EQP(ByVal vData As Double)
    On Error GoTo EQPLetErr
    mvarEQP = vData
    Exit Property
EQPLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: EQP Property Let")
End Property

Public Property Get E0() As Double
    On Error GoTo E0GetErr
    E0 = mvarE0
    Exit Property
E0GetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: E0 Property Get")
End Property

Public Property Let E0(ByVal vData As Double)
    On Error GoTo E0LetErr
    mvarE0 = vData
    Exit Property
E0LetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: E0 Property Let")
End Property

Public Property Get E0X() As Double
    On Error GoTo E0XGetErr
    E0X = mvarE0X
    Exit Property
E0XGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: E0X Property Get")
End Property

Public Property Let E0X(ByVal vData As Double)
    On Error GoTo E0XLetErr
    mvarE0X = vData
    Exit Property
E0XLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: E0X Property Let")
End Property

Public Property Get E0Y() As Double
    On Error GoTo E0YGetErr
    E0Y = mvarE0Y
    Exit Property
E0YGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: E0Y Property Get")
End Property

Public Property Let E0Y(ByVal vData As Double)
    On Error GoTo E0YLetErr
    mvarE0Y = vData
    Exit Property
E0YLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: E0Y Property Let")
End Property

Public Property Get C1() As Double
    On Error GoTo C1GetErr
    C1 = mvarC1
    Exit Property
C1GetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: C1 Property Get")
End Property

Public Property Let C1(ByVal vData As Double)
    On Error GoTo C1LetErr
    mvarC1 = vData
    Exit Property
C1LetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: C1 Property Let")
End Property

Public Property Get C2() As Double
    On Error GoTo C2GetErr
    C2 = mvarC2
    Exit Property
C2GetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: C2 Property Get")
End Property

Public Property Let C2(ByVal vData As Double)
    On Error GoTo C2LetErr
    mvarC2 = vData
    Exit Property
C2LetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: C2 Property Let")
End Property

Public Property Get C3() As Double
    On Error GoTo C3GetErr
    C3 = mvarC3
    Exit Property
C3GetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: C3 Property Get")
End Property

Public Property Let C3(ByVal vData As Double)
    On Error GoTo C3LetErr
    mvarC3 = vData
    Exit Property
C3LetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: C3 Property Let")
End Property

Public Property Get DC1() As Double
    On Error GoTo DC1GetErr
    DC1 = mvarDC1
    Exit Property
DC1GetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: DC1 Property Get")
End Property

Public Property Let DC1(ByVal vData As Double)
    On Error GoTo DC1LetErr
    mvarDC1 = vData
    Exit Property
DC1LetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: DC1 Property Let")
End Property

Public Property Get DW2() As Double
    On Error GoTo DW2GetErr
    DW2 = mvarDW2
    Exit Property
DW2GetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: DW2 Property Get")
End Property

Public Property Let DW2(ByVal vData As Double)
    On Error GoTo DW2LetErr
    mvarDW2 = vData
    Exit Property
DW2LetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: DW2 Property Let")
End Property

Public Property Get DW3() As Double
    On Error GoTo DW3GetErr
    DW3 = mvarDW3
    Exit Property
DW3GetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: DW3 Property Get")
End Property

Public Property Let DW3(ByVal vData As Double)
    On Error GoTo DW3LetErr
    mvarDW3 = vData
    Exit Property
DW3LetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: DW3 Property Let")
End Property

Public Property Get P10() As Double
    On Error GoTo P10GetErr
    P10 = mvarP10
    Exit Property
P10GetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: P10 Property Get")
End Property

Public Property Let P10(ByVal vData As Double)
    On Error GoTo P10LetErr
    mvarP10 = vData
    Exit Property
P10LetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: P10 Property Let")
End Property

Public Property Get DP10() As Double
    On Error GoTo DP10GetErr
    DP10 = mvarDP10
    Exit Property
DP10GetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: DP10 Property Get")
End Property

Public Property Let DP10(ByVal vData As Double)
    On Error GoTo DP10LetErr
    mvarDP10 = vData
    Exit Property
DP10LetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: DP10 Property Let")
End Property

Public Property Get p4() As Double
    On Error GoTo P4GetErr
    p4 = mvarP4
    Exit Property
P4GetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: P4 Property Get")
End Property

Public Property Let p4(ByVal vData As Double)
    On Error GoTo P4LetErr
    mvarP4 = vData
    Exit Property
P4LetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: P4 Property Let")
End Property

Public Property Get P5() As Double
    On Error GoTo P5GetErr
    P5 = mvarP5
    Exit Property
P5GetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: P5 Property Get")
End Property

Public Property Let P5(ByVal vData As Double)
    On Error GoTo P5LetErr
    mvarP5 = vData
    Exit Property
P5LetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: P5 Property Let")
End Property

Public Property Get DP4() As Double
    On Error GoTo DP4GetErr
    DP4 = mvarDP4
    Exit Property
DP4GetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: DP4 Property Get")
End Property

Public Property Let DP4(ByVal vData As Double)
    On Error GoTo DP4LetErr
    mvarDP4 = vData
    Exit Property
DP4LetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: DP4 Property Let")
End Property

Public Property Get DP5() As Double
    On Error GoTo DP5GetErr
    DP5 = mvarDP5
    Exit Property
DP5GetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: DP5 Property Get")
End Property

Public Property Let DP5(ByVal vData As Double)
    On Error GoTo DP5LetErr
    mvarDP5 = vData
    Exit Property
DP5LetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: DP5 Property Let")
End Property

Public Property Get P9X() As Double
    On Error GoTo P9XGetErr
    P9X = mvarP9X
    Exit Property
P9XGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: P9X Property Get")
End Property

Public Property Let P9X(ByVal vData As Double)
    On Error GoTo P9XLetErr
    mvarP9X = vData
    Exit Property
P9XLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: P9X Property Let")
End Property

Public Property Get P9Y() As Double
    On Error GoTo P9YGetErr
    P9Y = mvarP9Y
    Exit Property
P9YGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: P9Y Property Get")
End Property

Public Property Let P9Y(ByVal vData As Double)
    On Error GoTo P9YLetErr
    mvarP9Y = vData
    Exit Property
P9YLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: P9Y Property Let")
End Property

Public Property Get DP9X() As Double
    On Error GoTo DP9XGetErr
    DP9X = mvarDP9X
    Exit Property
DP9XGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: DP9X Property Get")
End Property

Public Property Let DP9X(ByVal vData As Double)
    On Error GoTo DP9XLetErr
    mvarDP9X = vData
    Exit Property
DP9XLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: DP9X Property Let")
End Property

Public Property Get DP9Y() As Double
    On Error GoTo DP9YGetErr
    DP9Y = mvarDP9Y
    Exit Property
DP9YGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: DP9Y Property Get")
End Property

Public Property Let DP9Y(ByVal vData As Double)
    On Error GoTo DP9YLetErr
    mvarDP9Y = vData
    Exit Property
DP9YLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: DP9Y Property Let")
End Property

Public Property Get P6() As Double
    On Error GoTo P6GetErr
    P6 = mvarP6
    Exit Property
P6GetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: P6 Property Get")
End Property

Public Property Let P6(ByVal vData As Double)
    On Error GoTo P6LetErr
    mvarP6 = vData
    Exit Property
P6LetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: P6 Property Let")
End Property

Public Property Get P7() As Double
    On Error GoTo P7GetErr
    P7 = mvarP7
    Exit Property
P7GetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: P7 Property Get")
End Property

Public Property Let P7(ByVal vData As Double)
    On Error GoTo P7LetErr
    mvarP7 = vData
    Exit Property
P7LetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: P7 Property Let")
End Property

Public Property Get BETA_X() As Double
    On Error GoTo BETA_XGetErr
    BETA_X = mvarBETA_X
    Exit Property
BETA_XGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: BETA_X Property Get")
End Property

Public Property Let BETA_X(ByVal vData As Double)
    On Error GoTo BETA_XLetErr
    mvarBETA_X = vData
    Exit Property
BETA_XLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: BETA_X Property Let")
End Property

Public Property Get BETA_Y() As Double
    On Error GoTo BETA_YGetErr
    BETA_Y = mvarBETA_Y
    Exit Property
BETA_YGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: BETA_Y Property Get")
End Property

Public Property Let BETA_Y(ByVal vData As Double)
    On Error GoTo BETA_YLetErr
    mvarBETA_Y = vData
    Exit Property
BETA_YLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: BETA_Y Property Let")
End Property

Public Property Get C8() As Double
    On Error GoTo C8GetErr
    C8 = mvarC8
    Exit Property
C8GetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: C8 Property Get")
End Property

Public Property Let C8(ByVal vData As Double)
    On Error GoTo C8LetErr
    mvarC8 = vData
    Exit Property
C8LetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: C8 Property Let")
End Property

Public Property Get DC8() As Double
    On Error GoTo DC8GetErr
    DC8 = mvarDC8
    Exit Property
DC8GetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: DC8 Property Get")
End Property

Public Property Let DC8(ByVal vData As Double)
    On Error GoTo DC8LetErr
    mvarDC8 = vData
    Exit Property
DC8LetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: DC8 Property Let")
End Property

Public Property Get ALPHA_X() As Double
    On Error GoTo ALPHA_XGetErr
    ALPHA_X = mvarALPHA_X
    Exit Property
ALPHA_XGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: ALPHA_X Property Get")
End Property

Public Property Let ALPHA_X(ByVal vData As Double)
    On Error GoTo ALPHA_XLetErr
    mvarALPHA_X = vData
    Exit Property
ALPHA_XLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: ALPHA_X Property Let")
End Property

Public Property Get ALPHA_Y() As Double
    On Error GoTo ALPHA_YGetErr
    ALPHA_Y = mvarALPHA_Y
    Exit Property
ALPHA_YGetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: ALPHA_Y Property Get")
End Property

Public Property Let ALPHA_Y(ByVal vData As Double)
    On Error GoTo ALPHA_YLetErr
    mvarALPHA_Y = vData
    Exit Property
ALPHA_YLetErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: ALPHA_Y Property Let")
End Property

'Methods
Public Function ReadLBR5txtFile(ts As TextStream)
    On Error GoTo ReadLBR5txtFileErr
        Dim sLine As String
        Dim v() As Variant
        sLine = ReadLn(ts)
        GetValues 2, sLine, v
        Me.REND = Val_(v(1))
        Me.EQP = Val_(v(2))
        sLine = ReadLn(ts)
        GetValues 3, sLine, v
        Me.E0 = Val_(v(1))
        Me.E0X = Val_(v(2))
        Me.E0Y = Val_(v(3))
        sLine = ReadLn(ts)
        GetValues 4, sLine, v
        Me.C1 = Val_(v(1))
        Me.C2 = Val_(v(2))
        Me.C3 = Val_(v(3))
        Me.DC1 = Val_(v(4))
        sLine = ReadLn(ts)
        GetValues 2, sLine, v
        Me.DW2 = Val_(v(1))
        Me.DW3 = Val_(v(2))
        sLine = ReadLn(ts)
        GetValues 2, sLine, v
        Me.P10 = Val_(v(1))
        Me.DP10 = Val_(v(2))
        sLine = ReadLn(ts)
        GetValues 4, sLine, v
        Me.p4 = Val_(v(1))
        Me.P5 = Val_(v(2))
        Me.DP4 = Val_(v(3))
        Me.DP5 = Val_(v(4))
        sLine = ReadLn(ts)
        GetValues 4, sLine, v
        Me.P9X = Val_(v(1))
        Me.P9Y = Val_(v(2))
        Me.DP9X = Val_(v(3))
        Me.DP9Y = Val_(v(4))
        sLine = ReadLn(ts)
        GetValues 4, sLine, v
        Me.P6 = Val_(v(1))
        Me.P7 = Val_(v(2))
        Me.BETA_X = Val_(v(3))
        Me.BETA_Y = Val_(v(4))
        sLine = ReadLn(ts)
        GetValues 4, sLine, v
        Me.C8 = Val_(v(1))
        Me.DC8 = Val_(v(2))
        Me.ALPHA_X = Val_(v(3))
        Me.ALPHA_Y = Val_(v(4))
    Exit Function
ReadLBR5txtFileErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: Function ReadLBR5txtFile")
End Function

Public Function WriteLBR5txtFile(ts As TextStream)
    On Error GoTo WriteLBR5txtFileErr
    Dim sComment As String
    
    sComment = "REND, EQP(0.01<k=EQP<0.1)"
    ts.WriteLine PVal(Me.REND, "0.0000") & "  " & PVal(Me.EQP, "0.0000") & getTabs(7) & sComment
    
    sComment = "Eo, Eox, Eoy  (épaisseur de référence en m)"
    ts.WriteLine PVal(Me.E0, "0.0000") & "  " & PVal(Me.E0X, "0.0000") & "  " & PVal(Me.E0Y, "0.0000") & getTabs(6) & sComment
    
    sComment = "Prix Matér. : C1.C2.C3(tole.Euro/kg). DC1(tole.variation par mm)"
    ts.WriteLine PVal(Me.C1, "0.0000") & "  " & PVal(Me.C2, "0.0000") & "  " & PVal(Me.C3, "0.0000") & "  " & _
                PVal(Me.DC1, "0.0000") & getTabs(5) & sComment
    
    sComment = "Extra weight: DW2(long.variation sur C2).DW3(trans.variation sur C3)"
    ts.WriteLine PVal(Me.DW2, "0.0000") & "  " & PVal(Me.DW3, "0.0000") & getTabs(7) & sComment
    
    sComment = "MdO Bordé: P10(h-h/m).DP10(variation par mm)"
    ts.WriteLine PVal(Me.P10, "0.0000") & "  " & PVal(Me.DP10, "0.0000") & getTabs(7) & sComment
    
    sComment = "MdO Assembl.: P4(Long.h-h/m).P5(trans.h-h/m).DP4(long).DP5(trans)"
    ts.WriteLine PVal(Me.p4, "0.0000") & "  " & PVal(Me.P5, "0.0000") & "  " & PVal(Me.DP4, "0.0000") & "  " & PVal(Me.DP5, "0.0000") & getTabs(5) & sComment
    
    sComment = "MdO Membr.  : P9x(long.h-h/m).P9y(trans.h-h/m).DP9x(long).DP9y(trans)"
    ts.WriteLine PVal(Me.P9X, "0.0000") & "  " & PVal(Me.P9Y, "0.0000") & "  " & PVal(Me.DP9X, "0.0000") & "  " & PVal(Me.DP9Y, "0.0000") & getTabs(5) & sComment
    
    sComment = "MdO Joints  : P6(intersect).P7(gousset).Béta-X(long.).Béta-Y(transv)"
    ts.WriteLine PVal(Me.P6, "0.0000") & "  " & PVal(Me.P7, "0.0000") & "  " & PVal(Me.BETA_X, "0.0000") & "  " & PVal(Me.BETA_Y, "0.0000") & getTabs(5) & sComment
    
    sComment = "Consommables: C8(Euro/m).DC8(variation par mm).Alpha-X(long.).Alpha-Y(transv)"
    ts.WriteLine PVal(Me.C8, "0.0000") & "  " & PVal(Me.DC8, "0.0000") & "  " & PVal(Me.ALPHA_X, "0.0000") & "  " & PVal(Me.ALPHA_Y, "0.0000") & getTabs(5) & sComment
    
    Exit Function
WriteLBR5txtFileErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: Function WriteLBR5txtFile")
End Function

Public Function Clone() As cCostData
    On Error GoTo CloneErr
    Set Clone = New cCostData
    Clone.REND = Me.REND
    Clone.EQP = Me.EQP
    Clone.E0 = Me.E0
    Clone.E0X = Me.E0X
    Clone.E0Y = Me.E0Y
    Clone.C1 = Me.C1
    Clone.C2 = Me.C2
    Clone.C3 = Me.C3
    Clone.DC1 = Me.DC1
    Clone.DW2 = Me.DW2
    Clone.DW3 = Me.DW3
    Clone.P10 = Me.P10
    Clone.DP10 = Me.DP10
    Clone.p4 = Me.p4
    Clone.P5 = Me.P5
    Clone.DP4 = Me.DP4
    Clone.DP5 = Me.DP5
    Clone.P9X = Me.P9X
    Clone.P9Y = Me.P9Y
    Clone.DP9X = Me.DP9X
    Clone.DP9Y = Me.DP9Y
    Clone.P6 = Me.P6
    Clone.P7 = Me.P7
    Clone.BETA_X = Me.BETA_X
    Clone.BETA_Y = Me.BETA_Y
    Clone.C8 = Me.C8
    Clone.DC8 = Me.DC8
    Clone.ALPHA_X = Me.ALPHA_X
    Clone.ALPHA_Y = Me.ALPHA_Y
    Exit Function
CloneErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: Function Clone")
End Function

Public Function WriteASCII(ByVal iNoOfFile As Integer)
    On Error GoTo WriteASCIIErr
    With Me
        Put #iNoOfFile, , .REND
        Put #iNoOfFile, , .EQP
        Put #iNoOfFile, , .E0
        Put #iNoOfFile, , .E0X
        Put #iNoOfFile, , .E0Y
        Put #iNoOfFile, , .C1
        Put #iNoOfFile, , .C2
        Put #iNoOfFile, , .C3
        Put #iNoOfFile, , .DC1
        Put #iNoOfFile, , .DW2
        Put #iNoOfFile, , .DW3
        Put #iNoOfFile, , .P10
        Put #iNoOfFile, , .DP10
        Put #iNoOfFile, , .p4
        Put #iNoOfFile, , .P5
        Put #iNoOfFile, , .DP4
        Put #iNoOfFile, , .DP5
        Put #iNoOfFile, , .P9X
        Put #iNoOfFile, , .P9Y
        Put #iNoOfFile, , .DP9X
        Put #iNoOfFile, , .DP9Y
        Put #iNoOfFile, , .P6
        Put #iNoOfFile, , .P7
        Put #iNoOfFile, , .BETA_X
        Put #iNoOfFile, , .BETA_Y
        Put #iNoOfFile, , .C8
        Put #iNoOfFile, , .DC8
        Put #iNoOfFile, , .ALPHA_X
        Put #iNoOfFile, , .ALPHA_Y
    End With
    Exit Function
WriteASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: Function WriteASCII")
End Function

Public Function ReadASCII(ByVal iNoOfFile As Integer)
    On Error GoTo ReadASCIIErr
    Dim ii As Integer, dd As Double
    With Me
        Get #iNoOfFile, , dd: .REND = dd
        Get #iNoOfFile, , dd: .EQP = dd
        Get #iNoOfFile, , dd: .E0 = dd
        Get #iNoOfFile, , dd: .E0X = dd
        Get #iNoOfFile, , dd: .E0Y = dd
        Get #iNoOfFile, , dd: .C1 = dd
        Get #iNoOfFile, , dd: .C2 = dd
        Get #iNoOfFile, , dd: .C3 = dd
        Get #iNoOfFile, , dd: .DC1 = dd
        Get #iNoOfFile, , dd: .DW2 = dd
        Get #iNoOfFile, , dd: .DW3 = dd
        Get #iNoOfFile, , dd: .P10 = dd
        Get #iNoOfFile, , dd: .DP10 = dd
        Get #iNoOfFile, , dd: .p4 = dd
        Get #iNoOfFile, , dd: .P5 = dd
        Get #iNoOfFile, , dd: .DP4 = dd
        Get #iNoOfFile, , dd: .DP5 = dd
        Get #iNoOfFile, , dd: .P9X = dd
        Get #iNoOfFile, , dd: .P9Y = dd
        Get #iNoOfFile, , dd: .DP9X = dd
        Get #iNoOfFile, , dd: .DP9Y = dd
        Get #iNoOfFile, , dd: .P6 = dd
        Get #iNoOfFile, , dd: .P7 = dd
        Get #iNoOfFile, , dd: .BETA_X = dd
        Get #iNoOfFile, , dd: .BETA_Y = dd
        Get #iNoOfFile, , dd: .C8 = dd
        Get #iNoOfFile, , dd: .DC8 = dd
        Get #iNoOfFile, , dd: .ALPHA_X = dd
        Get #iNoOfFile, , dd: .ALPHA_Y = dd
    End With
    Exit Function
ReadASCIIErr:
    Call RaiseError(MyUnhandledError, Err.Description & ErrStr & "cCostData: Function ReadASCII")
End Function

Private Sub Class_Initialize()
    #If DebugMode Then
        mlClassDebugID = GetNextClassDebugID()
        Debug.Print "'" & TypeName(Me) & "' instance " & mlClassDebugID & " created"
    #End If
End Sub

Private Sub Class_terminate()
    #If DebugMode Then
        Debug.Print "'" & TypeName(Me) & "' instance " & CStr(mlClassDebugID) & " is terminating"
    #End If
End Sub

#If DebugMode Then
    Public Property Get ClassDebugID()
        ClassDebugID = mlClassDebugID
    End Property
#End If
